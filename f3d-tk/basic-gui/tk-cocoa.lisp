(in-package :gui)

;;; 29 January 2010: COCOA (MacOSX OpenGL extensions API) is deprecated
;;; and is only supported in 32-bit mode, so we need to move to Cocoa.
;;; Cocoa support is through an Objective-C API.  Consider creating a
;;; tk-cocoa.lisp file.  Perhaps we can avoid creating yet another C
;;; file by using message passing from Lisp.

;;; This needs to move to a generic place and
;;; DEFAULT-GLX-ATTRIBUTE-LISTS in glx-ffi.lisp needs to derive from this.
(defun gl::default-opengl-attribute-lists ()
  '((:RGBA
     :DOUBLEBUFFER
     :DEPTH-SIZE 32)
    (:BGRA
     :DOUBLEBUFFER
     )
    (:RGBA
     :RED-SIZE 8
     :GREEN-SIZE 8
     :BLUE-SIZE 8
     ;;:ALPHA-SIZE 1
     :DOUBLEBUFFER
     :DEPTH-SIZE 24)
    (:RGBA
     :RED-SIZE 5
     :GREEN-SIZE 5
     :BLUE-SIZE 5
     ;:ALPHA-SIZE 1
     :DOUBLEBUFFER
     :DEPTH-SIZE 24)
    (:RGBA
     :RED-SIZE 5
     :GREEN-SIZE 5
     :BLUE-SIZE 5
     ;:ALPHA-SIZE 1
     :DOUBLEBUFFER
     :DEPTH-SIZE 16)
    ))
  

;;; ******************  TK-COCOA-WINDOW CLASS AND COCOA INTERFACE  ******************

;;; START COCOA DEPENDENCIES
;;;

(defclass tk-cocoa-window (tkgl-window)
  ((win-id :initform nil :initarg :id :accessor win-id) 
   (gl-view :initform nil :initarg :gl-view :accessor gl-view) ;; An NSView
   ))


#+never
(defmethod position-in-parent ((window tk-cocoa-window))
  (let* ((panel (panel window))
	 (px (window-x (widget panel)))
	 (py (window-y (widget panel)))
	 (h (window-height (widget panel)))
	 (rpos (root-position window)))
    (if rpos
	(destructuring-bind (root-x root-y)
	     rpos
	   (list (- root-x px)
		 (- root-y py)))
	)))

;;; Tk-Cocoa seems to generate useless window-x an window-y values.
;;; If we use those values, the NSOpenGLViews for GL rendering are all
;;; placed in the lowest left-hand pane in a frame.  We need to find
;;; the true coordinates of each window within its parent frame, and
;;; this seems to work:

(defmethod position-in-parent ((window tk-cocoa-window))
  (let* ((child (tk::widget window))
	 (parent (tk::widget-parent (tk::widget-parent child)))
	 (pheight (tk::window-height parent)))
    (list
     (- (tk::window-rootx child)
	(tk::window-rootx parent))
     (- pheight
	(- (tk::window-rooty child)
	   (tk::window-rooty parent))
	(tk::window-height window))
     )))



(defmethod drawable-p ((window tk-cocoa-window))
  (not (null (gl-view window))))


;;;
;;; Bad!  The root position computed in windows.lisp seems to be
;;; offset by the height of the decoration!!
;;;
(defvar *aqua-decoration-height* 25)

(defmethod glMakeCurrent ((window tk-cocoa-window))
  (and (drawable-p window)
       (gl-view window)
       (let ((rc (gl::fnsMakeViewCurrent (gl-view window) 0)))
	 (when (zerop rc)
	   (format t "~%glMakeViewCurrent failed."))
	 (update-window-parameters window)
	 rc)))



(defmethod glSwapBuffers ((window tk-cocoa-window))
;;  (gl::fnsSwapBuffers (gl-context window))
;;  (glFlush)
  )

#+never
(defmethod render-window :after ((window tk-cocoa-window) from-backing-store)
  (gl::fnsUnMakeViewCurrent (gl-view window)))


(defvar *cocoa-rendering-spec* nil)

(defparameter *cocoa-share-window* nil)

#+never
(defun get-global-cocoa-sharelist-context ()
  (and *cocoa-share-window*
       (gl-context *cocoa-share-window*)
       ))


(defvar *disable-init-state* nil)


;;;
;;; Variation - create subwindows whose bounds (we hope) are INSIDE
;;; each Tk window, then restrict GL operations to those bounds:
;;;

(defclass tk-cocoa-bbuffer-window (gl-bbuffer-window-mixin tk-cocoa-window)
    ())




(defmethod glUnMakeCurrent ((window t))
  nil)

(defmethod glUnMakeCurrent ((window tk-cocoa-window))
  (glFlush)
  (gl::fnsUnMakeViewCurrent (gl-view window))
  )

(defparameter *enable-cursor-blanking* t)

(defmethod set-blank-cursor ((window tk-cocoa-window))
  (when *enable-cursor-blanking*
    (gl::fnsHideCursor)
    ))

(defmethod unset-blank-cursor ((window tk-cocoa-window))
  (when *enable-cursor-blanking*
    (gl::fnsShowCursor)
    ))

(defmethod warp-pointer ((window tk-cocoa-window) x y)
  (let ((event *most-recent-buttonpress*)
	(pos (root-position window)))
    (when pos
      (destructuring-bind (wx wy)
	  pos
	(gl::fnsWarpCursor
	 (round (+ x wx)) (round (+ y wy))
	 )))))
      

(defmethod set-mouse-cursorpos ((window tk-cocoa-window) x y)
  (let ((event *most-recent-buttonpress*)
	(pos (root-position window)))
    (when pos
      (destructuring-bind (wx wy)
	  pos
	(gl::fnsWarpCursor
	 (round (+ x wx)) (round (+ y wy))
	 )))))
      

(defun window-rectangle (w)
  (values (window-x w)
	  (window-y w)
	  (window-width w)
	  (window-height w)))



(defun create-opengl-nsview (window-id x y w h)
  (if *cocoa-rendering-spec*
      (gl::fnsCreateOpenGLView window-id *cocoa-rendering-spec* x y w h)
      (progn ;; Else we need to initialize a decent rendering spec for FREEDIUS:
	(setf *cocoa-rendering-spec* (gl::fnsMakeRenderingSpec))
	(format t "~%New Cocoa rendering spec: ~a~%" *cocoa-rendering-spec*)

	(loop for attrs in (gl::default-opengl-attribute-lists)
	      for view =  (print (gl::fnsCreateOpenGLView window-id (gl::set-fns_rendering_spec *cocoa-rendering-spec* (print attrs)) x y w h))
	      while (zerop view)
	      finally
	   (return
	     (if (zerop view)
		 (progn
		   (format t "~%fnsCreateOpenGLView failed for ~a" attrs)
		   (setf *cocoa-rendering-spec* nil))
		 (progn
		   (format t "~%create-opengl-nsview returns ~a" view)
		   view)))))))


(defmethod reshape-nsview ((window tk-cocoa-window))
  (let ((pos (position-in-parent window)))
    (if (not pos)
	(break)
	(destructuring-bind (x0 y0)
	    pos
	  (gl::fnsSetViewFrame
	   (gl-view window)
	   (1+ x0) (1+ y0)
	   (- (window-width window) 2)
	   (- (window-height window) 2))))))


;;;
;;; Double check this - is the window-id an NSView object?  If so,
;;; make the rest of the cocoa api consistent...

(defmethod init-state ((window tk-cocoa-window) widget)
  (format t "~%init-state for ~a" window)
  (unless (or *disable-init-state* (gl-view window))
    (let* ((window-id (tk::widget-xwindow widget)) ;; Under windows, this is the HWND
	   )
      (update-window-parameters window)
      (setf (win-id window) window-id)

      (let ((pos (position-in-parent window)))
	(if (not pos)
	    (break)
	    (destructuring-bind (x0 y0)
		pos
	      (format t "~%New NSView object: ~x"
		      (setf (gl-view window)
			    (create-opengl-nsview
			     (win-id window)
			     (1+ x0) (1+ y0) 
			     (- (window-width window) 2)
			     (- (window-height window) 2)
			     )))))
	)))
  )


;;;
;;; END COCOA DEPENDENCIES



(defmethod tk-gl-window-script
           ((gl-pane-class tk-cocoa-window)
            widget-name 
            &key width height &allow-other-keys)
  (let ((tk-binding-name "image_pane"))
    (create-tkglwin-bindings tk-binding-name)
    `((frame ,widget-name -width ,width -height ,height
	     -takefocus true
	     -background "" ; This is needed in tk8.5 to prevent clearing the frame
					; Something else causes the background to be changed.
					; See DisplayFrame in tkFrame.c
	   ;;  -container true ;; prevents clearing the frame on expose and visibility events (in tk8.4)
	     )
      ;; Does the bindtags order really matter?
      (bindtags ,widget-name (concat (bindtags ,widget-name) ,tk-binding-name))
      ;(bindtags ,widget-name (concat ,tk-binding-name (bindtags ,widget-name)))
      )))



(defvar *default-pane-class* 'tk-cocoa-bbuffer-window)

(eval-when (eval load)
  (setq *default-pane-class* 'tk-cocoa-bbuffer-window)
  (pushnew :cocoa *features*))



(defun gl-pan-test (&key (current-view (top-view)) (dist 200.0) (dx 2.0))
  (let ((xf (2d-to-window-transform current-view)))
    (loop for i from 0.0 to dist by dx
	do (move-by xf (cv dx dx 0.0))
	(redisplay current-view))
    (loop for i from 0.0 to dist  by dx
	do (move-by xf (cv (- dx) (- dx) 0.0))
	(redisplay current-view))))


;;; This might improve texture mapping on Apples:

(defun apple-storage-shared ()
  ;; #define GL_STORAGE_SHARED_APPLE            0x85BF
  ;; #define GL_TEXTURE_STORAGE_HINT_APPLE      0x85BC
  ;; #define GL_TEXTURE_RECTANGLE_EXT          0x84F5
  (gl::glTexParameteri #x84f5 #x85bc #x85bf))


;;;

;;;
;;; Offscreen GLX windows: These will behave like regular windows, but
;;; are intended for offscreen rendering, e.g., when emitting web
;;; pages or status reports.  GRAB-GL-BUFFER will work on these
;;; windows.
;;;

(defclass cocoa-offscreen-window (basic-window)
    ((glcontext :initarg :glcontext :accessor glcontext)
     (pbuffer :initarg :pbuffer :accessor pbuffer)
     ))


#+never
(defun offscreen-glx-attribute-lists ()
  (list (list gl::GLX_RGBA
	      gl::GLX_RED_SIZE 8
	      gl::GLX_GREEN_SIZE 8
	      gl::GLX_BLUE_SIZE 8
	      ;;gl::GLX_ALPHA_SIZE 1
	      gl::GLX_DOUBLEBUFFER
	      gl::GLX_DEPTH_SIZE 32)
	(list gl::GLX_RGBA
	      gl::GLX_RED_SIZE 8
	      gl::GLX_GREEN_SIZE 8
	      gl::GLX_BLUE_SIZE 8
	      ;;gl::GLX_ALPHA_SIZE 1
	      gl::GLX_DOUBLEBUFFER
	      gl::GLX_DEPTH_SIZE 24)
	(list gl::GLX_RGBA
	      gl::GLX_RED_SIZE 8
	      gl::GLX_GREEN_SIZE 8
	      gl::GLX_BLUE_SIZE 8
	      ;;gl::GLX_ALPHA_SIZE 1
	      gl::GLX_DEPTH_SIZE 32)
        (list gl::GLX_RGBA
	      gl::GLX_RED_SIZE 8
	      gl::GLX_GREEN_SIZE 8
	      gl::GLX_BLUE_SIZE 8
	      ;;gl::GLX_ALPHA_SIZE 1
	      gl::GLX_DEPTH_SIZE 24)
	(list gl::GLX_RGBA
	      gl::GLX_RED_SIZE 5
	      gl::GLX_GREEN_SIZE 5
	      gl::GLX_BLUE_SIZE 5
	      ;;gl::GLX_ALPHA_SIZE 1
	      gl::GLX_DOUBLEBUFFER
	      gl::GLX_DEPTH_SIZE 24)
	(list gl::GLX_RGBA
	      gl::GLX_RED_SIZE 5
	      gl::GLX_GREEN_SIZE 5
	      gl::GLX_BLUE_SIZE 5
	      ;;gl::GLX_ALPHA_SIZE 1
	      gl::GLX_DOUBLEBUFFER
	      gl::GLX_DEPTH_SIZE 16)))


;;; This is hokey, and only exists to provide a meaningful result for
;;; PRINT-OBJECT.  Without it, VIEW and WINDOW objects have no
;;; identifying string.  This definition shows the window "top" state.

(defmethod widget ((win cocoa-offscreen-window))
  (let ((view (top-view win)))
    (if view
        (let ((site (3d-world view)))
          (if site
              (format nil "~a/~a" (name site) (name (2d-world view)))
              (name (2d-world view))))
        "Empty")))


(defun make-context-choose-format (&optional (attributes (gl::default-opengl-attribute-lists)))
  (let ((spec *cocoa-rendering-spec*))
    (if spec
	(gl::fnsCreateContext spec)
	(let ((ctx nil))
	  (setf *cocoa-rendering-spec* (setf spec (gl::fnsMakeRenderingSpec)))
	  (loop for attr in attributes
		for context = (gl::fnsCreateContext (gl::set-fns_rendering_spec spec attr))
		while (zerop context)
		finally
	     (setf ctx context))
	  (unless ctx
	    (format t "~%Could NOT find a suitable pixel format!!")
	    (setf *cocoa-rendering-spec* nil))
	  ctx))))


(defmethod initialize-instance :after ((win cocoa-offscreen-window) &rest args &key (width 640) (height 480)  &allow-other-keys)
  (setf (dimensions win) (list width height))
  (format t "~%Made cocoa rendering spec...")
  (when (setf (glcontext win)  (make-context-choose-format))
    (format t "~%Made gl context...")
    (setf (pbuffer win) (gl::fnsCreatePBuffer (glcontext win) width height))
    (format t "~%Made pbuffer...")
    ))


(defmethod drawable-p ((win cocoa-offscreen-window)) t)

(defmethod glMakeCurrent ((window cocoa-offscreen-window))
  ;;; The make/unmake pair was introduced for Cocoa, where the API
  ;;; contains an "unmake current" function.  
  (gl::fnsMakePBufferCurrent (pbuffer window) (glcontext window))
  )

(defmethod glMakeCurrent ((window cocoa-offscreen-window))
  ;;; The make/unmake pair was introduced for Cocoa, where the API
  ;;; contains an "unmake current" function.  
  (gl::fnsMakePBufferCurrent (pbuffer window) (glcontext window))
  )


(defclass gl-offscreen-window (cocoa-offscreen-window)
    ())
