(in-package :gui)

;;; 29 January 2010: AGL (MacOSX OpenGL extensions API) is deprecated
;;; and is only supported in 32-bit mode, so we need to move to Cocoa.
;;; Cocoa support is through an Objective-C API.  Consider creating a
;;; tk-cocoa.lisp file.  Perhaps we can avoid creating yet another C
;;; file by using message passing from Lisp.

;;; This needs to move to a generic place and
;;; DEFAULT-GLX-ATTRIBUTE-LISTS in glx-ffi.lisp needs to derive from this.
(defun gl::default-opengl-attribute-lists ()
  '((:RGBA
     :RED-SIZE 8
     :GREEN-SIZE 8
     :BLUE-SIZE 8
     ;;:ALPHA-SIZE 1
     :DOUBLEBUFFER
     :DEPTH-SIZE 24)
    (:RGBA
     :DOUBLEBUFFER
     :DEPTH-SIZE 32)
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
  

;;; ******************  TK-AGL-WINDOW CLASS AND AGL INTERFACE  ******************

(defclass tk-agl-window (tkgl-window)
  ((win-id :initform nil :initarg :id :accessor win-id)       ; agl DC (ie the counterpart to xwindow)
   (gl-ctx :initform nil :initarg :gl-ctx :accessor gl-ctx) ; agl context 
   ))

(defclass tk-agl-bbuffer-window (gl-bbuffer-window-mixin tk-agl-window)
    ())

;;; With gl-bbuffer-window-mixin added this isn't needed anymore
;;;(defmethod render-window ((window tk-agl-bbuffer-window) from-backing-store)
;;;  (let ((view (top-view window)))
;;;    (when (and from-backing-store
;;;               (backing-store-valid-p window))
;;;      (copy-to-front window)
;;;      (render-selected-objects-in-front-buffer view)
;;;      (debug-print "b")
;;;      (return-from render-window nil))
;;;
;;;    (clear-feedback-cache)
;;;;;    (gl-make-current window)
;;;;;    (glFinish)                                ; necessary for cross-machine 
;;;    (render-stationary-objects view)
;;;    (copy-to-front window)
;;;    (render-selected-objects-in-front-buffer view)
;;;    (setf (back-buffer-state window) (visible-p window))
;;;    (debug-print "B")
;;;    ))

;;(defvar *default-pane-class* 'tk-agl-window)
(defvar *default-pane-class* 'tk-agl-bbuffer-window)

(defmethod drawable-p ((window tk-agl-window))
  (not (null (gl-ctx window))))

(defmethod glMakeCurrent ((window tk-agl-window))
  (let ((rc (gl::aglMakeCurrent (gl-ctx window))))
    (when (zerop rc)
      (format t "~%glMakeCurrent failed."))
    (gl::aglPreDisplay (win-id window) (gl-ctx window)
		       (window-width window) (window-height window))
    rc))


(defmethod glSwapBuffers ((window tk-agl-window))
  (gl::aglSwapBuffers (win-id window) (gl-ctx window)
		      (window-width window) (window-height window)
		      ))

(defparameter *agl-rendering-spec* nil)

(defparameter *agl-share-window* nil)

(defun get-global-agl-sharelist-context ()
  (and *agl-share-window*
       (gl-ctx *agl-share-window*)))


(defvar *disable-init-state* nil)


;;; Cannot do these easily in Tcl/Tk under Aqua: These two below should be
;;; paired in an unwind-protect whenever they're used, probably...

;;(defparameter *enable-cursor-blanking* nil)
(defparameter *enable-cursor-blanking* t)
(defvar *MouseCoalescing-original-state* nil)

(defun gl::aglSetMouseCoalescing (&rest args) 0)

(defmethod set-blank-cursor ((window tk-agl-window))
  ;; I took this out because of persistent "disappearance" of the cursor.
  (when *enable-cursor-blanking*
    (gl::aglHideCursor)
    (let ((old-state (gl::aglSetMouseCoalescing 0))) ;; not defined anymore??
      (unless *MouseCoalescing-original-state*
	(setq *MouseCoalescing-original-state* old-state)))
    ))

(defmethod unset-blank-cursor ((window tk-agl-window))
  (when *enable-cursor-blanking*
    (gl::aglShowCursor)
    (when *MouseCoalescing-original-state*
      (gl::aglSetMouseCoalescing *MouseCoalescing-original-state*))
    (set-cursor window "arrow") ;; need this in case the cursor went kooky on us.
    ))

(defmethod warp-pointer ((window tk-agl-window) x y)
  (let ((event *most-recent-buttonpress*)
	(pos (root-position window)))
    (when pos
      (destructuring-bind (wx wy)
	  pos
	(gl::aglWarpCursor
	 (round (+ x wx)) (round (+ y wy))
	 )))))
      

(defmethod set-mouse-cursorpos ((window tk-agl-window) x y)
  (let ((event *most-recent-buttonpress*)
	(pos (root-position window)))
    (when pos
      (destructuring-bind (wx wy)
	  pos
	(gl::aglWarpCursor
	 (round (+ x wx)) (round (+ y wy))
	 )))))
      

(defmethod init-state ((window tk-agl-window) widget)
  (unless (or *disable-init-state* (gl-ctx window))
    (let* ((window-id (tk::widget-xwindow widget)) ;; Under windows, this is the HWND
	   (w (window-width widget))
	   (h (window-height widget))
	   ;(x (window-x window))
	   ;(y (window-y window))
	   ;(w (window-width (tk::widget window)))
	   ;(h (window-height (tk::widget window)))
	   )
      (when (and w h)

	(setf (win-id window) window-id)
	(format t "~%init-state:  window-id = ~d" window-id)
	(unless *agl-rendering-spec*
	  (setq *agl-rendering-spec* (gl::aglMakeRenderingSpec)))
	  
	(loop with spec = *agl-rendering-spec*
	      for attrs in (gl::default-opengl-attribute-lists)
	      for pixel-format
		= (gl::aglChooseAndSetPixelformat (gl::set-agl_rendering_spec spec attrs))
	      while (zerop  pixel-format)
	      finally
	   (if (zerop pixel-format)
	       (format t "aglChooseAndSetPixelformat failed for ~a" attrs)
	       (setf (gl-ctx window)
		     (gl::aglCreateContext window-id pixel-format
					   (or (get-global-agl-sharelist-context) 0)
					   w h))
	       ))

	(when (gl-ctx window) 
	  (unless *agl-share-window*  (setf *agl-share-window* window))
    
	  (when (zerop (gl-ctx window))
	    (error "aglCreateContext failed")))))))



#+old
(defmethod tk-gl-window-script
           ((gl-pane-class tk-agl-window)
	    widget-name &key width height &allow-other-keys)
  (let ((tk-binding-name "image_pane"))
    (create-tkglwin-bindings tk-binding-name)
    `((frame ,widget-name -width ,width -height ,height
	     -takefocus true
;;	     -background "" ; This is needed in tk8.5 to prevent clearing the frame
					; Something else causes the background to be changed.
					; See DisplayFrame in tkFrame.c
	     ;; was commented out:
	     -container true ;; prevents clearing the frame on expose and visibility events (in tk8.4)
	     )
      (bindtags ,widget-name (concat (bindtags ,widget-name) ,tk-binding-name))
      )))

(defmethod tk-gl-window-script
           ((gl-pane-class tk-agl-window)
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




;;; A simple example to show how the Apple top-level menu bar can be
;;; populated:
(defun menubar-example ()
  ;; First create a menu widget:
  (tcl-cmd '("menu" ".menuBar" "-tearoff" 0))
  ;; Add a cascade menu called "File":
  (tcl-cmd '(".menuBar" "add" "cascade" "-menu" ".menuBar.file" "-label" "File" "-underline" 0))
  ;; Create the file sub-menu:
  (tcl-cmd '("menu" ".menuBar.file" "-tearoff" 0))
  ;; Add a "Sites" item to it:
  (tcl-cmd '(".menuBar.file" "add" "command" "-label" "Sites" "-command" "siteMenu" "-underline" 0))
  ;; Add a separator:
  (tcl-cmd '(".menuBar.file" "add" "sep"))
  ;; Tell Tk to put this menu in the top-level Apple menu bar:
  (tcl-cmd '("." "configure" "-menu" ".menuBar")))

(eval-when (eval load)
  (setq *default-pane-class* 'tk-agl-bbuffer-window))



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

