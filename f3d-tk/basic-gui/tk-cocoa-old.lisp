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
  

;;; ******************  TK-COCOA-WINDOW CLASS AND COCOA INTERFACE  ******************

;;; START COCOA DEPENDENCIES
;;;

(defclass tk-cocoa-window (tkgl-window)
  ((win-id :initform nil :initarg :id :accessor win-id) 
   (gl-ctx :initform nil :initarg :gl-ctx :accessor gl-ctx) ;; GL Graphics Context
   ))

(defclass tk-cocoa-bbuffer-window (gl-bbuffer-window-mixin tk-cocoa-window)
    ())

(defmethod drawable-p ((window tk-cocoa-window))
  (not (null (gl-ctx window))))

(defmethod glMakeCurrent ((window tk-cocoa-window))
  (let ((rc (gl::fnsMakeCurrent (win-id window) (gl-ctx window))))
    (when (zerop rc)
      (format t "~%glMakeCurrent failed."))
    rc))


(defmethod glSwapBuffers ((window tk-cocoa-window))
  (gl::fnsSwapBuffers (gl-ctx window)))


(defparameter *cocoa-rendering-spec* nil)

(defparameter *cocoa-share-window* nil)

(defun get-global-cocoa-sharelist-context ()
  (and *cocoa-share-window*
       (gl-ctx *cocoa-share-window*)))


(defvar *disable-init-state* nil)


;;; Cannot do these easily in Tcl/Tk under Aqua: These two below should be
;;; paired in an unwind-protect whenever they're used, probably...

;;(defparameter *enable-cursor-blanking* nil)
(defparameter *enable-cursor-blanking* t)
(defvar *MouseCoalescing-original-state* nil)

(defun gl::cocoaSetMouseCoalescing (&rest args) 0)

(defmethod set-blank-cursor ((window tk-cocoa-window))
  ;; I took this out because of persistent "disappearance" of the cursor.
  (when *enable-cursor-blanking*
    (gl::fnsHideCursor)
    (let ((old-state (gl::fnsSetMouseCoalescing 0))) ;; not defined anymore??
      (unless *MouseCoalescing-original-state*
	(setq *MouseCoalescing-original-state* old-state)))
    ))

(defmethod unset-blank-cursor ((window tk-cocoa-window))
  (when *enable-cursor-blanking*
    (gl::fnsShowCursor)
    (when *MouseCoalescing-original-state*
      (gl::fnsSetMouseCoalescing *MouseCoalescing-original-state*))
    (set-cursor window "arrow") ;; need this in case the cursor went kooky on us.
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
      

(defmethod init-state ((window tk-cocoa-window) widget)
  (unless (or t *disable-init-state* (gl-ctx window))
    (let* ((window-id (tk::widget-xwindow widget)) ;; Under windows, this is the HWND
	   )
      (setf (win-id window) window-id)
      (format t "~%init-state (cocoa):  window-id = ~d" window-id)
      (unless *cocoa-rendering-spec*
	(setq *cocoa-rendering-spec* (gl::fnsMakeRenderingSpec)))
	  
      (loop with spec = *cocoa-rendering-spec*
	    for attrs in (gl::default-opengl-attribute-lists)
	    for context = (gl::fnsCreateContext
			     window-id
			     (gl::set-fns_rendering_spec spec attrs))
	    while (zerop context)
	    finally
	 (if (zerop context)
	     (format t "fnsCreateContext failed for ~a" attrs)
	     (setf (gl-ctx window) context))
	    )

      (when (gl-ctx window) 
	(unless *cocoa-share-window*  (setf *cocoa-share-window* window))
    
	(if (zerop (gl-ctx window))
	    (error "cocoaCreateContext failed")
	    (format t "~%New GL context: ~x" (gl-ctx window))
	    )))))

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

(defvar *default-pane-class* 'tk-cocoa-bbuffer-window)

(eval-when (eval load)
  (setq *default-pane-class* 'tk-cocoa-bbuffer-window))



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

