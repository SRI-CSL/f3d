(in-package :gui)

;;; This needs to move to a generic place and
;;; DEFAULT-GLX-ATTRIBUTE-LISTS in glx-ffi.lisp needs to derive from this.
(defun gl::default-opengl-attribute-lists ()
  '((:RGBA
     :RED-SIZE 8
     :GREEN-SIZE 8
     :BLUE-SIZE 8
     ;;:ALPHA-SIZE 1
     :DOUBLEBUFFER
     :DEPTH-SIZE 24) ;; was 32
    (:RGBA
     :RED-SIZE 5
     :GREEN-SIZE 5
     :BLUE-SIZE 5
     ;:ALPHA-SIZE 1
     :DOUBLEBUFFER
     :DEPTH-SIZE 24)
    (:RGBA
     :RED-SIZE 8
     :GREEN-SIZE 8
     :BLUE-SIZE 8
     ;;:ALPHA-SIZE 1
     :DOUBLEBUFFER
     :DEPTH-SIZE 32)
    (:RGBA
     :DOUBLEBUFFER
     :DEPTH-SIZE 24) ;; was 32
    (:RGBA
     :RED-SIZE 5
     :GREEN-SIZE 5
     :BLUE-SIZE 5
     ;:ALPHA-SIZE 1
     :DOUBLEBUFFER
     :DEPTH-SIZE 16)
    ))
  

;;; ******************  TK-WGL-WINDOW CLASS AND WGL INTERFACE  ******************

(defclass tk-wgl-window (tkgl-window)
  ((hdc :initform nil :initarg :hdc :accessor hdc)       ; wgl DC (ie the counterpart to xwindow)
   (hglrc :initform nil :initarg :hglrc :accessor hglrc) ; wgl context 
   ))

(defclass tk-wgl-bbuffer-window (gl-bbuffer-window-mixin tk-wgl-window)
    ())

#+never
(defmethod initialize-instance :after ((win tk-wgl-bbuffer-window) &rest args)
  (with-slots (damaged-p back-buffer-state) win
	      (setf damaged-p t
		    back-buffer-state nil)))
  

(defvar *default-pane-class* 'tk-wgl-window)

(defmethod drawable-p ((window tk-wgl-window))
  (not (null (hdc window))))


(defmethod glMakeCurrent ((window tk-wgl-window))
  (let ((rc (gl::wglMakeCurrent (hdc window) (hglrc window))))
    (when (zerop rc)
      (let ((code (gl::wgl_GetLastError)))
	(format t "~%glMakeCurrent failed: code = ~d" code)
	(when (= code 2000) (format t "~%The pixel format is invalid."))))
    rc))


(defmethod glSwapBuffers ((window tk-wgl-window))
  (let ((swapcode (gl::wglSwapBuffers (hdc window))))
    (when (zerop swapcode)
      (let ((errcode (gl::wgl_GetLastError)))
	(format t "SwapBuffers failed.  Error code was ~d" errcode)))
    (gl::glFlush)))

(defparameter *wgl-rendering-spec* nil)

(defparameter *wgl-share-window* nil)

(defun get-global-wgl-sharelist-context ()
  (hglrc *wgl-share-window*))


(defmethod check-state ((window tk-wgl-window))
  (let ((id (tk::widget-xwindow (tk::widget window))))
    (format t "~%window-id: ~d ; hdc: ~d" id (gl::wglGetDC id))))

(defvar *disable-init-state* nil)

;;; In Windoze, we need to get a qualified Drawing Context that refers
;;; to the child window's drawing region.  We need to use GetDCEx for
;;; this.

(defmethod init-state ((window tk-wgl-window) widget)
;;  (glEnable GL_DITHER)
  (tk::do-events)
  (unless (or *disable-init-state* (hglrc window))
    (let* ((window-id (tk::widget-xwindow widget)) ;; Under windows, this is the HWND
	   (hdc (gl::wglGetDCEx window-id 0))
	   (w (window-width widget))
	   (h (window-height widget))
	   ;(w (window-width (tk::widget window)))
	   ;(h (window-height (tk::widget window)))
	   )
      (unless (zerop hdc)
	(when (zerop hdc)
	  (let ((code (gl::wglGetDCExError)))
	    (format t "~%Last error code = ~d" code)))
	(setf (hdc window) hdc)		; is this correct?  Yes.
	(format t "~%window-id = ~x; hdc = ~x; Width=~d height=~d" window-id hdc w h)

	(let ((hdc (hdc window)))
	  (unless *wgl-rendering-spec*
	    (setq *wgl-rendering-spec* (gl::make_wglRenderingSpec)))
	  
	  (loop with spec = *wgl-rendering-spec*
		for attrs in (gl::default-opengl-attribute-lists)
		for pixel-format
		= (gl::wgl_chooseAndSetPixelformat hdc
						   (gl::set-wgl_rendering_spec spec attrs))
		while (zerop  pixel-format)
		finally
		(when (zerop pixel-format) (error "wgl_chooseAndSetPixelformat failed for ~a" attrs)))

	  

	  (setf (hglrc window) (gl::wglCreateContext hdc))

	  (if *wgl-share-window*
	      (gl::wglShareLists (get-global-wgl-sharelist-context) (hglrc window))
	    (setf *wgl-share-window* window))
    
	  (when (zerop (hglrc window))
	    (error "wglCreateContext failed"))
	  )))))




;;; This needs to be fixed to support multiple screens (?)
  


(defmethod tk-gl-window-script
           ((gl-pane-class tk-wgl-window)
	    widget-name &key width height &allow-other-keys)
  (let ((tk-binding-name "image_pane"))
    (create-tkglwin-bindings tk-binding-name)
    `((frame ,widget-name -width ,width -height ,height
	     -background "" ; This is needed in tk8.5 to prevent clearing the frame
					; Something else causes the background to be changed.
					; See DisplayFrame in tkFrame.c
	     ;;  -container true ;; prevents clearing the frame on expose and visibility events (in tk8.4)
	     )
      (bindtags ,widget-name (concat (bindtags ,widget-name) ,tk-binding-name))
      )))
