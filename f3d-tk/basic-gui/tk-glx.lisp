(in-package :gui)

;;(defparameter *default-pane-class* 'tk-glx-pbuffer-window) ; might run out of resources
(defvar *default-pane-class* 'tk-glx-bbuffer-window)
;;(defvar *default-pane-class* 'tk-glx-window)

(defclass GLX-visual  (basic-visual)
    ((screen-num :accessor screen-num)
     (xdisplay :accessor xdisplay)
     (sharelist-context :accessor sharelist-context)
     (fbconfig :accessor fbconfig)
     ))

;;; ***************  TK-GLX-WINDOW CLASS AND GLX INTERFACE  ******************

(defclass tk-glx-window (tkgl-window) ; (basic-window)
  ((visual :initarg :visual :accessor visual)
   ;; flush screen-num and xdisplay(?) - get from visual
   (xdisplay :initform nil :accessor xdisplay)
   (screen-num :initform 0 :accessor screen-num)
   (xwindow :initform nil :initarg :xwindow :accessor xwindow)
   (glcontext :initarg :glcontext :accessor glcontext)
   ))


(defclass tk-glx-bbuffer-window (gl-bbuffer-window-mixin tk-glx-window)
  ())

(defclass tk-glx-pbuffer-window (gl-pbuffer-window-mixin tk-glx-window)
  ())

(defmethod glMakeCurrent ((window tk-glx-window))
 ; (setq gui::*foo* window)   (break)
  ;(glFinish) ; this is needed for cross machine
  (gl::glXMakeCurrent (xdisplay window) (xwindow window) (glcontext window)))


(defvar *restore-gl-context* t) 

;;; Clean this up.  Need to check on the semantics of (glXMakeCurrent
;;; display 0 0).  In theory, this call should ensure that no window
;;; is the "current" window.  We don't REALLY need this in X11, but
;;; it's equivalent to a similar call in Cocoa that we must use.
;;;
(defvar *glx-unmake-current* nil)

(defmethod glUnMakeCurrent ((window tk-glx-window))
  ;;; The make/unmake pair was introduced for Cocoa, where the API
  ;;; contains an "unmake current" function.  
  (when *glx-unmake-current*
    (gl::glXMakeCurrent (xdisplay window) 0 0))
  )


;;; Calling (glXMakeCurrent display 0 0) is probably a good idea,
;;; since it allows us to isolate code whose GL context is
;;; unspecified.  Errors will be thrown when there is no "current"
;;; context:

;;; Experiment - maintains a context stack, but I'm not sure this is doing the right thing.

#+never
(defmethod glUnMakeCurrent ((window tk-glx-window))
  (declare (special gl::*gl-context-stack*))
  (when *restore-gl-context*
    (let ((prev-window (car gl::*gl-context-stack*)))
      (if prev-window
	  (gl::glXMakeCurrent (xdisplay prev-window) (xwindow prev-window) (glcontext prev-window))
	  (gl::glXMakeCurrent (xdisplay window) 0 0)
	  ))))

#|
(let ((window (nth 0 gui::*foo*))) 
  (gl::glXMakeCurrent (xdisplay window) (xwindow window) (glcontext window)))

(describe gui::*foo*)
|#

(defmethod glSwapBuffers ((window tk-glx-window))
  (gl::glXSwapBuffers (xdisplay window) (xwindow window)))

;;; So this, in particular, breaks the contract that glMakeCurrent
;;; would otherwise have.

(defmethod gl-make-current ((window tk-glx-pbuffer-window))
  (let ((pbuffer (make-window-pbuffer window)))
    (gl::glxMakeContextCurrent (xdisplay window)
			       pbuffer pbuffer
			       (pbuffer-context window))
    (glDrawBuffer GL_FRONT)
    ))

(defmethod copy-to-front ((window tk-glx-pbuffer-window) &optional (unmake-current t))
  (glFinish)				; needed for cross machine
  (gl::glxMakeContextCurrent (xdisplay window)
			     (xwindow window) (pbuffer window)
			     (pbuffer-context window))
  (normalize-gl-state) ; This is (apparently) important for indirect rendering			
  (glReadBuffer GL_FRONT)
  (glDrawBuffer GL_FRONT)
  (glDisable GL_DEPTH_TEST)
  (glDepthMask 1)
  (mv-bind (width height) (dimensions window)
    (glCopyPixels 0 0 width height
		  GL_COLOR
		  ;;(logior GL_COLOR GL_DEPTH)
		  ))
  (when unmake-current (gl-unmake-current window))
  )

(defmethod backing-store-valid-p ((window tk-glx-pbuffer-window))
  (back-buffer-state window))

(defun window-grabbed-p (window)
  (tk::window-grabbed-p (widget window)))

(defun initialize-window-x-params (window)
  (unless (xdisplay window)
    (when (widget window)
      (let ((screen-spec (tk::widget-screen (tk::widget-toplevel (widget window)))))
        (unless (plusp (length screen-spec))
          (setf screen-spec ":0.0"))
        (mv-bind (xdisplay screen-num)
            (gl::xdisplay-and-screen-num screen-spec)
          (setf (xdisplay window) xdisplay
                (screen-num window) screen-num))))))

(defmethod initialize-instance :after ((window tk-glx-window) &key &allow-other-keys)
  ;; Sometimes need to defer this step, esp when the panel is not yet
  ;; completely configured:
  (initialize-window-x-params window))

(defmethod drawable-p ((window tk-glx-window))
  (not (null (xwindow window))))

;;; This is for both tk-glx-window and tk-glx-bbuffer-window
(defmethod default-glx-attribute-lists ((window tk-glx-window))
  (list   
   (list gl::GLX_RGBA
	 gl::GLX_RED_SIZE 8
	 gl::GLX_GREEN_SIZE 8
	 gl::GLX_BLUE_SIZE 8
	 ;;gl::GLX_ALPHA_SIZE 1
	 gl::GLX_DOUBLEBUFFER
	 gl::GLX_DEPTH_SIZE 32)

   #+macosx
   (list gl::GLX_RGBA
	 gl::GLX_RED_SIZE 8
	 gl::GLX_GREEN_SIZE 8
	 gl::GLX_BLUE_SIZE 8
	 ;;gl::GLX_ALPHA_SIZE 1
	 gl::GLX_DOUBLEBUFFER
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

#|
(eval-cache-flush-function 'default-glXVisual)
|#

(defun default-glXVisualInfo (window screen-spec)
  (let ((pane-class (class-of window)))
    (eval-cache (default-glXVisual pane-class screen-spec)
      ;;(format t "default-glXVisualInfo ~a ~a~%" pane-class screen-spec)
      (multiple-value-bind (xdisplay screen-num)
	  (gl::xdisplay-and-screen-num screen-spec)
	(gl::glXChooseVisual (default-glx-attribute-lists window)
			     xdisplay screen-num)))))

;;(eval-cache-flush-function 'default-glXVisual)
(defun default-glXVisualInfo (window screen-spec)
  (let ((pane-class (class-of window))
	(xdisplay (xdisplay window)))
    (eval-cache (default-glXVisual pane-class screen-spec)
      ;;(format t "default-glXVisualInfo ~a ~a~%" pane-class screen-spec)
      (multiple-value-bind (ignore screen-num)
	  (gl::xdisplay-and-screen-num screen-spec)
	(gl::glXChooseVisual (default-glx-attribute-lists window)
			     xdisplay screen-num)))))



#|
(default-glXVisualInfo (make-instance 'tk-glx-pbuffer-window) ":0.0")
(gl::F3D_visinfo_visualid (default-glXVisualInfo (make-instance 'tk-glx-pbuffer-window) ":0.0"))
(tk::widget-screen ".")
(gl::F3D_visinfo_visualid *init-state-visualinfo*)

(gl::describe-glXFBConfig
 (default-glXVisualInfo (make-instance 'tk-glx-bbuffer-window) ":0.0"))
(gl::describe-glXFBConfig
 (default-glXVisualInfo (make-instance 'tk-glx-bbuffer-window)
     (tk::widget-screen  ".")))
|#

(defmethod init-state ((window tk-glx-window) widget)
  (unless (xwindow window)
    (let* ((screen-spec (tk::widget-screen widget))
	   (visualinfo (default-glXVisualInfo window screen-spec))
	   (sharelist (gl::get-global-glx-sharelist-context
		       visualinfo screen-spec)))
      #+never
      (format t "init-state visualid = ~a~%"
	      (list (gl::F3D_visinfo_visualid  visualinfo)
		    (window-width widget) (window-height widget)
		    sharelist))
      
      (multiple-value-bind (xdisplay screen-num)
	  (gl::xdisplay-and-screen-num screen-spec)
	(setf (xdisplay window) xdisplay
	      (screen-num window) screen-num
	      (xwindow window) (tk::widget-xwindow widget)
	      (glcontext window) (gl::glXCreateContext
				  :xvisualinfo visualinfo
				  :xdisplay xdisplay
				  :screen-num screen-num
				  :sharelist sharelist))
	;;(setq *foo2* (list widget window))
	;(break)
	(when (typep window 'tk-glx-pbuffer-window)
	  ;; It appears that there is no reason for the pbuffer context
	  ;; to be different for that used for the window.

	  (if t
	      (setf (pbuffer-context window) (glcontext window))

	      (setf (pbuffer-context window)
		    (gl::make-pbuffer-context screen-spec
					      (default-fbconfig screen-spec)
					      visualinfo sharelist))
	      ))))))

;;;  new Sun Jan 25 2004 -- attempt to support multiple screens
(defmethod tk-gl-window-script
           ((gl-pane-class tk-glx-window)
            widget-name 
            &key width height &allow-other-keys)
  (let ((tk-binding-name "image_pane"))
    (create-tkglwin-bindings tk-binding-name)
    `((frame ,widget-name -width ,width -height ,height
	     -takefocus 1
	     -background "" ; This is needed in tk8.5 to prevent clearing the frame
					; Something else causes the background to be changed.
					; See DisplayFrame in tkFrame.c
	   ;;  -container true ;; prevents clearing the frame on expose and visibility events (in tk8.4)
	     )
      ;; Does the bindtags order really matter?
      (bindtags ,widget-name (concat (bindtags ,widget-name) ,tk-binding-name))
      ;(bindtags ,widget-name (concat ,tk-binding-name (bindtags ,widget-name)))
      )))
#|
(tcl-cmd `(,(widget (selected-window)) configure :background ""))
(tcl-cmd '(source "/homedir/quam/downloads/tcltk/cvs/tk/library/demos/ttk_demo-hacked.tcl"))
|#

(defmethod tk-gl-window-script
           ((gl-pane-class tk-glx-pbuffer-window)
            widget-name 
            &key width height screen-spec &allow-other-keys)
  (let ((tk-binding-name "image_pane")
	(visual-specs (default-glx-attribute-lists (pane-class-instance *default-pane-class*)))
	(screen-spec (or screen-spec ":0.0"))
	)
    (create-tkglwin-bindings tk-binding-name)
    (multiple-value-bind (xdisplay screen-num)
	(gl::xdisplay-and-screen-num screen-spec)
      (let ((visualid (gl::glXChooseVisualid visual-specs xdisplay screen-num)))
        ;;(format t "tk-gl-window-script visualid = ~a~%"  visualid)
        (when (= visualid 0)
          (error "Cannot find glX visual on display ~a with attributes ~a~%"
                 screen-spec visual-specs ))
        ;; See man Tk_GetVisual for full documentation of allowed values for -visual
        ;; We really need a "naked-frame" widget that doesn't attempt to display anything itself.
        ;; I suspect that using "-container true" is fragile.
        `((frame ,widget-name -width ,width -height ,height
	   -visual ,visualid ;; pbuffers apparently require this
	   -takefocus 1
	   -background ""
           ;; -container true ;; prevents clearing the frame on expose and visibility events
           )
          (bindtags ,widget-name (concat (bindtags ,widget-name) ,tk-binding-name))
          )))))


#+never ; old Fri Dec 24 2004
(defmethod tk-gl-window-script
           ((gl-pane-class tk-glx-pbuffer-window)
            widget-name 
            &key width height &allow-other-keys)
  (let ((tk-binding-name "image_pane")
	(visual-specs (default-glx-attribute-lists (pane-class-instance *default-pane-class*)))
	)
    (create-tkglwin-bindings tk-binding-name)
    (multiple-value-bind (xdisplay screen-num)
	(gl::xdisplay-and-screen-num (tk::widget-screen (tk::widget-toplevel widget-name)))
      (let ((visualid (gl::glXChooseVisualid visual-specs xdisplay screen-num)))
        ;;(format t "tk-gl-window-script visualid = ~a~%"  visualid)
        (when (= visualid 0)
          (error "Cannot find glX visual on display ~a with attributes ~a~%"
                 (tk::widget-screen (tk::widget-toplevel widget-name))
                 visual-specs ))
        ;; See man Tk_GetVisual for full documentation of allowed values for -visual
        ;; We really need a "naked-frame" widget that doesn't attempt to display anything itself.
        ;; I suspect that using "-container true" is fragile.
        `((frame ,widget-name -width ,width -height ,height
	   -visual ,visualid ;; pbuffers apparently require this
           -container true ;; prevents clearing the frame on expose and visibility events
           )
          (bindtags ,widget-name (concat (bindtags ,widget-name) ,tk-binding-name))
          )))))




#|
(gl::glXChooseVisualid (gl::default-glx-attribute-lists))


(defun widget-xdisplay-and-screen-num (widget)
  (values (tk::xdisplay) 0))

(defun widget-xdisplay (widget)
  (tk::xdisplay))

(widget-xdisplay-and-screen-num ".")

(tk::tcl-cmd '(qtoplevel .top1 -title "foo" -screen "sleepy:0.0"))
(widget-xdisplay-and-screen-num ".top1")
(tk::widget-screen ".top1")
(tk::tcl-cmd '(destroy ".top1"))

;; This doesn't work because sleepy has no GLX or OpenGL.
(make-cme-frame "cme-frame" :screen-spec "sleepy:0.0")

(progn *global-glx-sharelist-context*)
(describe *foo-window*)
(tk::widget-xwindow (widget *foo-window*))
(describe (gui::panel (gui::view-window (gui::top-view))))
(describe (gui::view-window (gui::top-view)))
(tk::xdisplay )
(loop for (name . pane) in (gui::frame-pane-alist (gui::panel (gui::view-window (gui::top-view))))
      do (describe pane))

(glMakeCurrent (gui::view-window (gui::top-view)))
(tk::tcl-cmd `(winfo visual ,(tk:widget *foo-window*)))
(tk::tcl-cmd `(winfo visualsavailable ,(tk:widget *foo-window*)))
(tk::tcl-cmd `(winfo screen ,(tk:widget *foo-window*)))

(tk::widget-toplevel ".foo.bar")
(tk::widget-screen ".foo")
|#





;;; ************  PBUFFER SUPPORT  ************

(defparameter *enable-pbuffers* t)

(defmethod default-glx-attribute-lists ((window tk-glx-pbuffer-window))
  (list (list gl::GLX_RGBA
	      gl::GLX_RED_SIZE 8
	      gl::GLX_GREEN_SIZE 8
	      gl::GLX_BLUE_SIZE 8
	      ;;gl::GLX_ALPHA_SIZE 1
	      ;;gl::GLX_DOUBLEBUFFER
	      gl::GLX_DEPTH_SIZE 32)
	(list gl::GLX_RGBA
	      gl::GLX_RED_SIZE 5
	      gl::GLX_GREEN_SIZE 5
	      gl::GLX_BLUE_SIZE 5
	      ;;gl::GLX_ALPHA_SIZE 1
	      ;;gl::GLX_DOUBLEBUFFER
	      gl::GLX_DEPTH_SIZE 24)
	(list gl::GLX_RGBA
	      gl::GLX_RED_SIZE 5
	      gl::GLX_GREEN_SIZE 5
	      gl::GLX_BLUE_SIZE 5
	      ;;gl::GLX_ALPHA_SIZE 1
	      ;;gl::GLX_DOUBLEBUFFER
	      gl::GLX_DEPTH_SIZE 16)))

#+never ;; double-buffer version
(defmethod default-glx-attribute-lists ((window tk-glx-pbuffer-window))
  (list (list gl::GLX_RGBA
	      gl::GLX_RED_SIZE 8
	      gl::GLX_GREEN_SIZE 8
	      gl::GLX_BLUE_SIZE 8
	      ;;gl::GLX_ALPHA_SIZE 1
	      gl::GLX_DOUBLEBUFFER
	      gl::GLX_DEPTH_SIZE 32)
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

(defun default-fbconfig (screen-spec)
  (multiple-value-bind (xdisplay screen-num)
      (gl::xdisplay-and-screen-num screen-spec)
    (gl::min-cost-FBConfig xdisplay
			   ;;:attrs gl::*FBConfig-double-rgb8-depth24-attributes*
			   :attrs gl::*FBConfig-rgb8-depth24-attributes*
			   :screen screen-num)))

				   
(defun delete-pbuffer (window)
  (let ((pbuffer (pbuffer window)))
    (when pbuffer
      (gl::glXDestroyPbuffer (xdisplay window) pbuffer)
      (setf (pbuffer window) nil))))

(defun make-window-pbuffer (window)
  (when *enable-pbuffers*
    (mv-bind (width height) (dimensions window)
      (let ((pbuffer (pbuffer window))
	    (xdisplay (xdisplay window)))
	(if (and pbuffer
		 (mv-bind (pbwid pbhi)
		     (gl::pbuffer-dimensions xdisplay pbuffer)
		   (and (eql width pbwid) (eql height pbhi))))
	    pbuffer
	    (let* ((screen-spec (tk::widget-screen (widget window)))
		  (config (default-fbconfig screen-spec)))
	      #+never
	      (format t "make-window-pbuffer visualid = ~a~%"
		      (gl::glXGetFBConfigAttrib xdisplay config gl::GLX_VISUAL_ID ))
	      (delete-pbuffer window)
	      #+never
	      (unless (pbuffer-context window)
		(setf (pbuffer-context window)
		      (gl::make-pbuffer-context screen-spec config visualinfo)))
	      (setf (pbuffer window)
		    (gl::make-pbuffer xdisplay config width height))))))))

#|
(selected-window *interactor*)
(pbuffer (selected-window *interactor*))
(gl::pbuffer-dimensions (tk::xdisplay) (pbuffer (selected-window *interactor*)))
(pbuffer-context (selected-window *interactor*))
(make-window-pbuffer (view-window (top-view)))
(check-gl-errors "foo")
(dimensions (selected-window *interactor*))
(gl::describe-glXFBConfig
 (default-fbconfig (tk::widget-screen (widget (view-window (top-view)))))
 (tk::xdisplay))
|#

#|
(setq *enable-pbuffers* nil)
(setq *default-draw-buffer* GL_FRONT)
(setq *default-draw-buffer* GL_BACK)

(pbuffer (view-window (top-view)))

(let* ((window (view-window (top-view)))
       (pbuffer (pbuffer window)))
  (gl::glxMakeContextCurrent (tk::xdisplay)
			     pbuffer pbuffer (glcontext window)))

(delete-pbuffer (view-window (top-view)))

(let ((window (view-window (top-view))))
  (glmakecurrent window)
  (normalize-gl-state))

|#




#|
;;; Sun Mar  7 2004 LHQ        FAILED ATTEMPT TO ENABLE BACKING STORE.

;;; This doesn't work, apparently because XFree86 with the NVidia driver doesn't
;;; enable any form of backing store.  From /var/log/XFree86.0.log:
;;; (==) NVIDIA(1): Backing store disabled

;;; defined in glx-ffi.lisp
(declaim (special gl::XSetBackingStore_WhenMapped)

(defun set-window-backing-store (window &optional (flag gl::XSetBackingStore_WhenMapped))
  (gl::XSetBackingStore (xdisplay window) (xwindow window) flag))

(defun set-widget-backing-store (widget &optional (flag gl::XSetBackingStore_WhenMapped))
  (gl::XSetBackingStore (gl::xdisplay-and-screen-num (tk::widget-screen widget))
			(tk::widget-xwindow widget) flag))

#|
(set-window-backing-store (panel (view-window (top-view))))
(set-widget-backing-store (tk::widget-toplevel (widget (view-window (top-view)))))
(tk::widget-toplevel (widget (view-window (top-view))))
(tk::widget-xwindow (widget (panel (view-window (top-view)))))

|#

|#

;;;
;;; Offscreen GLX windows: These will behave like regular windows, but
;;; are intended for offscreen rendering, e.g., when emitting web
;;; pages or status reports.  GRAB-GL-BUFFER will work on these
;;; windows.
;;;
(defvar *default-offscreen-screen-spec* ":0.0")

(defclass glx-offscreen-window (basic-window)
    ((xdisplay :initform nil :accessor xdisplay)
     (screen-num :initform 0 :accessor screen-num)
     (config :initform nil :initarg :config :accessor config)
     (glcontext :initarg :glcontext :accessor glcontext)
     ))


(define-soft-slot glx-offscreen-window :screen-spec)

(defun offscreen-glx-attribute-lists ()
  (list 
   (list gl::GLX_RGBA
         gl::GLX_RED_SIZE 8
         gl::GLX_GREEN_SIZE 8
         gl::GLX_BLUE_SIZE 8
         gl::GLX_DOUBLEBUFFER
         gl::GLX_DEPTH_SIZE 32)

   (list gl::GLX_RGBA
         gl::GLX_RED_SIZE 8
         gl::GLX_GREEN_SIZE 8
         gl::GLX_BLUE_SIZE 8
         gl::GLX_DOUBLEBUFFER ;; We don't need double buffering, do we??
         gl::GLX_DEPTH_SIZE 24)


   (list gl::GLX_RGBA
         gl::GLX_RED_SIZE 8
         gl::GLX_GREEN_SIZE 8
         gl::GLX_BLUE_SIZE 8
         gl::GLX_DEPTH_SIZE 24)

   (list gl::GLX_RGBA
         gl::GLX_RED_SIZE 8
         gl::GLX_GREEN_SIZE 8
         gl::GLX_BLUE_SIZE 8
         gl::GLX_DOUBLEBUFFER
         gl::GLX_DEPTH_SIZE 16)

   (list gl::GLX_RGBA
         gl::GLX_RED_SIZE 8
         gl::GLX_GREEN_SIZE 8
         gl::GLX_BLUE_SIZE 8
         gl::GLX_DEPTH_SIZE 32)

   (list gl::GLX_RGBA
         gl::GLX_RED_SIZE 8
         gl::GLX_GREEN_SIZE 8
         gl::GLX_BLUE_SIZE 8
         gl::GLX_DEPTH_SIZE 24)

   (list gl::GLX_RGBA
         gl::GLX_RED_SIZE 8
         gl::GLX_GREEN_SIZE 8
         gl::GLX_BLUE_SIZE 8
         gl::GLX_DEPTH_SIZE 16)

   (list gl::GLX_RGBA
         gl::GLX_RED_SIZE 5
         gl::GLX_GREEN_SIZE 5
         gl::GLX_BLUE_SIZE 5
         gl::GLX_DOUBLEBUFFER
         gl::GLX_DEPTH_SIZE 24)
	
   (list gl::GLX_RGBA
         gl::GLX_RED_SIZE 5
         gl::GLX_GREEN_SIZE 5
         gl::GLX_BLUE_SIZE 5
         gl::GLX_DOUBLEBUFFER
         gl::GLX_DEPTH_SIZE 16)))


;;; This is hokey, and only exists to provide a meaningful result for
;;; PRINT-OBJECT.  Without it, VIEW and WINDOW objects have no
;;; identifying string.  This definition shows the window "top" state.

(defmethod widget ((win glx-offscreen-window))
  (let ((view (top-view win)))
    (if view
        (let ((site (3d-world view)))
          (if site
              (format nil "~a/~a" (name site) (name (2d-world view)))
              (name (2d-world view))))
        "Empty")))
  

(defun choose-offscreen-fbconfig (&key (screen-spec *default-offscreen-screen-spec*))
  (let ((visual-specs (offscreen-glx-attribute-lists)))
    (multiple-value-bind (xdisplay screen)
	(gl::xdisplay-and-screen-num screen-spec)
      (values
       (first (gl::glXGetFBConfigs xdisplay))
       xdisplay
       screen))))

;;;
;;; Not really sure if this is correct:
;;;
(defun choose-offscreen-fbconfig (&key (screen-spec *default-offscreen-screen-spec*))
  (let ((visual-specs (offscreen-glx-attribute-lists)))
    (multiple-value-bind (xdisplay screen)
	(gl::xdisplay-and-screen-num screen-spec)
      (values
       (or
        (loop for attr-list in visual-specs
              for config = (ignore-errors (gl::getfbconfig xdisplay attr-list))
              when (and config (plusp config)) do (return config))
        (gl::getfbconfig xdisplay)) ;; Just try for the first FBconfig if we can't get any others...
      xdisplay
      screen)))
  )


(defmethod make-offscreen-gl-context ((win glx-offscreen-window))
  ;; Assumes some initialization:
  (gl::glXCreateContext_int
   (xdisplay win)
   (gl::glXGetVisualFromFBConfig (xdisplay win) (config win))
   0 0))

(defmethod initialize-instance :after ((win glx-offscreen-window) &rest args &key (width 640) (height 480) (screen-spec *default-offscreen-screen-spec*) &allow-other-keys)
  (setf (dimensions win) (list width height))
;;  (format t "~%INITIALIZE-INSTANCE :AFTER glx-offscreen-window: ~dx~d" width height)
  (multiple-value-bind (fbconfig xdisplay screen)
      (choose-offscreen-fbconfig :screen-spec screen-spec)
    (setf (screen-spec win) screen-spec)
    (setf (xdisplay win) xdisplay)
    (setf (screen-num win) screen)
    (setf (config win) fbconfig)
    (setf (glcontext win) (make-offscreen-gl-context win))
    ;; (print 'ok)
    ))


(defmethod drawable-p ((win glx-offscreen-window)) t)

(defmethod glUnMakeCurrent ((window glx-offscreen-window))
  ;;; The make/unmake pair was introduced for Cocoa, where the API
  ;;; contains an "unmake current" function.  
  (gl::glXMakeCurrent (xdisplay window) 0 0)
  )


;;;
;;; Offscreen rendering to pixmaps:
;;;

(defclass glx-offscreen-pixmap-window (glx-offscreen-window)
    ((pixmap :initform nil :initarg :pixmap :accessor pixmap)
     ))


(defmethod initialize-instance :after ((win glx-offscreen-pixmap-window) &rest args &key (width 640) (height 480) &allow-other-keys)
  (format t "~%INITIALIZE-INSTANCE GLX-OFFSCREEN-PIXMAP-WINDOW width=~d, height=~d" width height)
  (let* ((x-display (xdisplay win))
         (pixmap (gl::XCreatePixmap x-display (gl::XRootWindow x-display (screen-num win)) width height 24)))
    (setf (pixmap win)
          (gl::glXCreatePixmap x-display (config win) pixmap 0))
    ))


(defmethod glMakeCurrent ((window glx-offscreen-pixmap-window))
  (gl::glXMakeCurrent (xdisplay window) (pixmap window) (glcontext window))
  )


(defmethod glUnmakeCurrent :before ((window glx-offscreen-pixmap-window))
  (glFlush)
  )


#||
(setq window (make-instance 'glx-offscreen-pixmap-window))
(setq site (cme::generic-load-site "Loyola-Marymount" cme::*radius-site-glue* :load-pyramids '("campus-ortho" "lioncam1")))
(setq view (push-image (first (image-list (first (2d-worlds site)))) window))
(setq image (grab-gl-buffer :window window))

;;; IMAGE should now be a capture of the offscreen VIEW.

||#

;;; Pbuffer approach:

;;;
;;; Offscreen rendering to pixmaps:
;;;

(defclass glx-offscreen-pbuffer-window (glx-offscreen-window)
    ((pbuffer :initform nil :initarg :pbuffer :accessor pbuffer)
     ))



(defmethod make-offscreen-gl-context ((win glx-offscreen-pbuffer-window))
  ;; Assumes some initialization:
  (gl::glXCreateNewContext (xdisplay win)
                       (config win)
                       gl::GLX_RGBA_TYPE
                       0
                       0)
  ;; (gl::make-pbuffer-context (screen-spec win) (config win) (gl::glXGetVisualFromFBConfig (xdisplay win) (config win)))
  )


(defmethod initialize-instance :after ((win glx-offscreen-pbuffer-window) &rest args &key (width 640) (height 480) &allow-other-keys)
  (setf (pbuffer win)
        (gl::make-pbuffer (xdisplay win) (config win) width height)
	))

#||
        (gl::glXCreatePbuffer
         (xdisplay win)
         (config win)
         (gl::make-glx-attribute-array
          (list gl::GLX_PBUFFER_WIDTH width
                gl::GLX_PBUFFER_HEIGHT height)))
||#


(defmethod glMakeCurrent ((window glx-offscreen-pbuffer-window))
  ;;(gl::glXMakeCurrent (xdisplay window) (pbuffer window) (glcontext window))
  (gl::glXMakeContextCurrent (xdisplay window) (pbuffer window) (pbuffer window) (glcontext window))
  (glFinish)
  )

(defmethod glUnmakeCurrent :before ((window glx-offscreen-pbuffer-window))
  (glFlush)
  )


;;(defclass gl-offscreen-window (glx-offscreen-pixmap-window)    ())

(defclass gl-offscreen-window (glx-offscreen-pbuffer-window)    ())


(defmethod set-window-border-color ((window gl-offscreen-window) color) nil)
