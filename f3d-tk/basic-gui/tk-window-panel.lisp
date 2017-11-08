(in-package :gui)


;;;(defmethod tk::make-cvv-item (panel parent (type (eql :glwindow)) name label &rest args
;;;                                    &key width height &allow-other-keys)
;;;  (let ((winid (or (get-prop panel :glwindow-counter) 0)))
;;;    (setf (get-prop panel :glwindow-counter) (1+ winid))
;;;    (mv-bind (script wframe-name tkglwin-name)
;;;        (apply 'tk::glwindow-script parent (format nil "~d" winid) args)
;;;      (tcl-eval 'set_global 'default_parent parent)
;;;      (tk::tcl-script script)
;;;      (let* ((clidget (tk::make-cvv-item-clidget panel :glwindow name wframe-name))
;;;             (pane-class *default-pane-class*)
;;;             (window (make-window tkglwin-name :class pane-class)))
;;;        (push (cons (list winid 0) window) (frame-pane-alist panel))
;;;        (setf (panel window) panel)
;;;        clidget))))


;;;
;;; On MacOSX, this works fine.  On Ubuntu, this is broken (as of
;;; 5/11/2012).  The immediate problem seems to be that the X screen
;;; and display numbers are not known by Tcl/Tk when the
;;; initialize-instance method is called for this window class.  My
;;; guess is that the call is happening well before the window is
;;; actually exposed on the screen.
;;;

(defmethod tk::make-cvv-item (panel parent (type (eql :glwindow)) name label &rest args
				    &key width height &allow-other-keys)
  (mv-bind (script wframe-name tkglwin-name)
      (apply 'tk::glwindow-script parent (string-downcase (symbol-name name)) args)
    (tcl-eval 'set_global 'default_parent parent)
    (tk::tcl-script script)
    (let* ((clidget (tk::make-cvv-item-clidget panel :glwindow name wframe-name))
	   (pane-class *default-pane-class*)
	   (window (make-window tkglwin-name :class pane-class)))
      (push (cons name window) (frame-pane-alist panel))
      (setf (panel window) panel)
      clidget)))


#+never ; unfinished
(defmethod tk::make-cvv-item (panel parent (type (eql :glwindow)) name label &rest args
				    &key width height &allow-other-keys)
  (mv-bind (script wframe-name tkglwin-name)
      (apply 'tk::glwindow-script parent (string-downcase (symbol-name name)) args)
    (tcl-eval 'set_global 'default_parent parent)
    (tk::tcl-script `(,@script
		      
		      ))
    (let* ((clidget (tk::make-cvv-item-clidget panel :glwindow name wframe-name))
	   (pane-class *default-pane-class*)
	   (window (make-window tkglwin-name :class pane-class)))
      (push (cons name window) (frame-pane-alist panel))
      (setf (panel window) panel)
      clidget)))





;;; tk dependent parts of window-panel.lisp

;;; TODO: 
;;; This should really be part of the tkgl subsystem rather than basic-gui.
;;; Ie, it should be possible to have tkgl windows without all of basic-gui

(defvar *tkglwin-bindings-created* nil)
#|
(setq *tkglwin-bindings-created* nil)
(setq tk::*tk-verbose* t)
(create-tkglwin-bindings "image_pane")
|#

;;; This handles event bindings for OpenGL windows created by Tcl/Tk.
;;; These windows are ordinary windows with OpenGL operations performed on them.
;;; Get rid of use of keysym -- X11 specific
#+old  ;; anonymous lambdas are a real pain in the ass to debug.
(defmethod create-tkglwin-bindings ((widget-class string))
  (unless *tkglwin-bindings-created*
    (create-tk-binding widget-class "<Configure>" (widget width height)
		       (glwin-configure-callback widget width height))
    (create-tk-binding widget-class "<Visibility>" (widget state button)
		       (glwin-visibility-callback widget state button))
    (create-tk-binding widget-class "<Unmap>" (widget)
		       (glwin-unmap-callback widget))
    #-(or agl cocoa)
    (create-tk-binding widget-class "<Expose>" (widget)
		       (glwin-expose-callback widget))
    (create-tk-binding widget-class "<KeyPress>" (widget keyname ascii state)
		       (glwin-keypress-callback widget keyname ascii state))
    (create-tk-binding widget-class "<KeyRelease>" (widget keyname ascii state)
		       (glwin-keyrelease-callback widget keyname ascii state))
    (create-tk-binding widget-class "<Enter>" (widget)
		       (glwin-enter-callback widget))
    (setq *tkglwin-bindings-created* t)
    ))

;;; Improved debuggability getting rid of anonymous lambdas.
;;; Get rid of use of keysym -- X11 specific
#+old ; Mon Jul  9 2007
(defmethod create-tkglwin-bindings ((widget-class string))
  (unless *tkglwin-bindings-created*
    (tk::create-named-tk-binding tk-glwin-configure-callback widget-class "<Configure>" (widget width height)
      (glwin-configure-callback widget width height))
    (tk::create-named-tk-binding tk-glwin-visibility-callback widget-class "<Visibility>" (widget state button)
      (glwin-visibility-callback widget state button))
    (tk::create-named-tk-binding tk-glwin-unmap-callback widget-class "<Unmap>" (widget)
      (glwin-unmap-callback widget))
    #-(or agl cocoa)
    (tk::create-named-tk-binding tk-glwin-expose-callback widget-class "<Expose>" (widget)
      (glwin-expose-callback widget))

    #+(or agl cocoa)
    (tk::create-named-tk-binding tk-glwin-keypress-callback widget-class "<KeyPress>" (widget keyname ascii state)
      (glwin-keypress-callback widget keyname ascii state))
    #-(or agl cocoa)
    (tk::create-named-tk-binding tk-glwin-keypress-callback widget-class "<KeyPress>" (widget keyname ascii)
      (glwin-keypress-callback widget keyname ascii))
    (tk::create-named-tk-binding tk-glwin-keyrelease-callback widget-class "<KeyRelease>" (widget keyname ascii state)
      (glwin-keyrelease-callback widget keyname ascii state))
    (tk::create-named-tk-binding tk-glwin-enter-callback widget-class "<Enter>" (widget)
      (glwin-enter-callback widget))
    (setq *tkglwin-bindings-created* t)
    ))


(defmethod create-tkglwin-bindings ((widget-class string))
  (unless *tkglwin-bindings-created*
    (tk::create-named-tk-binding tk-glwin-configure-callback widget-class "<Configure>" (widget width height)
      (glwin-configure-callback widget width height))
    (tk::create-named-tk-binding tk-glwin-visibility-callback widget-class "<Visibility>" (widget state button)
      (glwin-visibility-callback widget state button))
    (tk::create-named-tk-binding tk-glwin-unmap-callback widget-class "<Unmap>" (widget)
      (glwin-unmap-callback widget))
    #-(or agl cocoa)
    (tk::create-named-tk-binding tk-glwin-expose-callback widget-class "<Expose>" (widget)
      (glwin-expose-callback widget))
    (tk::create-named-tk-binding tk-glwin-keypress-callback widget-class "<KeyPress>" (widget keyname state)
      (glwin-keypress-callback widget keyname state))
    (tk::create-named-tk-binding tk-glwin-keyrelease-callback widget-class "<KeyRelease>" (widget keyname state)
      (glwin-keyrelease-callback widget keyname state))
    (tk::create-named-tk-binding tk-glwin-enter-callback widget-class "<Enter>" (widget)
      (glwin-enter-callback widget))
    (setq *tkglwin-bindings-created* t)
    ))

#|
(widget (selected-window))
(tk::tcl-cmd `(event generate ,(widget (selected-window)) "<KeyPress>" :keysym "Control_L"))
(progn (tk::tcl-cmd `(focus ,(widget (selected-window))))
       (tk::tcl-cmd `(event generate ,(widget (selected-window)) "<KeyPress>" :keysym "Control_L" :state 0)))
(progn (tk::tcl-cmd `(focus ,(widget (selected-window))))
       (tk::tcl-cmd `(event generate ,(widget (selected-window)) "<KeyRelease>" :keysym "Control_L" :state 4)))

(setq tk::*tk-verbose* t)
(tk::tcl-cmd `(focus ,(widget (selected-window))))
(tk::tcl-cmd `(focus))
(tk::tcl-cmd `(focus :displayof ,(widget (selected-window))))
(tk::tcl-cmd `(focus :lastfor ,(widget (selected-window))))
(tk::tcl-cmd `(puts (focus)))
(tk::tcl-cmd `(puts "foo"))
|#


;;;(defun do-events-for-awhile (&optional (poll-secs .1))
;;;  (let ((tk::*events-verbose* t))
;;;    (declare (special tk::*events-verbose*))
;;;    (loop with start-time = (get-internal-real-time)
;;;          with end-time = (+ start-time (round (* INTERNAL-TIME-UNITS-PER-SECOND poll-secs)))
;;;          do (force-outputs)
;;;             (sleep .1)
;;;             (tk::do-events)
;;;             (when (tk::standard-input-available) ; this doesn't help
;;;               (format t ";; do-events-for-awhile tk::standard-input-available~%")
;;;               (loop while (listen) do (read-char))) 
;;;          until (> (get-internal-real-time) end-time))))

(defun wait-for-windows-drawable (windows &optional out)
  (when t
    (format out "wait-for-windows-drawable:")
    (let ((tk::*events-verbose* out))
      (declare (special tk::*events-verbose*))
      (loop until (loop for window in windows
			always (drawable-p window)) ; loop until all windows are configured
	    do (tk::Tcl_DoOneEvent tk::TCL_ALL_EVENTS )) ; block until an event occurs
      ;; why do we need to wait longer?
      (when nil
	(loop with poll-secs = 1.0
	      with start-time = (get-internal-real-time)
	      with end-time = (+ start-time (round (* INTERNAL-TIME-UNITS-PER-SECOND poll-secs)))
	      do (unless (= (tk::Tcl_DoOneEvent tk::TCL_DONT_WAIT) 1)
		   (sleep .1))		; allow other processes to run
	      until (> (get-internal-real-time) end-time)))
      (format out "~%"))))





;;; This version creates the toplevel widget before constructing the
;;; pane scripts so that we can call (tk::widget-screen
;;; toplevel-widget), making it possible to have cme-frames on
;;; multiple screens.  Perhaps this is unnecessary, since in order for
;;; the toplevel-widget to be on a non-default screen, the -screen
;;; parameter must be passed to the toplevel widget constructor.

;;; Addendum - CC 3/2/2004, if screen-spec is NIL, then make-cme-frame
;;; fails if the user is running across X11 to a remote server.  Not
;;; sure if this is the right thing, but I am assuming that
;;; *cme-control-panel* exists and can be used to form the screen-spec.

;;; LHQ Fri Dec 24 2004:  I think I have fixed the problem where the toplevel-widget
;;; must be instantiated before the frame-script can be constructed.  Thbe problem was
;;; that screen-spec was not being communicated all the way down into 
;;; (method tk-gl-window-script tk-glx-pbuffer-window)).

(declaim (special *CME-CONTROL-PANEL*))

(defun make-cme-frame (label &key (nx 2) (ny 2)
		       (width *default-cme-frame-width*) (height *default-cme-frame-height*)
		       (borderwidth 1)
		       (pane-class *default-pane-class*)
		       (panel-class 'tiled-window-panel)
		       callback-function
		       frame-script pane-alist 
		       upper-widgets-script lower-widgets-script
		       (tk-frame-class ".frm")
		       (package :gui)
		       toplevel-widget screen-spec
		       resource-class
		       &allow-other-keys)
  (unless toplevel-widget (setq toplevel-widget (tk::gensym-toplevel-widget-path tk-frame-class)))
  ;; Create the frame now so that we can inquire about its screen
  (tcl-script `((qtoplevel ,toplevel-widget :title ,label 
			   ,@(and resource-class `(:class ,resource-class))
			   ,@(and screen-spec `(-screen ,screen-spec)))))
  (unless frame-script
    (mv-setq (frame-script pane-alist)
	     (tk::frame-panes-tk-script toplevel-widget
					:nx nx :ny ny
					:parent toplevel-widget
					:width width :height height
					:borderwidth borderwidth
					:screen-spec screen-spec
					))
    (setq frame-script `(,@upper-widgets-script
			 ,@frame-script
			 ,@lower-widgets-script
			 (qgrid_1_column ,toplevel-widget))))
    
  (tcl-script frame-script)
  (setq *cme-frame-script* frame-script)
  (let ((panel (tk::make-widget-panel toplevel-widget callback-function
				      :panel-class panel-class
				      :package package
				      :panel-args (list :nx nx :ny ny :width width :height height))))
    (setq *last-cme-frame* panel)
    (loop for entry in pane-alist
	  for (id . tkglwin-name) = entry
	  for window = (make-window tkglwin-name :class pane-class)
	  do (ignore id)
	     (setf (cdr entry) window)
	     (setf (panel window) panel))
    (setf (frame-pane-alist panel) pane-alist)
 
    ;; If WAIT-FOR-WINDOWS-DRAWABLE isn't done, we can end up trying to display images before
    ;; CONFIGURE-CALLBACK has happened and the window dimensions are not known.
    (tk::set-toplevel-widget-position (widget panel) (widget *cme-control-panel*))
    (wait-for-windows-drawable (loop for (id . window) in pane-alist collect window))
    panel))


;;; #+unused
;;;(defun tk::tk-displays ()
;;;  (let* ((num_displays (tk::tk_num_displays))
;;;         (tk-displays (make-array0 num_displays :element-type '(unsigned-byte 32))))
;;;    (tk::tk_displays tk-displays)
;;;    (loop for i from 0 below num_displays
;;;          collect (aref tk-displays i))))


;;;
;;; Tcl/Tk-specific:
;;;
(defmethod set-window-border-color ((window tk-window) color)
  ;; widget-parent is the tk frame surrounding the tkglwin widget who supplies
  ;; a border (and perhaps other decorations).
  (when (window-exists (widget window))
    (tcl-cmd `(,(widget-parent (widget window)) configure "-highlightbackground" ,color))
    (tcl-cmd `(,(widget-parent (widget window)) configure "-highlightcolor" ,color))
    ))

#|
(widget (selected-pane))
(widget-parent (widget (selected-pane)))
(set-window-border-color (selected-pane) "green")
(unhighlight-window-border (selected-pane))
(highlight-window-border (selected-pane))
|#

;;; **********************  CALLBACKS AND MOUSE-EVENTS  **********************

(defparameter *inhibit-no-mouse-button-object-select* nil
  "Set to T to disable mouse sensitivity when no mouse buttons are pressed.")

(defparameter *enable-no-mouse-button-object-drag* t
  "Set to T to enable drag operations when no mouse buttons are pressed.")
#|
(setq *inhibit-no-mouse-button-object-select* t)
(setq *enable-no-mouse-button-object-drag* nil)
|#

;;; This is temporarily bound in order to dispose of pending mouse-events
(defvar *ignore-tk-motion-callbacks* nil)


(defmethod tk-callback :around ((panel window-panel) widget item-name event args)
  (if (typep (widget-clidget widget) 'tk-window)
      (let ((largs (if (stringp args) (tcl-list-to-lisp args) args)))
	(case event
	  (tk::motion
	   (unless *ignore-tk-motion-callbacks*
	     (destructuring-bind (button state x y . rest) largs
	       (declare (ignore button rest))
	       ;;(format t "tk-motion-callback ~a ~a~%" widget largs)
	       (if (or *enable-no-mouse-button-object-drag*
		       (logtest state tk::*motion-button-masks*))
		   (glwin-drag-callback panel widget (cv (dfloat x) (dfloat y) 0.0))
		   (unless *inhibit-no-mouse-button-object-select*
		     (glwin-free-motion-callback panel widget (cv (dfloat x) (dfloat y) 0.0)))))))
	  
	  (tk::buttonpress
	   (destructuring-bind (button state x y . rest) largs
	     (declare (ignore rest))
	     ;;(format t "tk-callback tk::buttonpress ~a ~a ~16r ~a~%" widget button state (mouse-event-name button state))
	     ;; If single-button mode, recompute the button number:
	     (setq button (single-button-mouse-hack button))
	     (tk::update-modifier-mask state)
	     (glwin-mouse-buttonpress-callback panel widget
					       (mouse-event-name button state)
					       (cv (dfloat x) (dfloat y) 0.0))))
	  (tk::buttonrelease
	   (destructuring-bind (button state x y . rest) largs
	     (declare (ignore x y rest))
	     (tk::update-modifier-mask state)
	     ;;(format t "tk-buttonrelease-callback ~a ~16r~%" button state)
	     (glwin-mouse-buttonrelease-callback panel widget (mouse-event-name button state))))

	  ;; tk::keypress and tk::keyrelease only occur if the bindings are done in tcl
	  ;; funneling thru qcme_callback.

	  #+unused
	  (tk::keypress
	   (destructuring-bind (keysym keyname state) largs
	     (declare (ignore keysym))
	     ;;(format t "tk-keypress-callback ~a ~a~%" event largs)
	     (glwin-keypress-callback widget keyname keysym state)))

	  #+unused
	  (tk::keyrelease
	   (destructuring-bind (keysym keyname state) largs
	     (declare (ignore keyname))
	     ;;(format t "tk-keyrelease-callback ~a ~a~%" event largs)
	     (glwin-keyrelease-callback widget keyname keysym state)))

	  #+unimplemented
	  (tk::UPDATE_BUCKYS
	   (update-buckys widget args))
	  
	  (otherwise (call-next-method panel widget item-name event args))))

      (call-next-method panel widget item-name event args)))

#+experimental
(defmethod tk-callback :around ((panel window-panel) widget item-name event args)
  (if (typep (widget-clidget widget) 'tk-window)
      (let ((largs (if (stringp args) (tcl-list-to-lisp args) args)))
	(case event
	  (tk::motion
	   (unless *ignore-tk-motion-callbacks*
	     (destructuring-bind (button state x y . rest) largs
	       (declare (ignore button rest))
	       ;;(format t "tk-motion-callback ~a ~a~%" widget largs)
	       (if (or *enable-no-mouse-button-object-drag*
		       (logtest state tk::*motion-button-masks*))
		   (glwin-drag-callback panel widget (cv (dfloat x) (dfloat y) 0.0))
		   (unless *inhibit-no-mouse-button-object-select*
		     (glwin-free-motion-callback panel widget (cv (dfloat x) (dfloat y) 0.0)))))))
	  
	  (tk::buttonpress
	   (destructuring-bind (button state x y . rest) largs
	     (declare (ignore rest))
	     ;;(format t "tk-callback tk::buttonpress ~a ~a ~16r ~a~%" widget button state (mouse-event-name button state))
	     ;; If single-button mode, recompute the button number:
	     (setq button (single-button-mouse-hack button))
	     (tk::update-modifier-mask state)
	     (glwin-mouse-buttonpress-callback panel widget
					       (mouse-event-name button state)
					       (cv (dfloat x) (dfloat y) 0.0))))
	  (tk::buttonrelease
	   (destructuring-bind (button state x y . rest) largs
	     (declare (ignore x y rest))
	     (tk::update-modifier-mask state)
	     ;;(format t "tk-buttonrelease-callback ~a ~16r~%" button state)
	     (glwin-mouse-buttonrelease-callback panel widget (mouse-event-name button state))))

	  ;; tk::keypress and tk::keyrelease only occur if the bindings are done in tcl
	  ;; funneling thru qcme_callback.

	  (tk::keypress
	   (destructuring-bind (keysym keyname state) largs
	     (declare (ignore keysym))
	     (format t "tk-callback: tk-keypress-callback ~a ~a~%" event largs)
	     (glwin-keypress-callback widget keyname state)))

	  (tk::keyrelease
	   (destructuring-bind (keysym keyname state) largs
	     (declare (ignore keyname))
	     (format t "tk-callback: tk-keyrelease-callback ~a ~a~%" event largs)
	     (glwin-keyrelease-callback widget keyname state)))

	  #+unimplemented
	  (tk::UPDATE_BUCKYS
	   (update-buckys widget args))
	  
	  (otherwise (call-next-method panel widget item-name event args))))

      (call-next-method panel widget item-name event args)))



#|
(widget (selected-window *interactor*))
(tk::tcl-cmd `(focus ,(widget (selected-window *interactor*))))

(tk::tcl-cmd '(focus))

	       
|#
