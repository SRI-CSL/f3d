(in-package :gui)

;;; This file (and class) should be renamed to GlWindow.lisp
;;; Contains direct dependencies on TK and OpenGL.

;;; FIXME:  Move the keypress bindings to another file.

(defparameter *debug-print-enable* nil)


;;; These are independent of the native window system, but will allow
;;; us to make sure that glMakeCurrent and glUnMakeCurrent are called
;;; in the correct order.  We need this now because the current
;;; display code is not threadsafe.

(defvar *current-window-stack* nil)
(defvar *debug-makecurrent* nil)

(defmethod glMakeCurrent :before ((window t))
  (when *debug-makecurrent*
    (push window *current-window-stack*)))

(defmethod glUnMakeCurrent :after ((window t))
  (when *debug-makecurrent*
    (if (null *current-window-stack*)
	(break "Attempted to pop the current window stack, which is NIL.  Current = ~a" window)
	(let ((top (pop *current-window-stack*)))
	  (unless (eq top window)
	    (break "The top of the current window stack (~a) is not EQ to the glUnMakeCurrent window argument (~a)!"
                   top window))))))



#|
(setq *debug-print-enable* t)
(setq *gl-is-direct-p* nil)
|#


;;; Trick for single-button mouse on Macs.  The l, m, r keys will
;;; switch the default button.  If a three-button mouse is already
;;; plugged in, that button will be used.  Should this be better
;;; encapsulated?

;;#+old
(progn 

(defvar *single-button-mouse-mode* t)
(defvar *single-button-mouse-button* 1)

(defun single-button-mouse-mode (&optional (on-p t))
  (setq *single-button-mouse-mode* on-p))

(defun single-button-mouse-hack (button)
  (if (and *single-button-mouse-mode* (= button 1))
      *single-button-mouse-button*
    button))

;;; We can still be in single-button-mouse-mode, but use a 3-button
;;; mouse.  The only thing the mode allows that is different is the
;;; use of the l,m, and r keys to switch the number of the "true"
;;; button in response to a left click.

(defun single-button-mouse-hack (button)
  (if (and *single-button-mouse-mode* (= button 1))
      *single-button-mouse-button*
    button))

(defun (setf single-button-mouse-button) (state)
  (setq *single-button-mouse-button* state))

(defun (setf single-button-mouse-mode) (state &optional ignore)
  (declare (ignore state))
  (setq *single-button-mouse-mode* state))

) ; end progn


#+new
(progn

(defvar *single-button-mouse-mode* t)

(defun single-button-mouse-mode ()
  *single-button-mouse-mode*)

;;; State can be NIL, 1, 2, or 3.
(defun (setf single-button-mouse-button) (state)
  (setq *single-button-mouse-mode* state))

(defun single-button-mouse-hack (button)
  (or *single-button-mouse-button*
      button))

) ;end progn



(defun debug-print (&rest args)
  (when *debug-print-enable*
    (apply #'format t args)))

(defvar *gl-is-direct-p*)

(defun gl-is-direct-p ()
  (if (boundp '*gl-is-direct-p*)
      *gl-is-direct-p*
      (setq *gl-is-direct-p*
	    (eql 0 (position #\: (tk::widget-screen  "."))))))


#|
WINDOWS built using glxwin aglwin wglwin and their callback handlers.
    

FRAMES: WINDOW-PANEL TILED-WINDOW-PANEL
|#

(declaim (special *interactor* *LAST-CME-FRAME*
	    *ENABLE-DYNAMIC-OBJECT-DEPTH-TEST*
	    ))

;;; ************************  VISUALS  ***************************************

;;; This is info about the screen and the window-system dependent OpenGL "visual"
;;; information.  Specific OpenGL window system interfaces need to customize this.

(defclass basic-visual (property-list-mixin)
    ((dimensions :accessor dimensions)
     (direct-p :accessor direct-p) ; direct rendering
     )
  )




;;; ************************  WINDOWS  ***************************************

;;;  Split off basic-window from window.  This allows gtk-based
;;;  windows to be built on basic-window and share the same basic
;;;  operations (cycle-stack and such).  I kept the widget slot in
;;;  basic-window on the assumption that any underlying GUI system has
;;;  this notion, but this choice means that there is a strict common
;;;  API for all widgets regardless of their origin (e.g., they all
;;;  must support widget-width, and several other methods used by
;;;  basic-window).  

(defclass basic-window
	 ;;(property-list-mixin)
	 ;; Why does this include CVV-ITEM?
	 ;; Answer: because cvv-item includes slots for name, panel ...
	 ;; If these slots become necessary, we should include them.
	 ;; WINDOW and FRAME classes coordinate with CVV-ITEM to
	 ;; make an interconnected network.
	 ;; The modularity seems somewhat broken here:  I would think CVV could load after
	 ;; BASIC-GUI.
	 (cvv-item property-list-mixin)
    ((widget :initarg :widget :accessor widget)
     (dimensions :initform nil :accessor dimensions) 
     (root-position :initform nil :accessor root-position)
     (screen-dimensions :initform nil  :accessor screen-dimensions)
     (view-stack :initform nil :reader view-stack)
     (damaged-p :initform nil :accessor damaged-p)
     ;; back-buffer-state is really specific to bbuffer-window classes.
     ;; Until the event callbacks become methods on the window-class,
     ;; back-buffer-state needs to be here.
     (back-buffer-state :initform nil :accessor back-buffer-state)
     ))


(defmethod window-width ((window basic-window))
  (dimensions window)
  ;;(window-width (widget window))
  )

(defmethod window-height ((window basic-window))
  (nth-value 1 (dimensions window))
  ;;(window-height (widget window))
  )

(defmethod center-position ((window basic-window))
  (cv (* .5 (window-width window))
      (* .5 (window-height window))
      0.0))

(defmethod window-x ((window basic-window))
  (window-x (widget window)))

(defmethod window-y ((window basic-window))
  (window-y (widget window)))


(defmethod pane-frame ((window basic-window))
  (panel window))

(defmethod set-window-dimensions ((window basic-window))
  (let ((widget (widget window)))
    (when (and widget (window-exists widget))
      (setf (screen-dimensions window)
	    (list (screen-width widget)
		  (screen-height widget)))
      (values-list (setf (dimensions window)
			 (list (window-width widget)
			       (window-height widget)))))))


;;; Be sure to call this before calling WARP-POINTER-TO-CENTER
(defmethod update-window-parameters ((window basic-window))
  (let ((widget (widget window)))
    (when (and widget (window-exists widget))
      (destructuring-bind (width height rootx rooty screen-width screen-height)
	  (tk::widget-window-parameters widget)
	(setf (dimensions window) (list width height)
	      (root-position window) (list rootx rooty)
	      (screen-dimensions window) (list screen-width screen-height))
	))))

;;; It would nice to change this to:
;;; (defun push-image (image &optional (window (selected-window))) ...)
(defmethod push-image (image (window basic-window))
  (ensure-image-to-2d-transform image)
  (let ((view (make-view window :image image :2d-world (2d-world image))))
    (push view (view-stack window))
    view))

(defmethod dimensions ((window basic-window))
  (with-slots (dimensions) window
    (values-list dimensions)))

(defmethod print-object ((window basic-window) stream)
  (print-unreadable-object (window stream :type t :identity t)
    (princ (widget window) stream)))


(defmethod view-changed ((window basic-window))
  (with-slots (damaged-p back-buffer-state) window
    (setf damaged-p t
	  back-buffer-state nil)))

(defmethod (setf view-stack) (new-view-stack (window basic-window))
  (with-slots (view-stack) window
    (view-changed window)
    (setf  view-stack new-view-stack)))

(defun top-view (&optional (thing t))
  (if (eq thing t)
      (%top-view (selected-window)); (current-view *interactor*)
      (%top-view thing)))
	
(defun top-image (&optional (thing t))
  (view-image (top-view thing)))

(defmethod %top-view ((window basic-window))
  (first (view-stack window)))

(defmethod %top-view ((window string))
  (%top-view (widget-window window)))

;; Is this used?
(defmethod %top-view ((window symbol))
  (when window
    (let* ((tk-pathname (to-string window)))
      (%top-view (widget-window tk-pathname)))))



;; Should this move to a tk specific file?
(defclass tk-window (basic-window)
  ())

(defmethod glMakeCurrent ((window tk-window)) nil)

(defmethod glUnMakeCurrent ((window tk-window)) nil)


(defclass tkgl-window (tk-window)
    ())

;;;
;;; Does this really depend on Tk???
(defclass gl-buffered-window-mixin (tkgl-window) ())

(defclass gl-bbuffer-window-mixin (gl-buffered-window-mixin) ())

(defmethod backing-store-valid-p ((window  gl-bbuffer-window-mixin))
  (eql (back-buffer-state window) 'VISIBILITYUNOBSCURED))

;;; This mixin defines slots in violation to proposed defstruct extension.
(defclass gl-pbuffer-window-mixin (gl-buffered-window-mixin)
  ((pbuffer :initform nil :accessor pbuffer)
   (pbuffer-context :initform nil :accessor pbuffer-context)
   ))

(defmethod backing-store-valid-p ((window  gl-bbuffer-window-mixin))
  (eql (back-buffer-state window) 'VISIBILITYUNOBSCURED))


(defmethod set-mouse-cursorpos ((window tk-window) x y)
  (tk::set-mouse-cursorpos (widget window) x y))


(defmethod tk::tcl-string ((window tk-window) &optional tcl-code)
  (ignore tcl-code)
  (widget window))


(defmethod widget-window ((widget t) &key (create nil))
  (or (widget-clidget widget)
      (and create (make-window widget))))


;;; This is set the respective specialized pane-class files.
(defvar *default-pane-class*)

(defun-cached pane-class-instance (class)
  (make-instance class))


(defun tk::tkglwin-script (&rest args)
  (apply #'tk-gl-window-script (pane-class-instance *default-pane-class*) args))

(defun make-window (widget &rest args &key (class *default-pane-class*) &allow-other-keys)
  (setf (widget-clidget widget)
	(apply 'make-instance class :widget widget args)))

;;;
;;; GTK has no analogue for this.  Maybe this is a point of departure:
;;; the Tcl/Tk GUI is good for keeping compatibility with the old CME.
;;;

(defparameter *active-windows-hash-table* (make-hash-table :test 'equal))
(defparameter *all-windows* nil)

(defmethod map-over-active-windows-internal (fn)
  (loop for window being the hash-keys of *active-windows-hash-table*
	do (funcall fn window)))

(defmethod map-over-all-windows-internal (fn)
  (loop for window in *all-windows*
	do (funcall fn window)))


;;; Should these specialize?

;;; VisibilityFullyObscured => NIL, anything else => state
(defmethod visible-p (window)
  (gethash window *active-windows-hash-table*))

(defmethod (setf visible-p) (value window)
  (let ((visible-p (not (equal value 'VisibilityFullyObscured))))
    (if visible-p
	(setf (gethash window *active-windows-hash-table*) value)
	;; should this also unmap the window?
	(remhash window *active-windows-hash-table*))
    value))

(defun set-mouse-cursor-on-all-windows (cursor-name)
  (map-over-active-windows (window) 
    (ignore-errors  ; FIXME - sometimes a destroyed window remains in the active window list.
      (set-cursor window cursor-name))))


;;;  ************************  GL WINDOW CALLBACKS  ************************

;;; tk::tk-destroy-callback should be renamed to destroy-callback and put into a more generic package.
;;; tk/tk-callbacks.lisp installs this
(defmethod tk::tk-destroy-callback ((window basic-window) widget &rest args)
  (ignore widget args)
  ;;(format t "tk::tk-destroy-callback ~a~%" window)
  (setq *all-windows* (remove window *all-windows*))
  (remhash window *active-windows-hash-table*)
  ;; something appears to retain pointers to window - be sure to recover backing-store
  )


(defmethod glwin-visibility-callback (widget state &optional button)
  (declare (ignore button))
  (let* ((window (widget-window widget)) )
    (setf (visible-p window) state)
    ;;(format t "glwin-visibility-callback ~a ~a~%" widget state)
    (debug-print (if (eql state 'VISIBILITYUNOBSCURED) "V" "v"))
    (when (and (drawable-p window) (dimensions window)
	       (eql state 'VisibilityPartiallyObscured))
      (setf (damaged-p window) t))))

;;; GLWIN-EXPOSE-CALLBACK gets called when, among other things, another window
;;; is dragged over an image-pane.  Since there (in general) will be many
;;; drags, we would like to avoid refresh until the drag is complete. 
(defmethod glwin-expose-callback (widget)
  (let ((window (widget-window widget)))
    ;;(format t "glwin-expose-callback ~a ~a~%" window (visible-p window))
    (debug-print (if (eql (visible-p window) 'VISIBILITYUNOBSCURED) "E" "e"))
    (setf (damaged-p window) t)
    
    (unless (eql (visible-p window) 'VISIBILITYUNOBSCURED)
      ;; The back-buffer is clobbered
      (setf (back-buffer-state window) (visible-p window))
      )))

(defmethod glwin-unmap-callback (widget)
  (let ((window (widget-window widget)))
    (setf (visible-p window) 'VisibilityFullyObscured)
    (setf (back-buffer-state window) 'VisibilityFullyObscured)
    ;;(format t "unmap-callback ~a~%" window)
    ))


;;; glwin-enter-callback and glwin-leave-callback might not be needed if tk_focusFollowsMouse is used.

(defmethod glwin-enter-callback (widget)
  (tk::tcl-cmd `(focus -force ,widget))
  )

(defmethod glwin-leave-callback (widget)
  ;;(tk::tcl-cmd `(focus ,widget))
  )

(defvar *configure-callback-recursion* nil)

(defmethod glwin-configure-callback (widget width0 height0)
  (declare (ignorable width0 height0))
  ;; Apparently TK can cause a configure callback after window is gone. Check.
  ;;(format t "configure-callback ~a ~a~%" widget (widget-window widget))
  (when (window-exists widget)
    (let ((window (widget-window widget)))
      ;;(setq *foo* (list window )) (break)
      (unless (drawable-p window)
	(pushnew window *all-windows*)
	(init-state window widget))
      ;;(format t "glwin-configure-callback ~a ~a~%" widget (visible-p window))
      (when (eq (visible-p window) 'VisibilityPartiallyObscured)
	;; Might have made part of the window visible that is not in the
	;; back buffer.
	(setf (visible-p window) nil))
	
      (let ((width (window-width widget))
	    (height (window-height widget)))
	(mv-bind (previous-width previous-height) (dimensions window)
					;(setq *foo* (list window width height previous-width previous-height width0 height0 (visible-p window)))
					;(format t "glwin-configure-callback ~a~%" *foo*)
	  ;;(break)
	  (if (and (eql width previous-width) (eql height previous-height))
	      (redisplay window
			 :from-backing-store (not *enable-dynamic-object-depth-test*))

	      (let* ((borderwidth (or (get-prop window :borderwidth) 0))
		     (2bw (+ borderwidth borderwidth)))
		(setf (dimensions window) (list (- width 2bw) (-  height 2bw)))
		(gl::with-gl-window (window)
		  ;; (glMakeCurrent window)
		  ;; Set the viewport parameters
		  (glViewport borderwidth borderwidth (- width 2bw) (-  height 2bw)))

		  ;; Update window's idea of dimensions.
		(redisplay window :from-backing-store nil))))
	;;(break)
	))))


(defmethod glwin-configure-callback (widget width height)
  ;; Apparently TK can cause a configure callback after window is gone. Check.
  ;;(format t "configure-callback ~a ~a~%" widget (widget-window widget))
  (when (window-exists widget)
    (let ((window (widget-window widget)))
      ;;(setq *foo* (list window )) (break)
      (tk::do-events) ; I don't like this, but it fixes some problems.
      (unless (drawable-p window)
	(pushnew window *all-windows*)
	(init-state window widget))
      ;;(format t "glwin-configure-callback ~a ~a~%" widget (visible-p window))
      (when t ;(eq (visible-p window) 'VisibilityPartiallyObscured)
	;; Might have made part of the window visible that is not in the
	;; back buffer.
	(setf (visible-p window) nil))
	
      (mv-bind (previous-width previous-height) (dimensions window)
	;;(format t "glwin-configure-callback ~a~%" (setq *foo* (list window width height previous-width previous-height  (visible-p window))))
	;;(break)
	(if (and (eql width previous-width) (eql height previous-height))
	    (redisplay window
		       :from-backing-store (not *enable-dynamic-object-depth-test*))

	    (gl::with-gl-window (window)
	      (let* ((borderwidth (or (get-prop window :borderwidth) 0))
		     (2bw (+ borderwidth borderwidth)))
		(setf (dimensions window) (list (- width 2bw) (-  height 2bw)))
		;; (glMakeCurrent window)
		;; Set the viewport parameters:  this does not work correctly in Cocoa:
		#+cocoa (reshape-nsview window)
		;; #+cocoa (format t "~%Using Cocoa OpenGL API.")
		(glViewport borderwidth borderwidth (- width 2bw) (-  height 2bw))

		;; Update window's idea of dimensions.
		(redisplay window :from-backing-store nil)))))
      ;;(break)
      )))


(declaim (special *current-user-interface-context*))
;;;
;;; Is it worth dispatching on keyname within this method, or to use CLOS?
;;; LHQ: Using CLOS and ui-context fits the model used for other event bindings.
;;;

#+old ; get rid of use of keysym -- X11 specific
(progn
(defmethod glwin-keypress-callback (widget keyname &optional keysym state)
  (declare (ignorable keysym state))
  (format t "~%KeyPress: ~a ~a ~16r" keyname keysym state)
  (let ((kw (intern (string-upcase keyname) :keyword)))
    (keypress-callback kw (or (get-prop (current-view) :user-interface-context)
			      *current-user-interface-context*))
    (set-doc-line (add-to-modifier-mask keyname) (get-mouse-event-table (widget-window widget)))
    ))


(defmethod glwin-keyrelease-callback (widget keyname &optional keysym state)
  (declare (ignorable keysym state))
  ;(format t "~%KeyRelease: ~a ~a ~16r" keyname keysym state)
  (set-doc-line (remove-from-modifier-mask keyname) (get-mouse-event-table (widget-window widget)))
  )

) ; end progn

#+old ; Mon Jul  9 2007
(progn
(defmethod glwin-keypress-callback (widget keyname &optional ascii state)
  (declare (ignorable state))
  (format t "~%KeyPress: ~a ~s ~a ~16r" widget keyname ascii state)
  (let ((kw (intern (string-upcase keyname) :keyword)))
    (keypress-callback kw (or (get-prop (current-view) :user-interface-context)
			      *current-user-interface-context*))
    (set-doc-line (add-to-modifier-mask keyname) (get-mouse-event-table (widget-window widget)))
    ))


(defmethod glwin-keyrelease-callback (widget keyname &optional ascii state)
  (declare (ignorable keysym state))
  ;(format t "~%KeyRelease: ~a ~a ~16r" keyname ascii state)
  (set-doc-line (remove-from-modifier-mask keyname) (get-mouse-event-table (widget-window widget)))
  )

) ; end progn

(defmethod glwin-keypress-callback (widget keyname &optional state)
  (declare (ignorable state))
  ;;(format t "~%KeyPress: ~a ~s ~16r" widget keyname state)
  (let ((kw (intern (string-upcase keyname) :keyword)))
    (keypress-callback kw (or (get-prop (current-view) :user-interface-context)
			      *current-user-interface-context*))
    (set-doc-line (add-to-modifier-mask keyname) (get-mouse-event-table (widget-window widget)))
    ))


(defmethod glwin-keyrelease-callback (widget keyname &optional state)
  (declare (ignorable state))
  ;(format t "~%KeyRelease: ~a ~16r" keyname state)
  (set-doc-line (remove-from-modifier-mask keyname) (get-mouse-event-table (widget-window widget)))
  )

#|
(setq win (widget-window ".frm.f2.gl2"))
(widget win)
(view-stack win) 
(dimensions win)
(glMakeCurrent win)
(setq *tk-verbose* t)
(window-exists ".top3.frm.1-1.1-1_gl")
(widget-window ".top3.frm.1-1.1-1_gl")
(window-width (widget-window ".top3.frm.1-1.1-1_gl"))
(dimensions (widget-window ".top3.frm.1-1.1-1_gl"))

(setq tk::*tk-verbose* t)
(trace LISPTK::INVOKE-TCL-COMMAND)

(untrace)
(setq tk::*tk-verbose* nil)

(let ((w (widget (selected-window))))
  (tk::tcl-eval "event" "generate" w "<KeyPress>" "-keysym" "minus" "-keycode" "45" ))

(tk::tcl-cmd `(event generate ,(widget (selected-window)) "<KeyPress>" :keysym "KP_Add"))

(tk::tcl-cmd `(event generate ,(widget (selected-window)) "<minus>"))

(tk::tcl-cmd `(event generate ,(widget (selected-window)) "<KeyPress>" :keysym "KP_Add"
		     :sendevent 1))

(tk::tcl-cmd `(bindtags ,(widget (selected-window))))
".frm4.frm.0-1.0-1_gl Frame .frm4 all image_pane"

(tk::tcl-cmd `(bind ,(widget (selected-window))))
""

(tk::tcl-cmd `(bind "Frame"))
""

(tk::tcl-cmd `(bind all))
"<<PrevWindow>> <Key-Tab> <Key-F10> <Alt-Key>"

(tk::tcl-cmd `(bind "image_pane"))
"<Enter> <KeyRelease> <Expose> <Unmap> <Visibility> <Configure> <Key> <B3-Motion> <B2-Motion> <B1-Motion> <ButtonRelease> <Button>"

(tk::tcl-cmd '(list ("tk_focusFollowsMouse"))) 
"{}"

(tk::tcl-eval "tk_focusFollowsMouse" )

(tk::tcl-eval "::tk::FocusOK" (widget (selected-window)))
(tk::tcl-eval "::tk::FocusOK" (widget (selected-window)))

(tk::tcl-eval "focus" "-lastfor" ".frm9")
(tk::tcl-cmd `(focus))

(let ((w (widget (selected-window))))
  (tk::tcl-cmd `(focus "-force" ,w))
  (tk::tcl-cmd `(event generate ,w "<KeyPress>" :keysym "KP_Add" )))

(let ((w (widget (selected-window))))
  (tk::tcl-cmd `(focus "-force" ,w))
  (tk::tcl-cmd `(event generate ,w "<KeyPress>" :keysym "minus" )))

;;; This works
(tk::tcl-cmd `(event generate ".frm4.frm.0-0.0-0_gl" "<ButtonPress>" :button 1))




|#

