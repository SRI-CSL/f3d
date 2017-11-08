(in-package :gui)

;;; FIXME: move INIT-MOUSE-EVENT-TABLE to a file for specific bindings. 

;;; ******************************  WINDOW-PANEL  ******************************

#| 
WINDOW-PANEL is a WIDGET-PANEL subclass that controls a set of TKGLWIN widgets for
OpenGL rendering.  The widget immediately associated with this panel can be a
collection widget anywhere in the widget hierarchy above all of the PANES it
controls.  
|#

(defclass window-panel (widget-panel)
    ((pane-alist :initform nil :initarg :pane-alist :accessor frame-pane-alist)
     ))

(defmethod map-over-frame-panes-internal ((panel window-panel) fn)
  (loop for (pane-name . pane) in (frame-pane-alist panel)
	do (funcall fn pane)))

(defmacro map-over-frame-panes ((frame pane) &body body)
  `(map-over-frame-panes-internal ,frame
				  #'(lambda (,pane) ,@body)))

(defmacro map-over-frame-views ((frame view) &body body)
  `(map-over-frame-panes (,frame pane)
     (let ((,view (top-view pane)))
       (when ,view ,@body))))
  
(defmethod inferiors ((frame window-panel))
  (loop for (id . window) in (frame-pane-alist frame)
	do (ignore id)
	collect window))

(defmethod tk::tk-destroy-callback :after ((panel window-panel) widget &rest args)
  (declare (ignore widget args))
  (when tk::*tk-destroy-callback-verbose*
    (format t "tk-destroy-callback :after ~a~%" panel))
  (loop for (pane-name . pane) in (frame-pane-alist panel)
	do (tk::tk-destroy-callback pane (tk::widget pane))) ; this looks weird -- why NIL?
  ;; For some reason there are multiple destroy callbacks from TK
  (setf (frame-pane-alist panel) nil))

(defclass tiled-window-panel (window-panel)
    ((nx :initform 1 :initarg :nx :accessor frame-nx)
     (ny :initform 1 :initarg :ny :accessor frame-ny)))

(defmethod initialize-instance :after ((frame tiled-window-panel)
				       &rest args &key pane-alist
				       &allow-other-keys)
  (ignore args)
  (loop for (id . pane) in pane-alist
	do (ignore id)
	do (setf (panel pane) frame)))

(declaim (special *CME-FRAME-SCRIPT*))

(defparameter *default-cme-frame-width* 1024)
(defparameter *default-cme-frame-height* 800)

(defun select-frame-size (&optional (type :workstation))
  (if (consp type)
      (setq *default-cme-frame-width* (car type)
	    *default-cme-frame-height* (cadr type))
      (case type
	(:workstation   (setq *default-cme-frame-width* 1024
			      *default-cme-frame-height* 800))
	(:laptop	    (setq *default-cme-frame-width* 880
				  *default-cme-frame-height* 600))
	(t		    (error "Unrecognized system type.")))))

(defun get-image-pane (id &optional frame)
  (unless frame (setq frame (and (selected-window *interactor*)
				 (pane-frame (selected-window *interactor*)))))
  (and frame (cdr (assoc id (frame-pane-alist frame) :test #'equal))))

;;; This is a compatibility definition.  Move to cme-compat.
(defun image-calc-pane (x y &optional frame)
  (get-image-pane (list x y) frame))


#|
(setq frame (make-instance 'tiled-window-panel :nx 2 :ny 2))
(pprint (setq script (create-frame-panes-script frame :parent ".frm")))


(setq frame (make-cme-frame "CME 2x2" :borderwidth 6 :width 400 :height 400 ))

(push-image alv-2-44 (image-calc-pane 0 0 frame))

(setf (get-prop (image-calc-pane 0 0 frame) :borderwidth) 6)
(set-window-objects  (image-calc-pane 0 0 frame))
(image-calc-pane 0 0 frame)
|#

(declaim (special *the-screen*))

(defmethod screen ((window basic-window))
  *the-screen*)

(defmethod highlight-window-border ((window basic-window) tandem-selected)
  (ignore tandem-selected)
  (set-window-border-color window
			   (get-prop (screen window) :selected-window-border-color)))

(defmethod unhighlight-window-border ((window basic-window))
  (set-window-border-color window
			   (get-prop (screen window) :unselected-window-border-color)))

(defmethod get-mouse-event-table ((window basic-window))
  (or (and (selected-objects)
	   (get-mouse-event-table (selected-objects)))
      ;; falls thru if multiple-objects are selected
      (get-prop window :mouse-event-table)
      (let ((ui-context (get-user-interface-context *interactor* window))
	    (view (top-view window)))
	(eval-cache (get-mouse-event-table ui-context (class-of view))
	  (add-mouse-events (make-mouse-event-map )
			    (append '((<left> highlight-drag-select-object :bucky-doc "Select Object")
				      (<right> com-popup-menu :bucky-doc "Menu")
				      )
				    (extract-bucky-menu-list
				     (full-object-popup-menu-item-list ui-context view))))))))

#|
(get-prop (selected-window) :mouse-event-table)
(trace get-mouse-event-table)
(eval-cache-flush-function 'get-mouse-event-table)
(setq * (get-mouse-event-table (selected-window)))
(get-mouse-event-table (selected-view))
(get-mouse-event-table (selected-object))
(ui-bucky-menu-list (get-user-interface-context *interactor*) (caar (selected-objects)))
(typep (get-user-interface-context *interactor*) 'basic-object-accelerators)
(extract-bucky-menu-list (basic-object-popup-menu-item-list (get-user-interface-context *interactor*)
							    (caar (selected-objects))))
(extract-bucky-menu-list (object-popup-menu-item-list (get-user-interface-context *interactor*)
							    (top-view *last-window*) ))
(object-popup-menu-item-list (get-user-interface-context *interactor*)
			     (top-view *last-window*) )
|#

(defmethod get-mouse-event-table ((objects list))
  (and (= (length objects) 1)
       (get-mouse-event-table (caar objects))))

(defmethod get-mouse-event-table ((object obj::basic-gl-object))
  (let ((ui-context (get-user-interface-context *interactor*)))
    ;;(format t "get-mouse-event-table obj::gl-object ~a~%" object)
    (eval-cache (get-mouse-event-table ui-context (class-of object))
	(add-mouse-events (make-mouse-event-map )
	 (append '((<left> highlight-drag-select-object :bucky-doc "Select Object")
					;(<middle> com-do-nothing :bucky-doc "")
		   (<middle> (start-popup-image-drag 'drag-uv) :image-drag 'drag-uv
		    :bucky-doc "Drag Image")
		   (tk::<button4> (scroll-zoom-out *interactor*) :wheel-op t)
		   (tk::<button5> (scroll-zoom-in *interactor*) :wheel-op t)
		   (<right> com-popup-menu :bucky-doc "Menu")
		   )
		 (extract-bucky-menu-list
		  (full-object-popup-menu-item-list ui-context object)))))))

(defun extract-bucky-menu-list (menu-item-list)
  (flet ((extract-bucky-spec (item-label plist)
	   (when (evenp (length plist))
	     (let ((accel (getf plist :accel))
		   (action (getf plist :eval))
		   (object-drag (getf plist :object-drag))
		   (image-drag (getf plist :image-drag))
		   (wheel-op (getf plist :wheel-op))
		   ;;(drag-type (getf plist :drag-type))
		   (menu (getf plist :menu)))
	       (cond (menu (extract-bucky-menu-list menu))
		     ((and accel object-drag)
		      `((,accel (start-popup-object-drag ,object-drag) :bucky-doc ,item-label)))
		     ((and accel image-drag)
		      `((,accel (start-popup-image-drag ,image-drag) :bucky-doc ,item-label)))
		     ((and accel action)
		      `((,accel ,action :bucky-doc ,item-label
				,@(when wheel-op `(:wheel-op ,wheel-op))))))))))
    ;(setq *extract-bucky-menu-list* );
    (loop for item in menu-item-list
	  when (and (consp item) (oddp (length item)))
	    append (extract-bucky-spec (car item) (cdr item)))))



;;; Fri Mar 12 2004 LHQ:  Should these becomes slots of the interactor?
(defvar *most-recent-buttonpress* nil)
(defvar *inhibit-mouse-actions* nil)
(declaim (special *popup-image-drag*))

(defmethod glwin-mouse-buttonpress-callback
	   ((panel window-panel) widget mouse-event window-pos)
  (declare (special *most-recent-buttonpress* *inhibit-mouse-actions*))
  (cancel-release-textures-after-delay)
  (setf *most-recent-buttonpress* (list panel widget mouse-event window-pos))
  ;;(format t "glwin-mouse-buttonpress-callback ~a ~a~a ~%"  widget mouse-event *popup-image-drag*)
  (let* ((interactor *interactor*)
	 (window (widget-window widget)))
    (if *popup-image-drag*
	;; With drags initiated from popup menus, a buttonrelease is needed to choose from the popup
	;; menu, and because there is no binding for <Motion> with no buttons, a buttonpress is needed
	;; to initiate the drag.  This next inhibits the action normally associated with
	;; that buttonpress, and remembers the position of THIS buttonpress.
	(progn (set-state interactor window window-pos)
	       (setq *popup-image-drag* nil)) ; sync with start-popup-image-drag

	(unless *inhibit-mouse-actions*
	  (mv-bind (function drag-type wheel-op)
	      (get-mouse-event-function (get-mouse-event-table window) mouse-event)
					;(format t "glwin-mouse-buttonpress-callback ~a ~a ~a ~a~%" window function drag-type wheel-op)
	    (when (and function (not wheel-op))	; do not stop drag for unknown events
					;(format t "glwin-mouse-buttonpress-callback stop-tracking ~a~%" mouse-event)
	      (stop-drag interactor window)
	      (set-state interactor window window-pos))
	      
	    (cond ((consp function)	; This is a form to eval
		   (eval function)
		   #+never ;; ELIMINATE THESE SPECIAL BINDINGS -- CME LEGACY 
		   (let ((p panel) (i interactor) (w window) (wp window-pos))
		     ;; Not sure why this double binding is required in LCL.
		     (let ((panel p) (interactor i) (window w) (window-pos wp))
		       (declare (special panel interactor window window-pos))
		       (eval function))))

		  ((eq drag-type 'gl-object)
		   (when (selected-objects)
		     (start-drag interactor function 'gl-object)))
		  (drag-type
		   (start-drag interactor function drag-type))
		  (function
		   (funcall function interactor))
		  (t
		   (format t "tkglwin-mouse-button-callback UNDEFINED callback for ~a~%"
			   mouse-event)
		   )))))))

