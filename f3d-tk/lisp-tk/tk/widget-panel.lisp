(in-package :lisptk)

#|

CVV API directly from TK widget tree, requiring no explicit
cvv-item instances.

call MAKE-WIDGET-PANEL to create the CVV-PANEL instance 

|#


;;; FIXME LHQ Sun Nov  7 2004 - read-from-string -- what package?
(defmethod cvv-item-value ((widget string) &optional ignore)
  (declare (ignore ignore))
  (destructuring-bind (value type)
      (tcl-list-to-toplevel-strings (tcl-eval 'widget_value widget))
    ;;(setq *foo* (list value type))
    ;; (format t "~%Type = ~a" type)
    (cond ((member type '("qText" "qstring" "qEntry" "qLabel" "qTEntry" "qTLabel") :test 'string-equal)
	   value)
	  ((member type '("qListbox") :test 'string-equal)
	   (tcl-list-to-strings value 1))
	  ((member type '("qCheckButton" "qTCheckButton") :test 'string-equal)
	   (= (read-from-string value nil nil) 1))
	  (t (read-from-string value nil nil)))))

;;; widget is a TK widget-pathname
(defmethod (setf cvv-item-value) (value (widget string) &optional ignore)
  (declare (ignore ignore))
  (tcl-eval 'set_widget_value widget value))

(defmethod cvv-widget (panel (item string))
  (ignore panel)
  item)

(defmethod widget ((item string))
  ;; TK widget-pathname
  item)


(defmethod cvv-item (panel (name symbol))
  (unless panel (break))
  (cadr (assoc name (item-alist panel))))

(defmethod cvv-item-value ((panel widget-panel) &optional item-name)
  (cvv-item-value (cvv-item panel item-name)))

(defmethod (setf cvv-item-value) (value (panel widget-panel) &optional item-name)
  (setf (cvv-item-value (cvv-item panel item-name)) value))


(defmethod cvv-widget (panel (item symbol))
  (unless panel (break))
  (let ((item (cvv-item panel item)))
    (when item (widget item))))

(defmethod cvv-widget (panel (item symbol))
  (unless panel (break))
  (let ((item (cvv-item panel item)))
    (when item (widget item))))


#|
TK-DESTROY-CALLBACK is called when WIDGET of PANEL is destroyed.
|#
(defgeneric tk-destroy-callback (panel widget &rest args))


(defmethod initialize-instance :after ((panel widget-panel)
				       &key package callback-function
				       &allow-other-keys)
  (unless package
    (setq package (symbol-package (if (symbolp callback-function)
				      callback-function
				      (lcl::function-name callback-function)))))
  (unless (packagep package)
    (setq package (find-package package)))
  (setf (panel-package panel) package))

(defmethod panel ((panel widget-panel)) panel)
(defmethod name ((panel widget-panel)) (widget panel))

(defmethod widget-symbol (widget-pathname (panel widget-panel))
  (widget-symbol widget-pathname (panel-package panel)))

;(lx::eval-cache-flush-function 'tk::make-cvv-panel)

(defparameter *tk-destroy-callback-verbose* nil)
;(setq *tk-destroy-callback-verbose* t)

;;; We get here either from (:method tk-callback (widget-panel ...))
;;; or via an event-handler called from (:method tk-callback (t ...))
(defmethod tk-destroy-callback (object widget &rest args)
  (let ((clidget (widget-clidget widget)))
    (when *tk-destroy-callback-verbose*
      (format t "tk-destroy-callback ~a ~a ~a~%" object clidget widget))
    (when clidget
      ;; remove-widget-to-clidget causes problems for after methods
      ;; Fixed by (method tk-destroy-callback :around)
      ;;(remove-widget-to-clidget widget)
      (if (eq clidget object)
	  (progn (lx::eval-cache-flush object)
		 (when *tk-destroy-callback-verbose* 
		   (format t "eval-cache-flush ~a~%" object))
		 )
	  (apply 'tk-destroy-callback clidget widget args))
      )))

#+never ; no longer needed (I think)
(defmethod tk-destroy-callback :around (object widget &rest args)
  (declare (ignore args))
  (when *tk-destroy-callback-verbose*
    (format t "tk-destroy-callback :around ~a ~a~%" object widget))
  #+never ; no longer used
  (when (and config::glx-image-panes (stringp widget))
    (set-x11-window-destroyed (hex-string-to-number (tcl-cmd `(winfo id ,widget)))))
  (call-next-method)
  (let ((clidget (widget-clidget widget)))
    (when clidget (remove-widget-to-clidget widget))))


(defmethod tk-destroy-callback :after ((object t) widget &rest args)
  (ignore object  args)
  ;;(format t "tk-destroy-callback ~a~%" widget)
  (remove-widget-to-clidget widget))

(defmethod pop-down-panel ((panel widget-panel))
  (tcl-eval "wm" "withdraw" (widget panel)))

(defmethod pop-up-panel ((panel widget-panel))
  (pop-up-panel (widget panel)))

(defmethod pop-up-panel ((widget string))
  (setq *foo* widget)
  (tcl-eval "wm" "deiconify" widget)
  (raise-widget-toplevel widget)
  )



;;;(defmethod tk-callback ((panel widget-panel) widget item-name event args)
;;;  ;; (print event)
;;;  (let ((fn (callback-function panel)))
;;;    ;;(setq foo (list panel widget item-name event args))
;;;    (cond ((or (eq event 'MENUBUTTON) (eq event 'OPTIONBUTTON))
;;;           (setq item-name
;;;                 (if (stringp args)
;;;                     (car (tcl-list-to-lisp args :upcase t :package (panel-package panel)))
;;;                     (intern (symbol-name (car args)) (panel-package panel)))
;;;                 args nil))
;;;          ;; This screws up string being passed back
;;;          ;; Either we need to pass the original args also, or
;;;          ;; Panels that need the raw args must specialize this method.
;;;          ((stringp args) (setq args (tcl-list-to-lisp args :upcase t)))
;;;          )
;;;
;;;    ;;(break)
;;;    ;;(format t "tk-callback(widget-panel): ~a ~a ~a ~a~%" widget item-name event args)
;;;    (case event
;;;      (destroy (tk-destroy-callback panel widget item-name event args))
;;;      (otherwise
;;;       (case item-name
;;;         (lock-panel (setf (get-prop panel 'object-lock)
;;;                           (widget-value item-name panel)))
;;;         
;;;         (quit-panel (pop-down-panel panel))
;;;         
;;;         (otherwise
;;;          (cond #+never
;;;                ((get-prop panel :object)
;;;                 (update-object-from-panel-callback panel item-name))
;;;                (fn (funcall fn panel widget item-name event args))
;;;                (t (format t "Unhandled tk-callback ~a ~a ~s ~a ~s~%"
;;;                           panel widget item-name event args)
;;;                   ;;(break)
;;;                   ))))))))


;;; FIXME:
;;; TCL-LIST-TO-LISP screws up string being passed back.
;;; Either tk-callback need to pass the original args also, or
;;; panels that need the raw args must specialize this method.
;;; I am not sure that panels depend on processing args using tcl-list-to-lisp.
(defmethod process-callback-args ((panel widget-panel) args)
  (tcl-list-to-lisp args :upcase t))

(defmethod tk-callback ((panel widget-panel) widget item-name event args)
  (let ((fn (callback-function panel))
	(callback-handled t))
    ;;(setq foo (list panel widget item-name event args))
    (cond ((or (eq event 'MENUBUTTON) (eq event 'OPTIONBUTTON))
	   (setq item-name
		 (if (stringp args)
		     (car (tcl-list-to-lisp args :upcase t :package (panel-package panel)))
		     (intern (symbol-name (car args)) (panel-package panel)))
		 args nil))
	  ((stringp args) (setq args (process-callback-args panel args)))
	  )

    ;;(break)
    ;;(format t "tk-callback(widget-panel): ~a ~a ~a ~a~%" widget item-name event args)
    (case event
      (destroy (tk-destroy-callback panel widget item-name event args))
      (otherwise
       (case item-name
	 (lock-panel (setf (get-prop panel :object-lock) (widget-value item-name panel)))
	 
	 (quit-panel (pop-down-panel panel))
	 (otherwise (setq callback-handled nil))
	 )))

    ;; Even if the event was handled above, give the application a chance to respond also
    (cond #+never
	  ((get-prop panel :object)
	   (update-object-from-panel-callback panel item-name))
	  (fn (funcall fn panel widget item-name event args))
	  ((not callback-handled)
	   (format t "Unhandled tk-callback ~a ~a ~s ~a ~s~%"
		   panel widget item-name event args)
	   ;;(break)
	   ))))

;;;(defun item-alist-from-widget-tree (widget)
;;;  (let ((children (tcl-list-to-strings (tcl-eval 'winfo 'children widget ))))
;;;    (loop for child in children
;;;          collect (list (widget-symbol child )
;;;                        child)
;;;          append (item-alist-from-widget-tree child))))


(defun item-alist-from-widget-tree (widget &optional (package *package*))
  (loop for widget in (tcl-list-to-strings (tcl-cmd `(widget_tree ,widget)))
	collect (list (widget-symbol widget package)
		      widget)))
;(item-alist-from-widget-tree (widget (panel (gui::selected-window))))

(defun make-widget-panel (toplevel-widget callback-function &key 
			  (panel-class 'widget-panel)
			  package
			  (get-widget-tree t)
			  panel-args)
  (unless (stringp toplevel-widget) (setq toplevel-widget (widget toplevel-widget)))
  (let* ((panel (apply 'make-instance panel-class
		       :callback-function callback-function
		       :package package
		       :widget toplevel-widget
		       panel-args)))
    (when get-widget-tree
      (setf (item-alist panel)
	    (item-alist-from-widget-tree toplevel-widget (panel-package panel))))
    (setf (widget-clidget toplevel-widget) panel)
    panel))


(defun set-widget-panel-toplevel (panel toplevel-widget &key (get-widget-tree t))
  (unless (stringp toplevel-widget) (setq toplevel-widget (widget toplevel-widget)))
  (setf (tk::widget panel) toplevel-widget)
  (when get-widget-tree
    (setf (item-alist panel)
	  (item-alist-from-widget-tree toplevel-widget (panel-package panel))))
  (setf (widget-clidget toplevel-widget) panel)
  panel)


#|
(tcl-script '((qtoplevel .b)
	      (qframe .b.f)
	      (qbutton .b.f.b -text "Press Me")
	      (qinteger .b.f.i -initial_value 10 -labeltext "Num")
	      (qstring .b.f.s -initial_value "Hose" -labeltext "Name")
	      (qtext .b.f.t -initial_value "This is for hawks to attack"
	       -labeltext "Description" -weight 100)
	      (qgrid_2_column .b.f)
	      (pack .b.f)
	      ))

(defmethod test-callback ((panel widget-panel) widget item-name event args)
  (format t "test-callback ~a ~a ~a ~a ~a~%" panel widget item-name event args))

(setq panel (make-widget-panel ".b" 'test-callback ))
|#
