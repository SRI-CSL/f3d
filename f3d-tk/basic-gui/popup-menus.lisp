(in-package :gui)

;;; Should these move to commands.lisp?

(defvar *popup-image-drag* nil)

;;; For use with *ENABLE-NO-MOUSE-BUTTON-OBJECT-DRAG*
(defun start-popup-image-drag (fn)
  (unless *ENABLE-NO-MOUSE-BUTTON-OBJECT-DRAG*
    (let ((*popup-image-drag* t))
      (with-documentation ("Press Mouse Button to Initiate Drag")
	;; perhaps the cursor should be changed to indicate we are waiting for a button press.
	(tk::handle-events-until #'(lambda () (null *popup-image-drag*))))))
  (setf (popup-drag *interactor*) nil)
  (start-image-drag *interactor* fn))
	
(defun start-popup-object-drag (fn)
  (unless *ENABLE-NO-MOUSE-BUTTON-OBJECT-DRAG*
    (let ((*popup-image-drag* t))
      (with-documentation ("Press Mouse Button to Initiate Drag")
	;; perhaps the cursor should be changed to indicate we are waiting for a button press.
	(tk::handle-events-until #'(lambda () (null *popup-image-drag*))))))
  (setf (popup-drag *interactor*) nil)
  ;;(format t "start-popup-object-drag ~a ~a~%" fn (popup-drag *interactor*))
  (start-object-drag *interactor* fn))

;;; This expands :object-drag :image-drag key vals and ignores :accel key vals.
(defun preprocess-basic-elt-popup-menu-item-list (item-list)
  (loop for item in item-list
	when (eq item :separator)
	  collect '("" :separator)
	else when (and (consp item) (oddp (length item)))
	       collect (cons (car item)
			     (loop for (key val) on (cdr item) by #'cddr
				   when (eq key :object-drag)
				     collect :eval
				     and collect `(start-popup-object-drag ,val)
				   when (eq key :image-drag)
				     collect :eval
				     and collect `(start-popup-image-drag ,val)
				   unless (eq key :accel)
				     collect key and collect val))
	else collect item))
			     
	    
;;; Called from COM-POPUP-MENU in commands.lisp
(defmethod basic-elt-popup-ui-popup-menu ((interactor interactor))
  (with-class-slot-values interactor (selected-objects) interactor
    (let ((current-view (current-view interactor)))
      (menu-choose
       (preprocess-basic-elt-popup-menu-item-list
	(full-object-popup-menu-item-list (get-user-interface-context interactor)
					  (or selected-objects current-view)))
       ))))

#|
(object-popup-menu-item-list *default-popup-ui* (current-view *interactor*))
(tk::convert-menu-item-list (object-popup-menu-item-list *default-popup-ui* (top-view)))
(trace menu-choose)

(with-class-slot-values interactor (selected-objects current-view) *interactor*
  (full-object-popup-menu-item-list (get-user-interface-context *interactor*)
				    (or selected-objects current-view)))
(tk::convert-menu-item-list *)
(get-user-interface-context *interactor*)
(selected-objects)
(basic-elt-popup-ui-popup-menu *interactor*)
|#
