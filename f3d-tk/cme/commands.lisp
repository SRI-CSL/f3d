(in-package :cme)


(defmacro define-interaction-method (method-name cvv-item-list &body body)
  (let ((documentation (and (stringp (car body)) (pop body)))
	(choose-buttons
	 (if (and;;cvv-item-list
	      (consp (car body)) (eq (caar body) 'buttons ))
	     (cdr (pop body))
	     '(1 2 4))))
    (when (equal choose-buttons '(4))
      (setq documentation (string-append documentation "~ ;Rt for Menu")))
    (if (null cvv-item-list)
	`(progn
	  (defmethod ,method-name ((interactor ic-interactor))
	    ,@(list documentation)
	    (with-slots (selected-pane source-pane source-pane result-pane
				       moused-pane moused-pane-x moused-pane-y
				       main-menu-button
				       source-image source-image-x source-image-y)
		interactor
	      (let ((self interactor)) (ignore self) ,@body))))
	
	(let ((raw-variables (loop for item in cvv-item-list
				   when (listp item) collect (car item))))
	  ;; should this be  (EVAL-WHEN  (LOAD EVAL COMPILE) ...)  or (PROGN ...) ????
	  `(eval-when (load eval compile)
	    (setf (get ',method-name :image-calc-interactor-cvv-menu-item-list) 
	     ',(loop for item in cvv-item-list
		     for (var-name initial-value label type . rest) = item
		     when (consp item)
		     collect `(,var-name
			       ,(or label (string var-name))
			       ,(or type :string)
			       ,@ (when initial-value `(:initial-value ,initial-value))
			       . ,rest
			       )))
		      
	    (defmethod ,method-name ((interactor ic-interactor))
	      ,@(list documentation)
	      (with-slots (selected-pane source-pane source-pane result-pane
					 moused-pane moused-pane-x moused-pane-y
					 source-image source-image-x source-image-y)
		  interactor
		(multiple-value-bind ,raw-variables
		    (xw::get-cvv-menu-variables
		     (make-cvv-panel 
		      (get ',method-name :image-calc-interactor-cvv-menu-item-list)
		      :MAKE-CVV-MENU-ARGS (:label ,(string method-name)))
		     interactor)
		  (let ((self interactor)) (ignore interactor) ,@body)))))))))
