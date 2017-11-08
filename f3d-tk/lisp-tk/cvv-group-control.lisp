(in-package :tk)

#|
The functionality of this file has moved into the tcl/tk code.

|#


(defparameter *group-enable-keyword* :group-enable)
;; (defparameter *group-enable-keyword* :group)

(defun compute-group-list (item-list group-spec)
  (let* (group-name-list
	 group-item-names-alist
	 last-group-name)
    (labels ((add-group (group-name)
	       (unless (memq group-name group-name-list)
		 (setq group-name-list (nconc group-name-list (list group-name))
		       group-item-names-alist (nconc group-item-names-alist (list (list group-name ))))))
	     (add-group-item-name (group-name item-name)
	       (add-group group-name)
	       (let ((group-item-names (cdr (assoc group-name group-item-names-alist))))
		 (unless (memq item-name group-item-names)
		   (putassoc group-item-names-alist group-name
				 (nconc group-item-names (list item-name))))))

	     (find-item-group (item-name)
	       (loop for (group-name . item-names) in group-item-names-alist
		     when (memq item-name item-names)
		       return group-name
		     finally (return last-group-name )))
	     )

      (loop for thing in group-spec
	    for group-name = (if (consp thing)
				 (if (consp (car thing))
				     (caar thing) ; thing is (group-enable-item . item-names)
				     (car thing)) ; thing is (group-name . item-names)
				 thing)	; thing is group-name
	    for group-item-names = (and (consp thing) (cdr thing))
	    when group-name
	      do (add-group group-name)
		 (loop for item-name in group-item-names
		       do (add-group-item-name group-name item-name))	      
	    )

      ;; This is the last group name specified by the explicitly passed group-name-list or group-spec
      ;; This group becomes the catch all for items with unspecified group.
      (setq last-group-name (car (last group-name-list)))

      (let ((last-group-items (last group-item-names-alist)))
	(setq group-name-list (butlast group-name-list))
	(setq group-item-names-alist (butlast group-item-names-alist))
	
	(loop for item in item-list
	      for (item-name label-string type . key-vals) = item
	      do (ignore label-string key-vals)
	      when (eq type *group-enable-keyword*)
		do (add-group item-name)
		   (setq group-spec (append group-spec (list (list item))))
	      )
	
	(setq group-name-list (append group-name-list (list last-group-name)))
	(setq group-item-names-alist (append group-item-names-alist last-group-items)))
      (setq item-list
	    (loop for item in item-list
		  for (item-name label-string type . key-vals) = item
		  for group-name = (getf key-vals :group)
		  do (ignore item-name label-string type)
		  when (eq type *group-enable-keyword*)
		    do (progn nil)
		  else when group-name
			 do (add-group-item-name group-name item-name )
			 and collect item
		  else collect
		       (let ((group-name (find-item-group item-name)))
			 (if group-name
			     (list* item-name label-string type :group group-name key-vals)
			     item))))
      (values item-list
	      group-name-list
	      group-item-names-alist
	      group-spec
	      ))))


(defparameter *cvv-panel-separator-item* nil)

(defun reorder-items-by-group-item-list-order (item-list group-item-list)
  (loop for item-name in group-item-list
	for found-item = (if t
			     (assoc item-name item-list)
			     (loop for item in item-list
				   for (item-name2) = item
				   when (eq item-name2 item-name)
				     return item))
	when found-item
	  collect found-item into reordered-item-list
	  and collect item-name into reordered-item-names
	finally (return (append reordered-item-list
				(loop for item in item-list
				      for (item-name) = item
				      unless (memq item-name reordered-item-names)
					collect item)))))
     
	

(defun reorder-cvv-panel-item-list (item-list group-spec)
  (unless group-spec
    (if (loop for item in item-list
	      thereis (and (consp item) (eq (caddr item) *group-enable-keyword*)))
	(setq group-spec '((:preamble) (:misc)))
	(return-from reorder-cvv-panel-item-list item-list)))

  (multiple-value-bind (item-list group-list group-item-names-alist group-spec)
      (compute-group-list item-list group-spec )
    (setq *foo3* item-list)
    (if (null group-list)
	item-list 
	(flet ((get-group-enable-menu-item (group-name)
		 (loop for (group-header-item) in group-spec
		       when (and (consp group-header-item)
				 (eq (car group-header-item) group-name))
			 return group-header-item )
		 (assoc group-name item-list)))
	  (let* ((accepted-item-names nil)
		 (items-in-groups
		  (loop for (group . rest-groups) on group-item-names-alist by #'cdr
			for (group-header-item . group-items) = group
			for group-name = (or (if (consp group-header-item)
						 (car group-header-item )
						 group-header-item)
					     (error "group-spec has no group-name: ~a~%" group ))
				       
			for items = (loop for item in item-list
					  for (item-name) = item
					  when (or (memq item-name group-items)
						   ;;(eq item-name group-name)
						   (eq group-name
						       (getf (cdddr item) :group)))
					    do (push item-name accepted-item-names)
					    and collect item)
			do (ignore group-items)
			when (null rest-groups)
			  ;; the remaining items (MISC group) go at the end
			  do (setq items (nconc items
						(loop for item in item-list
						      for (item-name) = item
						      unless (or (memq item-name accepted-item-names)
								 ;;(assoc item-name group-list)
								 (memq item-name group-list)
								 )
							do (format t "adding ~a~%" item-name) and
						      collect item)))
			do (setq items
				 (reorder-items-by-group-item-list-order items
									 (list* group-name  group-items)))
			when (and items group-header-item)
			  do (let ((group-enable-menu-item (get-group-enable-menu-item group-name )))
			       (when group-enable-menu-item
				 (setq items
				       (if *cvv-panel-separator-item*
					   (list* *cvv-panel-separator-item* group-enable-menu-item items)
					   (list* group-enable-menu-item items )))))
			when items nconc items )) )

	    (values items-in-groups group-list group-item-names-alist) )))))

;;; This is for compatibility with CME-6 cvv-object-panels.
;;; This has been merged into MAKE-CVV-PANEL
;;;(defmethod make-cvv-panel2 (item-list &rest args &key
;;;                                      (panel-class 'cvv-panel)
;;;                                      title  screen resource-name
;;;                                      callback-function
;;;                                      (group-enable t)
;;;                                      item-group-specs item-group-name-list
;;;                                      (pop-up t) position package
;;;                                      (cache-p t) &allow-other-keys)
;;;  (ignore args title screen callback-function pop-up position cache-p
;;;          package resource-name panel-class)
;;;  (when group-enable
;;;    (multiple-value-setq (item-list item-group-name-list item-group-specs)
;;;      (reorder-cvv-panel-item-list item-list item-group-specs)))
;;;  (apply 'make-cvv-panel item-list args)
;;;  )

;;; This stuff appears to be unused.

;;;(defmethod merge-group-item-list ((panel cvv-panel) group-name)
;;;  (let ((group-item-names (cdr (assoc group-name (get-prop panel :item-group-specs)))))
;;;    (append group-item-names
;;;            (loop for (item-name item) in (item-alist panel)
;;;                  for item-group = (get-prop item :group)
;;;                  when (and (eq item-group group-name)
;;;                            (not (memq item-name group-item-names)))
;;;                    collect item-name))))

;;;(defmethod merge-group-item-lists ((panel cvv-panel))
;;;  (loop for (group-name) in (get-prop panel :item-group-specs)
;;;        collect (cons group-name (merge-group-item-list panel group-name))))

(defmethod merge-group-item-lists ((panel cvv-panel))
  (loop for (group-name) in (get-prop panel :item-group-specs)
	collect
	(cons group-name
	      (let ((group-item-names (cdr (assoc group-name (get-prop panel :item-group-specs)))))
		(append group-item-names
			(loop for (item-name item) in (item-alist panel)
			      for item-group = (get-prop item :group)
			      when (and (eq item-group group-name)
					(not (memq item-name group-item-names)))
				collect item-name))))))

(defun set-all-item-groups-visibility (panel item-group-name-list item-group-specs )
  (setf (get-prop panel :item-group-name-list) item-group-name-list
	(get-prop panel :item-group-specs) item-group-specs )
  (setf (get-prop panel :item-group-specs) (merge-group-item-lists panel))
  (loop for group-name in item-group-name-list
	for group-widget = (widget (get-named-item panel group-name))
	when (cvv-item-value group-name )
	  do (tcl-eval 'map_group group-widget)
	else do (tcl-eval 'unmap_group group-widget)))



#|

(setq item-list *house-object-cvv-item-list*)
(setq item-list '((NAME "Name:" :STRING)
		  (EDIT-GRAPHICS-STYLE NIL :BUTTON :BUTTON-LABEL "Edit Graphics Style"
		   :WIDGET-ARGS ("alignment" 0))
		  (ANNOTATION "ANNOTATION:" :GROUP :GROUP-ELEMENTS (DESCRIPTION))
		  (DESCRIPTION "Description:" :MULTI-LINE-STRING :NLINES 3)
		  (HIERARCHY "HIERARCHY:" :GROUP :GROUP-ELEMENTS
		   (CHANGE-SUPERIOR FEATURE-SETS))
		  (CHANGE-SUPERIOR "Superior:" :EXCLUSIVE-LIST :VISIBLE-ITEM-COUNT 3
		   :GROUP HIERARCHY)
		  (FEATURE-SETS "Feature Sets:" :MULTIPLE-CHOOSE-LIST
		   :VISIBLE-ITEM-COUNT 5 :GROUP HIERARCHY)))

(setq item-list '((HIERARCHY "HIERARCHY:" :GROUP :GROUP-ELEMENTS
		   (CHANGE-SUPERIOR FEATURE-SETS))
		  (CHANGE-SUPERIOR "Superior:" :EXCLUSIVE-LIST :VISIBLE-ITEM-COUNT 3
		   :GROUP HIERARCHY)
		  (FEATURE-SETS "Feature Sets:" :MULTIPLE-CHOOSE-LIST
		   :VISIBLE-ITEM-COUNT 5 :GROUP HIERARCHY)))

(pprint (REORDER-CVV-PANEL-ITEM-LIST item-list nil))

(compute-group-list item-list nil )

(compute-group-list *foo3* nil)
|#


