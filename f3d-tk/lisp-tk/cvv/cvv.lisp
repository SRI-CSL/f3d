(in-package :lisptk)

#|
PARTIAL COMPATIBILITY WITH CME-6 CVV PANELS

  The idea is to support the most common uses of CME-6 CVV panels.
  Item-group reordering is moved from this file into cme-cvv-compat.lisp.
  No support here for Motif resources (:widget-args  keyword)
  ABSOLUTELY NO SUPPORT FOR MOTIF LAYOUT CONSTRAINTS.

These cvv-panels are adequate for CME-6 cvv-object-menus and the inspect-panel, but
  NOT ADEQUATE for elt-panel and registration-panel which require layout control
  that is better expressed by straight TCL/TK scripts.

|#

(defmethod tcl-string ((x cvv-item) &optional (code-p t))
  (ignore code-p)
  (widget x))

(defmethod set-item-list ((clidget cvv-item) item-list)
  (case (item-class clidget)
    (:abbrev-assoc
     (set-optionmenu-items clidget item-list))
    ((:exclusive-list :multiple-choose-list)
     (set-listbox-items clidget item-list))
    ))

(defmethod set-item-list ((clidget cvv-item) item-list)
  (setf (get-prop clidget :items) item-list)
  (set-widget-items (widget clidget) item-list))


(defmethod set-item-list ((clidget cvv-indexed-items-item) item-list)
  (setf (get-prop clidget :items) item-list)
  (set-widget-items (widget clidget)
		    (loop for item in item-list
			  for index from 0
			  collect (list index item))))

(defmethod set-item-list ((clidget cvv-indexed-items-item) item-list)
  (setf (get-prop clidget :items) item-list)
  (set-widget-items (widget clidget)
		    (loop for item in item-list
			  for index from 0
			  collect (if (consp item)
				      item
				      (list index item)))))


(defmethod selected-item ((clidget cvv-item))
  (case (item-class clidget)
    (:abbrev-assoc
     (get-optionmenu-selected-item clidget))
    ((:exclusive-list :multiple-choose-list)
     (get-listbox-selected-items clidget))
    ))


(defmethod get-optionmenu-selected-item (clidget)
  (let* ((index (read-from-string
		 (tcl-eval 'get_optionmenu_selected_item (widget clidget))))
	 (item (if (numberp index)
		   (nth index (get-prop clidget :items))
		   index)))
    (if (consp item)
	(cadr item)
	item)))

#|
Philosophy issue:

Do we really need to attempt to maintain a cached value of the widget in
   the cvv-item?

   Pro:  makes it possible to repeatedly read or write the value without communication
         with TK until the state of either the widget or the cvv-item is changed.

   Con:  must keep the widget and cvv-item in sync.
|#

(defvar *item-changed-tag* (list nil))

(defmethod item-changed-p ((item cvv-item))
  (eq (value item) *item-changed-tag*))

(defmethod set-item-changed-p ((item cvv-item) state)
  (when state
    (setf (value item) *item-changed-tag*)))

(defsetf item-changed-p set-item-changed-p)

(defmethod (setf cvv-item-value) :around (value (item cvv-item) &optional ignore)
  (declare (ignore ignore))
  (when (or (not (equal value (value item))) (item-changed-p item))
    (call-next-method)
    (setf (value item) value
	  (item-changed-p item) nil ))
  )

(defmethod cvv-item-value :around ((item cvv-item) &optional ignore)
  (declare (ignore ignore))
  (setf (value item) (call-next-method)))

;;; default method
(defmethod cvv-item-value ((item cvv-item) &optional ignore)
  (declare (ignore ignore))
  (cvv-item-value (widget item)))

(defmethod (setf cvv-item-value) (value (item cvv-item) &optional ignore)
  (declare (ignore ignore))
  (setf (cvv-item-value (widget item)) value))

(defmethod cvv-item-value ((indexed-items cvv-indexed-items-item) &optional ignore)
  (declare (ignore ignore))
  (let ((index (cvv-item-value (widget indexed-items))))
    (and index (nth index (get-prop indexed-items :items)))))

(defmethod cvv-item-value ((indexed-items cvv-indexed-items-item) &optional ignore)
  (declare (ignore ignore))
  (let* ((items (get-prop indexed-items :items))
	 (index (cvv-item-value (widget indexed-items)))
	 (item (if (numberp index)
		   (nth index items)
		   (assoc index items :test #'equal))))
    (if (consp item)
	(car item)
	item)))

(defmethod (setf cvv-item-value) (value (indexed-items cvv-indexed-items-item) &optional ignore)
  (declare (ignore ignore))
  (let ((index (position value (get-prop indexed-items :items) :test #'equal)))
    (setf (cvv-item-value (widget indexed-items)) index)))

(defmethod (setf cvv-item-value) (value (indexed-items cvv-indexed-items-item) &optional ignore)
  (declare (ignore ignore))
  (let* ((items (get-prop indexed-items :items))
	 (index (or (loop for item in items
			  for index from 0
			  when (if (consp item)
				   (equal (car item) value)
				   (equal item value))
			    return index)
		    (error "(setf cvv-item-value) cvv-indexed-items-item ~a is not present~%" value)
		    ))
	 (item (nth index items)))
    (setf (cvv-item-value (widget indexed-items))
	  (if (consp item)
	      (car item)
	      index))))



(defmethod make-cvv-item-clidget (panel class name widget &rest args
					&key (item-class 'cvv-item)
					items alist item-list
					&allow-other-keys)
  (cond (alist (setq items alist) (remf args :items)
	       (setq args (list* :items items args)))
	(item-list (setq items item-list) (remf args :item-list)
		   (setq args (list* :items items args))))
  
  (let ((clidget (make-instance item-class :name name
				:widget widget
				:class class
				:panel panel)))
    (when items (setf (get-prop clidget :items) items))
    clidget))

;;; ********************  CME-6 CVV to TK COMPATIBILITY  ********************

;;; Mappings from CME-6 CVV widget classes to TK widget classes
(defparameter *cvv-to-tk-widget-class-alist*
  '((:string qstring)
    (:multi-line-string qtext)
    (:integer qinteger)
    (:double qfloat)
    (:float qfloat)
    (:button qbutton)
    (:toggle-button qtogglebutton)
    (:check-button qcheckbutton)
    (:label qlabel)
    (:group-button qgroupbutton)
    (:group qgroupbutton)
    (:menu-button qpulldownmenu)
    (:button-list qbuttonlist)
    (:assoc qradiogroup)
    (:slider qslider)
    ;;(:yes-or-no )
    (:exclusive-list qlistbox)
    (:abbrev-assoc qoptionmenu)
    ;;(:multiple-choose-list qlistbox )
    (:form :cvv-2-column-frame)
    (:1-column-frame :cvv-1-column-frame)
    (:2-column-frame :cvv-2-column-frame)
    (:frame qframe)
    ))

(defun handle-cvv-widget-type-aliases (type)
  (or (cadr (assoc type *cvv-to-tk-widget-class-alist*))
      type))

;; Mappings from CME-6 CVV keywords to TK widget keywords.
;;;(defparameter *cvv-to-tk-translate-keywords*
;;;  '((:widget-args) (:group)     ; remove these guys
;;;    (:alist :items)
;;;    (:item-list :items)
;;;    (:visible-item-count :height)
;;;    (:button-label :text)
;;;    (:read-only :state disabled)
;;;    (:nlines :height)
;;;    (:initial-value :initial_value)))

;;;(defun filter-tk-widget-args (keys-and-vals)
;;;  (loop for (key val) on keys-and-vals by #'cddr
;;;        for entry = (assoc key *cvv-to-tk-translate-keywords*)
;;;        for replace = (cadr entry)
;;;        ;;when (equal val '(list)) do (break)
;;;        ;;when (consp val) do (setq val (tcl-constant  val))
;;;        when replace
;;;          do (setq key replace)
;;;             (when (caddr entry) (setq val (caddr entry))) ; this looks wrong
;;;        when (and (not replace) entry)
;;;          do (progn) ; (format t "Warning: ~a = ~a removed~%" key val)
;;;        else do (when (and (eq key :items) (consp val))
;;;                  (setq val (cvv-to-qwidget-items val))) ; #### CVV-TO-TK translation
;;;             and collect key and collect val))

;;; Mappings from CME-6 CVV keywords to TK widget keywords.
(defparameter *cvv-to-tk-translate-keywords*
  '((:widget-args)  ; remove these guys
    (:group :ignore)        ; ignore these guys
    (:alist :items)
    (:item-list :items)
    ;;(:selectcolor |-selectColor|)
    (:visible-item-count :height)
    ;;(:button-type :button_type)
    (:button-label :text) ; TK inconsistant here. This is wrong for menu entries, but right for buttons.
    (:read-only :state disabled)
    (:nlines :height)
    (:initial-value :initial_value)))
  
(defun filter-tk-widget-args (keys-and-vals)
  (loop for (key val) on keys-and-vals by #'cddr
	for entry = (assoc key *cvv-to-tk-translate-keywords*)
	for new-key = (cadr entry)
	for new-val = (caddr entry)
	when new-key 
	  do (setq key new-key)
	     (when new-val (setq val new-val)) ; this looks wrong
	when (and entry (not new-key))
	  do (format t "Warning: ~a = ~a removed~%" key val)
	else when (eq new-key :ignore)
	  do (progn) ; ignore it
	else do (when (and (eq key :items) (consp val))
		  (setq val (cvv-to-qwidget-items val))) ; #### CVV-TO-TK translation
	     and collect key and collect val))
  
;;; Resolve incompatibility between qwidget item lists and CVV item lists.
;;; CVV items are list ("Foo" foo).  qwidget item is (foo "Foo")
;;;(defun cvv-to-qwidget-items (alist)
;;;  (tcl-constant (loop for thing in alist
;;;                      for index from 0
;;;                      collect
;;;                      (if (consp thing)
;;;                          (let ((label (car thing))
;;;                                (name (cadr thing)))
;;;                            (cond ((> (length thing) 2)
;;;                                   ;; must be of form (type name . buttonargs)
;;;                                   thing)
;;;                                  #+no_longer_needed
;;;                                  ((or (null name) (eq name  T))
;;;                                   `(qradiobutton ,index
;;;                                     :text ,label :value ,name :indicatoron 0))
;;;                                  (t `(,name ,label))))
;;;                          thing))))

;;; Tue Aug  5 2003 Removed the incompatibility between qwidget item lists and CVV item lists.
(defun cvv-to-qwidget-items (alist)
  (tcl-constant (loop for thing in alist
		      for index from 0
		      collect
		      (if (consp thing)
			  (let ((name (car thing))
				(label (cadr thing)))
			    (cond ((> (length thing) 2)
				   ;; must be of form (type name . buttonargs)
				   thing)
				  #+no_longer_needed
				  ((or (null name) (eq name  T))
				   `(qradiobutton ,index
				     :text ,label :value ,name :indicatoron 0))
				  (t `(,name ,label))))
			  thing))))

(defparameter *cvv-to-tk-button-type-alist*
  '((:toggle-button qtogglebutton)
    (:button qbutton)
    (:check-button qcheckbutton)
    (:flat-button qflatbutton)
    (:radio-button qradiobutton)
    (:menu-button qmenubutton)
    (:pulldown-menu qpulldownmenu)
    ))


;;; The following is a hack to improve the appearance of qbuttons on
;;; MacOSX/Aqua.  It's a hack because the standard Tcl/Aqua tests
;;; produce normal-looking panels, whereas in Freedius, button
;;; widgets have their labels clipped on either side, making it
;;; difficult to read the button label.

(defun maybe-add-width-arg (args)
  #-(or agl cocoa) args
  #+(or agl cocoa)
  (destructuring-bind (name label &rest extra &key width height &allow-other-keys) args
    (append args
	    (unless width (list :width  (+ (length label) 2)))
	    (unless height (list :height 1))))
  )


;;; Handle incompatibility of CVV button specs and qwidget button specs.
;;; CVV button-specs are:
;;;   xxx | (labeltext name) | (labeltext name &rest args &key button-type)
;;; The corresponding qwidget button-specs are:
;;;   xxx | (name labeltext) | (button-widget-class name :text labeltext . args)

;;;(defun cvv-to-qwidget-button-items (items)
;;;  (loop for item in items
;;;        collect
;;;        (cond ((not (consp item))
;;;               item)
;;;              ((= (length item) 2)
;;;               `(,(cadr item) ,(car item)))
;;;              (t (let* ((args (cddr item))
;;;                        (button-type (getf args :button-type)))
;;;                   (remf args :button-type)
;;;                   `(,(or (cadr (assoc button-type *cvv-to-tk-button-type-alist*))
;;;                          'qbutton)
;;;                     ,(cadr item)
;;;                     :text ,(car item)
;;;                     . ,args))))))

;;; This list gets mapped again by cvv-to-qwidget-items
;;; (defun cvv-to-qwidget-button-items (items)
;;;   (loop for item in items
;;; 	when (or (not (consp item)) (= (length item) 2))
;;; 	  collect item
;;; 	else collect
;;; 	     (let* ((args (cddr item))
;;; 		    (button-type (getf args :button-type)))
;;; 	       (remf args :button-type)
;;; 	       `(,(or (cadr (assoc button-type *cvv-to-tk-button-type-alist*))
;;; 		      'qbutton)
;;; 		 ,(cadr item)
;;; 		 :text ,(car item)
;;; 		 . ,(if nil args (filter-tk-widget-args args))))))

(defun cvv-to-qwidget-button-items (items)
  (loop for item in items
	when (or (not (consp item)) (= (length item) 2))
	  collect (maybe-add-width-arg item)
	else collect
	     (let* ((args (cddr item))
		    (button-type (getf args :button-type)))
	       (remf args :button-type)
	       `(,(car item)		; name
		 ,(cadr item)		; label
		 ,@(and button-type
			`(:button_type ,(or (cadr (assoc button-type *cvv-to-tk-button-type-alist*))
					    (error "cvv-to-qwidget-button-items unknown button type: ~a"
						   button-type))))
		 . ,(if nil args (filter-tk-widget-args args))))))
#|
(trace cvv-to-qwidget-button-items)
(untrace cvv-to-qwidget-button-items)
(trace make-cvv-item-widget)

(cvv-to-qwidget-button-items
 '((QUIT-PANEL "Quit") (UPDATE-PANEL "Update") (LOCK-PANEL "Lock" :BUTTON-TYPE :TOGGLE-BUTTON :DOCUMENTATION "Lock panel to current object.")))
|#

;;;(defun make-cvv-item-widget (parent class name labeltext args)
;;;  (let ((args (filter-tk-widget-args args)))
;;;    (when labeltext (setq args (list* :labeltext labeltext args)))
;;;    (apply 'tcl-eval class name :parent parent args)
;;;    (widget-named parent name)
;;;    ))

;;;(defun make-cvv-item-widget (parent class name labeltext args)
;;;  (let ((args (filter-tk-widget-args args)))
;;;    (setq *xxargs* args) (break)
;;;    (when labeltext (setq args (list* :labeltext labeltext args)))
;;;    (let ((w1 (apply 'tcl-eval class name :parent parent args))
;;;          (w2 (widget-named parent name)))
;;;      (unless (equal w1 w2)
;;;        (format t "make-cvv-item-widget bad tcl return ~a ~a ~a" class w1 w2))
;;;      w2)))

#|
What value should MAKE-CVV-ITEM-WIDGET return for placement in (item-alist panel)?

   1) value of (merge-widget-pathname parent)
   2) value of (widget-named parent name)
   3) value returned by widget creator

Option 1 is the worst choice since is doesn't know whether the widget is wrapped
inside another widget.  Hence it cannot create a correct widget name.

Option 2 depends on lots of machinery in tk/library/tk-utils.tcl that I am not
sure we want to depend upon.  Besides, the widget WIDGET-NAMED returns isn't
necessarily the one we want.

Option 3 only requires that the widget creator return something reasonable.  For
simple widgets, the choice is obvious -- the created widget.  For complicated
widgets, such as scrolled widgets, pulldown and other menus, it is not clear if
we want the bottommost widget or the topmost widget in the created widget
hierarchy.  I think that the widget creators in tk/library/composite-widgets.tcl
and tk/library/qwidgets.tcl consistently return the bottommost widget.
 
|#

(defun make-cvv-item-widget (parent class name labeltext args)
  (let ((args (filter-tk-widget-args args)))
    ;;(setq *xxargs* args) (break)
    (when labeltext (setq args (list* :labeltext labeltext args)))
    (apply 'tcl-eval class name :parent parent args)
   
    ;;(widget-named parent name)
    ))

;;; *********************  MAKE-CVV-ITEM METHODS  *********************

(defmethod make-cvv-item :around (panel parent class name label
					&rest args
					&key documentation width &allow-other-keys)
  (ignore args class label name class documentation parent)
  (let* ((item (call-next-method)))
    (add-cvv-item panel item)
    #+never ; this is handled by qwidgets tcl defs.
    (when documentation
      (tcl-eval 'add_widget_doc (widget item) documentation))
    item))


(defun set-label-text (widget label)
  (tcl-cmd `(,(format nil "~a_label" widget) configure -text ,label)))


(defparameter *cvv-widget-class-alist*
  '((qoptionmenu cvv-indexed-items-item)))

;; Fall-thru method:
;;; This guy handles a large number of widget classes.
(defmethod make-cvv-item (panel parent widget-class name label &rest args
				&key &allow-other-keys)
  (unless (or (memq widget-class
		      '(qstring qtext qinteger qfloat qdouble
			qbutton qtogglebutton ; qpulldownmenu
			qradiogroup qcheckbutton
			qlistbox qoptionmenu qslider))
	      )
	  (error "make-cvv-item: unknown widget-class = ~a" widget-class))
  (apply 'make-cvv-item-clidget panel widget-class name
	 (make-cvv-item-widget parent widget-class name label args)
	 :item-class (or (cadr (assoc widget-class *cvv-widget-class-alist*)) 'cvv-item)
	 args))

;;; All this guy does differently is map :initial-value to :text
(defmethod make-cvv-item (panel parent (class (eql 'qlabel)) name label &rest args
				&key initial-value &allow-other-keys)
  (when initial-value
    (setq args (list* :text initial-value (remf args :initial-value))))
  (make-cvv-item-clidget
   panel class name
   (make-cvv-item-widget parent 'qlabel name label args)))

;;; All this guy does differently is supply the qradiogroup items.
(defmethod make-cvv-item (panel parent (class (eql :yes-or-no)) name label &rest args)
  (apply 'make-cvv-item panel parent 'qradiogroup name label
	 :items '((t "Yes") (nil "No")) args))

;;; All this guy does differently is map :group-elements to :group_widgets
;;; and convert group-elements to a tcl-constant.
(defmethod make-cvv-item (panel parent (class (eql 'qgroupbutton)) name label
				&rest args &key group-elements &allow-other-keys)
  (let ((args (remf args :group-elements)))
    (make-cvv-item-clidget
     panel class name
     (make-cvv-item-widget parent 'qgroupbutton name nil
			   (list* :text label
				  ;;:selectcolor 'gray70
				  :group_widgets (tcl-constant group-elements)
				  args)))))

;;; This handles multiple names for ITEMS and reformats the button specs.
(defmethod make-cvv-item (panel parent (class (eql 'qbuttonlist)) name label
				&rest args
                                &key item-list (items item-list)
				&allow-other-keys)
  (remf args :items)
  (remf args :item-list)
  (remf args :button-type)
  (apply 'make-cvv-item-clidget
	 panel class name
	 (make-cvv-item-widget parent 'qbuttonlist name label
			       (list* :items
				      (cvv-to-qwidget-button-items items)
				      args))
	 args))

;(cvv-to-qwidget-button-items (car *qbuttonlist-args*))
; (filter-tk-widget-args (cvv-to-qwidget-button-items(car *qbuttonlist-args*)))

(defmethod make-cvv-item (panel parent (class (eql :menu-list)) name label
				&rest args
                                &key item-list (items item-list)
				&allow-other-keys)
  (remf args :items)
  (remf args :item-list)
  (remf args :button-type)
  
  (apply 'make-cvv-item-clidget
	 panel class name
	 (make-cvv-item-widget parent 'qmenulist name label
			       (list* :items
				      (cvv-to-qwidget-button-items items)
				      args))
	 args))

(defmethod make-cvv-item (panel parent (class (eql :pulldown-menu)) name label
				&rest args)
  (apply 'make-cvv-item panel parent 'qpulldownmenu name label args))
	 


(defmethod make-cvv-item (panel parent (class (eql 'qpulldownmenu)) name label
				&rest args
                                &key item-list (items item-list)
				&allow-other-keys)
  (remf args :items)
  (remf args :item-list)
  (remf args :button-type)
  
  (apply 'make-cvv-item-clidget
	 panel class name
	 (make-cvv-item-widget parent 'qpulldownmenu name label
			       (list* :items
				      (cvv-to-qwidget-button-items items)
				      args))
	 args))

;;; All this guy does differently is add :selectmode multiple to the widget args.
(defmethod make-cvv-item (panel parent (class (eql :multiple-choose-list))
				name label &rest args)
  (make-cvv-item-clidget panel class name
			 (make-cvv-item-widget parent 'qlistbox name label
					       (list* :selectmode 'multiple args))))

;;;; These methods belong elsewhere
(defmethod get-listbox-selected-items ((clidget cvv-item))
  (let ((item-indices (get-listbox-selected-item-indices (widget clidget))))
    (loop with items = (get-prop clidget :items)
	  for i in item-indices
	  for item = (nth i items)
	  collect (if (consp item) (cadr item) item))))

(defmethod set-listbox-items ((clidget cvv-item) list)
  (setf (get-prop clidget :items) list)
  (set-listbox-items (widget clidget) list))

(defmethod add-listbox-item ((clidget cvv-item) new-item)
  (setf (get-prop clidget :items) (nconc (get-prop clidget :items) (list new-item)))
  (add-listbox-item (widget clidget) new-item))

(defmethod set-optionmenu-items ((clidget cvv-item) list &optional select-item)
  (setf (get-prop clidget :items) list)
  (set-optionmenu-items (widget clidget) list select-item))

;;; ****************  REORDERING ITEMLIST IN GROUP ORDER  ****************


(defun cvv-item-group (cvv-item)
  (getf (cdddr cvv-item) :group))

(defun extract-groups (itemlist)
  (let ((group-buttons (loop for (item-name label-string type . key-vals) in itemlist
			     do (ignore label-string key-vals)
			     when (eq type :group)
			       collect item-name))
	(group-references
	 (loop with groups
	       for (item-name label-string type . key-vals) in itemlist
	       for group = (getf key-vals :group)
	       do (ignore item-name label-string type)
	       when group
		 do (pushnew group groups)
	       finally (return groups))))
    (values (cons :preamble group-buttons)
	    (reverse group-references))))

;;; This preserves the order of groups and items
(defun infer-group-button-items (itemlist &optional default-group-button-items)
  (multiple-value-bind (defined-group-buttons referenced-groups)
      (extract-groups itemlist)
    (values
     ;; group-button items
     (append (loop for item in default-group-button-items
		   for (item-name label-string type) = item
		   do (ignore label-string)
		   when (and (eq type :group)
			     (memq item-name referenced-groups)
			     (memq item-name defined-group-buttons))
		     collect (assoc item-name itemlist)
		   else when (and (eq type :group)
				  (memq item-name referenced-groups))
			  collect item)
	     ;; pick up any group buttons not in default-group-button-items
	     (loop for item in itemlist
		   for (item-name label-string type) = item
		   do (ignore item-name label-string)
		   when (and (eq type :group)
			     (not (assoc item-name default-group-button-items)))
		     collect item))
     
     ;; non group-button items
     (loop for item in itemlist
	   for (item-name label-string type) = item
	   do (ignore item-name label-string type)
	   unless (eq type :group)
	     collect item)
	    
     ;; unhandled groups
     (loop with groups
	   for (item-name label-string type . key-vals) in itemlist
	   for group = (getf key-vals :group)
	   do (ignore item-name label-string type)
	   when (and group
		     (not (or (memq group defined-group-buttons)
			      (assoc group default-group-button-items))))
	     do (pushnew group groups)
	   finally (return (reverse groups)))
     )))

;;; This preserves the order of groups and items
;;;(defun reorder-cvv-itemlist-by-groups (group-items non-group-items)
;;;  (append
;;;   (loop for group-item in group-items
;;;         for (group-name group-label-string group-type . group-key-vals) = group-item
;;;
;;;         for this-group-items = (loop for item in non-group-items
;;;                                      for (item-name label-string type . key-vals) = item
;;;                                      for group = (getf key-vals :group)
;;;                                      do (ignore item-name label-string type key-vals)
;;;                                      when (eq group group-name)
;;;                                        collect item)
;;;
;;;         do (ignore group-label-string group-type)
;;;         append (loop for item in non-group-items
;;;                      for (item-name label-string type . key-vals) = item
;;;                      for group = (getf key-vals :group)
;;;                      do (ignore label-string type)
;;;                      when (eq group group-name)
;;;                        collect item into items
;;;                        and collect item-name into group-elements
;;;                      finally
;;;                   (return `(,@(unless (getf group-key-vals :nobutton)
;;;                                       `((,@group-item :group-elements ,group-elements)))
;;;                             ,@items))))
;;;   non-group-items))

(defun reorder-cvv-itemlist-by-groups (group-items non-group-items)
  (append
   (loop for group-item in group-items
	 for (group-name group-label-string group-type . group-key-vals) = group-item
	 do (ignore group-label-string group-type)
	 append (loop for item in non-group-items
		      for (item-name label-string type . key-vals) = item
		      for group = (getf key-vals :group)
		      do (ignore label-string type)
		      when (eq group group-name)
			do (setf non-group-items (remove item non-group-items :test #'equal))
			and collect item into items
			and collect item-name into group-elements
		      finally
		   (return `(,@(unless (getf group-key-vals :nobutton)
				       `((,@group-item :group-elements ,group-elements)))
			     ,@items))))
   non-group-items))


(defun reorder-cvv-itemlist-group-order (item-list)
  (let* (grouped-items
	 (group-buttons
	  (loop for item in item-list
		for (item-name label-string type . key-vals) = item
		do (ignore item-name label-string key-vals)
		when (eq type :group)
		  collect item))
	 (preamble-items
	  (loop for item in item-list
		when (eq (cvv-item-group item) :preamble)
		  collect item
		  and do (push item grouped-items)))
	 (reordered-item-list
	  (append preamble-items
		  (loop for group-button-item in group-buttons
			for target-group = (car group-button-item)
			do (push group-button-item grouped-items)
			collect group-button-item
			append (loop for item in item-list
				     when (eq (cvv-item-group item) target-group)
				       collect item
				       and do (push item grouped-items))))))
    (append reordered-item-list
	    (set-difference item-list grouped-items :test #'equal))))


;;; *******************  FORMS AND FRAMES *******************

;;; The difference between :FORM and :FRAME is the order of args in the item-list.

;;; This is for "normal" cvv item-lists.
;;; Each item is: (slot-symbol label-string type ...)
(defmethod make-cvv-form-item-list (panel parent item-list)
  (loop for (slot-symbol label-string type0 . rest) in item-list
	for type = (handle-cvv-widget-type-aliases type0)
	do (apply 'make-cvv-item panel parent type slot-symbol label-string rest)))

;;; This "nearly" compatible with TK widget creation forms.
;;; The only difference is that :label-text is implicit here.
;;; Each item is: (type slot-symbol label-string ...)
;;; This is used by inspector.
(defmethod make-cvv-frame-item-list (panel parent item-list)
  (loop for (type0 slot-symbol label-string . rest) in item-list
        for type = (handle-cvv-widget-type-aliases type0)
        do (apply 'make-cvv-item panel parent type slot-symbol label-string rest)))

(defmethod make-cvv-item (panel parent (type (eql :cvv-2-column-frame)) name label &rest args
				&key item-list (items item-list) &allow-other-keys)
  (ignore label)
  (remf args :item-list) (remf args :items)
  (let ((frame-path (make-cvv-item-widget parent 'qframe name nil args)))
    (make-cvv-form-item-list panel frame-path items)
    (tcl-eval 'qgrid_2_column frame-path)	     
    (tcl-eval 'initialize_group_control_state frame-path)
    (make-cvv-item-clidget panel type name frame-path)
    ))

(defmethod make-cvv-item (panel parent (type (eql :cvv-1-column-frame)) name label &rest args
				&key item-list (items item-list) &allow-other-keys)
  (ignore label)
  (remf args :item-list) (remf args :items)
  (let ((frame-path (make-cvv-item-widget parent 'qframe name nil args)))
    (make-cvv-form-item-list panel frame-path items)
    (tcl-eval 'qgrid_1_column frame-path)
    (tcl-eval 'initialize_group_control_state frame-path)
    (make-cvv-item-clidget panel type name frame-path)
    ))


(defmethod make-cvv-item (panel parent (type (eql :cvv-1-row-frame)) name label &rest args
				&key item-list (items item-list) &allow-other-keys)
  (ignore label)
  (remf args :item-list) (remf args :items)
  (let ((frame-path (make-cvv-item-widget parent 'qframe name nil args)))
    (make-cvv-form-item-list panel frame-path items)
    (tcl-eval 'qgrid_1_row frame-path)
    (tcl-eval 'initialize_group_control_state frame-path)
    (make-cvv-item-clidget panel type name frame-path)
    ))

;;; This doesn't look very useful.
;;;(defmethod make-cvv-item (panel parent (type (eql 'qframe)) name label &rest args
;;;                                &key item-list (items item-list) &allow-other-keys)
;;;  (ignore label)
;;;  (remf args :item-list) (remf args :items)
;;;  (let ((frame-path (make-cvv-item-widget parent 'qframe name nil args)))
;;;    (make-cvv-frame-item-list panel frame-path items)
;;;    (tcl-eval 'qgrid_2_column frame-path)
;;;    (make-cvv-item-clidget panel type name frame-path)))

;;; This accepts vanilla tcl scripts and does no layout control of its own.
(defmethod make-cvv-item (panel parent (type (eql 'qframe)) name label &rest args
                                &key script &allow-other-keys)
  (remf args :script)
  (let ((frame-path (make-cvv-item-widget parent 'qframe name label args)))
    (tcl-eval 'set_global 'default_parent frame-path)
    (tcl-script script)
    (make-cvv-item-clidget panel type name frame-path)))

;;; Must move this to a file loaded by the gui subsystem.
;;; Moved to basic-gui/tk-window-panel.lisp
;;;(defmethod make-cvv-item (panel parent (type (eql :glwindow)) name label &rest args
;;;                                &key width height &allow-other-keys)
;;;  (mv-bind (script wframe-name tkglwin-name)
;;;      (apply 'glwindow-script parent name args)
;;;    (let* ((clidget (make-cvv-item-clidget panel :glwindow name wframe-name))
;;;           (pane-class gui::*default-pane-class*)
;;;           (window (gui::make-window tkglwin-name :class pane-class)))
;;;      (push (cons nil window) (gui::frame-pane-alist panel))
;;;      (setf (gui::panel window) panel)
;;;      clidget)))


;;; *****************************  MAKE-CVV-PANEL  *****************************

(defvar *default-cvv-group-items* nil)

;;; This is a simple version that uses the specified item-list without change.
;;;(defmethod make-cvv-panel (item-list
;;;                           &key
;;;                           (panel-class 'cvv-panel) package
;;;                           title
;;;                           screen       ; currently ignored
;;;                           (resource-name "Cvv")
;;;                           callback-function
;;;                           (pop-up t)
;;;                           position     ;(position :mouse)
;;;                           (cache-p t)  ; currently ignored
;;;                           (group-enable t)
;;;                           ;;item-group-specs
;;;                           ;;item-group-name-list
;;;                           height
;;;                           (default-group-items *default-cvv-group-items*)
;;;                           &allow-other-keys)
;;;  (declare (ignore screen cache-p))
;;;  (when group-enable
;;;    (setq item-list
;;;          (multiple-value-bind (group-buttons non-group-buttons unhandled-groups)
;;;              (infer-group-button-items item-list default-group-items)
;;;            (declare (ignore unhandled-groups))
;;;            (reorder-cvv-itemlist-by-groups group-buttons non-group-buttons))))
;;;
;;;  (setq item-list (reorder-cvv-itemlist-group-order item-list))
;;;  (let* ((panel-widget-name (gensym-toplevel-widget-path))
;;;         (panel (make-instance panel-class :widget panel-widget-name
;;;                                        ;:package package
;;;                               :height height
;;;                               :package (or package (unless callback-function :tk))
;;;                               :callback-function callback-function)))
;;;    
;;;    (make-cvv-item-widget '|.| 'qtoplevel panel-widget-name nil
;;;                          `(,@(and resource-name `(:class  ,resource-name))
;;;                            ,@(and title `(:title ,title))))
;;;    
;;;    (make-cvv-form-item-list panel panel-widget-name item-list)
;;;    (unless pop-up (tk-wm 'withdraw panel-widget-name))
;;;    (tcl-eval 'qgrid_2_column panel-widget-name)
;;;    (setf (widget-clidget (widget panel)) panel)
;;;    (tcl-eval 'initialize_group_control_state panel-widget-name)
;;;    (when (and pop-up position)
;;;      (do-events)
;;;      (set-toplevel-widget-position panel-widget-name position))
;;;    panel))


;;; Make :cache-p t work
;;;(defmethod make-cvv-panel (item-list
;;;                           &key
;;;                           (panel-class 'cvv-panel) package
;;;                           title
;;;                           screen       ; currently ignored
;;;                           (resource-name "Cvv")
;;;                           callback-function
;;;                           (pop-up t)
;;;                           ;position    
;;;                           (position :mouse)
;;;                           (cache-p t)  
;;;                           (group-enable t)
;;;                           ;;item-group-specs
;;;                           ;;item-group-name-list
;;;                           height
;;;                           (default-group-items *default-cvv-group-items*)
;;;                           &allow-other-keys)
;;;  (declare (ignore screen))
;;;  (when group-enable
;;;    (setq item-list
;;;          (multiple-value-bind (group-buttons non-group-buttons unhandled-groups)
;;;              (infer-group-button-items item-list default-group-items)
;;;            (declare (ignore unhandled-groups))
;;;            (reorder-cvv-itemlist-by-groups group-buttons non-group-buttons))))
;;;
;;;  (setq item-list (reorder-cvv-itemlist-group-order item-list))
;;;
;;;  (flet ((make-panel ()
;;;           (let* ((panel-widget-name (gensym-toplevel-widget-path))
;;;                  (panel (make-instance panel-class :widget panel-widget-name
;;;                                        ;:package package
;;;                                        :height height
;;;                                        :package (or package (unless callback-function :tk))
;;;                                        :callback-function callback-function)))
;;;             (setf (widget-clidget (widget panel)) panel)
;;;             (make-cvv-item-widget '|.| 'qtoplevel panel-widget-name nil
;;;                                   `(,@(and resource-name `(:class  ,resource-name))
;;;                                     ,@(and title `(:title ,title))))
;;;    
;;;             (make-cvv-form-item-list panel panel-widget-name item-list)
;;;             (unless pop-up (tk-wm 'withdraw panel-widget-name))
;;;             (tcl-eval 'qgrid_2_column panel-widget-name)
;;;             (tcl-eval 'initialize_group_control_state panel-widget-name)
;;;             (setf (item-alist panel)
;;;                   (item-alist-from-widget-tree panel-widget-name (panel-package panel)))
;;;             (when (and pop-up position)
;;;               (set-toplevel-widget-position (widget panel) position)
;;;               (do-events))
;;;             panel)))
;;;    (let ((panel (if cache-p
;;;                     (eval-cache (make-cvv-panel item-list title)
;;;                         (make-panel))
;;;                     (make-panel))))
;;;      ;;(format t "make-cvv-panel ~a~%" (widget panel))
;;;      panel)))

(defparameter *cvv-panel-default-resource-name* "Cvv")

(defmethod make-cvv-panel (item-list
			   &key
			   (panel-class 'cvv-panel) 
			   package
			   title
			   screen	; currently ignored
			   (resource-name *cvv-panel-default-resource-name*)
			   callback-function
			   (pop-up t)
			   ;position	
			   (position :mouse)
			   (cache-p t)	
			   (group-enable t)
			   (item-alist-from-widget-tree t)
			   ;;item-group-specs
			   ;;item-group-name-list
			   height
			   (default-group-items *default-cvv-group-items*)
			   &allow-other-keys)
  (declare (ignore screen))
  (when group-enable
    (setq item-list
	  (multiple-value-bind (group-buttons non-group-buttons unhandled-groups)
	      (infer-group-button-items item-list default-group-items)
	    (declare (ignore unhandled-groups))
	    (reorder-cvv-itemlist-by-groups group-buttons non-group-buttons))))

  (setq item-list (reorder-cvv-itemlist-group-order item-list))

  (flet ((make-panel ()
	   (let* ((panel-widget-name (gensym-toplevel-widget-path ))
		  (panel (make-instance panel-class :widget panel-widget-name
					;:package package
					:height height
					:package (or package (unless callback-function :tk))
					:callback-function callback-function)))
	     (setf (widget-clidget (widget panel)) panel)
	     (make-cvv-item-widget '|.| 'qtoplevel panel-widget-name nil
				   `(,@(and resource-name `(:class  ,resource-name))
				     ,@(and title `(:title ,title))))
    
	     (make-cvv-form-item-list panel panel-widget-name item-list)
	     (tcl-eval 'qgrid_2_column panel-widget-name)
	     (tcl-eval 'initialize_group_control_state panel-widget-name)
	     (when item-alist-from-widget-tree 
	       (merge-widget-tree-item-list panel))
	     (when (and pop-up position)
	       (set-toplevel-widget-position (widget panel) position)
	       (do-events))
	     panel)))
    (let ((panel (if cache-p
		     (eval-cache (make-cvv-panel item-list title)
			 (make-panel))
		     (make-panel))))
      (if pop-up
	  (pop-up-panel panel)
	  (tk-wm 'withdraw (widget panel)))
      (do-events)
      panel)))

;;; This version has an additional frame widget directly under the toplevel widget
;;; in order to support proper ttk theme inheritance.
(defmethod make-cvv-panel (item-list
			   &key
			   (panel-class 'cvv-panel) 
			   package
			   title
			   screen	; currently ignored
			   (resource-name *cvv-panel-default-resource-name*)
			   callback-function
			   (pop-up t)
			   ;position	
			   (position :mouse)
			   (cache-p t)	
			   (group-enable t)
			   (item-alist-from-widget-tree t)
			   ;;item-group-specs
			   ;;item-group-name-list
			   height
			   (default-group-items *default-cvv-group-items*)
			   &allow-other-keys)
  (declare (ignore screen))
  (when group-enable
    (setq item-list
	  (multiple-value-bind (group-buttons non-group-buttons unhandled-groups)
	      (infer-group-button-items item-list default-group-items)
	    (declare (ignore unhandled-groups))
	    (reorder-cvv-itemlist-by-groups group-buttons non-group-buttons))))

  (setq item-list (reorder-cvv-itemlist-group-order item-list))

  (flet ((make-panel ()
	   (let* ((panel-widget-name (gensym-toplevel-widget-path ))
		  ;; added a ttk::frame directly under toplevel so ttk theme inheritance works correctly.
		  (top-frame-widget (merge-widget-pathname panel-widget-name "topfrm"))
		  (panel (make-instance panel-class :widget panel-widget-name
					;:package package
					:height height
					:package (or package (unless callback-function :tk))
					:callback-function callback-function)))
	     (setf (widget-clidget (widget panel)) panel)
	     #+never
	     (make-cvv-item-widget '|.| 'qtoplevel panel-widget-name nil
				   `(,@(and resource-name `(:class  ,resource-name))
				     ,@(and title `(:title ,title))))
	     (tcl-cmd `(qtoplevel ,panel-widget-name 
				  ,@(and resource-name `(:class  ,resource-name))
				  ,@(and title `(:title ,title))))
	     (tcl-cmd `(qframe ,top-frame-widget))
	     (tcl-cmd `(pack ,top-frame-widget -fill both -expand true))
	     (make-cvv-form-item-list panel top-frame-widget item-list)
	     (print top-frame-widget)
	     (tcl-eval 'qgrid_2_column top-frame-widget)
	     (tcl-eval 'initialize_group_control_state top-frame-widget)
	     (when item-alist-from-widget-tree 
	       (merge-widget-tree-item-list panel))
	     (when (and pop-up position)
	       (set-toplevel-widget-position (widget panel) position)
	       (do-events))
	     panel)))
    (let ((panel (if cache-p
		     (eval-cache (make-cvv-panel item-list title)
			 (make-panel))
		     (make-panel))))
      (if pop-up
	  (pop-up-panel panel)
	  (tk-wm 'withdraw (widget panel)))
      (do-events)
      panel)))

;(get 'make-cvv-panel :function-cache)
;(eval-cache-flush-function 'make-cvv-panel)

(defmethod merge-widget-tree-item-list (panel)
  (let* ((widget-tree-item-alist (item-alist-from-widget-tree (widget panel) (panel-package panel)))
	 (missing-items (loop for item in widget-tree-item-alist
			      for (item-name widget) = item
			      unless (cvv-item panel item-name)
				collect item)))
    (when missing-items
      (setf (item-alist panel)
	    (append (item-alist panel) missing-items)))
    (item-alist panel)))

;(merge-widget-tree-item-list tk-inspect::*inspector-panel*)
    

;;; Where does this belong?
#+never ; unused
(defmethod bind-popup-menu-to-window ((popup-menu cvv-item) window
				      &optional (mouse-event '<alpha-right>))
  (bind-popup-menu-to-window (widget popup-menu) window mouse-event))

;;; Random tests

#|
(in-package :cl-user)

(defun getenv (var)
  #+cmu (cdr (assoc var ext:*environment-list* :test #'string=))
  #+sbcl (posix-getenv var)
  #+allegro (sys::getenv var))

(defun (setf getenv) (val var)
  #+cmu (push (cons var val) ext:*environment-list*)
  #+allegro (setf (sys::getenv var) val)
  val)

;; in emacs do
(progn

(setf (getenv "FREEDIUS_ARCH") 
      (format nil "~a/arch/linux-cmucl" (getenv "FREEDIUS")))

(setf (getenv "FREEDIUS_ARCH") 
      (format nil "~a/arch/linux-sbcl64" (getenv "FREEDIUS")))
)


(load (format nil "~a/lisp/boot.lisp" (getenv "FREEDIUS")))

(progn
  (st::load-config-files)
  (st:load-system :cvv)
  )

(tk::init-lisptk)

(tk::repl)
;;; For CMUCL, in the *slime-repl* listener do (tk::repl)
;;; For Allegro, in the *inferior-lisp* listener do (tk::repl)

(in-package :lisptk)

(let ((resource-name "foo") (title "bar"))
   `(,@(and resource-name `(:class  ,resource-name))
			    ,@(and title `(:title ,title))))
(setq item-list
      '((controls nil :button-list :items
	 ((1 "Foo") (2 "Bar")))
	(txt "Timezone" :string)
	(num "How Many" :integer)
	(but1 nil :button :button-label "DOIT" :anchor w)
	(list "List:" :exclusive-list :height 3 :alist ("A" "B" "c" "D" "E" "F"))
	))

(setq panel (make-cvv-panel item-list :title "Foo" :position '(100 100)))

(setq *tk-verbose* nil)

(setq panel
      (make-cvv-panel
       '((buts nil :button-list :items ((foo "Foo") (bar "Bar")))
	 (year "Year:" :integer :oformat "%6i" :initial_value 1998)
	 (angle "Angle:" :double :format "%f radians" :justify right
	  :oformat "%18.6f radians" :initial_value 3.1415926)
	 (author "Author:" :string :initial_value "L. H. Quam"))))

(cvv-item-value panel 'year)
(setf (cvv-item-value panel 'year) "2005")
(setf (cvv-item-value panel 'year) "2000")
(cvv-item-value panel 'angle)

(let ((toplevel-name ".cvv-test1"))
  (tcl-script
   `((set_global default_parent ,toplevel-name)
     (qtoplevel $default_parent -title "CVV Test1" )
     (qbuttonlist buts -items '((foo "Foo") (bar "Bar")))
     (qinteger year -labeltext "Year" -initial_value 1998 :oformat "%6i")
     (qfloat angle -labeltext "Angle" -initial_value 3.1415926
      :format "%f radians" :oformat "%18.6f radians")
     (qstring author -labeltext "Author" -initial_value "Author:")
     (qgrid_2_column $default_parent)))
  (make-widget-panel toplevel-name nil))

(tcl-cmd '(.top2.angle xview 0))
(tcl-cmd '(.top2.angle xview))

(cvv-item panel 'int)
(setf (cvv-item-value panel 'int) 456)
(setf (cvv-item-value panel 'double) 123.456)
(setf (cvv-item-value panel 'string) "Foo")

(tcl-eval (cvv-widget panel 'string) 'config :background 'red)
(tcl-eval (cvv-widget panel 'string) 'config :background "#ea7680")
(cvv-item-value panel 'int)
(cvv-item-value panel 'double)
(cvv-item 'double panel)
(tcl-eval 'bindtags (cvv-item panel 'double))
(inspect (cvv-item panel 'double))
(widget panel)
(progn hoo)
(tk-destroy (widget panel))
(widget panel)
(tk-destroy (cvv-widget panel 'double))
|#


;;;
;;; This is useful....
;;;

(defun popup-notice (string)
  (make-cvv-panel
   `((message "" :string :initial-value ,string :width ,(length string))
     (quit-panel nil :button :button-label "OK"))
   :title "FREEDIUS Notice"))

