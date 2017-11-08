(in-package :lisptk)


(defmacro with-cvv-items (items panel &body body)
  `(symbol-macrolet
    ,(loop for item-spec in items
	   for var = (if (consp item-spec)
			 (first item-spec)
			 item-spec)
	   for item-name = (if (consp item-spec)
			       (or (second item-spec) var)
			       var)
	   collect `(,var (cvv-item-value (cvv-item ,panel ',item-name))))
    (progn .,body)))


;;; ******************  CVV-PANEL CLASS  ******************

(defclass cvv-panel (widget-panel) ())

#+no-longer-needed
(defmethod tk-destroy-callback :after ((panel cvv-panel) widget &rest args)
  (declare (ignore args))
  (let ((clidget (widget-clidget widget)))
    (when (eq clidget panel)
      (format t "tk-destroy-callback ~a~%" panel)
      (lx::eval-cache-flush panel))))
#+never
(defmethod tk-destroy-callback :after ((panel cvv-panel) widget &rest args)
  )
  
(defmethod set-panel-title ((panel widget-panel) title)
  (set-toplevel-widget-title (widget panel) title))

(defmethod set-item-label ((menu widget-panel ) item-name label)
  (let ((item (get-named-item menu item-name)))
    (when item (set-label-text (widget item) label )
      )))

(defmethod add-cvv-item ((panel widget-panel) item)
  (setf (widget-clidget (widget item)) item)
  (setf (item-alist panel)
	(append (item-alist panel) (list (list (name item) item)))))

(defmethod get-named-item ((panel widget-panel ) name)
  (cadr (assoc name (item-alist panel ))))

(defmethod get-item-value ((panel widget-panel ) name)
  (cvv-item-value (get-named-item panel name)))

(defmethod set-item-value ((panel widget-panel ) name value)
  ;;(format t "set-item-value ~a ~A~%" name value)
  (setf (cvv-item-value (get-named-item panel name)) value))

(defmethod (setf get-item-value) (value (panel widget-panel) name)
  (setf (cvv-item-value (get-named-item panel  name)) value))


(defmethod name ((panel widget-panel))
  (get-prop panel :name))

;;; ******************  CVV-ITEM CLASS  ******************
(defclass cvv-item
     (property-list-mixin)
     ((name :initform nil :initarg :name :accessor name)
      (widget :initform nil :initarg :widget :accessor widget)
      (value :initform :uninitialized :accessor value) ; is this really needed?
      (class :initform nil :initarg :class :accessor item-class)
      (panel :initform nil :initarg :panel :accessor panel)
      ))

(defclass cvv-indexed-items-item (cvv-item) ())

(defmethod tk-destroy-callback ((item cvv-item) widget &rest args)
  (ignore widget  args)
  ;;(format t "tk-destroy-callback ~a~%" widget)
  )
  
;;;(defmethod print-object  ((thing cvv-item) stream)
;;;  (format stream "#<~a ~a #X~x>"
;;;          (type-of thing )
;;;          (name thing) ;(widget thing) ;; (or (name thing) (cadr (choice thing)))
;;;          (%pointer thing )))

(defmethod print-object  ((thing cvv-item) stream)
  (print-unreadable-object (thing stream :type t :identity t)
    (princ (name thing))))


