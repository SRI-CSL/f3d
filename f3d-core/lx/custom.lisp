(defpackage :custom (:use :common-lisp)
	    (:export defgroup defcustom in-group set-variable))

(in-package :custom)

;;; This is file might be better handled using require/provide machinery.

;;; These definitions could come from the LISP-EXTENSIONS subsystem.

(defclass property-list-mixin ()
  ((property-list  :initform nil :initarg :property-list :accessor property-list)))

(defgeneric get-prop (object slot-name))
(defgeneric put-prop (object value slot-name))

(defmethod get-prop ((self property-list-mixin) indicator)
  ;(check-type indicator symbol)
  (when (slot-boundp self 'property-list)
    (getf (property-list self) indicator)))

(defmethod (setf get-prop) (value (self property-list-mixin) indicator)
  ;(check-type indicator symbol)
  (setf (getf (property-list self) indicator) value)
  value)

(defmacro removef (item list)	   
  `(setf ,list (remove ,item ,list)))

;;; ********************************************************8

(defun quote-list-elements (l)
  (loop for x in l collect `',x))

(defparameter *save-customization-forms* t)

(defclass customization (property-list-mixin)
    ((name :initarg :name :accessor name)))

(defclass custom-group (customization)
    ((subgroups :initform nil :accessor subgroups)
     (supergroups :initform nil :accessor supergroups)
     (variables :initform nil :accessor group-variables)))

(defmethod print-object ((o custom-group) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (format stream "~s ~a ~s ~s" (name o) (subgroups o) (group-variables o) (property-list o))))
  
(defvar *custom-group-ht* (make-hash-table))

(defun get-group (name &optional (missing-action :create))
  (or (gethash name *custom-group-ht*)
      (case missing-action
	(:create (setf (gethash name *custom-group-ht*)
		       (make-instance 'custom-group :name name)))
	(otherwise (error "group ~a not found" name)))))

(defun (setf get-group) (value name)
  (setf (gethash name *custom-group-ht*) value))

(defun declare-group (symbol doc &rest keyvals &key supergroups subgroups tag &allow-other-keys)
  (declare (ignorable tag))
  (let ((new-group (get-group symbol)))
    (setf (subgroups new-group) (mapcar #'get-group subgroups))
    (loop for subgroup-name in subgroups
	  do (pushnew new-group (supergroups (get-group subgroup-name))))
    ;; remove new group from preexisting supergroups
    (setf (supergroups new-group) nil)
    (loop for supergroup in (supergroups new-group)
	  do (removef new-group (subgroups supergroup)))
    ;; add this new group to parent group
    (loop for supergroup-name in supergroups
	  for supergroup = (get-group supergroup-name)
	  do (pushnew new-group (subgroups supergroup))
	     (pushnew supergroup (supergroups new-group)))
    (loop for (key val) on keyvals by #'cddr
	  do (case key
	       (:tag (setf (get-prop new-group :tag) val))))
    (when doc (setf (get-prop new-group :documentation) doc))
    ;;(run-hooks 'custom-define-hook)
    symbol))

(defparameter *defgroup-forms* nil)

(defmacro defgroup (symbol (&rest keyvals &key supergroups subgroups &allow-other-keys) &optional doc)
  (when *save-customization-forms*
    (push `(defgroup ,symbol  ,@keyvals ,@(when doc `(,doc))) *defgroup-forms*))
  `(declare-group ',symbol ,doc ,@(quote-list-elements keyvals)))

(defvar *current-group* nil)

(defmacro in-group (group)
  `(setq *current-group* (get-group ,group)))

;(make-load-form (get-group :image))
;(make-load-form (get-group :freedius))

(defmethod make-load-form ((o custom-group) &optional env)
  (declare (ignorable env))
  (let ((name (name o))
	(doc (get-prop o :documentation)))
    `(defgroup ,name 
	       (,@(when (get-prop o :tag) `(:tag ,(get-prop o :tag)))
		  ,@(when (supergroups o) `(:supergroups ,(mapcar #'name (supergroups o))))
		  ,@(when (subgroups o) `(:subgroups ,(mapcar #'name (subgroups o))))
		  )
	       ,@(and doc `(,doc))
	       )))

;;; ********************************************************

(defclass custom-variable (customization)
    ((default-value :initarg :default-value :accessor default-value) 
     (default-expr :initarg :default-expr :accessor default-expr)
     (groups :initform nil :accessor groups)
     ))

(defmethod print-object ((o custom-variable) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (format stream "~s ~s ~s" (name o) (default-expr o) (property-list o))))

#|  
(defmethod documentation ((o custom-variable))
  (cl::documentation (name o) 'variable))

(defmethod (setf documentation) (doc-string (o custom-variable))
  (setf (cl::documentation (name o) 'variable) doc-string))
|#

(defvar *custom-variable-ht* (make-hash-table))

(defvar *loading-runtime-options-p* nil)

;;; keys in keyvals may be repeated
(defun declare-variable (symbol default-expr doc &rest keyvals &key 
				group groups type &allow-other-keys)
  (declare (ignorable doc group groups))
  (when *loading-runtime-options-p* 
    (setf (symbol-value symbol) (eval default-expr))) ; override existing value
  (let* ((ht *custom-variable-ht*)
	 (entry (or (gethash symbol ht)
		    (setf (gethash symbol ht) 
			  (make-instance 'custom-variable :name symbol))))
	 (groups (append groups
			 (loop for (key val) on keyvals by #'cddr
			       when (eql key :group) 
				 collect val))))
    (when type (setf (get-prop entry :type) type))
    ;; remove from preexisting groups
    (loop for group in (groups entry)
	  do (removef entry (group-variables group)))
    (loop for group-name in groups
	  do (pushnew entry (group-variables (get-group group-name))))
    (setf (groups entry) (mapcar 'get-group groups))
    (setf (default-expr entry) default-expr)
    symbol))

(defparameter *defcustom-forms* nil)
				 
(defmacro defcustom (var val (&rest keyvals &key type group groups &allow-other-keys) &optional doc)
  (declare (ignorable type group groups))
  (when *save-customization-forms* ; this occurs at compile-time.
    (push `(defcustom ,var ,val ,doc ,@keyvals) *defcustom-forms*))
  `(progn (declare-variable ',var ',val ,doc ,@(quote-list-elements keyvals))
	  (defvar ,var ,val ,doc)))

(defmacro set-variable (var val)
  `(setq ,var ,val))

;(make-load-form (gethash 'img::*DEFAULT_TMP_FILE_IMAGE_DIR* *custom-variable-ht*))

(defmethod make-load-form ((o custom-variable) &optional env)
  (declare (ignorable env))
  (let* ((name (name o))
	 (doc (cl:documentation name 'variable))
	 (type (get-prop o :type)))
    `(defcustom ,name ,(default-expr o)
       (,@(and type `(:type ,type))
	  :groups ,(mapcar #'name (groups o))
	  )
       ,@(and doc `(,doc)))))

(defun dump-customization-forms (path forms &key (label "Customization Forms"))
  (with-open-file (st path :direction :output :if-exists :supersede)
    (format st "(IN-PACKAGE :CUSTOM)~%~%")
    (format st ";;; ~a~%~%" label)
    (loop for form in forms
	  do (pprint form st)
	     (terpri st))))

;;; This version uses the internal data structures
(defun dump-customization-forms2 (path custom-objects &key (label "Customization Forms"))
  (dump-customization-forms path
			    (sort-defcustom-forms (mapcar #'make-load-form custom-objects))
			    :label label))

(defun sort-defcustom-forms (forms)
  (flet ((var-lessp (f1 f2)
	   (destructuring-bind (ignore1 var1 . rest1) f1
	     (declare (ignorable ignore1 rest1))
	     (destructuring-bind (ignore2 var2 . rest2) f2
	       (declare (ignorable ignore2 rest2))
	       (string-lessp (format nil "~s" var1) (format nil "~s" var2))))))
    (sort (copy-list forms) #'var-lessp)))

(defun generate-runtime-options-file (path &optional forms)
  (let ((*package* (find-package :custom)))
    (unless forms (setq forms  (loop for var being the hash-values of *custom-variable-ht*
				     collect (make-load-form var))))
    (setq forms (sort-defcustom-forms forms))
    (with-open-file (st path :direction :output :if-exists :supersede)
      (format st "#|
This file is automatically generated from the FREEDIUS DEFCUSTOM definitions,
providing a reference definition of all custom variables and their values.
Custom-variable values are set in the following order:

   . Lisp source files
   . $FREEDIUS/lisp/runtime-options.lisp
   . $FREEDIUS_ARCH/lisp/runtime-options.lisp   (optional)
   . $HOME/freedius-init.lisp                   (optional)

Changes to DEFCUSTOM definitions should be made in $FREEDIUS_ARCH/lisp/runtime-options.lisp,
$HOME/freedius-init.lisp, or in the source file where the variable is defined, NOT IN THIS FILE.
Use SLIME meta-dot to access the source code definitions.
|#

")
      (format st "(IN-PACKAGE :CUSTOM)~%~%")
      (loop for form in forms
	    do (pprint form st)
	       (terpri st)))))

;;;(generate-runtime-options-file "$FREEDIUS/lisp/runtime-options.lisp")
    


#|  Examples

(progn (setq *defgroup-forms* nil *defcustom-forms* nil)
       (clrhash *custom-group-ht*)
       (clrhash *custom-variable-ht*))

(defgroup :essential-settings () "Settings essential for running FREEDIUS")

(defgroup :image () "FREEDIUS Image Customization Group")
 
(defgroup :image-io (:image) "FREEDIUS Image I/O Customization Group")

(defcustom img::*DEFAULT_TMP_FILE_IMAGE_DIR* "/tmp/CME_TMP_PIX"
  (:type string :group :image-io :group :essential-settings)
  "Default directory for creating temporary file images.")
  

(defgroup :freedius (:supergroups (:image :essential-settings))
  "FREEDIUS Main Customization Group")

(dump-customization-forms "/tmp/defgroup-forms.lisp" *defgroup-forms* )
(dump-customization-forms "/tmp/defcustom-forms.lisp" *defcustom-forms* )

(dump-customization-forms2 "/tmp/defgroup-forms.lisp"
			   (loop for val being the hash-values of *custom-group-ht* collect val))

(dump-customization-forms2 "/tmp/defcustom-forms.lisp" 
			   (loop for val being the hash-values of *custom-variable-ht* collect val))

|#
