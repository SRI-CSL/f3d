(IN-PACKAGE :lx)


#|
(maybe-compile-file-load "~/cp/lisp/lisp/struct-class.lisp")
|#

#|
HISTORY:

Tue Jan 19 1999:  Found bug w.r.t. the package of the struct.s name.
    Both Allegro and LCL defined the slot accessors current package, not the
    package of the struct name.

  From the Common Lisp HyperSpec docs for defstruct: 

    :conc-name is nil or no argument is supplied, then no prefix is used; then
    the names of the reader functions are the same as the slot names. If a
    non-nil prefix is given, the name of the reader function for each slot is
    constructed by concatenating that prefix and the name of the slot, and
    interning the resulting symbol in the package that is current at the time
    the defstruct form is expanded.


TODO: Try to make DEFSTRUCT-CLASS for STRUCTURE-OBJECT classes more friendly to redefinition.
      It is already friendly to redefining  STANDARD-OBJECT classes.
      Can only go so far with this:  It slots are incompatibly changed, all
      code for this class and all subclassses must be recompiled.
      Changes to :DEFAULT-INITARGS requires propagating the change to all subclasses.
      Alternatively, the :DEFAULT-INITARGS slot could be cleared, and (make-instance pcl::structure-class)
      would detect this and recompute the :DEFAULT-INITARGS list.

      Perhaps DEFSTRUCT-CLASS support should be rolled into DEFCLASS. I am not sure how to
      "wrap" functionality around a macro in a portable manner.  Does DEFADVICE work for macros?

(DEFADVICE )

|#

#|
LHQ Sun Dec 26 2004:  CMUCL the MOP, thus we can (almost) eliminate
the need for the defstruct-class macro in favor of defclass.  

Unfortunately, Allegro is not MOP compartible.  Allegro SUCKS!

CMUCL Examples:

(defclass x ()
    ((a :initform 'a :accessor x-a))
  (:metaclass structure-class))

(setq x1 (make-instance 'x))

(defclass y (x)
    ((b :initform 'b :accessor x-b))
  (:metaclass structure-class))

(setq y1 (make-instance 'y))


(slot-value y1 'b)

|#


(eval-when (load eval compile)
  (pushnew :hack-slot-names *features*)
)

(eval-when (load eval compile)

(defmacro defstruct-class-p (class-symbol)
  `(get ,class-symbol :defstruct-class-p))

(defun defstruct-class-package (class-symbol)
  (or (get class-symbol :defstruct-package) (symbol-package class-symbol)))


(defun make-struct-accessor-function-name (struct-class slot-name)
  (intern (format nil "~a-~a" (symbol-name struct-class) (symbol-name slot-name))
	  (defstruct-class-package struct-class)
	  ))

#+never
(defun make-struct-slot-qualified-name (class-name slot-name)
  (let* ((class (find-class class-name))
	 (slot-symbol (loop for slot in (clos::class-slots class)
			  for name = (slot-value slot 'pcl::name)
			    when (string= (symbol-name slot-name )  (symbol-name name ))
			    return name)))
    (unless slot-symbol
      (warn "WITH-CLASS-SLOTS cannot find slot ~a of class ~a~%" slot-name class-name))
    slot-symbol))

;;; MAKE-STRUCT-SLOT-QUALIFIED-NAME is needed to obtain the slot-symbol in the
;;; package associated with the class.  Perhaps this is not necessary since CLOS WITH-SLOTS
;;; does not handle getting the package right.  Should WITH-CLASS-SLOTS be any friendlier?
;;; What about multiple slots with the same name in different packages.  They are distinct in CLOS.
#+hack-slot-names
(defun make-struct-slot-qualified-name (class-name slot-name &optional (warn nil))
  (let* ((slot-symbol (loop for name in (get class-name :all-slot-names)
			    when (string= (symbol-name slot-name )  (symbol-name name ))
			      return name)))
    (unless slot-symbol
      (when warn
	(warn "WITH-CLASS-SLOTS cannot find slot ~a of class ~a~%" slot-name class-name)))
    (or slot-symbol slot-name)))

;;;(defun make-defstruct-class-descr (class-name superclass direct-default-initargs struct-creation-function defstruct-package)
;;;  (setf (defstruct-class-p class-name) t
;;;        (get class-name :superclass) superclass
;;;        (get class-name :direct-default-initargs) direct-default-initargs
;;;        (get class-name :struct-creation-function) struct-creation-function
;;;        (get class-name :defstruct-package) defstruct-package))


(defun make-defstruct-class-descr (class-name superclass direct-default-initargs struct-creation-function direct-slots defstruct-package)
  (setf (defstruct-class-p class-name) t
	(get class-name :superclass) superclass
	(get class-name :direct-default-initargs) direct-default-initargs
	(get class-name :struct-creation-function)
	(cons struct-creation-function
	      (append direct-slots
		      (and superclass
			   (cdr (get superclass :struct-creation-function)))))
	(get class-name :defstruct-package) defstruct-package))

(defun defflavor-to-defclass-slots-and-options (slots options)
  (let* (initable-slots
	 readable-slots
	 writable-slots
	 accessible-slots
	 default-initform
	 (slot-names (loop for slot-info in slots
			   collect (if (symbolp slot-info) slot-info (car slot-info))))
	 clos-slots
	 clos-options
	 )
    
    (loop for option-info in options
	  for option = (if (symbolp option-info) option-info (car option-info))
	  for option-args = (if (symbolp option-info)
				(if (member option '(:initable-slots
						     :settable-slots
						     :gettable-slots
						     :readable-slots
						     :writable-slots
						     :accessible-slots
						     ))
				    slot-names
				    nil)
				(cdr option-info))
	  do (case option
	       (:initable-slots
		(setq initable-slots (nconc option-args initable-slots)))
	       ((:readable-slots :gettable-slots)
		(setq readable-slots (nconc option-args readable-slots )))
	       ((:writable-slots :settable-slots)
		(setq writable-slots (nconc option-args writable-slots )))
	       ((:accessible-slots )
		(setq accessible-slots (nconc option-args accessible-slots )))
	       (:default-initform
		   (setq default-initform (list (car option-args))))
	       (otherwise (push option-info clos-options))
	       ))

    (setq clos-slots
	  (loop for slot-info in slots
		for slot-name = (if (symbolp slot-info) slot-info (car slot-info))
		for clos-slot-syntax =  (and (consp slot-info) (> (length slot-info) 2))
		for flavor-initform-p = (and (not clos-slot-syntax) (consp slot-info) (cdr slot-info))
		for flavor-default-initform = (and default-initform
						   (symbolp slot-info) )
		for slot-options = (and clos-slot-syntax (cdr slot-info) )
		collect `(,slot-name
			  ,@(and flavor-initform-p
				 `(:initform ,(cadr slot-info )))
			  ,@(and flavor-default-initform
				 `(:initform ,(car flavor-initform-p)))
			  ,@(and (member slot-name initable-slots )
				 (not (getf slot-options :initarg))
				 `(:initarg ,(make-keyword slot-name)))
			  ,@(and (member slot-name accessible-slots )
				 (not (getf slot-options :accessor ))
				 `(:accessor ,slot-name))
			  ,@(and (member slot-name readable-slots )
				 (not (getf slot-options :reader ))
				 `(:reader ,slot-name))
			  ,@(and (member slot-name writable-slots )
				 (not (getf slot-options :writer ))
				 `(:writer ,(intern (format nil "SET-~a" slot-name) (symbol-package slot-name))))
			  . ,slot-options )
		))
    (values clos-slots clos-options)
    
    ))

) ; end eval-when (load eval compile)




;;; perhaps defstruct-class should be an advise on defclass ??

(defmacro defstruct-class (class-name super-classes slot-defs &rest options)
  (expand-defstruct-class class-name super-classes slot-defs options))

(eval-when (load eval compile)

(defun expand-defstruct-class (class-name super-classes slot-defs options)
  (unless (boundp '*keyword-package*) (setq *keyword-package* (symbol-package :key)))

  ;; add support for defflavor style definitions
  (multiple-value-setq (slot-defs options)
    (defflavor-to-defclass-slots-and-options slot-defs options))
  
  (if (not (defstruct-class-p (car super-classes)))
      #-hack-slot-names
      `(defclass ,class-name ,super-classes ,slot-defs . ,options)
      #+hack-slot-names
      (let* ((slot-names (loop for slot in slot-defs
			       collect (if (symbolp slot)
					   slot
					   (car slot))))
	     ;; forward referenced classes are not handled correctly here.
	     (all-slot-names (append (loop for supcls in super-classes
					   append (get supcls :all-slot-names))
				     slot-names)))
	
	`(progn (eval-when (eval load compile)
		  (setf (get ',class-name :all-slot-names) ',all-slot-names))
	  (defclass ,class-name ,super-classes ,slot-defs . ,options)))
      
      (let* ((struct-package (defstruct-class-package class-name))
	     (accessor-package *package*)
	     (creation-function (intern (format nil "MAKE-~a-INTERNAL" class-name) struct-package))
	     (default-initarg-thunks
		 (loop for (key initform) on (cdr (assoc :default-initargs options)) by #'cddr
		       collect key
		       collect `(function (lambda () ,initform)) ; create a lexical closure.
		       ))
	     )
	#-sbcl (declare (special *package*))
	#+never
	(when options
	  (format t
		  ";;; Warning: DEFSTRUCT-CLASS cannot handle the following options: ~s~%"
		  options))

	(when (> (length super-classes) 1)
	  (error "def-struct-class does not allow multiple inheritance"))
	`(progn
	  (eval-when (eval load compile)
	    ;; eval-when needed here so that :struct-creation-function info of
	    ;; class hierarchy is known at compile time
	    (make-defstruct-class-descr ',class-name ',(and super-classes (car super-classes))
					(list . ,default-initarg-thunks)
					',creation-function
					',(loop for slot in slot-defs
						for slot-name = (if (consp slot) (car slot) slot)
						for slot-key = (intern (string slot-name) *keyword-package*)
						for initarg = (and (consp slot) (cdr slot) (getf (cdr slot) :initarg))
						for key = (or initarg slot-key)
						collect (cons key slot-key))
					(find-package ,(package-name *package*))))
	  (defstruct (,class-name (:constructor ,creation-function)
				  (:include ,(or (car super-classes) 'base-struct-class))
				  (:copier nil)
				  )
	    . ,(loop for slot in slot-defs
		     collect (if (symbolp slot)
				 slot
				 (if (null (cddr slot))
				     slot
				     (let ((initform (getf (cdr slot) :initform))
					   (type (getf (cdr slot) :type)))
				       (if type
					   (list (car slot) (or initform
								(case type
								  (double-float 0.0d0)
								  (single-float 0.0f0)
								  (fixnum 0)
								  (otherwise nil)))
						 :type type)
					   (list (car slot) initform)))))))
	  ,@(loop for slot in slot-defs
		  for accessor = (and (consp slot) (cddr slot) (getf (cdr slot) :accessor))
		  for slot-accessor = (intern (format nil "~a-~a" class-name (car slot)) accessor-package)
		  when (and accessor (not (eq accessor slot-accessor)))
		  collect
		  `(defmethod ,accessor ((struct ,class-name))
		    (,slot-accessor struct))
		  and collect
		  `(defmethod (setf ,accessor) (value (struct ,class-name))
		    (setf (,slot-accessor struct) value)))
	  (find-class ',class-name)
	  ))))

) ; end eval-when

(defun runtime-struct-slot-value (struct-class instance slot-name)
  (funcall (make-struct-accessor-function-name struct-class slot-name)
	   instance))

;;; Caution-- unlike CLOS slot access, access to struct-slots are inline struct accessors.
;;; Therefore, you should be careful to confine the use of WITH-STRUCT-SLOTS to
;;; as few different files as possible so that recompilation can be simplified.
;;; I thought that WITH-CLASS-SLOTS was incorrectly named because slot-accessors were called.
;;; The accessors that are called are (possibly inlined) functions, NOT METHODS, that
;;; directly read and write the slots.  These accessors are analogous to slot access in CLOS.
#-allegro-v9.0 
(defmacro with-class-slots (class-name slots instance &body body)
  (if (defstruct-class-p class-name)
      `(with-accessors 
	,(loop for slot in slots
	       collect
	       (if (symbolp slot)
		   (list slot (make-struct-accessor-function-name class-name slot))
		   (list (car slot) (make-struct-accessor-function-name class-name (cadr slot)))))
	,instance
	. ,body)
      #-hack-slot-names
      `(with-slots ,slots ,instance . ,body)
      #+hack-slot-names
      `(with-slots ,(loop for slot in slots
			  collect
			  (if (symbolp slot)
			      (list slot (make-struct-slot-qualified-name class-name slot))
			      (list (car slot) (make-struct-slot-qualified-name class-name (cadr slot)))))
	,instance . ,body)
      ))

#+allegro-v9.0
(defmacro with-class-slots (class-name slots instance &body body)
  `(with-slots ,slots ,instance
     ,@body))

(defmacro with-class-slot-values (class-name slots instance &body body)
  `(with-class-slots ,class-name ,slots ,instance
		      (let ,(loop for slot in slots
				  unless (symbolp slot) do (setq slot (car slot))
				    collect `(,slot ,slot))
			. ,body)))

(defmacro class-slot-value (class-name instance slot-name )
  (if (defstruct-class-p class-name)
      (if (and (consp slot-name) (eq (car slot-name) 'quote))
	  `(,(make-struct-accessor-function-name class-name (cadr slot-name))
	     ,instance)
	  `(runtime-struct-slot-value ',class-name ,instance ,slot-name))
      #-hack-slot-names `(slot-value ,instance ,slot-name)
      #+hack-slot-names `(slot-value ,instance ,(make-struct-slot-qualified-name class-name slot-name))
      ))

;;; ***********************   EXTENSIONS TO MAKE-INSTANCE MACHINERY  ***********************


(defun get-default-initfunctions (defstruct-class)
  (let* ((no-default-initargs 1)
	 (val (get defstruct-class :default-initargs no-default-initargs)))
    (if (eq val no-default-initargs)
	(setf (get defstruct-class :default-initargs)
	      (append (get defstruct-class :direct-default-initargs)
		      (let ((super (get defstruct-class :superclass)))
			(and super (get-default-initfunctions super)))))
	val)))
	      
;;; This allows MAKE-INSTANCE to be used with STRUCTURE-CLASS, which is not normally supported. 
;;; This should use some of the machinery in $CMUCL/src/pcl/ for fast initialization.
(defmethod make-instance ((class #+allegro structure-class 
				 #+cmu pcl::structure-class
				 #+sbcl sb-pcl::structure-class)
			  &rest supplied-initargs)
  (let* ((name (#+allegro class-name #+cmu pcl::class-name #+sbcl sb-pcl::class-name
			  class))
	 (creator-and-slots (get name :struct-creation-function))
	 (initargs (append supplied-initargs (get-default-initfunctions name))))
    (if creator-and-slots
	(let* ((make-fn (car creator-and-slots))
	       (slots (cdr creator-and-slots))
	       (filtered-initargs (loop with not-found = (list nil)
					for (key value) on initargs by #'cddr
					for slot-key = (cdr (assoc key slots))
					;;do (setq foo value)
					when slot-key
					  collect slot-key
					  and collect (if (eq (getf supplied-initargs key not-found) not-found)
							  (funcall value) ; value is really a "THUNK"
							  value) 
					))
	       (instance (apply make-fn filtered-initargs)))
	  (apply 'initialize-instance instance initargs)
	  instance)

	(if initargs
	    (error "Initargs not permitted for ~S creations" 'structure-class)
	    (ALLOCATE-INSTANCE class)
	    ))))


;;; *******************************  BASE-STRUCT-CLASS   *******************************

(defstruct (base-struct-class)
  )

(eval-when (load eval compile)
  (setf (get 'base-struct-class :defstruct-class-p) t)
  )

(allow-redefinition
 
;;;(defmethod print-object ((object base-struct-class) stream)
;;;  (if t ;*print-escape*
;;;      (format stream "#<~a #X~x>" (type-of object) (%pointer object))
;;;      (format stream "~a" (type-of object) )))

(defmethod print-object ((object base-struct-class) stream)
  (print-unreadable-object (object stream :type t :identity t)))

) ; end allow-redefinition

;;; base method
(defmethod initialize-instance ((object base-struct-class) &rest args)
  (declare (ignore args)))

(defmethod fasd-form-init-plist ((self base-struct-class))
  nil)

;;; ***************   FASD-FORM-INIT-PLIST-STRUCT    *****************

(defstruct-class fasd-form-init-plist-struct (base-struct-class) ())

(defmethod fasd-form ((self fasd-form-init-plist-struct))
  (let ((*fast-dumping-p* *fast-dumping-p2*))
    `(make-instance ',(type-of self)
       . ,(fasd-form-init-plist self))))

(defmethod fasd-form-p ((self fasd-form-init-plist-struct))
  t)

#+unused
(defmethod cached-fasd-form ((self fasd-form-init-plist-struct))
  (let ((*fast-dumping-p* *fast-dumping-p2*))
    `(make-cached-instance ',(type-of self)
			   . ,(fasd-form-init-plist self))))

;;; this name should be avoided
(defmethod copy ((self fasd-form-init-plist-struct))
  (let ((*fast-dumping-p2* nil))
    (eval (fasd-form self))))

;;; base method
(defmethod fasd-form-init-plist ((self fasd-form-init-plist-struct))
  nil)

;;; ***************   PROPERTY-LIST-STRUCT   *****************

(defstruct-class property-list-struct (fasd-form-init-plist-struct)
  ((property-list :initform nil :initarg :property-list :accessor property-list)))

(defmethod fasd-form-p ((self property-list-struct))
  t)

(defmethod get-prop ((object property-list-struct) indicator)
  (with-class-slots property-list-struct
      (property-list) object
      (getf property-list indicator)))

(defmethod (setf get-prop) (value (object property-list-struct) indicator)
  (with-class-slots property-list-struct
	(property-list) object
    (setf (getf property-list indicator) value)
    value))

(defmethod rem-prop ((object property-list-struct) indicator)  (with-class-slots property-list-struct
      (property-list) object
      (remf property-list indicator)))

(defmethod fasd-form-properties-list-do-not-dump ((self property-list-struct))
  nil)

(define-fasd-form-init-plist property-list-struct
    (fasd-form-mixin-expand-property-list
     (property-list self) (fasd-form-properties-list-do-not-dump self)))

#|
(setq pls (make-property-list-struct-internal :property-list '(:foo :bar)))
(setq pls2 (make-instance 'property-list-struct :property-list '(:foo :bar)))
(setq pls2 (make-instance 'property-list-struct))

(fasd-form pls)
(copy pls)
(fasd-form pls2)
(type-of pls2)
|#

#+hack-slot-names
(eval-when (load eval compile)
  (setf *features* (remove :hack-slot-names *features*))
)
 
