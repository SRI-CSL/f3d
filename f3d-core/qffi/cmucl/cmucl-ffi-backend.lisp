(in-package :qffi)

#+never ; no longer used
(eval-when (load eval compile)
  (pushnew :cmucl-pointer-types *features*)
)

(deftype foreign-pointer (&optional struct-name)
  (declare (ignore struct-name))
  `alien:alien)


(defparameter *foreign-address-type* 'c-call:unsigned-long)
;;(defparameter *foreign-pointer-address-fn* 'foreign-pointer-address)

(defparameter *foreign-pointer-type* '(* t))
(defparameter *foreign-pointer-address-fn* nil)
(defparameter *foreign-array-address-type* 'c-call:unsigned-long)


(defun qffi-to-cmucl-type (type &optional return-type)
  (case type 
    (:lisp (values *foreign-address-type* 
		   (if return-type
		       '(kernel:make-lisp-obj)
		       'kernel:get-lisp-obj-address)))
    (:null 'c-call:void)
    (:fixnum 'c-call:int)
    (:char 'c-call:char)
    (:short 'c-call:short)
    (:int 'c-call:int)
    (:long 'c-call:long)
    (:unsigned-char 'c-call:unsigned-char)
    (:unsigned-short 'c-call:unsigned-short)
    (:unsigned-int 'c-call:unsigned-int)
    (:unsigned-long 'c-call:unsigned-long)
    (:double-float 'double-float)
    (:single-float 'single-float)
    (:pointer (values *foreign-pointer-type* *foreign-pointer-address-fn*))
    ((:array :array-or-null :simple-array-or-null)
     (values *foreign-array-address-type* 'inline-array-data-address))
    (:simple-array (values *foreign-array-address-type* 'inline-simple-array-data-address))    
    ((:simple-string :string ) 'c-call:c-string)
    (otherwise
     (cond ;;((keywordp type) type)
       ((symbolp type)
	(let ((expansion (expand-foreign-type type)))
	  (if (not (equal type expansion))
	      (qffi-to-cmucl-type expansion)
	      (error "QFFI-TO-CMUCL-TYPE Unknown qffi foreign type -> ~S" type))))
       ((consp type)
	(case (car type)
	  (:pointer 
	   (cond ((eq (cadr type) :char)
		  'c-call:c-string)
		 ((and (symbolp (cadr type)) (not (keywordp (cadr type))))
		  ;; FIXME? 
		  (if return-type 
		      `(* (alien:struct ,(cadr type)))
		      ;(values *foreign-pointer-type* *foreign-pointer-address-fn*)
		      (values `(* (alien:struct ,(cadr type)))  *foreign-pointer-address-fn*)
		      ))
		 ((or (eq (cadr type) :void) *foreign-pointer-address-fn*)
		  (values *foreign-pointer-type* *foreign-pointer-address-fn*))
		 (t `(* ,(qffi-to-cmucl-type (cadr type))))))
	  ((:array :simple-array :simple-array-or-null :array-or-null)
	   (qffi-to-cmucl-type (car type)))))
       (t (error "QFFI-TO-ACL-TYPE Unknown qffi foreign type -> ~S" type))))))

(defun convert-to-language (sym lang)
  (let ((string (string sym)))
    (case lang
      (:c
       (when (symbolp sym)
	 (unless (loop for i from 0 below (length string)
		       thereis (lower-case-p (aref string i)))
	   (setq string (string-downcase string)))
         (dotimes (i (length string))
           (when (char= (elt string i) #\-)
             (setf (elt string i) #\_)))))
      )
    string))


;;; ********************************  DEF-FOREIGN-FUNCTION  ********************************

;;; Changed Mon Sep  3 2007 to allow config.lisp to customize

;;(defvar config::def-foreign-function-without-gcing t)
(defvar config::def-foreign-function-without-gcing nil)
(defvar config::def-foreign-function-without-interrupts nil)
;;;
;;; Error occurs in simple-string declarations within CMUCL
(defvar config::def-foreign-function-arg-checking-default t)

(defvar config::def-foreign-function-arg-checking-decls
  '(ext:optimize-interface (safety 1) (speed 3))
  ;'(ext:optimize-interface (safety 3) (speed 2))
  )


(defun cmucl-expand-def-foreign-function (parser-fn args)
  ;;(format t "qffi expand-def-foreign-function~%")
  (multiple-value-bind (kw-opts lisp-fn-name c-name qffi-return-type arglist arg-decls vars 
				qffi-c-types must-wrap)
      (funcall parser-fn args)
    (flet ((get-kw-opt (name default)
	     (let ((entry (assoc name kw-opts)))
	       (if entry (cadr entry) default))))
      (let* ((arg-checking (get-kw-opt :arg-checking 
				       config::def-foreign-function-arg-checking-default))
	     (without-interrupts (get-kw-opt :without-interrupts 
					     config::def-foreign-function-without-interrupts ))
	     (without-gcing (get-kw-opt :without-gcing 
					config::def-foreign-function-without-gcing)))
	(multiple-value-bind (cmucl-return-type retform) (qffi-to-cmucl-type qffi-return-type t)
	  (loop with convert-fns-exist
		for var in vars
		for qffi-type in qffi-c-types
		with (cmucl-c-type convert-fn)
		do (multiple-value-setq (cmucl-c-type convert-fn) (qffi-to-cmucl-type qffi-type))
		collect cmucl-c-type into cmucl-c-types
		collect convert-fn into convert-fns
		when convert-fn do (setq convert-fns-exist t)
		  collect (if convert-fn (list convert-fn var) var) into argforms
		collect (list var cmucl-c-type) into c-arg-names-types
		finally
	     (return  
	       (if (and (null must-wrap)
			(null convert-fns-exist)
			(null retform)
			(not (and arg-checking arg-decls))
			(null without-interrupts)
			(null without-gcing))
		   `(progn
		      (declaim (optimize (extensions:inhibit-warnings 3)))
		      (alien:def-alien-routine (,c-name ,lisp-fn-name) ,cmucl-return-type
			. ,c-arg-names-types))
		   (let ((ff-name (intern (format nil "~a%internal" lisp-fn-name) (symbol-package lisp-fn-name))))
		     `(progn
			(declaim (inline ,ff-name))
			(declaim (optimize (extensions:inhibit-warnings 3)))
			(alien:def-alien-routine (,c-name ,ff-name) ,cmucl-return-type
			  . ,c-arg-names-types)
	    
			(defun ,lisp-fn-name ,arglist
			  ,@(and arg-checking arg-decls
				 `((declare ,config::def-foreign-function-arg-checking-decls  .,arg-decls))
				 )
			  ,(let ((body `(,ff-name . ,argforms)))
				(when retform (setq body `(,@retform ,body)))
				(when without-gcing
				  (setq body `(lisp::without-gcing  ,body)))
				(when without-interrupts
				  (setq body `(lisp::without-interrupts  ,body)))
				body))
			',lisp-fn-name))))))))))

;;; ****************************  DEF-FOREIGN-CALLABLE  ****************************

;;; Using Helmut Eller Callbacks 

;;; This might be needed for restarting a disksave.
(defvar *foreign-callable-list* nil)

(defun register-foreign-callback (callback-fn trampoline-address-symbol)
  (pushnew (list callback-fn trampoline-address-symbol) *foreign-callable-list* :test #'equal)
  (setf (foreign-variable-value trampoline-address-symbol 'c-call:unsigned-long)
	(alien::sap-int (alien::symbol-trampoline callback-fn))))

;;; Might be needed for restarting disksave disksave.

(defun register-all-callbacks (&optional verbose)
  (loop for (callback-fn trampoline-address-symbol) in *foreign-callable-list*
        when verbose
          do (format t ";;; registering alien callback ~a~%" callback-fn)
        do (register-foreign-callback callback-fn trampoline-address-symbol))
           
  (format t "; REGISTER-ALL-FOREIGN-CALLBACKS ~a callbacks registered ~%"
          (length *foreign-callable-list*)))


(defun qffi-to-cmucl-callback-return-type (qffi-type)
  (if (null qffi-type) 
      'c-call::unsigned-int ;; This is not quite right, but void doesn't work on the 11/2004 snapshot of cmucl
;;      'c-call::void
      (qffi-to-cmucl-type qffi-type t)))

(defun cmucl-expand-def-foreign-callable (parser-fn args)
  (multiple-value-bind (name c-name ret-type c-args-and-qffi-types body)
      (funcall parser-fn args)
    (let ((c-args-and-cmucl-types 
	   (loop for (var qffi-type) in c-args-and-qffi-types
		 collect `(,var ,(qffi-to-cmucl-type qffi-type t)))))
      `(progn (alien:def-callback  ; Helmut Eller's callback machinery
		  ,name (,(qffi-to-cmucl-callback-return-type ret-type) . ,c-args-and-cmucl-types)
		.,body)
	      ,@(when c-name
		      `((register-foreign-callback ',name ,(format nil "~a_callback_address" c-name))))))))

;;; ****************************  DEF-FOREIGN-STRUCT  ****************************

;;; CHANGE NEXT LINE FOR INLINING OF FOREIGN STRUCT ACCESSORS
(defparameter *inline-foreign-struct-accessors* nil)

(defun build-slot-accessors (struct-name slot-name)
  (let* ((pkg (symbol-package struct-name))
	 (getter (intern (format nil "~a-~a" struct-name slot-name) pkg)))
    `(locally
      (declare (ext:optimize-interface (speed 3) (safety 0)))
      (declare (optimize (speed 3) (safety 0)))
      (declare (optimize (extensions:inhibit-warnings 3)))
      ,(when *inline-foreign-struct-accessors*
	     `(declaim (inline ,getter (setf ,getter))))
      (defun ,getter (obj)
	;;(declare (ext:optimize-interface (speed 3) (safety 0)))
	;;(declare (optimize (speed 3) (safety 0)))
	(declare (type (alien:alien (* ,struct-name)) obj))
	(alien:slot obj ',slot-name))
      (defun (setf ,getter) (val obj)
	;;(declare (ext:optimize-interface (speed 3) (safety 0)))
	;;(declare (optimize (speed 3) (safety 0)))
	(declare (type (alien:alien (* ,struct-name)) obj))
	(setf (alien:slot obj ',slot-name) val)))))


;;;(defun foreign-type-struct-slot-array-index-decls (slot-name slot-type package)
;;;  (and (consp slot-type)
;;;       (member (car slot-type) '(:array :simple-array :simple-array-or-null :array-or-null))
;;;       (loop for bound in (caddr slot-type)
;;;             collect `((integer 0 ,bound) ,slot-name))))
	
(defun build-slot-accessors (struct-name slot-name slot-type)
  (let* ((pkg (symbol-package struct-name))
	 (getter (intern (format nil "~a-~a" struct-name slot-name) pkg)))
    (multiple-value-bind (indices index-decls) 
	(foreign-type-struct-slot-array-index-info slot-name slot-type pkg)
      (let* ((ref-form0 `(alien:slot obj ',slot-name))
	     (ref-form (if indices `(alien::deref ,ref-form0 .,indices) ref-form0)))
	`(locally
	  ;(declare (ext:optimize-interface (speed 3) (safety 0)))
	  ;(declare (optimize (speed 3) (safety 0)))
	  (declare (optimize (extensions:inhibit-warnings 3)))
	  ,(when *inline-foreign-struct-accessors*
		 `(declaim (inline ,getter (setf ,getter))))
	  (defun ,getter (obj .,indices)
	    (declare (type (alien:alien (* ,struct-name)) obj))
	    ,@(when indices `((declare .,index-decls)))
	    ,ref-form)
	  (defun (setf ,getter) (val obj .,indices)
	    (declare (type (alien:alien (* ,struct-name)) obj))
	    ,@(when indices `((declare .,index-decls)))
	    (setf ,ref-form val)))))))


(defun qffi-to-cmucl-struct-slot-type (qffi-type)
  (if (consp qffi-type)
      (case (car qffi-type)
	((:array :simple-array :simple-array-or-null :array-or-null)
	 `(array ,(qffi-to-cmucl-type (cadr qffi-type)) .,(caddr qffi-type))
	 )
	(otherwise (qffi-to-cmucl-type qffi-type)))

      (qffi-to-cmucl-type qffi-type)))
	

;;; Restrict to the form (DEF-FOREIGN-STRUCT name (slotdef ...))

(defun cmucl-expand-def-foreign-struct (parser-fn args)
  (multiple-value-bind (name slot-names-and-qffi-types) (funcall parser-fn args )
    (let ((creator `(defun ,(intern (format nil "MAKE-~a" name) (symbol-package name)) ()
		     (alien:make-alien (alien:struct ,name)))))
      (loop for (slot-name qffi-type) in slot-names-and-qffi-types
	    collect `(,slot-name ,(qffi-to-cmucl-struct-slot-type qffi-type)) 
	      into slot-names-and-cmucl-types
	    collect (build-slot-accessors name slot-name qffi-type) into accessors
	    finally 
	 (return `(progn 
		   (eval-when (compile load eval)
		   (alien::def-alien-type ,name (alien:struct ,name .,slot-names-and-cmucl-types)))
		   ,creator
		   ,@accessors
		   ;;(make-foreign-synonym-type ',name)
		   ))))))


(defmacro def-foreign-function (&rest args)
  (cmucl-expand-def-foreign-function #'parse-lcl-def-foreign-function args))

(defmacro def-foreign-callable (&rest args)
  (cmucl-expand-def-foreign-callable #'parse-lcl-def-foreign-callable args))

(defmacro def-foreign-struct (&rest args)
  (cmucl-expand-def-foreign-struct #'parse-lcl-def-foreign-struct args))

(def-foreign-synonym-type dummy-pointer :unsigned-long)

#+never  ;;; *def-foreign-function-list* and *def-foreign-callable-list* no longer needed

(progn
(defparameter *def-foreign-function-list* nil)

(defmacro def-foreign-function (&rest args)
  (let ((expansion (cmucl-expand-def-foreign-function #'parse-lcl-def-foreign-function args)))
    (push (list (caar args) expansion) *def-foreign-function-list*)
    expansion))


(defparameter *def-foreign-callable-list* nil)

(defmacro def-foreign-callable (&rest args)
  (let ((expansion (cmucl-expand-def-foreign-callable #'parse-lcl-def-foreign-callable args)))
    (push (list (caar args) expansion) *def-foreign-callable-list*)
    expansion))

(defun dump-foreign-functions (path)
  (let ((sorted-defs (sort (copy-list *def-foreign-function-list*) #'string< :key #'car))
	;;(*package* (find-package :qffi))
	)
    (with-open-file (st path :direction :output :if-exists :supersede)
      (format st "(in-package ~s)~%" (package-name *package*))
      (loop for (name expansion) in sorted-defs
	    do (pprint expansion st)
	       (terpri st)))))


(defun dump-foreign-callables (path)
  (let ((sorted-defs (sort (copy-list *def-foreign-callable-list*) #'string< :key #'car))
	(*package* (find-package :qffi)))
    (with-open-file (st path :direction :output :if-exists :supersede)
      (format st "(in-package ~s)~%" (package-name *package*))
      (loop for (name expansion) in sorted-defs
	    do (pprint expansion st)
	       (terpri st)))))

) ; end progn

#|
(list (length *def-foreign-function-list*) (length *def-foreign-callable-list*))
(dump-foreign-functions "/tmp/freedius2-foreign-functions.lisp" )
(dump-foreign-callables "/tmp/freedius2-foreign-callables.lisp")
|#






#+cmu
(progn

(fwrappers:define-fwrapper load-object-file/reload (file &optional (recordp t))
  (loop :for (sap . lib-path) :in sys::*global-table*
        :when (equal lib-path file)
        :do (sys::dlclose sap) (format t "dlclose ~a~%" file))
  (fwrappers:call-next-function file recordp))

(fwrappers:fwrap 'sys::load-object-file #'load-object-file/reload)


)


