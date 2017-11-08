(in-package :qffi)

#-acl-foreign-pointers
(progn

(deftype foreign-pointer (&optional struct-name)
  (declare (ignore struct-name))
  `integer)


(defun make-pointer-type-spec (&optional type return-type)
  (declare (ignorable type))
  (if return-type
      ':unsigned-long
      '(* :void))
  )

)

#+acl-foreign-pointers
(progn

(deftype foreign-pointer (&optional struct-name)
  (declare (ignore struct-name))
  `EXCL::FOREIGN-POINTER)

;;;
;;; These next two are bad for the new ACL API...
;;;
(defun make-pointer-type-spec (&optional type return-type)
  (declare (ignorable return-type))
  (if return-type
      (values `((* ,(or type :void)))
	      `((lambda (addr) (ff::make-foreign-pointer :foreign-address addr :foreign-type 
							 '(* ,(or type :void))))))
      `(* ,(or type :void))))

(defun make-pointer-type-spec (&optional type return-type)
  (declare (ignorable return-type))
  (if return-type
      (values (if type 
		  ;`((ff::foreign-pointer ,type))
		  `((* ,type))
		  `((* :void)))
	      `((lambda (addr) (ff::make-foreign-pointer :foreign-address addr :foreign-type 
							 ,(if type
							      `',type
							      `(* :void))))))
      (if type 
	  ;`(ff::foreign-pointer ,type)
	  `(* ,type)
	  `(* :void))))


;;;
;;; I think :pointer can always be converted to '* in Allegro:
;;;
(defun make-pointer-type-spec (&optional type-in return-type)
  (declare (ignorable return-type))
  (let ((type (if (and (consp type-in)
                       (eq (car type-in) :pointer))
                  (cons '* (cdr type-in))
                  type-in)))
    (if return-type
        (values (if type 
                    `((* ,type))
                    `((* :void)))
                `((lambda (addr)
                    (ff::make-foreign-pointer
                     :foreign-address addr
                     :foreign-type ,(if type
                                        `',type
                                        `(* :void))))))
        (if type 
                                        ;`(ff::foreign-pointer ,type)
            `(* ,type)
            `(* :void)))))

)


(defun foreign-string-value (foreign-pointer)
  (ff:char*-to-string foreign-pointer))


;; This is a macro to reduce some of the run-time overhead 
;;      if arg happens to be ok
(defmacro force-double-float (x)
  (if (numberp x)
    (coerce x 'double-float)
    `(let ((x ,x))
      (if (typep x 'double-float)
	x
	(coerce x 'double-float)))))

(defmacro convert-array-or-null (x)
  `(if (null ,x) 0 ,x))


;;;(defun convert-array-or-null (x)
;;;  (if (null x) 0 x))


(defun qffi-to-acl-type (type &optional return-type)
  (case type
    (:double-float (if return-type :double (values :double 'force-double-float)))
    (:single-float :float)
    (:pointer (make-pointer-type-spec nil return-type) )
    ((:array :simple-array) '(* :void))
    ((:simple-array-or-null :array-or-null)  (values '(* :void) 'convert-array-or-null))
    ((:simple-string :string ) 
     (if return-type  
	 (values (make-pointer-type-spec nil t) '(foreign-string-value))
	 '(* :char)))
    ;; (:char :byte)
    (:char :char)
    ((:int :fixnum) :fixnum)
					; ((:int :fixnum) :int)
    (otherwise
     (cond ((keywordp type) 
	    (if (and (eq type :null) return-type)
		:void
		type))
	   ((symbolp type)
	    (let ((expansion (expand-foreign-type type)))
	      (if (not (equal type expansion))
		  (qffi-to-acl-type expansion)
		  (error "QFFI-TO-ACL-TYPE Unknown qffi foreign type -> ~S" type))))
	   ((consp type)
	    (case (car type)
	      (:pointer 
	       (if (member (cadr type) '(:char :character))
		   `(* ,(cadr type))
		   (make-pointer-type-spec (cadr type) return-type)))
	      ((:array :simple-array) '(* :void))
	      ((:simple-array-or-null :array-or-null) (values '(* :void) 'convert-array-or-null))
	      (otherwise 
	       (if (gethash (car type) *user-defined-foreign-types*)
		   '(* :void)
		   (error "QFFI-TO-ACL-TYPE Unknown qffi foreign type -> ~S" type)))))
	   (t (error "QFFI-TO-ACL-TYPE Unknown qffi foreign type -> ~S" type))))))

;;;
;;; try to encapsulate the foreign symbol conversion -- depends on the platform.
;;;
(defun maybe-convert-foreign-name (name lang)
  #+macosx name
  #-macosx (ff:convert-to-lang name :language lang)
)

(defun convert-to-language (sym lang)
  (let ((string (string sym)))
    (case lang
      (:c
       (when (symbolp sym)
         (setq string (string-downcase string))
         (dotimes (i (length string))
           (when (char= (elt string i) #\-)
             (setf (elt string i) #\_)))
	 )
       )
      )
    string))

;;; ********************************  DEF-FOREIGN-FUNCTION  ********************************

(defparameter *def-foreign-function-without-gcing* nil)
(defparameter *def-foreign-function-without-interrupts* nil)

(defparameter *def-foreign-function-arg-checking-default* nil)
;;(defparameter *def-foreign-function-arg-checking-default* t)

;;; This will optimize the FFI call into the wrapper function.
(defparameter *def-foreign-function-call-direct-default* t)

(defun acl-expand-def-foreign-function (parser-fn args)
  ;;(format t "qffi expand-def-foreign-function~%")
  (multiple-value-bind (kw-opts lisp-fn-name c-name qffi-return-type arglist arg-decls vars 
				qffi-c-types must-wrap)
      (funcall parser-fn args)
    (let* ((arg-chk (assoc :arg-checking kw-opts))
	   (arg-checking (if arg-chk (cadr arg-chk) *def-foreign-function-arg-checking-default*))
	   (arg-call-direct (cadr (assoc :call-direct kw-opts)))
	   (any-simple-string-arg (loop for type in qffi-c-types
					thereis (eq type :simple-string)))
	   (call-direct (and (not any-simple-string-arg)
			     (if arg-call-direct 
				 (cdr arg-call-direct) 
				 *def-foreign-function-call-direct-default*)))
	   (must-wrap (or must-wrap call-direct)))
      (multiple-value-bind (acl-return-type retform) (qffi-to-acl-type qffi-return-type t)
	(loop with convert-fns-exist
	      with language = :c
	      for var in vars
	      for qffi-type in qffi-c-types
	      with (acl-c-type convert-fn)
	      do (multiple-value-setq (acl-c-type convert-fn) (qffi-to-acl-type qffi-type))
		 ;;collect acl-c-type into acl-c-types
		 ;;collect convert-fn into convert-fns
	      when convert-fn 
		do (setq convert-fns-exist t)
	      collect (if convert-fn (list convert-fn var) var) into argforms
	      collect (list var acl-c-type) into c-arg-names-types
	      finally
	   (return  
	     `(progn
	       #+never
	       (eval-when (compile load eval)
		 (record-pointer-accessor ',lisp-fn-name nil ',(qffi-to-lisp-type qffi-return-type) t))
	     		       
	       ,@(if (and (null convert-fns-exist) 
			  (null must-wrap)
			  (null retform))
		     `((ff:def-foreign-call (,lisp-fn-name ,(maybe-convert-foreign-name c-name language))
			   ,c-arg-names-types
			 :returning ,acl-return-type 
			 :arg-checking ,arg-checking
			 :call-direct ,call-direct
			 :strings-convert  t
			 ))

		     (let ((ff-name (intern (format nil "~a%internal" lisp-fn-name) 
					    (symbol-package lisp-fn-name))))
			   
		       `((ff:def-foreign-call (,ff-name  ,(maybe-convert-foreign-name c-name language))
			     ,c-arg-names-types
			   :returning ,acl-return-type
			   :arg-checking ,arg-checking
			   :call-direct ,call-direct
			   :strings-convert t
			   )
	      
			 (defun ,lisp-fn-name ,arglist
			   ,@(and arg-checking arg-decls
				  `((declare .,arg-decls)))
			   ,(let ((body `(,ff-name . ,argforms)))
				 (when retform (setq body `(,@retform ,body)))
				 body)))))

	       ',lisp-fn-name)))))))


(defmacro store-callback-value-in-place (value ctype)
  (if ctype
      `(coerce ,value ,ctype)
      0)
  )

;;;(defun qffi-to-acl-callback-return-type (qffi-type)
;;;  (if (null qffi-type) 
;;;      'integer
;;;      (qffi-to-acl-type qffi-type t))))

(defun qffi-to-lisp-callback-return-type (qffi-type)
  (if (null qffi-type)
      'integer
      (let ((lisp-type (qffi-to-lisp-type qffi-type t)))
       lisp-type)))


;;;(defun qffi-to-acl-callback-type (qffi-type)
;;;  (let ((acl-type  (qffi-to-acl-type qffi-type t)))
;;;    (if (eq acl-type :int) 
;;;        :fixnum
;;;        acl-type)))

(defun qffi-to-acl-callback-type (qffi-type)
  (if (eq qffi-type :simple-string)
      (values '(* :char) '(EXCL::REAL-NATIVE-TO-STRING))
      (let ((acl-type  (qffi-to-acl-type qffi-type nil)))
	(if (eq acl-type :int) 
	    :fixnum
	    acl-type))))

;;;(defun qffi-to-acl-callback-return-type (qffi-type)
;;;  (let ((acl-type  (qffi-to-acl-type qffi-type t)))
;;;    (if (eq acl-type :int) 
;;;        :fixnum
;;;        acl-type)))

;(trace qffi-to-acl-callback-type)
(defun acl-expand-def-foreign-callable (parser-fn args)
  (multiple-value-bind (name c-name qffi-ret-type c-args-and-qffi-types body)
      (funcall parser-fn args)
    (loop for (var qffi-type) in c-args-and-qffi-types
	  with (acl-type convert-fn) 
	  do (multiple-value-setq (acl-type convert-fn) (qffi-to-acl-callback-type qffi-type))
	  collect `(,var ,acl-type) into c-args-and-acl-types
	  when convert-fn collect `(,var (,@convert-fn ,var)) into lisp-let-bindings
	    finally
	 (return `(progn 
		   (ff:defun-foreign-callable
		       ,name ,c-args-and-acl-types
		     (store-callback-value-in-place
		      (let ,lisp-let-bindings ,@body)
					;',(qffi-to-acl-callback-return-type)
		      ',(qffi-to-lisp-callback-return-type qffi-ret-type)))
		   ,@(when c-name `((register-callback '(,name ,c-name))))
		   ',name)))))

;;; ***************************  DEF-FOREIGN-STRUCT  ***************************


(defun qffi-to-acl-struct-slot-type (qffi-type)
  (if (consp qffi-type)
      (case (car qffi-type)
	((:array :simple-array :simple-array-or-null :array-or-null)
	 `(:array ,(qffi-to-acl-type (cadr qffi-type)) .,(caddr qffi-type))
	 )
	(otherwise (qffi-to-acl-type qffi-type)))

      (let ((acl-type  (qffi-to-acl-type qffi-type)))
	(if(eq acl-type :fixnum) 
	   :int
	   acl-type))))
	
(defun build-slot-accessors (struct-name slot-name slot-type)
  (let* ((pkg (symbol-package struct-name))
         (svar (intern "X" pkg))
         (val-var (intern "VALUE" pkg))
         (getter (intern (format nil "~a-~a" struct-name slot-name) pkg))
	 (allocation nil) ; SLOW - ff:fslot-value-typed is not inlined with this
	; (allocation :c) ; this seems to work, but might not be safe
	; (allocation :foreign) ; this causes seg-faults
	 )
    (multiple-value-bind (indices index-decls) 
        (foreign-type-struct-slot-array-index-info slot-name slot-type pkg)
      (let* ((ref-form `(ff:fslot-value-typed ',struct-name ,allocation ,svar ',slot-name .,indices)))
        `(locally
          ;(declare (optimize (speed 3) (safety 1)))
          ;;,(when *inline-foreign-struct-accessors* `(declaim (inline ,getter (setf ,getter))))
          (defun ,getter (,svar .,indices)
            ;(declare (optimize (speed 3) (safety 1)))
	    ,@(when indices `((declare .,index-decls)))
            ,ref-form)
          (defun (setf ,getter) (,val-var ,svar .,indices) 
            ;(declare (optimize (speed 3) (safety 1)))
	    ,@(when indices `((declare .,index-decls)))
            (setf ,ref-form ,val-var)))))))
	

(defun acl-expand-def-foreign-struct (parser-fn args)
  (multiple-value-bind (name slot-names-and-qffi-types) (funcall parser-fn args )
    (let ((creator `(defun ,(intern (format nil "MAKE-~a" name) (symbol-package name)) ()
		     (ff:allocate-fobject ',name))))
      (loop for (slot-name qffi-type) in slot-names-and-qffi-types
	    for acl-slot-type = (qffi-to-acl-struct-slot-type qffi-type)
	    collect `(,slot-name ,acl-slot-type) into slot-names-and-acl-types
	    collect (build-slot-accessors name slot-name qffi-type) into accessors
	    finally 
	 (return `(eval-when (compile load eval) ; is this eval-when needed?
		   (ff:def-foreign-type ,name 
		       (:struct ,@slot-names-and-acl-types))
		   ,creator
		   ,@accessors
		   ;;(define-foreign-synonym-type ',name ???)
		   ))))))




(defmacro def-foreign-function (&rest args)
  (acl-expand-def-foreign-function #'parse-lcl-def-foreign-function args))

(defmacro def-foreign-callable (&rest args)
  (acl-expand-def-foreign-callable #'parse-lcl-def-foreign-callable args))

(defmacro def-foreign-struct (&rest args)
  (acl-expand-def-foreign-struct #'parse-lcl-def-foreign-struct args))

(def-foreign-synonym-type dummy-pointer :unsigned-long)


#|

(defun FREEDIUS-PREFIX (x) (format nil "FREEDIUS_~a" x))

(def-foreign-function (array_image_size_limit (:name (freedius-prefix "array_image_size_limit")))
    (size :int))

(expand-foreign-type 'dummy-pointer)
|#
