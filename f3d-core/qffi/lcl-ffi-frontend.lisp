(in-package :qffi)


;;; LCL type specific
(defmacro def-foreign-synonym-type (name lcl-type)
  (def-foreign-synonym-type-expander name (lcl-to-qffi-type lcl-type)))


;;; alternative it :int which causes conversion and type-checking costs.
;;(defparameter *lcl-int-qffi-type* :fixnum)
(defparameter *lcl-int-qffi-type* :int)

(defun lcl-to-qffi-type (lcl-arg-type)
  (let ((type lcl-arg-type))
    (or (case type
	  (:null :null)			; only used to indicate value returned  ???????????????
	  (:lisp :lisp)
	  (:int *lcl-int-qffi-type*)
	  ((:char :unsigned-char :short :unsigned-short :unsigned-int :long :unsigned-long)
	   type)
	  ((:single-float :double-float :simple-string :pointer 
			  :array :simple-array :array-or-null :simple-array-or-null)
	   type)
	  (:fixnum :fixnum)
	  (:signed-8bit :char)
	  (:character :char)
	  (:signed-16bit :short)
	  (:signed-32bit :int)
	  (:signed-64bit :long)		
	  (:unsigned-8bit :unsigned-char)
	  (:unsigned-16bit :unsigned-short)
	  (:unsigned-32bit :unsigned-int)
	  (:unsigned-64bit :unsigned-long) 
	  (otherwise 
	   (if (consp type)
	       (case (car type)
		 ((:array :simple-array :array-or-null :simple-array-or-null)
		  `(,(car type) ,(lcl-to-qffi-type (cadr type)) ., (cddr type)))
		 ((* :pointer)
		  (if (keywordp (cadr type))
		      `(:pointer ,(lcl-to-qffi-type (cadr type)))
		      `(:pointer ,(cadr type))))
		 (otherwise (error "LCL-TO-QFFI-TYPE: Unknown lcl foreign type: ~s" type)))
	       ;; FIXME -- do we really want expand-foreign-type here?
	       (let ((expansion (expand-foreign-type type)))
		 (when (not (equal expansion type))
		   expansion)))))
	(error "LCL-TO-QFFI-TYPE: Unknown lcl foreign type: ~s" type))))



;;; alternative is :int but that that is more expensive for arg-checking
;;(defparameter *default-arg-type* :fixnum) 
;;;(defparameter *default-arg-type* :int)
(defparameter *default-arg-type* :long)

;;; parse LCL def-foreign-function macro args returning multiple-values:
;;; (lisp-name c-name return-type arglist arg-decls c-args qffi-c-types kw-opts must-wrap)
;;; This is generic to all FFI backends.

(defun parse-lcl-def-foreign-function (macro-args)
  (destructuring-bind ((lisp-fn-name . kw-args) . args)  macro-args
    (let* ((language :c)
	   (c-name (let ((name-spec (cadr (assoc :name kw-args))))
		     (when (consp name-spec)
		       (setq name-spec (eval name-spec)))
		     (if (stringp name-spec)
			 name-spec
			 (convert-to-language (or name-spec lisp-fn-name) language))))
	   (qffi-return-type (lcl-to-qffi-type (or (cadr (assoc :return-type kw-args)) 
						   *default-arg-type*)))
	   )
      (loop for arg in args
	    with (lcl-type qffi-type var optional key lisp-type)
	    for larg = arg
	    for true-arg = t
	    when (eq arg '&rest)
	      do (warn ";;; DEF-FOREIGN-FUNCTION: ~s option not supported in ~a"
		       arg lisp-fn-name)
		 (return-from parse-lcl-def-foreign-function nil)
	    do (cond ((eq arg '&optional) (setq optional t true-arg nil))
		     ((eq arg '&key) (setq key t true-arg nil))
		     ((atom arg) 
		      (warn "DEF-FOREIGN-FUNCTION is defaulting arg ~a in ~a~%"
			    arg lisp-fn-name)
		      (setq var arg
			    larg arg
			    lcl-type *default-arg-type*))
		     (t (setq larg (car arg) ; larg is permitted to have an initialization option
			      lcl-type (second arg)
			      var (if (atom larg) larg (car larg)))) )
	    collect larg into arglist	; arglist for defun
	  
	    when true-arg
	      do (setq qffi-type (lcl-to-qffi-type lcl-type))
		 (setq lisp-type (qffi-to-lisp-type qffi-type))
	      and collect var into vars
	      and when lisp-type 
		    collect `(type ,lisp-type ,var) into arg-decls 
	    end
	    and collect qffi-type into qffi-c-types		    
	    finally
	 (return (values kw-args
			 lisp-fn-name c-name qffi-return-type arglist arg-decls vars qffi-c-types 
			 (or optional key)))))))
	 

(defun parse-lcl-def-foreign-callable (args)
  (labels ((convert-foreign-type (lcl-type) (lcl-to-qffi-type lcl-type))
	   (convert-callback-return-type (lcl-type) 
	     (and lcl-type (convert-foreign-type lcl-type))))
    (block nil
      (let (name options c-name (language :c) ret-type body lib-name)
	;; parse arguments
	(cond ((atom args)
	       (warn "DEF-FOREIGN-CALLABLE: Ignoring ill-formed form.")
	       (return-from nil))
	      ((atom (car args)) (setq name (pop args)))
	      (t (setq name (caar args)
		       options (cdar args)
		       args (cdr args))))
	(cond ((atom args)
	       (warn "DEF-FOREIGN-CALLABLE: Ignoring ill-formed form.")
	       (return-from nil))
	      (t (setq body (cdr args)
		       args (car args))))
	(setq c-name name)
	(dolist (opt options)
	  (case (car opt)
	    (:language    (setq language (second opt))
			  (unless (eq language :c)
			    (warn
			     "DEF-FOREIGN-CALLABLE: Option ~S is not supported for callbacks."
			     opt)
			    (return-from nil))
			  )
	    (:return-type (setq ret-type
				(convert-callback-return-type (second opt))))
	    (:name        (setq c-name (second opt))
			  (when (listp c-name) (setq c-name (eval c-name))))
	    (:library (setq lib-name (second opt)))
	    (otherwise
	     (warn "DEF-FOREIGN-CALLABLE: Ignoring illegal option: ~S" opt))))
	(ignore lib-name)
	(when (and c-name (symbolp c-name)) ; this looks bogus -- why convert name rather than c-name
	  (setq c-name (convert-to-language name language)))
	;; Make the Lisp function name weird so it isn't accidently called from Lisp.
	(setq name (intern (format nil "%%%~a%%%" name) (symbol-package name)))

	(loop for arg in args
	      for (arg-name lcl-type) = arg
	      for cmutype = (convert-foreign-type lcl-type)
	      collect (list arg-name cmutype) into c-args-and-qffi-types
	      finally
	   (return
	     (values name c-name ret-type c-args-and-qffi-types body)))))))


(defun parse-lcl-def-foreign-struct (args)
  (destructuring-bind (name &rest slotdefs) args
    (loop for slot in slotdefs
	  for (slot-name ignore slot-lcl-type . slot-extras) = slot
	  for slot-qffi-type = (lcl-to-qffi-type slot-lcl-type)
	  for val = (car slot-extras)
	  for ov/off = (cadr slot-extras)
	  do (ignore ignore)
	  when val 
	    do (warn ";;; DEF-FOREIGN-STRUCT Ignoring initialization for slot ~a of ~a~%" slot-name name )
	  when ov/off
	    do (warn ";;; DEF-FOREIGN-STRUCT: Ignoring :OFFSET in ~S of ~a" slot name)
	  collect `(,slot-name ,slot-qffi-type) into slot-names-and-qffi-types
	  finally 
       (return (values name slot-names-and-qffi-types)))))
