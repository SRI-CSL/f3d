(in-package :qffi)

#|

 ***************************   GENERIC FOREIGN FUNCTION INTERFACE  ***************************

The idea here is to separate the "syntax" of an FFI declaration from its target FFI implementation
by mapping the syntactic component of an FFI declaration for a specific syntax into a "canonical FFI
representation", from which any target FFI implementation can be generated.

To accomplish this not all features of every FFI are supported.  In some cases (such as CMUCL
callbacks) the "native" FFI requires extensions.


The Lucid Common Lisp (LCL) foreign argument type specifiers are used in the canonical FFI
representation.  Should this be extended to also allow (:signed <nbits>), (:unsigned <nbits>),
(:boolean <nbits>), and (enum name {spec}*) ?

FFI Syntax Support:
  
   Currently only LCL FFI syntax is supported, but it should be straight-forward to provide support
   for ACL and CMUCL FFI syntaxes as well.

FFI Target Support:
   
   Currently only ACL and CMUCL target FFIs are supported.  


More Detail:

Each FFI syntax has a way of specifying foreign function names, argument types, and return values.


(DEFINE-FOREIGN-FUNCTION-INTERNAL lisp-name foreign-namestring return-type args 
				  &key ARG-CHECKING OPTIMIZE)

(LCL:DEF-FOREIGN-FUNCTION (Lisp-name &rest options-alist) &rest args)

(FF:DEFFOREIGN name-spec args &rest keyword-options)
where name-spec is a Lisp symbol or the list (Lisp-name foreign-namestring).

|#

#|
Windows note: Windows builds require us to export the symbols we will
need.  Therefore, we will need to modify def-foreign-function so that
we can automatically generated the ".def" files that each library
needs.  Unfortunately, each .def file is exclusive to its
corresponding .dll file, so we also need to keep track of the DLL to
which a given symbol belongs. 
|#

()

 
#|
;;;***************************  FOREIGN FUNCTION ARGUMENT TYPES  ***************************

    There are two possible ways to specify FFI argument types:

  1. Common Lisp types in a subset of the CL type hierarchy augmented with pointer types.

  2. keyword package equivalents of C scalar types plus list syntax for array and pointer types.

Question:

  .  What should be the "canonical" representation for FFI argument types: Lisp or C types?

  
   Pro Lisp Types:  Lisp type "syntax" that can be directly used for arg checking.

   Con Lisp Types:  Difficult to specify the legal subset of the CL type hierarchy.
                    

   Pro C Types:     Direct mapping to C.


Foreign Function Lisp Argument Types:

<cl-arg-type> == <cl-scalar-type> | <cl-array-type> | <cl-pointer-type> | <cl-declared-type-name>

<cl-return-type> == <cl-scalar-type> | simple-string | <cl-pointer-type> | :void

<cl-scalar-type> ==    <cl-scalar-type> | boolean

<cl-scalar-type> == fixnum | (fixnum [lower-limit [upper-limit]])

                    integer | (integer [lower-limit [upper-limit]])

                    signed-byte | (signed-byte [nbits])

                    unsigned-byte | (unsigned-byte [nbits])

                    single-float | (single-float [lower-limit [upper-limit]])

                    double-float |(double-float [lower-limit [upper-limit]])

<cl-array-type> ==  array         | (array <cl-scalar-type> [<dimension-spec>]) |

                    simple-array  | (simple-array <cl-scalar-type> [<dimension-spec>]) |

                    simple-string | (simple-string <size>)

<dimension-spec> ==  <rank> | * | ( { <dimension> | * }* )

<cl-pointer-type> == :pointer |

                     (:pointer <cl-foreign-type>) |

                     ? (:pointer <cl-scalar-type>) |

                     ? (:pointer <cl-pointer-type>)

 
QFFI Foreign Function C Argument Types:

<c-arg-type> == <c-scalar-type> | <c-array-type> | :SIMPLE-STRING | <c-pointer-type> | <c-foreign-type-name>

<c-return-type> == <c-scalar-type> | <c-pointer-type> | :NULL

The following corresponds to Allegro "Primitive Types":

<c-scalar-type> == :FIXNUM | :CHAR | :SHORT | :INT | :LONG | 

                   :UNSIGNED-CHAR | :UNSIGNED-SHORT | :UNSIGNED-INT | :UNSIGNED-LONG |

                   :FLOAT | :DOUBLE | :LISP

<c-array-type> ==  :ARRAY         | (:ARRAY <c-scalar-type> [<dimension-spec>]) |

                   :SIMPLE-ARRAY  | (:SIMPLE-ARRAY <c-scalar-type> [<dimension-spec>]) |

                   :SIMPLE-STRING | (:SIMPLE-STRING <size>)

<c-pointer-type> == :POINTER |

                    (:POINTER <c-scalar-type>) |

                    (:POINTER <foreign-type>) |

                  ? (:POINTER <c-pointer-type>)



Foreign Function Declaration Keyword Options:

:RETURN-TYPE or :RETURNING

:ARG-CHECKING

:OPTIMIZE
     
|#               


()

#|

FROM THE UFFI DOCUMENTATION:

:char - Signed 8-bits. A dereferenced :char pointer returns an character. 

:unsigned-char - Unsigned 8-bits. A dereferenced :unsigned-char pointer returns an character. 

:byte - Signed 8-bits. A dereferenced :byte pointer returns an integer.

:unsigned-byte - Unsigned 8-bits. A dereferenced :unsigned-byte pointer returns an integer.

:short - Signed 16-bits.

:unsigned-short - Unsigned 16-bits.

:int - Signed 32-bits.

:unsigned-int - Unsigned 32-bits.

:long - Signed 32-bits. 

:unsigned-long - Unsigned 32-bits.

:float - 32-bit floating point.

:double - 64-bit floating point.

:cstring - A NULL terminated string used for passing and returning characters strings with a C function.

:void - The absence of a value. Used to indicate that a function does not return a value.

:pointer-void - Points to a generic object. 

* - Used to declare a pointer to an object.

|#



(defparameter *user-defined-foreign-types* (make-hash-table :test 'eq))

#|
(maphash #'(lambda (k v) 
	     (setf (gethash k *user-defined-foreign-types*) 
		   (if (numberp v) v (lcl-to-qffi-type v))))
	 alien::*user-defined-foreign-types*)
|#

(defun expand-foreign-type (type)
  ;; if result is EQ to arg, then this is a primitive type
  (let ((type0 type))
    (if (and (symbolp type) (setq type (gethash type *user-defined-foreign-types*)))
	(if (atom type)
	    ;;type
	    (if (numberp type) ; temporary hack
		type0
		(expand-foreign-type type)) ; recursively expand
	    (case (car type)
	      ((:pointer :array :simple-array :array-or-null :simple-array-or-null)
	       (let ((type2 (expand-foreign-type (cadr type))))
		 (if (numberp type2) ; temporary hack
		     type
		     `(,(car type) ,(expand-foreign-type (cadr type)) . ,(cddr type)))))
	      (otherwise (error "EXPAND-FOREIGN-TYPE: illegal foreign type: ~a" type))))
	
	(if (and (null type) (not (keywordp type0)))
	    (progn (format t ";;; *********  Warning: the foreign type ~a ~s is not defined  ********~%"
			   (symbol-package type0) type0)
		   type0)
	    type0))))

#|
(expand-foreign-type 'img::c-image)
(expand-foreign-type 'lcl:pointer)
(gethash 'lcl:pointer *user-defined-foreign-types*)
(gethash 'lcl:pointer alien::*user-defined-foreign-types*)
|#

;;; This version does not call expand-foreign-type so that type redefinitions 
;;; and out-of-order definitions are possible
(defun make-foreign-synonym-type (name qffi-type)
  (setf (gethash name *user-defined-foreign-types*) qffi-type)
  name)

(defun def-foreign-synonym-type-expander (name qffi-type)
  `(eval-when (compile load eval)
    (make-foreign-synonym-type ',name ',qffi-type)))


	   
;;; NIL means to do complete range checking
;;; alternative to FIXNUM is INTEGER which is more expensive for arg-checking
;;(defparameter *qffi-to-lisp-integer-type* 'fixnum)
(defparameter *qffi-to-lisp-integer-type* nil)

(defconstant *machine-word-length*
  #+sbcl sb-vm::n-machine-word-bits 
  #+cmu vm:word-bits ;; 32
  #+allegro 32 ; FIXME
)

;;; This is used to generate the declarations for arg-checking.
(defun qffi-to-lisp-type (qffi-c-type &optional array-element-type-p)
  (let ((type qffi-c-type))
    (if (keywordp type)
	(if (or (null *qffi-to-lisp-integer-type*) array-element-type-p)
	    (case type
	      (:lisp t)
	      (:fixnum 'fixnum)
	      (:char  '(signed-byte 8))
	      (:short  '(signed-byte 16))
	      (:int  '(signed-byte 32))
	      ;;(:long 'integer)		; ???????????????????????
	      (:long `(signed-byte ,*machine-word-length*))
	      (:unsigned-char '(unsigned-byte 8))
	      (:unsigned-short '(unsigned-byte 16))
	      (:unsigned-int '(unsigned-byte 32))
	      ;;(:unsigned-long '(integer 0 *)) ; ??????????????????????
	      (:unsigned-long `(unsigned-byte ,*machine-word-length*))
	      (:single-float  'single-float)
	      (:double-float  'double-float)
	      (:simple-string 'simple-string)
	      (:pointer 'foreign-pointer)
	      (:array 'array)
	      (:simple-array 'simple-array)
	      (:array-or-null '(or null array))
	      (:simple-array-or-null '(or null simple-array))
	      (otherwise (error "QFFI-TO-LISP-TYPE: Unknown qffi foreign type -> ~S" type))
	      )
	    (case type
	      ((:fixnum :char :short :unsigned-char :unsigned-short)
	       'fixnum)
	      ((:int :long :unsigned-int :unsigned-long)
	       *qffi-to-lisp-integer-type*)
	      (:single-float  'single-float)
	      (:double-float  'double-float)
	      (:simple-string 'simple-string)
	      (:pointer 'foreign-pointer)
	      (:array 'array)
	      (:simple-array 'simple-array)
	      (:array-or-null '(or null array))
	      (:simple-array-or-null '(or null simple-array))
	      (otherwise (error "QFFI-TO-LISP-TYPE: Unknown qffi foreign type -> ~S" type))
	      ))
	
	(cond ((symbolp type)
	       (let ((expansion (expand-foreign-type type)))
		 (if (not (equal expansion type))
		     (qffi-to-lisp-type expansion)
		     (error "QFFI-TO-LISP-TYPE: Unknown qffi foreign type -> ~S" type))))
	      ((consp type)
	       (case (car type)
		 ((* :pointer)
		  (case (cadr type)
		    (:char 'simple-string)
		    (:void 'foreign-pointer)
		    ;;FIXME? (otherwise `(foreign-pointer ,(qffi-to-lisp-type (cadr type) t)))
		    (otherwise `foreign-pointer)))
		 ((:array :simple-array :array-or-null :simple-array-or-null)
		  (let ((el-type (if (cadr type) (qffi-to-lisp-type (cadr type) t) '*))
			(bounds (cddr type)))
		    (case (car type)
		      (:array `(array ,el-type . ,bounds))
		      (:simple-array `(simple-array ,el-type . ,bounds))
		      (:array-or-null `(or null (array ,el-type . ,bounds)))
		      (:simple-array-or-null `(or null (simple-array ,el-type . ,bounds))))))
		 (otherwise (error "QFFI-TO-LISP-TYPE: Unknown qffi foreign type -> ~S" type))))
	      (t (error "QFFI-TO-LISP-TYPE: Unknown qffi foreign type -> ~S" type))))))


;;; slot-type is a qffi-slot-type
(defun foreign-type-struct-slot-array-index-info (slot-name slot-type package)
  (declare (ignore slot-name))
  (and (consp slot-type)
       (member (car slot-type) '(:array :simple-array :simple-array-or-null :array-or-null))
       (loop for i from 0
	     for bound in (caddr slot-type)
	     for var-name = (intern (format nil "I~d" i) package)
	     collect var-name into indices
	     collect `(type (integer 0 ,bound) ,var-name) into index-decls
	     finally (return (values indices index-decls)))))

;;; FIXME -- where does this belong?
(defun FREEDIUS-PREFIX (x) (format nil "FREEDIUS_~a" x))


#|

(alien:load-foreign "/opt/IU/FREEDIUS/lisp/ffi/cmucl-ffi-test.so")


(def-foreign-function (test1 (:name "FREEDIUS_identity")
			     (:return-type :pointer))
  (x :unsigned-32bit))

(def-foreign-function (test1 (:name "FREEDIUS_identity")
			     (:return-type :pointer))
  (x :unsigned-32bit))

(lcl:def-foreign-function (test1l (:name "FREEDIUS_identity")
			     (:return-type :pointer))
  (x :unsigned-32bit))

(list (test1 1) (test1l 1))

(def-foreign-function (test2 (:name "FREEDIUS_identity")
			     (:return-type :pointer))
  (x :pointer))

(setq *p* (test1 1))

(test2 *p*)

(lcl::def-foreign-struct foo
    

(setq dff-form
      '((make_tiff_file_image (:name (FREEDIUS-PREFIX "make_tiff_file_image"))
	 (:return-type  (:pointer c-image)))
	(xdim :int)
	(ydim :int)
	(element_type :int)
	(spp :int)
	(block_xdim :int)
	(block_ydim :int)
	(padded_block_xdim :int)
	(path :simple-string)))

(parse-def-foreign-function dff-form)
(expand-def-foreign-function dff-form)
 

(def-foreign-struct agl_rendering_spec
    (RgbaFlag :type :signed-32bit)
    (ColorBits :type :signed-32bit)
    (DoubleFlag :type :signed-32bit)
    (DepthSize :type :signed-32bit)
    (AccumBits :type :signed-32bit)
    (AlphaSize :type :signed-32bit)
    (StencilSize :type :signed-32bit)
    (OverlayFlag :type :signed-32bit)
    (StereoFlag :type :signed-32bit)
    (AuxNumber :type :signed-32bit))

(setq *agl_rendering_spec1* (alien:make-alien agl_rendering_spec))
(agl_rendering_spec-RgbaFlag *agl_rendering_spec1*)
(setf (agl_rendering_spec-RgbaFlag *agl_rendering_spec1*) 1)


(def-foreign-function (foo (:name (FREEDIUS-PREFIX "make_tiff_file_image"))
			   (:arg-checking t)
					;(:return-type  (:pointer c-image))
			   (:return-type :simple-string)
			   )
  (a :int)
  (b :fixnum)
  (c :unsigned-32bit)
  (d :simple-array)
  (e (:simple-array-or-null :unsigned-8bit))
  (e2 (:simple-array-or-null :unsigned-8bit (* *)))
  (f :pointer)
  (g (:pointer :unsigned-32bit))
  (s :simple-string))

(disassemble 'foo)

(defun foo (x)
  (declare (type (SIGNED-BYTE 32) x))
  (ash x -1))


(defun foo (x)
  (declare (type fixnum x))
  (ash x -1))

(defun foo (x)
  (declare (type integer x))
  (ash x -1))

(disassemble 'foo) 


(setq dff-forms
      (with-open-file (st "/opt/IU/FREEDIUS-QUAM/lisp/cmucl/fcme-foreign-functions.lisp")
	(loop with eof = (list nil)
	      for form = (lx:condition-case () (read st nil eof)
			    (error nil))
	      until (eq form eof)
	      when (and (consp form) (eq (car form) 'LCL:DEF-FOREIGN-FUNCTION))
		collect form)))


(with-open-file (st "/opt/IU/FREEDIUS-QUAM/lisp/cmucl/fcme-foreign-functions.lisp")
  (with-open-file (ost "/tmp/foreign-functions.lisp" :direction :output)
    (format ost "(in-package :qffi)~%%~%")
    (loop with eof = (list nil)
	  for form = (lx:condition-case () (read st nil eof)
					   (error nil))
	  until (eq form eof)
	  when (and (consp form) (eq (car form) 'LCL:DEF-FOREIGN-FUNCTION))
	    do (pprint form ost))))


(defmacro lcl:def-foreign-function (&rest args)
  (expand-def-foreign-function args))

(defmacro lcl::DEF-FOREIGN-STRUCT (&rest args)
  `(DEF-FOREIGN-STRUCT .,args))

(compile-file "/tmp/foreign-functions.lisp")

(def-foreign-synonym-type GLmatrixd (:simple-array :double-float))

(expand-foreign-type  'GLmatrixd)


(DEF-FOREIGN-FUNCTION
 (GLLOADMATRIXD (:NAME "glLoadMatrixd") (:RETURN-TYPE :NULL))
 (M GLMATRIXD))

|#




(defun foreign-array-dims (type)
  (and (consp type)
       (let ((tl (cddr type)))
	 (and (consp tl)
	      (if (consp (car tl))
		  (car tl)
		  (loop for d in tl
			while (typep d '(integer 0))
			collect d))))))
#|
(foreign-array-dims  '(:simple-array :unsigned-32bit (256)))
|#

