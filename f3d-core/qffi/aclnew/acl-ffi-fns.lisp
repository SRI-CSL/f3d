(in-package :qffi) 


(defvar *lisp_call_address_function_address* nil)

(defun register-callback-now (name)
  (let* ((c-name (if (consp name) (cadr name) (string-downcase name)))
	 (name (if (consp name) (car name) name))
	 (index (nth-value 1 (ff:register-foreign-callable name :reuse t))))
    ;;(format t "~%Registering callback ~a (c-name ~a) index=~d" name c-name index)
    (setf (foreign-variable-value (remap-foreign-symbol-name (format nil "~a_callback_index" c-name)))
	  index) 
    (setf (foreign-variable-value (remap-foreign-symbol-name (format nil "~a_lca" c-name)))
	  (or *lisp_call_address_function_address*
	      (setq *lisp_call_address_function_address*
		    (ff:get-entry-point (remap-foreign-symbol-name "lisp_call_address"))))) 
    index))

(defvar *deferred-register-callback-list* nil)

(defun register-callback (name)
  (pushnew name *deferred-register-callback-list*))

(defun register-all-callbacks ()
  (loop for name in *deferred-register-callback-list*
	do (register-callback-now name)))


(defun FOREIGN-POINTER-ADDRESS (x)
  (if (integerp x)
      x
      (ff:foreign-pointer-address x)
      ;(error "LtoA: Foreign pointer is not an integer.")
      ))


(defun null-pointer-p (x)
  (zerop (foreign-pointer-address x)))

(defun FOREIGN-POINTER-P (x)
  (or (integerp x) (ff:foreign-pointer-p x)))

(defmacro MAKE-FOREIGN-POINTER (&whole form
                                &key type address
				     static alignment
                                     &allow-other-keys)
  (when (or static alignment type)
    (warn "LtoA: Ignoring :TYPE :STATIC and :ALIGNMENT keywords in ~S" form))
  (unless address
    (warn "QFFI: MAKE-FOREIGN-POINTER ADDRESS keyword is required in ~S" form))
  address)


(defun FOREIGN-VARIABLE-POINTER (string)
  (ff:get-entry-point string))


#+old
(defmacro FOREIGN-VALUE (x &optional type)
  (let* ((acc
	  (case type
	    ((:char :character)
	     'callback-char-place)
	    ((nil :long) 
	     (unless type
	       (warn "LtoA: FOREIGN-VALUE defaulting to :LONG."))
	     'callback-long-place)
	    (:single-float 'callback-single-place)
	    (:double-float 'callback-double-place)
	    (otherwise (warn "LtoA: Unknown type ~S in FOREIGN-VALUE." type)))))
    `(foreign-value-fn #',acc ,x ',type)))

#-old
(defmacro FOREIGN-VALUE (x &optional type)
  x)

#||
(defun foreign-value-fn (acc ptr type)
  (let ((v (funcall acc ptr)))
    (if (eq type :char)
        #+allegro-v9.0 (code-char v) #-allegro-v9.0 (CLTL1:INT-CHAR  v)
      v)))

(defsetf foreign-value-fn (acc ptr type) (val) 
  `(set-foreign-value-fn ,acc ,ptr ,type ,val))

(ff:def-foreign-type callback-char-place :char)
;;; what about :unsigned-long?
(ff:def-c-type callback-long-place :long)
(ff:def-c-type callback-single-place :single-float)
(ff:def-c-type callback-double-place :double-float)
(defvar *callback-place* (make-callback-double-place))

(defun set-foreign-value-fn (acc ptr type val)
  (declare (ignore acc))
  (case type
    ((:char :character) (setf (callback-char-place ptr) (char-int val)))
    ;;((nil :long) (setf (callback-long-place ptr) (coerce val 'fixnum)))
    ((nil :long) (setf (callback-long-place ptr) (coerce val 'integer)))
    (:single-float (setf (callback-single-place ptr) 
		     (coerce val 'single-float)))
    (:double-float (setf (callback-double-place ptr) 
		     (coerce val 'double-float)))
    (otherwise (error "LtoA: Unknown FOREIGN-VALUE type ~S" type))))


(defmacro FOREIGN-POINTER-TYPE (x)
  `(foreign-pointer-type-fun ,x))

(defun foreign-pointer-type-fun (x)
  (declare (ignore x))
  ;;(warn "(FOREIGN-POINTER-TYPE) is bogus~%")
  :UNSIGNED-32BIT)

(defsetf foreign-pointer-type-fun (x) (val)
  `(foreign-pointer-type-set ,x ,val))

(defun foreign-pointer-type-set (x val)
  (declare (ignore x))
  ;;(warn "(SETF FOREIGN-POINTER-TYPE) is not supported~%")
  val)
||#


;;;
;;; This is nuts - apparently the only things we can look at are
;;; unsigned ints:
;;;
(defun foreign-variable-value (name &optional (type :unsigned-int))
  (let ((ptr (foreign-variable-pointer  name )))
    (cond ((null ptr)
	   (format t ";;; foreign-variable-value warning: symbol ~a is undefined~%"
		   name))
          #-old
          (t (ff::fslot-value-typed type :c ptr))
          #+old
	  (t (setf (foreign-pointer-type ptr) '(:pointer :unsigned-32bit))
	     (foreign-value ptr :long) ;; should be :unsigned-long
	     ))))

(defun (setf foreign-variable-value) (value name &optional (type :unsigned-int))
  (let ((ptr (foreign-variable-pointer name )))
    (cond ((null ptr)
	   (format t ";;; foreign-variable-value warning: symbol ~a is undefined~%"
		   name))
          #-old
          (t (setf (ff::fslot-value-typed type :c ptr) value))
          #+old
	  (t (setf (foreign-pointer-type ptr) '(:pointer :unsigned-32bit))
	     (setf (foreign-value ptr :long) value);; should be :unsigned-long
	     ))))

#+old
(defun malloc-foreign-string (str)
  (ff:string-to-char* str))

#-old
(defun malloc-foreign-string (str)
  str)
