(in-package :qffi)

(defmacro make-foreign-pointer (&key address type)
  `(sb-alien::sap-alien (sb-alien::int-sap ,address) ,type))

(declaim (inline foreign-pointer-address))

(defun foreign-pointer-address (x)
  (cond ((typep x 'integer)
	 x)
	((typep x 'SB-SYS::system-area-pointer)
	 (sb-sys:sap-int x))
	(t (sb-alien::sap-int (sb-alien::alien-sap x)))))

(defun null-pointer-p (x)
  (zerop (foreign-pointer-address x)))

(eval-when (compile eval)
  (when (find-symbol "foreign-symbol-address" :cl)
      (pushnew :foreign-symbol-address-in-cl-package *features*)))

(defun foreign-string-value (x)
  x)
 
(defun malloc-foreign-string (str)
  str)

(defun free-foreign-pointer (p)
  (declare (ignore p)))

(defun foreign-variable-pointer (string)
  (handler-case (sb-sys::foreign-symbol-sap (namestring string) t)
      (error () (error "sb-sys::foreign-symbol-address(~a) is undefined" string) nil)))

#|
(defvar *foreign-symbol-address-fn* #'sb-sys::foreign-symbol-address)

(defun foreign-variable-pointer (string)
  ;; some kind of code gen bug necessitates calling eval for now
  (handler-case (funcall *foreign-symbol-address-fn* string)
    (error () (error "sb-sys::foreign-symbol-address(~a) is undefined" string) nil)))

(defun foreign-variable-pointer (string)
  ;; some kind of code gen bug necessitates calling eval for now
  (handler-case (eval `(sb-sys::foreign-symbol-address ,string t))
    (error () (error "sb-sys::foreign-symbol-address(~a) is undefined" string) nil)))
|#

(defun foreign-variable-value (name &optional (type 'sb-alien:int))
  (handler-case
      (sb-alien::deref (sb-alien::%sap-alien (foreign-variable-pointer name)
				       (sb-alien::parse-alien-type `(* ,type) nil)))
      (error () (format t ";;; FOREIGN-VARIABLE-VALUE warning: symbol ~a is undefined~%"
			name))))

(defun (setf foreign-variable-value) (value name &optional (type 'sb-alien:int))
  (handler-case
      (setf (sb-alien::deref (sb-alien::%sap-alien (foreign-variable-pointer name)
					     (sb-alien::parse-alien-type `(* ,type) nil)))
	    value)
      (error () ;;(format t ";;; (SETF FOREIGN-VARIABLE-VALUE) warning: symbol ~a is undefined~%" name)
       (break ";;; (SETF FOREIGN-VARIABLE-VALUE) warning: symbol ~a is undefined~%" name)

       )))
#|
"FREEDIUS_make_lisp_page_handler_block_map_callback_address"
(foreign-variable-value "FREEDIUS_make_lisp_page_handler_block_map_callback_address"
			'unsigned-long)
(sb-alien::sap-int (sb-alien::symbol-trampoline 'IMG::%%%MAKE_LISP_PAGE_HANDLER_BLOCK_MAP%%%))

|#

