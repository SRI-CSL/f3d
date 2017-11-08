(in-package :qffi)

;;; The remainder of this file should move to cmucl-ffi-fns.lisp

(defmacro make-foreign-pointer (&key address type)
  `(alien::sap-alien (alien::int-sap ,address) ,type))

(declaim (inline foreign-pointer-address))

(defun foreign-pointer-address (x)
  (cond ((typep x 'integer)
	 x)
	((typep x 'SYSTEM:SYSTEM-AREA-POINTER)
	 (alien::sap-int x))
	(t (alien::sap-int (alien::alien-sap x)))))

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
  ;; some kind of code gen bug necessitates calling eval for now
  (handler-case (eval `(system::foreign-symbol-address ,string :flavor :data))
      (error () (error "system::foreign-symbol-address(~a) is undefined" string) nil)))

(defun foreign-variable-value (name &optional (type 'c-call:int))
  (handler-case
      (alien::deref (alien::%sap-alien (foreign-variable-pointer name)
				       (alien::parse-alien-type `(* ,type))))
      (error () (format t ";;; FOREIGN-VARIABLE-VALUE warning: symbol ~a is undefined~%"
			name))))

(defun (setf foreign-variable-value) (value name &optional (type 'c-call:int))
  (handler-case
      (setf (alien::deref (alien::%sap-alien (foreign-variable-pointer name)
					     (alien::parse-alien-type `(* ,type))))
	    value)
      (error () ;;(format t ";;; (SETF FOREIGN-VARIABLE-VALUE) warning: symbol ~a is undefined~%" name)
       (break ";;; (SETF FOREIGN-VARIABLE-VALUE) warning: symbol ~a is undefined~%" name)

       )))
#|
"FREEDIUS_make_lisp_page_handler_block_map_callback_address"
(foreign-variable-value "FREEDIUS_make_lisp_page_handler_block_map_callback_address"
			'unsigned-long)
(alien::sap-int (alien::symbol-trampoline 'IMG::%%%MAKE_LISP_PAGE_HANDLER_BLOCK_MAP%%%))
(foreign-variable-pointer "FREEDIUS_make_lisp_page_handler_block_map_callback_address")
|#

