(in-package :cl-user)

;;; The minimal package definition for :LCL to support QFFI 
(defpackage :lcl (:use #+sbcl :common-lisp #-sbcl :lisp)
	    (:import-from :asdf asdf::getenv)
	    #+cmu (:import-from :ext memq assq)
	    #+sbcl (:import-from :SB-INT memq assq)
	    #+allegro (:import-from excl memq assq)
	    )

(in-package :lcl)
 
;;; This doesn't really belong here, but these functions are defined by qffi.
(export '(underlying-simple-vector array-simple-vector with-interrupts-deferred
	  def-foreign-function def-foreign-callable def-foreign-synonym-type 
	  def-foreign-struct 
	  foreign-variable-value  foreign-variable-pointer foreign-pointer-address
	  foreign-string-value dummy-pointer register-all-callbacks
	  make-foreign-pointer

	  environment-variable setenv getenv

	  %POINTER MEMQ ASSQ COMMAND-LINE-ARGUMENT WITHOUT-FLOATING-UNDERFLOW-TRAPS
 
	  ) 
	:lcl)
