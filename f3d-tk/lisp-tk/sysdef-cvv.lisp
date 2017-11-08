(in-package :cl-user)

(st:define-system :cvv
    :required-systems '(ev-pathnames tk)
    :files '("cvv-basics.lisp"
	     "cvv.lisp"
	     ;;"cvv-group-control.lisp"
	     ))
