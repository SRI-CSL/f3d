(in-package :cl-user)

(st:define-system :lcl 
    :required-systems '() 
    :files '("lcl-pkg.lisp"
	     "generic-lcl-fns.lisp"
	     #+cmu     "cmucl-lcl-fns.lisp"
	     #+allegro "acl-lcl-fns.lisp"
	     #+sbcl "sbcl-lcl-fns.lisp"
	     ))
