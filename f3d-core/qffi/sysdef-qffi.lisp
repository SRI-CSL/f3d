(in-package :cl-user)

(st::define-system :qffi
      :required-systems '(ev-pathnames lcl)   
      :files `("qffi-pkg.lisp" 
	       "qffi.lisp"
	       "lcl-ffi-frontend.lisp"
	       #+cmu
	       ,@'("cmucl-array-address.lisp" 
		   "cmucl-ffi-fns.lisp" 
		   "cmucl-ffi-backend.lisp"
		   ;;"../cmucl/foreign-vector.lisp" ; move to qffi directory
		   )
	       #+sbcl
	       ,@'("sbcl-array-address.lisp" 
		   "sbcl-ffi-fns.lisp" 
		   "sbcl-ffi-backend.lisp"
		   ;;"../sbcl/foreign-vector.lisp" ; move to qffi directory
		   )
	       #+allegro
	       ,@'("acl-ffi-fns.lisp" 
		   "acl-ffi-backend.lisp"
		   ;;"../allegro/foreign-vector.lisp" ; move to qffi directory
		   )
	       "generic-ffi-fns.lisp"
	       "../system-tool/mixed-lisp-system.lisp"
	       ))
