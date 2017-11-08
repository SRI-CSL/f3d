(in-package :cl-user)

(st::define-system :foreign-vector
    :libraries '(#-allegro "libf3d-foreign-vector")
    ;; FIXME:  libf3d-foreign-vector currently needs galloc and xxfree from misc.c++
    :required-systems '(qffi libfreedius)
    :files '(#+cmu "cmucl-foreign-vector.lisp"
	     #+allegro "acl-foreign-vector.lisp"
	     #+sbcl "sbcl-foreign-vector.lisp"
	     ))

