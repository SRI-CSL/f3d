(in-package :cl-user)

;;; The files in this system should move to sysdef-generic-gui.lisp,
;;; since they are not really tk specific.

;;; tk-pkg.lisp needs to be split into generic and tk parts.

(st:define-system :tkmin
    :required-systems '(qffi lisp-extensions)
    :files '("tk-pkg.lisp"
             "colors.lisp"          ;; could be moved elsewhere to eliminate tk dependency
	     "tk-basics.lisp"
	     "mouse-events.lisp"
	     "cvv-basics.lisp"		; should this be here?
	     ))

