(in-package :cl-user)


(st:define-system "radius-classes"
    :files '("smc-pkg.lisp"
	     "radius-classes.lisp"
	     ))


#|
We have a real problem here.  If there are multiple choices
for the underlying cme system, then that needs to be known here.

The above call to define-system punts on the issue by assuming that
the cme system is already loaded.


|#


#+broken ;; wrong for system cme
(st:define-system "radius-classes"
;;    :required-systems '(cme)
    :required-systems '(cme-less-tk)
    :files '("smc-pkg.lisp"
	     "radius-classes.lisp"
	     ))
