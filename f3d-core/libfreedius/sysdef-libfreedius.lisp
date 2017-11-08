(in-package :cl-user)

(defvar config::*system-library-files* nil)

(st:define-system :libfreedius
    :system-class 'st::mixed-lisp-system
    :required-systems '(qffi) 
    :c-source-path "$FREEDIUS/c/"
   ;; :libraries `(,@config::*system-library-files* "$FREEDIUS_ARCH/lib/libfreedius")
    :libraries `(,@config::*system-library-files* "libfreedius" )
    :files '("libfreedius.lisp"))
