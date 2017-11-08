(in-package :cl)

(defvar config::weak-eval-cache nil)

(asdf:defsystem :cl-cme-lx
  :defsystem-depends-on (:cl-ev-pathnames :cl-cme-lcl :cl-cme-st)
  :serial t 
  :components
  ((:file "common-symbols-pkg")
   (:file "lx-pkg")
   (:file "custom")
   (:file "colors")             ; Not the best place, but see file for issues.
   (:file "file-property-list") ; this probably should move to system-tool)
   (:file "lisp-extensions")
   (:file "pathname-extensions")	; would like to eliminate this
   (:file "struct-class")
   (:file "eval-cache")
   (:file "lisp-io")
   (:file "universal-time")
   (:file "clos-tools")
   (:file "binary-search")
   ))
