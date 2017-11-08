(in-package :cl)

(asdf:defsystem :cl-ev-pathnames
  :depends-on (:cl-cme-st)
  :serial t
  :components
  ((:file "ev-pathname-pkg")
   (:file "ev-pathnames")))
