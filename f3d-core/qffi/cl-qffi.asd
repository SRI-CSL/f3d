(in-package :cl)

(asdf:defsystem :cl-qffi
  :depends-on (:ev-pathnames :lcl)
  :serial t 
  :components
  (
   (:file "qffi-pkg") 
   (:file "qffi")
   (:file "lcl-ffi-frontend")
   #+cmu
   ,@'((:file "cmucl-array-address") 
       (:file "cmucl-ffi-fns") 
       (:file "cmucl-ffi-backend")
       ;;(:file "../cmucl/foreign-vector") ; move to qffi directory
       )
   #+sbcl
   ,@'((:file "sbcl-array-address") 
       (:file "sbcl-ffi-fns") 
       (:file "sbcl-ffi-backend")
       ;;(:file "../sbcl/foreign-vector") ; move to qffi directory
       )
   #+allegro
   ,@'((:file "acl-ffi-fns") 
       (:file "acl-ffi-backend")
       ;;(:file "../allegro/foreign-vector") ; move to qffi directory
       )
   (:file "generic-ffi-fns")
   (:file "mixed-lisp-system")
   ))
