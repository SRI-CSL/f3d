(in-package :cl-user)

(asdf:defsystem :cl-cme-lcl 
  :serial t 
  :components
  ((:file "lcl-pkg")
   (:file "generic-lcl-fns")
   #+cmu     (:file "cmucl-lcl-fns")
   #+allegro (:file "acl-lcl-fns")
   #+sbcl (:file "sbcl-lcl-fns")
   ))
