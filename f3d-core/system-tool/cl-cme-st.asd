(in-package :cl)

(asdf:defsystem :cl-cme-st
  :serial t 
  :components
  ((:file "boot")
   (:file "system-tool-bootstrap")
   (:file "system-tool")
   ))
