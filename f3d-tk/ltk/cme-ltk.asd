;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package #:cl-user)

(asdf:defsystem :cme-ltk
  :name "cme-ltk"
  :depends-on (:f3d-tk)
  :maintainer "Chris Connolly, SRI International <connolly@ai.sri.com>"
  :license "Mozilla"
  :description "Ltk defs in asdf"
  :serial t
  :components 
  ((:file "ltk-pkg")
   (:file "ltk-lisptk")
   (:file "ltk-generic")
   (:file "ltk-tile")
   (:file "ltk-cvv")
   ))
           
