;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Connolly's port of CME to asdf and quicklisp.

(in-package #:cl-user)

(defvar config::*math-without-foreign-code* nil)

(defvar config::*math-transforms-support-only* nil)

(defvar config::modular-math nil)

;; See sysdef-transforms.lisp for the original system-tool defns:

(defsystem cl-cme-transforms
    :depends-on (:lisp-extensions)
    :serial t 
    :components
    ((:file "math-pkg.lisp")
     (:file "vectors.lisp")
     (:file "matrices.lisp")
     (:file "bounding-boxes.lisp")
     (:file "math-ffi-replacement.lisp")
     (:file "transform-matrix.lisp")
     (:file "rq-decomposition.lisp")
     (:file "geometry.lisp")
     (:file "quaternions.lisp")
     (:file "generic-arith.lisp")
     (:file "polynomial-fit.lisp")
     (:file "polynomial-fit-1d.lisp")
     ))


