;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Connolly's port of CME to asdf and quicklisp.

(in-package #:cl-user)

(defsystem :cl-cme-transforms
    :depends-on (:common-symbols :cl-cme-math)
    :components
    ((:file "transforms-pkg.lisp")
     (:file "coordinate-transforms.lisp")
     (:file ,@(when config::weak-eval-cache '("weak-children-hash-table-mixin.lisp")))
     (:file "transform-path.lisp" )
     (:file "4x4-transform.lisp")
     (:file "4x4-projection.lisp")
     (:file "frame-camera.lisp")
     (:file "composite-transforms.lisp")
     (:file "4x4-motions.lisp")
     (:file "frame-camera-motions.lisp")
     (:file "numeric-inverse-transform.lisp")
     ;;"transform-tools.lisp" ;; moved to the CME system due to package problems)
     (:file "4x4-projection-fit.lisp")
     (:file "4x4-projection-decomposition.lisp")
     (:file "transform-fit.lisp")
     (:file "temporal-transforms.lisp")
     ))
