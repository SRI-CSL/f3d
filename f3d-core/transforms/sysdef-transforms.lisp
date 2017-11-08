(in-package :cl-user)

(defvar CONFIG::WEAK-EVAL-CACHE nil)

;;; Sat Sep 11 2004 
;;; New feature enabling new API for coordinate-systems and transforms.
;;(push :FREEDIUS2-TRANSFORMS-API *features*)

(st:define-system "transforms"
    :required-systems '(common-symbols math)
    :files `("transforms-pkg.lisp"
	     "coordinate-transforms.lisp"
	     ,@(when config::weak-eval-cache '("weak-children-hash-table-mixin.lisp"))
	     "transform-path.lisp" 
	     "4x4-transform.lisp"
	     "4x4-projection.lisp"
	     "frame-camera.lisp"
	     "composite-transforms.lisp"
	     "4x4-motions.lisp"
	     "frame-camera-motions.lisp"
	     "numeric-inverse-transform.lisp"
	     ;;"transform-tools.lisp" ;; moved to the CME system due to package problems
	     "4x4-projection-fit.lisp"
	     "4x4-projection-decomposition.lisp"
	     "transform-fit.lisp"
             "temporal-transforms.lisp"
	     ))
