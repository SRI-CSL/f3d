(in-package :cl-user)

(defvar config::*math-without-foreign-code* nil)

(defvar config::*math-transforms-support-only* nil)

(defun config::math-subsystem-files (&key
				     (no-ffi config::*math-without-foreign-code*)
				     (transforms-support-only config::*math-transforms-support-only*))
  `("math-pkg.lisp"
    "vectors.lisp"
    "matrices.lisp"
    "bounding-boxes.lisp"
    ,@(if no-ffi
	  '("math-ffi-replacement.lisp")
	  '("math-ffi.lisp"))
    "transform-matrix.lisp"
    "rq-decomposition.lisp"
    ,@(unless transforms-support-only
	'("geometry.lisp"
	  "quaternions.lisp"
	  "generic-arith.lisp"
	  "polynomial-fit.lisp"
	  "polynomial-fit-1d.lisp"
	  ))))

(defvar config::modular-math t)

(if config::modular-math

(st:define-system :math
    ;;:system-class 'st::mixed-lisp-system
    :libraries '("libf3dmath")
    ;;:required-systems '(common-symbols qffi  libf3derror)
    :required-systems '(common-symbols qffi )
    :files (config::math-subsystem-files)) 

(st:define-system :math
    :required-systems (if config::*math-without-foreign-code*
			  '(common-symbols lisp-extensions)
			  '(common-symbols qffi lisp-extensions libfreedius))
    ;; needs some macros: mv-setq ... from lisp-extensions
    :files (config::math-subsystem-files)) 

) ; end config::modular-math 