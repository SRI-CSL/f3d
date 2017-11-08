(in-package :cl-user)

;;; FIXME:  eliminate the need for the image subsystem

(st:define-system "rpc-projections"
    :required-systems '(common-symbols math transforms geographic-transforms 
					; image
			)	       
    :files '("rpc-macros.lisp"
	     "polynomial-transforms.lisp"
	     "rational-polynomial.lisp"
	     "rpc-projection.lisp"
	     "dppdb-rpc.lisp"
	     "rpc00a.lisp"
	     "rpc-zerop.lisp"
	   ))


#|
Notes:  

There are dependencies on 2D-WORLDS

(LOAD-SYSTEM :RPC-PROJECTIONS)  without all of FREEDIUS:
Warning: These functions are undefined:
  COMMON-SYMBOLS:3D-TO-2D-PROJECTION TRANSFORMS::CHANGE-PROJECTION TRANSFORMS:LVCS-TO-LAT-LONG-TRANSFORM
   TRANSFORMS::MAKE-CONNECTIONS

Some functions have been commented out with #+NEVER in order to remove
dependencies on undefined classes.

|#