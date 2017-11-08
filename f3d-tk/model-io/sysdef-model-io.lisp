(in-package :cl-user)

;;; This depends on many of the subsystems of CME being defined.


(st:define-system :model-io
    :required-systems '(transforms gl-objects 
			;cme 
			;radius-classes  ;RADIUS-CLASSES depends on the CME package 
			)
    :files '("io-pkg.lisp"
	     "io-forms.lisp"
	     "transform-io.lisp"
	     ;;"object-io.lisp"
	     "model-input.lisp"
	     "pprint-object.lisp"

	     ;; Note: I am importing the unique-object-id code into
	     ;; Sentient.  For now, Sentient UID generators will rely
	     ;; largely on the time of object creation down to the
	     ;; millisecond level, along with type and reference
	     ;; information, so these are very unlikely to collide.  I
	     ;; view these generators as trivial straw-men to be
	     ;; replaced by something more robust and
	     ;; comprehensive. -CC

	    ;; "unique-object-id.lisp"  ; this isn't yet used - may be part of another system
	     ))
