(in-package :cme)

;;; Nothing left of this file.

#|

The permitted values for write-lock are:

   :enable - Enable  overriding locks on superiors.
   T   - Disable 
   NIL - get write-lock from superior which is the default.

|#

#|
(defmethod write-locked-p ((object gl-object))
  (with-slots (parent feature-sets) object
    (let ((my-write-lock (write-lock object)))
      (and (not (eq my-write-lock :enable))
	   (or my-write-lock
	       (and (instancep composite-superior)  (write-locked-p composite-superior))
	       ;; Is this overly restrictive ??
	       (loop for fs in feature-sets
		     thereis (write-locked-p fs)))))))

(defmethod write-lock-error-message ((object basic-object) window &optional message)
  (ic::report-error-to-image-calc-top-level
   (format nil
	   "[~a is Write Locked for operation ~a]~%"
	   (or (loop for obj first object then (object-composite-superior obj)
		 while (instancep obj)
		 when (write-locked-p obj) return obj)
	       object)
	   message))
  (beep window)
  )
|#

;;; ***********************  DUMMYS FOR UNIMPLEMENTED OBJECT CLASSES  ***********************

;;; The following object classes are pretty much dummies to allow CME
;;; sites to load into Freedius without much fuss.  They will be dealt
;;; with later.
;;;

;(defstruct-class 2d-object (gl-2d-object-mixin basic-object) ())

;(defstruct-class 3d-object (gl-3d-object-mixin basic-object) ())



