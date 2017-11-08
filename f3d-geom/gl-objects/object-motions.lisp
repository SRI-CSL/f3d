(in-package :obj)

;;; This file should be renamed to object-motions.lisp.
;;; No direct dependencies on TK or OpenGL.

#|
This file implements methods to perform various spatial-object motions.

The methods defined here should not depend on GUI personality (look and feel).

|#


#|
(defmethod position-for-object-motion ((object gl-object) interactor)
  *center-of-object-rotation-coordinate-vector*)

|#

;;; bogus
(defmethod position-for-object-motion ((object gl-object))
  (origin object))

(defvar *zero-position-vector*  (cv 0.0 0.0 0.0))

(defmethod move-world-position
	   ((object basic-gl-object) delta-position &optional world)
  (move-by object (transform-direction-vector (world-to-object-transform world (parent object))
					      delta-position *zero-position-vector*)))

(defmethod move-object-position ((object basic-gl-object) delta-position)
  (move-by object delta-position))

(defmethod move-world-position ((vertex object-vertex) delta-position &optional world)
  (move-by vertex (transform-direction-vector (world-to-object-transform world (object vertex))
					      delta-position *zero-position-vector*)))

(defmethod move-object-position ((vertex object-vertex) delta-position)
  (move-by vertex delta-position))


;;; **********************************************************************************

;;; *****************  SPECIALIZED OBJECT CLASS METHODS  ***************************

;;; This method is not meaningful for 2d-objects
(defmethod compute-position-along-camera-ray
	   ((object gl-3d-object-mixin) position transform distance)
  (compute-position-along-ray
   object (- distance )
   (camera-direction-vector transform position )
   position ))

;;; This method is not meaningful for 2d-objects
(defmethod compute-position-along-ray
	   ((object gl-3d-object-mixin)
	    distance direction-vector &optional position)
  (unless position
    (setq position (selected-point-world-position object)))
  (let* ((length (vector-euclidean-length direction-vector))
	 (k (/ distance length)))
    (declare (double-float length k))
    (vector-linear-combine 1.0 position k direction-vector)))

;;; *****************  GENERIC OBJECT CLASS METHODS  ***************************

(defmethod set-origin ((object basic-gl-object) vector)
  (set-origin (object-to-parent-transform object) vector))

(defmethod origin ((object basic-gl-object))
  (origin (object-to-parent-transform object)))

(defmethod move-by ((object basic-gl-object) delta-position)
  ;;(format t "move-by ~a ~a ~a~%" object delta-position (origin object))
  (set-origin object (vector-add (origin object) delta-position nil)))

;;; delta-position is relative to parent in this version 
;;;(defmethod move-by ((vertex object-vertex) delta-position)
;;;  (let* ((object (object vertex))
;;;         (object-to-world (object-to-parent-transform object)))
;;;    (setf (fragment-position vertex)
;;;          (inverse-transform-vector object-to-world 
;;;                                    (vector-add
;;;                                     (transform-vector object-to-world (fragment-position vertex))
;;;                                     delta-position nil)))))

;;;delta-position is relative to object in this version 
(defmethod move-by ((vertex object-vertex) delta-position)
  (setf (fragment-position vertex) (vector-add (fragment-position vertex) delta-position nil)))

(defmethod rotate-by
    ((object basic-gl-object)
     orientation-spec
     &optional center-of-rotation-vector)
  (rotate-by (object-to-parent-transform object)
	     orientation-spec center-of-rotation-vector))

(defmethod scale-by
    ((object gl-2d-object-mixin)
     scale
     &optional center-of-rotation-vector)
  (transforms::pre-multiply  (object-to-parent-transform object)
			     (make-and-fill-4x4-matrix scale 0.0 0.0 0.0
				      0.0 scale 0.0 0.0
				      0.0 0.0 1.0 0.0
				      0.0 0.0 0.0 1.0)
			     center-of-rotation-vector))

(defmethod rotate-relative-to-world-by
	   ((object basic-gl-object)
	    orientation-spec
	    &optional center-of-rotation-vector)
  (rotate-relative-to-world-by (object-to-parent-transform object)
			       orientation-spec center-of-rotation-vector))

;;;(defmethod selected-point-world-position ((object basic-gl-object) &optional to-vector)
;;;  (with-slots (last-selected-handle object-to-world-transform) object
;;;    (if (typep last-selected-handle 'base-object)
;;;        (selected-point-world-position last-selected-handle to-vector)
;;;        (let ((sp (selected-point object)))
;;;          (when sp (vertex-transform sp object-to-world-transform to-vector))))))

(defmethod selected-point-world-position ((object basic-gl-object) &optional to-vector)
  (ignore to-vector)
  (origin object))

;;; rigid motion of OBJECT so that vertex-object-position is moved to POSITION.
(defmethod move-to ((object basic-gl-object) parent-position vertex-object-position)
  (when vertex-object-position
    (bind-vector-elements (x y z) parent-position ; where we want it to be
      (let ((object-to-parent-transform (object-to-parent-transform object)))
	(bind-vector-elements (ox oy oz) (origin object)
	  (let ((vertex-parent-position (transform-vector object-to-parent-transform
							 vertex-object-position)))
	    (bind-vector-elements (sx sy sz) vertex-parent-position ; where it is now
	      (set-origin
	       object (set-coordinate-vector-elements
		       vertex-parent-position
		       (+ ox (- x sx)) (+ oy (- y sy)) (+ oz (- (or z 0.0) (or sz 0.0))))
	       ;;(enforce-constraints object) ; FIXME: THIS SHOULD MOVE TO UPDATE-OBJECT
	       ))))))))
