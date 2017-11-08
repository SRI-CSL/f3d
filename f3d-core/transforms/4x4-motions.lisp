(in-package :transforms)

;;; ************************  4X4 LINEAR TRANSFORM METHODS  ************************

;;; Should the remainder of this file move to transforms file ?

(defmethod set-origin ((tmat array) vector)
  (declare (type (simple-array double-float (*)) vector))
  (declare (type 4x4-matrix tmat))
  (setf (aref tmat 0 3) (aref vector 0)
	(aref tmat 1 3) (aref vector 1)
	(aref tmat 2 3) (if (>= (length vector) 3) (aref vector 2) 0.0)))

(defmethod origin ((mat array))
  (declare (type 4x4-matrix mat))
  (cv (aref mat 0 3) (aref mat 1 3) (aref mat 2 3)))

;;; delta-position is in to-coordinate-system.
(defmethod move-by ((tmat array) delta-position)
  (declare (type 4x4-matrix tmat))
  ;;(unless (= (length delta-position) 3) (error "lose"))
  (bind-vector-elements (dx dy dz) delta-position
    (incf (aref tmat 0 3) dx)
    (incf (aref tmat 1 3) dy)
    (when (>= (length delta-position) 3)
      (incf (aref tmat 2 3) dz))))

(defmethod transform-window-motion-to-2d-world-motion
	   ((2d-to-window-transform array) 
	    window-motion-vector &optional into-vector)
  (let ((object-position nil))
    ;; for 2d-to-window-transform other than linear-transforms, object-position
    ;; must be calculated as
    ;; (inverse-transform-vector 2d-to-window-transform world-position)
    ;; I see absolutely no reason for a non-linear transform here
    (transform-direction-vector
     (inverse-transform 2d-to-window-transform)
     window-motion-vector object-position into-vector) ))

;;; Rotation relative to to-coordinate-system, where
;;; center-of-rotation-vector is in to-coordinate-system.
(defmethod rotate-relative-to-world-by
	   ((coordinate-transform array) orientation-spec
	    &optional center-of-rotation-vector)
  (pre-multiply-transform-matrix
   coordinate-transform
   orientation-spec
   :center-of-rotation-vector center-of-rotation-vector))

;;; Rotation relative to from-coordinate-system with
;;; center-of-rotation-vector is in to-coordinate-system
(defmethod rotate-by ((transform array) orientation-spec
		      &optional center-of-rotation-vector)
  (post-multiply-transform-matrix
   transform
   orientation-spec
   :center-of-rotation-vector center-of-rotation-vector))

(defmethod pre-multiply ((transform array) matrix
		      &optional center-of-rotation-vector)
  (pre-multiply-transform-matrix
   transform
   matrix
   :center-of-rotation-vector center-of-rotation-vector))

(defmethod post-multiply ((transform array) matrix
		      &optional center-of-rotation-vector)
  (post-multiply-transform-matrix
   transform
   matrix
   :center-of-rotation-vector center-of-rotation-vector))

(defun rotation-spec-to-matrix (rotation-spec)
  (cond ((arrayp rotation-spec)
	 rotation-spec)
	((and (arrayp (car rotation-spec))
	      (= 2 (array-rank (car rotation-spec))))
	 (car rotation-spec))
	(t (apply #'math::make-object-to-parent-orientation-matrix rotation-spec))))

;;; M1x = Rx+T, M2x = Q.(Rx+T)+T2
;;; want V = Rx+T = Q.(Rx+T)+T2,      where x = Rtranspose*(V-T)
;;; T2 = V - Q.(Rx+T) = V - Q.V
;;;  where M1 = matrix, M2 = result matrix,
;;; Q = rotation, V = center-of-rotation-vector
;;; center-of-rotation-vector is in coordinates of to-coordinate-system

(defmethod pre-multiply-transform-matrix ((matrix array)
					  rotation-spec
					  &key center-of-rotation-vector
					  into-matrix
					  ;;(into-matrix matrix)
					  )
  (declare (type (or null (simple-array double-float (*))) center-of-rotation-vector))
  (declare (type 4x4-matrix matrix ))
  (declare (type (or null 4x4-matrix) into-matrix ))
  (unless into-matrix (setq into-matrix matrix))
  (let ((rotation (rotation-spec-to-matrix rotation-spec)))
    (declare (type 4x4-matrix rotation))
    (unless center-of-rotation-vector
      (setq center-of-rotation-vector
	    (cv (aref matrix 0 3) (aref matrix 1 3) (aref matrix 2 3))))
    (setq into-matrix (math:multiply-matrices rotation matrix into-matrix))
    (bind-vector-elements (qvx qvy qvz)
	(4x4-matrix-times-vector rotation center-of-rotation-vector)
      (bind-vector-elements (vx vy vz) center-of-rotation-vector
	;; column 3 now contains Q.T.  Here we add T2 = V - Q.V
	(incf (aref into-matrix 0 3) (- vx qvx))
	(incf (aref into-matrix 1 3) (- vy qvy))
	(incf (aref into-matrix 2 3) (- vz qvz))
	into-matrix))))

;;; M1x = Rx+T, M2x = (RQx+T)+T2 = Q2.(Rx+T)+T3
;;; must have Q2R = RQ, hence Q2 = RQinv(R)
(defmethod post-multiply-transform-matrix ((matrix array)
					   rotation-spec
					   &key center-of-rotation-vector
					   (into-matrix matrix))
  (let ((rotation (rotation-spec-to-matrix rotation-spec)))
    (pre-multiply-transform-matrix
     matrix
     (math:multiply-matrices (math:multiply-matrices matrix rotation)
			     (invert-matrix matrix)) 
     :center-of-rotation-vector center-of-rotation-vector
     :into-matrix into-matrix)))

;;; Position is location in 3d-world at which jacobian of projection is computed.
;;; du dy are rotation angles in radians.
(defmethod rolling-ball-rotation-matrix (projection position du dv )
  (let* ((jacobian (transform-jacobian projection position))
	 (ray-dir (vector-cross-product
		   (cv (aref jacobian 1 0) (aref jacobian 1 1) (aref jacobian 1 2))
		   (cv (aref jacobian 0 0) (aref jacobian 0 1) (aref jacobian 0 2))))
	 (inv (invert-3x3-matrix jacobian))
	 (u-axis-projection (cv (aref inv 0 0) (aref inv 1 0) 0.0))
	 (v-axis-projection (cv (aref inv 0 1) (aref inv 1 1) 0.0)))
    (declare (type (simple-array double-float (3 3)) jacobian inv))
    (math:multiply-matrices
     (make-orientation-matrix
      (vector-cross-product u-axis-projection ray-dir)
      du)
     (make-orientation-matrix
      (vector-cross-product v-axis-projection ray-dir)
      dv)
     )))

;;; ***********************  MOTIONS OF 4X4-COORDINATE-TRANSFORM  ***************

(defmethod origin ((coordinate-transform 4x4-coordinate-transform))
  (origin (transform-matrix coordinate-transform)))

(defmethod set-origin ((coordinate-transform 4x4-coordinate-transform) vector)
  (set-origin (transform-matrix coordinate-transform) vector)
  (update-transform coordinate-transform))
   
(defmethod set-transform-matrix ((coordinate-transform 4x4-coordinate-transform) new-mat)
  (copy-matrix new-mat (transform-matrix coordinate-transform))
  (update-transform coordinate-transform)
  new-mat)

(defmethod move-to ((coordinate-transform 4x4-coordinate-transform)
		    new-origin &optional ignore)
  (ignore ignore)
  (set-origin coordinate-transform new-origin))

(defmethod move-by ((coordinate-transform 4x4-coordinate-transform) delta-origin)
  (prog1 (move-by (transform-matrix coordinate-transform) delta-origin)
    (update-transform coordinate-transform)))

(defmethod rotate-relative-to-world-by
	   ((coordinate-transform 4x4-coordinate-transform) orientation-spec
	    &optional center-of-rotation-vector)
  (prog1 (rotate-relative-to-world-by (transform-matrix coordinate-transform)
				      orientation-spec center-of-rotation-vector)
    (update-transform coordinate-transform)))

(defmethod rotate-by ((coordinate-transform 4x4-coordinate-transform) orientation-spec
		      &optional center-of-rotation-vector)
  (prog1 (rotate-by (transform-matrix coordinate-transform)
		    orientation-spec center-of-rotation-vector)
    (update-transform coordinate-transform)))

(defmethod pre-multiply ((coordinate-transform 4x4-coordinate-transform) matrix
		      &optional center-of-rotation-vector)
  (prog1 (pre-multiply (transform-matrix coordinate-transform)
		       matrix center-of-rotation-vector)
    (update-transform coordinate-transform)))

(defmethod post-multiply ((coordinate-transform 4x4-coordinate-transform) matrix
		      &optional center-of-rotation-vector)
  (prog1 (post-multiply (transform-matrix coordinate-transform)
		       matrix center-of-rotation-vector)
    (update-transform coordinate-transform)))

(defmethod pre-multiply-transform-matrix
	   ((coordinate-transform 4x4-coordinate-transform)
	    rotation-spec &key center-of-rotation-vector)
  (ignore center-of-rotation-vector)
  (prog1 (pre-multiply-transform-matrix
	  (transform-matrix coordinate-transform)
	  rotation-spec
	  :center-of-rotation-vector center-of-rotation-vector)
    (update-transform coordinate-transform)))
 
(defmethod post-multiply-transform-matrix
	   ((coordinate-transform 4x4-coordinate-transform)
	    rotation-spec &key center-of-rotation-vector)
  (ignore center-of-rotation-vector)
  (prog1 (post-multiply-transform-matrix
	  (transform-matrix coordinate-transform)
	  rotation-spec
	  :center-of-rotation-vector center-of-rotation-vector)
    (update-transform coordinate-transform)))

(defmethod transform-window-motion-to-2d-world-motion
	   ((2d-to-window-transform 4x4-coordinate-transform)
	    window-motion-vector &optional into-vector)
  (let ((object-position nil))
    ;; for 2d-to-window-transform other than linear-transforms, object-position
    ;; must be calculated as (inverse-transform-vector 2d-to-window-transform
    ;; world-position) I see absolutely no reason for a non-linear transform
    ;; here
    (prog1 (transform-direction-vector
	    (inverse-transform 2d-to-window-transform)
	    window-motion-vector object-position into-vector)
      ;; WHAT IS THIS FOR?  We are not changing the transform here!
      ;(update-transform 2d-to-window-transform)
      )))

#|
(transform-window-motion-to-2d-world-motion
 (2d-to-window-transform (top-view))
 (cv 1.0 0.0 0.0))
(transform-matrix (inverse-transform (2d-to-window-transform (top-view))))
(transform-matrix (2d-to-window-transform (top-view)))
|#
  
