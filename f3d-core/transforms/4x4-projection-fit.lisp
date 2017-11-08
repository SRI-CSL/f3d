(in-package :transforms)

(eval-when (eval load compile)
(import '(math::fit-linear-projective-polynomial-to-from-to-vectors
	  math::make-to-from-vectors-with-specified-sampling
	  math::4X4-PROJECTION-MATRIX-FROM-PROJECTIVE-COEFFICIENTS))
)

#|

Code to support SURROGATE-PROJECTIONS which are 4x4-projections derived from
projections than cannot be represented as homogeneous 4x4-matrices
by least-squares fitting over a bounding volume.

SURROGATE-PROJECTIONS are needed in order to use OpenGL rendering.

|#




;;; Return a function (actually a closure) that maps from-vector = <u v z> into
;;; world coordinates <x y z> by inverting PROJECTION, ie. intersecting the
;;; camera ray at <u v> with the specified z-plane.
(defun make-project-to-world (projection &optional 3d-transform)
  #'(lambda (from-vector &optional to-vector)
      (setq to-vector
	    (if to-vector
		(copy-coordinate-vector
		 (intersect-camera-ray-with-z-plane projection from-vector (aref from-vector 2))
		 to-vector)
		(intersect-camera-ray-with-z-plane projection from-vector (aref from-vector 2))))
      (when to-vector
	(when 3d-transform (transform-vector 3d-transform to-vector to-vector))
	;;(break)
	to-vector)))

;(trace intersect-camera-ray-with-z-plane)
(defun make-project-to-world (projection &optional 3d-transform starting-approx)
  #'(lambda (2d-pt &optional 3d-pt)
      (let ((3d-pt2 (intersect-camera-ray-with-z-plane projection 2d-pt (aref 2d-pt 2) starting-approx)))
	(when 3d-transform (transform-vector 3d-transform 3d-pt2 3d-pt2))
	(if 3d-pt
	    (copy-coordinate-vector 3d-pt2 3d-pt)
	    3d-pt2))))

;;; Fit a linear-projective-polynomial to PROJECTION using fit-points uniformly distributed in u-v-h space.
;;; UVH-BBOX specifies the bounding box in u-v-h space for the fit-points.
;;; N-LIST specifies the number of fit-points in each of the u-v-h dimensions.
;;; NEAR-FAR specifies the near and far parameters for the resulting 4x4-projection.
;;; PPLANE is ignored.
;;;(defun fit-4x4-projection-to-2d-to-3d-projection (projection uvh-bbox n-list
;;;                                                  &optional (near-far nil) (pplane (cv 0.0 0.0 1.0 0.0)))
;;;  (ignore pplane)
;;;  (multiple-value-bind (to-vectors from-vectors)
;;;      ;; compute the fit-points with uniform sampling in u-v-h space.
;;;      (make-to-from-vectors-with-uniform-sampling (make-project-to-world projection) uvh-bbox n-list)
;;;    (let* ((coeffs (fit-linear-projective-polynomial-to-from-to-vectors from-vectors to-vectors))
;;;           (pmat (make-and-fill-4x4-matrix (aref coeffs 0) (aref coeffs 1) (aref coeffs 2) (aref coeffs 3)
;;;                                           (aref coeffs 4) (aref coeffs 5) (aref coeffs 6) (aref coeffs 7)
;;;                                           ;;(aref coeffs 8) (aref coeffs 9) (aref coeffs 10) 0.0
;;;                                           ;; 3rd row is coeffs for z-buffer calculations (ie. z')
;;;                                           0.0 0.0 0.0 -1.0
;;;                                           (aref coeffs 8) (aref coeffs 9) (aref coeffs 10) 1.0)))
;;;      (multiple-value-bind (3d-to-camera-matrix internal-params camera-to-2d-matrix)
;;;          (cme::decompose-projection-matrix-old pmat :1/f-threshold 1e-6 :roff 1.0)
;;;        (declare (ignorable camera-to-2d-matrix))
;;;        (let* ((4x4-projection (if nil
;;;                               (apply 'make-perspective-transform
;;;                                      :camera-to-3d-matrix (invert-matrix 3d-to-camera-matrix)
;;;                                      :camera-to-image-matrix camera-to-2d-matrix
;;;                                      near-far)
;;;                               (apply 'make-perspective-transform
;;;                                      :camera-to-3d-matrix (invert-matrix 3d-to-camera-matrix)
;;;                                      (append internal-params near-far)))))
;;;          ;;(setq *foo* (list pmat 4x4-projection 3d-to-camera-matrix internal-params camera-to-2d-matrix))
;;;          ;; Do we always want to do this?
;;;          (setf (get-prop projection :SURROGATE-PROJECTION) 4x4-projection)    
;;;          (values 4x4-projection
;;;                  from-vectors
;;;                  to-vectors))))))

;(describe *pmat*)
;(fmakunbound 'fit-4x4-projection-to-2d-to-3d-projection)
(defun fit-4x4-projection-to-2d-to-3d-projection (projection uvh-bbox 
                                                  &rest args &key 
						  (sampling '(:uniform 5 5 5))
						  (near-far nil) starting-approx
						  xyz-bbox (pplane (cv 0.0 0.0 1.0 0.0)))
  (declare (ignore pplane))
  (multiple-value-bind (uvh-vectors xyz-vectors)
      ;; compute the fit-points with uniform sampling in u-v-h space.
      (apply #'make-to-from-vectors-with-specified-sampling 
	     (make-project-to-world projection nil starting-approx)
	     uvh-bbox sampling :to-bbox xyz-bbox args)
    ;;(setq *foo* (list projection xyz-bbox uvh-bbox uvh-vectors xyz-vectors ))
    (let* ((coeffs (fit-linear-projective-polynomial-to-from-to-vectors xyz-vectors uvh-vectors))
           (pmat (4x4-projection-matrix-from-projective-coefficients coeffs)))
      (multiple-value-bind (3d-to-camera-matrix camera-to-2d-matrix)
          (decompose-projection-matrix pmat)
	(let* ((frame-camera (make-instance 'frame-camera 
					    :3d-to-camera-matrix 3d-to-camera-matrix
					    :camera-to-2d-matrix camera-to-2d-matrix)))
	 ; (set-near-far frame-camera near-far)
	  ;;(setq *foo* (list pmat frame-camera 3d-to-camera-matrix internal-params camera-to-2d-matrix))
	  ;; Do we always want to do this?
	  (setf (get-prop projection :SURROGATE-PROJECTION) frame-camera)    
	  (values frame-camera
		  xyz-vectors uvh-vectors))))))

;(fmakunbound 'fit-4x4-projection-to-3d-to-2d-projection)
(defun fit-4x4-projection-to-3d-to-2d-projection (projection xyz-bbox 
						  &rest args &key 
						  (sampling '(:uniform 5 5 5))
						  uvh-bbox (pplane (cv 0.0 0.0 1.0 0.0)))
  (declare (ignore pplane))
  (multiple-value-bind (xyz-vectors uvh-vectors)
      (apply #'make-to-from-vectors-with-specified-sampling 
	     #'(lambda (pt) (transform-vector projection pt))
	     xyz-bbox sampling :to-bbox uvh-bbox args)
    ;;(setq *foo* (list projection xyz-bbox uvh-bbox xyz-vectors uvh-vectors ))
    (let* ((coeffs (fit-linear-projective-polynomial-to-from-to-vectors xyz-vectors uvh-vectors))
	   (pmat (4x4-projection-matrix-from-projective-coefficients coeffs)))
      (values (make-instance '4x4-projection :projection-matrix pmat)
	      xyz-vectors uvh-vectors))))


;;;(defun fit-linear-projective-polynomial-to-transform (transform bounding-box sampling)
;;;  (multiple-value-bind (from-vectors to-vectors)
;;;      (make-to-from-vectors-with-specified-sampling transform bounding-box sampling )
;;;    (let ((coeffs
;;;           (fit-linear-projective-polynomial-to-from-to-vectors from-vectors to-vectors)))
;;;     coeffs
;;;      )))


(defun fit-linear-projective-polynomial-to-transform (transform bounding-box sampling)
  (multiple-value-bind (to-vectors from-vectors)
      (make-to-from-vectors-with-specified-sampling 
       (make-project-to-world transform) bounding-box sampling)
    (let ((coeffs
	   (fit-linear-projective-polynomial-to-from-to-vectors from-vectors to-vectors)))
      coeffs
      )))

