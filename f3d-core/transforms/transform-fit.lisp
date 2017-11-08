(in-package :math)

;;; Various math function needed by the TRANSFORMS package.

;;; These function cannot be part of the MATH subsystem because of dependencies on TRANSFORMS.


(defun make-4x4-transform-approximation (transform bbox &key (sampling '(:uniform 7 7 7)))
  (mv-bind (from-pts to-pts) 
      (make-to-from-vectors-with-specified-sampling
       #'(lambda (pt) (transforms::transform-vector transform pt))
       bbox
       sampling)
    (mv-bind (coeffs-mat resids)
	(fit-linear-polynomial-to-from-to-vectors from-pts to-pts)
      ;; coeffs-mat is only a 3x4 matrix 
      (let ((4x4-matrix (make-4x4-identity-matrix)))
	(loop for row from 0 below 3
	      do (loop for col from 0 below 4
		       do (setf (aref 4x4-matrix row col) (aref coeffs-mat row col)))) 
;	(setq *foo* (list 4x4-matrix resids))
	(values (transforms::make-4x4-coordinate-transform 
		 4x4-matrix
		 :from-coordinate-system (transforms::from-coordinate-system transform)
		 :to-coordinate-system (transforms::to-coordinate-system transform))
		resids)))))
