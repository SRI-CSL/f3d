(in-package :math)

;;;             MULTI-DIMENSIONAL POLYNOMIAL FITTING 

;;; Primarily in support of fitting polynomials to geographic transforms


(defun make-to-from-vectors-with-random-sampling (transform-fn bounding-box n-random
						  &key to-bbox &allow-other-keys)
  (loop with bad-cnt fixnum = 0
	with good-cnt fixnum = 0
	with n-dims fixnum = (ash (length bounding-box) -1)
	for i fixnum from 0
	while (< good-cnt n-random)
	while (< bad-cnt (* 10 n-random))
	for from-vector 
	;; compute a uniformally distributed random point in the bounding-box.
	  = (let ((from-vector (make-coordinate-vector n-dims)))
	      (declare (type (simple-array double-float (*)) from-vector ))
	      (loop for dim fixnum from 0 below n-dims
		    for bbox-index from 0 by 2
		    do (setf (aref from-vector dim)
			     (random-in-range (aref bounding-box bbox-index)
					      (aref bounding-box (1+ bbox-index) ))))
	      from-vector)
	for to-vector = (funcall transform-fn from-vector)
	  
	when (and to-vector (or (null to-bbox) (point-inside-bbox to-vector to-bbox)))
	  collect from-vector into from-vectors
	  and collect to-vector into to-vectors
	  and do (incf good-cnt)
	else do (incf bad-cnt)
	finally (return (values from-vectors to-vectors bad-cnt))))


(defun make-to-from-vectors-with-uniform-sampling
    (transform-fn bounding-box n-grid &key to-bbox &allow-other-keys)
  (let* ((n (apply '* n-grid))
	 (n-dims (length n-grid))
	 (index (make-array n-dims :element-type 'fixnum))
	 (ns (make-array n-dims :element-type 'fixnum))
	 (deltas (make-array n-dims :element-type 'double-float))
	 (starts (make-array n-dims :element-type 'double-float))
	 (pos (make-array n-dims :element-type 'double-float)))
    (declare (type (simple-array double-float (*)) starts deltas pos ))
    (declare (type (simple-array fixnum (*)) index ns ))
    (loop for i fixnum from 0 below n-dims
	  for bbox-index from 0 by 2
	  for ni = (elt n-grid i)
	  for start double-float = (aref bounding-box bbox-index)
	  do (setf (aref deltas i)
		   (if (> ni 1)
		       (/ (- (aref bounding-box (1+ bbox-index) ) start ) (1- ni))
		       0.0)
		   (aref starts i) start
		   (aref pos i) start
		   (aref ns i) ni
		   (aref index i) ni))
	 
    (loop with bad-count fixnum = 0
	  for i fixnum from 0 below n
	  for from-vector ;; compute the next grid point in the N-DIMS dimensional space.
	    = (let ((from-vector (make-coordinate-vector n-dims)))
		(declare (type (simple-array double-float (*)) from-vector  ))
		(loop with carry = t
		      for dim fixnum from 0 below n-dims
		      do (setf (aref from-vector dim) (aref pos dim ) )
		      when carry
			do (incf (aref pos dim) (aref deltas dim))
			   (if (= 0 (the fixnum (setf (aref index dim)
						      (the fixnum
							(1- (the fixnum (aref index dim)))))))
			       (setf (aref index dim ) (aref ns dim )
				     (aref pos dim) (aref starts dim))
			       (setq carry nil)))
		from-vector)
	  for to-vector = (funcall transform-fn from-vector)
	  when (and to-vector (or (null to-bbox) (point-inside-bbox to-vector to-bbox)))
	    collect from-vector into from-vectors
	    and collect to-vector into to-vectors
	  else do (incf bad-count)
	  finally 
       (when (> bad-count 0)
	 (format t ";;; warning: make-to-from-vectors-with-uniform-sampling could not transform ~d points~%"
		 bad-count))
       (return (values from-vectors to-vectors bad-count)))))

(defun make-to-from-vectors-with-specified-sampling (transform-fn from-bbox sampling 
						     &rest args &key to-bbox &allow-other-keys )
  (declare (ignore to-bbox))
  (case (car sampling)
    (:uniform (apply #'make-to-from-vectors-with-uniform-sampling 
		     transform-fn from-bbox (cdr sampling) args))
    (:random (apply #'make-to-from-vectors-with-random-sampling 
		    transform-fn from-bbox (cadr sampling) args))))



;;; Threshold for suppression of singular values in SVD.
(defparameter *linear-projective-fit-sv-rel-threshold* 1e-8)

(defparameter *fit-linear-projective-polynomial-to-from-to-vectors-solver* :SVD)
  
(defun ls-solve (c-mat rhs-vect &key (solver :lu))
  (let ((coeffs (case solver
		  (:svd ;; Solve using singular-value-decomposition.
		   (svd-solve c-mat rhs-vect nil *linear-projective-fit-sv-rel-threshold*))
		
		  ;; Solve using Gauss-elimination with pivoting.
		  (otherwise (lu-solve c-mat rhs-vect)))))
    (values coeffs
	    (least-squares-residuals c-mat coeffs rhs-vect))))


#|
Solve for the linear projection parameters c0-c10 to minimize the sum of 

    ((c0*xi + c1*yi + c2*zi +c3) - ui*(c8*xi +c9*yi +c10*zi + 1))^2
  + ((c4*xi + c5*yi + c6*zi +c7) - vi*(c8*xi +c9*yi +c10*zi + 1))^2

   where <ui,vi> are the components of to-vector[i]
   and <xi,yi,zi> are the components of from-vector[i].

The resulting 4x4-projection matrix is:

   u = (c0*x + c1*y + c2*z +c3) / (c8*x +c9*y +c10*z + 1)

   v = (c4*x + c5*y + c6*z +c7) / (c8*x +c9*y +c10*z + 1)

|#

(declaim (special *SVD-ERRS*))

;;; broken
(defun fit-linear-projective-polynomial-to-from-to-vectors 
    (from-vectors to-vectors 
     &key (solver *fit-linear-projective-polynomial-to-from-to-vectors-solver*))
  (let* ((n-pts (length from-vectors))
	 (total-observations (* 2 n-pts))
	 (total-coeffs 11)
	 (rhs-vect (make-array0 total-observations :element-type 'double-float))
	 (c-mat (make-array0 (list total-observations total-coeffs) :element-type 'double-float)))
    (declare (type (simple-array double-float (*))  rhs-vect)
	     (type (simple-array double-float (* *)) c-mat))
    (macrolet ((set-row-coeffs (row &rest coeffs)
		 `(setf ,@(loop for coeff in coeffs 
				for i from 0
				collect `(aref c-mat ,row ,i) 
				collect coeff))))
      (loop for pt-index fixnum from 0 below n-pts
	    for pt-index0 fixnum from 0 by 2
	    for pt-index1 fixnum from 1 by 2
	    do (bind-vector-elements (x y z) (elt from-vectors pt-index)
		 (bind-vector-elements (u v) (elt to-vectors pt-index)
		   (set-row-coeffs pt-index0 x y z 1.0 (- (* u x)) (- (* u y)) (- (* u z)))
		   (set-row-coeffs pt-index1 x y z 1.0 (- (* v x)) (- (* v y)) (- (* v z)))
		   (setf (aref rhs-vect pt-index0) u
			 (aref rhs-vect pt-index1) v)))))
    (ls-solve c-mat rhs-vect :solver solver)))

;;; Least-squares fit a linear-projective transformation (homogeneous 4x4-matrix) to map
;;; the from-vectors into the to-vectors.
;;; This is sometimes called DIRECT LINEAR TRANSFORM
;;; THIS DOES NOT FIT THE W COMPONENT OF THE PROJECTION.
(defun fit-linear-projective-polynomial-to-from-to-vectors 
    (from-vectors to-vectors 
     &key (solver *fit-linear-projective-polynomial-to-from-to-vectors-solver*)
     &allow-other-keys)
  (let* ((n-pts (length from-vectors))
	 (total-observations (* 2 n-pts))
	 (total-coeffs 11)
	 (rhs-vect (make-array0 total-observations :element-type 'double-float))
	 (c-mat (make-array0 (list total-observations total-coeffs) :element-type 'double-float)))
    (declare (type (simple-array double-float (*))  rhs-vect)
	     (type (simple-array double-float (* *)) c-mat))
    (macrolet ((set-row-coeffs (row col &rest coeffs)
		 `(setf ,@(loop for coeff in coeffs 
				for i from 0
				collect `(aref c-mat ,row (+ ,col ,i))
				collect coeff))))
      (loop for pt-index fixnum from 0 below n-pts
	    for pt-index0 fixnum from 0 by 2
	    for pt-index1 fixnum from 1 by 2
	    do (bind-vector-elements (x y z) (elt from-vectors pt-index)
		 (bind-vector-elements (u v) (elt to-vectors pt-index)
		   (set-row-coeffs pt-index0 0 x y z 1.0) 
		   (set-row-coeffs pt-index0 8 (- (* u x)) (- (* u y)) (- (* u z)))
		   (set-row-coeffs pt-index1 4 x y z 1.0)
		   (set-row-coeffs pt-index1 8 (- (* v x)) (- (* v y)) (- (* v z)))
		   (setf (aref rhs-vect pt-index0) u
			 (aref rhs-vect pt-index1) v)))))
    (ls-solve c-mat rhs-vect :solver solver)))

(defmacro set-row-coeffs (row col &rest coeffs)
  `(setf ,@(loop for i from 0
		 for var in '(x y z 1.0)
		 collect `(aref c-mat ,row (+ ,col ,i))
		 collect  var)

	 ,@(loop for coeff in coeffs 
		 for i from 0
		 collect `(aref c-mat ,row ,i) 
		 collect coeff)))

;;; This version allow for an arbitrary number of components in the to-vectors,
;;; all involved in the linear-projective fit.
#+never ; untested. 
(defun fit-linear-projective-polynomial-to-from-to-vectors 
    (from-vectors to-vectors 
     &key (n-to-dims 3) (solver *fit-linear-projective-polynomial-to-from-to-vectors-solver*))
  (let* ((n-pts (length from-vectors))
	 (total-observations (* n-to-dims n-pts))
	 (total-coeffs (+ 3 (* 4 n-to-dims)))
	 (rhs-vect (make-array0 total-observations :element-type 'double-float))
	 (c-mat (make-array0 (list total-observations total-coeffs) :element-type 'double-float)))
    (declare (fixnum n-pts n-to-dims  total-observations total-coeffs))
    (declare (type (simple-array double-float (*))  rhs-vect)
	     (type (simple-array double-float (* *)) c-mat))
    (macrolet ((set-row-coeffsx (row col &rest coeffs)
		 `(setf ,@(loop for i from 0
				for var in '(x y z 1.0)
				collect `(aref c-mat ,row (+ ,col ,i))
				collect  var)

			,@(loop for coeff in coeffs 
				for i from 0
				collect `(aref c-mat ,row ,i) 
				collect coeff))))
      (loop for from-vector in from-vectors
	    for to-vector in to-vectors
	    for coeff-index fixnum from 0 by n-to-dims
	    do
	 (bind-vector-elements (x y z) from-vector
	   (loop for i from 0 below n-to-dims
		 for col fixnum from 3 by 4
		 for coeff-index-i fixnum from coeff-index
		 for q double-float = (aref to-vector i) ; one of (u v w)
		 do (set-row-coeffs coeff-index-i col (- (* q x)) (- (* q y)) (- (* q z)))
		    (setf (aref rhs-vect coeff-index-i) q)))))
    (ls-solve c-mat rhs-vect :solver solver)))


(defparameter *4x4-projection-matrix-from-projective-coefficients-orthographic-threshold* 1e-6)

;;; Returns a 4x4-matrix compatible with 4X4-PROJECTION. 
(defun 4x4-projection-matrix-from-projective-coefficients (coeffs)
  (declare (type (coordinate-vector 11) coeffs))
  (let ((ortho-p 
	 (< (euclidean-length (aref coeffs 8) (aref coeffs 9) (aref coeffs 10)) 
	    *4x4-projection-matrix-from-projective-coefficients-orthographic-threshold*)))
    (make-and-fill-4x4-matrix 
     (aref coeffs 0) (aref coeffs 1) (aref coeffs 2) (aref coeffs 3)
     (aref coeffs 4) (aref coeffs 5) (aref coeffs 6) (aref coeffs 7)
     ;; 3rd row is coeffs for z-buffer calculations (ie. z')
     0.0 0.0 (if ortho-p 1.0 0.0) (if ortho-p 0.0 -1.0) ; FIXME? is this right?
     (aref coeffs 8) (aref coeffs 9) (aref coeffs 10) 1.0)))

(defun fit-4x4-projection-matrix-to-from-to-vectors 
    (from-vectors to-vectors &key (solver *fit-linear-projective-polynomial-to-from-to-vectors-solver*))
  (mv-bind (coeffs resids)
      (fit-linear-projective-polynomial-to-from-to-vectors from-vectors to-vectors :solver solver)
    (values (4x4-projection-matrix-from-projective-coefficients coeffs)
	    resids)))


#|
Let from-dims be the number components of from-vectors
    to-dims be the number components of to-vectors

For each component-index i of the to-vectors (ie, i from 0 below to-dims)
solve for the linear transform matrix M[n,m] to minimize the sum

      sum(j) (sum(k, M[i,k] from-vector[j][k]) - to-vector[j][i])^2


The resulting 4x4-projection matrix is:

   u = (c0*x + c1*y + c2*z +c3) / (c8*x +c9*y +c10*z + 1)

   v = (c4*x + c5*y + c6*z +c7) / (c8*x +c9*y +c10*z + 1)

|#
(defparameter *fit-linear-polynomial-to-from-to-vectors-solver* :lu)
;(setq *fit-linear-polynomial-to-from-to-vectors-solver* :svd)

;;; Least-squares fit a linear-transforms (homogeneous 4x4-matrix) to map
;;; the from-vectors into the to-vectors.
;;; This is sometimes called DIRECT LINEAR TRANSFORM
;;;(defun fit-linear-polynomial-to-from-to-vectors 
;;;    (from-vectors to-vectors 
;;;     &key (solver *fit-linear-polynomial-to-from-to-vectors-solver*))
;;;  (let* ((n-pts (length from-vectors))
;;;         (to-dims (length (elt to-vectors 0)))
;;;         (from-dims (length (elt from-vectors 0)))
;;;         (rhs-vect (make-array n-pts :element-type 'double-float))
;;;         (c-mat (make-array (list n-pts (1+ from-dims)) :element-type 'double-float))
;;;         (mat (make-array (list to-dims (1+ from-dims)) :element-type 'double-float)))
;;;    (declare (type (simple-array double-float (*)) rhs-vect)
;;;             (type (simple-array double-float (* *)) mat c-mat))
;;;
;;;    (loop for i fixnum from 0 below to-dims
;;;          do (loop for k fixnum from 0 below n-pts
;;;                   for from-vector of-type coordinate-vector in from-vectors
;;;                   for to-vector of-type coordinate-vector in to-vectors
;;;                   do (loop for j fixnum from 0 below from-dims
;;;                            do (setf (aref c-mat k j) (elt from-vector j)))
;;;                      (setf (aref c-mat k from-dims) 1.0)
;;;                      (setf (aref rhs-vect k) (aref to-vector i)))
;;;          collect (mv-bind (soln resids) (ls-solve c-mat rhs-vect :solver solver)
;;;                    (loop for j fixnum from 0 to from-dims
;;;                          do (setf (aref mat i j) (elt soln j)))
;;;                    resids)
;;;            into resids
;;;          finally (return (values mat resids)))))

(defun fit-linear-polynomial-to-from-to-vectors 
    (from-vectors to-vectors 
     &key (solver *fit-linear-polynomial-to-from-to-vectors-solver*))
  (let* ((n-pts (length from-vectors))
	 (to-dims (length (elt to-vectors 0)))
	 (from-dims (length (elt from-vectors 0)))
	 (rhs-mat (make-array (list n-pts to-dims) :element-type 'double-float))
	 (c-mat (make-array (list n-pts (1+ from-dims)) :element-type 'double-float)))
    (declare (type (simple-array double-float (* *)) rhs-mat)
	     (type (simple-array double-float (* *)) c-mat))

    (loop for i fixnum from 0 below to-dims
	  do (loop for k fixnum from 0 below n-pts
		   for from-vector of-type coordinate-vector in from-vectors
		   for to-vector of-type coordinate-vector in to-vectors
		   do (loop for j fixnum from 0 below from-dims
			    do (setf (aref c-mat k j) (elt from-vector j)))
		      (setf (aref c-mat k from-dims) 1.0)
		      (setf (aref rhs-mat k i) (aref to-vector i))))
    
    (mv-bind (soln resids) (ls-solve c-mat rhs-mat :solver solver)
      ;; soln is a 4x3 matrix  more generally, a (1+ from-dims) x to-dims matrix
      (values (transpose-matrix soln) resids))))

#|
(st:autoload-system :terrain-models)

(defun tst (x i)
  (declare (ext:optimize-interface (speed 3)(safety 0)))
  (declare (optimize (speed 3)(safety 0)))
  (declare (type (simple-array fixnum (*)) x))
  (declare (fixnum i))
  (elt x i))
(disassemble 'tst)

(defun test-transform-closure (projection from-pt to-pt)
  (bind-vector-elements (u1 v1) to-pt
    (bind-vector-elements (u2 v2) (transforms::transform-vector projection from-pt)
      (max (abs (- u1 u2)) (abs (- v1 v2))))))

(defun transform-fn (transform)
  #'(lambda (from &optional to) (transforms::transform-vector transform from to)))

(setq tm (get-prop (gui::3d-world (gui::top-view)) :terrain-model))
(mv-setq (*from-vects* *to-vects*) 
	 (make-to-from-vectors-with-uniform-sampling 
	  (transform-fn (cme::dtm-to-lvcs-transform tm))
	  (transforms::dtm-bbox (cme::dtm tm))
	  '(3 3 3)))


(length *from-vects*)

(mv-setq (*mat* *resids*) (fit-linear-polynomial-to-from-to-vectors *from-vects* *to-vects*))

(loop for i from 0 below (length *from-vects*)
      maximize (test-transform-closure (cme::dtm-to-lvcs-transform tm)
				       (elt *from-vects* i) (elt *to-vects* i)))

(funcall (transform-fn (cme::dtm-to-lvcs-transform tm)) (cv 0.0 0.0 0.0))



(mv-setq (*from-vects* *to-vects*) 
	 (make-to-from-vectors-with-uniform-sampling 
	  (transform-fn (list (cme::dtm-to-lvcs-transform tm) 
			      (gui::3d-to-2d-projection (gui::top-view))))
	  (transforms::dtm-bbox (cme::dtm tm))
	  '(3 3 3)))

(mv-setq (*from-vects* *to-vects*) 
	 (make-to-from-vectors-with-uniform-sampling 
	  (transform-fn (gui::3d-to-2d-projection (gui::top-view)))
	  (obj::transform-bounding-box (cme::dtm-to-lvcs-transform tm)
				       (transforms::dtm-bbox (cme::dtm tm)))
	  '(3 3 3)))

	

(mv-setq (*mat* *resids*) 
	 (fit-linear-projective-polynomial-to-from-to-vectors *from-vects* *to-vects* :solver :lu
							      :n-to-dims 2))

(in-package :gui)

(transform-vector (3d-to-2d-projection (top-view)) (selected-object-world-position))
(transforms::projection-matrix (3d-to-2d-projection (top-view)))

|#
