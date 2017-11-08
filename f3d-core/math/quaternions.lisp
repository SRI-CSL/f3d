(IN-PACKAGE :MATH)

#|
(maybe-compile-file-load "$FREEDIUS/lisp/math/quaternions.lisp")
		        
|#


#| *********************   QUATERNIONS   *********************

Representation:  A QUATERNION  is a double-float vector of 4 elements:

   <q, qx, qy, qz>

|#

(deftype quaternion () '(simple-array double-float (4)))

(defmacro inline-fill-matrix (mat &rest list-of-rows)
  `(let ((mat ,mat))
    (declare (type dmatrix mat))
    (setf . ,(loop for row in list-of-rows
		   for i fixnum from 0
		   nconc (loop for element in row
			       for j fixnum from 0
			       collect `(aref mat ,i ,j)
			       collect element)))))

(declaim (inline make-quaternion))

(defun make-quaternion (q qx qy qz)
  (declare (double-float q qx qy qz))
  (inline-coordinate-vector q qx qy qz))

(defun make-quaternion-from-axis-and-angle (axis-vector angle)
  (declare (double-float angle))
  (declare (type (simple-array double-float (3)) axis-vector))	   
  (let* ((angle/2 (* .5 angle))
	 (s (sin angle/2)))
    (declare (double-float angle/2 s))
    (make-quaternion (cos angle/2)
		     (* s (aref axis-vector 0))
		     (* s (aref axis-vector 1))
		     (* s (aref axis-vector 2)))))
;(disassemble 'make-quaternion-from-axis-and-angle)	
  
(defun quaternion-conjugate (q)
  (declare (type (simple-array double-float (*)) q))
  (make-quaternion (aref q 0) (- (aref q 1)) (- (aref q 2)) (- (aref q 3))))

(defun quaternion-dot-product (q1 q2)
  (declare (type quaternion q1 q2 ))
  (+ (* (aref q1 0) (aref q2 0))
     (* (aref q1 1) (aref q2 1)) 
     (* (aref q1 2) (aref q2 2)) 
     (* (aref q1 3) (aref q2 3))))

(defun quaternion-add* (q r)
  (declare (type quaternion q r))
  (declare (optimize (speed 3) (safety 0)))
  (make-quaternion (+ (aref q 0) (aref r 0))
		   (+ (aref q 1) (aref r 1))
		   (+ (aref q 2) (aref r 2))
		   (+ (aref q 3) (aref r 3))))
;(disassemble 'quaternion-add*)

(defmacro quaternion-add (q1 &rest quaternions)
  (if quaternions
      `(quaternion-add (quaternion-add* ,q1 ,(car quaternions))
		       . ,(cdr quaternions))
      q1))

(defun quaternion-subtract* (q r)
  (declare (type quaternion q r))
  (declare (optimize (speed 3) (safety 0)))
  (make-quaternion (- (aref q 0) (aref r 0))
		   (- (aref q 1) (aref r 1))
		   (- (aref q 2) (aref r 2))
		   (- (aref q 3) (aref r 3))))

(defmacro quaternion-subtract (q1 &rest quaternions)
  (if quaternions
      `(quaternion-subtract* ,q1 (quaternion-add .,quaternions))
      q1))
;(quaternion-subtract q1 q2 q3 g4)

;;; this compiles less code because of lack of flt pt registers in the previous version
(defun quaternion-multiply2 (q r)
  (declare (type quaternion q r))
  (declare (optimize (speed 3) (safety 0)))
  (let ((q0 (aref q 0))
	(r0 (aref r 0))
	(cx 0.0) (cy 0.0) (cz 0.0))
    (declare (double-float q0 r0 cx cy cz ))
    (inline-cross-prod ((aref q 1) (aref q 2) (aref q 3))
		       ((aref r 1) (aref r 2) (aref r 3))
		       (cx cy cz))
    (make-quaternion (- (* q0 r0)
			(+ (* (aref q 1) (aref r 1))
			   (* (aref q 2) (aref r 2))
			   (* (aref q 3) (aref r 3))))
		     (+ (* q0 (aref r 1) ) (* r0 (aref q 1)) cx)
		     (+ (* q0 (aref r 2) ) (* r0 (aref q 2)) cy)
		     (+ (* q0 (aref r 3) ) (* r0 (aref q 3)) cz))))
;(disassemble 'quaternion-multiply2)

(eval-when (eval compile load)
(defun quaternion-multiply-expand (q1-form quaternion-forms)
  (if quaternion-forms
      (quaternion-multiply-expand
       `(quaternion-multiply2 ,q1-form ,(car quaternion-forms ))
       (cdr quaternion-forms ))
      q1-form ))
)

(defmacro quaternion-multiply (q1 &rest quaternions)
  (quaternion-multiply-expand q1 quaternions ))


;;; compute (quaternion-multiply q (quaternion-conjugate r))
(defun quaternion-multiply-q-rconj (q r)
  (declare (type quaternion q r))
  (declare (optimize (speed 3) (safety 0)))
  (let ((q0 (aref q 0))
	(rs (aref r 0))
	(cx 0.0) (cy 0.0) (cz 0.0))
    (declare (double-float q0 rs cx cy cz ))
    (inline-cross-prod ((aref q 1) (aref q 2) (aref q 3))
		       ((aref r 1) (aref r 2) (aref r 3))
		       (cx cy cz))
    (make-quaternion (+ (* q0 rs )
			(+ (* (aref q 1) (aref r 1))
			   (* (aref q 2) (aref r 2))
			   (* (aref q 3) (aref r 3))))
		     (- (* q0 (aref r 1) ) (* rs (aref q 1)) cx)
		     (- (* q0 (aref r 2) ) (* rs (aref q 2)) cy)
		     (- (* q0 (aref r 3) ) (* rs (aref q 3)) cz))))

(defun quaternion-scale (q scale)
  (declare (type quaternion q))
  (declare (double-float scale))
  (declare (optimize (speed 3) (safety 0)))
  (make-quaternion (* scale (aref q 0)) (* scale (aref q 1)) (* scale (aref q 2)) (* scale (aref q 3))))

(defun quaternion-negate (q)
  (declare (type quaternion q))
  (declare (optimize (speed 3) (safety 0)))
  (make-quaternion (- (aref q 0)) (- (aref q 1)) (- (aref q 2)) (- (aref q 3))))

(defun quaternion-magnitude (q) (sqrt (quaternion-dot-product q q)))
       
(defun quaternion-normalize (q)
  (declare (type quaternion q))
  (declare (optimize (speed 3) (safety 0)))
  (let ((size (quaternion-magnitude q)))
    (declare (double-float size ))
    (if (= size 0.0)
	(error "Quaternion-normalize - zero size")
	(quaternion-scale q (/ 1.0 size)))))

(defun quaternion-linear-combine (scale1 q1 scale2 q2)
  (declare (double-float scale1 scale2))
  (declare (type quaternion q1 q2))
  (declare (optimize (speed 3) (safety 0)))
  (macrolet ((combine (i)
	       `(+ (* scale1 (aref q1 ,i)) (* scale2 (aref q2 ,i)))))
    (make-quaternion (combine 0) (combine 1) (combine 2) (combine 3))))
;(disassemble 'quaternion-linear-combine)

(defun quaternion-mean (qs)
  (loop with n fixnum = (length qs)
	with k double-float = (/ 1.0 n)
	with q0sum double-float = 0.0
	with q1sum double-float = 0.0
	with q2sum double-float = 0.0
	with q3sum double-float = 0.0
	for q in qs
	do (bind-vector-elements (q0 q1 q2 q3) q
	     (incf q0sum q0)
	     (incf q1sum q1)
	     (incf q2sum q2)
	     (incf q3sum q3))
	finally (return (make-quaternion (* k q0sum) (* k q1sum) (* k q2sum) (* k q3sum)))))

(defun quaternions-equal (q r)
  (declare (type (or null quaternion) q r))
  (declare (optimize (speed 3) (safety 0)))
  (and q r 
       (= (aref q 0) (aref r 0))
       (= (aref q 1) (aref r 1))
       (= (aref q 2) (aref r 2))
       (= (aref q 3) (aref r 3))
       ))


;;; ASSUMES V is a vector NOT quaternion
;;; Does not assume that q is normalized
;;; 24 *, 10+, 4- = 38 flt ops
(defun quaternion-rotate-vector (q v)
  (declare (type quaternion q))
  (declare (type (simple-array double-float (*)) v))
  (declare (optimize (speed 3) (safety 0)))
  (make-quaternion (- (aref q 0)) (- (aref q 1)) (- (aref q 2)) (- (aref q 3)))
  (let ((2q.v (* 2.0 (+ (* (aref q 1) (aref v 0) )
			(* (aref q 2) (aref v 1) )
			(* (aref q 3) (aref v 2) ))))
	(k (- (^2 (aref q 0)) (+ (^2 (aref q 1) )
				 (^2 (aref q 2) )
				 (^2 (aref q 3) ))))
	(2q (* 2.0 (aref q 0)))
	(cx 0.0) (cy 0.0) (cz 0.0))
    
    (inline-cross-prod ((aref q 1) (aref q 2) (aref q 3))
		       ((aref v 0) (aref v 1) (aref v 2))
		       (cx cy cz))
    (inline-coordinate-vector
     (+ (* k (aref v 0)) (* 2q cx) (* 2q.v (aref q 1)))
     (+ (* k (aref v 1)) (* 2q cy) (* 2q.v (aref q 2)))
     (+ (* k (aref v 2)) (* 2q cz) (* 2q.v (aref q 3)))))) 

;;; ASSUMES V is a vector NOT quaternion
;;; ASSUMES that q is normalized
;;; (19 multiplies, 12 adds) = 31 flt pt ops
(defun quaternion-rotate-vector-uni (q v)
  (declare (type quaternion q))
  (declare (type (simple-array double-float (*)) v))
  (declare (optimize (speed 3) (safety 0)))
  (let ((2q (* 2.0 (aref q 0)))
	(qxv0 0.0) (qxv1 0.0) (qxv2 0.0) 
	(qxqxv0 0.0) (qxqxv1 0.0) (qxqxv2 0.0))
    (inline-cross-prod ((aref q 1) (aref q 2) (aref q 3))
		       ((aref v 0) (aref v 1) (aref v 2))
		       (qxv0 qxv1 qxv2 ))
    (inline-cross-prod ((aref q 1) (aref q 2) (aref q 3))
		       (qxv0 qxv1 qxv2)
		       (qxqxv0 qxqxv1 qxqxv2 ))
    (inline-coordinate-vector
     (+ (aref v 0) (* 2q qxv0) (* 2.0 qxqxv0))
     (+ (aref v 1) (* 2q qxv1) (* 2.0 qxqxv1))
     (+ (aref v 2) (* 2q qxv2) (* 2.0 qxqxv2))
     ))) 

#| What were these for?

;;; Make 4 x 4 matrix Q_l out of quaternion qq such that: q o r = Q_l r

(defun quater-left-matrix (qq)
  (let ((qo (first qq)) (qx (second qq)) (qy (third qq)) (qz (fourth qq)))
    (list qq 
;	  (list qo     qx     qy     qz) 
	  (list (- qx) qo     qz     (- qy))
	  (list (- qy) (- qz) qo     qx)
	  (list (- qz) qy     (- qx) qo))))

;;; Make 4 x 4 matrix Q_r out of quaternion qq such that: r o q = Q_r r

(defun quater-right-matrix (qq)
  (let ((qo (first qq)) (qx (second qq)) (qy (third qq)) (qz (fourth qq)))
    (list qq
;	  (list qo     qx     qy     qz) 
	  (list (- qx) qo     (- qz) qy)
	  (list (- qy) qz     qo    (- qx))
	  (list (- qz) (- qy) qx    qo))))
|#


#|     3X3 TRANSFORM MATRIX DERIVED FROM QUATERNION (q0 qx qy qz)

    q0^2 + qx^2 - qy^2 - qz^2          2(qx qy - q0 qz)             2(qx qz + q0 qy)


        2(qx qy + q0 qz)           q0^2 - qx^2 + qy^2 - qz^2        2(qy qz - q0 qx)


        2(qx qz - q0 qy)               2(qy qz + q0 qx)         q0^2 - qx^2 - qy^2 + qz^2


|#

;;; Matrix will be orthonormal if q is normalized.  The numerical quality of this
;;; function is suspect since differences of squares of elements are computed.
(defun orthogonal-matrix-from-quaternion (q &optional mat)
  (declare (type quaternion q))
  (declare (optimize (speed 3) (safety 0)))
  (let* ((q0 (aref q 0)) (qx (aref q 1)) (qy (aref q 2)) (qz (aref q 3))
	 (q0q0 (* q0 q0)) (q0qx (* q0 qx)) (q0qy (* q0 qy)) (q0qz (* q0 qz))
	 (qxqx (* qx qx)) (qxqy (* qx qy)) (qxqz (* qx qz))
	 (qyqy (* qy qy)) (qyqz (* qy qz)) (qzqz (* qz qz))
	 (mat (or mat (make-array '(3 3) :element-type 'double-float :initial-element 0.0))))
    (declare (double-float q0 qx qy qz q0q0 q0qx q0qy q0qz qxqx qxqy qxqz qyqy qyqz qzqz))
    (declare (type (simple-array double-float (* *)) mat ))
    (setf (aref mat 0 0) (- (+ q0q0 qxqx) (+ qyqy qzqz))
	  (aref mat 0 1) (* 2.0 (- qxqy q0qz))
	  (aref mat 0 2) (* 2.0 (+ qxqz q0qy))

	  (aref mat 1 0) (* 2.0 (+ qxqy q0qz))
	  (aref mat 1 1) (- (+ q0q0 qyqy) (+ qxqx qzqz))
	  (aref mat 1 2) (* 2.0 (- qyqz q0qx))

	  (aref mat 2 0) (* 2.0 (- qxqz q0qy))
	  (aref mat 2 1) (* 2.0 (+ qyqz q0qx))
	  (aref mat 2 2) (- (+ q0q0 qzqz) (+ qxqx qyqy)))
    (loop with n fixnum = (array-dimension mat 0)
	  for i fixnum from 3 below n
	  ;; set additional rows and columns to unit vectors
	  do (loop for j fixnum from 0 below n
		   do (setf (aref mat i j) 0.0 (aref mat j i) 0.0)) 
	     (setf (aref mat i i) 1.0))
    mat))


#|
q0q0 = (cos theta/2)^2 = (1+cos(theta))/2
qxqx + qyqy + qzqz = (sin theta/2)^2 = (1-cos(theta))/2

q0q0 + qxqx - qyqy - qzqz = q0q0 - (qxqx + qyqy + qzqz) + 2qxqx 
                          = (1+cos(theta))/2 - (1-cos(theta))/2 + 2qxqx
                          = cos(theta) + 2qxqx = cos(theta) + (1-cos(theta))*xx

(let* ((theta (radians 20.0))
       (theta/2 (* .5 theta))
       (sin-theta/2 (sin theta/2)))
  ;; these are identical to within roundoff errors.
  (list (orthogonal-matrix-from-quaternion (cv (cos theta/2) sin-theta/2 0.0 0.0))
	(make-angle-axis-rotation-matrix theta (list 1.0 0.0 0.0 0.0))))

|#


(defun quaternion-from-orthogonal-matrix (mat)
  (declare (type dmatrix mat ))
  (declare (optimize (speed 3) (safety 1)))
  (let ((r00 (aref mat 0 0)) (r10 (aref mat 1 0)) (r20 (aref mat 2 0) )
	(r01 (aref mat 0 1)) (r11 (aref mat 1 1)) (r21 (aref mat 2 1) )
	(r02 (aref mat 0 2)) (r12 (aref mat 1 2)) (r22 (aref mat 2 2) ))
    (declare (double-float r00 r10 r20 r01 r11 r21 r02 r22))
    (let* ((qas (+ r00 r11 r22))     (qbs (- r00 (+ r11 r22)))
	   (qcs (- r11 (+ r22 r00))) (qds (- r22 (+ r00 r11)))
	   (qm (max qas qbs qcs qds))
	   (qi (sqrt (* .25 (+ qm 1.0))) )
	   (1/4qi (/ .25 qi))) ; same as (/ 1 (* 4 qi))
      (declare (double-float qas qbs qcs qds qm qi 1/4qi ))
      ;(break)
      (cond ((= qas qm) 
	     (make-quaternion qi
			      (* (- r21 r12) 1/4qi)
			      (* (- r02 r20) 1/4qi)
			      (* (- r10 r01) 1/4qi)))
	    ((= qbs qm)
	     (make-quaternion (* (- r21 r12) 1/4qi)
			      qi
			      (* (+ r01 r10) 1/4qi)
			      (* (+ r02 r20) 1/4qi)))
	    ((= qcs qm)
	     (make-quaternion (* (- r02 r20) 1/4qi)
			      (* (+ r01 r10) 1/4qi)
			      qi
			      (* (+ r12 r21) 1/4qi)))
	    ((= qds qm)
	     (make-quaternion (* (- r10 r01) 1/4qi)
			      (* (+ r02 r20) 1/4qi) 
			      (* (+ r12 r21) 1/4qi)
			      qi))))
    ))

;;; ********************* PARTIAL DERIVITIVES W.R.T. QUATERNION PARAMETERS  ************************

;;; this is the same as ORTHOGONAL-MATRIX-FROM-QUATERNION
#|
(defun derivative-quaternion-rotate-vector-wrt-vector (q &optional partials)
  (declare (type quaternion q))
  (declare (type (or null dmatrix) partials))
  (declare (optimize (speed 3) (safety 0)))
  (unless partials (setq partials (make-array '(3 3) :element-type 'double-float)))
  (let* ((q0 (aref q 0)) (qx (aref q 1)) (qy (aref q 2)) (qz (aref q 3))
	 (q2 (* q0 q0)) (qx2 (* qx qx)) (qy2 (* qy qy)) (qz2 (* qz qz)))
    (declare (double-float q0 qx qy qz q2 qx2 qy2 qz2))
    (setf (aref partials 0 0) (- (+ q2 qx2) (+ qy2 qz2))
	  (aref partials 0 1) (* 2.0 (- (* qx qy) (* q0 qz)))
	  (aref partials 0 2) (* 2.0 (+ (* qx qz) (* q0 qy)))

	  (aref partials 1 0) (* 2.0 (+ (* qx qy) (* q0 qz)))
	  (aref partials 1 1) (- (+ q2 qy2) (+ qx2 qz2))
	  (aref partials 1 2) (* 2.0 (- (* qy qz) (* q0 qx)))

	  (aref partials 2 0) (* 2.0 (- (* qx qz) (* q0 qy)))
	  (aref partials 2 1) (* 2.0 (+ (* qy qz) (* q0 qx)))
	  (aref partials 2 2) (- (+ q2 qz2) (+ qx2 qy2)))
    partials))
							    |#

(defun derivative-quaternion-rotate-vector-wrt-vector (q &optional partials)
  (orthogonal-matrix-from-quaternion q partials))

;;; Returns four 3x3 matrices, matrix i being the partial derivitive of
;;; ORTHOGONAL-MATRIX-FROM-QUATERNION with respect to element i of the
;;; quaternion.
(defun derivative-quaternion-rotation-matrices
    (q &optional partials-q0 partials-qx partials-qy partials-qz )
  (declare (type (or null dmatrix) partials-q0 partials-qx partials-qy partials-qz ))
  (declare (type quaternion q))
  (declare (optimize (speed 3) (safety 0)))
 
  (unless partials-q0 (setq partials-q0 (make-array '(3 3) :element-type 'double-float)))
  (unless partials-qx (setq partials-qx (make-array '(3 3) :element-type 'double-float)))
  (unless partials-qy (setq partials-qy (make-array '(3 3) :element-type 'double-float)))
  (unless partials-qz (setq partials-qz (make-array '(3 3) :element-type 'double-float)))
  
  (let* ((2q0 (* 2.0 (aref q 0) ))
	 (2qx (* 2.0 (aref q 1) ))
	 (2qy (* 2.0 (aref q 2) ))
	 (2qz (* 2.0 (aref q 3) )))
    (declare (double-float 2q0 2qx 2qy 2qz))

    (inline-fill-matrix partials-q0
			(2q0       (- 2qz)   2qy)
			(2qz       2q0       (- 2qx))
			((- 2qy)   2qx       2q0))
    
    (inline-fill-matrix partials-qx
			(2qx       2qy       2qz)
			(2qy       (- 2qx)   (- 2q0))
			(2qz       2q0       (- 2qx)))

    (inline-fill-matrix partials-qy
			((- 2qy)   2qx       2q0)
			(2qx       2qy       2qz)
			((- 2q0)   2qz       (- 2qy)))

    (inline-fill-matrix partials-qz
			((- 2qz)   (- 2q0)   2qx)
			(2q0       (- 2qz)   2qy)
			(2qx       2qy       2qz))
	  
    (values partials-q0 partials-qx partials-qy partials-qz )))

;;; This function computes the array of partials of the rotation of vector V by the
;;; quaternion Q with respective to each element of the quaternion.  Row i in
;;; the matrix contains the partial vector with respect to quaternion element i.
;;; Each row in this array is equivalent to the vector returned from multiplying
;;; the corresponding matrix returned from DERIVATIVE-QUATERNION-ROTATION-MATRICES
;;; by the vector V.
(defun derivative-quaternion-rotate-vector-wrt-quaternion (q v &optional partials)
  (declare (type quaternion q))
  (declare (type (simple-array double-float (*)) v))
  (declare (type (or null dmatrix) partials))
  (unless partials (setq partials (make-array '(4 3) :element-type 'double-float)))
  (let* ((q0 (aref q 0)) (qx (aref q 1)) (qy (aref q 2)) (qz (aref q 3))
	 (x (aref v 0)) (y (aref v 1)) (z (aref v 2))
	 (q.v (+ (* qx x) (* qy y) (* qz z))))
    (declare (double-float q0 qx qy qz x y z q.v ))

    ;; partial w.r.t. q0
    (let ((qxv0 0.0) (qxv1 0.0) (qxv2 0.0)) 
      (declare (double-float qxv0 qxv1 qxv2))
      (inline-cross-prod (qx qy qz) (x y z) (qxv0 qxv1 qxv2 ))
      (setf (aref partials 0 0) (* 2.0 (+ (* q0 x) qxv0))
	    (aref partials 0 1) (* 2.0 (+ (* q0 y) qxv1))
	    (aref partials 0 2) (* 2.0 (+ (* q0 z) qxv2))))
    #+never ; alternative computations
    (setf (aref partials 0 0) (* 2.0 (- (+ (* x q0) (* z qy)) (* y qz)))
	  (aref partials 0 1) (* 2.0 (- (+ (* y q0) (* x qz)) (* z qx)))
	  (aref partials 0 2) (* 2.0 (- (+ (* z q0) (* y qx)) (* x qy))))

    ;; partial w.r.t. qx
    (setf (aref partials 1 0) (* 2.0 q.v)
	  (aref partials 1 1) (* 2.0 (- (* x qy) (+ (* y qx) (* z q0))))
	  (aref partials 1 2) (* 2.0 (- (+ (* x qz) (* y q0)) (* z qx))))

    ;; partial w.r.t. qy
    (setf (aref partials 2 0) (* 2.0 (- (+ (* y qx) (* z q0)) (* x qy)))
	  (aref partials 2 1) (* 2.0 q.v)
	  (aref partials 2 2) (* 2.0 (- (* y qz) (+ (* z qy) (* x q0)))))

    ;; partial w.r.t. qz
    (setf (aref partials 3 0) (* 2.0 (- (* z qx) (+ (* x qz) (* y q0))))
	  (aref partials 3 1) (* 2.0 (- (+ (* z qy) (* x q0)) (* y qz)))
	  (aref partials 3 2) (* 2.0 q.v))
	  
    partials))

 ;;; *********  TESTS  *********

#|
(Setq m1 (cme::make-orientation-matrix :x .1 :y .2 :z .3)) 
(Setq m1 (cme::make-orientation-matrix :x .001 :y .002 :z .003)) 

(setq q1 (quaternion-from-orthogonal-matrix m1))

(setq m1b (orthogonal-matrix-from-quaternion q1 (make-array '(4 4) :element-type 'double-float)))
(list m1 m1b)
(setq v (coordinate-vector 10.0 20.0 30.0 0.0))

;;; These get the same answers except for minor roundoff
(list (multiply-matrices m1 v)
      (quaternion-rotate-vector-uni q1 v)
      (quaternion-rotate-vector q1 v))

(progn m1)
(progn q1)

(DERIVATIVE-QUATERNION-ROTATE-VECTOR-WRT-VECTOR q1)

(derivative-quaternion-rotate-vector-wrt-quaternion q1 v)


(defun test-derivative-quaternion-rotate-vector-wrt-quaternion (q v dq)
  (let* ((q2 (if nil
		 (quaternion-normalize (quaternion-add q dq))
		 (quaternion-add q dq) ))
	 (qv1 (quaternion-rotate-vector q v))
	 (qv2 (quaternion-rotate-vector q2 v))
	 (length (quaternion-magnitude dq))
	 (dv (vector-times-scalar (vector-difference qv2 qv1) (/ length)))
	 )
	
    (list  dv
	   (derivative-quaternion-rotate-vector-wrt-quaternion q v)
	   q q2 qv1 qv2)))

(quaternion-magnitude q1)

;;; these all agree (having discovered several bugs in derivations of formulas)
(setq eps 1e-6)
(setq eps 1e-8)
(test-derivative-quaternion-rotate-vector-wrt-quaternion q1 v (make-quaternion eps 0.0 0.0 0.0)) 
(test-derivative-quaternion-rotate-vector-wrt-quaternion q1 v (make-quaternion 0.0 eps 0.0 0.0)) 
(test-derivative-quaternion-rotate-vector-wrt-quaternion q1 v (make-quaternion 0.0 0.0 eps 0.0)) 
(test-derivative-quaternion-rotate-vector-wrt-quaternion q1 v (make-quaternion 0.0 0.0 0.0 eps )) 

|#



;;; QUATERNION-SLERP = Quaternion Spherical Linear Interpolation 

;;; Method of Ken Shoemake, "Animating Rotation with Quaternoin curves",
;;; Computer Graphics, Vol 19, No. 3, 1985.
(defun quaternion-slerp (q1 q2 alpha)
  (let ((dot (max -1.0 (min 1.0 (quaternion-dot-product q1 q2)))))
    (if (> dot .9995)
	;; quaternions are too close	
	(quaternion-normalize (quaternion-linear-combine (- 1.0 alpha) q1 alpha q2))
	(let* ((theta (* alpha (acos dot)))
	       ;; Gram-Schmidt orthogonalization 
	       (q2p (quaternion-normalize (quaternion-linear-combine 1.0 q2 (- dot) q1))))
	  (quaternion-linear-combine (cos theta) q1 (sin theta) q2p)))))

;;; Log Quaternion Map:  
;;; "A General Construction Scheme for Unit Quaternion Curves with Simple High Order Derivatives (1995)"
;;; Myoung-Jun Kim, Myung-Soo Kim, Sung Yong Shin, Computer Graphics.

;(disassemble 'quaternion-log)
(defun quaternion-log (q)
  (declare (type quaternion q))
  (bind-vector-elements (w x y z) q
    (declare (type (double-float -1.0 1.0) w))
    (let* ((k (/ (acos w) (sqrt (- 1.0 (* w w))))))
      (inline-coordinate-vector (* k x) (* k y) (* k z)))))

;;;(defun quaternion-log (q)
;;;  (declare (type quaternion q))
;;;  (bind-vector-elements (w x y z) q
;;;    (declare (type (double-float -1.0 1.0) w))
;;;    (let* ((omega (acos w))
;;;           (k (/ omega (sin omega))))
;;;      (inline-coordinate-vector (* k x) (* k y) (* k z)))))
		   
;(disassemble 'quaternion-exp)
(defun quaternion-exp (v)
  (declare (type (dvector 3) v))
  (let* ((vx (aref v 0)) (vy (aref v 1)) (vz (aref v 2))
	 (omega (inline-euclidean-length vx vy vz)))
    (declare (double-float vx vy vz omega))
    (if (< omega 1e-14)
	(inline-coordinate-vector 1.0 0.0 0.0 0.0)
	(let ((sin/omega (/ (sin omega) omega)))
	  (declare (double-float sin/omega))
	  (inline-coordinate-vector (cos omega) (* sin/omega vx) (* sin/omega vy) (* sin/omega vz)))) ))
  
;;; q^alpha = quaternion-exp(alpha*quaternion-log(q))
(defun quaternion-expt (q alpha)
  (declare (type quaternion q))
  (bind-vector-elements (w x y z) q
    (declare (type (double-float -1.0 1.0) w))
    (if (or (< (abs alpha) 1e-14) (= w 1.0))
	(inline-coordinate-vector 1.0 0.0 0.0 0.0)
	(let* ((omega*alpha (* (acos w) alpha))
	       (k (/ (sin omega*alpha) (sqrt (- 1.0 (* w w))))))
	  (declare (double-float omega*alpha k))
	  (inline-coordinate-vector (cos omega*alpha) (* k x) (* k y) (* k z))))))

;;; bug fix for boundary condition where w is "slightly" out of -1:+1 range.
(defun quaternion-expt (q alpha)
  (declare (type quaternion q))
  (bind-vector-elements (w x y z) q
    ;;(declare (type (double-float -1.0 1.0) w))
    ;; clamp w to -1:+1 range
    (cond ((> w 1.0) (setq w 1.0))
	  ((< w -1.0) (setq w -1.0)))
    ;;(setq *foo2* (list q alpha w))
    (if (or (< (abs alpha) 1e-14) (= w 1.0) (= w -1.0))
	(inline-coordinate-vector 1.0 0.0 0.0 0.0)
	(let* ((omega*alpha (* (acos w) alpha))
	       (denom (sqrt (- 1.0 (* w w))))
	       ;;(foo (setq *foo2* (list q alpha omega*alpha denom)))
	       (k (if t ;(> denom 1e-14)
		      (/ (sin omega*alpha) (sqrt (- 1.0 (* w w))))
		      0.0)))
	  (declare (double-float omega*alpha k))
	  (inline-coordinate-vector (cos omega*alpha) (* k x) (* k y) (* k z))))))
#|      
(trace quaternion-expt)
(trace quaternion-log)
(trace quaternion-exp)
(untrace)
|#

;;; LOG-QUATERNION-LERP = Linear Interpolation in Log of Quaternion space

;;; (defun log-quaternion-lerp (q1 q2 alpha)
;;;   (let ((omega (quaternion-log (quaternion-multiply (quaternion-conjugate q1) q2))))
;;;     (quaternion-multiply q1 (quaternion-exp (vector-times-scalar omega alpha)))))
    
(defun log-quaternion-lerp (q1 q2 alpha)
  (let ((q1inv*q2 (quaternion-multiply (quaternion-conjugate q1) q2)))
    (quaternion-multiply q1 (quaternion-expt q1inv*q2 alpha))))
    	    
;;; alpha=0 => mat0, alpha=1 => mat1
(defun interpolate-transform-matrices (mat0 mat1 alpha &optional 
				       (interpolator 'log-quaternion-lerp))
  (let* ((q0 (quaternion-from-orthogonal-matrix mat0))
	 (q1 (quaternion-from-orthogonal-matrix mat1))
	 ;; BUG FIX Wed Apr 13 2005
	 ;; Be careful -- there can be a sign flip between the axes of the 2 quaternions
	 (q (if (< (+ (* (aref q0 1) (aref q1 1)) ; dot product of axes
		      (* (aref q0 2) (aref q1 2))
		      (* (aref q0 3) (aref q1 3)))
		   0.0)
		(funcall interpolator (quaternion-negate q0) q1 alpha) ; > 90 degrees angle
		(funcall interpolator q0 q1 alpha)))
	 (interpolated-mat (make-4x4-identity-matrix)))
    (declare (type quaternion q0 q1))
    (orthogonal-matrix-from-quaternion q interpolated-mat)
    (setf (matrix-column interpolated-mat 3)
	  (vector-linear-combine (- 1.0 alpha) (matrix-column mat0 3)
				 alpha (matrix-column mat1 3))) interpolated-mat))

;;; FIXME:  This needs to interpolate at least the FOCAL-LENGTH.
;;; This is redefined in transforms/frame-camera.lisp
(defun interpolate-camera-to-2d-matrices (mat0 mat1 alpha )
  mat0)

;;; LOSE - LOSE -- in package ordering
;;; DECOMPOSE-PROJECTION-MATRIX is used here in MATH package before TRANSFORMS package exists, causing
;;; problems later.
;;; INTERPOLATE-PROJECTION-MATRICES has been moved to transfroms/4x4-projection-decomposition.lisp


;;; INTERPOLATE-PROJECTION-MATRICES assumes that the camera-to-2d-matrices are
;;; the same for both projections.
#+moved
(defun interpolate-projection-matrices (proj0 proj1 alpha &optional 
					       (interpolator 'log-quaternion-lerp))
  ;(unless (<= 0.0 alpha 1.0) (break))
  (cond ((= alpha 0.0) proj0)
	((= alpha 1.0) proj1)
	(t (mv-bind (3d-to-camera-mat0 camera-to-2d-mat0)
	       (decompose-projection-matrix proj0)  
	     (mv-bind (3d-to-camera-mat1 camera-to-2d-mat1)
		 (decompose-projection-matrix proj1)
	       (let ((3d-to-camera-matrix 
		      (interpolate-transform-matrices 3d-to-camera-mat0 3d-to-camera-mat1 alpha interpolator))
		     (camera-to-2d-mat 
		      (interpolate-camera-to-2d-matrices camera-to-2d-mat0 camera-to-2d-mat1 alpha)))
		 (multiply-matrices camera-to-2d-mat 3d-to-camera-matrix)))))))


#|
;(setq *rot* (upper-3x3-matrix (transforms::3d-to-camera-matrix (gui::3d-to-2d-projection (gui::top-view)))))

(let ((q (quaternion-from-orthogonal-matrix *rot*)))
  (list (quaternion-exp (quaternion-log q)) q))

;(list (quaternion-exp (quaternion-log (quaternion-from-orthogonal-matrix *rot*)))  *rot*) 
;(multiply-matrices *rot* (transpose-matrix *rot*))

(setq *q* (quaternion-from-orthogonal-matrix *rot*)
      *p* (quaternion-log *q*)
      *q2* (quaternion-exp *p*)
      *rot2* (orthogonal-matrix-from-quaternion *q2*))
(list *rot2* *rot*)

(list *q* *q2* (vector-euclidean-length *q*) (vector-euclidean-length *q2*))


(let* ((3d-to-cam1 (transforms::3d-to-camera-matrix (3d-to-2d-projection (top-view))))
       (alpha .3)
					;(rot 70.0)
       (rot 70.0)
       (rot1 (make-4x4-rotation-matrix :omega-degrees rot))
       (rot-alpha (make-4x4-rotation-matrix :omega-degrees (* alpha rot)))
       (translation (cv 10.0 100.0 1000.0 0.0))
       (alpha*translation (vector-times-scalar translation alpha))
       (3d-to-cam2 (multiply-matrices rot1 3d-to-cam1))
       (3d-to-cam-alpha (multiply-matrices rot-alpha 3d-to-cam1)))
  (setf (matrix-column 3d-to-cam2 3) 
	(vector-add (matrix-column 3d-to-cam1 3) translation))
  (setf (matrix-column 3d-to-cam-alpha 3) 
	(vector-add (matrix-column 3d-to-cam1 3) alpha*translation))
  (let ((interp12 (linear-interpolate-3d-to-camera-matrices 3d-to-cam1 3d-to-cam2 alpha))
	(interp21 (linear-interpolate-3d-to-camera-matrices 3d-to-cam2 3d-to-cam1 (- 1.0 alpha))))
    (values interp12 
	    interp21
	    3d-to-cam-alpha
	    3d-to-cam1 
	    3d-to-cam2)))
       
|#
