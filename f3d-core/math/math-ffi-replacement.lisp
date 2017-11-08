(in-package :math)

;;; This file contains a subset of the functionality in math-ffi.lisp suitable
;;; for supporting TRANSFORMS subsystem without the need for foreign-functons or
;;; shared libraries.

;;; The essential guts of MULTIPLY-MATRICES.  The ugly arglist is the same as
;;; used when calling the C implementation.
(defun multiply_matrices (n1 n2 n3 a b c)
  (declare (optimize (speed 3) (safety 1)))
  (declare (fixnum n1 n2 n3))
 ; (declare (type dmatrix a b c))
  (declare (type (array double-float) a b c))
  (let* ((a (array-simple-vector a))
	 (b (array-simple-vector b))
	 (c (array-simple-vector c)))
    (declare (type (simple-array double-float (*)) a b c))
    (loop for i fixnum from 0 below n1
	  for n2*i fixnum from 0 by n2
	  for n3*i fixnum from 0 by n3
	  do (loop for j fixnum from 0 below n3
		   for n3*i+j fixnum from n3*i
		   for sum double-float = 0.0
		   ;; Allegro sucks!   conses if using return value from loop.
		   do (loop for k fixnum from 0 below n2
			    for n2*i+k fixnum from n2*i
			    for n3*k+j fixnum from j by n3
			    ;; (* (aref a i k) (aref b k j))
			    do (incf sum (* (aref a n2*i+k) (aref b n3*k+j))))
		      (setf (aref c n3*i+j) sum))))
  c)


(defun multiply_matrices_transposed1 (n1 n2 n3 a b c)
  (declare (optimize (speed 3) (safety 1)))
  (declare (fixnum n1 n2 n3))
  ;(declare (type dmatrix a b c))
  (declare (type (array double-float) a b c))
  (let ((a (array-simple-vector a))
	(b (array-simple-vector b))
	(c (array-simple-vector c))
	(spana n1)
	(spanb n3)
	(spanc n3)
	(com-dim n2))
    (declare (type (simple-array double-float (*)) a b c))
    (declare (fixnum spana spanb spanc com-dim))
    (loop for i fixnum from 0 below n1	      
	  for n3*i fixnum from 0 by spanc
	  do (loop for j fixnum from 0 below n3
		   for n3*i+j fixnum from n3*i
		   for sum double-float = 0.0
		   ;; Allegro sucks!   conses if using return value from loop.
		   do (loop for k fixnum from 0 below com-dim
			    for k1 fixnum from i by spana
			    for k2 fixnum from j by spanb
			    ;; (* (aref a k i) (aref b k j))
			    do (incf sum (* (aref a k1) (aref b k2))))
		      (setf (aref c n3*i+j) sum))))
  c)

(defun multiply_matrices_transposed2 (n1 n2 n3 a b c)
  (declare (optimize (speed 3) (safety 1)))
  (declare (fixnum n1 n2 n3))
  ;(declare (type dmatrix a b c))
  (declare (type (array double-float) a b c))
  (let ((a (array-siMple-vector a))
	(b (array-simple-vector b))
	(c (array-simple-vector c))
	(spana n2)
	(spanb n2)
	(spanc n3)
	(com-dim n2))
    (declare (type (simple-array double-float (*)) a b c))
    (declare (fixnum spana spanb spanc com-dim))
    (loop for i fixnum from 0 below n1	      
	  for n3*i fixnum from 0 by spanc
	  for n2*i fixnum from 0 by spana
	  do (loop for j fixnum from 0 below n3
		   for n2*j fixnum from 0 by spanb
		   for n3*i+j fixnum from n3*i
		   for sum double-float = 0.0
		   ;; Allegro sucks!   conses if using return value from loop.
		   do (loop for k fixnum from 0 below com-dim
			    for k1 fixnum from n2*i
			    for k2 fixnum from n2*j
			    ;; (* (aref a i k) (aref b j k))
			    do (incf sum (* (aref a k1) (aref b k2))))
		      (setf (aref c n3*i+j) sum))))
  c)

#|
(disassemble 'multiply_matrices)
(disassemble 'multiply_matrices_transposed1)
(disassemble 'multiply_matrices_transposed2)

(defun make-random-matrix (dims lo hi)
  (let* ((a (make-array dims :element-type 'double-float))
	 (a1 (array-simple-vector a)))
    (declare (type (simple-array double-float (*)) a1))
    (loop for i fixnum from 0 below (array-dimension a1 0)
	  do (setf (aref a1 i) (random-in-range lo hi)))
    a))
    

;;;(defun make-identity-matrix (n)
;;;  (declare (fixnum n))
;;;  (let ((a (make-array (list n n) :element-type 'double-float :initial-element 0.0)))
;;;    (declare (type dmatrix a))
;;;    (loop for i fixnum from 0 below n
;;;          do (setf (aref a i i) 1.0))
;;;    a))

(setq *print-array* nil)

(setq m1 (make-object-to-parent-rotation-matrix :x-deg 10.0 :y-deg 20.0 :z-deg 30.0))

(time (setq m2 (invert-matrix m1)))
 
(matrix-max-abs-difference (invert-matrix m1) (invert-matrix2 m1)) 2; .0e-20

(let ((m1 (make-random-matrix '(100 100) -1e-6 1e6)))
  (matrix-max-abs-difference (multiply-matrices m1 (invert-matrix m1))
			     (make-identity-matrix (array-dimension m1 0))))

(let* ((m1 (make-random-matrix '(500 500) -1e-6 1e6))
       (inv (make-array (array-dimensions m1) :element-type 'double-float)))
  (time (invert-matrix m1 inv)))
;;; (500 500)  Allegro 3.59 secs  CMU 2.59 secs   C-code 2.63
;;; (300 300) Allegro   .35 secs  CMU  .47        C-code  .5

(copy-matrix m1)
(untrace lu_decompose)

(let ((m1 (make-random-matrix '(8 12) -1e-6 1e6))
      (m2 (make-random-matrix '(8 10) -1e-6 1e6)))
  (matrix-max-abs-difference (multiply-matrices (transpose-matrix m1) m2)
			     (multiply-matrices-transposed1 m1 m2)))

(let ((m1 (make-random-matrix '(8 10) -1e-6 1e6))
      (m2 (make-random-matrix '(12 10) -1e-6 1e6)))
  (matrix-max-abs-difference (multiply-matrices m2 (transpose-matrix m1))
			     (multiply-matrices-transposed2 m2 m1)))
|#



(defparameter *math_lu_solve_tiny* 0.0)

;(disassemble 'double-float-lu-decompose)
(defun double-float-lu-decompose (matrix &optional lu ps (tiny *math_lu_solve_tiny*))
  (declare (optimize (speed 3) (safety 1)))
  ;(declare (optimize (speed 0) (safety 3) (debug 3)))
  (declare (type dmatrix matrix))
  (declare (double-float tiny))
  (let* ((n (array-dimension matrix 0))
	 (element-type 'double-float)
	 (lu (or lu (make-array (list n n) :element-type element-type)))
	 (ps (or ps (make-array n :element-type '(signed-byte 32))))
	 (vv (make-array n :element-type element-type))
	 (d 1.0)
	 (a (array-simple-vector lu)))
    (declare (type (simple-array double-float (* *)) lu))
    (declare (type (simple-array double-float (*)) vv))
    (declare (type (simple-array (signed-byte 32) (*)) ps))
    (declare (double-float tiny))
    (declare (fixnum n))
    (declare (double-float d))
    (declare (type (simple-array double-float (*)) a))

    (unless (eq lu matrix) (copy-matrix matrix lu))

    (loop for i fixnum from 0 below n
	  for ni fixnum from 0 by n
	  for big double-float = 0.0
	  ;; Allegro sucks!  Conses double-floats if using return value
	  do (loop for j fixnum from 0 below n
		   for ni+j from ni
		   for abs-elem double-float = (abs (aref a ni+j))
		   when (> abs-elem big) 
		     do (setq big abs-elem))
	  when (< big tiny)
	    do (error "Singular Matrix")
	  do (setf (aref vv i) (/ big)))

    (loop for j fixnum from 0 below n
	  for nj fixnum from 0 by n
	  with imax fixnum
	  do (loop for i fixnum from 0 below j
		   for ni fixnum from 0 by n
		   for ni+j fixnum from j by n
		   for sum double-float = (aref a ni+j)
		   do (loop for k fixnum from 0 below i
			    for ni+k fixnum from ni
			    for nk+j fixnum from j by n
			    do (decf sum (* (aref a ni+k) (aref a nk+j))))
		   do (setf (aref a ni+j) sum))
	     
	     (loop with big double-float = 0.0
		   for i fixnum from j below n
		   for ni fixnum from nj by n
		   for ni+j fixnum from (+ ni j) by n
		   for sum double-float = (aref a ni+j)
		   with tmp double-float = 0.0
		   do (loop for k fixnum from 0 below j
			    for nk+j fixnum from j by n
			    for ni+k from ni
			    do (decf sum (* (aref a ni+k) (aref a nk+j))))
		      (setf (aref a ni+j) sum)

		   do (setf tmp (* (aref vv i) (abs sum)))
		   when (>= tmp  big)
		     do (setq big tmp
			      imax i))

	     (unless (= j imax)
	       (loop with nimax fixnum = (* n imax)
		     for k fixnum from 0 below n
		     do (rotatef (aref a (+ nimax k)) (aref a (+ nj k))))
	       (setf d (- d)
		     (aref vv imax) (aref vv j)))

	     (setf (aref ps j) imax)

	     (when (<= (abs (aref a (+ nj j))) tiny)
					;(warn "lu_decompose 2~%") (break)
	       (error "Singular Matrix")) ; singular matrix

	     (unless (= j (1- n))
	       (loop with dum double-float = (/ (aref a (+ nj j)))
		     for i fixnum from (1+ j) below n
		     for ni+j fixnum from (+ j n nj) by n
		     do (setf (aref a ni+j) (* (aref a ni+j) dum))))

	  finally (return (values lu ps d)))))

;(disassemble 'double-float-lu-backsubstitute)
(defun double-float-lu-backsubstitute (lu ps b &optional x (tiny *math_lu_solve_tiny*))
  (declare (optimize (speed 3) (safety 1)))
  ;;(declare (optimize (speed 0) (safety 3) (debug 3)))
  (declare (type (simple-array (signed-byte 32) (*)) ps))
  (declare (type (simple-array double-float (*)) b))
  (declare (type (or null (simple-array double-float (*))) b))
  (declare (double-float tiny))
  (let* ((n (array-dimension lu 0))
	 (x (or x (make-array n :element-type 'double-float)))
	 (a (array-simple-vector lu)))
    (declare (type (simple-array double-float (*)) a x))
    (declare (fixnum n))
    (unless (= n (array-dimension lu 1))
      (error "Matrix lu must be square"))

    (unless (eq x b) (vector-copy b x))

    (loop with ii fixnum = -1
	  for i fixnum from 0 below n
	  for ni fixnum from 0 by n
	  for ip fixnum = (aref ps i)
	  for sum double-float = (aref x ip)
	  do (setf (aref x ip) (aref x i))
	  when (>= ii 0)
	    do (loop for j fixnum from ii below i
		     for ni+j fixnum from (+ ni ii)
		     do (decf sum (* (aref a ni+j) (aref x j))))
	  else unless (zerop sum)
		 do (setf ii i)
	  do (setf (aref x i) sum))

    (loop for i fixnum from (1- n) downto 0
	  for ni fixnum from (* n i) by (- n)
	  for a_ii double-float = (aref a (+ ni i))
	  with sum double-float = 0.0
	  if (> (abs a_ii) tiny)
	    do (setf sum (aref x i))
	       (loop for j fixnum from (1+ i) below n
		     for ni+j fixnum from (+ ni j)
		     do (decf sum (* (aref a ni+j) (aref x j))))
	       (setf (aref x i) (/ sum a_ii))
	  else return 0 ; singular matrix
	  finally (return 1))))


(defun invert-matrix (matrix &optional inverse (tiny *math_lu_solve_tiny*))
  (declare (optimize (speed 3) (safety 1)))
  (declare (type square-dmatrix matrix))
  (declare (type (or null square-dmatrix) inverse))
  (declare (double-float tiny))
  (type-check square-dmatrix matrix)
  (type-check (or null square-dmatrix) inverse)
  (let* ((n (array-dimension matrix 0))
	 (col (make-array n :element-type 'double-float :initial-element 0.0d0))
	 (inverse (or inverse (make-array (list n n) :element-type 'double-float
					  :initial-element 0.0d0)))
	 (inv1d (array-simple-vector inverse)))
    (declare (type dmatrix inverse))
    (declare (type (simple-array double-float (*)) inv1d col))
    (declare (fixnum n))
    (fill-double-array inverse 0.0)
    (mv-bind (lu ps) (lu-decompose matrix)
      (declare (type (simple-array (signed-byte 32) (*)) ps))
      (declare (type dmatrix matrix lu))
      (loop for j fixnum from 0 below n
		   do (loop for i fixnum from 0 below n
			    ;; fill with columns of an identity matrix
			    do (setf (aref col i) 0.0))
		      (setf (aref col j) 1.0)
		      (double-float-lu-backsubstitute lu ps col col tiny)
		      (loop for i fixnum from 0 below n 
			    for ni+j fixnum from j by n
			    do (setf (aref inv1d ni+j) (aref col i))))
      inverse)))
