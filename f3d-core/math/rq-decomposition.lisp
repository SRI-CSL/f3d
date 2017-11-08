(in-package :math)

#|
(maybe-compile-file-load "$FREEDIUS/lisp/math/rq-decomposition.lisp")
|#


;;;(defun copy-matrix-rows (from-matrix to-matrix from-rows to-rows
;;;                         &optional (ncols (array-dimension from-matrix 1)))
;;;  (declare (type (simple-array double-float (* *)) from-matrix))
;;;  (let ((to-matrix (or to-matrix
;;;                       (make-array (list (length to-rows) ncols) :element-type 'double-float
;;;                                   :initial-element 0.0))))
;;;    (declare (type (simple-array double-float (* *))  to-matrix))
;;;    (loop for from-row fixnum in from-rows
;;;          for to-row fixnum in to-rows
;;;          do (loop for col fixnum from 0 below ncols
;;;                   do (setf (aref to-matrix to-row col)
;;;                            (aref from-matrix from-row col))))
;;;    to-matrix))

;;; Move this to matrices.lisp
(defun copy-matrix-rows (from-matrix to-matrix from-rows to-rows
			 &optional (dims (array-dimensions from-matrix)))
  (declare (type (simple-array double-float (* *)) from-matrix))
  (let* ((to-matrix (or to-matrix
			(make-array dims :element-type 'double-float
				    :initial-element 0.0)))
	 (ncols (min (array-dimension from-matrix 1)
		     (array-dimension to-matrix 1))))
    (declare (type (simple-array double-float (* *))  to-matrix))
    (loop for from-row fixnum in from-rows
	  for to-row fixnum in to-rows
	  do (loop for col fixnum from 0 below ncols
		   do (setf (aref to-matrix to-row col)
			    (aref from-matrix from-row col))))
    to-matrix))

(defun RQ-flip-row-col (R Q i)
  (declare (type (simple-array double-float (* *)) R Q))
  (loop for j fixnum from 0 below (array-dimension R 1)
	do (setf (aref R j i) (- (aref R j i)) ; flip column orientation
		 (aref Q i j) (- (aref Q i j)) ; flip row orientation
		 )))
    
;;; flip row orientation of R and column orientation of Q
;;; so that diagonal elements of R are non-negative.
(defun canonicalize-rq-decomposition (R Q &optional canonicalize-row)
  (declare (type (simple-array double-float (* *)) R Q))
  (loop with n fixnum = (array-dimension R 1)
	for i fixnum from 0 below n
	when (< (aref R i i) 0.0)
	  do (RQ-flip-row-col R Q i))
  (when (and canonicalize-row
	     (< (math::determinant Q) 0.0))
    (RQ-flip-row-col R Q canonicalize-row))
  ;;(format t "det(Q) = ~a~%" (math::determinant q))
  ;;(scale-matrix R (/ (aref R 0 0)))
  )


#| ****************************   QR-DECOMPOSITION   **************************

Facts about QR-DECOMPOSITION.

   A = QR

    A is any square matrix
    Q is an orthonormal matrix
    R is an upper-triangular matrix

  Q and R are unique up to a choice of orientations of the rows of Q and columns
  of R.  Ie, let matrix S be a diagonal matrix whose diagonal elements are 
  Sii = +1 or -1 (each Sii is chosen independently of the others).  Then,

   A = (QS) (SR), and Q' = QS, R' = SR is also a QR-DECOMPOSITION of A.

|#


;;; Based on Numerical Recipes qrdcmp.c
;;;    One-based indexing converted to zero-based indexing.

(defun qrdcmp (a c d)
  (declare (optimize (speed 3)(safety 1)))
  (declare (type (simple-array double-float (* *)) a))
  (declare (type (simple-array double-float (*)) c d))
  (let* ((n (array-dimension a 0))
	 (n-1 (1- n))
	 (sum 0.0) ( akk 0.0) ( sigma 0.0) 
	 singular)
    (declare (double-float sum akk sigma))
    (declare (fixnum n n-1))
    (loop for k fixnum from 0 below n-1
	  for scale double-float
	    = (loop for i fixnum from k below n
		    maximize (abs (aref a i k)) double-float)
	  when (= scale 0.0)
	    do (setf singular t
			      (aref c k) 0.0
			      (aref d k) 0.0)
	  else do
	    (loop for i fixnum from k below n
		  with 1/scale double-float = (/ scale)
		  do (setf (aref a i k) (* 1/scale (aref a i k))))
	    (setf sum (loop for i fixnum from k below n
			    sum (^2 (aref a i k)) double-float))
	    (setf akk (aref a k k)
		  sigma (the double-float (sqrt sum)))
	    (when (< akk 0.0)
	      (setq sigma (- sigma)))
	    (incf akk sigma)
	    (setf (aref a k k) akk
		  (aref c k) (* sigma akk)
		  (aref d k) (- (* scale sigma)))
	    (loop for j fixnum from (1+ k) below n
		  for tau double-float 
		    = (/ (loop for i fixnum from k below n
			       sum (* (aref a i k) (aref a i j)) double-float)
			 (aref c k))
		  do (loop for i fixnum from k below n
			   do (decf (aref a i j) (* tau (aref a i k)))))
	  )
    (setf (aref d n-1) (aref a n-1 n-1))
    (when (= (aref d n-1) 0.0)
      (setf singular t))
    singular))

#|
(disassemble 'qrdcmp)
(disassemble 'qrbcalc)
(disassemble 'qr-decompose)
|#

;; compute QT * bi-th column of b.  When iterated over the rows, Q*B
(defun qrbcalc (a c b bi)
  (declare (optimize (speed 3)(safety 1)))
  (declare (fixnum bi))
  (declare (type (simple-array double-float (* *)) a b))
  (declare (type (simple-array double-float (*)) c))
  (let* ((n (array-dimension a 0)))
    (declare (fixnum n))
    (loop for j fixnum from 0 below (1- n)
	  for tau double-float
	    = (/ (loop for i fixnum from j below n
		       sum (* (aref a i j) (* (aref b bi i))) double-float)
		 (aref c j))
	  do (loop for i fixnum from j below n
		   do (decf (aref b bi i) (* tau (aref a i j)))))))
	     
(defun qr-decompose (m)
  (declare (optimize (speed 3)(safety 1)))
  (declare (type (simple-array double-float (* *)) m))
  (let* ((n (array-dimension m 0))
	 (a (make-array (list n n) :element-type 'double-float))
	 (q (make-array (list n n) :element-type 'double-float :initial-element 0.0))
	 (r (make-array (list n n) :element-type 'double-float :initial-element 0.0))
	 (c (make-array n :element-type 'double-float :initial-element 0.0))
	 (d (make-array n :element-type 'double-float :initial-element 0.0))
	 )
    (declare (type (simple-array double-float (* *)) a q r))
    (declare (type (simple-array double-float (*)) c d))
    (declare (fixnum n))
    (copy-array-contents m a)
    (let ((singular (qrdcmp a c d)))
      (unless singular
	;; this solves for Q = A * Rinverse
	(loop for i fixnum from 0 below n
	      do (setf (aref r i i) (aref d i))
		 ;; set elements of r from a
		 (loop for j fixnum from (1+ i) below n
		       do (setf (aref r i j) (aref a i j)))
		 (setf (aref q i i) 1.0) ; setting row of identity matrix
		 ;; form Qt*Icol => Qcol.  When stored in a row we have a row of Q.
		 (qrbcalc a c q i))
	(values q r)))))

#|
(let ((p (transforms::projection-matrix (gui::3d-to-2d-projection (top-view)))))
  (mv-bind (r q) (qr-decompose p)
    (values (multiply-matrices r q)
	    p)))

;; this looks fine.

|#

#|
 *****************************  RQ-DECOMPOSITION  ****************************

THEORY:

   Given a procedure for QR decomposition, how do we do RQ decomposition?

   A = QR    decomposition using qrdcmp Numerical Recipes algorithm.

   Let Q'. R' be the QR-DECOMPOSITION. of inv(A)

   Then inv(R') . transpose(Q') = A

   Since R' is upper triangular and matrix inversion preserves the triangularity,
   
        R = inv(R')        is upper triangular
        Q = transpose(Q')  is orthonormal

        A = RQ  is the RQ decomposition of A.

THE DOWNSIDES OF THIS METHOD ARE:

    1. Two matrix inversions are required, vs none for the old version.
    2. QR decomposition algorithms for non-square matrices also exists, and the
       old version should also work for them.


|#

(defun rq-decompose (A &optional canonicalize-row)
  (format t "~%A Matrix = ~a" a)
  (mv-bind (qp rp) (qr-decompose (invert-matrix A))
    (let ((r (invert-matrix rp))
	  (q (transpose-matrix qp)))
      (canonicalize-rq-decomposition r q canonicalize-row)
      (values r q))))
   



#| 

OLD VERSION OF RQ DECOMPOSITION implemented before I realized the inv(A) QR-DECOMPOSITION approach. 

 *****************************  RQ-DECOMPOSITION  ****************************

NOTE:  I am sure that the Numerical Recipes QR decomposition algorithm
       can be reinvented to do RQ decomposition directly, but I am to lazy to do that.

THEORY:

   Given a procedure for QR decomposition, how do we do RQ decomposition?

   A = QR    decomposition using qrdcmp Numerical Recipes algorithm.

 Notation:
   
    NEGX(M) is horizontal mirroring of the elements of M.
    NEGY(M) is vertical mirroring of the elements of M.
    T(M) is transpose of M about the upper-left to lower-right axis.

   Relations between matrix operations:

     rule 1. T(A * B) = T(B) * T(A)

     rule 2. A * B = NEGX(A) * NEGY(B)

     rule 3: NEGY(A*B) = NEGY(A} * B

     rule 4: NEGY(NEGX(R)) = rot180(R)
    
   Let A' = T(NEGY(A)) => A = NEGY(T(A')
   
   T(NEGY(A)) is a 90 degree clockwise rotation of the elements of A.

   Let A' = Q' * R' be the QR decomposition of A'

   A = NEGY(T(A')) = NEGY(T(Q' * R'))

     = NEGY(T(R') * T(Q'))                from rule 1

     = NEGY(NEGX(T(R')) * NEGY(T(Q')))    from rule 2

     = NEGY(NEGX(T(R'))) * NEGY(T(Q'))    from rule 3
        
     = R'' * Q.

   R'' is upper-triangular since:
   
       R'' = NEGY(NEGX(T(R')) is a 180 degree rotation of T(R').  
       Since R' is upper-triangular, T(R') will be lower-triangular,
       and therefore the 180 degree rotation of T(R') will upper-triangular.

   Q = NEGY(T(Q'))  is a 90 degree counter-clockwise rotation of the elements of Q'.

We therefore have an R * Q decomposition of A.


|#


(defun matrix-transpose-ll-ur (m)
  (declare (type (simple-array double-float (* *)) m))
  (let* ((nrows (array-dimension m 0))
	 (ncols (array-dimension m 1))
	 (mt (make-array (list ncols nrows) :element-type (array-element-type m))))
    (declare (fixnum nrows ncols))
    (declare (type (simple-array double-float (* *)) mt))
    (loop for i fixnum from 0 below ncols
	  for i2 fixnum downfrom (1- ncols)
	  do (loop for j fixnum from 0 below nrows
		   for j2 fixnum downfrom (1- nrows)
		   do (setf (aref mt i j)
			    (aref m j2 i2))))
    mt))

(defun matrix-rotate-ccw (m)
  (declare (type (simple-array double-float (* *)) m))
  (let* ((nrows (array-dimension m 0))
	 (ncols (array-dimension m 1))
	 (mt (make-array (list ncols nrows) :element-type (array-element-type m))))
    (declare (fixnum nrows ncols))
    (declare (type (simple-array double-float (* *)) mt))
    (loop for i fixnum from 0 below ncols
	  for i2 fixnum downfrom (1- ncols)
	  do (loop for j fixnum from 0 below nrows
		   for j2 fixnum downfrom (1- nrows)
		   do (setf (aref mt i j)
			    (aref m j i2))))
    mt))

(defun matrix-rotate-cw (m)
  (declare (type (simple-array double-float (* *)) m))
  (let* ((nrows (array-dimension m 0))
	 (ncols (array-dimension m 1))
	 (mt (make-array (list ncols nrows) :element-type (array-element-type m))))
    (declare (fixnum nrows ncols))
    (declare (type (simple-array double-float (* *)) mt))
    (loop for i fixnum from 0 below ncols
	  for i2 fixnum downfrom (1- ncols)
	  do (loop for j fixnum from 0 below nrows
		   for j2 fixnum downfrom (1- nrows)
		   do (setf (aref mt i j)
			    (aref m j2 i))))
    mt))

(defun matrix-flip-x (m)
  (declare (type (simple-array double-float (* *)) m))
  (let* ((nrows (array-dimension m 0))
	 (ncols (array-dimension m 1))
	 (mt (make-array (list ncols nrows) :element-type (array-element-type m))))
    (declare (fixnum nrows ncols))
    (declare (type (simple-array double-float (* *)) mt))
    (loop for i fixnum from 0 below ncols
	  do (loop for j fixnum from 0 below nrows
		   for j2 fixnum downfrom (1- nrows)
		   do (setf (aref mt i j)
			    (aref m i j2))))
    mt))

(defun matrix-flip-y (m)
  (declare (type (simple-array double-float (* *)) m))
  (let* ((nrows (array-dimension m 0))
	 (ncols (array-dimension m 1))
	 (mt (make-array (list ncols nrows) :element-type (array-element-type m))))
    (declare (fixnum nrows ncols))
    (declare (type (simple-array double-float (* *)) mt))
    (loop for i fixnum from 0 below ncols
	  for i2 fixnum downfrom (1- ncols)
	  do (loop for j fixnum from 0 below nrows
		   for j2 fixnum downfrom (1- nrows)
		   do (setf (aref mt i2 j)
			    (aref m i j))))
    mt))



(defun rq-decompose-old (m &optional canonicalize-row)
  (multiple-value-bind (qp rp) (qr-decompose (matrix-rotate-cw m))
    (when qp
      (let ((r (matrix-transpose-ll-ur rp))
	    (q (matrix-rotate-ccw qp)))
	;;(math::print-matrix r) (math::print-matrix q)
	(canonicalize-rq-decomposition r q canonicalize-row)
	;;(math::print-matrix r) (math::print-matrix q)
	(values r q)))))

;;; this produces crummy results
(defun rq-decompose2 (m &optional canonicalize-row)
  (let* ((mmt (math::multiply-matrices-transposed2 m m))
	 (r (cholesky-decompose mmt))
	 (q (multiply-matrices (invert-matrix r) m))
	 (r (matrix-transpose-ll-ur r))
	 (q (matrix-rotate-ccw q))
	 )
    (canonicalize-rq-decomposition r q canonicalize-row)
    (values r q)))


;;; this produces crummy results
(defun rq-decompose2 (m &optional canonicalize-row)
  (let* ((mx (matrix-rotate-cw m))
	 (mmt (math::multiply-matrices-transposed2 mx mx))
	 (rx (cholesky-decompose mmt))
	 (qx (multiply-matrices (invert-matrix rx) mx))
	 (r (matrix-transpose-ll-ur rx))
	 (q (matrix-rotate-ccw qx))
	 )
   ; (canonicalize-rq-decomposition r q canonicalize-row)
    (values r q)))



#|
(setq math::*PRINT-MATRIX-DEFAULT-ELEMENT-FORMAT* "~12f ")

(multiple-value-setq (r q)
  (rq-decompose (setq p (projection-matrix (3d-to-2d-projection (top-view))))))

(let ((p (transforms::projection-matrix (transforms::3d-to-2d-projection (gui::top-view)))))
  (list (multiple-value-list (rq-decompose p))
	(multiple-value-list (rq-decompose-old p))))

(let ((p (transforms::projection-matrix (transforms::3d-to-2d-projection (gui::top-view)))))
  (mv-bind (r1 q1) (rq-decompose p)
    (mv-bind (r2 q2) (rq-decompose-old p)
      (values (matrix-max-abs-element (matrix-subtract r1 r2))
	      (matrix-max-abs-element (matrix-subtract q1 q2))))))

(print-matrix r)
(print-matrix q)
(matrix-subtract p (multiply-matrices r q))


(let* ((p (projection-matrix (3d-to-2d-projection (top-view))))
       (m (make-array '(4 4) :element-type 'double-float :initial-element 0.0)))
  (copy-matrix-rows p m '(0 1 3) '(0 1 2))
  (if nil
      (setf (aref m 3 0) 0.0 (aref m 3 1) 0.0 (aref m 3 2) 0.0 (aref m 3 3) 1.0)
      (setf (aref m 3 0) 0.0 (aref m 3 1) 0.0 (aref m 3 2) 1.0 (aref m 3 3) 0.0))
  (multiple-value-setq (r q) (rq-decompose m))
  (unless t ;r
    (copy-matrix-rows p m '(3) '(2))
    (setf (aref m 3 2) 0.0 (aref m 3 3) 1.0)
    (multiple-value-setq (r q) (rq-decompose m)))
    
  (print-matrix r)
  (determinant q))


(let* ((p (projection-matrix (3d-to-2d-projection (top-view))))
       (m (make-array '(4 4) :element-type 'double-float :initial-element 0.0)))
  (copy-matrix-rows p m '(0 1 3) '(0 1 3))
  (if t
      (setf (aref m 2 0) 0.0 (aref m 2 1) 0.0 (aref m 2 2) 0.0 (aref m 2 3) 1.0)
      (setf (aref m 2 0) 0.0 (aref m 2 1) 0.0 (aref m 2 2) 1.0 (aref m 2 3) 0.0))
  (multiple-value-setq (r q) (rq-decompose m))
   
  (print-matrix r)
  (determinant q))


(print-matrix (projection-matrix (3d-to-2d-projection (top-view))) )
|#


#|
 
--- ARCHIVE OF RULES
 
    T(A * B) = T(B) * T(A)

    A * B = NEGX(A) * NEGY(B)
    
    NEGX(A * B) = A * NEGX(B)

    NEGY(A * B) = NEGY(A) * B

    CW(A) = NEGX(T(A)) = T(NEGY(A))

    CCW(A) = T(NEGX(A)) = NEGY(T(A))

    CCW(A * B) = T(NEGX(A * B)) = 

    CCW(A * B) = CCW(B) * T(A)  

    NEGX(CCW(A)) = T-(A)

    NEGY(T(A)) = CCW(A)

    T-(M) = NEGX(T(NEGX(M)))

    T-(M) = NEGY(NEGX(T(M))

______________________________________
TESTS

(setq pmat
      (make-and-fill-2d-array
       '((0.0064930482258692395 0.9997721516874837 0.07361694880812925 -5065.350029502222)
	 (-1.0001973734151602 0.006944123671634666 0.04410998715449522 4637.178730787888)
	 (-0.002874338283744762 -0.0015720890292613599 0.9999946266863986 -11972.295618281258)
	 (1.5579586919309364E-6 8.521090859340529E-7 -5.420205162840362E-4 6.4892647209803))))

(matrix-subtract (matrix-rotate-ccw(multiply-matrices q r))
		       (multiply-matrices (matrix-rotate-ccw r)
						(transpose-matrix q)))
(qr-decompose pmat)
(time (multiple-value-setq (q r) (qr-decompose pmat)))
(matrix-subtract pmat (multiply-matrices q r))
(multiply-matrices q r)

(setq pm2 (matrix-rotate-cw pmat))
(multiple-value-setq (q r) (qr-decompose pm2))
(matrix-subtract pm2 (multiply-matrices q r))

(time (multiple-value-setq (r q) (rq-decompose pmat)))
(matrix-subtract pmat (multiply-matrices r q))
(time (multiple-value-setq (r q) (rq-decompose2 pmat)))
(matrix-subtract pmat (multiply-matrices r q))
(matrix-subtract (matrix-flip-y (multiply-matrices r q))
		       (multiply-matrices (matrix-flip-y r) q))
(matrix-subtract (matrix-flip-x (multiply-matrices r q))
		       (multiply-matrices r (matrix-flip-x q)))
(matrix-subtract (matrix-transpose-ll-ur m)
		       (matrix-flip-x (transpose-matrix (matrix-flip-x m))))
(matrix-subtract (multiply-matrices r q )
		       (multiply-matrices (matrix-flip-x r) (matrix-flip-y q)))
(matrix-subtract (matrix-transpose-ll-ur q)
		       (matrix-flip-x (matrix-flip-y (transpose-matrix q))))

|#
  

;;;  experiments  
;;; If r is the distance from the 3d-world origin to the projection center
;;; roff = 0.0 means put camera origin in matrix R,
;;;            leaving the 4th column of Q = <0,0,0,1>
;;; roff = 1.0 means put camera origin in matrix Q,
;;;            leaving the 4th column or R = <0,0,-1,0>

;;; This appears to work well, but I do not like the matrix inversion.
;;; This has been moved to cme/camera-models.lisp
;;;(defun decompose-projection-matrix2 (pmat &optional (roff 0.0))
;;;  (let ((p3 (copy-matrix-rows pmat nil '(0 1 3) '(0 1 2) 3)))
;;;    (multiple-value-bind (r3 q3) (rq-decompose p3 2)
;;;      ;;(setq merr (matrix-subtract p3 (multiply-matrices r3 q3)))
;;;      (ignore r3)
;;;      (let* ((q (make-array '(4 4) :element-type 'double-float :initial-element 0.0)))
;;;        (copy-matrix-rows q3 q '(0 1 2) '(0 1 2) 3)
;;;        ;;(setf (aref q 2 3) (/ (aref pmat 3 3) (aref r3 2 2)))
;;;        (setf (aref q 3 3) 1.0)
;;;        (let* ((r (multiply-matrices pmat
;;;                                           (if nil
;;;                                               (invert-matrix q)
;;;                                               (transpose-matrix q))))
;;;               (scale (/ (aref r 0 0))))
;;;          ;;(scale-matrix r scale)      ; force R00 to 1.0
;;;          ;;(setf (aref r 2 3) (/ (aref r 2 3) scale)) ; should be -1.0 ?
;;;          ;;#+never
;;;          (let ((inv (invert-matrix r))
;;;                (1-roff (- 1.0 roff))
;;;                )
;;;            (setf (aref q 0 3) (* roff (aref inv 0 2))
;;;                  (aref q 1 3) (* roff (aref inv 1 2))
;;;                  (aref q 2 3) (* roff (aref inv 2 2)))
;;;            (setf (aref r 1 0) 0.0
;;;                  (aref r 2 0) 0.0
;;;                  (aref r 2 1) 0.0
;;;                  (aref r 3 0) 0.0
;;;                  (aref r 3 1) 0.0)
;;;            (setf (aref r 0 3) (* 1-roff (aref r 0 3))
;;;                  (aref r 1 3) (* 1-roff (aref r 1 3))
;;;                  (aref r 3 3) (* 1-roff (aref r 3 3))))
;;;          (when (= roff 0.0)
;;;              (setf (aref q 0 3) 0.0
;;;                    (aref q 1 3) 0.0
;;;                    (aref q 2 3) 0.0))
;;;            (values r q)
;;;          )))))
     
#|
(defun 3x3-submatrix (m)
  (copy-matrix-rows m nil '(0 1 2) '(0 1 2) 3))

(defun decompose-projection-matrix2 (pmat &optional (roff 0.0))
  #+never
  (setf (aref q 0 3) 0.0
	(aref q 1 3) 0.0
	(aref q 2 3) 0.0)
  (multiple-value-bind (r q) (rq-decompose pmat 2)
    (unless (= roff 0.0)
      (multiple-value-bind (lu ps) (lu-decompose r)
	(let* ((r-col3 (cv (aref r 0 3) (aref r 1 3) (aref r 2 3) 0.0))
	       (p-col3 (lu-backsubstitute lu ps r-col3))
	       (1-roff (- 1.0 roff)))
	  (loop for i from 0 below 3
		do (incf (aref q i 3) (* roff (aref p-col3 i)))
		   (decf (aref r i 3) (* roff (aref r-col3 i))))))
      )
    (values r q)))


(defun decompose-projection-matrix2 (pmat &optional (roff 0.0))
  #+never
  (setf (aref q 0 3) 0.0
	(aref q 1 3) 0.0
	(aref q 2 3) 0.0)
  (multiple-value-bind (r q) (rq-decompose pmat 2)
    (unless (= roff 0.0)
      (multiple-value-bind (lu ps) (lu-decompose (3x3-submatrix r))
	(let* ((r-col3 (cv (aref r 0 3) (aref r 1 3) (aref r 2 3)))
	       (p-col3 (lu-backsubstitute lu ps r-col3)))
	  (loop for i from 0 below 3
		do (incf (aref q i 3) (* roff (aref p-col3 i)))
		   (decf (aref r i 3) (* roff (aref r-col3 i))))))
      )
    (values r q)))
|#


#|
(maybe-compile-file-load "/homedir/quam/cme/transforms/rq-decomposition.lisp")

(setq 3x4-mat (copy-matrix-rows
	       (projection-matrix (3d-to-2d-projection (top-view)))
	       nil '(0 1 3) '(0 1 2))
      p1 (make-instance '3X4-PROJECTION :projection-matrix 3x4-mat)
      pmat (projection-matrix p1))

(decompose-projection-matrix pmat)




(multiple-value-bind (r q) (decompose-projection-matrix2 pmat 1.0)
  (print-matrix r)
  (print-matrix q)
  (setq r2 r q2 q)
  nil
  )

(print-matrix pmat)
(print-matrix (invert-matrix r2))
(print-matrix (invert-matrix q2))
(print-matrix (multiply-matrices (invert-matrix q2)
					     (invert-matrix r2)))

(4x4-project-vector (multiply-matrices (invert-matrix q2)
					     (invert-matrix r2))
		    (cv 0.0 0.0 -100000000.0 1.0))


|#



