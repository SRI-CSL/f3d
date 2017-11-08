(in-package :math)


;;;(def-foreign-function (multiply_matrices (:name "math_multiply_matrices"))
;;;  (dim1 :int)
;;;  (com-dim :int)
;;;  (dim2 :int)
;;;  (mat1 (:simple-array :double-float))
;;;  (mat2 (:simple-array :double-float))
;;;  (result-mat (:simple-array :double-float)) )

#+never
(def-foreign-function (multiply_matrices (:name "math_multiply_matrices")
					  (:arg-checking nil))
  (dim1 :int)
  (com-dim :int)
  (dim2 :int)
  (mat1 (:array :double-float)) ; non-simple array is ok -- array-simple-vector gets the right thing.
  (mat2 (:array :double-float))
  (result-mat (:simple-array :double-float)) )

;;; better version for large matrices
(def-foreign-function (multiply_matrices (:name "math_multiply_matrices_v2")
					 (:arg-checking nil))
  (dim1 :int)
  (com-dim :int)
  (dim2 :int)
  (mat1 (:array :double-float)) ; non-simple array is ok -- array-simple-vector gets the right thing.
  (mat2 (:array :double-float))
  (result-mat (:simple-array :double-float)) )


(def-foreign-function (multiply_matrices_transposed1
		       (:name "math_multiply_matrices_transposed1")
		       (:arg-checking nil))
  (dim1 :int)
  (com-dim :int)
  (dim2 :int)
  (mat1 (:simple-array :double-float))
  (mat2 (:simple-array :double-float))
  (result-mat (:simple-array :double-float)) )

(def-foreign-function (multiply_matrices_transposed2
		       (:name "math_multiply_matrices_transposed2")
		       (:arg-checking nil))
  (dim1 :int)
  (com-dim :int)
  (dim2 :int)
  (mat1 (:simple-array :double-float))
  (mat2 (:simple-array :double-float))
  (result-mat (:simple-array :double-float)) )


;;; This sucks -- too much overhead in FFI
;;; assumes bottom row of mat is 0 0 0 1
(def-foreign-function (invert_homogeneous_4x4_matrix
		       (:name "math_invert_homogeneous_4x4_matrix")
		       (:arg-checking nil))
    (mat (:simple-array :double-float))	; double 4x4
  (inv (:simple-array :double-float))	; double 4x4
  )

;;; This sucks -- too much overhead in FFI
(def-foreign-function (invert_3x3_matrix
		       (:name "math_invert_3x3_matrix"))
    (mat (:simple-array :double-float)) ; double 3x3
  (inv (:simple-array :double-float))	; double 3x3
  )


(def-foreign-function (math_invert (:name "math_invert")
				   (:arg-checking nil))
    (a (:simple-array :double-float))
  (n :int)
  (inv (:simple-array :double-float))
  (indx (:simple-array :signed-32bit))
  (tiny :double-float)
  (col (:simple-array :double-float))	; col is a vector of n elements
  )


;;; LU DECOMPOSE SOLVE


(def-foreign-function (math_ludcmp (:name "math_ludcmp")
				   (:arg-checking nil))
    (a (:simple-array :double-float))
  (n :int)
  (indx (:simple-array :signed-32bit))
  (darr (:simple-array :double-float))
  (tiny :double-float)
  (vv (:simple-array :double-float)))	; vv is a vector of n elements

(def-foreign-function (math_lubksb (:name "math_lubksb")
				    (:arg-checking nil))
    (a (:simple-array :double-float))
  (n :int)
  (indx (:simple-array :signed-32bit))
  (col (:simple-array :double-float))
  (tiny :double-float))


(defparameter *math_lu_solve_tiny* 0.0)

(defun double-float-lu-decompose (a &optional lu ps (tiny *math_lu_solve_tiny*))
  (declare (type square-dmatrix a))
  (declare (type (or null square-dmatrix) lu))
  (type-check square-dmatrix a)
  (type-check (or null square-dmatrix) lu)
  (let* ((n (array-dimension a 0))
	 (element-type (array-element-type a))
	 (darr (make-array 1 :element-type element-type))
	 (vv (make-array n :element-type 'double-float)))
    (unless lu (setq lu (make-array (array-dimensions a) :element-type element-type)))
    (unless ps (setq ps (make-array n :element-type '(signed-byte 32))))
    (unless (eq lu a) (copy-matrix a lu))
    (lcl::with-interrupts-deferred
	(math_ludcmp lu n ps darr tiny vv))
    (values lu ps (aref darr 0))) )

(defun double-float-lu-backsubstitute (lu ps b &optional x (tiny *math_lu_solve_tiny*))
  (declare (type square-dmatrix lu))
  (declare (type (or dmatrix dvector) b))
  (type-check square-dmatrix lu)
  (type-check (or dmatrix dvector) b)
  (let* ((n (array-dimension lu 1))
	 (element-type (array-element-type lu)))
    (unless x (setq x (make-array n :element-type element-type)))
    (unless (eq x b) (vector-copy b x))
    (lcl::with-interrupts-deferred
	(math_lubksb lu n ps x tiny))
    x))


(defun invert-matrix (matrix &optional into-matrix (tiny *math_lu_solve_tiny*))
  (declare (type square-dmatrix matrix))
  (declare (type (or null square-dmatrix) into-matrix))
  (type-check square-dmatrix matrix)
  (type-check (or null square-dmatrix) into-matrix)
  (let* ((n (array-dimension matrix 0))
	 (m2 (copy-matrix matrix))
	 (index (make-array n :element-type '(signed-byte 32) :initial-element 0))
	 (col (make-array n :element-type 'double-float :initial-element 0.0d0))
	 )
    (unless into-matrix
      (setq into-matrix (make-array (list n n) :element-type 'double-float
				    :initial-element 0.0d0)))
    (let ((result 
	   (lcl::with-interrupts-deferred (math_invert m2 n into-matrix index tiny col))))
      (when (eql 0 result) (error "Singular Matrix"))
      into-matrix)))


;;; EIGENVALUES and EIGENVECTORS

;;; math_tred2 decomposes positive-definite symmetric matrix A into
;;; orthonormal matrix E and diagonal (vector) d, such that
;;; A = Z d Zt
;;; Operation count is 3n^3 
(def-foreign-function (math_tred2 (:name "math_tred2")
				  (:arg-checking nil))
    (a (:simple-array :double-float))
  (n :int)
  (d (:simple-array :double-float ))
  (z (:simple-array :double-float ))
  (eigenvectors-p :int))


;;; Operation count is 4n^3/3
(def-foreign-function (math_tqli (:name "math_tqli")
				 (:arg-checking nil))
    (d (:simple-array :double-float))
  (e (:simple-array :double-float))
  (n :int)
  (z (:simple-array :double-float)))

;;; Operation count is approx 4n^3  (3n^3 for math_tred2 and 4n^3/3 for math_tqli)
;;;eigenvectors are COLUMNS of z.
;;; Q*L*Qt = A, where Q are the eigenvectors and L is the vector of eigenvalues.
(defun eigenvalues-and-vectors (A)
  (declare (type square-dmatrix A))
  (type-check square-dmatrix A)
  (let* ((n (array-dimension A 0))
	 (z (copy-matrix A))
	 (d (make-array n :element-type 'double-float))
	 (e (make-array n :element-type 'double-float)))
    (math_tred2 z n d e 1)
    (math_tqli d e n z)
    (values d				; eigenvalues
	    z				; eigenvectors in COLUMNS of z.
	    )))


(defun eigen-solve (eigenvalues eigenvectors rhs &optional x)
  (let* ((n (length rhs))
	(tmp (make-array n :element-type 'double-float)))	
    (unless x (setq x (make-array n :element-type 'double-float)))
    (math_svbksb eigenvectors eigenvalues eigenvectors n n rhs x tmp)
    x))


;;; Faster function  (produces a different result) for this is CHOLESKY-DECOMPOSE
;;; This returns matrix S such that S*St = A.
(defun eigen-matrix-sqrt (a)
  (declare (type square-dmatrix a))
  (type-check square-dmatrix a)
  (multiple-value-bind (d z) (eigenvalues-and-vectors a)
    (declare (type (simple-array double-float (* *)) z)
	     (type (simple-array double-float (*)) d))
    (let* ((n (length d))
	   (s (make-array (list n n) :element-type 'double-float)))
      (declare (type (simple-array double-float (* *)) s))
      (declare (fixnum n))	       
      (loop for col fixnum from 0 below n
	    for dc double-float = (sqrt (aref d col))
	    do (loop for row fixnum from 0 below n
		     do (setf (aref s row col) (* dc (aref z row col)))))
      s)))


;;; WARNING:  This returns matrix S such that S*S = M, NOT S*St = M as with eigen-matrix-sqrt. 
;;; this presumes that m is symmetric (positive definite ?)
;;; since m = v w vt
;;; sqrt(m) = v sqrt(w) vt
(defun matrix-sqrt (m)
  (declare (type square-dmatrix m))
  (type-check square-dmatrix m)
  (multiple-value-bind (w v) (eigenvalues-and-vectors m)
    (let* ((n (array-dimension v 0))
           (sqrt-m (make-array (list n n) :element-type 'double-float))
           (sqrt-w (make-array n :element-type 'double-float)))
      (declare (type (simple-array double-float (* *)) sqrt-m)
               (type (simple-array double-float (*)) sqrt-w))
      (loop for i from 0 below n
            do (setf (aref sqrt-w i) (sqrt (aref w i))))
      (loop for j fixnum from 0 below n
            do (loop for k fixnum from 0 below n
                     do (setf (aref sqrt-m j k)
                              (the double-float
                                  (loop for i fixnum from 0 below n
                                        for wi double-float = (aref w i)
                                        unless (= wi 0.0)
                                          sum (* (aref v j i) (aref sqrt-w i) (aref v k i))
                                        double-float)))))
      sqrt-m)))
  
;;; SINGULAR VALUE DECOMPOSITION

(def-foreign-function (math_svdcmp (:name "math_svdcmp")
				   (:arg-checking nil))
    (a (:simple-array :double-float))
  (m :int)
  (n :int)
  (w (:simple-array :double-float))
  (v (:simple-array :double-float))
  (rv1 (:simple-array :double-float)) ; rv1 is a vector of n elements
  )

(def-foreign-function (math_svbksb (:name "math_svbksb")
				   (:arg-checking nil))
    (u (:simple-array :double-float))
  (w (:simple-array :double-float))
  (v (:simple-array :double-float))
  (m :int)
  (n :int)
  (b (:simple-array :double-float))
  (x (:simple-array :double-float))
  (tmp (:simple-array :double-float)) ; tmp is a vector of n elements
)

;;; NR SVD decomposition: Decompose a matrix A = U [w] V(T) where w is
;;; a vector of eigenvalues (singular values)
(defun svd-decompose (a &optional u w v)
  (declare (optimize (speed 3) (safety 1)))
  (let* ((m (array-dimension a 0))
	 (n (array-dimension a 1))
	 (rv1 (make-array n :element-type 'double-float)))
    (unless u (setq u (make-array (list m n) :element-type 'double-float)))
    (unless (eq u a) (copy-array-contents a u))
    (unless v (setq v (make-array (list n n) :element-type 'double-float)))
    (unless w (setq w (make-array n :element-type 'double-float)))

    (math_svdcmp u m n w v rv1)
    (values u w v)))

(defparameter *sv-suppress-print-supression-count* nil)

(defun svd-suppress (vect &key abs-threshold rel-threshold)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type (simple-array double-float (*)) vect))
  (when rel-threshold
    (setq abs-threshold
	  (* rel-threshold
	     (loop for i fixnum from 0 below (length vect)
		   maximize (abs (aref vect i))))))
  
  (loop for i fixnum from 0 below (length vect)
	when (< (abs (aref vect i)) abs-threshold)
	  do (setf (aref vect i) 0.0)
	  and collect i into suppressed-indices
	finally (when *sv-suppress-print-supression-count*
		  (format t ";;; sv-suppress count = ~a~%" suppressed-indices))
		(return (values vect suppressed-indices))))


(defun svd-suppress2 (u w b)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type (simple-array double-float (* *)) u))
  (declare (type (simple-array double-float (*)) b w))
  (let* ((n (array-dimension u 1))
	 (m (array-dimension u 0))
	 (utb (make-array n :element-type 'double-float)))
    (declare (type (simple-array double-float (*)) utb))
    (loop for j fixnum from 0 below n
	  do (setf (aref utb j)
		   (the double-float
		       (loop for i fixnum from 0 below m
			     sum (* (aref u i j) (aref b i)) double-float))))

    (let* ((l (loop for i fixnum from 0 below n
		    collect (cons (aref w i) i)))
	   (sorted-l (sort l #'> :key #'car))
	   (uutb (make-array m :element-type 'double-float :initial-contents b))
	   (resids
	    (loop with 1/m double-float = (/ (dfloat m))
		  for j fixnum from (1- n) downto 0
		  for (wi . i2) in sorted-l
		  for i fixnum = i2
		  do (ignore wi)
		  collect
		  (sqrt (* 1/m 
			   (the double-float
			       (loop for k fixnum from 0 below m
				     for new-elem double-float = (- (aref uutb k)
								    (* (aref u k i) (aref utb i)))
				     do (setf (aref uutb k) new-elem)
				     sum (^2 new-elem) double-float))))))
	   )
      (declare (type (simple-array double-float (*)) uutb))
      (loop with l
	    for resid in resids
	    for (wi . i) in sorted-l
	    do (push (list i resid wi) l)
	    finally (return l))))) 
#|

(setq cme::*rational-polynomial-sv-rel-threshold* 1e-6)
(setq cme::*rational-polynomial-sv-rel-threshold* 3e-5)
(destructuring-bind (u w v b) (nth 0 cme::sv-foo)
  (svd-suppress2 u w b))

(destructuring-bind (u w v b) (nth 2 cme::sv-foo)
  (sv-resid u w b))


(defun sv-resid (u w b)
  (* (sqrt (/ 1.0 (length b)))
     (vector-euclidean-length
      (vector-difference (math::multiply-matrices
			  (math::multiply-matrices u (math::transpose-matrix u))
			  b)
			 b))))
|#
;;; This was previously called sv-solve	
(defun svd-backsubstitute (u w v b &optional x)
  (let* ((m (array-dimension u 0))
	 (n (array-dimension u 1))
	 (tmp (make-array n :element-type 'double-float))
	 )
    (unless x (setq x (make-array n :element-type 'double-float)))
    (math_svbksb u w v m n b x tmp)
    x))

  
;;; **************************   INTEGRATION OF FUNCTIONS   **************************

(defun nr-trapzd (fn a b n)
  (declare (optimize (speed 3) (safety 1)))
  (declare (fixnum n))
  (declare (double-float a b))
  (let ((fn (if (symbolp fn) (symbol-function fn) fn)))
    (declare (type function fn))
    (macrolet ((truely (type x)
		 #+cmu `(ext:truly-the ,type ,x)
		 #+sbcl `(sb-ext:truly-the ,type ,x)
		 #-(or :cmu :sbcl) `(the ,type ,x)))
		       
      (if (= n 1)
	  (* .5 (- b a) (+ (truely double-float (funcall fn a))
			   (truely double-float (funcall fn b))))
	  (loop with it fixnum = (the fixnum (ash 1 (the (integer 0 20) (1- n))))
		with tmn of-type double-float = (dfloat it)
		with del of-type double-float = (/ (- b a) tmn)
		for j fixnum from 1 to it
		for x of-type double-float from (+ a (* .5 del)) by del
		for f of-type double-float = (funcall fn x)
		sum f into sum double-float
		finally (return (* .5 sum del)))))))

	   	    
(defun nr-polint (xa ya n x index-off)
  (declare (optimize (speed 3) (safety 1)))
  (declare (fixnum n index-off))
  (declare (type (simple-array double-float (*)) xa ya))
  (declare (double-float x))
  (let ((c (make-array n :element-type 'double-float))
	(d (make-array n :element-type 'double-float))
	(ns (loop with dif double-float = (abs (- x (aref xa index-off)))
		  with ns fixnum = 0
		  for i fixnum from 0 below n
		  for dift double-float = (abs (- x (aref xa (+ i index-off))))
		  when (< dift dif)
		    do (setf dif dift ns i)
		  finally (return ns))))
    (declare (type (simple-array double-float (*)) c d))
    (declare (fixnum ns))

    (decf ns)

    (loop for i fixnum from 0 below n
	  for j fixnum = (+ i index-off)
	  for yi double-float = (aref ya j)
	  do (setf (aref c i) yi (aref d i) yi))
    
    (loop with y double-float = (aref ya (+ ns index-off 1))
	  with dy double-float 
	  for m fixnum from 1 below n
	  do (loop for i fixnum from 0 below (- n m)
		   for io fixnum from index-off
		   for ho double-float = (- (aref xa io) x)
		   for hp double-float = (- (aref xa (+ io m)) x)
		   for w double-float = (- (aref c (1+ i)) (aref d i))
		   for den double-float = (- ho hp)
		   when (= den 0.0)
		     do (error "Error")
		   do (setq den (/ w den))
		      (setf (aref d i) (* hp den)
			    (aref c i) (* ho den)))
		
	     (setf dy (if (< (* 2 (1+ ns)) (- n m))
			  (aref c (1+ ns))
			  (prog1 (aref d ns) (decf ns))))
	     (incf y dy)
	  finally (return (values y dy)))))
	
	
(defun nr-romberg-integral (fn a b &key (eps 1e-6) (jmax 20) (jmaxp (1+ jmax)) (k 5))
;;  (declare (optimize (speed 3) (safety 1)))
;;  (declare (fixnum jmax jmaxp k))
;;  (declare (double-float a b eps))
  (if (= a b)
      0.0
      (let ((s (make-array (1+ jmaxp) :element-type 'double-float))
	    (h (make-array (1+ jmaxp) :element-type 'double-float)))
	(declare (type (simple-array double-float (*)) s h))
	(setf (aref h 0) 1.0)
	(loop for j fixnum from 0 below jmax
	      for ss of-type double-float = (nr-trapzd fn a b 1)
		then (+ (* .5 ss) (nr-trapzd fn a b (+ j 1)))
	      do (setf (aref s j) ss)
		 (when (>= (1+ j) k)
		   (multiple-value-bind (ss dss) (nr-polint h s k 0.0 (1+ (- j k)))
		     ;;(format t "nr-romberg-integral dss = ~a~%" dss)
		     (when (< (abs dss) (* eps (abs ss)))
		       ;;(format t ";;; nr-romberg-integral ~d iters~%" (1+ j))
		       (return (values ss (1+ j))))))

		 (setf (aref s (1+ j)) (aref s j)
		       (aref h (1+ j)) (* .25 (aref h j)))
	      finally (error "Too many steps.")))))


(defun gauss-density (x)
  (declare (optimize (speed 3) (safety 1)))
  (declare (double-float x))
  (* #.(/ (sqrt (* 2.0 pi))) (exp (* -.5 x x))))

;;; integral of gauss density function from -infinity to x.
(defun cumulative-gauss (x)
  (declare (optimize (speed 3) (safety 1)))
  (declare (double-float x))
  (if (< x 0.0)
      (- .5 (nr-romberg-integral #'gauss-density 0.0 (- x)))
      (+ .5 (nr-romberg-integral #'gauss-density 0.0 x))))



;;; **************************   INCOMPLETE GAMMA FUNCTION   **************************

(def-foreign-function (math_gammln (:name "math_gammln") (:return-type :double-float)
				    (:arg-checking nil))
    (x :double-float))


(defun gser (a x &optional (itmax 200) (eps 3e-7))
  (declare (optimize (speed 3) (safety 1)))
  (declare (double-float a x eps))
  (declare (fixnum itmax))
  (if (<= x 0.0)
      (if (< x 0.0)
	  (error "x less than 0.0")
	  0.0)
      (loop with del of-type double-float = (/ a)
	    with sum of-type double-float = del
	    for i fixnum from 0 below itmax
	    for ap of-type double-float from (+ a 1.0) by 1.0
	    do (setq del (* del (/ x ap)))
	       (incf sum del)
	    when  (< (abs del) (* eps (abs sum)))
	      return (* sum (exp (- (* a (log x)) x (math_gammln a))))
	    finally (error "Argument A too large, ITMAX too small in routine GSER. ~a"
			   (list del sum)
			   ))))


(defun gcf (a x &optional (itmax 200) (eps 3e-7) (fpmin 1e-30))
  (declare (optimize (speed 3) (safety 1)))
  (declare (double-float a x eps fpmin))
  (declare (fixnum itmax))
  (loop for i fixnum from 1 below itmax
	with b of-type double-float = (+ x 1.0 (- a))
	with c of-type double-float = (/ fpmin)
	with d of-type double-float = (/ b)
	with h of-type double-float = d
	with del of-type double-float
	for an of-type double-float = (* i (- a i))
	do (incf b 2.0)
	   (setq d (+ (* d an) b))
	   (when (< (abs d) fpmin) (setq d fpmin))
	   (setq c (+ b (/ an c)))
	   (when (< (abs c) fpmin) (setq c fpmin))
	   (setq d (/ d))
	   (setq del (* d c))
	   (setq h (* h del))
	when (< (abs (- del 1.0)) eps)
	  return (* h (exp (- (* a (log x)) x (math_gammln a))))
	finally (error "Argument A too large, ITMAX too small in routine GCF. ~a"
		       (list del h))))

(defun gammq (a x)
  (declare (optimize (speed 3) (safety 1)))
  (declare (double-float a x))
  (cond ((or (< x 0.0) (<= a 0.0))
	 (error "Invalid arguments in gammq")
	 ;;(format t "Invalid arguments in gammq :~a ~a~%" a x)
	 0.0)
	((< x (1+ a))
	 (- 1.0 (gser a x)))
	(t (gcf a x))))
	 
(defun gammp (a x)
  (declare (optimize (speed 3) (safety 1)))
  (declare (double-float a x))
  (cond ((or (< x 0.0) (<= a 0.0))
	 (error "Invalid arguments in gammp ~a" (list a x))
	 ;;(format t "Invalid arguments in gammp :~a ~a~%" a x)
	 0.0)
	((< x (1+ a))
	 (gser a x))
	(t (- 1.0 (gcf a x)))))

	 
;;; this is the complement of chi-sq probability as usually tabulated.
(defun chi-sq-prob-q (chsq dof)
  (declare (optimize (speed 3) (safety 1)))
  (declare (double-float chsq dof))
  (gammq (* .5 dof) (* .5 chsq)))

(defun chi-sq-prob (chsq dof)
  (declare (optimize (speed 3) (safety 1)))
  (declare (double-float chsq dof))
  (gammp (* .5 dof) (* .5 chsq)))

#|
(gammp 1.0 1.0)
(chi-sq .95 0.0)  ;Error in function GAMMP:  Invalid arguments in gammp

(chi-sq-prob 10.851 20.0)    0.050004656784553014
(chi-sq-prob-q 10.851 20.0)  0.949995343215447
(chi-sq .05 20.0)            10.850811548512143

(chi-sq-prob 31.410 20.0)    0.9499947608846163
(chi-sq-prob-q 31.410 20.0)  0.050005239115383696
(chi-sq .95 20.0)            31.41043283700518

For some reason, these chi-sq functions give slightly different (and better) results
than the C++ implementation in /opt/IU/QUAM/least-squares/solve.c++

Chi_sq_prob(22.530868, 20) = 0.312412
(chi-sq-prob 22.530868 20.0)
(chi-sq-prob-q 22.530868 20.0) = 0.3124123020010026
sigma0 = 1.218273, Chi_sq_prob(29.683770, 20) = 0.075142
Chi_sq(0.95, 20) = 10.850819, Chi_sq = 29.683770, Chi_sq(0.05, 20) = 31.409128
(chi-sq-prob-q 31.409128 20.0)

(chi-sq-prob 3.940299 10.0) = .05
|#

;;; **********************************  SOLVE-WITH-SECANT-METHOD  **********************************

;;; These functions belong in a different file.


;;;(defun solve-with-secant-method (fn x0 &optional x1 (eps 1e-5) (max-iters 20) true-secant-method)
;;;  (declare (optimize (speed 3) (safety 1)))
;;;  (declare (double-float x0 eps)
;;;           (type (or null double-float) x1)
;;;           (fixnum max-iters))
;;;  (unless x1 (setq x1 (1+ x0)))
;;;  (let ((f0 (funcall fn x0))
;;;        (f1 (funcall fn x1)))
;;;    (declare (double-float f0 f1))
;;;    (loop for iters fixnum from 1
;;;          for df of-type double-float = (- f1 f0)
;;;          when (or (< (abs f1) eps)
;;;                   (> iters max-iters)
;;;                   (= df 0.0)
;;;                   ;;(< (abs df) eps)
;;;                   )
;;;            return (values x1 f1 iters)
;;;          for x2 of-type double-float = (- x1 (* f1 (/ (- x1 x0) df)))
;;;          for f2 of-type double-float = (funcall fn x2)
;;;          ;; true secant method can have very slow convergence -- see first example below
;;;          when (or (not true-secant-method) (< (* f2 f1) 0.0)) ; retain previous point with opposite function sign 
;;;            do (setq x0 x1 f0 f1)       ; retain x1, f1  otherwise retain x0, f0
;;;          do (setq x1 x2 f1 f2)
;;;             ;;do (format t "solve-with-secant-method x0 = ~a f0= ~a x1= ~a f1= ~a~%" x0 f0 x1 f1)
;;;          )))

;;; A constraint function 
#+never ;; broken in sbcl
(defun solve-with-secant-method-constrained (fn constraint x0 &optional x1 (convergence-threshold 1e-5) (max-iters 10) true-secant-method)
 ; (declare (optimize (speed 3) (safety 1)))
  (declare (double-float x0 convergence-threshold)
	   ;;(type (or null double-float) x1)
	   (fixnum max-iters))
  (unless x1 (setq x1 (1+ x0)))
  (let ((f0 (funcall fn x0))
	(f1 (funcall fn x1)))
    (declare (double-float f0 f1))
    (loop for iters fixnum from 1
	  for df of-type double-float = (- f1 f0)
	  when (or (< (abs f1) convergence-threshold)
		   (> iters max-iters)
		   (= df 0.0)
		   ;;(< (abs df) convergence-threshold)
		   )
	    return (values x1 f1 iters)
	  for x2a of-type double-float = (- x1 (* f1 (/ (- x1 x0) df)))
	  for x2b = (if constraint (funcall constraint x2a) x2a)
	  for x2 of-type double-float = (or x2b (return-from solve-with-secant-method-constrained nil))
	  for f2 of-type double-float = (funcall fn x2)
	  ;; true secant method can have very slow convergence -- see first example below
	  when (or (not true-secant-method) (< (* f2 f1) 0.0)) ; retain previous point with opposite function sign 
	    do (setq x0 x1 f0 f1)	; retain x1, f1  otherwise retain x0, f0
	  do (setq x1 x2 f1 f2)
	  ;;do (format t "solve-with-secant-method x0 = ~a f0= ~a x1= ~a f1= ~a~%" x0 f0 x1 f1)
	  )))


(defun solve-with-secant-method-constrained (fn constraint x0 &optional x1 (convergence-threshold 1e-5) (max-iters 10) true-secant-method)
  ;; (declare (optimize (speed 3) (safety 1)))
  (declare (double-float x0 convergence-threshold)
	   ;;(type (or null double-float) x1)
	   (fixnum max-iters))
  (unless x1 (setq x1 (1+ x0)))
  (let ((f0 (funcall fn x0))
	(f1 (funcall fn x1))
	(iters 1))
    (declare (double-float f0 f1)
	     (fixnum iters))
    (loop for df of-type double-float = (- f1 f0)
	  until (or (< (abs f1) convergence-threshold)
		    (> iters max-iters)
		    (= df 0.0)
		    ;;(< (abs df) convergence-threshold)
		    )
	  for x2a of-type double-float = (- x1 (* f1 (/ (- x1 x0) df)))
	  for x2b = (if constraint (funcall constraint x2a) x2a)
	  for x2 of-type double-float = (or x2b (return-from solve-with-secant-method-constrained nil))
	  for f2 of-type double-float = (funcall fn x2)
	  ;; true secant method can have very slow convergence -- see first example below
	  when (or (not true-secant-method) (< (* f2 f1) 0.0)) ; retain previous point with opposite function sign 
	    do (setq x0 x1 f0 f1) ; retain x1, f1  otherwise retain x0, f0
	  do (setq x1 x2 f1 f2)
	     ;;do (format t "solve-with-secant-method x0 = ~a f0= ~a x1= ~a f1= ~a~%" x0 f0 x1 f1)
	     (incf iters)
	  )
    (values x1 f1 iters)))

(defun solve-with-secant-method (fn x0 &optional x1 (convergence-threshold 1e-5) (max-iters 20) true-secant-method)
  (solve-with-secant-method-constrained fn nil x0 x1 convergence-threshold max-iters true-secant-method))

#|
(cumulative-gauss .9)

(solve-with-secant-method #'(lambda(x) (* (- x 1.0) (- x -1))) 0.0 -5.0 1e-15 20)
(solve-with-secant-method #'(lambda(x) (* (- x 1.0) (- x -1))) .5 -5.0 1e-15 20)

double-float-epsilon = 1.1102230246251568E-16
(setq x (multiple-value-list (solve-with-secant-method #'(lambda(x) (- (sin x) .5)) 0.0 -.005 1e-15)))

(asin .5)
(sin (car x))

(chi-sq .5 3.0)

(chi-sq-prob .5 3.0)
(chi-sq 0.0811085882988202 3.0)
(chi-sq 0.95 10)
(chi-sq 0.05 10)
|#



;;; This is clearly sub-optimal, but Numerical Recipes doesn't have a function to do this:
;;; For 5 digits of precision, this requires about 4 iterations.
(defun chi-sq (prob dof)
  (let ((dof (dfloat dof)))
    (solve-with-secant-method-constrained
     #'(lambda (chi-sq) (- prob (chi-sq-prob chi-sq dof)))
     #'(lambda(x) (if (< x 0.0) 0.0 x))
     dof
     nil 1d-12 20
     )))


;;;(defun chi-sq (prob dof)
;;;  (let ((1-prob (- 1.0 prob)))
;;;    (solve-with-secant-method-constrained
;;;     #'(lambda (chi-sq) (- prob (chi-sq-prob-q chi-sq dof)))
;;;     #'(lambda(x) (if (< x 0.0) 0.0 x))
;;;     (dfloat dof)
;;;     nil 1e-12 20
;;;     )))


