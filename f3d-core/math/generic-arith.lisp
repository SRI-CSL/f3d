(in-package :math)

#|

GENERIC ARITHMETIC involving numbers, vectors, and matrices.

Should the generic function names be G+, G-, g* vs M+, M-, and M* ?

|#

(declaim (optimize (speed 3) (safety 1)))

(eval-when (eval load compile)

(defun left-associative-operator-expand (op x args)
  (if (null args)
      x
      (left-associative-operator-expand op `(,op ,x ,(car args)) (cdr args))))

(defun right-associative-operator-expand (op x args)
  (if (null args)
      x
      `(,op ,x ,(right-associative-operator-expand op (car args) (cdr args)))))

) ; end eval-when

(defparameter *unsupported-types-error-string*
  "Unsupported combination of types")

(defun generic-op-error (fn-name &rest args)
  (error "~a was called with an unsupported combination of argument types: ~a" 
	 fn-name (mapcar #'type-of args)))



;;; ***************************  G+ = GENERIC-PLUS  ***************************

;(fmakunbound 'g+)
(defmacro g+ (x &rest args)
  (right-associative-operator-expand 'generic-plus x args))

;(g* (make-4x4-identity-matrix) (cv 0.0 )) => error
;(g* (make-4x4-identity-matrix) (cv 0.0 1.0 2.0 3.0))
;(g* (make-4x4-identity-matrix) (make-array '(4 4)))
;(g+ (make-4x4-identity-matrix) (cv 0.0 1.0 2.0 3.0))
;(g* 2 (cv 0.0 1.0 2.0 3.0))
;(g* (sqrt -1.0) 2)


;;; GENERIC-ADD with typecase arg dispatching

(defgeneric generic-+ (x y))

(defun generic-plus (x y)
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 1)))
  (declare (optimize (speed 3) (safety 1)))
  (typecase x
    (number (+ x y))
    (dvector (vector-add x y)) ; assume that the 
    (dmatrix (matrix-add x y))
    (otherwise (generic-+ x y))
    ;;(otherwise (generic-op-error 'generic-plus x y))
    ))

;;; ****************************  G- = GENERIC-MINUS  ****************************

(defmacro g- (x &rest args)
  (if (null args)
      `(generic-negate ,x)
      `(generic-subtract ,x 
			 (g+ ,(car args) ,@(cdr args)))))

(defun generic-negate (x)
  (declare (optimize (speed 3) (safety 1)))
  (typecase x
    (number (- x))
    (gvector (vector-negate x))
     (gmatrix (matrix-negate x))
    (otherwise (generic-op-error 'generic-negate x))))

(defun generic-subtract (x y)
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 1)))
  (declare (optimize (speed 3) (safety 1)))
  (typecase x
    (number (- x y))
    (dvector (vector-difference x y))
    (dmatrix (matrix-difference x y))
    (otherwise (generic-op-error 'generic-subtract x y))))

	    
;;; ***************************  G* = GENERIC-TIMES  ***************************

(declaim (inline transpose))
(defun transpose (x)
  (matrix-transpose x))

(declaim (inline inverse))
(defun inverse (x)
  (invert-matrix x))

;(fmakunbound 'g*)
(defmacro g* (x &rest args)
  (right-associative-operator-expand 'generic-times x args))

;;; Experiment

;;; This is an optimization to avoid unnecessary calls to transpose-matrix
;;; and uses (lu-solve A B) to compute (matrix-multiply (inverse A) B).

(defmacro g* (x &rest args)
  (mostly-right-associative-g*-expand 'generic-times x args))

(eval-when (eval load compile)

(defun left-associative-g*-expand (op x args)
  (if (null args)
      x
      (cond ((and (consp x) (eq (car x) 'transpose))
	     (left-associative-g*-expand op `(generic-times-transposed1 ,(cadr x) ,(car args)) (cdr args)))
	    ((and (consp x) (eq (car x) 'inverse))
	     (left-associative-g*-expand op `(lu-solve ,(cadr x) ,(car args)) (cdr args)))
	    (t (left-associative-g*-expand op `(,op ,x ,(car args)) (cdr args))))))

;;; This works better with TRANSPOSE and INVERSE
(defun right-associative-g*-expand (op x args)
  (if (null args)
      x
      (cond ((and (consp x) (eq (car x) 'transpose))
	     `(generic-times-transposed1 ,(cadr x) 
					 ,(right-associative-g*-expand op (car args) (cdr args))))
	    ((and (consp x) (eq (car x) 'inverse))
	     `(lu-solve ,(cadr x) ,(right-associative-g*-expand op (car args) (cdr args))))
	    (t `(,op ,x ,(right-associative-g*-expand op (car args) (cdr args)))))))

;;; This works better yet with TRANSPOSE and INVERSE
(defun mostly-right-associative-g*-expand (op x args)
  (if (null args)
      x
      (cond ((and (consp x) (eq (car x) 'transpose))
	     `(generic-times-transposed1 ,(cadr x) 
					 ,(mostly-right-associative-g*-expand op (car args) (cdr args))))
	    ((and (consp x) (eq (car x) 'inverse))
	     `(lu-solve ,(cadr x) ,(mostly-right-associative-g*-expand op (car args) (cdr args))))
	    ((and (consp (car args)) (eq (car (car args)) 'transpose))
	     (mostly-right-associative-g*-expand op 
					  `(generic-times-transposed2 ,x ,(cadr (car args)))
					  (cdr args)))
	    (t `(,op ,x ,(mostly-right-associative-g*-expand op (car args) (cdr args)))))))

) ; end eval-when

(defun generic-times-transposed1 (x y)
  (declare (optimize (speed 3) (safety 1)))
  (or (typecase x
	(gvector (typecase y
		   (real (column-to-row-vector (vector-times-scalar x (dfloat y))))
		   (gvector (vector-inner-product x y))
		   (gmatrix (vector-times-matrix x y))))
	(gmatrix (typecase y
		   (real (vector-times-scalar (array-simple-vector (transpose-matrix x)) (dfloat y)))
		   (gvector (multiply-matrices-transposed1 x y))
		   (gmatrix (multiply-matrices-transposed1 x y))))
	(row-gvector (typecase y
		       (real (vector-times-scalar (row-to-column-vector x) (dfloat y))))))
      (generic-op-error 'generic-times-transposed1 x y)))

(defun generic-times-transposed2 (x y)
  (declare (optimize (speed 3) (safety 1)))
  (or (typecase x
	(real (typecase y
		(gvector (column-to-row-vector (vector-times-scalar y (dfloat x))))
		(gmatrix (scalar-times-matrix (dfloat x)(transpose-matrix y)))
		(row-gvector (vector-times-scalar (row-to-column-vector y) (dfloat x)))))
	(gvector (typecase y
		   (gvector (matrix-multiply x (column-to-row-vector y))))) ; outer-product
	(gmatrix (typecase y
		   (row-gvector (matrix-times-vector x (row-to-column-vector y)))
		   (gmatrix (multiply-matrices-transposed2 x y))))
	(row-gvector (typecase y
		       (row-gvector (vector-inner-product 
				     (row-to-column-vector x) (row-to-column-vector y)))
		       (gmatrix (multiply-matrices-transposed2 
				 (row-to-column-vector x) y)))))
      (generic-op-error 'generic-times-transposed2 x y)))


;;; GENERIC-TIMES with typecase arg dispatching
(defun generic-times (x y)
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 1))) 
  (declare (optimize (speed 3) (safety 1)))
  (or (typecase x
	(real (typecase y
		(number (* x y))
		(gvector (vector-times-scalar y (dfloat x)))
		(array (matrix-times-scalar y (dfloat x)))
		(row-gvector (column-to-row-vector
			      (vector-times-scalar (row-to-column-vector y) (dfloat x))))))
	(gvector (typecase y
		   (real (vector-times-scalar x (dfloat y)))
					; (gvector (vector-inner-product x y)) ; illegal nx1 * nx1 ?
		   (row-gvector (matrix-multiply x y)) ; outer-product  
		   (gmatrix (vector-times-matrix x y))))
	(row-gvector (typecase y
		       (real (column-to-row-vector
			      (vector-times-scalar (row-to-column-vector x) (dfloat y))))
		       (gvector (vector-inner-product (row-to-column-vector x) y))
		       (gmatrix (vector-times-matrix (row-to-column-vector x) y))))
	(gmatrix (typecase y
		   (real (matrix-times-scalar x (dfloat y)))
		   (gvector (matrix-times-vector x y))
		   (gmatrix (matrix-multiply x y))))
	(number (* x y)))
      (generic-op-error 'generic-times x y)))


#|
(g* (cv 1.0 2.0) (transpose (cv 1.0 2.0)))
(g* (transpose (cv 1.0 2.0)) (cv 1.0 2.0))
(g* (transpose B) 
    (inverse (g* A Q (transpose A)))
    B D)
(g* (inverse (g* A Q (transpose A))) B)
(lu-solve (g* (transpose B)

(g* (transpose v) W v)

(let ((Q (make-4x4-identity-matrix))
      (s pi)
      (A (make-matrix 6 4))
      (V (make-dvector 6)))
  (fill-random A -1.0 1.0) (fill-random v -1.0 0)
  (rms-difference (g* (transpose V) A Q (transpose A) V)
		  (g* (transpose-matrix V) A Q (transpose-matrix A) V)))

(let ((Q (make-4x4-identity-matrix))
      (s pi)
      (A (make-matrix 6 4))
      (V (make-dvector 6)))
  (fill-random A -1.0 1.0) (fill-random v -1.0 0)
  (let ((m1 (g* s (transpose V) A s Q (transpose A) V s))
	(m2 (g* s (transpose-matrix V) A s Q (transpose-matrix A) V s)))
    (values (rms-difference m1 m2)
	    m1)))
|#

;;; This is also called the "Infinity P-Norm"
(defun max-abs-norm (x)
  (typecase x
    (number (abs x))
    (gvector (vector-max-abs-norm x))
    (gmatrix (vector-max-abs-norm (array-simple-vector x)))
    (otherwise (generic-op-error 'max-abs-norm x))))

;;; This is also called the "Frobenius Norm"
(defun rms-norm (x)
  (typecase x
    (number (abs x))
    (gvector (vector-rms-norm x))
    (gmatrix (vector-rms-norm (array-simple-vector x)))
    (otherwise (generic-op-error 'rms-norm x))))

;;; This is also called the "Frobenius Norm"
(defun euclidean-norm (x)
  (typecase x
    (number (abs x))
    (gvector (vector-euclidean-length x))
    (gmatrix (vector-euclidean-length (array-simple-vector x))) ; Frobenius norm, not matrix 2-norm
    (otherwise (generic-op-error 'euclidean-norm x))))


(defun max-abs-difference (x y)
  (typecase x
    (number (abs (- x y)))
    (gvector (vector-max-abs-difference x y))
    (gmatrix (matrix-max-abs-difference x y))
    (otherwise (generic-op-error 'max-abs-difference x y))))

(defun rms-difference (x y)
  (typecase x
    (number (abs (- x y)))
    (gvector (vector-rms-difference x y))
    (gmatrix (matrix-rms-difference x y))
    (otherwise (generic-op-error 'rms-difference x y))))

;;; Variant versions

#+never
(progn

;;; GENERIC-ADD with single dispatch methods

(defgeneric generic-plus1 (x y))

(defmethod generic-plus1 ((x number) y) 
    (+ x y))

;(fmakunbound 'g1+)
(defmacro g1+ (x &rest args)
  (right-associative-operator-expand 'generic-plus1 x args))

#+cmu
(defmethod generic-plus1 ((x array) y) 
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 1))) 
  (declare (optimize (speed 3) (safety 1)))
  (typecase x
    (gvector (vector-add x y))
    (gmatrix (matrix-add x y))
    (otherwise (error *unsupported-types-error-string*))))

#+allegro ; In Allegro, calling ARRAY-RANK is faster than EXCL::ARRAY-TYPEP
(defmethod generic-plus1 ((x array) y) 
  (declare (optimize (speed 3) (safety 1)))
  (case (array-rank x)
    (1 (vector-add x y))
    (2 (matrix-add x y))
    (otherwise (error *unsupported-types-error-string*))))


;(fmakunbound 'generic-times1)
(defgeneric generic-times1 (x y))

(defmacro g1* (x &rest args)
  (right-associative-operator-expand 'generic-times1 x args))

(defmethod generic-times1 ((x number) y) 
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 1)))
  (declare (optimize (speed 3) (safety 1)))
  (typecase y
    (number (* x y))
    (array (vector-times-scalar (array-simple-vector y)))
    (otherwise (error *unsupported-types-error-string*))))

#+cmu
(defmethod generic-times1 ((x array) y)
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 1))) 
  (declare (optimize (speed 3) (safety 1)))
  (typecase x
    (gvector (typecase y
	       (number (vector-times-scalar x y))
	       (gvector (vector-inner-product x y))
	       (gmatrix (vector-times-matrix x y))
	       (otherwise (error *unsupported-types-error-string*))))
	     
    (gmatrix (typecase y
	       (number (scalar-times-matrix y x))
	       (gvector (matrix-times-vector x y))
	       (gmatrix (matrix-multiply x y))
	       (otherwise (error *unsupported-types-error-string*))))
    (otherwise (error *unsupported-types-error-string*))))

;(fmakunbound 'generic-times2)
(defgeneric generic-times2 (x y))

(defmacro g2* (x &rest args)
  (right-associative-operator-expand 'generic-times2 x args))

(defmethod generic-times2 ((x number) (y number) )
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 1)))
  (declare (optimize (speed 3) (safety 1)))
  (+ x y))

(defmethod generic-times2 ((x array) (y number))
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 1)))
  (declare (optimize (speed 3) (safety 1)))
  (typecase x 
    (gvector (vector-times-scalar x y))
    (gmatrix (scalar-times-matrix y x))
    (otherwise (error *unsupported-types-error-string*))))

(defmethod generic-times2 ((x number) (y array))
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 1)))
  (declare (optimize (speed 3) (safety 1)))
  (typecase y
    (gvector (vector-times-scalar y x))
    (gmatrix (scalar-times-matrix x y))
    (otherwise (error *unsupported-types-error-string*))))

(defmethod generic-times2 ((x array) (y array))
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 1)))
  (declare (optimize (speed 3) (safety 1)))
  (typecase x 
    (gvector (typecase y
	       (gvector (vector-inner-product x y))
	       (gmatrix (vector-times-matrix x y))
	       (otherwise (error *unsupported-types-error-string*))))
    (gmatrix (typecase y
	       (gvector (matrix-times-vector x y))
	       (gmatrix (matrix-multiply x y))
	       (otherwise (error *unsupported-types-error-string*))))
    (otherwise (error *unsupported-types-error-string*))))

) ; end progn Variant versions


;;; Test code
#|
(defun repeat-fn (fn n x y)
  (declare (fixnum n))
  (loop repeat n 
	do (funcall fn x y)))

(disassemble 'vector-add)

(time (repeat-fn #'g1+ 10000000 1 2))
70ns
(time (repeat-fn #'g1+ 10000000 1.0 2.0))
160ns 16 bytes
(time (repeat-fn #'g1+ 10000000 (cv 1.0 2.0) (cv 3.0 4.0)))
184ns 24 bytes
(time (repeat-fn #'g+ 10000000 (cv 1.0 2.0) (cv 3.0 4.0)))
160ns 24 bytes


(time (repeat-fn #'g2* 10000000 1.3 (cv 3.0 4.0)))
193ns 24 bytes
(time (repeat-fn #'g2* 10000000 (cv 1.0 2.0) (cv 3.0 4.0)))
180ns 24 bytes





(g+ (cv 1.0 2.0) (cv 3.0 4.0))
(g1+ (cv 1.0 2.0) (cv 3.0 4.0))


(g* 3 4)

(g* (cv 1.0 2.0) (cv 3.0 4.0))

(defun test-10 (x y)
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 1)))
  (declare (optimize (speed 3) (safety 1)))
  (declare (type coordinate-vector x y))
  (g* x y))

(disassemble 'test-10)

(defun test-10-no-decl (x y)
  ;#+cmu (declare (ext:optimize-interface (speed 3) (safety 1)))
  ;(declare (optimize (speed 3) (safety 1)))
  ;(declare (type coordinate-vector x y))
  (g* x y))

(test-10-no-decl (make-4x4-identity-matrix) (cv 1.0 2.0 3.3 1.0))
(test-10-no-decl (make-4x4-identity-matrix) (make-4x4-identity-matrix))
|#



#| old crap

(fmakunbound 'g+)

(defgeneric g+ (x y))


(defmethod g+ ((x number) (y number)) 
  (+ x y))

#+old
(defmethod g+ ((x array) (y array)) 
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 1))) 
  (declare (optimize (speed 3) (safety 1)))
  (typecase x
    ((array * (*))
     (typecase y 
       ((array * (*)) (vector-add x y))
       (t (error *unsupported-types-error-string*))))
    ((array * (* *))
     (typecase y
       ((array * (* *))
	(matrix-add x y))
       (t (error *unsupported-types-error-string*))))
    (otherwise (error *unsupported-types-error-string*))))

(defmethod g+ ((x array) (y array)) 
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 1))) 
  (declare (optimize (speed 3) (safety 1)))
  (typecase x
    (coordinate-vector
     (typecase y 
       (coordinate-vector (vector-add x y))
       (t (error *unsupported-types-error-string*))))
    (matrix
     (typecase y
       (matrix
	(matrix-add x y))
       (t (error *unsupported-types-error-string*))))
    (otherwise (error *unsupported-types-error-string*))))

(declaim (inline g+))
(declaim (inline (method g+ (array array))))
(declaim (inline (method g+ (number number)))) 

|#

#|
(describe #'g+)
(disassemble '(pcl::fast-method g+ (array array)))
(disassemble '(pcl::fast-method g+ (number number)))

(g+ 1 2)

(g+ (cv 1.0 2.0) (cv 1.0 2.0)) 

pcl::*inline-methods-in-emfs*
pcl::*optimize-inline-slot-access-p*    
pcl::*use-slot-types-p*    
  
(pcl:seal g+ :methods)
(pcl::seal-info-seals (pcl::seal-info-or-make 'g+))
(describe (pcl::seal-info 'g+))
(pcl::info pcl::pcl pcl::seal #'g+)
(pcl::generic-function-name-p 'g+)
(pcl::seal-quality->type :methods)

(defun test20 (x y)
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 1)))
  (declare (optimize (speed 3) (safety 1)))
  (declare (type coordinate-vector x y))
  (g+ x y))

(disassemble 'test20) 


(test20 (cv 1.0 2.0) (cv 1.0 2.0)) 

(defun time-test20 (n x y)
  (loop repeat n 
	do (test20 x y)))

(time (time-test20 1000000 (cv 1.0 2.0) (cv 1.0 2.0)))
 => 3.56 us 32 bytes  with g+ unsealed
 => 3.57 us 32 bytes  with g+ sealed

(defun test20x (x y)
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 1)))
  (declare (optimize (speed 3) (safety 1)))
  (declare (type coordinate-vector x y))
  (vector-add x y))

(defun time-test20x (n x y)
  (loop repeat n 
	do (test20x x y)))

(time (time-test20x 1000000 (cv 1.0 2.0) (cv 1.0 2.0))) => 3.75 us, 32 bytes

(defparameter *m1* (make-object-to-parent-rotation-matrix :omega-phi-kappa '(10.0 20.0 30.0)))
(defparameter *v1* (cv 1.0 2.0 3.0 4.0))

(defun test (m x)
  (let ((xp (lu-solve m (g* m x))))
    (values xp (rms-difference xp x))))
  
(test *m1* *v1*)

(defun test1 (m v)
  (values (g* (transpose m) m)
	  (g* (transpose m) v)
	  (g* (transpose m) 10.0)))

(test1 *m1* *v1*)

(defun test2 (m v)
  (g* (g* m v) (g* m v)))

(g* *m1* *v1*)
(g* *v1* *v1*)

(test2 *m1* *v1*)
|#
