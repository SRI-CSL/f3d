(IN-PACKAGE :TRANSFORMS)

;;; FIXME:  Need to eliminate the need for calling the compiler at runtime.

(defparameter *rat-poly-project-vector-tmp-vector-3* (make-coordinate-vector 10)) 
(defparameter *rat-poly-project-vector-tmp-result-vector* (make-coordinate-vector 10))

;;; This is PROJECT-VECTOR for rational polynomials.  (x y z) => (u v zp)

(defparameter *rat-poly-denoms* (make-coordinate-vector 2))

(defvar *rat-poly-save-denoms* nil)

(defmacro vector-length (vector)
  `(length ,vector))

(defmacro poly-factor (coeffs &rest factors)
  `(+ ,(loop for factor in factors
	     for i from 0
	     if factor
	       collect `(* (aref ,coeffs ,i) . ,factor)
	     else collect `(aref ,coeffs ,i))))

(eval-when (load eval compile)
(defun poly-exprs-internal (vars exprs)
  (let ((coeff-vector-names
	 (loop for i from 0 below (length exprs)
	       collect (intern (format nil "COEFF~d" i)))))
				    
    `(let ,(loop for var in vars
		 for i from 0
		 collect `(,var (aref from-vector ,i)))
       (unless (>= (vector-length to-vector) ,(length exprs))
	 (error "To-vector ~a must have length >= ~a" to-vector ,(length exprs)))

       (when mean-vector
	 . ,(loop for var in vars
		  for i from 0
		  collect `(setq ,var (- ,var (aref mean-vector ,i)))))
       (let ,(loop for name in coeff-vector-names
		   for i from 0
		   collect `(,name (aref coeffs-vector-vector ,i)))
	 (declare (type (simple-array double-float (*)) . ,coeff-vector-names))
	 . ,(loop for expr in exprs
		  for coeffs in coeff-vector-names
		  for j from 0
		  collect `(setf (aref to-vector ,j)
				 (+ .,(loop for factor in expr
					   for i from 0
					   if factor
					     collect `(* (aref ,coeffs ,i) . ,factor)
					   else collect `(aref ,coeffs ,i)))))))))
)

(defmacro poly-exprs (vars &rest exprs)
  (poly-exprs-internal vars exprs))


(defmacro build-simple-polynomial-transform (vars &rest exprs)
  `(make-polynomial-coordinate-transform
    (generate-polynomial-transform-function
     (poly-exprs ,vars . ,exprs))
    ',(loop for expr in exprs collect (length expr))
    nil nil))

(defmacro def-simple-polynomial-transform (name vars &rest exprs)
  (let ((fn (generate-polynomial-transform-function-internal
	     (list (poly-exprs-internal vars exprs))
	     )))
    `(progn
      ;;(setf (symbol-function ,name) ,fn)
      (defparameter ,name
	(make-polynomial-coordinate-transform ',fn
					      ',(loop for expr in exprs collect (length expr))
					      nil nil
					      )))))


#+allegro
(defmacro def-polynomial-transform (name form  n-coeffs-seq coeffs-vector-vector mean-vector
					 &rest options)
  (let ((fn (if (symbolp form)
		form
		(generate-polynomial-transform-function-internal
		 (list form)))))
    `(progn
      ;;(setf (symbol-function ,name) ,fn)
      (defparameter ,name
	(make-polynomial-coordinate-transform ',fn
					      ',n-coeffs-seq
					      ',coeffs-vector-vector ',mean-vector
					      .,options)))))

;;;
;;; Awful hack to coerce CMUCL to compile the transform definitions in polynomial-transforms.lisp
;;;
(defun generate-polynomial-transform-function-internal-2 (body)
  `(compile nil
	    '(lambda (transform from-vector to-vector)
	     (declare (type (simple-array double-float (*)) from-vector to-vector))
	     (with-class-slot-values
		 polynomial-coordinate-transform (coeffs-vector-vector mean-vector) transform
	       (declare (simple-vector coeffs-vector-vector))
	       (declare (type (simple-array double-float (*)) mean-vector))
	       . ,body)
	     to-vector)))

#+(or cmu sbcl)
(defmacro def-polynomial-transform (name form  n-coeffs-seq coeffs-vector-vector mean-vector
					 &rest options)
  (let ((fn (if (symbolp form)
		form
		(generate-polynomial-transform-function-internal-2 (list form)))))
    `(st::add-system-initialization :rpc
      ;;(setf (symbol-function ,name) ,fn)
      (setf ,name
	(make-polynomial-coordinate-transform ',fn
					      ',n-coeffs-seq
					      ',coeffs-vector-vector ',mean-vector
					      .,options)))))

    
(defmacro build-simple-polynomial-projection (vars &rest exprs)
  
  `(make-polynomial-coordinate-projection
    (generate-polynomial-transform-function
     (poly-exprs ,vars . ,exprs))
    ',(loop for expr in exprs collect (length expr))
    nil nil))



#|
(pprint (poly-exprs-internal '(x y z)
		     '(((x) (y) (z) ())
		       ((x) (y) (z) ())
		       ((x) (y) (z) ()))))
(generate-polynomial-transform-function-internal
 (list (poly-exprs-internal '(x y z)
			    '(((x) (y) (z) ())
			      ((x) (y) (z) ())
			      ((x) (y) (z) ())))))
(inspect 'pt-linear-3d-to-3d)
(disassemble 'pt-linear-3d-to-3d)

(def-simple-polynomial-transform pt-linear-3d-to-3d
    (x y z)
  ((x) (y) (z) ())
  ((x) (y) (z) ())
  ((x) (y) (z) ()))
|#

#|
    
(defparameter pt-linear-3d-to-3d
	      (build-simple-polynomial-transform
	       (x y z)
	       ((x) (y) (z) ())
	       ((x) (y) (z) ())
	       ((x) (y) (z) ())))

(defparameter pt-quadratic-3d-to-3d
	      (build-simple-polynomial-transform
	       (x y z)
	       ((x x) (y y) (z z) (x y) (x z) (y z) (x) (y) (z) ())
	       ((x x) (y y) (z z) (x y) (x z) (y z) (x) (y) (z) ())
	       ((x x) (y y) (z z) (x y) (x z) (y z) (x) (y) (z) ())))

(defparameter pt-linear-3d-to-2d
	      (build-simple-polynomial-transform
	       (x y z)
	       ((x) (y) (z) ())
	       ((x) (y) (z) ())))

(defparameter pt-linear-2d-to-2d
	      (build-simple-polynomial-transform
	       (x y)
	       ((x) (y) ())
	       ((x) (y) ())))
|#


(eval-when (load eval compile)
(defun generate-polynomial-transform-function-internal (body)
  (compile nil
	   `(lambda (transform from-vector to-vector)
	     (declare (type (simple-array double-float (*)) from-vector to-vector))
	     (with-class-slot-values
		 polynomial-coordinate-transform (coeffs-vector-vector mean-vector) transform
	       (declare (simple-vector coeffs-vector-vector))
	       (declare (type (simple-array double-float (*)) mean-vector))
	       . ,body)
	     to-vector)))

) ; end eval-when
 
(defmacro generate-polynomial-transform-function ( &body body)
  `(generate-polynomial-transform-function-internal ',body))


(eval-when (eval load compile)

;;; no callers
(defun generate-zp-projection-plane-code ()
  `(let ((to-vector-length (vector-length to-vector)))
    (when (>= to-vector-length 3)
      (with-class-slot-values rat-poly-projection
	    (zp-projection-plane) projection
	(declare (type (simple-array double-float (*)) zp-projection-plane))
	(when (>= to-vector-length 4)
	  (setf (aref to-vector 3) 1.0))
	(setf (aref to-vector 2)
	      (if zp-projection-plane
		  (+ (* x (aref zp-projection-plane 0))
		     (* y (aref zp-projection-plane 1))
		     (* z (aref zp-projection-plane 2))
		     (aref zp-projection-plane 3))
		  (aref from-vector 2)))))))
)

(defmacro define-rat-poly-project-vector (fn-name poly-functions &optional common-denominator)
  (let ((to-vector-length (if common-denominator
			      (1- (length poly-functions))
			      (ash (length poly-functions) -1))))
    `(defun ,fn-name (projection from-vector to-vector)
      (declare (type (simple-array double-float (*))  from-vector to-vector))
      (when from-vector
	(with-class-slot-values rat-poly-projection
	      (coeffs-vector-vector mean-vector scale-vector output-scale-vector output-offset-vector)
	    projection
	  (declare (type (simple-array double-float (3))
			 mean-vector scale-vector output-scale-vector output-offset-vector)
		   (simple-vector coeffs-vector-vector))
	  (let* ((normalized-from-vector *rat-poly-project-vector-tmp-vector-3*)
		 (tmp-vector *rat-poly-project-vector-tmp-result-vector*))
	    (declare (type (simple-array double-float (*)) normalized-from-vector tmp-vector))
	    (bind-vector-elements (x y z) from-vector
	      (if mean-vector
		  (setf (aref normalized-from-vector 0) (* (- x (aref mean-vector 0)) (aref scale-vector 0))
			(aref normalized-from-vector 1) (* (- y (aref mean-vector 1)) (aref scale-vector 1))
			(aref normalized-from-vector 2) (* (- z (aref mean-vector 2)) (aref scale-vector 2)))
		  (setf (aref normalized-from-vector 0) x
			(aref normalized-from-vector 1) y
			(aref normalized-from-vector 2) z))
		
	      
	      (unless to-vector (setq to-vector (make-coordinate-vector 3)))
	    
	      (macrolet ((eval-poly (i)
			   `(progn (,(nth i ',poly-functions) normalized-from-vector
				    (aref coeffs-vector-vector ,i) tmp-vector)
			     (aref tmp-vector 0))))
	       
		,(if common-denominator
		     `(let ((denom (eval-poly ,(1- (length poly-functions )))))
		       (declare (double-float denom))
		       (setf . ,(loop for to-index from 0 below (1- (length poly-functions))
				      collect `(aref to-vector ,to-index)
				      collect `(/ (eval-poly ,to-index) denom))))
		     `(setf . ,(loop for cv-index from 0 by 2 below (length poly-functions)
				     for to-index from 0
				     collect `(aref to-vector ,to-index)
				     collect `(/ (eval-poly ,cv-index)
					       (eval-poly ,(1+ cv-index))))))
		(when output-scale-vector
		  (setf . ,(loop for i from 0 below to-vector-length		  
				 collect `(aref to-vector ,i)
				 collect `(+ (* (aref to-vector ,i)
					      (aref output-scale-vector ,i))
					   (aref output-offset-vector ,i))))
		  )
		,(generate-zp-projection-plane-code)
		to-vector))))))))



;;; This is TRANSFORM-VECTOR for rational polynomials (x y z) => (xp yp z)
(defmacro define-rat-poly-transform-3-vector (fn-name poly-functions)
  (flet ((eval-poly (fn coeff-vector)
	   `(progn (,fn  normalized-from-vector ,coeff-vector tmp-vector)
	     (aref tmp-vector 0))))
    `(defun ,fn-name (transform from-vector to-vector)
      (declare (type (simple-array double-float (*))  from-vector to-vector))
      (when from-vector
	(with-class-slot-values rational-polynomial-coordinate-transform
	      (coeffs-vector-vector mean-vector scale-vector output-scale-vector output-offset-vector)
	    transform
	  (declare (type (simple-array double-float (3))
			 mean-vector scale-vector output-scale-vector output-offset-vector)
		   (simple-vector coeffs-vector-vector))
	  (let* ((normalized-from-vector *rat-poly-project-vector-tmp-vector-3*)
		 (tmp-vector *rat-poly-project-vector-tmp-result-vector*)
		 (denom 1.0) (ratio 1.0)
		 (save-denoms *rat-poly-save-denoms*))
	    (declare (type (simple-array double-float (*)) normalized-from-vector tmp-vector))
	    (declare (double-float denom ratio))
	    (bind-vector-elements (x y z) from-vector
	      (if mean-vector
		  (setf (aref normalized-from-vector 0) (* (- x (aref mean-vector 0)) (aref scale-vector 0))
			(aref normalized-from-vector 1) (* (- y (aref mean-vector 1)) (aref scale-vector 1))
			(aref normalized-from-vector 2) (* (- z (aref mean-vector 2)) (aref scale-vector 2)))
		  (setf (aref normalized-from-vector 0) x
			(aref normalized-from-vector 1) y
			(aref normalized-from-vector 2) z))
	  
	      (unless to-vector (setq to-vector (make-coordinate-vector 2)))
	      ,@(loop for out-var from 0
		      for coeffs-index from 0 by 2
		      for (num-fn denom-fn) on poly-functions by #'cddr
		      collect `(setf denom
				,(eval-poly denom-fn `(aref coeffs-vector-vector ,(1+ coeffs-index))))
		      collect `(setf ratio (/ ,(eval-poly num-fn `(aref coeffs-vector-vector ,coeffs-index))
					    denom))
		      collect `(when output-scale-vector
				(setq ratio (+ (* ratio (aref output-scale-vector ,out-var))
					     (aref output-offset-vector ,out-var))))
		      collect `(setf (aref to-vector ,out-var) ratio)
		      collect `(when save-denoms
				(setf (aref (the (simple-array double-float (*)) *rat-poly-denoms*)
				       ,out-var)
				 denom)))
	      
	      (let ((to-vector-length (vector-length to-vector)))
		(when (> to-vector-length ,(ash (length poly-functions) -1))
		  (setf . ,(loop for out-var from (ash (length poly-functions) -1) below 3
				 collect `(aref to-vector ,out-var)
				 collect `(aref from-vector ,out-var)))))

	      to-vector)))))))

#+rpc-fit
(defmacro rpc-fit::neq (a b) `(not (eq ,a ,b)))
