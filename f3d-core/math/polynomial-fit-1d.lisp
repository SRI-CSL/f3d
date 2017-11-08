(in-package :math)

;;; Simple 1d polynomial fitting

;;; Better version should use orthogonal polynomials


(defun 1d-fit-basis-fns (xarr yarr bases)
  (declare (vector bases))
  (declare (type dvector xarr yarr))
  ;(break)
  (let* ((npts (length xarr))
	 (mbases (length bases))
	 (J (make-matrix npts mbases))
	 (JtJ (make-matrix mbases mbases))
	 (Jtb (make-dvector mbases)))
    (declare (fixnum npts mbases)
	     (type dmatrix J))
    (loop for i fixnum from 0 below npts
	  for xi double-float = (aref xarr i)
	  do (loop for k fixnum from 0 below mbases
		   do (setf (aref J i k) (funcall (aref bases k) xi))))

    (let* ((JtJ (g* (transpose J) J))
	   (Jtb (g* (transpose J) yarr))
	   (S (cholesky-decompose JtJ))
	   (coeffs (cholesky-solve S Jtb)))
      coeffs)))

(defparameter *parabola-basis-fns*
  (vector #'(lambda(x) 1.0)
	  #'(lambda(x) x)
	  #'(lambda(x) (* x x))))
    

(defun 2d-fit-basis-fns (xarr yarr zarr bases)
  (declare (vector bases))
  (declare (type dvector xarr yarr zarr))
					;(break)
  (let* ((npts (length xarr))
	 (mbases (length bases))
	 (J (make-matrix npts mbases))
	 (JtJ (make-matrix mbases mbases))
	 (Jtb (make-dvector mbases)))
    (declare (fixnum npts mbases)
	     (type dmatrix J))
    (loop for i fixnum from 0 below npts
	  for xi double-float = (aref xarr i)
	  for yi double-float = (aref yarr i)
	  do (loop for k fixnum from 0 below mbases
		   do (setf (aref J i k) (funcall (aref bases k) xi yi))))

    (let* ((JtJ (g* (transpose J) J))
	   (Jtb (g* (transpose J) zarr))
	   (S (cholesky-decompose JtJ))
	   (coeffs (cholesky-solve S Jtb)))
      coeffs))) 

(defparameter *paraboloid-basis-fns*
  (vector #'(lambda(x y) 1.0)
	  #'(lambda(x y) x)
	  #'(lambda(x y) y)
	  #'(lambda(x y) (* x x))
	  #'(lambda(x y) (* x y))
	  #'(lambda(x y) (* y y))))
	  
(defun estimate-paraboloid-peak (coeffs)
  (bind-vector-elements (c1 cx cy cxx cxy cyy) coeffs
    (let* ((x (/ (- (* 2.0 cyy cx) (* cy cxy))
		 (- (* cxy cxy) (* 4.0 cxx cyy))))
	   (y (- (/ (+ (* 2.0 cxx x) cx) cxy)))
	   (z (+ c1 (* cx x) (* cy y) (* cxx x x) (* cxy x y) (* cyy y y))))
      (values x y z (list cxx cxy cyy)))))