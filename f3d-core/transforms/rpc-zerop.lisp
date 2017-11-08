(defpackage :rpc-fit (:use :common-lisp :transforms))

(in-package :rpc-fit)

#| A C version of this code in in:

/m/rom1/quam/cme/photogrammetry/rpc/c/v20020816/poly-zero-test.c

|#

#|
THIS ISN'T BEING USED BY fitting code in $RC/SRI/rpc/rpc-fit.lisp,
  nor by the C code in ~quam/cme/photogrammetry/rpc/c/...

This only current use is to test RPC00A file inputs for zeros.
|#

#|
Test to determine if a continuous function F of n variables X has a
zero within a bounding box.  We assume that the function F is the
linear combination of basis functions B[k] of the form

    F(X) = sum(k) coeffs[k]*B[k](X)

The theory is as follows:

A.  Each basis function must be continuous and it must be possible to subdivide
the bounding box into a set of bounding boxes where the minimum and maximium
of each basis function over points in the bbox is guaranteed to be determined
from one of the corner points of the bounding box.  Specifically,

   If Xmin is the corner of the bbox that minimizes coeffs[k]*B[k]
   over the set of corners, then we must have:

       coeffs[k]*B[k](X) >= coeffs[k]*B[k](Xmin) for every X in the bbox.


   And, if Xmax is the corner of the bbox that maximizes coeffs[k]*B[k]
   over the set of corners, then we must have:

       coeffs[k]*B[k](X) <= coeffs[k]*B[k](Xmax) for every X in the bbox.

For basis functions that are the products of powers of the elements of X, the
subdivision is trivial: divide the bbox on the planes X[i] = 0. 
Since X[i]^p and X[j]^q are independent, the extrema of X[i]^p*X[j]^q over the bbox
are the vectors of extrema of the X[i]^p and X[j]^q over the bbox coordinate
intervals.  

B.   The extrema of the function F over the bbox are bounded as follows:

   For every X in the bbox,

     F(X) >= sum(k) (min (j) coeffs[k]*B[k](Xj), where {Xj} are the bbox corners.

   And, for every X in the bbox,

     F(X) <= sum(k) (max (j) coeffs[k]*B[k](Xj), where {Xj} are the bbox corners.


The algorithm for RPC demonimator polynomials is as follows:

First, since RPC demoninator polynomials contain a constant coefficient
coeffs[0] = 1.0, we know that F<0,0,0> = 1.   Hence, because of the intermediate
value theorem, in order to show the existance of a zero within the bbox it is
sufficient to find a point X within the bbox such that F(X) <=0.

The initial bounding-box (by default <-1:+1:-1:+1:-1:+1> ) is subdivided into
monotone sub-bboxes.  Each sub-bbox (called bbox below) is tested as follows:

    F(X) is evaluated at each bbox corner Xj:

    If (min (j) F(Xj)) <= 0, then there is a zero: return Xj that minimizes F.

    otherwise:

    compute: Fmin = sum(k) (min (j) coeffs[k]*B[k](Xj))

    If Fmin <=0 then subdivide the bbox into 8 smaller bboxes, and
       recursively test each for a zero.

    otherwise return false.
|#

(setq *read-default-float-format* 'double-float)

(defmacro bind-vector-elements (element-let-list vector-form &body body)
  `(let* ((%vector%  ,vector-form))
     (declare (type (or null (simple-array double-float (*))) %vector%))
     (when %vector%
       (let ,(loop for i from 0
                   for var in element-let-list
                   collect `(,var (aref %vector% ,i)))
         (declare (double-float . ,element-let-list))
         .,body))))

(defun make-coordinate-vector (n)
  (make-array n :element-type 'double-float :initial-element 0.0d0))

(defun coordinate-vector (&rest args)
  (declare (optimize (safety 1) (speed 3)))
  (let* ((n (length args))
	 (v (make-array n :element-type 'double-float)))
    (declare (fixnum n))
    (declare (type (simple-array double-float (*)) v))
    (loop for i fixnum from 0 below n
	  for val of-type double-float in args
	  do (setf (aref v i) val))
    v))

;;;(defun cv (&rest args) (apply 'coordinate-vector args))

(defun subdivide-bbox (bbox subcube-spec)
  (destructuring-bind (kx ky kz) subcube-spec
    (bind-vector-elements (x0 x1 y0 y1 z0 z1) bbox
      (let ((x.5 (* .5 (+ x0 x1)))
	    (y.5 (* .5 (+ y0 y1)))
	    (z.5 (* .5 (+ z0 z1))))
	(declare (double-float x.5 y.5 z.5))
	(coordinate-vector (if kx x0 x.5) (if kx x.5 x1)
			   (if ky y0 y.5) (if ky y.5 y1)
			   (if kz z0 z.5) (if kz z.5 z1)
			   )))))

(defun bbox-corners (bbox)
  (bind-vector-elements (x0 x1 y0 y1 z0 z1) bbox
    (list (coordinate-vector x0 y0 z0)
	  (coordinate-vector x1 y0 z0)
	  (coordinate-vector x0 y1 z0)
	  (coordinate-vector x1 y1 z0)
	  (coordinate-vector x0 y0 z1)
	  (coordinate-vector x1 y0 z1)
	  (coordinate-vector x0 y1 z1)
	  (coordinate-vector x1 y1 z1))))

(defun bbox-negative-p-at-corner (fn corners)
  (loop for pt in corners
	for val = (funcall fn pt)
	if (<= val 0.0)
	  return pt))

(defun test-for-possible-zero (minfn bbox corners)
  (<= (funcall minfn bbox corners) 0.0))
  
(defun test-for-possible-zero (minfn bbox corners)
  (let ((possible-zero (<= (funcall minfn bbox corners) 0.0)))
    #+never
    (when possible-zero
      (format t "test-for-possible-zero ~a~%"  bbox))
    possible-zero))
  
(defparameter *subcube-specs*
  '((t t t) (nil t t) (t nil t) (nil nil t)
    (t t nil) (nil t nil) (t nil nil) (nil nil nil)))

;;;(defun test-for-zero (fn minfn bbox depth)
;;;  (if (zerop depth)
;;;      bbox
;;;      (let ((corners (bbox-corners bbox)))
;;;        (or;;(and (bbox-negative-p-at-corner fn corners) bbox) ; returns bbox
;;;         (bbox-negative-p-at-corner fn corners) ; returns point with negative fn value
;;;         (and (test-for-possible-zero minfn bbox corners)
;;;              (loop for subcube-spec in *subcube-specs*
;;;                    thereis (test-for-zero fn minfn
;;;                                              (subdivide-bbox bbox subcube-spec)
;;;                                              (1- depth))))))))

;;;(defun test-for-zero (fn minfn bbox depth)
;;;  (let ((corners (bbox-corners bbox)))
;;;    (or;;(and (bbox-negative-p-at-corner fn corners) bbox) ; returns bbox
;;;     (bbox-negative-p-at-corner fn corners) ; returns point with negative fn value
;;;     (and (test-for-possible-zero minfn bbox corners)
;;;          (if (zerop depth)
;;;              bbox ; depth exhausted, return sub-bbox
;;;              (loop for subcube-spec in *subcube-specs*
;;;                    thereis (test-for-zero fn minfn
;;;                                              (subdivide-bbox bbox subcube-spec)
;;;                                              (1- depth))))))))


;;; This version returns first zero found or all bboxes where depth was exhausted.
(defun transforms::rpc-test-for-zero (fn minfn bbox depth)
  (let ((possible-zeros nil))
    (labels ((test (bbox depth)
	       (let* ((corners (bbox-corners bbox))
		      (pt (bbox-negative-p-at-corner fn corners)))
		 (if pt
		     (return-from transforms::rpc-test-for-zero pt)
		     (when (test-for-possible-zero minfn bbox corners)
		       (if (zerop depth)
			   (push bbox possible-zeros) ; depth exhausted, return sub-bbox
			   (loop for subcube-spec in *subcube-specs* 
				 do (test (subdivide-bbox bbox subcube-spec) (1- depth)))))))))
      (test bbox depth)
      possible-zeros ; when no negative value is found, return list of bboxes with possible zeros
      )))

 ;;; Specialization to polynomial basis functions.

(defun eaa-poly-xyz-3 (vector c result)
  (declare (type (simple-array double-float (20)) c)
	   (type (simple-array double-float (1)) result)
	   (type (simple-array double-float (3)) vector))
  (bind-vector-elements (x y z) vector
    (setf (aref result 0)
	  (+ (aref c 0)
	     (* x (+ (aref c 1)
		     (* x (+ (aref c 8) (* x (aref c 11)) (* y (aref c 12)) (* z (aref c 13))))
		     (* y (+ (aref c 4) (* y (aref c 14)) (* z (aref c 7))))
		     (* z (+ (aref c 5) (* z (aref c 17))))))
	     (* y (+ (aref c 2)
		     (* y (+ (aref c 9) (* y (aref c 15)) (* z (aref c 16))))
		     (* z (+ (aref c 6) (* z (aref c 18))))))
	     (* z (+ (aref c 3)
		     (* z (+ (aref c 10) (* z (aref c 19))))))
	     ))	      
    nil))


(defparameter *rpc-eval-tmp-vector* (cv 0.0 ))
(defvar *rpc-eval-verbose* nil)
;;(defvar *rpc-poly-fn* rpc-fit::*default-rpc-poly-fn*)

(defparameter *default-rpc-poly-fn* #'EAA-POLY-XYZ-3)
(defparameter *rpc-poly-fn* *default-rpc-poly-fn*)

(defun rpc-eval (coeffs pt)
  (funcall *rpc-poly-fn* pt coeffs *rpc-eval-tmp-vector*)
  (when *rpc-eval-verbose*
    (format t "rpc-eval ~a = ~a~%" pt (aref *rpc-eval-tmp-vector* 0)))
  (aref *rpc-eval-tmp-vector* 0))

(defparameter *rpc-eval-min-tmp-coeffs* (make-coordinate-vector 20))

;;; Given that the basis functions are monotone over the bbox,
;;; this computes a lower bound on the value of the polynomial over the bbox.
;;; The lower bound is:  qmin = sum(k) (min (i) coeffs[k]*basisfn[k](Xi))
;;; where k ranges over coeff indices, and i ranges over corner indices.

(defvar *rpc-eval-min-verbose* nil)

(defun rpc-eval-min (coeffs bbox &optional corners)
  (declare (type (simple-array double-float (*)) coeffs))
  (let ((n (length coeffs))
	(tmp-coeffs *rpc-eval-min-tmp-coeffs*)
	(ptlist (or corners (bbox-corners bbox)))
	)
    (declare (type (simple-array double-float (*)) tmp-coeffs))
    (unless (eql n (length tmp-coeffs))
      (setq tmp-coeffs (make-coordinate-vector n)
	    *rpc-eval-min-tmp-coeffs* tmp-coeffs))
    (loop for i fixnum from 0 below n do (setf (aref tmp-coeffs i) 0.0))
    (let ((val (loop for i fixnum from 0 below n
		     with min
		     do (setf (aref tmp-coeffs i) (aref coeffs i))
			(setq min (the double-float
				      (loop for pt in ptlist
					    minimize (let ((*rpc-eval-verbose* nil))
						       (rpc-eval tmp-coeffs pt))
					    double-float)))
		     sum min double-float
		     do (setf (aref tmp-coeffs i) 0.0)
		     when nil ;*rpc-eval-min-verbose*
		       do (format t "rpc-eval-min ~a ~a~%" i min)
		     )))
      (when *rpc-eval-min-verbose*
	(format t "rpc-eval-min ~a = ~a~%" bbox val))
      val)))


;;;(defun rpc-test-for-zero (coeffs bbox)
;;;  (test-for-zero #'(lambda(pt) (rpc-eval coeffs pt))
;;;                 #'(lambda (ptlist) (rpc-eval-min coeffs ptlist))
;;;                 bbox))

;;; Break up bbox into monotone regions.
;;; For polynomial basis functions, break bbox at zeros of coordinates.
(defun rpc-monotone-subdivide-bbox (bbox)
  (bind-vector-elements (x0 x1 y0 y1 z0 z1) bbox
    (let* ((splitx (and (< x0 0.0) (> x1 0.0)))
	   (splity (and (< y0 0.0) (> y1 0.0)))
	   (splitz (and (< z0 0.0) (> z1 0.0)))
	   (xintervals (if splitx `((,x0 0.0) (0.0 ,x1)) `((,x0 ,x1))))
	   (yintervals (if splity `((,y0 0.0) (0.0 ,y1)) `((,y0 ,y1))))
	   (zintervals (if splitz `((,z0 0.0) (0.0 ,z1)) `((,z0 ,z1))))
	   )
      (loop for (x0 x1) in xintervals
	    append (loop for (y0 y1) in yintervals
			 append (loop for (z0 z1) in zintervals
				      collect (cv x0 x1 y0 y1 z0 z1)))))))

(defparameter *rpc-subdivide-test-for-zero-max-depth* 5)

(defun rpc-subdivide-test-for-zero (coeffs bbox
			  &key
			  (depth *rpc-subdivide-test-for-zero-max-depth*)
			  (poly-fn *default-rpc-poly-fn*))
  (let ((*rpc-poly-fn* poly-fn))
    (loop for subbbox in (rpc-monotone-subdivide-bbox bbox)
	  for result = (test-for-zero
			    #'(lambda(pt) (rpc-eval coeffs pt))
			    #'(lambda (bbox ptlist) (rpc-eval-min coeffs bbox ptlist))
			    subbbox depth)
	  when (consp result)
	    append result into depth-limited-bboxes
	  else when result
		 return result
	  finally (return depth-limited-bboxes))))

(defparameter *rpc-denominator-has-zeros-depth-exceeded* nil)

(defun rpc-denominator-has-zeros
    (coeffs numerator-coeffs-count denom-thresh poly-fn)
  (ignore denom-thresh)
					;(break)
  (let* ((n (- (length coeffs) numerator-coeffs-count))
	 (denom-coeffs (make-array (1+ n) :element-type 'double-float)))
    (setf (aref denom-coeffs 0) 1.0)
    (loop for i from 0 below n
	  do (setf (aref denom-coeffs (1+ i)) (aref coeffs (+ i numerator-coeffs-count))))
    (let ((zero-pt (rpc-subdivide-test-for-zero denom-coeffs (cv -1.0 1.0  -1.0 1.0 -1.0 1.0)
				      :poly-fn poly-fn)))
      ;;(format t "rpc-denominator-has-zeros ~a ~a~%" zero-pt (listarray denom-coeffs))
      (when (and zero-pt (> (length zero-pt) 3))
	(setq *rpc-denominator-has-zeros-depth-exceeded*
	      (list denom-coeffs zero-pt poly-fn))
	;(break)
	)
      zero-pt)))

#|
(defun print-list(l)
  (loop for x in l do (format t "~a~%" x)))

(print-list
 (sort '(28.005460 21.887582 20.370932 18.529850 16.577203 15.486501 9.839771 9.196828 8.507603 7.226337 6.276284 5.829379 5.520849 5.288562 5.230322 4.149243 3.944971 3.104794 1.117152 2.875336 1.596431 1.681939 1.728124 1.972023 1.946525 2.688135 2.611054 2.592062 2.267780 2.230704 0.000009 0.000008 0.000006 0.000005 0.000005 0.000002 0.000004 0.000004 0.000004 ) #'>))

|#


(defun split-pts-file (pts-in fitpts-out ckpts-out)
  (with-open-file (st pts-in)
    (with-open-file (fit-st fitpts-out :direction :output)
      (with-open-file (ck-st ckpts-out :direction :output)
	(loop for oddp = nil then (not oddp)
	      for line = (read-line st nil nil)
	      while line
	      when oddp
		do (format fit-st "~a~%" line)
	      else do (format ck-st "~a~%" line))))))

#|
(setf (ext:default-directory) "/homedir/quam/cme/photogrammetry/rpc/c/v20020816/sri-rpc-fit")
(split-pts-file "ikonos6.pts" "ikonos6.fitpts"  "ikonos6.ckpts")
(split-pts-file "ikonos5.pts" "ikonos5.fitpts"  "ikonos5.ckpts")		

|#




#|
(lx::maybe-compile-file-load "$FREEDIUS/lisp/transforms/rpc-zerop.lisp")

(setq cme::*rational-polynomial-avoid-zeros* nil)

(setq cme::*fit-rational-polynomial-transform-to-function-solve-case* 0)
(setq cme::*fit-rational-polynomial-transform-to-function-solve-case* 5)
(setq cme::*rational-polynomial-sv-rel-threshold* 1.0E-16)
(setq cme::*rational-polynomial-sv-rel-threshold* 1.0E-12)

(setq *coeffs* cme::*)
(inspect *coeffs*)

(setq pt (rpc-subdivide-test-for-zero *coeffs* (cv -.47 1.0  -1.0 1.0  -1.0 1.0 )))

(time (rpc-subdivide-test-for-zero *coeffs* (cv -1.0 1.0  -1.0 1.0  -1.0 1.0 )))
(time (rpc-subdivide-test-for-zero *coeffs* (cv -2.0 2.0  -2.0 2.0  -2.0 2.0 )))

(let ((*rpc-eval-verbose* t))
  (rpc-subdivide-test-for-zero *coeffs* (cv -2.0 2.0  -2.0 2.0  -2.0 2.0 )))

(rpc-eval *coeffs* pt)


(rpc-subdivide-test-for-zero (coordinate-vector 1.000000 -0.032885 -0.021531 -0.160134 0.013426 -0.086283 -0.001631 -0.002458 -1.239998 -0.466120 -0.465508 -0.011294 -0.018312 0.094735 -0.004028 -0.006927 0.035837 0.002542 -0.006774 0.036373 )
		   (cv -1.0 1.0  -1.0 1.0  -1.0 1.0 ))


(rpc-subdivide-test-for-zero (coordinate-vector 1.000000 0.164925 0.058927 -0.105965 0.002721 -0.012275 -0.003827 0.000000 0.001415 0.000655 0.002226 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000  )
		   (cv -1.0 1.0  -1.0 1.0  -1.0 1.0 ))


(rpc-subdivide-test-for-zero (coordinate-vector 1.000000 -0.004802 -0.039789 -0.133987 0.044295 -0.024621 -0.039664 -0.004235 -0.491094 -1.262728 -0.452888 -0.004473 -0.006894 0.037612 -0.010828 -0.018775 0.096622 -0.002226 -0.003487 0.035245 )
		   (cv -1.0 1.0  -1.0 1.0  -1.0 1.0 ))

|#


#|

Generate a test polynomial:

  p1(x) = (x - (x0+eps)) * (x - (x0-eps)) = x*x - 2*x0*x + x0*x0-eps*eps

After normalizing by dividing by x0*x0-eps*eps, we have the desired form:

  p(x) = a*x*x + b*x + 1    

Make the tri-variate polynomial:

 p(x,y,z) = 1/3 * (p(x) + p(y) + p(z))

Which also has the desired form and is parameterized by <x0, y0, z0, eps>.

Choose x0, y0, z0 to be irrational numbers in the interval -1:+1.

|#


(defun p1-gen (x0 eps &optional (k (/ 3.0)))
  (let ((norm (/ k (- (* x0 x0) (* eps eps)))))
    (list k (* -2.0 norm x0) norm)))

(declaim (special *roots* *eps*))

(defun p3-gen (x0 y0 z0 eps)
  (setq *roots* (cv x0 y0 z0)
	*eps* eps)
  (destructuring-bind (px0 px1 px2) (p1-gen x0 eps)
    (destructuring-bind (py0 py1 py2) (p1-gen y0 eps)
      (destructuring-bind (pz0 pz1 pz2) (p1-gen z0 eps)
	(cv (+ px0 py0 pz0) px1 py1 pz1 0.0 0.0 0.0 0.0
	    px2 py2 pz2 0.0 0.0 0.0 0.0 0.0              ; RPC00A format
	    0.0 0.0 0.0 0.0)))))

(defun df/dx (c x) (+ (aref c 1) (* 2.0 x (aref c 8))))
(defun df/dy (c y) (+ (aref c 2) (* 2.0 y (aref c 9))))
(defun df/dz (c z) (+ (aref c 3) (* 2.0 z (aref c 10))))


#|

(setq *coeffs* (p3-gen (sqrt .5) (sqrt .5) (sqrt .5) 1e-5))

(setq *coeffs* (p3-gen (* .25 (sqrt .5)) (* .25 (sqrt .5)) (* .25 (sqrt .5)) .001))

(setq *coeffs* (p3-gen (* (/ 1.0 16) (sqrt .5)) (* (/ 1.0 16) (sqrt .5)) (* (/ 1.0 16) (sqrt .5)) .001))

(setq *coeffs* (p3-gen (sqrt .5) (* .25 (sqrt .5)) (* .75 (sqrt .5)) .01))

(bind-vector-elements (x y z) *roots*
  (values (df/dx *coeffs* (+ x *eps*))
	  (df/dy *coeffs* (+ y *eps*))
	  (df/dz *coeffs* (+ z *eps*))))

(progn *coeffs*)

(setq soln (rpc-subdivide-test-for-zero *coeffs* (cv -1.0 1.0  -1.0 1.0  -1.0 1.0 )))

(length (setq soln (rpc-subdivide-test-for-zero *coeffs* (cv -1.0 1.0  -1.0 1.0  -1.0 1.0 ) :depth 8)))
(length (setq soln (rpc-subdivide-test-for-zero *coeffs* (cv -1.0 1.0  -1.0 1.0  -1.0 1.0 ) :depth 10)))
(list soln
      *roots*
      (loop for i from 0 below 3 collect (- (aref soln i) (aref *roots* i)))
      (rpc-eval *coeffs* soln))

(let ((*rpc-eval-min-verbose* t))
  (setq soln (rpc-subdivide-test-for-zero *coeffs* (cv -1.0 1.0  -1.0 1.0  -1.0 1.0 ) :depth 2)))

(setq soln (rpc-subdivide-test-for-zero *coeffs* (cv -1.0 1.0  -1.0 1.0  -1.0 1.0 ) :depth 2))

(rpc-eval-min *coeffs* (nth 1 soln))

(loop for pt in (bbox-corners (nth 1 soln))
      collect (rpc-eval *coeffs* pt))


(rpc-eval *coeffs* (cv 0.0 0.0 0.0))

(lx::maybe-compile-file-load "$FREEDIUS/lisp/transforms/rpc-zerop.lisp")
(lcl::load-foreign-library "/m/rom1/quam/cme/photogrammetry/rpc/c/v20020816/poly-zero-test.so")

(lcl::def-foreign-function (rpc_test_for_zero (:name "lisp_rpc_test_for_zero") (:return-type :int))
  (coeffs :simple-array) ; double 20
  (result :simple-array) ; double 6
  (bbox_range :double); 1.2
  (max_subdivision_depth :int) ; 5
  (denom_thresh  :double) ; 0.0
  )

(let* ((result (cv 0.0 0.0 0.0 0.0 0.0 0.0))
       (depth-limited-count (rpc_test_for_zero *coeffs* result 1.0 8 0.0)))
  (if (zerop depth-limited-count)
      (bind-vector-elements (x xmax y ymax z zmax) result
	(cv x y z))
      (values result depth-limited-count)))



|#
