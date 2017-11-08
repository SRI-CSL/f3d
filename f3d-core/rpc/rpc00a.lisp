(in-package :transforms)

#|

NITF RPC00A subheader RATIONAL POLYNOMIAL IMPLEMENTATION is implemented by
converting the coefficient order to that of DPPDB-rpc
and changing the scales and offsets appropriately.

|#

(defparameter *rpc-file-header* "RCDE RATIONAL POLYNOMIAL PROJECTION")

(defparameter  *dppdb-coeffs-order*
  '(1 x y z xy xz yz xx yy zz xyz xxx xyy xzz xxy yyy yzz xxz yyz zzz))

(defparameter  *sri-coeffs-order*
  '(1 x y z xx xy xz yy yz zz xxx xxy xxz xyy xyz xzz yyy yyz yzz zzz))

(defparameter  *rpc00aa-coeffs-order*
  '(1 x y z xy xz yz xyz xx yy zz xxx xxy xxz xyy yyy yyz xzz yzz zzz))

;;; o1 and o2 appear to be beckwards
(defun compute-permutation (o1 o2)
  (loop for c1 in o1
	for c2 = (position c1 o2)
	collect c2))

(defparameter *dppdb-to-sri-coeffs-permutation*
  '(0 1 2 3 7 4 5 8 6 9 11 14 17 12 10 13 15 18 16 19))

(defparameter *sri-to-dppdb-coeffs-permutation*
  '(0 1 2 3 5 6 8 4 7 9 14 10 13 15 11 16 18 12 17 19))

(defparameter *rpc00a-to-sri-coeffs-permutation* 
  '(0 1 2 3 8 4 5 9 6 10 11 12 13 14 7 17 15 16 18 19))

(defparameter *sri-to-rpc00a-coeffs-permutation*
  '(0 1 2 3 5 6 8 14 4 7 9 10 11 12 13 16 17 15 18 19))

(defparameter *rpc00aa-to-dppdb-coeffs-permutation*
  '(0 1 2 3 4 5 6 8 9 10 7 11 14 17 12 15 18 13 16 19))

(defparameter *dppdb-to-rpc00aa-coeffs-permutation*
  '(0 1 2 3 4 5 6 10 7 8 9 11 14 17 12 15 18 13 16 19))

#|
RPC00A coefficient order

 0 x(1) xx(8) xxx(11) xxy(12) xxz(13)
        xy(4)         xyy(14) xyz(7)
        xz(5)                 xzz(17)
   y(2) yy(9) yyy(15) yyz(16)
        yz(6)         yzz(18)
   z(3) zz(10) zzz(19)

   RPC00AA SRI  dppdb
1    0     0     0
x    1     1     1
y    2     2     2
z    3     3     3
xy   4     5     4
xz   5     6     5
yz   6     8     6
xyz  7    14    10
xx   8     4     7 
yy   9     7     8 
zz  10     9     9 
xxx 11    10    11
xxy 12    11    14
xxz 13    12    17
xyy 14    13    12
yyy 15    16    15
yyz 16    17    18
xzz 17    15    13
yzz 18    18    16
zzz 19    19    19
|#

(defstruct-class rpc00a-rpc (dppdb-rpc-projection) ())


(defmethod initialize-instance :after ((projection rpc00a-rpc) &rest args)
  (permute-rpc-coeffs projection *rpc00aa-to-dppdb-coeffs-permutation*)
  (with-class-slots rpc00a-rpc (scales) projection
    (setf (aref scales 3) (/ (aref scales 3))
	  (aref scales 4) (/ (aref scales 4)))))


(defun make-rat-poly-projection2 (means scales coeffs
					inv-means inv-scales inv-coeffs
					&rest args
					&key (zp-projection-plane (cv 0.0 0.0 1.0 0.0)))
  (ignore args)
  (labels ((split-norm (vect)
	     (values (cv (aref vect 2) (aref vect 3) (aref vect 4)) ; input norm
		     (cv (aref vect 0) (aref vect 1) 0.0) ; output norm
		     ))
	   (make-rat-trans (class transform-function means scales coeffs &rest args)
	     (setq coeffs (permute-coeffs-coeffs-vector coeffs
					      *rpc00a-to-sri-coeffs-permutation*
					      ;;*200ea-to-sri-coeffs-permutation*
					      ))
	     (multiple-value-bind (in-means out-means) (split-norm means)
	       (multiple-value-bind (in-scales out-scales) (split-norm scales)
		 (bind-vector-elements (sx sy sz) in-scales
		   (apply 'make-instance class
			  :mean-vector in-means :scale-vector (cv (/ sx) (/ sy) (/ sz))
			  :output-offset-vector out-means :output-scale-vector out-scales
			  :transform-function transform-function
			  :COEFFS-VECTOR-VECTOR coeffs
			  args)))))
	   )
    (let ((proj (make-rat-trans 'transforms::rat-poly-projection
				'transforms::RAT-POLY-3-NORMALIZED-PROJECT-VECTOR
				means scales coeffs
				:zp-projection-plane zp-projection-plane))
	  inv-proj)
      (when inv-coeffs
	(setq inv-proj (make-rat-trans 'transforms::RATIONAL-POLYNOMIAL-COORDINATE-TRANSFORM
				       'transforms::RAT-POLY-3-NORMALIZED-TRANSFORM-VECTOR
				       inv-means inv-scales inv-coeffs))
	
	(setf (inverse-transform proj) inv-proj
	      (inverse-transform inv-proj) proj))
      
      proj)))




;;; ************   Based on $RADIUSCODE/SRI/rpc/RPC00A-rpc.lisp ************ 


#| **********************   RPC00A RATIONAL POLYNOMIAL IMPLEMENTATION  **********************

To load the RPC00A rational polynomial implementation, evaluate:


To create a rational polynomial projection (RAT-POLY-PROJECTION) from a file containing
a RPC00A coefficient string which maps from an LVCS coordinate system (local site coordinates)
to 2d-world (full frame image) coordinates, do one of the following


1.  The simplist way to do the full job of building the RAT-POLY-PROJECTION and saving it to a file is
    as follows:

 1a. For a 2d-world tied into the radius-site-glue system
       (MAKE-AND-SAVE-RPC-PROJECTION <RPC00A-rpf-string>
				     lvcs-to-lat-long-transform
				     uvh-bbox
				     :2d-world 2d-world)

 1b. Otherwise:
       (MAKE-AND-SAVE-RPC-PROJECTION <RPC00A-rpf-string>
				     lvcs-to-lat-long-transform
				     uvh-bbox
				     :camera-model-pathname <output-path>)



 2. To build (but not immediately save) the RAT-POLY-PROJECTION:

  2a. (setq proj
	    (MAKE-RPC-PROJECTION-FROM-RPC00A-STRING (read-file-to-string <rpc00a-rcf-pathname>)
						   lvcs-to-lat-long-transform uvh-bbox))

  or

  2b. (setq proj
	    (MAKE-RPC-PROJECTION-FROM-RPC00A-STRING <RPC00A-rpf-string>
						   lvcs-to-lat-long-transform uvh-bbox))
  

For a given RADIUS site, you can get the lvcs-to-lat-long-transform as follows:

    (lvcs-to-lat-long-transform (coordinate-system  3d-world))

    (lvcs-to-lat-long-transform (coordinate-system (get-3d-world-named <site-name>)))

  Example:

    (lvcs-to-lat-long-transform (coordinate-system (get-3d-world-named "ft-hood-2")))

The uvh-bbox argument is important for specifing the legitimate range of 2d-world coordinates for
the rational polynomial projection. For a given image, you can get its uvh-bbox (ie, its bounding
box in 2d-world coordinates) by:

    (uvh-bbox image)

The format of the uvh-bbox coordinate-vector is:

    (umin umax vmin vmax hmin hmax)

 where u and v are image coordinates (line and sample respectively),
 and h is the height (elevation) in meters with respect to the WGS-84 ellipsoid.


If you have absolutely no way of determining the uvh-bbox, the uvh-bbox argument can be NIL, but
THE QUALITY OF THE RESULTING RATIONAL POLYNOMIAL FIT IS NOT GUARANTEED.  THERE IS NO RELIABLE
WAY TO EXTRACT THE UVH-BBOX FROM THE RPC00A RPC INFORMATION.  If the coordinate normalization
specified in the RPC00A rpf information covers a range larger than the actual coordinate
range covered by the image, then the resulting fit can be of poor quality.  


The camera model for a given 2d-world can be set to the resulting projection as follows:

    (change-projection 2d-world proj)

The 3d-to-2d-projection of a given 2d-world can be saved to the file system for later use as follows:

    (SAVE-3D-TO-2D-PROJECTION 2d-world (site-glue 2d-world) <camera-model-pathname>)

   The <camera-model-pathname> may be omitted, and the site-glue system will create an
   appropriate pathname using the site-slue conventions for saving the camera model. For example:

    (SAVE-3D-TO-2D-PROJECTION 2d-world (site-glue 2d-world))

|#

;;; Permutation to go from RPC00A coefficient order to SRI coefficient order.
;;; Must define this before loading $RADIUSCODE/SRI/rpc/rpc-fit.lisp
(defparameter *rpc00a-to-sri-coeffs-permutation* 
  '(0 1 2 3 8 4 5 9 6 10 11 12 13 14 7 17 15 16 18 19))

(defparameter *sri-to-rpc00a-coeffs-permutation*
  (loop with l = (make-list (length *rpc00a-to-sri-coeffs-permutation*))
	for i in *rpc00a-to-sri-coeffs-permutation*
	for j from 0
	do (setf (nth i l) j)
	finally (return l)))


;;; Load the improved rpc fitter if it is provided.
;;; The fallback is the older rpc-fitter in $CMEHOME/math/rational-polynomial-transforms.lisp
;;;     that sometimes will generate denominators with zero crossings.
#+old
(when (or (probe-file (cl-user::object-pathname "$RADIUSCODE/SRI/rpc/rpc-fit.lisp"))
	  (probe-file "$RADIUSCODE/SRI/rpc/rpc-fit.lisp"))
  (defpackage rpc-fit (:use lisp lcl ic clos cme )
	    (:import-from math COPY-ARRAY-CONTENTS)
	    (:import-from ic ^2)
	    (:import-from cme
			  BIND-VECTOR-ELEMENTS
			  COORDINATE-VECTOR
			  CV
			  *RATIONAL-POLYNOMIAL-SV-REL-THRESHOLD*)
	    )
  (cl-user::maybe-compile-file-load "$RADIUSCODE/SRI/rpc/rpc-fit.lisp"))


;;;  **********************  READER FOR RPC00A COMPRESSED COFFICIENTS FORMAT  *************************

;;; The file $RADIUSCODE/SRI/rpc/image1.rpc00a-rpc contains an example of the RPC00A rpf coefficients.

(defun read-rpc00a-poly-coeff (field)
  (let* ((len (length field))
	 (2nd-sign-pos (- len 2)))
    (read-from-string (concatenate 'string
				   (subseq field 0 1) ; 1st sign
				   "."
				   (subseq field 1 2nd-sign-pos)
				   "E"
				   (subseq field 2nd-sign-pos len)))))

#|
(read-rpc00a-poly-coeff "-006499-7")
(read-rpc00a-poly-coeff "     0")
|#

(defun parse-rpc00a-rpf-string (string)
  (let  ((error-scale .1)
	 (scale-and-offset-sizes '(6 5 7 8 5))
	 (scale-and-offset-scales '(1.0 1.0 1e-4 1e-4 1.0))
	 (pos 0))
    (labels ((ascii-fld (size) (subseq string pos (setq pos (+ pos size))) )
	     (int-fld (size) (read-from-string (ascii-fld size)))
	     (scales-or-offsets ()
	       (apply 'cv (loop for size in scale-and-offset-sizes
				for scale in scale-and-offset-scales
				collect (* scale (int-fld size)))))
	     (poly-coeffs-seq (n) (loop repeat n collect (read-rpc00a-poly-coeff (ascii-fld 9))))
	     (poly-coeffs ()
	       (apply 'cv (append (prog1 (poly-coeffs-seq 7) (ascii-fld 17))
				  (prog1 (poly-coeffs-seq 7) (ascii-fld 17))
				  (prog1 (poly-coeffs-seq 6) (ascii-fld 26)))))
	     )
      (let* ((image-name (ascii-fld 24))
	     (code (ascii-fld 1))
	     (correlated-error (* error-scale (int-fld 5)))
	     (uncorrelated-error (* error-scale (int-fld 5)))
	     (pad1 (ascii-fld 45))
	     (offsets (scales-or-offsets))
	     (scales (scales-or-offsets))
	     (pad2 (ascii-fld 18))
	     (u-numerator-coeffs (poly-coeffs))
	     (u-denominator-coeffs (poly-coeffs))
	     (v-numerator-coeffs (poly-coeffs))
	     (v-denominator-coeffs (poly-coeffs)))
	(ignore pad1 pad2)
	(values image-name
		code
		(list correlated-error uncorrelated-error)
		offsets
		scales
		u-numerator-coeffs
		u-denominator-coeffs
		v-numerator-coeffs
		v-denominator-coeffs)))))

(defun read-file-to-string (pathname)
  (with-open-file (st pathname)
    (loop with string = ""
	  for line = (read-line st nil nil)
	  while line
	  do (setq string (format nil "~a~a~%" string line))
	  finally (return string))))

(defun parse-rpc00a-rpf-file (pathname)
  (parse-rpc00a-rpf-string (read-file-to-string pathname)))

#|
(setq rpf-str (read-file-to-string "$RC/SRI/rpc/image1.rpc00a-rpc"))
(parse-rpc00a-rpf-string rpf-str)
|#

(defun permute-coeffs-coeffs-vector (coeffs-vector-vector permutation)
  (if permutation
      (apply 'vector
	     (loop for j from 0 below 4
		   for coeffs = (aref coeffs-vector-vector j)
		   for perm-coeffs = (make-array (length coeffs) :element-type 'double-float)
		   do (loop for p in permutation
			    for i from 0
			    do (setf (aref perm-coeffs i) (aref coeffs p)))
		   collect perm-coeffs))
      coeffs-vector-vector))

;;; THIS IS THE PRIMARY FUNCTION FOR GENERATING RATIONAL POLYNOMIAL PROJECTIONS FROM RPC00A COEFFICIENTS
(defun make-rpc-projection-from-rpc00a-string (rpf-string &optional
							 lvcs-to-wgs-84-lat-long-coordinate-transform
							 uvh-bbox
							 (permutation *rpc00a-to-sri-coeffs-permutation*))
  (multiple-value-bind (image-name code errors offsets scales
				   u-numerator-coeffs u-denominator-coeffs
				   v-numerator-coeffs v-denominator-coeffs)
      (parse-rpc00a-rpf-string rpf-string)
    (ignore code)
    (bind-vector-elements (u0 v0 y0 x0 z0) offsets ; note flip of y0 x0  - wierdness in spec
      (bind-vector-elements (us vs ys xs zs) scales ; note flip of ys xs
	(let* ((coeffs-vector-vector (vector u-numerator-coeffs u-denominator-coeffs
					     v-numerator-coeffs v-denominator-coeffs))
	       (projection 
		(make-instance 'rat-poly-projection
			       :transform-function 'rat-poly-3-normalized-project-vector
			       :coeffs-vector-vector
			       (permute-coeffs-coeffs-vector coeffs-vector-vector permutation)
			       :mean-vector (cv x0 y0 z0)
			       :scale-vector (cv (/ xs) (/ ys) (/ zs))
			       :output-scale-vector (cv us vs)
			       :output-offset-vector (cv u0 v0))))
	  (setf (get-prop projection :rpc00a-errors) errors
		(get-prop projection :rpc00a-image-name) image-name)
	  (when (test-rpc-for-demoninator-zeros projection)
	    (error "RPC has denominator polynomial with zeros."))
	  
	  ;;(least-square-fit-polynomial-transform-to-inverse-projection projection)

	  (when uvh-bbox (setf (get-prop projection :uvh-bbox) uvh-bbox))
	  (if lvcs-to-wgs-84-lat-long-coordinate-transform
	      (make-rpc-lvcs-projection-from-lat-long-projection
	       lvcs-to-wgs-84-lat-long-coordinate-transform projection)
	      projection))))))

	     
;;; depends on cme-compat/site-glue.lisp
#+depends-on-site-glue 
(defun make-and-save-rpc-projection (rpc00a-rpf-string lvcs-to-lat-long-transform uvh-bbox
						      &key
						      2d-world
						      camera-model-pathname)
  (unless 2d-world (setq 2d-world (make-instance '2d-world :name "foo")))
  
  (let ((lvcs-proj (make-rpc-projection-from-rpc00a-string rpc00a-rpf-string
							  lvcs-to-lat-long-transform
							  uvh-bbox)))
    (setf (3d-to-2d-projection 2d-world) lvcs-proj)
    (save-3d-to-2d-projection 2d-world
			      (if camera-model-pathname *radius-site-glue* (site-glue 2d-world))
			      camera-model-pathname)
    lvcs-proj))


;;; **********************   RAT-POLY-PROJECTION CONSTRUCTORS AND METHODS  **********************

(defparameter *rpc-fit-default-n-list* '(7 7 5))

(defparameter *rational-polynomial-avoid-zeros* t)
#|
(setq *rpc-fit-default-n-list* '(11 11 7))
(setq *rpc-fit-default-n-list* '(7 7 5))
|#

;;; To get the best results,  set the :uvh-bbox property of geo-proj to the uvh-bbox of the image
;;; (setf (get-prop geo-proj :uvh-bbox) image-uvh-bbox)
(defun make-rpc-lvcs-projection-from-lat-long-projection (lvcs-to-lat-long-transform geo-proj)
  (let ((projection 
	 (if (and (get-prop geo-proj :uvh-bbox) (fboundp 'invert-transform))
      (make-rpc-lvcs-projection-from-lat-long-projection-uvh-bbox lvcs-to-lat-long-transform geo-proj)
      (make-rpc-lvcs-projection-from-lat-long-projection-3d-bbox lvcs-to-lat-long-transform geo-proj))))
    (when (test-rpc-for-demoninator-zeros projection)
	    (error "RPC has denominator polynomial with zeros."))
    projection))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From rational-polynomial-transforms.lisp:
(defvar  *fit-rational-polynomial-transform-to-function-solve-case* 5)


(defun rpc-poly (vector c result)
  (declare (type (simple-array double-float (20)) c)
	   (type (simple-array double-float (1)) result))
  (bind-vector-elements (x y z) vector
    (setf (aref result 0)
	  (the double-float 
	      (case (length c)
		(20 (+ (aref c 0)
		       (* x (+ (aref c 1)
			       (* x (+ (aref c 4) (* x (aref c 10)) (* y (aref c 11)) (* z (aref c 12))))
			       (* y (+ (aref c 5) (* y (aref c 13)) (* z (aref c 14))))
			       (* z (+ (aref c 6) (* z (aref c 15))))))
		       (* y (+ (aref c 2)
			       (* y (+ (aref c 7) (* y (aref c 16)) (* z (aref c 17))))
			       (* z (+ (aref c 8) (* z (aref c 18))))))
		       (* z (+ (aref c 3)
			       (* z (+ (aref c 9) (* z (aref c 19))))))
		       ))
		(10 (+ (aref c 0)
		       (* x (+ (aref c 1)
			       (* x (+ (aref c 4)))
			       (* y (+ (aref c 5)))
			       (* z (+ (aref c 6)))))
		       (* y (+ (aref c 2)
			       (* y (+ (aref c 7)))
			       (* z (+ (aref c 8)))))
		       (* z (+ (aref c 3)
			       (* z (+ (aref c 9)))))
		       ))
		(4 (+ (aref c 0)
		      (* x (+ (aref c 1)))
		      (* y (+ (aref c 2)))
		      (* z (+ (aref c 3)))
		      ))
		(1 (aref c 0))
		(otherwise (error "")))))
	      
    nil))

(defun rpc-poly1 (transform from-vector to-vector)
  (declare (type (simple-array double-float (*)) from-vector to-vector))
  (with-class-slot-values rational-polynomial-coordinate-transform
	(coeffs-vector-vector mean-vector scale-vector) transform
    (declare (simple-vector coeffs-vector-vector))
    (declare (type (simple-array double-float (*)) scale-vector))
    (let* ((v (load-time-value* (make-coordinate-vector 1)))
	   (v2 (load-time-value* (make-coordinate-vector 3))))
      (declare (type (simple-array double-float (*)) v v2))
      (if mean-vector
	  (bind-vector-elements (x y z) from-vector
	    (decf x (aref mean-vector 0))
	    (decf y (aref mean-vector 1))
	    (decf z (aref mean-vector 2))
	    (when scale-vector
	      (setf x (* x (aref scale-vector 0))
		    y (* y (aref scale-vector 1))
		    z (* z (aref scale-vector 2))))
		   
	    (set-coordinate-vector-elements v2 x y z))
	  (setq v2 from-vector))

      (rpc-poly v2 (aref coeffs-vector-vector 0) v)
      (setf (aref to-vector 0) (aref v 0))
      (rpc-poly v2 (aref coeffs-vector-vector 1) v)
      (setf (aref to-vector 1) (aref v 0))
      ;;(setf (aref to-vector 2) (aref from-vector 2))
      ;;(break)
      to-vector
      )))

(defmethod compute-transform-errors (trans1 trans2 bounding-box &key (n-trials 10000))
  (declare (type (simple-array double-float (*))  bounding-box))
  (let* ((n (ash (length bounding-box) -1))
	 (from-vect (make-coordinate-vector n))
	 (to-vect1 (make-coordinate-vector n))
	 (to-vect2 (make-coordinate-vector n))
	 (function-p1 (functionp trans1))
	 (*rat-poly-save-denoms* t))
    (declare (fixnum n)
	     (type (simple-array double-float (*))  from-vect to-vect1 to-vect2))
    (loop repeat n-trials
	  with err double-float
	  with max-err0 double-float
	  with max-err double-float = 0.0
	  with max-err-vect 
	  with pos-u-denom-count fixnum = 0
	  with pos-v-denom-count fixnum = 0
	  do (loop for i fixnum from 0 below n
		   for bbox-index fixnum from 0 by 2
		   do (setf (aref from-vect i)
			    (random-in-range (aref bounding-box bbox-index)
					     (aref bounding-box (1+ bbox-index)))))
	     (if function-p1
		 (funcall trans1 from-vect to-vect1)
		 (transform-vector trans1 from-vect to-vect1))
	     (setf (aref to-vect1 2) (aref from-vect 2))
	     (transform-vector trans2 from-vect to-vect2)
	     (setf (aref to-vect2 2) (aref from-vect 2))
	     (setq err (vector-to-vector-distance to-vect1 to-vect2))
	     (when (> 0 (aref *rat-poly-denoms* 0)) (incf pos-u-denom-count))
	     (when (> 0 (aref *rat-poly-denoms* 1)) (incf pos-v-denom-count))
	     (setq max-err0
		   (if t
		       err
		       (loop for i from 0 below n
			     maximize (abs (- (aref to-vect1 i) (aref to-vect2 i))) double-float)))
	  when (> max-err0 max-err)
	    do (setq max-err max-err0
		     max-err-vect (vector-copy from-vect))
	  sum (^2 err) into sum-err-sq double-float
	  finally (report-rat-poly-singularities "project-to-world x" n-trials pos-u-denom-count)
		  (report-rat-poly-singularities "project-to-world y" n-trials pos-v-denom-count)
		  (return (values max-err (sqrt (/ sum-err-sq n-trials)) max-err-vect
				  (list n-trials pos-u-denom-count pos-v-denom-count))))))


(defun COMPUTE-2d-VECTOR-ARRAY-BOUNDING-BOX (vector-array)
  (loop with xmin double-float = 1e20 with xmax double-float = -1e20
	with ymin double-float = 1e20 with ymax double-float = -1e20
	for i fixnum from 0 below (length vector-array)
	do (bind-vector-elements (x y) (aref vector-array i)
	     (when (< x xmin) (setq xmin x)) (when (> x xmax) (setq xmax x))
	     (when (< y ymin) (setq ymin y)) (when (> y ymax) (setq ymax y)))
	finally (return (values (cv xmin xmax ymin ymax)
				(cv (* .5 (+ xmin xmax))
				    (* .5 (+ ymin ymax)) )
				(cv (/ 2.0 (- xmax xmin))
				    (/ 2.0 (- ymax ymin)) )
				))))


(defun COMPUTE-VECTOR-ARRAY-BOUNDING-BOX (vector-array)
  (if (= (length (aref vector-array 0)) 2)
      (COMPUTE-2d-VECTOR-ARRAY-BOUNDING-BOX vector-array)
      (loop with xmin double-float = 1e20 with xmax double-float = -1e20
	    with ymin double-float = 1e20 with ymax double-float = -1e20
	    with zmin double-float = 1e20 with zmax double-float = -1e20
	    for i fixnum from 0 below (length vector-array)
	    do (bind-vector-elements (x y z) (aref vector-array i)
		 (when (< x xmin) (setq xmin x)) (when (> x xmax) (setq xmax x))
		 (when (< y ymin) (setq ymin y)) (when (> y ymax) (setq ymax y))
		 (when (< z zmin) (setq zmin z)) (when (> z zmax) (setq zmax z)))
	    finally (return (values (cv xmin xmax ymin ymax zmin zmax)
				    (cv (* .5 (+ xmin xmax))
					(* .5 (+ ymin ymax))
					(* .5 (+ zmin zmax)) )
				    (cv (/ 2.0 (- xmax xmin))
					(/ 2.0 (- ymax ymin))
					(/ 2.0 (- zmax zmin)) )
				    )))))

(defun compute-normalization-from-bbox (bounding-box)
  (declare (type (simple-array double-float (*)) bounding-box ))
  (let* ((n-dims (ash (length bounding-box) -1))
	 (means (make-array n-dims :element-type 'double-float))
	 (scales (make-array n-dims :element-type 'double-float)))
    (declare (type (simple-array double-float (*)) means scales ))
    (loop for i fixnum from 0 below n-dims
	  for bbox-index fixnum from 0 by 2 
	  for start double-float = (aref bounding-box bbox-index)
	  for end double-float = (aref bounding-box (1+ bbox-index))
	  do (setf (aref scales i) (if (= end start) 0.0 (/ (* .5 (- end start) )))
		   (aref means i) (* .5 (+ end start) )))
    (values means scales)))

(defun make-to-from-vectors-with-chebyshev-sampling (transform bounding-box n-list n-to-dims
							       &optional (zeros-p t))
  (let* ((n (apply '* n-list))
	 (from-coord-array (make-array n))
	 (to-coord-array (make-array n))
	 (n-dims (length n-list))
	 (pos (make-array n-dims :element-type 'fixnum :initial-element 0))
	 (ns (make-array n-dims :element-type 'fixnum))
	 (x-vector-vector (make-array n-dims))
	 (function-p (functionp transform)))
    (declare (type (simple-array t (*)) from-coord-array to-coord-array x-vector-vector))
    (declare (type (simple-array fixnum (*)) pos  ns ))
    (loop for i fixnum from 0 below n-dims
	  for bbox-index from 0 by 2
	  for ni fixnum = (elt n-list i)
	  for start double-float = (aref bounding-box bbox-index)
	  for end double-float = (aref bounding-box (1+ bbox-index))
	  for bma double-float = (* .5 (- end start) )
	  for bpa double-float = (* .5 (+ end start) )
	  do (setf (aref ns i) ni)
	     (let ((xv (make-array ni :element-type 'double-float)))
	       (declare (type (simple-array double-float (*)) xv))
	       (setf (aref x-vector-vector i) xv)
	       (if zeros-p
		   (loop for j fixnum from 0 below ni
			 with delta double-float = (/ pi (float ni))
			 for ang double-float from (* .5 delta) by delta
			 do (setf (aref xv j) (+ bpa (* bma (cos ang)))))
		   (loop for j fixnum from 0 to (floor ni 2)
			 with delta double-float = (/ (* .5 pi) (float (1- ni)))
			 for ang double-float from 0.0 by delta
			 for sin-ang = (sin ang)
			 for q = (* (- end start) sin-ang sin-ang)
			 do (setf (aref xv j) (+ start q))
			    (setf (aref xv (- ni 1 j)) (- end q))))))
	 
    (loop for i fixnum from 0 below n
	  with obs-i fixnum = 0
	  do (let ((from-vector (make-coordinate-vector n-dims))
		   (to-vector (make-coordinate-vector n-to-dims)))
	       (declare (type (simple-array double-float (*)) from-vector to-vector ))
	       (loop with carry = t
		     for dim fixnum from 0 below n-dims
		     for xv = (aref x-vector-vector dim)
		     for j fixnum = (aref pos dim )
		     do (setf (aref from-vector dim)
			      (aref (the (simple-array double-float (*)) xv) j))
		     when carry
		       do (let ((j (incf (aref pos dim))))
			    (if (= j (aref ns dim ))
				(setf (aref pos dim) 0)
				(setf carry nil))))
	       (setq to-vector (if function-p
				   (funcall transform from-vector to-vector)
				   (transform-vector transform from-vector to-vector )))
	       (when to-vector
		 (setf (aref from-coord-array obs-i) from-vector 
		       (aref to-coord-array obs-i) to-vector)
		 (incf obs-i)))
	  finally (unless (= obs-i n)
		    (setq from-coord-array (adjust-array-copy from-coord-array obs-i)
			  to-coord-array (adjust-array-copy to-coord-array obs-i))))
    (values from-coord-array to-coord-array x-vector-vector)))

(defun report-rat-poly-singularities (transform-name n-trials count)
  (unless (or (zerop count) (= count n-trials))
    (format t ";;; Warning: ~a  has a singular denominator.~%" transform-name)))

(defmethod compute-inverse-transform-errors (trans1 inv-trans2 bounding-box &key (n-trials 10000))
  (declare (type (simple-array double-float (*))  bounding-box))
  (let* ((n (ash (length bounding-box) -1))
	 (from-vect (make-coordinate-vector n))
	 (to-vect1 (make-coordinate-vector n))
	 (from-vect2 (make-coordinate-vector n))
	 (function-p1 (functionp trans1))
	 (*rat-poly-save-denoms* t))
    (declare (fixnum n)
	     (type (simple-array double-float (*)) from-vect to-vect1 from-vect2))
    (loop repeat n-trials
	  with err double-float
	  with max-err0 double-float
	  with max-err double-float = 0.0
	  with max-err-vect
	  with pos-u-denom-count fixnum = 0
	  with pos-v-denom-count fixnum = 0
	  do (loop for i fixnum from 0 below n
		   for bbox-index fixnum from 0 by 2
		   do (setf (aref from-vect i)
			    (random-in-range (aref bounding-box bbox-index)
						 (aref bounding-box (1+ bbox-index)))))
	     (if function-p1
		 (funcall trans1 from-vect to-vect1)
		 (transform-vector trans1 from-vect to-vect1))
	     (setf (aref to-vect1 2) (aref from-vect 2))
	     (transform-vector inv-trans2 to-vect1 from-vect2)
	     (setf (aref from-vect2 2) (aref from-vect 2))
	     (setq err (vector-to-vector-distance from-vect from-vect2))
	     (when (> 0 (aref *rat-poly-denoms* 0)) (incf pos-u-denom-count))
	     (when (> 0 (aref *rat-poly-denoms* 1)) (incf pos-v-denom-count))
	     (setq max-err0
		   (loop for i from 0 below n
			 maximize (abs (- (aref from-vect i) (aref from-vect2 i))) double-float))
	  when (> max-err0 max-err)
	    do (setq max-err max-err0
		     max-err-vect (vector-copy from-vect))
	  sum (^2 err) into sum-err-sq double-float
	  finally (report-rat-poly-singularities "project-to-view u" n-trials pos-u-denom-count)
		  (report-rat-poly-singularities "project-to-view v" n-trials pos-v-denom-count)
		  (return (values max-err (sqrt (/ sum-err-sq n-trials)) max-err-vect
				  (list n-trials pos-u-denom-count pos-v-denom-count))))))


(defun fit-rational-polynomial-transform-to-function
    (mapping-function
     &key  (n-list '(7 7 7))
     (poly-coeff-count '((20 20) (20 20)))
     (poly-function 'rpc-poly1)
     bounding-box
     (solve-case *fit-rational-polynomial-transform-to-function-solve-case*)
     (max-iters 2)
     (create-forward t)
     (create-inverse nil)
     (transform-class 'rational-polynomial-coordinate-transform)
     (inverse-transform-class 'rational-polynomial-coordinate-transform)
     (transform-function 'rat-poly-3-normalized-transform-vector)
     (inverse-transform-function 'rat-poly-3-normalized-transform-vector)
     recompute-to-from-vectors-fn
     (sampling :chebyshev)		; :random and :uniform are currently broken -- do not support means and scales
     from-vectors to-vectors means scales
     projection
     )
  (ignore projection)
  (multiple-value-setq (from-vectors to-vectors)
    (case sampling
      (:random
       (make-to-from-vectors-with-random-sampling mapping-function bounding-box (apply #'* n-list)))
      (:chebyshev
       (make-to-from-vectors-with-chebyshev-sampling mapping-function bounding-box n-list 3 t))
      (:uniform
       (make-to-from-vectors-with-uniform-sampling mapping-function bounding-box n-list))
      (otherwise (if (listp sampling)
		     (values-list sampling)
		     (error "Illegal value for :sampling: ~a" sampling)))))
  
  (multiple-value-setq (means scales) (compute-normalization-from-bbox bounding-box))
  (when recompute-to-from-vectors-fn
    (funcall recompute-to-from-vectors-fn poly-function  from-vectors to-vectors means scales))
  (flet ((fit-poly (from-vectors to-vectors means scales to-vector-coords to-coords poly-coeff-count)
	   (least-squares-fit-polynomial-transform
	    (make-rational-polynomial-coordinate-transform poly-function
							   poly-coeff-count
							   nil
							   means
							   :scale-vector scales)
	    from-vectors to-vectors ;; :to-coords 2
	     :normalize-from-vector nil
	    ;; :to-coords is duplicated here -- to-coords should be eliminated from arglist
	    ;;:to-vector-coords to-vector-coords :to-coords to-coords
	    ))
	 )

    (let* ((*iterative-sv-solve-max-iters* max-iters)
	   (*solve-case* solve-case)
	   transform inverse-transform
	   )

      (when create-forward
	(let* ((x-poly (fit-poly from-vectors to-vectors means scales '(0) '(0) (car poly-coeff-count)))
	       (y-poly (fit-poly from-vectors to-vectors means scales '(1) '(1) (car poly-coeff-count))))
	  (setq transform
		(with-class-slot-values rational-polynomial-coordinate-transform
		      ((xcoeffs coeffs-vector-vector) mean-vector scale-vector) x-poly
		  (with-class-slot-values rational-polynomial-coordinate-transform
			((ycoeffs coeffs-vector-vector)) y-poly
		    (make-instance transform-class
				   :coeffs-vector-vector
				   (vector (aref xcoeffs 0) (aref xcoeffs 1)
					   (aref ycoeffs 0) (aref ycoeffs 1))
				   :mean-vector mean-vector
				   :scale-vector scale-vector
				   :transform-function transform-function)
		    )))
	  (setf (get-prop transform :sv-suppressed)
		(list (get-prop x-poly :sv-suppressed)
		      (get-prop y-poly :sv-suppressed)))
	  (when mapping-function
	    (setf (get-prop transform :least-squares-fit-errors)
		  (multiple-value-list (compute-transform-errors mapping-function transform bounding-box))))))
	
	  
      (when create-inverse
	(multiple-value-bind (bbox means scales)
	    (compute-vector-array-bounding-box to-vectors)
	  (ignore bbox )
	  (let* ((x-poly (fit-poly to-vectors from-vectors means scales '(0) '(0) (cadr poly-coeff-count)))
		 (y-poly (fit-poly to-vectors from-vectors means scales '(1) '(1) (cadr poly-coeff-count))))
	    (setq inverse-transform
		  (with-class-slot-values rational-polynomial-coordinate-transform
			((xcoeffs coeffs-vector-vector) mean-vector scale-vector) x-poly
		    (with-class-slot-values rational-polynomial-coordinate-transform
			  ((ycoeffs coeffs-vector-vector)) y-poly
		      (make-instance inverse-transform-class
				     :coeffs-vector-vector
				     (vector (aref xcoeffs 0) (aref xcoeffs 1)
					     (aref ycoeffs 0) (aref ycoeffs 1))
				     :mean-vector mean-vector
				     :scale-vector scale-vector
				     :transform-function inverse-transform-function)
		      )))
	    ;;(setq *last_rat-poly* (list to-vectors from-vectors inverse-transform))
	    (setf (get-prop inverse-transform :sv-suppressed)
		  (list (get-prop x-poly :sv-suppressed)
			(get-prop y-poly :sv-suppressed)))
	    (when transform
	      (setf (inverse-transform transform) inverse-transform
		    (inverse-transform inverse-transform) transform))
	    (when mapping-function
	      (setf (get-prop inverse-transform :least-squares-fit-errors)
		    (multiple-value-list
			(compute-inverse-transform-errors mapping-function inverse-transform
							  bounding-box))
		    ))
	    )))
	  
      (values transform inverse-transform))))


(proclaim '(special *fit-rat-poly-create-inverse*))

(defvar *fit-rat-poly-create-inverse* nil)
(defparameter *fit-rational-polynomial-projection-poly-coeff-count* '((20 20) (20 20)))
(defparameter *fit-rational-polynomial-projection-transform-function*
  'rat-poly-3-normalized-project-vector)

(defun fit-rational-polynomial-projection-to-2d-to-3d-projection
    (2d-to-3d-projection &rest args
                         &key
                         bounding-box
                         (zp-projection-plane (coordinate-vector 0.0 0.0 1.0 0.0))
                         (inverse-transform-class 'rat-poly-projection)
                         (inverse-transform-function
                          *fit-rational-polynomial-projection-transform-function*)
                         (poly-coeff-count *fit-rational-polynomial-projection-poly-coeff-count*)
                         &allow-other-keys)
  (multiple-value-bind (inverse-transform projection)
      (apply 'fit-rational-polynomial-transform-to-function
             2d-to-3d-projection
             :poly-coeff-count poly-coeff-count
             :inverse-transform-class inverse-transform-class
             :inverse-transform-function inverse-transform-function
             args)
    (when projection
      (setf (zp-projection-plane projection) zp-projection-plane)
      (setf (get-prop projection :3d-bbox) bounding-box)
      )
    (values inverse-transform projection)))

;;; This 3D-BBOX VERSION generates the from-vectors over the 3d-bbox, and the to-vectors
;;;   = (transform-vector from-vectors)
;;; This coordinate range coverage isn't as good as possible when the uvh-bbox of the actual image
;;; is known.  Unfortunately, the RPC00AA info doesn't NECESSARILY provide a tight uvh-bbox.
;;; Also, the underlying RPC fit that generated the RPC00A output MIGHT NOT HAVE COVERED THE 3D-BBOX.
;;; It is really much better to use the UVH-BBOX version of the function.

(defun make-rpc-lvcs-projection-from-lat-long-projection-3d-bbox (lvcs-to-lat-long-transform geo-proj)
  (let* ((geo-bbox (bounding-box geo-proj))
	 (lvcs-bbox (transform-bounding-box (inverse-transform lvcs-to-lat-long-transform) geo-bbox))
	 (fit-funct (if (and *rational-polynomial-avoid-zeros*
			     (fboundp 'fit-rational-polynomial-projection-to-3d-to-2d-projection2))
			'fit-rational-polynomial-projection-to-3d-to-2d-projection2
			'fit-rational-polynomial-projection-to-3d-to-2d-projection)))
    (multiple-value-bind (lvcs-proj)
	(funcall fit-funct
		 (list lvcs-to-lat-long-transform geo-proj)
		 :create-forward t
		 :create-inverse *fit-rat-poly-create-inverse*
		 :n-list *rpc-fit-default-n-list*
		 :bounding-box lvcs-bbox)
	   
      (make-connections lvcs-proj
			(from-coordinate-system lvcs-to-lat-long-transform)
			(to-coordinate-system geo-proj))
    
      (setf (get-prop lvcs-proj :rpc00a-errors) (get-prop geo-proj :rpc00a-errors))
      (setf (get-prop lvcs-proj :rpc00a-image-name) (get-prop geo-proj :rpc00a-image-name))

      lvcs-proj)))


;;; This UVH-BBOX VERSION requires code supporting INVERT-PROJECTION and new version of PROJECT-TO-WORLD.
;;; This version generates the from-vectors over the uvh-bbox, and the to-vectors
;;;   = (inverse-transform-vector from-vectors)
;;; This version loses unless the geo-proj has its output normalization exactly optimized
;;; to cover the desired uvh-bbox.
;;; If the uvh-bbox of the actual image is known, do (setf (get-prop geo-proj :uvh-bbox) image-uvh-bbox)
;;; before calling make-rpc-lvcs-projection-from-lat-long-projection.

(defun make-rpc-lvcs-projection-from-lat-long-projection-uvh-bbox (lvcs-to-lat-long-transform geo-proj)
  (let* ((uvh-bbox (uvh-bbox geo-proj))
	 (good-fitter (and *rational-polynomial-avoid-zeros*
			   (fboundp 'fit-rational-polynomial-projection-to-3d-to-2d-projection2)))
	 (fit-funct (if good-fitter
			'fit-rational-polynomial-projection-to-2d-to-3d-projection2
			'fit-rational-polynomial-projection-to-2d-to-3d-projection))
	 )
    (multiple-value-bind (inverse lvcs-proj)
	(funcall fit-funct
		 (inverse-transform (list lvcs-to-lat-long-transform geo-proj))
		 :create-inverse t :create-forward *fit-rat-poly-create-inverse*
		 :projection (list lvcs-to-lat-long-transform geo-proj)
		 :n-list *rpc-fit-default-n-list*
		 :bounding-box uvh-bbox)
      (ignore inverse)

      (make-connections lvcs-proj
			(from-coordinate-system lvcs-to-lat-long-transform)
			(to-coordinate-system geo-proj))
    
      (setf (get-prop lvcs-proj :rpc00a-errors) (get-prop geo-proj :rpc00a-errors))
      (setf (get-prop lvcs-proj :rpc00a-image-name) (get-prop geo-proj :rpc00a-image-name))

      lvcs-proj)))

;;; The best we can do is map the -1:+1 range of output values thru the output normalization.
;;; Override this computation with :uvh-bbox property-list slot 
;;;(defmethod uvh-bbox ((projection rat-poly-projection))
;;;  (or (get-prop projection :uvh-bbox)
;;;      (with-class-slot-values rat-poly-projection
;;;            (output-scale-vector output-offset-vector mean-vector scale-vector) projection
;;;        (let ((mu (aref output-offset-vector 0))
;;;              (mv (aref output-offset-vector 1))
;;;              (su (aref output-scale-vector 0))
;;;              (sv (aref output-scale-vector 1))
;;;              (mz (aref mean-vector 2))
;;;              (sz (/ (aref scale-vector 2))))
;;;          (cv (- mu su) (+ mu su) (- mv sv) (+ mv sv) (- mz sz) (+ mz sz))))))


;;; this will do for new --- let (uvh-bbox rat-poly-projection) add the hmin hmax components.
#+never ; might not have the image package.
(defmethod uvh-bbox ((object img::image))
  (2d-bbox object))
  
 
(defmethod uvh-bbox ((projection rat-poly-projection))
  (let ((uvh-bbox (get-prop projection :uvh-bbox)))
    (if uvh-bbox
	(if (>= (length uvh-bbox) 6)
	    uvh-bbox
	    ;; add the z range to the uvh-bbox
	    (setf (get-prop projection :uvh-bbox)
		  (bind-vector-elements (umin umax vmin vmax) uvh-bbox
		    (let ((3d-bbox (bounding-box projection)))
		      (cv umin umax vmin vmax (aref 3d-bbox 4) (aref 3d-bbox 5))))))
	
	(transform-bounding-box projection (bounding-box projection)))))
#|
(list (uvh-bbox geo-proj)
      (transform-bounding-box geo-proj (bounding-box geo-proj)))

(list (3d-bbox geo-proj)
      (transform-bounding-box (inverse-transform geo-proj) (uvh-bbox geo-proj)))
|#


;;; **********************   RPC00A RATIONAL POLYNOMIAL PROJECTION FILE I/O   **********************



;;; LHQ Thu Feb 27 1997
;;; File format for the normalization parameters changed to be the min/max value range of coordinates
;;; rather tham means and scales.  ;;; This is primarily cosmetic, but also isolates the file format
;;; from choice of coordinate normalization.

;;; Thus for the World-to-Image-Projection we have the following normalization parameters:
;;;        umin vmin xmin ymin zmin
;;;        umax vmax xmax ymax zmax

;; set to RPC00A for RPC00AA compliance, min-max otherwise.
(defparameter *rpc-file-header-coord-normalization* 'RPC00A)
#|
(setq *rpc-file-header-coord-normalization* 'min-max)
|#


(defun write-generic-file-header (stream format-id keyword-value-list &key byte-alignment)
  (let ((*package* (find-package "CME")))
    (format stream "~a~%" format-id)
    (loop for (keyword value) on keyword-value-list by #'cddr
	  do (format stream "~s ~s~%" keyword value))
    (format stream "HEADER-END")
    (when byte-alignment
      (multiple-value-bind (ignore extra) (ceiling (1+ (file-position stream)) byte-alignment)
	(ignore ignore)
	(loop repeat (- extra)
	      do (write-char #\space stream))))
    (format stream "~%")		; the stream is now positioned at a multiple of byte-alignment bytes
    ))

;;; Issues about case sensitivity 
(defun read-generic-header (stream expected-format-id)
  (let ((*package* (find-package "CME")))
    ;; HEADER-END and other keywords are excpeted to be in the CME package.
    (when (equal (read-line stream) expected-format-id)
      (loop for keyword = (read-preserving-whitespace stream)
	    until (eq keyword 'HEADER-END)
	    for value = (read stream)
	    collect keyword into keyword-value-list
	    collect value into keyword-value-list
	    finally (loop until (char= (read-char stream) #\newline))
		    (return keyword-value-list)))))

(defmethod 3d-to-2d-projection-io-props (projection)
  nil)

#+never ;;; This version requires too much FREEDIUS intrastructure -- images and worlds
(defmethod 3d-to-2d-projection-io-props (projection)
  (let* ((to-coordinate-system (to-coordinate-system projection))
	 (2d-world (and to-coordinate-system (world to-coordinate-system)) )
	 (image-name (and 2d-world (name 2d-world)))
	 (from-coordinate-system (from-coordinate-system projection))
	 (3d-cs-type (typecase from-coordinate-system
		       (local-vertical-coordinate-system 'lvcs)
		       (lat-long-coordinate-system 'lat-long)
		       (otherwise nil)))
	 (3d-world (and from-coordinate-system (world from-coordinate-system))))
    `(,@(when 3d-world `(site ,(name 3d-world)))
	,@(when 3d-cs-type `(world-coordinates ,3d-cs-type))
	,@(when image-name `(image-name ,image-name))
	)))


;;; This version requires too much FREEDIUS intrastructure -- images and worlds
(defmethod write-rat-poly-projection-to-file
	   (projection pathname
		       &key
		       (coeff-format "~24,16,2E ")
		       (norm-format "~24,16,2E ")
		       (coord-format "~24,16F ")
		       (coeff-normalization  *rpc-file-header-coord-normalization*))
  (labels ((merge-norms (in out) (cv (aref out 0) (aref out 1) (aref in 0) (aref in 1) (aref in 2)))
	   (permute-coeffs (coeffs-vector-vector &optional (perm *sri-to-rpc00a-coeffs-permutation*))
	     (apply 'vector
		    (loop for i from 0 below 4
			  for coeffs = (aref coeffs-vector-vector i)
			  collect (loop for i in perm collect (nth i coeffs)))))
	   (gen-params (means scales out-offsets out-scales coeffs)
	     (bind-vector-elements (sx sy sz) scales
	       (values (merge-norms means
				    (or out-offsets (cv 0.0 0.0)))
		       (merge-norms (cv (/ sx) (/ sy) (/ sz))
				    (or out-scales (cv 1.0 1.0)))
		       (permute-coeffs-coeffs-vector coeffs *sri-to-rpc00a-coeffs-permutation*))))
	   )
    
    (let ((inv-proj (inverse-transform projection))
	  means scales coeffs inv-means inv-scales inv-coeffs)
      (unless (typep inv-proj 'RATIONAL-POLYNOMIAL-COORDINATE-TRANSFORM)
	(setq inv-proj nil))
      (with-class-slots rat-poly-projection
	    (mean-vector scale-vector output-offset-vector output-scale-vector coeffs-vector-vector)
	  projection
	(multiple-value-setq (means scales coeffs)
	  (gen-params mean-vector scale-vector output-offset-vector output-scale-vector coeffs-vector-vector)))
      (when inv-proj
	(with-class-slots RATIONAL-POLYNOMIAL-COORDINATE-TRANSFORM
	      (mean-vector scale-vector output-offset-vector output-scale-vector coeffs-vector-vector)
	    inv-proj
	  (multiple-value-setq (inv-means inv-scales inv-coeffs)
	    (gen-params mean-vector scale-vector output-offset-vector output-scale-vector coeffs-vector-vector))))
      (lx::backup-file pathname)
      (with-open-file (st pathname :direction :output)
	(labels ((write-scales-and-offsets (means scales)
		   (loop for i from 0 below (length means)
			 do (format st norm-format (aref means i)))
		   (terpri st)
		   (loop for i from 0 below (length scales)
			 do (format st norm-format (aref scales i)))
		   
		   (terpri st) (terpri st)
		   )
		 (write-mins-and-maxs (means scales)
		   (loop for i from 0 below (length means)
			 for min = (- (aref means i) (aref scales i))
			 do (format st coord-format min))
		   (terpri st)
		   (loop for i from 0 below (length means)
			 for max = (+ (aref means i) (aref scales i))
			 do (format st coord-format max))
		   (terpri st) (terpri st)
		   )
		 (write-normalization-params (means scales)
		   (if (eq coeff-normalization 'min-max)
		       (write-mins-and-maxs means scales)
		       (write-scales-and-offsets means scales)))
		     
		 (write-coeffs (coeffs-vector-vector &optional (nums-per-line 5))
		   (loop for j from 0 below 4
			 for coeffs = (aref coeffs-vector-vector j)
			 do (loop for i from 0 below (length coeffs)
				  do (format st coeff-format (aref coeffs i))
				  when (zerop (mod (1+ i) nums-per-line) ) do (terpri st))
			    (terpri st))
		   ;;(terpri st)
		   )		 
		 )
	  (flet ((fit-errors (proj)
		   (let ((errs (get-prop proj :rpc-fit-errors)))
		     (when errs
		       (destructuring-bind (((u-rms-err u-max-err &rest ig1) &rest ig1b)
					    ((v-rms-err v-max-err &rest ig2) &rest ig2b)
					    &rest ig3)
			   errs
			 (ignore ig1 ig1b ig2 ig2b ig3)
			 (list u-rms-err u-max-err v-rms-err v-max-err))))))
	    (let* ((error-info (get-prop projection :rpc00a-errors))
		   (plist `(,@(3d-to-2d-projection-io-props projection)
			      coeff-order rpc00a
			      coeff-normalization ,coeff-normalization
			      ,@(when error-info `(rpc00a-errors ,error-info))
			      fit-errors ,(fit-errors projection)
			      ,@(when inv-proj `(inverse-fit-errors ,(fit-errors inv-proj))))))
	    
	      ;;(format st "~a~%" *rpc-file-header*) (write-keywords plist)
	      (write-generic-file-header st *rpc-file-header* plist) ; depends on cme-compat/
	      (terpri st)
	      (format st "World-to-Image-Projection~%")
	      (write-normalization-params means scales)
	      (write-coeffs coeffs)
	      (when inv-proj
		(format st "Image-to-World-Transform~%")
		(write-normalization-params inv-means inv-scales)
		(write-coeffs inv-coeffs)) )))))))

#+obsolete
(progn

;;; replaces method in $CMEHOME/cme/site-glue.lisp
(defmethod get-3d-to-2d-projection-pathname-format-lists ((site-glue radius-site-glue))
  '((("~a/fbip" fbip) ("~a/rpc" rpc) ("~a/camera-model" t) )
    (("~a/camera-models/~a.fbip" fbip) ("~a/camera-models/~a.rpc" rpc) ( "~a/camera-models/~a" t))))

(defmethod load-3d-to-2d-projection ((2d-world 2d-world) (site-glue radius-site-glue)
				      (type (eql 'rpc))
				      path)
  (setf (3d-to-2d-projection 2d-world) (read-rat-poly-projection path)))


;;; This replaces definition in $CMEHOME/cme/site-glue.lisp
(defmethod save-3d-to-2d-projection2
	   ((simple-projection rat-poly-projection)
	    2d-world
	    (site-glue radius-site-glue)
	    2d-world-camera-model-path site-camera-model-path )
  (multiple-value-bind (3d-transform-list simple-proj 2d-transform-list)
      (decompose-composite-coordinate-projection (3d-to-2d-projection 2d-world))
    (ignore 2d-transform-list)
    (let ((rpc-path (format nil "~arpc" 2d-world-camera-model-path)))
      (unless nil ; (probe-file rpc-path)
	;;(format t ";;; Warning: writing RAT-POLY-PROJECTION to ~a~%" rpc-path)
	(write-rat-poly-projection-to-file simple-proj rpc-path)))
    (if 3d-transform-list
	(site-glue-save-projection site-camera-model-path
				   (pprint
				    `(compose-3d-transform-with-2d-world-projection
				      ,(if (consp 3d-transform-list)
					   `(list . ,(loop for 3d-trans in 3d-transform-list
							   collect (site-glue-fasd-form site-glue 3d-trans)))
					   `(site-glue-fasd-form ,site-glue ,3d-transform-list)))
				    st))
	;; rename the any existing camera-model path
	(lx::backup-file-by-rename site-camera-model-path) )))


) ; end #+obsolete progn



;;; was in rpc-fit.lisp -- is there a better way to deal with this?
;;;
(defun poly-xyz-3 (vector c result)
  (declare (type (simple-array double-float (20)) c)
	   (type (simple-array double-float (*)) result))
  (bind-vector-elements (x y z) vector
    (setf (aref result 0)
	  (+ (aref c 0)
	     (* x (+ (aref c 1)
		     (* x (+ (aref c 4) (* x (aref c 10)) (* y (aref c 11)) (* z (aref c 12))))
		     (* y (+ (aref c 5) (* y (aref c 13)) (* z (aref c 14))))
		     (* z (+ (aref c 6) (* z (aref c 15))))))
	     (* y (+ (aref c 2)
		     (* y (+ (aref c 7) (* y (aref c 16)) (* z (aref c 17))))
		     (* z (+ (aref c 8) (* z (aref c 18))))))
	     (* z (+ (aref c 3)
		     (* z (+ (aref c 9) (* z (aref c 19))))))
	     ))	      
    nil))

(defun make-rat-poly-projection2 (means scales coeffs
					inv-means inv-scales inv-coeffs
					&rest args
					&key (zp-projection-plane (cv 0.0 0.0 1.0 0.0)))
  (ignore args)
  (labels ((split-norm (vect)
	     (values (cv (aref vect 2) (aref vect 3) (aref vect 4)) ; input norm
		     (cv (aref vect 0) (aref vect 1) 0.0) ; output norm
		     ))
	   (make-rat-trans (class transform-function means scales coeffs &rest args)
	     (setq coeffs (permute-coeffs-coeffs-vector coeffs *rpc00a-to-sri-coeffs-permutation*))
	     (multiple-value-bind (in-means out-means) (split-norm means)
	       (multiple-value-bind (in-scales out-scales) (split-norm scales)
		 (bind-vector-elements (sx sy sz) in-scales
		   (apply 'make-instance class
			  :mean-vector in-means :scale-vector (cv (/ sx) (/ sy) (/ sz))
			  :output-offset-vector out-means :output-scale-vector out-scales
			  :transform-function transform-function
			  :COEFFS-VECTOR-VECTOR coeffs
			  args)))))
	   )
    (let ((proj (make-rat-trans 'rat-poly-projection
				'RAT-POLY-3-NORMALIZED-PROJECT-VECTOR
				means scales coeffs
				:zp-projection-plane zp-projection-plane))
	  inv-proj)
      (when inv-coeffs
	(setq inv-proj (make-rat-trans 'RATIONAL-POLYNOMIAL-COORDINATE-TRANSFORM
				       'RAT-POLY-3-NORMALIZED-TRANSFORM-VECTOR
				       inv-means inv-scales inv-coeffs))
	
	(setf (inverse-transform proj) inv-proj
	      (inverse-transform inv-proj) proj))
      
      proj)))


;;; ***************************   REPLACEMENT CONVERT-TO-RPC-PROJECTION   ********************************

;;; The global *FIT-RAT-POLY-CREATE-INVERSE* controls whether inverse-projections are
;;; built by the rational polynomial fitter.
;;; In the new scheme of things (with project-to-world using the Newton method invert-transform),
;;; rational-polynomial-projection inverses should not be generated.

;;; FIXME:  Class 2d-world doesn't exist when running without FREEDIUS.
;;;         Otherwise, :RPC-PROJECTIONS is clean as a self contained subsystem.
;;; This replaces definition in $CMEHOME/math/rational-polynomial-transforms.lisp

#+never ;; not really needed here since change-projection is defined in basic-gui/worlds.lisp
(defmethod change-projection ((world 2d-world) new-projection)
  (setq *proj* (print new-projection))
  (push (3d-to-2d-projection world) (get-prop world :previous-projection ))
  (setf (3d-to-2d-projection world) new-projection))

(defun convert-to-rpc-projection (2d-world
				  &key bounding-box (n-list '(7 7 7))
				  &allow-other-keys)
  (ignore n-list)
  (let ((*rational-polynomial-accept-threshold* 1e-4)
	(fit-funct (if (and *rational-polynomial-avoid-zeros*
			    (fboundp 'fit-rational-polynomial-projection-to-2d-to-3d-projection2))
		       'fit-rational-polynomial-projection-to-2d-to-3d-projection2
		       'fit-rational-polynomial-projection-to-2d-to-3d-projection)))
    (declare (special *rational-polynomial-accept-threshold*))
    (multiple-value-bind (inverse projection)
	(funcall fit-funct
		 ;;(inverse-transform (3d-to-2d-projection 2d-world))
		 ;; inverse-transform should now do the correct thing 
		 (make-project-to-world (3d-to-2d-projection 2d-world))
		 :n-list n-list
		 :bounding-box bounding-box
		 :projection (3d-to-2d-projection 2d-world)
		 :create-inverse nil ; t
		 :create-forward *fit-rat-poly-create-inverse*
		 )
      (ignore inverse)
      (change-projection 2d-world projection))))


;;; ************************************   TESTS  ************************************

#|

(PARSE-RPC00A-RPF-FILE "$RC/SRI/rpc/image1.rpc00a-rpc")


;;; for setup of lvcs coordinate system and testing of fit to composite projection
(setq geo-proj (MAKE-RPC-PROJECTION-FROM-RPC00A-STRING
		(read-file-to-string "$RC/SRI/rpc/image1.rpc00a-rpc")))

;;; create lvcs-to-lat-long-transform for a location centered at center of (3d-bbox geo-proj)
;;;(defparameter *xxx-lvcs-to-lat-long-transform*
;;;  (let* ((lvcs-to-WGS-84-geocentric-transform
;;;          (bind-vector-elements (long-min long-max lat-min lat-max ) (3d-bbox geo-proj)
;;;            (make-lvcs-to-geocentric-transform
;;;             ;;:local-vertical-coordinate-system *xxx-local-vertical-coordinate-system*
;;;             :origin-lat (* .5 (+ lat-min lat-max))
;;;             :origin-long  (* .5 (+ long-min long-max))
;;;             ;; these next would be correctly defaulted if not supplied
;;;             :local-units-per-meter 1.0
;;;             :lat-long-to-geocentric-transform (to-geocentric-transform (get-gdc 'wgs84))
;;;             )))
;;;         (from-cs (from-coordinate-system lvcs-to-WGS-84-geocentric-transform)))
;;;    (ignore from-cs)
;;;    ;; this next is rather bogus
;;;    #+never
;;;    (unless (get-prop from-cs :world-center)
;;;      (setf (get-prop from-cs :world-center) (cv 0.0 0.0 0.0)))
;;;    (find-transform-to (from-coordinate-system lvcs-to-WGS-84-geocentric-transform)
;;;                       (from-coordinate-system (to-geocentric-transform (get-gdc 'wgs84))))))

(defparameter *xxx-lvcs-to-lat-long-transform*
  (let* ((lvcs
	  (bind-vector-elements (long-min long-max lat-min lat-max ) (bounding-box geo-proj)
	    (make-lvcs 'wgs84 (* .5 (+ lat-min lat-max)) (* .5 (+ long-min long-max))))))
    (find-transform-to lvcs (get-gdc 'wgs84))))


(list (uvh-bbox geo-proj)
      (transform-bounding-box geo-proj (bounding-box geo-proj)))


(setq lvcs-proj (MAKE-RPC-PROJECTION-FROM-RPC00A-STRING
		 (read-file-to-string "$RC/SRI/rpc/image1.rpc00a-rpc")
		 *xxx-lvcs-to-lat-long-transform*
		 (cv 0.0 2048.0 0.0 2048.0 170.0 450.0) ; some estimate the the uvh-bbox
		 ))

(setq lvcs-proj2 (MAKE-RPC-PROJECTION-FROM-RPC00A-STRING
		 (read-file-to-string "$RC/SRI/rpc/image1.rpc00a-rpc")
		 *xxx-lvcs-to-lat-long-transform*
		 ))

(make-and-save-rpc-projection (read-file-to-string "$RC/SRI/rpc/image1.rpc00a-rpc")
			      *xxx-lvcs-to-lat-long-transform*
			      (cv 0.0 2048.0 0.0 2048.0 170.0 450.0)
			      :camera-model-pathname
			      "/tmp/rpc1.rpc")
(setq lvcs-proj-read (load-camera-model nil "/tmp/rpc1.rpc"))


(setq old-lvcs-proj lvcs-proj)
(setq lvcs-proj2 geo-proj)

(setq 3d-world (make-3d-world :name "foo-3d"
			      :lvcs-to-geocentric-transform
			      (find-transform-to
			       (from-coordinate-system *xxx-lvcs-to-lat-long-transform*)
			       (get-gcc 'wgs84))))

(progn 
  (setf 2d-world (make-instance '2d-world :name "foo")
	(3d-to-2d-projection 2d-world) lvcs-proj
	glue (make-instance 'radius-site-glue2 ))
  (make-connections lvcs-proj (from-coordinate-system lvcs-proj) (make-default-coordinate-system  2d-world)))

(save-3d-to-2d-projection 2d-world glue "$RC/SRI/rpc/tests/foo.camera-model")
(2d-world-path 2d-world glue)
(read-rat-poly-projection "$RC/SRI/rpc/tests/foo.camera-modelrpc")

;;; for testing fasd-form adequacy
(setf lvcs-proj2 (eval (fasd-form lvcs-proj))
      (from-coordinate-system lvcs-proj2) (from-coordinate-system lvcs-proj))
(progn geo-proj)
(progn lvcs-proj2)
(progn lvcs-proj)
(fasd-form lvcs-proj)

(compute-gsd lvcs-proJ (cv 1000.0 -100.0 150.0))

(let* ((pt (cv 1000.0 -100.0 150.0))
       (lvcs-proj lvcs-proJ)
       (uv1 (transform-vector lvcs-proj pt))
       (uv2 (transform-vector (list *xxx-lvcs-to-lat-long-transform*  geo-proj) pt)))
  (setf (aref uv2 2) (aref pt 2))
  (bind-vector-elements (u v) uv1
    (let* (;;(pt2 (project-vector-to-world lvcs-proj uv1 (aref pt 2)))
	   (uvz (cv u v (aref pt 2)))
	   ;;(pt2 (invert-transform lvcs-proj uvz nil 1e-3 1e-2))
	   (pt2 (invert-transform lvcs-proj uvz))
	   (pt3 (inverse-transform-vector lvcs-proj uvz))
	   (pt4 (project-vector-to-world lvcs-proj uvz))
	   ;;(pt4 (inverse-transform-vector (list *xxx-lvcs-to-lat-long-transform*  geo-proj) uvz))
	   )
      (list uv1 uv2
	    ;; test consistancy in fit to composite projection
	    (vector-difference uv1 uv2)
	    ;; test round trip closure
	    pt3
	    (vector-difference pt2 pt) ; great closure with invert-transform
	    (vector-difference pt3 pt)
	    (vector-difference pt4 pt)
	    ))))

(let* ((pt (cv 100.0 -100.0 0.0))
       (lvcs-proj lvcs-proj)
       (uv1 (transform-vector lvcs-proj pt))
       (uv2 (transform-vector (list *xxx-lvcs-to-lat-long-transform*  geo-proj) pt)))
  (bind-vector-elements (u v) uv1
    (let* (;;(pt2 (project-vector-to-world lvcs-proj uv1 (aref pt 2)))
	   (uvz (cv u v (aref pt 2)))
	   )
      (list uv1 uv2 (vector-difference uv1 uv2)
	    ;; test round trip closure
	    ))))

(let* ((i 0) (l rpc-fit::foo2)
       (from-pt (aref (car l) i))
       (to-pt (aref (cadr l) i))
       ;;(to-pt2 (transform-vector (list *xxx-lvcs-to-lat-long-transform*  geo-proj) from-pt))
       (to-pt2 (transform-vector lvcs-proj from-pt))
       (to-pt3 (transform-vector lvcs-proj-good from-pt))
       )
  
  (list to-pt to-pt2 (vector-difference to-pt to-pt2) to-pt3 (vector-difference to-pt to-pt3)))  

(progn rpc-fit::foo3)
(progn rpc-fit::foo2)

(math::generate-rpc-norms-file "$RC/SRI/rpc/tests/rpc.norms"
			       (list (nth 2 rpc-fit::foo2) (nth 3 rpc-fit::foo2)))

(math::generate-rpc-pts-file "$RC/SRI/rpc/tests/rpc.pts"
			     (nth 0 rpc-fit::foo2) (nth 1 rpc-fit::foo2))
~/cme/photogrammetry/rpc/c/v960307/rpc-fit rpc.pts rpc.norms rpc.fit

(let ((pt (cv 100.0 0.0 0.0)))
  (bind-vector-elements (u v) (transform-vector lvcs-proj pt)
    (let ((uvz (cv u v (aref pt 2))))
      (invert-transform lvcs-proj uvz nil ;(get-prop lvcs-proj :last-world-pt)
			1e-2 1e-2))))

(setq uvz (cv 20.0 50.0 100.0))
(let* ((uvz (cv 20.0 50.0 100.0))
       (proj geo-proj)
       (xyz1 (inverse-transform-vector proj uvz))
       (xyz2 (project-vector-to-world proj uvz))
       (xyz3 (invert-transform proj uvz)))
  (list xyz1 xyz2 xyz3 (transform-vector proj xyz3)))

(let* ((uvz (cv 20.0 50.0 100.0))
       (proj lvcs-proj)
       (xyz1 (inverse-transform-vector proj uvz))
       (uvz2 (transform-vector proj xyz1))
	 
       (xyz3 (invert-transform proj uvz))
       
       )
  (list xyz1
        xyz3
	(vector-difference xyz1 xyz3)
	(vector-difference uvz2 uvz)))

(inspect lvcs-proj)



(setf (get-prop (3d-to-2d-projection (top-view)) :rpc00a-errors) '(.1234 .0543))
(write-rat-poly-projection-to-file (3d-to-2d-projection (top-view)) "~/tmp/test.rpc")
(read-rat-poly-projection "~/tmp/test.rpc")
(setq proj (read-rat-poly-projection "~/tmp/test.rpc"))
(transform-vector proj (cv 100.0 200.0 300.0))
 = (CV 1159.891897539103 1342.88318957824 300.0)
(progn proj)
(setq partials (projection-partial-derivitives proj (cv 100.0 200.0 300.0)))
(setq partials (projection-partial-derivitives proj
					       (RAT-POLY-PROJECTION-mean-vector proj)))

(normalize-coordinate-vector 
 (vector-cross-product
 (cv (aref partials 0 0) (aref partials 0 1) (aref partials 0 2) )
 (cv (aref partials 1 0) (aref partials 1 1) (aref partials 1 2) )))
(setf (get-prop proj :last-world-pt) nil)
(invert-transform proj (CV 1159.891897539103 1342.88318957824 300.0))
(setf (rat-poly-projection-ZP-PROJECTION-PLANE proj)
      (CV -0.11375218406477664 -0.051535239842575256 0.9921716382132989 50.0))
(invert-transform proj (CV 1159.891897539103 1342.88318957824 325.96922508899695) )
;;; good
(setf (rat-poly-projection-ZP-PROJECTION-PLANE proj)
      (CV 0.0 0.0 1.0 0.0))

(math::multiply-matrices (car foo) (cadr foo))
(rational-poynomial-transform-partial-derivitives proj (cv 100.0 200.0 300.0))
(make-connections proj (coordinate-system (3d-world (top-view))) (coordinate-system (2d-world (top-view))))
(progn proj)


(save-3d-to-2d-projection (2d-world (top-view)) (site-glue (2d-world (top-view))))
(setf (3d-to-2d-projection (2d-world (top-view))) nil)
(get-3d-to-2d-projection (2d-world (top-view)) (site-glue (2d-world (top-view))))


(setq comp-proj (list (inverse-transform *xxx-lvcs-to-lat-long-transform*) lvcs-proj))

(let* ((pt (transform-vector *xxx-lvcs-to-lat-long-transform* (cv 20.0 50.0 100.0)))
       (uv1 (transform-vector comp-proj pt)))
  (list uv1
	pt
	;;(inverse-transform-vector comp-proj uv1)
	(project-vector-to-world comp-proj uv1))) 
|#
			      

