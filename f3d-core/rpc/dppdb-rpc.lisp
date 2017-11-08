(in-package :transforms)

;;; ****************************  DPPDB-RPC-PROJECTION  ****************************

(defstruct-class dppdb-rpc-projection (rpc-projection) 
   ((poly-fn :initform #'dppdb-poly-xyz :initarg :poly-fn :accessor poly-fn)
    ))
    
(defmethod initialize-instance :after
	   ((projection dppdb-rpc-projection) &key create-inverse &allow-other-keys)
  (with-class-slots dppdb-rpc-projection (inverse-transform transform-function) projection
    (setf transform-function #'dppdb-rpc-projection-transform-vector)
    (when create-inverse
      (setf inverse-transform
	    (make-iterative-inverse-transform projection :eps-x 1.6e-9)) ; 1cm on ground
      )))

(defmethod test-rpc-for-demoninator-zeros ((projection dppdb-rpc-projection))
  (with-class-slots dppdb-rpc-projection (x-den-coeffs y-den-coeffs poly-fn) projection
    (let* ((bbox (cv -1.0 1.0 -1.0 1.0 -1.0 1.0 ))
	   (u-denom-zero-bbox
	    (rpc-test-for-zero x-den-coeffs bbox poly-fn))
	   (v-denom-zero-bbox
	    (rpc-test-for-zero y-den-coeffs bbox poly-fn)))
      (when u-denom-zero-bbox
	(format t "rat-poly-projection has u-denominator zeros near pt: ~a~%"
		u-denom-zero-bbox))
      (when v-denom-zero-bbox
	(format t "rat-poly-projection has v-denominator zeros near pt: ~a~%"
		v-denom-zero-bbox))
      (or u-denom-zero-bbox v-denom-zero-bbox))))


(defparameter *dppdb-coeffs-order*
  '(1 x y z xy xz yz xx yy zz xyz xxx xyy xzz xxy yyy yzz xxz yyz zzz))
;;  0 1 2 3 4  5  6  7  8  9  10  11  12  13  14  15  16  17  18  19

;;; general 3rd order polynomial in x y z for DPPDB coefficient order
(defun dppdb-poly-xyz (vector c result)
  (declare (optimize (safety 0) (speed 3)))
  (declare (type (simple-array double-float (3)) vector)
	   (type (simple-array double-float (20)) c)
	   (type (simple-array double-float (1)) result))
  (let ((x (aref vector 0))
	(y (aref vector 1))
	(z (aref vector 2)))
    (declare (double-float x y z))
    (setf (aref result 0)
	  (+ (aref c 0)
	     (* x (+ (aref c 1)
		     (* x (+ (aref c 7) (* x (aref c 11)) (* y (aref c 14)) (* z (aref c 17))))
		     (* y (+ (aref c 4) (* y (aref c 12)) (* z (aref c 10))))
		     (* z (+ (aref c 5) (* z (aref c 13))))))
	     (* y (+ (aref c 2)
		     (* y (+ (aref c 8) (* y (aref c 15)) (* z (aref c 18))))
		     (* z (+ (aref c 6) (* z (aref c 16))))))
	     (* z (+ (aref c 3)
		     (* z (+ (aref c 9) (* z (aref c 19))))))
	     ))	      
    nil))
   
(defun dppdb-rpc-projection-transform-vector (trans X &optional U)
  (declare (optimize (safety 0) (speed 3)))
  (when X
    (unless U (setq U (make-coordinate-vector 3)))
    (with-class-slot-values dppdb-rpc-projection
	  (x-num-coeffs x-den-coeffs y-num-coeffs y-den-coeffs scales offsets) trans
      (declare (type (simple-array double-float (*))
		     x-num-coeffs x-den-coeffs y-num-coeffs y-den-coeffs scales offsets))
      (let ((X X) (U U)
	    (scaled-x *rpc-projection-transform-vector-scaled-x*)
	    (tmp *rpc-projection-transform-vector-tmp*)
	    )
	(declare (type (simple-array double-float (*)) scales offsets X U scaled-x tmp))
	(setf (aref scaled-x 0) (* (- (aref x 0) (aref offsets 0)) (aref scales 0)))
	(setf (aref scaled-x 1) (* (- (aref x 1) (aref offsets 1)) (aref scales 1)))
	(setf (aref scaled-x 2) (* (- (aref x 2) (aref offsets 2)) (aref scales 2)))
	(macrolet ((poly (coeffs)
		     `(progn (dppdb-poly-xyz scaled-x ,coeffs tmp)
		       (aref tmp 0))))
	  (setf (aref U 0) (+ (aref offsets 3)
			      (/ (poly x-num-coeffs) (* (poly x-den-coeffs) (aref scales 3))))
		(aref U 1) (+ (aref offsets 4)
			      (/ (poly y-num-coeffs) (* (poly y-den-coeffs) (aref scales 4))))
		(aref U 2) (aref X 2))
	  U)))))

(defun make-dppdb-rpc-projection-from-ikonus-file (path)
  (with-open-file (st path)
    (labels ((read-float ()		; read from a labeled line
	       (let* ((line (read-line st))
		      (p (position #\space line)))
		 (read-from-string line nil nil :start p)))
	     (read-float-vector (n)
	       (let ((v (make-array n :element-type 'double-float)))
		 (loop for i from 0 below n
		       do (setf (aref v i) (read-float)))
		 v))
	     (read-scales-or-offsets ()
	       (let ((v (make-array 5 :element-type 'double-float)))
		 (setf (aref v 3) (read-float) ; line scale or offseet
		       (aref v 4) (read-float) ; samp
		       (aref v 1) (read-float) ; lon
		       (aref v 0) (read-float) ; lat
		       (aref v 2) (read-float) ; elev
		       )
		 v))
	     )
      (let ((offsets (read-scales-or-offsets))
	    (scales (read-scales-or-offsets))
	    (x-num-coeffs (read-float-vector 20))
	    (x-den-coeffs (read-float-vector 20))
	    (y-num-coeffs (read-float-vector 20))
	    (y-den-coeffs (read-float-vector 20)))
	(loop for i from 0 below 5
	      do (setf (aref scales i) (/ (aref scales i))))
	(make-instance 'dppdb-rpc-projection
		       :x-num-coeffs x-num-coeffs
		       :x-den-coeffs x-den-coeffs
		       :y-num-coeffs y-num-coeffs
		       :y-den-coeffs y-den-coeffs
		       :scales scales
		       :offsets offsets)))))


;;; Should make-dppdb-rpc-projection-from-ikonus-file and setup-dppdb-lat-long-rpc-projection
;;; be combined?
;;; This is the only function that depends on the cme subsystem.
#+never
(defun setup-dppdb-lat-long-rpc-projection (lat-long-rpc image 3d-world 2d-world-or-name)
  (let* ((2d-world (if (typep 2d-world-or-name 'obj::gl-2d-world)
		       2d-world-or-name
		       (cme::make-2d-world :name 2d-world-or-name :3d-world 3d-world)))
	 (lvcs-to-image-projection (make-lat-long-rpc-projection-surrogate lat-long-rpc 3d-world image)))
    (setf (3d-to-2d-projection 2d-world) lvcs-to-image-projection)
    (cme::setup-image-worlds image :2d-world 2d-world)
    (values lvcs-to-image-projection 2d-world)))

#|
(disassemble 'dppdb-poly-xyz)
(disassemble 'dppdb-rpc-projection-transform-vector)

;;; 30 meters is approximately 1 arc second of latitude =>

(let ((rads/meter (radians (/ (* 30 3600)))))
  (* .01 rads/meter))
|#
 

;;; Test Code

  
#|
(defun image-rotate-cw-90-transform (image)
  (let ((x0 0.0)
	(y0 0.0)
	(ydim (img::image-y-dim image)))
    (make-4x4-coordinate-transform 
     (make-and-fill-4x4-matrix
      0.0 -1.0 0.0 (float (- ydim y0)) ; should this be (- ydim y0 1) ?
      1.0 0.0 0.0 (float x0)
      0.0 0.0 1.0 0.0
      0.0 0.0 0.0 1.0))))
 
(defun test-ortho (q)
  (let ((q3x3 (math::copy-matrix-rows q nil '(0 1 2) '(0 1 2) 3)))
    (multiply-matrices q3x3 (transpose-matrix q3x3))))

(maybe-compile-file-load "$FREEDIUS/lisp/transforms/dppdb-rpc.lisp")


(load "/opt/IU/radius/sites/san-diego/3d-world.lisp")

(describe cme::*san-diego-3d-world*)

(setq po_39940_pan_0000010000-tiled-256
      (img::load-image "/opt/IU/radius/site-2d-worlds/san-diego/IKONOS/tiff_rpc/stereo_8bit_pan_tiff/po_39940_pan_0000010000-tiled-256.tif"))

(setq *po_39940_pan_0000010000-lat-long-rpc*
      (MAKE-DPPDB-RPC-PROJECTION-FROM-IKONUS-FILE
       "/opt/IU/radius/site-2d-worlds/san-diego/IKONOS/tiff_rpc/stereo_8bit_pan_tiff/po_39940_pan_0000010000_rpc.txt"))

(describe *po_39940_pan_0000010000-lat-long-rpc*)

(setup-dppdb-lat-long-rpc-projection *po_39940_pan_0000010000-lat-long-rpc*
				     po_39940_pan_0000010000-tiled-256
				     cme::*san-diego-3d-world*
				     "po_39940_pan_0000010000")

;;; Apparently the tiff image is rotated relative to RPC line, samp coordinate system,
;;; probably so that North is up in image.
(setf (get-prop po_39940_pan_0000010000-tiled-256 :image-to-2d-transform)
      (image-rotate-cw-90-transform po_39940_pan_0000010000-tiled-256))

(describe po_39940_pan_0000010000-tiled-256)
(gui::push-image po_39940_pan_0000010000-tiled-256 (gui::selected-window gui::*interactor*))

(cme::load-site-feature-sets cme::*san-diego-3d-world*
			     (cme::load-site-glue "san-diego" cme::*radius-site-glue*))


(setf (gui::object-sets (gui::top-view)) (gui::default-object-sets (3d-world (2d-world (gui::top-view)))))

(setq gui::*invalidate-all-object-sets* t)
(setq gui::*invalidate-all-object-sets* nil)

(transform-vector (3d-to-2d-projection (gui::top-view)) (cv 0.0 1.0 0.0))
(vector-difference (transform-vector (3d-to-2d-projection (gui::top-view)) (cv 0.0 1.0 0.0))
		   (transform-vector (3d-to-2d-projection (gui::top-view)) (cv 0.0 0.0 0.0)))
;;; North vector is Up vector in image  (after correction)
#(-0.20018929297202703 0.9797616726000342 7.862354323151521e-8)

(describe (surrogate-projection (3d-to-2d-projection (gui::top-view))))
(describe (3d-to-2d-projection (gui::top-view)))
(invert-matrix (3D-TO-CAMERA-MATRIX (surrogate-projection (3d-to-2d-projection (gui::top-view)))))
(projection-matrix (surrogate-projection (3d-to-2d-projection (gui::top-view))))

(print *svd-errs*)

(setq 3d-pos (obj::fragment-position (cadr (car (gui::selected-objects)))))

(setq 3d-pos (cv 0.0 0.0 0.0))
(setq *print-array* t)

(transform-vector (3d-to-2d-projection po_39940_pan_0000010000-tiled-256) 3d-pos)
(transform-vector (surrogate-projection (3d-to-2d-projection po_39940_pan_0000010000-tiled-256)) 3d-pos)
(let ((*transform-dammit* t)) (transform-vector (surrogate-projection (3d-to-2d-projection po_39940_pan_0000010000-tiled-256)) 3d-pos))


(describe (car (gui::object-sets (gui::top-view))))

(gui::default-object-sets (2d-world (gui::top-view)))
(gui::default-object-sets (3d-world (2d-world (gui::top-view))))

(describe (get-prop (2d-world (gui::top-view)) :DEFAULT-OBJECT-SETS))
(describe (3d-world (2d-world (gui::top-view)))))
(describe (get-prop (3d-world (2d-world (gui::top-view))) :DEFAULT-OBJECT-SETS))

(setf (get-prop (2d-world (gui::top-view)) :DEFAULT-OBJECT-SETS) nil)

(print *foo*)
(nth 4 *foo*)
(gui::decompose-interior-orientation-matrix  (nth 4 *foo*)))
(cme::decompose-camera-to-2d-matrix (nth 4 *foo*) 1e-5)

(multiply-matrices (invert-matrix (nth 4 *foo*)) (nth 2 *foo*))
(nth 0 *foo*)
(print cme::*bar*)
(projection-matrix (cadr *foo*))

(defun project-4x4 (m p)
  (bind-vector-elements (x y z) p
    (let* ((p4 (cv x y z 1.0))
	   (mp (matrix-times-vector m p4)))
      (values (/ (aref mp 0) (aref mp 3))
	      (/ (aref mp 1) (aref mp 3))
	      (aref mp 3)))))

(car *foo*)
(setq 3d-pos (cv 1000.0 2000.0 200.0))
(project-4x4 (car *foo*) 3d-pos)
(project-4x4 *pmat* 3d-pos)
(transform-vector (3d-to-2d-projection (top-view)) 3d-pos))
(transform-vector (surrogate-projection (3d-to-2d-projection (top-view))) 3d-pos)
(transform-vector (surrogate-projection (3d-to-2d-projection (top-view)))
		  (vector-add 3d-pos (cv .0 0.0 1.0)))

(car *foo*)
(maybe-compile-file-load "/homedir/quam/cme/transforms/rq-decomposition.lisp")

(setq *foo* (list (projection-matrix (3d-to-2d-projection (top-view)))))
(describe (3d-to-2d-projection (gui::top-view)))


(multiple-value-bind (q params r)
    (cme::decompose-projection-matrix (nth 0 *foo*) :roff 1.0 :1/f-threshold 1e-5)
  (values r
	  q
	  (setq *pmat* (multiply-matrices r q))
	  (nth 0 *foo*)
	  (math::matrix-subtract (nth 0 *foo*) (multiply-matrices r q)) ; 7.7e-11 max off diag
	  (test-ortho q)		; 3.6e-16 max off diag
	  params
	  ))


(make-camera-to-2d-matrix :principal-point  
			  (list -132.88385027608345 -86.66367004255429 (/ -5.420234251376902e-4) )
			  :ASPECT-RATIO 1.0000911272690982 :SKEW 0.017881318674450728)
(make-camera-to-2d-matrix :principal-point
			  (list -132.88385027608345 -86.66367004255429 (/ -5.420234251376902e-4 ))

 
 
(cme::decompose-projection-matrix2 (nth 0 *foo*) 0.0)
(cme::decompose-projection-matrix (nth 0 *foo*))
(cme::copy-matrix-rows (nth 0 *foo*) nil '(0 1 3) '(0 1 2) 3)

;;; this produces results equivalent to (math::decompose-projection-matrix2 (nth 0 *foo*) 0.0)
(multiple-value-bind (r q) (math::rq-decompose (nth 0 *foo*) 2)
  (values r ; good
	  q ; row 3 hax elem is q[3,2] = -5.4e-7 ? is this ok?
	  (setq *pmat* (multiply-matrices r q))
	  (nth 0 *foo*)
	  (math::matrix-subtract (nth 0 *foo*) (multiply-matrices r q)) ; 1e-13 max diff
	  (test-ortho q)		; 1e-17 off diag
	  ))

;;; this produces crummy results
(multiple-value-bind (r q) (math::rq-decompose2 (nth 0 *foo*) 2)
  (values r
	  q
	  (multiply-matrices r q)
	  (nth 0 *foo*)
	  (math::matrix-subtract (nth 0 *foo*) (multiply-matrices r q))  ; bad
	  (multiply-matrices q (transpose-matrix q)) ; not very good
))

(math::rq-decompose (nth 0 *foo*) 2)

;;; old stuff

(setq *po_39940_pan_0000010000-lvcs-to-image-projection*
      (make-composite-coordinate-projection 
       (list (lvcs-to-lat-long-transform (cme::3d-world (cme::2d-world (gui::view-image (gui::selected-view )))))
	     rpc)))


(setq *po_39940_pan_0000010000-2d-world*
      (cme::make-2d-world :name "po_39940_pan_0000010000"
			  :3d-world cme::*san-diego-3d-world*
			  :3d-to-2d-projection *po_39940_pan_0000010000-rpc*))



(BUILD-SURROGATE-PROJECTION po_39940_pan_0000010000-tiled-256 -300.0 1000.0)

(transform-vector (3d-to-2d-projection (gui::top-view)) (cv 0.0 0.0 0.0))


(cme::3d-world (cme::2d-world (gui::view-image (gui::selected-view ))))

(lvcs-to-geocentric-transform (cme::3d-world (cme::2d-world (gui::view-image (gui::selected-view )))))
(lvcs-to-lat-long-transform (cme::3d-world (cme::2d-world (gui::view-image (gui::selected-view )))))

(setq lvcs-to-image-projection
      (make-composite-coordinate-projection 
       (list (lvcs-to-lat-long-transform (cme::3d-world (cme::2d-world (gui::view-image (gui::selected-view )))))
	     rpc)))

(connect-transforms lvcs-to-image-projection
		    (make-Newton-Raphson-Inverse-Transform lvcs-to-image-projection))

(setq alv-projection (3d-to-2d-projection (gui::top-view)))
(math::scale-matrix (projection-matrix (3d-to-2d-projection (gui::top-view)))  (/ 6.47765850533283))

(math::scale-matrix (invert-matrix (projection-matrix (3d-to-2d-projection (gui::top-view))))
		    (/ 25.792289237325804))

(fit-linear-projective-polynomial-to-transform alv-projection (cv -5000.0 5000.0
								  -5000.0 5000.0
								  3000.0 10000.0) '( 9 9 5))

(setq alv-projection2 (fit-4x4-projection-to-3d-to-2d-projection alv-projection
								 (cv -5000.0 5000.0
								     -5000.0 5000.0
								     3000.0 10000.0)
								 '( 9 9 5)))

(setq alv-projection2 (fit-4x4-projection-to-2d-to-3d-projection alv-projection
								 (cv 0.0 1024.0
								     0.0 1024.0
								     3000.0 10000.0)
								 '(5 5 3)))

(fit-linear-projective-polynomial-to-transform alv-projection (cv 0.0 1024.0
								  0.0 1024.0
								  3000.0 10000.0) '(5 5 3))

(make-project-to-world alv-projection)
(funcall (make-project-to-world alv-projection) (cv 0.0 0.0 3000.0))

(make-to-from-vectors-with-uniform-sampling (make-project-to-world alv-projection) (cv 0.0 1024.0
										       0.0 1024.0
										       3000.0 10000.0) '(5 5 3))

(defun project-4x4 (m p)
  (bind-vector-elements (x y z) p
    (let* ((p4 (cv x y z 1.0))
	   (mp (matrix-times-vector m p4)))
      (values (/ (aref mp 0) (aref mp 3))
	      (/ (aref mp 1) (aref mp 3))))))

(project-4x4 (car *foo*) alv-pos)

(print *foo*)
(setq alv-pos (obj::fragment-position (cadr (car (gui::selected-objects)))))
(transform-vector alv-projection alv-pos)
(transform-vector alv-projection2 alv-pos)
(let ((*transform-dammit* t)) (transform-vector alv-projection2 alv-pos))

(describe (obj::fragment-position (cadr (car (gui::selected-objects)))))

(setq lvcs-to-image-projection-surrogate-coeffs
      (fit-linear-projective-polynomial-to-transform lvcs-to-image-projection
						     (cv -5000.0 5000.0
							 -5000.0 5000.0
							 -100.0 500.0)
						     '( 9 9 5)))

(setq lvcs-to-image-projection-surrogate
      (fit-4x4-projection-to-2d-to-3d-projection lvcs-to-image-projection
						     (cv 0.0 8000.0
							 0.0 800.0
							 -100.0 500.0)
						     '( 9 9 5)))

(projection-matrix lvcs-to-image-projection-surrogate)
(transform-vector lvcs-to-image-projection-surrogate (cv 0.0 0.0 0.0))
(transform-vector lvcs-to-image-projection (cv 0.0 0.0 0.0))


(setf (get-prop (inverse-transform lvcs-to-image-projection) :starting-approx)
      (cv 0.0 0.0 0.0))

(describe lvcs-to-image-projection)

(transform-vector lvcs-to-image-projection (cv 0.0 0.0 0.0))
(transform-vector lvcs-to-image-projection (cv 100.0 0.0 0.0))
(transform-vector lvcs-to-image-projection (cv 0.0 100.0 0.0))

(transform-vector lvcs-to-image-projection (cv 12.3 600.7 40.0))
(Newton-Raphson-inverse-transform-vector-starting-approx (inverse-transform lvcs-to-image-projection) nil)
(intersect-camera-ray-with-z-plane lvcs-to-image-projection
				   (cv 4542.954610878397 3736.2293605505024 40.02840609227178)
				   40.0)

(describe (img::property-list (gui::view-image (gui::selected-view ))))
(print (img::property-list (gui::view-image (gui::selected-view ))))

(gui::3d-to-2d-projection (gui::view-image (gui::selected-view )))

(setq *po_39940_pan_0000010000-2d-world*
      (cme::make-2d-world :name "po_39940_pan_0000010000"
			  :3d-world cme::*san-diego-3d-world*
			  :3d-to-2d-projection rpc))

(setf (3d-to-2d-projection *po_39940_pan_0000010000-2d-world*)
      lvcs-to-image-projection)


(cme::setup-image-worlds (gui::view-image (gui::selected-view ))
			 :2d-world *po_39940_pan_0000010000-2d-world*)


(setf (get-prop (gui::view-image (gui::selected-view )) :surrogate-projection)
      )

(3d-to-2d-projection (gui::view-image (gui::selected-view )))

(inverse-transform lvcs-to-image-projection)

(describe rpc)

(describe (offsets rpc))
(describe (scales rpc))

(transform-vector rpc (cv -117.11664956 32.74356650 -133.86838412 ))
(setq *print-array* t)


(aref (nth 3 math::*foo*) 0 0)

(apropos 'nan)
(excl::nan-p (aref (nth 3 math::*foo*) 0 0))
(excl::nan-p 1.0)

(defun math::test-for-nan (array)
  (cond ((= (array-rank array) 2)
	 (loop for i from 0 below (array-dimension array 0)
	       do (loop for j from 0 below (array-dimension array 1)
			for aij = (aref array i j)
			when (or (excl::nan-p aij) (eql aij  #.EXCL::*INFINITY-DOUBLE*))
			  do (format t "array[~a,~a]=~a~%" i j aij))))
	((= (array-rank array) 1)
	 (loop for i from 0 below (array-dimension array 0)
	       for ai = (aref array i)
	       when (or (excl::nan-p ai) (eql ai  #.EXCL::*INFINITY-DOUBLE*))
		 do (format t "array[~a]=~a~%" i ai)))))

(length math::*foo*)
(loop for arr in math::*foo*
      do (math::test-for-nan arr))

|#
