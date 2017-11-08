(in-package :cme)

#|
;;; Random crap for backward compatibility

;;; DO NOT DEFINE METHODS FOR THIS CLASS
;;; Is this really used?
;;; This makes it possible to load old data files
;;;     (radius/ft-hood-feature-sets-sept-15.feats)
;;;(defstruct-class basic-4x4-transform (4x4-coordinate-transform) ())

;;; Most of ft-benning-2 fails if we use the
;;; MAKE-CAMERA-TO-IMAGE-MATRIX option, so some incompatibility still
;;; exists here.

|#



(defmethod find-transform-to (from to)
  (transforms::canonical-coordinate-transform-path from to))
	  

(defun decompose-camera-to-2d-matrix-old 
    (mat &optional 
     (1/f-threshold transforms::*decompose-projection-matrix-1/f-threshold*))
  (let ((plist (transforms::decompose-camera-to-2d-matrix mat 1/f-threshold)))
    (destructuring-bind (ppu ppv &optional fl) (getf plist :principal-point)
      `(:principal-point-u ,ppu
	:principal-point-v ,ppv
	:1/f ,(if fl (/ fl) 0.0)
	:r/f ,(getf plist :gsd)
	:v-axis-aspect-ratio ,(getf plist :aspect-ratio)
	:v-axis-skew ,(getf plist :skew)))))

;;;
;;; This fails when loading ft-benning-2:
;;;
(defun decompose-projection-matrix-old
    (3d-to-2d-matrix
     &key (1/f-threshold transforms::*decompose-projection-matrix-1/f-threshold*))
  (setq GUI::*last-matrix* 3d-to-2d-matrix)
  (mv-bind (3d-to-camera-matrix camera-to-2d-matrix )
      (decompose-projection-matrix 3d-to-2d-matrix)
    (values 3d-to-camera-matrix
	    (decompose-camera-to-2d-matrix-old camera-to-2d-matrix 1/f-threshold)
	    camera-to-2d-matrix)))


;;; This is broken for orthographic projections
;;; returns coordinate-vector of (omega phi kappa xc yc zc ppu ppv fl v-axis-scale v-axis-skew)
(defun 4x4-projection-parameters (projection)
  (mv-bind (3d-to-camera-matrix plist)
      (decompose-projection-matrix-old (transforms::projection-matrix  projection))
    (let ((camera-to-3d-mat (invert-matrix 3d-to-camera-matrix)))
      (multiple-value-bind (omega phi kappa)
	  (math::decompose-omega-phi-kappa-rotation-matrix 3d-to-camera-matrix)
  	(cv omega phi kappa
	    (aref camera-to-3d-mat 0 3) (aref camera-to-3d-mat 1 3) (aref camera-to-3d-mat 2 3)
	    (getf plist :principal-point-u) (getf plist :principal-point-v) (/ (getf plist :1/f))
	    (or (getf plist :v-axis-scale) 1.0)
	    (or (getf plist :v-axis-skew) 0.0))))))


#|

(4x4-projection-parameters (3d-to-2d-projection (top-view)))
(camera-position (3d-to-2d-projection (top-view)))

(mv-list (decompose-projection-matrix-old
		      (transforms::projection-matrix
		       (3d-to-2d-projection (top-view)))))
(transforms::projection-matrix (3d-to-2d-projection (top-view)))

(math::invert-matrix (decompose-projection-matrix-old
		      (transforms::projection-matrix
		       (3d-to-2d-projection (top-view)))))
(gui::selected-point)
(mapcar #'degrees (mv-list
		   (math::decompose-omega-phi-kappa-rotation-matrix
		    (math::invert-matrix (decompose-projection-matrix-old
					  (transforms::projection-matrix
					   (3d-to-2d-projection (top-view))))))))

(degrees 1.46e-4) = .008

(progn orthographic-projection)
(mv-list (decompose-projection-matrix-old (projection-matrix  orthographic-projection)))
(let ((*decompose-projection-matrix-1/f-threshold* 0.0))
  (mv-list (decompose-projection-matrix-old (projection-matrix  orthographic-projection))))

(fasd-form (3d-to-2d-projection (top-view)))

 
(setq alv-2-44-projection-original
      (MAKE-PERSPECTIVE-TRANSFORM
       :focal-length -5067.3154
       :principal-point-u 1665.6791
       :principal-point-v 1986.6626
       :transform-matrix
       (cme::make-and-fill-2d-array
	'((0.9996994 0.013477996 -0.021072589 1812.722)
	  (-0.014055679 0.9992905 -0.03458094 2137.357)
	  (0.020597093 0.03486902 0.9991798 21716.98)
	  (0.0 0.0 0.0 1.0)))
       ))

(setq alv-2-44-projection-orthographic
      (MAKE-PERSPECTIVE-TRANSFORM
       :1/f 0.0 :r/f -3.1
       :near -1000.0 :far -10000.0
       :transform-matrix
       (cme::make-and-fill-2d-array
	'((0.9996994 0.013477996 -0.021072589 1812.722)
	  (-0.014055679 0.9992905 -0.03458094 2137.357)
	  (0.020597093 0.03486902 0.9991798 21716.98)
	  (0.0 0.0 0.0 1.0)))
       ))

(setq pmat (transforms::projection-matrix
	    (MAKE-PERSPECTIVE-TRANSFORM
	     :camera-to-world-matrix
	     (make-and-fill-2d-array
	      '((-0.54196656401916 -0.8404000496699567 5.061403216800092E-16 352.13731837585766) (-0.8404000496699351 0.5419665640191468 -2.3444804828336106E-15 5958.300710086118) (-1.1545493078743254E-17 7.002817966028024E-18 0.9999999999999998 500.0000221550892) (0.0 0.0 0.0 1.0)))
	     :1/F -4.359064112275498E-4 :R/F 0.0
	     :PRINCIPAL-POINT-U -57.24257300000216
	     :PRINCIPAL-POINT-V 4954.488300000001
	     :V-AXIS-ASPECT-RATIO 1.6348437979929582
	     :V-AXIS-SKEW 1.3796195621128342)))


(setq pmat (transforms::projection-matrix
	    (MAKE-PERSPECTIVE-TRANSFORM
	     :camera-to-world-matrix
	     (make-and-fill-2d-array
	      '((-0.9902294420064423 0.015442823664995486 -0.13858993968128375 -57.242572999999304) (-0.1367179657997017 0.08816820540563399 0.9866785522059054 4954.488300000001) (0.027456329164722685 0.9959858868241064 -0.08519544140384866 6054.9927) (0.0 0.0 0.0 1.0)))
	      
	     :1/F -0.001999999826537814 :R/F 0.0 :PRINCIPAL-POINT-U 190.99998700762265 :PRINCIPAL-POINT-V 113.00000295803383)))

(setq pmat (transforms::projection-matrix
	    (MAKE-PERSPECTIVE-TRANSFORM
	     :camera-to-world-matrix
	     (make-and-fill-2d-array
	      '((-0.54196656401916 -0.8404000496699567 5.061403216800092E-16 352.13731837585766) (-0.8404000496699351 0.5419665640191468 -2.3444804828336106E-15 5958.300710086118) (-1.1545493078743254E-17 7.002817966028024E-18 0.9999999999999998 500.0000221550892) (0.0 0.0 0.0 1.0)))
	     :1/F -4.359064112275498E-4 :R/F 0.0 :PRINCIPAL-POINT-U -57.24257300000216 :PRINCIPAL-POINT-V 4954.488300000001 :V-AXIS-ASPECT-RATIO 1.6348437979929582 :V-AXIS-SKEW (degrees 1.3796195621128342))))

(decompose-projection-matrix pmat) ; looks fine


(transforms::projection-matrix *)
(transforms::projection-matrix alv-2-44-projection-orthographic)

(transform-vector alv-2-44-projection-orthographic
		  (CV 2478.3429793238957 3229.6669742865415 -9844.16500200493))

(transform-vector (inverse-transform alv-2-44-projection-orthographic)
		  (cv 0.0 0.0 1.0))
(CV 2023.4478900000002 2483.1663999999996 11725.181999999999)
(transform-vector alv-2-44-projection-orthographic
		  (CV 2023.4478900000002 2483.1663999999996 11725.181999999999) )
(inverse-transform alv-2-44-projection-orthographic)

(multiply-matrices (transforms::projection-matrix alv-2-44-projection-orthographic)
		   (transforms::projection-matrix (inverse-transform alv-2-44-projection-orthographic)))

(multiply-matrices (transforms::projection-matrix alv-2-44-projection-orthographic)
		   (invert-matrix (transforms::projection-matrix alv-2-44-projection-orthographic)))


(cadr alv-2-44-projection-original)
(mv-bind (omega phi kappa)
    (math::decompose-omega-phi-kappa-rotation-matrix
     (math::invert-matrix (cadr alv-2-44-projection-original)))
  (make-coord-frame-matrix 1812.722 2137.357 21716.98
			   :omega omega :phi phi :kappa kappa))

(setq proj
      (mv-bind (omega phi kappa)
	  (math::decompose-omega-phi-kappa-rotation-matrix
	   (math::invert-matrix (cadr alv-2-44-projection-original)))
	(setq orthographic-projection
	      (MAKE-PERSPECTIVE-TRANSFORM
	       :1/f -1e-6 :r/f -3.1
	       :transform-matrix
	       (make-coord-frame-matrix 1812.722 2137.357 21716.98
					:omega omega :phi phi :kappa kappa)
	       :v-axis-aspect-ratio 1.5 :v-axis-skew .123
	       ))))

(decompose-projection-matrix-old (transforms::projection-matrix proj)
			     ;;:1/f-threshold 1e-20
			     )
 

;;; i0 is a window of alv-2-44
(setq i1 (ic::copy-image i0))
(progn (setf (image-prop i1 :image-to-2d-transform) (ic::copy (image-to-2d-transform i0)))
       (setf (3d-to-2d-projection (2d-world i1)) alv-2-44-projection-original)
       (setf (slot-value (2d-world i1) '3d-world) (3d-world i0))
       )

(setf (3d-to-2d-projection (2d-world alv-2-44)) alv-2-44-projection-original)

;;; This make a 4x3 matrix from rows 0,1,3 of a 4x4=projection matrix
;;; Used for testing decompose-projection-matrix
(defun make-3x4-projection-matrix (m)
  (let ((m2 (make-array '(3 4) :element-type (array-element-type m))))
    (loop for i from 0 below 3
	  for i2 = (if (= i 2) 3 i)
	  do (loop for j from 0 below 4
		   do (setf (aref m2 i j) (aref m i2 j))))
    m2))

(3d-to-2d-projection (top-view))
(projection-matrix (3d-to-2d-projection (top-view)))
(mv-list (decompose-projection-matrix proj))
(mv-list (decompose-projection-matrix (projection-matrix (3d-to-2d-projection (top-view)))))
(mv-list (decompose-projection-matrix
		      (scale-matrix 
		      (make-3x4-projection-matrix (projection-matrix (3d-to-2d-projection (top-view))))
		      1234566.0e-20)))

 
(transform-vector othro-proj (selected-point cube))
;;(-713.7864777473128 -933.8718487957079 -6834.711825714972)
;; ppu = 1666 ppv = 1987

(transform-vector (3d-to-2d-projection (top-view)) (selected-point cube))
;;(1136.4769755183022 1294.2889004531128 -5067.3154)

(transform-vector alv-2-44-projection-orthographic (selected-point cube))
(transform-vector othro-proj (selected-point cube))

|#






#| Mon Sep  6 2004

(transform-projection-matrix-to-camera-centered-lvcs alv-2-44-projection)

(transform-projection-matrix-to-camera-centered-lvcs
 (3d-to-2d-projection (top-view)) 
 :decomp-fn 'math::decompose-azimuth-tilt-swing-rotation-matrix)

;;; alv-oblique-tower
(172.00450012656668 94.87377601585187 -178.42267073100098)

(:|1/F| -0.0019999999113796464 :R/F 0.0 :PRINCIPAL-POINT-U 190.9999870076229
 :PRINCIPAL-POINT-V 113.00000295803387 :V-AXIS-ASPECT-RATIO 1.0000000168753318
 :V-AXIS-SKEW 3.4462503603565476e-8)

#2A((-0.9372868125714783 -0.5136454727879479 0.059881644280194
     -5.452420737128705e-10)
    (0.04676722641896421 -0.13506197967821618 1.0152079196286994
     -9.840732673183084e-10)
    (2.771847758471987e-4 -0.001973396775963782 1.6992177662937687e-4
     -0.001000000004760082)
    (2.771847758471987e-4 -0.001973396775963782 1.6992177662937687e-4
     -4.760636329592671e-12))


(inverse-transform (transforms::gcc-to-gdc-transform
 (transforms::ellipsoid (to-coordinate-system (lvcs-to-lat-long-transform (from-coordinate-system alv-2-44-projection))))))

(setq alv-2-44-projection (3d-to-2d-projection (get-2d-world-named "alv-2-44")))

(mapcar #'degrees (mv-list
		   (math::decompose-omega-phi-kappa-rotation-matrix
		     (math::invert-matrix (decompose-projection-matrix
					   (transforms::projection-matrix alv-2-44-projection))))))
(-1.998524969960044 1.1801977455201356 0.8055201643433042)

(decompose-projection-matrix
 (transforms::projection-matrix alv-2-44-projection))
#2A((0.9996890569143531 -0.014055533577194313 0.020596879898444866
     -2229.418682364879)
    (0.013331744653748345 0.9993030575429711 0.03486637018326328
     -2917.22639593563)
    (-0.021072590495186887 -0.034580936383063206 0.9991797159513858
     -21587.055352889416)
    (0.0 0.0 0.0 1.0))

(:|1/F| -1.9734313674786607e-4 :R/F 0.0 :PRINCIPAL-POINT-U 1665.6791166043904
 :PRINCIPAL-POINT-V 1986.6625966352044 :V-AXIS-ASPECT-RATIO 1.0000105174483886
 :V-AXIS-SKEW -0.008390220651967912)

#2A((0.9999896538042876 -1.4643697482230475e-4 -0.32871034168612495 0.0)
    (0.0 1.000010506726382 -0.3920542284796518 0.0)
    (0.0 0.0 0.0 -1.0)
    (0.0 0.0 -1.9734313674786607e-4 0.0))

(transform-vector (lvcs-to-lat-long-transform (from-coordinate-system alv-2-44-projection))
		  (cv 1812.722 2137.357 21716.98))
#(-105.11441597675547 39.50586158474837 6619.4059412861925)

(setq alv-2-44-lvcs
      (make-lvcs 'nad27 39.50586158474837 -105.11441597675547 :local-units-per-meter *feet-per-meter*))

(obj::3d-object-to-world-transform (gui::selected-object) alv-2-44-lvcs)

(multiply-matrices 
 (transforms::transform-matrix (lvcs-to-geocentric-transform (from-coordinate-system alv-2-44-projection)))
 (invert-matrix (transform-matrix alv-2-44-lvcs-to-geocentric-transform)))

(setq alv-2-44-lvcs2-projection-matrix
      (multiply-matrices (transforms::projection-matrix alv-2-44-projection)
			 (multiply-matrices
			  (invert-matrix (transforms::transform-matrix (lvcs-to-geocentric-transform (from-coordinate-system alv-2-44-projection))))
			  (transform-matrix alv-2-44-lvcs-to-geocentric-transform))))


(mapcar #'degrees (mv-list
		   (math::decompose-omega-phi-kappa-rotation-matrix
		     (math::invert-matrix (decompose-projection-matrix alv-2-44-lvcs2-projection-matrix)))))
(-2.004457029560062 1.185066214677839 0.8097247922823747)

(decompose-projection-matrix alv-2-44-lvcs2-projection-matrix)

#2A((0.9996862691949913 -0.014128870538497432 0.020681832614762422
     -2.279294118958121e-9)
    (0.013399924895999426 0.9992985330016423 0.0349697576993003
     -1.0572502312252537e-9)
    (-0.021161408171013512 -0.034681651605316985 0.9991743480725206
     -3.195483199361418e-9)
    (0.0 0.0 0.0 1.0))

(:|1/F| -1.9734313674786607e-4 :R/F 0.0 :PRINCIPAL-POINT-U 1665.6791166043897
 :PRINCIPAL-POINT-V 1986.6625966352037 :V-AXIS-ASPECT-RATIO 1.0000105174483884
 :V-AXIS-SKEW -0.008390220651968474)

|#


(defun  xy-rotation-normalization (matrix)
  (sqrt (abs (- (* (aref matrix 0 0) (aref matrix 1 1))
		(* (aref matrix 0 1) (aref matrix 1 0))))))

(defun normalize-x-y-rotation (inv-focal-length matrix
					       &optional (intermediate-type 0.0) (result-type 0.0))
  (ignore intermediate-type result-type)
  (let* ((l (xy-rotation-normalization matrix))
	 (k (/ (if (<= inv-focal-length 0.0) 1.0 -1.0) l)))
    
    (setf (aref matrix 0 0) (* k (aref matrix 0 0))
	  (aref matrix 0 1) (* k (aref matrix 0 1))
	  (aref matrix 1 0) (* k (aref matrix 1 0))
	  (aref matrix 1 1) (* k (aref matrix 1 1)))
    (and inv-focal-length (/ inv-focal-length k))
    ))

(defun orthonormal-p (mat &optional (thresh 1e-12))
  (declare (type (simple-array double-float (* *)) mat))
  (let ((error (loop for row fixnum from 0 below 3
		     for len double-float =  (loop for col fixnum from 0 below 3
						   for elt double-float = (aref mat row col)
						   sum (* elt elt) double-float)
		     maximize (- len 1.0))))
    (values (< error thresh)
	    error)))

#|
(orthonormal-p (cme::make-and-fill-2d-array
		'((0.9996994 0.013477996 -0.021072589 1812.722)
		  (-0.014055679 0.9992905 -0.03458094 2137.357)
		  (0.020597093 0.03486902 0.9991798 21716.98)
		  (0.0 0.0 0.0 1.0)))
	       )

(orthonormal-p (make-coord-frame-matrix 10.0 20.0 30.0 :omega 2.0 :phi 1.0 :kappa .5))
|#


(defun normalize-camera-to-2d-matrix (matrix focal-length)
  (let* ((k0 (math::euclidean-length (aref matrix 0 0) (aref matrix 0 1)))
	 (k (if (< focal-length 0.0) 
		(/ k0)
		(/ -1.0 k0))))
    (setf (aref matrix 0 0) (* k (aref matrix 0 0))
	  (aref matrix 0 1) (* k (aref matrix 0 1))
	  (aref matrix 1 0) (* k (aref matrix 1 0))
	  (aref matrix 1 1) (* k (aref matrix 1 1)))
    (/ focal-length k)))

;;; This provides an interface from old CME arglists to
;;; new arglists.

;;; FOR ORTHOGRAPHIC PROJECTIONS:
;;;  The last column of the camera-to-3d-matrix specifies the lvcs position (usually on
;;;  the terrain surface) on the principal ray of the camera.
;;;  specify 1/f = 0,
;;;          r/f = GSD at the lvcs position in the camera-to-3d-matrix (last column).

;;; Communication variable for loading camera models from site-glue.
(defvar *make-perspective-transform-default-3d-world* nil)
(defvar *make-perspective-transform-default-2d-world* nil)


(defun invert-euclidean-xform-matrix (m)
  ;; Decompose M into x' = TRx = Rx + t.  x' - t = Rx, and x = R^T
  ;; (x'- t) = R^Tx' - R^Tt.  Procedurally, remove and negate the
  ;; translation t, transpose R, and then add the new t back in after
  ;; premultiplying by R.
  (let ((trans (math::cv (- (aref m 0 3)) (- (aref m 1 3)) (- (aref m 2 3)) 1.0))
	(into (math::matrix-transpose m)))
    (setf (aref into 3 0) 0.0)
    (setf (aref into 3 1) 0.0)
    (setf (aref into 3 2) 0.0)
    (setf (aref into 0 3) (+ (* (aref into 0 0) (aref trans 0))
			     (* (aref into 0 1) (aref trans 1))
			     (* (aref into 0 2) (aref trans 2))))
    (setf (aref into 1 3) (+ (* (aref into 1 0) (aref trans 0))
			     (* (aref into 1 1) (aref trans 1))
			     (* (aref into 1 2) (aref trans 2))))
    (setf (aref into 2 3) (+ (* (aref into 2 0) (aref trans 0))
			     (* (aref into 2 1) (aref trans 1))
			     (* (aref into 2 2) (aref trans 2))))
    into))



;;; camera-to-world-matrix is really camera-to-3d-matrix
;;; camera-to-image-matrix is really camera-to-2d-matrix
;;; projection-matrix is 3d-to-2d-projection-matrix
(defun make-perspective-transform
    (&rest args &key
     projection-matrix		 ; full 4x4 projection - 4th row must be 0 0 0 1
     transform-matrix			; this is same as camera-to-3d-matrix
     camera-to-world-matrix
     camera-to-3d-matrix  
     position			  ; alternative specification of camera position
     rotations		       ; alternative specification of camera orientation
     focal-length
     inv-focal-length			; allowed for backward compatibility
     (1/f (or inv-focal-length (and focal-length (/ focal-length)) 0.0))
     (r/f 0.0) ; this is the GSD at the lvcs position in the camera-to-3d-matrix (last column).
     camera-to-image-matrix  
     ;; 2d transform encoding rotation, scale and principal point
     camera-to-2d-matrix 
     principal-point-u
     principal-point-v
     v-axis-aspect-ratio v-axis-skew
     near far
     (to-coordinate-system *make-perspective-transform-default-2d-world*) ; 3d-world
     (from-coordinate-system (or *make-perspective-transform-default-3d-world* 
				 (and to-coordinate-system (parent to-coordinate-system ))))
     &allow-other-keys )
;;  (ignore near far)
  
  (let (internal-params 3d-to-camera-matrix)

    ;; map old arg names to new 
    (setq camera-to-3d-matrix
	  (or camera-to-3d-matrix transform-matrix camera-to-world-matrix))
    (setq camera-to-2d-matrix
	  (or camera-to-2d-matrix camera-to-image-matrix))

    (when (and focal-length camera-to-2d-matrix)
      ;; normalize upper 2x2 submatrix
      (setq focal-length (normalize-camera-to-2d-matrix camera-to-2d-matrix focal-length)))

    (when camera-to-3d-matrix
      (setq 3d-to-camera-matrix
	    (math::invert-matrix camera-to-3d-matrix)
	    ))
  
    (unless (or projection-matrix camera-to-3d-matrix)
      (let ((x 0.0) (y 0.0) (z 0.0))
	(bind-vector-elements (xc yc zc) position
	  (setq x xc y yc z zc))
	(setq camera-to-3d-matrix
	      (apply 'make-coord-frame-matrix x y z rotations))))

    ;; either camera-to-3d-matrix or projection-matrix must be defined now.

    (unless principal-point-u (setq principal-point-u 0.0))
    (unless principal-point-v (setq principal-point-v 0.0))

    (when (and camera-to-2d-matrix (not projection-matrix))
      (setq projection-matrix
	    (math::multiply-matrices
	     camera-to-2d-matrix
	     3d-to-camera-matrix)))
	
    ;; If camera-to-2d-matrix was supplied, projection-matrix is now defined.
    
    (when projection-matrix
      (mv-setq (3d-to-camera-matrix
                internal-params
                ;;camera-to-2d-matrix
                )
               (decompose-projection-matrix-old projection-matrix))
      (macrolet ((set-arg (var key default)
		   `(setf ,var
		     (or (getf internal-params ,key) ,default))))
	(set-arg principal-point-u :principal-point-u 0.0)
	(set-arg principal-point-v :principal-point-v 0.0)
	(set-arg 1/f :1/f 0.0)
	(set-arg r/f :r/f 0.0)
	(set-arg v-axis-aspect-ratio :v-axis-aspect-ratio 1.0)
	(set-arg v-axis-skew :v-axis-skew 0.0))
      )

    (when v-axis-aspect-ratio
      (setq args (list* :v-axis-aspect-ratio v-axis-aspect-ratio args)))
    (when v-axis-skew
      (setq args (list* :v-axis-skew v-axis-skew args)))
    (mv-bind (3d-to-camera-matrix camera-to-2d-matrix)
	(apply 'make-gl-camera-matrices
	       :3d-to-camera-matrix 3d-to-camera-matrix
	       :1/f 1/f
	       :r/f (or r/f 0.0)
	       :principal-point-u principal-point-u
	       :principal-point-v principal-point-v
	       :3d-world from-coordinate-system
	       args ; pass various keyword parameters thru
	       )
      (let ((projection (make-frame-camera-from-matrices
			 3d-to-camera-matrix camera-to-2d-matrix)))
	(when (or from-coordinate-system to-coordinate-system)
	  (set-transform-coordinate-systems projection from-coordinate-system to-coordinate-system))
	(when near
	  (setf (get-prop projection :near-far) (list near far)))
	
	projection))))

#|
(trace cme::guess-gl-near-far)
(cme::load-camera-model (cme::2d-world (cme::top-view t))
			"/opt/IU/radius/site-2d-worlds/ft-hood-2/fhn719/camera-model")
(transforms::projection-matrix (cme::3d-to-2d-projection (cme::top-view t)))

(terrain-bbox (3d-world (top-view)))
|#

;(defparameter *default-near-far* '(1.0 nil))
(defparameter *default-near-far* nil)

;; ugly hack, totally sucks
;;;(defun guess-gl-near-far (camera-to-world-matrix 1/f &optional 3d-world)
;;;  ;;(setq foo camera-to-world-matrix) (break)
;;;  (let ((units/meter (or (and 3d-world (get-prop 3d-world :units-per-meter)) 1.0)))
;;;    (if *default-near-far*
;;;        (values-list *default-near-far*)
;;;        (let* ((cam-z (/ (aref camera-to-world-matrix 2 3) units/meter)))
;;;          ;(setq *guess-gl-near-far* (list camera-to-world-matrix cam-z))
;;;          (* units/meter
;;;             (cond ((> cam-z 4000.0)    ; assuming meters
;;;                    (values (- cam-z 3000.0) ; assume high alt image
;;;                            (and (zerop 1/f) (+ cam-z 3000.0)))
;;;                    )
;;;                   ((> cam-z 2000.0)    ; assuming meters
;;;                    (values 300.0       ; assume medium alt image
;;;                            (and (zerop 1/f) 600.0))
;;;                    )
;;;                   ((> cam-z 1000.0)    ; assuming meters
;;;                    (values 150.0       ; assume medium alt image
;;;                            (and (zerop 1/f) 300.0))
;;;                    )
;;;                   (t (values 2.0       ; assume ground level image
;;;                              (and (zerop 1/f) 1000.0)))
;;;                   ))))))

(defun site-bbox (3d-world)
  (get-prop 3d-world :site-bbox))

(defun terrain-bbox (3d-world)
  (get-prop 3d-world :terrain-bbox))

;; less ugly hack, only partially sucks
(defun guess-gl-near-far (camera-to-world-matrix 1/f &optional 3d-world)
  (let ((bbox (and 3d-world (or (site-bbox 3d-world) (terrain-bbox 3d-world)))))
    (if bbox
	(let ((m22 (aref camera-to-world-matrix 2 2))
	      (zcam (aref camera-to-world-matrix 2 3))
	      (zmin (aref bbox 4))
	      (zmax (aref bbox 5)))
	  ;; This is only valid for near nadir views.
	  (values (* .5 (/ (- zcam zmax) m22)) 
		  (* 2.0 (/ (- zcam zmin) m22))))
    	
	(if *default-near-far*
	    (values-list *default-near-far*)
	    (let* ((units/meter (or (and 3d-world (get-prop 3d-world :units-per-meter)) 1.0))
		   (cam-z (/ (aref camera-to-world-matrix 2 3) units/meter)))
					;(setq *guess-gl-near-far* (list camera-to-world-matrix cam-z))
	      (cond ((> cam-z 4000.0)	; assuming meters
		     (values (- cam-z 3000.0) ; assume high alt image
			     (and (zerop 1/f) (+ cam-z 3000.0)))
		     )
		    ((> cam-z 2000.0)	; assuming meters
		     (values 300.0	; assume medium alt image
			     (and (zerop 1/f) 600.0))
		     )
		    (t (values 2.0	; assume ground level image
			       (and (zerop 1/f) 1000.0)))
		    ))))))
#| 

(let ((proj (3d-to-2d-projection (top-view))))
  (guess-gl-near-far (invert-matrix (transforms::3d-to-camera-matrix proj))
		     (transforms::1/f proj)
		     (from-coordinate-system proj)))

(terrain-bbox (3d-world (top-view)))
(transforms::extract-near-far (3d-to-2d-projection (top-view)))

(defun terrain-bbox-near-far (camera-to-world-matrix bbox)
  (let* ((inverse-transform (make-4x4-coordinate-transform (invert-matrix camera-to-world-matrix)
							  :create-inverse nil))
	 (cam-rel-bbox (obj::transform-bounding-box inverse-transform bbox))
	 )
    (values (aref cam-rel-bbox 4) (aref cam-rel-bbox 5))))



|#

;;; This is the canonical interface for building OpenGL compatible projections.
;;; camera-to-world-matrix must be a 4x4 matrix with orthonormal upper-left 3x3,
;;; and 4th row = <0 0 0 1>
;;;
;;; At least one of 1/f and r/f must be non-zero.
;;; principal-point-u and principal-point-v default to 0.0
;;; near should be specified
;;; far defaults to infinity.
;;; v-axis-aspect-ratio defaults to 1.0
;;; v-axis-skew defaults to 0.0
(defun make-gl-camera-matrices (&key camera-to-world-matrix
				3d-to-camera-matrix
				(1/f 0.0) (r/f 0.0)
				principal-point-u principal-point-v
				near far
				v-axis-aspect-ratio v-axis-skew
				3d-world
				&allow-other-keys)
  
  (when (and camera-to-world-matrix (not 3d-to-camera-matrix))
    (setq 3d-to-camera-matrix
	  (math::invert-homogeneous-4x4-matrix camera-to-world-matrix)))
  (unless near
    ;; Invent some values for near and far.  
    ;; When rendering, GL_DEPTH_TEST will be disabled by SET-DEFAULT-GRAPHICS-ATTRIBUTES.
    (mv-setq (near far)
      (guess-gl-near-far (or camera-to-world-matrix
			     (math::invert-homogeneous-4x4-matrix 3d-to-camera-matrix))
			 1/f
			 3d-world)))
  ;;(break)
  (let ((focal-length (if (zerop 1/f) nil (/ 1/f))))
    (values 3d-to-camera-matrix
	    (make-camera-to-2d-matrix
	     :principal-point (list principal-point-u principal-point-v focal-length)
	     ;; missing gsd arg caused singular camera-to-2d-matrix in orthographic views:
	     :r/f r/f :gsd r/f :near-far (list near far)
	     :aspect-ratio v-axis-aspect-ratio
	     :skew v-axis-skew))))

;;; omega phi kappa in degrees
;;;(defun make-central-projection-camera (&key xc yc zc
;;;                                            omega phi kappa
;;;                                            focal-length
;;;                                            (1/f (if focal-length (/ focal-length) 0.0))
;;;                                            principal-point-u principal-point-v
;;;                                            camera-to-world-matrix
;;;                                            camera-to-image-matrix
;;;                                            r/f positive-w-clip-plane
;;;                                            v-axis-aspect-ratio 
;;;                                            v-axis-skew 
;;;                                            property-list)
;;;  (make-perspective-transform :transform-class '4x4-cordinate-projection
;;;                              :focal-length focal-length
;;;                              :1/f 1/f
;;;                              :principal-point-u principal-point-u
;;;                              :principal-point-v principal-point-v
;;;                              :v-axis-aspect-ratio v-axis-aspect-ratio
;;;                              :v-axis-skew v-axis-skew
;;;                              :camera-to-world-matrix
;;;                              (or CAMERA-TO-WORLD-MATRIX
;;;                                  (MAKE-COORD-FRAME-MATRIX xc yc zc
;;;                                                           :KAPPA-DEGREES (- kappa)
;;;                                                           :PHI-DEGREES (- phi)
;;;                                                           :OMEGA-DEGREES (- omega)
;;;                                                           ))
;;;                              :camera-to-image-matrix camera-to-image-matrix
;;;                              :r/f r/f
;;;                              :positive-w-clip-plane (or positive-w-clip-plane 0.0)
;;;                              :property-list property-list
;;;                              ))

;;; omega phi kappa in degrees
(defun make-central-projection-camera (&key xc yc zc
					    (omega 0.0) (phi 0.0) (kappa 0.0)
					    focal-length
					    (1/f (if focal-length (/ focal-length) 0.0))
					    (principal-point-u 0.0) (principal-point-v 0.0)
					    camera-to-world-matrix
					    camera-to-image-matrix
					    r/f positive-w-clip-plane
					    v-axis-aspect-ratio 
					    v-axis-skew 
				            near far
					    property-list)
  (make-perspective-transform :transform-class '4x4-cordinate-projection
			      :focal-length focal-length
			      :1/f 1/f
			      :principal-point-u principal-point-u
			      :principal-point-v principal-point-v
			      :v-axis-aspect-ratio v-axis-aspect-ratio
			      :v-axis-skew v-axis-skew
			      :camera-to-world-matrix
			      (or CAMERA-TO-WORLD-MATRIX
				  (MAKE-COORD-FRAME-MATRIX xc yc zc
							   :OMEGA-DEGREES omega
							   :PHI-DEGREES phi
							   :KAPPA-DEGREES kappa
							   ))
			      :camera-to-image-matrix camera-to-image-matrix
			      :near near :far far
			      :r/f r/f
			      :positive-w-clip-plane (or positive-w-clip-plane 0.0)
			      :property-list property-list
			      ))


(defun make-orthographic-projection (&key xc yc zc
				     (omega 0.0) (phi 0.0) (kappa 0.0)
				     (principal-point-u 0.0) (principal-point-v 0.0)
				     camera-to-world-matrix
				     camera-to-image-matrix
				     gsd positive-w-clip-plane
				     v-axis-aspect-ratio 
				     v-axis-skew 
				     flip-image-y
				     near far
				     property-list)
  (let* ((focal-length nil)
	 (1/f  0.0)
	 (xf   (make-perspective-transform :transform-class '4x4-cordinate-projection
					   :focal-length focal-length
					   :1/f 0.0
					   :principal-point-u principal-point-u
					   :principal-point-v principal-point-v
					   :v-axis-aspect-ratio v-axis-aspect-ratio
					   :v-axis-skew v-axis-skew
					   :camera-to-world-matrix
					   (or CAMERA-TO-WORLD-MATRIX
					       (MAKE-COORD-FRAME-MATRIX xc yc zc
									:OMEGA-DEGREES omega
									:PHI-DEGREES phi
									:KAPPA-DEGREES kappa
									))
					   :camera-to-image-matrix camera-to-image-matrix
					   :near near :far far
					   :r/f gsd
					   :positive-w-clip-plane (or positive-w-clip-plane 0.0)
					   :property-list property-list
					   )))
    (when flip-image-y
      (let ((im (transforms::camera-to-2d-matrix  xf)))
	(loop for i from 0 below 3
	      do (setf (aref im 1 i) (- (aref im 1 i))))))
;;    (update-transform xf)
    xf))


			   
;;;
;;; This is exactly the same as the CME version...
;;;
(defun make-camera-to-image-matrix (a b c d e f)
  (make-and-fill-4x4-matrix a b 0.0 c
			    d e 0.0 f
			    0.0 0.0 1.0 0.0
			    0.0 0.0 0.0 1.0))



(defun make-camera-to-image-matrix (a b c d e f)
  (make-and-fill-4x4-matrix a b  0.0 c
			    d e  0.0 f
			    0.0 0.0 0.0 1.0
			    0.0 0.0 1.0 0.0))

;;;
;;; I can only assume that this version is meant to arrange the matrix
;;; to "do the right thing" for the q-r decomposition used for old CME
;;; camera models...
#+broken?
(defun make-camera-to-image-matrix (a b c d e f)
  (make-and-fill-4x4-matrix a b c 0.0
			    d e f 0.0
			    0.0 0.0 1.0 0.0
			    0.0 0.0 0.0 1.0))



(defvar *synthetic-2d-world-count* 0)

;;; CC code
;;; Create synthetic view of a 3d-world. 
;;;     Not sure that this code belongs in this file.
;;;
;;; Not quite right.  The bounding box projection does not seem to get
;;; the whole site in view within the window.  Switched to
;;; orthographic projection to avoid distortion from incorrect
;;; principal point.  Needs more work:
;;;
;;;(defun push-synthetic-view (world window)
;;;  (let* ((bbox (obj::bounding-box world))
;;;         (frame-camera (cme::make-central-projection-camera
;;;                        ;; :1/F -1.0e-04 :R/F 0.0
;;;                        :1/F 0.0 :R/F -1.0
;;;                        :omega 0.0 :phi 0.0 :kappa 0.0
;;;                        :xc (* 0.5 (+ (aref bbox 0) (aref bbox 1)))
;;;                        :yc (* 0.5 (+ (aref bbox 2) (aref bbox 3)))
;;;                        :principal-point-u (* 0.5 (+ (aref bbox 0) (aref bbox 1)))
;;;                        :principal-point-v (* 0.5 (+ (aref bbox 2) (aref bbox 3)))
;;;                        :zc 10000.0)))
;;;    (incf *synthetic-2d-world-count*)
;;;    (let ((2d-world (make-instance '2d-world
;;;                                   :name (format nil "Synthetic 2D World ~d"
;;;                                                 *synthetic-2d-world-count*)
;;;                                   :3d-world world))
;;;          (pbbox (obj::transform-bounding-box frame-camera bbox)))
;;;      (setf (3d-to-2d-projection 2d-world) frame-camera)
;;;      (let ((view (make-instance 'gui::synthetic-view
;;;                                 :2d-world 2d-world
;;;                                 :object-sets (default-object-sets world)
;;;                                 :3d-world world
;;;                                 :window window))
;;;            (scale (min (/ (window-width window) (- (aref pbbox 1) (aref pbbox 0)))
;;;                        (/ (window-height window) (- (aref pbbox 3) (aref pbbox 2))))))
;;;        (push-view view window)
;;;        (let ((m (transform-matrix (2d-to-window-transform view ))))
;;;          (setf (aref m 0 0) scale)
;;;          (setf (aref m 1 1) (- scale)))
;;;        ))))

(defmethod gui::push-object ((world 3d-world) (window gui::basic-window))
  (push-synthetic-view world window))
      
(defvar *synthetic-view-standoff* 10000.0)

(defun push-synthetic-view (3d-world window &key (1/f 0.0) (gsd -1.0) &allow-other-keys)
  (let ((bbox (obj::bounding-box 3d-world)))
    (bind-vector-elements (xmin xmax ymin ymax) bbox
      (let* ((xc (* 0.5 (+ xmin xmax)))
	     (yc (* 0.5 (+ ymin ymax)))
	     (frame-camera (cme::make-central-projection-camera
			    :1/F 1/f :R/F gsd
			    :xc xc
			    :yc yc
			    :zc *synthetic-view-standoff*
			    )))
	(incf *synthetic-2d-world-count*)
	(let ((2d-world (make-instance '2d-world
				       :name (format nil "Synthetic 2D World ~d"
						     *synthetic-2d-world-count*)
				       :3d-world 3d-world))
	      (pbbox (transform-bounding-box frame-camera bbox)))
	  (setf (3d-to-2d-projection 2d-world) frame-camera)
	  (let ((view (make-instance 'gui::synthetic-view
				     :2d-world 2d-world
				     :object-sets (default-object-sets 3d-world)
				     :3d-world 3d-world
				     :window window))
		(scale (min (/ (gui::window-width window) (max 1.0 (- (aref pbbox 1) (aref pbbox 0))))
			    (/ (gui::window-height window) (max 1.0 (- (aref pbbox 3) (aref pbbox 2)))))))
	    (gui::push-view view window)
	    (let ((m (transform-matrix (2d-to-window-transform view ))))
	      (setf (aref m 0 0) scale
		    (aref m 1 1) (- scale))
	      (transforms::update-transform (2d-to-window-transform view ) ))
	    ))))))

(defun push-cloned-synthetic-view (view window)
  (let* ((frame-camera (3d-to-2d-projection view))
	 (frame-camera (transforms::make-frame-camera-from-matrices 
			(copy-matrix (transforms::3d-to-camera-matrix frame-camera))
			(copy-matrix (transforms::camera-to-2d-matrix frame-camera))))
	 (3d-world (3d-world view))
	 (2d-world (make-instance '2d-world
				  :name (format nil "Synthetic 2D World ~d"
						(incf *synthetic-2d-world-count*))
				  :3d-world 3d-world)))
    (setf (3d-to-2d-projection 2d-world) frame-camera)
    (let* ((2d-to-window-transform (make-4x4-coordinate-transform 
				    (transforms::transform-matrix (2d-to-window-transform view))))
	   (view (make-instance 'gui::synthetic-view
				:2d-world 2d-world
				:2d-to-window-transform 2d-to-window-transform
				:object-sets (default-object-sets 3d-world)
				:3d-world 3d-world
				:window window)))
      (gui::push-view view window))))


#|
(declaim (optimize (debug 3)))
 (maybe-compile-file-load "$FREEDIUS/lisp/basic-gui/special-objects.lisp")

(push-synthetic-view (3d-world (top-view)) (gui::selected-window))
(gui::add-perspective-transform-stare-point-object (gui::selected-view) 
						   (vector-add (gui::selected-object-world-position)
							       (cv 10.0 0.0 0.0)))
(pop-view (selected-window))
|#
