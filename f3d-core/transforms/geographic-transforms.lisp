(IN-PACKAGE :transforms)

(defmacro set-functional-transform (function ellipsoid)
  (ignore ellipsoid)
  `(progn (setf (functional-coordinate-transform-transform-function *tmp-transform*) ',function
		(functional-coordinate-transform-functional-transform-info *tmp-transform*) ellipsoid)
	  *tmp-transform*))

#| DEFINITIONS OF STANDARD COORDINATE SYSTEMS AND MAP PROJECTIONS.

Equations taken from USGS software documentation:
"Plane: Generalized Plane Coordinate Conversion Subroutine Package".

Angles lat and long are always in degrees.  Lat increases to north. Long increases east

Angles phi and lambda are always in radians. Sign conventions same as for lat and long.

|#


;;; Eggsucking Allegro's LOOP macro needs the deftype side effect at compile time.
(eval-when (load eval compile)		      
(deftype angle () '(double-float -1d6 1d6))
) ; end eval-when

;;; Many of the code optimizations used here depend on the CL:TYPE-REDUCE declaration.  To eliminate
;;; its use will require introducing many double-float declarations, and the use of (declare (ftype
;;; (function (float) float) sin cos sqrt ...)).

(declaim (type double-float pi/180 double-pi double-2pi double-pi/180 double-180/pi))

(eval-when (load compile eval)
(declaim (double-float pi/180 double-pi double-2pi double-pi/180 double-180/pi))
(defconstant pi/180 (/ pi 180.0))
(defconstant double-pi pi)
(defconstant double-2pi (* 2 double-pi))
(defconstant double-pi/180 (/ double-pi 180.0))
(defconstant double-180/pi (/ 180.0 double-pi))
)

;;; I think this has something to do with USGS DEMs - cannot find references to this
(defconstant *utm-scale-factor* .9996d0) 

(defun from-deg-min-sec (deg &optional (min 0) (sec 0))
  (* (signum deg)
     (+ (abs deg) (/ min 60d0) (/ sec 3600d0))))

(defun from-deg-min-sec-string (string )
  (let ((sign 1.0) deg min sec pos1 pos2 pos3)
    (setq string (string-left-trim '(#\space) string))
    (when (char-equal (aref string 0) #\-)	
      (setq string (substring string 1)
	    sign -1))

    (mv-setq (deg pos1) (read-from-string string))
    
    (mv-setq (min pos2) (read-from-string string nil nil :start pos1))
    (when min
      (when (and (symbolp min) (member min '(deg d) :test 'string-equal)
		 )
	(mv-setq (min pos2) (read-from-string string nil nil :start pos2)))
      
      (when min
	(mv-setq (sec pos3) (read-from-string string nil nil :start pos2))
	(when (and (not (numberp sec)) (equal sec "min")) 
	  (mv-setq (sec pos3) (read-from-string string nil nil :start pos3)))
	(when sec
	  (when (and (listp sec) (equal (car sec) 'quote)) (setq sec (cadr sec))))))
    (unless (numberp deg) (setq deg 0))
    (unless (numberp min) (setq min 0))
    (unless (numberp sec) (setq sec 0))
    (* sign
       (+ (abs deg) (/ min 60.0d0) (/ sec 3600.0d0))))
  )


(defun to-deg-min-sec-string (deg0 &key (stream nil) (seconds-fraction-digits 4) abbrev)
  (let ((round-deg (when seconds-fraction-digits
		     (* (/ .5 3600) (expt 10.0d0 (- seconds-fraction-digits))))))
    (mv-bind (deg min sec)
	(to-deg-min-sec deg0 round-deg)
      (format stream (if abbrev
			 "~4d deg ~2d' ~v,2$\" "
			 "~4d deg ~2d min ~v,2$ sec ")
	      (if (minusp deg0) (- deg) deg)
	      min
	      seconds-fraction-digits
	      (- sec (* 3600.0 round-deg)) ; compensate for rounding that format conversion will do
	      ))))

(defun to-deg-min-sec (deg &optional (round-sec nil))
  ;; backward compatibility defaulting
  (cond ((eq round-sec t) (setq round-sec (/ .5 3600)))  ; round up by 1/2 second of arc.
	((eq round-sec nil) (setq round-sec 0.0)))
  
  (let* ((sign (signum deg ))
	 (deg (+ (abs deg) round-sec)) 
	 (int-deg (floor deg))
	 (min (* 60 (- deg int-deg)))
	 (int-min (floor min))
	 (sec (* 60 (- min int-min))))
    (values int-deg int-min sec sign)))


(declaim (type double-float *feet-per-meter-international* *feet-per-meter-us* *feet-per-meter*))

;;; Should these be in geographic-constants.lisp?
(defconstant *feet-per-meter-international*  (/ .3048d0))
(defconstant *feet-per-meter-us* (/ 39.37d0 12.0d0))
(defparameter *feet-per-meter* *feet-per-meter-us* ) ; defaulting to US standard

 ;;; ****************  ELLIPSOID DEFINITION  *******************

(defparameter *ellipsoid-alist* nil)

;;; ELLIPSOID class is the hub for finding geographic transforms.

;;; For some reason in CMUCL the :type double-float declarations sometimes 
;;; cause reader and writer functions to be missing from the effective-slot-definition 
;;; in the class-slots of ellipsoid. Ie, (describe (nth 2 (pcl::class-slots (find-class 'ellipsoid))))
;;; will show slots (a b f e es 1-es) to have missing readers and writers.

;;;(defstruct-class ellipsoid
;;;                                        ;(base-coordinate-system-class)
;;;    (base-struct-class)                 ; need to change to this ---
;;;    
;;;  ((name :initform nil :initarg :name :accessor name)
;;;   (abbrev :initform nil :initarg :abbrev :accessor abbrev)
;;;   (a :type double-float :initarg :a)   ; semi-major-axis   ; equatorial radius
;;;   (b :type double-float :initarg :b)   ; semi-minor-axis   ; polar radius
;;;   (f :type double-float)
;;;   (e :type double-float)
;;;   (es :type double-float)
;;;   (1-es :type double-float)
;;;   (gcc-coordinate-system :initarg :gcc-coordinate-system
;;;                          :accessor gcc-coordinate-system)
;;;   ;; gdc-coordinate-system  same as geoid-coordinate-system in SRIQCT
;;;   ;; gdc vertical datum is this ellipsoid
;;;   (gdc-coordinate-system :initarg :gdc-coordinate-system :accessor gdc-coordinate-system)
;;;   (gcc-to-gdc-transform :initarg :gcc-to-gdc-transform
;;;                                    :accessor gcc-to-gdc-transform)
;;;   ;; Linear 4x4 transform
;;;   (gcc-to-wgs84-gcc-transform :initarg :gcc-to-wgs84-gcc-transform
;;;                                          :accessor gcc-to-wgs84-gcc-transform)
;;;   ;; Molodensky transform for ellipsoid shift
;;;   (gdc-to-wgs84-gdc-transform :initarg :gdc-to-wgs84-gdc-transform
;;;                                          :accessor gdc-to-wgs84-gdc-transform)
;;;   ))

(defstruct-class ellipsoid
					;(base-coordinate-system-class)
    (base-struct-class)			; need to change to this ---
    
  ((name :initform nil :initarg :name :accessor name)
   (abbrev :initform nil :initarg :abbrev :accessor abbrev)
   (a :initarg :a)	; semi-major-axis   ; equatorial radius
   (b :initarg :b)	; semi-minor-axis   ; polar radius
   (f )
   (e )
   (es )
   (1-es )
   (gcc-coordinate-system :initarg :gcc-coordinate-system
			  :accessor gcc-coordinate-system)
   ;; gdc-coordinate-system  same as geoid-coordinate-system in SRIQCT
   ;; gdc vertical datum is this ellipsoid
   (gdc-coordinate-system :initarg :gdc-coordinate-system :accessor gdc-coordinate-system)
   (gcc-to-gdc-transform :initarg :gcc-to-gdc-transform
				    :accessor gcc-to-gdc-transform)
   ;; Linear 4x4 transform
   (gcc-to-wgs84-gcc-transform :initarg :gcc-to-wgs84-gcc-transform
					  :accessor gcc-to-wgs84-gcc-transform)
   ;; Molodensky transform for ellipsoid shift
   (gdc-to-wgs84-gdc-transform :initarg :gdc-to-wgs84-gdc-transform
					  :accessor gdc-to-wgs84-gdc-transform)
   ))

;;;(defmethod print-object ((object ellipsoid) stream)
;;;  (format stream "#<~a ~a #X~x>"
;;;          (type-of object)
;;;          (name object)
;;;          (%pointer object)))

(defmethod print-object ((object ellipsoid) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (prin1 (name object) stream)))

(defmethod initialize-instance :after ((ellipsoid ellipsoid) &key shift-to-wgs84-gcc rf &allow-other-keys)
  (declare (type (or null double-float) rf))
  (with-class-slots ellipsoid (a b f e es 1-es) ellipsoid
    (if rf
	(setq f (/ rf)
	      b (* a (- 1.0d0 f)))
	(setq f (/ (- a b) a)))

    ;; This should move to lat-long-coordinate-system
    (setf es (- (* 2d0 f) (^2 f))
	  1-es (- 1.0d0 es)
	  e (sqrt es)))

  (with-class-slots ellipsoid (name gcc-coordinate-system gdc-coordinate-system
				    gcc-to-gdc-transform gcc-to-wgs84-gcc-transform)
      ellipsoid
    (setf gcc-coordinate-system 
	  (make-coordinate-system name 3
				  :class 'geocentric-coordinate-system
				  :component-names '("GX" "GY" "GZ")
				  :component-units 'meters
				  :ellipsoid ellipsoid)
	  gdc-coordinate-system (make-gdc ellipsoid)
	  #+old
	  (make-coordinate-system name 3
				  :class 'lat-long-coordinate-system
				  :component-names '("Long" "Lat" "h")
				  :component-units '(degrees degrees meters)
				  :ellipsoid ellipsoid
				  :parent gcc-coordinate-system
				  )
	  gcc-to-gdc-transform
	  (inverse-transform
	   (make-lat-long-to-geocentric-transform gdc-coordinate-system gcc-coordinate-system))
	  gcc-to-wgs84-gcc-transform
	  (unless (equal name "WGS84")
	    (make-4x4-coordinate-transform
	     (make-object-to-parent-matrix (or shift-to-wgs84-gcc (cv 0.0 0.0 0.0)))
	     :from-coordinate-system gcc-coordinate-system
	     :to-coordinate-system (get-gcc 'WGS84))))

    (when gcc-to-wgs84-gcc-transform
      (setf (parent gcc-coordinate-system) (get-gcc 'wgs84)
	    (object-to-parent-transform gcc-coordinate-system) gcc-to-wgs84-gcc-transform))
    (push (list name ellipsoid) *ellipsoid-alist*)
    ))
  

;;; instantiating using rf vs b from the NIMA ellplst.txt file results in differences
;;; at about 11 digits, or about .1 mm (or less).
(defun-cached make-ellipsoid (&key a b rf name (ak0 .9996d0) shift-to-wgs84-gcc)
  (make-instance 'ellipsoid :a a :b (or b 0.0d0) :rf rf :name (string name) :ak0 ak0
		 :shift-to-wgs84-gcc shift-to-wgs84-gcc :property-list nil))

(defun get-ellipsoid (name)
  (setq name (string name))
  (cadr (assoc name *ellipsoid-alist* :test #'equal)))


(defun get-gdc (ellispoid-name &optional vertical-datum)
  (make-gdc (get-ellipsoid ellispoid-name) vertical-datum))
    
(defun get-gcc (ellipsoid-name)
  (gcc-coordinate-system  (get-ellipsoid ellipsoid-name)))

(defmacro gdc (ellipsoid-name &optional vertical-datum)
  `(get-gdc ',ellipsoid-name ., (when vertical-datum `(',vertical-datum))))

(defmacro gcc (ellipsoid-name)
  `(get-gcc ',ellipsoid-name))




;;;(defun-cached make-ellipsoid (&rest args &key a b rf name (ak0 .9996d0) property-list)
;;;  (apply #'make-instance 'ellipsoid (or ak0 .9996d0) args))



;;; GEOCENTRIC-COORDINATE-SYSTEM GEOGRAPHIC-COORDINATE-SYSTEM LAT-LONG-COORDINATE-SYSTEM

(defstruct-class geocentric-coordinate-system (3d-cartesian-coordinate-system)
  ((ellipsoid :initarg :ellipsoid :accessor ellipsoid)))

(defmethod to-geocentric-transform ((cs geocentric-coordinate-system))
  nil)


(declaim (special *default-ellipsoid*))
(defparameter *default-ellipsoid* nil)

;;; This needs to be changed to correspond to SRIQCT.
;;; SRIQCT equivalent is GeoCoordSys
(defstruct-class geographic-coordinate-system (3d-coordinate-system-mixin coordinate-system)
  ((ellipsoid :initform *default-ellipsoid* :initarg :ellipsoid :accessor ellipsoid)))

(defmethod ellipsoid ((cs basic-coordinate-system))
  (with-class-slot-values basic-coordinate-system (parent) cs
    (when parent (ellipsoid parent))))

;;; some code depends on :NAME keyword argument to be handled.
(defmethod initialize-instance :after ((coordinate-system geographic-coordinate-system)
				       &key &allow-other-keys))
  

;;; This is called GeoidCoordSys in SRIQCT
(defstruct-class lat-long-coordinate-system (geographic-coordinate-system)
  ((vertical-datum :initform nil :initarg :vertical-datum :accessor vertical-datum)))

(defmethod to-geocentric-transform ((coordinate-system lat-long-coordinate-system))
  (inverse-transform (gcc-to-gdc-transform (ellipsoid coordinate-system))))

;;; Some code expects :NAME keyword argument to be handled.
(defmethod initialize-instance :after ((coordinate-system lat-long-coordinate-system)
				       &rest initargs &key &allow-other-keys)
  (ignore initargs)
  (when (equal (name coordinate-system) "NAD27 ") (break))
  )

(defun-cached make-gdc (ellipsoid &optional vertical-datum)
  (make-coordinate-system (if vertical-datum 
			      (format nil "~a.~a" (name ellipsoid) (name vertical-datum))
			      (name ellipsoid))
			  3
			  :class 'lat-long-coordinate-system
			  :component-names '("Long" "Lat" "h")
			  :component-units '(degrees degrees meters)
			  :ellipsoid ellipsoid
			  :vertical-datum vertical-datum
			  :parent (gcc-coordinate-system ellipsoid)))
#|
(eval-cache-flush-function 'make-gdc)
(make-gdc (get-ellipsoid 'wgs-84))
|#

(defmethod to-geocentric-transform ((cs basic-coordinate-system))
  (with-class-slot-values basic-coordinate-system (parent object-to-parent-transform) cs
    (when (and parent object-to-parent-transform)
      (let ((trans (to-geocentric-transform parent)))
	(if trans
	    (cons object-to-parent-transform (if (consp trans) trans (list trans)))
	    object-to-parent-transform)))))


(defmethod to-lat-long-transform ((cs basic-coordinate-system))
  (let ((to-geocentric-transform (to-geocentric-transform cs)))
    (when to-geocentric-transform
      (list to-geocentric-transform
	    (gcc-to-gdc-transform (ellipsoid (to-coordinate-system to-geocentric-transform)))))))

(defmethod to-lat-long-transform ((cs geographic-coordinate-system))
  (with-class-slot-values basic-coordinate-system (parent object-to-parent-transform) cs
    (when (and parent object-to-parent-transform)
      (cons object-to-parent-transform
	    (to-lat-long-transform parent)))))

(defmethod to-lat-long-transform ((cs lat-long-coordinate-system))
  nil)


(defmethod to-utm-transform ((lvcs local-vertical-coordinate-system) &optional central-meridian)
  (transforms::canonical-coordinate-transform-path 
   lvcs
   (make-utm-coordinate-system (or central-meridian (get-prop lvcs :origin-long))
			       (to-coordinate-system (to-lat-long-transform lvcs)))))


;;; from-vector = (deg-longitude deg-latitude)
;;; to-vector = (x y z)

;;; NEED TO FIX THIS FOR ALLEGRO -- NO type-reduce
;(disassemble 'lat-long-to-geocentric-transform-vector)
;(fmakunbound 'lat-long-to-geocentric-transform-vector)
(defun lat-long-to-geocentric-transform-vector (transform from-vector to-vector)
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 2)))
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (or null (simple-array double-float (*))) from-vector))
  (declare (type (simple-array double-float (*)) to-vector))
  (when from-vector
    (with-class-slot-values
	functional-coordinate-transform ((ellipsoid functional-transform-info)) transform
      (bind-vector-elements (deg-longitude deg-latitude h) from-vector
	(with-class-slot-values ellipsoid (a e 1-es) ellipsoid
	  (declare (double-float a e 1-es))
	  (let* ((rad-latitude (* double-pi/180 deg-latitude))
		 (rad-longitude (* double-pi/180 deg-longitude)) 
		 (sin-latitude (sin rad-latitude))
		 (n (/ a (dsqrt (- 1.0d0 (^2 (* e sin-latitude))))))
		 (xy (* (+ n h) (cos rad-latitude)))
		 )
	  ;  (declare (type (double-float -1d6 1d6) rad-latitude rad-longitude))
	    (declare (type angle rad-latitude rad-longitude))
	    (declare (double-float sin-latitude n xy))
	    (inline-set-coordinate-vector-elements to-vector
					    (* xy (cos rad-longitude))
					    (* xy (sin rad-longitude))
					    (* (+ (* n 1-es) h) sin-latitude))
	    to-vector))))))

(declaim (type double-float *GEOCENTRIC-TO-LAT-LONG-TRANSFORM-VECTOR-eps*))

(defvar *GEOCENTRIC-TO-LAT-LONG-TRANSFORM-VECTOR-eps* 1e-12)
(setq *GEOCENTRIC-TO-LAT-LONG-TRANSFORM-VECTOR-eps* 1e-10)
;;(declaim (special *GEOCENTRIC-TO-LAT-LONG-TRANSFORM-VECTOR-iters*))

;;; GEOCENTRIC-TO-LAT-LONG-TRANSFORM-VECTOR is relatively slow due to iterative implementation.
;;; Takes about 350 usecs and 304 Ephemeral Bytes Consed with *GEOCENTRIC-TO-LAT-LONG-TRANSFORM-VECTOR-eps* = 1e-12.
;;; This is a candidate for C coding because of consing done by atan.

#|
(describe (3d-world (top-view)))
(3d-world (top-view))
(describe (car (object-to-parent-transform (3d-world (top-view)))))
(setq trans (lvcs-to-lat-long-transform (3d-world (top-view))))
(setq pt (gui::selected-object-world-position *interactor*))

(lvcs-to-lat-long-transform (3d-world (top-view)))

(cadr trans)
(progn pt)
(transform-vector trans pt)
(inverse-transform-vector trans (cv -105.12088796236267 (+ (/ .01 3600)39.4999187050919) 2.1793324901441338))
(* 3600 (degrees 1e-6)) = .2 arc-seconds
;;; Careful, ALV site has LVCS units in feet!
(let ((transforms::*GEOCENTRIC-TO-LAT-LONG-TRANSFORM-VECTOR-eps*  1e-6)) ; 1 micro-radian
  (inverse-transform-vector trans (transform-vector trans pt))
  (values (vector-difference (inverse-transform-vector trans (transform-vector trans pt))
			     pt)
	  transforms::*GEOCENTRIC-TO-LAT-LONG-TRANSFORM-VECTOR-iters*))

(inverse-transform-vector trans (coordinate-vector -97.78094053067132 31.580917803244678 0.0))
(inverse-transform-vector trans (coordinate-vector -97.78094053067132 (+ (/ .01 3600) 31.580917803244678) 0.0))
(inverse-transform-vector trans (coordinate-vector -97.78094053067132 31.580917803244678 -0.4429763807226191E7
(progn transforms::*GEOCENTRIC-TO-LAT-LONG-TRANSFORM-VECTOR-iters*)
(atan .001 6378137d0)
(transform-jacobian (inverse-transform (cadr trans ))
		    (cv -105.12088796236267 39.4999187050919 2.1793324901441338))

|#

#|		
(disassemble 'tst)
(defun tst (x y)
  (declare (type (double-float -1e6 1e6) x y))
  (atan x y))

(defun tst2 (x)
  (declare (type (double-float -1e6 1e6) x))
  (atan x ))

|#

;;; ALLEGRO NOTE: Apparently Allegro cannot inline ATAN.  I have tried
;;; many experiments, but nothing works.  This really SUCKS the big one,
;;; because GEOCENTRIC-TO-LAT-LONG-TRANSFORM-VECTOR is called frequently.

;;; Version based on /opt/IU/apgd/code/SRIQCT/MUSE-mdtcc/src/usrfns.c
;;; Converges in 2 iterations with greater precision
;;; from-vector = (x y z)
;;; to-vector = (deg-longitude deg-latitude deg-latitude)
;(disassemble 'GEOCENTRIC-TO-LAT-LONG-TRANSFORM-VECTOR)
(defun GEOCENTRIC-TO-LAT-LONG-TRANSFORM-VECTOR (transform from-vector to-vector)
  #+cmu (declare (ext:optimize-interface (speed 3)(safety 2)))
  (declare (optimize (speed 3)(safety 1)))
  (declare (type (or null (simple-array double-float (*))) from-vector))
  (declare (type (simple-array double-float (*)) to-vector))
  (when from-vector
    (unless (>= (length to-vector) 3) (error "To-vector ~a must have length >= 3" to-vector))
    (with-class-slot-values
	functional-coordinate-transform ((ellipsoid functional-transform-info)) transform
      (bind-vector-elements (x y z) from-vector
	(with-class-slot-values ellipsoid (a b es) ellipsoid
	  (declare (double-float a b es))
	  (let* ((one 1.0d0)
		 (dd (+ (* x x) (* y y)))
		 (d (dsqrt dd)))
	    (declare (type (double-float 0.0 1d20) dd))
	    (declare (type double-float one d))
	    (if (zerop d)
		(inline-set-coordinate-vector-elements
		 to-vector
		 (if (< z 0.0) -90.0 90.0)
		 0.0
		 (- (abs z) b))

		(loop for iters fixnum from 1 to 2
		      with epsq double-float = (/ es (- 1.0 es))
		      with en double-float = (sqrt (the (double-float 0.0 1e20) (- 1.0 es)))
		      for beta of-type angle = (atan (/ (* a z) (* b d)))
			then (atan (* en tan-latitude))
		      for sb double-float = (sin beta)
		      for cb double-float = (cos beta)
		      for tan-latitude double-float = (/ (+ z (* epsq b sb sb sb))
							      (- d (* a es cb cb cb)))
		      finally
		   (let* ((latitude (atan tan-latitude))
			  (sin-latitude (sin latitude))
			  (cos-latitude (cos latitude))
			  (deg-latitude (* latitude DOUBLE-180/pi))
			  (n (/ a (dsqrt (the (double-float 0.0 1e20)
					   (- one (* es sin-latitude sin-latitude))))))
			  (h (if (> (abs deg-latitude) 89.0)
				 (+ (/ z sin-latitude) (* n (- es one)))
				 (-  (/ d cos-latitude) n))))
		     (declare (double-float deg-latitude sin-latitude cos-latitude n h))
		     (declare (type angle latitude))
		     (declare (special *GEOCENTRIC-TO-LAT-LONG-TRANSFORM-VECTOR-iters*))
		     ;; what is this!
		     (setq *GEOCENTRIC-TO-LAT-LONG-TRANSFORM-VECTOR-iters* 2)
			 
		     (return (inline-set-coordinate-vector-elements
			      to-vector
			      (* (truly-the double-float (atan y x)) DOUBLE-180/pi )
			      deg-latitude
			      h)))))))))))

#|
(disassemble 'TRANSFORMS::GEOCENTRIC-TO-LAT-LONG-TRANSFORM-VECTOR)
|#

(defun radius-at-latitude (ellipsoid deg-latitude)
  (declare (double-float deg-latitude))
  (with-class-slots
	ellipsoid (a 1-es e) ellipsoid
      (let* ((sin-deg-latitude (sind deg-latitude))
	     (cos-deg-latitude (cosd deg-latitude))
	     (h 0.0)
	     (n (/ a (sqrt (- 1.0d0 (^2 (* e sin-deg-latitude))))))
	     (xy (* (+ n h) cos-deg-latitude))
	     (z (* (+ (* n 1-es) h) sin-deg-latitude)))
	(sqrt (+ (* xy xy) (* z z))))))


#|
(mv-setq (x y cm conv)
  (lat-long-to-utm *clarke-1866-ellipsoid* (from-deg-min-sec 27 7 30) (- (from-deg-min-sec 92 30))))

(mv-setq (x y cm conv)
  (lat-long-to-utm *clarke-1866-ellipsoid2* (from-deg-min-sec 27 7 30) (- (from-deg-min-sec 92 30))))

(- x 549553.918d0)
(- y 3000211.052) 

(mv-setq (lat long) (utm-to-lat-long *clarke-1866-ellipsoid* x y 93.0))

(mv-setq (x y cm conv)
  (lat-long-to-utm *clarke-1866-ellipsoid* (from-deg-min-sec  27 15) (- (from-deg-min-sec 94 37 30))))
(- x 339117.754d0)  ; These numbers came from USGS circular 895-B, page 6.
(- y 3015002.066d0) ;Find .1 meter difference here.  Fortran program on VAX agrees with Lispm.

;; 339117.7613846427       3015001.963555338     Fortran program on VAX iu:usr:[quam]utm.for 
;; 339117.7613846401       3015001.9635553383    LISPM and Sun Sparc
;; 339117.754d0            3015002.066d0         USGS circular 895-B, page 6.
|#

#|
(defun tst (n)
  (time (loop with v = (coordinate-vector 27.0 -92.0 0)
	      with into-v = (make-coordinate-vector 3)
	      with gdc-to-gcc = (to-geocentric-transform (get-gdc 'wgs84))
	      repeat n do (transform-vector gdc-to-gcc v into-v))))
(tst 10000) ;41 usec, 0 conses

(defun tst (n)
  (time (loop with x = 27.0 repeat n do (setq foo (sin x)))))
(tst 100000) ; 35 usec, 16 conses

(defun tst (n)
  (time (loop with x = 27.0 repeat n do (setq foo (sind x)))))
(tst 100000) ; 56 usec, 32 conses

(defun tst (n eps)
  (let ((*GEOCENTRIC-TO-LAT-LONG-TRANSFORM-VECTOR-eps* eps)
	(gdc-to-gcc (to-geocentric-transform (get-gdc 'wgs84))))
    (time (loop with v = (transform-vector gdc-to-gcc
					   (coordinate-vector 27.0 -92.0 0))
		with into-v = (make-coordinate-vector 3)
		repeat n
		do (inverse-transform-vector gdc-to-gcc v into-v)))))
(inverse-transform (to-geocentric-transform (get-gdc 'wgs84)))
(tst 1000 1e-6) ; 110 usec 96 conses
(tst 1000 1e-10) ; 140 usec 128 conses
(tst 1000 1e-12) ; 140 usec 128 conses

(lat-long-to-geocentric *clarke-1866-ellipsoid* 27.0 -92.0 0.0)
(lat-long-to-geocentric *clarke-1866-ellipsoid2* 27.0 -92.0 0.0)

(bind-vector-elements (x y z)
    (lat-long-to-geocentric2 *clarke-1866-ellipsoid2* 27.0 -92.0 0.0)
  (geocentric-to-lat-long2 *clarke-1866-ellipsoid2* x y z))
|#

#|    
(radius-at-latitude *clarke-1866-ellipsoid* 0)
(radius-at-latitude2 *clarke-1866-ellipsoid2* 0)
(slot-value *clarke-1866-ellipsoid* 'a)
(radius-at-latitude *clarke-1866-ellipsoid* 90)
(slot-value *clarke-1866-ellipsoid* 'b)
|#

#|
(lat-long-to-geocentric *clarke-1866-ellipsoid* (cv 37.0 122.0 300.0))

|#

 ;;; ***************   UTM CONVERSIONS  ***************

(defun longitude-to-utm-zone (long)
  (let ((bin (floor long 6.0)))
    (1+ (mod (+ 30 bin) 60))))

#| this doesn't work, but the Lucid manual suggests that it should ...
(defun utm-zone (long)
  (let ((bin (floor long 6.0)))
    (declare (lcl:type-reduce integer fixnum))
    (1+ (mod (+ 30 (the fixnum bin)) 60))))
|#

(defun central-meridian-from-utm-zone (zone)
  (dfloat (mod (+ 3 (* 6 (- zone 31))) 360)))

(defstruct-class utm-coordinate-system (geographic-coordinate-system)
  ((cm :initform nil :initarg :cm)
   (southern-hemisphere-p :initform nil :initarg :southern-hemisphere-p)
   (ak0 :type double-float :initform .9996d0 )))


;;;(defmethod object-to-superior-transform ((cs utm-coordinate-system))
;;;  (utm-to-lat-long-transform cs))

(defmethod utm-zone ((coordinate-system utm-coordinate-system))
  (with-class-slots utm-coordinate-system (cm) coordinate-system
    (longitude-to-utm-zone cm)))

(defun make-utm-coordinate-system (central-meridian lat-long-coordinate-system)
  ;; force central-meridian to really be where it should be
  (setq central-meridian (central-meridian-from-utm-zone (longitude-to-utm-zone central-meridian)))
  (eval-cache (make-utm-coordinate-system central-meridian lat-long-coordinate-system)
      (let* ((ellipsoid (ellipsoid lat-long-coordinate-system))
	     (name (or (get-prop lat-long-coordinate-system :generic-name)
		       (name ellipsoid))))
	(let ((utm-cs
	       (make-coordinate-system
		(format nil "UTM Zone ~a in ~A" (longitude-to-utm-zone central-meridian) name) 3
		:class 'utm-coordinate-system
		:ellipsoid ellipsoid
		:cm central-meridian
		;;:lat-long-coordinate-system  lat-long-coordinate-system
		:parent lat-long-coordinate-system
		:component-names '("UTMx" "UTMy" "UTMz") :component-units 'meters
		:property-list '(:local-units-per-meter 1.0)
		)))
	  (make-lat-long-to-utm-transform lat-long-coordinate-system utm-cs)
	  utm-cs))))

(defun make-utm-zone-to-lat-long-transform (utm-zone lat-long-to-geocentric-transform)
  (let ((utm-cs (make-utm-coordinate-system (central-meridian-from-utm-zone utm-zone)
					    (from-coordinate-system lat-long-to-geocentric-transform))))
    (object-to-parent-transform utm-cs)))

(declaim (inline positive-normalize-angle signed-normalize-angle))


#+cmu
(progn

;;; returns an angle in the range 0 : 2pi
;;; avoid possible loss of significance when possible
(defun positive-normalize-angle (angle &optional (modulus double-2pi))
  (declare (optimize (speed 3)(safety 1)))
  (declare (type angle angle modulus))
  (if (and (>= angle 0.0)
	   (< angle modulus))
      angle
      (mod angle modulus)))
;(disassemble 'positive-normalize-angle)
;(disassemble 'signed-normalize-angle)

;;; returns an angle in the range -pi : +pi
(defun signed-normalize-angle (angle &optional (modulus double-2pi))
  (declare (optimize (speed 3)(safety 1)))
  (declare (type angle angle modulus))
  (let* ((plus-angle (positive-normalize-angle angle modulus))
	 (dplus-angle plus-angle)
	 (dmodulus modulus)
	 )
    (declare (type angle plus-angle dplus-angle dmodulus))
    (if (> dplus-angle (* .5 dmodulus))
	(- dplus-angle dmodulus)
	plus-angle)))

) ; end progn

#+allegro
(progn

;;; Allegro sucks -- cna't figure out how to get inline (mod df df)
(defmacro dmod (x y)
  `(let ((x ,x)
	 (y ,y))
     (declare (double-float x y))
     (let* ((x/y  (/ x y))
	    (n (floor x/y))
	    (mod (- x (* (dfloat n) y))))
       (declare (fixnum n))
       (declare (type small-double-float x/y))
       (declare (double-float mod))
       mod)))

;;; returns an angle in the range 0 : 2pi
;;; avoid possible loss of significance when possible
(defmacro positive-normalize-angle (angle &optional (modulus double-2pi))
  `(dmod (the angle ,angle) (the angle ,modulus)))

;;; returns an angle in the range -pi : +pi
(defmacro signed-normalize-angle (angle &optional (modulus double-2pi))
  `(let* ((angle ,angle)
	  (modulus ,modulus)
	  (positive-angle (dmod angle modulus)))
     (declare (optimize (speed 3)(safety 0)))
     (declare (type angle angle modulus))
     (declare (double-float positive-angle))
     (if (> positive-angle (* .5 modulus))
	 (- positive-angle modulus)
	 positive-angle)))
	 
) ; end progn


(defstruct-class lat-long-to-utm-coordinate-transform (coordinate-transform)
  ((a :type double-float)
   (cm :initform nil :initarg :cm)
   (es :type double-float)
   (f1 :type double-float)
   (f2 :type double-float)
   (f3 :type double-float)
   (f4 :type double-float)
   (epri :type double-float)
   (ak0 :type double-float :initform .9996d0 )))

(defmethod initialize-instance :after ((transform lat-long-to-utm-coordinate-transform)
				       &key &allow-other-keys)
  (let ((utm-cs (to-coordinate-system transform)))
    (with-class-slot-values ellipsoid ((ua a) f) (ellipsoid utm-cs)
      (with-class-slot-values utm-coordinate-system ((ucm cm) (uak0 ak0)) utm-cs
	(with-class-slots lat-long-to-utm-coordinate-transform
	      (a cm es f1 f2 f3 f4 epri ak0) transform
	  (let (es2 es3)
	    (setf a ua
		  cm ucm
		  ak0 uak0
		  es (- (* 2d0 f) (^2 f))
		  epri (/ es (- 1.0d0 es))
		  es2 (* es es)			
		  es3 (* es2 es)
		  f1 (- 1.0d0
			(* (/ 1d0 4d0) es)
			(* (/ 3d0 64d0) es2)
			(* (/ 5d0 256d0) es3))
	
		  f2 (+ (* (/ 3d0 8d0) es)
			(* (/ 3d0 32d0) es2)
			(* (/ 45d0 1024d0) es3))
	
		  f3 (+ (* (/ 15d0 256d0) es2)
			(* (/ 45d0 1024d0) es3))
	
		  f4 (* (/ 35d0 3072d0) es3))))))))

(defun positive-normalize-angle (angle &optional (modulus double-2pi))
  (let ((dangle angle) (dmodulus modulus))
    (declare (double-float dangle dmodulus))
    (if (and (>= dangle 0.0)
	     (< dangle dmodulus))
	angle
	(mod angle modulus))))


(defun signed-normalize-angle (angle &optional (modulus double-2pi))
  ;;(declare (type-reduce number double-float))
  (let* ((plus-angle (positive-normalize-angle angle modulus))
	 (dplus-angle plus-angle)
	 (dmodulus modulus)
	 )
    (declare (double-float dplus-angle dmodulus))
    (if (> dplus-angle (* .5 dmodulus))
	(- dplus-angle dmodulus)
	plus-angle)))

;(disassemble 'lat-long-to-utm-transform-vector)	
(defun lat-long-to-utm-transform-vector (transform from-vector to-vector)
  #+cmu (declare (ext:optimize-interface (speed 3)(safety 2)))
  (declare (optimize (speed 3)(safety 1)))
  (declare (type (or null coordinate-vector) from-vector))
  (declare (type coordinate-vector to-vector))
  (when from-vector
    (bind-vector-elements (deg-long deg-lat h) from-vector
      (with-class-slot-values lat-long-to-utm-coordinate-transform
	    (a cm es f1 f2 f3 f4 epri ak0) transform
	(declare (double-float a cm es f1 f2 f3 f4 epri ak0))
	(let* ((dlam (signed-normalize-angle (* (- deg-long cm) DOUBLE-pi/180)))
	       (phi (* deg-lat DOUBLE-pi/180))
	       (cos-phi (cos phi))
	       (sin-phi (sin phi))
	       (en (/ a (dsqrt (the (double-float 0.0) (- 1d0 (* es (^2 sin-phi)))))))
	       (t* (^2 (/ sin-phi cos-phi)))
	       (t2 (^2 t*))
	       (c (* epri (^2 cos-phi)))
	       (aa dlam)		; dlam^1 * cos-phi^0
	       (aa2 (* aa aa cos-phi))	; dlam^2 * cos-phi^1
	       (aa4 (* aa2 aa2 cos-phi)) ; dlam^4 * cos-phi^3
	       (aa6 (* aa2 aa4 cos-phi)) ; dlam^6 * cos-phi^5
	       (em (* a (+ (* f1 phi)
			   (- (* f2 (sin (* 2d0 phi))))
			   (* f3 (sin (* 4d0 phi)))
			   (- (* f4 (sin (* 6d0 phi)))))))

	       (x (+ 5d5
		     (* ak0 en aa cos-phi
			(+ 1d0
			   (* cos-phi aa2 (+ 1d0 (- t*) c) #.(/ 6d0))
			   (* cos-phi aa4 (+ 5d0 (* -18d0 t*) t2 (* 72d0 c) (* -58d0 epri)) #.(/ 120d0))))))
	       (y (* ak0
		     (+ em (* en sin-phi
			      (+ (* aa2 .5d0)
				 (* aa4 (+ 5d0 (- t*) (* 9d0 c) (* 4d0 c c)) #.(/ 24d0))
				 (* aa6 (+ 61d0 (* -58d0 t*) t2 (* 600d0 c) (* -330d0 epri)) #.(/ 720d0)))))))
	       
	       #|
	       (conv (* (/ DOUBLE-PI/180)
	             dlam 
	             (+ sin-phi
	             (* 1.9587d-12 dlam dlam sin-phi (cos (^2 phi)))
	             )))
	       |#
	       )
	  (declare (type angle phi))
	  (declare (double-float dlam cos-phi sin-phi en t* t2 c aa aa2 aa4 aa6 em x y))
	  (when (minusp phi) (incf y 1d7))
	  (inline-set-coordinate-vector-elements to-vector x y h)
	  to-vector
	  )))))

(defstruct-class utm-to-lat-long-coordinate-transform (coordinate-transform)
  ((a :type double-float)
   (cm :initform nil :initarg :cm)
   (southern-hemisphere-p)
   (ak0 :type double-float)
   (es :type double-float)
   (1-es :type double-float)
   (epri :type double-float)
   (f1 :type double-float)
   (e2 :type double-float)
   (e3 :type double-float)
   (e4 :type double-float)))

(defmethod initialize-instance :after ((transform utm-to-lat-long-coordinate-transform)
				       &key &allow-other-keys)
  (let ((utm-cs (from-coordinate-system transform)))
    (with-class-slots utm-to-lat-long-coordinate-transform (southern-hemisphere-p) transform
      (with-class-slot-values ellipsoid ((ea a) f) (ellipsoid utm-cs)
	(with-class-slot-values utm-coordinate-system
	      ((ucm cm) (uak0 ak0) (usouthern-hemisphere-p southern-hemisphere-p)) utm-cs
	  (with-class-slots utm-to-lat-long-coordinate-transform
		(a cm ak0 es 1-es epri f1 e2 e3 e4) transform
	    (setf a ea
		  ak0 uak0
		  cm ucm
		  southern-hemisphere-p usouthern-hemisphere-p
		  es (- (* 2d0 f) (^2 f))
		  1-es (- 1.0d0 es)
		  epri (/ es (- 1.0d0 es))
		  f1 (- 1.0d0
			(* (/ 1d0 4d0) es)
			(* (/ 3d0 64d0) (* es es))
			(* (/ 5d0 256d0) (* es es es))))
	    
	    (let* ((sqrt-1-es (sqrt 1-es))
		   (e1 (/ (- 1d0 sqrt-1-es) (+ 1d0 sqrt-1-es)))
		   (e1^3 (* e1 e1 e1)))
	      (setq e2 (- (* 1.5d0 e1) (* (/ 27d0 32d0) e1^3))
		    e3 (- (* (/ 21d0 16d0) e1 e1) (* (/ 55d0 32d0) (* e1 e1^3)))
		    e4 (* (/ 151d0 96d0) e1^3)))))))))
#|
(disassemble 'tst)
(defun tst (x)
  (declare (optimize (speed 3)(safety 0)))
  (declare (type (double-float -1e6 1e6) x))
  ;(declare (double-float x))
  (let ((y (sin x)))
    (declare (double-float y))
    (plusp y)))

(defun tst (x)
  (declare (optimize (speed 3)(safety 0)))
  ;(declare (type (double-float 0.0 1e6) x))
  (declare (type (double-float 0.0) x))
  ;(declare (double-float x))
  (let ((y (sqrt x)))
    (declare (double-float y))
    (plusp y)))

(defun tst (x)
  (declare (optimize (speed 3)(safety 0)))
  ;(declare (type (double-float -1e6 1e6) x))
  (declare (double-float x))
  (let ((y (^2 x)))
    (declare (double-float y))
    (plusp y)))


(defun tst (x)
  (declare (optimize (speed 3)(safety 0)))
  (declare (double-float x))
  ;(declare (type (double-float -1e6 1e6) x))
  (let ((y (^2 x)))
    (declare (type (double-float -1e6 1e6) y))
    (nth-value 0 (round y))))
|#
;(disassemble 'utm-to-lat-long-transform-vector)
(defun utm-to-lat-long-transform-vector (transform from-vector to-vector)
  #+cmu (declare (ext:optimize-interface (speed 3)(safety 2)))
  (declare (optimize (speed 3)(safety 0)))
  (declare (type (or null coordinate-vector) from-vector))
  (declare (type coordinate-vector to-vector))
  (when from-vector
    (bind-vector-elements (x y h) from-vector
      (with-class-slot-values utm-to-lat-long-coordinate-transform
	  (southern-hemisphere-p a cm epri ak0 1-es f1 es e2 e3 e4) transform
	(declare (double-float a cm epri ak0 1-es f1 es e2 e3 e4))
	(when southern-hemisphere-p (decf y 1d7)) 
	(decf x 5d5)
	(let* ((em (/ y ak0))
	       (um (/ em (* a f1)))
	       (phi1 (+ um
			(* e2 (sin (the angle (* 2d0 um))))
			(* e3 (sin (the angle (* 4d0 um))))
			(* e4 (sin (the angle (* 6d0 um))))))
	       (sin-phi1 (sin phi1))
	       (cos-phi1 (cos phi1))
	       (tan-phi1 (/ sin-phi1 cos-phi1))
	       (t* (* tan-phi1 tan-phi1))
	       (t2 (* t* t*))
	       (c (* epri (* cos-phi1 cos-phi1)))
	       (1-es-sin-phi^2 (- 1d0 (* es (^2 sin-phi1))))
	       (sqrt-1-es-sin-phi^2 (dsqrt (the (double-float 0.0) 1-es-sin-phi^2)))
	       (1/r (/ (* 1-es-sin-phi^2 sqrt-1-es-sin-phi^2)
		       (* a 1-es)))
	       (en (/ a sqrt-1-es-sin-phi^2))
	       (d (/ x (* en ak0)))
	       (d2 (* d d))
	       (d4 (* d2 d2))
	       (d6 (* d4 d2))
	       (phi (- phi1
		       (* en tan-phi1 1/r
			  (+ (* .5d0 d d)
			     (* (+ 5d0 (* 3d0 t*) (* 10d0 c) (* -4d0 c c) (* -9d0 epri))
				d4 #.(/ -24d0))
			     (* (+ 61d0 (* 90d0 t*) (* 298d0 c) (* 45d0 t2) (* -252d0 epri)
				   (* -3d0 c c))
				d6 #.(/ 720d0))))))
	       (lam (+	; changed minus to plus here to get degrees east. why ??
		     (* cm DOUBLE-pi/180)
		     (/ (+ d
			   (* (+ 1d0 (* 2d0 t*) c) (* d2 d) #.(/ -6d0))
			   (* (+ 5d0 (* -2d0 c) (* 28d0 t*) (* -3d0 c c) (* 8d0 epri)
				 (* 24d0 t2))
			      (* d4 d) #.(/ 120d0)))
			cos-phi1)))
	       #|               (deg-lat (/ phi DOUBLE-pi/180))
	       (deg-long (/ lam  DOUBLE-pi/180))
	       (dlam (* (- cm deg-long) DOUBLE-pi/180))
	       (sin-phi (sin phi))
	       (cos-phi (cos phi ))
	       (conv (* #.(/ DOUBLE-pi/180)
	       dlam
	       (+ sin-phi
	       (* 1.9587d-12 dlam dlam sin-phi cos-phi cos-phi))))
	       |#
	       (foo (abs 2.0))	; this is sufficient to patch around register allocator bug
	       )
	  (declare (type angle phi1 phi lam))
	  (declare (double-float em um sin-phi1 cos-phi1 tan-phi1 t* t2 c 
				 1-es-sin-phi^2 sqrt-1-es-sin-phi^2 1/r en d d2 d4 d6))
	  
	  (declare (ignore foo))
	  ;; Code generation bug -- probably flt pt register allocation screwed up
	  ;; requires modifications to this function to get correct answers.
	  ;;(break)
	  (inline-set-coordinate-vector-elements to-vector
						 (/ lam DOUBLE-pi/180)
						 (/ phi DOUBLE-pi/180)
						 h)
	  to-vector)))))


#|
(lat-long-to-utm *clarke-1866-ellipsoid* 48.0 6.0 3.0)
(lat-long-to-utm *clarke-1866-ellipsoid2* 48.0 6.0 3.0)

(mv-bind (x y) (lat-long-to-utm *clarke-1866-ellipsoid2* 48.0 6.0 3.0)
(utm-to-lat-long2 *clarke-1866-ellipsoid2* x y 3.0))

(mv-bind (x y) (lat-long-to-utm *clarke-1866-ellipsoid* 48.0 6.0 3.0)
(utm-to-lat-long *clarke-1866-ellipsoid* x y 3.0))
|#


#|
(defun tst (n)
(mv-bind (x y cm) (lat-long-to-utm *clarke-1866-ellipsoid* 48.0 6.0 3.0)
(time (loop repeat n do (utm-to-lat-long *clarke-1866-ellipsoid* x y cm)))))
(tst 10000)		; 326 usec 48 conses
|#


#|
(mv-setq (x y cm conv)
(lat-long-to-utm *clarke-1866-ellipsoid* (from-deg-min-sec 48 ) 6d0 3d0))

(mv-setq (lat long)
(utm-to-lat-long *clarke-1866-ellipsoid* x y 3d0))

(lat-long-to-utm *clarke-1866-ellipsoid* lat long 3d0)
(lat-long-to-utm *clarke-1866-ellipsoid2* lat long 3d0)

(utm-to-lat-long *clarke-1866-ellipsoid* x y 3d0)
(utm-to-lat-long *clarke-1866-ellipsoid2* x y 3d0)
|#

#|
(defun tst (n)
(time (loop repeat n do (lat-long-to-utm *clarke-1866-ellipsoid* 48.0 6.0 3.0))))
(tst 10000)		; 314 usec 64 conses
|#

;;;  *******************   LAMBERT-CONFORMAL-CONIC-STRUCT STATE PLANE   *************

(defstruct-class lambert-conformal-conic-coordinate-system (lat-long-coordinate-system)
  ((lcc-struct :initform nil :initarg :lcc-struct :accessor lambert-conformal-conic-struct)
   (lat-long-coordinate-system :initform nil :initarg :lat-long-coordinate-system
			       :accessor lat-long-coordinate-system)
   (lcc-to-lat-long-transform :initform nil :initarg :lcc-to-lat-long-transform :accessor lcc-to-lat-long-transform)
   (lcc-to-geocentric-transform :initform nil :initarg :lcc-to-geocentric-transform :accessor lcc-to-geocentric-transform)
   ))

;;;(defmethod object-to-superior-transform ((cs lambert-conformal-conic-coordinate-system))
;;;  (lat-long-coordinate-system cs))

;;; Wed May 26 2004 -- this is wrong:  lambert-conformal-conic-coordinate-system doesn't have
;;; a geocentric-coordinate-system slot.
#+never
(defmethod initialize-instance :after ((coordinate-system lambert-conformal-conic-coordinate-system)
				       &key &allow-other-keys)
  (setf (geocentric-coordinate-system coordinate-system)
	(geocentric-coordinate-system (lat-long-coordinate-system coordinate-system))))

(defun-cached make-lambert-conformal-conic-coordinate-system
    (lambert-conformal-conic-struct lat-long-to-geocentric-transform)
  (let* ((lat-long-coordinate-system (from-coordinate-system lat-long-to-geocentric-transform))
	 (name (lambert-conformal-conic-struct-zone-name lambert-conformal-conic-struct)))
    (make-coordinate-system
     name 3
     :lcc-struct lambert-conformal-conic-struct
     :class 'lambert-conformal-conic-coordinate-system
     :lat-long-coordinate-system  lat-long-coordinate-system
     :component-names '("LCCx" "LCCy" "LCCz") :component-units 'meters
     )))

(defstruct-class lambert-conformal-conic-struct (base-struct-class)
  (zone-name
   t1					; false easting at central meridian
   t2					; central meridian longitude in seconds (west positive)
   t3					; map radius of central parallel (phi0)
   t4					; map radius of "lowest" parallel in table plus false northing, if any.
   t5					; scale of projection on central parallel
   t6					; cone constant (l)
   t7					; degrees and minutes portion of rectifying latitude in minutes
   t8					; seconds portion of rectifying latitude
   t9					; (1/(6R0N0))*10d16
   t10					; (tan(phi0)/(24*(R0N0)^(3/2))) * 10d24
   t11					; ((5+3*tan(phi0))/(120*R0*N0^3)) * 10d32
   ))

#| where

	       R0 = a(1-e^2) / sqrt (1-e^2*(sin(phi0)^2))
 
	       N0 = a / (1-e^2*(sin(phi0)^2))^(3/2)
   
	       a = semi-major axis
	       e = eccentricity

	       |#

(defmethod initialize-instance :after ((self lambert-conformal-conic-struct) &key coeffs &allow-other-keys)
  (with-class-slots
      lambert-conformal-conic-struct (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11) self
      (destructuring-bind (t1x t2x t3x t4x t5x t6x t7x t8x t9x t10x t11x) coeffs
	  (setf t1 t1x t2 t2x t3 t3x t4 t4x t5 t5x t6 t6x t7 t7x t8 t8x t9 t9x t10 t10x t11 t11x))))
		     
(defun-cached make-lambert-conformal-conic-struct
    (&key zone-name coeffs &allow-other-keys)
  (make-instance 'lambert-conformal-conic-struct :zone-name zone-name :coeffs coeffs))

#|
	       (defun make-lambert-conformal-conic (&key zone-name coeffs &allow-other-keys)
(destructuring-bind (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11) coeffs
(make-instance 'lambert-conformal-conic-struct
:zone-name zone-name
:t1 t1 :t2 t2 :t3 t3 :t4 t4 :t5 t5
:t6 t6 :t7 t7 :t8 t8 :t9 t9 :t10 t10 :t11 t11)))
	       |#

;;; NEED TO FIX THIS FOR ALLEGRO -- NO type-reduce
(defun lambert-conformal-conic-lat-long-to-state-plane-transform-vector
    (transform from-vector to-vector)
  ;; had to flip sign of longitute to be consistant with East positive.
  (declare (type (or null (simple-array double-float (*))) from-vector to-vector))
  (when from-vector
    (with-class-slot-values
	functional-coordinate-transform ((lambert-conformal-conic functional-transform-info)) transform
      (bind-vector-elements (long lat h) from-vector
	(setq long (- long))
	(with-class-slots
	    lambert-conformal-conic-struct (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11) lambert-conformal-conic
	  (let* ((c1 101.2794065d0)
		 (c3 1052.893882d0)
		 (c4 4.483344d0)
		 (c5 .02352d0)
		 (/3600 (/ 3600d0))
		 (phi (* lat DOUBLE-pi/180))
		 (sin-phi (sin phi))
		 (cos-phi (cos phi))
		 (cos-phi^2 (^2 cos-phi))
		 (temp (* cos-phi sin-phi (- c3 (* (- c4 (* c5 cos-phi^2)) cos-phi^2)))) 
		 (s (* c1 (+ (* 60d0 t7) t8 (* -3600d0 lat) temp)))
		 (temp (* s 1.0d-8))
		 (temp1 (+ t9 (- (* t10 temp)) (* t11 (* temp temp))))
		 (temp (+ 1d0 (* temp1 temp temp)))
		 (r (+ t3 (* s t5 temp)))
		 (theta (* t6 (- (* t2 /3600) long) DOUBLE-pi/180))
		 (x (+ (* r (sin theta)) t1))
		 (sin-theta/2 (sin (* .5 theta)))
		 (y (+ t4 (- r) (* 2d0 r (* sin-theta/2 sin-theta/2))))
		 ;;(conv (* t6 (- (* t2 /3600) long)))
		 )
	    (set-coordinate-vector-elements to-vector x y h)	  
	    to-vector))))))

;;; NEED TO FIX THIS FOR ALLEGRO -- NO type-reduce
(defun lambert-conformal-conic-state-plane-to-lat-long-transform-vector (transform from-vector to-vector)
  (declare (type (or null (simple-array double-float (*))) from-vector to-vector))
  (when from-vector 
    (with-class-slot-values
	functional-coordinate-transform ((lambert-conformal-conic functional-transform-info)) transform
      (bind-vector-elements (x y h) from-vector
	(with-class-slots
	    lambert-conformal-conic-struct (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11) lambert-conformal-conic
	  (let* ((rad-per-sec (/ DOUBLE-pi/180 3600d0))
		 (c6 1d8)
		 (c7 9.873675553d-3)
		 (c8 1047.54671d0)
		 (c9 6.19276d0)
		 (c10 .050912d0)
		 (/3600 (/ 3600d0))
		 (theta0 (atan (- x t1) (- t4 y)))
		 (theta (if (> theta0 DOUBLE-pi)
			    (- theta0 DOUBLE-2pi)
			    theta0))
		 (long (- (* t2 /3600)  
			  (/ theta (* DOUBLE-pi/180 t6))))
		 (r (/ (- t4 y) (cos theta)))
		 (sin-theta/2 (sin (* theta .5d0)))
		 (y-p (- y (* 2d0 r (* sin-theta/2 sin-theta/2))))
		 (sappp (/ (- t4 t3 y-p) t5))
		 )
	    (loop repeat 10
		  ;; Why must I declare the next a double float to get flonum optimization?
		  ;; Without this decaration, (abs (- s sapp)) causes flonum consing
		  for sapp double-float = sappp then s
		  for temp1 = (/ sapp c6)
		  for temp2 = (* temp1 temp1)
		  for temp = (+ (* t9 temp2) (* (- t10) (* temp1 temp2)) (* t11 (* temp2 temp2)))
		  for s = (/ sappp (+ 1d0 temp))
		  when (<= (abs (- s sapp)) .1d0) ; what is this magic number ?
		    do (setq s (- sappp (* sappp temp (- 1d0 temp))))
		       (when (<= (abs (- s sapp)) .01d0) ; what is this magic number ?
			 (let* ((wsec (+ (* 60d0 t7) t8 (* (- c7) s)))
				(w (* wsec rad-per-sec))
				(sin-w (sin w))
				(cos-w (cos w))
				(cos-w^2 (* cos-w cos-w))
				(lat (* (+ wsec (* sin-w cos-w
						   (+ c8 (* cos-w^2
							    (+ c9 (* c10 cos-w^2))))))
					/3600))
				;;(conv (* t6 (- (* t2 /3600) long)))
				)
			   (set-coordinate-vector-elements to-vector
							   (- long) ; sign flipped from USGS code -- why ???
							   lat
							   h)
			   (return to-vector)))
		  finally (error "Error Lambert-Conformal-Conic"))))))))

#|
	       (setq calif-zone-3-2-cs
(make-lambert-conformal-conic-coordinate-system
(make-lambert-conformal-conic-struct
:zone-name "Calif Zone 3"
:coeffs '(2d6 433800d0 .2705747585d8
.2751299204d8 .9999291792d0 .6122320427d0 2256d0
35.52018d0 3.812650d0 3.529980d0 0.0))
(get-gdc 'NAD27)
))

	       (setq calif-zone-3-2-to-lat-long-transform
(make-lcc-to-lat-long-transform calif-zone-3-2-cs
:to-coordinate-system (get-gdc 'nad27) )) 

	       (setq calif-zone-3-2-to-utm-transform
(make-composite-coordinate-transform
(list calif-zone-3-2-to-lat-long-transform
(make-lat-long-to-utm-transform
(get-gdc 'NAD27)
(make-utm-coordinate-system (central-meridian-from-utm-zone 10)
(get-gdc 'NAD27)))))

(transform-vector calif-zone-3-2-to-utm-transform (coordinate-vector 1.5e6 .28e6 0.0))

;; 569343.9771484279 4123400.0638161944 ; pt near Portola State Park headquarters

(mv-setq (x y) (lat-long-to-state-plane calif-zone-3-2 37.5d0 -119.75d0 ))

(vector-elements (transform-vector calif-zone-3-2-to-lat-long-transform
(coordinate-vector 2.268144d6 .449421d6 0.0)))
;;; -119.57279588900896 37.73068165081891
|# 
 ;;; ***************    MISC COMPOSITE TRANSFORMS   ***************

(defgeneric (setf geocentric-coordinate-system) (coordinate-system geo-coordinate-system))

;;; The class definition for LOCAL-VERTICAL-COORDINATE-SYSTEM has been moved to 
;;; coordinate-transforms.lisp

(defmethod lat-long-to-geocentric-transform
	   ((coordinate-system local-vertical-coordinate-system))
  (car (transform-list
	(inverse-transform (lvcs-to-lat-long-transform coordinate-system)))))

#|  Ellipse Tangent

ellipse eqn:  (x/a)^2 + (y/b)^2 -1 = 0

x(s) = a * cos(s),  y(s) = b * sin(s)
x'(s) = -a * sin(s), y'(s) = b * cos(s)

tan phi(s) = y'(s)/x'(s) = (b * cos(s)) / (- a * sin(s)) = - (b/a)^2 * x(s)/y(s)

|#

(defvar *tmp-transform*
  (make-functional-coordinate-transform 'lat-long-to-geocentric-transform-vector
					nil))

;;;(defun make-lvcs-to-geocentric-transform-matrix
;;;    (&key origin-lat origin-long (origin-elev 0.0)
;;;     (local-ccw-z-rotation 0.0)
;;;     (local-units-per-meter 1.0)
;;;     gdc
;;;     &allow-other-keys)
;;;  (bind-vector-elements (gx gy gz)
;;;      (transform-vector (to-geocentric-transform gdc)
;;;                        ;;(set-functional-transform lat-long-to-geocentric-transform-vector ellipsoid)
;;;                        (cv origin-long origin-lat origin-elev))
;;;    (let* ((phi (radians (- 90.0 origin-lat)))
;;;           ;; Interesting algebraic relation here:: phi = (- 90.0 origin-lat)
;;;           ;; This is because 1-es = (^2 (/ b a)) and (/ gz (sqrt (+ (^2 gx) (^2 gy)))) = (* 1-es (tan origin-lat))
;;;           ;; Work the algebra  ... obvious --- by the definition of GEODETIC LATITUDE,
;;;           ;; this relation holds.
;;;           (matrix
;;;            (make-coord-frame-matrix
;;;             gx gy gz
;;;             :z-rot (* (+ 90.0 origin-long) (/ pi 180.0)) ; rotate x,y,z to 0,y',z
;;;             :x-rot phi                 ; rotate 0,'y',z to 0,0,z'
;;;             :z-rot (* (/ pi 180.0) local-ccw-z-rotation)
;;;             )))
;;;      (scale-upper-3x3-matrix matrix (/ 1.0d0 local-units-per-meter ))
;;;      matrix
;;;      )))

#|
;;(apply 'make-lvcs-to-geocentric-transform-matrix *foo*)
;;(setq *foo* nil)
(to-geocentric-transform (getf *foo* :gdc))
(transform-vector (to-geocentric-transform (getf *foo* :gdc)) (cv -105.12083333333332 39.5 0.0))
(setq *to-geo-xform* (to-geocentric-transform (getf *foo* :gdc)))
(with-class-slots coordinate-transform (transform-function) (to-geocentric-transform (getf *foo* :gdc))
	  transform-function)
(funcall #'LAT-LONG-TO-GEOCENTRIC-TRANSFORM-VECTOR
	 (to-geocentric-transform (getf *foo* :gdc))
	 (cv -105.12083333333332 39.5 0.0)
	 (make-coordinate-vector 3))
(bind-vector-elements (deg-longitude deg-latitude h) (cv -105.12083333333332 39.5 0.0) (values deg-longitude deg-latitude h))
|#

(defun make-lvcs-to-geocentric-transform-matrix
    (&rest args &key origin-lat origin-long (origin-elev 0.0)
     (local-ccw-z-rotation 0.0)
     (local-units-per-meter 1.0)
     gdc
     &allow-other-keys)
  (setq *foo* args)
  (let* ((gcc-position (transform-vector (to-geocentric-transform gdc)
					 (cv origin-long origin-lat origin-elev)))
	 (foo2 (setq *foo2* gcc-position))
	 (phi (radians (- 90.0 origin-lat)))
	 ;; Interesting algebraic relation here:: phi = (- 90.0 origin-lat)
	 ;; This is because 1-es = (^2 (/ b a)) and (/ gz (sqrt (+ (^2 gx) (^2 gy)))) = (* 1-es (tan origin-lat))
	 ;; Work the algebra  ... obvious --- by the definition of GEODETIC LATITUDE,
	 ;; this relation holds.
	 (matrix
	  (make-object-to-parent-matrix gcc-position
					:z-rot (* (+ 90.0 origin-long) (/ pi 180.0)) ; rotate x,y,z to 0,y',z
					:x-rot phi ; rotate 0,'y',z to 0,0,z'
					:z-rot (* (/ pi 180.0) local-ccw-z-rotation)
					)))
      (scale-upper-3x3-matrix matrix (/ 1.0d0 local-units-per-meter ))
      matrix))

#|
(eval-cache-flush-function 'make-lvcs-to-geocentric-transform)

(eval-cache-flush-function 'make-lvcs)
|#

;;;(defun make-lvcs (ellipsoid lat long &key vertical-datum local-units-per-meter (origin-elev 0.0)
;;;                  (cs-class 'local-vertical-coordinate-system))
;;;  (unless (typep ellipsoid 'ellipsoid) (setq ellipsoid (get-ellipsoid ellipsoid)))
;;;  (unless local-units-per-meter (setq local-units-per-meter 1.0))
;;;  (eval-cache (make-lvcs lat long ellipsoid vertical-datum local-units-per-meter origin-elev)
;;;      (let* ((lvcs (make-coordinate-system (format nil "LVCS ~6,4f ~6,4f ~a" lat long (name ellipsoid))
;;;                                           3
;;;                                           :class cs-class
;;;                                           :component-names '("x" "y" "z")      
;;;                                           :component-units (if (eql local-units-per-meter *feet-per-meter*)
;;;                                                                'feet 'meters)))
;;;             ;(gdc (gdc-coordinate-system ellipsoid))
;;;             (gcc (gcc-coordinate-system ellipsoid))
;;;         
;;;             (transform-matrix (make-lvcs-to-geocentric-transform-matrix
;;;                                :ellipsoid ellipsoid
;;;                                :origin-lat lat
;;;                                :origin-long long
;;;                                :origin-elev origin-elev
;;;                                :local-units-per-meter local-units-per-meter))
;;;             (lvcs-to-gcc (make-instance '4x4-coordinate-transform
;;;                                         :transform-matrix transform-matrix
;;;                                         :from-coordinate-system lvcs
;;;                                         :to-coordinate-system gcc)))
;;;        #+old (setf (geocentric-coordinate-system lvcs) gcc)
;;;        (set-parent-transform lvcs lvcs-to-gcc)
;;;        lvcs)))

;;;(fmakunbound 'set-lvcs-transform)

(defun set-lvcs-transform (lvcs gdc gdc-position local-units-per-meter)
  (bind-vector-elements (long lat origin-elev) gdc-position
    (unless local-units-per-meter (setq local-units-per-meter 1.0))
    (let* ((transform-matrix  
	    ;; FIXME: need to allow for datum containing vertical-datum
	    (make-lvcs-to-geocentric-transform-matrix :gdc gdc
						      :origin-lat lat
						      :origin-long long
						      :origin-elev origin-elev
						      :local-units-per-meter local-units-per-meter))
	   (lvcs-to-gcc (make-instance '4x4-coordinate-transform
				       :transform-matrix transform-matrix
				       :from-coordinate-system lvcs
				       :to-coordinate-system (gcc-coordinate-system (ellipsoid gdc)))))
      (setf (get-prop lvcs :units-per-meter) local-units-per-meter)
      (setf (get-prop lvcs :gdc-origin) gdc-position)
      #+old (setf (geocentric-coordinate-system lvcs) gcc)
      (set-parent-transform lvcs lvcs-to-gcc)
      lvcs)))

(defparameter *units-name-size-alist*
  `((feet ,*feet-per-meter*)
    (us-feet ,*feet-per-meter-us*)
    (international-feet ,*feet-per-meter-international*)
    (meters 1.0)))

(defun quantize (number &optional (quantization 1e-8))
  (* quantization (round number quantization)))

;;; (quantize 1.0000000001 1e-8)

;;;(defun make-lvcs (datum lat long
;;;                  &key name (units 1.0) (origin-elev 0.0) (cs-class 'local-vertical-coordinate-system))
;;;  ;;(when (and lat long datum) (unless (typep datum 'lat-long-coordinate-system) (setq datum (get-ellipsoid datum))))
;;;    
;;;  (unless (numberp units)
;;;    (setq units (or (cadr (assoc units *units-name-size-alist*
;;;                                 :test #'string-equal))
;;;                    (error "Unknown units specification ~a" units))))
;;;
;;;  (unless name (setq name (format nil "~a@~6,4f,~6,4f" (name datum) lat long )))
;;;  (let* ((degrees-quantization 1e-10) ; equivalent to about .1 inch error at the equator.
;;;         (quantized-lat (and lat (quantize lat degrees-quantization)))
;;;         (quantized-long (and long (quantize long degrees-quantization)))
;;;         (quantized-origin-elev (and origin-elev (quantize origin-elev .001))))
;;;    ;;; quantize these numbers to avoid possible rounding errors due to floating pt IO.
;;;    (eval-cache (make-lvcs name quantized-lat quantized-long datum units origin-elev)
;;;        (let ((lvcs (make-coordinate-system 
;;;                     name
;;;                     3
;;;                     :class cs-class
;;;                     :component-names '("x" "y" "z")    
;;;                     :component-units 
;;;                     (cond ((eql units *feet-per-meter*) 'feet)
;;;                           ((eql units 1.0) 'meters)
;;;                           (t (format nil "%f*meters" units))))))
;;;          (when (and datum lat long)
;;;            (set-lvcs-transform lvcs datum (cv long lat origin-elev) units))
;;;          lvcs))))

(defmethod lvcs-gdc-position ((lvcs local-vertical-coordinate-system))
  (and (parent lvcs)
       (transform-vector (to-lat-long-transform lvcs) (cv 0.0 0.0 0.0))))

(defmethod lvcs-gcc-position ((lvcs local-vertical-coordinate-system))
  (and (parent lvcs)
       (transform-vector (to-geocentric-transform lvcs) (cv 0.0 0.0 0.0))))

(defmethod lvcs-gdc-name ((lvcs local-vertical-coordinate-system))
  (and (parent lvcs) 
       (name (ellipsoid (parent lvcs)))))


;;(fmakunbound 'coordinate-vectors-almost-equal)
(defun coordinate-vectors-almost-equal (cv1 cv2 &optional (tolerance 1e-1))
  ;(break)
  (declare (double-float tolerance))
  (flet ((almost-equal (x y) 
	   (< (abs (- x y)) tolerance)))
    (bind-vector-elements (x1 y1 z1) cv1
      (bind-vector-elements (x2 y2 z2) cv2
	(and (almost-equal x1 x2) (almost-equal y1 y2) (almost-equal z1 z2))))))

(defmethod lvcs-position-equal ((lvcs local-vertical-coordinate-system) 
				gdc gdc-position units-per-meter)
  ;(break)
  (let ((lvcs-units-per-meter (get-prop lvcs :units-per-meter)))
    (and (parent lvcs)
	 (equal (name (ellipsoid gdc)) (name (ellipsoid (parent lvcs))))
	 (or (null lvcs-units-per-meter) (equal units-per-meter lvcs-units-per-meter))
	 (coordinate-vectors-almost-equal 
	  (lvcs-gcc-position lvcs)
	  (transform-vector (to-geocentric-transform gdc) gdc-position)))))
     
(defmacro cache-quantized-lvcs-with-position ((datum lat long origin-elev units-per-meter) &body body)
  `(let* ((degrees-quantization 1e-10) ; equivalent to about .1 inch error at the equator.
	  (quantized-lat (and lat (quantize ,lat degrees-quantization)))
	  (quantized-long (and long (quantize ,long degrees-quantization)))
	  (quantized-origin-elev (and ,origin-elev (quantize origin-elev .001))))
    (eval-cache 
	(lvcs-with-position ,datum quantized-lat quantized-long quantized-origin-elev ,units-per-meter)
	,@body)))

(defun-cached lvcs-with-name (name))
(defun-cached lvcs-with-position (datum quantized-lat quantized-long quantized-origin-elev units-per-meter))

#| 
MAKE-LVCS way be called in 3 primary ways:

1. lat, long, datum, and name are all specified: 
Return either a matching eval-cache entry found either by name or position,
or create and return a named LVCS at the specified position.
It is an error if there exists an LVCS with the specified name but a different position
or units-per-meter.

2. lat, long, and datum are all specified but name is NIL.
Return either a matching eval-cache entry found by position 
or create and return a unnamed LVCS at the specified position.

3. name is specified, but one of lat, long, or datum is NIL.
Return either a matching eval-cache entry found by name
or create and return a named LVCS with no position.

Whenever an LVCS is created or matching LVCS is found, lvcs-with-name or lvcs-with-position
eval-cache entries are made as appropriate.
|#
(defun make-lvcs (gdc lat long &key (origin-elev 0.0) (elev origin-elev) (units 1.0) name 
		  (cs-class 'local-vertical-coordinate-system))
  (let* ((units-per-meter (if (numberp units) units 
			      (or (cadr (assoc units *units-name-size-alist*
					       :test #'string-equal))
				  (error "Unknown units specification ~a" units))))
	 (have-position (and lat long gdc))
	 (gdc-position (and have-position (cv long lat elev)))
	 (have-name name)
	 (lvcs-with-name (when have-name (lx::eval-cache-probe (lvcs-with-name name))))
	 (lvcs-with-position 
	  (when have-position 
	    (lx::eval-cache-probe 
	      (cache-quantized-lvcs-with-position (gdc lat long elev units-per-meter)))))
	 ;(lvcs-with-position-name (and lvcs-with-position (name lvcs-with-position)))
	 ;(lvcs-name-match (and lvcs-with-position-name (string-equal lvcs-with-position-name name)))
	 )
    ;;(setq *foo* (list name have-position lvcs-with-name lvcs-with-position)) (break)
    (cond ((and lvcs-with-name (eq lvcs-with-name lvcs-with-position))
	   lvcs-with-name)                ; everything matches, return it
	  ;;
	  ((and lvcs-with-name (parent lvcs-with-name)) ; it has a position
	   (when (and have-position 
		      (not (lvcs-position-equal lvcs-with-name gdc gdc-position units-per-meter)))
	     (error "MAKE-LVCS found another LVCS with the same name at a different position ~a ~a"
		    lvcs-with-name gdc-position))
	   lvcs-with-name)		; return it
	  ;;
	  (lvcs-with-name	  ; it has a name, but it didn't have a position
	   (when have-position
	     (set-lvcs-transform lvcs-with-name gdc gdc-position units-per-meter)
	     (cache-quantized-lvcs-with-position (gdc lat long elev units-per-meter) 
	       lvcs-with-name))
	   lvcs-with-name)
	  ;;
	  (lvcs-with-position		; it has a position
	   (when have-name
	     ;; what should be done with multiple names for the name lvcs?
	     (eval-cache (lvcs-with-name name) lvcs-with-position)) ; cache it with name
	   lvcs-with-position)		; return it 
	  ;;
	  ((not (or have-name have-position))
	   (error "MAKE-LVCS called without specifying a name or a position"))
	  ;;
	  (t				; couldn't find by name or position
	   (let ((lvcs (make-coordinate-system 
			name
			3
			:class cs-class
			:component-names '("x" "y" "z")	
			:component-units 
			(cond ((eql units-per-meter *feet-per-meter*) 'feet)
			      ((eql units-per-meter 1.0) 'meters)
			      (t (format nil "~f*meters" units-per-meter))))))
	     (when have-position
	       (set-lvcs-transform lvcs gdc gdc-position units-per-meter))
	     (setf (name lvcs) (or name (format nil "~a@~6,4f,~6,4f" (name gdc) lat long )))
	     (eval-cache (lvcs-with-name name) lvcs)
	     (cache-quantized-lvcs-with-position (gdc lat long elev units-per-meter) 
	       lvcs))))))
	     
#|
(eval-cache-flush-function 'lvcs-with-name)
(eval-cache-flush-function 'lvcs-with-position)

(list (LVCS (GDC NAD27) 39.5 -105.12083333333332 :units feet)
(LVCS (GDC NAD27) 39.5 -105.12083333333332 :units feet :name "alv1")
(LVCS :name "alv1")
)

(LVCS (GDC NAD27) 39.5 -105.12083333333332 :name "alv1") ; error 
(LVCS (GDC NAD27) 39.5 -105.1209 )	; ok 
(list (LVCS (GDC NAD27) 39.5 -105.12083333333332 :units feet :name "alv2")
(LVCS :name "alv2"))			; ok

(let ((lvcs (LVCS (GDC NAD27) 39.5 -105.12083333333332 :units feet)))
(transform-vector (transform-path lvcs (GDC NAD27)) (cv 0.0 0.0 0.0)))

(let ((lvcs (LVCS (GDC NAD27) 39.5 -105.12083333333332 :units feet)))
(transform-vector (to-lat-long-transform lvcs) (cv 0.0 0.0 0.0)))

#(-105.12083333333332 39.5 -4.3382897274568677e-10)
(describe (LVCS :name "foo"))
|#

;;;(defmacro local-vertical-coordinate-system
;;;    (lat long ellispoid-name &key vertical-datum units origin-elev name)
;;;  `(make-lvcs ',ellispoid-name ,lat ,long
;;;    ,@(when vertical-datum `(:vertical-datum ',vertical-datum))
;;;    ,@(when units `(:units ',units))
;;;    ,@(when origin-elev `(:origin-elev ,origin-elev))
;;;    ,@(when name `(:name ,name))
;;;    ))


;;; 3 varient forms: (lvcs <geodetic-cs-spec> lat long . key-vals)
;;;            (lvcs <geodetic-cs-spec> . key-vals)
;;;   and      (lvcs . key-vals)
(defmacro local-vertical-coordinate-system (&rest args)
  (let (geodetic-cs-spec lat0 long0)
    (if (keywordp (car args))
	(destructuring-bind (&key gdc ellipsoid vertical-datum lat long &allow-other-keys) args
	  (setq geodetic-cs-spec (or gdc `(gdc ,(or ellipsoid "WGS84") ,@(when vertical-datum `(,vertical-datum))))
		lat0 lat long0 long))
	(cond ((and (>= (length args) 3) (numberp (cadr args)) (numberp (caddr args)))
	       (setq geodetic-cs-spec (pop args)
		     lat0 (pop args)
		     long0 (pop args)))
	      ((and (>= (length args) 1) (consp (car args)) (eq (caar args) 'gdc))
	       (setq geodetic-cs-spec (pop args))
	       (destructuring-bind (&key lat long &allow-other-keys) args
		 (setq lat0 lat long0 long)))))

    (destructuring-bind (&key units origin-elev elev name &allow-other-keys) args
      `(make-lvcs ,geodetic-cs-spec ,lat0 ,long0
	;;,@(when vertical-datum `(:vertical-datum ',vertical-datum))
	,@(when units `(:units ',units))
	,@(when origin-elev `(:elev ,origin-elev))
	,@(when elev `(:elev ,elev))
	,@(when name `(:name ,name))
	))))


(defmacro lvcs (&rest args)
  `(local-vertical-coordinate-system . ,args))

;(setq *lvcs* (lvcs :gdc (gdc wgs84) :name "foo" :lat 25.0 :long -135.0 :units :feet))
;(setq *lvcs* (lvcs (gdc wgs84) :name "foo" :lat 25.0 :long -135.0 :units :feet))
;(setq *lvcs* (lvcs :name "foo"))
;(setq *lvcs* (lvcs :gdc (gdc wgs84) :name "foo" :lat 25.0 :long -135.0 :elev 0.0 :units :feet))
;(describe *lvcs*)


;;; (Sun Sep 12 2004) Old version for backward compatibility only.
;;; This is all screwed up.  The caching is wrong.
(defun make-lvcs-to-geocentric-transform
    (&rest args
     &key
     LVCS-COORDINATE-SYSTEM ; backward compatibility with old 3d-world files.
     (local-vertical-coordinate-system LVCS-COORDINATE-SYSTEM)
     origin-lat
     origin-long
     (origin-elev 0.0)
     (local-ccw-z-rotation 0.0)         ; angle between local y-axis and true North.
     (local-units-per-meter 1.0)
     lat-long-to-geocentric-transform 
     (lat-long-coordinate-system (from-coordinate-system lat-long-to-geocentric-transform))
     (geocentric-coordinate-system (to-coordinate-system lat-long-to-geocentric-transform))
     transform-matrix)
  ;;(break)    
  (unless local-vertical-coordinate-system
    (setq local-vertical-coordinate-system
          (make-coordinate-system "? LVCS" 3
                                  :class 'local-vertical-coordinate-system
                                  :component-names '("x" "y" "z")
                                  :component-units (if (= local-units-per-meter *feet-per-meter*)
                                                       'feet 'meters))))
  (setf (get-prop local-vertical-coordinate-system :origin-lat) origin-lat
        (get-prop local-vertical-coordinate-system :origin-long) origin-long
        (get-prop local-vertical-coordinate-system :origin-elev) origin-elev)
  (unless (= local-ccw-z-rotation 0.0)
    (setf (get-prop local-vertical-coordinate-system :local-ccw-z-rotation) local-ccw-z-rotation))
  (unless (= local-units-per-meter 1.0)
    (setf (get-prop local-vertical-coordinate-system :local-units-per-meter)
          local-units-per-meter))
  
  
  (let* ((transform-matrix
          (or transform-matrix
              (apply 'make-lvcs-to-geocentric-transform-matrix
                     :local-units-per-meter
                     (or local-units-per-meter
                         (error "Local-units-per-meter not specified")
                         ;;(local-units-per-meter local-vertical-coordinate-system)
                         )
		     :gdc lat-long-coordinate-system
                     args )))
         
         (lvcs-to-geocentric-transform
          (eval-cache (make-lvcs-to-geocentric-transform
                       transform-matrix
                       local-vertical-coordinate-system
                       geocentric-coordinate-system)
              (make-instance '4x4-coordinate-transform
               ;;'cached-4x4-coordinate-transform
               :transform-matrix transform-matrix
               :from-coordinate-system local-vertical-coordinate-system
               :to-coordinate-system geocentric-coordinate-system
               ))))
    #+maybe
    (setf (from-coordinate-system lvcs-to-geocentric-transform) local-vertical-coordinate-system
          (to-coordinate-system lvcs-to-geocentric-transform) geocentric-coordinate-system)
    #+obsolete
    (setf (geocentric-coordinate-system local-vertical-coordinate-system) geocentric-coordinate-system)
    #+obsolete
    (setf (lvcs-to-geocentric-transform local-vertical-coordinate-system) lvcs-to-geocentric-transform)
    ;(setq *foo* (list local-units-per-meter local-vertical-coordinate-system))
    lvcs-to-geocentric-transform
    ))

(defun-cached make-lat-long-to-geocentric-transform
    (lat-long-coordinate-system
     geocentric-coordinate-system
     &key (create-inverse t)
     (ellipsoid (ellipsoid lat-long-coordinate-system))) 
  (let ((lat-long-to-geocentric-transform
         (make-functional-coordinate-transform
          'lat-long-to-geocentric-transform-vector
          ellipsoid   
          :from-coordinate-system lat-long-coordinate-system
          :to-coordinate-system geocentric-coordinate-system
          :propertyq-list '(:transform-cost 40.0))))
    #+never
    (setf (lat-long-to-geocentric-transform lat-long-coordinate-system)
          lat-long-to-geocentric-transform
          (lat-long-to-geocentric-transform geocentric-coordinate-system)
          lat-long-to-geocentric-transform)
    
    (when create-inverse
      (setf (inverse-transform lat-long-to-geocentric-transform)
            (make-functional-coordinate-transform
             'geocentric-to-lat-long-transform-vector
             ellipsoid
             :inverse-transform lat-long-to-geocentric-transform
             :from-coordinate-system geocentric-coordinate-system
             :to-coordinate-system lat-long-coordinate-system
             :property-list '(:transform-cost 360.0)))
      (connect-transforms lat-long-to-geocentric-transform
                          (inverse-transform lat-long-to-geocentric-transform)
                          lat-long-coordinate-system geocentric-coordinate-system))
    lat-long-to-geocentric-transform))

(defun-cached make-lat-long-to-utm-transform
    (lat-long-coordinate-system
     utm-coordinate-system
     &key (create-inverse t))
  (let* (;;(ellipsoid (ellipsoid lat-long-coordinate-system))
	 (lat-long-to-utm-transform
	 (make-instance 'lat-long-to-utm-coordinate-transform
			;;:ellipsoid ellipsoid
			:from-coordinate-system lat-long-coordinate-system
			:to-coordinate-system utm-coordinate-system
			:transform-function 'lat-long-to-utm-transform-vector)))
    (when create-inverse
      (setf (inverse-transform lat-long-to-utm-transform)
	    (make-instance 'utm-to-lat-long-coordinate-transform
			   ;;:ellipsoid ellipsoid
			   :from-coordinate-system utm-coordinate-system
			   :to-coordinate-system lat-long-coordinate-system
			   :transform-function 'utm-to-lat-long-transform-vector))
      (connect-transforms lat-long-to-utm-transform (inverse-transform lat-long-to-utm-transform)
			  lat-long-coordinate-system utm-coordinate-system)
      )
    lat-long-to-utm-transform))

(defun-cached make-lcc-to-lat-long-transform
    (lcc-coordinate-system &key lat-long-coordinate-system 
     (create-inverse t))
  (let ((lcc-to-lat-long-transform
	 (make-functional-coordinate-transform
	  'lambert-conformal-conic-state-plane-to-lat-long-transform-vector
	  (lambert-conformal-conic-struct lcc-coordinate-system)
	  :to-coordinate-system lat-long-coordinate-system
	  :from-coordinate-system lcc-coordinate-system)))
    (setf (lcc-to-lat-long-transform lcc-coordinate-system) lcc-to-lat-long-transform)
    (when create-inverse
      (setf (inverse-transform lcc-to-lat-long-transform)
	    (make-functional-coordinate-transform
	     'lambert-conformal-conic-lat-long-to-state-plane-transform-vector
	     (lambert-conformal-conic-struct lcc-coordinate-system)
	     :inverse-transform lcc-to-lat-long-transform
	     :to-coordinate-system lcc-coordinate-system
	     :from-coordinate-system lat-long-coordinate-system)))
    lcc-to-lat-long-transform))



;;; quick and dirty version for world-map
;;; from-vector = (deg-longitude deg-latitude )
;;; to-vector = (x y z)
;;; NEED TO FIX THIS FOR ALLEGRO -- NO type-reduce
(defun quick-lat-long-to-geocentric-transform-vector (transform from-vector to-vector)
  ;;#+cmu (declare (ext::optimize-interface (speed 3) (safety 0)))
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (or null (simple-array double-float (*))) from-vector to-vector))
  (declare (inline sin cos))
  (when from-vector
    (with-class-slot-values
	functional-coordinate-transform ((ellipsoid functional-transform-info)) transform
      (bind-vector-elements (deg-longitude deg-latitude ) from-vector
	(with-class-slot-values
	    ellipsoid (a) ellipsoid
	  (let* ((rad-latitude (* double-pi/180 deg-latitude))
		 (rad-longitude (* double-pi/180 deg-longitude)) 
		 (sin-latitude (sin rad-latitude))
		 (xy (* a (cos rad-latitude)))
		 )
	    (declare (type double-float rad-latitude rad-longitude sin-latitude xy))
	    (inline-set-coordinate-vector-elements to-vector
					    (* xy (cos rad-longitude))
					    (* xy (sin rad-longitude))
					    (* a sin-latitude))
	    to-vector))))))

(declaim (special *CLARKE-1866-ELLIPSOID*))

(defun make-quick-lat-long-to-geocentric-transform
    (lat-long-coordinate-system
     geocentric-coordinate-system
     &key (ellipsoid *CLARKE-1866-ELLIPSOID*))
  (let ((lat-long-to-geocentric-transform
	 (make-functional-coordinate-transform 'quick-lat-long-to-geocentric-transform-vector
					       ellipsoid   
					       :from-coordinate-system lat-long-coordinate-system
					       :to-coordinate-system geocentric-coordinate-system
					       :property-list '(:transform-cost 40.0))))
    ;;(setf (lat-long-to-geocentric-transform lat-long-coordinate-system) lat-long-to-geocentric-transform)
    lat-long-to-geocentric-transform))

