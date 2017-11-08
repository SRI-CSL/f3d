(IN-PACKAGE :transforms)

;;;
;;; This has been imported from the TIME subsystem - need to make
;;; timebase representations as rigorous as coordinate
;;; representations.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Temporal transforms: every temporal object should have a notion of
;;; time-base that pins down the object's temporal frame of reference.
;;; The temporal transform hierarchy should reflect the spatial
;;; transform hierarchy.  Objects' time bases can be 0-based and
;;; expressed in milliseconds (or other units!).  These time bases are
;;; nested within a temporal coordinate frame that expresses time and
;;; date in site timezone units, which is further nested within a
;;; geocentric temporal coordinate frame that represents UTC.
;;; Likewise, a variety of different dating systems can be expressed,
;;; depending on cultural calendars.  The default for geocentric (UTC)
;;; date is Lisp universal time.
;;; 
;;; The temporal transform tree I describe above is isomorphic in some
;;; sense to the spatial transform tree, but spatial and temporal
;;; transforms are independent and should remain so.
;;;
;;; Every temporal object must have a slot (time-parent) that
;;; expresses the temporal relationship between it and its parent.  As
;;; with spatial transforms, this is a tree that brings the object
;;; temporal frame into a geocentric temporal frame.
;;;

;;; catch-all is effectively an identity transform:
(defmethod transform-time ((transform t) time)  time)

(defmethod inverse-transform-time ((transform t) time)  time)

;;;
;;; base class from which temporal transforms should be constructed:
(defstruct-class temporal-transform (coordinate-transform)
  ((time-unit :initform :milliseconds :initarg :time-unit :accessor time-unit)))


;;;
;;; Have to be careful here - the transform functions need to be
;;; initialized before we can use these:
(defmethod transform-time ((transform temporal-transform) time)
  (with-class-slot-values linear-temporal-transform (transform-function) transform
    (if transform-function
	(funcall transform-function time)
	time
	)))


;;; Why does this depart from the convention used by
;;; coordinate-transform?  We could simply store an inverse-transform

(defmethod inverse-transform-time ((transform temporal-transform) time)
  (with-class-slot-values temporal-transform (inverse-transform) transform
    (transform-time inverse-transform time)))

;;; Temporal transforms map one timebase to another.  They should
;;; support the conversion from frame numbers to true timestamps.

;;; temporal-transforms come in two basic flavors:

;;; 1) continuous transforms - for example, video is often collected
;;; isochronously, so that there is a simple linear relationship
;;; between frame number and time, or across timebases.

;;; 2) sampled transforms - in this case, we have a record of
;;; timestamps corresponding to frame numbers, so the conversion
;;; requires a lookup table.  The WORLD-LINE object implements this
;;; kind of scheme.

(defstruct-class timestamp-shift-transform (temporal-transform)
  ((offset :initform 0 :accessor temporal-transform-offset :initarg :offset)
   ))


(defstruct-class linear-temporal-transform (temporal-transform)
  ((scale :initform 1.0 :accessor temporal-transform-scale :initarg :scale) ;; scale, then offset.
   (offset :initform 0 :accessor temporal-transform-offset :initarg :offset)
   ))

;;; Ultimately should not be cached:
(defun-cached temporal-transform-matrix-internal (x)
  (let ((m (make-4x4-identity-matrix)))
    (setf (aref m 0 0) (temporal-transform-scale x))
    (setf (aref m 0 3) (temporal-transform-offset x))
    m))

(defmethod transforms::transform-matrix ((xf linear-temporal-transform))
  (temporal-transform-matrix-internal xf))

;;; Here we should check the type of the offset to see how the
;;; arithmetic should be handled.  Many timestamps are expressed as
;;; 64-bit unsigned integers, so conversion to float is not really
;;; desirable.  Where integer arithmetic is required, it may be
;;; necessary to express scale and offset as integral bit shifts and
;;; integer offsets, perhaps using a different class of transform:

(defun make-linear-temporal-inverse-transform (xf offset scale)
  (make-instance 'linear-temporal-transform
		 :offset (- (/ offset scale))
		 :scale (/ 1.0 scale)
		 :inverse-transform xf))


(defmethod set-transform-parameters ((xf linear-temporal-transform) &rest args)
  (destructuring-bind (off sc) args
      (with-class-slots linear-temporal-transform (offset scale transform-function) xf
	  (setf offset off)
	  (setf scale sc))))


(defmethod set-transform-function ((xf linear-temporal-transform))
  (with-class-slots linear-temporal-transform (offset scale transform-function) transform
     (setf transform-function #'(lambda (x) (+ offset (* scale x))))))
  


(defmethod update-transform ((transform linear-temporal-transform))
  (set-transform-function transform)
  (with-class-slots linear-temporal-transform (offset scale inverse-transform) transform
       (set-transform-parameters inverse-transform (- (/ offset scale)) (/ 1.0 scale))))


(defmethod initialize-instance :after ((thing linear-temporal-transform) &rest args)
  (with-slots (offset scale transform-function inverse-transform) thing
      (unless inverse-transform
	(setf inverse-transform
	      (make-linear-temporal-inverse-transform thing offset scale))
	(setf (from-coordinate-system inverse-transform) (to-coordinate-system thing))
	(setf (to-coordinate-system inverse-transform) (from-coordinate-system thing)))
      ))


;;;
;;; Doesn't really use the transform-function.  Why not???


(defun make-time-shift-transform (delta-t &optional (scale 1.0))
  (make-instance 'linear-temporal-transform :offset delta-t :scale scale))
		 

;;;  
(defmethod transform-timestamp ((transform linear-temporal-transform) ms-time)
  (with-slots (offset scale) transform
    (+ (* 1000 (+ offset (* scale (floor ms-time 1000))))
       (* scale (mod ms-time 1000)))))



(defmethod inverse-transform :around ((transform linear-temporal-transform))
  (let ((xf (call-next-method)))
    (setf (from-coordinate-system xf) (to-coordinate-system transform))
    (setf (to-coordinate-system xf) (from-coordinate-system transform))
    xf))
;;;
;;; sampled-temporal-transform: a lookup table between sample numbers
;;; and timestamps.  No interpolation, just nearest-element lookup:
;;;
(defstruct-class sampled-temporal-transform (temporal-transform)
  ((timestamp-array :initform nil :initarg :timestamp-array :accessor timestamp-array)))

(defmethod initialize-instance :after ((self sampled-temporal-transform) &rest args)
  (with-class-slot-values sampled-temporal-transform (timestamp-array) self
    (unless (arrayp timestamp-array)
      (error "You forgot to specify a valid timestamp-array."))))

(defmethod transform-time ((transform sampled-temporal-transform) frameno)
  (with-class-slot-values sampled-temporal-transform (timestamp-array) transform
    (aref timestamp-array (max 0 (min frameno (1- (length timestamp-array)))))))


(defun find-index-bisection (value ts-array &key (start 0) (end (1- (length ts-array))))
  (let ((begin-time (aref ts-array start))
	(end-time (aref ts-array end))
	tmid imid)
    (if (<= value begin-time)
	start
	(if (>= value end-time)
	    end
	    (let ((timestamp value))
	      (loop while (and (< begin-time timestamp end-time) (> (- end start) 2))
		    do
		 (setf imid (floor (+ start end) 2))
		 (setf tmid (aref ts-array imid))
		 (if (<= begin-time timestamp tmid)
		     (setf end-time tmid end imid)
		     (if (<= tmid timestamp end-time)
			 (setf begin-time tmid start imid)
			 (error "How did I get here??"))))
	      (if (< (abs (- timestamp begin-time)) (abs (- timestamp end-time)))
		  (values start begin-time end)
		  (values end end-time start)))))))

(defmethod inverse-transform-time ((transform sampled-temporal-transform) time)
  (find-index-bisection time (timestamp-array transform)))


;;;
;;; The difference here is that we also have an array of frame numbers
;;; that allows us to index arbitrarily into an existing sequence:
;;;

(defstruct-class frame-lookup-temporal-transform (sampled-temporal-transform)
  ((frame-array :initform nil :initarg :frame-array :accessor frame-array)))

(defmethod initialize-instance :after ((self frame-lookup-temporal-transform) &rest args)
  (with-class-slot-values frame-lookup-temporal-transform (frame-array) self
    (unless (arrayp frame-array)
      (error "You forgot to specify a valid frame-array."))))

(defmethod transform-time ((transform frame-lookup-temporal-transform) frameno)
  (with-class-slot-values frame-lookup-temporal-transform (timestamp-array frame-array) transform
    (let* ((minframe (aref frame-array 0))
	   (n (1- (length frame-array)))
	   (maxframe (aref frame-array (1- (length frame-array))))
	   (where  (if (= maxframe minframe)
                       0
                       (/ (dfloat (- frameno minframe))
                          (dfloat (- maxframe minframe))))))
      (if (< where 0.0)
	  (aref timestamp-array 0)
	  (if (> where 1.0)
	      (aref timestamp-array n)
	      (loop with k = (round (* (dfloat n) where))
		    for j from 0 below n
		    for k1 = (max 0 (min n (+ k j)))
		    for k2 = (max 0 (min n (- k j)))
		    do (when (= (aref frame-array k1) frameno)
			 (return (aref timestamp-array k1)))
		       (when (= (aref frame-array k2) frameno)
			 (return (aref timestamp-array k2)))))))))

			 

(defmethod inverse-transform-time ((transform frame-lookup-temporal-transform) time)
  (with-class-slot-values frame-lookup-temporal-transform (frame-array) transform
    (aref frame-array
	  (max 0 (min (1- (length frame-array))
		      (find-index-bisection time (timestamp-array transform)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Temporal coordinate systems define the reference and units for
;;; measuring time.  The default - nil - is the assumed temporal
;;; coordinate frame corresponding to frame numbers.  There are no
;;; "units" to speak of (except "sample number"), and we have no other
;;; information about time.  This can also be a video or un-referenced
;;; track object.  At the other end of the spectrum are fully
;;; qualified temporal references, like "seconds since midnight
;;; 1/1/1970 UTC".  Units can be any standard units of time: seconds,
;;; minutes, etc.  The most common will probably be milliseconds.
;;;
;;;
;;; This is modeled after Quam's coordinate system type hierarchy.
;;; Every temporal object incorporates this class.  While
;;; basic-coordinate-system is redundant, it is necessary for the
;;; parent and children slots.  The idea is that for every spatial
;;; transformation in the tree, there is also a temporal
;;; transformation.

(defstruct-class temporal-coordinate-system (basic-coordinate-system) ; should this be changed to  property-list-struct?
  ((object-to-parent-temporal-transform :initform nil
					:initarg :object-to-parent-temporal-transform
					:accessor object-to-parent-temporal-transform)
   ))


(defstruct-class time-base (temporal-coordinate-system)
  (
   ;; Redundant.  This next is already defined in BASIC-COORDINATE-SYSTEM
   ;; (name :initform nil :initarg :name :accessor name)
   (description :initform "Time base" :initarg :description :accessor time-base-description)
   ))


(defvar *unix-time-base* (make-instance 'time-base :name "Unix Time" :description "Time since midnight, Jan 1, 1970" :units :milliseconds))
(defvar *lisp-universal-time-base* (make-instance 'time-base :name "Lisp Universal Time" :description "Time since midnight, Jan 1, 1900" :units :milliseconds))



(defmethod initialize-instance :after ((self time-base) &rest args &key (offset 0) &allow-other-keys)
  (with-class-slots temporal-coordinate-system (object-to-parent-temporal-transform) self
    (setf object-to-parent-temporal-transform (make-time-shift-transform (or offset 0)))))


(defmethod print-object ((object time-base) stream)
  (with-class-slots time-base (name) object
    (if *print-escape*
	(format stream "#<~A ~a #X~x>" (type-of object) name  (%pointer object))
	(format stream "~a ~a" (type-of object) name ))))


(defmethod dimensionality ((ref time-base))
  1)

;;;
;;; Default method will transform a timestamp from 'local' to 'global' temporal coordinates.
;;;
(defmethod canonical-timestamp ((object time-base) timestamp)
  (round  ;; Forces the value to be an integer...is this always the right thing?
   (transform-timestamp
    (object-to-parent-temporal-transform object)
    timestamp)))


;;;
;;; This needs to be improved:
;;;
(defun unix-to-local-timestamp (time) time)

;;; Until this is better integrated with existing site models, we do
;;; nothing here.  Ultimately, each site has an offset in hours from
;;; UTC, and this initialization should set things up to agree with
;;; that offset.
;;; 
#+never
(defmethod initialize-instance :after ((world time-base) &key &allow-other-keys)
  (set-parent-transform world (lvcs-to-utc-transform world)))



(defmethod absolute-time-transform ((self t)) nil)


;;;


(defvar *30-fps-frames-to-relative-timestamps*
  (make-instance 'linear-temporal-transform :offset 0.0 :scale (/ 1000.0 30.0)))

(defvar *15-fps-frames-to-relative-timestamps*
  (make-instance 'linear-temporal-transform :offset 0.0 :scale (/ 1000.0 15.0)))


(defvar *unix-to-universal-time-transform*
  (let ((xf (make-time-shift-transform (* 1000 (encode-universal-time 0 0 0 1 1 1970 0)))))
    (setf (from-coordinate-system xf) *unix-time-base*)
    (setf (to-coordinate-system xf) *lisp-universal-time-base*)
    xf))
