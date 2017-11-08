(in-package :obj)

;;; Not really sure that this should be in the OBJ package, but the
;;; gl-objects system is loaded early, and this allows us to use the
;;; definitions most anywhere.


(deftype timestamp () `(or (signed-byte 64) double-float))

(deftype timestamp-array (&optional n)
  `(simple-array timestamp (,(or n '*))))

;;; Fundamental classes and methods for representing temporal
;;; objects...  note that "timestamp" is the active timestamp and
;;; replaces the TIME slot in TEMPORAL-VIEW (through TIMESTAMP-MIXIN).
;;; This unifies the different time-related APIs that have been
;;; floating around.
;;; 

(defstruct-class time-point (transforms::time-base)
  ((begin-time :initform 0 :initarg :begin-time :accessor begin-time)))

;;; Generalizing to distributions: The motivation is that measurement
;;; of the exact time of an event in absolute UTC coordinates is
;;; impossible.  Clocks have jitter and drift, and video is sampled
;;; only at discrete intervals.  Moreover, the criteria for defining
;;; an event are subjective and error prone.  It makes sense,
;;; therefore, to talk about representing significant points in time
;;; as distributions indicating the probability that the event
;;; occurred at a particular time.

;;; This issue is further complicated by the fact that the
;;; transformation from one timebase to another adds its own
;;; uncertainty.  We will assume that timebases can be corrected up to
;;; noise in the temporal alignment.  Example: we are given a timeline
;;; T that is to be registered to "absolute" UTC coordinates.  The
;;; implied transformation has a scale and offset, plus a jitter
;;; model.  After applying scale and offset, a time t on T is
;;; transformed to t' in UTC, but only up to some distribution that
;;; tells us the probability that t' is at a particular point on the
;;; UTC timeline.  This addresses the uncertainty inherent in temporal
;;; transformations.
;;;
;;; To place this event on UTC, we would then propagate the event's
;;; time distribution through the T-to-UTC transformation, to get a
;;; new distribution in UTC representing the event occurrence
;;; probability combined with transformation uncertainty.

(defmethod time-interval ((obj t))
  (values nil nil))

(defmethod time-interval ((obj time-point))
  (values (begin-time obj) nil))

(defstruct-class time-interval (time-point)
  ((end-time :initform 0 :initarg :end-time :accessor end-time)))

(defmethod time-interval ((obj time-point))
  (values (begin-time obj) (end-time obj)))

(defmethod duration ((obj time-interval))
  (- (end-time obj) (begin-time obj)))

(defmethod set-interval  ((obj time-interval) begin end)
  (with-class-slots time-interval (begin-time end-time) obj
    (setf begin-time begin
          end-time end)))

(defmethod print-object ((object time-interval) stream)
  (with-class-slots time-base (name) object
    (if *print-escape*
	(format stream "#<~a ~a #X~x>" (type-of object) (duration object)  (%pointer object))
	(format stream "~a ~a" (type-of object) (duration object) ))))

;;;
;;; What about a temporal parent and associated temporal transforms???
;;;
(defstruct-class timestamp-mixin (time-interval)
  ((timestamp :initform 0)))


(defmethod timestamp ((obj timestamp-mixin))
  (with-class-slot-values timestamp-mixin (timestamp) obj
    timestamp))


(defmethod (setf timestamp) (value (obj timestamp-mixin))
  (with-class-slots timestamp-mixin (timestamp) obj
    (setf timestamp value)))


;;;
;;; Why have this?  Can't we specialize the above setf method??
;;;
(defmethod set-timestamp ((obj timestamp-mixin) value)
  (with-class-slots timestamp-mixin (timestamp) obj
    (setf timestamp value)))


;;;
;;; Not entirely sure about this.  I think this method should move up
;;; to those classes that really need this to be done.  The reasoning
;;; here is to ensure that every object has a meaningful timestamp at
;;; all times.
;;;
;;; (defmethod set-interval :after ((obj timestamp-mixin) begin end)
;;;  (setf (timestamp obj) (* 0.5 (+ begin end))))

(defmethod initialize-instance :after ((obj timestamp-mixin) &rest args)
  ;; Temporal objects must use immediate render, since we can't
  ;; predict (at present) whether they need to be drawn.
  (with-class-slot-values timestamp-mixin (begin-time end-time timestamp) obj
    (setf timestamp (* 0.5 (+ begin-time end-time)))))

  

(proclaim '(inline time-intervals-overlap))



;;; This definition assumes that the begin and end pairs are already
;;; ordered properly.  Thus, if end2 is before begin1, or if begin2 is
;;; after end1, then the intervals cannot overlap.  All other
;;; possibilities are admissible.
(defun time-intervals-overlap (begin1 begin2 end1 end2)
  (declare (timestamp begin1 begin2 end1 end2)
           (optimize (speed 3)(safety 0)))
  (not (or (< end2 begin1) (> begin2 end1))))

(defmethod intervals-overlap ((e1 time-interval) (e2 time-interval))
;;  (format t "~%e1: ~a ~a ; e2: ~a ~a" (begin-time e1) (end-time e1) (begin-time e2) (end-time e2))
  (time-intervals-overlap (begin-time e1) (begin-time e2) (end-time e1) (end-time e2)))


;;;
;;; Not sure if this is the best way, but if we drop through, then we
;;; assume non-temporal objects, which have unbounded temporal extent.
;;;

(defmethod intervals-overlap (e1 e2) t)

(defmethod interval-intersection ((i1 time-interval) (i2 time-interval))
  (let ((max-begin (max (begin-time i1) (begin-time i2)))
	(min-end (min (end-time i1) (end-time i2))))
    (and (>= min-end max-begin)
	 (values max-begin min-end))))

