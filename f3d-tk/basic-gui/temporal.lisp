(in-package :gui)

;;;
;;; Restructured the notion of time - too many conflicts between
;;; TIMESTAMP-MIXIN API and TEMPORAL-VIEW API.  Everything that deals
;;; with time in any sense should go through TIMESTAMP-MIXIN's
;;; hierarchy.  I've changed things here and in

#|
Skeleton implementation of:

temporal-view
temporal-object-set
temporal-object

Here is the idea:

TEMPORAL-VIEW specifies a TIME.

  For VIDEO-VIEWS, which subclass TEMPORAL-VIEW, the image and 3d-to-2d-projection
  are determined from the TEMPORAL-VIEW-TIME.
||#


#||
[Note (CIC) - this is a SPATIOTEMPORAL-VIEW - there is a timeline view that is 
purely temporal, and also has a notion of CURRENT-TIME that is distinct from the
TEMPORAL-VIEW object.  The "HURT" interface converts all of its spatial views to 
temporal-views, and this works well for highlighting the "current time" by 
conditionally displaying temporal objects (e.g. tracks).]


[ I don't see why this really needs to be so:]

TEMPORAL-OBJECTS must be placed in TEMPORAL-OBJECT-SETS.

  Each TEMPORAL-VIEW has its own DISPLAY-LIST-STATE in the TEMPORAL-OBJECT-SET
  with a COOKIE being the TEMPORAL-VIEW-TIME.  Thus the display-list must be
  regenerated when the TEMPORAL-VIEW-TIME changes.

[Suggestion (CIC):  This is why I usually turn on *always-immediate-render-p* - when showing tracks
overlaid on spatial or video views, there's enough time-varying geometry that display-lists won't 
work. However, I'd say about 80-90%  of the display operations for  spatiotemporal objects like tracks are 
static (i.e., only spatial).  It might make sense to partition the display operations into 
static and dynamic components.  For static components (geometry that isn't time-varying), 
display lists can be created and probably will not change.  Dynamic components (e.g., highlighting 
the current point in time) need to be recomputed whenever the view time changes.

|#

(defmethod cookie ((view t))
  nil)

;;; Maybe this should be a mixin, and merged with timestamp-mixin in
;;; the TIME system.
(defstruct-class temporal-view (view temporal-view-grouping-mixin obj::timestamp-mixin)
  ())

(defmethod temporal-view-time ((view temporal-view))
  (with-class-slot-values temporal-view (obj::timestamp) view
    obj::timestamp))

(defmethod (setf temporal-view-time) (value (view temporal-view))
  (with-class-slots temporal-view (obj::timestamp) view
    (setf obj::timestamp value)))

(defmethod cookie ((view temporal-view))
  (temporal-view-time view))

;;; Conjugate point methods have problems with this definition because 
;;; different 2d-worlds are created in different views for the same video and 
;;; temporal-view-time.  It might be incorrect for an xxx-video-frame
(defmethod view-2d-descr ((view temporal-view))
  (list (2d-world view) (temporal-view-time view)))

#+never ; this must be defined after the video package is defined.
(defmethod view-2d-descr ((view temporal-view))
  (let ((2d-world (2d-world view)))
    (typecase 2d-world
      (video::video-frame (list (video::video-frame-video 2d-world) (temporal-view-time view)))
      (t (list 2d-world (temporal-view-time view))))))
  

(defstruct-class temporal-object-set-mixin (object-set) ())

(defmethod cookies-match (view (object-set temporal-object-set-mixin) display-list-state)
  (with-class-slot-values display-list-state (cookie) display-list-state
    (equal cookie (cookie view))))

(defmethod set-cookie (view (object-set temporal-object-set-mixin) display-list-state)
  (with-class-slots display-list-state (cookie) display-list-state
    (setf cookie (cookie view))))

(defmethod object-set-view-dependencies ((object-set temporal-object-set-mixin) view)
  (let ((deps (call-next-method)))
    (if deps
	(cons view deps)
	view))) 

(defun add-mixins-to-class (class mixins classname-prefix)
  (let* ((base-name (class-name class))
	 (name (intern (format nil "~a-~a" classname-prefix base-name) (symbol-package base-name)))
	 (cds (mop::class-direct-superclasses class)))
    (if (member class cds)
	class
	(mop::ensure-class name :direct-superclasses `(,@mixins ,class ,@cds)))))

(defun-cached add-temporal-object-set-mixin-to-class (class)
  (add-mixins-to-class class '(temporal-object-set-mixin) "TEMPORAL"))

(defun add-temporal-object-set-mixin (object)
  (change-class object (add-temporal-object-set-mixin-to-class (class-of object))))

;(add-temporal-object-set-mixin (first (object-feature-sets (selected-object))))


#+never ;; no, use soft-slots instead so change-class can work more easily
(defclass temporal-object-transform-mixin () 
    ((object-to-parent-transform-table :initform (make-instance 'binary-sorted-table :initial-length 2)
				       :accessor object-to-parent-transform-table)
     (object-to-parent-transform-time-tag :initform nil :accessor object-to-parent-transform-time-tag)
     ))


;;;(lx::all-class-inferiors (find-class 'temporal-object-transform-mixin))

(defclass temporal-object-transform-mixin () ())

;;; alternative without needing extra hard slots
(define-soft-slot basic-gl-object :object-to-parent-transform-table object-to-parent-transform-table)
(define-soft-slot basic-gl-object :object-to-parent-transform-time-tag 
  object-to-parent-transform-time-tag)


;;;(defun interpolate-transform-for-time (table time)
;;;  (mv-bind (val found) (lx::find-entry table (dfloat time))
;;;    (if found
;;;        val
;;;        (let ((vector (lx::binary-sorted-table-vector table))
;;;              (pos (max 1 (min val (- (lx::binary-sorted-table-num-entries table) 1)))))
;;;          (destructuring-bind (time0 mat0) (svref vector (1- pos))
;;;            (destructuring-bind (time1 mat1) (svref vector pos)
;;;              (math::linear-interpolate-transform-matrices mat0 mat1 
;;;                                                           (/ (- time time0) (- time1 time0))))))
;;;        )))

(defun ensure-object-to-parent-transform-table (object)
  (unless (object-to-parent-transform-table object)
    (setf (object-to-parent-transform-table object) (make-instance 'lx::binary-sorted-table))))

;;; Big problems here with multiple temporal-views containing the object.
;;; Any changes to the object-to-parent-transform get lost when alternating between views,
;;; since there is no way for remembering the transform-matrix.
;;; The only way to avoid this problem is to always generate a new keyframe and set its 
;;; transform-matrix whenever the object-to-parent-transform is modified.

(defmethod object-to-parent-transform-for-time ((object temporal-object-transform-mixin) 
						time &optional (interpolate nil))
  (ensure-object-to-parent-transform-table object)
  ;;(format t "object-to-parent-transform-for-time ~a ~a~%" object time)
  (with-class-slots basic-coordinate-system (object-to-parent-transform) object
    (let ((time-tag (object-to-parent-transform-time-tag object)))
      (unless (or (null time) (eql time time-tag))
        (let ((table (object-to-parent-transform-table object)))
          ;; We have a new time -- save the current transform-matrix in the table
          ;; No, this should only be done explicitely
          ;;(remember-object-to-parent-transform object)
          (mv-bind (val found) (lx::find-entry table (dfloat time))
            ;;(format t "object-to-parent-transform-for-time ~{~a ~}~%" (list object time val found) )
            (cond (found
		   (set-transform-matrix object-to-parent-transform val))
		  ((null interpolate)
		   (return-from object-to-parent-transform-for-time nil))
		  (t (let* ((vector (lx::binary-sorted-table-vector table))
			    (num-entries (lx::binary-sorted-table-num-entries table))
			    (pos val)) ; pos is the table insert position
		       (declare (simple-vector vector))
		       (declare (fixnum num-entries pos))
		       (if (< num-entries 1)
			   (return-from object-to-parent-transform-for-time nil)
			   ;; Interpolate the transform-matrix from the nearest surrounding matrices.
			   (destructuring-bind (time0 . mat0) (svref vector (max 0 (1- pos)))
			     (destructuring-bind (time1 . mat1) (svref vector (min pos (1- num-entries)))
			       #+never
			       (when (eq object (gui::selected-object))
				 (format t "object-to-parent-transform-for-time: interpolating ~a ~a~%" 
					 time0 time1))

			       (set-transform-matrix object-to-parent-transform
						     (if (= time1 time0)
							 mat0
							 (math::interpolate-transform-matrices 
							  mat0 mat1 (/ (- time time0) (- time1 time0)))))))))))
            (setf (object-to-parent-transform-time-tag object) time)))))
    ;;(when time (setf (object-to-parent-transform-time-tag object) time))
    object-to-parent-transform))

(defun 4x4-transform-matrix-for-position (position)
  (declare (type dvector position))
  (let ((m (math::make-identity-matrix 4)))
    (declare (type dmatrix m))
    (loop for i fixnum from 0 below (length position)
	  do (setf (aref m i 3) (aref position i)))
    m))

(defun math::interpolate-vectors (v1 v2 alpha)
  (declare (type dvector v1 v2))
  (declare (double-float alpha))
  (loop with n fixnum = (length v1)
	with v of-type dvector = (make-dvector n)
	with 1-alpha double-float = (- 1.0 alpha)
	for i fixnum from 0 below n
	do (setf (aref v i) (+ (* 1-alpha (aref v1 i)) (* alpha (aref v2 i))))
	finally (return v)))

;;; version that allows OBJECT-TO-PARENT-TRANSFORM-TABLE to contain either 4x4-matrices or vectors. 
(defmethod object-to-parent-transform-for-time ((object temporal-object-transform-mixin) 
						time &optional (interpolate nil))
  (ensure-object-to-parent-transform-table object)
  ;;(format t "object-to-parent-transform-for-time ~a ~a~%" object time)
  (with-class-slots basic-coordinate-system (object-to-parent-transform) object
    (let ((time-tag (object-to-parent-transform-time-tag object)))
      (unless (or (null time) (eql time time-tag))
        (let ((table (object-to-parent-transform-table object)))
          ;; We have a new time -- save the current transform-matrix in the table
          ;; No, this should only be done explicitely
          ;;(remember-object-to-parent-transform object)
          (mv-bind (val found) (lx::find-entry table (dfloat time))
            ;;(format t "object-to-parent-transform-for-time ~{~a ~}~%" (list object time val found) )
            (cond (found
		   (set-transform-matrix object-to-parent-transform
					 (if (= (array-rank val) 2)
					     val
					     (4x4-transform-matrix-for-position val))))
		  ((null interpolate)
		   (return-from object-to-parent-transform-for-time nil))
		  (t (let* ((vector (lx::binary-sorted-table-vector table))
			    (num-entries (lx::binary-sorted-table-num-entries table))
			    (pos val)) ; pos is the table insert position
		       (declare (simple-vector vector))
		       (declare (fixnum num-entries pos))
		       (if (< num-entries 1)
			   (return-from object-to-parent-transform-for-time nil)
			   ;; Interpolate the transform-matrix from the nearest surrounding matrices.
			   (destructuring-bind (time0 . v0) (svref vector (max 0 (1- pos)))
			     (destructuring-bind (time1 . v1) (svref vector (min pos (1- num-entries)))
			       #+never
			       (when (eq object (gui::selected-object))
				 (format t "object-to-parent-transform-for-time: interpolating ~a ~a~%" 
					 time0 time1))
			       (set-transform-matrix 
				object-to-parent-transform
				(if (= (array-rank v0) 2)
				    (let ((mat0 v0) (mat1 v1))
				      (if (= time1 time0)
					  mat0
					  (math::interpolate-transform-matrices 
					   mat0 mat1 (/ (- time time0) (- time1 time0)))))
				    (4x4-transform-matrix-for-position 
				     (if (= time1 time0)
					 v0
					 (math::interpolate-vectors 
					  v0 v1 (/ (- time time0) (- time1 time0))))))
				)))))))
            (setf (object-to-parent-transform-time-tag object) time)))))
    ;;(when time (setf (object-to-parent-transform-time-tag object) time))
    object-to-parent-transform))


(defmethod previous-frame-number ((object temporal-object-transform-mixin) frame-number)
  (let* ((table (object-to-parent-transform-table object)))
    (mv-bind (entry pos) (lx::find-entry table (dfloat frame-number))
      (when (and pos (> pos 0.0)) 
	(round (car (aref (lx::binary-sorted-table-vector table) (1- pos))))))))
      
    

(defmethod object-to-parent-transform ((object temporal-object-transform-mixin))
  (object-to-parent-transform-for-time object (current-time)))

(defmethod obj::set-origin :after ((object temporal-object-transform-mixin) vector)
  (remember-object-to-parent-transform object (current-time)))

(defmethod obj::rotate-by :after ((object temporal-object-transform-mixin) orientation-spec
				  &optional center-of-rotation-vector)
  (declare (ignore orientation-spec center-of-rotation-vector))
  (remember-object-to-parent-transform object (current-time)))
    
(defmethod obj::rotate-relative-to-world-by :after ((object temporal-object-transform-mixin) 
						    orientation-spec &optional center-of-rotation-vector)
  (declare (ignore orientation-spec center-of-rotation-vector))
  (remember-object-to-parent-transform object (current-time)))
    
;(fmakunbound 'remember-object-to-parent-transform)
(defmethod remember-object-to-parent-transform ((object temporal-object-transform-mixin) time)
  (let ((time-tag (or time (object-to-parent-transform-time-tag object))))
    (when time-tag
      (ensure-object-to-parent-transform-table object)
      (lx::add-entry (object-to-parent-transform-table object) 
		     (dfloat time-tag)
		     (copy-matrix (transform-matrix (slot-value object 'object-to-parent-transform)))))))

;;; Specialization to save just the object position, not the entrie 4x4 matrix.
(defclass temporal-object-position-mixin (temporal-object-transform-mixin) ())

(defmethod remember-object-to-parent-transform ((object temporal-object-transform-mixin) time)
  (let ((time-tag (or time (object-to-parent-transform-time-tag object))))
    (when time-tag
      (ensure-object-to-parent-transform-table object)
      (lx::add-entry (object-to-parent-transform-table object) 
		     (dfloat time-tag)
		     (let ((m (transform-matrix (slot-value object 'object-to-parent-transform))))
		       (declare (type dmatrix m))
		       (cv (aref m 0 3) (aref m 1 3) (aref m 2 3)))))))

;;;(defvar *current-time* nil)
;;;(defun current-time () *current-time*)

(defun current-time () (cookie *current-view*))
(defun current-time () (and *current-view* (cookie *current-view*)))

#|
;(cookie (selected-view))

(current-time)

(let ((*current-view* (selected-view)))
  (describe (transform-matrix (object-to-parent-transform (selected-object)))))

(setf (object-to-parent-transform-table (selected-object)) nil
      (object-to-parent-transform-time-tag (selected-object)) nil)
|#


;(undefmethod update-object-after :after (temporal-object-transform-mixin))
#+never
(defmethod update-object-after :after ((object temporal-object-transform-mixin))
  (let ((time (current-time)))
    (format t "update-object-after :after ~a ~a~%" object time)
    (when time
      #+never ;; this next is done in the init method
      (unless (object-to-parent-transform-table object)
	(setf (object-to-parent-transform-table object) (make-instance 'binary-sorted-table)))
      (lx::add-entry (object-to-parent-transform-table object) time
		 (copy-matrix (transform-matrix (slot-value object 'object-to-parent-transform)))))))


(defun-cached add-temporal-object-transform-mixin-to-class (class)
  (add-mixins-to-class class '(temporal-object-transform-mixin) "TEMPORAL"))


(defun add-temporal-object-transform-mixin (object)
  (change-class object (add-temporal-object-transform-mixin-to-class (class-of object))))


#|
(describe (class-name (find-class 'obj::2d-crosshair-object)))
(add-temporal-object-transform-mixin (selected-object))
|#


(defmethod object-popup-menu-item-list  ((ui cme-object-popup-ui)
					 (object temporal-object-transform-mixin))
  (append (call-next-method)
	  `(("Set Trans" :eval (set-temporal-object-to-parent-transform *interactor*))
	    ("Clr Trans" :eval (clear-temporal-object-to-parent-transform *interactor*))
	    ("Clr All Trans" :eval (clear-all-temporal-object-to-parent-transforms *interactor*)))))


(defmethod set-temporal-object-to-parent-transform ((interactor interactor))
  (let* ((object (selected-object))
	 (*current-view* (top-view (current-window)))
	 (time (current-time)))
    ;;(format t "set-temporal-object-to-parent-transform ~a ~%" time)
    (when (and object time)
      ;(object-to-parent-transform-for-time object time) ; force cache to be up-to-date
      (remember-object-to-parent-transform object time))))

(defmethod clear-temporal-object-to-parent-transform ((interactor interactor))
  (let* ((object (selected-object))
	 (*current-view* (top-view (current-window)))
	 (time (current-time)))
    (when (and object time)
      (lx::remove-entry (object-to-parent-transform-table object) (dfloat time)))))

(defmethod clear-all-temporal-object-to-parent-transforms ((interactor interactor))
  (let* ((object (selected-object))
	 (*current-view* (top-view (current-window))))
    (when object
      (setf (object-to-parent-transform-table object) nil)))) 


;(clear-all-temporal-object-to-parent-transforms *interactor*)
