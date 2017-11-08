(in-package :gui)

(eval-when (eval load compile)
  (pushnew :merged-object-sets *features*))

;;; Contains NO direct dependencies on OpenGL or TK.

;;; **********************  DISPLAY-LISTS & OBJECT-SETS   **********************

#|
Sat Nov  6 2004 LHQ:  It appears that BACKING-STORE-P = T always, and should be flushed.

Sat Nov 6 2004 LHQ: I am attempting change object-sets so that they can contain
both immediate-render objects and display-list-objects.  The object-ht will
contain both.  Map-over-object-set allows an optional 3rd argument controlling
the selection of immediate-render-objects, display-list-objects, or both.  The
complication is that the DISPLAY-LIST-STATE info in DISPLAY-LISTS needs to exist
for both types of object.

|#

#|

Each VIEW contains an ordered list of OBJECT-SETS.

OBJECT-SETS contain NON-COMPOSITE objects and the leaf-node objects of COMPOSITE-OBJECTS.

Each OBJECT-SET has a floating point PRIORITY.

OBJECT-SETS are rendered in order of appearance in view's the list of object-sets,
   which is sorted in increasing priority-order.

All objects in an object set must belong to the same WORLD, and
   have the same IMMEDIATE-RENDER-P, and BACKING-STORE-P properties.

IMMEDIATE-RENDER-P means that the object must be rendered immediately and cannot
   be rendered from a display-list.  It makes no sense to be in a display-list
   since view-dependent computations might be required, as for an axis-object.
   It is still permissible this object to be in backing-store.  Images and
   texture-mapped terrain are examples of objects which require direct
   rendering, because we cannot assume that all of their required texture maps
   can be handled in display-lists.
 
BACKING-STORE-P means that the object should be saved in the backing-store.
   Selected-objects and objects undergoing motion (dynamic-objects) should not
   be saved in backing-store.  Thereby, redisplay can be accomplished by
   displaying the backing-store, followed by displaying the objects with
   BACKING-STORE-P = NIL.

FURTHER RESTRICTIONS:

The backing-store does not include the depth-buffer and therefore
REDISPLAY-FROM-BACKING-STORE totally overwrites the contents of frame-buffer
from the backing-store.

Writing to the depth buffer is disabled for all object-sets with BACKING-STORE-P
= NIL.  Because the backing-store does not include the depth-buffer, we cannot
allow "dynamic-objects" to defeat the purpose of the backing-store.  Object-sets
with BACKING-STORE-P = NIL can be rendered with GL_DEPTH_TEST enabled, which
will allow proper z-buffering against the backing store, but not other
"dynamic-objects".

|#

;;; The object-set class contains both a list of objects to represent the
;;; desired priority ordering, and a hash-table of objects to permit rapid tests
;;; of object membership.  The list could be eliminated if priority ordering can
;;; be eliminated.  Since the view's list of object-sets is also priority
;;; ordered, maybe that is enough to control rendering order.

;;; Should object-set be specialized to 2d-object-set and 3d-object-set according
;;; to the class of world?

(defstruct-class object-set (property-list-mixin)
    ((priority :initform 0.0 :initarg :priority :accessor priority)
     (world :initform nil :initarg :world :accessor world)
     (immediate-render-p :initform nil :initarg :immediate-render-p :accessor immediate-render-p)
     (backing-store-p :initform t :initarg :backing-store-p :accessor backing-store-p)
     (object-ht :initform (make-hash-table) :accessor object-ht)
     (display-lists :initform nil :accessor display-lists) ; misnamed instance-var
     ))

(define-soft-slot object-set :name name)
(define-soft-slot object-set :unique-id unique-id)  

(defmethod invalidate ((object-set object-set))
  (loop for dl in (display-lists object-set)
	do (setf (invalid dl) t)))

#+never ; old Wed Dec 22 2004
(defstruct-class display-list-state (base-struct-class)
  ((surrogate-projection :initform nil :initform :surrogate-projection :accessor surrogate-projection)
   (excluded-objects :initform (list nil) :accessor excluded-objects)
   (invalid :initform t :accessor invalid)
   (display-list-id :initform nil :initarg :display-list-id :accessor display-list-id )))

#|  **********************  DISPLAY-LIST-STATE  **********************

KEY distinguishes views that require different display-lists.  KEYs are tested
    with EQUAL, thus may contain a list of attributes which must match.

VIEW-COOKIE is a unique value that serves to validate that the display-list is up-to-date
    for the view.  When relevent parameters of the view are changed, the cookie value of the view
    changes, thus making it different from VIEW-COOKIE of the DISPLAY-LIST-STATE.

Examples:  
   
   VANILLA VIEWS:

       KEY is NIL.  VIEW-COOKIE is unused.

   VIEWS WITH SURROGATE PROJECTIONS:

       (equal KEY (surrogate-projection view))

   TEMPORAL VIEWS:

       (and (equal KEY view) (equal VIEW-COOKIE (COOKIE view)))

       invalid = (not (equal VIEW-COOKIE (COOKIE view))) 
       



|#



(defstruct-class display-list-state (base-struct-class)
  ((key :initform nil :initarg :key)
   (cookie :initform nil :accessor cookie)
   (excluded-objects :initform (list nil) :accessor excluded-objects)
   (invalid :initform t :accessor invalid)
   (display-list-id :initform nil :initarg :display-list-id :accessor display-list-id )))

(define-accessor-synonym inferiors object-set children)

(defmethod children ((obj object-set))
  (loop for obj being the hash-keys of (object-ht obj) collect obj))

;(fmakunbound 'sensitive-p )
;(fmakunbound '(setf sensitive-p ))
;(define-soft-slot object-set :sensitive-p sensitive-p)

;;;; composite-objects also define add-object and remove-object methods

(define-soft-slot basic-gl-object :feature-sets object-feature-sets)

(defmethod add-object ((object t) (object-set object-set))
  (let ((object-ht (object-ht object-set)))
    (unless (gethash object object-ht)
      (invalidate object-set)
      (add-object-to-world-namespace object-set object)
      (pushnew object-set (object-feature-sets object))
      (setf (gethash object object-ht) t)
      )))

(defmethod add-object :after ((object t) (object-set object-set))
  (add-object-to-world-namespace object-set object)
  (pushnew object-set (object-feature-sets object)))

(defmethod remove-object ((object t) (object-set object-set))
  (let ((object-ht (object-ht object-set)))
    (when (gethash object object-ht)
      (remhash object object-ht)
      (invalidate object-set)
      (removef object-set (object-feature-sets object))
      t)))

;;;(defmethod remove-object :after ((object t) (object-set object-set))
;;;  (removef object-set (object-feature-sets object)))

(defmethod contains-object ((object-set object-set) (object t))
  (let ((object-ht (object-ht object-set)))
    (gethash object object-ht)))

(defmethod add-objects ((objects t) (object-set object-set) &key clear)
  (when clear (clrhash (object-ht object-set)))
  (loop with invalid
	for obj in objects
	when (add-object obj object-set)
	  do (setq invalid t)
	finally (when invalid (invalidate object-set))))

(defmethod remove-objects ((objects t) (object-set object-set))
  ;;(format t "obj::remove-object ~a~%" objects)
  (loop with invalid
	for obj in objects
	when (remove-object object-set obj)
	  do (setf invalid t)
	finally (when invalid (invalidate object-set))))

(defmethod (setf children) (new-children (object-set object-set))
  (add-objects new-children object-set :clear t))

#+broken-for-composites
(defmethod add-objects (objects (object-set object-set) &key clear)
  (when clear (clrhash (object-ht object-set)))
  (loop with object-ht = (object-ht object-set)
	with invalid
	for obj in objects
	unless (gethash obj object-ht)
	  do (setf (gethash obj object-ht) t
		   invalid t)
	finally (when invalid (invalidate object-set))))

#+broken-for-composites
(defmethod remove-objects (objects (object-set object-set))
  (loop with object-ht = (object-ht object-set)
	with invalid
	for obj in objects
	when (gethash obj object-ht)
	  do (remhash obj object-ht)
	     (setf invalid t)
	finally (when invalid (invalidate object-set))))

(defun make-object-set (object-list &rest initargs )
  (apply 'make-instance 'object-set :objects object-list initargs))

#-merged-object-sets
(defun separate-immediate-render-objects (objects)
  (loop for obj in objects
	when (immediate-render-p obj)
	  collect obj into immediate-objects
	else collect obj into display-list-objects
	finally (return (values display-list-objects immediate-objects))))

#+merged-object-sets
(defun separate-immediate-render-objects (objects)
  objects)

(defvar *object-set-instance-counter* 0)


;;;(defmethod print-object ((cs object-set) stream)
;;;   (if t ;*print-escape*
;;;       (format stream "#<~a ~s #X~x>" (type-of cs) (or (name cs) "") (%pointer cs))
;;;       (format stream "~a ~a" (type-of cs) (or (name cs) ""))))

(defmethod print-object ((cs object-set) stream)
  (print-unreadable-object (cs stream :type t :identity t)
    (princ (name cs) stream)
    (princ " (" stream)
    (princ (length (inferiors cs)) stream)
    (princ ")" stream)
    ))


(defmethod initialize-instance :after ((object-set object-set)
				       &key objects 
				       (immediate-render-p nil immediate-render-p-supplied)
				       name
				       &allow-other-keys)
  (ignore immediate-render-p)
  (unless immediate-render-p-supplied
    ;; default immediate-render-p to complement of backing-store-p.
    (with-slots (immediate-render-p backing-store-p) object-set
      (setf immediate-render-p (not backing-store-p))))
  
  (unless name 
    (setq name (format nil "_FS-~a" (incf *object-set-instance-counter*))))
  (setf (name object-set) name)

  (when objects
    (unless (world object-set)
      (setf (world object-set) (world (first objects))))
  
    (add-objects objects object-set :clear t)))

(defmethod selected-objects-in-object-set ((object-set object-set) selected-objects)
  (loop with object-ht = (object-ht object-set)
	for obj in selected-objects
	when (gethash obj object-ht)
	  collect obj))

;;;(defmethod map-over-object-set-int ((object-set object-set) fn)
;;;  (loop with object-ht = (object-ht object-set)
;;;        for obj being the hash-keys of object-ht
;;;        do (funcall fn obj)))

#+old
(defmethod map-over-object-set-int ((object-set object-set) fn
				&optional (immediate-p nil immediate-p-supplied))
  (loop with object-ht = (object-ht object-set)
	for obj being the hash-keys of object-ht
	do (funcall fn obj)))

(declaim (special *always-immediate-render-p*))

;;; new version allowing both immediate and display-list objects in same object-set.
(defmethod map-over-object-set-int ((object-set object-set) fn
				    &optional (immediate-p nil immediate-p-supplied))
  (loop with object-ht = (object-ht object-set)
	for obj being the hash-keys of object-ht
	when  (or (null immediate-p-supplied)
		  (eq immediate-p (or *always-immediate-render-p* (immediate-render-p obj))))
	  do (funcall fn obj)))

;(immediate-render-p (selected-object))

;(fmakunbound 'map-over-object-set)
(defmacro map-over-object-set ((var os &optional (immediate-p nil immediate-p-supplied)) &body body)
  `(map-over-object-set-int ,os #'(lambda (,var) .,body) ,@(when immediate-p-supplied `(,immediate-p))))

;;; This is sorta expensive, but should be fine as long as the number of
;;; containing-object-sets isn't very large and this isn't called in a
;;; time-sensitive context.  
(defmethod object-sets-containing-objects (objects)
  (let ((containing-object-sets nil))
    (map-over-all-views (view)
      (loop for obj-set in (object-sets view)
	    when (and (not (memq obj-set containing-object-sets))
		      (loop for obj in objects
			    thereis (contains-object obj-set obj)))
	      do (push obj-set containing-object-sets)))
    containing-object-sets))	

#+never ; replaced by definition in worlds.lisp
(defmethod remove-self ((object gl-object))
  (loop for obj-set in (object-sets-containing-objects (list object))
	do (remove-object object obj-set)))


;;; **********************  PRIORITIZED-OBJECT-SET  ****************************

#|
Objects within a PRIORITIZED-OBJECT-SET are sorted in increasing priority-order.
  Each object has a floating point PRIORITY.
  Objects are rendered in order of INCREASING PRIORITY.
|#

(defstruct-class prioritized-object-set (object-set)
    ((objects :initform nil :reader objects)
     ))

(defmethod (setf objects) (new-objects (object-set prioritized-object-set))
  (with-slots (objects object-ht world) object-set
    (when (and new-objects (null world))
      (setf world (world (first new-objects))))
    (setf objects (sort new-objects
			#'<
			:key #'(lambda (obj) (priority obj))))
    (clrhash object-ht)
    (loop for obj in objects
	  do (setf (gethash obj object-ht) t))
    objects))

(defmethod add-objects (new-objects (object-set prioritized-object-set) &key clear)
  (setf (objects object-set) (if clear
				 new-objects
				 (append new-objects (objects object-set)))
	;;(excluded-objects object-set) (list nil)
	)
  (invalidate object-set))


(defmethod remove-objects (remove-objects (object-set prioritized-object-set))
  (setf (objects object-set) (set-difference (objects object-set) remove-objects)
	;;(excluded-objects object-set) (list nil)
	)
  (invalidate object-set))

(defmethod map-over-object-set-int ((object-set prioritized-object-set) fn
				&optional (immediate-p nil immediate-p-supplied))
  (loop for obj in (objects object-set)
	when  (or (null immediate-p-supplied)
		  (eq immediate-p (immediate-render-p obj)))
	  do (funcall fn obj)))

;;; display-lists is a cache of all of the distinct display-list-states
;;; for this object-sets.  The display-list-states are distiguished by the
;;; surrogate-projection, and possibly time.
;;; Specialize this for temporal-views.

;; old Wed Dec 22 2004
;;;(defmethod get-object-set-display-list-state (object-set (view view))
;;;  (let* ((projection (3d-to-2d-projection view))
;;;         (surrogate-projection (and (typep (world object-set) 'gl-3d-world)
;;;                                   (surrogate-projection projection))))
;;;    (with-class-slots object-set (display-lists) object-set
;;;      (loop for dl-state in display-lists
;;;            when (eq (surrogate-projection dl-state) surrogate-projection)
;;;              return dl-state
;;;            finally
;;;         (let ((dl-state (make-instance 'display-list-state
;;;                                        :surrogate-projection surrogate-projection
;;;                                        :display-list-id (glGenLists 1))))
;;;           (push dl-state display-lists)
;;;           (return dl-state))))))

;;; This is a cache-key for the display-list-states of an object-set.
(defmethod object-set-view-dependencies (object-set view)
  (and (typep (world object-set) 'gl-3d-world) 
       (3d-to-2d-projection view)
       (surrogate-projection (3d-to-2d-projection view))))
#|
(defmethod object-set-view-dependencies (object-set (view temporal-view))
  (cons (temporal-view-time view) (call-next-method)))
|#

(defmethod get-object-set-display-list-state (object-set view)
  (let* ((projection (3d-to-2d-projection view))
	 (key (object-set-view-dependencies object-set view)))
    (with-class-slots object-set (display-lists) object-set
      (loop for dl-state in display-lists
	    when (equal (display-list-state-key dl-state) key)
	      return dl-state
	    finally
	 (let ((dl-state (make-instance 'display-list-state
					:key key
					:display-list-id (glGenLists 1))))
	   (push dl-state display-lists)
	   (return dl-state))))))

;;#+experimental
(defmethod get-object-set-display-list-state (object-set view)
  (let* (;(projection (3d-to-2d-projection view))
	 (key (object-set-view-dependencies object-set view)))
    (with-class-slots object-set (display-lists) object-set
      (loop for dl-state in display-lists
	    when (equal (display-list-state-key dl-state) key)
	      return dl-state
	    finally
	 (let ((dl-state (make-instance 'display-list-state
					:key key
					:display-list-id (glGenLists 1))))
	   (push dl-state display-lists)
	   (return dl-state))))))


;;; Vanilla views do nothing with cookies.
(defmethod set-cookie (view object-set display-list-state))

;;; Vanilla views always match their cookies
(defmethod cookies-match (view object-set display-list-state)
  t)

#+never
(defmethod cookies-match (view display-list-state)
  (with-class-slot-values display-list-state (cookie) display-list-state
    (equal cookie (cookie view))))



;;; Should thie be renamed to DISPLAY-LIST-VALID-P?
;;; Specialize this for temporal-views ?
(defmethod DISPLAY-LIST-VALID-P (view object-set new-excluded-objects)
  (let ((display-list-state (get-object-set-display-list-state object-set view)))
    (with-class-slot-values display-list-state (excluded-objects invalid) display-list-state
      (values (not (or invalid 
		       (not (cookies-match view object-set display-list-state))
		       *invalidate-all-object-sets*
		       (not (equal excluded-objects new-excluded-objects))))
	      display-list-state))))

;;; Should this be renamed to DISPLAY-LIST-VALID-P?
;;; Specialize this for temporal-views ?
;;;(defmethod regenerate-display-list-p (view display-list-state new-excluded-objects)
;;;  (with-class-slot-values display-list-state (excluded-objects invalid) display-list-state
;;;    (or invalid 
;;;        (not (cookies-match view display-list-state))
;;;        *invalidate-all-object-sets*
;;;        (not (equal excluded-objects new-excluded-objects)))))

(defmethod redisplay-required ((object-set object-set)
			       &optional excluded-object-list view)
  (let* ((dl (get-object-set-display-list-state object-set view)))
    (with-class-slot-values display-list-state (excluded-objects invalid) dl
      (let* ((new-excluded-objects
	      (selected-objects-in-object-set object-set excluded-object-list))
	     (redisplay (or invalid 
			    (not (cookies-match view object-set dl))
			    (not (equal excluded-objects new-excluded-objects)))))
	;(format t "redisplay-required ~a ~a ~a~%" object-set view redisplay)
	(values redisplay new-excluded-objects)))))


	  
;;; Chris:  I would like to use the name BOUNDING-BOX for a generic function that is 
;;; slightly different from this.
;;;
;;; Should this really exist at all?  Probably not, since the
;;; object-set exists more for managing the display of objects.  The
;;; feature-set is the thing...
;;;
;;; This has no change of working:  
;;; bbox of children must be transformed thru object-to-parent-transform
#+broken
(defmethod obj::cc-version-of-bounding-box ((o object-set) &optional bbox (reset t))
  (let ((bbox (or bbox (make-coordinate-vector 6))))
    (with-slots (object-ht) o
      (loop for obj being the hash-keys of object-ht
	    for r = reset then nil
	    do (obj::cc-version-of-bounding-box obj :bbox bbox :reset r)))
    bbox))


;;; *********************   FEATURE-SETS  *********************

;;; Sun Sep 26 2004: FEATURE-SETS are no longer part of the coordinate-system
;;; and object-hierarchy.  OBJECT-COLLECTIONS are now just simple sets of
;;; objects with name and unique-id.

(defmethod set-world ((obj object-set) new-world)
  (setf (world obj) new-world)
  #+never ; this next totally breaks composites
  (loop for inf in (children obj)
	do (setf (parent inf) new-world)))

(defmethod add-feature-set (feature-set (object basic-gl-object) &key &allow-other-keys)
  (let ((object-feature-sets (object-feature-sets object)))
    (unless (memq feature-set object-feature-sets)
      (setf (object-feature-sets object) (append object-feature-sets (list feature-set))))))

(defstruct-class feature-set (object-set) ())
;;; There is an INITIALIZE-INSTANCE :AFTER method for FEATURE-SET defined in cme/compat-defs.lisp


#|
(cme::feature-set-pathnames (get-3d-world-named "alv") cme::*radius-site-glue2*)
(directory #P"$RADIUS/sites/alv/models/")
(directory #P"$RADIUS/sites/alv/models/" :truenamep nil)
(directory #P"$RADIUS/sites/alv/models/" :follow-links nil :truenamep nil)
(cme::feature-set-pathname "" (get-3d-world-named "alv") cme::*radius-site-glue2*)

|#

(defstruct-class 2d-feature-set (feature-set) ())

(defstruct-class 3d-feature-set (feature-set) ())

;;; Chris:  I would like to use the name BOUNDING-BOX for a generic function that is 
;;; slightly different from this.
(defmethod obj::cc-version-of-bounding-box ((obj object-set) 
					    &optional (bbox (make-coordinate-vector 6)) (reset t))
  (loop for r = reset then nil
	for obj in (inferiors obj)
	do (obj::cc-version-of-bounding-box obj  bbox  r))
  bbox)


(defmethod obj::bounding-box ((os object-set) &key bbox transform)
  (let ((world (world os)))
    (map-over-object-set (child os)
      (unless (typep child 'obj::composite-object)
	;; ignore composites since all of their children are also in the object set.
	(setq bbox (obj::bounding-box child
				      :bbox bbox 
				      :transform (cons (cs-to-superior-cs-transform child world) transform)))))
    bbox))


;;(obj::bounding-box (first (obj::object-feature-sets (selected-object))))



