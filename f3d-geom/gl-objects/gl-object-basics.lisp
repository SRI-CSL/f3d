(in-package :obj)


(defparameter *draw-debug* nil)
#|
(setq *draw-debug* nil)
(progn *draw-debug*)
|#


#|  *************  TOO MANY CLASSES DEFINED HERE  *************

GL-WORLD GL-2D-WORLD GL-3D-WORLD

GL-OBJECT GL-2D-OBJECT-MIXIN GL-3D-OBJECT-MIXIN ...

split this into gl-object-basics.lisp and gl-objects.lisp

|#

(declaim (special *GL-VERTEX-TYPE* *GL-VERTEX-BYTES-PER-ELEMENT* *GL-VERTEX-INDEX-TYPE*))

;;; ***********************  GL-WORLDS  ***********************

(defstruct-class gl-world () ())

;;; Allow any cartesian-coordinate-system to have 2d-projection inferiors
(defmethod 2d-worlds ((cs cartesian-coordinate-system))
  (get-prop cs :2d-worlds))
  (defmethod (setf 2d-worlds) (new-2d-worlds (cs cartesian-coordinate-system))
    (setf (get-prop cs :2d-worlds) new-2d-worlds))

 ;;;  *************   GL-2D-WORLD  ***************

 ;;; A GL-2D-WORLD is nothing more than a 2D-CARTESIAN-COORDINATE-SYSTEM
 ;;; whose parent is a LOCAL-VERTICAL-COORDINATE-SYSTEM or NIL,
 ;;; and has methods defining the 3D-WORLD and 3D-TO-2D-PROJECTION in terms
 ;;; of parent and object-to-parent-transform.

(defstruct-class gl-2d-world (gl-world 2d-cartesian-coordinate-system) ())

#|
MUST GUARANTEE THE FOLLOWING:
   3d-world = (parent 2d-world)
   3d-to-2d-projection = (inverse-transform (object-to-parent-transform 2d-world))
   2d-world = (to-coordinate-system 3d-to-2d-projection)
   3d-world = (from-coordinate-system 3d-to-2d-projection)
|#

(define-accessor-synonym 3d-world gl-2d-world parent)

;;; What to do...what to do.  For certain video sequences, it's
;;; convenient to introduce a new coordinate frame that is "portable",
;;; allowing the video to be registered with the 3d-world through a
;;; rigid transformation.  This breaks when that is done:
(defmethod 3d-to-2d-projection ((world gl-2d-world))
    (inverse-transform (object-to-parent-transform world)))

(defmethod (setf 3d-to-2d-projection)  (new-projection (world gl-2d-world))
  (set-parent-transform world (inverse-transform new-projection)))

;;; FIXME:  This needs to interact better with the construction of the 3d-to-2d-projection.
;;; All of the pointers relating 2d-world, 3d-world and 3d-to-2d-projection must be correct.
;; WHO IS ALLOCATED FIRST:  the 3d-to-2d-projection or the 2d-world?
  
(defmethod initialize-instance :after ((world gl-2d-world) &key 3d-world 3d-to-2d-projection
				       &allow-other-keys)
  (unless 3d-world 
    (when 3d-to-2d-projection 
      (setq 3d-world (from-coordinate-system 3d-to-2d-projection))))
    
  (setf (parent world) 3d-world
	(3d-to-2d-projection world) 3d-to-2d-projection))

;;;  *************   GL-3D-WORLD  ***************

(defstruct-class gl-3d-world (gl-world transforms::local-vertical-coordinate-system)
  ((2d-worlds :initform nil :accessor 2d-worlds)))

  
;;; *****************************  GL-OBJECT CLASSES *****************************


(defstruct-class basic-gl-object (cartesian-coordinate-system)
    ((graphics-style :initform nil :initarg :graphics-style :accessor graphics-style)))

(define-soft-slot basic-gl-object :feature-sets object-feature-sets)
(define-soft-slot basic-gl-object :unique-id unique-id)

(defstruct-class basic-object (basic-gl-object) ())

(defstruct-class gl-3d-object-mixin (3d-cartesian-coordinate-system) ())

(defstruct-class gl-2d-object-mixin (2d-cartesian-coordinate-system) ())

;;; GL-OBJECT has a vertex-array
(defstruct-class gl-object (basic-gl-object)
  ( ;; vertex-array is a 2d-array (nverts x 3)
   (vertex-array :initform nil :initarg :vertex-array :accessor %vertex-array)
   ;; vertex-array-alist holds transformed vertex-arrays for non-4x4-projections.
   ;; Entries are: ((non-4x4-projection . vertex-array) . transformed-vertex-array)
   (vertex-array-alist :initform nil :accessor vertex-array-alist)
   ))

;;; This method can be eliminated when object-to-world-transform is 
;;; replaced with object-to-parent-transform.
;;; FIXME: OBJECT-TO-WORLD-TRANSFORM is specific to CME and should move to BASIC-OBJECT.
;;; OBJECT-TO-WORLD-TRANSFORM keyword should become OBJECT-TO-PARENT-TRANSFORM
(defmethod initialize-instance :after ((object basic-gl-object) &key world object-to-world-transform
				       graphics-style 
				       attributes
				       vertices
				       &allow-other-keys)
  (when (consp graphics-style)
    (setf (graphics-style object) (apply 'make-instance 'gl-graphics-style graphics-style)))
  (when attributes (setf (get-prop object :attributes) attributes))

  ;; OBJECT-TO-WORLD-TRANSFORM is for backward compatibility with old CME feature-sets only.
  (when (arrayp object-to-world-transform) ; flush this ?
    (setf object-to-world-transform
	  (make-4x4-coordinate-transform object-to-world-transform)
	  (object-to-parent-transform object) object-to-world-transform))
  (when object-to-world-transform
    (transforms::connect-transforms object-to-world-transform
				    (inverse-transform object-to-world-transform)
				    object world)
    ;; This looks wrong -- assumes (eq parent world)
    (set-parent-transform object object-to-world-transform) ; this also sets the parent of the object
    )
  (when vertices
    (setf (%vertex-array object)
	  (if (arrayp vertices)
	      vertices
	      (apply #'make-vertex-array vertices))))
  )

(defmethod scale-vertex-array ((o basic-gl-object) scale-vector)
  (scale-vertex-array (vertex-array o) scale-vector))
				       


;;; Mix gl-3d-object-mixin or gl-2d-object-mixin with various object classes to
;;; define the dimensionality of the class.
;;; basic-gui/view.lisp defines methods for gl-3d-object-mixin and gl-2d-object-mixin.


(defmethod fasd-form-properties-list-do-not-dump :around ((obj basic-gl-object))
  (append (call-next-method)
	  `(:selectid :immediate-render-p :suppress-label)))

(defmethod short-name-string ((object basic-gl-object))
  (let* ((class (class-of object)))
    (string-capitalize (class-name class))))

(defmethod name-string ((object basic-gl-object))
  (short-name-string object))

(defmethod vertex-count ((object basic-gl-object))
  (array-dimension (vertex-array object) 0))

;;;  This is refedined in lisp/globj/object-sets.lisp to support non-linear-projections.
(defun vertex-array (object)
  (%vertex-array object))

;;; WORLD is the first PARENT coordinate-system that is not a BASIC-GL-OBJECT
;;; For a 2d-object, this will ordinarily be a 2D-WORLD associated with an IMAGE.
;;; For a 3d-object, this will ordinarily be a LOCAL-VERTICAL-COORDINATE-SYSTEM.
;;;    It does not want to be a GEOCENTRIC-COORDINATE-SYSTEM, because OpenGL only
;;;    supports 32 bit IEEE floats, which are not big enough for accurate geocentric coordinates.
(defmethod world ((object basic-gl-object))
  (with-class-slots gl-object (parent) object
    (cond ((null parent)
	   object) ; should this be an error?
	  ((typep parent 'basic-gl-object)
	   (world parent))
	  (t parent))))

(defmethod world ((object basic-gl-object))
  (loop for obj = object then parent
	for parent = (parent obj) 
	unless parent
	  do (error "object ~a belongs to no world" obj)
	  ;;return obj ; should this be an error?
	unless (typep parent 'basic-gl-object)
	  return parent))

;;; Not clear where these belong.  They are not really the most basic stuff about objects.

(define-soft-slot basic-gl-object :attributes attributes)

(defmethod set-attribute ((object basic-gl-object) key val)
  (setf (attributes object)
	(list* key val (attributes object))))

(defmethod get-attribute ((object basic-gl-object) key)
  (getf (attributes object) key))


;;; Allegro sucks -- no support for inline.
(declaim (inline force-list))

(defun force-list (x)
  (if (listp x)
      x
      (list x)))

;;; Find the chain of object-to-parent-transforms which terminates with the
;;; object-to-parent-transform of an object whose parent is not a
;;; basic-gl-object.
;;; FIXME:  need to allow an optional 2nd argument specifying the target world.
;;;(defmethod object-to-world-transform ((object basic-gl-object))
;;;  (let ((parent (parent object)))
;;;    (if (typep parent 'basic-gl-object)
;;;        ;; this recursion will terminate at NIL, or at non-objects such as a world, or an LVCS.
;;;        (cons (object-to-parent-transform object)
;;;              (force-list (object-to-world-transform parent)))
;;;        (object-to-parent-transform object))))

;;; In its current form, this will fail if OBJECT is a coordinate
;;; frame.  If we introduce one or more intervening coordinate frames
;;; between OBJECT and its top-level world, this function will not be
;;; able to compute the correct transform.  One alternative is to
;;; force all intermediate coordinate frames to be basic-gl-objects,
;;; but then feature set coordinate specs might need to change.

(defun object-to-world-transform-internal (object target-world)
  (typecase object 
    (basic-gl-object
     (let ((parent (parent object)))
       (if parent
	   (cons (object-to-parent-transform object)
		 (force-list (object-to-world-transform-internal parent target-world)))
	   (if target-world
	       (error "object-to-world-transform cannot bridge between 2d-worlds ~a ~a"
		      object target-world)
	       nil	;; should this be an error?
	   ))))
    (local-vertical-coordinate-system
     (if (or (null target-world) (eq object target-world))
	 nil
	 (transforms::cached-optimized-cs-to-cs-transform object target-world)))
    (gl-2d-world 
     (if (or (null target-world) (eq object target-world))
	 nil
	 (error "object-to-world-transform cannot bridge between 2d-worlds ~a ~a" object target-world)))
    (t (error "object-to-world-transform cannot connect ~a to ~a" object target-world))))

(defmethod object-to-world-transform ((object t) &optional world)
  (object-to-world-transform-internal object world))


#|
(object-to-world-transform (gui::selected-object) (cme::get-2d-world-named "alv-oblique-tower"))
(object-to-world-transform (gui::selected-object) (cme::get-2d-world-named "alv-2-44"))
(object-to-world-transform (gui::selected-object) )
(object-to-world-transform (gui::selected-object) (gui::3d-world (gui::top-view)))
(cs-to-cs-transform (cme::get-2d-world-named "alv-2-44") (cme::get-2d-world-named "alv-oblique-tower"))
|#

(defmethod world-to-object-transform (world (object t))
  (inverse-transform (object-to-world-transform object world)))

(defmethod object-to-world-transform-matrix (object &optional world)
  ;(setq *foo* (list object world)) (break)
  (transforms::multiply-4x4-transform-matricies (object-to-world-transform object world)))

(defmethod transform-path ((from-cs gl-3d-object-mixin) (to-cs local-vertical-coordinate-system))
  (let* ((path-to-lvcs (transforms::cs-to-lvcs-transform-path from-cs))
	 (lvcs (to-coordinate-system path-to-lvcs)))
    (transforms::collapse-transforms
     (if (eq lvcs to-cs)
	 path-to-lvcs
	 (transforms::flatten-coordinate-transform-list
	  (list path-to-lvcs (cached-optimized-cs-to-cs-transform lvcs to-cs)))))))

(defmethod transform-path ((from-cs local-vertical-coordinate-system) (to-cs gl-3d-object-mixin))
  (inverse-transform (transform-path to-cs from-cs)))

(defmethod transform-path ((from-cs gl-2d-object-mixin) (to-cs gl-2d-world))
  (let ((path (transforms::cs-to-cs-of-class-transform-path from-cs  'gl-2d-world)))
    (unless (eq (to-coordinate-system path) to-cs)
      (error "transform-path cannot bridge between 2d-worlds ~a ~a" (to-coordinate-system path) to-cs))
    path))

(defmethod transform-path ((from-cs gl-2d-world) (to-cs gl-2d-object-mixin))
  (inverse-transform (transform-path to-cs from-cs)))

(defmethod transform-path ((from-cs basic-gl-object) (to-cs local-vertical-coordinate-system))
  (let* ((path-to-lvcs (transforms::cs-to-lvcs-transform-path from-cs))
	 (lvcs (to-coordinate-system path-to-lvcs)))
    (transforms::collapse-transforms
     (if (eq lvcs to-cs)
	 path-to-lvcs
	 (transforms::flatten-coordinate-transform-list
	  (list path-to-lvcs (cached-optimized-cs-to-cs-transform lvcs to-cs)))))))

#|
(transform-path (gui::selected-object) (lvcs (gdc wgs84) 39.5 -105.125))
(transform-path (lvcs (gdc wgs84) 39.5 -105.125) (gui::selected-object))
(fmakunbound 'transform-path)
|#

#+unused
(defmethod object-to-world-matrix ((object basic-gl-object))
  (object-to-world-matrix (object-to-world-transform object)))

(defmethod update-object-before ((object basic-gl-object)))

(defmethod update-object-after ((object basic-gl-object)))

(defmethod update-object-after ((object gl-object))
  ;;(format t "update-object-after ~a~%" object)
  ;; ascend the composite hierarchy
  (flush-transform-vertices-ht object))

(defvar *update-object-after-recursion* nil)

(defmethod update-object-after :after ((object basic-gl-object))
  ;; ascend the composite hierarchy
  (loop with *update-object-after-recursion* = t
	for o = (parent object) then (parent o)
	while (typep o 'basic-gl-object)
	do (update-object-after o)))

(defmethod update-objects-before ((objects list))
  (loop for obj in objects
	do (update-object-before obj)))

(defmethod update-objects-after ((objects list))
  (loop for obj in objects
	do (update-object-after obj))
  (redisplay-views-containing-objects objects)
  )

(defparameter *default-draw-object-verbose* nil)

(defmethod draw-object ((object basic-gl-object) view)
  (declare (ignorable view))
  (when *default-draw-object-verbose*
    (format t "Undefined method draw-object ~a~%" object))
  )

;;; Should this always return NIL, and require immediate-render objects to have
;;; gl-dynamic-object-mixin?
(defmethod immediate-render-p ((object basic-gl-object))
  (get-prop object :immediate-render-p)) 

(defmethod selectable-p ((object basic-gl-object))
  t)

(defmethod object-graphics-style ((object basic-gl-object) view)
  (ignore view)
  (or (graphics-style object)
      (gethash (type-of object) (default-object-graphics-styles-ht))))

;;; This mixin has a slot, in violation of proposed defstruct extension
(defstruct-class gl-sizable-object-mixin (gl-object)
  ((sizes :initform (cv 1.0 1.0 1.0) :initarg :sizes :accessor sizes)))

(defmethod initialize-instance :after ((object gl-sizable-object-mixin) 
				       &key x-size y-size z-size
				       &allow-other-keys)
  ;; The support of :X-SIZE :Y-SIZE :Z-SIZE init keywords is for compatibility with old CME feature-sets.
  (with-slots (obj::sizes) object
    ;;(break)
    (flet ((setsize(i size)
	     (when size
	       (unless (numberp size)
		 ;;(setq *foo* (list object x-size y-size z-size))
		 (break))
	       ;; Allegro under wINdOEz creates fixnums sometimes.
	       (setf (aref obj::sizes i) (if (floatp size) size (coerce size 'double-float))))))
      (setsize 0 x-size)
      (setsize 1 y-size)
      (setsize 2 z-size))))
  
(defstruct-class gl-dynamic-object-mixin () ())

(defmethod immediate-render-p ((object gl-dynamic-object-mixin))
  t)
