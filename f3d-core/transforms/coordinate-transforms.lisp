(IN-PACKAGE :transforms)

#|
TODO:

Split coordinate-projections into a separate file.

   Coordinate-projection semantics need to be clarified:

      The transform-function of all projections return the vector <u v w>.
         The w computation must be specified.
         Compatibility with OpenGL depth clipping needs to be specified.

      All projections have inverses <u v w> => <x y z>

      For accuracy, the inverses of some projections must be
        computed numerically.  To facilitate this, there
	must be a way to estimate the inverse.

|#

;;;  ***************************   COORDINATE-SYSTEM  *****************************

#| Mon Jan 18 1999 

GL-OBJECT, GL-2D-WORLD, GL-3D-WORLD, and VIEW also inherit from
  BASIC-COORDINATE-SYSTEM

The reason for this is that all of these classes represent objects having some
kind of coordinate system. In FREEDIUS we mixin the BASIC-COORDINATE-TRANSFORM
into each of these objects.

In CME-6, VIEW has multiple coordinate-systems: 2d-world, 3d-world, and window.
In FREEDIUS, coordinate-system of the view is its window-coordinate-system.

|#

;;; BASIC-COORDINATE-SYSTEM is the glue that defines the
;;; CANONICAL COORDINATE-SYSTEM/COORDINATE-TRANSFORM TREE.
;;; For performance, this should inherit from property-list-struct.
;;; The PARENT slot could be eliminated since
;;;    parent = (to-coordinate-system object-to-parent-transform)

(defstruct-class basic-coordinate-system (fasd-form-property-list-mixin) ; should this be changed to  property-list-struct?
    ((parent :initform nil :initarg :parent :accessor parent)
     (children :initform nil :initarg :children :accessor children)
     (object-to-parent-transform :initform nil
				 :initarg :object-to-parent-transform
				 :accessor object-to-parent-transform)
     (name :initform nil :initarg :name :accessor name)
     ))

(defmethod %children ((cs basic-coordinate-system))
  (slot-value cs 'children))

(defmethod (setf %children) (new-children (cs basic-coordinate-system))
  (setf (slot-value cs 'children) new-children))

;(define-soft-slot basic-coordinate-system :name name)
;(name )

(defmethod inferior-of-p ((object basic-coordinate-system) possible-superior)
  (or (eq object possible-superior)
      (let ((parent (parent object)))
	(when parent (inferior-of-p parent possible-superior)))))

(defmethod initialize-instance :after ((cs basic-coordinate-system)
				       &rest initargs
				       &key name &allow-other-keys)
  (with-class-slots basic-coordinate-system (parent object-to-parent-transform) cs
    (cond (object-to-parent-transform
	   (unless parent (setq parent (to-coordinate-system object-to-parent-transform)))
	   (let ((old-from-cs (from-coordinate-system object-to-parent-transform)))
	     (connect-transforms object-to-parent-transform
				 (inverse-transform object-to-parent-transform)
				 cs parent)
	     (when old-from-cs ;; cs inherits properties from old-from-cs
	       (loop for (key val) on (property-list old-from-cs) by #'cddr
		     do (setf (get-prop cs key) val)))))
	  (parent (add-child parent cs)) ; this looks wrong --- no object-to-parent-transform is defined
	  )
    (setf (name cs) name)
    ))

;;;(defmethod print-object ((cs basic-coordinate-system) stream)
;;;   (if t ;*print-escape*
;;;       (format stream "#<~a ~s #X~x>" (type-of cs) (or (name cs) "") (%pointer cs))
;;;       (format stream "~a ~a" (type-of cs) (or (name cs) ""))))

(defmethod print-object ((cs basic-coordinate-system) stream)
  (print-unreadable-object (cs stream :type t :identity t)
    (let ((name (name cs)))
      (when name (prin1 name stream)))))


;;; Perhaps there should be class for HIERARCHICAL-OBJECT, with these methods.
(defmethod remove-child ((cs basic-coordinate-system) child)
  (with-class-slots basic-coordinate-system (children) cs
    (when (memq child children)
      (setf (parent child) nil
	    children (delete child children)))))

(defvar *force-reparent* nil)

(defmethod add-child ((cs basic-coordinate-system) child)
  (when (and (parent child) (not (eq (parent child) cs)))
    ;; When setup-image-worlds is called from the FREEDIUS port of
    ;; Aaron's DOQ system, the child appears to have the wrong
    ;; parent.  Force it if necessary...bad solution, I know, but...
    (if *force-reparent*
	(setf (parent child) cs)
	(break))
    (reparent child cs))
  (setf (parent child) cs)
  (pushnew child (children cs)))

(defmethod map-over-children-int ((cs basic-coordinate-system) fn)
  (loop for child in (children cs)
	do (funcall fn child)))

(defmacro map-over-children ((var cs) &body body)
  `(map-over-children-int ,cs #'(lambda (,var) .,body)))

;;; ********************  WEAK-CHILD-COORDINATE-SYSTEM  *******************

;;; THIS CLASS MIGHT BE UNNECESSARY.

;;; Mixin for BASIC-COORDINATE-SYSTEM that makes WEAK-CHILD pointers from its
;;; parent coordinate-system.

(defstruct-class weak-child-coordinate-system (basic-coordinate-system) ()) 

(defmethod weak-child-handle ((object basic-coordinate-system))
  object)

(defmethod weak-child-handle ((object weak-child-coordinate-system))
  (if config::weak-eval-cache
      (or (get-prop object :weak-object-handle)
	  (setf (get-prop object :weak-object-handle)
		(lx::make-weak-pointer object)))
      object))

(defmethod add-child ((cs basic-coordinate-system) (child weak-child-coordinate-system))
  (when (and (parent child) (not (eq (parent child) cs)))
    ;; When setup-image-worlds is called from the FREEDIUS port of
    ;; Aaron's DOQ system, the child appears to have the wrong
    ;; parent.  Force it if necessary...bad solution, I know, but...
    (if *force-reparent*
	(setf (parent child) cs)
	(break))
    (reparent child cs))
  (setf (parent child) cs)
  (pushnew (weak-child-handle child) (children cs)))

;;;  ********************  CS-CHILDREN-HASH-TABLE-MIXIN  ********************

;;; It is desirable to have a CS-CHILDREN-HASH-TABLE-MIXIN that put
;;; the children an a hashtable for performance when the number of
;;; children can be very large, such as for worlds (lvcs).

(defstruct-class CS-CHILDREN-HASH-TABLE-MIXIN (basic-coordinate-system) ())

(defmethod make-children-hash-table ((cs CS-CHILDREN-HASH-TABLE-MIXIN))
  (make-hash-table ))

(defmethod initialize-instance :after ((cs CS-CHILDREN-HASH-TABLE-MIXIN) &key children &allow-other-keys)
  (setf (%children cs) (make-children-hash-table cs))
  (setf (children cs) children))

;;; What was this for?
(defmethod update-instance-for-different-class :before 
	   ((previous t) (current CS-CHILDREN-HASH-TABLE-MIXIN) &rest initargs)
  (declare (ignore initargs))
  (unless (hash-table-p (%children current))
    (setf (%children current) (make-children-hash-table current))
    (setf (children current) (children previous))
    ))

(defmethod children ((cs CS-CHILDREN-HASH-TABLE-MIXIN))
  (loop with ht = (%children cs)
	for child being the hash-keys of ht
	collect child))

(defmethod map-over-children-int ((cs CS-CHILDREN-HASH-TABLE-MIXIN) fn)
  (loop with ht = (%children cs)
	for child being the hash-keys of ht
	do (funcall fn child)))

(defmethod (setf children) (new-children (cs CS-CHILDREN-HASH-TABLE-MIXIN))
  (let ((ht (%children cs)))
    (clrhash ht)
    (loop for child in new-children
	  do (add-child cs child))
    new-children))

(defmethod remove-child ((cs CS-CHILDREN-HASH-TABLE-MIXIN) child)
  (let ((ht (%children cs))
	(key child))
    (when (gethash key ht) 
      (setf (parent child) nil)
      (remhash key ht))))

(defmethod add-child ((cs CS-CHILDREN-HASH-TABLE-MIXIN) child)
  (setf (parent child) cs)
  (setf (gethash child (%children cs)) t))


;;; **************  CS-WEAK-CHILDREN-HASH-TABLE-MIXIN METHODS  **************

(defstruct-class CS-WEAK-CHILDREN-HASH-TABLE-MIXIN (CS-CHILDREN-HASH-TABLE-MIXIN) ())

#+never 
(progn 

;;; These are moved to weak-children-hash-table-mixin.lisp and
;;; loaded from sysdef-basic-gui.lisp based on config::weak-eval-cache

;;; (defmethod make-children-hash-table ((cs CS-WEAK-CHILDREN-HASH-TABLE-MIXIN))
;;;   (lx::make-weak-hash-table ))

(defmethod children ((cs CS-WEAK-CHILDREN-HASH-TABLE-MIXIN))
  (loop with ht = (%children cs)
	for child being the hash-keys of ht
	collect (lx::follow-weak-pointer child)))

(defmethod map-over-children-int ((cs CS-WEAK-CHILDREN-HASH-TABLE-MIXIN) fn)
  (loop with ht = (%children cs)
	for child being the hash-keys of ht
	do (funcall fn (lx::follow-weak-pointer child))))

(defmethod remove-child ((cs CS-WEAK-CHILDREN-HASH-TABLE-MIXIN) child)
  (let ((ht (%children cs))
	(key (weak-child-handle child)))
    (when (gethash key ht)
      (setf (parent child) nil)
      (remhash key ht))))

(defmethod add-child ((cs CS-WEAK-CHILDREN-HASH-TABLE-MIXIN) child)
  (setf (parent child) cs)
  (setf (gethash (weak-child-handle child) (%children cs)) t))

) ; end progn

(defmethod reparent ((cs basic-coordinate-system) new-parent)
  ;;(break)
  (with-class-slots basic-coordinate-system (parent) cs
    ;;(format t "reparent ~a ~a ~a~%" cs parent new-parent)
    (when (and parent (not (eq parent new-parent)))
      (remove-child parent cs))
    (when new-parent (add-child new-parent cs))))

;;;(defmethod set-parent-transform ((cs basic-coordinate-system)
;;;                                 new-object-to-parent-transform)
;;;  (with-class-slots basic-coordinate-system
;;;        (object-to-parent-transform) cs
;;;    (when (and object-to-parent-transform
;;;             (to-coordinate-system object-to-parent-transform))
;;;      (remove-child (to-coordinate-system object-to-parent-transform) cs))
;;;    (setf object-to-parent-transform new-object-to-parent-transform)
;;;    (when (and new-object-to-parent-transform
;;;               (to-coordinate-system new-object-to-parent-transform))
;;;      (reparent cs (to-coordinate-system new-object-to-parent-transform)))))

(defmethod set-parent-transform ((cs basic-coordinate-system)
				 new-object-to-parent-transform)
  (let ((object-to-parent-transform (object-to-parent-transform cs)))
    (when (not (eq object-to-parent-transform new-object-to-parent-transform))
      (when (and object-to-parent-transform (to-coordinate-system object-to-parent-transform))
	(remove-child (to-coordinate-system object-to-parent-transform) cs))
      (setf (slot-value cs 'object-to-parent-transform) new-object-to-parent-transform)
      ;;(setf (object-to-parent-transform cs) new-object-to-parent-transform)
      )
    (when (and new-object-to-parent-transform
	       (to-coordinate-system new-object-to-parent-transform))
      (reparent cs (to-coordinate-system new-object-to-parent-transform)))))

(defmethod set-parent-transform ((cs basic-coordinate-system)
				 new-object-to-parent-transform)
  (with-class-slots basic-coordinate-system (object-to-parent-transform) cs
    (unless (eq object-to-parent-transform new-object-to-parent-transform)
      (when (and object-to-parent-transform (to-coordinate-system object-to-parent-transform))
	(remove-child (to-coordinate-system object-to-parent-transform) cs))
      (setf object-to-parent-transform new-object-to-parent-transform))
    (when (and new-object-to-parent-transform
	       (to-coordinate-system new-object-to-parent-transform))
      (reparent cs (to-coordinate-system new-object-to-parent-transform)))))

(defmethod (setf object-to-parent-transform) (new-object-to-parent-transform
					      (cs basic-coordinate-system))
  (set-parent-transform cs new-object-to-parent-transform))

;;;(defun connect-transforms (transform inv-transform
;;;                           &optional
;;;                           (from-cs (from-coordinate-system transform))
;;;                           (to-cs (to-coordinate-system transform)))
;;;  (setf (from-coordinate-system transform) from-cs
;;;        (to-coordinate-system transform) to-cs
;;;        (from-coordinate-system inv-transform) to-cs
;;;        (to-coordinate-system inv-transform) from-cs
;;;        (inverse-transform transform) inv-transform
;;;        (inverse-transform inv-transform) transform)
;;;  ;;(setq foo (list transform inv-transform from-cs to-cs (eq (parent from-cs) to-cs) (eq (parent to-cs) from-cs)))
;;;  (when (and from-cs to-cs)
;;;    (cond ((eq (parent from-cs) to-cs)
;;;           (setf (object-to-parent-transform from-cs) transform)
;;;           (add-child to-cs from-cs)
;;;           )
;;;          ((eq (parent to-cs) from-cs)
;;;           (setf (object-to-parent-transform to-cs) inv-transform)
;;;           (add-child from-cs to-cs)))))

(defun connect-transforms (transform inv-transform
			   &optional
			   (from-cs (from-coordinate-system transform))
			   (to-cs (to-coordinate-system transform)))
  (when (and from-cs to-cs)
    ;; new Sat Sep 11 2004
    (let ((old-from-cs (from-coordinate-system transform))
	  (old-to-cs (to-coordinate-system transform)))
      (when (and old-from-cs (eq (object-to-parent-transform old-from-cs) transform))
	(remove-child (parent old-from-cs) old-from-cs)
	(setf (object-to-parent-transform old-from-cs) nil
	      (parent old-from-cs) nil)
	;; Currently, this only occurs when creating a 3d-world with an existing LVCS.
	;; It is probably better to fix 3d-world creation.
	(format t ";;; Flushing eval-cache for ~a~%" old-from-cs)
	(eval-cache-flush old-from-cs))
      (when (and old-to-cs (eq (object-to-parent-transform old-to-cs) inv-transform))
	(remove-child (parent old-to-cs) old-to-cs)
	(setf (object-to-parent-transform old-to-cs) nil
	      (parent old-to-cs) nil)
	(format t ";;; Flushing eval-cache for ~a~%" old-to-cs)
	(eval-cache-flush old-to-cs) ; ugly, but otherwise cached coordinate-systems are broken.
	))

    (unless nil ;(eq from-cs (from-coordinate-system transform))
      
      (cond ((eq (parent from-cs) to-cs)
	     (setf (object-to-parent-transform from-cs) transform)
	     (add-child to-cs from-cs)
	     )
	    ((eq (parent to-cs) from-cs)
	     (setf (object-to-parent-transform to-cs) inv-transform)
	     (add-child from-cs to-cs)))))

  ;;(setq foo (list transform inv-transform from-cs to-cs (eq (parent from-cs) to-cs) (eq (parent to-cs) from-cs)))

  (setf (from-coordinate-system transform) from-cs
	(to-coordinate-system transform) to-cs
	(from-coordinate-system inv-transform) to-cs
	(to-coordinate-system inv-transform) from-cs
	(inverse-transform transform) inv-transform
	(inverse-transform inv-transform) transform))

(defstruct-class coordinate-system (basic-coordinate-system)
  ())

(defclass 2d-coordinate-system-mixin (basic-coordinate-system) ())

(defmethod initialize-instance :after ((cs 2d-coordinate-system-mixin) &rest args)
  (declare (ignore args))
  (setf (get-prop cs :dimensionality) 2))

(defclass 3d-coordinate-system-mixin (basic-coordinate-system) ())
	   
(defmethod initialize-instance :after ((cs 3d-coordinate-system-mixin) &rest args)
  (declare (ignore args))
  (setf (get-prop cs :dimensionality) 3))
    
;;; This merely supplies the semantics that the underlying coordinates are
;;; Cartesian, meaning axes are orthogonal and of equal units
;;; (=> distance is isotropic).
;;; Geocentric and lvcs coordinate systems are built on this.

(defstruct-class cartesian-coordinate-system (coordinate-system) ())

(defstruct-class 2d-cartesian-coordinate-system (2d-coordinate-system-mixin cartesian-coordinate-system) 
  ())

(defstruct-class 3d-cartesian-coordinate-system (3d-coordinate-system-mixin cartesian-coordinate-system)
  ())

(defstruct-class local-vertical-coordinate-system (3d-cartesian-coordinate-system) ())

;;; This doesn't make sense. A coordinate-system is not a transform.
#+broken
(defmethod linear-transform-p ((coordinate-system t))
  nil)

;;; This should be cached
(defun make-coordinate-system
    (name dimensionality &rest init-args
     &key (class 'coordinate-system)
     component-names
     component-units
     property-list
     &allow-other-keys)
  (when (and component-units (not (consp component-units)))
    (setq component-units (loop repeat dimensionality collect component-units)))
  (let ((cs (apply 'make-instance class :name name :property-list property-list init-args)))
    (setf (get-prop cs :dimensionality) dimensionality
	  (get-prop cs :component-names) component-names
	  (get-prop cs :component-units) component-units)
    cs))

(defmethod component-units ((coordinate-system basic-coordinate-system))
  (get-prop coordinate-system :component-units))

(defmethod component-names ((coordinate-system basic-coordinate-system))
  (get-prop coordinate-system :component-names))

(defmethod dimensionality ((coordinate-system basic-coordinate-system))
  (or (get-prop coordinate-system :dimensionality)
      3))

(declaim (special *feet-per-meter*))

;;; This is really wrong unless the coordinate-system is Cartesian.  For Non-Cartesian
;;; coordinate-systems, local-units-per-meter is position dependent, and therefore requires another
;;; argument.  Also, for some coordinate systems, units-per-meter is not isotropic.  For example,
;;; except at the equator, lat and long units map differently to meters.  The computation of
;;; ground-sample-distance (computed using the Jacobian matrix of the coordinate transform)
;;; is probably the appropriate implementation of LOCAL-UNITS-PER-METER.

;;;(defmethod local-units-per-meter ((coordinate-system coordinate-system))
;;;  (or (get-prop coordinate-system :local-units-per-meter)
;;;      (loop with homogeneous = t
;;;            with units-per-meters
;;;            for units-name in (component-units coordinate-system)
;;;            for upm = (case units-name
;;;                        ((feet :feet) *feet-per-meter*)
;;;                        ((meters :meters) 1.0)
;;;                        (otherwise nil))
;;;            collect upm into units-per-meters-list
;;;            unless units-per-meters
;;;              do (setq units-per-meters upm)
;;;            unless (equal upm units-per-meters)
;;;              do (setq homogeneous nil)
;;;            finally
;;;         (return (values (and homogeneous units-per-meters)
;;;                         units-per-meters-list)))))

(defmethod local-units-per-meter ((coordinate-system basic-coordinate-system))
  (or (get-prop coordinate-system :local-units-per-meter)
      (loop with homogeneous = t
	    for units-name in (component-units coordinate-system)
            for upm = (case units-name
                        ((feet :feet) *feet-per-meter*)
                        ((meters :meters) 1.0)
                        (otherwise nil))
	    for units-per-meters = upm then units-per-meters
            collect upm into units-per-meters-list
	    unless (equal upm units-per-meters)
	      do (setq homogeneous nil)
            finally
         (return (values (and homogeneous units-per-meters)
                         units-per-meters-list)))))
#|
(setq cs1 (make-coordinate-system "Geocentric" 3 :component-names '("GX" "GY" "GZ") :component-units 'meters))

(setf (get-prop cs1 :foo) 'bar)
(get-prop cs1 :foo)

|#

;;;  **************************   COORDINATE-TRANSFORM  ***************************


(defstruct-class coordinate-transform (property-list-struct)
  ((transform-function :initform nil :initarg :transform-function)
   (inverse-transform :initform nil :initarg :inverse-transform :accessor inverse-transform)
   (from-coordinate-system :initform nil :initarg :from-coordinate-system :accessor from-coordinate-system)
   (to-coordinate-system :initform nil :initarg :to-coordinate-system :accessor to-coordinate-system)
   ))

;;; should this be specialized on coordinate-transform class?
(defmethod transforms::linear-transform-p ((obj t))  nil)


(defstruct-class coordinate-projection (coordinate-transform)
  ())

(define-soft-slot coordinate-projection :surrogate-projection surrogate-projection)


(defmethod construct-surrogate-projection ((object coordinate-projection) 2d-world)
  (declare (ignore 2d-world))
  nil)
    
;;;(defmethod print-object ((object coordinate-transform) stream)
;;;  (format stream "#<~a (~a to ~a) #X~x>"
;;;          (type-of object)
;;;          (from-coordinate-system object) (to-coordinate-system object)
;;;          (%pointer object)))
    
(defmethod print-object ((object coordinate-transform) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~a to ~a)" (from-coordinate-system object) (to-coordinate-system object))))

(defmacro define-coordinate-transform-method (method-name args &body body)
  (flet	((coordinate-transform-function-name (class-name method-name)
	   (intern (format nil "~a-~a" class-name method-name)
		   (symbol-package class-name))))
    (let ((class-name (cadr (first args))))
      (if (defstruct-class-p class-name)
	  `(defun ,(coordinate-transform-function-name class-name method-name)
	          (,(car (car args)) . ,(cdr args))
	    . ,body)
        
	  `(defmethod ,method-name ,args . ,body))
      )))

(defmethod set-transform-coordinate-systems ((transform coordinate-transform)
					     from-cs to-cs)
  (setf (from-coordinate-system transform) from-cs
	(to-coordinate-system transform) to-cs)
  (let ((inverse (inverse-transform transform)))
    (when inverse
      (setf (from-coordinate-system inverse) to-cs
	    (to-coordinate-system inverse) from-cs))))

;;; This should be flushed.
;;;(defmethod add-related-coordinate-system
;;;           ((from-coordinate-system coordinate-system)
;;;            to-coordinate-system
;;;            coordinate-transform)
;;;  (with-class-slots coordinate-system (coordinate-transforms) from-coordinate-system
;;;    (if (from-coordinate-system coordinate-transform)
;;;        (unless (eq (from-coordinate-system coordinate-transform) from-coordinate-system)
;;;          (error "From-coordinate-system conflicts with from-coordinate-system of transform"))
;;;        (setf (from-coordinate-system coordinate-transform) from-coordinate-system))
;;;    
;;;    (cond ((null (to-coordinate-system coordinate-transform))
;;;           (setf (to-coordinate-system coordinate-transform) to-coordinate-system))
;;;          ((and to-coordinate-system
;;;                (neq (to-coordinate-system coordinate-transform)
;;;                     to-coordinate-system))
;;;           (error "To-coordinate-system conflicts with to-coordinate-system of transform")))
;;;    
;;;    (pushnew coordinate-transform coordinate-transforms :test 'equal)))

;;;(defmethod initialize-instance :after ((coordinate-transform coordinate-transform)
;;;                                       &key &allow-other-keys)
;;;  (with-class-slots
;;;      coordinate-transform (from-coordinate-system to-coordinate-system)
;;;      coordinate-transform
;;;      (when (and to-coordinate-system from-coordinate-system )
;;;        (add-related-coordinate-system from-coordinate-system
;;;                                       to-coordinate-system
;;;                                       coordinate-transform)
;;;        )))

;;; NULL method -- transform classes can specialize this.
(defmethod update-transform ((transform coordinate-transform))
  )

;;; defaulting changed Sat Jun 12 1993 -- to-vector = NIL forces allocation of new vector
;;; Mon Jan  3 1994 -- I was funding it too easy to unintentionally screw up the input-vector
;;; so,  defaulting was changed again.
;;; unsupplied TO-VECTOR no longer causes destructive modification of vector.
;;; TO-VECTOR = T causes destructive modification of vector.
;;; TO-VECTOR = NIL or unsupplied TO-VECTOR forces allocation of new vector.

;;; This is a defun vs a defmethod for performance.
;(disassemble 'transform-vector)
(defun transform-vector (transform vector &optional to-vector)
  #+cmu (declare (ext::optimize-interface (speed 3) (safety 2)))
  (declare (optimize (speed 3) (safety 2)))
  (declare (type (or null coordinate-vector) vector to-vector))
  (when vector
    (if (null to-vector)
	(setq to-vector (make-coordinate-vector (length vector )))
	;; what was this next ???
	;;(when (symbolp to-vector) (setq to-vector vector))
	)
    (if (listp transform)
	(list-transform-vector transform vector to-vector)
	(with-class-slots coordinate-transform (transform-function) transform
	  (funcall transform-function transform vector to-vector))))
  )

(defmethod inverse-transform-vector (transform vector &optional to-vector)
  (if (listp transform)
      (composite-coordinate-transform-inverse-transform-list-vector transform vector to-vector)
      (with-class-slots coordinate-transform (inverse-transform) transform
	(transform-vector inverse-transform vector to-vector))))


;;; *********************  METHODS FOR LISTS OF TRANSFORMS  ************************

(defun list-transform-vector (transform-list from-vector &optional to-vector )
  (when from-vector
    ;; not sure this next is needed
    ;;(unless to-vector (setq to-vector (make-coordinate-vector (length from-vector ))))
    (if (null transform-list)
	(unless (eq from-vector to-vector)
	  (setq from-vector (copy-coordinate-vector from-vector to-vector t)))
	(loop for transform in transform-list
	      while from-vector
	      do (setq from-vector (transform-vector transform from-vector to-vector))))
    ;;(break)
    from-vector))

(defun composite-coordinate-transform-inverse-transform-list-vector (transform-list from-vector to-vector)
  (if (null transform-list)
      (if to-vector
	  (copy-coordinate-vector from-vector to-vector)
	  from-vector)
      (inverse-transform-vector
       (car transform-list)
       (composite-coordinate-transform-inverse-transform-list-vector
	(cdr transform-list) from-vector to-vector)
       to-vector)))

(defun collapse-transforms (transforms)
  (if (and (consp transforms) (null (cdr transforms)))
      (car transforms)
      transforms))

;;;(defmethod inverse-transform ((transform list))
;;;  (collapse-transforms
;;;   (loop for trans in (reverse transform)
;;;         collect (inverse-transform trans))))

(defmethod inverse-transform ((transforms list))
  (collapse-transforms
   (loop with inverse-transforms
	 for trans in transforms
	 do (push (inverse-transform trans) inverse-transforms)
	 finally (return inverse-transforms))))

(defmethod from-coordinate-system ((transform list))
  (and transform	;to prevent infinite recursion if transform is nil
       (from-coordinate-system (car transform))))

(defmethod to-coordinate-system ((transform list))
  (and transform	;to prevent infinite recursion if transform is nil
       (to-coordinate-system (car (last transform)))))



;;; *********************  GENERIC COORDINATE-TRANSFORM METHODS  *********************

(defgeneric transform-jacobian (transform position &optional mat3x3))

#|

TRANSFORM-JACOBIAN computes the jacobian-matrix of the TRANSFORM-FUNCTION of
TRANSFORM at POSITION.  If <u,v,w> = F<x,y,z>, then the jacobian-matrix is:

               | du/dx du/dy du/dz |
        J(F) = | dv/dx dv/dy dv/dz |
               | dw/dx dw/dy dw/dz |

consisting of partial derivitives of the TRANSFORM-FUNCTION evaluated at POSITION.

|#

(defparameter *transform-jacobian-eps* 1.0)
#|
(setq *transform-jacobian-eps* .01)
(setq *transform-jacobian-eps* (/ .03 3600))
|#

(defun TRANSFORM-NUMERICAL-JACOBIAN (projection position 
				     &optional partials (eps *transform-jacobian-eps*))
  (let* ((partials (or partials (make-array '(3 3) :element-type 'double-float)))
	 (tmp-vect (load-time-value* (make-coordinate-vector 3)))
	 (px (load-time-value* (make-coordinate-vector 3)))
	 (py (load-time-value* (make-coordinate-vector 3)))
	 (pz (load-time-value* (make-coordinate-vector 3)))
	 )
    (declare (type (simple-array double-float (3 3)) partials))
    (bind-vector-elements (x y z) position
      (bind-vector-elements (u v w) (transform-vector projection position tmp-vect)
	(let* ((1/eps (/ eps)))
	  (declare (double-float eps 1/eps))
	  (math::inline-set-coordinate-vector-elements px (+ x eps) y z)
	  (bind-vector-elements (ux vx wx)
	      (transform-vector projection px tmp-vect)
	    (math::inline-set-coordinate-vector-elements py x (+ y eps) z)
	    (bind-vector-elements (uy vy wy)
		(transform-vector projection py tmp-vect)
	      (math::inline-set-coordinate-vector-elements pz x y (+ z eps))
	      (bind-vector-elements (uz vz wz)
		  (transform-vector projection pz tmp-vect)
		(setf (aref partials 0 0) (* (- ux u) 1/eps)
		      (aref partials 0 1) (* (- uy u) 1/eps)
		      (aref partials 0 2) (* (- uz u) 1/eps))
	      
		(setf (aref partials 1 0) (* (- vx v) 1/eps)
		      (aref partials 1 1) (* (- vy v) 1/eps)
		      (aref partials 1 2) (* (- vz v) 1/eps))
		
		(setf (aref partials 2 0) (* (- wx w) 1/eps)
		      (aref partials 2 1) (* (- wy w) 1/eps)
		      (aref partials 2 2) (* (- wz w) 1/eps))
					;(when (= (aref partials 2 0) -1.0) (break))
		partials))))))))

;;; Need to fix this to use a vector of epsilon values.
;;; See ~quam/consulting/SRI/PRB/code/transforms.c++
;;; We really need to know the units of the from-coordinate-system to do this correctly.
;;; 
;;; Numerical calculation of the JACOBIAN matrix.
(defmethod TRANSFORM-JACOBIAN ((projection t) position &optional partials)
  (TRANSFORM-NUMERICAL-JACOBIAN projection position partials))


;;; FIXME -- We can do better than this.  For linear transforms we do not
;;; need a position vector.
(defun generic-transform-direction-vector
    (transform direction-vector
	       &optional position-vector into-vector (epsilon .01))
  (unless position-vector
    (error "position vector must be specified"))
  (let* ((p0 (transform-vector transform position-vector nil))
	 (p1 (vector-linear-combine 1.0 position-vector epsilon direction-vector))
	 (p1 (transform-vector transform p1 p1))
	 (eps-inverse (/ epsilon) ))
    (vector-linear-combine eps-inverse p1 (- eps-inverse) p0 (or into-vector p1))))


(defun linear-transform-list-direction-vector (transform-list direction-vector &optional into-vector)
  (loop for trans in transform-list
	do (setq direction-vector (transform-direction-vector trans direction-vector))
	finally (return (if into-vector
			    (copy-coordinate-vector direction-vector into-vector)
			    direction-vector))))
	
	

(defmethod transform-direction-vector ((transform t)
				       direction-vector
				       &optional position-vector into-vector
				       (epsilon .01))
  (if (linear-transform-p transform)
      (linear-transform-list-direction-vector transform direction-vector into-vector)
      (generic-transform-direction-vector transform direction-vector position-vector
					  into-vector epsilon)))



;;; *******************  GENERIC COORDINATE-PROJECTION METHODS  *******************

;;; This version is different from CME version and works wrong for non-nadir views.
;;;(defmethod project-2d-motion-to-3d-motion
;;;           (projection 2d-motion-vector 3d-position &optional dtm)
;;;  (if (not dtm)
;;;      (let* ((jacobian (transform-jacobian projection 3d-position))
;;;             (inv-jacobian (invert-3x3-matrix jacobian))
;;;             (dx 0.0) (dy 0.0))
;;;        (declare (type (simple-array double-float (3 3)) inv-jacobian))
;;;        (declare (double-float dx dy))
;;;        (bind-vector-elements (du dv) 2d-motion-vector
;;;          (math::inline-matrix-times-vector inv-jacobian (du dv) (dx dy))
;;;          (cv dx dy 0.0)))
;;;      
;;;      (error "dtm not supported")))

(defmethod project-2d-motion-to-3d-motion
	   (projection 2d-motion-vector 3d-position &optional dtm)
  (if (not dtm)
      (let* ((jacobian (transform-jacobian projection 3d-position))
	     (inv-jacobian (math::fast-invert-2x3 jacobian))
	     (dx 0.0) (dy 0.0))
	(declare (type (simple-array double-float (2 3)) inv-jacobian))
	(declare (double-float dx dy))
	(bind-vector-elements (du dv) 2d-motion-vector
	  (math::inline-matrix-times-vector inv-jacobian (du dv) (dx dy))
	  (cv dx dy 0.0)))
      
      (error "dtm not supported")))

#| unfinished
(defmethod project-2d-motion-to-3d-motion
	   (projection 2d-motion-vector 3d-position &optional dtm)
  (let* ((jacobian (transform-jacobian projection 3d-position))
	 (inv-jacobian (math::fast-invert-2x3 jacobian))
	 (dx 0.0) (dy 0.0))
    (declare (type (simple-array double-float (2 3)) inv-jacobian))
    (declare (double-float dx dy))
    (bind-vector-elements (du dv) 2d-motion-vector
      (math::inline-matrix-times-vector inv-jacobian (du dv) (dx dy))
      (if (not dtm)
	  (cv dx dy 0.0)
	  (bind-vector-elements (u v) 2d-motion-vector
	    (project-to-world projection (cv (+ u du) (+ v dv)) z)
	  ))))
|#

#| from CME

(defun project-2d-motion-to-3d-motion-internal
    (projection object 2d-motion-vector 3d-position dtm)
  (ignore object)
  (if (not dtm)
      (let ((partials (projection-partial-derivitives projection 3d-position *tmp-projection-partials*)))
	(if partials
	    (let ((inv (fast-invert-3x2 partials)))
	      (declare (type (simple-array double-float (3 2)) inv))
	      ;;(setq foo (list partials inv))
	      ;;(format t ".")
	      (bind-vector-elements (du dv) 2d-motion-vector
		(cv (+ (* (aref inv 0 0) du) (* (aref inv 0 1) dv))
		    (+ (* (aref inv 1 0) du) (* (aref inv 1 1) dv))
		    0.0)))
	    *zero-coordinate-vector*)); always return something other than NIL

      (or (bind-vector-elements (du dv) 2d-motion-vector
	    (bind-vector-elements (x y z) 3d-position
	      (bind-vector-elements (u v) (transform-position projection 3d-position)
		(multiple-value-bind(tx ty tz)
		    ;; track dtm if possible
		    (ic::condition-case
		     ()
		     (project-to-world projection (+ u du) (+ v dv) dtm)
		     (error  nil ))
		  (if tx
		      (progn;;(format t "+")
			(multiple-value-setq-vector-elements (tx ty tz)
			  (compensate-xy-for-round-trip-projection-error
			   projection (coordinate-vector tx ty tz) dtm))
			(coordinate-vector (- tx x) (- ty y) (- tz z)))
		      ;; otherwise try again without DTM
		      (progn;;(format t "-") 
			(project-2d-motion-to-3d-motion projection
							object 2d-motion-vector 3d-position NIL)))))))
	  *zero-coordinate-vector*	; always return something other than NIL
	  )))



|#

;;; This also works
;;;(defmethod project-2d-motion-to-3d-motion2
;;;           (projection 2d-motion-vector 3d-position &optional dtm)
;;;  (if (not dtm)
;;;      (let ((3d-motion
;;;             (transform-direction-vector (inverse-transform projection)
;;;                                         2d-motion-vector
;;;                                         (transform-vector projection 3d-position))))
;;;        (setf (aref 3d-motion 2) 0.0)
;;;        3d-motion)
;;;      (error "dtm not supported")))

#|
(project-2d-motion-to-3d-motion (3d-to-2d-projection (top-view))
				(cv 1.0 0.0)
				(origin (gui::selected-object)))

(project-2d-motion-to-3d-motion2 (3d-to-2d-projection (top-view))
				(cv 1.0 0.0 0.0)
				(origin (gui::selected-object)))
|#

;;;(defmethod camera-direction-vector ((projection t) 3d-pt)
;;;  (let ((partials (transform-jacobian projection 3d-pt)))
;;;    (declare (type (simple-array double-float (* *)) partials))
;;;    (normalize-coordinate-vector 
;;;     ;; Might be able to avoid the vector-cross-product by computing the
;;;     ;; partials of the projection-matrix of the inverse-transform.
;;;     (vector-cross-product
;;;      (cv (aref partials 1 0) (aref partials 1 1) (aref partials 1 2))
;;;      (cv (aref partials 0 0) (aref partials 0 1) (aref partials 0 2))))))

(defmethod camera-direction-vector ((projection t) 3d-pt)
  (let ((inv-jacobian (invert-matrix  (transform-jacobian projection 3d-pt))))
    (declare (type (simple-array double-float (* *)) inv-jacobian))
    (normalize-coordinate-vector
     (cv (aref inv-jacobian 0 2) (aref inv-jacobian 1 2) (aref inv-jacobian 2 2)))))


#|
 This computes the sqrt(area) of the projection of a unit pixel onto the plane
thru pt with the specified normal direction vector, which must be a unit vector.

Tue Mar 16 1999.  The rigorous derivation of this formula is possible but
non-trivial.  See hand written notes in Quam's "Generic Sensor" notebook.

Note that the (method compute-gsd (frame-camera t)) uses a totally different
computation the gets results that are consistent to 6 digits.

The theory is based on LR decomposition of the Jacobian.  The R matrix rotates
world coordinates into a coordinate system whose z axis is the camera direction vector.
The components of the L matrix determine how to map a unit circle in the image
coordinate system into an ellipse in the rotated world coordinate system.
These ellipse parameters define the GSD.

In normal is NIL, the normal is set to the camera-direction-vector.

|#

(defmethod compute-gsd ((projection t) position &optional normal)
  (let* ((m (transform-jacobian projection position)))
    (when m
      ;; This looks wrong: should matrix elements be transposed?
      ;; No, transposing produces bad results.
      ;; Numerically this closely agrees with another implementation which I believe.
      ;; Need to recover the theory of why this works.
      (let* ((m00 (aref m 0 0)) (m01 (aref m 0 1)) (m02 (aref m 0 2))
	     (m10 (aref m 1 0)) (m11 (aref m 1 1)) (m12 (aref m 1 2))
	     (L0 (+ (^2 m00) (^2 m01) (^2 m02)))
	     (L1 (+ (^2 m10) (^2 m11) (^2 m12)))
	     (L01 (inline-inner-prod (m00 m01 m02) (m10 m11 m12)))
	     ;; gsd is the area on plane thru pt normal to camera direction vector
	     (gsd (the double-float
		      (sqrt (the double-float
				(/ (the double-float
				       (sqrt (- (* L0 L1) (^2 L01))))))))))
	(declare (type (simple-array double-float (3 3)) m))
	(declare (double-float m00 m01 m02 m10 m11 m12 L0 L1 L01 gsd))
	(if normal
	    (let ((dx 0.0d0) (dy 0.0d0) (dz 0.0d0))
	      (declare (double-float dx dy dz))
	      ;; compute camera direction vector
	      (inline-cross-prod (m00 m01 m02) (m10 m11 m12) (dx dy dz)) ; camera direction vector
	      (normalize-vector-elements dx dy dz)
	      ;;(format t "camera direction vector = ~a~%" (cv dx dy dz))
	      (bind-vector-elements (nx ny nz) normal
		(/ gsd (sqrt (abs (inline-inner-prod (dx dy dz) (nx ny nz)))))))

	    gsd)))))


;;; This isn't quite right: should be
;;; (expt (abs (determinant jacobian)) (/ pwr)) where pwr is 2 or 3.
;;; Can this be replaced by (/ (compute-gsd transform position)) ?
;;;(defmethod transform-scale-factor ((transform coordinate-transform) position)
;;;  (ignore position)
;;;  (let ((jacobian  (transform-jacobian transform position)))
;;;    (euclidean-length (aref jacobian 0 0)
;;;                      (aref jacobian 1 0)
;;;                      (aref jacobian 2 0))))

(defmethod transform-scale-factor (transform position)
  (/ (compute-gsd transform position)))


#|  Experiments verifying the correctness of the compute-gsd calculation

(defun 2d-triangle-area (x1 y1 x2 y2 x3 y3)
  (let* ((dx12 (- x2 x1)) (dy12 (- y2 y1))
	 (dx13 (- x3 x1)) (dy13 (- y3 y1)))
    (* .5 (- (* dy12 dx13) (* dx12 dy13)))))


;;; compute-gsd3 agrees with compute-gsd to 6 digits when eps = .01

;;; This version is based on constructing orthogonal unit axes U V W with the W axis aligned with
;;; the camera direction vector.  A unit triangle in the U-V plane is projected into the image.
;;; The gsd in the U-V plane is (sqrt (/ .5 triangle-area)).  gsd in a different plane is
;;; (/ (sqrt (/ .5 triangle-area)) (inner-product  camera-direction-vector normal))
(defmethod compute-gsd3 ((projection t) position &optional normal (eps 1.0))
  (let* ((m (transform-jacobian projection position)))
    (let* ((wx 0.0) (wy 0.0) (wz 0.0)) 
      ;; Camera direction vector
      (inline-cross-prod ((aref m 0 0) (aref m 0 1) (aref m 0 2))
			 ((aref m 1 0) (aref m 1 1) (aref m 1 2))
			 (wx wy wz))
      (let* ((vx 0.0) (vy wz) (vz (- wy)) ; cross-prod(<wx wy wz> <1,0,0>)
	     (ux 0.0) (uy 0.0) (uz 0.0))
	(inline-cross-prod (vx vy vz) (wx wy wz) (ux uy uz))
	(normalize-vector-elements vx vy vz)
	(normalize-vector-elements ux uy uz)
	;; We now have orthogonal unit axes
	(bind-vector-elements (x0 y0 z0) position
	  (bind-vector-elements (u0 v0)
	      (transform-vector projection position)
	    (bind-vector-elements (u1 v1)
		(transform-vector projection
				  (cv (+ x0 (* eps ux)) (+ y0 (* eps uy)) (+ z0 (* eps uz))))
	      (bind-vector-elements (u2 v2)
		  (transform-vector projection
				    (cv (+ x0 (* eps vx)) (+ y0 (* eps vy)) (+ z0 (* eps vz))))
		;; We have now projected a unit triangle into the image.
		(let ((gsd (* eps (sqrt (/ .5 (abs (2d-triangle-area u0 v0 u1 v1 u2 v2)))))))
		  (if (null normal)
		      gsd
		      (bind-vector-elements (nx ny nz) normal
			(normalize-vector-elements wx wy wz)
			(/ gsd (sqrt (inline-inner-prod (wx wy wz) (nx ny nz)))))))))))))))

(defmethod camera-position ((projection frame-camera))
  (let ((m (invert-matrix (3d-to-camera-matrix projection))))
    (declare (type 4x4-matrix m))
    (cv (aref m 0 3) (aref m 1 3) (aref m 2 3))))

#+incomplete
(defmethod world-point-at-range (projection u v range)
  (let ((cam-pos (camera-position projection))
	(direction (camera-direction-vector projection (cv u v))))))
  

(defmethod focal-length ((projection frame-camera))
  (let ((m (projection-matrix projection)))
    (declare (type 4x4-matrix m))
    (/ (abs (aref m 3 2)))))

(defmethod camera-direction-vector ((projection t) position)
  (let* ((m (transform-jacobian projection position)))
    (let* ((wx 0.0) (wy 0.0) (wz 0.0)) 
      ;; Camera direction vector
      (inline-cross-prod ((aref m 0 0) (aref m 0 1) (aref m 0 2))
			 ((aref m 1 0) (aref m 1 1) (aref m 1 2))
			 (wx wy wz))
      (normalize-vector-elements wx wy wz)
      (cv wx wy wz))))

;;; This agrees with compute-gsd when normal = (cv 0.0 0.0 1.0)
;;; compute-gsd4 does not correctly handle normal
(defmethod compute-gsd4 ((projection t) position)
  (bind-vector-elements (cx cy cz) (camera-position projection)
    (bind-vector-elements (x0 y0 z0) position
      (let ((m (3d-to-camera-matrix projection)))
	(declare (type 4x4-matrix m))
	(let ((d (inline-inner-prod ((aref m 2 0) (aref m 2 1) (aref m 2 2))
				    ((- cx x0) (- cy y0) (- cz z0)))))
	  (/ d (focal-length projection)))))))



|#

#|
(transform-scale-factor (2d-to-window-transform (top-view)) nil)
(/ (compute-gsd (2d-to-window-transform (top-view)) nil))
(transform-jacobian (2d-to-window-transform (top-view)) nil)
(compute-gsd (2d-to-window-transform (top-view)) nil)
(compute-gsd2 (2d-to-window-transform (top-view)) nil)

(camera-direction-vector (3d-to-2d-projection (top-view)) (origin (gui::selected-object)))
(let ((proj (3d-to-2d-projection (top-view)))
      (pt (origin (gui::selected-object))))
  (compute-gsd proj pt (camera-direction-vector proj pt)))

(compute-gsd (3d-to-2d-projection (top-view)) (origin (gui::selected-object)) )
(compute-gsd3 (3d-to-2d-projection (top-view)) (origin (gui::selected-object)) )
(compute-gsd3 (3d-to-2d-projection (top-view)) (origin (gui::selected-object)) nil .1)
(compute-gsd3 (3d-to-2d-projection (top-view)) (origin (gui::selected-object)) nil .01)
(compute-gsd4 (3d-to-2d-projection (top-view)) (origin (gui::selected-object)))

(compute-gsd2 (3d-to-2d-projection (top-view)) (origin (gui::selected-object)) )
(compute-gsd2 (3d-to-2d-projection (top-view)) (origin (gui::selected-object)) nil .1)
(compute-gsd2 (3d-to-2d-projection (top-view)) (origin (gui::selected-object)) nil .01)

(compute-gsd (3d-to-2d-projection (top-view)) (origin (gui::selected-object)) (cv 0.0 0.0 1.0))
(compute-gsd3 (3d-to-2d-projection (top-view)) (origin (gui::selected-object)) (cv 0.0 0.0 1.0) .01)

(compute-gsd (3d-to-2d-projection (top-view))
	      (origin (gui::selected-object)) (cv 0.0 0.0 1.0 0.0))

(compute-gsd (list (3d-to-2d-projection (top-view))
		   (2d-to-window-transform (top-view)))
	     (origin (gui::selected-object)))

(4x4-projection-partials (4x4-projection-projection-matrix
			  (3d-to-2d-projection (top-view)))
			 (origin (gui::selected-object)))

(setq *print-array* t)

(describe (3d-to-2d-projection (top-view)))
(describe (PROJECTION-MATRIX (3d-to-2d-projection (top-view))))

(describe (3D-TO-CAMERA-MATRIX (3d-to-2d-projection (top-view))))
(describe (invert-matrix (3D-TO-CAMERA-MATRIX (3d-to-2d-projection (top-view)))))
(describe (inverse-transform (3d-to-2d-projection (top-view))))
(camera-position (3d-to-2d-projection (top-view)))
(focal-length (3d-to-2d-projection (top-view)))
(origin (gui::selected-object))

(let* ((projection (3d-to-2d-projection (top-view)))
       (position (origin (gui::selected-object)))
       (v1 (camera-direction-vector projection position)))
  (bind-vector-elements (cx cy cz) (camera-position projection)
    (bind-vector-elements (x0 y0 z0) position
      (let ((dx (- cx x0))
	    (dy (- cy y0))
	    (dz (- cz z0)))
	(normalize-vector-elements dx dy dz)
	(bind-vector-elements (vx vy vz) v1
	  (inline-inner-prod (vx vy vz) (dx dy dz)))))))
|#

;;; ******************   FUNCTIONAL-COORDINATE-TRANSFORM  ************************

(defstruct-class functional-coordinate-transform (coordinate-transform)
  ((functional-transform-info :initform nil 
			      :initarg :functional-transform-info
			      :accessor functional-transform-info))
  )
     		    
(defun make-functional-coordinate-transform
    (transform-function transform-info
     &rest initargs
     &key from-coordinate-system to-coordinate-system
     inverse-transform property-list
     &allow-other-keys)
  (ignore from-coordinate-system to-coordinate-system inverse-transform property-list)
  (apply 'make-instance 'functional-coordinate-transform
         :transform-function transform-function
         :functional-transform-info transform-info
         initargs))



;(disassemble 'transform-2d-bbox)
;;; This returns NIL is ANY of the corner points fail in transform-vector.
(defun transform-2d-bbox (transform bbox &optional into-bbox)
  (declare (optimize (speed 3) (safety #+cmu 0 #-cmu 1)))
  (declare (type dvector bbox))
  (declare (type (or null dvector) into-bbox))
  (bind-vector-elements (umin umax vmin vmax) bbox
    (let ((tmp-vector (load-time-value* (make-coordinate-vector 3))))
      (bind-vector-elements (u0 v0)
	  (transform-vector transform
			    (inline-set-coordinate-vector-elements
			     tmp-vector umin vmin 0.0)
			    tmp-vector)
	(bind-vector-elements (u1 v1)
	    (transform-vector transform
			      (inline-set-coordinate-vector-elements
			       tmp-vector umin vmax 0.0)
			      tmp-vector)
	  (bind-vector-elements (u2 v2)
	      (transform-vector transform
				(inline-set-coordinate-vector-elements
				 tmp-vector umax vmin 0.0)
				tmp-vector)
	    (bind-vector-elements (u3 v3)
		(transform-vector transform
				  (inline-set-coordinate-vector-elements
				   tmp-vector umax vmax 0.0)
				  tmp-vector)
	      (inline-set-coordinate-vector-elements
	       (or into-bbox (make-coordinate-vector 4))
	       (min u0 u1 u2 u3)
	       (max u0 u1 u2 u3)
	       (min v0 v1 v2 v3)
	       (max v0 v1 v2 v3)))))))))

(declaim (type fixnum *transform-bounding-box-miss-count-reject-threshold*))
(defparameter *transform-bounding-box-miss-count-reject-threshold* 8)

;;; This currently works only for 2d and 3d bboxes.
(defun transform-bounding-box (transform bbox &optional into-bbox)
  (declare (optimize (speed 3) (safety #+cmu 0 #-cmu 1)))
  (declare (type dvector bbox))
  (declare (type (or null dvector) into-bbox))
  (when bbox
    (let ((bbox-length (length bbox)))
      (declare (fixnum bbox-length))
      (cond ((null transform)
	     bbox)
	    ((= bbox-length 4)
	     (transform-2d-bbox transform bbox into-bbox))
	  
	    ((= bbox-length 6)
	     (bind-vector-elements (xmin xmax ymin ymax zmin zmax) bbox
	       (let ((rxmin most-positive-double-float)
		     (rxmax most-negative-double-float)
		     (rymin most-positive-double-float)
		     (rymax most-negative-double-float)
		     (rzmin most-positive-double-float)
		     (rzmax most-negative-double-float)
		     (rx 0.0d0) (ry 0.0d0) (rz 0.0d0)
		     (miss-count 0)
		     (vector (load-time-value* (make-coordinate-vector 3))))
		 (declare (double-float rx ry rz rxmin rxmax rymin rymax rzmin rzmax))
		 (declare (type (simple-array double-float (*)) vector))
		 (declare (fixnum miss-count))
		 (macrolet ((transform (x y z)
			      `(progn
				(setf (aref vector 0) ,x
				 (aref vector 1) ,y
				 (aref vector 2) ,z)
				(cond ((transform-vector transform vector vector )
				       (setf rx (aref vector 0)
					     ry (aref vector 1)
					     rz (aref vector 2)) 
				       t)
				      (t (incf miss-count)
					 nil)))))
				
		   (macrolet ((minmax (x y z)
				`(progn
				  (when (transform ,x ,y ,z)
				    (when (< rx rxmin) (setq rxmin rx))
				    (when (> rx rxmax) (setq rxmax rx))
				    (when (< ry rymin) (setq rymin ry))
				    (when (> ry rymax) (setq rymax ry))
				    (when (< rz rzmin) (setq rzmin rz))
				    (when (> rz rzmax) (setq rzmax rz)))
				  )))
		     (minmax xmin ymin zmin)
		     (minmax xmax ymin zmin)
		     (minmax xmin ymax zmin)
		     (minmax xmax ymax zmin)
		     (minmax xmin ymin zmax)
		     (minmax xmax ymin zmax)
		     (minmax xmin ymax zmax)
		     (minmax xmax ymax zmax)
	      
		     (unless into-bbox (setq into-bbox (make-coordinate-vector 6)))
		     (cond ((and (>= rxmax rxmin) (>= rymax rymin) (>= rzmax rzmin))
			    (values (inline-set-coordinate-vector-elements
				     into-bbox rxmin rxmax rymin rymax rzmin rzmax)
				    miss-count))
			   ((>= miss-count *transform-bounding-box-miss-count-reject-threshold*)
			    (values nil miss-count))
			   (t (values (inline-set-coordinate-vector-elements into-bbox 0.0 0.0 0.0 0.0 0.0 0.0)
				      miss-count)))
		     )))))

	    (t (error "Cannot transform a bbox of length ~a" bbox-length))))))

