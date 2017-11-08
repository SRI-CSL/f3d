(in-package :gui)

#|
TODO:
  All Worlds need a bounding-box.
    Important for various coordinate-transforms, particularly those
    that must compute inverses numerically, and for performing
    least-squares fits.
|#

(eval-when (eval load compile)
(pushnew :object-name-hash-tables *features*)
)

;;; Mixins are ok here since coordinate-systems are built on standard-class, not structure-class.
;;; basic-world-mixin should be renamed to basic-world-mixin-mixin

;;; Problems using transforms::CS-WEAK-CHILDREN-HASH-TABLE-MIXIN

(defstruct-class basic-world-mixin (transforms::CS-WEAK-CHILDREN-HASH-TABLE-MIXIN)
  ((feature-sets :initform nil :accessor feature-sets) 
   #+object-name-hash-tables
   (object-name-hash-table :initform (make-hash-table :test 'equal)
			   :accessor object-name-hash-table)
   ))


(define-soft-slot basic-world-mixin :selected-feature-set selected-feature-set)

(defmethod coordinate-system ((world basic-world-mixin))
  world)

;;; For 3d-worlds, should this take the terrain model into account?
;;; For 2d-worlds, should this take the base-image into account?
(defmethod obj::bounding-box ((obj basic-world-mixin)  
			      &key (bbox (cv 0.0 0.0 0.0 0.0 0.0 0.0)) transform)
  (with-slots (feature-sets) obj
    (loop for fs in feature-sets
	  do (obj::bounding-box fs :bbox bbox :transform transform)))
  bbox)


#-merged-object-sets
(progn
(defmethod feature-set-object-sets ((fs feature-set))
  (mv-bind (display-list-objects immediate-objects)
      (separate-immediate-render-objects (inferiors fs))
    (nconc (when display-list-objects
	     (let ((object-set (make-object-set display-list-objects)))
	       (setf (get-prop object-set :feature-set) fs)
	       (list object-set)))
	   (when immediate-objects
	     (let ((object-set (make-object-set immediate-objects :immediate-render-p t)))
	       (setf (get-prop object-set :feature-set) fs)
	       (list object-set))))))

(defmethod (setf default-object-sets) (new-object-sets (world basic-world-mixin))
  (setf (get-prop world :default-object-sets) new-object-sets))

;;; DEFAULT-OBJECT-SETS constructs object-sets from the feature-sets of the world.
;;; The relation between feature-sets and object-sets needs to be rethought.
(defmethod default-object-sets ((world basic-world-mixin))
  (or (get-prop world :default-object-sets)
      (setf (default-object-sets world)
	    (with-slot-values (feature-sets) world
	      (loop for fs in feature-sets
		    append (feature-set-object-sets fs))))))

) ; end progn

#+merged-object-sets
(progn

(defmethod (setf default-object-sets) (new-object-sets (world basic-world-mixin))
  (break "(setf default-object-sets) shouldn't happen")
  (setf (get-prop world :default-object-sets) new-object-sets))

;;; FIXME This looks totally bogus. 
;;;  (setf (default-object-sets world) x) followed by (default-object-sets world) doesn't return x.
(defmethod default-object-sets ((world basic-world-mixin))
  (feature-sets world))

) ; end progn
 
;;; *******************************  FIND-FEATURE-SET  *******************************

(defmethod find-feature-set ((world basic-world-mixin) fs-name &optional create-fs-of-class)
  (with-slots (feature-sets) world
    (loop for fs in feature-sets
	  when (equal (name fs) fs-name)
	    return fs
	  finally
	    (when create-fs-of-class
	      (return (add-feature-set (make-instance create-fs-of-class :name fs-name :world world) world))))))

;;; *******************************  FIND-OBJECT-NAMED *******************************

#+object-name-hash-tables
(progn 

(defmethod find-object-named ((world basic-world-mixin) object-name)
  (gethash object-name (object-name-hash-table world)))

(defmethod (setf find-object-named) (object (world basic-world-mixin) object-name)
  (if object
      (setf (gethash object-name (object-name-hash-table world)) object)
      (remhash object-name (object-name-hash-table world))))

) ; end progn


#-object-name-hash-tables
(progn

(defmethod find-object-named ((world basic-world-mixin) object-name)
  (loop for fs in (feature-sets world)
	thereis (find-object-named fs object-name)))

(defmethod find-object-named ((os object-set) object-name)
  (loop for obj being the hash-keys of (object-ht os)
	when (equal (name obj) object-name)
	  return obj))

(defmethod (setf find-object-named) (object (world basic-world-mixin) object-name)
  (when object
    (setf (name object) object-name)))
	
) ; end progn

(defmethod (setf find-object-named) (object (world t) object-name) nil)

;;; *******************************  REMOVE-SELF  *******************************


;;; This totally removes an object fom its superior and its feature-sets.
;;; In theory, this allows the object to be garbage-collected.
;;; (There may be some temporary caching in object-sets).
(defmethod remove-self ((object basic-object))
  #+object-name-hash-tables 
  (setf (gethash (name object) (object-name-hash-table (world object))) :deleted-object)

  (remove-child object (parent object))

  (loop for fs in (object-feature-sets object)
	do (remove-object object fs))  
  ;;(setf (object-feature-sets object) nil)

  )

;;; *******************************  ADD-FEATURE-SET  *******************************

(defmethod add-feature-set ((feature-set feature-set) (world basic-world-mixin) &key &allow-other-keys)
  (with-slots (feature-sets) world
    ;;(pushnew feature-set feature-sets)
    (let* ((name (name feature-set))
	   (another-fs-with-same-name (find-feature-set world name)))
      (when (eq another-fs-with-same-name feature-set) (setq another-fs-with-same-name nil))
      ;; This allows a feature-set to be reloaded from a file, flushing the old feature-set
      (when another-fs-with-same-name
	(format t "Warning: adding feature-set ~a to ~a which already has a feature set of the same name ~a~%
The old feature set will be removed~%"
		feature-set world name)
	(remove-feature-set another-fs-with-same-name world)
	(map-over-all-world-views (world view)
	  (when (member another-fs-with-same-name (object-sets view))
	    (remove-object-set another-fs-with-same-name view)
	    (add-object-set feature-set view)
	    (view-changed (view-window view))
	    )))	
      (unless (member feature-set feature-sets )
	(setq feature-sets (append feature-sets  (list feature-set))))
      (unless (eq (world feature-set) world)
	(if (null (world feature-set))
	    (set-world feature-set world)
	    (format t ";;; Warning: adding feature set ~a in world ~a to a different world ~a."
		    feature-set (world feature-set) world )))
      #+never ;; No!  The objects should already be in the world-namespace.
      (loop for obj in (inferiors feature-set)
	    ;; Add the object to the object-name-hash-table of the world.  This is done
	    ;; to make it simple to find an object by name rather than searching the coordinate-system 
	    ;; tree of the world, which is certainly another alternative.
	    do (add-object-to-world-namespace feature-set obj))
      #-merged-object-sets
      (setf (default-object-sets world) nil) ; force recomputation of the default-object-sets
      feature-set)))

;;; *******************************  REMOVE-FEATURE-SET  *******************************

(defmethod remove-feature-set ((feature-set feature-set) (world basic-world-mixin))
  (with-slots (feature-sets) world
    (setq feature-sets (removef feature-set feature-sets))
    #+never ; No!  remove-feature-set should not affect the world-namespace
    (loop for obj in (inferiors feature-set)
	  ;; FIXME:  This is wrong when the same object is in multiple feature-sets.
	  do (remove-object-from-world-namespace feature-set obj))
    (map-over-all-world-views (world view)
      (when (member feature-set (object-sets view))
	(remove-object-set feature-set view)
	(view-changed (view-window view))))
    #-merged-object-sets
    (setf (default-object-sets world) nil) ; force recomputation of the default-object-sets
    nil))

;;; ***********************  2D-WORLD   ***********************

;;; Should IMAGE-LIST be flushed, using CHILDREN slot?
;;; Probably not, since views are also children of 2d-worlds.
(defstruct-class 2d-world (gl-2d-world basic-world-mixin)
     ((image-list :initform nil :accessor image-list)))

;;#+weak-image-list
(progn

(defmethod image-list ((world 2d-world))
  (if config::weak-eval-cache
      (loop for wp in (slot-value world 'image-list)
	    collect (lx::follow-weak-pointer wp))
      (slot-value world 'image-list)))

(defmethod (setf image-list) (image-list (world 2d-world))
  (setf (slot-value world 'image-list)
	(if config::weak-eval-cache
	    (loop for img in image-list
		  collect (lx::weak-object-handle img))
	    image-list)))

) ; end progn
	

#+broken
(progn

;;; This will not work until images inherit from BASIC-COORDINATE-SYSTEM,
;;; which will probably never happen because images are built on STRUCTURE-CLASS.
(defmethod image-list ((world gl-2d-world))
  (loop for thing in (children world)
	when (typep thing 'image)
	  collect thing))

(defmethod (setf image-list) (new-image-list (world gl-2d-world))
  (setf (children world)
	(append new-image-list
		(loop for thing in (children world)
		      unless (typep thing 'image)
			collect thing))))

); end progn

;;; This is sorta wierd - how can a coordinate system be a transform?
;;; This this really used?
(defmethod transforms::linear-transform-p ((2d-world 2d-world))
  (break "If this break point is encountered, look at backtrace and place comments in code,
and then remove the break-point.")
  (transforms::linear-transform-p (3d-to-2d-projection 2d-world)))

;;; strip off the " 2d World" suffix to the name
(defun fixup-2d-world-name (name)
  (when (and name (> (length name) 9)
	     (string-equal name " 2d World" :start1 (- (length name) 9) :end1 (length name)))
    (substring name 0 (- (length name) 9))))

(defmethod initialize-instance :after ((world 2d-world) &key 3d-world name &allow-other-keys)
  (when 3d-world (pushnew world (2d-worlds 3d-world)))
  (with-slots (3d-to-2d-projection) world
    (if name
	(setf (name world) (or (fixup-2d-world-name name) name))
	(generate-name world))
    #+unfinished
    (when (and 3d-to-2d-projection (inverse-transform 3d-to-2d-projection))
      (copy-transform-coordinate-systems 3d-to-2d-projection 3d-to-2d-projection))
    ))

(defun make-2d-world (&rest args)
  (apply 'make-instance '2d-world args))

(defparameter *3d-worlds* nil)

;;; Why not use a hash-table instead of 2 levels of loops?
;;; The only advantage I can see in this implementation is when worlds are deleted
;;; nothing is required to keep this search up-to-date.
;;; This will not find LVCS that is not a 3d-world.
(defun get-2d-world-named (name &optional (3d-worlds *3d-worlds*))
  (unless (listp 3d-worlds) (setq 3d-worlds (list 3d-worlds)))
  (loop for 3d-world in 3d-worlds
	thereis (loop for 2d-world in (2d-worlds 3d-world)
		      when (string-equal (name 2d-world) name)
			return 2d-world)))

(defun get-or-make-2d-world (&rest initargs
				   &key (2d-world-class '2d-world)
				   name 3d-world
				   3d-world-name
				   &allow-other-keys)
  (or (get-2d-world-named name (list (or 3d-world (get-or-make-3d-world :name 3d-world-name))))
      (apply 'make-instance 2d-world-class initargs)))


;;; ********************************  3D-WORLD  ******************************** 

;;; LHQ Sun Oct 3 2004: 

;;; We really need to remove the FEATURE-SET aggregation functionality from
;;; 3D-WORLD (really BASIC-WORLD-MIXIN).  In a multi-lvcs environment, it doesn't make
;;; much sense to associate feature-sets with any particular LVCS.  Another
;;; object class, like a SITE class is needed for this aggregation and a general
;;; purpose database system might be used to perform the aggregations and
;;; searches.
	   		  
(defstruct-class 3d-world (gl-3d-world basic-world-mixin) ())

;;; A note about time and 3d-worlds: 3D coordinate systems can have an
;;; explicit or implicit temporal reference, denoted right now by a
;;; property :TIME-BASE on the property-list of the world.  If this is
;;; not specified, then we use Common Lisp Universal Time as the
;;; unique implicit temporal reference.  Common Lisp Universal time is
;;; the number of seconds since midnight, Jan 1, 1900 GMT.


(defun string-last-word (string)
  (let ((pos (position #\space string :from-end t)))
    (if pos
	(substring string (1+ pos))
	string)))

(defmethod get-site-named (name)
  (loop for 3d-world in *3d-worlds*
	when (string-equal name (string-last-word (get-prop 3d-world :site-name)))
	  return 3d-world))

(defmethod get-world-named (name)
  (if (stringp name)
      (get-3d-world-named name)
      (let ((3d-world (get-3d-world-named (car name))))
	(if (cadr name)
	    (get-2d-world-named (cadr name))
	    3d-world))))

(defun get-3d-world-named (name &optional lvcs)
  (or (loop for 3d-world in *3d-worlds*
	    when (string-equal (name 3d-world) name)
	      return 3d-world)
      (get-site-named name)
      ))

#|
(get-3d-world-named "alv")
|#

(defmethod initialize-instance :after ((world 3d-world) &rest initargs &key &allow-other-keys)
  (when (get-3d-world-named (name world))
    (break))
  (pushnew world *3d-worlds*))

;;; This guarantees that the 3d-world and the lvcs are the same object.
(defun possibly-coerce-lvcs-to-3d-world (name lvcs-to-gdc units-per-meter)
  (let ((lvcs (and lvcs-to-gdc (from-coordinate-system lvcs-to-gdc))))
    (when lvcs
      (unless (parent lvcs)
	(setf (parent lvcs) (to-coordinate-system lvcs-to-gdc)
	      (object-to-parent-transform lvcs) lvcs-to-gdc
	      (get-prop lvcs :units-per-meter) 
	      (or units-per-meter (get-prop lvcs :local-units-per-meter) 1.0)
	      )
	))
    (lx::set-eval-cache (transforms::lvcs-with-name name) (from-coordinate-system lvcs-to-gdc))
    ;;(break)
    (cond ((typep lvcs '3d-world)
	   lvcs)
	  ((null lvcs)
	   nil)
	  (t (pushnew lvcs *3d-worlds*)
	     (setf (name lvcs) name)
	     ;; This is sorta ugly, but the best we can do for now.
	     ;(format t "change-class ~a to 3d-world~%" lvcs)
	     (change-class lvcs '3d-world)
	     ;(describe lvcs)
	     lvcs
	     )
	  )))
      
#|
(setq *
      (possibly-coerce-lvcs-to-3d-world 
       "Radius Alv 3d World" 
       (make-lvcs (transforms::GDC NAD27) 39.5 (from-deg-min-sec -105 7 15.000) 
		  :units *feet-per-meter*
		  :name "Radius Alv 3d World")))

(get-3d-world-named "Radius Alv 3d World")
(pop *3d-worlds*)
(transforms::lvcs-with-name "SUAV Site 1")
(name (transforms::make-lvcs (transforms::GDC WGS84) 
			     42.49484154087466 -71.67634637264614 
			     :name "SUAV Site 1"))
|#
    
(defun get-or-make-3d-world (&rest initargs &key 
			     name object-to-parent-transform units-per-meter
			     (3d-world-class '3d-world) &allow-other-keys)
  (or (get-3d-world-named name)
      (possibly-coerce-lvcs-to-3d-world name object-to-parent-transform units-per-meter)
      (apply 'make-instance 3d-world-class initargs)))

;;; FIXME:  This is really wrong when LVCS is passed.  It reparents the lvcs-to-geocentric-transform
;;;         to the new 3d-world, which is really ugly. 
(defun make-3d-world (&rest initargs
		      &key
		      lvcs
		      (name (and lvcs (name lvcs)))
		      lat long ellipsoid vertical-datum local-units-per-meter (origin-elev 0.0)
		      (lvcs-to-geocentric-transform (to-geocentric-transform lvcs))
		      property-list
		      &allow-other-keys)
  (let* ((existing-3d-world (get-3d-world-named name))
	 (3d-world
	  (if existing-3d-world
	      (progn
		(when lvcs-to-geocentric-transform
		  (set-parent-transform existing-3d-world lvcs-to-geocentric-transform))
		(when property-list
		  (setf (property-list existing-3d-world) property-list))
		existing-3d-world)

	      (apply 'get-or-make-3d-world
		     :name name
		     :object-to-parent-transform lvcs-to-geocentric-transform initargs))))
    #+never ;; (initialize-instance :after basic-coordinate-system) should handle this 
    (when lvcs-to-geocentric-transform
      (connect-transforms lvcs-to-geocentric-transform (inverse-transform lvcs-to-geocentric-transform)
			  3d-world (to-coordinate-system lvcs-to-geocentric-transform)))
    (unless (get-prop 3d-world :time-zone)
      (set-time-zone 3d-world))
    3d-world))

(defmethod lat-long-origin ((world 3d-world))
  (let ((lvcs-to-lat-long-transform (lvcs-to-lat-long-transform world)))
    (when lvcs-to-lat-long-transform
      (setq *foo4* lvcs-to-lat-long-transform)
      (transform-vector lvcs-to-lat-long-transform (cv 0.0 0.0 0.0)))))

(defmethod lvcs-to-geocentric-transform ((world 3d-world))
  (object-to-parent-transform world))

;; this shouldn't be needed -- call to-lat-long-transform instead
(defmethod lvcs-to-lat-long-transform ((world 3d-world))
  (to-lat-long-transform world))

(defmethod lvcs-to-utm-transform ((world 3d-world) &optional central-meridian)
  (transforms::to-utm-transform world central-meridian))



#|
(lat-long-origin (3d-world (top-view t)))
(lvcs-to-geocentric-transform (3d-world (top-view t)))
(inspect (3d-world (top-view t)))
(3d-world (top-view t))
|#

(defmethod set-time-zone ((3d-world 3d-world) &optional time-zone)
  (unless time-zone
    (bind-vector-elements (long)
	(lat-long-origin 3d-world)
      (setq time-zone (round long 15.0)) ; this is oversimplified
      ))
  (when time-zone
    (setf (get-prop 3d-world :time-zone) time-zone)))



;;; This method is poorly named. 
(defmethod add-object-to-world-namespace ((object-set object-set) object)
  (setf (find-object-named (world object-set) (name object))
	object))

;;; This method is poorly named.
(defmethod remove-object-from-world-namespace ((object-set object-set) object)
  (setf (find-object-named (world object-set) (name object))
	nil))
      
(defmethod generate-name ((world 2d-world) &optional ignore)
  (ignore ignore)
  (let* ((base-image (car (pyramid-list world)))
	 (2d-world-name (or (and base-image (name base-image))
			    (format nil "~d"
				    ;; appears to be a CLOS bug with class-allocated slots
				    ;; (incf (instance-count 2d-world))
				    (setf (get '2d-world 'unnamed-instance-count)
					  (1+ (or (get '2d-world 'unnamed-instance-count)
						  0)))
				    ))))
    (setf (name world)
	  (if (3d-world world)
	      (format nil "2d World ~a of ~a" 2d-world-name (name (3d-world world)))
	      (format nil "2d World ~a" 2d-world-name)))))


#|;;; old cme stuff to reconsider

(defmethod all-objects-fs ((world basic-world-mixin))
  (internal-get-prop world :all-objects-fs))

(defmethod deleted-objects-fs ((world basic-world-mixin))
  (internal-get-prop world :deleted-objects-fs))

(defvar *add-object-to-world-namespace-recursion* nil)

(defmethod add-object-to-world-namespace ((feature-set feature-set) object)
  (let* ((world (world feature-set)))
    (when (and world (not *add-object-to-world-namespace-recursion*))
      (let ((deleted-objects-fs (deleted-objects-fs world))
	    (all-objects-fs (all-objects-fs world))
	    (*add-object-to-world-namespace-recursion* t))
	;;(format t "add-object-to-world-namespace ~a ~A~%" object feature-set)
	(cond ((eq feature-set deleted-objects-fs)
	       (loop for fs in (object-feature-sets object)
		     unless (eq fs deleted-objects-fs)
		       do (remove-object object fs))
	       #+never
	       (when (and all-objects-fs (memq object (inferiors all-objects-fs)))
		 (remove-object object all-objects-fs)))
	      (t (when (and deleted-objects-fs (memq object (inferiors deleted-objects-fs)))
		   (remove-object object deleted-objects-fs))
		 (when (and all-objects-fs (neq feature-set all-objects-fs))
		   (add-object object all-objects-fs))))))))

(defparameter *deletion-from-deleted-objects-fs-adds-to-add-objects-fs* nil)

(defmethod remove-object-from-world-namespace ((feature-set feature-set) object)
  (let* ((world (world feature-set)))
    (when (and world (not *add-object-to-world-namespace-recursion*))
      (let ((deleted-objects-fs (deleted-objects-fs world))
	    (all-objects-fs (all-objects-fs world))
	    (*add-object-to-world-namespace-recursion* t))
	;;(format t "remove-object-from-world-namespace ~a ~A~%" object feature-set)
	(cond ((eq feature-set deleted-objects-fs)
	       ;; ******  should object become totally deleted here  ******
	       (when (and all-objects-fs *deletion-from-deleted-objects-fs-adds-to-add-objects-fs*)
		 (add-object object all-objects-fs)))
	      ((eq feature-set all-objects-fs)
	       (loop for fs in (feature-sets object)
		     unless (eq fs all-objects-fs)
		       do (remove-object object fs))
	       (when (and deleted-objects-fs (not (memq object (inferiors deleted-objects-fs))))
		 (add-object object deleted-objects-fs))
	       )
	      (nil (loop for fs in (feature-sets world)
		     unless (or (eq fs all-objects-fs)
				(eq fs deleted-objects-fs))
		       never (memq object (inferiors fs)))
	       (when (and all-objects-fs (memq object (inferiors all-objects-fs))
			  (neq feature-set all-objects-fs))
		 (remove-object object all-objects-fs))
	       (when (and deleted-objects-fs (not (memq object (inferiors deleted-objects-fs))))
		 (add-object object deleted-objects-fs))))))))
|#


;;; ***********************  SETUP-IMAGE-WORLDS   ***********************

(defparameter *setup-image-worlds-error-to-change-3d-world* t)

#+never
(defmethod set-3d-world ((2d-world 2d-world) new-3d-world)
  (with-slots (3d-world name) 2d-world
    (when (and 3d-world (neq 3d-world new-3d-world))
      (if *SETUP-IMAGE-WORLDS-ERROR-TO-CHANGE-3D-WORLD*
	  (error "Attempting to change the 3d world of a 2d world that already has a different 3d world")
	  (format t  ";;; Warning: Attempting to change the 3d world of a 2d world that already has a different 3d world~%"))
      )
    (unless (memq 2d-world (2d-worlds new-3d-world))
      (loop for 2d-world-b in (2d-worlds new-3d-world)
	    when (equal (name 2d-world-b) (name 2d-world))
	      do (error "Attempting to make a 2d-world ~a whose name conflicts with another 2d-world of ~a"
			2d-world new-3d-world))
      
      (if (null 3d-world)
	  (progn (setq 3d-world new-3d-world)
		 (unless name (generate-name 2d-world)))
	  (setq 3d-world new-3d-world))
      (push 2d-world (2d-worlds new-3d-world)))))

(defmethod set-3d-world ((2d-world 2d-world) new-3d-world)
  (with-slots (parent name) 2d-world
    (when (and parent (neq parent new-3d-world))
      (if *SETUP-IMAGE-WORLDS-ERROR-TO-CHANGE-3D-WORLD*
	  (error "Attempting to change the 3d world of a 2d world that already has a different 3d world")
	  (format t  ";;; Warning: Attempting to change the 3d world of a 2d world that already has a different 3d world~%"))
      )
    (unless (memq 2d-world (2d-worlds new-3d-world))
      (loop for 2d-world-b in (2d-worlds new-3d-world)
	    when (equal (name 2d-world-b) (name 2d-world))
	      do (error "Attempting to make a 2d-world ~a whose name conflicts with another 2d-world of ~a"
			2d-world new-3d-world))
      
      (if (null parent)
	  (progn (setq parent new-3d-world)
		 (unless name (generate-name 2d-world)))
	  (setq parent new-3d-world))
      (push 2d-world (2d-worlds new-3d-world)))))

(defmethod add-image-to-world ((world 2d-world) (image t))
  (unless (img::image-indirected-to image) ; let parent provide pointer to image
    (pushnew image (image-list world)))
  (setf (get-prop image :2d-world) world) ; should this be internal-get-prop ??
  (unless (get-prop world :sun-vector)
    (let ((image-sun-vector (get-prop image :sun-vector)))
      (when image-sun-vector (setf (get-prop world :sun-vector) image-sun-vector))))
  image)

(defmethod make-default-coordinate-system (world)
  (ignore world)
  nil)


(defmethod move-images-between-2d-worlds ((old-2d-world 2d-world) (new-2d-world 2d-world))
  (unless (zerop (or (search "2d World " (name old-2d-world)) -1))
	;;(equal (ic::substring (name old-2d-world) 0 9) "2d World ")
    (format t ";;; Warning: moving images from ~a to ~a~%" old-2d-world new-2d-world))
  (loop for image in (image-list old-2d-world)
	do (add-image-to-world new-2d-world image))
  (setf (image-list old-2d-world) nil)
  (loop for slot-name in '(parent object-to-parent-transform feature-sets name property-list)
	for value = (slot-value new-2d-world slot-name)
	unless value do (setf (slot-value new-2d-world slot-name) (slot-value old-2d-world slot-name)))
  )

(defmethod setup-image-worlds
	   ((image t) 
	    &key
	    (2d-world (2d-world image))
	    (image-to-2d-transform (get-prop image :image-to-2d-transform))
	    3d-world
	    3d-to-2d-projection)

  (unless (or 2d-world 3d-world) (setq 3d-world (3d-world image)))
  
  (let ((image-2d-world (2d-world image)))
    ;; Allow for NIL to be passed for 3d-to-2d-projection which means to use the projection
    ;; associated with the image.
    (unless 3d-to-2d-projection
      (setq 3d-to-2d-projection (or (and 2d-world (3d-to-2d-projection 2d-world))
				    (get-prop image :3d-to-2d-projection))))
    ;; *****************************************
    ;;  Handle the setup of the 2d world
  
    (if 2d-world
	(when (and image-2d-world (neq image-2d-world 2d-world))
	  (move-images-between-2d-worlds image-2d-world 2d-world))
      
	(setq 2d-world (make-instance '2d-world :3d-world 3d-world)))
    
    (add-image-to-world 2d-world image)
  
    (unless (coordinate-system 2d-world) (make-default-coordinate-system 2d-world))

    #+deprecated
    (unless (selected-feature-set 2d-world)
      ;; Every 2d-world must have a selected-feature-set
      (setf (selected-feature-set 2d-world)
	    (make-instance '2d-feature-set :world 2d-world)))
  
    (unless image-to-2d-transform
      ;; Make a default image-to-2d-transform with origin at bottom left corner. 
      (setq image-to-2d-transform (make-4x4-coordinate-transform (make-4x4-identity-matrix))))
    (setf (get-prop image :image-to-2d-transform) image-to-2d-transform)

    (when 3d-world
      (let ((existing-3d-world-of-2d-world
	     ;;(slot-value 2d-world '3d-world)
	     (slot-value 2d-world 'parent)
	      ))
	(print existing-3d-world-of-2d-world)
	(if (and existing-3d-world-of-2d-world
		 (neq 3d-world existing-3d-world-of-2d-world))
	    (if *setup-image-worlds-error-to-change-3d-world*
		(error "Attempting to change the 3d world of a 2d world that already has a different 3d world")
		(format t ";;; Warning: Attempting to change the 3d world of a 2d world that already has a different 3d world~%"))

	    ;; tie the 3d world and 2d world together
	    (set-3d-world 2d-world 3d-world)
	    ;; (set-parent 2d-world 3d-world)
	    )

	(unless (coordinate-system 3d-world) (make-default-coordinate-system 3d-world))))

    #+never
    (when 3d-to-2d-projection
      ;; make coordinate-systems agree
      (when 3d-world (setf (from-coordinate-system 3d-to-2d-projection) (coordinate-system 3d-world)))
      (setf (to-coordinate-system 3d-to-2d-projection) (coordinate-system 2d-world)))
    
    (setf (3d-to-2d-projection 2d-world) 3d-to-2d-projection)

    (rem-prop image :base-image)	; :base-image property flushed Wed May 13 1992
  
    #+never
    (when 3d-world
      (format t ";;; SETUP-IMAGE-WORLDS is setting 3d-world of image ~a to ~a.~%" image 3d-world)
      (setf (get-prop image :3d-to-2d-projection) 3d-to-2d-projection
	    (get-prop image :3d-world) 3d-world ; force image to be consistant with world
	    ))
    nil))

(defmethod pyramid-list ((2d-world 2d-world))
  (loop for image in (image-list 2d-world)
	when (eq image (top-of-image-pyramid image))
	  collect image))

(defmethod change-projection ((2d-world 2d-world) new-projection)
  (let ((old-projection (3d-to-2d-projection 2d-world))
	(3d-world (3d-world 2d-world)))
    (unless 3d-world (break))
    (transforms::set-transform-coordinate-systems new-projection 3d-world 2d-world)
   ;; (clear-x-buffers 2d-world)  ; ***** this needs a display-list-cache counterpart
    
    (when old-projection
      (push old-projection (get-prop 2d-world :projection-history )))
    (setf (3d-to-2d-projection 2d-world) new-projection)))

(defmethod revert-previous-projection ((world 2d-world))
  (let ((previous-projection (pop (get-prop world :projection-history ))))
    (when previous-projection
      ;;(clear-x-buffers world)
      (setf (3d-to-2d-projection world) previous-projection))))

(defun revert-to-simple-projection (&optional (2d-world (2d-world (top-view))))
  (loop with proj = (3d-to-2d-projection 2d-world)
	while (typep proj 'composite-coordinate-projection)
	do (setq proj (composite-coordinate-projection-simple-projection proj))
	finally (when proj (setf (3d-to-2d-projection 2d-world) proj) )
		(return proj)))

;;; This code is used only in cvv-object-menus.lisp

;;; unfinished and replaced in cvv-object-menus.lisp
(defmethod cvv-menu-item-list (coordinate-system)
  nil)

;;;#+unused
;;;(defun get-coordinate-system-named (name)
;;;  (loop for 3d-world in *3d-worlds*
;;;        for coordinate-system = (coordinate-system 3d-world)
;;;        when (equal (name coordinate-system) name)
;;;          return coordinate-system))

(defmethod geographic-transform-alist ((3d-world 3d-world))
  (get-prop 3d-world :geographic-transform-alist))

(defmethod (setf geographic-transform-alist) (alist (3d-world 3d-world))
  (setf (get-prop 3d-world :geographic-transform-alist) alist))

(defmethod register-geographic-transform
	   ((3d-world 3d-world)
	    lvcs-to-xxx-coordinate-transform
	    &optional (to-coordinate-system
		       (to-coordinate-system lvcs-to-xxx-coordinate-transform)))

  (let* ((item-list (cvv-menu-item-list to-coordinate-system))
	 (transform-name (car (car item-list))))
    #+never
    (format t "register-geographic-transform ~a~%"
	    (list transform-name lvcs-to-xxx-coordinate-transform item-list))
    (setf (geographic-transform-alist 3d-world)
	  (if (assoc transform-name (geographic-transform-alist 3d-world) :test 'equal)
	      (putassoc (geographic-transform-alist 3d-world)
			transform-name
			(list lvcs-to-xxx-coordinate-transform item-list)
			:test 'equal)
	      (nconc (geographic-transform-alist 3d-world)
		     (list (list transform-name lvcs-to-xxx-coordinate-transform item-list))
		     )))))

;;; Called from cvv-object-menus.lisp
(defmethod find-geographic-transform ((3d-world 3d-world) transform-name )
  (or (assoc transform-name (geographic-transform-alist 3d-world) :test 'equal)
      (first (geographic-transform-alist 3d-world))))

(defmethod find-geographic-transform-of-class ((3d-world 3d-world) to-coordinate-system-class )
  (loop for (name transform) in (geographic-transform-alist 3d-world)
	for to-coordinate-system = (to-coordinate-system transform)
	do (ignore name)
	when (typep to-coordinate-system to-coordinate-system-class)
	  return transform))
 

#+object-name-hash-tables
(eval-when (load eval compile)
  (setf *features* (remove :object-name-hash-tables *features*))
)
 




(defmethod get-world-named (name)
  (if (stringp name)
      (get-3d-world-named name)
      (let ((3d-world (get-3d-world-named (car name))))
	(if (cadr name)
	    (get-2d-world-named (cadr name))
	    3d-world))))
