(in-package :smc)

#|
(maybe-compile-file-load "~/cp/lisp/cme-compat/radius-classes.lisp")
|#

(defvar *radius-context*)

;;; ***********************************  RADIUS-MIXIN  ***********************************

;;#+old
(progn 

#+old
(defclass radius-mixin ()
    ((attributes    :initarg :attributes    :accessor attributes :initform nil)
     (metadata      :initarg :metadata      :accessor metadata   :initform nil))
  (:default-initargs :attributes nil :metadata nil))

;;; This class defines slots in violation to proposed defstruct extension.
;;; The definition below, using soft-slots fixes the problem.
(defclass radius-mixin (basic-object)
    ((attributes    :initarg :attributes    :accessor attributes :initform nil)
     (metadata      :initarg :metadata      :accessor metadata   :initform nil))
  (:default-initargs :attributes nil :metadata nil))

); end progn

#+new ; unfinished
(progn
  
  (defclass radius-mixin () ())

  (define-soft-slot radius-mixin :attributes attributes)
  (define-soft-slot radius-mixin :metadata metadata)

) ;end progn

(defparameter *radius-mixin-obsolete-attributes* '(:name))

(define-fasd-form-init-plist radius-mixin
    (let ((attributes (attributes self))
	  (metadata (metadata self)))
      `(,@(and attributes
	       `(:attributes
		 (list .,(loop for (prop value) on  attributes by #'cddr
			       unless (memq prop *radius-mixin-obsolete-attributes*)
			       collect (optimize-eval-form `',prop)
			       and collect (optimize-eval-form
					    (if (memq prop '(:model-creation-date :model-modification-date))
						(if (listp value)
						    `',(mapcar
							#'(lambda (value) (print (if (numberp value)
										     (print-universal-time value nil)
										     value)))
							value)
						    (print-universal-time value nil))
						(if (fasd-form-p value)
						    (fasd-form value)
						    `',value)))))))
	,@(and metadata `( :metadata (list .,metadata)))
	)))

(defmethod initialize-radius-attributes ((obj radius-mixin) feature-type)
  (let ((time (get-universal-time)))
    ;;; changed by Heller Oct 24, 95
    #+never (set-attribute obj :NAME (name obj))
    (set-attribute obj :FEATURE-TYPE feature-type)
    (set-attribute obj :MODEL-CREATOR (lcl::environment-variable "USER"))
    (set-attribute obj :MODEL-CREATION-DATE time)
    ))

;;; Inserted this so the the radius-attributes of an object are
;;; correctly set on cloned object.  Wed Mar 22 1995 heller
;;;(defmethod initialize-instance :after
;;;  ((obj radius-mixin) &key (feature-type (get-attribute obj :feature-type)) &allow-other-keys)
;;;  ;; I'm not sure if this is the right thing to do -- ajh
;;;  ;; (setf (attribute-list obj) nil)
;;;  (initialize-radius-attributes obj feature-type))

(defparameter *set-object-graphics-style-attributes* (member :cme-5  *features*))

(declaim (special *radius-class-graphics-attributes-alist*))

(defparameter *radius-context-error-menu*
  '(("Option 1: Set Context from current view   " :eval :set)
    ("Option 2: Pop-up Context Menu             " :eval :menu)
    ("Option 3: Abort                           " :eval :abort)))

(defmethod initialize-instance :after
  ((obj radius-mixin) &key
   (feature-type (get-attribute obj :feature-type))
   initialize-radius-object
   &allow-other-keys)
  ;; I'm not sure if this is the right thing to do -- ajh
  ;; (setf (attribute-list obj) nil)
  ;;(break)
  (unless initialize-radius-object	; loading from a file
    (flet ((convert-attribute-to-universal-time (attr)
	     (let ((time (get-attribute obj attr)))
	       (when (stringp time)
		 (set-attribute obj attr (parse-printed-universal-time time))))))
      (convert-attribute-to-universal-time :model-creation-date)
      (convert-attribute-to-universal-time :model-modification-date)
      (rem-prop obj :snippet)	;flush crap from old site model files
      ))
#|
  (when nil ;initialize-radius-object
;;; THIS CODE HAS NOT BEEN CONVERTED TO RUN IN FREEDIUS
    ;; check that the context is set correctly
    (let* ((object-creation-view 
	    (or
	     ;; 3d-object
	     (and (boundp '*CREATE-AND-ADD-3D-OBJECT-VIEW*) *CREATE-AND-ADD-3D-OBJECT-VIEW*)
	     ;; 2d-object
	     (and (boundp '*object-creation-view*) *object-creation-view*)
	     ;; there are no RADIUS window objects
	     (progn
	       (format *error-output* "~&;;; A serious error has occured.  Please file a DR.")
	       (break)
	       nil)))
	   (3d-world-of-object-creation-view
	    (and object-creation-view
		 (3d-world object-creation-view))))

      #+debug
      (format *error-output*
	      "~&;;; view = ~s~&;;; 3d-world = ~s"
	      3d-world-of-object-creation-view
	      (get-3d-world-named (site-name *radius-context*)))
      (unless (and (boundp '*radius-context*) *radius-context*
		   (eq 3d-world-of-object-creation-view
		       (get-3d-world-named (site-name *radius-context*))))

	(let ((choice (tk:menu-choose (tk:make-menu *RADIUS-CONTEXT-ERROR-MENU*
						      :label "RADIUS Context is set incorrectly")
				       t)))
	  (cond
	    ((eq choice :abort) (abort))
	    ((eq choice :menu)
	     (setf (selected-pane) (view-window object-creation-view))
	     (parameter-menu (hub-context object-creation-view)))
	    ((eq choice :set)
	     (setf (selected-pane) (view-window object-creation-view))
	     (hub-context object-creation-view)))))

      (initialize-radius-object obj)
      (initialize-radius-attributes obj feature-type)
    ;;; This next might be kinda slow, but does away in CME-6
      (when *set-object-graphics-style-attributes* 
	(apply 'set-graphics-style-attributes
	       obj
	       (cdr (assoc (type-of obj) *radius-class-graphics-attributes-alist*)))
	)))
|#
  )

(defmethod initialize-radius-object ((self radius-mixin) &key))
	   
(defmethod orient-radius-object ((self radius-mixin) &optional (axis-spec :z-rot) ( degrees 90.0))
  (rotate-to self `(,axis-spec ,(radians (- degrees (site-orientation *radius-context*))))))

(defmethod meters-to-local-units ((object basic-object) meters)
  (* meters (local-units-per-meter object)))
  
(defmethod size-and-orient-radius-object ((self radius-mixin) x-meters y-meters z-meters
					  &optional (axis-spec :z-rot) ( degrees 90.0))
  (if (or (not (boundp '*radius-context*)) (not *radius-context*))
      (progn
	(menu-choose `((,(format nil "RADIUS context not set.~&Please select a pane and push [Context] button.")
			    :eval t))
		     :popup-position :mouse)
	;(xw::do-events)
	(abort))
      (let ((upm (local-units-per-meter self)))
	(rotate-to self `(,axis-spec ,(radians (- degrees (site-orientation *radius-context*)))))
	(change-size self (* x-meters upm) (* y-meters upm) (* z-meters upm)))))

;;; *************************  RADIUS ATTRIBUTES  *************************

(defgeneric GET-ATTRIBUTE (object attribute))

(defgeneric SET-ATTRIBUTE (object attribute value))

(defgeneric PUSH-ATTRIBUTE (object attribute value))

(defgeneric POP-ATTRIBUTE (object attribute))

(defgeneric REMOVE-ATTRIBUTE (object attribute))

(defgeneric ATTRIBUTE-LIST (object))

;--------------------------------------------------------------------------------

(defmethod GET-ATTRIBUTE ((self radius-mixin) attribute)
  (getf (attributes self) attribute))

(defmethod SET-ATTRIBUTE ((self radius-mixin) attribute value)
  (setf (getf (attributes self) attribute) value))

(defmethod PUSH-ATTRIBUTE ((self radius-mixin) attribute value)
  (let ((attributes (attributes self)))
    (setf (getf attributes attribute)
	  (cons value (getf attributes attribute)))))

(defmethod POP-ATTRIBUTE ((self radius-mixin) attribute)
  (let ((attributes (attributes self)))
    (setf (getf attributes attribute)
	  (cdr (getf attributes attribute)))))

(defmethod REMOVE-ATTRIBUTE ((self radius-mixin) attribute)
  (remf (attributes self) attribute))

(defmethod ATTRIBUTE-LIST ((self radius-mixin))
  (attributes self))

(defmethod (setf ATTRIBUTE-LIST) (attribute-list (self radius-mixin))
  (setf (attributes self) attribute-list))

;;;--------------------------------------------------------------------------------

;;;  3d-worlds are used to represent a site
;;; they don't have the radius-mixin, so their attributes get stuck on the property-list

(defmethod GET-ATTRIBUTE ((self 3d-world) attribute)
  (getf (attribute-list self) attribute))

(defmethod SET-ATTRIBUTE ((self 3d-world) attribute value)
  (setf (getf  (attribute-list self) attribute) value))

(defmethod PUSH-ATTRIBUTE ((self 3d-world) attribute value)
  (setf (getf (attribute-list self) attribute)
	(cons value (getf  (attribute-list self) attribute))))

(defmethod POP-ATTRIBUTE ((self 3d-world) attribute)
  (setf (getf (attribute-list self) attribute)
	(cdr (getf  (attribute-list self) attribute))))

(defmethod REMOVE-ATTRIBUTE ((self 3d-world) attribute)
  (remf (attribute-list self) attribute))

(defmethod ATTRIBUTE-LIST ((self 3d-world))
  (getf (property-list self) :attributes))

(defmethod (setf ATTRIBUTE-LIST) (attribute-list (self 3d-world))
  (put-prop self attribute-list :attributes))

;;;   New improved version
(defparameter *query-result* nil)

#+never ; not converted
(defun retrieve (labels fss)
  "Given a list of labels and a list of feature-sets, this fn returns the set of objects (contained in any of the feature-sets) whose feature-type matches one of the labels."
  (setq labels (radius-subclasses labels))
  (if (not (listp fss)) (setq fss (list fss)))
  (setq *query-result* nil)
  (map-over-all-objects-in-fss
   fss
   #'(lambda(obj)
       (let ((label (and (typep obj 'radius-mixin)
			 (get-attribute obj :feature-type))))
	 (when label
	   (if (memq label labels) (push obj *query-result*))))))
  *query-result*)

#||   Can we support a query like this?
      or implement an in-memory SQL?
(query baseline (or (and (:feature-type :building) (> :height 4.0))
		    (and (:feature-type :road) (> :width 3.0))))
||#

(defun slot-or-attribute-value (x att)
  (or (and (slot-exists-p x att)
	   (slot-boundp x att)
	   (slot-value x att))
      (get-attribute x att)))

;;; **********************************   RADIUS-CLASSES  ********************************

(defvar *radius-subclass-table* (make-hash-table))

;;#-cmu
(defmacro define-radius-class (class (parent-class &optional slots options)
				     short-name-string 
				     &body initializations)
  `(progn
    (defstruct-class ,class (radius-mixin ,parent-class) ,slots . ,options)
    
    (setf (gethash ',parent-class *radius-subclass-table*)
          (cons ',class (gethash ',parent-class *radius-subclass-table*)))

    (defmethod obj::short-name-string ((self  ,class ))
      ,short-name-string)

    ,(when initializations
	   `(defmethod initialize-radius-object ((self ,class) &key (reset-width? t))
	     (ignore reset-width?)
	     . ,initializations)) 
    ))

;;; We are trying to avoid the use of MIXINS here.
;;;; Thu Oct 14 2004 -- no good reason since there is no likelyhood of totally eliminating mixins.
#+never ; #+cmu
(defmacro define-radius-class (class (parent-class &optional slots options)
				     short-name-string 
				     &body initializations)
  `(progn
    (defstruct-class ,class (,parent-class) ,slots . ,options)
    
    (defmethod obj::short-name-string ((self  ,class ))
      ,short-name-string)

    ,(when initializations
	   `(defmethod initialize-radius-object ((self ,class) &key (reset-width? t))
	     (ignore reset-width?)
	     . ,initializations)) 
    ))

;;;================================================================================

#+undef
(define-radius-class radius-tool (view-hacking-object) "Tool")

#+undef
(define-radius-class radius-window-label (window-text-object) "Window Label"
  (set-text self "Window Annotation"))

#+undef
(define-radius-class radius-north-arrow (north-arrow-object) "North Arrow")

#+undef
(defun object-creation-view ()
;;  #-cme-6 (top-view) ;;; UGLY !!!!!!!!!!!!!!
  *object-creation-view*)

#+undef
(define-radius-class radius-map-scale
    (3d-ruler-object ((world-length :initarg :world-length :accessor world-length)))
     "Map Scale"
  (with-slots (open-for-vertex-modification world-length ) self
    (setq open-for-vertex-modification nil)
    (setq world-length (meters-to-local-units self 100.0))
    (set-map-scale-position self 100.0 300.0 ; these window coordinates are totally bogus
			    ;;(view-window (object-creation-view))
			    (object-creation-view))))

#+never 
(progn
;;; Moved here so that strat-cme-transforms.lisp could be eliminated.
(defun transform-window-to-world (view wi wj &optional z )
  "Returns (values wx wy wz)
   z can be an elevation, a terrain-model, a view, or a 3d-world.
   If z is not specified, the terrain model will be used."
  (unless z (setq z (3d-world view)))
  (coordinate-vector-elements
   (intersect-camera-ray-with-z-plane
    (3d-to-2d-projection (2d-world view))
    (inverse-transform-vector (2d-to-window-transform view)
				   (cv wi wj))
    z)))


(defmethod set-map-scale-position ((self radius-map-scale) wi0 wj0 view)
  (with-slot-values (vertices world-length) self
    (mv-bind (wx0 wy0 wz0)
	(transform-window-to-world view wi0 wj0)
      (mv-bind (wx wy)	;just to get direction of motion
	  (transform-window-to-world view (+ wi0 100.0) wj0 wz0 )
	(let* ((object-to-world-transform (object-to-world-transform self))
	       (dx (- wx wx0))
	       (dy (- wy wy0))
	       (sqrt (/ (euclidean-length dx dy)))
	       (wx1 (+ wx0 (* world-length dx sqrt)))
	       (wy1 (+ wy0 (* world-length dy sqrt))))
	  (set-vertex-world-xyz (aref vertices 0) object-to-world-transform wx0 wy0 wz0)
	  (set-vertex-world-xyz (aref vertices 1) object-to-world-transform wx1 wy1 wz0))))))


(define-radius-class radius-reference-point (3d-crosshair-object) "Reference Point")

(define-radius-class radius-ruler (3d-ruler-object) "Ruler"
		     (setf (vertices-visible-p self) t))

(define-radius-class radius-2d-label ( 2d-text-object) "2D Label"
  (set-text self "2D Annotation"))

(define-radius-class radius-2d-curve (2d-curve) "2D Curve")

(define-radius-class radius-2d-closed-curve (closed-2d-curve) "2D Closed Curve")

(define-radius-class radius-2d-ribbon-curve (2d-ribbon-curve) "2D Ribbon Curve")

) ; end progn

;;;================================================================================

(define-radius-class radius-3d-label (3d-text-object) "3D Label"
  (setf (name self) "3D Annotation"))

(define-radius-class radius-walkway (3d-ribbon-curve) "Walkway"
  (when reset-width?
    ;; wierd width -- one lane
    (set-width-of-ribbon self (meters-to-local-units self 2.0) )))


(define-radius-class radius-loc (3d-ribbon-curve) "LOC"
  (when reset-width?
    (set-width-of-ribbon self (meters-to-local-units self 10.0))))

(define-radius-class radius-road (3d-ribbon-curve) "Road"
  (when reset-width?
    ;; wierd width -- one lane
    (set-width-of-ribbon self (meters-to-local-units self 4.0) )))

(define-radius-class radius-street (3d-ribbon-curve) "Street"
  (when reset-width?
    (set-width-of-ribbon self (meters-to-local-units self 7.0) )))

(define-radius-class radius-highway (3d-ribbon-curve) "Highway"
  (when reset-width?
    (set-width-of-ribbon self (meters-to-local-units self 14.0))))

(define-radius-class radius-railroad (3d-ribbon-curve) "Railroad"
  (when reset-width?
    (set-width-of-ribbon self (meters-to-local-units self 1.5 ) )))

(define-radius-class radius-powerline (3d-curve) "Powerline"
  (setf (vertices-visible-p self) t))

(define-radius-class radius-pipeline (3d-curve) "Pipeline")

(define-radius-class radius-runway (3d-ribbon-curve) "Runway"
  (set-width-of-ribbon self (meters-to-local-units self 70.0 )))

;;; This should not be here  - prevents radius-classes from being loaded by itself

;;;================================================================================

(define-radius-class radius-building (cube-object) "Building"
  (size-and-orient-radius-object self 20.0 40.0 10.0))

(define-radius-class radius-complex-building (extruded-object) "Building"
  (orient-radius-object self))

(define-radius-class radius-barracks (house-object) "Barracks"
  (size-and-orient-radius-object self 10.0 25.0 8.0))

(define-radius-class radius-warehouse (house-object) "Warehouse"
  (size-and-orient-radius-object self 50.0 100.0 10.0))

(define-radius-class radius-quanset (half-cylinder) "Quanset"
  (size-and-orient-radius-object self 7.0 13.0 5.0))

(define-radius-class radius-house (house-object) "House"
  (size-and-orient-radius-object self 10.0 20.0 5.0))

(define-radius-class radius-shed (house-object) "Shed"
  (setf (roof-type self) :shed)
  (size-and-orient-radius-object self 10.0 20.0 5.0))

(define-radius-class radius-hangar (cube-object) "Hangar"
  (size-and-orient-radius-object self 50.0 100.0 20.0))
#+undef
(define-radius-class radius-stadium (superquadric) "Stadium"
  (size-and-orient-radius-object self 100.0 200.0 40.0))

(define-radius-class radius-tower (cylinder) "Tower"
  (size-and-orient-radius-object self 1.0 1.0 7.0))

;;;===============================================================================

(define-radius-class radius-structure (cube-object) "Structure"
  (size-and-orient-radius-object self 20.0 40.0 10.0))

#+oldcme
(define-radius-class radius-fence (3d-curtain) "Fence")

#-oldcme
(define-radius-class radius-fence (3d-curve) "Fence")

#+oldcme
(define-radius-class radius-wall (3d-curtain) "Wall")

#-oldcme
(define-radius-class radius-wall (3d-curve) "Wall")

#+oldcme
(define-radius-class radius-bunker (superquadric) "Bunker"
  (size-and-orient-radius-object self 10.0 40.0 2.0))

#-oldcme
(define-radius-class radius-bunker (cube-object) "Bunker"
  (size-and-orient-radius-object self 10.0 40.0 2.0))

(define-radius-class radius-pier (cube-object) "Pier"
  (size-and-orient-radius-object self 30.0 300.0 3.0))

(define-radius-class radius-bridge (cube-object) "Bridge"
  (size-and-orient-radius-object self 13.0 100.0 3.0))

#+undef
(define-radius-class radius-gate (3d-text-object) "Gate"
  (set-text self "Gate"))

(define-radius-class radius-tank (cylinder) "Tank")

(define-radius-class radius-POL-tank (cylinder) "POL Tank")

(define-radius-class radius-water-tank (cylinder) "Water Tank"
  (size-and-orient-radius-object self 10.0 10.0 10.0))

(define-radius-class radius-pole (cylinder) "Pole"
  (size-and-orient-radius-object self 0.5 0.5 3.0))

;;;================================================================================

(define-radius-class radius-vegetation (3d-closed-curve) "Vegetation"
  (setf (vertices-visible-p self) t))

(define-radius-class radius-veg-boundary (3d-curve) "Veg Boundary")

(define-radius-class radius-forest (3d-closed-curve) "Forest")

(define-radius-class radius-field (3d-closed-curve) "Field")

;;;================================================================================

(define-radius-class radius-water (3d-closed-curve) "Water")

(define-radius-class radius-shoreline ( 3d-curve) "Shoreline")

(define-radius-class radius-stream (3d-ribbon-curve) "Stream"
  (when reset-width?
    (set-width-of-ribbon self (meters-to-local-units self 4.0 ))))

(define-radius-class radius-river (3d-ribbon-curve) "River"
  (when reset-width?
    (set-width-of-ribbon self (meters-to-local-units self 12.0 ) )))

(define-radius-class radius-lake (3d-closed-curve) "Lake")

;;;================================================================================

(define-radius-class radius-functional-area (3d-closed-curve) "Functional Area")

(define-radius-class radius-site-perimeter (3d-closed-curve) "Site Perimeter")

(define-radius-class radius-rail-transfer-area (3d-closed-curve) "Rail Xfer")

(define-radius-class radius-motor-pool (3d-closed-curve) "Motor Pool")

(define-radius-class radius-vehicle-park (3d-closed-curve) "Vehicle Park")

(define-radius-class radius-parade-field (3d-closed-curve) "Parade Field")

(define-radius-class radius-loading-dock (3d-closed-curve) "Loading Dock")

#+undef
(define-radius-class radius-dtm-mesh (quad-grid-dtm-object) "DTM Mesh")

(define-radius-class radius-3d-composite (3d-composite-object) "Composite")

;;;================================================================================

;;; from /home/woodruff3/quam/testbed/radius/code/CME-5d/VF/testbed/radius-lof.lisp

#+undef
(define-radius-class complicated-structure (list-of-faces-object) "Complicated Structure")

;;; phoney definition just to allow SRI to read the VF data sets.

(defstruct-class collateral-object ()
    ((object-tag :initarg :object-tag)
     (collateral-text-list :initarg :collateral-text-list)
     (world :initarg :world)))

;;; phoney definition just to allow SRI to read the VF data sets.

(defstruct-class collateral-text ()
    ())

(defmethod initialize-instance :after ((self collateral-text) &key &allow-other-keys))


;;; ********************  RADIUS TYPE HIERARCHY   ********************

;;; radius-subclasses is called from hub-prolog.lisp 
(defun radius-subclasses (labels)
  (descendants labels))

;;; radius-superclasses is called from rcde-class-of-feature-type  defiend in radius-menus.lisp
;;; which is called from hub-prolog.lisp
(defun radius-superclasses (labels)
  (ancestors labels))

(defun parents (x)
  (get x :parents))

(defun xxx-children (x)
  (get x :children))

(defun ancestors (x)
  (if (atom x)
      (let ((parents (parents x)))
	(cond ((null parents) (list x))
	      (t (adjoin x (remove-duplicates (mapcan #'ancestors parents))))
	      ))
      (remove-duplicates (mapcan #'ancestors x))))

;;; new, faster version
(defun ancestors2 (x)
  (let (l)
    (labels ((ancestors-internal (x)
	     (if (atom x)
		 (progn (pushnew x l)
			(loop for y in (parents x)
			      do (ancestors-internal y)))
		 (loop for y in x do (ancestors-internal y)))))
      (ancestors-internal x)
      (nreverse l))))

(defun descendants (x)
  (if (atom x)
      (let ((children (xxx-children x)))
	(cond ((null children) (list x))
	      (t (adjoin x (remove-duplicates (mapcan #'descendants children))))
	      ))
      (remove-duplicates (mapcan #'descendants x))
      ))

;;; new, faster version
(defun descendants2 (x)
  (let (l)
    (labels ((descendants-internal (x)
	       (if (atom x)
		   (progn (pushnew x l)
			  (loop for y in (xxx-children x)
				do (descendants-internal y)))
		   (loop for y in x do (descendants-internal y)))))
      (descendants-internal x)
      (nreverse l))))

#|
(let ((type ':MOLE)) (equal (ancestors type) (ancestors2 type)))
(let ((type ':area)) (equal (descendants type) (descendants2 type)))
|#

;;; this should be called make-subtype
(defun subtype (parent &rest children)
  (loop for child in children
	do (pushnew child (get parent :children))
	   (pushnew parent (get child :parents))))


;;;(defun print-hierarchy (&optional (n :all) (indentation 0))
;;;  (format t "~V T~S~%" indentation n)
;;;  (loop for x in (xxx-children n)
;;;        do (print-hierarchy x (+ 3 indentation))))

(defun print-hierarchy (&optional (n :all) (indentation 0))
  (format t "~VT~S~%" indentation n)
  (loop for x in (xxx-children n)
	do (print-hierarchy x (+ 3 indentation))))

(subtype :ALL :AREA)
(subtype :AREA :SITE-PERIMETER)
(subtype :AREA :FUNCTIONAL-AREA)
(subtype :FUNCTIONAL-AREA :AIRFIELD)
(subtype :FUNCTIONAL-AREA :APRON)
(subtype :FUNCTIONAL-AREA :ATHLETIC-FIELD)
(subtype :FUNCTIONAL-AREA :BIVOUAC-AREA)
(subtype :FUNCTIONAL-AREA :GARRISON-AREA)
(subtype :FUNCTIONAL-AREA :HARBOR)
(subtype :FUNCTIONAL-AREA :HOUSING-AREA)
(subtype :FUNCTIONAL-AREA :MAINTENANCE-AREA)
(subtype :FUNCTIONAL-AREA :MOLE)
(subtype :FUNCTIONAL-AREA :MOORING-AREA)
(subtype :FUNCTIONAL-AREA :MOTOR-POOL)
(subtype :FUNCTIONAL-AREA :PARKING-AREA)
(subtype :FUNCTIONAL-AREA :QUAY)
(subtype :FUNCTIONAL-AREA :RAILYARD)
(subtype :FUNCTIONAL-AREA :RUNWAY)
(subtype :FUNCTIONAL-AREA :STORAGE-AREA)
(subtype :FUNCTIONAL-AREA :TAXIWAY)
(subtype :FUNCTIONAL-AREA :TRAINING-AREA)
(subtype :AREA :NATURAL-FEATURE)
(subtype :NATURAL-FEATURE :WATER-BODY)
(subtype :WATER-BODY :LAKE)
(subtype :WATER-BODY :POND)
(subtype :WATER-BODY :RESERVOIR)
(subtype :NATURAL-FEATURE :ISLAND)
(subtype :NATURAL-FEATURE :VEGETATION)
(subtype :VEGETATION :FOREST)
(subtype :VEGETATION :FIELD)
(subtype :VEGETATION :MARSH)
(subtype :VEGETATION :FARM)
(subtype :ALL :STRUCTURE)
(subtype :STRUCTURE :BUILDING)
(subtype :BUILDING :ADMINISTRATION-BUILDING)
(subtype :BUILDING :AIRCRAFT-SHELTER)
(subtype :BUILDING :ASSEMBLY-BUILDING)
(subtype :BUILDING :BARRACKS)
(subtype :BUILDING :FACTORY)
(subtype :BUILDING :FIRE-STATION)
(subtype :BUILDING :GARAGE)
(subtype :BUILDING :GUARDHOUSE)
(subtype :BUILDING :HANGAR)
(subtype :BUILDING :HEADQUARTERS-BUILDING)
(subtype :BUILDING :HOSPITAL)
(subtype :BUILDING :HOUSE)
(subtype :BUILDING :MAINTENANCE-BUILDING)
(subtype :BUILDING :MESS-HALL)
(subtype :BUILDING :NUCLEAR-REACTOR)
(subtype :BUILDING :OFFICE-BUILDING)
(subtype :BUILDING :POWER-PLANT)
(subtype :BUILDING :QUONSET-HUT)
(subtype :BUILDING :SHED)
(subtype :BUILDING :STORAGE-BUILDING)
(subtype :BUILDING :SUPPORT-BUILDING)
(subtype :BUILDING :WAREHOUSE)
(subtype :STRUCTURE :TOWER)
(subtype :TOWER :ANTENNA-TOWER)
(subtype :TOWER :CONTROL-TOWER)
(subtype :TOWER :SMOKE-STACK)
(subtype :STRUCTURE :UTILITY-POLE)
(subtype :UTILITY-POLE :TELEPHONE-POLE)
(subtype :UTILITY-POLE :LAMP-POLE)
(subtype :STRUCTURE :STORAGE-TANK)
(subtype :STORAGE-TANK :WATER-TANK)
(subtype :STORAGE-TANK :POL-TANK)
(subtype :STRUCTURE :FENCE)
(subtype :STRUCTURE :WALL)
(subtype :STRUCTURE :BUNKER)
(subtype :STRUCTURE :REVETMENT)
(subtype :STRUCTURE :BRIDGE)
(subtype :STRUCTURE :PIER)
(subtype :STRUCTURE :DAM)
(subtype :STRUCTURE :STRUCTURE-DETAIL)
(subtype :STRUCTURE-DETAIL :DOOR)
(subtype :STRUCTURE-DETAIL :WINDOW)
(subtype :STRUCTURE-DETAIL :GATE)
(subtype :STRUCTURE-DETAIL :VENT)
(subtype :STRUCTURE-DETAIL :EXHAUST-DUCT)
(subtype :STRUCTURE-DETAIL :ANTENNA)
(subtype :STRUCTURE-DETAIL :CRANE)
(subtype :ALL :TRANS/COMM)
(subtype :TRANS/COMM :ROAD)
(subtype :ROAD :ONE-LANE-ROAD)
(subtype :ROAD :TWO-LANE-ROAD)
(subtype :ROAD :FOUR-LANE-ROAD)
(subtype :ROAD :CITY-STREET)
(subtype :ROAD :HIGHWAY)
(subtype :TRANS/COMM :BRIDGE)
(subtype :TRANS/COMM :TUNNEL)
(subtype :TRANS/COMM :RAILROAD)
(subtype :RAILROAD :RAIL-MAINLINE)
(subtype :RAILROAD :RAIL-SPUR)
(subtype :RAILROAD :RAIL-TRANSFER-AREA)
(subtype :TRANS/COMM :PIPELINE)
(subtype :TRANS/COMM :TRANSMISSION-LINE)
(subtype :TRANSMISSION-LINE :COMM-LINE)
(subtype :TRANSMISSION-LINE :POWER-LINE)
(subtype :TRANS/COMM :RUNWAY)
(subtype :TRANS/COMM :TAXIWAY)
(subtype :TRANS/COMM :APRON)
(subtype :TRANS/COMM :WATERWAY)
(subtype :WATERWAY :RIVER)
(subtype :WATERWAY :CANAL)
(subtype :WATERWAY :SHIPPING-LANE)
(subtype :ALL :NATURAL-FEATURE)
(subtype :NATURAL-FEATURE :WATER-BODY)
(subtype :WATER-BODY :LAKE)
(subtype :WATER-BODY :POND)
(subtype :WATER-BODY :RESERVOIR)
(subtype :WATER-BODY :STREAM)
(subtype :WATER-BODY :RIVER)
(subtype :WATER-BODY :CANAL)
(subtype :WATER-BODY :IRRIGATION-CHANNEL)
(subtype :NATURAL-FEATURE :SHORELINE)
(subtype :NATURAL-FEATURE :ISLAND)
(subtype :NATURAL-FEATURE :VEGETATION)
(subtype :VEGETATION :FOREST)
(subtype :VEGETATION :FIELD)
(subtype :VEGETATION :MARSH)
(subtype :VEGETATION :FARM)
(subtype :NATURAL-FEATURE :VEGETATION-BOUNDARY)
(subtype :NATURAL-FEATURE :DITCH)
(subtype :NATURAL-FEATURE :BERM)
(subtype :NATURAL-FEATURE :DAM)
(subtype :ALL :POINT)
(subtype :POINT :REFERENCE-POINT)
(subtype :POINT :REGISTRATION-POINT)
(subtype :POINT :SURVEY-POINT)
(subtype :ALL :IU-SUPPORT-OBJECT)
(subtype :IU-SUPPORT-OBJECT :MATERIAL-IMAGE)
(subtype :IU-SUPPORT-OBJECT :RADIOMETRIC-IMAGE)
(subtype :IU-SUPPORT-OBJECT :ILLUMINATION-STATE)
(subtype :IU-SUPPORT-OBJECT :WEATHER-STATE)
(subtype :IU-SUPPORT-OBJECT :MATERIAL)
(subtype :IU-SUPPORT-OBJECT :3D-MATERIAL-PATCH)

;;; ******************************  RADIUS-CLASS-GRAPHICS-STYLE-ALIST  ******************************

(defparameter *100%-stipple '((1 1 1 1) (1 1 1 1) (1 1 1 1) (1 1 1 1)))

(defparameter *50%-stipple '((0 1 0 1) (1 0 1 0) (0 1 0 1) (1 0 1 0)))

(defparameter *0%-stipple '((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0)))

(defparameter *light-stipple* '((0 1 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0)))

(defparameter *default-stipple* *50%-stipple)

;;(defparameter *enable-functional-area-stipples* nil)
(defparameter *enable-functional-area-stipples* t)

;;(setq *enable-functional-area-stipples* t)

;;; Huh?  This will never do anything...
(defun fa-stipple (&optional (pattern *0%-stipple))
  (ignore pattern)
  (when *enable-functional-area-stipples* pattern))

;;(defparameter *default-radius-class-graphics-style-line-width* 2)
(defparameter *default-radius-class-graphics-style-line-width* 1)

#|
(setq *default-radius-class-graphics-style-line-width* nil)
(recompute-world-graphics-style-filter (3d-world (top-view)))
|#

(let ((line-width *default-radius-class-graphics-style-line-width*))
  (setq *radius-class-graphics-attributes-alist*
	`((radius-tool :color cyan)
	  (radius-window-label :color cyan)
	  (radius-3d-label :color cyan)
	  (radius-2d-label :color cyan)
	  (radius-site-name :color cyan)
	  (radius-north-arrow :color cyan :line-width ,line-width)
	  (radius-map-scale :color cyan :line-width ,line-width)
	  (radius-ruler :color cyan :line-width ,line-width)
	  (radius-reference-point :color cyan )

	  (radius-2d-curve :color cyan)
	  (radius-closed-2d-curve :color cyan)
	  (radius-ribbon-curve :color cyan)
	  (radius-loc :stipple ,*default-stipple* :color Orange )
	  (radius-road :stipple ,*default-stipple* :color Orange )
	  (radius-walkway :stipple ,*default-stipple* :color yellow )
	  (radius-street :stipple ,*default-stipple* :color Orange
	   :material-diffuse-color Brown)
	  (radius-highway :stipple ,*default-stipple* :color Orange
	   :material-diffuse-color Brown)
	  (radius-railroad :stipple ,*default-stipple* :color blue )
	  (radius-powerline :line-width ,line-width :color brown )
	  (radius-pipeline :line-width ,line-width :color brown )
	  (radius-runway :stipple ,*default-stipple* :color Orange )
	  (radius-building :line-width ,line-width :color yellow )
	  (radius-complex-building :line-width ,line-width :color yellow )
	  (radius-barracks :line-width ,line-width :color yellow )
	  (radius-warehouse :line-width ,line-width :color yellow )
	  (radius-quanset :line-width ,line-width :color yellow )
	  (radius-house :line-width ,line-width :color yellow )
	  (radius-shed :line-width ,line-width :color yellow )
	  (radius-hangar :line-width ,line-width :color yellow )
	  (radius-stadium :line-width ,line-width :color yellow )
	  (radius-tower :line-width ,line-width :color yellow )
	  (radius-structure :line-width ,line-width :color yellow )
	  (radius-bunker :color yellow )
	  (radius-pier :line-width ,line-width :color yellow )
	  (radius-bridge :line-width ,line-width :color yellow )
	  (radius-gate :color yellow )
	  (radius-tank :color yellow :line-width ,line-width)
	  (radius-pol-tank :color yellow :line-width ,line-width)
	  (radius-water-tank :color yellow :line-width ,line-width)
	  (radius-pole :color yellow :line-width ,line-width)
	  (radius-fence :stipple ,*default-stipple* :color yellow :line-width ,line-width ) 
	  (radius-wall :stipple ,*default-stipple* :color yellow :line-width ,line-width )
	  (radius-vegetation :stipple ,*light-stipple* :color LightGreen )
	  (radius-veg-boundary :line-width ,line-width :color LightGreen )
	  (radius-forest :stipple ,(fa-stipple) :color LightGreen )
	  (radius-field :stipple ,(fa-stipple) :color Orange )
	  (radius-water :stipple ,(fa-stipple) :color LightBlue )
	  (radius-shoreline :line-width ,line-width :color LightBlue )
	  (radius-water :stipple ,*default-stipple* :color LightBlue )
	  (radius-stream :stipple ,*default-stipple* :color LightBlue )
	  (radius-river :stipple ,*default-stipple* :color LightBlue )
	  (radius-lake :stipple ,(fa-stipple) :color LightBlue
	   :material-diffuse-color Blue)
	  (radius-functional-area :stipple ,(fa-stipple) :color magenta :line-width ,line-width )
	  (radius-site-perimeter  :color magenta :line-width 4 :dash-style (12 6))
   
	  (radius-rail-transfer-area :stipple ,*light-stipple* :color magenta)
	  (radius-motor-pool :stipple ,*light-stipple* :color magenta)
	  (radius-vehicle-park :stipple ,*light-stipple* :color magenta)
	  (radius-loading-dock :stipple ,*light-stipple* :color blue)
	  (radius-parade-field :stipple ,*light-stipple* :color magenta)
	  (radius-dtm-mesh  :color brown)
	  (complicated-structure :color  yellow)
	  )))

#| ;; removed Wed Sep 13 1995 -- now uses radius-graphics-style-filter

(defstruct-class hub-graphics-style-filter (radius-graphics-style-filter)
    ()
  #+never (:default-initargs :name "Hub Graphics Style Filter"
	    :otherwise-filter *default-cme-graphics-style-filter*))

(defstruct-class hub-world-graphics-style-filter (hub-graphics-style-filter)
    ()
  (:default-initargs
      :class-graphics-style-alist
      (make-graphics-style-alist *radius-class-graphics-attributes-alist*)))

(defvar *radius-class-graphics-style-filter*)

(defun initialize-hub-classes ()
  (setq *default-graphics-style-filter-class* 'hub-graphics-style-filter
	*default-world-graphics-style-filter-class* 'hub-world-graphics-style-filter
	*default-global-graphics-style-filter*
	(make-instance *default-graphics-style-filter-class*
		       :class-graphics-style-alist
		       (make-graphics-style-alist *radius-class-graphics-attributes-alist*))
	;; *radius-class-graphics-style-filter* isn't used in any important way -- only in comments
	*radius-class-graphics-style-filter* *default-global-graphics-style-filter*
	))
|#

(defun initialize-hub-classes ()
  )

;;(st::add-system-initialization :hub '(initialize-hub-classes))
(st::add-system-initialization :cme '(initialize-hub-classes))

#|
(setf (name smc::*radius-class-graphics-style-filter*) "Hub Site Graphic")

(graphics-style-filter (top-view))
(setf (graphics-style-filter (top-view)) *radius-class-graphics-style-filter*)

(let ((*enable-functional-area-stipples* t))
  (setf (class-graphics-style-alist *radius-class-graphics-style-filter*)
	(make-radius-class-graphics-style-alist)))

(compute-graphics-style *radius-class-graphics-style-filter* obj)
(object-graphic-style-attributes  *radius-class-graphics-style-filter* obj)
(progn smc::*radius-class-graphics-style-filter*)
(update-graphics-style obj)
(flush-graphics-style-filter-caches *radius-class-graphics-style-filter*)
(type-of obj)

(setf (object-graphics-style-alist *radius-class-graphics-style-filter*)
      (putassoc (object-graphics-style-alist *radius-class-graphics-style-filter*)
		    obj '(:color cyan)))

(setf (object-graphics-style-alist *radius-class-graphics-style-filter*)
      (remass obj (object-graphics-style-alist *radius-class-graphics-style-filter*)))
|#
