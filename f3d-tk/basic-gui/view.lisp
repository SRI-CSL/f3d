(in-package :gui)

;;; Contains NO direct dependencies on TK or OpenGL.

(declaim (special *interactor* *DEFAULT-MATERIAL-MODEL* *DEFAULT-LIGHT-SOURCE*))

;;; ************************  VIEWS  ***************************************

;;; LHQ Sun Jul 23 2006: In principle, the view can exist fine without
;;; a window, but should a view without a window be removed continue
;;; to hang onto the image (and other) resources, or should the view
;;; be removed from the children of its parent (2d-world)?  I vote for
;;; the later.  When the view is pushed onto the view-stack of a
;;; window, then we should add it to the children of its 2d-world.
;;; Alternatively, the list of children of the 2d-world could contain
;;; weak-pointers to its views.

(defstruct-class basic-view (basic-coordinate-system) ; 2d-cartesian-coordinate-system instead?
  ((window :initform nil :initarg :window :accessor view-window)
   (display-attributes :initform (default-view-display-attributes )
		       :initarg :display-attributes
		       :accessor display-attributes)
   ))

;;; does this belong here?
(defmethod transforms::weak-child-handle ((o basic-view))
  (if config::weak-eval-cache
      (or (get-prop o :weak-object-handle)
	  (setf (get-prop o :weak-object-handle)
		(lx::make-weak-pointer o)))
      o))

#+never
(defstruct-class basic-view (weak-child-coordinate-system) ; 2d-cartesian-coordinate-system instead?
  ((window :initform nil :initarg :window :accessor view-window)
   (display-attributes :initform (default-view-display-attributes )
		       :initarg :display-attributes
		       :accessor display-attributes)
   ))

;;; COPY (with 1 single arg) isn't a good thing to do here.  Major conflicts if this symbol is exported.
(defmethod copy-view ((view basic-view) &key shared-display-attributes &allow-other-keys)
  (with-class-slots basic-view (display-attributes) view
    (make-instance (type-of view)
                   :window nil
                   :display-attributes
                   (if shared-display-attributes
                       display-attributes
                       (copy display-attributes))
                   )))


;;; OBJECT-SETS is a list of object-set controlling generation of display-lists.
;;; The association of objects to views and the control of display-list
;;; generation is deferred to the object-sets.  Object-set classes can be
;;; implemented to provide object grouping as desired (ie. feature-sets).  See
;;; documentation for OBJECT-SETS

;;; New version Fri Sep 17 2004 using PARENT and OBJECT-TO-PARENT-TRANSFORM to implement
;;; 2D-WORLD and 2D-TO-WINDOW-TRANSFORM.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  4/13/2006 -CIC
;;;
;;; Need tandem views - locked in space OR time.  The scheme I went
;;; with here defines a class called GROUP, specialized to
;;; SPATIAL-GROUP and TEMPORAL-GROUP.  ONLY VIEWS can be tandem-ed.
;;; This was a lose with soft-slots, but if view objects are allowed
;;; to contain slots for their spatial and temporal groups, this is
;;; much faster.  Also faster than an equivalent hash-table
;;; implementation.
;;;

(defmethod view-group-views ((group (eql NIL))) nil)

;;; Modifications to the VIEW class that allow for grouping:

;;; It is really assumed here that a view belongs to at most 1 spatial
;;; and 1 temporal group.

;;; All normal VIEWs are spatial in nature.
(defstruct-class spatial-view-grouping-mixin ()
  ((spatial-group :initform nil :initarg :spatial-group :accessor view-spatial-group)))

;;; Some views are temporal in nature, including video views and timelines:
(defstruct-class temporal-view-grouping-mixin ()
  ((temporal-group :initform nil :initarg :temporal-group :accessor view-temporal-group)))



#|
MUST GUARANTEE THE FOLLOWING:
   2d-world = (parent view)
   2d-to-window-transform = (inverse-transform (object-to-parent-transform view))

   3d-world = (parent 2d-world)
   3d-to-2d-projection = (inverse-transform (object-to-parent-transform 2d-world))
   2d-world = (to-coordinate-system 3d-to-2d-projection)
   3d-world = (from-coordinate-system 3d-to-2d-projection)
|#


(defstruct-class view (basic-view spatial-view-grouping-mixin)
  (;; image should really be a 2d-object
   (image :initform nil :initarg :image :accessor view-image)
   (object-sets :initform nil :initarg :object-sets :reader object-sets)
   ))

;;; COPY (with 1 single arg) isn't a good thing to do here.  Major conflicts if this symbol is exported.
(defmethod copy-view :around ((view view) &key shared-display-attributes &allow-other-keys)
  (let* ((new-view (call-next-method)))
    (with-class-slots view (image object-sets display-attributes) view
      (setf (view-image new-view) image
	    (2d-world new-view) (2d-world view)
	    (2d-to-window-transform new-view) (copy (2d-to-window-transform view))
	    (object-sets new-view) object-sets ; should these be copies?
	    (display-attributes new-view) (if shared-display-attributes
					      display-attributes
					      (copy display-attributes))))
    new-view))

;;(define-accessor-synonym window-to-2d-transform view object-to-parent-transform)

(defmethod sensitive-p ((view view) (object-set object-set))
  (not (null (member object-set (sensitive-feature-sets view)))))

(define-soft-slot view  :sensitive-feature-sets sensitive-feature-sets)

(defmethod (setf sensitive-p) (state (view view) (object-set object-set))
  (if state
      (pushnew object-set (sensitive-feature-sets view))
      (setf (sensitive-feature-sets view)
	    (remove object-set (sensitive-feature-sets view)))))


(defmethod 2d-to-window-transform ((view view))
  (let ((object-to-parent-transform (object-to-parent-transform view)))
    (if object-to-parent-transform
	(inverse-transform object-to-parent-transform)
	(setf (2d-to-window-transform view)
	      (with-slots (image) view
		(default-2d-to-window-transform view image))))))

(defmethod (setf 2d-to-window-transform) (new-transform (view view))
  (connect-transforms (inverse-transform new-transform) new-transform view (2d-world view))
  (setf (object-to-parent-transform view) (inverse-transform new-transform))
  new-transform)

(define-accessor-synonym 2d-world view parent)

;;; FIXME:  These image methods do not belong here, but the image subsystem does not
;;; require the transforms subsystem and does not have access to the function MAKE-2D-WORLD
;;; Automatically generate a 2d-world for an image is it doesn't already exist.
(defmethod 2d-world ((image img::image))
  (or (get-prop image :2d-world)
      (setf (get-prop image :2d-world) (make-2d-world))))

(defmethod ensure-image-to-2d-transform ((image img::image))
  (or (image-to-2d-transform image)
      (setf (image-to-2d-transform image)
	    (transforms::make-4x4-coordinate-transform (math::make-4x4-identity-matrix)
						       :from-coordinate-system image
						       :to-coordinate-system (2d-world image)))))

;;; automatically generate an image-to-2d-transform for an image is it doesn't already exist.
;;; I am not sure that this is totally safe.  set-image-to-2d-transforms in image-pyramids.lisp
;;; "probes" the image-pyramid, looking for an image that has an image-to-2d-transform.
;;; That has been fixed, but there might be other instances of similar probing.
#+never ; might be unsafe
(defmethod image-to-2d-transform ((image img::image))
  (or (get-prop image :image-to-2d-transform)
      (setf (get-prop image :image-to-2d-transform)
	    (transforms::make-4x4-coordinate-transform (math::make-4x4-identity-matrix)
						       :from-coordinate-system image
						       :to-coordinate-system (2d-world image)))))

(defmethod initialize-instance :after ((view view) &key 
				       2d-world 2d-to-window-transform &allow-other-keys)
  (setf (2d-world view) 2d-world)
  (setf (get-prop view :dimensionality) 2) ; this is really the dimensionality associated with the window
  (with-class-slot-values view (object-sets) view
    (unless object-sets
      (when (and 2d-world (parent 2d-world))
	(setf object-sets (default-object-sets 2d-world))
	(let ((3d-world (3d-world 2d-world)))
	  (when 3d-world
	    (setf object-sets (append (default-object-sets 3d-world) object-sets)))))
      )
    ;; This call was in the previous version -- keep it?
    (when 2d-to-window-transform
      ;; This calls connect-transforms -- thus making view a child of the 2d-world
      ;; Do we really want this behavior?  
      ;; Should it really be a weak-pointer to the view?
      (setf (2d-to-window-transform view) 2d-to-window-transform))
    (setf (object-sets view) object-sets) ; this forces the object-sets to be sorted.
    )
  )


(defmethod image-to-2d-transform ((obj view))
  (let ((image (view-image obj)))
    (and image (image-to-2d-transform image))))

(defmethod transforms::object-to-view-transform ((object gl-2d-object-mixin) view)
  (list (object-to-world-transform object)
	(2d-to-window-transform view)))

;;; This doesn't work with multi-LVCS environment
;;;(defmethod transforms::object-to-view-transform ((object gl-3d-object-mixin) view)
;;;  (list (object-to-world-transform object)
;;;        (3d-to-2d-projection view)
;;;        (2d-to-window-transform view)))

;;; This is really object-to-window-transform, but the window is determined from the view argument.
;;; VIEW COORDINATES ARE WINDOW COORDINATES.
;;; Perhaps this should be called OBJECT-TO-VIEW-WINDOW-TRANSFORM
(defmethod transforms::object-to-view-transform ((object gl-3d-object-mixin) view)
  (let ((object-to-world-transform (object-to-world-transform object (3d-world view))))
    (if (consp object-to-world-transform)
	`(,@object-to-world-transform
	  ,(3d-to-2d-projection view)
	  ,(2d-to-window-transform view))
	`(,object-to-world-transform
	  ,(3d-to-2d-projection view)
	  ,(2d-to-window-transform view)))))
    

;;; This is currently unused
;;;(defmethod transforms::object-to-2d-transform ((object gl-2d-object-mixin) view)
;;;  (declare (ignore view))
;;;  (object-to-world-transform object))

;;; This is currently unused
;;;(defmethod transforms::object-to-2d-transform ((object gl-3d-object-mixin) view)
;;;  (list (object-to-world-transform object)
;;;        (3d-to-2d-projection view)))


#|
(defun print-vector (v)
  (let ((*print-array* t))
    (format t "~a~%" v)))

(print-vector (transform-vector (gui::object-to-world-transform (selected-object))
				(cv 0.0 0.0 0.0)))
(object-to-view-transform (selected-object) (top-view))
(print-vector (transform-vector (transforms::object-to-view-transform (selected-object) (top-view))
		  (cv 0.0 0.0 0.0)))

(print-vector (transform-vector (transforms::object-to-2d-transform (selected-object) (top-view))
				(cv 0.0 0.0 0.0)))
(print-vector (transform-vector (3d-to-2d-projection (top-view))
				(cv -129.642 4764.955 6067.465)))


(defmethod obj::cc-version-of-bounding-box ((v view) &optional (bbox (cv 0.0 0.0 0.0 0.0 0.0 0.0)) 
					    (reset t))
  (with-slots (object-sets) v
    (loop for o in object-sets
	  for r = reset then nil
	  do (obj::cc-version-of-bounding-box o bbox  r)))
  bbox)

(let ((sym 'object-to-2d-transform))
  (list (apropos sym)
	(symbol-package 'sym)))
|#

;;;;;; Explicit dependency on window:
;;;
;;;(defmethod print-object ((view basic-view) stream)
;;;  (format stream "#<~a ~@[~s~] #X~x>"
;;;          (type-of view)(and (view-window view) (widget (view-window view))) (%pointer view)))

(defmethod print-object ((view basic-view) stream)
  (print-unreadable-object (view stream :type t :identity t)
    (format stream "~@[~s~]" (and (view-window view) (widget (view-window view))))))

;;;;;; Explicit dependency on window:
;;;
;;; Sun Jul 23 2006 - should window and 2d-to-window-transform be unspecified until push-view is called?
;;; 
(defun make-view (window &rest initargs
			 &key image 2d-world 2d-to-window-transform
			 &allow-other-keys)
  (ignore image 2d-world 2d-to-window-transform)
  (when (stringp window) (setq window (widget-window window)))
  (let ((view (apply 'make-instance 'view :window window initargs)))
    view))


;;; How can a view be a transform?
;;;(defmethod transforms::linear-transform-p ((view view))
;;;  (transforms::linear-transform-p (2d-world view)))


;;;;;; Explicit dependency on window:
;;;
;;; This assumes the 2d-to-window-transform is already present.
(defmethod push-view ((view basic-view) window)
  (setf (view-window view) window)
  (push view (view-stack window))
  view)

(defmethod pop-view ((window basic-window))
  (pop (view-stack window)))

(defmethod 3d-world ((view view))
  (let ((2d-world (2d-world view)))
    (when 2d-world (3d-world 2d-world))))

;;; This isn't very useful, since (3d-world view) is guaranteed to be the same
;;; and is more direct.
;;;(defmethod view-3d-world ((view view))
;;;  (from-coordinate-system (3d-to-2d-projection view)))

;;; This isn't very useful, since 3d-to-2d-projection is found (2d-world view)
;;;(defmethod view-2d-world ((view view))
;;;  (to-coordinate-system (3d-to-2d-projection view)))

(defmethod 3d-to-2d-projection ((view view))
  (let ((2d-world (2d-world view)))
    (when 2d-world
      (3d-to-2d-projection 2d-world))))

#|
;;; These are needed because images get created with 2d-worlds are are not really 2d-worlds.
;;; In particular, dtm-images of terrain-models have a 2d-world which is connected 
;;; to a UTM coordinate-system.
(defmethod 3d-to-2d-projection (thing)
  nil)

(defmethod 3d-world (thing)
  nil)
|#

(defmethod (setf 3d-to-2d-projection) (3d-to-2d-projection (view view))
  (let ((2d-world (2d-world view)))
    (when 2d-world
      (setf (3d-to-2d-projection 2d-world) 3d-to-2d-projection))))


(defmethod 2d-to-window-matrix ((thing view))
  (transform-matrix (2d-to-window-transform thing)))

;;; currently there are no callers for this.
(defmethod visible-p ((view basic-view))
  (let ((window (view-window view)))
    (and (eq view (top-view window))
	 (visible-p window))))

(defmethod (setf object-sets) (new-object-sets (view view))
  (with-slots (object-sets) view
    (setf object-sets (sort new-object-sets #'< :key #'priority))))

(defmethod add-object-set (object-set (view view))
  (with-slots (object-sets) view
    (pushnew object-set object-sets)
    (setf object-sets (sort object-sets #'< :key #'priority))))

(defmethod remove-object-set (object-set (view view))
  (with-slots (object-sets) view
    (setq object-sets (remove object-set object-sets))))

(defmethod find-terrain-model ((vorld gl-3d-world))
  (get-prop vorld :terrain-model))


(defmethod find-terrain-model ((object gl-object))
  (find-terrain-model (parent object)))

(defmethod find-terrain-model ((object list))
  (find-terrain-model (car object)))

(defmethod find-terrain-model ((view view))
  (when (3d-world view)
    (find-terrain-model (3d-world view))))

;;; There is a problem trying to set the 2d-to-window transform if the view-window
;;; has not been configured.  I think the underlying problem is that a call to (do-events)
;;; is needed after creating new GL windows.
#+never
(defmethod initialize-instance :after ((view view) &key &allow-other-keys)
  (with-slot-values (2d-world object-sets) view
    (unless object-sets
      (when 2d-world
	(setf object-sets (default-object-sets 2d-world))
	(let ((3d-world (3d-world 2d-world)))
	  (when 3d-world
	    (setf object-sets (append (default-object-sets 3d-world) object-sets)))))
      )
    (setf (object-sets view) object-sets)
    ))


;;; hook to be supplied.
(defmethod default-object-sets ((world t))
  nil)
       
;; should this move to display.lisp or a file dedicated to parameter specifications?
;;(defparameter *default-view-background-color* (cv 1.0 .6 0.0 1.0))
(defparameter *default-view-background-color-name* "MediumPurple4")
(defparameter *default-view-foreground-color-name* "white")
;(defparameter *default-view-image-modulation-color* (cv .8 .8 .8 1.0))

;;; Should we change to this and use the default photometric-transform to implement attentuation?
;;; *default-view-image-modulation-color*interacts with *default-photometric-transform* 
(defparameter *default-view-image-modulation-color* (cv 1.0 1.0 1.0 1.0))

(defparameter *default-anti-alias-lines* nil)

(defstruct-class basic-view-display-attributes (fasd-form-property-list-mixin)
  ((background-color :initform (gl-color-named *default-view-background-color-name*)
		     :initarg :background-color)
   (foreground-color :initform (gl-color-named *default-view-foreground-color-name*)
		     :initarg :foreground-color)
   (anti-alias-lines :initform *default-anti-alias-lines* :initarg :anti-alias-lines))
    
  (:default-initform nil) :initable-slots :accessible-slots
  )

(defmethod background-color ((window basic-window))
  (let ((view (top-view window)))
    (if view
	(background-color (display-attributes view))
	(gl-color-named *default-view-background-color-name*))))

(defmethod foreground-color ((window basic-window))
  (let ((view (top-view window)))
    (if view
	(foreground-color (display-attributes view))
	(gl-color-named *default-view-foreground-color-name*))))


(defstruct-class view-display-attributes (basic-view-display-attributes)
  ((image-modulation-color :initform *default-view-image-modulation-color* :initarg :image-modulation-color)
   (shading-enabled :initform nil :initarg :shading-enabled)
   (default-material :initform *default-material-model* :initarg :default-material)
   (default-light-source :initform *default-light-source* :initarg :default-light-source)
   )
  (:default-initform nil) :initable-slots :accessible-slots
  )



(define-fasd-form-init-plist view-display-attributes
    (with-slots (background-color image-modulation-color anti-alias-lines
				  shading-enabled default-material default-light-source)
	self
      `(:background-color ',background-color
	:image-modulation-color ',image-modulation-color
	:anti-alias-lines ',anti-alias-lines
	:shading-enabled ',shading-enabled
	:default-material ,(fasd-form default-material)
	:default-light-source ,(fasd-form default-light-source)
	)))
		 

;;; This is shared among all views.
(defparameter *default-view-display-attributes* nil)

(defparameter *default-view-display-attributes-shared* nil)

(defmethod-cached gl-color-named ((name string) &optional (widget "."))
  (let ((rgb (lx::color-name-to-3d name))) ;; (tk::tk-color-rgb name :widget widget)))
    (if rgb
	(destructuring-bind (r g b) rgb
	  (cv r g b 1.0))
	(cv .0 .0 .0 1.0)		; default to black if not found
	)))

(defun default-view-display-attributes (&optional (shared *default-view-display-attributes-shared*))
  (if shared
      (or *default-view-display-attributes*
	  (setq *default-view-display-attributes*
		(make-instance 'view-display-attributes
			       :background-color (gl-color-named *default-view-background-color-name*)
			       :image-modulation-color *default-view-image-modulation-color*
			       )))
      (make-instance 'view-display-attributes
		     :background-color (gl-color-named *default-view-background-color-name*)
		     :image-modulation-color *default-view-image-modulation-color*
		     )))



;;; hook to be supplied.
;;;(defmethod set-default-graphics-attributes ((view view))
;;;  (with-slots (graphics-attributes) view
;;;    (if graphics-attributes
;;;        (set-default-graphics-attributes graphics-attributes)
;;;        (set-object-drawing-parameters view) ; fallback to old funky function
;;;        )))

;;;(defmethod set-default-graphics-attributes ((view view))
;;;  (with-slots (graphics-attributes) view
;;;    (if graphics-attributes
;;;        (set-default-graphics-attributes graphics-attributes)
;;;        (set-object-drawing-parameters view) ; fallback to old funky function
;;;        )))



(defmethod redisplay-required  ((view view) &optional (interactor *interactor*) ignore)
  (declare (ignore ignore))
  (loop for object-set in (object-sets view)
	thereis (redisplay-required object-set
				    (selected-objects-in-world (world object-set) interactor)
			             view)))

(defun map-over-all-views-internal (fn)
  (map-over-all-windows (window)
   (loop for view in (view-stack window)
	 do (funcall fn view))))

(defmethod sun-vector ((view view))
  (or (get-prop view :sun-vector)
      (let ((image (view-image view)))
	(and image (get-prop image :sun-vector)))))

#|
(progn (setq l nil)
       (map-over-active-world-views ((cme::get-3d-world-named "alv") view)
				    (push view l))
       l)

(selected-object)
(object-sets-containing-objects (list (selected-object)))

|#

;;; Set MODELVIEW matrix and PROJECTION_MATRIX in OpenGL pipeline for
;;; drawing 3d-objects.
;;;(defmethod set-3d-matrices ((view view))
;;;  (set-2d-matrices view)
;;;  (let ((3d-to-2d-projection (3d-to-2d-projection view)))
;;;    (when 3d-to-2d-projection
;;;      (set-gl-3d-to-2d-projection 3d-to-2d-projection)
;;;      t)))

(defmethod set-3d-matrices ((view view))
  (set-2d-matrices view)
  (let ((3d-to-2d-projection (3d-to-2d-projection view)))
    (when 3d-to-2d-projection
      (set-gl-3d-to-2d-projection (or (surrogate-projection 3d-to-2d-projection)
				      3d-to-2d-projection))				  
      t)))


(defun 1st-quadrant-window-to-ndc-matrix (window)
  ;;(format t "1st-quadrant-window-to-ndc-matrix~%")
  (mv-bind (width height) (dimensions window)
    (let ((mat (make-4x4-identity-matrix )))
      (setf (aref mat 0 0) (/ 2.0 width) (aref mat 0 3) -1.0
	    (aref mat 1 1) (/ 2.0 height) (aref mat 1 3) -1.0
	    (aref mat 2 2) 2.0 (aref mat 2 3) -1.0
	    )
      mat)))

(defun 3rd-quadrant-window-to-ndc-matrix (window)
  ;;(format t "1st-quadrant-window-to-ndc-matrix~%")
  (mv-bind (width height) (dimensions window)
    (let ((mat (make-4x4-identity-matrix )))
      (declare (dmatrix mat))
      (setf (aref mat 0 0) (/ 2.0 width) (aref mat 0 3) -1.0
	    (aref mat 1 1) (/ -2.0 height) (aref mat 1 3) 1.0
	    (aref mat 2 2) 2.0 (aref mat 2 3) -1.0
	    )
      mat)))

;;; Set MODELVIEW matrix and PROJECTION_MATRIX in OpenGL pipeline for
;;; drawing 2d-objects.
(defmethod set-2d-matrices ((view view))
  (let ((2d-to-window-matrix (2d-to-window-matrix view)))
    ;; should something happen when 2d-to-window-matrix is NULL?
    (when 2d-to-window-matrix
      (gui::set-2d-to-ndc-matrix 2d-to-window-matrix) ;; sets GL_PROJECTION_MATRIX - 3rd quadrant
      (load-gl-modelview-matrix nil)
      t)))


;;; *************   MAPPING WINDOW MOTIONS TO WORLD MOTIONS  *************

(defmethod project-window-distance-to-world-distance
	   ((view view) (world gl-2d-world) world-position window-distance)
  (ignore world-position)
  ;; coordinate-transform-scale-factor doesn't work for non-linear projections
  (let ((sf (transform-scale-factor (2d-to-window-transform view) nil)))
    (if (zerop sf) window-distance (/ window-distance sf))))

(defmethod project-window-distance-to-world-distance
	   ((view view) (world gl-3d-world) world-position window-distance)
  ;; coordinate-transform-scale-factor doesn't work for non-linear projections
  (let ((3d-units/2d-unit (compute-gsd (3d-to-2d-projection view) world-position))
	(2d-distance (project-window-distance-to-world-distance
		      view (2d-world view) world-position window-distance)))
    (* 3d-units/2d-unit 2d-distance)))

(defmethod project-window-motion-to-world-motion
	   ((view view) (world gl-2d-world)
	    window-motion-vector world-position &optional on-dtm)
  (ignore on-dtm world-position )
  (transform-window-motion-to-2d-world-motion (2d-to-window-transform view)
					      window-motion-vector))

(defmethod project-window-motion-to-parent-motion
	   ((view view) (world gl-2d-world)
	    window-motion-vector world-position &optional on-dtm)
  (ignore on-dtm world-position )
  (transform-window-motion-to-2d-world-motion (2d-to-window-transform view)
					      window-motion-vector))

;;;(defmethod project-window-motion-to-world-motion
;;;           ((view view) (world gl-3d-world)
;;;            window-motion-vector world-position &optional on-dtm)
;;;  (ignore on-dtm world-position )
;;;  (project-2d-motion-to-3d-motion
;;;   (3d-to-2d-projection view)
;;;   (transform-window-motion-to-2d-world-motion (2d-to-window-transform view)
;;;                                               window-motion-vector)
;;;   world-position
;;;   #+never (if (eq on-dtm t)
;;;               (find-terrain-model view )
;;;               on-dtm)
;;;   ))

(defmethod project-window-motion-to-world-motion
	   ((view view) (world gl-3d-world)
	    window-motion-vector world-position &optional on-dtm)
  (ignore on-dtm world-position )
  (let ((view-3d-world (3d-world view))
	(world-motion (project-2d-motion-to-3d-motion
		       (3d-to-2d-projection view)
		       (transform-window-motion-to-2d-world-motion (2d-to-window-transform view)
								   window-motion-vector)
		       world-position)))
    (if (eq world view-3d-world)
	world-motion
	(transform-direction-vector (cached-optimized-cs-to-cs-transform view-3d-world world)
				    world-motion))
    ))



;;; This should work for both 3d-objects and 2d-objects	     
;;;(defmethod project-window-motion-to-world-motion
;;;           ((view view) (object gl-object)
;;;            window-motion-vector object-position &optional on-dtm)
;;;  (ignore on-dtm object-position )
;;;  (let ((world (object-view-world object view)))
;;;    (project-world-motion-to-object-motion
;;;     (obj::world-to-object-transform world object)
;;;     (project-window-motion-to-world-motion ; this function is undefined
;;;      view world
;;;      window-motion-vector
;;;      (transform-vector (object-to-world-transform object world) 
;;;                        object-position)))))

(defmethod project-window-motion-to-world-motion
	   ((view view) (object gl-object)
	    window-motion-vector object-position &optional on-dtm)
  (ignore on-dtm)
  (let ((world (object-view-world object view)))
    (transform-direction-vector (obj::world-to-object-transform world object)
				(project-window-motion-to-world-motion ; this function is undefined
				 view world
				 window-motion-vector
				 (transform-vector (object-to-world-transform object world) 
						   object-position)))))

#|
(project-window-motion-to-world-motion (top-view) (selected-object) (cv 1.0 0.0) 
				       (selected-object-position))
(let ((view (top-view))
      (object (selected-object))
      (motion (cv 1.0 0.0)))
  (project-window-motion-to-world-motion view (object-view-world object view)
					 motion (selected-object-world-position)))

(compute-gsd (3d-to-2d-projection (top-view)) (selected-object-world-position))
|#


(defmethod lisptk::process-result ((image img::image))
  (let ((pane (selected-window *interactor*)))
    (when pane
      (let ((view (top-view pane)))
	(when (or (null view) (neq image (view-image view)))
	  (push-image image pane))))))



;;;
;;; Mod to allow arrow keys to pan around - CC 4/1/2004 (no foolin!)
;;;
(defvar *in-scroll-view* nil)  ;; not sure if we will need this.  We don't want multiple / recursive calls to render-window.

#+old
(defmethod scroll-view ((view view) dx dy &optional (nsteps 10))
  (let ((xf (2d-to-window-transform view))
	(window (view-window view))
	(delta (math::cv (round dx nsteps) (round dy nsteps) 0.0)))
    (loop for i from 1 to nsteps
	  do (move-by xf delta)
	     ;; (redisplay view)
	     (render-window window nil)
	  )))

#-old
(defmethod scroll-view ((view view) dx dy &optional ignore)
  (let ((xf (2d-to-window-transform view))
	(window (view-window view))
	(delta (math::cv dx dy 0.0)))
     (move-by xf delta)
     (render-window window nil)
     ))
;;;
;;; Synthetic view: Allows a 3d-world to be pushed on a pane.  This
;;; presents the site as pure geometry, with no image, and is intended
;;; to allow the user to view the site from arbitrary vantage points.
;;;

;;;
;;; This is going to look a lot more like the standard view, with a
;;; frame-camera.  The difference here is that there is no image
;;; associated with this view, hence the frame-camera can be moved at
;;; will to render the site from arbitrary vantage points.
;;;

(defstruct-class synthetic-view (view) ())
 
(defmethod initialize-instance :after ((view synthetic-view) &rest args)
  (declare (ignore args))
  ;; Force this view to have no image.  This causes the geometry to be
  ;; rendered from the viewpoint provided by the 3d-to-2d-projection.
  ;; Specialize this to display other objects that may be desired.
  (setf (view-image view) nil)
  ;;; Arbitrary choice, but let's try it. 
  (setf (get-prop view :sun-vector) (cv 1.0 1.0 1.0 0.0))
  (let ((attr (display-attributes view)))
    (setf (shading-enabled attr) t)
    ;; (setf (default-material attr) something??)
    (setf (background-color attr) (cv 0.5 0.5 1.0 1.0))
    (setf (default-material attr)
	  (make-instance 'obj::material-model
			 :diffuse-color (fv 0.6 0.6 0.6 1.0)
			 :ambient-color (fv 0.0 0.6 0.6 1.0)
			 :specular-color (fv 0.8 0.8 0.8 1.0)
			 :shininess 3.0))
    (setf (default-light-source attr)
	  ;; I am not sure why CC wanted these colors for default-light-source components
	  ;; (I'm not sure either.  Just playing around. -CC).
	  (make-instance 'infinite-light-source
			 :direction (fv 1.0 1.0 1.0 0.0)
			 :diffuse-color (fv 0.3 0.3 0.3 1.0)
			 :ambient-color (fv 1.0 1.0 1.0 1.0)
			 :specular-color (fv 0.0 1.0 1.0 1.0)))
    ))

(defmethod show-image-point-info (position (view synthetic-view))
  (format t "~%show-image-point-info for synthetic-view is under construction.")
  nil)


(defmethod set-default-graphics-attributes  ((view synthetic-view))
  (let* ((attributes (display-attributes view)))
    (glColor3d 1.0d0 1.0d0 0.0d0)		; yellow 
    (glPolygonMode GL_FRONT GL_LINE)
    (if t
	(glPolygonMode GL_BACK GL_LINE) ; what is this for? - perhaps for glCullFace GL_BACK?
	(glPolygonMode GL_BACK GL_LINE))
    (glFrontFace GL_CCW)
    (glCullFace GL_BACK) ; this doesn't work for lines, only for fill mode
    ;; (glEnable GL_CULL_FACE)
    (glDisable GL_CULL_FACE)
    (if (and *enable-depth-test* 
	     (get-prop (3d-to-2d-projection view) :near-far))
	(glEnable GL_DEPTH_TEST)
	(glDisable GL_DEPTH_TEST))
    (glDisable GL_LIGHTING)
    (glDisable GL_POLYGON_STIPPLE)

    #+never
    (format t "set-default-graphics-attributes ~a ~a~%" 
	    view (and *enable-depth-test* (get-prop (3d-to-2d-projection view) :near-far)))
  
    (enable-anti-aliasing (anti-alias-lines attributes))))

#||
;;;
;;; centroid is a bit peculiar in that it returns a 4-vector.  The
;;; last element of the 4-vector is the number of points, while the
;;; first 3 elements actually represent the SUM of xyz positions.  The
;;; centroid must be post-processed by dividing out the number of
;;; points to get the true values.
;;;

(defmethod centroid ((obj t) &optional (into-vector (cv 0.0 0.0 0.0 0.0)) (reset t))
  into-vector)

(defmethod centroid ((obj obj::basic-coordinate-system) &optional (into-vector (cv 0.0 0.0 0.0 0.0)) (reset t))
  (when reset (fill into-vector 0.0))
  (let ((o (origin obj)))
    (incf (aref into-vector 0) (aref o 0))
    (incf (aref into-vector 1) (aref o 1))
    (incf (aref into-vector 2) (aref o 2))
    (incf (aref into-vector 3)))
  into-vector)

;;; What target coordinate-system?
(defmethod centroid ((obj obj::gl-object) &optional (into-vector (cv 0.0 0.0 0.0 0.0)) (reset t))
  (let ((varray (obj::vertex-array obj)))
    (when varray
      (let ((wv (cv 0.0 0.0 0.0))
	    (xf (object-to-world-transform obj))
	    (nv (array-dimension varray 0)))
	(loop for r = reset then nil
	      for i fixnum from 0 below nv
	      for v = (obj::vertex-array-vertex varray i)
	      do (transform-vector xf v wv)
	      (when reset (fill into-vector 0.0))
	      (loop for j fixnum from 0 below 3
		    do (incf (aref into-vector j) (aref wv j)))
	      (incf (aref into-vector 3))))))
  into-vector)
	  
(defmethod centroid ((obj 3d-world) &optional (into-vector (cv 0.0 0.0 0.0 0.0)) (reset t))
  (with-slots (cme::feature-sets) obj
    (loop for r = reset then nil
	  for fs in cme::feature-sets
	  do (centroid fs into-vector r)))
  into-vector)

(defun finalize-centroid (centroid)
  (cv (/ (aref centroid 0) (aref centroid 3))
      (/ (aref centroid 1) (aref centroid 3))
      (/ (aref centroid 2) (aref centroid 3))))

(defmethod obj::centroid ((obj feature-set) &optional (into-vector (cv 0.0 0.0 0.0 0.0)) (reset t))
  (loop for r = reset then nil
	for fs in (inferiors obj)
	do (centroid fs into-vector r))
  into-vector)
||#

 ;;; The code on this page is currently (Thu Sep 30 2004) unused.

(defun flat-list (&rest args)
  (loop for x in args
	if (consp x) 
	  append x
	else if x
	       collect x))

(defmethod object-to-2d-world-transform ((object gl-3d-object-mixin) 2d-world)
  (flat-list (object-to-world-transform object (3d-world 2d-world))
             (3d-to-2d-projection 2d-world)))

(defmethod object-to-2d-world-transform ((object gl-2d-object-mixin) 2d-world)
  (object-to-world-transform object 2d-world))

;;; This is the only caller to OBJECT-TO-2D-WORLD-TRANSFORM
(defmethod object-bounding-box-in-2d-world ((object gl-object) 2d-world)
  (obj::transform-bounding-box (object-to-2d-world-transform object 2d-world)
			       (obj::bounding-box object)))

;;; This handles anything other than 3d-objects
;;;(defmethod object-to-view-world-transform ((object t) view)
;;;  (declare (ignore view))
;;;  (object-to-world-transform object))

;;;(defmethod object-to-view-world-transform ((object gl-3d-object-mixin) view)
;;;  (let ((3d-world (3d-world view)))
;;;    (unless 3d-world (error "object-to-view-world-transform view has no 3d-world"))
;;;    (obj::3d-object-to-world-transform object 3d-world)))

;;;(defmethod object-to-view-world-transform ((object gl-2d-object-mixin) view)
;;;  (let ((2d-world (2d-world view)))
;;;    (unless 2d-world (error "object-to-view-world-transform view has no 2d-world"))
;;;    (obj::2d-object-to-world-transform object 2d-world)))

(defmethod object-to-image-transform ((object gl-3d-object-mixin) image)
  (let ((2d-world (2d-world image)))
    (unless 2d-world (error "object-to-image-transform: image has no 2d-world"))
    (flat-list (object-to-world-transform object (3d-world 2d-world))
	       (3d-to-2d-projection 2d-world)
	       (inverse-transform (image-to-2d-transform image)))))

(defmethod object-to-image-transform ((object gl-2d-object-mixin) image)
  (let ((2d-world (2d-world image)))
    (unless 2d-world (error "object-to-image-transform: image has no 2d-world"))
    (flat-list (object-to-world-transform object 2d-world)
	       (inverse-transform (image-to-2d-transform image)))))

;;; This is the only caller to object-to-image-transform
(defmethod object-bounding-box-in-image ((object gl-object) image)
  (obj::transform-bounding-box (object-to-image-transform object image)
			       (obj::bounding-box object)))

(defmethod object-bounding-box-in-superior-cs ((object gl-object) superior-cs)
  (obj::transform-bounding-box (cs-to-superior-cs-transform object superior-cs)
			       (obj::bounding-box object)))

#|
(object-to-2d-world-transform (gui::selected-object) (2d-world (gui::top-view)))
(setq *trans* (object-to-image-transform (gui::selected-object) (gui::view-image (gui::top-view))))
(transforms::canonical-coordinate-transform-path (gui::selected-object) (gui::top-view))
(transforms::canonical-coordinate-transform-path (gui::selected-object) (2d-world (gui::top-view)))
(transforms::canonical-coordinate-transform-path (gui::selected-object) (3d-world (gui::top-view)))
(transforms::canonical-coordinate-transform-path (gui::selected-object) alv-2-44-lvcs)

(object-bounding-box-in-superior-cs (gui::selected-object) (3d-world (top-view)))
(object-bounding-box-in-2d-world (gui::selected-object) (2d-world (top-view)))
(object-bounding-box-in-image (gui::selected-object) (view-image (top-view)))
(obj::bounding-box (gui::selected-object))
(describe (image-to-2d-transform (view-image (top-view))))
(object-to-2d-world-transform (gui::selected-object) (2d-world (top-view)))
(object-to-image-transform (gui::selected-object) (view-image (top-view)))
|#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  4/13/2006 -CIC
;;;
;;; View groups - allow views to be grouped spatially, temporally, or
;;; both.  Can be used to implement tandem views.


(defclass view-group ()
    ((view-list :initform nil :initarg :view-list :accessor view-group-views)))


(defun clear-view-group (group)
  (loop for v in (view-group-views group)
        do (remove-view v group)))


(defun set-view-group (group list)
  (loop for v in list
        do (add-view v group)))

;;;
;;; Basic methods: add and remove views.  Maintenance of
;;; cross-references appears in the specializations:
(defmethod add-view ((view view) (group view-group))
  (pushnew view (view-group-views group)))

(defmethod remove-view ((view view) (group view-group))
  (setf (view-group-views group)
	(remove view (view-group-views group))))

(defmethod remove-view ((view view) (group (eql NIL)))
  nil)


(defclass spatial-view-group (view-group)
    ())

;;; For safety's sake, always set the view's spatial group to NIL
;;; after removal:
(defmethod remove-view :after ((view view) (group spatial-view-group))
  (setf (view-spatial-group view) nil))


;;; First remove the view from any existing spatial-view-groups, set the new

(defmethod add-view :before ((view view) (group spatial-view-group))
  (remove-view view (view-spatial-group view)))

(defmethod add-view :after ((view view) (group spatial-view-group))
  (setf (view-spatial-group view) group))





(defclass temporal-view-group (view-group)
    ())

;;; For safety's sake, always set the view's spatial group to NIL
;;; after removal:
(defmethod remove-view :after ((view view) (group temporal-view-group))
  (setf (view-temporal-group view) nil))


;;; First remove the view from any existing spatial-view-groups, set the new

(defmethod add-view :before ((view view) (group temporal-view-group))
  (remove-view view (view-temporal-group view)))

(defmethod add-view :after ((view view) (group temporal-view-group))
  (setf (view-temporal-group view) group))



(defun group-views-spatially (views)
  (let ((new-group (make-instance 'spatial-view-group)))
    (loop for v in views when v do (add-view v new-group))
    new-group))


(defun group-views-temporally (views)
  (let ((new-group (make-instance 'temporal-view-group)))
    (loop for v in views when v do (add-view v new-group))
    new-group))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  4/13/2006 -CIC
;;;
;;; Generic functions for enabling "tandem" behavior - ok for this file??


(defmethod tandem-views-in-frame ((pane basic-window))
  (tandem-views-in-frame (tk::panel pane)))

(defmethod tandem-views-in-frame (frame)
  (group-views-spatially (loop for p in (inferiors frame)
			       for v = (top-view p)
			       when v collect v)))

(defun find-view-group (pane)
  (loop for p in (inferiors (tk::panel pane))
	for v = (top-view p)
	thereis (and v (view-spatial-group v))))

(defmethod untandem-views ((view view))
  (clear-view-group (view-spatial-group view)))

(defmethod untandem-views ((pane basic-window))
  (untandem-views (top-view pane)))


(defmethod untandem-views-in-frame (frame)
  (ungroup-views-spatially (loop for p in (inferiors frame)
			       for v = (top-view p)
			       when v collect v)))
  

(defun get-spatial-views (view)
  (with-class-slot-values view (spatial-group) view
    (when spatial-group
      (with-class-slot-values view-group (view-list) spatial-group
	view-list))))


(defun get-temporal-views (view)
  (with-class-slot-values view (temporal-group) view
    (when temporal-group
      (with-class-slot-values view-group (view-list) temporal-group
	view-list))))



(defun redisplay-tandem (&optional (v (top-view)))
  (loop for v in (get-spatial-views v)
	do (redisplay v)))



(defmethod window-center-point ((window basic-window))
  (cv (* 0.5 (window-width window))
      (* 0.5 (window-height window))
      0.0))

(defmethod window-center-point ((view view))
  (window-center-point (view-window view)))

;;;
;;; This computes the point on the ground at which the view is
;;; centered within its window:
(defun ground-point-for-view (view)
  (let ((site (3d-world view)))
    (when site
      (let ((terrain (get-prop site :terrain-model)))
	(when terrain
	  (let ((camera (3d-to-2d-projection view)))
	     (intersect-camera-ray-with-terrain-model
	      terrain camera
	      (inverse-transform-vector
	       (2d-to-window-transform view)
	       (window-center-point view)))))))))


;;;
;;; Set the view's 2d-to-window transform so that the specified ground
;;; point projects to the center of the view-window:

(defun set-ground-point-for-view (view ground-pos)
  (declare (coordinate-vector ground-pos))
  (bind-vector-elements (dx dy dz)
      (vector-difference  (window-center-point view)
                          (transform-vector
                           (2d-to-window-transform view)
                           (transform-vector (3d-to-2d-projection view)
                                             ground-pos)))
    ;; Make sure z=0.0!
    (move-by (2d-to-window-transform view) (cv dx dy 0.0))))


;;;
;;; Update all spatial tandem views so that they look at the same
;;; point that VIEW sees:
(defun translate-spatial-tandem-views (view &optional delta-vector)
  (let ((group (get-spatial-views view)))
    (when group
      (let ((ground-pos (ground-point-for-view view)))
	(loop for v in group
	      unless (eq view v)
		do (if ground-pos
		       (set-ground-point-for-view v ground-pos)
		       (when delta-vector
			 (move-by (2d-to-window-transform view) delta-vector)))
		   (redisplay v))
	))))

