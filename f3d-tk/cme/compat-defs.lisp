(in-package :cme)

(defvar *feet-to-meters* (* 0.0254 12.0))

;;;  The cache-p keyword arg is not currently used by any callers of
;;;  make-coordinate-transform.  The x,y,z keyword args will be eliminated in
;;;  the future.
 
(defun make-coordinate-transform-keyword-form
    (&rest args &key from-coordinate-system to-coordinate-system cache-p 
           matrix position x y z rotations
	   omega-radians phi-radians kappa-radians
	   omega-degrees phi-degrees kappa-degrees
           (class '4x4-coordinate-transform)
           &allow-other-keys)
  (ignore from-coordinate-system to-coordinate-system)

  (when (or omega-radians phi-radians kappa-radians)
    (setf rotations (list :x-rot (or omega-radians 0.0)
			  :y-rot (or phi-radians 0.0)
			  :z-rot (or kappa-radians 0.0))))

  (when (or omega-degrees phi-degrees kappa-degrees)
    (setf rotations (list :x-rot (* pi (/ (or omega-degrees 0.0) 180.0))
			  :y-rot (* pi (/ (or phi-degrees 0.0) 180.0))
			  :z-rot (* pi (/ (or kappa-degrees 0.0) 180.0)))))
  
  (when position
    (mv-setq-vector-elements (x y z) position))

  (unless matrix
    (setq matrix (apply 'make-coord-frame-matrix (or x 0.0) (or y 0.0) (or z 0.0) rotations)))

  (let ((transform 
         (apply (if cache-p 'make-cached-instance 'make-instance)
                class
                :transform-matrix matrix
                :transform-function '4x4-coordinate-transform-transform-vector
                args)))
    (when (and x y) (set-origin transform (coordinate-vector x y (or z 0.0))))
    ;;(break)
    transform))

;;; Tue Jun 18 1996 -- hacked to handle either keyword or make-coord-frame form of arglists.

;;; This is need for backward compatibility with CME.  It should move the the CME subsystem.
;;;(defun make-coord-frame-matrix (x y z &rest matrix-or-axis-angle-pairs)
;;;  (let ((mat (if (arrayp (car matrix-or-axis-angle-pairs))
;;;                 (copy-4x4-matrix (car matrix-or-axis-angle-pairs)) ; copy an orientation matrix
;;;                 (apply 'make-object-to-parent-orientation-matrix matrix-or-axis-angle-pairs))))
;;;    (setf (aref mat 0 3) (dfloat x)
;;;          (aref mat 1 3) (dfloat y)
;;;          (aref mat 2 3) (dfloat z))
;;;    mat))

;;; This is need for backward compatibility with CME.  It should move the the CME subsystem.
;;;(defun make-coord-frame-matrix (x y z &rest matrix-or-axis-angle-pairs)
;;;  (let ((mat (if (arrayp (car matrix-or-axis-angle-pairs))
;;;                 (copy-4x4-matrix (car matrix-or-axis-angle-pairs)) ; copy an orientation matrix
;;;                 (apply 'math::MAKE-OBJECT-TO-PARENT-ORIENTATION-MATRIX matrix-or-axis-angle-pairs))))
;;;    (setf (aref mat 0 3) (dfloat x)
;;;          (aref mat 1 3) (dfloat y)
;;;          (aref mat 2 3) (dfloat z))
;;;    mat))

(defun make-coord-frame-matrix (x y z &rest matrix-or-axis-angle-pairs)
  (apply 'math::MAKE-OBJECT-TO-PARENT-MATRIX (cv x y z) matrix-or-axis-angle-pairs))

;(make-coord-frame-matrix 10.0 20.0 30.0 :omega 1.0 :phi 2.0 :kappa 3.0)

(defun make-coordinate-transform (&rest args)
  (if (keywordp (car args))
      (apply 'make-coordinate-transform-keyword-form args)
      
      ;; args must be (x y z . axis-angle-pairs)
      (make-coordinate-transform-keyword-form
       :matrix (apply 'make-coord-frame-matrix args))))
       

#+unused
(defun make-cached-instance (class &rest init-options)
  ;; hack for backward compatibility with old site models.
  (case class
    (cme::3d-world (apply 'cme::make-3d-world init-options))
    (cme::2d-world (apply 'cme::get-or-make-2d-world init-options))
    (t ;;; be sure :property-list is canonicalized
     (let ((prop-list (getf init-options :property-list)))
       (when prop-list
	 (setf (getf init-options :property-list)
	       (sort-keyword-value-list prop-list))))

     ;; THIS TOTALLY SUCKS -- if making the instance causes causes interactions with asnything else
     ;; such as making pointers from somewhere else to this guy, then we lose big.
     (let* ((object (apply 'make-instance class init-options))
	    (init-form (fasd-form object))
	    )
       ;;(setq *make-cached-instance-last-init-form* init-form)
       ;;(break)
       ;; This is really awful -- an instance is built just so we can compute a canonical
       ;; init-plist.  If the init-plist matches something is the cache, then we win
       ;; otherwise, the a new cache entry is made.
       (if (memq (car init-form) '(make-cached-instance make-instance))
	   (eval-cache
	       ;; This should really be:  (make-cached-instance . (cdr init-form))
	       ;; This next ABSOLUTELY REQUIRES that FASD-FORM returns a form like:
	       ;;          (make-instance-fn 'flavor . init-plist)
	       (make-cached-instance (cadr (cadr init-form )) (cddr init-form))
	       object)
	   (eval-cache (make-cached-instance class init-options)
	       object))))))


;;; Code moved here from object-classes.lisp in preparation to remove that file

;;; OBJECT-FEATURE-SETS is needed so that REMOVE-SELF can delete the object from 
;;; any feature-sets containing it.

;;; The coord-frame keyword is archaic and should be eliminated.
;;; Some old feature-sets still use :COORD-FRAME
(defmethod initialize-instance :after ((object basic-object)
				       &key graphics-style
				       feature-sets
				       coord-frame
				       object-to-world-transform
				       &allow-other-keys)
  (when coord-frame 
    (warn "initialize-instance :after basic-object using obsolete keyword :COORD-FRAME~%")
    (setf (object-to-parent-transform object) (eval coord-frame)))
  #+old ; moved-to-basic-gl-object
  (when (and graphics-style (consp graphics-style))
    ;; create a graphics-style from a list of keyword options.
    (setf (graphics-style object) (apply 'make-instance 'graphics-style graphics-style)))
  (loop for fs in feature-sets do (add-object object fs))
  )

;;;(defmethod (setf world) (new-world (object basic-object))
;;;  (let ((world (world object)))
;;;    (when (and world (neq world new-world))
;;;      (format t ";;; Warning: moving object ~a from world ~a to ~a~%" object world new-world)
;;;      (setf (find-object-named world (name object)) nil))
;;;
;;;    (when (and new-world (neq world new-world))
;;;      (setf (find-object-named new-world (name object)) object))
;;;    ;; this is a loss --- needs to fix the object-to-world-transform
;;;    (setf (parent object) new-world)))

#+never ; Fri Dec 31 2004 - eliminate
(defmethod (setf world) (new-world (object basic-object))
  (let ((world (world object)))
    (when (neq world new-world)
      (when world
	(format t ";;; Warning: moving object ~a from world ~a to ~a~%" object world new-world)
	(setf (find-object-named world (name object)) nil))
      (when new-world
	(setf (find-object-named new-world (name object)) object)))

    ;; this is a loss --- needs to fix the object-to-world-transform
    ;; This is wrong if the object is an inferior of a composite-object
    (setf (parent object) new-world)))

;;; FIXME:  inferiors keyword is deprecated.  
(defmethod initialize-instance :after 
	   ((feature-set feature-set) &key inferiors &allow-other-keys)
  ;;(unless (typep feature-set 'view-private-feature-set) )
  (when inferiors
    (warn "initialize-instance :after feature-set: inferiors keyword is deprecated.~%")  
    (setf (children feature-set) inferiors))
  ;;(setq *foo* feature-set) (break)
  (let ((world (world feature-set)))
    (when world
      (when (stringp world)
	;; This is backward compatibility for old style feature-sets files.
	;;(setq world (world-name-to-world world feature-set))
	(flet ((world-name-to-world (world-name object)
		 #+never
		 (format t "INITIALIZE-INSTANCE :AFTER FEATURE-SET CALLED WORLD-NAME-TO-WORLD ~a~%" 
			 world-name)
		 (typecase object
		       (gl-3d-object-mixin (get-3d-world-named world-name))
		       (gl-2d-object-mixin (get-2d-world-named world-name))
		       (otherwise (error "Cannot find world for feature set")))))
	  (setq world (world-name-to-world world (first (children feature-set))))
	  (setf (world feature-set) world)))

      (add-feature-set feature-set world))

    (loop for inf in (inferiors feature-set)
	  ;;unless (world inf) do (setf (world inf) world)
	  unless (parent inf) do (setf (parent inf) world)
	  do (add-object inf feature-set)
	  )))


;;; FIXME:  Move this into image-defs.lisp or some such place.
;;; CAREFUL:  SETUP-IMAGE-WORLDS is called here.
;;; Inherit-properties is called by image-filter-decimate2.  It should probably
;;; be called by more image operators.
;;#+never
(progn

;;; ***********  INHERIT-PROPERTIES  ***********

;;; This is currently used only in this file, but the function inheritable-properties uses it
;;; and is called from other files.
(defparameter *non-inheritable-image-properties*
    '(:inferiors
      :indirected-to
      :time-of-last-access
      :time-of-creation
      :linear-geom-transform 
      :window-of
      :terrain-model
      :base-image
      :2d-to-image-transform
      :3d-to-2d-projection
      :3d-world ; inherit indirectly via 2d-world
      :pyramid
      :pathname ; This shouldn't propagate to other images.
      ))

;;; not sure this is used any more
;;; Yes, this is used many places.
(defparameter *inheritable-image-properties*
    '(:name
      :sun-vector              ; this should come from 2d-world
      :photometric-transform
      :image-to-2d-transform
      :2d-world))

(defmethod inheritable-properties ((image img::image))
  (loop for (ind val) on (property-list image) by #'cddr
	unless (member ind *non-inheritable-image-properties*)
	  collect ind and collect val))

(defmethod img::inherit-properties
	   ((image img::image) property-list-or-image
	    &key properties  
	    (not-properties *non-inheritable-image-properties*)
	    inherit-only-if-not-present
	    )
  ;;(setq *args* (list image property-list-or-image))
  (if t image
  (let* ((from-image (and (img::image-p property-list-or-image) property-list-or-image))
	 (property-list (if from-image
			    (property-list from-image)
			    property-list-or-image))
	 (2d-world (if from-image
		       (2d-world from-image)
		       (getf property-list :2d-world))))

    ;; problem here when running without window system
    ;; 2d-world is sometimes an old eval-cache form (a list)
    ;; Various thing depend on push-image doing some good things to the property-list of the image.
    (when (consp 2d-world)
      (setq 2d-world nil)
      (when from-image (setf (2d-world from-image) nil))
      )

    (when (and (memq :2d-world properties ) (fboundp 'add-image-to-world))
      (when (and from-image (not 2d-world))
	;; MUST FORCE from-image to have a 2d-world so that inheritance can work.
	;; Otherwise image will get its own 2d-world when it is pushed onto a view.
	(setup-image-worlds from-image)
	(setq 2d-world (2d-world from-image)))

      (add-image-to-world 2d-world image))
      
    (if properties
	(loop for (prop val) on property-list by #'cddr
	      when (and (memq prop properties)
			(or (not inherit-only-if-not-present)
			    (null (image-prop image prop))))
		do (setf (image-prop image prop) val))
	(loop for (prop val) on property-list by #'cddr
	      unless (or (memq prop not-properties)
			 (and inherit-only-if-not-present (image-prop image prop)))
		do (setf (image-prop image prop) val)))))
  ;;(break)
  image)

) ; end progn







;;; Changed Wed May 26 1993 to eliminate wired in coordinate systems and transforms
;;; No callers in the FREEDIUS directory tree.  Called from site 3d-world.lisp files.
(defmethod register-default-coordinate-systems
	   ((3d-world 3d-world) lvcs-to-geocentric-transform
	    &key
	    lat-long-to-geocentric-transform
	    (utm-lat-long-to-geocentric-transform lat-long-to-geocentric-transform )
	    utm-zone)
  (if (null lvcs-to-geocentric-transform)
      (register-geographic-transform 3d-world nil (coordinate-system 3d-world))
      
      (let* ((geocentric-to-lvcs-transform (inverse-transform lvcs-to-geocentric-transform))
	     (lat-long-to-lvcs-transform
	      (make-composite-coordinate-transform
	       (list lat-long-to-geocentric-transform geocentric-to-lvcs-transform)))
	     (lat-long-coordinate-system (from-coordinate-system lat-long-to-lvcs-transform))
	     (utm-lat-long-coordinate-system
	      ;; USGS 15 minute series DEMs use UTMs in NAD-27.
	      (from-coordinate-system utm-lat-long-to-geocentric-transform ) )
	     (utm-to-lvcs-coordinate-transform
	      (and utm-zone
		   (make-composite-coordinate-transform
		    (list* (inverse-transform
			    (make-lat-long-to-utm-transform
			     utm-lat-long-coordinate-system
			      (make-utm-coordinate-system (central-meridian-from-utm-zone utm-zone)
							  utm-lat-long-coordinate-system)))
			   ;; changed Wed May 26 1993
			   (if (eq utm-lat-long-coordinate-system lat-long-coordinate-system)
			       (list lat-long-to-lvcs-transform)
			       (list (make-transform-path
				      utm-lat-long-coordinate-system
				      lat-long-coordinate-system)
				     lat-long-to-lvcs-transform))))))
	     (lvcs-to-lat-long-transform (inverse-transform lat-long-to-lvcs-transform))
	     (lvcs-to-utm-transform (inverse-transform utm-to-lvcs-coordinate-transform))
	     )
	#+obsolete
	(let ((local-vertical-coordinate-system (from-coordinate-system lvcs-to-geocentric-transform)))
	  (setf (lvcs-to-lat-long-transform local-vertical-coordinate-system) lvcs-to-lat-long-transform
		(lvcs-to-utm-transform local-vertical-coordinate-system) lvcs-to-utm-transform))
    
	;; this next is now automatic when world is created, 
	(register-geographic-transform 3d-world nil (to-coordinate-system geocentric-to-lvcs-transform))
  
	(register-geographic-transform 3d-world lvcs-to-lat-long-transform)
	(when utm-zone (register-geographic-transform 3d-world lvcs-to-utm-transform))
       
	)))

