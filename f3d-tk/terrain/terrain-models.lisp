(IN-PACKAGE "GUI")

;;;
;;; Was in package CME, but why now in GUI?
;;;
;;;

(eval-when (eval load compile)
(import '(img::image-element-min-max transforms::linear-transform-p transforms::transform-matrix))
)

(defun dtm-bbox (dtm-image)
  (mv-bind (zmin zmax) (img::image-element-min-max dtm-image)
    (cv 0.0 (dfloat (image-x-dim dtm-image))
	0.0 (dfloat (image-y-dim dtm-image))
	(dfloat zmin) (dfloat zmax))))

;;; FIXME: This needs to be defined in a file not directly in the TRANSFORMS or IMAGE subsystems.
;;;(defmethod obj::cc-version-of-bounding-box ((image img::image) &optional ignore1 ignore2)
;;;  (declare (ignore ignore1 ignore2))
;;;  ;; FIXME:  Should 1 be subtracted here? 
;;;  ;; Is it a bounding-box of discrete point samples or a volume of Euclidean space?
;;;  (cv 0.0 (1- (img::image-x-dim image)) 0.0 (1- (img::image-y-dim image)) 0.0 1.0))

(defmethod bounding-box ((image img::image) &key bbox transform)
  (math::destructive-union-bounding-boxes
   bbox 
   (transform-vector transform
		     (cv 0.0 (img::image-x-dim image) 0.0 (img::image-y-dim image) 0.0 1.0))))


(defclass basic-terrain-model (fasd-form-property-list-mixin) ())


(defmethod terrain-model-z-min-max ((terrain-model basic-terrain-model))
  (values (get-prop terrain-model :min-z)
	  (get-prop terrain-model :max-z)))

;;; MISDESIGN -- THE SIGN OF N1 SHOULD BE FLIPPED so that the plane equation
;;; is homogeneous: <x y z 1> . plane.

;;; **************************  PLANAR-TERRAIN-MODEL  **************************

(defclass planar-terrain-model (basic-terrain-model)
    ((plane :initform (cv 0.0 0.0 1.0 0.0) :initarg :plane))
  )

(defmethod initialize-instance :after ((terrain-model planar-terrain-model) &rest initargs
				       &key nx ny nz n1)
  (ignore initargs)
  (with-slots (plane) terrain-model
    (when (and nx ny nz n1)
      (setf plane (cv (dfloat nx) (dfloat ny) (dfloat nz) (dfloat n1))))))


(defmethod intersect-camera-ray-with-terrain-model ((terrain-model planar-terrain-model)
						    projection 2d-position 
						    &optional approximate-intersection)
  (with-class-slots planar-terrain-model (plane) terrain-model
    (transforms::intersect-camera-ray-with-plane projection 2d-position plane
						 approximate-intersection)))

(defun make-planar-terrain-model (&key (plane-normal (cv 0.0 0.0 1.0)) n1 surface-pt z)
  (when (and z (not surface-pt))
    (setf surface-pt (cv 0.0 0.0 z)))
  (bind-vector-elements (nx ny nz) plane-normal
    (make-instance 'planar-terrain-model
;;		   :plane (cv nx ny nx (or n1 (math::vector-inner-product plane-normal surface-pt))))))
		   ;; The above is a typo, I think. --CC
		   :plane (cv nx ny nz (or n1 (- (math::vector-inner-product plane-normal surface-pt)))))))

(defvar *tmp-planar-terrain-model* nil)

(defun get-tmp-planar-terrain-model ()
  (or *tmp-planar-terrain-model*
      (setq *tmp-planar-terrain-model*
	    (make-instance 'planar-terrain-model))))

(defmethod set-planar-terrain-model ((terrain-model planar-terrain-model) nx0 ny0 nz0 x0 y0 z0)
  (with-class-slots planar-terrain-model (plane) terrain-model
    (setf plane (cv nx0 ny0 nz0 (math::inline-inner-prod (x0 y0 z0) (ny0 ny0 nz0))))))

(defmethod find-terrain-model ((terrain-model planar-terrain-model))
  terrain-model)

;;; *****************************  GRIDDED-TERRAIN-MODEL  *****************************
(defclass gridded-terrain-model (basic-terrain-model)
    ((dtm-image :initarg :dtm-image :accessor dtm-image)
     (dtm-to-lvcs-transform :initarg :dtm-to-lvcs-transform :accessor dtm-to-lvcs-transform)))

;;; *****************************  REGULAR-GRID-TERRAIN-MODEL  *****************************
(defclass regular-grid-terrain-model (gridded-terrain-model) ())

;;; **************************  NON-LINEAR-MAPPED-REGULAR-GRID-TERRAIN-MODEL  *************************
(defclass non-linear-mapped-regular-grid-terrain-model (gridded-terrain-model) ())


(defmethod intersect-camera-ray-with-terrain-model ((terrain-model gridded-terrain-model)
						    projection 2d-position 
						    &optional approximate-intersection)
  (transforms::intersect-camera-ray-with-gridded-terrain-model
   terrain-model projection 2d-position approximate-intersection))

#+never
(defmethod default-stare-point-z ((terrain-model gridded-terrain-model) )
  (get-prop terrain-model :default-stare-point-z))

#+never
(defmethod default-stare-point-w ((terrain-model gridded-terrain-model) )
  (get-prop terrain-model :default-stare-point-w))

(defun make-gridded-terrain-model (dtm-image dtm-to-lvcs-transform)
  (let* ((dtm-to-lvcs-transform (if (arrayp dtm-to-lvcs-transform ) 
				    (make-4x4-coordinate-transform dtm-to-lvcs-transform)
				    dtm-to-lvcs-transform))
	 (3d-world (to-coordinate-system dtm-to-lvcs-transform))
	 (terrain-model
	  (eval-cache (make-terrain-model dtm-image dtm-to-lvcs-transform)
	    (make-instance (if (linear-transform-p dtm-to-lvcs-transform) 
			       'regular-grid-terrain-model
			       'non-linear-mapped-regular-grid-terrain-model)
			   :dtm-image dtm-image
			   :dtm-to-lvcs-transform dtm-to-lvcs-transform)))
	 (dtm-bbox (dtm-bbox dtm-image))
	 (lvcs-bbox (transform-bounding-box dtm-to-lvcs-transform dtm-bbox)))
    (setf (get-prop terrain-model :max-z) (aref lvcs-bbox 5)
	  (get-prop terrain-model :min-z) (aref lvcs-bbox 4)
	  (get-prop terrain-model :terrain-bbox) lvcs-bbox
	  (get-prop 3d-world :terrain-bbox) lvcs-bbox)
    terrain-model))


(defmethod world-units-per-dtm-cell ((terrain-model gridded-terrain-model))
  (with-slots (dtm-to-lvcs-transform) terrain-model
    (let ((mat (transform-matrix dtm-to-lvcs-transform)))
      (euclidean-length (aref mat 0 0) (aref mat 0 1))  )))


(defmethod dtm-to-world-matrix ((terrain-model gridded-terrain-model))
  (with-slots (dtm-to-lvcs-transform) terrain-model
    (transform-matrix dtm-to-lvcs-transform)))

(defmethod dtm-surface-normal-at-point ((terrain-model gridded-terrain-model) 3dpt &optional normalize)
  (with-slots (dtm dtm-to-lvcs-transform) terrain-model
    (bind-vector-elements (dtm-x dtm-y) (inverse-transform-vector dtm-to-lvcs-transform 3dpt)
      (let* ((mat (transform-matrix dtm-to-lvcs-transform))
	     (z-scale (/ (aref mat 2 2)
			 (sqrt (abs (- (* (aref mat 0 0) (aref mat 1 1))
				       (* (aref mat 0 1) (aref mat 0 1))))))))
	(declare (type 4x4-matrix mat))
	(multiple-value-bind (lnx lny lnz)
	    (bicubic-surface-normal dtm dtm-x dtm-y nil z-scale)
	  (bind-vector-elements (wnx wny wnz)
	      (transform-direction-vector dtm-to-lvcs-transform (cv lnx lny (/ lnz z-scale)))
	    (if normalize
		(let ((l (euclidean-length wnx wny wnz)))
		  (cv (/ wnx l) (/ wny l) (/ wnz l)))
		(cv wnx wny wnz)))
	  )))))

(defmethod find-terrain-model ((terrain-model gridded-terrain-model))
  terrain-model)

(defmethod find-gridded-terrain-model ((terrain-model basic-terrain-model))
  nil)

(defmethod find-gridded-terrain-model ((view view))
  (find-gridded-terrain-model (3d-world view)))

(defmethod find-gridded-terrain-model ((world gl-3d-world))
  (let ((tm (get-prop world :terrain-model)))
    (when tm (find-gridded-terrain-model tm))))

(defmethod find-gridded-terrain-model ((terrain-model gridded-terrain-model))
  terrain-model)


;;; *************************  MULTI-TERRAIN-MODEL  *************************

(defclass multi-terrain-model (basic-terrain-model)
    ((terrain-model-list :initform nil :initarg :terrain-model-list :reader terrain-model-list)))

(defmethod find-gridded-terrain-model ((terrain-model multi-terrain-model))
  (loop for tm in (terrain-model-list terrain-model)
	thereis (find-gridded-terrain-model tm)))

(defun make-multi-terrain-model (terrain-model-list &key z-surround &allow-other-keys)
  (unless (listp terrain-model-list) (setq terrain-model-list (list terrain-model-list)))
  (when z-surround
    (setq terrain-model-list
	  (append terrain-model-list
		  (list (make-planar-terrain-model :surface-pt   
						   ;(cv z-surround 0.0 0.0)
						   (cv 0.0 0.0 z-surround)
						   )))))
  (make-instance 'multi-terrain-model
		 :terrain-model-list terrain-model-list))

(defmethod terrain-model-z-min-max ((terrain-model multi-terrain-model))
  (let ((min (get-prop terrain-model :min-z))
	(max (get-prop terrain-model :max-z)))
    (if (and min max)
	(values min max)
	(loop for tm in (terrain-model-list terrain-model)
	      do (multiple-value-setq (min max) (terrain-model-z-min-max tm))
	      when min
		minimize min into z-min
		and maximize max into z-max
	      finally (setf (get-prop terrain-model :min-z) z-min
			    (get-prop terrain-model :max-z) z-max)
		      (return (values z-min z-max))))))

	
(defmethod (setf terrain-model-list) (new-list (terrain-model multi-terrain-model))
  (with-slots (terrain-model-list) terrain-model
    (setf terrain-model-list new-list
	  (get-prop terrain-model :min-z) nil)
    (terrain-model-z-min-max terrain-model) ; force recomputation of min-z max-z
    new-list))

(defmethod initialize-instance :after ((terrain-model multi-terrain-model)
				       &key terrain-model-list &allow-other-keys)
  (when terrain-model-list
    (setf (terrain-model-list terrain-model) terrain-model-list)))
  
(defmethod dtm-image ((terrain-model multi-terrain-model))
  (dtm-image (first (terrain-model-list terrain-model))))

(defmethod dtm-to-lvcs-transform ((terrain-model multi-terrain-model))
  (dtm-to-lvcs-transform (first (terrain-model-list terrain-model))))

(defmethod find-terrain-model ((terrain-model multi-terrain-model))
  terrain-model)


(defmethod intersect-camera-ray-with-terrain-model ((terrain-model multi-terrain-model )
						    projection 2d-pt &optional approximate-intersection)
  (loop for tm in (terrain-model-list terrain-model)
	thereis (ignore-errors
		  (intersect-camera-ray-with-terrain-model tm projection 2d-pt
							   approximate-intersection))))




(defun bbox-corners (bbox)
  (bind-vector-elements (x0 x1 y0 y1 z0 z1) bbox
    (list (coordinate-vector x0 y0 z0)
	  (coordinate-vector x1 y0 z0)
	  (coordinate-vector x0 y1 z0)
	  (coordinate-vector x1 y1 z0)
	  (coordinate-vector x0 y0 z1)
	  (coordinate-vector x1 y0 z1)
	  (coordinate-vector x0 y1 z1)
	  (coordinate-vector x1 y1 z1))))

