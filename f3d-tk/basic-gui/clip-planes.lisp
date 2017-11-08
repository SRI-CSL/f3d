(in-package :gui)

;;; Contains direct dependencies on OpenGL.

#|
clip plane experiments -- as per suggestions from Lee Iverson

Let p be a clip plane defined in NDC, such that a point U is on the correct side
   of the clip plane if the dot product  P . U >= 0.

Let X be a point in 3d space.

Let M be the 3d-to-NDC projection matrix.

    U = M X

P . M X >= 0 is the clipping test.
|#

;;;(defun transform-clip-plane (p-camera projection-matrix)
;;;  (let* ((p-world (math::vector-times-matrix p-camera projection-matrix))
;;;         (norm (aref p-world 3)))
;;;    (math::vector-times-scalar p-world (/ norm))))

(defun transform-clip-plane (p-camera projection-matrix)
  (let* ((p-world (math::vector-times-matrix p-camera projection-matrix))
	 (norm (euclidean-length (aref p-world 0) (aref p-world 1) (aref p-world 2))))
    (math::vector-times-scalar p-world (/ norm))))

(defun 3d-to-ndc-projection (view)
  (gl::with-gl-window ((view-window view))
    ;;(glMakeCurrent (view-window view))
    (set-3d-matrices view)
    (let ((2d-to-ndc-matrix (math::multiply-matrices
			     (window-to-ndc-matrix)
			     (2d-to-window-matrix view))))
      (math::multiply-matrices
       2d-to-ndc-matrix
       (transforms::projection-matrix (3d-to-2d-projection view)))
      )))

(defun 3d-clip-planes (view)
  (loop with 3d-to-ndc-projection = (3d-to-ndc-projection view)
	for 2d-clip-plane in (list (cv 1.0 0.0 0.0 1.0)
				   (cv -1.0 0.0 0.0 1.0)
				   (cv 0.0 1.0 0.0 1.0)
				   (cv 0.0 -1.0 0.0 1.0)
				   (cv 0.0 0.0 1.0 0.0)
				   (cv 0.0 0.0 -1.0 1.0)
				   )
	collect (transform-clip-plane 2d-clip-plane 3d-to-ndc-projection)))

(defun 4-vector (v)
  (bind-vector-elements (x y z) v (cv x y z 1.0)))

(defmethod (clip-planes center radius)
  (unless (>= (length center) 4)
    (setq center (4-vector center)))
  (loop for clip-plane in clip-planes
	minimize (+ (vector-inner-product clip-plane center) radius)))

(defmethod bounding-sphere-test (clip-planes center radius)
  (unless (>= (length center) 4)
    (setq center (4-vector center)))
  (loop for clip-plane in clip-planes
	always (>= (+ (vector-inner-product clip-plane center) radius) 0.0)))

(defmethod compute-bounding-sphere ((object gl-object))
  (loop with verts of-type (simple-array double-float (* *)) = (vertex-array object)
	with n fixnum = (array-dimension verts 0)
	with xmin double-float = most-positive-double-float
	with xmax double-float = most-negative-double-float
	with ymin double-float = most-positive-double-float
	with ymax double-float = most-negative-double-float
	with zmin double-float = most-positive-double-float
	with zmax double-float = most-negative-double-float
	for i fixnum from 0 below n
	for x double-float = (aref verts i 0)
	for y double-float = (aref verts i 1)
	for z double-float = (aref verts i 2)
	do (when (> x xmax) (setq xmax x))
	   (when (< x xmin) (setq xmin x))
	   (when (> y ymax) (setq ymax y))
	   (when (< y ymin) (setq ymin y))
	   (when (> z zmax) (setq zmax z))
	   (when (< z zmin) (setq zmin z))
	finally (return (values (transform-vector (object-to-world-transform object)
						  (cv (* .5 (+ xmin xmax))
						      (* .5 (+ ymin ymax))
						      (* .5 (+ zmin zmax))))
				(* .5 (euclidean-length (- xmax xmin)
							(- ymax ymin)
							(- zmax zmin)))))))
		  
#|
(compute-bounding-sphere (selected-object))
(3d-clip-planes (top-view))
(multiple-value-bind (center radius)
    (compute-bounding-sphere (selected-object))
  (bounding-sphere-test (3d-clip-planes (top-view))
			center radius))

(4x4-project-vector (3d-to-ndc-projection (top-view)) (origin (selected-object)))
(let ((view (top-view)))
  (glMakeCurrent (view-window view))
  (set-3d-matrices view)
  (glProject_to_window (origin (selected-object))))
(top-view)


(3d-clip-plane)
(3d-clip-plane (cv -1.0 0.0 0.0 1.0))
(origin (selected-object))
(selected-object)
(vector-inner-product (3d-clip-plane) (4-vector (origin (selected-object))))
(vector-inner-product (3d-clip-plane (cv -1.0 0.0 0.0 1.0)) (4-vector (origin (selected-object))))
(top-view)
(transforms::projection-matrix (3d-to-2d-projection (top-view)))
(3d-to-2d-projection (top-view))
(math::invert-matrix (cadr (3d-to-2d-projection (top-view))))
|#
