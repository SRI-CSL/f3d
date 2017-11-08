(in-package :transforms)

;;; The PROJECTION-MATRIX the product of 
;;; CAMERA-TO-2D-MATRIX (interior orientation matrix) and
;;; 3D-TO-CAMERA-MATRIX (exterior orientation matrix).

;;; This projection can be interactively modified by the GUI:
;;; See frame-camera-motions.lisp

#|
The upper-left 3x3 submatrix of 3D-TO-CAMERA-MATRIX must be a right-handed
orthonormal matrix.  The 4th row of 3D-TO-CAMERA-MATRIX must be <0,0,0,1>

The structure of CAMERA-TO-2D-MATRIX is defined with the function
MAKE-CAMERA-TO-2D-MATRIX.

|#

;;; The wrong choice might have been made to store 3D-TO-CAMERA-MATRIX rather
;;; than CAMERA-TO-3D-MATRIX which can be easily decomposed into camera-position
;;; and orientation without the need for matrix-inversion.  OTOH, the
;;; upper-3x3-submatrix of 3d-to-camera-matrix is orthonormal, thus the
;;; matrix-inversion is actually a 3x3 transpose and a 3x3 matrix-times-vector
;;; calculation for the 3rd column of camera-to-3d-matrix.

(defstruct-class frame-camera (4x4-projection)
  ((3d-to-camera-matrix :initform nil :initarg :3d-to-camera-matrix
			:accessor 3d-to-camera-matrix)
   (camera-to-2d-matrix :initform nil :initarg :camera-to-2d-matrix
			:accessor camera-to-2d-matrix)
#|
   (1/f :initarg :1/f)
   (GSD :initarg :1/f)
   (ppu :initarg :ppu)
   (ppv :initarg :ppv)
   (vskew :initarg :vskew) 
   (vscale :initarg :vscale)
   (near :initarg :near)
   (1/far :initarg :1/far)
|#
   )
  (:default-initargs :create-inverse nil))

(defmethod initialize-instance :after ((projection frame-camera) &key &allow-other-keys)
  (unless *4x4-projection-opengl-depth-normalize* 
    (with-class-slots frame-camera (inverse-transform 3d-to-camera-matrix projection-matrix) projection
      (setf inverse-transform
	    (make-instance 'frame-camera-inverse
			   :projection-matrix (invert-matrix projection-matrix)
			   :camera-to-3d-matrix (invert-matrix 3d-to-camera-matrix)
			   :create-inverse nil))
      (setf (inverse-transform inverse-transform) projection)
      (standardize-4x4-projection-matrix projection)
      (set-interior-orientation-parameters  projection)
      (set-opengl-depth-unnormalization projection)
      )))

#+unneeded
(progn

(defmethod initialize-instance :after ((projection frame-camera) &key &allow-other-keys)
  (setf (4x4-projection-transform-function projection) 'frame-camera-transform-vector))

(defun frame-camera-transform-vector (projection from-vector to-vector)
  (declare (type (simple-array double-float (*))  from-vector))
  (declare (type (or null (simple-array double-float (*)))  to-vector))
  (bind-vector-elements (x y z) from-vector
    (let ((m (projection-matrix projection))
	  (p (camera-to-2d-matrix projection))
	  (us 1.0d0) (vs 1.0d0) (ws 1.0d0) (s 1.0d0) (1/s 1.0d0))
      (declare (double-float us vs ws s 1/s))
      (declare (type 4x4-matrix m p))
      (inline-matrix-times-vector m (x y z 1.0) (us vs ws s))
      (when (or *transform-dammit* (> s 0.0))
	(unless to-vector (setq to-vector (make-coordinate-vector 3)))
	(setf 1/s (/ 1.0 s))
	(setf (aref to-vector 0) (* us 1/s)
	      (aref to-vector 1) (* vs 1/s)
	      (aref to-vector 2) 
	      #+project-opengl-depth (* ws 1/s)
	      #-project-opengl-depth (/ (- ws (aref p 2 3)) (aref p 2 2))
	      ;;(aref to-vector 2) (* .5 (+ 1.0 (* ws 1/s))) ; OpenGL rescales -1:+1 => 0:+1
	      )
	to-vector))))

) ; end progn

#|
This decomposition into projection-matrix = camera-to-2d-matrix*3d-to-camera-matrix
is ok, but it might be easier to deal with the inverses of each matrix.

In particular, when GSD=0:

2d-to-camera-matrix = ((1 0 0 -ppu) (0 1 0 -ppv) (0 0 0 f) (0 0 x y))
    where x and y are related to OpenGL near/far parameters

  Thus, we can easily hack with ppu ppv and f in the 2d-to-camera-matrix.

On the other hand, 

camera-to-2d-matrix = ((1 0 ppu/f ppu*GSD)
                       (0 1 ppv/f ppv*GSD)
                       (0 0 alpha beta)
                       (0 0 1/f GSD))
    where alpha and beta are related to OpenGL near/far parameters,
isn't hard to deal with.

Similarly, in the camera-to-3d-matrix, the camera-position is the 4th column.
|#

#|
(undefmethod '(standardize-4x4-projection-matrix :before (frame-camera)))
(defmethod standardize-4x4-projection-matrix :before ((transform frame-camera))
  (with-class-slots frame-camera
	(projection-matrix 3d-to-camera-matrix camera-to-2d-matrix
			   projection-category) transform
    (setf projection-matrix
	  (multiply-matrices camera-to-2d-matrix 3d-to-camera-matrix))
    ;; No, WE ASSUME ROWS 3 AND 4 of CAMERA-TO-2D-MATRIX ARE APPROPRIATE
    ;; FOR OPENGL DEPTH-CLIPPING
    ;;(copy-matrix-rows projection-matrix projection-matrix '(2) '(3))
    ))
|#

(defmethod standardize-4x4-projection-matrix ((transform frame-camera))
  (with-class-slots frame-camera
	(projection-matrix 3d-to-camera-matrix camera-to-2d-matrix
			   inverse-transform) transform
    (multiply-matrices camera-to-2d-matrix 3d-to-camera-matrix projection-matrix)
    ;; No, WE ASSUME ROWS 3 AND 4 of CAMERA-TO-2D-MATRIX ARE APPROPRIATE
    ;; FOR OPENGL DEPTH-CLIPPING
    ;;(copy-matrix-rows projection-matrix projection-matrix '(2) '(3))
    (when inverse-transform
      (invert-matrix projection-matrix
		     (projection-matrix inverse-transform)))
    ))

(defmethod update-transform :before ((transform frame-camera))
  (standardize-4x4-projection-matrix transform)
  ;;(_get_projection_matrix transform)
  )

#| MAKE-FRAME-CAMERA-FROM-MATRICES is called from
MAKE-PERSPECTIVE-TRANSFORM in cme-compat/camera-models.lisp The 3rd row of
camera-to-2d-matrix has been constructed for compatibility with OpenGL
depth-clipping (and to make the matrix non-singular).  See
MAKE-CAMERA-TO-2D-MATRIX defined in basic-gui/gl-matrices.lisp.  The near and
far parameters need to be known for the 3rd row to be correct for OpenGL
depth-clipping and depth-buffering.

|#

(defun set-interior-orientation-parameters (frame-camera)
  (declare (ignore frame-camera)))

#+never
(defun set-interior-orientation-parameters (frame-camera)
  (with-class-slots frame-camera (1/f GSD ppu ppv vskew vscale near 1/far camera-to-2d-matrix) frame-camera
    (declare (double-float 1/f GSD ppu ppv vskew vscale near 1/far))
    (declare (type 4x4-matrix camera-to-2d-matrix))
    (let* ((r camera-to-2d-matrix))
      (declare (type 4x4-matrix r))
    (setf vskew  (aref r 0 1)
	  vscale (aref r 1 1)
	  1/f (aref r 3 2)
	  GSD (aref r 3 3)
	  ppu (if (= 1/f 0.0) 
		  (/ (aref r 0 3) GSD) 
		  (/ (aref r 0 2) 1/f))
	  ppv (if (= 1/f 0.0) 
		  (/ (aref r 1 3) GSD) 
		  (/ (aref r 1 2) 1/f))
	  near (/ (aref r 2 3) (aref r 2 2))
	  1/far (/ (- 1/f (aref r 2 2)) (- GSD (aref r 2 3)))))))
		  

(defun make-frame-camera-from-matrices
    (world-to-camera-matrix camera-to-2d-matrix &rest initargs &key near-far)
  ;;(break)
  (when near-far 
    (setq camera-to-2d-matrix (copy-matrix camera-to-2d-matrix))
    (destructuring-bind (&optional near far) near-far
      (set-camera-to-2d-matrix-depth-normalization camera-to-2d-matrix near far)))
  (apply 'make-instance 'frame-camera
	 :3d-to-camera-matrix world-to-camera-matrix
	 :camera-to-2d-matrix camera-to-2d-matrix
	 initargs))

(defmethod copy ((projection frame-camera))
  (make-frame-camera-from-matrices (3d-to-camera-matrix projection)
				   (camera-to-2d-matrix projection)))


;;; This is only used by SET-GL-3D-TO-2D-PROJECTION in gl-matrices.lisp
(defmethod interior-and-exterior-matrices ((projection frame-camera))
  (with-class-slots frame-camera (3d-to-camera-matrix camera-to-2d-matrix)
      projection      
    (values camera-to-2d-matrix 3d-to-camera-matrix)))


#|
(setq m (transforms::3d-to-camera-matrix (3d-to-2d-projection (top-view))))
(setq 3d-to-camera-matrix-copy (copy-matrix (transforms::3d-to-camera-matrix (3d-to-2d-projection (top-view)))))
(copy-matrix 3d-to-camera-matrix-copy (transforms::3d-to-camera-matrix (3d-to-2d-projection (top-view))))
(transforms::update-transform (3d-to-2d-projection (top-view)))

(invert-matrix m)
(origin (3d-to-2d-projection (top-view)))
(set-origin (3d-to-2d-projection (top-view)) (cv 1000.0 2000.0 10000.0))
(mv-bind (3d-to-camera-matrix  camera-to-2d-matrix)
    (decompose-projection-matrix projection-matrix)
  (freedius-io::io-form (transforms::make-frame-camera-from-matrices 3d-to-camera-matrix
								     camera-to-2d-matrix)))

|#


;;; This assumes that upper 3x3 of 3d-to-camera-matrix is orthonormal.
;;; origin is the camera position.
(defun 3d-to-camera-matrix-3d-origin (3d-to-camera-matrix)
  (declare (optimize (speed 3) (safety 1)))
  (let ((m 3d-to-camera-matrix)
	(origin (make-coordinate-vector 3)))
    (declare (type 4x4-matrix m))
    (declare (type (simple-array double-float (3)) origin))
    (loop for j fixnum from 0 below 3
	  do (setf (aref origin j)
		   (- (loop for i fixnum from 0 below 3
			    sum (* (aref m i j) (aref m i 3)) double-float))))
    origin))

(defmethod origin ((projection frame-camera))
  (declare (optimize (speed 3) (safety 1)))
  (with-class-slot-values frame-camera (3d-to-camera-matrix) projection
    (3d-to-camera-matrix-3d-origin 3d-to-camera-matrix)))


;;; This assumes that upper 3x3 of 3d-to-camera-matrix is orthonormal.
(defmethod set-origin ((projection frame-camera) new-origin)
  (declare (type (simple-array double-float (3)) new-origin))
  (with-class-slot-values frame-camera (3d-to-camera-matrix) projection
    (let ((m 3d-to-camera-matrix))
      (declare (type 4x4-matrix m))
      (loop for j fixnum from 0 below 3
	    do (setf (aref m j 3)
		     (- (loop for i fixnum from 0 below 3
			      sum (* (aref m j i) (aref new-origin i)) double-float))))
      ;; should the update be deferred?
      (update-transform projection)
      new-origin)))


;;; Even though the following refer to "image-point", they assume a
;;; point in the 2d-world, not the image itself.

;;; this works
(defmethod camera-direction-vector-at-image-point ((projection frame-camera) 2d-pos)
  (declare (optimize (speed 3) (safety 1)))
  (bind-vector-elements (u v) 2d-pos
    (let* ((cam2d (camera-to-2d-matrix projection))
	   (rot (3d-to-camera-matrix projection)))
      (declare (type dmatrix cam2d rot))
      
      (let* ((1/f (aref cam2d 3 2))
	     (up/f (aref cam2d 0 2))
	     (vp/f (aref cam2d 1 2))
	     (vscale (aref cam2d 1 1))
	     (vskew (aref cam2d 0 1))
	     (dyp (/ (- (* v 1/f) vp/f) vscale))
	     (dxp (- (* u 1/f) up/f (* vskew dyp)))
	     ;; (dzp 1.0)
	     ;; <dxp dyp dzp> is now cam2d-inv * <u/f v/f 1>
	     (dx (+ (* (aref rot 0 0) dxp) (* (aref rot 1 0) dyp) (aref rot 2 0)))
	     (dy (+ (* (aref rot 0 1) dxp) (* (aref rot 1 1) dyp) (aref rot 2 1)))
	     (dz (+ (* (aref rot 0 2) dxp) (* (aref rot 1 2) dyp) (aref rot 2 2))))
	;; <dx dy dz> is now rot-transpose * cam2d-inv * <u/f v/f 1>
	(declare (double-float 1/f up/f vp/f vscale dxp dyp))
	(declare (double-float dx dy dz))
	(normalize-vector-elements dx dy dz)
	(math::inline-coordinate-vector (- dx) (- dy) (- dz))))))


(defmethod camera-origin-and-direction-vector-at-image-point ((pmat array) 2d-pos)
  (mv-bind (3d2cam cam2d) (decompose-projection-matrix pmat)
    (camera-origin-and-direction-vector-at-image-point-int 3d2cam cam2d 2d-pos)))


(defmethod camera-origin-and-direction-vector-at-image-point ((projection frame-camera) 2d-pos)
  (camera-origin-and-direction-vector-at-image-point-int (3d-to-camera-matrix projection)
							 (camera-to-2d-matrix projection)
							 2d-pos))

(defmethod camera-origin-and-direction-vector-at-image-point-int (3d2cam cam2d 2d-pos)
  
  (declare (type dmatrix cam2d 3d2cam))
  (bind-vector-elements (u v) 2d-pos
    (let* ((1/f (aref cam2d 3 2))
	   (up/f (aref cam2d 0 2))
	   (vp/f (aref cam2d 1 2))
	   (vscale (aref cam2d 1 1))
	   (vskew (aref cam2d 0 1))
	   (dyp (/ (- (* v 1/f) vp/f) vscale))
	   (dxp (- (* u 1/f) up/f (* vskew dyp)))
	   ;; (dzp 1.0)
	   ;; <dxp dyp dzp> is now cam2d-inv * <u/f v/f 1>
	   (dx (+ (* (aref 3d2cam 0 0) dxp) (* (aref 3d2cam 1 0) dyp) (aref 3d2cam 2 0)))
	   (dy (+ (* (aref 3d2cam 0 1) dxp) (* (aref 3d2cam 1 1) dyp) (aref 3d2cam 2 1)))
	   (dz (+ (* (aref 3d2cam 0 2) dxp) (* (aref 3d2cam 1 2) dyp) (aref 3d2cam 2 2))))
      ;; <dx dy dz> is now rot-transpose * cam2d-inv * <u/f v/f 1>
      (declare (double-float 1/f up/f vp/f vscale dxp dyp))
      (declare (double-float dx dy dz))
      (normalize-vector-elements dx dy dz)
      (values (3d-to-camera-matrix-3d-origin 3d2cam) 
	      (math::inline-coordinate-vector (- dx) (- dy) (- dz))))))

(defmethod 1/f ((projection frame-camera))
  (with-class-slot-values frame-camera (camera-to-2d-matrix) projection
    (declare (type 4x4-matrix camera-to-2d-matrix))
    (aref camera-to-2d-matrix 3 2)))

(defmethod (setf 1/f) (new-1/f (projection frame-camera) &optional update)
  (with-class-slot-values frame-camera (camera-to-2d-matrix) projection
    (declare (type 4x4-matrix camera-to-2d-matrix))
    (setf (aref camera-to-2d-matrix 3 2) new-1/f)
    (when update (update-transform projection))
    new-1/f))

;;; FIXME -- POORLY NAMED -- THIS IS THE GSD FOR A POINT AT THE CAMERA ORIGIN
(defmethod GSD ((projection frame-camera))
  (with-class-slot-values frame-camera (camera-to-2d-matrix) projection
    (declare (type 4x4-matrix camera-to-2d-matrix))
    (- (aref camera-to-2d-matrix 3 3))))

(defmethod (setf GSD) (new-GSD (projection frame-camera) &optional update)
  (with-class-slot-values frame-camera (camera-to-2d-matrix) projection
    (declare (type 4x4-matrix camera-to-2d-matrix))
    (setf (aref camera-to-2d-matrix 3 3) (- new-GSD))
    (when update (update-transform projection) )
    new-GSD))

(defmethod (setf GSD) (new-GSD (projection frame-camera) &optional update)
  (with-class-slot-values frame-camera (camera-to-2d-matrix) projection
    (declare (type 4x4-matrix camera-to-2d-matrix))
    (setf (aref camera-to-2d-matrix 3 3) (- new-GSD))
    (when update (update-transform projection) )
    new-GSD))

(defmethod r/f ((projection frame-camera))
  (warn "The method r/f is deprecated: use GSD instead~%")
  (GSD projection))

(defmethod (setf r/f) (new-r/f (projection frame-camera) &optional update)
  (warn "The method (setf r/f) is deprecated: use (setf GSD) instead~%")
  (setf (gsd projection) new-r/f))

;;; pointing into camera
(defmethod principal-ray ((projection frame-camera))
  (declare (optimize (speed 3) (safety 1)))
  (with-class-slot-values frame-camera (3d-to-camera-matrix) projection
    (declare (type dmatrix 3d-to-camera-matrix))
    ;; assumes upper 3x3 submatrix of 3d-to-camera-matrix is orthonormal
    (cv (aref 3d-to-camera-matrix 2 0)
	(aref 3d-to-camera-matrix 2 1)
	(aref 3d-to-camera-matrix 2 2))))

(defmethod principal-point ((projection frame-camera))
  (with-class-slot-values frame-camera (camera-to-2d-matrix) projection
    (declare (type 4x4-matrix camera-to-2d-matrix))
    (let ((1/f (aref camera-to-2d-matrix 3 2))
	  (GSD (- (aref camera-to-2d-matrix 3 3))))
      (if (> (abs 1/f) (abs GSD))
	  (cv (/ (aref camera-to-2d-matrix 0 2) 1/f) (/ (aref camera-to-2d-matrix 1 2) 1/f))
	  (cv (/ (aref camera-to-2d-matrix 0 3) GSD) (/ (aref camera-to-2d-matrix 1 3) GSD))))))

(defmethod (setf principal-point) (new-principal-point (projection frame-camera) &optional update)
  (with-class-slot-values frame-camera (camera-to-2d-matrix) projection
    (declare (type 4x4-matrix camera-to-2d-matrix))
    (let ((1/f (aref camera-to-2d-matrix 3 2))
	  (GSD (- (aref camera-to-2d-matrix 3 3))))
      (bind-vector-elements (ppu ppv) new-principal-point
	(setf (aref camera-to-2d-matrix 0 2) (* ppu 1/f) (aref camera-to-2d-matrix 1 2) (* ppu GSD)
	      (aref camera-to-2d-matrix 1 2) (* ppv 1/f) (aref camera-to-2d-matrix 1 3) (* ppv GSD)))
      (when update (update-transform projection) )
      new-principal-point)))

;;;(pcl::undefmethod compute-gsd (frame-camera t))
;;; This is consistent with compute-gsd to about 6 digits
(defmethod compute-gsd2 ((projection frame-camera) point &optional normal)	      
  (let* ((1/f (1/f projection))
	 (v (vector-difference (origin projection) point)) ; vector from point to camera
	 (r (sqrt (math:vector-inner-product v v)))) ; range from point to camera
    (abs (if normal 
	     (/ (* 1/f r)
		(sqrt (abs (/ (math:vector-inner-product normal v) r))))
	     (* 1/f r)))))
	 
#|
(defun incr-ppu (proj delta)
  (let* ((mat (camera-to-2d-matrix proj))
	(1/f (aref mat 3 2)))
    (incf (aref mat 0 2) (* delta 1/f))
    (update-transform proj)))

(incr-ppu (3d-to-2d-projection (top-view)) 100)
(incr-ppu (3d-to-2d-projection (top-view)) -100)

(3d-to-2d-projection (top-view))
(transform-vector (3d-to-2d-projection (top-view))
		  (origin (gui::selected-object)))
(CV 940.281748238133 2580.670636271136 0.7213043833988215) 

(origin (gui::selected-object))

|#


#|

(defmethod principal-point-u ((projection frame-camera))
  (with-class-slot-values frame-camera (camera-to-2d-matrix) projection
    ))

|#


;;; STARE-AT-POINT - rotate, but now keep the camera orientation
;;; fixated relative to a specific center of rotation.  Although the
;;; fixation point may not be the center of rotation, the effect is
;;; that the scene rotates about that point.  Hence this might be a
;;; misnomer...

(defmethod stare-at-point ((transform frame-camera) orientation-spec center)
  (let* ((origin1 (origin transform))
	 (rot (rotation-spec-to-matrix orientation-spec))
	 (delta1 (cv 0.0 0.0 0.0 0.0))
	 (3dc (3d-to-camera-matrix transform)))
    (setf (aref 3dc 0 3) 0.0)
    (setf (aref 3dc 1 3) 0.0)
    (setf (aref 3dc 2 3) 0.0)
    (math::multiply-matrices 3dc rot 3dc)
    (vector-difference origin1 center delta1)
    (set-origin transform
		(vector-add
		 center
		 (matrix-times-vector (invert-matrix rot) delta1))))
  (update-transform transform))


(defmethod rotate-by ((transform frame-camera) orientation-spec
				  &optional center-of-rotation-vector)
  (rotate-by (3d-to-camera-matrix transform) orientation-spec center-of-rotation-vector))


(defmethod rotate-relative-to-world-by ((transform frame-camera) orientation-spec
				  &optional center-of-rotation-vector)
  (rotate-by (3d-to-camera-matrix transform) orientation-spec center-of-rotation-vector))




;;; ***********************   MAKE-CAMERA-TO-2D-MATRIX   ***********************
#| 

MAKE-CAMERA-TO-2D-MATRIX unifies PERSPECTIVE projection and ORTHOGRAPHIC
projection. See CME 4x4-projection class in $CMEHOME/transforms/4x4-projection.lisp.

far = NIL means far = infinity.

MAKE-CAMERA-TO-2D-MATRIX implements the near-far z-clipping calculations
described above and makes the third row compatible with OpenGL NDC Z clipping.

THE RESULTING MATRIX DOES NOT INCLUDE THE OpenGL 2D-TO-NDC TRANSFORMATION.

CAMERA-TO-2D-MATRIX has the following structure:

 1  r01  up*1/f   up*GSD
 0  r11  vp*1/f   vp*GSD
 0    0   alpha     beta
 0    0     1/f      GSD



Where:
    1/f      is the inverse focal-length, always negative or zero
    <up, vp> is the location of the principal point,

    GSD      is the ground sample distance AT the origin of the camera-to-world matrix
    r01      = sind(v-axis-skew),
    r11      = cosd(v-axis-skew) * v-axis-aspect-ratio

    alpha and beta are OpenGL depth range parameters computed from NEAR FAR

    Usually  GSD = 0 at the camera, r01 = 0.0 and r11 = 1.0.


|#


;;; FIXME:  Are the calls to abs really desired?

(defun make-camera-to-2d-matrix (&key 
				 (principal-point '(0.0 0.0))
				 (gsd 0.0) focal-length
				 aspect-ratio ; scale of v-axis relative to u-axis
				 skew ; rotation angle of v-axis from perpendicular to u-axis.
				 near-far
				 &allow-other-keys)
  ;;(break)
  (let ((ppu (elt principal-point 0))
	(ppv (elt principal-point 1))
        (gsd gsd)
	(focal-length (if (> (length principal-point) 2) 
                          (elt principal-point 2)
                          focal-length)))
    (unless (or focal-length gsd)
      (error "Must specify either 1/f or gsd."))
    (let* ((1/f (if focal-length (/ focal-length) 0.0))
	   (skew (or skew 0.0))
	   (aspect-ratio (or aspect-ratio 1.0))
	   ;; mat is the camera-to-2d-matrix
	   (camera-to-2d-matrix
	    (let ((R32 (- (abs 1/f)))	; R32 is always negative
		  (R33 (abs gsd))	; R33 is always positive
		  (R11 aspect-ratio)
		  (R01 0.0)
		  (R22 1.0)
		  (R23 0.0))
	      (declare (type double-float skew R32 R33 R11 R01 R22 R23))
	      (unless (zerop skew)
		;; This corrects a bug in the CME-6 version
		(setq R11 (* aspect-ratio (cos (radians skew)))
		      R01 (sin (radians skew))))
	      (make-and-fill-4x4-matrix
	       1.0 R01 (* R32 ppu) (* R33 ppu)
	       0.0 R11 (* R32 ppv) (* R33 ppv)
	       0.0 0.0 R22         R23
	       0.0 0.0 R32         R33))))
      (when near-far
	(destructuring-bind (&optional near far) near-far
	  (set-camera-to-2d-matrix-depth-normalization camera-to-2d-matrix near far)))
      camera-to-2d-matrix)))




(defun math::interpolate-camera-to-2d-matrices (mat0 mat1 alpha )
  (labels ((linear-interpolate (x y)
	     (+ (* (- 1.0 alpha) x)  (* alpha y)))
	   (log-interpolate (x y)
	     (exp (linear-interpolate (log x) (log y)))))
    (let* ((params0 (transforms::decompose-camera-to-2d-matrix mat0))
	   (params1 (transforms::decompose-camera-to-2d-matrix mat1))
	   (fl0 (abs (third (getf params0 :principal-point))))
	   (fl1 (abs (third (getf params1 :principal-point))))
	   (fl (- (log-interpolate fl0 fl1)))
	   (near (linear-interpolate (getf params0 :near) (getf params1 :near)))
	   (far0 (getf params0 :far)) (far1 (getf params1 :far))
	   (far (and far0 far1 (linear-interpolate far0 far1)))
	   ;;(far (* 3.0 near)) ;; FIXME
	   )
      (destructuring-bind (ppu ppv fl0) (getf params0 :principal-point)
	(declare (ignore fl0))
	;;(setq *foo* (list mat0  params0 params1 fl near far))      
	(transforms::make-camera-to-2d-matrix :principal-point (cv ppu ppv fl)
					      :near-far (list near far))))))

(defun math::interpolate-camera-to-2d-matrices (mat0 mat1 alpha )
  (labels ((linear-interpolate (x y)
	     (+ (* (- 1.0 alpha) x)  (* alpha y)))
	   (log-interpolate (x y)
	     (exp (linear-interpolate (log x) (log y)))))
    (let* ((params0 (transforms::decompose-camera-to-2d-matrix mat0))
	   (params1 (transforms::decompose-camera-to-2d-matrix mat1))
	   (fl0 (abs (third (getf params0 :principal-point))))
	   (fl1 (abs (third (getf params1 :principal-point))))
	   (fl (- (log-interpolate fl0 fl1)))
	   (near (linear-interpolate (getf params0 :near) (getf params1 :near)))
	   (far0 (getf params0 :far)) (far1 (getf params1 :far))
	   (far (and far0 far1 (linear-interpolate far0 far1)))
	   (aspect-ratio (linear-interpolate (or (getf params0 :aspect-ratio) 1.0)
					     (or (getf params1 :aspect-ratio) 1.0)))
	   ;;(far (* 3.0 near)) ;; FIXME
	   )
      (destructuring-bind (ppu ppv fl0) (getf params0 :principal-point)
	(declare (ignore fl0))
	;;(setq *foo* (list mat0  params0 params1 fl near far))      
	(transforms::make-camera-to-2d-matrix :principal-point (cv ppu ppv fl)
					      :aspect-ratio aspect-ratio
					      :near-far (list near far))))))



(defstruct-class frame-camera-inverse (4x4-projection)
  ((camera-to-3d-matrix :initarg :camera-to-3d-matrix)
   ))

(defmethod initialize-instance :after ((projection frame-camera-inverse) &key &allow-other-keys)
  (with-class-slots frame-camera-inverse (transform-function) projection
    (setf transform-function 'frame-camera-inverse-transform-vector)))

#|
;;; FIXME -- broken when 1/f=0
#+never
(defun frame-camera-inverse-transform-vector (projection from-vector to-vector)
  (declare (type coordinate-vector from-vector))
  (declare (type (or null coordinate-vector)  to-vector))
  (declare (optimize (speed 3) (safety 2)))
  #+cme (declare (ext:optimize-interface (speed 3) (safety 1)))
  (if *4x4-projection-opengl-depth-normalize*
      (4x4-projection-transform-vector projection from-vector to-vector)
      (with-class-slots frame-camera (1/f GSD ppu ppv vskew vscale near far) 
	  (inverse-transform projection)
	(declare (double-float 1/f GSD ppu ppv vskew vscale near far))
      (bind-vector-elements (u v w) from-vector
	(let* ((m (frame-camera-inverse-camera-to-3d-matrix projection))
	       (wp  (- (+ w near)))
	       (vp (/ (- v ppv) vscale))
	       (up (- u ppu (* vskew vp)))
	       (s (- (* wp 1/f) GSD)) 
	       (xp (* s up))
	       (yp (* s vp))
	       (to-vector (or to-vector (make-coordinate-vector 3)))
	       )
	  (declare (type 4x4-matrix m))
	  (declare (type coordinate-vector  to-vector))
	  (declare (double-float wp yp up s xp yp)
	  (inline-matrix-times-vector m (xp yp zp 1.0)
				      ((aref to-vector 0) (aref to-vector 1) (aref to-vector 2)))
	  ;(break)
	  to-vector))))
|#

;;; When *4x4-projection-opengl-depth-normalize* = NIL,  FRAME-CAMERA-INVERSE-TRANSFORM-VECTOR 
;;; returns a linear depth component rather than a perspective scaled depth for OpenGL
;;; depth clipping (and z-buffer calculations).  This may not be as useful as I previously
;;; thought.

;(disassemble 'frame-camera-inverse-transform-vector)
(defun frame-camera-inverse-transform-vector (projection from-vector to-vector)
  (declare (type coordinate-vector from-vector))
  (declare (type (or null coordinate-vector)  to-vector))
  (declare (optimize (speed 3) (safety 1)))
  #+cme (declare (ext:optimize-interface (speed 3) (safety 1)))
  (if *4x4-projection-opengl-depth-normalize*
      (4x4-projection-transform-vector projection from-vector to-vector)
      (bind-vector-elements (u v wp) from-vector
	(let* ((r (frame-camera-camera-to-2d-matrix (inverse-transform projection)))
	       (m (frame-camera-inverse-camera-to-3d-matrix projection))
	       (near (/ (aref r 2 3) (aref r 2 2)))
	       (zp  (- (+ wp near)))
	       (1/f (aref r 3 2))
	       (vp (/ (- v (/ (aref r 1 2) 1/f))
		      (aref r 1 1)))
	       (up (- u (/ (aref r 0 2) 1/f) (* (aref r 0 1) vp)))
	       (s (- (* zp 1/f) (aref r 3 3)) )
	       (xp (* s up))
	       (yp (* s vp))
	       (to-vector (or to-vector (make-coordinate-vector 3)))
	       )
	  (declare (type 4x4-matrix r m))
	  (declare (type coordinate-vector  to-vector))
	  (declare (double-float near wp 1/f yp up s xp yp))
	  (inline-matrix-times-vector m (xp yp zp 1.0)
				      ((aref to-vector 0) (aref to-vector 1) (aref to-vector 2)))
	  ;(break)
	  to-vector))))

#|
(let* ((proj (gui::3d-to-2d-projection (gui::top-view)))
       (pt (gui::selected-object-world-position))
       (2d-pt (transform-vector proj pt)))
  (list (inverse-transform-vector proj 2d-pt)
	pt
	2d-pt ))
  


|#

;;; *************************   MAKE-FRAME-CAMERA  *************************

;;;(defun make-frame-camera (&rest keyvals &key 
;;;                          parent name 2d-world
;;;                          (origin '(0.0 0.0 0.0))
;;;                          omega-phi-kappa heading-pitch-roll (azimuth-elevation-twist heading-pitch-roll)
;;;                          Euler-angles ; a list (Euler-angle-system angle1 angle2 angle3)
;;;                          principal-point focal-length
;;;                          (skew 0.0) (aspect-ratio 1.0)
;;;                          near-far)
;;;  ;; These args are passed on to MAKE-CAMERA-TO-2D-MATRIX
;;;  (declare (ignore principal-point focal-length skew aspect-ratio)) 
;;;  (destructuring-bind (&optional Euler-angle-system . Euler-angles) Euler-angles
;;;    (unless (and Euler-angle-system Euler-angles)
;;;      (setq Euler-angle-system
;;;            (cond (omega-phi-kappa (setq Euler-angles omega-phi-kappa) :omega-phi-kappa)
;;;                  (azimuth-elevation-twist (setq Euler-angles omega-phi-kappa) :azimuth-elevation-twist))))
;;;    (let* ((camera-to-2d-matrix (apply 'transforms::make-camera-to-2d-matrix keyvals))
;;;           (camera-to-3d-matrix 
;;;            (math::make-object-to-parent-matrix origin Euler-angle-system Euler-angles))
;;;           (frame-camera-projection (make-instance 'transforms::frame-camera
;;;                                                   :3d-to-camera-matrix 
;;;                                                   (invert-matrix camera-to-3d-matrix)
;;;                                                   :camera-to-2d-matrix camera-to-2d-matrix
;;;                                                   :from-coordinate-system parent))
;;;           (2d-world (or 2d-world (cme::get-or-make-2d-world :name name :3d-world parent))))
;;;      ;; (CME::CHANGE-PROJECTION 2d-world frame-camera-projection) also does this next:
;;;      ;; and is called by LOAD-CAMERA-MODEL2 
;;;      (set-transform-coordinate-systems frame-camera-projection parent 2d-world)
;;;      (when 2d-world
;;;        (setf (object-to-parent-transform 2d-world) (inverse-transform frame-camera-projection)))
;;;      (when near-far
;;;        (setf (get-prop frame-camera-projection :near-far) near-far))
;;;      ;;frame-camera-projection
;;;      2d-world)))

(defun make-frame-camera (&rest keyvals &key 
			  parent name 2d-world
			  (origin '(0.0 0.0 0.0))
			  omega-phi-kappa heading-pitch-roll (azimuth-elevation-twist heading-pitch-roll)
			  Euler-angles ; a list (Euler-angle-system angle1 angle2 angle3)
			  principal-point focal-length
			  (skew 0.0) (aspect-ratio 1.0)
			  near-far)
  ;; These args are passed on to MAKE-CAMERA-TO-2D-MATRIX
  (declare (ignore name principal-point focal-length skew aspect-ratio)) 
  (destructuring-bind (&optional Euler-angle-system . Euler-angles) Euler-angles
    (unless (and Euler-angle-system Euler-angles)
      (setq Euler-angle-system
	    (cond (omega-phi-kappa (setq Euler-angles omega-phi-kappa) :omega-phi-kappa)
		  (azimuth-elevation-twist (setq Euler-angles omega-phi-kappa) :azimuth-elevation-twist))))
    (let* ((camera-to-2d-matrix (apply 'transforms::make-camera-to-2d-matrix keyvals))
	   (camera-to-3d-matrix 
	    (math::make-object-to-parent-matrix origin Euler-angle-system Euler-angles))
	   (frame-camera-projection (make-instance 'transforms::frame-camera
						   :3d-to-camera-matrix 
						   (invert-matrix camera-to-3d-matrix)
						   :camera-to-2d-matrix camera-to-2d-matrix
						   :from-coordinate-system parent)))
      (set-transform-coordinate-systems frame-camera-projection parent 2d-world)
      (when near-far
	(setf (get-prop frame-camera-projection :near-far) near-far))
      frame-camera-projection)))
 
(defmethod set-near-far ((projection frame-camera) near-far)
  (if near-far
      (setf (get-prop projection :near-far) near-far)
      (rem-prop projection :near-far))
  (when near-far
    (destructuring-bind (&optional near far) near-far
      (set-camera-to-2d-matrix-depth-normalization (camera-to-2d-matrix projection) near far))
    (update-transform projection)))



#| SOLVING FOR THE OPENGL Z-CLIPPING PARAMETERS

What file does this belong in?

OPENGL NOTE: FREEDIUS generates row 2 of a 4x4-projection matrices so that
z''/w'' of NEAR points maps to 0 and FAR points to +1.  The OpenGL functions
gluPerspective and gluOrtho2D compute rows 2 and 3 of the projection matrix
using the NEAR/FAR parameters such that z''/w'' is in the range -1:+1.  OpenGL
internally remaps this range to 0:+1.  To achieve compatibility between FREEDIUS
4x4-projection matrices and OpenGL projection matrices, SET-2D-TO-NDC-MATRIX
maps z from the range 0:+1 to the range -1:+1.

Must compute values for P22 P23 of the projection matrix such that vectors at
range NEAR from the camera have z''/w'' = 0 and vectors at range FAR from the
camera have z''/w'' = 1.

<x',y',z'> = M <x,y,z,1>

   where M is the exterior orientation matrix,
   <x,y,z,1> is a point in world coordinates.

<x'',y'',z'',w''> = P <x', y', z', 1>,

   where P is the projection matrix, which has the structure:

    1 0 P02 P03
    0 1 P12 P13
    0 0 P22 P23
    0 0 P32 P33

FIXME:  There appear to be sign errors wrt the near and far parameters in the following.
        I believe this derivation assumes the near far parameters are z' relative to the camera,
        and thus NEGATIVE.  In the code below, they are POSITIVE.
   
z''/w'' = (P22*z' + P23) / (P32*z' + P33),

   where P32 = 1/f, and P33 = GSD.

When z'=NEAR, must have z''/w'' = 0

   (P22*NEAR + P23) / (P32*NEAR + P33) = 0

or P22*NEAR + P23 = 0                                                        (eqn 1)

When z'=FAR,  must have z''/w'' = 1
 
   (P22*FAR + P23) / (P32*FAR + P33) = 1

or P22*FAR + P23 = P32*FAR + P33                                             (eqn 2)

Subtracting (eqn 1) from (eqn 2)

   P22*(FAR-NEAR) = P32*FAR + P33                                            (eqn 3)

Finally:

   P22 = (P32*FAR + P33) / (FAR-NEAR)                                        (eqn 4)

We can eliminate the singularity in (eqn 4) when FAR becomes infinite
by dividing both numerator and denominator by FAR resulting in:

   P22 = (P32 + P33*(1/FAR)) / (1 - NEAR*(1/FAR))

   P23 = - P22*NEAR

Similarly, we can derive NEAR and FAR from a given interior-orientation matrix:

   NEAR = - P23/P22

   1/FAR = (P32 - P22) / (P23 - P33)

In order for P to be non-singular, P22*P33 - P23*P32 must be non-zero.

Since P23 = -near*P22, we can substitute and divide by P22, getting

             GSD + near * 1/f /=0.             (as noted above, the sign of near is in question).

|#


(defparameter *disable-near-far-clipping* nil)
(defparameter *disable-far-clipping* nil)


;;; Rename to set-OpenGL-near-far
(defun set-camera-to-2d-matrix-depth-normalization (camera-to-2d-matrix near far)
  (when near
    (let* ((r camera-to-2d-matrix)
	   (1/f (aref r 3 2))
	   (gsd (aref r 3 3))
	   (R22 (cond ((or (null far) *disable-far-clipping*)
		       1/f)
		      (t (when (or (not near) *disable-near-far-clipping*)
			   (setq near (/ 1.0 1/f))) ; FORCE R23 = -1.0
			 (/ (- (* 1/f far) gsd) (- far near))))))

      (setf (aref r 2 2) R22
	    (aref r 2 3) (* R22 near))     ; FIXME:  near assumed to be positive 
      ;(break)
      camera-to-2d-matrix)))


(defun 2d-matrix-depth-normalization-params (1/f gsd near far)
  (when near
    (let* ((R22 (cond ((or (null far) *disable-far-clipping*)
		       1/f)
		      (t (when (or (not near) *disable-near-far-clipping*)
			   (setq near (/ 1.0 1/f))) ; FORCE R23 = -1.0
			 (/ (- (* 1/f far) gsd) (- far near))))))

      (values R22 (* R22 near)))))

;(2d-matrix-depth-normalization-params (/ -500.0) 0.0 10.0 1000.0)

(defparameter *extract-near-far-infinity-reciprocal* 1e-7)

;;; Rename to extract-OpenGL-near-far
(defmethod extract-near-far ((camera-to-2d-matrix array)) 
  (declare (optimize (speed 3)(safety 1)))
  (declare (type 4x4-matrix camera-to-2d-matrix))
  (setq *foo* (list camera-to-2d-matrix))
  (let* ((r camera-to-2d-matrix)
	 (r22 (aref r 2 2))
	 (r23 (aref r 2 3))
	 (r32 (aref r 3 2))
	 (r33 (aref r 3 3))
	 (near (if (= r22 0.0) 0.0 (/ r23 r22))) ;; bogus
	 )
    (if (= r33 r23)
	(values near nil)
	(let ((1/far (/ (- r32 r22) (-  r33 r23))))
	  (values near (if (< (abs 1/far) *extract-near-far-infinity-reciprocal*) nil (/ 1/far)))))))

(defmethod extract-near-far ((projection transforms::frame-camera))
  (extract-near-far (transforms::camera-to-2d-matrix projection)))

(defmethod extract-near-far ((projection transforms::4x4-projection))
  (mv-bind (3d-to-camera-matrix  camera-to-2d-matrix)
      (decompose-projection-matrix (transforms::projection-matrix projection))
    (extract-near-far camera-to-2d-matrix)))




#+unfinished
(defmethod guess-OpenGL-near-far ((projection frame-camera))
  (let* ((3d-world (from-coordinate-system projection))
	 (bbox (and 3d-world (or (site-bbox 3d-world) (terrain-bbox 3d-world)))))
    (bind-vector-elements (xmin xmax ymin ymax zmin zmax) bbox
      (let* ((camera-to-3d-matrix (invert-matrix (3d-to-camera-matrix projection)))
	     (zcam (aref camera-to-3d-matrix 2 3))
	    )
	(when (and (> zcam zmax) ; should we require that camera be "well above" the terrain?
		   
		   
	))))))

  
#|
;(extract-near-far (3d-to-2d-projection (top-view)))
;(invert-matrix ( 3d-to-camera-matrix (3d-to-2d-projection (top-view))))

(let ((projection (3d-to-2d-projection (top-view))))
  (set-camera-to-2d-matrix-depth-normalization (camera-to-2d-matrix projection) 4000.0 nil)
  (standardize-4x4-projection-matrix projection))

(let ((projection (3d-to-2d-projection (top-view))))
  (set-camera-to-2d-matrix-depth-normalization (camera-to-2d-matrix projection) 1000.0 nil)
  (standardize-4x4-projection-matrix projection))


(MAKE-PERSPECTIVE-TRANSFORM
	       :focal-length -500.0 :near 10.0 :far 1000.0
	       :principal-point-u 191.0
	       :principal-point-v 113.0
	       :transform-matrix
	       (make-and-fill-2d-array
		'((-0.9902294 0.015442824 -0.13858992 -57.242573)
		  (-0.13671796 0.088168204 0.9866786 4954.4883)
		  (0.027456328 0.99598587 -0.08519544 6054.9927)
		  (0.0 0.0 0.0 1.0)))
	       ;;:property-list (list :reference-frame *alv-reference-frame*)
	       )

;(transform-vector (3d-to-2d-projection (gui::top-view)) (gui::selected-object-world-position))
(setf (coordinate-transform-transform-function (3d-to-2d-projection (gui::top-view)))
      'frame-camera-transform-vector)
|#
