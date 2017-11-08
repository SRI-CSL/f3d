(in-package :transforms)

#| FIXME: This commentary should be in a file common to all coordinate-projections.

Generic Properties of Coordinate Projections:

<u,v,w> = P<x,y,z>

P is invertable:

<x,y,z> = Pinv<u,v,w>

(P and Pinv have mathematical properties such as differentiability, non-zero Jacobian
matrix everywhere "important", ...)

<u,v> are sensor coordinates.

W imposes a depth ordering on points <x,y,z> relative to the sensor.  
  For fixed <u,v>, the distance from the sensor to a point <x,y,z> = Pinv<u,v,w>
  monotonically increases with w (-w if the <u,v,w> coordinate-system is
  right-handed).

Option 0:  w is only required to be a differentiable, monotonic depth ordering.
  Any choice for the w computation must satisfy this requirement.  A 360 degree
  (in azimuth) panoramic camera could define w as the Euclidean distance from 
  optical center of the camera.

Option 1: (CME does this; FREEDIUS optionally does this):
  W is the (signed) distance of point <x,y,z> to the "w-plane".

  w = <x,y,z,1> . <wx,wy,wz,w1>.

  If wx,wy,wz> is a unit vector, then w units are the same as 3d-world units.

  The w-axis (the normal to the w-plane) must be chosen so that determinant of
  the Jacobian matrix of P is non-zero everywhere.  This is best accomplished by
  making the w-axis be nearly orthogonal to both the u-axis and the v-axis.
  (The rows of inverse of the Jacobian matrix of P at a point X can be thought
  of as projection of the u, v and w axes into 3d-world coordinates at point X.)

  For a frame camera, <wx,wy,wz> is the first 3 elements of the 3rd row of the
  3d-to-camera matrix.

  For other sensors, w-plane defaults to  <0,0,1,0>, the LVCS xy-plane with z=0. 
  Hence w=z.
  
Option 2: (FREEDIUS optionally does this. Problems - See notes below): 
  W is a first order rational polynomial:

  w = <x,y,z,1> . <wnx,wny,wnz,wn1> / <x,y,z,1> . <wdx,wdy,wdz,wd1>

  where either <wnx,wny,wnz> = alpha * <wdx,wdy,wdz> and ||wdx,wdy,wdz|| > 0
            or <wdx,wdy,wdz> = alpha * <wnx,wny,wnz> and ||wnx,wny,wnz|| > 0

  Option 2 is appropriate for implementing the OPENGL depth ordering in which
  points at distance NEAR from the w-plane have w=0, and points at distance FAR
  from the w-plane have w=1.  See xxx for details about the calculation of 
  the polynomial coefficients.

  If Option 2 is chosen, there should be support for mapping w to z.
  In the OpenGL case, w = (c22*z'+c23)/(c32*z'+c33), where
  z' = <m20,m21,m22,m23><x,y,z,1>

Note about OPENGL depth normalization:  

  I ran into problems with numeric-inverse-projections with OpenGL depth
normalization.  The problem is that the normalized w component is highly
non-linear near 1.0 and thus the assumption of local linearity of the
transformation is violated.  Newton-Raphson iteration diverges and
iterative-inverse-transform has problems.


METHODS OF ALL COORDINATE PROJECTIONS:

(TRANSFORM-VECTOR projection xyz-pt &optional uvw-pt)
      <u,v,w> = P<x,y,z>

(INVERSE-TRANSFORM-VECTOR projection uvw-pt &optional xyz-pt)
  = (TRANSFORM-VECTOR (INVERSE-TRANSFORM projection) uvw-pt &optional xyz-pt)
      <x,y,z> = Pinv<u,v,w>

(TRANSFORM-JACOBIAN projection xyz-pt &optional 3x3-matrix)

(INTERSECT-CAMERA-RAY-WITH-PLANE projection uv-pt plane)


For user applications, it doesn't really matter how w is computed; given the
above methods, all choices for the w computation are essentially equivalent.  

For near-nadir images, it is sometimes useful to have the camera w-axis be the
3d-world z-axis.  We can accomplish that behavior as follows:

<u,v,z> => <x,y,z> is computed as:

   (INTERSECT-CAMERA-RAY-WITH-PLANE projection uv-pt (cv 0.0 0.0 -1.0 z))

Similarly, projection onto an arbitrary W' plane:

<u,v,w'> => <x,y,z> is computed as:

   (INTERSECT-CAMERA-RAY-WITH-PLANE projection uv-pt (cv wx' wy' wz' -w'))


|#

;;; **************************   4X4-PROJECTION  **************************
 
#| 

This looks very good.  The INVERSE-TRANSFORM is just another 4X4-PROJECTION
whose projection-matrix is just the INVERSE-MATRIX of this PROJECTION-MATRIX.
What could be simpler?
Furthermore, the w calculation is relatively natural, and symmetric between the
projection and its inverse-projection.

TERMINOLOGY, ASSUMPTIONS, and COMPUTATION OF PROJECTION-MATRIX:

Let M be the projection-matrix.

The projection equation is:

     <us, vs, ws, s> = M <x, y, z, 1>

     <u, v, w, 1> = 1/s <us, vs, ws, s>

The PRINCIPAL RAY direction is <M30, M31, M32>.

The PRINCIPAL PLANE is the plane in perpendicular to the principal ray direction
    containing the projection center.

1/f = vector-length <M30, M31, M32> is the reciprocal of the projection focal length.

GSD = M33 is the GSD in the plane perpendicular to the principal ray containing
    the 3d origin.  

r is the distance from the 3d origin to the principal plane.

We assume that the 3d-world ORIGIN is chosen so that 3d-points we project are
    near that origin rather than arbitrarily far away.

We also assume that the ground-sample-distance (GSD) = r/f at our 3d-world
    points have reasonable values, ie. neither huge nor infinitesimal.  Hence a
    camera with a very large focal-length is expected to be at a proportionately
    large distance R from the 3d-points.

There are a variety of ways to compute row 2.  Are objectives are:

   . The projection-matrix must be non-singular.  
     We also want to avoid ill-conditioned projection matrices.

   . In order to invert the transform, the value of w = M20*x + M21*y + M22*z + M23 
     should have significant (relative) variation over the range of plausible world
     points.  Ie, w should not have forms like:
    
        w = 1/(big-constant + small-variation) or

        w = big-constant + small-variation

     If the computation of w has such a form, then the number of wasted
        bits of precision is w is log2(big-constant/small-variation).

The following cases related to 1/f and GSD need to be considered.

NEAR-FAR CLIPPING:

       NEAR and FAR clip plane distances are specified, consistant
       with values of 1/f and GSD.

       row 2 is chosen consistant with OpenGL depth clipping calculation, which
       guarantees that w ranges from 0 to 1.

ORTHOGRAPHIC: 1/f = 0  =>  ordinary 4x4-coordinate-transform

  if GSD = 0   we have a singular transformation. This is illegal.

  otherwise

       row 2 = UNIT(<M00,M01,M02> x <M10,M11,M12>), M23 = 0

       We divide entire matrix by M33 to make it compatible with
       4x4-coordinate-transforms, ie row3 = <0,0,0,1>

       w = ws*f/r

NEARLY-ORTHOGRAPHIC: 1/f /= 0 and abs(GSD) > T1*abs(1/f)

  The following choice of row 2 guarantees no huge elements in M or M-inverse:

       row 2 = f * <M30, M31, M32, 0>, which is a unit-vector 

       w = ws/s

       Since row 2 is a unit vector with M23 = 0 and the points are near the 3d 
       world origin, w is has small relative error, ie is not offset by a 
       large number.

PERSPECTIVE: 1/f /= 0 and and abs(GSD) <= T1*abs(1/f)

       row 2 = <0,0,0,-1>

       w = -1/s

       s = 1/f<unit-vector>.<x,y,z> + GSD

         = 1/f*dr + GSD = (dr+r)/f

       Small r relative to dr implies sis not offset by a large constant,
       hence its reciprocal (-w) has considerable relative variation.
|#

#+never ; use the one in rq-decomposition
(defun copy-matrix-rows (from-matrix to-matrix from-rows to-rows
				     &optional (ncols (array-dimension from-matrix 1)))
  (declare (type (simple-array double-float (* 4)) from-matrix))
  (declare (type (or null (simple-array double-float (* 4))) to-matrix))
  (unless to-matrix
    (setq to-matrix
	  (make-array (list (length to-rows) ncols)
		      :element-type 'double-float)))
  (loop for from-row fixnum in from-rows
	for to-row fixnum in to-rows
	do (loop for col fixnum from 0 below ncols
		 do (setf (aref to-matrix to-row col)
			  (aref from-matrix from-row col))))
  to-matrix)

;;; When *4X4-PROJECTION-OPENGL-DEPTH-NORMALIZE* is set to nil, 4X4-PROJECTION-TRANSFORM-VECTOR 
;;; returns linear depth values in world units.
;;; When set to non-nil, 4X4-PROJECTION-TRANSFORM-VECTOR returns OpenGL depth values for near-far 
;;; clipping
;;; Do not setq this, but it can be locally rebound as needed.
(defvar *4x4-projection-opengl-depth-normalize* t)
;(setq *4x4-projection-opengl-depth-normalize* nil)
;(setq *4x4-projection-opengl-depth-normalize* t)

#|
NOTE: LHQ Wed Dec  1 2004 

*4X4-PROJECTION-OPENGL-DEPTH-NORMALIZE* = NIL was introduced because of apparent
problems with the high non-linearity of the w computation in
4X4-PROJECTION-TRANSFORM-VECTOR due to the perspective division.

This "appeared" to cause problems in numerical inverse calculations such as 
ITERATIVE-INVERSE-INTERSECT-RAY-WITH-PLANE, which may now be fixed.

The linear w mode may no longer be needed and everything with 
*4x4-projection-opengl-depth-normalize* = NIL may be deprecated. 
|#

;;; **************************  4X4-PROJECTION  **************************

(defstruct-class 4x4-projection (coordinate-projection)       
  ((projection-matrix :initform nil
		     :initarg :projection-matrix
		     :accessor projection-matrix)
   (opengl-depth-unnormalization)
   )
  (:default-initargs :create-inverse t))


(defmethod linear-depth-p ((coordinate-transform t))
  t)

(defmethod linear-depth-p ((coordinate-transform 4x4-projection))
  (declare (optimize (speed 3)))
  (or (null *4x4-projection-opengl-depth-normalize*)
      (zerop (aref (the dmatrix (projection-matrix coordinate-transform)) 3 2))))

(defmethod linear-transform-p ((coordinate-transform 4x4-projection))
  t)

(defmethod transform-matrix ((transform 4x4-projection))
  (projection-matrix transform))

;;; FIXME:  should call decompose-projection-matrix and cache the results.
(defmethod interior-and-exterior-matrices ((projection 4x4-projection))
  (with-class-slots 4x4-projection (projection-matrix)
		    projection      
		    (values nil projection-matrix)))


#+unfinished
(defmethod fasd-form-init-plist ((transform 4x4-projection))
  (with-class-slots 4x4-projection (projection-matrix) transform
    `(:projection-matrix
      (make-and-fill-4x4-matrix . ,(flat-list-4x4-matrix projection-matrix)))))

#+old
(define-fasd-form-init-plist 4x4-projection
    (with-class-slots 4x4-projection (r/f 1/f principal-point-u principal-point-v positive-w-clip-plane) self
      (append `(:1/f ,1/f
		:principal-point-u ,principal-point-u
		:principal-point-v ,principal-point-v)

	      (unless (= r/f 0.0) `(:r/f ,r/f))
	      (unless (= positive-w-clip-plane 0.0) `(:positive-w-clip-plane ,positive-w-clip-plane))
	      
	      )))

#|
If PROJECTION-MATRIX is a 4x4 matrix, it rows 0,1, 3 are used unchanged, and
row 2 is computed by standardize-4x4-projection-matrix.

If PROJECTION-MATRIX is a 3x4 matrix, rows 0,1,2 are copied into rows 0,1,3 of a
new 4x4-matrix and row 2 is computed by standardize-4x4-projection-matrix. 
|#

(defmethod initialize-instance :after
	   ((projection 4x4-projection)
	    &key
	    projection-matrix		; this is really ignored here
	    create-inverse
	    &allow-other-keys)
  (ignore projection-matrix)
  (with-class-slots 4x4-projection (projection-matrix inverse-transform transform-function)
      projection
    (setq projection-matrix
	  (if projection-matrix
	      (copy-matrix projection-matrix)
	      (make-4x4-identity-matrix)))
    (when (and projection-matrix (eql (array-dimension projection-matrix 0) 3))
      (let ((p2 (make-array '(4 4) :element-type 'double-float)))
	(math::copy-matrix-rows projection-matrix p2 '(0 1 2) '(0 1 3))
	(setq projection-matrix p2)))

    ;;(standardize-4x4-projection-matrix projection)

    (unless transform-function 
      (setq transform-function '4x4-projection-transform-vector))
    (when create-inverse
      (let ((inverse (make-instance '4x4-projection
				    :projection-matrix
				    (make-4x4-identity-matrix)
				    ;;(invert-matrix projection-matrix)
				    :create-inverse nil)
	      ))
	(setf inverse-transform inverse
	      (inverse-transform inverse) projection)
	;; The next constructs the projection-matrix of the inverse
	(standardize-4x4-projection-matrix projection) 
	(set-opengl-depth-unnormalization inverse)))
    (set-opengl-depth-unnormalization projection)
    ))

(defmethod transform-direction-vector ((transform 4x4-projection)
				       direction-vector 
				       &optional position-vector into-vector (epsilon .01))
  (generic-transform-direction-vector transform direction-vector position-vector into-vector epsilon))

(defmethod set-opengl-depth-unnormalization ((transform 4x4-projection))
  (let* ((m (4x4-projection-projection-matrix transform))
	 (k (euclidean-length (aref m 2 0) (aref m 2 1)(aref m 2 2))))
    (setf (4x4-projection-opengl-depth-unnormalization transform)
	  (if (= k 0.0) 1.0 (/ 1.0 k)))))
   
(defparameter *4X4-PROJECTION-R-THRESHOLD* 1e5)

;;; This "standardizes" a homogeneous 4x4 matrix by computing row2 such that the
;;; 4x4 matrix is non-singular and appropriate for computing the w depth ording
;;; component.  ORTHOGRAPHIC, NEAR-ORTHOGRAPHIC, and ORDINARY PERSPECTIVE cases
;;; are specifically handled.  Note that this function does nothing to deal with
;;; the OpenGL near/far depth planes.
(defmethod standardize-4x4-projection-matrix ((transform 4x4-projection))
  (with-class-slots 4x4-projection (projection-matrix inverse-transform) transform
    (setf projection-matrix
	  (let* ((m projection-matrix)
		 (wx/f (aref m 3 0))
		 (wy/f (aref m 3 1))
		 (wz/f (aref m 3 2))
		 (1/f (inline-euclidean-length wx/f wy/f wz/f))
		 (GSD (aref m 3 3)))
	    (declare (type 4x4-matrix m))
	    (declare (double-float wx/f wy/f wz/f 1/f GSD))
	    (flet ((set-row2 (f m23)
		     (declare (double-float f m23))
		     (setf (aref m 2 0) (* wx/f f)
			   (aref m 2 1) (* wy/f f)
			   (aref m 2 2) (* wz/f f)
			   (aref m 2 3) m23)))
	      (cond
		;; ORTHOGRAPHIC case
		((zerop 1/f)		
		 (when (zerop GSD) (error "Projection not invertable"))
		    
		 ;; Compute a vector orthogonal to rows 0 and 1
		 (inline-cross-prod ((aref m 0 0)(aref m 0 1)(aref m 0 2))
				    ((aref m 1 0)(aref m 1 1)(aref m 1 2))
				    (wx/f wy/f wz/f))
		 ;; Set row2 to the orthonormal unit vector.
		 (set-row2 (/ (inline-euclidean-length wx/f wy/f wz/f)) 0.0)
		 ;; Division of M by GSD= M33 makes matrix compatible with
		 ;; 4x4-coordinate-transform. Is that important?
		 (values (scale-matrix m (/ GSD)) 'orthographic))

		;; NEAR-ORTHOGRAPHIC case
		((>= (abs GSD) (* 1/f *4x4-projection-r-threshold*))
		 ;; That test is equivalent to (>= (abs r) *4x4-projection-r-threshold*)
		 ;; Set row 2 to a unit-vector copy of the first 3 elements of row 3.  
		 ;; Note that since M23=0 and M33 /= 0, rows 2 and 3 are linearly independent.
		 (set-row2 (abs (/ 1/f)) 0.0) 
		 (values m 'near-orthographic))
		   
		;; ORDINARY PERSPECTIVE case
		(t  
		 ;; Since the euclidean-length of first 3 elements of row 3 is non-zero,
		 ;; rows 2 and 3 will be linearly independent.
		 (set-row2 0.0 -1.0)
		 (values m 'perspective)))
	      )))
    (when inverse-transform
      (invert-matrix projection-matrix
		     (projection-matrix inverse-transform)))
    ))

(defmethod update-transform ((transform 4x4-projection))
  (with-class-slots 4x4-projection (inverse-transform projection-matrix)
      transform
    (when inverse-transform
      (invert-matrix projection-matrix
		     (projection-matrix inverse-transform)))))

(defvar *transform-dammit* nil)


(defun 4x4-project-vector (projection-matrix from-vector &optional to-vector)
  (declare (type (simple-array double-float (*))  from-vector))
  (declare (type (or null (simple-array double-float (*)))  to-vector))
  (bind-vector-elements (x y z) from-vector
    (let ((m projection-matrix)
	  (us 1.0d0) (vs 1.0d0) (ws 1.0d0) (s 1.0d0) (1/s 1.0d0))
      (declare (double-float us vs ws s 1/s))
      (declare (type 4x4-matrix m))
      (inline-matrix-times-vector m (x y z 1.0) (us vs ws s))
      (when (or *transform-dammit* (> s 0.0))
	(unless to-vector (setq to-vector (make-coordinate-vector 3)))
	(setf 1/s (/ 1.0 s))
	(setf (aref to-vector 0) (* us 1/s)
	      (aref to-vector 1) (* vs 1/s)
	      (aref to-vector 2) 
	      (if *4x4-projection-opengl-depth-normalize*
		  (* ws 1/s)
		  (/ ws (inline-euclidean-length (aref m 2 0)(aref m 2 1)(aref m 2 2)))))
	to-vector))))

#+never
(defun 4x4-projection-transform-vector (projection from-vector to-vector)
  (4x4-project-vector (4x4-projection-projection-matrix projection)
		      from-vector to-vector))


;(disassemble '4x4-projection-transform-vector)
(defun 4x4-projection-transform-vector (projection from-vector to-vector)
  #+cme (declare (ext:optimize-interface (speed 3) (safety 1)))
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (or null coordinate-vector) from-vector))
  (declare (type coordinate-vector to-vector))
  (bind-vector-elements (x y z) from-vector
    (let ((m (4x4-projection-projection-matrix projection))
	  (us 1.0d0) (vs 1.0d0) (ws 1.0d0) (s 1.0d0) (1/s 1.0d0))
      (declare (double-float us vs ws s 1/s))
      (declare (type 4x4-matrix m))
      (inline-matrix-times-vector m (x y z 1.0) (us vs ws s))
      (when (or *transform-dammit* (> s 0.0))
	(setf 1/s (/ 1.0 s))
	(setf (aref to-vector 0) (* us 1/s)
	      (aref to-vector 1) (* vs 1/s)
	      (aref to-vector 2) 
	      (if *4x4-projection-opengl-depth-normalize*
		  (* ws 1/s)
		  (* ws (the double-float (4x4-projection-opengl-depth-unnormalization projection)))))
	to-vector))))
#|
;(transform-vector (3d-to-2d-projection (gui::top-view)) (gui::selected-object-world-position))

(let ((proj (3d-to-2d-projection (gui::top-view)))
      (3dpt (gui::selected-object-world-position))
      (*4x4-projection-opengl-depth-normalize* t)
      ;(*transform-dammit* t)
      )
  (list 3dpt (inverse-transform-vector proj  (transform-vector proj 3dpt))))
|#

#|
When *4x4-projection-opengl-depth-normalize* = NIL, the inverse-transform must be computed differently.

|#

;;; FIXME -- broken when 1/f=0
#+unfinished
(defun inverse-4x4-projection-transform-vector (projection from-vector to-vector)
  (declare (type coordinate-vector from-vector))
  (declare (type (or null coordinate-vector)  to-vector))
  (declare (optimize (speed 3) (safety 2)))
  #+cme (declare (ext:optimize-interface (speed 3) (safety 1)))
  (if *4x4-projection-opengl-depth-normalize*
      (4x4-projection-transform-vector projection from-vector to-vector)
      (bind-vector-elements (u v zp) from-vector
	(let* ((frame-camera (coordinate-transform-inverse-transform projection))
	       (m (invert-matrix (frame-camera-3d-to-camera-matrix frame-camera))) ; camera-to-3d
	       (r (frame-camera-camera-to-2d-matrix frame-camera))
	       (near (/ (aref r 2 3) (aref r 2 2)))
	       (zp  (-(+ zp near)))
	       (1/f (aref r 3 2))
	       (GSD (aref r 3 3))
	       (ppu (/ (aref r 0 2) 1/f))
	       (ppv (/ (aref r 1 2) 1/f))
	       (vp (/ (- v ppv) (aref r 1 1)))
	       (up (/ (- u ppu (* (aref r 0 1) vp)) (aref r 0 0)))
	       (zp/f-GSD (- (* zp 1/f) GSD)) 
	       (xp (* zp/f-GSD up))
	       (yp (* zp/f-GSD vp))
	       (to-vector (or to-vector (make-coordinate-vector 3)))
	       )
	  (declare (type 4x4-matrix m r))
	  (declare (type coordinate-vector  to-vector))
	  (declare (double-float near vp up 1/f GSD zp/f-GSD xp yp))
	  (inline-matrix-times-vector m (xp yp zp 1.0)
				      ((aref to-vector 0) (aref to-vector 1) (aref to-vector 2)))
	  ;(break)
	  to-vector))))





;;;(undefmethod '(transform-jacobian (4x4-projection t)))
;;; This is for a pinhole-camera projection-matrix.
;;; Should rename to just JACOBIAN or COORDINATE-TRANSFORM-JACOBIAN?
(defmethod transform-jacobian ((projection 4x4-projection)
			       3d-position &optional J)
  (declare (type (or null (dmatrix (3 3))) J))
  (declare (type coordinate-vector 3d-position))
	   
  ;; Could avoid multiplying interior and exterior matrices
  ;; using chain rule.  Is that worth it?
  (let* ((m (4x4-projection-projection-matrix projection))
	 (J (or J (make-array '(3 3) :element-type 'double-float :initial-element 0.0d0)))
	 (xs 0.0d0) (ys 0.0d0) (zs 0.0d0) (s 0.0d0) (1/s^2 0.0d0))
      (declare (type 4x4-matrix m))
      (declare (type (dmatrix (3 3)) J))
      (declare (double-float xs ys zs s 1/s^2))
      (bind-vector-elements (x y z) 3d-position
	(inline-matrix-times-vector m (x y z 1.0) (xs ys zs s))
	(setq 1/s^2 (/ (* s s)))
	(when (or *transform-dammit* (> s 0.0))
	  (let ((m30 (aref m 3 0))
		(m31 (aref m 3 1))
		(m32 (aref m 3 2))
		)
	    (declare (double-float m30 m31 m32))
	    (setf (aref J 0 0) (* 1/s^2 (- (* s (aref m 0 0)) (* xs m30)))
		  (aref J 0 1) (* 1/s^2 (- (* s (aref m 0 1)) (* xs m31)))
		  (aref J 0 2) (* 1/s^2 (- (* s (aref m 0 2)) (* xs m32))))

	    (setf (aref J 1 0) (* 1/s^2 (- (* s (aref m 1 0)) (* ys m30)))
		  (aref J 1 1) (* 1/s^2 (- (* s (aref m 1 1)) (* ys m31)))
		  (aref J 1 2) (* 1/s^2 (- (* s (aref m 1 2)) (* ys m32))))

	    (if *4x4-projection-opengl-depth-normalize*
		;; This treats the w-axis in a "homogeneous" manner, ie the same as the u and v axes.
		;; See the discussion about "Generic Properties of Coordinate Projections", and the
		;; definition of the w-axis.
		(setf (aref J 2 0) (* 1/s^2 (- (* s (aref m 2 0)) (* zs m30)))
		      (aref J 2 1) (* 1/s^2 (- (* s (aref m 2 1)) (* zs m31)))
		      (aref J 2 2) (* 1/s^2 (- (* s (aref m 2 2)) (* zs m32))))
		;; This is the non-homogeneous treatment of the w-axis
		(let ((1/p22 (4x4-projection-opengl-depth-unnormalization projection)))
		  (setf (aref J 2 0) (* 1/p22 (aref m 2 0))
			(aref J 2 1) (* 1/p22 (aref m 2 1))
			(aref J 2 2) (* 1/p22 (aref m 2 2))
			)))
          
	    J)))))


(defmethod compose-transforms ((object-to-world-transform 4x4-coordinate-transform)
			       (projection 4x4-projection))
  (make-instance '4x4-projection :projection-matrix 
		 (multiply-matrices (projection-matrix projection)
				    (transform-matrix object-to-world-transform))
		 :from-coordinate-system (from-coordinate-system object-to-world-transform)
		 :to-coordinate-system (to-coordinate-system projection)))



;;;  ****************   INTERSECT-CAMERA-RAY-WITH-PLANE   **************** 
#|
Theory:

X' = <x',y',z',s'> = Pinv U  = Pinv<u,v,w,1>

Xs = <xs,ys,zs,s>  = Pinv U  = Pinv<u,v,w,1> 

X = <x,y,z,1> = Xs/s

Let Xuv = Pinv<u,v,0,1> 
Let Xw  = Pinv<0,0,1,0>

Then, X' = Xuv + w*Xw

Want          X . N = 0
Equivalently, X' . N = 0.

Solve for w in

      N.Xuv + w * N.Xw = 0

w = -  N.Xuv / N.Xw

|#

;;; LHQ Sun Nov 28 2004:  This apparently doesn't care whether there is Opengl depth normalization.
;;; FIXME:  Should report when the intersection is behind the camera.
(defmethod inverse-intersect-ray-with-plane ((transform 4x4-projection) 2d-pos plane
					     &optional starting-approx)
  (declare (ignore starting-approx))
  ;(break)
  (bind-vector-elements (u v) 2d-pos
    (bind-vector-elements (nx ny nz n1) plane
      (let* ((pinv (4x4-projection-projection-matrix transform))
	     (xuv 0.0) (yuv 0.0) (zuv 0.0) (suv 0.0) 
	     (xw 0.0)  (yw 0.0)  (zw 0.0)  (sw 0.0))
	(declare (type (simple-array double-float (* *)) pinv))
	(declare (double-float xuv yuv zuv suv xw yw zw sw))
	(inline-matrix-times-vector pinv (u v 0.0 1.0) (xuv yuv zuv suv))
	(inline-matrix-times-vector pinv (0.0 0.0 1.0 0.0) (xw yw zw sw))  
	(let* ((w (- (/ (inline-inner-prod (nx ny nz n1) (xuv yuv zuv suv))
			(inline-inner-prod (nx ny nz n1) (xw yw zw sw)))))
	       (xs (+ xuv (* w xw)))
	       (ys (+ yuv (* w yw)))
	       (zs (+ zuv (* w zw)))
	       (s  (+ suv (* w sw)))
	       (1/s (/ s)))
	  (declare (double-float w xs ys zs s 1/s))
	  (math::inline-coordinate-vector (* xs 1/s) (* ys 1/s) (* zs 1/s)))))))

#+experimental ; 
(defmethod inverse-intersect-ray-with-plane ((transform 4x4-projection) 2d-pos plane
					     &optional starting-approx)
  (declare (ignore starting-approx))
					;(break)
  (bind-vector-elements (u v) 2d-pos
    (bind-vector-elements (nx ny nz n1) plane
      (let* ((pinv (4x4-projection-projection-matrix transform))
	     (xuv 0.0) (yuv 0.0) (zuv 0.0) (suv 0.0) 
	     (xw 0.0)  (yw 0.0)  (zw 0.0)  (sw 0.0))
	(declare (type (simple-array double-float (* *)) pinv))
	(declare (double-float xuv yuv zuv suv xw yw zw sw))
	(inline-matrix-times-vector pinv (u v 0.0 1.0) (xuv yuv zuv suv))
	(inline-matrix-times-vector pinv (0.0 0.0 1.0 0.0) (xw yw zw sw))
	(let* ((w-num (inline-inner-prod (nx ny nz n1) (xuv yuv zuv suv)))
	       (w-den (inline-inner-prod (nx ny nz n1) (xw yw zw sw)))
	       (w (- (/ w-num w-den)))
	       (xs (+ xuv (* w xw)))
	       (ys (+ yuv (* w yw)))
	       (zs (+ zuv (* w zw)))
	       (s  (+ suv (* w sw))))
	  (declare (double-float w xs ys zs s))
	  (setq *foo* (list transform 2d-pos plane w-den w suv sw s))
	  (let ((1/s (/ s)))
	    (declare (double-float 1/s))
	    (values (math::inline-coordinate-vector (* xs 1/s) (* ys 1/s) (* zs 1/s))
		    1/s)))))))
#|
(destructuring-bind (transform 2d-pos plane w-den w suv sw s) *foo*
  (+ suv (* w sw)))

(let* ((proj (car *foo*))
       (p (4x4-projection-projection-matrix (inverse-transform proj)))
       (pinv (4x4-projection-projection-matrix proj)))
  (list pinv p (multiply-matrices p pinv)))
|#

;;; this works
(defmethod camera-direction-vector-at-image-point ((transform 4x4-projection) 2d-pos)
  ;(break)
  (declare (optimize (speed 3)))
  (bind-vector-elements (u v) 2d-pos
    (let* ((pinv (4x4-projection-projection-matrix (inverse-transform transform)))
	   (x1 0.0) (y1 0.0) (z1 0.0) (s1 0.0) 
	   (x0 0.0) (y0 0.0) (z0 0.0) (s0 0.0))
      (declare (type (simple-array double-float (* *)) pinv))
      (declare (double-float x1 y1 z1 s1 x0 y0 z0 s0))
      (inline-matrix-times-vector pinv (u v .9 1.0) (x1 y1 z1 s1))
      (inline-matrix-times-vector pinv (u v 0.0 1.0) (x0 y0 z0 s0))
      (when (or (zerop s0)(zerop s1)) (setq *foo* (list transform 2d-pos pinv u v x1 y1 z1 s1)))
      (normalize-coordinate-vector (cv (- (/ x1 s1) (/ x0 s0)) 
				       (- (/ y1 s1) (/ y0 s0)) 
				       (- (/ z1 s1) (/ z0 s0)))))))

;;; this works
(defmethod camera-direction-vector-at-image-point2 ((transform 4x4-projection) 2d-pos)
  ;(break)
  (declare (optimize (speed 3)))
  (bind-vector-elements (u v) 2d-pos
    (let* ((pinv (4x4-projection-projection-matrix (inverse-transform transform)))
	   (tinv (invert-matrix (camera-to-2d-matrix transform)))
	   (rinv (invert-matrix (3d-to-camera-matrix transform)))
	   (x1 0.0) (y1 0.0) (z1 0.0) (dx 0.0) (dy 0.0) (dz 0.0) )
      (declare (type (simple-array double-float (* *)) pinv))
      (declare (double-float x1 y1 z1 dx dy dz))
      (inline-matrix-times-vector tinv (u v 0.0 1.0) (x1 y1 z1))
      (setq *foo* (list tinv (camera-to-2d-matrix transform) x1 y1 z1))
      (inline-matrix-times-vector rinv (x1 y1 z1) (dx dy dz))
      (normalize-coordinate-vector (cv dx dy dz)))))

#+broken
(defmethod camera-direction-vector-at-image-point2 ((transform 4x4-projection) 2d-pos)
  ;(break)
  (declare (optimize (speed 3)))
  (bind-vector-elements (u v) 2d-pos
    (let* ((pinv (4x4-projection-projection-matrix (inverse-transform transform)))
	   (x1 0.0) (y1 0.0) (z1 0.0))
      (declare (type (simple-array double-float (* *)) pinv))
      (declare (double-float x1 y1 z1))
      (setq *foo* (list (invert-matrix (camera-to-2d-matrix transform)) pinv))
      (inline-matrix-times-vector pinv (u v 0.0 1.0) (x1 y1 z1))
      (bind-vector-elements (x0 y0 z0) (origin transform )
	(normalize-coordinate-vector (cv (- x1 x0) (- y1 y0) (- z1 z0)))))))

#|
(let ((proj (gui::3d-to-2d-projection (gui::top-view))))
  (intersect-camera-ray-with-plane proj (transform-vector proj (gui::selected-object-world-position))
				   (cv 0.0 0.0 -1.0 6071.0)))
(let ((proj (gui::3d-to-2d-projection (gui::top-view))))
  (intersect-camera-ray-with-plane proj (transform-vector proj (gui::selected-object-world-position))
				   (cv 0.0 0.0 -1.0 289.0)))
|#

#| tests

(setq pt (origin (gui::selected-object)))
(setq p1 (3d-to-2d-projection (top-view)))
(standardize-4x4-projection-matrix  p1)

(setq pmat0 (copy-matrix-rows
	     (transform-matrix (3d-to-2d-projection (top-view)))
	     (make-array '(3 4) :element-type 'double-float)
	     '(0 1 3) '(0 1 2)
	     ))
(setq pmat
      (transform-matrix (setq p1 (make-instance '4X4-PROJECTION :projection-matrix pmat0))))

(decompose-projection-matrix pmat)
(math::invert-matrix pmat)
(setq iparms (multiple-value-list
		 (decompose-projection-matrix (math::invert-matrix pmat))))
(decompose-projection-matrix (math::invert-matrix pmat))

(setq ipmat2
      (transform-matrix
       (MAKE-PERSPECTIVE-TRANSFORM
	:camera-to-world-matrix
	(make-and-fill-2d-array
	 '((-0.54196656401916 -0.8404000496699567 5.061403216800092E-16 352.13731837585766) (-0.8404000496699351 0.5419665640191468 -2.3444804828336106E-15 5958.300710086118) (-1.1545493078743254E-17 7.002817966028024E-18 0.9999999999999998 500.0000221550892) (0.0 0.0 0.0 1.0)))
	:1/F -4.359064112275498E-4 :GSD 0.0 :PRINCIPAL-POINT-U -57.24257300000216 :PRINCIPAL-POINT-V 4954.488300000001 :V-AXIS-ASPECT-RATIO 1.6348437979929582 :V-AXIS-SKEW 1.3796195621128342)))

(scale-matrix ipmat2 (/ (aref (math::invert-matrix pmat) 3 3) (aref ipmat2 3 3)))
(decompose-projection-matrix pmat)
(decompose-projection-matrix (math::invert-matrix pmat))
(math::invert-matrix pmat)

;;; case1  orthographic 
(setq pmat
      (transform-matrix 
       (setq p1 (make-instance
		 '4X4-PROJECTION
		 :projection-matrix
		 (make-and-fill-2d-array
		  '((1.0 0.0 0.0 0.0)
		    (0.0 1.0 0.0 0.0)
		    (0.0 0.0 0.0 3.0)))))))
(inverse-transform-vector p1 p1p)
(math::invert-matrix pmat)
(transform-vector p1 (cv 0.0 0.0 1e12))
(setq p1p (transform-vector p1 (cv 0.0 0.0 (+ 10.0 -1e12))))
(setq p1p (transform-vector p1 (cv 0.0 0.0 (+ 10.1 -1e12))))
(transform-vector p1 (cv 0.0 0.0 (+ 1e-3 1e12)))
(setq p1p (transform-vector p1 (cv 0.0 0.0 0.0)))
(setq p1p (transform-vector p1 (cv 0.0 0.0 1e-3)))
(transform-vector p1 (cv 0.0 0.0 (+ 100 1e12)))

(transform-vector p1 (cv 0.0 0.0 1e6))
(transform-vector p1 (cv 0.0 0.0 (+ 100 1e6)))

(transform-vector p1 (cv 0.0 0.0 0.0))
(transform-vector p1 (cv 0.0 0.0 100.0))
(transform-vector p1 (cv 0.0 0.0 101.0))
(transform-vector p1 (cv 0.0 0.0 1000.0))

(COMPUTE-W-AT-ZP p1 100.0) ; case4
(COMPUTE-W-AT-ZP p1 1e12)  ; case4
(COMPUTE-W-AT-ZP p1 2e12) ; case3
(COMPUTE-W-AT-ZP p1 100.0) ; case1
(transform-vector p1 pt)
(COMPUTE-W-AT-ZP p1 6086.6777668068)

 __________________________

(setq 3x4-mat
      (make-and-fill-2d-array
       '((0.0064930482258692395 0.9997721516874837 0.07361694880812925 -5065.350029502222)
	 (-1.0001973734151602 0.006944123671634666 0.04410998715449522 4637.178730787888)
	 (1.5579586919309364E-6 8.521090859340529E-7 -5.420205162840362E-4 6.4892647209803))))

(setq 3x4-mat (copy-matrix-rows (projection-matrix (3d-to-2d-projection (top-view)))
				nil '(0 1 3) '(0 1 2) '(3 4)))

(setq p1 (make-instance '4X4-PROJECTION :projection-matrix 3x4-mat)
      pmat (projection-matrix p1))


(change-projection (2d-world (top-view)) p1)
(revert-previous-projection (2d-world (top-view)))
(decompose-projection-matrix pmat)
(math::print-matrix pmat)
 
(setq pt (origin obj))
(setq pt (CV 2403.775 5949.493 5901.912))
(CV 2403.775 5949.493 5901.912)

(transform-vector (3d-to-2d-projection (top-view)) pt)
(CV 404.00930825175636 768.2579244707855 -6086.677726288257)

(transform-vector p1 pt)
(inverse-transform-vector p1 (transform-vector p1 pt))
(inverse-transform-vector p1 (vector-add (transform-vector p1 pt)
					 (cv 0.0 0.0 1.0)))
(inverse-transform-vector p1 (vector-add (transform-vector p1 pt)
					 (cv 0.0 0.0 1e-6)))
(inverse-transform-vector p1 (cv 0.0 0.0 1e10))
(math::print-matrix (rq-decompose pmat))
(math::print-matrix pmat)
(math::print-matrix(projection-partial-derivitives p1 pt))
(copy-matrix-rows pmat nil '(0 1 3) '(0 1 2) '(3 3))

(CV 404.00930825175636 768.2579244707855 0.999671413523662)
(invert-projection p1 (transform-vector p1 pt))
(CV 5193.099164541676 4150.214013072601 11991.811283716214) 
(CV -0.37876347473224703 -0.30260312536647205 -0.8746253933697543) 

(vector-to-vector-distance pt (cv 5193.557582271814 4149.918306536297 11992.812141858109))

6936.888385421038

(origin (gui::selected-object))
(transform-vector p1 (origin (gui::selected-object)))
(CV 940.281748238133 2580.670636271136 5901.912)

(let* ((pt1 (origin (gui::selected-object)))
       (2dpt (transform-vector p1 pt1))
       (pt2 (intersect-camera-ray-with-plane p1 2dpt (cv 0.0 0.0 1.0 (- 5901.912)))))
      (values pt2 (transform-vector p1 pt2) 2dpt))


(TRANSFORM-JACOBIAN p1 (origin (gui::selected-object)))

(CV 2403.775000002313 5949.492999998939 5901.9120000012135)

(projection-partial-derivitives (3d-to-2d-projection (top-view)) pt)
(projection-partial-derivitives p1 pt)

;;; (transform-matrix p1) should be (from alv-3-42)
  0  0        0.0064930482258692395
  0  1        0.9997721516874837
  0  2        0.07361694880812925
  0  3        -5065.350029502222
  1  0        -1.0001973734151602
  1  1        0.006944123671634666
  1  2        0.04410998715449522
  1  3        4637.178730787888
  2  0        1.5579586919309364E-6
  2  1        8.521090859340529E-7
  2  2        -5.420205162840362E-4
  2  3        6.488180674122808
  3  0        1.5579586919309364E-6
  3  1        8.521090859340529E-7
  3  2        -5.420205162840362E-4
  3  3        6.4892647209803
>>

(setq p1 (make-instance
	  '4X4-PROJECTION
	  :near 2.0			; :near (- 11993.813 3000.0)
	  :projection-matrix
	  (make-and-fill-2d-array
	   '((0.9856085334905242 -0.19475829220043553 -0.7456240466432562 4421.260325566256)
	     (0.19404268721514403 0.9771474655431764 -0.7568962563439589 1972.1695674708299)
	     (1.0080086144806885E-6 -1.1939286199559458E-6 -1.9550592913710567E-4 0.3819035896891346)))))

fhn719
  0  0        0.9856085334905242
  0  1        -0.19475829220043553
  0  2        -0.7456240466432562
  0  3        4421.260325566256
  1  0        0.19404268721514403
  1  1        0.9771474655431764
  1  2        -0.7568962563439589
  1  3        1972.1695674708299
  2  0        1.0080086144806885E-6
  2  1        -1.1939286199559458E-6
  2  2        -1.9550592913710567E-4
  2  3        0.38151256534269923
  3  0        1.0080086144806885E-6
  3  1        -1.1939286199559458E-6
  3  2        -1.9550592913710567E-4
  3  3        0.3819035896891346
>

(setq p1 (make-instance
	  '4X4-PROJECTION
	  :near 1000.0 :far 10000.0
	  :projection-matrix
	  (make-and-fill-2d-array
	   '((0.9996767617444122 -0.01420172295925684 0.020591557799247182 -2228.9683558189213)
	     (0.01333188471274901 0.9993135569238071 0.034866737178149414 -2917.2570607693883)
	     (0.0 0.0 0.0 3.1)))))

  0  0        0.9996767617444122
  0  1        -0.01420172295925684
  0  2        0.020591557799247182
  0  3        -2228.9683558189213
  1  0        0.01333188471274901
  1  1        0.9993135569238071
  1  2        0.034866737178149414
  1  3        -2917.2570607693883
  2  0        7.258336115888097E-6
  2  1        1.191121041941884E-5
  2  2        -3.4416187322622934E-4
  2  3        7.091096218643031
  3  0        0.0
  3  1        0.0
  3  2        0.0
  3  3        3.1

(inverse-transform-vector p1 (cv 0.0 0.0 1.0))
(CV 2478.3429793238957 3229.6669742865415 -9844.16500200493)
(CV 2023.4478900000002 2483.1663999999996 11725.181999999999)

(transform-vector p1 (CV 2478.3429793238957 3229.6669742865415 -9844.16500200493))
(inverse-transform-vector p1 (cv 0.0 0.0 0.0))
(CV 2288.689694268662 2918.4385404525247 -851.5475580434115) 
(transform-vector p1 (CV 2288.689694268662 2918.4385404525247 -851.5475580434115))

(math::vector-to-vector-distance
 (CV 2288.689694268662 2918.4385404525247 -851.5475580434115)
 (cv 1812.722 2137.357 21716.98)
 )

(math::vector-to-vector-distance
 (CV 2023.4478900000002 2483.1663999999996 11725.181999999999)
 (cv 1812.722 2137.357 21716.98)
 )



(defmethod compute-r ((projection 4x4-projection))
  (let* ((m (4x4-projection-transform-matrix projection))
	 (1/f (euclidean-length (aref m 3 0) (aref m 3 1) (aref m 3 2)))
	 (GSD (aref m 3 3))
	 (r (/ GSD 1/f)))
    r))


(compute-r p1) = 11972.295697979904

(pray-pt p1 (compute-r p1))
(transform-vector p1 (pray-pt p1 0.0))
(transform-vector p1 (pray-pt p1 (compute-r p1)))

|#




;;; PROJECTION-MATRIX can be the full projection-matrix or just the
;;; CAMERA-TO-2D-MATRIX
(defun set-projection-matrix-depth-clipping (projection-matrix &key near far)
  (if (not near)
      projection-matrix
      (let* ((m projection-matrix)
	     (1/f (- (inline-euclidean-length (aref m 3 0) (aref m 3 1) (aref m 3 2))))
	     (GSD (aref m 3 3))
	     (near (- (abs (the double-float near))))
	     (1/far (if far (/ -1.0 (abs (the double-float far))) 0.0))
	     (P22 (/ (+ 1/f (* GSD 1/far))
		     (- 1.0 (* near 1/far))))
	     (P22*f (/ P22 1/f))
	     )
	(declare (type 4x4-matrix m))
	(declare (double-float 1/f GSD near 1/far P22 P22*f))
	(loop for i fixnum from 0 below 3
	      do (setf (aref m 2 i) (* P22*f (aref m 3 i))))
	(setf (aref m 2 3) (- (* P22 near)))
	;;(setq foo (list 1/f GSD near 1/far P22 P22*f))
	m)))

(defun set-projection-matrix-depth-clipping (projection-matrix &key near far)
  (if (not near)
      projection-matrix
      (let* ((m projection-matrix)
	     (1/f (- (inline-euclidean-length (aref m 3 0) (aref m 3 1) (aref m 3 2))))
	     (GSD (aref m 3 3))
	     (near (- (abs (the double-float near))))
	     (1/far (if far (/ -1.0 (abs (the double-float far))) 0.0))
	     (P22 (/ 1/f
		     (- 1.0 (* near 1/far))))
	     (P22*f (/ P22 1/f))
	     (P23 (- (* P22 near)))
	     )
	(declare (type 4x4-matrix m))
	(declare (double-float 1/f GSD near 1/far P22 P22*f))
	(loop for i fixnum from 0 below 3
	      do (setf (aref m 2 i) (* P22*f (aref m 3 i))))
	(setf (aref m 2 3) (+ (* P22*f GSD) P23))
	m)))

#+never
(progn ;; flush all of this

;;; z in the camera-relative z position of the point.
(defmethod 4x4-projection-w-at-z ((projection 4x4-projection) z)
  (declare (double-float z))
  (let ((m (projection-matrix projection)))
    (declare (type 4x4-matrix m))
    (setq z (- z))
    (/ (+ (* z (aref m 2 2)) (aref m 2 3))
       (+ (* z (aref m 3 2)) (aref m 3 3)))))

(defmethod 4x4-projection-z-at-w ((projection 4x4-projection) w)
  (declare (double-float w))
  (let ((m (projection-matrix projection)))
    (declare (type 4x4-matrix m))
    (/ (- (* w (aref m 3 3)) (aref m 2 3))
       (- (* w (aref m 3 2)) (aref m 2 2)))))

;;; z in the camera-relative z position of the point.
(defmethod world-point-at-z ((projection 4x4-projection) 2d-pos z)
  (let ((w (4x4-projection-w-at-z projection z)))
    (bind-vector-elements (u v) 2d-pos
      (inverse-transform-vector projection (cv u v w)))))

;;; z in the camera-relative z position of the point.
#+never ; unused and a generally bad API.  Really want intersect-camera-ray-with-surface
(defmethod project-to-world ((projection 4x4-projection) 2d-pos z)
  (let ((w (4x4-projection-w-at-z projection z)))
    (bind-vector-elements (u v) 2d-pos
      (inverse-transform-vector projection (cv u v w)))))

) ; end progn

;;; This sucks, but cannot really do much better
(defmethod 3d-to-camera-matrix ((projection 4x4-projection))
  (decompose-projection-matrix (projection-matrix  projection)))

;;; Calls to this method are to be avoided because dynamic sensors do not have a 
;;; constant camera-position.
(defmethod camera-position ((projection 4x4-projection))
  (let ((camera-to-3d-mat (invert-matrix (3d-to-camera-matrix projection))))
    (declare (type 4x4-matrix camera-to-3d-mat))
    (cv (aref camera-to-3d-mat 0 3) (aref camera-to-3d-mat 1 3) (aref camera-to-3d-mat 2 3))))



;;; New Mon Sep  6 2004

#+never
(defmethod transform-projection-matrix-to-camera-centered-lvcs 
           ((frame-camera 4x4-projection)
            &key (local-units-per-meter 1.0))

  (let* ((initial-lvcs (from-coordinate-system frame-camera))
         (lvcs-to-gcc (lvcs-to-geocentric-transform initial-lvcs))       
         )
    ;;(mv-bind (world-to-camera-matrix camera-to-2d-parameters camera-to-2d-matrix) 
    ;;(decompose-projection-matrix (projection-matrix frame-camera))
    (let* ((lvcs-to-lat-long-transform (lvcs-to-lat-long-transform initial-lvcs))
	   (lat-long-cs (to-coordinate-system lvcs-to-lat-long-transform))
	   #+unused
	   (gcc-to-gdc-transform 
	    (gcc-to-gdc-transform (ellipsoid lat-long-cs)))
	   (geo-origin (transform-vector lvcs-to-lat-long-transform (camera-position frame-camera)))
             
	   (camera-lvcs
	    (bind-vector-elements (long lat) geo-origin
	      (make-lvcs lat-long-cs lat long :units local-units-per-meter)))
	   (camera-lvcs-to-gcc (to-geocentric-transform camera-lvcs))
	   (new-projection-matrix
	    (multiply-matrices (projection-matrix frame-camera)
			       (multiply-matrices 
				(invert-matrix (transform-matrix lvcs-to-gcc))
				(transform-matrix camera-lvcs-to-gcc))))
                                                                               
	   )
        
      new-projection-matrix)))


;;; FIXME:  This should "cache" its results.
(defmethod interior-and-exterior-matrices ((projection 4x4-projection))
  (mv-bind (3d-to-camera-matrix camera-to-2d-parameters camera-to-2d-matrix)
      (decompose-projection-matrix (projection-matrix projection))
    (declare (ignore camera-to-2d-parameters))
    (values camera-to-2d-matrix 3d-to-camera-matrix)))

;;;(defmethod transform-projection-matrix-to-camera-centered-lvcs 
;;;           ((frame-camera 4x4-projection) &key (local-units-per-meter 1.0))
;;;
;;;  (mv-bind (camera-to-2d-matrix  3d-to-camera-matrix) 
;;;      (interior-and-exterior-matrices frame-camera)
;;;    (let* ((initial-lvcs (from-coordinate-system frame-camera))
;;;           (lvcs-to-gcc (lvcs-to-geocentric-transform initial-lvcs))
;;;           (lvcs-to-lat-long-transform (lvcs-to-lat-long-transform initial-lvcs))
;;;           (lat-long-cs (to-coordinate-system lvcs-to-lat-long-transform))
;;;           ;;(gcc-to-gdc-transform (gcc-to-gdc-transform (ellipsoid lat-long-cs)))
;;;           (geo-origin (transform-vector lvcs-to-lat-long-transform (camera-position frame-camera)))
;;;             
;;;           (camera-lvcs
;;;            (bind-vector-elements (long lat) geo-origin
;;;              (make-lvcs lat-long-cs lat long :units local-units-per-meter)))
;;;           (camera-lvcs-to-gcc (to-geocentric-transform camera-lvcs))
;;;           (lvcs-to-lvcs-matrix (multiply-matrices (invert-matrix (transform-matrix lvcs-to-gcc))
;;;                                                   (transform-matrix camera-lvcs-to-gcc)))
;;;           #+unused
;;;           (scale (/ (inline-euclidean-length (aref lvcs-to-lvcs-matrix 0 0)
;;;                                              (aref lvcs-to-lvcs-matrix 0 1)
;;;                                              (aref lvcs-to-lvcs-matrix 0 2))))
;;;           (new-3d-to-camera-matrix (multiply-matrices 3d-to-camera-matrix
;;;                                                          lvcs-to-lvcs-matrix)))
;;;      ;; These 2 cases produce the same result (modulo rounding)
;;;      #+never
;;;      (if t
;;;          (loop for row from 0 to 1
;;;                do (loop for col from 0 to 3
;;;                         do (setf (aref new-3d-to-camera-matrix row col)
;;;                                  (* scale (aref new-3d-to-camera-matrix row col)))))
;;;          (progn
;;;            (loop for row from 0 to 2
;;;                  do (loop for col from 0 to 3
;;;                           do (setf (aref new-3d-to-camera-matrix row col)
;;;                                    (* scale (aref new-3d-to-camera-matrix row col)))))
;;;            (loop for row from 0 to 3
;;;                  do (loop for col from 2 to 2
;;;                           do (setf (aref camera-to-2d-matrix row col) 
;;;                                    (/ (aref camera-to-2d-matrix row col) scale ))))
;;;            ))
;;;      ;;(setq *lvcs-to-lvcs-transform* (make-4x4-coordinate-transform  lvcs-to-lvcs-matrix))
;;;      (values (multiply-matrices camera-to-2d-matrix new-3d-to-camera-matrix)
;;;              camera-to-2d-matrix 
;;;              new-3d-to-camera-matrix
;;;              lvcs-to-lvcs-matrix))))

;;; We assume that 3d-coordinate-system is a LOCAL-VERTICAL-COORDINATE-SYSTEM
(defmethod transform-3d-to-camera-matrix-to-camera-centered-lvcs 
	   (3d-to-camera-matrix 3d-coordinate-system &key (local-units-per-meter 1.0))
    (let* ((initial-lvcs 3d-coordinate-system)
	   (lvcs-to-gcc (lvcs-to-geocentric-transform initial-lvcs))
	   (camera-to-3d-matrix (invert-matrix 3d-to-camera-matrix))
	   (lvcs-to-lat-long-transform (lvcs-to-lat-long-transform initial-lvcs))
	   (lat-long-cs (to-coordinate-system lvcs-to-lat-long-transform))
	   ;;(gcc-to-gdc-transform (gcc-to-gdc-transform (ellipsoid lat-long-cs)))
	   (lvcs-origin (cv (aref camera-to-3d-matrix 0 3)
			    (aref camera-to-3d-matrix 1 3)
			    (aref camera-to-3d-matrix 2 3)))
	   (geo-origin (transform-vector lvcs-to-lat-long-transform lvcs-origin))
	   (camera-lvcs (bind-vector-elements (long lat) geo-origin
			  (make-lvcs lat-long-cs lat long :units local-units-per-meter)))
	   (camera-lvcs-to-gcc (to-geocentric-transform camera-lvcs))
	   (lvcs-to-lvcs-matrix (multiply-matrices (invert-matrix (transform-matrix lvcs-to-gcc))
						   (transform-matrix camera-lvcs-to-gcc)))
	   (new-3d-to-camera-matrix (multiply-matrices 3d-to-camera-matrix lvcs-to-lvcs-matrix)))
      (values new-3d-to-camera-matrix camera-lvcs lvcs-to-lvcs-matrix)))


(defmethod transform-to-camera-centered-lvcs 
	   ((frame-camera 4x4-projection) &key (local-units-per-meter 1.0))
  (mv-bind (camera-to-2d-matrix 3d-to-camera-matrix) 
      (interior-and-exterior-matrices frame-camera)
    (mv-bind (3d-to-camera-matrix camera-lvcs)
	(transform-3d-to-camera-matrix-to-camera-centered-lvcs 
	 3d-to-camera-matrix (from-coordinate-system frame-camera)
	 :local-units-per-meter local-units-per-meter)
      (let* ((2d-world (to-coordinate-system frame-camera))
	     (3d-to-2d-projection
	      (make-instance 'frame-camera 
			     :3d-to-camera-matrix 3d-to-camera-matrix
			     :camera-to-2d-matrix camera-to-2d-matrix
			     :from-coordinate-system camera-lvcs
			     :to-coordinate-system 2d-world)))
	;; We have a problem here:  
	;; Do we return the 3d-to-2d-projection, or do we modify the 2d-world
	;; and return the 2d-world?
	3d-to-2d-projection
	#+never
	(when nil
	  (setf (3d-to-2d-projection 2d-world) 3d-to-2d-projection)
	  2d-world)
	))))


#|
(defun print-projmat(pm)
  (format t "{")
  (loop for i from 0 below 4 do
    (format t "{"~12,6f, "~12,6f, "~12,6f, "~12,6f}"
	    (aref pm i 0) (aref pm i 1) (aref pm i 2) (aref pm i 3))
    (if (< i 3) (format t ",~%")))
  (format t "}~%"))

(setq proj 
      (cme::MAKE-PERSPECTIVE-TRANSFORM
       :focal-length -5067.3154
       :principal-point-u 1665.6791
       :principal-point-v 1986.6626
       :transform-matrix
       (cme::make-and-fill-2d-array
	'((0.9996994 0.013477996 -0.021072589 1812.722)
	  (-0.014055679 0.9992905 -0.03458094 2137.357)
	  (0.020597093 0.03486902 0.9991798 21716.98)
	  (0.0 0.0 0.0 1.0)))
       :near 10000.0 :far 20000.0		
       ))
(inspect proj)
(inspect (projection-matrix proj))
  0  0        1.006603540097247
  0  1        -0.002834611658267497
  0  2        -0.30784914476290354
  0  3        4866.919914490998
  1  0        0.021593482935399755
  1  1        1.0128711592805353
  1  2        -0.3568658961351635
  1  3        5546.039285089431
  2  0        8.317062215446898E-6
  2  1        1.36486209150242E-5
  2  2        -3.9436251864137664E-4
  2  3        4.573251366195164
  3  0        4.158531107723449E-6
  3  1        6.8243104575121E-6
  3  2        -1.9718125932068832E-4
  3  3        4.2600572164890105

(inspect (CAMERA-TO-2D-MATRIX proj))
  0  0        1.0
  0  1        0.0
  0  2        -0.32871036604510545
  0  3        0.0
  1  0        0.0
  1  1        1.0
  1  2        -0.3920542621049402
  1  3        0.0
  2  0        0.0
  2  1        0.0
  2  2        -3.946863066782857E-4
  2  3        -3.9468630667828566
  3  0        0.0
  3  1        0.0
  3  2        -1.9734315333914284E-4
  3  3        0.0

(set-projection-matrix-depth-clipping (projection-matrix proj)
				      :near 10000.0 :far 20000.0)
(progn foo)
(inspect (projection-matrix proj))
  0  0        1.006603540097247
  0  1        -0.002834611658267497
  0  2        -0.30784914476290354
  0  3        4866.919914490998
  1  0        0.021593482935399755
  1  1        1.0128711592805353
  1  2        -0.3568658961351635
  1  3        5546.039285089431
  2  0        8.317062215446898E-6
  2  1        1.36486209150242E-5
  2  2        -3.9436251864137664E-4
  2  3        -3.94686273495732
  3  0        4.158531107723449E-6
  3  1        6.8243104575121E-6
  3  2        -1.9718125932068832E-4
  3  3        4.2600572164890105

|#
