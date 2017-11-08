(in-package :transforms)

;;; This depends on both the IMAGE and TRANSFORMS subsystems.

;;(import '(img::image-x-dim img::image-y-dim))

;;; RPC-PROJECTION is a base-class.  DPPDB-RPC-PROJECTION and
;;; 20EAA-RPC-PROJECTION specialize this and provide the transform-vector
;;; functions.

(defstruct-class rpc-projection (coordinate-projection)       
  ((x-num-coeffs :initarg :x-num-coeffs :accessor x-num-coeffs)
   (x-den-coeffs :initarg :x-den-coeffs :accessor x-den-coeffs)
   (y-num-coeffs :initarg :y-num-coeffs :accessor y-num-coeffs)
   (y-den-coeffs :initarg :y-den-coeffs :accessor y-den-coeffs)
   (scales :initarg :scales :accessor scales)
   (offsets :initarg :offsets :accessor offsets))
  (:default-initargs :create-inverse t))

(defmethod bounding-box ((3d-to-2d-projection rpc-projection))
  (with-class-slots rpc-projection (offsets scales) 3d-to-2d-projection
    (apply 'math::coordinate-vector-general 
	   (loop for i from 0 below 3
		 collect (+ (/ -1.0 (aref scales i)) (aref offsets i))
		 collect (+ (/ 1.0 (aref scales i)) (aref offsets i))))))

			   
(defmethod permute-rpc-coeffs ((projection rpc-projection) permutation)
  (with-class-slot-values rpc-projection
	(x-num-coeffs x-den-coeffs y-num-coeffs y-den-coeffs) projection
    (flet ((permute-vector (coeffs permutation)
	     (declare (type (simple-array double-float (*)) coeffs))
	     (let ((perm-coeffs (make-array (length coeffs) :element-type 'double-float)))
	       (declare (type (simple-array double-float (*)) perm-coeffs))
	       (loop for p fixnum in permutation
		     for i fixnum from 0
		     do (setf (aref perm-coeffs i) (aref coeffs p)))
	       (copy-array-contents perm-coeffs coeffs))))
      (permute-vector x-num-coeffs permutation)
      (permute-vector x-den-coeffs permutation)
      (permute-vector y-num-coeffs permutation)
      (permute-vector y-den-coeffs permutation))))



(declaim (type (simple-array double-float (*))
	       *rpc-projection-transform-vector-scaled-x*
	       *rpc-projection-transform-vector-tmp*))

(defparameter *rpc-projection-transform-vector-scaled-x*
  (make-array 3 :element-type 'double-float))

(defparameter *rpc-projection-transform-vector-tmp*
  (make-array 1 :element-type 'double-float))

#|  least-squares-fit-4x4-projection isn't yet defined.
;;; The 2d-world is used to get define a bounding volume in 3d space for the least-squares fit.
(defmethod construct-surrogate-projection ((projection dppdb-rpc-projection) 2d-world)
  (setf (get-prop projection :surrogate-projection)
	(least-squares-fit-4x4-projection projection 2d-world)))

(defmethod construct-surrogate-projection ((projection rpc-projection) uvh-box)
  (setf (get-prop projection :surrogate-projection)
	(fit-4x4-projection-to-2d-to-3d-projection projection uvh-box '(9 9 5))))
|#

;;; Uggh -- we have a package order problem here.
;;; The TRANSFORMS system is loaded before the IMAGE system and the CME system.
;;; Thus the next functions refer to packages that do not yet exist.
;;; As an UGLY HACK to work around this problem, this file will be loaded
;;; from the CME system.

;;; lvcs-to-rpc-transform is a transform (or NIL) that connects LVCS coordinate to
;;; the coordinate system defined for the RPC.
;;; This should work correctly for LVCS, UTM and LAT-LONG based RPCs.

;;;(defun setup-rpc-projection (lvcs-to-rpc-transform 
;;;                             rpc 3d-world 2d-world-name image
;;;                             &key near-far (n-list '( 9 9 5) ))
;;;  (let* ((lvcs-to-image-projection
;;;          (if lvcs-to-rpc-transform
;;;              (make-composite-coordinate-projection (list lvcs-to-rpc-transform rpc))
;;;              rpc))
;;;         ;; offsets and scales define the bound-boxes for the RPC domain and range.
;;;         (offsets (dppdb-rpc-projection-offsets rpc))
;;;         (scales (dppdb-rpc-projection-scales rpc))
;;;         (rpc-center (cv (aref offsets 0) (aref offsets 1) (aref offsets 2)))
;;;         (lvcs-center (inverse-transform-vector lvcs-to-rpc-transform rpc-center))
;;;         (zdim/2 (/ (aref scales 2)))
;;;         (udim/2 (/ (aref scales 3)))
;;;         (vdim/2 (/ (aref scales 4)))
;;;         (lvcs-zmin (aref (inverse-transform-vector 
;;;                           lvcs-to-rpc-transform 
;;;                           (cv (aref offsets 0) (aref offsets 1) (- (aref offsets 1) zdim/2)))
;;;                          2))
;;;         (lvcs-zmax (aref (inverse-transform-vector 
;;;                           lvcs-to-rpc-transform 
;;;                           (cv (aref offsets 0) (aref offsets 1) (+ (aref offsets 1) zdim/2)))
;;;                          2))
;;;         (uvh-bbox (cv 0.0 (dfloat (image-x-dim image)) 
;;;                       0.0 (dfloat (image-y-dim image))
;;;                       lvcs-zmin lvcs-zmax))
;;;         #+never
;;;         (uvh-bbox (cv (- (aref offsets 3) udim/2) (+ (aref offsets 3) udim/2)
;;;                       (- (aref offsets 4) vdim/2) (+ (aref offsets 4) vdim/2)
;;;                       lvcs-zmin lvcs-zmax))
;;;         (2d-world (or (2d-world image)
;;;                       (cme::make-2d-world :name 2d-world-name
;;;                                           :3d-world 3d-world
;;;                                           :3d-to-2d-projection lvcs-to-image-projection))))
;;;    (setf (3d-to-2d-projection 2d-world) lvcs-to-image-projection)
;;;    (when lvcs-to-rpc-transform
;;;      (connect-transforms lvcs-to-image-projection
;;;                          (make-Newton-Raphson-Inverse-Transform lvcs-to-image-projection))
;;;      (setf (get-prop (inverse-transform lvcs-to-image-projection) :starting-approx)
;;;            (cv 0.0 0.0 0.0)))
;;;    (cme::setup-image-worlds image :2d-world 2d-world)
;;;    (setf (get-prop lvcs-to-image-projection :surrogate-projection) 
;;;          (fit-4x4-projection-to-2d-to-3d-projection lvcs-to-image-projection uvh-bbox n-list near-far))
;;;    ;; Need to extend fit-4x4-projection-to-2d-to-3d-projection to figure out nea/far parameters
;;;    ;; from the uvh-bbox and resulting 4x4-projection.
;;;    (values lvcs-to-image-projection uvh-bbox)))
    

;;; FIXME -- really only need image-dimensions, not image.



;;; image must be the top of the image-pyramid to which the RPC applies.
#+never ;; requires the IMG package
(defun make-rpc-projection-surrogate (lvcs-to-rpc-transform rpc image &optional near-far n-list)
  (unless n-list (setq n-list '( 9 9 5)))
  (let* ((lvcs-to-image-projection
	  (if lvcs-to-rpc-transform
	      (make-composite-coordinate-projection (list lvcs-to-rpc-transform rpc))
	      rpc))
	 ;; offsets and scales define the bound-boxes for the RPC domain and range.
	 (offsets (dppdb-rpc-projection-offsets rpc))
	 (scales (dppdb-rpc-projection-scales rpc))
	 ;(rpc-center (cv (aref offsets 0) (aref offsets 1) (aref offsets 2)))
	 ;(lvcs-center (inverse-transform-vector lvcs-to-rpc-transform rpc-center))
	 (zdim/2 (/ (aref scales 2)))
	 (udim/2 (/ (aref scales 3)))
	 (vdim/2 (/ (aref scales 4)))
	 (lvcs-zmin (aref (inverse-transform-vector 
			   lvcs-to-rpc-transform 
			   (cv (aref offsets 0) (aref offsets 1) (- (aref offsets 1) zdim/2)))
			  2))
	 (lvcs-zmax (aref (inverse-transform-vector 
			   lvcs-to-rpc-transform 
			   (cv (aref offsets 0) (aref offsets 1) (+ (aref offsets 1) zdim/2)))
			  2))
	 (uvh-bbox (if image
		       (cv 0.0 (dfloat (image-x-dim image)) 
			   0.0 (dfloat (image-y-dim image))
			   lvcs-zmin lvcs-zmax)
		       (cv (- (aref offsets 3) udim/2) (+ (aref offsets 3) udim/2)
			   (- (aref offsets 4) vdim/2) (+ (aref offsets 4) vdim/2)
			   lvcs-zmin lvcs-zmax))))
    (when lvcs-to-rpc-transform
      (connect-transforms lvcs-to-image-projection
			  (make-Newton-Raphson-Inverse-Transform lvcs-to-image-projection))
      (setf (get-prop (inverse-transform lvcs-to-image-projection) :starting-approx)
	    (cv 0.0 0.0 0.0)))

    (setf (get-prop lvcs-to-image-projection :SURROGATE-PROJECTION) 
	  (fit-4x4-projection-to-2d-to-3d-projection lvcs-to-image-projection uvh-bbox 
						     :sampling `(:uniform ,n-list) :near-far near-far))
    ;; Need to extend fit-4x4-projection-to-2d-to-3d-projection to figure out near-far parameters
    ;; from the uvh-bbox and resulting 4x4-projection.

    (values lvcs-to-image-projection uvh-bbox)))

(defun make-rpc-projection-surrogate (lvcs-to-rpc-transform rpc image-bbox &optional near-far n-list)
  (unless n-list (setq n-list '( 9 9 5)))
  (let* ((lvcs-to-image-projection
	  (if lvcs-to-rpc-transform
	      (make-composite-coordinate-projection (list lvcs-to-rpc-transform rpc))
	      rpc))
	 ;; offsets and scales define the bound-boxes for the RPC domain and range.
	 (offsets (dppdb-rpc-projection-offsets rpc))
	 (scales (dppdb-rpc-projection-scales rpc))
					;(rpc-center (cv (aref offsets 0) (aref offsets 1) (aref offsets 2)))
					;(lvcs-center (inverse-transform-vector lvcs-to-rpc-transform rpc-center))
	 (zdim/2 (/ (aref scales 2)))
	 (udim/2 (/ (aref scales 3)))
	 (vdim/2 (/ (aref scales 4)))
	 (lvcs-zmin (aref (inverse-transform-vector 
			   lvcs-to-rpc-transform 
			   (cv (aref offsets 0) (aref offsets 1) (- (aref offsets 1) zdim/2)))
			  2))
	 (lvcs-zmax (aref (inverse-transform-vector 
			   lvcs-to-rpc-transform 
			   (cv (aref offsets 0) (aref offsets 1) (+ (aref offsets 1) zdim/2)))
			  2))
	 (uvh-bbox (if image-bbox
		       (bind-vector-elements (xmin xmax ymin ymax) image-bbox
			 (cv xmin xmax ymin ymax lvcs-zmin lvcs-zmax))		       
		       (cv (- (aref offsets 3) udim/2) (+ (aref offsets 3) udim/2)
			   (- (aref offsets 4) vdim/2) (+ (aref offsets 4) vdim/2)
			   lvcs-zmin lvcs-zmax))))
    (when lvcs-to-rpc-transform
      (connect-transforms lvcs-to-image-projection
			  (make-Newton-Raphson-Inverse-Transform lvcs-to-image-projection))
      (setf (get-prop (inverse-transform lvcs-to-image-projection) :starting-approx)
	    (cv 0.0 0.0 0.0)))

    (setf (get-prop lvcs-to-image-projection :SURROGATE-PROJECTION) 
	  (fit-4x4-projection-to-2d-to-3d-projection lvcs-to-image-projection uvh-bbox 
						     :sampling `(:uniform ,n-list) :near-far near-far))
    ;; Need to extend fit-4x4-projection-to-2d-to-3d-projection to figure out near-far parameters
    ;; from the uvh-bbox and resulting 4x4-projection.

    (values lvcs-to-image-projection uvh-bbox)))
    
;;; REQUIRES 3D-WORLD
(defun make-lat-long-rpc-projection-surrogate (lat-long-rpc 3d-world image &optional near-far n-list)
  (make-rpc-projection-surrogate (lvcs-to-lat-long-transform 3d-world) 
				 lat-long-rpc image near-far n-list))

(defun make-utm-rpc-projection-surrogate (utm-rpc 3d-world image &optional near-far n-list)
  (let (;(central-meridian nil)
	(central-meridian (aref (dppdb-rpc-projection-offsets utm-rpc) 0))
	)
  (make-rpc-projection-surrogate (to-utm-transform 3d-world central-meridian)
				 utm-rpc image near-far n-list)))

(defun make-lvcs-rpc-projection-surrogate (lvcs-rpc image &optional near-far n-list)
  (make-rpc-projection-surrogate nil lvcs-rpc image near-far n-list))



(define-soft-slot rpc-projection :surrogate-projection surrogate-projection)

;;; FIXME -- really only need image-dimensions and (3d-to-2d-projection image), not the image.
#+never
(defun build-surrogate-projection (image zmin zmax near-far)
  (let* ((projection (3d-to-2d-projection image))
	 (surrogate (fit-4x4-projection-to-2d-to-3d-projection 
		     projection
		     (cv 0.0 (float (image-x-dim image))
			 0.0 (float (image-y-dim image))
			 zmin zmax)
		     :sampling '(:uniform 9 9 5)
		     :near-far near-far)))
    (setf (get-prop projection :surrogate-projection) surrogate)))



(defun build-surrogate-projection (projection image-bbox zmin zmax near-far)
  (let* ((surrogate (fit-4x4-projection-to-2d-to-3d-projection 
		     projection
		     (bind-vector-elements (xmin xmax ymin ymax) image-bbox
		       (cv xmin xmax ymin ymax zmin zmax))
		     :sampling '(:uniform 9 9 5)
		     :near-far near-far)))
    (setf (get-prop projection :surrogate-projection) surrogate)))


