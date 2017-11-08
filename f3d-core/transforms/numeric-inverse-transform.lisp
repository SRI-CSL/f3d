(in-package :transforms)

#|       NUMERICAL-INVERSE-TRANSFORMS and INTERSECT-RAY-WITH-PLANE

The file contains two primary classes:

Class NEWTON-RAPHSON-INVERSE-TRANSFORM
   For computing an inverse-transform by Newton Raphson iteration of the
       forward-transform function.  Useful when no approximation is available
       for the inverse of the 

Class ITERATIVE-INVERSE-TRANSFORM
   For computing an inverse-transform using an approximate inverse-transform
       without needing to compute a JACOBIAN matrix.

Iterative-Inverse-Transform is generally much faster than
Newton-Raphson-Inverse-Transform when the approximate-inverse-transform is
good, since we immediately get a good starting approximation for ray intersection.

___________________________________________________________________


Fri Jul  9 1999  MAJOR CHANGE UNDERWAY FOR RAY INTERSECTION.

To make it easier to determine the starting approximation for Newton Raphson ray
intersection, we will intersect camera rays with convex planar polygons defined by
n vertices instead of an infinite plane.  The starting approximation is
determined as follows:

  starting-approx = vertex-mean
  
    the average of the coordinates of the polygon vertices.


NEWTON-RAPHSON-INTERSECT-RAY-WITH-POLYGON also makes sense for the following
    other reasons:

   1. Usually we are only interested in points of intersection inside the convex
      region defined by the polygon.  NEWTON-RAPHSON-INTERSECT-RAY-WITH-POLYGON
      can also perform the inside test.

   2. We can intersect camera rays with DTED defined in GDC (or UTM) coordinate
      much more efficiently if we intersect with the polygons in LVCS
      coordinates defined by projecting DTED vertex coordinates to LVCS
      coordinates.  Unless the DTED post spacing is extremely large, this
      intersection will be sufficiently accurate.
  

The code changes required for NEWTON-RAPHSON-INTERSECT-RAY-WITH-POLYGON:

  1.  INTERSECT-CAMERA-RAY-WITH-PLANE ALWAYS requires an explicit	
      starting-approximation.  

      No problem: NEWTON-RAPHSON-INTERSECT-RAY-WITH-POLYGON computes vertex-mean.

      (We could alternatively parameterize INTERSECT-CAMERA-RAY-WITH-PLANE
      to define the PLANE by the plane NORMAL vector and VERTEX-MEAN.)

  
___________________________________________________________________


|#

(defvar *intersect-camera-ray-with-z-plane-tmp* (cv 0.0 0.0 -1.0 0.0))

;(fmakunbound 'intersect-camera-ray-with-z-plane)
;;; Be careful with oblique projections.  The camera ray might be parallel to the z-plane.
(defmethod intersect-camera-ray-with-z-plane (projection 2d-pos z &optional starting-approx)
  (declare (double-float z))
  (let ((plane *intersect-camera-ray-with-z-plane-tmp*))
    (declare (type (simple-array double-float (4)) plane))
    (setf (aref plane 3) z)
    (intersect-camera-ray-with-plane projection 2d-pos plane starting-approx)))

(defmethod intersect-camera-ray-with-plane (projection 2d-pos plane
						       &optional starting-approx)
  (inverse-intersect-ray-with-plane (inverse-transform projection) 2d-pos plane starting-approx))

;;; This assumes 2d-to-3d-transform is a reasonable approximation to
;;; the inverse of (inverse-transform 2d-to-3d-transform)
;;; INVERSE-INTERSECT-RAY-WITH-PLANE is specialized for 4X4-PROJECTION.
(defmethod inverse-intersect-ray-with-plane (2d-to-3d-transform 2d-pos plane &optional starting-approx)
  (iterative-inverse-intersect-ray-with-plane
   (inverse-transform 2d-to-3d-transform)
   (or (and (not (listp 2d-to-3d-transform)) (get-prop 2d-to-3d-transform :approximate-transform))
       2d-to-3d-transform)
   2d-pos
   plane
   starting-approx))

;;; FIXME:  Move APPROXIMATE-TRANSFORM methods to elsewhere.

#+never ; unfinsihed
(defmethod construct-approximate-transform (transform)
  (make-4x4-transform-approximation transform transform bbox)) ; what boox?

(defmethod construct-approximate-transform (transform)
  )


(defmethod approximate-transform (transform)
  (if (linear-transform-p transform)
      transform
      (or (get-prop transform :approximate-transform)
	  (setf (get-prop transform :approximate-transform)
		(construct-approximate-transform transform)))))

#+not-needed
(defmethod approximate-transform ((transform 4x4-coordinate-transform))
  transform)

(defmethod approximate-transform ((transform 4x4-projection))
  transform)

(defmethod approximate-transform ((transform coordinate-projection))
  (or (surrogate-projection transform)
      (setf (surrogate-projection transform)
	    (construct-surrogate-projection transform (to-coordinate-system transform)))))

(defmethod approximate-transform ((transform composite-coordinate-transform))
  (or (get-prop transform :approximate-transform)
      (let ((approx-transforms (approximate-transform (transform-list transform))))
	(when approx-transforms
	  (setf (get-prop transform :approximate-transform)
		(compose-coordinate-transform-list approx-transforms))))))

;#+unfinished
(defmethod approximate-transform ((transform composite-coordinate-projection))
  (or (get-prop transform :approximate-transform)
      (with-class-slots composite-coordinate-projection (3d-transforms simple-projection 2d-transforms)
	  transform
	(setf (get-prop transform :approximate-transform)
	      (remove nil `(,(approximate-transform 3d-transforms)
			     ,(approximate-transform simple-projection)
			     ,(approximate-transform 2d-transforms)))))))

(defmethod approximate-transform ((transform list))
  (loop for trans in (flatten-coordinate-transform-list transform)
	for approx-trans = (approximate-transform trans)
	unless approx-trans
	  do (break "approximate-transform returned NIL")
	  ;return nil   ; this looks bogus
	collect approx-trans))

(defmethod intersect-camera-ray-with-plane ((projection list) 2d-pos plane
					    &optional starting-approx)
  (let ((approx-inverse-transform (approximate-transform (inverse-transform projection))))
    (if approx-inverse-transform
	(iterative-inverse-intersect-ray-with-plane projection approx-inverse-transform
						    2d-pos plane starting-approx)
	(Newton-Raphson-intersect-ray-with-plane projection 2d-pos plane starting-approx))))

;;; FIXME: The name INTERSECT-CAMERA-RAY-WITH-POLYGON is misleading.  It does
;;; not test that the intersection point is contained inside the polygon.  Call
;;; POINT-INSIDE-POLYGON to test the resulting point.

;;; INTERSECT-CAMERA-RAY-WITH-POLYGON can be specialized by 4x4-projection to
;;; avoid the need for computing polygon-center.  The needs more smarts for
;;; composite projections to handle transforms in the list on each side of the
;;; projection.
(defmethod INTERSECT-CAMERA-RAY-WITH-POLYGON (projection 2d-pos polygon)
  (let ((polygon-center (load-time-value* (make-coordinate-vector 3)))
	(plane (load-time-value* (make-coordinate-vector 4))))
    (declare (type (simple-array double-float (*)) plane))
    (math::polygon-center polygon polygon-center)
    (destructuring-bind (v0 v1 v2) polygon
      (math::triangle-normal v0 v1 v2 plane)
      (bind-vector-elements (nx ny nz) plane
	(bind-vector-elements (xc yc zc) polygon-center
	  (setf (aref plane 3) (- (inline-inner-prod (nx ny nz) (xc yc zc)))))))
    (intersect-camera-ray-with-plane projection 2d-pos plane polygon-center)
    ;; should this test that the intersection is inside the polygon?
    ))



(defparameter *Numerical-Inverse-Transform-default-max-iters* 10)
(defparameter *Numerical-Inverse-Transform-eps-u* 1e-4)
(defparameter *Numerical-Inverse-Transform-eps-x* 1e-4)
(defparameter *Numerical-Inverse-Transform-Jacobian-update-freq* 1)

;;; This is an abstract class.
(defstruct-class Numerical-Inverse-Transform
    (coordinate-transform)
  ((approx-transform :initform nil
			     :initarg :approx-transform 
			     :accessor approx-transform)
   (max-iterations :initform *Numerical-Inverse-Transform-default-max-iters*
		   :initarg :max-iterations :accessor max-iterations)
   (eps-u :initform *Numerical-Inverse-Transform-eps-u*
	  :initarg eps-u :accessor eps-u)
   (eps-x :initform *Numerical-Inverse-Transform-eps-x*
	  :initarg eps-x :accessor eps-x)
   ))

(defmethod initialize-instance :after ((transform Numerical-Inverse-Transform)
				       &key inverse-transform &allow-other-keys)
  (with-class-slots coordinate-transform (transform-function) transform
    (connect-transforms transform inverse-transform)))

;;; TRANSFORM-JACOBIAN is expensive since we must compute transform-vector
;;; of the Numerical-Inverse-Transform in order to compute the JacobianMatrix of
;;; the inverse-transform.
(defmethod transform-jacobian
	   ((transform Numerical-Inverse-Transform) position &optional jacobian) 
  (let ((to-point (transform-vector transform position))) ; EXPENSIVE
    (when to-point
      (let ((inv-jacobian
	     (transform-jacobian (inverse-transform transform) to-point)))
	(when inv-jacobian
	  (invert-3x3-matrix inv-jacobian jacobian))))))

;;; **********************  Newton-Raphson-Inverse-Transform  **********************

#|
Newton-Raphson-Inverse-Transform is the appropriate class to use for the inverse
of a transform for which we have no reasonable approximation to the inverse.
It the transform has an approximate-inverse, use the class
ITERATIVE-INVERSE-COORDINATE-TRANSFORM instead.
|#

(defstruct-class Newton-Raphson-Inverse-Transform
    (Numerical-Inverse-Transform)
  ((starting-approx :initform nil :initarg :starting-approx :accessor starting-approx)
   (Jacobian-update-freq :initarg :Jacobian-update-freq :accessor Jacobian-update-freq
    :initform *Numerical-Inverse-Transform-Jacobian-update-freq*)
  )
  ;;(:default-initargs :transform-function #'Newton-Raphson-Inverse-Transform-transform-vector)
  )

;; something wrong with :default-initargs in defstruct-class
(defmethod initialize-instance :after ((projection Newton-Raphson-Inverse-Transform)
				       &key &allow-other-keys)
  (with-class-slots coordinate-transform (transform-function) projection
    (setf transform-function  #'Newton-Raphson-Inverse-Transform-transform-vector)))

(defun make-Newton-Raphson-Inverse-Transform (transform &rest args)
  (apply 'make-instance 'Newton-Raphson-Inverse-Transform
	 :inverse-transform transform args))
    

#|
Use Newton Raphson iteration to INVERT a coordinate-transform or coordinate-projection

Given transformed-point = <x' y' z>, SOLVE for (cv x y z) such that

   (transform-vector transform (cv x y z)) = (cv x' y' z)

Unfinished:  Can there be multiple solutions?  What about the starting approximation?

This is used in transforms/generic-projections.lisp (method project-to-world t)
           and cme/3d-objects.lisp (method fixup-cumulative-ray-direction-errors ...)
|#

;;; Allow this to be specialized by coordinate-projections (or transforms)
;;; that know more about computing a starting approximation.
;;; For instance, rpc-projection (and fbip-projection) knows the 3d-world
;;; bounding-box, and can threrby compute the center of the 3d-bbox.
(defmethod Newton-Raphson-inverse-transform-vector-starting-approx
	   (transform approx)
  (let ((approx0 (get-prop transform :starting-approx)))
    (and approx0
	 (if approx
	     (vector-copy approx0 approx)
	     approx0))))

(define-coordinate-transform-method transform-vector
    ((transform Newton-Raphson-Inverse-Transform) from-vector to-vector)
  (with-class-slot-values Newton-Raphson-Inverse-Transform
	(inverse-transform starting-approx approx-transform
			   max-iterations eps-u eps-x Jacobian-update-freq)
      transform
    (Newton-Raphson-inverse-transform-vector
     inverse-transform			; inverse-transform is the 3d-to-2d-projection
     from-vector to-vector
     ;; approx-transform is an approximation to the 2d-to-3d-transform
     (or starting-approx
	 (and approx-transform
	      (transform-vector approx-transform from-vector to-vector))
	 (Newton-Raphson-inverse-transform-vector-starting-approx transform nil))
     max-iterations eps-u eps-x Jacobian-update-freq)))

(defparameter *Newton-Raphson-inverse-transform-vector-verbose* nil)
(defparameter *Newton-Raphson-inverse-transform-vector-very-verbose* nil)

(defun Newton-Raphson-inverse-transform-vector
    (transform from-vector &optional to-vector starting-approx
     (max-iterations *Numerical-Inverse-Transform-default-max-iters*)
     (eps-u *Numerical-Inverse-Transform-eps-u*)
     (eps-x *Numerical-Inverse-Transform-eps-x*)
     (Jacobian-update-freq *Numerical-Inverse-Transform-Jacobian-update-freq*))
  ;; (setq foo (list transform from-vector starting-approx)) (break)
  (declare (double-float eps-u eps-x)
	   (fixnum max-iterations Jacobian-update-freq))
  (when from-vector
    (let ((verbose *Newton-Raphson-inverse-transform-vector-verbose*)
	  (very-verbose *Newton-Raphson-inverse-transform-vector-very-verbose*)
	  (transformed-point from-vector)
	  (Xi (or to-vector (make-coordinate-vector 3)))
	  (pt2 (load-time-value* (make-coordinate-vector 3)))
	  (Jacobian (load-time-value*
		     (make-array0 '(3 3) :element-type 'double-float)))
	  (Jacobian-inv (load-time-value*
			 (make-array0 '(3 3) :element-type 'double-float))))
      (declare (type (simple-array double-float (3 3)) Jacobian Jacobian-inv))
      (declare (type (simple-array double-float (3)) Xi pt2))
      (macrolet ((convergence-test (ct du dv dw)
		   `(and (< (abs ,du) ,ct) (< (abs ,dv) ,ct) (< (abs ,dw) ,ct))))

	;; Set Xi to starting approximation.
	(or (and starting-approx (vector-copy starting-approx Xi))
	    (error ";;; Newton-Raphson-inverse-transform-vector has no starting approximation~%"))
      
	(bind-vector-elements (u v w) transformed-point
	  (loop for iter fixnum from 1 below max-iterations
		with Jacobian-use fixnum = Jacobian-update-freq
		unless (transform-vector transform Xi pt2)
		  do (when verbose
		       (format t ";;; Newton-Raphson-inverse-transform-vector transform-vector failed~%"))
		     (return (values nil "transform-vector failed"))
		do
	     (bind-vector-elements (ui vi wi) pt2
	       (let* ((du (- u ui)) (dv (- v vi)) (dw (- w wi))
		      (dx 0.0d0) (dy 0.0d0) (dz 0.0d0))
		 (declare (double-float du dv dw dx dy dz))
		 (when (convergence-test eps-u du dv dw)
		   (when verbose
		     (format t ";;; Newton-Raphson-inverse-transform-vector ~a iters~%" iter))
		   (return (values Xi iter :err-u-limit)))
		   
		 (when (or (eql iter 2) (>= Jacobian-use Jacobian-update-freq))
		   ;; Avoid recomputing the Jacobian every time
		   (unless (transform-jacobian transform Xi Jacobian)
		     (when verbose
		       (format t ";;; Newton-Raphson-inverse-transform-vector transform-jacobian~% failed~%"))
		     (return (values nil "transform-jacobian failed")))
		   (unless (invert-3x3-matrix Jacobian Jacobian-inv)
		     (when verbose
		       (format t ";;; Newton-Raphson-inverse-transform-vector invert-3x3-matrix failed~%"))
		     (return (values nil "invert-3x3-matrix failed")))
		   (setq Jacobian-use 0))
	       
		 (incf Jacobian-use)
		 (math::inline-matrix-times-vector Jacobian-inv (du dv dw) (dx dy dz))
		 (when very-verbose
		   (format t ";;; Newton-Raphson-inverse-transform-vector Xi = ~a~%"
			   Xi)
		   (format t ";;; Newton-Raphson-inverse-transform-vector dX = ~a~%"
			   (list dX dy dz))
		   (format t ";;; Newton-Raphson-inverse-transform-vector dU = ~a~%"
			   (list du dv dw))
		   (pprint (math::list-2d-array Jacobian))
		   (pprint (math::list-2d-array Jacobian-inv))
		   )
		 (incf (aref Xi 0) dx) (incf (aref Xi 1) dy) (incf (aref Xi 2) dz)
		 (when (convergence-test eps-x dx dy dz)
		   (when verbose
		     (format t ";;; Newton-Raphson-inverse-transform-vector ~a iters~%" iter))
		   (return (values Xi iter :err-x-limit)))
		 ))
	  
		finally
	     (format t ";;; Warning: Newton-Raphson-inverse-transform-vector FAILED TO CONVERGE for ~a ~a~%"
		     transform transformed-point))
	  )))))

;;; *****************  Newton-Raphson-intersect-ray-with-plane  *****************
#|
Newton Raphson implementation of INTERSECT-RAY-WITH-PLANE.
This is appropriate when we do not have an approximation to the 2d-to-3d trransform.
Otherwise, use iterative-inverse-intersect-ray-with-plane.
We are given:

   Projection P, 
   Image position <u',v'>,
   The equation of a Plane as the 4-vector <Nx, Ny, Nz, N1>.

We are trying to solve for <x,y,z> such that:

  P(x,y,z) = <u',v'>

  <x,y,z,1> . Plane = 0

This generalizes to other surfaces using the characteristic equation of the surface.
The last constraint becomes:

  surface->characteristic_equation(x,y,z) = 0.

NOTE: NO ATTEMPT IS MADE HERE TO DETERMINE IF THE POINT OF INTERSECTION IS 
      BEHIND THE CAMERA.

_________________________________________________________________

|#

(defparameter *intersect-ray-with-plane-max-iters* 10)
(defparameter *intersect-ray-with-plane-convergence-threshold* 1e-3)
;(setq *intersect-ray-with-plane-convergence-threshold* 1e-2)
;(setq *Newton-Raphson-inverse-transform-vector-verbose* 3)
;(setq *Newton-Raphson-inverse-transform-vector-verbose* 2)
;(setq *Newton-Raphson-inverse-transform-vector-verbose* t)
;(setq *Newton-Raphson-inverse-transform-vector-verbose* nil)


;;; Newton-Raphson based ray intersection.
;;; Projection is a 3d-to-2d-projection.
;;; This doesn't work well unless starting-approx is reasonable because
;;; we can easily move outside the domain of the projection, causing the
;;; iteration to fail.

;;; LHQ Wed Dec  1 2004:  Really crummy convergence with dtm-to-image projection when
;;; using a bad STARTING-APPROX. About 9 iteractions and about 1 or 2 bits per iteration.  
;;; Works much better when using INVERSE-TRANSFORM-VECTOR.  The STARTING-APPROX maps to the 
;;; correct 2d-pos.
#|
;(let ((*Newton-Raphson-inverse-transform-vector-verbose* 3)) (apply 'Newton-Raphson-intersect-ray-with-plane *foo9*))
;(let () (apply 'intersect-camera-ray-with-plane *foo9*)) => #(2.0 2.4652949255581005 2014.9589320076618)
(#<COMPOSITE-COORDINATE-PROJECTION (#<ARRAY-IMAGE 21x23 32-bit block-dims RASTER>
 to #<2D-WORLD "fhn711" #X61EF961D>) #X5AAF1F65>
 #(2069.8984368272204 4946.050090099405 841.7716811850283) #(-1.0 0.0 0.0 2.0)
 #(15.47679188674265 -1.3399537914582595 290.1826727464795))

(destructuring-bind (p uv plane est) *foo9*
  (let ((plane ;(cv 0.0 0.0 -1.0 0.0)
	 (cv -1.0 0.0 0.0 2.0))
	(uv (cv 2069.8984368272204 4946.050090099405 841.7716811850283))
	(est (cv 15.47679188674265 -1.3399537914582595 290.1826727464795))
	(*Newton-Raphson-inverse-transform-vector-verbose* 1))
    (list (intersect-camera-ray-with-plane p uv plane est)
	  (Newton-Raphson-intersect-ray-with-plane p uv plane))))
|#

(defun Newton-Raphson-intersect-ray-with-plane
    (projection 2d-pos plane &optional starting-approx
     (eps-u *intersect-ray-with-plane-convergence-threshold*)
     (max-iterations *intersect-ray-with-plane-max-iters*))
  (declare (fixnum max-iterations) (double-float eps-u))
  ;;(break)
  (macrolet ((fail (msg)
	       `(progn
		  #+never
		  (setq *foo9* (list projection 
				     (copy-coordinate-vector 2d-pos) 
				     (copy-coordinate-vector plane)
				     (copy-coordinate-vector starting-approx)))
		  ;(break)
		  (when verbose
		    (format t "Newton-Raphson-intersect-ray-with-plane ~a~%" ,msg))
		  (return-from Newton-Raphson-intersect-ray-with-plane (values nil ,msg)))))
    (bind-vector-elements (u v) 2d-pos
      (bind-vector-elements (px py pz p1) plane
	(let* ((verbose *Newton-Raphson-inverse-transform-vector-verbose*)
	       (*transform-dammit* t)
	       (very-verbose (and (numberp verbose) (> verbose 1) ))
					;(*transform-jacobian-eps* 1e-4)
	       (jacobian (make-array '(3 3) :element-type 'double-float))
	       (jinv (make-array '(3 3) :element-type 'double-float))
	       ;; ;FIXME:  when no starting-approx is provided, this usually fails.  
;;;  inverse-transform-vector is too weak.
	       (Xi (make-coordinate-vector 3))
	       (Ui (load-time-value* (make-coordinate-vector 3))))
	  (declare (type dmatrix jacobian jinv))
	  (declare (type coordinate-vector Xi Ui))
	  ;; Works really poorly when a bad starting-approx is used.
	  (if nil ;starting-approx 
	      (copy-coordinate-vector starting-approx Xi)
	      (unless (inverse-transform-vector projection 2d-pos Xi)
		(fail "CANNOT COMPUTE A STARTING POSITION")))
	  (macrolet ((convergence-test (ct du dv dw)
		       `(and (< (abs ,du) ,ct) (< (abs ,dv) ,ct) (< (abs ,dw) ,ct))))
	    (when verbose 
	      (format t "~%Newton-Raphson-intersect-ray-with-plane START Xi= ~a Plane=~a~%" Xi plane))
	    (loop with (du dv dw) double-float
		  with (dx dy dz) double-float
		  for iter fixnum from 1
		  when (> iter max-iterations)
		    do (fail "FAILED TO CONVERGE")
		  do (unless (transform-vector projection Xi Ui)
		       (fail "TRANSFORM-VECTOR FAILED"))
		     (bind-vector-elements (x y z) Xi
		       (setf du (- u (aref Ui 0))
			     dv (- v (aref Ui 1))
			     dw (- (+ (inline-inner-prod (x y z) (px py pz)) p1))))
		  until (convergence-test eps-u du dv dw)
		  ;; With a little smarts we could avoid calling transform-jacobian
		  ;; more than 1 or 2 times.
		  do (transform-jacobian projection Xi jacobian)
		     (setf (aref jacobian 2 0) px
			   (aref jacobian 2 1) py
			   (aref jacobian 2 2) pz)
		     (invert-3x3-matrix jacobian jinv)
		     (math::inline-matrix-times-vector jinv (du dv dw) (dx dy dz))
		     (incf (aref Xi 0) dx) (incf (aref Xi 1) dy) (incf (aref Xi 2) dz)
		     (when very-verbose 
		       (format t "Newton-Raphson-intersect-ray-with-plane Xi= ~a dU=~a~%" 
			       Xi (cv du dv dw))
		       (when (> verbose 2)
			 (pprint (math::list-2d-array jacobian))
			 (pprint (math::list-2d-array jinv))
			 (terpri)))
		  finally 
	       (when verbose 
		 (format t "Newton-Raphson-intersect-ray-with-plane returned ~a ~a iters" Xi iter))
	       (return (values Xi iter)))))))))

;;;(disassemble 'Newton-Raphson-intersect-ray-with-plane)
   
(defmethod inverse-intersect-ray-with-plane
           ((transform Newton-Raphson-Inverse-Transform) 2d-pos plane
	    &optional starting-approx0)
  (with-class-slot-values Newton-Raphson-Inverse-Transform
        (inverse-transform starting-approx approx-transform) transform
    (Newton-Raphson-intersect-ray-with-plane
     inverse-transform 2d-pos plane
     (or starting-approx0
	 (and approx-transform
	      (transform-vector approx-transform 2d-pos))
         (and starting-approx (vector-copy starting-approx))))))


#|
(disassemble 'Newton-Raphson-inverse-transform-vector)
(transform-vector (3d-to-2d-projection (top-view)) (cv 0.0 0.0 7849.702289325509))
(origin (gui::selected-object))
(transform-vector (3d-to-2d-projection (top-view)) (origin (gui::selected-object)))
(3d-to-2d-projection (top-view))

(setq inv-proj-4x4
      (make-instance '4x4-projection
		     :create-inverse nil
		     :projection-matrix
		     (invert-matrix (projection-matrix (3d-to-2d-projection (top-view))))
		     ))
(setq pt (origin (gui::selected-object)))
(setq inverse-proj
      (make-Newton-Raphson-Inverse-Transform
       (3d-to-2d-projection (top-view))
					;:starting-approx (cv 0.0 0.0 0.0 )
       :starting-approx (cv 342.6919406950001 5107.39358945 6653.853521375)
       ;;:approx-transform inv-proj-4x4
       ))

(connect-transforms (3d-to-2d-projection (top-view)) inv-proj-4x4))

(progn inverse-proj)
(inverse-transform-vector (3d-to-2d-projection (top-view))
		  (transform-vector (3d-to-2d-projection (top-view)) pt))
(CV -71.33299998179996 4541.890000024861 6106.684000024053)

(transform-jacobian inverse-proj
		    (transform-vector (3d-to-2d-projection (top-view)) pt))

(3d-to-2d-projection (top-view))
(let ((proj (3d-to-2d-projection (top-view)))
      (*transform-dammit* t))
  (declare (special *transform-dammit*))
  (intersect-camera-ray-with-plane proj
				   (transform-vector proj pt)
				   (cv 0.0 0.0 -1.0 5901.912)))
;; should be (CV -96.04907172505122 4573.4292596090445  5901.912)

(inverse-transform-vector
 (3d-to-2d-projection (top-view))
 (transform-vector (3d-to-2d-projection (top-view))
		   (cv 342.6919406950001 5107.39358945 6653.853521375)))

(let ((proj (3d-to-2d-projection (top-view)))
      (pt (origin (gui::selected-object))))
  (format t "~a " pt)
  (Newton-Raphson-inverse-transform-vector
   proj (transform-vector proj pt) nil
   ;;(cv 342.6919406950001 5107.39358945 6653.853521375)
   (cv 0.0 0.0 0.0)
   ))

(transform-jacobian 

(setq uvwpt (transform-vector (3d-to-2d-projection (top-view)) (origin tower)))
(Newton-Raphson-invert-transform (3d-to-2d-projection (top-view)) uvwpt)
(origin tower)
(3d-to-2d-projection (top-view))
(transform-jacobian (3d-to-2d-projection (top-view)) (origin tower))
(setq pt (origin ch))
(setq 2dpt (bind-vector-elements (u v)(transform-vector (3d-to-2d-projection (top-view)) pt)
	     (cv u v (aref pt 2))))
(setq approx (vector-add pt (cv 10.0 -10.0 4.0)))
(setq approx nil)
(Newton-Raphson-invert-transform (3d-to-2d-projection (top-view)) 2dpt approx 1e-2 1e-3)
(Newton-Raphson-invert-transform (3d-to-2d-projection (top-view)) 2dpt nil 1e-8 1e-8)

(defun time-Newton-Raphson-invert-transform (n &optional (eps 1e-3) (pup 3))
  (loop repeat n do (Newton-Raphson-invert-transform (3d-to-2d-projection (top-view)) 2dpt approx eps eps pup)))

(time (time-Newton-Raphson-invert-transform 1000 1e-2 6));; 910 usec, 350 bytes consed per pt

(list pt approx)

(bind-vector-elements (u v z) 2dpt
  (multiple-value-bind (x y z)
      (inverse-project-point (3d-to-2d-projection (top-view)) u v z)
    (bind-vector-elements (x2 y2 z2) (Newton-Raphson-invert-transform (3d-to-2d-projection (top-view)) 2dpt nil 1e-8 1e-8)
     (euclidean-length (- x x2) (- y y2)  (- z z2)))))

(3d-to-2d-projection (top-view))
|#



#|
Sat Feb 20 1999  LHQ  --- Nothing in CME-6 uses this --- but FREEDIUS will use it.

ITERATIVE-INVERSE-TRANSFORM:  FORCE THE TRANSFORM AND ITS INVERSE TO HAVE CLOSURE.
   
Ie,

   pt = (ITERATIVE-INVERSE-TRANSFORM (approx-inverse trans)
				     (transform-vector trans pt))

We assume trans is exact and approx-inverse-trans is only an approximation to
the inverse of trans.

We also assume that trans and approx-inverse-trans are nearly linear in the
neighborhood of the point, where the size of the neighborhood is determined by
the closure-error of the pair of transforms.  Basicially, this is the same
assumption required with Newton-Raphson iterative inversion.

We really need a better name for this method, which must exist somewhere in the
numerical analysis literature.

MATHEMATICAL FORMULATION OF THE PROBLEM: *******************************************

NEWTON-RAPHSON INVERSION:

 We have a coordinate transform U = P(X).  

 Given U, compute X such that U = P(X).
   
   1) i=0, pick a starting approximation X[0].

   2) X[i+1] = X[i] + MATRIX-INVERSE(J(P, X[i])) . (U - P(X[i]), 
      where J(P, X) is the Jacobian matrix of P at point X.

   3) i = i+1, repeat steps 2-3 until convergence.  X[i] is the result

ITERATIVE-INVERSE-TRANSFORM INVERSION:

 We have a coordinate transform P such that U = P(X),
   and its approximate inverse transform Pinv such that X =approx Pinv(U). 

 Given U, compute X such that U = P(X).

  1) i=0, U[0] = U

  2) X[i] = Pinv(U[i])

  3) U[i+1] = U[i] + (U - P(X[i]))

  4) i = i+1, repeat steps 2-4 until convergence.  X[i] is the result

  It is easy to show that steps 2 and 3 of ITERATIVE-INVERSE-TRANSFORM INVERSION
approximate step 2 of NEWTON-RAPHSON INVERSION.

  Using the Taylor series expansion of Pinv,
 
      X[i+1] = Pinv(U[i+1]).
   
             = Pinv(U[i]) + J(Pinv,U[i])              . (U[i+1]-U[i])     + O2[A]
     
             = Pinv(U[i]) + MATRIX-INVERSE(J(P,X[i])) . (U[i+1]-U[i])     + O2[B]
      
	     = X[i]       + MATRIX-INVERSE(J(P,X[i])) . (U - P(X[i]))     + O2[B]

             = RHS of step 2 of NEWTON-RAPHSON INVERSION                  + O2[B]

   where O2 means order 2 and higher terms of the Taylor series expansion.

The success of the ITERATIVE-INVERSE-TRANSFORM depends on how well the Jacobian
of Pinv approximates the Matrix-Inverse of the Jacobian of P.  Clearly if Pinv
is a very poor approximation to P, one cannot expect ITERATIVE-INVERSE-TRANSFORM
to converge.  Empirical evidence shows that when Pinv is a good approximation to
the inverse of P, the rate of convergence is quadratic as with NEWTON-RAPHSON INVERSION.

END OF MATHEMATICAL FORMULATION **********************************************

This should work for inverses of rpc-projections and fbip-projections as well as
for inacurate xxx-to-lat-long geographic transforms (The lat-long-to-xxx
geographic transforms are usually exact or highly accurate.  Their inverses
involve approximations to the computation of geodetic latitude, conformal
latitude, or authalic latitude).

The intended manner of use is for the TRANSFORM-FUNCTION of the
inverse-transform of the exact transform to be ITERATIVE-INVERSE-TRANSFORM-VECTOR,
with a slot in the coordinate-transform instance pointing to the
approximation to the inverse-transform.

ITERATIVE-INVERSE-TRANSFORM should be more reliable than using NEWTON-RAPHSON
INVERSION with no APPROXIMATE-TRANSFORM to provide a starting approximation.

ITERATIVE-INVERSE-TRANSFORM is faster to compute than NEWTON-RAPHSON INVERSION
since APPROXIMATE-TRANSFORM provides a good starting approximation, and
we do not have to compute the JACOBIAN-MATRIX of the transform, do matrix
inversion, ...  The rate of convergence should be comparable to that of
NEWTON-RAPHSON INVERSION.

|#

(defstruct-class iterative-inverse-transform
    (Numerical-Inverse-Transform)
    ()
    ;;(:default-initargs :transform-function #'iterative-inverse-transform-vector)
  )

(defmethod initialize-instance :after ((projection iterative-inverse-transform)
				       &key &allow-other-keys)
  (with-class-slots coordinate-transform (transform-function) projection
    (setf transform-function #'iterative-inverse-transform-vector)))



(defun make-iterative-inverse-transform (transform &rest args)
  (apply 'make-instance 'iterative-inverse-transform
	 :inverse-transform transform args))

;;; P is the 3d-to-2d-projection
;;; Pinv is a 2d-to-3d-transform, a good approximation to the inverse of P.
;;; U is a point 2d-space.
(defun iterative-inverse-transform-vector (trans U &optional Xi)
  (when U
    (unless Xi (setq Xi (make-coordinate-vector 3)))
  
    (with-class-slot-values iterative-inverse-transform
	  (approx-transform inverse-transform
				    max-iterations eps-u eps-x) trans
      (declare (fixnum max-iterations) (double-float eps-u eps-x))
      (declare (ignore eps-u))
      (let ((Pinv approx-transform)
	    (P inverse-transform))
	(unless (transform-vector Pinv U Xi)
	  (return-from iterative-inverse-transform-vector
	    (values nil "transform-vector Pinv failed.")))
	(when (> max-iterations 0)
	  (let ((Ui (load-time-value* (make-coordinate-vector 3)))
		(Xi Xi)
		(PXi (load-time-value* (make-coordinate-vector 3))))
	    (declare (type (simple-array double-float (*)) Xi Ui PXi ))
	    (bind-vector-elements (u v w) U
	      (set-coordinate-vector-elements Ui u v w)
	      (macrolet ((convergence-test (ct du dv dw)
			   `(and (< (abs ,du) ,ct)
			     (< (abs ,dv) ,ct)
			     (< (abs ,dw) ,ct))))

		(loop with (du dv dw xp yp zp) double-float
		      for iter fixnum from 1 to max-iterations
		      
		      unless (transform-vector P Xi PXi)
			return (values nil "transform-vector P failed")
		      
		      do (setf du (- u (aref PXi 0))
			       dv (- v (aref PXi 1))
			       dw (- w (aref PXi 2)))
			 
			 ;;  until (convergence-test eps-u du dv dw)

		      do (incf (aref Ui 0) du)
			 (incf (aref Ui 1) dv)
			 (incf (aref Ui 2) dw)

			 ;; save previous approximation Xi-1
		      do (mv-setq-vector-elements (xp yp zp) Xi)
		      unless (transform-vector Pinv Ui Xi)
			return (values nil "transform-vector Pinv failed")
			   
		      ;; This convergence test requires that units in Xi are
		      ;; compatible with ct.  This is usually not a problem,
		      ;; since ground units (meters) are usually isotropic.
		      ;; What about lat-long coordinate-systems?
		      
		      until (convergence-test eps-x
					      (- (aref Xi 0) xp)
					      (- (aref Xi 1) yp)
					      (- (aref Xi 2) zp))
		      finally (return (values Xi iter))
		      )))))))))


#|
ITERATIVE_INVERSE_TRANSFORM RAY INTERSECTION

 We have a coordinate transform P such that U = P(X),
   and its approximate inverse transform Pinv such that X =approx Pinv(U). 
 
 Given U = <u,v,w>, and plane = <Nx, Ny, Nz, N1>, compute X such that 

      u         = Pu(X), 
      v         = Pv(X),
      X . plane = X . <Nx, Ny, Nz> + N1 = 0.

  1) i=0

  2) X'[i] = Pinv(U[i])

  3) CX[i] =  (Pinv(U[i] + <0,0,eps>) - Pinv(U[i]))     -- ray-direction

  4) X[i] = X'[i] + s*CX[i] such that X[i] . plane = 0

        ie,  s = - (X'[i] . <Nx, Ny, Nz> + N1) / (CX[i] . <Nx, Ny, Nz>)

  5) U[i+1] = U[i] + (U - P(X[i]))

  6) i = i+1, repeat steps 2-6 until convergence.  X[i] is the result


|#

(defparameter *iterative-inverse-intersect-ray-with-plane-eps-u* 1e-2)
(defparameter *iterative-inverse-intersect-ray-with-plane-verbose* nil)
;(setq *iterative-inverse-intersect-ray-with-plane-verbose* t)

(defparameter *iterative-inverse-intersect-ray-with-plane-calls* 0)
(defparameter *iterative-inverse-intersect-ray-with-plane-total-iters* 0)

#|
(setq *iterative-inverse-intersect-ray-with-plane-calls* 0 
      *iterative-inverse-intersect-ray-with-plane-total-iters* 0)
(list (dfloat (/ *iterative-inverse-intersect-ray-with-plane-total-iters* 
		 *iterative-inverse-intersect-ray-with-plane-calls*))
      *iterative-inverse-intersect-ray-with-plane-calls*)
|#


;;; P is the 3d-to-2d-projection
;;; Pinv is a 2d-to-3d-transform, a good approximation to the inverse of P.
;;; U is a point 2d-space.
;(disassemble 'iterative-inverse-intersect-ray-with-plane)
;;;(defun iterative-inverse-intersect-ray-with-plane
;;;    (P Pinv U0 plane &optional starting-approx
;;;     (eps-u *iterative-inverse-intersect-ray-with-plane-eps-u*)
;;;     (max-iterations *Numerical-Inverse-Transform-default-max-iters*))
;;;  (declare (optimize (speed 3) (safety #+cmu 0 #-cmu 1)))
;;;  (declare (fixnum max-iterations) (double-float eps-u))
;;;  ;(unless starting-approx (break))
;;;  (flet ((try-newton () 
;;;           (Newton-Raphson-intersect-ray-with-plane P U0 plane starting-approx)))
;;;    (if nil
;;;        (try-newton)
;;;        (when U0
;;;          (let* ((verbose *iterative-inverse-intersect-ray-with-plane-verbose*)
;;;                 (*transform-dammit* t)
;;;                 (linear-depth-p (linear-depth-p P))
;;;                 ;(*4x4-projection-opengl-depth-normalize* t)
;;;                 ;; Xi is returned -- need a new vector
;;;                 (Xi (make-coordinate-vector 3))
;;;                 (Xi+eps (load-time-value* (make-coordinate-vector 3)))
;;;                 (Ui (load-time-value* (make-coordinate-vector 3)))
;;;                 (PXi (load-time-value* (make-coordinate-vector 3))))
;;;            (declare (type coordinate-vector Xi Xi+eps Ui PXi ))
;;;            (if starting-approx
;;;                (copy-coordinate-vector starting-approx Xi)
;;;                (transform-vector Pinv Xi))
;;;            (when (and (numberp verbose) (> verbose 1))
;;;              (format t "~%ENTER iterative-inverse-intersect-ray-with-plane ~a ~a ~a~%"
;;;                      U0 plane starting-approx))
;;;           ; (setq *foo7* Pinv) (break)
;;;            (incf *iterative-inverse-intersect-ray-with-plane-calls*)
;;;            (macrolet ((fail (x msg)
;;;                         `(progn (when verbose
;;;                                   (format t "iterative-inverse-intersect-ray-with-plane FAILED ~a~% ~a ~a~%"
;;;                                           ,msg U0 plane))
;;;                                 ;(setq *foox* (list P Pinv U0 plane starting-approx))
;;;                                 ;(break ,msg)
;;;                                 (return (values ,x ,msg))
;;;                                 ;;(return (try-newton))
;;;                                 ))
;;;                       (convergence-test (ct &rest xs)
;;;                         `(and .,(loop for x in xs
;;;                                       collect `(< (abs ,x) ,ct)))))
;;;              (bind-vector-elements (nx ny nz n1) plane
;;;                (bind-vector-elements (u v w) U0
;;;                  (inline-set-coordinate-vector-elements Ui u v w)
;;;                  (loop with (x y z) double-float
;;;                        with eps double-float = 1e0
;;;                        for w double-float = (aref Ui 2)
;;;                        for iter fixnum from 1
;;;                        when (> iter max-iterations)
;;;                          do (fail nil "iterations exceeded")
;;;                          
;;; ;;; step 2
;;;                        unless (transform-vector Pinv Ui Xi)
;;;                          do ;(setq *foox1* (list Pinv Ui))
;;;                             (fail nil "transform-vector Pinv failed")
;;; ;;; step 3 estimate the direction of the camera ray
;;;                        do (if linear-depth-p
;;;                               (setf (aref Ui 2) (- w eps))
;;;                               (let* ((eps 1e-3)
;;;                                      (s (/ 1.0 (- 1.0 w))))
;;;                                 (declare (double-float w s))
;;;                                 (setf (aref Ui 2) (- 1.0 (/ (- s eps))))))
;;;                        unless (transform-vector Pinv Ui Xi+eps)
;;;                          do ;(setq *foox1* (list Pinv Ui))
;;;                             (fail nil "transform-vector Pinv failed")
;;;
;;;                        do (setf (aref Ui 2) w) ; restore the value 
;;;                       
;;;                           (mv-setq-vector-elements (x y z) Xi)
;;;                         
;;;                           ;; This convergence-test is good if world units are isotropic.
;;;                           ;; That is a problem with lat-long coordinate systems.
;;;                           ;; until (convergence-test eps-x (- x xlast) (- y ylast) (- z zlast))
;;;                    
;;;                        do (let* ((cx (- (aref Xi+eps 0) x))
;;;                                  (cy (- (aref Xi+eps 1) y))
;;;                                  (cz (- (aref Xi+eps 2) z))
;;;;;; step 4                  
;;;                                  (sdenom (inline-inner-prod (cx cy cz) (nx ny nz))))
;;;                             (declare (double-float cx cy cz sdenom))
;;;                             (when (< (abs sdenom) 1e-12 ) ; camera ray in plane causes problems
;;;                               (when verbose
;;;                                 (format t "iterative-inverse-intersect-ray-with-plane: (< (abs sdenom) 1e-6 )~%"))
;;;                               (fail nil "(abs sdenom) 1e-6 )"))
;;;                             (let ((s (- (/ (+ n1 (inline-inner-prod (x y z) (nx ny nz)))
;;;                                            sdenom)))) ; plane equation
;;;                               (declare (double-float s))
;;;                               (incf (aref Xi 0) (* s cx))
;;;                               (incf (aref Xi 1) (* s cy))
;;;                               (incf (aref Xi 2) (* s cz))
;;;                               (when (and (numberp verbose) (> verbose 1))
;;;                                 (format t "iterative-inverse-intersect-ray-with-plane:~% ~a~% Xi=~a~% dX=~a ~%"
;;;                                         (list s eps sdenom) Xi (cv (* s cx) (* s cy) (* s cz))))
;;;
;;; ;;; step 5
;;;                               (unless (transform-vector P Xi PXi)
;;;                                 ;(setq *foox1* (list P Xi))
;;;                                 (fail nil "transform-vector P failed")
;;;                                 )
;;;                               (incf *iterative-inverse-intersect-ray-with-plane-total-iters*)
;;;                               (let ((du (- u (aref PXi 0)))
;;;                                     (dv (- v (aref PXi 1))))
;;;                                 (declare (double-float du dv))
;;;                           
;;;
;;;                                 ;; force eps to a value that causes CX to be a unit vector
;;;                                 (setf eps (/ eps (abs sdenom))) 
;;;
;;;                                 ;; This convergence test is always good.
;;;                                 ;; Since Xi is guaranteed to lie on plane,
;;;                                 ;; we only require PXi be very close to U.
;;;                                 (when (convergence-test eps-u du dv) (loop-finish))
;;;
;;;                                 (incf (aref Ui 0) du)
;;;                                 (incf (aref Ui 1) dv)
;;;                                 (setf (aref Ui 2) (aref PXi 2)) ; use w component at new estimate
;;;                                 )))
;;;                    
;;;                        finally (return (values Xi iter))
;;;                        )))))))))


(defun iterative-inverse-intersect-ray-with-plane
    (P Pinv U0 plane &optional starting-approx
     (eps-u *iterative-inverse-intersect-ray-with-plane-eps-u*)
     (max-iterations *Numerical-Inverse-Transform-default-max-iters*))
  (declare (optimize (speed 3) (safety #+cmu 0 #-cmu 1)))
  (declare (fixnum max-iterations) (double-float eps-u))
  ;;(unless starting-approx (break))
  (flet ((try-newton () 
	   (Newton-Raphson-intersect-ray-with-plane P U0 plane starting-approx)))
    (if nil
	(try-newton)
 	(when U0
	  (let* ((verbose *iterative-inverse-intersect-ray-with-plane-verbose*)
		 (*transform-dammit* t)
		 (linear-depth-p (linear-depth-p P))
					;(*4x4-projection-opengl-depth-normalize* t)
		 ;; Xi is returned -- need a new vector
		 (Xi (make-coordinate-vector 3))
		 (Xi+eps (load-time-value* (make-coordinate-vector 3)))
		 (Ui (load-time-value* (make-coordinate-vector 3)))
		 (PXi (load-time-value* (make-coordinate-vector 3)))
		 (cx 0.0) (cy 0.0) (cz 0.0) (sdenom 0.0)
		 )
	    (declare (type coordinate-vector Xi Xi+eps Ui PXi ))
	    (declare (double-float cx cy cz sdenom))
	    (if starting-approx
		(copy-coordinate-vector starting-approx Xi)
		(transform-vector Pinv Xi))
	    (when (and (numberp verbose) (> verbose 1))
	      (format t "~%ENTER iterative-inverse-intersect-ray-with-plane ~a ~a ~a~%"
		      U0 plane starting-approx))
					; (setq *foo7* Pinv) (break)
	    (incf *iterative-inverse-intersect-ray-with-plane-calls*)
	    (macrolet ((fail (x msg)
			 `(progn (when verbose
				   (format t "iterative-inverse-intersect-ray-with-plane FAILED ~a~% ~a ~a~%"
					   ,msg U0 plane))
					;(setq *foox* (list P Pinv U0 plane starting-approx))
					;(break ,msg)
				 (return (values ,x ,msg))
				 ;;(return (try-newton))
				 ))
		       (convergence-test (ct &rest xs)
			 `(and .,(loop for x in xs
				       collect `(< (abs ,x) ,ct)))))
	      (bind-vector-elements (nx ny nz n1) plane
		(bind-vector-elements (u v w) U0
		  (inline-set-coordinate-vector-elements Ui u v w)
		  (loop with (x y z) double-float
			with eps double-float = 1e0
			for first = t then nil
			for w double-float = (aref Ui 2)
			for iter fixnum from 1
			when (> iter max-iterations)
			  do (fail nil "iterations exceeded")
			  
;;; step 2
			unless (transform-vector Pinv Ui Xi)
			  do ;;(setq *foox1* (list Pinv Ui))
			     (fail nil "transform-vector Pinv failed")
			do (mv-setq-vector-elements (x y z) Xi)
			 
;;; step 3 estimate the direction of the camera ray
			when t ;first
			  ;; If pinv is cheap to compute, we should probably estimate
			  ;; the ray direction at every iteration, otherwise only once.
			  do (progn
			       (if linear-depth-p
				   (setf (aref Ui 2) (- w eps))
				   (let* ((eps 1e-3)
					  (s (/ 1.0 (- 1.0 w))))
				     (declare (double-float w s))
				     (setf (aref Ui 2) (- 1.0 (/ (- s eps))))))
			       (unless (transform-vector Pinv Ui Xi+eps)
					;(setq *foox1* (list Pinv Ui))
				 (fail nil "transform-vector Pinv failed"))
			       (setf (aref Ui 2) w) ; restore the value 
		       
			       ;; This convergence-test is good if world units are isotropic.
			       ;; That is a problem with lat-long coordinate systems.
			       ;; until (convergence-test eps-x (- x xlast) (- y ylast) (- z zlast))
		    
			       (setq cx (- (aref Xi+eps 0) x)
				     cy (- (aref Xi+eps 1) y)
				     cz (- (aref Xi+eps 2) z)
				     sdenom (inline-inner-prod (cx cy cz) (nx ny nz)))

			       (when (< (abs sdenom) 1e-12 ) ; camera ray in plane causes problems
				 (when verbose
				   (format t "iterative-inverse-intersect-ray-with-plane: (< (abs sdenom) 1e-6 )~%"))
				 (fail nil "(abs sdenom) 1e-6 )")))
;;; step 4:
			do (let ((s (- (/ (+ n1 (inline-inner-prod (x y z) (nx ny nz)))
					  sdenom)))) ; plane equation
			     (declare (double-float s))
			     (incf (aref Xi 0) (* s cx))
			     (incf (aref Xi 1) (* s cy))
			     (incf (aref Xi 2) (* s cz))
			     (when (and (numberp verbose) (> verbose 1))
			       (format t "iterative-inverse-intersect-ray-with-plane:~% ~a~% Xi=~a~% dX=~a ~%"
				       (list s eps sdenom) Xi (cv (* s cx) (* s cy) (* s cz))))

;;; step 5
			     (unless (transform-vector P Xi PXi)
					;(setq *foox1* (list P Xi))
			       (fail nil "transform-vector P failed"))
			     (incf *iterative-inverse-intersect-ray-with-plane-total-iters*)
			     (let ((du (- u (aref PXi 0)))
				   (dv (- v (aref PXi 1))))
			       (declare (double-float du dv))
			   

			       ;; force eps to a value that causes CX to be a unit vector
			       (setf eps (/ eps (abs sdenom))) 

			       ;; This convergence test is always good.
			       ;; Since Xi is guaranteed to lie on plane,
			       ;; we only require PXi be very close to U.
			       (when (convergence-test eps-u du dv) (loop-finish))

			       (incf (aref Ui 0) du)
			       (incf (aref Ui 1) dv)
			       (setf (aref Ui 2) (aref PXi 2)) ; use w component at new estimate
			       ))
		    
			finally (return (values Xi iter))
			)))))))))

#|
(destructuring-bind (p pinv u plane approx) *foox*
 (list  u (transform-vector p approx) approx (transform-vector pinv u)))
(destructuring-bind (p pinv u plane approx) *foox*
  (transform-list pinv))

(transform-list (car *foox1*))
(destructuring-bind (p pt0) *foox1*
  (loop for trans in (transform-list p)
	for pt = pt0 then next-pt
	for next-pt = (transform-vector trans pt)
	do (format t "~a ~a~%" next-pt trans)))

(destructuring-bind (p pinv u plane approx) *foox*
  (loop for trans in (transform-list pinv)
	for pt = u then next-pt
	for next-pt = (transform-vector trans pt)
	do (format t "~a ~a~%" next-pt trans)))

(setq *trans* (nth 1 *foox*))
(setq * (nth 0 (transform-list *trans*)))
(nth 2 *foox*) = (cv 2043.8303906401543 4907.345640123765 769.3720483468755) ; image pt
(setq * (nth 4 *foox*)) = (363.4096855544558 66.00904012254614 362.44915132783353) est gndpt

(let* ((proj (nth 0 (transform-list *trans*)))
      (pt (cv 2043.8303906401543 4907.345640123765 769.3720483468755))
      (pt2 (transform-vector proj pt)))
  (list pt2
	(inverse-transform-vector proj pt2)
	pt))

(cv 354.97074962763963 -256.7911243841748 7244.69905255643)
(transform-vector (inverse-transform (nth 0 (transform-list *trans*)))
		  (cv 354.97074962763963 -256.7911243841748 7244.69905255643))

(transform-vector (inverse-transform (nth 0 (transform-list *trans*)))
		  (nth 4 *foox*))
(cv 2764.8356224942963 5414.131182703467 772.7627471349106)

|#

;(trace inverse-intersect-ray-with-plane)
(defmethod inverse-intersect-ray-with-plane
           ((transform iterative-inverse-transform) 2d-pos plane
	    &optional starting-approx)
  (with-class-slot-values iterative-inverse-transform
        (inverse-transform approx-transform max-iterations eps-u)
      transform
    (iterative-inverse-intersect-ray-with-plane
     inverse-transform approx-transform 2d-pos plane
     starting-approx eps-u max-iterations)))




#|
(defun perburb-transform (4x4-transform du dv)
  (let ((minv (make-4x4-identity-matrix)))
    (copy-matrix (invert-matrix (projection-matrix 4x4-transform)) minv)
    (loop for delta in (list du dv)
	  for i from 0 to 1
	  do (loop for j from 0 to 3
		   do (incf (aref minv i j) (* delta (aref minv 3 j)))))
    (make-instance '4x4-projection
		   :create-inverse nil
		   :projection-matrix (invert-matrix minv)
		   )))

(setq inv-proj-4x4
      (make-instance '4x4-projection
		     :create-inverse nil
		     :projection-matrix
		     (invert-matrix (projection-matrix (3d-to-2d-projection (top-view))))
		     ))
(setq inverse-proj
      (make-iterative-inverse-transform
       (3d-to-2d-projection (top-view))
       :approx-transform (perburb-transform inv-proj-4x4 10.0 5.0)
       ))

(setq pt (origin (gui::selected-object)))
pt = #(0.0 0.0 289.42159637304707)
(transform-vector (3d-to-2d-projection (top-view)) pt)

(progn inverse-proj)

(inverse-transform-vector (3d-to-2d-projection (top-view))
			  (transform-vector (3d-to-2d-projection (top-view)) pt))

(iterative-inverse-transform-vector inverse-proj (transform-vector (3d-to-2d-projection (top-view)) pt))

(CV -71.33300000000019 4541.89 6106.683999999999)

(let ((proj (3d-to-2d-projection (top-view)))
      (*transform-dammit* t))
  (declare (special *transform-dammit*))
  (intersect-camera-ray-with-plane proj
				   (transform-vector proj pt)
				   (cv 0.0 0.0 -1.0 5901.912)))
(CV -96.04756726124868 4573.432068867624 5901.912)
 shit = #.(CV -204.77199999999903 1.2208939087004183 167.7230089696891 1.0E-4)  
X= #.(CV -96.0475672612848 4573.432068867286 5901.912)  
dX = #.(CV -71.33300000000019 4541.890000000003 6106.683999999999)  
dU = #.(CV 1.0800249583553523E-11 1.0868461686186493E-10 -0.01655642952674241 0.0) 
shit = #.(CV -404.30904484638814 0.9745840011873952 414.85294685095767 8.190719872330686E-5)  
X= #.(CV -96.04756726129129 4573.432068868181 5901.912)  
dX = #.(CV 24.08274430085293 -30.735702196197053 199.5370448463891)  
dU = #.(CV 1.5575096767861396E-11 -1.7598722479306162E-10 -2.4424906541753444E-15 0.0) 
#.(CV -96.04756726116475 4573.432068867645 5901.912) 
3

(pprint 
 (list (math::list-2d-array (projection-matrix (approx-transform inverse-proj)))
       (math::list-2d-array (invert-matrix (projection-matrix (3d-to-2d-projection (top-view)))))))

(setq lvcs-to-lat-long
      (lvcs-to-lat-long-transform (cme::coordinate-system (3d-world (top-view)))))

(setq proj (append (inverse-transform lvcs-to-lat-long)
		   (list (3d-to-2d-projection (top-view)))))

(setq lat-long-pt (transform-vector lvcs-to-lat-long pt))
(CV -105.12108607676376 39.51246556817012 1861.4716383619234)

(transform-vector proj lat-long-pt)
(inverse-transform-vector proj (transform-vector proj lat-long-pt))

(intersect-camera-ray-with-plane proj
				 (transform-vector proj lat-long-pt)
				 (cv 0.0 0.0 -1.0 1860.0))
(transform-vector proj *)

(setq inverse-proj
      (make-iterative-inverse-transform
       proj :approx-transform 
       ))

(inverse-transform proj)
(approximate-transform (inverse-transform proj))

(progn proj)
|#
