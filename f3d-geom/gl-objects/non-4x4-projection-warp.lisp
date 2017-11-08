(in-package :obj)

#|                Using OpenGL with Non-4x4 Coordinate Projections  

                     Copyright (C) 2002, 2003 by Lynn H. Quam.

LHQ Tue Mar 19 2002

Here is an idea for handling non-linear coordinate-projections in the the OpenGL rendering
environment.

A linear-projective coordinate-projection is defined here as a projection from
3d to 2d coordinates that is expressed by a 4x4 homogeneous matrix P. Ie:
 
      U = P4x4 * X, where U and X are coordinate vectors and P4x4 is a 4x4 matrix.

      <us,vs,ws,s> = P4x4 <x,y,z,1>

      u = us/s
      v = vs/s
      w = ws/s

A non-linear coordinate-projection in a projection from 3d to 2d coordinates that CANNOT be
expressed by a linear-projective coordinate-projection.

Examples of non-linear coordinate-projections are linear-projective
coordinate-projections combined with lens distortion models, rational cubic
polynomials projections (RPC), fast block interpolation projections (FBIP), and
rigorous implementations of the underlying image acquisition systems.

One of the challenges in handling non-linear coordinate-projections in OpenGL is to preserve the
ability to render scenes accurately.  The procedure described here (I believe) accurately deals with
all aspects of OpenGL rendering except for specular reflections, ie. diffuse (Lambertian) reflection
is correctly handled.

Let the non-linear coordinate-projection be described by the function Pnl:

    U = Pnl(X}

Assume that we are rendering (dislaying) a view using the coordinate-projection Pnl:
of some volume of the 3-space defined the the bounding-box B = <xmin, xmax, ymin, ymax, zmin, zmax>.

Perform a least-squares estimation of a best fit of a linear coordinate-projection over the
bounding-box B to the non-linear coordinate-projection Pnl.

HERE IS THE GUTS TO THE METHOD:

Compute the linear-projective coordinate-projection P4x4 which is the least-squares fit of the non-linear
coordinate-projection Pnl over the bounding-box B.

Let X be the world location of a point.

Compute the projection-specific perturbed coordinate X' as follows:

     Let N be a vector in the direction of the principal-ray of P4x4.

     Let <u ,v ,w> = Pnl(X)

     Solve for the point of intersection X' of the camera ray from P4x4 at image point <u,v>
           with the plane whose normal is N and contains point X.

     X' has the following properties:

          1.  Let <u',v',w'> = P4x4(X')

              Then u' = u and v' = v,   ie. X' is on the ray defined by U and P4x4

          2.  (X' - X) dot N = 0,   ie. X' is in the plane defined by N and X

This transformation is invertable.  We can derive X from X' as follows:

     As before, let N be a vector in the direction of the principal-ray of P4x4.

     Let <u',v',w'> = P4x4(X')
     
     Solve for the point of intersection X of the camera ray from Pnl at image point <u',v'>
           with the plane whose normal is N and contains point X'.

When rendering (displaying) a view involving the non-linear projection Pnl and vertices X, substitute
the linear-projective projection P4x4 and vertices X'.

Everything here should be completely non-controversial, just simple algebra.  Our choice of P4x4 means
that the local geometry in 3-space (X) is preserved, world (X) units, and surface normals are
meaningful.  Thus, diffuse lighting calculations are correct.

Specular lighting cannot be done correctly in general because the linear coordinate-projection P4x4 does
not accurately define the camera (eye) position for arbitrary points X'.

The position of local light sources must be corrected in the same manner as vertices.


PROBLEM:  What happens when the point X (or X') is behind" the camera or otherwise
          outside the domain of the projection?

       For 4x4-projections this can be handled by turning off depth clipping.  

       For RPCs and other non-linear projections this should not be a problem for points 
       that project to within the image and are near the terrain surface.  

       RPC projections can be expected to behave strangely for points that are far outside the 
       domain of the projection.  Perhap some kind of weighted mix of Pnl and P4x4 can be
       used at points outside the domain.

NOTE:  The problem describes is not with the method for perturbing vertices and linearizing 
       the non-linear projection.  It is a problem with the non-linear project itself
       when "inappropriately" applied.

|#



(in-package :transforms)

#|

SURROGATE-PROJECTION-PERTURB-POINT implements to guts of the above method, and
SURROGATE-PROJECTION-UNPERTURB-POINT implements the inverse.

|#

;;; This version correctly handles oblique projections by intersecting camera rays with 
;;; a plane perpendicular to the principal-ray of the surrogate-projection.
(defmethod surrogate-projection-perturb-point ((surrogate-projection frame-camera)
					       non-4x4-projection 
					       3dpt)
  (declare (type (simple-array double-float (3)) 3dpt))
  (bind-vector-elements (x y z) 3dpt
    (let* ((m (4x4-projection-projection-matrix (inverse-transform surrogate-projection)))
	   (w (+ (* (aref m 2 0) x) (* (aref m 2 1) y) (* (aref m 2 2) z)))
	   (plane (cv (aref m 2 0) (aref m 2 1) (aref m 2 2) (- w)))
	   (*transform-dammit* t) ; force points behind the camera to project anyhow.
	   ;; Binding *transform-dammit* will not help with rpc projections, but they do not clip anyway
	   ;; and are not likely to be given points behind the camera.
	   (2dpt (transform-vector non-4x4-projection 3dpt))
	   (perturbed-3dpt (intersect-camera-ray-with-plane surrogate-projection 2dpt plane)))
      (declare (type 4x4-matrix m))
      (declare (double-float w))
      perturbed-3dpt)))

(defmethod surrogate-projection-unperturb-point ((surrogate-projection t)
						 (non-4x4-projection transforms::frame-camera)
						 perturbed-pt)
  ;; Some functions, PROJECT-PICK-TO-OBJECT in particular, are calling
  ;; this method even for 4x4 transforms.  SURROGATE-PROJECTION is NIL
  ;; in this case.  Just return the point:
  perturbed-pt)



;;; Given a SURROGATE-PROJECTION for NON-4X4-PROJECTION and a previously perturbed-point,
;;; "unperturb" it back to the range of the non-4x4-projection.
(defmethod surrogate-projection-unperturb-point ((surrogate-projection frame-camera)
						 non-4x4-projection 
						 perturbed-3dpt)
  (declare (type (simple-array double-float (3)) perturbed-3dpt))
  (bind-vector-elements (x y z) perturbed-3dpt
    (let* ((m (4x4-projection-projection-matrix (inverse-transform surrogate-projection)))
	   (w (+ (* (aref m 2 0) x) (* (aref m 2 1) y) (* (aref m 2 2) z)))
	   (plane (cv (aref m 2 0) (aref m 2 1) (aref m 2 2) (- w)))
	   (*transform-dammit* t) ; force points behind the camera to project anyhow.
	   ;; Binding *transform-dammit* will not help with rpc projections, but they do not clip anyway
	   ;; and are not likely to be given points behind the camera.
	   (2dpt (transform-vector surrogate-projection perturbed-3dpt))
	   (3dpt (intersect-camera-ray-with-plane non-4x4-projection 2dpt plane)))
      (declare (type 4x4-matrix m))
      (declare (double-float w))
      3dpt)))



(in-package :obj)

;;; This transform is equivalent to: x' = M_inv * P4x4_inv * Pnl(M * x).
;;; A composite coordinate transform (list) that perturbs object vertices
;;; so that a frame-camera approximation can accurately replace a non-4x4-projection.

;;; This structure definition isn't used yet, but documents the lists that are constructed 
;;; and destructured. 
(defstruct (nonlinear-projection-perturbation (:type list))
  o2w                   ; object-to-world-transform
  non-4x4-projection    ; a non-linear coordinate projection (RPC, FBIP, ...)
  surrogate-projection  ; frame-camera approximation to non-4x4-projection
  w2o                   ; (inverse-transform o2w)
  )

(deftype vertex-index () '(integer 0 #.most-positive-fixnum ))
(deftype positive-fixnum () '(integer 0 #.most-positive-fixnum))

;;; verts is a vertex array (in object coordinates)
;;; xform is a nonlinear-projection, ie. a list of the form:  
;;;    (o2w non-4x4-projection surrogate-projection w2o)
;;; Returns an array of vertices transformed by the nonlinear-projection.
(defun surrogate-projection-perturb-vertex-array (verts xform &optional into-array)
  (declare (type vertex-array-type verts))
  (declare (type (or null vertex-array-type) into-array))
  (declare (optimize (speed 3) (safety 0)))
  (destructuring-bind (o2w non-4x4-projection surrogate-projection w2o) xform
    (let* ((nverts (array-dimension verts 0))
	   (ncomps (array-dimension verts 1))
	   (new (or into-array
		    (make-array (list nverts ncomps) :element-type 'vertex-element-type)))
	   (pt (cv 0.0 0.0 0.0)))
      (declare (type (simple-array double-float *) pt))
      (loop for i of-type #-allegro positive-fixnum #+allegro (integer 0 #.most-positive-fixnum)
	      from 0 below nverts
	    do (setf (aref pt 0) (aref verts i 0)
		     (aref pt 1) (aref verts i 1)
		     (aref pt 2) (aref verts i 2))
	       (cond (o2w
		      (setf (object-vertex new i)
			    (transform-vector w2o 
					      (transforms::surrogate-projection-perturb-point 
					       surrogate-projection non-4x4-projection 
					       (transform-vector o2w pt)))))
		     (t (setf (object-vertex new i)
			      (transforms::surrogate-projection-perturb-point 
			       surrogate-projection non-4x4-projection pt)))))
      ;;(format t "*")
      new)))

;;; *TRANSFORM-VERTICES-PROJECTION* is bound by various DISPLAY methods.
(defvar *transform-vertices-projection* nil)

;;; This version uses (VERTEX-ARRAY-ALIST object) instead of hash-tables.
;;; In this version, each element of vertex-array-alist is a list of the form:
;;;     ((surrogate-projection vertex-array do-object-to-world-transform ). transformed-vertex-array)
;;; There are 2 callers:  (defun vertex-array ..) with do-object-to-world-transform = T
;;;      and (defmethod ribbon-edge-vertices ...) with do-object-to-world-transform = NIL
(defun maybe-transform-vertex-array (object vertex-array &optional (do-object-to-world-transform t))
  (when vertex-array
    ;; *TRANSFORM-VERTICES-PROJECTION* is a list of the form: (non-4x4-projection surrogate-projection)
    (let ((tv-projection *transform-vertices-projection*))
      (if (null tv-projection)
	  vertex-array
	  (let* ((non-4x4-projection (car tv-projection))
		 (surrogate-projection (cadr tv-projection))
		 (object-to-world-transform 
		  (and do-object-to-world-transform
		       (object-to-world-transform object (from-coordinate-system non-4x4-projection))))
		 ;; Why is it necessary to include vertex-array in the assoc key?
		 ;; Answer:  to support ribbons which have both centerline verts and edge verts.
		 ;; do-object-to-world-transform must also be part of the key
		 (key (list surrogate-projection vertex-array do-object-to-world-transform)) 
		 (alist (vertex-array-alist object)))
	    (or (cdr (assoc key alist :test #'equal))
		(let ((perturbed-vertex-array
		       (surrogate-projection-perturb-vertex-array
			vertex-array
			(list object-to-world-transform
			      non-4x4-projection
			      surrogate-projection
			      (inverse-transform object-to-world-transform)
			      ))))  
		  #+debug
		  (when (and (eq object *obj*) (eq gui::*current-view* *view*))
		    (setq *foo2* (list vertex-array perturbed-vertex-array 
				       (list object-to-world-transform
					     (car tv-projection) ; non-4x4-projection
					     (cadr tv-projection) ; surrogate-projection
					     (inverse-transform object-to-world-transform)
					     ))))

		  ;;(format t "transform-vertex-array ~a~%" object)
		  ;; Each element of vertex-array-alist is a list of the form:
		  ;;     ((non-4x4-projection . vertex-array) . perturbed-vertex-array)
		  (setf (vertex-array-alist object)
			(cons (cons key perturbed-vertex-array)
			      alist))
		  perturbed-vertex-array)))))))


;;; Redefine the function originally defined in gl-object-basics.lisp
(defun vertex-array (object)
  (maybe-transform-vertex-array object (%vertex-array object)))

(#-sbcl defconstant #+sbcl defvar *zero-vector* (cv 0.0 0.0 0.0))

;;; The idea here is to guarantee that vertex-array[0] corresponds to the object origin.
;;; In this way, the PERTURB-POINT machinery can maintain the correct coordinates for the origin.
;;; Broken:  vertex-array[0] is not necessarily the object origin.
;;;(defmethod perturbed-object-origin ((object gl-object))
;;;  (if (null *transform-vertices-projection*)
;;;      *zero-vector*
;;;      (let* ((vertex-array (%vertex-array object)))
;;;        (unless vertex-array
;;;          ;; Must force all objects to have a vertex-array of at least one element.
;;;          (setf vertex-array (make-array '(1 3) :element-type 'double-float)
;;;                (%vertex-array object) vertex-array
;;;                (object-vertex vertex-array 0) *zero-vector*))
;;;        ;; This is only meaningful for objects that otherwise have no vertex-array,
;;;        ;; since vertex-array[0] is not necessarily the object origin.
;;;        (object-vertex (vertex-array object) 0))))

;;; For performance, vertex-origin-alist should be a hard-slot of the object rather than a property.
(defmethod vertex-origin-alist (object) (get-prop object :vertex-origin-alist))
(defmethod (setf vertex-origin-alist) (alist object) (setf (get-prop object :vertex-origin-alist) alist))

(defmethod perturbed-object-origin ((object gl-2d-object-mixin))
  *zero-vector*)


(defmethod perturbed-object-origin ((object gl-3d-object-mixin))
  (if (null *transform-vertices-projection*)
      *zero-vector*
      (let* ((tv-projection *transform-vertices-projection*)
	     (non-4x4-projection (car tv-projection))
	     (surrogate-projection (cadr tv-projection))
	     (key surrogate-projection)
	     (alist (vertex-origin-alist object))
	     (o2w (object-to-world-transform object (from-coordinate-system non-4x4-projection))))
	(or (cdr (assoc key alist))
	    (let ((perturbed-origin 
		   (inverse-transform-vector o2w
					     (transforms::surrogate-projection-perturb-point 
					      surrogate-projection non-4x4-projection 
					      (transform-vector o2w *zero-vector*)))))
	      (push (cons key perturbed-origin) (vertex-origin-alist object))
	      perturbed-origin)))))

;;; Call this when the object vertices or object-to-world-transform are changed.
;;; This should be renamed to FLUSH-VERTEX-CACHE
(defun flush-transform-vertices-ht (object)
  (setf (vertex-origin-alist object) nil)
  (setf (vertex-array-alist object) nil))


#|
;;; from /opt/IU/radius/site-2d-worlds/alv/alv-oblique-tower/camera-model
(setf (get-prop (3d-to-2d-projection (top-view)) :surrogate-projection)
      (cme::MAKE-PERSPECTIVE-TRANSFORM
       :focal-length -500.0 :near 1.0 
       :principal-point-u 191.0
       :principal-point-v 113.0
       :transform-matrix
       (make-and-fill-2d-array
	'((-0.9902294 0.015442824 -0.13858992 -57.242573)
	  (-0.13671796 0.088168204 0.9866786 4954.4883)
	  (0.027456328 0.99598587 -0.08519544 6054.9927)
	  (0.0 0.0 0.0 1.0)))
       ;;:property-list (list :reference-frame *alv-reference-frame*)
       ))

;;; alv-oblique-tower
(let ((xdidl 100.0))
  (setf (get-prop (3d-to-2d-projection (top-view)) :surrogate-projection)
	(cme::MAKE-PERSPECTIVE-TRANSFORM
	 :focal-length -500.0 :near 1.0 
	 :principal-point-u 191.0
	 :principal-point-v 113.0
	 :transform-matrix
	 (make-and-fill-2d-array
	  `((-0.9902294 0.015442824 -0.13858992 ,(+ xdidl -57.242573))
	    (-0.13671796 0.088168204 0.9866786 4954.4883)
	    (0.027456328 0.99598587 -0.08519544 6054.9927)
	    (0.0 0.0 0.0 1.0))))))

(setf (get-prop (3d-to-2d-projection (top-view)) :surrogate-projection) nil)

(setf (get-prop (3d-to-2d-projection (top-view)) :surrogate-projection)
      (3d-to-2d-projection (top-view)))

(flush-transform-vertices-ht *obj*)
(describe (vertex-array-alist *obj*))
(setq *obj* (gui::selected-object ))
(setq  *view* (top-view))
(pushnew :debug *features*)

(setq * (ribbon-edge-vertices *obj*))
(eq (ribbon-edge-vertices *obj*) (cdr (car (vertex-array-alist *obj*))))

(defun compare-vertices (va1 va2 &optional (n (array-dimension va1 0)))
  (loop for i from 0 below n
	collect (list (cv (aref va1 i 0) (aref va1 i 1) (aref va1 i 2))
		      (cv (aref va2 i 0) (aref va2 i 1) (aref va2 i 2)))))

(compare-vertices (ribbon-edge-vertices *obj*)
		  (cdr (car (vertex-array-alist *obj*))))

(compare-vertices (ribbon-edge-vertices *obj*)
		   (let ((*transform-vertices-projection* (gui::transform-vertices-projection (top-view))))
		     (maybe-transform-vertex-array *obj* (ribbon-edge-vertices *obj*) nil)))
(untrace intersect-camera-ray-with-plane)

(compare-vertices (vertex-array *obj*)
		  (let ((*transform-vertices-projection* (gui::transform-vertices-projection (top-view))))
		    (maybe-transform-vertex-array *obj* (vertex-array *obj*))))
(let ((*transform-vertices-projection* (gui::transform-vertices-projection (top-view))))
  (transforms::object-to-view-transform *obj* (top-view)))

(setq gui::*invalidate-all-object-sets* nil)

(let ((xoff 100.0))
  (setf (get-prop (3d-to-2d-projection (top-view)) :surrogate-projection)
	;; from /opt/IU/radius/sites/alv/camera-models/alv-3-41
	(MAKE-PERSPECTIVE-TRANSFORM
	 :focal-length -1844.06
	 :principal-point-u 1045.0917
	 :principal-point-v -93.465294
	 :transform-matrix
	 (cme::make-and-fill-2d-array
	  `((0.017695444 -0.99979705 -0.0065462585 ,(+ xoff 5236.7095))
	    (0.99986976 0.01741435 -0.00271249 7803.4717)
	    (0.0028280467 -0.0064978567 0.9999749 12000.94)
	    (3.0066943e-25 -1.58982905e-24 -3.4509467e-25 1.0)))
	 ;;:near 3000.0 :far 10000.0	
	 )))

(typep *obj* 'basic-curve)
(setf (get-prop (3d-to-2d-projection (top-view)) :surrogate-projection) 
      (3d-to-2d-projection (top-view)))
(setf (get-prop (3d-to-2d-projection (top-view)) :surrogate-projection) nil)


(defun make-4x4-projection-from-4x4-projection (projection &optional near-far)
  (multiple-value-bind (3d-to-camera-matrix internal-params camera-to-2d-matrix)
      (cme::decompose-projection-matrix (transforms::projection-matrix projection) :1/f-threshold 1e-6 :roff 1.0)
    (apply 'cme::make-perspective-transform
	   :camera-to-3d-matrix (invert-matrix 3d-to-camera-matrix)
	   (append internal-params near-far))))

(setf (get-prop (3d-to-2d-projection (top-view)) :surrogate-projection)
      (make-4x4-projection-from-4x4-projection alv-3-41-projection))

(setf (get-prop (3d-to-2d-projection (top-view)) :surrogate-projection)
      (make-4x4-projection-from-4x4-projection alv-oblique-tower-projection))

(describe alv-3-41-projection)
(describe (get-prop (3d-to-2d-projection (top-view)) :surrogate-projection))
	
(setf (get-prop (3d-to-2d-projection (top-view)) :surrogate-projection) nil)
(SURROGATE-PROJECTION (3d-to-2d-projection (top-view)))

(get-prop (3d-to-2d-projection (top-view)) :surrogate-projection)
(transforms::projection-matrix (get-prop (3d-to-2d-projection (top-view)) :surrogate-projection))
(describe (3d-to-2d-projection (top-view)))


(vertex-array-alist (caar (selected-objects)))
(setq *print-array* t)
(progn gui::*from*)
(transform-vector gui::*xform* gui::*from*)

(trace redisplay-required)
(untrace redisplay-required)

(describe *object-set*)
(loop for dls in (DISPLAY-LISTS *object-set*)
      do (describe dls))

(describe *last-new-excluded-objects*)


(get-object-set-display-list-state *object-set* (top-view))

(setf (invalid (get-object-set-display-list-state *object-set* (top-view)))
      t)
(setq *always-immediate-render-p* t)
(setq *always-immediate-render-p* nil)

(setq *print-array* t)

(let ((obj (car *last-new-excluded-objects*))
      (*transform-vertices-projection* (transform-vertices-projection (top-view))))
  (list (%vertex-array obj)
	(vertex-array obj)))

(progn *xform*)
(loop with mat = (transforms::projection-matrix (car *xform*))
      for tr in (cdr *xform*)
      for m = (transforms::projection-matrix tr)
      do (setq mat (multiply-matrices m mat))
      finally (return mat))

(transform-vector *xform* (cv 0.0 0.0 0.0))
(transform-vector (nth 2 *xform*) (cv 0.0 0.0 0.0))

(flush-transform-vertices-ht (car *last-new-excluded-objects*))

(transform-vertices-projection (top-view))

(surrogate-projection (3d-to-2d-projection (top-view)))
(describe (surrogate-projection (3d-to-2d-projection (top-view))))
(describe (inverse-transform (surrogate-projection (3d-to-2d-projection (top-view)))))

(multiply-matrices (transforms::projection-matrix (inverse-transform (surrogate-projection (3d-to-2d-projection (top-view)))))
		   (transforms::projection-matrix (surrogate-projection (3d-to-2d-projection (top-view)))))

(list 
 (transform-vector (list (nth 0 *xform*)) (cv 0.0 0.0 0.0))
 (transform-vector (list (nth 0 *xform*) (nth 1 *xform*)) (cv 0.0 0.0 0.0))
 (intersect-camera-ray-with-z-plane (nth 2 *xform*)
				    (transform-vector (list (nth 0 *xform*) (nth 1 *xform*)) (cv 0.0 0.0 0.0))
				    (aref (transform-vector (list (nth 0 *xform*)) (cv 0.0 0.0 0.0)) 2))
 )

(flush-all-transform-vertices-ht )

(transform-vector (transforms::object-to-view-transform (car *last-new-excluded-objects*) (top-view))
		  (cv 0.0 0.0 0.0))
|#
