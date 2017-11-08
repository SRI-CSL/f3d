(in-package :math)

#| GENERAL PURPOSE ANALYTICAL GEOMETRY

vertex-array-polygon-normal point-inside-polygon 3d-polygon-convex-p polygon-center

triangle-normal 3d-triangle-area 

intersect-ray-with-plane intersect-curve-with-surface 

(maybe-compile-file-load "$FREEDIUS/lisp/math/geometry.lisp")
|#
;;; *******************  3-SPACE TRIANGLE AND POLYGON FUNCTIONS  *******************


;;; solves Ax=0 where A is of rank 2.
;;; called only from this file
(defun solve-3x3-homogeneous-linear-system (mat &optional into-vector)
  (declare (type (simple-array double-float (* *)) mat))
  (declare (optimize (speed 3)(safety 1)))
  (let ((dets (make-array '(3 3) :element-type 'double-float))
	(soln (or into-vector (make-array 3 :element-type 'double-float))))
    (declare (type (simple-array double-float (* *)) dets)
	     (type (simple-array double-float (*)) soln) )
    (declare (optimize (speed 3)(safety 0)))
    (macrolet ((solve2! (a1 b1 c1 a2 b2 c2 x1 x2)
		 ;; This solves the matrix equation Mx = y where x and y are 2-vectors and M is 2x2.
		 ;; a1*x+b1*y=c1, and a2*x+b2*y=c2 instead of
		 ;; a1*x+b1*y+c1=0, and a2*x+b2*y+c2=0
		 `(let ((%a1 ,a1) (%b1 ,b1) (%c1 ,c1)
			(%a2 ,a2) (%b2 ,b2) (%c2 ,c2))
		    (declare (double-float %a1 %b1 %c1 %a2 %b2 %c2))
		    (let ((%det (dfloat (- (* %a2 %b1) (* %a1 %b2)))))
		      (declare (double-float %det))
		      (if (zerop %det)
			  nil
			  (progn (setq %det (/ %det))
				 (setf ,x1 (* %det (- (* %b1 %c2) (* %b2 %c1)))
				       ,x2 (* %det (- (* %a2 %c1) (* %a1 %c2))))
				 t)))))
	       (f+ (i j)
		 `(the fixnum (+ ,i ,j)))
	       (f- (i j)
		 `(the fixnum (- ,i ,j)))
	       (compute-det (i j)
		 `(let* ((i ,i) (j ,j)
			 (i1 (f+ i 1))
			 (i2 (f+ i 2))
			 (j1 (f+ j 1))
			 (j2 (f+ j 2)))
		    (declare (fixnum i j i1 i2 j1 j2))
		    (when (> i1 2) (setq i1 (f- i1 3)))
		    (when (> i2 2) (setq i2 (f- i2 3)))
		    (when (> j1 2) (setq j1 (f- j1 3)))
		    (when (> j2 2) (setq j2 (f- j2 3)))
		    (- (* (aref mat i1 j1) (aref mat i2 j2))
		       (* (aref mat i2 j1) (aref mat i1 j2))))))
			 
      (loop with abs-det-max double-float  = 0.0d0
	    with i-max fixnum = 0
            with j-max fixnum = 0
	    for i fixnum from 0 below 3
	    do (loop for j fixnum from 0 below 3
		     for det double-float = (compute-det i j)
		     for abs-det double-float = (abs det)
		     when (> abs-det abs-det-max)
		       do (setq abs-det-max abs-det i-max i j-max j)
		     do (setf (aref dets i j) det)
		     )
	    finally
	 (return (if (and i-max j-max (> abs-det-max 0.0d0))
		     (let* ((i i-max ) (j j-max )
			    (i1 (f+ i 1))
			    (i2 (f+ i 2))
			    (j1 (f+ j 1))
			    (j2 (f+ j 2))
			    (x1 0.0d0) (x2 0.0d0)
			    )
		       (declare (fixnum i j i1 i2 j1 j2))
		       (declare (double-float x1 x2))
		       (when (> i1 2) (setq i1 (f- i1 3)))
		       (when (> i2 2) (setq i2 (f- i2 3)))
		       (when (> j1 2) (setq j1 (f- j1 3)))
		       (when (> j2 2) (setq j2 (f- j2 3)))

		       (solve2! (aref mat i1 j1) (aref mat i1 j2) (- (aref mat i1 j))
				(aref mat i2 j1) (aref mat i2 j2) (- (aref mat i2 j))
				x1 x2)
			 
		       (setf (aref soln j1) x1
			     (aref soln j2) x2
			     (aref soln j) 1.0d0)
		       ;;(setq foo (list dets soln)) (break)
		       soln)

		     (set-coordinate-vector-elements soln 0.0 0.0 0.0)
		     ))))))
        

;;; package problem here -- this code is duplicated from /globj/macros-gl-objects.lisp

(deftype vertex-element-type () 'double-float)
(deftype vertex-array-type () '(simple-array vertex-element-type (* *)))

;;; verts is a vertex-array-type for use in OpenGL object drawing.
;;; indicies is a vector of indices that belong to the polygon
(defun vertex-array-polygon-centroid (verts indices &optional into-vector)
  (declare (type vertex-array-type verts))
  (loop with n fixnum = (length indices)
	with 1/n double-float = (/ 1.0 (dfloat n))
	for i fixnum from 0 below n
	for vert-index fixnum = (aref indices i)
	for x double-float = (aref verts vert-index 0)
	for y double-float = (aref verts vert-index 1)
	for z double-float = (aref verts vert-index 2)
	sum x into xs double-float
	sum y into ys double-float
	sum z into zs double-float
	finally (return (set-coordinate-vector-elements (or into-vector (make-coordinate-vector 3))
							(* 1/n xs) (* 1/n ys) (* 1/n zs)))))


;;; ************************************  POLYGON-NORMAL  **************************************

;;; This handles non-planar and non-convex polygons.
(defun vertex-array-polygon-normal (verts indices &optional into-normal)
  (declare (type vertex-array-type verts))
  (macrolet ((bind-vertex-array-elements (element-let-list vertex-array index &body body)
		 `(let* ((%vertex-array%  ,vertex-array)
			 (%index% ,index))
		    (declare (type vertex-array-type %vertex-array%))
		    (declare (fixnum %index%))
		    (let ,(loop for i from 0
				for var in element-let-list
				collect `(,var (aref %vertex-array% %index% ,i)))
		      (declare (double-float . ,element-let-list))
		      .,body))))
    (let* ((centroid (vertex-array-polygon-centroid verts indices 
						    (load-time-value* (make-coordinate-vector 3) ) ))
	   (n (length indices))
	   (mat (make-array (list n 3) :element-type 'double-float))
	   (mtm (make-array '(3 3) :element-type 'double-float))
	   (area 0.0))
      (declare (double-float area))
      (declare (fixnum n))
      (declare (type (simple-array double-float (* *)) mat mtm ))
      (bind-vector-elements (xc yc zc) centroid
	(loop for i fixnum from 0 below n
	      for vert-index fixnum = (aref indices i)
	      do (bind-vertex-array-elements (x y z) verts vert-index
		   (setf (aref mat i 0) (- x xc)
			 (aref mat i 1) (- y yc)
			 (aref mat i 2) (- z zc))))
	;(setq *foo* (list centroid mat))
	(multiply-matrices-transposed1 mat mat mtm)
	;; Least-squares fit of plane to the pts.
	(setq into-normal (solve-3x3-homogeneous-linear-system mtm into-normal ))

	;; To correctly orient the normal we must determine whether the polygon area using this normal
	;; is negative.
	(bind-vector-elements (nx0 ny0 nz0) into-normal
	  (bind-vertex-array-elements (x1 y1 z1) verts (aref indices 0)
	    (bind-vertex-array-elements (x2 y2 z2) verts (aref indices 1)
	      (setq area
		    (loop with dx21 double-float = (- x2 x1)
			  with dy21 double-float = (- y2 y1)
			  with dz21 double-float = (- z2 z1)
			  for i fixnum from 2 below n
			  for vert-index fixnum = (aref indices i)
			  sum (the double-float
				(bind-vertex-array-elements (x3 y3 z3) verts vert-index
				  (let* ((dx31 (- x3 x1)) (dy31 (- y3 y1)) (dz31 (- z3 z1))
					 (nx 0.0) (ny 0.0) (nz 0.0))
				    (declare (double-float dx31 dy31 dz31 nx ny nz ))
				    (inline-cross-prod (dx21 dy21 dz21) (dx31 dy31 dz31) (nx ny nz))
				    (setq dx21 dx31 dy21 dy31 dz21 dz31)
				    (inline-inner-prod (nx ny nz) (nx0 ny0 nz0)))))
			  double-float))))
	  (when (minusp area)
	    (set-coordinate-vector-elements into-normal (- nx0) (- ny0) (- nz0)))
	  ;;(format t "polygon-normal ~a ~A~%" into-normal area)
	  (values into-normal area) )))))


;;; This may not return reasonable results unless all vertices are co-planar.
;;; See the function polygon-normal below.
;;; no callers
;;;(defun 3d-polygon-area (pt1 pt2 &rest pts)
;;;  (bind-vector-elements (x1 y1 z1) pt1
;;;    (bind-vector-elements (x2 y2 z2) pt2
;;;      (let* ((dx21 (- x2 x1))
;;;	     (dy21 (- y2 y1))
;;;	     (dz21 (- z2 z1))
;;;	     nx0 ny0 nz0
;;;	     (first t)
;;;	     (area (* .5 (loop for pt3 in pts
;;;			       sum (the double-float
;;;				       (bind-vector-elements (x3 y3 z3) pt3
;;;					 (let* ((dx31 (- x3 x1)) (dy31 (- y3 y1)) (dz31 (- z3 z1))
;;;						nx ny nz)
;;;					   (declare (double-float dx31 dy31 dz31 nx ny nz ))
;;;					   (inline-cross-prod dx21 dy21 dz21 dx31 dy31 dz31 nx ny nz)
;;;					   (setq dx21 dx31 dy21 dy31 dz21 dz31)
;;;					   (if first
;;;					       (let* ((l (ic::inline-euclidean-length nx ny nz))
;;;						      (1/l (if (zerop l) 0.0 (/ l))))
;;;						 (setq nx0 (* nx 1/l) ny0 (* ny 1/l) nz0 (* nz 1/l))
;;;						 (unless (zerop l) (setq first nil))
;;;						 l)
;;;					       (inline-inner-prod nx ny nz nx0 ny0 nz0)))))
;;;			       double-float ))))
;;;	(declare (double-float dx21 dy21 dz21  nx0 ny0 nz0 area ))
;;;	(when (< area 0.0)
;;;	  (setq nx0 (- nx0) ny0 (- ny0) nz0 (- nz0) area (- area )))
;;;	
;;;	(values area
;;;		(coordinate-vector nx0 ny0 nz0))))))


;;; *******************************   POINT-INSIDE-POLYGON  *******************************
#|
(point-inside-polygon (cv .4 .5 0.0)
		      (list (cv 0.0 0.0 0.0)
			    (cv 0.0 1.0 0.0)
			    (cv 1.0 0.0 0.0)))
(disassemble 'POINT-INSIDE-POLYGON)
|#

#|

POINT-INSIDE-POLYGON returns oddp of the number of intersections.  Clockwise vs
counter-clockwise ordering doesn't matter.

The algorithm is as follows:  BIND-SUBSPACE-ELEMENTS projects the 3d vertices to
a 2d-plane according to the component of the polygon-normal that is largest in
magnitude.  With everything projected onto a plane, the number of proper
intersections of semi-infinite horizontal line from pt to infinity with the line
segments connecting adjacent vertices around the 2d-polygon.  
|#
(defun POINT-INSIDE-POLYGON (pt vertex-list &optional polygon-normal)
  (unless polygon-normal
    (destructuring-bind (v0 v1 v2 &rest ignore) vertex-list
      (setq polygon-normal (triangle-normal v0 v1 v2))))
  (bind-vector-elements (nx ny nz) polygon-normal
    (let* ((anx (abs nx)) (any (abs ny)) (anz (abs nz))
	   (max-elem (max anx any anz))
	   (which (cond ((= anx max-elem) 0) ((= any max-elem) 1) (t 2)))
	   )
      (declare (double-float max-elem anx any anz))
      ;;(format t "which = ~a~%" which)
      (macrolet ((bind-subspace-elements (vars vector &body body)
		   `(bind-vector-elements (%x %y %z) ,vector
		     (let ,(loop for var in vars collect `(,var 0.0d0))
		       (declare (double-float . ,vars))
		       (case which
			 (2 (setq ,(car vars) %x ,(cadr vars) %y))
			 (1 (setq ,(car vars) %x ,(cadr vars) %z))
			 (0 (setq ,(car vars) %y ,(cadr vars) %z)))
		       . ,body))))
			      
	(bind-subspace-elements
	 (x1 y1) pt
	 (bind-subspace-elements
	  (x2 y2) (car vertex-list)
	  (oddp (loop with last = nil
		      until last
		      for rest = (cdr vertex-list)
			then (or (cdr rest) (progn (setq last t) vertex-list))
		      for pt2b = (car rest)
		      count (bind-subspace-elements
			     (x2b y2b) pt2b
			     (let* ((dx2 (- x2b x2))
				    (dy2 (- y2b y2))
				    (dy12 (- y1 y2))
				    (beta 0.0d0) (x 0.0d0))
			       (declare (double-float dx2 dy2 dy12 beta x ))
			       (prog1 (unless (= dy2 0.0) ; horizontal line - do not count
					(setq beta (/ dy12 dy2))
					(when (< 0.0 beta 1.0) ; line must intersect between the 2 vertices
					  (setq x (+ x2 (* beta dx2)))
					  ;;(format t "~a ~a " x (> x x1) )
					  (> x x1)))
				 (setq x2 x2b y2 y2b)) ))))))))))



;;; polygon is a list of <x y z> vertices
(defun polygon-center (polygon &optional vert)
  (unless vert (setq vert (make-coordinate-vector 3)))
  (loop with n double-float = (dfloat (length polygon))
	with (x y z) double-float
	for vert in polygon
	do (mv-setq-vector-elements (x y z) vert)
	sum x into xs double-float
	sum y into ys double-float
	sum z into zs double-float
	finally (math::inline-set-coordinate-vector-elements
		       vert (/ xs n) (/ xs n) (/ xs n))
		(return vert)))

;;; 3D-POLYGON-CONVEX-P determines that angle formed by vertex sequence v[i] v[i+1] v[i+2] is always
;;; positive or always negative.  Since the vertices are in 3-space, the normal to the plane of the
;;; polygon is computed from the first sequence of 3 vertices that make this possible.  This normal
;;; is effectively used to map the vertex coordinates into the plane of the polygon for the
;;; convexity test.

;;; It isn't clear that this is correct unless the polygon is planar.
;;; called from cme/extruded-object.lisp: and this file
(defun 3d-polygon-convex-p (poly)
  (or (<= (length poly) 3)
      (bind-vector-elements (x1 y1 z1) (car poly)
	(bind-vector-elements (x2 y2 z2) (cadr poly)
	  (let* ((dx21 (- x2 x1))
		 (dy21 (- y2 y1))
		 (dz21 (- z2 z1))
		 (nx0 0.0) (ny0 0.0) (nz0 0.0)
		 (first t))
	    (declare (double-float dx21 dy21 dz21 nx0 ny0 nz0))
	    (not (loop for pt3 in (cddr poly)
		       do (bind-vector-elements (x3 y3 z3) pt3
			    (let* ((dx31 (- x3 x1)) (dy31 (- y3 y1)) (dz31 (- z3 z1))
				   (nx 0.0) (ny 0.0) (nz 0.0))
			      (declare (double-float dx31 dy31 dz31 nx ny nz ))
			      (inline-cross-prod (dx21 dy21 dz21) (dx31 dy31 dz31) (nx ny nz))
			      (setq dx21 dx31 dy21 dy31 dz21 dz31)
			      (if first
				  (unless (zerop (inline-inner-prod (nx ny nz) (nx ny nz )))
				    (setq nx0 nx ny0 ny nz0 nz first nil))
				  (when (minusp (inline-inner-prod (nx ny nz) (nx0 ny0 nz0)))
				    (return t))))))))))))

#|
(3d-polygon-convex-p (list (coordinate-vector 0.0 0.0 0.0)
			   (coordinate-vector 1.0 0.0 0.0)
			   (coordinate-vector 0.0 1.0 0.0)
			   (coordinate-vector .5 .2 0.0)))

(3d-polygon-convex-p (list (coordinate-vector 0.0 0.0 0.0)
			   (coordinate-vector 1.0 0.0 0.0)
			   (coordinate-vector 0.0 1.0 0.0)
			   (coordinate-vector .0 .5 0.0)))
|#


(defun triangle-normal (v0 v1 v2 &optional normal)
  (declare (optimize (speed 3) (safety #+cmu 0 #-cmu 1)))
  (declare (type dvector v0 v1 v2))
  (declare (type (or null dvector) normal))
  (let ((normal (or normal (make-coordinate-vector 3)))
	(nx 0.0) (ny 0.0) (nz 0.0))
    (declare (double-float nx ny nz))
    (declare (type dvector normal))
    (bind-vector-elements (x0 y0 z0) v0
      (bind-vector-elements (x1 y1 z1) v1
	(bind-vector-elements (x2 y2 z2) v2
	  (inline-cross-prod ((- x1 x0) (- y1 y0) (- z1 z0))
			     ((- x2 x0) (- y2 y0) (- z2 z0))
			     (nx ny nz))
	  (normalize-vector-elements nx ny nz)
	  (math::inline-set-coordinate-vector-elements
	   normal nx ny nz)
	  normal)))))

;;; No callers
(defun 3d-triangle-area (pt1 pt2 pt3)
  (bind-vector-elements (x1 y1 z1) pt1
    (bind-vector-elements (x2 y2 z2) pt2
      (bind-vector-elements (x3 y3 z3) pt3
	(let ((nx 0.0) (ny 0.0) (nz 0.0))
	  (declare (double-float nx ny nz))
	  (inline-cross-prod ((- x2 x1) (- y2 y1) (- z2 z1))
			     ((- x3 x1) (- y3 y1) (- z3 z1))
			     (nx ny nz))
	  (* .5 (inline-euclidean-length nx ny nz)))))))


;;; **************************  INTERSECT-RAY-WITH-PLANE **************************


;;; the result is the intersection of the ray with the plane
(defun intersect-ray-with-plane (pt dir plane)
  (bind-vector-elements (rx ry rz) pt
    (bind-vector-elements (dx dy dz) dir
      (bind-vector-elements (nx ny nz n1) plane
	;; <rx ry rz> and <dx dy dz> define a ray
	;; <nx ny nz n1> define a plane
	(let ((alpha (- (/ (inline-inner-prod (rx ry rz 1) (nx ny nz n1))
			   (inline-inner-prod (dx dy dz 1) (nx ny nz n1))))))
	  (cv (+ rx (* alpha dx))
	      (+ ry (* alpha dy))
	      (+ rz (* alpha dz))))))))

;;; **************************  INTERSECT-CURVE-WITH-SURFACE  **************************

;;#+never  ; no callers
(progn 

;;; curve is function which maps the single parameter s to a point in n-space
;;; surface is a function of a point in n-space which returns 0.0 when the point is on the surface,
;;; and < 0 for points on one side of the surface, and > 0 for points on the other side of the surface.
;;; No attempt is made to deal with multiple solutions.

(defun mv-intersect-curve-with-surface (curve surface s0 &optional s1)
  (math::solve-with-secant-method
   #'(lambda (s) (let ((pt (multiple-value-list (funcall curve s))))
		   (apply surface pt)))
   s0 s1))

(defun intersect-curve-with-surface (curve surface s0 &optional s1 (convergence-threshold 1e-5) (MAX-ITERS 10) )
  (math::solve-with-secant-method
   #'(lambda (s) (funcall surface (funcall curve s)))
   s0 s1 convergence-threshold MAX-ITERS ))

(defun intersect-curve-with-surface-constrained (curve surface s0 &optional s1
						 &key constraint-fn 
						 (convergence-threshold 1e-5) (MAX-ITERS 10) )
  (math::solve-with-secant-method-constrained 
   #'(lambda (s) (funcall surface (funcall curve s)))
   constraint-fn
   s0 s1 convergence-threshold MAX-ITERS ))

)





;;; Returns the distance between 2 lines in 3-space.
;;;(defun line-to-line-distance (pt0 dir0 pt1 dir1)
;;;  (vector-inner-product (normalize-coordinate-vector (vector-cross-product dir0 dir1))
;;;                        (vector-difference pt0 pt1)))

;(disassemble 'line-to-line-distance)
(defun line-to-line-distance (pt1 dir1 pt2 dir2)
  (declare (optimize (speed 3) (safety 1)))
  (bind-vector-elements (x1 y1 z1) pt1
    (bind-vector-elements (dx1 dy1 dz1) dir1
      (bind-vector-elements (x2 y2 z2) pt2
	(bind-vector-elements (dx2 dy2 dz2) dir2
	  (let ((nx 0.0) (ny 0.0) (nz 0.0)) 
	    (declare (double-float nx ny nz))
	    (inline-cross-prod (dx1 dy1 dz1) (dx2 dy2 dz2) (nx ny nz))
	    (/ (inline-inner-prod (nx ny nz) ((- x1 x2) (- y1 y2) (- z1 z2)))
	       (sqrt (the (double-float 0.0) (inline-inner-prod (nx ny nz) (nx ny nz)))))))))))

;(line-to-line-distance (cv 0.0 0.0 0.0) (cv 1.0 0.0 0.0) (cv 100.0 -100.0 2.0) (cv 0.0 1.0 0.0))
;(INTERSECT-3D-LINES (cv 0.0 0.0 0.0) (cv 1.0 0.0 0.0) (cv 100.0 -100.0 2.0) (cv 0.0 1.0 0.0))
;(INTERSECT-3D-LINES-v2 (cv 0.0 0.0 0.0) (cv 1.0 0.0 0.0) (cv 100.0 -100.0 2.0) (cv 0.0 1.0 0.0))

;;; returns the "nearest" point of intersection and the vector between the 2 lines which
;;; is normal to the plane defined by the cross-product of dir1 and dir2, and whose
;;; length is the intersection miss-distance (ie. skew of the lines).
(defun INTERSECT-3D-LINES (pt1 dir1 pt2 dir2)
  (bind-vector-elements (x1 y1 z1) pt1
    (bind-vector-elements (dx1 dy1 dz1) dir1
      (bind-vector-elements (x2 y2 z2) pt2
	(bind-vector-elements (dx2 dy2 dz2) dir2
	  (block top
	    (let* ((dxq (- x1 x2))
		   (dyq (- y1 y2))
		   (dzq (- z1 z2))
		   (dir1-dot-dir1 (inline-inner-prod (dx1 dy1 dz1) (dx1 dy1 dz1)))
		   (dir2-dot-dir2 (inline-inner-prod (dx2 dy2 dz2) (dx2 dy2 dz2)))
		   (dir1-dot-dir2 (inline-inner-prod (dx1 dy1 dz1) (dx2 dy2 dz2)))
		   (dir1-dot-q    (inline-inner-prod (dx1 dy1 dz1) (dxq dyq dzq)))
		   (dir2-dot-q    (inline-inner-prod (dx2 dy2 dz2) (dxq dyq dzq)))
		   (alpha 0.0)
		   (beta 0.0))
	      (declare (double-float dxq dyq dzq dir1-dot-dir1
				     dir2-dot-dir2 dir1-dot-dir2 dir1-dot-q dir2-dot-q alpha beta))
	  
	      (when (math::solve2! dir1-dot-dir1 (- dir1-dot-dir2) (- dir1-dot-q)
				   (- dir1-dot-dir2) dir2-dot-dir2 dir2-dot-q alpha beta )
		(let* ((x3 (+ x1 (* alpha dx1)))
		       (y3 (+ y1 (* alpha dy1)))
		       (z3 (+ z1 (* alpha dz1)))
		       (x4 (+ x2 (* beta dx2)))
		       (y4 (+ y2 (* beta dy2)))
		       (z4 (+ z2 (* beta dz2))))
		  (declare (double-float x3 y3 z3 x4 y4 z4 ))
		  ;; What should we return?  The 2 points on the lines, or the midpoint of the line 
		  ;; connecting them and the vector between them?

		  ;; This version returns the midpoint on the line connecting the 2 pts
		  ;; and the vector between the 2 lines.  Its length is the miss-distance.  Its direction
		  ;; is the normal to the plane defined by dir1 dir2.
		  (values (inline-coordinate-vector (* .5 (+ x3 x4)) (* .5 (+ y3 y4)) (* .5 (+ z3 z4)))
			  (inline-coordinate-vector (- x3 x4) (- y3 y4) (- z3 z4))))))))))))

;;; returns 2 points, one on each line, at the "nearest" point of intersection.
(defun INTERSECT-3D-LINES-V2 (pt1 dir1 pt2 dir2)
  (bind-vector-elements (x1 y1 z1) pt1
    (bind-vector-elements (dx1 dy1 dz1) dir1
      (bind-vector-elements (x2 y2 z2) pt2
	(bind-vector-elements (dx2 dy2 dz2) dir2
	  (block top
	    (let* ((dxq (- x1 x2))
		   (dyq (- y1 y2))
		   (dzq (- z1 z2))
		   (dir1-dot-dir1 (inline-inner-prod (dx1 dy1 dz1) (dx1 dy1 dz1)))
		   (dir2-dot-dir2 (inline-inner-prod (dx2 dy2 dz2) (dx2 dy2 dz2)))
		   (dir1-dot-dir2 (inline-inner-prod (dx1 dy1 dz1) (dx2 dy2 dz2)))
		   (dir1-dot-q (inline-inner-prod (dx1 dy1 dz1) (dxq dyq dzq)))
		   (dir2-dot-q (inline-inner-prod (dx2 dy2 dz2) (dxq dyq dzq)))
		   (alpha 0.0)
		   (beta 0.0))
	      (declare (double-float dxq dyq dzq dir1-dot-dir1
				     dir2-dot-dir2 dir1-dot-dir2 dir1-dot-q dir2-dot-q alpha beta))
	  
	      (when (math::solve2! dir1-dot-dir1 (- dir1-dot-dir2) (- dir1-dot-q)
				   (- dir1-dot-dir2) dir2-dot-dir2 dir2-dot-q alpha beta )
		(let* ((x3 (+ x1 (* alpha dx1)))
		       (y3 (+ y1 (* alpha dy1)))
		       (z3 (+ z1 (* alpha dz1)))
		       (x4 (+ x2 (* beta dx2)))
		       (y4 (+ y2 (* beta dy2)))
		       (z4 (+ z2 (* beta dz2))))
		  (declare (double-float x3 y3 z3 x4 y4 z4 ))
		  (values (inline-coordinate-vector x3 y3 z3)
			  (inline-coordinate-vector x4 y4 z4)))))))))))
