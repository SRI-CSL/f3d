(in-package :transforms)

;;; This should be in a different package.

#| 
Fri Nov 12 2004 LHQ: 
   A small subset of this code has been tested in FREEDIUS.  See the test code
   in terrain-models.lisp which calls the function INTERSECT-RAY-WITH-DTM .


(st:load-system :terrain-models)
|#
;;; Digital Terrain Model Ray Tracing and other stuff.

(eval-when (eval load compile)
(import '(lx::multi-mv-bind))
(import '(img::diref))
(import '(gui::dtm-image gui::dtm-to-lvcs-transform gui::dtm-bbox))

(import '(img::image-prop img::image-x-dim img::image-y-dim img::image-neighborhood-min-max
	  img::MAKE-BORDERED-IMAGE img::image-neg-x img::image-neg-y img::image-transpose))
(import '(img::image-element-type))
)

(defmacro icv (&rest args) `(math::inline-coordinate-vector .,args))

;;; fixnums remain fixnums
(defmacro f+ (&rest args) `(the fixnum (+ .,args)))
(defmacro f- (&rest args) `(the fixnum (- .,args)))
(defmacro f1+ (x) `(the fixnum (1+ ,x)))

(deftype small-double-float ()
  '(double-float #.(dfloat most-negative-fixnum) #.(dfloat most-positive-fixnum)))

(defmacro fixnum-floor (x)
  `(floor (the small-double-float ,x)))

;;; To do:   Must handle :no-data-value property of the dtm-image.
;;; Need proper definition of what that means.

(defparameter *debugging-printout* nil)

;(setq  *debugging-printout* 0)
;(setq *debugging-printout* nil)

;#+never  ; enable this for debugging printout
(progn
(defmacro debug-print (&rest args)
  `(debug-printer nil ,@args))

(defmacro debug-print2 (level &rest args)
  `(debug-printer ,level ,@args))

(defun debug-printer (level &rest args)
  (when *debugging-printout*
    (if (and (numberp level) (numberp *debugging-printout*))
	(when (<= level *debugging-printout*)
	  (apply 'format t args))
	  
	(apply 'format t args))))

) ;end progn

;; this version simplifies code optimization by getting rid of extraneous debugging crap.
#+never
(progn 

(defmacro debug-print (&rest args)
  (declare (ignore args)))

(defmacro debug-print2 (level &rest args)
  (declare (ignore level args)))

) ;end progn


(defun compute-octant (dx dy)
  (declare (double-float dx dy))
  (let* ((y-dominant (< (abs dx) (abs dy)))
	 (octant-bit4 (< dy 0.0))
	 (octant-bit2 (neq (< dx 0.0) octant-bit4))
	 (octant-bit1 (neq y-dominant octant-bit2)))
    (+ (if octant-bit1 1 0)
       (if octant-bit2 2 0)
       (if octant-bit4 4 0)))
  )


;;; Returns the transform matrix from coordinates in the octant to the 
;;; actual dtm-image.
(defun octant-dtm-transform (octant-dtm)
  (image-to-2d-transform octant-dtm))

;(loop for i from 0 to 7 collect (octant-transform i 100 100))
    
(defun map-dtm-to-octant (dtm octant)
  (let ((octant-map (image-prop dtm :octant-map)))
    (if octant-map
	(values-list (aref octant-map octant))

	(multiple-value-bind  (dtm-min dtm-max x-scale y-scale)
	    (dtm-min-max-images dtm)
	  (let* ((xdim (image-x-dim dtm))
		 (ydim (image-y-dim dtm))
		 (padded-x-dim (1+ (pad-to-multiple xdim x-scale)))
		 (padded-y-dim (1+ (pad-to-multiple ydim y-scale)))
		 (padded-dtm (make-bordered-image dtm
						  (list 0 (- padded-x-dim xdim))
						  (list 0 (- padded-y-dim ydim)))))
	    (setf (2d-world padded-dtm) (2d-world dtm))
	    (setf (image-to-2d-transform padded-dtm) (image-to-2d-transform dtm))
	    (setq octant-map (make-array 8))
	    (loop for octant from 0 to 7
		  for oct-dtm = padded-dtm
		  for oct-dtm-min = dtm-min
		  for oct-dtm-max = dtm-max
		  do
	       (when (memq octant '(2 3 4 5)) ; (< dx 0)	
		 (setq oct-dtm (image-neg-x oct-dtm)
		       oct-dtm-min (image-neg-x oct-dtm-min)
		       oct-dtm-max (image-neg-x oct-dtm-max)))
	      
	       (when (memq octant '(4 5 6 7)) ; (< dy 0)
		 (setq oct-dtm (image-neg-y oct-dtm)
		       oct-dtm-min (image-neg-y oct-dtm-min)
		       oct-dtm-max (image-neg-y oct-dtm-max)))
	      
	       (setf (aref octant-map octant)
		     (if (memq octant '(1 2 5 6)) ;(< dx dy)
			 (list (image-transpose oct-dtm)
			       (image-transpose oct-dtm-min)
			       (image-transpose oct-dtm-max)
			       y-scale x-scale padded-y-dim padded-x-dim)
			 (list oct-dtm oct-dtm-min oct-dtm-max
			       x-scale y-scale padded-x-dim padded-y-dim)))
		  )
	    
	    ;; Set the :octant-map of the image AFTER computing its entries
	    ;; to prevent problems if the above computation is aborted.
	    (setf (image-prop dtm :octant-map) octant-map)
	    
	    (values-list (aref octant-map octant)))))))

;; (object-to-world-transform (img::image-neg-x (gui::view-image (gui::top-view))))
;;(property-list (img::image-neg-x (gui::view-image (gui::top-view))))

;;; If the magnitude of dx is less than the magnitude of dy, the x and y components of everything
;;; are interchanged and transpose is returned as T.
(defun map-point-to-octant-0 (x y dx dy sx sy)
  (let (neg-x neg-y transpose)
     (when (< (abs dx) (abs dy))
	(rotatef x y) (rotatef dx dy) (setq transpose t)
	;; Is there a bug here?  Should padded-x-dim and padded-y-dim be swapped too?
	;; If not, x0 and y0 are swapped, then are subtracted from the wrong padded dims. 
	;; (swapf sx sy)
	;; The answer is that padded-x-dim padded-y-dim are the correct values
	;; computed by MAP-DTM-TO-OCTANT for the appropriate octant.
	)
      (when (< dx 0.0) (setq dx (- dx) x (- sx 1 x)) (setq neg-x t))
      (when (< dy 0.0) (setq dy (- dy) y (- sy 1 y)) (setq neg-y t))
      (values x y dx dy neg-x neg-y transpose)))

(defun-cached dtm-min-max-images (dtm)
  (let* ((dtm-min-max-scales (get-prop dtm :dtm-min-max-scales))
	 (xscale (or (car dtm-min-max-scales) (isqrt (image-x-dim dtm))))
	 (yscale (or (cadr dtm-min-max-scales) (isqrt (image-y-dim dtm)))))
    (multiple-value-bind (dtm-min dtm-max)
	(image-neighborhood-min-max dtm xscale yscale)
      (loop for img in (list dtm-min dtm-max)
	    with xform = (make-4x4-coordinate-transform 
			      (make-and-fill-4x4-matrix
				   xscale 0.0 0.0 0.0
				   0.0 yscale 0.0 0.0
				   0.0 0.0 1.0 0.0
				   0.0 0.0 0.0 1.0))
	    do (setf (2d-world img) (2d-world dtm)
		     (image-to-2d-transform img) xform))
      (setf (get-prop dtm :dtm-min-max-scales) (list xscale yscale))
      (values dtm-min dtm-max xscale yscale))))
  
;;; Thu Jun 10 1999 - LHQ --added to make it possible to avoid recomputing the
;;; dtm min/max images by saving them in the file system are reloading them
;;; when the dtm-image is loaded.
(defun set-dtm-min-max-images (dtm dtm-min dtm-max)
  (let* ((xscale (floor (image-x-dim dtm) (image-x-dim dtm-min)))
	 (yscale (floor (image-y-dim dtm) (image-y-dim dtm-min))))
    (setf (get-prop dtm :dtm-min-max-scales) (list xscale yscale))
    (lx::set-eval-cache (dtm-min-max-images dtm)
				dtm-min dtm-max xscale yscale)))


;(disassemble 'intersect-ray-with-plane)
;(fmakunbound 'intersect-ray-with-plane)

;;; FREEDIUS-ified
(defun intersect-ray-with-plane (pt dir plane-normal)
 ;;#+cmu  (declare (ext:optimize-interface  (speed 3) (safety 0)))
  ;(declare (optimize (speed 3) (safety 1)))
  (declare (optimize (speed 3) (safety 0)))
  (bind-vector-elements (x0 y0 z0) pt
    (bind-vector-elements (dx dy dz) dir
      (bind-vector-elements (nx ny nz) plane-normal
	(let* ((n-dot-d (+ (* nx dx) (* ny dy) (* nz dz)) )
	       (alpha (- (/ (+ (* nx x0) (* ny y0) (* nz z0))
			    n-dot-d))))
	  (declare (double-float n-dot-d alpha))
	  (and (plusp n-dot-d) ; dot produce of face normal with camera ray must be positive
	       (math::inline-coordinate-vector (+ x0 (* alpha dx))
					       (+ y0 (* alpha dy))
					       (+ z0 (* alpha dz)))))))))

#|
;;; *****************************  INTERSECT-RAY-WITH-SQUARE  ********************************


Intersection of Ray in Space with DTM:
    
Canonical Square:  


  0,1____________________1,1
    |                    |
    |                  / |
    |                /   |
    |              /     |
    |            /       |
    |          /         |
    |        /           |
    |      /             |
    |    /               |
    |  /                 |
    |/___________________|
  0,0        x0          1,0

NOTE:  The code in INTERSECT-RAY-WITH-CANONICAL-SQUARE handles all cases for a ray
with direction in the 1st quadrant (dx and dy must be >=0) cutting thru a
unit square, where the  ray enters the square crossing either the edge from 0,0 to 1,0
or from 0,0 to 0,1.
|#

;;; FREEDIUS-ified
(defun intersect-ray-with-square (pt dir zvect)
  (declare (optimize (speed 3) (safety 1)))
  (bind-vector-elements (x0 y0 z0) pt
    (bind-vector-elements (z00 z10 z01 z11) zvect
      (macrolet ((plane (nx ny)
		   `(progn (setf (aref n 0) ,nx
				 (aref n 1) ,ny) 
			   n)))
	(let* ((eps .001)
	       (-eps (- eps))
	       (1+eps (1+ eps))
	       (p (icv x0 y0 (- z0 z00)))
	       (n (cv 0.0 0.0 -1.0)))
	  (declare (type (simple-array double-float (*)) p n))
	  (declare (double-float eps -eps 1+eps))
	
	  (if (<= y0 x0)
	      ;; under diagonal -- try lower triangle first
	      (or (bind-vector-elements (xb yb zb)
		      (intersect-ray-with-plane p dir (plane (- z10 z00) (- z11 z10)))
		    (when (and (< yb (+ xb eps))) ; below diagonal
		      (return-from intersect-ray-with-square
			(if (>= yb -eps)
			    (if (<= xb 1+eps)
				(icv xb yb (+ zb z00))
				:try-right-neighbor)
			    nil))))
	      
		  (bind-vector-elements (xu yu zu)
		      (intersect-ray-with-plane p dir (plane (- z11 z01) (- z01 z00)))
		    (if (and (<= xu (+ eps yu)) (<= yu 1+eps))
			(if (>= xu -eps)
			    (icv xu yu (+ zu z00))
			    :try-left-neighbor)
			nil)))

	      
	      ;; try upper triangle first
	      (or (bind-vector-elements (xu yu zu)
		      (intersect-ray-with-plane p dir (plane (- z11 z01) (- z01 z00)))
		    ;; (break)
		    (if (<= xu (+ yu eps))
			(return-from intersect-ray-with-square
			  (if (<= yu 1+eps)
			      (if (>= xu -eps)
				  (icv xu yu (+ zu z00))
				  :try-left-neighbor)
			      nil))))
		  
		  (bind-vector-elements (xb yb zb)
		      (intersect-ray-with-plane p dir (plane (- z10 z00) (- z11 z10)))
		    (if (and (<= yb (+ eps xb)) (>= yb -eps))
			(if (<= xb 1+eps)
			    (icv xb yb (+ zb z00))
			    :try-right-neighbor)
			nil)))
	      ))))))


(defmethod intersect-ray-with-dtm-cell ((image img::image)
					  ix iy pos dir &optional flip-diagonalization)
  ;;(push (list ix iy) *ray-list)
  (declare (optimize (speed 3) (safety 1)))
  ;;#+cmu (declare (ext:optimize-interface (speed 3) (safety 0)))
  (declare (fixnum ix iy))
  (img::with-iref-checking 
    (bind-vector-elements (x0 y0 z0) pos
    (bind-vector-elements (dx dy dz) dir
      (let* ((image image)
	     (x0 (- x0 (dfloat ix)))
	     (y0 (- y0 (dfloat iy)))
	     (z00 (diref image ix iy))
	     (z10 (diref image (f+ 1 ix) iy))
	     (z01 (diref image ix (f+ 1 iy)))
	     (z11 (diref image (f+ 1 ix) (f+ 1 iy)))
	     q) 
	(declare (double-float x0 y0 z00 z10 z01 z11))
	(if flip-diagonalization
	    ;; This needs to know which diagonal is used to cut the square. 
	    ;; In the old implementation (prior to Mar 5, 87) it didn't matter, since only
	    ;; image-transpose was applied to the dtm.  Now, image-neg-x/y are applied, which
	    ;; flips the sense of the diagonalization.
	    ;; When FLIP-DIAGONALIZATION = T, this performs a NEG-X transformation on the
	    ;; information passed into and outof INTERSECT-RAY-WITH-SQUARE.
	    (when (vectorp (setq q (intersect-ray-with-square
				    (icv (- 1.0 x0) y0 z0)
				    (icv (- dx) dy dz)
				    (icv z10 z00 z11 z01))))
	      (let ((q q)) (declare (type (simple-array double-float (*)) q))
		   (setf (aref q 0) (- 1.0 (aref q 0)))))
		  
	    (setq q (intersect-ray-with-square (icv x0 y0 z0) (icv dx dy dz) (icv z00 z10 z01 z11))))

	(if (vectorp q)
	    (progn (incf (aref q 0) (dfloat ix))
		   (incf (aref q 1) (dfloat iy))
		   (debug-print2 3 "intersect-ray-with-dtm-cell ~a ~a~%" (list ix iy pos dir) q)
		   q)
	    
	    (progn (debug-print2 3 "intersect-ray-with-dtm-cell -- lose~a ~%"
				 (list ix iy (list x0 y0 z0) dir (list z00 z10 z01 z11)))
		   nil)))))))





(defstruct-class basic-intersector (lx::base-struct-class)
  (;; convergence-threshold units will be the mesh units.
   (convergence-threshold :initform 1e-4 :initarg :convergence-threshold 
			  :accessor convergence-threshold)
   ))

(defvar *intersect-xyz-plane-tmp-plane* (cv 0.0 0.0 0.0 0.0))

(defmethod intersect-xyz-plane ((intersector basic-intersector) coord-index val &optional arg)
  (declare (optimize (speed 3)(safety 1)))
  (let ((plane *intersect-xyz-plane-tmp-plane*))
    (declare (type dvector plane))
    (setf (aref plane 0) 0.0 (aref plane 1) 0.0 (aref plane 2) 0.0 (aref plane 3) val)
    (setf (aref plane coord-index) -1.0)
    (intersect-plane intersector plane arg)))

(defmethod intersect-yz-plane ((intersector basic-intersector) x &optional arg)
  (intersect-xyz-plane intersector 0 x arg))
   
(defmethod intersect-xz-plane ((intersector basic-intersector) y &optional arg)
  (intersect-xyz-plane intersector 1 y arg))
     
(defmethod intersect-xy-plane ((intersector basic-intersector) z &optional arg)
  (intersect-xyz-plane intersector 2 z arg))

(defmethod intersect-bbox ((intersector basic-intersector) bbox)
  (declare (optimize (speed 3)(safety 1)))
  (bind-vector-elements (xmin xmax ymin ymax zmin zmax) bbox
    (let (intersections)
      (flet ((intersect (coord-index val)
	       (mv-bind (pt s) (intersect-xyz-plane intersector coord-index val)
		 (debug-print2 3 "intersect-bbox ~a ~a ~a ~a~%" coord-index val s pt)
		 (when (and pt ;(> s 0.0) 
			    (3d-point-inside-bbox pt bbox))
		   (push (list s pt coord-index) intersections)))))

	(intersect 2 zmin)
	(intersect 2 zmax)
	(intersect 0 xmin)
	(intersect 0 xmax)
	(intersect 1 ymin)
	(intersect 1 ymax)
	(let* ((intersections (sort intersections #'< :key #'car)))
	  (debug-print2 2 "intersect-bbox ~a ~% intersections: ~a~%" bbox intersections)
	  ;; (length intersections) can be greater than 2 at corners and edges of the bbox.
	  (cond ((> (length intersections) 2)
		 (values (cadr (car intersections)) (cadr (car (last intersections)))))
		((= (length intersections) 2)
		 (values (cadr (car intersections)) (cadr (cadr intersections))))
		(intersections
		 ;; one intersection means the camera is inside the bbox
		 (break "inside bbox --- what to do?")
		 (values (intersect-yz-plane intersector xmin) (cadr (car intersections))))
	       
		))))))

;;; ***************************  CAMERA-RAY-INTERSECTOR  ********************

(defstruct-class camera-ray-intersector (basic-intersector)
  ((uv-pt :initarg :uv-pt :accessor uv-pt)
   (3d-estimate :initiform nil :initarg :3d-estimate :accessor 3d-estimate)
   (projection :initarg :projection :accessor projection)
   ))

;(describe (car *foo0*))
;(fmakunbound 'intersect-plane)
(defmethod intersect-plane ((intersector camera-ray-intersector) plane &optional estimate)
  (declare (optimize (speed 3)(safety 1)))
  (with-class-slots camera-ray-intersector (projection uv-pt convergence-threshold 3d-estimate) 
      intersector
    (let* ((intersection (intersect-camera-ray-with-plane projection uv-pt plane
							 (or estimate 3d-estimate))))
      (when intersection
	(unless (arrayp intersection) 
	  (break))
	(values intersection
		(- (aref intersection 2)))))))

#|
(apply 'intersect-plane *foo0*)
(describe (car *foo0*))
(destructuring-bind (intersector plane estimate) *foo0*
  (let ((intersection
	 (intersect-camera-ray-with-plane (projection intersector) (uv-pt intersector) plane estimate)))
    (values intersection (transform-vector (projection intersector) intersection))))
|#

(defmethod estimate-direction ((intersector camera-ray-intersector) pt)
  (declare (optimize (speed 3)(safety 1)))
  (bind-vector-elements (x0 y0 z0) pt
    (bind-vector-elements (x1 y1 z1) (intersect-yz-plane intersector (+ x0 1) pt)
      (normalize-vector (cv (- x1 x0) (- y1 y0) (- z1 z0))))))  


#|
THIS ALGORITHM DESCRIPTION IS TOTALLY OUT OF DATE. 
It applied to an older version of the code.

HIERARCHICAL CAMERA-RAY MESH INTERSECTION ALGORITHM:

Givens: 

    1. An intersector which when given a returns a 3d point <x y z> or NIL
       

    2. A uniform, integer spaced mesh of dimensions [0:XMAX] x [0:YMAX]

    3. A function Z-MIN-MAX-FN(ix iy) returning (zmin zmax) that defines the min and max Z values
       of bounding-boxes in the grid:

           (ix, ix+1, iy, iy+1, zmin(ix,iy), zmax(ix,iy))

    4. A function CELL-HIT-FN(s) that performs the camera-ray plane intersection algorithm for
       the next level of detail at the cell edge intersection discovered at this level 
       of detail.


Everything has been preprocessed so that the curve is almost always in the 1st octant
and Z-MIN-MAX-FN CELL-HIT-FN and CURVE-FN do their computations accordingly.

Thus, we have a curve whose slope is -eps < dy/dx(s) < 1_eps.

Step 1:  We calculate curve-fn(sleft) = (xleft yleft zleft) such that xleft is an integer.
         (Actually we approximate sleft, yleft, zleft based on the local curve point differences.)

         If xleft>xmax or yleft>ymax then we are done.

Step 2:  Return the curve intersection with the cell whose lower-left corner is 
         <xleft, floor(yleft)> if it exists.

         We calculate the ytop = (floor left) + 1 where the curve intersects
         a horizontal line at the top of the cell.  

Step 4:  If ytop-yleft > dy/dx then the curve intersects the cell immediately above.
         Return the curve intersection with the cell whose lower-left corner is 
         <xleft, floor(yleft)+1> if it exists.

Step 5:  Increment s by ds/dx and x by 1 and go to step 1.  We are now approximately
         at the next vertical line in the mesh. 
         (Ie. curve-fn(s)+ds/dx = (xleft2 yleft2 zleft2) is such that xleft2 is very close to xleft+1.)

There are some details left out of this explanation for simplicity.
|#



#|
(apply 'intersect-yz-plane *foo2*)
(let ((*iterative-inverse-intersect-ray-with-plane-verbose* t))
  (destructuring-bind (intersector x) *foo2*
    (list (intersect-yz-plane intersector x)
	  (intersect-yz-plane intersector (1+ x)))))
(trace iterative-inverse-intersect-ray-with-plane)

(setq *foo2* nil)
(apply 'intersect-yz-plane *foo2*)
|#

;(fmakunbound 'intersect-camera-ray-with-mesh)
;;; x-start must be the mesh intersection with a y-z plane.
(defmethod INTERSECT-CAMERA-RAY-WITH-MESH (intersector xs0 xmax z-min-max-fn cell-hit-fn)
  (declare (optimize (speed 3)(safety 1) #+cmu(extensions:inhibit-warnings 3)))
  (declare (double-float xs0))
  (declare (fixnum xmax))
  
  (loop with ix-start fixnum = (fixnum-floor xs0)
	for ix-left fixnum from ix-start below xmax
	for x-left double-float = (dfloat ix-left)
	for x-right double-float = (+ x-left 1.0)
	for left-intersect = (intersect-yz-plane intersector x-left)
	  then right-intersect
	for right-intersect = (intersect-yz-plane intersector x-right left-intersect)
	do 
     (unless (and left-intersect right-intersect)
       (debug-print "intersect-camera-ray-with-mesh intersect-yz-plane failure ~a~%" x-left x-right))
     (bind-vector-elements (x-right y-right z-right) right-intersect
       (bind-vector-elements (x-left y-left z-left) left-intersect
	 (let* ((iy-bottom (fixnum-floor y-left))
		(y-bottom (dfloat iy-bottom))
		(y-top (dfloat (f1+ iy-bottom)))
		(cuts-top-edge (> y-right y-top)) 
		(cuts-bottom-edge (< y-right y-bottom))	; very rare indeed - much curvature
		)
	   (declare (double-float y-top y-bottom))
	   (declare (fixnum iy-bottom))
	   (labels ((z-min-max-test (ix iy xenter zenter zexit)
		      (declare (double-float zenter zexit))
		      (declare (fixnum ix iy))
		      (bind-vector-elements (cell-z-min cell-z-max) (funcall z-min-max-fn ix iy)
					; #+never
			(when (= zenter zexit)
			  (setq *foo2* (list intersector x-left ))
			  (break))
			(debug-print2 2 "z-min-max-test ~a ~a ~a ~a~%" (list ix iy) 
				      xenter (list zenter zexit) (list cell-z-min cell-z-max))
			(when (and (>= (max zenter zexit) cell-z-min)
				   (<= (min zenter zexit) cell-z-max))
			  (debug-print2 2 "cell hit ~a ~a ~a ~a ~a~%" (list ix iy) xenter 
					(list left-intersect right-intersect)
					(list zenter zexit) (list cell-z-min cell-z-max))
			  (funcall cell-hit-fn xenter) ; cell-hit-fn does non-local return when successful
			  )
					;#+never
			(if (> zenter zexit) ; downward looking camera
			    (when (< zenter cell-z-min)
			      (debug-print2 1 "cell hit BELOW SURFACE: ~a ~A ~a ~a~%"
					    (list ix iy) xenter (list zenter zexit) 
					    (list cell-z-min cell-z-max))
			      (return-from intersect-camera-ray-with-mesh nil))
			    (when (> zenter cell-z-max)
			      (debug-print2 1 "cell hit ABOVE SURFACE: ~a ~A ~a ~a~%"
					    (list ix iy) xenter (list zenter zexit) 
					    (list cell-z-min cell-z-max))))
			))
		    )
	       
	     (if (not (or cuts-top-edge cuts-bottom-edge))
		 (z-min-max-test ix-left iy-bottom x-left z-left z-right) ; simplist case
		   
		 (let* ((y-exit (if cuts-bottom-edge y-bottom y-top))
			(dy (- y-right y-left))
			(dz (- z-right z-left))
			(dx-exit (/ (- y-exit y-left) dy))
			;; the ray exits this cell along top or bottom edge at this x increment
			(z-ray-exit (+ z-left (* dz dx-exit))))
		   (declare (double-float y-exit dy dz dx-exit z-ray-exit))
		   (unless (< iy-bottom 0) ; curve entered this mesh at bottom rather than left
		     (z-min-max-test ix-left iy-bottom x-left z-left z-ray-exit))
		     
		   (when (or cuts-top-edge cuts-bottom-edge)
		     (let* ((x-ray-exit (+ x-left dx-exit))
			    (z-ray-exit1 (+ z-ray-exit (* dz (- x-right x-ray-exit)))))
		       (declare (double-float x-ray-exit z-ray-exit1))
		       (z-min-max-test ix-left (if cuts-top-edge (f1+ iy-bottom) (1- iy-bottom))
				       x-ray-exit z-ray-exit z-ray-exit1)

		       (when (> y-right (+ 2.0 y-bottom)) ; possible rare sliver - much curvature
			 (let* ((dx-exit1 (/ (- (+ y-top 1.0) dy)))
				(x-ray-exit2 (+ x-left dx-exit1))
				(z-ray-exit2 (+ z-ray-exit1 (* dz (- x-right x-ray-exit2)))))
			   (declare (double-float x-ray-exit2 z-ray-exit2))
			   (z-min-max-test ix-left (f1+ iy-bottom) 
					   x-ray-exit2 z-ray-exit1 z-ray-exit2)
			   ))))))))))))


(defparameter *intersect-camera-ray-with-gridded-terrain-model-count* 0)
(defparameter *intersect-camera-ray-with-gridded-terrain-model-miss-list* nil)
#|
(setq *intersect-camera-ray-with-gridded-terrain-model-miss-list* nil
      *intersect-camera-ray-with-gridded-terrain-model-count* 0)
(list (length *intersect-camera-ray-with-gridded-terrain-model-miss-list*)
      *intersect-camera-ray-with-gridded-terrain-model-count*)

(setq *
      (approximate-transform 
       (list (inverse-transform (gui::3d-to-2d-projection (gui::top-view)))
	     (inverse-transform (dtm-to-lvcs-transform (gui::find-terrain-model (gui::top-view)))))))
	
(approximate-transform (inverse-transform (gui::3d-to-2d-projection (gui::top-view))))
(flatten-coordinate-transform-list 
 (list (inverse-transform (gui::3d-to-2d-projection (gui::top-view)))
       (inverse-transform (dtm-to-lvcs-transform (gui::find-terrain-model (gui::top-view))))))
(approximate-transform 
 (inverse-transform (dtm-to-lvcs-transform (gui::find-terrain-model (gui::top-view)))))

(let ((tm (gui::find-terrain-model (gui::top-view))))
  (mv-setq (approx-dtm-to-lvcs approx-lvcs-to-dtm-resids)
	   (math::make-4x4-transform-approximation 
	    (dtm-to-lvcs-transform tm)
	    (dtm-bbox (dtm-image tm)))))
(setq * (list approx-dtm-to-lvcs approx-lvcs-to-dtm-resids))

(approximate-transform (gui::3d-to-2d-projection (gui::top-view)))
(approximate-transform (gui::find-terrain-model (gui::top-view)))
       
|#
(defmethod approximate-transform ((tm gui::gridded-terrain-model))
  (let ((dtm-to-lvcs-transform (dtm-to-lvcs-transform tm)))
    (if (linear-transform-p dtm-to-lvcs-transform)
	dtm-to-lvcs-transform
	(or (get-prop dtm-to-lvcs-transform :approximate-transform)
	    (setf (get-prop dtm-to-lvcs-transform :approximate-transform)
		  (math::make-4x4-transform-approximation dtm-to-lvcs-transform
							  (dtm-bbox (dtm-image tm))))))))

(defun intersect-camera-ray-with-gridded-terrain-model(terrain-model projection 2d-pt &optional estimate)
  ;;(declare (optimize (speed 3)(safety 1)(debug 2) #+cmu(extensions:inhibit-warnings 3)))
  ;;(declare (optimize (speed 0) (space 0) (safety 3) (debug 3)))
  (or 
   (block top
     (let* ((dtm-image (dtm-image terrain-model))
	    (dtm-to-lvcs-transform (dtm-to-lvcs-transform terrain-model))
	    (approx-inverse-projection (inverse-transform (approximate-transform projection)))
	    (approx-lvcs-to-dtm-transform (inverse-transform (approximate-transform terrain-model)))
	    (dtm-estimate (inverse-transform-vector dtm-to-lvcs-transform estimate))
	    (bbox (dtm-bbox dtm-image)))
       (labels ((make-projection (octant-to-dtm-transform)
		  (let ((proj
			 (make-composite-coordinate-projection 
			  (remove nil (list octant-to-dtm-transform dtm-to-lvcs-transform projection)))))
		    (setf (get-prop (inverse-transform proj) :approximate-transform)
			  (list* approx-inverse-projection
				 approx-lvcs-to-dtm-transform
				 (when octant-to-dtm-transform
				   (list (inverse-transform octant-to-dtm-transform)))))
		    (setq *foo* (get-prop (inverse-transform proj) :approximate-transform))
		    proj))
		(make-intersector (octant-to-dtm-transform)
		  (make-instance 'camera-ray-intersector
				 :3d-estimate (inverse-transform-vector octant-to-dtm-transform
									dtm-estimate)
				 :projection (make-projection octant-to-dtm-transform)
				 :uv-pt 2d-pt)))
	 (multiple-value-bind (pt0 pt1) (intersect-bbox (make-intersector nil) bbox)
	   (unless pt1 
	     (debug-print2 2 "intersect-bbox FAILED in intersect-camera-ray-with-gridded-terrain-model~%")
	     (return-from intersect-camera-ray-with-gridded-terrain-model nil)
	     )
	   ;; bind-vector-elements falls thru if pt0 or pt1 is NIL.
	   (bind-vector-elements (x0 y0) pt0
	     (bind-vector-elements (x1 y1) pt1
	       (let* ((dx (- x1 x0)) 
		      (dy (- y1 y0))
		      (octant (compute-octant dx dy))
		      (flip-diagonalization (minusp (* dx dy))))
		 (multiple-value-bind (dtm dtm-min dtm-max x-scale y-scale)
		     (map-dtm-to-octant dtm-image octant)
		   (debug-print2 2 "Octant = ~a deltas= ~a scales=~a~%" 
				 octant (list dx dy) (list x-scale y-scale dtm-min))
		   (let* ((octant-transform (octant-dtm-transform dtm))
			  (scaled-octant-transform (octant-dtm-transform dtm-min))
			  (coarse-to-fine-matrix 
			   (multiply-matrices (transform-matrix (inverse-transform octant-transform))
					      (transform-matrix scaled-octant-transform)))
			  (xs0 (aref (inverse-transform-vector scaled-octant-transform pt0) 0))
			  (coarse-intersector (make-intersector scaled-octant-transform))
			  (fine-intersector (make-intersector octant-transform)))
		     (declare (type dmatrix coarse-to-fine-matrix))
		     #+never
		     (setq *octant-transforms* (list octant-transform scaled-octant-transform dtm dtm-min
						     fine-intersector coarse-intersector 
						     coarse-to-fine-matrix))
		     (img::with-iref-checking
		       (labels ((coarse-z-min-max-fn (ix iy)
				  (declare (fixnum ix iy))
				  (when (and (>= ix 0) (>= iy 0)
					     (< ix (image-x-dim dtm-max))
					     (< iy (image-y-dim dtm-max)))
				    (cv (diref dtm-min ix iy) (diref dtm-max ix iy))))

				(fine-z-min-max-fn (ix iy)
				  (declare (fixnum ix iy))
				  (when (and (>= ix 0) (>= iy 0)
					     (< (1+ ix) (image-x-dim dtm))
					     (< (1+ iy) (image-y-dim dtm)))
				    (let ((z00 (diref dtm ix iy))
					  (z10 (diref dtm (1+ ix) iy))
					  (z01 (diref dtm ix (1+ iy)))
					  (z11 (diref dtm (1+ ix) (1+ iy))))
				      (declare (double-float z00 z10 z01 z11))
				      (cv (min z00 z10 z01 z11)
					  (max z00 z10 z01 z11)))))
				(map-x-coarse-to-fine (x)
				  (+ (* (aref coarse-to-fine-matrix 0 0) x) 
				     (aref coarse-to-fine-matrix 0 3)))
			     
				(facet-intersection-fn (x)
					;#+never
				  (let ((pt (intersect-yz-plane fine-intersector x)))
				    (bind-vector-elements (x y) pt
				      (bind-vector-elements (dx dy dz) 
					  (estimate-direction fine-intersector pt)
					(debug-print2 2 "ray-direction = ~a~%" (list dx dy dz))
					(let ((facet-pt
					       (intersect-ray-with-dtm-cell dtm
									    (floor (+ .001 x))
									    (floor (+ .001 y))
									    pt
									    (icv 1.0 (/ dy dx) (/ dz dx))
									    flip-diagonalization)))
					  (when facet-pt
					    (return-from top 
					      (transform-vector (list octant-transform dtm-to-lvcs-transform)
								facet-pt)))))))))
			 (intersect-camera-ray-with-mesh 
			  coarse-intersector
			  xs0 (image-x-dim dtm-min)
			  #'coarse-z-min-max-fn
			  #'(lambda (x)
			      (let ((xmax (min (floor (map-x-coarse-to-fine (1+ (floor x)))) 
					       (image-x-dim dtm))))
				(intersect-camera-ray-with-mesh
				 fine-intersector (map-x-coarse-to-fine x)
				 xmax 
				 #'fine-z-min-max-fn
				 #'facet-intersection-fn)))))))))))))))
   ;; This is the fall thru if no intersection is found.
   (progn (push (list terrain-model projection 2d-pt estimate) 
		*intersect-camera-ray-with-gridded-terrain-model-miss-list*)
	  nil)))


(defun test-transform-closure (transform selected-pt intersection-pt )
  (math::g- (transform-vector transform selected-pt)
	    (transform-vector transform intersection-pt)))
  

#|
(st:load-system :terrain-models)

(defun set-glbls ()
  (setq *tm* (gui::find-gridded-terrain-model (gui::top-view))
      *proj* (gui::3d-to-2d-projection (gui::top-view))
      *pt* (gui::selected-object-world-position)
      *img-pt* (transform-vector *proj* *pt*)
      *dtm-to-lvcs* (dtm-to-lvcs-transform *tm*)
      *dtm-pt* (inverse-transform-vector *dtm-to-lvcs* *pt*)
      *proj2e* (make-composite-coordinate-projection (list *dtm-to-lvcs* *proj*))
      *ci* (make-instance 'camera-ray-intersector
                          :projection *proj2e*
			  :uv-pt (transform-vector *proj* *pt*))
      
      ))

(transform-vector *proj2e* *dtm-pt*)

(dtm-image (gui::find-terrain-model (gui::top-view)))  402x466
(dtm-min-max-images  (dtm-image (gui::find-terrain-model (gui::top-view)))) 21x23
(set-glbls)
(setq  *debugging-printout* t)
(setq  *debugging-printout* 1)
(setq  *debugging-printout* nil)

(trace iterative-inverse-intersect-ray-with-plane)
(untrace)
(let ((*iterative-inverse-intersect-ray-with-plane-verbose* t)
      )
  (intersect-bbox *ci* (dtm-bbox (dtm-image *tm*))))

(describe *ci*)
(let ((foo (set-glbls))
      (intersection-pt (intersect-camera-ray-with-gridded-terrain-model *tm* *proj* (transform-vector *proj* *pt*) 
							    *pt*)))
  (when intersection-pt
    (list intersection-pt *pt* (test-transform-closure *proj* *pt* intersection-pt))))

(list (length gui::*move-object-uv-on-dtm-miss-list*) gui::*move-object-uv-on-dtm-count*)
(setq gui::*move-object-uv-on-dtm-miss-list* nil gui::*move-object-uv-on-dtm-count* 0)
(loop for (tm proj 2dpt) in gui::*move-object-uv-on-dtm-miss-list*
      for i from 0
      collect (list i 2dpt))
(setq *iterative-inverse-intersect-ray-with-plane-verbose* t)
(setq *iterative-inverse-intersect-ray-with-plane-verbose* nil)
(setq *debugging-printout* nil)
(setq *debugging-printout* 1)
(let ((*debugging-printout* 3)
      ;(*iterative-inverse-intersect-ray-with-plane-verbose* 1)
      )
  (set-glbls)
  (destructuring-bind (tm proj 2d-pos 3d-est) (nth 0 gui::*move-object-uv-on-dtm-miss-list*)
    (let ((intersection-pt (intersect-camera-ray-with-gridded-terrain-model tm proj 2d-pos 3d-est)))
      (setq *2d-pos* 2d-pos)
      (list intersection-pt
	    2d-pos (transform-vector proj intersection-pt)))))

(trace iterative-inverse-intersect-ray-with-plane)
(untrace)
*pt*
(transform-vector (3d-to-2d-projection (gui::top-view)) *pt*)

(inverse-intersect-ray-with-plane (inverse-transform *proj*)
				  (transform-vector *proj* *pt*)
				  (cv 0.0 0.0 -1.0 288.7) 
				  *pt*)

(inverse-intersect-ray-with-plane (inverse-transform *proj*)
				  (transform-vector *proj* *pt*)
				  (cv 0.0 -1.0 0.0 0.0) 
				  *pt*)

(#<CME::NON-LINEAR-MAPPED-REGULAR-GRID-TERRAIN-MODEL {58702995}>
 #<FRAME-CAMERA (#<3D-WORLD "Fort Hood 2" #X610B9435> to #<2D-WORLD "fhn711" #X586AE725>) #X58E3BFCD>
 #(1914.5748968668986 4542.0679272913185 0.9987814307000141))
  
(#(42.9999154812717 -148.2462740103772 290.3526136565238)
 #(1914.5748968668986 4542.0679272913185 0.9987814307000141)
 #(1914.5745531998853 4542.068062018215 0.9987808907446085))

  Octant = 3 deltas= (-3.782628193860546 2.106271870593389) scales=(20 21)
cell hit (60 62) 60.0 (322.1981383395675 259.84369542843973) (289.0 291.0)
ray-direction = (0.011290065525888179 0.006286620159631 -0.9999165029278144)
intersect-ray-with-dtm-cell (60 62 #(60.0 62.60796856867562 322.1981383395675)
                             #(1.0 0.5568276061122717 -88.56604956233431)) #(60.36161927797731 62.80932816555578 290.1709474435331)

(list *2d-pos*
      (loop for v in (list (cv 2.0 2.4775355220090454 2004.9169930136763)
			   (cv 3.0 3.007847436892699 233.6320887653249)
			   )
	    collect (transform-vector (nth 1 *octant-transforms*) v)))

(list *2d-pos*
      (loop for v in (list (cv 60.36161927797731 62.80932816555578 290.1709474435331)
			   )
	    collect (transform-vector (nth 0 *octant-transforms*) v)))

(list
 (transform-vector (nth 2 *octant-transforms*) (cv 60.36161927797731 62.80932816555578 290.1709474435331))
 (transform-vector (nth 3 *octant-transforms*) (cv 2.0 2.4775355220090454 2004.9169930136763))
 (transform-vector (nth 3 *octant-transforms*) (cv 3.0 3.007847436892699 233.6320887653249)))

(let ((*print-array* t))
  (describe (transform-matrix (image-to-2d-transform (nth 4 *octant-transforms*))))
  (describe (transform-matrix (image-to-2d-transform (nth 5 *octant-transforms*)))))
(let ((*print-array* t))
  (describe (transform-matrix (car (transform-list (nth 0 *octant-transforms*)))))
  (describe (transform-matrix (car (transform-list (nth 1 *octant-transforms*))))))

(list (transform-list (nth 0 *octant-transforms*)) 
      (transform-list (nth 1 *octant-transforms*)))

(destructuring-bind (octant-to-2d-projection scaled-octant-to-2d-projection
					     octant-transform scaled-octant-transform dtm dtm-min
					     fine-intersector coarse-intersector)
    *octant-transforms*
  (list (transform-vector scaled-octant-transform (cv 2.0 3.0 0.0))
	(transform-vector octant-transform (cv 40.0 63.0 0.0))
	(transform-vector scaled-octant-to-2d-projection (cv 2.0 3.0 0.0))
	(transform-vector octant-to-2d-projection (cv 40.0 63.0 0.0))
	dtm-min
	(transform-matrix (image-to-2d-transform dtm-min))  
	(transform-matrix (image-to-2d-transform (image-neg-x dtm-min)))
	dtm
	(transform-matrix (image-to-2d-transform dtm))
	(transform-matrix (image-to-2d-transform (image-neg-x dtm)))
	))

(inverse-transform (nth 1 *octant-transforms*))
(trace iterative-inverse-intersect-ray-with-plane)

(setq *Newton-Raphson-inverse-transform-vector-verbose* t)

(let ((*Newton-Raphson-inverse-transform-vector-verbose* t)
      (*Newton-Raphson-inverse-transform-vector-very-verbose* t))
  (intersect-yz-plane (nth 7 *octant-transforms*) 2.0))

(setq *dtm-gnd-pt*
      (intersect-camera-ray-with-plane *proj2e* (transform-vector *proj* *pt*) 
				       (cv 0.0 0.0 -1.0 289.3223756376133)))

#(362.8950410418947 67.5931051522943 289.3223756376133)

(dtm-image *tm*)

(setq *proj2e* (make-composite-coordinate-projection (list *dtm-to-lvcs* *proj*)))

(defun time-trans (n trans pt)
  (loop with to-pt = (make-coordinate-vector 3)
	repeat n do (transform-vector trans pt to-pt)))

(defun time-trans2 (n trans pt)
  (loop with to-pt = (make-coordinate-vector 3)
	repeat n do (utm-to-lat-long-transform-vector trans pt to-pt)))

(setq * transforms::*proj2e*)
(time (time-trans 1000000 *proj2e* (cv 0.0 0.0 0.0)))
; cmu 2.0us 0 bytes per iter
; acl 1.5us 0 bytes per iter

(transform-vector *proj2e* (cv 0.0 0.0 0.0))
(inverse-transform-vector *proj2e* (transform-vector *proj2e* (cv 0.0 0.0 0.0)))
(transform-vector (nth 1 (transform-list *proj2e*)) (cv 0.0 0.0 0.0))
(setq * (nth 1 (transform-list *proj2e*)))
(setf (coordinate-transform-transform-function (nth 1 (transform-list *proj2e*)))
      'utm-to-lat-long-transform-vector)
(setf (coordinate-transform-transform-function (nth 1 (transform-list *proj2e*)))
      'lat-long-to-utm-transform-vector)

(let* ((n 5)
       (pt (loop repeat n
		for trans in (transform-list *proj2e*)
		with pt = (cv 0.0 0.0 0.0)
		do (setq pt (transform-vector trans pt))
		finally (return pt))))
  (time (time-trans 1000000 (nth n (transform-list *proj2e*)) pt)))
; cmu n=0 =>  .2 us   0 bytes
; acl n=0 =>  .11 us  0 bytes
; cmu n=1 =>  .63 us 0 bytes per iter
; acl n=1 =>  .59 us 0 bytes per iter
; acl n=2 =>  .29 us 0 bytes per iter

(setq *proj2ei* (inverse-transform *proj2e*))
(transform-vector (nth 5 (transform-list *proj2ei*)) (transform-vector *proj2e* (cv 0.0 0.0 0.0)))
(nth 4 (transform-list *proj2ei*))

(let* ((n 3)
       (pt (loop repeat n
		for trans in (transform-list *proj2ei*)
		with pt = (transform-vector *proj2e* (cv 0.0 0.0 0.0))
		do (setq pt (transform-vector trans pt))
		finally (return pt))))
  (time (time-trans 1000000 (nth n (transform-list *proj2ei*)) pt)))


(let ((pt (loop repeat 1 
		for trans in (transform-list *proj2e*)
		with pt = (cv 0.0 0.0 0.0)
		do (setq pt (transform-vector trans pt))
		finally (return pt))))
  (time (time-trans2 1000000 (nth 1 (transform-list *proj2e*))  pt))) .49us 0 bytes

(intersect-yz-plane *ci* 401.0) 
(intersect-yz-plane *ci* 0.0) 
(intersect-bbox *ci* (dtm-bbox (dtm-image *tm*)))
   
(let ((intersection-pt (intersect-camera-ray-with-gridded-terrain-model *tm* *proj* (transform-vector *proj* *pt*))))
  (list intersection-pt *pt* (test-transform-closure *proj* *pt* intersection-pt)))

(set-glbls)
(defun time-intersect (n)
  (let ((*debugging-printout* nil)
	(2d-pt (transform-vector *proj* *pt*)))
    (time (loop repeat n 
	    do (intersect-camera-ray-with-gridded-terrain-model *tm* *proj* 2d-pt)))))
(set-glbls)
(time (time-intersect 1000))
;; cmu  .84 ms 24.5 kbytes              .41ms 10.4 kB using :approximate-lvcs-to-dtm-transform
;; acl  .97 ms 36.8   kbytes

;;; intersect-camera-ray-with-gridded-terrain-model without doing the actual search
cmu  .43 ms  13.1 kbytes
acl 1.04 ms  73   kbytes
;;; intersect-camera-ray-with-gridded-terrain-model empty fine search
cmu .69 ms   18.4 kbytes
acl 1.57    112.7 kbytes
;;; intersect-camera-ray-with-gridded-terrain-model with empty facet-intersector
cmu 1.07 ms  30  kbytes
acl 2.55     189 kbytes

(transform-path (from-coordinate-system (nth 3 *octant-transforms*))
		(from-coordinate-system *proj*))

|#
