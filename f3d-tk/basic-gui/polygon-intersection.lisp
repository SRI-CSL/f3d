(in-package :gui)

;;; This file is currently unsued by FREEDIUS.

;;; Some code has been moved to other files.

;;; Contains no direct dependencies on TK or OpenGL.

(defun compute-bounding-box (vertices)
  (when (and vertices (> (length vertices) 0))
    (if (= (length (first vertices)) 3)
	(let* ((xmin most-positive-single-float) (ymin xmin) (zmin xmin)
	       (xmax most-negative-single-float) (ymax xmax) (zmax xmax))
	  (declare (double-float xmin ymin zmin xmax ymax zmax))
	  (loop for vertex in vertices
		when vertex 
		  do (bind-vector-elements (x y z) vertex
		       (when (< x xmin) (setq xmin x))
		       (when (> x xmax) (setq xmax x))
		       (when (< y ymin) (setq ymin y))
		       (when (> y ymax) (setq ymax y))
		       (when (< z zmin) (setq zmin z))
		       (when (> z zmax) (setq zmax z)))
		finally (return (cv xmin xmax ymin ymax zmin zmax))))
	
	(let* ((xmin most-positive-single-float) (ymin xmin) 
	       (xmax most-negative-single-float) (ymax xmax))
	  (declare (double-float xmin ymin xmax ymax))
	  (loop for vertex in vertices
		when vertex 
		  do (bind-vector-elements (x y) vertex
		       (when (< x xmin) (setq xmin x))
		       (when (> x xmax) (setq xmax x))
		       (when (< y ymin) (setq ymin y))
		       (when (> y ymax) (setq ymax y)))
		finally (return (cv xmin xmax ymin ymax)))))))



(defun 2d-bbox-intersect-p (bbox1 bbox2)
  (cme::bind-vector-elements (l1 r1 b1 t1) bbox1
    (cme::bind-vector-elements (l2 r2 b2 t2) bbox2
      (not (or (< r1 l2) (> l1 r2) (< t1 b2) (> b1 t2))))))

(defun 3d-bbox-intersect-p (bbox1 bbox2)
  (bind-vector-elements (xmin1 xmax1 ymin1 ymax1 zmin1 zmax1) bbox1
    (bind-vector-elements (xmin2 xmax2 ymin2 ymax2 zmin2 zmax2) bbox2
      (not (or (< xmax1 xmin2) (> xmin1 xmax2)
	       (< ymax1 ymin2) (> ymin1 ymax2)
	       (< zmax1 zmin2) (> zmin1 zmax2))))))

(defun 2d-bbox-totally-inside-bbox (bbox1 bbox2)
  (bind-vector-elements (xmin1 xmax1 ymin1 ymax1 ) bbox1
    (bind-vector-elements (xmin2 xmax2 ymin2 ymax2) bbox2
      (and (>= xmin1 xmin2) (<= xmax1 xmax2)
	   (>= ymin1 ymin2) (<= ymax1 ymax2)))))


;;; **********************  CLIP-PLANES  **********************

(defun vector-intersect-line-with-plane (p0 p1 plane-vector &optional into-vector)
  (bind-vector-elements (x0 y0 z0) p0
    (bind-vector-elements (x1 y1 z1) p1
      (bind-vector-elements (nx ny nz n1) plane-vector
	(let* ((dx (- x1 x0))
	       (dy (- y1 y0))
	       (dz (- z1 z0))
	       (num (+ n1 (inline-inner-prod (x0 y0 z0) (nx ny nz)) ))
	       (denom (inline-inner-prod (dx dy dz) (nx ny nz)))
	       (alpha 0.0))
	  (declare (double-float dx dy dz denom num alpha ))
	  (unless (= denom 0.0)
	    (setq alpha (- (/ num denom )))
	    ;;(format t "vector-intersect-line-with-plane alpha = ~a~%" alpha)
	    (values (set-coordinate-vector-elements (or into-vector (make-coordinate-vector 3))
						    (+ x0 (* alpha dx))
						    (+ y0 (* alpha dy))
						    (+ z0 (* alpha dz)))
		    alpha )))))))

(defun make-plane (dir pt)
  (let ((dot (vector-inner-product dir pt)))
    (bind-vector-elements (dx dy dz) (normalize-coordinate-vector dir)
      (cv dx dy dz (- dot)))))

(defun compute-side (plane pt)
  (bind-vector-elements (nx ny nz n1) plane
    (bind-vector-elements (x y z) pt
      (+ (* x nx) (* y ny) (* z nz) n1))))

(defparameter *intersection-epsilon* 1e-5)
(defun intersect-3d-polygon-with-half-space (poly plane)
  (when (>= (length poly) 3)
    (bind-vector-elements (nx ny nz n1) plane
      (macrolet ((compute-side (p)
			       `(bind-vector-elements (x y z) ,p
				  (+ (* x nx) (* y ny) (* z nz) n1))))
	   
	(loop for p0 first (car (last poly)) then p1
	      for side-0 double-float first (+ *intersection-epsilon*
					       (compute-side p0))
		then side-1
	      for p1 in poly
	      for side-1 double-float = (compute-side p1)
	      with pt
	      do (format t "~a ~a~%" side-0 side-1)
	      when (> side-0 0.0)
		collect p0
	      when (or (and (= side-0 0) (> side-1 0.0))
		       (< (* side-0 side-1) 0.0))
		do (setq pt (vector-intersect-line-with-plane p0 p1 plane))
		   (unless pt (break))
		and collect pt
	      )))))

(defun intersect-3d-polygon-with-clip-planes (poly planes)
  (loop for plane in planes
	do (setq poly (intersect-3d-polygon-with-half-space poly plane))
	finally (return poly)))

(defun bbox-to-clip-planes (bbox)
  (bind-vector-elements (xmin xmax ymin ymax) bbox
    (list (make-plane (cv 1.0 0.0 0.0 ) (cv xmin ymin 0.0))
	  (make-plane (cv -1.0 0.0 0.0 ) (cv xmax ymin 0.0))
	  (make-plane (cv 0.0 1.0 0.0 ) (cv xmin ymin 0.0))
	  (make-plane (cv 0.0 -1.0 0.0 ) (cv xmin ymax 0.0))
    )))

;;; verts is a list of 3d vertices in screen space (3rd component is normalized z).
(defun clip-tri-strip-to-bbox (verts bbox)
  (let ((tri-strip-bbox (compute-bounding-box verts))
	(clip-planes (bbox-to-clip-planes bbox)))
    (when (2d-bbox-intersect-p tri-strip-bbox bbox)
      (if (2d-bbox-totally-inside-bbox tri-strip-bbox bbox)
	  (values verts :tri-strip)
	  (values (loop with verts = verts
			with (clipped-tri0 clipped-tri1)
			for v0 first (pop verts) then v2
			for v1 first (pop verts) then v3
			for v2 = (pop verts)
			for v3 = (pop verts)
			for tri0 = (list v0 v1 v2)
			for tri1 = (list v1 v3 v2)
			while v2
			do (setq foo0 (list tri0 v0 v1 v2 v3))
			   (setq clipped-tri0
				 (intersect-3d-polygon-with-clip-planes tri0
					   clip-planes))
			when clipped-tri0
			  collect clipped-tri0
			while v3
			do (setq foo1 tri1)
			do (setq clipped-tri1
				 (intersect-3d-polygon-with-clip-planes tri1
					   clip-planes))
			when clipped-tri1
			  collect clipped-tri1
			)
		  :poly-list
		  )))))

#|


(setq poly (list (cv 0.0 0.0 0.0) (cv 1.0 0.0 0.0) (cv 0.0 1.0 0.0)))

(intersect-3d-polygon-with-clip-planes poly
				       (bbox-to-clip-planes (cv .1 .9 .1 .85)))

(intersect-3d-polygon-with-half-space
 (list (CV 0.9 0.1 0.0)
       (CV 0.09999999999999998 0.9 0.0)
       (CV 0.1 0.09999999999999998 0.0))
 (make-plane (cv 0.0 -1.0 0.0 ) (cv .1 .9 0.0)))

(clip-tri-strip-to-bbox (list (cv 0.0 1.0 0.0)
			      (cv 0.0 0.0 0.0)
			      (cv 1.0 1.0 0.0)
			      (cv 1.0 0.0 0.0))
			(cv .1 .9 .1 .85))


       
 
(intersect-3d-polygon-with-half-space
 poly
 (make-plane (cv -1.0 0.0 0.0) (cv .5 0.0 0.0)))

(compute-side (make-plane (cv 1.0 0.0 0.0) (cv .5 0.0 0.0))
	      (cv 0.0 1.5 0.0))

(make-plane (cv 1.0 0.0 0.0) (cv .5 0.0 0.0))

|#
