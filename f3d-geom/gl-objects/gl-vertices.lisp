(in-package :obj)

#|
  
Implements:
     face-normal calculation
     arrays of vertices compatible with OpenGL glVertexPointer
     arrays of normal vectors compatible with OpenGL glNormalPointer
|#


(defparameter *gl-vertex-type* GL_DOUBLE)
(defparameter *gl-vertex-bytes-per-element*  8)
(defparameter *gl-vertex-index-type* GL_UNSIGNED_SHORT)

(defun vertex-array-vertex (verts i)
  (declare (type vertex-array-type verts))
  (declare (fixnum i))
  (cv (aref verts i 0) (aref verts i 1) (aref verts i 2)))

(defun set-vertex-array-vertex (verts i new-vertex)
  (declare (type vertex-array-type verts))
  (declare (type coordinate-vector new-vertex))
  (declare (fixnum i))
  (setf (aref verts i 0) (aref new-vertex 0))
  (setf (aref verts i 1) (aref new-vertex 1))
  (setf (aref verts i 2) (aref new-vertex 2))
  new-vertex)


(defsetf vertex-array-vertex set-vertex-array-vertex) 

;;;(defun compute-face-normal (v0 v1 v2)
;;;  (let ((nx 0.0) (ny 0.0) (nz 0.0))
;;;    (declare (double-float nx ny nz))
;;;    (bind-vector-elements (x0 y0 z0) v0
;;;      (bind-vector-elements (x1 y1 z1) v1
;;;        (bind-vector-elements (x2 y2 z2) v2
;;;          (inline-cross-prod ((- x1 x0) (- y1 y0) (- z1 z0))
;;;                             ((- x2 x0) (- y2 y0) (- z2 z0))
;;;                             (nx ny nz))
;;;          (normalize-vector-elements nx ny nz)
;;;          (cv nx ny nz))))))

(defun compute-vertex-array-face-normal (vertices normals vi0 vi1 vi2 ni)
  (declare (type vertex-array-type vertices normals))

  (let ((nx 0.0) (ny 0.0) (nz 0.0))
    (declare (double-float nx ny nz))
    (bind-vertex-array-elements
     (x0 y0 z0) vertices vi0
       (bind-vertex-array-elements
      (x1 y1 z1) vertices vi1
      (bind-vertex-array-elements
       (x2 y2 z2) vertices vi2
       (inline-cross-prod ((- x1 x0) (- y1 y0) (- z1 z0))
			  ((- x2 x0) (- y2 y0) (- z2 z0))
			  (nx ny nz)))))
    ;;(setq foo1 (list nx ny nz))
    (normalize-vector-elements nx ny nz)
    (set-vertex-array-elements normals ni nx ny nz)))
	   
(defun make-vertex-index-array (arg)
  (if (numberp arg)
      (make-array0 arg :element-type 'vertex-index-type)
      (make-array (length arg) :element-type 'vertex-index-type
		  :initial-contents arg)))

;;; vectors is a list of lists of double-floats.
(defun make-vertex-array (&rest vectors)
  (maybe-with-static-area
      (let* ((nvects (length vectors))
	     (ncomps (max 3 (length (car vectors)))); at least 3 elements per vertex
	     (arr (make-array (list nvects ncomps)
			      :element-type 'vertex-element-type
			      :initial-element 0.0)))
	(declare (type vertex-array-type arr))
	(loop for i fixnum from 0
	      for vector in vectors
	      when (consp vector)
		do (loop for j fixnum from 0 below ncomps
			 for elt in vector
			 do (setf (aref arr i j) (dfloat elt)))
	      else when (vectorp vector)
		     do (loop for j fixnum from 0 below ncomps
			      do (setf (aref arr i j)
				       (dfloat (aref vector j))))
	      else do (error "list elements must be lists or vectors of numbers"))
	arr)))

;;; It is the caller's responsibility to copy elements from verts to the new-verts
(defun resize-vertex-array (verts new-nverts)
  (declare (type vertex-array-type verts))
  (let* (;;(nverts (array-dimension verts 0))
	 (ncomps (array-dimension verts 1))
	 (new-verts (make-array0 (list new-nverts ncomps)
				 :element-type 'vertex-element-type)))
    new-verts))

(defun copy-vertex-subarray (verts new-nverts from-i to-i n)
  (declare (type vertex-array-type verts new-nverts))
  (declare (fixnum from-i to-i n))
  (declare (optimize (speed 3) (safety 0)))
  (loop with ncomps fixnum = (array-dimension verts 1)
	for from-i fixnum from from-i below (+ from-i n)
	for to-i fixnum from to-i
	do (loop for j from 0 below ncomps
		 do (setf (aref new-nverts to-i j) (aref verts from-i j)))))
#|
(disassemble 'copy-vertex-subarray)
|#

(defun transform-vertex-array (verts xform &optional into-array)
  (declare (type vertex-array-type verts))
  ;;(declare (optimize (speed 3) (safety 0)))
  (let* ((nverts (array-dimension verts 0))
	 (ncomps (array-dimension verts 1))
	 (new (or into-array
		  (make-array0 (list nverts ncomps)
			       :element-type  'vertex-element-type)))
	 (from (cv 0.0 0.0 0.0))
	 (to (cv 0.0 0.0 0.0)))
    (declare (type vertex-array-type new))
    (loop for i fixnum from 0 below nverts
	  do (loop for j fixnum from 0 below ncomps
		   do (setf (aref from j) (aref verts i j)))
	     (if (functionp xform)
		 (funcall xform from to)
		 (transform-vector xform from to))
	     (set-vertex-array-elements new i (aref to 0) (aref to 1) (aref to 2)))
    new))

;(fmakunbound 'scale-vertex-array)
(defmethod scale-vertex-array ((vertices array) scale-vector)
  (declare (type vertex-array-type vertices))
  (declare (optimize (safety 0) (speed 3)))
  (bind-vector-elements (sx sy sz) scale-vector
    (loop for i fixnum from 0 below (array-dimension vertices 0)
	  do (setf (aref vertices i 0) (* sx (aref vertices i 0))
		   (aref vertices i 1) (* sy (aref vertices i 1))
		   (aref vertices i 2) (* sz (aref vertices i 2))))))

;;;(defun vertex-array-bounding-box (vertices)
;;;  (declare (type vertex-array-type vertices))
;;;  (declare (optimize (safety 0) (speed 3)))
;;;  (let* ((xmin most-positive-double-float) (ymin xmin) (zmin xmin)
;;;         (xmax most-negative-double-float) (ymax xmax) (zmax xmax))
;;;    (declare (double-float xmin ymin zmin xmax ymax zmax))
;;;    (loop for i fixnum from 0 below (array-dimension vertices 0)
;;;          for x double-float = (aref vertices i 0) 
;;;          for y double-float = (aref vertices i 1) 
;;;          for z double-float = (aref vertices i 2) 
;;;          do (when (< x xmin) (setq xmin x))
;;;             (when (> x xmax) (setq xmax x))
;;;             (when (< y ymin) (setq ymin y))
;;;             (when (> y ymax) (setq ymax y))
;;;             (when (< z zmin) (setq zmin z))
;;;             (when (> z zmax) (setq zmax z))
;;;          finally (return (math::inline-coordinate-vector xmin xmax ymin ymax zmin zmax)))))

#+never ; moved to math/bounding.boxes.lisp
(defun vertex-array-bounding-box (vertices)
  (declare (type vertex-array-type vertices))
  (declare (optimize (safety 0) (speed 3)))
  (loop for i fixnum from 0 below (array-dimension vertices 0)
	for x double-float = (aref vertices i 0) 
	for y double-float = (aref vertices i 1) 
	for z double-float = (aref vertices i 2)
	minimize x into xmin double-float
	maximize x into xmax double-float
	minimize y into ymin double-float
	maximize y into ymax double-float
	minimize z into zmin double-float
	maximize z into zmax double-float
	finally 
     (return (math::inline-coordinate-vector xmin xmax ymin ymax zmin zmax))))

;;; moved it back to here so that transform-vector can be used.
(defun vertex-array-bounding-box (vertices &optional bbox transform (reset t))
  (declare (optimize (speed 3) (safety #+cmu 0 #-cmu 1)))
  (declare (type vertex-array-type vertices))
  (declare (type (or null (dvector 6)) bbox))
  (let ((bbox (or bbox (make-coordinate-vector 6)))
	(tmp-vect (and transform (make-coordinate-vector 3))))
    (declare (type (dvector 6) bbox))
    (loop for i fixnum from 0 below (array-dimension vertices 0)
	  for x double-float = (aref vertices i 0) 
	  for y double-float = (aref vertices i 1) 
	  for z double-float = (aref vertices i 2)
	  when transform
	    do (math::inline-set-coordinate-vector-elements tmp-vect x y z)
	       (mv-setq-vector-elements (x y z) (transform-vector transform tmp-vect tmp-vect))
	  minimize x into xmin double-float
	  maximize x into xmax double-float
	  minimize y into ymin double-float
	  maximize y into ymax double-float
	  minimize z into zmin double-float
	  maximize z into zmax double-float
	  finally 
       (return (math::inline-set-coordinate-vector-elements bbox xmin xmax ymin ymax zmin zmax)))))

(defun vertex-array-bounding-box (vertices &optional bbox transform)
  (declare (optimize (speed 3) (safety #+cmu 0 #-cmu 1)))
  (declare (type vertex-array-type vertices))
  (declare (type (or null (dvector 6)) bbox))
  (let ((tmp-vect (and transform (make-coordinate-vector 3))))
    (loop for i fixnum from 0 below (array-dimension vertices 0)
	  for x double-float = (aref vertices i 0) 
	  for y double-float = (aref vertices i 1) 
	  for z double-float = (aref vertices i 2)
	  when transform
	    do (math::inline-set-coordinate-vector-elements tmp-vect x y z)
	       (mv-setq-vector-elements (x y z) (transform-vector transform tmp-vect tmp-vect))
	  minimize x into xmin double-float
	  maximize x into xmax double-float
	  minimize y into ymin double-float
	  maximize y into ymax double-float
	  minimize z into zmin double-float
	  maximize z into zmax double-float
	  finally 
       (return (math::destructive-union-bounding-boxes 
		bbox
		(math::inline-coordinate-vector xmin xmax ymin ymax zmin zmax))))))

#|
(disassemble 'vertex-array-bounding-box)
|#
	

;;;
;;; Not sure where to put these:
;;;

(declaim (special *default-z*))

(defun make-vertex* (x y &optional z id )
  (declare (ignore id))
  (coordinate-vector x y (if (or (null z) (zerop z)) *default-z* z)))

;;; This next is bogus -- it doesn't make a 2d-array
;;;(defun make-vertex-array-from-vertex-list  (vertex-list)
;;;  (let* ((n-verts (length vertex-list))
;;;         (arr (make-array n-verts ;;:fill-pointer n-verts
;;;                          )))
;;;    (loop for i from 0 below n-verts
;;;          for (x y z) in vertex-list
;;;          do (setf (aref arr i) (make-vertex* x y z i)))
;;;    arr))
