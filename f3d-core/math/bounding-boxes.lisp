(in-package :math)

;;; This is replicated from macros-gl-objects.  This should be fixed.

(deftype vertex-element-type () 'double-float)
(deftype vertex-array-type () '(simple-array vertex-element-type (* *)))
(deftype vertex-index-type () '(unsigned-byte 16))
(deftype vertex-index-array-type () '(simple-array vertex-index-type (*)))

;;(deftype bbox (&optional (n '*)) `(dvector ,n))

(defun bbox-dims (bbox) 
  (declare (type dvector bbox))
  (apply #'coordinate-vector-general
	 (loop for i fixnum from 0 below (length bbox) by 2
	       collect (- (aref bbox (1+ i)) (aref bbox i)))))

(defun bbox-center (bbox) 
  (declare (type dvector bbox))
  (apply #'coordinate-vector-general
	 (loop for i fixnum from 0 below (length bbox) by 2
	       collect (* .5 (+ (aref bbox (1+ i)) (aref bbox i))))))

(defun 2d-point-in-bbox (2d-pt 2d-bbox)
  (declare (optimize (speed 3) (safety #+cmu 0 #-cmu 1)))
  (declare (type dvector 2d-pt 2d-bbox))
  (bind-vector-elements (u v) 2d-pt
    (bind-vector-elements (umin umax vmin vmax) 2d-bbox
      (and  (<= umin u umax)(<= vmin v vmax)))))

;(disassemble 'point-in-bounding-box)
(defun point-in-bounding-box (point bbox)
  (declare (optimize (speed 3) (safety #+cmu 0 #-cmu 1)))
  (declare (type dvector point bbox))
  (loop for i fixnum from 0 below (length point)
	for j fixnum from 0 below (length bbox) by 2
	always (<= (aref bbox j) (aref point i) (aref bbox (1+ j)))))

	
(defmethod union-bounding-boxes (&rest bboxes)
  (declare (optimize (speed 3) (safety #+cmu 0 #-cmu 1)))
  (let* ((result-bbox (vector-copy (car bboxes)))
	 (n (length result-bbox)))
    (declare (type (simple-array double-float (*)) result-bbox)
	     (fixnum n))
    (loop for bbox in (cdr bboxes)
	  do (let ((bbox bbox))
	       (declare (type (simple-array double-float (*)) bbox))
	       (loop for i fixnum from 0 below n by 2
		     for i+1 fixnum from 1 by 2
		     do (setf (aref result-bbox i) (min (aref result-bbox i) (aref bbox i))
			      (aref result-bbox i+1) (max (aref result-bbox i+1) (aref bbox i+1))))))	    
    result-bbox))
	
;(fmakunbound 'destructive-union-bounding-boxes)
;;; modifies first bbox
(defmethod destructive-union-bounding-boxes (into-bbox &rest bboxes)
  (declare (optimize (speed 3) (safety #+cmu 0 #-cmu 1)))
  (let* ((result-bbox (or into-bbox (vector-copy (pop bboxes))))
	 (n (length result-bbox)))
    (declare (type (simple-array double-float (*)) result-bbox)
	     (fixnum n))
    (loop for bbox in bboxes
	  do (let ((bbox bbox))
	       (declare (type (simple-array double-float (*)) bbox))
	       (loop for i fixnum from 0 below n by 2
		     for i+1 fixnum from 1 by 2
		     do (setf (aref result-bbox i) (min (aref result-bbox i) (aref bbox i))
			      (aref result-bbox i+1) (max (aref result-bbox i+1) (aref bbox i+1))))))	    
    result-bbox))

;(disassemble 'intersect-bounding-boxes)
(defun intersect-bounding-boxes (&rest bboxes)
  (declare (optimize (speed 3) (safety #+cmu 0 #-cmu 1)))
  (let* ((result-bbox (vector-copy (car bboxes)))
	 (n (length result-bbox)))
    (declare (type dvector result-bbox)
	     (fixnum n))
    (loop for bbox in (cdr bboxes)
	  do (let ((bbox bbox))
	       (declare (type dvector bbox))
	       (loop for min-i fixnum from 0 below n by 2
		     for max-i fixnum from 1 by 2
		     do (setf (aref result-bbox min-i) (max (aref result-bbox min-i) (aref bbox min-i))
			      (aref result-bbox max-i) (min (aref result-bbox max-i) (aref bbox max-i)))
		     )))
    result-bbox))

(defun bbox-intersect-p (bbox1 bbox2)
  (declare (optimize (speed 3) (safety #+cmu 0 #-cmu 1)))
  (declare (type dvector bbox1 bbox2))
  (loop for min-i fixnum from 0 below (length bbox1) by 2
	for max-i fixnum from 1 by 2
	always (and (<= (aref bbox2 min-i) (aref bbox1 max-i))
		    (<= (aref bbox1 min-i) (aref bbox2 max-i)))))

;(disassemble 'bbox-totally-inside-bbox)
;;; Returns T iff bbox1 is totally inside bbox2
(defun bbox-totally-inside-bbox (bbox1 bbox2)
  (declare (optimize (speed 3) (safety #+cmu 0 #-cmu 1)))
  (declare (type (dvector 4) bbox1 bbox2))
  (loop for min-i fixnum from 0 below (length bbox1) by 2
	for max-i fixnum from 1 by 2
	always (and (<= (aref bbox1 max-i) (aref bbox2 max-i))
		    (>= (aref bbox1 min-i) (aref bbox2 min-i)))))


(defun vertex-array-bounding-box (vertices &optional bbox)
  (declare (optimize (speed 3) (safety #+cmu 0 #-cmu 1)))
  (declare (type vertex-array-type vertices))
  (declare (type (or null (dvector 6)) bbox))
  (let ((bbox (or bbox (make-coordinate-vector 6))))
    (declare (type (dvector 6) bbox))
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
       (return (math::inline-set-coordinate-vector-elements bbox xmin xmax ymin ymax zmin zmax)))))

(defun compute-bounding-box (vertices)
  (declare (optimize (speed 3) (safety #+cmu 0 #-cmu 1)))
  (when (and vertices (> (length vertices) 0))
    (if (= (length (first vertices)) 3)
	(let* ((xmin most-positive-double-float) (ymin xmin) (zmin xmin)
	       (xmax most-negative-double-float) (ymax xmax) (zmax xmax))
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
	
	(let* ((xmin most-positive-double-float) (ymin xmin) 
	       (xmax most-negative-double-float) (ymax xmax))
	  (declare (double-float xmin ymin xmax ymax))
	  (loop for vertex in vertices
		when vertex 
		  do (bind-vector-elements (x y) vertex
		       (when (< x xmin) (setq xmin x))
		       (when (> x xmax) (setq xmax x))
		       (when (< y ymin) (setq ymin y))
		       (when (> y ymax) (setq ymax y)))
		finally (return (cv xmin xmax ymin ymax)))))))


#+never ;; cannot do this because transform-vector isn't defined here
(defun compute-bounding-box (vertices &optional transform)
  (declare (optimize (speed 3) (safety #+cmu 0 #-cmu 1)))
  (when (and vertices (> (length vertices) 0))
    (if (= (length (first vertices)) 3)
	(let* ((xmin most-positive-double-float) (ymin xmin) (zmin xmin)
	       (xmax most-negative-double-float) (ymax xmax) (zmax xmax))
	  (declare (double-float xmin ymin zmin xmax ymax zmax))
	  (loop for vertex in vertices
		when vertex 
		  do (bind-vector-elements (x y z) (if transform
						       (transform-vector transform vertex)
						       vertex)
		       (when (< x xmin) (setq xmin x))
		       (when (> x xmax) (setq xmax x))
		       (when (< y ymin) (setq ymin y))
		       (when (> y ymax) (setq ymax y))
		       (when (< z zmin) (setq zmin z))
		       (when (> z zmax) (setq zmax z)))
		finally (return (cv xmin xmax ymin ymax zmin zmax))))
	
	(let* ((xmin most-positive-double-float) (ymin xmin) 
	       (xmax most-negative-double-float) (ymax xmax))
	  (declare (double-float xmin ymin xmax ymax))
	  (loop for vertex in vertices
		when vertex 
		  do (bind-vector-elements (x y) (if transform
						     (transform-vector transform vertex)
						     vertex)
		       (when (< x xmin) (setq xmin x))
		       (when (> x xmax) (setq xmax x))
		       (when (< y ymin) (setq ymin y))
		       (when (> y ymax) (setq ymax y)))
		finally (return (cv xmin xmax ymin ymax)))))))

