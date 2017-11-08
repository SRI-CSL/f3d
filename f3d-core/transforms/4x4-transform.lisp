(IN-PACKAGE :transforms)


;;;  **********************   4X4-COORDINATE-TRANSFORM  ***************************

;;; Should this be renamed to 3D-COORDINATE-TRANSFORM or LINEAR-COORDINATE-TRANSFORM?

;;; Homogeneous transform from 3d to 3d coordinate systems.
;;; This class is also used to implement transforms from 3d to 2d coordinate systems.
;;;   In this case the 3rd row and column of the matrix is <0 0 1 0>.

(defstruct-class 4x4-coordinate-transform
    (coordinate-transform)
  ((transform-matrix :initform nil :initarg :transform-matrix
		     :accessor transform-matrix)
   (scale-factor :initform 1.0 :accessor scale-factor)))

;;; FIXME: there are more slots to copy
;; this name should be avoided
(defmethod copy ((transform 4x4-coordinate-transform))
  (with-class-slots 4x4-coordinate-transform (transform-matrix scale-factor) transform
      (make-instance '4x4-coordinate-transform
		     :transform-matrix (copy-matrix transform-matrix (make-4x4-matrix))
		     :scale-factor scale-factor)))
	

(defmethod linear-transform-p ((coordinate-transform 4x4-coordinate-transform))
  t)

(defmethod initialize-instance :after ((transform 4x4-coordinate-transform)
                                       &rest args
                                       &key ((:transform-matrix new-transform-matrix))
                                       from-coordinate-system to-coordinate-system
                                       (create-inverse t) &allow-other-keys)
  (with-class-slots
      4x4-coordinate-transform (transform-matrix transform-function inverse-transform) transform
    (let* ((transform-matrix2 (make-4x4-matrix)))
      (if new-transform-matrix
          (math::copy-array-contents new-transform-matrix transform-matrix2)
	  (make-4x4-identity-matrix transform-matrix2))
      (setf transform-matrix transform-matrix2)

      (unless transform-function (setq transform-function '4x4-coordinate-transform-transform-vector))

      (when create-inverse
        (setf inverse-transform
              (apply 'make-instance '4x4-coordinate-transform
                     :transform-matrix (math::invert-matrix transform-matrix)
                     :inverse-transform transform
                     :create-inverse nil
                     :from-coordinate-system to-coordinate-system
                     :to-coordinate-system from-coordinate-system
                     args)))
      transform)))

;;;  This is really a 3d-homogeneous transform,
;;;  ie. mapping 3d coordinates using a 4x4 homogeneous matrix.

(defun make-4x4-coordinate-transform
    (4x4-matrix &rest args &key from-coordinate-system to-coordinate-system cache-p 
                &allow-other-keys)
  (ignore from-coordinate-system to-coordinate-system)
  (when cache-p (error "cache-p option no longer supported"))
  (apply 'make-instance ; (if cache-p 'make-cached-instance 'make-instance)
         '4x4-coordinate-transform
         :transform-matrix 4x4-matrix
         :transform-function '4x4-coordinate-transform-transform-vector
         args
         ))

;;;  Function to perform 4x4 homogeneous matrix times vector
;;;  transform-matrix, from-vector, and to-vector must be simple-arrays of
;;;      double-floats.
;;;  FROM-VECTOR MAY HAVE LENGTH 2 or 3.
;;;  TO-VECTOR   MAY HAVE LENGTH 2 or 3.

;;; Might speed this up using underlying-simple-vector and 1d-arefs.
;;; In CME-6 this required having the value of the underlying-simple-vector
;;; stored in the coordinate-transform's struct.
;;; This would be worth the bother when transforming a collection of vectors.
;;; The macro INLINE-MATRIX-TIMES-VECTOR might be able to use 1d-arefs.

;(disassemble '4x4-coordinate-transform-transform-vector)
(define-coordinate-transform-method transform-vector
    ((4x4-transform 4x4-coordinate-transform) from-vector to-vector)
  (declare (type (or null coordinate-vector) from-vector))
  (declare (type coordinate-vector to-vector))
  #+cmu(declare (ext:optimize-interface (speed 3) (safety 2)))
  (declare (optimize (speed 3) (safety 0)))
  (when from-vector
    (let* ((from-vector from-vector)
	   (to-length (length to-vector))
	   (from-length (length from-vector)))
      (declare (fixnum to-length from-length))
      (declare (type coordinate-vector from-vector))
      (with-class-slot-values 4x4-coordinate-transform
	    (transform-matrix) 4x4-transform
	(declare (type 4x4-matrix transform-matrix))
	(cond ((>= to-length 3)
	       (cond ((>= from-length 3)
		      (bind-vector-elements (x y z) from-vector
			(inline-matrix-times-vector
			 transform-matrix (x y z 1.0)
			 ((aref to-vector 0) (aref to-vector 1) (aref to-vector 2)))))
		     ((= from-length 2)
		      (bind-vector-elements (x y) from-vector
			(inline-matrix-times-vector
			 transform-matrix (x y 0.0 1.0)
			 ((aref to-vector 0) (aref to-vector 1) (aref to-vector 2)))))
		     (t (error "From-vector ~a must have length >= 2" from-vector)) )
	       
	       ;; does anyone really care about element 3 anymore?
	       (when (>= to-length 4)
		 (setf (aref to-vector 3) 1.0))
	       to-vector)
          
	      ((= to-length 2)
	       (cond ((= from-length 2)
		      (bind-vector-elements (x y) from-vector
			(inline-matrix-times-vector
			 transform-matrix (x y 0.0 1.0)
			 ((aref to-vector 0) (aref to-vector 1)))
			to-vector ))
                
		     ((>= from-length 3)
		      (bind-vector-elements (x y z) from-vector
			(inline-matrix-times-vector
			 transform-matrix (x y z 1.0)
			 ((aref to-vector 0) (aref to-vector 1)))
			to-vector ))
		     (t (error "From-vector ~a must have length >= 2" from-vector))))
              
	      (t (error "To-vector ~a must have length >= 2" to-vector)))))))

;;; This method is wrong for 4x4-coordinate-projections.
;;; Bacause of the screwed up class hierarchy, we must make sure this isn't used for
;;; 4x4-coordinate-projections.

(defmethod transform-direction-vector
	   ((transform 4x4-coordinate-transform)
	    direction-vector &optional position-vector into-vector epsilon)
  (declare (ignore position-vector epsilon))
  (4x4-transform-direction-vector transform direction-vector into-vector))

(defun 4x4-transform-direction-vector (4x4-transform direction-vector
				       &optional to-vector)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (or null (simple-array double-float (*))) direction-vector to-vector))
  (when direction-vector
    (let* ((from-vector direction-vector)
	   (to-vector (or to-vector (make-coordinate-vector 3)))
	   (z 0.0)
	   (to-z 0.0d0))
      (declare (type (simple-array double-float (*))
		     from-vector  to-vector))
      (declare (double-float to-z))
      (with-class-slot-values 4x4-coordinate-transform
	    (transform-matrix) 4x4-transform
	(declare (type 4x4-matrix transform-matrix))
	(bind-vector-elements (x y) from-vector
	  (when (>= (length from-vector) 3) (setf z (aref from-vector 2)))
	  (inline-matrix-times-vector transform-matrix (x y z 0.0)
				      ((aref to-vector 0) (aref to-vector 1) to-z))
	  (when (>= (length to-vector) 3) (setf (aref to-vector 2) to-z)))
	
	to-vector))))

(defmethod update-transform ((coordinate-transform 4x4-coordinate-transform))
  (with-class-slots
      4x4-coordinate-transform (inverse-transform) coordinate-transform
    (when inverse-transform
      ;;(format t "updating inverse transform~%")
      (math:invert-matrix (transform-matrix coordinate-transform)
			  (transform-matrix inverse-transform))
      )))

(defmethod set-transform-matrix ((coordinate-transform 4x4-coordinate-transform)
				 new-matrix)
  (copy-matrix new-matrix (transform-matrix coordinate-transform))
  (update-transform coordinate-transform)
  new-matrix)

(defmethod post-multiply ((coordinate-transform 4x4-coordinate-transform) 4x4-mat &optional (update nil))
  (with-class-slots 4x4-coordinate-transform (transform-matrix) coordinate-transform
    (multiply-matrices transform-matrix 4x4-mat transform-matrix)
    (when update (update-transform coordinate-transform))))


(defmethod inverse-transform-matrix ((coordinate-transform 4x4-coordinate-transform))
  (transform-matrix (inverse-transform coordinate-transform)))

(defmethod transform-jacobian ((coordinate-transform 4x4-coordinate-transform)
			       position &optional mat3x3)
  (declare (optimize (speed 3) (safety 1)))
  (declare (ignore position))
  (unless mat3x3 (setq mat3x3 (make-array '(3 3) :element-type 'double-float)))

  (let ((from (transform-matrix coordinate-transform))
	(to mat3x3))
    (declare (type 4x4-matrix from)
	     (type (simple-array double-float (3 3)) to))
    (loop for row fixnum from 0 below 3
	  do (loop for col fixnum from 0 below 3
		   do (setf (aref to row col) (aref from row col)))))
  mat3x3)
    

;;;
;;; Compose transformations so as to apply trans1 followed by trans2:
;;;
(defmethod compose-transforms ((trans1 4x4-coordinate-transform)
			       (trans2 4x4-coordinate-transform))
  (make-4x4-coordinate-transform
   (multiply-matrices (transform-matrix trans2)
		      (transform-matrix trans1))
   :from-coordinate-system (from-coordinate-system trans1)
   :to-coordinate-system (to-coordinate-system trans2)))

(defun multiply-4x4-transform-matricies (4x4-transforms)
  (cond ((null 4x4-transforms) 
	 (make-4x4-identity-matrix)) ; should this be a constant allocated identity matrix?
	((not (listp 4x4-transforms))
	 (transform-matrix 4x4-transforms))
	((null (cdr 4x4-transforms))
	 (transform-matrix (car 4x4-transforms)))
	(t
	 (loop with result-mat = (copy-4x4-matrix (transform-matrix (car 4x4-transforms)))
	       for trans in (cdr 4x4-transforms)
	       do (math::multiply-4x4-matrices (transform-matrix trans) result-mat result-mat)
	       finally (return result-mat)))))


;;; UNUSED
;;; This doesn't really belong here, but ...
;;;(defun 4x4-matrix-transform-vector (4x4-matrix vect &optional (into-vect))
;;;  (declare (type (simple-array double-float (*)) vect))
;;;  (declare (type (or null (simple-array double-float (*))) into-vect))
;;;  (declare (type 4x4-matrix 4x4-matrix))
;;;  #+cmu(declare (ext::optimize-interface (speed 2) (safety 3)))
;;;  (declare (optimize (speed 3) (safety 0)))
;;;  (unless into-vect (setq into-vect (make-coordinate-vector 4)))
;;;  (bind-vector-elements (x y z) vect
;;;    (inline-matrix-times-vector 4x4-matrix
;;;                                (x y z 1.0)
;;;                                ((aref into-vect 0) (aref into-vect 1) (aref into-vect 2) (aref into-vect 3)))
;;;    into-vect))

