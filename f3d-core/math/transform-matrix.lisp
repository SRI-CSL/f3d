(IN-PACKAGE :MATH)

(defun make-and-fill-3x3-matrix (&rest list)
  (declare (dynamic-extent list))
  (let ((m (make-array '(3 3) :element-type 'double-float)))
    (declare (type (simple-array double-float (3 3)) m))
    (loop for row fixnum from 0 below 3
	  do (loop for col fixnum from 0 below 3
		   for elem = (pop list)
		   do (setf (aref m row col) (if elem (dfloat elem) 0.0d0))))
    m))

;;; ********************  4x4-MATRICES  ******************** 

(deftype 4x4-matrix () '(simple-array double-float (4 4)))

(defun make-and-fill-4x4-matrix (&rest list)
  (declare (dynamic-extent list))
  (let ((m (make-array '(4 4) :element-type 'double-float)))
    (declare (type 4x4-matrix m))
    (loop for row fixnum from 0 below 4
	  do (loop for col fixnum from 0 below 4
		   for elem = (pop list)
		   do (setf (aref m row col) (if elem (dfloat elem) 0.0d0))))
    m))

(defun clear-4x4-matrix (m &optional (v 0.0))
  (declare (type 4x4-matrix m))
  (declare (double-float v))
  (declare (optimize (safety 0) (speed 3)))
  (setf (aref m 0 0) v (aref m 0 1) v (aref m 0 2) v (aref m 0 3) v
	(aref m 1 0) v (aref m 1 1) v (aref m 1 2) v (aref m 1 3) v
	(aref m 2 0) v (aref m 2 1) v (aref m 2 2) v (aref m 2 3) v
	(aref m 3 0) v (aref m 3 1) v (aref m 3 2) v (aref m 3 3) v)
  m)

(defun set-4x4-identity-matrix (m)
  (declare (type 4x4-matrix m))
  (declare (optimize (safety 0) (speed 3)))
  (setf (aref m 0 0) 1.0 (aref m 0 1) 0.0 (aref m 0 2) 0.0 (aref m 0 3) 0.0
	(aref m 1 0) 0.0 (aref m 1 1) 1.0 (aref m 1 2) 0.0 (aref m 1 3) 0.0
	(aref m 2 0) 0.0 (aref m 2 1) 0.0 (aref m 2 2) 1.0 (aref m 2 3) 0.0
	(aref m 3 0) 0.0 (aref m 3 1) 0.0 (aref m 3 2) 0.0 (aref m 3 3) 1.0)
  m)

(defun make-4x4-matrix ()
  (clear-4x4-matrix (make-array '(4 4) :element-type 'double-float)))

(defun make-4x4-identity-matrix (&optional into-matrix)
  (set-4x4-identity-matrix (or into-matrix (make-array '(4 4) :element-type 'double-float))))

(defun copy-4x4-matrix (from-mat &optional to-mat)
  (declare (optimize (safety 0) (speed 3)))
  (declare (type 4x4-matrix from-mat))
  (let ((to-mat (or to-mat (make-array '(4 4) :element-type 'double-float))))
    (declare (type 4x4-matrix to-mat))
    (macrolet ((cpy (i j) `(setf (aref to-mat ,i ,j) (aref from-mat ,i ,j))))
      (cpy 0 0) (cpy 0 1) (cpy 0 2) (cpy 0 3)
      (cpy 1 0) (cpy 1 1) (cpy 1 2) (cpy 1 3)
      (cpy 2 0) (cpy 2 1) (cpy 2 2) (cpy 2 3)
      (cpy 3 0) (cpy 3 1) (cpy 3 2) (cpy 3 3))
    to-mat))

;;; 4th row must be <0 0 0 1>
(defun invert-homogeneous-4x4-matrix  (mat &optional inverse)
  (unless inverse (setq inverse (make-4x4-matrix)))
  (if (= 0 (invert_homogeneous_4x4_matrix mat inverse) )
      nil
      inverse))

(defun transpose-4x4-matrix (m &optional mt)
  (declare (type 4x4-matrix m))
  (let ((mt (or mt (make-4x4-matrix))))
    (declare (type 4x4-matrix mt))
    (loop for i fixnum from 0 below 4
	  do (loop for j fixnum from 0 below 4
		   do (setf (aref mt i j) (aref m j i))))
    mt))

(defun invert-orthonormal-homogeneous-4x4-matrix  (mat &optional inverse)
  (declare (type (or null 4x4-matrix) mat inverse))
  (unless inverse (setq inverse (make-4x4-matrix)))
  (transpose-4x4-matrix mat inverse)
  (loop for i fixnum from 0 to 2
	do (setf (aref inverse 3 i)
		 0.0
		 (aref inverse i 3)
		 (- (the double-float (loop for j fixnum from 0 below 3
					    sum (* (aref inverse i j) (aref mat j 3))
					    double-float)))))
  inverse)

(defvar *multiply-4x4-matrices-tmp-mat* (make-array 16 :element-type 'double-float))

(defun multiply-4x4-matrices (mat-a mat-b &optional mat-c)
 ;; #+cmu (declare (ext:optimize-interface (speed 3) (safety 0) (debug 1)(ext:inhibit-warnings 1)))
  (declare (optimize (speed 3) (safety 0) (debug 1) #+cmu (ext:inhibit-warnings 1)))
  (declare (type 4x4-matrix  mat-a mat-b)
	   (type (or null 4x4-matrix) mat-c))
  (unless mat-c (setq mat-c (make-array '(4 4) :element-type 'double-float)))
  (let ((a (lcl::array-simple-vector mat-a))
	(b (lcl::array-simple-vector mat-b))
	(c (lcl::array-simple-vector mat-c)))
    (declare (type (simple-array double-float *) a b c))
    (when (eq b c)
      (setq b *multiply-4x4-matrices-tmp-mat*)
      (loop for i fixnum from 0 below 16 do (setf (aref b i) (aref c i))))
    (when (eq a c)
      (setq a *multiply-4x4-matrices-tmp-mat*)
      (loop for i fixnum from 0 below 16 do (setf (aref a i) (aref c i))))

    (loop for 4i fixnum from 0 by 4 below 16
	  do (loop for j fixnum from 0 below 4
		   for 4i+j fixnum from 4i
		   do (setf (aref c 4i+j)
			    (loop for k fixnum from 0 below 4 
				  for 4i+k fixnum from 4i
				  for 4k+j fixnum from j by 4
				  sum (* (aref a 4i+k) (aref b 4k+j)) double-float)))))
  mat-c)

;;; *************************  4X4-ROTATION-MATRICES  *************************


;;; FIXME: :AZIMUTH is wrong.  It is treated here the same as :Z-ROT, a counter-clockwise rotation
;;;         about the z-axis relative to the x-axis.  :AZIMUTH should be a clockwise rotation
;;;         from the y-axis.  Unfortunately, old CME site models use :AZIMUTH-DEG.
;;;         Sat Oct 23 2004 On cowell I did cd $RADIUS/sites;find . -type f | xargs grep -i azimuth
;;;         The only meaningful hits were in alv/models/baseline
(defun make-4x4-rotation-matrix (axis-spec angle &optional into-matrix)
  (setq into-matrix (make-4x4-identity-matrix into-matrix))
  (cond ((symbolp axis-spec)
	 (let* ((into-matrix into-matrix)
		(axis (case axis-spec
			((:x-deg :x-rad :x :x-rot :x-axis :omega :omega-degrees :omega-radians) 0)
			((:y-deg :y-rad :y :y-rot :y-axis :phi :phi-degrees :phi-radians) 1)
			((:z-deg :z-rad :z :z-rot :z-axis :kappa :kappa-degrees :kappa-radians
				 :azimuth :azimuth-deg :azimuth-degrees) 2)
			(otherwise (error "illegal axis spec = ~a" axis-spec))))
		(radians (if (memq axis-spec '(:x-deg :y-deg :z-deg :azimuth-deg :azimuth-degrees
					       :omega-degrees :phi-degrees :kappa-degrees))
			     (* angle (/ pi 180.0))
			     angle))
		(i0 (mod (+ axis 1) 3))
		(i1 (mod (+ axis 2) 3))
		(cos (cos radians))
		(sin (sin radians)))
	   (declare (fixnum i0 i1))
	   (declare (type 4x4-matrix into-matrix))
	   (declare (type double-float radians cos sin))
	   (setf (aref into-matrix i0 i0) cos)
	   (setf (aref into-matrix i1 i1) cos)
	   (setf (aref into-matrix i0 i1) sin)
	   (setf (aref into-matrix i1 i0) (- sin))
	   into-matrix))
	((or (listp axis-spec) (vectorp axis-spec))
	 ;; FIXME:  Should angle be in radians or degrees?
	 (make-angle-axis-rotation-matrix angle axis-spec into-matrix))
	(t (error "Illegal axis-spec ~a" axis-spec)))
  into-matrix)

(defun make-4x4-translation-matrix (position &optional into-matrix)
  (declare (type coordinate-vector position))
  (setq into-matrix (make-4x4-identity-matrix into-matrix))
  (setf (aref into-matrix 0 3) (aref position 0))
  (setf (aref into-matrix 1 3) (aref position 1))
  (setf (aref into-matrix 2 3) (aref position 2))
  into-matrix)



;;; This returns a matrix transforming from parent to object coordinates.
;;; The matrix multiplications are formed right-to-left, ie. M3 x M2 x M1
;;; for axis1 angle1 axis2 angle2 axis3 angle3.
(defun make-orientation-matrix (&rest axis-spec-angle-pairs)
  (declare (dynamic-extent axis-spec-angle-pairs))
  (if (null axis-spec-angle-pairs)
      (make-4x4-identity-matrix) 
      (let ((tmp-matrix (make-4x4-identity-matrix)))
	(loop with into-matrix
	      with orientation-matrix
	      for (axis radians) on axis-spec-angle-pairs by #'cddr
	      unless (memq axis '(:y-flip :scale))
		do (cond (orientation-matrix
			  (make-4x4-rotation-matrix axis radians tmp-matrix)
			  (multiply-matrices tmp-matrix orientation-matrix orientation-matrix))
			 (t (setq orientation-matrix
				  (make-4x4-rotation-matrix axis radians into-matrix))))
	      finally
	   (unless orientation-matrix
	     (setq orientation-matrix (or into-matrix (make-4x4-identity-matrix))))
	   
	   (return orientation-matrix)))))

(defparameter *euler-angle-system-keywords* 
  '(:euler-angles :omega-phi-kappa :azimuth-elevation-twist :heading-pitch-roll :z-y-x-rotation
    :angle-axis))

;;; ARGS specifies either:
;;;      1 element: a list or vector specifying axis and angle.
;;;      1 element: a 4x4 matrix specifying an orientation matrix
;;;      Euler-angle-system-keyword Euler-angle-vector-or-list
;;;      axis-angle-pairs
;;; The resulting matrix transforms from object to parent
;;;(defun make-object-to-parent-orientation-matrix (&rest args)
;;;  (cond ((= (length args) 1)
;;;         (let ((arg (car args)))
;;;           (cond ((and (arrayp arg) (= (array-rank arg) 2))
;;;                  arg)                  ; an orientation matrix
;;;                 ((or (consp arg) (arrayp arg))
;;;                  ;; This must be an axis-angle-orientation-spec (angle . axis)
;;;                  (make-angle-axis-rotation-matrix (radians (car arg)) (cdr arg)))
;;;                 (t (error "Illegal orientation specifications: ~a" args)))))
;;;        ((member (car args) *euler-angle-system-keywords*)
;;;         (apply #'make-object-to-parent-rotation-matrix (car args) (cadr args)))
;;;        (t (transpose-4x4-matrix (apply #'make-orientation-matrix 

(defun make-object-to-parent-orientation-matrix (&rest args)
  (if (null args)
      (make-4x4-identity-matrix)
      (apply 'make-object-to-parent-rotation-matrix args)))


(defun make-object-to-parent-matrix (origin &rest axes-and-angles)
  (let* ((object-to-parent-matrix 
	  (if axes-and-angles
	      (apply #'MAKE-OBJECT-TO-PARENT-ROTATION-MATRIX axes-and-angles)
	      (make-4x4-identity-matrix)))
	 (origin (or origin '(0.0 0.0 0.0)))
	 (x (elt origin 0))
	 (y (elt origin 1))
	 (z (elt origin 2)))
    (declare (type 4x4-matrix object-to-parent-matrix))
    (setf (aref object-to-parent-matrix 0 3) x
	  (aref object-to-parent-matrix 1 3) y
	  (aref object-to-parent-matrix 2 3) z)
    object-to-parent-matrix))


(defgeneric MAKE-OBJECT-TO-PARENT-ROTATION-MATRIX (axis-spec &rest args))
#|
MAKE-OBJECT-TO-PARENT-ROTATION-MATRIX has 3 major calling sequences:

(MAKE-OBJECT-TO-PARENT-ROTATION-MATRIX Euler-angle-system Euler-angles)

     MAKE-OBJECT-TO-PARENT-ROTATION-MATRIX constructs a 4x4 rotation matrix from a set of 3 Euler angles
     successively applies a sequence of Euler angle rotations of an object about its 
     coordinate axes which are initially aligned with the parent coordinate axes.
     For example, when EULER-ANGLE-SPEC = OMEGA-PHI-KAPPA, the object is first rotated
     about its x-axis, then its current y-axis, then its current z-axis.

     The first argument EULER-ANGLE-SPEC must be a symbol that specifies the group of
     Euler angles.

     All angles are specified in DEGREES.

     MAKE-OBJECT-TO-PARENT-ROTATION-MATRIX returns an OBJECT-TO-3D-PARENT rotation matrix, that is,
         
     parent-direction-vector = (multiply-matrices matrix object-direction-vector)

(MAKE-OBJECT-TO-PARENT-ROTATION-MATRIX matrix-vector-or-list)
    The single argument is:
         . A 4x4-rotation-matrix
         . A list or vector containing 4 elements: (angle-in-radians nx ny nz) 
             where <nx ny nz> is a unit vector specifying the rotation axis.

(MAKE-OBJECT-TO-PARENT-ROTATION-MATRIX &rest axis-angle-pairs)
    axis-angle-pairs is a sequence of rotations (confusingly specified in world to object order).
    See MAKE-4X4-ROTATION-MATRIX and MAKE-ORIENTATION-MATRIX

|#

(defgeneric DECOMPOSE-OBJECT-TO-PARENT-ROTATION-MATRIX (Euler-angle-spec 4x4-rotation-matrix) 
  (declare (values ang1-deg ang2-deg ang3-deg)))
#|
     DECOMPOSE-OBJECT-TO-PARENT-ROTATION-MATRIX returns the Euler angles (in degrees) specified
     by the EULER-ANGLE-SPEC and the 4X4-ROTATION-MATRIX.

     The matrix is assumed to be the object-to-parent rotation matrix
|#

(defgeneric Euler-angle-keyvals (Euler-angle-system orientation-matrix))


#| **************************  OMEGA-PHI-KAPPA-FRAME-ROTATION-MATRIX  **************************

    Omega, Phi, and Kappa are the Euler angles about the x, y, and z axes
respectively and conforms to the equations in the "Manual of Photogrammetry",
4th edition, page 51.

    (make-object-to-parent-rotation-matrix :x-deg Omega :y-deg Phi :z-deg Kappa))

    This encoding of the Euler angles is most appropriate for downward looking
aerial imagery where the angle between the principal ray and the nadir line is
less than 90 degrees.

|#

;;; Matrix transforming from 3d-world to camera relative coordinates.
;;; Angles in radians
(defun make-omega-phi-kappa-rotation-matrix
       (omega phi kappa &optional matrix)
  (declare (type (or null dmatrix) matrix))
  (declare (type double-float omega phi kappa))
  (let ((cos-omega (cos omega))
	(cos-phi (cos phi))
	(cos-kappa (cos kappa))
	(sin-omega (sin omega))
	(sin-phi (sin phi))
	(sin-kappa (sin kappa)))
    (declare (type double-float cos-omega cos-phi cos-kappa sin-omega sin-phi sin-kappa))

    (unless matrix (setq matrix (make-4x4-identity-matrix)))

    (setf (aref matrix 0 0) (* cos-phi cos-kappa))
    (setf (aref matrix 0 1) (+ (* cos-omega sin-kappa) (* sin-omega sin-phi cos-kappa)))
    (setf (aref matrix 0 2) (- (* sin-omega sin-kappa) (* cos-omega sin-phi cos-kappa)))

    (setf (aref matrix 1 0) (- (* cos-phi sin-kappa)))
    (setf (aref matrix 1 1) (- (* cos-omega cos-kappa) (* sin-omega sin-phi sin-kappa)))
    (setf (aref matrix 1 2) (+ (* sin-omega cos-kappa) (* cos-omega sin-phi sin-kappa)))

    (setf (aref matrix 2 0) sin-phi)
    (setf (aref matrix 2 1) (- (* sin-omega cos-phi)))
    (setf (aref matrix 2 2) (* cos-omega cos-phi))
    matrix))

#|
(disassemble 'make-omega-phi-kappa-rotation-matrix)
|#


;;; m is a matrix transforming from 3d-world to camera relative coordinates.
;;; Returns angles in radians.
(defun decompose-omega-phi-kappa-rotation-matrix (m)
  (declare (optimize (speed 3)(safety 1)))
  ;; Let this work for 3x3 matrices too
  ;;(declare (type 4x4-matrix m))
  (declare (type dmatrix m))
  (if (and (< (abs (aref m 2 2)) 1e-6) (< (abs (aref m 2 1)) 1e-6))
      ;; Degenerate case:  When phi = +/- 90 degrees, then we cannot separate omega and kappa,
      ;; ie, we can only compute (+ omega kappa).  
      ;; Then (aref m 1 2) = (sin (+ omega kappa))
      ;; and  (aref m 1 1) = (cos (+ omega kappa))
      
      (let* ((omega+kappa (atan (aref m 1 2) (aref m 1 1)))
	     (phi (* (signum (aref m 2 0)) (/ pi 2))) ; either +pi/2 or -pi/2
	     (kappa 0.0)  ;(/ pi 2)  ; arbitrary
	     )	
	(values (- omega+kappa kappa) phi kappa))

      (let* ((omega (atan (- (aref m 2 1)) (aref m 2 2)))
	     (phi (atan (aref m 2 0) (inline-euclidean-length (aref m 0 0)(aref m 1 0))))
	     ;; with asin, matrix must be orthonormal
	     ;;(phi (asin (min 0.0 (max -1.0 (aref m 2 0)))))
	     (kappa (atan (- (aref m 1 0)) (aref m 0 0))))
	(values omega phi kappa))))
;(disassemble 'decompose-omega-phi-kappa-rotation-matrix)

;;; m is a matrix transforming from camera to world relative coordinates.
;;; returns angles in radians.
(defun decompose-omega-phi-kappa-rotation-matrix-transposed (m)
  (declare (optimize (speed 3)(safety 1)))
  ;;(declare (type 4x4-matrix m))
  (declare (type dmatrix m))
  (if (and (< (abs (aref m 2 2)) 1e-6) (< (abs (aref m 1 2)) 1e-6))
      ;; Degenerate case:  When phi = +/- 90 degrees, then we cannot separate omega and kappa,
      ;; ie, we can only compute (+ omega kappa).  
      ;; Then (aref m 2 1) = (sin (+ omega kappa))
      ;; and  (aref m 1 1) = (cos (+ omega kappa))
      (let* ((omega+kappa (atan (aref m 2 1) (aref m 1 1)))
	     (phi (* (signum (aref m 0 2)) (/ pi 2))) ; either +pi/2 or -pi/2
	     (kappa 0.0)		;(/ pi 2)    		; arbitrary
	     )
	(values (- omega+kappa kappa) phi kappa))

      (let* ((omega (atan (- (aref m 1 2)) (aref m 2 2)))
	     (phi (atan (aref m 0 2) (euclidean-length (aref m 0 0)(aref m 0 1))))
	     ;; with asin, matrix must be orthonormal
	     ;;(phi (asin (min 1.0 (max -1.0 (aref m 0 2)))))
	     (kappa (atan (- (aref m 0 1)) (aref m 0 0))))
	(values omega phi kappa))))

 
#| ****************************  AZIMUTH-TILT-SWING-ROTATION-MATRIX  ****************************

    Azimuth, Tilt, and Swing are the Euler angles about the z, x, and z axes
respectively and conforms to the equations in the "Manual of Photogrammetry",
4th edition, page 53.

    (make-object-to-parent-rotation-matrix :z-deg (- Azimuth)
                                           :x-deg Tilt
			                   :z-deg (+ 180 Swing))

    This encoding of the Euler angles is most appropriate for non-nadir imagery
where the angle between the principal ray and the Nadir line is greater than 0
degrees.  

    CRITIQUE  The swing angle seems to be weirdly oriented in the wrong direction.

|#

;;;  angles in radians
(defun make-azimuth-tilt-swing-rotation-matrix (azimuth tilt swing)
  (declare (double-float azimuth tilt swing))
  (let* ((m (make-4x4-identity-matrix))
	 (sinA (sin azimuth))
	 (cosA (cos azimuth))
	 (sinT (sin tilt))
	 (cosT (cos tilt))
	 (sinS (sin swing))
	 (cosS (cos swing)))
    (declare (double-float sinA cosA sinT cosT sinS cosS))
    (declare (type 4x4-matrix m))

    (setf (aref m 0 0) (- (* (- cosS) cosA) (* sinS cosT sinA))
	  (aref m 0 1) (- (* sinS cosA)     (* cosS cosT sinA))
	  (aref m 0 2) (- (* sinA sinT))

	  (aref m 1 0) (- (* cosS sinA)     (* sinS cosT cosA))
	  (aref m 1 1) (- (* (- sinS) sinA) (* cosS cosT cosA))
	  (aref m 1 2) (- (* cosA sinT))

	  (aref m 2 0) (- (* sinS sinT))
	  (aref m 2 1) (- (* cosS sinT))
	  (aref m 2 2) cosT)
    m))


;;; returns angles in radians
;;;(defun decompose-azimuth-tilt-swing-rotation-matrix (m)
;;;  (declare (type 4x4-matrix m))
;;;  (if (and (< (abs (aref m 0 2)) 1e-6) (< (abs (aref m 1 2)) 1e-6))
;;;      ;; Degenerate case:  When tilt = 0 degrees or 180 degrees, we cannot separate azimuth and swing
;;;      ;; ie, we can only compute (- azimuth swing).  Here we will let swing = 0 degrees.
;;;      ;; Then (aref m 1 0) = (sin (- azimuth swing))
;;;      ;; and  (aref m 0 0) = (- (cos (- azimuth swing)))
;;;      (let* ((azimuth (atan (aref m 1 0) (- (aref m 0 0))))
;;;             (tilt (if (>= (aref m 2 2) 0.0) 0.0 pi))
;;;             (swing 0.0))
;;;        (values azimuth tilt swing))
;;;
;;;      (let* ((azimuth (atan (- (aref m 0 2)) (- (aref m 1 2))))
;;;             ;;(tilt (acos (aref m 2 2))) ; m must be orthonormal
;;;             ;; this next version allows m to be scaled.
;;;             (tilt (atan (euclidean-length (aref m 0 2) (aref m 1 2)) (aref m 2 2)))
;;;             (swing (atan (- (aref m 2 0)) (- (aref m 2 1)))))
;;;        (values azimuth tilt swing))) )

;;; returns angles in radians
(defun decompose-azimuth-tilt-swing-rotation-matrix (m)
  (declare (type 4x4-matrix m))
  (if (and (< (abs (aref m 0 2)) 1e-6) (< (abs (aref m 1 2)) 1e-6))
      ;; Degenerate case:  When tilt = 0 degrees or 180 degrees, we cannot separate azimuth and swing
      ;; ie, we can only compute (- azimuth swing).  Here we will let swing = 180 degrees.
      ;; Then (aref m 1 0) = (sin (- azimuth swing))
      ;; and  (aref m 0 0) = (- (cos (- azimuth swing)))
      (let* ((azimuth-swing (atan (aref m 1 0) (- (aref m 0 0))))
	     (tilt (if (>= (aref m 2 2) 0.0) 0.0 pi))
	     (swing pi))
	(values (+ azimuth-swing swing) tilt swing))

      (let* ((azimuth (atan (- (aref m 0 2)) (- (aref m 1 2))))
	     ;;(tilt (acos (aref m 2 2))) ; m must be orthonormal
	     ;; this next version allows m to be scaled.
	     (tilt (atan (euclidean-length (aref m 0 2) (aref m 1 2)) (aref m 2 2)))
	     (swing (atan (- (aref m 2 0)) (- (aref m 2 1)))))
	(values azimuth tilt swing))) )

#|
(make-azimuth-tilt-swing-rotation-matrix (radians 0.0) (radians 45.0) (radians 0.0))

#2A((-1.0 0.0 -0.0 0.0)
    (0.0 -0.7071067811865476 -0.7071067811865475 0.0)
    (-0.0 -0.7071067811865475 0.7071067811865476 0.0)
    (0.0 0.0 0.0 1.0))

(make-azimuth-tilt-swing-rotation-matrix (radians 90.0) (radians 45.0) (radians 0.0))

#2A((0.0 -0.7071067811865476 -0.7071067811865475 0.0)
    (1.0 0.0 0.0 0.0)
    (-0.0 -0.7071067811865475 0.7071067811865476 0.0)
    (0.0 0.0 0.0 1.0))

(defun angerr (ang) 
  (let ((n (round ang (* 2.0 pi))))
    (- ang (* n (* 2.0 pi)))))

(defun test-azimuth-tilt-swing (n &optional degenerate-tilt)
  (declare (type (or null double-float) degenerate-tilt))
  (loop repeat n
	for azimuth = (random-in-range (- pi) pi)
	for tilt = (or degenerate-tilt (random-in-range 0 pi))
	for swing = (random-in-range (- pi) pi)
	for m = (make-azimuth-tilt-swing-rotation-matrix azimuth tilt swing)
	with (azimuth2 tilt2 swing2 err)
	do (multiple-value-setq (azimuth2 tilt2 swing2)
	     (decompose-azimuth-tilt-swing-rotation-matrix m))
	   (setq err (if degenerate-tilt
			 (if (zerop degenerate-tilt)
			     (euclidean-length (angerr (- (- azimuth swing) (- azimuth2 swing2))) (- tilt tilt2))
			     (euclidean-length (angerr (- (+ swing azimuth) (+ azimuth2 swing2))) (- tilt tilt2)))
			 (euclidean-length (- azimuth azimuth2) (- tilt tilt2) (- swing swing2))))
	collect (list err (list azimuth azimuth2) (list tilt tilt2) (list swing swing2) m)
	into stuff
	maximize err into maxerr
	finally (return (values maxerr stuff))))

(defun matrix-max-diff (a b)
  (loop for j from 0 below (array-dimension a 1)
	maximize (loop for i from 0 below (array-dimension a 0)
		       maximize (abs (- (aref a i j) (aref b i j))))))

(test-azimuth-tilt-swing 1000)  ; looks good

(test-azimuth-tilt-swing 1000 0.0)
(test-azimuth-tilt-swing 1000 pi)

(defun compare-ats-matrices (azimuth tilt swing)
  (let ((m1 (make-azimuth-tilt-swing-rotation-matrix (radians azimuth) (radians tilt) (radians swing)))
      (m2 (transpose-matrix (make-orientation-matrix :z-deg (- azimuth) :x-deg tilt :z-deg (+ 180 swing)))))
  (list (matrix-max-diff m1 m2) m1 m2)))


(compare-ats-matrices 90.0 45.0 10.0)

(compare-ats-matrices 0.0 0.0 180.0)
(compare-ats-matrices 90.0 0.0 90.0)
(compare-ats-matrices 0.0 90.0 0.0)

(compare-ats-matrices 30.0 45.0 0.0)

|#


#| ***********************  AZIMUTH-ELEVATION-TWIST-ROTATION-MATRIX  ***********************

    Azimuth, Elevation, and Twist are the Euler angles about the z, x, and z
axes respectively and conforms to the VIVID camera model metadata
parameterization.  These rotation axes are equivalent to Pan, Tilt, and Roll on
a conventional camera tripod, with the camera u axis points East, v points up, and w
points South.
 
   (make-object-to-parent-rotation-matrix :x-deg 90.0         ; rotate camera to North looking
                                          :y-deg (- Azimuth)
		                          :x-deg Elevation
			                  :z-deg Twist)))

or, equivalently, 

   (make-object-to-parent-rotation-matrix :z-deg (- Azimuth)
		                          :x-deg (+ 90.0 Elevation)
                                          :z-deg Twist)

    This encoding of the Euler angles is most appropriate for non-nadir imagery
where the angle between the principal ray and the Nadir line is greater than 0
degrees.

|#

(defun map-degrees-to-plus-minus-range (degrees)
  (mv-bind (n fract) (round degrees 360.0)
    fract))

(defun map-radians-to-plus-minus-range (degrees)
  (mv-bind (n fract) (round degrees (* 2.0 pi))
    fract))

(defun map-radians-to-positive (degrees)
  (mv-bind (n fract) (floor degrees (* 2.0 pi))
    fract))


;;;  angles in radians
(defun make-azimuth-elevation-twist-rotation-matrix (azimuth elevation twist)
  (make-azimuth-tilt-swing-rotation-matrix azimuth (+ (* .5 pi) elevation) (+ pi twist)))

;;; returns angles in radians
(defun decompose-azimuth-elevation-twist-rotation-matrix (m)
  (mv-bind (azimuth tilt swing) (decompose-azimuth-tilt-swing-rotation-matrix m)
    (values azimuth (- tilt (* .5 pi)) (map-radians-to-positive (- swing pi)))))


#|
(make-azimuth-elevation-twist-rotation-matrix (radians 0.0) (radians 0.0) (radians 0.0))
#2A((1.0 0.0 -0.0 0.0)
    (0.0 0.0 -1.0 0.0)
    (0.0 1.0 0.0 0.0)
    (0.0 0.0 0.0 1.0))

(let ((aet '(172.0044808842515 4.8872636044886075 1.5790720429665726))
      (principal-point (cv 191.0 113.0 -500.0))
      (camera-position (cv -57.242573 4954.4883 6054.9927)))

  (destructuring-bind (azimuth elevation twist) aet
  (bind-vector-elements (x y z) camera-position
       (bind-vector-elements (ppu ppv fl) principal-point
	 (let* ((camera-to-3d-matrix (transpose-matrix 
		 (make-azimuth-elevation-twist-rotation-matrix 
		  (radians azimuth) (radians elevation) (radians twist)))))
	   (setf (aref camera-to-3d-matrix 0 3) x
		 (aref camera-to-3d-matrix 1 3) y
		 (aref camera-to-3d-matrix 2 3) z)
	   (setq *camera-to-3d-matrix* camera-to-3d-matrix)
	   (setq *proj*
	   (transforms::make-perspective-transform :camera-to-3d-matrix camera-to-3d-matrix
						   :focal-length fl
						   :principal-point-u ppu
						   :principal-point-v ppv)))))))

(transforms::projection-matrix *proj*)
(transforms::decompose-projection-matrix (transforms::projection-matrix *proj*))
(cme::camera-position *proj*)

(setq m (make-azimuth-elevation-twist-rotation-matrix (radians 10) (radians 20) (radians 5)))
(loop for ang in (mv-list(decompose-azimuth-elevation-twist-rotation-matrix m))
      collect (map-degrees-to-plus-minus-range (degrees ang)))

(map-degrees-to-plus-minus-range -355.0)


(setq alv-2-44-projection (3d-to-2d-projection (get-2d-world-named "alv-2-44")))

(let* ((projection (gui::3d-to-2d-projection (gui::top-view))))
  (mv-bind (mat params)
      (transforms::decompose-projection-matrix-old (transforms::projection-matrix projection))
    (let ((imat (math::invert-matrix mat)))
      (list params
	    (cme::camera-position projection)
	    (loop for ang in (mv-list (math::decompose-omega-phi-kappa-rotation-matrix imat))
		  collect (map-degrees-to-plus-minus-range (degrees ang)))
	    (loop for ang in (mv-list (math::decompose-azimuth-elevation-twist-rotation-matrix imat))
		  collect (map-degrees-to-plus-minus-range (degrees ang)))
	    ))))

;;; alv-oblique-tower
((:|1/F| -0.001999999911379647 :R/F 0.0 :PRINCIPAL-POINT-U 190.99998700762262
  :PRINCIPAL-POINT-V 113.00000295803387 :V-AXIS-ASPECT-RATIO 1.0000000168753316
  :V-AXIS-SKEW 3.446251901772461e-8)
 #(-57.242572999999595 4954.4883 6054.992699999998)
 (-94.88911126436454 1.5733295000421974 172.13904444776722)
 (172.0044808842515 4.8872636044886075 1.5790720429665726))

;;; alv-oblique-quonset
((:|1/F| -0.0019999999351577456 :R/F 0.0 :PRINCIPAL-POINT-U 169.99999379347219
  :PRINCIPAL-POINT-V 111.99999918649429 :V-AXIS-ASPECT-RATIO 0.9999999754127175
  :V-AXIS-SKEW 3.714932615354514e-7)
 #(-62.56800999999989 4953.8364 6054.611)
 (-95.77968198612665 0.8007683639575636 -152.30451600288345)
 (-152.38556550938753 5.779115601058179 0.8048593464112059))



|#


#| *******************   OMEGA-ALPHA-KAPPA-ROTATION-MATRIX  *******************

(make-object-to-parent-rotation-matrix :x-deg (+ 90 omega) :y-deg (- alpha) :z-deg (+ kappa))

This is a stupid set of Euler angles.

|#

;;;  angles in radians
(defun make-omega-alpha-kappa-rotation-matrix (omega alpha kappa)
  (declare (double-float omega alpha kappa))
  (let* ((m (make-4x4-identity-matrix))
	 (sinO (sin omega))
	 (cosO (cos omega))
	 (sinA (sin alpha))
	 (cosA (cos alpha))
	 (sinK (sin kappa))
	 (cosK (cos kappa)))
    (declare (double-float sinO cosO sinA cosA sinK cosK))
    (declare (type 4x4-matrix m))

    (setf (aref m 0 0) (* cosA cosK)
	  (aref m 0 1) (- (- (* sinO sinK)) (* cosO sinA cosK))
	  (aref m 0 2) (- (* cosO sinK) (* sinO sinA cosK))

	  (aref m 1 0) (- (* cosA sinK))
	  (aref m 1 1) (- (* cosO sinA sinK) (* sinO cosK))
	  (aref m 1 2) (+ (* cosO cosK) (* sinO sinA sinK))

	  (aref m 2 0) (- sinA)
	  (aref m 2 1) (- (* cosO cosA))
	  (aref m 2 2) (- (* sinO cosA)))
    m))

#|

(make-object-to-parent-rotation-matrix :x-deg (+ 90 omega) :y-deg (- alpha) :z-deg (+ kappa))

This is a stupid set of Euler angles.

|#

;;;  angles in radians
(defun make-omega-alpha-kappa-rotation-matrix (omega alpha kappa)
  (declare (double-float omega alpha kappa))
  (let* ((m (make-4x4-identity-matrix))
	 (sinO (sin omega))
	 (cosO (cos omega))
	 (sinA (sin alpha))
	 (cosA (cos alpha))
	 (sinK (sin kappa))
	 (cosK (cos kappa)))
    (declare (double-float sinO cosO sinA cosA sinK cosK))
    (declare (type 4x4-matrix m))

    (setf (aref m 0 0) (* cosA cosK)
	  (aref m 0 1) (- (- (* sinO sinK)) (* cosO sinA cosK))
	  (aref m 0 2) (- (* cosO sinK) (* sinO sinA cosK))

	  (aref m 1 0) (- (* cosA sinK))
	  (aref m 1 1) (- (* cosO sinA sinK) (* sinO cosK))
	  (aref m 1 2) (+ (* cosO cosK) (* sinO sinA sinK))

	  (aref m 2 0) (- sinA)
	  (aref m 2 1) (- (* cosO cosA))
	  (aref m 2 2) (- (* sinO cosA)))
    m))

#|


(defun compare-oak-matrices (omega alpha kappa)
  (let ((m1 (make-alpha-omega-kappa-rotation-matrix (radians omega) (radians alpha) (radians kappa)))
	(m2 (make-orientation-matrix :x-deg (+ 90 omega) :y-deg (- alpha) :z-deg (+ kappa))))
    (values (matrix-max-diff m1 m2) m1 m2)))


(list
 (compare-oak-matrices 0.0 0.0 0.0)

 (compare-oak-matrices 90.0 0.0 0.0)
 
 (compare-oak-matrices 0.0 45.0 0.0)

 (compare-oak-matrices 0.0 0.0 45.0)

 (compare-oak-matrices 20.0 30.0 40.0)
)
|#


;;; *******************   ANGLE-AXIS-ROTATION-MATRIX  *******************

;;; THETA in radians.
(defun make-angle-axis-rotation-matrix (theta axis-vector &optional m)
  (declare (type double-float theta))
  ;(declare (type (or list coordinate-vector) axis-vector))
  (declare (optimize (speed 3) (safety 0)))
  (let* ((m (or m (make-4x4-identity-matrix)))
	 (x (elt axis-vector 0))
	 (y (elt axis-vector 1))
	 (z (elt axis-vector 2))
	 (cos (cos theta))
	 (sin (sin theta))
	 (1-cos (- 1.0 cos))
	 (L^2 (+ (* x x) (* y y) (* z z)))
	 (1/L (if (< L^2 1e-100) 0.0 (/ 1.0 (sqrt l^2))))
	 )
    (declare (double-float x y z))
    (declare (type 4x4-matrix m))
    (declare (double-float cos sin 1-cos l^2 1/L))
    (if (zerop 1/L) 
	(setq x 1.0 y 0.0 z 0.0)
	;; normalize the axis vector elements
	(setq x (* x 1/L) y (* y 1/L) z (* z 1/L)))
    (setf (aref m 0 0) (+ cos (* x x 1-cos))      
	  (aref m 0 1) (- (* x y 1-cos) (* z sin))
	  (aref m 0 2) (+ (* x z 1-cos) (* y sin))
	     
	  (aref m 1 0) (+ (* y x 1-cos) (* z sin))
	  (aref m 1 1) (+ cos (* y y 1-cos))
	  (aref m 1 2) (- (* y z 1-cos) (* x sin))
	     
	  (aref m 2 0) (- (* z x 1-cos) (* y sin))
	  (aref m 2 1) (+ (* z y 1-cos) (* x sin))
	  (aref m 2 2) (+ cos (* z z 1-cos)))
    m))

(defparameter *decompose-angle-axis-rotation-matrix-angle-threshold* 1e-6) ; arbitrary threshold 

;;; The upper 3x3 submatrix of M should be orthonormal.  
;;; The result returned is a valid angle-axis even if M is not orthonormal..
;;; Returns axis of rotation x y z, and angle of rotation theta in radians.
(defun decompose-angle-axis-rotation-matrix (m)
  (declare (type 4x4-matrix m))
  (let* ((2cos+1 (+ (aref m 0 0) (aref m 1 1) (aref m 2 2)))
	 (2cos (- 2cos+1 1.0))
	 ;(cos (* .5 2cos))
	 (2sx (- (aref m 2 1) (aref m 1 2)))
	 (2sy (- (aref m 0 2) (aref m 2 0)))
	 (2sz (- (aref m 1 0) (aref m 0 1)))
	 (2sin (euclidean-length 2sx 2sy 2sz)))
    (declare (double-float 2cos+1 2cos 2sx 2sy 2sz 2sin))
    (if (< (abs 2sin) *decompose-angle-axis-rotation-matrix-angle-threshold*)
	(values 0.0 1.0 0.0 0.0) ; no rotation at all, cannot determine the axis.
	(values (atan 2sin 2cos) ;(acos cos)
		(/ 2sx 2sin) (/ 2sy 2sin) (/ 2sz 2sin)))))


(defun orthogonalize-rotation-matrix (matrix)
  (mv-bind (angle nx ny nz) (decompose-angle-axis-rotation-matrix matrix)
    (make-angle-axis-rotation-matrix angle (cv nx ny nz))))

;;; **************************  KEYFRAME-INTERPOLATOR  **************************

(defstruct-class keyframe-interpolator (lx::base-struct-class)
    ((m0 :initarg :m0 )
     (m1 :initarg :m1)
     (t0 :initarg :t0)
     (t1 :initarg :t1)
     angle
     axis))

(defmethod initialize-instance :after ((obj keyframe-interpolator) &rest initargs)
  (declare (ignore initargs))
  (with-class-slots keyframe-interpolator (m0 m1 angle axis) obj
    (mv-bind (theta nx ny nz)
	(decompose-angle-axis-rotation-matrix (multiply-matrices m1 (invert-matrix m0)))
      (setf angle theta axis (cv nx ny nz)))))

(defmethod interpolate-transform-matrix ((obj keyframe-interpolator) time)
  (with-class-slot-values keyframe-interpolator (m0 m1 t0 t1 angle axis) obj
    (let* ((dt (/ (- time t0) (- t1 t0)))
	   (1-dt (- 1.0 dt))
	   (mat (make-angle-axis-rotation-matrix (* dt angle) axis))) ; interpolate the orientation
      (declare (double-float dt 1-dt))
      (declare (type 4x4-matrix mat))
      (multiply-matrices mat m0 mat)
      ;; interpolate the position
      (setf (aref mat 0 3) (+ (* dt (aref m1 0 3)) (* 1-dt (aref m0 0 3)))
	    (aref mat 1 3) (+ (* dt (aref m1 1 3)) (* 1-dt (aref m0 1 3)))
	    (aref mat 2 3) (+ (* dt (aref m1 2 3)) (* 1-dt (aref m0 2 3))))
      mat)))


#|
(setq kfi (make-instance 
	   'keyframe-interpolator
	   :m0 (make-object-to-parent-matrix (cv 100.0 0.0 0.0) :z-y-x-rotation '(30.0 20.0 10.0))
	   :m1 (make-object-to-parent-matrix (cv 200.0 0.0 0.0) :z-y-x-rotation '(100.0 0.0 0.0))
	   :t0 0.0
	   :t1 1.0))

(list (keyframe-interpolator-m1 kfi) (interpolate-transform-matrix kfi 1.0))
(list (keyframe-interpolator-m0 kfi) (interpolate-transform-matrix kfi 0.0))

(interpolate-transform-matrix kfi 0.5)

(decompose-angle-axis-rotation-matrix (make-object-to-parent-matrix (cv .0 0.0 0.0) :z-y-x-rotation '(270.0 0.0 0.0)))
(decompose-angle-axis-rotation-matrix (make-object-to-parent-matrix (cv .0 0.0 0.0) :z-y-x-rotation '(90.0 0.0 0.0)))
|#


	      

;;; **********************  METHODS  **********************

(defmacro values-to-degrees (form) 
  `(mv-bind (rads1 rads2 rads3) ,form
    (values (degrees rads1) (degrees rads2) (degrees rads3))))

(defmethod make-object-to-parent-rotation-matrix ((aeg (eql nil)) &rest Euler-angles)
  (declare (ignore Euler-angles))
  (make-4x4-identity-matrix))

(defmethod make-object-to-parent-rotation-matrix ((aeg t) &rest args)
  (if (null args) ; is aeg a matrix or an angle-axis spec?
      (let ((arg aeg))
	(cond ((and (arrayp arg) (= (array-rank arg) 2))
	       arg)			; an orientation matrix
	      ((or (consp arg) (arrayp arg))
	       ;; This must be an axis-angle-orientation-spec (angle . axis)
	       (make-angle-axis-rotation-matrix (car arg) (cdr arg)))
	      (t (error "Illegal orientation specifications: ~a" args))))
      ;; Otherwise, aeg must be the name of the first Euler angle.
      ;; Transpose the matrix because of the confusing convention that the axis-angle list
      ;; in in the parent to object direction.
      (transpose-4x4-matrix (apply #'make-orientation-matrix aeg args))))
 
;(make-object-to-parent-rotation-matrix :x-deg 1.0)
;(make-object-to-parent-rotation-matrix )
;(make-object-to-parent-rotation-matrix :omega-phi-kappa '(1.0 2.0 3.0))

;;; :omega-phi-kappa in degrees
(defmethod make-object-to-parent-rotation-matrix ((aeg (eql :omega-phi-kappa)) &rest args)
  (let* ((Euler-angles (first args))
	 (omega (elt Euler-angles 0))
	 (phi (elt Euler-angles 1))
	 (kappa (elt Euler-angles 2)))
    (make-object-to-parent-rotation-matrix :x-deg omega :y-deg phi :z-deg kappa)))

(defmethod decompose-object-to-parent-rotation-matrix ((aeg (eql :omega-phi-kappa)) 4x4-rotation-matrix)
  (values-to-degrees (decompose-omega-phi-kappa-rotation-matrix-transposed 4x4-rotation-matrix)))

(defmethod make-object-to-parent-rotation-matrix ((aeg (eql :azimuth-tilt-swing)) &rest args)
  (let* ((Euler-angles (first args))
	 (azimuth (elt Euler-angles 0))
	 (tilt (elt Euler-angles 1))
	 (swing (elt Euler-angles 2)))						  
    (make-azimuth-tilt-swing-rotation-matrix (radians azimuth) (radians tilt) (radians swing))))

(defmethod decompose-object-to-parent-rotation-matrix ((aeg (eql :azimuth-tilt-swing)) 4x4-rotation-matrix)
  (values-to-degrees (decompose-azimuth-tilt-swing-rotation-matrix 4x4-rotation-matrix)))

(defmethod make-object-to-parent-rotation-matrix ((aeg (eql :azimuth-elevation-twist)) &rest args)
  (let* ((Euler-angles (first args))
	 (azimuth (elt Euler-angles 0))
	 (elevation (elt Euler-angles 1))
	 (twist (elt Euler-angles 2)))	
    (make-azimuth-elevation-twist-rotation-matrix (radians azimuth) (radians elevation) (radians twist))))

(defmethod decompose-object-to-parent-rotation-matrix ((aeg (eql :azimuth-elevation-twist)) 4x4-rotation-matrix)
  (values-to-degrees (decompose-azimuth-elevation-twist-rotation-matrix 4x4-rotation-matrix)))

(defmethod make-object-to-parent-rotation-matrix ((aeg (eql :heading-pitch-roll)) &rest args)
  (let* ((Euler-angles (first args))
	 (heading (elt Euler-angles 0))
	 (pitch (elt Euler-angles 1))
	 (roll (elt Euler-angles 2)))
    (make-azimuth-elevation-twist-rotation-matrix (radians heading) (radians pitch) (radians roll))))

(defmethod decompose-object-to-parent-rotation-matrix ((aeg (eql :heading-pitch-roll)) 4x4-rotation-matrix)
  (values-to-degrees  (decompose-azimuth-elevation-twist-rotation-matrix 4x4-rotation-matrix)))

(defmethod make-object-to-parent-rotation-matrix ((aeg (eql :z-y-x-rotation)) &rest args)
  (let* ((Euler-angles (first args))
	 (rot-z (elt Euler-angles 0))
	 (rot-y (elt Euler-angles 1))
	 (rot-x (elt Euler-angles 2)))
  (make-object-to-parent-rotation-matrix :z-deg rot-z :y-deg rot-y :x-deg rot-x)))

;;; returns angles in radians.
(defun decompose-zyx-rotation-matrix (m)
  (mv-bind (omega phi kappa) (decompose-omega-phi-kappa-rotation-matrix m)
    (values (- kappa) (- phi) (- omega))))

(defmethod decompose-object-to-parent-rotation-matrix ((aeg (eql :z-y-x-rotation)) 4x4-rotation-matrix)
  (values-to-degrees (decompose-zyx-rotation-matrix 4x4-rotation-matrix)))

(defmethod make-object-to-parent-rotation-matrix ((aeg (eql :angle-axis)) &rest args)
  (let* ((Euler-angles (first args))
	 (angle (car Euler-angles))
	 (axis (cdr Euler-angles)))
    (make-angle-axis-rotation-matrix (radians angle) axis)))

(defmethod decompose-object-to-parent-rotation-matrix ((aeg (eql :angle-axis)) 4x4-rotation-matrix)
  (mv-bind (theta nx ny nz) 
      (decompose-angle-axis-rotation-matrix 4x4-rotation-matrix)
    (values (degrees theta) nx ny nz)))

(defparameter *almost-zero* 1e-8)

(defun almost-zerop (x)
  (< (abs x) *almost-zero*))


(defmethod Euler-angle-keyvals ((Euler-angle-system (eql :omega-phi-kappa)) object-to-parent-rotation-matrix)
  (mv-bind (omega phi kappa)
      (decompose-object-to-parent-rotation-matrix Euler-angle-system object-to-parent-rotation-matrix)
    (unless (and (almost-zerop omega) (almost-zerop phi) (almost-zerop kappa))
      (if (and (almost-zerop omega) (almost-zerop phi))
	  `(:z-rotation ,kappa)
	  `(,Euler-angle-system (,omega ,phi ,kappa))))))

(defmethod Euler-angle-keyvals ((Euler-angle-system (eql :z-y-x-rotation)) object-to-parent-rotation-matrix)
  (mv-bind (zrot yrot xrot)
      (decompose-object-to-parent-rotation-matrix Euler-angle-system object-to-parent-rotation-matrix)
    (unless (and (almost-zerop zrot) (almost-zerop yrot) (almost-zerop xrot))
      (if (and (almost-zerop yrot) (almost-zerop xrot))
	  `(:z-rotation ,zrot)
	  `(,Euler-angle-system (,zrot ,yrot ,xrot))))))

(defmethod Euler-angle-keyvals ((Euler-angle-system (eql :angle-axis)) object-to-parent-rotation-matrix)
  `(,Euler-angle-system 
    ,(mv-list (decompose-object-to-parent-rotation-matrix Euler-angle-system object-to-parent-rotation-matrix))))


#|
(list (make-object-to-parent-rotation-matrix :omega-phi-kappa '(10.0 20.0 30.0))
      (make-object-to-parent-rotation-matrix :x-deg 10.0 :y-deg 20.0 :z-deg 30.0))
;;; these agree

(let ((omega 10.0) (phi 20.0) (kappa 30.0))
  (list (transpose-matrix
	 (make-omega-phi-kappa-rotation-matrix (radians omega) (radians phi) (radians kappa)))
	(make-object-to-parent-rotation-matrix :omega-phi-kappa (list omega phi kappa))))
;;; these agree

(values-to-degrees  (decompose-omega-phi-kappa-rotation-matrix-transposed (make-object-to-parent-rotation-matrix :omega-phi-kappa '(10.0 20.0 30.0))))
;;; good 

(decompose-object-to-parent-rotation-matrix :omega-phi-kappa (make-object-to-parent-rotation-matrix :omega-phi-kappa '(10.0 20.0 30.0)))
;;; good

(make-object-to-parent-rotation-matrix :heading-pitch-roll '(10.0 20.0 30.0))
(decompose-object-to-parent-rotation-matrix :heading-pitch-roll (make-object-to-parent-rotation-matrix :heading-pitch-roll '(10.0 20.0 30.0)))

(matrix-times-vector (make-object-to-parent-rotation-matrix :z-y-x-rotation '(90.0 0.0 0.0)) (cv 1.0 0.0 0.0 0.0))
#(6.123031769111886e-17 1.0 0.0 0.0)

(list (make-object-to-parent-rotation-matrix :z-y-x-rotation '(90.0 20.0 10.0))
      (make-object-to-parent-rotation-matrix :z-deg 90.0 :y-deg 20.0 :x-deg 10.0))

(decompose-object-to-parent-rotation-matrix :omega-phi-kappa (make-object-to-parent-rotation-matrix :z-y-x-rotation '(90.0 0.0 0.0)))
;;; good

(decompose-object-to-parent-rotation-matrix :omega-phi-kappa (make-object-to-parent-rotation-matrix :z-y-x-rotation '(0.0 10.0 0.0)))
;;; good

(decompose-object-to-parent-rotation-matrix :omega-phi-kappa (make-object-to-parent-rotation-matrix :z-y-x-rotation '(0.0 0.0 10.0)))
;;; good

(decompose-object-to-parent-rotation-matrix :z-y-x-rotation (make-object-to-parent-rotation-matrix :z-y-x-rotation '(30.0 20.0 10.0)))
30.0 20.0 10.0

(matrix-times-vector (make-object-to-parent-rotation-matrix :z-y-x-rotation '(90.0 0.0 10.0)) (cv 1.0 0.0 0.0 0.0))
#(0.0 1.0 0.0 0.0)

(matrix-times-vector (make-object-to-parent-rotation-matrix :z-y-x-rotation '(90.0 0.0 10.0)) (cv 0.0 1.0 0.0 0.0))
#(-0.984807753012208 0.0 0.17364817766693033 0.0)

(matrix-times-vector (make-object-to-parent-rotation-matrix :z-y-x-rotation '(90.0 0.0 10.0)) (cv 0.0 0.0 1.0 0.0))
#(0.17364817766693033 0.0 0.984807753012208 0.0)

(setq *rot* (make-object-to-parent-rotation-matrix :heading-pitch-roll '(0.0 0.0 0.0)))
(matrix-times-vector *rot* (cv 0.0 0.0 -1.0 0.0))
= #(0.0 1.0 0.0 0.0) ; camera -z axis points North.

(let ((heading 10.0) (pitch 20.0) (roll 30.0))
  (list (make-object-to-parent-rotation-matrix :heading-pitch-roll (list heading pitch roll))
	(make-object-to-parent-rotation-matrix :x-deg 90.0 :y-deg (- heading) :x-deg pitch :z-deg roll)))

|#
