(in-package :transforms)

;;;
;;; Permits the use of rational polynomial sensor models.  Fitting is
;;; not implemented here.
;;;

;;; RATIONAL POLYNOMIAL TRANSFORMS

;;; Are of the form:

;;; Output is the homogeneous vector <y0 y1 ... ym> = Pj<x1 x2 ... xn>

;;; where each polynomial Pj is arbitrary.  Usually, the constant coefficient of Pm = 1.

;;; Final results are zi = yi/ym, for  i<m

;;; The least squares solution must concatanate the coeffs vectors of each polynomial


(defstruct-class rational-polynomial-coordinate-transform
    (polynomial-coordinate-transform)
  ((scale-vector :initform nil :initarg :scale-vector)
   (output-scale-vector :initform nil :initarg :output-scale-vector)
   (output-offset-vector :initform nil :initarg :output-offset-vector)
   )
  )

(define-fasd-form-init-plist rational-polynomial-coordinate-transform
    (with-class-slots rational-polynomial-coordinate-transform
	  (scale-vector output-scale-vector output-offset-vector) self
      self
      `(,@(when scale-vector `( :scale-vector ,(fasd-form scale-vector)))
	,@(when output-scale-vector `(:output-scale-vector ,(fasd-form output-scale-vector)))
	,@(when output-offset-vector `( :output-offset-vector ,(fasd-form output-offset-vector)))
	)))

(defun make-rational-polynomial-coordinate-transform
    (transform-function n-coeffs-seq coeffs-vector-vector mean-vector &rest initargs)
  (apply 'make-instance 'rational-polynomial-coordinate-transform
	 :transform-function transform-function
	 :coeffs-vector-vector coeffs-vector-vector
	 :n-coeffs-seq n-coeffs-seq
	 :mean-vector mean-vector
	 initargs))

;;; Why do we have both rat-poly-projection and rational-polynomial-coordinate-transform?
;;; Be careful that the slots are in same places  --- huh?? -- same places as what?
(defstruct-class rat-poly-projection (coordinate-transform)
  ((coeffs-vector-vector :initform nil :initarg :coeffs-vector-vector)
   (mean-vector :initform nil :initarg :mean-vector)
   (n-coeffs-seq :initform nil :initarg :n-coeffs-seq)
   (scale-vector :initform nil :initarg :scale-vector)
   (output-scale-vector :initform nil :initarg :output-scale-vector)
   (output-offset-vector :initform nil :initarg :output-offset-vector)
   (zp-projection-plane :initform nil :initarg :zp-projection-plane :accessor zp-projection-plane)
   
   ))

(defmethod linear-transform-p ((proj rat-poly-projection))
  nil)

;;;(define-fasd-form-init-plist rat-poly-projection
;;;    (with-class-slots rat-poly-projection
;;;          ( coeffs-vector-vector mean-vector scale-vector transform-function zp-projection-plane)
;;;        self
;;;      `(:transform-function ',transform-function
;;;        :coeffs-vector-vector (vector . ,(loop for i from 0 below (length  coeffs-vector-vector)
;;;                                               for cv = (aref coeffs-vector-vector i)
;;;                                               collect `(coordinate-vector . ,(list-vector cv))))
;;;        :mean-vector ,(fasd-coordinate-vector  mean-vector)
;;;        :scale-vector ,(fasd-coordinate-vector scale-vector)
;;;        :zp-projection-plane ,(fasd-coordinate-vector zp-projection-plane)
;;;        )))

(define-fasd-form-init-plist rat-poly-projection
    (with-class-slots rat-poly-projection
	  (transform-function inverse-transform coeffs-vector-vector mean-vector
			     scale-vector output-scale-vector output-offset-vector
			     zp-projection-plane)
	self
      `(:transform-function ',transform-function
	,@(when inverse-transform `( :inverse-transform ,(fasd-form inverse-transform)))
	:coeffs-vector-vector ,(fasd-coeffs-vector-vector coeffs-vector-vector)
	,@(when mean-vector `( :mean-vector ,(fasd-form mean-vector)))
	,@(when scale-vector `( :scale-vector ,(fasd-form scale-vector)))
	,@(when output-scale-vector `(:output-scale-vector ,(fasd-form output-scale-vector)))
	,@(when output-offset-vector `( :output-offset-vector ,(fasd-form output-offset-vector)))
	,@(when zp-projection-plane `( :zp-projection-plane ,(fasd-form zp-projection-plane))))))

(defmethod fasd-form ((projection rat-poly-projection))
  (with-class-slots rat-poly-projection (property-list) projection
    (let* ((*coordinate-transform-fasd-form-enable-dumping-coordinate-systems* nil)
	   (save-property-list property-list)
	   (rpf-props-to-dump '(:200ea-errors :200ea-image-name :rpc-fit-errors))
	   (fasd-prop-list (loop for key in rpf-props-to-dump
				 with flg = (list nil)
				 for val = (getf property-list key flg)
				 unless (eq val flg)
				   collect key and collect val))
	   )
      (declare (special *coordinate-transform-fasd-form-enable-dumping-coordinate-systems*))
      (setf property-list fasd-prop-list)
      (unwind-protect
	   `(make-rat-poly-projection . ,(fasd-form-init-plist projection))
	(setf property-list save-property-list)))))

(defun make-rat-poly-projection (&rest args)
  (let ((proj (apply 'make-instance 'rat-poly-projection args)))
    (let ((inverse-transform (inverse-transform proj)))
      (when inverse-transform
	(setf (inverse-transform inverse-transform) proj))
      proj)))

;;; ************  METHODS FOR RAT-POLY-PROJECTION  ************
	
;;; general 3rd order rational polynomial projection -- independent denominators
(DEFINE-RAT-POLY-PROJECT-VECTOR rat-poly-3-normalized-project-vector
    (poly-xyz-3 poly-xyz-3 poly-xyz-3 poly-xyz-3))

(DEFINE-RAT-POLY-PROJECT-VECTOR rat-poly-3-normalized-project-vector-1-output
    (poly-xyz-3 poly-xyz-3 ))

(DEFINE-RAT-POLY-PROJECT-VECTOR rat-poly-2-normalized-project-vector  
    (poly-xyz-2 poly-xyz-2 poly-xyz-2 poly-xyz-2))

(DEFINE-RAT-POLY-PROJECT-VECTOR rat-poly-1-normalized-project-vector  
    (poly-xyz-1 poly-xyz-1 poly-xyz-1 poly-xyz-1))

(DEFINE-RAT-POLY-PROJECT-VECTOR rat-poly-1-common-normalized-project-vector  
    (poly-xyz-1 poly-xyz-1 poly-xyz-1 ) t)

(DEFINE-RAT-POLY-PROJECT-VECTOR rat-poly-3-1-normalized-project-vector
    (poly-xyz-3 poly-xyz-1 poly-xyz-3 poly-xyz-1))

(DEFINE-RAT-POLY-PROJECT-VECTOR rat-poly-3-1-common-normalized-project-vector
    (poly-xyz-3 poly-xyz-3 poly-xyz-1) t)



;;; general 3rd order rational polynomial transform -- independent denominators
(DEFINE-RAT-POLY-TRANSFORM-3-VECTOR rat-poly-3-normalized-transform-vector
    (poly-xyz-3 poly-xyz-3 poly-xyz-3 poly-xyz-3))

(DEFINE-RAT-POLY-TRANSFORM-3-VECTOR rat-poly-2-normalized-transform-vector
    (poly-xyz-2 poly-xyz-2 poly-xyz-2 poly-xyz-2))

(DEFINE-RAT-POLY-TRANSFORM-3-VECTOR rat-poly-1-normalized-transform-vector
    (poly-xyz-1 poly-xyz-1 poly-xyz-1 poly-xyz-1))



;;;
;;; Absolutely sucky.  Current practice is to rely on the 3d-to-2d
;;; projection machinery in OpenGL to display objects.  Doesn't work
;;; for rational polynomials.  One alternative is to pre-project from
;;; 3D to 2.5D image coordinates (+ height), then treat this in the
;;; usual way (as for frame cameras), if we want to continue to feed
;;; full 3D objects into OpenGL.  This is a view-centric operation,
;;; perhaps accomplished by storing the 2.5D intermediate results on
;;; the view or 2d-world property list.
;;;

(defvar *bogus-identity-matrix* nil)

;;; Twice as sucky now that fit-4x4-etc-etc takes different keywords.

#+never
(defun build-surrogate-projection (image zmin zmax near-far)
  (let* ((projection (3d-to-2d-projection image))
	 (surrogate (fit-4x4-projection-to-2d-to-3d-projection projection
							       (cv 0.0 (float (img::image-x-dim image))
								   0.0 (float (img::image-y-dim image))
								   zmin zmax)
							       '( 9 9 5)
							       near-far)))
    (setf (get-prop projection :surrogate-projection) surrogate)))


(defmethod interior-and-exterior-matrices ((projection rat-poly-projection))
  (unless *bogus-identity-matrix*
    (format t "~%Warning: identity matrices generated for (interior-and-exterior-matrices ~a)" projection)
    (setf *bogus-identity-matrix* (make-4x4-identity-matrix)))
  (values *bogus-identity-matrix* *bogus-identity-matrix*))

(define-soft-slot rat-poly-projection :surrogate-projection surrogate-projection)

#+maybe
(defmethod interior-and-exterior-matrices ((projection t))
  (unless *bogus-identity-matrix*
    (format t "~%Warning: identity matrices generated for (interior-and-exterior-matrices ~a)" projection)
    (setf *bogus-identity-matrix* (make-4x4-identity-matrix)))
  (values *bogus-identity-matrix* *bogus-identity-matrix*))

