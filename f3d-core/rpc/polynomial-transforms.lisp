;;; -*- MODE: Lisp; Package: TRANSFORMS; Syntax: COMMON-LISP; Base: 10 -*-

(IN-PACKAGE :TRANSFORMS)

#+never
(USER::FILE-PROPERTY-LIST 
   :FILE-NAME "$CMEHOME/transforms/polynomial-transforms.lisp"
   :SYSTEM "3DIUS"
   :DISTRIBUTION-CATEGORY :IN-QUESTION
   :TIME-STAMP "$Date: 2001/02/21 15:31:27 connolly"
   :COMMENT "Polynomial coordinate transforms - least squares fitting.")

#||

Yanked from the compat/rpc freedius system and combed for
compatibility with the new core freedius rpc stuff. -CC

||#

#|
                                       TM
                                  3DIUS

    UNPUBLISHED-RIGHTS RESERVED UNDER THE COPYRIGHT LAWS OF THE UNITED STATES. 
   
     This licensed material is the valuable property of SRI International.
   Its use, duplication, or disclosure is subject to the restrictions
   set forth in the License Agreement.  
   
     Use is limited to the licensed machine(s) identified by Serial Number(s)
   in the License Agreement and on a backup machine only when a licensed
   machine is inoperative because of malfunction or emergency.  A backup copy
   may be made.
   
     Other use, duplication or disclosure, unless expressly provided for in the
   License Agreement, is unlawful.

   3DIUS, ImagCalc, and CME Copyrights 1991, 1992, 1993, 1994 SRI International 
|#

#|
DEPENDENCIES ON OTHER FILES

((IC::PUT-PROP "lisp/lisp-extensions.lisp")
 (MATH:MULTIPLY-MATRICES "math/matrices.lisp")
 (CME::COORDINATE-VECTOR-3D "math/new-coord-transforms.lisp")
 (IC::WITH-CLASS-SLOT-VALUES "lisp/struct-class.lisp")
 (CME::TRANSFORM "*undeclared generic functions*")
 (CME::TRANSFORM-VECTOR "math/new-coord-transforms.lisp")
 (IC::RANDOM-IN-RANGE "lisp/lisp-extensions.lisp")
 (MATH::LEAST-SQUARES-RESIDUALS "math/matrices.lisp")
 (MATH:SOLVE "math/matrices.lisp")
 (MATH::LEAST-SQUARES-SOLVE-SETUP "math/matrices.lisp"))


(user::maybe-compile-file-load "$CMEHOME/math/polynomial-transforms.lisp")
|#

;;; ***********************    POLYNOMIAL COORDINATE TRANSFORM   ***********************
;;;
;;; In the new regime, maybe this should be a functional-coordinate-transform?
;;;
(defstruct-class polynomial-coordinate-transform
    (coordinate-transform)
  ((coeffs-vector-vector :initform nil :initarg :coeffs-vector-vector)
   (mean-vector :initform nil :initarg :mean-vector)
   (n-coeffs-seq :initform nil :initarg :n-coeffs-seq)
   )
  )

(defun fasd-coeffs-vector-vector (coeffs-vector-vector)
  `(vector . ,(loop for i from 0 below (length coeffs-vector-vector)
		    collect (fasd-form (aref coeffs-vector-vector i)))))

(defmethod linear-transform-p ((xform polynomial-coordinate-transform))
  nil)

;;; One of these is wrong 

;;;(define-fasd-form-init-plist polynomial-coordinate-transform
;;;    (with-class-slots
;;;        polynomial-coordinate-transform (coeffs-vector-vector mean-vector n-coeffs-seq) self
;;;        `(:coeffs-vector-vector
;;;          (make-array ,(length coeffs-vector-vector)
;;;                      :initial-contents 
;;;                      (list . ,(loop for vector being the array-elements of coeffs-vector-vector
;;;                        collect `(make-and-fill-double-float-vector ',(ic::listarray vector)))))
;;;          :mean-vector
;;;          (make-and-fill-double-float-vector ',(ic::listarray mean-vector))
;;;          :n-coeffs-seq ',n-coeffs-seq
;;;
;;;          )))

(define-fasd-form-init-plist polynomial-coordinate-transform
    (with-class-slots polynomial-coordinate-transform
	  (transform-function coeffs-vector-vector mean-vector n-coeffs-seq) self
      self
      `(,@(when mean-vector `( :mean-vector ,(fasd-form mean-vector)))
	:transform-function ',transform-function
	:coeffs-vector-vector ,(fasd-coeffs-vector-vector coeffs-vector-vector)
	;;:n-coeffs-seq ',n-coeffs-seq
	)))


(defstruct-class polynomial-coordinate-projection
    (coordinate-transform)
  ((coeffs-vector-vector :initform nil :initarg :coeffs-vector-vector)
   (mean-vector :initform nil :initarg :mean-vector)
   (n-coeffs-seq :initform nil :initarg :n-coeffs-seq)
   (zp-projection-plane :initform nil :initarg :zp-projection-plane :accessor zp-projection-plane))
  )


;;;(undefmethod '(project-to-world (polynomial-coordinate-projection t t t)))
#+old
(defmethod project-to-world  ((projection polynomial-coordinate-projection) u0 v0 z &key z-if-outside)
  (if (numberp z)
      (bind-vector-elements (x y)
	  (inverse-transform-vector projection
				    (set-coordinate-vector-elements
				     (load-time-value* (make-coordinate-vector 3)) u0 v0 z)
				    (load-time-value* (make-coordinate-vector 2)))
	(values x y z))
      (composite-coordinate-projection-project-to-world-internal nil projection nil u0 v0 z
								 :z-if-outside z-if-outside)))


;;(undefmethod '(project-to-world (polynomial-coordinate-projection t t t)))

#+old
(defmethod project-to-world :around ((projection polynomial-coordinate-projection) u0 v0 z &key z-if-outside)
  (declare (ignore  z-if-outside))
  (if nil ; (typep (inverse-transform projection) '2d-to-3d-projection )
      (call-next-method)
      (if (numberp z)
	  (bind-vector-elements (x y)
	      (inverse-transform-vector projection
					(set-coordinate-vector-elements
					 (load-time-value* (make-coordinate-vector 3)) u0 v0 z)
					(load-time-value* (make-coordinate-vector 2)))
	    (values x y z))
	  (composite-coordinate-projection-project-to-world-internal nil projection nil u0 v0 z
								     :z-if-outside z-if-outside))))

;;
#+ok?
(defmethod project-to-world  ((projection polynomial-coordinate-projection) 2d-pos z)
  (bind-vector-elements (u0 v0) 2d-pos
    (if (numberp z)
	(bind-vector-elements (x y)
	  (inverse-transform-vector projection
				    (set-coordinate-vector-elements
				     (load-time-value* (make-coordinate-vector 3)) u0 v0 z)
				    (load-time-value* (make-coordinate-vector 2)))
	  (values x y z))
      (composite-coordinate-projection-project-to-world-internal nil projection nil u0 v0 z))))


#+ok?
(defmethod project-to-world :around ((projection polynomial-coordinate-projection) 2d-pos z)
  (bind-vector-elements (u0 v0) 2d-pos
    (if nil ; (typep (inverse-transform projection) '2d-to-3d-projection )
	(call-next-method)
      (if (numberp z)
	  (bind-vector-elements (x y)
	      (inverse-transform-vector projection
					(set-coordinate-vector-elements
					 (load-time-value* (make-coordinate-vector 3)) u0 v0 z)
					(load-time-value* (make-coordinate-vector 2)))
	    (values x y z))
	  (composite-coordinate-projection-project-to-world-internal nil projection nil u0 v0 z)))))

(defun fasd-coordinate-vector (v) (and v `(coordinate-vector . ,(list-vector v))))
	
(define-fasd-form-init-plist polynomial-coordinate-projection
    (with-class-slots polynomial-coordinate-projection
	  (coeffs-vector-vector mean-vector transform-function zp-projection-plane)
	self
      `(:transform-function ',transform-function
	:coeffs-vector-vector (vector . ,(loop for i from 0 below (length  coeffs-vector-vector)
					       for cv = (aref coeffs-vector-vector i)
					       collect `(coordinate-vector . ,(list-vector cv))))
	:mean-vector ,(fasd-coordinate-vector  mean-vector)
	:zp-projection-plane ,(fasd-coordinate-vector zp-projection-plane)
	)))

(defun make-and-fill-double-float-vector (elements)
  (make-array (length elements) :element-type 'double-float
	      :initial-contents elements))
  
			    
(defmethod transform-cost ((transform polynomial-coordinate-transform))
  (* .64 (apply #'+ (class-slot-value polynomial-coordinate-transform transform 'N-COEFFS-SEQ)))
  )


(eval-when (load eval compile)
(defun make-polynomial-coordinate-transform
    (transform-function n-coeffs-seq coeffs-vector-vector mean-vector
			&rest initargs &key (transform-class 'polynomial-coordinate-transform)
			&allow-other-keys)
  (apply 'make-instance transform-class
	 :transform-function transform-function
	 :coeffs-vector-vector coeffs-vector-vector
	 :n-coeffs-seq n-coeffs-seq
	 :mean-vector mean-vector
	 initargs))
)



#|
(setq polyxx1 (generate-polynomial-transform-function
	       (let* ((x (aref from-vector 0))
		      (y (aref from-vector 1))
		      (z (aref from-vector 2))
		      (coeffs (aref coeffs-vector-vector 0)))
		 (declare (type (simple-array double-float (*)) coeffs))
		 (setf (aref to-vector 0)
		       (+ (* (aref coeffs 0) x)
			  (* (aref coeffs 1) y)
			  (* (aref coeffs 2) z)
			  (aref coeffs 3))))))

(setq pt (make-polynomial-coordinate-transform polyxx1 '(4)
					       (vector (coordinate-vector 1.0 10.0 100.0 1000.0) )
	  nil))
(transform-point pt 1.0 2.0 3.0)


(setq polyxx (generate-polynomial-transform-function
	      (let* ((x (aref from-vector 0))
		     (y (aref from-vector 1))
		     (z (aref from-vector 2))
		     (xx (* x x))
		     (yy (* y y))
		     (xy (* x y))
		     (coeffs (aref coeffs-vector-vector 0)))
		(declare (type (simple-array double-float (*)) coeffs))
		(setf (aref to-vector 0)
		      (+ (* (aref coeffs 0) xx)
			 (* (aref coeffs 1) yy)
			 (* (aref coeffs 2) xy)
			 (* (aref coeffs 3) x)
			 (* (aref coeffs 4) y)
			 (* (aref coeffs 5) z)
			 (aref coeffs 6))))))

(setq pt2 (make-polynomial-coordinate-transform
	   polyxx '(6) (vector (coordinate-vector .001 .01 .1 1.0 10.0 100.0 1000.0)) nil))

(transform-point pt2 1.0 2.0 3.0)
(transform-point pt2 2.0 0.0 0.0)


(time (test-transform 100000 pt2 (coordinate-vector 1.0 2.0 3.0) (coordinate-vector 0.0))) ; 4.3 usec no consing
|#


(defun test-transform (n transform vector &optional (to-vector vector))
  (loop repeat n do (transform-vector transform vector to-vector)))



(defun poly-least-squares-solve-compute-mean-vector (vector-vector &optional (element-type 'double-float))
  (declare (simple-vector vector-vector))
  (let* ((m-eqns (length vector-vector))
	 (n-vars (vector-length (aref vector-vector 0)))
	 (mean-vector (make-array n-vars :element-type element-type :initial-element 0.0)))
    (declare (type (simple-array double-float (*)) mean-vector))
    (loop for var fixnum from 0 below n-vars
	  do (setf (aref mean-vector var)
		   (/ (the double-float
			  (loop for eqn fixnum from 0 below m-eqns
				sum (the double-float
					(aref (the (simple-array double-float (*))
						  (aref vector-vector eqn))
					      var))
				double-float))
				 (float m-eqns))))
    mean-vector))

(defun poly-least-squares-solve-build-cmat-vector
    (polynomial-transform c-mat-vector from-vector-vector &optional (element-type 'double-float))
  (declare (simple-vector c-mat-vector from-vector-vector))
  (with-class-slots
      polynomial-coordinate-transform (n-coeffs-seq coeffs-vector-vector) polynomial-transform
    (declare (simple-vector coeffs-vector-vector))
    (let* ((n-to-coords (length c-mat-vector))
	   (m-pts (length from-vector-vector))
	   (tmp-vector (make-array n-to-coords :element-type 'double-float :initial-element 0.0))
	   (max-n-coeffs (loop for i fixnum from 0 below n-to-coords
			       maximize (the fixnum (elt n-coeffs-seq i)) fixnum))
	   )
      (declare (fixnum n-to-coords m-pts max-n-coeffs))
      (declare (type (simple-array double-float (*)) tmp-vector))
      (loop for i fixnum from 0 below n-to-coords
	    for n-coeffs fixnum = (elt n-coeffs-seq i)
	    do (setf (aref c-mat-vector i) (make-array (list m-pts n-coeffs) :element-type element-type)))

      ;; zero out all coeffs
      (loop for to-index fixnum from 0 below n-to-coords
	    do (loop for var fixnum from 0 below (elt n-coeffs-seq to-index)
		     do (setf (aref (the (simple-array double-float (*))(aref coeffs-vector-vector to-index))
				    var)
			      0.0)))
    
      (loop for pt-index fixnum from 0 below m-pts
	    do (let ((from-vector (aref from-vector-vector pt-index)))
		 (declare (type (simple-array double-float (*)) from-vector))
		 (loop for var fixnum from 0 below max-n-coeffs
		       do (loop for to-index fixnum from 0 below n-to-coords
				when (< var (the fixnum (elt n-coeffs-seq to-index)))
				  ;; Turn on the basis functions of each output function one at a time.
				  do (setf (aref (the (simple-array double-float (*))
						     (aref coeffs-vector-vector to-index))
						 var) 1.0))
			  (transform-vector polynomial-transform from-vector tmp-vector)
			  (loop for to-index fixnum from 0 below n-to-coords
				when (< var (the fixnum (elt n-coeffs-seq to-index)))
				  do;; Turn off the previously enabled basis functions.
				    (setf (aref (the (simple-array double-float (*))
						    (aref coeffs-vector-vector to-index))
						var) 0.0)
				    ;; Record the from-vector mapped thru these basis functions.
				    (setf (aref (the (simple-array double-float (* *))
						    (aref c-mat-vector to-index))
						pt-index var)
					  (aref tmp-vector to-index))))))
    
      c-mat-vector)))


;;;(defun error-anal (error-list)
;;;  (loop with n = (length error-list)
;;;        for err in error-list
;;;        maximize (abs err) into max-err
;;;        sum (* err err) into sum-err^2
;;;        sum err into sum-err
;;;        finally (return (values max-err
;;;                                (sqrt (- (/ sum-err^2 n) (^2 (/ sum-err n))))))))

(defun error-anal (error-list)
  (loop with n = (length error-list)
	for err in error-list
	maximize (abs err) into max-err
	sum (* err err) into sum-err^2
	sum err into sum-err
	finally (return (values max-err
				(sqrt (/ sum-err^2 n))))))

(defun compute-polynomial-fit-error-info (residuals)
  (loop with (max-err rms-err)
	for resid-list in residuals
	do (multiple-value-setq (max-err rms-err)
	       (error-anal resid-list))
	collect max-err into max-errs
	collect rms-err into rms-errs
	finally (return (list max-errs
			      rms-errs
			      ;;residuals
			      ))
	))

(defparameter *least-squares-fit-polynomial-transform-normalize-from-vector* t)

(defmethod least-squares-fit-polynomial-transform
    ((polynomial-transform polynomial-coordinate-transform) from-vector-vector to-vector-vector
			  &key
			  (normalize-from-vector *least-squares-fit-polynomial-transform-normalize-from-vector*)
			  (element-type 'double-float))
  (least-squares-fit-polynomial-transform-internal polynomial-transform from-vector-vector to-vector-vector
						   normalize-from-vector element-type))
	
(defmethod least-squares-fit-polynomial-transform
    ((polynomial-transform polynomial-coordinate-projection) from-vector-vector to-vector-vector
			  &key
			  (normalize-from-vector *least-squares-fit-polynomial-transform-normalize-from-vector*)
			  (element-type 'double-float))
  (least-squares-fit-polynomial-transform-internal polynomial-transform from-vector-vector to-vector-vector
						   normalize-from-vector element-type))

						   
(defparameter *least-squares-fit-polynomial-transfoRm-solve-case* :svd)

(defun least-squares-solve-setup (c-mat)
  (let* ((c-trans (transpose-matrix c-mat))
	 (c-trans-c (multiply-matrices c-trans c-mat)))
    (multiple-value-bind (lu ps) (decompose c-trans-c)
      (values lu ps c-trans))))

(defun least-squares-fit-polynomial-transform-internal
    (polynomial-transform from-vector-vector to-vector-vector
			  normalize-from-vector element-type
			  &optional (solve-case *least-squares-fit-polynomial-transform-solve-case*))
  (with-class-slots
      polynomial-coordinate-transform (n-coeffs-seq mean-vector coeffs-vector-vector) polynomial-transform
    (let* ((n-pts (length from-vector-vector))
	   (to-coords (vector-length (aref to-vector-vector 0)))
	   (rhs-vect (make-array n-pts :element-type element-type))
	   (c-mat-vector (make-array to-coords))
	   )
      (setq coeffs-vector-vector (make-array to-coords))
      (loop for i from 0 below to-coords
	    do (setf (aref coeffs-vector-vector i) (make-array (elt n-coeffs-seq i) :element-type element-type)))

      (when normalize-from-vector
	(setq mean-vector (poly-least-squares-solve-compute-mean-vector from-vector-vector element-type)))
	
      (poly-least-squares-solve-build-cmat-vector polynomial-transform c-mat-vector from-vector-vector element-type)
  
      (loop for to-coord fixnum from 0 below to-coords
	    for coeffs-vector = (aref coeffs-vector-vector to-coord)
	    for c-mat = (aref c-mat-vector to-coord)
	    do (loop for i fixnum from 0 below n-pts
		     do (setf (aref rhs-vect i) (aref (aref to-vector-vector i) to-coord)))
	       (case solve-case
		 (:lu (math::lu-least-squares-solve c-mat rhs-vect coeffs-vector))
		 (:cholesky (math::cholesky-least-squares-solve c-mat rhs-vect coeffs-vector))
		 (:svd (math::svd-least-squares-solve c-mat rhs-vect coeffs-vector))
		 )
		 
	    collect (multiple-value-list (math::least-squares-residuals c-mat coeffs-vector rhs-vect))
	      into residuals

	    finally (setf (get-prop polynomial-transform :least-squares-fit-errors)
			  (compute-polynomial-fit-error-info residuals))
		    (return (values polynomial-transform residuals))))))


;;;(defun copy-polynomial-transform
;;;    (template-polynomial-transform &rest initargs)
;;;  (when template-polynomial-transform
;;;    (with-class-slots
;;;        polynomial-coordinate-transform (transform-function n-coeffs-seq) template-polynomial-transform
;;;        (apply 'make-polynomial-coordinate-transform
;;;               transform-function N-COEFFS-SEQ nil nil
;;;               initargs))))

;;;(fmakunbound 'copy-polynomial-transform)


(defmethod copy-polynomial-transform
    ((template-polynomial-transform polynomial-coordinate-transform) &rest initargs)
  (when template-polynomial-transform
    (with-class-slots
	polynomial-coordinate-transform (transform-function n-coeffs-seq) template-polynomial-transform
	(apply 'make-polynomial-coordinate-transform
	       transform-function N-COEFFS-SEQ nil nil
	       :transform-class (class-of template-polynomial-transform)
	       initargs))))

(defmethod copy-polynomial-transform
    ((template-polynomial-transform polynomial-coordinate-projection) &rest initargs)
  (when template-polynomial-transform
    (with-class-slots
	polynomial-coordinate-transform (transform-function n-coeffs-seq) template-polynomial-transform
	(apply 'make-polynomial-coordinate-transform
	       transform-function N-COEFFS-SEQ nil nil
	       :transform-class (class-of template-polynomial-transform)
	       initargs))))

(defun make-polynomial-transform-pair
    (template-polynomial-transform inverse-template-polynomial-transform exact-transform)
  (with-class-slots
      coordinate-transform (from-coordinate-system to-coordinate-system) exact-transform
      (let* ((polynomial-transform
	      (copy-polynomial-transform template-polynomial-transform
					 :from-coordinate-system from-coordinate-system
					 :to-coordinate-system to-coordinate-system))
	     (inverse-polynomial-transform
	      (copy-polynomial-transform inverse-template-polynomial-transform
					 :from-coordinate-system to-coordinate-system
					 :to-coordinate-system from-coordinate-system
					 :inverse-transform polynomial-transform)))
	(when polynomial-transform
	  (setf (inverse-transform polynomial-transform) inverse-polynomial-transform))
	(values polynomial-transform inverse-polynomial-transform))))


(defun make-to-from-vectors-with-random-sampling (transform bounding-box n )
  (let* ((n-dims (ash (length bounding-box) -1))
	 (from-coord-array (make-array n))
	 (to-coord-array (make-array n))
	 (function-p (functionp transform))
	 (ri 0))
    (declare (fixnum ri))
    (declare (type (simple-array t (*)) from-coord-array to-coord-array))
    (loop for i fixnum from 0 below n
	  do (let ((from-vector (make-coordinate-vector n-dims))
		   (to-vector (make-coordinate-vector n-dims)))
	       (declare (type (simple-array double-float (*)) from-vector to-vector ))
  
	       (loop for dim fixnum from 0 below n-dims
		     for bbox-index from 0 by 2
		     do (setf (aref from-vector dim)
			      (lx::random-in-range (aref bounding-box bbox-index)
						   (aref bounding-box (1+ bbox-index) ))))
	       (setq to-vector (if function-p
				   (funcall transform from-vector to-vector)
				   (transform-vector transform from-vector to-vector)))
	       (when to-vector
		 (setf (aref from-coord-array ri) from-vector
		       (aref to-coord-array ri) to-vector
		       ri (1+ ri)))))
	     
    (if (= ri n)
	(values from-coord-array to-coord-array)
	(values (adjust-array-copy from-coord-array ri)
		(adjust-array-copy to-coord-array ri)
		(- n ri)))
    ))

(defun make-to-from-vectors-with-uniform-sampling
    (transform bounding-box n-list &optional (n-to-dims (length n-list)))
  (let* ((n (apply '* n-list))
	 (from-coord-array (make-array n))
	 (to-coord-array (make-array n))
	 (n-dims (length n-list))
	 (tmp-to-vector (make-coordinate-vector 20))
	 (index (make-array n-dims :element-type 'fixnum))
	 (ns (make-array n-dims :element-type 'fixnum))
	 (deltas (make-array n-dims :element-type 'double-float))
	 (starts (make-array n-dims :element-type 'double-float))
	 (pos (make-array n-dims :element-type 'double-float))
	 (bad-count 0)
	 (ri 0)
	 (function-p (functionp transform)))
    (declare (fixnum ri))
    (declare (type (simple-array t (*)) from-coord-array to-coord-array))
    (declare (type (simple-array double-float (*)) starts deltas pos tmp-to-vector ))
    (declare (type (simple-array fixnum (*)) index ns ))
    (ignore tmp-to-vector)
    (loop for i fixnum from 0 below n-dims
	  for bbox-index from 0 by 2
	  for ni = (elt n-list i)
	  for start double-float = (aref bounding-box bbox-index)
	  do (setf (aref deltas i)
		   (if (> ni 1)
		       (/ (- (aref bounding-box (1+ bbox-index) ) start ) (1- ni))
		       0.0)
		   (aref starts i) start
		   (aref pos i) start
		   (aref ns i) ni
		   (aref index i) ni))
	 
    (loop for i fixnum from 0 below n
	  do (let ((from-vector (make-coordinate-vector n-dims))
		   ;;(to-vector (make-coordinate-vector n-dims)) ; n-dims looks wrong
		   (to-vector (make-coordinate-vector n-to-dims))	
		   )
	       (declare (type (simple-array double-float (*)) from-vector to-vector ))
	       (loop with carry = t
		     for dim fixnum from 0 below n-dims
		     do (setf (aref from-vector dim) (aref pos dim ) )
		     when carry
		       do (incf (aref pos dim) (aref deltas dim))
			  (if (= 0 (the fixnum (setf (aref index dim)
						     (the fixnum
							 (1- (the fixnum (aref index dim)))))))
			      (setf (aref index dim ) (aref ns dim )
				    (aref pos dim) (aref starts dim))
			      (setq carry nil)))
	       (if (if function-p
		       (funcall transform from-vector to-vector)
		       (transform-vector transform from-vector to-vector))
		   (progn (setf (aref to-coord-array ri) to-vector)
			  (setf (aref from-coord-array ri) from-vector 
				ri (1+ ri)))
		   (incf bad-count)))
	  )
    (when (> bad-count 0)
      (format t ";;; warning: make-to-from-vectors-with-uniform-sampling could not transform ~d points~%"
	      bad-count))
    (if (= ri n)
	(values from-coord-array to-coord-array bad-count)
	(values (adjust-array-copy from-coord-array ri)
		(adjust-array-copy to-coord-array ri)
		bad-count))))

	     
    
(defun least-squares-fit-polynomial-transform-pair-make-to-from-vectors
    (transform bounding-box n-seq &optional (sampling-type :uniform ))
  (case sampling-type
    (:uniform (make-to-from-vectors-with-uniform-sampling
		     transform bounding-box n-seq ))
    (:random (make-to-from-vectors-with-random-sampling
	      transform transform n-seq ))
    (otherwise (error "Unknowm sampling type ~A" sampling-type ))))

(defun least-squares-fit-polynomial-transform-pair
    (template-polynomial-transform template-inverse-polynomial-transform exact-transform
				   bounding-box n-seq sampling-type)
  (multiple-value-bind (polynomial-transform inverse-polynomial-transform)
      (make-polynomial-transform-pair template-polynomial-transform template-inverse-polynomial-transform exact-transform)
    (multiple-value-bind (from-coord-vector to-coord-vector)
	(least-squares-fit-polynomial-transform-pair-make-to-from-vectors
	 exact-transform bounding-box n-seq sampling-type )

      (when polynomial-transform
	(least-squares-fit-polynomial-transform polynomial-transform from-coord-vector to-coord-vector))
      (when inverse-polynomial-transform
	(least-squares-fit-polynomial-transform inverse-polynomial-transform to-coord-vector from-coord-vector))
      (values polynomial-transform inverse-polynomial-transform))))


#|
(setq trans1 (make-4x4-coordinate-transform
	      (make-and-fill-4x4-matrix 2.0 0.0 0.0 0.0
					0.0 20.0 0.0 0.0
					0.0 0.0 200.0 0.0
					0.0 0.0 0.0 1.0 )))

(setq xx (multiple-value-list
	     (make-to-from-vectors-with-uniform-sampling
	      trans1
	      (cv 0.0 100.0 0.0 1000.0)
	      (list 5 5))))

(setq xx (multiple-value-list
	     (make-to-from-vectors-with-random-sampling
	      trans1
	      (cv 0.0 100.0 0.0 1000.0)
	      25)))
|#


;;; ***************************  SPECIAL CASE FOR 3D-TO-3D TRANSFORMS  ****************************

(defun 3d-make-to-from-vectors-with-random-sampling (3d-to-3d-transform bounding-box n)
  (bind-vector-elements (start-x end-x start-y end-y start-z end-z) bounding-box 
    (let* ((from-coord-array (make-array n))
	   (to-coord-array (make-array n))
	   (function-p (functionp 3d-to-3d-transform)))
      (loop for i from 0 below n
	    for x = (lx::random-in-range start-x end-x)
	    for y = (lx::random-in-range start-y end-y)
	    for z = (lx::random-in-range start-z end-z)
	    for from-vector = (coordinate-vector x y z)
	    for to-vector = (coordinate-vector 0.0 0.0 0.0)
	    do (if function-p
		   (funcall 3d-to-3d-transform from-vector to-vector)
		   (transform-vector 3d-to-3d-transform from-vector to-vector))
	       (setf (aref from-coord-array i) from-vector
		     (aref to-coord-array i) to-vector)
	    finally;;(setq foo from-coord-array) (break)
		   (return (values from-coord-array to-coord-array))))))

(defun 3d-make-to-from-vectors-with-uniform-sampling (3d-to-3d-transform bounding-box nx ny nz)
  (bind-vector-elements (start-x end-x start-y end-y start-z end-z) bounding-box
    (let* ((n (* nx ny nz))
	   (from-coord-array (make-array n))
	   (to-coord-array (make-array n))
	   (function-p (functionp 3d-to-3d-transform))
	   )
    
      (loop with type = 0.0d0
	    with i = 0
	    with dx = (/ (float (- end-x start-x) type) (1- nx))
	    with dy = (/ (float (- end-y start-y) type) (1- ny))
	    with dz = (if (< nz 2) 0.0 (/ (float (- end-z start-z) type) (1- nz) ))
	    repeat ny
	    for y from (float start-y type) by dy
	    do (loop repeat nx
		     for x from (float start-x type) by dx
		     do (loop repeat nz
			      for z from (float start-z type) by dz
			      for from-vector = (coordinate-vector x y z)
			      for to-vector = (coordinate-vector 0.0 0.0 0.0)
			      do (if function-p
				     (funcall 3d-to-3d-transform from-vector to-vector)
				     (transform-vector 3d-to-3d-transform from-vector to-vector))
				 (setf (aref from-coord-array i) from-vector
				       (aref to-coord-array i) to-vector)
				 (incf i)))
	    finally (return (values from-coord-array to-coord-array))))))

(defun 3d-least-squares-fit-polynomial-transform-pair-make-to-from-vectors
    (3d-to-3d-transform bounding-box sampling-info)
  (case (car sampling-info)
    (:uniform (apply '3d-make-to-from-vectors-with-uniform-sampling
		     3d-to-3d-transform bounding-box (cdr sampling-info)))
    (:random (3d-make-to-from-vectors-with-random-sampling
	      3d-to-3d-transform bounding-box (cadr sampling-info)))
    (otherwise (error "Unknowm sampling specification ~A" sampling-info))))

(defun 3d-least-squares-fit-polynomial-transform-pair
    (template-polynomial-transform template-inverse-polynomial-transform exact-transform
				   bounding-box sampling-info)
  (multiple-value-bind (polynomial-transform inverse-polynomial-transform)
      (make-polynomial-transform-pair template-polynomial-transform template-inverse-polynomial-transform exact-transform)
    (multiple-value-bind (from-coord-vector to-coord-vector)
	(3d-least-squares-fit-polynomial-transform-pair-make-to-from-vectors
	 exact-transform bounding-box sampling-info)

      (when polynomial-transform
	(least-squares-fit-polynomial-transform polynomial-transform from-coord-vector to-coord-vector))
      (when inverse-polynomial-transform
	(least-squares-fit-polynomial-transform inverse-polynomial-transform to-coord-vector from-coord-vector))
      (values polynomial-transform inverse-polynomial-transform))))

#|
(setq *default-convert-to-poly-projection-3d-to-2d-poly* *poly-x3-y3-z3-projection-template*
      *default-convert-to-poly-transform-3d-to-2d-poly* *poly-x3-y3-z3-transform-template*)
(setq *default-convert-to-poly-projection-nlist* '(7 7 7))
|#

(defun convert-to-poly-projection (2d-world
				   &key bounding-box
				   (n-list *default-convert-to-poly-projection-nlist*)
				   (3d-to-2d-poly *default-convert-to-poly-projection-3d-to-2d-poly*)
				   (2d-to-3d-poly *default-convert-to-poly-transform-3d-to-2d-poly*)
				   &allow-other-keys)
  (change-projection 2d-world
		     (inverse-transform 
		      (3d-least-squares-fit-polynomial-transform-pair
		       2d-to-3d-poly
		       3d-to-2d-poly
		       (make-2d-to-3d-projection (3d-to-2d-projection 2d-world) nil)
		       bounding-box
		       (list* :uniform n-list)))))


#|
(def-simple-polynomial-transform pt-linear-3d-to-3d
    (x y z)
  ((x) (y) (z) ())
  ((x) (y) (z) ())
  ((x) (y) (z) ()))

(def-simple-polynomial-transform pt-quadratic-3d-to-3d
    (x y z)
  ((x x) (y y) (z z) (x y) (x z) (y z) (x) (y) (z) ())
  ((x x) (y y) (z z) (x y) (x z) (y z) (x) (y) (z) ())
  ((x x) (y y) (z z) (x y) (x z) (y z) (x) (y) (z) ()))

(def-simple-polynomial-transform pt-linear-3d-to-2d
    (x y z)
  ((x) (y) (z) ())
  ((x) (y) (z) ()))

(def-simple-polynomial-transform pt-linear-2d-to-2d
    (x y)
  ((x) (y) ())
  ((x) (y) ()))
|#


#|
(setq *ft-hood-long-lat-to-lvcs-transform*
      (make-composite-coordinate-transform 	    
       (list *NAD-27-lat-long-to-geocentric-transform*
	     (inverse-transform *ft-hood-lvcs-to-NAD-27-geocentric-transform*))))

(setq *ft-hood-map-uv-to-long-lat-list*
      (list (list 1515.75 34.5 (- (from-deg-min-sec 97 45)) (from-deg-min-sec 31 7 30))
	    (list 1505.75 40.25 (- (from-deg-min-sec 97 45 1)) (from-deg-min-sec 31 7 30.6))
	    (list 383.0 76.25 (- (from-deg-min-sec 97 47 30)) (from-deg-min-sec 31 7 30.6))
	    (list 1550.75 1340.5 (- (from-deg-min-sec 97 45 1)) (from-deg-min-sec 31 10))
	    (list 426.75 1382.25 (- (from-deg-min-sec 97 47 30)) (from-deg-min-sec 31 10))
	    ))

(setq *ft-hood-map-uv-to-xy-lat-transform
      (calibrate-map-image-uv-to-xy2
       pt-linear-2d-to-2d
       *ft-hood-map-uv-to-long-lat-list* *ft-hood-long-lat-to-lvcs-transform*
       :default-z (/ 900.0 *feet-per-meter*) :normalize-from-vector nil))

(solve-dtm-to-lvcs-transform (dtm *ft-hood-terrain-model*) (dtm-frame *ft-hood-terrain-model*)
			      :TEMPLATE-POLYNOMIAL-TRANSFORM pt-linear-3d-to-3d
			      :SAMPLING-INFO '(:uniform 7 7 5 ))
;; residuals
;;(0.39522097342296547 0.6390865861030761 12.209065469923985)
;;(0.14472444952308888 0.2043794790710521 6.046179646215622)

(solve-dtm-to-lvcs-transform (dtm *ft-hood-terrain-model*)
			     (get-prop *ft-hood-terrain-model* :EXACT-DEM-TO-LVCS-TRANSFORM)
			     :TEMPLATE-POLYNOMIAL-TRANSFORM pt-quadratic-3d-to-3d )
;; residuals
;;(0.0013887551717743918 0.006763129171304172 3.125191769868252E-4)
;;(0.0012145477604765785 0.0028060308837986885 7.799163323398176E-5)

(setq *solve-dtm-to-lvcs-transform-default-sampling-info* '(:random 1000))
(setq *solve-dtm-to-lvcs-transform-default-sampling-info* '(:uniform 7 7 5 ))

(solve-dtm-to-lvcs-transform (dtm *ft-hood-terrain-model*)
			     (get-prop *ft-hood-terrain-model* :EXACT-DEM-TO-LVCS-TRANSFORM)
			     :TEMPLATE-POLYNOMIAL-TRANSFORM pt-quadratic-3d-to-3d
			     :SAMPLING-INFO '(:random 1000))
;; residuals 
;;(0.002409503463695728 0.007734099337540101 3.1812345253001695E-4)
;;(8.879543333399679E-4 0.0019755423123776007 5.575406248652799E-5)

(setq pt-etl-3d-to-3d (build-simple-polynomial-transform
		       (x y z)
		       ((x) (y) ())
		       ((x) (y) ())
		       ((x x) (y y) (x y) (x) (y) (z) ())))

(solve-dtm-to-lvcs-transform (dtm *ft-hood-terrain-model*)
			     (get-prop *ft-hood-terrain-model* :EXACT-DEM-TO-LVCS-TRANSFORM)
			     :TEMPLATE-POLYNOMIAL-TRANSFORM pt-etl-3d-to-3d
			    :SAMPLING-INFO '(:uniform 7 7 5))
;; residuals
;;(0.5909285778325284 0.8719408340039081 6.847216316145932E-4)
;;(0.20152218039375972 0.26245289442297337 1.6855133571463228E-4)


(setq tec-lvcs-to-geo-transform (build-simple-polynomial-transform
				 (x y)
				 ((x x) (y y) (x y) (x) (y) ())
				 ((x x) (y y) (x y) (x) (y) ())))
				 
(setq pt-v1-3d-to-3d-template (build-simple-polynomial-transform
			       (x y z)
			       ((x x) (x z) (x) (y) (z) ()) ; 6
			       ((x x) (y y) (x y) (y z) (x) (y) (z) ()) ; 8
			       ((x x) (y y) (x y)  (x) (y) (z) ())) ; 7
      )

(setq pt-v1-3d-to-3d
      (solve-dtm-to-lvcs-transform (dtm *ft-hood-terrain-model*)
				   (get-prop *ft-hood-terrain-model* :EXACT-DEM-TO-LVCS-TRANSFORM)
				   :TEMPLATE-POLYNOMIAL-TRANSFORM pt-v1-3d-to-3d
				   :SAMPLING-INFO '(:uniform 7 7 5)))
;;(0.1391052761755418 0.009050056301930454 6.847216316145932E-4)
;;(0.0907266331601637 0.00300600688723861 1.6855133571463228E-4)

(vector-elements(transform-vector (get-prop *ft-hood-terrain-model* :EXACT-DEM-TO-LVCS-TRANSFORM)
				  (coordinate-vector 0.0 0.0 0.0)))
(vector-elements (inverse-transform-vector
		  (get-prop *ft-hood-terrain-model* :EXACT-DEM-TO-LVCS-TRANSFORM)
			  (coordinate-vector -4666.4 6562.14 958.6)))

(setq pt-v2-3d-to-3d (build-simple-polynomial-transform
		      (x y z)
		      ((x) (y) (z) (x x) (y y) ()) ; 6 
		      ((x) (y) (z) (x x) (y y) ()) ; 6
		      ((x) (y) (z) (x x) (y y) ()))) ; 6

(solve-dtm-to-lvcs-transform (dtm *ft-hood-terrain-model*)
			     (get-prop *ft-hood-terrain-model* :EXACT-DEM-TO-LVCS-TRANSFORM)
			     :TEMPLATE-POLYNOMIAL-TRANSFORM pt-v2-3d-to-3d)

;;(0.20981910499858714 0.6300076837542292 0.00226283376525771)
;;(0.09500672978155914 0.2042579774792154 7.213514432939579E-4)

(setq pt-v3-3d-to-3d-template (build-simple-polynomial-transform
			       (x y z)
			       ((x x) (y y) (x) (y) (z) ()) 
			       ((x x) (y y) (x) (y) (z) ()) 
			       ((x) (y) (z) ())))
(setq pt-v3-3d-to-3d
      (solve-dtm-to-lvcs-transform (dtm *ft-hood-terrain-model*)
			     (get-prop *ft-hood-terrain-model* :EXACT-DEM-TO-LVCS-TRANSFORM)
			     :TEMPLATE-POLYNOMIAL-TRANSFORM pt-v3-3d-to-3d-template ))
;;(0.20981910499858714 0.6300076837542292 12.209065469928305)
;;(0.09500672978156137 0.204257977479168 6.0461796462156245)

(time (test-transform 100000 pt-v3-3d-to-3d  (coordinate-vector 1.0 2.0 3.0)
		      (coordinate-vector 0.0 0.0 0.0)))
;; 10.3 usec no consing

;;; approx transform - default made by make-usgs-terrain-model using pt-quadratic-3d-to-3d template
;;;  27 double flt multiplies 30 double flt adds = (/ 16.5 57) = .29 us per flt pt op - not too bad
(time (test-transform 100000 (dtm-frame *ft-hood-terrain-model*)
		      (coordinate-vector 1.0 2.0 3.0)
		      (coordinate-vector 0.0 0.0 0.0)))
;; 16.5 usec no consing

;;; full transform
(time (test-transform 10000 (get-prop *ft-hood-terrain-model* :EXACT-DEM-TO-LVCS-TRANSFORM)
		      (coordinate-vector 1.0 2.0 3.0) (coordinate-vector 0.0 0.0 0.0 0.0)))
;; 259 usec no consing

;;; full inverse-transform
(time (test-transform 1000 (inverse-transform (get-prop *ft-hood-terrain-model* :EXACT-DEM-TO-LVCS-TRANSFORM))
		      (coordinate-vector 1.0 2.0 3.0) (coordinate-vector 0.0 0.0 0.0 0.0)))
;; 670 usec no consing


(setq long-lat-dem-transform
      (make-composite-coordinate-transform
       (list
	(make-4x4-coordinate-transform
	 (make-and-fill-4x4-matrix (/ 3 3600.0) 0.0 0.0 -98.0 ; 3 arc seconds per pixel
				   0.0 (/ 3 3600.0) 0.0 31.0
				   0.0 0.0 1.0 0.0
				   0.0 0.0 0.0 1.0))
	*NAD-27-lat-long-to-geocentric-transform*
	(inverse-transform *ft-hood-lvcs-to-NAD-27-geocentric-transform*)
	)))

(setq phoney-dtm-image '(0.0 1000.0 1200 1200)) ; 0 to 1 km elevations 1 degree square

(solve-dtm-to-lvcs-transform phoney-dtm-image
			     long-lat-dem-transform
			     :TEMPLATE-POLYNOMIAL-TRANSFORM pt-quadratic-3d-to-3d
			     :sampling-info '(:uniform 7 7 5))
(3.8176620148296934 1.5749797578901053 1.795999681758076)
(1.5772611284033649 0.6722318176109083 0.7952221027554054)

(solve-dtm-to-lvcs-transform phoney-dtm-image
			     long-lat-dem-transform
			     :TEMPLATE-POLYNOMIAL-TRANSFORM pt-quadratic-3d-to-3d
			     :sampling-info '(:uniform  13 13 5 ))
(4.315344422968337 1.8731354144401849 1.9757054853293994)
(1.327629906580761 0.5825447284314403 0.6662327226114845)

|#



;;; New (Mon Mar 20 1995) code to generate polynomials in many variables

(eval-when (eval load compile)
(defun poly-expand (n v &optional (poly-vars '(x y z)) (coeff-index -1))
  (let ((poly-vars (reverse (loop repeat v for var in poly-vars collect var))))
    (labels ((poly-expand-int (n v)
	       (if (= n 0)
		   `(aref coeffs ,(incf coeff-index))
		   `(+ (aref coeffs ,(incf coeff-index))
		     . ,(loop for k from 0 below v
			      collect `(* ,(nth (- v 1 k) poly-vars)
					,(poly-expand-int (1- n) (- v k) )))))))
      (poly-expand-int n v))))
) ; end eval-when

(defmacro inline-poly (n v offset)
  (poly-expand n v '(x y z) (1- offset)))

#|
(poly-expand 3 3)
(poly-expand 5 2)
|#

(defmacro generate-poly-fn (name n v)
  (let ((poly-vars '(x y z)))
    `(defun ,name (vars coeffs)
      (declare (type (simple-array double-float (*)) vars coeffs))
      (let ,(loop for i from 0 below v
		  for var in poly-vars
		  collect `(,var (aref vars ,i)))
	,(poly-expand n v )))))

(defun poly-x5-y5-z3 (vars coeffs into-vect into-index)
  (declare (type (simple-array double-float (*)) vars coeffs into-vect))
  (declare (fixnum into-index))
  (let* ((x (aref vars 0))
	 (y (aref vars 1))
	 (z (aref vars 2))
	 (p52 (inline-poly 5 2 0))
	  (p42 (inline-poly 4 2 21))
	 (p32 (inline-poly 3 2 36))
	 (p22 (inline-poly 2 2 46)))
    (setf (aref into-vect into-index)
	  (+ p52
	     (* z (+ p42
		     (* z (+ p32
			     (* z p22)))))))
    nil))

(defun poly-x3-y3-z3 (vars coeffs into-vect into-index)
  (declare (type (simple-array double-float (*)) vars coeffs into-vect))
  (declare (fixnum into-index))
  (let* ((x (aref vars 0))
	 (y (aref vars 1))
	 (z (aref vars 2))
	 (p32 (inline-poly 3 2 0))
	 (p22 (inline-poly 2 2 10))
	 (p12 (inline-poly 1 2 16))
	 (p02 (inline-poly 0 2 19)))
    (setf (aref into-vect into-index)
	  (+ p32
	     (* z (+ p22
		     (* z (+ p12
			     (* z p02)))))))
    nil))

(defparameter *poly-x5-y5-z3-tmp-vector* (make-coordinate-vector 3))

(defparameter *poly-x3-y3-z3-tmp-vector* (make-coordinate-vector 3))

#|
(in-package "CME")

(defun copy-world (from-image to-pane)
  (let* ((image (ic::copy-image from-image))
	 (from-2d-world (2d-world from-image)))
    
    (setup-image-worlds image
			:3d-to-2d-projection (3d-to-2d-projection from-2d-world)
			:3d-world (3d-world from-2d-world))
    (ic::push-image image  to-pane)
    nil
    ))

(copy-world mb1-j2 (ic::image-calc-pane 0 0))
(copy-world mb1-j2 (ic::image-calc-pane 0 1))
(copy-world mb1-j2 (ic::image-calc-pane 1 1))
(copy-world mb1-j2 (ic::image-calc-pane 1 0))

(setf (3d-to-2d-projection (2d-world (top-view)))
      (make-composite-coordinate-projection
       (list (make-4x4-coordinate-transform (identity-matrix 4 :element-type 'double-float))
	     (3d-to-2d-projection (2d-world (top-view))))))

;;; with new composite-projection code
(time (ic::refresh (view-window (top-view))))
;;; .71 comp fbip
;;; .59 comp 4x4
;;; .61 simple fbip
;;; .52 simple 4x4


;;; with old composite-projection code
(time (ic::refresh (view-window (top-view))))
;;; .60 comp fbip
;;; .52 comp 4x4
;;; .59 simple fbip
;;; .50 simple 4x4


with draw-on-view-insternal disabled:
(time (ic::refresh (view-window (top-view))))
;;; .16 secs in all views (time to redraw model board image)

(time (update-object cube nil))
;;; .65 comp fbip
;;; .61 simple fbip
;;; .56 comp 4x4
;;; .54 simple 4x4

;;; speed up of regen-unmodified-objects-on-view
(time (update-object cube nil))
;;; ? comp fbip
;;; .46 simple fbip
;;; .41 comp 4x4
;;; .39 simple 4x4

;;; fixed declarations for transform-array-of-vertices and transform-array-of-vertices-xx-4x4-xx
(time (update-object cube nil))
;;; .51 comp fbip
;;; .47 simple fbip
;;; .39 comp 4x4
;;; .38 simple 4x4

;;; fixed  draw-face-8b --- truncate! was causing problems
(time (loop repeat 10 do (update-object cube nil)))
;;; .42 comp fbip
;;; .30 comp 4x4
;;; .35/.20 simple fbip
;;; .33/.18 simple rpc
;;; .30/.18 simple poly
;;; .28/.15 simple 4x4

(time (let ((*update-object-inhibit-draw-on-view* t)) (update-object cube nil)))
;;; .59 no drawing-context setup
;;; .81 with drawing-context setup --- .22 secs drawing-context setup overhead

;;; after speed up of regen-unmodified-objects-on-view
(time (let ((*update-object-inhibit-draw-on-view* t)) (update-object cube nil)))
;;; .06 secs

(loop for fs in (feature-sets (top-view))
      collect (cons fs (length (inferiors fs))))

(make-image '(10 10))
(3d-to-2d-projection (2d-world (top-view)))

(get-prop cube :dash-style)


(defun tstxx (n)
  (let* ((view (top-view))
	 (obj cube)
	 (projection (3d-to-2d-projection view))
	 (object-to-world-transform (object-to-world-transform obj))
	 (2d-to-window-transform (2d-to-window-transform view))
	 )
    
    (loop repeat n do
      (multiple-value-bind (hacked-projection hacked-object-to-world-transform)
	  (compose-object-and-view-transforms-hack
	   projection object-to-world-transform *tmp-projection*)

	(transform-array-of-vertices
	 hacked-object-to-world-transform hacked-projection 2d-to-window-transform
	 (vertices obj))))))

(defun tstxx (n)
  (let* ((view (top-view))
	 (obj cube)
	 (projection (3d-to-2d-projection view))
	 (object-to-world-transform (object-to-world-transform obj))
	 (2d-to-window-transform (2d-to-window-transform view))
	 (vertices (vertices obj))
	 )
    (multiple-value-bind (hacked-projection hacked-object-to-world-transform)
	(compose-object-and-view-transforms-hack
	 projection object-to-world-transform *tmp-projection*)

      (loop repeat n do
	(transform-array-of-vertices
		  hacked-object-to-world-transform hacked-projection 2d-to-window-transform vertices)))))

(defun tstxx (n)
  (let* ((view (top-view))
	 (obj cube)
	 (projection (3d-to-2d-projection view))
	 (object-to-world-transform (object-to-world-transform obj))
	 (2d-to-window-transform (2d-to-window-transform view))
	 (vertices (vertices obj))
	 )
    
    (loop repeat n do
      (multiple-value-bind (hacked-projection hacked-object-to-world-transform)
	  (compose-object-and-view-transforms-hack
	   projection object-to-world-transform *tmp-projection*)

	(transform-array-of-vertices
	 hacked-object-to-world-transform hacked-projection 2d-to-window-transform vertices)))))

(defun tstxx (n)
  (let* ((view (top-view))
	 (obj cube)
	 (projection (3d-to-2d-projection view))
	 (object-to-world-transform (object-to-world-transform obj))
	 (2d-to-window-transform (2d-to-window-transform view))
	 (vertices (vertices obj))
	 )
    
    (loop repeat n do
     (transform-array-of-vertices
	 object-to-world-transform projection 2d-to-window-transform vertices)))))

(defun tstxx (fs)
  (let* ((view (top-view))
	 (projection (3d-to-2d-projection view))
	 )
    
    (loop for obj in (inferiors fs)
	  for vertices = (vertices obj)
	  for object-to-world-transform = (object-to-world-transform obj)
	  for 2d-to-window-transform = (2d-to-window-transform view)
	  when t
	  do (transform-array-of-vertices
		       object-to-world-transform projection 2d-to-window-transform vertices))))

(time (tstxx (car (feature-sets cube)))) ; 4512
(/ 4512 16.0)


(defun tstxx1 (obj x)
  (declare (double-float x))
  (values x x x x))

(defun tstxx2 (n)
  (loop repeat n do (tstxx1 cube 1.0)))

(time (tstxx2 1000)) 



(loop for fs in (feature-sets (top-view))
      sum (loop for obj in (inferiors fs)
	       when (slot-exists-p obj 'vertices)
		 sum (length (vertices obj))))
;;; 544

(loop for fs in (feature-sets (top-view))
      sum (loop for obj in (inferiors fs)
		when (slot-exists-p obj 'vertices)
		count 1))

51 objects with verts

|#
