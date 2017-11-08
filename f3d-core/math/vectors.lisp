(in-package :math)

(deftype coordinate-vector-element-type () 'double-float)
(deftype coordinate-ivector-element-type () '(signed-byte 32))

(deftype coordinate-vector (&optional n) `(simple-array double-float (,(or n '*))))
(deftype coordinate-ivector (&optional n) `(simple-array (signed-byte 32) (,(or n '*))))

;;; These vectors types are treated as COLUMN VECTORS during matrix multiplication.
(deftype dvector (&optional n) `(simple-array double-float (,(or n '*))))
(deftype ivector (&optional n) `(simple-array (signed-byte 32) (,(or n '*))))
(deftype fvector (&optional n) `(simple-array fixnum (,(or n '*))))
(deftype gvector (&optional n) `(simple-array * (,(or n '*))))

;;; These vectors types are treated as ROM VECTORS during matrix multiplication.
(deftype row-gvector (&optional n) `(array * (1 ,(or n '*))))
(deftype row-dvector (&optional n) `(array double-float (1 ,(or n '*))))

;;; array-dimension-limit/2 because double-float-arrays require 2*n words which we want to be a fixnum.
(deftype array-index () `(integer 0 ,(ash array-dimension-limit -1)))

(declaim (inline make-dvector))
(defun make-dvector (n)
  (declare (type array-index n))
  (make-array n :element-type 'double-float))

(declaim (inline fill-dvector))
(defun fill-dvector (vector &optional (value 0.0))
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dvector vector))
  (declare (double-float value))
  (loop for i fixnum from 0 below (length vector)
	do (setf (aref vector i) value)))

(defun subvector (vector n start &optional v)
  (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum n start))
  (declare (type dvector vector))
  (let ((v (or v  (make-array n :element-type (array-element-type vector)))))
    (declare (type dvector v))
    (loop for to-i fixnum from 0 below n
	  for from-i fixnum from start
	  do (setf (aref v to-i) (aref vector from-i)))
    v))

(defun (setf subvector) (sub-vect vector n start)
  (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum n start))
  (declare (type dvector vector sub-vect))
  (loop for from-i fixnum from 0 below n
	for to-i fixnum from start
	do (setf (aref vector to-i) (aref sub-vect from-i)))
  sub-vect)


(declaim (inline column-to-row-vector))
(defun column-to-row-vector (col-vector)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type gvector col-vector))
  (make-array (list 1 (length col-vector)) :element-type (array-element-type col-vector)
	      :displaced-to col-vector))

(declaim (inline row-to-column-vector))
(defun row-to-column-vector (row-vector)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type (array * (1 *)) row-vector))
  (array-simple-vector row-vector))

#+(or :cmu :sbcl) ; assume speed and safety settings allow the type-decl machinery to do the type checking
(defmacro type-check (type &rest vars)
  `(progn))

#+allegro ; Allegro type decls do not enforce runtime type checking.
(defmacro type-check (type &rest vars)
  `(progn ,.(loop for var in vars
		  collect `(unless (typep ,var ',type)
			     (error "The binding of ~a = ~a is not a ~a" ',var ,var ',type)))))

#+(or :cmu :sbcl)
(progn

(defmacro bind-vector-elements (element-let-list vector-form &body body)
  `(let* ((%vector%  ,vector-form))
     (declare (type (or null coordinate-vector) %vector%))
     (when %vector%
       (let ,(loop for i from 0
                   for var in element-let-list
                   collect `(,var (aref %vector% ,i)))
         (declare (double-float . ,element-let-list))
         .,body))))


(defmacro mv-setq-vector-elements (element-setq-list vector-form)
  `(let* ((%vector%  ,vector-form))
     (declare (type (or null coordinate-vector) %vector%))
     (when %vector%
       (setf . ,(loop for i from 0
                      for var in element-setq-list
                      collect var collect `(aref %vector% ,i)))
       t)))

) ;end progn

;;; Allegro type inference doesn't exist.
#+allegro
(defmacro bind-vector-elements (element-let-list vector-form &body body)
  `(let* ((%vector%  ,vector-form))
     (when %vector%
       (let* ((%vector% %vector%)
	     ,@(loop for i from 0
                   for var in element-let-list
                   collect `(,var (aref %vector% ,i))))
	     (declare (type coordinate-vector %vector%))
	     (declare (double-float . ,element-let-list))
	     .,body))))


;;; Allegro type inference doesn't exist.
#+allegro
(defmacro mv-setq-vector-elements (element-setq-list vector-form)
  `(let* ((%vector%  ,vector-form))
     (when %vector%
       (let ((%vector% %vector%))
	 (declare (type coordinate-vector %vector%))
	 (setf . ,(loop for i from 0
			for var in element-setq-list
			collect var collect `(aref %vector% ,i)))
	 t))))

(defparameter *print-vector-length* 10)

#+unused
(defun print-coordinate-vector-p (x) (and (vectorp x)
					  (eq (array-element-type x) 'double-float)
					  (<= (length x) *print-vector-length*)))

#|
CMUCL does not use PRINT-OBJECT methods for primitive (non CLOS(PCL)) objects,
thus defining PRINT-OBJECT methods on primitive types has no effect.

vectors and small 2d arrays are the only types that FREEDIUS would like to print fully
   when *print-array* = NIL

Could modify lisp::output-vector in cmucl/src/code/print.lisp
|#

;;; This is called by FORMAT and PRINT in Allegro, but not CMUCL.
(defmethod print-object :around ((x vector) stream)
  (if (or;;*print-escape*
       (> (length x) *print-vector-length* ))
      (call-next-method)
      (if nil
	  (let ((*print-array* t))
	    #-quicklisp (declare (special *print-array*))
	    (call-next-method))
	  (if (eq (array-element-type x) 'double-float)
	      (progn (format stream "#.(~s" 'cv)
		     (loop for i from 0 below (length x)
			   do (format stream " ~a" (aref x i)))
		     (format stream ") "))
	      (let ((*print-array* t)) #-quicklisp (declare (special *print-array*))
		   (call-next-method))
	      ))))

(defun print-vector (v)
  (let ((*print-array* t))(format t "~a~%" v)))

(defun make-coordinate-vector (n)
  (make-array n :element-type 'double-float :initial-element 0.0d0))

#|
(defun tst (v i)
  (declare (optimize (speed 3) (safety 1)))
  (declare (ext:optimize-interface (speed 3) (safety 1)))
  (declare (type (or null (coordinate-vector)) v))
  (declare (fixnum i))
  (= (aref v i) 0.0))

(defun tst (v i)
  (declare (optimize (speed 3) (safety 1)))
  (declare (ext:optimize-interface (speed 3) (safety 1)))
  (declare (type (or null coordinate-vector) v))
  (declare (fixnum i))
  (= (aref v i) 0.0))

(disassemble 'tst)
(disassemble 'copy-coordinate-vector)
|#

(defun copy-coordinate-vector (from-vector &optional to-vector truncation-ok)
  (declare (type coordinate-vector from-vector))
  (declare (optimize (speed 3) (safety 1)))
  (let* ((from-length (length from-vector))
	 (to-vector (or to-vector (make-coordinate-vector (length from-vector))))
	 (to-length (length to-vector)))
    (declare (fixnum from-length to-length))
    (declare (type coordinate-vector to-vector))
    (if (< to-length from-length )
	(if truncation-ok
	    (loop for i fixnum from 0 below to-length
		  do (setf (aref to-vector i) (aref from-vector i)))
	    (error "Cannot copy vector ~a into ~A~%" from-vector to-vector))
	(progn (loop for i fixnum from 0 below from-length
		     do (setf (aref to-vector i) (aref from-vector i)))
	       ;; not sure this is needed
	       (when  (> to-length from-length)
		 ;;(break)
		 (setf (aref to-vector from-length) 1.0)) ; homogeneous component -- where should it go?
	       ))
    to-vector))

;;;(defmacro inline-check-coordinate-vector (vector length)
;;;  `(unless (>= (vector-length ,vector) ,length)
;;;    (error "Coordinate vector ~a of should have length >= ~d" ,vector ,length)))

;;;(defmacro inline-coordinate-vector (&rest elems)
;;;  (case (length elems)
;;;    (3 `(let ((vector (make-array 3 :element-type 'double-float)))
;;;         (declare (type (coordinate-vector 3) vector))
;;;         (setf (aref vector 0) ,(first elems)
;;;          (aref vector 1) ,(second elems)
;;;          (aref vector 2) ,(third elems))
;;;         vector))
;;;    (2 `(let ((vector (make-array 2 :element-type 'double-float)))
;;;         (declare (type (coordinate-vector 2) vector))
;;;         (setf (aref vector 0) ,(first elems)
;;;          (aref vector 1) ,(second elems))
;;;         vector))
;;;    (4 `(let ((vector (make-array 4 :element-type 'double-float)))
;;;         (declare (type (coordinate-vector 4) vector))
;;;         (setf (aref vector 0) ,(first elems)
;;;          (aref vector 1) ,(second elems)
;;;          (aref vector 2) ,(third elems)
;;;          (aref vector 3) ,(fourth elems) )
;;;          
;;;         vector))
;;;    ;;(otherwise `(set-coordinate-vector-elements (make-coordinate-vector ,(length elems)) . ,elems))
;;;    (otherwise `(let ((vector (make-array ,(length elems) :element-type 'double-float)))
;;;                 (declare (type (coordinate-vector *) vector))
;;;                 (setf .,(loop for i from 0 below (length elems)
;;;                               for elem in elems
;;;                               collect `(aref vector ,)
;;;                               collect elem))
;;;                 vector))))

(defmacro inline-coordinate-vector (&rest elems)
  `(let ((vector (make-array ,(length elems) :element-type 'double-float)))
    (declare (type (coordinate-vector *) vector))
    (setf .,(loop for elem in elems
		  for i from 0
		  collect `(aref vector ,i)
		  collect elem))
    vector))

(defmacro coordinate-vector (&rest elems)
  (case (length elems)
    (2 `(coordinate-vector-2d . ,elems))
    (3 `(coordinate-vector-3d . ,elems))
    (4 `(coordinate-vector-4d . ,elems))
    (otherwise `(coordinate-vector-general . ,elems))))

(defmacro cv (&rest elems)
  (case (length elems)
    (2 `(coordinate-vector-2d . ,elems))
    (3 `(coordinate-vector-3d . ,elems))
    (4 `(coordinate-vector-4d . ,elems))
    (otherwise `(coordinate-vector-general . ,elems))))

(defun cv3 (x y z)
  (inline-coordinate-vector (dfloat x) (dfloat y) (dfloat z)))

(defun coordinate-vector-4d (x y z w)
  (inline-coordinate-vector (dfloat x) (dfloat y) (dfloat z) (dfloat w)))

(defun coordinate-vector-3d (x y z)
  (inline-coordinate-vector (dfloat x) (dfloat y) (dfloat z)))

(defun coordinate-vector-2d (x y)
  (inline-coordinate-vector (dfloat x) (dfloat y)))
      
(defun coordinate-vector-general (&rest elems)
  (let ((length (length elems)))
    (case length
      (3 (let ((vector (make-array 3 :element-type 'coordinate-vector-element-type)))
           (declare (type coordinate-vector vector))
           (setf (aref vector 0) (dfloat (first elems))
                 (aref vector 1) (dfloat (second elems))
                 (aref vector 2) (dfloat (third elems)))
           vector))
      (2 (let ((vector (make-array 2 :element-type 'coordinate-vector-element-type)))
           (declare (type coordinate-vector vector))
           (setf (aref vector 0) (dfloat (first elems))
                 (aref vector 1) (dfloat (second elems)))
           vector))
      (otherwise  (let ((vector (make-array length :element-type 'coordinate-vector-element-type)))
                    (declare (type coordinate-vector vector))
                    (loop for x in elems
                          for i fixnum from 0
                          do (setf (aref vector i) (dfloat x)))
                    vector)))))

(defun set-coordinate-vector-elements (vector &rest elems)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type coordinate-vector vector))
  (loop for elem in elems
	for i fixnum from 0
	do (setf (aref vector i) (dfloat elem)))
  vector)

(defmacro inline-set-coordinate-vector-elements (vector &rest forms)
  `(let ((!!vector!! ,vector))
     (declare (type coordinate-vector !!vector!!))
     (setf . ,(loop for i from 0
                    for form in forms
                    collect `(aref !!vector!! ,i)
                    collect form))
     !!vector!!))

(defun coordinate-vector-elements (vector)
  (declare (type coordinate-vector vector))
  (values-list (loop for i fixnum from 0 below (length vector)
		     collect (aref vector i))))

(defun normalize-coordinate-vector (vector)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type coordinate-vector vector))
  (let ((length (the double-float
		    (sqrt (loop for i fixnum from 0 below (length vector)
				for elem double-float = (aref vector i)
				sum (* elem elem) double-float))))
        (1/length 0.0d0))
    (declare (double-float length 1/length))
    (unless (zerop length)
      (setq 1/length (/ 1.0 length))
      (loop for i fixnum  from 0 below (length vector)
            do (setf (aref vector i) (* (aref vector i) 1/length))))
    vector))


;; integer version
(defmacro bind-ivector-elements (element-let-list vector-form &body body)
  `(let* ((%vector%  ,vector-form))
     (when %vector%
       (let* ((%vector% %vector%)
	      ,@(loop for i from 0
		      for var in element-let-list
		      collect `(,var (aref %vector% ,i))))
	 (declare (type coordinate-ivector %vector%))
         (declare (fixnum . ,element-let-list))
         .,body))))

(defmacro inline-coordinate-ivector (&rest elems)
  `(let ((vector (make-array ,(length elems) :element-type 'coordinate-ivector-element-type)))
    (declare (type (coordinate-ivector *) vector))
    (setf .,(loop for elem in elems
		  for i from 0 
		  collect `(aref vector ,i)
		  collect elem))
    vector))

(defmacro coordinate-ivector (&rest elems)
  (case (length elems)
    (2 `(coordinate-ivector-2d . ,elems))
    (3 `(coordinate-ivector-3d . ,elems))
    (4 `(coordinate-ivector-4d . ,elems))
    (otherwise `(coordinate-ivector-general . ,elems)))
  )

(defmacro civ (&rest elems)
  `(coordinate-ivector ,.elems))

(defun coordinate-ivector-4d (x y z w)
  (inline-coordinate-ivector x y z w))

(defun coordinate-ivector-3d (x y z)
  (inline-coordinate-ivector x y z))

(defun coordinate-ivector-2d (x y)
  (inline-coordinate-ivector x y))

(defun coordinate-ivector-general (&rest elems)
  (let ((length (length elems)))
    (case length
      (3 (let ((vector (make-array 3 :element-type 'coordinate-ivector-element-type)))
           (declare (type coordinate-ivector vector))
           (setf (aref vector 0) (first elems)
                 (aref vector 1) (second elems)
                 (aref vector 2) (third elems))
           vector))
      (2 (let ((vector (make-array 2 :element-type 'coordinate-vector-element-type)))
           (declare (type coordinate-vector vector))
           (setf (aref vector 0) (first elems)
                 (aref vector 1) (second elems))
           vector))
      (otherwise  (let ((vector (make-array length :element-type 'coordinate-ivector-element-type)))
                    (declare (type coordinate-ivector vector))
                    (loop for x in elems
                          for i fixnum from 0
                          do (setf (aref vector i) x))
                    vector)))))





(defmacro inline-cross-prod ((x1 y1 z1) (x2 y2 z2) (rx ry rz))
  `(setf ,rx (- (* ,y1 ,z2)
		(* ,z1 ,y2))
	 ,ry (- (* ,z1 ,x2)
		(* ,x1 ,z2))
	 ,rz (- (* ,x1 ,y2)
		(* ,y1 ,x2))))

;;; returns  skew-symmetric matrix S(v) such that S.w = cross(v,w)
(defun cross-product-matrix (v)
  (bind-vector-elements (x y z) v
    (make-and-fill-3x3-matrix 0.0 (- z) y
			      z 0.0 (- x)
			      (- y) x 0)))

(defmacro inline-inner-prod ((&rest elems1) (&rest elems2))
  `(+ . ,(loop for x1 in elems1
	       for x2 in elems2
	       collect `(* ,x1 ,x2))))

(defmacro solve2! (a1 b1 c1 a2 b2 c2 x1 x2)
  `(let* ((%a1 ,a1) (%b1 ,b1) (%c1 ,c1)
	  (%a2 ,a2) (%b2 ,b2) (%c2 ,c2)
	  (%det (dfloat (- (* %a2 %b1) (* %a1 %b2)))))
     (declare (double-float %a1 %b1 %c1 %a2 %b2 %c2 %det))
     (if (zerop %det)
	 nil
	 (progn (setq %det (/ %det))
		(setf ,x1 (* %det (- (* %b1 %c2) (* %b2 %c1)))
		      ,x2 (* %det (- (* %a2 %c1) (* %a1 %c2))))
		t)))) 


;;; This should move to math package ---
;;; This solves the matrix equation Mx + y = 0
;;; where x and y are 2-vectors and M is 2x2.
;;; a1*x+b1+y+c1=0 and
;;; a2*x+b2+y+c2=0
;;; This is different from CME version (signs of c1 c2 reversed).
(defun solve2r (a1 b1 c1 a2 b2 c2)
  (declare (double-float a1 b1 c1 a2 b2 c2 ))
  (let ((det (dfloat (- (* a2 b1) (* a1 b2)))))
    (declare (double-float det))
    (if (zerop det)
	nil
	(progn (setq det (/ det))
	       (values (* det (- (* b2 c1) (* b1 c2)))
		       (* det (- (* a1 c2) (* a2 c1))))))))

;;; least squares solve sum ||a*x[i] +b -y[i]|| = min
(defun linear-regression (xarr yarr)
  (declare (type (simple-array double-float (*)) xarr yarr))
  (loop for i of-type fixnum from 0 below (length xarr)
	for xi of-type double-float = (aref xarr i)
	for yi of-type double-float = (aref yarr i)
	sum xi into sx double-float
	sum yi into sy double-float		     
	sum (* xi xi) into sxx double-float
	sum (* xi yi) into sxy double-float
	finally
     (multiple-value-bind (a b)
	 (solve2r sxx sx (- sxy)
		  sx (dfloat (length xarr)) (- sy))
       (return (values a b
		       (loop for ri in (linear-regression-residuals xarr yarr a b)
			     sum (^2 ri) into sri
			     finally (return (sqrt (/ sri (length xarr)))))
		       )))))

(defun linear-regression-residuals (xarr yarr a b)
  (loop with n of-type fixnum = (length xarr)
	for i of-type fixnum from 0 below n
	collect (- (aref yarr i) (+ b(* a (aref xarr i))))))

(defmacro solve-quadratic! (a b c x1 x2 ok)
  `(let* ((%a ,a) (%b ,b) (%c ,c)
	  (%b^2-4ac (- (* %b %b) (* 4.0 %a %c))))
     (declare (double-float %a %b %c %b^2-4ac))
     (if (minusp %b^2-4ac)
	 (setq ,ok nil)
	 (let ((%sqrt (sqrt %b^2-4ac)))
	   (declare (double-float %sqrt))
	   (setq ,x1 (/ (+ (- %b) %sqrt) (* 2.0 %a))
		 ,x2 (/ (- (- %b) %sqrt) (* 2.0 %a))
		 ,ok t))))) 


;;; Unlike its CME-6 counterpart, INLINE-MATRIX-TIMES-VECTOR works for
;;; matrices of general dimensions, not just 4x4 matrices.
;;; For good code generation, the matrix must be declared:
;;; (declare (type (simple-array double-float (nrows ncols)) m))
(defmacro inline-matrix-times-vector (m input-vars output-vars)
  (labels ((product-optimize (a b)
	     (cond ((or (equal a 0.0) (equal b 0.0)) nil)
		   ((equal a 1.0) (list b))
		   ((equal b 1.0) (list a))
		   (t `((* ,a ,b)))))
	   (mvoptimize (m input-vars row)
	     `(+ . ,(loop for col from 0
			  for in-var in input-vars
			  nconc (product-optimize in-var `(aref ,m ,row ,col))))))
    (if (eq (car output-vars) 'values)
	`(values . ,(loop for row in (cdr output-vars)
			  collect (mvoptimize  m input-vars row)))
	`(setf . ,(loop for row from 0
			for out-var in output-vars
			when out-var
			collect out-var
			and collect (mvoptimize m input-vars row))))))

(defmacro inline-euclidean-length (&rest elems)
  `(let ((tmp 0.0d0))
    (declare (double-float tmp))
    (declare (ignorable tmp))
    (the double-float
	(sqrt
	 (the double-float
	     (+ . ,(loop for elem in elems
			 collect (if (symbolp elem)
				     `(* ,elem ,elem)
				     `(the double-float (progn (setq tmp ,elem)
							       (* tmp tmp)))))))))))

(defun list-vector (v) (loop for i from 0 below (length v) collect (aref v i)))

(defmacro normalize-vector-elements (&rest elems)
  `(let ((l (inline-euclidean-length . ,elems)))
    (declare (double-float l ))
    (unless (zerop l)
      (setq l (/ 1.0 l))
      ,@(loop for elem in elems
	       collect `(setf ,elem (* ,elem l)))
      t)))


(defun euclidean-length (&rest elements)
  (declare (optimize (speed 3) (safety 1)))
  (declare (dynamic-extent elements))
  (sqrt (the double-float
	    (loop for x double-float in elements
		  sum (* x x) double-float))))


;;; ****************************  VECTOR OPS  ****************************

(defun make-vector (n)
  (make-array n :element-type 'double-float :initial-element 0.0d0))

(defmacro inline-check-coordinate-vector (vector length)
  `(unless (>= (length ,vector) ,length)
     (error "Coordinate vector ~a of should have length >= ~d" ,vector ,length)))

(defmacro define-vector-op (function-name args expression 
			    &key
			    (input-vectors args)
			    (element-type 'double-float) 
			    declares)
  (let ((input-vectors (loop for v in input-vectors
			     until (eq v '&optional)
			     collect v))
	(into-vector (loop for (v . rest) on input-vectors by #'cdr
			   when (eq v '&optional)
			     return (car rest)
			   finally (return 'into-vector)))
	(vector-type `(simple-array ,element-type (*))))
    `(defun ,function-name ,args
      (declare (optimize (speed 3) (safety 1)))
      (declare (type ,vector-type ,@input-vectors))
      (declare (type (or null ,vector-type) ,into-vector))
      ,declares
      (type-check ,vector-type .,input-vectors)
      (type-check (or null ,vector-type) ,into-vector)
      (let ((n (length ,(car input-vectors ))))
	(when ,into-vector (inline-check-coordinate-vector ,into-vector n))
	(let ((into-vector (or ,into-vector 
			       ;(make-vector n)
			       (make-array n :element-type ',element-type)
			       )))
	  (declare (fixnum n))
	  (declare (type (simple-array ,element-type (*)) into-vector ))
	  (loop for i fixnum from 0 below n
		do (setf (aref into-vector i) ,expression))
	  into-vector )))))

(define-vector-op vector-copy (vector &optional into-vector)
  (aref vector i))

(define-vector-op vector-add (v1 v2 &optional into-vector )
  (+ (aref v1 i) (aref v2 i)))

(define-vector-op vector-difference (v1 v2 &optional into-vector)
  (- (aref v1 i) (aref v2 i) ))

(define-vector-op vector-negate (v1 &optional into-vector)
  (- (aref v1 i) ))

(define-vector-op vector-times-scalar (vector scalar &optional into-vector)
  (* scalar (aref vector i))
  :input-vectors (vector)
  :declares (declare (double-float scalar)))

(define-vector-op vector-linear-combine (scale1 v1 scale2 v2 &optional into-vector)
  (+ (* scale1 (aref v1 i)) (* scale2 (aref v2 i)))
  :input-vectors (v1 v2)
  :declares (declare (double-float scale1 scale2 )))

;(disassemble 'copy-vector)
(defun copy-vector (from &optional to)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dvector from))
  (declare (type (or null dvector) to))
  (type-check dvector from)
  (type-check (or null dvector) to)
  (let* ((n (length from))
	 (to (or to (make-array n :element-type 'double-float))))
    (declare (fixnum n))
    (declare (type dvector to))   
    (unless (= n (length to))
      (error "Vectors must have same dimensions ~a ~a" from to))
    
    (loop for i fixnum from 0 below n
	  do (setf (aref to i) (aref from i)))
    to))
  

;;; This is also called the "P-Infinity Norm"
(defun vector-max-abs-norm (a)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dvector a))
  (type-check dvector a)
  (let ((n (length a)))
    (declare (fixnum n))
    (loop for i fixnum from 0 below n
	  maximize (abs (aref a i)) double-float)))

;;; This is also called the "P-2 Norm"
(defun vector-rms-norm (x)
  (/ (vector-euclidean-length x) (sqrt (dfloat (length x)))))


(defun vector-max-abs-difference (a b)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dvector a b))
  (type-check dvector a b)
  (let ((n (length a)))
    (declare (fixnum n))
    (unless (equal n (length b))
      (error "Vectors must have same dimensions ~a ~a" a b))
    (loop for i fixnum from 0 below n
	  maximize (abs (- (aref a i) (aref b i))) double-float)))

;;; This is misnamed -- no "mean" is taken.  
;;; Rename to vector-Euclidian-difference
(defun vector-rms-difference (a b)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dvector a b))
  (type-check dvector a b)
  (let ((n (length a)))
    (declare (fixnum n))
    (unless (equal n (length b))
      (error "Vectors must have same dimensions ~a ~a" a b))
    (sqrt (/ (loop for i fixnum from 0 below n
		   for diff double-float = (- (aref a i) (aref b i))
		   sum (* diff diff) double-float)
	     n))))

(defparameter *vector-equal-threshold* 0.0)

;(disassemble 'vector-equal)
(defun vector-equal (v1 v2 &optional (threshold *vector-equal-threshold*))
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dvector v1 v2))
  (declare (double-float threshold))
  (type-check dvector v1 v2)
  (type-check double-float threshold)
  (loop for i fixnum from 0 below (length v1)
	always (< (- (aref v1 i) (aref v2 i)) threshold)))

(defun vector-inner-product (v1 v2)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dvector v1 v2))
  (type-check dvector v1 v2)
  (loop for i fixnum from 0 below (length v1)
	sum (* (aref v1 i) (aref v2 i)) double-float))

;;; moved here from $RADIUSCODE/SRI/hub/start-cme-patch.lisp

(defun vector-dot-product (v1 v2)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dvector v1 v2))
  (type-check dvector v1 v2)
  (loop for i fixnum from 0 below (length v1)
	sum (* (aref v1 i) (aref v2 i)) double-float))

(defun det-2d (vector0 vector1)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dvector vector0 vector1))
  (- (* (aref vector0 0) (aref vector1 1))
     (* (aref vector0 1) (aref vector1 0))))
   
(defun det-3d (vector0 vector1 vector2)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type (simple-array double-float (*)) vector0 vector1 vector2))
  (- (+ (* (aref vector0 0) (aref vector1 1) (aref vector2 2))
	(* (aref vector0 1) (aref vector1 2) (aref vector2 0))
	(* (aref vector0 2) (aref vector1 0) (aref vector2 1)))
     (+ (* (aref vector0 2) (aref vector1 1) (aref vector2 0))
	(* (aref vector0 0) (aref vector1 2) (aref vector2 1))
	(* (aref vector0 1) (aref vector1 0) (aref vector2 2)))))

(defun scalar-triple-product (vector1 vector2 vector3)
  (det-3d vector1 vector2 vector3))
  
(defun vector-triple-product (v1 v2 v3)
  (vector-linear-combine (vector-inner-product v1 v3) v2
			 (- (vector-inner-product v1 v2)) v3))
  


(defun vector-euclidean-length (vector)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dvector vector ))
  (type-check dvector vector)
  (the double-float
      (sqrt (the double-float
		(loop for i fixnum from 0 below (length vector )
		      for elem double-float = (aref vector i)
		      sum (* elem elem) double-float)))))

(defun vector-to-vector-distance (v1 v2)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dvector v1 v2))
  (type-check dvector v1 v2)
  (the double-float
      (sqrt (the double-float
		(loop for i fixnum from 0 below (length v1)
		      for delta double-float = (- (aref v1 i) (aref v2 i))
		      sum (* delta delta ) double-float)))))

;;; Compute the ddistance from v0 to the line thru arc-v1 and arc-v2
;;; This is based on ||V1xV2|| = ||V1||*||V2||*sin(theta)
;;; and  ptl-dist = ||V2||*sin(theta)
;;; thus ptl-dist = ||V1xV2||/||V1||
;;;(defun vector-to-arc-distance (v0 arc-v1 arc-v2)
;;;  #+cmu (declare (ext:optimize-interface (speed 1) (safety 2)))
;;;  (declare (optimize (speed 3) (safety 1)))
;;;  (declare (type (simple-array double-float (3)) v0 arc-v2 arc-v2))
;;;  (bind-vector-elements (x y z) v0
;;;    (bind-vector-elements (x1 y1 z1) arc-v1
;;;      (bind-vector-elements (x2 y2 z2) arc-v2
;;;        (let* ((dx (- x2 x1))
;;;               (dy (- y2 y1))
;;;               (dz (- z2 z1))
;;;               (dx1 (- x x1))
;;;               (dy1 (- y y1))
;;;               (dz1 (- z z1))
;;;               (length-squared (+ (^2 dx) (^2 dy) (^2 dz)))
;;;               (distance-squared
;;;                (and (> length-squared 0.0)
;;;                     (/ (+ (^2 (- (* dy1 dz) (* dz1 dy)))
;;;                           (^2 (- (* dz1 dx) (* dx1 dz)))
;;;                           (^2 (- (* dx1 dy) (* dy1 dx))))
;;;                        length-squared))))
;;;          (declare (type double-float dx dy dz length-squared))
;;;          (declare (type (or null double-float) distance-squared))
;;;          #+never
;;;          (unless distance-squared
;;;            (format t "vector-to-arc-distance arc-v1 = arc-v2~%")
;;;            (break))
;;;          (if distance-squared
;;;              (the double-float (sqrt (the double-float distance-squared)))
;;;              (vector-to-vector-distance arc-v1 v0)))))))

(defun vector-to-arc-distance (v0 arc-v1 arc-v2)
  #+cmu (declare (ext:optimize-interface (speed 1) (safety 2)))
  (declare (optimize (speed 3) (safety 1)))
  (declare (type (dvector 3) v0 arc-v1 arc-v2))
  (type-check (dvector 3) v0 arc-v1 arc-v2)
  (bind-vector-elements (x y z) v0
    (bind-vector-elements (x1 y1 z1) arc-v1
      (bind-vector-elements (x2 y2 z2) arc-v2
        (let* ((dx (- x2 x1))
               (dy (- y2 y1))
               (dz (- z2 z1))
               (dx1 (- x x1))
               (dy1 (- y y1))
               (dz1 (- z z1))
               (length-squared (+ (^2 dx) (^2 dy) (^2 dz))))
          (declare (double-float dx dy dz dx1 dy1 dz1 length-squared))
	  (if (> length-squared 0.0)
	      (let ((distance-squared (/ (+ (^2 (- (* dy1 dz) (* dz1 dy)))
					    (^2 (- (* dz1 dx) (* dx1 dz)))
					    (^2 (- (* dx1 dy) (* dy1 dx))))
					 length-squared)))
		(declare (type (double-float 0.0 *) distance-squared))
		(the double-float (sqrt (the double-float distance-squared))))
	      (progn
		#+never
		(progn (format t "vector-to-arc-distance arc-v1 = arc-v2~%")
		       (break))
		(vector-to-vector-distance arc-v1 v0))))))))

(defun vector-to-internal-arc-distance (v0 arc-v1 arc-v2)
  #+cmu (declare (ext:optimize-interface (speed 1) (safety 2)))
  (declare (optimize (speed 3) (safety 1)))
  (declare (type (dvector 3) v0 arc-v1 arc-v2))
  (type-check (dvector 3) v0 arc-v1 arc-v2)
  (bind-vector-elements (x y z) v0
    (bind-vector-elements (x1 y1 z1) arc-v1
      (bind-vector-elements (x2 y2 z2) arc-v2
	(let* ((arc-dx (- x2 x1))
               (arc-dy (- y2 y1))
               (arc-dz (- z2 z1))
               (qx (- x x1))
               (qy (- y y1))
               (qz (- z z1))
               (dir2-dot-dir2 (+ (* arc-dx arc-dx) (* arc-dy arc-dy) (* arc-dz arc-dz)))
               (dir2-dot-q (+ (* arc-dx qx) (* arc-dy qy) (* arc-dz qz))))
	  (when (> dir2-dot-dir2  0.0)
	    (let ((alpha (/ dir2-dot-q dir2-dot-dir2)))
	      (declare (type double-float alpha))
	      (and  (>= alpha 0.0) (<= alpha 1.0)
		    (inline-euclidean-length (- x (+ x1 (* alpha arc-dx)))
					     (- y (+ y1 (* alpha arc-dy)))
					     (- z (+ z1 (* alpha arc-dz)))))
	      )))))))


(defun nearest-point-on-arc (v0 arc-v1 arc-v2)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dvector v0 arc-v2 arc-v2))
  (type-check dvector v0 arc-v2 arc-v2)
  (bind-vector-elements (x y z) v0
    (bind-vector-elements (x1 y1 z1) arc-v1
      (bind-vector-elements (x2 y2 z2) arc-v2
        (let* ((arc-dx (- x2 x1))
               (arc-dy (- y2 y1))
               (arc-dz (- z2 z1))
               (qx (- x x1))
               (qy (- y y1))
               (qz (- z z1))
               (dir2-dot-dir2 (+ (* arc-dx arc-dx) (* arc-dy arc-dy) (* arc-dz arc-dz)))
               (dir2-dot-q (+ (* arc-dx qx) (* arc-dy qy) (* arc-dz qz)))
               (alpha (and (> dir2-dot-dir2  0.0)
                           (/ dir2-dot-q dir2-dot-dir2))))
          (and alpha
               (values (coordinate-vector (+ x1 (* alpha arc-dx))
                                          (+ y1 (* alpha arc-dy))
                                          (+ z1 (* alpha arc-dz)))
                       alpha))
          )))))


;;;(defun normalize-vector (vector)
;;;  ;;(declare (type (simple-array double-float (*)) vector))
;;;  (let ((length (the double-float
;;;                    (sqrt (loop for i fixnum from 0 below (length vector)
;;;                                for elem double-float = (aref vector i)
;;;                                sum (* elem elem) double-float))))
;;;        (1/length 0.0d0))
;;;    (declare (double-float length 1/length))
;;;    (unless (zerop length)
;;;      (setq 1/length (/ 1.0 length))
;;;      (loop for i fixnum  from 0 below (length vector)
;;;            do (setf (aref vector i) (* (aref vector i) 1/length))))
;;;    vector))

(defun normalize-vector (vector &optional (into-vector vector))
  (let* ((length (vector-euclidean-length vector)))
    (declare (double-float length))
    (vector-times-scalar vector (if (> length 0.0) (/ length) 1.0) into-vector )))

(defun vector-cross-product (v1 v2 &optional into-vector)
  (declare (optimize (speed 3) (safety 1)))
  (bind-vector-elements (x1 y1 z1) v1
    (bind-vector-elements (x2 y2 z2) v2
      (let ((nx 0.0d0) (ny 0.0d0) (nz 0.0d0))
	(declare (double-float nx ny nz))
	(inline-cross-prod (x1 y1 z1) (x2 y2 z2) (nx ny nz))
	(inline-set-coordinate-vector-elements (or into-vector (make-coordinate-vector 3))
					nx ny nz)))))

;;;(defun max-vector-element (vector)
;;;  (case (array-element-type vector)
;;;    (double-float
;;;     (let ((vector vector))
;;;       (declare (type (simple-array double-float (*)) vector))
;;;       (loop for i fixnum from 0 below (length vector)
;;;             for x double-float = (aref vector i)
;;;             maximize (if (plusp x) x (- x)) double-float)))
;;;    (t (loop for i fixnum from 0 below (length vector)
;;;             for x double-float = (aref vector i)
;;;             maximize (if (plusp x) x (- x)) double-float))))

(defun max-vector-element (vector)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type gvector vector))
  (type-check gvector vector)
  (case (array-element-type vector)
    (double-float
     (let ((vector vector))
       (declare (type dvector vector))
       (loop for i fixnum from 0 below (length vector)
	     with max-elem double-float = 0.0
	     with max-index fixnum = 0
	     for x double-float = (aref vector i)
	     when (< x 0.0)
	       do (setq x (- x))
	     when (> x max-elem)
	       do (setq max-elem x max-index i)
	     finally (return (values max-elem max-index)))))

    (t (loop for i fixnum from 0 below (length vector)
	     for x  = (aref vector i)
	     with max-elem = 0.0
	     with max-index fixnum = 0
	     when (< x 0.0)
	       do (setq x (- x))
	     when (> x max-elem)
	       do (setq max-elem x max-index i)
	     finally (return (values max-elem max-index))))))

;;; polar vector = (azimuth elevation length)
;;; angles in degrees - azimuth direction clockwise relative to y-axis (north)

(defun polar-to-cartesian (polar-vector)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type (dvector 3) polar-vector))
  (type-check (dvector 3) polar-vector)
  (bind-vector-elements (azimuth elevation length) polar-vector
      (let ((cos-az (cosd azimuth))
	    (sin-az (sind azimuth))
	    (cos-el (cosd elevation))
	    (sin-el (sind elevation))
	    )
	(cv (* length cos-el sin-az)
	    (* length cos-el cos-az)
	    (* length sin-el)))))

;;; Returns azimuth (degrees relative to North), elevation (degrees),
;;; and length.  Is this misnamed -- the issue is the convention
;;; returning azimuth relative to North rather than East.
(defun cartesian-to-polar (direction-vector)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type (dvector 3) direction-vector))
  (type-check (dvector 3) direction-vector)
  (bind-vector-elements (dx dy dz) direction-vector
    (let* ((length (euclidean-length dx dy dz))
	   (azimuth (if (and (zerop dx) (zerop dy)) 0.0 (* #.(/ 180.0 pi) (atan dx dy))))
	   (elevation (if (zerop length) 0.0 (* #.(/ 180.0 pi) (asin (/ dz length))))))
      (cv azimuth elevation length))))


(defun 3d-point-inside-bbox (pt bbox &optional (eps 1e-3))
  (declare (optimize (speed 3) (safety 1)))
  (declare (type (dvector 3) pt))
  (declare (type (dvector 6) bbox))
  (declare (double-float eps))
  (type-check (dvector 3) pt)
  (type-check (dvector 6) bbox)
  (bind-vector-elements (xmin xmax ymin ymax zmin zmax) bbox
    (bind-vector-elements (x y z) pt
      (and (>= x (- xmin eps)) (<= x (+ xmax eps))
	   (>= y (- ymin eps)) (<= y (+ ymax eps))
	   (>= z (- zmin eps)) (<= z (+ zmax eps))))))

(defun 2d-point-inside-bbox (pt bbox &optional (eps 1e-3))
  (declare (optimize (speed 3) (safety 1)))
  (declare (type (dvector 2) pt))
  (declare (type (dvector 4) bbox))
  (declare (double-float eps))
  (type-check (dvector 2) pt)
  (type-check (dvector 4) bbox)
  (bind-vector-elements (umin umax vmin vmax) bbox
    (bind-vector-elements (u v) pt
      (and (>= u (- umin eps)) (<= u (+ umax eps))
	   (>= v (- vmin eps)) (<= v (+ vmax eps))))))

(defun point-inside-bbox (pt bbox &optional (eps 1e-3))
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dvector pt bbox))
  (declare (double-float eps))
  (type-check dvector pt bbox)
  (loop for i fixnum from 0 below (length pt)
	for 2i fixnum from 0 by 2
	for x double-float = (aref pt i)
	always (and (>= x (- (aref bbox 2i) eps))
		    (<= x (+ (aref bbox (1+ 2i)) eps)))))
