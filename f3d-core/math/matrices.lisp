(in-package :math)

;;; This file depends on some deftype decls vectors.lisp.

;;; ********************************  MATRIX-TYPES  ********************************

;;; matrix with unspecified element-type
(deftype matrix (&optional (bounds '(* *))) `(simple-array * ,bounds))
(deftype gmatrix (&optional (bounds '(* *))) `(simple-array * ,bounds))
(deftype dmatrix (&optional (bounds '(* *))) `(simple-array double-float ,bounds))

(declaim (inline square-matrix-p))
(defun square-matrix-p (matrix)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type gmatrix matrix))
  (eql (array-dimension matrix 0) (array-dimension matrix 1)))

(deftype square-matrix (&optional (size '*)) `(and (simple-array * (,size ,size))
						  (satisfies square-matrix-p)))
(deftype square-gmatrix (&optional (size '*)) `(and (simple-array * (,size ,size))
						   (satisfies square-matrix-p)))
(deftype square-dmatrix (&optional (size '*)) `(and (simple-array double-float (,size ,size))
						   (satisfies square-matrix-p)))

(declaim (inline make-matrix))
(defun make-matrix (nrows ncols)
  (make-array (list nrows ncols) :element-type 'double-float :initial-element 0.0))

(declaim (inline make-dmatrix))
(defun make-dmatrix (nrows ncols)
  (make-array (list nrows ncols) :element-type 'double-float :initial-element 0.0))

(defun make-identity-matrix (n)
  (declare (fixnum n))
  (loop with m of-type dmatrix = (make-matrix n n)
        for i fixnum from 0 below n
        do (setf (aref m i i) 1.0)
           finally (return m)))

#|
(defun test-square-matrix (m)
  (declare (type square-dmatrix m))
  (type-check square-dmatrix m)
  m)
(test-square-matrix (make-array '(2 2) :element-type 'double-float))
(test-square-matrix (make-array '(2 3) :element-type 'double-float))
(defun test-square-matrix2 (m)
  (declare (type square-dmatrix m))
  (type-check square-dmatrix m)
  (setf (aref m 0 0) 1.0)
  m)
|#


;;;******************************** MATRIX-COLUMN MATRIX-ROW ********************************

;;; Allegro doesn't support inlining.  Perhaps these next inlined functions
;;; should be converted to macros.  Then it is possible to make them generic in
;;; the matrix element-type, under the assumption that the macro is expanded in
;;; the environment of appropriate array type declarations.

(declaim (inline matrix-column (setf matrix-column) matrix-row (setf matrix-row)))

;;; Stupid Lisp compilers (CMUCL and SBCL) do not do move array-index bounds checks 
;;; to outside the loops.  If you want speed, do explicit checks outside then 
;;; (declare (optimize (safety 0) (debug 0)) on the inside.

;(disassemble 'matrix-column)
(defun matrix-column (m col-i &optional col)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dmatrix m))
  (declare (fixnum col-i))
  (declare (type (or null dvector) col))
  ;(type-check dmatrix m)
  ;(type-check (or null dvector) col)
  (let* ((n (array-dimension m 0))
	 (col (or col (make-dvector n))))
    (declare (type array-index n))
    (declare (type dvector col)) 
    (loop for j fixnum from 0 below n
	  do (setf (aref col j) (aref m j col-i)))
    col))

;(disassemble '(setf matrix-column))
(defun (setf matrix-column) (col m col-i)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dmatrix m))
  (declare (type dvector col))
  (declare (fixnum col-i))
  ;(type-check dmatrix m)
  ;(type-check dvector col)
  (let* ((n (min (array-dimension m 0) (length col))))
    (declare (type array-index n))
    (loop for j fixnum from 0 below n
	  do (setf (aref m j col-i) (aref col j)))
    col))

(defun matrix-row (m row-i &optional row)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dmatrix m))
  (declare (fixnum row-i))
  (declare (type (or null dvector) row))
  ;(type-check dmatrix m)
  ;(type-check (or null dvector) row)
  (let* ((n (array-dimension m 1))
	 (row (or row (make-dvector n))))
    (declare (type array-index n))
    (declare (type dvector row))
    (loop for j fixnum from 0 below n
	  do (setf (aref row j) (aref m row-i j)))
    row))
	
(defun (setf matrix-row) (row m row-i)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dmatrix m))
  (declare (type dvector row))
  (declare (fixnum row-i))
  ;(type-check dmatrix m)
  ;(type-check dvector row)
  (let* ((n (array-dimension m 1)))
    (declare (fixnum n))
    (loop for j fixnum from 0 below n
	  do (setf (aref m row-i j) (aref row j)))
    row))

(declaim (inline swap-matrix-rows swap-matrix-columns))

(defun swap-matrix-rows (matrix row1 row2)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dmatrix matrix))
  (declare (fixnum row1 row2))
  (type-check dmatrix matrix)
  (loop for col fixnum from 0 below (array-dimension matrix 1)
	do (rotatef (aref matrix row1 col) (aref matrix row2 col))))

(defun swap-matrix-columns (matrix col1 col2)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dmatrix matrix))
  (declare (fixnum col1 col2))
  (type-check dmatrix matrix)
  (loop for row fixnum from 0 below (array-dimension matrix 0)
	do (rotatef (aref matrix row col1) (aref matrix row col2))))


(defun submatrix (matrix nrows ncols start-row start-col &optional m)
  (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum nrows ncols start-row start-col))
  (declare (type dmatrix matrix))
  (let ((m (or m  (make-array (list nrows ncols) :element-type (array-element-type matrix)))))
    (declare (type dmatrix m))
    (loop for to-row fixnum from 0 below nrows
	  for from-row fixnum from start-row
	  do (loop for to-col fixnum from 0 below ncols
		   for from-col fixnum from start-col
		   do (setf (aref m to-row to-col) (aref matrix from-row from-col))))
    m))

(defun (setf submatrix) (sub-mat matrix nrows ncols start-row start-col)
  (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum nrows ncols start-row start-col))
  (declare (type dmatrix matrix sub-mat))
  (loop for from-row fixnum from 0 below nrows
	for to-row fixnum from start-row
	do (loop for from-col fixnum from 0 below ncols
		 for to-col fixnum from start-col
		 do (setf (aref matrix to-row to-col) (aref sub-mat from-row from-col))))
  sub-mat)
	  

;;; *********************  MATRIX-COPY MATRIX-FILL ... *********************

;;;(defun copy-matrix (from &optional to)
;;;  (unless to
;;;    (setq to (make-array (array-dimensions from) :element-type (array-element-type from))))
;;;  (copy-array-contents from to)
;;;  to)

;;; This should really be called copy-dmatrix, but a lot of code uses this name.
(defun copy-matrix (from &optional to)
  (declare (type dmatrix from))
  (declare (type (or null dmatrix) to))
  (type-check dmatrix from)
  (type-check (or null dmatrix) to)
  (when to (unless (equal (array-dimensions from) (array-dimensions to))
	     (error "Matrices must have same dimensions ~a ~a" from to)))
  (let ((to (or to (make-array (array-dimensions from) :element-type (array-element-type from)))))
    (copy-vector (array-simple-vector from) (array-simple-vector to))
    to))
#|
(copy-matrix (make-object-to-parent-rotation-matrix :omega-phi-kappa '(10.0 20.0 30.0)))
(let ((m (make-object-to-parent-rotation-matrix :omega-phi-kappa '(10.0 20.0 30.0))))
  (matrix-equal m (copy-matrix m)))

|#

(defun make-and-fill-2d-array (elts &optional (element-type 'double-float))
  (make-array (list (length elts) (length (car elts)) )
	      :element-type element-type
	      :initial-contents elts))


;;; This needs serious work
(defun fill-double-array (array value)
  (declare (optimize (speed 3) (safety 1)))
  (mv-bind (arr1d offset size) (underlying-simple-vector array)
    (declare (type (simple-array double-float (*)) arr1d))
    (ignore offset)
    (loop for i fixnum from 0 below size
	  do (setf (aref arr1d i) value))))

(defun fill-matrix (matrix &optional (value 0.0))
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dmatrix matrix))
  (fill-dvector (array-simple-vector matrix) value))

(defun fill-random (x min max)
  (if (> (array-rank x) 1)
      (fill-random (array-simple-vector x) min max)
      (loop for i fixnum from 0 below (length x)
	    do (setf (aref x i) (random-in-range min max)))))

(defun list-2d-array (arr)
  (list-matrix arr))

(defun list-matrix (matrix)
  (declare (type gmatrix matrix))
  (loop for row from 0 below (array-dimension matrix 0)
	collect (loop for col from 0 below (array-dimension matrix 1)
		      collect (aref matrix row col))))

(defun print-matrix (matrix &key (stream t))
  (let ((*print-array* t))
    (pprint matrix stream)))


;;; See also 
(defun flyspec-print-matrix (matrix &key (stream t))
  (loop for row from 0 below (array-dimension matrix 0)
	do (loop for col from 0 below (array-dimension matrix 1)
		 do (write-char (if (zerop (aref matrix row col)) #\space #\X) stream))
	   (terpri stream)))
    
(defun flyspec-print-matrix (matrix &key (stream t) (expand-x "") signed-p)
  (loop for row from 0 below (array-dimension matrix 0)
	do (loop for col from 0 below (array-dimension matrix 1)
		 for elem = (aref matrix row col)
		 do (write-char (if (zerop elem) 
				    #\space
				    (if signed-p
					(if (< elem 0) #\- #\+)
					#\X))				    
			  stream)
		    (format stream expand-x)
		 )
	   (terpri stream)))
    
;;; *****************************  MATRIX-TRANSPOSE  ********************************

(defmacro generate-transpose-matrix-body (type)
  `(let* ((matrix-1d (lcl::array-simple-vector matrix))
	  (into-matrix-1d (lcl::array-simple-vector into-matrix)))
     (declare (type (simple-array ,type (*)) matrix-1d into-matrix-1d))
     (if (eq matrix into-matrix)	;special case
	 (loop for i fixnum from 0 below n
	       for mi fixnum from 0 by m
	       do (loop for j fixnum from (1+ i) below m
			for mi+j fixnum from (+ mi j)
			for mj+i fixnum from (the fixnum (+ mi m i)) by m
			do (rotatef (aref matrix-1d mj+i) (aref matrix-1d mi+j))))
	 (loop for i fixnum from 0 below n
	       for mi fixnum from 0 by m
	       do (loop for j fixnum from 0 below m
			for nj+i fixnum from i by n
			for mi+j fixnum from mi
			do (setf (aref into-matrix-1d nj+i)
				 (aref matrix-1d mi+j)))))
     into-matrix))

#+never ; for testing only -- aids disassembly.
(defmacro generate-transpose-matrix (type &optional (name type))
  `(defun ,(intern (format nil "~a-TRANSPOSE-MATRIX" name)) (matrix &optional into-matrix)
     (declare (optimize (speed 3) (safety 0)))
     (declare (type dmatrix matrix))
     (declare (type (or null dmatrix) into-matrix))
     (let* ((n (array-dimension matrix 0))
	    (m (array-dimension matrix 1)))
       (declare (fixnum n m))
       (if into-matrix
	   (unless (and (eql n (array-dimension into-matrix 1))
			(eql m (array-dimension into-matrix 0)))
	     (error "~s wrong dimensions for transpose of ~s" into-matrix matrix))
	   (setq into-matrix
		 (make-array (list m n) :element-type ',type)))
       (generate-transpose-matrix-body ,type ))))
#|
(generate-transpose-matrix double-float)
(generate-transpose-matrix single-float)
(generate-transpose-matrix fixnum)
(generate-transpose-matrix (SIGNED-BYTE 32) signed-byte-32)
(disassemble 'double-float-transpose-matrix)
(disassemble 'fixnum-transpose-matrix)
|#

(defun matrix-transpose (x &optional into)
  (declare (optimize (speed 3) (safety 1)))
  (typecase x
    (gvector (column-to-row-vector x))        ; FIXME: into not handled
    (row-gvector (row-to-column-vector x))    ; FIXME: into not handled
    (gmatrix 
     (let ((matrix x) (into-matrix into))
       (declare (type matrix matrix))
       (declare (type (or null matrix) into-matrix))
       (let ((n (array-dimension matrix 0))
	     (m (array-dimension matrix 1))
	     (element-type (array-element-type matrix)))
	 (declare (fixnum n m))
	 (if into-matrix
	     (progn
	       (unless (and (eql n (array-dimension into-matrix 1))
			    (eql m (array-dimension into-matrix 0)))
		 (error "Incompatible matrix-dimensions for ~a ~a" into-matrix matrix))
	       (unless (equal element-type (array-element-type into-matrix))
		 (error "Incompatible matrix-element-types ~a ~a" into-matrix matrix))
	       )
	     (setq into-matrix (make-array (list m n) :element-type 'double-float)))
	 (case element-type
	   (double-float (generate-transpose-matrix-body double-float))
	   (single-float (generate-transpose-matrix-body single-float))
	   (fixnum (generate-transpose-matrix-body fixnum)) 
	   (otherwise
	    (cond ((equal element-type '(SIGNED-BYTE 32)) ; fixnum in allegro
		   (generate-transpose-matrix-body (SIGNED-BYTE 32)))
		  (t (generate-transpose-matrix-body t)))))
	 into-matrix)))
    (otherwise "Argument type error: ~a" (type-of x))))

(defun transpose-matrix (matrix &optional into-matrix)
  (matrix-transpose matrix into-matrix))

#|
(setq arr (make-array '(3 3) :element-type 'double-float
		      :initial-contents '((0.0 1.0 2.0) (3.0 4.0 5.0) (6.0 7.0 8.0))))
(transpose-matrix arr)
(transpose-matrix arr arr)

(setq arr2 (make-array '(4 3) :element-type 'double-float
		      :initial-contents '((0.0 1.0 2.0) (3.0 4.0 5.0) (6.0 7.0 8.0) (9.0 10.0 11.0))))
(transpose-matrix arr2)
(transpose-matrix arr2 arr2)

(disassemble 'TRANSPOSE-MATRIX)
(transpose-matrix (make-and-fill-2d-array '((1.0 2.0 ) (3.0 4.0))))
|#

;;; **************************  VECTOR-TIMES-MATRIX MATRIX-TIMES-VECTOR  **************************

;;; This is really the same as (matrix-times-vector (transpose-matrix matrix) vector)
;;; The row-vector is implicitly used as a column-vector and the resulting column-vector
;;; implicitly represents a row-vector.
(defun vector-times-matrix (vector matrix &optional into-vector)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dmatrix matrix))
  (declare (type dvector vector))
  (declare (type (or null dvector) into-vector))
  (type-check dmatrix matrix)
  (type-check dvector vector)
  (type-check (or null dvector) into-vector)
  (let ((into-vector (or into-vector (make-array0 (array-dimension matrix 0)
						  :element-type 'double-float)))
	(nrows (array-dimension matrix 0))
	(ncols (array-dimension matrix 1)))
    (declare (fixnum nrows ncols))
    (declare (type dvector into-vector))
    (unless (= nrows (length vector))
      (error "Matrix and vector are incompatible for multiplication."))
    (unless (= (length into-vector) ncols)
      (error "Into-vector is of incorrect length."))
    
    (loop for col fixnum from 0 below ncols
	  do (setf (aref into-vector col)
		   (the double-float
		     (loop for row fixnum from 0 below nrows
			   sum (* (aref vector row) (aref matrix row col))
			   double-float))))
    into-vector))

;(disassemble 'matrix-times-vector)
(defun matrix-times-vector (matrix vector &optional into-vector)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dmatrix matrix))
  (declare (type dvector vector))
  (declare (type (or null dvector) into-vector))
  (type-check dmatrix  matrix)
  (type-check dvector vector)
  (type-check (or null dvector) into-vector)
  (let ((into-vector (or into-vector (make-array0 (array-dimension matrix 0)
						  :element-type 'double-float)))
	(nrows (array-dimension matrix 0))
	(ncols (array-dimension matrix 1)))
    (declare (fixnum nrows ncols))
    (declare (type dvector into-vector))
    (unless (= ncols (length vector))
      (error "Matrix ~a and vector ~a are incompatible for multiplication." 
	     (array-dimensions matrix) (length vector)))
    (unless (= (length into-vector) nrows)
      (error "Into-vector is of incorrect length."))
    
    (loop for row fixnum from 0 below nrows
	  do (setf (aref into-vector row)
		   (the double-float
		     (loop for col fixnum from 0 below ncols
			   sum (* (aref matrix row col) (aref vector col))
			   double-float))))
    into-vector))
		  
;;; ****************************  MATRIX-MULTIPLY  ****************************

;;; for backward compatibility
(defun multiply-matrices (matrix-1 matrix-2 &optional matrix-3)
  (matrix-multiply matrix-1 matrix-2 matrix-3))

;(disassemble 'matrix-multiply)
(defun matrix-multiply (matrix-1 matrix-2 &optional matrix-3)
  ;; follow-structure-forwarding because eqness is checked below.
  (declare (optimize (compilation-speed 0) (speed 3) (safety 1)))
  ;(declare (type (or dmatrix dvector) matrix-1 matrix-2))
  (declare (type (array double-float) matrix-1 matrix-2))
  (declare (type (or null dmatrix dvector) matrix-3))
  ;(type-check dmatrix matrix-1)
  ;(type-check (or dmatrix dvector) matrix-2)
  (type-check (array double-float) matrix-1 matrix-2)
  (type-check (or null dmatrix dvector) matrix-3)
  (macrolet ((matrix-dims (matrix)
	       `(case (array-rank ,matrix)
		  (2 (values (array-dimension ,matrix 0)
			     (array-dimension ,matrix 1)
			     2))
		  (1 (values (length ,matrix) 1 1))
		  (otherwise (error "the argument ~s is not a 1- or 2-dimensional array" ,matrix)))))
      
    (mv-bind (dim-1 com-dim) (matrix-dims matrix-1)
      (declare (fixnum dim-1 com-dim ))
      (mv-bind (com-dim-2 dim-2 rank-matrix-2) (matrix-dims matrix-2)
	(declare (fixnum com-dim-2 dim-2 rank-matrix-2))	       
	(unless (= com-dim com-dim-2)
	  (error "The matrices ~s and ~s are not compatible for multiplication"
		 matrix-1 matrix-2))
	;; decode the result array, creating it if necessary

	(if (null matrix-3) ; the simple case where the result-matrix is not specified
	    (if (eql rank-matrix-2 1)
		(matrix-times-vector matrix-1 matrix-2)
		(let ((matrix-3 (make-array (list dim-1 dim-2) :element-type 'double-float)))
		  (multiply_matrices dim-1 com-dim dim-2 matrix-1 matrix-2 matrix-3)
		  matrix-3))
	    ;; more complicated case
	    (mv-bind (dim-3-1 dim-3-2) (matrix-dims matrix-3)
	      (unless (and (= dim-3-1 dim-1) (= dim-3-2 dim-2))
		(error "The matrix ~s is not the right size to contain the result~@
				of multiplying ~s and ~s"
		       matrix-3 matrix-1 matrix-2))
	      (if (or (eq matrix-3 matrix-1) (eq matrix-3 matrix-2))
		  ;; complicated case where matrix-1 or matrix-2 is overwritten
		  (let ((matrix-3-tmp (make-array (array-dimensions matrix-3)
						  :element-type 'double-float)))
		    (multiply_matrices dim-1 com-dim dim-2 matrix-1 matrix-2 matrix-3-tmp)
		    (copy-array-contents matrix-3-tmp matrix-3)
		    matrix-3)
		  (progn (multiply_matrices dim-1 com-dim dim-2 matrix-1 matrix-2 matrix-3)
			 matrix-3))))))))
       
(defun multiply-matrices-transposed2 (matrix-1 matrix-2 &optional matrix-3)
  (matrix-multiply-transposed2 matrix-1 matrix-2 matrix-3))

(defun matrix-multiply-transposed2 (matrix-1 matrix-2 &optional matrix-3)
  ;; follow-structure-forwarding because eqness is checked below.
  (declare (optimize (compilation-speed 0) (speed 3) (safety 2)))
  (declare (type dmatrix matrix-1 matrix-2))
  (declare (type (or null dmatrix) matrix-3))
  (type-check dmatrix matrix-1 matrix-2)
  (type-check (or null dmatrix) matrix-3)
  (macrolet ((matrix-dims (matrix)
	       `(case (array-rank ,matrix)
		  (2 (values (array-dimension ,matrix 0)
			     (array-dimension ,matrix 1)
			     2))
		  (1 (values (length ,matrix) 1 1))
		  (otherwise (error "the argument ~s is not a 1- or 2-dimensional array" ,matrix)))))
      
    (mv-bind (dim-1 com-dim) (matrix-dims matrix-1)
      (declare (fixnum dim-1 com-dim ))
      (mv-bind (dim-2 com-dim-2) (matrix-dims matrix-2)
	(declare (fixnum com-dim-2 dim-2))	       
	(unless (= com-dim com-dim-2)
	  (error "The matrices ~s and ~s are not compatible for multiplication"
		 matrix-1 matrix-2))
	;; decode the result array, creating it if necessary

	(if (null matrix-3) ; the simple case where the result-matrix is not specified
	    (let ((matrix-3 (make-array (list dim-1 dim-2) :element-type 'double-float)))
	      (multiply_matrices_transposed2 dim-1 com-dim dim-2 matrix-1 matrix-2 matrix-3)
	      matrix-3)
	    ;; more complicated case
	    (mv-bind (dim-3-1 dim-3-2) (matrix-dims matrix-3)
	      (unless (and (= dim-3-1 dim-1) (= dim-3-2 dim-2))
		(error "The matrix ~s is not the right size to contain the result~@
				of multiplying ~s and ~s"
		       matrix-3 matrix-1 matrix-2))
	      (if (or (eq matrix-3 matrix-1) (eq matrix-3 matrix-2))
		  ;; complicated case where matrix-1 or matrix-2 is overwritten
		  (let ((matrix-3-tmp (make-array (array-dimensions matrix-3)
						  :element-type 'double-float)))
		    (multiply_matrices_transposed2 dim-1 com-dim dim-2 matrix-1 matrix-2 matrix-3-tmp)
		    (copy-array-contents matrix-3-tmp matrix-3)
		    matrix-3)
		  (progn (multiply_matrices_transposed2 dim-1 com-dim dim-2 matrix-1 matrix-2 matrix-3)
			 matrix-3))))))))

(defun multiply-matrices-transposed1 (matrix-1 matrix-2 &optional matrix-3)
  (matrix-multiply-transposed1 matrix-1 matrix-2 matrix-3))

(defun matrix-multiply-transposed1 (matrix-1 matrix-2 &optional matrix-3)
  ;; follow-structure-forwarding because eqness is checked below.
  (declare (optimize (compilation-speed 0) (speed 3) (safety 2)))
  (declare (type dmatrix matrix-1))
  (declare (type (or dmatrix dvector) matrix-2))
  (declare (type (or null dmatrix dvector) matrix-3))
  (type-check dmatrix matrix-1)
  (type-check (or dmatrix dvector) matrix-2)
  (type-check (or null dmatrix) matrix-3)
  (macrolet ((matrix-dims (matrix)
	       `(case (array-rank ,matrix)
		  (2 (values (array-dimension ,matrix 0)
			     (array-dimension ,matrix 1)
			     2))
		  (1 (values (array-dimension ,matrix 0) 1 1))
		  (otherwise (error "the argument ~s is not a 1- or 2-dimensional array" ,matrix)))))
      
    (mv-bind (com-dim dim-1) (matrix-dims matrix-1)
      (declare (fixnum dim-1 com-dim ))
      (mv-bind (com-dim-2 dim-2 rank-matrix-2) (matrix-dims matrix-2)
	(declare (fixnum com-dim-2 dim-2))	       
	(unless (= com-dim com-dim-2)
	  (error "The matrices ~s and ~s are not compatible for multiplication"
		 matrix-1 matrix-2))
	;; decode the result array, creating it if necessary

	(if (null matrix-3) ; the simple case where the result-matrix is not specified
	    (let ((matrix-3 (make-array (if (= rank-matrix-2 1) dim-1 (list dim-1 dim-2))
					:element-type 'double-float)))
	      (multiply_matrices_transposed1 dim-1 com-dim dim-2 matrix-1 matrix-2 matrix-3)
	      matrix-3)
	    ;; more complicated case
	    (mv-bind (dim-3-1 dim-3-2) (matrix-dims matrix-3)
	      (unless (and (= dim-3-1 dim-1) (= dim-3-2 dim-2))
		(error "The matrix ~s is not the right size to contain the result~@
				of multiplying ~s and ~s"
		       matrix-3 matrix-1 matrix-2))
	      (if (or (eq matrix-3 matrix-1) (eq matrix-3 matrix-2))
		  ;; complicated case where matrix-1 or matrix-2 is overwritten
		  (let ((matrix-3-tmp (make-array (array-dimensions matrix-3)
						  :element-type 'double-float)))
		    (multiply_matrices_transposed1 dim-1 com-dim dim-2 matrix-1 matrix-2 matrix-3-tmp)
		    (copy-array-contents matrix-3-tmp matrix-3)
		    matrix-3)
		  (progn (multiply_matrices_transposed1 dim-1 com-dim dim-2 matrix-1 matrix-2 matrix-3)
			 matrix-3))))))))

;;; *********************************  MATRIX OPERATORS  *********************************
    
(defmacro define-matrix-op-using-vector-op
    (function-name args vector-op
     &key
     (input-matrices args)
     (element-type 'double-float) 
     declares)
  (let ((input-matrices (loop for m in input-matrices
			      until (eq m '&optional)
			      collect m))
	(into-matrix (loop for (m . rest) on input-matrices by #'cdr
			   when (eq m '&optional)
			     return (car rest)
			   finally (return 'into-matrix)))
	(matrix-type `(simple-array ,element-type (* *))))
    `(defun ,function-name ,args
       (declare (optimize (speed 3) (safety 1)))
       (declare (type ,matrix-type ,@input-matrices))
       (declare (type (or null ,matrix-type) ,into-matrix))
       ,declares
       (type-check ,matrix-type ,@input-matrices)
       (type-check (or null ,matrix-type) ,into-matrix)
       (let ((dims (array-dimensions ,(car input-matrices ))))
	 ,@(loop for m in (cdr input-matrices)
		 collect `(unless (equal dims (array-dimensions ,m))
			    (error "Array size mismatch of ~a ~a ~a "
				   ',m dims (array-dimensions ,m))))
	 (when ,into-matrix
	   (unless (unless (equal dims (array-dimensions ,into-matrix))
		     (error "Array size mismatch of ~a ~a ~a "
			    ',into-matrix dims (array-dimensions ,into-matrix)))))
	 (let ((into-matrix (or ,into-matrix 
					;(make-matrix n)
				(make-array dims :element-type ',element-type))))
	   (,vector-op ,@(loop for m in input-matrices
				collect `(array-simple-vector ,m))
			(array-simple-vector into-matrix))
	   into-matrix)))))  

(define-matrix-op-using-vector-op matrix-add (a b &optional c) vector-add)
(define-matrix-op-using-vector-op matrix-difference (a b &optional c) vector-difference)
(define-matrix-op-using-vector-op matrix-negate (a &optional b) vector-negate)

(defmacro define-scalar-matrix-operator (name operation element-type)
  `(defun ,name (scalar matrix into-matrix)
     (declare (,element-type scalar))
     (declare (type (simple-array ,element-type (* *)) matrix into-matrix))
     (loop with dim1 fixnum = (array-dimension matrix 0)
	   with dim2 fixnum = (array-dimension matrix 1)
	   for i fixnum from 0 below dim1
	   do (loop for j fixnum from 0 below dim2
		    do (setf (aref into-matrix i j)
			     (the ,element-type (,operation scalar (aref matrix i j)))))) )
  )

(define-scalar-matrix-operator double-float-scalar-times-matrix * double-float)
(define-scalar-matrix-operator single-float-scalar-times-matrix * single-float)
(define-scalar-matrix-operator generic-scalar-times-matrix * t)

(defun scalar-times-matrix (scalar matrix &optional into-matrix)
  (let ((element-type (array-element-type matrix)))
    (unless into-matrix
      (setq into-matrix (make-array (array-dimensions matrix) :element-type element-type )))
    (case element-type
      (double-float (double-float-scalar-times-matrix (dfloat scalar) matrix into-matrix))
      (single-float (single-float-scalar-times-matrix (float scalar) matrix into-matrix))
      (otherwise (generic-scalar-times-matrix scalar matrix into-matrix)))
    into-matrix))


(defun matrix-times-scalar (matrix scalar)
  (declare (type dmatrix matrix))
  (type-check dmatrix matrix)
  (let ((into-matrix (make-array (array-dimensions matrix)
				 :element-type (array-element-type matrix))))
    (vector-times-scalar (array-simple-vector matrix) 
			 scalar
			 (array-simple-vector into-matrix))
    into-matrix))

;(disassemble 'scale-matrix)
(defun scale-matrix (matrix new-scale-factor
		     &optional into-matrix
		     (nrows (array-dimension matrix 0)) ; allows only the first nrows to be scaled
		     (ncols (array-dimension matrix 1)) ; allows only the first ncols to be scaled
		     )
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dmatrix matrix))
  (declare (double-float new-scale-factor))
  (declare (type (or null dmatrix) into-matrix))
  (declare (fixnum nrows ncols))
  (let ((into-matrix (or into-matrix (make-array (array-dimensions matrix)
						 :element-type (array-element-type matrix)))))
    (declare (type dmatrix into-matrix))
    (loop for row fixnum from 0 below nrows
	  do (loop for col fixnum from 0 below ncols
		   do (setf (aref into-matrix row col)
			    ;; Scale from local units to world units.
			    (* (aref matrix row col)
				     new-scale-factor))))
    into-matrix))

;;; *******************************  MATRIX-NORMS  *******************************

;;; This is also called the "P-Infinity Norm"
(defun matrix-max-abs-norm (matrix)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dmatrix matrix))
  (type-check dmatrix matrix)
  (vector-max-abs-norm (array-simple-vector matrix)))

;;; This is also called the "Frobenius Norm"
(defun matrix-rms-norm (matrix)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dmatrix matrix))
  (type-check dmatrix matrix)
  (vector-rms-norm (array-simple-vector matrix)))

(defun matrix-max-abs-difference (a b)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dmatrix a b))
  (type-check dmatrix a b)
  (unless (equal (array-dimensions a) (array-dimensions b))
    (error "Matrices must have same dimensions ~a ~a" a b))
  (vector-max-abs-difference (array-simple-vector a) (array-simple-vector b)))

(defun matrix-rms-difference (a b)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dmatrix a b))
  (type-check dmatrix a b)
  (unless (equal (array-dimensions a) (array-dimensions b))
    (error "Matrices must have same dimensions ~a ~a" a b))
  (vector-rms-difference (array-simple-vector a) (array-simple-vector b)))

(defparameter *matrix-equal-threshold* 0.0)

(defun matrix-equal (a b &optional (threshold *matrix-equal-threshold*))
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dmatrix a b))
  (type-check dmatrix a b)
  (unless (equal (array-dimensions a) (array-dimensions b))
    (error "Matrices must have same dimensions ~a ~a" a b))
  (vector-equal (array-simple-vector a) (array-simple-vector b) threshold))



;;; ***************************  LU-DECOMPOSITION  ****************************

(defparameter *math_lu_solve_tiny* 0.0)

(defun lu-decompose (a &optional lu ps)
  (double-float-lu-decompose a lu ps))

;;; This was previously (incorrectly) called lu-solve.
(defun lu-backsubstitute (lu ps b &optional x)
  (double-float-lu-backsubstitute lu ps b x))

;;; This needs to be fixed to return zero for singular matrices instead of causing an error.
(defun determinant (matrix)
  (declare (type dmatrix matrix))
  (type-check dmatrix matrix)
  (condition-case ()
    (multiple-value-bind (lu ps sign) (lu-decompose matrix nil nil)
      (ignore ps)
      (loop with det = sign
	    for i from (1- (array-dimension matrix 0)) downto 0
	    do (setq det (* det (aref lu i i)))
	    finally (return det)))
    ;; this isn't really the right way to do this, but ---
    ;; We assume that any error in this context is due to a singular matrix which has
    ;; a zero determinant.
    (error () 0.0)))

(declaim (special *unsupported-types-error-string*))

;;; This is generic for b being either a matrix or a vector.
;;; Using LU-decomposition, solve the matrix equation M*X=B or M*x=b 
;;; where b is either a matrix or a vector.
;;; If the matrix M has more rows than columns, then we return a least-squares solution.
(defun lu-solve (M b &optional x)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dmatrix M))
  (declare (type (or dvector dmatrix) b))
  (type-check dmatrix M)

  (if (not (= (array-dimension M 0) (array-dimension M 1)))
      (lu-least-squares-solve M b x)
      (mv-bind (lu ps) (lu-decompose M)
	(typecase b
	  (dvector (unless (= (array-dimension M 1) (length b))
		     (error "The matrices are incompatible for LU-SOLVE"))
		   (lu-backsubstitute lu ps b x))
	  (dmatrix 
	   (let* ((n1 (array-dimension M 0))
		  (n2 (array-dimension B 1))
		  (X (or x (make-array (array-dimensions B) :element-type 'double-float)))
		  (bv (make-array n1 :element-type 'double-float))
		  (xv (make-array n1 :element-type 'double-float)))
	     (declare (fixnum n1 n2))
	     (declare (type dvector bv xv))
	     (declare (type dmatrix X))
	     (unless (= (array-dimension B 0) n1)
	       (error "The matrices are incompatible for LU-SOLVE"))
	     (loop for j fixnum from 0 below n2
		   do (loop for i from 0 below n1
			    do (setf (aref bv i) (aref B i j)))
		      (lu-backsubstitute lu ps bv xv)
		      (loop for i from 0 below n1
			    do (setf (aref X i j) (aref xv i))))
	     X))
	  (otherwise (error *unsupported-types-error-string*))))))

;;; *******************************  SINGULAR VALUE DECOMPOSITION  **********************************

;;; This is generic for b being either a matrix or a vector.
;;; Using Singular Value Decomposition, solve the matrix equation M*X=B or M*x=b 
;;; where b is either a matrix or a vector.
;;; If the matrix M has more rows than columns, then we return a least-squares solution.

#+old
(defun svd-solve (M b &optional x suppress)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dmatrix M))
  (declare (type (or dvector dmatrix) b))
  (type-check dmatrix M)

  (mv-bind (u w v) (svd-decompose M)
    (when suppress
      (svd-suppress w :rel-threshold suppress))
    (typecase b
      (dvector (unless (= (array-dimension M 0) (length b))
		 (error "The matrices are incompatible for SVD-SOLVE"))
	       (svd-backsubstitute u w v b))
      (dmatrix 
       (let* ((n0 (array-dimension M 0))
	      (n1 (array-dimension M 1))
	      (n2 (array-dimension B 1))
	      (X (or x (make-array (list n1 n2) :element-type 'double-float)))
	      (bv (make-array n0 :element-type 'double-float))
	      (xv (make-array n1 :element-type 'double-float)))
	 (declare (fixnum n0 n1 n2))
	 (declare (type dvector bv xv))
	 (declare (type dmatrix X))
	 (unless (= (array-dimension B 0) n0)
	   (error "The matrices are incompatible for SVD-SOLVE"))
	 (loop for j fixnum from 0 below n2
	       do (loop for i from 0 below n0
			do (setf (aref bv i) (aref B i j)))
		  (svd-backsubstitute u w v bv xv)
		  (loop for i from 0 below n1
			do (setf (aref X i j) (aref xv i))))
	 X))
      (otherwise (error *unsupported-types-error-string*)))))

(defun svd-solve-part2 (M u w v b &optional x suppress)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dmatrix M))
  (declare (type (or dvector dmatrix) b))
  (type-check dmatrix M)
  (when suppress
    (svd-suppress w :rel-threshold suppress))
  (typecase b
    (dvector (unless (= (array-dimension M 0) (length b))
	       (error "The matrices are incompatible for SVD-SOLVE"))
	     (svd-backsubstitute u w v b))
    (dmatrix 
     (let* ((n0 (array-dimension M 0))
	    (n1 (array-dimension M 1))
	    (n2 (array-dimension B 1))
	    (X (or x (make-array (list n1 n2) :element-type 'double-float)))
	    (bv (make-array n0 :element-type 'double-float))
	    (xv (make-array n1 :element-type 'double-float)))
       (declare (fixnum n0 n1 n2))
       (declare (type dvector bv xv))
       (declare (type dmatrix X))
       (unless (= (array-dimension B 0) n0)
	 (error "The matrices are incompatible for SVD-SOLVE"))
       (loop for j fixnum from 0 below n2
	     do (loop for i from 0 below n0
		      do (setf (aref bv i) (aref B i j)))
		(svd-backsubstitute u w v bv xv)
		(loop for i from 0 below n1
		      do (setf (aref X i j) (aref xv i))))
       X))
    (otherwise (error *unsupported-types-error-string*))))

(defun svd-solve (M b &optional x suppress)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type dmatrix M))
  (declare (type (or dvector dmatrix) b))
  (type-check dmatrix M)

  (mv-bind (u w v) (svd-decompose M)
    (svd-solve-part2 M u w v b x suppress)))

;;; **********************************  LEAST SQUARES  **********************************

(defun lu-least-squares-solve (M b &optional x)
  (unless (>= (array-dimension M 0) (array-dimension M 1))
    (error "The matrix has too few rows."))
  (lu-solve (multiply-matrices-transposed1 M M) (multiply-matrices-transposed1 M b) x))

(defun Cholesky-least-squares-solve (M b &optional x)
  (unless (>= (array-dimension M 0) (array-dimension M 1))
    (error "The matrix has too few rows."))
  (cholesky-solve (cholesky-decompose (multiply-matrices-transposed1 M M))
		  (multiply-matrices-transposed1 M b)
		  x))

(defun svd-least-squares-solve (M b &optional x)
  (svd-solve M b x))

;;; This is just a convenience function to get the residuals too.
(defun least-squares-solve (c-mat d-vector &optional x)
  (let ((soln (lu-least-squares-solve c-mat d-vector x)))
    (mv-bind (mag residuals) (least-squares-residuals c-mat soln d-vector)
      (values soln mag residuals))))

(defun least-squares-residuals (c-mat soln-vector rhs-vector)
  (declare (type dmatrix c-mat))
  (declare (type (or dvector dmatrix) soln-vector rhs-vector))
  (type-check dmatrix c-mat)
  (type-check (or dvector dmatrix) soln-vector rhs-vector)
  (if (typep soln-vector 'dmatrix)
      (loop for i from 0 below (array-dimension soln-vector 1)
	    collect (mv-list (least-squares-residuals c-mat 
						      (matrix-column soln-vector i)
						      (matrix-column rhs-vector i))))
      (let* ((rhs (matrix-times-vector c-mat soln-vector))
	     (residuals (vector-difference rhs rhs-vector))
	     (mag (vector-euclidean-length residuals)))
	(values mag residuals))))

(defun least-squares-rms-error (c-mat soln rhs)
  (declare (type dmatrix c-mat))
  (declare (type (or dvector dmatrix) soln rhs))
  (type-check dmatrix c-mat)
  (type-check (or dvector dmatrix) soln rhs)
  (if (typep soln 'dmatrix)
      (loop for i from 0 below (array-dimension soln 1)
	    collect (least-squares-rms-error c-mat
					     (matrix-column soln i)
					     (matrix-column rhs i)))
      (vector-euclidean-length (vector-difference (matrix-times-vector c-mat soln) 
						  rhs))))


;;; *************************  CHOLESKY DECOMPOSITION  *************************

;;; A must be a positive-definite symmetric matrix.
;;; Returns matrix C such that C x C-transpose = A.
;;; Inner loop count is n^3/6
;;; This requires that the upper triangle of A be specified -- lower triangle is ignored.

(defun cholesky-decompose (a &optional overwrite)
  (declare (optimize (speed 3) (safety 0)))
  ;; bug in cmucl-2005-02-x86-linux
  ;(declare (type square-dmatrix a))
  (declare (type dmatrix a))
  (type-check square-dmatrix a)
  (unless overwrite (setq a (copy-matrix a)))
  (let* ((n (array-dimension a 0))
	 (p (make-array n :element-type 'double-float :initial-element 0.0)))
    (declare (fixnum n))
    (declare (type (simple-array double-float (*)) p))
    (loop for i fixnum from 0 below n
	  do (loop for j fixnum from i below n
		   for sum double-float = (- (aref a i j)
					     (the double-float
						 (loop for k fixnum from (1- i) downto 0
						       sum (* (aref a i k) (aref a j k)) double-float)))
		   when (= i j)
		     do (when (<= sum 0.0) (error "cholesky-decompose: matrix is not positive definite."))
			(setf (aref p i) (the double-float (sqrt sum)))
		   else do (setf (aref a j i) (/ sum (aref p i)))))

    ;;; Copy p into diagonal and zero upper right triangle.
    (loop for i fixnum from 0 below n
	  do (setf (aref a i i) (aref p i))
	     (loop for j fixnum from (1+ i) below n
		   do (setf (aref a i j) 0.0)))
	     
    a))

;;; This requires that the lower triangle of A be specified -- upper triangle is ignored.
(defun cholesky-decompose-lower (a)
  (cholesky-decompose (transpose-matrix a) t))

;;; Finds solution x to (S . St) . x = (S . (St . x) = b
;;; S must be the result of cholesky-decompose.
;;; x must be a vector
(defun cholesky-solve (S b &optional x)
  (declare (type dmatrix S))
  (declare (type (simple-array double-float (*)) b))
  (declare (type (or null (simple-array double-float (*))) x))
  (unless x (setq x (make-array (length b) :element-type 'double-float)))
  ;; Solve S . y = b, storing y in x
  (loop with n fixnum = (length b)
	for i fixnum from 0 below n
	do (setf (aref x i)
		 (/ (- (aref b i)
		       (loop for k fixnum from (1- i) downto 0
			     sum (* (aref S i k) (aref x k)) double-float )
			     )
		    (aref S i i))))
  ;;; Solve St . x = y
  (loop with n fixnum = (length b)
	for i fixnum from (1- n) downto 0
	do (setf (aref x i)
		 (/ (- (aref x i)
		       (loop for k fixnum from (1+ i) below n
			     sum (* (aref S k i) (aref x k)) double-float )
			     )
		    (aref S i i))))
  x)

#|
(defun test-cholesky-decompose (A)
  (let* ((G (cholesky-decompose A))
	 (GxGt (multiply-matrices-transposed2 G G))
	 (diff (max-abs-difference GxGt a))
	 )
    (format t "test-cholesky-decompose ~a~%" diff)
    G))

(defun test-cholesky-solve (A b)
  (let* ((S (test-cholesky-decompose A))
	 (x (cholesky-solve S b))
	 (b2 (multiply-matrices A x))
	 (diff (max-abs-difference b b2))
	 )
    (format t "test-cholesky-solve ~a~%" diff)
    x))

|#

;;; **************************  OPTIMIZATIONS ON SMALL MATRICES  ****************************

;;; Lots of random optimizations for performance.
;;; Some should probably go away.
;;; Others should move to elsewhere

;;; MULTIPLY-SMALL-MATRICES should be roughly equivalent to MULTIPLY-MATRICES
;;; but avoids a foreign function call.  The performance crossover is at about
;;; 7x7 matrices, beyond which MULTIPLY-MATRICES is faster.
;;; Unlike MULTIPLY-MATRICES, the matrices must be 2-dimensional arrays.

;(disassemble 'multiply-small-matrices)
(defun multiply-small-matrices (mat-a mat-b &optional mat-c)
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 1) (debug 1)(ext:inhibit-warnings 1)))
  (declare (optimize (speed 3) (safety 1) (debug 1) #+cmu (ext:inhibit-warnings 1)))
  (declare (type dmatrix mat-a mat-b)
	   (type (or null dmatrix) mat-c))
  (type-check dmatrix mat-a mat-b)
  (type-check (or null dmatrix) mat-c)
  (let* ((n1 (array-dimension mat-a 0))
	 (n2 (array-dimension mat-a 1))
	 (n2b (array-dimension mat-b 0))
	 (n3 (array-dimension mat-b 1)))
    (declare (fixnum n1 n2 n2b n3))
    (unless (= n2 n2b) (error "multiply-matrices-2: matrices not compatible ~a ~a" mat-a mat-b))
    (unless mat-c (setq mat-c (make-array (list n1 n3) :element-type 'double-float)))
    (when (eq mat-c mat-b) (setq mat-b (copy-matrix mat-c)))
    (let ((a (lcl::array-simple-vector mat-a))
	  (b (lcl::array-simple-vector mat-b))
	  (c (lcl::array-simple-vector mat-c)))
      (declare (type (simple-array double-float *) a b c))
      (when (eq a c)
	;; perhaps there should be a small cache of tmp vectors to avoid calling make-array
	(setq a (make-array (* n1 n2) :element-type 'double-float))
	(loop with nelems fixnum = (* n1 n2)
	      for i fixnum from 0 below nelems do (setf (aref a i) (aref c i))))
      (when (eq b c)
	;; perhaps there should be a small cache of tmp vectors to avoid calling make-array
	(setq b (make-array (* n2 n3) :element-type 'double-float))
	(loop with nelems fixnum = (* n2 n3)
	      for i fixnum from 0 below nelems do (setf (aref b i) (aref c i))))
      (multiply-small-matrices-loops a b c n1 n2 n3))
    mat-c))

;(disassemble 'multiply-small-matrices-loops)
(defun multiply-small-matrices-loops (a b c n1 n2 n3)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (simple-array double-float *) a b c))
  (declare (fixnum n1 n2 n3))
  (loop for i fixnum from 0 below n1
	for n2*i fixnum from 0 by n2
	for n3*i fixnum from 0 by n3
	do (loop for j fixnum from 0 below n3
		 for n3*i+j fixnum from n3*i
		 do (multiply-small-matrices-inner-loop a b n2 n2*i j n3 c n3*i+j))))

;(disassemble 'multiply-small-matrices-inner-loop)
;;#+cmu ; This seems to produce the best code
(defun multiply-small-matrices-inner-loop (a b n2 n2*i j n3 c n3*i+j)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (simple-array double-float (*))  a b))
  (declare (fixnum n2 n2*i j n3))
  (declare (type (simple-array double-float (*)) c)) (declare (fixnum n3*i+j))
  (loop with sum double-float = 0.0
	for k fixnum from 0 below n2
	for n2*i+k fixnum from n2*i
	for n3*k+j fixnum from j by n3
	do (incf sum (* (aref a n2*i+k) (aref b n3*k+j)))
	finally 
     (setf (aref c n3*i+j) sum)))

#+never ; #+allegro ; actually this provides no significant improvement
(defun multiply-small-matrices-inner-loop (a b n2 n2*i j n3 c n3*i+j)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (simple-array double-float (*))  a b))
  (declare (fixnum n2 n2*i j n3))
  (declare (type (simple-array double-float (*)) c)) (declare (fixnum n3*i+j))
  (let ((sum 0.0)
	(n3*k+j j)
	(n2*i+k n2*i))
    (declare (fixnum n3*k+j n2*i+k))
    (declare (double-float sum))
    (dotimes (k n2)
      (declare (fixnum k))
      (incf sum (* (aref a n2*i+k) (aref b n3*k+j)))
      (incf n3*k+j n3)
      (incf n2*i+k))
    (setf (aref c n3*i+j) sum)
    nil))

(defun matrix-times-diagonal-matrix (a d &optional c)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (simple-array double-float (* *))  a))
  (if (= (array-rank d) 1)
      (loop with n fixnum = (array-dimension a 0)
	    with m fixnum = (array-dimension a 1)
	    with c of-type dmatrix = (or c (make-matrix n m))
	    with d of-type (simple-array double-float (* )) = d
	    for j fixnum from 0 below m
	    for djj double-float = (aref d j)
	    do (loop for i fixnum from 0 below n
		     do (setf (aref c i j) (* (aref a i j) djj)))
	    finally (return c))

      (loop with n fixnum = (array-dimension a 0)
	    with m fixnum = (array-dimension a 1)
	    with c of-type dmatrix = (or c (make-matrix n m))
	    with d of-type (simple-array double-float (* *)) = d
	    for j fixnum from 0 below m
	    for djj double-float = (aref d j j)
	    do (loop for i fixnum from 0 below n
		     do (setf (aref c i j) (* (aref a i j) djj)))
	    finally (return c))))

;;; d can be either a vector of the diagonal elements or a square matrix.
(defun matrix-transposed-times-diagonal-matrix (a d &optional c)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (simple-array double-float (* *))  a))
  (if (= (array-rank d) 2)
      (let ((d d))
	(declare (type (simple-array double-float (* *))  d))
	(loop with n fixnum = (array-dimension a 1)
	      with m fixnum = (array-dimension a 0)
	      with c of-type dmatrix = (or c (make-matrix n m))
	      for j fixnum from 0 below m
	      for djj double-float = (aref d j j)
	      do (loop for i fixnum from 0 below n
		       do (setf (aref c i j) (* (aref a j i) djj)))
	      finally (return c)))
      (let ((d d))
	(declare (type (simple-array double-float (*))  d))
	(loop with n fixnum = (array-dimension a 1)
	      with m fixnum = (array-dimension a 0)
	      with c of-type dmatrix = (or c (make-matrix n m))
	      for j fixnum from 0 below m
	      for djj double-float = (aref d j)
	      do (loop for i fixnum from 0 below n
		       do (setf (aref c i j) (* (aref a j i) djj)))
	      finally (return c)))))




(defun 3x3-matrix-times-vector (m v &optional into-v)
  (declare (type (dmatrix (3 3)) m ))
  (declare (type dvector v))
  (declare (type (or null dvector) into-v))
  (type-check (dmatrix (3 3)) m)
  (type-check dvector v)
  (type-check (or null dvector) into-v)
  (unless into-v
    (setq into-v (make-array 3 :element-type 'double-float)))
  (let ((x (aref v 0))
	(y (aref v 1))
	(z (aref v 2))
	)
    (declare (double-float x y z ))
    (math::inline-matrix-times-vector m
				      (x y z)
				      ((aref into-v 0)
				       (aref into-v 1)
				       (aref into-v 2))))
  into-v)

;(disassemble '3x3-matrix-times-matrix)
; C must not be equal to A.
(defun 3x3-matrix-times-matrix (a b &optional c)
  (declare (optimize (speed 3)(safety 0)))
  (declare (type (dmatrix (3 3)) a b ))
  (declare (type (or null (dmatrix (3 3))) c))
  (type-check (dmatrix (3 3)) a b)
  (type-check (or null (dmatrix (3 3))) c)
  (assert (not (eq A C)))
  (unless c (setq c (make-array '(3 3) :element-type 'double-float)))
  (macrolet ((compute-col (j)
	       `(let ((x (aref b 0 ,j))
		      (y (aref b 1 ,j))
		      (z (aref b 2 ,j))
		      )
		  (declare (double-float x y z ))
		  (math::inline-matrix-times-vector A
						    (x y z)
						    ((aref c 0 ,j)
						     (aref c 1 ,j)
						     (aref c 2 ,j))))))
    (compute-col 0)
    (compute-col 1)
    (compute-col 2)
    c))


#|
(let ((m1 (submatrix (make-omega-phi-kappa-rotation-matrix .1 .2 .3) 3 3 0 0))
      (m2 (submatrix (make-omega-phi-kappa-rotation-matrix .2 .1 -.3) 3 3 0 0)))
  (rms-difference (3x3-matrix-times-matrix m1 m2)
		  (g* m1 m2)))

|#



(defun 4x4-matrix-times-vector (m v &optional into-v)
  (declare (type (dmatrix (4 4)) m ))
  (declare (type dvector v))
  (declare (type (or null dvector) into-v))
  (type-check (dmatrix (4 4)) m)
  (type-check dvector v)
  (type-check (or null dvector) into-v)
  (unless into-v
    (setq into-v (make-array 3 :element-type 'double-float)))
  (let ((x (aref v 0))
	(y (aref v 1))
	(z (if (>= (length v) 3) (aref v 2) 0.0))
	)
    (declare (double-float x y z))
    (math::inline-matrix-times-vector m
				      (x y z 1.0)
				      ((aref into-v 0)
				       (aref into-v 1)
				       (aref into-v 2))))
  into-v)

(defun 2x4-matrix-times-vector (m v &optional into-v)
  (declare (type (dmatrix (2 4)) m ))
  (declare (type dvector v))
  (declare (type (or null dvector) into-v))
  (type-check (dmatrix (2 4)) m)
  (type-check dvector v)
  (type-check (or null dvector) into-v)
  (unless into-v
    (setq into-v (make-array 2 :element-type 'double-float)))
  (let ((x (aref v 0))
	(y (aref v 1))
	(z (if (>= (length v) 3) (aref v 2) 0.0))
	)
    (declare (double-float x y z))
    (math::inline-matrix-times-vector m
				      (x y z 1.0)
				      ((aref into-v 0)
				       (aref into-v 1))))
  into-v)

(defparameter *opengl-depth-normalize* nil)

(defun 4x4-project-vector (m v &optional into-v)
  (declare (optimize (speed 3) (safety 2)))
  (declare (type (dmatrix (4 4)) m ))
  (declare (type dvector v))
  (declare (type (or null dvector) into-v))
  (type-check (dmatrix (4 4)) m)
  (type-check dvector v)
  (type-check (or null dvector) into-v)
  (unless into-v
    (setq into-v (make-array 3 :element-type 'double-float)))
  (let ((x (aref v 0))
	(y (aref v 1))
	(z (aref v 2))
	;; Stupid Allegro compiler:  Unintialized vars declared as double-floats are
	;; initialized by unboxing NIL.  Need these inits to prevent bus errors.
	(xp 1.0d0) (yp 1.0d0) (zp 1.0d0) (s 1.0d0) 
	)
    (declare (double-float x y z xp yp zp s))
    (math::inline-matrix-times-vector m
				      (x y z 1.0)
				      (xp yp zp s))
    (setq s (/ 1.0 s))
    (setf (aref into-v 0) (* xp s)
	  (aref into-v 1) (* yp s)
	  (aref into-v 2) (if *opengl-depth-normalize* (* zp s) zp))
    )
  into-v)

;;; this used to be called scale-transform-matrix
(defun scale-upper-3x3-matrix (transform-matrix new-scale-factor )
  (declare (optimize (speed 3)(safety 1)))
  (declare (double-float new-scale-factor))
  (declare (type dmatrix transform-matrix))
  (type-check dmatrix transform-matrix)
   (loop for row from 0 to 2
	do (loop for col from 0 to 2
		 do (setf (aref transform-matrix row col)
			  ;; Scale from local units to world units.
			  (* (aref transform-matrix row col)
				   new-scale-factor))))
  transform-matrix)
;(disassemble 'scale-upper-3x3-matrix)

;(disassemble 'upper-3x3-matrix)
(defun upper-3x3-matrix (transform-matrix)
  (declare (optimize (speed 3)(safety 1)))
  (declare (type dmatrix transform-matrix))
  (let* ((m3 (make-array '(3 3) :element-type (array-element-type transform-matrix)
			 :initial-element 0.0d0)))
    (declare (type dmatrix m3))
    (loop for row fixnum from 0 below 3
	  do (loop for col fixnum from 0 below 3
		   do (setf (aref m3 row col) (aref transform-matrix row col))))
    m3))

(defun invert-3x3-matrix  (mat &optional inverse)
  (declare (optimize (speed 3)(safety 1)))
  (declare (type (dmatrix (3 3)) mat))
  (declare (type (or null (dmatrix (3 3))) inverse))
  (type-check (dmatrix (3 3)) mat)
  (type-check (or null (dmatrix (3 3))) inverse)
  (unless inverse
    (setq inverse (make-array '(3 3) :element-type 'double-float
			      :initial-element 0.0d0)))
  (if (= 0 (invert_3x3_matrix mat inverse) )
      nil
      inverse))


;;; mat is a homogeneous 2x3 matrix.  There is an implicit 3rd row: (0 0 1).
;;; Its inverse is also a 2x3 matrix.
;;; Should rename to invert-3x2-homogeneous-matrix
(defun fast-invert-2x3 (mat)
  (declare (type dmatrix mat))
  ;(declare (type (simple-array double-float (2 3)) mat))
  (declare (optimize (speed 3)(safety 1)))
  (let* ((a00 (aref mat 0 0))
	 (a01 (aref mat 0 1))
	 (a02 (aref mat 0 2))
	 (a10 (aref mat 1 0))
	 (a11 (aref mat 1 1))
	 (a12 (aref mat 1 2))
	 (inv (make-array '(2 3) :element-type 'double-float))
	 (det (- (* a00 a11) (* a01 a10)))
	 (1/det (/ 1.0 det)) )
    (declare (type (simple-array double-float (2 3)) inv))
    (declare (double-float a00 a01 a02 a10 a11 a12 det 1/det))
    (setf (aref inv 0 0) (* a11 1/det)
	  (aref inv 1 0) (- (* a10 1/det))
	  (aref inv 0 1) (- (* a01 1/det))
	  (aref inv 1 1) (* a00 1/det)
	  (aref inv 0 2) (* (- (* a01 a12) (* a11 a02)) 1/det)
	  (aref inv 1 2) (* (- (* a10 a02) (* a00 a12)) 1/det))
    inv))
;(disassemble 'fast-invert-3x2)


#|

(defvar *multiply-4x4-matrices-tmp-mat* (make-array 16 :element-type 'double-float))


(defmacro inline-4x4-matrix-times-vector (m input-vars output-vars)
  `(setf . ,(loop for 4xrow from 0 by 4
                  for out-var in output-vars
                  when out-var
                  collect out-var
                  and collect 
                  `(+ . ,(loop for input-var in input-vars
                               for index from 4xrow
                               collect `(* (aref ,m ,index) ,input-var))))))

(defun multiply-4x4-matrices (mat-a mat-b &optional mat-c)
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 0) (debug 1)(ext:inhibit-warnings 1)))
  (declare (optimize (speed 3) (safety 0) (debug 1) #+cmu (ext:inhibit-warnings 1)))
  (declare (type (simple-array double-float (4 4)) mat-a mat-b)
	   (type (or null (simple-array double-float (4 4))) mat-c))
  (unless mat-c (setq mat-c (make-array '(4 4) :element-type 'double-float)))
  (let ((a (lcl::array-simple-vector mat-a))
	(b (lcl::array-simple-vector mat-b))
	(c (lcl::array-simple-vector mat-c)))
    (declare (type (simple-array double-float (*)) a b c))
    (when (eq b c)
      (setq b *multiply-4x4-matrices-tmp-mat*)
      (loop for i fixnum from 0 below 16 do (setf (aref b i) (aref c i))))
    (when (eq a c)
      (setq a *multiply-4x4-matrices-tmp-mat*)
      (loop for i fixnum from 0 below 16 do (setf (aref a i) (aref c i))))

    (loop for i fixnum from 0 below 4
	    do (let ((b0i (aref b i))
		     (b1i (aref b (+ i 4)))
		     (b2i (aref b (+ i 8)))
		     (b3i (aref b (+ i 12))))
		 (inline-4x4-matrix-times-vector a (b0i b1i b2i b3i)
						 ((aref c i)
						  (aref c (+ i 4))
						  (aref c (+ i 8))
						  (aref c (+ i 12))))))))
  mat-c)
					  


(config::proclaim-optimizations :fast-no-tail-calls)
(disassemble 'multiply-4x4-matrices)
(disassemble 'multiply-matrices-2)

(setq m1 (make-omega-phi-kappa-rotation-matrix .1 .2 .3))
(setq m2 (make-omega-phi-kappa-rotation-matrix .01 .02 .03))
(setq m3 (make-array '(4 4) :element-type 'double-float))

(list (multiply-matrices m1 m2)
      (multiply-4x4-matrices m1 m2))

(list (multiply-matrices m1 m2)
      (progn (copy-array-contents m1 m3) (multiply-matrices m3 m2 m3))
      (progn (copy-array-contents m1 m3) (multiply-small-matrices m3 m2 m3)))

(list (multiply-matrices m1 m2)
      (progn (copy-array-contents m2 m3) (multiply-matrices m1 m3 m3))
      (progn (copy-array-contents m2 m3) (multiply-small-matrices m1 m3 m3)))

(list (multiply-matrices m1 m2)
      (multiply-4x4-matrices m1 m2))

(list (multiply-matrices m1 m2)
      (multiply-small-matrices m1 m2))

(defun time-multiply-matrices (n)
  (loop repeat n do (multiply-matrices m1 m2 m3))) ; this involves a foreign-function call

(defun time-multiply-4x4-matrices (n)
  (loop repeat n do (multiply-4x4-matrices m1 m2 m3)))

(defun time-multiply-matrices (n)
  (loop repeat n do (multiply-small-matrices m1 m2 m3)))

(defun time-multiply-matrices (n &optional (n1 4) (n2 n1) (n3 n2))
  (let ((m1 (make-array (list n1 n2) :element-type 'double-float))
	(m2 (make-array (list n2 n3) :element-type 'double-float))
	(m3 (make-array (list n1 n3) :element-type 'double-float)))
    (loop repeat n do (multiply-matrices m1 m2 m3))))

(defun time-multiply-small-matrices (n &optional (n1 4) (n2 n1) (n3 n2))
  (let ((m1 (make-array (list n1 n2) :element-type 'double-float))
	(m2 (make-array (list n2 n3) :element-type 'double-float))
	(m3 (make-array (list n1 n3) :element-type 'double-float)))
    (loop repeat n do (multiply-small-matrices m1 m2 m3))))

;;; the call to make-array (and subsequent GC) is significant.
(time (time-multiply-matrices 1000000))     2.66 secs 
(time (time-multiply-small-matrices 1000000))   1.14 secs 
(time (time-multiply-4x4-matrices 1000000)) 0.88 secs

(let ((m3 nil)) (time (time-multiply-matrices 1000000)))     ; 7.26 secs
(let ((m3 nil)) (time (time-multiply-small-matrices 1000000)))   ; 5.73 secs 
(let ((m3 nil)) (time (time-multiply-4x4-matrices 1000000))) ; 1.24 secs

(time (time-multiply-matrices 1000000 5))   ; 3.19
(time (time-multiply-small-matrices 1000000 5)) ; 1.98

(time (time-multiply-matrices 1000000 6))   ; 4.45
(time (time-multiply-small-matrices 1000000 6)) ; 3.33

(time (time-multiply-matrices 1000000 8))   ; 7.5
(time (time-multiply-small-matrices 1000000 8)) ; 7.72

|#




;(disassemble 'nr-tridiag)
(defun nr-tridiag (a b c r)
  (declare (optimize (speed 3)))
  (declare (type dvector a b c r))
  (let* ((n (length b))
	 (u (make-dvector n))
	 (gam (make-dvector n))		; temp vector
	 (bet (aref b 0))
	 )
    (declare (type dvector u gam))
    (declare (fixnum n))
    (declare (double-float bet))

    (when (zerop (aref b 0)) (error "b[0]=0"))

    (setf (aref u 0)(/ (aref r 0) bet))
    
    (loop for j fixnum from 1 below n
	  do (setf (aref gam j) (/ (aref c (1- j)) bet))
	     (setf bet (- (aref b j) (* (aref a j) (aref gam j))))
	     (when (zerop bet) (error "beta = 0"))
	     (setf (aref u j) 
		   (/ (- (aref r j) (* (aref a j) (aref u (1- j)))) bet)))

    (loop for j fixnum from (1- n) downto 1
	  do (decf (aref u (1- j)) (* (aref gam j) (aref u j))))
    
    u))
	     
