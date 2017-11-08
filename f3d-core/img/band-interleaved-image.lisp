(in-package :img)

#|

Perhaps this file should be split and merged into array-image.lisp and paged-image.lisp

vector-image.lisp provides an alternative based on creating
red, green, blue scalar-images mapped into the band-interleaved RGB8 and RGBA8 images.

getline/putline works with 2d arrays of dimension XDIMx3 for RGB8 images and XDIMx4 for RGBA8
images.  The viref/viset functions are implemented to pass the pixel components in 1d arrays of length
3 for RGB8 images and 4 for RGBA8 images.  This approach then generalizes to vector-images of
arbitrary numbers of bands.

|#

;;; ********************  BAND-INTERLEAVED-ARRAY-IMAGE  ********************

;;; WE HAVE A CLASS HIERARCHY PROBLEM HERE
;;; band-interleaved-array-image SHOULD NOT be considered to be a SCALAR-IMAGE
;;; Wrap a band-interleaved image with a VECTOR-IMAGE to solve this problem.
;;; Never pass a band-interleaved image to a method that specializes on SCALAR-IMAGE.
;;; The problem is that scalar-image methods expect to do IREF and getline with a single buffer.
;;; That will fail for a band-interleaved-image.

(defstruct-class band-interleaved-array-image (array-image)
  ((nbands :initarg :nbands :accessor nbands)))

(defstruct-class array-image-rgb (band-interleaved-array-image) () (:default-initargs :nbands 3))

(defstruct-class array-image-rgba (band-interleaved-array-image) () (:default-initargs :nbands 4))

(eval-when (:compile-toplevel :load-toplevel)

(defvar config::*default-band-interleaved-image-element-types-and-nbands* '((img::rgb8 1) (img::rgba8 1)))

) ; end eval-when


(eval-when (:compile-toplevel #-COMPILE-FOR-RUNTIME-ONLY-DISKSAVE :load-toplevel)

(defun tree-subst-atom (old new sexpr)
  (if (atom sexpr)
      (if (eql sexpr old)
	  new
	  sexpr)
      (cons (tree-subst-atom old new (car sexpr))
	    (tree-subst-atom old new (cdr sexpr)))))
#|
(tree-subst-atom 'i 1 '(setf (aref pvect i) 
			(element-type->fixnum (aref array (the fixnum (+ index i))))))
|#

#+old
(defmethod generate-image-ref-fns ((image-class (eql :band-interleaved-array-image))
				   &key element-type nbands)
  (mv-bind (iref-fn iset-fn diref-fn diset-fn) 
      (get-image-iref-fn-symbols image-class (element-type-to-name element-type) nbands)
    (let* ((int-pvect (get-preallocated-integer-iref-array nbands))
	   (float-pvect (get-preallocated-float-iref-array nbands))
	   (mult-index (case element-type (RGB8 3) (RGBA8 4) (otherwise 1)))
	   (nbands (case element-type (RGB8 3) (RGBA8 4) (otherwise nbands)))
	   (element-type (case element-type
			   ((RGB8 RGBA8) '(unsigned-byte 8))
			   (otherwise element-type))))
      (declare (type int-pixel-vector int-pvect)
	       (type dfloat-pixel-vector float-pvect))
      `(with-pixel-ref-macros (,element-type)
	 (macrolet ((referencing-components (&body body)
		      `(let ((array (array-image-array image))
			     (index (compute-pixel-index)))
			 (declare (type (simple-array ,',element-type (*)) array)
				  (fixnum index))
			 ,',@(when mult-index `((setq index (the fixnum (* ,mult-index index)))))
			 ,@body)))
      
	   (defun ,iref-fn (image x y pvect)
	     ,@(iref-preamble)
	     (referencing-components 
	      (let ((pvect
		     (cond ((null pvect) ,int-pvect)
			   ((eq pvect t) 
			    (make-array ,nbands :element-type 'int-pixel-vector-element-type))
			   (t pvect))))
		(declare (type int-pixel-vector pvect))
		,@(loop for i from 0 below nbands
			collect `(setf (aref pvect ,i) 
				       (element-type->fixnum (aref array (the fixnum (+ index ,i))))))
		pvect)))

	   (defun ,iset-fn (image x y pvect)
	     (declare (type int-pixel-vector pvect))
	     ,@(iref-preamble)
	     (referencing-components
	      ,@(loop for i from 0 below nbands
		      collect `(setf (aref array (+ index ,i)) 
				     (fixnum->element-type (aref pvect ,i))))
	      pvect))

	   (defun ,diref-fn (image x y pvect)
	     ,@(iref-preamble)
	     (referencing-components
	      (let ((pvect 
		     (cond ((null pvect) ,float-pvect)
			   ((eq pvect t) 
			    (make-array ,nbands :element-type 'dfloat-pixel-vector-element-type))
			   (t pvect))))
		(declare (type dfloat-pixel-vector pvect))
		,@(loop for i from 0 below nbands
			collect `(setf (aref pvect ,i) 
				       (element-type->dfloat (aref array (the fixnum (+ index ,i))))))
		pvect)))

	   (defun ,diset-fn (image x y pvect)
	     (declare (type dfloat-pixel-vector pvect))
	     ,@(iref-preamble)
	     (referencing-components
	      ,@(loop for i from 0 below nbands
		      collect `(setf (aref array (+ index ,i))
				     (dfloat->element-type (aref pvect ,i))))
	      pvect))
	   )))))


(defmethod generate-image-ref-fns ((image-class (eql :band-interleaved-array-image))
				   &key element-type nbands)
  (mv-bind (iref-fn iset-fn diref-fn diset-fn) 
      (get-image-iref-fn-symbols image-class (element-type-to-name element-type) nbands)
    (let* ((mult-index 1))
      (case element-type
	(RGB8  (setq nbands 3 mult-index 3 element-type '(unsigned-byte 8)))
	(RGBA8 (setq nbands 4 mult-index 4 element-type '(unsigned-byte 8))))

      `(with-pixel-ref-macros (,element-type)
	 (macrolet ((loop-over-components (form)
		      `(progn ,@(loop for i from 0 below ,nbands
				      collect (tree-subst-atom 'i i form))))
		    (referencing-components (assignment-form)
		      `(let ((array (array-image-array image))
			     (index (compute-pixel-index)))
			 (declare (type (simple-array ,',element-type (*)) array)
				  (fixnum index))
			 ,',@(unless (eql mult-index 1) `((setq index (the fixnum (* ,mult-index index)))))
			 (loop-over-components ,assignment-form)
			 pvect)))
      
	   (defun ,iref-fn (image x y pvect)
	     ,@(iref-preamble)
	     (with-int-pixel-vector (,nbands)
	       (referencing-components 
		(setf (aref pvect i) 
		      (element-type->fixnum (aref array (the fixnum (+ index i))))))))

	   (defun ,iset-fn (image x y pvect)
	     (declare (type int-pixel-vector pvect))
	     ,@(iref-preamble)
	     (referencing-components
	      (setf (aref array (+ index i)) 
		    (fixnum->element-type (aref pvect i)))))

	   (defun ,diref-fn (image x y pvect)
	     ,@(iref-preamble)
	     (with-dfloat-pixel-vector (,nbands)
	       (referencing-components
		(setf (aref pvect i) 
		      (element-type->dfloat (aref array (the fixnum (+ index i))))))))

	   (defun ,diset-fn (image x y pvect)
	     (declare (type dfloat-pixel-vector pvect))
	     ,@(iref-preamble)
	     (referencing-components
	      (setf (aref array (+ index i))
		    (dfloat->element-type (aref pvect i)))))
	   )))))

) ; end eval-when

#|
(disassemble 'band-interleaved-array-image-rgb8-1-iref)
|#

;(generate-image-ref-fns :band-interleaved-array-image :element-type 'rgb8 :nbands 1)
(defmacro generate-default-band-interleaved-array-image-iref-fns ()
  `(progn . ,(loop for (element-type nbands)
		   in config::*default-band-interleaved-image-element-types-and-nbands*
		   collect (generate-image-ref-fns :band-interleaved-array-image 
						   :element-type element-type :nbands 1))))

;;; Force the default IREF/ISET functions to be compiled at compile-time.
;;; This is needed in order to distribute code in a "disksave" without a compiler.
(generate-default-band-interleaved-array-image-iref-fns)

(defmethod set-image-struct-slots :after ((image band-interleaved-array-image))
  (let ((element-type (image-element-type image)))
    (multiple-value-bind (viref-fn viset-fn vdiref-fn vdiset-fn)
	(get-image-ref-fns :band-interleaved-array-image 
			   :element-type element-type :nbands (nbands image))
      (setf (image-viref-fn image) viref-fn
	    (image-viset-fn image) viset-fn
	    (image-vdiref-fn image) vdiref-fn
	    (image-vdiset-fn image) vdiset-fn))))

#|
(setq *ref-fns1*
      (mv-list (get-band-interleaved-array-image-vref-fns '(unsigned-byte 8) 3)))

(disassemble (nth 0 *ref-fns1*))

|#


;;; ********************  BAND-INTERLEAVED-PAGED-IMAGE  ********************

(defstruct-class band-interleaved-paged-image (paged-image)
  ((nbands :initarg :nbands :accessor nbands)))

(defstruct-class paged-image-rgb (band-interleaved-paged-image)
  ()
  (:default-initargs :nbands 3))

(defstruct-class paged-image-rgba (band-interleaved-paged-image)
  ()
  (:default-initargs :nbands 4))

(eval-when (:compile-toplevel #-COMPILE-FOR-RUNTIME-ONLY-DISKSAVE :load-toplevel)

(defmethod generate-image-ref-fns ((image-class (eql :band-interleaved-paged-image))
				   &key element-type tile-offset-bits nbands largest-possible)
  (mv-bind (iref-fn iset-fn diref-fn diset-fn) 
      (get-image-iref-fn-symbols image-class element-type tile-offset-bits nbands)
    (let* ((tile-offset-mask (1- (ash 1 tile-offset-bits)))
	   (page-number-bytespec
	    `(byte ,(- #.(1+ (log2 most-positive-fixnum)) tile-offset-bits)
		   ,tile-offset-bits) )
	   (int-pvect (get-preallocated-integer-iref-array nbands))
	   (float-pvect (get-preallocated-float-iref-array nbands))
	   (mult-index (case element-type (RGB8 3) (RGBA8 4) (otherwise 1)))
	   (nbands (case element-type (RGB8 3) (RGBA8 4) (otherwise nbands)))
	   (element-type (case element-type
			   ((RGB8 RGBA8) '(unsigned-byte 8))
			   (otherwise element-type))))
      (declare (type int-pixel-vector int-pvect)
	       (type dfloat-pixel-vector float-pvect))
      `(with-pixel-ref-macros (,element-type)
	 (macrolet 
	     ((referencing-components (&body body)
		`(let* ((index (compute-pixel-index))
			;; ldb used here instead of ash to prevent sign bit from
			;; propagating right for very large block numbers.  With
			;; paged-image64 this could to flushed, with significant speed-up
			(page-number ,',(if largest-possible
					    `(ldb ,page-number-bytespec offset)
					    `(ash index ,(- tile-offset-bits))))
			(array (aref (the simple-vector (paged-image-block-map image)) page-number))
			)
		   (declare (fixnum index page-number))
		   (when (eq array 0)
		     (paged-image-iref-fault image page-number )
		     (setq array (aref (the simple-vector (paged-image-block-map image))
				       page-number)))
		   (setq index ,',@(if mult-index 
				       `((the fixnum (* ,mult-index (logand index ,tile-offset-mask))))
				       `((logand index ,tile-offset-mask))))
		   (let ((array array)) (declare (type (simple-array ,',element-type (*)) array))
			,@body))))
      
	   (defun ,iref-fn (image x y pvect)
	     ,@(iref-preamble)
	     (referencing-components 
	      (let ((pvect
		     (cond ((null pvect) ,int-pvect)
			   ((eq pvect t) 
			    (make-array ,nbands :element-type 'int-pixel-vector-element-type))
			   (t pvect))))
		(declare (type int-pixel-vector pvect))
		,@(loop for i from 0 below nbands
			collect `(setf (aref pvect ,i) 
				       (element-type->fixnum (aref array (the fixnum (+ index ,i))))))
		pvect)))

	   (defun ,iset-fn (image x y pvect)
	     (declare (type int-pixel-vector pvect))
	     ,@(iref-preamble)
	     (referencing-components
	      ,@(loop for i from 0 below nbands
		      collect `(setf (aref array (+ index ,i)) 
				     (fixnum->element-type (aref pvect ,i))))
	      pvect))

	   (defun ,diref-fn (image x y pvect)
	     ,@(iref-preamble)
	     (referencing-components
	      (let ((pvect 
		     (cond ((null pvect) ,float-pvect)
			   ((eq pvect t) 
			    (make-array ,nbands :element-type 'dfloat-pixel-vector-element-type))
			   (t pvect))))
		(declare (type dfloat-pixel-vector pvect))
		,@(loop for i from 0 below nbands
			collect `(setf (aref pvect ,i) 
				       (element-type->dfloat (aref array (the fixnum (+ index ,i))))))
		pvect)))

	   (defun ,diset-fn (image x y pvect)
	     (declare (type dfloat-pixel-vector pvect))
	     ,@(iref-preamble)
	     (referencing-components
	      ,@(loop for i from 0 below nbands
		      collect `(setf (aref array (+ index ,i))
				     (dfloat->element-type (aref pvect ,i))))
	      pvect))
	   )))))


;;#+experimental ; broken - cannot get the assignment-form to work correctly
(defmethod generate-image-ref-fns ((image-class (eql :band-interleaved-paged-image))
				   &key element-type tile-offset-bits nbands largest-possible)
  (mv-bind (iref-fn iset-fn diref-fn diset-fn) 
      (get-image-iref-fn-symbols image-class (element-type-to-name element-type)
				 tile-offset-bits nbands)
    (let* ((tile-offset-mask (1- (ash 1 tile-offset-bits)))
	   (page-number-bytespec
	    `(byte ,(- #.(1+ (log2 most-positive-fixnum)) tile-offset-bits)
		   ,tile-offset-bits) )
	   (mult-index 1))
      (case element-type
	(RGB8  (setq nbands 3 mult-index 3 element-type '(unsigned-byte 8)))
	(RGBA8 (setq nbands 4 mult-index 4 element-type '(unsigned-byte 8))))

      `(with-pixel-ref-macros (,element-type)
	 (macrolet 
	     ((loop-over-components (form)
		`(progn ,@(loop for i from 0 below ,nbands
				collect (tree-subst-atom 'i i form))))
	      (referencing-components (assignment-form)
		`(let* ((index (compute-pixel-index))
			;; ldb used here instead of ash to prevent sign bit from
			;; propagating right for very large block numbers.  With
			;; paged-image64 this could to flushed, with significant speed-up
			(page-number ,',(if largest-possible
					    `(ldb ,page-number-bytespec index)
					    `(ash index ,(- tile-offset-bits))))
			(array (aref (the simple-vector (paged-image-block-map image)) page-number))
			)
		   (declare (fixnum index page-number))
		   (when (eql array 0)
		     (paged-image-iref-fault image page-number )
		     (setq array (aref (the simple-vector (paged-image-block-map image))
				       page-number)))
		   (setq index (logand index ,',tile-offset-mask))
		   ,',@(unless (eql mult-index 1) `((setq index (the fixnum (* ,mult-index index)))))
		   (let ((array array)) 
		     (declare (type (simple-array ,',element-type (*)) array))
		     (loop-over-components ,assignment-form)
		     pvect))))
      
	   (defun ,iref-fn (image x y pvect)
	     ,@(iref-preamble)
	     (with-int-pixel-vector (,nbands)
		 (referencing-components 
		  (setf (aref pvect i) 
			(element-type->fixnum (aref array (the fixnum (+ index i))))))))

	   (defun ,iset-fn (image x y pvect)
	     (declare (type int-pixel-vector pvect))
	     ,@(iref-preamble)
	     (referencing-components
	      (setf (aref array (+ index i)) 
		    (fixnum->element-type (aref pvect i)))))

	   (defun ,diref-fn (image x y pvect)
	     ,@(iref-preamble)
	     (with-dfloat-pixel-vector (,nbands)
		 (referencing-components
		  (setf (aref pvect i) 
			(element-type->dfloat (aref array (the fixnum (+ index i))))))))

	   (defun ,diset-fn (image x y pvect)
	     (declare (type dfloat-pixel-vector pvect))
	     ,@(iref-preamble)
	     (referencing-components
	      (setf (aref array (+ index i))
		    (dfloat->element-type (aref pvect i)))))

	   )))))

  ) ;; end eval-when

#|
(defun foo0 (x)
  #+cmu (declare (ext::optimize-interface (speed 3)(safety 0) (debug 0)))
  (declare (optimize (speed 3)(safety 0) (debug 0)))
  (declare (double-float x))
  (let ((v (make-array 2 :element-type 'double-float)))
    (setf (aref v 0) x)
    v))
(disassemble 'foo0)

(defun foo1 (x)
  #+cmu (declare (ext::optimize-interface (speed 3)(safety 0) (debug 0)))
  (declare (optimize (speed 3)(safety 0) (debug 0)))
  (declare (fixnum x))
  (let ((v (make-array 2 :element-type 'fixnum)))
    (setf (aref v 0) x)
    v))
(disassemble 'foo1)
(disassemble 'sys::allocate-vector)

(compile-top-level-form 
 (generate-image-ref-fns :band-interleaved-paged-image :element-type 'rgb8 :tile-offset-bits 20 :nbands 1))
(disassemble 'band-interleaved-paged-image-rgb8-20-1-iref)
(disassemble 'band-interleaved-paged-image-rgb8-20-1-iset)
(disassemble 'band-interleaved-paged-image-rgb8-20-1-diref)
(disassemble 'band-interleaved-paged-image-rgb8-20-1-diset)
(compile-top-level-form 
 (generate-image-ref-fns :band-interleaved-paged-image :element-type 'rgba8 :tile-offset-bits 20 :nbands 1))
(disassemble 'band-interleaved-paged-image-rgba8-20-1-iref)
(disassemble 'band-interleaved-paged-image-rgba8-20-1-iset)
(disassemble 'band-interleaved-paged-image-rgba8-20-1-diref)
(disassemble 'band-interleaved-paged-image-rgba8-20-1-diset)

|#

(defmacro generate-default-band-inverleaved-paged-image-iref-fns ()
  `(progn 
     . ,(loop for tile-offset-bits in config::*paged-image-allowed-tile-offset-bits*
	      append (loop for (element-type nbands) 
			   in config::*default-band-interleaved-image-element-types-and-nbands*
			   collect (generate-image-ref-fns :band-interleaved-paged-image 
					    :element-type element-type
					    :tile-offset-bits tile-offset-bits
					    :nbands 1)))))

;;; Force the default IREF/ISET functions to be compiled at compile-time.
;;; This is needed in order to distribute code in a "disksave" without a compiler.
(generate-default-band-inverleaved-paged-image-iref-fns)

(defmethod set-image-struct-slots :after ((image band-interleaved-paged-image))
  (let ((element-type (image-element-type image))
	(tile-offset-bits (tile-offset-bits image))
	(read-only (eql 1 (paged_image_read_only (image-id image)))))
    (multiple-value-bind (viref-fn viset-fn vdiref-fn vdiset-fn)
	(get-image-ref-fns :band-interleaved-paged-image
			   :element-type element-type :tile-offset-bits tile-offset-bits
			   :nbands (nbands image))
      (when read-only
	(setf viset-fn #'paged-image-write-write-lock-error
	      vdiset-fn #'paged-image-write-write-lock-error))
      (setf (image-viref-fn image) viref-fn
	    (image-viset-fn image) viset-fn
	    (image-vdiref-fn image) vdiref-fn
	    (image-vdiset-fn image) vdiset-fn))))

