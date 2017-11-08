(in-package :img)

#|
Sun Apr  6 2003

IMPLEMENTATION OF VECTOR-IMAGES

Classes:

   VECTOR-IMAGE
   COLOR-IMAGE



In the case of vector-image.lisp, either band-interleaved of band-seperate vector-images are
possible.  In the case of band-interleaved vector-images, the actual band-interleaved image is a
slot of the vector image, and each component (band) is represented by an image mapped into the
band-interleaved image.  The element-size of the each image band is the element-size of the
band-interleaved image divided by the number of bands.  The elements of x-map y-map tables are
multiplied by the number of bands in the vector-image, and the elements of x-map are offset by the
band number.

In the case of band-seperate vector-images, each band-image is a totally seperate image (required to
have the same dimensions), and the iref/iset and getline/putline access is the same as for
band-interleaved vector-images.

For both types of vector-image, the iref/iset and getline/putline access is to the band images
rather than to the band-interleaved image.


INCOMPLETE:

   only band-interleaved images RGB8 and RGBA8 are supported

FIXME:

   keyword arg :IMAGE-type vs :IMAGE-class  is inconsistent

   All unary and binary image operators must accept the :INTO-IMAGE  keyword argument

|#


;;; ******************************  VIREF VDIREF  ******************************

(deftype int-pixel-vector-element-type () '(signed-byte 32))

(deftype dfloat-pixel-vector-element-type () 'double-float)

(deftype int-pixel-vector () '(simple-array (signed-byte 32) (*)))

(deftype dfloat-pixel-vector () '(simple-array double-float (*)))


;;; These guys are quite efficient and have a good programming interface.

(defmacro viref (image x y &optional (pvect nil pvect-supplied-p))
  `(the int-pixel-vector
    (let* ((%image% ,image))
      (declare (type image %image%))
      (prog1 ;; avoid multiple-value-return
	  (funcall (the function (image-viref-fn %image%))
		   %image% ,x ,y  .,(if pvect-supplied-p `(,pvect) '(t)))))))


(defmacro vdiref (image x y &optional (pvect nil pvect-supplied-p))
  `(the dfloat-pixel-vector
    (let* ((%image% ,image))
      (declare (type image %image%))
      (prog1 ;; avoid multiple-value-return
	  (funcall (the function (image-vdiref-fn %image%))
		   %image% ,x ,y  .,(if pvect-supplied-p `(,pvect) '(t)))))))

;;; The bind-int-pixel-vector-elements macros might be "better" done using communication
;;; from bind-int-pixel-vector-elements down to the viref macro to tell viref that we are
;;; inside bind-int-pixel-vector-elements.
;;; 
;;; The problem with this will occur with constructs like this:
;;; (bind-dfloat-pixel-vector-elements (r g b) (add-rgb (viref cimg1 x y) (viref cimg2 x y))
;;; ...)  In this construct, the virefs must allocate a new vector for the
;;; return values.  If we wanted to get really fancy, the viref macros could
;;; lexically allocate from a pool of pixel vectors.
;;;
;;; For now we only optimize forms like (bind-dfloat-pixel-vector-elements (r g b) (viref cimg x y) ...)

(defmacro bind-int-pixel-vector-elements (vars viref-form &body body)
  (let ((arr (gensym))
        (viref-form (if (eq (car viref-form) 'viref)
			(destructuring-bind (img x y &optional pvect) (cdr viref-form)
                         (if pvect
                             viref-form
                             `(viref ,img ,x ,y nil) ; tell viref not to allocate a new vector
                             ))
			viref-form)))
    `(let ((,arr ,viref-form))
      (declare (type math::coordinate-ivector ,arr))
      (let  ,(loop for var in vars
                   for i from 0
                   collect `(,var (aref ,arr ,i)))
        (declare (fixnum . ,vars))
        . ,body))))

(defmacro bind-dfloat-pixel-vector-elements (vars vdiref-form &body body)
  (let ((arr (gensym))
        (vdiref-form (if (eq (car vdiref-form) 'vdiref)
			 (destructuring-bind (img x y &optional pvect) (cdr vdiref-form)
			   (if pvect
			       vdiref-form
			       `(vdiref ,img ,x ,y nil)	; tell vdiref not to allocate a new vector
			       ))
			 vdiref-form)))
    `(let ((,arr ,vdiref-form))
      (declare (type math::coordinate-vector ,arr))
      (let  ,(loop for var in vars
                   for i from 0
                   collect `(,var (aref ,arr ,i)))
        (declare (type dfloat-pixel-vector-element-type . ,vars))
        . ,body))))



(eval-when (eval load compile)

  (defparameter *preallocated-float-iref-array-alist* nil)
  
  (defun get-preallocated-float-iref-array (nelems)
    (let ((entry (assoc nelems *preallocated-float-iref-array-alist*)))
      (if entry
	  (cadr entry)
	  (let ((arr (make-array nelems :element-type 'dfloat-pixel-vector-element-type)))
	    (push (list nelems arr) *preallocated-float-iref-array-alist*)
	    arr))))

  (defparameter *preallocated-integer-iref-array-alist* nil)
  
  (defun get-preallocated-integer-iref-array (nelems)
    (let ((entry (assoc nelems *preallocated-integer-iref-array-alist*)))
      (if entry
	  (cadr entry)
	  (let ((arr (make-array nelems :element-type 'int-pixel-vector-element-type)))
	    (push (list nelems arr) *preallocated-integer-iref-array-alist*)
	    arr))))
  

  (defmacro with-int-pixel-vector ((nbands) &body body)
    `(let ((pvect
	    (cond ((null pvect) ,(get-preallocated-integer-iref-array nbands))
		  ((eq pvect t) 
		   (make-array ,nbands :element-type 'int-pixel-vector-element-type))
		  (t pvect))))
       (declare (type int-pixel-vector pvect))
       ,@body))

  (defmacro with-dfloat-pixel-vector ((nbands) &body body)
    `(let ((pvect 
	    (cond ((null pvect) ,(get-preallocated-float-iref-array nbands))
		  ((eq pvect t) 
		   (make-array ,nbands :element-type 'dfloat-pixel-vector-element-type))
		  (t pvect))))
       (declare (type dfloat-pixel-vector pvect))
       ,@body))

  )

(defmacro int-pixel-vector (&rest components)
  `(math::civ .,components))

(defmacro dfloat-pixel-vector (&rest components)
  `(math::cv .,components))

(eval-when (eval load compile)

;;; FIXME:  What are to allowed constructs here?
  
(defun viset-optimize-pvect-form-p (pvect-form)
  (setq pvect-form (macroexpand-1 pvect-form)) ; allow 1 level of macro redefinition 
  (and (consp pvect-form) (memq (car pvect-form) '(int-pixel-vector math::civ))))

(defun vdiset-optimize-pvect-form-p (pvect-form)
  (setq pvect-form (macroexpand-1 pvect-form)) ; allow 1 level of macro redefinition
  (and (consp pvect-form) (memq (car pvect-form) '(dfloat-pixel-vector math::cv))))

) ; end eval-when

(defmacro viset (image x y pvect-form)
  (if (viset-optimize-pvect-form-p pvect-form)
      (let ((forms (cdr pvect-form)))
	`(let ((%image% ,image)
	       (%array% ',(get-preallocated-integer-iref-array (length forms))))
	  (declare (type math::coordinate-ivector %array%))
	  (declare (type image %image%))
	  (setf .,(loop for form in forms
			for i from 0
			collect `(aref %array% ,i)
			collect form))
	  (the int-pixel-vector
	      (funcall (the function (image-viset-fn %image%))
		       %image% ,x ,y %array%))))
      
      `(let ((%image% ,image))
	(declare (type image %image%))
	(the int-pixel-vector
	    (funcall (the function (image-viset-fn %image%))
		     %image% ,x ,y ,pvect-form)))))

(defmacro vdiset (image x y pvect-form)
  (if (vdiset-optimize-pvect-form-p pvect-form)
      (let ((forms (cdr pvect-form)))
	`(let ((%image% ,image)
	       (%array% ',(get-preallocated-float-iref-array (length forms))))
	  (declare (type math::coordinate-vector %array%))
	  (declare (type image %image%))
	  (setf .,(loop for form in forms
			for i from 0
			collect `(aref %array% ,i)
			collect form))
	  (the dfloat-pixel-vector
	      (funcall (the function (image-vdiset-fn %image%))
		       %image% ,x ,y %array%))))
      
      `(let ((%image% ,image))
	(declare (type image %image%))
	(the dfloat-pixel-vector
	    (funcall (the function (image-vdiset-fn %image%))
		     %image% ,x ,y ,pvect-form)))))

(defsetf viref viset)
(defsetf vdiref vdiset)

;;; *******************************  MV-IREF MV-DIREF  *******************************

#+to-be-expunged
(progn 
;;; I am not sure how useful these guys are.  Have fun trying them out.
;;; They are inherently slow.

(defun mv-iref (image x y)
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 0)))
  (declare (optimize (speed 3) (safety 0)))
  (let ((v (viref image x y nil)))              ; tell viref not to allocate a new vector
    (declare (type math::coordinate-ivector v))
    (case (length v)
      (1 (aref v 0))
      (2 (values (aref v 0) (aref v 1)))
      (3 (values (aref v 0) (aref v 1) (aref v 2)))
      (4 (values (aref v 0) (aref v 1) (aref v 2) (aref v 3)))
      (otherwise (values-list (loop for i fixnum from 0 below (length v)
				    collect (aref v i)))))))

(defun mv-iset-internal (image x y values)
  (let* ((n (length values))
	 (vector (make-array n :element-type 'math::coordinate-ivector-element-type)))
    (declare (fixnum n))
    (declare (type math::coordinate-ivector vector))
    (loop for i fixnum from 0 below n
	  for value in values
	  do (setf (aref vector i) value))
    (the int-pixel-vector
	(funcall (the function (image-iset-fn image))
		 image x y vector))))

(defmacro mv-iset (image x y form)
  `(mv-iset-internal ,image ,x ,y (multiple-value-list ,form)))

(defsetf mv-iref mv-iset)

(defun mv-diref (image x y)
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 0)))
  (declare (optimize (speed 3) (safety 0)))
  (let ((v (vdiref image x y nil)))              ; tell vdiref not to allocate a new vector
    (declare (type math::coordinate-vector v))
    (case (length v)
      (1 (aref v 0))
      (2 (values (aref v 0) (aref v 1)))
      (3 (values (aref v 0) (aref v 1) (aref v 2)))
      (4 (values (aref v 0) (aref v 1) (aref v 2) (aref v 3)))
      (otherwise (values-list (loop for i fixnum from 0 below (length v)
				    collect (aref v i)))))))

(defun mv-diset-internal (image x y values)
  (let* ((n (length values))
	 (vector (make-array n :element-type 'math::coordinate-vector-element-type)))
    (declare (fixnum n))
    (declare (type math::coordinate-vector vector))
    (loop for i fixnum from 0 below n
	  for value in values
	  do (setf (aref vector i) (dfloat value)))
    (the dfloat-pixel-vector
	(funcall (the function (image-diset-fn image))
		 image x y vector))))

(defmacro mv-diset (image x y form)
  `(mv-diset-internal ,image ,x ,y (multiple-value-list ,form)))

(defsetf mv-diref mv-diset)

) ; #+to-be-expunged progn
    
;;; ************************************  VECTOR-IMAGE CLASS  ************************************

(defstruct-class vector-image (image)
  ((component-images :initform nil  :initarg :component-images  :accessor component-images)
   (component-names :initform nil
		    :initarg :component-names :accessor component-names)
   (band-interleaved-image :initform nil :initarg :band-interleaved-image
			   :accessor band-interleaved-image)
   ))

(defmethod iref* ((image scalar-image) x y)
  (iref image x y))

(defmethod iref* ((image vector-image) x y)
  (viref image x y))

(defmethod diref* ((image scalar-image) x y)
  (diref image x y))

(defmethod diref* ((image vector-image) x y)
  (vdiref image x y))

(defmethod initialize-instance :after ((vector-image vector-image) &rest init-plist
				       &key ;;element-type x-dim y-dim
				       band-element-type band-image-class n-bands &allow-other-keys)
  (ignore init-plist)
  (with-class-slots vector-image
	(x-dim y-dim element-type band-interleaved-image component-images) vector-image
    (cond (band-interleaved-image
	   (unless (and band-element-type n-bands)
	     (unless band-image-class
	       (typecase band-interleaved-image
		 (array-image (setq band-image-class 'array-image))
		 (paged-image (setq band-image-class 'paged-image))))

	     (case (image-element-type vector-image)
	       (rgb8 (setq band-element-type '(unsigned-byte 8) n-bands 3))
	       (rgba8 (setq band-element-type '(unsigned-byte 8) n-bands 4))
	       (otherwise (setq band-element-type (image-element-type band-interleaved-image)
				n-bands (image-samples-per-pixel band-interleaved-image)))
	       #+never
	       (otherwise (error "initialize-instance band-interleaved-vector-image unknown element-type ~a"
				 (image-element-type vector-image)))
	       ))
	   (setf (image-id vector-image)
		 (image-id band-interleaved-image))
	   (setf (component-images vector-image)
		 (loop for i from 0 below n-bands
		       collect (make-component vector-image n-bands i band-element-type band-image-class))))

	  ((and (not band-interleaved-image) component-images)
	   (with-class-slots vector-image (element-type x-dim y-dim) vector-image
	     (let ((component (first component-images)))
	       (setf element-type (loop for img in component-images
					collect (image-element-type img))
		     x-dim (image-x-dim component)
		     y-dim (image-y-dim component)))))

	  ((and (not band-interleaved-image) (null component-images))
	   (setq component-images
		 (loop repeat n-bands
		       collect (make-image (list x-dim y-dim) :element-type element-type))))
	  )
    
    (set-iref-fns vector-image)
    (assure-vector-image-iref-arrays n-bands)
    ))

(defmethod clone-image ((image vector-image) &optional (element-type (image-element-type image))
			(image-class (class-of image)))
  (let* ((band-interleaved-image (band-interleaved-image image))
	 (clone (make-image (image-dimensions image)
			    :element-type element-type
			    :image-type image-class
			    :band-interleaved-image
			    (and band-interleaved-image
				 (clone-image band-interleaved-image))
			    :component-images
			    (loop for comp in (component-images image)
				  collect (clone-image comp))
			    :component-names (component-names image))))
    clone))


(defmethod image-for-display ((image vector-image))
  (error "image-for-display of band-sequential vector-image is not currently supported"))
  

(defun make-vector-image-from-band-interleaved-image
    (band-interleaved-image &key (image-class 'vector-image))
  (with-class-slots image (x-dim y-dim element-type) band-interleaved-image
    (make-instance image-class
		   :x-dim x-dim :y-dim y-dim
		   :element-type element-type
		   :band-interleaved-image band-interleaved-image)))

(defun make-band-interleaved-vector-image (dimensions &rest init-args &key
					   element-type sample-per-pixel
					   (vector-image-class 'vector-image) &allow-other-keys)
  (declare (ignorable element-type sample-per-pixel))
  (make-vector-image-from-band-interleaved-image (apply #'make-image dimensions init-args)
						 :image-class vector-image-class))

(defun make-color-image-from-band-interleaved-image (band-interleaved-image)
  (make-vector-image-from-band-interleaved-image band-interleaved-image :image-class 'color-image))


(defun copy-to-stationary-vector (vector)
  (let ((new-vector (make-stationary-vector (length vector) :element-type (array-element-type vector))))
    (copy-array-contents vector new-vector)
    new-vector))

(defmethod band-interleaved-image-hack-map ((image array-image) map mult offset band-interleaved-image )
  (declare (ignore band-interleaved-image))
  (declare (type image-map-type map))
  (loop for i fixnum from 0 below (length map)
	do (setf (aref map i) (+ offset (* mult (aref map i) )))))

(defmethod band-interleaved-image-hack-map ((image paged-image) map mult offset band-interleaved-image)
  (declare (type image-map-type map))
  (loop for i fixnum from 0 below (length map)
	do (multiple-value-bind (tile-number tile-index)
	       (decompose-paged-image-pixel-index band-interleaved-image (aref map i) )
	     (declare (fixnum tile-number tile-index))
	     (setf (aref map i)
		   (compose-paged-image-pixel-index
		    image tile-number (+ offset (* mult tile-index ))))))
  )

(defmethod band-interleaved-image-hack-maps ((packed-image image) band-image nbands band-num)
  (with-class-slots image (x-map y-map) band-image
    (band-interleaved-image-hack-map band-image x-map nbands band-num packed-image)
    (band-interleaved-image-hack-map band-image y-map nbands 0 packed-image)
    (set-image-maps band-image x-map y-map)
    ))

#|
RGB8 (RGBA8) images:

   C++ band-interleaved-image elements are structs of 3(4) bytes.  x-map and y-map contain pixel indices
   C++ component image elements are 8bit bytes.  x-map and y-map contain sample indices:  

            sample-index = pixel-index*nbands + band-num

Other band-interleaved images:

   C++ band-interleaved-image elements are sub-pixel samples.  x-map and y-map contain sub-pixel indices
   C++ component image elements are sub-pixel samples.  x-map and y-map contain sub-pixel indices offset by band-num

            sample-index = sub-pixel-index + band-num



|#

(defmethod make-component ((image vector-image) nbands band-num element-type band-image-class)
  (declare (optimize debug))
  (unless (memq (image-element-type image) '(RGB8 RGBA8))
    ;; NBANDS here is really the number of sub-samples per map-element.  RGB8(RGBA8) images have 3(4)
    ;; sub-samples per pixel.  x-map and y-map of the band interleaved image index in units of the full
    ;; RGB8(RGBA8) pixel, rether than the 8bit subsamples.  For other band-interleaved images, x-map
    ;; and y-map index the actual subsamples and rather than the pixel.
    (setq nbands 1))
;;  (break)
  (with-class-slots vector-image (band-interleaved-image) image
    (if (null band-interleaved-image)
	(make-image (image-dimensions image) :element-type element-type)
	
        (progn
          (print (length (image-x-map band-interleaved-image)))
          (print (length (image-y-map band-interleaved-image)))

	(let* ((new-image (clone-image band-interleaved-image element-type band-image-class)))
          (print new-image)
          ;; ===>
	  (set-image-maps new-image
			  (copy-to-stationary-vector (image-x-map band-interleaved-image))
			  (copy-to-stationary-vector (image-y-map band-interleaved-image)))
          ;; <===
          (print new-image)
	  
	  (when (typep new-image 'paged-image)
	    (let* ((bits (+ (tile-offset-bits band-interleaved-image)
			    (log2 nbands))))
	      (when (eql (image-id new-image) (image-id band-interleaved-image))
		(error "make-component vector-image is munging tile_offset_bit"))
	      (set_paged_image_tile_offset_bits (image-id new-image) bits)
	      (setf (tile-offset-bits new-image) bits)))

	  (set-image-struct-slots new-image)
	  (band-interleaved-image-hack-maps band-interleaved-image new-image nbands band-num)
	
	  (setf (image-samples-per-pixel new-image) 1) ; needed for getline/putline 
	  new-image
	  )))))

(defmethod init-new ((vector-image vector-image) &key &allow-other-keys)
  )

(defmethod unmake :around ((vector-image vector-image) &rest args &key &allow-other-keys)
  (with-class-slots vector-image (component-images band-interleaved-image) vector-image
    (loop for image in component-images
	  do (apply 'unmake-image image args))
    (when band-interleaved-image
      (apply 'unmake-image band-interleaved-image args))
    (call-next-method)))

;;; There is some component with no data
(defmethod no-data-p ((vector-image vector-image))
  (with-class-slots vector-image (component-images) vector-image
    (loop for image in component-images
	  thereis (or (null image) (no-data-p image)))))

;;; should no-data mean 
;;; (1) there is some component with no data, or
;;; (2) every component has no data?


(defmethod similar-image ((vector-image vector-image) &rest args)
  (with-class-slot-values vector-image
	(element-type component-images component-names  band-interleaved-image)
      vector-image
    (if band-interleaved-image
	(make-image (image-dimensions vector-image) :image-type (type-of vector-image)
		    :element-type element-type
		    :band-interleaved-image (similar-image band-interleaved-image)
		    :component-names component-names)
	(make-image (image-dimensions vector-image) :image-type (type-of vector-image)
		    :element-type element-type
		    :component-images (loop for component in component-images
					    collect (apply 'similar-image component args))
		    :component-names component-names))))



(defmethod unary-operator ((vector-image vector-image) opr into-image &rest args)
  (unless into-image (setq into-image (similar-image vector-image)))
  (loop for img in (component-images vector-image)
	for into-img in (component-images into-image)
	collect (apply opr img :into-image into-img args))
  into-image)

(defmethod binary-operator ((image1 vector-image) (image2 vector-image) opr into-image &rest args)
  (unless into-image (setq into-image (similar-image image1)))
  (loop for img1 in (component-images image1)
	for img2 in (component-images image2)
	for into-img in (component-images into-image)
	collect (apply opr img1 img2 :into-image into-img args))
  into-image)

(defmacro define-vector-image-unary-opr
    (function-name method-arglist function-arglist)
  `(defmethod ,function-name ((vector-image vector-image) &key into-image . ,method-arglist)
    (unary-operator vector-image ',function-name into-image . ,function-arglist)  ))

(defmacro define-vector-image-binary-opr
    (function-name method-arglist function-arglist)
  `(defmethod ,function-name ((image1 vector-image) (image2 vector-image) &key into-image . ,method-arglist)
    (binary-operator self ',function-name into-image . ,function-arglist)  ))

#|
(fmakunbound 'copy-image-xx)

(defmethod copy-image-xx (image &key into-image)
  (copy-image image into-image))

(define-vector-image-unary-opr copy-image-xx () ())
|#


(defmethod copy-image ((vector-image vector-image) &optional into-image)
  (unless into-image (setq into-image (similar-image vector-image)))
  (loop for component in (vector-image-component-images vector-image)
	for into-component in (vector-image-component-images into-image)
	collect (copy-image component into-component))
  into-image)
    

#+unfinished
(defmethod window-self ((vector-image vector-image)
			superior from-x from-y new-x-dim new-y-dim
			&optional new-x-map new-y-map)
  (ignore from-x from-y new-x-dim new-y-dim new-x-map new-y-map)
  (push vector-image (image-prop superior :inferiors))
  (setf (image-window-of vector-image) (or (image-window-of superior) superior))
  vector-image)

#+unfinished
(defmethod image-bitbltable ((vector-image vector-image))
  (with-class-slots vector-image (component-images) vector-image
    (loop for image in component-images
	  always (image-bitbltable image))))

;;; Need to rework the semantics of element-type and element-size of vector-images.


(defmethod coerce-element-type
    ((vector-image vector-image)
     new-element-type
     &rest args &key (bitbltable t) doit-damnit auto-rescale into-image)
  (ignore doit-damnit auto-rescale into-image  bitbltable)
  (with-class-slots vector-image (component-images) vector-image
    (loop for image in component-images
	  for new-image = (apply 'coerce-image-element-type
				 image new-element-type :into-image nil args) 
	  collect new-image into new-components
	  count (neq image new-image) into diffs
	  finally
       (return (if (zerop diffs)
		   vector-image
		   (make-image (image-dimensions vector-image) :image-type (type-of vector-image)
			       :element-type new-element-type
			       :component-images new-components))))))

#| -- this is wrong w.r.t. the element-size of the result
(defmethod change-element-size
    ((vector-image vector-image)
     new-size &rest args &key (bitbltable t) doit-damnit  auto-rescale into-image)
  (ignore doit-damnit auto-rescale into-image new-size bitbltable)
  (with-class-slots vector-image (component-images element-type) vector-image
    (loop for image in component-images
	  for new-image = (apply 'change-element-size image
				 :into-image nil :new-size new-size args) 
	  collect new-image into new-components
	  count (neq image new-image) into diffs
	  finally
       (return (if (zerop diffs) vector-image
		   (make-image (image-dimensions vector-image) :image-type (type-of vector-image)
			       :element-size ????? 
			       :component-images new-components))))))
|#

(defparameter *allow-2d-buffers-all-vector-images* t)

;;; eliminate this when image-getline band-interleaved-array-image is fixed.
(defun temporary-band-interleaved-image-getline-putline-ok-test (image)
  (and image
       (or *allow-2d-buffers-all-vector-images*
	   (typep image 'ARRAY-IMAGE-RGB)
	   (typep image 'ARRAY-IMAGE-RGBA)
	   (typep image 'PAGED-IMAGE-RGB)
	   (typep image 'PAGED-IMAGE-RGBA)

	   )))

;;; buffer must be a list of buffers.
;;; FIXME:  extend so buffer can be a 2d array
(defmethod image-getline
	   ((vector-image vector-image) buffer x y &optional n (to-start 0) (dx 1) (band 0))
  (with-class-slot-values vector-image (component-images band-interleaved-image) vector-image
    (cond ((and (typep buffer 'array)
		band-interleaved-image
		(= (array-rank buffer) 2)
		;;(temporary-band-interleaved-image-getline-putline-ok-test band-interleaved-image)
		)
	   (unless n (setq n (array-dimension buffer 0)))
	   (image-getline band-interleaved-image buffer x y n to-start dx band))
	  
	  ((consp buffer)
	   (unless n
	     (setq n (loop for buf in buffer
			   when buf minimize (array-dimension buf 0))))
	   (loop for image in component-images
		 for buf in buffer
		 when buf 
		   do (image-getline image buf x y n to-start dx band)
		 finally (return buffer)))
	  
	  (t (error "Buffer arg must be a list of arrays: ~a" buffer)))))

;;; buffer must be a list of buffers.
;;; FIXME:  extend so buffer can be a 2d array
(defmethod image-putline
	   ((vector-image vector-image) buffer x y &optional n (from-start 0) (band 0))
  ;; This wants to see an array:
  (with-class-slot-values vector-image (component-images band-interleaved-image) vector-image
    (cond ((and (typep buffer 'array)
		band-interleaved-image
		(= (array-rank buffer) 2)
		;;(temporary-band-interleaved-image-getline-putline-ok-test band-interleaved-image)
		)
	   (unless n (setq n (if (listp buffer) (length buffer) (array-dimension buffer 0))))
	   (image-putline band-interleaved-image buffer x y n from-start band))
	  
	  ((consp buffer)
	   (unless n
	     ;; but this wants to see a list:
	     (setq n (loop for buf in buffer
			   when buf minimize (array-dimension buf 0))))
	   (loop for image in component-images
		 for buf in buffer
		 when buf 
		   do (image-putline image buf x y n from-start band)
		 finally (return buffer)))
	  
	  (t (error "Buffer arg must be a list of arrays: ~a" buffer)))))

(defmethod make-scan-line-buffers ((image vector-image) &optional length element-type)
  (loop for component in (component-images image)
	collect (make-scan-line-buffer component length element-type)))

(defmethod make-dfloat-scan-line-buffers ((image vector-image) &optional length)
  (loop for component in (component-images image)
	collect (make-dfloat-scan-line-buffer component length)))

(defmethod make-integer-scan-line-buffers ((image vector-image) &optional length)
  (loop for component in (component-images image)
	collect (make-integer-scan-line-buffer component length)))

#|
(make-foreign-array '(4 4) :element-type 'double-float)
|#

(defvar *require-foreign-scan-line-buffers* nil)

#|
FIXME:  If 2d-scan-line-buffers are ever used with lazy-images, they must be 
        stationary in memory, otherwise the page-fault-callback from the lazy-image
        could trigger a GC and cause the 2d-scan-line-buffer to move in memory.
|#

(defmethod lazy-image-p ((image image))
  nil)

#+never ;; add this method when lazy-images with Lisp callbacks are supported.
(defmethod lazy-image-p ((image lazy-image))
  t)

(defmethod lazy-image-p ((image vector-image))
  (or (lazy-image-p (band-interleaved-image image))
      (loop for component in (component-images image)
	    thereis (lazy-image-p component))))

(defmethod make-scan-line-2d-buffer ((image vector-image) element-type &optional length)
  (when *require-foreign-scan-line-buffers*
    (error "2d scan-line-buffers are not yet supported with lazy-images"))
  (let ((dims (list (or length (image-x-dim (car (component-images image))))
		    (length (component-images image)))))
    (if (lazy-image-p image)
	;; WARNING: returning a DISPLACED-ARRAY, not a SIMPLE-ARRAY.
	(qffi::make-foreign-array dims :element-type element-type)
	(make-array dims :element-type element-type))))

(defmethod make-dfloat-scan-line-2d-buffer ((image vector-image) &optional length)
  (make-scan-line-2d-buffer image 'dfloat-pixel-vector-element-type length))

(defmethod make-integer-scan-line-2d-buffer ((image vector-image) &optional length)
  (make-scan-line-2d-buffer image 'int-pixel-vector-element-type length))

#|
(typep (qffi::make-foreign-array '(10 10) :element-type 'double-float) 'simple-array) = NIL
(typep (make-array '(10 10) :element-type 'double-float) 'simple-array) = T
(underlying-simple-vector (make-array '(10 10) :element-type 'double-float))
(underlying-simple-vector (qffi::make-foreign-array '(10 10) :element-type 'double-float))

(defun tst (a)
  (declare (type (simple-array double-float (* *)) a))
  (declare (optimize (speed 3) (safety 0)))
  (aref a 0 0))
(tst (qffi::make-foreign-array '(10 10) :element-type 'double-float)) 
; function entry causes an error due to type mismatch.

|#


(defun vector-image-iref (image x y buf)
  (declare (ignore image x y buf))
  (error "iref not supported for vector-images"))

(defun vector-image-iset (image x y buf)
  (declare (ignore image x y buf))
  (error "iset not supported for vector-images"))

(defun vector-image-diref (image x y buf)
  (declare (ignore image x y buf))
  (error "diref not supported for vector-images"))

(defun vector-image-diset (image x y buf)
  (declare (ignore image x y buf))
  (error "diset not supported for vector-images"))

(defun vector-image-band-interleaved-viref (image x y buf)
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 0)))
  (declare (optimize (safety 0) (speed 3)))
  (viref (vector-image-band-interleaved-image image) x y buf))

(defun vector-image-band-interleaved-viset (image x y buf)
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 0)))
  (declare (optimize (safety 0) (speed 3)))
  (viset (vector-image-band-interleaved-image image) x y buf)
  buf)

(defun vector-image-band-interleaved-vdiref (image x y buf)
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 0)))
  (declare (optimize (safety 0) (speed 3)))
  (vdiref (vector-image-band-interleaved-image image) x y buf))

(defun vector-image-band-interleaved-vdiset (image x y buf)
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 0)))
  (declare (optimize (safety 0) (speed 3)))
  (vdiset (vector-image-band-interleaved-image image) x y buf)
  buf)

(defun assure-vector-image-iref-arrays (n)
  (declare (ignore n)))

(defmethod image-indirected-p ((image image))
  (image-indirected-to image))

(defmethod image-indirected-p ((vector-image vector-image))
  (with-class-slots vector-image (component-images) vector-image
    (loop for img in component-images
	  always (image-indirected-to img))))


(eval-when (:compile-toplevel #-COMPILE-FOR-RUNTIME-ONLY-DISKSAVE :load-toplevel)

(defmacro unsafe-destructuring-bind (vars list &rest body)
  (let ((l (gensym)))
    `(let* ((,l ,list)
	    .,(loop for var in vars
		    collect `(,var (pop ,l))))
      .,body)))

(defun component-image-vars (nbands)
  (loop for i from 0 below nbands collect (gensym)))

(defun vector-image-viref-lambda (nbands)
  (let ((component-image-vars (component-image-vars nbands))
	(buffer-array (get-preallocated-integer-iref-array nbands)))
    `(lambda (image x y buf)
      #+cmu (declare (ext:optimize-interface (speed 3) (safety 0)))
      (declare (optimize (safety 0) (speed 3)))
      (declare (type fixnum x y))
      (declare (type vector-image image))
      (let* ((buf (cond ((null buf) ,buffer-array)
			((eq buf t) (make-array ,nbands :element-type 'int-pixel-vector-element-type))
			(t buf))))
	(declare (type int-pixel-vector buf))
	(unsafe-destructuring-bind
	 ,component-image-vars (vector-image-component-images image)
	 ,@(loop for i fixnum from 0 
		 for img-var in component-image-vars
		 collect `(setf (aref buf ,i) (iref ,img-var x y))))
	buf))))

(defun vector-image-viset-lambda (nbands)
  (let ((component-image-vars (component-image-vars nbands)))
    `(lambda (image x y buf)
      #+cmu (declare (ext:optimize-interface (speed 3) (safety 0)))
      (declare (optimize (safety 0) (speed 3)))
      (declare (type vector-image image))
      (declare (type fixnum x y))
      (declare (type int-pixel-vector buf))
      (unsafe-destructuring-bind
       ,component-image-vars (vector-image-component-images image)
       ,@(loop for i fixnum from 0 
	       for img-var in component-image-vars
	       collect `(setf (iref ,img-var x y) (aref buf ,i))))
      buf)))
  
(defun vector-image-vdiref-lambda (nbands)
  (let ((component-image-vars (component-image-vars nbands))
	(buffer-array (get-preallocated-float-iref-array nbands)))
    `(lambda (image x y buf)
      #+cmu (declare (ext:optimize-interface (speed 3) (safety 0)))
      (declare (optimize (safety 0) (speed 3)))
      (declare (type fixnum x y))
      (declare (type vector-image image))
      (let* ((buf (cond ((null buf) ,buffer-array)
			((eq buf t) (make-array ,nbands :element-type 'dfloat-pixel-vector-element-type))
			(t buf))))
	(declare (type dfloat-pixel-vector buf))
	(unsafe-destructuring-bind
	 ,component-image-vars (vector-image-component-images image)
	 ,@(loop for i fixnum from 0 
		 for img-var in component-image-vars
		 collect `(setf (aref buf ,i) (diref ,img-var x y))))
	buf))))

(defun vector-image-vdiset-lambda (nbands)
  (let ((component-image-vars (component-image-vars nbands)))
    `(lambda (image x y buf)
      #+cmu (declare (ext:optimize-interface (speed 3) (safety 0)))
      (declare (optimize (safety 0) (speed 3)))
      (declare (type vector-image image))
      (declare (type fixnum x y))
      (declare (type dfloat-pixel-vector buf))
      (unsafe-destructuring-bind
       ,component-image-vars (vector-image-component-images image)
       ,@(loop for i fixnum from 0 
	       for img-var in component-image-vars
	       collect `(setf (diref ,img-var x y) (aref buf ,i))))
      buf)))

(defmethod generate-image-ref-fns ((image-class (eql :vector-image)) &key nbands)
  (mv-bind (viref-fn viset-fn vdiref-fn vdiset-fn) 
      (get-image-iref-fn-symbols image-class nbands)
    `(progn (defun ,viref-fn .,(cdr (vector-image-viref-lambda nbands)))
	    (defun ,viset-fn .,(cdr (vector-image-viset-lambda nbands)))
	    (defun ,vdiref-fn .,(cdr (vector-image-vdiref-lambda nbands)))
	    (defun ,vdiset-fn .,(cdr (vector-image-vdiset-lambda nbands))))))

) ; end eval-when

#+experimental
(eval-when (:compile-toplevel #-COMPILE-FOR-RUNTIME-ONLY-DISKSAVE :load-toplevel)

(defmethod generate-image-ref-fns ((image-class (eql :vector-image)) &key nbands)
  (mv-bind (iref-fn iset-fn diref-fn diset-fn) 
      (get-image-iref-fn-symbols image-class nbands)
    (let ((int-pvect (get-preallocated-integer-iref-array nbands))
	  (float-pvect (get-preallocated-float-iref-array nbands)))
      `(macrolet ((loop-over-components (form)
		    `(progn ,@(loop for i from 0 below ,nbands
				    collect (tree-subst-atom 'img-var '(pop component-images)
							     (tree-subst-atom 'i i form)))))
		  (referencing-components (assignment-form)
		    `(let ((component-images (vector-image-component-images image))) 
		       (loop-over-components ,assignment-form)
		       pvect)))
	 (defun ,iref-fn (image x y pvect)
	   ,@(iref-preamble)
	   (let ((pvect
		  (cond ((null pvect) ,int-pvect)
			((eq pvect t) 
			 (make-array ,nbands :element-type 'int-pixel-vector-element-type))
			(t pvect))))
	     (declare (type int-pixel-vector pvect))
	     (referencing-components 
	      (setf (aref pvect i) (iref img-var x y)))))

	 (defun ,iset-fn (image x y pvect)
	   (declare (type int-pixel-vector pvect))
	   ,@(iref-preamble)
	   (referencing-components
	    (setf (iref img-var x y) (aref pvect i))))

	 (defun ,diref-fn (image x y pvect)
	   ,@(iref-preamble)
	   (let ((pvect 
		  (cond ((null pvect) ,float-pvect)
			((eq pvect t) 
			 (make-array ,nbands :element-type 'dfloat-pixel-vector-element-type))
			(t pvect))))
	     (declare (type dfloat-pixel-vector pvect))
	     (referencing-components
	      (setf (aref pvect i) (diref img-var x y)))))

	 (defun ,diset-fn (image x y pvect)
	   (declare (type dfloat-pixel-vector pvect))
	   ,@(iref-preamble)
	   (referencing-components
	    (setf (diref img-var x y) (aref pvect i))))

	 ))))

  ) ;; end eval-when

#|
(generate-image-ref-fns :vector-image :nbands 2)
(disassemble 'vector-image-2-iref)
|#

(defmacro generate-default-vector-image-iref-fns ()
  `(progn . ,(loop for nbands in config::*default-vector-image-nbands*
		   collect (generate-image-ref-fns :vector-image :nbands nbands
						   ))))

;;; Force the default IREF/ISET functions to be compiled at compile-time.
;;; This is needed in order to distribute code in a "disksave" without a compiler.
(generate-default-vector-image-iref-fns)
   
(defmethod set-iref-fns ((image vector-image))
  (setf (image-iref-fn image) #'vector-image-iref
	(image-iset-fn image) #'vector-image-iset
	(image-diref-fn image) #'vector-image-diref
	(image-diset-fn image) #'vector-image-diset)

  (if (band-interleaved-image image)
      (setf (image-viref-fn image) #'vector-image-band-interleaved-viref
	    (image-viset-fn image) #'vector-image-band-interleaved-viset
	    (image-vdiref-fn image) #'vector-image-band-interleaved-vdiref
	    (image-vdiset-fn image) #'vector-image-band-interleaved-vdiset)
      
      (multiple-value-bind (iref-fn iset-fn diref-fn diset-fn)
	  (get-image-ref-fns :vector-image :nbands (length (component-images image)))
	(setf (image-viref-fn image) iref-fn
	      (image-viset-fn image) iset-fn
	      (image-vdiref-fn image) diref-fn
	      (image-vdiset-fn image) diset-fn))))

    
#|
(vector-image-viref-lambda  3)
(vector-image-vdiref-lambda 3)

			    
(vector-image-viref-fn vector-image-viref-1 1 *vector-image-iref-int-array1*)
(vector-image-viref-fn vector-image-viref-2  2 *vector-image-iref-int-array1*)

(vector-image-vdiref-fn vector-image-vdiref-1  1 *vector-image-iref-float-array1*)
(vector-image-vdiref-fn vector-image-vdiref-2  2 *vector-image-iref-float-array1*)

(disassemble 'vector-image-viref-1)
(disassemble 'vector-image-viref-2)
(disassemble 'vector-image-vdiref-1)
(disassemble 'vector-image-vdiref-3)
|#

;;; ********************  COLOR-IMAGE   ********************

#|
(defstruct-class color-image (vector-image) ())
|#

(defstruct-class color-image
    (vector-image)
  ((red-image :initarg :red-image :accessor red-image)
   (green-image :initarg :green-image :accessor green-image)
   (blue-image :initarg :blue-image :accessor blue-image))
  (:default-initargs
      :component-names '(RED GREEN BLUE)
    :component-pathnames '(".red" ".grn" ".blu")))

(defmethod initialize-instance :after ((image color-image) &rest ignore)
  (ignore ignore)
  (with-class-slots color-image (red-image green-image blue-image component-images) image
    (setf red-image (nth 0 component-images)
	  green-image (nth 1 component-images)
	  blue-image (nth 2 component-images))))

(defmethod image-for-display ((image color-image))
  (or (band-interleaved-image image)
      (error "image-for-display of band-sequential color-image is not currently supported")))


(defmethod load-image-wrap-image ((image image))
  (case (image-element-type image)
    ((rgb8 rgba8)
     (make-color-image-from-band-interleaved-image image))
    (otherwise image)))

(defmethod save-image ((image vector-image) path  &rest args &key compression-mode jpeg-quality)
  (declare (ignore compression-mode jpeg-quality))
  (if (band-interleaved-image image)
      (apply 'save-image (band-interleaved-image image) path args)
      (error "load-save-image non-band-interleaved vector-image is not yet supported"))
  )

(defmethod red-image ((image color-image))
  (car (component-images image)))

(defmethod green-image ((image color-image))
  (cadr (component-images image)))

(defmethod blue-image ((image color-image))
  (caddr (component-images image)))

(defmethod alpha-image ((image color-image))
  (cadddr (component-images image)))

(defun make-color-image-from-rgb (red green blue)
  (make-instance 'color-image :component-images (list red green blue) ))

(defun make-color-image (dims &rest args)
  (make-color-image-from-band-interleaved-image
   (apply 'make-image dims :element-type 'rgb8 args)))


;;; THE REMAINDER OF THIS FILE IS UNFINISHED

#+UNFINISHED
(progn
  

;;; there should be a whopper (:image-linear-geom-map-internal image)
;;; which handles all of the common stuff here
(defmethod image-linear-geom-map-internal
    ((vector-image vector-image)
     a00 a01 a02 a10 a11 a12 xdim ydim identity)
  (ignore identity)
  (eval-cache (image-linear-geom-map vector-image a00 a01 a10 a11 a02 a12 xdim ydim)
      (let ((image vector-image)
	    (result-image
	     (unary-operator vector-image 'image-linear-geom-map-internal
			     a00 a01 a02 a10 a11 a12 xdim ydim identity)))
	(set-linear-geom-transform
	 result-image
	 (make-linear-geom-transform-info
	  :image image
	  :matrix (transform-matrix-from-coeffs nil a00 a01 a02 a10 a11 a12)
	  :x-dim xdim :y-dim  ydim))
	(when identity 
	  (setf (image-window-of result-image) (or (image-window-of image) image))
	  (window-self result-image image nil nil nil nil)) ; last 4 args ignored
	result-image)))

(defmethod image-geom-linear-xform
    ((vector-image vector-image)
     du/dx du/dy u0 dv/dx dv/dy v0
     &optional x-dim y-dim
     interpolator
     (background 0))
  (eval-cache (image-geom-linear-xform vector-image du/dx du/dy u0 dv/dx dv/dy v0
				       x-dim y-dim interpolator background)
      (unary-operator vector-image 'image-geom-linear-xform
		      du/dx du/dy u0 dv/dx dv/dy v0
		      x-dim y-dim interpolator background)))

(defmethod initialize-elements
    ((vector-image vector-image) &optional new-initial-value-list )
  (with-class-slots vector-image (component-images) vector-image
    (unless new-initial-value-list
      (setq new-initial-value-list
	    (loop for img in component-images
		  collect (slot-value img 'initial-value))))
    (loop for img in component-images
	  for init-value in new-initial-value-list
	  do  (initialize-elements img init-value))))

#| this loses -- complex image does not have instance vars for x-map and y-map
(defmethod map-image-geometry
    ((vector-image vector-image)
     x-fun  y-fun			
     &optional
     xy-swap transform-matrix
     (x-range (image-x-dim vector-image))
     (y-range (image-y-dim vector-image)))
  (unary-operator vector-image 'map-image-geometry x-fun y-fun xy-swap x-range y-range))
|#

(defmethod map-image-geometry
	   ((vector-image vector-image)
	    x-fun  y-fun			
	    &optional
	    xy-swap transform-matrix
	    (x-range (image-x-dim vector-image))
	    (y-range (image-y-dim vector-image)))
  (with-class-slots vector-image (component-images element-type element-size band-interleaved-image)
      vector-image
    (let* ((opr-images 
	    (loop for img in component-images
		  collect (map-image-geometry img x-fun y-fun xy-swap transform-matrix x-range y-range)))
	   (first (car opr-images))
	   (bii (and band-interleaved-image
		     (map-image-geometry band-interleaved-image
					 x-fun y-fun xy-swap transform-matrix x-range y-range))))
      (let ((result-image (make-image (image-dimensions first)
				      :element-type element-type
					;:element-size element-size
				      :image-type (type-of vector-image)
				      :band-interleaved-image bii
				      :component-images opr-images)))
	(set-linear-geom-transform result-image
				   (make-linear-geom-transform-info
				    :image vector-image
				    :matrix transform-matrix
				    :x-dim x-range :y-dim y-range))
	result-image))))

;;; These methods are inconsistant w.r.t. com-zoom-in and com-zoom-out
;;; (method zoom-in :around (basic-image) ...) does caching and search rule
(defmethod zoom-in ((vector-image vector-image) &rest args)
  (apply 'unary-operator vector-image 'zoom-in args) )

;;; (method zoom-out :around (basic-image) ...) does caching and search rule
(defmethod zoom-out ((vector-image vector-image) &rest args)
  (apply 'unary-operator vector-image 'zoom-out args) )


(defmethod gauss-image ((vector-image vector-image) level &rest args)
  (apply 'unary-operator vector-image 'gauss-image level args))

(defmethod image-linear-xform ((vector-image vector-image) mult off &rest args)
  (apply 'unary-operator vector-image 'image-linear-xform mult off args))

(defmethod image-add ((image1 vector-image) (image2 vector-image) &rest args)
  (apply 'binary-operator image1 image2 'image-add args))


(defmethod image-threshold ((vector-image vector-image) threshold-value &rest args)
  (apply 'unary-operator vector-image 'image-threshold threshold-value args))

(defmethod image-element-min-max ((vector-image vector-image) &rest args)
  (with-class-slots vector-image (component-images) vector-image
    (loop for image in component-images
	  with comp-min and comp-max
	  do (multiple-value-setq (comp-min comp-max)
		 (apply 'image-element-min-max image args))
	  maximize comp-max into vector-max
	  minimize comp-min into vector-min
	  finally (return (values vector-min vector-max)))))


(defmethod stretch-params
    ((vector-image vector-image)
     &rest args &key (allow-offset t) (result-type '(unsigned-byte 8))
     &allow-other-keys)
  (with-class-slots vector-image (component-images) vector-image
    (loop for image in component-images
	  with result-range = (result-range (array-element-size* result-type)) ; fix this
	  with factor and offset and left and right
	  do (multiple-value-setq (factor offset left right)
		 (apply 'stretch-params image args))
	     (ignore factor offset)
	  maximize right into vector-max
	  minimize left into vector-min
	  finally
       (unless allow-offset (setq vector-min 0))
       (let ((gain (handler-case (/ (float (1- result-range))
				    (- vector-max vector-min))
		       (division-by-zero () 1.0))))
	 (return (values gain
			 (- (* vector-min gain))
			 vector-min
			 vector-max))))))


(defmethod copy-image ((vector-image vector-image) &optional into-image)
  (with-class-slots vector-image (component-images) vector-image
    (make-vector-image
     (loop for component in component-images
	   with into-components = (and into-image (component-images into-image))
	   for into-component = (pop into-components) 
	   collect (copy-image component :into-image into-component))
    
     :image-type (type-of vector-image))))

(defmethod similar-image ((vector-image vector-image) &rest args)
  (with-class-slots vector-image (component-images) vector-image
    (make-vector-image
     (loop for component in component-images
	   collect (apply 'similar-image component args))
     :image-type (type-of vector-image))))


(defmacro vector-image-unary-opr
    (function-name method-arglist function-arglist)
  `(defmethod ,function-name ((vector-image vector-image) . ,method-arglist)
     (unary-operator vector-image ',function-name . ,function-arglist)  ))

(defmacro vector-image-binary-opr
    (function-name method-arglist function-arglist)
  `(defmethod ,function-name ((image1 vector-image) (image2 vector-image). ,method-arglist)
    (binary-operator self ',function-name . ,function-arglist)  ))

(vector-image-unary-opr image-negate (&key into-image) (:into-image into-image))


(defun make-vector-image
    (component-images &rest args &key image-type &allow-other-keys)
  #+symbolics
  (declare (arglist component-images &key image-type component-names component-pathnames))
  (let ((first (first component-images)))
    (apply 'make-image (image-dimensions first)
	   :image-type (or image-type 'vector-image)
	   :component-images component-images args)))



(defun image-calc-merge-pathnames (path defaults)
  (when (stringp path)
    (setq path (merge-pathnames path defaults)))
  (when (zerop (length (pathname-name path)))
    (setq path (make-pathname :name nil :defaults path)))
  (when (zerop (length (pathname-type path)))
    (setq path (make-pathname :type nil :defaults path)))
  path)


(defun load-vector-image (file-or-list
			 &key component-paths component-names
			 file-format (image-type 'vector-image))
  (if (listp file-or-list)
      (make-vector-image (loop for pathname in file-or-list
			       collect (load-image pathname))
			 :image-type image-type)
      
      (loop with path = (pathname file-or-list)
	    for component-path in component-paths
	    for component-name = (pop component-names)
	    for merged-path = (image-calc-merge-pathnames
				    (setq component-path (string component-path))
				    path)
	    for name = (or component-name (intern (substring component-path 1)))
	    collect
	    (if file-format
		(load-image merged-path :file-format file-format)
		(load-image merged-path))
	      into images
	    collect name into names
	    collect component-path into paths
	    finally 
	 (return (make-vector-image images
				    :image-type image-type
				    :component-pathnames paths
				    :component-names (or component-names names))))))

(defclass vector-image-file
	(image-file-format)
	((vector-image-class :initarg :vector-image-class)
	 (component-pathnames :initarg :component-pathnames)
	 (first-component-header :initarg :first-component-header)))


(defmethod image-file-format-make-image ((vector-image vector-image-file))
  (with-class-slots vector-image (element-size element-type component-pathnames x-dim y-dim) vector-image
    (apply 'make-vector-image
	   (loop with type = element-type ;(array-type-specifier-from-size element-size)
		 repeat (length component-pathnames) ; this looks wrong !!
		 collect
		 (make-image (list x-dim y-dim) :element-type type)))))


(defmethod image-file-format-recognize-header
    ((image-format vector-image-file) header-string new-stream)
  (ignore new-stream)
  (and (string-equal header-string "VECTOR-IMAGE" :end1 12 :end2 12)
       (allocate-file-format 'vector-image-file))
  )

#|
VECTOR-IMAGE header format:

    VECTOR-IMAGE
    <vector-image-class>
    <list-of-component-pathnames>
|#

(defmethod image-file-format-read-header
    ((image-format vector-image-file) new-stream &optional ignore)
  (ignore new-stream ignore)
  (with-class-slots vector-image (pathname vector-image-class
			component-pathnames
			x-dim y-dim element-size element-type header-length
			block-x-dim block-y-dim block-size)
      image-format
    
    (with-open-file (strm (ev-pathname-translate pathname) :direction :input)
      (read strm)			; skip VECTOR-IMAGE string
      (setq vector-image-class
	    (let ((*package* (find-package "USER"))) (read strm))
	    component-pathnames (read strm)))
    (setq component-pathnames
	  (loop for path in component-pathnames
		collect (string-downcase path)))
    (let (type)
      (ignore type)
      (multiple-value-setq
	  (type x-dim y-dim element-size element-type header-length
		block-x-dim block-y-dim block-size)
	  (load-image-header (image-calc-merge-pathnames
			      (string (car component-pathnames))
			      pathname))))))

(defmethod image-file-format-load-image
    ((image-format vector-image-file) file into-image)
  (with-class-slots vector-image (component-pathnames vector-image-class) image-format
    (when into-image (unmake-image into-image :expunge t))
    (load-vector-image file :component-paths component-pathnames
		       :image-type vector-image-class)))

;;; this conflicts with the slot-accessor of same name
(defmethod component-pathnames ((vector-image vector-image))
  (with-class-slots vector-image (component-pathnames component-names component-images) vector-image
    (or component-pathnames
	(setq component-pathnames
	      (if component-names
		  (loop for name in component-names
			collect (format nil ".~s" name))
		  (loop for index from 0 below (length component-images)
			collect (format nil ".C~d" index)))))))

(defmethod save-image*
    ((vector-image vector-image)
     pathname
     &key 
     component-paths
     (file-format *save-image-default-file-format*)
     &allow-other-keys)

  (with-class-slots vector-image (component-pathnames component-images) vector-image

    (unless component-paths (setq component-paths component-pathnames))

    (with-open-file (stream (ev-pathname-translate pathname) :direction :output :characters t)
      (let ((*package* (find-package "USER")))
	(format stream "VECTOR-IMAGE~%~s~%~s~%"
		(type-of vector-image)
		component-pathnames)))

    (loop for image in component-images
	  for path in component-paths
	  do (save-image image
			 (image-calc-merge-pathnames (string path) pathname) 
		  :file-format file-format))
    vector-image))

(setup-image-file-formats '(vector-image-file))

#|

(setq v-img (make-vector-image (list (image-window heeger-7 0 0 64 64)
				     (image-window heeger-7z 0 0 64 64))
			       :component-names '(int zbuf)))
(save-image v-img "ti:>pix>test.vimg")
(eval-cache-flush-value v-img2)
(setq v-img2 (load-image "ti:>pix>test.vimg"))
|#


;;; Perhaps vector-image should be renamed to vector-image-mixin and this 
;;; can then be renamed to vector-image.

;;; Who uses this ?????

(defclass vector-image-with-viewpoint
	(vector-image)
	())

#|
;;; This forces the same gain and offset on every component, and only makes sense when
;;; each component has the same element size.
(fundefine '(flavor:method :compute-gain-and-offset vector-image))
(defmethod (:compute-gain-and-offset vector-image) (gain offset)
  (loop for img in component-images
	with (g o)
	do (cl:multiple-value-setq (g o) (send img :compute-gain-and-offset gain offset))
	minimize g into vector-gain
	minimize o into vector-offset
	finally (return (values vector-gain vector-offset ))))
|#

(defmethod compute-photometric-transform ((vector-image vector-image) photometric-transform)
  (with-class-slots vector-image (element-size) vector-image
    (let ((element-size (car element-size)))
      (if (or (= element-size 1) (= element-size 8))
	  (multiple-value-bind (gain offset)
	      (photometric-transform-gain-and-offset photometric-transform)
	    (make-photometric-transform (or gain 1) (or offset 0))) 
	  (compose-photometric-transforms vector-image photometric-transform)))))


#|
(defmethod (:24bit-screen color:common-color-hardware-mixin) ()
  (>= tv:bits-per-pixel 24))
|#

;;; this needs to be specialized to each hardware screen type.

(defun directly-displayable-image-p (screen image)
  (let ((element-size (image-element-size image))
	(bitbltable (image-bitbltable image))
	)
    (and bitbltable
	 (case (screen-depth screen)
	   (1 (eql element-size 1))
	   (8 (or (eql element-size 8) (eql element-size 1)))
	   (otherwise nil)))))


;;; This is used for both color-image and anaglyph-image
;;; It is not clear that this should be specialized to vector-image 
(defmethod display-image-choose-image-and-method
    ((vector-image vector-image)
     screen image-to-window-matrix photometric-transform zoom-factor)
  (with-class-slots vector-image (component-images) vector-image
    (let* ((zoom-factor (if image-to-window-matrix
			    (transform-matrix-zoom-factor image-to-window-matrix )
			    zoom-factor))
	   (zoom-p (not (= zoom-factor 1)))
	   (all-direct-p (loop for img in component-images
			       always (directly-displayable-image-p screen img)))
			    
	   (remap-p (not (and all-direct-p
			      (photometric-transform-identity-p photometric-transform ))))
	   (image vector-image)
	   (method 'direct-display-image))
      ;;(format t "display-image-choose-image-and-method: zoom-p = ~a~%" zoom-p)
      (case (screen-depth screen)
	(1 (error "Cannot display color images on B&W screen"))
      
	(8 (multiple-value-bind (gain offset )
	       (photometric-transform-gain-and-offset photometric-transform)
	     (setq image (map-to-8-bits vector-image screen gain offset)
		   photometric-transform (make-photometric-transform 1 0))
	     (unless (and image (image-bitbltable image) (not zoom-p))
	       (setq method 'non-interpolating-zoom-remap-display-image))))
	(24 (unless (and (not zoom-p) (not remap-p))
	      (setq method 'zoom-remap-display-image ))))
      (values image method photometric-transform))))



) ; end #+unfinished progn



;;; This is a slow hack - only intended as a fallback in case we
;;; "don't know how" to compute an image operation on a vector image.


(defmethod ensure-scalar-image ((img vector-image) &key (into-image (make-image (image-dimensions img))))
  (let ((max-sq-sum (loop for component in (img::component-images img)
			  sum (multiple-value-bind (min max)
				  (image-element-min-max component)
				(* max max)))))

    (loop with maxval = (sqrt max-sq-sum)
	  for i fixnum from 0 below (image-x-dim img)
	  when (plusp maxval)
	  do (loop for j fixnum from 0 below (image-y-dim img)
		   for pixel = (viref img i j)
		   do (setf (iref into-image i j)
			    (round
			      (* 255.0
				 (/ (sqrt (loop for x across pixel sum (* x x)))
				    maxval))))))
    into-image))
					    

(defmethod fast-gauss-convolve ((image vector-image) &key (into-image (similar-image image))
				(level 1)
				scratch-image)
  (loop for comp-in in (component-images image)
	for comp-out in (component-images into-image)
	do (fast-gauss-convolve comp-in
		:level level
		:into-image comp-out
		:scratch-image scratch-image))
  into-image)



(defmethod intensity-contour-image ((image vector-image) &optional contour-level (into-image (similar-image image)))
  (declare (optimize (speed 3)(safety 0)))
  (loop for in in (component-images image)
	for out in (component-images into-image)
	do (intensity-contour-image in contour-level out))
  into-image)

(defmethod image-sobel ((image vector-image) &key (into-image (make-image (image-dimensions image)
									  :element-type 'double-float)))
  (declare (optimize (speed 3)(safety 0)))
  (initialize-image into-image 0.0)
  (loop for in in (component-images image)
	do (img::image-add (image-sobel in) into-image  :into-image  into-image))
  into-image)


(defmethod image-subtract ((image-a vector-image) (image-b vector-image) &key
			   (into-image (similar-image image-a))
			   (temp-float-image (make-image (image-dimensions image-a) :element-type 'double-float)))
  (declare (optimize (speed 3)(safety 0)))
  ;; if we do lots of these, then we will surely die:
  (let ((float-result (or temp-float-image (make-image (image-dimensions image-a) :element-type 'double-float))))
    (loop for a-in in (component-images image-a)
	  for b-in in (component-images image-b)
	  for out in (component-images into-image)
	  do (image-subtract a-in b-in :into-image float-result)
	     (image-round float-result out -128.0 128.0 0 255)))
  (values into-image float-result)) ;; in the meantime, return the temp image as the second value in case we want to re-use it.

(defmethod image-add ((image-a vector-image) (image-b vector-image) &key (into-image (similar-image image-a)))
  (declare (optimize (speed 3)(safety 0)))
  (let ((float-result (make-image (image-dimensions image-a) :element-type 'double-float)))
    (loop for a-in in (component-images image-a)
	  for b-in in (component-images image-b)
	  for out in (component-images into-image)
	  do (image-add a-in b-in :into-image float-result)
	     (image-round float-result out -128.0 128.0 0 255)))
  into-image)


(defmethod zero-crossing-image ((image vector-image) &key zero (into-image (similar-image image)))
  (loop for in in (component-images image)
	for out in (component-images into-image)
	do (zero-crossing-image in :zero zero :into-image out))
  into-image)


(defmethod image-sobel ((image vector-image)  &key (into-image (similar-image image))
			(scratch-image (make-image (image-dimensions image) :element-type 'double-float)))
  (declare (optimize (speed 3)(safety 0)))
  (let ((float-result scratch-image))
    (loop for in in (component-images image)
	  for out in (component-images into-image)
	  do (image-sobel in :into-image float-result)
	     (image-round float-result out 0.0 1442.0 0 255)))
  into-image)
