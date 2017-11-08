(in-package :img)

#|

Array-image interoperation between Lisp and C.

  . Lisp OWNS THE IMAGE.

  . make-array-image calls make_array_image to create the C structures.

  . make_array_image calls Lisp foreign-callable make_lisp_array_image
      to construct the Lisp array-image structure, and its STATIONARY arrays
      containing x-map y-map and image-array.

  . make_lisp_array_image calls set_array_image_arrays to set the array pointers
    in the C array_image structure.
   
  . A hash-table associates the C image-id to the Lisp array-image structure
    (since the Lisp array-image structure is not stationary).

|#

;;; *********************  ARRAY-IMAGE  *********************

(defstruct-class array-image (scalar-image)
    ((array :initarg :array  :accessor image-array)
     (block-x-dim :initarg :block-x-dim :accessor array-image-block-x-dim)
     (block-size :initarg :block-size :accessor array-image-block-size)
     (element-type-code :initarg :element-type-code :accessor array-image-element-type-code)
     ))

(defmethod initialize-instance :after ((image array-image) &rest initargs )
  (declare (ignore initargs))
  ;;(unless (member (image-element-type image) '(rgb8 rgba8)))
  (set-image-struct-slots image)
  ;; why were these added only here?
  (setf (array-image-block-x-dim image) (image-block-x-dim image)
	(array-image-block-size image) (image-block-size image)
	(array-image-element-type-code image) (l2c-element-type (image-element-type image)))
  )

;;;
;;; cmucl goes bonkers recursively printing coordinate transforms and
;;; inverses if we don't define something here...at least my copy of
;;; cmucl does.
;;;

#+connolly
(defmethod print-object ((image array-image) stream)
  (format stream "#<ARRAY-IMAGE #X~16,8,Vr>" #\0 (%pointer image)))

;;; *********************  ARRAY-IMAGE IREF/ISET  *********************

(eval-when (:compile-toplevel #-COMPILE-FOR-RUNTIME-ONLY-DISKSAVE :load-toplevel)

(defmethod generate-image-ref-fns ((image-class (eql :array-image)) &key element-type)
  (mv-bind (iref-fn iset-fn diref-fn diset-fn) 
      (get-image-iref-fn-symbols image-class (element-type-to-name element-type))
    `(with-pixel-ref-macros (,element-type)
       (macrolet ((ref-pixel() 
		    '(aref (the (simple-array ,element-type (*)) (array-image-array image))
		           (maybe-reverse-bit-order (compute-pixel-index) ',element-type))))
	 (defun ,iref-fn (image x y)
	   ,@(iref-preamble)
	   (element-type->fixnum (ref-pixel)))

	 (defun ,iset-fn (image x y value)
	   ,@(iref-preamble)
	   (setf (ref-pixel) (fixnum->element-type value))
	   value)

	 (defun ,diref-fn (image x y)
	   ,@(iref-preamble)
	   (with-darray (setf (aref darray 0) (element-type->dfloat (ref-pixel)))
	     darray))

	 (defun ,diset-fn (image x y darray)
	   (declare (type (simple-array double-float (*)) darray))
	   ,@(iref-preamble)
	   (setf (ref-pixel) (dfloat->element-type (aref darray 0)))
	   darray)))))

) ;end eval-when


#|
(setq *print-lines* nil)

(setq swank::*macroexpand-printer-bindings*
  '((*print-circle* . nil)
    (*print-lines*    . nil)
    (*print-pretty* . t)
    (*print-escape* . t)
    (*print-level* . nil)
    (*print-length* . nil)))

(setq swank::*swank-pprint-bindings*
  `((*print-pretty*   . t) 
    (*print-lines*    . nil)
    (*print-level*    . nil)
    (*print-length*   . nil)
    (*print-circle*   . nil)
    (*print-gensym*   . t)
    (*print-readably* . nil)))

(generate-image-ref-fns :array-image :element-type '(unsigned-byte 8))
(generate-image-ref-fns :array-image :element-type 'double-float)

(compile-top-level-form (generate-image-ref-fns :array-image :element-type '(unsigned-byte 8)))

(disassemble 'array-image-unsigned-8bit-iref)
(disassemble 'array-image-unsigned-8bit-diref)
(disassemble 'array-image-unsigned-8bit-diset)
|#

;;; Force all implemented IREF/ISET functions to be compiled at compile-time.
;;; This is needed in order to distribute code in a "disksave" without a compiler.
(defmacro generate-default-array-image-iref-fns ()
  `(progn . ,(loop for element-type in *all-scalar-image-element-types*
		   collect (generate-image-ref-fns :array-image :element-type element-type))))

;;; Force the default IREF/ISET functions to be compiled at compile-time.
;;; This is needed in order to distribute code in a "disksave" without a compiler.
(generate-default-array-image-iref-fns)


;(get-image-ref-fns :array-image :element-type 'single-float)
(defmethod set-image-struct-slots ((image array-image))
  (let ((element-type (image-element-type image))
	(image-struct image))
    (unless (member (image-element-type image) '(rgb8 rgba8))
      ;; rgb8 and rgba8 images must be wrapped in a color-image
      (multiple-value-bind (iref-fn iset-fn diref-fn diset-fn)
	  (get-image-ref-fns :array-image :element-type element-type)
	(setf (image-iref-fn image-struct) iref-fn
	      (image-iset-fn image-struct) iset-fn
	      (image-diref-fn image-struct) diref-fn
	      (image-diset-fn image-struct) diset-fn))
      )))

    
;;; *****************  ARRAY-IMAGE INTERACTIONS WITH C CODE  *******************
;;; New Sun Jul  5 2009
(def-foreign-function (lisp_make_array_image (:name (freedius-prefix "lisp_make_array_image"))
					(:return-type (:pointer c-image)))
    (xdim :int)
    (ydim :int)
    (element-type-code :int)
    (sample-per-pixel :int)
    (blk-xdim :int)
    (blk-ydim :int)
    (padded-blk-xdim :int)
    (array :simple-array)
    (xmap :simple-array)  ; image-map-type
    (ymap :simple-array)  ; image-map-type
    )

(defvar *array-image-default-block-dims* nil)

;; default to a raster image
(defun array-image-default-block-dims (xdim ydim bits-per-sample samples-per-pixel)
  (if *array-image-default-block-dims*
      (values-list *array-image-default-block-dims*)
      (if (> (* xdim ydim) (ash 1 16))  ; FIXME -- MAKE THIS SMARTER.
	  (values 256 -256 256)
	  (values xdim (- ydim) xdim))))

(defun make-array-image (dims &key 
			 element-type 
			 (samples-per-pixel 1)
			 block-x-dim block-y-dim padded-block-x-dim
			 &allow-other-keys)
  (unless element-type (setq element-type '(unsigned-byte 8)))
  (let* ((x-dim (car dims))
	 (y-dim (cadr dims))	 
	 (bits-per-sample (element-size-from-element-type element-type))
	 )
    (unless (and block-x-dim block-y-dim padded-block-x-dim)
      (mv-setq (block-x-dim block-y-dim padded-block-x-dim)
	       (array-image-default-block-dims x-dim y-dim bits-per-sample samples-per-pixel)))

    (let* ((blocks-wide (ceiling x-dim block-x-dim))
	   (blocks-hi (ceiling y-dim (abs block-y-dim)))
	   (block-size (* block-x-dim (abs block-y-dim)))
	   (imgsize (* blocks-wide blocks-hi block-size))
	   (xmap (make-stationary-vector x-dim :element-type 'image-map-element-type))
	   (ymap (make-stationary-vector y-dim :element-type 'image-map-element-type))
	   (array
	    (or *mmapped-array-image-array*
		(case element-type
		  (RGB8  (make-stationary-vector (* 3 imgsize) :element-type '(unsigned-byte 8)))
		  (RGBA8 (make-stationary-vector (* 4 imgsize) :element-type '(unsigned-byte 8)))
		  (otherwise
		   (make-stationary-vector (* samples-per-pixel imgsize) :element-type element-type)))))
	   (element-type-code (l2c-element-type element-type))
	   (cimage (intern-foreign-pointer 
		    (lisp_make_array_image x-dim y-dim element-type-code samples-per-pixel
					   block-x-dim block-y-dim padded-block-x-dim
					   array xmap ymap)))
	     
	   (image (make-instance (case element-type
				   (RGB8 'array-image-rgb)
				   (RGBA8 'array-image-rgba)
				   (otherwise
				    (if (> samples-per-pixel 1)
					'band-interleaved-array-image
					'array-image)))
				 :id cimage
				 :x-dim x-dim :y-dim y-dim
				 :element-type element-type
				 :x-map xmap :y-map ymap
				 :array array
				 :nbands samples-per-pixel)))
    
      (when (> samples-per-pixel 1) (setf (get-prop image :samples-per-pixel) samples-per-pixel))
      (make-cimage-id-entry cimage image)
      (unless config::weak-eval-cache
	(push image *array-image-list*))
      image)))


(def-foreign-function (map_array_image (:name (freedius-prefix "map_array_image"))
				       (:return-type (:pointer c-image)))
    (image (:pointer c-image))
  (xdim :int) (ydim :int) (element-type-code :int)
  (sample-per-pixel :int)
  (xmap :simple-array)  ; image-map-type
  (ymap :simple-array)  ; image-map-type
  )

(def-foreign-function (set_array_image_arrays (:name (freedius-prefix "set_array_image_arrays")))
    (c_image (:pointer c-image))
    (xmap :simple-array) ; image-map-type
    (ymap :simple-array) ; image-map-type
    (xdim :int)
    (ydim :int)
    (array :simple-array)
    (maps-hacked :int))

(def-foreign-function (update_array_image_arrays (:name (freedius-prefix "update_array_image_arrays")))
    (c_image (:pointer c-image))
  (xmap :simple-array)			; image-map-type
  (ymap :simple-array)			; image-map-type
  (array :simple-array)
  )

(defmethod set-image-maps :after ((image array-image) x-map y-map)
   (set_array_image_arrays (image-id image)
			   x-map y-map (length x-map) (length y-map) (image-array image) 1)
  )


;;; *array-image-list* could be flushed since *c-image-id-hash-table* has info
;;; about all images.

;;; This will continue to present a very serious problem until we can
;;; reap images.  Right now, images can be made but never unmade...
(defparameter *array-image-list* nil)

;;; Special hack for creating array-images mmapped to files.
(defvar *mmapped-array-image-array* nil)

#+obsolete
(progn

;;; FIXME:  need to handle image :initial-element argument.
;;; Possible problem here: when signed 16-bit images are created,
;;; their x-maps appear to contain garbage??
(defun make_lisp_array_image_internal (c_image xdim ydim imgsize element-type-code samples-per-pixel)
  (setq c_image (intern-foreign-pointer c_image))
  (let* ((xmap (make-stationary-vector xdim :element-type 'image-map-element-type))
	 (ymap (make-stationary-vector ydim :element-type 'image-map-element-type))
	 (image-element-type
	  (or (car (rassoc element-type-code *image-element-type-alist*))
	      (error "make_lisp_array_image_internal: illegal image-element-type" )))
	 (array
	  (or *mmapped-array-image-array*
	      (case image-element-type
		(RGB8  (make-stationary-vector (* 3 imgsize) :element-type '(unsigned-byte 8)))
		(RGBA8 (make-stationary-vector (* 4 imgsize) :element-type '(unsigned-byte 8)))
		(otherwise
		 (make-stationary-vector (* samples-per-pixel imgsize) :element-type image-element-type)))))
	 (image (make-instance (case image-element-type
				 (RGB8 'array-image-rgb)
				 (RGBA8 'array-image-rgba)
				 (otherwise
				  (if (> samples-per-pixel 1)
				      'band-interleaved-array-image
				      'array-image)))
			       :id c_image
			       :x-dim xdim :y-dim ydim
			       :element-type image-element-type
			       :x-map xmap
			       :y-map ymap
			       :array array
			       :nbands samples-per-pixel)))
    
    (when (> samples-per-pixel 1) (setf (get-prop image :samples-per-pixel) samples-per-pixel))
    (make-cimage-id-entry c_image image)
    (unless config::weak-eval-cache
      (push image *array-image-list*))
    (set_array_image_arrays c_image xmap ymap xdim ydim array 0)
    #+never
    (when (> samples-per-pixel 1)
      (setq image (make-vector-image-from-band-interleaved-image image samples-per-pixel))
      (push image *array-image-list*))
    
    image))

;;; Should this move to image-ffi.lisp?

(def-foreign-callable (make_lisp_array_image
		       (:name (freedius-prefix "make_lisp_array_image"))
		       (:return-type nil))
    ((c_image (:pointer c-image))
     (xdim :signed-32bit) 
     (ydim :signed-32bit)
     (imgsize :signed-32bit)
     (element-type-code :signed-32bit)
     (samples-per-pixel :signed-32bit)
     )
  (make_lisp_array_image_internal c_image xdim ydim imgsize element-type-code samples-per-pixel)
  0)

) ; end obsolete progn


(defun make_array_image_callback_internal (xdim ydim element-type-code samples-per-pixel 
					  block-x-dim block-y-dim padded-block-x-dim)
  (let ((image (make-array-image (list xdim ydim) 
				 :element-type (c2l-element-type element-type-code)
				 :samples-per-pixel samples-per-pixel
				 :block-x-dim (if (= block-x-dim 0) nil block-x-dim)
				 :block-y-dim (if (= block-y-dim 0) nil block-y-dim)
				 :padded-block-x-dim (if (= padded-block-x-dim 0) nil padded-block-x-dim))))
    (image-id image)))

(def-foreign-callable (make_array_image_callback
		       (:name (freedius-prefix "make_array_image_callback"))
		       (:return-type (:pointer c-image)))
    ((xdim :int) 
     (ydim :int)
     (element-type-code :int)
     (samples-per-pixel :int)
     (block-x-dim :int)
     (block-y-dim :int)
     (padded-block-x-dim :int)     
     )
  (make_array_image_callback_internal xdim ydim element-type-code samples-per-pixel 
				      block-x-dim block-y-dim padded-block-x-dim))



(defmethod clone-image ((image array-image)
			&optional (element-type (image-element-type image))
			(image-class (class-of image)))
  (let* ((element-type-code (cdr (assoc element-type
					*image-element-type-index-alist*
					:test 'equal)))
	 (clone (wrap-foreign-image 
		 (map_array_image (image-id image)
				  (image-x-dim image) (image-y-dim image)
				  element-type-code 
				  (image-samples-per-pixel image)
				  (image-x-map image) (image-y-map image))
		 :image-class image-class
		 )))
    (setf (image-array clone) (image-array image))

    ;; are these needed?
  ;  (setf (image-element-type clone) element-type)
  ;  (setf (c-image-element-type-code (image-id clone)) element-type-code)
    (set-image-struct-slots clone)
    (clone-image-after-copy-slots image clone)
    clone))


;;; ***********************  ARRAY-IMAGE PERFORMANCE TESTS ***********************
;;;                           moved to img/iref-tests.lisp  


#| LISP_CLASS_SLOT_HACKS  Thu Jul 27 2006

Experiments passing Lisp image class instance directly to C++ code


(def-foreign-function (lisp_class_fixnum_slot (:name "lisp_class_fixnum_slot"))
    (array-image :lisp)
  (slot-num :int))

(def-foreign-function (lisp_array_image_xmap_ref (:name "lisp_array_image_xmap_ref"))
    (array-image :lisp)
  (xindex :int))

(def-foreign-function (lisp_array_image_xdim (:name "lisp_array_image_xdim"))
    (array-image :lisp))


(setq *img* (gui::view-image (gui::top-view)))

(lisp_array_image_xdim *img*) 1024
(lisp_array_image_xmap_ref *img* 10) 10

(describe *img*)
#<ARRAY-IMAGE 1024x1024 (UNSIGNED-BYTE 8) block-dims (128x-90) {5E425AA5}> is a structure of type ARRAY-IMAGE.
PROPERTY-LIST: (:2D-WORLD #<2D-WORLD "alv-3-41" {5E4042BD}> :NAME
                "alv-3-41.g0" :IMAGE-TO-2D-TRANSFORM ...).
ID: #<Alien (* #) at #x0837CD40>.
X-DIM: 1024.
Y-DIM: 1024.
X-MAP: #(0 1 2 3 4 ...).
Y-MAP: #(1017984 1017856 1017728 1017600 1017472 ...).
ELEMENT-TYPE: (UNSIGNED-BYTE 8).
INITIAL-VALUE: 0.
IREF-FN: #<Function ARRAY-IMAGE-UNSIGNED-8BIT-IREF {5E44B591}>.
DIREF-FN: #<Function ARRAY-IMAGE-UNSIGNED-8BIT-DIREF {5E4CA3C1}>.
ISET-FN: #<Function ARRAY-IMAGE-UNSIGNED-8BIT-ISET {5E478A29}>.
DISET-FN: #<Function ARRAY-IMAGE-UNSIGNED-8BIT-DISET {5E4FFC51}>.
VIREF-FN: #<Function UNDEFINED-VIREF-FN {60362961}>.
VDIREF-FN: #<Function UNDEFINED-VDIREF-FN {603628F1}>.
VISET-FN: #<Function UNDEFINED-VISET-FN {60362881}>.
VDISET-FN: #<Function UNDEFINED-VDISET-FN {60362811}>.
ARRAY: #(228 255 255 255 255 ...).

(lisp_class_fixnum_slot *img*
|#
