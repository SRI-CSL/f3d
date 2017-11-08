(in-package :img)

#|

Paged-image interoperation between Lisp and C.

  . Lisp OWNS THE IMAGE.

  . make-paged-image calls make_paged_image to create the C structures.

  . make_paged_image calls Lisp foreign-callable make_lisp_paged_image
      to construct the Lisp paged-image structure, and its STATIONARY arrays
      containing x-map y-map and block-map

  . make_lisp_paged_image calls set_paged_image_arrays to set the array pointers
    in the C paged_image structure.
   
  . A hash-table associates the C image-id to the Lisp paged-image structure
    (since the Lisp paged-image structure is not stationary).

  . Individual image pages (pointed to by block-map) are allocated by Lisp.

|#

(defstruct-class paged-image (scalar-image)
    ((block-map :initarg :block-map :accessor block-map)
     (tile-offset-bits :initarg :tile-offset-bits :accessor tile-offset-bits)
     ))

(eval-when (:compile-toplevel #-COMPILE-FOR-RUNTIME-ONLY-DISKSAVE :load-toplevel)

(defmethod generate-image-ref-fns ((image-class (eql :paged-image)) &key element-type tile-offset-bits
				   largest-possible)
  (mv-bind (iref-fn iset-fn diref-fn diset-fn) 
      (get-image-iref-fn-symbols image-class (element-type-to-name element-type) tile-offset-bits)
    (let* ((tile-offset-mask (1- (ash 1 tile-offset-bits)))
	   (page-number-bytespec
	    `(byte ,(- #.(1+ (log2 most-positive-fixnum)) tile-offset-bits)
		   ,tile-offset-bits) ))
      `(with-pixel-ref-macros (,element-type)
	 (macrolet ((referencing-page (&body body)
		      `(let* ((index (compute-pixel-index))
			      ;; ldb used here instead of ash to prevent sign bit from
			      ;; propagating right for very large block numbers.  With
			      ;; paged-image64 this could to flushed, with significant speed-up
			      (page-number ,',(if largest-possible
						  `(ldb ,page-number-bytespec index)
						  `(ash index ,(- tile-offset-bits))))
			      (page-array (aref (the simple-vector (paged-image-block-map image)) 
						page-number)))
			 (declare (fixnum index page-number))
			 (when (eql page-array 0)
			   (paged-image-iref-fault image page-number )
			   (setq page-array (aref (the simple-vector (paged-image-block-map image))
						  page-number)))
			 ,@body))
		    (ref-pixel () 
		      '(aref (the (simple-array ,element-type (*)) page-array)
			(logand index ,tile-offset-mask))))
	   (defun ,iref-fn (image x y)
	     ,@(iref-preamble)
	     (referencing-page 
	      (element-type->fixnum (ref-pixel))))

	   (defun ,iset-fn (image x y value)
	     ,@(iref-preamble)
	     (referencing-page 
	      (setf (ref-pixel) (fixnum->element-type value))))

	   (defun ,diref-fn (image x y)
	     ,@(iref-preamble)
	     (referencing-page
	      (with-darray (setf (aref darray 0) (element-type->dfloat (ref-pixel)))
		darray)))

	   (defun ,diset-fn (image x y darray)
	     (declare (type (simple-array double-float (*)) darray))
	     ,@(iref-preamble)
	     (referencing-page
	      (setf (ref-pixel) (dfloat->element-type (aref darray 0)))
	      darray))
	   )))))

) ; end eval-when

(defmacro generate-default-paged-image-iref-fns ()
  `(progn . ,(loop for tile-offset-bits in config::*paged-image-allowed-tile-offset-bits*
		   append (loop for element-type in *all-scalar-image-element-types*
				collect (generate-image-ref-fns :paged-image 
						 :element-type element-type 
						 :tile-offset-bits tile-offset-bits)))))

#|
(compile-top-level-form
 '(def-image-ref-fns :paged-image :element-type (unsigned-byte 8) :tile-offset-bits 20))
(compile-top-level-form 
 (generate-image-ref-fns :paged-image :element-type '(unsigned-byte 8) :tile-offset-bits 20))

(generate-image-ref-fns :paged-image :element-type '(unsigned-byte 8) :tile-offset-bits 20)
(generate-image-ref-fns :paged-image :element-type '(unsigned-byte 8) :tile-offset-bits 16)
(generate-image-ref-fns :paged-image :element-type 'single-float :tile-offset-bits 20)
(generate-image-ref-fns :paged-image :element-type 'double-float :tile-offset-bits 20)
(disassemble 'paged-image-unsigned-8bit-20-iref)
(disassemble 'paged-image-unsigned-8bit-16-iref)
(disassemble 'paged-image-double-float-20-diref)
(disassemble 'paged-image-single-float-20-iref)
(compile-top-level-form
 (generate-image-ref-fns :paged-image :element-type '(unsigned-byte 8) :tile-offset-bits 22))
(disassemble 'paged-image-unsigned-8bit-22-iref)
(in-package :gui)
(setq *img1* (view-image (top-view)))
(setq *img2* (view-image (top-view)))
(let ((x 992)(y 3081))(list (iref *img1* x y) (iref *img2* x y) (diref *img1* x y) (diref *img2* x y)))
(img::tile-offset-bits *img2*) ; 16
(in-package :img)
(let* ((img gui::*img2*)
       ;(x 992) (y 3081)
       (x 763)(y 3140)
       (x 0) (y 0)
       (index (+ (aref (image-x-map img) x) (aref (image-y-map img) y)))
       (offset-bits (tile-offset-bits img))
       (page-number (ash index (- offset-bits)))
       (offset (logand index (1- (ash 1 offset-bits))))
       (page (aref (paged-image-block-map img) page-number))
       )
  (list (iref img x y) (iref gui::*img1* x y) (aref page offset) page-number offset page))
;;; These are difference beause one uses tiff jpeg compression
|#

;;; Force the default IREF/ISET functions to be compiled at compile-time.
;;; This is needed in order to distribute code in a "disksave" without a compiler.
(generate-default-paged-image-iref-fns)

(defmethod set-image-struct-slots ((image paged-image))
  (let* ((element-type (image-element-type image))
	 (tile-offset-bits (tile-offset-bits image))
	 (read-only (eql 1 (paged_image_read_only (image-id image)))))
    (unless (member (image-element-type image) '(rgb8 rgba8))
      ;; rgb8 and rgba8 images must be wrapped in a color-image
      (multiple-value-bind (iref-fn iset-fn diref-fn diset-fn)
	  (get-image-ref-fns :paged-image :element-type element-type
			     :tile-offset-bits tile-offset-bits)
	(when read-only
	  (setf iset-fn #'paged-image-write-write-lock-error
		diset-fn #'paged-image-write-write-lock-error))
	(setf (image-iref-fn image) iref-fn
	      (image-iset-fn image) iset-fn
	      (image-diref-fn image) diref-fn
	      (image-diset-fn image) diset-fn))
      )))


;;; Numbers greater than most-positive-fixnum are mapped to negative fixnums
;;; Produces an error unless integer is in the range 0 to (1+ (* 2 most-positive-fixnum))
(defun truncate-image-map-entry-to-fixnum (integer)
  (if (typep integer 'bignum)
      (if (or (> integer #.(+ most-positive-fixnum most-positive-fixnum 1 ))
	      (< integer 0))
	  (error "integer out of range ~d: requires ~d bits to represent" integer (log2 integer)) 
	  (+ integer #.(+ most-negative-fixnum most-negative-fixnum)))
	  
      integer))

(defun paged-image-page-number-bytespec (tile-offset-bits )
  ;; the magic number 30 is the number
  (byte (- #.(1+ (log2 most-positive-fixnum)) tile-offset-bits) tile-offset-bits))
			     
(defun compose-paged-image-pixel-index (image tile-number tile-index)
  (truncate-image-map-entry-to-fixnum
   (dpb tile-number
	(paged-image-page-number-bytespec (tile-offset-bits image))
	tile-index)))

(defun decompose-paged-image-pixel-index (image pixel-index)
  (let ((offset-bits (tile-offset-bits image)))
    (values (ldb (paged-image-page-number-bytespec offset-bits) pixel-index )
	    (ldb (byte offset-bits 0) pixel-index))))

;;; *****************  PAGED-IMAGE INTERACTIONS WITH C CODE  *******************

(def-foreign-function (paged_image_iref_fault (:name (freedius-prefix "paged_image_iref_fault")))
    (img (:pointer c-image)) ( tile-number :int))

(def-foreign-function (paged_image_iset_fault (:name (freedius-prefix "paged_image_iset_fault")))
    (img (:pointer c-image)) (tile-number :int))

(defun paged-image-iref-fault (image tile-number)
  (paged_image_iref_fault (image-id image) tile-number))

(defun paged-image-iset-fault (image tile-number)
  (paged_image_iset_fault (image-id image) tile-number))

(defun paged-image-write-write-lock-error (image x y val)
  (declare (ignore x y val))
  (error "~a is write-locked." image))

(def-foreign-function (paged_image_read_only (:name (freedius-prefix "paged_image_read_only")))
    (img (:pointer c-image))) 

;;; ************************  MAKE_LISP_PAGED_IMAGE  ************************

(def-foreign-function (set_paged_image_arrays (:name (freedius-prefix "set_paged_image_arrays")))
    (img (:pointer c-image))
    (xmap :simple-array)
    (ymap :simple-array)
    (xdim :int)
    (ydim :int)
    (maps-hacked :int))

;;; 
(defmethod set-image-maps :after ((image paged-image) x-map y-map)
  ;;(break)
   (set_paged_image_arrays (image-id image) x-map y-map (length x-map) (length y-map) 1)
  )

(def-foreign-function (set_paged_image_tile_offset_bits (:name (freedius-prefix "set_paged_image_tile_offset_bits")))
    (img (:pointer c-image)) (tile-offset-bits :int))


;;; *paged-image-list* could be flushed since *c-image-id-hash-table* has info
;;; about all images.
(defparameter *paged-image-list* nil)

(defun make_lisp_paged_image_internal (c_image xdim ydim element-type-code samples-per-pixel
				       tile-offset-bits)
  (setq c_image (intern-foreign-pointer c_image))
  ;;(setq *foo* (list element-type-code samples-per-pixel tile-offset-bits))
  (let* ((xmap (make-stationary-vector xdim :element-type 'image-map-element-type))
	 (ymap (make-stationary-vector ydim :element-type 'image-map-element-type))
	 (image-element-type
	  (or (car (rassoc element-type-code *image-element-type-alist*))
	      (error "make_lisp_paged_image_internal: illegal image-element-type ~a" element-type-code)))
	 (offset-bits (if config::*paged-image-allowed-tile-offset-bits*
			  (loop for allowed fixnum in config::*paged-image-allowed-tile-offset-bits*
				when (<= tile-offset-bits allowed)
				  return allowed
				finally (error "tile-offset-bits too large: ~a" tile-offset-bits))
			  tile-offset-bits))
	 (image (make-instance (case image-element-type
				 (RGB8 'paged-image-rgb)
				 (RGBA8 'paged-image-rgba)
				 (otherwise (if (> samples-per-pixel 1)
						'band-interleaved-paged-image
						'paged-image)))
			       :id c_image
			       :x-dim xdim :y-dim ydim
			       :element-type image-element-type
			       :x-map xmap
			       :y-map ymap
			       ;;:block-map block-map
			       :tile-offset-bits offset-bits
			       :nbands samples-per-pixel)))
    (when (> samples-per-pixel 1) (setf (get-prop image :samples-per-pixel) samples-per-pixel))
    (make-cimage-id-entry c_image image)
    (unless config::weak-eval-cache
      (push image *paged-image-list*))
    ;(set_paged_image_arrays c_image xmap ymap xdim ydim 1) ; BUG  - why map-hacked?
    (set_paged_image_arrays c_image xmap ymap xdim ydim 0) ; BUG  - why map-hacked? -- needed only for vector-images
    (set_paged_image_tile_offset_bits c_image offset-bits)
    image))

(defmethod clone-image ((image paged-image) &optional
			(element-type (image-element-type image))
			(image-class (class-of image)))
  (let* ((element-type-code (cdr (assoc element-type
					*image-element-type-index-alist*
					:test 'equal)))

	 (clone (wrap-foreign-image 
		 (map_paged_image (image-id image)
				  (image-x-dim image) (image-y-dim image)
				  element-type-code 
				  (image-samples-per-pixel image)
				  (image-x-map image) (image-y-map image))
		 :image-class image-class)))
    (setf (block-map clone) (block-map image)
	  (tile-offset-bits clone) (tile-offset-bits image))
						   
    ;; are these needed?
    ;;(setf (image-element-type clone) element-type)
    ;;(setf (c-image-element-type-code (image-id clone)) element-type-code)
    (set-image-struct-slots clone)
    (clone-image-after-copy-slots image clone)
    clone))

#|
(describe *c-image-id-hash-table*)
(maphash (lambda (key val) (print (list key val))) *c-image-id-hash-table*)
|#

;;;
;;; Is this ok?
;;;
(def-foreign-function (map_paged_image (:name (freedius-prefix "map_paged_image"))
				       (:return-type (:pointer c-image)))
  (image (:pointer c-image))
  (xdim :int) (ydim :int) (element-type-code :int)
  (samples_per_pixel :int)
  (xmap :simple-array)
  (ymap :simple-array)
  )


;;; *******************  IMAGE PAGE QUEUE ENTRY INTERACTIONS  *******************

(def-foreign-function (set_image_page_queue_entry_arrays
		       (:name (freedius-prefix "set_image_page_queue_entry_arrays")))
    ;;(image_page_queue_entry :int)
  (image_page_queue_entry :unsigned-long)
  (lisp-array :simple-array)
  (array :simple-array))


(defvar *image_page_queue_entry_arrays* nil)

(defun make_image_page_queue_entry_array (image_page_queue_entry bytes-per-page)
  (let* ((array (make-stationary-vector bytes-per-page
					:element-type '(unsigned-byte 8))))
    ;; This array can be accessed in other element sizes via
    ;; declarations to the compiler.
    ;; Make sure GC doesn't reclaim array.
    ;;#+allegro
    (push array *image_page_queue_entry_arrays*)
    (set_image_page_queue_entry_arrays image_page_queue_entry array array)
    array))

;;; array-address is passed because I am not sure how to make the FFI pass a Lisp object in a callback
(defun unmake_image_page_queue_entry_array (array-address)
  ;;(format t ";;; UNMAKE_IMAGE_PAGE_QUEUE_ENTRY_ARRAY ~a " array-address)
  ;;(setq *unmake_image_page_queue_entry_array-array-address* array-address)
  ;;#+never	
  (loop for array in *image_page_queue_entry_arrays*
	for addr = (%pointer array)
	when (= addr array-address)
	  do ;;(setq *unmake_image_page_queue_entry_array-array* array)
	     ;;(format t "~a~%" array)
	     (removef array *image_page_queue_entry_arrays*)
	     (unmake-stationary-vector array))
  0)

#|
(loop for array in *image_page_queue_entry_arrays*
      for addr = (%pointer array)
      when (= addr *unmake_image_page_queue_entry_array-array-address*)
	do (format t "~a~%" array))

(describe *unmake_image_page_queue_entry_array-array*)
(format t "~16r" (%pointer *unmake_image_page_queue_entry_array-array*))
|#

;;; ******************* MAKE_PAGE_HANDLER_BLOCK_MAP  *******************

(def-foreign-function (set_page_handler_block_map
			(:name (freedius-prefix "set_page_handler_block_map")))
  (page_handler :pointer)
  (block_map :simple-array) ;;(block_map :lisp)
  )

(def-foreign-function (update_paged_image_arrays
			(:name (freedius-prefix "update_paged_image_arrays")))
  (image-id :pointer) ; paged-image
  (xmap :simple-array); image-map-type
  (ymap :simple-array)
  (block_map :simple-array) ;;(block_map :lisp)
  )

(defun make_page_handler_block_map (cimage page_handler npages)
  (let* ((qffi::*prohibit-type-t-foreign-vectors* nil)
	 (block-map
	  ;; In Allegro, everything looks ok.  make-stationary-vector creates
	  ;;   stationary-vector which interacts properly with GC.
	  ;;   Must be sure something always points to each active page.
	  (make-stationary-vector npages :initial-element 0))
	 (image (image-from-cimage-id (intern-foreign-pointer cimage)))
	 )
    (declare (special qffi::*prohibit-type-t-foreign-vectors*))
    (set_page_handler_block_map page_handler block-map)
    (when image
      (setf (paged-image-block-map image) block-map)
      (set-image-struct-slots image) ; this is causing problems -- write_permit hasn't been set
      )
    (unless image (break))
    ;;(format t "make_page_handler_block_map ~a ~a ~a~%" image page_handler npages )
    ))

;;; This is called from C function make_paged_image.
(def-foreign-callable (make_lisp_paged_image
		       (:name (freedius-prefix "make_lisp_paged_image"))
		       (:return-type nil))
    ((c_image (:pointer c-image))		; address of the C paged_image struct
     (xdim :signed-32bit) 
     (ydim :signed-32bit)
     (element-type-code :signed-32bit)
     (samples-per-pixel :signed-32bit)
     (tile-offset-bits :signed-32bit)
     )
  (make_lisp_paged_image_internal c_image xdim ydim
				  element-type-code samples-per-pixel tile-offset-bits)
  0)

(def-foreign-callable (make_image_page_queue_entry_arrays
		       (:name (freedius-prefix "make_image_page_queue_entry_arrays"))
		       (:return-type nil))
    ((image_page_queue_entry dummy-pointer) ; address of the C array_image struct
     (bytes-per-page :int)
     )
  (make_image_page_queue_entry_array image_page_queue_entry bytes-per-page)
  0)

(def-foreign-callable (unmake_image_page_queue_entry_arrays
		       (:name (freedius-prefix "unmake_image_page_queue_entry_arrays"))
		       (:return-type nil))
    ((array-address dummy-pointer))
  (unmake_image_page_queue_entry_array array-address)
  0)

(def-foreign-callable (make_lisp_page_handler_block_map
		       (:name (freedius-prefix "make_lisp_page_handler_block_map"))
		       (:return-type nil))
    ((c_image (:pointer c-image)); address of the C paged_image struct
     ;;(page_handler (:pointer :void))
     ;;(page_handler (:pointer :unsigned-32bit))
     (page_handler :pointer)
     (npages :signed-32bit)
     )
  (make_page_handler_block_map c_image page_handler npages)
  0)


;;; Automatic test that the Lisp array storage representation agrees with
;;; assumptions built into LIBfreedius.

;;; fixnum_aref_T and aref_T_aref_8 are defined in $FREEDIUS/c/img/paged-image.c++
;;; which depend on definitions for LISP_ARRAY_HEADER_OFFSET and MAP_ELEMENT_SHIFT defined 
;;; in $FREEDIUS/c/include/page_handler.h and $FREEDIUS/c/include/image-types.h

(def-foreign-function (fixnum_aref_T (:name (freedius-prefix "fixnum_aref_T")) (:return-type :int))
  (arr :simple-array)
  (i :int))

(def-foreign-function (aref_T_aref_8 (:name (freedius-prefix "aref_T_aref_8")) (:return-type :int))
  (arr :simple-array) ; array must be a type T array whose elements are (unsigned-byte 8) arrays.
  (i :int)
  (j :int))

(def-foreign-function (array_address (:name (freedius-prefix "array_address"))
				     (:return-type :unsigned-int))
  (array :simple-array))

(defun compute-array-data-offset ()
  (let ((arr (make-array 1)))
    ;; LISP_ARRAY_HEADER_OFFSET = Lisp_tagged_address - C_address
    (values (- (%pointer arr) (array_address arr))
	    (%pointer arr)
	    (array_address arr))))

(def-foreign-function (set_lisp_array_header_offset
		       (:name (freedius-prefix "set_lisp_array_header_offset")))
    (offset :long)
    )

(defun set-lisp-array-header-offset ()
  (set_lisp_array_header_offset (compute-array-data-offset)))


(defun test-lisp-array-repn ()
  (flet ((test-error (name)
	   (format t "~%")
	   (format t ";;; ********************************************************************~%") ;
	   (format t ";;; TEST-LISP-ARRAY-REPN FAILED for ~a~%" name)
	   (format t ";;; ********************************************************************~%")))
    (let ((arr (make-array 10 :element-type 'image-map-element-type)))
      (loop for i from 0 below (length arr)
	    do (setf (aref arr i) i))
      (unless (loop for i from 0 below (length arr)
		    always (= i (fixnum_aref_T arr i)))
	(test-error "MAP_ELEMENT_SHIFT in fixnum_aref_T"))

      (let ((arr (make-array 2 :element-type T)))
	(loop for i from 0 below (length arr)
	      for arri = (make-array 20 :element-type '(unsigned-byte 8))
	      do (setf (aref arr i) arri)
		 (loop for j from 0 below (length arri)
		       do (setf (aref arri j) j)))
	(setq *arr* arr)
	(unless (loop for i from 0 below (length arr)
		      for arri = (aref arr i)
		      always (loop for j from 0 below (length arri)
				   always (= (aref arri j) (aref_T_aref_8 arr i j))))
	  (test-error "LISP_ARRAY_HEADER_OFFSET in aref_T_aref_8"))))))

#|
(test-lisp-array-repn)
(loop for j from 0 below (length (aref *arr* 0))  collect (aref_T_aref_8 *arr* 0 j) )
(loop for j from 0 below (length (aref *arr* 1))  collect (aref_T_aref_8 *arr* 1 j) )
(setq img (gui::view-image (gui::top-view)))
(iref img 100 100) ; broken in allegro
(let ((buf (make-integer-scan-line-buffer img)))
  (image-getline img buf 0 100)
  (aref buf 100)) ; this is correct

(compute-array-data-offset)

|#

(st::add-system-initialization :image '(set-lisp-array-header-offset))


;;;  ***********************  PAGED-IMAGE PERFORMANCE TESTS ************************
;;;                            moved to img/test-iref.lisp
