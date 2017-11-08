(in-package :img)

;;; FIXME:  This file is a mess -- needs massive cleanup

;;(config::proclaim-optimizations :fastest-unsafe)

;;; Should this move to img/paged-image.lisp?
(def-foreign-function (mkfile
		       ;; (:name (freedius-prefix "cmkfile")) ;; wrong.
		       (:name "cmkfile")
		       )
  (path :simple-string)
  (size :int)
  (units :int))

(def-foreign-function (set_default_file_image_directory 
		       (:name (freedius-prefix "set_default_file_image_directory")))
    (default_file_image_directory :simple-string))

(custom:defcustom *DEFAULT_TMP_FILE_IMAGE_DIR* 
  (or (st::getenv "CME_TMP_FILE_IMAGE_DIR")
      "/tmp/CME_TMP_PIX")
  (:type string :group :image :group :essential-settings)
  "Default directory for creating temporary file images."
  )

;(probe-file *DEFAULT_TMP_FILE_IMAGE_DIR*)
;(create-directory (pathname-as-directory *DEFAULT_TMP_FILE_IMAGE_DIR*))

(defun ensure-tmp-file-image-directory ()
  (let ((tmp-file-image-dir *DEFAULT_TMP_FILE_IMAGE_DIR*))
    (unless (probe-file tmp-file-image-dir)
      (create-directory (pathname-as-directory tmp-file-image-dir)))
    (set_default_file_image_directory tmp-file-image-dir)))

(st::add-system-initialization :image '(ensure-tmp-file-image-directory))
			       


;;; This is really a blocked_mapped_image
#+old ; Sun Jul  2 2006
(lcl::def-foreign-struct c-image
    (c++header :type :long)
  (property_list :type :long) ; really a pointer to a c_list
  (x-dim :type :long)
  (y-dim :type :long)
  (element-size :type :long) ; this slot will be flushed
  (flags :type :long)
  (element-type-code :type :long)
  (class-code :type :long)
  (samples-per-pixel :type :long)
  (x-map :type :long)
  (y-map :type :long)
  (block-x-dim :type :long)
  (block-y-dim :type :long)
  (padded-block-xdim :type :long)
  (block-size :type :long)
  (blocks-wide :type :long)
  (blocks-hi :type :long)
  )

(lcl::def-foreign-struct c-image
    (c++header :type :long) ; ??? FIXME AMD64
  (property_list :type :long) ; really a pointer to a c_list
  (x-dim :type :int)
  (y-dim :type :int)
  (flags :type :int)
  (samples-per-pixel :type :int)
  (x-map :type :long) ; pointer array
  (y-map :type :long) ; pointer array
  (block-x-dim :type :int)
  (block-y-dim :type :int)
  (padded-block-xdim :type :int)
  )

(def-foreign-function (c-image-element-size (:name (freedius-prefix "image_element_size")))
    (img (:pointer c-image)))

(def-foreign-function (c-image-element-type-code (:name (freedius-prefix "image_element_type")))
    (img (:pointer c-image)))

(def-foreign-function (c-image-class-code (:name (freedius-prefix "image_class_code")))
    (img (:pointer c-image)))

(def-foreign-function (c-image-blocks-wide (:name (freedius-prefix "image_blocks_wide")))
    (img (:pointer c-image)))

(def-foreign-function (c-image-blocks-hi (:name (freedius-prefix "image_blocks_hi")))
    (img (:pointer c-image)))

(def-foreign-function (c-image-block-size (:name (freedius-prefix "image_block_size")))
    (img (:pointer c-image)))

#|
(disassemble 'c-image-x-dim)
(disassemble 'c-image-block-x-dim)
|#

;;; FIXME  -- all of these should be accessible from Lisp image-struct.

(defun image-samples-per-pixel (image)
  (and (image-id image)
       (c-image-samples-per-pixel (image-id image))))

(defun (setf image-samples-per-pixel) (spp image)
  (and (image-id image)
       (setf (c-image-samples-per-pixel (image-id image)) spp)))

(defun image-block-x-dim (image)
  (and (image-id image)
       (c-image-block-x-dim (image-id image))))

(defun image-block-y-dim (image)
  (and (image-id image)
       (c-image-block-y-dim (image-id image))))

(defun image-padded-block-x-dim (image)
  (and (image-id image)
       (c-image-padded-block-xdim (image-id image))))

(defun image-blocks-wide (image)
  (and (image-id image)
       (c-image-blocks-wide (image-id image))))

(defun image-blocks-hi (image)
  (and (image-id image)
       (c-image-blocks-hi (image-id image))))

(defun image-block-size (image)
  (and (image-id image)
       (c-image-block-size (image-id image))))

(defun image-block-dims (image)
  (list (image-block-x-dim image) (image-block-y-dim image)))

(defun image-bitbltable (image)
  (not (logtest (c-image-flags (image-id image)) 1)))



#+never ; #+cmu
(alien::def-alien-type c-image
    (alien::struct c-image
		   (c++header c-call:int)
		   (property_list c-call:int)
		   (x-dim c-call:int)
		   (y-dim c-call:int)
		   (element-size c-call:int)
		   (flags c-call:int)
		   (element-type-code c-call:int)
		   (class-code c-call:int)
		   (x-map (array c-call:unsigned-int 10000))
		   (y-map (array c-call:unsigned-int 10000))
		   (block-x-dim c-call:int)
		   (block-y-dim c-call:int)
		   (padded-block-xdim c-call:int)
		   (block-size c-call:int)
		   (blocks-wide c-call:int)
		   (blocks-hi c-call:int)
		   ))

(defconstant IMG_UNSIGNED_1BIT 0)
(defconstant IMG_UNSIGNED_2BIT 1)
(defconstant IMG_UNSIGNED_4BIT 2)
(defconstant IMG_UNSIGNED_8BIT 3)
(defconstant IMG_UNSIGNED_16BIT 4)
(defconstant IMG_UNSIGNED_32BIT 5)
(defconstant IMG_SIGNED_1BIT 6)
(defconstant IMG_SIGNED_2BIT 7)
(defconstant IMG_SIGNED_4BIT 8)
(defconstant IMG_SIGNED_8BIT 9)
(defconstant IMG_SIGNED_16BIT 10)
(defconstant IMG_SIGNED_32BIT 11)
(defconstant IMG_SINGLE_FLOAT 12)
(defconstant IMG_DOUBLE_FLOAT 13)
(defconstant IMG_RGB8 14)
(defconstant IMG_RGBA8 15)
(defconstant IMG_ANY 16)
;;; mapping to C type codes.  See $FREEDIUS/c/include/image-types.h

(#-sbcl defconstant #+sbcl defvar *image-element-type-index-alist*
	`(((unsigned-byte 1) . ,IMG_UNSIGNED_1BIT)
	  (bit . ,IMG_UNSIGNED_1BIT)
	  ((unsigned-byte 2) . ,IMG_UNSIGNED_2BIT)
	  ((unsigned-byte 4) . ,IMG_UNSIGNED_4BIT)
	  ((unsigned-byte 8) . ,IMG_UNSIGNED_8BIT)
	  ((unsigned-byte 16) . ,IMG_UNSIGNED_16BIT)
	  ((unsigned-byte 32) . ,IMG_UNSIGNED_32BIT)
	  ((signed-byte 1) . ,IMG_SIGNED_1BIT)
	  ((signed-byte 2) . ,IMG_SIGNED_2BIT)
	  ((signed-byte 4) . ,IMG_SIGNED_4BIT)
	  ((signed-byte 8) . ,IMG_SIGNED_8BIT)
	  ((signed-byte 16) . ,IMG_SIGNED_16BIT)
	  ((signed-byte 32) . ,IMG_SIGNED_32BIT)
	  (single-float . ,IMG_SINGLE_FLOAT)
	  (double-float . ,IMG_DOUBLE_FLOAT)
	  (RGB8 . ,IMG_RGB8)
	  (RGBA8 . ,IMG_RGBA8)
	  (t . ,IMG_ANY)))

(defun l2c-element-type (lisp-array-element-type)
  (cdr (assoc lisp-array-element-type *image-element-type-index-alist* :test #'equal)))

(defun c2l-element-type (element-type-code)
  (or (car (rassoc element-type-code *image-element-type-index-alist* :test #'equal))
      (error "c2l-element-type unknown element-type-code = ~a" element-type-code)))

;(l2c-element-type '(unsigned-byte 8))


(defvar *image-element-type-alist* *image-element-type-index-alist*)

(defvar *image-element-type-to-c-name-alist*
    '((bit "BIT")
      ((unsigned-byte 1) "BIT")
      ((unsigned-byte 8) "UNSIGNED-8BIT")
      ((unsigned-byte 16) "UNSIGNED-16BIT")
      ((signed-byte 8) "SIGNED-8BIT")
      ((signed-byte 16) "SIGNED-16BIT")
      ((signed-byte 32) "SIGNED-32BIT")
      ((unsigned-byte 32) "UNSIGNED-32BIT")
      (single-float "SINGLE-FLOAT")
      (double-float "DOUBLE-FLOAT")
      (RGB8 "RGB8")
      (RGBA8 "RGBA8")
      ))

(defun describe-c-image (cimg &optional (stream *standard-output*))
  (format stream "#<C-IMAGE #X~16,8,Vr> is an ALIEN structure of type C-IMAGE~%" #\0 (foreign-pointer-address cimg))
  (format stream "X-DIM: ~D~%" (c-image-x-dim cimg))
  (format stream "Y-DIM: ~D~%" (c-image-y-dim cimg))
  (format stream "ELEMENT-TYPE: ~a~%"
	  (car (rassoc (c-image-element-type-code cimg) *image-element-type-index-alist*)))
  (format stream "BLOCK-X-DIM: ~D~%" (c-image-block-x-dim cimg))
  (format stream "BLOCK-Y-DIM: ~D~%" (c-image-block-y-dim cimg)))

;;; ********************  FOREIGN FUNCTIONS  ********************

 
(def-foreign-function (array_image_size_limit (:name (freedius-prefix "array_image_size_limit")))
    (size :int))


(def-foreign-function (make_image (:name (freedius-prefix "make_image"))
				  (:without-gcing nil)
				  (:return-type (:pointer c-image)))
  (xdim :int)
  (ydim :int)
  (element-type-code :int)
  (samples-per-pixel :int))

(def-foreign-function (make_image_blocked (:name (freedius-prefix "make_image_blocked"))
					  (:without-gcing nil)
				  (:return-type (:pointer c-image)))
    (xdim :int)
  (ydim :int)
  (element-type-code :int)
  (samples_per_pixel :int)
  (block-x-dim :int)         ; 0 forces default
  (block-y-dim :int)         ; 0 forces default
  (padded-block-xdim :int)   ; 0 forces same as block-x-dim
  )


(def-foreign-function (make_array_image (:name (freedius-prefix "make_array_image"))
					(:without-gcing nil)
					(:return-type (:pointer c-image)))
  (xdim :int)
  (ydim :int)
  (element-type-code :int)
  (samples_per_pixel :int)
  (block-x-dim :int)         ; 0 forces default
  (block-y-dim :int)         ; 0 forces default
  (padded-block-xdim :int)   ; 0 forces same as block-x-dim
  )


(def-foreign-function (make_file_image (:name (freedius-prefix "make_file_image"))
				       (:without-gcing nil)
					(:return-type (:pointer c-image)))
  (xdim :int)
  (ydim :int)
  (element-type-code :int)
  (samples_per_pixel :int)
  (block-x-dim :int)         ; 0 forces default
  (block-y-dim :int)         ; 0 forces default
  (padded-block-xdim :int)   ; 0 forces same as block-x-dim
  ;; was :simple-string, which causes an error
  (pathname :simple-array) ; or NULL
  )

(def-foreign-function (make_tiff_file_image (:name (FREEDIUS-PREFIX "make_tiff_file_image"))
					    (:without-gcing nil)
					    (:return-type  (:pointer c-image)))
  (xdim :int)
  (ydim :int)
  (element_type :int)
  (spp :int)
  (block_xdim :int)
  (block_ydim :int)
  (padded_block_xdim :int)
  ;; was :simple-string, which causes an error
  ;;(path :simple-array)
  (path :simple-string) ; Thu Jul  9 2009 -- this appears to be needed, otherwise filename isn't passed.
  )

(def-foreign-function (make_tiff_lazy_image (:name (FREEDIUS-PREFIX "make_tiff_lazy_image"))
					    (:without-gcing nil)
					    (:return-type  (:pointer c-image)))
    (path :simple-string)
  (xdim :int)
  (ydim :int)
  (element_type :int)
  (block_xdim :int)
  (block_ydim :int)
  (format :int)
  (spp :int)
  (photometric-type :int)
  )


#|
(setq forestcat (load-image "$HOME/pix/forestcat.tif"))
(setq cimg (make-tiff-file-image "/tmp/pix/test5.tif" (image-x-dim forestcat) (image-y-dim forestcat)
				 :block-x-dim 64 :block-y-dim -64 :element-type 'rgb8 ))

(setq cimg (make-file-image "/tmp/pix/test5b.tif" (image-x-dim forestcat) (image-y-dim forestcat)
				 :block-x-dim 64 :block-y-dim -64 :element-type 'rgb8 ))


(copy-image forestcat cimg)
(gui::push-image cimg (gui::selected-window))
(save-image cimg "/tmp/pix/test5b.tif" :compression-mode :jpeg :jpeg-quality 75)
(save_untiled_tiff_image (image-id cimg) "/tmp/pix/test5b.tif" 0 COMPRESSION_JPEG 75)
(release_image_pages (image-id (band-interleaved-image cimg)))

(setq rugby (load-image "$HOME/pix/rugby.pic"))
(setq img (make-tiff-file-image "/tmp/pix/test4.tif" (image-x-dim rugby) (image-y-dim rugby)
				:block-x-dim 64 :block-y-dim -64))
(copy-image rugby img)
(gui::push-image img (gui::selected-window))

(save-image rugby "/tmp/pix/rugby-save.tif")
(save-image forestcat "/tmp/pix/forestcat-save-jpeg.tif" :compression-mode :jpeg :jpeg-quality 75)
(save-image forestcat "/tmp/pix/forestcat-save-packbits.tif" :compression-mode :packbits)
(save-image forestcat "/tmp/pix/forestcat-save.tif")
|#

#+unused
(def-foreign-function (make_band_interleaved_array_image
			(:name (freedius-prefix "make_band_interleaved_array_image"))
			(:without-gcing  nil)
			(:return-type (:pointer c-image)))
  (xdim :int)
  (ydim :int)
  (element-type-code :int)
  (samples-per-pixel :int)
  (block-x-dim :int)         ; 0 forces default
  (block-y-dim :int)         ; 0 forces default
  (padded-block-xdim :int)   ; 0 forces same as block-x-dim
  )

#+unused
(def-foreign-function (make_band_interleaved_paged_image
			(:name (freedius-prefix "make_band_interleaved_paged_image"))
			(:without-gcing  nil)
			(:return-type (:pointer c-image)))
  (xdim :int)
  (ydim :int)
  (element-type-code :int)
  (samples-per-pixel :int)
  (block-x-dim :int)         ; 0 forces default
  (block-y-dim :int)         ; 0 forces default
  (padded-block-xdim :int)   ; 0 forces same as block-x-dim
  )

#|
(array_image_size_limit 0) ;1536000
(array_image_size_limit (* 1024 1024))
|#


;;; defined in cimage.c++ 
(def-foreign-function (_interpolate-iref (:name (freedius-prefix "interpolate_iref"))
					(:return-type :double-float))
    (img (:pointer c-image))
  (x :double-float)
  (y :double-float))

;;; FIXME?  Need to compare speed of calling thru FFI vs. doing this in Lisp code.
(defun interpolate-iref (image x y)
  (_interpolate-iref (image-id image) x y))



;;; ******************************* WARNING *******************************
;;; These getline/putline methods are unsafe for paged-images unless the buffer
;;; is allocated in a stationary area or using make-foreign-vector.  The
;;; paged-image methods might cause a call to malloc that triggers a Lisp GC.
;;; An examination of the paged-image related code indicates that get_page
;;; doesn't make any calls to malloc.  Be careful if any code is added to detect
;;; page thrashing and grow the page-pool.  GC might really only be a problem
;;; for lazy-images whose tile builder call calls malloc or performs a callback
;;; to Lisp.
;;; ******************************* WARNING *******************************

(def-foreign-function (image_getline (:name (freedius-prefix "image_getline"))
				     (:return-type :int)
				     (:arg-checking nil))
    (img (:pointer c-image))
  (buffer_type :int) 
  (buf :array)		
  (x :int)
  (y :int)
  (n :int)
  (to-index :int)
  (dx :int)
  (band :int))

(def-foreign-function (image_putline (:name (freedius-prefix "image_putline"))
				     (:return-type :int)
				     (:arg-checking nil))
    (img (:pointer c-image))
  (buffer_type :int) 
  (buf :array)		
  (x :int)
  (y :int)
  (n :int)
  (to-index :int)
  (band :int))

(def-foreign-function (image_getcolumn (:name (freedius-prefix "image_getcolumn"))
				       (:return-type :int)
					    (:arg-checking nil))
    (img (:pointer c-image))
  (buffer_type :int) 
  (buf :array)		
  (x :int)
  (y :int)
  (n :int)
  (to-index :int)
  (dy :int)
  (band :int))

(def-foreign-function (image_putcolumn (:name (freedius-prefix "image_putcolumn"))
				       (:return-type :int)
				       (:arg-checking nil))
    (img (:pointer c-image))
  (buffer_type :int) 
  (buf :array)		
  (x :int)
  (y :int)
  (n :int)
  (to-index :int)
  (band :int))

(def-foreign-function (image_get_rectangle  (:name (FREEDIUS-PREFIX "image_get_rectangle")))
    (img (:pointer img::c-image))
  (buf-element-type :int)
  (buf :array)	
  (xleft :int)
  (ybot :int)
  (nx :int)
  (ny :int)
  (nbands :int)
  (buf_start :int)
  (buf_row_length :int)
  )
 
(def-foreign-function (image_get_rectangle_bordered  
		       (:name (FREEDIUS-PREFIX "image_get_rectangle_bordered")))
    (img (:pointer img::c-image))
  (buf-element-type :int)
  (buf :array)	
  (xleft :int)
  (ybot :int)
  (nx :int)
  (ny :int)
  (border :int)
  (nbands :int)
  (buf_start :int)
  (buf_row_length :int)
  )

(def-foreign-function (image_put_rectangle  (:name (FREEDIUS-PREFIX "image_put_rectangle")))
    (img (:pointer img::c-image))
  (buf-element-type :int)
  (buf :array)	
  (xleft :int)
  (ybot :int)
  (nx :int)
  (ny :int)
  (nbands :int)
  (buf_start :int)
  (buf_row_length :int)
  )

#+never  ;; not used
(def-foreign-function  (init_image_io (:name (freedius-prefix "init_image_io")))
    (flags :int)
  (default_file_image_directory :simple-string))

(def-foreign-function (load_image (:name (freedius-prefix "load_image"))
				   (:return-type (:pointer c-image)))
    #-cmu19c (path :simple-string)
    #+cmu19c (path :simple-array)
  )


(def-foreign-function (read_property_list_string
			(:name (freedius-prefix "read_property_list_string"))
			(:return-type :simple-string))
    #-cmu19c (path :simple-string)
    #+cmu19c (path :simple-array)
  )

(defun read-image-property-list-string (file-name)
  (read_property_list_string (namestring (truename (lx::ev-pathname-translate file-name)))))


(def-foreign-function (reload_image (:name (freedius-prefix "reload_image"))
				     (:return-type (:pointer c-image)))
  (path :simple-string)
  (image-id (:pointer c-image))
  )


(def-foreign-function (recognize_image_header (:name (freedius-prefix "recognize_image_header"))
					      (:call-direct nil))
   (path :simple-string) )

(def-foreign-function (read_image_header (:name (freedius-prefix "read_image_header"))
					     (:call-direct nil)
					     (:return-type (:pointer c-image-header)))
   (path :simple-string) )

#|
(read_image_header "/m/scans/digital-camera/20080530/IMG_0072.JPG")
(read_image_header "/m/scans/digital-camera/20080530/IMG_0073.JPG")
(read_image_header "/opt/IU/radius/site-2d-worlds/Olalla/7189_69/image.g0")
(tiff-file-header-params "/opt/IU/radius/site-2d-worlds/Olalla/7189_69/image.g0")
((UNSIGNED-BYTE 8) 15240 15240 256 -256 8)

(truename "/opt/IU/radius/site-2d-worlds/Olalla/7189_69/image.g0")
(recognize-image-header "/opt/IU/radius/site-2d-worlds/Olalla/7189_69/image.g0")
|#

(defun recognize-image-header (path)
  (not (= 0 (recognize_image_header path))))

;int FREEDIUS_GLOBAL(tiff_file_header_params) (char *path, int params[10]) {

(def-foreign-function (tiff_file_header_params (:name (freedius-prefix "tiff_file_header_params")))
    (path :simple-string)
  (params :simple-array) ; (array int 6) element_type_code xdim ydim blk-xdim blk-ydim file-offset
  )

(defun tiff-file-header-params (path)
  (let ((params (make-array 7 :element-type '(signed-byte 32))))
    (if (zerop (tiff_file_header_params path params))
	nil
	(list* (car (rassoc (aref params 0) *image-element-type-index-alist*))
	      (loop for i from 1 below (length params) collect (aref params i)))))) 
    
(def-foreign-function (save_image (:name (freedius-prefix "save_image")))
    (img (:pointer c-image))
  (path :simple-string))


(def-foreign-function (save_tiff_image (:name (freedius-prefix "save_tiff_image")))
    (img (:pointer c-image))
  (path :simple-string)
  (error-ok :int))


(def-foreign-function (save_iu_testbed_image (:name (freedius-prefix "save_iu_testbed_image")))
    (img (:pointer c-image))
  (path :simple-string)
  (error-ok :int))

(def-foreign-function (set_save_image_default_format
			(:name (freedius-prefix "set_save_image_default_format"))
			(:return-type :int))
  (format :int)) ; 1= iu-testbed, 2 = TIFF

(defun save-image-default-format ()
  (let ((format (set_save_image_default_format 1)))
    (set_save_image_default_format format)
    (case format
      (1 :iu-testbed)
      (2 :tiff))))
      
(defun (setf save-image-default-format) (format)
  (case format
    (:iu-testbed (set_save_image_default_format 1))
    (:tiff (set_save_image_default_format 2))
    (otherwise (error "setf save-image-default-format illegal format: ~a" format)))
  format)

(def-foreign-function (set_file_image_default_format
			(:name (freedius-prefix "set_file_image_default_format"))
			(:return-type :int))
  (format :int)) ; 1= iu-testbed, 2 = TIFF

(defun file-image-default-format ()
  (let ((format (set_file_image_default_format 1)))
    (set_file_image_default_format format)
    (case format
      (1 :iu-testbed)
      (2 :tiff))))
      
(defun (setf file-image-default-format) (format)
  (case format
    (:iu-testbed (set_file_image_default_format 1))
    (:tiff (set_file_image_default_format 2))
    (otherwise (error "setf file-image-default-format illegal format: ~a" format)))
  format)


(def-foreign-function (save_untiled_tiff_image (:name (freedius-prefix "save_untiled_tiff_image")))
  (img (:pointer c-image))
  (path :simple-string)
  (error_ok :int)
  (compression-mode :int) ; COMPRESSION_NONE COMPRESSION_PACKBITS COMPRESSION_JPEG COMPRESSION_LZW
  (jpeg-quality :int)
  )

(def-foreign-function (gauss_convolve_decimate (:name (freedius-prefix "gauss_convolve_decimate"))
						(:return-type (:pointer c-image)))
    (img (:pointer c-image))
    (ka :double-float))


(defmethod gauss-convolve-image (img &optional (ka .375))
  (make-image-from-cimage
   (gauss_convolve_decimate (image-id img) ka)))

(def-foreign-function (image_filter_decimate2 (:name (freedius-prefix "image_filter_decimate2"))
						(:return-type (:pointer c-image)))
    (img (:pointer c-image))
    (pyramid-type :int)
    (into-img (:pointer c-image))
    )

#|
typedef enum _image_pyramid_filter_type_ {
  IMAGE_PYRAMID_DEFAULT_FILTER,
  IMAGE_PYRAMID_BOX_FILTER,
  IMAGE_PYRAMID_GAUSS_FILTER_MIN_BLUR,
  IMAGE_PYRAMID_GAUSS_FILTER_MIN_ALIAS
} image_pyramid_filter_type;
|#

(defconstant *IMAGE_PYRAMID_DEFAULT_type* 0)
(defconstant *IMAGE_PYRAMID_BOX_FILTER_type* 1)
(defconstant *IMAGE_PYRAMID_GAUSS_FILTER_MIN_BLUR_type* 2)

#+old
(defvar *null-pointer*
  #+(and :CMUCL-POINTER-TYPES :cmu) (alien::int-sap 0)
  #+(and :CMUCL-POINTER-TYPES :sbcl) (sb-alien::int-sap 0)
  #-CMUCL-POINTER-TYPES 0)
  
(defvar *null-pointer*
  #+(or cmu sbcl) (qffi::int-sap 0)
  #-(or cmu sbcl) 0)
  
(defun image-filter-decimate2 (image pyramid-type &optional into-image)
  (setq *image-filter-decimate2-args* (list image pyramid-type into-image))
  (let ((image2 (make-image-from-cimage
		 (image_filter_decimate2 (image-id image)
					 (case pyramid-type
					   (box-2 1)
					   (gauss-2 2) ; min blur
					   (gauss-2-min-alias 3)
					   (otherwise 0))
					 (if into-image
					     (image-id into-image)
					     *null-pointer*)))))
    (unless into-image
      (inherit-properties image2 image)
      (rem-prop image2 :image-to-2d-transform))	;; transform must be set by caller
    image2))
    

(def-foreign-function (fast_gauss_convolve (:name (freedius-prefix "fast_gauss_convolve"))
					    (:return-type (:pointer c-image)))
  (img (:pointer c-image))
  (into (:pointer c-image))
  (level :int)
  (ka :double-float)
  (scratch (:pointer c-image)))

(def-foreign-function (image_page_pool_stats (:name (freedius-prefix "image_page_pool_stats")))
    (img (:pointer c-image))
  (resetp :int)  ;&optional (resetp :int 1) ; what is the correct syntax here ?
  )

(def-foreign-function (all_image_page_pool_stats (:name (freedius-prefix "all_image_page_pool_stats")))
  (resetp :int)  ;&optional (resetp :int 1) ; what is the correct syntax here ?
  )

(def-foreign-function (release_image_pages (:name (freedius-prefix "release_image_pages")))
    (img (:pointer c-image)))


(def-foreign-function (resize_cimage_page_pool (:name (freedius-prefix "resize_image_page_pool")))
    (img (:pointer c-image))
  (pool-size-change-count :int))

(def-foreign-function (get_page_pool (:name (freedius-prefix "get_page_pool")) )
    (bytes_per_page :int)
  (pool-size :int))

(def-foreign-function (add_to_working_set (:name (freedius-prefix "add_to_working_set"))
					  (:return-type :int))
  (image (:pointer c-image))
  (npages :int)
  (nrows :int)
  (ncols :int))

(def-foreign-function (remove_from_working_set (:name (freedius-prefix "remove_from_working_set"))
					  (:return-type :int))
  (image (:pointer c-image))
  (npages :int)
  (nrows :int)
  (ncols :int))

(defun add-to-working-set (image &key (npages 0) (nrows 1) (ncols 0))
  (add_to_working_set (image-id image) npages nrows ncols))

(defun remove-from-working-set (image &key (npages 0) (nrows 1) (ncols 0))
  (remove_from_working_set (image-id image) npages nrows ncols))



(def-foreign-function (lazy_retile_image (:name (freedius-prefix "lazy_retile_image"))
					   (:return-type (:pointer c-image)))
    (img (:pointer c-image))
  (bx :int)
  (by :int)
  (element_type  :int)
  (map :simple-array-or-null) ;(map :int) ; 0 or pointer to map of type element_type
  )

;;;This doesn't work in CMUCL:  doesn't allow displacement into a array with a different element-type.
;;;(defun machine-endian ()
;;;  (let* ((a8 (make-array 2 :element-type '(unsigned-byte 8)))
;;;         (a16 (make-array 1 :element-type '(unsigned-byte 16) :displaced-to a8)))
;;;    (setf (aref a16 0) 1)
;;;    (if (= (aref a8 0) 1) :LITTLE-ENDIAN :BIG-ENDIAN))))


#|
;;; This isn't really used.
(def-foreign-function (machine_endian (:name (freedius-prefix "machine_endian"))
				      (:return-type :int)))

;;; This isn't really used.
(defun machine-endian ()
  (if (= (machine_endian) 0)
      :LITTLE-ENDIAN
      :BIG-ENDIAN))
|#		       

;;(lx::load-shared-libraries '("-liflTIFF" ))


(def-foreign-function (rgb8_to_yuv_images
		       (:name (freedius-prefix "rgb8_to_yuv_images"))
		       (:return-type (:pointer c-image)))
  (rgb-in (:pointer c-image))
  (y-out (:pointer c-image))
  (u-out (:pointer c-image))
  (v-out (:pointer c-image)))


;;; *************************  FUNCTIONS AND METHODS  *************************

#|
;;; Why can we not use typedef as follows:
(deftype c-image () 'integer)

>>Error: Illegal parameter-specializer-name: C-IMAGE
LIQUID-RUNTIME-SUPPORT:DEFINE-METHOD
...

(deftype c-image2 () 'foreign-pointer)

(defmethod cimage-element-type ((image-id c-image2))
  (car (rassoc (c-image-element-type-code image-id)
	       *image-element-type-index-alist*)))

;;; This is fine.
(defmethod cimage-element-type ((image-id integer))
  (car (rassoc (c-image-element-type-code image-id)
	       *image-element-type-index-alist*)))
|#

(eval-when (eval load compile)
(defun map-argument-specializers (arglist class-mapping-alist)
  (loop for arg in arglist
	collect (if (not (consp arg))
		    arg
		    (list (car arg)
			  (or (cdr (assoc (cadr arg) class-mapping-alist))
			      (cadr arg))))))
)
	
;;(map-argument-specializers '((image cimage)) '((cimage . foreign-pointer)))

;;; For some reason neither deftype nor symbol-macrolet can be used
;;; to allow renaming of a parameter-specializer to defmethod.

#|

Sun Dec  9 2001 - CMUCL aliens do not work with generic-function dispatch.
Therefore, either must wrap the alien with some kind of struct, or just
give up and abandon alien images altogether.  (That is the ultimate plan)

(defvar *c-image-type-spec* (type-of (alien:make-alien c-image)))

(progn *c-image-type-spec*)
(typep (alien:make-alien c-image) *c-image-type-spec*)
(image-type (alien:make-alien c-image))

(defmethod foo ((x (eql *c-image-type-spec*)))
  t)
(foo (alien:make-alien c-image))
|#

(defun c-image-class (c-image samples-per-pixel)
  (if (> samples-per-pixel 1)
      (case (c-image-class-code c-image)
	(1 'band-interleaved-array-image)
	;;    (2 'file-image) ;; eh?
	(2 'band-interleaved-paged-image) ;; eh?
	(3 'band-interleaved-lazy-image))
      
      (case (c-image-class-code c-image)
	(1 'array-image)
	;;    (2 'file-image) ;; eh?
	(2 'paged-image) ;; eh?
	(3 'lazy-image))))

(defun cimage-element-type (image-id)
  (c2l-element-type (c-image-element-type-code image-id)))


(defvar *c-image-id-hash-table* (make-hash-table))

#+never
(progn

#+sbcl
(defun image-id-hash-key (image-id)
  (sb-kernel::sap-int (SB-ALIEN-INTERNALS:ALIEN-VALUE-SAP image-id)))

#+cmu
(defun image-id-hash-key (image-id)
  (kernel::sap-int (ALIEN-INTERNALS:ALIEN-VALUE-SAP image-id)))

) ; end progn

(defun image-id-hash-key (image-id)
  (foreign-pointer-address image-id))

;;; These may be redefined in image-gc.lisp
(defun make-cimage-id-entry (image-id image)
  (setf (gethash (image-id-hash-key image-id) *c-image-id-hash-table*) image))

(defun image-from-cimage-id (image-id)
  (gethash (image-id-hash-key image-id) *c-image-id-hash-table*))


(defun make-image-from-cimage (image-id &key (null-ok nil) image-class (samples-per-pixel 1))
  (let* ((handle (intern-foreign-pointer image-id))
	 (image 
	  (cond (handle
		 (or (image-from-cimage-id handle)
		     (make-cimage-id-entry 
		      handle 
		      (make-instance (or image-class (c-image-class image-id samples-per-pixel))
				     :x-dim (c-image-x-dim image-id)
				     :y-dim (c-image-y-dim image-id)
				     ;;:x-map (c-image-x-map image-id)
				     ;;:y-map (c-image-y-map image-id)
				     :element-type (cimage-element-type image-id)
				     :id handle
				     :nbands samples-per-pixel
				     ))))
		(null-ok nil)
		(t (error "Cannot make image from NULL pointer.")))))
    ;;(setq *make-image-from-cimage-last-image* image)
    image))

;;; (defun make-image-from-cimage (image-id &key (null-ok nil) image-class (samples-per-pixel 1))
;;;   (let* ((handle (intern-foreign-pointer image-id))
;;; 	 (image 
;;; 	  (cond (handle
;;; 		 (or (gethash handle *c-image-id-hash-table*)
;;; 		     (make-cimage-id-entry 
;;; 		      handle
;;; 		      (let ((image-class
;;; 			     (or image-class (c-image-class image-id samples-per-pixel))))
;;; 			;;(break "make-image-from-cimage -- unknown image")
;;; 			(make-instance image-class
;;; 				       :x-dim (c-image-x-dim image-id)
;;; 				       :y-dim (c-image-y-dim image-id)
;;; 				       ;;:x-map (c-image-x-map image-id)
;;; 				       ;;:y-map (c-image-y-map image-id)
;;; 				       :element-type (cimage-element-type image-id)
;;; 				       :id handle
;;; 				       :nbands samples-per-pixel
;;; 				       )))))
;;; 		(null-ok nil)
;;; 		(t (error "Cannot make image from NULL pointer.")))))
;;;     ;;(setq *make-image-from-cimage-last-image* image)
;;;     image))

(defun wrap-foreign-image (cimage &key image-class (samples-per-pixel 1))
  (let ((image-id (intern-foreign-pointer cimage)))
    (when (zerop (foreign-pointer-address image-id))
      (error "wrap-foreign-image failed"))
    (make-image-from-cimage image-id :image-class image-class :samples-per-pixel samples-per-pixel)))

(defun make-foreign-image (dims &key element-type image-type
			   block-x-dim block-y-dim
			   padded-block-x-dim
			   (samples-per-pixel 1)
			   &allow-other-keys)
  (unless element-type (setq element-type '(unsigned-byte 8)))
  (let* ((xdim (car dims))
	 (ydim (cadr dims))
	 (element-type-code (or (cdr (assoc element-type
					    *image-element-type-index-alist*
					    :test 'equal))
				(error "illegal element-type")))
	 (cimage (if (and block-x-dim block-y-dim)
		     (make_image_blocked xdim ydim element-type-code samples-per-pixel
					 block-x-dim block-y-dim
					 (or padded-block-x-dim block-x-dim))
		     (make_image xdim ydim element-type-code samples-per-pixel))))
    (wrap-foreign-image cimage :samples-per-pixel samples-per-pixel)))


;;; this is redefined in vector-image.lisp
(defmethod load-image-wrap-image ((image image))
  image)


(defun load-image-property-list (image &optional (pathname (get-prop image :pathname)))
  (when pathname
    (let ((properties (parse-property-list-string
		       (read-image-property-list-string pathname))))
      (loop for (key val) on properties by #'cddr
	    do (setf (get-prop image key) val))
      properties)))



;;; I don't like this, or rather, I'm not sure where this should be
;;; done.  This allows 16 and 32-bit images to be scaled so that their
;;; 8-bit tiles have reasonable dynamic range for display:

;;; Not sure photometric-transform code belongs in this file

(declaim (special *default-photometric-transform*))

(defun compute-photometric-transform-from-clip-levels (min max)
  (destructuring-bind (type gain offset) *default-photometric-transform*
    (let ((scale (/ gain (max 1.0 (- max min)))))
      (list :linear scale (+ offset (- (* scale min)))))))

(defun compute-photometric-transform-from-image-min-max (image)
  (mv-bind (nmin nmax) (img::normalized-image-element-min-max image)
      (compute-photometric-transform-from-clip-levels nmin nmax)))

(defun maybe-compute-image-photometric-transform (image)
  (when (and (null (image-prop image :photometric-transform))
	     (scalar-image-p image)
	     ;; A hack, for now.  Should somehow designate band-interleaved images as non-scalar:
	     (not (typep image 'img::band-interleaved-paged-image))
	     (not (typep image 'img::band-interleaved-array-image))
	     (not (get-prop image :display-scale-offset))
	     (not (eql (image-element-size image) 8)))
    (setf (image-prop image :photometric-transform)
	  (compute-photometric-transform-from-image-min-max image))))

#+unused
(defun set-pyramid-dynamic-range (image min max)
  (let ((pyramid (get-prop image :pyramid)))
    (if (null pyramid)
	(set-image-dynamic-range image min max)
	(with-class-slots image-pyramid (levels) pyramid
	    (loop for im across levels
		  when im
		    do (set-image-dynamic-range im min max))))))

(defparameter *load-image-read-property-list* nil)

;;; FIXME: canonicalize the path.  Use eval-cache internally rather than using defun-cached.
(defun-cached load-image (path &key (read-property-list *load-image-read-property-list*) )
  (let ((cimage-id (load_image (namestring (truename (lx::ev-pathname-translate path))))))
    (when (and cimage-id (zerop (foreign-pointer-address cimage-id))) (setq cimage-id nil))
    (unless cimage-id
      (error "load-image failed to load ~a~%" path))
    (let ((image (load-image-wrap-image (make-image-from-cimage cimage-id))))
      (setf (image-prop image :pathname) path)
      (when read-property-list
	(load-image-property-list image path))
      ;; Perhaps this should happen at the first attempt to display an image 
      ;; that needs a photometric-transform rather than now.
     ; (maybe-compute-image-photometric-transform image)
      #+never
      (loop for i from 0 below 5
	    for img = (get-image-pyramid-level image i)
	    do (maybe-compute-display-scale-offset img))
      image)))

(defmethod reload-image ((path string) image)
  (reload_image path (image-id image))
  image)

(defmethod reload-image ((path pathname) image)
  (reload_image (namestring path) (image-id image))
  image)

;;; PROPERTY-LISTS ARE CURRENTLY NOT SAVED --- BIG LOSE.

;;; defined in tiff-ffi.lisp
(declaim (special COMPRESSION_LZW COMPRESSION_PACKBITS COMPRESSION_NONE))

(defmethod save-image ((image scalar-image) path &key compression-mode jpeg-quality error_ok
		       &allow-other-keys)
  (let* ((path (namestring (lx::ev-pathname-translate path)))
	 )
    (if (or compression-mode jpeg-quality)
	(save_untiled_tiff_image (image-id image) path (if error_ok 1 0)
				 (case compression-mode
				   (:jpeg COMPRESSION_JPEG)
				   (:lzw COMPRESSION_LZW)
				   (:PACKBITS COMPRESSION_PACKBITS)
				   (:otherwise COMPRESSION_NONE))
				 (or jpeg-quality 100))
	(save_image (image-id image) path))
    image))


;;; 6/6/05 - added a condition handler here.  Intermittent error:
;;; trailing garbage appeared in the property-list string of some DEM
;;; files.  I could not zero these out, and am not sure whether or not
;;; they are artifacts of the IU-TESTBED format.  In any case, as far
;;; as I can tell, such garbage only appears at the end of the string,
;;; by which time we have consumed all the items of interest.  So I
;;; return the EOF code here:

(defun read-from-string-ignoring-package-not-found-errors (&rest args)
  (condition-case ()
      (apply #'read-from-string args)
    (reader-error (values (third args) nil t))))


;;(proclaim '(optimize (compilation-speed 3) (speed 0) (safety 3)))

(defvar *last-image-file-property-list-string* nil)
(defvar *load-image-inhibit-evaluating-property-list-slots* nil)
(defvar *image-file-format-read-property-list-inhibit-eval* nil)
(defvar *keyword-package* (find-package :keyword))

(defparameter *image-file-properties-not-to-dump*
  '(:base-image :inferiors :time-of-last-access  :time-of-creation :equal-hash-code :octant-map
    :pyramid :linear-geom-transform))

(defun parse-property-list-string (string)
  (when string
    (let ((*package* (find-package "USER"))
          (*read-base* 10.))
      (loop with pos = 0
            with eof = (list nil)
            with property-name-error-p and property-value-error-p
            for eval-error-p = nil
            ;; property-names are always in *keyword-package*
            with ignore 
            for name = (let ((*package* *keyword-package*))
                         (multiple-value-setq (ignore pos property-name-error-p)
                           (read-from-string-ignoring-package-not-found-errors
                            string nil eof :start pos)))
            until (eq name eof)
            for delim = (multiple-value-setq (ignore pos)
                          (read-from-string string nil eof :start pos))
            for value = (multiple-value-setq (ignore pos property-value-error-p)
                          (read-from-string-ignoring-package-not-found-errors
                           string nil eof :start pos))
            do (ignore ignore property-name-error-p property-value-error-p)
		    
            when (eq value 'cl-user::eval)
              do (setq delim 'cl-user::eval
                       value (multiple-value-setq (ignore pos property-value-error-p)
                               (read-from-string-ignoring-package-not-found-errors
                                string nil eof :start pos)))
            when (memq name *image-file-properties-not-to-dump*)
              do (setq eval-error-p t)
            when (and (eq delim 'cl-user::eval) (not eval-error-p)
                      (not *image-file-format-read-property-list-inhibit-eval*))
              do (setq value (condition-case () (eval value)
                               (error	;(setq eval-error-p t)
                                `(eval ,value))))
            unless (or ;; property-name-error-p  ; add it to the property-list anyway
                       ;; property-value-error-p
                       eval-error-p)
              collect name and collect value))))
#|
(setq pl (parse-property-list-string pl-string))
(describe (getf pl :IMAGE-TO-2D-TRANSFORM ))
(describe (getf pl :2d-world ))
|#

(defun lisp2cboolean (x)
  (cond ((numberp x)
	 x)
	((eq x nil) 0)
	(t 1)))
 
(defun resize-image-page-pool (image pool-size-change-count)
  (resize_cimage_page_pool (image-id image) pool-size-change-count))
  

;;; ********************  GETLINE / PUTLINE  ********************

(defun canonical-array-element-type (type-spec)
  (array-element-type (make-array 1 :element-type type-spec)))

(defvar *image-int-buffer-type* (canonical-array-element-type 'image-int-buffer-element-type))
(defvar *image-float-buffer-type* (canonical-array-element-type 'image-float-buffer-element-type))

(defun scalar-image-getline-buffer-type-code (buf-element-type)
  (cond ((equal buf-element-type 'double-float) IMG_DOUBLE_FLOAT)
	((equal buf-element-type 'single-float) IMG_SINGLE_FLOAT)
	((equal buf-element-type '(signed-byte 32)) IMG_SIGNED_32BIT)
	((equal buf-element-type '(unsigned-byte 8)) IMG_UNSIGNED_8BIT)
	((equal buf-element-type '(unsigned-byte 16)) IMG_UNSIGNED_16BIT)
	((equal buf-element-type '(signed-byte 16)) IMG_SIGNED_16BIT)
	(t (error "unsupported buffer element-type ~a" buf-element-type))))

(defun getline-buffer-type-code (image buffer)
  (let ((buf-element-type (array-element-type buffer)))
    (or (case (array-rank buffer)
	  (1 (cond ((eq buf-element-type 'double-float) IMG_DOUBLE_FLOAT)
		   ((eq buf-element-type 'single-float) IMG_SINGLE_FLOAT)
		   ((equal buf-element-type '(signed-byte 32)) IMG_SIGNED_32BIT)
		   ((equal buf-element-type '(unsigned-byte 8)) IMG_UNSIGNED_8BIT)
		   ((equal buf-element-type '(unsigned-byte 16)) IMG_UNSIGNED_16BIT)
		   ((equal buf-element-type '(signed-byte 16)) IMG_SIGNED_16BIT)))
	  (2 (and (equal buf-element-type '(unsigned-byte 8))
		  (case (array-dimension buffer 1)
		    (3 IMG_RGB8)
		    (4 IMG_RGBA8)))))
	(error "unsupported buffer element-type ~a" buf-element-type))))

(defun image-element-type-code (element-type)
  (cond ((eq element-type 'double-float) IMG_DOUBLE_FLOAT)
	((eq element-type 'single-float) IMG_SINGLE_FLOAT)
	((equal element-type '(signed-byte 32)) IMG_SIGNED_32BIT)
	((equal element-type '(unsigned-byte 8)) IMG_UNSIGNED_8BIT)
	((equal element-type '(unsigned-byte 16)) IMG_UNSIGNED_16BIT)
	((equal element-type '(signed-byte 16)) IMG_SIGNED_16BIT)))

(defun getline-buffer-type-code (image buffer)
  (let ((buf-element-type (array-element-type buffer)))
    (or (case (array-rank buffer)
	  (1 (image-element-type-code buf-element-type))
	  (2 (let ((image-element-type (image-element-type image))
		   (buf-spp (array-dimension buffer 1)))
	       (cond ((and (eql image-element-type 'RGB8)
			   (eql buf-spp 3))
		      IMG_RGB8)
		     ((and (eql image-element-type 'RGBA8)
			   (eql buf-spp 4))
		      IMG_RGBA8)
		     ((eql buf-spp (nbands image))
		      (image-element-type-code buf-element-type))))))
	(error "unsupported buffer element-type ~a" buf-element-type))))

(declaim (special *image-get-put-args*))

;(fmakunbound 'image-getline)
(defmethod image-getline (image buffer x y
				&optional (n (array-dimension buffer 0)) (to-start 0) (dx 1) (band 0))
  (declare (type fixnum x y n to-start dx))
  (let* ((n (min n (floor (- (image-x-dim image) x) dx)))
	 (id (image-id image)))
    (unless id (error "image-getline not supported for image ~a~%" image))
    (let* ((buf-type-code (getline-buffer-type-code image buffer))
	   (err-code (image_getline id buf-type-code buffer x y n to-start dx band)))
      ;;(format t "image-getline ~a~%" image)
      (case err-code
	(0 buffer)
	(-1 (setq *image-get-put-args* (list image buffer x y n to-start dx band)) 
	    (error "getline bounds error"))
	(-2 (setq *image-get-put-args* (list image buffer))
	    (error "getline unsupported buffer element-type"))
	(otherwise (error "getline bad return err-code ~a" err-code)))
      )))

(defmethod image-putline (image buffer x y
				&optional (n (array-dimension buffer 0)) (to-start 0) (band 0))
  (declare (type fixnum x y n to-start dx))
  (let* ((n (min n (- (image-x-dim image) x)))
	 (id (image-id image)))
    (unless id (error "image-putline not supported for image ~a~%" image))
    (let* ((buf-type-code (getline-buffer-type-code image buffer))
	   (err-code (image_putline id buf-type-code buffer x y n to-start band)))
      (case err-code
	(0 buffer)
	(-1 (setq *image-get-put-args* (list image buffer x y n to-start band)) 
	    (error "putline bounds error"))
	(-2 (setq *image-get-put-args* (list image buffer))
	    (error "putline unsupported buffer element-type"))
	(otherwise (error "putline bad return err-code ~a" err-code)))
      )))

#+experimental
(defmethod image-getline (image buffer x y
				&optional (n (array-dimension buffer 0)) (to-start 0) (dx 1) (band 0))
  (declare (type fixnum x y n to-start dx))
  (let* ((n (min n (floor (- (image-x-dim image) x) dx)))
	 (id (image-id image)))
    (unless id (error "image-getline not supported for image ~a~%" image))
    (let* ((buf-type (array-element-type buffer))
	   (buf-type-code (l2c-element-type buf-type))
	   ;;(bar (setq *foo* (list image buffer buf-type-code)))    
	   (err-code (if (not (= buf-type-code IMG_ANY))
			 (image_getline id buf-type-code buffer x y n to-start dx band)
			 (error "unsupported buffer element-type ~a" buf-type))))
      (when (eql err-code -1) 
	;;(setq *foo* (list image buffer x y n to-start dx band))
	(error "getline failed"))
      )))
    
(defmethod image-getcolumn (image buffer x y
				  &optional (n (array-dimension buffer 0)) (to-start 0) (dy 1) (band 0))
  (declare (type fixnum x y n to-start dy))
  (let* ((n (min n (floor (- (image-y-dim image) y) dy)))
	 (id (image-id image)))
    (unless id (error "image-getcolumn not supported for image ~a~%" image))
    (let* ((buf-type-code (getline-buffer-type-code image buffer))
	   (err-code (image_getcolumn id buf-type-code buffer x y n to-start dy band)))
      (case err-code
	(0 buffer)
	(-1 (setq *image-get-put-args* (list image buffer x y n to-start dy band)) 
	    (error "getcolumn bounds error"))
	(-2 (setq *image-get-put-args* (list image buffer))
	    (error "getcolumn unsupported buffer element-type"))
	(otherwise (error "getcolumn bad return err-code ~a" err-code)))
      )))

(defmethod image-putcolumn (image buffer x y
				  &optional (n (array-dimension buffer 0)) (to-start 0) (band 0))
  (declare (type fixnum x y n to-start))
  (let* ((n (min n (- (image-y-dim image) y)))
	 (id (image-id image)))
    (unless id (error "image-putcolumn not supported for image ~a~%" image))
    (let* ((buf-type-code (getline-buffer-type-code image buffer))
	   (err-code (image_putcolumn id buf-type-code buffer x y n to-start band)))
      (case err-code
	(0 buffer)
	(-1 (setq *image-get-put-args* (list image buffer x y n to-start band)) 
	    (error "getcolumn bounds error"))
	(-2 (setq *image-get-put-args* (list image buffer))
	    (error "getcolumn unsupported buffer element-type"))
	(otherwise (error "putcolumn bad return err-code ~a" err-code)))
      )))

(defun image-get-rectangle (image buffer xleft ybot nx ny pbx)
  (let ((err-code (image_get_rectangle (image-id image)
				       (getline-buffer-type-code image buffer)
				       buffer
				       xleft ybot nx ny
				       (img::nbands image)
				       0
				       pbx)))
    (case err-code
      (0 buffer)
      (-1 (setq *image-get-put-args* (list image buffer xleft ybot nx ny pbx)) 
	  (error "get-rectangle bounds error"))
      (-2 (setq *image-get-put-args* (list image buffer))
	  (error "get-rectangle unsupported buffer element-type"))
      (otherwise (error "get-rectangle bad return err-code ~a" err-code)))))

(defun image-get-rectangle-bordered (image buffer xleft ybot nx ny pbx border)
  (let ((err-code (image_get_rectangle_bordered (image-id image)
						(img::getline-buffer-type-code image buffer)
						buffer
						xleft ybot nx ny border
						(img::nbands image)
						0
						pbx)))
    (case err-code
      (0 buffer)
      (-1 (setq *image-get-put-args* (list image buffer xleft ybot nx ny pbx)) 
	  (error "get-rectangle bounds error"))
      (-2 (setq *image-get-put-args* (list image buffer))
	  (error "get-rectangle unsupported buffer element-type"))
      (otherwise (error "get-rectangle bad return err-code ~a" err-code)))))

    
(defun image-put-rectangle (image buffer xleft ybot nx ny pbx)
  (let ((err-code (image_put_rectangle (image-id image)
				       (getline-buffer-type-code image buffer)
				       buffer
				       xleft ybot nx ny
				       (img::nbands image)
				       0
				       pbx)))
    (case err-code
      (0 buffer)
      (-1 (setq *image-get-put-args* (list image buffer xleft ybot nx ny pbx)) 
	  (error "put-rectangle bounds error"))
      (-2 (setq *image-get-put-args* (list image buffer))
	  (error "put-rectangle unsupported buffer element-type"))
      (otherwise (error "put-rectangle return err-code ~a" err-code)))))

;;;
;;; Kind of a hack, but uses the fast defaults to smooth an image:
;;;
;;; Even more of a hack: there is a scratch image involved.  It should
;;; be allocated and deallocated inside cfast_gauss_convolve, but
;;; unmake-image doesn't work, so allow it to be passed in.
;;;
(defmethod fast-gauss-convolve (image &key into-image (level 1) (ka .375) scratch-image)
  (unless into-image
    (setq into-image (make-image (image-dimensions image)
				 :element-type (image-element-type image))))
  (fast_gauss_convolve (image-id image) (image-id into-image)
		       level ka (if scratch-image (image-id scratch-image) *null-pointer*))
  into-image)

(defun write-untiled-tiff-image (image path)
  (write_untiled_tiff_image (image-id image) path))

(defun rgb8-to-yuv-images (rgb8-in &key
				   (y-out (make-image (image-dimensions rgb8-in)))
				   (u-out (make-image (image-dimensions rgb8-in)))
				   (v-out (make-image (image-dimensions rgb8-in))))
  (rgb8_to_yuv_images (image-id rgb8-in) (image-id y-out) (image-id u-out) (image-id v-out))
  (values y-out u-out v-out))

(def-foreign-function (extant_c_gallocs (:name "xxcounter_value")
					(:return-type :int))
    )


;;; *************  DEPRECATED CODE  *************

#+never ; LHQ Fri Feb 23 2007
(progn

(def-foreign-function (image_prop* (:name (freedius-prefix "image_prop")) (:return-type :fixnum))
    (img (:pointer c-image))
  (prop :simple-string))

(def-foreign-function (image_putprop* (:name (freedius-prefix "image_putprop")))
    (img (:pointer c-image))
  (prop :simple-string)
  (value :fixnum))

(def-foreign-function (image_prop_float* (:name (freedius-prefix "image_prop_float")) 
					 (:return-type :single-float))
    (img (:pointer c-image))
  (prop :simple-string))

(def-foreign-function (image_prop_int* (:name (freedius-prefix "image_prop_int")) 
					 (:return-type :int))
    (img (:pointer c-image))
  (prop :simple-string))

(def-foreign-function (image_putprop* (:name (freedius-prefix "image_putprop")))
    (img (:pointer c-image))
  (prop :simple-string)
  (value :fixnum))

(def-foreign-function (image_putprop_float* (:name (freedius-prefix "image_putprop_float")))
    (img (:pointer c-image))
  (prop :simple-string)
  (value :single-float))

(def-foreign-function (image_putprop_int* (:name (freedius-prefix "image_putprop_int")))
    (img (:pointer c-image))
  (prop :simple-string)
  (value :fixnum))

(defun convert-image_prop-key-to-string (prop)
  (if (symbolp prop)
      (string-downcase (symbol-name prop))
      prop))

(defun image_prop (image prop &optional (value-example 1))
  (let ((imgid (image-id image))
	(key (convert-image_prop-key-to-string prop)))
    (typecase value-example
      (integer (image_prop_int* imgid key))
      ((or single-float double-float) (dfloat (image_prop_float* imgid key)))
      (otherwise (image_prop* imgid key))
      )))
			   
(defun (setf image_prop) (value image prop)
  (let ((imgid (image-id image))
	(key (convert-image_prop-key-to-string prop)))
    (typecase value
      (integer (image_putprop_int* imgid key value))
      ((or single-float double-float) (image_putprop_float* imgid key (float value 1f0)))
      (otherwise (image_putprop* imgid key value))
      ))
  value)

) ; end #+removed progn


#+never ; deprecated Mon Mar 26 2007 
(progn

#+cmu  
(def-foreign-function (_iref (:name (freedius-prefix "iref")) (:return-type :fixnum) (:arg-checking nil))
  (img (:pointer c-image))
  (x :int)
  (y :int))
	
#+allegro
(ff:def-foreign-call (_iref #.(freedius-prefix "iref"))
    ((img :long)
     (x :long)
     (y :long))
  :call-direct t :arg-checking nil)
	      
#+never
(defun iref (image x y)
  (_iref (image-id image) x y))

;(config::proclaim-optimizations :safest)
(def-foreign-function (_diref (:name (freedius-prefix "diref")) (:return-type :double-float))
    (img (:pointer c-image))
  (x :int)
  (y :int))

#|
(img::_diref (img::image-id (gui::view-image (gui::top-view))) 0 0)
(img::_diref 1 0 0)
|#

#+never
(defun diref (image x y)
  (_diref (image-id image) x y))

(def-foreign-function (_iset (:name (freedius-prefix "iset")) (:return-type :null))
    (img (:pointer c-image))
  (x :int)
  (y :int)
  (val :int))

(def-foreign-function (_diset (:name (freedius-prefix "diset")) (:return-type :null))
    (img (:pointer c-image))
  (x :int)
  (y :int)
  (val :double-float))

;;;
;;;; Some higher-level Lisp iref/isets that avoid possible brain
;;;; damage with map arrays.
;;;

(defun c-iref (image i j)
  (if (image-floatp image)
      (_diref (image-id image) i j)
      (_iref (image-id image) i j)))


(defun c-iset (image i j val)
  (if (image-floatp image)
      (_diset (image-id image) i j val)
      (_iset (image-id image) i j val)))


) ; end #+never progn 

;; No callers.  Deleted - must '#define ENABLE_IMAGE_PYRAMIDS' in image-pyramids.c++
#+never
(progn 

(def-foreign-function (get_image_pyramid_level (:name (freedius-prefix "get_image_pyramid_level"))
						(:return-type (:pointer c-image)))
    (img (:pointer c-image))
  (level :int)
  (instantiate :int)
  (pyramid-type :int)
  )

(def-foreign-function (set_pyramid_level (:name (freedius-prefix "set_pyramid_level")))
    (base-image (:pointer c-image))
  (image (:pointer c-image))
  (level :int))

;; No callers
(defun set-pyramid-level (base-image image level)
  (set_pyramid_level (image-id base-image) (image-id image) level))

) ; end progn

#+never  ;; these don't mix this the Lisp image-pyramid implementation.
(progn

(def-foreign-function (recompute_pyramid (:name (freedius-prefix "recompute_pyramid")))
    (img (:pointer c-image)))

(def-foreign-function (set_pyramid_damaged (:name (freedius-prefix "set_pyramid_damaged")))
  (img (:pointer c-image)))

(defun recompute-pyramid (image)
  (recompute_pyramid (image-id image)))

(defun set-pyramid-damaged (image)
  (set_pyramid_damaged (image-id image)))

) ; end progn


#+old
(progn

;;; provisional -- needs to be flushed.
(def-foreign-function (image_set_dynamic_range (:name (freedius-prefix "image_set_dynamic_range")))
    (img (:pointer c-image))
  (scale :double-float)
  (offset :double-float))

(defun set-image-dynamic-range (image &optional min max)
  (multiple-value-bind (imin imax)
      (img::image-element-min-max image)
    (unless min (setq min imin))
    (unless max (setq max imax))
    (let* ((scale (* 256.0 (/ 256.0 (- max min))))
	   (offset (* scale min)))
      (image_set_dynamic_range (image-id image) scale offset)
      (list scale offset))))

(defun maybe-compute-display-scale-offset (image)
  (when (and (scalar-image-p image)
	     ;; A hack, for now.  Should somehow designate band-interleaved images as non-scalar:
	     (not (typep image 'img::band-interleaved-paged-image))
	     (not (typep image 'img::band-interleaved-array-image))
	     (not (get-prop image :display-scale-offset))
	     (not (eql (image-element-size image) 8)))
    ;; why not use :photometric-transform?
    (setf (get-prop image :display-scale-offset)
	  (set-image-dynamic-range image))))
) ; end #+old progn



(def-foreign-function (save_jpeg_image
		       (:name (FREEDIUS-PREFIX "save_jpeg_image"))
		       (:return-type :int))
    (image (:pointer c-image))
  (path :simple-string)
  (quality :int))


(defun save-jpeg-image (image filename &optional (quality 90))
  (save_jpeg_image (image-id image) filename quality))


(def-foreign-function (make-image-from-jpeg-buffer
                       (:name (FREEDIUS-PREFIX "make_image_from_jpeg_buffer"))
                       (:return-type (:pointer c-image)))
    #-sbcl (buffer :simple-string)
    #+sbcl (buffer :simple-array)
  (size :int)
  (into-image (:pointer c-image)))

#-mswindows
(def-foreign-function (make-coef-image-from-jpeg-buffer
                       (:name (FREEDIUS-PREFIX "make_coef_image_from_jpeg_buffer"))
                       (:return-type (:pointer c-image)))
    #-sbcl (buffer :simple-string)
    #+sbcl (buffer :simple-array)
  (size :int)
  (into-image (:pointer c-image)))


(def-foreign-function (make-jpeg-buffer-from-image
                       (:name (FREEDIUS-PREFIX "make_jpeg_buffer_from_image"))
                       (:return-type :int))
    (source-image (:pointer c-image))
  (into-buffer :simple-array)
  (size :int)
  (quality :int))


(def-foreign-callable (make_image_callback
		       (:name (freedius-prefix "make_image_callback"))
		       (:return-type (:pointer c-image)))
    ((xdim :int) 
     (ydim :int)
     (element-type-code :int)
     (samples-per-pixel :int)
     (block-xdim :int)
     (block-ydim :int)
     (padded-block-xdim :int)
     )
  (make_image_internal xdim ydim element-type-code samples-per-pixel 
		       block-xdim block-ydim padded-block-xdim))

;;; called from C++ code
(defun make_image_internal (xdim ydim element-type-code samples-per-pixel 
			    block-x-dim block-y-dim padded-block-x-dim)
  (setq *foo* (list xdim ydim element-type-code samples-per-pixel 
			    block-x-dim block-y-dim padded-block-x-dim))
  (let ((image
	 (new-make-image (list xdim ydim) 
			 :element-type (c2l-element-type element-type-code)
			 :samples-per-pixel samples-per-pixel
			 :block-x-dim (if (= block-x-dim 0) nil block-x-dim)
			 :block-y-dim (if (= block-y-dim 0) nil block-y-dim)
			 :padded-block-x-dim (if (= padded-block-x-dim 0) nil padded-block-x-dim))))
    (image-id image)))

#+never
(defun make_image_internal (xdim ydim element-type-code samples-per-pixel 
			    block-xdim block-ydim padded-block-xdim)
  (setq *foo* (list xdim ydim element-type-code samples-per-pixel 
		    block-xdim block-ydim padded-block-xdim)))
  

(defvar *local-hostname* nil)

;;; Upon exit, clean these up:
(defvar *file-image-filenames* nil)

(defun file-image-cleanup ()
  (loop for name in *file-image-filenames*
	do (delete-file name)))


(defun local-hostname ()
  (or *local-hostname*
      (setf *local-hostname* 
	    (let* ((hostname (lx::gethostname))
		   (pos (position #\. hostname)))
	      (if pos 
		  (subseq hostname 0 pos) ; return just then host name without the domain
		  hostname)))))

(defvar *file-image-allocation-counter* 0)

;(allocate-file-image-pathname "tif")
(defun allocate-file-image-pathname (type)
  (let* ((dir *DEFAULT_TMP_FILE_IMAGE_DIR*)
	 (localhost (local-hostname))
	 (count (incf *file-image-allocation-counter*))
	 (process-id (lx:getpid))
	 (pathname (format nil "~a/~a-~d-~d.~a" dir localhost process-id count type)))
    pathname))

(defun make-tiff-file-image (dims &key
			     (element-type '(unsigned-byte 8))
			     (block-x-dim 256) ; these need to default better
			     (block-y-dim -256)
			     (padded-block-x-dim block-x-dim)
			     (samples-per-pixel 1)
			     pathname 
			     &allow-other-keys)
  (unless pathname
    (setq pathname (allocate-file-image-pathname "tif")))
  (when (probe-file pathname)
    (error ";;; make-file-image overwriting existing file: ~a~%" pathname)
    ;;(delete-file pathname)
    )
  
  (let* ((x-dim (car dims))
	 (y-dim (cadr dims))
	 (bits-per-sample (element-size-from-element-type element-type))
	 (element_type_code (l2c-element-type element-type)))
    (unless (and block-x-dim block-y-dim padded-block-x-dim)
      (mv-setq (block-x-dim block-y-dim padded-block-x-dim)
	       (array-image-default-block-dims x-dim y-dim bits-per-sample samples-per-pixel)))
    (let* ((cimage (make_tiff_file_image x-dim y-dim element_type_code samples-per-pixel
					 block-x-dim block-y-dim padded-block-x-dim pathname)))
      (pushnew pathname *file-image-filenames*)
      (load-image-wrap-image (wrap-foreign-image cimage :samples-per-pixel samples-per-pixel)))))

(defun make-file-image (dims &key
			(element-type '(unsigned-byte 8))
			(block-x-dim 256) ; these need to default better
			(block-y-dim -256)
			(padded-block-x-dim block-x-dim)
			(samples-per-pixel 1)
			pathname
			&allow-other-keys)
  (when (probe-file pathname)
    (error ";;; make-file-image overwriting existing file: ~a~%" pathname)
    ;;(delete-file pathname)
    )
  
  (let* ((xdim (car dims))
	 (ydim (cadr dims))
	 (bits-per-sample (element-size-from-element-type element-type))
	 (element_type_code (l2c-element-type element-type)))
    (unless (and block-x-dim block-y-dim padded-block-x-dim)
      (mv-setq (block-x-dim block-y-dim padded-block-x-dim)
	       (array-image-default-block-dims x-dim y-dim bits-per-sample samples-per-pixel)))
    (let* ((cimage (make_file_image xdim ydim element_type_code samples-per-pixel
				    block-x-dim block-y-dim padded-block-x-dim pathname)))
      (pushnew pathname *file-image-filenames*)
      (load-image-wrap-image (wrap-foreign-image cimage :samples-per-pixel samples-per-pixel)))))

(defparameter *max-array-image-size-bytes* (* 2048 2049))
(defparameter *make-file-image-fn* 'make-tiff-file-image)
(defparameter *make-image-fn* nil)


(defun new-make-image (dims &rest initargs  
		       &key element-type image-type
		       (samples-per-pixel 1)
		       block-x-dim block-y-dim
		       padded-block-x-dim
		       (make-image-fn *make-image-fn*)
		       (make-file-image-fn *make-file-image-fn*)
		       &allow-other-keys)
  (unless element-type (setq element-type '(unsigned-byte 8)))
  (let* ((xdim (car dims))
	 (ydim (cadr dims))
	 (bits-per-sample (element-size-from-element-type element-type))
	 (imgsize-bytes (* ydim (ash (* xdim bits-per-sample samples-per-pixel) -3)))
	 (image-make-fn (or *make-image-fn*
			    (if (< imgsize-bytes  *max-array-image-size-bytes*)
				'make-array-image
				*make-file-image-fn*)))
	 )
    (apply image-make-fn dims :element-type element-type initargs)))

#|
(make-file-image )
(defparameter *img1* (new-make-image '(512 512)))
(defparameter *win2* (gui::selected-window))
(let* ((img (gui::view-image (top-view)))
       (new (new-make-image (image-dimensions img))))
  (copy-image img new) 
  (gui::push-image new *win2*))

(setq *img* (new-make-image '(3000 3000) :element-type '(unsigned-byte 8) :samples-per-pixel 3))
(gui::push-image *img* (gui::selected-window))

(defparameter *erdas-img* (load-image "/m/rom1/downloads/tiff/other-pics/leica-example.tif"))
(gui::push-image *erdas-img* (gui::selected-window))
|#

#+never
(defun new-make-image (dims &rest initargs  
		       &key element-type image-type
		       samples-per-pixel
		       block-x-dim block-y-dim
		       padded-block-x-dim
		       &allow-other-keys)
  (unless element-type (setq element-type '(unsigned-byte 8)))
  (let* ((xdim (car dims))
	 (ydim (cadr dims))
	 (bits-per-sample (element-size-from-element-type element-type))
	 ;;(imgsize-bytes (* ydim (ash (* xdim bits-per-sample samples-per-pixel) -3)))
	 )
    (unless (and block-xdim block-ydim padded-block-xdim)
      (mv-setq (block-xdim block-ydim padded-block-xdim)
	       (default-block-dims xdim ydim bits-per-sample samples-per-pixel)))

    (let* ((blocks-wide (ceiling xdim blk-xdim))
	   (blocks-hi (ceiling ydim (abs blk-ydim)))
	   (block-size (* blk-xdim (abs blk-ydim)))
	   (imgsize-bytes (* blocks-wide blocks-hi (ash (* block-size bytes-per-sample) -3)))
	   (image-make-fn (if (< imgsize-bytes  *max-array-image-size-bytes*)
			      'make-array-image
			      'make-file-image))
	   )
      (funcall image-make-fn dims 
	       :element-type element-type 
	       :samples-per-pixel samples-per-pixel
	       :block-x-dim block-x-dim
	       :block-y-dim block-y-dim
	       :padded-block-x-dim padded-block-x-dim))))


	  

