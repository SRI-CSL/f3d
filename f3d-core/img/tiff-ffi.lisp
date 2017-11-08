(in-package :img)
#|
(maybe-compile-file-load "$FREEDIUS/lisp/img/tiff-ffi.lisp")
|#

;;; This stuff should be in package  TIFF

(def-foreign-synonym-type TIFF dummy-pointer)

(def-foreign-function (TIFFOpen (:name "TIFFOpen") (:return-type TIFF))
    (filename :simple-string)
  (mode :simple-string)
  )


(def-foreign-function (TIFFPrintDirectory (:name "TIFFPrintDirectory"))
    (tif TIFF) (fd :int) (flags :int))

(def-foreign-function (TIFFWriteDirectory (:name "TIFFWriteDirectory"))
    (tif TIFF))

(def-foreign-function (TIFFFlush (:name "TIFFFlush"))
    (tif TIFF))
(def-foreign-function (TIFFFlushData (:name "TIFFFlushData"))
    (tif TIFF))
(def-foreign-function (TIFFClose (:name "TIFFClose"))
    (tif TIFF))
(def-foreign-function (TIFFCurrentTile (:name "TIFFCurrentTile"))
    (tif TIFF))
(def-foreign-function (TIFFCurrentRow (:name "TIFFCurrentRow"))
    (tif TIFF))

;;;(def-foreign-function (TIFFGetField-internal (:name "TIFFGetFieldDefaulted"))
;;;    (tif TIFF) tag &rest value-pointer)

(def-foreign-function (TIFFGetField (:name "TIFFGetFieldDefaulted"))
    (tif TIFF) (tag :int) (value-pointer :simple-array))

(def-foreign-function (TIFFGetField2 (:name "TIFFGetFieldDefaulted"))
    (tif TIFF) (tag :int) (value-pointer1 :simple-array) (value-pointer2 :simple-array))

(defun TIFFGetField-uint32 (tif tag)
  (let ((arr (make-array 1 :element-type '(unsigned-byte 32))))
    (TIFFGetField tif tag arr)
    (aref arr 0)))

(def-foreign-function (TIFFGetField-uint32-array-int (:name "TIFFGetField_uint32_array"))
    (tif TIFF) (tag :int) (arr :simple-array) (n :int))

(def-foreign-function (TIFFGetField-float-array-int (:name "TIFFGetField_float_array"))
    (tif TIFF) (tag :int) (arr :simple-array) (n :int))

(def-foreign-function (TIFFGetField-uint16-array-int (:name "TIFFGetField_uint16_array"))
    (tif TIFF) (tag :int) (arr :simple-array) (n :int))

(defun TIFFGetField-uint32-array (tif tag n)
  (let ((arr (make-array n :element-type '(unsigned-byte 32))))
    (TIFFGetField-uint32-array-int tif tag arr n)
    arr))
    
(defun TIFFGetField-uint16 (tif tag)
  (let ((arr (make-array 1 :element-type '(unsigned-byte 16))))
    (TIFFGetField tif tag arr)
    (aref arr 0)))

(defun TIFFGetField-uint16-pair (tif tag)
  (let ((arr (make-array 1 :element-type '(unsigned-byte 16)))
	(arr2 (make-array 1 :element-type '(unsigned-byte 16))))
    (TIFFGetField2 tif tag arr arr2)
    (values (aref arr 0) (aref arr2 0))))


;;; This uses the varargs version of the call: broken
;;;(defun TIFFGetField-uint16-list (tif tag n)
;;;  (let ((arrs (loop for i fixnum from 0 below n
;;;                    collect (make-array 1 :element-type '(unsigned-byte 16)))))
;;;    (apply 'TIFFGetField-internal tif tag arrs)
;;;    (loop for arr in arrs collect (aref arr 0))))


  
(defun TIFFGetField-uint16-array (tif tag n)
  (let ((arr (make-array n :element-type '(unsigned-byte 16))))
    (TIFFGetField-uint16-array-int tif tag arr n)))

(def-foreign-function (TIFFGetField-string-internal (:name "tiffgetfield_string")
						    (:return-type :simple-string))
    (tif TIFF) (tag :int))

(defparameter *c_strings* nil)

(defun TIFFGetField-string (tif tag)
  (let ((cstr (TIFFGetField-string-internal tif tag)))
    (when (> (length cstr) 0)
      cstr)))

(def-foreign-function (TIFFReadColormap (:name "TIFFReadColormap"))
    (tif TIFF) (rmap :simple-array) (gmap :simple-array) (bmap :simple-array))

#|
(def-foreign-function (TIFFSetField-uint32-array-int (:name "TIFFSetField_uint32_array"))
    (tif TIFF) tag array n)
(def-foreign-function (TIFFSetField-float-array-int (:name "TIFFSetField_float_array"))
    (tif TIFF) tag array n)
|#

;;; This uses the varargs version of the call: broken

;;;(def-foreign-function (TIFFSetField (:name "TIFFSetField"))
;;;    (tif TIFF) tag &rest value)

(def-foreign-function (TIFFSetField (:name "TIFFSetField"))
    (tif TIFF) (tag :int) (value :int))

(def-foreign-function (TIFFSetField2 (:name "TIFFSetField"))
    (tif TIFF) (tag :int) (value1 :int) (value2 :int))

(def-foreign-function (TIFFSetAsciiField (:name "TIFFSetField"))
    (tif TIFF) (tag :int) (string :simple-string))


(def-foreign-function (TIFFReadScanline_int (:name "TIFFReadScanline"))
    (tif TIFF) (buf :simple-array) (ypos :int) (sample-index :int))

(defun TIFFReadScanline (tif buf ypos &optional (sample-index 0))
  (TIFFReadScanline_int tif buf ypos sample-index))
 
(def-foreign-function (TIFFWriteScanline_int (:name "TIFFWriteScanline"))
    (tif TIFF) (buf :simple-array) (ypos :int) (sample-index :int))

(defun TIFFWriteScanline (tif buf ypos &optional (sample-index 0))
  (TIFFWriteScanline_int tif buf ypos sample-index))
 
(def-foreign-function (TIFFReadTile_int (:name "TIFFReadTile"))
    (tif TIFF) (buf :simple-array) (xpos :int) (ypos :int) (zpos :int) (sample-index :int))

(defun TIFFReadTile (tif buf xpos ypos &optional (zpos 0) (sample-index 0))
  (TIFFReadTile_int tif buf xpos ypos zpos sample-index))

(def-foreign-function (TIFFWriteTile_int (:name "TIFFWriteTile"))
    (tif TIFF) (buf :simple-array) (xpos :int) (ypos :int) (zpos :int) (sample-index :int))

(defun TIFFWriteTile (tif buf xpos ypos &optional (zpos 0)(sample-index 0))
  (TIFFWriteTile_int tif buf xpos ypos zpos sample-index))

(def-foreign-function (TIFFReadEncodedStrip (:name "TIFFReadEncodedStrip"))
    (tif TIFF) (strip-num :int) (buf :array) (size :int))

(def-foreign-function (TIFFNumberOfStrips (:name "TIFFNumberOfStrips"))
    (tif TIFF))

(def-foreign-function (TIFFStripSize (:name "TIFFStripSize"))
    (tif TIFF))

;; #+never ;; This isn't defined in old tiff library versions.
(def-foreign-function (TIFFSetupStrips (:name "TIFFSetupStrips") (return-type :int))
  (tif TIFF))

(def-foreign-function (TIFFReadDirectory (:name "TIFFReadDirectory") (return-type :int))
  (tif TIFF))

(def-foreign-function (TIFFWriteDirectory (:name "TIFFWriteDirectory") (return-type :int))
  (tif TIFF))

(def-foreign-function (TIFFSetDirectory (:name "TIFFSetDirectory") (return-type :int))
  (tif TIFF) (dirnun :int))

(def-foreign-function (TIFFIsTiled (:name "TIFFIsTiled"))
    (tif TIFF))

(def-foreign-function (TIFFReadRGBAImage (:name "TIFFReadRGBAImage"))
    (tif TIFF) (width :int) (height :int) (raster :simple-array) (stop-on-error :int))

(def-foreign-function (array_copy_u16_to_u32 (:name "array_copy_u16_to_u32"))
    (array-uint16 :simple-array) (array-uint32 :simple-array) (n :int))

(def-foreign-function (array_copy_u32_to_u16 (:name "array_copy_u32_to_u16"))
    (array-uint32 :simple-array) (array-uint16 :simple-array)  (n :int))

(def-foreign-function (array_copy_u8_to_u32 (:name "array_copy_u8_to_u32"))
    (array-uint8 :simple-array) (array-uint32 :simple-array) (n :int))

(def-foreign-function (array_copy_u32_to_u8 (:name "array_copy_u32_to_u8"))
    (array-uint32 :simple-array) (array-uint8 :simple-array) (n :int))

(def-foreign-constants
    "TIFFTAG_IMAGEWIDTH"
    "TIFFTAG_IMAGELENGTH"
  "TIFFTAG_BITSPERSAMPLE"
  "TIFFTAG_SAMPLESPERPIXEL"
  "TIFFTAG_TILEWIDTH"
  "TIFFTAG_TILELENGTH"
  "TIFFTAG_ROWSPERSTRIP"
  "TIFFTAG_TILEOFFSETS"
  )

(def-foreign-constants "TIFFTAG_SAMPLEFORMAT" 
    "SAMPLEFORMAT_UINT" "SAMPLEFORMAT_INT" "SAMPLEFORMAT_IEEEFP" "SAMPLEFORMAT_VOID")

(def-foreign-constants "TIFFTAG_ORIENTATION" 
    "ORIENTATION_TOPLEFT" "ORIENTATION_TOPRIGHT" 
    "ORIENTATION_BOTRIGHT" "ORIENTATION_BOTLEFT"
    "ORIENTATION_LEFTTOP" "ORIENTATION_RIGHTTOP" 
    "ORIENTATION_RIGHTBOT" "ORIENTATION_LEFTBOT")

(def-foreign-constants
    "TIFFTAG_JPEGCOLORMODE" "JPEGCOLORMODE_RAW" "JPEGCOLORMODE_RGB"
    "TIFFTAG_JPEGQUALITY"
    )

(def-foreign-constants "TIFFTAG_PHOTOMETRIC" "PHOTOMETRIC_MINISWHITE" 
  "PHOTOMETRIC_MINISBLACK" "PHOTOMETRIC_RGB" "PHOTOMETRIC_PALETTE"
  "PHOTOMETRIC_MASK" "PHOTOMETRIC_SEPARATED" "PHOTOMETRIC_YCBCR" "PHOTOMETRIC_CIELAB")

(def-foreign-constants
    "TIFFTAG_COMPRESSION"
    "COMPRESSION_NONE" "COMPRESSION_CCITTRLE" "COMPRESSION_CCITTFAX3"
    "COMPRESSION_CCITTFAX4" "COMPRESSION_LZW"
    "COMPRESSION_OJPEG" ; not configured on SGI O2
    "COMPRESSION_JPEG" "COMPRESSION_PACKBITS"
    "COMPRESSION_DEFLATE" "COMPRESSION_DCS" "COMPRESSION_JBIG")

(def-foreign-constants "TIFFTAG_PLANARCONFIG"
    "PLANARCONFIG_CONTIG" "PLANARCONFIG_SEPARATE")


(def-foreign-constants "TIFFTAG_YCBCRSUBSAMPLING" "TIFFTAG_REFERENCEBLACKWHITE")

;;; string slots
(def-foreign-constants "TIFFTAG_IMAGEDESCRIPTION" "TIFFTAG_ARTIST" "TIFFTAG_DATETIME"
		       "TIFFTAG_DOCUMENTNAME"
		       "TIFFTAG_HOSTCOMPUTER" "TIFFTAG_INKNAMES" "TIFFTAG_MAKE"
		       "TIFFTAG_MODEL" "TIFFTAG_PAGENAME" "TIFFTAG_SOFTWARE"
		       "TIFFTAG_TARGETPRINTER")

(defun make-tiff-date-and-time  (&optional (universal-time (get-universal-time)) tz)
  (unless (numberp universal-time)
    (error "universal-time must be a number"))
  (multiple-value-bind (sec min hr day mon yr)
      (if tz
	  (decode-universal-time universal-time tz)
	  (decode-universal-time universal-time))
    (format nil "~4d:~2,vd:~2,vd ~2,vd:~2,vd:~2,vd"
	    yr #\0 mon #\0 day #\0 hr #\0 min #\0  sec)))

(defvar *stderr-fd*
  #+sbcl  (sb-impl::get-stream-fd-and-external-format SB-SYS:*STDERR* :output)
  )

(defun print-tiff-file-directory (path &optional (flags 0))
  (let* ((tif (TIFFOpen (namestring (truename path)) "rm")))
    (when (eql tif 0)
      (error "File ~a cannot be opened as a TIFF file" path))
    (TIFFPrintDirectory tif *stderr-fd* flags)
    (TIFFClose tif)
    ))






(def-foreign-function (make_tiff_lazy_image (:name (FREEDIUS-PREFIX "make_tiff_lazy_image"))
					     (:return-type  (:pointer c-image)))
    (path :simple-string)
  (xdim :int)
  (ydim :int)
  (element_type :int)
  (block_xdim :int)
  (block_ydim :int)
  (format :int)
  (spp :int)
  (photometric_type :int)
  (compression_mode :int)
  (jpeg_quality :int))


(def-foreign-function (make_tiff_file_image (:name (FREEDIUS-PREFIX "make_tiff_file_image"))
					    (:return-type  (:pointer c-image)))
  (xdim :int)
  (ydim :int)
  (element_type :int)
  (spp :int)
  (block_xdim :int)
  (block_ydim :int)
  (padded_block_xdim :int)
  (path :simple-string))






(defparameter *tiff-image-file-property-list-header* "SRI CME PROPERTY-LIST")
(defparameter *tiff-image-file-save-image-property-list* t)


(defmethod tif-get-property-list (tif)
  (let* ((header *tiff-image-file-property-list-header*)
	 (header-length (length header))
	 (creation-date (TIFFGetField-string tif TIFFTAG_DATETIME))
	 (description (TIFFGetField-string tif TIFFTAG_IMAGEDESCRIPTION))
	 (props (when description
		  (if (and (> (length description) header-length)
			   (string= description header :end1 header-length))
		      (parse-property-list-string
		       (substring description (1+ header-length)))
		      (list :description description)))))
    (when creation-date (setq props (list* :creation-date creation-date props)))
    props))



(defun read-tiff-image-header (path &optional close)
  (let* ((tif (TIFFOpen (namestring (truename path)) "rm")))
    (when (eql tif 0)
      (error "File ~a cannot be opened as a TIFF file" (truename path)))
    (let* ((xdim (TIFFGetField-uint32 tif   TIFFTAG_IMAGEWIDTH))
	   (ydim (TIFFGetField-uint32 tif TIFFTAG_IMAGELENGTH))
	   (spp (TIFFGetField-uint16 tif TIFFTAG_SAMPLESPERPIXEL))
	   ;;(photometric-mode (TIFFGetField-uint16 tif TIFFTAG_PHOTOMETRIC))
	   ;; Although the TIFF spec says TIFFTAG_BITSPERSAMPLE is an array of
	   ;; spp elements, the code only handles 1 value, and tiff.h
	   ;; has only a single value in the TIFFDirectory struct.
	   (bps (TIFFGetField-uint16 tif TIFFTAG_BITSPERSAMPLE))
	   (tiled-p (neq 0 (TIFFIsTiled tif)))
	   (props (tif-get-property-list tif))
	   )

      (if tiled-p
	  (let* ((bx (TIFFGetField-uint32 tif   TIFFTAG_TILEWIDTH))
		 (by (TIFFGetField-uint32 tif   TIFFTAG_TILELENGTH))
		 (tiles-wide (ceiling xdim bx))
		 (tiles-hi (ceiling ydim by))
		 (tile-offsets (TIFFGetField-uint32-array tif TIFFTAG_TILEOFFSETS
							  (* spp tiles-wide tiles-hi)))
		 )
	    (when close (TIFFClose tif) (setq tif nil))
	    (values tif xdim ydim spp bps props tiled-p bx by tile-offsets))

	  (progn (when close (TIFFClose tif) (setq tif nil))
		 (values tif xdim ydim spp bps props)
		 )))))


(defun make-tiff-lazy-image-internal
    (tif xdim ydim bps props bx by
	 &key
	 (tile-builder 'tiff-lazy-image-tile-builder))
  (let* ((*force-image-block-dims* nil)
	 (image (make-image (list xdim ydim)
			   :element-size bps
			   :image-type 'lazy-image 
			   :block-x-dim bx :block-y-dim (- by)
			   :block-size (* bx by)
			   :property-list props
			   :page-handler (make-instance *tiff-lazy-image-page-handler-class*)
			   :tile-builder tile-builder )))
    (declare (special *force-image-block-dims*))
    (setf (internal-get-prop image :tiff-info)
	  (list tif (TIFFGetField-uint16 tif TIFFTAG_SAMPLESPERPIXEL)))

    image))

(defun make-tiff-lazy-image-internal (tif xdim ydim bps props bx by )
  
  

(defun load-tiff-lazy-image (pathname)
  (
  
(defparameter *max-image-array-size* (floor 1e7)) ; 10 MB
(defparameter *max-image-array-size* (floor 1e6)) ; 1 MB

(defmethod load-tiff-image (pathname)
  (mv-bind (tif xdim ydim spp bps props tiled-p bx by tile-offsets)
      (read-tiff-image-header pathname t)
    (let* ((props nil)
	   (photometric (TIFFGetField-uint16 tif TIFFTAG_PHOTOMETRIC))
	   (compression (TIFFGetField-uint16 tif TIFFTAG_COMPRESSION))
	   (uncompressed-p (memq compression (list 0 COMPRESSION_NONE)))
	   (size-in-bytes (ash (* x-dim y-dim element-size) -3))
	   (large-p (> size-in-bytes *max-image-array-size*))
	   (tiled-p (neq 0 (TIFFIsTiled tif)))
	   image
	   )
      #+todo
      (when (> spp 1)			; samplesperpixel indicates a color image
	(when into-image (unmake-image into-image :expunge t))
	(return-from image-file-format-load-image
	  (image-file-format-load-color-image image-format file tif tiled-p large-p)))

      #+never
      (when (and (eql photometric PHOTOMETRIC_PALETTE) (not large-p))
	(return-from image-file-format-load-image
	  (read-RGBA-tiff-raster-image file))) ; the image better be small
      
      #+todo
      (format *tiff-trace-output* "~&;;; image-file-format-load-image tiff-image-file-format compression=~a~%"
	      compression)
      (setq image
	    (if tiled-p
		(if large-p
		    (make-tiff-lazy-image-internal tif x-dim y-dim element-size
						   props block-x-dim (abs block-y-dim))
		    ;; read tiled tiff into an array image
		    (read-tiled-tiff-image-internal tif x-dim y-dim element-size
						    props block-x-dim (abs block-y-dim)
						    into-image))
		;; read untiled tiff into an array image (or a file image if large)
		(read-untiled-tiff-image-internal tif x-dim y-dim element-size
						  props into-image)))
      ;;#+never
      (when (eql photometric PHOTOMETRIC_PALETTE)
	(let ((tif (TIFFOpen (namestring (truename file)) "rm")))
	  (setf (get-prop image :tiff-color-map)
		(get-tiff-palette-color-map tif)))
	(TIFFClose tif))
      image)))


#|
(read-tiff-image-header "/homedir/quam/pix/forestcat.tif")
(defparameter *tif2* (mv-list (read-tiff-image-header "$RADIUS/site-2d-worlds/alv/alv-2-44/full-tiff/image.g0")))

(TIFFIsTiled (nth 0 *tif2*))

(tif-get-property-list (nth 0 *tif2*))

|#