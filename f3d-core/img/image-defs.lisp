(in-package :img)

(custom:defgroup :image () "FREEDIUS Image Customization Group")


;;; ****************  IMAGE-MAP-ELEMENT-TYPE  ****************

;;; LCL-5x FIXNUM arrays are now aligned in memory compatible with C arrays and arefs
;;; are inlined.  Unfortunately, it is faster to fastcall MACHINE-TO-LISP-SIGNED-32BIT
;;; than take the hit of crappy inline code generation that causes memory stores.
;;; Allegro does not fully inline arefs to (unsigned-byte 32) arrays.

;;; FREEDIUS allows 2^29 pixels/image vs 2^30 pixels/image in CME-6.
;;; For paged-images, there might be a way to recover the lost bit.
;;; See TRUNCATE-IMAGE-MAP-ENTRY-TO-FIXNUM in $CME-6/ic/paged-image.lisp

#+(or cmu sbcl) ; #-MAP_ELEMENT_TYPE_T
(progn

#-image-map-type-u32
(progn
;; Fri Jun 30 2006:  Why were these signed?  The C code uses unsigned int.
(deftype image-map-type () '(simple-array (signed-byte 32) (*)))
(deftype image-map-element-type () '(signed-byte 32))
) ; end progn

#+image-map-type-u32
(progn
(deftype image-map-type () '(simple-array (unsigned-byte 32) (*)))
(deftype image-map-element-type () '(unsigned-byte 32))
) ; end progn

) ; end #-MAP_ELEMENT_TYPE_T

#+allegro ;; MAP_ELEMENT_TYPE_T
(progn
#|
For Allegro:
    (deftype image-map-type () '(simple-array fixnum (*)))           => smallest/fastest code
    (deftype image-map-type () '(simple-array (signed-byte 32) (*))) => biggest images
    (deftype image-map-type () '(simple-array T (*)))                => crummy code

|#

(deftype image-map-type () '(simple-array T (*)))
(deftype image-map-element-type () 'T)

); end #+MAP_ELEMENT_TYPE_T

;(array-element-type *dummy-image-map*) ; CMU: (signed-byte 32) Allegro: T.
(defvar *dummy-image-map* (make-array0 1 :element-type 'image-map-element-type))
(declaim (type image-map-type *dummy-image-map*))

;;; IMAGE class should inherit from gl-2d-object or cartesian-coordinate-system, but
;;; those classes require STANDARD-CLASS (CLOS), not STRUCTURE-CLASS
(defstruct-class image (property-list-struct)
    ((id :initform nil :initarg :id)
     (x-dim :initform 0 :initarg :x-dim :type fixnum)
     (y-dim :initform 0 :initarg :y-dim :type fixnum)
     (x-map :initform *dummy-image-map* :initarg :x-map :type image-map-type)
     (y-map :initform *dummy-image-map* :initarg :y-map :type image-map-type)
#| FIXME -- need to add these but there are unfinished 
     (block-x-dim :initarg :block-x-dim :accessor image-block-x-dim)
     (block-y-dim :initarg :block-y-dim :accessor image-block-y-dim)
     (padded-block-x-dim :initarg :padded-block-x-dim :accessor image-padded-block-x-dim)
     (block-size :initarg :block-size :accessor image-block-size)
|#
     (element-type :initarg :element-type)
     (initial-value :initarg :initial-value)
     (iref-fn  :initform #'undefined-iref-fn) 
     (diref-fn :initform #'undefined-diref-fn)
     (iset-fn  :initform #'undefined-iset-fn)
     (diset-fn :initform #'undefined-diset-fn)
     (viref-fn  :initform #'undefined-viref-fn) ; needed for band-interleaved images
     (vdiref-fn :initform #'undefined-vdiref-fn); needed for band-interleaved images
     (viset-fn  :initform #'undefined-viset-fn); needed for band-interleaved images
     (vdiset-fn :initform #'undefined-vdiset-fn); needed for band-interleaved images
   
     )
    (:default-initargs :x-map *dummy-image-map* :y-map *dummy-image-map*)
    )

(defmethod initialize-instance :after ((image image) &key &allow-other-keys)
  (with-class-slots image (initial-value element-type) image
    (unless initial-value
      (setq initial-value (case element-type
			    (single-float 0.0f0)
			    (double-float 0.0d0)
			    (otherwise 0))))))


(defmethod nbands ((image image))
  1)

#|
(defun image-id (image)
  (declare (optimize (speed 3) (safety 0)))
  (if (null image)
      *null-pointer*
      (image-id0 image)))
|#
				    
(defstruct-class scalar-image (image)
  ())


(defun make-image (dims &rest initargs  &key element-type image-type
		   samples-per-pixel
		   block-x-dim block-y-dim
		   padded-block-x-dim
		   &allow-other-keys)
  (declare (ignore element-type block-x-dim block-y-dim padded-block-x-dim))
  (if image-type
      (apply 'make-instance image-type :x-dim (car dims) :y-dim (cadr dims) initargs)
      (apply 'make-foreign-image dims initargs)))


(defun make-raster-image (dims &rest initargs  &key element-type image-type top-to-bottom-p
			  &allow-other-keys)
  (declare (ignore element-type))
  (destructuring-bind (xdim ydim) dims
    (if image-type
	(apply 'make-instance image-type :x-dim xdim :y-dim ydim
	       :block-x-dim xdim :block-y-dim (if top-to-bottom-p ydim (- ydim)) initargs)
	(apply 'make-foreign-image dims :block-x-dim xdim :block-y-dim ydim initargs))))

(defun make-raster-image (dims &rest initargs  &key element-type image-type top-to-bottom-p
			  &allow-other-keys)
  (declare (ignore element-type))
  (destructuring-bind (xdim ydim) dims
    (if image-type
	(apply 'make-instance image-type :x-dim xdim :y-dim ydim
	       :block-x-dim xdim :block-y-dim (if top-to-bottom-p ydim (- ydim)) initargs)
	(apply 'make-foreign-image dims :block-x-dim xdim 
	       :block-y-dim (if top-to-bottom-p ydim (- ydim)) initargs))))

(defmethod image-initial-element ((image image))
  (image-initial-value image))

(defmethod image-prop (image prop)
  (get-prop image prop))

(defmethod (setf image-prop) (val image prop)
  (setf (get-prop image prop) val))

(defmethod remove-image-prop (image prop)
  (rem-prop image prop))

(defmethod image-dimensions (image)
  (list (image-x-dim image) (image-y-dim image)))

;;; This expects a Common Lisp array element type specifier such as (unsigned-byte 8)
(defun element-size-from-element-type (type)
  (case type
    (double-float 64)
    (single-float 32)
    (bit 1)
    (rgb8 24) (rgba8 32)
    (otherwise (cond ((consp type)
		      (if (integerp (cadr type))
			  (cadr type)
			  (loop for band-type in type
				sum (element-size-from-element-type band-type))))
		     ((eq type t) 32)
		     (t (error "unknown image element type ~a" type))))))

(defmethod image-element-size (image)
  (declare (type image image)
	   (values fixnum))
  (element-size-from-element-type (image-element-type image)))

(defmethod image-type ((image image))
  (class-name (class-of image)))

(defmethod name ((image image))
  (or (get-prop image :name) (get-prop image :pathname) "<unnamed image>"))

(defun image-floatp (image)
  (floatp (image-initial-value image)))

(defun image-fixp (image)
  (not (floatp (image-initial-value image))))

;;;(defmethod describe-image (image &optional (stream t))
;;;  (let ((xdim (image-x-dim image))
;;;        (ydim (image-y-dim image))
;;;        (element-type (image-element-type image))
;;;        ;(bpp (image-element-size image))
;;;        (bx (image-block-x-dim image))
;;;        (by (image-block-y-dim image))
;;;        (type (image-type image)))
;;;    (if (and (eql xdim bx) (eql ydim (abs by)))
;;;        (format stream "#<~a ~dx~d ~a block-dims RASTER #X~16,8,Vr>"
;;;                type xdim ydim element-type #\0 (%pointer image))
;;;        (format stream "#<~a ~dx~d ~a block-dims (~dx~d) #X~16,8,Vr>"
;;;                type xdim ydim element-type bx by #\0 (%pointer image)))))

(defmethod describe-image (image &optional (stream t))
  (let ((xdim (image-x-dim image))
	(ydim (image-y-dim image))
	(element-type (image-element-type image))
	;;(bpp (image-element-size image))
	(bx (image-block-x-dim image))
	(by (image-block-y-dim image)))
    (print-unreadable-object (image stream :type t :identity t)
      (if (and (eql xdim bx) (eql ydim (abs by)))
	  (format stream "~dx~d ~a block-dims RASTER" xdim ydim element-type)
	  (format stream "~dx~d ~a block-dims (~dx~d)" xdim ydim element-type bx by)))))

(defmethod print-object ((image image) stream)
  (describe-image image stream))

#|
(defmethod print-object ((image image) stream)
  (format stream "#<IMAGE #X~16,8,Vr> is a structure of type IMAGE~%" #\0 (%pointer image))
  (format stream "X-DIM: ~D~%" (image-x-dim image))
  (format stream "Y-DIM: ~D~%" (image-y-dim image))
  (format stream "ELEMENT-TYPE: ~a~%" (image-element-type image))
  (format stream "BLOCK-X-DIM: ~D~%" (image-block-x-dim cimg))
  (format stream "BLOCK-Y-DIM: ~D~%" (image-block-y-dim cimg))
  )
|#

;;; FIXME: Missing function: INHERITABLE-PROPERTIES
;;; Missing slots: IMAGE-WRITE-ACTION IMAGE-WRITE-LOCK
(defmethod clone-image-after-copy-slots (image new-image)
  ;;(setf (property-list new-image) (inheritable-properties image))
  (setf (image-prop new-image :indirected-to) image)
  (push new-image (image-prop image :inferiors))
  ;; FIXME -- unfinished
  ;;(setf (image-write-action new-image) #'do-copy-on-write)
  ;;(setf (image-write-lock new-image) t)
  )


;;; This needs work
(defmethod image-indirected-to ((image image))
  (image-prop image :indirected-to))

(defmethod image-indirected-p ((image image))
  (image-indirected-to image))


(defmethod set-image-maps ((image image) x-map y-map)
  (setf (image-x-map image) x-map
	(image-y-map image) y-map
	(image-x-dim image) (length x-map)
	(image-y-dim image) (length y-map)
	)
  ;;(remove-internal-image-prop image :window-of) ;; CME6 had this
  )

(defmacro image-window-of (image)
  `(image-prop ,image :window-of))


;;; ********************  SCAN LINE BUFFERS  ********************


;;; This machinery is more general than its use in the image ffi.
;;; This should be used for vectors passed to foreign code if there is
;;; any possibility of the foreign code trigering a GC.

(deftype image-int-buffer-element-type () '(signed-byte 32))
(deftype image-float-buffer-element-type () 'double-float)

(deftype image-int-buffer-type () '(simple-array (signed-byte 32) (*)))
(deftype image-float-buffer-type () '(simple-array  double-float (*)))

(eval-when (eval load compile)

;;; x86 gencgc implementations do conservative marking of the stack (including the C call-frames)
;;; so Lisp arrays are ok to pass into C even when callbacks to Lisp might occur.
(defconstant *scanline-buffers-are-stationary-p* 
  (or #+(or allegro 
	    (and (or :cmu :sbcl) (not (and (or x86 x86-64) gencgc)))) 
      t))
) ;end eval-when


;;; Wrap calls to MAKE-SCAN-LINE-BUFFER with WITH-SCAN-LINE-BUFFERS
;;; FIXME -- BROKEN FOR COLOR IMAGES.
(defmethod make-scan-line-buffer (image &optional length element-type)
  (unless element-type
    (setq element-type
	  (if (image-fixp image)
	      'image-int-buffer-element-type
	      'image-float-buffer-element-type)))
  ;; Note: this will not work if the image is (unsigned-byte 32), but
  ;; then again, the image-int-buffer-element-type will be wrong for
  ;; such images - also, two type designations might not be eq.  Use subtypep
  (let* ((zero (if (subtypep element-type 'image-int-buffer-element-type) 0 0.0)) ; SUCKS
	 (length (or length (image-x-dim image)))
	 (dims (case (image-element-type image)
		 (rgb8 (setq element-type '(unsigned-byte 8)) (list length 3))
		 (rgba8 (setq element-type '(unsigned-byte 8)) (list length 4))
		 (otherwise length))))

    (if *scanline-buffers-are-stationary-p*
	(qffi::make-stationary-array dims :element-type element-type :initial-element zero)
	(make-array dims :element-type element-type :initial-element zero)
	)))

(defmethod unmake-scan-line-buffers (&rest arrays)
  (declare (ignorable arrays))
  ;; Explicit calls to unmake-foreign-vector is needed in CMUCL non GENCGC variants,
  ;; but not in Allegro which does a stationary garbage collect of the static area,
  ;; and not with gencgc where the scan-line-buffers are allocated in dynamic space.
  #-allegro
  (when *scanline-buffers-are-stationary-p*
    (loop for arr in arrays
	  when (consp arr)
	    do (apply 'unmake-scan-line-buffers arr)
	  else do (qffi::unmake-stationary-array arr))))

;;; Wrap calls to MAKE-DFLOAT-SCAN-LINE-BUFFER with WITH-SCAN-LINE-BUFFERS
(defmethod make-dfloat-scan-line-buffer ((image t) &optional length)
  (make-scan-line-buffer image length 'image-float-buffer-element-type))

;;; Wrap calls to MAKE-INTEGER-SCAN-LINE-BUFFER with WITH-SCAN-LINE-BUFFERS
(defmethod make-integer-scan-line-buffer ((image t) &optional length)
  (make-scan-line-buffer image length 'image-int-buffer-element-type))

(defmethod make-scan-line-buffers (image &optional length element-type)
  (make-scan-line-buffer image length element-type))

(defmethod make-dfloat-scan-line-buffers ((image t) &optional length)
  (make-dfloat-scan-line-buffer image length))

(defmethod make-integer-scan-line-buffers ((image t) &optional length)
  (make-integer-scan-line-buffer image length))


(defun image-element-type-from-make-scan-line-buffer-spec (spec)
  ;;(break)
  (eval spec))

;;; Except for Allegro, you must UNMAKE-SCAN-LINE-BUFFERS to reclaim their storage.
(defmacro with-scan-line-buffers (let-list &body body)
  (loop for (var form) in let-list
	for buffer-element-type
	  = (and (consp form)
		 (case (car form)
		   (make-scan-line-buffer
		    (and (> (length form) 3)
			 (image-element-type-from-make-scan-line-buffer-spec (fourth form))))
		   (make-dfloat-scan-line-buffer 'image-float-buffer-element-type)
		   (make-integer-scan-line-buffer 'image-int-buffer-element-type)))
	when buffer-element-type
	  collect `(type (simple-array ,buffer-element-type (*)) ,var)
	    into decls
	collect var into arr-vars
	finally
     (if *scanline-buffers-are-stationary-p*
	 (return `(let ,let-list
		   (declare . ,decls)
		   (unwind-protect
			(progn . ,body)
		     (unmake-scan-line-buffers . ,arr-vars)
		     )))
	 (return `(let ,let-list
		   (declare . ,decls)
		   . ,body)))
	))


#| Example:

(with-scan-line-buffers
    ((prev-line (make-dfloat-scan-line-buffer gauss-image padded-x-range ))
     (line      (make-dfloat-scan-line-buffer gauss-image padded-x-range ))
     (next-line (make-dfloat-scan-line-buffer gauss-image padded-x-range ))
     (s-line  (make-dfloat-scan-line-buffer image block-x-dim )))
  ...)

|#			 

;;; REQUIRE-IMAGE-ROWS is used to control the number of pages in the image-page-pool according
;;; the number of rows of pixels of the specified images required by BODY.

(defmacro require-image-rows (image-row-specs &body body)
  `(progn ,@(loop for image-row-spec in image-row-specs
		  collect (if (consp image-row-spec)
			      `(add-to-working-set ,(car image-row-spec) :nrows ,(cadr image-row-spec))
			      `(add-to-working-set ,image-row-spec :nrows 1)))
    (unwind-protect
	 (progn . ,body)
      .,(loop for image-row-spec in image-row-specs
	      collect (if (consp image-row-spec)
			  `(remove-from-working-set ,(car image-row-spec) :nrows ,(cadr image-row-spec))
			  `(remove-from-working-set ,image-row-spec :nrows 1))))))

#| Example

(require-image-rows ((img 2) into-img)
		    body
		    )

|#




(defmethod similar-image ((image image) &rest keys &key element-type &allow-other-keys)
  (declare (dynamic-extent keys))
  (apply 'img::make-image  (list (img::image-x-dim image) (img::image-y-dim image))
	 :element-type (or element-type (img::image-element-type image))
	 keys))


(defmethod copy-image ((image scalar-image) &optional into-image)
  (let* ((xdim (image-x-dim image))
	 (ydim (image-y-dim image))
	 (image2 (or into-image
		     (make-image (list xdim ydim)
				 :element-type (image-element-type image)))))
    (with-scan-line-buffers ((buf (make-integer-scan-line-buffer image)))
      (loop for y fixnum from 0 below ydim
	    do (image-getline image buf 0 y)
	       (image-putline image2 buf 0 y))
      image2)))

(defun retile-image (image blkx blky)
  (copy-image image
	      (make-image (image-dimensions image)
			  :element-type (image-element-type image)
			  :block-x-dim blkx
			  :block-y-dim blky)))

#+unused ;; #-cmu
(defun image-slot-values (img)
  (list (c-image-x-dim img)
	(c-image-y-dim img)
	(c-image-element-size img)
	(c-image-element-type-code img)
	(c-image-block-x-dim img)
	(c-image-block-y-dim img)
	;;(c-image-c++header img)
	))


#|
(setq img2 (make_image 256 256 ))
(list (c-image-x-dim img)
      (c-image-y-dim img)
      (c-image-element-size img)
      (c-image-block-x-dim img)
      (c-image-block-y-dim img)
      (c-image-c++header img)
      )

(describe img)

(let* ((xdim (c-image-x-dim img))
       (arr (make-array0 xdim :element-type '(signed-byte 32)))
       (code (image_getline_int img arr 0 0 xdim)))
  (values arr code))
(let* ((xdim (c-image-x-dim img))
       (arr (make-array0 xdim :element-type 'double-float))
       (code (image_getline_double img arr 0 0 xdim)))
  (values arr code))

(iref img 0 0)
(diref img 0 0)

(setq img2 (make-image '(256 256)))
(iref img2 0 0)
(iset img2 0 0 3)

(setf (environment-variable "CME_TMP_FILE_IMAGE_DIR")
      "/homedir/quam/tmp/CME_TMP_FILE_IMAGE_DIR")
(setq img3 (make-image '(256 256) :element-type 'double-float))
(setq img3 (make-image '(256 256) :element-type 'single-float))
(setq img3 (make-image '(64 64) :element-type 'double-float))


|#


;;; The next should probably live elsewhere, such as a file for image methods.

;;; ***************************  IMAGE METHODS  ***************************


;;; Support some of the major methods of BASIC-COORDINATE-SYSTEM
(define-soft-slot image :2d-world parent)

;;; NOTE:  (method 2d-world image) is redefined in view.lisp
;;; to automatically generate a 2d-world
(define-soft-slot image :2d-world 2d-world)

;;; Support some of the major methods of BASIC-COORDINATE-SYSTEM
(define-soft-slot image :image-to-2d-transform object-to-parent-transform)

;;; NOTE:  (method image-to-2d-transform image) is redefined in view.lisp
;;; to automatically generate an image-to-2d-transform
(define-soft-slot image :image-to-2d-transform image-to-2d-transform)

(defun probe-image-to-2d-transform (image)
  (when image (get-prop image :image-to-2d-transform)))

;;; FIXME?  Should a coordinate-system hierarchy be allowed for images?
;;;         This requires that the parent of an image is always a 2d-world.
(defmethod world ((image image))
  (parent image))

;;; For compatibility with coordinate-systems.
(defmethod object-to-world-transform ((image image) &optional world)
  (declare (ignore world))
  (image-to-2d-transform image))

(defmethod 3d-to-2d-projection ((image image))
  (3d-to-2d-projection (2d-world image)))

#+never ; package problems calling MAKE-2D-WORLD
(progn

(defparameter *automatically-generate-bogus-image-2d-world* nil)

;;; Support some of the major methods of BASIC-COORDINATE-SYSTEM
(defmethod parent ((image image))
  (or (get-prop image :2d-world)
      (and *automatically-generate-bogus-image-2d-world*
	   (setf (get-prop image :2d-world) (make-2d-world)))))  ; package problems here

); end progn


;;; ********************   IREF ISET MACROS   ********************

;;; Set this to non-NIL to cause ALL IREF code to include type and bounds checking.
(defparameter *iref-macro-bounds-checking* nil
  "Compile iref code for optional runtime type and bounds checking.")
;(setq *iref-macro-bounds-checking* t)

;;; Dynamically (or globally) bind this to enable bounds and type checks at runtime.
(defparameter *iref-bounds-and-type-check* t
  "Enable runtime IREF bounds and type checking.")

(defun iref-bounds-and-type-check (image x y)
  #+cmu (declare (ext:optimize-interface (safety 3)))
  (declare (fixnum x y))
  (declare (type image image))
  (when (or (< x 0) (>= x (the fixnum (image-x-dim image))) 
	    (< y 0) (>= y (the fixnum (image-y-dim image))))
    (error "Image iref bounds violation ~a ~a ~a" image x y)))

(declaim (ftype (function (image fixnum fixnum) fixnum) bounds-checking-iref))
(defun bounds-checking-iref (image x y)
  (declare (fixnum x y) (values fixnum))
  (when *iref-bounds-and-type-check*
    (iref-bounds-and-type-check image x y))
  (funcall (image-iref-fn image) image x y))

(declaim (ftype (function (image fixnum fixnum fixnum) fixnum) bounds-checking-iset))
(defun bounds-checking-iset (image x y val)
  (declare (fixnum x y val) (values fixnum))
  (when *iref-bounds-and-type-check*
    (iref-bounds-and-type-check image x y))
  (funcall (image-iset-fn image) image x y val))

(declaim (ftype (function (image fixnum fixnum) (dvector 1)) bounds-checking-diref))
(defun bounds-checking-diref (image x y)
  (declare (fixnum x y) (values (dvector 1)))
  (when *iref-bounds-and-type-check*
    (iref-bounds-and-type-check image x y))
  (funcall (image-diref-fn image) image x y))

(declaim (ftype (function (image fixnum fixnum (dvector 1)) (dvector 1)) bounds-checking-diset))
(defun bounds-checking-diset (image x y darray)
  (declare (fixnum x y) (type (dvector 1) darray) (values (dvector 1)))
  (when *iref-bounds-and-type-check*
    (iref-bounds-and-type-check image x y))
  (funcall (image-diset-fn image) image x y darray))

;;; The *double-aref-array* only for scalar-image diset
;;; NO, cannot use defconstant -- compiler complains when setting elements.
;;;   "Warning: Destructive function LISP::%ASET called on constant data."
;;;(defconstant *double-aref-array* (make-array 1 :element-type 'double-float))
(defvar *double-aref-array* (make-array 1 :element-type 'double-float))
(declaim (type (simple-array double-float (1)) *double-aref-array*))

;;; Compile code in an environment which compiles function calls to 
;;; bounds-checking versions of IREF/ISET rather than inline calls to 
;;; optimized, non-bounds checking functions.
(defmacro with-iref-checking (&body body)
  (if *iref-macro-bounds-checking*
       `(progn .,body) ; avoid doing the checking twice.
       `(macrolet ((iref (image x y)
		     `(the fixnum (bounds-checking-iref ,image ,x ,y)))
		   (iset (image x y val)
		     `(bounds-checking-iset  ,image ,x ,y ,val))
		   (diref (image x y)
		     `(the double-float
			(aref (the (simple-array double-float (*)) 
				(bounds-checking-diref ,image ,x ,y))
			      0)))
		   (diset (image x y val)
		     `(let ((%darray% *double-aref-array*))
			(prog1 (setf (aref %darray% 0) ,val)
			  (bounds-checking-diset ,image ,x ,y %darray%)))))
	  .,body)))

#|
(iref img 1 2)
(with-iref-checking (iref img 1 2))
|#


#+old ; #+allegro 
(progn

(defmacro iref (image x y)
  `(let ((%image% ,image))
    (the fixnum (funcall (the function (image-iref-fn %image%))
			 %image% ,x ,y))))

(defmacro iset (image x y val)
  `(let ((%image% ,image))
    (funcall (the function (image-iset-fn %image%)) %image% ,x ,y ,val)))

(defmacro diref (image x y)
  `(let* ((%image% ,image))
     (declare (type image %image%))
     (the double-float
       (aref (the (simple-array double-float (1))
	       (funcall (the function (image-diref-fn %image%))
			%image% ,x ,y nil))
	     0))))

(defmacro diset (image x y val)
  `(let ((%image% ,image)
	 (%darray% *double-aref-array*))
     (declare (type image %image%))
     (prog1 (setf (aref %darray% 0) ,val)
       (funcall (the function (image-diset-fn %image%)) %image% ,x ,y %darray%))))


) ; end progn


;;#+cmu  ; new iref macros supporting bounds and type checking
(progn 
;;; These macros are useful to compile all code with iref checking enabled,
;;; without the need for inserting (WITH-IREF-CHECKING ...) into the code.

(defmacro iref (image x y)
  `(the fixnum
     ,(if *iref-macro-bounds-checking*
	  `(bounds-checking-iref ,image ,x ,y)
	  `(let ((%image% ,image))
	     (funcall (the function (image-iref-fn %image%)) %image% ,x ,y)))))

(defmacro iset (image x y val)
  (if *iref-macro-bounds-checking*
      `(bounds-checking-iset  ,image ,x ,y ,val)
      `(let ((%image% ,image))
	 (funcall (the function (image-iset-fn %image%)) %image% ,x ,y ,val))))


(defmacro diref (image x y)
  (if *iref-macro-bounds-checking*
      `(the double-float
	 (aref (the (simple-array double-float (1)) (bounds-checking-diref ,image ,x ,y)) 0))

      `(let* ((%image% ,image))
	 (declare (type image %image%))
	 (the double-float
	   (aref (the (simple-array double-float (1)) 
		   (funcall (the function (image-diref-fn %image%))
			    %image% ,x ,y))
		 0)))))

(defmacro diset (image x y val)
  (if *iref-macro-bounds-checking*
      `(let ((%darray% *double-aref-array*))
	 ;; stupid Allegro doesn't type propagate the declaration for *double-aref-array*
	 (declare (type (simple-array double-float (1)) %darray%))
	 (prog1 (setf (aref %darray% 0) ,val)
	   (bounds-checking-diset ,image ,x ,y %darray%)))
      `(let ((%darray% *double-aref-array*)
	     (%image% ,image))
	 ;; stupid Allegro doesn't type propagate the declaration for *double-aref-array*
	 (declare (type (simple-array double-float (1)) %darray%))
	 (declare (type image %image%))
	 (prog1 (setf (aref %darray% 0) ,val)
	   (funcall (the function (image-diset-fn %image%)) %image% ,x ,y %darray%)))))

;(diset img a b 12.3)
#|
(defun foo (img i j v)
  (setf (diref img i j) v))

(defun foo1 (x)
 ; (declare (fixnum x))
  x)

(defun foo (x)
  (let ((y x))
    (declare (fixnum y))
    (foo1 y)))

(foo pi)
    
(disassemble 'foo)

(setq *img* (make-image '(4 4) :element-type '(unsigned-byte 8)))

(foo *img* 0 0 1.0)
(diref *img* 0 0)
(foo *img* 0 0 1)
(iref *img* 0 0)
(describe (BOUNDS-CHECKING-DIREF *IMG* 0 0))
(iref *img* 4 4)
(iref *img* pi 0)
(trace bounds-checking-iref )
(image-iref-fn *img*)
(trace ARRAY-IMAGE-UNSIGNED-8BIT-IREF)
(iref-type-check *img* pi 0)
(iref-type-check nil 0 0)
|#
)  ; end progn

(defsetf iref iset)
(defsetf diref diset)

;;; These handle iref of weird element types or vector-images
(defun undefined-iref-fn (image x y)
  (declare (ignore x y))
  (error "iref not supported for ~a" image))

(defun undefined-diref-fn (image x y)
  (declare (ignore x y))
  (error "diref not supported for ~a" image))

(defun undefined-iset-fn (image x y val)
  (declare (ignore x y val))
  (error "iset not supported for ~a" image))

(defun undefined-diset-fn (image x y val)
  (declare (ignore x y val))
  (error "diset not supported for ~a" image))

(defun undefined-viref-fn (image x y &optional buf)
  (declare (ignore x y buf))
  (error "viref not supported for ~a" image))

(defun undefined-vdiref-fn (image x y &optional buf)
  (declare (ignore x y buf))
  (error "vdiref not supported for ~a" image))

(defun undefined-viset-fn (image x y val)
  (declare (ignore x y val))
  (error "viset not supported for ~a" image))

(defun undefined-vdiset-fn (image x y val)
  (declare (ignore x y val))
  (error "vdiset not supported for ~a" image))


;;;
;;;

;;; No definition yet.
(defmethod unmake-image (image &rest args &key expunge &allow-other-keys)
  nil)

;;; Lots of older code uses this, and it's probably a good thing to
;;; have in a production setting (lots of spurious images that need to
;;; be reaped):

(defmacro with-local-images (image-list &body body)
  (let* ((vars (loop for item in image-list
		     for var = (if (symbolp item) item (car item))
		     collect var)))
    `(let* ,vars
       (unwind-protect
	 (progn (progn . ,(loop for item in image-list
				when (listp item) collect `(setq . ,item)))
		. ,body)
	 (return-images . ,(reverse vars))))))

(defmacro return-images (&rest var-list)
  `(progn . ,(loop for var in var-list
		   collect
		   `(when ,var
		      (unmake-image (prog1 ,var (setq ,var nil))
				    :expunge t :verbose 'return-fails)))))

;;; Definitions supporting the generation of the IREF/ISET functions

(defvar *image-package* (find-package "IMG"))

;;; *Xlib-bitmap-index-xor* should really be computed after X11 is started up,
;;; but we want to know it at compile time ....

;;; This is correct for SunOS/Sparc, Irix/Mips, and Linux/X86
(defconstant *Xlib-bitmap-index-xor* #+BIG-ENDIAN 0 #+LITTLE-ENDIAN 31)

;;; FIXME?  Is this relevent with the OpenGL implementation?
;;; This makes CME bit images compatible with Xlib bitmap indexing so that
;;; XPutImage will work.  
(defmacro maybe-reverse-bit-order (index element-type)
  (if (and (eq element-type 'bit) (not (zerop *Xlib-bitmap-index-xor*)))
      `(the fixnum (logxor ,*Xlib-bitmap-index-xor* (the fixnum ,index)))
      index))

(defparameter *enable-image-iref-bounds-checking* nil)


#+unused
(progn

;;; There is little reason for the class specific pixel accessors to do bounds checking since
;;; the IREF/ISET macros handle the option for IREF-BOUNDS-CHECKING.
;;; The only possible reason would be to reduce the code footprint for checking.

(defmacro image-bounds-check ()
  (when *enable-image-iref-bounds-checking*
    '(when (or (< x 0) (< y 0) (>= x (image-x-dim image)) (>= y (image-y-dim image)))
      (error "Image iref bounds violation ~a ~a ~a" image x y))))

); end #+unused progn

(defun iref-preamble ()
  `(#+cmu (declare (ext::optimize-interface (ext:inhibit-warnings 1) (speed 3) (safety 0) (debug 0))
		   (optimize (ext:inhibit-warnings 1)))
	  (declare (optimize (speed 3) (safety 0) (debug 0)))
	  (declare (type fixnum x y))
	  ;; Possibly add bounds checking here.  See note above.
	  ))

(defmacro with-pixel-ref-macros ((element-type) &body body)
  `(macrolet ((compute-pixel-index ()
		'(the fixnum
		  (+ (the fixnum (aref (the image-map-type (image-x-map image)) x))
		     (the fixnum (aref (the image-map-type (image-y-map image)) y)))))
	      (with-darray (&body body)
		`(let* ((darray *double-aref-array*))
		   (declare (type (simple-array double-float (*)) darray))
		   ,@body))
		    
	      ,@(case element-type
		      (single-float '((element-type->fixnum (value) `(round! ,value))
				      (element-type->dfloat (value) `(float ,value 0.0d0))
				      (fixnum->element-type (value) `(float ,value 0.0f0))
				      (dfloat->element-type (value) `(float ,value 0.0f0))))
		      (double-float '((element-type->fixnum (value) `(round! ,value))
				      (element-type->dfloat (value) value)
				      (fixnum->element-type (value) `(float value, 0.0d0))
				      (dfloat->element-type (value) value)))
		      (otherwise    '((element-type->fixnum (value) value)
				      (element-type->dfloat (value) `(float (the fixnum ,value) 0.0d0))
				      (fixnum->element-type (value) value)
				      (dfloat->element-type (value) `(the fixnum (round! ,value)))))))
     ,@body))

;;; change to a config variable?
(defvar *all-scalar-image-element-types*
  '(bit
    (unsigned-byte 8) (unsigned-byte 16) (unsigned-byte 32)
    (signed-byte 8) (signed-byte 16) (signed-byte 32)
    single-float double-float))

;;; FUNCTION TO COMPILE COMPLEX TOP-LEVEL FORMS (rather than just lambda expressions).
#+cmu
(defun compile-top-level-form (form)
  (with-input-from-string (stream (with-output-to-string (st)  (print form st)))
    (ext:compile-from-stream 
     stream)))

#+sbcl
(progn ;; from swank-sbcl.lisp

(sb-alien:define-alien-routine "tmpnam" sb-alien:c-string
  (dest (* sb-alien:c-string)))

(defun temp-file-name ()
  "Return a temporary file name to compile strings into."
  (concatenate 'string (tmpnam nil) ".lisp"))
;(temp-file-name)

(defun compile-top-level-form (form &optional (package *image-package*))
  (let ((tmp-file (temp-file-name))
	)
    (with-open-file (st tmp-file :direction :output)
      (format st "~a~%" `(in-package ,(package-name package)))
      (pprint form st))
    (let ((output-file (compile-file tmp-file)))
      (when output-file
	(load output-file)
	(delete-file output-file)
	(delete-file tmp-file)))))

) ; end progn


#+allegro
(progn 

#+never ; requires swank to be loaded
(defun compile-top-level-form (form &optional (package *image-package*))
  (swank::swank-compile-string (format nil "~s" form)))

;;; based on swank-allegro.lisp swank-compile-string
(defun compile-top-level-form (form &optional (package *image-package*))
  (allegro-compile-from-temp-file
   (format nil "~S ~S~%~S" 
	   `(in-package ,(package-name package))
	   `(eval-when (:compile-toplevel :load-toplevel)) ;; what is this?
	   form)))

;;; from swank-allegro
(defun allegro-call-with-temp-file (fn)
  (let ((tmpname (system:make-temp-file-name)))
    (unwind-protect
         (with-open-file (file tmpname :direction :output :if-exists :error)
           (funcall fn file tmpname))
      (delete-file tmpname))))

;;; from swank-allegro
(defun allegro-compile-from-temp-file (string)
  (allegro-call-with-temp-file 
   (lambda (stream filename)
       (write-string string stream)
       (finish-output stream)
       (let ((binary-filename
              (excl:without-redefinition-warnings
                ;; Suppress Allegro's redefinition warnings; they are
                ;; pointless when we are compiling via a temporary
                ;; file.
                (compile-file filename :load-after-compile t))))
         (when binary-filename
           (delete-file binary-filename))))))


) ; end progn

;(fmakunbound 'get-image-iref-fn-symbols)
(defmethod get-image-iref-fn-symbols (image-class &rest args)
  (let* ((img-package *image-package*)
	 (bounds-check *enable-image-iref-bounds-checking*)
	 (chk-str (if bounds-check "CHECKING-" "")))
    (flet ((make-function-symbol (iref-name)
	     (intern (format nil "~a~{-~a~}-~a~a" image-class args chk-str iref-name)
		     img-package )))
      (values (make-function-symbol "IREF")
	      (make-function-symbol "ISET")
	      (make-function-symbol "DIREF")
	      (make-function-symbol "DISET")))))

(defgeneric generate-image-ref-fns (image-class  &key &allow-other-keys))

(defmacro def-image-ref-fns (image-class &rest keyvals)
  (apply 'generate-image-ref-fns image-class keyvals))

(defun element-type-to-name (element-type)
  (or (cadr (assoc element-type *image-element-type-to-c-name-alist*
		   :test 'equal))
      (error "Illegal image element type ~a" (setq *foo* element-type))))

;(fmakunbound 'get-image-ref-fns)
(defun get-image-ref-fns (image-class &rest keyvals)
  (flet ((extract-keyval-vals (keyvals)
	   (loop for (key val) on keyvals by #'cddr 
		 collect (if (eql key :element-type)
			     (element-type-to-name val)
			     val))))
    (mv-bind (iref-fn iset-fn diref-fn diset-fn) 
	(apply 'get-image-iref-fn-symbols image-class (extract-keyval-vals keyvals))
      (unless (fboundp iref-fn)
	(config::with-optimization-profile (:fastest-unsafe)
	  (compile-top-level-form (apply 'generate-image-ref-fns image-class keyvals))))

      (values (symbol-function iref-fn)
	      (symbol-function iset-fn)
	      (symbol-function diref-fn)
	      (symbol-function diset-fn)))))
