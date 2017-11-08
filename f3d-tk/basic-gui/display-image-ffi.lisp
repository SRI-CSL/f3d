(in-package :gui)

(defvar *enable-overlay-images* nil)

;;; DISPLAY-IMAGE assumes caller has done (glMakeCurrent window)
(defmethod display-image ((view view) base-image 2d-to-window-matrix
			  &rest args &key (max-mipmap-level 1)
			  (min-level *display-image-min-level*)
					;photometric-transform
			  image-modulation-color
			  )
  (declare (ignorable min-level))
  (unless *inhibit-image-display*
    (let* ((img (select-display-image-image view base-image 2d-to-window-matrix)))
      (when img
	(let* ((image-to-2d-matrix (transform-matrix (image-to-2d-transform img)))
	       (display-mode *gl-display-image-mode*))
	  ;;(setq *foo88* (list img (transform-matrix (image-to-2d-transform img))))
	  (glDisable GL_POLYGON_STIPPLE)
	  (glMatrixMode GL_TEXTURE) (glLoadIdentity)
	  (progn (glHint GL_PERSPECTIVE_CORRECTION_HINT  GL_FASTEST) (glDisable GL_DITHER))

	  (maybe-photometric-transform-for-display view img base-image)

	  ;; display-image modulates this color
	  ;; image-modulation-color could be changed to 1.0 and photometric-transform
	  ;; used to attenuate the image, at some cost in performance.
	  ;; FIXME:  change this to glColor4dv to allow alpha
	  ;(glColor3dv (image-modulation-color (display-attributes view)))
	  (glcolor (or image-modulation-color (image-modulation-color (display-attributes view))))
	  
	  ;; Using display_tiled_image2 allows for C to have no pyramids or
	  ;; image_to_2d_transforms The glue tieing everything together is maintained in Lisp
	  ;; only.
	  (display_tiled_image2 (image-id img)
				2d-to-window-matrix
				image-to-2d-matrix
				max-mipmap-level
				display-mode ; 0 = glTexMap, 1 = glDrawPixels
				*display_tiled_image-cvt-mode* ; cvt_mode
				)
	  (when *enable-overlay-images*
	    (loop for (image . modulation-color) in (get-prop view :overlay-images)
		  for img2 = (select-display-image-image view image 2d-to-window-matrix)
		  ;; Assume images agree with above...
		  ;; for matrix = (transform-matrix (image-to-2d-transform img2))
		  do (glEnable GL_BLEND)
		     ;; (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
		     ;; (glBlendFunc GL_ONE GL_ONE)
		     (glBlendFunc GL_SRC_ALPHA GL_ONE)
		     (glColor modulation-color)
		     (display_tiled_image2 (image-id img2)
			     2d-to-window-matrix
			     image-to-2d-matrix
			     max-mipmap-level
			     display-mode ; 0 = glTexMap, 1 = glDrawPixels
			     *display_tiled_image-cvt-mode* ; cvt_mode
			     )))

	  )))))


#+unused
(defun raw-display-image (img 2d-to-window-matrix 
			  &optional 
			  (image-to-2d-matrix (transform-matrix (image-to-2d-transform img)))
			  (max-mipmap-level 999))
  (glDisable GL_POLYGON_STIPPLE)
  (glMatrixMode GL_TEXTURE)
  (progn (glHint GL_PERSPECTIVE_CORRECTION_HINT  GL_FASTEST) (glDisable GL_DITHER))
  (display_tiled_image2 (image-id img)
			2d-to-window-matrix
			image-to-2d-matrix
			max-mipmap-level
			*gl-display-image-mode*	; 0 = glTexMap, 1 = glDrawPixels
			*display_tiled_image-cvt-mode* ; cvt_mode
			))






;;; pyramid_level=1000 means display_tiled_image must choose the level.
;;; pyramid_level=1001 means use img as is.
#+never ; no longer used
(def-foreign-function (display_tiled_image (:name (freedius-prefix "display_tiled_image")))
   (img (:pointer c-image))
  (mat_2d_to_window :simple-array)	;(trans (:pointer t-trans))
  (pyramid-level :int)
  (max_mipmap_level :int)
  (draw_mode :int);; 0 means use texture mapping. 1 means draw pixels (ugh).
  (cvt_mode :int);; luminance-to-rgba conversion (-1 means no conversion)
  )

(def-foreign-function (display_tiled_image2 (:name (freedius-prefix "display_tiled_image2")))
   (img (:pointer c-image))
  (mat_2d_to_window :simple-array) ;(trans (:pointer t-trans))
  (mat_image_to_2d :simple-array-or-null) ; (mat_image_to_2d :simple-array) ;(trans (:pointer t-trans))
  (max_mipmap_level :int)
  (draw_mode :int);; 0 means use texture mapping. 1 means draw pixels (ugh).
  (cvt_mode :int);; luminance-to-rgba conversion (-1 means no conversion)
  )



;;;
;;; Oh does this really belong here??
;;;
(def-foreign-function (inhibit_tile_conversion (:name (freedius-prefix "InhibitTileConversion")))
  (state gl::GLint))

(defun inhibit-tile-conversion (t-or-nil)
  ;; (setq *luminance-conversion* (if t-or-nil -1 1))
  (inhibit_tile_conversion
   (if t-or-nil 1 0))
  t-or-nil)

#+mswindows
(st::add-system-initialization
 :basic-gui
 '(progn
    (format t "~%Enabling luminance conversion for image tiles.")
    (inhibit-tile-conversion nil)))


(def-foreign-function (setup-texmap-tile (:name (freedius-prefix "setup_texmap_tile")))
    (img (:pointer c-image))
  (x :int)
  (y :int))

(def-foreign-function (texmap_1_tile (:name (freedius-prefix "texmap_1_tile")))
    (img (:pointer c-image))
 (level :int)
  (z_image_to_2d_mat :simple-array) ; 4x4 double
  (vertx :int)
  (verty :int)
  (vertex_array :simple-array)
  (verts_wide :int)
  (verts_hi :int)
  )

(def-foreign-function (image_tex_handler_page_size
		       (:name (freedius-prefix "image_tex_handler_page_size")))
    (img (:pointer c-image))
  (dims-array :simple-array) ; array unsigned-byte 32
)

(defun image_tex_tile_dims (image)
  (let ((dims (make-array 2 :element-type '(signed-byte 32) :initial-element 0)))
    (image_tex_handler_page_size (image-id image) dims)
    (values (aref dims 0) (aref dims 1))))




(in-package :img)

;;; I believe this is to only call to make-foreign-pointer in the entire system
;;; Can probably eliminate using a trivial c-function.
(defparameter *null-image*
  #+allegro
  (make-foreign-pointer :address 0 :type '(:pointer c-image))
  #+cmu
  (make-foreign-pointer :address 0 :type (* (alien::struct c-image)))
  #+sbcl
  (make-foreign-pointer :address 0 :type (* (sb-alien::struct c-image)))
  )


(defparameter *inhibit-release-image-textures* nil)
;; LHQ Wed Apr 17 2002I tried setting *inhibit-release-image-textures* to t to see it it affected
;; the segfault problems, but it didn't appear to matter.

(defun release-image-textures (img &optional (release-texid 1))
  (unless *inhibit-release-image-textures*
    (release_image_textures (if img (image-id img) *null-image*) release-texid)))

(defun release-image-pool-textures (img &optional (release-texid 1))
  (unless *inhibit-release-image-textures*
    (release_image_pool_textures (if img (image-id img) *null-image*) release-texid)))



(def-foreign-function (resize-texid-page-pool (:name (freedius-prefix "resize_texid_page_pool")))
    (pool-size-change-count :int))


;;;(def-foreign-function (cget_image_xy_texid  (:name (freedius-prefix "get_image_xy_texid")))
;;;    (img (:pointer c-image))
;;;  x y
;;;  defined_ptr ;; (pointer int)
;;;  )

;;;(defun get-image-tile-texid (image x y)
;;;  (let* ((defined (make-array0 1 :element-type '(signed-byte 32)))
;;;        (texid (cget_image_xy_texid image x y defined)))
;;;    (values texid
;;;            (eql (aref defined 0) 1))))

(def-foreign-function (image_tex_pool_stats (:name (freedius-prefix "image_tex_pool_stats")))
    (img (:pointer c-image))
  (resetp :int)  ;&optional (resetp :int 1); what is the correct syntax here ?
  )
#|
(get-image-tile-texid  alv-2-44 0 0)
|#

(def-foreign-function ( release_image_textures (:name (freedius-prefix "release_image_textures")))
    (img (:pointer c-image))
  (release-texid :int))

(def-foreign-function ( release_image_pool_textures (:name (freedius-prefix "release_image_pool_textures")))
    (img (:pointer c-image))
  (release-texid :int))

(def-foreign-function (get_fill_tile_mipmaps_cnt (:name (freedius-prefix "get_fill_tile_mipmaps_cnt"))))
