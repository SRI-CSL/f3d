(in-package :obj)

(defstruct-class image-object (gl-xy-sizable-object-mixin  gl-2d-object-mixin gl-object)
  ((image :initform nil :initarg :image :accessor image)
   ))

(defmethod initialize-instance :after ((object image-object) &key &allow-other-keys)
  (setf (get-prop object :immediate-render-p) t) 
  (let ((image (image object)))
    (setf (sizes object) (cv (dfloat (img::image-x-dim image)) (dfloat (img::image-y-dim image)) 1.0)
	  (%vertex-array object) (make-vertex-array (cv 0.0 0.0 0.0)
						    (cv 1.0 0.0 0.0)
						    (cv 1.0 1.0 0.0)
						    (cv 0.0 1.0 0.0)))
    ))

(defmethod initialize-instance :after ((object image-object) &key &allow-other-keys)
  (setf (get-prop object :immediate-render-p) t) 
  (let* ((image (image object))
	 (xdim (dfloat (img::image-x-dim image)))
	 (ydim (dfloat (img::image-y-dim image))))
    (setf (sizes object) (cv xdim ydim 1.0)
	  (%vertex-array object) (make-vertex-array (cv 0.0 0.0 0.0)
						    (cv xdim 0.0 0.0)
						    (cv xdim ydim 0.0)
						    (cv 0.0 ydim 0.0)))
    ))

(defmethod draw-object ((obj image-object) view)
  (with-slots (image sizes) obj
    (bind-vector-elements (sx sy) sizes
      (gl::preserving-glAttributes
	(glDisable GL_DEPTH_TEST)
	(glDisable GL_CULL_FACE)	; not sure this is needed
	(when (get-prop obj :draw-outline)
	  (let* ((verts (vertex-array obj))
		 (nverts (array-dimension verts 0)))
	    (glBegin  GL_LINE_LOOP)
	    (loop for i of-type fixnum from 0 below nverts
		  do (glVertex3d (aref verts i 0) (aref verts i 1) (aref verts i 2)))
	    (glEnd)))	
	(gl::preserving-glPolygonMode
	  (glPolygonMode GL_FRONT_AND_BACK GL_FILL) ; needed for reflections
	  ;; FIXME:  This need to be done in an :after method to commands that modify the
	  ;;         object-to-parent-transform
	  (let ((ifd (gui::image-for-display image)))
	    (when (get-prop ifd :pyramid)
	      (img::recompute-image-to-2d-transforms (get-prop ifd :pyramid) 
						     (object-to-parent-transform obj))))
	  (bind-vector-elements (r g b a) (get-prop obj :blend-color)
	    (glEnable GL_BLEND)
	    ;;(glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
	    (glBlendFunc GL_CONSTANT_ALPHA_EXT GL_ONE_MINUS_CONSTANT_COLOR_EXT)
	    (macrolet ((f (x) `(float ,x 1.0f0)))
	      (gl::glBlendColor (f r) (f g) (f b) (f a))))
					;#+never
	  (gui::display-image view image (2d-to-window-matrix view) :max-mipmap-level 999))
	))))

(defmethod center-vertex ((object obj::image-object))
  (bind-vector-elements (x-size y-size) (sizes object)
    (cv (* .5 x-size) (* .5 y-size) 0.0)))

(in-package :gui)

(defmethod resize-object-xy ((object obj::image-object) interactor)
  (object-motion-operator object interactor #'obj::change-object-to-parent-scale))

(defmethod rotate-resize-object-xy ((object obj::image-object) interactor)
  (object-motion-operator object interactor #'obj::change-object-to-parent-scale-and-rotation))

(in-package :obj)

#|
(setq *img* (img::load-image "/opt/IU/radius/site-2d-worlds/alv/alv-oblique-tower/image.g0"))
(setq *img* (img::load-image "/homedir/quam/pix/forestcat.tif"))

(let* (;(*img* (img::load-image "/opt/IU/radius/site-2d-worlds/alv/alv-oblique-tower/image.g0"))
       (img *img*)
       (img2 (img::copy-image img)))
  (setf *img2* img2)
  (setf (image-to-2d-transform img2)
	(make-4x4-coordinate-transform 
	 (if t
	     (make-object-to-parent-matrix (cv 0.0 0.0 0.0))
	     (make-object-to-parent-matrix (cv 500.0 300.0 0.0))))
	(get-prop img2 :2d-world) (cme::get-2d-world-named "alv-2-44")))

(get-prop *img2* :pyramid)
(eq *img2* *)

(let* ((view (gui::selected-view))
       (img *img2*)
       ;;(world (get-prop img :2d-world))
       (world (gui::2d-world view))
       (obj (make-instance 'image-object :image img 
			   :parent world 
			   :object-to-parent-transform (image-to-2d-transform img)))
       (fs (gui::make-instance 'gui::2d-feature-set :world world))
       )
  (gui::add-object obj fs)
  (gui::add-feature-set fs world)
  (setq *obj1* obj)
  (setq *obj-set* fs)
  )

(center-vertex *obj1*)
(setf (get-prop *obj1* :blend-color) (cv .7 .7 .7 .7))

(get-prop (slot-value *obj1* 'image) :image-pyramid)

(setf (get-prop *obj1* :immediate-render-p) t)
(world *obj1*)
(pop (gui::object-sets (gui::selected-view)))
(gui::remove-feature-set *obj-set* (gui::2d-world (gui::selected-view)))


(setf (image-to-2d-transform *img2*) 
      (make-4x4-coordinate-transform  (make-object-to-parent-matrix (cv 0.0 0.0 0.0))))

(setf (image-to-2d-transform *img2*) 
      (make-4x4-coordinate-transform (make-object-to-parent-matrix (cv 500.0 300.0 0.0))))

(img::recompute-image-to-2d-transforms (get-prop *img2* :pyramid) 
				       (make-4x4-coordinate-transform 
					(make-object-to-parent-matrix (cv 500.0 100.0 0.0))))

(img::recompute-image-to-2d-transforms (get-prop *img2* :pyramid) 
				       (make-4x4-coordinate-transform 
					(make-and-fill-4x4-matrix 1.0 0.0 0.0 500.0
								  0.0 2.0 0.0 100.0
								  0.0 0.0 1.0 0.0
								  0.0 0.0 0.0 1.0)))

(img::recompute-image-to-2d-transforms (get-prop *img2* :pyramid) 
				       (make-4x4-coordinate-transform 
					(make-and-fill-4x4-matrix 1.0 0.0 0.0 500.0
								  -.7 1.0 0.0 100.0
								  0.0 0.0 1.0 0.0
								  0.0 0.0 0.0 1.0)))

(img::recompute-image-to-2d-transforms (get-prop *img2* :pyramid) 
				       (make-4x4-coordinate-transform 
					(make-and-fill-4x4-matrix 1.0 1.0 0.0 500.0
								  -1.0 1.0 0.0 300.0
								  0.0 0.0 1.0 0.0
								  0.0 0.0 0.0 1.0)))

(img::recompute-image-to-2d-transforms (get-prop *img2* :pyramid) 
				       (make-4x4-coordinate-transform 
					(make-and-fill-4x4-matrix 1.0 1.0 0.0 500.0
								  -0.1 1.0 0.0 300.0
								  0.0 0.0 1.0 0.0
								  0.0 0.0 0.0 1.0)))
(img::recompute-image-to-2d-transforms (get-prop *img2* :pyramid) 
				       (make-4x4-coordinate-transform 
					(make-and-fill-4x4-matrix 1.0 1.0 0.0 500.0
								  0.0 -1.0 0.0 300.0
								  0.0 0.0 1.0 0.0
								  0.0 0.0 0.0 1.0)))
(img::recompute-image-to-2d-transforms (get-prop *img2* :pyramid) 
				       (make-4x4-coordinate-transform 
					(make-and-fill-4x4-matrix .7 .7 0.0 500.0
								  -1.0 .5 0.0 300.0
								  0.0 0.0 1.0 0.0
								  0.0 0.0 0.0 1.0)))

(img::recompute-image-to-2d-transforms (get-prop *img2* :pyramid) 
				       (make-4x4-coordinate-transform 
					(make-and-fill-4x4-matrix 1.0 1.0 0.0 500.0
								  -.8 .5 0.0 300.0
								  0.0 0.0 1.0 0.0
								  0.0 0.0 0.0 1.0)))

(img::recompute-image-to-2d-transforms (get-prop *img2* :pyramid) 
				       (make-4x4-coordinate-transform 
					(make-and-fill-4x4-matrix 4.0 4.0 0.0 500.0
								  -3.2 2.0 0.0 300.0
								  0.0 0.0 1.0 0.0
								  0.0 0.0 0.0 1.0)))
(img::recompute-image-to-2d-transforms (get-prop *img2* :pyramid) 
				       (make-4x4-coordinate-transform 
					(make-and-fill-4x4-matrix 4.0 4.0 0.0 500.0
								  4.0 -4.0 0.0 300.0
								  0.0 0.0 1.0 0.0
								  0.0 0.0 0.0 1.0)))
(set-parent-transform *obj1* (image-to-2d-transform *img2*))


(world *obj1*)
(transform-matrix (image-to-2d-transform *img2*))

(gui::clear-view-stack (gui::selected-window))
(gui::pop-view (gui::selected-window))

(setq *alv-2-44* (gui::view-image (gui::top-view)))

(setq *img10* (img::image-window *alv-2-44* 2000 2000 64 64))

(setq *img10* (img::image-window *alv-2-44* 00 00 64 64))
(list (img::iref *img10* 0 0)
      (img::iref *alv-2-44* 0 0))

(setq *img10* (img::copy-image (img::image-window *alv-2-44* 00 00 64 64)))

(gui::push-image *img10* (gui::selected-window))

(let* ((view (gui::selected-view)))
  (setf (gui::object-sets view)) (copy-list (gui::object-sets view2)))

|#



(in-package :gui)

#+no-longer-needed
(progn 

(defparameter *texture-transform-matrix* nil)

#|
(setq *texture-transform-matrix* (make-and-fill-4x4-matrix 1.0 0.0 0.0 0.0
							   0.0 0.5 0.0 0.0
							   0.0 0.0 1.0 0.0
							   0.0 0.0 0.0 1.0))
|#

(defmethod hacked-display-image ((view view) image 2d-to-window-matrix
			  &rest args &key base (max-mipmap-level 1)
			  (min-level *display-image-min-level*)
			  )
  (declare (ignorable min-level))
  (unless *inhibit-image-display*
    (if (= *gl-display-image-mode* 1)
	(apply 'display-image-pixels view args)
     
	;; this assumes caller has done (glMakeCurrent window)
	(let* ((window (view-window view))
	       (reduce-levels (display-image-reduce-level window))
	       (image-to-window-matrix 
		(multiply-matrices 2d-to-window-matrix 
				   (transform-matrix (image-to-2d-transform image))))
	       (desired-level (tex-map-pyramid-level image-to-window-matrix base))
	       (level (floor (+ desired-level reduce-levels)))
	       (img (img::pyramid-level (image-for-display image) level))
	       (image-to-2d-matrix (transforms::transform-matrix (image-to-2d-transform img)))
	       ;; display_tiled_image calls get_image_pyramid_level unless pyramid-level = 1001
	       (display-mode *gl-display-image-mode*))
	  (setq *foo* (list level img image-to-2d-matrix))
	  ;;(format t "display-image ~a~%" (list min-level level3 red-level level))
	  (glDisable GL_POLYGON_STIPPLE)
	  (glMatrixMode GL_TEXTURE) (glLoadIdentity)
	  (when *texture-transform-matrix*  (glMultMatrix *texture-transform-matrix*))
	  ;;(fixup)
	  ;;#+mesa
	  (progn (glHint GL_PERSPECTIVE_CORRECTION_HINT  GL_FASTEST)
		 (glDisable GL_DITHER))

	  (maybe-photometric-transform-for-display view)

	  ;; display-image modulates this color
	  (glColor3dv (image-modulation-color (display-attributes view)) )
	  ;;(blend-hack t)
	  
	  ;; Connolly had a handler-bind around the next ?? 
	  (progn			;time
	    ;; Using display_tiled_image2 allows for C to have no pyramids or
	    ;; image_to_2d_transforms The glue tieing everything together is maintained in Lisp
	    ;; only.
	    (display_tiled_image2 (image-id img)
				  2d-to-window-matrix
				  image-to-2d-matrix
				  max-mipmap-level
				  display-mode ; 0 = glTexMap, 1 = glDrawPixels
				  *display_tiled_image-cvt-mode* ; cvt_mode
				  ))
	  ;;(blend-hack nil)
	  ))))

) ; end #+no-longer-needed progn





;;;; Connolly code:


#+connolly
(progn 

;;; When an person is tracked, an image chip (sample) is created that
;;; fills the bounding box of the mover.  The bounding box is defined
;;; with respect to the image coordinate system of the full video
;;; frame.  Thus, the chip can be thought of as an image that is
;;; related to its "parent" (the full frame video image) by an
;;; image-to-image coordinate transformation.
;;;
;;; This suggests a generic mechanism for displaying image chips with
;;; respect to a larger background image.  The idea is that, rather
;;; than store the full frame video, we store one background frame
;;; that serves as the parent.  Tracked objects can be shown on this
;;; frame as textured rectangles that are rendered in their proper
;;; size and position atop the background frame.
;;;
;;; This next should be sufficient to properly render a chip on a
;;; background frame.  Each image will have an associated
;;; image-to-2d-transform.  If the chip is derived from a track, then
;;; the track should be prepared such that it refers to one 2d world
;;; (corresponding to the source sensor) and each
;;; image-to-2d-transform is prepared using the bounding box for the
;;; corresponding frame.

(defstruct-class image-chip (obj::gl-object)
   ((image :initform nil :initarg :image))
   )


(defmethod world ((obj image-chip))
  (with-slots (image) obj
    (2d-world image)))

(defmethod image-to-2d-matrix (image)
  (transform-matrix (image-to-2d-transform image)))


;;;
;;; Ok, so should this really be the matrix that relates the chip to the parent frame??
(defmethod object-to-parent-matrix ((obj image-chip))
  ;; (with-slots (image) obj (image-to-2d-matrix image))
  gui::*identity-matrix*
  )

;;;
;;; Total hacks, to allow us to fiddle with different matrix ops:
;;;
(defvar *matrix* (make-4x4-identity-matrix))
(defvar *flip*
  (let ((m  (make-4x4-identity-matrix)))
;;    (setf (aref m 0 0) -1.0)
    (setf (aref m 1 1) -1.0)
    m))


;;;
;;; In this version, the rotation appears to be reversed.
;;;
#+inverted
(defun compute-2d-to-window-matrix (image view)
  (math::multiply-matrices
   *flip*
   (transform-matrix (2d-to-window-transform view))
   *matrix*)
;;    (setf (aref *matrix* 0 3) 0.0)
  (setf (aref *matrix* 1 3) (- (image-y-dim (gui::view-image view)) (aref *matrix* 1 3)))
;;    (setf (aref *matrix* 2 3) 0.0))
  *matrix*)



(defun compute-2d-to-window-matrix (image view)
  (math::multiply-matrices
   *flip*
   (transform-matrix (2d-to-window-transform view))
   *matrix*)
;;    (setf (aref *matrix* 0 3) 0.0)
  (setf (aref *matrix* 1 3) (- (image-y-dim (gui::view-image view)) (aref *matrix* 1 3)))
;;    (setf (aref *matrix* 2 3) 0.0))
  (matrix-invert-2d-rotation *matrix*)
  *matrix*)

#+never ;; Lynn's proposed solution:
(defun compute-2d-to-window-matrix (image view)
  (multiply-matrices (2d-to-window-matrix view)
		     (image-to-2d-matrix image)))




(defmethod draw-object ((obj image-chip) view)
  (unwind-protect
      (with-slots (image) obj
	  (glMatrixMode GL_PROJECTION) (glPushMatrix)
	  (glMatrixMode GL_MODELVIEW) (glPushMatrix)
	  (glPushAttrib GL_ALL_ATTRIB_BITS)
	  (glPolygonMode GL_FRONT GL_FILL)
	  (gui::display-image view image (compute-2d-to-window-matrix image view))
	  )
    (progn
      (glPopAttrib)
      (glMatrixMode GL_MODELVIEW) (glPopMatrix)
      (glMatrixMode GL_PROJECTION) (glPopMatrix)
      )))


) ; end #+connolly progn
