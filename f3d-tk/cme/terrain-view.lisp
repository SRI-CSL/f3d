(in-package :gui)

(defstruct-class dtm-tex-mapped-quad-mesh (obj::gl-3d-object-mixin obj::gl-dynamic-object-mixin obj::gl-object )
  ((texture-image :initarg :texture-image)
   (selected-texture-image :initform nil)
   (lvcs-vertex-array :initarg :lvcs-vertex-array :accessor lvcs-vertex-array)
   texture-x-dim texture-y-dim
   tile-width tile-height
   tiles-wide tiles-hi
   x-pixels-per-vertex y-pixels-per-vertex
   x-verts-per-tile y-verts-per-tile
   (full-bbox :initform nil)
   bbox
   ))

(defmethod initialize-instance :after ((object dtm-tex-mapped-quad-mesh) &key &allow-other-keys)
  (with-class-slots dtm-tex-mapped-quad-mesh
	(texture-image selected-texture-image tiles-wide tiles-hi full-bbox) object
    ;;(setf selected-texture-image texture-image)
    (initialize-slots object 0)))

(defmethod initialize-slots ((object dtm-tex-mapped-quad-mesh) level)
  (with-class-slots dtm-tex-mapped-quad-mesh
	(texture-image
	 lvcs-vertex-array
	 selected-texture-image
	 texture-x-dim texture-y-dim
	 tile-width tile-height
	 tiles-wide tiles-hi
	 x-pixels-per-vertex y-pixels-per-vertex
	 x-verts-per-tile y-verts-per-tile
	 full-bbox bbox)
      object
    (let ((image (if (= level 0)
		     texture-image
		     (get-image-pyramid-level texture-image level 1))))
      (unless (eq image selected-texture-image)
  
	(multiple-value-bind (tilewidth tileheight) (image_tex_tile_dims texture-image)
	  (let ((udim (array-dimension lvcs-vertex-array 0))
		(vdim (array-dimension lvcs-vertex-array 1)))
	    (setf selected-texture-image image
		  texture-x-dim (image-x-dim selected-texture-image)
		  texture-y-dim (image-y-dim selected-texture-image)
		  tile-width tilewidth
		  tile-height tileheight
		  tiles-wide (floor texture-x-dim tile-width)
		  tiles-hi (floor texture-y-dim tile-height)
		  x-pixels-per-vertex (floor texture-x-dim (1- udim))
		  y-pixels-per-vertex (floor texture-y-dim (1- vdim))
		  x-verts-per-tile (round tile-width x-pixels-per-vertex)
		  y-verts-per-tile (round tile-height y-pixels-per-vertex))))))
    
    (unless full-bbox
      (setf full-bbox (list 0 (1- tiles-wide) 0 (1- tiles-hi))))
    (setf bbox (destructuring-bind (umin umax vmin vmax) full-bbox
		 (list (ash umin (- level)) (ash umax (- level))
		       (ash vmin (- level)) (ash vmax (- level))))
	  )))

(defmethod set-bbox ((object dtm-tex-mapped-quad-mesh) new-bbox)
  (destructuring-bind (umin umax vmin vmax) new-bbox
    (with-class-slots dtm-tex-mapped-quad-mesh
	  (texture-image full-bbox tile-width tile-height) object
      (let* ((texture-x-dim (image-x-dim texture-image))
	     (texture-y-dim (image-y-dim texture-image))
	     (tiles-wide (floor texture-x-dim tile-width))
	     (tiles-hi (floor texture-y-dim tile-height)))
	(setf full-bbox (list (max 0 umin) (min umax (1- tiles-wide))
			      (max 0 vmin) (min vmax (1- tiles-hi))))))))

(defun render-texture-tile (lvcs-vertex-array bbox x-verts-per-tile y-verts-per-tile)
  (declare (fixnum x-verts-per-tile y-verts-per-tile))
  (declare (optimize (speed 3)(safety 1)))
  (declare (type (simple-array t (* *)) lvcs-vertex-array))
  ;;(format t "render-texture-tile ~a ~a~%" bbox (list x-verts-per-tile y-verts-per-tile))
  (destructuring-bind (u0 u1 v0 v1) bbox
    (declare (fixnum u0 u1 v0 v1))
    (setq u1 (min u1 (1- (array-dimension lvcs-vertex-array 0)))
	  v1 (min v1 (1- (array-dimension lvcs-vertex-array 1))))
    ;; #+never
    (loop with 1/x-vpt double-float = (/ 1.0 x-verts-per-tile)
	  with 1/y-vpt double-float = (/ 1.0 y-verts-per-tile)
	  for v fixnum from v0 below v1
	  for tv double-float from 0.0 by 1/y-vpt
	  do (glBegin GL_TRIANGLE_STRIP)
	     (loop for u fixnum from u0 to u1
		   for tu double-float from 0.0 by 1/x-vpt
		   do (glTexCoord2d tu (+ tv 1/y-vpt)) ; double-float consing here
		      (glVertex3dv (aref lvcs-vertex-array u (1+ v)))
		      (glTexCoord2d tu tv)             ; double-float consing here
		      (glVertex3dv (aref lvcs-vertex-array u v)))
	     (glEnd))))

(defun render-texture-tile (lvcs-vertex-array bbox x-verts-per-tile y-verts-per-tile)
  (declare (fixnum x-verts-per-tile y-verts-per-tile))
  (declare (optimize (speed 3)(safety 1)))
  (declare (type (simple-array t (* *)) lvcs-vertex-array))
  ;;(format t "render-texture-tile ~a ~a~%" bbox (list x-verts-per-tile y-verts-per-tile))
  (destructuring-bind (u0 u1 v0 v1) bbox
    (declare (fixnum u0 u1 v0 v1))
    (setq u1 (min u1 (1- (array-dimension lvcs-vertex-array 0)))
	  v1 (min v1 (1- (array-dimension lvcs-vertex-array 1))))
    ;; #+never
    (let ((tile-pos0 (cv 0.0 0.0))
	  (tile-pos1 (cv 0.0 0.0)))
      (declare (type (simple-array double-float (*)) tile-pos0 tile-pos1))

      (loop with 1/x-vpt double-float = (/ 1.0 x-verts-per-tile)
	    with 1/y-vpt double-float = (/ 1.0 y-verts-per-tile)
	    for v fixnum from v0 below v1
	    for tv double-float from 0.0 by 1/y-vpt
	    do (setf (aref tile-pos0 1) (+ tv 1/y-vpt)
		     (aref tile-pos1 1) tv)
	       (glBegin GL_TRIANGLE_STRIP)
	       (loop for u fixnum from u0 to u1
		     for tu double-float from 0.0 by 1/x-vpt
		     do (setf (aref tile-pos0 0) tu
			      (aref tile-pos1 0) tu)
			(glTexCoord2dv tile-pos0)
			(glVertex3dv (aref lvcs-vertex-array u (1+ v)))
			(glTexCoord2dv tile-pos1) ; double-float consing here
			(glVertex3dv (aref lvcs-vertex-array u v)))
	       (glEnd)))))

#|
(disassemble 'render-texture-tile)
|#

(defmethod render-texture ((object dtm-tex-mapped-quad-mesh))
  (declare (optimize (speed 3)(safety 1)))
  (with-class-slot-values dtm-tex-mapped-quad-mesh
	(selected-texture-image
	 lvcs-vertex-array
					;texture-x-dim texture-y-dim
	 tile-width tile-height
					;tiles-wide tiles-hi
					;x-pixels-per-vertex y-pixels-per-vertex
	 x-verts-per-tile y-verts-per-tile
	 bbox)
      object
    (declare (fixnum tile-height tile-width x-verts-per-tile y-verts-per-tile))

    (let* ((udim (array-dimension lvcs-vertex-array 0))
	   (vdim (array-dimension lvcs-vertex-array 1)))
      (declare (fixnum udim vdim))
      ;;(format t "render-texture ~a~%" (list x-verts-per-tile y-verts-per-tile))
      (glPushAttrib GL_ALL_ATTRIB_BITS)
      (glPolygonMode GL_FRONT GL_FILL)
      ;;(glPolygonMode GL_BACK GL_FILL)
      (glDisable GL_LIGHTING)
      (glShadeModel GL_FLAT)
      ;;(glDisable GL_CULL_FACE)
      (glEnable GL_CULL_FACE)
      (glFrontFace GL_CCW)
      (let ((k .8)) (glColor3d k k k))
      
      (glMatrixMode GL_TEXTURE) (glLoadIdentity)
      (glMultMatrixd_transposed
       (make-and-fill-4x4-matrix 1.0 0.0 0.0 0.0
				 0.0 -1.0 0.0 1.0 ; patch is top-to-bottom ordered
				 0.0 0.0 1.0 0.0
				 0.0 0.0 0.0 1.0))
      (glEnable GL_TEXTURE_2D)
      (unwind-protect 
	   ;;nil #+never
	   (destructuring-bind (u0 u1 v0 v1) bbox
	     (declare (fixnum u0 u1 v0 v1))
	     (loop with xstart fixnum = (* u0 tile-width)
		   with xend fixnum = (* u1 tile-width)
		   with ustart fixnum = (* u0 x-verts-per-tile)
		   with ystart fixnum = (* v0 tile-height)
		   with yend fixnum = (* v1 tile-height)
		   with vstart fixnum = (* v0 y-verts-per-tile)
		   for y fixnum from ystart to yend by tile-height
		   for v fixnum from vstart below vdim by y-verts-per-tile
		   do (loop for x fixnum from xstart to xend by tile-width
			    for u fixnum from ustart below udim by x-verts-per-tile
			    do (setup-texmap-tile (image-id selected-texture-image) x y)
			       (render-texture-tile lvcs-vertex-array
						    (list u (+ u x-verts-per-tile) v (+ v y-verts-per-tile))
				      x-verts-per-tile y-verts-per-tile))))
	;; clean-up forms
	(glPopAttrib))
      
      (handle_gl_errors "render-texture")
      )))

(defparameter *dtm-tex-mapped-quad-mesh-image-level* 0)
(defparameter *dtm-tex-mapped-quad-mesh-image-level* 1)


(defmethod obj::draw-object ((object dtm-tex-mapped-quad-mesh) view)
  (declare (ignorable view))
  (initialize-slots object *dtm-tex-mapped-quad-mesh-image-level*)
  (render-texture object)
  )



(defun make-2d-to-window-matrix (win 2d-pos-at-window-center 2d-to-window-scale)
  (mv-bind (width height) (dimensions win)
    (bind-vector-elements (uc vc) 2d-pos-at-window-center
      (let* ((scale 2d-to-window-scale)
             (xoff (- (* .5 width) (* scale uc)))
             (yoff (+ (* .5 height) (* scale vc))))
        (make-array '(4 4) :element-type 'double-float
                    :initial-contents
                    `((,scale 0.0 0.0 ,xoff)
                      (0.0 ,(- scale) 0.0 ,yoff)
                      (0.0 0.0 1.0 0.0)
                      (0.0 0.0 0.0 1.0)))))))

#|
(maybe-compile-file-load "$FREEDIUS/lisp/cme-compat/orthorectify.lisp")
(maybe-compile-file-load "$FREEDIUS/lisp/cme-compat/terrain-view.lisp")
(maybe-compile-file-load "$FREEDIUS/lisp/basic-gui/special-objects.lisp")

(setq tex-mesh (make-instance 'dtm-tex-mapped-quad-mesh
			      :world (cme::get-3d-world-named "alv")
			      :object-to-world-transform (make-4x4-coordinate-transform
							  (make-4x4-identity-matrix))
			      :texture-image *ortho*
			      :lvcs-vertex-array *lvcs-verts*))



(let* ((win (selected-window))
       (rotation-center (cv 256.9 4532.1 6015.4))
       (projection (make-adjustable-camera rotation-center
					   :height 1e3 :gsd 2.0
					;:win win :fov 20.0
					   ))
       (3d-world (cme::get-3d-world-named "alv"))
       (2d-world (cme::make-2d-world :3d-world 3d-world
				     :3d-to-2d-projection projection))
       (view (make-view win :2d-world 2d-world
			:2d-to-window-transform (make-4x4-coordinate-transform
						 (make-2d-to-window-matrix win (cv 0.0 0.0) 1.0))))
       (obj (make-instance 'perspective-transform-stare-point-object
			   :projection projection
			   :world 3d-world
			   :object-to-world-transform
			   (make-object-to-parent-matrix (cv 256.9 4532.1 6020.0))))
       (object-set (make-object-set (list tex-mesh) 
				    :world 3d-world
				    :immediate-render-p t
				    :backing-store-p t
				    :direct-render-p t))
       (object-set2 (make-object-set (list obj) 
				     :world 3d-world
				     :immediate-render-p t
				     :backing-store-p t
				     ;:direct-render-p t
				     )))
  (setf (get-prop view :inhibit-scale-rot-rotation) t)
					;(setf (get-prop view :world-center-of-rotation) rotation-center)
  (setf (get-prop object-set2 :name) "stare-pt"
	 (get-prop object-set :name) "terrain")
  (push object-set2 (object-sets view))
  (push object-set (object-sets view))
  (setf (get-prop view :sun-vector) (cv 0.6082805 -0.1486257 0.7796828 0.0))
  (push-view view win)
  )





(let* ((win (selected-window))
       (view (top-view win))
       (tex-mesh (make-instance 'dtm-tex-mapped-quad-mesh
				:world (3d-world view)
				:object-to-world-transform (make-4x4-coordinate-transform
							    (make-4x4-identity-matrix))
				:texture-image *ortho*
				:lvcs-vertex-array *lvcs-verts*))


       (object-set (make-object-set (list tex-mesh) 
				    :world (3d-world view)
					;:immediate-render-p t
					;:backing-store-p t
					;:direct-render-p t
				    )))
  (setf (get-prop view :inhibit-scale-rot-rotation) t) ; is this needed?
  (setf (get-prop object-set :name) "terrain")
  (push object-set (object-sets view))
  (setf (get-prop view :sun-vector) (cv 0.6082805 -0.1486257 0.7796828 0.0))
  )

















(let* ((win (selected-window))
       (rotation-center (cv 256.9 4532.1 6015.4))
       (2d-world (cme::make-2d-world
		  :3d-world (cme::get-3d-world-named "Alv")
		  :3d-to-2d-projection
		  (make-adjustable-camera rotation-center
					  :height 20000.0 :gsd 2.0
					  ;:win win :fov 20.0
					  )))
       (view (make-view 
	      win
	      :2d-world 2d-world
	      :2d-to-window-transform
	      (make-4x4-coordinate-transform  (make-2d-to-window-matrix win (cv 0.0 0.0) 1.0))))
       
       )
  (setf (get-prop view :inhibit-scale-rot-rotation) t)
  (setf (get-prop view :world-center-of-rotation) rotation-center)
  (push-view view win)
  )


(setq tex-mesh (make-instance 'dtm-tex-mapped-quad-mesh
			      :world (cme::get-3d-world-named "Olalla")
			      :object-to-world-transform (make-4x4-coordinate-transform
							  (make-4x4-identity-matrix))
			      :texture-image *olalla-ortho*
			      :lvcs-vertex-array *olalla-lvcs-verts*))

(0 32 0 41) ; 41 is double what it should be 

(set-bbox tex-mesh '(10 32 0 20))
(set-bbox tex-mesh '(2 32 10 30))

(progn (set-bbox tex-mesh '(4 32 4 40))
       (setq *dtm-tex-mapped-quad-mesh-image-level* 1))


(progn (set-bbox tex-mesh '(10 32 12 30))
       (setq *dtm-tex-mapped-quad-mesh-image-level* 0))

(progn (set-bbox tex-mesh '(16 32 16 36))
       (setq *dtm-tex-mapped-quad-mesh-image-level* 0))

(progn (set-bbox tex-mesh '(18 30 18 34))
       (setq *dtm-tex-mapped-quad-mesh-image-level* 0))

;; This one runs fast -- no tile swapping
(progn (set-bbox tex-mesh '(18 30 18 34))                      
       (setq *dtm-tex-mapped-quad-mesh-image-level* 0))

(progn (set-bbox tex-mesh '(20 32 20 40))                      
       (setq *dtm-tex-mapped-quad-mesh-image-level* 0))

(img::release-image-pool-textures nil 1)

(setq *olalla-center-of-rotation* (selected-object-world-position))
(setq *olalla-center-of-rotation* (cv -522.1892809752424 -1625.4550389975034 720.0))

(img::resize-image-page-pool *olalla-ortho* 400)
(img::resize-texid-page-pool 400)
(img::resize-texid-page-pool 200)

(let* ((win (selected-window))
       (rotation-center *olalla-center-of-rotation*)
       (2d-world (cme::make-2d-world
		  :3d-world (cme::get-3d-world-named "Olalla")
		  :3d-to-2d-projection
		  (make-adjustable-camera rotation-center
					  :height 1e3 :gsd 2.0
					  :win win :fov 20.0
					  )))
       (view (make-view 
	      win
	      :2d-world 2d-world
	      :2d-to-window-transform
	      (make-4x4-coordinate-transform  (make-2d-to-window-matrix win (cv 0.0 0.0) 1.0))))
       
       (object-set (make-object-set (list tex-mesh)
				    :world (3d-world view)
				    :immediate-render-p t
				    :backing-store-p t
				    :direct-render-p t)))
  (setf (get-prop view :inhibit-scale-rot-rotation) t)
  (setf (get-prop view :world-center-of-rotation) rotation-center)
  (Push object-set (object-sets view))
					;(setf (get-prop view :sun-vector) (cv 0.6082805 -0.1486257 0.7796828 0.0))
  (push-view view win)
  )


;;; Use Views/Make Adjustable Copy 

(let* ((win (selected-window))
       (rotation-center *olalla-center-of-rotation*)
       (2d-world (cme::make-2d-world
		  :3d-world (cme::get-3d-world-named "Olalla")
		  :3d-to-2d-projection (3d-to-2d-projection (top-view))))
       (view (make-view 
	      win
	      :2d-world 2d-world
	      :2d-to-window-transform
	      (make-4x4-coordinate-transform  (make-2d-to-window-matrix win (cv 0.0 0.0) 1.0))))
       
       (object-set (make-object-set (list tex-mesh)
				    :world (3d-world view)
				    :immediate-render-p t
				    :backing-store-p t
				    :direct-render-p t)))
  (setf (get-prop view :inhibit-scale-rot-rotation) t)
  (setf (get-prop view :world-center-of-rotation) rotation-center)
  (Push object-set (object-sets view))
					;(setf (get-prop view :sun-vector) (cv 0.6082805 -0.1486257 0.7796828 0.0))
  (push-view view win)
  )







(clear-view-stack (selected-window))

(setq  *degrees-per-window-unit* .1)

(OpenGL-reset-state)
(clear-view-stack (selected-window))
(trace render-texture-tile)
(untrace)

(describe *ortho*)
(describe *olalla-ortho*)
(describe *lvcs-verts*)
(image_tex_tile_dims *ortho*)
(image_tex_tile_dims *olalla-ortho*)
(/ 4352 256.0)
(/ 4352 136.0)

(img::resize-image-page-pool *ortho* 100)
(img::resize-image-page-pool *olalla-ortho* 100)
(img::resize-texid-page-pool 200)
(img::release-image-pool-textures nil 1)

(img::read-image-property-list-string "$RADIUS/site-2d-worlds/alv/alv-2-44/full-tiff/image.g3")
(img::read-image-property-list-string "$RADIUS/site-2d-worlds/alv/alv-2-44/full-IUTB/image.g5")
(img::read-image-property-list-string "$RADIUS/site-2d-worlds/alv/alv-oblique-tower/image.g0")
(img::read-image-property-list-string "$HOME/pix/rugby.pic")
(img::read-image-property-list-string "$RADIUS/sites/alv/alv-dtm.g0")
(img::read-image-property-list-string "$RADIUS/site-2d-worlds/Olalla/7189_70/win.tif")
(img::read-image-property-list-string "$RADIUS/site-2d-worlds/san-diego/IKONOS/tiff_rpc/stereo_8bit_pan_tiff/po_39940_pan_0000010000.tif")

(defun read-depth-buffer (&optional (window (selected-window)))
  (gl::with-gl-window (window)
    ;; (glMakeCurrent window)
    (glReadBuffer GL_DEPTH)
    (mv-bind (width height) (dimensions window)
      ;;(glViewPort  0 0 width height)
      (let* ((image (img::make-raster-image (list width height)
					    :element-type 'single-float :top-to-bottom-p t))
	     (array (img::image-array image)))
	(glReadPixels 0 0 width height GL_DEPTH_COMPONENT GL_FLOAT array)
	
	image))))

(defun invert-project (i j &optional (view (selected-view)))
  (let* ((depth-image (read-depth-buffer))
	 (projection (3d-to-2d-projection view))
	 (z (diref depth-image i j )))
    (inverse-transform-vector
     projection
     (inverse-transform-vector
      (2d-to-window-transform view)
      (cv (dfloat i) (dfloat (- (window-height (view-window view)) 1 j)) z) ))))   


(defun invert-project (i j &optional (view (selected-view)))
  (let* ((depth-image (read-depth-buffer))
	 (projection (3d-to-2d-projection view))
	 (z (diref depth-image i j )))
    (inverse-transform-vector
     projection
     (inverse-transform-vector
      (2d-to-window-transform view)
      (cv (dfloat i) (dfloat (- (window-height (view-window view)) 1 j)) z) ))))   


(let ((view (selected-view)))
  (bind-vector-elements (u v w)
      (transform-vector (list (3d-to-2d-projection view) (2d-to-window-transform view))
			(selected-object-world-position))
    (cv u v w)
    ;;(cv u (- (window-height (view-window view)) 1 v) w)
    ))

(dimensions (selected-window))
(setq depth-image (read-depth-buffer))

(let ((u 256) (v 199))
  (values (diref (read-depth-buffer) u v)
	  (invert-project u v)
	  (selected-object-world-position)))


(selected-object-world-position)

(defun glproject3d (pt &optional (window (selected-window)))
  (gl::with-gl-window (window)
    (let ((view (top-view window)))
      (glMakeCurrent window)
      (mv-bind (width height) (dimensions window)
	(glViewPort 0 0 width height))
      (set-3d-matrices view)
      (values (glProject_to_window pt)
	      (transform-vector (list (3d-to-2d-projection view) (2d-to-window-transform view)) pt)))))

(glproject3d (selected-object-world-position))

(defun glproject3dx (pt &optional (window (selected-window)))
  (gl::with-gl-window (window)
    (let ((view (top-view window)))
      ;; (glMakeCurrent window)
      (mv-bind (width height) (dimensions window)
	(glViewPort 0 0 width height)
	(glMatrixMode GL_PROJECTION) (glLoadIdentity)
	;;(set-2d-to-ndc-matrix (make-4x4-identity-matrix ))
	(glOrtho 0.0 1.0 0.0 1.0 .5 1.5))
      ;;(glOrtho 0.0 (dfloat width) (dfloat height) 0.0 .5 1.5))
					;(glOrtho 0.0 (dfloat width) 0.0 (dfloat height) .5 1.5))
      (glMatrixMode GL_MODELVIEW) (glLoadIdentity)
      (values (glProject_to_window pt)))))


(glproject3dx (cv 0.0 0.0 -1.0)) ; #(0.0 399.0 0.5)
(glproject3dx (cv 1.0 1.0 -1.0)) ; #(0.0 399.0 0.5)
(dimensions (selected-window))

(defun ortho-test (pt &optional (window (selected-window)))
  (let ((projmat (make-4x4-identity-matrix)))
    (glMakeCurrent window)
    (mv-bind (width height) (dimensions window)
      (glViewPort 0 0 width height)
      (glMatrixMode GL_MODELVIEW) (glLoadIdentity)
      (glMatrixMode GL_PROJECTION) (glLoadIdentity)
      (glOrtho 0.0 1.0 0.0 1.0 .5 1.5)
      (gl::glGetDoublev GL_PROJECTION_MATRIX projmat)
      (values (transpose-matrix projmat)
	      (glProject_to_window pt))
      )))
;;; w from glProject_to_window will be linear with distance between near and far
  
(ortho-test (cv 0.0 0.0 -1.0))
 #2A((2.0 0.0 0.0 -1.0) (0.0 2.0 0.0 -1.0) (0.0 0.0 -2.0 -2.0) (0.0 0.0 0.0 1.0))
#(0.0 399.0 0.5)

(ortho-test (cv 0.0 0.0 -1.5))
#2A((2.0 0.0 0.0 -1.0) (0.0 2.0 0.0 -1.0) (0.0 0.0 -2.0 -2.0) (0.0 0.0 0.0 1.0))
#(0.0 399.0 1.0)

(ortho-test (cv 0.0 0.0 -.5))
#2A((2.0 0.0 0.0 -1.0) (0.0 2.0 0.0 -1.0) (0.0 0.0 -2.0 -2.0) (0.0 0.0 0.0 1.0))
#(0.0 399.0 0.0)

(ortho-test (cv 0.0 0.0 -.6))
#2A((2.0 0.0 0.0 -1.0) (0.0 2.0 0.0 -1.0) (0.0 0.0 -2.0 -2.0) (0.0 0.0 0.0 1.0))
#(0.0 399.0 0.09999999999999998)


(defun perspective-test (pt &optional (window (selected-window)))
  (let ((projmat (make-4x4-identity-matrix)))
    (glMakeCurrent window)
    (mv-bind (width height) (dimensions window)
      (glViewPort 0 0 width height)
      (glMatrixMode GL_MODELVIEW) (glLoadIdentity)
      (glMatrixMode GL_PROJECTION) (glLoadIdentity)
      (gl::gluPerspective 10.0 1.0 .5 1.5)
      (gl::glGetDoublev GL_PROJECTION_MATRIX projmat)
      (let ((projmat (transpose-matrix projmat)))
	(values projmat
		(glProject_to_window pt)
		(transforms::4x4-project-vector projmat pt))
	))))

(setq *projmat* (perspective-test (cv 0.0 0.0 -1.0)))
#2A((11.430052757263184 0.0 0.0 0.0)
    (0.0 11.430052757263184 0.0 0.0)
    (0.0 0.0 -2.0 -1.5)
    (0.0 0.0 -1.0 0.0))

(perspective-test (cv 0.0 0.0 -1.0))
#(256.0 199.0 0.75)
#(0.0 0.0 0.5)
  
(perspective-test (cv 0.0 0.0 -.5))
#(256.0 199.0 0.0)
#(0.0 0.0 -1.0)

(perspective-test (cv 0.0 0.0 -.6))
#(256.0 199.0 0.24999999999999994)
#(0.0 0.0 -0.5000000000000001)

(perspective-test (cv 0.0 0.0 -1.5))
#(256.0 199.0 1.0)
#(0.0 0.0 1.0)

(make-4x4-identity-matrix (transform-matrix (2d-to-window-transform (top-view))))

(defun glproject2d (pt &optional (window (selected-window)))
  (let ((view (top-view window)))
    (glMakeCurrent window)
    (mv-bind (width height) (dimensions window)
      (glViewPort 0 0 width height))
    (set-2d-matrices view)
    (values  (glProject_to_window pt)
	     (transform-vector (2d-to-window-transform view) pt)
	     )))


(defun glproject2dx (pt &optional (window (selected-window)))
  (let ((view (top-view window)))
    (glMakeCurrent window)
    (mv-bind (width height) (dimensions window)
      (glViewPort 0 0 width height))
    (set-2d-matrices view)
    (values  (glProject_to_window pt)
	     (transform-vector (2d-to-window-transform view) pt)
	     )))



(glproject2d (cv 0.0 0.0 0.0))  ; #(0.0 -1.0 0.5)
(glproject2d (cv 0.0 (dfloat (window-height (selected-window))) 0.0)) ; #(0.0 -1.0 0.5)


(progn (glMakeCurrent (selected-window))
       (set-3d-matrices (top-view))
       (glProject_to_world xxx)
       )

(transform-vector (list (3d-to-2d-projection (top-view)) (2d-to-window-transform (top-view)))
		  (selected-object-world-position))

(bind-vector-elements (u v)
    (transform-vector (list (3d-to-2d-projection (top-view)) (2d-to-window-transform (top-view)))
		       (selected-object-world-position))
  (values (invert-project (round u) (- (window-height (selected-window)) 1 (round v)))
	  (selected-object-world-position)))
  
(diref (read-depth-buffer) 67 288)
(invert-project 256 204))
(selected-object-world-position)

(diref (read-depth-buffer) 263 198)
(invert-project 263 198)
(selected-object-world-position)




(diref depth-image 200 200)
(invert-project 261 209)
(cv -126.3 4736.1 6067.5)
(invert-project 

(progn (setq depth-image (read-depth-buffer))
       (list (diref depth-image 0 0)
	     (diref depth-image (1- (image-x-dim depth-image)) (1- (image-y-dim depth-image)) )))

(extract-near-far (3d-to-2d-projection (top-view)))
(3d-to-2d-projection (top-view))
|#
