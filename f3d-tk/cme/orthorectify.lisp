(in-package :gui)

;;; what package should this be in?

#|


ORTHORECTIFY AN IMAGE TO A REGULAR-GRID TERRAIN MODEL


In order to use OpenGL texture mapping, we need texture tiles that
conform to rectangular patches of the terrain-model mesh.

The terrain-model grid is assumed to be simple mesh, uniformly
spaced in either lvcs, lat-long, or utm coordinates.

Rectification consists of texture mapping the image to an
orthographic projection of the terrain-model.

Since the OpenGL texture memory cannot be assumed to hold the 
entire image, we must devise a scheme to build the rectified
image in rectangular chunks, perhaps one tile at a time.

Assumptions:

     tile size: 256x256 or 512x512.  Must be a power of 2. Not wired in.

     image size: almost anything -- 4k x 4k and larger.

      
Limitations:

     Various combinations of terrain and image acquisition geometry
     causes parts of the terrain surface to be occluded in the image.

     There are 2 alteratives choices for the pixel values at occluded points:

          1. The pixel value at the projection of the point on the terrain into the image.
             This is easiest to implement.
      
          2. A special value such, as (0,0,0).
             This requires the detection of occluded points.


Approach:

     Given:  dtm-image - terrain mesh as an image

             dtm-to-lvcs-transform

             texture-image  - image to be orthorectified

             texture-image-to-2d-transform

             texture-image-3d-to-2d-projection


    Specify: dtm-image-to-ortho-image-transform

                usually u' = sv + b, v' = sv + c.

             Choose s such that s*tile-width and s*tile-height are both integers.

             For mipmap pyramids, it is best if s is an integer.

             texture-tile dimensions.




    Implementation:

             Let a point in the dtm-image be

               dtm-pt = (cv tmx tmy (iref dtm-image tmx tmy))

             Its lvcs coordinates are:

               lvcs-pt = (transform-vector dtm-to-lvcs-transform dtm-pt)

             Its texture-image coordinates are:

               texture-image-pt = (transform-vector texture-image-3d-to-2d-projection lvcs-pt)


             For each ortho-image tile there is a corresponding rectangular
	     sub-image of the dtm-image.  

	     For each dtm-pt in that dtm-image sub-image, compute the
	     corresponding texture-image-pt in the texture-image.  Determine a
	     sub-image of the texture-image that contains all of the
	     texture-image-pts.  Let tex-subimage-origin be the bottom left corner
	     of that sub-image.


	     Load an OpenGL texture map with the texture sub-image.    

	     Load a vertex-array with the dtm-pts of the dtm sub-image.
	     
             Load the texture-vertex-array with the texture-image-pts
             corresponding to mesh vertices for the sub-image with tex-subimage-origin subtracted.
	     
	     Set the MODELVIEW_MATRIX to identity

             Set the PROJECTION_MATRIX to the dtm-image-to-ortho-image-transform
             (with suitable enhancements to be a proper ortho projection).

             Texture-map a GL_TRIANGLE_STRIP for the texture sub-image.
            


|#


(defun inside-image-p (image pt)
  (bind-vector-elements (x y) pt
    (and (<= 0 x (image-x-dim image))
	 (<= 0 y (image-y-dim image)))))


(defun outside-bbox (image dtm-image dtm-to-image-transform )
  (flet ((col-outside-p (x)
	   (loop for y from 0 below (image-y-dim dtm-image)
		 never (inside-image-p image
				       (transform-vector dtm-to-image-transform (cv x y (diref dtm-image x y))))))
	 (row-outside-p (y)
	   (loop for x from 0 below (image-x-dim dtm-image)
		 never (inside-image-p image
				       (transform-vector dtm-to-image-transform (cv x y (diref dtm-image x y)))))))
    (let ((left (loop for x from 0 below (image-x-dim dtm-image)
		      while (col-outside-p x)
		      finally (return (max 0 (1- x)))))
	  (right (loop for x from (1- (image-x-dim dtm-image)) downto 0
		       while (col-outside-p x)
		       finally (return (min (1- (image-x-dim dtm-image))(1+ x)))))
	  (bottom (loop for y from 0 below (image-y-dim dtm-image)
			while (row-outside-p y)
			finally (return (max 0 (1- y)))))
	  (top (loop for y from (1- (image-y-dim dtm-image)) downto 0
		     while (row-outside-p y)
		     finally (return (min (1- (image-y-dim dtm-image))(1+ y))))))
	    
      (values left right bottom top))))

;;; this doesn't work
(defun inside-bbox (image dtm-image dtm-to-image-transform )
  (flet ((col-inside-p (x)
	   (loop for y from 0 below (image-y-dim dtm-image)
		 always (inside-image-p image
				       (transform-vector dtm-to-image-transform (cv x y (diref dtm-image x y))))))
	 (row-inside-p (y)
	   (loop for x from 0 below (image-x-dim dtm-image)
		 always (inside-image-p image
				       (transform-vector dtm-to-image-transform (cv x y (diref dtm-image x y)))))))
    (let ((left (loop for x from 0 below (image-x-dim dtm-image)
		      until (col-inside-p x)
		      finally (return x)))
	  (right (loop for x from (1- (image-x-dim dtm-image)) downto 0
		       until (col-inside-p x)
		       finally (return x)))
	  (bottom (loop for y from 0 below (image-y-dim dtm-image)
			until (row-inside-p y)
			finally (return y)))
	  (top (loop for y from (1- (image-y-dim dtm-image)) downto 0
		     until (row-inside-p y)
		     finally (return y))))
	    
      (cv left right bottom top))))
#|
(outside-bbox alv-2-44 alv-dtm (list alv-dtm-to-lvcs-transform alv-2-44-projection))
(inside-bbox alv-2-44 alv-dtm (list alv-dtm-to-lvcs-transform alv-2-44-projection))
(progn alv-dtm)
|#

(defun image-getline-clipped (image buf x y)
  (let ((n (length buf))
	(into-x 0)
	(xmax (image-x-dim image)))
    (img::fill-array buf 0)
    (when (and (>= y 0) (< y (image-y-dim image)))
      (when (< x 0)
	(setq into-x (- x)
	      n (+ n x)
	      x 0))
      (when (>= (+ x n) xmax)
	(setq n (- xmax x)))
      (when (> n 0)
	(image-getline image buf x y n into-x)))))


	    
;;; The specified window of texture-image is copied, centered in dest-image.
;;;(defun copy-texture-sub-image-to-image (texture-image bbox dest-image)
;;;  (unless (typep dest-image 'img::array-image)
;;;    (error "COPY-TEXTURE-SUB-IMAGE-TO-IMAGE: dest-image must be an array-image"))
;;;  ;;(format t "copy-texture-sub-image-to-image ~a ~a~%" texture-image bbox)
;;;  (img::fill-array (img::image-array dest-image) 0)
;;;  (bind-vector-elements (u0 u1 v0 v1) bbox
;;;    (let ((u0 (round u0)) (v0 (round v0))(u1 (round u1)) (v1 (round v1)))
;;;      (declare (fixnum u0 v0 v1 v1))
;;;      (let* ((nu (- u1 u0))
;;;             (nv (- v1 v0))
;;;             (dcu (ash (image-x-dim dest-image) -1))
;;;             (dcv (ash (image-y-dim dest-image) -1))
;;;             (buf (img::make-integer-scan-line-buffer texture-image nu))
;;;             (dest-u0 (- dcu (ash nu -1)))
;;;             (dest-v0 (- dcv (ash nv -1)))
;;;             (from-color-p (typep texture-image 'img::array-image-rgb))
;;;             (to-color-p (typep dest-image 'img::array-image-rgb))
;;;             (to-buf (if to-color-p (list buf buf buf) buf))
;;;             )
;;;        (declare (fixnum nu nv))
;;;      
;;;        (when (and from-color-p to-color-p)
;;;          (setq buf (list buf
;;;                          (img::make-integer-scan-line-buffer texture-image nu)
;;;                          (img::make-integer-scan-line-buffer texture-image nu))
;;;                to-buf buf))
;;;        (loop for v fixnum from v0 below v1
;;;              for dest-v fixnum from dest-v0
;;;              do ;;(format t "~a ~a ~%" (list u0 v nu) (list dest-u0 dest-v))
;;;                 (image-getline-clipped texture-image buf u0 v)
;;;                 (image-putline dest-image to-buf dest-u0 dest-v))
;;;        (img::set-pyramid-damaged dest-image)
;;;        (values (- dest-u0 u0) (- dest-v0 v0))
;;;        ;;(values dest-u0 dest-v0)
;;;        ))))


(defun copy-texture-sub-image-to-image (texture-image bbox dest-image)
  (unless (typep dest-image 'img::array-image)
    (error "COPY-TEXTURE-SUB-IMAGE-TO-IMAGE: dest-image must be an array-image"))
  ;;(format t "copy-texture-sub-image-to-image ~a ~a~%" texture-image bbox)
  (img::fill-array (img::image-array dest-image) 0)
  (bind-vector-elements (u0 u1 v0 v1) bbox
    (let* ((u0 (round u0)) (v0 (round v0))(u1 (round u1)) (v1 (round v1))
	   (nu (- u1 u0)) (nv (- v1 v0))
	   (dcu (ash (image-x-dim dest-image) -1))
	   (dcv (ash (image-y-dim dest-image) -1))
	   (dest-u0 (- dcu (ash nu -1)))
	   (dest-v0 (- dcv (ash nv -1)))
	   (from-color-p (typep texture-image 'img::array-image-rgb))
	   (to-color-p (typep dest-image 'img::array-image-rgb))
	   )
      (declare (fixnum u0 v0 v1 v1 nu nv dcu dcv dest-u0 dest-v0))
      (img::with-scan-line-buffers
	  ((buf (img::make-integer-scan-line-buffers texture-image nu)))
	(let* ((to-buf (if (and to-color-p (not from-color-p))
			   (list buf buf buf)
			   buf)))
	  (loop for v fixnum from v0 below v1
		for dest-v fixnum from dest-v0
		do ;;(format t "~a ~a ~%" (list u0 v nu) (list dest-u0 dest-v))
		   (image-getline-clipped texture-image buf u0 v)
		   (image-putline dest-image to-buf dest-u0 dest-v))
	  (img::set-pyramid-damaged dest-image)
	  (values (- dest-u0 u0) (- dest-v0 v0))
	  )))))


(defun compute-transformed-vertex-array (verts xform)
  (declare (type (simple-array t (* *)) verts))
  ;;(declare (optimize (speed 3) (safety 0)))
  (let* ((xdim (array-dimension verts 0))
	 (ydim (array-dimension verts 1))
	 (new (make-array (list xdim ydim))))
    (declare (type (simple-array t (* *)) new))
    (loop for y fixnum from 0 below ydim
	  do (loop for x fixnum from 0 below xdim
		   do (setf (aref new x y) (transform-vector xform (aref verts x y)))))
    new))

;;;(defun compute-transformed-bbox-of-vertices (verts transform)
;;;  (declare (type simple-vector verts))
;;;  (declare (optimize (speed 3) (safety 1)))
;;;  (let ((to-pt (cv 0.0 0.0 0.0)))
;;;    (declare (type (simple-array double-float (*)) to-pt))
;;;    (loop for i from 0 below (length verts)
;;;          with u double-float = 0.0
;;;          with v double-float = 0.0
;;;          do (transform-vector transform (aref verts i) to-pt)
;;;             (setq u (aref to-pt 0)
;;;                   v (aref to-pt 1))
;;;          minimize u into umin double-float
;;;          maximize u into umax double-float
;;;          minimize v into vmin double-float
;;;          maximize v into vmax double-float
;;;          finally (return (cv umin umax vmin vmax)))))

;;;(defun compute-vertex-array-bbox (verts index-bbox)
;;;  (declare (type (simple-array t (* *)) verts))
;;;  (declare (optimize (speed 3) (safety 1)))
;;;  (destructuring-bind (i0 i1 j0 j1) index-bbox
;;;    (declare (fixnum i0 i1 j0 j1) )
;;;    (let ((x 0.0)
;;;          (y 0.0)
;;;          (z 0.0))
;;;      (declare (double-float x y z))
;;;      (loop for i from i0 to i1
;;;            for vert of-type (coordinate-vector 3) = (aref verts i)
;;;            do (setq x (aref vert 0)
;;;                     y (aref vert 1)
;;;                     z (aref vert 2))
;;;            minimize x into xmin double-float
;;;            maximize x into xmax double-float
;;;            minimize y into ymin double-float
;;;            maximize y into ymax double-float
;;;            minimize z into zmin double-float
;;;            maximize z into zmax double-float
;;;            finally (return (cv xmin xmax ymin ymax zmin zmax))))))

(defun compute-vertex-array-bbox (verts index-bbox)
  (declare (type (simple-array t (* *)) verts))
  (declare (optimize (speed 3) (safety 1)))
  (setq *foo* (list verts index-bbox))
  (destructuring-bind (i0 i1 j0 j1) index-bbox
    (declare (fixnum i0 i1 j0 j1) )
    (let ((x 0.0)
	  (y 0.0)
	  (z 0.0))
      (declare (double-float x y z))
      (loop for j fixnum from j0 to j1
	    for (xminj xmaxj yminj ymaxj zminj zmaxj) 
	      =	(loop for i fixnum from i0 to i1
		      for vert of-type (coordinate-vector 3) = (aref verts i j)
		      do (setq x (aref vert 0)
			       y (aref vert 1)
			       z (aref vert 2))
		      minimize x into xmin double-float
		      maximize x into xmax double-float
		      minimize y into ymin double-float
		      maximize y into ymax double-float
		      minimize z into zmin double-float
		      maximize z into zmax double-float
		      finally (return (list xmin xmax ymin ymax zmin zmax)))
	    minimize xminj into xmin double-float
	    maximize xmaxj into xmax double-float
	    minimize yminj into ymin double-float
	    maximize ymaxj into ymax double-float
	    minimize zminj into zmin double-float
	    maximize zmaxj into zmax double-float
	    finally (return (cv xmin xmax ymin ymax zmin zmax))))))

;;; returns vertices of the form (cv u v (diref u v))
(defun make-dtm-vertex-array (dtm-image bbox) 
  (destructuring-bind (u0 u1 v0 v1) bbox
    (declare (fixnum u0 v0 u1 v1))
    (let* ((nu (1+ (- u1 u0)))
	   (nv (1+ (- v1 v0)))
	   (verts (make-array (list nu nv))))
      (declare (type (simple-array t (* *)) verts))
      (declare (optimize (speed 3) (safety 1)))
      (loop for v fixnum from v0 to v1
	    do (loop for u fixnum from u0 to u1
		     do (setf (aref verts (- u u0) (- v v0))
			      (cv u v (diref dtm-image u v)))))
      verts))) 

(defun bloat-bbox-l (bbox bloat)
  (destructuring-bind (u0 u1 v0 v1) bbox
    (values (- u0 bloat) (+ u1 bloat) (- v0 bloat) (+ v1 bloat))))

(defun bloat-bbox (bbox bloat)
  (bind-vector-elements (u0 u1 v0 v1) bbox
    (cv (- u0 bloat) (+ u1 bloat) (- v0 bloat) (+ v1 bloat))))



#|
(disassemble 'compute-vertex-array-bbox)
(disassemble 'compute-transformed-bbox-of-vertices)

(setq alv-dtm (load-image "$RADIUS/sites/alv/alv-dtm.g0"))

(setq alv-2-44 (load-image "$RADIUS/site-2d-worlds/alv/alv-2-44/full-tiff/image.g0"))
(setq alv-2-44-projection (3d-to-2d-projection (cme::get-2d-world-named "alv-2-44")))

(setq alv-dtm-to-lvcs-transform (make-4x4-coordinate-transform
				 (make-and-fill-4x4-matrix
				  98.49512 -.13215083 -.13852036 -7890.5884
				  0.13259056 98.494644 0.15808614 176.01
				  0.11577392 -0.37007928 98.42377 4.989898
				  0.0 0.0 0.0 1.0)))

(setq *dtm-to-alv-2-44-projection*
      (list alv-dtm-to-lvcs-transform
	    alv-2-44-projection))

(img::image-dimensions alv-dtm)

(setq *verts* (make-dtm-vertex-array alv-dtm (cv 100.0 108.0 60.0 68.0)))

(setq *image-verts* (obj::transform-vertex-array *verts* *dtm-to-alv-2-44-projection*))

(setq *bbox2* (compute-vertex-array-bbox *image-verts*))

(setq *bbox* (compute-transformed-bbox-of-vertices *verts* *dtm-to-alv-2-44-projection*))

(setq *print-array* nil)
(setq *print-array* t)

(compute-gsd alv-2-44-projection (selected-object-world-position)) = 3 ft/pixel
|#



(defparameter *texid* nil)

(defun get-ortho-texid ()
  (or *texid*
      (let ((arr (make-array 1 :element-type '(unsigned-byte 32))))
	(gl::glGenTextures 1 arr)
	(setq *texid* (aref arr 0)))))

(defun release-ortho-texid ()
  (when *texid*
    (let ((arr (make-array 1 :element-type '(unsigned-byte 32))))
      (setf (aref arr 0) *texid*)
      (gl::glDeleteTextures 1 arr)
      (setq *texid* nil))))

#|
(get-ortho-texid)

(setq *tex-array* (make-array (* 256 256)))
(setup-gl-texture-map (get-ortho-texid) *tex-array*  256 256 )
|#


(defun setup-gl-texture-map (tex-id pixel-array tex-width tex-height &key
			     ;(internal-format GL_LUMINANCE) (tex-format GL_LUMINANCE)
			     ;(internal-format GL_RGB) (tex-format GL_LUMINANCE)
			     (internal-format GL_RGB) (tex-format GL_RGB)
			     ;(internal-format GL_RGBA) (tex-format GL_RGBA)
			     (tex-type  GL_UNSIGNED_BYTE)
			     (clamp-mode gl::GL_CLAMP_TO_EDGE)
			     ;;(clamp-mode gl::GL_CLAMP_TO_BORDER)
			     )
  (gl::glBindTexture GL_TEXTURE_2D tex-id)
  
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S clamp-mode)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T clamp-mode)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
  (glTexParameterfv GL_TEXTURE_2D GL_TEXTURE_BORDER_COLOR (fv 0.0 0.0 0.0 0.0))
  (glTexImage2D GL_TEXTURE_2D 0 internal-format tex-width tex-height 0 tex-format tex-type pixel-array)
  ;;(glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
  (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_REPLACE)
 ;; (gl::glBindTexture GL_TEXTURE_2D 0)
  (glFlush)
  (handle_gl_errors "setup-gl-texture-map")
  
  )

(defun initialize-rendering-state (window &key
				   (background-color (cv 0.0 0.0 0.0 1.0))
				   (image-modulation-color (cv 1.0 1.0 1.0 1.0))
				   )
  (gl-make-current window)
  (mv-bind (width height) (dimensions window)
      (glViewPort  0 0 width height))
  (glClearColor4dv background-color )
  (glDepthMask 1) (glClearDepth 1.0)
  (glClear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glColor4dv image-modulation-color ) 
  (glDisable GL_POLYGON_STIPPLE)
  ;(glHint GL_PERSPECTIVE_CORRECTION_HINT  GL_NICEST) ;GL_FASTEST
  (glDisable GL_DITHER)
  (glEnable GL_DEPTH_TEST)
  (glDisable GL_DEPTH_TEST)
  (glPolygonMode GL_FRONT GL_FILL)
  (glPolygonMode GL_BACK GL_FILL)
  (glDisable GL_LIGHTING)
  (glShadeModel GL_FLAT)
  (glDisable GL_CULL_FACE)
  )


    
#|
(setq *rgb-image-patch* (make-image '(1024 1024) :element-type 'img::RGB8))
(setq *rgba-image-patch* (make-image '(1024 1024) :element-type 'img::RGBA8))
(length (img::image-array *rgb-image-patch*))
(length (img::image-array *rgba-image-patch*))
(setq offsets (mv-list (copy-texture-sub-image-to-image alv-2-44
							(bloat-bbox *bbox2* 0)
							*rgb-image-patch*)))

(push-image *rgb-image-patch* (selected-window))
(img::release-image-pool-textures *rgb-image-patch*)
(img::set-pyramid-damaged *rgb-image-patch*)

(setq *image-patch* (make-image '(1024 1024)))
(bloat-bbox *bbox2* 0)
(bloat-bbox *bbox2* 50)
(setq offsets (mv-list (copy-texture-sub-image-to-image alv-2-44
							(bloat-bbox *bbox2* 0)
							*image-patch*)))
(setq offsets (mv-list (copy-texture-sub-image-to-image alv-2-44
							(bloat-bbox *bbox2* 50)
							*image-patch*)))

(setq dtm-to-ortho-scale 32.0)

(push-image *image-patch* (selected-window))
(window-damaged *interactor* (selected-window))
(release-image-pool-textures *image-patch*)
(release-image-textures *image-patch*)
(img::set-pyramid-damaged *image-patch*)
(clear-view-stack (selected-window))

(orthorectify-bbox (cv 100.0 108.0 60.0 68.0) (selected-window)
		   alv-2-44 alv-2-44-projection
		   alv-dtm alv-dtm-to-lvcs-transform
		   *rgb-image-patch* 32.0)

(orthorectify-bbox (cv 100.0 108.0 60.0 68.0) (selected-window)
		   alv-2-44 alv-2-44-projection
		   alv-dtm alv-dtm-to-lvcs-transform
		   *image-patch* 32.0)

(orthorectify-bbox (cv 108.0 116.0 60.0 68.0) (selected-window)
		   alv-2-44 alv-2-44-projection
		   alv-dtm alv-dtm-to-lvcs-transform
		   *rgb-image-patch* 32.0)
(get-ortho-texid)
(OpenGL-reset-state)
(trace glTexImage2D)

(destructuring-bind (dtm-to-ortho-matrix dtm-verts) *orthorectify-bbox*
  (loop for i from 0 below 64
	collect
	(transform-vector (make-4x4-coordinate-transform dtm-to-ortho-matrix)
			  (obj::vertex-array-vertex dtm-verts i))))
(progn *orthorectify-bbox*)

(iref *image-patch* 512 512)

(setq *print-array* nil)

(cv 1.2)
(img::image-array *image-patch*)
(aref (img::image-array *image-patch*) (+ 512 (* 512 1024)))
(iref *image-patch* 512 512)


(gl::glProject_to_window )

|#

(defun render-terrain-texture (dtm-verts image-verts dtm-bbox tile-offsets)
  (destructuring-bind (uoff voff) tile-offsets
    (let ((uoff (dfloat uoff)) (voff (dfloat voff)))
      (declare (double-float uoff voff))
      ;; (format t "tile-offsets= ~a~%" tile-offsets)
      (destructuring-bind (u0 u1 v0 v1) dtm-bbox
	(declare (fixnum u0 v0 u1 v1))
	(glEnable GL_TEXTURE_2D)
	(glBindTexture GL_TEXTURE_2D (get-ortho-texid))
	(loop for v fixnum from v0 to (1- v1)
	      do
	   (glBegin GL_TRIANGLE_STRIP)
	   (loop for u fixnum from u0 to u1
		 do
	      #+never
	      (bind-vector-elements (tx ty) (aref image-verts u (1+ v))
		(format t "~a ~a ~a ~%"
			(aref dtm-verts (1+ v) u)
			(gl::glProject_to_window (aref dtm-verts u (1+ v)))
			(list (+ tx (car *tile-offsets*)) (+ ty (cadr *tile-offsets*)))))
		
	      (glTexCoord2dv (aref image-verts u (1+ v)))
	      (glVertex3dv (aref dtm-verts u (1+ v)))
	      (glTexCoord2dv (aref image-verts u v))
	      (glVertex3dv (aref dtm-verts u v))
		 )
	   (glEnd))
	(glDisable GL_TEXTURE_2D)
	))))

(declaim (special *tile-offsets*))

(defun orthorectify-bbox (dtm-verts-bbox ; a list
			  window
			  texture-image
			  dtm-verts image-verts
			  image-patch
			  dtm-to-ortho-scale 
			  )
  (destructuring-bind (u0 u1 v0 v1) dtm-verts-bbox
    (declare (ignorable u1 v1))
    (format t "orthorectify-bbox ~a~%" dtm-verts-bbox)
    
    (bind-vector-elements (dtm-x dtm-y) (aref dtm-verts u0 v0)
      (let* ((texture-verts-bbox
	      (bloat-bbox (compute-vertex-array-bbox image-verts dtm-verts-bbox)
			  10))
	     (patch-width (image-x-dim image-patch))
	     (patch-height (image-y-dim image-patch))
	     (tile-offsets
	      (mv-list (copy-texture-sub-image-to-image texture-image texture-verts-bbox image-patch)))
	     (*tile-offsets* tile-offsets)
	     (dtm-to-ortho-matrix
	      (make-and-fill-4x4-matrix dtm-to-ortho-scale 0.0 0.0 (- (* dtm-x dtm-to-ortho-scale))
					0.0 dtm-to-ortho-scale 0.0 (- (* dtm-y dtm-to-ortho-scale))
					0.0 0.0 .0001 .5
					;;0.0 0.0 0.0 .5
					0.0 0.0 0.0 1.0))
	     (nu (round (1+ (- u1 u0))))
	     (texid (get-ortho-texid))
	     )
	(gl-make-current window)
	(glPushAttrib GL_ALL_ATTRIB_BITS)
	(initialize-rendering-state window)
	(set-window-raster-transform window)
	(glMultMatrixd_transposed dtm-to-ortho-matrix) ; GL_MODELVIEW matrix is selected
	(setup-gl-texture-map texid (img::image-array image-patch) patch-width patch-height
			      :tex-format (if (typep image-patch 'img::array-image-rgb)
					      GL_RGB GL_LUMINANCE)
			      )
	(glMatrixMode GL_TEXTURE) (glLoadIdentity)
	(glMultMatrixd_transposed
	 (destructuring-bind (uoff voff) tile-offsets
	   (make-and-fill-4x4-matrix (/ 1.0 patch-width) 0.0 0.0 (/ (dfloat uoff) patch-width)
				     0.0 (/ -1.0 patch-height) 0.0 (- 1.0 (/ (dfloat voff) patch-height)) ; patch is top-to-bottom ordered
				     0.0 0.0 1.0 0.0
				     0.0 0.0 0.0 1.0)))
       
	#+never
	(format t "(gl::glProject_to_window ~a) = ~a~%"
		(aref dtm-verts u0 v0) (gl::glProject_to_window (aref dtm-verts u0 v0)))
	;;(break)
	(render-terrain-texture dtm-verts image-verts dtm-verts-bbox tile-offsets)
	(glPopAttrib)
	(copy-to-front window)
	(glFlush)
	(gl::glBindTexture GL_TEXTURE_2D 0)
	(release-ortho-texid)
	(handle_gl_errors "orthorectify-bbox")
	(setq *orthorectify-bbox* (list dtm-to-ortho-matrix   dtm-verts image-verts))
	nil
	))))


(declaim (special *orthorectify-image-params*))
(defun orthorectify-image (window
			   texture-image
			   texture-image-3d-to-2d-projection
			   dtm-image 
			   dtm-to-lvcs-transform
			   dtm-to-ortho-scale
			   &key
			   image-patch ;(image-patch *rgb-image-patch*)
			   (tile-dims '(256 256))
			   (ortho-image-element-type 'img::rgba8)
			   ;(ortho-image-element-type '(unsigned-byte 8))
			   )
			   
  (let* ((tile-width (car tile-dims))
	 (tile-height (cadr tile-dims))
	 (dtm-to-image-projection (list dtm-to-lvcs-transform
					texture-image-3d-to-2d-projection))
	 (u-verts-per-tile (round tile-width dtm-to-ortho-scale))
	 (v-verts-per-tile (round tile-height dtm-to-ortho-scale))
	 (glReadPixels-format (cond ((eq ortho-image-element-type 'img::rgba8)
				     GL_RGBA)
				    ((equal ortho-image-element-type '(unsigned-byte 8))
				     GL_LUMINANCE)
				    (t (error "illegal ortho-image-element-type = ~a" 
					      ortho-image-element-type))))
	 (image-patch (or image-patch (make-image '(1024 1024) :element-type ortho-image-element-type)))
	 )
    (declare (fixnum tile-width tile-height u-verts-per-tile v-verts-per-tile))
    (mv-bind (u0 u1 v0 v1) (outside-bbox texture-image dtm-image dtm-to-image-projection)
      (setq u1 (+ u0 (pad-to-multiple (- u1 u0) u-verts-per-tile))
	    v1 (+ v0 (pad-to-multiple (- v1 v0) v-verts-per-tile)))
      (let* ((ortho-image (make-image (list (round (* dtm-to-ortho-scale (- u1 u0)))
					    (round (* dtm-to-ortho-scale (- v1 v0))))
				      :block-x-dim tile-width
				      :block-y-dim (- tile-height)
				      :element-type ortho-image-element-type
				      ))
	     (tile-image (make-image tile-dims :element-type ortho-image-element-type))
	     (flipped-tile-image (img::image-neg-y tile-image))
	     (tile-array (img::image-array tile-image))
	     (dtm-verts-bbox (list u0 u1 v0 v1))
	     (dtm-verts (make-dtm-vertex-array dtm-image dtm-verts-bbox))
	     (image-verts (compute-transformed-vertex-array dtm-verts dtm-to-image-projection))
	     )

	(setq *orthorectify-image-params*
	      (list flipped-tile-image ortho-image))
	(loop for v from 0 below (- v1 v0) by v-verts-per-tile
	      for iv from 0 by tile-height
	      do  (loop for u from 0 below (- u1 u0) by u-verts-per-tile
			for iu from 0 by tile-width
			do (orthorectify-bbox (list u (+ u u-verts-per-tile) v (+ v v-verts-per-tile))
					window
					texture-image
					dtm-verts image-verts
					image-patch
					dtm-to-ortho-scale)
			   (glReadBuffer GL_BACK)
			   (let ((vp (make-int-vector 4)))
			     (glGetIntegerv GL_VIEWPORT vp)
			     (glReadPixels (aref vp 0) (aref vp 1)
					   tile-width tile-height glReadPixels-format GL_UNSIGNED_BYTE
					   tile-array)
			     (img::insert-image-into-image flipped-tile-image ortho-image iu iv))
			   ;;(break)
			))

	;;(setf (get-prop ortho-image :verts) verts)
	;;(setf (get-prop ortho-image :image-verts) image-verts)
			
	(values ortho-image (compute-transformed-vertex-array dtm-verts dtm-to-lvcs-transform))
	))))

#|
(maybe-compile-file-load "$FREEDIUS/lisp/cme-compat/orthorectify.lisp")

(progn
  (setq alv-dtm (load-image "$RADIUS/sites/alv/alv-dtm.g0"))

  (setq alv-2-44 (load-image "$RADIUS/site-2d-worlds/alv/alv-2-44/full-tiff/image.g0"))
  (setq alv-2-44-projection (3d-to-2d-projection (cme::get-2d-world-named "alv-2-44")))

  (setq alv-dtm-to-lvcs-transform (make-4x4-coordinate-transform
				   (make-and-fill-4x4-matrix
				    98.49512 -.13215083 -.13852036 -7890.5884
				    0.13259056 98.494644 0.15808614 176.01
				    0.11577392 -0.37007928 98.42377 4.989898
				    0.0 0.0 0.0 1.0)))
  
  ;;(setq *rgb-image-patch* (make-image '(1024 1024) :element-type 'img::RGB8))
  (setq *ortho-pane* (selected-window))
  (mv-setq (*ortho* *lvcs-verts*)
    (orthorectify-image *ortho-pane*
			alv-2-44 alv-2-44-projection
			alv-dtm alv-dtm-to-lvcs-transform
			32.0
			:ortho-image-element-type '(unsigned-byte 8)
			;:image-patch *rgb-image-patch*
			))
  )

  
(push-image *ortho* (selected-window))
(push-image alv-dtm (selected-window))
(destructuring-bind (dtm-to-ortho-matrix dtm-verts image-verts) *orthorectify-bbox*
  (let ((i 8)) (list (obj::vertex-array-vertex dtm-verts i) (obj::vertex-array-vertex image-verts i))))
(OpenGL-reset-state)
(let ((*print-array* t))
  (describe (car *orthorectify-bbox* )))

(describe *rgb-image-patch*)
(describe *ortho*)
(image_tex_tile_dims *ortho*)
|#



#|

(maybe-compile-file-load "$FREEDIUS/lisp/cme-compat/orthorectify.lisp")
(maybe-compile-file-load "$FREEDIUS/lisp/cme-compat/terrain-models.lisp")
(maybe-compile-file-load "$FREEDIUS/lisp/cme-compat/usgs-dem.lisp")

(progn
  (setq *olalla-dtm* (load-image "$RADIUS/sites/Olalla/Olalla-dem2.g0"))

  (setf (get-prop *olalla-dtm* :USGS-DEM-INFO)
	'(:UTM-TRANSFORM 10 30.0 449070.0 30.0 4760730.0 1.0 0.0))

  (setq *olalla-texture-image*
	(img::pyramid-level (load-image-pyramid "$RADIUS/site-2d-worlds/Olalla/7189_70") 1))

  (setq *olalla-7189_70-projection* (3d-to-2d-projection *olalla-texture-image*))
  
  (setq *olalla-3d-to-texture-image-projection*
	(list *olalla-7189_70-projection*
	      (inverse-transform (image-to-2d-transform *olalla-texture-image*))))
	
  (setq *olalla-terrain-model*
	(cme::make-usgs-terrain-model
	 (img::load-image "$RADIUS/sites/Olalla/Olalla-dem2.g0")
	 (inverse-transform (lvcs-to-geocentric-transform (cme::get-3d-world-named "Olalla")))
	 "Olalla UTM DTM"
	 :lat-long-to-geocentric-transform transforms::*NAD-27-lat-long-to-geocentric-transform*
	 :exact t)
	olalla-dtm-to-lvcs-transform (cme::dtm-to-lvcs-transform *olalla-terrain-model*)
	)
  
  (setq *ortho-pane* (selected-window))
  (setq *rgb-image-patch* (make-image '(1024 1024) :element-type 'img::RGB8))  
  (mv-setq (*olalla-ortho* *olalla-lvcs-verts*)
    (orthorectify-image *ortho-pane*
			*olalla-texture-image*
			*olalla-3d-to-texture-image-projection*
			*olalla-dtm* olalla-dtm-to-lvcs-transform
			32.0
			;:image-patch *rgb-image-patch*
			))

  )


(progn *olalla-texture-image*)
(cme::get-3d-world-named "Olalla")
(lvcs-to-geocentric-transform (cme::get-3d-world-named "Olalla"))
(inverse-transform (lvcs-to-geocentric-transform (cme::get-3d-world-named "Olalla")))


(describe *olalla-dtm*)

(setq *olalla-terrain-model*
	(cme::make-usgs-terrain-model
	 (img::load-image "$RADIUS/sites/Olalla/Olalla-dem2.g0")
	 (inverse-transform (lvcs-to-geocentric-transform (cme::get-3d-world-named "Olalla")))
	 "Olalla UTM DTM"
	 :lat-long-to-geocentric-transform transforms::*NAD-27-lat-long-to-geocentric-transform*
	 :exact t
	 ))
  
(setq olalla-dtm-to-lvcs-transform (cme::dtm-to-lvcs-transform *olalla-terrain-model*))
(setq *ortho-pane* (selected-window))

(transform-vector olalla-dtm-to-lvcs-transform (cv 100.0 100.0 (diref *olalla-dtm* 100 100)))

(setq *olalla-center-of-rotation* (cv -522.1892809752424 -1625.4550389975034 720.0))

(inverse-transform-vector olalla-dtm-to-lvcs-transform *olalla-center-of-rotation*)
#(279.05804112851985 292.0395897439659 219.47770250921758)
			  
(diref *olalla-dtm* 279 292)) ; 220.0
(* 220.0 *feet-per-meter*) = 721.78

(setq *rgb-image-patch* (make-image '(1024 1024) :element-type 'img::RGB8))

(mv-setq (*olalla-ortho* *olalla-lvcs-verts*)
  (orthorectify-image *ortho-pane*
		      *olalla-texture-image*
		      *olalla-3d-to-texture-image-projection*
		      *olalla-dtm* olalla-dtm-to-lvcs-transform
		      32.0
		      :image-patch *rgb-image-patch*))

(push-image *olalla-ortho* (selected-window))

(describe *olalla-ortho*)


(transform-vector (cme::dtm-to-lvcs-transform *olalla-terrain-model*)
		  (cv 0.0 0.0 (diref *olalla-dtm* 0 0)))
#(-27819.773015713137 -30550.008521665623 -40.809425262137665)

(selected-object-world-position)

#(-23.52102306544589 -180.27685185506266 684.1207357822782)

(inverse-transform-vector (cme::dtm-to-lvcs-transform *olalla-terrain-model*)
			  (cv -23.52102306544589 -180.27685185506266 684.1207357822782))
#(284.2134635495078 306.6848454647365 208.52065849882183)

(describe *olalla-dtm*)
(trace make-image)


(setq *img* (make-image '(8448 10752) :element-type 'img::rgba8))
(setq *img* (make-image '(8448 10752) :element-type 'img::rgba8 :block-x-dim 256 :block-y-dim -256))

|#
