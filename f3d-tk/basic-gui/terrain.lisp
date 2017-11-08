(in-package :gui)

;;; Contains direct dependencies on OpenGL.

#|
(maybe-compile-file-load "$FREEDIUS/lisp/basic-gui/terrain.lisp")

(maybe-compile-file-load "$FREEDIUS/lisp/cme-compat/terrain-models.lisp")


|#

(import '(obj::vertex-array obj::vertex-element-type obj::vertex-array-type obj::vertex-index-array-type
	  obj::*GL-VERTEX-TYPE* obj::*gl-vertex-bytes-per-element* obj::*GL-VERTEX-INDEX-TYPE*))

(defun compute-mesh-face-normals (vertex-array nx ny)
  (let ((verts vertex-array)
	(normals (make-array0 (list (* 2 (1- nx) (1- ny)) 3)
			     :element-type 'vertex-element-type)))
    (declare (type vertex-array-type verts normals))
    (labels ((v (i) (cv (aref verts i 0) (aref verts i 1) (aref verts i 2)))
	     (normal (face v0 v1 v2)
	       (bind-vector-elements (nx ny nz) (triangle-normal (v v0) (v v1) (v v2))
		 (setf (aref normals face 0) nx
		       (aref normals face 1) ny
		       (aref normals face 2) nz))))
	
      (loop for y fixnum from 0 below (1- ny)
	    for i fixnum from 0 by nx
	    with face fixnum = 0
	    do (loop for x fixnum from 0 below (1- nx)
		     do (normal face (+ i x) (+ i x 1) (+ i x nx))
			(normal (1+ face) (+ i x nx) (+ i x 1) (+ i x nx 1))
			(incf face 2))
	    )
      normals)))

(defun compute-vertex-normal (face-normal-array &rest face-indices)
  (loop with (sum-nx sum-ny sum-nz) double-float
	for face-index in face-indices
	do (obj::bind-vertex-array-elements
	    (nx ny nz)
		face-normal-array face-index
				  (incf sum-nx nx) (incf sum-ny ny) (incf sum-nz nz))
	finally (normalize-vector-elements sum-nx sum-ny sum-nz )
		(return (cv sum-nx sum-ny sum-nz ))))

(defun compute-mesh-vertex-normals (face-normals nx ny)
  (let ((vertex-normals (make-array0 (list (* nx ny) 3)
				    :element-type 'vertex-element-type)))
    (loop with fnx fixnum = (* 2 (1- nx))
	  for y fixnum from 0 below (1- ny)
	  for i fixnum from 0 by nx
	  for face fixnum = (* y fnx)
	  for vert fixnum = (* y nx)
	  do
       (loop for x fixnum from 0 below (1- nx)
	     for vertex-normal 
	       = (cond ((and (> x 0) (> y 0))
			(compute-vertex-normal face-normals
					       face (- face fnx 1)
					       (- face fnx) (- face fnx -1)
					       (- face 1) (- face 2)))
		       ((> y 0)
			(compute-vertex-normal face-normals
					       face (- face fnx) (- face fnx -1)))
		       ((> x 1)
			(compute-vertex-normal face-normals
					       face (- face 1) (- face 2)))
		       (t (compute-vertex-normal face-normals face))
		       )
	     do (bind-vector-elements (nx ny nz) vertex-normal
		  (setf (aref vertex-normals vert 0) nx
			(aref vertex-normals vert 1) ny
			(aref vertex-normals vert 2) nz))
		(incf vert)
		(incf face 2)
		      
	     )
	  )
    vertex-normals))


(defstruct-class dtm-quad-mesh (obj::gl-object)
    ((dtm-image :initform nil :initarg :dtm-image :accessor dtm-image)
     (dtm-to-lvcs-transform :initform nil :initarg :dtm-to-lvcs-transform
			    :accessor dtm-to-lvcs-transform)
     (x0 :initform 0.0 :initarg :x0)
     (y0 :initform 0.0 :initarg :y0)
     (delta :initform 1.0 :initarg :delta)
     (nx :initform 30 :initarg :nx)
     (ny :initform 30 :initarg :ny)
     (vertex-normals :initform nil :accessor vertex-normals)
     
     ))

(defmethod selectable-p ((object dtm-quad-mesh))
  nil)


(defmethod compute-vertices ((object dtm-quad-mesh))
  (with-slot-values (dtm-image x0 y0 nx ny delta dtm-to-lvcs-transform) object
    (let ((vertex-array (make-array0 (list (* nx ny) 3)
				    :element-type 'obj::vertex-element-type))
	  )
      (declare (type obj::vertex-array-type vertex-array))
      (setf (obj::%vertex-array object) vertex-array)
      (loop for y double-float from y0 by delta
	    repeat ny
	    with index fixnum = 0
	    do (loop for x double-float from x0 by delta
		     repeat nx
		     ;;for z double-float = (img::interpolate-iref dtm-image x y)
		     for z double-float = (img::iref dtm-image (round x) (round y))
		     do (bind-vector-elements (wx wy wz)
			    (transform-vector dtm-to-lvcs-transform (cv x y z))
			  (setf (aref vertex-array index 0) wx
				(aref vertex-array index 1) wy
				(aref vertex-array index 2) wz))
		     (incf index)))
      vertex-array)))

(defmethod compute-face-normals ((object dtm-quad-mesh))
  (with-slot-values (vertex-array nx ny) object
    (compute-mesh-face-normals vertex-array nx ny)))

(defmethod compute-vertex-normals ((object dtm-quad-mesh))
  (let ((face-normals (compute-face-normals object)))
    (with-slot-values (nx ny) object
      (setf (vertex-normals object)
	    (compute-mesh-vertex-normals face-normals nx ny)))))
    
(defmethod draw-object ((object dtm-quad-mesh) view)
  (declare (ignorable view))
  (with-class-slot-values dtm-quad-mesh (nx ny vertex-normals vertex-array) object
    (declare (fixnum nx ny))
    (let* ((verts vertex-array)
	   (ncomps (array-dimension verts 1))
	   (quad-strip-indices (make-array0 (* 2 nx) :element-type 'vertex-index-type))
	   )
      (declare (type vertex-index-array-type quad-strip-indices))
      ;; compute-vertices takes object-to-world-matrix into account in order
      ;; to compute face normals and properly level the road left-to-right.
      (unwind-protect
	   (progn 
	     (glPushAttrib GL_POLYGON)
	     (glDisable GL_CULL_FACE)
	     ;;(glPolygonMode GL_BACK GL_FILL)
	     ;;(glPolygonMode GL_FRONT GL_FILL)
	     (glVertexPointer 3 *GL-VERTEX-TYPE*
			      (* *gl-vertex-bytes-per-element* ncomps)
			      verts)
	     (when vertex-normals
	       (glEnableClientState GL_NORMAL_ARRAY)
	       (glNormalPointer *GL-VERTEX-TYPE*
				(* *gl-vertex-bytes-per-element* ncomps)
				vertex-normals))
	     (loop for y fixnum from 0 below (1- ny)
		   for i0 fixnum from 0 by nx
		   do (loop for x fixnum from 0 below nx
			    for x2 fixnum from 0 by 2
			    do (if t
				   (setf (aref quad-strip-indices x2) (+ i0 x)
				     (aref quad-strip-indices (1+ x2)) (+ i0 nx x))
				   (setf (aref quad-strip-indices x2) (+ i0 nx x)
				     (aref quad-strip-indices (1+ x2)) (+ i0 x))))
		      (glDrawElements GL_TRIANGLE_STRIP (+ nx nx)
				      *GL-VERTEX-INDEX-TYPE* quad-strip-indices)
		   ))
	;; clean-up form
	(progn (glPopAttrib)
	       (when vertex-normals (glDisableClientState GL_NORMAL_ARRAY))
	       )
	))))



(defstruct-class dtm-quad-mesh2 (cme::3d-object dtm-quad-mesh)
    ())

(defmethod initialize-instance :after ((object dtm-quad-mesh2) &key &allow-other-keys)
  (compute-vertices object)
  (compute-vertex-normals object))

    
#|

(setf (get-prop (top-view ".frm.f2.gl2") :sun-vector)
      (normalize-coordinate-vector (cv 1.0 1.0 1.0)))

(push dtm-obj alv-objects)
(pop alv-objects)
(first alv-objects)

(progn dtm-obj)

(compute-vertices dtm-obj)
(compute-face-normals dtm-obj)
(setq vert-normals (compute-vertex-normals dtm-obj))

(print-verts vert-normals 0 5)
(print-verts (vertex-array dtm-obj) (let ((x 30)(y 45))(+ (* 100 y) x)) 5)
(setf (graphics-style dtm-obj)
      (make-instance 'graphics-style
		     :color-vector (fv 0.47 0.047 0.047 .2)))

(setf (graphics-style dtm-obj) nil)

(color-name-to-gl "brown")
(glEnable GL_BLEND)

(setq depths (read-depth-buffer ".frm.f2.gl2"))


(let ((x 100) (y 100))
  (list (aref depths x y)
	(aref (nth 2 (get-prop ".frm.f2.gl2" :window-state)) x y)))

(progn depths)
(aref depths 100 100)
(aref depths 0 0)

(let ((x 0.0) (y 0.0))
  (glMakeCurrent ".frm.f2.gl2")
  (glProject_to_world (cv x y (aref depths (round x)( round y)))))




(CV -2975.7300886836647 192.93998916275797 3522.9)

(progn (glMakeCurrent ".frm.f2.gl2")
       (glEnable GL_DEPTH_TEST)
       (glClearDepth .5) (glDepthRange 0.0 1.0) (glDepthMask 1)
       (glClear GL_DEPTH_BUFFER_BIT)
       (setq depths (read-depth-buffer ".frm.f2.gl2"))
       )

(glGetInteger GL_DEPTH_WRITEMASK) = 1
(format nil "~x" (glGetFloat GL_DEPTH_CLEAR_VALUE)) = 1.0
(glIsEnabled GL_DEPTH_TEST) = T
(glDepthRange 0.0 1.0)
(glDepthMask 1)

(progn (glMakeCurrent ".frm.f2.gl2")
       (list (glGetInteger GL_RED_BITS)
	     (glGetInteger GL_ACCUM_RED_BITS)
	     (glGetInteger GL_ALPHA_BITS)
	     (glGetInteger GL_DEPTH_BITS)
	     (glGetBoolean GL_DOUBLEBUFFER)
	     (glGetBoolean GL_STEREO)
	     (glGetBoolean GL_RGBA_MODE)))



(tcl-cmd '(winfo visualsavailable ".frm.f2.gl2"))
(tcl-cmd '(winfo id ".frm"))
(tcl-cmd '(winfo id ".frm.f2"))
(tcl-cmd '(winfo id ".frm.f2.gl2"))

(tk::get_window_visualid (aref (tk::get_tk_displays) 0)
			 (tk::hex-string-to-number (tcl-cmd '(winfo id ".frm.f2.gl2"))))
(format nil "~x" 49)
|#




;;; z-image is derived from ray tracing image locations on tex tile pool aligned
;;; grid in tex-image  The grid must be aligned to tiling of tex tile pool.
;;; This means that tile corners are at i*tile_xdim image-y-dim-1-j*tile_ydim
;;; tex-image locations, where i,j are integers.

;;; Tiles are in raster order left-to-right, top-to-bottom.

(defstruct-class dtm-tex-mapped-quad-mesh (obj::gl-3d-object-mixin obj::gl-dynamic-object-mixin obj::gl-object )
    ((tex-image :initarg :tex-image :accessor tex-image)
     (z-image :initarg :z-image :accessor z-image)
     (z-image-to-2d-transform :initarg :z-image-to-2d-transform
			      :accessor z-image-to-2d-transform)
     (2d-to-world-matrix :initarg :2d-to-world-matrix :accessor 2d-to-world-matrix)
     (3d-to-2d-projection :initarg :3d-to-2d-projection :accessor 3d-to-2d-projection)
     (vertex-normals :initform nil :accessor vertex-normals)
     ))

(defmethod compute-face-normals ((object dtm-tex-mapped-quad-mesh))
  (with-slot-values (vertex-array z-image) object
    (compute-mesh-face-normals vertex-array
			       (image-x-dim z-image) (image-y-dim z-image))))

(defmethod compute-vertex-normals ((object dtm-tex-mapped-quad-mesh))
  (let ((face-normals (compute-face-normals object))
	(z-image (z-image object)))
    (setf (vertex-normals object)
	  (compute-mesh-vertex-normals face-normals
				       (image-x-dim z-image) (image-y-dim z-image)))))


(defmethod selectable-p ((object dtm-tex-mapped-quad-mesh))
  nil)



;;; This ray traces from the texture-image to 
(defmethod compute-vertices ((object dtm-tex-mapped-quad-mesh))
  (with-class-slot-values dtm-tex-mapped-quad-mesh
	(z-image z-image-to-2d-transform 3d-to-2d-projection)
      object
    (let* ((nx (1+ (image-x-dim z-image)))
	   (ny (1+ (image-y-dim z-image)))
	   (xmax (- nx 2))
	   (ymax (- ny 2))
	   (vertex-array (make-array0 (list (* nx ny) 3)
				     :element-type 'obj::vertex-element-type))
	   )
      (declare (type obj::vertex-array-type vertex-array))
      (setf (obj::%vertex-array object) vertex-array)
      (loop for v double-float from 0.0 by 1.0
	    for j fixnum from 0 below ny
	    with index fixnum = 0
	    do (loop for u double-float from 0.0 by 1.0
		     for i fixnum from 0 below nx
		     for z double-float = (img::diref z-image (min i xmax) (min j ymax))
		     ;;do (setq foo88 (list u v z))
		     do (bind-vector-elements (wx wy wz)
			    (transforms::intersect-camera-ray-with-z-plane
			     3d-to-2d-projection
			     (transform-vector z-image-to-2d-transform (cv u v 0.0))
			     z)
			  (setq foo99 (list i j u v z wx wy wz))
			  (setf (aref vertex-array index 0) wx
				(aref vertex-array index 1) wy
				(aref vertex-array index 2) wz))
			(incf index)))
      vertex-array)))

(declaim (special foo88 foo99))

(defmethod initialize-instance :after ((object dtm-tex-mapped-quad-mesh)
				       &key &allow-other-keys)
  (compute-vertices object))

(defparameter *dtm-tex-mapped-quad-mesh-image-level* 1)

(defmethod obj::draw-object ((object dtm-tex-mapped-quad-mesh) view)
  (declare (ignorable view))
  ;;(format t "obj::draw-object ~a~%")
  (tex-map-object object
		  (with-class-slots interactor (drag-object-type popup-drag) *interactor*
		    (if	;;(and popup-drag (eq drag-object-type 'image))
		     t			; popup-drag
		     *dtm-tex-mapped-quad-mesh-image-level* 0))))

#|
(trace align-tile-y)
|#
(defmethod align-tile-y (image y tile-height)
  (let ((ydim (image-y-dim image)))
    (- ydim (pad-to-multiple (- ydim y) tile-height))))

(defmethod align-tile-x (image x tile-width)
  (ignore image)
  (truncate-to-multiple x tile-width))

(defmethod tex-map-object ((object dtm-tex-mapped-quad-mesh) &optional (level 1))
  ;;(break)
  ;;(format t "tex-map-object ~a ~%" level)
  ;;(glFlush) (handle_gl_errors "tex-map-object start")
  (with-class-slot-values dtm-tex-mapped-quad-mesh
	(tex-image z-image z-image-to-2d-transform obj::vertex-array)
      object
    (unwind-protect
	 (let ((image (if (= level 0)
			  tex-image
			  (get-image-pyramid-level tex-image level 1))))
	   (glPushAttrib GL_ALL_ATTRIB_BITS)
	   (glEnable GL_DEPTH_TEST)
	   ;;(glDisable GL_DEPTH_TEST)
	   (glDisable GL_POLYGON_STIPPLE)
	   ;;(glDisable GL_CULL_FACE) (glPolygonMode GL_BACK GL_FILL)
	   (when t
	     ;; Usually do not want to apply lighting to terrain.
	     ;; Besides, we do not generate and apply normal vectors.
	     (glDisable GL_LIGHTING) 

	     (glEnable GL_TEXTURE_2D)
	     ;;(glEnableClientState GL_VERTEX_ARRAY)
	     (glPolygonMode GL_FRONT GL_FILL)
	     (glColor3d .9 .9 .9))	; ad hoc numbers
	   
	   (glVertexPointer 3 obj::*GL-VERTEX-TYPE*
			    (* obj::*gl-vertex-bytes-per-element*
			       (array-dimension obj::vertex-array 1))
			    obj::vertex-array)
	   
	   (multiple-value-bind (tile-x-dim tile-y-dim)	; texture tile dimensions 
	       (image_tex_tile_dims image)
	     (let* ((z-xdim (image-x-dim z-image))
		    (z-ydim (image-y-dim z-image))
		    (mat (transform-matrix z-image-to-2d-transform))
		    (xverts/tile (ash (floor tile-x-dim
					     (round (aref mat 0 0)))
				      level))
		    (yverts/tile (ash (floor tile-y-dim
					     (round (aref mat 1 1)))
				      level))
		    (img-start-x (ash (round (aref mat 0 3))
				      (- level)))
		    (img-start-y (ash (round (aref mat 1 3))
				      (- level)))
		    (xpixels/vert (/ tile-x-dim xverts/tile))
		    (ypixels/vert (/ tile-y-dim yverts/tile))
		    (vert-start-x (floor (- (align-tile-x image img-start-x tile-x-dim)
					    img-start-x)
					 xpixels/vert))
		    (vert-start-y (floor (- (align-tile-y image img-start-y tile-y-dim)
					    img-start-y)
					 ypixels/vert))
		  
		    (tiles-wide (ceiling z-xdim xverts/tile))
		    (tiles-hi (ceiling z-ydim yverts/tile))
		    )
	       ;; (format t "tex-map-object ~a~%" (list vert-start-x vert-start-y xverts/tile yverts/tile tiles-wide tiles-hi tile-x-dim tile-y-dim level))
	       ;; Needed at this point:
	       ;;   object image level tiles-wide tiles-hi
	       ;;   vert-start-x xverts/tile vert-start-y yverts/tile
	
	       (loop repeat tiles-hi
		     for verty fixnum from vert-start-y by yverts/tile
		     do (loop repeat tiles-wide			      
			      for vertx fixnum from vert-start-x by xverts/tile
			      ;; do (format t "texmap-1-tile ~a~%" (list vertx verty level))
			      do (texmap-1-tile object image vertx verty level)))
	       )))
      ;; cleanup-form
      (glPopAttrib))

    ;;(glFlush) (handle_gl_errors "tex-map-object end")
    ))
    
;;; We must handle case where the z-array is not exactly aligned to tex-tile grid.
;;; vert-x vert-y are aligned to tex tile lower-left corner.
(defmethod texmap-1-tile ((object dtm-tex-mapped-quad-mesh) image
			  vert-x vert-y
			  &optional (level 0))
  (declare (fixnum vert-x vert-y level))
  (with-slot-values (z-image z-image-to-2d-transform) object
    (let* ((tile-x-dim 256)
	   (mat (transform-matrix z-image-to-2d-transform))
	   (tile-y-dim tile-x-dim)
	   (verts-wide (1+ (image-x-dim z-image)))
	   (verts-hi (1+ (image-y-dim z-image)))
	   (vertex-row-pitch verts-wide)
	   (pix/vert (round (aref mat 0 0)))
	   (xverts/tile (ash (/ tile-x-dim pix/vert) level))
	   (yverts/tile (ash (/ tile-y-dim pix/vert) level))
	   (tex-dx (/ 1.0 xverts/tile))
	   (tex-dy (/ -1.0 yverts/tile))
	   (image-x (ash (+ (round (aref mat 0 3))
			    (* pix/vert vert-x))
			 (- level)))
	   (image-y (ash (+ (round (aref mat 1 3))
			    (* pix/vert vert-y))
			 (- level)))
	   ;;(1/vert (/ 1.0 xverts/tile))
	   (start-vert-x (max vert-x 0))
	   (end-vert-x (min (+ vert-x xverts/tile) (1- verts-wide)))
	   (start-vert-y (max vert-y 0))
	   (end-vert-y (min (+ vert-y yverts/tile) (1- verts-hi)))
	   (tex-start-x (if (< vert-x 0) (* tex-dx (- vert-x)) 0.0))
	   (tex-start-y (if (< vert-y 0) (- 1.0 (* tex-dy (- vert-y)))  1.0))
	   )
      (declare (type (simple-array double-float (* *)) mat))
      (declare (double-float tex-start-x tex-start-y tex-dx tex-dy))
      (declare (fixnum tile-x-dim tile-y-dim verts-wide verts-hi vertex-row-pitch
		       pix/vert
		       start-vert-x end-vert-x start-vert-y end-vert-y
		       xverts/tile yverts/tile  image-x image-y))
      (when (and (< image-x (image-x-dim image)) (< image-y (image-y-dim image)))
	(setup-texmap-tile (image-id image) image-x image-y)
	;;(format t "texmap-1-tile ~a~%" (list vert-x vert-y image-x image-y (/ 1.0 xverts/tile) start-vert-y end-vert-y))

	(loop for y fixnum from start-vert-y below end-vert-y
	      for tex-y double-float from tex-start-y by tex-dy
	      for vertex-row-index fixnum from (+ start-vert-x
						  (* start-vert-y vertex-row-pitch))
		by vertex-row-pitch
	      do
	   (glBegin GL_TRIANGLE_STRIP)
	   (loop for vert-index fixnum from vertex-row-index
		 for x fixnum from start-vert-x to end-vert-x
		 for tex-x double-float from tex-start-x by tex-dx
		 do (glTexCoord2d tex-x (+ tex-y tex-dy))
		    (glArrayElementEXT (+ vert-index vertex-row-pitch))
		    (glTexCoord2d tex-x tex-y)
		    (glArrayElementEXT vert-index)
		 )
	   (glEnd))))))

(defmethod texmap-1-tile ((object dtm-tex-mapped-quad-mesh) image
			  vert-x vert-y
			  &optional (level 0))
  ;;(declare (type-reduce number fixnum))
  (declare (fixnum vert-x vert-y level))
  (with-class-slot-values dtm-tex-mapped-quad-mesh (vertex-array z-image  z-image-to-2d-transform) object
    (texmap_1_tile (image-id image) level (transform-matrix z-image-to-2d-transform)
		   vert-x vert-y
		   vertex-array
		   (1+ (image-x-dim z-image)) (1+ (image-y-dim z-image)))))



;;; This is run when file is loaded



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

(declaim (special *tex-mesh* *tex-mesh-object-set*))

(defun make-alv-tex-mesh (tex-image z-image z-img-to-3d-transform
			  &key 2d-world-name  (win (selected-window)))
  (let* ((tex-image-2d-world (or (2d-world tex-image) (cme::get-2d-world-named 2d-world-name )))
	 (3d-world (3d-world tex-image-2d-world))
	 ;; FIXME -- this is only good for *alv-terrain-image-path*
	 (z-img-to-2d-transform
	  (make-4x4-coordinate-transform
	   (make-and-fill-4x4-matrix 32.0 0.0 0.0 0.0    ; 32 x 32 pixel terrain mesh
				     0.0 32.0 0.0 1280.0
				     0.0 0.0 1.0 0.0
				     0.0 0.0 0.0 1.0)))
	 (tex-mesh (make-instance 'dtm-tex-mapped-quad-mesh
				  :world 3d-world
				  :tex-image tex-image
				  :z-image z-image
				  :z-image-to-2d-transform z-img-to-2d-transform
				  :3d-to-2d-projection (3d-to-2d-projection tex-image-2d-world)))

	 (2d-world (cme::make-2d-world
		    :3d-world (cme::get-3d-world-named "alv")
		    :3d-to-2d-projection
		    (make-adjustable-camera (world-center-of-rotation *interactor*)
					    :height 1e3 :gsd 2.0
					    :win win :fov 20.0 )))
	 (view (make-view 
		win
		:display-attributes
		(make-instance 'view-display-attributes
			       :background-color (cv .3 .6 1.0 1.0))
		:2d-world 2d-world
		:2d-to-window-transform
		(make-4x4-coordinate-transform 
		 (make-2d-to-window-matrix win (cv 0.0 0.0) 1.0))))
	 (object-set (make-object-set (list tex-mesh)
				      :world 3d-world
				      :priority 9.0
				      :immediate-render-p t
				      :backing-store-p t :direct-render-p t))
	 )
    (setf (get-prop view :world-center-of-rotation) (cv 256.9 4532.1 6015.4))
    (setq *tex-mesh* tex-mesh
	  *tex-mesh-object-set* object-set)
    (push object-set (object-sets view))
    (push-view view win)
    (setf (get-prop view :sun-vector) (cv 0.6082805 -0.1486257 0.7796828 0.0))
    ;;(set-window-objects win (and nil (list tex-mesh)))
    ))




#|
(setq alv-dtm (load-image "$RADIUS/sites/alv/alv-dtm.g0"))
(setq *z-image* (img::image-linear-transform alv-dtm 98.49530689561705 0.0))
(setq *z-image* (img::image-linear-transform alv-dtm (* 30 *feet-per-meter*) 0.0))
(setq alv-2-44 (load-image "$RADIUS/site-2d-worlds/alv/alv-2-44/full-tiff/image.g0"))

(setq alv-dtm-image-to-3d-transform
      (make-4x4-coordinate-transform
       (make-and-fill-4x4-matrix
	98.49512 -.13215083 -.13852036 -7890.5884
	0.13259056 98.494644 0.15808614 176.01
	0.11577392 -0.37007928 98.42377 4.989898
	0.0 0.0 0.0 1.0)))
(clear-view-stack (selected-window))
(setf (get-prop (top-view) :world-center-of-rotation) (cv 256.9 4532.1 6015.4))

(cme::get-2d-world-named "alv-2-44"))
(MAKE-ALV-TEX-MESH alv-2-44 *z-image* :2d-world-name "alv-2-44")
(describe *tex-mesh*)
(obj::immediate-render-p *tex-mesh*)

(let* ((i 100 )(j 100) (obj *tex-mesh*) (zimg (z-image obj)))
  (obj::vertex-array-vertex (vertex-array *tex-mesh*) (+ i (* j (1+(image-x-dim zimg))))))

(array-dimensions (vertex-array *tex-mesh*))

(setq buf (img::make-dfloat-scan-line-buffer *z-image*))
(image-getline *z-image* buf 0 0)
(image-getline alv-dtm buf 0 0)
(loop for i from 0 below 16 collect (aref buf i))

98.49530689561705 feet GSD of alv-dtm = 30 meters

(setq alv-dtm (load-image "$RADIUS/sites/alv/alv-dtm.g0"))


(setf (priority (first (object-sets (top-view ".frm.f1.gl1")))) 9.0)
(setf (object-sets (top-view ".frm.f1.gl1")) (object-sets (top-view ".frm.f1.gl1")))
(progn tex-mesh)

(object-sets (top-view ".frm.f1.gl1"))
(setf (EXCLUDED-OBJECTS (car (object-sets (top-view ".frm.f1.gl1")))) nil)
(compute-vertices tex-mesh)

(progn 
  (setq dtm (img::load-image "$RADIUS/sites/alv/alv-dtm.g0"))

  (setq dtm-to-lvcs-transform
	(make-and-fill-4x4-matrix
	 98.49512 -.13215083 -.13852036 -7890.5884
	 0.13259056 98.494644 0.15808614 176.01
	 0.11577392 -0.37007928 98.42377 4.989898
	 0.0 0.0 0.0 1.0))

  (setq dtm-obj (make-instance 'dtm-quad-mesh2
			       :dtm-image dtm
			       :dtm-to-lvcs-transform dtm-to-lvcs-transform
			       :x0 0.0 :y0 0.0 :delta 1.0
			       :nx (image-x-dim dtm) :ny (image-y-dim dtm)
			       ))
  #+never
  (setf (graphics-style dtm-obj)
	(make-instance 'graphics-style
		      ;; :color-vector (fv 0.47 0.047 0.047 .2)
		       :color-vector (fv 0.47 0.047 0.047 .5)
		       ))
  )


(setf (graphics-style dtm-obj)
      (make-instance 'graphics-style
		     :color-vector (fv 0.47 0.047 0.047 .5)))
(setf (graphics-style dtm-obj) nil)

(setq *alv-2-44-projection-matrix* (multiply-matrices
				   *alv-2-44-interior-orientation-mat*
				   *alv-2-44-exterior-orientation-mat*))

(4x4-project-vector *alv-2-44-projection-matrix*
			  (cv -1374.0 2897.0 6241.0 1.0))
;; (CV 510.70991328753 2044.8322432710047 0.7031850350742847) 

(4x4-project-vector (invert-matrix *alv-2-44-projection-matrix*)
			  (CV 510.70991328753 2044.8322432710047 0.7031850350742847))

(progn (glMakeCurrent ".frm.f2.gl2")
       (glProject_to_window (cv -1374.0 2897.0 6241.0 1.0)))

 (CV -0.04031908506982873 587.0989973161336 0.8515924472903635)

(lx::eval-cache-flush z-img)

(progn 
  (setq z-img-to-2d-transform
	(make-and-fill-4x4-matrix 32.0 0.0 0.0 0.0
				  0.0 32.0 0.0 1024.0
				  0.0 0.0 1.0 0.0
				  0.0 0.0 0.0 1.0))

  (setq z-img (load-image "~/cp/lisp/cme-compat/data/alv-244-z.img"))

  (setq tex-mesh (make-instance 'dtm-tex-mapped-quad-mesh
				:tex-image alv-2-44
				:z-image z-img
				:z-image-to-2d-transform z-img-to-2d-transform
				:3d-to-2d-projection
				(list *alv-2-44-interior-orientation-mat*
				      *alv-2-44-exterior-orientation-mat*))))

(progn 
  (setq z-img-to-2d-transform
	(make-and-fill-4x4-matrix 32.0 0.0 0.0 0.0
				  0.0 32.0 0.0 1280.0
				  0.0 0.0 1.0 0.0
				  0.0 0.0 0.0 1.0))

  (setq z-img (load-image "~/cp/lisp/cme-compat/data/alv-244-z2.img"))

  (setq tex-mesh (make-instance 'dtm-tex-mapped-quad-mesh
				:tex-image alv-2-44
				:z-image z-img
				:z-image-to-2d-transform z-img-to-2d-transform
				:3d-to-2d-projection
				(list *alv-2-44-interior-orientation-mat*
				      *alv-2-44-exterior-orientation-mat*))))

(compute-vertex-normals tex-mesh)
(list (image-x-dim z-img) (image-y-dim z-img))
(image-block-y-dim (get-image-pyramid-level alv-2-44 1 0))
(image-y-dim (get-image-pyramid-level alv-2-44 1 0))
(reset-alv-camera-matrices)

(let* ((win (widget-window ".frm.f1.gl1"))
       (view (make-view 
	      win
	      :2d-world
	      (cme::make-2d-world
	       :3d-world (cme::get-3d-world-named "alv")
	       :3d-to-2d-projection
	       (make-adjustable-camera (world-center-of-rotation *interactor*)
				       :height 1e3 :gsd 2.0
				       :win win :fov 20.0
				       ))
	      :2d-to-window-transform
	      (make-2d-to-window-matrix win (cv 0.0 0.0) 1.0))))
  (push-view view win)
  (setf (get-prop view :sun-vector) (cv 0.6082805 -0.1486257 0.7796828 0.0))
  (set-window-objects win (and nil (list tex-mesh)))
  (setf (dynamic-3d-objects view) (list tex-mesh))
  )

(setf (dynamic-3d-objects (top-view ".frm.f1.gl1")) (list tex-mesh))
(resize-texid-page-pool 100)
(img::resize-image-page-pool alv-2-44 200)
(setq *dtm-tex-mapped-quad-mesh-image-level* 0)
(let* ((win ".frm.f1.gl1")
       (view (make-view
	      win
	      :2d-world
	      (make-2d-world
	       :3d-world (cme::get-3d-world-named "alv")
	       :3d-to-2d-projection
	       (make-adjustable-camera *world-center-of-rotation*
				       :height 1e5 :gsd 2.0))
	      :2d-to-window-transform
	      (make-2d-to-window-matrix win (cv 0.0 0.0) 1.0))))
  ;;(setf (get-prop view :sun-vector) (cv 0.0 0.0 1.0 0.0))
  (setf (get-prop view :sun-vector) (cv 0.6082805 -0.1486257 0.7796828 0.0))
  ;;(setf (get-prop view :sun-vector) (cv -0.6082805 0.1486257 -0.7796828 0.0))

  (set-objects win (list dtm-obj) 3))

(defparameter *world-center-of-rotation*  (CV -71.333 4541.89 6106.684))
(setq *world-center-of-rotation* (cv 3475.0 2658.0 5807.0))

(get-prop (top-view ".frm.f1.gl1") :sun-vector)


(defun print-verts (arr i0 n)
  (loop for i from i0 repeat n
	do (loop for j from 0 to 2
		 do (format t "~6,2f " (aref arr i j)))
	   (format t "~%")))


(defun ray-trace-raster (projection terrain u0 v0 nu nv delta)
  (let ((img (make-image (list nu nv) :element-type 'single-float)))
    (progn ;; ic::with-image-elements ((img :write))
      (loop for v from v0 by delta repeat nv
	    for j fixnum  from 0
	    do (loop for u from u0 by delta repeat nu
		     for i fixnum from 0
		     do (img::c-iset img i j
				     (bind-vector-elements (x y z)
					 ;; ray-tracing not yet implemented
					(intersect-ray-with-terain-model projection
					  (Cv u v)
					  terrain)
					(ignore x y)
					z
					))))
      img)))

(setq z-img (ray-trace-raster (3d-to-2d-projection (top-view))
			      (find-terrain-model (top-view))
			      0.0 1280.0 112 88
			      32.0))

(setq z-img (ray-trace-raster (3d-to-2d-projection (top-view))
			      (find-terrain-model (top-view))
			      0.0 1024.0 112 96
			      32.0))


|#
