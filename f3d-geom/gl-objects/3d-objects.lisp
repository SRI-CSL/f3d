(in-package :obj)

#|

|#

(defstruct-class 3d-object (gl-3d-object-mixin basic-gl-object) ())

(defstruct-class gl-xyz-sizable-object-mixin (gl-sizable-object-mixin gl-3d-object-mixin)
  (;(sizes :initform (cv 1.0 1.0 1.0) :initarg :sizes :accessor sizes)
   ))

(defmethod fasd-form-properties-list-do-not-dump :around ((obj gl-xyz-sizable-object-mixin))
  (append (call-next-method)
	  `(:face-normals :vertex-normals :complex-polygons :smooth-other-faces  )))


(defstruct-class 3d-crosshair-object (gl-3d-object-mixin crosshair-object) ())

(defstruct-class 3d-point-object (gl-3d-object-mixin point-object) ())

(defmethod short-name-string ((object 3d-point-object))
  "3d Point")

(defstruct-class 3D-CURVE (gl-3d-object-mixin curve) ())

(defmethod vertex-array-elements-per-vertex ((object 3D-CURVE)) 3)
;;(defmethod vertex-array-elements-per-vertex ((object 2d-curve)) 2)

(defstruct-class 3D-closed-CURVE (3D-CURVE) ()
  (:default-initargs :closed-p t))

(defstruct-class 3D-RIBBON-CURVE (gl-3d-object-mixin ribbon) ())
(defmethod vertex-array-elements-per-vertex ((object 3D-RIBBON-CURVE)) 4)

;;; problem here in CMUCL -- gl-label is forward referenced
;;; Move this to object-labels.lisp to avoid the forward reference.
;;; (defstruct-class 3d-text-object (gl-3d-object-mixin gl-label) ())

;; FIXME - just a temporary hack
(defstruct-class 3d-ruler-object (3d-curve) ())

(defmethod draw-object :after ((obj 3d-ruler-object) view)
  (let* ((graphics-style (or (object-graphics-style obj view)
                             *DEFAULT-GL-OBJECT-LABEL-GRAPHICS-STYLE*))
         (v0 (vertex-array-vertex (vertex-array obj) 0))
         (v1 (vertex-array-vertex (vertex-array obj) 1))
         (ruler-length (vector-euclidean-length (vector-difference v1 v0)))
         (ruler-midpoint (vector-times-scalar (vector-add v0 v1) 0.5)))
    (glDrawString-with-color obj
                             (case (get-prop obj :units)
                               (:feet   (format nil "~f feet" (* ruler-length *feet-per-meter*)))
                               (:inches (format nil "~f inches" (* 12 (* ruler-length *feet-per-meter*))))
                               (t       (format nil "~f meters" ruler-length)))
                             ruler-midpoint
                             (font graphics-style)
                             (color-vector graphics-style))))
             
;;; **********************  BASIC-EXTRUSION  ************************

#|
FIXME:  Need to sort out the differences between BASIC-EXTRUSION and EXTRUDED-OBJECT.
        Why must CYLINDER be an EXTRUDED-OBJECT when CUBE is a BASIC-EXTRUSION?
        Both object classes are parameterized by :SIZES and need not have :TOP-VERTICES specified.
|#

(defstruct-class basic-extrusion (gl-xyz-sizable-object-mixin)
    ((quad-strip-indices :initform nil :initarg :quad-strip-indices
			 :accessor quad-strip-indices)
     (other-polygon-indices :initform nil :initarg :other-polygon-indices
			    :accessor other-polygon-indices)))

;;; There are 4 ways a basic-extrusion be be instantiated:
;;;   1.  vertex-array is set to all of the vertices of the extrusion.
;;;   2.  vertices is a list of all of the vertices of the extrusion.
;;;   2.  top-vertices is a list of the vertices of the top face of the extrusion.
;;;   3.  none of vertex-array, vertices, or top-vertices is supplied and COMPUTE-VERTICES
;;;           must be called.

(defmethod initialize-instance :after
	   ((object basic-extrusion) &rest initargs 
	    &key vertex-array vertices top-vertices &allow-other-keys)
  ;;(setq *basic-extrusion-initargs* initargs)
  ;;(when (typep object 'extruded-object) (break))
  (when (consp (quad-strip-indices object))
    ;; This next occurs with cube-object
    (setf (quad-strip-indices object)
	  (make-vertex-index-array (quad-strip-indices object))))
  (when (other-polygon-indices object)
    ;; This next occurs with cube-object
    (setf (other-polygon-indices object)
	  (loop for thing in (other-polygon-indices object)
		collect (cond ((vectorp thing)
			       thing)
			      ((consp thing)
			       (make-vertex-index-array thing))
			      (t (error "illegal element ~a in other-polygon-indices"
					thing))))))

  (cond (vertex-array
	 ;; When we have a parametric extruded-object (such as cube-object), scale
	 ;; the object according to SIZES.
	 (scale-vertex-array (vertex-array object) (sizes object)))
	(vertices			; need to finish this
	 )
	(top-vertices 
	 (make-extrusion-vertex-array object top-vertices))
	(t (make-extrusion-vertex-array object (compute-top-face-vertices object))
	   ;;(scale-vertex-array (vertex-array object) (sizes object))
	   ))

  (make-extrusion-polygon-indicies object)

  (bind-vector-elements (xmin xmax ymin ymax zmin zmax) 
	(vertex-array-bounding-box (%vertex-array object))
    (setf (sizes object) (cv (- xmax xmin) (- ymax ymin) (- zmax zmin))))

  )

;;; Should have a class called z-axis-extruded-object.
;;; For now, basic-extrusion defaults to z-axis extrusion.
;;; Extrude in z-axis direction
(defmethod make-extrusion-vertex-array ((object basic-extrusion) top-vertices)
  (extrude-axis object 2 top-vertices)) ; extrude in z-axis direction

#|
In an extrusion, the EVEN NUMBERED vertices are the "TOP" vertices or those resulting
from the form (OR VERTICES TOP-VERTICES (COMPUTE-TOP-FACE-VERTICES OBJECT)).
The ODD NUMBERED vertices are those computed by extruding along an axis.

Here "TOP" refers to the face which defined the cross section of the extrusion.
  When axis-index = 0 the face is East facing.
  When axis-index = 1 the face is North facing.
|#

;;; Computes the VERTEX-ARRAY of object by extruding TOP-VERTICES.
;;; TOP-VERTICES are the vertices of the "top" face of the object and they are expected to be coplanar.
;;; TOP-VERTICES is a list of either coordinate-vectors or lists of double-floats. 
(defmethod extrude-axis ((object basic-extrusion) axis-index top-vertices)
  (declare (fixnum axis-index))
  (let* ((verts top-vertices)
	 (nverts/2 (length verts))
	 (nverts (* nverts/2 2))
	 (vertex-array (make-array0 (list nverts 3)
				    :element-type 'vertex-element-type))
	 (extrusion-size (aref (sizes object) axis-index))
	 ;;(zsize 1.0)
	 )
    (declare (type vertex-array-type  vertex-array))
    (declare (fixnum nverts/2 nverts))
    (declare (double-float extrusion-size))
    (loop for vert in verts
	  for i2 fixnum from 0 by 2
	  for i2+1 fixnum = (1+ i2)
	  for x double-float = (elt vert 0)
	  for y double-float = (elt vert 1)
	  for z double-float = (elt vert 2)
	  do (setf (aref vertex-array i2 0) x 
		   (aref vertex-array i2 1) y
		   (aref vertex-array i2 2) z
		   (aref vertex-array i2+1 0) x 
		   (aref vertex-array i2+1 1) y
		   (aref vertex-array i2+1 2) z)
	     (decf (aref vertex-array i2+1 axis-index) extrusion-size))
    (setf (%vertex-array object) vertex-array)))

;;; See comment above about the meaning of "TOP".
(defmethod extrusion-top-vertices ((object basic-extrusion))
  (let* ((verts (%vertex-array object))
	 (n (ash (array-dimension verts 0) -1))
	 (m (array-dimension verts 1))
	 (top-verts (make-array (list n m) :element-type 'double-float)))
    (loop for i from 0 below n
	  for 2i from 0 by 2
	  do (loop for j from 0 below m
		   do (setf (aref top-verts i j) (aref verts 2i j))))
    top-verts))

(defmethod make-extrusion-polygon-indicies ((object basic-extrusion))
  (let* ((nverts (array-dimension (vertex-array object) 0))
	 (nverts/2 (ash nverts -1))
	 (flip nil))
    (declare (fixnum nverts/2 nverts))

    (unless (quad-strip-indices object)
      (loop with quad-strip-indices of-type vertex-index-array-type
	      = (make-vertex-index-array (+ nverts 2))
	    for i fixnum from 0 below nverts/2
	    for i2 fixnum from 0 by 2
	    for i2+1 fixnum = (1+ i2)
	    do (setf (aref quad-strip-indices i2+1) i2+1
		     (aref quad-strip-indices i2) i2)
	    finally (setf (aref quad-strip-indices nverts) 0 ;(- nverts 1)
			  (aref quad-strip-indices (1+ nverts)) 1 ;(- nverts 2)
			  )
		    (setf (quad-strip-indices object) quad-strip-indices)))

    (unless (other-polygon-indices object)
      (setf (other-polygon-indices object)
	    (list ;; bottom face -- can't figure out how to cull back faces
	     ;; with tesselation and boundary-only
	     (loop with arr of-type vertex-index-array-type
		     = (make-vertex-index-array  nverts/2)
		   for i fixnum from 0 below nverts/2
		   do (if flip
			  (setf (aref arr i) (+ 1 i i))
			  (setf (aref arr (- nverts/2 1 i)) (+ 1 i i)))
		   finally (return arr))
	     (loop with arr of-type vertex-index-array-type
		     = (make-vertex-index-array  nverts/2)
		   for i fixnum from 0 below nverts/2
		   do (if flip
			  (setf (aref arr (- nverts/2 1 i)) (+  i i))
			  (setf (aref arr i) (+  i i)))
		   finally (return arr)))))
    ;;(break)
    ))


(defmethod update-object-after ((object basic-extrusion))
  (flush-transform-vertices-ht object)
  (setf (get-prop object :face-normals) nil
	(get-prop object :vertex-normals) nil))

(defun vertex-array-sequence-convex-p (verts indices)
  (declare (type vertex-index-array-type indices))
  (3d-polygon-convex-p (loop for i fixnum from 0 below (length indices)
			     collect (vertex-array-vertex verts (aref indices i)))))


(defmethod compute-face-normals ((object basic-extrusion))
  (or (get-prop object :face-normals)
      (setf (get-prop object :face-normals)
	    (let* ((verts (vertex-array object))
		   (indices (quad-strip-indices object))
		   (n-quad-strip-faces (1- (/ (length indices) 2)))
		   (other-polygon-indices (other-polygon-indices object))
		   (n-other-faces (length other-polygon-indices))
		   (normals (make-array0 (list (+ n-quad-strip-faces n-other-faces) 3)
					 :element-type 'vertex-element-type)))
	      (declare (type vertex-array-type verts normals))
	      (declare (type vertex-index-array-type indices))
	      (declare (fixnum n-quad-strip-faces))
		   
	      (labels ((normal (face v0 v1 v2)
			 (compute-vertex-array-face-normal verts normals
							   v0 v1 v2 face)))
		(loop for face fixnum from 0 below n-quad-strip-faces
		      for i fixnum from 0 by 2
		      do (normal face (aref indices i)
				      (aref indices (+ i 1))
				      (aref indices (+ i 2))))
		(loop with complex-p = nil
		      for face fixnum from n-quad-strip-faces repeat n-other-faces
		      for indices of-type vertex-index-array-type
		      in other-polygon-indices
		      do (bind-vector-elements (nx ny nz)
			     (vertex-array-polygon-normal verts indices)
			   (set-vertex-array-elements normals face nx ny nz))

			 ;; We also check convexity here
		      when (and (not complex-p)
				(null (vertex-array-sequence-convex-p verts indices)))
			do (setf complex-p t)
		      finally (setf (get-prop object :complex-polygons) complex-p)
					
		      ))
		
	      normals))))
#|
(car (car (selected-objects)))
(get-prop (car (car (selected-objects))) :face-normals)
(get-prop (car (car (selected-objects))) :complex-polygons)
(other-polygon-indices (car (car (selected-objects))))
(setf (get-prop (car (car (selected-objects))) :face-normals) nil)
(compute-face-normals (car (car (selected-objects))))

|#

(defmethod compute-vertex-normals ((object basic-extrusion))
  (get-prop object :vertex-normals))

(defun compute-vertex-normal (&rest face-normals)
  (loop with (sum-nx sum-ny sum-nz) double-float
	for face-normal in face-normals
	do (bind-vector-elements (nx ny nz) face-normal
	     (incf sum-nx nx) (incf sum-ny ny) (incf sum-nz nz))
	finally (normalize-vector-elements sum-nx sum-ny sum-nz )
		(return (cv sum-nx sum-ny sum-nz))))

		
(defparameter *gl-shading-enabled* nil)
(defparameter *gl-shading-always-enabled* t)

#|
(setq *gl-shading-always-enabled* nil)
(setq *gl-shading-enabled* t)

(setq *obj* (caar (gui::selected-objects)))
(describe obj::*interactor*)
|#

(defvar *obj* nil)

;;; It isn't clear that vertex-array usage here buys enough to justify the
;;; extra code complexity.
(defmethod draw-object ((object basic-extrusion) view)
  (declare (ignorable view))
  ;;(format t "draw-object ~a~%"  object)
  #+never
  (setq *gl-mats* (list (glGetMatrix GL_MODELVIEW_MATRIX) (glGetProjectionMatrix)))
  (let* ((verts (vertex-array object))
	 (vertex-normals (and (or *gl-shading-always-enabled* *gl-shading-enabled*)
			      (compute-vertex-normals object)))
	 (face-normals (and (or *gl-shading-always-enabled* *gl-shading-enabled*)
			    (compute-face-normals object))))
    (unless *draw-debug*
      (cond (vertex-normals
	     ;;(format t "draw-object-with-vertex-normals ~a~%" object)
	     (draw-object-with-vertex-normals object vertex-normals))
	    
	    (face-normals
	     ;;(format t "draw-object-with-face-normals ~a~%" object)
	     (draw-object-with-face-normals object))
	    
	    ((allow-vertex-arrays-in-display-lists)
	     (with-object-transform object
	       ;; VERTS is allocated in dynamic-space and can be moved by
	       ;; GC. Why does this work?
	       (glVertexPointer 3 *GL-VERTEX-TYPE*
				(* *gl-vertex-bytes-per-element* (array-dimension verts 1))
				verts)
	       (glDrawElements GL_QUAD_STRIP (length  (quad-strip-indices object))
			       *GL-VERTEX-INDEX-TYPE*
			       (quad-strip-indices object))
	       (loop with complexp = (get-prop object :complex-polygons)
		     for poly in (other-polygon-indices object)
		     do (if complexp
			    (draw-polygon-tesselated verts poly ) 
			    (glDrawElements GL_POLYGON (length poly)
					    *GL-VERTEX-INDEX-TYPE*
					    poly)))
	       ))

	    (t (with-object-transform object
		 ;; VERTS is allocated in dynamic-space and can be moved by
		 ;; GC. Why does this work?
		 ;;(format t "draw-object basic-extrusion ~a~%" object)
		 (let* ((indices  (quad-strip-indices object))
			(n-quad-strip-faces (1- (/ (length indices) 2))))
		   (glDrawquad_strip_indexed verts n-quad-strip-faces indices))
		 (loop with complexp = (get-prop object :complex-polygons)
		       for poly in (other-polygon-indices object)
		       do (if complexp
			      (draw-polygon-tesselated verts poly ) 
			      (draw_polygon verts poly (length poly) nil 0)))))
	    ))))


(defparameter *obj* nil)
#|
(setq *obj* nil)
(setq *obj* (caar (gui::selected-objects)))


(defun flip-poly-indices (v1)
  (loop with n = (length v1)
	with v2 = (make-vertex-index-array n)
	for i from 0 below n
	for j downfrom (1- n)
	do (setf (aref v2 j) (aref v1 i))
	finally (return v2)))
|#

;;; I do not think we need to worry about *glu-1-1* any longer.
(defparameter *glu-1-1* nil) ; *GLU-1-1* doesn't seem to do face normals correctly.


;;; It isn't clear that vertex-array usage here buys enough to justify the
;;; extra code complexity.
(defmethod draw-object-with-vertex-normals ((object basic-extrusion) vertex-normals)
  (let* ((verts (vertex-array object))
	 (complexp (get-prop object :complex-polygons))
	 (indices (quad-strip-indices object))
	 (n-quad-strip-faces (1- (/ (length indices) 2)))
	 (ncomps (array-dimension verts 1)))

    (flet ((do-flat-other-faces ()
	     (glShadeModel GL_FLAT)
	     (loop with face-normals = (compute-face-normals object)
		   for poly-indices in (other-polygon-indices object)
		   for face-index fixnum from n-quad-strip-faces
		   do (glNormal3d (aref face-normals face-index 0)
				  (aref face-normals face-index 1)
				  (aref face-normals face-index 2))
		      (if *GLU-1-1*
			  ;; *GLU-1-1* doesn't  seem to do face normals correctly.
			  ;; For cylinders and quansets, we want flat shading anyhow.
			  (progn (glBegin GL_POLYGON)
				 (loop for i fixnum from 0 below (length poly-indices)
				       for j fixnum = (aref poly-indices i)
				       do (glVertex3d (aref verts j 0)
						      (aref verts j 1)
						      (aref verts j 2)))
				 (glEnd))
			  (draw-polygon-tesselated verts poly-indices))
		   )))
      (with-object-transform object
	(glPushAttrib GL_POLYGON_BIT)
	(glShadeModel GL_SMOOTH)
	(cond ((allow-vertex-arrays-in-display-lists)
	       (glVertexPointer 3 *GL-VERTEX-TYPE*
				(* *gl-vertex-bytes-per-element* ncomps)
				verts)
	       (glEnableClientState GL_NORMAL_ARRAY)
	       (glNormalPointer *GL-VERTEX-TYPE*
				(* *gl-vertex-bytes-per-element*
				   (array-dimension vertex-normals 1))
				vertex-normals)
	       (glDrawElements GL_QUAD_STRIP (length indices)
			       *GL-VERTEX-INDEX-TYPE* indices)
	       (cond ((get-prop object :smooth-other-faces)
		      (loop for poly-indices in (other-polygon-indices object)
			    do (if complexp
				   (draw-polygon-tesselated verts poly-indices) 
				   (glDrawElements GL_POLYGON (length poly-indices)
						   *GL-VERTEX-INDEX-TYPE*
						   poly-indices)))
		      (glDisableClientState GL_NORMAL_ARRAY))
		     (t (glDisableClientState GL_NORMAL_ARRAY)
			(do-flat-other-faces))))

	      (t
	       (glDrawquad_strip_indexed_with_vertex_normals
		verts vertex-normals n-quad-strip-faces indices)
	       (cond ((get-prop object :smooth-other-faces)
		      (loop for poly-indices in (other-polygon-indices object)
			    do (if complexp
				   (draw-polygon-tesselated verts poly-indices)
				   (draw_polygon verts poly-indices (length poly-indices) nil 0))))
		     (t (do-flat-other-faces)))))
	(glPopAttrib)
	))))
     

(defmethod draw-object-with-face-normals ((object basic-extrusion))
  (Let* ((verts (vertex-array object))
	 (normals (compute-face-normals object))
	 (indices (quad-strip-indices object))
	 (n-quad-strip-faces (1- (/ (length indices) 2)))
	 (complexp (get-prop object :complex-polygons))
	 )
    (declare (type vertex-array-type verts normals))
    (declare (fixnum n-quad-strip-faces))
    (declare (type vertex-index-array-type indices))
    ;;(when (eq object *obj*) (format t "draw-object-with-face-normals ~a~%" object))
     (with-object-transform object
      ;; draw the faces that are organized in a quad-strip
      (glDrawquad-strip-with-face-normals verts normals n-quad-strip-faces indices)
      ;;(glDrawquad-strip-with-face-normals2 verts normals n-quad-strip-faces indices)
      
      ;; draw the other (top and bottom) faces
      ;;(break)
      (loop for vert-indices in (other-polygon-indices object)
		   for face-index fixnum from n-quad-strip-faces
		   do (if complexp
			  (draw-polygon-tesselated verts vert-indices
						   :normals normals
						   :normal-index face-index)
		   
			  ;; Should this be a call to C?
			  (if t
			      (draw_polygon verts vert-indices (length vert-indices) normals face-index)
		       
			      (progn (glBegin GL_POLYGON)
			      ;;; apparently the normal calculation is wrong
				     (glNormal3d (- (aref normals face-index 0))
						 (- (aref normals face-index 1))
						 (- (aref normals face-index 2)))
				     (loop for i fixnum from 0 below (length vert-indices)
					   for j fixnum = (aref vert-indices i)
					   do (glVertex3d (aref verts j 0)
							  (aref verts j 1)
							  (aref verts j 2)))
				     (glEnd)))))
      )))


(defun glNormal3dvi (normals i)
  (declare (type (simple-array double-float (* *)) normals))
  (declare (fixnum i))
  (glNormal3d (aref normals i 0) (aref normals i 1) (aref normals i 2))
  )

(defun glVertex3dvi (verts i)
  (declare (type (simple-array double-float (* *)) verts))
  (declare (fixnum i))
  (glVertex3d (aref verts i 0) (aref verts i 1) (aref verts i 2))
  )

(defun glDrawquad-strip-with-face-normals2 (verts normals nfaces indices)
  (declare (type (simple-array (UNSIGNED-BYTE 16) (*)) indices))
  (declare (fixnum nfaces))
  ;;#+never
  (loop for face fixnum from 0 below nfaces
	for vert fixnum from 0 by 2
	do (glBegin GL_POLYGON)
	   (if nil ;(= face (1- nfaces))
	       (glNormal3dvi normals 0)
	       (glNormal3dvi normals face))
	   (glVertex3dvi verts (aref indices vert))
	   (glVertex3dvi verts (aref indices (+ vert 1)))
	   (glVertex3dvi verts (aref indices (+ vert 3)))
	   (glVertex3dvi verts (aref indices (+ vert 2)))
	   ;;(glVertex3dvi verts (aref indices vert))
	   (glEnd)))
	       


;;;
;;; Hack for Mac OS X - not sure why this is happening, but under Mac
;;; OS X X11/OpenGL, cylinders are not properly drawn when using
;;; glDrawElements.  In this case, punt back to explicit draws within
;;; glBegin/End pairs.  That seems to work ok...
;;;
;;; This draws the extrusion without support for face-normals or vertex-normals
;;; and without using glDrawElements.
;;; The problem on Mac OS X might have been specific to a FREEDIUS bug in using
;;; glVertexPointer glNormalPointer when it should not.  This should now be fixed.

;;; Mon Nov 15 2004 LHQ:  According to Chris Connolly, this problem in MacOSX is gone.

;;;#+macosx
;;;(defun MACOSX-draw-extrusion (object)
;;;  ;;(format t "draw-object ~a~%"  object)
;;;  #+never
;;;  (when (eq object *obj*)
;;;    (format t "draw-object basic-extrusion GL-STATE=~a~%"
;;;            (gl::glstate '(GL_DEPTH_TEST GL_DEPTH_WRITEMASK
;;;                      ;; GL_POLYGON_OFFSET_FILL GL_POLYGON_OFFSET_LINE GL_POLYGON_OFFSET_FACTOR GL_POLYGON_OFFSET_UNITS
;;;                           GL_POLYGON_STIPPLE GL_POLYGON_MODE
;;;                           GL_CULL_FACE GL_CULL_FACE_MODE GL_LINE_WIDTH GL_SHADE_MODEL GL_LIGHTING ))))
;;;  (let ((verts (vertex-array object)))
;;;    (unless *draw-debug*
;;;      (with-object-transform object
;;;          ;; Draw the quad strips first
;;;          (glBegin GL_QUAD_STRIP)
;;;          (loop for i across (quad-strip-indices object)
;;;                do (glVertex3d (aref verts i 0)
;;;                               (aref verts i 1)
;;;                               (aref verts i 2)))
;;;          (glEnd)
;;;          (loop with complexp = (get-prop object :complex-polygons)
;;;                for poly in (other-polygon-indices object)
;;;                do (if complexp
;;;                       (draw-polygon-tesselated verts poly ) 
;;;                     (progn
;;;                       (glBegin GL_POLYGON)
;;;                       (loop for i across poly
;;;                             do (glVertex3d (aref verts i 0)
;;;                                            (aref verts i 1)
;;;                                            (aref verts i 2)))
;;;                       (glEnd)
;;;                       )))
;;;          ))))

;;; ****************************  EXTRUDED-OBJECT  ****************************

;;; The EXTRUDED-OBJECT class should only be used for "free-form", ie. non-parametric
;;; extrusions which require either VERTICES or TOP-VERTICES be passed
;;; when creating the object.  
;;; FIXME: BASIC-EXTRUSION does not yet support interactive instantiation.

(defstruct-class extruded-object (basic-extrusion)
    ())

(defmethod initialize-instance :after ((object extruded-object ) &key &allow-other-keys)
  (bind-vector-elements (xmin xmax ymin ymax zmin zmax) 
      (vertex-array-bounding-box (vertex-array object))
    (setf (sizes object) (cv (- xmax xmin) (- ymax ymin) (- zmax zmin))))
  )

;;; This is called for interactively created extrusions.
(defmethod compute-top-face-vertices ((object extruded-object ))
  (error "Instantiating an EXTRUDED-OBJECT without specifying either TOP-VERTICES or VERTICES"))


;;; This is compatible with the CME cube-object vertex numbers.
(defstruct-class cube-object (3d-object basic-extrusion) ()
  (:default-initargs
      :sizes (cv 20.0 20.0 5.0)
      :vertex-array (make-vertex-array (cv -.5 -.5 -.5)
				       (cv .5 -.5 -.5)
				       (cv .5 .5 -.5)
				       (cv -.5 .5 -.5)
				       (cv -.5 -.5 .5)
				       (cv .5 -.5 .5)
				       (cv .5 .5 .5)
				       (cv -.5 .5 .5))
				     
    ;; ccw order
    :quad-strip-indices '(0 4 3 7 2 6 1 5 0 4)
    :other-polygon-indices '((4 5 6 7) (3 2 1 0))
    ))



;;; *************************   SPHERE   *************************

;;; WARNING:  This can be inaccurate at large dimensions for non-4x4-projections.
;;;           It assumes that the 3d-to-2d-projection is locally isotropic (Cartesian).

(defstruct-class sphere (gl-xyz-sizable-object-mixin)
    ((order :initform '(12 12) :initarg :order :accessor order)
     ))

(defmethod draw-object ((object sphere) view)
  (declare (ignorable view))
  (with-object-transform object
    (bind-vector-elements (x-size y-size z-size) (sizes object)
      (ignore y-size z-size)
      (setq x-size (abs x-size))
      (let ((quadObj (get_quadric))
	    (order (order object))
	    (radius (* .5 x-size)))
	(glTranslatedv (perturbed-object-origin object))  ; This hasn't been debugged.
	(gluQuadricDrawStyle quadObj GLU_FILL)
	(gluQuadricOrientation quadObj GLU_OUTSIDE)
	;;(gluQuadricDrawStyle quadObj GLU_LINE)
	(gluQuadricNormals quadObj GLU_SMOOTH)
	(gluSphere quadObj radius (car order) (cadr order))
	))))


;;; *************************   CYLINDER   *************************

#| ;;; I couldn't get gluQuadrics and gluCylinder to work.

(defstruct-class cylinder (gl-xyz-sizable-object-mixin)
  ((order :initform '(12 1) :initarg :order :accessor order)
   )
  (:default-initargs
      :vertex-array (make-vertex-array (cv 0.0 0.0 0.0))))

(defmethod draw-object ((object cylinder) view)
  (declare (ignorable view))
  (with-object-transform object
    (bind-vector-elements (x-size y-size z-size) (sizes object)
      (ignore y-size)
      (setq z-size (abs z-size) x-size (abs x-size))
      (let ((quadObj (get_quadric))
	    (order (order object))
	    (radius (* .5 x-size))
	    ;;(sign (signum (* x-size y-size z-size)))
	    )
	(gluQuadricDrawStyle quadObj GLU_FILL)
	(gluQuadricOrientation quadObj GLU_OUTSIDE)
	;;(gluQuadricDrawStyle quadObj GLU_LINE)
	(gluQuadricNormals quadObj GLU_SMOOTH)
	#+never
	(format t "gluCylinder ~a~%" (list object radius z-size order (sizes object)
					   view
					   (transform-vector ;;(object-to-world-transform object)
					    (transforms::object-to-view-transform object view)
					    (cv 0.0 0.0 0.0))))
	;; gluCylinder (quad, base, top, height, slices, stacks)
	(let ((vertices (vertex-array object)))
	  (glTranslated (aref vertices 0 0) (aref vertices 0 1) (aref vertices 0 2)))
	(gluCylinder quadObj radius radius z-size (car order) (cadr order))
	#+never
	(progn (glTranslated 0.0 0.0 z-size)
	       ;; need to generate a circle instead of this.
	       (gluDisk quadObj 0.0 radius (car order) 1))

	(gl-draw-circle z-size radius (car order))
	(gl-draw-circle 0.0 (- radius) (car order) (cv 0.0 0.0 -1.0))
	))))



(defmethod gl-draw-polygon (verts &optional normal)
  (glBegin GL_POLYGON)
  (when normal
    (glNormal3dv normal))
  (loop for i fixnum from 0 below (length verts)
	do (glVertex3dv (aref verts i)))
  (glEnd))

;;; This assumes the top circle has normal = (0 0 1), and center = (0 0 zpos)
;;; and the bottom circle has normal = (0 0 -1), and center = (0 0 0)

(defmethod gl-draw-circle (zpos radius nverts &optional flip)
  (declare (double-float zpos radius))
  (let ((verts (make-array nverts :initial-element nil))
	(sign (if flip -1.0 1.0)))
    (loop with dtheta = (* sign (/ 360.0 (float nverts)))
	  for theta double-float from 0.0 by dtheta
	  for i fixnum from 0 below nverts
	  for sin double-float = (sind theta)
	  for cos double-float = (cosd theta)
	  do (setf (aref verts i) (cv (* radius cos) (* radius sin) zpos)))
    (gl-draw-polygon verts (cv 0.0 0.0 sign))
    ))
  
(defmethod gl-draw-circle (zpos radius nverts &optional flip)
  (declare (double-float zpos radius))
  (let ((sign (if flip -1.0 1.0)))
    (glNormal3dv (cv 0.0 0.0 sign))
    (loop with dtheta = (* sign (/ 360.0 (float nverts)))
	  for theta double-float from 0.0 by dtheta
	  for i fixnum from 0 below nverts
	  for sin double-float = (sind 	  for cos double-float = (cosd theta)
	  do (glVertex3d (* radius cos) (* radius sin) zpos))))
|#


(defstruct-class cylinder (basic-extrusion) 
  ((order :initform '(12 1) :initarg :order :accessor obj::order)))

(defmethod update-object-after ((object cylinder))
  (flush-transform-vertices-ht object))

;;; Computes the vertex normals and returns the "top" vertices of a cylinder.
(defmethod compute-top-face-vertices ((object cylinder))
  (bind-vector-elements (x-size y-size z-size) (obj::sizes object)
    (loop with nslices fixnum = (car (obj::order object))
	  with nverts = nslices
	  with vertex-normals = (make-array0 (list (* 2 nverts) 3)
				     :element-type 'obj::vertex-element-type)
	  with x-size/2 double-float = (* .5 x-size)
	  with y-size/2 double-float = (* .5 y-size)
	  with z-size/2 double-float = (* .5 z-size)
	  for i fixnum from 0 below nverts
	  for 2i fixnum from 0 by 2
	  for theta double-float from 0.0 by (/ 360.0 (float nslices))
	  for sin double-float = (sind theta)
	  for cos double-float = (cosd theta)
	  collect (cv (* x-size/2 cos) (* y-size/2 sin) z-size/2)
	  do (obj::set-vertex-array-elements vertex-normals 2i cos sin 0.0)
	     (obj::set-vertex-array-elements vertex-normals (1+ 2i) cos sin 0.0)
	  finally ;;(setf (get-prop object :smooth-other-faces) t)
		  (setf (get-prop object :vertex-normals) vertex-normals))
    ))

;;; Chris:  Is this still needed?  As of Mon Nov 15 2004 -- no.
;;;#+macosx
;;;(defmethod draw-object ((object cylinder) view)
;;;  (declare (ignore view))
;;;  (MACOSX-draw-extrusion  object))
;;; Chris:  Is this still needed?
#+never
(defmethod draw-object ((object cylinder) view)
  (declare (ignore view))
  (MACOSX-draw-extrusion  object))

;;; ****************************  HALF-CYLINDER  ****************************

(defparameter *quanset-rotation* nil)

(defstruct-class half-cylinder (basic-extrusion)
    ((order :initform '(12 1) :initarg :order :accessor order)))

(defmethod make-extrusion-vertex-array ((object half-cylinder) top-vertices)
  (extrude-axis object 1 top-vertices))

(defmethod update-object-after ((object half-cylinder))
  (flush-transform-vertices-ht object))

(defmethod scale-vertex-array ((o half-cylinder) scale-vector)
  (let ((zscale (aref scale-vector 0))	; apply same scale change to x and z components.
	(sizes (sizes o)))	
    (setf (aref scale-vector 2) zscale)
    (setf (aref sizes 2) (* zscale (aref sizes 2))))

  (scale-vertex-array (vertex-array o) scale-vector))

(defmethod compute-top-face-vertices ((object half-cylinder))
  (let ((sizes (sizes object)))
    (setf (aref sizes 2) (* .5 (aref sizes 0)))) ; constraint z-size = (* .5  x-size)
  (bind-vector-elements (x-size y-size z-size) (sizes object)
    (setq *compute-top-face-vertices-args* (list object (copy-array (sizes object))))
    (loop with nslices fixnum = (car (order object))
	  with nverts = (1+ (ash nslices -1))
	  with vertex-normals = (make-array0 (list (* 2 nverts) 3)
				     :element-type 'vertex-element-type)
	  with x-size/2 double-float = (* .5 x-size)
	  with y-size/2 double-float = (* .5 y-size)
	  ;;with z-size/2 double-float = (* .5 z-size)
	  for i fixnum from 0 below nverts
	  for 2i fixnum from 0 by 2
	  for theta double-float downfrom  180.0 by (/ 360.0 (float nslices))
	  for sin double-float = (sind theta)
	  for cos double-float = (cosd theta)
	  collect (cv (* x-size/2 cos) y-size/2 (* z-size sin))
	  do (set-vertex-array-elements vertex-normals 2i cos 0.0 sin)
	     (set-vertex-array-elements vertex-normals (1+ 2i) cos 0.0 sin)
	  finally ;;(setf (get-prop object :smooth-other-faces) t)
		  (setf (get-prop object :vertex-normals) vertex-normals))
    ))
		     
;;; Patches to handle bugs in macosx drawing...
;;;
;;;
;;; Both cylinders and half-cylinders are affected in Mac OS X.  Other
;;; extruded objects are not.
;;;

;;;#+macosx
;;;(defmethod draw-object ((object half-cylinder) view)
;;;  (declare (ignore view))
;;;  (MACOSX-draw-extrusion object))


(defstruct-class vertical-half-cylinder (basic-extrusion)
    ((order :initform '(12 1) :initarg :order :accessor order)))

(defmethod update-object-after ((object half-cylinder))
  (flush-transform-vertices-ht object))


;;; LHQ Mon Jul 13 2009 -- Corrected apparent error where make-extrusion-vertex-array and
;;; compute-top-face-vertices were defined for half-cylinder rather than vertical-half-cylinder.

(defmethod make-extrusion-vertex-array ((object vertical-half-cylinder) top-vertices)
  (extrude-axis object 2 top-vertices))

(defmethod compute-top-face-vertices ((object vertical-half-cylinder))
  (bind-vector-elements (x-size y-size z-size) (sizes object)
    (loop with nslices fixnum = (car (order object))
	  with nverts = (1+ (ash nslices -1))
	  with vertex-normals = (make-array0 (list (* 2 nverts) 3)
				     :element-type 'vertex-element-type)
	  with x-size/2 double-float = (* .5 x-size)
	  with y-size/2 double-float = (* .5 y-size)
	  ;;with z-size/2 double-float = (* .5 z-size)
	  for i fixnum from 0 below nverts
	  for 2i fixnum from 0 by 2
	  for theta double-float downfrom 180.0 by (/ 360.0 (float nslices))
	  for sin double-float = (sind theta)
	  for cos double-float = (cosd theta)
	  collect (cv (* x-size/2 cos) (* y-size/2 sin)  z-size)
	  do (set-vertex-array-elements vertex-normals 2i cos sin 0.0)
	     (set-vertex-array-elements vertex-normals (1+ 2i) cos sin 0.0)
	  finally ;;(setf (get-prop object :smooth-other-faces) t)
		  (setf (get-prop object :vertex-normals) vertex-normals))
    ))

(defmethod compute-top-face-vertices ((object vertical-half-cylinder))
  (bind-vector-elements (x-size y-size z-size) (sizes object)
    (loop with nslices fixnum = (car (order object))
	  with nverts = (1+ (ash nslices -1))
	  with vertex-normals = (make-array0 (list (* 2 nverts) 3)
				     :element-type 'vertex-element-type)
	  with x-size/2 double-float = (* .5 x-size)
	  with y-size/2 double-float = (* .5 y-size)
	  ;;with z-size/2 double-float = (* .5 z-size)
	  for i fixnum from 0 below nverts
	  for 2i fixnum from 0 by 2
	  for theta double-float from 0.0 by (/ 360.0 (float nslices))
	  for sin double-float = (sind theta)
	  for cos double-float = (cosd theta)
	  collect (cv (* x-size/2 cos) (* y-size/2 sin)  z-size)
	  do (set-vertex-array-elements vertex-normals 2i cos sin 0.0)
	     (set-vertex-array-elements vertex-normals (1+ 2i) cos sin 0.0)
	  finally ;;(setf (get-prop object :smooth-other-faces) t)
		  (setf (get-prop object :vertex-normals) vertex-normals))
    ))
	

;;; ***************************  LIST-OF-FACES-OBJECT  ***************************

(defstruct-class list-of-faces-object (gl-xyz-sizable-object-mixin)
    ((face-vertex-list :initform nil :initarg :face-vertex-list :accessor face-vertex-list)
     )   
  )



(defmethod initialize-instance :after ((object list-of-faces-object) &key &allow-other-keys)
  (with-class-slots list-of-faces-object (face-vertex-list) object
    (when face-vertex-list
      (setf face-vertex-list
	    (loop for face-verts in face-vertex-list
		  collect (if (arrayp face-verts)
			      face-verts
			      (make-vertex-index-array face-verts)))))))

(defmethod draw-object ((object list-of-faces-object) view)
  (declare (ignorable view))
  (let* ((verts (vertex-array object))
	 (vertex-normals (and (or *gl-shading-always-enabled* *gl-shading-enabled*)
			      (compute-vertex-normals object)))
	 (face-normals (and (or *gl-shading-always-enabled* *gl-shading-enabled*)
			    (compute-face-normals object))))
    (cond (vertex-normals
	   ;;(format t "draw-object-with-vertex-normals ~a~%" object)
	   (draw-object-with-vertex-normals object vertex-normals))
	    
	  (face-normals
	   ;;(format t "draw-object-with-face-normals ~a~%" object)
	   (draw-object-with-face-normals object))

	  (t (with-object-transform object
	       (loop with complexp = (get-prop object :complex-polygons)
		     for face-verts in (face-vertex-list object)
		     do (if complexp
			    (draw-polygon-tesselated verts face-verts ) 
			    (draw_polygon verts face-verts (length face-verts) nil 0)))))
	  )))


(defmethod draw-object ((object list-of-faces-object) view)
  (declare (ignorable view))
  (let* ((verts (vertex-array object)))
    (with-object-transform object
      (loop with complexp = (get-prop object :complex-polygons)
	    for face-verts in (face-vertex-list object)
	    do (if complexp
		   (draw-polygon-tesselated verts face-verts ) 
		   (draw_polygon verts face-verts (length face-verts) nil 0))))))

#|

(defmacro test-3d-object (object-creation-form)
  `(let* ((view (gui::top-view))
	  (obj ,object-creation-form)
	  (os (gui::make-object-set (list obj) :world (3d-world view) :immediate-render-p t)))
     (push os (gui::object-sets view))))

(let* ((view (gui::top-view))
       (obj (make-instance 'list-of-faces-object
			   :world (3d-world view)
			   :object-to-world-transform
			   (make-4x4-coordinate-transform
			    (make-object-to-parent-matrix (cv 266.9 4632.1 6015.4)))
			   :vertices '((0.0 0.0 0.0)
				       (100.0 0.0 0.0)
				       (0.0 100.0 0.0)
				       (0.0 0.0 100.0))
			   :face-vertex-list '((0 1 2 3))))
       (os (gui::make-object-set (list obj) :world (3d-world view) :immediate-render-p t)))
  (push os (gui::object-sets view))
  )

(pop (gui::object-sets (gui::top-view)))

(setq *os* (gui::object-sets (gui::top-view)))
(describe (find-class '3d-point-object))
|#


;;; *********************************  AXIS-OBJECT  *********************************

(defstruct-class axis-object (no-vertices-mixin  gl-3d-object-mixin) ;(no-vertices-mixin gl-object gl-3d-object-mixin)
    ((axis-length :initform 20.0)))

(defmethod initialize-instance :after ((object axis-object) &key &allow-other-keys)
  (setf (get-prop object :immediate-render-p) t))

;;;(defmethod draw-object ((object axis-object) view)
;;;  (with-class-slot-values axis-object (axis-length) object
;;;    (declare (double-float axis-length))
;;;    (let* ((position (origin (object-to-parent-transform object)))
;;;           (projection (list (3d-to-2d-projection view) (2d-to-window-transform view)))
;;;           (gsd (compute-gsd projection position))) ; COMPUTE-GSD needs to be optimized
;;;      (when gsd ;; gsd is NIL when point is behind camera
;;;        (glPushAttrib GL_DEPTH_BUFFER_BIT)
;;;        (glDisable GL_DEPTH_TEST)
;;;        (let ((scale (* axis-length (compute-gsd projection position))))
;;;          (declare (double-float scale))
;;;          ;;(format t "draw-object axis-object ~a ~a~%" position scale)
;;;          (glBegin GL_LINES)
;;;          (glVertex3d 0.0 0.0 0.0)
;;;          (glVertex3d scale 0.0 0.0)
;;;          (glVertex3d 0.0 0.0 0.0)
;;;          (glVertex3d 0.0 scale 0.0)
;;;          (glVertex3d 0.0 0.0 0.0)
;;;          (glVertex3d 0.0 0.0 scale)
;;;          (glEnd))
;;;        
;;;        (glPopAttrib)))))

(defmethod draw-object ((object axis-object) view)
  (with-class-slot-values axis-object (axis-length) object
    (declare (double-float axis-length))
    (let* ((3d-to-2d-projection (3d-to-2d-projection view))
	   (projection (list 3d-to-2d-projection (2d-to-window-transform view)))
	   (world-position (transform-vector 
			    (object-to-world-transform object (from-coordinate-system 3d-to-2d-projection))
			    *zero-vector*))
	   (gsd (compute-gsd projection world-position))) ; COMPUTE-GSD needs to be optimized
      (when gsd	;; gsd is NIL when point is behind camera
	(let ((scale (* axis-length gsd)))
	  (declare (double-float scale))
	  ;;(format t "draw-object ~a ~a ~a~%" object world-position scale)
	  (bind-vector-elements (x y z) (perturbed-object-origin object)
	    (glTranslated x y z))
	  (flet ((draw-axis ()
		   (glBegin GL_LINES)
		   (glVertex3d 0.0 0.0 0.0)
		   (glVertex3d scale 0.0 0.0)
		   (glVertex3d 0.0 0.0 0.0)
		   (glVertex3d 0.0 scale 0.0)
		   (glVertex3d 0.0 0.0 0.0)
		   (glVertex3d 0.0 0.0 scale)
		   (glEnd)))
	    (glPushAttrib GL_ALL_ATTRIB_BITS)
	    ;; This next is probably wrong if 3d-to-2d-projection has no near-far parameters.
	    (glEnable GL_DEPTH_TEST)    
	    (draw-axis)			; draw it solid with depth test
	    (glDisable GL_DEPTH_TEST)
	    (glEnable GL_LINE_STIPPLE)
	    ;(glColor3d 1.0 0.0 0.0)
	    (glLineStipple 1 #x9999)
	    ;(glLineStipple 1 #x1111)
	    (draw-axis)			; draw it dashed without depth-test
	    (glPopAttrib)
	    ))))))

;;; incomplete

(defstruct-class  superellipse (gl-3d-object-mixin basic-object) ())




;;; The new code for house-object fails to load $RADIUS/sites/ft-hood-2/models/Sample_Buildings

;; #+new-house-object
(progn

;;; FIXME - incomplete 
(defstruct-class house-object (list-of-faces-object)
  ((roof-pitch :initarg :roof-pitch :accessor roof-pitch)	
   ;;(loft :initform 0)		; height of attic loft.  Normally computed from roof-pitch
   (roof-type :initform :gable :initarg :roof-type :accessor roof-type)
					; roof type is one of :flat :gable :shed :hip
   (roof-overhang :initform 0.0 :initarg :roof-overhang :accessor roof-overhang)
   )
  (:default-initargs :roof-pitch .5)  
  )



(defmethod initialize-instance :after ((object house-object) &rest args &key z-size &allow-other-keys)
  ;;(setq *house-object-args* args)
  (unless z-size
    (let ((sizes (sizes object)))
      (setf (aref sizes 2)
	    ;; (feet-to-local-units object 18.0)
	    (/ 5.4864 (or (local-units-per-meter object) 1.0)))))
  (compute-vertices object)
  (compute-topology object))
  
(defmethod foundation-face ((object house-object ))
  (with-slots (faces) object
    (aref faces 0)))

(defmethod roof-faces ((object house-object))
  (with-slot-values (faces roof-type) object
    (case roof-type
      (:gable (list (aref faces 5) (aref faces 6)))
      (:hip (list (aref faces 5) (aref faces 6) (aref faces 7) (aref faces 8)))
      ((:flat :shed) (list (aref faces 5)))
      )))

(setf (get 'house-object :short-name-string) "House")
(defmethod short-name-string ((object house-object))
  "House")


#+cme
(define-fasd-form-init-plist house-object
    (with-slots (roof-pitch roof-type roof-overhang) self
      `(:roof-pitch ',roof-pitch
	:roof-type ',roof-type
	:roof-overhang ',roof-overhang
	)))


#+cme
(defmethod compute-topology ((object house-object) &rest ignore1)
  (ignore ignore1)
  (with-slots (roof-type) object
    (case roof-type
      (:gable
       ;; vertices 12-13 are on the roof ridge
       ;; vertices 14-15 are underneath roof ridge
       (compute-topology-internal object 16 
				  '((0 3 2 1) (0 4 7 3) (2 6 5 1)
				    (4 0 1 5 14) (6 2 3 7 15)
				    (8 12 13 11) (9 10 13 12)))
       )
      (:hip
       ;; vertices 12-13 are on the roof ridge
       (compute-topology-internal object 14
				  '((0 3 2 1) (0 4 7 3) (2 6 5 1)
				    (4 0 1 5) (6 2 3 7)
				    (8 12 13 11) (9 10 13 12) (9 12 8) (10 11 13))))
      (otherwise ;;(:shed :flat)
       (compute-topology-internal object 12
				  '((0 3 2 1) (0 4 7 3) (2 6 5 1)
				    (4 0 1 5) (6 2 3 7)
				    (9 10 11 8))))
      )
    (when nil
    (loop for face in (roof-faces object)
	  do (setf (both-sides-visible-p face) t)))
    (with-slots (roof-pitch) object
      (set-roof-pitch object roof-pitch)  ))
  (set-roof-overhang-topology object)
  )


(defmethod compute-topology ((object house-object) &rest ignore1)
  (ignore ignore1)
  (with-slots (roof-type face-vertex-list) object
    (case roof-type
      (:gable
       ;; vertices 12-13 are on the roof ridge
       ;; vertices 14-15 are underneath roof ridge
       (setf face-vertex-list
	     (mapcar 'make-vertex-index-array
		     '((0 3 2 1) (0 4 7 3) (2 6 5 1)
		       (4 0 1 5 14) (6 2 3 7 15)
		       (8 12 13 11) (9 10 13 12))))
       )
      (:hip
       ;; vertices 12-13 are on the roof ridge
       (setf face-vertex-list
	     (mapcar 'make-vertex-index-array
		     '((0 3 2 1) (0 4 7 3) (2 6 5 1)
		       (4 0 1 5) (6 2 3 7)
		       (8 12 13 11) (9 10 13 12) (9 12 8) (10 11 13)))))
      (otherwise ;;(:shed :flat)
       (setf face-vertex-list
	     (mapcar 'make-vertex-index-array
		     '((0 3 2 1) (0 4 7 3) (2 6 5 1)
		       (4 0 1 5) (6 2 3 7)
		       (9 10 11 8)))))
      )
    #+old
    (loop for face in (roof-faces object)
	  do (setf (both-sides-visible-p face) t)))
    (with-slots (roof-pitch) object
      (set-roof-pitch object roof-pitch))
  ;;(set-roof-overhang-topology object)
  )

#+cme
(defmethod set-roof-overhang-topology ((object house-object))
  (let ((faces (faces object))
	(overhang (roof-overhang object)))
    (setf (list-of-edge-ids (aref faces 0)) nil)
    (flet ((set-fv (face-index elm nooverhang-vertex-index overhang-vertex-index)
	     (let ((face (aref faces face-index)))
	       (setf (nth elm (list-of-vertex-ids face))
		     (if (zerop overhang)
			 nooverhang-vertex-index
			 overhang-vertex-index))
	       ;;(setf (list-of-edge-ids face) nil)
	       )))
      (set-fv 3 3 9 5)  (set-fv 3 0 8 4)
      (set-fv 2 1 10 6) (set-fv 2 2 9 5)
      (set-fv 1 1 8 4) (set-fv 1 2 11 7)
      (set-fv 4 3 11 7) (set-fv 4 0 10 6)

      (when (eq (roof-type object) :gable)
	(set-fv 3 4 12 14) (set-fv 4 4 13 15))
      )))

(defun set-vertex-object-xyz (vert x y z)
  (setf (aref vert 0) x)
  (setf (aref vert 1) y)
  (setf (aref vert 2) z))

(defmethod compute-vertices ((object house-object))
  (with-class-slots house-object (vertex-array sizes roof-pitch roof-type roof-overhang)
      object
    ;;(setq bounding-sphere-radius (* .5 (euclidean-length x-size y-size z-size)))

    (setf vertex-array
	  (case roof-type
	    ((:hip :gable)  (make-array0 (list 16 3) :element-type 'vertex-element-type))
	    (otherwise  (make-array0 (list 12 3) :element-type 'vertex-element-type))))
    (bind-vector-elements (x-size y-size z-size) sizes
      (loop with loft = (compute-loft object)
	    with overhang-dz = (* roof-pitch roof-overhang)
	    with xs/2 = (* .5 x-size)
	    with ys/2 = (* .5 y-size)
	    with zs/2 = (* .5 z-size)
	    with xw/2 = (- xs/2 roof-overhang)
	    with yw/2 = (- ys/2 roof-overhang)
	    with zw/2 = (- zs/2 overhang-dz)
	    with xr/2 = xs/2
	    with yr/2 = ys/2
	    with y-indent = (if (eq roof-type :hip) xw/2 0.0)
	    with hip-y = (- yr/2 y-indent)
	    with wz = (- zs/2 (- loft overhang-dz))
	    with rz = (- zs/2 loft)
	    for vertex-num from 0 below (array-dimension vertex-array 0)
	    do (multiple-value-bind (x y z)
		   (case roof-type
		     ((:hip :gable)
		      (case vertex-num
			(0 (values (- xw/2) (- yw/2) (- zs/2)))
			(1 (values xw/2 (- yw/2) (- zs/2)))
			(2 (values xw/2 yw/2 (- zs/2)))
			(3 (values (- xw/2) yw/2 (- zs/2)))
			(4 (values (- xw/2) (- yw/2) wz))
			(5 (values xw/2 (- yw/2) wz))
			(6 (values xw/2 yw/2 wz))
			(7 (values (- xw/2) yw/2 wz))
			(8 (values (- xr/2) (- yr/2) rz))
			(9 (values xr/2 (- yr/2) rz))
			(10 (values xr/2 yr/2 rz))
			(11 (values (- xr/2) yr/2 rz))
			(12 (values 0.0 (- hip-y) zs/2))
			(13 (values 0.0 hip-y zs/2))
			(14 (values 0.0 (- yw/2) zs/2))
			(15 (values 0.0 yw/2 zs/2))
			))
		     (otherwise		;(:flat :shed)
		      (case vertex-num
			(0 (values (- xw/2) (- yw/2) (- zs/2)))
			(1 (values xw/2 (- yw/2) (- zs/2)))
			(2 (values xw/2 yw/2 (- zs/2)))
			(3 (values (- xw/2) yw/2 (- zs/2)))
			(4 (values (- xw/2) (- yw/2) wz))
			(5 (values xw/2 (- yw/2) zw/2))
			(6 (values xw/2 yw/2 zw/2))
			(7 (values (- xw/2) yw/2 wz))
			(8 (values (- xr/2) (- yr/2) rz))
			(9 (values xr/2 (- yr/2) zs/2))
			(10 (values xr/2 yr/2 zs/2))
			(11 (values (- xr/2) yr/2 rz))
			))
		     )
		 (setf (aref vertex-array vertex-num 0) x)
		 (setf (aref vertex-array vertex-num 1) y)
		 (setf (aref vertex-array vertex-num 2) z))))

    ;;(set-vertex-object-xyz center-vertex 0.0 0.0 0.0)
    ))




(defmethod compute-loft ((object house-object))
  (with-class-slots house-object (sizes roof-pitch roof-type ) object
    (bind-vector-elements (x-size y-size z-size) sizes
      (min z-size (if (eq roof-type :shed)
		      (* x-size roof-pitch)
		      (* .5 x-size roof-pitch))))))

(defmethod set-roof-type ((object house-object ) new-roof-type)
  (with-slots (roof-type vertices faces) object
    (setf roof-type new-roof-type)
    (setq vertices nil faces nil) ; flush old state
    (compute-topology object)
    (compute-vertices object)))


(defmethod set-roof-pitch ((object house-object) new-pitch)
  (with-slots (roof-type roof-pitch sizes) object
    (unless (>= new-pitch 0.0) (setq new-pitch 0.0))
    (let* ((old-pitch roof-pitch)
	   (delta-z (* (- new-pitch old-pitch)
		       (if (eq roof-type :shed)
			   (abs (aref sizes 0))
			   (/ (abs (aref sizes 0)) 2.0)))))
      (setq roof-pitch new-pitch)
      (setf (aref sizes 2) (+ (aref sizes 2) delta-z)))))

(defmethod set-roof-overhang ((object house-object) new-overhang)
  (with-slots (roof-overhang) object
    (setq roof-overhang new-overhang)
    (compute-vertices object)
    (set-roof-overhang-topology object)))


) ; end progn




#|	

(let* ((view (top-view))
       (obj (make-instance 'axis-object
			   :world (3d-world view)
			   :object-to-world-transform
			   (make-4x4-coordinate-transform
			    (make-object-to-parent-matrix (cv 256.9 4632.1 6015.4)))))
       (obj2 (make-instance '3d-crosshair-object
			    :world (3d-world view)
			    :object-to-world-transform
			    (make-4x4-coordinate-transform
			     (make-object-to-parent-matrix (cv 260.0 4732.1 6015.4)))))
       (os (gui::make-object-set (list obj obj2)
				 :world (3d-world view)
				 :immediate-render-p t
				 )))
  (push os (gui::object-sets view))
  )
(selected-point-world-position (gui::selected-object))

(let* ((gnd-pos (selected-point-world-position (gui::selected-object)))
       (view (top-view))
       
       (obj (make-instance 'cme::axis-object
			   :world (3d-world view)
			   :object-to-world-transform
			   (make-4x4-coordinate-transform
			    (make-object-to-parent-matrix gnd-pos))))
       (obj2 (make-instance 'cme::3d-crosshair-object
			    :world (3d-world view)
			    :object-to-world-transform
			    (make-4x4-coordinate-transform
			     (make-object-to-parent-matrix gnd-pos))))
       (os (gui::make-object-set (list obj obj2)
				 :world (3d-world view)
				 :immediate-render-p t
				 )))
  (push os (gui::object-sets view))
  )

(pop (gui::object-sets (top-view)))
(gui::clear-view-stack (gui::selected-window))
(gui::opengl-reset-state)
|#

(defparameter *light-stipple* '((0 1 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0)))

(st::add-system-initialization :gl-objects
			       '(progn
				 (obj::set-object-class-graphics-attributes
				  'obj::house-object :color-vector (cv 0.8 1.0 0.0 0.8))
				 (obj::set-object-class-graphics-attributes
				  'obj::extruded-object :color-vector (cv 0.8 1.0 0.0 0.8))
				 (obj::set-object-class-graphics-attributes
				  'obj::cube-object :color-vector (cv 0.8 1.0 0.0 0.8))
				 (obj::set-object-class-graphics-attributes
				  'cylinder :color-vector (cv 0.8 1.0 0.0 0.8))))
