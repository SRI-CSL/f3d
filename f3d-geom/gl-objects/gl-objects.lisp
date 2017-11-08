(in-package :obj)

#|

This file contains the class definitions, instantiation and drawing methods
for many spatial object classes.

Various interaction methods are defined elsewhere.

|#

#| 
LHQ Sun Sep  5 2004  - Planned changes to GL-OBJECT API:

Everything in this file SHOULD be independent on details of the GUI
or other subsystems outside of those specified by the the GL-OBJECTS
subsystem definition in sysdef-gl-objects.lisp

There are highlight methods and a couple of gl-object classes that violate the
above note and should either be rewritten or be moved to a different file.  Most
of these problems could be resolved by defining a GL-VIEW class that BASIC-VIEW
would inherit from.

Some of the slots and methods related to GL-VIEW that are needed are:

           METHOD: (object-to-view-transform object view)
              METHOD: (3d-to-2d-projection view)
           SLOT:  (2d-to-window-transform view)


|#

#|
LHQ Wed Mar  3 2004

While attempting to get FREEDIUS to run in cross machine client/server mode
I discovered a serious mistake in my usage of vertex-arrays in OpenGL.
glVertexPointer glNormalPointer ... must not appear in display lists,
since the arrays are store on the client side, not the server side.
This makes the use of glVertexPointer pretty useless in the cross machine
mode.  It would be a real pain to group the vertex-arrays of
all objects in a given display-list together so that one could make a single
global call to glVertexPointer before glCallList.

|#

;;(defvar *allow-vertex-arrays-in-display-lists* #+macosx nil)

;;; I am not sure that vertex arrays really help much for the small number of
;;; vertices involved in these models.  Vertex-arrays are likely to be of value
;;; for terrain meshes, but not here.
(defvar *allow-vertex-arrays-in-display-lists* nil)

(defun allow-vertex-arrays-in-display-lists ()
  (if (boundp '*allow-vertex-arrays-in-display-lists*)
      *allow-vertex-arrays-in-display-lists*
      (setq *allow-vertex-arrays-in-display-lists* (gl-is-direct-p))))


#|
(setq *allow-vertex-arrays-in-display-lists* t)
(setq *allow-vertex-arrays-in-display-lists* nil)
(progn *allow-vertex-arrays-in-display-lists*)
|#




(defmethod draw-vertices ((object gl-object) &optional (point-size 1.0))
  (declare (double-float point-size))
  (let* ((verts (vertex-array object)))
    (declare (type (or null vertex-array-type) verts))
    (when verts
      (with-object-transform object
	(glPushName 0)
	(glPointSize point-size)
	(loop for i of-type fixnum from 0 below (array-dimension verts 0)
	      do (glLoadName i)
		 (glBegin GL_POINTS)
		 (glVertex3d (aref verts i 0) (aref verts i 1) (aref verts i 2))
		 (glEnd))
	  
	(glPopName)))))

#+never ;; experiment for use with crosshair-object
(defmethod draw-vertices ((object gl-object))
  (let* ((verts (vertex-array object)))
    (with-object-transform object
      (glPushName 0)
      (glPointSize 1.0)
      (if verts
	  (let ((verts verts))
	    (declare (type (or null vertex-array-type) verts))
	    (loop for i of-type fixnum from 0 below (array-dimension verts 0)
		  do (glLoadName i)
		     (glBegin GL_POINTS)
		     (glVertex3d (aref verts i 0) (aref verts i 1) (aref verts i 2))
		     (glEnd)))
	  (progn (glLoadName 0)
		 (glBegin GL_POINTS)
		 (glVertex3d 0.0 0.0 0.0)
		 (glEnd))
	  )
	  
      (glPopName))))

;;; This is called from process-multiple-selection-hits selection with (glRenderMode GL_SELECT)
(defmethod draw-fragments ((object gl-object))
  ;;(format t "draw-fragments ~a~%" object)
  (draw-vertices object)
  )

(defmethod draw-fragments ((object basic-gl-object))
 )

(defvar *highlight-vertex-identity-matrix* (make-4x4-identity-matrix))

;;; ***************************** DRAW-HOLLOW-BOX DRAW-CROSSHAIR  *****************************

;;; Point must be in view 2d-coordinates.  Not sure why this choice was made.
;;; box-radius in pixels. 
(defun draw-hollow-box (point &optional (box-radius 4.0))
  (declare (type double-float box-radius))
  (bind-vector-elements (x y) point
    (let ((x0 (- x box-radius)) (y0 (- y box-radius))
	  (x1 (+ x box-radius)) (y1 (+ y box-radius))
	  (z 0.0))
      (declare (type double-float x0 x1 y0 y1))
      (with-gl-window-transform
	(glPushAttrib GL_LINE_BIT)
	(glLineWidth 1.0)
	(glBegin GL_LINE_LOOP)
	(glVertex3d x0 y0 z) (glVertex3d x1 y0 z) (glVertex3d x1 y1 z) (glVertex3d x0 y1 z) 
	(glEnd)
	(glPopAttrib)))))

;;; Point must be in view (window) 2d-coordinates.  Not sure why this choice was made.
;;; inside-size and outside-size in pixels.
(defun draw-crosshair (point inside-size outside-size)
  (declare (type double-float inside-size outside-size))
  (bind-vector-elements (x y) point
    (with-gl-window-transform
      (glPushAttrib GL_LINE_BIT)
      (glLineWidth 1.0)
      (glBegin GL_LINES)
      (glVertex2d (- x outside-size) y)
      (glVertex2d (- x inside-size) y)
      (glVertex2d (+ x inside-size) y)
      (glVertex2d (+ x outside-size) y)
      (glVertex2d x (- y outside-size))
      (glVertex2d x (- y inside-size))
      (glVertex2d x (+ y inside-size))
      (glVertex2d x (+ y outside-size))
      (glEnd)
      (glPopAttrib))))

(defun draw-crosshair-x (point inside-size outside-size)
  (declare (type double-float inside-size outside-size))
  (bind-vector-elements (x y) point
    (with-gl-window-transform
      (glPushAttrib GL_LINE_BIT)
      (glLineWidth 1.0)
      (glBegin GL_LINES)
      (glVertex2d (- x outside-size) (- y outside-size))
      (glVertex2d (- x inside-size) (- y inside-size))
      (glVertex2d (+ x inside-size) (+ y inside-size))
      (glVertex2d (+ x outside-size) (+ y outside-size))
      (glVertex2d (- x outside-size) (+ y outside-size))
      (glVertex2d (- x inside-size) (+ y inside-size))
      (glVertex2d (+ x inside-size) (- y inside-size))
      (glVertex2d (+ x outside-size) (- y outside-size))
      (glEnd)
      (glPopAttrib))))

(defun draw-dotted-line (from-pt to-pt)
  (progn ;with-gl-window-transform
    (glPushAttrib GL_LINE_BIT)
    (glLineWidth 1.0) 
    (glEnable GL_LINE_STIPPLE)
    (glLineStipple 1 #x1111)
    (glBegin GL_LINES)
    (glVertex3dv from-pt)
    (glVertex3dv to-pt)
    (glEnd)
    (glDisable GL_LINE_STIPPLE)       
    (glPopAttrib)))

;;; ********************** HIGHLIGHT-POINT HIGHLIGHT-VERTEX HIGHLIGHT-ARC *****************************

(defmethod highlight-point ((object basic-gl-object) point view &optional (box-radius 4.0))
  (declare (type double-float box-radius))
  (draw-hollow-box (transform-vector (transforms::object-to-view-transform object view) point)
		   box-radius))

;;; FIXME: needs (2D-TO-WINDOW-TRANSFORM VIEW)
(defmethod highlight-point ((object gl-2d-object-mixin) point view &optional (box-radius 4.0))
  (declare (type double-float box-radius))
  (let ((xf (transforms::object-to-view-transform object view)))
    (draw-hollow-box (transform-vector xf point)
		     box-radius)))

(defmethod highlight-vertex ((object gl-object) vertex-id view &optional (box-radius 4.0))
  (declare (type double-float box-radius))
  (declare (type fixnum vertex-id))
  ;; (vertex-array ...) is not used here since the TRANSFORM-VECTOR is used rather then OpenGL xform.
  (let* ((verts (%vertex-array object)))
    (declare (type vertex-array-type verts))
    (when (< vertex-id (array-dimension verts 0))
      (highlight-point object 
		       (vertex-array-vertex verts vertex-id)
		       view
		       box-radius))))

(defmethod highlight-arc ((object gl-object) arc-index)
  (let* ((verts (vertex-array object)) ; here we need the perturbed vertices for non-4x4-projections
	 (i (mod (1- arc-index) (array-dimension verts 0)))
	 (j arc-index))
    (declare (type fixnum i j))
    (declare (type vertex-array-type verts))
    (glBegin GL_LINES)
    (glVertex3d (aref verts i 0) (aref verts i 1) (aref verts i 2))
    (glVertex3d (aref verts j 0) (aref verts j 1) (aref verts j 2))
    (glEnd)
    ))

;;; *************************  OBJECT FRAGMENTS  *************************

(defstruct-class objfrag (base-struct-class) ())

(defstruct-class object-vertex (objfrag)
  ((object :initform nil :initarg :object :accessor object)
   (vertex-id :initform nil :initarg :vertex-id :accessor vertex-id)))

(defmethod highlight ((frag-descr object-vertex) view)
  (with-class-slot-values object-vertex (object vertex-id) frag-descr
    (highlight-vertex object vertex-id view)))

;;; Returns the (unperturbed) position of an OBJECT-VERTEX in OBJECT local coordinates.
(defmethod fragment-position ((frag-descr object-vertex))
  (with-class-slot-values object-vertex (object vertex-id) frag-descr
    (let ((vertex-array (%vertex-array object))) 
      (if (and vertex-array (< vertex-id (array-dimension vertex-array 0)))
	  (object-vertex vertex-array vertex-id)
	  (cv 0.0 0.0 0.0)
	  ))))

(defmethod (setf fragment-position) (position (frag-descr object-vertex))
  (with-class-slot-values object-vertex (object vertex-id) frag-descr
    ;; FIXME -- must flush caches?
    (setf (object-vertex (%vertex-array object) vertex-id) position)))

(defstruct-class object-arc (objfrag)
  ((object :initform nil :initarg :object :accessor object)
   (start-vertex-id :initform nil :initarg :start-vertex-id)
   (end-vertex-id :initform nil :initarg :end-vertex-id)
   (pick-percent :initform nil :initarg :pick-percent)
   ))

;;; (arclength (cadar (gui::selected-objects)))
(defmethod arclength ((frag-descr object-arc))
  (with-class-slot-values object-arc (object start-vertex-id end-vertex-id ) frag-descr
    (let ((vertex-array (%vertex-array object)))
      (math::vector-to-vector-distance (object-vertex vertex-array start-vertex-id)
				       (object-vertex vertex-array end-vertex-id)))))

(defmethod highlight ((frag-descr object-arc) view)
  ;;(declare (ignore view))
  (with-class-slot-values object-arc (object  end-vertex-id ) frag-descr
    (highlight-arc object end-vertex-id)
    (highlight-point object (fragment-position frag-descr) view 2.0)
    ))  


#||
;;; Consider this alternative, which uses both start and end vertex IDs, and
;;; therefore does not assume a "curve" topology.  This is useful for
;;; drawing general graph structures:
;;; - CC 2008/03/03


(defmethod highlight ((frag-descr object-arc) (view gui::canvas-view))
  (with-class-slot-values object-arc (object start-vertex-id end-vertex-id ) frag-descr
    ;; Need to redefine highlight-arc for the extra index arg:
    (highlight-arc2 object start-vertex-id end-vertex-id)
    (highlight-point object (fragment-position frag-descr) view 2.0)
    ))  
||#


(defmethod fragment-position ((frag-descr object-arc))
  (with-class-slot-values object-arc (object start-vertex-id end-vertex-id pick-percent) frag-descr
    (let ((vertex-array (%vertex-array object)))
      (vector-linear-combine (- 1.0 pick-percent) (object-vertex vertex-array start-vertex-id)
			     pick-percent (object-vertex vertex-array end-vertex-id)))))
  
;;; This base method makes an OBJECT-VERTEX fragment.
;;; This throws away all but the first elements of frags+z
(defmethod make-fragment-descr ((object basic-gl-object) view x y frags+z)
  (declare (ignore view x y))
  ;; default is the indicated vertex.
  (let ((vertex-id (caar frags+z)))
    (make-instance 'object-vertex :object object :vertex-id vertex-id)))

(defmethod object ((objfrag list))
  (car objfrag))

(defun fragment (objfrag)
  (if (consp objfrag)
      (cadr objfrag)
      objfrag))

(defmethod project-pick-to-object ((object gl-3d-object-mixin) pick-position view)
  (let* ((projection (3d-to-2d-projection view))
	 (surrogate-projection (surrogate-projection projection))
	 (w2o (inverse-transform (object-to-world-transform object)))
	 (2dpt (inverse-transform-vector (2d-to-window-transform view) pick-position))
	 (3dpt (if (null surrogate-projection)
		   (inverse-transform-vector projection 2dpt) ; projection must be a 4x4-projection
		   ;;
		   (let* ((perturbed-3dpt (inverse-transform-vector surrogate-projection 2dpt))
			  (3dpt (transforms::surrogate-projection-unperturb-point 
				 surrogate-projection projection perturbed-3dpt)))
		     3dpt)))
	 (o3dpt (transform-vector w2o 3dpt))) ; compute object coordinates
    o3dpt))
 
(defmethod project-pick-to-object ((object gl-2d-object-mixin) pick-position view)
  (inverse-transform-vector (transforms::object-to-view-transform object view) pick-position))

;;; x and y are window coordinates
(defun make-arc-descr (view x y z object start-vertex-id end-vertex-id)
  (let* ((vertex-array (%vertex-array object))
	 (pick-pt (project-pick-to-object object (cv x y z) view))
	 (start-pt (object-vertex vertex-array start-vertex-id))
	 (end-pt (object-vertex vertex-array end-vertex-id))
	 (dist-start-to-pick-pt (vector-to-vector-distance start-pt pick-pt))
	 (dist-start-to-end (vector-to-vector-distance start-pt end-pt))
	 (pick-percent (/ dist-start-to-pick-pt dist-start-to-end)))
    (declare (type fixnum end-vertex-id start-vertex-id))
    (declare (type double-float dist-start-to-pick-pt dist-start-to-end pick-percent))
    (make-instance 'object-arc :object object
		   :start-vertex-id start-vertex-id
		   :end-vertex-id end-vertex-id
		   :pick-percent pick-percent)))


(declaim (special *enable-object-labels* *enable-highlight-object-labels* 
		  *default-highlight-label-graphics-style*))

(defparameter *label-highlighting-offset-format-string* "   ~a")
;(defparameter *label-highlighting-offset-format-string* "~a")

;;; LHQ Sun Sep 23 2007:  The label drawn with glDrawString-with-color is positioned differently
;;; from that of DRAW-OBJECT-LABEL.  Why?  Why was *label-highlighting-offset-format-string* introduced?
(defmethod highlight-object ((object basic-gl-object) frag-descr view 
			     object-graphics-style highlight-graphics-style selectids)
  (declare (ignorable object-graphics-style highlight-graphics-style))
  (if frag-descr
      (unwind-protect
	   (progn
	     (glPushAttrib GL_ENABLE_BIT)
	     ;; FIXME -- something broken here
	     (draw-object-around object view  object-graphics-style selectids)
	     (with-gl-object-drawing (object view highlight-graphics-style)
	       (glDisable GL_POLYGON_STIPPLE)
	       (highlight frag-descr view)
	       (when *enable-highlight-object-labels*
		 (let ((highlight-graphics-style *default-highlight-label-graphics-style*))
		   (glDrawString-with-color object 
					    (format nil *label-highlighting-offset-format-string*
						    (or (name object) "<unnamed>")) ; add spaces to offset label
					    (fragment-position frag-descr)
					    (font highlight-graphics-style)
					    (color-vector highlight-graphics-style))))
	       
	       ))
	(glPopAttrib))

      (draw-object-around object view highlight-graphics-style selectids)))


;;; **************************  BASIC-CURVE  **************************

(defstruct-class basic-curve (gl-object)
  ((closed-p :initform nil :initarg :closed-p :accessor closed-p)))

(declaim (special *object-fragment-start-index*))

(defmethod make-fragment-descr ((object basic-curve) view x y frags+z)
  ;; First look for a vertex
  (loop with nverts fixnum = (array-dimension (%vertex-array object) 0)
	for frag+z in frags+z
	for frag = (car frag+z)
	when (<= frag nverts)		; found a vertex
	  return (make-instance 'object-vertex :object object :vertex-id frag)
	finally	
     ;; It must have been an arc
     (return (destructuring-bind (fragment-id pick-z) (car frags+z)
	       (let ((arc-end-vertex-id (- *object-fragment-start-index* fragment-id)))
		 (make-arc-descr view x y pick-z object
				 (1- arc-end-vertex-id) (mod arc-end-vertex-id nverts)))))))

(defparameter *object-fragment-start-index* 1000000)

(defmethod draw-fragments ((object basic-curve))
  (draw-vertices object) ; this does glLoadName for each vertex
  (let* ((verts (vertex-array object))
	 (nverts (array-dimension verts 0))
	 (closed-p  (closed-p object)))
    
    (declare (type fixnum nverts))
    (declare (type vertex-array-type verts))
    #+never
    (when (and (eq object *obj*) (eq gui::*current-view* (gui::selected-view )))
      (format t "draw-fragments basic-curve ~a~%" (list object gui::*current-view* verts *transform-vertices-projection*)))
    (unless *draw-debug*
      (with-object-transform object
	(glPushName 0)
	(loop with nsegs = (if closed-p
			       nverts
			       (1- nverts))
	      for i of-type fixnum from 0 below nsegs
	      for j of-type fixnum = (mod (1+ i) nverts)
	      do (glLoadName (- *object-fragment-start-index* 1 i))
		 (glBegin GL_LINES)
		 (glVertex3d (aref verts i 0) (aref verts i 1) (aref verts i 2))
		 (glVertex3d (aref verts j 0) (aref verts j 1) (aref verts j 2))
		 (glEnd))	  
	(glPopName)))))

;;; this appears to be unused
(defmethod highlight-fragment ((object basic-curve) fragment-id view)
  (let ((nverts (array-dimension (vertex-array object) 0)))
    (when (and fragment-id (>= fragment-id 0))
      (if (< fragment-id nverts)
	  (highlight-vertex object fragment-id view)
	  (highlight-arc object (- *object-fragment-start-index* fragment-id))))))

(defparameter *default-ribbon-width* 10.0)

(defmethod initialize-instance :after ((object basic-curve) &key  &allow-other-keys)
  (with-class-slots basic-curve (vertex-array) object
    (unless vertex-array
      (setf vertex-array
	    (make-array (list 2 (vertex-array-elements-per-vertex object))
			:element-type 'vertex-element-type
			:initial-element 0.0))
      ;; ugly hack for ribbons
      (when (= (vertex-array-elements-per-vertex object) 4) 
	(loop for i from 0 below (array-dimension vertex-array 0)
	      do (setf (aref vertex-array i 3) *default-ribbon-width*)))
      )))
  


;;; Both 2d-curve and 3d-curve inherit from this class.
(defstruct-class curve (basic-curve)
    ())

(defmethod draw-closed-curve ((object curve))
  (let* ((verts (vertex-array object))
	 (nverts (array-dimension verts 0)))
    (declare (type vertex-array-type verts))
    (declare (type fixnum nverts))
    (unless *draw-debug*
      (with-object-transform object
	;; Draw the boundary of the closed curve
	(glBegin GL_LINE_LOOP)
	(loop for i of-type fixnum from 0 below nverts
	      do (glVertex3d (aref verts i 0) (aref verts i 1) (aref verts i 2)))
	(glEnd)
	;; Draw the interior of the closed curve
	(progn
	  (glPushAttrib (logior GL_POLYGON_BIT GL_ENABLE_BIT))
	  ;; NO GUARANTEE OF CW VS CWW VERTEX ORDERING.
	  (glDisable GL_CULL_FACE)
	  (when *GL-ENABLE-STIPPLES*
	    (glEnable GL_POLYGON_STIPPLE))
	  (glPolygonMode GL_BACK GL_FILL)
	  (glPolygonMode GL_FRONT GL_FILL)
	  ;; Nothing happens here unless there is a non-zero stipple pattern
	  (draw-polygon-tesselated verts nil)
	  (glPopAttrib)
	  )))))


;;;(defun gl-draw-curve (verts nverts ncomps)
;;;  #+macosx
;;;  (progn
;;;    (glBegin GL_LINE_STRIP)
;;;    (loop for i fixnum from 0 below nverts
;;;          do (glVertex3d (aref verts i 0)
;;;                         (aref verts i 1)
;;;                         (aref verts i 2)))
;;;    (glEnd))
;;;  #-macosx
;;;  (progn
;;;    (glVertexPointer 3 *GL-VERTEX-TYPE*
;;;                     (* *GL-VERTEX-bytes-per-element* ncomps)
;;;                     verts)
;;;    (glDrawArrays GL_LINE_STRIP 0 nverts)))

;;; no callers to this any more
(defun gl-draw-curve (verts nverts)
  (glBegin GL_LINE_STRIP)
  (loop for i fixnum from 0 below nverts
	do (glVertex3d (aref verts i 0)
		       (aref verts i 1)
		       (aref verts i 2)))
  (glEnd))


(defmethod draw-object ((object curve) view)
  (declare (ignorable view))
  ;; (format t "draw-object ~a~%" object)
  (if (closed-p object)
      (draw-closed-curve object)
      (let* ((verts (vertex-array object))
	     (nverts (array-dimension verts 0))
	     ;(ncomps (array-dimension verts 1))
	     )
	(declare (type vertex-array-type verts))
	(declare (type fixnum nverts))
	(unless *draw-debug*
	  (with-object-transform object
	    ;;(glPushAttrib (logior GL_ENABLE_BIT ))
	    ;;(glPushAttrib (logior GL_ENABLE_BIT GL_CURRENT_BIT GL_COLOR_BUFFER_BIT GL_POLYGON))
	    (glPushAttrib GL_LIGHTING_BIT)
	    (glDisable GL_LIGHTING)  ; have no face normals -- turn off lighting

	    ;; Perhaps a bug in the Mac OS X GL implementation - the
	    ;; glDrawArrays version doesn't seem to work in Mac OS X.
	    ;;(draw_polygon verts nil nverts nil 0)
	    ;;(gl-draw-curve verts nverts)
	    ;; Looks like it's faster:
	    (draw_line_strip verts nverts)
	    (glPopAttrib))))))

#|
void FREEDIUS_GLOBAL(draw_line_strip) (double verts[][3], int n)
{
  glBegin(GL_LINE_STRIP);
  for (int i = 0; i<n; i++) 
      glVertex3dv(verts[i]);  // it is difficult to index the vertex-array in lisp
  glEnd();
}
|#

;;; This is currently only meaningful as a 3d-object.
(defstruct-class ribbon (basic-curve)
    ())

(defmethod fasd-form-properties-list-do-not-dump :around ((obj ribbon))
  (append (call-next-method)
	  `(:ribbon-edge-vertices )))

(defmethod quad-strip-indices ((object ribbon))
  (let* ((vertices (vertex-array object))
	 (n (* 2 (array-dimension vertices 0)))
	 (arr (make-vertex-index-array n)))
    (declare (type vertex-index-array-type arr))
    (loop for i fixnum from 0 below n
	  do (setf (aref arr i) i))
    arr))

(defmethod update-object-after ((object ribbon))
  (setf (get-prop object :ribbon-edge-vertices) nil))


#+old
(defmethod ribbon-edge-vertices ((object ribbon))
  (maybe-transform-vertex-array
   object
   (or  (get-prop object :ribbon-edge-vertices)
	(setf (get-prop object :ribbon-edge-vertices)
	      (let* ((verts (%vertex-array object))
		     (nverts (array-dimension verts 0))
		     (edge-verts (make-array0 (list (* 2 nverts) 3)
					      :element-type 'vertex-element-type))
		     (object-to-world-matrix (object-to-world-transform-matrix object)))
		(compute_ribbon_edge_vertices object-to-world-matrix verts nverts edge-verts)
		edge-verts)))
   nil))



;;; Alternative method: this should allow the display machinery to
;;; work unmolested (avoid the odd glPopMatrix).  Compute edge
;;; vertices in world coordinates, but transform back into object
;;; coordinates and display as normal (-CC 3/2/06)
(defun revert-edge-verts-to-object (verts xf)
  (loop with r = (cv 0.0 0.0 0.0)
	for i from 0 below (array-dimension verts 0)
	do (inverse-transform-vector
		   xf (vertex-array-vertex verts i) r)
	   (setf (aref verts i 0) (aref r 0))
	   (setf (aref verts i 1) (aref r 1))
	   (setf (aref verts i 2) (aref r 2)))
  verts)

(defmethod ribbon-edge-vertices ((object ribbon))
  (maybe-transform-vertex-array
   object
   (or  (get-prop object :ribbon-edge-vertices)
	(setf (get-prop object :ribbon-edge-vertices)
	      (let* ((verts (%vertex-array object))
		     (nverts (array-dimension verts 0))
		     (edge-verts (make-array0 (list (* 2 nverts) 3)
					      :element-type 'vertex-element-type))
		     (object-to-world-matrix (object-to-world-transform-matrix object)))
		(compute_ribbon_edge_vertices object-to-world-matrix verts nverts edge-verts)
		;; Ok, at this time we've stored the ribbon edge vertices in the
		;; array, but they're in world coordinates.  Sweep through and bring
		;; them back into object coordinates:
		(revert-edge-verts-to-object edge-verts (object-to-world-transform object)))))
   nil))


(defmethod compute-face-normals ((object ribbon))
  (let* ((centerline-verts (vertex-array object))
	 (nfaces (1- (array-dimension centerline-verts 0)))
	 (face-normals (make-array0 (list nfaces 3)
				   :element-type 'vertex-element-type))
	 )
    (declare (type vertex-array-type face-normals))
    (loop for i fixnum from 0 below nfaces
	  do (setf (aref face-normals i 2) 1.0))
    face-normals))

(defvar *object-feedback-mode* nil)

(declaim (special *gl-shading-enabled*))

;;; This method is NON-STANDARD.  It pops the GL_MODELVIEW matrix stack in order to
;;; remove the object-to-world-transform.  This is because RIBBON-EDGE-VERTICES
;;; are stored in 3d-world coordinates rather than object coordinates.

;;; It isn't clear that vertex-array usage here buys enough to justify the
;;; extra code complexity.

;;; New symptom: If there is an intervening coordinate frame to which
;;; the ribbon is anchored (e.g., the ribbon's parent is one or more
;;; steps removed from the LVCS), then this fails.  The ribbon is not
;;; drawn in the correct location:

#+old
(defmethod draw-object ((object ribbon) view)
  (declare (ignorable view))
  (unless *draw-debug*
    (let* ((nverts (array-dimension (%vertex-array object) 0))
	   (nfaces (1- nverts)))
      (unwind-protect
	   (cond #+no-longer-used
		 (*object-feedback-mode* 				  
		  (let ((verts (vertex-array object))) ; centerline vertices
		    (with-object-transform object
		      (draw_line_strip verts nverts)))
		  (glMatrixMode GL_MODELVIEW) (glPopMatrix))
		 ;;
		 ((not *gl-shading-enabled*) ; FIXME - drawing-parameter
		  (let* ((edge-verts (ribbon-edge-vertices object)))
		    ;; ribbon-edge-vertices takes object-to-world-matrix into account in
		    ;; order to compute face normals and properly level the road
		    ;; left-to-right.  THIS IS DONE INCORRECTLY -- Should change
		    ;; object-to-world-transform to be identity-transform, and store the
		    ;; actual object-to-world-transform elsewhere.
		    (glMatrixMode GL_MODELVIEW) (glPopMatrix)
		    (glPushAttrib GL_POLYGON_STIPPLE)
		    (glEnable GL_POLYGON_STIPPLE)
		    (cond ((allow-vertex-arrays-in-display-lists)
			   (let ((n-edge-verts (array-dimension edge-verts 0)))
			     (glVertexPointer 3 *GL-VERTEX-TYPE*
					      (* *gl-vertex-bytes-per-element*
						 (array-dimension edge-verts 1))
					      edge-verts)
			     (glDrawArrays GL_QUAD_STRIP 0 n-edge-verts)))
			
			  (t #+debug
			     (when (and (eq object *obj*) (eq gui::*current-view* *view*))
			       (setq *foo* (list edge-verts nfaces)))
			     (glDrawquad_strip edge-verts nfaces)))
		    (glPopAttrib)))
		 ;;
		 (t (let ((edge-verts (ribbon-edge-vertices object))
			  (face-normals (compute-face-normals object)))
		      (glPushAttrib GL_POLYGON_STIPPLE)
		      (glEnable GL_POLYGON_STIPPLE)
		      (glMatrixMode GL_MODELVIEW) (glPopMatrix)
		      ;; defined in gl/glffi.C
		      (glDrawquad-strip-with-face-normals edge-verts face-normals nfaces)
		      (glPopAttrib ))))
	;; cleanup form
	(progn (glMatrixMode GL_MODELVIEW) (glPushMatrix)) ; resync stack
	))))


;;; Is this right?  I think so.  Seems to work even in displaced video
;;; views.  Eliminate the need to perform glPop- and glPushMatrix -
;;; edge vertices are now in object coordinates.  Some more work might
;;; be needed to ensure normals are ok, but at least the roads show up
;;; in the right place (-CC 3/2/06)
;;;
(defmethod draw-object ((object ribbon) view) ;; (view gui::video-view))
  (declare (ignorable view))
  (unless *draw-debug*
    (let* ((nverts (array-dimension (%vertex-array object) 0))
	   (nfaces (1- nverts)))
      (when (plusp nverts)
	(cond ((not *gl-shading-enabled*) ; FIXME - drawing-parameter
	       (let* ((edge-verts (ribbon-edge-vertices object)))
		 ;; ribbon-edge-vertices takes object-to-world-matrix into account in
		 ;; order to compute face normals and properly level the road
		 ;; left-to-right.  THIS IS DONE INCORRECTLY -- Should change
		 ;; object-to-world-transform to be identity-transform, and store the
		 ;; actual object-to-world-transform elsewhere.

		 (glPushAttrib GL_POLYGON_STIPPLE)
		 (glEnable GL_POLYGON_STIPPLE)
		 (cond ((allow-vertex-arrays-in-display-lists)
			(let ((n-edge-verts (array-dimension edge-verts 0)))
			  (glVertexPointer 3 *GL-VERTEX-TYPE*
					   (* *gl-vertex-bytes-per-element*
					      (array-dimension edge-verts 1))
					   edge-verts)
			  (glDrawArrays GL_QUAD_STRIP 0 n-edge-verts)))
		       
		       (t #+debug
			  (when (and (eq object *obj*) (eq gui::*current-view* *view*))
			    (setq *foo* (list edge-verts nfaces)))
			  (glDrawquad_strip edge-verts nfaces)))
		 (glPopAttrib)))
	      ;;
	      (t (let ((edge-verts (ribbon-edge-vertices object))
		       (face-normals (compute-face-normals object)))
		   (glPushAttrib GL_POLYGON_STIPPLE)
		   (glEnable GL_POLYGON_STIPPLE)
		   (glDrawquad-strip-with-face-normals edge-verts face-normals nfaces)
		   (glPopAttrib )))))
      )))

#|
(setf (slot-value (graphics-style (caar (gui::selected-objects))) 'stipple) gl::*stipple0*)

(describe (caar (gui::selected-objects)))
(describe (object-graphics-style (caar (gui::selected-objects)) (top-view))))
(describe (graphics-style (caar (gui::selected-objects))))
|#



#| ;; unfinished

(defparameter *default-3d-network-initial-array-length* 10)
(defparameter *3d-network-select-distance* 1.0)
(defparameter *3d-network-default-move-mode* :vertex-uv-on-dtm )
(defparameter *draw-entire-network-unless-open-for-vertex-modification* t)
(defparameter *merge-distance-threshold* 8)

(defstruct-class basic-network (basic-curve)
  ((arcs :initform (make-array *default-3d-network-initial-array-length* :initial-element nil
			       :fill-pointer 0 :adjustable t)
	 :initarg :arcs :accessor arcs)
   (vertex-arc-ht :initform (make-hash-table :test 'eql)
		  :accessor vertex-arc-ht)
   ;;(center-vertex nil)
   ))

(defmethod map-over-all-arcs ((object basic-network) fn)
  (with-slot-values (arcs) object
    (loop for i from 0 below (length arcs)
	  for arc = (aref arcs i)
	  do (funcall fn (aref arc 0) (aref arc 1)))))

(defmethod set-arc-list ((object basic-network) arc-list)
  (loop with vertices = (vertices object)
	for (v1 v2) in arc-list
	do (add-arc-only object (aref vertices v1) (aref vertices v2))))

(defmethod get-arc-list ((object basic-network))
  (let (l)
    (map-over-all-arcs object
		       #'(lambda (v1 v2)
			   (push (list (vertex-id object v1) (vertex-id object v2))
				 l)))
    l))

|#

;;;  ****************************  NO-VERTICES-MIXIN  ****************************

(defstruct-class no-vertices-mixin (basic-gl-object) ())

(defmethod %vertex-array ((object no-vertices-mixin))
  ;;(break)
  nil)


(defmethod draw-fragments ((object no-vertices-mixin))
  ;;(format t "draw-fragments ~a~%" object) 
  ;;(break)
  (glPushName 0)
  (glPointSize 2.0)
  (glLoadName 0)
  (glBegin GL_POINTS)
  ;;(glVertex3d 0.0 0.0 0.0) ; FIXMEXX
  (glVertex3dv (perturbed-object-origin object))
  (glEnd)
  (glPopName)
  )

#| broken
(defmethod draw-fragments ((object no-vertices-mixin))
  (glPushName 0)
  (glLoadName 0)
  (highlight-point object (perturbed-object-origin object) gui::*current-view*)
  (glPopName)
  )
|#

;(pcl::undefmethod highlight-object (no-vertices-mixin t t t t t))
#+never
(defmethod highlight-object ((object no-vertices-mixin) frag-descr view object-graphics-style highlight-graphics-style selectids)
  ;;(format t "highlight-object ~a~%" frag-descr)
  (draw-object-around object view highlight-graphics-style selectids))

;(pcl::undefmethod highlight-vertex (no-vertices-mixin t t))
(defmethod highlight-vertex ((object no-vertices-mixin) vertex-id view &optional (box-radius 4.0))
  (declare (type double-float box-radius))
  (declare (type fixnum vertex-id))
  (draw-object-around object view))

;;;  ****************************  CROSSHAIR-OBJECT  ****************************

(defstruct-class crosshair-object (no-vertices-mixin)
   ((outside-size :initform 8.0 :initarg :outside-size) ; outside size of crosshair
    (inside-size :initform 4.0 :initarg :inside-size)
    )
  )

(defmethod initialize-instance :after ((object crosshair-object) &key &allow-other-keys)
  (with-class-slots crosshair-object (outside-size inside-size) object
    (setf (get-prop object :immediate-render-p) t) ;
    (setf inside-size (dfloat inside-size)
	  outside-size (dfloat outside-size))))

;;; This works but requires immediate-rendering rather than display-list.
;;; DEFAULT-OBJECT-SETS in cme-compat/worlds.lisp separates immediate-render objects
;;; into their own object-sets with :immediate-render-p = t
;;;

;;;(defmethod draw-object ((object crosshair-object) view)
;;;  (declare (ignorable view))
;;;  ;;(when gui::*building-display-list* (format t "draw-object crosshair-object ~a ~a~%" object view))
;;;  (with-class-slot-values crosshair-object (inside-size outside-size) object
;;;    (let ((view-position (transform-vector (transforms::object-to-view-transform object view)
;;;                                           (perturbed-object-origin object))))
;;;      (when view-position ; view-position is NIL when object is behind camera.
;;;        (draw-crosshair view-position inside-size outside-size)))))

(defmethod draw-object ((object crosshair-object) view)
  (declare (ignorable view))
  (if *object-selection-mode* ; *gl-selection-pick-matrix-params*
      (draw-fragments object)

      ;;(when gui::*building-display-list* (format t "draw-object crosshair-object ~a ~a~%" object view))
      (with-class-slot-values crosshair-object (inside-size outside-size) object
	(let ((view-position (transform-vector (transforms::object-to-view-transform object view)
					       (perturbed-object-origin object))))
	  (when view-position ; view-position is NIL when object is behind camera.
	    (draw-crosshair view-position inside-size outside-size))))))

;;(undefmethod draw-fragments (crosshair-object))
#+never
(defmethod draw-fragments ((object crosshair-object))
  (
    ;;(format t "draw-fragments ~a ~a~%" object *gl-selection-pick-matrix-params*)
    (glPushName 0)
    (glPointSize 2.0)
    (glLoadName 0)
    (glBegin GL_POINTS)
    ;;(glVertex3d 0.0 0.0 0.0) ; FIXMEXX
    (glVertex3dv (perturbed-object-origin object))
    (glEnd)
    (glPopName)
    ))

;;;  ****************************  POINT-OBJECT  ****************************


(defstruct-class point-object (no-vertices-mixin gl-object)
   ((circle-radius :initform 1.0 :initarg :circle-radius :accessor circle-radius))
  )

(defmethod draw-object ((object point-object) view) 
  (declare (ignorable view))
  (with-object-transform object
    (with-class-slots point-object (circle-radius) object
      (glPointSize (* 2.0 circle-radius))
      (glBegin GL_POINTS)
      (glVertex3dv (perturbed-object-origin object))
      (glEnd))))

#|

(let* ((view (gui::top-view))
       (obj (make-instance '3d-point-object
			   :world (3d-world view)
			   :object-to-world-transform
			   (make-4x4-coordinate-transform
			    (make-object-to-parent-matrix (cv 266.9 4632.1 6015.4)))))
       (obj2 (make-instance '3d-crosshair-object
			    :world (3d-world view)
			    :object-to-world-transform
			    (make-4x4-coordinate-transform
			     (make-object-to-parent-matrix (cv 260.0 4732.1 6015.4)))))
       (os (gui::make-object-set (list obj obj2) :world (3d-world view) :immediate-render-p t)))
  (push os (gui::object-sets view))
  )

(pop (gui::object-sets (gui::top-view)))

(setq *os* (gui::object-sets (gui::top-view)))
(describe (find-class '3d-point-object))
|#




(defstruct-class rectangle-object (gl-sizable-object-mixin) ()
  (:default-initargs
      :sizes (cv 10.0 10.0 0.0)
    :vertex-array (make-vertex-array (cv 0.0 0.0 0.0)
				     (cv 1.0 0.0 0.0)
				     (cv 1.0 1.0 0.0)
				     (cv 0.0 1.0 0.0))
    ))

(defmethod initialize-instance :after ((object rectangle-object) &key &allow-other-keys)
  (scale-vertex-array object (sizes object)))


(defmethod center-vertex ((object rectangle-object))
  (cv 0.5 0.5 0.0))

  
(defmethod draw-object ((obj rectangle-object) view) 
  (declare (ignorable view))
  (with-object-transform object
    (let* ((verts (vertex-array obj))
	   (nverts (array-dimension verts 0)))
      (glBegin  GL_LINE_LOOP)
      (loop for i of-type fixnum from 0 below nverts
	    do (glVertex3d (aref verts i 0) (aref verts i 1) (aref verts i 2)))
      (glEnd))))
	 

(defclass forward-referenced-object (basic-object) ())

(defmethod draw-object ((object forward-referenced-object) view)
  (declare (ignore object view)))



(defstruct-class 3d-circle (3d-closed-curve)
  ((radius :initform 10.0 :initarg :radius :accessor radius)
   (npts :initform 20 :initarg :npts :accessor npts)))

(defmethod initialize-instance :after ((obj 3d-circle) &rest args &key &allow-other-keys)
  (with-class-slots circle (vertex-array radius npts) obj
    (setf vertex-array
	  (make-vertex-array-from-vertex-list
	   (loop with dt = (/ (* 2.0 pi) (dfloat npts))
		 for i from 0 below npts
		 for theta from 0.0 by dt
		 collect (cv (* radius (cos theta)) (* radius (sin theta)) 0.0))))))



