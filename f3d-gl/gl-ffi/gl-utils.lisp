(in-package :gl)

#|
(setq *arr* (make-array '(100) :element-type '(unsigned-byte 8)))

(%pointer *arr*) = 1581711039
(setq *print-array* nil)
(underlying-simple-vector *arr*)   5E46FEBF
(qffi::array-data-address *arr*) 1581711040
|#

#+old
(progn 

(defun glDrawPixels-offset (width height format type pixel-array offset)
  (glDrawPixels_offset width height format type
		       (+ offset (qffi::array-data-address pixel-array))))

(def-foreign-function (glDrawPixels_offset
                       (:name "glDrawPixels")
                       (:return-type :null))
    (width  GLsizei)
    (height  GLsizei)
    (format  GLenum)
    (type   GLenum)
    (pixels :unsigned-int)	
    )
); end progn

(def-foreign-function (glDrawPixels-offset
                       (:name (freedius-prefix "glDrawPixels_offset"))
                       (:return-type :null))
    (width  GLsizei)
    (height  GLsizei)
    (format  GLenum)
    (type   GLenum)
    (pixels :array)
    (pixels-offset GLint)
    ;; &optional ((pixels-offset 0) GLint)
    )

;;; The complication here is with the units for pixels-offset.  Must multiply by bytes-per-elem.
(def-foreign-function (glTexImage2D-offset
                       (:name (freedius-prefix "glTexImage2D_offset"))
                       (:return-type :null))
    (target     GLenum)
    (level      GLint)
    (internalFormat  GLint)
    (width      GLsizei)
    (height     GLsizei)
    (border     GLint)
    (format     GLenum)
    (type       GLenum)
    (pixels :array) ; allow non-simple arrays too
    (pixels-offset GLint)
    ;; &optional ((pixels-offset 0) GLint)
    )

(def-foreign-function (glClearColor4dv (:name (freedius-prefix "glClearColor4dv")))
  (color :simple-array) ; double-float of 4 elements
  )

(defun glTranslatedv (delta-vector)
  (glTranslated (aref delta-vector 0) (aref delta-vector 1) (aref delta-vector 2)))


;;; FV stands for float-vector.

(defun fv (&rest floats)
  (let* ((n (length floats))
	 (arr (make-array n :element-type 'single-float)))
    (declare (type (simple-array single-float (*)) arr))
    (loop for i fixnum from 0 below n
	  for x in floats
	  do (setf (aref arr i) (float x 1.0f0)))
    arr))

#+never ;; defined in gl-ffi.lisp
(def-foreign-function (handle_gl_errors (:name (freedius-prefix "handle_gl_errors")))
  (context-string :simple-string))

(def-foreign-function (transpose_4x4_double (:name (freedius-prefix "transpose_4x4_double")))
    (from-mat (:simple-array :double-float))
  (to-mat (:simple-array :double-float)))

(def-foreign-function (glMultMatrixd_transposed (:name (freedius-prefix "glMultMatrixd_transposed")))
    (mat (:simple-array :double-float)))


(defun gl-make-4x4-matrix ()
  (make-array '(4 4) :element-type 'double-float :initial-element 0.0d0)
  )

(defparameter *gl-4x4-double-float-matrix*
  (gl-make-4x4-matrix))

(def-foreign-function (glDrawquad_strip
		       (:name (freedius-prefix "glDrawquad_strip")))
    (verts :simple-array)
  (nfaces :int))

(def-foreign-function (glDrawquad_strip_indexed
			(:name (freedius-prefix "glDrawquad_strip_indexed")))
  (verts :simple-array)
  (nfaces :int)
  (indices  :simple-array))

(def-foreign-function (glDrawquad_strip_with_face_normals
						   (:name (freedius-prefix "glDrawquad_strip_with_face_normals")))
  (verts :simple-array)
  (face_normals :simple-array)
  (nfaces :int))

(def-foreign-function (glDrawquad_strip_indexed_with_face_normals
		       (:name (freedius-prefix "glDrawquad_strip_indexed_with_face_normals")))
    (verts :simple-array)
  (face_normals :simple-array)
  (nfaces :int)
  (indices  :simple-array))

(defun glDrawquad-strip-with-face-normals (verts face_normals nfaces &optional indices)
  (if indices
      (glDrawquad_strip_indexed_with_face_normals verts face_normals nfaces indices)
      (glDrawquad_strip_with_face_normals verts face_normals nfaces)))


(def-foreign-function (glDrawquad_strip_with_vertex_normals
			(:name (freedius-prefix "glDrawquad_strip_with_vertex_normals")))
  (verts :simple-array)
  (normals :simple-array)
  (nfaces :int))

(def-foreign-function (glDrawquad_strip_indexed_with_vertex_normals
		       (:name (freedius-prefix "glDrawquad_strip_indexed_with_vertex_normals")))
    (verts :simple-array)
  (normals :simple-array)
  (nfaces :int)
  (indices  :simple-array))

(defun glDrawquad-strip-with-vertex-normals (verts normals nfaces &optional indices)
  (if indices
      (glDrawquad_strip_indexed_with_vertex_normals verts normals nfaces indices)
      (glDrawquad_strip_with_vertex_normals verts normals nfaces)))



#|
(defparameter m1 (make-array '(4 4) :element-type 'double-float
		       :initial-contents
		       '((0.0 1.0 2.0 3.0)
			 (0.1 1.1 2.1 3.1)
			 (0.2 1.2 2.2 3.2)
			 (0.3 1.3 2.3 3.3)
			 )))
(progn (transpose_4x4_double m1 *gl-4x4-double-float-matrix*)
       (inspect *gl-4x4-double-float-matrix*))

(inspect m1)
|#


;;;(defun glMultMatrix (matrix)
;;;  (transpose_4x4_double matrix *gl-4x4-double-float-matrix*)
;;;  (glMultMatrixd *gl-4x4-double-float-matrix*))

(defun glMultMatrix (matrix)
  (glMultMatrixd_transposed matrix))

(defun glLoadMatrix (matrix)
  (transpose_4x4_double matrix *gl-4x4-double-float-matrix*)
  (glLoadMatrixd *gl-4x4-double-float-matrix*))

(defun glGetProjectionMatrix (&optional matrix)
  (let ((m (or matrix (gl-make-4x4-matrix))))
    (glGetDoublev GL_PROJECTION_MATRIX *gl-4x4-double-float-matrix*)
    (transpose_4x4_double *gl-4x4-double-float-matrix* m)
    m))

(defun glGetModelviewMatrix (&optional matrix)
  (let ((m (or matrix (gl-make-4x4-matrix))))
    (glGetDoublev GL_MODELVIEW_MATRIX *gl-4x4-double-float-matrix*)
    (transpose_4x4_double *gl-4x4-double-float-matrix* m)
    m))

(defun glGetMatrix (pname &optional matrix)
  (let ((m (or matrix (gl-make-4x4-matrix))))
    (glGetDoublev pname *gl-4x4-double-float-matrix*)
    (transpose_4x4_double *gl-4x4-double-float-matrix* m)
    m))

(defun glPreMultMatrix (gl-matrix-name matrix)
  (let ((current-matrix (glGetMatrix gl-matrix-name)))
    (glLoadMatrix matrix)
    (glMultMatrix current-matrix)))

(defun glPreMultProjectionMatrix (matrix)
  (let ((current-matrix (glGetMatrix GL_PROJECTION_MATRIX)))
    (glMatrixMode GL_PROJECTION)
    (glLoadMatrix matrix)
    (glMultMatrix current-matrix)))
#|
(glMatrixMode GL_PROJECTION)
(glGetProjectionMatrix)
|#

(defparameter *glGetInteger-arr* (make-array0 20 :element-type '(signed-byte 32)))
(defparameter *glGetFloat-arr* (make-array0 20 :element-type 'single-float))
(defparameter *glGetBoolean-arr* (make-array0 20 :element-type '(unsigned-byte 8)))

(defun glGetInteger (keyword)
  (glGetIntegerv keyword *glGetInteger-arr*)
  (aref *glGetInteger-arr* 0))

(defun glGetFloat (keyword)
  (glGetFloatv keyword *glGetFloat-arr*)
  (aref *glGetFloat-arr* 0))

(defun glGetBoolean (keyword)
  (glGetBooleanv keyword *glGetBoolean-arr*)
  (= (aref *glGetBoolean-arr* 0) 1))

(def-foreign-function (gluProject_to_window (:name (freedius-prefix "gluProject_to_window")))
    (from (:simple-array :double-float))			; (array double-float (3))
  (to (:simple-array :double-float))		; (array double-float (3))
  )

(def-foreign-function (gluProject_to_world (:name (freedius-prefix "gluProject_to_world")))
    (from (:simple-array :double-float))	; (array double-float (3))
  (to (:simple-array :double-float))		; (array double-float (3))
  )


(defun gl-current-color ()
  (let ((v (make-array 4 :element-type 'single-float)))
    (gl::glGetFloatv GL_CURRENT_COLOR v)
    v))

(defmacro preserving-current-color (&body body)
  `(let ((old-color (gl-current-color)))
    (unwind-protect (progn .,body)
      (glColor4fv old-color))))

(defun gl-make-int-vector (n)
  (make-array0 n :element-type '(signed-byte 32)))

(defun glXcontext_matrices ()
  (let ((model-matrix (gl-make-4x4-matrix))
	(proj-matrix (gl-make-4x4-matrix))
	(vp (gl-make-int-vector 4)))

    (glGetDoublev GL_MODELVIEW_MATRIX model-matrix)
    (glGetDoublev GL_PROJECTION_MATRIX proj-matrix)
    (glGetIntegerv GL_VIEWPORT vp)
    (values model-matrix proj-matrix vp)
    ))

(defun gl-make-coordinate-vector (n)
  (make-array0 n :element-type 'double-float))

;;; FIXME:  Rename to glProject_to_current_window
;;; transform vector to window coordinates in current glX context.
(defun glProject_to_window (vect &optional to_vect)
  (unless to_vect (setq to_vect (gl-make-coordinate-vector 3)))
  (and (eql 1 (gluProject_to_window vect to_vect))
       to_vect))

(defun glProject_to_world (vect &optional to_vect)
  (unless to_vect (setq to_vect (gl-make-coordinate-vector 3)))
  (and (eql 1 (gluProject_to_world vect to_vect))
       to_vect))

#|                                              MATROX  NVIDIA?    NVidia 6600
(glGetInteger GL_MAX_MODELVIEW_STACK_DEPTH) ; = 32        32           32
(glGetInteger GL_MAX_PROJECTION_STACK_DEPTH); = 10        4             4
(glGetInteger GL_SUBPIXEL_BITS) ;                         8            12
(glGetInteger GL_MAX_TEXTURE_SIZE) ;                      4096       4096
(glGetInteger GL_MAX_VIEWPORT_DIMS);                      4096       4096
(glGetInteger GL_STEREO);                                 0             0
(glGetInteger GL_AUX_BUFFERS);                            0             4
(glGetInteger GL_MAX_VIEWPORT_DIMS)                                  4096


|#


(defun preserving-glPolygonMode_int (fn)
  (let ((old-polygon-mode (make-array 2 :element-type '(signed-byte 32))))
    (glGetIntegerv GL_POLYGON_MODE old-polygon-mode)
    (unwind-protect (funcall cfn)
      (glPolygonMode GL_FRONT (aref old-polygon-mode 0))
      (glPolygonMode GL_BACK (aref old-polygon-mode 1)))))

(defmacro preserving-glPolygonMode (&body body)
  `(preserving-glPolygonMode_int #'(lambda() . ,body)))

(defun preserving-glAttributes_int (fn)
  (unwind-protect 
       (progn (glPushAttrib GL_ALL_ATTRIB_BITS)
	      (funcall fn))
    (glPopAttrib)))

(defmacro preserving-glAttributes (&body body)
  `(preserving-glAttributes_int #'(lambda () . ,body)))



;;; ****************************  POLYGON TESSELATION  ****************************

(def-foreign-function (draw_line_strip
			(:name (freedius-prefix "draw_line_strip")))
  (verts (:simple-array :double-float))
  (n :int)
  )


(def-foreign-function (draw_polygon
		       (:name (freedius-prefix "draw_polygon"))
		       (:call-direct nil))
    (verts (:simple-array :double-float))
  (indices (:simple-array-or-null :unsigned-16bit))
  (n :int)
  (normals (:simple-array-or-null :double-float))
  (normal-index :int))

(def-foreign-function (draw_polygon_tesselated
			(:name (freedius-prefix "draw_polygon_tesselated")))
  (verts (:simple-array :double-float))
  (indices (:simple-array-or-null :unsigned-16bit))
  (n :int)
  (normals (:simple-array-or-null :double-float))
  (normal-index :int))


(defun draw-polygon-tesselated (verts indices &key 
				;;boundary-only
				(edge-flags t)
				;;(winding-rule GLU_TESS_WINDING_ABS_GEQ_TWO)
				(winding-rule GLU_TESS_WINDING_POSITIVE)
				normal
				normals
				normal-index
				;;(normal *vertical-unit-vector*)
				)
  (ignore edge-flags winding-rule)
  (draw_polygon_tesselated verts (or indices #+allegro 0)
			   (if indices (length indices) (array-dimension verts 0))
			   (or normals normal #+allegro 0) (or normal-index 0))
  
  )
    
;;; *******************  STIPPLE PATTERNS  *******************

(defparameter *glstipple-cache* (make-hash-table :test 'equal))

;;; pattern is a 2d-array or list which is replicated to fill a 32x32 bitmap
(defun make-glstipple (pattern)
  (or (gethash pattern *glstipple-cache*)
      (setf (gethash pattern *glstipple-cache*)
	    (let ((ydim (length pattern))
		  (xdim (length (car pattern)))
		  (array (make-array '(32 32) :element-type 'bit :initial-element 0)))
	      (declare (fixnum ydim xdim))
	      (declare (type (simple-array bit (32 32)) array))
	      (loop for y fixnum from 0 below 32
		    for y2 fixnum = (mod y ydim)
		    for row = (elt pattern y2)
		    do (loop for x fixnum from 0 below 32
			     for x2 fixnum = (mod x xdim)
			     do (setf (aref array x y)
				      (elt row x2))))
	      array))))


(defparameter *stipple0*
  (make-glstipple '((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0))))

(defparameter *stipple1*
  (make-glstipple '((0 1 0 0) (0 0 0 1) (1 0 0 0) (0 0 1 0))))

(defparameter *stipple2*
  (make-glstipple '((0 1 0 1) (1 0 1 0) (0 1 0 1) ( 1 0 1 0))))

(defparameter *stipple3*
  (make-glstipple '((0 1 0 0) (0 0 0 0) (0 0 0 1) (0 0 0 0))))

(defparameter *stipple4*
  (make-glstipple '((0 1 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0))))


(def-foreign-function (compute_ribbon_face_vertex
		       (:name (freedius-prefix "compute_ribbon_face_vertex")))
    (object-to-world-matrix (:simple-array :double-float)) ; 4x4 double
  (v1 (:simple-array :double-float))
  (v2 (:simple-array :double-float))
  (v3 (:simple-array :double-float)) ; vertices double 4
  (vleft (:simple-array :double-float))
  (vright (:simple-array :double-float))	;vertices double 3
  )

(def-foreign-function (compute_ribbon_edge_vertices
		       (:name (freedius-prefix "compute_ribbon_edge_vertices")))
    (object-to-world-matrix (:simple-array :double-float)) ; 4x4 double
  (verts (:simple-array :double-float))		; nx4 double
  (nverts :int) ; int
  (edge_verts (:simple-array :double-float))	; 2nx3 double
  )



(defun glColor (list-or-vector)
  (if (consp list-or-vector)
      (case (length list-or-vector)
	(3 (apply 'glColor3d list-or-vector))
	(4 (apply 'glColor4d list-or-vector))
	(otherwise (error "")))

      (let ((vector list-or-vector))
	(declare (type (simple-array double-float (*)) vector))
	(let ((r (aref vector 0))
	      (g (aref vector 1))
	      (b (aref vector 2)))
	  (case (length vector)
	    (3 (glColor3d r g b))
	    (4 (glColor4d r g b (aref vector 3)))
	    (otherwise (error "")))))))


;;; 
;;; Virtually every glColor call in FREEDIUS uses double-floats, so do the same here:
(defun make-gl-Color (list-or-vector-or-name)
  (cond ((or (symbolp list-or-vector-or-name)
	     (stringp list-or-vector-or-name))
         (color-name-to-gl list-or-vector-or-name)) ;; Case independence is enforced down in lx::color-name-to-3d
	((or (vectorp list-or-vector-or-name)
	     (consp list-or-vector-or-name))
	 (let ((color (make-array (length list-or-vector-or-name)
				  :element-type 'double-float)))
;;				  :element-type 'single-float)))
	   (loop for i from 0
		 for x in list-or-vector-or-name
		 do (setf (aref color i)
                          (float (elt list-or-vector-or-name i) 0.0d0)
                          ;;(float (elt list-or-vector-or-name i) 0.0f0)
                          ))
	   color))
	(t (error "make-glColor: Illegal color-spec ~a~%" list-or-vector-or-name))))


;(make-gl-Color :red) 

;;;
;;; Cocoa forces us to wrap a lockFocus / unlockFocus pair of calls
;;; around GL drawing operations.  Here's a simple API spec that
;;; should help:

;;(defvar *gl-context-stack* nil)

;; (defmethod glMakeCurrent :before (window) (push window *gl-context-stack*))

(defmethod glMakeCurrent (window) nil)

;;;
(defmethod glUnMakeCurrent (window) nil)

;;;

;; Not sure if we need this:
;; (defmethod glUnMakeCurrent :before (window) (glFinish))

;; (defmethod glUnMakeCurrent :after (window)
;;  (unless (pop *gl-context-stack*)
;;    (format t "~%gl context stack is already empty.")))


;;;
;;; Not really a form, but a useful macro:

#+never
(defmacro gui::without-interrupts (&body body)
  `(sb-thread::with-interruptions-lock (sb-thread::*current-thread*) 
     ,#body))


;;;
;;; SBCL supports OS threads and these are used elsewhere on occasion
;;; (e.g., the web server).  Since OpenGL is a shared resource, we
;;; need to protect it.  So far, the following forms only exist for
;;; SBCL - CMUCL does not need these, as everything is
;;; single-threaded.  Haven't tested Allegro MP yet.


;;; Some OpenGL models (notably Cocoa) require GL operations to be
;;; nested within a MakeCurrent / UnMakeCurrent pair for the desired
;;; GL canvas.  GL usage within FREEDIUS does not, at present, fit
;;; well with that model.  For example, CLEAR-WINDOW exits after a
;;; call to MakeCurrent - as a result, there may be multiple
;;; MakeCurrent calls before a call to UnMakeCurrent.

#+sb-thread
(defvar *opengl-mutex* (sb-thread::make-mutex :name "opengl-mutex"))

#-sb-thread
(defmacro with-gl-locked (&body body)
  `(progn ,@body))

#+sb-thread
(defmacro with-gl-locked (&body body)
  `(sb-thread::with-recursive-lock (*opengl-mutex*)
     ,@body))




;;;
;;; I think this should be controlled in glUnMakeCurrent, not in the
;;; macro form itself:
(defvar *gl-unmake-current* nil)

;;; It's probably best to include mutexes in this form for those Lisps
;;; that support threading.  This kind of a form is probably the way
;;; to go.

(defmacro with-gl-window ((window) &body body)
  `(with-gl-locked
     (unwind-protect
	  (progn
	    (glMakeCurrent ,window)
	    ,@body
	    (glFlush)
	    )
       (when *gl-unmake-current*
	 (glUnMakeCurrent ,window)))))
