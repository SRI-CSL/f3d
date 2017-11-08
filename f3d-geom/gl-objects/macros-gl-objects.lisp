(in-package :obj)

;;; Contains direct dependencies on OpenGL.

;;; These moved to math/bounding-boxes.lisp
#+never
(progn
(deftype vertex-element-type () 'double-float)
(deftype vertex-array-type () '(simple-array vertex-element-type (* *)))
(deftype vertex-index-type () '(unsigned-byte 16))
(deftype vertex-index-array-type () '(simple-array vertex-index-type (*)))
); end progn

(defmacro bind-vertex-array-elements (element-let-list vertex-array index &body body)
  `(let* ((%vector%  ,vertex-array)
	  (%index% ,index))
    (declare (type vertex-array-type %vector%))
    (declare (fixnum %index%))
    (let ,(loop for i from 0
		for var in element-let-list
		collect `(,var (aref %vector% %index% ,i)))
      (declare (double-float . ,element-let-list))
      .,body)))

(defmacro set-vertex-array-elements (vertex-array index &rest elems)
  `(let* ((%vector%  ,vertex-array)
	  (%index% ,index))
    (declare (type vertex-array-type %vector%))
    (declare (fixnum %index%))
    (setf . ,(loop for i from 0
		   for elem in elems
		   collect `(aref %vector% %index% ,i)
		   collect elem))
    nil))

;;; unused
;;;(defmacro with-object-transform2 (object &body body)
;;;  `(unwind-protect
;;;    (let ((mat (object-to-world-matrix ,object)))
;;;      (glMatrixMode GL_MODELVIEW)
;;;      (glPushMatrix)
;;;      (when mat
;;;        (glMultMatrixd_transposed mat))
;;;      . ,body)
;;;    (progn
;;;      (glMatrixMode GL_MODELVIEW) ;; Careful, set the matrix mode to avoid stack underflow -CC
;;;      (glPopMatrix))))

(defmacro with-object-transform (object &body body)
  (ignore object)
  `(progn . ,body))

(defmacro maybe-with-static-area (&body body)
  `(progn . ,body))

(declaim (inline object-vertex))

;;; This is misnamed -- it does not return a vertex, but a coordinate-vector.
;;; Perhaps this should be called OBJECT-VERTEX-POSITION.
(defun object-vertex (vertex-array vertex-id)
  (declare (type fixnum vertex-id))
  (declare (type vertex-array-type vertex-array))
  (cv (aref vertex-array vertex-id 0)
      (aref vertex-array vertex-id 1)
      (aref vertex-array vertex-id 2)))

(declaim (inline set-object-vertex))

(defun (setf object-vertex) (cv vertex-array vertex-id)
  (declare (type fixnum vertex-id))
  (declare (type vertex-array-type vertex-array))
  (bind-vector-elements (x y z) cv
    (setf (aref vertex-array vertex-id 0) x
	  (aref vertex-array vertex-id 1) y
	  (aref vertex-array vertex-id 2) z))
  cv)

(defvar *gl-selection-pick-matrix-params* nil)

;;; Careful, set the matrix mode to avoid stack underflow -CC
;;;(defmacro with-gl-window-transform (&body body)
;;;  `(unwind-protect
;;;    (progn (when *gl-selection-pick-matrix-params*
;;;             (apply #'set-gl-pick-matrix *gl-selection-pick-matrix-params*))
;;;           (glMatrixMode GL_PROJECTION)(glPushMatrix) 
;;;           (set-2d-to-ndc-matrix *highlight-vertex-identity-matrix*)
;;;           (glMatrixMode GL_MODELVIEW) (glPushMatrix) (glLoadIdentity)
;;;           . ,body)
;;;    ;; clean up
;;;    (progn (glMatrixMode GL_MODELVIEW) (glPopMatrix) 
;;;           (glMatrixMode GL_PROJECTION) (glPopMatrix))))

;;; Careful, set the matrix mode to avoid stack underflow -CC
(defmacro with-gl-window-transform (&body body)
  `(unwind-protect
    (progn (glMatrixMode GL_PROJECTION)(glPushMatrix) 
	   (set-2d-to-ndc-matrix *highlight-vertex-identity-matrix*)
	   (glMatrixMode GL_MODELVIEW) (glPushMatrix) (glLoadIdentity)
	   . ,body)
    ;; clean up
    (progn (glMatrixMode GL_MODELVIEW) (glPopMatrix) 
	   (glMatrixMode GL_PROJECTION) (glPopMatrix))))


;; Can't define this here, because of dependency on gui::set-2d-matrices.
;;;(defmacro with-gl-view-transform ((view) &body body)
;;;  `(unwind-protect
;;;    (progn (glMatrixMode GL_PROJECTION)(glPushMatrix)
;;;           (gui::set-2d-matrices ,view)
;;;           (glMatrixMode GL_MODELVIEW) (glPushMatrix) (glLoadIdentity)
;;;           . ,body)
;;;    ;; clean up
;;;    (progn (glMatrixMode GL_MODELVIEW) (glPopMatrix) (glMatrixMode GL_PROJECTION) (glPopMatrix))))

(defvar *break-on-display-error* nil)
;(setq *break-on-display-error* t)
;(setq *break-on-display-error* nil)
(defvar *gl-object-drawing-superior* nil)


;;; WITH-GL-OBJECT-MATRICES is called from pick.lisp
;;; KEEP THIS IN SYNC WITH WITH-GL-OBJECT-DRAWING-INT  - nearly identical
;;;(defmethod with-gl-object-matrices (object view fn)
;;;  (let* ((object-to-parent-transform (object-to-parent-transform object)))
;;;    (unwind-protect
;;;         (progn (when object-to-parent-transform
;;;                  (glMatrixMode GL_MODELVIEW)
;;;                  (glPushMatrix)
;;;                  (obj::set-modelview-matrix object (and (typep object 'gl-3d-object-mixin) 
;;;                                                         (3d-world view)))
;;;                  )
;;;                (funcall fn object))
;;;             
;;;      (progn (when object-to-parent-transform
;;;               ;; Careful, set the matrix mode to avoid stack underflow -CC
;;;               (glMatrixMode GL_MODELVIEW)
;;;               (glPopMatrix))
;;;             (let ((glerr (glGetError)))
;;;               (unless (eql glerr gl:GL_NO_ERROR)
;;;                 (if *break-on-display-error*
;;;                     (error "with-gl-object-matrices GL Error ~a drawing object ~a"
;;;                            (gl::gluErrorString glerr) object)
;;;                     (format t "with-gl-object-matrices GL Error ~a drawing object ~a"
;;;                             (gl::gluErrorString glerr) object))
;;;                 ))))))

(defun with-gl-object-drawing-int (object view graphics-style fn)
  ;;(format t "with-gl-object-drawing-int ~a~%" (list object *gl-object-drawing-superior*))
  (let ((object-to-parent-transform (and object (object-to-parent-transform object))))
    (unwind-protect
         (progn
           (when object-to-parent-transform
             (glMatrixMode GL_MODELVIEW)
             (glPushMatrix)
             (set-modelview-matrix object (and (typep object 'gl-3d-object-mixin) (3d-world view)))
             )
           (when graphics-style
             (glPushAttrib GL_ALL_ATTRIB_BITS)
             (set-gl-attributes graphics-style))

           (let ((*gl-object-drawing-superior* object))
             (funcall fn)))
      
      ;; cleanup forms
      (progn (when graphics-style (glPopAttrib))
             (when object-to-parent-transform
               ;; Careful, set the matrix mode to avoid stack underflow -CC
               (glMatrixMode GL_MODELVIEW)
               (glPopMatrix))
             (let ((glerr (glGetError)))
               (unless (eql glerr gl:GL_NO_ERROR)
		 (setq *bad-object* object)
                 (if *break-on-display-error*
                     (error "with-gl-object-drawing GL Error ~a drawing object ~a~%"
                            (gl::gluErrorString glerr) object)
                     (format t "with-gl-object-drawing GL Error ~a drawing object ~a~%"
                             (gl::gluErrorString glerr) object))
                 ))

             ))))
  
(defun with-gl-object-drawing-int (object view graphics-style fn)
  ;;(format t "with-gl-object-drawing-int ~a~%" (list object *gl-object-drawing-superior*))
  (let ((object-to-parent-transform (and object (object-to-parent-transform object))))
    (unwind-protect
         (progn
           (when object-to-parent-transform
             (glMatrixMode GL_MODELVIEW)
             (glPushMatrix)
             (set-modelview-matrix object (and (typep object 'gl-3d-object-mixin) (3d-world view)))
             )
           (when graphics-style
             (glPushAttrib GL_ALL_ATTRIB_BITS)
             (set-gl-attributes graphics-style))

           (let ((*gl-object-drawing-superior* object))
             (funcall fn)))
      
      ;; cleanup forms
      (progn (when graphics-style (glPopAttrib))
             (when object-to-parent-transform
               ;; Careful, set the matrix mode to avoid stack underflow -CC
               (glMatrixMode GL_MODELVIEW)
               (glPopMatrix))
             (let ((glerr (glGetError)))
               (unless (eql glerr gl:GL_NO_ERROR)
		 (setq *bad-object* object)
		 (format t "with-gl-object-drawing GL Error ~a drawing object ~a~%"
			 (gl::gluErrorString glerr) object)
		 (when *break-on-display-error*
		   (break))
                 ))

             ))))
  
(defmacro with-gl-object-drawing ((object view graphics-style) &body body)
  `(with-gl-object-drawing-int ,object ,view ,graphics-style
    #'(lambda () .,body)))

#|
(defmacro update-objects (objs &body body)
  `(let ((objs ,objs))
    (unwind-protect
	 (progn (update-objects-before objs)
		.,body)
      ;; cleanup clauses
      (update-objects-after objs))))


(defmacro update-object (obj &body body)
  `(let ((objs (list ,obj)))
    (unwind-protect
	 (progn (update-objects-before objs)
		.,body)
      ;; cleanup clauses
      (update-objects-after objs))))
|#

(defun update-objects-internal (objs fn)
  (unwind-protect
       (progn (update-objects-before objs)
	      (funcall fn))
    ;; cleanup clauses
    (update-objects-after objs)))

(defmacro update-objects (objs &body body)
  `(update-objects-internal ,objs
    #'(lambda () (progn .,body))))

(defmacro update-object (obj &body body)
  `(update-objects-internal (list ,obj)
    #'(lambda () (progn .,body))))

