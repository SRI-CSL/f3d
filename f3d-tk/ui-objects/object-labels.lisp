(in-package :obj)

;;; This file cannot be loaded by the :OBJ subsystem because is needs the GUI package.
;;; Is this a fundamental flaw in the the design of the SYSTEM tool?  Perhaps
;;; there needs to be a "standard way" to load the files defining the packages of a subsystem.

;;; **********************************  OBJECT LABELS  **********************************

(defparameter *default-highlight-label-graphics-style*
  (make-instance 'gl-graphics-style
		 :font '("Helvetica" 12)
		 :color :yellow))  ; why not use cyan?
     
;;; for labels draw for normal gl-objects
(defparameter *default-gl-object-label-graphics-style*
  (make-instance 'gl-graphics-style
		 :font '("Helvetica" 12)
		 :color :yellow))

;;; for text-objects (gl-label)
(defparameter *default-gl-label-graphics-style*
  (make-instance 'gl-graphics-style
                 :font '("Helvetica" 12)
		 :color :yellow))

(defparameter *labels-use-object-color* nil)
;;(defparameter *labels-use-object-color* t)

;;; Enables object-labels for ordinary object drawing This slows
;;; things down significantly, and the labels are displayed when
;;; highlighting, so set this to NIL:
(defparameter *enable-object-labels* nil)

;;; Enables object-labels for object highlighting
(defparameter *enable-highlight-object-labels* t)

#|
(setq *enable-object-labels* t *enable-highlight-object-labels* t)
(setq *enable-object-labels* nil *enable-highlight-object-labels* t)
(setq *enable-object-labels* t *enable-highlight-object-labels* nil)
(setq *enable-object-labels* nil *enable-highlight-object-labels* nil)
|#

(defun glColornfv (color)
  (if (= (length color) 4)
      (glColor4fv color)
      (if (= (length color) 3)
	  (glColor3fv color)
	  (error "Color vector ~a is not of length 3 or 4." color))))

(defun glColorndv (color)
  (if (= (length color) 4)
      (glColor4dv color)
      (if (= (length color) 3)
	  (glColor3dv color)
	  (error "Color vector ~a is not of length 3 or 4." color))))

;(fmakunbound 'draw-string-bbox)
;;; This purpose for this is mouse-sensitivity.  Drawing is explicitly disabled.
;;; string-bbox must be drawn in window coordinates in order for the font metrics to be meaningful.
(defmethod draw-string-bbox ((object basic-gl-object) view string font)
  (with-gl-window-transform
    (gui::maybe-set-gl-pick-matrix)
    (mv-bind (w h dx dy) (gl::string-raster-size string font)
      (bind-vector-elements (x y z) 
	  (transform-vector (transforms::object-to-view-transform object view)
			    (perturbed-object-origin object))
	(decf x dx) (decf y (+ dy 2))
	;;(format t "draw-string-bbox ~a ~a ~%" object (list x y z w h))
	(glPushName 0)
	(glLoadName 0)
	(glColorMask 0 0 0 0)
	(glBegin GL_POLYGON)
	;(glBegin GL_LINE_LOOP)
	(glVertex3d x y z)
	(glVertex3d (+ x w) y z)
	(glVertex3d (+ x w) (- y h) z)
	(glVertex3d x (- y h) z)
	(glEnd)
	(glColorMask 1 1 1 0)
	(glPopName)))))

(defmethod glDrawString-with-color
	   ((object basic-gl-object) text position font color)
  (when text
;;    (when color (glColornfv color))
    (when color (glColorndv color))
    ;(format t ".")
    (glDrawString text position :font font))      
  nil)

(defmethod draw-object-label ((object basic-gl-object) object-graphics-style highlight-graphics-style)
  (declare (ignorable object-graphics-style highlight-graphics-style))
  (when t				; *enable-object-labels*
    ;;(format t "draw-object-label ~a~%" object)
    (when (name object)
      (let ((graphics-style *default-gl-object-label-graphics-style*))	  
	(glDrawString-with-color object (name object) (perturbed-object-origin object) 
				 (font graphics-style) (color-vector graphics-style))))))

#+never ; moved into the guts of draw-object-around
(defmethod draw-object-around :after ((object basic-gl-object) view &optional force-graphics-style selectids)
  (declare (ignore view selectids))
  ;;(format t "~%Object: ~a ; Objfrag: ~a" object (obj::fragment object))
  (when *enable-object-labels*
    (draw-object-label object force-graphics-style nil)
    nil))


;(pcl::undefmethod draw-fragments :after (basic-gl-object))
;;; This doesn't work right because the wrong select-id is used and
;;; there is no highlight method for the label.  The label needs to be made into
;;; a valid fragment.


(defstruct-class gl-label (no-vertices-mixin)
  ((text :initform "Label" :initarg :text :accessor gl-label-text)))

(defstruct-class 3d-text-object (gl-3d-object-mixin gl-label) ()
		 (:default-initargs :text "3d Text"))

(defstruct-class 2d-text-object (gl-2d-object-mixin gl-label) ()
		 (:default-initargs :text "2d Text"))

(defmethod initialize-instance :after ((obj gl-label) &key &allow-other-keys)
  (setf (get-prop obj :immediate-render-p) t)
  ;;(setf (get-prop obj :suppress-label) t) ; what is this?
  )

#+unused ; ?
(defmethod font ((object gl-label))
  (let ((object-graphics-style (object-graphics-style object gui::*current-view*)))
    (or (and object-graphics-style (font object-graphics-style))
	(font *default-gl-label-graphics-style*))))

(defmethod draw-object-label ((object gl-label) object-graphics-style highlight-graphics-style)
  (let ((graphics-style (or object-graphics-style *default-gl-label-graphics-style*)))
    (glDrawString-with-color object (gl-label-text object) (perturbed-object-origin object) 
			     (font graphics-style) 
			     (color-vector (or highlight-graphics-style graphics-style)))))

(defmethod draw-bbox ((object gl-label) view)
  (let* ((graphics-style (or (object-graphics-style object view) *default-gl-label-graphics-style*)))
    (draw-string-bbox object view (gl-label-text object) (font graphics-style))))

(defmethod draw-fragments ((object gl-label))
  (draw-bbox object gui::*current-view*)) ; for mouse sensitivity

(defmethod draw-object ((object gl-label) view)
  (declare (ignore view))
  (let ((graphics-style (or *force-graphics-style* (object-graphics-style object view))))
    (draw-object-label object graphics-style nil)
    ;; DRAW-BBOX to enable mouse sensitivity.  This is required because text
    ;; display (glutBitmapCharacter) drawing puts nothing in the select-buffer
    ;; used for picking, unlike objects that draw points, lines and/or polygons.
    (draw-bbox object view)		; for mouse sensitivity 
    ))

#|

;(pcl::undefmethod draw-fragments (gl-label))

(setf (gl-label-text (gui::selected-object)) "3D Text")

;(gl::string-raster-size "Label" (font *default-highlight-label-graphics-style*))


(defun tst-draw-fragments ()
  (let ((gui::*current-view* (top-view)))
    (glMakeCurrent (gui::view-window gui::*current-view*))
    (glDrawBuffer GL_FRONT)
    (draw-fragments (gui::selected-object))
    (glFinish)))

(tst-draw-fragments)
(gui::redisplay (top-view))
|#

