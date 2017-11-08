(in-package :obj)

(defstruct-class 2d-object (gl-2d-object-mixin basic-gl-object) ())

(defstruct-class gl-xy-sizable-object-mixin (gl-sizable-object-mixin gl-2d-object-mixin)
    ((sizes :initform (cv 1.0 1.0) :initarg :sizes :accessor sizes)))


(defstruct-class 2d-crosshair-object (gl-2d-object-mixin crosshair-object) ())

(defstruct-class 2d-point-object (gl-2d-object-mixin point-object) ())

(defmethod short-name-string ((object 2d-point-object))
  "2d Point")

(defstruct-class 2D-CURVE (gl-2d-object-mixin curve) ())

(defmethod vertex-array-elements-per-vertex ((object 2D-CURVE)) 3)

(defstruct-class 2D-closed-CURVE (2D-CURVE) ()
  (:default-initargs :closed-p t))

(defstruct-class 2D-RIBBON-CURVE (gl-2d-object-mixin ribbon) ())
(defmethod vertex-array-elements-per-vertex ((object 2D-RIBBON-CURVE)) 4)

;; FIXME - just a temporary hack
(defstruct-class 2d-ruler-object (2d-curve) ())
(defmethod ruler-length ((o 2d-ruler-object))
  (let* ((verts (vertex-array o)))
    (loop for i from 0 below (1- (array-dimension verts 0))
	  sum (vector-euclidean-length (g- (obj::vertex-array-vertex verts (1+ i)) (obj::vertex-array-vertex verts i))))))

(defstruct-class 2D-rectangle (gl-xy-sizable-object-mixin rectangle-object) ())

(defmethod draw-fragments ((object 2d-rectangle))
  ;;(format t "draw-fragments ~a~%" object)
  (let* ((verts (vertex-array object)))
    (declare (type (or null vertex-array-type) verts))
    (when verts
      (with-object-transform object
	(glPushName 0)
	(glPointSize 1.0)
	(loop for i of-type fixnum from 0 below (array-dimension verts 0)
	      do (glLoadName i)
		 (glBegin GL_POINTS)
		 (glVertex3d (aref verts i 0) (aref verts i 1) (aref verts i 2))
		 (glEnd))
	  
	(glPopName))))
  )
