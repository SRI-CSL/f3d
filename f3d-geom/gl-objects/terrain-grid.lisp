(in-package :obj)

(defstruct-class terrain-grid (gl-xyz-sizable-object-mixin)
  ((nx :initform 1 :initarg :nx :accessor nx)
   (ny :initform 1 :initarg :ny :accessor ny)))

#+never
(defmethod initialize-instance :after ((object terrain-grid) &key &allow-other-keys)
  )

;;; This implementation sucks!  Should use GL_QUAD_STRIP rather than GL_LINE_STRIP.
#+sucks
(defmethod draw-object ((object terrain-grid) view)
  (declare (ignorable view))
  ;#+never
  (format t "draw-object terrain-grid~%")
  (with-class-slot-values terrain-grid (nx ny) object
    (declare (fixnum nx ny))
    (let ((verts (vertex-array object))
	  (vert (cv 0.0 0.0 0.0)))
      (declare (type vertex-array-type verts))
      (glPushAttrib GL_ALL_ATTRIB_BITS)
      (glEnable GL_LINE_STIPPLE)
      (glLineStipple 1 (case 4
			 (1 #x5555)
			 (2 #x9999)
			 (4 #x1111)
			 (8 #x0101)
			 (16 1)
			 (otherwise #x1111)))
      (loop for v fixnum from 0 below ny
	    do (glBegin GL_LINE_STRIP)
	       (loop for u fixnum from 0 below nx
		     do (glVertex3dv (vertex-array-vertex verts (+ (* v nx) u))))
	       (glEnd))
      (loop for u fixnum from 0 below nx
	    do (glBegin GL_LINE_STRIP)
	       (loop for v fixnum from 0 below ny
		     do (glVertex3dv (vertex-array-vertex verts (+ (* v nx) u))))
	       (glEnd))
      (glPopAttrib))))

(defmethod compute-terrain-grid-quadstrip-indices ((object terrain-grid) indices v nx)
  (loop for k fixnum from 0 below nx
	for j fixnum from 0 by 2
	for i fixnum from (* v nx) 
	do (setf (aref indices j) i 
		 (aref indices (1+ j)) (+ i nx)))
  indices)


#-sucks
(defmethod draw-object ((object terrain-grid) view)
  (declare (ignorable view))
  ;#+never
  (format t "draw-object terrain-grid~%")
  (with-class-slot-values terrain-grid (nx ny) object
    (declare (fixnum nx ny))
    (let ((verts (vertex-array object))
	  (vert (cv 0.0 0.0 0.0))
	  (indices (make-vertex-index-array (* 2 nx))))
      (declare (type vertex-array-type verts))
      (glPushAttrib GL_ALL_ATTRIB_BITS)
      (glEnable GL_LINE_STIPPLE)
      (glLineStipple 1 (case 4
			 (1 #x5555)
			 (2 #x9999)
			 (4 #x1111)
			 (8 #x0101)
			 (16 1)
			 (otherwise #x1111)))
      (loop for v fixnum from 0 below (1- ny)
	    do (glDrawquad_strip_indexed verts (1- nx) 
			  (compute-terrain-grid-quadstrip-indices object indices v nx)))	    
      (glPopAttrib))))

;;;(defun add-terrain-grid-to-view (view terrain-model)
;;;  (mv-bind (verts nx ny) (terrain-grid-vertices-from-terrain-model terrain-model)
;;;    (let* ((3d-world (3d-world view))
;;;           (terrain-grid (make-instance
;;;                          'terrain-grid
;;;                          :object-to-parent-transform (make-4x4-coordinate-transform 
;;;                                                       (make-4x4-identity-matrix))
;;;                          :vertex-array verts
;;;                          :nx nx :ny ny))
;;;           (object-set (gui::make-object-set (list terrain-grid) :world 3d-world)))
;;;      (push object-set (gui::object-sets view)))))

(defun make-terrain-grid (terrain-model dx dy)
  (mv-bind (verts nx ny) (terrain-grid-vertices-from-terrain-model terrain-model dx dy)
    (let* ((dtm-to-lvcs-transform (cme::dtm-to-lvcs-transform terrain-model))
	   (terrain-grid (make-instance
			  'terrain-grid
			  :parent (to-coordinate-system dtm-to-lvcs-transform)
			  :object-to-parent-transform (make-4x4-coordinate-transform 
						       (make-4x4-identity-matrix))
			  :vertex-array verts
			  :nx nx :ny ny)))
      terrain-grid)))

(defun terrain-grid-vertices-from-terrain-model (terrain-model &optional (dx 1) (dy 1))
  (let* ((dtm-image (gui::dtm-image terrain-model))
         (dtm-to-lvcs-transform (cme::dtm-to-lvcs-transform terrain-model))
         (nx (floor (img::image-x-dim dtm-image) dx))
         (ny (floor (img::image-y-dim dtm-image) dy))
         (vertex-array (make-array (list (* nx ny) 3)
                                   :element-type 'vertex-element-type)))
    (declare (type vertex-array-type vertex-array))
    (loop with vi fixnum = 0
          for dtm-y fixnum from 0 by dy
	  repeat ny
          do (loop for dtm-x fixnum from 0 by dx
		   repeat nx
                   for dtm-pt = (cv (dfloat dtm-x) (dfloat dtm-y) (img::diref dtm-image dtm-x dtm-y))
                   for lvcs-pt = (transform-vector dtm-to-lvcs-transform dtm-pt)
                   do (bind-vector-elements (x y z) (transform-vector dtm-to-lvcs-transform dtm-pt)
                        (set-vertex-array-elements vertex-array vi x y z))
                      (incf vi)))
    (values vertex-array nx ny)))


#|
(st:load-system :terrain-models)

(maybe-compile-file-load "$FREEDIUS/lisp/globj/terrain-grid.lisp")

(setq alv-dtm (img::load-image "$RADIUS/sites/alv/alv-dtm.g0"))

(transforms::dtm-min-max-images (cme::dtm (get-prop (3d-world (gui::top-view)) :terrain-model))) => 20 21
(let ((tm (get-prop (3d-world (gui::top-view)) :terrain-model)))
  (terrain-grid-vertices-from-terrain-model tm 20 21))

(setq alv-dtm-to-lvcs-transform (make-4x4-coordinate-transform
				 (make-and-fill-4x4-matrix
				  98.49512 -.13215083 -.13852036 -7890.5884
				  0.13259056 98.494644 0.15808614 176.01
				  0.11577392 -0.37007928 98.42377 4.989898
				  0.0 0.0 0.0 1.0)))

(add-terrain-grid-to-view (top-view (selected-window)) alv-dtm alv-dtm-to-lvcs-transform)

(let ((tm (gui::find-terrain-model (gui::top-view))))
  
(pop (object-sets (top-view)))

(let* ((view (gui::top-view))
       (world (3d-world view))
       (tm (get-prop world :terrain-model)))
  (when tm 
    (mv-bind (dtm-min dtm-max xs ys) (transforms::dtm-min-max-images (cme::dtm tm))
      (cme::add-object (setq *tg* (make-terrain-grid tm xs ys))
		       (setq *fs* (make-instance 'cme::3d-feature-set :world world))))))


(let* ((view (gui::top-view))
       (world (3d-world view))
       (tm (get-prop world :terrain-model)))
  (when tm 
    (mv-bind (dtm-min dtm-max xs ys) (transforms::dtm-min-max-images (cme::dtm tm))
      (cme::add-object (setq *tg* (make-terrain-grid tm 1 1))
		       (setq *fs* (make-instance 'cme::3d-feature-set :world world))))))


(cme::feature-sets (3d-world (gui::top-view)))
(setf (cme::feature-sets (3d-world (gui::top-view)))
      (butlast (cme::feature-sets (3d-world (gui::top-view)))))

(vertex-array-vertex (vertex-array (car (cme::inferiors *fs*))) 75)
(describe (car (cme::inferiors *fs*)))
(setq *print-array* nil)
|#




 
