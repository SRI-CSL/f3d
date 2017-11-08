(in-package :obj)

;;; :OBJECT-SETS-CONTAIN-COMPOSITE-CHILDREN must be defined.  
;;; composite-objects are currently broken otherwise.
;;; Do not remove :OBJECT-SETS-CONTAIN-COMPOSITE-CHILDREN from *FEATURES*.
(eval-when (eval load compile)
  (pushnew :object-sets-contain-composite-children *features*))

#+never
(defparameter *default-draw-3d-bbox-mixin-graphics-style*
  (make-instance 'gl-graphics-style
		 :color :cyan  ; cyan
		 :dash-style '(1 3)
		 ))

(defparameter *default-draw-3d-bbox-mixin-graphics-style*
  (make-instance 'gl-graphics-style
		 :color-vector (math::fv 0.0f0 1.0f0 1.0f0 .3f0)  ; transparent cyan
		 ))	

;(setf (color-vector *default-draw-3d-bbox-mixin-graphics-style*) (gl::fv 0.0f0 1.0f0 1.0f0 .3f0))
;(setf (dash-style *default-draw-3d-bbox-mixin-graphics-style*) nil)	

(defparameter *draw-3d-bbox-mixin-array*
  (make-vertex-array (cv -.5 -.5 -.5)
		     (cv .5 -.5 -.5)
		     (cv .5 .5 -.5)
		     (cv -.5 .5 -.5)
		     (cv -.5 -.5 .5)
		     (cv .5 -.5 .5)
		     (cv .5 .5 .5)
		     (cv -.5 .5 .5)))
  
(defstruct-class draw-3d-bbox-mixin (gl-object)
  ((quad-strip-indices :initform nil :initarg :quad-strip-indices
		       :accessor quad-strip-indices)
   (other-polygon-indices :initform nil :initarg :other-polygon-indices
			  :accessor other-polygon-indices))
  (:default-initargs 
      :vertex-array (copy-matrix *draw-3d-bbox-mixin-array*)
    :quad-strip-indices (load-time-value (make-vertex-index-array '(0 4 3 7 2 6 1 5 0 4))) ; ccw order
    :other-polygon-indices (load-time-value (loop for indices in '((4 5 6 7) (3 2 1 0)) 
						  collect (make-vertex-index-array indices)))
    :graphics-style *default-draw-3d-bbox-mixin-graphics-style*
    ))

(defmethod compute-vertices ((object draw-3d-bbox-mixin))
  (declare (optimize (speed 3) (safety 1)))
  (let* ((verts (vertex-array object))
	 (default-verts *draw-3d-bbox-mixin-array*))
    (declare (type vertex-array-type verts default-verts))
    (bind-vector-elements (xmin xmax ymin ymax zmin zmax) (bounding-box object)
      (let* ((xs (- xmax xmin)) (ys (- ymax ymin)) (zs (- zmax zmin))
	     (xo (* .5 (+ xmax xmin))) (yo (* .5 (+ ymax ymin))) (zo (* .5 (+ zmax zmin))))
	(declare (double-float xs ys zs xo yo zo))
	(loop for i fixnum from 0 below (array-dimension verts 0)
	      do (setf (aref verts i 0) (+ (* xs (aref default-verts i 0)) xo)
		       (aref verts i 1) (+ (* ys (aref default-verts i 1)) yo)
		       (aref verts i 2) (+ (* zs (aref default-verts i 2)) zo)))))))
  
(defun draw-+ (&optional (size .5))
  (glBegin GL_LINES)
	     (glVertex3d (- size) 0.0 0.0) 
	     (glVertex3d size 0.0 0.0) 
	     (glVertex3d 0.0 (- size) 0.0) 
	     (glVertex3d 0.0 size 0.0) 
	     (glEnd))

;;; This also controls object sensitivity
(defparameter *draw-3d-bbox-mixin-always-draw-p* t)
;;(setq *draw-3d-bbox-mixin-always-draw-p* nil)

(defmethod draw-object-p ((object draw-3d-bbox-mixin) view)
  (declare (ignorable view))
  (or *draw-3d-bbox-mixin-always-draw-p*
      (eq object (gui::selected-object)) ; this probably needs to be smarter
      (get-prop object :draw-object-p)))

(defvar *draw-bounding-boxes* nil)

(defmethod draw-object ((object draw-3d-bbox-mixin) view)
  (declare (ignorable view))
  (when (and (draw-object-p object view) *draw-bounding-boxes*)
    (let* ((verts (vertex-array object))
	   (indices  (quad-strip-indices object))
	   (n-quad-strip-faces (1- (/ (length indices) 2))))
      (glDrawquad_strip_indexed verts n-quad-strip-faces indices)
      (loop for poly in (other-polygon-indices object)
	    do (draw_polygon verts poly (length poly) nil 0)))))

;;; ****************************   COMPOSITE-OBJECT   ****************************

;;; FIXME:  Dumb implementation of composite-objects.
;;; It is merely a hack to place the children of composite-objects into object-sets.
;;; No composite interaction behavior is supported, and the interaction
;;; behavior of the children is broken.

(defstruct-class COMPOSITE-OBJECT (basic-gl-object) ())

(defstruct-class 3D-COMPOSITE-OBJECT (COMPOSITE-OBJECT draw-3d-bbox-mixin gl-3d-object-mixin) ())

(defstruct-class 2D-COMPOSITE-OBJECT (COMPOSITE-OBJECT gl-2d-object-mixin) ())

(defmethod all-children ((obj COMPOSITE-OBJECT))
  (let ((result nil))
    (map-over-children (child obj)
      (push child result)
      (when (typep child 'COMPOSITE-OBJECT)
	(setf result (nconc (all-children child) result))))
    result))

#+never ; problems here because basic-coordinate-system initialize-instance calls connect-transforms
(defmethod (setf children) :after (new-childen (obj COMPOSITE-OBJECT))
  (compute-vertices obj))

(defmethod bounding-box ((obj COMPOSITE-OBJECT) &key bbox transform)
  (map-over-children (child obj)
      (setq bbox (bounding-box child
			       :bbox bbox 
			       :transform (cons (object-to-parent-transform child) transform)
			       )))
  bbox)
#|
(obj::bounding-box (gui::selected-object))
(loop for obj in (children (gui::selected-object)) 
      collect (obj::bounding-box obj :transform (object-to-parent-transform obj)))
|#
(defmethod update-object-before ((obj COMPOSITE-OBJECT))
  ;;(format t "update-object-before ~a~%" obj)
  (map-over-children (child obj)
	(update-object-before child)))

(defmethod update-object-after ((obj COMPOSITE-OBJECT))
  ;(format t "update-object-after ~a~%" obj)
  (unless *update-object-after-recursion*
    (map-over-children (child obj)
      (update-object-after child)))
  (compute-vertices obj)
  )

(defmethod initialize-instance :after ((composite composite-object) 
				       &key object-to-world-transform inferiors &allow-other-keys)
  (when inferiors (setf (children composite) inferiors))
  (when object-to-world-transform
    (loop with world-to-composite-transform 
	  ;; = (inverse-transform (object-to-world-transform composite))
	    = (inverse-transform object-to-world-transform)
	  for child in (children composite)
	  do (setf (object-to-parent-transform child)
		   (compose-transforms (object-to-parent-transform child) world-to-composite-transform))
	     (setf (parent child) composite)	       
	  ))
   (compute-vertices composite)
  )


;;; Many references to the GUI package

;;; Perhaps these methods should be in some file in the basic-gui subsystem.

#-object-sets-contain-composite-children
(defmethod gui::draw-object-around ((object obj::composite-object) view &optional force-graphics-style
				    (selectids gui::*draw-objects-with-selectid*))
  ;(format t "gui::draw-object-around ~a~%" object)
  (obj::with-gl-object-drawing (object view nil)
    ;; allow composites that have their own drawing methods
    (call-next-method object view force-graphics-style selectids)
    (let ((obj::*gl-object-drawing-superior* object))
      (loop for c in (obj::children object)
	    do ;(format t "gui::draw-object-around child=~a~%" c)
	       (gui::draw-object-around c view force-graphics-style selectids))
      )))

(in-package :gui)

#+object-sets-contain-composite-children
(progn
  ;; FIXME? These methods cause the leaf objects to be placed in the object-set.
  ;;        Will this be a problem for being able to manipulate composite-objects?
  
(defmethod add-object :after ((object obj::composite-object) (object-set object-set))
  (map-over-children (child object)
    (add-object child object-set)))

(defmethod gui::remove-object :after ((object obj::composite-object) (object-set object-set))
  ;;(format t "obj::remove-object ~a~%" object)
  (map-over-children (child object)
    (remove-object child object-set)))

) ; end progn


(defmethod add-object ((object t) (composite obj::COMPOSITE-OBJECT))
  (when (transforms::inferior-of-p composite object)
    ;; Must be sure that composite is not an inferior of object
    (error "Attempt to add an inferior which is also my superior"))
  (setf (find-object-named (world composite) (name object)) object)
  (add-child composite object)
  #+object-sets-contain-composite-children
  (loop for fs in (object-feature-sets composite)
	do (add-object object fs))
  (obj::compute-vertices object)
  )
 
(defmethod remove-object ((object t) (composite obj::COMPOSITE-OBJECT))
  (remove-child composite object)
  #+object-sets-contain-composite-children
  (loop for fs in (object-feature-sets composite)
	do (remove-object object fs))
  (obj::compute-vertices object)
  )

(defmethod select-parent ((object basic-gl-object)))

#| unfinished
(defmethod select-parent ((object obj::COMPOSITE-OBJECT))
  (setf (selected-objects) (list )
|#

#|
(setq *composite* (parent (gui::selected-object)))
(transforms::map-over-children (obj *composite*) (describe obj))

(setq *obj* (make-instance 'axis-object :parent *composite* 
			   :object-to-parent-transform 
			   (make-4x4-coordinate-transform
			    (copy-matrix (transform-matrix
					  (object-to-parent-transform 
					   (first (children *composite*))))))))

(setq *composite* (parent (gui::selected-object)))

(setq *obj* (make-instance 'axis-object :parent *composite* 
			   :object-to-parent-transform 
			   (make-4x4-coordinate-transform (make-4x4-identity-matrix))))

(setq *comp2* (make-instance '3d-composite-object :parent *composite* 
			     :object-to-parent-transform 
			     (make-4x4-coordinate-transform  
			      (make-object-to-parent-matrix (cv 100.0 100.0 0.0)))))

(setq *obj2a* (make-instance 'obj::axis-object :parent *comp2*
			     :object-to-parent-transform 
			     (make-4x4-coordinate-transform 
			      (make-object-to-parent-matrix (cv 100.0 100.0 0.0))))
      *obj2b* (make-instance 'obj::cube-object :parent *comp2*
			     :object-to-parent-transform 
			     (make-4x4-coordinate-transform 
			      (make-object-to-parent-matrix (cv -100.0 100.0 0.0)))))

(member *obj2a* (children (gui::3d-world (gui::top-view))))
(loop for obj in (list *comp2* *obj2a* *obj2b*)
      do (gui::add-object obj (first (object-feature-sets *composite*))))

(gui::add-object *obj* (first (object-feature-sets *composite*)))
(gui::remove-object *obj* (first (object-feature-sets *composite*)))

(typep *composite* 'obj::3d-composite-object)
(typep *composite* 'obj::draw-3d-bbox-mixin)
(obj::compute-vertices *composite*)
(math::print-matrix (obj::vertex-array *composite*))
(describe *composite*)
(math::print-matrix obj::*draw-3d-bbox-mixin-array*)		
(gui::map-over-object-set (obj (first (object-feature-sets *composite*)))
  (when (eq obj *composite*) (print "win")))
(gui::map-over-object-set (obj (first (object-feature-sets *composite*)))
  (print obj))

(setf (obj::graphics-style *composite*) obj::*default-draw-3d-bbox-mixin-graphics-style*)

*default-draw-3d-bbox-mixin-graphics-style*

(obj::center-vertex *obj*)

(obj::bounding-box *composite*)
(pcl::compute-applicable-methods #'obj::bounding-box  (list *composite*))
(compute-applicable-methods #'update-object-after (list *obj2b*))

(children *composite*)
(describe *composite*)
(describe (nth 0 (obj::other-polygon-indices *composite*)))
(math::print-matrix (obj::vertex-array *composite*))
(obj::bounding-box (make-instance 'obj::3d-composite-object))
(obj::bounding-box *obj*)
(trace obj::bounding-box)
(untrace)
(describe (object-graphics-style *composite* nil))
(describe (object-graphics-style (first (children *composite*)) nil))
(describe (object-graphics-style (selected-object) nil))
(math::print-matrix (transform-matrix (object-to-parent-transform *composite*)))
(math::print-matrix (transform-matrix (object-to-parent-transform (first (children *composite*)))))

(selected-objects)
|#



(eval-when (eval load compile)
  (setf *features* (remove :object-sets-contain-composite-children *features*)))
