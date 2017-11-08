(in-package :gui)

#|
(maybe-compile-file-load "$FREEDIUS/lisp/globj/object-ops.lisp")
|#

;;; This really belongs elsewhere with tk specific stuff.
(defun gui-usage-error (format-string &rest args)
  (let* ((string (apply 'format nil format-string args))
	 ;(window (selected-window))
	 (top-level (tk::gensym-toplevel-widget-path ".error"))
	 (msg (tk::merge-widget-pathname top-level "msg"))
	 (button (tk::merge-widget-pathname top-level "but"))
	 (padding (and (not (member :THEMED config::*tk-features*)) '(:padx 20 :pady 20))))
    (tk::tcl-script `((qtoplevel ,top-level)
		      (qlabel ,msg :text ,string :wraplength 300 :justify left ,@padding)
		      (wm title ,top-level "Error")
		      (qbutton ,button :text "OK" :command '(destroy ,top-level) :width 20)
		      (qgrid_1_column ,top-level)
		      ))
    (tk::do-events) ; widget must be instantiated before width and height are meaningful
    ;(tk::set-toplevel-widget-position top-level (widget *CME-CONTROL-PANEL*))
    (tk::set-toplevel-widget-position top-level :mouse-screen-center)
    nil))


;(gui-usage-error "Foobar")

;;; This really belongs in a differnt file.
;;; This is redefined by terrain-models.lisp
(defmethod intersect-camera-ray-with-terrain-model (terrain-model projection 2d-position
								  &optional intersection-estimate)
  (declare (ignore terrain-model))
  (let* ((3d-world (from-coordinate-system projection))
	 (default-z (or (and 3d-world (get-prop 3d-world :default-z)) 0.0)))
    (TRANSFORMS::INTERSECT-CAMERA-RAY-WITH-Z-PLANE projection 2d-position default-z
						   intersection-estimate)))


;;; *************************  CREATE-OBJECT *************************

(declaim (special *create-object*))

;(fmakunbound 'create-and-add-object)


;;; crap, Allegro doesn't support (make-instance (class-name (class-of class-proto)))
;(class-name (class-of (class-prototype (find-class 'obj::3d-crosshair-object))))

(defun gui-create-object (class-name)
  (mv-bind (window window-pos) 
      (pick-a-window "Pick a Window and Position for New Object")
    (let* ((view (top-view window)))
      (when view
	(set-state *interactor* window window-pos)
	(create-and-add-object (get-class-prototype class-name) view window-pos)))))

;(selected-feature-set (3d-world (top-view)))

(defmethod create-and-add-object-guts ((class-proto gl-3d-object-mixin) view window-pos)
  (let* ((world (3d-world view))
	 (selected-feature-set (and world (selected-feature-set world))))
    (cond ((null world)
	   (gui-usage-error "Cannot add a 3d-object to a view that has no 3d-world")
	   nil)
	  ((null selected-feature-set)
	   (select-view (make-feature-set-configuration-panel :3d-p t) view)
	   (gui-usage-error "No feature-set is selected in ~a" world)
	   nil)
	  (t (let* ((2d-position (inverse-transform-vector (2d-to-window-transform view) window-pos))
		    (3d-position (intersect-camera-ray-with-terrain-model (find-terrain-model view)
									  (3d-to-2d-projection view)
									  2d-position))
		    (object (make-instance (class-name (class-of class-proto))
					   :parent world
					   :object-to-parent-transform 
					   (make-4x4-coordinate-transform 
					    (math::make-object-to-parent-matrix 3d-position)))))
	       (setq *create-object* (list object 3d-position))
	       (add-object object selected-feature-set)
	       object)))))

(defmethod create-and-add-object-guts ((class-proto gl-2d-object-mixin) view window-pos)
  (let* ((world (2d-world view))
	 (selected-feature-set (and world (selected-feature-set world))))
    (cond ((null world)
	   (gui-usage-error "Cannot add a 2d-object to a view that has no 3d-world")
	   nil)
	  ((null selected-feature-set)
	   (select-view (make-feature-set-configuration-panel :3d-p t) view)
	   (gui-usage-error "No feature-set is selected in ~a" world)
	   nil)
	  (t (let* ((2d-position (inverse-transform-vector (2d-to-window-transform view) window-pos))
		    (object (make-instance (class-name (class-of class-proto))
					   :parent world
					   :object-to-parent-transform 
					   (make-4x4-coordinate-transform 
					    (math::make-object-to-parent-matrix 2d-position)))))
	       (setq *create-object* (list object 2d-position))
	       (add-object object selected-feature-set)
	       object)))))


;(fmakunbound 'create-and-add-object)
(defmethod create-and-add-object ((class-proto basic-gl-object) view window-pos)
  (let ((object (create-and-add-object-guts class-proto view window-pos)))
    (when object
      (redisplay-all-world-views *interactor* (world object))
      (setf (selected-objects) 
	    `((,object ,(make-instance 'object-vertex :object object :vertex-id 0))))
      (start-popup-object-drag 'move-object-uv)
      )))

(defmethod create-and-add-object ((class-proto obj::basic-curve) view window-pos)
  (let ((object (create-and-add-object-guts class-proto view window-pos)))
    (when object
      (redisplay-all-world-views *interactor* (world object))
      (setf (selected-objects) 
	    `((,object ,(make-instance 'object-vertex :object object :vertex-id 1))))
      (start-popup-object-drag 'move-object-vertex-uv)
      )))


;; I think we want this for 2d-rectangles &such:
(defmethod create-and-add-object ((class-proto obj::gl-xy-sizable-object-mixin) view window-pos)
  (let ((object (create-and-add-object-guts class-proto view window-pos)))
    (when object
      (redisplay-all-world-views *interactor* (world object))
      (setf (selected-objects) 
	    `((,object ,(make-instance 'object-vertex :object object :vertex-id 3))))
      ;; Not sure which of these makes more sense - for 2d selection, I vote for #2:
      ;;(start-popup-object-drag 'rotate-resize-object-xy)
      (start-popup-object-drag 'resize-object-xy)
      )))


(defmethod create-and-add-object ((class-proto obj::gl-xyz-sizable-object-mixin) view window-pos)
  (let ((object (create-and-add-object-guts class-proto view window-pos)))
    (when object
      (redisplay-all-world-views *interactor* (world object))
      (setf (selected-objects) 
	    `((,object ,(make-instance 'object-vertex :object object :vertex-id 1))))
      (start-popup-object-drag 'rotate-resize-object-xy)
      )))

#|
 
(defmethod create-and-add-object ((class-proto gl-3d-object-mixin) view window-pos)
  (let ((object (create-and-add-object-guts class-proto view window-pos)))
    (when object
      (redisplay-all-world-views *interactor* (3d-world view))
      (setf (selected-objects) 
	    `((,object ,(make-instance 'object-vertex :object object :vertex-id 0))))
      (start-popup-object-drag 'move-object-uv)
      )))
 
(defmethod create-and-add-object ((class-proto gl-2d-object-mixin) view window-pos)
  (let ((object (create-and-add-object-guts class-proto view window-pos)))
    (when object
      (redisplay-all-world-views *interactor* (2d-world view))
      (setf (selected-objects) 
	    `((,object ,(make-instance 'object-vertex :object object :vertex-id 0))))
      (start-popup-object-drag 'move-object-uv)
      )))
 
;(pcl::undefmethod create-and-add-object (3d-curve t t))
(defmethod create-and-add-object ((class-proto 3d-curve) view window-pos)
  (let ((object (create-and-add-object-guts class-proto view window-pos)))
    (when object
      (redisplay-all-world-views *interactor* (3d-world view))
      (setf (selected-objects) 
	    `((,object ,(make-instance 'object-vertex :object object :vertex-id 1))))
      (start-popup-object-drag 'move-object-vertex-uv)
      )))

(defmethod create-and-add-object ((class-proto obj::basic-curve) view window-pos)
  (let ((object (create-and-add-object-guts class-proto view window-pos)))
    (when object
      (redisplay-all-world-views *interactor* (world object))
      (setf (selected-objects) 
	    `((,object ,(make-instance 'object-vertex :object object :vertex-id 1))))
      (start-popup-object-drag 'move-object-vertex-uv)
      )))
 
(defmethod create-and-add-object ((class-proto obj::gl-xyz-sizable-object-mixin) view window-pos)
  (let ((object (create-and-add-object-guts class-proto view window-pos)))
    (when object
      (redisplay-all-world-views *interactor* (3d-world view))
      (setf (selected-objects) 
	    `((,object ,(make-instance 'object-vertex :object object :vertex-id 1))))
      (start-popup-object-drag 'rotate-resize-object-xy)
      )))
|# 


;;; Note this this does not do everything that REMOVE-SELF does.  It does not
;;; remove the object from its parent.  Perhaps it should add the object to the
;;; deleted-objects-feature-set of the world, as is done in CME.
(defmethod com-delete-object ((interactor interactor))
  (let* ((object (selected-object))
	 (world (world object)))
    ;; Remove it from all feature-sets
    (loop for fs in (object-feature-sets object)
	  do (remove-object object fs))
    ;; I guess it is ok to leave it selected just in case there is some menu command
    ;; to cause it to be added to some other feature-set.
    ;; (setf (selected-objects) nil)
    
    ;; Update the display state
    (redisplay-all-world-views interactor world)
    ;;(map-over-all-world-views (world view)(view-changed (view-window view)))
    ))


#|
(member (selected-object) (children (car (object-sets (top-view)))))
|#



(in-package :obj)

;; The same generic function is also applicable to network objects
(defmethod map-over-all-arcs ((object basic-curve) fn)
  (let* ((verts (vertex-array object))
	 (closed-p (closed-p object)))
    (declare (type (or null vertex-array-type) verts))
    (loop with n fixnum = (array-dimension verts 0)
	  with end-index fixnum = (if closed-p (1- n) (- n 2))
	  for i fixnum from 0 to end-index
	  for i+1 fixnum = (mod (1+ i) n)
	  do (funcall fn i i+1 ))))

(defmethod add-vertex-internal ((object basic-curve) before-vertex-index obj-pos)
  (declare (fixnum before-vertex-index))
  (declare (type (coordinate-vector *) obj-pos))
  (let* ((verts (vertex-array object))
	 (nverts (1+ (array-dimension verts 0)))
	 (ncomps (array-dimension verts 1))
	 (n-comps-obj-pos (length obj-pos))
	 (bvi before-vertex-index)
	 ;;(closed-p (closed-p object))
	 (new-verts (resize-vertex-array verts nverts)))
    (declare (type vertex-array-type verts new-verts))
    (declare (fixnum nverts ncomps bvi n-comps-obj-pos))
    (declare (optimize (speed 3) (safety 0)))
    ;(format t "add-vertex-internal ~a ~a ~a~%" object before-vertex-index nverts)
    ;(break)
    (copy-vertex-subarray verts new-verts 0 0 bvi)

    ;; Copy all components:
    (loop for i fixnum from 0 below n-comps-obj-pos
	  do (setf (aref new-verts bvi i) (aref obj-pos i)))
    (when (> ncomps n-comps-obj-pos)
      (loop with nbvi = (if (= bvi 0) 0 (1- bvi)) ; inherit components from nearest vertex
	    for i fixnum from n-comps-obj-pos below ncomps
	    do (setf (aref new-verts bvi i) (aref verts nbvi i))))

    ;; Copy the rest of the array:
    (copy-vertex-subarray verts new-verts bvi (1+ bvi) (- (1- nverts) bvi))
    ;;(setq *foo* (list verts new-verts before-vertex-index vert))
    (setf (%vertex-array object) new-verts)
    (flush-transform-vertices-ht object)
    ))


(defmethod delete-vertex-internal ((object basic-curve) vertex-index)
  (declare (fixnum vertex-index))
  (let* ((verts (vertex-array object))
	 (nverts (1- (array-dimension verts 0)))
	 (vi vertex-index)
	 (new-verts (resize-vertex-array verts nverts)))
    (declare (type vertex-array-type verts new-verts))
    (declare (fixnum nverts vi))
    (declare (optimize (speed 3) (safety 0)))
    (copy-vertex-subarray verts new-verts 0 0 vi)
    (copy-vertex-subarray verts new-verts (1+ vi) vi (- nverts vi))
    (setf (%vertex-array object) new-verts)
    (flush-transform-vertices-ht object)
    
    ))

;;; Delete the vertex of basic-curve specified by object-vertex.
(defmethod delete-vertex ((object basic-curve) (vert object-vertex))
  (update-object object
      (delete-vertex-internal object (vertex-id vert))))

;;; Add a new vertex to a basic-curve at the position specified by object-arc.
(defmethod add-vertex ((object basic-curve) (arc object-arc))
  (update-object object
    (with-class-slot-values object-arc (end-vertex-id) arc
      (add-vertex-internal object end-vertex-id (fragment-position arc))
      end-vertex-id)))

(defmethod add-vertex ((object basic-curve) (vertex object-vertex))
  (update-object object
    (with-class-slot-values object-vertex (vertex-id) vertex
      (when (> vertex-id 0) (incf vertex-id))
      (add-vertex-internal object vertex-id (fragment-position vertex))
      vertex-id)))

#|
(draw-object )
;;; pick an arc of a curve object
(let ((arc (cadr (car (selected-objects)))))
  (with-class-slot-values object-arc (object) arc
    (add-vertex object arc)))

(let ((vert (cadr (car (selected-objects)))))
  (with-class-slot-values object-vertex (object) vert
    (delete-vertex object vert)))

|#



(defmethod center-vertex ((object basic-gl-object))
  (cv 0.0 0.0 0.0))


(defmethod change-xy-size ((object gl-sizable-object-mixin) sel-obj-pos delta-xy-size)
  (let ((center-vertex (center-vertex object))
	(sizes (sizes object)))
    (bind-vector-elements (x-size y-size) sizes
      (bind-vector-elements (xp yp) sel-obj-pos
	(bind-vector-elements (cx cy) center-vertex
	  (bind-vector-elements (dx-size dy-size) delta-xy-size
	  #+never  (setf dx-size (* .5 dx-size) dy-size (* .5 dy-size))
	  (let ((new-x-size (+ x-size (* (signum (* (- xp cx) x-size))
					 (- dx-size) ;;(- xp dx-size)
					 )))
		(new-y-size (+ y-size (* (signum (* (- yp cy) y-size))
					 (- dy-size) ;;(- yp dy-size)
					 ))))
	    #+never
	    (format t ";; change-xy-size from (~a ~a) to (~a ~a) by (~a ~a)~%"
		    x-size y-size new-x-size new-y-size dx-size dy-size)
	    (when t
	      (scale-vertex-array object ;(vertex-array object)
				  (cv (/ new-x-size x-size)
				      (/ new-y-size y-size)
				      1.0))
	      (setf (aref sizes 0) new-x-size
		    (aref sizes 1) new-y-size))
    
	    ;; reset world position of anchor point
	    ;;(align-at-vertex object selected-position)
	    )))))))


;;;(defmethod rotate-change-xy-size ((object gl-object) sel-obj-pos object-motion)
;;;  (let ((center-vertex (center-vertex object))
;;;        (sizes (sizes object)))
;;;    (bind-vector-elements (x-size y-size) sizes
;;;      (bind-vector-elements (xp yp) sel-obj-pos
;;;        (bind-vector-elements (cx cy) center-vertex
;;;          (bind-vector-elements (dxp dyp) object-motion
;;;            (let* ((vx (+ (- cx xp) (* .5 (abs x-size) (if (plusp (- cx xp)) 1.0 -1.0))))
;;;                   (vy (+ (- cy yp) (* .5 (abs y-size) (if (plusp (- cy yp)) 1.0 -1.0))))
;;;                   ;; compute coords of point in local x-y plane diagonally
;;;                   ;; opposite w.r.t. object center
;;;                   (ocxp (+ xp vx))
;;;                   (ocyp (+ yp vy))
;;;                   (old-length (euclidean-length vx vy))
;;;                   (nocxp (+ ocxp dxp))
;;;                   (nocyp (+ ocyp dyp))
;;;                   (nvx (- nocxp xp))
;;;                   (nvy (- nocyp yp))
;;;                   (new-length (euclidean-length nvx nvy))
;;;                   (ratio (/ new-length old-length))
;;;                   (sin-d-theta0 (/ (- (* nvx vy) (* vx nvy))
;;;                                    (* old-length new-length)))
;;;                   (sin-d-theta (min 1.0 (max -1.0 sin-d-theta0)))
;;;                   )
;;;              (rotate-by object (list :z-rot (- (asin sin-d-theta)))
;;;                         (gui::selected-object-parent-position *interactor*))
;;;              (scale-vertex-array (vertex-array object)
;;;                                  (cv ratio
;;;                                      ratio
;;;                                      1.0))
;;;              (setf (aref sizes 0) (* ratio x-size)
;;;                    (aref sizes 1) (* ratio y-size))
;;;    
;;;              )))))))


(defmethod rotate-change-xy-size ((object gl-sizable-object-mixin) sel-obj-pos object-motion)
  (let ((center-vertex (center-vertex object))
	(sizes (sizes object)))
    (bind-vector-elements (x-size y-size) sizes
      (bind-vector-elements (xp yp) sel-obj-pos
	(bind-vector-elements (cx cy) center-vertex
	  (bind-vector-elements (dxp dyp) object-motion
	    (let* (;(vx (+ (- cx xp) (* .5 (abs x-size) (signum (- cx xp)))))
		   ;(vy (+ (- cy yp) (* .5 (abs y-size) (signum (- cy yp)))))
		   (vx (* 2.0 (- cx xp)))
		   (vy (* 2.0 (- cy yp)))
		   ;; compute coords of point in local x-y plane diagonally
		   ;; opposite w.r.t. object center
		   (ocxp (+ xp vx))
		   (ocyp (+ yp vy))
		   (old-length (euclidean-length vx vy))
		   (nocxp (+ ocxp dxp))
		   (nocyp (+ ocyp dyp))
		   (nvx (- nocxp xp))
		   (nvy (- nocyp yp))
		   (new-length (euclidean-length nvx nvy))
		   (ratio (/ new-length old-length))
		   (sin-d-theta0 (/ (- (* nvx vy) (* vx nvy))
				    (* old-length new-length)))
		   (sin-d-theta (min 1.0 (max -1.0 sin-d-theta0)))
		   )
	      (rotate-by object (list :z-rot (- (asin sin-d-theta)))
			 (gui::selected-object-parent-position *interactor*))
	      (scale-vertex-array object ;(vertex-array object)
				  (cv ratio
				      ratio
				      1.0))
	      (setf (aref sizes 0) (* ratio x-size)
		    (aref sizes 1) (* ratio y-size))
    
	      )))))))


(defmethod change-z-size ((object gl-sizable-object-mixin) sel-obj-pos object-motion)
  (let ((center-vertex (center-vertex object))
	(sizes (sizes object)))
    (bind-vector-elements (x-size y-size z-size) sizes
      (declare (ignore x-size y-size))
      (bind-vector-elements (xp yp zp) sel-obj-pos
	(declare (ignore xp yp))
	(bind-vector-elements (cx cy cz) center-vertex
	  (declare (ignore cx cy))
	  (bind-vector-elements (dxp dyp) object-motion
	    (let* ((dzp (gui::max-dx-dy dxp dyp))
		   (new-z-size (+ z-size (* (if (minusp (* (- zp cz) z-size)) -1.0 1.0)
					    (- dzp)))))
	      #+never
	      (format t ";; change-xy-size from (~a ~a) to (~a ~a) by (~a ~a)~%"
		      x-size y-size new-x-size new-y-size dxp dyp)
	      (unless (zerop z-size)
		(scale-vertex-array object ; (vertex-array object)
				    (cv 1.0
					1.0
					(/ new-z-size z-size)))
		(setf (aref sizes 2) new-z-size))
    
	      ;; reset world position of anchor point
	      ;;(align-at-vertex object selected-position)
	      )))))))


;;; OBJECT MOTIONS THAT CHANGE THE OBJECT-TO-PARENT-MATRIX

(defmethod change-object-to-parent-xy-size ((object gl-object) sel-obj-pos delta-xy-size)
  (let ((center-vertex (center-vertex object)))
    (bind-vector-elements (xp yp) sel-obj-pos
      (bind-vector-elements (cx cy) center-vertex
	(bind-vector-elements (dx-size dy-size) delta-xy-size
	  #+never  (setf dx-size (* .5 dx-size) dy-size (* .5 dy-size))
	  (let* ((x-size (* 2.0 (abs (- xp cx))))
		 (y-size (* 2.0 (abs (- yp cy))))
		 (new-x-size (- x-size (* (signum (- xp cx)) dx-size)))
		 (new-y-size (- y-size (* (signum (- yp cy)) dy-size)))
		 (x-scale (/ new-x-size x-size))
		 (y-scale (/ new-y-size y-size)))
	    (let ((mat (transform-matrix (object-to-parent-transform object))))
	      (loop for i from 0 below 3
		    do (setf (aref mat i 0) (* x-scale (aref mat i 0))))
	      (loop for i from 0 below 3
		    do (setf (aref mat i 1) (* y-scale (aref mat i 1))))))
	  ;; reset world position of anchor point
	  ;;(align-at-vertex object selected-position)
	  )))))

(defmethod change-object-to-parent-scale ((object gl-object) sel-obj-pos delta-xy-size)
  (let ((center-vertex (center-vertex object)))
    (bind-vector-elements (xp yp) sel-obj-pos
      (bind-vector-elements (cx cy) center-vertex
	(bind-vector-elements (dx-size dy-size) delta-xy-size
	  (let* ((mat (transform-matrix (object-to-parent-transform object)))
		 (o2p-scale (euclidean-length (aref mat 0 0) (aref mat 0 1)))
		 (delta (* (gui::max-dx-dy dx-size dy-size) o2p-scale))
		 (scale (expt 1.0002 delta)))
	    ;(format t "delta=~a~%" delta)
	    (unless (> o2p-scale 1e5)
	      (loop for i from 0 below 3
		    do (setf (aref mat i 0) (* scale (aref mat i 0))))
	      (loop for i from 0 below 3
		    do (setf (aref mat i 1) (* scale (aref mat i 1)))))
	    ))))))

;;;(defmethod change-object-to-parent-scale-and-rotation ((object gl-object) sel-obj-pos object-motion)
;;;  (let ((center-vertex (center-vertex object)))
;;;    (bind-vector-elements (xp yp) sel-obj-pos
;;;      (bind-vector-elements (cx cy) center-vertex
;;;        (bind-vector-elements (dxp dyp) object-motion
;;;          (let* ((vx (* 2.0 (- cx xp)))
;;;                 (vy (+ 2.0 (- cy yp)))
;;;                 ;; compute coords of point in local x-y plane diagonally
;;;                 ;; opposite w.r.t. object center
;;;                 (ocxp (+ xp vx))
;;;                 (ocyp (+ yp vy))
;;;                 (old-length (euclidean-length vx vy))
;;;                 (nocxp (+ ocxp dxp))
;;;                 (nocyp (+ ocyp dyp))
;;;                 (nvx (- nocxp xp))
;;;                 (nvy (- nocyp yp))
;;;                 (new-length (euclidean-length nvx nvy))
;;;                 ;; project motion onto the vector from sel-obj-pos to object center
;;;                 (scale-motion (/ (math::inline-inner-prod (vx vy) (dxp dyp))
;;;                                  (euclidean-length vx vy)))
;;;                 (scale (expt 1.001 scale-motion))
;;;                 (sin-d-theta0 (/ (- (* nvx vy) (* vx nvy))
;;;                                  (* old-length new-length)))
;;;                 (sin-d-theta (min 1.0 (max -1.0 sin-d-theta0)))
;;;                 (object-to-parent-transform (object-to-parent-transform object))
;;;                 (mat (transform-matrix object-to-parent-transform))
;;;                 (o2p-scale (euclidean-length (aref mat 0 0) (aref mat 0 1)))
;;;                 )
;;;            ;;(format t "change-object-to-parent-xy-size-rotation ~a~%" (list scale o2p-scale old-length new-length))
;;;            ;;(format t "~a~%" (list (list vx vy) (list dxp dyp) scale-motion (list (- cx xp) (- cy yp)) sel-obj-pos center-vertex))
;;;            (when (or (> scale 1.0)
;;;                      (> (* scale o2p-scale) .01))
;;;              (transforms::post-multiply 
;;;               object-to-parent-transform
;;;               (multiply-matrices (make-and-fill-4x4-matrix scale 0.0 0.0 0.0
;;;                                                            0.0 scale 0.0 0.0
;;;                                                            0.0 0.0 1.0 0.0
;;;                                                            0.0 0.0 0.0 1.0)
;;;                                  (math::make-4x4-rotation-matrix :z-rad  (asin sin-d-theta)))
;;;               (gui::selected-object-parent-position *interactor*))
;;;                
;;;              )))))))

(defmethod change-object-to-parent-scale-and-rotation ((object gl-object) sel-obj-pos object-motion)
  (let ((center-vertex (center-vertex object)))
    (bind-vector-elements (xp yp) sel-obj-pos
      (bind-vector-elements (cx cy) center-vertex
	(bind-vector-elements (dxp dyp) object-motion
	  (let* ((vx (- cx xp))
		 (vy (- cy yp))
		 (l (euclidean-length vx vy))
		 (scale-motion (/ (math::inline-inner-prod (vx vy) (dxp dyp)) l))
		 (rotate-motion (/ (math::inline-inner-prod (vx vy) ((- dyp) dxp)) l))
		 (dtheta (* rotate-motion (radians .05)))
		 (scale (expt 1.001 scale-motion))
		 (object-to-parent-transform (object-to-parent-transform object)))
	    (transforms::post-multiply 
	     object-to-parent-transform
	     (multiply-matrices (make-and-fill-4x4-matrix scale 0.0 0.0 0.0
							  0.0 scale 0.0 0.0
							  0.0 0.0 1.0 0.0
							  0.0 0.0 0.0 1.0)
				(math::make-4x4-rotation-matrix :z-rad dtheta))
	     (gui::selected-object-parent-position *interactor*))))))))

;;; **********************************  BOUNDING-BOX  **********************************

#|
This implementation of BOUNDING-BOX is not very efficient.  Two major things
need to be done:

   1.  The transform used in vertex-array-bounding-box needs to be optimized,
       ie. for chains, it should be collapsed to a single 4x4-transform (or matrix).

   2.  There needs to be a caching mechanism so that BOUNDING-BOX isn't recalculated
       when nothing (vertices or transforms) has changed.

|#


(defmethod bounding-box ((o no-vertices-mixin) &key bbox transform)
  bbox)

(defmethod bounding-box ((o gl-object) &key bbox transform)
  (vertex-array-bounding-box (vertex-array o) bbox transform))

(defmethod bounding-box ((o basic-gl-object) &key bbox transform)
  (bind-vector-elements (x y z) (transform-vector transform (center-vertex o))
    (let ((my-bbox (math::inline-coordinate-vector x x y y z z)))
      (math::destructive-union-bounding-boxes bbox my-bbox))))
