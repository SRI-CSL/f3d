(in-package :gui)

;;; These are primarily DRAG OPERATIONS that are generic to all objects.

;;; Some drag operations that are very specialized to specific object classes
;;; are in the files defining those classes.

(defun max-dx-dy (dx dy)
  (if (> (abs dx) (abs dy))
      dx
      (- dy)))

;;; ***************  OPERATIONS ON OBJECT-TO-WORLD-TRANSFORM   **********************

(defmethod move-object-uv ((object basic-gl-object) interactor)
  (with-class-slot-values interactor (window-motion) interactor
    (let* ((view (current-view interactor))
	   (world (object-view-world object view)))
      (let* ((delta-position
	      (project-window-motion-to-world-motion
	       view world
	       window-motion
	       (selected-object-world-position interactor world))))
	(move-world-position object delta-position world)
	))))

;(find-terrain-model (top-view))
#|
((#<CME::NON-LINEAR-MAPPED-REGULAR-GRID-TERRAIN-MODEL {580B69E5}>
  #<FRAME-CAMERA (#<3D-WORLD "Fort Hood 2" #X610B9435> to #<2D-WORLD "fhov625" #X581A3585>) #X582A7ABD>
  #(-1957.4367229485174 4032.8234438073773 0.910885510126195)))
|#

(defmethod move-object-uv-on-dtm  ((object gl-3d-object-mixin) interactor)
  (with-class-slot-values interactor (window-motion) interactor
    (let* ((view (current-view interactor))
	   (tm (find-terrain-model view)))
      (if tm
	  (let* ((projection (3d-to-2d-projection view))
		 (world (object-view-world object view))
		 (3d-pos (selected-object-world-position interactor world))
		 (2d-pos (transform-vector projection 3d-pos))
		 (ignore (unless 2d-pos (return-from move-object-uv-on-dtm nil)))
		 (2d-motion (transform-direction-vector (inverse-transform (2d-to-window-transform view))
							window-motion))
		 (new-2d-pos (math::g+ 2d-pos 2d-motion))
		 (new-3d-pos (intersect-camera-ray-with-terrain-model
			      tm projection new-2d-pos 3d-pos)))
	    (if new-3d-pos
		(let ((3d-motion (math::g- new-3d-pos 3d-pos)))
		  ;(format t "move-object-uv-on-dtm ~a ~a~%" 3d-motion new-3d-pos)
		  (move-world-position object (math::g- new-3d-pos 3d-pos)))
		(move-object-uv object interactor)))
	  (move-object-uv object interactor)))))

;;;
(defmethod move-object-w ((object gl-3d-object-mixin) interactor)
  (with-class-slot-values interactor (window-motion) interactor
    (let* ((view (current-view interactor))
	   (world (object-view-world object view)))
      (bind-vector-elements (du dv) window-motion
	(let* ((d-window (max-dx-dy du dv))
	       (position-for-object-motion (selected-object-world-position interactor world))
	       (newpos (obj::compute-position-along-camera-ray
			object
			position-for-object-motion
			(3d-to-2d-projection view)
			(project-window-distance-to-world-distance
			 view world position-for-object-motion (- d-window)))))
	  (when newpos
	    (move-world-position object (vector-difference newpos position-for-object-motion) world)
	    ))))))


;;; Still not right:
(defmethod move-object-w-to-dtm ((object gl-3d-object-mixin) interactor)
  (let* ((view (current-view interactor))
	 (tm (find-terrain-model view)))
    (when tm
      (let* ((projection (3d-to-2d-projection view))
	     (world (object-view-world object view))
	     (3d-pos (selected-object-world-position interactor world))
	     (2d-pos (transform-vector projection 3d-pos))
	     (ignore (unless 2d-pos (return-from move-object-w-to-dtm nil)))
	     (new-3d-pos (intersect-camera-ray-with-terrain-model
			  tm projection 2d-pos 3d-pos)))
	(when new-3d-pos
	  (print new-3d-pos)
	  (move-world-position object (math::g- new-3d-pos 3d-pos)))
	))))


(defmethod move-object-verts-w-to-dtm ((object obj::basic-curve) interactor)
  (let* ((view (current-view interactor))
	 (tm (find-terrain-model view)))
    (when tm
      (let* ((projection (3d-to-2d-projection view))
	     (va (obj::vertex-array object))
	     (xf (object-to-world-transform object)))
	(loop for i from 0 below (array-dimension va 0)
	      for 3d-pos = (transform-vector xf (obj::vertex-array-vertex va i))
	      for 2d-pos = (transform-vector projection 3d-pos)
	      when 2d-pos
		do (let ((new-world-pos (intersect-camera-ray-with-terrain-model
					 tm projection 2d-pos 3d-pos)))
		     (when new-world-pos
		       (let ((new-3d-pos (inverse-transform-vector xf new-world-pos)))
			 (print new-3d-pos)
			 (setf (aref va i 0) (aref new-3d-pos 0))
			 (setf (aref va i 1) (aref new-3d-pos 1))
			 (setf (aref va i 2) (aref new-3d-pos 2))))))
	(update-object object)
	))
    ))






(defparameter *roll-object-degrees-per-pixel* .3)
(defparameter *rads-per-pixel* (* *roll-object-degrees-per-pixel* #.(/ pi 180)))

(defmethod roll-object ((object gl-3d-object-mixin) interactor)
  (with-class-slot-values interactor (window-motion) interactor
    (let* ((view (current-view interactor))
	   (world (object-view-world object view)))
      (bind-vector-elements (dx dy) window-motion
	(let* ((projection (3d-to-2d-projection view))
	       (position-for-object-motion (selected-object-parent-position interactor))
	       ;;(position-for-object-motion (selected-object-world-position interactor world))
	       (center-of-rotation position-for-object-motion)
	       (2d-to-window-transform (2d-to-window-transform view))
	       (pixels-per-2d-unit (transforms::transform-scale-factor
				    2d-to-window-transform nil))
	       (rads-per-2d-unit (+  (* *rads-per-pixel* pixels-per-2d-unit)))
	       )
	  (bind-vector-elements (du dv)
	      (transform-window-motion-to-2d-world-motion 2d-to-window-transform
							  (cv dx dy 0.0))
	    (rotate-relative-to-world-by object
					 (rolling-ball-rotation-matrix
					  projection position-for-object-motion
					  (* du rads-per-2d-unit)
					  (* dv rads-per-2d-unit))
					 center-of-rotation )))))))

(defmethod rotate-about-object-z ((object basic-gl-object) interactor)
  (with-class-slot-values interactor (window-motion) interactor
    (bind-vector-elements (dx dy) window-motion
      (let* ((center-of-rotation (selected-object-parent-position interactor))
	     (d-theta  (* *rads-per-pixel* (max-dx-dy dx dy))))
	(rotate-by object (list :z-rot d-theta) center-of-rotation)))))



;;; ***************  OPERATIONS ON OBJECT ALL VERTICES   **********************

(defmethod object-view-world ((object gl-3d-object-mixin) view)
  (3d-world view))

(defmethod object-view-world ((object gl-2d-object-mixin) view)
  (2d-world view))

(defmethod object-motion-operator ((object basic-gl-object) interactor action-function)
  (with-class-slot-values interactor (window-motion) interactor
    (let* ((view (current-view interactor))
	   (objfrag (car (selected-objects interactor)))
	   (object (car objfrag))
	   )
      (let* ((fragment (cadr objfrag))
	     (selected-obj-pos (obj::fragment-position fragment))
	     ;(world (object-view-world object view))
	     (world (world object))
	     (object-to-world-transform (object-to-world-transform object world))
	     (selected-world-pos (transform-vector object-to-world-transform selected-obj-pos)))
	;;(format t "resize-object-xy ~a ~a~%" object window-motion)
	(when object
	  (let* ((object-position-motion
		  (transform-direction-vector
		   (inverse-transform object-to-world-transform)
		   (project-window-motion-to-world-motion
		    view world
		    window-motion
		    selected-world-pos))))
	    (funcall action-function object selected-obj-pos object-position-motion)
	    (obj::move-to object selected-world-pos
			  (obj::fragment-position fragment))))))))

;;#+experimental
(defmethod object-motion-operator ((object basic-gl-object) interactor action-function)
  (with-class-slot-values interactor (window-motion) interactor
    (let* ((view (current-view interactor))
	   (objfrag (car (selected-objects interactor)))
	   (object (car objfrag))
	   )
      (let* ((fragment (cadr objfrag))
	     (selected-obj-pos (obj::fragment-position fragment))
	     (parent (parent object))
	     (object-to-parent-transform (object-to-parent-transform object))
	     (selected-parent-pos (transform-vector object-to-parent-transform selected-obj-pos)))
	;;(format t "resize-object-xy ~a ~a~%" object window-motion)
	(when object
	  (let* ((object-position-motion
		  (transform-direction-vector
		   (inverse-transform object-to-parent-transform)
		   (project-window-motion-to-world-motion
		    view parent
		    window-motion
		    selected-parent-pos))))
	    (funcall action-function object selected-obj-pos object-position-motion)
	    (obj::move-to object selected-parent-pos
			  (obj::fragment-position fragment))))))))


(defmethod resize-object-xy ((object basic-gl-object) interactor)
  (object-motion-operator object interactor #'obj::change-xy-size))

(defmethod rotate-resize-object-xy ((object basic-gl-object) interactor)
  (object-motion-operator object interactor #'obj::rotate-change-xy-size))

(defmethod resize-object-z ((object basic-gl-object) interactor)
  (object-motion-operator object interactor #'obj::change-z-size))

;;;(defmethod resize-object-xy ((object gl-object) interactor)
;;;  (with-class-slot-values interactor (window-motion) interactor
;;;    (let* ((view (current-view interactor))
;;;           (objfrag (car (selected-objects interactor)))
;;;           (object (car objfrag))
;;;           )
;;;      (let* ((fragment (cadr objfrag))
;;;             (selected-obj-pos (obj::fragment-position fragment))
;;;             (selected-world-pos (transform-vector
;;;                                  (object-to-world-transform object)
;;;                                  selected-obj-pos)))
;;;        ;;(format t "resize-object-xy ~a ~a~%" object window-motion)
;;;        (when object
;;;          (let* ((fragment-position-motion
;;;                  (transform-direction-vector
;;;                   (inverse-transform (object-to-world-transform object))
;;;                   (gui::project-window-motion-to-world-motion
;;;                    view (world object)
;;;                    window-motion
;;;                    selected-world-pos))))
;;;            (obj::change-xy-size object selected-obj-pos object-position-motion)
;;;            (obj::move-to object selected-world-pos
;;;                          (obj::fragment-position fragment))))))))


;;;(defmethod resize-object-z ((object gl-object) interactor)
;;;  (with-class-slot-values interactor (window-motion) interactor
;;;    (let* ((view (current-view interactor))
;;;           (objfrag (car (selected-objects interactor)))
;;;           (object (car objfrag))
;;;           )
;;;      (let* ((fragment (cadr objfrag))
;;;             (selected-obj-pos (obj::fragment-position fragment))
;;;             (selected-world-pos (transform-vector
;;;                                  (object-to-world-transform object)
;;;                                  selected-obj-pos)))
;;;        ;;(format t "resize-object-z ~a ~a~%" object window-motion)
;;;        (when object
;;;          (let* ((object-position-motion
;;;                  (transform-direction-vector
;;;                   (inverse-transform (object-to-world-transform object))
;;;                   (gui::project-window-motion-to-world-motion
;;;                    view (world object)
;;;                    window-motion
;;;                    selected-world-pos))))
;;;            (obj::change-z-size object selected-obj-pos object-position-motion)
;;;            (obj::move-to object selected-world-pos
;;;                          (obj::fragment-position fragment))))))))



;;; ***************  OPERATIONS ON INDIVIDUAL OBJECT VERTICES   **********************

(defmethod move-object-vertex-uv ((object basic-gl-object) interactor)
  (with-class-slot-values interactor (window-motion) interactor
    (let* ((view (current-view interactor))
	   (world (object-view-world object view))
	   (fragment (selected-object-fragment interactor)))
      (when (and fragment (typep fragment 'object-vertex))
	(let* ((delta-position
		(project-window-motion-to-world-motion
		 view world
		 window-motion
		 (selected-object-world-position interactor world))))
	  (move-world-position fragment delta-position world)
	  )))))

(defmethod move-object-vertex-w ((object gl-3d-object-mixin) interactor)
  (with-class-slot-values interactor (window-motion) interactor
    (let* ((view (current-view interactor))
	   (world (object-view-world object view))
	   (fragment (selected-object-fragment interactor)))
      (when (and fragment (typep fragment 'object-vertex))
	(bind-vector-elements (du dv) window-motion
	  (let* ((d-window (max-dx-dy du dv))
		 (position-for-object-motion (selected-object-world-position interactor world))
		 (newpos (compute-position-along-camera-ray
			  object
			  position-for-object-motion
			  (3d-to-2d-projection view)
			  (project-window-distance-to-world-distance
			   view (world object) position-for-object-motion (- d-window)))))
	    (when newpos
	      (move-world-position fragment (vector-difference newpos position-for-object-motion) world)
	      )))))))


(defparameter *ribbon-min-width* .1)

(defmethod change-ribbon-width ((object obj::ribbon) interactor)
  (with-class-slot-values interactor (window-motion) interactor
    (let* ((fragment (selected-object-fragment interactor)))
      (when (and fragment (typep fragment 'object-vertex))
	(update-object object
	  (bind-vector-elements (du dv) window-motion
	    (with-class-slot-values object-vertex (vertex-id) fragment
	      (let* ((old-width (aref (obj::%vertex-array object) vertex-id 3))
		     (factor (expt 2.0 (* .005 (max-dx-dy du dv))))
		     (new-width (* factor old-width)))
		(setf (aref (obj::%vertex-array object) vertex-id 3) 
		      (max  *ribbon-min-width* new-width))))))))))


(defmethod camera-az-elev (ignore interactor)
  (with-class-slot-values interactor (window-motion) interactor
    (let ((current-view (current-view interactor)))
      (unless (view-image current-view)	;; do not adjust camera models on views with images.
	(change-azimuth-elevation-relative-to-position
	 (3d-to-2d-projection current-view)
	 (get-prop current-view :world-center-of-rotation)
	 ;;(world-center-of-rotation interactor) ; this is currently borken
	 window-motion
	 (2d-to-window-transform current-view)
	 )))))


;;;
(defmethod roof-pitch-drag ((object house-object) interactor)
  (with-class-slot-values interactor (window-motion) interactor
    (bind-vector-elements (xsize ysize zsize)
        (obj::sizes object)
      (bind-vector-elements (dx dy) window-motion
        (obj::set-roof-pitch object (+ (obj::roof-pitch object) (* 0.001 dy)))
        (setf (obj::sizes object) (cv xsize ysize zsize))
        (obj::compute-vertices object)
        ))))
