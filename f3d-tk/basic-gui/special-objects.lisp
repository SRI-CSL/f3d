(in-package :gui)

(import '(math::g+ math::g* math::g- math::rms-norm math::transpose))
(import '(transforms::update-transform))


(defparameter *adjustable-2d-world-count* 0)

(defun make-adjustable-view (&key (interactor *interactor*) (view (pick-a-view "Pick View with Projection to Copy"))
			     (window (pick-a-window "Pick a Window for New Adjustable View")))
  (declare (ignore interactor))
  (let* ((frame-camera (3d-to-2d-projection view))
	 (frame-camera (transforms::make-frame-camera-from-matrices 
			(copy-matrix (transforms::3d-to-camera-matrix frame-camera))
			(copy-matrix (transforms::camera-to-2d-matrix frame-camera))))
	 (3d-world (3d-world view))
	 (2d-world (make-instance '2d-world
				  :name (format nil "Synthetic 2D World ~d"
						(incf *adjustable-2d-world-count*))
				  :3d-world 3d-world)))
    (setf (3d-to-2d-projection 2d-world) frame-camera)
    (let* ((2d-to-window-transform (make-4x4-coordinate-transform 
				    (copy-matrix (transforms::transform-matrix 
						  (2d-to-window-transform view)))))
	   (view (make-instance 'gui::synthetic-view
				:2d-world 2d-world
				:2d-to-window-transform 2d-to-window-transform
				:object-sets (default-object-sets 3d-world)
				:3d-world 3d-world
				:window window))
	   (principal-point (transforms::principal-point frame-camera))
	   (stare-point-position 
	    (intersect-camera-ray-with-terrain-model (find-terrain-model 3d-world)
						     frame-camera
						     principal-point))
	   )
      (gui::push-view view window)
      (mv-bind (spo object-set)
	  (gui::add-perspective-transform-stare-point-object view stare-point-position)
	(push object-set (sensitive-feature-sets view)))
      view)))

(defun com-make-adjustable-view (&optional (interactor *interactor*))
  (make-adjustable-view
   :interactor interactor
   ))


#|
(maybe-compile-file-load "$FREEDIUS/lisp/basic-gui/special-objects.lisp")

;(cme::push-cloned-synthetic-view (top-view) (gui::selected-window))
(make-adjustable-view)
(gui::add-perspective-transform-stare-point-object (gui::selected-view) 
						   (cv -18.88548967102416 4383.756156207143 6015.446))
(gui::add-perspective-transform-stare-point-object (gui::selected-view) 
						   (cv -2296.086 268.917 282.141))
(gui::add-perspective-transform-stare-point-object (gui::selected-view) 
						   (vector-add (gui::selected-object-world-position)
							       (cv 10.0 0.0 0.0)))

(pop-view (selected-window))
|#

(defparameter *default-perspective-transform-object-graphics-style*
  (make-instance 'gl-graphics-style
		 :color :red
		 :line-width 2))
;(describe *default-perspective-transform-object-graphics-style*)

(defstruct-class perspective-transform-object-mixin (obj::3d-object)
  ((projection :initform nil :initarg :projection)))

(defmethod initialize-instance :after ((object perspective-transform-object-mixin)
				       &key &allow-other-keys)
  (with-class-slots perspective-transform-object-mixin (graphics-style projection) object
    (unless graphics-style
      (setf graphics-style *default-perspective-transform-object-graphics-style*))
    (unless projection
      (setf projection (3d-to-2d-projection (current-view))))))

#+never
(defmethod object-popup-menu-item-list  ((ui cme-object-popup-ui)
					 (object perspective-transform-object-mixin))
  '(("Move XY" :accel <alpha-left> :eval (start-popup-object-drag 'move-object-uv) )
    ("Move W" :accel <alpha-middle> :eval (start-popup-object-drag 'move-object-w) )
    ("Rot W" :accel <alpha-right>  :image-drag 'rotate-about-object-w)
    ("" :separator)
    ;;("Princ Pt" :accel <gamma-left> :eval (start-popup-object-drag 'move-principal-point))
    ("Roll" :accel <gamma-left> :eval (start-popup-object-drag 'roll-object))
    ;;("Z Up" :eval (com-canonicalize-orientation *interactor*))
    ("" :separator)
    ("Focal Length" :accel <gamma-middle> :image-drag 'change-focal-length)
	    
    ))

(defstruct-class perspective-transform-stare-point-object (obj::axis-object
							   perspective-transform-object-mixin)
  ((camera-model-object :initform nil :initarg :camera-model-object :accessor camera-model-object)
   ))

(defmethod obj::short-name-string ((o perspective-transform-stare-point-object))
  "Stare Pt")

(defmethod transform-image-to-camera ((projection transforms::frame-camera) from-vector &optional to-vector)
  (bind-vector-elements (u v) from-vector
    (unless to-vector (setq to-vector (make-coordinate-vector 2)))
    (let* ((image-to-camera-matrix (invert-matrix (transforms::camera-to-2d-matrix projection))))
      (declare (type (simple-array double-float (* *)) image-to-camera-matrix))
      ;; This assumes m[0,0]=1, m[1,0]=0
      (let ((up (+ u (* (aref image-to-camera-matrix 0 1) v) (aref image-to-camera-matrix 0 3)))
	    (vp (+   (* (aref image-to-camera-matrix 1 1) v) (aref image-to-camera-matrix 1 3))))
	(declare (double-float up vp))
	(set-coordinate-vector-elements to-vector up vp)))))
     
#+old
(defun add-perspective-transform-stare-point-object (view gnd-position)
  (unless gnd-position (error "must supply a ground position"))
  (let* ((projection (3d-to-2d-projection view))
	 (spo (make-instance 'perspective-transform-stare-point-object
			     :projection projection
			     :parent (3d-world view)
			     :object-to-parent-transform
			     (make-4x4-coordinate-transform 
			      (make-object-to-parent-matrix gnd-position))))
	 (object-set (make-object-set (list spo)
				      :world (3d-world view)
				      :immediate-render-p t
				      :backing-store-p t
				      :direct-render-p t))
	 )
    (push object-set (object-sets view))
    ))

#+old
(defun add-perspective-transform-stare-point-object (view gnd-position)
  (unless gnd-position (error "must supply a ground position"))
  (let* ((projection (3d-to-2d-projection view))
	 (spo (make-instance 'perspective-transform-stare-point-object
			     :projection projection
			     :parent (3d-world view)
			     :object-to-parent-transform
			     (make-4x4-coordinate-transform 
			      (make-object-to-parent-matrix gnd-position))))
	 (feature-set (make-new-feature-set-for-world (3d-world view)))
	 )
    (add-object spo feature-set)
    ;(push feature-set (cme::feature-sets (3d-world view)))
    (add-feature-set feature-set (3d-world view))
    ))

(defun add-perspective-transform-stare-point-object (view gnd-position)
  (unless gnd-position (error "must supply a ground position"))
  (let* ((projection (3d-to-2d-projection view))
	 (spo (make-instance 'perspective-transform-stare-point-object
			     :projection projection
			     :parent (3d-world view)
			     :object-to-parent-transform
			     (make-4x4-coordinate-transform 
			      (make-object-to-parent-matrix gnd-position))))
	 (object-set (make-object-set (list spo) :world (3d-world view)))
	 )
    (push object-set (object-sets view))
    (values spo object-set)))


(defmethod uv-on-principal-ray-p ((projection transforms::frame-camera) stare-point-uv)
  (bind-vector-elements (spup spvp) (transform-image-to-camera projection stare-point-uv)
    (and (< (abs spup) 2.0)
	 (< (abs spvp) 2.0))))

(defmethod stare-point-uv ((object perspective-transform-stare-point-object) projection)
  (transform-vector projection (origin (object-to-world-transform object))))

(defmethod stare-point-on-principal-ray-p ((object perspective-transform-stare-point-object))
  (with-class-slot-values perspective-transform-object-mixin (projection) object
    (if (not (typep projection 'transforms::frame-camera))
	t
	(uv-on-principal-ray-p projection (stare-point-uv object projection)))))

(defmethod set-stare-point-and-camera-positions ((object perspective-transform-stare-point-object)
						 newpos)
  (with-class-slot-values perspective-transform-object-mixin (projection) object
    (move-by projection (vector-difference newpos (origin object)))
    (set-origin object  newpos)))
  

(defmethod move-object-uv ((object perspective-transform-stare-point-object) interactor)
  (with-class-slot-values interactor (window-motion) interactor
    (let ((view (current-view interactor)))
      (setf (aref window-motion 0) (- (aref window-motion 0))
	    (aref window-motion 1) (- (aref window-motion 1)))
      (let* ((current-position (selected-object-world-position interactor))
	     (delta-position
	      (project-window-motion-to-world-motion
	       view (world object)
	       window-motion
	       current-position))
	     (new-position (vector-add delta-position current-position)))
	;;(format t "move-object-uv ~a ~a~%" delta-position new-position)
	(set-stare-point-and-camera-positions object new-position)
	))))

(defun retaining-window-position (view 3d-pos function)
  (let* ((projection (3d-to-2d-projection view))
	 (3d-to-window (list projection (2d-to-window-transform view)))
	 (window-pos (transform-vector 3d-to-window 3d-pos))
	 )
    (funcall function)
    (move-by (transform-matrix (2d-to-window-transform view)) 
	     (vector-difference window-pos (transform-vector 3d-to-window 3d-pos)))))
    

(defparameter *perspective-transform-object-default-scale-change-per-pixel* 1.001) 

;(get-prop (3d-to-2d-projection (top-view)) :near-far)
;;;(defmethod change-focal-length ((object perspective-transform-stare-point-object) interactor)
;;;  (with-class-slot-values interactor (window-motion) interactor
;;;    (let* ((view (current-view interactor))
;;;           (projection (3d-to-2d-projection view))
;;;           (2d-pos (transform-vector projection (origin object)))
;;;           (window-pos (transform-vector (2d-to-window-transform view) 2d-pos)))
;;;      (unless (view-image view) ;; do not adjust camera models on views with images.
;;;        (bind-vector-elements (du dv) window-motion
;;;          (let* ((d-window (max-dx-dy du dv))
;;;                 (scale (expt *perspective-transform-object-default-scale-change-per-pixel* d-window)))
;;;            (setf (transforms::1/f projection) (* scale (transforms::1/f projection)))
;;;            (setf (transforms::GSD projection) (* scale (transforms::GSD projection)))
;;;            (transforms::update-transform projection)
;;;            (move-by (transform-matrix (2d-to-window-transform view)) 
;;;                     (vector-difference window-pos 
;;;                                        (transform-vector (2d-to-window-transform view) 
;;;                                                          (transform-vector projection (origin object)))))
;;;            ))))))


(defmethod change-focal-length ((object perspective-transform-stare-point-object) interactor)
  (let* ((view (current-view interactor))
	 (view-projection (3d-to-2d-projection view)))
    (with-class-slots perspective-transform-object-mixin (projection) object
      (when (eq projection view-projection) 
	(retaining-window-position
	 view (origin object)
	 #'(lambda ()
	     (with-class-slot-values interactor (window-motion) interactor
	       (bind-vector-elements (du dv) window-motion
		 (let* ((d-window (max-dx-dy du dv))
			(scale (expt *perspective-transform-object-default-scale-change-per-pixel* d-window)))
		   (setf (transforms::1/f projection) (* scale (transforms::1/f projection)))
		   (setf (transforms::GSD projection) (* scale (transforms::GSD projection)))
		   (transforms::update-transform projection))))))))))


;(origin (3d-to-2d-projection (top-view)))
;(pop-view (selected-window))

#+never 
(defmethod change-focal-length-and-range ((object perspective-transform-stare-point-object) interactor)
  (let* ((view (current-view interactor))
	 (view-projection (3d-to-2d-projection view)))
    (with-class-slots perspective-transform-object-mixin (projection) object
      (when (eq projection view-projection) 
	(retaining-window-position 
	 view (origin object)
	 #'(lambda ()
	     (with-class-slot-values interactor (window-motion) interactor
	       (bind-vector-elements (du dv) window-motion
		 (let* ((d-window (max-dx-dy du dv))
			(scale (expt *perspective-transform-object-default-scale-change-per-pixel* d-window))
			(projection (3d-to-2d-projection view))
			(principal-ray (transforms::principal-ray projection))
			(range (g* (transpose principal-ray) (g- (origin projection) (origin object))))
			(new-origin (g- (origin projection) (g* (* range (- 1.0 (/ scale))) principal-ray)))
			)
		   #+never
		   (progn 
		     (setq *foo* (list new-origin (origin projection) range (- 1.0 (/ scale)) principal-ray 
				       (invert-matrix (transforms::3d-to-camera-matrix projection)))) 
		     (break)
		     )
		   (setf (transforms::1/f projection) (* scale (transforms::1/f projection)))
		   (setf (transforms::GSD projection) (* scale (transforms::GSD projection)))
		   (set-origin projection new-origin)
		   (update-transform projection))))))))))

(defmethod change-focal-length-and-range ((object perspective-transform-stare-point-object) interactor)
  (let* ((view (current-view interactor))
	 (view-projection (3d-to-2d-projection view))
	 (transforms::*transform-dammit* t))
    (with-class-slots perspective-transform-object-mixin (projection) object
      (when (eq projection view-projection) 
	(retaining-window-position 
	 view (origin object)
	 #'(lambda ()
	     (with-class-slot-values interactor (window-motion) interactor
	       (bind-vector-elements (du dv) window-motion
		 (let* ((d-window (max-dx-dy du dv))
			(scale (expt *perspective-transform-object-default-scale-change-per-pixel* d-window)))
		   (scale-stare-point-range-and-focal-length-by view-projection scale object))))))))))


(defmethod move-object-w ((object perspective-transform-stare-point-object) interactor)
  (with-class-slot-values interactor (window-motion) interactor
    (let ((view (current-view interactor)))
      (bind-vector-elements (du dv) window-motion
	(let* ((d-window (- (max-dx-dy du dv)))
	       (position-for-object-motion (selected-object-world-position interactor))
	       (newpos (obj::compute-position-along-camera-ray
			object
			position-for-object-motion
			(3d-to-2d-projection view)
			(project-window-distance-to-world-distance
			 view (world object) position-for-object-motion (- d-window)))))
	  (when newpos
	    (set-stare-point-and-camera-positions object newpos)))))))

(defmethod move-object-z ((object perspective-transform-stare-point-object) interactor)
  (with-class-slot-values interactor (window-motion) interactor
    (let ((view (current-view interactor)))
      (bind-vector-elements (du dv) window-motion
	(let* ((d-window (max-dx-dy du dv))
	       (position-for-object-motion (selected-object-world-position interactor))
	       (delta-z (project-window-distance-to-world-distance
			 view (world object) position-for-object-motion (- d-window))))
	  (bind-vector-elements (x y z) (origin object)
	    (set-stare-point-and-camera-positions object (cv x y (+ z delta-z)))))))))


(defmethod position-camera-relative-to-stare-point-internal
	   ((projection transforms::frame-camera) stare-point-object stare-point-2d-position)
  (bind-vector-elements (spx spy spz) (origin stare-point-object)
    (bind-vector-elements (ox oy oz) (origin projection) ; this is not necessarily the camera-position
      (with-class-slots transforms::frame-camera (3d-to-camera-matrix ) projection
	(declare (type 4x4-matrix 3d-to-camera-matrix ))
	(let* ((delta-range (+ (* (- ox spx) (aref 3d-to-camera-matrix 0 2))
			       (* (- oy spy) (aref 3d-to-camera-matrix 1 2))
			       (* (- oz spz) (aref 3d-to-camera-matrix 2 2))))
	       (GSD (transforms::GSD projection))
	       (1/f (transforms::1/f projection))
	       )
	  (declare (double-float delta-range GSD 1/f))
	  (setf GSD (+ GSD (* 1/f delta-range))
		;; positive-w-clip-plane (+ positive-w-clip-plane delta-range)
		)
	  ;; offset camera from stare point by difference in image positions.
	  (bind-vector-elements (spup spvp)
	      (transform-image-to-camera projection stare-point-2d-position)
	    (let* ((du (* GSD spup))
		   (dv (* GSD spvp))
		   (xc (+ spx (* du (aref 3d-to-camera-matrix 0 0)) (* dv (aref 3d-to-camera-matrix 0 1))))
		   (yc (+ spy (* du (aref 3d-to-camera-matrix 1 0)) (* dv (aref 3d-to-camera-matrix 1 1))))
		   (zc (+ spz (* du (aref 3d-to-camera-matrix 2 0)) (* dv (aref 3d-to-camera-matrix 2 1)))))
	      (declare (double-float du dv xc yc zc))
	      (setf (transforms::GSD projection) GSD)
	      #+never 
	      (format t "position-camera-relative-to-stare-point-internal ~a~%"
		      (list delta-range GSD 1/f (coordinate-vector xc yc zc) (coordinate-vector du dv)))
	      (set-origin projection (coordinate-vector xc yc zc))
					;(update-transform projection)
	      )))))

    ))

#|
(set-camera-to-2d-matrix-depth-normalization
 (transforms::camera-to-2d-matrix (3d-to-2d-projection (top-view)))
 100.0 nil)

(set-camera-to-2d-matrix-depth-normalization
 (transforms::camera-to-2d-matrix (3d-to-2d-projection (top-view)))
 1.0 nil)

|#


(defmethod position-camera-relative-to-stare-point
	   ((projection transforms::frame-camera) stare-point-object)
  (let ((stare-point-position (transform-vector projection (origin stare-point-object) )))
    (when (uv-on-principal-ray-p projection stare-point-position)
      (setq stare-point-position (transforms::principal-point projection)))
    (position-camera-relative-to-stare-point-internal
     projection stare-point-object stare-point-position)))

(defmethod set-stare-point-position
    ((projection transforms::frame-camera) stare-point-object)
  (position-camera-relative-to-stare-point projection stare-point-object))

(defmethod scale-stare-point-range-and-focal-length-by ((projection transforms::frame-camera) factor stare-point-object)
  (set-stare-point-position projection stare-point-object)
  ;;(break)
  (setf (transforms::1/f projection) (/ (transforms::1/f projection) factor)
	;;positive-w-clip-plane (* factor positive-w-clip-plane)
	)
  (transforms::update-transform projection)
  )

;(fmakunbound 'change-orientation-relative-to-stare-point)
(defmethod change-orientation-relative-to-stare-point
    ((projection transforms::frame-camera)
     stare-point-object  pre-multiply &rest axis-spec-radian-pairs)
  (declare (special *temporary-matrix*))
  (let ((tmat (transform-matrix projection)))
    (bind-vector-elements (x y z) (origin stare-point-object)
      (let* ((range (euclidean-length (- (aref tmat 0 3) x) (- (aref tmat 1 3) y) (- (aref tmat 2 3) z))))	;; distance from camera origin to stare-point
	(transforms::rotate-transform-matrix tmat
				 axis-spec-radian-pairs
				 ;;(apply #'make-orientation-matrix axis-spec-radian-pairs)
				 :pre-multiply-p pre-multiply )
    
	;; Compute new camera position, storing result over existing transform-matrix.
	;; Note: w-axis (column 2) points into the camera. 
	;;(format t "change-orientation-relative-to-stare-point range = ~a~%" range)
	(position-camera-relative-to-stare-point projection stare-point-object )
	(transforms::update-transform projection)
      ))))


#|
(origin (3d-to-2d-projection (top-view)))
|#

;(trace change-camera-az-elev) (untrace)
(defmethod change-camera-az-elev ((object perspective-transform-stare-point-object) interactor)
  (with-class-slot-values interactor (window-motion) interactor
    (with-class-slot-values perspective-transform-stare-point-object (projection) object
      (let ((current-view (current-view interactor)))
	(when (eq projection (3d-to-2d-projection current-view))
	  #+never
	  (format t "change-camera-az-elev ~a ~%"
		  (list (3d-to-2d-projection current-view) window-motion (origin object)))
	  (change-azimuth-elevation-relative-to-position
	   projection
	   ;;(selected-object-world-position interactor) 
	   (origin object)
	   ;;(transform-vector (object-to-world-transform object current-view) (cv 0.0 0.0 0.0))
	   window-motion
	   (2d-to-window-transform current-view)))))))


#+broken
(defmethod rotate-about-object-w ((object perspective-transform-stare-point-object) interactor)
  (with-class-slot-values interactor (window-motion) interactor
    (with-class-slot-values perspective-transform-stare-point-object (projection) object
      (let ((current-view (current-view interactor)))
	(when (eq projection (3d-to-2d-projection current-view))
	  #+never
	  (format t "change-camera-az-elev ~a ~%"
		  (list (3d-to-2d-projection current-view) window-motion (origin object)))
	  (bind-vector-elements (du dv) window-motion
	    (let* ((d-window (- (max-dx-dy du dv))))	  
	      (change-orientation-relative-to-stare-point
	       projection object nil :z-rot d-window))))))))


(defmethod rotate-about-principal-ray ((object perspective-transform-stare-point-object) interactor) 
  (let* ((view (current-view interactor))
	 (view-projection (3d-to-2d-projection view)))
    (with-class-slots perspective-transform-object-mixin (projection) object
      (when (eq projection view-projection) 
	(retaining-window-position
	 (current-view interactor) (origin object) 
	 #'(lambda ()
	     (with-class-slot-values interactor (window-motion) interactor
	       (with-class-slot-values perspective-transform-stare-point-object (projection) object
		 (bind-vector-elements (du dv) window-motion
		   (let* ((d-window (- (max-dx-dy du dv))))
		     (transforms::rotate-about-principal-ray projection d-window)))))))))))


(defmethod rotate-about-camera-ray ((object perspective-transform-stare-point-object) interactor)
  (let* ((view (current-view interactor))
	 (view-projection (3d-to-2d-projection view)))
    (with-class-slots perspective-transform-object-mixin (projection) object
      (when (eq projection view-projection) 
	(with-class-slot-values interactor (window-motion) interactor
	  (with-class-slot-values perspective-transform-stare-point-object (projection) object
	    (bind-vector-elements (du dv) window-motion
	      (let* ((d-window (- (max-dx-dy du dv))))
		(transforms::rotate-about-camera-ray projection (origin object) d-window)))))))))
#|
(eval-cache-flush-function 'get-mouse-event-table)
|#


;;;(defmethod object-popup-menu-item-list  ((ui cme-object-popup-ui)
;;;                                         (object perspective-transform-stare-point-object))
;;;  ;;(format t "object-popup-menu-item-list ~a~%" object)
;;;  (append (call-next-method)
;;;          '(("Scale/Rotate" :accel <middle> :image-drag 'scale-rot-at-center
;;;             :bucky-doc "Scale/Rotate Image @C")
;;;            ("Move XY" :accel <alpha-left> :image-drag 'move-object-uv )
;;;            ("Move W" :accel <alpha-middle> :image-drag 'move-object-w )
;;;            ("Move Z" :accel <alpha-right> :image-drag 'move-object-z )
;;;            ("Focal Length" :accel <gamma-middle> :image-drag 'change-focal-length)
;;;            ("Range/FL" :accel <beta-right> :image-drag 'change-focal-length-and-range)
;;;            ("Cam W" :accel <beta-gamma-right> :image-drag 'rotate-about-camera-ray)
;;;            #+old
;;;            ("Cam Az/Elev" :accel <beta-gamma-left> 
;;;             :eval (start-drag *interactor* 'change-camera-az-elev 'image))
;;;            ("Cam Az/Elev" :accel <beta-gamma-left> :image-drag 'change-camera-az-elev)
;;;            ("Rot W" :accel <alpha-right>  :image-drag 'rotate-about-principal-ray))))


(defmethod object-popup-menu-item-list  ((ui cme-object-popup-ui)
					 (object perspective-transform-stare-point-object))
  ;;(format t "object-popup-menu-item-list ~a~%" object)
  (append (call-next-method)
	  '(;("Scale/Rotate" :accel <middle> :image-drag 'scale-rot-at-center :bucky-doc "Scale/Rotate Image @C")
	    ("Move XY" :accel <alpha-left> :image-drag 'move-object-uv )
	    ("Move W" :accel <alpha-middle> :image-drag 'move-object-w )
	    ("Move Z" :accel <alpha-right> :image-drag 'move-object-z )
	    ("Range/FL" :accel <beta-right> :image-drag 'change-focal-length-and-range)
	    ("Focal Length" :accel <gamma-middle> :image-drag 'change-focal-length)
	    ("Cam W" :accel <beta-gamma-right> :image-drag 'rotate-about-camera-ray)
	    #+old
 	    ("Cam Az/Elev" :accel <beta-gamma-left> 
	     :eval (start-drag *interactor* 'change-camera-az-elev 'image))
	    ("Cam Az/Elev" :accel <beta-gamma-left> :image-drag 'change-camera-az-elev)
	    ("Rot W" :accel <alpha-right>  :image-drag 'rotate-about-principal-ray)
	    )))




#|

(defun make-2d-to-window-matrix (win 2d-pos-at-window-center 2d-to-window-scale)
  (mv-bind (width height) (dimensions win)
    (bind-vector-elements (uc vc) 2d-pos-at-window-center
      (let* ((scale 2d-to-window-scale)
             (xoff (- (* .5 width) (* scale uc)))
             (yoff (+ (* .5 height) (* scale vc))))
        (make-array '(4 4) :element-type 'double-float
                    :initial-contents
                    `((,scale 0.0 0.0 ,xoff)
                      (0.0 ,(- scale) 0.0 ,yoff)
                      (0.0 0.0 1.0 0.0)
                      (0.0 0.0 0.0 1.0)))))))

(let* ((win (selected-window))
       (rotation-center (cv 256.9 4532.1 6015.4))
       (projection (make-adjustable-camera rotation-center
					   :height 10e3 :gsd 2.0
					   ;;:win win :fov 20.0
					   ))
       (2d-world (cme::make-2d-world
		  :3d-world (cme::get-3d-world-named "Alv")
		  :3d-to-2d-projection projection))
       
       (view (make-view 
	      win
	      :2d-world 2d-world
	      :2d-to-window-transform
	      (make-4x4-coordinate-transform  (make-2d-to-window-matrix win (cv 0.0 0.0) 1.0))))
       (obj (make-instance 'perspective-transform-stare-point-object
			   :projection projection
			   :world (3d-world view)
			   :object-to-world-transform
			   (make-4x4-coordinate-transform
			    (make-object-to-parent-matrix (cv 256.9 4532.1 6015.4)))))
       (object-set (make-object-set (list obj)
				    :world (3d-world view)
				    :immediate-render-p t
				    :backing-store-p t
				    :direct-render-p t)))
  (setf (get-prop view :inhibit-scale-rot-rotation) t)
  (setf (get-prop view :world-center-of-rotation) rotation-center)
  (push object-set (object-sets view))
  ;;(setf (get-prop view :sun-vector) (cv 0.6082805 -0.1486257 0.7796828 0.0))
  (push-view view win)
  )


(pop (gui::object-sets (top-view)))
(pop-view (gui::selected-window))
(gui::clear-view-stack (gui::selected-window))
|#
