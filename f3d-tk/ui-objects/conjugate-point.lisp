(in-package :obj)

;;; UNFINISHED UNFINISHED UNFINISHED UNFINISHED UNFINISHED

;;; Conjugate point which draws both 2d image positions and the projection of a 3d position.

(defstruct-class conjugate-point-object (3d-crosshair-object)
  ((2d-point-list :initform nil :initarg :2d-point-list :accessor 2d-point-list)))

;;; FIXME FOR VIDEO-FRAMES.
(defmethod initialize-instance :after ((object conjugate-point-object) 
				       &key 2d-point-list &allow-other-keys)
  (when (world object)
    (setf (2d-point-list object)
	  (loop for entry in 2d-point-list
		for (2d-world . rest) = entry
		with 3d-worlds = (list (world object))
		collect (cons (if (stringp 2d-world) 
				  (gui::get-2d-world-named 2d-world 3d-worlds)
				  2d-world)
			      (if (numberp (car rest))
				  (cv (car rest) (cadr rest) 0.0)
				  (car rest))))))
  )


#|
;(gui::temporal-view-time (gui::top-view))
;(view-2d-descr (gui::top-view))
;(setf (2d-point-list (gui::selected-object)) nil)
(video::video-frame (gui::top-view))
(let* ((view (gui::top-view))
       (video (video::view-video view)))
  (video::frame-for-timestamp video (gui::video-view-time view)))
|#

;; Crap!  package freedius-io isn't yet defined.
#+never
(defmethod freedius-io::io-attributes ((obj conjugate-point-object))
  `(:2d-point-list ,(loop for (2d-world uv) in (2d-point-list obj)
			  for name = (typecase 2d-world
				       (2d-world (name 2d-world))
				       (string 2d-world)
				       (list )
				       (otherwise (error "illegal 2d-world descr ~a" 2d-world)))
			  collect (bind-vector-elements (u v) uv
				    `(,name (cv ,u ,v))))))

			  
				     

;(fmakunbound 'get-2d-world-point)
(defmethod get-2d-world-point ((object conjugate-point-object) 2d-world-descr)
  (cdr (assoc 2d-world-descr (2d-point-list object) :test #'equal)))

(defvar *highlighting-conjugate-point-vertex* nil)

(defmethod draw-object ((object conjugate-point-object) view)
  ;;(format t "draw-object ~a ~a ~a~%" object *object-selection-mode* view )
  (if *object-selection-mode*
      (draw-fragments object)
      (with-class-slot-values crosshair-object (inside-size outside-size) object
	(let ((view-3d-position (transform-vector (transforms::object-to-view-transform object view)
						  (perturbed-object-origin object))))
	  (when view-3d-position ; view-position is NIL when object is behind camera.
	    (let ((2d-pt (get-2d-world-point object (view-2d-descr view))))
	      (when (or (null 2d-pt) (not (eql *highlighting-conjugate-point-vertex* 1)))
		(draw-crosshair view-3d-position inside-size outside-size))
	      (when 2d-pt
		(unless (eql *highlighting-conjugate-point-vertex* 0)
		  (let ((view-2d-position (transform-vector (transforms::2d-to-window-transform view)
							    2d-pt)))
		    (when view-2d-position
		      (draw-crosshair-x view-2d-position inside-size outside-size)
		      (unless nil	;*highlighting-conjugate-point-vertex*
			(with-gl-window-transform
			  (draw-dotted-line view-3d-position view-2d-position)))
                    
		      ))))))))))

;;; This is super ugly
(defmethod draw-fragments ((object conjugate-point-object))
  (let* ((view gui::*current-view*)
	 (3d-position (perturbed-object-origin object))
	 (view-2d-descr (view-2d-descr view))
	 (2d-pt (get-2d-world-point object view-2d-descr)))
    (glPushName 0)
    (glPointSize 2.0)
    (glLoadName 0)			; vertex 0
    (glBegin GL_POINTS)
    ;;(format t "draw-fragments ~a ~a~%" object 3d-position)
    (glVertex3dv 3d-position)
    (glEnd)
    ;;(format t "draw-fragments ~a ~a ~a~%" view-2d-descr 2d-pt gui::*gl-selection-pick-matrix-params* )
    (when 2d-pt
      (unwind-protect
	   (progn (glMatrixMode GL_PROJECTION)(glPushMatrix) 
		  (glMatrixMode GL_MODELVIEW) (glPushMatrix)
		  (gui::set-2d-matrices view)
		  (gui::maybe-set-gl-pick-matrix)
		  (glLoadName 1)	; vertex 1
		  (glBegin GL_POINTS)
		  (glVertex3dv 2d-pt)	; this is the 2d crosshair
		  (glEnd))
	;; clean-up
	(glMatrixMode GL_MODELVIEW) (glPopMatrix) 
	(glMatrixMode GL_PROJECTION) (glPopMatrix))
      )
    (glPopName)))

#|

(defmethod draw-fragments ((object conjugate-point-object))
  (let* ((view gui::*current-view*)
	 (3d-position (perturbed-object-origin object))
	 (view-2d-descr (view-2d-descr view))
	 (2d-pt (get-2d-world-point object view-2d-descr)))
    (glPushName 0)
    (glPointSize 2.0)
    (glLoadName 0)			; vertex 0
    (glBegin GL_POINTS)
    ;;(format t "draw-fragments ~a ~a~%" object 3d-position)
    (glVertex3dv 3d-position)
    (glEnd)
    ;;(format t "draw-fragments ~a ~a ~a~%" view-2d-descr 2d-pt gui::*gl-selection-pick-matrix-params* )
    (when 2d-pt
      (unwind-protect
	   (progn (glMatrixMode GL_PROJECTION)(glPushMatrix) 
		  (glMatrixMode GL_MODELVIEW) (glPushMatrix)
		  (gui::set-2d-matrices view)
		  (gui::maybe-set-gl-pick-matrix)
		  (glLoadName 1)	; vertex 1
		  (glBegin GL_POINTS)
		  (glVertex3dv 2d-pt)	; this is the 2d crosshair
		  (glEnd))
	;; clean-up
	(glMatrixMode GL_MODELVIEW) (glPopMatrix) 
	(glMatrixMode GL_PROJECTION) (glPopMatrix))
      )
    (glPopName)))

(describe (get-2d-world-point (gui::selected-object) (view-2d-descr (gui::top-view))))
|#

 
(defmethod highlight-object ((object conjugate-point-object) frag-descr view 
			     object-graphics-style highlight-graphics-style selectids)
  (declare (ignorable object-graphics-style highlight-graphics-style))
  (if frag-descr
      (unwind-protect
	   (progn
	     (glPushAttrib GL_ENABLE_BIT)
	     (draw-object-around object view  object-graphics-style selectids)
	     (with-gl-object-drawing (object view highlight-graphics-style)
	       (let ((*highlighting-conjugate-point-vertex* (vertex-id frag-descr)))
		 (draw-object-around object view highlight-graphics-style selectids))))
	(glPopAttrib))

      (draw-object-around object view highlight-graphics-style selectids)))


(defmethod set-conjugate-point-2d-position ((object conjugate-point-object) view uv)
  (let* ((conj-uv (get-2d-world-point object (view-2d-descr view))))
    (format t "set-conjugate-point-2d-position ~a~%" (list object view uv conj-uv))
    (bind-vector-elements (u v) uv
      (if conj-uv	  
	  (setf (aref conj-uv 0) u (aref conj-uv 1) v)
	  (push (cons (view-2d-descr view) (cv u v 0.0)) (2d-point-list object))))))

(defmethod set-conjugate-point-2d-position ((object conjugate-point-object) view uv)
  (let* ((conj-uv (get-2d-world-point object (view-2d-descr view))))
    (format t "set-conjugate-point-2d-position ~a~%" (list object view uv conj-uv))
    (bind-vector-elements (u v) uv
      (if conj-uv	  
	  (setf (aref conj-uv 0) u (aref conj-uv 1) v)
	  (setf (2d-point-list object)
		(putassoc (2d-point-list object)
			  (view-2d-descr view)
			  (cv u v 0.0)
			  :test #'equal))))))

(defmethod remove-conjugate-point-2d-position ((object conjugate-point-object) view)
  (setf (2d-point-list object)
	(remass (view-2d-descr view) (2d-point-list object) :test #'equal)))



;;(maybe-compile-file-load "$SUAV/code/multi-ray-intersection.lisp")

(defmethod triangulate-3d-position ((object conjugate-point-object))
  (loop for (2d-descr . uv) in (2d-point-list object)
	for 3d-to-2d-projection = (gui::view-2d-descr-2d-to-3d-projection 2d-descr)
	for cam-xyz = (transforms::camera-position 3d-to-2d-projection)
	for ray-dir = (transforms::camera-direction-vector-at-image-point 3d-to-2d-projection uv)
	;;collect (list cam-xyz ray-dir)
	collect cam-xyz into cam-origins
	collect ray-dir into cam-dirs
	finally (mv-bind (xyz errs) (math::ls-solve-ray-intersection  cam-origins cam-dirs)
		  (set-origin object xyz)
		  (format t "com-triangulate-3d-position: errs = ~{~a ~}~%" errs)
		  (gui::redisplay-all-world-views gui::*interactor* (world object))
		  )))

#|
;;; oops -- this depends on the video package being loaded.
(defmethod video-2d-points ((object conjugate-point-object))
  (sort (loop for (2d-descr . uv) in (2d-point-list object)
	      when (list 2d-descr)
		collect (destructuring-bind (video-frame timestamp) 2d-descr
			  (list (video::frame-for-timestamp (video::video-frame-video video-frame) timestamp)
				uv)))
	#'< :key #'car))

|#

;(video-2d-points (gui::selected-object))
;(triangulate-3d-position (gui::selected-object))

(in-package :gui)

(defmethod view-2d-descr-2d-to-3d-projection ((2d-descr 2d-world))
  (3d-to-2d-projection 2d-descr))

(defmethod view-2d-descr-2d-to-3d-projection ((2d-descr list))
  (destructuring-bind (2d-world time) 2d-descr
    (temporal-2d-to-3d-projection 2d-world time)))

#|
(in-package :video)

;;; Move this to vivid-viewer.lisp
(defmethod gui::temporal-2d-to-3d-projection ((2d-world video-frame) timestamp) 
  (let* ((video (video-frame-video 2d-world))
	 (pmat (gui::camera-matrix-for-frame video (frame-for-timestamp video timestamp))))
    (gui::projection-for-pmat pmat)))

(defmethod gui::temporal-2d-to-3d-projection ((video basic-video) timestamp) 
  (let* ((pmat (gui::camera-matrix-for-frame video (frame-for-timestamp video timestamp))))
    (gui::projection-for-pmat pmat)))

(in-package :gui)
|#


#|
(typep (2d-world (top-view))
(video::video-frame-timestamp (2d-world (top-view)))
|#

(defmethod com-add-conjugate-point-2d-position ((interactor interactor))
  (with-class-slots interactor (current-window-pos current-window) interactor  
    (let* ((object (caar (selected-objects interactor)))
	   (view (top-view current-window)))
      (obj::set-conjugate-point-2d-position object view 
				       (inverse-transform-vector (2d-to-window-transform view) 
								 current-window-pos))
      (setf (obj::vertex-id (selected-object-fragment interactor) ) 1)
      (start-popup-object-drag 'move-object-uv))))
	

(defmethod com-delete-conjugate-point-2d-position ((interactor interactor))
  (with-class-slots interactor (current-window-pos current-window) interactor  
    (let* ((object (caar (selected-objects interactor)))
	   (view (top-view current-window)))
      (obj::remove-conjugate-point-2d-position object view)
      (current-window-damaged interactor)
      )))
	
(defmethod com-triangulate-3d-position ((interactor interactor))
  (obj::triangulate-3d-position (selected-object interactor))
  (current-window-damaged interactor)
  )

(defmethod object-popup-menu-item-list :around ((ui augmented-cme-ui) 
						(object obj::conjugate-point-object))
  (append (call-next-method)
	  '(("Add Conj UV" :accel <beta-left> :eval (com-add-conjugate-point-2d-position *interactor*))
	    ("Del Conj UV" :accel <beta-middle> :eval (com-delete-conjugate-point-2d-position *interactor*))
	    ("Solve XYZ" :accel <beta-right> :eval (com-triangulate-3d-position *interactor*)))))

(defmethod move-object-uv ((object obj::conjugate-point-object) interactor)
  (with-class-slot-values interactor (window-motion) interactor
    (let* ((view (current-view interactor))
	   (world (object-view-world object view))
	   (fragment (selected-object-fragment interactor)))
      (when (and fragment (typep fragment 'object-vertex))
	(let* ((vertex-id (obj::vertex-id fragment)))
	  ;;(format t "move-object-vertex-uv ~a~%" vertex-id)
	  (if (eql vertex-id 1)
	      (let* ((2d-descr (view-2d-descr view))
		     (entry (assoc 2d-descr (obj::2d-point-list object) :test #'equal)))
		(when entry 
		  (let* ((2d-pos (cdr entry))
			 (delta-position (project-window-motion-to-world-motion
					  view (2d-world view) window-motion 2d-pos)))
		    (vector-add 2d-pos delta-position 2d-pos))))

	      (let ((delta-position
		     (project-window-motion-to-world-motion
		      view world
		      window-motion
		      (selected-object-world-position interactor world))))
		(move-world-position object delta-position world))
	      ))))))


#|
(in-package :obj)

(setq *conj1* (change-class (gui::selected-object) 'conjugate-point-object))

(setf (obj::2d-point-list *conj1*) nil)

(describe *conj1*)

(gui::view-2d-descr-2d-to-3d-projection (view-2d-descr (gui::top-view)) )

(setf (2d-point-list *conj1*) nil)

(let ((2d-ch (gui::selected-object)))
  (push (cons (parent 2d-ch) (origin 2d-ch)) (2d-point-list *conj1*)))

(setq *fs* (car (object-feature-sets *conj1*)))
(gui::remove-object *conj1* *fs*)
(gui::add-object *conj1* *fs*)
(/ 96000 

(/ 104500 3135) 100/3
(/ 95000 2850)
|#
  
