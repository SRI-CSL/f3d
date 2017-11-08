(in-package :gui)

;;; VIEW GUI METHODS   - GUI specific 

(defvar *view-rotation-mode* :object)


(defmethod object-rotation-vertex ((vert t)) nil)

(defmethod object-rotation-vertex ((vert obj::object-vertex))
  (obj::vertex-array-vertex
   (obj::vertex-array (obj::object vert))
   (obj::vertex-id vert)))


;;; probably should have the interactor as the argument:
(defun rotation-center-from-mode (projection)
  (let ((selected (selected-objects *interactor*)))
    (when selected
      (destructuring-bind (obj ov) (car selected)
	(let ((vert (object-rotation-vertex ov)))
	  (when vert
	    (transform-vector
	     (object-to-world-transform obj)
	     vert)))))))

(defmethod rotate-view ((mode (eql :camera)) proj spec)
  (rotate-by proj spec (cv 0.0 0.0 0.0)))

(defmethod rotate-view ((mode (eql :object)) proj spec)
  (let ((vec (rotation-center-from-mode proj)))
    (if vec
	(transforms::stare-at-point proj spec vec)
      (rotate-view :camera proj spec))
    ))

(defmethod view-rotation-callback (menu item (keyword (eql 'done)) action value)
  (setf (get-prop menu :destroyed) t)
  (tk-destroy (widget menu)))

(defmethod view-rotation-callback (menu item (keyword (eql 'omega)) action value)
  (unless (get-prop menu :destroyed)
    (let* ((view (get-prop menu :view))
	   (proj (3d-to-2d-projection (current-view)))
	   (omega  (cvv-item-value menu keyword))
	   (spec (list :omega-degrees (-  omega (get-prop view :omega)))))
      (rotate-view *view-rotation-mode* proj spec)
      (setf (get-prop view :omega) omega)
      (redisplay view))))


(defmethod view-rotation-callback (menu item (keyword (eql 'phi)) action value)
  (unless (get-prop menu :destroyed)
    (let* ((view (get-prop menu :view))
	   (proj (3d-to-2d-projection (current-view)))
	   (phi  (cvv-item-value menu keyword))
	   (spec (list :phi-degrees (-  phi (get-prop view :phi)))))
      (rotate-view *view-rotation-mode* proj spec)
      (setf (get-prop view :phi) phi)
      (redisplay view))))


(defmethod view-rotation-callback (menu item (keyword (eql 'kappa)) action value)
  (unless (get-prop menu :destroyed)
    (let* ((view (get-prop menu :view))
	   (proj (3d-to-2d-projection (current-view)))
	   (kappa  (cvv-item-value menu keyword))
	   (spec (list :kappa-degrees (-  kappa (get-prop view :kappa)))))
      (rotate-view *view-rotation-mode* proj spec)
      (setf (get-prop view :kappa) kappa)
      (redisplay view))))

(defmethod view-rotation-callback (menu item (keyword t) action value)
  (format t "~%view-rotation-callback ~a ~a ~a ~a ~a" menu item keyword action value))

;;;
;;; Here, the panel is explicitly associated with a specific view, as
;;; it will not be meaningful to apply this to the current view of the
;;; interactor.
;;;
(defmethod view-rotation-cvv-panel ((view synthetic-view))
  ;; These should be sliders.  Need to implement the Tcl "scale" object...
  (let ((panel (make-cvv-panel `((omega "Omega" :slider :initial-value 0.0
					:from -360.0 :to 360.0 :resolution 0.1 :orient "horiz")
				 (phi "Phi" :slider :initial-value 0.0
				      :from -360.0 :to 360.0 :resolution 0.1 :orient "horiz")
				 (kappa "Kappa" :slider :initial-value 0.0
					:from -360.0 :to 360.0 :resolution 0.1 :orient "horiz")
				 (done nil :button :button-label "Done" :anchor w))
			       :title "Rotate View"
			       :callback-function 'view-rotation-callback)))
    (setf (get-prop panel :view)  view)
    (setf (get-prop view :omega) 0.0)
    (setf (get-prop view :phi) 0.0)
    (setf (get-prop view :kappa) 0.0)
    panel))






