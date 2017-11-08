(in-package :transforms)

;;; **************************  MOTIONS OF A FRAME CAMERA  **************************

(defmethod move-by ((projection frame-camera) delta-origin)
  (with-class-slot-values frame-camera (3d-to-camera-matrix) projection
    (let ((camera-to-3d-matrix (invert-matrix 3d-to-camera-matrix)))
      (move-by camera-to-3d-matrix delta-origin)
      (invert-matrix camera-to-3d-matrix 3d-to-camera-matrix)
      (update-transform projection))))

(defparameter *degrees-per-window-unit* .1)

;;;(defmethod change-azimuth-elevation-relative-to-position
;;;           ((transform frame-camera)
;;;            world-position window-motion 2d-to-window-transform)
;;;  (ignore 2d-to-window-transform)
;;;  (let ((w2c-matrix (3d-to-camera-matrix transform)))
;;;    (bind-vector-elements (du dv) window-motion
;;;      (let* ( ;; could use (transpose-4x4-matrix ) here 
;;;             (c2w-matrix (invert-matrix w2c-matrix))
;;;             (radians-per-window-unit (* *degrees-per-window-unit* (/ pi 180.0)))
;;;             (jacobian (transform-jacobian transform world-position))
;;;             (lzuzv (euclidean-length (aref jacobian 0 2) (aref jacobian 1 2)))
;;;             (zup (if (< lzuzv 1e-5) 0.0 (/ (aref jacobian 0 2) lzuzv)))
;;;             (zvp (if (< lzuzv 1e-5) 1.0 (/ (aref jacobian 1 2) lzuzv)))
;;;             (d-azimuth (* radians-per-window-unit (- (* dv zup) (* du zvp))))
;;;             (d-elev (* radians-per-window-unit (+ (* dv zvp) (* du zup))))
;;;             )
;;;
;;;        #+never
;;;        (setq d-elev (- d-elev)
;;;              d-azimuth (- d-azimuth))
;;;        
;;;        (rotate-relative-to-world-by c2w-matrix
;;;                                     (list :z-rot d-azimuth)
;;;                                     world-position)
;;;
;;;        ;;#+never
;;;        (let* ((win2world c2w-matrix)
;;;               (lwxwy (euclidean-length (aref win2world 0 2) (aref win2world 1 2)))
;;;               (princ-ray (cv (aref win2world 0 2)
;;;                              (aref win2world 1 2)
;;;                              (aref win2world 2 2)))
;;;               (elev-axis
;;;                (if (< lwxwy 1e-5)
;;;                    (cv (aref win2world 0 0) (aref win2world 0 1) (aref win2world 0 2))
;;;                    ;; rotate elevation about camera u-axis
;;;
;;;                    ;; this is the cross product of the principal ray with world z.
;;;                    ;; elevation angle rotates around this axis.
;;;                    (if t
;;;                        (vector-cross-product
;;;                         princ-ray
;;;                         (cv 0.0 0.0 1.0))
;;;                             
;;;                        (cv (- (aref win2world 1 2))
;;;                            (+ (aref win2world 0 2))
;;;                            0.0)))))
;;;          ;;(setq foo4 (list elev-axis princ-ray win2world))
;;;          (rotate-relative-to-world-by c2w-matrix
;;;                                       (make-orientation-matrix
;;;                                        (normalize-coordinate-vector elev-axis)
;;;                                        d-elev)
;;;                                       world-position
;;;                                       ))
;;;        
;;;        (invert-matrix c2w-matrix w2c-matrix)
;;;        (update-transform transform)
;;;        #+never
;;;        (progn 
;;;          ;;(print (decompose-camera-to-2d-matrix c2w-matrix))
;;;          (print (mv-list (math::decompose-object-to-parent-rotation-matrix 
;;;                           :omega-phi-kappa (invert-matrix w2c-matrix))))
;;;          )
;;;        ))))


;;; Mon Dec 13 2004 new version based on AZIMUTH-ELEVATION-TWIST decomposition
(defmethod change-azimuth-elevation-relative-to-position
	   ((transform frame-camera)
	    world-position window-motion 2d-to-window-transform)
  (ignore 2d-to-window-transform)
  (bind-vector-elements (du dv) window-motion
    (let* ((w2c-matrix (3d-to-camera-matrix transform))
	   (c2w-matrix (invert-matrix w2c-matrix))
	   (d-azimuth (* *degrees-per-window-unit* du))
	   (d-elev (- (* *degrees-per-window-unit* dv))))
      (mv-bind (azimuth elevation twist) 
	  (math::decompose-object-to-parent-rotation-matrix :azimuth-elevation-twist c2w-matrix )
	(let* ((new-azimuth (incf azimuth d-azimuth))
	       (new-elevation (max -90.0 (min 90.0 (incf elevation d-elev))))
	       (new-c2w-matrix (math::make-object-to-parent-rotation-matrix 
				:azimuth-elevation-twist (list new-azimuth new-elevation twist))))
	  (pre-multiply-transform-matrix c2w-matrix 
					 (math::g* new-c2w-matrix w2c-matrix)
					 :center-of-rotation-vector world-position)
	  (invert-matrix c2w-matrix w2c-matrix)
	  (update-transform transform))))))


(defmethod rotate-about-principal-ray ((transform frame-camera) delta-pixels)
  (let* ((w2c-matrix (3d-to-camera-matrix transform))
	 (c2w-matrix (invert-matrix w2c-matrix))
	 (degrees-per-pixel .1)
	 (z-rotation (math::make-4x4-rotation-matrix :z-deg (* degrees-per-pixel delta-pixels)))
	 )
    (transforms::rotate-transform-matrix c2w-matrix z-rotation :pre-multiply-p nil)
    (invert-matrix c2w-matrix w2c-matrix)
    (update-transform transform)))

(defparameter *rotate-about-principal-ray-degrees-per-pixel* .1)

(defmethod rotate-about-principal-ray ((transform frame-camera) delta-pixels)
  (let* ((w2c-matrix (3d-to-camera-matrix transform))
	 (c2w-matrix (invert-matrix w2c-matrix))
	 (z-rotation (math::make-4x4-rotation-matrix 
		      :z-deg (* *rotate-about-principal-ray-degrees-per-pixel* delta-pixels)))
	 )
    (transforms::rotate-transform-matrix c2w-matrix z-rotation :pre-multiply-p nil)
    (invert-matrix c2w-matrix w2c-matrix)
    (update-transform transform)))

(defparameter *rotate-about-camera-ray-degrees-per-pixel* .1)

(defmethod rotate-about-camera-ray ((frame-camera frame-camera) gnd-position delta-pixels)
  (let* ((w2c-matrix (3d-to-camera-matrix frame-camera))
	 (c2w-matrix (invert-matrix w2c-matrix))
	 (camera-ray (normalize-coordinate-vector 
		      (vector-difference (origin frame-camera) gnd-position)))
	 (rotation (math::make-4x4-rotation-matrix 
		    camera-ray 
		    (radians (* *rotate-about-camera-ray-degrees-per-pixel* delta-pixels))))
	 )
    (transforms::rotate-transform-matrix c2w-matrix rotation :pre-multiply-p t)
    (invert-matrix c2w-matrix w2c-matrix)
    (update-transform frame-camera)))



#|
(top-view ".frm.f2.gl2")
(setf (view-image (top-view ".frm.f2.gl2")) nil)

(3d-to-2d-projection (top-view ".frm.f2.gl2"))
(cadr (3d-to-2d-projection (top-view ".frm.f2.gl2")))

(change-azimuth-elevation-relative-to-position
 (cadr (3d-to-2d-projection (top-view ".frm.f2.gl2")))
 (CV -71.333 4541.89 6106.684)
 (cv 0.0 40.0 ))
|#


;;; rotation goes from world-coordinates to object coordinates
(defmethod set-transform-origin-after-rotation
    (transform-matrix rotation center-of-rotation-vector)
  (declare (type (or null (simple-array double-float (*))) center-of-rotation-vector))
  (let* ((m transform-matrix)
	 (xo (aref m 0 3))
	 (yo (aref m 1 3))
	 (zo (aref m 2 3))
	 (xr 0.0) (yr 0.0) (zr 0.0))
    (declare (type 4x4-matrix m))
    (declare (double-float xo yo zo xr yr zr))
    (if center-of-rotation-vector
	(mv-setq (xr yr zr) (coordinate-vector-elements center-of-rotation-vector))
	(setq xr xo yr yo zr zo))	; default to rotation about current object origin
    (let* ((dx1 (- xr xo))
	   (dy1 (- yr yo) )
	   (dz1 (- zr zo) )
	   (r rotation)
	   (x2 0.0) (y2 0.0) (z2 0.0))
      (declare (double-float dx1 dy1 dz1 x2 y2 z2))
      (inline-matrix-times-vector r (dx1 dy1 dz1 0.0) (x2 y2 z2))
      (setf (aref m 0 3) (- xr x2)
	    (aref m 1 3) (- yr y2)
	    (aref m 2 3) (- zr z2))))
  transform-matrix)

(defparameter *rotate-transform-matrix-orthogonalize* nil)

(defun rotate-transform-matrix
    (matrix rotation 
     &key center-of-rotation-vector pre-multiply-p
     (orthogonalize *rotate-transform-matrix-orthogonalize*))
  (declare (special *temporary-matrix*))
  (setq rotation
	(cond ((arrayp rotation)
	       rotation)
	      ((and (arrayp (car rotation)) (= 2 (array-rank (car rotation))))
	       (car rotation))
	      (t (apply #'math::make-object-to-parent-orientation-matrix rotation))))
  (let ((x0 (aref matrix 0 3))
	(y0 (aref matrix 1 3))
	(z0 (aref matrix 2 3)))
    
    (if pre-multiply-p
	(multiply-matrices rotation matrix matrix)
	(multiply-matrices matrix rotation matrix))
    
    (setf (aref matrix 0 3) x0)
    (setf (aref matrix 1 3) y0)
    (setf (aref matrix 2 3) z0)
    
    (when center-of-rotation-vector
      (set-transform-origin-after-rotation
       matrix
       (if pre-multiply-p
	   rotation
	   ;; matrix to pre-multiply by rather than post-multiply
	   (math:multiply-matrices (math:multiply-matrices matrix rotation)
				   (math:invert-matrix matrix)))
       center-of-rotation-vector))

    (when orthogonalize (math::orthogonalize-rotation-matrix matrix))
    ;;(check-rotation-matrix matrix)
    ;;(break)
    matrix))
  
