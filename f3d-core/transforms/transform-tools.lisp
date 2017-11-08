(in-package :gui)

;;; Should this file go into the basic-gui directory.

;;; This file requires TRANSFORMS, GL-OBJECTS, and parts of BASIC-GUI subsystems.

;;; These messages should go the documentation window
(defun report-error-to-gui (message)
  ;;(error message)
  (format t "~a~%" message)
  )

;;; *******************************  NORTH-DIRECTION AND UP-DIRECTION  *******************************

(defmethod geocentric-surface-normal ((world gl-3d-world) position)
  (let ((lvcs-to-geocentric-transform (lvcs-to-geocentric-transform world)))
    (when lvcs-to-geocentric-transform
      (normalize-coordinate-vector (transform-vector lvcs-to-geocentric-transform position)))))

(defmethod geodetic-surface-normal ((world gl-3d-world) position)
  (let* ((lvcs-to-gcc (lvcs-to-geocentric-transform world)))	 
    (when lvcs-to-gcc
      (let* ((ellipsoid (ellipsoid (to-coordinate-system lvcs-to-gcc)))
	     (gcc-to-gdc (transforms::gcc-to-gdc-transform ellipsoid))
	     (gcc-pos (transform-vector lvcs-to-gcc position)))
	(bind-vector-elements (long lat elev) (transform-vector gcc-to-gdc gcc-pos)
	  (math::polar-to-cartesian (cv (- 90.0 long) lat 1.0)))))))

#|
(let* ((world (3d-world (top-view)))
       (lvcs-to-gcc (lvcs-to-geocentric-transform world))
       (pos (selected-object-position)))
  (labels ((lvcs-normal (gcc-normal) 
	     (transform-direction-vector (inverse-transform lvcs-to-gcc) gcc-normal))
	   (elevation-angle (gcc-normal)
	     (bind-vector-elements (az elev) 
		 (math::cartesian-to-polar (lvcs-normal gcc-normal))
	       elev)))
	     
    (list (geocentric-surface-normal world pos)
	  (geodetic-surface-normal world pos)
	  (elevation-angle (geocentric-surface-normal world pos))
	  (elevation-angle (geodetic-surface-normal world pos))
	  ))  )
|#

;;; Should these functions move to photogrammetry/geographic-transforms.lisp ??
;;; POSITION is in LVCS coordinates.
(defmethod north-direction-in-tangent-plane ((world gl-3d-world) position)
  (let ((geocentric-surface-normal (geocentric-surface-normal world position)))
    (if geocentric-surface-normal
	(let ((gz (coordinate-vector 0.0 0.0 1.0))) ; north axis in geocentric coordinates.
	  (transform-direction-vector 
	   (inverse-transform (lvcs-to-geocentric-transform world))
	   (vector-cross-product (vector-cross-product geocentric-surface-normal gz)
				 geocentric-surface-normal)
	   (transform-vector (lvcs-to-geocentric-transform world) position)))

	(coordinate-vector 0.0 1.0 0.0) ; assume LVCS Y-axis points North.
	)))
  
(defmethod up-direction-in-tangent-plane ((world gl-3d-world) position)
  (let ((geocentric-surface-normal (geocentric-surface-normal world position)))
    (if geocentric-surface-normal
	(normalize-coordinate-vector
	 (transform-direction-vector 
	  (inverse-transform (lvcs-to-geocentric-transform world))
	  geocentric-surface-normal
	  (transform-vector (lvcs-to-geocentric-transform world) position)))

	(coordinate-vector 0.0 0.0 1.0) ; assume LVCS Z-axis points Up.
	)))

(defmethod world-center ((world transforms::cartesian-coordinate-system))
  (get-prop world :world-center)
  ;; if get-prop returns NIL, should this return the center of the bounding-box of the world?
  )

;;#+missing-functions
(defun lvcs-position-for-2d-world (2d-world &optional lvcs-position)
  (or lvcs-position
      (let ((3d-world (3d-world 2d-world)))
	(unless 3d-world
	  (progn (report-error-to-gui "View does not have a 3d world")
		 (abort)))
	(or (world-center 3d-world)
	    (let ((3d-to-2d-projection (3d-to-2d-projection 2d-world)))
	      (unless 3d-to-2d-projection
		(progn (report-error-to-gui
			"View does not have a 3d-to-2d-projection")
		       (abort)))
	      (let ((2d-position (or (world-center 2d-world)
				     (bind-vector-elements (umin umax vmin vmax)
					 (bounding-box 2d-world)
				       (cv (* .5 (+ umin umax)) (* .5 (+ vmin vmax))))
				     (cv 0.0 0.0 ))))
		(or (intersect-camera-ray-with-terrain-model 3d-to-2d-projection
							     2d-position 
							     (find-terrain-model (3d-world 2d-world)))
		    (intersect-camera-ray-with-z-plane 3d-to-2d-projection
						       2d-position 0.0))))))))
#|
(world-center (2d-world (top-view)))
(world-center (3d-world (top-view)))
(lvcs-position-for-2d-world (2d-world (top-view)))
|#

(defun north-direction-in-2d-world (2d-world &optional lvcs-position)
  (let ((3d-world (3d-world 2d-world))
	(3d-to-2d-projection (3d-to-2d-projection 2d-world)))
    (unless 3d-world
      (progn (report-error-to-gui "View does not have a 3d world")
	     (abort)))
    (unless 3d-to-2d-projection
      (progn (report-error-to-gui "View does not have a 3d-to-2d-projection")
	     (abort)))
    (unless lvcs-position
      (setq lvcs-position (lvcs-position-for-2d-world 2d-world lvcs-position)))
    (when lvcs-position
      (let* ((north-direction
	      (transform-direction-vector 3d-to-2d-projection
					  (north-direction-in-tangent-plane 3d-world lvcs-position)
					  lvcs-position)))
	(quantize-2d-direction-vector north-direction 5.0)))))

(defun north-direction-in-view-window (view &optional lvcs-position)
  (transforms::4x4-transform-direction-vector (2d-to-window-transform view)
				  (north-direction-in-2d-world (2d-world view) lvcs-position)))


;;; Projection is considered to be near nadir is angle between camera ray to vertical
;;; is less than 1 degrees.
(defparameter *projection-near-nadir-threshold* (cos (radians 1.0)))

;;; quantization was used in RCDE to reduce the number of lazy-images used
;;; for rotations.  No quantization is needed with OpenGL texture-mapped image display.
(defparameter *up-direction-degrees-quantization* nil)


(defun up-direction-in-2d-world (2d-world &optional lvcs-position)
  (let ((3d-world (3d-world 2d-world))
	(3d-to-2d-projection (3d-to-2d-projection 2d-world)))
    (unless 3d-world
      (progn (report-error-to-gui "View does not have a 3d world")
	     (abort)))
    (unless 3d-to-2d-projection
      (progn (report-error-to-gui "View does not have a 3d-to-2d-projection")
	     (abort)))
      
    (unless lvcs-position
      (setq lvcs-position (lvcs-position-for-2d-world 2d-world lvcs-position)))
    (when lvcs-position
      (let* ((world-up-direction (up-direction-in-tangent-plane 3d-world lvcs-position))
	     (2d-up-direction
	      (transform-direction-vector 3d-to-2d-projection world-up-direction lvcs-position))
	     (camera-ray-direction (camera-direction-vector 3d-to-2d-projection lvcs-position))
	     (view-dot-vertical (- (vector-inner-product
				    (normalize-coordinate-vector camera-ray-direction)
				    (normalize-coordinate-vector world-up-direction))))
	     )
	(values (when (< view-dot-vertical *projection-near-nadir-threshold*)
		  (quantize-2d-direction-vector 2d-up-direction *up-direction-degrees-quantization*))
		view-dot-vertical)))))


(defun quantize-2d-direction-vector (vector degrees-quantization)
  (let* ((ang (atan (aref vector 1) (aref vector 0))))
    (when degrees-quantization
      (setq ang (let ((precision (radians degrees-quantization)))
		  (* precision (round ang precision)))))
    (coordinate-vector (cos ang) (sin ang))
    ))
	     
(defun up-direction-in-view-window (view &optional lvcs-position)
  (let ((vector (transforms::4x4-transform-direction-vector
		 (2d-to-window-transform view)
		 (up-direction-in-2d-world (2d-world view) lvcs-position))))
    (when vector
      (setf (aref vector 1) (- (aref vector 1)))
      (normalize-coordinate-vector vector))))


(defun up-direction-in-image (image &optional lvcs-position)
  (let* ((2d-world (2d-world image))
	 (up-direction-in-2d-world (up-direction-in-2d-world 2d-world lvcs-position))
	 (vector (and up-direction-in-2d-world
		      (transforms::4x4-transform-direction-vector
		       (inverse-transform (image-to-2d-transform image))
		       up-direction-in-2d-world))))
    (when vector
      (normalize-coordinate-vector vector))))

(defun north-direction-in-image (image &optional lvcs-position)
  (let* ((2d-world (2d-world image))
	 (north-direction-in-2d-world (north-direction-in-2d-world 2d-world lvcs-position))
	 (vector (and north-direction-in-2d-world
		      (transforms::4x4-transform-direction-vector
		       (inverse-transform (image-to-2d-transform image))
		       north-direction-in-2d-world))))
    (when vector (normalize-coordinate-vector vector))))

;;; Constrain new-2d-to-window such that
;;;   (inverse-transform-vector new-2d-to-window window-pos)
;;; = (inverse-transform-vector 2d-to-window window-pos)

;;; If angle-or-vector is a number, it should be the azimuth angle of North in the image.
(defun rotate-image (view angle-or-vector window-pos &key scale (doit t))
  (let* ((2d-to-window (2D-TO-WINDOW-TRANSFORM view))
	 (scale (or scale (transform-scale-factor 2d-to-window window-pos)))
	 (azimuth (if (numberp angle-or-vector)
		      angle-or-vector
		      (atan (aref angle-or-vector 1) (aref angle-or-vector 0))))
	 (theta (- azimuth (* .5 pi)))
	 ;; Theta is really azimuth in radians
	 (mat (make-and-fill-4x4-matrix (* scale (cos theta)) (* scale (sin theta)) 0.0 0.0
					(* scale (sin theta)) (- (* scale (cos theta))) 0.0 0.0
					0.0 0.0 1.0 0.0
					0.0 0.0 0.0 1.0))
	 (window-pos2 (matrix-times-vector mat (inverse-transform-vector 2d-to-window window-pos)))
	 )
    (incf (aref mat 0 3) (- (aref window-pos 0) (aref window-pos2 0)))
    (incf (aref mat 1 3) (- (aref window-pos 1) (aref window-pos2 1)))
    (let ((new-2d-to-window (make-4x4-coordinate-transform mat)))
      (when doit
	(setf (2D-TO-WINDOW-TRANSFORM view) new-2d-to-window)
	(gui::window-damaged *interactor* (gui::view-window view))
	)
      
      (values new-2d-to-window 2d-to-window
	      (inverse-transform-vector 2d-to-window window-pos)
	      (inverse-transform-vector new-2d-to-window window-pos)
	      ))))



#|

(maybe-compile-file-load "$FREEDIUS/lisp/transforms/transform-tools.lisp")

(describe (2D-TO-WINDOW-TRANSFORM (top-view)))
(describe (top-view))
(describe *interactor*)
(transform-scale-factor (2D-TO-WINDOW-TRANSFORM (top-view)) nil)
(describe (2D-TO-WINDOW-TRANSFORM (top-view)))
(describe (gui::image-to-2D-TRANSFORM (gui::view-image (top-view))))

(setq dir (up-direction-in-image (gui::view-image (top-view)) (gui::WORLD-CENTER-OF-ROTATION *interactor*)))
(setq dir (north-direction-in-image (gui::view-image (top-view)) (gui::WORLD-CENTER-OF-ROTATION *interactor*)))


(rotate-image (top-view) dir (gui::CURRENT-WINDOW-POS *interactor*))
(rotate-image (top-view) dir (gui::CURRENT-WINDOW-POS *interactor*) :scale 1.0)

(describe (rotate-image (top-view) dir (gui::CURRENT-WINDOW-POS *interactor*)))
(up-direction-in-2d-world (2d-world (top-view)) (gui::WORLD-CENTER-OF-ROTATION *interactor*))
#(-0.8642065987423104 0.503137113210949)
(up-direction-in-image (gui::view-image (top-view)) (gui::WORLD-CENTER-OF-ROTATION *interactor*))
#(0.503137113210949 0.8642065987423104 0.0)

(transform-direction-vector (3d-to-2d-projection (top-view)) (cv 0.0 0.0 1.0) (gui::WORLD-CENTER-OF-ROTATION *interactor*))
#(-0.4132195992724519 0.24259048241219716 0.9999998902458174)
(normalize-coordinate-vector (cv -0.4132195992724519 0.24259048241219716))
#(-0.862371553701206 0.5062759162422877)
(geocentric-surface-normal (3d-world (top-view)) (gui::WORLD-CENTER-OF-ROTATION *interactor*))
(up-direction-in-tangent-plane (3d-world (top-view)) (gui::WORLD-CENTER-OF-ROTATION *interactor*))


(describe (transforms::surrogate-projection (3d-to-2d-projection (top-view))))

|#
