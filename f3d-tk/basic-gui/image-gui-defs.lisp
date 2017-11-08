(in-package :img)

;;; Definitions that are not part of the IMAGE subsystem because they depend on
;;; other packages and subsystems, in particular TRANSFORMS and MATH.


(defmethod set-linear-geom-transform :after  ((image image) transform-info)
  (let* ((parent (linear-geom-transform-info-image transform-info))
	 (parent-transform (and parent (image-to-2d-transform parent)))
	 (parent-2d-world (or (2d-world parent)
			      (setf (2d-world parent) (gui::make-2d-world))))
	 (to-parent-mat (linear-geom-transform-info-matrix transform-info))
	 (to-world-mat (if parent-transform
			   (math::multiply-matrices (transform-matrix parent-transform) to-parent-mat)
			   to-parent-mat)))
    (setf (2d-world image) parent-2d-world)
    (unless parent-transform
      (setf (image-to-2d-transform parent)
	    (transforms::make-4x4-coordinate-transform (math::make-4x4-identity-matrix)
						       :from-coordinate-system parent-2d-world
						       :to-coordinate-system parent-transform)))
    (setf (image-to-2d-transform image)
	  (transforms::make-4x4-coordinate-transform to-world-mat
						     :from-coordinate-system image
						     :to-coordinate-system parent-2d-world))))
