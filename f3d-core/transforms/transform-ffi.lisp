(in-package :transforms)

(def-foreign-struct coordinate_system
    (c++header :type :long)  
    ;;(name :type (:pointer :simple-string))
  (name :type (:pointer :character))
  (dimensionality :type :int)
  (transform-list :type :int) ; (:pointer pointer_list)
  )

#|
(def-foreign-struct linear_coordinate_transform_3d_to_3d
    (c++header :type :long)
    (inverse :type (:pointer linear_coordinate_transform_3d_to_3d ))
  (from-cs :type (:pointer coordinate_system))
  (to-cs :type (:pointer coordinate_system))
  ;;(mat :type (:pointer (:array :double-float (4 4 ))))
  (mat :type (:array :double-float (4 4 )))
  )

(undefine-foreign-type 'linear_coordinate_transform_3d_to_3d)
|#

;;; in allegro self references (linear_coordinate_transform_3d_to_3d)
;;; need to be defined a second time
(def-foreign-struct linear_coordinate_transform_3d_to_3d
    (c++header :type :long)
    (from-cs :type (:pointer coordinate_system))
  (to-cs :type (:pointer coordinate_system))
  (inverse :type (:pointer linear_coordinate_transform_3d_to_3d ))
  ;;(mat :type (:pointer (:array :double-float (4 4 ))))
  (mat :type (:array :double-float (4 4 )))
  )

(def-foreign-struct linear_4x4_coordinate_projection
    (c++header :type :long)
    (from-cs :type (:pointer coordinate_system))
  (to-cs :type (:pointer coordinate_system))
  (inverse :type (:pointer linear_coordinate_transform_3d_to_3d ))
  ;;(mat :type (:pointer (:array :double-float (4 4 ))))
  (mat :type (:array :double-float (4 4 )))
  )


(def-foreign-function (make_3d_to_3d_coordinate_transform
		       (:name "make_3d_to_3d_coordinate_transform")
		       (:return-type (:pointer linear_coordinate_transform_3d_to_3d ))
		       )
    4x4-matrix ; of double floats
  )

(def-foreign-function (make_4x4_linear_coordinate_projection
		       (:name "make_4x4_linear_coordinate_projection")
		       (:return-type (:pointer linear_4x4_coordinate_projection ))
		       )
    4x4-matrix ; of double floats
  )


;; (def-foreign-synonym-type cv3 (:pointer (:array :double-float (3))))

(def-foreign-function (transform_vector (:name "transform_vector"))
    trans ; (trans (:pointer coordinate-transform))
  from_vector ; array double-float 3
  to_vector) ; array double-float 3

(def-foreign-function (inverse_transform_vector (:name "inverse_transform_vector"))
    trans ; (trans (:pointer coordinate-transform))
  from_vector ; array double-float 3
  to_vector) ; array double-float 3

(def-foreign-function (pinhole_jacobian (:name "pinhole_jacobian"))
    mat ; 4x4 double
  from_vector ; array double-float 3
  matrix) ; array double-float 3x3


(defun pinhole-partial-derivitives (4x4mat position)
  (let ((mat (make-array '(3 3) :element-type 'double-float :initial-element 0.0d0)))
    (pinhole_jacobian 4x4mat position mat)
    mat))

#|
(setq t1 (make_3d_to_3d_coordinate_transform
	  (make-array '(4 4) :element-type 'double-float
		      :initial-contents
		      '((1.0 0.0 0.0 100.0)
			(0.0 10.0 0.0 200.0)
			(0.0 0.0 100.0 300.0)
			(0.0 0.0 0.0 1.0)))))


(describe t1)

(setq to_vector (make-array0 3 :element-type 'double-float))
(setq from-vector (make-array0 3 :element-type 'double-float
			      :initial-contents '(1.0 .1 .01)))
(transform_vector t1 from-vector to_vector)
(inverse_transform_vector t1 from-vector to_vector)
(describe to_vector)
(progn to_vector)

|#
