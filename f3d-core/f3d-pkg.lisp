(in-package :cl-user)

;;; A "default" package for code that does not neatly fall into other
;;; categories.


(defpackage :f3d
  (:use :common-lisp
        :qffi
        :lcl
        :lx
	:math
        :common-symbols
	:transforms
        :img
        )
  

  (:import-from :img
                "NORMALIZED-IMAGE-ELEMENT-MIN-MAX"
                "COMPUTE-PHOTOMETRIC-TRANSFORM-FROM-CLIP-LEVELS"
		"COMPUTE-PHOTOMETRIC-TRANSFORM-FROM-IMAGE-MIN-MAX"
                "*DEFAULT-PHOTOMETRIC-TRANSFORM*")

  (:export "MAKE-GRIDDED-TERRAIN-MODEL"
           "MAKE-USGS-TERRAIN-MODEL"
           "MAKE-PLANAR-TERRAIN-MODEL" 
	   "MAKE-MULTI-TERRAIN-MODEL"
           "INTERSECT-CAMERA-RAY-WITH-TERRAIN-MODEL")

  
  )

#+sbcl
(progn
  ;; needed for sbcl >=1.3.15:
  (unlock-package "SB-IMPL")
  (unlock-package "SB-EXT"))
