(in-package :cl-user)

(st:define-system :terrain-models
    :required-systems '(:transforms :image :geographic-transforms) 
    :files
    '("terrain-models.lisp"
      "dtm-intersect.lisp"
      "usgs-dem.lisp"
      ))
      
