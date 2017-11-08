(in-package :cl-user)

;;; subsystem to load gl-object classes that depend on basic-gui

(st:define-system "more-gl-objects"
    :required-systems '(common-symbols gl transforms gl-objects basic-gui)
    :files '("image-object.lisp"
	     "object-labels.lisp" 
	     "conjugate-point.lisp"
;;	     "terrain-grid.lisp"
	     ))
