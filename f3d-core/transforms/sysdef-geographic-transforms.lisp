(in-package :cl-user)

(st:define-system "geographic-transforms"
    :required-systems '(transforms) 
    :files '("geographic-transforms.lisp"
	     "geographic-constants.lisp")
)
