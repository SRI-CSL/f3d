(in-package :cl-user)

(st:define-system "gl-objects"
    :required-systems '(common-symbols gl math transforms)
    :files '("obj-pkg.lisp"
	     "macros-gl-objects.lisp"
	     "gl-vertices.lisp"
	     "graphics-styles.lisp"
	     "gl-object-basics.lisp"
	     "non-4x4-projection-warp.lisp"
	     "gl-objects.lisp"
	     "3d-objects.lisp"
	     "2d-objects.lisp"
	     "object-motions.lisp"
	     ;"polygon-geometry.lisp"
	     ;;"object-ops.lisp" ; no this depends on gui package
             "time.lisp"
	     ))
