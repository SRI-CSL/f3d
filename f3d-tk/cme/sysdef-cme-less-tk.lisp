(in-package :cl-user)

(st:define-system "cme-less-tk"
    :required-systems '(qffi
                        common-symbols
                        tkmin
                        lcl
                        lisp-extensions
                        ffi-extension
                        #+never cvv
                        math
                        gl-objects
                        more-gl-objects
                        basic-gui-lite
                        transforms
                        geographic-transforms
                        rpc-projections
                        terrain-models
                        model-io
                        radius-classes)
    :files '("cme-pkg.lisp"
	     ;"object-classes.lisp"
	     ;;"feature-sets.lisp"
	     ;;"worlds.lisp"
	     "load-cme-objects.lisp"
	     "camera-models.lisp"
             "site-glue.lisp"
	     "camera-model-io.lisp"
             "compat-defs.lisp"

	     "start-cme.lisp"
	     "cme-control-panel.lisp"
             "selection-panel.lisp"
	     "cvv-object-menus.lisp"
	      ))

