(in-package :cl-user)

(st:define-system "cme"
    :required-systems 
  '(qffi common-symbols ev-pathnames lcl lisp-extensions ffi-extension 
    cvv math gl-objects more-gl-objects basic-gui transforms geographic-transforms
    rpc-projections
    terrain-models
    model-io 
    radius-classes 
    )

  :files '("cme-pkg.lisp"
	   "load-cme-objects.lisp"
	   "camera-models.lisp"
	   "site-glue.lisp"
	   "camera-model-io.lisp"
	   "compat-defs.lisp" 

	   "start-cme.lisp"
	   "cme-control-panel.lisp"
	   "selection-panel.lisp"
	   "cvv-object-menus.lisp" ; this should move to the gui subsystem
	   
	   ))

#+never ;; experiment with running FREEDIUS without any CME site-model I/O -- It works!
(st:define-system "cme"
    :required-systems 
  '(qffi common-symbols ev-pathnames lcl lisp-extensions ffi-extension 
    cvv math gl-objects basic-gui more-gl-objects transforms geographic-transforms
    rpc-projections
    terrain-models
    model-io 
    radius-classes 
    )

  :files `("cme-pkg.lisp"
	   ,@(when nil
	       '("load-cme-objects.lisp"
		 "camera-models.lisp"
		 "site-glue.lisp"
		 "camera-model-io.lisp"
		 "compat-defs.lisp"))

	   "start-cme.lisp"
	   "cme-control-panel.lisp"
	   "cvv-object-menus.lisp" ; this should move to the gui subsystem
	   
	   ))

