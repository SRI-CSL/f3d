(in-package :cl-user)

(defpackage :FREEDIUS-IO (:use :common-lisp :lx :common-symbols :transforms))

#|
The following imports are here for two reasons:
  1) To eliminate the need for package qualifiers in the :freedius-io code.
  2) To make explicit (some of) the dependencies on other packages and subsystems.
     In particular, I would like to totally eliminate the need for the CME package.

|#

(in-package :OBJ)

(export '(unix-to-local-timestamp))
(import '(make-graphics-style graphics-style gl-graphics-style forward-referenced-object unix-to-local-timestamp) :freedius-io)
(import '(radius-mixin REMOVE-ATTRIBUTE) :freedius-io)

(in-package :gui)

(import '(add-object add-feature-set feature-sets feature-set COMPOSITE-OBJECT SAVE-FEATURE-SET
	  *object-feature-sets*)
	:freedius-io)

(import '(get-or-make-3d-world get-3d-world-named
	  get-or-make-2d-world get-2d-world-named
	  ;forward-referenced-object
	  ) 
	:freedius-io)

#+never
(loop for sym in '(feature-set feature-sets add-feature-set add-object save-feature-set
		   get-or-make-3d-world get-or-make-2d-world 3d-world 2d-world  
		   forward-referenced-object composite-object)
      collect (list sym (package-name (symbol-package sym))))

;;; MAKE-FRAME-CAMERA-AND-2D-WORLD defined in transform-io.lisp needs CME::GET-OR-MAKE-2D-WORLD

(in-package :MATH)
(import '(make-object-to-parent-matrix 
	  make-object-to-parent-rotation-matrix 
	  decompose-object-to-parent-rotation-matrix
	  coordinate-vector-general
	  ) :freedius-io)

(defpackage :FREEDIUS-MODELS-V0 (:use :common-lisp ))

;;; OBJECT MODELS are written using the PPRINT infrastructure in the :FREEDIUS-MODELS-V0 package  
;;; which is a bare minimim package for defining model files.
;;; There should be no qualified symbols in model files.
;;; A function is called by SAVE-FEATURE-SETS to test for package qualifiers.

(in-package :freedius-io)
	
(defvar *model-file-package* :FREEDIUS-MODELS-V0)
		  
;;; These are the only symbols (other than object class names) that need to be accessible from 
;;; the :FREEDIUS-MODELS-V0 package.
(import '(in-package 
	  common-symbols:feet st:autoload-system
	  with-coordinate-system with-lvcs lvcs 
	  gdc wgs84 nad27 ; FIXME:  there might be more geoid names - perhaps they should be keywords
	  with-feature-set create-object attributes
	  ; graphics-style
	  transforms::frame-camera
	  )
	:FREEDIUS-MODELS-V0)

;;; Perhaps this should be part of the LISP-EXTENSIONS subsystem.
;;;(defun symbol-accessible-in-package (sym package)
;;;  (or (keywordp sym)
;;;      (eq (find-symbol (symbol-name sym) package) sym)))

(defun symbol-accessible-in-package (sym package)
  (or (null sym)
      (keywordp sym)
      (find-symbol (symbol-name sym) package)))

(defun import-all-object-classes (&optional (pkg *model-file-package*))
  (loop for class in (lx::all-class-inferiors (find-class 'obj::basic-gl-object))
	for class-name = (class-name class)
	unless (symbol-accessible-in-package class-name pkg)
	  collect class-name into classes-to-import
	  ;; Not clear why, but image-object is already in the package
	  ;; when this is called.  You would think that
	  ;; symbol-accessible-in-package takes care of this, but I
	  ;; guess not...
	  finally (ignore-errors (import classes-to-import pkg))))


(st::add-system-initialization :model-io '(import-all-object-classes)) 

;(st:load-system :model-io :initialize t)
