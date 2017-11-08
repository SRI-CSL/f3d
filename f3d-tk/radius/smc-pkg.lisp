(in-package :cl-user)

;;; :cme package must exist before this is loaded.
#+never
(eval-when (compile load eval)
  (loop for name in '("ROOF-TYPE" "SITE-ORIENTATION")
	with pkg = (find-package :cme)
	do (intern name pkg)))

;;;
;;; Removed "GUI" from the :use list to allow us to load this system
;;; as part of f3d-geom.
;;;

(defpackage :smc
  (:use :common-lisp
        :lcl
        :lx
        :transforms
        :math
        :gl 
	;; :cme
	:obj
        ;;:gui
        :common-symbols)

	    
  (:import-from :lx "DEFINE-FASD-FORM-INIT-PLIST")
  ;(:import-from :cme "ROOF-TYPE" "SITE-ORIENTATION" "HOUSE-OBJECT" "CUBE-OBJECT" "3D-RULER-OBJECT" )
   
  (:import-from :obj "*RADIUS-CLASS-GRAPHICS-ATTRIBUTES-ALIST*" 
		"RADIUS-MIXIN" "ADD-ATTRIBUTE" "REMOVE-ATTRIBUTE")
  )



;;; This is needed for loading some old cme site models.
(import 'lx::FILE-PROPERTY-LIST :cl-user)
