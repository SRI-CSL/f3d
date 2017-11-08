(in-package :cl-user)

;;; This is only needed in order to load some old CME site-models.
;;; Not sure where this belongs
(unless (find-package "USER")
  (defpackage "COMMON-LISP-USER"
    (:nicknames "USER")))


(defpackage :cme
  (:use :common-lisp :lcl :common-symbols :lx :math :transforms :gl :obj :img :gui)
  #+later
  (:import-from :math
		"4x4-coordinate-projection-covariance-model")

  #+never
  (:import-from :math
		"MAKE-AND-FILL-2D-ARRAY" "MAKE-AND-FILL-4X4-MATRIX" 
		"COPY-ARRAY"
		"INLINE-INNER-PROD" "INLINE-CROSS-PROD"
		"INLINE-EUCLIDEAN-LENGTH" "NORMALIZE-VECTOR-ELEMENTS"
		"SCALE-MATRIX" )

  (:import-from :math  "DECOMPOSE-OMEGA-PHI-KAPPA-ROTATION-MATRIX")

  (:import-from :st "AUTOLOAD-SYSTEM")
  
  (:import-from :lx "FILE-PROPERTY-LIST")

  (:import-from :transforms "FEET" "METERS" "MAKE-CAMERA-TO-2D-MATRIX" "DECOMPOSE-PROJECTION-MATRIX"
		)

  (:import-from :gui
		"DEFAULT-OBJECT-SETS"
		"MAKE-VERTEX-ARRAY"
		"MAKE-CME-FRAME"
		"MAKE-2D-WORLD"
		"FIXUP-2D-WORLD-NAME"
		"ADD-OBJECT" "REMOVE-OBJECT"
		"SET-WORLD"
		"READ-USGS-DEM-HEADER"
		"MAKE-DEM-IMAGE"
		"MAKE-DEM-COORDINATE-SYSTEM"
		)

  (:import-from :obj "GRAPHICS-STYLE")

  (:import-from :tk "CVV-ITEM" "CVV-ITEM-VALUE" "MAKE-CVV-PANEL"
		"GET-LISTBOX-SELECTED-ITEMS" "SET-LISTBOX-ITEMS"
		"POP-UP-PANEL" "POP-DOWN-PANEL" "QUIT-PANEL"
		"MENU-CHOOSE" "MAKE-MENU"  "GET-ITEM-VALUE" "SET-ITEM-VALUE")
  
  (:shadow "HASH-TABLE")

  (:export "VIEW-WINDOW" "TOP-VIEW"
	   "SITE-NAME"
	   "MENU-CHOOSE" "MAKE-MENU"
	   "ROTATE-TO" "CHANGE-SIZE" 
	   "LOCAL-UNITS-PER-METER"
	   "SET-TEXT"
	   "FEATURE-SETS" 
	   )

  )

;;; needed to load some old site models.
(defpackage :ic (:use :common-lisp)
  (:import-from :img  "LOAD-IMAGE-PYRAMID" "*PATH*" "GAUSS-2" )
  )

