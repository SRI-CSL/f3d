(in-package :cl-user)

;;; Not sure the best way to handle these
;;; Alternatives:  export them from gl
;;;                import them to gui.


(defpackage :gui
  (:use :common-lisp :qffi :lcl :lx :gl 
	:math :common-symbols
        :tk   ; Can the tk package definition live here with GUI?
	:transforms :img :obj)
  
  ;;(:import-from :tk "FRAME-PANES-TK-SCRIPT")
  (:import-from :tk
                "INSTALL-TK-EVENT-HANDLER"
                "SET-DOC-LINE"
                "ADD-TO-MODIFIER-MASK"
                "REMOVE-FROM-MODIFIER-MASK"
                )

  (:import-from :img "NORMALIZED-IMAGE-ELEMENT-MIN-MAX" "COMPUTE-PHOTOMETRIC-TRANSFORM-FROM-CLIP-LEVELS"
		"COMPUTE-PHOTOMETRIC-TRANSFORM-FROM-IMAGE-MIN-MAX" "*DEFAULT-PHOTOMETRIC-TRANSFORM*")

  (:import-from :obj "*OBJECT-SELECTION-MODE*" "*GL-ENABLE-STIPPLES*" 
		"*GL-SELECTION-PICK-MATRIX-PARAMS*"
		;; "*BREAK-ON-DISPLAY-ERROR*"
		"*GL-SHADING-ENABLED*")
  (:import-from :gl "GLMAKECURRENT" "GLSWAPBUFFERS" "COLOR-NAME-TO-GL")

  
  (:export "PUSH-IMAGE" "MAKE-CME-FRAME")

  (:export "MAKE-GRIDDED-TERRAIN-MODEL" "MAKE-USGS-TERRAIN-MODEL" "MAKE-PLANAR-TERRAIN-MODEL" 
	   "MAKE-MULTI-TERRAIN-MODEL" "INTERSECT-CAMERA-RAY-WITH-TERRAIN-MODEL")

  (:export "GRAPHICS-STYLE" "GL-GRAPHICS-STYLE")

  (:export "OBJECT-COLLECTION" "FEATURE-SET" "2D-FEATURE-SET" "3D-FEATURE-SET" "OBJECT-FEATURE-SETS"
	   "FEATURE-SETS" "ADD-FEATURE-SET" "SAVE-FEATURE-SET" "*OBJECT-FEATURE-SETS*"
	   "FIND-FEATURE-SET" "FIND-OBJECT-NAMED")

  (:export "GL-OBJECT" "GL-2D-OBJECT-MIXIN" "GL-3D-OBJECT-MIXIN"
	   "GL-WORLD" "GL-2D-WORLD" "GL-3D-WORLD"
	   "2D-WORLDS" "2D-WORLD" "3D-WORLD"  "BASIC-WORLD-MIXIN"
	   "MAKE-2D-WORLD" "MAKE-3D-WORLD")

  (:export "GET-OR-MAKE-3D-WORLD" "GET-3D-WORLD-NAMED"
	   "GET-OR-MAKE-2D-WORLD" "GET-2D-WORLD-NAMED"
	   "ADD-IMAGE-TO-WORLD" "COORDINATE-SYSTEM" "PYRAMID-LIST" "SETUP-IMAGE-WORLDS" 
	   "CHANGE-PROJECTION"
	   )

  (:export "SELECT-CME-SITE")
  
  )



(import '(obj::SET-GL-PICK-MATRIX) 'gui)
 
