(in-package :cl-user)

(defpackage :img (:use :common-lisp :qffi :lcl :lx :ev-pathnames :common-symbols)
	    ;;(:import-from :common-symbols "TRANSFORM-MATRIX")
	    (:export "IMAGE-DIMENSIONS" "IMAGE-X-DIM" "IMAGE-Y-DIM"
		     "IMAGE-ELEMENT-TYPE"
		     "IMAGE-BLOCK-X-DIM" "IMAGE-BLOCK-Y-DIM" "IMAGE-BLOCK-DIMS"
		     "IMAGE-ID"
		     "IMAGE-ELEMENT-SIZE" "MAKE-IMAGE"
		     "IMAGE-GETLINE" "IMAGE-PUTLINE"
		     "IMAGE-GET-RECTANGLE" "IMAGE-GET-RECTANGLE-BORDERED"
		     "COPY-IMAGE"

		     "IREF" "DIREF" "INTERPOLATE-IREF" "C-IMAGE" "DISET"
		     "WITH-IREF-CHECKING"

		     "C-IMAGE" "LOAD-IMAGE" "CGAUSS_CONVOLVE_DECIMATE"
		     
		     "IMAGE_PAGE_POOL_STATS"
		     "RELEASE_IMAGE_PAGES" "RESIZE-IMAGE-PAGE-POOL"
		     
		     "IMAGE-PROP" "REMOVE-IMAGE-PROP"

		     
		     "LOAD-IMAGE-PYRAMID" "GET-IMAGE-PYRAMID-LEVEL"
		     "GET-IMAGE-PYRAMID-LEVEL" "SET-PYRAMID-LEVEL"
		     "TOP-OF-IMAGE-PYRAMID" "TOP-OF-IMAGE-HIERARCHY"
		     "IMAGE-PYRAMID-LOADABLE-P"

		     ;; needed for defpackage :ic in cme-pkg-def.lisp
		     "LOAD-IMAGE-PYRAMID" "*PATH*" "GAUSS-2"
		     "IMAGE-FOR-DISPLAY"
		     
		     )
	    ) 


