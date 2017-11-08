(in-package :cl-user)

;;; config::compile-for-runtime-only-disksave can be flushed.
;;; The default image iref/iset functions are always compiled.
(defvar config::compile-for-runtime-only-disksave nil)
(defvar config::weak-eval-cache nil)
(defvar config::standalone-image-subsystem nil)

(eval-when (eval load compile)
  (when config::COMPILE-FOR-RUNTIME-ONLY-DISKSAVE
    (pushnew :COMPILE-FOR-RUNTIME-ONLY-DISKSAVE *features*))    
) ; end eval-when

;; touch $F3D/lisp/img/array-image.lisp $F3D/lisp/img/paged-image.lisp $F3D/lisp/img/band-interleaved-image.lisp vector-image.lisp

;;; this isn't a runtime option -- it affects the compile-time generation of iref/iset macros
(defvar config::*paged-image-allowed-tile-offset-bits* '(16))
;;(defvar config::*paged-image-allowed-tile-offset-bits* '(13 16 18 20))
;;(defvar config::*paged-image-allowed-tile-offset-bits* nil)

(defvar config::*default-vector-image-nbands* '(2 3 4))

(st::define-system "image"
    :system-class 'st::mixed-lisp-system
    ;;:libraries (list "$FREEDIUS_ARCH/lib/libfreedius")
    :libraries '("libfreedius")
    :required-systems '(common-symbols qffi foreign-vector ev-pathnames lisp-extensions math)
    :files `("image-pkg.lisp"
	     "image-defs.lisp"
	     "image-ffi.lisp"
	     ;; "tiff-ffi.lisp" ;; This isn't needed when using the tiff-io code in libFREEDIUS.
	     "array-image.lisp"
	     "paged-image.lisp"
	     "vector-image.lisp"
	     ;;"color-image.lisp" ; obsolete
	     "band-interleaved-image.lisp"
	     "image-mapping.lisp"
	     "image-ops.lisp"
	     ,@(if config::weak-eval-cache '("image-gc.lisp"))
	     ,@(unless config::standalone-image-subsystem  
		       '( "image-pyramids.lisp"))
	     #+(and :sbcl :x86-64) "mmap-array-image.lisp"
	     ))


#|

Note:  image-pyramids.lisp requires :MATH and :TRANSFORMS subsystems.  SUCKS.

       image-mapping.lisp requires :MATH

       vector-image.lisp  requires coordinate-vector defs in math
|#