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
