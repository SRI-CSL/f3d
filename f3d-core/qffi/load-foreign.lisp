(in-package :qffi)

;;;
;;; A temporary hack to make sure that everything works.  This will
;;; soon be replaced by extensions to asdf that will invoke cmake and
;;; do something reasonable:
;;;

#+never
(eval-when (load)
  (qffi::load-foreign-library
   ;; Temporary - we will need to extend asdf:
   "/Users/connolly/quicklisp/local-projects/cl-cme/arch/macosx-sbcl64/lib/libfreedius.dylib")
  (qffi::load-foreign-library
   ;; Temporary - we will need to extend asdf:
   "/Users/connolly/quicklisp/local-projects/cl-cme/arch/macosx-sbcl64/lib/libf3d-foreign-vector.dylib"
   )
  (qffi::load-foreign-library
   ;; Temporary - we will need to extend asdf:
   "/Users/connolly/quicklisp/local-projects/cl-cme/arch/macosx-sbcl64/lib/libf3dglffi.dylib"
   ))


