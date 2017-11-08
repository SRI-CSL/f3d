(in-package :cl-user)

(defpackage :gl
  (:use :common-lisp :qffi :lcl :lx :common-symbols)
  (:export "GLMAKECURRENT"
	   "GLSWAPBUFFERS"
	   "COLOR-NAME-TO-GL"
	   "*BREAK-ON-DISPLAY-ERROR*"
	   "GLTRANSLATEDV"
	   "GL_NO_ERROR"
	   "MAKE-GL-COLOR")
  )


;;(load-system-library "libf3dglffi")

#+never
(qffi::load-foreign-library
  (merge-pathnames
    (qffi::merge-shared-library-pathname-type
     #+macosx "arch/macosx-sbcl64/lib/libf3dglffi"
     #+linux "arch/linux-sbcl64/lib/libf3dglffi"
     )
    (asdf::system-source-file :cl-cme-tk)))


