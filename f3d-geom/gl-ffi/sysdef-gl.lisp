(in-package :cl-user)

;;; This needs to be set in $FREEDIUS/arch/<arch>/lisp/config.lisp If
;;; it's NIL, then just use the glut-defs.lisp file:
(defvar config::*libglut-library-file* nil)

(defvar config::*enable-openglut* nil)
(defvar *glffi-libname* "libf3dglffi")

#-asdf
(st:define-system "gl" 
    ;;:system-class 'st::mixed-lisp-system
    ;;:libraries (list "$FREEDIUS_ARCH/lib/libFREEDIUS" config::*libglut-library-file*)
    :libraries
  (append
   (if config::*libglut-library-file*
       (list config::*libglut-library-file* *glffi-libname*)
       (list *glffi-libname*))
   (list
    #+mswindows '(:search "glu32")
    #+mswindows '(:search "opengl32")))


    ;; must load libfreedius before libf3dglffi
    :required-systems '(common-symbols qffi foreign-vector lisp-extensions libfreedius)
    :files `("gl-pkg.lisp"
	     "gl-ffi.lisp"
	     "glext-ffi.lisp"
	     "glu-ffi.lisp"
	     "gl-utils.lisp"
             "glut-fonts.lisp"
	     ,(if config::*libglut-library-file* "glut-ffi.lisp" "glut-defs.lisp")
	     "gl-pkg-export-all.lisp"
	     ))

;(st::find-system-named :gl)
