(in-package :config)

;;; *******************  DEFAULT FREEDIUS CONFIGURATION FILE  ***********************
;;;
;;; These settings are needed for compilation and loading of FREEDIUS.
;;; Architecture specific varients of this file should be installed in $FREEDIUS_ARCH/lisp/config.lisp

;;; This is needed for sharpsign macros.  Do not remove unless #+qffi and #-qffi uses are removed.
;;(pushnew :qffi *features*) ;; no longer used
(pushnew :cmake *features*)

;;; The LOGICAL-PATHNAME version of FREEDIUS-PATHNAME-TRANSLATIONS facilitates 
;;; disksaves being run in a different pathname environment.  Restart the diskave and
;;; set (logical-pathname-translations "FREEDIUS") to values appropriate for the new environment.
;;; new Sat Jun 27 2009 -- looks good 

(let ((arch st::*freedius-arch-name*)
      (f3d (pathname (format nil "~a/" (st::getenv "FREEDIUS")))))
  (flet ((f3d-path (&rest infs)
	   (make-pathname :host (pathname-host f3d)
			  :device (pathname-device f3d)
			  :directory (append (pathname-directory f3d) infs))))

    (setf (logical-pathname-translations "FREEDIUS")
	  `(("lisp;**;*.*.*" ,(f3d-path "lisp" :wild-inferiors))
	    ("lib;**;*.*"    ,(f3d-path "arch" arch "lib" :wild-inferiors))
	    ;;("lib;**;*.*"  ,(f3d-path "arch" arch "lib"))
	    ("fasl;**;*.*"   ,(f3d-path "arch" arch "fasl" :wild-inferiors))
	    ("build;**;*.*"  ,(f3d-path "arch" arch "build" :wild-inferiors))
	    ("**;*.*.*"      ,(f3d-path :wild-inferiors))
	    ("**;*.*"        ,(f3d-path :wild-inferiors)))))

  (setf freedius-pathname-translations
	`(("$FREEDIUS/lisp/"
	   ,(pathname "FREEDIUS:lisp;")
	   ,(pathname "FREEDIUS:fasl;")
	   (:flat ,(pathname "FREEDIUS:lib;"))
	   ,(pathname "FREEDIUS:build;"))

	  ("$FREEDIUS/c/" ; needed for auto building liblisptk and libfreedius
	   nil
	   nil
	   (:flat ,(pathname "FREEDIUS:lib;"))
	   ,(pathname "FREEDIUS:build;")))))

(when (st::getenv "F3DSYS")
  (let ((arch st::*freedius-arch-name*)
	(f3dsys (pathname (format nil "~a/" (st::getenv "F3DSYS")))))
    (flet ((f3dsys-path (&rest infs)
	     (make-pathname :host (pathname-host f3dsys)
			    :device (pathname-device f3dsys)
			    :directory (append (pathname-directory f3dsys) infs))))
      (setf (logical-pathname-translations "F3DSYS")
	    `(("lisp;**;*.*.*" ,(f3dsys-path :wild-inferiors))
	      ("lib;**;*.*"    ,(f3dsys-path "arch" arch "lib" :wild-inferiors))
	      ;;("lib;**;*.*"  ,(f3dsys-path "arch" arch "lib"))
	      ("fasl;**;*.*"   ,(f3dsys-path "arch" arch "fasl" :wild-inferiors))
	      ("build;**;*.*"  ,(f3dsys-path "arch" arch "build" :wild-inferiors))
	      ("**;*.*.*"      ,(f3dsys-path :wild-inferiors))
	      ("**;*.*"        ,(f3dsys-path :wild-inferiors)))))
	
    (setf freedius-pathname-translations
	  (append freedius-pathname-translations
		  `(("$F3DSYS/"
		     ,(pathname "F3DSYS:lisp;")
		     ,(pathname "F3DSYS:fasl;")
		     (:flat ,(pathname "F3DSYS:lib;"))
		     ,(pathname "F3DSYS:build;")))))))

#+never
(flet ((make-to-path (dir arch infs &optional (wild-inferiors t))
	 (make-pathname
	  :host (pathname-host dir)
	  :device (pathname-device dir)
	  :directory (append (pathname-directory dir)
			     (and arch `("arch" ,arch))
			     infs
			     (and wild-inferiors '(:wild-inferiors)))))
       )
  (let ((arch st::*freedius-arch-name*)
	(f3d (pathname (format nil "~a/" (st::getenv "FREEDIUS"))))
	(f3dsys (pathname (format nil "~a/" (st::getenv "F3DSYS")))))
  
    (setf (logical-pathname-translations "FREEDIUS")
	  `(("lisp;**;*.*.*" ,(make-to-path f3d nil '("lisp")))
	    ("lib;**;*.*"    ,(make-to-path f3d arch '("lib")))
	    ;;("lib;**;*.*"    ,(make-to-path f3d arch '("lib") nil))
	    ("fasl;**;*.*" ,(make-to-path f3d arch '("fasl")))
	    ("build;**;*.*" ,(make-to-path f3d arch '("build")))
	    ("**;*.*.*" ,(make-to-path f3d nil nil))
	    ("**;*.*",(make-to-path f3d nil nil))))

    (setf (logical-pathname-translations "F3DSYS")
	  `(("lisp;**;*.*.*" ,(make-to-path f3dsys nil nil))
	    ("lib;**;*.*"    ,(make-to-path f3dsys arch '("lib")))
	    ;;("lib;**;*.*" ,(make-to-path f3dsys arch '("lib") nil))
	    ("fasl;**;*.*" ,(make-to-path f3dsys arch '("fasl")))
	    ("build;**;*.*" ,(make-to-path f3dsys arch '("build")))
	    ("**;*.*.*" ,(make-to-path f3dsys nil nil))
	    ("**;*.*",(make-to-path f3dsys nil nil))))

    (setf freedius-pathname-translations
	  `(("$FREEDIUS/lisp/"
	     ,(pathname "FREEDIUS:lisp;")
	     ,(pathname "FREEDIUS:fasl;")
	     (:flat ,(pathname "FREEDIUS:lib;"))
	     ,(pathname "FREEDIUS:build;"))

	    ("$FREEDIUS/c/" ; needed for auto building liblisptk and libfreedius
	     nil
	     nil
	     (:flat ,(pathname "FREEDIUS:lib;"))
	     ,(pathname "FREEDIUS:build;"))	    
	     
	    ("$F3DSYS/"
	     ,(pathname "F3DSYS:lisp;")
	     ,(pathname "F3DSYS:fasl;")
	     (:flat ,(pathname "F3DSYS:lib;"))
	     ,(pathname "F3DSYS:build;"))))
    ))


;;; Use this if you want the FREEDIUS-SYSTEMS compiled lisp fasls and libraries inside
;;; freedius-systems/arch/<arch>/.  The libraries are stored :FLAT, ie. without any hierarchy. 
;;; New Fri Jun 26 2009 - attempt to defer the expansion of $FREEDIUS and $F3DSYS until last moment
#+never
(defvar freedius-pathname-translations
  (let ((ARCH st::*freedius-arch-name*))
    `(("$FREEDIUS/lisp/"
       "$FREEDIUS/lisp/"
       ,(format nil "$FREEDIUS/arch/~a/lisp/" ARCH)
       (:flat ,(format nil "$FREEDIUS/arch/~a/lib/" ARCH))
       ,(format nil "$FREEDIUS/arch/~a/c/" ARCH) ; change to tmp ?
       )
      ("$FREEDIUS/c/"
       nil
       nil
       (:flat ,(format nil "$FREEDIUS/arch/~a/lib/" ARCH))
       ,(format nil "$FREEDIUS/arch/~a/build/" ARCH)
       )
      .,(when (st::getenv "F3DSYS")
	      `(("$F3DSYS/"
		 "$F3DSYS/"
		 ,(format nil "$F3DSYS/arch/~a/fasl/" ARCH)
		 (:FLAT ,(format nil "$F3DSYS/arch/~a/lib/" ARCH))
		 ,(format nil "$F3DSYS/arch/~a/build/" ARCH)
		 ))))))



#+win32
(pushnew :mswindows *features*)

#+win32
(pushnew :wgl *features*)

#+(and cmu darwin)
(eval-when (eval load compile)
 (pushnew :macosx *features*)
;;; In order to allow darwin-x11, agl must be set in $FREEDIUS_ARCH/lisp/config.lisp
;;;(pushnew :agl *features*) 
) ; end eval-when

(defvar *default-optimization-profile* :fast-no-tail-calls)
;;; CMUCL compiler problems compiling def-foreign-callable with *default-optimization-profile* = :safest
;;;(defvar *default-optimization-profile* :safest)

#| old Sat Jun 20 2009
(defvar *tcltk-library-files* '("/usr/lib/libtcl" "/usr/lib/libtk"))
(defvar *libglut-library-file* "/usr/lib/libglut")
|#

;;; new Sat Jun 20 2009 -- uses find-shared-library
(defvar *library-search-paths* 
  `(,(truename (st::merge-freedius-arch-path "lib/"))
    .,(or #+(and :unix :X86-64) '("/usr/lib64/" "/usr/local/lib64/" "/opt/lib64/" )
	  #+(and :unix (not :X86-64)) '("/usr/lib/" "/usr/local/lib/" "/opt/lib/")
	  #+mswindows `("C:\\windows\\system32\\"
			"c:\\tcl\\bin\\"
			,(st::merge-freedius-path "3rd-party/windows-nmake-sbcl/lib/"))
	  (error "*default-library-search-paths* not defined for this architecture")))
  "Search paths used by qffi::find-shared-library")

(defvar *tk-features* nil
  "A list of keywords describing the features of the 
   Permissible values: :THEMED :FREEFONT.")

(defvar *tk-version* #-mswindows 'tk8.6 #+mswindows 'tk8.5
  "allowed values: nil tk8.5 tk8.6")

;;; new Sat Jun 20 2009 -- uses find-shared-library
#-mswindows
(defvar *tcltk-library-files*
  (case *tk-version*
    (tk8.5 (setq *tk-features* '(:THEMED))
	   '((:search "libtcl8.5") (:search "libtk8.5")))
    (tk8.6 (setq *tk-features* '(:THEMED :FREEFONT))
	   '((:search "libtcl8.6") (:search "libtk8.6")))
    (otherwise (setq *tk-features* nil)
	       '((:search "libtcl") (:search "libtk")))))

#+mswindows
(defvar *tcltk-library-files*
  (case *tk-version*
    (tk8.5 (setq *tk-features* '(:THEMED))
	   '((:search "tcl85") (:search "tk85")))
    (tk8.6 (setq *tk-features* '(:THEMED :FREEFONT))
	   '((:search "tcl8.6") (:search "tk8.6")))
    (otherwise (setq *tk-features* nil)
	       '((:search "tcl") (:search "tk")))))


#+never
(defvar *libglut-library-file*
  '(:search #-mswindows "libglut" #+mswindows "glut32")
  )

;;; Set *enable-openglut* to T if you have openglut in /usr/lib
(defvar *enable-openglut* nil)

(defvar glx-image-panes (or #-(or mswindows agl cocoa) t))

(defvar wgl-image-panes (or #+mswindows t))

(defvar agl-image-panes (or #+agl t))

(defvar cocoa-image-panes (or #+cocoa t))

(defvar weak-eval-cache nil) ; currently broken for CMUCL

;;; this isn't a runtime option -- it affects the compile-time generation of iref/iset macros
(defvar *paged-image-allowed-tile-offset-bits* '(16))

(defvar COMPILE-FOR-RUNTIME-ONLY-DISKSAVE nil) ; might not be needed anymore

(defvar config::freedius-systems-path (st::getenv "F3DSYS"))

#| The following have moved to $FREEDIUS/lisp/runtime-options.lisp, and the symbols
   have moved out of the CONFIG package.

DEFAULT_TMP_FILE_IMAGE_DIR
lisptk-app-defaults-file
freedius-app-defaults-file
gui::*enable-tix-dir-browser*

|#

;;; Shifting this over to config so that it can be turned off at will.
;;; Needs testing, but with the new LP library search, we might not
;;; need this anymore:

(defvar *force-shared-library-pathname-type* t)


;;;
;;; libFREEDIUS needs OpenGL and GLU.  Load them explicitly on Windows
;;; - this is unsatisfying.  Can we add something to cl-cmake to get
;;; this to work at load time?
;;;
#+mswindows
(defun load-foreign-libraries-int (list)
  (loop for lib-path in list
	do
	#+sbcl (sb-alien::load-shared-object lib-path)
	#+cmu (cl-user::load-foreign (list lib-path))
	#+allegro (load lib-path)))

#+mswindows
(load-foreign-libraries-int
 '(#p"C:\\windows\\system32\\opengl32.dll"
     #p"C:\\windows\\system32\\glu32.dll"
     #p"C:\\Tcl\\bin\\tcl86.dll"
     #p"C:\\Tcl\\bin\\tk86.dll"))
