(in-package :config)
;;;
;;; config.lisp file for windows-nmake-sbcl: Assumes "native" freedius
;;; libs compiled with nmake (cl & link) and native windows (Tcl/Tk,
;;; non-X11).  The glut32.dll library is provided by svn.
;;;
;;; Also note that you will need to install Tcl/Tk 8.5.
;;;

;;; SBCL defines WIN32 as a feature, but not MSWINDOWS:
#+win32
(pushnew :mswindows *features*)

;;;
;;; Be sure that the default pane class corresponds to the Windows OpenGL version:
;;;
;;(defvar *default-pane-class* 'tk-wgl-bbuffer-window)

(defvar wgl-image-panes (or #+mswindows t))


;;;
;;; libFREEDIUS needs OpenGL and GLU.  Load them explicitly on Windows:
;;;
(defvar config::*system-library-files* '(#p"C:\\windows\\system32\\opengl32.dll"
					 #p"C:\\windows\\system32\\glu32.dll"))

(defvar *default-optimization-profile* :fast-no-tail-calls)

;;; CMUCL compiler problems compiling def-foreign-callable with *default-optimization-profile* = :safest
;;;(defvar *default-optimization-profile* :safest)

(defvar *tk-features* nil
  "A list of keywords describing the features of the 
   Permissible values: :THEMED :FREEFONT.")

;;; Set *enable-openglut* to T if you have openglut in /usr/lib
(defvar *enable-openglut* nil)

(defvar glx-image-panes (or #-(or mswindows agl) t))

(defvar wgl-image-panes (or #+mswindows t))

(defvar agl-image-panes (or #+agl t))

(defvar weak-eval-cache nil) ; currently broken for CMUCL

(defvar COMPILE-FOR-RUNTIME-ONLY-DISKSAVE nil) ; might not be needed anymore

;;;
;;; Here is where the tcl and tk DLLs live - we assume 8.5 on Windows:
;;;
(defvar *tcltk-library-files* '("c:/Tcl/bin/tcl85.dll" "c:/Tcl/bin/tk85.dll"))

(defvar *libglut-library-file* (st::merge-freedius-arch-path "lib/glut32.dll"))

(defvar default_tmp_file_image_dir "c:/tmp")

;; (defvar CPP-IMAGE-DISPLAY T)
