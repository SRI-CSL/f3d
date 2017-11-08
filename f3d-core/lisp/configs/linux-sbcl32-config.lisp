(in-package :config)

	     

;;; It would be nice if the user had a way to affect some of the configuration settings
;;; when loading FREEDIUS, without needing to edit this file.
;;; OTOH, when starting disksaves, the configuration options are frozen.

(defvar *tk-features*)
(defvar *tcltk-library-files*)
(defvar *tk-version* 'tk8.5-xft
  "allowed values: tk8.4 tk8.5 tk8.5a5-xft tk8.4-ttk")
(setq *tk-version* 'tk8.5)

;; (defvar *tk-version* 'tk8.6
;;  "allowed values: tk8.5 tk8.6")

#| ;;; old Sat Jun 20 2009 --
(case *tk-version*
  (tk8.5
   (setq *tk-features* '(:THEMED)
	 *tcltk-library-files* '("/usr/lib/libtcl8.5" "/usr/lib/libtk8.5")))
  
  (tk8.5-xft 
   (setq *tk-features* '(:THEMED :FREEFONT)
	 *tcltk-library-files* '("/usr/lib/libtcl8.5" "/usr/lib/libtk8.5-xft")))
  (tk8.4-ttk
   (setq *tk-features* '(:THEMED)
	 *tcltk-library-files* '("/usr/lib/libtcl" "/usr/lib/libtk")))
  (tk8.6
   (setq *tk-features* '(:THEMED :FREEFONT)
	 *tcltk-library-files* '("/opt/lib/libtcl8.6" "/opt/lib/libtk8.6")))
  (otherwise 
   (setq *tk-features* nil
	 *tcltk-library-files* '("/usr/lib/libtcl" "/usr/lib/libtk"))))

(defvar *libglut-library-file* "/usr/lib/libglut")
|#

;;(defvar *enable-openglut* t)

;;(defvar glx-image-panes t)

(defvar *default-optimization-profile* :fast-no-tail-calls)
;(defvar *default-optimization-profile* :safest)
;(defvar *default-optimization-profile* :safe)

;;(defvar config::weak-eval-cache t)
(defvar weak-eval-cache nil)

;;; this isn't a runtime option -- it affects the compile-time generation of iref/iset macros
(defvar *paged-image-allowed-tile-offset-bits* '(13 16 18 20))


#|
(describe C::*DEFAULT-COOKIE*)
(describe C::*default-interface-cookie*)



:fast-no-tail-calls => (ext:optimize-interface (speed 3) (safety 2) (debug 9/4)))
                       (optimize (speed 3) (safety 1) (debug 9/4)))
#S(C::COOKIE :SPEED 3 :SPACE 1 :SAFETY 1 :CSPEED 0 :BREVITY 3 ...) is a structure of type COOKIE.
SPEED: 3.
SPACE: 1.
SAFETY: 1.
CSPEED: 0.
BREVITY: 3.
DEBUG: 9/4.
FLOAT-ACCURACY: 3.
#S(C::COOKIE :SPEED 3 :SPACE NIL :SAFETY 2 :CSPEED NIL :BREVITY NIL ...) is a structure of type COOKIE.
SPEED: 3.
SPACE: NIL.
SAFETY: 2.
CSPEED: NIL.
BREVITY: NIL.
DEBUG: 9/4.
FLOAT-ACCURACY: 3.

:safe =>               (ext:optimize-interface (speed 3) (safety 2) (debug 9/4)))
                       (optimize (speed 3) (safety 1) (debug 9/4))
 #S(C::COOKIE :SPEED 3 :SPACE 1 :SAFETY 1 :CSPEED 0 :BREVITY 5/2 ...) is a structure of type COOKIE.
SPEED: 3.
SPACE: 1.
SAFETY: 1.
CSPEED: 0.
BREVITY: 5/2.
DEBUG: 9/4.
FLOAT-ACCURACY: 3.

 #S(C::COOKIE :SPEED 3 :SPACE NIL :SAFETY 2 :CSPEED NIL :BREVITY NIL ...) is a structure of type COOKIE.
SPEED: 3.
SPACE: NIL.
SAFETY: 2.
CSPEED: NIL.
BREVITY: NIL.
DEBUG: 9/4.
FLOAT-ACCURACY: 3.
                     
|#
