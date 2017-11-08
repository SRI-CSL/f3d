(in-package :config)

	     

;;; It would be nice if the user had a way to affect some of the configuration settings
;;; when loading FREEDIUS, without needing to edit this file.
;;; OTOH, when starting disksaves, the configuration options are frozen.

(defvar *tk-version* 'tk8.6
  "allowed values: tk8.4 tk8.5 tk8.5a5-xft tk8.4-ttk")

#|
(case *tk-version*
  (tk8.5
   (setq *tk-features* '(:THEMED)
	 *tcltk-library-files* '("/usr/local/lib/libtcl8.5" "/usr/local/lib/libtk8.5")))
  
  (tk8.5-xft 
   (setq *tk-features* '(:THEMED :FREEFONT)
	 *tcltk-library-files* '("/usr/local/lib/libtcl8.5" "/usr/local/lib/libtk8.5-xft")))
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

(defvar *enable-openglut* t)

(defvar glx-image-panes t)

(defvar *default-optimization-profile* :fast-no-tail-calls)
;(defvar *default-optimization-profile* :safest)
;(defvar *default-optimization-profile* :safe)

;;(defvar config::weak-eval-cache t)
(defvar config::weak-eval-cache nil)

(defvar config::*paged-image-allowed-tile-offset-bits* '(13 16 18 20))
