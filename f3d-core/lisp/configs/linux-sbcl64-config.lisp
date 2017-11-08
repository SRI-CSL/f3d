(in-package :config)

;;; It would be nice if the user had a way to affect some of the configuration settings
;;; when loading FREEDIUS, without needing to edit this file.
;;; OTOH, when starting disksaves, the configuration options are frozen.

(defvar *tk-version* 'tk8.6 ;; changed - cc 
  "allowed values: tk8.5 tk8.6")

#| ;;; old Sat Jun 20 2009 --
(case *tk-version*
  (tk8.5
   (setq *tk-features* '(:THEMED)
	 *tcltk-library-files* '("/usr/lib64/libtcl8.5" "/usr/lib64/libtk8.5")))
  (tk8.5-xft 
   (setq *tk-features* '(:THEMED :FREEFONT)
	 *tcltk-library-files* '("/usr/local/lib64/libtcl8.5" "/usr/local/lib64/libtk8.5-xft")))
  (tk8.6
   (setq *tk-features* '(:THEMED :FREEFONT)
	 *tcltk-library-files* '("/usr/local/lib64/libtcl8.6" "/usr/local/lib64/libtk8.6")))
  (otherwise 
   (setq *tk-features* nil
	 *tcltk-library-files* '("/usr/lib64/libtcl8.5" "/usr/lib64/libtk8.5"))))

(defvar *libglut-library-file* "/usr/lib64/libglut")
|#

;;(defvar *enable-openglut* t)

;;(defvar glx-image-panes t)

(defvar *default-optimization-profile* :fast-no-tail-calls)
;(defvar *default-optimization-profile* :safest)
;(defvar *default-optimization-profile* :safe)

(defvar config::weak-eval-cache t)
;;(defvar config::weak-eval-cache nil)

;;; this isn't a runtime option -- it affects the compile-time generation of iref/iset macros
(defvar config::*paged-image-allowed-tile-offset-bits* '(13 16 18 20))
