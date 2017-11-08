(in-package :gui) ; bad choice of package
;(in-package :cme)


(custom:defgroup :freedius (:subgroups (:image :essential-settings))
  "FREEDIUS Main Customization Group")

(custom:defgroup :essential-settings (:supergroups (:freedius))
  "Settings essential for running FREEDIUS")

;;; Why is load-freedius-init-file in package cl-user?
(defun cl-user::load-freedius-init-file ()
  (let* ((homedir (environment-variable "HOME"))
	 (init-file-path (format nil "~a/freedius-init.lisp" homedir)))
    (when (probe-file init-file-path)
      ;; why is this compiled?  Don't compile it.  Too much bs when
      ;; switching lisps and versions.
      (load init-file-path)
;;      (maybe-compile-file-load init-file-path)
      )))

;; #+never ; old version with arch runtime-options in $FREEDIUS/arch/<arch>/lisp
#+old
(defun load-runtime-options ()
  (let ((custom::*loading-runtime-options-p* t))
    (load "$FREEDIUS/lisp/runtime-options.lisp")
    (when (probe-file "$FREEDIUS_ARCH/lisp/runtime-options.lisp")
      (load "$FREEDIUS_ARCH/lisp/runtime-options.lisp"))))

;;; new Fri Jun 19 2009 placing all runtime-options files in $FREEDIUS/lisp/configs/
(defun load-runtime-options ()
  (flet ((load-noisely (path)
	   (format t ";;; loading ~a~%" path)
	   (load path)))
    (let ((custom::*loading-runtime-options-p* t)
	  (arch-runtime-options-path (format nil "$FREEDIUS/lisp/configs/~a-runtime-options.lisp"
					     st::*freedius-arch-name*)))
      (load-noisely "$FREEDIUS/lisp/configs/default-runtime-options.lisp")
      (when (probe-file arch-runtime-options-path) 
	(load-noisely arch-runtime-options-path)))))

(defvar tk::*freedius-started* nil)

(defun cl-user::start-cme (&optional forms)
  ;; Tried to move st::initialize-all-systems to here from tk:init-lisptk, but broke pop-up menus.
  ;; This might be related to display_tiled_image segfault problem.
  ;;(st::initialize-all-systems)
  ;;(setq qffi::*initial-debug-level* -99) ; shut up all tracing
  ;;(setq qffi::*initial-debug-level* 2)
  (load-runtime-options)
  (cl-user::load-freedius-init-file)
  (st::initialize-all-systems)
  (tk:init-lisptk `((progn 
			   (lcl::initialize-lisp-bindings)
			   ;; ,@forms
			   (gui::MAKE-CME-CONTROL-PANEL)
			   (setq tk::*freedius-started* t)
			   ;; So far, I think the only usage for forms has been to load other systems at startup:
			   ,@forms
			   (in-package :gui))
		    )))

#|

mp::*all-processes*

(tk:init-lisptk nil :start-repl nil)

(setq *repl-process* 
      (mp::process-run-restartable-function 
       "freedius-repl" 
       #'tk::repl 
       `((progn (lcl::initialize-lisp-bindings)
		(gui::MAKE-CME-CONTROL-PANEL)
		(setq tk::*freedius-started* t)
		(in-package :gui)))))

(tk::repl `((progn (lcl::initialize-lisp-bindings)
		   (gui::MAKE-CME-CONTROL-PANEL)
		   (setq tk::*freedius-started* t)
		   (in-package :gui))))
|#

(defun cl-user::custom-start-cme (forms)
  ;; tk:init-lisptk runs the system initializations.
  (load-runtime-options)
  (tk:init-lisptk
   (append
    '((in-package :gui)
      ;;(tk-wm "withdraw" ".")
      )
    forms))
  )

(defun cl-user::start-cme-with-systems (systems forms)
  ;; tk:init-lisptk runs the system initializations.
  (load-runtime-options)
  (tk:init-lisptk
   `((progn ;;(tk-wm "withdraw" ".")
       (menu-choose '(("Load Systems" :eval nil)))
       (tk::do-events)
       ,@(loop for s in systems collect `(st::load-system ,s))
       ,@forms
       (in-package :gui)))))

;;; For restarting Allegro disksaves see $FREEDIUS/lisp/allegro/disksave-forms.lisp

#+cmu  ; this works with SLIME 
(defun cl-user::restart-disksave ()
  ;;(cl-user::cme-initialize-environment) ; FIXME -- this no longer exists
  (lcl::initialize-lisp-bindings)
  (load "$FREEDIUS/lisp/patches/load-patches.lisp")
  (system::reinitialize-global-table)
  (qffi::register-all-callbacks)
  ;; I haven't figured out how to make START-CME work with SLIME.
  ;; (cl-user::start-cme '((lcl::initialize-lisp-bindings )))
  (lisp::%top-level)
  )

#+cmu
(defun cl-user::restart-disksave-app ()
  ;; Suited for stand-alone apps, e.g., MacOSX
  (lcl::initialize-lisp-bindings)
  (load "$FREEDIUS/lisp/patches/load-patches.lisp")
  (system::reinitialize-global-table)
  (qffi::register-all-callbacks)
  (cl-user::start-cme '((lcl::initialize-lisp-bindings )))
  (lisp::%top-level)
  )

#|

;;; *********************  BUILDING DISKSAVES *********************

For Allegro disksaves, see $FREEDIUS/lisp/allegro/build-disksave.lisp

In a Unix shell do:

$F3DA/bin/freedius

(progn (st::load-cme :start nil)
       ;;(st::maybe-compile-file-load "$FREEDIUS/lisp/patches/reinitialize-global-table-patch.lisp")
       (ext:save-lisp (st::merge-freedius-arch-path "bin/freedius-save.core")
		      :purify t
		      :init-function #'cl-user::restart-disksave
		      )
       #+cmu (ext:quit) #+allegro (excl:exit))


cmucl-experimental -core $F3DA/bin/freedius-save.core
(cl-user::start-cme)



Thu Mar  3 2005

In a Unix shell:

Make sure FREEDIUS and FREEDIUS_ARCH are set correctly

cmucl -load $FREEDIUS/lisp/boot.lisp
(progn (st::load-cme :start nil)
       ;; this is no longer needed in newer versions of CMUCL.
       (st::maybe-compile-file-load "$FREEDIUS/lisp/patches/reinitialize-global-table-patch.lisp")
       (ext:save-lisp (st::merge-freedius-arch-path "bin/freedius-save.core")
		      :purify t
		      :init-function #'cl-user::restart-disksave
		      ))




Thu Dec 11 2003 on 1.2 gHz AMD Athlon.  2385.51 bogomips

Timings  (time (cl-user::load-cme :start nil))

Lisp version             full recompile+load   load only
______________________________________________________

Allegro                  26 secs               3.4 secs

CMUCL                   126 secs               6.8 secs


Fri Jun 11 2004 on 2.0 gHz AMD Athlon. 4071.42 bogomips

Timings  (time (cl-user::load-cme :start nil))

Lisp version             full recompile+load   load only
______________________________________________________

Allegro                  28 secs               2.7 secs

CMUCL                    66 secs               5.8 secs
|#


;;;
;;; Really should not be here, will be relocated as soon as possible:
#+sbcl
(st::add-system-initialization :cme '(push '(img::file-image-cleanup) tk::*quit-cleanup-forms*))


