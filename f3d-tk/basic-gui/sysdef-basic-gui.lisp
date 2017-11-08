(in-package :cl-user)

#|
LHQ Sat Jan  1 2005:   

The BASIC-GUI subsystem is getting too big.  Needs to be partitioned.

|#

;;; basic-gui system is specific to the Tcl/Tk GUI.
;;; The *features* wgl-image-panes glx-image-panes and agl-image-panes are set in config.lisp

;;; AGL is deprecated.  Switch to Cocoa...

;;; The :tkgui *feature* is the only difference between basic-gui and basic-gui-lite

(defvar config::tkgui t)
(defvar config::cpp-image-display nil) ; old display-image supported by C++ code
(defvar config::glx-image-panes nil)
(defvar config::wgl-image-panes nil)
(defvar config::agl-image-panes nil)
(defvar config::cocoa-image-panes ni)

(st:define-system "basic-gui"
    :required-systems '(common-symbols image gl tkgl transforms gl-objects)
    :files `("gui-pkg.lisp"
	     "macros.lisp"
	     
	     ;; GL image-pane machinery
	     "gl-qcme-ffi.lisp"
	     "window.lisp"
	     "screen.lisp"
	     ,@(when config::glx-image-panes '("tk-glx.lisp"))
	     ,@(when config::wgl-image-panes '("tk-wgl.lisp"))
	     ,@(when config::agl-image-panes '("tk-agl.lisp"))
	     ,@(when config::cocoa-image-panes '("tk-cocoa.lisp"))
	     
	     ;; GUI infrastructure 
	     "object-sets.lisp" ;; must be loaded before worlds.lisp and display.lisp
	     "worlds.lisp" ; this can go almost anywhere in this file (after object-sets.lisp).
	     ;;,@(when config::weak-eval-cache '("weak-children-hash-table-mixin.lisp"))
	     "view.lisp"   ; needs object-sets.lisp
	     "interactor.lisp"
	     "window-panel.lisp" ; glwin-mouse-buttonpress-callback needs interactor class def
	     ,@(when config::tkgui '("tk-window-panel.lisp")) ; needs window-panel.lisp

	     ;; View display machinery
	     "gl-matrices.lisp"
	     "display.lisp"
	     ,@(if config::cpp-image-display 
		   '("display-image-ffi.lisp")
		   '("display-image.lisp"))
	     "lighting.lisp"
	     "pick.lisp"; need interactor class def before pick.lisp

	     ;; GUI interaction methods
	     "commands.lisp" 
	     "object-motions.lisp"	     
	     "../globj/object-ops.lisp"
	     "../globj/composite-objects.lisp"
	     "../transforms/transform-tools.lisp" ; this file should move to the basic-gui directory
	     "../img/image-gui-defs.lisp"
	     
	     ;; GUI bindings and panels
	     "popup-menus.lisp"
	     "ui-classes.lisp"
	     "special-objects.lisp"
	     "view-panels.lisp"
	     "photometric-transform-panel.lisp"
	     "temporal.lisp"
	     ))
