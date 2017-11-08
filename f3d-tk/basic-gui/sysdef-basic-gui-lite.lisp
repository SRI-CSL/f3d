(in-package :cl-user)

;;; LHQ Fri May 30 2003 Does this sysdef file get used any more?

;;; This system is GUI independent (more or less).

(st:define-system "basic-gui-lite"
    :required-systems '(common-symbols gl tkmin transforms image gl-objects)
    :files '("gui-pkg.lisp"
	     "macros.lisp"
	     "gl-qcme-ffi.lisp"
	     "object-sets.lisp"
	     "window.lisp"
	     "window-panel.lisp"
	     "screen.lisp"
	     "view.lisp"
	     "gl-matrices.lisp"
	     "interactor.lisp"
	     "commands.lisp"		; need interactor class def before pick.lisp
	     "display.lisp"
	     "lighting.lisp"
	     "pick.lisp"
	     "popup-menus.lisp"
	     )) 
