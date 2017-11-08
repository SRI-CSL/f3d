(in-package :config)

;;; File "tk-pkg.lisp" adds exports to :lisptk

;;; These default configuration settings can be modified by $FREEDIUS/arch/<arch>/lisp/config.lisp

(defvar *tk-features* nil
  "A list of keywords describing the features of the Tk library. 
   Permissible values: :THEMED :TRUEFONT.")

(defvar *tcltk-library-files* nil)


;;; Windows users will need to explicitly load the Tcl and Tk DLLs (as
;;; well as several others, like GL) before loading Freedius.  On
;;; other systems, these libraries are loaded automatically.

;;; A change and a question: Cygwin under Windoze provides Tcl/Tk in
;;; the form of .a files that can be statically linked into liblisptk.
;;; One also has the option of using an installed binary version of
;;; Tcl/Tk as found in the "Program Files" directory.  For now, I am
;;; reverting to the assumed cygwin default, but how should this be
;;; handled?  Should we add a :cygwin feature if cygwin is present?

(defvar *enable-slime* t)

(unless (find-package :swank)
  (setq *enable-slime* nil))

;;; This is called by tcl-tk-init to possibly load a different REPL
;;; file.  Something is wonky with the fd-handler slime communication
;;; style when using threaded SBCL.  Locks up rather often:
(defun select-repl ()
  (if (and (find-package :swank) ) ;; (not (find-package :clim)))
      #+cmu "cmu-slime-repl.lisp"
      #+sbcl "sbcl-slime-repl.lisp"
;;      #+(and sbcl (not sb-thread)) "sbcl-slime-repl.lisp"
;;      #+(and sbcl sb-thread) "sbcl-alt-slime-repl.lisp"
      #+allegro "acl-slime-repl.lisp"
      "repl.lisp"))

;;; Problem here -- can't make a mixed-lisp-system until qffi is loaded.
;;; Perhaps the system-tool should autoload mixed-lisp-system.lisp.
(st:define-system :tk 
    ;;:system-class 'st::mixed-lisp-system
    :required-systems '(qffi lisp-extensions tkmin)
    ;;:c-source-path "$FREEDIUS/c/lisptk/"
    :libraries`(,@*tcltk-library-files*
		"liblisptk.dll"
		;;		 #-mswindows "liblisptk"
		;;		 #+mswindows "lisptk"
		)
    :files `("tk-ffi.lisp"
	     "tcl-tk-init.lisp"
	     "tcl-eval.lisp"
	     "tk-commands.lisp"
	     "tk-bindings.lisp"
	     "tk-widget.lisp"
	     "tk-callbacks.lisp"
	     ,(select-repl)
	     "widget-panel.lisp"
	     "menus.lisp"
	     ))

; (st::find-system-named :tk)
