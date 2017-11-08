(in-package :lisptk)

;;; ***********************  TK-INIT-MAIN-WINDOW  ***********************

;;; This is a mess -- rewrite it.  We need to figure out the right
;;; windoze magic for pointing to the tcl/tk library directories for
;;; the tcl interpreter.  This is a hack:
;;#+mswindows
;;(defvar cl-user::*tcl-base* "c:/cygwin/opt/IU/freedius/freedius/tk/library/tcl8.4")

;;#+mswindows
;;(defvar cl-user::*tk-base* "c:/cygwin/opt/IU/freedius/freedius/tk/library/tk8.4")

;;;
;;; A mechanism only partially implemented, and probably needs to move
;;; somewhere else, but allows a set of cleanup forms to be run on
;;; exit.  Probably best put into LISP-EXTENSIONS.

(defvar *quit-cleanup-forms* nil)

;;;(defun interp-result (interp)
;;;  #-allegro (foreign-string-value (tcl_interp-result interp))
;;;  #+allegro (ff:char*-to-string (tcl_interp-result interp))
;;;)

(defun interp-result (interp)
  (foreign-string-value (tcl_interp-result interp)))

(defun set-tk-argv (argv)
  (tcl-eval "set" "argv" `(list .,(cdr argv)))
  (tcl-eval "set" "argc" (1- (length argv)))
  (tcl-eval "set" "argv0" (car argv))
  )

(defun set-tk-argv (argv)
  (tcl-cmd `(set argv (list .,(cdr argv))))
  (tcl-eval "set" "argc" (1- (length argv)))
  (tcl-eval "set" "argv0" (car argv))
  )

(defun make-tcl-console (interp)
  (declare (ignorable interp))
  (Tk_InitConsoleChannels interp)
  (format t "~%Initialized console channels.")
;;  (Tk_CreateConsoleWindow interp)
;;  (tcl-eval "console" "show")
  )

;;;
;;; If the DISPLAY environment variable is incorrect (possible on
;;; MacOSX), this throws an error.  It would be nice to allow a
;;; restart with an alternate DISPLAY.

(defun tk-init-main-window (interp &optional argv)
  (let (status1 status2 status3)
    (when *tk-verbose*
      (format t "tk-init-main-window before (Tcl_Init ~x)~%" interp) (force-output))
    (setq status1 (Tcl_Init interp))
    (when *tk-verbose* 
      (format t "tk-init-main-window (Tcl_Init ~x) returns ~a.~%" interp status1) (force-output))
    (when (eql status1 TCL_ERROR)
      (error "tcl_init error: ~s~%" (interp-result interp)))
    (when argv (set-tk-argv argv))
    ;;(format t "~%set-tk-argv done.")

    ;; Doesn't get inherited on Windows...
    #+mswindows
    (tcl-eval "set" "env(FREEDIUS)" (asdf::getenv "FREEDIUS"))
    #+mswindows
    (tcl-eval "set" "env(RADIUS)" (asdf::getenv "RADIUS"))
    #+mswindows
    (tcl-eval "set" "env(FREEDIUSTK)" (asdf::getenv "FREEDIUSTK"))

    #+never
    (progn
      (tcl-eval "lappend" "auto_path" cl-user::*tcl-base*)
      (tcl-eval "lappend" "auto_path" cl-user::*tk-base*)
      (tcl-eval "source" (format nil "~a/auto.tcl" cl-user::*tcl-base*))
      (tcl-eval "source" (format nil "~a/package.tcl" cl-user::*tcl-base*))
      (format t "~%Finished WiNDoZe preloads...")
      ;;(tcl-eval "source" (format nil "~a/tk.tcl" cl-user::*tk-base*))
      )
    (setq status2 (Tk_Init interp))
    (when *tk-verbose* (format t "~%Tk_Init done."))

    (when (eql status2 TCL_ERROR)
      (error "tk_init error: ~s~%" (interp-result interp)))
        
    ;; Depend on tcl loader to hook up to libtkGLx and call TkGLx_Init
    ;; Can accomplish this next by doing (tcl-eval 'load "libtkGLx.so")
					;(TkGLx_Init interp)
    (when *tk-verbose* (format t "tk-init-main-window ~a~%" (list status1 status2)))
    (Tcl_SetVar interp "tcl_interactive" "0"  TCL_GLOBAL_ONLY)
    #+mswindows (make-tcl-console interp)
    ))

(defvar *the-interpreter* nil)
;;;
;;; Only useful on threaded SBCL - saves the thread that created the
;;; tcl interpreter.  It is a TCL constraint that the interpreter can
;;; ONLY be used by the thread that created it.

(defvar *the-thread* nil)

;;; This next is needed to cause stdin characters to unblock Tcl_DoOneEvent.
;;; This has been moved to init-tcl-tk for better understanding
;;; of the initialization ordering constraints.
;;; The dependence on *the-interpreter* in this initialization is wrong.

;;;(st::add-system-initialization
;;; :tk
;;; '(when (and *the-interpreter* (not (member :mswindows *features*)))
;;;   (tcl-eval 'fileevent 'stdin 'readable " "))
;;; ;; A do-nothing script for fileevent, which is enough to wake up
;;; ;; the read-eval-print loop.
;;; ;; Must be extremely careful about the quoting '| |.
;;; ;; Must not be seen as an empty command which means to remove the
;;; ;; event binding. 
;;; )


(defun tcl-filename-filter (string)
  ;; On windows, allegro chooses to translate forward slashes into
  ;; backslashes.  Tcl, however, is most offended by this action.  So
  ;; switch 'em back!

  ;; While we are at it, accept pathname argument as well as a string.
  (setq string (namestring (truename string)))
  
  (loop for i from 0 below (length string)
	do (when (char= (aref string i) #\\)
	     (setf (aref string i) #\/)))
  string)

;;; *LISPTK-APP-DEFAULTS-FILE* contains app-defaults for "vanilla" uses of the :TK subsystem.
;;; To customize for a particular architecture, install a customized version of
;;; $FREEDIUS/lisp/tk/<xxx>.app-defaults in $FREEDIUS_ARCH/lisp/<xxx>.app-defaults and change
;;; this variable accordingly in $FREEDIUS_ARCH/lisp/config.lisp.

(custom:defcustom *LISPTK-APP-DEFAULTS-FILE* "$FREEDIUS/lisp/tk/lisptk.app-defaults"
  (:groups (:lisptk))
  "app-defaults filename for \"vanilla\" uses of the :LISPTK subsystem.
To customize for a particular architecture, install a customized version of
$FREEDIUS/lisp/tk/lisptk.app-defaults in $FREEDIUS_ARCH/lisp/<xxx>.app-defaults
and specify a setting for this variable in $FREEDIUS_ARCH/lisp/runtime-options.lisp file.
")

#-mswindows
(defvar *default-tk-init-argv*
  '("Qcme" "-sync")
  ) ; used for resource matching in .app-defaults files

#+mswindows
(defvar *default-tk-init-argv*
  '("Qcme" "-sync" "-visual" "truecolor 32")
  ) ; used for resource matching in .app-defaults files

(defvar *default-tk-application-name* "Qcme")

;;;
;;; This needs to change from FREEDIUSTK to dependency on the FREEDIUS
;;; load path, but this normally depends on f3d-core to set up the
;;; environment variable.  See the asd definition file for this system:

(custom:defcustom *LISPTK-LIBRARY-PATH*  
  (namestring
   (lx::pathname-as-file 
    (truename (if (member :THEMED config::*tk-features*) 
		  "$FREEDIUSTK/tk/library-ttk"
		  "$FREEDIUSTK/tk/library"))))
  (:groups (:lisptk)))

(custom:defcustom *DEFAULT-WISHRC-FILE* (format nil "~a/.wishrc" *LISPTK-LIBRARY-PATH*)
  (:groups (:lisptk))
  "Filename of the Lisptk Tcl/Tk initialization file."
  )

;(makunbound '*TCL-AUTO-PATH-EXTRAS*)
(custom:defcustom *TCL-AUTO-PATH-EXTRAS* ""
  ;;(format nil "/usr/lib /usr/local/lib ~a" TK::*LISPTK-LIBRARY-PATH*)
  ;; do not include TK::*LISPTK-LIBRARY-PATH* yet:  .wishrc adds it
  (:groups (:lisptk))
  "A string containing Tcl auto_path names needed for extra Tk libraries such as Tix and Tile")

;(tcl-cmd `(list "$auto_path"))
;(tcl-cmd `(set "auto_path" "auto_path: /usr/local/lib /opt/IU/FREEDIUS/default/tk/library8.5 /usr/local/lib/tcl8.5 /usr/local/lib ./lib /usr/local/lib/tk8.5 /usr/local/lib/tk8.5/ttk"))
;(tcl-cmd `(set "auto_path" "auto_path: /usr/local/lib /opt/IU/FREEDIUS/default/tk/library8.5 /usr/local/lib/tcl8.5 /usr/local/lib ./lib /usr/local/lib/tk8.5 /usr/local/lib/tk8.5/ttk"))

(defvar *tcl-tk-after-init-hooks* nil)

(defun init-tcl-tk (argv &key
		    (init-file *default-wishrc-file*)
		    (app-defaults *lisptk-app-defaults-file*)
		    (tk-application-name *default-tk-application-name*)
		    )
  (when *tk-verbose* (format t "~%In init-tcl-tk"))
  (when tk-application-name (setf (car argv) tk-application-name))
  (TclpInitplatform)
;;  #-mswindows (TclpInitplatform)
  #-mswindows (Tcl_FindExecutable (car argv))
  (unless (and (boundp '*the-interpreter*) *the-interpreter*)
    (when *tk-verbose* (format t "init-tcl-tk calling (tcl_create_interp)~%"))
    (let ((interp (tcl_create_interp)))
      ;;(Tcl_InitStubs *the-interpreter* "8.4" 0)
      (when *tk-verbose* (format t "init-tcl-tk (tcl_create_interp) returns ~a~%" interp))
      (setq *the-interpreter* interp)
      #+sb-thread (setq *the-thread* sb-thread::*current-thread*)
      (tk-init-main-window interp argv))
    ;; Should this stuff be in a TCL script ??
    (when (and init-file (probe-file init-file))
      (tcl-eval 'source (tcl-filename-filter init-file)))
    
    (tcl-cmd `(set auto_path (concat "$auto_path" ,*TCL-AUTO-PATH-EXTRAS*)))
    (when (and app-defaults (probe-file app-defaults))
      (tk-load-app-defaults app-defaults))
    (tk-wm "withdraw" ".")
    ;; This fileevent command causes tcl to wake up the event loop when a character is typed.
    ;; With CMUCL/SLIME, interrupts are used to get the character handers.
    ;; With Allegro/SLIME, a different process is used for the SLIME REPL.
    #+never
    (when (or (not (find-package :swank)) ; check runtime existence of SLIME not just the config option.
	      #+(and (not :mswindows) :no-slime) t)
      (tcl-eval 'fileevent 'stdin 'readable " ")
    )
    #-mswindows (tcl-eval 'fileevent 'stdin 'readable " ")
    (loop for hook in *tcl-tk-after-init-hooks*
	  do (funcall hook))
    
  ))
    
    
;;; *****************  INIT-LISPTK  *****************

(defun init-lisptk (&optional forced-read-forms &key (argv *default-tk-init-argv*) (start-repl t))
  (maybe-switch-repl)
  ;;(format t "~%In init-lisptk...")
  (init-tcl-tk argv)
  ;; THIS LOOKS WRONG  - I tried to move this to start-cme, but pop-up menus became busted.
  ;; Must be some side effect of init-tcl-tk that is fixed by st::initialize-all-systems later.
  ;;(st::initialize-all-systems)
  ;;; FIXED:  Tue Jun 30 2009 -- needed to use *tcl-tk-after-init-hooks*
  (when start-repl (repl forced-read-forms))
  )

(defun tcl-tk-initialized-p ()
  (not (null *the-interpreter*)))

(defvar *loaded-repl* nil)
 
;;; This is needed when restarting a disksave which was built with a different
;;; REPL than is needed with the current execution environment.
(defun maybe-switch-repl ()
  #-quicklisp
  (let ((needed-repl-filename (config::select-repl)))
    (unless (equal *loaded-repl* needed-repl-filename)
      (maybe-compile-file-load (format nil "$FREEDIUS/lisp/tk/~a" needed-repl-filename)))))
