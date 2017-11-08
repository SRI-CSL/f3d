(in-package :gui)

;;;
;;; Problem: There are dependencies on system-tool that cause some of
;;; these initializations to reach back inappropriately to $FREEDIUS
;;; and $FREEDIUS_ARCH...

;;; Redefine st::initialize-all-systems:
  
#+never
(defun st::initialize-all-systems (&key (all-systems t) (stream *standard-output*) silentp)
  "Invoke the function initialize-system on all systems"
  #+debug (break)
  (loop for (system . forms) in (reverse st::*system-initializations*)
	unless silentp
	  do (format stream "~&;;; Initializing ~a"	;
		     system)
	do (loop for form in (reverse forms) do  (eval form)))
  (unless silentp (terpri))
  (setf st::*system-initializations* nil)
  (setq *systems-initialized* t))

;;; It turns out that the only dependency is for the repl.

#+sb-thread
(defun cme-thread (&rest args)
  ;; Tried to move st::initialize-all-systems to here from tk:init-lisptk, but broke pop-up menus.
  ;; This might be related to display_tiled_image segfault problem.
  ;;(st::initialize-all-systems)
  ;;(setq qffi::*initial-debug-level* -99) ; shut up all tracing
  ;;(setq qffi::*initial-debug-level* 2)
  (load-runtime-options)
  (cl-user::load-freedius-init-file)
  (st::initialize-all-systems)
  (sb-thread::with-mutex (tk::*tcl-event-mutex*)
    ;; Ensure only ONE tcl interpreter:
    (unless (and (boundp 'tk::*the-interpreter*)
		 tk::*the-interpreter*)
      (tk::init-tcl-tk tk::*default-tk-init-argv*)
      (lcl::initialize-lisp-bindings)
      (tk::Tcl_InitNotifier)
      (tk::init-lisptk nil :start-repl nil))
      (gui::make-cme-control-panel)
      (setq tk::*freedius-started* t)
      (force-output t)
      (setq tk::*forced-read-forms* nil))
  (format t "~%Tcl initialized")
  (loop
        do (tk::do-pending-tcl-events)
           (gui::redisplay-damaged-views)
           (tk::usleep 60000))
  )

;;;
 
#+sb-thread
(defun cl-user::start-cme2 (&optional forms)
  (when forms
    (format t "~% Can't eval supplied forms: ~a"))
  (format t "~% *** Starting CME Thread ***~%")
  (setf *cme-thread*
	(sb-thread:make-thread
	 #'gui::cme-thread
	 :name "CME Thread")))

