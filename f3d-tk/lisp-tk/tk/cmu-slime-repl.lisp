(in-package :lisptk)

(defparameter *loaded-repl* "cmu-slime-repl.lisp")

#|
Thu Jul  2 2009  - With more recent versions of SLIME, START-CME must be run in *inferior-lisp* buffer.


With CMUCL/SLIME, the EMACS REPL is handled via interrupts. 

    (SWANK::PREFERRED-COMMUNICATION-STYLE) = :SIGIO

Thus, the LISPTK event loop for TK needn't bother with input from the "terminal".

CURRENT PROCESSING MODEL:

   A single process does everything.  
  
   .  The tk::repl loop handles TK events.

   .  SLIME/SWANK interrupts the TK::REPL loop to handle "keyboard" input.
      Since no locks are used,  this can corrupt data structures being modified 
      by the TK::REPL loop events (roughly equivalent to the LISPM mouse-process).
   
TODO:  BETTER PROCESSING MODEL:

   A single process (still) does everything, but events whose handling can
   corrupt data structures are enqueued for the TK::REPL to handle SYNCHRONOUSLY.
   Long duration computations should also be enqued.

   There needs to be an additional event layer, so that (some) safe GUI events can be run 
   at a higher "priority" than the enqueued events. 

   . SLIME/SWANK interrupts
      (Issue:  These can be non-safe)

   . Safe TK events

   . Non-safe events and enqueued tasks.

|#

;;; Nothing special needed in :SIGIO single-process environment
(defun output-to-slime-repl-fn (fun)
  (funcall fun))

;;; This is identical across all REPL versions
(defmacro with-output-to-slime-repl (&body body)
  `(output-to-slime-repl-fn  #'(lambda () .,body)))

(defvar *freedius-started* nil)

(declaim (special *** ** * $$$ $$ $ /// // /))

(defvar *no-value-return* (list nil))

(defvar *modular-slime* (fboundp 'swank::cat))

(defun print-and-set-top-level-variables (values)
  (unless (eq (car values) *no-value-return*)
    ;; add-repl-result
    (when values
      (setq *** ** ** * * (car values))
      ;;(setq $$$ $$ $$ $ $ values)
      (setq /// // // // / values))
    ;; top-level-print
    (if *modular-slime*
	(funcall swank::*send-repl-results-function* values)
	
	;; for older, non-modular version of SLIME
	(flet ((send  (value)
		 (let ((id (and (boundp 'swank::*record-repl-results*)
				swank::*record-repl-results*
				(swank::save-presented-object value))))
		   (swank::send-to-emacs `(:write-string ,(prin1-to-string value)
							 ,id nil))
		   (swank::send-to-emacs `(:write-string ,(string #\Newline) 
							 nil nil)))))
	  (if (null values)
	      (swank::send-to-emacs `(:write-string "; No value" nil :repl-result))
	      (mapc #'send values))))))

;;; ******************************************************************************

(defvar *inside-repl* nil)
(defvar *quit-repl* nil)
(defparameter *forced-read-forms* nil)

(defun add-forced-read-forms (&rest forms)
  (setf *forced-read-forms* (nconc *forced-read-forms* forms))) 

;;; The problem under SLIME is that HANDLE-EVENTS-UNTIL is blocked until a TCL/TK
;;; event occurs.  Thus, (throw 'quit-repl ) is required rather than just setting *quit-repl*.
(defun quit-repl (&optional quit-code)
  (when *inside-repl*
    (setq *quit-repl* (or quit-code t))
    (throw 'quit-repl quit-code)))

;;; This is identical across all REPL versions
(defun repl (&optional forced-read-forms)
  (setq *forced-read-forms* forced-read-forms)
  
  (unless *inside-repl*
    (setq *quit-repl* nil)
    (let* ((*inside-repl* t)
	   (exit-code
	    (catch 'quit-repl
	      (tagbody
	       spot 
		 (restart-case
		     (progn (do-events)	;handle initial new-panes...
			    (unwind-protect 
				 (handle-events-until #'(lambda() nil) t)
			      ;; CLEAN-UP form -- never leave repl unless *quit-repl* is non-NIL.
			      ;; That confines us to this function even after aborts.
			      (unless *quit-repl* (go spot))
			      ))
		   (abort () (format t "Aborted~%")))
		 (unless *quit-repl* (go spot))
		 ))))      
      (case *quit-repl*
	(:quit-cme (format t "Quiting FREEDIUS") (lcl::quit))
	(otherwise (format t "Quiting tk::repl~%")))
      )))

(defun handle-events-until (completion-function &optional (eval-read-forms t))
  (ignore eval-read-forms)
  (loop	do #+cmu (lisp::scrub-control-stack)
	   (if *forced-read-forms*
	       (let ((values (multiple-value-list (eval (pop *forced-read-forms*)))))
		 (when *freedius-started*
		   (print-and-set-top-level-variables values)))
	       (wait-for-any-event))
	   (do-events)
	until (or *quit-repl* (funcall completion-function))))


#+cmucl-mp
(defun handle-events-until (completion-function &optional (eval-read-forms t))
  (ignore eval-read-forms)
  (loop	do #+cmu (lisp::scrub-control-stack)
	   (if *forced-read-forms*
	       (let ((values (multiple-value-list (eval (pop *forced-read-forms*)))))
		 (when *freedius-started*
		   (print-and-set-top-level-variables values)))
	       (wait-for-any-event))
	   (mp::process-yield)
	   (do-events)
	until (or *quit-repl* (funcall completion-function))))


;;;   **********************  TCL/TK Event Handling  **************************

;;; bind *repl-event-wait-hook* to a function of no args to be
;;; called after any event is handled.
(defparameter *repl-event-wait-hook* nil)

#-agl
(defun wait-for-any-event (&optional (flags TCL_ALL_EVENTS))
  (declare (ignorable flags))
  (when (and *repl-event-wait-hook*
	     (zerop (Tcl_DoOneEvent TCL_DONT_WAIT)))
    (funcall *repl-event-wait-hook*))	; update windows before blocking

  ;; CMUCL works just fine with interrupts while in foreign code.
  (Tcl_DoOneEvent flags) 
  1 ;; 1 indicates TCL event handled
  )


#+agl ; LHQ hacks Mon Mar 12 2007
(defun wait-for-any-event (&optional (flags TCL_DONT_WAIT))
  (declare (ignorable flags))
  (setq flags TCL_DONT_WAIT)
  (when (and *repl-event-wait-hook*
	     (zerop (Tcl_DoOneEvent flags)))
    (funcall *repl-event-wait-hook*)  ; update windows before blocking
    ;;(sleep (wait-for-any-event-sleep-time))
    (when (eql flags TCL_DONT_WAIT) (sleep .001)) ; allow another process to run
    ) 
  ;; CMUCL works X86 just fine with interrupts while in foreign code.
  ;; CMUCL with Darwin AGL apparently prevents interrupting Tcl_DoOneEvent 
  (Tcl_DoOneEvent TCL_DONT_WAIT)
  1 ;; 1 indicates TCL event handled
  )



#+old ;;; **************  The remainer of this file contains dead code  ***************************
(progn

(defun eval-print-to-slime (form)
  (swank::eval-in-emacs `(slime-repl-eval-string ,(format nil "~s" form))))

(defun handle-events-until (completion-function &optional (eval-read-forms t))
  (ignore eval-read-forms)
  (loop	#+cmu do #+cmu (lisp::scrub-control-stack)
	when *forced-read-forms*
	  ;; With CMUCL/SLIME WAIT-FOR-ANY-EVENT never returns T.  This causes
	  ;; *forced-read-forms* that occur because of a form that is read and
	  ;; evaled to be ignored until an X11 event occurs.  Thus quit-repl
	  ;; must call quit-repl2.
	  do (catch 'eval-read-forms
	       (restart-case  ;; why was there a restart-case here???
		   (if *freedius-started* 
		       (eval-print-to-slime (pop *forced-read-forms*))
		       ;; cmucl/slime has problems calling eval-print-to-slime during startup
		       (eval (pop *forced-read-forms*))) ; slime isn't fully set up?
		   (abort ()
		     (format t "Aborted~%")
		     (return-from handle-events-until nil))))
	else do (wait-for-any-event) 
	do (do-events)
	until (or *quit-repl* (funcall completion-function))
	))

;;; why the catch?
(defun evaluate-form-in-repl (form)
  (catch 'eval-read-forms
    (restart-case      ;; why was there a restart-case here???
	(if *freedius-started* 
	    (eval-print-to-slime form)
	    ;; cmucl/slime has problems calling eval-print-to-slime during startup
	    (eval form) ; slime isn't fully set up?
	    )
      (abort ()
	(format t "Aborted~%")
	(return-from evaluate-form-in-repl nil)))))

(defun evaluate-form-in-repl (form)
  (restart-case	;; why was there a restart-case here???
      (if *freedius-started* 
	  (eval-print-to-slime form)
	  ;; cmucl/slime has problems calling eval-print-to-slime during startup
	  (eval form)			; slime isn't fully set up?
	  )
    (abort ()
      (format t "Aborted~%")
      (return-from evaluate-form-in-repl nil))))

) ; end #+old progn 


#+old
(progn

(defvar *freedius-repl-output*)

(defun get-freedius-repl-output ()
  (values-list *freedius-repl-output*))

(defun print-and-set-top-level-variables (values)
  (let ((*freedius-repl-output* values))
    (eval-print-to-slime '(tk::get-freedius-repl-output))))

) ; end progn

#+old
(progn

(defun top-level-print (value-list &optional stream)
  (declare (ignorable stream)) 
  (send-values-to-emacs value-list))

(defun add-repl-result (result-list)
  (declare (special *** ** * $$$ $$ $ /// // /))
  (when result-list
    (setq *** ** ** * * (car result-list))
    ;(setq $$$ $$ $$ $ $ result-list)
    (setq /// // // // / result-list)))

(defun print-and-set-top-level-variables (result-list)
  (unless (eq (car result-list) *no-value-return*)
    (add-repl-result result-list)
    (top-level-print result-list)
    ))

) ; end progn


#+old
(progn

;;; Called only from handle-events-until
;;; This is roughly equivalent to 
;;;   (print-and-set-top-level-variables (multiple-value-list (eval form)))
(defun evaluate-form-in-repl (form)
  (if *freedius-started* 
      (swank::eval-in-emacs `(slime-repl-eval-string ,(format nil "~s" form)))
      ;; slime isn't fully set up
      ;; cmucl/slime has problems calling eval-print-to-slime during startup
      (eval form)))


(defun handle-events-until (completion-function &optional (eval-read-forms t))
  (ignore eval-read-forms)
  (loop	do #+cmu (lisp::scrub-control-stack)
	   (if *forced-read-forms*
	       (evaluate-form-in-repl (pop *forced-read-forms*)) 
	       (wait-for-any-event))
	   (do-events)
	until (or *quit-repl* (funcall completion-function))))

) ;end progn
