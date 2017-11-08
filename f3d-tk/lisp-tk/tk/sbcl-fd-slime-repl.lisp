(in-package :lisptk)

#|  lisptk REPL for SBCL and SLIME/SWANK

This REPL requires minimal modification to the default SLIME/SWANK REPL.

WARNING:  SB-IMPL::SUB-SERVE-EVENT is modified here to call TK::PROCESS-PENDING-TCLTK-EVENTS,
          and to call SUB-SUB-SERVE-EVENT using tk::*tcltk-default-select-timeout*.
          
|#

;;; This is for swank::*communication-style* = :FD-HANDLER only
(eval-when (load eval compile)
  (assert (eq swank::*communication-style* :FD-HANDLER)))

(defparameter *loaded-repl* "sbcl-slime-repl.lisp")

(declaim (special $$$ $$ $))

(defvar *no-value-return* (list nil))

;;; Nothing special needed in :SIGIO single-process environment
(defun output-to-slime-repl-fn (fun)
  (funcall fun))

;;; This is identical across all REPL versions
(defmacro with-output-to-slime-repl (&body body)
  `(output-to-slime-repl-fn  #'(lambda () .,body)))

;;; called from various places
(defun print-and-set-top-level-variables (values)
  (unless (eq (car values) *no-value-return*)
    ;; add-repl-result
    (when values
      (setq *** ** ** * * (car values))
      ;;(setq $$$ $$ $$ $ $ values)
      (setq /// // // // / values))
    ;; top-level-print
    (let ((swank::*emacs-connection* (swank::default-connection)))
      (funcall swank::*send-repl-results-function* values))))

(defparameter *quit-repl* nil)
(defparameter *handle-events-until-sleep-time* .016)

;;; called from menu-choose-internal  in tk/menus.lisp
(defun handle-events-until (completion-function &optional (eval-read-forms t))
  (ignore eval-read-forms)
  (loop	do #+cmu (lisp::scrub-control-stack)
	   (process-pending-tcltk-events)
	until (or *quit-repl* 
		  (handler-case (funcall completion-function)
		    (error () t)))
	do (sleep *handle-events-until-sleep-time*)))

(defparameter *forced-read-forms* nil)
(defparameter *enable-tcltk-events* nil)

(defun repl (&optional forced-read-forms)
  (setq *forced-read-forms* forced-read-forms)
  (setq *enable-tcltk-events* t))

(defvar *process-pending-tcltk-events-recursion-count* 0); prevent recursion during errors.

(defvar *freedius-started* nil) ; flag to inhibit printing during startup.

;;; Set by basic-gui/display.lisp to call REDISPLAY-DAMAGED-VIEWS
(defvar *repl-event-wait-hook* nil)

(defun process-pending-tcltk-events ()
  (when *enable-tcltk-events*
    (when *forced-read-forms*
      (let ((values (multiple-value-list (eval (pop *forced-read-forms*)))))
	(when *freedius-started*
	  (print-and-set-top-level-variables values))
	(return-from process-pending-tcltk-events t)))
    (when (and				;nil
	   *repl-event-wait-hook*
	   (zerop (Tcl_DoOneEvent TCL_DONT_WAIT)))
      (when (< *process-pending-tcltk-events-recursion-count* 3)
	(let ((*process-pending-tcltk-events-recursion-count* 
	       (1+ *process-pending-tcltk-events-recursion-count*)))
	  (funcall *repl-event-wait-hook*) ; update windows before blocking
	  )
	))
    ;;(> (Tcl_DoOneEvent tk::TCL_DONT_WAIT) 0)
    (> (loop while (> (Tcl_DoOneEvent tk::TCL_DONT_WAIT) 0)
	     count t) 0)
    ))


;;; *tcltk-default-timeout* prevents select blocking for arbitrary periods.
;;; Set to NIL to (re)enable blocking in select
(defparameter *tcltk-default-select-timeout* 016000) ; .016 seconds
;(setq *tcltk-default-select-timeout* nil)
;(setq *tcltk-default-select-timeout* 100000)

;;; Hack to the guts of SBCL event handling

#+sbcl
(in-package "SB-IMPL")

;;; From sbcl/src/code/serve-event.lisp
;;; This appears to be the only change needed for tcl-tk repl
#+sbcl
(defun sub-serve-event (to-sec to-usec deadlinep)
  (or
   (tk::process-pending-tcltk-events) 
   (if *periodic-polling-function*
       (multiple-value-bind (p-sec p-usec)
           (decode-internal-time
            (seconds-to-internal-time *periodic-polling-period*))
         (if to-sec
             (loop repeat (/ (+ to-sec (/ to-usec 1e6))
				  *periodic-polling-period*)
                   thereis (sub-sub-serve-event p-sec p-usec)
                   do (funcall *periodic-polling-function*))
             (loop thereis (sub-sub-serve-event p-sec p-usec)
                   do (funcall *periodic-polling-function*))))
       (if (or to-sec to-usec)
	   (sub-sub-serve-event to-sec to-usec)
	   (sub-sub-serve-event nil (and tk::*enable-tcltk-events* tk::*tcltk-default-select-timeout*)))
       )
   (when deadlinep
     (signal-deadline))))



#|
Thu Jul  2 2009  Attempt to do the same for CMUCL
Never get to tk::process-pending-tcltk-events - SWANK doesn't appear to be calling sys::serve-event.

The problem is that wait-until-fd-usable sub-serve-event are compiled using 
     (declaim (start-block wait-until-fd-usable serve-event serve-all-events)), which makes it impossible
to redefine SUB-SERVE-EVENT.



are 
|#
#+never ;;#+cmu
(in-package "LISP")

#+never ;#+cmu
(without-package-locks 

(defparameter *sub-serve-event-count* 0)

;; Hack to the guts of CMUCL event handling
(defun sub-serve-event (to-sec to-usec)
  (declare (type (or null (unsigned-byte 29)) to-sec to-usec))

  (incf *sub-serve-event-count*)
  (when (tk::process-pending-tcltk-events) (return-from sub-serve-event t))

  (when (handle-queued-clx-event) (return-from sub-serve-event t))

  (let ((call-polling-fn nil))
    (when (and *periodic-polling-function*
	       ;; Enforce a maximum timeout.
	       (or (null to-sec)
		   (> to-sec *max-event-to-sec*)
		   (and (= to-sec *max-event-to-sec*)
			(> to-usec *max-event-to-usec*))))
      (setf to-sec *max-event-to-sec*)
      (setf to-usec *max-event-to-usec*)
      (setf call-polling-fn t))
    (unless (or to-sec to-usec)
      (setq to-usec (and tk::*enable-tcltk-events* tk::*tcltk-default-select-timeout*)))

    ;; Next, wait for something to happen.
    (alien:with-alien ((read-fds (alien:struct unix:fd-set))
		       (write-fds (alien:struct unix:fd-set)))
      (let ((count (calc-masks)))
	(multiple-value-bind
	      (value err)
	    (unix:unix-fast-select
	     count
	     (alien:addr read-fds) (alien:addr write-fds)
	     nil to-sec to-usec)
	
	  ;; Now see what it was (if anything)
	  (cond (value
		 (cond ((zerop value)
			;; Timed out.
			(when call-polling-fn
			  (funcall *periodic-polling-function*)))
		       (t
			(call-fd-handler))))
		((eql err unix:eintr)
		 ;; We did an interrupt.
		 t)
		(t
		 ;; One of the file descriptors is bad.
		 (handler-descriptors-error)
		 nil)))))))

); end without-package-locks
