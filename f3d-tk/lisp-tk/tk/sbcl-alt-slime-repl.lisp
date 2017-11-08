(in-package :swank)

(defvar *recursive-send-to-emacs* nil)

(defun send-to-emacs (event)
  "Send EVENT to Emacs."
  ;;(log-event "send-to-emacs: ~a" event)
  (unless *recursive-send-to-emacs*  ;; Don't send if this is recursive.
    (let ((*recursive-send-to-emacs* t))
      (declare (special *recursive-send-to-emacs*))
      (without-slime-interrupts
	(let ((c *emacs-connection*))
	  (etypecase c
	    (multithreaded-connection
	     (send (mconn.control-thread c) event))
	    (singlethreaded-connection
	     (dispatch-event c event)))
	  (maybe-slow-down))))))

(in-package :lisptk)

;;; This uses swank::*communication-style* = :SPAWN, the SLIME
;;; default.  Always invoke (START-CME) from *inferior-lisp*

(defparameter *loaded-repl* "sbcl-alt-slime-repl.lisp")

;;; Ouch.
;;;
;;; For reasons still unclear, sending output to emacs this way causes
;;; Emacs/Slime to go apeshit (; SLIME 2012-08-04) - too many nested
;;; errors.  Near-term solution is to print these events, since they
;;; appear to be restricted to :WRITE-STRINGs.
;;;
(defvar *send-output-to-slime* nil)


(defun send-to-emacs (event)
  (if *send-output-to-slime*
      (swank::send-to-emacs event)
      (progn
	(print event)
	(force-output t))))
;;;
;;; .hcuO
;;;

#-mswindows
(def-foreign-function (usleep
		       (:name "usleep")
		       (:return-type :unsigned-32bit))
  (microseconds :unsigned-32bit))

#+mswindows
(defun usleep (microseconds)
  (sleep (* 1e-6 microseconds))) 


(defun output-to-slime-repl-fn (fun)
  (let ((swank::*emacs-connection* (swank-repl-process-emacs-connection))
	(string (with-output-to-string (*standard-output*)
		  (funcall fun))))
    (send-to-emacs `(:write-string ,string nil nil))
    (send-to-emacs `(:write-string ,(string #\Newline) nil nil))
    ))

;;; This is identical across all REPL versions
(defmacro with-output-to-slime-repl (&body body)
  `(output-to-slime-repl-fn  #'(lambda () .,body)))

(defun find-process-named (name)
  (loop for thread in sb-thread::*all-threads*
	when (and thread (equal (sb-thread::thread-name thread) name))
	  return thread))

(defvar *swank-repl-process* nil)
(defvar *swank-repl-process-emacs-connection* nil)

(defun swank-repl-process ()
  (or *swank-repl-process*
      (setf *swank-repl-process* (find-process-named "repl-thread"))))

(defun swank-repl-process-emacs-connection ()
  (or *swank-repl-process-emacs-connection*
      (and (swank-repl-process)
	   (setf *swank-repl-process-emacs-connection*
		 (sb-thread::symbol-value-in-thread
		  'swank::*emacs-connection*
		  (swank-repl-process))
		 ))))

(defun swank-inferior-lisp-process ()
  (find-process-named "main thread"))

(defvar *no-value-return* (list nil))

(defun print-and-set-top-level-variables (values)
  (unless (eq (car values) *no-value-return*)
    ;; add-repl-result
    (when (and values (swank-repl-process))
      (sb-thread:interrupt-thread
       (swank-repl-process)
       #'(lambda (&rest args)
	   (setq *** ** ** * * (car values))
	   ;;(setq $$$ $$ $$ $ $ values)
	   (setq /// // // / / values))))
    ;; top-level-print
    (let ((swank::*emacs-connection* (swank-repl-process-emacs-connection)))
      (when swank::*emacs-connection*
	(flet ((send  (value)
		      (let ((id (and (boundp 'swank::*record-repl-results*)
				     swank::*record-repl-results*
				     (swank::save-presented-object value))))
			(send-to-emacs `(:write-string ,(prin1-to-string value)
							      ,id nil))
			(send-to-emacs `(:write-string ,(string #\Newline) 
							      nil nil)))))
	  (if (null values)
	      (send-to-emacs `(:write-string "; No value" nil :repl-result))
	    (mapc #'send values)))))))


;;; **********************  REPL for the freedius-repl process  **********************

;;;  This repl does nothing other than handle TCL/TK events and *forced-read-forms*.

(defvar *inside-repl* nil)
(defvar *quit-repl* nil)

(defvar *tcltk-repl-process* nil)

(defun quit-repl (&optional quit-code)
  (declare (ignorable quit-code))
  (let ((repl-process *tcltk-repl-process*)) 
    (when repl-process
      (format t "Quiting tk::repl~%")
      (setq *quit-repl* (or quit-code t))
      (setq *tcltk-repl-process* nil)
      (sb-thread::terminate-thread repl-process)))
  (when (eql quit-code :quit-cme)
    (sb-thread::interrupt-thread (find-process-named "main thread")
				 #'(lambda (&rest args)
				     ;; (mp::process-progn (find-process-named "main thread")
				     (cl-user::quit)))))

(defparameter *forced-read-forms* nil)

(defun add-forced-read-forms (&rest forms)
  (setf *forced-read-forms* (nconc *forced-read-forms* forms))) 



#+never
(defun repl (&optional forced-read-forms)
  (unless *tcltk-repl-process*
    (setq *tcltk-repl-process*		; for debugging only
	  (sb-thread::make-thread ;; mp::process-run-function ;;mp::process-run-restartable-function
	   #'(lambda (&rest args)
	       (format t "~%Reinitializing notifier?") (force-output t)
	       (sb-thread::with-mutex (*tcl-event-mutex*) ;; :wait-p nil)
		 (init-tcl-tk *default-tk-init-argv*)
		 (Tcl_InitNotifier)
		 (setf *the-interpreter* nil)
		 (init-lisptk nil :start-repl nil))
	       (format t "~%Starting the tcltk repl thread...") (force-output t)
	       (tk::repl-innards forced-read-forms))
	   :name "freedius-repl"
	   ))))

(defun repl (&optional forced-read-forms)
  (format t "~%Reinitializing notifier?") (force-output t)
  (init-tcl-tk *default-tk-init-argv*)
  (Tcl_InitNotifier)
  (setf *the-interpreter* nil)
  (init-lisptk nil :start-repl nil)
  (format t "~%Starting the tcltk repl...") (force-output t)
  (tk::repl-innards forced-read-forms)
  )


;;; This is identical across all REPL versions
(defun repl-innards (&optional forced-read-forms)
  (setq *forced-read-forms* forced-read-forms)
  (setq *repl-thread* sb-thread::*current-thread*)
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
			      (unless *quit-repl* (go spot))))
		   (abort () (format t "Aborted~%")))
		 (unless *quit-repl* (go spot))
		 ))))
      (case *quit-repl* ; exit-code
	(:quit-cme (format t "Quiting FREEDIUS") (lcl::quit))
	(otherwise (format t "Quiting tk::repl~%")))
      )))

(defun handle-events-until (completion-function &optional ignore)
  (declare (ignore ignore))
  (loop when *forced-read-forms*
	  do (print-and-set-top-level-variables
		   (multiple-value-list (eval (pop *forced-read-forms*))))
	do (wait-for-any-event)
	until (or *quit-repl* (funcall completion-function))))

;;;   **********************  TCL Event Handling  **************************

(defparameter *wait-for-any-event-sleep-time* .001)

;;; Due to package problems, this is redefined in interactor.lisp
;;; to check whether a drag-op in in progress.
(defun wait-for-any-event-sleep-time () *wait-for-any-event-sleep-time*)

;;; Under MacOSX/Carbon and Allegro, Tcl sometimes blocks, waiting indefinitely 
;;(defparameter *repl-dont-block* (or #+(and allegro (not mswindows)) t))

;;(defparameter *repl-dont-block*  #+linux t #-linux nil)

;; If we spin this off to its own thread then we can let it block.
;; The problem there is that any tcl evaluations MUST be protected.

(defparameter *repl-dont-block*  nil)

;;; bind *repl-event-wait-hook* to a function of no args to be called after any event is handled.
(defparameter *repl-event-wait-hook* nil)

;;; New version for *inferior-lisp* process version.  Doesn't need to handle input.
(defun wait-for-any-event (&optional (flags TCL_ALL_EVENTS))
  (declare (ignorable flags)) 
  ;; Loop until an event is seen and handled.
  #+macosx (usleep 5000)
  ;; (sleep (wait-for-any-event-sleep-time))

  (if *repl-dont-block*
      (loop with sleep-time = (wait-for-any-event-sleep-time)
	    ;; In Allegro, foreign functions block signals, which screws up all interrupts,
	    ;; and multiprocessing task switching.
	    ;; Keep the code footprint small while polling.
	    while (= (protected-tcl-do-event TCL_DONT_WAIT) 0)
	    do
	 ;; (sb-thread::thread-yield)
	 (usleep (round (* 100000.0 sleep-time)))
	 ;;(sleep sleep-time)
	    )
      (protected-tcl-do-event flags))

  (when *repl-event-wait-hook*
    (funcall *repl-event-wait-hook*));; Update windows
 
  1 ; 1 indicates TCL event handled
  )



