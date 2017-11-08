(in-package :lisptk)

;;; This uses swank::*communication-style* = :SPAWN, the SLIME default for Allegro.

(defparameter *loaded-repl* "acl-slime-repl.lisp")

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
      (progn
        (swank::send-to-emacs event)
        (sleep 1))
      (progn (print event)
             (force-output t))))
;;;
;;; .hcuO
;;;

(defun output-to-slime-repl-fn (fun)
  (let ((swank::*emacs-connection* (swank-repl-process-emacs-connection))
	(string (with-output-to-string (*standard-output*)
		  (funcall fun))))
    (send-to-emacs `(:write-string ,string nil nil))
    (send-to-emacs `(:write-string ,(string #\Newline) nil nil))))

;;; This is identical across all REPL versions
(defmacro with-output-to-slime-repl (&body body)
  `(output-to-slime-repl-fn  #'(lambda () .,body)))

(defun find-process-named (name)
  (loop for proc in mp::*all-processes*
	for thread = (mp::process-thread proc)
	when (and thread (equal (system::thread-name thread) name))
	  return proc))

(defvar *swank-repl-process* nil)
(defvar *swank-repl-process-emacs-connection* nil)

(defun swank-repl-process ()
  (or *swank-repl-process*
      (setf *swank-repl-process* (find-process-named "repl-thread"))))

(defun swank-repl-process-emacs-connection ()
  (or *swank-repl-process-emacs-connection*
      (and (swank-repl-process)
	   (setf *swank-repl-process-emacs-connection*
		 (mp::symeval-in-process 'swank::*emacs-connection* (swank-repl-process))))))

(defun swank-inferior-lisp-process ()
  (find-process-named "Initial Lisp Listener"))

(defvar *no-value-return* (list nil))

(defun print-and-set-top-level-variables (values)
  (unless (eq (car values) *no-value-return*)
    ;; add-repl-result
    (when (and values (swank-repl-process))
      (mp:process-progn (swank-repl-process)
	(setq *** ** ** * * (car values))
	;;(setq $$$ $$ $$ $ $ values)
	(setq /// // // / / values)))
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
      (mp::process-kill repl-process)))
  (when (eql quit-code :quit-cme)
    (mp::process-progn (find-process-named "Initial Lisp Listener")
      (excl::exit))))

(defparameter *forced-read-forms* nil)

(defun add-forced-read-forms (&rest forms)
  (setf *forced-read-forms* (nconc *forced-read-forms* forms))) 

(defun repl (&optional forced-read-forms)
  (unless *tcltk-repl-process*
    (setq *tcltk-repl-process*		; for debugging only
	  (mp::process-run-function ;;mp::process-run-restartable-function
	   "freedius-repl"
	   #'tk::repl-innards
	   forced-read-forms))))

;;; This is identical across all REPL versions
(defun repl-innards (&optional forced-read-forms)
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

(defparameter *wait-for-any-event-sleep-time* .08)

;;; Due to package problems, this is redefined in interactor.lisp
;;; to check whether a drag-op in in progress.
(defun wait-for-any-event-sleep-time () *wait-for-any-event-sleep-time*)

;;; Under MacOSX/Carbon and Allegro, Tcl sometimes blocks, waiting indefinitely 
(defparameter *repl-dont-block* (or #+(or macosx (and allegro (not mswindows))) t))

;;; bind *repl-event-wait-hook* to a function of no args to be called after any event is handled.
(defparameter *repl-event-wait-hook* nil)

;;; New version for *inferior-lisp* process version.  Doesn't need to handle input.
(defun wait-for-any-event (&optional (flags TCL_ALL_EVENTS))
  (declare (ignorable flags)) 
  ;; Loop until an event is seen and handled.
  (if *repl-dont-block*
      (loop with sleep-time = (wait-for-any-event-sleep-time)
	    ;; In Allegro, foreign functions block signals, which screws up all interrupts,
	    ;; and multiprocessing task switching.
	    ;; Keep the code footprint small while polling.
	    while (= (Tcl_DoOneEvent TCL_DONT_WAIT) 0)
	    do #+allegro (mp::process-sleep sleep-time))
      (Tcl_DoOneEvent flags))

  (when *repl-event-wait-hook*
    (funcall *repl-event-wait-hook*));; Update windows
 
  1 ; 1 indicates TCL event handled
  )

;;; **************  The remainer of this file contains dead code  ***************************
#+old
(progn 
(defun repl-innards (&optional forced-read-forms)
  ;;#+(or ALLEGRO-V6.0 ALLEGRO-V6.1 ALLEGRO-V6.2 cmu)
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
				 (handle-events-until #'(lambda() *quit-repl*))
			      ;; CLEAN-UP form -- never leave repl unless *quit-repl* is non-NIL.
			      ;; That confines us to this function even after aborts.
			      (unless *quit-repl* (go spot))
			      ))
		   #+never
		   (quit-repl () 
		     :report (lambda (stream) (format stream "Quit REPL"))
		     (setq *quit-repl* t)
		     )
		   (abort () (format t "Aborted~%")))
		 (unless *quit-repl* (go spot))
		 ))))
      (case exit-code
	(:quit-cme (format t "Quiting FREEDIUS") (lcl::quit))
	(otherwise (format t "Quiting tk::repl~%")))
      )))

(defun handle-events-until (completion-function)
  (loop when *forced-read-forms*
	  do (catch 'eval-read-forms
	       (print-and-set-top-level-variables
		(restart-case
		    ;; why was there a restart-case here???
		    (multiple-value-list (eval (pop *forced-read-forms*)))
		  (abort ()
		    (format t "Aborted~%")
		    (return-from handle-events-until nil)))))
	do (restart-case (wait-for-any-event)
	     (quit-repl () 
	       :report (lambda (stream) (format stream "Quit REPL"))
	       (quit-repl))
	     (abort ()
	       (format t "Aborted~%")
	       (return-from handle-events-until nil)))
	   ;; do (do-events)
	   ;;(when *repl-event-hook* (funcall *repl-event-hook*))
	until (funcall completion-function)))

) ; end #+old progn

#+old
(progn

(defun send-string-to-emacs (string)
  (send-to-emacs `(:write-string ,string nil nil))
  (send-to-emacs `(:write-string ,(string #\Newline) nil nil)))

(defun top-level-print-string (string &optional stream)
  (declare (ignorable stream)) 
  (let ((swank::*emacs-connection* (swank-repl-process-emacs-connection)))
    (send-string-to-emacs string)))

(defun output-to-slime-repl-fn (fun)
  (top-level-print-string
   (with-output-to-string (*standard-output*)
     (funcall fun))))

) ; end progn

#+old
(progn

(in-package :swank)

(defun tk::send-values-to-emacs (values)
  (flet ((send  (value)
           (let ((id (and *record-repl-results*
                          (save-presented-object value))))
             (send-to-emacs `(:write-string ,(prin1-to-string value)
                              ,id nil))
             (send-to-emacs `(:write-string ,(string #\Newline) 
                              nil nil)))))
    (if (null values)
        (send-to-emacs `(:write-string "; No value" nil :repl-result))
        (mapc #'send values))))

;;; This appears to work fine.
(defun top-level-print (value-list &optional stream)
  (declare (ignorable stream)) 
  (let ((swank::*emacs-connection* (swank-repl-process-emacs-connection)))
	(send-values-to-emacs value-list)))

) ; end progn


#| old
(in-package :swank)

(defun tk::output-to-slime-repl-fn (fun)
  (let ((*emacs-connection* (tk::swank-repl-process-emacs-connection))
	(string (with-output-to-string (*standard-output*)
		  (funcall fun))))
    (send-to-emacs `(:write-string ,string nil nil))
    (send-to-emacs `(:write-string ,(string #\Newline) nil nil))))

(defun tk::top-level-print (values &optional stream)
  (declare (ignorable stream)) 
  (let ((*emacs-connection* (tk::swank-repl-process-emacs-connection)))
    (flet ((send  (value)
	     (let ((id (and *record-repl-results*
			    (save-presented-object value))))
	       (send-to-emacs `(:write-string ,(prin1-to-string value)
					      ,id nil))
	       (send-to-emacs `(:write-string ,(string #\Newline) 
					      nil nil)))))
      (if (null values)
	  (send-to-emacs `(:write-string "; No value" nil :repl-result))
	  (mapc #'send values)))))

(in-package :lisptk)

;;; Slashes are assumed by ILISP, so let's honor that convention.
;;; What should be done when result-list = NIL?
(defun add-repl-result (result-list)
  (when result-list
    (mp:process-progn (swank-repl-process)
      (setq *** ** ** * * (car result-list))
      ;;(setq $$$ $$ $$ $ $ result-list)
      (setq /// // // / / result-list))))


(defvar *no-value-return* (list nil))

(defun print-and-set-top-level-variables (result-list)
  (unless (eq (car result-list) *no-value-return*)
    (add-repl-result result-list)
    (top-level-print result-list)))

|#

