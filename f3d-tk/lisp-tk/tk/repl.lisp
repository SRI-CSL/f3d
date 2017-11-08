(in-package :lisptk)

;;; This file should be renamed to xterm-repl.lisp (or something similar).

;;; This REPL is expected to be run from an "vanilla" shell window (ie. without SLIME or ILISP)

(defparameter *loaded-repl* "repl.lisp")

(defun output-to-slime-repl-fn (fun)
  (funcall fun))

;;; This is identical across all REPL versions
(defmacro with-output-to-slime-repl (&body body)
  `(output-to-slime-repl-fn  #'(lambda () .,body)))

#+(or allegro cmu sbcl)
(progn (defvar $ nil) (defvar $$ nil) (defvar $$$ nil))

(defvar *no-value-return* (list nil))

#-sbcl ;; package locking problems here
(declaim (special *** ** * $$$ $$ $ /// // /))

(defun print-and-set-top-level-variables (result-list)
  (unless (eq (car result-list) *no-value-return*)
    ;; add-repl-result
    (when result-list ; is this correct?
      (setq *** ** ** * * (car result-list))
      (setq $$$ $$ $$ $ $ result-list)
      (setq /// // // $$ / $))
    ;; top-level-print
    (loop for result in result-list
	  do (prin1 result)
	     (fresh-line))
    (print-lisp-prompt)
    ;; finish-standard-output-streams
    (dolist (stream '(*standard-output* *error-output* *trace-output* *terminal-io*))
      (when (boundp stream)
	(let ((stream (symbol-value stream)))
	  (when (and (streamp stream) (open-stream-p stream))
	    (finish-output stream)))))
    *))

;;; ******************************************************************************

(defvar *inside-repl* nil)
(defvar *quit-repl* nil)

(defparameter *forced-read-forms* nil)

(defun add-forced-read-forms (&rest forms)
  (setf *forced-read-forms* (nconc *forced-read-forms* forms))) 

(defun quit-repl (&optional quit-code)
  ;; Apparently, throws around foreign code callbacks do not work.
  ;; Thus we enqueue a call to quit-repl2 which will do the dirty work.
  (when *inside-repl*
    (setq *forced-read-forms*
	  (nconc *forced-read-forms* `((quit-repl2 ,quit-code))))))

(defun quit-repl2 (&optional quit-code)
  ;; Throw based method for quitting.  
  (when *inside-repl*
    (setq *quit-repl* (or quit-code t))
    (throw 'quit-repl quit-code )))

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
  (loop	do (progn #+cmu (lisp::scrub-control-stack))
	;; Wait until either a character is typed or some kind of X11 event
	;; occurs.  WAIT-FOR-EVENT returns T if character-available
	when (or *forced-read-forms* (eq (wait-for-any-event) t))
	  do (print-and-set-top-level-variables
	      (multiple-value-list (eval (or (pop *forced-read-forms*)
					     (read)))))
	do (do-events)
	until (or *quit-repl* (funcall completion-function))))

;;;   **********************  TCL based Event Handling  **************************

(defparameter *wait-for-any-event-sleep-time* .08)

;;; Due to package problems, this is redefined in interactor.lisp
;;; to check whether a drag-op in in progress.
(defun wait-for-any-event-sleep-time ()
  *wait-for-any-event-sleep-time*)

#|
;;; This is the redefinition in interactor.lisp
(defun wait-for-any-event-sleep-time ()
  (if (gui::popup-drag *interactor*)
      0
      *wait-for-any-event-sleep-time*))
|#

;;; Under MacOSX/Carbon, Tcl sometimes blocks, waiting indefinitely for a spinlock - see if this fixes it:
(defparameter *repl-dont-block* (or ;(not (find-package :swank))
				 ;; #+(or macosx (and allegro (not mswindows)))
				 #+(and allegro (not mswindows))
				 t))
;;;(setq *repl-dont-block* nil)	    ; SLIME arglist doesn't work in Allegro if *repl-dont-block* = NIL
;;;(setq *repl-dont-block* t)

(defparameter *repl-event-wait-hook* nil)

(defun wait-for-any-event (&optional (flags TCL_ALL_EVENTS))
  (declare (ignorable flags))
  (when (standard-input-available)
    (return-from wait-for-any-event t))
    
  (when (and *repl-event-wait-hook*
	     (zerop (Tcl_DoOneEvent TCL_DONT_WAIT)))
    (funcall *repl-event-wait-hook*)) ; update windows before blocking

  ;; CMUCL works just fine with interrupts while in foreign code.
  (unless *repl-dont-block* (Tcl_DoOneEvent flags)) ; block waiting for an event
  (when   *repl-dont-block*
    ;; In Allegro, foreign functions block signals, which screws up keyboard interrupts,
    ;; and multiprocessing task switching.  Allegro on Linux REALLY SUCKS.  
    ;; The LISP process needs to run continuously even when there are no events.
    ;; Wait until an event is seen (and handled).  
    (loop with sleep-time = (wait-for-any-event-sleep-time)
	  ;; Keep the code footprint small while polling.
	  until (or (not (= (Tcl_DoOneEvent TCL_DONT_WAIT) 0) )
		    (listen) ; Needed for SLIME socket based I/O ?   Apparently no.
		    )
	  do #+allegro (mp::process-sleep sleep-time)
	  #+(or cmu sbcl) (sleep sleep-time) ; an experiment to fix repl problems with SLIME arglist
	  ))   
  (if (standard-input-available)
      t				      ; T indicates input is available
      1)			      ; 1 indicates TCL event handled
  )

(defun char-whitespace-p (char)
  (or (char-equal char #\space)
      (char-equal char #\tab)
      (char-equal char #\newline)))

;;; This fails when it's running in allegro's IDE debug window in Windoze.  
;;; In particular, read-char gets an error.
(defun standard-input-available ()
  (loop while (listen)
	;repeat 100 ; on Allegro this sometime loops infinitely without this limit
	for char = (read-char)
	unless (or (null char) (char-whitespace-p char))
	  do (unread-char char)
	     (return t)))

;;; **************  The remainer of this file contains dead code  ***************************

#+old
(progn

(defun listen-harder (&optional peek-type (stream *standard-input*))
  #-allegro (listen stream)
  #+allegro (peek-char peek-type stream))

(defun listen-harder (&optional peek-type (stream *standard-input*))
  (listen stream))

;;; All of this "stuff" related to terminal-out-stream-position is a no-op
;;; unless ILISP is being used and ilisp-repl-hacks.lisp is loaded.

(defun print-prompt ()
  (print-lisp-prompt)
  (finish-standard-output-streams)
  (set-terminal-output-stream-position)
  )

(defun set-terminal-out-stream-hacks ())
(defun set-terminal-output-stream-position ())
(defun increment-terminal-output-stream-position ())

;;; Needed for vanilla xterm window
;;; This causes problems for allegro-slime in the *slime-repl allegro* buffer.
(defun finish-standard-output-streams ()
  (dolist (stream '(*standard-output* *error-output* *trace-output* *terminal-io*))
    (when (boundp stream)
      (let ((stream (symbol-value stream)))
	(when (and (streamp stream) (open-stream-p stream))
	  ;; Allegro 8.0 and Slime 2.0 seem to interact horribly here.
	  ;; I sometimes see complaints of an attempt to ask for too
	  ;; much heap space.  I suspect there's some sort of
	  ;; buffering error, where I don't know.
	  (finish-output stream))))))


;;; Thu Jun 10 2004 - Changed order of prin1 and fresh-line for use in vanilla xterm window
(defun top-level-print (value-list &optional (stream t))
  (loop for value in value-list
	do (prin1 value stream)
	   (fresh-line stream))
  (unless value-list			; in case of (values) returned
    (increment-terminal-output-stream-position))
  )

(defun print-and-set-top-level-variables (result-list)
  (unless (eq (car result-list) *no-value-return*)
    (add-repl-result result-list)
    (top-level-print result-list)
    ;; Allegro 8 and Slime seem to interact horribly here:
    (finish-standard-output-streams)
    ;;#+allegro
    (print-prompt) ; added Thu Jun 10 2004
    *))

(defparameter *handle-events-until-read-counter* 0)

(defun handle-events-until (completion-function &optional (eval-read-forms t))
  (ignore eval-read-forms)
  (loop	#+cmu do #+cmu (lisp::scrub-control-stack)
	;; Wait until either a character is typed or some kind of X11 event
	;; occurs.  WAIT-FOR-EVENT returns T if character-available
	when (or *forced-read-forms* (eq (wait-for-any-event) t))
	  ;; With CMUCL SLIME WAIT-FOR-ANY-EVENT never returns T.  This causes
	  ;; *forced-read-forms* that occur because of a form that is read and
	  ;; evaled to be ignored until an X11 event occurs.  Thus quit-repl
	  ;; must call quit-repl2.
	  do (catch 'eval-read-forms
	       (let () ;((forced-read *forced-read-forms*))
		 (finish-standard-output-streams)
		 (print-and-set-top-level-variables
		  (restart-case
		      ;; why was there a restart-case here???
		      (multiple-value-list (eval (or (pop *forced-read-forms*)
						     (read))))
		    (abort ()
		      (format t "Aborted~%")
		      (return-from handle-events-until nil))))
		 (when t		;forced-read
		   (finish-standard-output-streams))	; needed for ilisp sync
		 ))
	do (do-events)
	   ;;(when *repl-event-hook* (funcall *repl-event-hook*))
	until (funcall completion-function)
	))

) ; end progn

#+old
(progn

;;; Needed for vanilla xterm window
(defun finish-standard-output-streams ()
  (dolist (stream '(*standard-output* *error-output* *trace-output* *terminal-io*))
    (when (boundp stream)
      (let ((stream (symbol-value stream)))
	(when (and (streamp stream) (open-stream-p stream))
	  (finish-output stream))))))

;;; Slashes are assumed by ILISP, so let's honor that convention.
;;; What should be done when result-list = NIL?
(defun add-repl-result (result-list)
  (declare (special *** ** * $$$ $$ $ /// // /))
  (when result-list
    (setq *** ** ** * * (car result-list))
    (setq $$$ $$ $$ $ $ result-list)
    (setq /// // // $$ / $)))

;;; Thu Jun 10 2004 - Changed order of prin1 and fresh-line for use in vanilla xterm window
(defun top-level-print (value-list &optional (stream t))
  (loop for value in value-list
	do (prin1 value stream)
	   (fresh-line stream)))

(defvar *no-value-return* (list nil))

(defun print-and-set-top-level-variables (result-list)
  (unless (eq (car result-list) *no-value-return*)
    (add-repl-result result-list)
    (top-level-print result-list)
    (print-lisp-prompt)
    (finish-standard-output-streams)
    *))

) ; end progn
