(in-package :tk)

#|
LHQ - Mon May 26 2003
  Why are the callbacks (bindings) created here routed thru a different hash-table and handler
  than is used for QCME-CALLBACK?  It would seem desirable for all events to go thru a common
  point.

  Well, they all do go thru a common point:  INVOKE-TCL-COMMAND.  Most of the
  events are dispatched from INVOKE-TCL-COMMAND to QCME-CALLBACK.

  In principle, everything could go thru QCME-CALLBACK to make things more uniform.

  The function qcme_buttonpress_callback in $FREEDIUS/tk/library/qwidgets.tcl, funnels the event
  thru qcme_callback.
 

|#

;;; Called only in this file.
;;; Create a TCL command named TCL-FN which calls (LISP-FN . ARGS).
(defmethod new-command ((widget t) tcl-fn lisp-fn &rest args)
  (ignore args)
  (let ((tcl-fn (if (functionp lisp-fn)        
		    (string-downcase tcl-fn)
		    (string-downcase lisp-fn)))) ; why do we want this?
    (apply 'tcl-create-command *the-interpreter* tcl-fn lisp-fn (copy-list args))
    tcl-fn))

(defparameter *uniq-command-counter* 0)

;;; Called only in this file.
(defun uniq-command (prefix)
  (format nil "~a~d" prefix (incf *uniq-command-counter*)))

;;; Called only in this file.
;;; Create a tcl binding: TCL-TAG TCL-SEQUENCE that calls LISP-FN ARGS 
;;; If LISP-FN = NIL, delete the tcl binding for TCL-TAG and TCL-SEQUENCE.
;;; If TCL-TAG is a symbol or string
(defun tk-bind-event (tcl-tag tcl-sequence tcl-event-keywords
		      lisp-fn &optional (breakp t) args)
  (setq breakp (if (eq breakp t)
		   " ; break"
		   (or breakp "")))
  (cond ((null lisp-fn)
	 (tcl-eval "bind" tcl-tag tcl-sequence "")) ; delete the binding
	
	((or (symbolp tcl-tag) (stringp tcl-tag))
	 (let* ((name (uniq-command tcl-sequence))
		(tcl-fn (string-append tcl-tag "*" name))
		(binding-cmd (string-append  tcl-fn " " tcl-event-keywords breakp )))
	   
	   ;; create tcl command: tcl-fn() => (lisp-fn args)
	   (apply 'tcl-create-command *the-interpreter* tcl-fn lisp-fn (copy-list args))
	   ;; create tcl binding: tcl-tag tcl-sequence => binding-cmd
	   (tcl-eval  'bind tcl-tag tcl-sequence binding-cmd)))

	(t (break)
	
	 (let* ((tcl-fn
		   ;; create tcl command: tcl-fn() => (lisp-fn args)
		   ;; FIXME: THIS LOOKS BROKEN 
		   (apply 'new-command tcl-tag tcl-sequence lisp-fn (copy-list args)))
		  
		  (binding-cmd (string-append tcl-fn " " tcl-event-keywords breakp)))
	     
	     ;; create tcl binding: tcl-tag tcl-sequence => tcl-cmd
	     (tcl-eval 'bind tcl-tag tcl-sequence binding-cmd)))
	))

(defparameter *tk-arg-substs*
  `((serial            "%#" string-to-number)
    (above             "%a" string-to-number) ; hexadecimal?
    (button            "%b" string-to-number)
    (count             "%c" string-to-number)
    (detail            "%d" string-to-symbol)
    (focus             "%f" string-to-number)
    (height            "%h" string-to-number)
    (keycode           "%k" string-to-number)
    (mode              "%m" string-to-symbol)
    (override-redirect "%o" string-to-number)
    (place             "%p" string-to-symbol)
    (state             "%s" string-to-lisp) ; should be string-to-number - or read-from-string
    (time              "%t" identity)
    (width             "%w" string-to-number)
    (x                 "%x" string-to-number)
    (y                 "%y" string-to-number)
    (ascii             "%A" identity)
    (border-width      "%B" string-to-number)
    (send-event        "%E" string-to-number)
    (keyname           "%K" identity)
    (keysym            "%N" string-to-number) ; bad idea to use this -- X11 specific
    (root              "%R" identity)
    (subwindow         "%S" identity)
    (type              "%T" identity)
    ;;		       (window            "%W" string-to-number) ; should be identity
    (pathname          "%W" identity) 
    (widget            "%W" pathname-to-widget) 
    ;;(window            "%W" identity)	; should be identity
    (x-root            "%X" string-to-number)
    (y-root            "%Y" string-to-number)))

(defun args-to-tcl-event-keywords (args)
  (loop with str = ""
	for first = t then nil
	for arg in args
	for entry = (assq (intern (to-string arg) :lisptk) *tk-arg-substs*)
	when entry
	  do (setq str (if first
			   (cadr entry) 
			   (string-append str " " (cadr entry))))
	else do
	  (error "unrecognized event parameter ~a" arg)
	finally (return str)))
  
;;; *******************  CREATE-TK-BINDING  CREATE-TK-CALLBACK  ********************

#| CREATE-TK-BINDING: 

  Create a TCL event binding for TCL-TAG and TCL-SEQUENCE that executes the Lisp
  code BODY with ARGS bound to TCL event keywords.  ARGS must be a list of
  tcl-event-keywords defined in *tk-arg-substs*.

  CREATE-TK-BINDING: Is typically used for handling events that are generic to a
  widget class rather than specific to a particular panel. 

 |#

;;; anonymous lambdas are a real pain in the ass to debug.
(defmacro create-tk-binding (tcl-tag tcl-sequence args &body body)
  `(tk-bind-event ,tcl-tag ,tcl-sequence
    ,(args-to-tcl-event-keywords args)
    #'(lambda ,args
	(setq . ,(loop for arg in args
		       for a = (assq (intern (to-string arg) :lisptk)
				     *tk-arg-substs*)
		       for cvt-fn = (if a
					(caddr a)
					(error "unrecognized event parameter ~a" arg))
		       collect arg
		       collect `(,cvt-fn ,arg)))
	. ,body)))

;;; Improved debuggability getting rid of anonymous lambdas.
(defmacro create-named-tk-binding (name tcl-tag tcl-sequence args &body body)
  `(progn (defun ,name ,args
	    (setq . ,(loop for arg in args
			   for a = (assq (intern (to-string arg) :lisptk)
					 *tk-arg-substs*)
			   for cvt-fn = (if a
					    (caddr a)
					    (error "unrecognized event parameter ~a" arg))
			   collect arg
			   collect `(,cvt-fn ,arg)))
	    . ,body)
	  
	  (tk-bind-event ,tcl-tag ,tcl-sequence
			 ,(args-to-tcl-event-keywords args)
			 #',name)))

#| CREATE-TK-CALLBACK: 

  Create a TCL event binding for TCL-TAG and TCL-SEQUENCE that calls the generic
  callback function QCME-CALLBACK.

  EVENT-CODE-AND-ARGS must be a list of the form (EVENT-CODE . ARGS)
  ARGS must be a list of names of tcl-event-keywords defined in *tk-arg-substs*.

  When the event is triggered, we execute: (QCME-CALLBACK widget EVENT-CODE
  mapped-args) where mapped-args is the result of mapping each tcl-event-keyword
  to its Lisp representation.

  CREATE-TK-CALLBACK is typically used for binding an event-handler that is
  specific to a particular panel.  The event is dispatched to the panel's 
  event-handler: 
  (funcall <panel-event-handler> panel widget widget-name EVENT-CODE ARGS)

  See example in: $FREEDIUS/lisp/applications/inspector/inspector.lisp
|#

(defun create-tk-callback (tcl-tag tcl-sequence event-code-and-args &key (break "break"))
  (tcl-cmd `(bind ,tcl-tag ,tcl-sequence
	     ,(if (stringp event-code-and-args)
		  event-code-and-args
		  (format nil "qcme_callback %W ~a ~a; ~a"
			  (string-downcase (car event-code-and-args))
			  (args-to-tcl-event-keywords (cdr event-code-and-args))
			  (or break ""))))))
