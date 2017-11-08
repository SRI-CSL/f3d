(in-package :ltk)


;communication with wish
;;; this ist the only function to adapted to other lisps

(defun do-execute (program args &optional (wt nil))
  "execute program with args a list containing the arguments passed to the program
   if wt is non-nil, the function will wait for the execution of the program to return.
   returns a two way stream connected to stdin/stdout of the program"
  #+:clisp (declare (ignore wt))
  (let ((fullstring program))
    (dolist (a args)
      (setf fullstring (concatenate 'string fullstring " " a)))
    #+(or :cmu :scl)
    (let ((proc (run-program program args :input :stream :output :stream :wait wt
                             #+scl :external-format #+scl :utf-8)))
      (unless proc
        (error "Cannot create process."))
      (make-two-way-stream
       (ext:process-output proc)
       (ext:process-input proc))
      )
    #+:clisp (let ((proc (ext:run-program program :arguments args :input :stream :output :stream :wait t)))
             (unless proc
               (error "Cannot create process."))
	     proc
             )
    #+:sbcl (let ((proc (sb-ext:run-program program args :input :stream :output :stream :wait wt :search t)))
             (unless proc
               (error "Cannot create process."))
             #+:ext-8859-1
             (make-two-way-stream 
              (sb-sys:make-fd-stream 
               (sb-sys:fd-stream-fd (process-output proc))
               :input t :external-format :iso-8859-1)
              (sb-sys:make-fd-stream 
               (sb-sys:fd-stream-fd (process-input proc))
               :output t  :external-format :iso-8859-1))
             #-:ext-8859-1
	     (make-two-way-stream 
	      (process-output proc)              
	      (process-input proc))	     
             )
    #+:lispworks (system:open-pipe fullstring :direction :io)
    #+:allegro (let ((proc (excl:run-shell-command
			    #+:mswindows fullstring
			    #-:mswindows (apply #'vector program program args)
			    :input :stream :output :stream :wait wt)))
		 (unless proc
		   (error "Cannot create process."))
		 proc
		 )
    #+:ecl(ext:run-program program args :input :stream :output :stream
:error :output)
    #+:openmcl (let ((proc (ccl:run-program program args :input
					    :stream :output :stream :wait wt)))
		 (unless proc
		   (error "Cannot create process."))
		 (make-two-way-stream
		  (ccl:external-process-output-stream proc)
		  (ccl:external-process-input-stream proc)))
    ))

(defvar *ltk-version* "0.90")

;;; global var for holding the communication stream
(defstruct (ltk-connection (:constructor make-ltk-connection ())
			   (:conc-name #:wish-))
  (stream nil)
  (callbacks (make-hash-table :test #'equal))
  (after-ids (make-hash-table :test #'equal))
  (counter 1)
  (after-counter 1)
  (event-queue nil)
  ;; This is should be a function that takes a thunk, and calls it in
  ;; an environment with some condition handling in place.  It is what
  ;; allows the user to specify error-handling in START-WISH, and have
  ;; it take place inside of MAINLOOP.
  (call-with-condition-handlers-function (lambda (f) (funcall f)))
  ;; This is only used to support SERVE-EVENT.
  (input-handler nil))

(defmacro with-ltk-handlers (() &body body)
  `(funcall (wish-call-with-condition-handlers-function *wish*)
	    (lambda () ,@body)))


(defvar *wish-pathname*
  #+freebsd "wish8.4"
  #-freebsd "wish")

(defvar *wish-args* '("-name" "LTK"))

;;; start wish and set (wish-stream *wish*)
(defun start-wish (&rest keys &key handle-errors handle-warnings (debugger t)
                   stream)
  (declare (ignore handle-errors handle-warnings debugger))
  ;; open subprocess
  (if (null (wish-stream *wish*))
      (progn
	(setf (wish-stream *wish*) (or stream (do-execute *wish-pathname* *wish-args*))
	      (wish-call-with-condition-handlers-function *wish*)
	      (apply #'make-condition-handler-function keys))
	;; perform tcl initialisations
        (with-ltk-handlers ()
          (init-wish)))
      ;; By default, we don't automatically create a new connection, because the
      ;; user may have simply been careless and doesn't want to push the old
      ;; connection aside.  The NEW-WISH restart makes it easy to start another.
      (restart-case (ltk-error "There is already an inferior wish.")
	(new-wish ()
	  :report "Create an additional inferior wish."
	  (push *wish* *wish-connections*)
	  (setf *wish* (make-ltk-connection))
	  (apply #'start-wish keys)))))



;;; CMUCL, SCL, and SBCL, use a two-way-stream and the constituent
;;; streams need to be closed.
(defun close-process-stream (stream)
  "Close a 'stream open by 'do-execute."
  (when *debug-tk*
    (format t "Closing wish stream: ~S~%" stream))
  (ignore-errors (close stream))
  #+(or :cmu :scl :sbcl)
  (when (typep stream 'two-way-stream)
    (close (two-way-stream-input-stream stream) :abort t)
    (close (two-way-stream-output-stream stream) :abort t))
  nil)

;;; tcl -> lisp: puts "$x" mit \ und " escaped
;;;  puts [regsub {"} [regsub {\\} $x {\\\\}] {\"}]

;;; call to convert untility
(defun convert(from to)
  (close-process-stream (do-execute "convert" (list from to) t)))


(defun exit-wish ()
  (with-ltk-handlers ()
    (let ((stream (wish-stream *wish*)))
      (when stream
        (remove-input-handler)
        (when (open-stream-p stream)
          (ignore-errors (send-wish "exit")))
        (close-process-stream stream))
      (setf (wish-stream *wish*) nil)
      #+:allegro (system:reap-os-subprocess)
      (setf *wish-connections* (remove *wish* *wish-connections*)))
    nil))

;;; send a string to wish
(defun send-wish (text)
  (declare (string text)
           (optimize (speed 3)))
  (when *debug-tk*
    (format t "~A~%" text)
    (finish-output))
  (let ((*print-pretty* nil)
        (stream (wish-stream *wish*)))
    (declare (stream stream))
    (handler-bind ((stream-error (lambda (e)
                                   (when *debug-tk*
                                     (format t "Error sending command to wish: ~A" e)
                                     (finish-output))
                                   (ignore-errors (close stream))
                                   (exit-wish))))
    (format stream "~A~%" text)
    (finish-output stream))))

#+never
(defmacro format-wish (control &rest args)
  "format 'args using 'control as control string to wish"
  (let ((stream (gensym)))
    `(progn
       (when *debug-tk*
         (format t ,control ,@args)
         (format t "~%")
         (finish-output))
       (let ((*print-pretty* nil)
             (,stream (wish-stream *wish*)))
         (declare (type stream ,stream))
         ;(optimize (speed 3)))
         
         (format ,stream ,control ,@args)
         (format ,stream "~%")
         (finish-output ,stream))
       nil)))

(defun format-wish (control &rest args)
  (when *debug-tk*
    (apply #'format t control args)
    (format t "~%")
    (finish-output))
  (let ((stream (wish-stream *wish*))
	(*print-pretty* nil))
    (declare (type stream stream))
    ;;(optimize (speed 3)))
         
    (apply #'format stream control args)
    (format stream "~%")
    (finish-output stream)
    nil))


(defun format-wish-and-read (control &rest args)
  (apply #'format-wish control args)
  (read-data))

;; differences:
;; cmucl/sbcl READ expressions only if there is one more character in the stream, if
;; it is a whitespace its discarded. Lispworks READs the expression as soon as it can
;; be fully read from the stream - no character is discarded
;; so I am printing an additional space after every READable expression printed from tcl,
;; this has to be eaten for read-line from the stream in lispworks (which returns the line
;; ending character, cmucl/sbcl don't)

(defun read-all(stream)
  (declare (stream stream)
           (inline read-char-no-hang))
  (let ((c (read-char-no-hang stream nil nil))
        (s (make-array 256 :adjustable t :element-type 'character :fill-pointer 0)))
    (loop
       while c
       do
         (vector-push-extend c s)
         (setf c (read-char-no-hang stream nil nil)))
    (coerce s 'simple-string)))

;;; read from wish 
(defun read-wish ()
  "Reads from wish. If the next thing in the stream is looks like a lisp-list
  read it as such, otherwise read one line as a string."
  ;; FIXME: The problem here is that wish sends us error-messages on the same
  ;; stream that we use for our own communication. It would be good if we could
  ;; get the error-messages (that are presumably written to stderr) onto a separate
  ;; stream. The current workaround is based on the observation that wish error
  ;; messages always seem to end on a newline, but this may not always be so.
  ;;
  ;; READ-ALL would be a bad idea anyways, as in that case we could accidentally
  ;; snarf a real message from the stream as well, if it immediately followed
  ;; an error message.
  (let ((*read-eval* nil)
        (*package* (find-package :ltk))
	(stream (wish-stream *wish*)))
    (if (eql #\( (peek-char t stream nil))
	(read stream nil)
	(read-line stream nil))))



(defun can-read (stream)
  "return t, if there is something to READ on the stream"
  (declare (stream stream)
           (inline read-char-no-hang unread-char))
  (let ((c (read-char-no-hang stream)))
    (loop 
       while (and c
                  (member c '(#\Newline #\Return #\Space)))
       do
         (setf c (read-char-no-hang stream)))
    (when c
      (unread-char c stream)
      t)))

(defun read-event (&key (blocking t) (no-event-value nil))
  "read the next event from wish, return the event or nil, if there is no
event to read and blocking is set to nil"
  (or (pop (wish-event-queue *wish*))
      (if (or blocking (can-read (wish-stream *wish*)))
          (read-preserving-whitespace (wish-stream *wish*) nil nil)
          no-event-value)))

(defun read-data ()
  "Read data from wish. Non-data events are postponed, bogus messages (eg.
+error-strings) are ignored."
  (loop
     for data = (read-wish)
     when (listp data) do
       (cond ((eq (first data) :data)
	      (dbg "read-data: ~s~%" data)
	      (return (second data)))
	     (t
	      (dbg "postponing event: ~s~%" data)
	      (setf (wish-event-queue *wish*)
		    (append (wish-event-queue *wish*) (list data)))))
       else do
       (dbg "read-data error: ~a~%" data)))

(defun read-keyword ()
  (let ((string (read-data)))
    (when (> (length string) 0)
      (values (intern #-scl (string-upcase string)
                      #+scl (if (eq ext:*case-mode* :upper)
                                (string-upcase string)
                                (string-downcase string))
                      :keyword)))))

;;; setup of wish
;;; put any tcl function definitions needed for running ltk here
(defun init-wish ()
  ;; print string readable, escaping all " and \
  ;; proc esc {s} {puts "\"[regsub {"} [regsub {\\} $s {\\\\}] {\"}]\""}
  ;(send-wish "proc esc {s} {puts \"\\\"[regsub -all {\"} [regsub -all {\\\\} $s {\\\\\\\\}] {\\\"}]\\\"\"} ")
  ;(send-wish "proc escape {s} {return [regsub -all {\"} [regsub -all {\\\\} $s {\\\\\\\\}] {\\\"}]} ")
  (send-wish "package require Tk")
  (send-wish "proc escape {s} {regsub -all {\\\\} $s {\\\\\\\\} s1;regsub -all {\"} $s1 {\\\"} s2;return $s2}")
  ;;; proc senddata {s} {puts "(data \"[regsub {"} [regsub {\\} $s {\\\\}] {\"}]\")"}
  (send-wish "proc senddata {s} {puts \"(:data [escape $s])\";flush stdout}")
  (send-wish "proc senddatastring {s} {puts \"(:data \\\"[escape $s]\\\")\";flush stdout} ")
  (send-wish "proc senddatastrings {strings} {
                 puts \"(:data (\"
 	         foreach s $strings {
                     puts \"\\\"[escape $s]\\\"\"
                     }
                 puts \"))\";flush stdout} ")

  ;;; proc sendevent {s} {puts "(event \"[regsub {"} [regsub {\\} $s {\\\\}] {\"}]\")"}
  ;(send-wish "proc sendevent {s x y keycode char width height root_x root_y} {puts \"(:event \\\"$s\\\" $x $y $keycode $char $width $height $root_x $root_y)\"} ")
  (send-wish "proc sendevent {s x y keycode char width height root_x root_y mouse_button} {puts \"(:event \\\"$s\\\" $x $y $keycode $char $width $height $root_x $root_y $mouse_button)\"} ")
  ;;; proc callback {s} {puts "(callback \"[regsub {"} [regsub {\\} $s {\\\\}] {\"}]\")"}

  ;;; callback structure: (:callback "widgetname")          ;; for non-parameter callbacks
  ;;;                     (:callback "widgetname" val)      ;; wideget returns non-string value
  ;;;                     (:callback "widgetname" "string") ;; widget returns string value

  (send-wish "proc callback {s} {puts \"(:callback \\\"$s\\\")\";flush stdout} ")
  (send-wish "proc callbackval {s val} {puts \"(:callback \\\"$s\\\" $val)\"} ")
  (send-wish "proc callbackstring {s val} {puts \"(:callback \\\"$s\\\" \\\"[escape $val]\\\")\"} ")

  (dolist (fun *init-wish-hook*)	; run init hook funktions 
    (funcall fun)))






;;;; main event loop, runs until stream is closed by wish (wish exited) or
;;;; the variable *exit-mainloop* is set

(defvar *exit-mainloop* nil)
(defvar *break-mainloop* nil)

(defun break-mainloop ()
  (setf *break-mainloop* t))

(defgeneric handle-output (key params))

(defmethod handle-output (key params)
  (declare (ignore key params)))

(defun process-one-event (event)
  (when event
    (when *debug-tk*
      (format *trace-output* "l:~s<=~%" event)
      (finish-output *trace-output*))
    (cond
     ((and (not (listp event))
           *trace-tk*)
      (princ event *trace-output*)
      (finish-output *trace-output*))
     ((not (listp event)) nil)
     ((eq (first event) :callback)
      (let ((params (rest event)))
        (callback (first params) (rest params))))
     ((eq (first event) :event)
      (let* ((params (rest event))
             (callback (first params))
             (evp (rest params))
             (event (construct-tk-event evp)))
        (callback callback (list event))))
     (t
      (handle-output
       (first event) (rest event))))))

(defun process-events ()
  "A function to temporarliy yield control to wish so that pending
events can be processed, useful in long loops or loops that depend on
tk input to terminate"
  (let (event)
    (loop 
     while (setf event (read-event :blocking nil))
     do
     (process-one-event event))))

(defun main-iteration (&key (blocking t))
  "The heart of the main loop.  Returns true as long as the main loop should continue."
  (let ((no-event (cons nil nil)))
    (labels ((proc-event ()
	       (let ((event (read-event :blocking blocking
					:no-event-value no-event)))
		 (cond
		   ((null event)
                    (ignore-errors (close (wish-stream *wish*)))
                    (exit-wish)
		    nil)
		   ((eql event no-event)
		    t)
		   (t (process-one-event event)
		      (cond
			(*break-mainloop* nil)
			(*exit-mainloop*
			 (exit-wish)
			 nil)
			(t t)))))))
      (restart-case (proc-event)
        (abort ()
          :report "Abort handling Tk event"
          t)
        (exit ()
          :report "Exit Ltk main loop"
          nil)))))

(defun mainloop (&key serve-event)
  (if serve-event
      (install-input-handler)
      (let ((*exit-mainloop* nil)
	    (*break-mainloop* nil)
	    (*read-eval* nil)) ;;safety against malicious clients
	(remove-input-handler)
	(loop while (with-ltk-handlers ()
		      (main-iteration))))))

;;; Event server

#-(or sbcl cmu)
(progn
  (defun install-input-handler ()
    (error "SERVE-EVENT is not implemented on this system"))
  (defun remove-input-handler ()
    nil))

#+(or sbcl cmu)
(progn
  (defun add-fd-handler (fd direction function)
    #+sbcl (sb-sys:add-fd-handler fd direction function)
    #+cmu (system:add-fd-handler fd direction function))

  (defun remove-fd-handler (handler)
    #+sbcl (sb-sys:remove-fd-handler handler)
    #+cmu (system:remove-fd-handler handler))
  
  (defun fd-stream-fd (stream)
    #+sbcl (sb-sys:fd-stream-fd stream)
    #+cmu (system:fd-stream-fd stream))

  (defun make-input-handler (wish)
    "Return a SERVE-EVENT input handler."
    (let ((fd-stream (two-way-stream-input-stream (wish-stream wish))))
      (labels ((call-main ()
		 (with-ltk-handlers () 
		   (handler-bind ((stream-error
				   ;; If there was a stream error on the fd that
				   ;; we're listening to, we need to remove the
				   ;; input handler to avoid getting stuck in an
				   ;; infinite loop of stream errors making
				   ;; noise on the fd, causing us to try to read
				   ;; from it, causing an error, which makes
				   ;; noise on the fd...
				   (lambda (e)
				     (when (eql (stream-error-stream e) fd-stream)
				       (return-from call-main nil)))))
		     (main-iteration :blocking nil))))
	       (ltk-input-handler (fd)
		 (declare (ignore fd))
		 (let ((*wish* wish)) ; use the wish we were given as an argument
                   ;; FIXME: close over all ltk dynamic variables
                   (unless (call-main)
		     (remove-input-handler)))))
	#'ltk-input-handler)))

  (defun install-input-handler ()
    (unless (wish-input-handler *wish*)
      (let ((fd (fd-stream-fd (two-way-stream-input-stream (wish-stream *wish*)))))
	(setf (wish-input-handler *wish*)
	      (add-fd-handler fd :input (make-input-handler *wish*))))))

  (defun remove-input-handler ()
    (remove-fd-handler (wish-input-handler *wish*))
    (setf (wish-input-handler *wish*) nil)))



#|
:HANDLE-ERRORS determines what to do if an error is signaled.  It can be set to
T, NIL, :SIMPLE, or :DEBUG.

When an error is signalled, there are four things LTk can do:

 (default)
     The simplest is to do nothing, which usually means you will end out in the
     SLIME debugger (although see the discussion of :DEBUGGER below).

 note
     Show a dialog box indicating that an error has occured.  The only thing
     the user can do in this case is to hit "OK" and try to keep using the
     application.  The "OK" button will invoke the ABORT restart, which in most
     cases will just return to the LTk main loop.

 show, offer to continue
     Show a dialog box containing the error message.  If there is a CONTINUE
     restart, the user will be prompted with a question and presented with
     "Yes" button and a "No" button.  If there is not CONTINUE restart, the
     only thing the user can do is to hit "OK".  The "Yes" button will invoke
     the CONTINUE restart.  The "No" and "OK" buttons will invoke the ABORT
     restart (see above).

     CONTINUE restarts are usually created by the CERROR function.  In a
     situation where "show, offer to continue" is handling the error, the
     following code:

        (when (= (+ 1 1) 2)
          (cerror "Continue anyway"
                  "One plus one is two."))

     Will tell you that there is an error, display the error message "One plus
     one is two", and ask you "Continue anyway?".  Contrast this with the
     following:

        (when (= (+ 1 1) 2)
          (error "One plus one is two."))

     This will show the user the error "One plus one is two" and allow them to
     hit "OK".

 show, offer to start the debugger
     Show a dialog box containing the error message, and ask the user if they
     want to start the debugger.  Answering "No" will abort (usually to the LTk
     main loop).  Answering "Yes" will invoke the debugger; usually this means
     you will see the SLIME debugger, but see the description of :DEBUGGER
     below.

 LTk considers two types of errors: SIMPLE-ERRORs and all others.  SIMPLE-ERROR
 is what is signalled when you type a form like (error "Something is wrong.").

 If :HANDLE-ERRORS is T, SIMPLE-ERRORs will be shown to the user, and all others
 (such as those generated by the Lisp system itself, eg, if you attempt to divide
      by zero) will be noted.  In this model, you can call ERROR yourself to send an
 error message to the user in a user-friendly manner.  If :HANDLE-ERRORS is NIL,
 LTk will not interfere with the normal error handling mechanism.

 For details of all the options, see the tables below.

 :HANDLE-WARNINGS can be T, NIL, or :DEBUG.


 :DEBUGGER can be T, NIL, or a function designator.  If it is a function
 designator, that function will be used as the debugger.  If it is T, Ltk will
 use the default debugger (see *ltk-default-debugger* for details).  If it is
 NIL, LTk will prevent the user from ever seeing the Lisp debugger.  In the
 event that the debugger would be invoked, LTk will use its "trivial debugger"
 which dumps a stack trace and quits (note that this is only implemented on SBCL
 and CMUCL).  This is useful in conjunction with :HANDLE-ERRORS T, which should
 never call the debugger if :HANDLE-ERRORS is T and the debugger is called, this
 means that the system is confused beyond all hope, and dumping a stack trace is
 probably the right thing to do.
|#

;;

(defun filter-keys (desired-keys keyword-arguments)
  (loop for (key val) on keyword-arguments by #'cddr
	when (find key desired-keys) nconc (list key val)))

(defparameter *default-ltk-debug* 0)
;(setq *default-ltk-debug* 2)

(defun call-with-ltk (thunk &rest keys &key (debug *default-ltk-debug*) stream serve-event
                      &allow-other-keys)
  "Functional interface to with-ltk, provided to allow the user the build similar macros."
  (declare (ignore stream))
  (flet ((start-wish ()
           (apply #'start-wish
                  (append (filter-keys '(:stream :handle-errors
                                         :handle-warnings :debugger)
                                       keys)
                          (debug-setting-keys debug))))
         (mainloop () (apply #'mainloop (filter-keys '(:serve-event) keys))))
    (let ((*wish* (make-ltk-connection)))
      (unwind-protect
           (progn
             (start-wish)
             (with-ltk-handlers () (funcall thunk))
             (mainloop))
        (unless serve-event
          (exit-wish))))))

;;; wrapper macro - initializes everything, calls body and then mainloop

(defmacro with-ltk ((&rest keys &key (debug 2) stream serve-event &allow-other-keys)
		    &body body)
  "Create a new Ltk connection, evaluate BODY, and enter the main loop.

  :DEBUG indicates the level of debugging support to provide.  It can be a
  number from 0 to 3, or one of the corresponding keywords:
  :minimum, :deploy, :develop, or :maximum.

  If :SERVE-EVENT is non-NIL, Ltk will use SERVE-EVENT handlers instead of a
  blocking main loop.  This is only supported on SBCL and CMUCL.  Note that
  using SERVE-EVENT means that WITH-LTK will return immediately after evaluating
  BODY.

  If :STREAM is non-NIL, it should be a two-way stream connected to a running
  wish.  This will be used instead of running a new wish."
  (declare (ignore debug serve-event stream))
  `(call-with-ltk (lambda () ,@body) ,@keys))

