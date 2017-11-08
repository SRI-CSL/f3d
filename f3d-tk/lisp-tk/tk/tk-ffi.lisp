(in-package :lisptk)


;;(compile-ffi-decls-from-header "$FREEDIUS/c/include/tcl-ffi-h")


;;#+mswindows
;;(defun Tk_CreateConsoleWindow (&rest args)
;;  (format t "~%Tk_CreateConsoleWindow is not yet implemented."))

(def-foreign-constants
  "TCL_OK"
  "TCL_ERROR"
  "TCL_GLOBAL_ONLY"
  "TCL_ALL_EVENTS"
  "TCL_DONT_WAIT"
  "TCL_WINDOW_EVENTS"
  "TCL_TIMER_EVENTS"
  "TCL_STATIC"
  "TCL_DYNAMIC"
  "TCL_VOLATILE"
  "TCL_LINK_INT"
  "TCL_LINK_DOUBLE"
  "TCL_LINK_BOOLEAN"
  "TCL_LINK_STRING")

#+allegro
(def-foreign-struct tcl_interp
    (result :type :simple-string)
;;   (result :type (:array :character))
  (freeproc :type dummy-pointer)
  (errorline :type :int)
  )

#+(or :cmu :sbcl)
(def-foreign-struct tcl_interp
    (result :type :simple-string)
  (freeproc :type dummy-pointer)
  (errorline :type :int)
  )


;;#+mswindows
#+mswindows
(def-foreign-function (Tk_CreateConsoleWindow (:name "Tk_CreateConsoleWindow"))
    (interp (:pointer tcl_interp)))

(def-foreign-function (Tk_InitConsoleChannels (:name "Tk_InitConsoleChannels"))
  (interp (:pointer tcl_interp)))

(def-foreign-function (tcl_create_interp (:name "Tcl_CreateInterp")
					 (:return-type (:pointer tcl_interp)))
    )
;(tcl_create_interp)


(def-foreign-function (Tcl_InitMemory (:name "Tcl_InitMemory"))
    (interp (:pointer tcl_interp)))

(def-foreign-function (Tcl_FindExecutable (:name "Tcl_FindExecutable"))
    (interp (:pointer tcl_interp)))


(def-foreign-function (Tcl_Init (:name "Tcl_Init")
				(:return-type :int))
    (interp (:pointer tcl_interp)))

(def-foreign-function (Tk_Init (:name "Tk_Init"))
    (interp (:pointer tcl_interp)))
#|

(def-foreign-function (TclTk_Init (:name "FREEDIUS_TclTk_Init") (:return-type (:pointer tcl_interp)))
    )
    

|#

;;#+fixme
(def-foreign-function (Tcl_WaitForEvent (:name "Tcl_WaitForEvent"))
    (timeval :pointer))


;;; 
(def-foreign-function (Tcl_PollEvent (:name (freedius-prefix "Tcl_PollEvent"))
				     (:return-type :int))
    )

(def-foreign-function (Tcl_InitNotifier (:name "Tcl_InitNotifier")
				     (:return-type :int))
    )


(defun widget-xwindow (widget)
  (hex-string-to-number (tcl-cmd `(winfo id ,widget))))

(defun widget-screen (widget)
  (tcl-cmd `(winfo screen ,widget)))


;;; Is this really used?  Yes.  In later versions of Tcl, it is a
;;; required part of the initialization process.
;;;
;;; Not clear whether this exists anymore!
;;#+mswindows
;;(def-foreign-function (TclpInitPlatform (:name "TclpInitPlatform")))

;;#-mswindows
(defun TclpInitPlatform () nil)

#+mswindows
(def-foreign-function (Tcl_FindExecutable (:name "Tcl_FindExecutable")
					  (:return-type :simple-string))
  (argv0 :simple-string))

#-mswindows
(defun Tcl_FindExecutable (x) x)


#+never
(def-foreign-function (Tcl_InitStubs (:name "Tcl_InitStubs")
				     (:return-type :int))
    (interp (:pointer tcl_interp))
  (version :simple-string)
  (exact :int))

;;Tcl_InitStubs(interp, TCL_VERSION, 1)

#|
(setq int-arr (with-static-area (make-array0 10 :element-type '(unsigned-byte 32))))
(set_tk_display_connection_fdmask int-arr (length int-arr))
(format nil "~x" (aref int-arr 0))

(tk_displays int-arr)
(aref int-arr 0)
|#
;;; Tcl_SetVar makes a copy of newval, so :simple-string is ok.
(def-foreign-function (Tcl_SetVar (:name "Tcl_SetVar"))
    (interp (:pointer tcl_interp))
  (varname :simple-string)
  (newval :simple-string)
  (flags :signed-32bit))

;;; Warning: Tcl_GlobalEval can "temporarily" modify the COMMAND argument
(def-foreign-function  (Tcl_GlobalEval (:name "Tcl_GlobalEval")
				       (:without-gcing nil)
				       (:return-type :signed-32bit))
    (interp (:pointer tcl_interp))
  (command :simple-string)
  )

(def-foreign-function (Tcl_FreeResult (:name "Tcl_FreeResult"))
    (interp (:pointer tcl_interp)))

(defvar *tcl-wait* 1.0)

;;;
;;; Under SBCL threads, protect the command eval entry point - The
;;; interpreter might be a shared resource.
;;;
#+sb-thread
(defvar *tcl-command-mutex* (sb-thread::make-mutex :name "tcl-command-mutex"))

;;;
;;; We have to funnel all tcl evals into the tcl thread.  This is one
;;; way, but it is probably better to queue forced-read-forms to the
;;; repl thread.  Unfortunately, we can't get the return values that
;;; way. 
;;;
#+sb-thread
(defun tcl-global-eval (interp command &optional ignore-result)
  (if (eq sb-thread::*current-thread* *the-thread*)
      (sb-thread::with-recursive-lock (*tcl-command-mutex*)
        (let* ((status (Tcl_GlobalEval interp command))
               (result (unless ignore-result (interp-result interp))))
          (Tcl_FreeResult interp)
          (values status result)))
      (let ((status 0)
            (result ""))
        ;; A hack.
        (push `(tcl-global-eval ,interp ,command)
              *forced-read-forms*)
        (values status result))))
                                     

#-sb-thread
(defun tcl-global-eval (interp command &optional ignore-result)
  (let* ((status (Tcl_GlobalEval interp command))
         (result (unless ignore-result (interp-result interp))))
    (Tcl_FreeResult interp)
    (values status result)))

;;; I believe :simple-string is the right thing here. No copy is made,
;;; and we call Tcl_SetResult with freeproc = TCL_VOLATILE to cause
;;;; TCL to make a copy.
(def-foreign-function (Tcl_SetResult (:name "Tcl_SetResult"))
    #+(and sbcl mswindows) (interp :pointer)
    #-(and sbcl mswindows) (interp (:pointer tcl_interp))
  (command :simple-string)
  (freeproc dummy-pointer) ; this can be either an int or a pointer to a procedure
  )

(def-foreign-function (Tcl_CreateCommand (:name "Tcl_CreateCommand"))
    (interp (:pointer tcl_interp))
  (cmdname :simple-string) 
  (invoke-callback dummy-pointer) ; pointer def-foreign-callable
  (clientdata dummy-pointer) 
  (delete-callback dummy-pointer)	; pointer def-foreign-callable
  )

(def-foreign-function (Tcl_DeleteCommand (:name "Tcl_DeleteCommand"))
    (interp (:pointer tcl_interp))
  (cmdname :simple-string))

;;#-allegro
(def-foreign-function (Tcl_DoOneEvent (:name "Tcl_DoOneEvent")
				      (:without-gcing nil)
				      (:return-type :int))
    (flags :signed-32bit))


#+never ;; #+allegro This doesn't appear to fix the problems in Allegro
;;; This returns -1 on SIGINT -2 on SIGALRM
(def-foreign-function (Tcl_DoOneEvent (:name (freedius-prefix "Tcl_DoOneEvent"))
				      (:return-type :signed-32bit))
  (flags :signed-32bit))

#+never
(def-foreign-function (Tcl_SetMaxBlockTime (:name (freedius-prefix "Tcl_SetMaxBlockTime")))
  (sec :int)
  (usec :int))

#+allegro
(def-foreign-function (Tcl_DoOneEventWithTimeout (:name (freedius-prefix "Tcl_DoOneEventWithTimeout"))
						 (:without-gcing nil)
						 (:return-type :signed-32bit))
    (flags :signed-32bit)
  (sec :int)
  (usec :int))
  

#+sb-thread
(defvar *tcl-event-mutex* (sb-thread::make-mutex :name "tcl-event-mutex"))


#+sb-thread
(defun protected-tcl-do-event (&optional (flags TCL_DONT_WAIT))
  (unless *tcl-event-mutex*
    (setf *tcl-event-mutex*
	  (sb-thread::make-mutex :name "tcl-event-mutex")))
  (or
   ;; This is suspicious.  Why are we recurring on this piece of code?
   (sb-thread::with-recursive-lock (*tcl-event-mutex*) ;; :wait-p nil)
;;   (sb-thread::with-mutex (*tcl-event-mutex* :wait-p nil) 
     (Tcl_DoOneEvent flags))
   0))



#-sb-thread
(defun protected-tcl-do-event (&optional (flags TCL_DONT_WAIT))
  (tcl_DoOneEvent flags))

#-allegro
(defun do-pending-tcl-events ()
  (loop for flg = (protected-tcl-do-event) ;; (Tcl_DoOneEvent TCL_DONT_WAIT )
	while (= flg 1)))

#+allegro
(defun do-pending-tcl-events ()
  (loop for flg = (Tcl_DoOneEvent TCL_DONT_WAIT )
	repeat 1000 ; why is this needed in Allegro ?
	while (= flg 1)))


(defun do-events ()
  (do-pending-tcl-events))


#|
;;; LHQ:  Does this really work?  Ie, as called from REPL, when do timer and file events
;;;   ever get handled?

;;;
;;; Under linux, a condition sometimes arises where Tcl_DoOneEvent
;;; continually returns 1 even though there is apparently nothing to
;;; do, resulting in an infinite loop.  ILISP will hang when this
;;; occurs.  Possibly some event being inappropriately initiated
;;; within Tcl/Tk.  Things get unstuck when we restrict this to window
;;; events only, so allow this as an optional arg:
;;;
(defun do-pending-tcl-events (&key window-events timer-events file-events)
  (let ((arg TCL_DONT_WAIT))
    (when window-events (setf arg (logior arg TCL_WINDOW_EVENTS)))
    (when timer-events (setf arg (logior arg TCL_TIMER_EVENTS)))
    (when file-events (setf arg (logior arg TCL_FILE_EVENTS)))
    (loop for flg = (protected-tcl-do-event arg)
	while (= flg 1))))
|#




;;; *****************  MAKE-TCL-HANDLER-ID  *****************

(defparameter *tcl-handler-callback-map* (make-hash-table)) ; maps id -> handler
(defparameter *tcl-command-id-map* (make-hash-table :test 'equal)) ;maps command -> id
(defvar *tcl-handler-id* 0)
#|
(hash-table-count *tcl-command-id-map*)
(hash-table-count *tcl-handler-callback-map*)
(maphash #'(lambda(key val) (format t "~a => ~a~%" key val)) *tcl-handler-callback-map*)

|#

;;; This is the only place where *tcl-command-id-map* is used?
(defun make-tcl-handler-id (command info)
  (let ((id (gethash command *tcl-command-id-map*)))
    (if nil ;(and id (gethash id *tcl-handler-callback-map*))
	(let ()
	  (format t "make-tcl-handler-id ~a resued~%" command)
	  (setf (gethash id *tcl-handler-callback-map*) info)
	  (values id nil))
	(let ((id (incf *tcl-handler-id*)))
	  (setf (gethash id *tcl-handler-callback-map*) info)
	  (setf (gethash command *tcl-command-id-map*) id)
	  (values id t)))))

(declaim (special *tcl-handler-callback-map* *TK-VERBOSE*))


;;; *****************  INVOKE-TCL-COMMAND CALLBACK  *****************

(defstruct tcl-command-info
  proc    ; lisp function to be called
  interp  ; tcl interpreter -- not clear this is needed -- only 1 *the-interpreter*
  args    ; 
  command ; tcl function name 
  )

(defvar *invoke_tcl_command-address*)
(defvar *delete_tcl_command-address*)

;;; This is the only place in FREEDIUS that uses TYPED-FOREIGN-AREF (except 1 place conditioned on LUCID).
;;; typed-foreign-aref is not yet supported in cmucl
;;; We really need to eliminate this use of TYPED-FOREIGN-AREF
;;; The best approach is to keep the keep the array over on the C side,
;;; and call a C function to access to array element.
  
#|
(progn lisptk::*invoke-tcl-command-args*)
(nth 3 lisptk::*invoke-tcl-command-args*)
(alien:deref (nth 3 lisptk::*invoke-tcl-command-args*) 0)

|#
(defvar *invoke-tcl-command-args*)


(def-foreign-function (invoke_tcl_command_arg_ref
			(:name (freedius-prefix "invoke_tcl_command_arg_ref"))
			(:return-type :simple-string) (:arg-checking nil))
    #-allegro (argv (:pointer (:pointer :char)))
    #+allegro (argv (:pointer (* :char)))
  (i :int))

(defparameter *invoke-tcl-command-verbose* nil)
;(setq *invoke-tcl-command-verbose* t)


(defun invoke-tcl-command (id interp argc argv)
  (declare (ignore interp argv))
  (let ((info (gethash id *tcl-handler-callback-map*)))
    (if (null info)
	(format t "invoke-tcl-command cannot find command with id=~a~%" id)
	
	(let* ((proc (tcl-command-info-proc info))
	       (const-args (tcl-command-info-args info))
	       (args (loop for i from 1 below argc
			   collect (invoke_tcl_command_arg_ref argv i)
			   ))
	       (result
		(block foo
		  ;; Must be extremely careful that proc does not screw up while
		  ;; the mouse is grabbed.  Those functions that could be called
		  ;; during a grab (like menu popup) should enqueue their
		  ;; actions.
		  (when *invoke-tcl-command-verbose*
		    (format t "invoke-tcl-command ~a ~a~%" id (list argc const-args args proc)))
		  (unwind-protect-case
		      ()
		      (progn
			(apply proc (append const-args args)))
		    (:abort (return-from foo nil)))))
	       )

	  ;; What is this ???
	  (when (and (consp result)
		     (numberp (car result))
		     (stringp (cdr result)))
	    (format t "invoke-tcl-command: (list int string)~%"))))))

;;;
;;; *****************  DELETE-TCL-COMMAND CALLBACK  *****************

(defun tcl-delete-command (interp cmd)
  ;;(break)
  (Tcl_DeleteCommand interp cmd))

(defun delete-tcl-command (id)
  (let ((info (gethash id *tcl-handler-callback-map*)))
    (if (null info)
	(format t "delete-tcl-command cannot find command with id=~a~%" id)
	(let* ((command (tcl-command-info-command info))
	       )
	  (remhash id *tcl-handler-callback-map*)
    
    ;;; need to do
	  (remhash command *tcl-command-id-map*))  
	)))


(def-foreign-callable (invoke_tcl_command (:name (freedius-prefix "invoke_tcl_command"))
					  (:return-type :signed-32bit)
					  (:library :liblisptk))
    ((id :signed-32bit)
     (interp (:pointer tcl_interp))
     (argc :signed-32bit)
     ;; ?  This next looks like 3 levels of indirection....?
;;     #+allegro (argv (:pointer (:array (:pointer :char) (999))))
     #+allegro (* (* :char))
     #+cmu (argv (:pointer (:pointer :char)))
     #+sbcl (argv (:pointer (:pointer :char)))
     )
;;  (print id)
  (invoke-tcl-command id interp argc argv)
  (Tcl_SetResult interp "" TCL_VOLATILE)
  tcl_ok)


(def-foreign-callable (delete_tcl_command (:name (freedius-prefix "delete_tcl_command"))
					  (:return-type :signed-32bit)
					  (:library :liblisptk))
    ((id :signed-32bit))
  (delete-tcl-command id)
  0)


;;; *****************  SET-TK-CALLBACKS  *****************

(st:add-system-initialization :tk
			      '(setq *invoke_tcl_command-address*
				(foreign-code-address  (freedius-prefix "invoke_tcl_command"))
				*delete_tcl_command-address*
				(foreign-code-address  (freedius-prefix "delete_tcl_command"))))


;;; *****************  TCL-CREATE-COMMAND  *****************

;;; TCL-CREATE-COMMAND creates a TCL command that calls a LISP function.
;;; INTERP is *the-interpreter* 
;;; TCL-FN is tcl name of callback
;;; LISP-FN is lisp function to call
;;; ARGS are arguments passed to proc at the callback

(defun tcl-create-command (interp tcl-fn lisp-fn &rest args)
  (when args (break))
  (multiple-value-bind (id new)
      (make-tcl-handler-id tcl-fn
			   (make-tcl-command-info :proc lisp-fn
						  :interp interp
						  :args args
						  :command tcl-fn))
    (Tcl_CreateCommand interp
			 tcl-fn
			 *invoke_tcl_command-address*
			 id
			 *delete_tcl_command-address*)
 
    (when *tk-verbose*
      (if new
	  (format t "tcl-create-command NEW ~a ~a ~a~%" tcl-fn lisp-fn args)
	  (format t "tcl-create-command REDEFINITION ~a ~a ~a~%" tcl-fn lisp-fn args)))))


;;; UNUSED FFI DEFINITIONS


;;; unused
;;;(def-foreign-function (Tcl_GetOpenFile (:name "Tcl_GetOpenFile"))
;;;    (interp (:pointer tcl_interp))
;;;  name ; :simple-string usch as stdin or file4
;;;  write ; int
;;;  check-usage ; int (0)
;;;  result-ptr ; pointer to place to store file struct pointer
;;;  )
  
;;;(def-foreign-function (Tcl_CreateFileHandler (:name "Tcl_CreateFileHandler"))
;;;    (file :signed-32bit) ; pointer to file struct
;;;  mask ; or of TCL_READABLE TCL_WRITABLE TCL_EXCEPTION.
;;;  (proc :signed-32bit)                  ; callback
;;;  clientData)

;;;(def-foreign-constants "TCL_READABLE" "TCL_WRITABLE" "TCL_EXCEPTION")

;;;(def-foreign-function (set_tk_display_connection_fdmask
;;;                       (:name "set_tk_display_connection_fdmask"))
;;;    int-array
;;;  num-worlds)

;;;(def-foreign-function (Tcl_LinkVar (:name "Tcl_LinkVar"))
;;;    (interp (:pointer tcl_interp))
;;;  (varName :simple-string)
;;;  addr    ; (:pointer :signed-32bit)
;;;  type    ; TCL_LINK_INT TCL_LINK_DOUBLE TCL_LINK_BOOLEAN TCL_LINK_STRING
;;;)

;;;(def-foreign-function (Tcl_UnlinkVar (:name "Tcl_UnlinkVar"))
;;;    (interp (:pointer tcl_interp))
;;;  (varName  :simple-string)
;;;  )

;;;(def-foreign-function (Tcl_UpdateLinkedVar (:name "Tcl_UpdateLinkedVar"))
;;;    (interp (:pointer tcl_interp))
;;;  (varName :simple-string)
;;;  )

;;;(def-foreign-function (copy_to_tcl_string (:name "copy_to_tcl_string")
;;;                                        (:return-type (:pointer :char)))
;;;    nchars)

;;;(def-foreign-function (Tcl_Merge (:name "Tcl_Merge")
;;;                                 (:return-type (:pointer :character)))
;;;  (argc :signed-32bit)
;;;  argv ;;(argv (:pointer (:pointer :char)))
;;;  )

;;;(def-foreign-function (va_Tcl_Merge (:name "va_Tcl_Merge")
;;;                                    (:return-type (:pointer :character))
;;;                                    (:max-rest-args 100))
;;;    (argc :signed-32bit)
;;;  &rest strings)

;;;(def-foreign-function (Tcl_Invoke (:name "Tcl_Invoke")
;;;                                  (:max-rest-args 100))
;;;    (interp (:pointer tcl_interp))
;;;  cmd ; :simple-string
;;;  &rest args



#+never
(progn

(def-foreign-function (Tcl_FindExecutable (:name "Tcl_FindExecutable"))
    (name :simple-string))

(def-foreign-function (Tcl_GetNameOfExecutable (:name "Tcl_GetNameOfExecutable")
					       (:return-type :simple-string))
    )

;;; (Tcl_FindExecutable (COMMAND-LINE-ARGUMENT 0))
;;;(Tcl_GetNameOfExecutable) = 0

) ; end progn
