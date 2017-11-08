(in-package :qffi)

(define-condition foreign-code-error2 (error)
  ((text :initarg :text :reader error-text))
  (:report (lambda (condition stream)
	     (format stream "Error in foreign code: ~a~%" (error-text condition))))
  )


(defun handle-callback-from-foreign-code (error_msg)
  ;;(error error_msg)
  (error 'foreign-code-error2 :text error_msg)
)


(def-foreign-callable (lisp_error_callback (:name (freedius-prefix "lisp_error_callback"))
                                           (:return-type :unsigned-32bit)
					   )
    ((error_msg :simple-string))
  (handle-callback-from-foreign-code error_msg))


(def-foreign-function (freedius_so_init (:name (freedius-prefix "freedius_so_init"))))

(defvar *initial-debug-level* 2)

(def-foreign-function (setLispErrorHandlers (:name (freedius-prefix "setLispErrorHandlers"))))

(def-foreign-function (set_qcmebindir_path (:name (freedius-prefix "set_qcmebindir_path")))
    (path :simple-string))

(defun init-libfreedius ()
  (setLispErrorHandlers) 
  #+allegro (progn
	      (setq *lisp_call_address_function_address*
		    (ff:get-entry-point (ff:convert-foreign-name "lisp_call_address")))
	      (register-all-callbacks))
  (format t "~%;;; Setting trace debug level to ~d." ;
	  (set-trace-debug-level *initial-debug-level*))

  (set_qcmebindir_path #+unix "/bin" #-unix "c:\\windows\\system32\\" )
  
  (freedius_so_init))

(def-foreign-function (set-trace-debug-level (:name (freedius-prefix "set_write_trace_debug_level")))
    (level :signed-32bit)
    )


;;; This is really the wrong place:  ffi-extensions.lisp is not part of the lisp-extensions system.
(st::add-system-initialization :lisp-extensions '(init-libfreedius))
  
