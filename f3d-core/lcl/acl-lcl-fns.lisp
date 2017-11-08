(in-package :lcl)


;;; A minimal subset of run-lcl.lisp.

;;; how to do this in Allegro?
;;;(defun function-name (fn) (??? fn))

(defun quit (&rest x)
  (declare (ignore x))
  (excl::unload-shared-libraries)
  (excl:exit))

(defun COMMAND-LINE-ARGUMENT (index)
  (system:COMMAND-LINE-ARGUMENT index :application nil))

(defun environment-variable (name)
  (system:getenv name))

(defun (setf environment-variable) (value name)
  (setf (sys::getenv name) value))

(defmacro WITH-INTERRUPTS-DEFERRED (&body body)
  `(excl:without-interrupts ,@body))

(defun %POINTER (x)
  (EXCL::POINTER-TO-address x))

(defun underlying-simple-vector (array)
  (multiple-value-bind (1d-arr offset) (excl::array-base  array)
    (values 1d-arr offset (length 1d-arr))))

(defun array-simple-vector (array)
  (multiple-value-bind (1d-arr offset) (excl::array-base  array)
    (values 1d-arr offset (length 1d-arr))))

(defmacro without-floating-underflow-traps (&body body)
  `(progn . ,body))

(defun initialize-lisp-bindings ()
  (setq comp:*cltl1-compile-file-toplevel-compatibility-p* t)
  (proclaim '(optimize (compilation-speed 0) (speed 2) (safety 1)))
  (setq *print-length* 100
	*read-default-float-format* 'double-float)
  (tpl:setq-default *PRINT-LENGTH* 1000)
  (tpl:setq-default *read-default-float-format* 'double-float)
  (format t ";;; Called cme-initialize-lisp-bindings")
  )

(initialize-lisp-bindings)


(defvar *load-binary-pathname-types* (list excl:*fasl-default-type*))
 

(defun REPORT-COMPILER-OPTIONS ()
  (warn "Use of EXCL:EXPLAIN-COMPILER-SETTINGS is preferred.")
  (excl:explain-compiler-settings)
  )

(defmacro WITH-SCHEDULING-ALLOWED (&body body)
  `(progn
     ,@(mapcan #'(lambda (x)
                   (list '(mp:process-allow-schedule)
                         x))
               body))
  )
(defmacro with-scheduling-inhibited (&body body)
  `(mp:without-scheduling ,@body)
  )

(defun FOREIGN-POINTER-P (x)
  (integerp x))

(defun STATIONARY-OBJECT-P (x)
  (or (foreign-pointer-p x)
      (eq :static (sys:pointer-storage-type x))))

(defun WRITABLE-OBJECT-P (x)
  (or (foreign-pointer-p x)
      (not (eq :immediate (sys:pointer-storage-type x)))))


;;; This isn't compatible with the CMUCL version.  Different return values.
;;; Why not use &rest args, rather than a list of specific keywords?
(defun RUN-PROGRAM (name arguments &key input output error-output
		    wait if-input-does-not-exist
		    if-output-exists if-error-output-exists
		    environment directory
		    )
  (let (iostream errstream process)
    (multiple-value-setq (iostream errstream process)
      (excl:run-shell-command
       (apply #'concatenate 'string
              name " "
              (mapcan #'(lambda (x) (list x " ")) arguments))
       :input input
       :output output
       :error-output error-output
       :wait wait
       :if-input-does-not-exist if-input-does-not-exist
       :if-output-exists if-output-exists
       :if-error-output-exists if-error-output-exists
       :environment environment
       :directory directory
       ))
    (values iostream errstream (if wait 0 nil) process))
  )

