(in-package :lcl)


(eval-when (:compile-toplevel :load-toplevel)
  (let ((little-endian-p (or #+(or :x86 :amd64 :alpha) t)))
    ;;; sparc, ppc, :hppa are big-endian
    (pushnew (if little-endian-p :LITTLE-ENDIAN :BIG-ENDIAN) 
	     *features*)))

(defun function-name (fn) (lisp::%function-name fn))

(defun quit ()
  (unix:unix-exit))

(defun COMMAND-LINE-ARGUMENT (index)
  (nth index extensions:*command-line-strings* ))

(defun environment-variable (name)
  (getenv name))
  
(alien::def-alien-routine ("getenv" unix-getenv-internal) c-call:c-string
  (name c-call:c-string))

(defun unix-getenv (name)
  (let ((result (unix-getenv-internal (format nil "~a" name))))
    (if (and result (> (length result) 0))
	(concatenate 'string result) ; force a copy
	nil)))

(alien::def-alien-routine ("setenv" unix-setenv-internal) c-call:int
  (name c-call:c-string)
  (value c-call:c-string)
  (overwrite c-call:int))

(defun unix-setenv (name value &optional (overwrite t))
  (unix-setenv-internal name value (if overwrite 1 0)))

(alien::def-alien-routine ("unsetenv" unix-unsetenv) c-call:int
  (name c-call:c-string))

;;; Wed Mar 12 2003: Fixed the CMUCL problem with (setf (environment-variable ...)).  It is now ok
;;; to do (setf (environment-variable ...)) for load-foreign load-paths and run-program,
;;; as well as environment vars needed by foreign code.

(defvar *keyword-package* (find-package :keyword))

(defun (setf environment-variable) (val name)
  (let* ((atom (intern (string-upcase name) *keyword-package*))
	 (hit (assoc atom ext:*environment-list*)))
    (if (stringp val)
	(progn 
	  (if hit 
	      (setf (cdr hit) val)
	      (push (cons atom val) ext:*environment-list*))
	  (unix-setenv name val) ;; this allows foreign loaded code to see environment vars
	  )
	(when hit
	  (setf ext:*environment-list* (delete hit ext:*environment-list*))
	  (unix-unsetenv name)
	  ))
    val))

(defmacro WITH-INTERRUPTS-DEFERRED (&body body)
  `(system::without-interrupts ,@body))

(defun %pointer (object)
  (KERNEL:GET-LISP-OBJ-ADDRESS object))


;;; Use launder-object to avoid type-inference problems in underlying-simple-vector
(defun launder-object (object)
  object)
  
(defun underlying-simple-vector (array0)
  (declare (values simple-array fixnum fixnum))
  (let ((array array0))
    (declare (array array) )
    (if (not (kernel::array-header-p array))
        (values (the simple-array array0) 0 (length array))
        (multiple-value-bind (sarray start end offset)
            (KERNEL::%WITH-ARRAY-DATA array 0 nil)
          (values (the simple-array (launder-object sarray)) offset (- end start))))))

#|
For multi-dimension arrays known to have no displacement,
use KERNEL::%array-data-vector
|#


(declaim (inline array-simple-vector))

;;; This version does not handle simple-vectors
;;;(defun array-simple-vector (array)
;;;  (KERNEL::%array-data-vector array))

;;; This version works with either a simple-vector or a non-simple-vector or array
(defun array-simple-vector (array)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type array array))
  (if (not (kernel::array-header-p array))
      array
      (KERNEL::%array-data-vector array)))

#|
(defun test-sv (v)
  (declare (ext:optimize-interface (speed 3)(safety 0)))
  (declare (optimize (speed 3) (safety 0)))
  (array-simple-vector v))
(disassemble 'test-sv)
|#

(defmacro without-floating-underflow-traps (&body body)
  `(let* ((old-modes (vm::get-floating-point-modes))
	  (old-traps (getf old-modes :traps)))
    (unwind-protect
	 (progn
	   (vm::set-floating-point-modes :traps (remove :underflow old-traps))
	   . ,body)
      (vm::set-floating-point-modes :traps old-traps))))


(defun initialize-lisp-bindings ()
  ;;(config::proclaim-optimizations)
  (setq *read-default-float-format* 'double-float
;;	*print-length* 100
;;	*print-array* nil
	)
  ;;(tpl:setq-default *PRINT-LENGTH* 1000)
  ;;(tpl:setq-default *read-default-float-format* 'double-float)
  ;;(format t ";;; Called cme-lisp-cmucl-bindings")
  ;;(describe-optimization-settings)
  )

(initialize-lisp-bindings) 


;;; Shut up most compiler messages.
(setq *compile-verbose* nil
      *compile-print* nil
      ext:*gc-verbose* nil
      )

