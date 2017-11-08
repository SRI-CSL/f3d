(in-package :lcl)


(eval-when (:compile-toplevel :load-toplevel)
  (let ((little-endian-p (or #+(or :x86 :amd64 :x86-64 :alpha) t)))
    ;;; sparc, ppc, :hppa are big-endian
    (pushnew (if little-endian-p :LITTLE-ENDIAN :BIG-ENDIAN) 
	     *features*)))

(defvar *keyword-package* (find-package :keyword))

(defun function-name (fn) (sb-kernel::%simple-fun-name fn))
;(sb-kernel::%simple-fun-name #'unix-getenv)

(defun quit ()
  (sb-unix:unix-exit))

(defun COMMAND-LINE-ARGUMENT (index)
  (nth index sb-ext::*posix-argv* ))

(defun environment-variable (name)
  (getenv name))
  
#+never
(sb-alien::define-alien-routine ("getenv" unix-getenv-internal) sb-alien:c-string
  (name sb-alien:c-string))

(defun unix-getenv (name)
  (let ((result (sb-ext::posix-getenv (format nil "~a" name))))
    (if (and result (> (length result) 0))
	(concatenate 'string result) ; force a copy
	nil)))

#+mswindows
(defun unix-setenv-internal (name value)
  (sb-win32::setenv (format nil "~a" name) value))


#-mswindows
(sb-alien::define-alien-routine ("setenv" unix-setenv-internal) sb-alien::int 
  (name sb-alien:c-string)
  (value sb-alien:c-string)
  (overwrite sb-alien::int))


(defun unix-setenv (name value &optional (overwrite t))
  (unix-setenv-internal name value (if overwrite 1 0)))

#-mswindows
(sb-alien::define-alien-routine ("unsetenv" unix-unsetenv) sb-alien::int
  (name sb-alien:c-string))


;;; Wed Mar 12 2003: Fixed the CMUCL problem with (setf (environment-variable ...)).  It is now ok
;;; to do (setf (environment-variable ...)) for load-foreign load-paths and run-program,
;;; as well as environment vars needed by foreign code.

#-mswindows
(defun (setf environment-variable) (val name)
  (if val
      (unix-setenv (format nil "~a" name) (format nil "~a" val))
      (unix-unsetenv (format nil "~a" name))))


;;; I'm afraid that the above is not easy to do in Windoze:
#+mswindows
(defun (setf environment-variable) (val name)
  (if val
      (unix-setenv (format nil "~a" name) (format nil "~a" val))
      (unix-setenv (format nil "~a" name) "")))

#|
(setf (environment-variable "foo") "bar")
(environment-variable "foo")
(setf (environment-variable "foo") nil)
|#

(defmacro WITH-INTERRUPTS-DEFERRED (&body body)
  `(sb-sys::without-interrupts ,@body))

(defun %pointer (object)
  (sb-kernel:get-LISP-OBJ-ADDRESS object))

;;; Use launder-object to avoid type-inference problems in underlying-simple-vector
(defun launder-object (object)
  object)
  
(defun underlying-simple-vector (array0)
  (declare (values simple-array fixnum fixnum))
  (let ((array array0))
    (declare (array array) )
    (if (not (sb-kernel::array-header-p array))
        (values (the simple-array array0) 0 (length array))
        (multiple-value-bind (sarray start end offset)
            (sb-kernel::%with-array-data array 0 nil)
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
  (if (not (sb-kernel::array-header-p array))
      array
      (sb-kernel::%array-data-vector array)))

#|
(defun test-sv (v)
  (declare (ext:optimize-interface (speed 3)(safety 0)))
  (declare (optimize (speed 3) (safety 0)))
  (array-simple-vector v))
(disassemble 'test-sv)
|#

(defmacro without-floating-underflow-traps (&body body)
  `(let* ((old-modes (sb-int::get-floating-point-modes))
	  (old-traps (getf old-modes :traps)))
    (unwind-protect
	 (progn
	   (sb-int::set-floating-point-modes :traps (remove :underflow old-traps))
	   . ,body)
      (sb-int::set-floating-point-modes :traps old-traps))))

(defmacro without-divide-by-zero-traps (&body body)
  `(let* ((old-modes (sb-int::get-floating-point-modes))
	  (old-traps (getf old-modes :traps)))
    (unwind-protect
	 (progn
	   (sb-int::set-floating-point-modes :traps (remove :divide-by-zero old-traps))
	   . ,body)
      (sb-int::set-floating-point-modes :traps old-traps))))


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
      ;; sb-ext:*gc-verbose* nil ; no such control -- maybe use sb-ext:*after-gc-hooks*
      )

