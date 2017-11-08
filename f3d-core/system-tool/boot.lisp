(in-package :cl-user)

;;; Indicate that FREEDIUS is in use as opposed to CME.
(pushnew :FREEDIUS *features*)

;;; We must have one of :glx, :cocoa, or :wgl on the features list.
;;; Default to glx (OpenGL with X11):
#-(or wgl cocoa)
(pushnew :glx *features*)

(defvar *freedius-asd-pathname*
  (ql::where-is-system :f3d-core))

;;; Make sure no unspecific floats are read as single-floats.
(setq *read-default-float-format* 'double-float) 


#+allegro ;; This allows top level imports and exports without complaints.
(setq comp:*cltl1-compile-file-toplevel-compatibility-p* t)


;;;
;;; Define SETENV for all platforms:
;;;
#+sbcl
(progn

#+win32
(progn
  (defun set-environment-variable-int (name value &optional (overwrite 1))
    (sb-win32::setenv (format nil "~a" name) value))

  (pushnew :mswindows *features*))

#-win32
(sb-alien::define-alien-routine ("setenv" set-environment-variable-int) sb-alien::int 
  (name sb-alien:c-string)
  (value sb-alien:c-string)
  (overwrite sb-alien::int))
)


#+(and cmu (not cmu20))
(alien::def-alien-routine ("setenv" set-environment-variable-int) c-call:int
  (name c-call:c-string)
  (value c-call:c-string)
  (overwrite c-call:int))

#+cmu20
(defun set-environment-variable-int (name value &optional (overwrite 1))
  (UNIX:UNIX-SETENV name value overwrite))


#+allegro
(defun set-environment-variable-int (name value &optional (overwrite 1))
  (setf (sys::getenv name) value))


;;;
;;; This is stolen from Xach's Quicklisp source, but mainly because,
;;; although we love it, we do not want to depend on the presence of
;;; ql.

(defun system-source-path (system-name)
  (let ((system (asdf:find-system system-name nil)))
    (when system
      (asdf:system-source-directory system))))


;;;
;;; Attempt to autodetect the appropriate environment variables:
;;;

(defvar *default-freedius-path*
  (system-source-path :f3d-core)
  #+old
  (progn
    #-(or allegro sbcl) nil
    #+(or allegro sbcl)
    (make-pathname :host (pathname-host *load-pathname*)
		   :directory (butlast (pathname-directory *load-pathname*))))
  )

(defvar *default-freedius-arch-path*
  (make-pathname :host (pathname-host *default-freedius-path*)
		 :directory (append (pathname-directory *default-freedius-path*)
				    (list "arch"
					  ;; These next need to be updated - need a function.
					  #+(and mswindows allegro) "windows-acl"
					  #+(and linux sbcl) "linux-sbcl"
					  #+(and macosx sbcl) "linux-sbcl"
                                          #+cmu "linux-cmucl"
					  ))))


;;;
;;; RADIUS default path will be /opt/IU/radius on non-windows sytems,
;;; and "\RADIUS" on windows:
;;;
(defvar *default-radius-path*
  (make-pathname :host (pathname-host *default-freedius-path*)
		 :directory (append
                             ;;(pathname-directory *default-freedius-path*)
                             #-(or mswindows win32) '(:absolute "opt" "IU")
                             #+(or mswindows win32) '(:absolute)
                             (list "radius"))))


;;; ********************  DEFINE NEEDED PACKAGES  *************************


(defpackage :system-tool (:use :common-lisp)  (:nicknames :st)
	    #+asdf (:import-from :asdf
                                 "GETENV"
                                 "ADD-SYSTEM-INITIALIZATION"
                                 "INITIALIZE-ALL-SYSTEMS")
	    #+cmu (:import-from :lisp "WITHOUT-PACKAGE-LOCKS")
	    #+sbcl (:import-from :sb-ext "WITHOUT-PACKAGE-LOCKS")
	    #+allegro (:import-from :excl "WITHOUT-PACKAGE-LOCKS"))

(defpackage :config (:use :common-lisp)) ; needed for system configuration options

(defpackage :env)

(in-package :system-tool)

#-asdf
(defun getenv (var)
  #+cmu (cdr (assoc var ext:*environment-list* :test #'string=))
  #+sbcl (sb-ext:posix-getenv var)
  #+allegro (sys::getenv var))

(defun setenv (name value)
  (cl-user::set-environment-variable-int name value 1)
  (getenv name))


#|
In emacs:  
   (setf (getenv "FREEDIUS_ARCH") "/opt/IU/FREEDIUS/default/arch/linux-cmucl")
before starting cmucl

(progn (load (format nil "~a/lisp/boot.lisp" (getenv "FREEDIUS")))
       (load-cme))

 (load-cme :start nil)
|#

#+cmu
(defun getenv (name) (unix:unix-getenv name))


;;;
;;; If environment variables are not already set, set them to the
;;; defaults:
;;;

(unless (getenv "FREEDIUS")
  (setenv "FREEDIUS" (namestring cl-user::*default-freedius-path*)))

(unless (getenv "RADIUS")
  (setenv "RADIUS" (namestring cl-user::*default-radius-path*)))



;;; *FREEDIUS-HOME-PATH* is the location of the FREEDIUS source hierarchy.
;;; I suppose that if the FREEDIUS environment variable isn't defined, it 
;;; could be computed from *LOAD-PATHNAME*, but we also need *FREEDIUS-EXEC-PREFIX*,
;;; so we require that both be defined.  
(defvar *freedius-home-path* 
  (or (getenv "FREEDIUS")
      (error "FREEDIUS environment variable must be set")))

;;; The environment-variable FREEDIUS_ARCH should be set by a shell script of an EMACS startup fn.
;;; *FREEDIUS-EXEC-PREFIX* is needed to determine the location of compiled lisp files (fasl)
;;; and foreign code libraries.
(defvar *freedius-exec-prefix*
  (or (getenv "FREEDIUS_ARCH")
      (getenv "FREEDIUS_EXEC_PREFIX") ; legacy name
      (setenv
       "FREEDIUS_ARCH"
       (namestring cl-user::*default-freedius-arch-path*))
      ;; cl-user::*default-freedius-arch-path*
      (error "FREEDIUS_ARCH or FREEDIUS_EXEC_PREFIX environment variable must be set")))

;;; FIXME:  merge-freedius-path is used by several lisp/sysdefs/<system>.sysdef files
(import '(merge-freedius-path) :cl-user) 

(defvar *freedius-arch-name*
  (or (pathname-name *freedius-exec-prefix*)
      (car (last (pathname-directory *freedius-exec-prefix*)))))

(defun merge-freedius-path (file)
  (format nil "~a/~a" *freedius-home-path* file))

(defun merge-freedius-arch-path (file)
  (format nil "~a/~a" *freedius-exec-prefix* file))

(declaim (special config::freedius-pathname-translations))

;;; Pathnames needed in system-tool for lisp and fasl file hierarchies.
;;; This might be better handled with logical-pathnames. 
;;; No, in CMUCL, all components of logical-pathnames are converted to upper-case.
;;; Better to define $FREEDIUS_ARCH ev-pathname translation.
;;; If non-NIL, config::freedius-pathname-translations will override this.
;;; See FIND-SOURCE-FASL-HIERARCIES in system-tool-bootstrap.lisp

;;; new Fri Jun 19 2009 placing all config files in $FREEDIUS/lisp/configs/
;;; There is a similar modification for loading runtime-options, defined in cme-compat/rstart-cme.lisp.
(defun load-config-files ()
  ;; Load the configuration parameters.
  ;; config.lisp contains the configuration options needed to compile and load FREEDIUS.
  ;; Runtime options are determined by LOAD-RUNTIME-OPTIONS in start-cme.lisp.
  (flet ((load-noisely (path)
	   (format t ";;; loading ~a~%" path)
	   (load path)))
    (let* ((user-config-file-path (format nil "~a/freedius-config.lisp" (getenv "HOME")))
	   (arch-config-path 
	    (merge-freedius-path (format nil "lisp/configs/~a-config.lisp"
					 (pathname-name *freedius-exec-prefix*))))
	   )
      (when (probe-file user-config-file-path)
	;;(maybe-compile-file-load user-config-file-path) ; should never have been compiled
	(load-noisely user-config-file-path)
	)
      (when (probe-file arch-config-path) 
	(load-noisely arch-config-path))
      ;; defaults for configs
      (load-noisely (merge-freedius-path "lisp/configs/default-config.lisp")))))

;;; this is needed 
(ensure-directories-exist (merge-freedius-arch-path "lib/"))

;;; Sun Jun 28 2009 -- load config.lisp files before loading system-tool code
;;; so that the fasls go to the right place and have the propery (environment-variable) source
;;; locations recorded.
(load-config-files)
  
;; Do not compile yet since we can't put the fasl file in the right place
;;(load (merge-freedius-path "lisp/system-tool/system-tool-bootstrap.lisp"))

;; reload system-tool-bootstrap.lisp after compiling.
#-quicklisp
(allow-redefinition 
  (maybe-compile-file-load (merge-freedius-path "lisp/system-tool/system-tool-bootstrap.lisp"))
  ) ;; end allow-redefinition

;; Compile and load the remainder of the system-tool.  
;; Note that MIXED-LISP-SYSTEM requires loading the QFFI system
#-quicklisp
(maybe-compile-file-load (merge-freedius-path "lisp/system-tool/system-tool.lisp"))

;;; *************  THE SYSTEM-TOOL IS NOW FUNCTIONAL (without FFI)  *************

;;(import '(cl-user::load-cme)) ; added Tue Aug  3 2004 for CC
(export '(load-cme))

#+cmu
(defun cl-user::lisp-debugfn ()
  (setq *lisp-debugfn-all-processes* (copy-list mp::*all-processes*))
  nil)

;;;
;;; I introduced this because the FREEDIUS web server interface
;;; conflicts with the Tcl/Tk interface.  In particular, the Tcl/Tk
;;; repl locks out the http server under CMUCL.  There may be a
;;; workaround for this, but the simplest fix for now is to prevent
;;; M-x slime-connect from implicitly invoking start-cme:

(defvar *start-cme-after-load* t)

(defun load-cme (&key (start *start-cme-after-load*) load-systems initialize)
  ;; Just to make sure no unspecific floats are read as single-floats.
  (setq *read-default-float-format* 'double-float) 

  (unless (boundp 'config::freedius-pathname-translations)
    (load-config-files)
    ;;(bootstrap-system-tool)
    )
  (load (merge-freedius-path "lisp/optimization-profiles.lisp"))

  (with-compilation-unit ()  
    ;; Set the compiler optimizations
    (config::proclaim-optimizations)
    #+sbcl (maybe-compile-file-load (merge-freedius-path "lisp/sbcl/sbcl-patches.lisp"))
    #+cmu (maybe-compile-file-load (merge-freedius-path "lisp/cmucl/cmucl-patches.lisp"))

    ;;(load-system :ev-pathnames)
    ;; Now we can use environment-variable pathnames
    ;;(st:load-system :lcl)

    (load-system :qffi) ; must load qffi in order for mixed-lisp-system class to be defined
    (load-system :cme)
    )

  (unless start 
    #-cmu (format t "~%;; (start-cme)~%")
    #+cmu (format t "~%;; In *inferior-lisp* eval (cl-user::start-cme)~%"))

  (when start (cl-user::start-cme (append
				   (loop for s in load-systems
					 collect `(st::load-system ,s))
				   `((when ,initialize
				       (st::initialize-all-systems)))))))

