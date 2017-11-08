(in-package :system-tool)

(export '(MERGE-FREEDIUS-PATH DEFINE-SYSTEM LOAD-SYSTEM 
	  ADD-SYSTEM-INITIALIZATION FIND-SYSTEM-NAMED AUTOLOAD-SYSTEM))

(export '(*all-systems* *sysdef-path-environment-variable* *sysdef-search-paths*))

(defvar *sysdef-path-environment-variable* "FREEDIUS_SYSDEF_PATH")

(defvar *all-systems* nil "The list of system-objects of all defined system.")

;;; old variable for controlling search paths.
(defvar *sysdef-paths-override* nil)

;;; Set this to a list of directories containing sysdef files.
;;; Cannot use CUSTOM:DEFCUSTOM SINCE the custom system isn't yet loaded.
;;; Use DEFVAR here so *sysdef-search-paths* can be set before loading this file.
;;; However, no systems get loaded until after config.lisp is loaded.  Hence,
;;; *sysdef-search-paths* can be customized in config.lisp.
(defvar *sysdef-search-paths* nil)

;;; *systems-initialized* appears to be wrong -- not clear that
;;; a single global for this is correct.
(defvar *systems-initialized* nil)

(defvar *initialize-system-after-load* nil)

(defun load-pathname () 
  (unless *load-pathname*
    (error "LOAD-PATHNAME must be called from the dynamic environment of LOAD
Cannot be called from eval"))
  (ev-pathname-backtranslate *load-pathname*))

(defclass basic-system ()
  ((name
    :reader system-name
    :initarg :name
    :initform nil)
   (version
    :accessor system-version
    :initarg :version
    :initform nil)
   (state
    :accessor system-state
    :initarg :state
    :initform nil)
   (sysdef-pathname
    :reader system-sysdef-pathname
    :initform nil)
   (documentation
    :accessor system-documentation
    :initarg :documentation
    :initform nil)))

;;;; interface

(defgeneric load-system (system-name-or-object &key initialize recompile &allow-other-keys)
  (:documentation  "Loads a system, searching for the sysdef file if necessary."))

(defgeneric initialize-system (system-name-or-object)
  (:documentation  "Performs actions on the system's initialization list."))

(defgeneric add-system-initialization (system-name-or-object form &optional when)
  (:documentation  "Adds an initialization to the system's initialization list."))

(defun show-herald (&key (all-systems *all-systems*) (stream *standard-output*))
  "Print a listing of the defined systems and their states"
  
  ;;(format stream "Current Time: ~a~%" (print-universal-time (get-universal-time) nil))
  ;;(format stream "Running Disksave: ~a~%" (command-line-argument 0))
  ;;(format stream "Uname Info: ~s~%" (uname-info))

  (format stream "SUBSYSTEMS:~%")
  (loop for system in all-systems
	for patch-level = (system-patch-level system)
	for version = (system-version system)
	do
     (format stream
	     "~&   ~a ~aState: ~a ~a"
	     (system-name system)
	     (if version (format nil "Version: ~s " version) "")
	     (system-state system)
	     (if patch-level
		 (format nil "Patch-Level: ~a" patch-level)
		 ""))))

#+never
(defun initialize-all-systems (&key (all-systems *all-systems*) (stream *standard-output*) silentp)
  "Invoke the function initialize-system on all systems"
  #+debug (break)
  (loop for system in all-systems
	unless silentp
	  when (system-initializations system)
	    do (format stream "~&;;; Initializing ~a subsystem Version: ~a"	;
			      (system-name system) (system-version system))
	do (initialize-system system))
  (unless silentp (terpri))
  (setq *systems-initialized* t))
	

;;;(defmethod print-object ((system basic-system) stream)
;;;  (format stream "#<~S ~A ~A>"
;;;          (type-of system)
;;;          (if (slot-boundp system 'name) (system-name system) "(unnamed)")
;;;          (if (slot-boundp system 'state) (system-state system) "(uninitialized)")))

(defmethod print-object ((system basic-system) stream)
  (print-unreadable-object (system stream :type t :identity t)
    (princ (system-name system) stream)))

(defmethod initialize-instance :after ((system basic-system) &key &allow-other-keys)
  (with-slots (name state) system
    (setf *all-systems* (append *all-systems* (list system)))
    ;;(push system *all-systems*)
    (setq name (string-upcase name)
	  state :defined)))

;;; FIXME -- reload the system definition file if its last-write-time has newer than the
;;; time it was last read.
(defmethod find-defined-system (name)
  ;; canonicalize system name
  (setq name (string-upcase (string name)))
  #-quicklisp
  (find name *all-systems* :key #'system-name :test #'string= :from-end t)
  #+quicklisp
  (ql::find-system name)
  )

(defmethod find-defined-system ((system basic-system))
  system)

(defmethod autoload-system (system &key (initialize t))
  #+quicklisp t
  #-quicklisp
  (unless (find-defined-system system)
    (load-system system :initialize initialize)))

(defun undefine-system (name)
  (let ((system (find-system-named name)))
    (when system
      (setf *all-systems* (remove system *all-systems* :test 'equal)))))
  
(defmethod system-state-eq (system-name test-state)
  (let ((system (find-defined-system system-name)))
    (and system (eq (system-state system) test-state))))

;;; ***************************  SIMPLE-LISP-SYSTEM  ***************************

(defclass simple-lisp-system (basic-system)
  ((default-pathname
    :initform nil
    :initarg :default-pathname
    :accessor system-default-pathname)
   (default-binary-pathname
    :initform nil
    :initarg :default-binary-pathname
    :accessor system-default-binary-pathname)  
   (files
    :initform nil
    :initarg :files
    :accessor system-files)

   (initializations
    :initform nil
    :initarg :initializations
    :accessor system-initializations)
   (required-systems
    :initform nil
    :initarg :required-systems
    :accessor system-required-systems)
   (default-required-systems-not-loaded-action
    :initform :load
    :initarg :default-required-systems-not-loaded-action
    :accessor system-default-required-systems-not-loaded-action)
   (patchable :initform nil :initarg :patchable :accessor patchable)
   ;; This interacts with $CME/emacs-site-lisp/packages/3dius-support/cl-patch-sys.el
   ;; DEFAULT-PACKAGE is passed on to MAKE-PATCHABLE-SYSTEM and might (someday) end
   ;; up in the (IN-PACKAGE <default-package>) in patch files for the system.
   ;; For now, this can be safely ignored.
   (default-package :initform "CL-USER" :initarg :default-package :accessor system-default-package)
   (patch-system :initform nil :accessor patch-system)
   ))

(defmethod system-version ((system simple-lisp-system))
  (with-slots (version) system
    (if (and (patchable system) (patch-system system))
	(major-version (patch-system system))
	(or version 0))))


(defmethod system-patch-level ((system simple-lisp-system))
  (and (patch-system system)
       (patch-level (patch-system system))))

;;;(defmethod print-object ((system simple-lisp-system) stream)
;;;  (if *print-escape*
;;;      (format stream "#<~S ~A ~A ~a>"
;;;              (type-of system)
;;;              (if (slot-boundp system 'name) (system-name system) "(unnamed)")
;;;              (if (slot-boundp system 'state) (system-state system) "(uninitialized)")
;;;              (if (patch-system system)
;;;                  (format nil "patch-level ~a " (system-patch-level system))
;;;                  ""))
;;;      (format stream "#<~S ~A ~A ~a>"
;;;              (type-of system)
;;;              (if (slot-boundp system 'name) (system-name system) "(unnamed)")
;;;              (if (slot-boundp system 'state) (system-state system) "(uninitialized)")
;;;              (if (patch-system system)
;;;                  (format nil "patch-level ~a " (system-patch-level system))
;;;                  ""))))

(defmethod print-object ((system simple-lisp-system) stream)
  (print-unreadable-object (system stream :type t :identity nil)
    (format stream "~A ~A ~a"
	    (if (slot-boundp system 'name) (system-name system) "(unnamed)")
	    (if (slot-boundp system 'state) (system-state system) "(uninitialized)")
	    (if (patch-system system)
		(format nil "patch-level ~a " (system-patch-level system))
		""))))

(defmethod describe-object ((system simple-lisp-system) stream)
  (format stream "~%~%System: ~a~%" (system-name system))
  (format stream "  :required-systems ~s~%" (system-required-systems system))
  (format stream "  :default-pathname ~a~%"
	  (namestring (system-default-pathname system)))
  (format stream "  :files~%")
  (loop for file in (system-files system)
	do (format t "    ~s~%" file))

  )

(defmethod initialize-instance :after
	   ((system simple-lisp-system) &key &allow-other-keys)

  (with-slots (initializations name default-pathname default-binary-pathname)
      system
    (flet ((bless-pathname (pathname default-type error-name)
	     (when (stringp pathname) (setq pathname (pathname pathname)))

	     (unless pathname (error ";;; ~a not defined for system ~a" error-name name))
	     (when (eq (car (pathname-directory (ev-pathname-translate pathname)))
		       :relative)
	       (error ";;; ~a ~a is a relative pathname" error-name  pathname))

	     (unless (pathname-type pathname)
	       (setq pathname (make-pathname :name "*" :type default-type :defaults pathname)))

	     ;; This next looks totally bogus
	     #+never
	     (when (pathname-name pathname)
	       (setq pathname (make-pathname :name "*" :defaults pathname)))
	     
	     (ev-pathname-backtranslate pathname))

	   )
  
      ;; default binary directory to source directory
      (setq default-binary-pathname (or default-binary-pathname default-pathname))

      (setq default-pathname (bless-pathname default-pathname
					     (car (load-source-pathname-types))
					     "default-pathname"))
      (setq default-binary-pathname (bless-pathname default-binary-pathname
						    *lisp-compiled-file-type*
						    "default-binary-pathname"))
  
      #+debug
      (format t "~&System=~s~&Default pathname=~s~&Default Binary pathname=~s"
	      system default-pathname default-binary-pathname)
      )))

    
(defmethod merge-system-source-pathname (pathname (system simple-lisp-system))
  (merge-pathnames pathname (system-default-pathname system)))
    
(defmethod merge-system-source-pathname (pathname system)
  (merge-system-source-pathname pathname (find-system-named system)))

(defmethod merge-system-binary-pathname (pathname (system simple-lisp-system))
  (merge-pathnames pathname (system-default-binary-pathname system)))


(defmethod add-system-initialization
	   ((system simple-lisp-system)
	    form
	    &optional when)
  
  (declare (ignore when))
  (with-slots (initializations name) system
    (unless (member form initializations :test #'equal)
      (setf initializations
	    (nconc initializations
		   (list form))))))

(defun same-system-p (sys1 sys2)
  (labels ((system-namestring (sys)
	     (typecase sys
	       (string sys)
	       (symbol (symbol-name sys))
	       (simple-lisp-system (system-namestring (system-name sys))))))
    (string-equal (system-namestring sys1) (system-namestring sys2))))

(defvar *required-systems-recursion* nil)

(defmethod check-for-required-systems
    ((system simple-lisp-system)
     &key
     (required-systems (system-required-systems system))
     (not-loaded-action (system-default-required-systems-not-loaded-action system))
     load-args)
  (let ((*required-systems-recursion* (cons (system-name system) *required-systems-recursion*)))
    (when not-loaded-action
      (loop for rsys in required-systems
	    unless (or (system-state-eq rsys :loaded)
		       (system-state-eq rsys :initialized)
		       (member rsys *required-systems-recursion* :test #'same-system-p))
	      do
	   (ecase not-loaded-action
	     (:load (apply 'load-system rsys load-args)) 
	     (:warn (format *error-output*
			    "~&;;; WARNING: The required system ~s was not found." rsys))
	     (:error (cerror "What, me worry?"
			     "The required system ~s was not found." rsys)))))))

(declaim (special *force-recompilation*  
		  #-sbcl lisp::*in-compilation-unit*))

(defvar *system-being-loaded* nil)

(defmethod compile-system ((system simple-lisp-system))
  #-sbcl (format t "compile-system lisp::*in-compilation-unit* = ~a~%" lisp::*in-compilation-unit*)
  (with-compilation-unit ()
      (check-for-required-systems system)
    (loop with *force-recompilation* = t
	  for file in (system-files system)
	  unless (consp file)
	    do (let ((*system-being-loaded* (system-name system)))
		 (maybe-compile-file-load
		  (merge-system-source-pathname file system))))))

(defmethod load-system-file ((file-spec string) (system simple-lisp-system))
  (let ((*system-being-loaded* (system-name system)))
    (maybe-compile-file-load
     file-spec :pathname-defaults (system-default-pathname system))))

(defmethod get-system-master-file ((system t))
  (get-system-master-file (find-system-named system)))

(defmethod get-system-master-file ((system simple-lisp-system))
  (with-slots (default-binary-pathname) system
    (merge-pathnames
     (format nil "~a.system" (system-name system))
     default-binary-pathname)))

(defmethod load-system-file ((file-spec cons) (system simple-lisp-system))
  (destructuring-bind (type &rest args) file-spec
    (ecase type
      (:skip)
      (:eval (apply #'eval args))
      (:load (destructuring-bind (file-spec &rest load-args) args
	       (declare (ignore load-args))
	       (maybe-compile-file-load file-spec :pathname-defaults
					(system-default-pathname system)))))))
#|
(verify-system-files (find-system-named 'hub))
|#

;;; Returns NIL if all files required by the system are available, otherwise
;;; returns a list of missing files

;;; unused
;;; Currently (Sun Sep 26 1999) only VERIFY-SYSTEM-FILES defined in system-tool.lisp
;;; calls this function.  This needs to be rethought for consistency with
;;; CME-COMPILE-OBJECT-FILE-PATHNAME+
;;;(defun compiled-file-pathname (source-pathname)
;;;  (flet ((strip-leading-dot (string)
;;;           ;; Not sure this is needed anymore.
;;;           (if (and (stringp string)
;;;                    (> (length string) 0)
;;;                    (char= (aref string 0) #\.))
;;;               (subseq string 1)
;;;               string)))
;;;    (let* ((source-type (pathname-type source-pathname))
;;;           (object-defaults
;;;            (cdr (assoc source-type *source-to-compiled-file-alist* :test 'equal)))
;;;           (object-pathname (make-pathname :defaults source-pathname
;;;                                           :type (strip-leading-dot object-defaults))))
;;;      object-pathname)))

;;; unused
;;;(defun compiled-file-pathnames (source-pathnames)
;;;  (loop for path in source-pathnames
;;;        collect (compiled-file-pathname path)))

(defmethod verify-system-files ((system simple-lisp-system) &key (verbose t))
  (flet ((bless-file-spec (file)
	   (if (stringp file)
	       (merge-system-source-pathname file system)
	       file)))
    (let* ((*required-systems-recursion* (cons system *required-systems-recursion*))
	   (my-missing (loop for file-spec in (system-files system)
			    with file2
			    for found = (or (not (stringp file-spec))
					    (probe-file (setq file2 (bless-file-spec file-spec)))
					    ;;(probe-file (compiled-file-pathname file2))
					    (probe-file (compile-object-file-pathname file2)))
			    unless found
			      collect file2 into missing-files
			      and do 
				(when verbose
				  (format t ";;; VERIFY-SYSTEM-FILES of ~a: file ~a is missing~%"
					  system file2))
			    finally (return (and missing-files (list* system missing-files)))))
	  (subsystems-missing
	   (loop for sub-system in (system-required-systems system)
		 for missing = (unless (member sub-system *required-systems-recursion* 
					       :test #'same-system-p)
				 (verify-system-files (find-system-named  sub-system) :verbose verbose))
		 when missing append missing)))
      (if my-missing
	  (list* my-missing subsystems-missing)
	  subsystems-missing))))

;;; FIXME:  once loaded, the system files are never loaded again.

;;; New arg: :compile-if-new defaults to T, same behavior as the old version.
;;;
;;; If compile-if-new is 
;;;     1) T, source files that are newer than compiled versions are recompiled,
;;;     2) NIL, existing compiled files are loaded.
;;;
;;; Case 2 applies when binaries or lisp images have been distributed
;;; without source.  This is designed to accomodate binary-only
;;; distributions with patches, and distributions that are on isolated
;;; (non-networked) systems.

(defmethod load-system ((system simple-lisp-system)
			&key
			;;(initialize *systems-initialized*) ; this looks wrong
			(initialize *initialize-system-after-load*)
			(compile-if-new t)
			recompile
			(recompile-subsystems recompile)
			(load-from-scratch t)
			(load-patches nil)
			(force nil)
			&allow-other-keys)
  (with-compilation-unit ()
      (with-slots (patch-system patchable) system
	(let ((*force-recompilation* ; communication var to maybe-compile-file-load
	       (or *force-recompilation* recompile))
	      (*compile-if-new* compile-if-new)
	      #+old
	      (load-from-scratch t ; change LHQ Sat May 29 2004
		;;(not (member (system-state system) '(:loaded :initialized)))
		)
	      )
	  (declare (special *compile-if-new*))

	  (format t ";;; Loading system ~a~%" (system-name system))

	  (when load-from-scratch
	    (let ((missing-files (verify-system-files system)))
	      (when missing-files
		(cerror "Continue loading system?" "System ~a is missing files: ~a" system missing-files)))
	    (check-for-required-systems system :load-args
					(list :recompile recompile-subsystems
					      :initialize initialize
					      :compile-if-new compile-if-new
					      :load-patches load-patches
					      :force force))
	    (let ((system-file (get-system-master-file system)))
	      (if (probe-file system-file)
		  (load system-file)
		  (loop for file-spec in (system-files system)
			do (load-system-file file-spec system))))
	    (setf (system-state system) :loaded))

	  (when (and patchable (null patch-system))
	    (make-patch-system system))

	  (when (and patchable load-patches
		     (not load-from-scratch)) ; never load patches if system is loaded from scratch
	    (load-system-patches system))
      
	  (when (and initialize
		     (not (eq (system-state system) :initialized))
		     )
	    (initialize-system system))
	  system))))

(defun strip-trailing-slash (path)
  (loop for pos from (1- (length path)) downto 0
        while (eql (aref path pos) #\/)
        finally (return (subseq path 0 (1+ pos)))))

#+never
(defmethod make-patch-system ((system simple-lisp-system))
  (with-slots (name default-pathname patchable default-package patch-system) system
    (setq patch-system
	  #+never
	  (make-patchable-system-version
	   nil
	   :system-name name
	   ;:version name
	   :version nil
	   :default-package (or default-package "USER")
	   :version-directory-pathname
	   (strip-trailing-slash
	    (namestring (ev-pathname-backtranslate
			 (make-pathname :defaults default-pathname
					:type nil :name nil)))))
	  #-never
	  nil
	  )
    ;;(add-patch-system-for-cme-load-patches patch-system)
    patch-system))

;;; Ugh, Common Lisp union doesn't guarantee the order when there are duplicates.
;;; This version of union guarantees that the position of the FIRST occurance
;;; of a duplicate is preserved.
(defun union-eq-preserve-first (&rest lists)
  (let (l
	(ht (make-hash-table)))
    (loop for list in lists
	  do (loop for x in list
		   unless (gethash x ht)
		     do (setf (gethash x ht) t)
			(push x l)))
    (nreverse l)))

(defmethod system-precedence-list ((system simple-lisp-system) &optional current-systems)
  (union-eq-preserve-first
   (unless (member system current-systems)
     (loop for req in (system-required-systems system)
	   for rsys = (find-system-named req)
	   append
	   (prog1
	       (system-precedence-list rsys current-systems)
	     (pushnew rsys current-systems)
	     #+never
	     (unless (member rsys current-systems)
	       (setf current-systems
		     (nconc current-systems (list rsys))))
	     )))
   current-systems))

(defmethod list-system ((system simple-lisp-system) &key (list-required-systems t) binaries)
  (let ((default (if binaries
		     (system-default-binary-pathname system)
		     (system-default-pathname system))))
    (apply 'union-eq-preserve-first
	   (append ;; must collect required systems first:
	    (and list-required-systems
		 (loop for required-system in (system-required-systems system)
		       collect (list-system required-system
				    :list-required-systems list-required-systems)))
	    (list
	     (loop for file in (system-files system)
		   unless (consp file)
		     collect (merge-pathnames file default)))))))

(defmethod list-system ((system t) &key (list-required-systems t) binaries)
  (list-system (find-system-named system)
	       :list-required-systems list-required-systems
	       :binaries binaries))
	
(defmethod initialize-system ((system simple-lisp-system))
  (when (system-state-eq system :loaded)
    (unless (eq (system-state-eq system :loaded) :initializing)
      (setf (system-state system) :initializing)
      (loop for form in (system-initializations system)
	    do (eval form))
      (setf (system-state system) :initialized)
      system)))
	
(defmethod distribute-system ((system simple-lisp-system))
  ())

;;; These version of the of the operations take strings as their arguments.

(defun split-string (string char &optional (unique-p nil))
  (let ((c-pos (position char string))
	(c-pos-back (when unique-p (position char string :from-end t))))
    (when (and unique-p (not (equal c-pos c-pos-back)))
      (error "Badly formed string: ~s" string))
    (if c-pos
	(values (subseq string 0 c-pos) (subseq string (1+ c-pos)) c-pos)
	(values string "" c-pos))))

(defun parse-unix-search-path (pathstring &optional (separator-char #\:))
  (loop for (first-thing rest) = (multiple-value-list (split-string pathstring separator-char))
	then (multiple-value-list (split-string rest separator-char))
	when first-thing
	  collect first-thing
       	until (zerop (length rest))))

;(get-sysdef-paths)
(defun get-sysdef-paths ()
  (or *sysdef-search-paths*
      *sysdef-paths-override*
      ;; Why do we repeatedly compute this rather than doing it only once?
      ;; We must be careful when restarting a disksave with the FREEDIUS directory hierarchy moved.
      (let ((env (getenv *sysdef-path-environment-variable*)))
	(if env
	    (parse-unix-search-path env #+mswindows #\; )
	    `(,(merge-freedius-path "lisp/sysdefs")
	       .,(and config::freedius-systems-path
		      `(,(merge-f3dsys-path "sysdefs"))))))))

;;; Why do we require sysdef files to have file-type = sysdef, why not lisp, or
;;; possibly a list of file-types?
(defun find-sysdef-pathname (system-name &key (file-type "sysdef")
					 (sysdef-paths (get-sysdef-paths)))
  (setq system-name (string-downcase system-name))
  (loop for directory in sysdef-paths
	;; FIXME: Unix specific pathname merging
	for sysdef-file = (pathname (format nil "~a/~a.~a" directory system-name file-type))
	when (probe-file sysdef-file)
	  do (return sysdef-file)))

;;;
;;; Modified to "fake" system loading.  Within ASDF, this code is
;;; superfluous:
;;;
(defun find-system-named (name &key
			       (defined-only nil)
			       (file-type "sysdef")
			       (sysdef-paths (get-sysdef-paths))
			       (error-ok nil))
  (when (symbolp name) (setq name (string name)))
  (or (find-defined-system name)
      (and (not defined-only)
	   (let ((sysdef-file (find-sysdef-pathname name
						    :sysdef-paths sysdef-paths
						    :file-type file-type)))
	     (if sysdef-file
		 (progn (load sysdef-file)
			(or (find-defined-system name)
			    (unless error-ok
			      (error "Loading ~a did not define a system named ~a"
				     sysdef-file name))))
	    
		 (unless error-ok (error "Cannot find a sysdef file for the system named ~a" name)))))))

(defun get-system-path-prefix (system-name)
  (let* ((system (find-system-named system-name :error-ok t))
	 (path (and system (system-default-pathname system))))
    (and path (make-pathname :defaults path :type nil :name nil))))


(defun handle-relative-path (path &optional (relative-to (load-pathname)))
  (unless (pathnamep path) (setq path (pathname path)))
  (let ((new-path (if (and (not (ev-pathname-p path))
			   (eq (car (pathname-directory path)) :relative))
		      (let ((relative-to (or relative-to (load-pathname))))
			(make-pathname
			 :directory (append (pathname-directory relative-to)
					    (cdr (pathname-directory path)))
			 :defaults relative-to))
		      path)))
    ;;(setq foo new-path) (break)
    new-path))

#|
(handle-relative-path "$GEOFEX/lisp/lisp/"
		      "/homedir/quam/cp/lisp/lisp/sysdef-ffi-extensions.lisp")
(ev-pathname-p "$GEOFEX/lisp/lisp/")
|#


(defparameter *default-system-class* 'simple-lisp-system)


(defun define-system (name &rest rest &key
		      libraries
		      (system-class (if libraries 'mixed-lisp-system *default-system-class*))
		      (default-pathname (load-pathname)) ; this doesn't work with ilisp.
		      &allow-other-keys)
  ;; This will allow us to do concatenate-system:
  (when (fboundp 'define-shadow-system)
    (define-shadow-system
      name
      default-pathname
    rest))
    
  ;;; Special hack to automatically load qffi if needed.
  (when (and (eq system-class 'mixed-lisp-system)
	     (not (find-class system-class nil)))
    (load-system :qffi))

  (apply #'make-instance system-class
	 :name name
	 :default-pathname (handle-relative-path default-pathname )
	 rest))

(defmethod load-system (system-name &rest args &key initialize load-patches
				    &allow-other-keys)
  (declare (ignore initialize load-patches))
  (apply 'load-system (find-system-named system-name) args ))

(defmethod compile-system (system-name)
  (apply 'compile-system (list (find-system-named system-name))))

(defmethod initialize-system (system-name)
  (initialize-system (find-system-named system-name)))

(defmethod add-system-initialization (system-name form &optional when)
  (add-system-initialization (find-system-named system-name) form when))


;;(defvar *system-initializations* nil)

#+never
(defmethod add-system-initialization :before ((system-name symbol) form &optional when)
  (pushnew (list system-name form when) *system-initializations*))

;;; In this version we simply redefine the method.  Appropriate when
;;; ST is not used (in favor of ASDF):

#||
;;;
;;; These are now defined in :cl-cmake...
#+asdf
(defmethod add-system-initialization :before  ((system-name symbol) form &optional when)
  (unless (find system-name *system-initializations* :key 'car)
    (push (list system-name) *system-initializations*)))

#+asdf
(defmethod add-system-initialization  ((system-name symbol) form &optional when)
  (let ((sys-init-list (find system-name *system-initializations* :key 'car)))
    (unless sys-init-list (error "Can't find system ~a." system-name))
    (rplacd sys-init-list
	    (cons form (cdr sys-init-list)))))
||#

(defmethod load-system-patches (system-name)
  (load-system-patches (find-system-named system-name)))

(defmethod load-system-patches ((system simple-lisp-system ))
  (with-slots (patch-system) system
    (when patch-system
      ;;(LOAD-SYSTEM-PATCHES patch-system :verbose t)
      (LOAD-SYSTEM-PATCHES patch-system)
      ;;; This is a special hack which could be generalized to other properties.
      #+never
      (when (get-prop patch-system :quam-patch-system)
	(LOAD-SYSTEM-PATCHES (get-prop patch-system :quam-patch-system) :verbose t))
      )))



