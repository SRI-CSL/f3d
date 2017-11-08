#+sbcl
(in-package :common-lisp)

;;;
;;; The most successful so far:
#+sbcl
(sb-impl::unlock-package *package*)
#+sbcl
(defun ignore (&rest args) (declare (ignore args)))
#+sbcl
(sb-impl::lock-package *package*)

(in-package :system-tool)

;;; BE VERY CAREFUL WITH THIS FILE.  IT IS LOADED, THEN MAYBE-COMPILED AND LOADED.

;;; FIXME -- DOESN'T REALLY BELONG HERE.
;;; Should put this in a subsystem known to $FREEDIUS/lisp/sysdefs/

;;(defvar config::freedius-systems-path nil)
(declaim (special config::freedius-systems-path))

(defun merge-f3dsys-path (system-rel-path)
  (format nil "~a/~a" config::freedius-systems-path system-rel-path))


;;; end FIXME -- DOESN'T REALLY BELONG HERE.

;;; ************************  CONFIGURATION PARAMETERS  ************************

(defparameter *object-file-pathname-location* :PARALLEL-BINDIRS)

(defparameter *maybe-compile-file-load-allow-missing-source-files* :warn)

;;; This is actually set in boot.lisp
(defvar *source-fasl-lib-hierarchy-alist* nil)

;;; ************************  TEMPORARY DEFINITIONS  ************************

;;; These are redefined in ev-pathnames.lisp

(defun ev-pathname-translate (pathname)
  pathname)

;;; simplified version for bootstrap.  
(defun ev-pathname-translate (pathname &optional (error-if-no-translation t))
  (if (ev-pathname-p pathname)
      (let* ((path (pathname pathname))
	     (dir-list (pathname-directory path))
	     (env-var (if (cadr dir-list)
			  (subseq (cadr dir-list) 1)
			  (subseq (pathname-name path) 1))) ; strip off leading $
	     (ev-value (getenv env-var)))
	;;(format t "~a~%" (format nil "~a~a" expansion (subseq (namestring path) (1+ (length env-var)))))
	(if ev-value
	    (format nil "~a~a"
		    ev-value (subseq (namestring path) (1+ (length env-var))))
	    ;; otherwise
	    (when error-if-no-translation
	      (error "The environment variable ~a is not defined." env-var))))
      pathname))

(defun ev-pathname-backtranslate
    (pathname &key backtranslation-alist relative-to fail-action)
  (declare (ignore backtranslation-alist relative-to fail-action))
  pathname)

;;; CMUCL type-inference is overly agressive here.
;;; It appears that even though EV-PATHNAME-P is not inlined, the NIL result is inferred.
;;; This screws things up in HANDLE-RELATIVE-PATH defined in system-tool.lisp,
;;; when EV-PATHNAME-P is redefined in ev-pathnames.lisp
#+cmu
(defvar *cmucl-compiler-bug-1-workaround* nil)

#+old
(defun ev-pathname-p (pathname)
  (declare (ignore pathname))
  #+cmu *cmucl-compiler-bug-1-workaround*
  #-cmu nil)

(defun ev-pathname-p (pathname)
  (position #\$ (namestring pathname)))

(defun ev-to-logical-pathname (path)
  path)


;;; ************************  MISC DEFINITIONS THAT SHOULD BE IN LISP  ************************
;;;  move to another file ?

;;; FREEDIUS makes considerable use of this function.
;;; The IGNORE function could probably be moved to another file.
;;; IGNORE is in the xxx package, so WITHOUT-PACKAGE-LOCKS is required.
#-sbcl
(without-package-locks

(defun ignore (&rest args) (declare (ignore args)))

)

#+sbcl0
(eval-when (load)
  (let ((*package* (find-package "CL-USER"))
	(sb-impl::*ignored-package-locks* t))
    (without-package-locks
      (defun cl-user::ignore (&rest args) (declare (ignore args))))
    ))


#+sbcl1
(eval-when (load)
  (let ((*package* (find-package "COMMON-LISP"))
	(sb-impl::*ignored-package-locks* t))
    (without-package-locks
      (defun common-lisp::ignore (&rest args) (declare (ignore args))))
    ))


#+cmu
(defmacro allow-redefinition (&body body)
  `(progn .,body))

#+sbcl
(defmacro allow-redefinition (&body body)
  `(progn .,body))

#+allegro
(defmacro allow-redefinition (&body body)
  `(let ((%old-redefinition-action% EXCL::*redefinition-warnings*))
    (unwind-protect
	 (progn (setq EXCL::*redefinition-warnings* nil)
		. ,body)
      ;; cleanup form
      (setq EXCL::*redefinition-warnings* %old-redefinition-action% ))))

(defun pwd ()
  #+allegro (excl:current-directory)
  #+cmu (ext:default-directory)
  #+sbcl (sb-unix:posix-getcwd/)
  )

#+sbcl
(eval-when (eval load compile)
  (require :sb-posix)
)

#+sbcl
(in-package :sb-posix)
#+sbcl
(define-call* "chdir" int minusp (pathname filename))

(in-package :system-tool)

(defun cd (&optional directory)
  #+cmu
  (setf (ext:default-directory) directory)
  #+sbcl
  (if directory
      (sb-posix::chdir (namestring directory))
      nil)

  #+allegro
  (if directory
      (excl:chdir directory)
    (excl:chdir)))

;;; UNWIND-PROTECT-CASE is used later in this file
(defmacro unwind-protect-case ((&optional aborted-p-var) body-form
			       &rest cleanup-clauses)
  (if (null aborted-p-var) (setq aborted-p-var (gensym)))
  `(let ((,aborted-p-var t))
    (unwind-protect
	 (multiple-value-prog1 ,body-form (setq ,aborted-p-var nil))
      ,@(loop for (cleanup-condition . forms) in cleanup-clauses
	      collect
	      (case cleanup-condition
		(:normal `(when (not ,aborted-p-var) ,@forms))
		(:abort `(when ,aborted-p-var ,@forms))
		(:always `(progn ,@forms))
		(otherwise (error "~s is not a valid cleanup-condition condition,~@
					     expected one of :normal, :abort, :always"
				  cleanup-condition)))))))

;;; *****************************  SYSTEM TOOL BOOTSTRAP  *****************************

(in-package :system-tool)

(export 'file-property-list)
;;(setq file-property-list t)

;;; Change the X86 Allegro fasl type.  This is only important if fasl files of
;;; multiple architectures reside in the same directory.

;;#+(and allegro x86) (setq excl:*fasl-default-type* "faslx86")
;;#+(and :sbcl :x86-64) (setq sb-fasl:*fasl-file-type* "fasl64")

(defparameter *lisp-compiled-file-type*
  #+allegro excl:*fasl-default-type*
  #+sbcl sb-fasl:*fasl-file-type*
  #+cmu (car ext:*load-object-types*)
)

(defun load-source-pathname-types ()
  #+cmu extensions:*load-source-types*
  #+sbcl (list sb-int:*load-source-default-type*)
  #+allegro '("lisp" "cl" nil))

(defun relative-pathname-p (path)
  (unless (and (ev-pathname-p path) (eql (aref (namestring path) 0) #\$)) 
    ;; assume namestrings with leading $ are absolute. 
      (let ((dirs (pathname-directory path)))
	(or (null dirs) (eq (car dirs) :relative))
	)))

#+old ;; Sun Jun 21 2009
(defun pathname-inside-hierarchy-p (path hierarchy-path)
  (let ((dirs (pathname-directory path))
	(hier-dirs (pathname-directory hierarchy-path)))
    (and (>= (length dirs) (length hier-dirs))
	 (loop for d1 in hier-dirs
	       for d2 in dirs
	       always (equal d1 d2)))))

;;; added probe-file to deal with /../ inside paths.
;;; Changed Mon Jun 22 2009 to return the relative-path below hierarchy-path.
(defun pathname-inside-hierarchy-p (path hierarchy-path)
  (when (and (setq hierarchy-path (probe-file (ev-pathname-translate hierarchy-path)))
	     (setq path (probe-file (ev-pathname-translate path)))
	     (equal (pathname-host path)
		    (pathname-host hierarchy-path)))
    (let ((dirs (pathname-directory path))
	  (hier-dirs (pathname-directory hierarchy-path)))
      (and (>= (length dirs) (length hier-dirs))
	   (loop for d1 in hier-dirs
		 for d2 = (pop dirs)
		 always (equal d1 d2))
	   (make-pathname :defaults path :directory (list* :relative dirs))))))

#|
(pathname-inside-hierarchy-p "$FREEDIUS/lisp/lisp/lisp-io.lisp" "$FREEDIUS/")
(pathname "$FREEDIUS/lisp/lisp/lisp-io.lisp")
;;; enough-namestring doesn't work with relative pathnames
(enough-namestring "foo/bar/baz/a.b" "foo/bar/")
(enough-namestring "$FREEDIUS/lisp/lisp/lisp-io.lisp" "$FREEDIUS/") ; fails
(sb-impl::%pathname-host (pathname "$FREEDIUS/lisp/lisp/lisp-io.lisp"))
;; this works
(enough-namestring (ev-pathname-translate "$FREEDIUS/lisp/lisp/lisp-io.lisp") 
		   (ev-pathname-translate "$FREEDIUS/"))
(pathname-inside-hierarchy-p (ev-pathname-translate "$FREEDIUS/lisp/lisp/lisp-io.lisp")
			     (ev-pathname-translate "$FREEDIUS/"))
(pathname-inside-hierarchy-p (ev-pathname-translate "$F3DSYS/proprietary/aerovironment/sysdef-aerovironment.lisp")
			     (ev-pathname-translate "$F3DSYS/"))
(pathname-inside-hierarchy-p "/opt/IU/FREEDIUS/freedius-systems-svn/quam/vivid/code2/vivid-viewer.lisp"
			     (ev-pathname-translate "$F3DSYS/"))

(compile-object-file-pathname "/opt/IU/FREEDIUS/freedius-systems-svn/quam/vivid/code2/vivid-viewer.lisp")

;(find-source-fasl-hierarcies (ev-pathname-translate "$FREEDIUS/lisp/lisp/lisp-io.lisp"))
;(find-source-fasl-hierarcies "$FREEDIUS/lisp/lisp/lisp-io.lisp")
;(compile-object-file-pathname (ev-pathname-translate "$FREEDIUS/lisp/lisp/lisp-io.lisp"))
(compile-object-file-pathname (merge-freedius-path "lisp/lisp/lisp-io.lisp"))
(pathname-inside-hierarchy-p (merge-freedius-path "lisp/lisp/lisp-io.lisp") #p"$FREEDIUS/")
(ev-pathname-translate #p"$FREEDIUS/")
(probe-file (merge-freedius-path "lisp/lisp/lisp-io.lisp"))
(probe-file "$FREEDIUS/")
;(compile-object-file-pathname (ev-pathname-translate "$F3DSYS/proprietary/aerovironment/sysdef-aerovironment.lisp"))
;;(probe-file "/opt/IU/freedius-systems/arch/linux-sbcl64/fasl")
(find-source-fasl-hierarcies (merge-system-source-pathname "libfreedius_avapi.so" (find-system-named "aerovironment"))

(relocate-system-path "$FREEDIUS/lisp/lisp/lisp-io.lisp" "lisp/lisp-io.lisp"  "$FREEDIUS/arch/linux-sbcl64/fasl/")
(relocate-system-path "$FREEDIUS/lisp/lisp/lisp-io.lisp" "lisp/lisp-io.lisp" #P"FREEDIUS:LISP;")
(relocate-system-path (ev-pathname-translate "$FREEDIUS/lisp/lisp/lisp-io.lisp")
		      "lisp/lisp/" 
		      (ev-pathname-translate "$FREEDIUS/arch/linux-sbcl64/fasl/"))
(relative-pathname-p "$FREEDIUS/arch/linux-sbcl64/fasl/")
(ev-pathname-p "$FREEDIUS/arch/linux-sbcl64/fasl/") 
(aref (namestring "$FREEDIUS/arch/linux-sbcl64/fasl/") 0)
(pathname-inside-hierarchy-p (cmake-script-path (find-system-named  :tk)) "$FREEDIUS/lisp/")
(symbol-package (type-of (pathname "FREEDIUS:")))
(typep (pathname "FREEDIUS:") 'logical-pathname)

(find-source-fasl-hierarcies (ev-pathname-translate "$FREEDIUS/c/lisptk/CMakeLists.txt"))
(relocate-system-path "liblisptk.so" #P"lisptk/CMakeLists.txt" '(:FLAT #P"FREEDIUS:ARCH;LIB-FLAT;"))
(relocate-system-path "liblisptk.so" #P"lisptk/CMakeLists.txt" #P"FREEDIUS:ARCH;LIB-FLAT;")
(multiple-value-bind (rel-path lisp-hier fasl-hier lib-hier)
    (find-source-fasl-hierarcies (ev-pathname-translate "$FREEDIUS/c/lisptk/CMakeLists.txt"))
  (setq *foo* lib-hier)
  (relocate-system-path "liblisptk.so" rel-path  (cadr lib-hier)))
(describe (cadr *foo*))


|#

;;; Search thru *source-fasl-lib-hierarchy-alist* looking for source-hierarchy
;;; which contains pathname and is "most-specific" in the sense of begin the deepest
;;; (longest) path.
(defun find-source-fasl-hierarcies (pathname)
  ;; find the source-hierarchy maximally matching pathname
  (loop for entry in (or (and (boundp 'config::freedius-pathname-translations )
			      config::freedius-pathname-translations)
			 *source-fasl-lib-hierarchy-alist*)
	for source-hier = (car entry)
	with best-source-hier
	with best-entry
	with best-score = -1
	for rel-path = (pathname-inside-hierarchy-p pathname source-hier)
	when (and rel-path
		  (> (length source-hier) best-score))
	  do (setq best-score (length source-hier) ; FIXME assumes namestring
		   best-entry (cons rel-path (cdr entry)))
	finally (return (values-list best-entry))))

;;; Tue Jun 23 2009 - additional syntax for to-path  allow (:FLAT <lib-path>)
;;;                   to specify lib directly under <lib-path> rather than heierarchy
;;;; rel-path is part of path beyond the path-prefix for the source-heirarchy
(defun relocate-system-path (path rel-path to-path)
  (if (consp to-path)
      (case (car to-path)
	(:FLAT (make-pathname :defaults path
			      :host (pathname-host (cadr to-path))
			      :directory (pathname-directory (cadr to-path))))
	(otherwise (error "system path - bad to-path spec:~a" to-path)))
      (make-pathname :defaults path
		     :host (pathname-host to-path)
		     :directory (if (relative-pathname-p to-path)
				    ;; fasl located within the directory heirarchy of the system.
				    (append (pathname-directory path)
					    (cdr (pathname-directory to-path)))
				    ;; fasl located at an absolute location
				    (append (pathname-directory to-path)
					    (cdr (pathname-directory rel-path)))))))


;;; debugging version
;; #+quam
(defun relocate-system-path (path rel-path to-path)
  (let ((result
	 (if (consp to-path)
	     (case (car to-path)
	       (:FLAT (make-pathname :defaults path
				     :host (pathname-host (cadr to-path))
				     :directory (pathname-directory (cadr to-path))))
	       (otherwise (error "system path - bad to-path spec:~a" to-path)))
	     (make-pathname :defaults path
			    :host (pathname-host to-path)
			    :directory (if (relative-pathname-p to-path)
					   ;; located within the directory heirarchy of the system.
					   (append (pathname-directory path)
						   (cdr (pathname-directory to-path)))
					   ;; located at an absolute location
					   (append (pathname-directory to-path)
						   (cdr (pathname-directory rel-path))))))))
    ;;(setq *foo* (list result path rel-path to-path))
    (when (position #\$  (namestring result) :start 1)
      (error "relocate-system-path  failure"))
  result))
#|
(ev-pathname-p "$FREEDIUS/arch/linux-sbcl64/lisp/")
(relative-pathname-p "$FREEDIUS/arch/linux-sbcl64/lisp/")
(aref (namestring "$FREEDIUS/arch/linux-sbcl64/lisp/") 0)
|#
	

;;; Determine the pathname for the compiled (fasl) file.
#+never ;; does not return multiple-values
(defun compile-object-file-pathname (source-pathname)
  (or (let* ((source-pathname (ev-pathname-translate source-pathname)))
	(when (relative-pathname-p source-pathname)
	  ;; FIXME:  the call to (pwd) is suspicious
	  (setq source-pathname (format nil "~a~a" (pwd) source-pathname))
	  (format nil ";;; compile-object-file-pathname received a relative pathname ~a~%" source-pathname))
	(multiple-value-bind (rel-path lisp-hier fasl-hier)
	    (find-source-fasl-hierarcies source-pathname)
	  ;;(declare (ignore lisp-hier))
	  (when rel-path
	    (return-from compile-object-file-pathname
	      (values (relocate-system-path
		       (make-pathname :defaults source-pathname :type *lisp-compiled-file-type*)
		       rel-path fasl-hier)
		      (relocate-system-path source-pathname rel-path lisp-hier)
		      )))))
		   
      ;; If source/fasl hierarchies are not found:

      (and (eq *object-file-pathname-location* :BIN-SUBDIRS)
	   ;; THIS PLACES THE OBJECT FILE IN A SUBDIRECTORY OF THE DIRECTORY
	   ;; CONTAINING THE SOURCE FILE.
	   (make-pathname :defaults source-pathname :type *lisp-compiled-file-type*
			  :directory (append (pathname-directory source-pathname) 
					     (list (string-upcase *lisp-compiled-file-type*)))
			  ))

      ;; THIS PLACES OBJECT FILE IN SAME DIRECTORY AS SOURCE FILE
      (make-pathname :defaults source-pathname :type *lisp-compiled-file-type*)))


;;; Hacked Sun Jun 28 2009 to return possibly relocated source pathname too.  ugly semantics
(defun compile-object-file-pathname (source-pathname)
  (let* (new-source
	 (obj-path
	  (or (let* ((source-pathname (ev-pathname-translate source-pathname)))
		(when (relative-pathname-p source-pathname)
		  ;; FIXME:  the call to (pwd) is suspicious
		  (setq source-pathname (format nil "~a~a" (pwd) source-pathname))
		  (format nil ";;; compile-object-file-pathname received a relative pathname ~a~%" source-pathname))
		(multiple-value-bind (rel-path lisp-hier fasl-hier)
		    (find-source-fasl-hierarcies source-pathname)
		  ;;(declare (ignore lisp-hier))
		  (when rel-path
		    (setq new-source (relocate-system-path source-pathname rel-path lisp-hier))
		    (relocate-system-path
		     (make-pathname :defaults source-pathname :type *lisp-compiled-file-type*)
		     rel-path fasl-hier))))
		   
	      ;; If source/fasl hierarchies are not found:

	      (and (eq *object-file-pathname-location* :BIN-SUBDIRS)
		   ;; THIS PLACES THE OBJECT FILE IN A SUBDIRECTORY OF THE DIRECTORY
		   ;; CONTAINING THE SOURCE FILE.
		   (make-pathname :defaults source-pathname :type *lisp-compiled-file-type*
				  :directory (append (pathname-directory source-pathname) 
						     (list (string-upcase *lisp-compiled-file-type*)))
				  ))

	      ;; THIS PLACES OBJECT FILE IN SAME DIRECTORY AS SOURCE FILE
	      (make-pathname :defaults source-pathname :type *lisp-compiled-file-type*))))
    (values obj-path new-source)))
    



(defvar *force-recompilation* nil)
(defvar *compile-if-new* t)

;;; Not sure this is needed
(defvar *load-path-times-hash-table* (make-hash-table :test #'equal))

;;; This is redefined by optimization-profiles.lisp
(defun config::proclaim-optimizations (&optional profile))

#|
(ev-pathname-p (pathname "FREEDIUS:LISP;IMG;IMAGE-DEFS.LISP.NEWEST"))
(ev-pathname-translate (pathname "FREEDIUS:LISP;IMG;IMAGE-DEFS.LISP.NEWEST"))
(describe (pathname "FREEDIUS:LISP;IMG;IMAGE-DEFS.LISP.NEWEST"))
(sb-c::verify-source-file "FREEDIUS:LISP;IMG;IMAGE-DEFS.LISP.NEWEST")
;(untrace)
;(trace relocate-system-path)
;(trace compile-object-file-pathname)
;(maybe-compile-file-load "$FREEDIUS/lisp/img/image-defs.lisp")
;(truename #P"FREEDIUS:LISP;IMG;IMAGE-DEFS.LISP.NEWEST")
;(compile-object-file-pathname "$FREEDIUS/lisp/img/image-defs.lisp")
(compile-object-file-pathname "/opt/IU/FREEDIUS/FREEDIUS-cmake-20090619/lisp/system-tool/system-tool-bootstrap.lisp")
#P"$FREEDIUS/arch/linux-sbcl64/lisp/system-tool/system-tool-bootstrap.fasl64"
#P"$FREEDIUS/lisp/system-tool/system-tool-bootstrap.lisp"
(ev-pathname-translate #P"$FREEDIUS/arch/linux-sbcl64/lisp/system-tool/system-tool-bootstrap.fasl64")
(ev-pathname-translate #P"$FREEDIUS/lisp/system-tool/system-tool-bootstrap.lisp")
(find-source-fasl-hierarcies "/opt/IU/FREEDIUS/FREEDIUS-cmake-20090619/lisp/system-tool/system-tool-bootstrap.lisp")

(st::relative-pathname-p "$FREEDIUS/lisp/img/image-defs.lisp")

Bug in sb-c::verify-source-file in src/compiler/main.lisp
(sb-c::verify-source-file "FREEDIUS:LISP;IMG;IMAGE-DEFS.LISP.NEWEST")
(sb-c::verify-source-file "FREEDIUS:LISP;IMG;IMAGE-DEFS.LISP")

(namestring (make-pathname :host (pathname-host "FREEDIUS:LISP;IMG;IMAGE-DEFS.LISP")))
(describe (make-pathname :host (pathname-host "FREEDIUS:LISP;IMG;IMAGE-DEFS.LISP")))
(describe (make-pathname :host (pathname-host "FREEDIUS:LISP;IMG;IMAGE-DEFS.LISP")
			 :name :unspecific :type :unspecific))

(namestring (make-pathname :type "LISP" 
			   :defaults (make-pathname :host (pathname-host "FREEDIUS:LISP;IMG;IMAGE-DEFS.LISP"))))

(compile-object-file-pathname "/opt/IU/FREEDIUS/freedius-systems-svn/quam/vivid/code2/vivid-viewer.lisp")

|#

(defun maybe-compile-file-load
    (source-pathnames
     &key
     (force-compile *force-recompilation*)
     (compile-if-new *compile-if-new*)
     force-reload
     pathname-defaults
     starting-with (warnings-stream t))
  (unless (consp source-pathnames) (setq source-pathnames (list source-pathnames)))
  (when starting-with 
    (setq source-pathnames 
	  (loop for (pathname . rest) on source-pathnames by #'cdr
		when (equal pathname starting-with)
		  return (cons pathname rest)
		finally (error "cannot find starting pathname"))))

  (labels ((load-if-newer (path)
	     ;; The pathname that is cached and whose file-write-date is checked
	     ;; is the binary file rather than the source file.
	     (let ((last-write-time
		    (gethash (namestring path) *load-path-times-hash-table*))
		   (current-write-time (and (probe-file path) (file-write-date path))))
	       (when (or force-reload
			 (null last-write-time)
			 (< last-write-time current-write-time))
		 #+sbcl (format t "~%; Loading ~a~%" path) ;
		 
		 (prog1 #+sbcl (handler-bind ((sb-kernel::implicit-generic-function-warning 
					       #'sb-kernel::muffle-warning))
				 (load path))
			#-sbcl (load path)
		   (setf (gethash (namestring path) *load-path-times-hash-table*)
			 current-write-time)))))
	   
	   (maybe-compile-file-load-internal
	       (source-pathname pathname-defaults warnings-stream)
	     ;;(setq *foo0* source-pathname)
	     (when pathname-defaults
	       (setq source-pathname 
		     (merge-pathnames source-pathname pathname-defaults)))
	     (unless (probe-file source-pathname)
	       (cond ((eq *maybe-compile-file-load-allow-missing-source-files* :warn)
		      (warn "; SYSTEM-TOOL Cannot find source file ~a~%" source-pathname))
		     ((not *maybe-compile-file-load-allow-missing-source-files*)
		      (error "Cannot find source file ~a~%" source-pathname))))
	     (let* ((object-pathname
		     (multiple-value-bind (obj-path lisp-path)
			 (compile-object-file-pathname source-pathname)
		       (when lisp-path 
			 (setq source-pathname lisp-path))
		       obj-path))
		    (must-compile (or force-compile
				      (null (probe-file object-pathname))
				      (and (probe-file source-pathname)
					   compile-if-new
					   (< (file-write-date object-pathname)
					      (file-write-date source-pathname)))
				      ;; Sometimes we see zero length files
				      ;; if compilation is aborted.
				      (with-open-file (st object-pathname)
					(zerop (file-length st))))))
	       ;;(setq *foo* (list source-pathname object-pathname  must-compile))
	       (when must-compile
		 (when (probe-file object-pathname)
		   ;; DELETE-FILE was added because of uname problems.
		   ;; If the object file exists, written by a user with a "bad" uname
		   ;; we can DELETE the file, but not OVERWRITE it.  UNIX SUCKS.
		   (delete-file object-pathname))
		 (unwind-protect-case
		     ()
		     (progn
		       ;; For Allegro, we want a logical pathname in order for
		       ;; meta-dot to work right when we are running a DISKSAVE AND
		       ;; the source file hierarchy has been moved.
		       (setq source-pathname (ev-to-logical-pathname source-pathname))
		       ;;; These next are needed until full ev-pathnames.lisp is loaded
		       ;; FIXME:  this next sucks  - needed during bootstrap phase
		       (unless (let ((evp (find-package :ev-pathnames)))
				 (and evp (boundp (intern "SUBSTITUTE-IN-FILE-NAME" evp))))
			 (setq source-pathname (ev-pathname-translate source-pathname))
			 (setq object-pathname (ev-pathname-translate object-pathname)))
		       (ensure-directories-exist object-pathname)
		       ;; Make sure the global optimization profile exists.
		       ;; This allows a file to change the optimization settings without
		       ;; affecting other files.
		       (config::proclaim-optimizations) 
		       ;; #+cmu (describe  C::*DEFAULT-COOKIE*)
		       #-allegro (format t "~%; Compiling ~a~%" source-pathname)
		       (setq *foo* (list source-pathname object-pathname pathname-defaults))
		       (if (eql warnings-stream  t)
			   (compile-file source-pathname :output-file object-pathname)
			   (compile-file source-pathname :output-file object-pathname
					 #+allegro :messages #+allegro warnings-stream))
		       
		       )
		   (:abort (when (probe-file object-pathname)
			     ;; Occasionally see zero-length object files.
			     ;; Hopefully this will fix that problem.
			     (delete-file object-pathname))))
		 )
	       (if (probe-file object-pathname)
		   (load-if-newer object-pathname)
		   (error "compilation-failed for ~a" source-pathname)))
	     ))
    (with-compilation-unit ()
      (loop for source-pathname in source-pathnames
	    do
	 (loop do
	   (restart-case
	       (progn (maybe-compile-file-load-internal
		       source-pathname  pathname-defaults warnings-stream)
		      (return))
	     (retry ()
	       :report (lambda (stream)
			 (format stream "Retry compile-file-load of ~a" source-pathname))
	       (continue))))))))
