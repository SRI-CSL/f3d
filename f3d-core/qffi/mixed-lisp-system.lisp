(in-package :system-tool)

;;; ***********************  SYSTEMS WITH SHARED LIBRARIES  ********************

#|
Sun Jun 21 2009  Changes for cmake and freedius-systems.

Terminology:

   SUBSYSTEM-LIBRARY:  A library built as part of a freedius or freedius-systems subsystem.
  
   DEPENDENT-LIBRARY:  A library referenced by a SUBSYSTEM-LIBRARY. 

   RESOLVED-DEPENDENT-LIBRARY:  A DEPENDENT-LIBRARY that is resolved as build time.
                                Ie. the full path to the DEPENDENT-LIBRARY is built in.

Here are some questions for you:

1.  Do you want to be able to build and install a SUBSYSTEM-LIBRARY and at a later
    time, possibly on the different machine, invoke the library under conditions where the
    DEPENDENT-LIBRARIES are in different place in the filesystem?

    I can imagine this occuring when a disksave is built and used from a different machine, or 
    installed at a different "site".

    If this is a goal, then the reference to the DEPENDENT-LIBRARY must not be built into the 
    SUBSYSTEM-LIBRARY, and the system-tool::mixed-lisp-system must load the DEPENDENT-LIBRARIES
    before loading the SUBSYSTEM-LIBRARY.   

    This is easy enough to accomplish, but I need to understand what you really want to do.

2.  On Mac OS X and winDoze, are the issues regarding inter-library references (ie. a SUBSYSTEM-LIBRARY 
    referencing a DEPENDENT-LIBRARY) similar to those for Linux?  Is is ok you build a library
    with unresolved references and having them resolved at run time?


Here is my current thinking for controlling the loading libraries by ST:DEFINE-SYSTEM:

A.  A minimalist approach requiring that every (ST:DEFINE-SYSTEM ...) explicitly 
    contain information about where fasl files are compiled and where libraries are to be found.

    For example:

    (define-system "aerovironment"
        :required-systems '(basic-video color mpeg2-video)
	:fasl-pathname (merge-f3dsys-arch-path "fasl/proprietary/aerovironment/")
        :libraries (list (merge-f3dsys-arch-path "lib/libfreedius_avapi"))
        :files '("mpeg2-av-ffi.lisp" "mpeg2-av-defs.lisp"))

B.  A more automatic approach that will require little or no modification to existing system definitions.

  1.  I have extended the functionality of st:*source-fasl-hierarchy-alist* by adding a new
      variable config::freedius-pathname-translations  See config.lisp.  The elements
      of the config are (source-path fasl-path lib-path).  
      This allows the control of where subsystem fasl files are compiled and subsystem libraries are 
      found.  See find-source-fasl-hierarcies in system-tool-bootstrap-lisp.

      If a source file matches a source-path in config::freedius-pathname-translations
      then fasl and library pathnames are constructed using the corresponding entries in the
      config::freedius-pathname-translations entry.

      For examples, see $FREEDIUS/lisp/config.lisp




To me option B is attractive and appears to be working.

I think that the mods to SHARED-LIBRARY-PATHNAME are backward compatible with previous usage.



|#
;
;; ****************************  MIXED-LISP-SYSTEM  ***************************

(eval-when (load compile)
(import '(qffi::require-shared-libraries
	  qffi::merge-shared-library-pathname-type ))
)

;;;;
;;;; mixed-lisp-system -- contains a mixture of lisp and foreign code.
;;;; Added this for some systems (gtk, for example) that require
;;;; additional foreign libraries (-CC 4.9.2002):
;;;;

;;;
;;; In the context of quicklisp, map these same functions onto the
;;; ASDF system classes and see if we can extend appropriately.
;;;

(defclass mixed-lisp-system (simple-lisp-system) ;(property-list-mixin)
    ((shared-libraries
      :reader system-shared-libraries
      :initarg :libraries
      :initform nil)
     (c-source-path :initform nil :initarg :c-source-path :reader system-c-source-path))
  )

(defmethod initialize-instance :after ((o mixed-lisp-system) &rest initargs)
  (with-slots (c-source-path) o
    (unless c-source-path
      (setq c-source-path (merge-system-source-pathname "" o )))))

(defmethod merge-system-c-source-pathname (pathname (system mixed-lisp-system))
  (merge-pathnames pathname (system-c-source-path system)))
 

;;; Sat Jun 20 2009 - allow (:search "libfoo") 
;;; Sun Jun 21 2009 - use find-source-fasl-hierarcies

; Fri Jun 26 2009 - returns 2nd value: :local if the libname at the library install path of the system.
(defmethod shared-library-pathname ((system mixed-lisp-system) libname)
  (cond ((null libname)
	 nil)
	((listp libname)
	 (if (eq (car libname) :search)
	     ;; Search for the library using config::*library-search-paths*  
	     (qffi::find-shared-library (merge-shared-library-pathname-type (cadr libname)))
	     (error "shared-library-pathname: bad library spec: ~a" libname)))
	((not (relative-pathname-p libname))
	 ;; absolute pathname -- use it with proper extension
	 (merge-shared-library-pathname-type libname))
	(t (let* ((libname+ext (merge-shared-library-pathname-type libname))
		  (local-libpath (merge-system-source-pathname libname+ext system)))
	     ;; 2 cases to consider - a library in the hierarchy of the system files
	     ;; or a system library with a relative pathname -- 
	     (if (probe-file local-libpath)
		 (values local-libpath :local) ;; the library is found in the top level of the source hierarchy  .
		 (multiple-value-bind (rel-path lisp-hier fasl-hier lib-hier)
		     (find-source-fasl-hierarcies (system-default-pathname system))
		   ;;(find-source-fasl-hierarcies local-libpath)
		   (declare (ignore lisp-hier fasl-hier))
		   (if (null lib-hier)
		       ;; source file doesn't match any of the known places in freedius-pathname-translations
		       ;; Try to load it with default loader.
		       libname+ext
		       (values (relocate-system-path local-libpath rel-path lib-hier) :local))))))))

#|
;(pathname-directory *foo1*)
(relative-pathname-p *foo1*)
(pathname-directory (caddr *foo1*))
(pathname-directory (cadr *foo1*))
;(system-default-pathname (find-system-named "aerovironment"))
;(shared-library-pathname (find-system-named "aerovironment") "libfreedius_avapi")
(shared-library-pathname (find-system-named :tk) "liblisptk")
;(merge-system-source-pathname "libfreedius_avapi.so" (find-system-named "aerovironment")) 
;(find-source-fasl-hierarcies (ev-pathname-translate "$F3DSYS/proprietary/aerovironment/sysdef-aerovironment.lisp"))
(find-system-named :uid)
(merge-system-source-pathname "uid-macros.lisp" (find-system-named :uid))
(system-default-pathname (find-system-named :uid))
(merge-pathnames "uid-macros.lisp" (system-default-pathname (find-system-named :uid)))
|#


#+old ;; prior to using cmake
(progn

;;;
;;; If a Makefile is detected, do a "make" in the current directory in
;;; case any C files were changed recently.  This is especially useful
;;; when the system directory has been updated with CVS or unison...

(defvar *make-before-loading-libraries* #+mswindows nil #-mswindows t)

#+allegro
(defun maybe-execute-make (system)
  ;; We always assume that the name is "Makefile".
  (when (and *make-before-loading-libraries*
	     (probe-file (make-pathname
			  :defaults (merge-system-source-pathname "Makefile" system)
			  :type nil)))
    (let ((pathname (namestring (make-pathname
				 :directory (pathname-directory (system-default-pathname system))))))
      #+allegro
      (excl::run-shell-command
       (format nil "cd ~a ; make"  pathname) ;
       :wait t)
      #+cmu
      (extensions:run-program
       "make" nil
       :before-execve (unix:unix-chdir (format nil "~a" pathname)))
      #+sbcl
      (sb-ext:run-program
       "make" nil
       :before-execve (sb-posix:chdir (format nil "~a" pathname)))
      )))

#+(or :cmu :sbcl) ;; FIXME:  make lisp=cmucl -- problems with multiple architectures
(defun maybe-execute-make (system)
  (declare (ignore system)))

) ;end progn ;; prior to using cmake

#+old
(defmethod maybe-execute-cmake ((system mixed-lisp-system))
  (loop do
    (restart-case 
	(let ((missing-libs 
	       (and (probe-file (cmake-script-path system))
		    (system-libraries-missing system :library-location-type :local))))
	  (when missing-libs 
	    (error "subsystem ~a libraries not found: ~a." 
		   (system-name system) missing-libs))
	  (return))
      (retry ()
	:report (lambda (stream)
		  (format stream "Run cmake for system ~a?" system))
	(cmake-compile-system-libraries system)
	(continue)))))


(defparameter *automatically-run-cmake* t)

;;;
;;; Windows note: For some reason, the Windows builds do not install
;;; their DLLs in the correct place.  We still need to manually copy
;;; the files after building...
;;;
(defmethod maybe-execute-cmake ((system mixed-lisp-system)
				&key (automatic-cmake *automatically-run-cmake*))
  (when (and (probe-file (cmake-script-path system))
	     (system-libraries-missing system :library-location-type :local))
    ;; automatically run cmake once
    (when automatic-cmake
      (cmake-compile-system-libraries system))

    (loop do
      (restart-case 
	  (let ((missing-libs 
		 (system-libraries-missing system :library-location-type :local)))
	    (when missing-libs 
	      (error "subsystem ~a libraries not found: ~a." 
		     (system-name system) missing-libs))
	    (return))
	(run-cmake ()
	  :report (lambda (stream)
		    (format stream "Run cmake for system ~a?" system))
	  (cmake-compile-system-libraries system)
	  (continue))
	(clear-build&run-cmake ()
	  :report (lambda (stream)
		    (format stream "Clear build directory and run cmake for system ~a?" system))
	  (cmake-clear-build-directory system)
	  (cmake-compile-system-libraries system)
	  (continue))))))


(defmethod load-system-shared-libraries ((system mixed-lisp-system))
  ;;(maybe-execute-make system)
  (maybe-execute-cmake system)

  (with-slots (shared-libraries) system
    (loop for libname in shared-libraries
	  for fullname = (shared-library-pathname system libname)
	  when fullname
	    do (require-shared-libraries fullname))))


(defmethod load-system-shared-libraries ((system t)) nil)

(defmethod load-system-shared-libraries ((system symbol))
  (load-system-shared-libraries (find-system-named system)))

;;; Libraries need to be loaded before the files of this system in order that 
;;; ffi definitions can be run.
;;; Libraries should be loaded after required-systems.
#+never
(defmethod load-system :before ((system mixed-lisp-system)  &rest args
				&key initialize recompile &allow-other-keys)
  (declare (ignore initialize recompile args))
  (load-system-shared-libraries system))

(defmethod check-for-required-systems :after ((system mixed-lisp-system)  &rest args)
  (load-system-shared-libraries system))

(defun load-systems (system-list)
  (loop for name in system-list do (load-system name)))


#||
#+unix
(defmethod concatenate-system ((system simple-lisp-system) &optional output-file recursive)
  (unless output-file
    (setq output-file (get-system-master-file system)))
  (loop for file in (system-files system)
	as i from 1
	for fullname = (merge-pathnames file (system-default-pathname system))
	do
     (format t "~%concatenating ~a into ~a" fullname output-file)
     (shell (format nil (if (= i 1)
			    "cat ~a >~a"
			    "cat ~a >>~a")
		    fullname output-file))))


#+unix
(defmethod concatenate-system ((sysname t) &optional output-file)
  (concatenate-system (find-system-named sysname) output-file))
||#

(defvar config::freedius-systems-path (getenv "F3DSYS"))

(defun f3dsys-arch-library-path(libname)
  (let* ((f3dsys-path config::freedius-systems-path))
    (if f3dsys-path
	(let* ((f3dsys-arch-lib (format nil "~a/arch/~a/lib" f3dsys-path *freedius-arch-name*)))
	  (format nil "~a/~a" f3dsys-arch-lib libname))
	libname)))





;;; New  Mon Jun 22 2009


(defvar *lisp-type*
  (or 
   #+cmu :cmucl
   #+allegro :allegro
   #+(and :sbcl (not :x86-64)) :sbcl32
   #+(and :sbcl :x86-64) :sbcl64
   (error "unknown Lisp type")))


(defvar *freedius-arch-name*
  (pathname-name (st::getenv "FREEDIUS_ARCH")))

;(fmakunbound 'system-libraries-missing)

(defmethod system-libraries-missing ((system mixed-lisp-system) 
				     &key (missing-action :warn) (library-location-type :all))
  (loop for libname in (system-shared-libraries system)
	for (libpath-sans-ext local-p) = (multiple-value-list (shared-library-pathname system libname))
	for libpath = (merge-shared-library-pathname-type libpath-sans-ext)
	when (and (null (probe-file libpath))
		  (case library-location-type
		    ((nil :all) t)
		    (:local local-p)
		    (:global (null local-p))))
	  do (when (eq missing-action :warn)
	       (warn "missing library ~a of system ~a" libpath (system-name system)))
	  and collect libpath
	))

(defmethod system-libraries-installed-p ((system mixed-lisp-system))
  (let ((missing (system-libraries-missing system)))
    (values (null missing)
	    missing)))

#|
(load-system-shared-libraries (find-system-named  "aerovironment"))
(system-libraries-missing (find-system-named  "aerovironment") :library-location-type nil)
(multiple-value-list (merge-shared-library-pathname-type (shared-library-pathname (find-system-named  "aerovironment") "libfreedius_avapi")))
(relocate-system-path "/opt/IU/freedius-systems/proprietary/aerovironment/sysdef-aerovironment.lisp"
		      "proprietary/aerovironment/sysdef-aerovironment.lisp" 
		      "/opt/IU/freedius-systems/arch/linux-sbcl64/fasl/")
#P"/opt/IU/freedius-systems/arch/linux-sbcl64/fasl/proprietary/aerovironment/sysdef-aerovironment.lisp"

(find-source-fasl-hierarcies  (truename "$FREEDIUS/c/CMakeLists.txt"))
(merge-shared-library-pathname-type(shared-library-pathname (find-system-named  :tk) "liblisptk"))
(translate-logical-pathname (merge-shared-library-pathname-type(shared-library-pathname (find-system-named  :tk) "liblisptk")))
(maybe-execute-cmake (find-system-named  :tk))
(system-libraries-missing (find-system-named  :tk) :library-location-type :local)
(probe-file (cmake-script-path (find-system-named  :tk)))
(find-source-fasl-hierarcies (cmake-script-path (find-system-named  :tk)))
(translate-logical-pathname #P"FREEDIUS:LIB-FLAT;")
(translated-namestring (lx::pathname-as-file 
			(relocate-system-path (cmake-script-path (find-system-named  :tk))
					      #P"lisptk/CMakeLists.txt" '(:FLAT #P"FREEDIUS:LIB-FLAT;"))
			))
(let* ((script-path (cmake-script-path (find-system-named  :tk)))
       (source-path (make-pathname :directory (pathname-directory script-path))))
  (multiple-value-bind (rel-path lisp-hier fasl-hier lib-hier build-hier)
      (find-source-fasl-hierarcies script-path)
    ;;(relocate-system-path source-path rel-path lib-hier)
    (translated-namestring (lx::pathname-as-file 
			    (relocate-system-path source-path rel-path lib-hier)))
    ))
(lx::pathname-as-file #P"FREEDIUS:LIB-FLAT;")

(find-source-fasl-hierarcies (make-pathname :directory (pathname-directory (cmake-script-path (find-system-named  :tk)))))
#P"lisptk/"

NIL

NIL

(:FLAT #P"FREEDIUS:LIB-FLAT;")

#P"FREEDIUS:TMP;"

#P"$FREEDIUS/c/lisptk/"


(translate-logical-pathname "FREEDIUS:ARCH;LIB-FLAT;LIBLISPTK.SO.NEWEST")
|#

;(system-libraries-installed-p (find-system-named  "aerovironment"))
   
(defmethod cmake-script-path ((system mixed-lisp-system))
  (merge-system-c-source-pathname "CMakeLists.txt" system))

(defun run-program-namestring (path)
  ;; FIXME --- lx package isn't yet around. Need lx::pathname-as-file and lx::run-program 
  (flet ((pathname-as-file (path)
	   (funcall (intern "PATHNAME-AS-FILE" :lx) path)))
    (namestring (pathname-as-file (if (typep path 'logical-pathname)
				      (translate-logical-pathname path)
				      (ev-pathname-translate path))))))


;;; Do we want a new system class for systems with cmake compiled libraries?
;(fmakunbound 'cmake-compile-system-libraries)
#+old
(defmethod cmake-compile-system-libraries ((system mixed-lisp-system)
					   &key 
					   (arch *freedius-arch-name*) ; non-canonical placement
					   (lisp (string-downcase *lisp-type*))
					   (options "-DNOLINK_TCL_TK=1")
					   test)
  (flet ((run-program (&rest args)
	   (apply (intern "RUN-PROGRAM" :lx) args)))
    (let ((CMakeLists-txt-path (cmake-script-path system)))
      (when (probe-file CMakeLists-txt-path)
	(if (null (system-libraries-missing system :library-location-type :local :missing-action :warn))
	    :installed
	    (multiple-value-bind (rel-path lisp-hier fasl-hier lib-hier build-hier)
		(find-source-fasl-hierarcies CMakeLists-txt-path)
	      (declare (ignore  lisp-hier fasl-hier))
	      (let* ((source-path (make-pathname :directory (pathname-directory CMakeLists-txt-path)))
		     (source-dir (run-program-namestring source-path))
		     (build-dir (run-program-namestring
				 (relocate-system-path source-path rel-path build-hier)))
		     (lib-dir (run-program-namestring
			       (relocate-system-path source-path rel-path lib-hier)))
		     (script (run-program-namestring "$FREEDIUS/c/run-cmake.sh"))
		     (shell-command (format nil "$FREEDIUS/c/run-cmake.sh ~a ~a ~a ~a ~a ~a ~a ~a~%"
					    *freedius-home-path* *freedius-exec-prefix*
					    lisp arch source-dir build-dir lib-dir options))
		     )
	      
		(if (eq test :script)
		    shell-command
		    (progn
		      (unless test
			(run-program script
				     (list 
				      *freedius-home-path* *freedius-exec-prefix*
				      lisp arch source-dir build-dir lib-dir options)
				     :output t :wait t))
		      (multiple-value-bind (installed-p missing)
			  (system-libraries-installed-p system)
			(if installed-p
			    :compiled-and-installed
			    (progn (warn "cmake-compile-system-libraries ~a libraries missing: ~a~%;; ~a~%" ;
					 (system-name system) missing shell-command)
				   (values :missing-libraries missing shell-command)))))
		    ))))))))

(defmethod cmake-compile-system-libraries ((system mixed-lisp-system)
					   &key 
					   (arch *freedius-arch-name*) ; non-canonical placement
					   (lisp (string-downcase *lisp-type*))
					   (options '(("NOLINK_TCL_TK" 1) ;; #+macosx 0 #-macosx 1)         ;; we should be able to change this using config
						      ("CMAKE_BUILD_TYPE" "debug")
						      #+cocoa ("FREEDIUS_USE_COCOA" 1) ;; same here
						      ))
					   test)
  (flet ((run-program (&rest args)
	   (apply (intern "RUN-PROGRAM" :lx) args)))
    (let ((CMakeLists-txt-path (cmake-script-path system)))
      (when (probe-file CMakeLists-txt-path)
	(if (null (system-libraries-missing system :library-location-type :local :missing-action :warn))
	    :installed
	    (multiple-value-bind (rel-path lisp-hier fasl-hier lib-hier build-hier)
		(find-source-fasl-hierarcies CMakeLists-txt-path)
	      (declare (ignore  lisp-hier fasl-hier))
	      (let* ((source-path (make-pathname :directory (pathname-directory CMakeLists-txt-path)))
		     (source-dir (run-program-namestring source-path))
		     (build-dir (run-program-namestring
				 (relocate-system-path source-path rel-path build-hier)))
		     (lib-dir (run-program-namestring
			       (relocate-system-path source-path rel-path lib-hier)))
		     (script (run-program-namestring #-mswindows "$FREEDIUS/c/run-cmake2.sh"
						     #+mswindows "$FREEDIUS/c/run-cmake2.bat"
						     ))
		     (script-options 
		      (loop for (name val) in `(("F3D" ,*freedius-home-path*)
						("F3DA" ,*freedius-exec-prefix*)
						("LISP" ,lisp)
						("LIBRARY_INSTALL_DIR" ,lib-dir)
						     . ,options)
			    collect (format nil "-D~a=~a" name val)))
		     (shell-command 
		      (format nil "$FREEDIUS/c/run-cmake2.sh ~a ~a ~{~a ~}"
			      build-dir source-dir script-options))
		     )
	      
		(if (eq test :script)
		    shell-command
		    (progn
		      (unless test
			(run-program script (list* build-dir source-dir script-options)
				     #-allegro :output #-allegro t
				     :wait t))
		      (multiple-value-bind (installed-p missing)
			  (system-libraries-installed-p system)
			(if installed-p
			    :compiled-and-installed
			    (progn (warn "cmake-compile-system-libraries ~a libraries missing: ~a~%;; ~a~%" ;
					 (system-name system) missing shell-command)
				   (values :missing-libraries missing shell-command)))))
		    ))))))))

(defmethod cmake-clear-build-directory ((system mixed-lisp-system) &key test)
  (flet ((run-program (&rest args)(apply (intern "RUN-PROGRAM" :lx) args)))
	   (let ((CMakeLists-txt-path (cmake-script-path system)))
    (multiple-value-bind (rel-path lisp-hier fasl-hier lib-hier build-hier)
	(find-source-fasl-hierarcies CMakeLists-txt-path)
      (declare (ignore lisp-hier fasl-hier lib-hier))
      (let* ((source-path (make-pathname :directory (pathname-directory CMakeLists-txt-path)))
	     (build-dir (run-program-namestring (relocate-system-path source-path rel-path build-hier))))
	(if test
	    (format nil "rm -rf ~a" build-dir)
	    (run-program "rm" `("-rf" ,build-dir))))))))


	     
#|
(load "/m/rom1/downloads/slime/slime-20090213/contrib/swank-arglists.lisp")
(cmake-compile-system-libraries (find-system-named  :tk) :test :script)
;(cmake-compile-system-libraries (find-system-named  "aerovironment") :test :script)
;(cmake-compile-system-libraries (find-system-named  "color") :test :script)
;(cmake-compile-system-libraries (find-system-named  "aerovironment"))
;(cmake-compile-system-libraries (find-system-named  "mpeg2"))
;(cmake-compile-system-libraries (find-system-named  "color"))
;(cmake-compile-system-libraries (find-system-named  "color") :test :script)
;(cmake-compile-system-libraries (find-system-named  "aerovironment"))
;(cmake-compile-system-libraries (find-system-named  "aerovironment") :options "-DINSTALL_PREFIX=$F3DSYS/arch/foobar")
(cmake-compile-system-libraries (find-system-named :libfreedius) :test :script)
(cmake-compile-system-libraries (find-system-named :libfreedius))
(cmake-compile-system-libraries (find-system-named :libfreedius) :test :script)
(cmake-compile-system-libraries (find-system-named :libfreedius))
(cmake-clear-build-directory (find-system-named :math) :test :script)
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; cmake-library - using cmake, construct and load a shared object
;;; using cmake.  
;;; 
;;; Shamelessly lifted from :asdf-additions, and adapted for use with
;;; cmake.


(defclass cmake-library (asdf:module mixed-lisp-system)
  (
   #+old
   (link-flags :initarg :link-flags :initform ""
               :reader unix-dso-link-flags)
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Extension of asdf API for using cmake to compile - It is the
;;; responsibility of the cmaker to manage the c sources, but we will
;;; assume that the C source files are in the 'c' subdirectory of the
;;; parent module.
;;;
;;; This should consist of a series of shared object files with no
;;; suffix. The suffix is system-dependent and will be generated
;;; automatically.
;;;
(defun unix-name (pathname)
  (namestring 
   (typecase pathname
     (logical-pathname (translate-logical-pathname pathname))
     (t pathname))))


(defmethod asdf:input-files ((operation asdf:compile-op) (lib cmake-library))
  (mapcar #'asdf:component-pathname (asdf:module-components lib)))

(defmethod asdf:output-files ((operation asdf:compile-op) (lib cmake-library))
  (let ((dir (asdf:component-pathname lib)))
    (list
     (qffi::merge-shared-library-pathname-type
      (make-pathname :type "so"
		     :name (car (last (pathname-directory dir)))
		     :directory (butlast (pathname-directory dir))
		     :defaults dir)))))
;;;
;;; Here's where we run cmake:
;;;
(defmethod asdf:perform ((operation asdf:compile-op) (lib cmake-library))
  (maybe-execute-cmake lib))


(defmethod asdf:perform ((o asdf:load-op) (c cmake-library))
  (let ((co (make-instance 'asdf:compile-op)))
    (let ((filename (car (asdf:output-files co c))))
      (qffi:load-foreign-library filename)
      (terpri *debug-io*)
      (format *debug-io* "~%>> Loaded ~A~%~%" filename))))