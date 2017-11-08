(in-package :cl-user)


;;;
;;; Cmake stuff:
;;;

(defparameter *automatically-run-cmake* t)

;;;
;;; 
(defun load-foreign-library (path &key reload)
  ;; FIXME:  should we cache using evp::ev-pathname-translate value or original path?
  ;; Is this right when using disksaves?
  (let ((namestring (namestring path)))
    (when (or reload (not (member namestring *loaded-foreign-libraries* :test #'string=)))
      (format t "; Loading foreign library ~a~%" path) ;
      (unwind-protect
	   (prog1
	       #+cmu  (multiple-value-bind (status msg) (system::load-object-file namestring)
			(unless status
			  (error msg))
			status)
	       #+allegro (load namestring)
	       #+sbcl (sb-alien:load-shared-object namestring)
	       (push namestring *loaded-foreign-libraries*) ; do not push unless load succeeds.
	       )))))
;;;
;;; Windows note: For some reason, the Windows builds do not install
;;; their DLLs in the correct place.  We still need to manually copy
;;; the files after building...
;;;
(defmethod system-libraries-missing ((system cmake-library) 
				     &key (missing-action :warn) (library-location-type :all))
  (break)
  (loop for libname in (asdf::system-shared-libraries system)
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

(defmethod maybe-execute-cmake ((system cmake-library)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; cmake-library - using cmake, construct and load a shared object
;;; using cmake.  
;;; 
;;; Shamelessly lifted from :asdf-additions, and adapted for use with
;;; cmake.


(defclass cmake-library (asdf:source-file)
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
     (merge-shared-library-pathname-type
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
      (cffi::load-foreign-library filename)
      (terpri *debug-io*)
      (format *debug-io* "~%>> Loaded ~A~%~%" filename))))
