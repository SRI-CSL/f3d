(in-package :qffi)


(defvar *shared-library-pathname-type*
  #+linux "so"
  #+sun "so"
  #+irix "so"
  #+macosx "dylib"
  #+mswindows "dll"
  )

(defvar *shared-library-pathname-types*
  '("so" "dylib" "dll"))

;;; this is really defined in config.lisp
(defvar config::*library-search-paths* 
  `(,(truename (st::merge-freedius-arch-path "lib/"))
     .,(or #+(and :unix :X86-64) '("/usr/lib64/" "/usr/local/lib64/" "/opt/lib64/" )
	   #+(and :unix (not :X86-64)) '("/usr/lib/" "/usr/local/lib/" "/opt/lib/")
	   #+mswindows '("C:\\windows\\system32\\" )
	   (error "*default-library-search-paths* not defined for this architecture")))
  "Search paths used by qffi::find-shared-library")

(defun find-shared-library (path &key (library-search-paths config::*library-search-paths*))
  (let ((path-dirs (pathname-directory path)))
    (if (or (null path-dirs) (eq (car path-dirs) :relative))
	(loop ;; with path = (merge-shared-library-pathname-type path)
	      for library-path in library-search-paths
	      for full-path = (make-pathname :defaults path
				   :directory (append (pathname-directory library-path)
						      (cdr (pathname-directory path))))
	      when (probe-file full-path)
		return full-path)
	path)))

;;(find-shared-library "atlas/libatlas.so")


(defvar *loaded-foreign-libraries* nil)

(defun load-foreign-library (path &key reload)
  ;; FIXME:  should we cache using evp::ev-pathname-translate value or original path?
  ;; Is this right when using disksaves?
  (let ((namestring (namestring (evp::ev-pathname-translate path))))
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


(defvar *force-shared-library-pathname-type* t)
;;; Apparently this was added to deal with library names of the form libfoo.2.3, where the
;;; pathname type is part of the library version, not the OS library extension.
;;; 
(defun merge-shared-library-pathname-type (path &optional (force *force-shared-library-pathname-type*))
  (if force
      (let ((type (pathname-type path)))
	(if (and type (member type *shared-library-pathname-types* :test #'string=))
	    (make-pathname :defaults path :type *shared-library-pathname-type*)
	    ;; loses for library names like "libtcl.8.6"
	    (format nil "~a.~a" path *shared-library-pathname-type*)))
      (if (pathname-type path)
	  path
	  (or (and (string= *shared-library-pathname-type* "dylib")
		   ;; Whoa, why?
		   ;; MacOSX framework dylibs do NOT have any extension, e.g. /Library/Frameworks/Tk.framework/Tk
		   ;; If they need to be loaded explicitly, then this function would not work, since it forces the
		   ;; dylib extension.
		   (let ((new (probe-file path)))
		     (and  new
			   (null (pathname-type new))
			   path)))
	      (make-pathname :defaults path :type *shared-library-pathname-type*)))))

;;; hack to compare *shared-library-pathname-types* using string-equal rather than string=
;;; This is needed due to probably bug in SBCL (pathname-type <logical-pathname>)
(defun merge-shared-library-pathname-type (path &optional (force *force-shared-library-pathname-type*))
  (if force
      (let ((type (pathname-type path)))
	(if (and type (member type *shared-library-pathname-types* :test #'string-equal))
	    ;; replace the possibly bogus pathname-type with the proper type on this platform.
	    ;; FIXME:  It is probably better to leave it alone.
	    (make-pathname :defaults path :type *shared-library-pathname-type*)
	    ;; append .<type> to namestring to handle cases like libtcl.8.6
	    (format nil "~a.~a" (namestring path) *shared-library-pathname-type*)
	    ))
      (if (pathname-type path)
	  path
	  (or (and (string= *shared-library-pathname-type* "dylib")
		   ;; Whoa, why?
		   ;; MacOSX framework dylibs do NOT have any extension, e.g. /Library/Frameworks/Tk.framework/Tk
		   ;; If they need to be loaded explicitly, then this function would not work, since it forces the
		   ;; dylib extension.
		   (let ((new (probe-file path)))
		     (and  new
			   (null (pathname-type new))
			   path)))
	      (make-pathname :defaults path :type *shared-library-pathname-type*)))))

(defun require-shared-libraries (&rest libs)
  (loop for lib in libs
	do (load-foreign-library (merge-shared-library-pathname-type lib))))


;;; Rename to MAKE-OSDEP-GLOBAL-NAME
(defun remap-foreign-symbol-name (ffi-name)
  #-allegro ffi-name
  #+allegro (ff:convert-foreign-name ffi-name))

(defun foreign-code-address (callback-namestring)
  (let ((pointer (foreign-variable-pointer (remap-foreign-symbol-name callback-namestring))))
    (if pointer
	(foreign-pointer-address pointer)
	(error "The foreign symbol ~a was not found." callback-namestring)))
  )


(defparameter *foreign-pointer-hash-table* (make-hash-table :test 'eql))

(defun intern-foreign-pointer (fp)
  (and fp
       (if (numberp fp)
	   (if (eql fp 0)
	       nil
	       fp)
	   (let ((addr (foreign-pointer-address fp)))
	     (if (zerop addr)
		 nil
		 (or (gethash addr *foreign-pointer-hash-table*)
		     (setf (gethash addr *foreign-pointer-hash-table*)
			   fp)))))))


;;; ******************  DEF-FOREIGN-CONSTANTS  ******************

;;#-(and macosx allegro) (defvar *foreign-loader-constant-symbol-prefix* (freedius-prefix "C_"))
;;#+(and macosx allegro) (defvar *foreign-loader-constant-symbol-prefix* (format nil "_~a" (freedius-prefix "C_")))

(defparameter *foreign-loader-constant-symbol-prefix* 
  (remap-foreign-symbol-name (freedius-prefix "C_")))

(defmacro def-foreign-constants (&rest foreign-names)
  (loop with package = *package*	;(find-package "IC")
	     for name in foreign-names
	     for lisp-symbol = (if (consp name) (car name) (intern (string-upcase name) package))
	     for c-name = (concatenate 'string
				       *foreign-loader-constant-symbol-prefix*
				       (if (consp name) (cadr name) name))	
	     collect lisp-symbol into foreign-constant-symbols
	     collect (list lisp-symbol c-name) into symbol-c-name-list
	     finally
	     (return `(progn (declaim (special . ,foreign-constant-symbols))
		       (def-foreign-constants-internal ',symbol-c-name-list)))))

(defparameter *foreign-value-constants-list* nil)

(defun def-foreign-constants-internal (symbol-c-name-list)
  (setq *foreign-value-constants-list* (append *foreign-value-constants-list* symbol-c-name-list))
  (loop for (symbol  c-name) in symbol-c-name-list
	do (setf (symbol-value symbol) (foreign-variable-value c-name)))) 

#|
(length *foreign-value-constants-list*) = 86

(with-open-file (st "/tmp/freedius2-foreign-constants.lisp" :direction :output :if-exists :supersede)
  (loop for (name c-name) in *foreign-value-constants-list*
	do (pprint `(defconstant ,name ,(foreign-variable-value c-name)) st)))
|#

;;;
;;; This is the earliest point at which I think I can insert this.  It
;;; really ought to be in LISP-EXTENSIONS, but will usually require
;;; ffi.  For now, put it here:

#+linux ;; really posix - also, usleep is deprecated.  We should migrate to nanosleep...
(def-foreign-function (usleep
		       (:name "usleep")
		       (:return-type :unsigned-32bit))
  (microseconds :unsigned-32bit))

