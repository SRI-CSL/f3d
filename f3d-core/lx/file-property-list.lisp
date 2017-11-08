(in-package :system-tool)

;;; Not sure where this file belongs  - what system?  - what directory?

;;; This is file might be better handled using require/provide machinery.

;;; ***************************  FILE-PROPERTY-LIST  ***************************

(defun compile-file-source-pathname ()
  #+allegro excl::*source-pathname*
  #+cmu *compile-file-pathname*
  )

(defmacro FILE-PROPERTY-LIST (&rest file-property-list )
  `(apply 'file-property-list-internal
    ,(compile-file-source-pathname)
    :file-name (getf ',file-property-list :file-name)
    ',file-property-list))

#| ; moved back to common-symbols-pkg.lisp
(eval-when (compile load)
  (import '(st::FILE-PROPERTY-LIST) :common-symbols))

(eval-when (compile load)
  (export '(common-symbols::FILE-PROPERTY-LIST) :common-symbols))
|#

(defparameter *file-property-list-hash-table* (make-hash-table :test 'equal))

#+never ; already defined in $FREEDIUS/lisp/system-tool/system-tool.lisp
(defun get-system-path-prefix (system-name)
  (let* ((system (find-system-named system-name :error-ok t))
	 (path (and system (system-default-pathname system))))
    (and path (make-pathname :defaults path :type nil :name nil))))

(defparameter *disable-file-property-list-warnings* t)

(declaim (special *system-being-loaded*))
;(defvar *system-being-loaded* nil)

(defun file-property-list-internal (this-file
				    &rest file-property-list
				    &key file-name &allow-other-keys)
  (when (and this-file file-name)
    ;;; special hack for building CME.
    (let* ((system-path-prefix (get-system-path-prefix *system-being-loaded*)))

      (unless (member (aref file-name 0) '(#\$ #\~ #\/))
					; some form of absolute pathname
	(when system-path-prefix
	  (setq file-name (format nil "~a~a" system-path-prefix file-name))))
      (let* ((translated-file-name (probe-file file-name)))
	(when #+allegro EXCL::*multiple-redefinition-action*
	      ;; If redefinition warnings are off we are probably loading files from patch files.
	      ;; Allow name disagreement in that case.
	      (when (or (null translated-file-name)
			(not (equal (pathname-name file-name) (pathname-name this-file)))
			(and (ev-pathname-p this-file )
			     (ev-pathname-p file-name)
			     (not (equal (namestring this-file) (namestring file-name)))))
		(unless  *disable-file-property-list-warnings*
		    
		  (format t ";;; Warning: :file-name ~a in file-property-list of ~a is wrong~%"
			  file-name this-file)))

	      (when *system-being-loaded*
		(when (and (getf file-property-list :system)
			   (not *disable-file-property-list-warnings*)
			   (not (equal (getf file-property-list :system)
				       *system-being-loaded*)))
		  (format t ";;; Warning: in file-property-list of ~a :system disagrees with system-being-loaded = ~a~%"
			  this-file *system-being-loaded*))
		(setf (getf file-property-list :system) *system-being-loaded*)))

	(setq this-file (ev-pathname-backtranslate this-file))
	
	(setf (gethash (namestring this-file) *file-property-list-hash-table*)
	      file-property-list)
	this-file))))


(defun get-file-property-list-hash-table-pathname-list ()
  (loop for path being the hash-keys of *file-property-list-hash-table*
	collect (namestring (ev-pathname-backtranslate path))))

(defun get-loaded-pathnames ()
  (loop for path being the hash-keys of *load-path-times-hash-table*
	collect (namestring (ev-pathname-backtranslate path))))

