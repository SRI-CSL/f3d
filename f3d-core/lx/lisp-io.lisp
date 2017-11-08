(in-package :lisp-extensions)

;;; ****************************  I/O FUNCTIONS  ****************************
   

#| FIXME:  

LHQ Sat Feb  4 2006:

For some reason (perhaps CLtL-I didn't have these fns)
I have never used the functions:

   PARSE-NAMESTRING
   FILE-NAMESTRING, DIRECTORY-NAMESTRING, HOST-NAMESTRING, ENOUGH-NAMESTRING
   TRANSLATE-PATHNAME

   
There are many places where I should be using these functions rather than
using ad-hoc code to implement equivalent functionality which has Unix filename dependencies.


There are many places where I should be using MERGE-PATHNAMES rather than string concatenation.

|#


#|

Some of the functions here assume the existence of Unix (or GneWin32)
programs to implement their functionality:

   mkdir
   ln
   cp
   

Other functions assume the existence Posix library functions:

   stat
   lstat
   readlink
   


BE CAREFUL -- CMUCL directory follows links by default.  This can cause problems.

|#

;;; Defined in system-tool-bootstrap.lisp
;;;(defun relative-pathname-p (path)
;;;  (let ((dirs (pathname-directory path)))
;;;    #+allegro (or (null dirs) (eq (car dirs) :relative))
;;;    ))

;;; There must be a better way ...
;;;(defun create-directory (path)
;;;  (setq path (ev-pathname-translate path))
;;;  (unless (probe-file path)
;;;    (run-program "mkdir" :arguments (list (namestring path))
;;;                 :wait t
;;;                 ))
;;;  (probe-file path) )


(defun create-directory (path)
  (ensure-directories-exist path))

(defun path-absolute-p (path)
  (eq (car (pathname-directory path)) :absolute))


#+cmu
(progn

  (defun get-symbolic-link (path)
    (unix:unix-readlink (namestring (ev-pathname-translate path))))

  (defun follow-symbolic-links (path)
    (unix:unix-resolve-links (namestring (ev-pathname-translate path))))

  (defun symbolic-link-p (path)
    (eq (unix:unix-file-kind (namestring (ev-pathname-translate path)) t) :link))

  (defun directory-p (path &key follow-links)
    (eq (unix:unix-file-kind (namestring (ev-pathname-translate path)) (null follow-links)) :directory))

  ) ;; end #+cmu progn

#+sbcl
(progn

  (defun get-symbolic-link (path)
    (sb-unix:unix-readlink (namestring (ev-pathname-translate path))))

  (defun follow-symbolic-links (path)
    ;; I hope this is good enough.  Looking at the sbcl source code it looks right
    (probe-file path))

  (defun symbolic-link-p (path)
    (eq (sb-impl::native-file-kind (namestring (ev-pathname-translate path))) :link))

  (defun directory-p (path &key follow-links)
    (setq path (ev-pathname-translate path))
    (when (and follow-links (symbolic-link-p path))
      (setq path (follow-symbolic-links path)))
    (and path (eq (sb-impl::native-file-kind (namestring path)) :directory)))

  ) ;; end #+sbcl progn


#+allegro
(progn 

  (defun get-symbolic-link (path)
    (excl::symbolic-link-p (namestring (ev-pathname-translate path))))

  (defun follow-symbolic-links (path)
    (namestring (excl:pathname-resolve-symbolic-links path)))

  (defun symbolic-link-p (path)
    (declare (ignore follow-links))
    (not (null (excl::symbolic-link-p path))))
  	

  (defun directory-p (path &key follow-links)
    (setq path (ev-pathname-translate path))
    (when (and follow-links (symbolic-link-p path))
      (setq path (probe-file path)))
    (and path (excl::file-directory-p path)))

  ) ;; end #+allegro progn

#|

(directory-p "/tmp/")
(symbolic-link-p "/tmp/")
cd /tmp; mkdir lispio-tests; cd lispio-tests; touch file; ln -s file symlink
(symbolic-link-p "/tmp/lispio-tests/file")
(symbolic-link-p "/tmp/lispio-tests/symlink")
(get-symbolic-link "/tmp/lispio-tests/symlink")
(follow-symbolic-links "/tmp/lispio-tests/symlink")
|#


(defun unix-dot-directory-p (path)
  (let* ((path (if (pathnamep path) (namestring path) path))
	 (rt-side (subseq path (1+ (or (position #\/ path :from-end t ) -1)))))
    (or (string-equal rt-side ".")
	(string-equal rt-side ".."))))

#|
(directory-p "~/.login")
(directory-p "~/tmp")
(directory-p "~/IU/cme") 
(directory-p "~/IU/cme" :follow-links t) 
|#

(defun extract-cme-lisp-file-header (path &key (max-header-length 40))
  (let ((*package* *package*)
	package-decl package-decl-line  property-list comment-lines)
    (with-open-file (in-stream path)
      (loop for line-num from 1 below max-header-length
	    for file-pos = (file-position in-stream)
	    for line = (read-line in-stream  nil nil)
	    while line
	    when (and (null package-decl) (search "(in-package" line :test #'char-equal))
	      do (setq package-decl (read-from-string line)
		       package-decl-line line-num
		       *package* (eval package-decl))
	    else when (search "(CL-USER::FILE-PROPERTY-LIST" line :test #'char-equal)
		   do (file-position in-stream file-pos)
		      (setq property-list (read in-stream))
	    else collect line into lines
	    
	    when (and (>= (length line) 2) (string-equal line "|#" :end1 2))
	      do (setq comment-lines lines)))
    
    (values comment-lines
	    package-decl
	    property-list
	    package-decl-line)))

;;; The CL HyperSpec says to do (merge-pathnames new-name file), whicxh
;;; causes a null (pathname-type new-name) to be replaced by (pathname-type file),
;;; which is "almost" certainly not want one wants.
(defun rename-file* (file new-name)
  (if (pathname-type new-name)
      (rename-file file new-name)   
      (rename-file file (make-pathname :defaults new-name :type :UNSPECIFIC))))

(defun purge-excess-versions (path &key (kept-versions *file-kept-versions*)
				   (kept-old-versions *file-kept-old-versions* )
				   report-only )
  (let* ((version-number-list (nthcdr kept-old-versions (file-version-list path)))
	 (excess-versions (- (length version-number-list) kept-versions)))
    (when (> excess-versions 0)
      (loop repeat excess-versions
	    for purge-version in version-number-list
	    for purge-path = (pathname (format nil "~a.~~~d~~" path purge-version ))
	    do (if report-only
		   (format t "purge file ~a~%" purge-path)
		   (delete-file purge-path))
	    collect purge-path
	    ))))

(defun purge-directory (directory-path &rest args &key kept-versions kept-old-versions report-only )
  (ignore kept-versions kept-old-versions report-only)
  (loop for path in (directory directory-path )
	unless (position #\~ (namestring path))
	  do (apply 'purge-excess-versions path args)))

(defun revert-file-version (pathname)
  (let ((prev-path (previous-file-version-pathname pathname)))
    (when prev-path
      (delete-file pathname)
      (rename-file* prev-path pathname))))

(custom:defcustom *FILE-KEPT-VERSIONS* 5 (:group :lisp-extensions)
  "Number of newest versions kept by PURGE-DIRECTORY.")

(custom:defcustom *FILE-KEPT-OLD-VERSIONS* 1 (:group :lisp-extensions)
  "Number of oldest versions kept by PURGE-DIRECTORY.")

;;; Fri Oct 15 2004 LHQ: BACKUP-FILE-PATHNAME is now broken in CMUCL.  
;;;  DIRECTORY 
(defun backup-file-pathname (pathname)
  (let ((version (pathname-version-number pathname)))
    ;; never backup files which have version numbers
    (unless version
      ;; FIXME: Unix specific pathname merging
      (let* ((pathname-list (directory #+cmu pathname #-cmu (format nil "~a.*"  pathname) ))
	     (highest-version (loop for pn in pathname-list
				    for ver = (pathname-version-number pn)
				    when ver maximize ver))
	     ;; FIXME: Unix specific pathname merging
	     (backup-pathname (pathname (format nil "~a.~~~d~~" pathname (1+ (or highest-version 0))))))
	backup-pathname))))

(defun backup-file-by-rename (pathname)
  (when (probe-file pathname)
    (let ((backup-pathname (backup-file-pathname pathname)))
      (when backup-pathname
	(rename-file* pathname backup-pathname)
	backup-pathname))))

(defun backup-file-by-copy (pathname)
  (when (probe-file pathname)
    (let ((backup-pathname (backup-file-pathname pathname)))
      (when backup-pathname
	(copy-file pathname backup-pathname)
	backup-pathname))))

(defun backup-file (pathname &key copy)
  (when (probe-file pathname)
    (if copy
	(backup-file-by-copy pathname)
	(backup-file-by-rename pathname))))

(defmacro with-directory (directory-pathname &rest body)
  `(let ((old-directory (pwd)))
     (unwind-protect
	 (progn (cd ,directory-pathname)
		. ,body)
       ;; cleanup form
       (cd old-directory))))

;;; This will work on any Unix, but not windows.
;;; options = "-sf" means create symbolic link, overwriting any previously existing link.
;;;(defun make-directory-link (from-path to-path &optional (options '("-sf")))
;;;  (setq from-path (ev-pathname-translate from-path)
;;;        to-path (ev-pathname-translate to-path))
;;;  (let* ((directory (directory-namestring from-path))
;;;         (name (pathname-name from-path))
;;;         (type (pathname-type from-path))
;;;         (link-name (if type
;;;                        ;;(format nil "~a.~a" name type)
;;;                        (namestring (make-pathname :name name :type type))
;;;                        name))
;;;         )
;;;    ;; I do not recall why we do (with-directory ...) rather than passing the complete from-path
;;;    (with-directory directory
;;;      (run-program "ln" `(,@options ,(namestring to-path) ,link-name)))
;;;    ))

;;; This will work on any Unix, but not windows.
;;; options = "-sf" means create symbolic link, overwriting any previously existing link.
(defun make-directory-link (from-path to-path &optional (options '("-sf")))
  (setq from-path (ev-pathname-translate from-path)
	to-path (ev-pathname-translate to-path))
  (run-program "ln" `(,@options ,(namestring to-path) ,(namestring from-path)))
  )

;(unix::unix-symlink   ...)

#+allegro
(defun directory* (wildpath &rest args
		   &key (all t) (check-for-subdirs t) (truenamep t) (follow-links t))
  (declare (ignorable all check-for-subdirs truenamep follow-links))
  (apply 'directory wildpath args))

#+(or cmu sbcl)
(progn

(defun directory-wildcards (wildpath)
  (integerp (position #\* (namestring (make-pathname :directory (pathname-directory wildpath))))))

;;; CMUCL implementation of DIRECTORY misbehaves with follow-links = NIL
(defun directory* (wildpath &rest args
		   &key (all t) (check-for-subdirs t) (truenamep t) (follow-links t))
  (declare (ignorable all check-for-subdirs truenamep))
  (if (or follow-links (directory-wildcards wildpath))
      (apply 'directory wildpath args)
      (let ((prefix-dir (pathname-directory wildpath))
	    (rel-wildpath (make-pathname :directory '(:relative) :defaults wildpath)))
	(with-directory (make-pathname :directory prefix-dir)
	  (loop for path in (apply 'directory rel-wildpath args)
		collect (make-pathname :defaults path
			     :directory (append prefix-dir (cdr (pathname-directory path)))))))))

) ; end #+(or cmu sbcl) progn


(defparameter *cp-program-options* '("-p"))

(defun copy-file (source-pathname dest-pathname &optional (options *cp-program-options* ))
  (setq source-pathname (
			 ev-pathname-translate source-pathname)
	dest-pathname (ev-pathname-translate dest-pathname))
  (run-program "cp"
	       (append options
		       (list (namestring source-pathname)
			     (namestring dest-pathname)))
	       :wait t
	       ))

(defun copy-files (path-list &key from-defaults to-defaults (backup t) (options '("-p" )) )
  (when from-defaults (setq from-defaults (ev-pathname-translate from-defaults)))
  (when to-defaults (setq to-defaults (ev-pathname-translate to-defaults)))
  (loop for path in path-list
	for from-path = (merge-pathnames path from-defaults)
	for to-path = (merge-pathnames path to-defaults)
	do (format t "copying file from ~a to ~a~%" from-path to-path)
	   (when backup (backup-file to-path))
	   (copy-file from-path to-path options )
	))
