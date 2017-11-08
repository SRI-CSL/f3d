(in-package :lisp-extensions)

;;; This is now loaded as part of system-tool-bootstrap.lisp

;;; Allegro defines PATHNAME-AS-DIRECTORY in the excl package.

#+allegro
(eval-when (eval load compile)
  (import 'excl::pathname-as-directory))

;;;#+cmu
;;;(defun pathname-as-directory (path)
;;;  (if (directory-p path)
;;;      (if (pathname-name path)
;;;          (format nil "~a/"  path)
;;;          path)
;;;      (make-pathname :host (pathname-host path)
;;;                     :device (pathname-device path)
;;;                     :directory (append (or (pathname-directory path) '(:relative))
;;;                                        (list (if (pathname-type path)
;;;                                                  ;; FIXME: Unix specific pathname merging
;;;                                                  (format nil "~a.~a"
;;;                                                          (or (pathname-name path) "")
;;;                                                          (pathname-type path))
;;;                                                  (pathname-name path))))
;;;                     ;;:name :wild ; omit for compatibility with Allegro
;;;                     )
;;;      ))

;;; Modification of above to avoid use of directory-p
;;; Turn pathname, a file namestring or pathname, into a pathname
;;; where all of pathname is in the directory component. 


;;; my version before snarfing the version from Practical Common Lisp
#+never
(defun pathname-as-directory (path)
  (make-pathname :defaults path 
		 :directory (append (or (pathname-directory path) '(:relative))
				    (and  (pathname-name path)
					  (list (if (pathname-type path)
						    (format nil "~a.~a"
							    (or (pathname-name path) "")
							    (pathname-type path))
						    (pathname-name path)))))
		 :name nil :type nil))


;;;;  Taken from Practical Common Lisp

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (p)
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))

#-allegro ; pcl version
(defun pathname-as-directory (pathname)
  (let ((pathname (if (stringp pathname) (pathname pathname) pathname)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p pathname))
	(make-pathname
	 :directory (append (or (pathname-directory pathname) '(:relative))
			    (list (file-namestring pathname)))
	 :name nil
	 :type nil
	 :defaults pathname)
	pathname)))

;;; (defun pathname-as-file (name)
;;;   (let ((pathname (if (stringp name) (pathname name) name)))
;;;     (when (wild-pathname-p pathname)
;;;       (error "Can't reliably convert wild pathnames."))
;;;     (if (directory-pathname-p name)
;;; 	(let* ((directory (pathname-directory pathname))
;;; 	       (name-and-type (pathname (first (last directory))))) ; this loses if there is a ":" 
;;; 	  (make-pathname
;;; 	   :directory (butlast directory)
;;; 	   :name (pathname-name name-and-type)
;;; 	   :type (pathname-type name-and-type)
;;; 	   :defaults pathname))
;;; 	pathname)))


;;; This has been "fixed" to handle directory names containing a colon(:), but the fix 
;;; assumes that period(.)  is the separator between names and types.
(defun pathname-as-file (name)
  (let ((pathname (if (stringp name) (pathname name) name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p name)
	(let* ((directory (pathname-directory pathname))
	       (directory-name-and-type (first (last directory)))
	       (dot-pos (position #\. directory-name-and-type))
	       (name (if dot-pos (subseq directory-name-and-type 0 dot-pos) directory-name-and-type))
	       (type (if dot-pos (subseq directory-name-and-type (1+ dot-pos)) nil)))
	  (make-pathname
	   :directory (butlast directory)
	   :name name
	   :type type
	   :defaults pathname))
	pathname)))


(defun directory-wildcard (dirname)
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

(defun list-directory (dirname &rest args)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (let ((wildcard (directory-wildcard dirname)))
    #+(or sbcl cmu lispworks)
    (apply #'directory wildcard args)

    #+openmcl
    (directory wildcard :directories t)

    #+allegro
    (directory wildcard :directories-are-files nil)

    #+clisp
    (nconc
     (directory wildcard)
     (directory (clisp-subdirectories-wildcard wildcard)))

    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented")))

#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard))

;;; replacement for probe-file
;;; Not really needed since both Allegro and CMUCL implement this now
(defun file-exists-p (pathname)
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)

  #+(or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

  #+clisp
  (or (ignore-errors
	(probe-file (pathname-as-file pathname)))
      (ignore-errors
	(let ((directory-form (pathname-as-directory pathname)))
	  (when (ext:probe-directory directory-form)
	    directory-form))))

  #-(or sbcl cmu lispworks openmcl allegro clisp)
  (error "list-directory not implemented"))

(defun walk-directory (dirname fn &key directories (test (constantly t)) directory-args)
  (labels
      ((walk (name)
	 (cond
	   ((directory-pathname-p name)
	    (when (and directories (funcall test name))
	      (funcall fn name))
	    (dolist (x (apply #'list-directory name directory-args)) (walk x)))
	   ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))



(defun pathname-immediate-directory-name (path)
  (or (and (directory-p path :follow-links t) (pathname-name path))
      (car (last (pathname-directory path)))))

(defun pathname-directory-path (path)
  (make-pathname :host (pathname-host path)
		 :device (pathname-device path)
		 :directory (pathname-directory path)))

#|

(describe (pathname "*"))
(dir-relative-path "$RADIUS/sites" "*") = #P"$RADIUS/sites/\\*/"

(car (last (pathname-directory (pathname-as-directory *freedius-exec-prefix*))))

(describe (pathname "lib/")) ; :directory = (:relative "lib")

|#

#|	
MERGE-DIRECTORIES-AND-PATHNAME is a generalization of MERGE-PATHNAMES PREFIX.

PREFIX is an initial directory pathname-specifier.  

Except for the last element, the elements of DIRS are relative pathname specifiers represented by
either a string, a list of strings, or a pathname specifying the directory components of a relative
pathname.

If the last element of DIRS is NULL, then a directory pathname is
always returned otherwise it the last element is merged with the directory-pathname formed from
everything else. 
|#

(defun merge-directories-and-pathname (&rest dirs)
  (let (prefix)
    (loop do (setq prefix (pop dirs)) until prefix)
    (if (null dirs)
	(pathname prefix)
	(loop for (dir . rest) on dirs
	      append (typecase dir
		       (pathname (let ((dl (pathname-directory dir)))
				   (when dl
				     (if (eq (car dl ) :relative)
					 (cdr dl)
					 (error "dir specs must be relative after the first")))))
		       (string (and rest (list dir)))
		       (list (and rest dir)))
		into dir-list
	      finally 
	   (return (let* ((dir-path (merge-pathnames (make-pathname :directory (list* :relative dir-list))
						     (pathname-as-directory prefix))))
		     (if dir
			 (merge-pathnames (pathname dir) dir-path)
			 dir-path)))))))

(defun cme-arch-relative-path (&rest dirs)
  (apply #'merge-directories-and-pathname *freedius-exec-prefix* dirs))

(defun cme-arch-relative-lisp-path (&rest dirs)
  (apply #'merge-directories-and-pathname *freedius-home-path*
	 "lisp" 
	 #+cmu "cmucl" #+allegro "allegro" #+sbcl "sbcl"
	 dirs))

(defun cme-relative-path (&rest dirs)
  (apply #'merge-directories-and-pathname *freedius-home-path* dirs))

(defun dir-relative-path (dir-path &rest dirs)
  (apply #'merge-directories-and-pathname dir-path dirs))


	
#|
(trace dir-relative-path)
(merge-directories-and-pathname (pathname-as-directory "/usr") "bin" nil)
(merge-directories-and-pathname (pathname "/usr/") "bin" nil)
(merge-directories-and-pathname (pathname "/usr/") "bin" "foo.bar")
(merge-directories-and-pathname "/usr/" "*") = #P"/usr/\\*/"
(merge-directories-and-pathname (pathname "/usr/") "*") = #P"/usr/\\*/"
(merge-directories-and-pathname (pathname "/usr/") "bin" "*") = #P"/usr/bin/*"
(describe (pathname "*"))
(merge-pathnames (pathname "*") (pathname "/usr/bin/"))
(merge-directories-and-pathname (pathname "/usr/") '("bin" "baz") "foo.bar")
(merge-directories-and-pathname (pathname "/usr/") (pathname "bin/baz/") "foo.bar")
(merge-directories-and-pathname (pathname "usr/") (pathname "bin/baz/") "foo.bar")
(merge-directories-and-pathname "/usr/" (pathname "bin/baz/") "foo.bar")
(merge-directories-and-pathname "/usr/" '("bin" "baz") "foo.bar")
(merge-directories-and-pathname "$FREEDIUS/" '("lisp" "math") "matrices.lisp")
(truename (merge-directories-and-pathname "$FREEDIUS/" '("lisp" "math") "matrices.lisp"))
(merge-directories-and-pathname "$FREEDIUS" '("lisp" "math") "matrices.lisp")
(make-pathname :directory '(:absolute "usr" :relative "bin"))
(merge-pathnames "*" "/usr/")
(enough-namestring "/usr/local/foo.bar" (make-pathname :directory '(:absolute "usr" "local") 
						       ;:name "foo" :type nil
						       :name :wild :type "bar"
						       ))
(setf (logical-pathname-translations "FREEDIUS")
      (list (list "**;*.*" (pathname (merge-freedius-path "")))))
(setf (logical-pathname-translations "FREEDIUS")
      (list (list "**;*.*.*" (merge-freedius-path ""))))

(describe (pathname "FREEDIUS:lisp;math;matrices.lisp"))
(describe (lisp::%pathname-host (pathname "FREEDIUS:LISP;MATH;MATRICES.LISP")))
(pathname-match-p (pathname "FREEDIUS:lisp;math;matrices.lisp")
		  ";**;*.*.*")
(logical-pathname-translations (pathname-host (pathname "FREEDIUS:lisp;math;matrices.lisp")))

(cme::load-site-glue "Alv" cme::*radius-site-glue*)
 
(cme-arch-relative-path "lib")

(cme-relative-path1 "/opt/radius/sites" "*")
(pathname "*")

(describe cme::*radius-site-glue*)
(cme::glue-file-name cme::*radius-site-glue*)
(cme::site-path "alv"cme::*radius-site-glue*)

(merge-pathnames (pathname "foo/bar.lisp") "/usr/")

(merge-pathnames (pathname "foo/bar.lisp") "$FREEDIUS/")

(cme::camera-model-paths (cme::get-2d-world-named "alv-2-44" )
			 (cme::load-site-glue "alv" cme::*radius-site-glue*))
|#








#|
(describe #P"foo.lisp.~1~") 
 NAME         "foo.lisp"
 TYPE         "~1~"
 VERSION      :NEWEST
(describe #P".cshrc.~1~") 
 NAME         ".cshrc"
 TYPE         "~1~"

(make-pathname :name "foo" :type "bar" :version :wild)
(pathname-version #P"foo.bar.~*~")

(parse-versioned-pathname #P"foo.lisp.~1~")
(parse-versioned-pathname #P"foo.lisp.~*~")
(namestring #P"foo.lisp.~*~")
(parse-versioned-pathname  #P".cshrc.~1~")
(parse-versioned-pathname  #P".cshrc.~*~")
(parse-versioned-pathname #P"foo.lisp")
(parse-versioned-pathname  #P".cshrc")
(pathname-type "foo") NIL
(parse-versioned-pathname #P"foo.bar.baz")
(parse-versioned-pathname #P"./")
(parse-versioned-pathname #P"..")
(parse-versioned-pathname #P"../")
(describe #P"foo.bar.baz") 
(describe #P"./")
(describe #P"../")
(describe #P"/tmp/directory-test/../") 
(probe-file "/tmp/directory-test/../")
SBCL
NAME         "foo.bar"
 TYPE         "baz"

(let ((type "~*~"))
  (read-from-string type nil nil :start 1 :end (1- (length type))))
|#

#+(or sbcl allegro)
(progn

;;; returns (name type version)
(defun parse-versioned-pathname (pathname-or-namestring)
  (let* ((path (pathname pathname-or-namestring))
	 (name (pathname-name path))
	 (type (pathname-type path))
	 (size (and (stringp type) (length type))))
    (declare (pathname path))
    (if (and size (> size 2)
	     (char= (aref type 0) #\~) 
	     (char= (aref type (1- size)) #\~))
	;; here we have a versioned pathname
	(let* ((version (read-from-string type nil nil :start 1 :end (1- (length type))))
	       (pos (position #\. name :from-end t))) 
	  (if (and pos (> pos 0)) ; starting dot not a pathname-type delimiter
	      (values (subseq name 0 pos) 
		      (subseq name (1+ pos))
		      version)
	      (values name nil version)))
	(values name type nil))))


;;; returns (name type version)
(defun parse-versioned-pathname (pathname-or-namestring)
  (let* ((path (pathname pathname-or-namestring))
	 (name (pathname-name path))
	 (type (pathname-type path))
	 (size (and (stringp type) (length type))))
    (when (and type (null size) (wild-pathname-p path :type))
      (let* ((namestring (namestring path))
	     (pos (position #\. namestring :from-end t)))
	(when (and pos (> pos 1))
	  (setq type (subseq namestring (1+ pos))
		size (length type)))))

    (if (and size (> size 2)
	     (char= (aref type 0) #\~) 
	     (char= (aref type (1- size)) #\~))
	;; here we have a versioned pathname
	(let* ((version0 (read-from-string type nil nil :start 1 :end (1- (length type))))
	       (version (if (eql version0 '*) :wild version0))
	       (pos (position #\. name :from-end t))) 
	  (if (and pos (> pos 0)) ; starting dot not a pathname-type delimiter
	      (values (subseq name 0 pos) 
		      (subseq name (1+ pos))
		      version)
	      (values name nil version)))
	(values name type nil))))

) ; end progn

#+cmu
(progn

(defun parse-versioned-pathname (pathname-or-namestring)
  (let ((path (pathname pathname-or-namestring)))
    (values (pathname-name path) (pathname-type path) (pathname-version path))))

) ; end progn



(defun path-with-version-p (path)
  (setq path (pathname path))
  (let* ((name (pathname-name path))
	 (type (or (pathname-type path) name)))
    (cond ((and name (find #\. name))
	   type)
	  ((and type (> (length type) 0)
		(or (char-equal (aref type 0) #\~) (char-equal #\# (aref type (1- (length type))))))
	   type)
	  (name (let ((ver (pathname-version path)))
		  #+allegro (unless (eq ver :unspecific) ver)
		  #+(or :cmu :sbcl) (and ver (neq ver :NEWEST))
		  
		  )))))
;;; *********************  PATHNAME VERSIONS - BACKUP FILE  *********************

;;; BROKEN in CMUCL -- This does not return the version number -- just T or NIL.
#+(or allegro :sbcl)
(defun pathname-version-number (pathname)
  (unless (typep pathname 'pathname) (setq pathname (pathname pathname)))
  (let ((type (pathname-type pathname)) (name (pathname-name pathname)))
    (if (and type (stringp type) (> (length type) 0))
	(cond ((and (char-equal (aref type 0) #\~)
		    (char-equal (aref type (1- (length type))) #\~))
	       (read-from-string type nil nil :start 1 :end (- (length type) 1)))
	      ;; This handles the temporary version GNUEmacs creates of the form
	      ;; #foo.lisp#
	      ((and (char-equal (aref type (1- (length type))) #\#)
		    (pathname-name pathname)
		    (char-equal (aref (pathname-name pathname) 0) #\#))
	       t))

	(and (null type) name
	     (char-equal (aref name (1- (length name))) #\#)
	     (char-equal (aref name 0) #\#))
	)))

#+:cmu
(defun pathname-version-number (pathname)
  (pathname-version pathname))

#|
(path-with-version-p "/tmp/directory-test/foo.lisp.~1~")
(path-with-version-p "/tmp/directory-test/foo.lisp")
(pathname-version-number "~/#changed-files#")
(inspect (pathname "~/#changed-files#"))
(inspect (pathname "~/#start-up-ic.lisp#"))
(file-version-list "$C6/ic/paged-image.lisp")
(file-version-list "/tmp/directory-test/foo.lisp")
(pathname-version-number "/tmp/directory-test/foo.lisp.~1~")
(backup-file-pathname "/tmp/directory-test/foo.lisp")
(directory "/tmp/directory-test/*.lisp.~*~")

|#



#|
(defun create-test-directory (&optional (dir "/tmp/directory-test"))
  (unless (probe-file "/tmp/directory-test")
    (flet ((make-file (name)
	     (with-open-file (st (format nil "~a/~a" dir name) :direction :output :if-exists :supersede)
	       (format st "foo~%"))))
      (create-directory dir)
      (make-file "foo.lisp") (make-file "foo.lisp.~1~") (make-file "foo.lisp.~2~")
      (make-file "foox.lisp") (make-file "foox.lisp.~1~"))))



(defun directory-regression-test (&optional (dir "/tmp/directory-test"))
  (create-test-directory (pathname-as-directory dir))

  (flet ((tst (spec)
	   (directory (format nil "~a/~a" dir spec))))
    (list (tst "foo.lisp")
	  (tst "foo.lisp.*")
	  (tst "foo.lisp*")
	  (tst "foo.lisp*.*"))))

(directory-regression-test)

cmucl-19f-pre1-x86-linux
((#P"/tmp/directory-test/foo.lisp" #P"/tmp/directory-test/foo.lisp.~1~"
    #P"/tmp/directory-test/foo.lisp.~2~")
 NIL
 (#P"/tmp/directory-test/foo.lisp" #P"/tmp/directory-test/foo.lisp.~1~"
    #P"/tmp/directory-test/foo.lisp.~2~")
 NIL)
clisp
((#P"/tmp/directory-test/foo.lisp")
 (#P"/tmp/directory-test/foo.lisp.~2~" #P"/tmp/directory-test/foo.lisp.~1~")
 (#P"/tmp/directory-test/foo.lisp.~2~" #P"/tmp/directory-test/foo.lisp2.~1~"
  #P"/tmp/directory-test/foo.lisp2" #P"/tmp/directory-test/foo.lisp.~1~"
  #P"/tmp/directory-test/foo.lisp")
 (#P"/tmp/directory-test/foo.lisp.~2~" #P"/tmp/directory-test/foo.lisp2.~1~"
  #P"/tmp/directory-test/foo.lisp.~1~"))
sbcl 20090403
((#P"/tmp/directory-test/foo.lisp")
 (#P"/tmp/directory-test/foo.lisp.~1~" #P"/tmp/directory-test/foo.lisp.~2~")
 (#P"/tmp/directory-test/foo.lisp")
 (#P"/tmp/directory-test/foo.lisp.~1~" #P"/tmp/directory-test/foo.lisp.~2~"))

(defun file-version-list-regression-test (&optional (dir "/tmp/directory-test"))
  (create-test-directory dir)
  (let ((result (file-version-list (format nil "~a/foo.lisp" dir))))
    (unless (equal result '(1 2))
      (warn "FILE-VERSION-LIST-REGRESSION-TEST FAILED: returned ~a~%" result))))
  
(file-version-list-regression-test)
(directory "/tmp/directory-test/foo.lisp.*")
(describe #P"/tmp/directory-test/foo.lisp.~1~") 

SBCL
HOST         #<SB-IMPL::UNIX-HOST {916A6B1}>
 DEVICE       NIL
 DIRECTORY    (:ABSOLUTE "tmp" "directory-test")
 NAME         "foo.lisp"
 TYPE         "~1~"
 VERSION      :NEWEST

CMUCL
HOST: #<LISP::UNIX-HOST>.
DEVICE: NIL.
DIRECTORY: (:ABSOLUTE "tmp" "directory-test").
NAME: "foo".
TYPE: "lisp".
VERSION: 1.

Allegro
HOST               NIL
 DEVICE             :UNSPECIFIC
 DIRECTORY          (:ABSOLUTE "tmp" "directory-test")
 NAME               "foo.lisp"
 TYPE               "~1~"
 VERSION            :UNSPECIFIC
 NAMESTRING         "/tmp/directory-test/foo.lisp.~1~"
 HASH               NIL
 DIR-NAMESTRING     "/tmp/directory-test/"
 PLIST              NIL

(describe #P"/tmp/directory-test/foo.lisp.1")
SBCL
 HOST         #<SB-IMPL::UNIX-HOST {916A6B1}>
 DEVICE       NIL
 DIRECTORY    (:ABSOLUTE "tmp" "directory-test")
 NAME         "foo.lisp"
 TYPE         "1"
 VERSION      :NEWEST

(describe (make-pathname :defaults "/tmp/directory-test/foo.lisp" :version 1))
(namestring (make-pathname :defaults "/tmp/directory-test/foo.lisp" :version 1))
"/tmp/directory-test/foo.lisp"
Apparently SBCL doesn't support pathname-version at all (other than :newest, nil, :unspecific, and :wild)
Allegro appears to have the same behavior.

(file-version-list "/tmp/directory-test/foo.lisp")
|#

#+cmu
(defun file-version-list (pathname)
  (let ((version (pathname-version-number pathname)))
    ;; never backup files which have version numbers
    (unless version
      ;; FIXME: Unix specific pathname merging
      (let ((version-list (loop for pn in (directory (format nil "~a.*"  pathname))
				for ver = (pathname-version-number pn)
				when (numberp ver) collect ver)))
	(sort version-list #'< :key #'identity )))))

#+(or allegro sbcl)   ;; FIXME -- what should be done here?
(defun file-version-list (pathname)
  (let ((version (pathname-version-number pathname)))
    ;; never backup files which have version numbers
    (unless version
      ;; FIXME: Unix specific pathname merging
      (let ((version-list (loop for pn in (directory (format nil "~a.*"  pathname))
				for ver = (pathname-version-number pn)
				when (numberp ver) collect ver)))
	(sort version-list #'< :key #'identity )))))

(defun previous-file-version-pathname (pathname)
  (let ((versions (file-version-list pathname)))
    (when versions (format nil "~a.~~~d~~" pathname (car (last versions))))))


(defun recursive-directory (dir-path &optional leaf-test-fn &rest args &key (allow-versions nil))
  (loop for path in (directory (format nil #-cmu "~a*.*" #+cmu "~a/*" (pathname-as-directory dir-path)))
	when (directory-p path)
	  append (apply #'recursive-directory path leaf-test-fn args)
	else when (or (null leaf-test-fn) 
		      (funcall leaf-test-fn path ))		      
	       when (or allow-versions (null (path-with-version-p path)))
		 collect path))


#|
(recursive-directory  "/opt/IU/radius/sites/alv/models")
(recursive-directory  "/opt/IU/radius/sites/alv/models"
		      #'(lambda(path) (equal (pathname-type path) "fs")))
(recursive-directory  "/opt/IU/radius/sites/alv/models"
		      #'(lambda(path) (multiple-value-bind (name type) (parse-versioned-pathname path)
					  (equal type "fs")))
		     ; :allow-versions t
		      )

(recursive-directory "/opt/IU/FREEDIUS/FREEDIUS3M/admin" 
		     #'(lambda(path) (equal (pathname-type path) "txt"))
		     :allow-versions t
		     )

(recursive-directory "/opt/IU/FREEDIUS/FREEDIUS3M/admin" 
		     #'(lambda(path) (multiple-value-bind (name type) (parse-versioned-pathname path)
				       (equal type "txt")))
		     :allow-versions t
		     )

|#