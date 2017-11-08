(in-package :ev-pathnames)

;;; This is specific to Allegro because DEFINE-SHADOW-SYSTEM is only used with Allegro.
;;; #+allegro (import '(st::define-shadow-system ))


;;; FIXME -- change this package-name 

;;; Hacks to support ENVIRONMENT VARIABLE PATHNAMES.
;;; On UNIX systems, this makes more sense than COMMON-LISP logical-pathnames
;;; because the same ev-pathname  syntax can be used in both Lisp and Unix shells.

#|

(ev-pathname-translate pathname &optional (error-if-no-translation t)) 
	Translates a pathname containing an environment variable
	 (example: $CMEHOME/alv/alv-camera-models.lisp translates to
	/tmp_mnt/home/avila1/local/cme-public/alv/alv-camera-models.lisp).
	Pathnames not containing environment variable are return unchanged.

(ev-pathname-backtranslate pathname &optional (backtranslation-alist *ev-backtranslation-alist*))
	Backtranslates a physical pathname to its environment variable form 
	by comparing the pathname against the car of each element of backtranslation-alist.
	The comparison is done in first to last order.
	
(ev-pathname-register thing)
	Adds a new entry to *ev-backtranslation-alist* of the form (physical-path ev-path).
	If thing is a list it is interpreted as (physical-path ev-path).
	If thing is a string, the physical-path is obtained by calling (ev-pathname-translate thing)
	and ev-path is thing.
	In all cases, the physical pathnames are resolved to their true positions in the file hierarchy by calling
	TRUENAME, thereby insuring that symbolic file links are removed.
|#

;;; allow-redefinition is defined elsewhere in FREEDIUS

#+ev-pathname-standalone
(progn

#+cmu
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

) ; end #+ev-pathname-standalone progn


#+(or :allegro :sbcl)
(defmacro without-package-locks-lhq (&body body)
  `(without-package-locks ., body))


#+cmu
(progn

(defvar *enable-package-locked-errors-stack*)

(eval-when (eval compile load)
  (setq *enable-package-locked-errors-stack* nil)
  #-sbcl (setq lisp::*enable-package-locked-errors* nil)
  )

(defmacro without-package-locks-lhq (&body body)
  `(progn
    (eval-when (eval compile load)
      (push lisp::*enable-package-locked-errors* *enable-package-locked-errors-stack*)
      (setq lisp::*enable-package-locked-errors* nil))
    ,@body
    ;; This fails if compilation or loading of body causes an a error which is aborted.
    (eval-when (eval compile load)
      (setq lisp::*enable-package-locked-errors* (pop *enable-package-locked-errors-stack*)))))

(eval-when (compile eval load)
  (setq lisp::*enable-package-locked-errors* t))

) ; end #+cmu progn



;;; tests whether a pathname (or string) has an environment variable prefix, ie. $XXX/
(allow-redefinition

(defun ev-pathname-p (pathname)
  (cond ((pathnamep pathname)
	 (let* ((directory-list (pathname-directory pathname))
		(fstdir (cadr directory-list)))
	   (and (eq (car directory-list) :RELATIVE)
		(stringp fstdir)
		(> (length fstdir) 0)
		(char= (aref fstdir 0) #\$))))
	((stringp pathname)
	 (and (plusp (length pathname)) (char= (aref pathname 0) #\$)))))

;; This makes use of allegro logical pathname machinery when possible.  For
;; Allegro, we want a logical pathname in order for meta-dot to work right when
;; we are running a DISKSAVE AND the source file hierarchy has been moved.

(defun ev-to-logical-pathname (path)
  #+cmu path
  #+sbcl path
  ;;#+(or :allegro :quam)
  #+allegro
  (let* ((path2 (pathname path))
	 (dir (pathname-directory path2))
	 (fstdir (second dir)))               
    (if (and (eq (first dir) :relative)  ; these tests are same as ev-pathname-p
	     fstdir 
	     (> (length fstdir) 0)
	     (eql (aref fstdir 0) #\$))
	(let* ((hostname (subseq fstdir 1)))
	  (if (and (typep (pathname (format nil "~a:" hostname)) 'logical-pathname) 
		   (handler-case (logical-pathname-translations hostname) ; must have defined translations
		     (error () nil)))
	      (make-pathname :host hostname
		       :directory (list* #+allegro :relative #-allegro :absolute
					 (cddr dir))
		       :name (pathname-name path2)
		       :type (pathname-type path2))
	      path))
	path)))

) ; end allow-redefinition


#+old ; prior to use of SUBSTITUTE-IN-FILE-NAME 
(progn

(defvar *ev-cached-translations-alist* nil)

(defun set-ev-pathname-translation (env-var &optional translation)
  ;; FIXME? should this also do (setf (environment-variable env-var) translation)
  (let* ((translation 
	  (or translation 
	      (getenv env-var)
	      (error "set-ev-pathname-translation the environment-variable ~a does not exist" env-var)))
	 (entry (assoc env-var *ev-cached-translations-alist* :test #'string-equal)))
    (if entry
	(setf (cadr entry) translation)
	(push (list env-var translation) *ev-cached-translations-alist*))
    translation))

(allow-redefinition
 
(defun ev-pathname-translate (pathname &optional (error-if-no-translation t))
  (if (ev-pathname-p pathname)
      (let* ((path (pathname pathname))
	     (dir-list (pathname-directory path))
	     (env-var (if (cadr dir-list)
			  (subseq (cadr dir-list) 1)
			  (subseq (pathname-name path) 1))) ; strip off leading $
	     (ev-value (or (cadr (assoc env-var *ev-cached-translations-alist* :test #'string-equal))
			   (set-ev-pathname-translation env-var))))
	;;(format t "~a~%" (format nil "~a~a" expansion (subseq (namestring path) (1+ (length env-var)))))
	(if ev-value
	    (let* ((translated-pathname
		    ;; FIXME: Unix specific pathname merging
		    (pathname (format nil "~a~a"
				      ev-value (subseq (namestring path) (1+ (length env-var))))))
		   )
	      (remember-pathname-backtranslation pathname translated-pathname)
	      ;; should truename be returned?
	      translated-pathname)

	    ;; otherwise
	    (when error-if-no-translation
	      (error "The environment variable ~a is not defined." env-var))))
      pathname))
) ; end allow-redefinition

) ; end progn OLD


;;#+new using SUBSTITUTE-IN-FILE-NAME
(progn


(allow-redefinition

;;; problems in SBCL sb-c::verify-source-file in src/compiler/main.lisp
;;; namestring is broken in ceertain circumstances


#+never ;; broken
(defun ev-pathname-p (pathname)
  (cond ((typep pathname 'logical-pathname)
	 nil)
	((pathnamep pathname)
	 (handler-case (ev-pathname-p (namestring pathname))
	   (error () 
	     (let ((old-err *ev-pathname-p-error-path*))
	       (setq *ev-pathname-p-error-path* (list  pathname *default-pathname-defaults*))
	       (unless old-err(break)))
	     nil)))
	((stringp pathname)
	 (position #\$ pathname))))

(defun ev-pathname-p (pathname)
  (typecase pathname
    (string (position #\$ pathname))
    (logical-pathname nil)
    (pathname
     ;; This is needed because Allegro sometimes has pathnames that produce illegal namestrings.
     (flet ((evp (x) (and (stringp x) (position #\$ x))))
       (or (loop for name in (pathname-directory pathname)
		 thereis (evp name))
	   (evp (pathname-name pathname))
	   (evp (pathname-type pathname)))))))


) ; end allow-redefinition


(defparameter *ev-getenv-auto-import* :remember)

(defun ev-getenv (var-name &optional (auto-import *ev-getenv-auto-import*))
  (let ((symbol (intern var-name :env)))
    (if (boundp symbol)
	(symbol-value symbol)
	(when auto-import
	  (let ((val (getenv var-name)))
	    (when (and val (eq auto-import :remember))
	      (setf (symbol-value symbol) val))
	    val)))))

(defun (setf ev-getenv) (var-val var-name)
  (let ((symbol (intern (format nil "*~a*" var-name) :env)))
    (setf (symbol-value symbol)
	  var-val)))

(defun set-ev-pathname-translation (env-var &optional translation)
  (setf (ev-getenv env-var) translation))

;(NAMESTRING #P"$FREEDIUS_ARCH/lib/liblisptk.so")

;;; based on substitute-in-file-name in emacs/src/fileio.c
(defun substitute-in-file-name (f)
  (declare (type simple-string f))
  ;;(declare (optimize (speed 0) (safety 3) (debug 3)))
  ;;(declare (optimize (speed 3) (safety 1)))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (position #\$ f)
    (return-from substitute-in-file-name f))

  (loop with n fixnum = (length f)
	with jnext fixnum
	with j fixnum = 0
	with result = ""
	with ch of-type character
	for p = (position #\$ f :start j)
	while p
	do 
     (let ((p (1+ p)))			; scan past $
       (declare (fixnum p)) 
       (when (>= p n)
	 (error "SUBSTITUTE-IN-FILE-NAME: $ at end of ~a" f))
       (setq ch  (aref f p))
       (if (char= ch #\$) ;; $$ in filename
	   (setq result (concatenate 'string result (subseq f j (1+ p)))
		 j (1+ p))
	   (let* ((var-name (if (char= ch #\{)
				(let ((pc (position #\} f :start (1+ p))))
				  (unless pc 
				    (error "SUBSTITUTE-IN-FILE-NAME: Missing \"}\" in ~a" f))
				  (setq jnext (1+ pc))
				  (subseq f (1+ p) pc))
				(loop for i fixnum from p below n
				      for ch of-type character = (aref f i)
				      while (or ;;(alphanumericp ch)
					     (alpha-char-p ch)(digit-char-p ch)
					     (char= ch #\_))
				      finally 
				   ;;(setq jnext (min i (the fixnum (1- n))))
				   (setq jnext i)
				   (return (subseq f p i)))))
		  (var-val (ev-getenv var-name)))
	     (cond (var-val
		    (setq result (concatenate 'string result (subseq f j (1- p)) var-val)))
		   ((char= (aref f (1- jnext)) #\})
		    (error "SUBSTITUTE-IN-FILE-NAME: nonexistent environment variable: ~a" var-name))
		   (t (setq result (concatenate 'string result (subseq f j jnext))))
		   )
	     (setq j jnext))))
	finally (return (concatenate 'string result (subseq f j)))))

(defun ev-pathname-translate (pathname &optional (error-if-no-translation t))
  (if (ev-pathname-p pathname)
      (let ((translation (substitute-in-file-name (namestring pathname))))
	(if (and (ev-pathname-p translation) error-if-no-translation)
	    (error "ev-pathname-translate: Cannot find environment variable in ~a" translation)
	    translation))
      (namestring pathname)))

#+never
(defun ev-pathname-translate (pathname &optional (error-if-no-translation t))
  (if (ev-pathname-p pathname)
      (let ((translation (substitute-in-file-name (namestring pathname))))
	(if (and (ev-pathname-p translation) error-if-no-translation)
	    (error "ev-pathname-translate: Cannot find environment variable in ~a" translation)
	    translation))
      (namestring (if (typep (pathname pathname) 'logical-pathname)
		      (translate-logical-pathname pathname)
		      pathname))))


); end progn using SUBSTITUTE-IN-FILE-NAME

;;; *************************  PATHNAME BACKTRANSLATION  *************************

;;; This finds the physical-pathname specified by path.
;;; The subdirectory chain is truncated backwards until PROBE-FILE succeeds.
;;; The truncated portion of PATH is appended to the result of PROBE-FILE.
(defun extended-truename (path &optional (unique-p t))
  (let* ((pathname (pathname path))
	 (host (pathname-host pathname))
	 (directory-list (pathname-directory pathname)))
    ;; back up the directory tree until probe-file succeeds
    (loop for dirs = directory-list then (butlast dirs)
	  while dirs
	  for path2 = (make-pathname :host  host :directory dirs)
	  for truename = (probe-file path2)
	  when truename
	    do (when unique-p
		 ;; use pwd to get unique name for directory
		 (setq truename (true-directory-name truename))) 
	       (return-from extended-truename
		 (values (make-pathname
			  :host (pathname-host truename)
			  :directory (append (pathname-directory truename)
					     (nthcdr (length dirs) directory-list))
			  :name (pathname-name pathname)
			  :type (pathname-type pathname))
			 truename)))
    (error "No such pathname exists: ~a" path)))

(defvar *ev-backtranslation-hash-table* (make-hash-table :test #'equal))
(defvar *ev-automatic-remember-backtranslation* nil)

#|
(hash-table-count *ev-backtranslation-hash-table*)
(EV-PATHNAME-BACKTRANSLATE "/usr/local/cme/CME-6" :fail-action :return-nil)
(sort (loop for item being the hash-elements of *ev-backtranslation-hash-table*
	    collect (car item))
      'string-lessp :key 'namestring)
|#

;;; This list is compared with pathnames in first-to-last order.
;;; Therefore, the order of this list is important of the matching process.
;;; Probably need some smarts to either automatically reorder the list or
;;; to allow the user to specify partial ordering constraints.
(defvar *ev-backtranslation-alist* nil)

;;; THIS IMPLEMENTATION IS NOT VERY EFFICIENT and must be replaced if
;;; the length of backtranslation-alist gets very long.

(defun remember-pathname-backtranslation (pathname translated-pathname)
  (when *ev-automatic-remember-backtranslation*
    (let* ((truename (old-probe-file translated-pathname)))
      (when truename (setq truename (extended-truename translated-pathname)))
      ;;(setq foo1 (list pathname translated-pathname truename))(break)
      (when (and truename
		 (not (ev-pathname-backtranslate truename :fail-action :return-nil)))
	;;(when (pathnamep pathname) (break))
	(pushnew (namestring pathname)
		 (gethash (namestring truename) *ev-backtranslation-hash-table*)
		 :test #'equal)))))


;;(defparameter *ev-pathname-backtranslate-default-fail-action* :warn)
(defparameter *ev-pathname-backtranslate-default-fail-action* nil)

(allow-redefinition
(defun ev-pathname-backtranslate (pathname &key (backtranslation-alist *ev-backtranslation-alist*)
					   relative-to
					   (fail-action *ev-pathname-backtranslate-default-fail-action*)
					   )
  (if (ev-pathname-p pathname)
      pathname
      (let* ((truename (extended-truename pathname))
	     (pathstring (namestring truename)))
	(or (let ((back-paths (gethash pathstring *ev-backtranslation-hash-table*)))
	      (when back-paths
		(or (and relative-to
			 (loop for back-path in back-paths
			       with len = (length relative-to)
			       when (string= relative-to back-path :end2 len)
				 return back-path))
		    (first back-paths)
		    )))
	      
	    (and backtranslation-alist
		 (loop with path-length = (length pathstring)
		       for thing in backtranslation-alist
		       for look-for = (if (consp thing)
					   (car thing)
					   (namestring (truename (getenv thing))))
		       for replace-with = (if (consp thing)
					      (cadr thing)
					      thing)
		       for look-for-length = (length look-for)
		       ;;do (format t "~s ~s" look-for (subseq pathstring 0 look-for-length))
		       when (> path-length look-for-length)
			 do (let ((subseq (subseq pathstring 0 look-for-length)))
			      (when (and (string= look-for subseq)
					 (or (= (length subseq) (length pathstring))
					     (eql (aref look-for (1- look-for-length))
						  #\/)
					     (eql (aref pathstring look-for-length)
						  #\/)))
				;; FIXME: Unix specific pathname merging
				(return (pathname (format nil "~a~a"
							  replace-with
							  (subseq pathstring look-for-length))))))))
	    (case fail-action
	      (:warn (format t ";;; WARNING: EV-PATHNAME-BACKTRANSLATE failed to backtranslate ~a~%"
			     pathname)
		     ;;#-agl (break)
		     ;;(setq foo (list pathname backtranslation-alist truename pathstring)) (break)
		     pathname)
	      (:error (cerror "Return physical pathname"
			      "EV-PATHNAME-BACKTRANSLATE failed to backtranslate ~a"
			      pathname)
		      pathname)
	      (:return-nil nil)
	      (otherwise pathname))))))
) ; end allow-redefinition

(defun strip-trailing-slash (path)
  (loop for pos from (1- (length path)) downto 0
        while (eql (aref path pos) #\/)
        finally (return (subseq path 0 (1+ pos)))))

;; FIXME: Unix specific pathname merging
(defun force-trailing-slash-consistancy (path1 path2)
  (setq path1 (namestring path1)
	path2 (namestring path2))
  (let ((trailing-slash2 (eql (aref path2 (1- (length path2))) #\/)))
    (if (eql (aref path1 (1- (length path1))) #\/)
	(if trailing-slash2
	    path2
	    (format nil "~a/" path2))
	(if trailing-slash2
	    (strip-trailing-slash path2)
	    path2))))
     
(defun ev-pathname-register (ev-path-or-backtranslation)
  (let ((phys-path (if (consp ev-path-or-backtranslation)
		       (car ev-path-or-backtranslation)
		       ev-path-or-backtranslation))

	(ev-path (if (consp ev-path-or-backtranslation)
		     (cadr ev-path-or-backtranslation)
		     ev-path-or-backtranslation)))
    
    (if (ev-pathname-p ev-path)
	(let* ((trans-path (ev-pathname-translate phys-path)))
	  (set-ev-pathname-translation (subseq ev-path 1) trans-path)
	  (set-ev-backtranslation trans-path ev-path))
	(format t ";;; Warning EV-PATHNAME-REGISTER ignored registration of ~a~%" ev-path)
	)))

;;; Because of symbolic-links and automounting, this is the only way to
;;; get a unique name for a given directory.  
(defun true-directory-name (path)
  (let ((old-directory (pwd))
	(path2 (probe-file path)))
    (if path2
	(unwind-protect
	     (progn (cd path)
		    (pwd))
	  ;; cleanup form
	  (cd old-directory))
	path2)))

(defun set-ev-backtranslation (phys-path ev-path)
  (when (probe-file phys-path)
    (setq phys-path (true-directory-name phys-path))) ; force unique directory name
  (setq phys-path (force-trailing-slash-consistancy ev-path phys-path))
  
  (unless (loop for entry in *EV-BACKTRANSLATION-ALIST*
		for (phys ev) = entry
		do (ignore phys)
		when (equal ev ev-path)
		  do (setf (car entry) phys-path)
		  and return t)
    (push (list phys-path ev-path) *EV-BACKTRANSLATION-ALIST*)))

;;; This is called when starting up a disksaved executable to
;;; fix  cached pathname backtranslations.
(defun disksave-startup-update-pathname-translations ()
  (loop for entry in *EV-BACKTRANSLATION-ALIST*
	for (phys ev) = entry
	for new-phys = (namestring (extended-truename (ev-pathname-translate ev)))
	do (ignore phys)
	   (setf (car entry) new-phys)
	#+never (unless (equal phys new-phys)
		  (format t "translation of ~a changed from ~a to ~a~%" ev phys new-phys))))

(defun real-filename (filename)
  (ev-pathname-translate filename))

;;; Redefinition  of function in logical-pathnames.lisp to extend to translation of ev-pathnames.
;;; Be very careful that everything needed is defined before making this definition



;;; I need to use ev-pathnames at compile-time
(eval-when (compile load eval)


;;; LHQ (Sun Mar 20 1994): Changed to compile the new functions.
(defmacro convert-file-function (name &optional optionalp)
  (let ((old-name (intern (concatenate 'string "OLD-" (string name))))
	(new-name (intern (concatenate 'string "LP-" (string name)))))
    `(progn
      (unless (fboundp ',old-name)
	(declaim (type (function ,(if optionalp '(&optional t &rest t) '(&rest t)) t) ,old-name))
	(setf (symbol-function ',old-name) (symbol-function ',name)))
      
      (defun ,new-name ,(if optionalp 
			    '(&optional filename &rest args)
			    '(filename &rest args))
	,(if optionalp
	     `(if filename
	       (apply #',old-name (real-filename filename) args)
	       (funcall #',old-name)
	       ;;(,old-name)		; instead of (funcall #',old-name)
	       )
	     `(apply #',old-name (real-filename filename) args)))
      (setf (symbol-function ',name) (symbol-function ',new-name))
      )))

;;; LHQ (Sun Mar 20 1994): Changed to compile the new functions.
(defmacro convert-file-function-2-args (name)
  (let ((old-name (intern (concatenate 'string "OLD-" (string name))))
	(new-name (intern (concatenate 'string "LP-" (string name)))))
    `(progn
      (unless (fboundp ',old-name)
	(declaim (type (function (t t &rest t) t) ,old-name))
	(setf (symbol-function ',old-name) (symbol-function ',name)))
      
      (defun ,new-name (filename1 filename2 &rest args)
	(apply #',old-name
	       (real-filename filename1)(real-filename filename2)
	       args))
      (setf (symbol-function ',name) (symbol-function ',new-name))
      ;;(fix-arglist ',name)
      )))

) ; end eval-when

;; this stuff also defined in lisp/logical-pathnames.lisp

(without-package-locks-lhq
(convert-file-function lisp::load)
(convert-file-function lisp::open)
(convert-file-function lisp::probe-file)
(convert-file-function lisp::delete-file)
(convert-file-function lisp::truename)
(convert-file-function lisp::directory)
(convert-file-function lisp::dribble t)
#+allegro (convert-file-function lisp::ed t)
(convert-file-function lisp::file-author)
(convert-file-function lisp::file-write-date)
(convert-file-function cd t)
(convert-file-function cl:ensure-directories-exist t)
;;(convert-file-function lisp::namestring)

(convert-file-function-2-args lisp::rename-file)
;; should take care of :output-file as well
(convert-file-function lisp::compile-file)			  

) ; end without-package-locks


#|   Damn SBCL DIRECTORY FUNCTION is totally different form CMUCL and Allegro

(directory "/tmp/directory-test/") returns 
on SBCL returns truenames and follows symbolic links
        without wildcards (#P"/tmp/directory-test/") 

        (directory "/tmp/directory-test/**")
        (directory (make-pathname :directory "/tmp/directory-test/" :name :wild :type :wild ))
on Allegro returns all of the files in directory (except for dot files) with subdirs as file
          (ie .without trailing slash)
          returns truenames and follows symbolic links

on CMUCL returns all of the files in directory (except for dot files) with subdirs as directories
          (ie .with trailing slash)
          returns truenames and follows symbolic links by default
|#

#+sbcl

(without-package-locks-lhq
(defun directory (filename &rest args &key &allow-other-keys)
  (let ((path (pathname (real-filename filename))))
    (when (and (null (pathname-name path)) (null (pathname-type path)))
      (setq path (make-pathname :defaults path :name :wild :type :wild)))
    (apply 'old-directory path args)))

) ; end without-package-locks

#|
(after-logical-pathnames-loaded)
(setf (symbol-function 'lisp::parse-namestring) (symbol-function 'lp::old-parse-namestring))
|#



#| ; restore original defn.
(setf (symbol-function 'merge-pathnames) (symbol-function 'old-merge-pathnames))
|#

;;; The presumption is that if pathname is a logical-pathname it resolves to an
;;; ABSOLUTE pathname.  Therefore, the host and directory part of the result of
;;; merge-pathnames must come from pathname.  

(unless (fboundp 'old-merge-pathnames)
  (setf (symbol-function 'old-merge-pathnames) (symbol-function 'merge-pathnames)))

(defun pathname-components (pathname)
  (setq pathname (pathname pathname))
  (values :physical-pathname
	  (pathname-host pathname)
	  (pathname-directory pathname)
	  (pathname-name pathname)
	  (pathname-type pathname)
	  (pathname-version pathname)))

#|
;;; must install it in this manner
(setf (symbol-function 'merge-pathnames) (symbol-function 'new-merge-pathnames))

(pathname "$FREEDIUS/lisp/.lisp")
(new-merge-pathnames (make-pathname :defaults "$FREEDIUS/lisp/.lisp")
		     (make-pathname :defaults "foo" :version 1))

(new-merge-pathnames "$FREEDIUS/lisp/.lisp" (make-pathname :defaults "foo" :version 1))
(new-merge-pathnames "foo.lisp" "$FREEDIUS/lisp/lisp/")
|#

(defun new-merge-pathnames
    (pathname &optional
	      (defaults *default-pathname-defaults*)
	      (default-version :newest) ; this is required for compatibility with CL
	      )
  #+never
  (setq pathname (pathname pathname)
        defaults (pathname (or defaults (error "defaults is NIL")))) ; BE CAREFUL -- IS THIS ANSI?
  (cond ((ev-pathname-p pathname)
         (if (and (pathname-name pathname) (pathname-type pathname))
             pathname
             (make-pathname :directory (pathname-directory pathname)
                            :name (or (pathname-name pathname) (pathname-name defaults))
                            :type (or (pathname-type pathname) (pathname-type defaults))
                            :version (or (pathname-version pathname) (pathname-version defaults)
                                         default-version)
                            )))
        ((ev-pathname-p defaults)
         (if (eq (car (pathname-directory pathname)) :root)
             (funcall 'old-merge-pathnames pathname defaults default-version)
             (make-pathname :directory (append (pathname-directory defaults)
                                               (cdr (pathname-directory pathname)))
                            :name (or (pathname-name pathname) (pathname-name defaults))
                            :type (or (pathname-type pathname) (pathname-type defaults))
                            :version (or (pathname-version pathname) (pathname-version defaults)
                                         default-version))))        
        (t (funcall 'old-merge-pathnames pathname defaults default-version))))

#| ; this ordinarly doesn't happen until AFTER-LOGICAL-PATHNAMES-LOADED is called 
(setf (symbol-function 'merge-pathnames) (symbol-function 'old-merge-pathnames))
(setf (symbol-function 'merge-pathnames) (symbol-function 'new-merge-pathnames))
|# 
 
#|
(load (compile-file "$FREEDIUS/lisp/lisp/tst.lisp"))
(real-filename "$FREEDIUS/lisp/lisp/tst.lisp")
(merge-pathnames "$FREEDIUS/lisp/ic/foo" "~quam/bar.lisp"); #P"$FREEDIUS/lisp/ic/foo.lisp"
(merge-pathnames "~quam/.lisp" "$FREEDIUS/lisp/ic/foo")   ; #P"/home/clam1/quam/foo.lisp"
(merge-pathnames "~quam/.lisp" "$FREEDIUS/lisp/ic/foo")   ; #P"/home/clam1/quam/foo.lisp"
(merge-pathnames "bar.lisp" "$FREEDIUS/lisp/ic/foo")      ; #P"$FREEDIUS/lisp/ic/bar.lisp"
(merge-pathnames "garply/bar.lisp" "$FREEDIUS/lisp/ic/foo");#P"$FREEDIUS/lisp/ic/garply/bar.lisp"
(merge-pathnames "bar.lisp" "~quam/.lisp")          ; #P"/home/clam1/quam/bar.lisp"
(merge-pathnames "garply/bar.lisp" "~quam/.lisp")          ; #P"/home/clam1/quam/garply/bar.lisp"
(merge-pathnames "~quam/.lisp" "garply/bar.lisp")          ; #P"/home/clam1/quam/bar.lisp"
(pathname "garply/bar.lisp")
(pathname "$FREEDIUS/lisp/ic/foo")
(pathname "$FREEDIUS/lisp/lisp/tst.dsbin"); relative
(pathname2 "$FREEDIUS/lisp/lisp/tst.dsbin")
(pathname "~quam/bar.lisp") 
(pathname "~quam") ; relative
(pathname "~quam/") ; absolute
(pathname "foo::/bar/zap.lisp")
(setq path (make-pathname :defaults "~quam/unspecific.lisp" :name :unspecific))
|#




(defun use-old-merge-pathname ()
  (without-package-locks 
   (when (fboundp 'old-merge-pathnames)
     (setf (symbol-function 'merge-pathnames)
	   (symbol-function 'old-merge-pathnames)))))

(defun use-new-merge-pathname ()
  (without-package-locks
   (when (fboundp 'new-merge-pathnames)
     (setf (symbol-function 'merge-pathnames)
	   (symbol-function 'new-merge-pathnames)))))

(defmacro with-old-merge-pathnames (&body body)
  `(unwind-protect
       (progn (when (fboundp 'use-old-merge-pathname) (use-old-merge-pathname))
	      .,body)
    (when (fboundp 'use-new-merge-pathname) (use-new-merge-pathname))))

;;;
;;; This exists SOLELY to allow us to concatenate fasl files.  Not
;;; sure that allegro defsystem supports parallel bindirs, so this is
;;; a hack to point to the fasl files directly.

#+allegro
(progn

(defun keywordify (string &optional case-sensitive)
  (intern (if case-sensitive string (string-upcase string))
	  :keyword))

(defun define-shadow-system (name default-pathname other-args)
  (with-old-merge-pathnames
      (destructuring-bind (&rest ignore &key files default-binary-pathname &allow-other-keys)
	  other-args
	(declare (ignore default-binary-pathname))
	(DEFSYSTEM:DEFSYSTEM-1 (keywordify name)
	    (list :default-pathname
		  (compile-object-file-pathname
		   #-allegro (merge-pathnames "*.*" default-pathname)
		   ;; Apparently, allegro 8.x can't tolerate wildcards here:
		   #+allegro default-pathname
		   ))
	  (list (cons :serial
		      (loop for f in files collect (pathname-name f))))))))

) ; end #+allegro progn


(without-package-locks-lhq
  (setf (symbol-function 'merge-pathnames)
	(symbol-function 'new-merge-pathnames))
  )

(ev-pathname-register "$FREEDIUS" )
(ev-pathname-register (list *freedius-exec-prefix* "$FREEDIUS_ARCH"))



