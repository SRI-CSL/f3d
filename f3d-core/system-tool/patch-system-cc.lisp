(in-package :system-tool)

;;; **************************  System-tool patch facility **************************
;;;
;;; Per-system patch facility that accomodates major versions and
;;; patch levels.  Still not entirely clear when we need this in the
;;; modern age, but isolated (embedded) systems and binary-only
;;; systems should be easier to patch this way.  This is also designed
;;; to work with SLIME by allowing add-patch and finish-patch, etc. to
;;; work in Emacs.

;;; Not sure where these should be.  These use the parent system to
;;; determine patch information:

(defmethod patch-directory-name ((system simple-lisp-system))
  (format nil "~d" (or (system-version system) 0)))

(defmethod system-patch-directory ((system simple-lisp-system))
  (with-slots (default-pathname) system
    (make-pathname :name nil
		   :type nil
		   :host (pathname-host default-pathname)
		   :device (pathname-device default-pathname)
		   :directory (append (pathname-directory default-pathname)
				      (list "patches"  (patch-directory-name system))))))

(defmethod system-patch-files ((system simple-lisp-system ))
  (let ((patch-index (merge-pathnames "system-patches.index" (system-patch-directory system))))
    (when (probe-file patch-index)
      (with-open-file (str patch-index :direction :input)
	(read str)))))

;;;
;;; Another crack at patches.  Looks a bit like the old lispm patch
;;; systems.  Probably needs more work.
;;;
;;; This goes outside the basic-system hierarchy.  The problem with
;;; building patch systems on top of basic-system is that it's
;;; inherently recursive, with patch-systems in theory being patchable
;;; themselves.  Also, regular systems are pushed onto the
;;; *all-systems* list.  This seems bad for patches.

(defclass patch-system ()
    ((parent :initform nil :initarg :parent :accessor patch-system-parent)
     (patches :initform nil :initarg :patches :accessor patch-system-patches)
     (level :initform 0 :initarg :level :accessor patch-system-level)
     (open-patches :initform nil :accessor open-patches)))

(defmethod patch-level ((system patch-system))
  (patch-system-level system))

(defmethod major-version ((system patch-system))
  (with-slots (version) (patch-system-parent system)
    version))

(defmethod patch-level ((system simple-lisp-system))
  (let ((p-system (patch-system system)))
    (if p-system
	(patch-level p-system)
	0)))

(defmethod system-patch-directory ((system patch-system))
  (system-patch-directory (patch-system-parent system)))
  
(defmethod print-object ((system patch-system) stream)
  (print-unreadable-object (system stream :type t :identity nil)
    (format stream "~a" (patch-system-parent system))))


(defmethod revert-patch-system ((sys patch-system))
  (setf (patch-system-patches sys)
	(system-patch-files (patch-system-parent sys)))
  (setf (patch-system-level sys)
	(or 
	 (loop for (num file comment) in (patch-system-patches sys)
	       maximize num)
	 0)))


(defmethod initialize-instance :after ((sys patch-system) &rest args &key parent &allow-other-keys)
  (revert-patch-system sys))

(defmethod make-patch-system ((system simple-lisp-system))
  (with-slots (name default-pathname patchable default-package patch-system) system
    (setq patch-system
	  (make-instance 'patch-system
			 :parent system))
    patch-system))



(defmethod load-system-patches ((system patch-system ))
  (let ((patch-dir (system-patch-directory system))
	(parent-name (system-name (patch-system-parent system))))
    (maybe-compile-file-load
     (loop for (num comment file) in (patch-system-patches system)
	   do (format t "~%System ~a patch ~d: ~a" parent-name num comment)
	   collect (merge-pathnames file patch-dir)))))
  

(defclass system-patch ()
    ((package :initform :cl-user :initarg :package :accessor system-patch-package)
     (comment :initform "None" :initarg :comment :accessor system-patch-comment)
     (body    :initform "()" :initarg :body :accessor system-patch-body)))


;;; No explicit start-patch is necessary.  We can add patches to start
;;; the process.
;;;  (defmethod start-patch ((system patch-system))


(defun maybe-make-patch-system (sys)
  (or (patch-system sys)
      (make-patch-system sys)))

(defmethod add-patch (patch-package comment patch-body-string (system simple-lisp-system))
  (add-patch patch-package comment patch-body-string (patch-system system)))
    
(defmethod add-patch (patch-package comment patch-body-string (system (eql nil)))
  (error "System is not patchable."))

(defun find-system-for-file (filename)
  (loop for sys in *all-systems*
	when (member (pathname-name filename)
		     (system-files sys) 
		     :test #'(lambda (x y) (string= x (pathname-name y))))
	  do (return sys)))

(defmethod add-patch (patch-package comment patch-body-string (file pathname))
  (add-patch patch-package comment patch-body-string (find-system-for-file file)))

(defmethod add-patch (patch-package comment patch-body-string (file string))
  (add-patch patch-package comment patch-body-string (find-system-for-file file)))


(defmethod add-patch (patch-package comment patch-body-string (system patch-system))
  (setf (open-patches system)
	(cons (make-instance 'system-patch
			     :package patch-package
			     :comment comment
			     :body patch-body-string)
	      (open-patches system))))

(defun slime-add-patch (patch-package comment patch-body-string system)
  (length (add-patch patch-package comment patch-body-string system)))
  


(defmethod finish-patches ((file string))
  (finish-patches (find-system-for-file file)))

(defmethod finish-patches ((system simple-lisp-system))
  (finish-patches (patch-system system)))

(defmethod finish-patches ((system (eql NIL)))
  nil)



(defmethod finish-patches ((system patch-system))
  (if (null (open-patches system))
      "No patches to finish."
      (let* ((level (incf (patch-system-level system)))
	     (new-patch-file (format nil "patch-~d.lisp" level)))
	(create-directory (system-patch-directory (patch-system-parent system)))
	(setf (patch-system-patches system)
	      (append (patch-system-patches system)
		      (list (list level
				  (format nil "New patch created by ~a on ~a"
					  (user-email-address)
					  (print-universal-date (get-universal-time) NIL))
				  new-patch-file))))
	(with-open-file (str (merge-pathnames "system-patches.index" (system-patch-directory (patch-system-parent system)))
			     :direction :output
			     :if-exists :rename)
	  (write (patch-system-patches system)
		 :stream str))

	(with-open-file (str (merge-pathnames new-patch-file
					      (system-patch-directory (patch-system-parent system)))
			     :direction :output)
	  (loop for patch in (reverse (open-patches system))
		do (format str "~%(IN-PACKAGE ~A)~%" (system-patch-package patch))
		   (format str "~%(ST::PATCH-COMMENT ~s)~%" (system-patch-comment patch))
		   (format str "~%~a~%" (system-patch-body patch))))
	(setf (open-patches system) nil)
	)))
  


(defmethod abort-patches ((system string))
  (abort-patches (find-system-for-file system)))


(defmethod abort-patches ((system simple-lisp-system))
  (abort-patches (patch-system system)))

(defmethod abort-patches ((system (eql NIL)))
  nil)

(defmethod abort-patches ((system patch-system))
  (setf (open-patches system) nil))

(defvar *print-patch-comments* t)

(defmacro patch-comment (string)
  `(eval-when (load)
     (when st::*print-patch-comments*
       (print ,string))))

