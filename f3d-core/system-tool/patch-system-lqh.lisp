(in-package :system-tool)

;;; Lynn Quam's support for a patch system based on $FREEDIUS/emacs/patch-system.el.

;;; Extensions to standard SYSTEM-TOOL machinery.

;;; (defun find-system-for-file (target-filename)
;;;   (let ((target-length (length target-filename)))
;;;     (flet ((file-match (filename)
;;; 	     (let* ((filename-length (length filename))
;;; 		    (start (- target-length filename-length)))
;;; 	       (and (>= start 0) 
;;; 		    (<= (+ start filename-length) target-length)
;;; 		    (string= target-filename filename :start1 start)))))
;;;       (loop for sys in *all-systems*
;;; 	    when (loop for system-file in (system-files sys)
;;; 		       thereis (file-match system-file))
;;; 	      return sys))))

;;; (defun find-system-for-file (target-filename)
;;;   (let ((target-length (length target-filename)))
;;;     (flet ((file-match (filename)
;;; 	     (let* ((filename-length (length filename))
;;; 		    (start (- target-length filename-length)))
;;; 	       (and (>= start 0) 
;;; 		    (<= (+ start filename-length) target-length)
;;; 		    (string= target-filename filename :start1 start)))))
;;;       (loop for sys in *all-systems*
;;; 	    when (loop with default-path = (system-default-pathname sys)
;;; 		       for system-file in (system-files sys)
;;; 		       thereis (file-match (namestring (merge-pathnames system-file default-path))))
;;; 	      return sys))))

;;; The best way to guarantee that FIND-SYSTEM-FOR-FILE works properly is
;;; to consistently use ev-pathnames for the SYSTEM-DEFAULT-PATHNAME,
;;; and to use that same ev-pathnames to access the files in Emacs.
;;; FIND-SYSTEM-FOR-FILE will fail if SYSTEM-DEFAULT-PATHNAME is not a
;;; logical-pathnamne or an ev-pathname and if the host filesystem of
;;; target-filename is organized differently from where the "disksave"
;;; is built.
(defun find-system-for-file (target-filename)
  (setq target-filename (namestring (ev-pathname-backtranslate target-filename)))
  (let ((target-length (length target-filename)))
    (loop for sys in *all-systems*
	  when (loop with default-path = (system-default-pathname sys)
		     for system-file in (system-files sys)
		     thereis (string= (namestring (merge-pathnames system-file default-path))
				    target-filename))
	    return sys)))

; (find-system-for-file "$FREEDIUS/lisp/img/image-defs.lisp")
; (find-system-for-file "/opt/IU/FREEDIUS/default/lisp/img/image-defs.lisp")


#| not clear that this is useful(defvar *all-patch-info* nil)

(defun patch-info-internal (l)
  (declare (ignorable l))
  (push  `(:patch-file ,(and *load-pathname* (namestring *load-pathname*))
		,@l)
	 *all-patch-info*))
|#

(defun patch-info-internal (l)
  (declare (ignorable l)))

(defmacro patch-info (&rest key-vals)
  `(patch-info-internal ',key-vals))

(export '(patch-info))

#|
(load "/tmp/freedius-patches/FREEDIUS-103.lisp")
(make-patch-system (find-system-named :image) :patch-directory "/tmp/freedius-patches/")
(patch-system-name-for-file "/opt/IU/FREEDIUS/default/lisp/image-defs.lisp")
(find-system-for-file "/opt/IU/FREEDIUS/default/lisp/img/image-defs.lisp")
(find-system-for-file "$FREEDIUS/lispimg//image-defs.lisp")
(system-patch-directory (patch-system (find-system-named :image)))

(loop with patch-system = (make-instance 'patch-system2 :name "FREEDIUS"
			       :patch-directory "/tmp/freedius-patches/")
      for sys in *all-systems*
      do (setf (patch-system sys) patch-system))

(car *all-patch-systems*)
(push (patch-system (find-system-named :image)) *all-patch-systems*)
|#

(defvar *all-patch-systems* nil)

(defclass basic-patch-system ()
    ((patch-directory :initform nil :initarg :patch-directory :accessor patch-system-patch-directory)
     (level :initform 0 :initarg :level :accessor patch-system-level)))

(defclass patch-system (basic-patch-system)
    ((parent :initform nil :initarg :parent :accessor patch-system-parent)))

(defclass patch-system2 (basic-patch-system)
    ((name :initarg :name :accessor patch-system-name)
     ))

(defmethod initialize-instance :after ((o basic-patch-system) &key parent)
  (with-slots (patch-directory level) o
    (push o *all-patch-systems*)
    (when (and (null patch-directory) parent)
      (setq patch-directory 
	    (let ((default-pathname (system-default-pathname parent))
		  (patch-directory-name (format nil "V~a" level)))
	      (make-pathname :name nil
			     :type nil
			     :host (pathname-host default-pathname)
			     :device (pathname-device default-pathname)
			     :directory (append (pathname-directory default-pathname)
						(list "patches" patch-directory-name))))))))

(defun find-patch-system-named (name)
  (find name *all-patch-systems* :key #'patch-system-name :test #'string=))

;(fmakunbound 'make-patch-system)
;;; (defmethod make-patch-system ((system simple-lisp-system) &rest initargs)
;;;   (with-slots (name default-pathname patchable default-package patch-system) system
;;;     (setq patch-system
;;; 	  (apply 'make-instance 'patch-system :parent system initargs))))

(defmethod patch-level ((system basic-patch-system))
  (patch-system-level system))

(defmethod patch-directory-name ((system simple-lisp-system))
  (patch-directory-name (patch-system system)))

(defmethod patch-directory-name ((system patch-system))
  "V0")

;;; (defmethod system-patch-directory ((system patch-system))
;;;   (let ((default-pathname (patch-system-patch-directory system)))
;;;     (make-pathname :name nil
;;; 		   :type nil
;;; 		   :host (pathname-host default-pathname)
;;; 		   :device (pathname-device default-pathname)
;;; 		   :directory (append (pathname-directory default-pathname)
;;; 				      (list "patches"  (patch-directory-name system))))))

(defmethod system-patch-directory ((system patch-system))
  (or (patch-system-patch-directory system)
      (system-patch-directory (patch-system-parent system))))
  
;;; (defun patch-system-name-for-file (target-filename)
;;;   (let ((sys (find-system-for-file target-filename)))
;;;     (and sys (system-name sys))))

(defun patch-system-name-for-file (target-filename)
  (let ((sys (find-system-for-file target-filename)))
    (and sys (patch-system-name (patch-system sys)))))

(defun system-info-for-emacs (system-name)
  (let* ((system (find-system-named system-name))
	 (patch-system (and system (patch-system system))))
    (when patch-system
      (list (patch-system-level patch-system)
	    (namestring (patch-system-patch-directory patch-system))
	    (system-default-package system)))))

;;(patch-system-patch-directory "FREEDIUS")
;;(patch-system-patch-directory-and-version "FREEDIUS")
;;(find-patch-system-named "FREEDIUS")
;(fmakunbound 'patch-system-patch-directory)
(defmethod patch-system-patch-directory ((patch-system-name string))
  (let* ((patch-system (find-patch-system-named patch-system-name)))
    (when patch-system
      (namestring (patch-system-patch-directory patch-system)))))

(defmethod patch-system-patch-directory-and-version ((patch-system-name string))
  (let* ((patch-system (find-patch-system-named patch-system-name)))
    (when patch-system
      (list (namestring (patch-system-patch-directory patch-system))
	    (patch-system-level patch-system)))))

(defun filename-backtranslate (filename)
  (namestring (ev-pathname-backtranslate filename)))

#+never
(defun patch-system-name-for-file (target-filename)
  "FREEDIUS")

(defmethod patch-index-pathname ((system simple-lisp-system))
  (merge-pathnames (format nil "~a.patch-index" (system-name system))
		   (system-patch-directory system)))

(defmethod read-patch-index ((system simple-lisp-system))
  (with-open-file (st (patch-index-pathname system))
    (read st) ; skip over the header form
    (loop for entry = (read st nil nil)
	  while entry
	  collect entry)))

;(read-patch-index (find-system-named "IMAGE"))

(defmethod load-system-patches ((system simple-lisp-system))
  (let ((patch-index (read-patch-index system)))
    (maybe-compile-file-load
     (loop for (patch-num state creation-time who-done-it comment) in patch-index
	   when (eql state :finished)
	     collect (merge-pathnames 
		      (format nil "~a-~d.lisp" (system-name system) patch-num)
		      (system-patch-directory system))
	   until (eql state :incomplete) ; stop when an incomplete patch is seen
	   ))))
