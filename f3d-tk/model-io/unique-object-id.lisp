(in-package :freedius-io)

#|
(st:load-system :model-io :recompile t)
|#

#|
Tue Sep 28 2004

My current thinking is that persistent object storage must consist of 
two parts:

   A.  Object geometry

       .  object class, geometry, and rendering properties.

       .  coordinate-system and coordinate-transform

   B.  Object relationships.

       . feature-set membership

Object geometry is straight forward to represent.  Object geometry can be stored
in either an ODB or simple text files, with one or many objects stored per file.

Simple object relations, such as set membership (feature-sets), can easily be
represented using text files, but more complicated relations are better handled
using a general purpose relational database system.

Persistent storage of object relationships requires that every persistent object
have a "unique object id" (UID).  When everything is stored in a single file,
simple sequence numbers may be used for the UIDs.  UID generation become more
complicated when objects are stored in multiple files and created and accessed
by multiple, independent users, located in multiple sites.

From the following sections, it should be possible to create an extensible,
object-oriented implementation for UID generation and object access. 

1.  Abstract Specification of the Properties of UIDs:

  The UID generation scheme must deal with the following problems:

   .  Any persistent object must have a UID.

   .  IUDs must be globally unique.

   .  IUDs are persistent.  Ie. the UID of an object never changes.

   .  It must be possible to merge objects from independently generated "databases", 
      retaining their UIDs.

   .  It must be possible to retrieve an object from persistent storage given
      its UID.
   

  It would be useful if UIDs also had the following properties:

   .  The UID can be decomposed into one or more keys that aid in retreiving the
      object.  A particularly useful key would the (approximate) geographic
      location of the object.  Is is assumed that any non-trivial geographic database
      must be able to be partitioned into geographic cells, such as lat-long buckets.

   .  CC 10/3/2005: Some provision should be made for temporal
      decomposition of the UID.  Example 1: an object whose state can change
      over time.  Example 2: multiple objects of the same type that are
      tracked at different times within the same spatial region.



2.  UID Generation API

    UIDs are objects with appropriate read and print methods.

    UID-generators are objects stored in a database or file-system with a
    locking mechanism to guarantee that unique objects ids are generated
    even when multiple processes are making calls to GENERATE-UID.
    
    (MAKE-UID-GENERATOR class . initargs) => UID-generator

       Creates and returns a UID-generator.  What args should allowed?
    
    (OPEN-UID-GENERATOR generator-path) => UID-generator

       Opens and returns an existing UID-generator.  GENERATOR-PATH is implementation
    dependent and may a (partial) pathname in the filesystem or the name of the UID generator
    in the database.  In either case, GENERATOR-PATH is (probably) a string.

    It appears that OPEN-UID-GENERATOR and MAKE-UID-GENERATOR are implementation dependent
    and therefore it is not possible to completely specify their arguments.

    (WITH-UID-GENERATOR UID-generator &rest body)
    
       Performs the following steps:

          1. Obtains the lock for UID-generator
 
          2. Updates the UID-generator from the global state

          3. Executes body 

          4. Updates the global state of the UID-generator

          5. Releases the lock for UID-generator.

    (GENERATE-UID UID-generator object) => UID

       Generates and returns a UID for object in UID-context.  

       The UID-generator is permitted to use any properties of object, such as
       its geographic location, in the UID generation process.

3.  Spatial Object Access from UID

    (GET-OBJECT-FROM-UID UID &key load-all) => object or NIL

       Returns the object referenced by UID.  It is presumed that there is a
       hashtable relating UIDs to objects in the running process.  If IUD is not
       in the hashtable, the object is loaded from the persistent storage
       (either an ODB or the file system), added to the hashtable, and returned.

       If the persistent store is the file system, and multiple objects are
       stored per file, then LOAD-ALL specifies whether to load all of the
       object definitions contained in the file.  This is an option for
       performance only.


|#



(defclass uid-generator () 
    ((have-lock :initform nil :accessor have-lock)
  ))

(defgeneric generate-uid (uid-generator object))

(defgeneric lock-uid-generator (uid-generator &key wait))

(defgeneric unlock-uid-generator (uid-generator lock))


(defmacro with-uid-generator (uid-generator &body body)
  `(let* ((uid-generator ,uid-generator)
	 (lock (lock-uid-generator uid-generator)))
    (unwind-protect 
	 (progn . ,body)
      (unlock-uid-generator uid-generator lock))))

(defun make-uid-generator (class &rest initargs)
  (apply #'make-instance class initargs))

#|
(defun open-uid-generator (class generator-path) 
 
  ) 
|#

(defclass filesystem-uid-generator (uid-generator) 
    ((pathname :initarg :pathname :accessor uid-generator-pathname) ; the name of the lock file
     (stream :accessor uid-generator-stream)
     (uid-counter :initform 0 :accessor uid-counter)
     (uid-prefix :initarg :uid-prefix :accessor uid-prefix)
     ))

(defmethod initialize-instance :after ((uid-generator filesystem-uid-generator) 
				       &key &allow-other-keys)
  (unless (probe-file (uid-generator-pathname uid-generator))
    (unless (slot-boundp uid-generator 'uid-prefix)
      (error "Must specify a uid-prefix"))
    (with-open-file (st (uid-generator-pathname uid-generator) :direction :output)
      (setf (uid-generator-stream uid-generator) st)
      (write-uid-generator-lock-file uid-generator)
      (setf (uid-generator-stream uid-generator) nil))))


(defmethod write-uid-generator-lock-file ((uid-generator filesystem-uid-generator))
  (let ((st (uid-generator-stream uid-generator)))
    (file-position st 0)
    (format st "~a~%" (uid-prefix uid-generator))
    (format st "~a~%" (uid-counter uid-generator)) ; the initial unique-id counter 
    ))

(defmethod read-uid-generator-lock-file ((uid-generator filesystem-uid-generator))
  (let ((st (uid-generator-stream uid-generator)))
     (file-position st 0)
    (setf (uid-prefix uid-generator) (read-line st)
	  (uid-counter uid-generator) (read st))
    (setf (have-lock uid-generator) t)
    ))

;;#+cmu(alien::def-alien-variable "errno" c-call:int)

#+cmu
(import 'unix::unix-errno)

(defmethod lock-uid-generator ((uid-generator filesystem-uid-generator) &key (wait t))
  (unless (have-lock uid-generator)	; is it already locked?
    (let* ((st (open (uid-generator-pathname uid-generator) :direction :io :if-exists :overwrite))
	   (status (posix-lock-file st :wait wait)))
      (setq *lockf-status* status)
      (unless (eql status 0)
	(unless wait (return-from lock-uid-generator (unix::unix-errno)))
	(error "lock-uid-generator bad return code ~a" (unix::unix-errno))
	)

      (setf (uid-generator-stream uid-generator) st)
      (read-uid-generator-lock-file uid-generator)
      (setf (have-lock uid-generator) t)
      t)))

(defmethod unlock-uid-generator ((uid-generator filesystem-uid-generator) ignore)
  (declare (ignore ignore))
  (unless (have-lock uid-generator)
    (error "Attempt to unlock a uid-generator that is not locked"))
  (write-uid-generator-lock-file uid-generator)
  (posix-unlock-file (uid-generator-stream uid-generator))
  (close (uid-generator-stream uid-generator))
  (setf (uid-generator-stream uid-generator) nil)
  (setf (have-lock uid-generator) nil))

(defclass simple-uid ()
    ((prefix :initarg :prefix :accessor uid-prefix)
     (count :initarg :count :accessor uid-count)
     (description :initform nil :initarg :description :accessor description)))

(defmethod print-object ((object simple-uid) stream)
  (if (description object)
      (format stream "~a.~d(~a)" (uid-prefix object) (uid-count object) (description object))
      (format stream "~a.~d" (uid-prefix object) (uid-count object))))

(defmethod print-object ((object simple-uid) stream)
  (if (description object)
      (format stream "(SIMPLE-UID ~a ~d ~a" (uid-prefix object) (uid-count object) (description object))
      (format stream "(SIMPLE-UID ~a.~d" (uid-prefix object) (uid-count object))))

(defmethod generate-uid ((uid-generator filesystem-uid-generator) description)
  (unless (have-lock uid-generator)
    (error "Attempt to generate a UID with a uid-generator that is not locked"))
  (make-instance 'simple-uid 
		 :prefix (uid-prefix uid-generator)
		 :count (incf (uid-counter uid-generator))
		 :description description))

(defmethod object-location-string ((object obj::basic-gl-object))
  (let ((lvcs (obj::world object)))
    (if (object-to-parent-transform lvcs)
	(bind-vector-elements (long lat elev) 
	    (transform-vector (to-lat-long-transform lvcs) (origin object))
	  (format nil "(GDC-LOCATION ~a ~10,6f ~10,6f)" (name (ellipsoid (parent lvcs))) lat long))

	(bind-vector-elements (x y z) (origin object)
	  (format nil "(LVCS-LOCATION ~a ~a ~a)" (name lvcs) x y)))))

(defmethod object-location ((object obj::basic-gl-object))
  (flet ((lat-long-string (x) (format nil "~10,6f" x)))
    (flet ((xyz-string (x) (format nil "~6,1f" x)))
      (let ((lvcs (obj::world object)))
	(if (object-to-parent-transform lvcs)
	    (bind-vector-elements (long lat elev) 
		(transform-vector (to-lat-long-transform lvcs) (origin object))
	      `(GDC-LOCATION ,(name (ellipsoid (parent lvcs))) 
		,(lat-long-string lat) ,(lat-long-string long)))

	    (bind-vector-elements (x y z) (origin object)
	      `(LVCS-LOCATION ,(name lvcs) ,(xyz-string x) ,(xyz-string y))))))))

;; (object-location-string (gui::selected-object))
;; (object-location (gui::selected-object))

(defmethod generate-uid ((uid-generator filesystem-uid-generator) (object obj::basic-gl-object))
  (generate-uid uid-generator (object-location-string object)))
		 
(defmethod parse-simple-uid-string (uid-string)
  (let* ((pos (position #\. uid-string))
	 (prefix (substring uid-string 0 pos))
	 (pos2 (position #\( uid-string :start pos))
	 (count (substring uid-string (1+ pos) pos2))
	)
    (if pos2
	(values prefix
		count
		(substring uid-string (1+ pos2) (1- (length uid-string))))
	(values prefix
		count))))
;; (parse-simple-uid-string (print-object (nth 0 uids) nil))
; (print-object (nth 0 uids) nil)

;;; FIXME -- these should be defined using (qffi::def-foreign-constants )
(defconstant POSIX_F_ULOCK 0) ; Unlock a previously locked region
(defconstant POSIX_F_LOCK 1)  ; Lock a region for exclusive use
(defconstant POSIX_F_TLOCK 2) ; Test and lock a region for exclusive use.
(defconstant POSIX_F_TEST 3)  ; Test a region for other processes locks.
(defconstant UNIX_ERRNO_EAGAIN 11)
    
;; (unix::unix-lockf)
(lcl::def-foreign-function (posix_lockf (:name "lockf"))
  (fd :int)
  (cmd :int)
  (position :int))

(defun posix_fd (stream)
  #+cmu (lisp::fd-stream-fd stream)
  )

(defun posix-lock-file (stream &key (wait t))
  (if wait
      (posix_lockf (posix_fd stream) POSIX_F_LOCK 0)
      (posix_lockf (posix_fd stream) POSIX_F_TLOCK 0)))

(defun posix-unlock-file (stream)
  (posix_lockf (posix_fd stream) POSIX_F_ULOCK 0)) 


#|
(setq uid-gen1
      (make-uid-generator 'filesystem-uid-generator 
			  :pathname "/tmp/uid-generator1"
			  :uid-prefix "uid-generator1"))
(describe uid-gen1)
(lock-uid-generator uid-gen1)
(lock-uid-generator uid-gen1 :wait nil)
(unlock-uid-generator uid-gen1 nil)

(setq *st* (open "/tmp/foo.bar"))
(lisp::fd-stream-fd *st*)
(close *st*)

(setq uids
      (with-uid-generator uid-gen1
	(loop repeat 5 collect (generate-uid uid-gen1 (gui::selected-object)))))

(format nil "~s" (nth 0 uids))

|#
