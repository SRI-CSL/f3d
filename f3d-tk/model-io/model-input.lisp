(in-package :freedius-io)

#| 

This file contains thefunctions and macro definitions to support the
construction of object models from the file produced by SAVE-FEATURE-SETS.

|#

;;; Move the to lisp/lisp-extensions.lisp
(defun round-to-multiple (x multiple)
  (* multiple (round x multiple)))
	
(defun get-or-make-feature-set (name world)
  (or (and world (loop for fs in (feature-sets world)
		       when (string-equal (name fs) name)
			 return fs))
      (make-instance 'feature-set :name name :parent world)))   

;;;(defun 4x4-coordinate-transform-from-io-form (origin z-y-x-rotation)
;;;  (let ((object-to-world-matrix 
;;;         (apply #'make-object-to-parent-rotation-matrix :z-y-x-rotation z-y-x-rotation)))
;;;    (destructuring-bind (x y &optional (z 0.0)) origin
;;;      (setf (aref object-to-world-matrix 0 3) x
;;;            (aref object-to-world-matrix 1 3) y
;;;            (aref object-to-world-matrix 2 3) z))
;;;    (make-4x4-coordinate-transform object-to-world-matrix)))

(defun 4x4-coordinate-transform-from-io-form (origin z-y-x-rotation)
  (make-4x4-coordinate-transform 
   (math::make-object-to-parent-matrix origin :z-y-x-rotation z-y-x-rotation)))
		  
;;; Do not use azimuth unless it is done correctly.
;;;(defun object-to-parent-transform-from-io-form (&key (origin '(0.0 0.0 0.0)) (azimuth 0.0))
;;;  (destructuring-bind (x y z) origin
;;;    (make-4x4-coordinate-transform (make-coord-frame-matrix x y z :azimuth-degrees azimuth))))
    
#+never ;; unused  
(defun object-to-parent-transform-from-io-form (&key (origin '(0.0 0.0 0.0)) (azimuth 0.0))
  (math::make-object-to-parent-matrix origin :heading-pitch-roll (list azimuth 0.0 0.0)))                     

(defparameter *quoting-keywords* nil)

(defun make-object-initargs-from-io-form (&rest initargs &key
					  origin 
					  (z-rotation 0.0)
					  (z-y-x-rotation (list z-rotation 0.0 0.0))
					  graphics-style
					  attributes
					  sizes
					  children
					  vertices
					  &allow-other-keys
					  )
  ;;(when children (break))
  `(:object-to-parent-transform
    ,(4x4-coordinate-transform-from-io-form origin z-y-x-rotation)
    ,@(when graphics-style `(:graphics-style ,(apply 'make-graphics-style graphics-style)))
    ,@(when attributes `(:attributes ,attributes))
    ,@(when sizes `(:sizes ,(apply #'coordinate-vector-general sizes)))
    ,@(when vertices `(:vertex-array ,(apply #'obj::make-vertex-array vertices)))  ;; seemed to be the easiest way to force this
    ,@(loop for (key form) on initargs by #'cddr
	    unless (member key '(:origin :z-rotation :z-y-x-rotation :graphics-style :attributes :sizes))
	    collect key
	    and collect (if (member key *quoting-keywords*)
			    `',form
			    form))
    ))



(defvar *default-parent* nil)

;;#+old ;; This is still very useful.  I would not use it for standard
;;#model I/O, but in some cases it is extremely useful for attaching a
;;#feature set to a named world without going through the coordinates.

(defmacro with-3d-world ((name &rest initargs) &rest io-forms)
  `(let ((*default-parent* (get-or-make-3d-world :name ,name .,initargs)))
    ,@io-forms
    *default-parent*))


(defmacro with-2d-world ((name &rest initargs &key 3d-world) &rest io-forms)
  `(let ((*default-parent* 
	  (get-or-make-2d-world :name ,name :3d-world ,(or 3d-world '*default-parent*) .,initargs)))
    ,@io-forms
    *default-parent*))

(defmacro with-coordinate-system (cs-spec &rest io-forms)
  `(let ((*default-parent* ,cs-spec))
    ,@io-forms
    *default-parent*))

(defmacro with-lvcs (cs-spec &rest io-forms)
  `(let ((*default-parent* ,cs-spec))
    ;; FIXME ---
    (change-class *default-parent* '3d-world)
    ,@io-forms
    *default-parent*))
	
;;; *************************  WITH-FEATURE-SET  *************************

;;; Here we really want to support feature-sets that are outside of the
;;; coordinate-system hierarchy, ie, not based on object-collection or basic-gl-object.  
;;; The children of go into the children of the *default-parent* the feature-set.  
;;; This in a stopgap implementation using the existing 3d-feature-set class.
;;; This is incorrect since (children fs) contains the children, but (parent child) is different.

(declaim (special *object-feature-sets*))

;;; In this version the use of the CREATE-OBJECT macro is eliminated
;;; to that it isn't needed in the file.	
(defun with-feature-set-internal2 (world name object-specs)
  (let* ((fs (get-or-make-feature-set name world))
	 (previous-children (children world)))

    (loop for (class . initargs) in object-specs
          when class   ;; Shows up when we convert feature sets with COMPOSITE-OBJECTS over to the new format.
	  do (create-object-internal *default-parent* class initargs))
		    
    (add-feature-set fs world)
    (let ((new-children (set-difference (children world) previous-children)))
      ;(setf (children fs) (union (children fs) new-children))
      (loop for obj in new-children
	    do (add-object obj fs)))
    #+bogus (push fs obj::*object-feature-sets*)
    fs))

(defmacro with-feature-set ((&key name parent ) &body io-forms)
  `(with-feature-set-internal2 ,(or parent '*default-parent*) ,name ',io-forms))



#+old
(defmacro 3d-object (class &rest io-initargs &key parent &allow-other-keys)
  (let ((initargs (apply 'make-object-initargs-from-io-form io-initargs)))
  `(make-instance ',class ,@initargs :parent ,(or parent '*default-parent*))))

#+old
(defmacro 2d-object (class &rest io-initargs &key parent &allow-other-keys)
  (let ((initargs (apply 'make-object-initargs-from-io-form io-initargs)))
  `(make-instance ',class ,@initargs :parent ,(or parent '*default-parent*))))

;;;(defmacro create-object (class &rest io-initargs &key parent &allow-other-keys)
;;;  `(apply 'make-instance ',class :parent ,(or parent '*default-parent*)
;;;    (apply 'make-object-initargs-from-io-form ',io-initargs )))

;(fmakunbound 'create-object-internal)
;;; This deals with a composite hierarchy.
(defmethod create-object-internal (parent class initargs)
  (let* ((children-specs (getf initargs :children))
	 (initargs (apply 'make-object-initargs-from-io-form initargs)))
    (remf initargs :children)
    (let* ((object (apply 'make-instance class :parent parent initargs)))
	 (when (and (typep object 'composite-object) children-specs)
	   (let* ((composite object))
	     (setf (children composite) nil)
	     ;; If CREATE-OBJECT appears, skip it (fn):
	     (loop for (class . initargs) in children-specs
		   do
                (when (eq class 'create-object)
                  (setf class (car initargs)
                        initargs (cdr initargs)))
                (create-object-internal composite class initargs))
	     (obj::compute-vertices composite)
	     ))
	 object)))
      
;;; What do we do about object classes that are not defined by the
;;; base FREEDIUS system?  Ideally, we want the I/O system to fail
;;; gracefully, perhaps by saving a list of "failed" feature-set loads
;;; that can be retried at a later time?

(defmacro create-object (class &rest io-initargs &key parent &allow-other-keys)
  `(create-object-internal ,(or parent '*default-parent*) ',class ',io-initargs))

(defmacro attributes (&rest key-vals)
  key-vals)

;;; This should move to another file
(defun make-graphics-style (&rest initargs)
  (apply 'make-instance 'graphics-style initargs))

(in-package :freedius-models-v0)

;;; graphics-style macro in :freedius-models-v0 conflicts
;;; with accessor-function defined in :obj.
(defmacro graphics-style (&rest initargs)
  `(apply 'freedius-io::make-graphics-style ',initargs))


;;;
;;; Quickie little COPY-OBJECT function, exploits IO-FORM:
;;;

(defmethod obj::copy-object ((object obj::basic-gl-object))
  ;; Sucks to have to use eval, but macro expansions are required:
  (if (typep (obj::parent object) 'obj::composite-object)
      (obj::copy-object (obj::parent object))
      (eval (freedius-io::io-form  object))))

