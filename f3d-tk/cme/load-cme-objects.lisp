(in-package :cme)

#|
(st::load-system 'cme)
(progn (st::load-system 'radius-classes)
       ;;(load-feature-sets  "$RADIUS/sites/alv/models/baseline")
       (make-3d-world :name "Radius Alv 3d World")
       (cme::load-feature-sets  "~/cp/lisp/cme-compat/data/alv-baseline.fs")
)

|#

(define-soft-slot basic-object :2d-world-history 2d-world-history)

;;; These are bound by site-glue-load-feature-sets
;;(defparameter *loading-models-into-world* nil)
;;(defparameter *loading-models-from-container* nil)

(declaim (special *reloading-objects-verbose* *reloading-objects-action* *redefine-object-retain-old-objects*))

(defun loading-container-internal (container-spec &rest initargs &key max-object-id &allow-other-keys)
  (let ((container (apply 'make-container container-spec :instance-count max-object-id initargs)))
    (setf (get-prop container :load-instance-count) max-object-id)
    
    (mv-bind (prefix site-name image-name)
	(parse-container-spec container-spec)
      (ignore prefix)
      (setq *loading-models-into-world*
	     (if image-name
		 (get-or-make-2d-world :name image-name :3d-world-name site-name )
		 (get-or-make-3d-world :name site-name))
	     *loading-models-from-container* container))))

(defmacro loading-container (container-spec &rest initargs)
  `(apply 'loading-container-internal ',container-spec ',initargs))

(defmethod parse-container-spec ((container-spec list))
  (let* ((l container-spec)
	 (ev-prefix
	  (cond ((ev-pathname-p (car l))
		 (pop l))
		((char= (aref (car l) 0) #\/)
		 (error "container path prefixes must start with an environment variable: ~a"
			container-spec))
		(t "$RADIUS/sites")))
	 (site-name (when (equal (pathname-name ev-prefix)  "sites") (pop l)))
	 (image-name (when (> (length l) 1) (pop l)))
	 (container-name (pop l)))
    (when l (error "container container-spec is has too many elements: ~a ~a"
		   container-spec (list ev-prefix site-name image-name container-name)))
    (values ev-prefix site-name image-name container-name)))



(defclass basic-site-glue
	 (lx::fasd-form-property-list-mixin)
    ((name :initform nil  :initarg :name :accessor name)
     ))

(defclass radius-site-glue
	 (basic-site-glue)
    ((2d-worlds-path :initform "$RADIUS/2d-worlds" :initarg :2d-worlds-path :accessor 2d-worlds-path)
     (sites-path :initform "$RADIUS/sites" :initarg :sites-path :accessor sites-path)
     (feature-set-paths :initform '("baseline.feats") :initarg :feature-set-paths :accessor feature-set-paths)
     (3d-world-file-name :initform "3d-world.lisp" :initarg :3d-world-file-name :accessor 3d-world-file-name)
     (glue-file-name :initform "glue.lisp" :initarg :glue-file-name :accessor glue-file-name)
     
  
     ))

(defclass radius-site-glue2 (radius-site-glue)
    ((feature-set-subdir-name :initform "models" :initarg :feature-set-subdir-name
			      :accessor feature-set-subdir-name)
     (site-path :initform nil :initarg :site-path)
     (site-2d-worlds-path :initform nil :initarg :site-2d-worlds-path
			  :accessor site-2d-worlds-path)
     (configurations-subdir-name :initform "configs" :initarg :configurations-subdir-name
				 :accessor configurations-subdir-name)
     
     )
  (:default-initargs :feature-set-paths nil)
  )


;;;
;;; RADIUS site glues look for "3d-world.lisp", so use a different
;;; name.  Eventually, I would like to stereotype site.lisp so that
;;; its form is very consistent from site to site.  As it stands now,
;;; 3d-world.lisp is usually full of site customizations.
;;; 

(defclass freedius-site-glue (radius-site-glue2)
    ()
  (:default-initargs :3d-world-file-name "site.lisp")
  )




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

(defun get-file-site-glue-instance (pathname)
  (mv-bind (comment-lines package-decl property-list package-decl-line)
      (extract-cme-lisp-file-header pathname)
    (ignore comment-lines package-decl package-decl-line)
    (let ((glue-spec (getf (cdr property-list) :glue)))
      (when glue-spec
	(cond ((symbolp glue-spec)
	       (make-instance glue-spec))
	      ((eq (car glue-spec) 'quote)
	       (make-instance (cadr glue-spec)))
	      ((consp glue-spec)
	       (eval glue-spec)))))))

;;;
;;; Really just creates a freedius-site-glue with everything defaulted
;;; to match the site subdirectory name.
;;;
(defun autoglue-site (site-subdir)
  (let ((site-path (format nil "$RADIUS/sites/~a" site-subdir))
	(2d-worlds-path (format nil "$RADIUS/site-2d-worlds/~a" site-subdir)))
    (format t "~%Generating autoglue for ~a." site-subdir)
    (make-instance 'freedius-site-glue
		   :name site-subdir
		   :site-path site-path
		   :glue-file-name nil
		   :site-2d-worlds-path 2d-worlds-path
		   :2d-worlds-path 2d-worlds-path
                   :3d-world-file-name "site.lisp"
		   :feature-set-subdir-name "models")))

#|
(get-file-site-glue-instance "$RADIUS/sites/alv/models/baseline")
|#

(defvar *reloading-objects-action* :retain)
(defvar *reloading-objects-verbose* t)

(defmethod redefining-object ((site-glue radius-site-glue) unique-id old-object class initargs)
  (ignore class initargs)
  (unless (typep old-object 'forward-referenced-object)
    (case *reloading-objects-action*
      (:retain (when *reloading-objects-verbose*
		 (format t ";;; Warning: reloading object ~a, old instance ~a retained.~%"
			 unique-id old-object))
	       (return-from redefining-object old-object))
      (:error (error "Reloading object with unique-id ~a, which is in use by ~a"
		     unique-id old-object)))
    
    (when (and *reloading-objects-verbose* *reloading-objects-action*)
      (format t ";;; Warning: reloading object ~a, replacing ~a.~%"
	      unique-id old-object))
    )
  
  (when (typep old-object 'feature-set)
    (setf (name old-object) nil)
    (remove-feature-set old-object (world old-object)))
   
  (if nil
      ;; Really want another class for this that indicates redefined objects.
      (change-class old-object 'forward-referenced-object)
      
      ;; Maybe should not call change-class, instead push the object onto a redefined-objects-list of world
      (progn (remove-self old-object)	; remove old-object ffrom feature-sets, ...
	     (when *redefine-object-retain-old-objects*
	       (push old-object (internal-get-prop (world old-object) :redefined-objects-list)))))

  ;; Uproot the unique-id from old-object.
  (let ((unique-id (unique-id old-object)))
    (remhash (instance-id unique-id) (hash-table (container unique-id)))
    (setf (unique-id old-object) nil))
    
  )
(defparameter *object-io-site-glue* nil )
  
(defun make-cme-object (class &rest initargs)
  (apply 'site-glue-make-cme-object *object-io-site-glue*
	 class initargs))

(defmethod site-glue-make-cme-object ((site-glue radius-site-glue)
				      class &rest initargs
				      &key
				      world
				      feature-sets
				      object-to-world-transform 
				      inferiors
				      vertices
				      top-vertices
				      unique-id
				      graphics-style
				      property-list
				      2d-world-history
				      &allow-other-keys)

  (ignore unique-id)
  ;;(break)
  (when unique-id
    (let* ((container (reference-container (butlast unique-id))))
      (when (and *LOADING-MODELS-FROM-CONTAINER* (neq container *LOADING-MODELS-FROM-CONTAINER*))
	(error "Attempted to create an object in the wrong container ~s while loading container ~s."
	       unique-id (container-spec *LOADING-MODELS-FROM-CONTAINER*)))
      (let ((object-in-container (gethash (car (last unique-id))(hash-table container))))
	(when object-in-container
	  (let ((return-object (redefining-object site-glue unique-id object-in-container class initargs)))
	    (when return-object
	      (return-from site-glue-make-cme-object return-object)))))))
  (setq world (cond ((stringp world)
		     (gui::get-world-named world))
		    ((null world)
		     *loading-models-into-world*)
		    (t world)))
  (setf (getf initargs :world) world)
	
  (unless (loop for fs in feature-sets
		always (typep fs 'feature-set))
    (setf (getf initargs :feature-sets)
	  (loop for fs in feature-sets
		collect (if (typep fs 'feature-set)
			    fs
			    (find-feature-set world fs)))))
  
  (when (listp object-to-world-transform)
    (setq object-to-world-transform
	  (if (numberp (car object-to-world-transform))
	      (apply 'make-coord-frame-to-world world object-to-world-transform)
	      (eval object-to-world-transform)
	      ))
    (setf (getf initargs :object-to-world-transform)
	  object-to-world-transform))

  (let ((sizes (extract-object-dims initargs)))
    (when sizes
      (setf initargs (list* :sizes sizes initargs))))
  
  (when (and (consp inferiors) (not (typep (car inferiors) 'basic-object)))
    (setf (getf initargs :inferiors)
	  (OBJECT-LIST-FROM-UNIQUE-IDS inferiors)))

  (let ((2d-world-history (getf property-list :2d-world-history)))
    (when (and 2d-world-history (consp 2d-world-history))
      (setf (getf property-list :2d-world-history)
	    (make-2d-world-history 2d-world-history))))
  
  (when (consp graphics-style)
    (setf (getf initargs :graphics-style)
	  (apply 'make-instance 'graphics-style graphics-style)))
  
  (when (consp vertices)
    (setf (getf initargs :vertices) (make-vertex-array-from-list vertices)))

  (when (consp top-vertices)
    (setf (getf initargs :top-vertices) (make-face-vertex-list top-vertices)))
  
  ;;(format t "site-glue-make-cme-object ~a ~a~%" class initargs)
  
  (let* ((*object-io-site-glue* nil) ; need this to prevent composites from calling site-glue to clone
	 (object (apply 'make-instance class initargs)))
    ;; new site model files specify 2d-world-history outside of the property-list
    (when 2d-world-history
      (setf (2d-world-history object)
	    (if (consp 2d-world-history)
		(make-2d-world-history 2d-world-history)
		2d-world-history)))
    #+never 
    (when (and world object-to-world-transform)
      (transforms::connect-transforms object-to-world-transform
				      (inverse-transform object-to-world-transform) object world))
    
    (when unique-id (set-object-unique-id object unique-id))
    object
    ))

(defun extract-object-dims (init-plist)
  (let ((x-size (getf init-plist :x-size))
	(y-size (getf init-plist :y-size))
	(z-size (getf init-plist :z-size)))
    (when (and x-size y-size z-size)
      (cv x-size y-size z-size))))
    

(defparameter *radius-site-glue* (make-instance 'radius-site-glue))
(defparameter *freedius-site-glue* (make-instance 'freedius-site-glue))
(defparameter *default-object-io-site-glue* (make-instance 'radius-site-glue))

(defun load-feature-sets (pathname &rest args &key (site-glue *radius-site-glue*)
				   &allow-other-keys)
  (ignore site-glue)
  ;; This handler is used to deal with loading RADIUS site models from CME by autoloading SMC if possible.
  ;; Perhaps CME should always contain radius-classes.
  (unless (find-package "SMC")
    (progn ;ignore-errors ;; allow for posibility that system doesn't exist (non RADIUS distribution) of 3DIUS
      (st::load-system 'radius-classes)))
  (apply 'load-feature-sets-internal pathname :site-glue site-glue args))


(defun load-feature-sets-internal (pathname &rest args &key site-glue &allow-other-keys)
  (unless site-glue
    (setq site-glue
	  (or (get-file-site-glue-instance pathname)
	      *default-object-io-site-glue*)))
  (let ((*object-io-site-glue* site-glue))
    (apply 'site-glue-load-feature-sets site-glue pathname args)
    ))

(declaim (special *loading-models-from-container*
		  *loading-models-into-world*
		  *fasd-loading-p*
		  *object-feature-sets*))

(defvar *force-load-feature-set* nil)

;;;  file-write-dates and feature-sets of loaded feature-set files.
;;;  Should site-glue-load-feature-sets use EVAL-CACHE instead?
(defparameter *load-feature-sets-times-hash-table* (make-hash-table :test #'equal))

(defmethod site-glue-load-feature-sets ((site-glue radius-site-glue) file
					&key &allow-other-keys )
  (if (not (probe-file file))
      (warn "~%WARNING:  Feature set file ~a was not found." file)
    (let* ((namestring (namestring file))
	   (entry (gethash namestring *load-feature-sets-times-hash-table*))
	   (last-write-time (car entry))
	   (current-write-time (file-write-date namestring)))
      (format t "~%;Loading feature set ~a" file)
      (cond ((or *force-load-feature-set* (null last-write-time) (< last-write-time current-write-time))
	     (let ((*object-feature-sets* nil)
		   ;; file is expected to setq *loading-models-into-world* if worlds not explicit
		   (*loading-models-into-world* nil)
		   (*loading-models-from-container* nil)
		   (*package* (find-package :cme))
		   (*fasd-loading-p* t))
	       (declare (special *loading-models-into-world* *loading-models-from-container*
				 *fasd-loading-p* *object-feature-sets*))
	       (load file)		; (ic::ev-pathname-translate file)
	       (loop for fs in *object-feature-sets*
		     for world = (world fs)
		     do (setf (get-prop fs :pathname) file)
		     do (set-world fs world))
	       (setf (gethash namestring *load-feature-sets-times-hash-table*)
		     (cons current-write-time *object-feature-sets*))
	       ;;(break)
	       *object-feature-sets*))
	    (t (let ((feature-sets (cdr entry)))
		 (loop for fs in feature-sets
		    do (fixup-feature-set-worlds (list fs) (world fs)))
		 feature-sets))))))

#|
(site-glue-load-feature-sets% *radius-site-glue* "/opt/IU/radius/sites/alv/models/alv.fs")
(let ((lx::*eval-cache-recompute* t)) ; force reload
  (site-glue-load-feature-sets% *radius-site-glue* "/opt/IU/radius/sites/alv/models/alv.fs"))
;(get 'site-glue-load-feature-sets% :function-cache)
|#

(defun-cached site-glue-load-feature-sets% (site-glue file)
  (let ((*object-feature-sets* nil)
	;; file is expected to setq *loading-models-into-world* if worlds not explicit
	(*loading-models-into-world* nil)
	(*loading-models-from-container* nil)
	(*package* (find-package :cme))
	(*object-io-site-glue* site-glue) 
	(*fasd-loading-p* t))
    (declare (special *object-io-site-glue* *loading-models-into-world* *loading-models-from-container*
		      *fasd-loading-p* *object-feature-sets*))
    (format t "~%;Loading feature set ~a" file)
    (load file)			    ; (ic::ev-pathname-translate file)
    (loop for fs in *object-feature-sets*
	  for world = (world fs)
	  do (setf (get-prop fs :pathname) file)
	  do (set-world fs world))
    (values *object-feature-sets* (file-write-date (namestring file)))))

(defmethod site-glue-load-feature-sets ((site-glue radius-site-glue) file
					&key &allow-other-keys )
  (mv-bind (feature-sets last-write-time)
      (unless *force-load-feature-set*
	(lx::eval-cache-probe (site-glue-load-feature-sets% site-glue file)))
    (cond ((null last-write-time)
	   (site-glue-load-feature-sets% site-glue file))
	  ((< last-write-time (file-write-date (namestring file)))
	   (let ((lx::*eval-cache-recompute* t)) ; force reload
	     (site-glue-load-feature-sets% site-glue file)))
	  (t (values feature-sets last-write-time)))))
;(site-glue-load-feature-sets%     

(defmethod fixup-object-forward-references ((object forward-referenced-object) (real-object basic-object))
  ;; what should happen here?  Can we really find all pointers to an object?
  ;; Feature-sets and composite objects are the main things that point to objects.
  ;; HOWEVER, it is possible for users to build arbitrary lists of things.
  ;; GRAPHICS-STYLE-FILTERS CAN REFER TO OBJECTS.   NOT HANDLED HERE.
  (error "fixup-object-forward-references ~a ~a~%" object real-object)
  #+not-converted-to-freedius
  (with-slots (composite-superior feature-sets) object
    (when (and composite-superior (not (typep composite-superior 'forward-referenced-object)))
      (remove-object object composite-superior)
      (add-object real-object composite-superior))

    (loop for fs in feature-sets
	  do (unless (typep fs 'forward-referenced-object)
	       (remove-object object fs)
	       (add-object real-object fs)))
    ))



;;; ****************************  OBJECT-UNIQUE-ID  ******************************

;; (defstruct-class object-unique-id (lx::fasd-form-init-plist))
(defclass object-unique-id (lx::fasd-form-property-list-mixin)
  ((container :initarg :container :accessor container)
   (instance-id :initarg :instance-id :accessor instance-id)))

(defmethod fasd-form ((object-unique-id object-unique-id))
  `(make-object-unique-id ,(instance-id object-unique-id)
    ',(container-spec (container object-unique-id))))

(defmethod print-object ((object-unique-id object-unique-id) stream)
  (format stream "#<UNIQUE-ID~{ ~s~} ~s>"
	  (container-spec (container object-unique-id)) (instance-id object-unique-id))) 

(defun object-with-id (&rest unique-id-list)
  (let* ((container (reference-container (butlast unique-id-list)))
	 (object-id (car (last unique-id-list)))
	 (object (gethash object-id (hash-table container))))
    (or object
	;; fall thru for forward referenced containers
	(let ((obj (make-instance 'forward-referenced-object)))
	  (set-object-unique-id obj (make-instance 'object-unique-id
						   :container container
						   :instance-id object-id))
	  obj))))
	

(defmethod set-object-unique-id ((object basic-object) (unique-id-spec list))
  (unless (eq (reference-container (butlast unique-id-spec)) *LOADING-MODELS-FROM-CONTAINER*)
    (error "Attempted to create an object in the wrong container ~s while loading container ~s."
	   unique-id-spec (container-spec *LOADING-MODELS-FROM-CONTAINER*)))

  (set-object-unique-id object
			(make-instance 'object-unique-id
				       :container *LOADING-MODELS-FROM-CONTAINER*
				       :instance-id (car (last unique-id-spec)))))

(declaim (special  *LOADING-MODELS-FROM-CONTAINER*))

;;; SET-OBJECT-UNIQUE-ID should only be called during object creation while loading container files.
;;; The container should always be defined and the container being loaded.
(defmethod set-object-unique-id ((object basic-object) (unique-id-spec number))
  (set-object-unique-id object
			(make-instance 'object-unique-id
				       :container *LOADING-MODELS-FROM-CONTAINER*
				       :instance-id unique-id-spec)))

;;; NEED TO FIX THINGS HERE TO PERMIT RELOADING A MODEL FILE
;;; (perhaps a we had to abort in the middle of loading)

(defparameter *object-io-debug* t)

(defmethod set-object-unique-id ((object basic-object) (unique-id object-unique-id))
  (let ((object-in-container (gethash (instance-id unique-id) (hash-table (container unique-id)))))
    (when (and object-in-container (neq object-in-container object)
	       (not *object-io-debug*))
      (unless  (typep object-in-container 'forward-referenced-object)
	(error "Attempting to assign the same unique-id ~a to ~a, but ~a already has that id."
	       unique-id object object-in-container))
      (fixup-object-forward-references object-in-container object)
      ))

  (setf (gethash (instance-id unique-id) (hash-table (container unique-id))) object)
  (setf (unique-id object) unique-id))


;;; ******************************   BASIC-CONTAINER  ******************************

(defclass basic-container (lx::fasd-form-property-list-mixin)
    ((container-spec :initform nil :initarg :container-spec :accessor container-spec)
     (instance-count :initform 0 :initarg :instance-count :accessor instance-count)
     ;; given that ids are allocated sequentially, we could use an array instead of a hash-table
     (hash-table :initform (make-hash-table :test 'equal) :accessor hash-table)
     (pathname :initform nil :accessor container-pathname)
     (status :initform nil :initarg :status :accessor status)
     (owner :initform nil :initarg owner)
     (security :initform nil :initarg :security :accessor security)
     ))


(defparameter *container-hash-table* (make-hash-table :test 'equal))

(defun reference-container (container-spec)
  (setq container-spec (canonicalize-container-spec container-spec))
  (or (gethash container-spec *container-hash-table*)
      (setf (gethash container-spec *container-hash-table*)
	    (make-instance 'basic-container
			   :status :forward-referenced
			   :container-spec container-spec ))))

(defun make-container (container-spec &rest initargs &key (if-exists :warn) &allow-other-keys)
  (setq container-spec (canonicalize-container-spec container-spec))
  (let ((exists (gethash container-spec *container-hash-table*)))
    (if exists
	(case if-exists
	  (:error
	   (error "MAKE-CONTAINER found an existing container with same spec ~s" container-spec))
	  (:warn
	   (format t ";;; Warning: MAKE-CONTAINER found an existing container with same spec ~s~%"
		   container-spec)
	   exists)
	  (otherwise exists))
	(setf (gethash container-spec *container-hash-table*)
	      (apply 'make-instance 'basic-container
		     :container-spec container-spec 
		     initargs)))))


(defmethod object-list-from-unique-ids (unique-id-list &optional container-spec)
  (loop with container = (if container-spec
			     (make-container Container-Spec)
			     *Loading-models-from-container*)
	for id in unique-id-list
	collect (if (numberp id)
		    (gethash id (hash-table container))
		    (apply 'object-with-id id))))

(defun list-hash-table (ht)
  (let ((l nil))
    (maphash #'(lambda (key val) (push (list key val) l)) ht)
    l))
#|
(cadr (car (list-hash-table *container-hash-table*)))
(list-hash-table (hash-table lisptk::container1))
(setq *alv-object-list*
      (loop for (id obj)
	    in (list-hash-table (hash-table (cadr (car (list-hash-table *container-hash-table*)))))
	    collect obj))

 |#
 
(defmethod initialize-instance :after ((container basic-container) &key &allow-other-keys)
  (make-container-pathname container))

(defmethod print-object ((container basic-container) stream)
  (let ((pathname (container-pathname container))
	(container-spec (container-spec container)))
    (if (container-pathname container)
	(format stream "#<Container ~a>"pathname)
	(if (listp container-spec)
	    (format stream "#<Container~{ ~a~}>" container-spec)
	    (format stream "#<Container ~a>" container-spec)))))

(defmethod canonicalize-container-spec ((container-spec list))
  (mv-bind (ev-prefix site-name image-name container-name)
      (parse-container-spec container-spec)
    (let ((postfix (if image-name
		       (list image-name container-name)
		       (list container-name))))
      (if (equal ev-prefix "$RADIUS/sites")
	  (list* site-name postfix)
	  (if site-name
	      (list* ev-prefix site-name postfix)
	      (list* ev-prefix postfix))))))

(defmethod canonicalize-container-spec ((container basic-container))
  (setf (container-spec container) (canonicalize-container-spec (container-spec container))))


(defmethod make-container-pathname ((container basic-container))
  (mv-bind (ev-prefix site-name image-name container-name)
      (parse-container-spec (container-spec container))
  
    (setf (container-pathname container)
	  (cond ((and site-name image-name)
		 (format nil "~a/~a/images/~a/models/~a" ev-prefix site-name image-name container-name))
		(site-name
		 (format nil "~a/~a/models/~a" ev-prefix site-name container-name))
		(image-name
		 (format nil "~a/~a/models/~a" ev-prefix image-name container-name))
		))))

(defmethod fasd-form ((container basic-container))
  `(make-container ',(container-spec container)))


(defmethod fasd-form-p ((container basic-container)) t)


;;; unfinished stubs

(defmethod ic::load-image (path &rest args) (apply 'load-image path args))

(defun ic::make-gaussian-pyramid (image &optional image-hierarchy-list)
  nil   )

     
(defmethod ic::ev-pathname-register (thing)
  (lx::ev-pathname-register thing))
  
(defmethod MAKE-TERRAIN-MODEL (dtm-image dtm-to-lvcs-transform)
  (MAKE-GRIDDED-TERRAIN-MODEL dtm-image dtm-to-lvcs-transform))

#+never  ;; these are defined in terrain-models.lisp
(progn

(defmethod MAKE-MULTI-TERRAIN-MODEL (terrain-model &key &allow-other-keys)
  terrain-model)

(defmethod MAKE-GRIDDED-TERRAIN-MODEL (dtm-image dtm-to-lvcs-transform)
  nil)

(defmethod MAKE-DMA-DTED-TERRAIN-MODEL (&rest ignore)
  nil
  )

(defmethod  make-planar-terrain-model (&rest ignore)
  nil)

(defun MAKE-USGS-TERRAIN-MODEL (dem-image geocentric-to-lvcs-transform 
				dem-coordinate-system-name &rest args &key
				exact lat-long-to-geocentric-transform )
  (declare (ignore exact lat-long-to-geocentric-transform)))

#+never ; disable this for now
(defun MAKE-USGS-TERRAIN-MODEL (dem-image geocentric-to-lvcs-transform 
				dem-coordinate-system-name &rest args &key
				exact lat-long-to-geocentric-transform )
  (declare (ignore exact lat-long-to-geocentric-transform))
  (st::autoload-system :terrain-models)
  (apply 'MAKE-USGS-TERRAIN-MODEL 
	 dem-image geocentric-to-lvcs-transform dem-coordinate-system-name args))

(defmethod read-dma-dted (&rest ignore)
  nil)

) ; end progn



;; (defmethod find-feature-set (&rest ignore))

;;(defmethod object-list-from-unique-ids ( inferiors))

(defmethod  make-2d-world-history ( 2d-world-history))

(defmethod FIXUP-FEATURE-SET-WORLDS (&rest ignore))


(defmethod MAKE-VERTEX-ARRAY-FROM-LIST (vertices)
  (apply 'make-vertex-array vertices)
  )

(defmethod make-face-vertex-list (xyz-list)
  (loop for i from 0
	for (x y z) in xyz-list
	;; If the List really has 2d vertices, z will be nil and make vertex will default z to 0.0.
	collect (cv x y z)))

(defun MAKE-VERTEX-ARRAY-FROM-VERTEX-LIST (vertex-list)
  (MAKE-VERTEX-ARRAY-FROM-LIST vertex-list)) 

(defun make-ribbon-vertex-array-from-list (xyz-list)
  (apply 'make-vertex-array xyz-list))


;; associate-2d-pt-list-with-2d-worlds is called from some old site model CONJUGATE-POINTS
(progn

(defparameter *2d-world-mapping-alist* nil)
(defvar *default-2d-world* nil)

(defun map-2d-world-name (name &optional (missing-ok t))
  (let ((mapped-name (cdr (assoc name *2d-world-mapping-alist* :test 'equal))))
    (if mapped-name
	(get-2d-world-named mapped-name)
	(unless missing-ok
	  (format t "Cannot find 2d world ~a~%" name)
	  nil))))

(defun associate-2d-pt-list-with-2d-worlds (2d-pt-list)
  (loop for entry in 2d-pt-list
	for (name . rest ) = entry
	collect (if (typep name '2d-world)
		    entry
		    (let ((fixed-name (fixup-2d-world-name name)))
		      (list* (or (get-2d-world-named name)
				 (and fixed-name (get-2d-world-named fixed-name))
				 (map-2d-world-name name)
				 *default-2d-world*
				 name)
			     rest)))))

) ;end progn


(defclass math::basic-covariance-model (fasd-form-property-list-mixin)
    ((source :initform "Unknown" :initarg :source :accessor source)))

(defclass math::4x4-coordinate-projection-covariance-model (math::basic-covariance-model)
  ((covariance-matrix :initarg :covariance-matrix :accessor covariance-matrix)))

(import '(math::basic-covariance-model math::4x4-coordinate-projection-covariance-model) "CME")

 ;;; *******************************  COORDINATE-SYSTEMS **********************


;;;(defun make-coordinate-system (&rest args)
;;;  nil)


;;; defined in transforms/geographic-transforms.lisp
;;;(defun make-lvcs-to-geocentric-transform (&rest args)
;;;  nil)


;;;(defun from-deg-min-sec (deg &optional (min 0) (sec 0))
;;;  (* (signum deg)
;;;     (+ (abs deg) (/ min 60d0) (/ sec 3600d0))))

;;;(defun from-deg-min-sec-string (string )
;;;  (let ((sign 1.0) deg min sec pos1 pos2 pos3)
;;;    (setq string (string-left-trim '(#\space) string))
;;;    (when (char-equal (aref string 0) #\-)      
;;;      (setq string (substring string 1)
;;;            sign -1))
;;;
;;;    (mv-setq (deg pos1) (read-from-string string))
;;;    
;;;    (mv-setq (min pos2) (read-from-string string nil nil :start pos1))
;;;    (when min
;;;      (when (and (symbolp min) (member min '(deg d) :test 'string-equal)
;;;                 )
;;;        (mv-setq (min pos2) (read-from-string string nil nil :start pos2)))
;;;      
;;;      (when min
;;;        (mv-setq (sec pos3) (read-from-string string nil nil :start pos2))
;;;        (when (and (not (numberp sec)) (equal sec "min")) 
;;;          (mv-setq (sec pos3) (read-from-string string nil nil :start pos3)))
;;;        (when sec
;;;          (when (and (listp sec) (equal (car sec) 'quote)) (setq sec (cadr sec))))))
;;;    (unless (numberp deg) (setq deg 0))
;;;    (unless (numberp min) (setq min 0))
;;;    (unless (numberp sec) (setq sec 0))
;;;    (* sign
;;;       (+ (abs deg) (/ min 60.0d0) (/ sec 3600.0d0))))
;;;  )


;;;(defun to-deg-min-sec-string (deg0 &key (stream nil) (seconds-fraction-digits 4) abbrev)
;;;  (let ((round-deg (when seconds-fraction-digits
;;;                     (* (/ .5 3600) (expt 10.0d0 (- seconds-fraction-digits))))))
;;;    (mv-bind (deg min sec)
;;;        (to-deg-min-sec deg0 round-deg)
;;;      (format stream (if abbrev
;;;                         "~4d deg ~2d' ~v,2$\" "
;;;                         "~4d deg ~2d min ~v,2$ sec ")
;;;              (if (minusp deg0) (- deg) deg)
;;;              min
;;;              seconds-fraction-digits
;;;              (- sec (* 3600.0 round-deg)) ; compensate for rounding that format conversion will do
;;;              ))))

;;;(defun to-deg-min-sec (deg &optional (round-sec nil))
;;;  ;; backward compatibility defaulting
;;;  (cond ((eq round-sec t) (setq round-sec (/ .5 3600)))  ; round up by 1/2 second of arc.
;;;        ((eq round-sec nil) (setq round-sec 0.0)))
;;;  
;;;  (let* ((sign (signum deg ))
;;;         (deg (+ (abs deg) round-sec)) 
;;;         (int-deg (floor deg))
;;;         (min (* 60 (- deg int-deg)))
;;;         (int-min (floor min))
;;;         (sec (* 60 (- min int-min))))
;;;    (values int-deg int-min sec sign)))

;;;(defvar *feet-per-meter-international*  (/ .3048))
;;;(defvar *feet-per-meter-us* (/ 39.37 12.0))

;;;(defparameter *feet-per-meter* *feet-per-meter-us* ) ; defaulting to US standard



;;; ***************************************************************************
;;; These next are compatibility definitions

;;; This is only called from within this file
(defun make-coordinate-frame (v2w-matrix &optional property-list)
  (make-4x4-coordinate-transform v2w-matrix :property-list property-list))

;;; This has lots of callers from this file and cme-compat files.
(defun make-coord-frame (x y z &rest axis-angle-pairs)
  (make-4x4-coordinate-transform
   (apply 'make-coord-frame-matrix x y z axis-angle-pairs)))

(defun make-coord-frame-to-world (to-world x y z &rest axis-angle-pairs)
  (make-4x4-coordinate-transform
   (apply 'make-coord-frame-matrix x y z axis-angle-pairs)
   :to-coordinate-system to-world))

(defun make-coord-frame-from-elements (property-list &rest elements)
  (make-4x4-coordinate-transform
   (apply 'make-and-fill-4x4-matrix elements)
   :property-list property-list))

#|
(setq trans (make-coord-frame 10. 20.0 30.0 :z-deg 45.0)) 
(transform-vector trans (cv 1.0 2.0 3.0))
|#
