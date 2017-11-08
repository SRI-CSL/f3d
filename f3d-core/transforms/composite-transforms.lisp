(IN-PACKAGE :transforms)


;;; **********************   COMPOSITE-COORDINATE-TRANSFORM  ************************

(defstruct-class composite-coordinate-transform
    (coordinate-transform)
  ((transform-list :initform nil :initarg :transform-list :accessor transform-list))
  )

(defstruct-class composite-coordinate-projection (composite-coordinate-transform)
  ((3d-transforms :initform nil :initarg 3d-transforms :accessor 3d-transforms)
   (simple-projection :initarg :simple-projection :accessor simple-projection)
   (2d-transforms :initform nil :initarg 2d-transforms :accessor 2d-transforms)
   ))

(define-soft-slot composite-coordinate-projection :surrogate-projection surrogate-projection)

(defmethod initialize-instance :after ((coordinate-transform composite-coordinate-projection)
				       &key &allow-other-keys)
  (with-class-slots composite-coordinate-projection (transform-list 
						     simple-projection 3d-transforms 2d-transforms)
      coordinate-transform
    (mv-setq (3d-transforms simple-projection 2d-transforms)
	     (decompose-composite-coordinate-projection transform-list))))


#+wrong
(defmethod interior-and-exterior-matrices ((xf composite-coordinate-projection))
  (transforms::interior-and-exterior-matrices (transforms::simple-projection xf)))

(defmethod linear-depth-p ((coordinate-transform composite-coordinate-projection))
  (linear-depth-p (composite-coordinate-projection-simple-projection coordinate-transform)))


(define-fasd-form-init-plist composite-coordinate-transform
    (with-class-slots composite-coordinate-transform (transform-list) self
      (when transform-list
	`(:transform-list (list .,(loop for transform in transform-list
					collect (fasd-form transform)))))))

;;;  Not sure what should be done here.
;;;  Do we want to iterate update-transform over each transform in transform-list?
(defmethod update-transform ((coordinate-transform composite-coordinate-transform))
  nil)

(defun flatten-coordinate-transform-list (transform-list)
  (loop for transform in transform-list
	when (or (typep transform 'composite-coordinate-transform)
		 (typep transform 'composite-coordinate-projection))
	  append (flatten-coordinate-transform-list (transform-list transform))
	else when (consp transform)
	  append (flatten-coordinate-transform-list transform)
	else when transform collect transform))

(defun coordinate-projection-p (projection)
  (typep projection 'coordinate-projection))

(defmethod linear-transform-p ((transform-list list))
  (loop for transform in transform-list
	always (linear-transform-p transform)))

(defmethod linear-transform-p ((coordinate-transform composite-coordinate-transform))
  (with-class-slots composite-coordinate-transform (transform-list) coordinate-transform
    (loop for transform in transform-list
	  always (linear-transform-p transform))))

(defun composite-coordinate-transform-transform-list-vector (transform-list from-vector to-vector)
  (loop for transform in transform-list
        do (setq from-vector (transform-vector transform from-vector to-vector)))

  to-vector)

(define-coordinate-transform-method transform-vector
    ((transform composite-coordinate-transform) from-vector to-vector )
  (with-class-slots composite-coordinate-transform (transform-list) transform
    (loop for transform in transform-list
	  while from-vector
	  do (setq from-vector (transform-vector transform from-vector to-vector)))
    (when from-vector
      to-vector)))

;; (defvar *default-transform-incompatibility-action* :warn)

(defvar *default-transform-incompatibility-action* :ignore)

;;; Possible values for ERROR-ACTION are :WARN :ERROR :RETURN, or :IGNORE

;;; :IGNORE added 7/10/07 by CC because of NAD83/WGS84
;;; "incompatibility" errors between USGS & other sources.

(defun check-transform-sequence-compatibility (transform-list &key (error-action *default-transform-incompatibility-action*))
  (loop with ok = t
	for (transform1 transform2) on transform-list by #'cdr
	while transform2
	for to-coordinate-system = (to-coordinate-system transform1)
	for from-coordinate-system = (from-coordinate-system transform2)
	when (and to-coordinate-system
		    from-coordinate-system
		    (not (eq to-coordinate-system from-coordinate-system)))
	  ;; Both coordinate-systems are known but they disagree
	  do (case error-action
	       (:ignore
		(setq ok nil))
	       (:warn
		(setq ok nil)
		(format t ";;;  Warning: To-coordinate-system of ~a should be same as from-coordinate-system of ~a"
			transform1 transform2))
	       (:error
		(error "To-coordinate-system of ~a must be same as from-coordinate-system of ~a"
		       transform1 transform2))
	       (:return (return-from check-transform-sequence-compatibility nil)))
#|    
          when (and to-coordinate-system
		    from-coordinate-system
		    (not (eql (dimensionality from-coordinate-system)
			      (dimensionality to-coordinate-system))))
	  do (case error-action
	       (:warn
		(setq ok nil)
		(format t ";;;  Warning: To-coordinate-system dimensionality (~d) of ~a should be same as from-coordinate-system dimensionality (~d) of ~a"
			(dimensionality to-coordinate-system)  transform1
			(dimensionality from-coordinate-system) transform2))
	       (:error
		(error "To-coordinate-system dimensionality (~d) of ~a must be same as from-coordinate-system dimensionality (~d) of ~a"
		       (dimensionality to-coordinate-system)  transform1
		       (dimensionality from-coordinate-system) transform2))
	       (:return (return-from check-transform-sequence-compatibility nil)))
|#	     
	finally (return ok)))



(defun-cached cached-make-composite-coordinate-transform
    (transform-list
     &key (class 'composite-coordinate-transform) (create-inverse t) (error-action *default-transform-incompatibility-action*) )
  (make-composite-coordinate-transform transform-list
				       :class class
				       :create-inverse create-inverse
				       :error-action error-action))

(defun make-composite-coordinate-transform
    (transform-list
     &key (class 'composite-coordinate-transform) (create-inverse t) (error-action *default-transform-incompatibility-action*) )
  (cond ((not (consp transform-list))
	 transform-list)
	 
	((< (length transform-list) 2)
	 (car transform-list))
	(t (check-transform-sequence-compatibility transform-list :error-action error-action )
	   (let* ((from-coordinate-system (from-coordinate-system (first transform-list)))
		  (to-coordinate-system (to-coordinate-system (car (last transform-list))))
		  (composite-transform
		   (make-instance class
				  :transform-function 'composite-coordinate-transform-transform-vector
				  :transform-list transform-list
				  :from-coordinate-system from-coordinate-system
				  :to-coordinate-system to-coordinate-system
				  )))
	     (when create-inverse
	       (setf (inverse-transform composite-transform)
		     (make-instance 'composite-coordinate-transform ; class
				    :transform-function 'composite-coordinate-transform-transform-vector
				    :transform-list (loop for transform in (reverse transform-list)
							  collect (inverse-transform transform))
				    :from-coordinate-system to-coordinate-system
				    :to-coordinate-system from-coordinate-system
				    :inverse-transform composite-transform
				    )))
	     composite-transform))))



(defmethod optimize-transform ((transform composite-coordinate-transform))
  (eval-cache (optimize-transform transform)
      (or (optimize-transform-list (transform-list transform))
       transform)))


(define-coordinate-transform-method project-vector
    ((transform composite-coordinate-projection) from-vector to-vector )
  (with-class-slots composite-coordinate-transform (transform-list) transform
    (loop for transform in transform-list
	  while from-vector
	  do (setq from-vector (transform-vector transform from-vector to-vector)))
    (when from-vector ; from-vector will become NIL when a transform fails
      to-vector)))

(defun-cached cached-make-composite-coordinate-projection
    (transform-list &key (class 'composite-coordinate-projection) (error-action *default-transform-incompatibility-action*) )
  (make-composite-coordinate-projection transform-list
					:class class
					:error-action error-action))

(defun decompose-composite-coordinate-projection (transform-list)
  (loop with flattened-transform-list = (flatten-coordinate-transform-list transform-list)
        for transform = (pop flattened-transform-list)
        while (and transform  (not (coordinate-projection-p transform)))
        collect transform into 3d-transform-list
        finally ;;(break)
		(return (values 3d-transform-list transform flattened-transform-list))))

(progn ;; Version of functions where TRANSFORM-LIST is NOT CANONICALIZED

#+old
(defmethod composite-coordinate-projection-simple-projection 
	   ((coordinate-projection composite-coordinate-projection))
  (mv-bind (3d-transform-list simple-projection 2d-transform-list)
      (decompose-composite-coordinate-projection (transform-list coordinate-projection))
    (declare (ignore 3d-transform-list 2d-transform-list))
    simple-projection))

(defun make-composite-coordinate-projection
    (transform-list &rest args &key (class 'composite-coordinate-projection) (create-inverse t)
     (error-action *default-transform-incompatibility-action*))
  (declare (ignore create-inverse error-action))
  (apply #'make-composite-coordinate-transform (flatten-coordinate-transform-list transform-list)
	 :class class args))

) ; end progn

#+never ; broken: MAKE-COMPOSITE-COORDINATE-PROJECTION doesn't create inverse-transform.
(progn ;; Version of functions where transform-list is canonicalized

(defmethod composite-coordinate-projection-simple-projection 
	   ((projection composite-coordinate-projection))
  (cadr (transform-list projection)))

;;; The primary difference from make-composite-coordinate-transform is that
;;; transform-list is canonicalized to the 3-elements: (3d-transform simple-proj 2d-transform).
;;; This may not longer be needed.
;;; FIXME:  doesn't create inverse-transform.
(defun make-composite-coordinate-projection
    (transform-list &key (class 'composite-coordinate-projection) (create-inverse t)
     (error-action *default-transform-incompatibility-action*) )
  (let ((composite-projection
         (progn
	   (check-transform-sequence-compatibility transform-list :error-action error-action )
           (let* ((from-coordinate-system (from-coordinate-system (first transform-list)))
                  (to-coordinate-system (to-coordinate-system (car (last transform-list))))
                  (composite-transform
                   (make-instance class
                                  :transform-function 'composite-coordinate-projection-project-vector
                                  :transform-list transform-list
                                  :from-coordinate-system from-coordinate-system
                                  :to-coordinate-system to-coordinate-system
                                  )))
        
             composite-transform))))
    (with-class-slots composite-coordinate-projection (transform-list ) composite-projection
      (multiple-value-bind (3d-transform-list simple-proj 2d-transform-list)
	  (decompose-composite-coordinate-projection transform-list)
	(when (eq simple-proj composite-projection) 
	  (error "decompose-composite-coordinate-projection screwed-up"))
	(when (null simple-proj)
	  (error "Transform-list for a composite coordinate projection must include a simple-projection."))
	
	(let ((3d-transform
	       (if (> (length 3d-transform-list) 1)
		   (make-composite-coordinate-transform
		    (compose-coordinate-transform-list 3d-transform-list))
		   (car 3d-transform-list)))
	      (2d-transform
	       (if (> (length 2d-transform-list) 1)
		   (make-composite-coordinate-transform
		    (compose-coordinate-transform-list 2d-transform-list))
		   (car 2d-transform-list))))
	  (setq transform-list (list 3d-transform simple-proj 2d-transform))
	  composite-projection)))))

) ; end progn



;;; *********************  COMPOSE-TRANSFORM METHODS  *****************************

(defmethod compose-transforms ((transform1 t)
			       (transform2 t))
  nil)

(defmethod compose-transforms ((transform1 (eql nil))
			       (transform2 t))
  transform2)

(defmethod compose-transforms ((transform1 t)
			       (transform2 (eql nil)))
  transform1)

(defun compose-coordinate-transform-list (transform)
  (let* ((transform-list (flatten-coordinate-transform-list
                          (if (listp transform) transform (list transform))))
         (composed-transform-list
	  (and transform-list
	       (loop with trans1 = (pop transform-list)
		     with result
		     with trans3
		     for trans2 = (pop transform-list)
		     while (and trans1 trans2)
		     do (cond ((eq trans2 (inverse-transform trans1))
			       (setq trans3 nil
				     trans1 (pop transform-list)))
			      (t (setq trans3 (compose-transforms trans1 trans2))
				 (cond (trans3
					(setq trans1 trans3))
				       (t (push trans1 result)
					  (setq trans1 trans2)))))
			
		     finally (return (reverse (cons trans1 result)))))
           ))
    (if (and (listp composed-transform-list) (null (cdr composed-transform-list)))
        (car composed-transform-list)
        composed-transform-list)
    ))

(defun optimize-transform-list (transform-list)
  (let* ((new-transform (compose-coordinate-transform-list transform-list)))
    (and (not (equal new-transform transform-list))
	 new-transform
	 (if (consp new-transform)
	     (make-composite-coordinate-projection new-transform)
	     new-transform))))

(defmethod optimize-transform ((transform t))
  transform)

(defmethod optimize-transform ((transform list))
  (eval-cache (optimize-transform transform)
      (or (optimize-transform-list transform)
          transform)))

#|
(trace compose-transforms)	
(setq tr (list (object-to-world-transform obj) (3d-to-2d-projection (top-view)) (2d-to-window-transform (top-view))))
(optimize-transform tr)

(setq ctl (compose-coordinate-transform-list
           (setq tl (list (object-to-world-transform cube)
                          (3d-to-2d-projection (top-view))
                          (2d-to-window-transform (top-view))))))
(transform-point ctl 0.0 0.0 0.0 0.0)
(transform-point tl 0.0 0.0 0.0 0.0)

(setq alv-to-ft-hood
      (make-transform-path (coordinate-system (world tower)) (coordinate-system (3d-world (top-view)))
                         :encache :optimize
                         ;:search-mode :explicitely-related
                         ))

(transform-point alv-to-ft-hood 0.0 0.0 0.0 0.0)
|#


(defmethod compose-transforms ((transform1 list)
			       (transform2 t))
  (compose-transforms (compose-coordinate-transform-list transform1) transform2))

(defmethod compose-transforms ((transform1 t)
			       (transform2 list))
  (if transform2
      (compose-transforms transform1 (compose-coordinate-transform-list transform2))
      transform1))

