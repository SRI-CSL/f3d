(in-package :lisp-extensions)

#+cmu
(import 'pcl::undefmethod)

;;; sh#t-eating Allegro doesn't have undefmethod
#+allegro
(progn 

(defmacro undefmethod (&rest args)
  `(undefmethod-1 ',args))

(defun undefmethod-1 (args)
  (labels ((parse-defmethod (form)
	     (declare (list form))
	     (loop with original-form = form 
		   with name = (pop form)
		   while (and (car form) (atom (car form)))
		   collect (pop form) into qualifiers
		   finally
		(let ((lambda-list (pop form)))
		  (when (and (null lambda-list)
			     (consp (car form))
			     (consp (caar form)))
		    (error "~@<Qualifiers must be non-null atoms: ~s~@:>"
			   original-form))
		  (return (values name qualifiers lambda-list form)))))

	   (parse-specializers (specializers)
	     (declare (list specializers))
	     (flet ((parse (spec)
		      (if (symbolp spec)
			  (find-class spec)
			  (error "~S is not a legal specializer." spec))))

	       (mapcar #'parse specializers)))

	   (parse-method-or-spec (spec &optional (errorp t))
	     (let (gf method)
	       (multiple-value-bind (gf-spec quals specls)
		   (parse-defmethod spec)
		 (and (setq gf (and (or errorp (fboundp gf-spec))
				    (fdefinition gf-spec)))
		      (let ((nreq (excl::arg-info-number-required (excl::gf-arg-info gf))))
			(setq specls (append (parse-specializers specls)
					     (make-list (- nreq (length specls)) :initial-element excl::*the-class-t*)))
			(setq method (excl::get-method gf quals specls errorp)))))
	       (values gf method)))
	   )
    (multiple-value-bind (gf method)
	(parse-method-or-spec args)
      (when (and gf method)
	(remove-method gf method)
	method)))
  )

) ; end allegro progn 

#|

(defclass foo () () )

(defmethod bar ((x foo)) nil)

(defmethod bar :after ((x foo)) nil)

(undefmethod bar (foo))
(undefmethod bar :after (foo))

|#

#+sbcl
(eval-when (load compile)
  (defpackage mop (:use :sb-mop)
	      (:export "CLASS-PROTOTYPE" "CLASS-DIRECT-SUPERCLASSES" "CLASS-DIRECT-SUBCLASSES"  ))
)


#+allegro
(defun get-class-prototype (class-name)
  (let ((class (find-class class-name nil)))
    (unless (mop:class-finalized-p class)
      (mop::finalize-inheritance class))
    (mop:class-prototype class)))
  
#+sbcl
(defun get-class-prototype (class-name)
  (let ((class (find-class class-name nil)))
    (unless (sb-mop:class-finalized-p class)
      (sb-mop:finalize-inheritance class))
    (sb-mop:class-prototype class)))

#+cmu
(defun get-class-prototype (class-name)
  (mop:class-prototype (find-class class-name nil)))


(defun-cached class-superior-depth (class)
  (1+ (loop for cls in (mop:class-direct-superclasses class)
            maximize (class-superior-depth cls))))

(defun-cached all-class-inferiors (class)
  (cons class
        (loop with set
              for cls in (mop:class-direct-subclasses class)
              do (setq set (union set (all-class-inferiors cls)))
              finally (return set))))

(defun-cached all-class-superiors (class)
  (cons class
	(loop with set
	      for cls in (mop:class-direct-superclasses class)
	      do (setq set (union set (all-class-superiors cls)))
	      finally (return set))))


(defun find-multiple-inheritance-classes (base-class)
  (loop for class in (all-class-inferiors base-class)
	when (> (length (mop:class-direct-superclasses class)) 1)
	  collect class))

#|

(lx::find-multiple-inheritance-classes (find-class 'transforms::basic-coordinate-system))

(#<STANDARD-CLASS 2D-WORLD {58092A65}> #<STANDARD-CLASS AXIS-OBJECT {5888D26D}>
 #<STANDARD-CLASS 3D-CROSSHAIR-OBJECT {580385A5}>
 #<STANDARD-CLASS 3D-OBJECT {58091C3D}> #<STANDARD-CLASS 2D-OBJECT {58091B6D}>
 #<STANDARD-CLASS 3D-CURVE {580382D5}> #<STANDARD-CLASS 2D-CURVE {580382F5}>
 #<STANDARD-CLASS 3D-RIBBON-CURVE {58038335}>
 #<STANDARD-CLASS OBJ::IMAGE-OBJECT {58038295}>
 #<STANDARD-CLASS OBJ::GL-LABEL {5888D175}>
 #<STANDARD-CLASS 3D-TEXT-OBJECT {580382B5}>
 #<STANDARD-CLASS OBJ::GL-XYZ-SIZABLE-OBJECT {5888CAFD}>
 #<STANDARD-CLASS CUBE-OBJECT {58038395}>
 #<STANDARD-CLASS HOUSE-OBJECT {580383B5}>
 #<STANDARD-CLASS HALF-CYLINDER {580383D5}>
 #<STANDARD-CLASS 3D-WORLD {580381F5}>)

(loop for class in (lx::find-multiple-inheritance-classes (find-class 'transforms::basic-coordinate-system))
      collect (cons class (mop:class-direct-superclasses class)))



(loop for class in (lx::find-multiple-inheritance-classes (find-class 'lx::property-list-mixin))
      collect (cons class (mop:class-direct-superclasses class)))

(mop:class-direct-subclasses (find-class 'lx::fasd-form-property-list-mixin))
(all-class-superiors (find-class 'SMC::RADIUS-BUILDING))
(pcl::class-precedence-list (find-class 'SMC::RADIUS-BUILDING))
|#
