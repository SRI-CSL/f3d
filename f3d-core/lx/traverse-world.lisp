(IN-PACKAGE :lx)

#+cmu (import 'pcl::class-slots)
#+sbcl (import 'sb-pcl::class-slots)
#+allegro (import 'clos::class-slots)

(declaim (special *traversal-ht* *traversal-test* *traversal-target*))
;;(defvar *traversal-ht*)
;;(defvar *traversal-test*)

(defvar *traversal-last-symbol* nil)
(defparameter *traversal-ignore-symbols*
	      '(*traversal-ht*
		*traversal-test*
		*traversal-last-symbol*
		#+cmu c::*info-environment*
		))

(defparameter *traverse-object-last-object* nil)

#+cmu
(defun traverse-object (x)
  (let ((type (type-of x)))
    (cond ((eq type 'function) (traverse-function x))
	  ((eq type 'KERNEL:FUNCALLABLE-INSTANCE))
	  ((eq type 'EXTENSIONS:INSTANCE)) ; fixme
	  ((eq type t))
	  (t (typecase x
	       (number nil)
	       (character nil)
	       (symbol nil)
	       (package nil)
	       (otherwise
		(let ((been-there (gethash x *traversal-ht*)))
		  (cond ((eq been-there :hot)
			 (format t "*traversal-last-symbol* = ~s :hot~%" *traversal-last-symbol* )
			 ;(break "*traversal-last-symbol* = ~s :hot" *traversal-last-symbol* )
			 t)
			(been-there nil)
			((funcall *traversal-test* x)
			 (break "*traversal-last-symbol* = ~s" *traversal-last-symbol* )
			 t)
			((consp x) (traverse-list x))
			(t (setf (gethash x *traversal-ht*) t)
			   (when (typecase x
				   ;;(symbol (traverse-symbol x))
				   (array (when (eq (array-element-type x) t) (traverse-array x)))
				   (standard-object  (traverse-standard-object x))
				   (sys::system-area-pointer nil)
				   (ext::weak-pointer nil)
				   (sys::fdefn nil)
				   (sys::stream nil)
				   (condition nil)
				   (sys::code-component nil)
				  ; (PCL::SLOT-OBJECT nil)
				   (structure-object (traverse-structure x))
				   (hash-table (traverse-hash-table x))
				  ; (package (traverse-package x))
				   (function #+allegro COMPILED-FUNCTION
					     (traverse-compiled-function x))
				   (otherwise (setq *traverse-object-last-object* x)
					      (break ";; TRAVERSE-OBJECT: unknown type ~s" (type-of x)))
				   )
			     (setf (gethash x *traversal-ht*) :hot))
			   )))))))))

#+sbcl
(defun traverse-object (x)
  (let ((type (type-of x)))
    (cond ((eq type 'function) (traverse-function x))
	  ((eq type 'sb-KERNEL:FUNCALLABLE-INSTANCE))
	  ;;((eq type 'sb-EXT:INSTANCE)) ; fixme
	  ((eq type t))
	  (t (typecase x
	       (number nil)
	       (character nil)
	       (symbol nil)
	       (package nil)
	       (otherwise
		(let ((been-there (gethash x *traversal-ht*)))
		  (cond ((eq been-there :hot)
			 (format t "*traversal-last-symbol* = ~s :hot~%" *traversal-last-symbol* )
			 ;(break "*traversal-last-symbol* = ~s :hot" *traversal-last-symbol* )
			 t)
			(been-there nil)
			((funcall *traversal-test* x)
			 (break "*traversal-last-symbol* = ~s" *traversal-last-symbol* )
			 t)
			((consp x) (traverse-list x))
			(t (setf (gethash x *traversal-ht*) t)
			   (when (typecase x
				   ;;(symbol (traverse-symbol x))
				   (array (when (eq (array-element-type x) t) (traverse-array x)))
				   (standard-object  (traverse-standard-object x))
				   (sb-sys::system-area-pointer nil)
				   (sb-ext::weak-pointer nil)
				   (sb-kernel::fdefn nil)
				   (sb-sys::stream nil)
				   (sb-sys::string-stream nil)
				   (sb-sys::file-stream nil)
				   (condition nil)
				  ;; (sb-sys::code-component nil);; FIXME
				   (sb-pcl::slot-object nil)
				   (structure-object (traverse-structure x))
				   (hash-table (traverse-hash-table x))
				  ; (package (traverse-package x))
				   (function #+allegro COMPILED-FUNCTION
					     (traverse-compiled-function x))
				   (otherwise (setq *traverse-object-last-object* x)
					      (break ";; TRAVERSE-OBJECT: unknown type ~s" (type-of x)))
				   )
			     (setf (gethash x *traversal-ht*) :hot))
			   )))))))))

#+allegro
(defun traverse-object (x)
  (let ((type (type-of x)))
    (cond ((eq type 'EXCL::NOT-A-TYPE))
	  (t (typecase x
	       (number nil)
	       (character nil)
	       (symbol nil)
	       (package nil)
	       (otherwise
		(let ((been-there (gethash x *traversal-ht*)))
		  (cond ((eq been-there :hot)
			 (format t "*traversal-last-symbol* = ~s :hot~%" *traversal-last-symbol* )
			 (break "*traversal-last-symbol* = ~s :hot" *traversal-last-symbol* )
			 t)
			(been-there nil)
			((funcall *traversal-test* x)
			 (break "*traversal-last-symbol* = ~s" *traversal-last-symbol* )
			 t)
			((consp x) (traverse-list x))
			((excl::sysvectorp x) nil)
			(t (setf (gethash x *traversal-ht*) t)
			   (when (typecase x
					;(symbol (traverse-symbol x))
				   (array (when (eq (array-element-type x) t) ; is this enough element-types?
					    (traverse-array x)))
				   (standard-object  (traverse-standard-object x))
				   (lx::weak-pointer nil)
				   (structure (traverse-structure x))
				   (hash-table (traverse-hash-table x))
					;(package (traverse-package x))
				   (COMPILED-FUNCTION (traverse-compiled-function x))
				   (function (traverse-function x))
				   (readtable nil)
				   (excl::closure (traverse-closure x))
				   (otherwise (setq *traverse-object-last-object* x)
					      (break ";; TRAVERSE-OBJECT: unknown type ~s" (type-of x)))
				   )
			     (setf (gethash x *traversal-ht*) :hot))
			   )))))))))

;(describe (type-of *traverse-object-last-object*))

(defun traverse-symbol (sym)
  (unless (member sym *traversal-ignore-symbols* )
    (let ((*traversal-last-symbol* sym)
	  hot)
      (declare (special *traversal-last-symbol*))
      (when (and (boundp sym) (traverse-object (symbol-value sym))) (setq hot t))
      (when (and (fboundp sym) (traverse-object  (symbol-function sym))) (setq hot t))
      (when (traverse-object (symbol-plist sym)) (setq hot t))
      hot)))

;;; (defun traverse-list (l)
;;;   (loop with hot 
;;; 	for x on l by #'cdr
;;; 	while x
;;; 	for been-there = (gethash x *traversal-ht*)
;;; 	;; the hash-table is needed here to handle circular lists  -- uggh
;;; 	when (eq been-there :hot) 
;;; 	  do (setq hot t)
;;; 	until been-there ; do not traverse shared list structure multiple times
;;; 	do (setf (gethash x *traversal-ht*) t)
;;; 	   (when (if (consp x)
;;; 		     (traverse-object (car x))
;;; 		     (traverse-object x))
;;; 	     (setq hot t))
;;; 	finally (return hot)))


(defun traverse-list (l)
  (loop with hot 
	while l
	for been-there = (gethash l *traversal-ht*)
	for x = (if (consp l) (car l) l)
	do (setq l (if (consp l) (cdr l) nil))
	;; the hash-table is needed here to handle circular lists  -- uggh
	when (eq been-there :hot) 
	  do (setq hot t)
	until been-there ; do not traverse shared list structure multiple times
	do (setf (gethash l *traversal-ht*) t)
	   (when (traverse-object x)
	     (setf (gethash l *traversal-ht*) :hot)
	     (setq hot t))
	finally (return hot)))

(defun traverse-list (l)
  (let ((been-there (gethash l *traversal-ht*)))
    (cond ((eq been-there t)
	   t)
	  (been-there nil)
	  (t
	   (let ((hot (traverse-object (car l))))
	     (setf (gethash l *traversal-ht*) (if hot :hot t))
	     (when (traverse-object (cdr l))
	       (setq hot t)
	       (setf (gethash l *traversal-ht*) :hot))
	     hot)))))

(defun traverse-array (x)
  (multiple-value-bind (1d-arr offset n) (underlying-simple-vector x)
    (declare (simple-vector 1d-arr))
    (loop with hot
	  for i fixnum from offset below n
	  for x = (aref 1d-arr i)
	  when (and x (traverse-object x)) 
	    do (setq hot t)
	  finally (return hot))))

(defun traverse-hash-table (ht)
  (loop with hot 
	for value being the hash-values of ht using (hash-key key)
	do (when (traverse-object value) (setq hot t))
	   (when (traverse-object key) (setq hot t))
	finally (return hot)))

(defun traverse-package (pkg)
  (let (hot)
    (do-symbols (sym pkg) (when (traverse-object sym) (setq hot t)))
    hot))

(defparameter *default-traversal-packages* nil)

(defun default-traversal-packages ()
  (or *default-traversal-packages*
      (list-all-packages)))

(defun default-traversal-test (x)
  (and x (eq x (weak-pointer-value *traversal-target*))))

(defun traverse-world (&optional (test 'default-traversal-test) (packages (default-traversal-packages) ))
  (setq *traverse-object-last-object* nil *traversal-last-symbol* nil)
  (let ((*traversal-test* test)
	(*traversal-ht* (make-hash-table :test #'eq)))
    (clrhash *traversal-ht*)
    (traverse-object packages )))

(defun traverse-world (&optional (test 'default-traversal-test) (packages (default-traversal-packages) ))
  (setq *traverse-object-last-object* nil *traversal-last-symbol* nil)
  (let ((*traversal-test* test)
	(*traversal-ht* (make-hash-table :test #'eq)))
    (clrhash *traversal-ht*)
    (loop for pkg in packages
	  do (do-symbols (sym pkg)
	       (traverse-symbol sym)))))

(defun find-references-to-object (object &optional (packages (default-traversal-packages) ) )
  (traverse-world #'(lambda (x) (eq x object)) packages ))

(defun find-references-to-objects (objects &optional (packages (default-traversal-packages) ) )
  (traverse-world #'(lambda (x) (member x objects)) packages ))

#+(or cmu allegro sbcl)
(progn

;;; This could be completely flushed and handled by traverse-structure,
;;; but with this function, the stack-frame structure is simplified.
(defun traverse-standard-object (object)
  (flet ((slot-name (slot)
	   #+allegro (slot-value slot 'excl::name)
	   #+cmu (pcl::slot-definition-name slot)
	   #+sbcl (sb-pcl::slot-definition-name slot)
	   ))
    (loop with type = (class-of object)
	  with hot
	  for slot in (class-slots type)
	  for slot-name = (slot-name slot)
	  when (slot-boundp object slot-name)
	    do (when (traverse-object (slot-value object slot-name)) 
		 (setq hot t))
	  finally (return hot))))


(defun traverse-structure (object)
  (flet ((slot-name (slot)
	   #+allegro (slot-value slot 'excl::name)
	   #+cmu (pcl::slot-definition-name slot)
	   #+sbcl (sb-pcl::slot-definition-name slot)
	   ))
    
    (loop with type = (class-of object)
	  with hot
	  for slot in (class-slots type)
	  for slot-name = (slot-name slot)
	  when (slot-boundp object slot-name)
	    do (when (traverse-object (slot-value object slot-name))
		 (setq hot t))
	  finally (return hot))))

#|
(describe 'traverse-world)


(disassemble 'inspect::inspect-ctl)
(disassemble ')
(apropos 'SYS::INSPECTOR-FUNCTION)
(type-of #'traverse-world) = compiled-function
(apropos 'compiled-function)
(describe #'traverse-world)
(disassemble 'disassemble)
(apropos 'inspect- 'inspect)
|#

;;; FIXME
(defun traverse-compiled-function (object)
  )

;;; FIXME
(defun traverse-function (object)
  )

;;; FIXME
(defun traverse-closure (object)
  )


) ; end #+(or cmu allegro) progn


#|
(defclass foo () (a))
(defstruct foos (a))
(typep (make-instance 'foo) 'foo)
(typep (make-instance 'foo) 'standard-object) = t
(typep (make-instance 'foo) 'structure) = nil
(typep (make-foos) 'structure) = t
|#



#+cmu
(defun clear-eval-stack ()
  (loop with stack = c::*eval-stack*
	for i fixnum from (c::current-stack-pointer) below (length stack)
	do (setf (c::eval-stack-ref i) nil)))

#|
(load (compile-file "$F3D/lisp/lisp/traverse-world.lisp"))

(setq *default-traversal-packages*
      (loop for pkg-name in
	    '(CONFIG lx OBJ IMG GUI LCL GL CME MATH 
	      EV-PATHNAMES SYSTEM-TOOL LISPTK SMC 
	      FREEDIUS-IO FREEDIUS-MODELS-V0 IC TK-INSPECT QFFI
	      TK-INSPECT CL-USER INSPECT
	      SWANK SLIME-NREGEX SWANK-BACKEND SWANK-COMPLETION-TEST SWANK-LOADER 
	      SWANK-IO-PACKAGE SWANK-MOP)
	    for pkg = (find-package pkg-name)
	    when pkg collect pkg))

(setq *default-traversal-packages* nil)

(setq *traversal-target* (make-weak-pointer (gui::view-image (gui::top-view))))
(setq *traversal-target* (make-weak-pointer (gui::2d-world *traversal-target*)))
(setq *traversal-target* (make-weak-pointer *))

(TRAVERSE-SYMBOL 'DOCUMENTATION)
(fboundp 'DOCUMENTATION)
(traverse-object (symbol-function 'DOCUMENTATION))
(class-slots (class-of (symbol-function 'DOCUMENTATION)))

(progn (setq * nil ** nil *** nil 
	     $ nil $$ nil $$$ nil
	     / nil // nil /// nil)
       #+cmu (clear-eval-stack)
       (let (;(c::*info-environment* nil)
	     )
	 (progn (clrhash swank::*object-to-presentation-id*)
		(clrhash swank::*presentation-id-to-object*) 
		(swank::reset-inspector)
		)

	 (lx::traverse-world)))

EXCL::*RESOURCE-VECTOR*

(setq *traversal-target* (make-weak-pointer (list 'a 'b)))
(full-gc)

(let ((*traversal-test* 'default-traversal-test)
      (*traversal-ht* (make-hash-table :test #'eq)))
  (setq *traverse-object-last-object* nil *traversal-last-symbol* nil)
  (clrhash *traversal-ht*)
  (traverse-symbol 'gui::*3d-worlds* ))

(traverse-object 'gui::*3d-worlds*)
(setq *traversal-target* nil)
(setq *traversal-target* (make-weak-pointer *))

kernel::*EVAL-STACK* /  LISPTK::$$ LISPTK::$
(setq / nil // nil LISPTK::$$ nil LISPTK::$ nil)

(setq lx::*traversal-target*
      (make-weak-pointer (nth 1 (loop for val being the hash-values of img::*c-image-id-hash-table*
				      collect (lx::weak-pointer-value val))))
      )

(setq lx::*traversal-target*
      (make-weak-pointer (loop for val being the hash-values of img::*c-image-id-hash-table*
			       thereis (lx::weak-pointer-value val)))
      )

(lx::weak-pointer-value lx::*traversal-target*)

(clrhash CME::*CONTAINER-HASH-TABLE*)

(describe *traverse-object-last-object*)
(type-of *traverse-object-last-object*)

(progn *traversal-last-symbol*)

(user::find-references-to-object (nth 0 (list-weak-set ic::misc-weak-set t)))
(user::find-references-to-objects (list-weak-set ic::misc-weak-set t))

(cme::clear-all-cme-object-sets)
(loop for x first cme::*sensitivity-bucket-cons-cell-freelist* then (cdr x)
      while x
      do (setf (car x) nil))

UGGH BLETCH FOOBAZ !!!!
I found dangling pointers from CLOS::*METHOD-SORTING-RESOURCE* to instances of my objects,
in particular:

TRAVERSE-SYMBOL:
   Required arg 0 (SYM): CLOS::*METHOD-SORTING-RESOURCE*
-> :p
TRAVERSE-OBJECT:
   Required arg 0 (X): ((#<Simple-Vector T 16 169200E> . #<Simple-Vector T 16 1692056>))
-> :i 0
#<List 1637C19>

[0] (#<Simple-Vector T 16 169200E> . #<Simple-Vector T 16 1692056>)
>> 0
#<Cons 1637C21>

[0: CAR] #<Simple-Vector T 16 169200E>
[1: CDR] #<Simple-Vector T 16 1692056>
>> 0
#<Simple-Vector T 16 169200E>

[0] #<2d World Alv-Dtm of  2D-WORLD #X5D5944E>
[1] #<Stream OSI-BUFFERED-STREAM 5CD03D6>
[2] INSPECT
[3] 317
[4] 166
[5] 0
[6] #<X-Screen #X5D014FE>
[7] 212
[8] 117
[9] T
[10] IC::X-BITBLT
[11] 0
[12] 0
[13] 0
[14] 0
[15] 0
>> 
|#
