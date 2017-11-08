(in-package :lisp-extensions)

;;; ********************  WEAK-EVAL-CACHE  ********************

#|
This is a simplified implementation of the CME eval-cache mechanism.
This implementation does not build the full network of relations between
cache entries.
|#

#|
Ideas for WEAK-EVAL-CACHE

   1.  WEAK-OBJECT-HANDLEs (WOH): Objects that need weak caching support the method
       "WEAK-OBJECT-HANDLE" (WOH) which contains a weak pointer to the object.  Thus the object and
       its WOH are one-to-one.  Even after an object is destroyed by the garbage collector, its WOH
       can persist providing a meaningful (unique) handle for the object.  (Note 1)

   2.  WEAK-EVAL-CACHE-HASH-TABLE (WECHT): 

       WECHTs use weak EQL-hash keys that are called WEAK-EVAL-CACHE-KEYS (WECK).  When a WECK dies,
       the corresponding WECHT entry is allowed to die.

       The values in a WECHT may be weakly referenced, using weak-pointers, or strongly (normally)
       referenced.

       See the discussion below describing the behavior of WECHTs with variaous combinations
       of strong/weak keys/values. 

   3.  WEAK-EVAL-CACHE-KEY (WECK): A WECK is an sexpression representing the computation used to
       compute the corresponding WECHT value.  A WECK contains an arbitrary mix of symbols, numbers,
       strings, objects, and WEAK-OBJECT-HANDLES.  

       WECKs are "interned" (ie. made unique) using an equal-hash HT called the WECKHT.  Since the
       WECKHT is not a weak hash-table, some explicit action is required to remove its entries.
       (Note 2).

   4.  WEAK-EVAL-CACHE-INVERSE-HASH-TABLE (WECIHT):

       The WECIHT is a weak hash-table associating values back to their keys.  This is the inverse
       of the WECHT.  Thus it is possible to generate an s-expression representing the computation
       that generated the value.

WECHT behavior:

  key    value     behavior
  _______________________________________________________________

  strong strong   "traditional" eval-cache behavior: keeps key and value alive until cache entry
                   is explicitly removed.

   weak   weak     Cache does not prevent any object GC.  The intact cache 

   weak  strong    keeps value alive until key dies (Note 2)

  strong  weak     allows value to die while keeping objects in key alive.  This
                   allows the value to be recomputed with a single level of computation.

Note 1: The original EVAL-CACHE in FREEDIUS (and image-calc) maintained a more complete network
        representing the "web of computations" that have been cached.  Objects could be
        "spliced-out" of the network to allow their destruction and replaced by a surrogate, leaving
        the structure of the network intact.  WEAK-OBJECT-HANDLEs provide a more elegant mechanism
        for this.


Note 2:  Killing WECKs:  The only strong pointers to WECKs are from the WECKHT.  
  


|#



#+cmu
(eval-when (eval load compile)
(import '(;;ext::make-weak-pointer 
	  ext::weak-pointer-p ext::weak-pointer-value))
)

#+sbcl
(eval-when (eval load compile)
(import '(;;sb-ext::make-weak-pointer 
	  sb-ext::weak-pointer-p sb-ext::weak-pointer-value))
)



#+cmu
(progn

(defparameter *weak-pointer-count* 0)

(defun make-weak-pointer (o)
  (incf *weak-pointer-count*)
  (ext::make-weak-pointer o))

(defun make-weak-hash-table ()
  (make-hash-table :test #'eql :weak-p t))

(defun finalize-object (object fn &optional weak-pointer)
  (if weak-pointer
      ;; generate finalization using the preexisting weak-pointer
      (push (cons weak-pointer fn) ext::*objects-pending-finalization*)
      (ext::finalize object fn))

  ;; return the weak-pointer associated with the finalization
  (or weak-pointer
      (caar ext::*objects-pending-finalization*))
  )

;;; return the weak-pointer associated with the finalization
#+never
(defun finalize-object (object fn &optional weak-pointer)
  (declare (ignore weak-pointer))
  (ext::finalize object fn)
  (caar ext::*objects-pending-finalization*) ; return the weak pointer
  )

(defun cancel-object-finalization (object)
  (ext:cancel-finalization object))

(defun full-gc ()
  (ext:gc :full t))

) ; end #+cmu progn


#+sbcl
(progn

(defparameter *weak-pointer-count* 0)

(defun make-weak-pointer (o)
  (incf *weak-pointer-count*)
  (sb-ext::make-weak-pointer o))

(defun make-weak-hash-table ()
  (make-hash-table :test #'eql :weakness :key))

(defun finalize-object (object fn &optional weak-pointer)
  (if weak-pointer
      ;; generate finalization using the preexisting weak-pointer
      (sb-impl::with-finalizer-store-lock
	(push (list weak-pointer fn nil)
	      sb-impl::*finalizer-store*))
      (sb-ext::finalize object fn))

  (or weak-pointer
      (caar sb-impl::*finalizer-store*)))


(defun cancel-object-finalization (object)
  (sb-ext:cancel-finalization object))

(defun full-gc ()
  (sb-ext:gc :full t))

) ; end #+sbcl progn

#+allegro
(progn

;;; Crummy Allegro provides no way to ask if an object is a weak-vector.

(defstruct (weak-pointer (:constructor %make-weak-pointer)) 
  pointer)

(defun make-weak-pointer (object)
  "Creates a new weak pointer which points to OBJECT. For
   portability reasons, OBJECT most not be NIL."
  (assert (not (null object)))
  (let ((wv (excl:weak-vector 1)))
    (setf (svref wv 0) object)
    (%make-weak-pointer :pointer wv)))

(defun weak-pointer-value (weak-pointer)
  "If WEAK-POINTER is valid, returns its value. Otherwise, returns NIL."
  (svref (weak-pointer-pointer weak-pointer) 0))

(defun make-weak-hash-table ()
  (make-hash-table :weak-keys t))

(defun finalize-object (object fn &optional weak-pointer)
  (declare (ignore weak-pointer))
  (excl::schedule-finalization object fn))

(defun full-gc ()
  (excl:gc t))

) ; end #+allegro progn

(defun find-hash-table-value-broken-weak-pointers (ht)
  (let ((broken-keys nil) 
	(total-cnt (hash-table-count ht)))
    (maphash #'(lambda (key val)
		 (when (consp val) (setq val (car val)))
		 (when (and (weak-pointer-p val)
			    (null (nth-value 1 (weak-pointer-value val))))
		   (push key broken-keys)))
	     ht)
    (values broken-keys total-cnt)))
	
(defun remove-hash-table-value-broken-weak-pointers (ht)
  (mv-bind (broken-keys total-cnt) (find-hash-table-value-broken-weak-pointers ht)
    (loop for key in broken-keys do (remhash key ht))
    (values (length broken-keys) total-cnt)))
		   


(defun follow-weak-pointer (object)
  (if (weak-pointer-p object)
      (weak-pointer-value object)
      object))



(in-package :lx)

#+never
(defmethod weak-object-handle ((object t))
  nil)

(defmethod weak-object-handle ((object t))
  object)

(defparameter *WEAK-EVAL-CACHE-KEY-HT* (make-hash-table :test #'equal))

(defun make-weak-eval-cache-key (arglist)
  (let ((key (loop for arg in arglist
		   collect (or (weak-object-handle arg) arg))))
    (or (gethash key *WEAK-EVAL-CACHE-KEY-HT*)
	(setf (gethash key *WEAK-EVAL-CACHE-KEY-HT*) key))))

(defun make-weak-result-list (result-list)
  (loop for val in result-list
	collect (or (weak-object-handle val) val)))

(defun unmake-weak-result-list (weak-result-list)
  (loop with dead-value-p 
	for result in weak-result-list
	when (weak-pointer-p result)
	  do (setq result (weak-pointer-value result))
	     (unless result (setq dead-value-p t))
	collect result into results
	finally (return (values results dead-value-p))))
	
	
(defun result-list-contains-broken-weak-pointer (result-list)
  (loop for result in result-list
	thereis (and (weak-pointer-p result)
		     (null (nth-value 1 (weak-pointer-value result))))))



(defvar *eval-cache-probe* nil
  "Binding this to non-nil will cause cache misses to return nil rather than evaluating body.")

(defvar *eval-cache-recompute* nil)
  
;;(defparameter *eval-cache-object-hash-table* (make-hash-table))

(defparameter *eval-cache-object-hash-table* (make-weak-hash-table))
;;(defparameter *eval-cache-object-hash-table* nil)

(defmacro def-eval-cache-search-rule (name args &body body)
  (let ((fn-name (intern (string-append (string name) "-SEARCH-RULE"))))
    `(progn (defun ,fn-name ,args . ,body)
      (setf (get ',name  'eval-cache-search-rule) #',fn-name))))

;;; by default this makes weak keys and weak values
(defun quick-eval-cache-find-match (fn args)
  (let ((hash-table (get fn :function-cache))
	(hash-key (make-weak-eval-cache-key args)))
    (multiple-value-bind (result foundp)
	(and (not *eval-cache-recompute*) hash-table (gethash hash-key hash-table))
      (if (and foundp (not (result-list-contains-broken-weak-pointer result)))
	  (values (unmake-weak-result-list result) t)
	  (let ((rule (get fn 'eval-cache-search-rule)))
	    (when rule
	      (let ((result (funcall rule (cons fn args))))
		(if result
		    (values result t)))))))))

(defvar *eval-cache-inhibit-caching* nil)
(defvar *eval-cache-function-symbols* nil)

(defun quick-eval-cache-make-entry (fn arglist result-list)
  (unless *eval-cache-inhibit-caching*
    (let ((hash-table (or (get fn :function-cache)
			  (progn (pushnew fn *eval-cache-function-symbols*)
				 (setf (get fn :function-cache)
				       ;;(make-weak-hash-table)
				       (make-hash-table :test #'eql)
				       ))))
	  (hash-key (make-weak-eval-cache-key arglist))
	  (weak-result-list (make-weak-result-list result-list)))
      (setf (gethash hash-key hash-table) weak-result-list)
      (when *eval-cache-object-hash-table*
	(setf (gethash (car weak-result-list) *eval-cache-object-hash-table*)
	      (list fn hash-key)))))
  (values-list result-list))

(defun eval-cache-process-results (form result-list)
  (quick-eval-cache-make-entry (car form) (cdr form) result-list)
  result-list
  )

(defvar *eval-cache-set-entry-values* nil)

;;; no multiple-values - hash table caching on function only.
;;; no relational network.
(defmacro eval-cache ((fn . args) &body body)
  (let ((arglist (gensym)))
    `(let ( ;;(,arglist ,(if (cdr args) `(list . ,args) (car args)))
	   (,arglist ,`(list . ,args))
	   )
      (mv-bind (result found)
	  (quick-eval-cache-find-match ',fn ,arglist)
	(cond (found
	       (values-list result))
	      (*eval-cache-probe*
	       (throw 'eval-cache-probe 'eval-cache-probe-failure))
	      (t
	       (quick-eval-cache-make-entry ',fn ,arglist
					    (or *eval-cache-set-entry-values*
						(multiple-value-list (progn . ,body))))))))))


(defmacro set-eval-cache (form &body values)
  `(let ((*eval-cache-set-entry-values* (list . ,values)))
     ,form))
;;; These setf forms do not work with eval-cache being a macro
#+never
(defun (setf eval-cache) (value form)
  (set-eval-cache form value))

#+never
(defsetf eval-cache (form) (value)
  `(set-eval-cache ,form . ,value))

(defun eval-cache-probe-internal (fn)
  (let* ((*eval-cache-probe* t)
	 (vals (multiple-value-list (catch 'eval-cache-probe (funcall fn))))
	 (val (car vals)))
    (unless (eq val 'eval-cache-probe-failure)
      (values-list vals))))

(defmacro eval-cache-probe (&body body)
  `(eval-cache-probe-internal
     #'(lambda() . ,body)))

(defun eval-cache-flush (object)
  (when *eval-cache-object-hash-table*
    (let* ((weak-result (or (weak-object-handle object) object))
	   (entry (gethash weak-result *eval-cache-object-hash-table*)))
      (when entry
	(let ((ht (get (car entry) :function-cache)))
	  (when ht
	    (remhash (cadr entry) ht))
	  (remhash weak-result *eval-cache-object-hash-table*))))))


(defun eval-cache-flush-function (fn)
  (let ((ht (get fn :function-cache)))
    (when ht
      (loop for result-list being the #+(or allegro cmu sbcl) hash-values of ht
	    using (hash-key entry)
	    do (remhash entry ht)
	       (when *eval-cache-object-hash-table*
		 (loop for obj in result-list
		       do	(remhash obj *eval-cache-object-hash-table*)))
	    append result-list)
      ;;(when ht (clrhash ht))
      )))

(defun eval-cache-flush-form (form)
  (multiple-value-bind (values entry)
      (eval-cache-find-match (eval (make-eval-cache-pattern form)))
    (ignore values)
    (when entry (eval-cache-flush-entry entry))))

(defvar *keyword-package* (find-package "KEYWORD"))

(defvar *lambda-list-keywords*
  '(&optional &rest &key &allow-other-keys &aux))

(defun vars-from-arglist (arglist)
  (loop with keys = nil
	while arglist
	for thing = (pop arglist)
	when (listp thing) do (setq thing (car thing))

	when (eq thing '&key) do (setq keys t) ;;  and collect '&key
	else when (eq thing '&rest) do (pop arglist)
	       
	else when (not (memq thing *lambda-list-keywords*))
	       when keys
		 collect (intern (string thing) *keyword-package*)
		 and collect thing
	     else collect thing
	until (eq thing '&aux)))

(defmacro defun-cached (function-name args &body body)
  (let* ((documentation (and (stringp (car body)) (list (pop body))))
	 (declarations
	  (loop while (and (listp (car body)) (eq (caar body) 'declare))
		append (list (pop body))))
	 (vars (vars-from-arglist args)))
    (when (and (fboundp function-name)
	       (get function-name :function-cache)
					#| I don't know how to do this in LUCID LISP
               (boundp 'compiler:*compile-function*)
	       (FUNCALL compiler:*COMPILE-FUNCTION* ':TO-CORE-P) |#
	       (loop for val being the #+(or allegro cmu sbcl) hash-values
		     of (get function-name :function-cache)
		     thereis val)
	       #+never ;; this is broken with ilisp
	       (y-or-n-p (format nil "~aFlush function cache of ~a~%~a"
					(lisp-prompt-string)
					function-name (lisp-prompt-string))))
      (format t "~a cache entries flushed~%"
	      (length (eval-cache-flush-function function-name))))
    `(progn
      #+record-source-file
      (record-source-file
       ',function-name
       'defun-cached
       ,(compile-file-source-pathname))
      (defun ,function-name ,args
	,@documentation ,@declarations
	(eval-cache (,function-name ,@vars)
	    . ,body))
      )))

(defun method-spec-from-args (args)
  (loop for arg in args
	when (and (symbolp arg) (member arg '(&key &rest &optional)))
	  return method-spec
	when (consp arg)
	  collect (cadr arg) into method-spec
	else collect t into method-spec
	finally (return method-spec)))

(defmacro defmethod-cached (function-name args &body body)
  (let* ((documentation (and (stringp (car body)) (list (pop body))))
	 (declarations
	  (loop while (and (listp (car body)) (eq (caar body) 'declare))
		append (list (pop body))))
	 (vars (vars-from-arglist args)))
    (when (and (fboundp function-name)
	       (get function-name :function-cache)
					#| I don't know how to do this in LUCID LISP
               (boundp 'compiler:*compile-function*)
               (FUNCALL compiler:*COMPILE-FUNCTION* ':TO-CORE-P) |#
	       (loop for val being the #+(or allegro cmu sbcl) hash-values
		     of (get function-name :function-cache)
		     thereis val)
	       #+never ;; this is broken with ilisp
	       (y-or-n-p (format nil "~aFlush function cache of ~a~%~a"
					(lisp-prompt-string)
					function-name (lisp-prompt-string))))
      (format t "~a cache entries flushed~%"
	      (length (eval-cache-flush-function function-name))))
    `(progn
      #+record-source-file
      (record-source-file
		  '`(method ,function-name . ,(method-spec-from-args args))
		  'defmethod-cached
		  ,(compile-file-source-pathname))
       
       (defmethod ,function-name ,args
	 ,@documentation ,@declarations
	 (eval-cache (,function-name ,@vars)
	     . ,body))
       )))
 
;;;#+never 
(progn 

;;; ARE THESE METHODS APPLIED FAR TOO GLOBALLY, or is the
;;; WEAK-OBJECT-HANDLE method used inapropriately for purposes other
;;; than weak-eval-cache?

(defmethod weak-object-handle ((object property-list-mixin))
  (or (get-prop object :weak-object-handle)
      (setf (get-prop object :weak-object-handle)
	    (make-weak-pointer object))))

(defmethod weak-object-handle ((object property-list-struct))
  (or (get-prop object :weak-object-handle)
      (setf (get-prop object :weak-object-handle)
	    (make-weak-pointer object))))

) ; end progn

;;; TEST CODE


#|
(in-package :transforms)

(defmethod remove-child ((cs CS-CHILDREN-HASH-TABLE-MIXIN) child)
  (let ((ht (%children cs))
	(key child))
    (when (typep child 'gui::2d-world) (break))
    (when (gethash key ht) 
      (setf (parent child) nil)
      (remhash key ht))))
|#

#|


#|
These are globals that retain pointers to elements of the alv 3d-world, particularly
its images.  This screws up the alv 2d-worlds.  They lost their parent, the alv 3d-world.
Another problem is that (:method initialize-instance :after ((feature-set feature-set) ...)
calls (add-feature-set feature-set world).  Thus if a new 3d-world must be created, then
the feature-sets must somehow be added to the 3d-world.  The explicit network structure of
coordinate-systems, worlds, objects, images, etc complicates the caching.
|#
(let ((3d-world (gui::get-3d-world-named "alv")))
  (transforms::remove-child (parent 3d-world) 3d-world)  ; the coordinate-transform network
 ; (setf gui::*3d-worlds* (remove 3d-world gui::*3d-worlds*)) ; bad idea
  (setq cme::*alv-3d-world2* nil)  ; gratuitous global
  ;;(setq cme::*LOADING-MODELS-FROM-CONTAINER* nil)
  (clrhash CME::*CONTAINER-HASH-TABLE*)
  (clrhash cme::*load-feature-sets-times-hash-table*)
  (clrhash GUI::*OBJECT-SELECT-NAME-HT*)
  )

(let* ((2d-world (gui::get-2d-world-named "alv-2-44" (list (gui::get-3d-world-named "alv")))))
  (setf (gui::children 2d-world) nil) ; flush the views
  )
win!

(gui::children (gui::get-2d-world-named "alv-2-44" (list (gui::get-3d-world-named "alv"))))

(pushnew (cme::load-3d-world "alv" (cme::load-site-glue "alv" cme::*radius-site-glue*))
	 gui::*3d-worlds*)

(progn
  #+never
  (let ((3d-world (gui::get-3d-world-named "alv")))
    (transforms::remove-child (parent 3d-world) 3d-world) ; the coordinate-transform network
    (setf gui::*3d-worlds* (remove 3d-world gui::*3d-worlds*)))

  (setq cme::*alv-3d-world2* nil)  ; gratuitous global
  ;;(setq cme::*LOADING-MODELS-FROM-CONTAINER* nil)
  (clrhash CME::*CONTAINER-HASH-TABLE*)
  (clrhash cme::*load-feature-sets-times-hash-table*)
  (clrhash GUI::*OBJECT-SELECT-NAME-HT*)
  )

(clrhash swank::*object-to-presentation-id*)
(clrhash swank::*presentation-id-to-object*)
(setq lx::*traversal-target* nil)

(eval-cache-flush-function 'cme::load-3d-world)
(eval-cache-flush-function 'cme::load-site-glue)
(eval-cache-flush-function 'cme::load-2d-world-image-pyramid)
(eval-cache-flush-function 'cme::site-2d-world-pyramid-loadable-p)
(eval-cache-flush-function 'transforms::lvcs-with-name)
(eval-cache-flush-function 'transforms::lvcs-with-position)
(eval-cache-flush-function 'gui::make-terrain-model)


(push common-lisp-user::*alv-3d-world* gui::*3d-worlds*)

(in-package :img)

(setq gui::*img* (image-window (gui::view-image (gui::top-view)) 100 100 256 256))
gui::*img*
(setf (get-prop gui::*img* :INDIRECTED-TO) nil)
(setq gui::*img-wp* (lx::make-weak-pointer gui::*img*)
      gui::*img* nil)
(lx::weak-pointer-value gui::*img-wp*)

(clrhash (transforms::%children *))

(lx::weak-pointer-value *)

(progn (clrhash swank::*object-to-presentation-id*)
       (clrhash swank::*presentation-id-to-object*) 
       (swank::reset-inspector)
)

(setq *obj1* (list 1)
      *final1* (lx::finalize-object *obj1* #'(lambda (o) (format t "finalized *obj1"))))

(setq *obj1* nil)


(describe *final1*) 


(gc-and-test-for-broken-pointers)

gui::*interactor*

(loop for val being the hash-values of *c-image-id-hash-table*
      collect (lx::weak-pointer-value val))

(defclass foo () ())

(setq *xxht* (make-hash-table))
(setf (gethash 1 *xxht*) (lx::make-weak-pointer (make-instance 'foo))) ; gc gets rid of this


(defun cache-contents (fn)
  (loop for val being the hash-values of (get fn :function-cache) using (hash-key key)
	collect (list key val)))

(cache-contents 'img::IMAGE-ELEMENT-MIN-MAX)

(let ((l nil)) (do-symbols (sym (find-package :cme) l) 
		 (when (char= (aref (symbol-name sym) 0) #\*)
		   
		   (push sym l))))

(setq *all-caches*
      (loop with l
	    for pkg in (list-all-packages)
	    do (do-symbols (sym pkg)
		 (when (and (eq (symbol-package sym) pkg) (get sym :function-cache))
		   (push (list sym (hash-table-count (get sym :function-cache))) l)))
	    finally (return l)))

(do-symbols (sym :cme)
(apropos 'package-symbols)

Before GC reclaim images
(maphash #'(lambda (key val) (print (mapcar 'type-of val))) (get 'load-image :function-cache))
(EXTENSIONS:WEAK-POINTER) ...

(maphash #'(lambda (key val) (print (mapcar 'type-of val))) (get 'load-image :function-cache))
(maphash #'(lambda (key val) (format t "~s =~%   ~s~%" key val)) (get 'load-image :function-cache))
(maphash #'(lambda (key val) (format t "~s =~%   ~s~%" key val)) *c-image-id-hash-table*)
(maphash #'(lambda (key val) (print key)) lx::*eval-cache-object-hash-table*)
(maphash #'(lambda (key val) (format t "~s =~%   ~s~%" key val)) lx::*eval-cache-object-hash-table*)

(maphash #'(lambda (key val) (print (mapcar 'type-of key))) (get 'load-image :function-cache))
(maphash #'(lambda (key val) (print key)) (get 'load-image :function-cache))
(maphash #'(lambda (key val) (print key)) lx::*WEAK-EVAL-CACHE-KEY-HT*)

(describe (get 'load-image :function-cache))

(setq *alv-2-44a* (gui::view-image (gui::top-view)))
(setq *alv-2-44a* nil)

(setq *alv-2-44a-WOH* (lx::weak-object-handle (gui::view-image (gui::top-view))))
(setf (get-prop cme::*LOAD-PYRAMID-PANEL* :image-list) nil)
(let ((lx::*traversal-target* *alv-2-44a-WOH*))
  (setq * nil ** nil *** nil 
	$ nil $$ nil $$$ nil
	/ nil // nil /// nil)
  (lx::traverse-world))

(lx::weak-object-handle (lx::follow-weak-pointer *alv-2-44a-WOH*))

( image-from-cimage-id (image-id (lx::follow-weak-pointer *alv-2-44a-WOH*)))

(setq *c-image-id-hash-table* (make-hash-table))

(setq *alv-2-44b* (load-image "/opt/IU/radius/site-2d-worlds/alv/alv-2-44/full-IUTB/image.g0"))
(setq *alv-2-44b-WOH* (lx::weak-object-handle *alv-2-44b*)
      *alv-2-44b* nil)



(setq *rugby* (load-image "/m/rom1/quam/pix/rugby.pic"))
(setq *rugby-woh* (lx::weak-object-handle *rugby*)
      *rugby* nil)

(let ((lx::*traversal-target* *rugby-woh*))
  (lx::traverse-world))

(gui::push-image *rugby* (gui::selected-window))


(setq *alv-2-44b-WOH* (lx::weak-object-handle 
		       (load-image "/opt/IU/radius/site-2d-worlds/alv/alv-2-44/full-IUTB/image.g0")))

(eq *alv-2-44a* *alv-2-44b*)

(type-of (image-id *alv-2-44a*))

(alien::unparse-alien-type (image-id *alv-2-44a*))

(qffi::foreign-pointer-address (image-id *alv-2-44a*))

(in-package :gl)

(get 'make-glstipple :function-cache)


(sb-ext::gc )
(lx::full-gc)

(gui::view-image (gui::top-view))


|#





#|

(in-package :lx)

(defclass foo ()
    ((weak-object-handle :initform nil :initarg :woh)))

(defmethod weak-object-handle ((object foo))
  (or (slot-value object 'weak-object-handle)
      (setf (slot-value object 'weak-object-handle)
	    (make-weak-pointer object))))

(defun-cached make-a-foo (i)
  (make-instance 'foo))

(defun make-many-foos (n)
  (loop for i from 0 below n collect (make-a-foo i)))


(defparameter *foo-list* (make-many-foos 10))

(clrhash  (get 'make-a-foo :function-cache))

(maphash #'(lambda (key val) (format t "~s =~%   ~s~%" key val)) (get 'make-a-foo :function-cache))

(weak-object-handle (car *foo-list*))

(full-gc)

(setq wv (make-weak-pointer 1))
(describe wv)

(type-of wv)
(typep wv 'excl::weak-vector)

|#
