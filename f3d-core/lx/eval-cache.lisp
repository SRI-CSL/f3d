(in-package :lisp-extensions)

;;; ********************  QUICK-EVAL-CACHE  ********************

#|
This is a simplified implementation of the CME eval-cache mechanism.
This implementation does not build the full network of relations between
cache entries.
|#

(defstruct (eval-cache-dummy))

(defvar *eval-cache-probe* nil
  "Binding this to non-nil will cause cache misses to return nil rather than evaluating body.")
  
(defvar *eval-cache-recompute* nil)
 
(defvar *eval-cache-object-hash-table* (make-hash-table :test #'eq ))

;;(declaim (inline quick-eval-cache-find-match))
;;(declaim (notinline quick-eval-cache-find-match))

;;;(defun quick-eval-cache-find-match (fn args)
;;;  (let ((hash-table (get fn :function-cache)))
;;;    (and hash-table (gethash args hash-table))))

(defmacro def-eval-cache-search-rule (name args &body body)
  (let ((fn-name (intern (string-append (string name) "-SEARCH-RULE"))))
    `(progn (defun ,fn-name ,args . ,body)
      (setf (get ',name  'eval-cache-search-rule) #',fn-name))))

(defun quick-eval-cache-find-match (fn args)
  (let ((hash-table (get fn :function-cache)))
    (multiple-value-bind (result foundp)
	(and (not *eval-cache-recompute*) hash-table (gethash args hash-table))
      (if foundp
	  (values result t)
	  (let ((rule (get fn 'eval-cache-search-rule)))
	    (when rule
	      (let ((result (funcall rule (cons fn args))))
		(if result
		    (values result t)))))))))

(defvar *eval-cache-inhibit-caching* nil)

(defun quick-eval-cache-make-entry (fn arglist result-list)
  (unless *eval-cache-inhibit-caching*
    (let ((hash-table (or (get fn :function-cache)
			  (setf (get fn :function-cache)
				(make-hash-table :test #'equal)))))
      (setf (gethash arglist hash-table) result-list)
      (setf (gethash (car result-list) *eval-cache-object-hash-table*)
	    (list fn arglist))))
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
    (unless (or (typep val 'eval-cache-dummy)
		(eq val 'eval-cache-probe-failure))
      (values-list vals))))

(defmacro eval-cache-probe (&body body)
  `(eval-cache-probe-internal
     #'(lambda() . ,body)))

(defun eval-cache-flush (object)
  (let ((entry (gethash object *eval-cache-object-hash-table*)))
    (when entry
      (let ((ht (get (car entry) :function-cache)))
	(when ht
	  (remhash (cadr entry) ht))
	(remhash object *eval-cache-object-hash-table*)))))


(defun eval-cache-flush-function (fn)
  (let ((ht (get fn :function-cache)))
    (when ht
      (loop for obj being the #+(or allegro cmu sbcl) hash-values of ht
	    using (hash-key entry)
	    do (remhash entry ht)
	       (remhash obj *eval-cache-object-hash-table*)
	    collect obj)
      ;;(when ht (clrhash ht))
      )))

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
	       
	else when (not (member thing *lambda-list-keywords*))
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
	       (loop for val being the  #+(or allegro cmu sbcl) hash-values
		     of (get function-name :function-cache)
		     thereis val)
	       #+never ;; this is broken with ilisp
	       (y-or-n-p (format nil "~aFlush function cache of ~a~%~a"
					(lisp-prompt-string)
					function-name (lisp-prompt-string)))
               #+swanky
               (swank::y-or-n-p-in-emacs
                (format nil "~aFlush function cache of ~a~%~a"
                        (lisp-prompt-string)
                        function-name (lisp-prompt-string)))
               )
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


