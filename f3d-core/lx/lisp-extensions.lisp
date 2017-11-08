(in-package :lisp-extensions)

;;;
;;; What to do?  SBCL 1.0.54 handles this already and throws a
;;; restartable error, but older versions are mute.

#-sbcl
(without-package-locks
(declaim (ftype (function ((double-float -.9d19 .9d19)) double-float) sin cos tan))
)


#|
(defun test-trig-decls (x)
  (declare (optimize (speed 3) (safety 1)))
  (sin x))

(test-trig-decls 1)
(test-trig-decls 1.0)
(disassemble 'test-trig-decls)

|#


;;; crap!  CMUCL type-expands '(unsigned-byte 32) => (INTEGER 0 4294967295)
;;; whereas Allegro type-expands '(unsigned-byte 32) => (unsigned-byte 32)
;;; Therefore we do not have a canonical type representation.
;;;(defun type-expand (form)
;;;  #+cmu (lisp::type-expand form)
;;;  #+allegro (excl::subtype-expand form)  ; this doesn't exist anymore
;;;  )

(defun type-equiv (type1 type2)
  (and (subtypep type1 type2)
       (subtypep type2 type1)))

#|
(deftype foo () '(unsigned-byte 32))
(type-equiv 'foo '(unsigned-byte 32))
(type-equiv 'foo '(INTEGER 0 4294967295))

|#

;;; *******************************  NON-DESTRUCTIVE  SORT  *******************************

;;; Common Lisp totally blew it with the semantics of SORT
;;; SORT is DESTRUCTIVE of the list or sequence it is passed.
;;; Here is a replacement that is safe.

(defun safe-sort (sequence predicate &key key)
  (sort (copy-seq sequence) predicate :key key))

;;;  ******************************  MULTIPLE-VALUE-xxx ABBREVIATIONS  ******************************

(defmacro mv-bind (vars values-form &body body)
  `(multiple-value-bind ,vars ,values-form . ,body))

(defmacro mv-setq (vars values-form)
  `(multiple-value-setq ,vars ,values-form))

(defmacro mv-list (values-form)
  `(multiple-value-list ,values-form))


(defmacro multi-mv-bind (bindings &body body)
  (loop while (eq (caar body) 'declare)
	collect (car body) into declarations
	do (setq body (cdr body))
	finally
     (return (loop for (vars) in bindings
		   append vars into all-vars
		   finally (return `(let ,all-vars ,@declarations
					 ,@(loop for (vars form) in bindings
						 collect `(multiple-value-setq ,vars ,form))

					 . ,body))))))

;;; ******************************   STRING OPERATIONS  ******************************

#+(or allegro cmu)
(progn

;;; Optimizations to reduce consing in string-append
	
(defun string-append2 (s1 s2)
  (declare (type simple-string s1 s2))
  (let* ((n1 (length s1))
	 (n2 (length s2))
	 (result (make-string (+ n1 n2))))
    (declare (type simple-string result))
    (loop for i from 0 below n1
	  do (setf (aref result i) (aref s1 i)))
    (loop for i from 0 below n2
	  for j from n1
	  do (setf (aref result j) (aref s2 i)))
    result))


;;;(defun string-append (&rest strings)
;;;  (apply 'concatenate 'string strings))

(defun string-append (&rest strings)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((n (loop with n of-type fixnum = 0
		  for string in strings
		  do (let ((string string))
		       (declare (type SIMPLE-BASE-STRING string))
		       (incf n (length string)))
		  finally (return n)))
	 (result (make-array n :element-type 'base-char)))
    (declare (type SIMPLE-BASE-STRING result))
    (loop with p of-type fixnum = 0
	  for string in strings
	  do (let* ((string string)
		    (n2 (length string)))
	       (declare (type SIMPLE-BASE-STRING string)
			(fixnum n2))
	       (loop for i of-type fixnum from 0 below n2
		   do (setf (aref result p) (aref string i))
		      (incf p))))
 result))

) ; end #+(or allegro cmu)


#|
(type-of (string-append "{" "basasa  xdsds" "}"))
(type-of "foo")

(type-of (make-array 10 :element-type 'base-char))
|#


;;;#+sbcl (defun string-append (&rest strings) (apply 'concatenate 'string strings))

#+sbcl
(progn
	
(defun string-append2 (s1 s2)
  (declare (type (simple-array character) s1 s2))
  (let* ((n1 (length s1))
	 (n2 (length s2))
	 (result (make-string (+ n1 n2))))
    (declare (type (simple-array character) result))
    (loop for i from 0 below n1
	  do (setf (aref result i) (aref s1 i)))
    (loop for i from 0 below n2
	  for j from n1
	  do (setf (aref result j) (aref s2 i)))
    result))

(defun string-append (&rest strings)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((n (loop with n of-type fixnum = 0
		  for string in strings
		  do (let ((string string))
		       (declare (type (simple-array character) string))
		       (incf n (length string)))
		  finally (return n)))
	 (result (make-array n :element-type 'character)))
    (declare (type (simple-array character) result))
    (loop with p of-type fixnum = 0
	  for string in strings
	  do (let* ((string string)
		    (n2 (length string)))
	       (declare (type (simple-array character) string)
			(fixnum n2))
	       (loop for i of-type fixnum from 0 below n2
		   do (setf (aref result p) (aref string i))
		      (incf p))))
 result))

) ; end progn

(defun string-search-set (char-set string &key from-end (start 0) (end nil))
  (unless end (setq end (length string)))
  (if from-end
      (loop for pos from (1- end) downto start
	    when (member (aref string pos) char-set :test 'char-equal) return pos)
      (loop for pos from start below end
	    when (member (aref string pos) char-set :test 'char-equal) return pos)))


(defun string-search-not-set (char-set string &key from-end (start 0) (end nil))
  (unless end (setq end (length string)))
  (if from-end
      (loop for pos from (1- end) downto start
	    unless (member (aref string pos) char-set :test 'char-equal) return pos)
      (loop for pos from start below end
	    unless (member (aref string pos) char-set :test 'char-equal) return pos)))

(defun substring (string from  &optional to area)
  (ignore area)
  (subseq string from to))

#|
(disassemble 'string-append)
(string-append "foo" "bar" "baz")
|#

(defun to-string (thing)
  (typecase thing
    (symbol (symbol-name thing))
    (pathname (namestring thing))
    (string thing)
    (t (format nil "~a" thing))))


;;; ***********************************  ALISTS  ***********************************

(defun remass (item alist &key (test #'eql))
  (let ((found (assoc item alist :test test)))
    (if found
      (remove found alist)
      alist)))

(defun delass (item alist &key (test #'eql))
  (let ((found (assoc item alist :test test)))
    (if found
	(delete found alist)
	alist)))

(defun putassoc (alist name value &key (test #'eql))
  (let ((found (assoc name alist :test test)))
    (cond (found
	   (rplacd found value)
	   alist)
	  (alist (nconc alist `((,name . ,value)))  ;; why are new entries put at end of alist??
		 alist)
	  (t `((,name . ,value))))))


(defun sort-keyword-value-list (keyword-value-list)
  (let* ((l2 (loop for (key value) on keyword-value-list by #'cddr
		   collect (list key value)))
	 (l3 (stable-sort l2 #'string-lessp :key #'car)))
    (loop for (key value) in l3
	  collect key
	  collect value)))


(defun list-hash-table (ht)
  (let ((l nil))
    (maphash #'(lambda (key val) (push (list key val) l)) ht)
    l))

;;; *******************  PROPERTY-LISTS FOR RANDOM OBJECTS *******************

(defgeneric get-prop (object slot-name))
(defgeneric put-prop (object value slot-name))

(defparameter *object-properties-ht* (make-hash-table :test 'equal))

(defmethod get-prop (object prop)
  (gethash (list object prop) *object-properties-ht*))

(defmethod (setf get-prop) (val object prop)
  (setf (gethash (list object prop) *object-properties-ht*) val))

(defmethod rem-prop (object prop)
  (remhash (list object prop) *object-properties-ht*))

(defmacro with-object-properties (prop-specs object &body body)
  `(symbol-macrolet
    ,(loop for prop-spec in prop-specs
	   for var = (if (consp prop-spec)
			 (car prop-spec)
			 prop-spec)
	   for slot = (if (consp prop-spec)
			  (or (cadr prop-spec) var)
			  (make-keyword (symbol-name var)))
	   collect `(,var (get-prop ,object ',slot)))
    (progn .,body)))

;;; *******************   PROPERTY-LIST-MIXIN  *******************

(defclass property-list-mixin
	  ()
  ((property-list
     :initarg :property-list
     :initform nil
     :accessor property-list))
  )

(defmethod get-prop ((self property-list-mixin) indicator)
  ;(check-type indicator symbol)
  (when (slot-boundp self 'property-list)
    (getf (property-list self) indicator)))

(defmethod (setf get-prop) (value (self property-list-mixin) indicator)
  ;(check-type indicator symbol)
  (setf (getf (property-list self) indicator) value)
  value)

(defmacro define-soft-slot (class slot-keyword &optional accessor-name)
  (unless accessor-name 
    (setq accessor-name (intern (symbol-name slot-keyword))))
  `(progn (defmethod ,accessor-name ((object ,class))
	   (get-prop object ,slot-keyword))
	 (defmethod (setf ,accessor-name) (value (object ,class))
	   (setf (get-prop object ,slot-keyword) value))))

(defmacro define-property-list-accessor (class slot-keyword &optional accessor-name)
  (unless accessor-name 
    (setq accessor-name (intern (symbol-name slot-keyword))))
  `(progn (defmethod ,accessor-name ((object ,class))
	   (get-prop object ,slot-keyword))
	 (defmethod (setf ,accessor-name) (value (object ,class))
	   (setf (get-prop object ,slot-keyword) value))))

(defmacro define-accessor-synonym (synonym class accessor-name)
  `(progn (defmethod ,synonym ((object ,class))
	   (,accessor-name object))
	 (defmethod (setf ,synonym) (value (object ,class))
	   (setf (,accessor-name object) value))))


(defmethod rem-prop ((self property-list-mixin) indicator)
  ;(check-type indicator symbol)
  (remf (property-list self) indicator))

;;; ************************  INTERNAL-PROPERTY-LIST-MIXIN  ************************

(defclass internal-property-list-mixin
	  ()
  ((internal-property-list :initform nil
			   :accessor internal-property-list-mixin-property-list))
  )
  
(defmethod internal-get-prop ((self internal-property-list-mixin) indicator)
  (getf (internal-property-list-mixin-property-list self) indicator))

(defmethod (setf internal-get-prop) (value (self internal-property-list-mixin)
					   indicator)
  (setf (getf (internal-property-list-mixin-property-list self) indicator) value))

(defmethod internal-rem-prop ((self internal-property-list-mixin) indicator)
  (remf (internal-property-list-mixin-property-list self) indicator))


;;; ************************  FASD-FORM-MIXIN  ************************

(defclass fasd-form-mixin
	()
	()
)
(defmethod fasd-form-p ((self t))
  nil)

(defmethod fasd-form-p ((self fasd-form-mixin))
  t)

(defgeneric fasd-form-init-plist (object))

(defmacro define-fasd-form-init-plist (class init-plist)
  (let ((self-symbol (intern "SELF" *package*)))
    `(defmethod fasd-form-init-plist :around ((,self-symbol ,class))
      (append ,init-plist
       (call-next-method)))))


(defgeneric fasd-form (object))
  

(defvar *fast-dumping-p* nil)
(defvar *fast-dumping-p2* t)

;;;  For CLOS compatibility, need to replace FASD-FORM with MAKE-LOAD-FORM 
(defmethod fasd-form ((self fasd-form-mixin))
  (let ((*fast-dumping-p* *fast-dumping-p2* ))
    `(make-instance ',(type-of self) 
		    . ,(fasd-form-init-plist self))))

;;; *fast-dumping-p2* is an ugly hack so that only copy prevents binding *fast-dumping-p* to T.

;;; this name should be avoided, but it is used in a few places
(defmethod copy ((self fasd-form-mixin))
  (let ((*fast-dumping-p2* nil))
    (eval (fasd-form self))))

;;; base method
(defmethod fasd-form-init-plist ((self fasd-form-mixin))
  ;;(ignore self)
  nil)

(defclass fasd-form-property-list-mixin
	(fasd-form-mixin property-list-mixin)
	())

(defun fasd-form-mixin-expand-property-list (property-list &optional props-not-to-dump)
  (when property-list
    (let ((filtered-property-list
	   (loop for (prop value) on  property-list by #'cddr
		 unless (memq prop props-not-to-dump)
		   collect (if (keywordp prop)
			       prop
			       `',prop)
		   and collect (if (fasd-form-p value)
				   (fasd-form value)
				   `',value))))
      (when filtered-property-list
	`(:property-list (list .,filtered-property-list))))))

(defmethod fasd-form-properties-list-do-not-dump ((self fasd-form-property-list-mixin))
  nil)

(define-fasd-form-init-plist fasd-form-property-list-mixin
    (fasd-form-mixin-expand-property-list
     (property-list self) (fasd-form-properties-list-do-not-dump self)))

#|
(setq pls2 (make-instance 'fasd-form-property-list-mixin :property-list '(:foo :bar)))
(fasd-form pls2)
|#

;;; ********************************  NUMERIC FUNCTIONS  ********************************

;;; ALLEGRO sucks: ignores inline declarations.  What to lose.
;;; ---apparently not anymore.  Windows version of acl62 barfs when dfloat is a macro.

(declaim (inline truncate! round! dfloat ))

;;; Inline versions that return only a single value for maximum speed.
;;; If the type of X is known to be a SINGLE-FLOAT or DOUBLE-FLOAT, these
;;; will open code.
;;; TRUNCATE does not inline in CMUCL.
#-allegro
(defun truncate! (x) (values (the fixnum (truncate x))))

#+allegro
(defmacro truncate! (x) `(values (the fixnum (truncate ,x))))

#+(and cmu x86)
(defun round! (x) (values (ext:truly-the fixnum (kernel:%unary-round  x))))

#+(and sbcl (or x86 x86-64))
(defun round! (x) (values (sb-ext:truly-the fixnum (sb-kernel:%unary-round  x))))

#-(or (and cmu x86) (and sbcl (or x86 x86-64)))
(defmacro round! (x) `(values (the fixnum (truncate (+ .5, x)))))

;; why isn't this getting inlined in CMUCL?
#-(and allegro (not mswindows))
(defun dfloat (x) (float x 1d0))

#+(and allegro (not mswindows))
(defmacro dfloat (x) `(float ,x 1d0))

(declaim (type double-float *rads-per-deg* *degs-per-rad*))
(defconstant *rads-per-deg* (/ pi 180.0d0))
(defconstant *degs-per-rad* (/ 180.0d0 pi))

;;; For egg-sucking Allegro, these guys need to be macrofied.
#+(or cmu sbcl)
(progn

(declaim (inline sind cosd tand degrees radians 2^ ^2 ))

(defun sind (deg) (sin (* deg *rads-per-deg*)))
(defun cosd (deg) (cos (* deg *rads-per-deg*)))
(defun tand (deg) (tan (* deg *rads-per-deg*)))
(defun degrees (radians) (* radians *degs-per-rad*))
(defun radians (degrees) (/ degrees *degs-per-rad*))
(defun 2^ (n) (ash 1 n))

(proclaim '(ftype (function (double-float) double-float ) ^2))
(proclaim '(ftype (function (fixnum) fixnum ) ^2))
(proclaim '(ftype (function (t) t ) ^2))
(defun ^2 (x) (* x x))

) ; end progn

#+allegro
(progn

(defmacro sind (deg) `(sin (* ,deg *rads-per-deg*)))
(defmacro cosd (deg) `(cos (* ,deg *rads-per-deg*)))
(defmacro tand (deg) `(tan (* ,deg *rads-per-deg*)))
(defmacro degrees (radians) `(* ,radians *degs-per-rad*))
(defmacro radians (degrees) `(/ ,degrees *degs-per-rad*))
(defmacro 2^ (n) `(ash 1 ,n))
(defmacro ^2 (x) `(let ((x ,x)) (* x x)))  ; apparently Allegro is smart enough to 
#+never
(defmacro ^2 (x) (if (consp x)
		     `(let ((x ,x)) (* x x))
		     ;; If the form is a symbol, no need to bind it.
		     `(* ,x ,x)))

) ; end progn

#-(or cmu sbcl)
(defmacro truly-the (type form)
  `(the ,type ,form))

(defmacro dsqrt (x)
  `(truly-the double-float (sqrt (the (double-float 0.0) ,x))))

;;; END OF INLINES



;;; These declarations appear to be broken in LCL-4.1
;;;(proclaim '(ftype (function (float) float) ^2))
;;;(proclaim '(ftype (function (fixnum) fixnum ) ^2))
;;;(proclaim '(ftype (function (single-float) single-float ) ^2))
;;;(proclaim '(ftype (function (double-float) double-float ) ^2))

(defun ^2p (x)
  (declare (fixnum x))
  ;;(declare (type-reduce number fixnum))
  (zerop (the fixnum (logand x (the fixnum (1- x))))))

(defun log2 (n)
  (setq n (abs n))
  (cond ((integerp n) (integer-length (1- n))) 
	((> n 1.0) (log2 (ceiling n)))
	(t (- (log2 (floor (/ 1.0 n)))))))

(defun pad-to-multiple (value multiple)
  (* multiple (ceiling value multiple)))

(defun truncate-to-multiple (value multiple)
  (* multiple (truncate value multiple)))

(defun random-in-range (lo hi) (+ lo (random (- hi lo))))

;;; Polar method for normal deviates
;;; Taken from Knuth Volume II.
(defun normal-random (&optional (sigma 1.0d0) (mean 0.0d0))
  (declare (values double-float double-float))
  (declare (type double-float sigma mean))
  (loop for v1 = (random-in-range -1.0d0 1.0d0)
	for v2 = (random-in-range -1.0d0 1.0d0)
	for s = (+ (^2 v1) (^2 v2))
	until (< s 1d0)
	finally
	(let ((r (* sigma (sqrt (/ (* -2 (log s)) s)))))
	  (return (values (+ (* v1 r) mean) (+ (* v2 r) mean))))))




;;;
;;; Possible cruft.  Remove if it gets too odious.  Used by image
;;; windowing functions...

(defun fix-if-integer (x)
  (let ((fix-x (floor x)))
    (if (= x fix-x)
	fix-x
	x)))


;;; ********************************  ARRAYS  ********************************


;;; Sun Mar 10 2002.  I thought Allegro 6.0 fixed this problem.  I tested examples evaled in the
;;; interpreter work fine. Apparently, compiled code has optimizations that trigger the bug where
;;; MAKE-ARRAY of numeric arrays do not get initialized unless :INITIAL-ELEMENT or :INITIAL-CONTENTS
;;; are specified.



(defun make-array-initial-element-for-type (element-type)
  (if (consp element-type)
      (case (car element-type)
	(signed-byte 0)
	(unsigned-byte 0))
      (case element-type
	(fixnum 0)
	(bit 0)
	(double-float 0.0d0)
	(single-float 0.0f0)
	(character #\0))))

(defvar lcl::*default-make-array-allocation* nil)

;;; Really should put advice around make-array to force the :allocation.
;;; Allegro does not zero array alements by default, so you get total randomness
;;; unless :INITIAL-ELEMENT is specified.
;;; make-array0 makes sure the array elements are zeroed when :INITIAL-ELEMENT
;;; (or :INITIAL-CONTENTS) is not specified.

;;; Does not load 

#+allegro
(defun make-array0 (dims &rest args
		    &key
		    element-type
		    initial-element
		    initial-contents
		    (allocation (or lcl::*default-make-array-allocation*
                                    :lispstatic-reclaimable)) ;??
		    &allow-other-keys)
  
  (cond ((or initial-element initial-contents)
	 (apply #'make-array dims args))
	((or (null element-type) (eq element-type t))
	 (apply #'make-array dims :initial-element args))
	(t (let ((init (make-array-initial-element-for-type element-type)))
	     (if init
		 (apply #'sys::make-array dims :initial-element init :allocation allocation args)
		 (apply #'sys::make-array dims :allocation allocation args))
	     ))))

#-allegro
(defun make-array0 (dims &rest args
		    &key
		    element-type
		    initial-element
		    initial-contents
		    &allow-other-keys)
  
  (cond ((or initial-element initial-contents)
	 (apply #'make-array dims args))
	((or (null element-type) (eq element-type t))
	 (apply #'make-array dims :initial-element nil args))
	(t (let ((init (make-array-initial-element-for-type element-type)))
	     (if init
		 (apply #'make-array dims :initial-element init args)
		 (apply #'make-array dims args))
	     ))))

(defun make-int-vector (n)
  (make-array0 n :element-type '(signed-byte 32)))

(defun dvector (&rest elts)
  (make-array (length elts) :element-type 'double-float
	      :initial-contents elts))

;;; Break the type-inference on object.
(defun break-type-inference (object)
  object)

;;; CAST is needed to shut up cmucl compiler complaints, when the type-inference machinery will not
;;; deliver the correct type.  Currently this is only used with UNDERLYING-SIMPLE-VECTOR in the file
;;; $FREEDIUS/lisp/math/matrices.lisp.
(defmacro cast (x type)
  (let ((y (gensym)))
    `(let ((,y (break-type-inference ,x)))
      (declare (type ,type ,y))
      ,y)))

(defmacro with-element-types (element-type-var type-list expander)
  #+(or cmu sbcl) (setq expander (eval expander))
  `(let ((%element-type% ,element-type-var))
    (cond ,@ (loop for type in type-list
		   collect `((equal %element-type% ',type)
			     ,(funcall expander type)))
	  ;(t ,(funcall expander t))
	  (t (error "WITH-ELEMENT-TYPES: unhandled-type ~a" %element-type%))
	  )))

(defun copy-array-contents (from to)
  (declare (optimize (speed 3) (safety 0)))
  (let ((element-type (array-element-type from)))
    (unless (and (arrayp from) (arrayp to)) (error "Args ~a ~a must be arrays." from to))
    (unless (equal element-type (array-element-type to))
      (error "Element types of ~a and ~a must be the same." from to))

    (mv-bind (from-1d from-off from-length) (lcl:underlying-simple-vector from)
      (mv-bind (to-1d to-off to-length) (lcl:underlying-simple-vector to)
	(unless (and (equal (array-element-type from-1d) element-type)
		     (equal (array-element-type to-1d) element-type))
	  (error "COPY-ARRAY-CONTENTS displaced arrays with incompatible element-types ~a ~a ~a ~a"
		 from to from-1d to-1d))
	  
	(let* ((n (min from-length to-length))
	       (from-end (+ from-off n)))
	  (declare (fixnum n from-end))
	  (with-element-types element-type
	    (double-float single-float (signed-byte 32) (unsigned-byte 32)
			  (unsigned-byte 16) (signed-byte 16)
							 (unsigned-byte 8) base-char t)
	    (lambda (type) `(let ((from-1d (cast from-1d (simple-array ,type (*))))
				  (to-1d (cast to-1d (simple-array ,type (*)))))
			       (declare (type (simple-array ,type (*)) from-1d to-1d))
			       (loop for from-i fixnum from from-off below from-end 
				     for to-i fixnum from to-off
				     do (setf (aref to-1d to-i) (aref from-1d from-i)))))))))
    to))

(defun fast-fill-array (arr &optional val)
  (declare (optimize (speed 3) (safety 0) #+cmu (ext:inhibit-warnings 1)))
  (mv-bind (arr-1d offset length)
      (lcl:underlying-simple-vector arr)
    (let* ((element-type (array-element-type arr))
	   (val (or val (case element-type
			  (double-float 0.0d0)
			  (single-float 0.0f0)
			  (base-char #\NULL)
			  (otherwise 0))))
	   (end (+ offset length)))
      (declare (fixnum offset length end ))
      (with-element-types element-type
	(double-float single-float (signed-byte 32) (unsigned-byte 32) 
		      (signed-byte 16) (unsigned-byte 16) fixnum
		      (unsigned-byte 8) base-char t)
	(lambda (type)
	  `(let ((arr-1d (cast arr-1d (simple-array ,type (*))))
		 (val (break-type-inference val)))
	    (declare (type ,type val))
	    (declare (type (simple-array ,type (*)) arr-1d))
	    (loop for i fixnum from offset below end
		  do (setf (aref arr-1d i) val))
	    )))
      arr)))

(defun listarray (arr)
  (case (array-rank arr)
    (1 (loop for i from 0 below (length arr)
	     collect (aref arr i)))
    (2 (loop for row from 0 below (array-dimension arr 0)
	     collect (loop for col from 0 below (array-dimension arr 1)
			   collect (aref arr row col))))))

(defun adjust-array-copy (array1 new-dims)
  (let ((array2 (make-array new-dims :element-type (array-element-type array1))))
    (multiple-value-bind (array1d1 off1 n1) 
	(underlying-simple-vector array1)
      (multiple-value-bind (array1d2 off2 n2)
	  (underlying-simple-vector array2)
	(declare (ignore off2))
	(loop for i fixnum from 0 below (min n1 n2)
		do (setf (aref array1d2 i) (aref array1d1 (+ i off1))))
	array2))))

    


#|
(defun test-with-element-types (arr)
  (declare (optimize (speed 3) (safety 1) (ext:inhibit-warnings 1)))
  (let ((element-type (array-element-type arr)))
    (mv-bind (arr-1d) (lcl:underlying-simple-vector arr)
      (with-element-types element-type
	(double-float)
	(lambda (type) `(let ((arr-1d (cast arr-1d (simple-array ,type (*)))))
			 (declare (type (simple-array ,type (*)) arr-1d))
			 (zerop (aref arr-1d 0))))))))

(defun test-with-element-types (arr)
  (declare (optimize (speed 3) (safety 1) (ext:inhibit-warnings 1)))
  (let ((element-type (array-element-type arr)))
    (mv-bind (arr-1d) (lcl:underlying-simple-vector arr)
      (with-element-types element-type
	(double-float)
	(lambda (type) `(let ((arr-1d arr-1d))
			 (declare (type (simple-array ,type (*)) arr-1d))
			 (zerop (aref arr-1d 0))))))))

(disassemble 'test-with-element-types)
(disassemble 'copy-array-contents)

|#

#|
(defun test-with-element-types (arr i)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum i))
  (let ((element-type (array-element-type arr)))
    (with-element-types element-type (fixnum (unsigned-byte 8))
			(lambda (type)
			  `(let ((arr arr))
			    (declare (type (simple-array ,type (*)) arr))
			    (aref arr i))))))

(defun test-8bit-aset (arr n)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (simple-array (unsigned-byte 8) (*)) arr))
  (loop for i fixnum from 0 below n 
	do (setf (aref arr i) 0)))

(defun test-16bit-aset (arr n)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (simple-array (unsigned-byte 16) (*)) arr))
  (loop for i fixnum from 0 below n 
	do (setf (aref arr i) 0)))

(disassemble 'test-with-element-types)
(disassemble 'copy-array-contents)
(disassemble 'fast-fill-array)
(disassemble 'test-8bit-aset)
(disassemble 'test-16bit-aset)
|#

(defun copy-array (array)
  (let ((new-array (make-array (array-dimensions array) :element-type (array-element-type array))))
    (copy-array-contents array new-array)
new-array))


;;; ***********************************  MISC  ***********************************


(defvar *keyword-package* (find-package "KEYWORD"))

(defun make-keyword (symbol)
  (intern (string symbol) *keyword-package*))

;;; Use the default CL prompts to avoid problems with Ilisp.
(defun print-lisp-prompt ()
  ;;#+allegro (TPL::OUTPUT-TOP-LEVEL-PROMPT t) 
  #+allegro (format *terminal-io* "~&~a(0): " (package-name *package*))
  #+cmu (format *terminal-io* "~&~a" lisp::*prompt*)
  #+sbcl (format *terminal-io* "~&~a>: "
		 (or (first (package-nicknames *package*))
		     (package-name *package*)))
  )

(defun lisp-prompt-string ()
  #+allegro (with-output-to-string (st) (TPL::OUTPUT-TOP-LEVEL-PROMPT st)) 
  #+cmu lisp::*prompt*
  #+sbcl (first (package-nicknames *package*))
  )

(defun my-identity (x) x)

(defmacro with-slot-values (slots instance &body body)
  `(with-slots ,slots ,instance
     (let ,(loop for slot in slots collect
		 (if (consp slot)
		     `(,(cadr slot) ,(cadr slot))
		     `(,slot ,slot)))
       . ,body)))

;;; Bug fix for Lucid excessive compiler optimization w.r.t. load-time-value.
;;; See the file ~quam/lucid/load-time-value-bug.lisp.
(defmacro load-time-value* (form)
  `(my-identity (load-time-value ,form)))

(declaim (inline neq))
(defun neq (x y)
  (not (eq x y)))

(defmacro removef (item list)	   
  `(setf ,list (remove ,item ,list)))

(defun random-in-range (lo hi) (+ lo (random (- hi lo))))


(defmacro condition-case (vars form &rest clauses)
  (ignore vars)
  `(handler-case ,form . ,
		 (loop for (error-name . action) in clauses
		       collect `(,error-name () . ,action)) ))

;; This will be redefined if config::weak-eval-cache is T:
(defun follow-weak-pointer (object) object)

(defun username ()
  (or (getenv "USER") (getenv "USERNAME") "nobody"))

;;;
;;; Can this work under Windows?
;;;
(defun user-email-address ()
  (format nil "~a@~a" (username) (machine-instance)))

(defun getpid ()
  #+cmu (unix::unix-getpid)
  #+sbcl (sb-unix::unix-getpid)
  #+allegro (excl::getpid))

#-allegro
(defun gethostname ()
  #+asdf (asdf:hostname)
  #+(and cmu (not asdf)) (unix::unix-gethostname)
  #+(and sbcl (not asdf)) (sb-unix:unix-gethostname)
  )

#+allegro
(defun gethostname ()
  (let* ((iostream (run-program "uname" '("-n") :output :stream :wait nil)))
    (read-line iostream)))
    
 
;;;
;;; Given a "distribution" that provides samples drawn from the set of
;;; indices into SEQUENCE, return a random element of that sequence:
;;;

(defun random-element (sequence &optional (distribution #'(lambda () (random (length sequence)))))
  (elt sequence (funcall distribution)))
