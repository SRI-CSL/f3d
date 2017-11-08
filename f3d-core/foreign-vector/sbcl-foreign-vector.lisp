(in-package :qffi)

#|
On x86 (32 bit), SBCL arrays are the same as CMUCL arrays.  No change is required for the creation
of foreign-vectors.

On AMD64 (aka x86-64) SBCL array headers are different.

   The array element-type codes have changed.


|#



#|
(setq fv1 (make-foreign-vector 1024 :element-type '(unsigned-byte 8)))
(unmake-foreign-vector fv1)
(type-of *foo*)
(sb-kernel::vector-sap *foo*)
(sb-kernel:get-LISP-OBJ-ADDRESS (make-array 3 :element-type '(unsigned-byte 8)))68811630559
(logand 15 (sb-kernel:get-LISP-OBJ-ADDRESS (make-array 3 :element-type '(unsigned-byte 8)))) 15
(dump-array-header2 *foo*)
(let* ((a (make-array 1024 :element-type '(unsigned-byte 8)))
       (addr (sb-kernel:get-LISP-OBJ-ADDRESS a)))
  (values addr (sb-kernel:make-lisp-obj addr)))
|#

(def-foreign-function (make_lisp_vector (:name (freedius-prefix "make_lisp_simple_vector")) (:return-type :lisp))
    (length :int) (typecode :int) (initial-element :lisp))

(def-foreign-function (make_integer_vector (:name (freedius-prefix "make_integer_simple_vector")) 
					   (:return-type :lisp))
    (length :int) (typecode :int) (elemsize :int) (initial-element :int))


#|
(def-foreign-function (make_integer_vector (:name (freedius-prefix "make_integer_simple_vector")) (:return-type :lisp))
    (length :int) (typecode :int) (initial-element :int))
|#


(def-foreign-function (make_single_float_vector (:name (freedius-prefix "make_single_float_simple_vector")) (:return-type :lisp))
    (length :int) (typecode :int) (initial-element :double-float))


(def-foreign-function (make_double_float_vector (:name (freedius-prefix "make_double_float_simple_vector")) (:return-type :lisp))
    (length :int) (typecode :int) (initial-element :double-float))

(def-foreign-function (simple_vector_typecode (:name (freedius-prefix "simple_vector_typecode")) (:return-type :int))
    (simple-vector :lisp))

(def-foreign-function (unmake_simple_vector (:name (freedius-prefix "unmake_simple_vector")) (:return-type :null))
    (foreign_simple_vector :lisp))


;;; Lisp objects are problematic in foreign vectors because the lisp garbage
;;; collector ignores the contents of the vector, since it is not in the Lisp
;;; heap.  The only safe lisp values are those known to be in stationary areas.

(eval-when (load eval compile)
  
(defun array-element-type-code-from-type-spec (type-spec)
  (sb-impl::%vector-widetag-and-n-bits type-spec)
  #+never(cadr (assoc type-spec *sv-type-code-alist* :test 'equal))
  )

) ; end eval-when

;; (format nil "#X~x" (array-element-type-code-from-type-spec '(unsigned-byte 8))) #X96
;; (format nil "#X~x" (array-element-type-code-from-type-spec '(signed-byte 8))) #XC2

;;; unused
(defmacro array-element-type-case (type-code &rest clauses)
  `(case ,type-code
    . ,(loop for (codes . forms) in clauses
	     when (eq codes 'otherwise)
	     collect (cons codes forms)
	     else
	     collect (progn (unless (consp codes) (setq codes (list codes)))
			    (cons (loop for code in codes
					collect (or (array-element-type-code-from-type-spec code)
						    (error "unknown array element-type ~a" code)))
				  forms)))))

(defparameter *prohibit-type-t-foreign-vectors* t)

(defparameter *make-foreign-vector-bytes-allocated* 0)
(defparameter *make-foreign-vector-bytes-freed* 0)

(defvar *foo*)

(defparameter *make-foreign-vector-bytes-min-addr* (ash 1 32)) ; FIXME 32 bit pointer dependent
(defparameter *make-foreign-vector-bytes-max-addr* 0)

(defun update-foreign-vector-address-range (foreign-vector)
  (let ((addr (%pointer foreign-vector)))
    (setq *make-foreign-vector-bytes-min-addr* (min addr *make-foreign-vector-bytes-min-addr*)
	  *make-foreign-vector-bytes-max-addr* (max addr *make-foreign-vector-bytes-max-addr*))))

(defun foreign-vector-p (vector)
  (and (typep vector 'array)
       (let ((addr (%pointer vector)))
	 (and (<= addr *make-foreign-vector-bytes-max-addr*)
	      (>= addr *make-foreign-vector-bytes-min-addr*)))))


#|
(list *make-foreign-vector-bytes-min-addr* *make-foreign-vector-bytes-max-addr*)
(foreign-vector-p (make-array 2))
(foreign-vector-p nil)
(setq fv (make-foreign-vector 10 :element-type '(unsigned-byte 8)))
(foreign-vector-p fv)
(unmake-foreign-vector fv)
(lisp::%vector-type-code 'single-float)
(simple_vector_typecode *foo*)
(aref *foo* 0)
|#

    
;;;

(defun make-foreign-vector (length &key (element-type t) initial-element)
  (setq element-type (upgraded-array-element-type element-type))
  (multiple-value-bind (typecode elemsize) (array-element-type-code-from-type-spec element-type)
    (let* ((foreign-vector
	    (case element-type
	      ((t)
	       (when *prohibit-type-t-foreign-vectors*
		 (error "Type T foreign vectors are currently disallowed."))
	       (make_lisp_vector length typecode initial-element)
	       )
	      (single-float
	       (make_single_float_vector length typecode (float (or initial-element 0.0d0) 0.0d0)))
	      (double-float
	       (make_double_float_vector length typecode (float (or initial-element 0.0d0) 0.0d0)))
	      (t (make_integer_vector length typecode elemsize
				      (if (eq element-type 'base-char)
					  (if initial-element (char-code initial-element) 0)
					  (round (or initial-element 0))))))))
      (unless (vectorp foreign-vector)
	(setq *foo* foreign-vector)
	(error "make-foreign-vector: malloc failed to return ~a elements of type ~a~%"
	       length element-type))
      #+never
      (incf *make-foreign-vector-bytes-allocated*
	    (+ 12 (* (ash (array-element-size* foreign-vector) -3)
		     length)))

      (update-foreign-vector-address-range foreign-vector)
      foreign-vector)))



#+never
(defun make-foreign-vector (length &key (element-type t) initial-element)
  (sb-impl::make-heap-vector length
			       :element-type element-type
			       :initial-element (or initial-element 0)))


(defun make-stationary-vector (length &rest args
			       &key (element-type t) initial-element)
  (declare (ignore element-type initial-element))
  (apply 'make-foreign-vector length args))

(defun unmake-stationary-vector (vector)
  (unmake-foreign-vector vector))
  
(defun make-stationary-array (dims &rest args
			       &key (element-type t) initial-element)
  (declare (ignore element-type initial-element))
  (apply 'make-foreign-array dims args))

(defun unmake-stationary-array (array)
  (unmake-foreign-vector array))
  
;;; Error encountered in Mac OS X CMUCL - the args list passed to
;;; MAKE-FOREIGN-ARRAY by MAKE-STATIONARY-ARRAY includes the
;;; :INITIAL-ELEMENT keyword.  Modified MAKE-FOREIGN-ARRAY to ignore
;;; that arg.
;;;
;;; WARNING:  MAKE-FOREIGN-ARRAY returns a DISPLACED-ARRAY which is not a SIMPLE-ARRAY.
(defun make-foreign-array (dims &key element-type initial-element)
  (declare (ignore initial-element))
  (if (numberp dims)
      (make-foreign-vector dims :element-type element-type)
      (let ((vector (make-foreign-vector (apply #'* dims) :element-type element-type)))
	(make-array dims :element-type element-type :displaced-to vector))))

(defun unmake-foreign-vector (foreign-simple-vector)
  ;;; sys::object-space is a LUCIDism.  There doesn't appear to be a counterpart for CMUCL
  #+never
  (unless (eq (sys::object-space foreign-simple-vector) :NONEXISTENT)
    (error "unmake-foreign-vector: vector is not in foreign space."))

  ;; special hack for releasing multi-dimension foreign-arrays
  (when (and (> (array-rank foreign-simple-vector) 1) (array-displacement foreign-simple-vector))
    (setq foreign-simple-vector (array-displacement foreign-simple-vector)))

  (unless (foreign-vector-p foreign-simple-vector)
    (error "unmake-foreign-vector: vector is not in foreign space."))
  #+never
  (incf *make-foreign-vector-bytes-freed*
	(+ 12 (* (ash (array-element-size* foreign-simple-vector) -3)
		 (length foreign-simple-vector))))
  ;;(break)
  (unmake_simple_vector foreign-simple-vector)  )



#|

(setq *v8* (make-foreign-vector 100 :element-type '(unsigned-byte 8)))
(setq *vdf* (make-foreign-vector 100 :element-type 'double-float))


136521783 is not a valid argument to SB-KERNEL:MAKE-LISP-OBJ
(format nil "#X~x" 136521783)  #X8232837
(sb-di::valid-lisp-pointer-p (sb-impl::int-sap 136521783)) = 0

(describe *v8*)
(describe *vdf*)
(sb-ext::gc :full t)

(progn (setq vl (loop repeat 1000 collect (make-foreign-vector 8192 :element-type '(unsigned-byte 8))))
       (length vl))
	      
(loop for vect in vl do (unmake-foreign-vector vect))

(setq sv1 (make-array 10 :element-type '(unsigned-byte 8)))
(setq sv32 (make-array 10 :element-type '(unsigned-byte 32)))
(setq sv8disp (make-array (* 10 4) :element-type '(unsigned-byte 8) :displaced-to sv32))
(setf (aref sv32 0) #x01234567)
(loop for i from 0 below 16 collect (aref sv8disp i))

(setq sv1disp (make-array (* 10 4) :element-type '(unsigned-byte 1) :displaced-to sv32))
(loop for i from 0 below 32 collect (aref sv1disp i))

(setq sv8g (make-array 10 :element-type '(unsigned-byte 8)))
(setq sv8 (make-foreign-vector 10 :element-type '(unsigned-byte 8) :initial-element 3))
(loop for i from 0 below 10 collect (aref sv8 i))

(setq sv2 (make-foreign-vector 32 :element-type '(unsigned-byte 2) :initial-element 3))
(loop for i from 0 below 32 collect (aref sv2 i))
(decode-sv-header sv1)
(decode-sv-header (make-array 10 :element-type '(unsigned-byte 8)))
(setq svbit (make-foreign-vector 10 :element-type '(unsigned-byte 1)))
(setq svindir (make-array (* 10 8) :element-type '(unsigned-byte 1) :displaced-to sv1))

(setq sv-doubleg (make-array 10 :element-type 'double-float :initial-element 1.4))
(setq sv-double (make-foreign-vector 10 :element-type 'double-float :initial-element 1.4d0))
(aref sv-double 0)
(describe sv-double)

(setq sv-singleg (make-array 10 :element-type 'single-float :initial-element 1.4))
(setq sv-single (make-foreign-vector 10 :element-type 'single-float :initial-element 1.4))
(aref sv-single 1)
(aref sv-singleg 1)
(setq sv-32 (make-foreign-vector 10 :element-type '(unsigned-byte 32)))
(setq sv-30 (make-foreign-vector 10 :element-type 'fixnum))

(setq sv-charq "foo")
(describe sv-charq)
(setq sv-charq (make-array 10 :element-type 'base-char :initial-element #\A))

(setq sv-char (make-foreign-vector 10 :element-type 'base-char :initial-element #\A))
(loop for i from 0 below 10 collect (aref sv-char i))
(setf (aref sv-char 9 ) #\null)
(setf (aref sv-char 1 ) #\B)
(describe sv-char)
(aref svbit 3)
(format t "~s" sv-char)
(decode-sv-header sv-char)
(decode-sv-header sv-charq)

(setf (aref sv1 0) #2r11010010)
(aref sv1 0)
(loop for i from 0 below 32 collect (aref svindir i))

(length sv1)
(describe sv1)

(lcl::stationary-object-p sv1) = nil
(stationary-object-p t) = t
(stationary-object-p 'foo) = t
(stationary-object-p (list nil t)) = nil

(unmake-foreign-vector sv1)

(sys::object-space sv1)
(sys::object-space svindir)
(sys::object-space sv-32)

(setq svt (make-foreign-vector 10 :element-type t))
(aref svt 0)
(setf (aref svt 0) t)


(defun tst8 (arr i)
  (declare (type (simple-array (unsigned-byte 8)(*)) arr)
	   (fixnum i))
  (aref arr i))
(disassemble 'tst8)

(defun tstd (arr i)
  (declare (type (simple-array double-float(*)) arr)
	   (fixnum i))
  (aref arr i))
(disassemble 'tstd)

(setf (aref sv-double 0) pi)
(setf (aref sv-double 1) 0.0)
(mem_read (+ (sys::%pointer sv-double) -6) ) ; header
(mem_read (+ (sys::%pointer sv-double) -2) )
(mem_read (+ (sys::%pointer sv-double) 2) )
(mem_read (+ (sys::%pointer sv-double) 6) )
(mem_read (+ (sys::%pointer sv-double) 10) )

(make-foreign-vector 255 :element-type 'double-float)
(make-foreign-vector 255 :element-type 'single-float)
(make-foreign-vector 256 :element-type '(unsigned-byte 8))
(loop repeat 100 do (make-foreign-vector 3000 :element-type 'double-float))
(make-foreign-vector 13000 :element-type 'double-float)
(make-foreign-vector 33000 :element-type 'double-float)
(make-foreign-vector 65000 :element-type 'double-float)

(loop repeat 50
      for arrs = (loop repeat 10000
		       collect (make-foreign-vector 33000 :element-type 'double-float)
		       collect (make-foreign-vector (1+ (* 8 33000))
				    :element-type '(unsigned-byte 8)))
      do (loop for arr in arrs
	       do (unmake-foreign-vector arr)))

(loop repeat 50
      for arrs = (loop repeat 10000
		       collect (make-foreign-vector 33000 :element-type 'double-float)
		      )
      do (loop for arr in arrs
	       do (unmake-foreign-vector arr)))

(with-static-area (make-array (* 12 1024 1024) :element-type '(unsigned-byte 8)))
(with-static-area (make-array (* 12 1024 1024) :element-type '(unsigned-byte 32)))

(list *make-foreign-vector-bytes-allocated* *make-foreign-vector-bytes-freed*)
(- *make-foreign-vector-bytes-allocated* *make-foreign-vector-bytes-freed*)

(def-foreign-synonym-type array-example-type
    (:pointer (:array :double-float (33000))))

(malloc-foreign-pointer :type 'array-example-type)

(loop repeat 10000
      for fp = (malloc-foreign-pointer :type 'array-example-type)
      when fp
	collect fp
	and sum (* 33000 8) into bytes
      else do (error "malloc-foreign-pointer failed after ~a bytes" bytes)
      )


|#






#| 


;;;  LOSE -- Cannot use SB-IMPL::MAKE-STATIC-VECTOR --
;;;       Not enough memory left in static space to allocate vector.



(defun make-array-initial-element-for-type (element-type)
  (setq element-type (upgraded-array-element-type element-type))
  (if (consp element-type)
      (case (car element-type)
	(signed-byte 0)
	(unsigned-byte 0)
	(otherwise (error "make-array-initial-element-for-type bad element-type: ~a" element-type)))
      (case element-type
	(fixnum 0)
	(bit 0)
	(double-float 0.0d0)
	(single-float 0.0f0)
	(character #\0)
	(otherwise (error "make-array-initial-element-for-type bad element-type: ~a" element-type)))))

(defun make-foreign-vector (length &key element-type initial-element initial-contents)
  (if initial-contents
      (sb-impl::make-static-vector length :element-type element-type :initial-contents initial-contents)
      (sb-impl::make-static-vector length 
				   :element-type element-type 
				   :initial-element 
				   (or initial-element (make-array-initial-element-for-type element-type)))))

;;;  will GC reclaim static vectors ?
(defun unmake-foreign-vector (arr)
  nil)

(defun make-stationary-vector (length &rest args &key element-type initial-element)
  (declare (ignore element-type initial-element))
  (apply 'make-foreign-vector length args))


(defun unmake-stationary-vector (vector)
  (declare (ignore vector))
  ;; do nothing -- GC will reclaim it once all references are gone.
  )


|#





#|

(defun init-arr (a &optional (zero 0))
  (loop for i from 0 below (length a) do (setf (aref a i) (+ zero i)))
  a)

(defun init-arrs (arrs type-code-size-zero-list)
  (loop for arr in arrs
	for (type size zero) in type-code-size-zero-list
	do (init-arr arr zero))
  arrs)

#-x86-64
(defun dump-array-header (a &optional (n 2))
  (format t "SBCL Array header for ~a #X~x~%" (type-of a) (sb-impl::sap-int (sb-sys::vector-sap a)))
  (loop with sap = (sb-sys::vector-sap a)
	for off from -6 to n
	for wd = (sb-sys:sap-ref-32 sap (* 4 off))
	do (format t "~8x~%" wd)))

#+x86-64
(defun dump-array-header (a &optional (n 2))
  (format t "SBCL Array header for ~a #X~x~%" (type-of a) (sb-impl::sap-int (sb-sys::vector-sap a)))
  (loop with sap = (sb-sys::vector-sap a)
	for off from -6 to n
	for wd = (sb-sys:sap-ref-64 sap (* 8 off))
	do (format t "~8x~%" wd)))

(defun dump-array-header2 (a &optional (n 2))
  (let* ((address (sb-kernel:get-LISP-OBJ-ADDRESS a))
	 (sap (sb-kernel::int-sap address)))
    (format t "SBCL Array header for ~a #X~x~%" (type-of a) address)
    (loop for off from -6 to n
	  for wd = (sb-sys:sap-ref-32 sap (* 4 off))
	  do (format t "~8x~%" wd))))

(defun make-test-arrays (type-code-size-zero-list)
  (loop for (type size zero) in type-code-size-zero-list
	for arr = (make-array size :element-type type)
	do (init-arr arr zero)
	   collect arr))

(sb-c::describe-compiler-policy)
  Basic qualities:
COMPILATION-SPEED = 0
DEBUG = 2
SAFETY = 1
SPACE = 1
SPEED = 3
SB-EXT:INHIBIT-WARNINGS = 1
  Dependent qualities:
SB-C::CHECK-CONSTANT-MODIFICATION = 1 -> 1 (maybe)
SB-C::TYPE-CHECK = 1 -> 2 (weak)
SB-C::CHECK-TAG-EXISTENCE = 1 -> 3 (yes)
SB-C::LET-CONVERSION = 1 -> 3 (on)
SB-C:VERIFY-ARG-COUNT = 1 -> 3 (yes)
SB-C::MERGE-TAIL-CALLS = 1 -> 3 (yes)
SB-C::INSERT-DEBUG-CATCH = 1 -> 0 (no)
SB-C::RECOGNIZE-SELF-CALLS = 1 -> 3 (yes)
SB-C::FLOAT-ACCURACY = 1 -> 3 (full)
SB-C:INSERT-STEP-CONDITIONS = 1 -> 0 (no)
SB-C::COMPUTE-DEBUG-FUN = 1 -> 2 (yes)
SB-C::PRESERVE-SINGLE-USE-DEBUG-VARIABLES = 1 -> 0 (no)
SB-C::INSERT-ARRAY-BOUNDS-CHECKS = 1 -> 3 (yes)
SB-C::STORE-XREF-DATA = 1 -> 3 (yes)
SB-C:STORE-COVERAGE-DATA = 1 -> 0 (no)


(defmacro make-test-arrays! (type-code-size-zero-list)
  `(init-arrs (list .,(loop for (type size zero) in type-code-size-zero-list
			    collect `(make-array ,size :element-type ',type)))
	      ',type-code-size-zero-list))

;;(disassemble 'make-xxx-test-arrs)


(defun make-xxx-test-arrs ()
  (declare (optimize (speed 0)(safety 3) (debug 3) 
		     (sb-c::PRESERVE-SINGLE-USE-DEBUG-VARIABLES 1)
		     (SB-C::MERGE-TAIL-CALLS 0)))
  (make-test-arrays! (((unsigned-byte 8) 7 0) 
		      ((signed-byte 8) 7 0)
		      ((unsigned-byte 16) 7 0) 
		      ((signed-byte 16) 7 0)
		      ((unsigned-byte 32) 7 0) 
		      ((signed-byte 32) 7 0)
		      (single-float 7 0.0f0)
		      (double-float 7 0.0d0))))

(defparameter *test-arrs* (make-test-arrays! (((unsigned-byte 8) 7 0) 
					      ((signed-byte 8) 7 0)
					      ((unsigned-byte 16) 7 0) 
					      ((signed-byte 16) 7 0)
					      ((unsigned-byte 32) 7 0) 
					      ((signed-byte 32) 7 0)
					      (single-float 7 0.0f0)
					      (double-float 7 0.0d0))))

(defparameter *test-arrs*
  (let ((size 7))
    (make-test-arrays `(((unsigned-byte 8) ,size 0) 
			((signed-byte 8) ,size 0)
			((unsigned-byte 16) ,size 0) 
			((signed-byte 16) ,size 0)
			((unsigned-byte 32) ,size 0) 
			((signed-byte 32) ,size 0)
			(single-float ,size 0.0f0)
			(double-float ,size 0.0d0)))))
	      
(defun dump-array-sizes (arrays)
  (loop for prev-arr-addr = nil then arr-addr
	for arr in arrays
	for arr-addr = (sb-impl::sap-int (sb-sys::vector-sap arr))
	do (format t "arr ~a addr #X~x~%" (type-of arr) arr-addr)
	when prev-arr-addr
	  do (format t "(- arr-addr prev-arr-addr) ~d~%" (- arr-addr prev-arr-addr))))

(dump-array-sizes *test-arrs*)
arr (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (7)) addr #XCC333B8
arr (SIMPLE-ARRAY (SIGNED-BYTE 8) (7)) addr #XCC33450
(- arr-addr prev-arr-addr) 152
arr (SIMPLE-ARRAY (UNSIGNED-BYTE 16) (7)) addr #XCC334E8
(- arr-addr prev-arr-addr) 152
arr (SIMPLE-ARRAY (SIGNED-BYTE 16) (7)) addr #XCC33600
(- arr-addr prev-arr-addr) 280
arr (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (7)) addr #XCC336A0
(- arr-addr prev-arr-addr) 160
arr (SIMPLE-ARRAY (SIGNED-BYTE 32) (7)) addr #XCC337C8
(- arr-addr prev-arr-addr) 296
arr (SIMPLE-ARRAY SINGLE-FLOAT (7)) addr #XCC33818
(- arr-addr prev-arr-addr) 80
arr (SIMPLE-ARRAY DOUBLE-FLOAT (7)) addr #XCC33888
(- arr-addr prev-arr-addr) 112

(describe (make-array 10 :element-type 'double-float))



(dump-array-sizes (make-test-arrays (loop for size from 0 to 16 collect `((unsigned-byte 8) ,size 0))))
arr (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (0)) addr #XACED748
arr (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (1)) addr #XACED760
(- arr-addr prev-arr-addr) 24
arr (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (2)) addr #XACED780
(- arr-addr prev-arr-addr) 32
arr (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (3)) addr #XACED7A0
(- arr-addr prev-arr-addr) 32
arr (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (4)) addr #XACED7C0
(- arr-addr prev-arr-addr) 32
arr (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (5)) addr #XACED7E0
(- arr-addr prev-arr-addr) 32
arr (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (6)) addr #XACED800
(- arr-addr prev-arr-addr) 32
arr (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (7)) addr #XACED820
(- arr-addr prev-arr-addr) 32
arr (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (8)) addr #XACED840
(- arr-addr prev-arr-addr) 32
arr (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (9)) addr #XACED860
(- arr-addr prev-arr-addr) 32
arr (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (10)) addr #XACED888
(- arr-addr prev-arr-addr) 40
arr (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (11)) addr #XACED8B0
(- arr-addr prev-arr-addr) 40
arr (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (12)) addr #XACED8D8
(- arr-addr prev-arr-addr) 40
arr (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (13)) addr #XACED900
(- arr-addr prev-arr-addr) 40
arr (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (14)) addr #XACED928
(- arr-addr prev-arr-addr) 40
arr (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (15)) addr #XACED950
(- arr-addr prev-arr-addr) 40
arr (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (16)) addr #XACED978
(- arr-addr prev-arr-addr) 40

;;; Conclusion:  arrays are allocated on multiple of 8 byte boundary

#x1c = 28 ; 28/4 = 7
(loop for arr in *test-arrs* do (dump-array-header arr))

(dump-array-header (init-arr (make-array 8 :element-type '(unsigned-byte 8))))
Array header for (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (16))
 D23E3DB
 110000B
      40
 110000B
      96
      40
 3020100
 7060504
 B0A0908

(dump-array-header (init-arr (make-array #x12 :element-type '(signed-byte 8))))
Array header for (SIMPLE-ARRAY (SIGNED-BYTE 8) (8))
 942FD09
 942FD09
 1100027
 1100027
      C2 type code
      20 length *4
 3020100 data
 7060504
 CD2FE37

(dump-array-header (init-arr (make-array 8 :element-type '(signed-byte 16))))
Array header for (SIMPLE-ARRAY (SIGNED-BYTE 16) (8))
 942FD39
 942FD39
 1100027
 1100027
      C6
      20
   10000
   30002
   50004

(dump-array-header (init-arr (make-array 8 :element-type '(unsigned-byte 16))))
Array header for (SIMPLE-ARRAY (UNSIGNED-BYTE 16) (8))
 942FC49
 C000511
 110000B
 1100027
      9E
      20
   10000
   30002
   50004

(dump-array-header (init-arr (make-array 8 :element-type 'single-float) 0.0f0))
Array header for (SIMPLE-ARRAY SINGLE-FLOAT (8))
 942AFA9
 942FB09
 110000B
 1100027
      D2
      20
       0
3F800000
40000000

(dump-array-header (init-arr (make-array 8 :element-type 'double-float) 0.0d0) 8)
Array header for (SIMPLE-ARRAY DOUBLE-FLOAT (8))
 942FB29
 942FB09
 110000B
 1100027
      D6
      20
       0
       0
       0
3FF00000
       0
40000000
       0
40080000
       0

|#




#|
On AMD64

(loop for arr in *test-arrs* do (dump-array-header arr))


SBCL Array header for (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (7)) #X1002828300
      58
       0
      8E
       0
      38
       0
 3020100
   60504
       0
SBCL Array header for (SIMPLE-ARRAY (SIGNED-BYTE 8) (7)) #X1002828320
       0
       0
      C2
       0
      38
       0
 3020100
   60504
       0
SBCL Array header for (SIMPLE-ARRAY (UNSIGNED-BYTE 16) (7)) #X1002828340
       0
       0
      96
       0
      38
       0
   10000
   30002
   50004
SBCL Array header for (SIMPLE-ARRAY (SIGNED-BYTE 16) (7)) #X1002828360
   50004
       6
      C6
       0
      38
       0
   10000
   30002
   50004
SBCL Array header for (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (7)) #X1002828380
   50004
       6
      B2
       0
      38
       0
       0
       1
       2
SBCL Array header for (SIMPLE-ARRAY (SIGNED-BYTE 32) (7)) #X10028283B0
       6
       0
      CA
       0
      38
       0
       0
       1
       2
SBCL Array header for (SIMPLE-ARRAY SINGLE-FLOAT (7)) #X10028283E0
       6
       0
      D6
       0
      38
       0
       0
3F800000
40000000
SBCL Array header for (SIMPLE-ARRAY DOUBLE-FLOAT (7)) #X10027CFAD0
       0
       0
      DA
       0
      38
       0
       0
       0
       0





#2r10010110 = 150 = #x96

(format nil "~2r" #x8E) "10001110" 142

(sb-impl::%vector-widetag-and-n-bits  '(unsigned-byte 8)) = 142
(format nil "~2r" 142)



|#


#||

;;; Experiment - use malloc here instead of static space.  Works, but
;;; only up to a point.

(def-foreign-function (heap-alloc (:name (freedius-prefix "zalloc"))
				  (:return-type :unsigned-64bit))
    (len :int))


(def-foreign-function (heap-free (:name (freedius-prefix "free"))
				  (:return-type :null))
    (pointer :unsigned-64bit))

(in-package :sb-vm)

(defun allocate-heap-vector (widetag length words)
  (declare (type (unsigned-byte #.n-widetag-bits) widetag)
           (type (unsigned-byte #.n-word-bits) words)
           (type index length))
  ;; WITHOUT-GCING implies WITHOUT-INTERRUPTS
  (or
   (without-gcing
     (let* ((nwords (logandc2 (+ lowtag-mask (+ words vector-data-offset 1))
                              lowtag-mask))
	    (nbytes (1+ (ash nwords word-shift)))
	    (pointer (qffi::heap-alloc nbytes))
            (vector (logior pointer other-pointer-lowtag))
            ;; rounded to dual word boundary Name refers to static
	    ;; pointer, but this was used to zero-terminate the array:
	    ;; (new-pointer (+ pointer (ash nwords word-shift)))
	    )
       (when (plusp pointer)
         (store-word widetag
                     vector 0 other-pointer-lowtag)
         (store-word (fixnumize length)
                     vector vector-length-slot other-pointer-lowtag)

;;         (store-word 0 new-pointer)

	 #+never ; only needed for static space
         (setf *static-space-free-pointer*
               (ash new-pointer (- n-fixnum-tag-bits)))

         (%make-lisp-obj vector))))
   (error 'simple-storage-condition
          :format-control "Not enough memory left in heap space to ~
                           allocate vector.")))

(in-package :sb-impl)

(defun make-heap-vector (length &key
                           (element-type '(unsigned-byte 8))
                           (initial-contents nil initial-contents-p)
                           (initial-element nil initial-element-p))
  "Allocate vector of LENGTH elements in heap space. Only allocation
of specialized arrays is supported."
  ;; STEP 1: check inputs fully
  ;;
  ;; This way of doing explicit checks before the vector is allocated
  ;; is expensive, but probably worth the trouble as once we've allocated
  ;; the vector we have no way to get rid of it anymore...
  (when (eq t (upgraded-array-element-type element-type))
    (error "Static arrays of type ~S not supported."
           element-type))
  (when initial-contents-p
    (when initial-element-p
      (error "can't specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))
    (unless (= length (length initial-contents))
      (error "There are ~W elements in the :INITIAL-CONTENTS, but the ~
              vector length is ~W."
             (length initial-contents)
             length))
    (unless (every (lambda (x) (typep x element-type)) initial-contents)
      (error ":INITIAL-CONTENTS contains elements not of type ~S."
             element-type)))
  (when initial-element-p
    (unless (typep initial-element element-type)
      (error ":INITIAL-ELEMENT ~S is not of type ~S."
             initial-element element-type)))
  ;; STEP 2
  ;;
  ;; Allocate and possibly initialize the vector.
  (multiple-value-bind (type n-bits)
      (sb!impl::%vector-widetag-and-n-bits element-type)
    (let ((vector
           (sb-vm::allocate-heap-vector type length
                                   (ceiling (* length n-bits)
                                            sb!vm:n-word-bits))))
      (cond (initial-element-p
             (fill vector initial-element))
            (initial-contents-p
             (replace vector initial-contents))
            (t
             vector)))))

||#
