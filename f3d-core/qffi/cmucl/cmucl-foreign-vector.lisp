(in-package :qffi)
;;(in-package :lx)


#|
(setq fv1 (make-foreign-vector 1024 :element-type '(unsigned-byte 8)))
(unmake-foreign-vector fv1)
|#

(def-foreign-function (make_lisp_vector (:name (freedius-prefix "make_lisp_simple_vector")) (:return-type :lisp))
    (length :int) (typecode :int) (initial-element :lisp))

(def-foreign-function (make_integer_vector (:name (freedius-prefix "make_integer_simple_vector")) (:return-type :lisp))
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
  (lisp::%vector-type-code type-spec)
  #+never(cadr (assoc type-spec *sv-type-code-alist* :test 'equal))
  )

) ; end eval-when

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

To: Lynn Quam <quam@AI.SRI.COM>
Cc: cmucl-imp@cons.org
Subject: Re: Testing for foreign object addresses
From: Daniel Barlow <dan@telent.net>
Date: Sun, 04 May 2003 22:33:14 +0100
In-Reply-To: <E19Bf7C-0005Gg-00@rom.localdomain> (Lynn Quam's message of
 "Fri, 02 May 2003 11:18:34 -0700")
X-Spam-Status: No, score=0.0 threshold=8.0
X-Spam-Level: 
X-MIME-Autoconverted: from quoted-printable to 8bit by Seacliff.AI.SRI.COM id OAA25299

-----BEGIN PGP SIGNED MESSAGE-----
Hash: SHA1


Lynn Quam <quam@ai.sri.com> writes:

> Can anyone recommend a procedure to test whether an arbitrary address
> is in GC space or not?

In the absense of any better suggestion, something like

(<= (common-lisp::current-dynamic-space-start) 
    address
    (system:sap-int (kernel:dynamic-space-free-pointer)))

ought to do the trick.  Note this will return NIL if the address is in
the static (i.e. purified) space.

|#


#|
(list *make-foreign-vector-bytes-min-addr* *make-foreign-vector-bytes-max-addr*)
(foreign-vector-p (make-array 2))
(foreign-vector-p nil)
(setq fv (make-foreign-vector 10 :element-type '(unsigned-byte 8)))
(foreign-vector-p fv)
(unmake-foreign-vector fv)
(lisp::%vector-type-code 'single-float)
(simple_vector_typecode *foo*)
|#

    
(defun make-foreign-vector (length &key (element-type t) initial-element)
  (setq element-type (kernel::type-expand element-type))
  (multiple-value-bind (typecode elemsize) (lisp::%vector-type-code element-type)
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


(defun init-arr (a &optional (zero 0))
  (loop for i from 0 below (length a) do (setf (aref a i) (+ zero i)))
  a)

(defun dump-array-header (a &optional (n 2))
  (format t "CMU Array header for ~a~%" (type-of a))
  (loop with sap = (sys::vector-sap a)
	for off from -6 to n
	for wd = (sys:sap-ref-32 sap (* 4 off))
	do (format t "~8x~%" wd)))

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
CMU Array header for (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (8))
58394FF3
2800000B
      20
2800000B
      46
      20
 3020100
 7060504
5839506F
(dump-array-header (init-arr (make-array 8 :element-type '(signed-byte 8))))
Array header for (SIMPLE-ARRAY (SIGNED-BYTE 8) (8))
 942FD09
 942FD09
 1100027
 1100027
      C2
      20
 3020100
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