(in-package :qffi)

#|
Definitions for ARRAY-DATA-ADDRESS which is used to convert arguments to foreign functions
declared to be type :ARRAY. 

|#

#+never
(declaim (optimize (compilation-speed 0) 
		   (speed 3) 
		   (safety 0)))

;;; With these constants, EQ may be used to test the ARRAY-ELEMENT-TYPE
(defconstant *signed-byte-8* (array-element-type (make-array 1 :element-type '(signed-byte 8))))
(defconstant *unsigned-byte-8* (array-element-type (make-array 1 :element-type '(unsigned-byte 8))))
(defconstant *signed-byte-16* (array-element-type (make-array 1 :element-type '(signed-byte 16))))
(defconstant *unsigned-byte-16* (array-element-type (make-array 1 :element-type '(unsigned-byte 16))))
(defconstant *signed-byte-32* (array-element-type (make-array 1 :element-type '(signed-byte 32))))
(defconstant *unsigned-byte-32* (array-element-type (make-array 1 :element-type '(unsigned-byte 32))))
#+x86-64
(progn
(defconstant *signed-byte-64* (array-element-type (make-array 1 :element-type '(signed-byte 64))))
(defconstant *unsigned-byte-64* (array-element-type (make-array 1 :element-type '(unsigned-byte 64))))
) ; end progn

(defun array-bytes-per-element (array)
  (declare (optimize (speed 3)(safety 0)))
  (declare (type array array))
  (declare (values (integer 0 8)))
  (let ((type (array-element-type array)))
    (cond ((or (eq type *signed-byte-8*)
	       (eq type *unsigned-byte-8*))
	   1)
	  ((or (eq type *signed-byte-16*)
	       (eq type *unsigned-byte-16*))
	   2)
	  ((or (eq type *signed-byte-32*)
	       (eq type *unsigned-byte-32*))
	   4)
	  #+x86-64
	  ((or (eq type *signed-byte-64*)
	       (eq type *unsigned-byte-64*))
	   8)
	  ((eq type 'single-float)
	   4)
	  ((eq type 'double-float)
	   8)
	  (t
	   (error "Unknown specialized array element type")
	   0) ; zero needed in order for compiler to understand that this function returns (integer 0 8)
	  )))

;; ARRAY-DATA-AND-BYTE-OFFSET can be called without consing
(defun array-data-and-byte-offset (array)
  (declare (type array array))
  (declare (values simple-array fixnum))
  (declare (optimize (speed 3) (safety 0)))
  (sb-kernel::with-array-data ((data array) (start) (end))
    (declare (fixnum start))
    (declare (ignore end))
    (values data
	    (if (= start 0)
		0
		(* start (the (integer 0 16) (array-bytes-per-element data)))))))


;;; WARNING: It is critical that INLINE-ARRAY-DATA-ADDRESS and INLINE-SIMPLE-ARRAY-DATA-ADDRESS be
;;; inlined and do no consing, particularly of bignums.  Failure to comply with these requirements
;;; could result in incorrect array addresses being passed to foreign-functions due to movement
;;; of the array by the garbage collector.

;;; This version has minimal inlined footprint and is GC safe.  It works for displaced arrays as
;;; well as simple arrays.
(declaim (inline inline-array-data-address))

#+never
(deftype machine-address () '(unsigned-byte #+x86-64 64 #-x86-64 32))

#+x86-64
(deftype machine-address () '(signed-byte 48))

#-x86-64
(deftype machine-address () '(unsigned-byte 32))

(defun inline-array-data-address (array)
  (declare (values machine-address))
  (declare (optimize (speed 3) (safety 0)))
  #+cmu(declare (ext:optimize-interface (speed 3) (safety 0)))
  (if array
      (multiple-value-bind (array bytes-offset)
	  (array-data-and-byte-offset array) ; array-data-and-byte-offset can be called without consing
	;;(declare ((unsigned-byte #+x86-64 64 #-x86-64 32) bytes-offset))
	(declare (fixnum bytes-offset))
	;; Do the  arithmetic inline
	(let ((addr ;;(+ bytes-offset 8 (logandc1 7 (sb-kernel:get-lisp-obj-address array)))
	       (+ bytes-offset 1 (the machine-address (sb-kernel:get-lisp-obj-address array)))
	       ))
	  (declare (type machine-address addr)
		   #+cmu (optimize (speed 3) (safety 0) (ext:inhibit-warnings 3)))
	  addr))
      0))

;(disassemble 'inline-array-data-address)

;;; Simple arrays only, totally inline, has minimal footprint and is GC safe.
(declaim (inline inline-simple-array-data-address))

(defun inline-simple-array-data-address (array)
  (declare (values machine-address))
  (declare (type simple-array array))
  (declare (optimize (speed 3) (safety 0)))
  #+cmu (declare (ext:optimize-interface (speed 3) (safety 0)))
  (when (sb-kernel::array-header-p array)         ; Handle multi-dimension case 
    (setq array (sb-kernel::%array-data-vector array))) ; get underlying simple-vector
  (let ((addr ;;(+ 8 (logandc1 7 (the machine-address (sb-kernel:get-lisp-obj-address array))))
	 (+ 1 (the machine-address (sb-kernel:get-lisp-obj-address array)))))
    (declare (type machine-address addr)
	     (optimize (speed 3) (safety 0) #+cmu (ext:inhibit-warnings 3)))
    addr))


(defun array-data-address (array)
  (inline-array-data-address array)) 
