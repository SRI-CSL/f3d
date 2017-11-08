(in-package :qffi)

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

(defvar *default-make-array-allocation* nil)

;;; Really should put advice around make-array to force the :allocation.
;;; Allegro does not zero array alements by default, so you get total randomness
;;; unless :INITIAL-ELEMENT is specified.
;;; make-array0 makes sure the array elements are zeroed when :INITIAL-ELEMENT
;;; (or :INITIAL-CONTENTS) is not specified.

(defun make-array0 (dims &rest args
			&key
			element-type
			initial-element
			initial-contents
			(allocation *default-make-array-allocation*)
			&allow-other-keys)
  
  (if (or (null element-type) (eq element-type t) initial-element initial-contents)
      (apply #'sys::make-array dims :allocation allocation args)
      
      (let ((init (make-array-initial-element-for-type element-type)))
        (if init
            (apply #'sys::make-array dims :initial-element init :allocation allocation args)
            (apply #'sys::make-array dims :allocation allocation args)))))


(defun make-foreign-vector (length &rest args &key element-type &allow-other-keys)
  (apply 'make-array0 length
	 :element-type (or element-type t)
	 ;;  :static-reclaimable doesn't seem to work -- arrays move
	 :allocation :lispstatic-reclaimable
	 args))

(defun make-foreign-array (dims &rest args &key element-type &allow-other-keys)
  (apply #'make-array dims 
	 :element-type (or element-type t) 
	 :allocation :lispstatic-reclaimable))


;;; In Allegro, GC will reclaim foreign-vectors in the :lispstatic-reclaimable area.
(defun unmake-foreign-vector (arr)
  ;;(declare (ignore arr))
  (when (numberp arr)
    (excl:aclfree arr))
  nil)

(defun make-stationary-vector (length &rest args
			       &key (element-type t) initial-element)
  (declare (ignore element-type initial-element))
  (apply 'make-foreign-vector length args))

(defun make-stationary-array (dims &rest args
			       &key (element-type t) initial-element)
  (declare (ignore element-type initial-element))
  (apply 'make-foreign-vector dims args))

(defun unmake-stationary-vector (vector)
  (declare (ignore vector))
  ;; do nothing -- GC will reclaim it once all references are gone.
  )

(defun unmake-stationary-array (vector)
  (declare (ignore vector))
  ;; do nothing -- GC will reclaim it once all references are gone.
  )
