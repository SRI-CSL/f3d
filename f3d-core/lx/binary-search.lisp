(in-package :lx)

;;; Binary search code (shamelessly) taken from CMUCL/src/hemlock/table.lisp

;;;; Bi-SvPosition, String-Compare, String-Compare*

;;; Much like the CL function POSITION; however, this is a fast binary
;;; search for simple vectors.  Vector must be a simple vector and Test
;;; must be a function which returns either :equal, :less, or :greater.
;;; (The vector must be sorted from lowest index to highest index by the
;;; Test function.)  Two values are returned: the first is the position
;;; Item was found or if it was not found, where it should be inserted;
;;; the second is a boolean flag indicating whether or not Item was
;;; found.
;;; 

(defun bi-svposition (item vector test &key (start 0) end key)
  (declare (simple-vector vector) (fixnum start))
  (let ((low start)
	(high (if end end (1- (length vector))))
	(mid 0))
    (declare (fixnum low high mid))
    (loop
      (when (< high low) (return (values low nil)))
      (setf mid (+ (the fixnum (ash (the fixnum (- high low)) -1)) low))
      (let* ((array-item (svref vector mid))
	     (test-item (if key (funcall key array-item) array-item)))
	(ecase (funcall test item test-item)
	  (:equal (return (values mid t)))
	  (:less (setf high (1- mid)))
	  (:greater (setf low (1+ mid))))))))

;;; optimization to inline test and key, and declare the type of the items.
(defmacro define-numeric-bi-svposition-fn (fn-name &optional key (item-type t))
  `(defun ,fn-name (item vector &key (start 0) end)
     (declare (optimize (speed 3) (safety 0) (debug 0)))
     (declare (simple-vector vector) (fixnum start))
     (declare (type ,item-type item))
     (let ((low start)
	   (high (if end end (1- (length vector))))
	   (mid 0))
       (declare (fixnum low high mid))
       (loop
	 (when (< high low) (return (values low nil)))
	 (setf mid (+ (the fixnum (ash (the fixnum (- high low)) -1)) low))
	 (let* ((array-item (svref vector mid))
		(test-item ,(if key `(,key array-item) 'array-item)))
	   (declare (type ,item-type test-item))
	   (cond ((= item test-item)
		  (return (values mid t)))
		 ((< item test-item)
		  (setf high (1- mid)))
		 (t (setf low (1+ mid)))))))))

(define-numeric-bi-svposition-fn fixnum-bi-svposition nil fixnum)
(define-numeric-bi-svposition-fn double-float-bi-svposition nil double-float)


#|
(disassemble 'fixnum-bi-svposition)
(disassemble 'double-float-bi-svposition)

(setq *fvect* (vector 1 3 5 100 1000 100000))
(fixnum-bi-svposition 12 *fvect*)
(setq *df-vect* (vector 1.0 3.0 5.0 100.0 1000.0 100000.0))
(double-float-bi-svposition 12.0 *df-vect*)
|#

;;;; Insert-Element, remove-element

(eval-when (compile eval load)

;;; Insert-Element is a macro which encapsulates the hairiness of
;;; inserting an element into a simple vector.  Vector should be a
;;; simple vector with Num elements (which may be less than or equal to
;;; the length of the vector) and Element is the element to insert at
;;; Pos.  The optional argument Grow-Factor may be specified to control
;;; the new size of the array if a new vector is necessary.  The result
;;; of INSERT-ELEMENT must be used as a new vector may be created.
;;; (Note that the arguments should probably be lexicals since some of
;;; them are evaluated more than once.)
;;;
;;; We clear out the old vector so that it won't hold on to garbage if it
;;; happens to be in static space.
;;; 
(defmacro insert-element (vector pos element num &optional (grow-factor 2))
  `(let ((new-num (1+ ,num))
	 (max (length ,vector)))
     (declare (fixnum new-num max))
     (cond ((= ,num max)
	    ;; grow the vector
	    (let ((new (make-array (truncate (* max ,grow-factor)) :initial-element nil)))
	      (declare (simple-vector new))
	      ;; Blt the new buggers into place leaving a space for
	      ;; the new element
	      (replace new ,vector :end1 ,pos :end2 ,pos)
	      (replace new ,vector :start1 (1+ ,pos) :end1 new-num
		       :start2 ,pos :end2 ,num)
	      (fill ,vector nil)
	      (setf (svref new ,pos) ,element)
	      new))
	   (t
	    ;; move the buggers down a slot
	    (replace ,vector ,vector :start1 (1+ ,pos) :start2 ,pos)
	    (setf (svref ,vector ,pos) ,element)
	    ,vector))))

(defmacro remove-element (vector pos num)
  `(progn
     (replace ,vector ,vector :start1 ,pos :start2 (1+ ,pos) :end1 (1- ,num) :end2 ,num)
     (setf (svref ,vector (1- ,num)) nil)
     ,vector))

) ; eval-when






(defstruct-class binary-sorted-table (lx::base-struct-class)
  (vector 
   num-entries))

(define-numeric-bi-svposition-fn binary-sorted-table-bi-svposition car double-float)

(defmethod initialize-instance :after ((table binary-sorted-table) &key (initial-length 32)
				       &allow-other-keys)
  (setf (binary-sorted-table-vector table) (make-array initial-length :initial-element nil)
	(binary-sorted-table-num-entries table) 0))
  
;;; If entry is not found, returns insert position.
(defmethod find-entry ((table binary-sorted-table) key)
  (mv-bind (pos found)
      (binary-sorted-table-bi-svposition key (binary-sorted-table-vector table)
					 :end (1- (binary-sorted-table-num-entries table)))
    (if found
	(values (cdr (svref (binary-sorted-table-vector table) pos)) pos)
	(values pos nil))))

;(fmakunbound 'add-entry)
(defmethod add-entry ((table binary-sorted-table) key value &key (unique-keys-p nil) )
  (let ((vector (binary-sorted-table-vector table))
	(num-entries (binary-sorted-table-num-entries table))
	(new-entry (cons key value)))
    (declare (simple-vector vector))
    (declare (fixnum num-entries))
    (mv-bind (pos found) 
	(binary-sorted-table-bi-svposition key vector :end (1- num-entries))
      (declare (fixnum pos))
      (if (and found unique-keys-p)
	  (setf (svref vector pos) new-entry)
	  (progn (setf (binary-sorted-table-vector table)
		       (insert-element vector pos new-entry num-entries)
		       (binary-sorted-table-num-entries table) 
		       (1+ num-entries))
		 new-entry))
      ;;(values key value) ; why were multiple-values returned?
      value)))

(defmethod (setf find-entry) (value (table binary-sorted-table) key)
  (add-entry table key value)
  value)

(defmethod remove-entry ((table binary-sorted-table) key)
  (let ((vector (binary-sorted-table-vector table))
	(num-entries (binary-sorted-table-num-entries table)))
    (declare (simple-vector vector))
    (declare (fixnum num-entries))
    (mv-bind (pos found) 
	(binary-sorted-table-bi-svposition key vector :end (1- num-entries))
      (declare (fixnum pos))
      (when found
	(remove-element vector pos num-entries)
	(setf (binary-sorted-table-num-entries table) (1- num-entries))
	t))))

(defmethod list-entries ((table binary-sorted-table))
  (with-class-slot-values binary-sorted-table (vector num-entries) table
    (declare (simple-vector vector))
    (declare (fixnum num-entries))
    (loop for i fixnum from 0 below num-entries
	  collect (aref vector i))))

#|
(setq *table* (make-instance 'binary-sorted-table))

(add-entry *table* 1.0 'foo1)

(loop for (key val) in '((100.0 foo100) (10.0 foo10) (1.0 foo1) 
			 (100000.0 foo10000) (3.0 foo3) (5.0 foo5))
      do (add-entry *table* key val))

(describe *table*)
(find-entry *table* 7.0)
(find-entry *table* 10.0)

(remove-entry *table* 3.0)

|#
