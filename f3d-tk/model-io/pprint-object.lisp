(in-package :freedius-io)

#|

Fri Oct 15 2004 LHQ:

CMUCL pprint infrastructure output from this code looks good.

Crap! Allegro output is readable, but the PPRINT-TAB behavior is wrong.
I spent many hours trying the track down this problem in Allegro.  
Allegro isn't even repeatable.  Calls to (GC) cause the behavior to change.
CC indicates that Allegro 6.2 doesn't have the problem.
See the file ~quam/lisp/allegro-bugs/20041016-pprint-bug.lisp.
|#

;(maybe-compile-file-load "$FREEDIUS/lisp/model-io/pprint-object.lisp")

(defparameter *print-float-format-fill* "~,3f")
(defparameter *print-float-format* nil)
;;; "~,3f"

;;; callable from pprint dispatch
(defmethod pprint-float (stream (object float) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format stream (or *print-float-format* *print-float-format-fill*) object))
  
;;; perhaps pprint-dispatch should be used to handle the printing of floats.
(defun pprint-token (token stream)
  (if (floatp token)
      (pprint-float stream token)
      (let ((*print-float-format* *print-float-format-fill*))
	(write token :stream stream))))

;;; These slots must have slot-value form  (LIST key1 val1 key2 val2 ...)
(defmethod pprint-slot ((parent t) slot-name slot-val stream)
  (ignore slot-name)
  (pprint-token slot-val stream))

(defun pprint-object-list (list stream)
  ;(pprint-tab :section-relative -20 1 stream)
  (pprint-newline :mandatory)
  (pprint-tab :section-relative 2 1 stream)
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    ;(pprint-indent :block -10 stream)
    (pprint-exit-if-list-exhausted)
    (loop 
      (pprint-gl-object-io-form (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      ;(pprint-newline :mandatory)
      (pprint-newline :mandatory))))

(defparameter *basic-object-key-value-slot-names*
  '(:graphics-style :attributes :metadata :property-list))

(defmethod pprint-slot ((parent obj::basic-gl-object) slot-name slot-val stream)
  (cond ((member slot-name *basic-object-key-value-slot-names*)
	 (pprint-key-val-pairs slot-val stream))
	((member slot-name '(:vertices :top-vertices))
	 (pprint-vertex-list slot-val stream))
	((member slot-name '(:origin :sizes :z-y-x-rotation))
	 ;(pprint-vertex slot-val stream "~10,3f")
	 (pprint-vertex slot-val stream)
	 )
	((eq slot-name :children)
	 (pprint-object-list slot-val stream))
	((eq slot-name :parent)
	 (pprint-lvcs-io-form slot-val stream))
	(t (pprint-token slot-val stream))))

(defparameter *vertex-element-nfraction-digits* 3)
(defparameter *vertex-element-print-format* "~10,3f")

;(fmakunbound 'pprint-vertex)
(defun pprint-vertex (vertex stream &optional (format-string *vertex-element-print-format*))
  (pprint-logical-block (stream vertex :prefix "(" :suffix ")")
    (loop 
      (format stream format-string (pprint-pop))
      (pprint-exit-if-list-exhausted)
      (write-char #\space stream))))

;(pprint-vertex '(-679.397 -313.293 32.834 22.966) *standard-output*)

(defun max-abs-element-size (vertex-list)
  (loop for v in vertex-list
	maximize (loop for c in v
		       maximize (max (abs c)))))

;(fmakunbound 'pprint-vertex-list)
(defun pprint-vertex-list (vertex-list stream)
  (let* ((fraction-digits *vertex-element-nfraction-digits*)
	 (integer-digits (ceiling (log (max-abs-element-size vertex-list) 10)))
	 (chars-per-element (+ 2 fraction-digits integer-digits))
	 (format-string (format nil "~~~d,~df" chars-per-element fraction-digits))
	 (ncomps (length (car vertex-list)))
	 (tabsize (+ 3 (* ncomps (1+ chars-per-element)))))
    ;;(setq *format-string* (list fraction-digits chars-per-element format-string tabsize))
    (pprint-logical-block (stream vertex-list :prefix "(" :suffix ")")
      (pprint-exit-if-list-exhausted)
      (loop (pprint-vertex (pprint-pop) stream format-string)
	    (pprint-exit-if-list-exhausted)
	    (write-char #\Space stream)
	    (pprint-tab :section-relative 0 tabsize stream)
	    (pprint-newline :fill stream)))))


#|
(let ((*print-right-margin* 100)
      (*print-pretty* t))
  (pprint-vertex-list '((-679.397 -313.293 32.834 22.966) (-734.519 -300.453 34.86 22.966)) *standard-output*))
|#

;;; ***************************  PPRINT-KEY-VAL-PAIRS  ***************************

(defparameter *pprint-key-val-pairs-tabsize* 4)

;(fmakunbound 'pprint-key-val-pairs)
(defun pprint-key-val-pairs (pairs stream &key (n-initial-elements 0) parent)
  (let ((*standard-output* stream)
	(max-key-length (+ 2 (loop for (key val) on (nthcdr n-initial-elements pairs) by #'cddr
				   maximize (length (symbol-name key))))))
    (pprint-logical-block (*standard-output* pairs :prefix "(" :suffix ")")
      (when (> n-initial-elements 0)
	(loop repeat n-initial-elements
	      do (princ (pprint-pop))	; (write (pprint-pop))
		 (write-char #\space) 
		 (pprint-tab :section-relative 0 1)
		 (pprint-newline :fill)
	      )
	(pprint-indent :block 4)
	(pprint-newline :linear)
	)
      (pprint-exit-if-list-exhausted)
      (loop (let ((key-val (list (pprint-pop) (pprint-pop))))
	      (pprint-logical-block (*standard-output* key-val)
		(let ((key (pprint-pop)) (value (pprint-pop)))
		  (when key
		    (pprint-token key *standard-output*) (write-char #\space)
		    (pprint-tab :section-relative 0 max-key-length)
		    (pprint-slot parent key value *standard-output*))))
	      (pprint-exit-if-list-exhausted)
	      (pprint-tab :section-relative 1 max-key-length)
	      ;;(pprint-newline :fill)
	      (pprint-newline :linear)
	      )))))

;;; version twiddled for allegro
#+allegro
(defun pprint-key-val-pairs (pairs stream &key (n-initial-elements 0) parent)
  (let ((*standard-output* stream)
	(max-key-length (+ 2 (loop for (key val) on (nthcdr n-initial-elements pairs) by #'cddr
				   maximize (length (symbol-name key))))))
    (pprint-logical-block (*standard-output* pairs :prefix "(" :suffix ")")
      (when (> n-initial-elements 0)
	(loop repeat n-initial-elements
	      do (princ (pprint-pop))	; (write (pprint-pop))
		 (write-char #\space) 
		 (pprint-tab :section-relative 0 1)
		 (pprint-newline :fill)
	      )
	(pprint-indent :block 4)
	(pprint-newline :linear)
	)
      (pprint-exit-if-list-exhausted)
      (loop (let ((key-val (list (pprint-pop) (pprint-pop))))
	      (pprint-logical-block (*standard-output* key-val)
		(let ((key (pprint-pop)) (value (pprint-pop)))
		  (when key
		    (pprint-token key *standard-output*) (write-char #\space)
		    ;; pprint-tab is sometimes failing in Allegro
		    (pprint-tab :section-relative 1 max-key-length) 
		    (pprint-slot parent key value *standard-output*))))
	      (pprint-exit-if-list-exhausted)
	      (pprint-tab :section-relative 1 max-key-length)
	      ;;(pprint-newline :fill)
	      (pprint-newline :linear)
	      )))))


;;; ***************************  PRINT-IO-FORM METHODS  ***************************

;;; Arglist is compatible with pprint dispatch.  Do not reverse the order of stream and object.
(defgeneric PRINT-IO-FORM (stream object &optional colon? atsign?))

#+never ; ??
(defmethod print-io-form (stream (object forward-referenced-object) &optional colon? atsign?)
  (declare (ignore colon? atsign? stream))
  )

(defmethod print-io-form (stream (object NULL) &optional colon? atsign?)
  (declare (ignore colon? atsign? stream))
  )

(defvar *dummy-gl-object* (make-instance 'obj::basic-gl-object))

(defun pprint-gl-object-io-form (form stream)
  (pprint-key-val-pairs (cdr form) stream :n-initial-elements 1 :parent *dummy-gl-object*)
  (pprint-newline :mandatory))

(defmethod print-io-form (stream (object obj::basic-gl-object) &optional colon? atsign?)
  (declare (ignore colon? atsign? stream))
  (pprint-gl-object-io-form (io-form object) stream)
  )

(defmethod pprint-lvcs-io-form (io-form stream)
  (let ((*print-float-format* "~f")) ; make sure enough digits are printed for latitute and longitude.
    (if (consp (cadr io-form)) 
	(if (eq (car (cadr io-form)) 'gdc) ; gdc-spec
	    (pprint-key-val-pairs io-form stream :n-initial-elements 2)
	    (error "illegal parent cs form ~a" io-form))
	(pprint-key-val-pairs io-form stream :n-initial-elements 1))))


(defmethod print-io-form (stream (lvcs transforms::local-vertical-coordinate-system) 
				 &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (pprint-lvcs-io-form (io-form lvcs) stream))


;;;(defmethod print-io-form (stream (feature-set feature-set) &optional colon? atsign?)
;;;  (declare (ignore colon? atsign? ))
;;;  (let ((*standard-output* stream)
;;;        (form `(WITH-FEATURE-SET ,(io-attributes feature-set) ,@(children feature-set)))
;;;        (*default-parent* (world feature-set))
;;;        )
;;;    (pprint-logical-block (*standard-output* form :prefix "(" :suffix ")")
;;;      (write 'WITH-COORDINATE-SYSTEM)
;;;      (pprint-newline :mandatory)
;;;      (pprint-tab :section-relative 1 1)
;;;      (pprint-indent :block 2)
;;;      ;;(write (io-form (world feature-set)))
;;;      (print-io-form *standard-output* (world feature-set))
;;;      (pprint-newline :mandatory)
;;;      (pprint-newline :mandatory)
;;;      (pprint-tab :section-relative 1 1)
;;;      (pprint-indent :block 2) 
;;;      (pprint-logical-block (*standard-output* form :prefix "(" :suffix ")")
;;;        (write (pprint-pop)) ; WITH-FEATURE-SET
;;;        (pprint-tab :section-relative 1 1)
;;;        ;; print (io-attributes feature-set)
;;;        (pprint-key-val-pairs (pprint-pop) *standard-output* :parent feature-set)
;;;        (pprint-newline :mandatory)
;;;        (pprint-newline :mandatory)
;;;        (pprint-exit-if-list-exhausted)
;;;
;;;        (loop (pprint-exit-if-list-exhausted)
;;;              (pprint-tab :section-relative 1 1)
;;;              ;; print each child
;;;              (print-io-form *standard-output* (pprint-pop))
;;;              (pprint-newline :mandatory)
;;;              )))))

(defun pprint-feature-set-io-form (io-form stream)
  (let ((*standard-output* stream))
    (pprint-logical-block (*standard-output* io-form :prefix "(" :suffix ")")
      (write (pprint-pop))		; WITH-COORDINATE-SYSTEM
      (pprint-newline :mandatory)
      (pprint-tab :section-relative 3 1)
      (pprint-lvcs-io-form (pprint-pop) *standard-output*) ;lvcs io-form
      (pprint-newline :mandatory)
      (pprint-newline :mandatory)
      (pprint-tab :section-relative 1 1)
      (pprint-logical-block (*standard-output* (pprint-pop) :prefix "(" :suffix ")")
	(write (pprint-pop))		; WITH-FEATURE-SET
	(pprint-tab :section-relative 1 1)
	;; print (io-attributes feature-set)
	(pprint-key-val-pairs (pprint-pop) *standard-output*) 
	(pprint-newline :mandatory)
	(pprint-newline :mandatory)
	(pprint-exit-if-list-exhausted)

	(loop (pprint-exit-if-list-exhausted)
	      (pprint-tab :section-relative 1 1)
	      ;; print each child
	      (pprint-gl-object-io-form (pprint-pop) *standard-output*)
	      (pprint-newline :mandatory)
	      )))))
			       
(defmethod print-io-form (stream (feature-set feature-set) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (let* ((lvcs (world feature-set))
	 (*default-parent* lvcs))
    (pprint-feature-set-io-form `(WITH-COORDINATE-SYSTEM 
				  ,(io-form lvcs)
				  (WITH-FEATURE-SET ,(io-attributes feature-set)
				    ,@(loop for child in (children feature-set)
					    collect (io-form child))))
				stream)))


(in-package :gui)

;;; This should move to a file in the CME system
;;; Autoload :model-io subsystem and recall save-feature-set

#+never
(defun save-feature-set (feature-set path)
  (unless (find-package :freedius-io) (st:load-system :model-io))
  (save-feature-set feature-set path))

(in-package :freedius-io)   

(defun save-feature-set (feature-set path &key (right-column 100) (if-exists :supersede))
  (when (probe-file path) (backup-file path))
  (with-open-file (st path :direction :output :if-exists if-exists)
    (let* ((pkg-name *model-file-package*)
	   (*package* (find-package pkg-name))
	   (*print-right-margin* right-column)
	   (*print-miser-width* nil)
	   (cl-user::*print-level* nil)
	   (cl-user::*print-length* nil)
	   (*print-pretty* t)
	   )
      ;(format st "(IN-PACKAGE :SYSTEM-TOOL)~%~%")
      ;; We expect feature-set files to be loaded with *PACKAGE* bound to
      ;; a package with ST:AUTOLOAD-SYSTEM imported such as :CME or *model-file-package*.
      (format st "(AUTOLOAD-SYSTEM :MODEL-IO)~%~%")
      (format st "(IN-PACKAGE ~s)~%~%" pkg-name)
      (loop for obj in (children feature-set)
	    when (typep obj 'radius-mixin)
	    do (REMOVE-ATTRIBUTE obj :snippets))
      (print-io-form st feature-set)))

  (test-model-file-for-package-qualifiers path)
  )

(defun test-model-file-for-package-qualifiers (path &key (print-warnings nil))
  (with-open-file (st path)
    (loop with eof = (list nil)
	  with bad-count = 0
	  for line = (read-line st nil eof)
	  for line-num from 0
	  until (eq line eof)
	  when (search "::" line)
	    do (when print-warnings
		 (warn "package qualifier found in file ~a at line ~a~%" path line-num))
	       (incf bad-count)
	  finally (unless (zerop bad-count)
		    (error "~a package qualifiers found in file ~a" bad-count path))
		  (return bad-count))))

(defun test-sexpression-for-package-qualifiers (sexpr package &optional warn)
  (typecase sexpr
    (symbol
     (let ((accessible (symbol-accessible-in-package sexpr package)))
       (cond (accessible
	       0)
	     (t (when warn  (warn "~s is not accessible~%" sexpr))
		1))))
    (cons 
     (+ (test-sexpression-for-package-qualifiers (car sexpr) package warn)
	(test-sexpression-for-package-qualifiers (cdr sexpr) package warn)))
    (otherwise 0)))

;(test-sexpression-for-package-qualifiers '(obj:basic-gl-object) *model-file-package*) = 0; ok
;(test-sexpression-for-package-qualifiers '(obj:gl-graphics-style) *model-file-package* t) = 1 ; ok


(defun test-model-file-for-package-qualifiers (path &key (print-warnings t) 
					       (package *model-file-package*))
  (with-open-file (st path)
    (loop with eof = (list nil)
	  with bad-count = 0
	  for form = (read st nil eof)
	  until (eq form eof)
	  for qualified-symbols = (test-sexpression-for-package-qualifiers form package print-warnings)
	  do (incf bad-count qualified-symbols)
	  when (and (> qualified-symbols 0)
		    nil
		    print-warnings)
	    do (warn "package qualifier found in file ~a in form ~a~%" path form)
	  finally (unless (zerop bad-count)
		    (error "~a package qualifiers found in file ~a" bad-count path))
		  (return bad-count))))
#|

(save-feature-set (car (obj::object-feature-sets (gui::selected-object))) 
		  #+cmu "/tmp/alv.fs.cmu"  #+allegro "/tmp/alv.fs.acl"
		  :right-column 80)
(test-model-file-for-package-qualifiers "/tmp/alv.fs.cmu")

(test-model-file-for-package-qualifiers  "/tmp/alv.fs")

cp /tmp/alv.fs $RADIUS/sites/alv/models/alv2

(save-feature-set (car (obj::object-feature-sets 
			     (cme::find-object-named (gui::get-3d-world-named "Fort Hood 2") "Building-96"))) 
		  "/tmp/ft-hood-2-sample-buildings.fs" 
		  :right-column 100)
(test-model-file-for-package-qualifiers "/tmp/ft-hood-2-sample-buildings.fs")
(test-model-file-for-package-qualifiers "$RADIUS/sites/ft-hood-2/models/Sample_Buildings")

(in-package :obj)

(st:load-system :model-io :initialize t :recompile t)
(st:load-system :model-io :initialize t)


(cme::find-object-named (cme::get-3d-world-named "Fort Hood 2") "Building-7")
(describe (cme::find-object-named (cme::get-3d-world-named "Fort Hood 2") "Building-7"))

|#
