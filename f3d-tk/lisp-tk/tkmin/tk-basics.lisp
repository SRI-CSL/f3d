(in-package :lisptk)

;;; This class defines slots in violation to proposed defstruct extension.
(defclass related-panels-mixin (property-list-mixin)
    ((related-panels :initform nil :initarg :related-panels :accessor related-panels)))

(defclass widget-panel (related-panels-mixin)
     ((widget :initform nil :initarg :widget :accessor widget )
      (item-alist :initform nil :initarg :item-alist :accessor item-alist)
      (callback-function :initform nil :initarg :callback-function :accessor callback-function)
      (package :initform *package* :accessor panel-package)
      ))


;;; from tk-callbacks.lisp

;;; TK-CALLBACK

(defparameter *tk-callback-event-handler-hash-table* (make-hash-table :test 'equal))
#|
(hash-table-count *tk-callback-event-handler-hash-table*) = 1
|#

;;; Arg can be a string or a symbol.
(defun canonicalize-event-handler-hash-table-key (string)
  (intern (string-upcase string) :lisptk))

;;; This alternative should also work, but event arg passed to tk-callback methods
;;; will become a lowercase string rather than a symbol.  Maybe this is a bad idea.
;;;(defun canonicalize-event-handler-hash-table-key (string)
;;;  (string-downcase string))

;;; name is either a widget name or an event name.
(defun install-tk-event-handler (name function)
  (setf (gethash name ;; (canonicalize-event-handler-hash-table-key name)
		 *tk-callback-event-handler-hash-table*)
	function))
    
;;; This is called if there is no panel associated with the tcl event.
;;; This is used for tcl timer events and other events that are not associated with
;;; widgets.

(defmethod tk-callback (panel widget item-name event args)
  (let ((event-handler (or (gethash widget *tk-callback-event-handler-hash-table*)
			   (gethash event *tk-callback-event-handler-hash-table*))))
    #+never
    (format t "tk-callback ~a ~a ~a ~a ~s ~a~%"
	    panel widget item-name event args event-handler)

    (if event-handler
	(funcall event-handler panel widget item-name event args)
	(format t "tk-callback UNHANDLED EVENT ~a ~a ~s ~s~%"
		widget item-name event args))))

