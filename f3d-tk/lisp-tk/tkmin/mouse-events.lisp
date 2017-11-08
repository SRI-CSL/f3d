(in-package :tk)

;;  This file is (will become) part of the generic-gui system,
;;; and its package should be changed to :GUI, and it should
;;; move to $FREEDIUS/lisp/basic-gui/mouse-events.lisp.

;;; *************  CHANGING THE MOUSE MODIFIER MAPPINGS  *************

#|
If you use a non-standard .Xmodmap, you need to call

   (set-mouse-modifier-key-bindings your-modifier-keylist)

|#


;;; These numbers are apparently specific to TK.
;;; Should these be DEFCONSTANT
(defvar *motion-button-left-mask* (ash 1 8))
(defvar *motion-button-middle-mask* (ash 1 9))
(defvar *motion-button-right-mask* (ash 1 10))
(defvar *motion-button-masks*
  (logior *motion-button-left-mask* *motion-button-middle-mask* *motion-button-right-mask*))


;;;    ****************  MODIFIER KEY BINDINGS ****************

;;; Each entry in this list is of the form:  (logical-modifier X11-modifier . keysym-names)

(defvar *modifier-key-bindings*)

(defparameter *modifier-key-bindings-virgin*
  '((alpha mod1 Alt_L)
    (beta control Control_L)
    (gamma shift Shift_L)))

(defparameter *modifier-key-bindings-windows-key-is-meta*
  '((alpha mod1 Alt_L)
    (beta mod2 Meta_L)
    (gamma control Control_L)))

(defparameter *modifier-key-bindings-windows-key-is-super*
  '((alpha mod1 Alt_L)
    (beta mod3 Super_L)
    (gamma control Control_L)))

(defparameter *modifier-key-bindings-cntl-alt-swapped*
  '((alpha control Control_L)
    (beta mod1 Alt_L)
    (gamma 1 Shift_L)))

(defparameter *modifier-key-bindings-cntl-alt-swapped-windows-key-is-meta*
  '((alpha control Control_L)
    (beta mod2 Meta_L)
    (gamma mod1 Alt_L)))

(defparameter *modifier-key-bindings-cntl-alt-swapped-windows-key-is-meta+scroll-lock*
  '((alpha control Control_L)
    (beta mod2 Meta_L)
    (gamma mod1 Alt_L)
    (lock mod5 Scroll_lock)))

(defparameter *modifier-key-bindings-cntl-alt-swapped-windows-key-is-super*
  '((alpha control Control_L)
    (beta mod3 Super_L)
    (gamma mod1 Alt_L)))

(defparameter *modifier-key-bindings-control-mod2-mod3*
  '((alpha control Control_L)
    (beta mod2 Meta_L)
    (gamma mod3 Super_L)))

(custom:defcustom *modifier-key-bindings-default* *modifier-key-bindings-virgin*
  (:groups (:lisptk))
  "Keyboard modifier key configuration.  See code for options.
")

#|
(set-mouse-modifier-key-bindings)
(set-mouse-modifier-key-bindings *modifier-key-bindings-virgin*)
(set-mouse-modifier-key-bindings *modifier-key-bindings-windows-key-is-meta*)
(set-mouse-modifier-key-bindings *modifier-key-bindings-windows-key-is-super*)
(set-mouse-modifier-key-bindings *modifier-key-bindings-cntl-alt-swapped*)
(set-mouse-modifier-key-bindings *modifier-key-bindings-cntl-alt-swapped-windows-key-is-meta*)
(set-mouse-modifier-key-bindings *modifier-key-bindings-cntl-alt-swapped-windows-key-is-meta*)
(set-mouse-modifier-key-bindings *modifier-key-bindings-cntl-alt-swapped-windows-key-is-meta+scroll-lock*)
(set-mouse-modifier-key-bindings *modifier-key-bindings-cntl-alt-swapped-windows-key-is-super*)

(set-mouse-modifier-key-bindings *modifier-key-bindings-control-mod2-mod3*)
|#

(defun set-mouse-modifier-key-bindings (&optional (modifier-key-bindings *modifier-key-bindings-default*))
  (setq *modifier-key-bindings* modifier-key-bindings)
  (lx::eval-cache-flush-function 'mouse-event-name)
  (set-mouse-modifier-translations modifier-key-bindings))


;;; ***************  MOUSE-EVENT NAMES  ***************

#|
The Lisp symbol for the combinations of keyboard modifiers and mouse-button
should probably use more abstract names than CONTROL, META, and SHIFT, since,
for the fingers, we would like the physical layout to remain invariant
even if the xmodmap mapping is changed.

So, we define a mapping between Tk keyboard modifiers names and
the FREEDIUS mouse-event names.  This is an example mapping:

Logical Name   Tk name      Tk state
GAMMA          Shift        1
               Shift_Lock   2
ALPHA          Control      4
BETA           Mod1         8
               Mod2         16
               Mod3         32
               Mod4         64
               Mod5         128
               ???          256             
               ???          512             button held

We can use virtual event definitions to provide a mappings between
the logical modifier names (alpha beta gamma) and the physical names
in an xmodmap independent manner.

|#

(export '(<Left> <Middle> <Right>
	  <Alpha-Left> <Alpha-Middle> <Alpha-Right>
	  <Beta-Left> <Beta-Middle> <Beta-Right>
	  <Gamma-Left> <Gamma-Middle> <Gamma-Right>
	  <Alpha-Beta-Left> <Alpha-Beta-Middle> <Alpha-Beta-Right>
	  <Alpha-Gamma-Left> <Alpha-Gamma-Middle> <Alpha-Gamma-Right>
	  <Alpha-Beta-Gamma-Left> <Alpha-Beta-Gamma-Middle> <Alpha-Beta-Gamma-Right>
	  <Beta-Gamma-Left> <Beta-Gamma-Middle> <Beta-Gamma-Right>
	  ))

(defvar *mouse-button-names*
  (vector nil 
	  "LEFT" 
	  "MIDDLE" 
	  "RIGHT" 
	  "BUTTON4"                  ; mouse scroll wheel up
	  "BUTTON5"                  ; mouse scroll wheel down
	  ))

(defun mouse-button-name (button-number)
  (if (< button-number 6) (aref *mouse-button-names* button-number)))

;;;(defparameter *modifier-name-bit-alist*
;;;  '((shift 1)
;;;    (shift-lock 2)
;;;    (control 4)
;;;    (mod1 8)
;;;    (mod2 16)
;;;    (mod3 32)
;;;    (mod4 64)
;;;    (mod5 128)
;;;    (mod6 256)
;;;    (mod7 512)        ; button held
;;;    ))

(defvar *modifier-name-bit-alist*
  '((shift 1)
    (shift-lock 2)
    (control 4)
    (mod1 8)
    (mod2 #x10) ; 16
    (mod3 #x20) ; 32
    (mod4 #x40) ; 64
    (mod5 #x80) ;128
    (mod6 #x100);256
    (mod7 #x200);512        ; button held
    ))
(defun modifier-name-bit (modifier-name)
  (cadr (assoc modifier-name tk::*modifier-name-bit-alist*)))

(defun modifier-mask-name (state)
  (if (= state 0)
      ""
      (loop with full-name = ""
	    for (key-name state-name) in *modifier-key-bindings*
	    for bit = (modifier-name-bit state-name)
	    when (logtest bit state )
	      do (setf full-name (format nil "~a-~a" full-name key-name ))
	    finally (return (if (> (length full-name) 0) (subseq full-name 1) "")))))

;;; This builds a list of all combinations of the state codes of the modifier keys.
(defun logical-modifier-combinations (&optional (modifier-bindings *modifier-key-bindings*) (bits 0))
  (if modifier-bindings
      (destructuring-bind (name state-name . rest) (car modifier-bindings)
	(ignore name rest)
	(let ((state (modifier-name-bit state-name)))
	  (nconc
	   (logical-modifier-combinations (cdr modifier-bindings) bits)
	   (logical-modifier-combinations (cdr modifier-bindings) (logior state bits)))))
      (list bits)))

#|
(logical-modifier-combinations)
(eval-cache-flush-function 'mouse-event-name)
(get 'mouse-event-name 'eval-cache-fu
|#
;;; This returns a symbol, not a string
(defun-cached mouse-event-name (button state)
  (let* ((modifier-mask-name (modifier-mask-name state))
	 (event-name (intern (if (zerop (length modifier-mask-name))
				 (format nil "<~a>" (mouse-button-name button))
				 (format nil "<~a-~a>" modifier-mask-name (mouse-button-name button)))
			     :tk)))
    event-name))

(st:add-system-initialization :tkmin '(set-mouse-modifier-key-bindings))

;;; *********************  MOUSE-EVENT-MAP  *********************

;;; MOUSE-EVENT-NAME  is a lisp symbol such as <ALPHA-BETA-GAMMA-LEFT>

(defun make-mouse-event-map ()
  (make-hash-table))

(defmethod mouse-event-binding ((context hash-table) mouse-event-name)
  (gethash mouse-event-name context))
		
(defmethod (setf mouse-event-binding) (info (context hash-table) mouse-event-name)
  (setf (gethash mouse-event-name context) info))

;;; Not clear we need a structure here rather than a simple list.
(defstruct-class mouse-event-info (lx::property-list-mixin)
  ((function :initform nil :initarg :function)))

;;;(defmethod print-object ((object mouse-event-info) stream)
;;;  (with-class-slots mouse-event-info (function) object
;;;    (format stream "#<~S ~A #X~x>"
;;;            (type-of object)
;;;            function
;;;            (%pointer object))))

(defmethod print-object ((object mouse-event-info) stream)
  (with-class-slots mouse-event-info (function) object
    (print-unreadable-object (object stream :type t :identity t)
      (princ function stream))))

(defun make-mouse-event-info (function plist)
  (make-instance 'mouse-event-info
		 :function function
		 :property-list plist))

;(trace mouse-event-binding)
;(trace get-mouse-event-function)
;(untrace)


(defmethod get-mouse-event-function (context mouse-event-name)
  (let ((info (mouse-event-binding context mouse-event-name)))
    (with-class-slots mouse-event-info (function) info
      (and info
	   (values function
		   (get-prop info :drag-type)
		   (get-prop info :wheel-op)
		   )))))

(defmethod mouse-event-property (context mouse-event-name prop-key)
  (let ((info (mouse-event-binding context mouse-event-name)))
    (and info (get-prop info prop-key))))

(defmethod (setf mouse-event-property) (value context mouse-event-name prop-key)
  (let ((info (mouse-event-binding context mouse-event-name)))
    (and info (setf (get-prop info prop-key) value))))

(defun add-mouse-event (context mouse-event-name function &optional property)
  (setf (mouse-event-binding context mouse-event-name)
	(make-mouse-event-info function property)))

(defun add-mouse-events (context mouse-event-specs)
  (loop for (mouse-event-name function . plist) in mouse-event-specs
	do (setf (mouse-event-binding context mouse-event-name)
		 (make-mouse-event-info function plist)))
  context)





(defparameter *doc-line-width* 80)

(defun-cached mouse-modifier-doc-string (state-name mouse-event-map)
  (let* ((nchars *doc-line-width*)
	 (n3 (- (floor nchars 3) 3)))
    (flet ((button-doc (button)
	     (let* ((name
		     (intern
		      (if (zerop (length state-name))
			  (format nil "<~a>" (tk::mouse-button-name button))
			  (format nil "<~a-~a>" state-name (tk::mouse-button-name button)))
		      :tk))
		    (doc (get-prop (gethash name mouse-event-map) :bucky-doc)))
	       ;;(format t "~a => ~a~%" name doc)
	       (or doc ""))))
      
      (format nil "L: ~va M: ~va R: ~va"
	      n3 (button-doc 1) n3 (button-doc 2) n3 (button-doc 3)))))

(defun recompute-doc-strings (width)
  (setq *doc-line-width* width)
  (lx::eval-cache-flush-function 'mouse-modifier-doc-string)
  )

(defun set-doc-line (modifier-state mouse-event-map)
  (when modifier-state
    (set-documentation
     (mouse-modifier-doc-string (tk::modifier-mask-name modifier-state)
				mouse-event-map))))

(defvar *mouse-modifier-translations*)

(defun tk::set-mouse-modifier-translations (modifier-keylist)
  (setq *mouse-modifier-translations*
	(loop for (key-name state-name . keynames) in modifier-keylist
	      nconc (loop for keyname in keynames
			  collect (cons (symbol-name keyname)
					(tk::modifier-name-bit state-name))))))

(defun modifier-keyname-bit (keyname)
  (or (cdr (assoc keyname *mouse-modifier-translations* :test #'string-equal))
      0))

;;;(defun-cached mouse-modifier-translation (keyname)
;;;  (assoc keyname *mouse-modifier-translations* :test #'string-equal))
	
(defvar *current-modifier-mask* 0)
			     
(defun add-to-modifier-mask (keyname)
  (setf *current-modifier-mask*
	(logior *current-modifier-mask* (modifier-keyname-bit keyname))))

(defun remove-from-modifier-mask (keyname)
  (setf *current-modifier-mask*
	(logand *current-modifier-mask* (lognot (modifier-keyname-bit keyname)))))


(defun update-modifier-mask (new-mask)
  (setq new-mask (logand new-mask (lognot #x200))) ; remove button-held bit
  ;(format t "update-modifier-mask ~16r~%" new-mask)
  (setf *current-modifier-mask* new-mask))


(defun update-modifier-mask (new-mask)
  (declare (ignore new-mask)))

;(untrace)
;(trace add-to-modifier-mask)
;(trace modifier-keyname-bit)


#|

From CME6

(defmethod ui-bucky-menu ((mouse-window ic::pane) (object t ))
  (ui-bucky-menu (get-user-interface-context mouse-window) object))

(defmethod ui-bucky-menu ((ui t) (object t ))
  (ui-make-bucky-menu ui object))


(defmethod get-modifier-map ()
  )

|#
