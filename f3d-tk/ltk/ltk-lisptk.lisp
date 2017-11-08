(in-package :ltk)

#| 

This software is a version of LTK (LTK Copyright notice below)
modified by Lynn H. Quam to operate directly with the Tcl/Tk libraries
without using pipe or socket connections to wish.

The original file ltk.lisp has been split into pieces to 
separate the communication specific parts from the generic parts.

Additional modifications have been made to support compatibility with the 
:LISPTK infrastructure of FREEDIUS.

|#

#|

 This software is Copyright (c) 2003, 2004, 2005, 2006  Peter Herth <herth@peter-herth.de>
 Parts Copyright (c) 2005 Thomas F. Burdick
 Parts Copyright (c) Cadence Design Systems, GmbH

 Peter Herth grants you the rights to distribute
 and use this software as governed by the terms
 of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html),
 known as the LLGPL.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
|#


#|
Communication between LTK the underlying Tcl/Tk libraries using LISPTK.
|#

(defparameter *ltk-command-result* nil)

(defparameter *debug-tk* nil)
;;(setq *debug-tk* t)

(defun send-wish (text)
  (when *debug-tk*
    (format t "~A~%" text)
    (finish-output))
  (let ((tk::*tk-verbose* nil))
    (setq *ltk-command-result* (tk::tcl-eval-internal text nil)) ))

(defun format-wish (control &rest args)
  (send-wish (apply #'format nil control args))) 

(defun format-wish-and-read (control &rest args)
 (let ((val  (send-wish (apply #'format nil control args))))
   (if (stringp val)
       (read-from-string val)
       val)))

;;; Problems deciding whether return string or (read-from-string string )

(defun format-wish-and-read-string (control &rest args)
 (send-wish (apply #'format nil control args)))
   
(defun format-wish-and-read-list (control &rest args)
 (send-wish (apply #'format nil control args)))
   
(defparameter *ltk-command-result* nil)

(defun init-wish ()
  ;; print string readable, escaping all " and \
  ;; proc esc {s} {puts "\"[regsub {"} [regsub {\\} $s {\\\\}] {\"}]\""}
  (send-wish "package require Tk")
  (send-wish "proc escape {s} {regsub -all {\\\\} $s {\\\\\\\\} s1;regsub -all {\"} $s1 {\\\"} s2;return $s2}")
;  (send-wish "proc senddata {s} {ltk_callback \"(:data [escape $s])\";flush stdout}")
;  (send-wish "proc senddata {s} {list \"[escape $s]\"}")
  (send-wish "proc senddata {s} {escape $s}")
  
;;  (send-wish "proc senddatastring {s} {ltk_callback \"(:data \\\"[escape $s]\\\")\";flush stdout} ") ;
;;  (send-wish "proc senddatastring {s} {list \"[escape $s]\"}")
  (send-wish "proc senddatastring {s} {escape $s}")
#+never
  (send-wish "proc sendevent {s x y keycode char width height root_x root_y mouse_button} {ltk_callback \"(:event \\\"$s\\\" $x $y $keycode $char $width $height $root_x $root_y $mouse_button)\"} ")

  (send-wish "proc sendevent {s x y keycode char width height root_x root_y mouse_button} {ltk_callback event \"(:event \\\"$s\\\" $x $y $keycode $char $width $height $root_x $root_y $mouse_button)\"} ")
  ;;; callback structure: (:callback "widgetname")          ;; for non-parameter callbacks
  ;;;                     (:callback "widgetname" val)      ;; wideget returns non-string value
  ;;;                     (:callback "widgetname" "string") ;; widget returns string value

;  (send-wish "proc callback {s} {ltk_callback \"\\\"$s\\\"\"}")
;  (send-wish "proc callbackval {s val} {ltk_callback \"\\\"$s\\\"\" $val}")
;  (send-wish "proc callbackstring {s val} {ltk_callback \"\\\"$s\\\"\" \\\"[escape $val]\\\"}")
  (send-wish "proc callback {s} {ltk_callback \"$s\"}")
  (send-wish "proc callbackval {s val} {ltk_callback \"$s\" $val}")
  (send-wish "proc callbackval2 {s val1 val2} {ltk_callback \"$s\" $val1 $val2}")
  (send-wish "proc callbackstring {s val} {ltk_callback \"$s\" \\\"[escape $val]\\\"}")
  (send-wish "proc callback_ret {s} {return [ltk_callback \"$s\"]}")


  (dolist (fun *init-wish-hook*)	; run init hook funktions 
    (funcall fun)))

;;; global var for holding the communication stream
(defstruct (ltk-connection (:constructor make-ltk-connection ())
			   (:conc-name #:wish-))
  (callbacks (make-hash-table :test #'equal))
  (after-ids (make-hash-table :test #'equal))
  (counter 1)
  (after-counter 1)
  ;; This is should be a function that takes a thunk, and calls it in
  ;; an environment with some condition handling in place.  It is what
  ;; allows the user to specify error-handling in START-WISH, and have
  ;; it take place inside of MAINLOOP.
  (call-with-condition-handlers-function (lambda (f) (funcall f)))
  )

(defun install-ltk-lisptk-fns ()
  (tk::tcl-create-command tk::*the-interpreter*
			  "ltk_callback"
			  'ltk-callback))


(defun cl-user::after-lisptk-init ()
  (if tk::*the-interpreter*
      (progn
	(ltk::install-ltk-lisptk-fns)
	(ltk::init-wish))
      (progn
	(format t "~%;;; --- adding after-init hooks for LTK")
	(setf tk::*tcl-tk-after-init-hooks*
	      (append tk::*tcl-tk-after-init-hooks*
		      '(ltk::install-ltk-lisptk-fns
			ltk::init-wish))))))

;;; FIXME: make sure this is run after :lisptk is initialized.  If it
;;; is not, nothing will happen and you will need to call the above
;;; function some other way.  Usually, this happens during a disksave
;;; restart...

;;; Still puking in disksaves:

(st:add-system-initialization :ltk '(cl-user::after-lisptk-init))




(defmacro with-ltk ((&rest keys &key (debug 2) stream serve-event &allow-other-keys)
		    &body body)
  (declare (ignorable keys debug stream serve-event))
  `(progn .,body))

;;; bogus
(defvar *EXIT-MAINLOOP* nil)



(defparameter *ltk-callback-args* nil)

;;; LTK-CALLBACK handles all events from ltk created widgets.
;;; This should be merged into the LISPTK event handler.
(defun ltk-callback (widget-name &rest args )
  (setq *ltk-callback-args* (list* widget-name args))
  (if (equal widget-name "event")
      (destructuring-bind (evt callback-name &rest args)
	  (read-from-string (car args))
	(let ((fun (gethash callback-name (wish-callbacks *wish*))))
	  (setq *foo2* (list callback-name fun args))
	  (if fun
	      ;;(apply fun (loop for arg in args collect (if (stringp arg) (read-from-string arg) arg)))
	      (funcall fun (construct-tk-event args))
	      (format t "ltk-callback: no callback function for ~a ~a~%" callback-name args))))

      (let ((fun (gethash widget-name (wish-callbacks *wish*))))
	(if fun
	    ;;(apply fun (loop for arg in args collect (if (stringp arg) (read-from-string arg) arg)))
	    (apply fun args)
	    (format t "ltk-callback: no callback function for ~a ~a~%" widget-name args)))
					; (break)
      ))

;(gethash (car *ltk-callback-args*) (wish-callbacks *wish*))



;;; Extended version of ltk-callback allowing commands to be handled by the 
;;; panel associated with the top-level widget (uppermost parent).





 
