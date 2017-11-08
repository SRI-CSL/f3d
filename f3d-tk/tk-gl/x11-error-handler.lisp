(in-package :gl)

;;; *****************  X11 ERROR HANDLER  *****************

#+never ; no longer used
(progn

(defparameter *x11-windowid-state-ht* nil)
;;(defparameter *x11-windowid-state-ht* (make-hash-table))

(defun tk::set-x11-window-destroyed (windowid)
  (when *x11-windowid-state-ht*
    (setf (gethash windowid *x11-windowid-state-ht*)
	  (get-internal-real-time))))


(defun x11-window-destroyed-p (windowid)
  (and *x11-windowid-state-ht*
       (gethash windowid *x11-windowid-state-ht*)))

;; 1 minute
(defparameter *sufficient-time-to-destroy-window* (* 160 internal-time-units-per-second))

;;; This needs to be called occasionally
(defun remove-old-x11-window-destroyed-entries ()
  (when *x11-windowid-state-ht*
    (loop with current-time = (get-internal-real-time)
	  with time-threshold = (- current-time *sufficient-time-to-destroy-window*)
	  with ht = *x11-windowid-state-ht*
	  for windowid being the hash-keys of ht using (hash-value destruction-time)
	  when (< destruction-time time-threshold)
	    do (remhash windowid ht))))

#|
(remove-old-x11-window-destroyed-entries)
*x11-windowid-state-ht*
(tk::tk-destroy-callback )
(setq *break-on-x11-errors* t)
(setq *break-on-x11-errors* nil)
|#


) ; end progn

(defparameter *break-on-x11-errors* nil)
(defvar *inhibit-x11_error_messages* nil)
(defparameter *ignore-badwindow-errors* t)

;;; For some unknown reason, Tcl/Tk generates BadWindow X Protocol Errors 
;;; with request_code = 15 (XQueryTree).  This problem appears to be benign. 
;;; This is used to suppress messages about them.

(defconstant *XRequest.XQueryTree* 15)
(defconstant *XProtoError.BadWindow* 3)

(def-foreign-callable (x11_lisp_error_handler
		       (:name (freedius-prefix "x11_lisp_error_handler"))
		       (:return-type nil)
		       (:library :liblisptk))
    ((error_text :simple-string)
     (error_code :signed-32bit)
     (request_code :signed-32bit)
     (minor_code :signed-32bit)
     (Resourceid :unsigned-long)
     )
  (unless (or *inhibit-x11_error_messages*
	      (and (eql error_code *XProtoError.BadWindow*) 
		   (eql request_code *XRequest.XQueryTree*)))
    (format t "X protocol error ~a
  error code = ~d
  request code = ~d
  minor code = ~d
  resource id = ~d~%"
	    error_text error_code request_code minor_code resourceid))
  (when *break-on-x11-errors*
    (break))
  0)


(def-foreign-function (set_x11_lisp_error_handler (:name (freedius-prefix "set_x11_lisp_error_handler"))))

(defun install-lisp-error-handler ()
  #-mswindows
  (progn (format t ";;; Installing x11_lisp_error_handler~%")
	 (set_x11_lisp_error_handler))
  )

