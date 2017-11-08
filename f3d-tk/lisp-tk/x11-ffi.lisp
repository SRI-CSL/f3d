(in-package :lisptk)

;;; This stuff really should be in an X11 package (currently non-existant)

;;; Everything here should (now) be independent of tcl/tk

;;; This file isn't really needed anymore

;;; *****************  X11 ERROR HANDLER  *****************

(defvar *break-on-x11-errors* nil)

(defvar *inhibit-x11_error_messages* nil)

(def-foreign-callable (x11_lisp_error_handler
		       (:name (freedius-prefix "x11_lisp_error_handler"))
		       (:return-type nil)
		       (:library :liblisptk))
    ((error_text :simple-string)
     (error_code :signed-32bit)
     (request_code :signed-32bit)
     (minor_code :signed-32bit)
     (Resourceid :signed-32bit)
     )
  (unless *inhibit-x11_error_messages*
    (format t "X protocol error ~a
  error code = ~d
  request code = ~d
  minor code = ~d
  resource id = ~d~%"
	    error_text error_code request_code minor_code resourceid))

  (when *break-on-x11-errors*
    (break)))

(def-foreign-function (set_x11_lisp_error_handler (:name (freedius-prefix "set_x11_lisp_error_handler"))))

(defun install-lisp-error-handler ()
  #-mswindows
  (set_x11_lisp_error_handler)
)

(st::add-system-initialization :tk '(install-lisp-error-handler))

(def-foreign-function (XRaiseWindow (:name "XRaiseWindow"))
    xdisplay xwindow)

(def-foreign-function (window_stacking_order (:name (freedius-prefix "window_stacking_order")) (:return-type :int))
    xdisplay xwindow)

(def-foreign-function (window_child (:name (freedius-prefix "window_child")) (:return-type :int))
    (i :int))

;;; uses the array of windows saved in a C global set by window_stacking_order
(def-foreign-function (restack_windows (:name (freedius-prefix "restack_windows")))
    xdisplay free-p)

(def-foreign-function (XFlush (:name "XFlush") )
    xdisplay) 

(def-foreign-function (XSync (:name "XSync") )
    xdisplay discard)

#+unused
(def-foreign-function (XSetWindowBackingstore (:name (freedius-prefix "XSetWindowBackingstore")))
    xdisplay xwindow state)


#|
(raise-window-toplevel (gui::view-window (gui::top-view)))
(gui::visible-p (gui::view-window (gui::top-view)))
(tcl-cmd `(winfo screen ,(widget (gui::view-window (gui::top-view)))))
(raise-widget-toplevel (widget (gui::view-window (gui::top-view))))
(XRaiseWindow (xdisplay) (widget-xwindow (tcl-cmd `(winfo toplevel ,(widget (gui::view-window (gui::top-view)))))))
(tcl-cmd `(winfo toplevel ,(widget (gui::view-window (gui::top-view)))))
|#



;;; Unused code

#|
(def-foreign-function (get_window_visualid (:name (freedius-prefix "get_window_visualid")))
    xdisplay xwindow)

(get_window_visualid (aref (get_tk_displays) 0) (widget-xwindow ".frm.f2"))

(get_window_visualid (aref (get_tk_displays) 0)
		     (window-xwindow (gui::view-window (gui::top-view))))

(window-xwindow (gui::view-window (gui::top-view)))


(XSetWindowBackingstore (xdisplay) (window->xwindow (gui::view-window (gui::top-view))) 1)

(xdisplay)
(window->xwindow (gui::view-window (gui::top-view)))
|#

#|

;;; This came from tk-ffi.lisp

#-mswindows
(def-foreign-function (xsynchronize (:name "XSynchronize"))
    (display xdisplay)
  (on-off :signed-32bit))

(defun xsync-displays ()
  (let ((displays (with-static-area (make-array0 10 :element-type '(unsigned-byte 32)))))
    (tk_displays displays)
    (XSynchronize (aref displays 0)) ;;; Needs another argument...
    (format t "~%Display #x~x now in synchronous mode." (aref displays 0))
    ))

;;; (st:add-system-initialization :tk '(xsync-displays))

;;;
;;; It turns out that this is always defined when Tcl/Tk is around.
;;; Tcl/Tk will emulate it under non-X systems...
;;;
(def-foreign-function (xdrawlines (:name "XDrawLines"))
    (display xdisplay)
    (drawable :unsigned-32bit)
    (gc :unsigned-32bit)
    (xpoints (:pointer :unsigned-16bit))
    (n :signed-32bit)
    (mode :signed-32bit))
    
|#
