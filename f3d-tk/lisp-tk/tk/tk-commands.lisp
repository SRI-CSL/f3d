(in-package :lisptk)

;;; *********************  TK-COMMANDS  *********************

(defmacro define-simple-tk-command (fnname command)
  `(defun ,fnname (&rest args)
    (apply 'tcl-eval ,command args)))

(define-simple-tk-command tk-pack 'pack)
(define-simple-tk-command tk-grid 'grid)
(define-simple-tk-command tk-place 'place)
(define-simple-tk-command tk-option 'option)
(define-simple-tk-command tk-update 'update)
(define-simple-tk-command tk-wm 'wm)
(define-simple-tk-command tk-tkwait 'tkwait)
(define-simple-tk-command tk-winfo 'winfo)
(define-simple-tk-command tk-focus 'focus)
(define-simple-tk-command tk-grab 'grab)
(define-simple-tk-command tk-popup 'tk_popup)
(define-simple-tk-command tk-lower 'lower)
(define-simple-tk-command tk-raise 'raise)
(define-simple-tk-command tk-flush 'flush)

(defmethod tk-destroy (widget)
  (when *tk-verbose* (format t "tk-destroy ~a~%" widget))
  (tcl-eval 'destroy widget))

;;;(defun tk-configure (widget attribute value)
;;;  (tcl-eval widget "configure" attribute value))

(defun tk-configure (widget &rest attribute-value-pairs)
  (tcl-cmd `(,widget configure ,@attribute-value-pairs)))

(defun tk-cget (widget attribute)
  (tcl-eval widget "cget" attribute))

(defun-cached tk-load-app-defaults (path)
  (when (and path (probe-file path))
    (format t "~%Loading app-defaults file ~a." path)
    (tcl-eval 'option 'readfile (tk::tcl-filename-filter path))) )

(defun-cached tk-load-app-defaults (path)
  (when (and path (probe-file path))
    (format t "~%Loading app-defaults file ~a." path)
    (if (string= (pathname-type path) "tcl")
	(tcl-eval 'source (tk::tcl-filename-filter path))
	(tcl-eval 'option 'readfile (tk::tcl-filename-filter path))) ))



;;;
;;; This version establishes a hard dependency on WORKING Tk color
;;; lookup (i.e., tcl/tk needs to be initialized):
;;;
#+never
(defun tk-color-rgb (color-name &key (widget ".") (normalize (/ 1.0 (ash 1 16))))
  (let ((rgb (handler-case (tcl-list-to-lisp (tcl-cmd `(winfo rgb ,widget ,color-name)))
		 (error nil))))
    (and rgb (loop for x in rgb collect (* x normalize)))))


(defun tk-color-rgb (color-name &key (widget ".") (normalize (/ 1.0 (ash 1 16))) (colors *color-rgb-mapping*))
  (declare (ignore widget normalize colors))
  (lx::color-name-to-3d color-name))


#|
(tk-color-rgb "lavender")
(tk-color-rgb (widget (gui::selected-window gui::*interactor*)) "foo")
|#



(defun raise-widget-toplevel (widget)
  (tcl-cmd `(raise (winfo toplevel  ,widget)))
  (do-events))

(defun raise-window-toplevel (window)
  (raise-widget-toplevel (widget window)))


(defun set-toplevel-widget-title (widget title)
  ;; TK has a bug in that 'wm title' just stores the title in a struct associated
  ;; with the toplevel window.  It doesn't do anything with until the window is mapped,
  ;; or restacked.
  ;; FIXME:  This restacking doesn't fix the problem unless there are other windows that can be raised.
  (tcl-cmd `(lower ,widget))
  (tcl-cmd `(wm title ,widget ,title))
  (tcl-cmd `(raise ,widget))
  ;;(format t "set-toplevel-widget-title ~a ~a~%" widget title)
  )

;;; LHQ Changed Fri Nov 24 2006
(defun set-toplevel-widget-title (widget title)
  ;; TK has a bug in that 'wm title' just stores the title in a struct associated
  ;; with the toplevel window.  It doesn't do anything with until the window is mapped,
  ;; or restacked.
  ;; FIXME:  This restacking doesn't fix the problem unless there are other windows that can be raised.
  ;(tcl-cmd `(lower ,widget))
  (tcl-cmd `(wm title ,widget ,title))
  ;(tcl-cmd `(raise ,widget))
  ;;(format t "set-toplevel-widget-title ~a ~a~%" widget title)
  )


(defun window-grabbed-p (widget)
  (not (equal (tk::tcl-cmd `(grab status ,widget)) "none")))

;;; warp the mouse pointer to the center of the window
(defmethod  warp-pointer (window x y)
  (tcl-cmd `(event generate ,(widget window) <<enter>> -warp 1 -x ,x  -y ,y)))

(defmethod enable-no-mouse-button-motion-callback (window)
  (tcl-cmd `(bind ,(widget window) "<Motion>" "{qcme_mouse_handler %W motion %b %s %x %y ;  break }")))

(defmethod disable-no-mouse-button-motion-callback (window)
  (tcl-cmd  `(bind ,(widget window) "<Motion>" "")))

  
(defun set-mouse-cursorpos (widget x y)
  (tcl-cmd `(event generate ,widget <<enter>> -warp 1 -x ,x -y ,y)))

  
(defun set-mouse-cursor (widget cursor-name)
  ;;(format t "set-mouse-cursor ~a ~a~%" widget cursor-name)
  ;;(when (equal cursor-name "")  (break))
  (tcl-cmd `(,widget config -cursor ,cursor-name)))


(defun bell ()
  (tk::tcl-cmd '(bell)))
