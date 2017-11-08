(in-package :lisptk)

;;;  QCME-CALLBACK


#|
Panel is not required to be associated with a toplevel widget, it can be
associated with any collection widget above this widget.  If the widget has a
clidget that specifies a panel, dispatch to that panel's TK-CALLBACK handler.
Otherwise the toplevel widget is expected to have a clidget that specifies a
panel.
|# 

(defmethod widget-panel (widget)
  (let ((clidget (widget-clidget widget)))
    (or (and clidget (panel clidget))
	;; this next looks bogus -- widget-clidget does not return a panel.
	(widget-clidget (widget-toplevel widget)))))
#|
WIDGET-PANEL should be changed to call the tcl function widget_panel rather
than calling widget-toplevel.  Another change would be to pass widget_panel
directly to qcme-callback from qcme_lisp_callback.

(defmethod widget-panel (widget)
  (widget-clidget (tcl-eval 'widget_panel widget)))
|#

;;; Is thing a widget pathname: a string beginning with a period?
(defun widget-string-p (thing)
  (and (stringp thing) (> (length thing) 0) (eql (aref thing 0) #\.)))

(defvar *events-verbose* nil)

;;; This used to be defmethod rather than defun -- defmethod seems to trigger consing in cmucl.
(defun qcme-callback (widget event &optional args)
  ;;(return-from qcme-callback tcl_ok)
  ;;(setq *qcme-callback-args* (list widget event args))
  (when *events-verbose*
    (format t "qcme-callback ~a ~a ~a~%" widget event args))

  (let* ((event (canonicalize-event-handler-hash-table-key event)))
    ;; canonicalize-event-handler-hash-table-key is needed to convert EVENT from
    ;; a lower-case string to the form in which keys are stored in the hash-table.
    (if (not (widget-string-p widget))
	;; If we do not have a widget pathname, we end up dispatching on the event,
	;; which must have a handler installed with INSTALL-TK-EVENT-HANDLER.
	(tk-callback nil nil nil event args)
	
	(let* ((panel (widget-panel widget)))
	  (tk-callback panel
		       widget
		       (widget-symbol widget panel)
		       event
		       args)))
    tcl_ok))

#|
(setq *events-verbose* t)
(let* ((widget (car *qcme-callback-args*))
       (event (cadr *qcme-callback-args*))
       (args (caddr *qcme-callback-args*))
       (panel (widget-panel widget)))
  (tk-callback panel widget (widget-symbol widget panel) event args))

(let* ((widget (car *qcme-callback-args*))
       (event (cadr *qcme-callback-args*))
       (args (caddr *qcme-callback-args*))
       (panel (widget-panel widget)))
  (tk::callback-function panel))

(gui::selected-window gui::*interactor*)
|#

;;; This is only called when a widget that is not part of a widget-panel is destroyed.
;;; This is never called (I think).
(defun tk-destroy-callback0 (panel widget &rest args)
  ;;(break)
  (apply 'tk-destroy-callback panel widget args))

(defun install-qcme-widget-callbacks ()
  ;; Conditionalizing on *the-interpreter* LOOKS WRONG
  ;; Perhaps there needs to be a *tcl-intepreter-initialization-list*
  ;; that is run when the tk-main-window is created.
  (when *the-interpreter*
    (tcl-eval "initialize_qwidgets")	; must override defns in this file
    (tcl-create-command *the-interpreter*
			"qcme_lisp_callback"
			'qcme-callback)
    ;; This next appears to be unused.
    (install-tk-event-handler 'destroy 'tk-destroy-callback0)
    ))

;;(st::add-system-initialization :tk '(install-qcme-widget-callbacks))
(push #'install-qcme-widget-callbacks tk::*tcl-tk-after-init-hooks*)
