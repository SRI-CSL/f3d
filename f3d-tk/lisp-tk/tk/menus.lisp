(in-package :lisptk)

;;; Most of this should be in the gui package, not the tk package.

;;; MENU SUPPORT -- PROBABLY DOES NOT BELONG IN THIS FILE

;;; This takes an list of items of the form (button-label type value . rest)
;;; and converts it to the form needed by add_menu_items defined in
;;; $FREEDIUS/tk/library/composite-widgets.tcl.
;;; Type may be :SEPARATOR :MENU :QUOTE :EVAL :COMMAND or other.
;;; The format of ITEM-LIST is mostly compatible with CME menu-item-lists.


(defparameter *tk-menu-args-hack* t)
#|
(tk::tcl-cmd `(source ,(namestring (truename "$F3D/tk/library/composite-widgets.tcl"))))
(lx::eval-cache-flush-function 'make-menu)
(setq *tk-menu-args-hack* t)
(trace convert-menu-item-list)
|#

(defun convert-menu-item-list (item-list &optional menu-widget menu-name panel-class)
  (when (symbolp item-list)
    (setq item-list (symbol-value item-list)))
  (let ((*package* (find-package :keyword)))
    (loop with commands
	  with submenu-widgets
	  for item in item-list
	  for index from 0
					;          for (button-label type value . rest ) = item
	  for (button-label . plist) = item
	  with (type value)
	  when (eq (car plist) :accel)
	    do (setq plist (cddr plist)) ; ignore the :accel spec
	  do (setq type (car plist) value (cadr plist))
	  when button-label ; ignore menu items with no label -- provided for bare :accel specs
	    collect (case type
		      (:separator `(list -separator))
		      (:menu
		       (let ((submenu-widget (format nil "~a.sm~a" menu-widget index)))
			 (multiple-value-bind (itemlist cmds submenu-wids)
			     (convert-menu-item-list value submenu-widget)
			   (setq submenu-widgets (append submenu-widgets
							 (cons submenu-widget submenu-wids)))
			   (setq commands(append commands
						 `((qmenu ,submenu-widget -items (list .,itemlist))
						   .,cmds))))
			 (if *tk-menu-args-hack*
			     `(list 0 ,button-label -button_type cascade -menu ,submenu-widget)
			     `(list cascade -label ,button-label -menu ,submenu-widget))))
		      (:quote `(list (list "QUOTE" ,(format nil "~s" value)) ,button-label))
		      (:eval `(list (list "EVAL" ,(format nil "~s" value)) ,button-label))
		      (:command
		       (when (consp value)
			 (setq value (generate-popup-menu-command-method panel-class menu-name value))) 
		       `(list (list "COMMAND" ,(format nil "~s" value) ,(format nil "~s" menu-name)) ,button-label))
		      (otherwise
		       `(list (list ,type ,value) ,button-label))
                  
		      )
	      into itemlist
	  finally (return (values itemlist commands (reverse submenu-widgets))))))




(defun make-menu-script (item-list menu-widget)
  (multiple-value-bind (tcl-item-list commands submenu-widgets)
      (convert-menu-item-list item-list menu-widget)
    (values `((qmenu ,menu-widget -tearoff 0 -items (list .,tcl-item-list))
	      .,commands)
	    submenu-widgets)))

#|
(tk::tcl-cmd `(wm deiconify .top5))
(tk::tcl-cmd `(wm deiconify .top6))
(tk::tcl-cmd `(wm withdraw .top6))

(pprint (gui::object-popup-menu-item-list gui::*default-popup-ui* (gui::current-view gui::*interactor*)))
(pprint
 (make-menu-script (gui::object-popup-menu-item-list gui::*default-popup-ui* (gui::current-view gui::*interactor*))
		   ".foo"))
|#

;;; ******************  THIS REQUIRES RUNTIME COMPILER  ******************
;;; Might need to change this if we ever want to make a cheap disksave
;;; that doesn't have a compiler.  I would be fairly easy to do this dispatching
;;; using an equal hash-table.

(defun GENERATE-POPUP-MENU-COMMAND-METHOD (panel-class menu-name body)
  (when config::COMPILE-FOR-RUNTIME-ONLY-DISKSAVE (error "no compiler"))

  (let* ((command-name (intern (to-string (gensym "CMD-")) :lisptk))
	 (args `(,(if panel-class `(panel ,panel-class) 'panel)
		 ,(if menu-name `(menu-name (eql ',menu-name)) menu-name)
		 (command (eql ',command-name))
		 window x y))
			
	 (decl `(defmethod popup-menu-command ,args
		 ,body
		 t)))
    (eval decl)
    (compile `(method popup-menu-command (,(or panel-class t)
					  ,(if menu-name `(eql ',menu-name) t)
					  (eql ',command-name) t t t)))
    command-name))

#|
(lx::eval-cache-flush-function 'make-menu)
|#
(defun-cached make-menu (item-list &key
				   (label "Menu Choose")
				   parent handler
				   menu-name panel-class)
  (ignore label)
  (let* ((menu-widget (gensym-widget-path parent)))
    (multiple-value-bind (script submenu-widgets)
	(make-menu-script item-list menu-widget)
      (tcl-script script)
      (install-tk-event-handler menu-widget (or handler 'tk-menu-callback))
      (loop for submenu-widget in submenu-widgets
	    do (install-tk-event-handler submenu-widget (or handler 'tk-menu-callback)))
      (create-tk-binding menu-widget "<Unmap>" (widget)
			 ;; There is some problem here.
			 ;; <Unmap> seems to happen when a menu is buried
			 ;; as opposed to actually removed from the screen.
			 (menu-popdown-callback widget))
      menu-widget)))


;;; This is largely for CME compatibility.  This converts lists of items of the
;;; form (button-label method-name) or (button-label method-name type) to the
;;; form acceptable to MAKE-MENU.
(defun make-menu-item-list (string-symbol-pairs &optional (default-type :command))
  (loop for (button-label method-name . rest) in string-symbol-pairs
	for type = (if rest (car rest) default-type)
	collect (cond ((eq button-label :separator)
		       '("" :separator nil))
		      (method-name `(,button-label ,type ,method-name))
		      (t `(,button-label :no-select nil)))))

;;; Uggh -- too many globals
(defparameter *waiting-for-menu-choice* nil)
(defparameter *menu-choice-type* nil)
(defparameter *menu-choice* nil)
(defvar *popup-menu-state*)

(defmethod tk-menu-callback (panel widget item-name event args)
  (ignore panel widget item-name event)
  (setq args (tcl-list-to-lisp args)
	*popup-menu-state* (cadr args)
	args (car args))
;;  (format t "tk-menu-callback ~a ~a ~a ~a ~a~%" widget item-name event args *popup-menu-state*)
  (when (consp args)
    (case (car args)
      (eval (eval (cadr args))) ; do we want this?
      (command  (apply (cadr args) *popup-menu-state*)))))

(defun menu-popdown-callback (widget)
  ;;(format t "menu-popdown-callback ~a~%" widget)
  ;;(break)
  (tcl-cmd '(set_global qcme_buttonpress_state (list)))
  (setq *waiting-for-menu-choice* nil
	*menu-choice-type* :no-selection
	*menu-choice* (list :popdown widget)))

;;; This is used only in conjunction with menu-choose for 
(defmethod tk-menu-choose-callback (panel widget item-name event args)
  (ignore panel widget item-name event)
  ;;(setq args (tcl-list-to-strings args))
  (setq args (tcl-list-to-lisp args))
  (setq *popup-menu-state* (cadr args))
  (setq args (car args))
  #+never
  (format t "tk-menu-choose-callback ~a ~a ~a ~a ~a~%"
	  widget item-name event args *popup-menu-state*)
  (setq *menu-choice-type* nil
	*menu-choice* (if (consp args)
			  (case (car args) 
			    (quote (cadr args))
			    ;; (eval (eval (cadr args)))
			    (eval (eval (caadr args)))  ;; ???
			    (command (setq *menu-choice-type* :command)
				     (funcall (cadr args))))
			  args)
	*waiting-for-menu-choice* nil))

(defun make-menu-choose-menu (item-list &key label)
  (make-menu item-list :label label :handler 'tk-menu-choose-callback))

(defun tk-pointer-position ()
  (dvector (dfloat (car (tcl-list-to-lisp (tcl-cmd '(winfo pointerx ".")))))
	   (dfloat (car (tcl-list-to-lisp (tcl-cmd '(winfo pointery ".")))))))

(defun tk-pointer-position2 ()
  (list (car (tcl-list-to-lisp (tcl-cmd '(winfo pointerx "."))))
	(car (tcl-list-to-lisp (tcl-cmd '(winfo pointery "."))))))

;;;
;;; Moved this around a bit.  By default, this should work exactly as
;;; it did before.  There's an extra dispatch layer here to allow us
;;; to switch between GUI subsystems.  Tcl/Tk is the default.  Define
;;; new menu-choose-internal methods to hook into other GUI libraries
;;; (GTK for example).  
;;;

#|
(parse-geometry (tcl-cmd `(winfo geometry ,(car *menu-choose-internal-widget-geom*))))
|#
(defvar *menu* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;
;;; Allows timeouts for menu operations - wrap this around menu-choose:
;;;
(defvar *release-menu-after-delay-timer-id*)

(defparameter *release-menu-after-delay-seconds* #+(or agl cocoa) 90 #-(or agl cocoa) 10)
;;(defparameter *release-menu-after-delay-seconds* 60)

(defparameter *release-menu-after-delay-tk-string*
  (format nil "after ~a {qcme_callback nil menu-release-timeout 0}"
	  (floor (* 1000 *release-menu-after-delay-seconds*))))



(defun cancel-release-menu-after-delay ()
  (when (and (boundp '*release-menu-after-delay-timer-id*)
	     *release-menu-after-delay-timer-id*)
    (lisptk::tcl-eval-internal
     (lx::string-append2
      "after cancel "
      *release-menu-after-delay-timer-id*)
     :ignore)
    (setq *release-menu-after-delay-timer-id* nil)))

(defun release-menu-after-delay-callback (&rest  args)
  (declare (ignorable args) (special *waiting-for-menu-choice*))
  ;(format t "release-menu-after-delay-callback ~a ~a~%" *release-menu-after-delay-timer-id* args)
  (cancel-release-menu-after-delay)
  (setq *waiting-for-menu-choice* nil)
  )

(defparameter *inhibit-release-menu-after-delay* nil)

(defun release-menu-after-delay (&optional delay)
  (unless *inhibit-release-menu-after-delay*
    (unless (boundp '*release-menu-after-delay-timer-id*)
      (install-tk-event-handler 'tk::menu-release-timeout 
				'release-menu-after-delay-callback))
    (cancel-release-menu-after-delay)
    (setq *release-menu-after-delay-timer-id*
	  (lisptk::tcl-eval-internal
	   (if delay
	       (format nil "after ~a {qcme_callback nil menu-release-timeout 0 }"
		       (floor (* 1000 delay)))
	       *release-menu-after-delay-tk-string*)
	   nil))
    ;;(format t "release-menu-after-delay ~a~%" *release-menu-after-delay-timer-id*)
    ))


(defmethod menu-choose-internal ((gui (eql :tk)) item-list-or-menu-widget &optional popup-position label)
  (let ((menu-widget
	 (if (consp item-list-or-menu-widget)
	     (make-menu-choose-menu item-list-or-menu-widget :label label)
	     item-list-or-menu-widget)))
    (unless popup-position
      (setq popup-position (confine-to-screen-position menu-widget)))

    (tk-popup (setq *menu* menu-widget) (car popup-position) (cadr popup-position))
    
    (setq *waiting-for-menu-choice* t *menu-choice* nil)
    #-cocoa (release-menu-after-delay 10)
    (ignore-errors
      (catch 'menu-abort
	(handle-events-until #'(lambda () (not *waiting-for-menu-choice*)))
	;;(format t "menu-choose ~a ~a ~%" *menu-choice* *menu-choice-type*);;
	))
    (values *menu-choice*
	    *menu-choice-type*)))

;;; FIXME *default-gui-subsystem* should be in gui package which might not yet exist
;;; This whole file is in the wrong package, but was originally meant to be independent of
;;; the FREEDIUS  UI, and able to standalone as part of the tk system.

(defvar *default-gui-subsystem* :tk)

;;; When an error occurs in a menu-choose operation, the X11 version
;;; of FREEDIUS can get stuck because the mouse is grabbed.  The
;;; introduction of a timeout will cancel the menu if nothing has
;;; happened after about a minute.


(defun menu-choose (item-list-or-menu-widget &key
					     popup-position
					     label (gui *default-gui-subsystem*))
  (menu-choose-internal gui item-list-or-menu-widget popup-position label))


#|


;;; we have problems with package names
(defun cmd-kill-bar ()
  (format t "cmd-kill-bar~%"))

(gethash menu1 *tk-callback-event-handler-hash-table*)
(setq menu1 (make-menu '(("Foo" :quote "Foo")
			 ;;("Garp" :eval (+ 1 2)) ; this doesn't work
			 ("Bar" :command cmd-kill-bar))))

(setq menu1
      (make-menu '(("Foo" :QUOTE "Foo")
		   ;; this next doesn't work because of stupid TCL quoting
		   ;; TCL returns {(+ 1 2)}
		   ("Garp" :EVAL (+ 1 2)) 
		   ("Bar" :COMMAND cme::cmd-kill-bar))
		 :handler 'tk-menu-choose-callback))

(tcl-cmd `(tk_popup ,menu1 10 10))

(menu-choose menu1)
|#


;;; This is a popup menu
(defmethod make-cvv-item (panel parent (class (eql :popup-menu)) name label &rest args
                                &key item-list panel-class &allow-other-keys)
  (declare (ignore label))
  (remf args :button-label) (remf args :item-list)
  (make-cvv-item-clidget
   panel class name
   (make-menu item-list :parent parent :menu-name name :panel-class panel-class )))

;;; no callers
(defun make-popup-menu (item-list &key parent handler menu-name)
  (let ((menu (make-menu item-list :parent parent :handler handler :menu-name menu-name)))
    ;;(tk-wm "withdraw" menu)
    menu))

;;; ********************  THIS REQUIRES CVV SYSTEM   ********************

#+never ; unused
(defmethod bind-popup-menu-to-window ((popup-menu string) window
				      &optional (mouse-event '<alpha-right>))
  (destructuring-bind (button state) mouse-event
    (tcl-script `((putprop ,(widget window)
		   ,(format nil "popup_menu(~a,~a)" button state)
		   ,popup-menu)))))



#+never ; unused
(progn

;;; class for supplying handlers to popup menus 

(defclass popup-menu-pseudo-panel (widget-panel) ())

;; should this move to widget-panel?
(defmethod add-cvv-item ((panel popup-menu-pseudo-panel) item)
  (setf (widget-clidget (widget item)) item)
  (setf (item-alist panel)
	(append (item-alist panel) (list (list (name item) item)))))

(defmethod tk-callback :around
	   ((panel popup-menu-pseudo-panel) widget item-name event args)
  (case event
    (MENUBUTTON
     (let* ((args (tcl-list-to-lisp args))
	    (popup-menu-state (cadr args))
	    (button-action (car args)))
       (destructuring-bind (window-widget x y) popup-menu-state
	 ;; item-name is the name of the menu
	 (if (consp button-action)
	     (unless (popup-menu-command panel
					 (caddr button-action) ;;item-name
					 (cadr button-action)
					 (widget-clidget window-widget)
					 x y)
	       (call-next-method))
	     (call-next-method)))))
    (otherwise (call-next-method))))

(defmethod popup-menu-command ((panel popup-menu-pseudo-panel)
			       menu-name command window x y)
  ;;(break)
  (cond ((fboundp command)
	 (funcall command panel window x y menu-name)
	 t)
	(t (format t ";;POPUP-MENU-COMMAND NOT HANDLED: ~a ~s ~a ~%"
		   menu-name command panel)
	   t)))


) ; end progn unused 
