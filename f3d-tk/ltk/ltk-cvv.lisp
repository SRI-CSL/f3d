(in-package :ltk)

;; basic class for all widgets --- add lx::property-list-mixin
(defclass widget (tkobject lx::property-list-mixin)
  ((master :accessor master :initarg :master :initform *current-master*) ;; parent widget or nil
   (widget-path :initarg :path :initform nil :accessor %widget-path)         ;; pathname to refer to the widget
   (init-command :accessor init-command :initform nil :initarg :init-command)
   )
  (:documentation "Base class for all widget types"))


(defmethod widget-named (panel name)
  (tk:widget-clidget (tk::widget-named panel name)))

(defun concat-symbol-name (symbol string)
  (intern (format nil "~a~a" symbol string) (symbol-package symbol)))



(defparameter *default-toplevel-widget-close-window-action* :destroy)

(defwidget toplevel (widget) 
  ((protocol-destroy :accessor protocol-destroy :initarg :on-close :initform nil)
   (title :accessor title :initform nil :initarg :title)
   (panel :initform nil :initarg :panel :accessor tk::widget-panel)
   ) 
  "toplevel"
  (when (title widget)
    (wm-title widget (title widget)))
  (unless (protocol-destroy widget)
    (format-wish "wm protocol ~a WM_DELETE_WINDOW {wm withdraw ~a}" (widget-path widget) (widget-path widget))))

(defmethod tk::widget-panel ((widget widget))
  (tk::widget-panel (master widget)))


(defmethod tk::panel ((w widget))
  (tk::widget-panel w))

(defmethod tk::widget ((w widget))
  (widget-path w))

;;; Problems calling widget-path here.
#+never
(defmethod initialize-instance :after ((w widget) &key)
  (push w *all-widgets*)
  (unless (name w)			; generate name if not given 
    (setf (name w) (create-name)))
  (setf (tk::widget-clidget (widget-path w)) w)
  )

(defvar *current-panel* nil)


;; around - initializer
(defmethod initialize-instance :around ((w widget) &key name pack place grid tooltip)
  (call-next-method)
  (let ((widget-path (widget-path w)))
    (setf (tk::widget-clidget widget-path) w)
    ;; pack widget if parameter has been supplied
    (when pack
      (apply #'pack w pack))
    (when place
      (apply #'place w place))
    (when grid
      (apply #'grid w grid))
    (when tooltip  
      (format-wish "add_widget_doc ~a {~a}" widget-path tooltip))
    (when (init-command w)
      (format-wish "set_bindingtags ~a" widget-path)
      )
    (when (and *current-panel* name)
      (pushnew (list name widget-path) (tk::item-alist *current-panel*) :key #'car))
    ))
  
(defun make-toplevel (master &rest initargs &key title 
		      (close-window-action *default-toplevel-widget-close-window-action*))
  (declare (ignorable title close-window-action))
  (let ((protocol-destroy (case CLOSE-WINDOW-ACTION 
			    (:destroy t)
			    (:unmap nil)
			    (otherwise 
			     (warn "initialize-instance :after toplevel illegal CLOSE-WINDOW-ACTION ~a~%"
				   CLOSE-WINDOW-ACTION)))))
    (apply #'make-instance 'toplevel :master master :on-close protocol-destroy initargs)))



(in-package :tk)


;;; Extended version of ltk-callback allowing commands to be handled by the 
;;; panel associated with the top-level widget (uppermost parent).

(defun ltk::ltk-callback (callback-id &rest args)
  (setq ltk::*ltk-callback-args* (list* callback-id args))
  (let ((entry (gethash callback-id (ltk::wish-callbacks ltk::*wish*))))
    (typecase entry
      (function (apply entry args))
      (cons (destructuring-bind (widget-path event . args) entry
	      (ltk::ltk-panel-callback widget-path event args)))
      (otherwise (format t "ltk-callback: no callback info for ~a~%" callback-id)))))

(defun ltk::ltk-callback (callback-id &rest args)
  (setq ltk::*ltk-callback-args* (list* callback-id args))
  (let ((entry (gethash callback-id (ltk::wish-callbacks ltk::*wish*))))
    (cond ((or (functionp entry)
	       (and (symbolp entry) (fboundp entry)))
	   (apply entry args))
	  ((consp entry)
	   (destructuring-bind (widget-path event . args) entry
	      (ltk::ltk-panel-callback widget-path event args)))
	  (t (format t "ltk-callback: no callback info for ~a~%" callback-id)
	     "0"))))

(in-package :ltk)

(defun ltk-callback (callback-id &rest args )
  (setq *ltk-callback-args* (list* callback-id args))
  (if (equal callback-id "event")
      (destructuring-bind (evt callback-name &rest args)
	  (read-from-string (car args))
	(let ((fun (gethash callback-name (wish-callbacks *wish*))))
	  (setq *foo2* (list callback-name fun args))
	  (if fun
	      ;;(apply fun (loop for arg in args collect (if (stringp arg) (read-from-string arg) arg)))
	      (funcall fun (construct-tk-event args))
	      (format t "ltk-callback: no callback function for ~a ~a~%" callback-name args))))

      (let ((entry (gethash callback-id (wish-callbacks *wish*))))
	(cond ((or (functionp entry)
		   (and (symbolp entry) (fboundp entry)))
	       (apply entry args))
	      ((consp entry)
	       (destructuring-bind (widget-path event . args) entry
		 (ltk-panel-callback widget-path event args)))
	      (t (format t "ltk-callback: no callback info for ~a ~a~%" callback-id args)
		 "0")))
					; (break)
      ))

(in-package :tk)
;;(gethash "rcb6" (ltk::wish-callbacks ltk::*wish*))
;;(funcall (gethash "rcb6" (ltk::wish-callbacks ltk::*wish*)))

(defun ltk::ltk-panel-callback (widget-path &optional event &rest args)
  (let* ((widget (widget-clidget widget-path))
	 (panel (and widget (widget-panel widget))))
    (if panel
	(tk-callback panel
		     widget
		     widget-path
		     event
		     args)
	(format t "ltk-panel-callback: no panel for ~a~%" widget-path))))

(defun ltk::ltk-panel-callback (widget-path &optional event &rest args)
  (let* ((widget (widget-clidget widget-path))
	 (panel (and widget (widget-panel widget))))
    (if panel
	(tk-callback panel
		     widget-path
		     (tk::widget-symbol widget-path panel)
		     event
		     args)
	(format t "ltk-panel-callback: no panel for ~a~%" widget-path))))

(defun ltk::ltk-panel-callback (widget-path &optional event &rest args)
  (let* ((panel (widget-panel widget-path)))
    (if (and panel (typep panel 'tk::widget-panel))
	(tk-callback panel
		     widget-path
		     (tk::widget-symbol widget-path panel)
		     event
		     args)
	(format t "ltk-panel-callback: no panel for ~a~%" widget-path))))

(in-package :ltk)

#+never
(defmethod initialize-instance :after ((m menubutton) &key command underline accelerator)
  (when (master m)
    (format-wish "~A add command -label {~A}  -command {callback ~A}~@[ -underline ~a ~]~@[ -accelerator {~a} ~]"
		 (menu-widget-path (master m)) (text m) (callback-name m) underline accelerator))
  (add-callback m (or command `(,(widget-path m) :button))))

#+never
(defmethod initialize-instance :after ((m menubutton) &key command underline accelerator)
  (let ((callback-name (callback-name m))) ; lexical closure
    (when (master m)
      (format-wish "~A add command -label {~A}  -command {callback ~A}~@[ -underline ~a ~]~@[ -accelerator {~a} ~]"
		   (menu-widget-path (master m)) (text m) (callback-name m) underline accelerator))
    (add-callback callback-name (or command 
				    #'(lambda () (ltk-panel-callback callback-name :buttonpress))))))

(defmethod default-callback-command ((w widget))
  `(,(make-widget-path w) tk::button))

;;; extend add-callback to work with panels
(defmethod add-callback ((w widget) fun)
  (unless fun
    (setq fun (default-callback-command w)))
  (when fun
    (add-callback (callback-name w) fun)))

;;; New version using qcme_callback when fun is NIL.
#+unfinished
(defmethod add-callback ((w widget) fun)  
  (if fun
      (add-callback (callback-name w) fun)
      (default-callback-command w)))

(import '(lx::get-prop))

;(widget-children-paths (tk::widget-clidget ".w6.topfrm"))
(defun widget-children-paths (w)
  (ltk::format-wish-and-read-strings "non_slaved_children ~a" (widget-path w)))

(defun set-grid-2-column (parent rows-widget-names)
  (flet ((p (n) (format nil "~a.~(~a~)" parent n)))
    (loop with minsize = 0
	  for row from 0
	  for entry in rows-widget-names ; entries for form (w0 wt) or (w0 w1 wt)
	  when (= (length entry) 3)
	    do (format-wish "grid ~a -row ~a -column 0 -sticky e" (p (car entry)) row)
	       (format-wish "grid ~a -row ~a -column 1 -sticky news" (p (cadr entry)) row)
	       (format-wish "grid rowconfigure [winfo parent ~a] ~a -weight ~a -minsize ~a" 
			    (p (car entry)) row (caddr entry) minsize)
	  else do (format-wish "grid ~a -row ~a -column 0 -columnspan 2 -sticky news" (p (car entry)) row)
		  (format-wish "grid rowconfigure [winfo parent ~a] ~a -weight ~a -minsize ~a" 
			       (p (car entry)) row (cadr entry) minsize)
	  )
    (format-wish "grid columnconfigure [winfo parent ~a] 1 -weight 1" (p (caar rows-widget-names)))))

(defun grid-unmap-widget (w)
  (setf (get-prop w :grid-config) 
	(ltk::format-wish-and-read-strings "grid info ~a" (widget-path w)))
  (format-wish "grid remove ~a" (widget-path w)))

(defun grid-map-widget (w)
  (let ((widget-path (widget-path w)))
    (format-wish "grid ~a" widget-path)
    (format-wish "grid configure ~a ~{~a ~}" widget-path (get-prop w :grid-config))
    (format-wish "grid rowconfigure [winfo parent ~a] [grid_info ~a -row] -weight 0 -minsize 0" 
		 widget-path widget-path)
    ))
(defun grid-map-widget (w)
  (let ((widget-path (widget-path w)))
    (format-wish "grid ~a" widget-path)
    ))

(defun grid-unmap-widget (w)
  (let ((widget-path (widget-path w)))
    (setf (get-prop w :grid-config) 
	  (ltk::format-wish-and-read-strings "grid info ~a" widget-path))
    (format-wish "grid rowconfigure [winfo parent ~a] [grid_info ~a -row] -weight 0 -minsize 0" 
		 widget-path widget-path)
    (format-wish "grid remove ~a" widget-path)
    ))

#+never
(defun grid-map-widget (w)
  (format-wish "grid ~a" (widget-path w)))


(defun pack-unmap-widget (w)
  (setf (get-prop w :pack-config) 
	(ltk::format-wish-and-read-strings "pack info ~a" (widget-path w)))
  (format-wish "pack forget ~a" (widget-path w)))

(defun pack-map-widget (w)
  (format-wish "pack configure ~a ~{~a ~}" (widget-path w) (get-prop w :pack-config)))

#|
(defun get-widget-named (panel name) (tk::widget-clidget (tk::widget-named panel name)))

(ltk::format-wish-and-read-strings "pack info ~a" 
				   (tk::widget-named video::*play-video-panel* 'video::params))
(pack-unmap-widget (get-widget-named video::*play-video-panel* 'video::params))
(pack-map-widget (get-widget-named video::*play-video-panel* 'video::params))
(grid-unmap-widget (get-widget-named video::*play-video-panel* 'video::params))
(grid-map-widget (get-widget-named video::*play-video-panel* 'video::params))
|#

#+unfinished
(progn

(defun add-group-control (group-widget slave-widget-names)
  (setf (lx:get-prop group-widget :group-widgets) slave-widget-names)
  )

(defun grid-rowconfigure-get (w option)
  (ltk::format-wish-and-read-strings "grid rowconfigure ~a -~(~a~)" (widget-path w) option))

;(grid-rowconfigure-get (tk::widget-clidget ".w42.topfrm") :size)

;(ltk::format-wish-and-read-string "grid size .w42.topfrm")
;(ltk::format-wish-and-read-strings "grid slaves .w42.topfrm")
;(ltk::format-wish-and-read-string "grid slaves .w42.topfrm -row 5")
;(ltk::format-wish-and-read-string "grid info .w42.topfrm.glwin")
;(grid-rowconfigure (tk::widget-clidget ".w42.topfrm") 5 :weight)

#+incomplete
(defun unmap-group (group-widget)
  (loop with gp = (widget-path w)
	with group-name = (name group-widget)
	for wp in (ltk::format-wish-and-read-strings "grid slaves ~a" gp)
	for w = (tk::widget-clidget wp)
	when (and w (eq (get-prop w :group) group-name))
	  do 
	for label = (get-prop w :label-widget)
	for row = (get-prop w :grid-row)
	for wt = (ltk::format-wish-and-read "grid rowconfigure ~a 5 -weight" wp)
	unless (= wt 0) 
	  do (setf (get-prop w :weight) wt)
	when row 
	  do (grid-rowconfigure w row :weight 0)
	do (grid-unmap-widget w)
	when label
	  do (grid-unmap-widget label)))

(defun unmap-group (group-widget)
  (loop for w in (get-prop group-widget :group-widgets)
	for label = (get-prop w :label-widget)
	for row = (get-prop w :grid-row)
	for wt = (ltk::format-wish-and-read "grid rowconfigure ~a 5 -weight" (widget-path w))
	unless (= wt 0) 
	  do (setf (get-prop w :weight) wt)
	when row 
	  do (grid-rowconfigure w row :weight 0)
	do (grid-unmap-widget w)
	when label
	  do (grid-unmap-widget label)))

(defun map-group (group-widget)
  (loop for w in (get-prop group-widget :group-widgets)
	for label = (get-prop w :label-widget)
	for row = (get-prop w :grid-row)
	for wt = (get-prop w :weight)
	do (grid-map-widget w)
	when row 
	  do (grid-rowconfigure w row :weight wt)
	when label
	  do (grid-map-widget label)))
	
(defun set-widget-group-info ()

  )



) ; end progn


#+unfinished
(progn 


;;; Mappings from CME-6 CVV widget classes to TK widget classes
(defparameter *cvv-to-ltk-widget-class-alist*
  '((:string entry)
    (:multi-line-string entry)
    (:integer entry)
    (:double entry)
    (:float entry)
    (:button button)
    (:toggle-button check-button)
    (:check-button check-button)
    (:label label)
    (:group-button qgroupbutton)
    (:group qgroupbutton)
    (:menu-button qpulldownmenu)
    (:button-list qbuttonlist)
    (:assoc qradiogroup)
    (:slider qslider)
    ;;(:yes-or-no )
    (:exclusive-list qlistbox)
    (:abbrev-assoc qoptionmenu)
    ;;(:multiple-choose-list qlistbox )
    (:form :cvv-2-column-frame)
    (:1-column-frame :cvv-1-column-frame)
    (:2-column-frame :cvv-2-column-frame)
    (:frame qframe)
    ))

(defun handle-cvv-widget-type-aliases (type)
  (or (cadr (assoc type *cvv-to-ltk-widget-class-alist*))
      type))


(defmethod make-cvv-form-item-list (panel parent item-list)
  (loop for (slot-symbol label-string type0 . rest) in item-list
	for type = (tk::handle-cvv-widget-type-aliases type0)
	do (apply 'make-cvv-item panel parent type slot-symbol label-string rest)))


(defmethod make-cvv-item (panel parent (type (eql :cvv-2-column-frame)) name label &rest args
				&key item-list (items item-list) &allow-other-keys)
  (ignore label)
  (remf args :item-list) (remf args :items)
  (let* ((frame (apply #'make-instance 'frame :master parent :name name args))
	 (frame-path (widget-path frame)))
    (make-cvv-form-item-list panel frame-path items)
    (format-wish "qgrid_2_column ~a" frame-path)	     
    (format-wish "initialize_group_control_state ~a" frame-path)
    frame))



) ; end progn


;(fmakunbound 'make-glframe)
(defun make-glframe (name &key (master *current-master*))
  (let* ((frame (make-instance 'frame :name name :master master 
			       :background "{}" 
			       :pack '(:expand true :fill both)))
	 (widget-path (widget-path frame)))
    (format-wish "bindtags ~a [concat [bindtags ~a] image_pane]" widget-path widget-path)
    frame))

(in-package :gui)

#+never
(defmethod ltk::make-glframe (panel parent name label &rest args
			       &key width height &allow-other-keys)
  (mv-bind (script wframe-name tkglwin-name)
      (apply 'tk::glwindow-script parent (string-downcase (symbol-name name)) args)
    (tcl-eval 'set_global 'default_parent parent)
    (tk::tcl-script script)
    (let* ((clidget (tk::make-cvv-item-clidget panel :glwindow name wframe-name))
	   (pane-class *default-pane-class*)
	   (window (make-window tkglwin-name :class pane-class)))
      (push (cons name window) (frame-pane-alist panel))
      (setf (panel window) panel)
      clidget))) 


;(fmakunbound 'ltk::make-glframe)
;;;
;;; When sb-thread is enabled, this fails if it is invoked from the
;;; repl.  Specifically, it cannot find the X screen and window.  If
;;; we push the invoking forms on tk::*forced-read-forms*, we can get
;;; some success.
;;;
(defun ltk::make-glframe (parent name &rest args &key width height pack grid (borderwidth  1)
			  &allow-other-keys)
  ;;(setq name (format nil "~(~a~)" name))
  (gui::create-tkglwin-bindings "image_pane")
  (let* ((wframe-name name)
	 (winid name)
	 (wframe (make-instance 'ltk::frame :master parent :name wframe-name 
				:highlightthickness borderwidth
				:highlightbackground 'black
				:relief 'flat
				:pack pack :grid grid))
	 (glframe (make-instance 'ltk::frame :name (ltk::concat-symbol-name winid "_GL")
				 :master wframe
				 :background "{}" 
				 ;;:container 1
				 :width width :height height
				 :pack '(:expand true :fill both)))
	 ;;(wframe-path (ltk::widget-path wframe))
	 (glframe-path (ltk::widget-path glframe))
	 (pane-class *default-pane-class*))
    (let ((window (make-window glframe-path :class pane-class)))
      ;;    (tk::tk-flush)
      ;;(bindtags ,widget-name (concat (bindtags ,widget-name) ,tk-binding-name))
      (ltk::format-wish "bindtags ~a [concat [bindtags ~a] image_pane]" glframe-path glframe-path)
      (values window wframe))))


(in-package :tk)

(defmethod cvv-item-value ((item ltk::widget) &optional ignore)
  (declare (ignore ignore))
  (cvv-item-value (ltk::widget-path item)))

(defmethod (setf cvv-item-value) (value (item ltk::widget) &optional ignore)
  (declare (ignore ignore))
  (setf (cvv-item-value (ltk::widget-path item)) value))


(in-package :lttkw)


;;(make-pulldown-menu)

(defun make-pulldown-menu (item-list &rest options &key (pack '(:side left :padx 4)) &allow-other-keys)
  (with-parent (apply #'make-instance 'pulldownmenu-button :pack pack options)
    (loop for (name text . boptions) in item-list
	  for button-type = (or (getf boptions :button-type) 'menubutton)
	  do (remf boptions :button-type)
	     (apply button-type name :text text boptions))))
	       
(defparameter *make-pulldown-menu-default-packing* '(:side left :padx 4))

(defun make-pulldown-menu (item-list &rest options &key (pack *make-pulldown-menu-default-packing*)
			   &allow-other-keys)
  (with-parent (apply #'make-instance 'pulldownmenu-button :pack pack options)
    (loop for (name text . boptions) in item-list
	  for button-type = (or (getf boptions :button-type) 'menubutton)
	  do (remf boptions :button-type)
	     (apply button-type name :text text boptions)))) 
