#|
LHQ:  Thu May  3 2007

This file contains slightly different varients of MAKE-CME-CONTROL-PANEL
defined in $FREEDIUS/lisp/cme-compat/cme-control-panel.lisp
reimplemented using the rudimentary ltk-tile and ltk-cvv tools.

This represents work in progress towards a complete implementation of make-cvv-panel
using ltk.

|#

#|
(progn (st:load-system :ltk :initialize t)
       (maybe-compile-file-load "$FREEDIUS/lisp/ltk/examples/cme-control-panel.lisp")
)

;;; The version in $FREEDIUS/lisp/cme-compat/cme-control-panel.lisp using TK::TCL-SCRIPT
(gui::make-cme-control-panel)

|#



(in-package :gui)

;;;(make-cme-control-panel-xxx)
(defun make-cme-control-panel-xxx (&key (width 1500) (height 100) (panel-class 'cme-control-panel)
				   (tk-class *control-panel-tk-class*))
  (let* ((panel-proto (get-class-prototype panel-class))
	 (toplevel 
	  (lttk::with-lttk-widgets
	    (flet ((make-doc-widget (name)
		     (entry name :style "Flat.TEntry" :font "courier 10 normal"
			    :pack '(:side top :fill x :anchor nw ))))
	      (with-parent (toplevel :title (format nil "FREEDIUS Control Panel XXX (pid=~d)" (getpid)))
		(with-parent (frame 'mf :pack '(:side top :fill both :anchor nw :expand 1))
		  (with-parent (frame 'f2 :pack '(:side top :anchor nw :fill x :expand 1))
		    (make-doc-widget 'doc)
		    (make-doc-widget 'doc2)
		    (with-parent (frame 'mb :pack '(:side left :anchor nw))
		      (loop for (name text . items) in (menubar-menu-items panel-proto)
			    do (with-parent (pulldownmenu-button name :text text :underline 0 
								 ;:style  "Toolbutton" 
								 :pack '(:side left :padx 4))
				 (loop for (name text) in items
				       do (menubutton name :text text)))))
		    (with-parent (frame 'pc :pack '(:side right :anchor ne))
		      (loop for (name text . keyvals) 
			    in (panel-control-items panel-proto)
			    do (check-button name :text text :style "Toolbutton" :pack '(:side left) 
					:tooltip (getf keyvals :documentation))))
		    ))))))
	 (panel (make-widget-panel toplevel 'cme-control-panel-callback :panel-class panel-class)))
    (create-tk-binding (ltk:widget-path toplevel) "<Configure>" (widget width height)
      (docline-configure-callback widget width height))
    (setq *cme-control-panel* panel)
    panel))


;;(make-cme-control-panel-xxx2)
(defun make-cme-control-panel-xxx2 (&key (width 1500) (height 100) (panel-class 'cme-control-panel)
				   (tk-class *control-panel-tk-class*))
  (let* ((panel-proto (get-class-prototype panel-class))
	 (toplevel 
	  (lttk::with-lttk-widgets
	    (flet ((make-doc-widget (name)
		     (entry name :style "Flat.TEntry" :font "courier 10 normal"
			    :pack '(:side top :fill x :anchor nw ))))
	      (with-parent (toplevel :title (format nil "FREEDIUS Control Panel XXX (pid=~d)" (getpid)))
		(with-parent (frame 'mf :pack '(:side top :fill both :anchor nw :expand 1))
		  (with-parent (frame 'f2 :pack '(:side top :anchor nw :fill x :expand 1))
		    (make-doc-widget 'doc)
		    (make-doc-widget 'doc2)
		    (with-parent (frame 'mb :pack '(:side left :anchor nw))
		      (loop for (name text . items) in (menubar-menu-items panel-proto)
			    do (lttkw::make-pulldown-menu items :name name :text text :underline 0
				     :pack '(:side left :padx 4))))
		    (with-parent (frame 'pc :pack '(:side right :anchor ne))
		      (loop for (name text . keyvals) 
			    in (panel-control-items panel-proto)
			    do (check-button name :text text :style "Toolbutton" :pack '(:side left) 
				     :tooltip (getf keyvals :documentation))))
		    ))))))
	 (panel (make-widget-panel toplevel 'cme-control-panel-callback :panel-class panel-class)))
    (create-tk-binding (ltk:widget-path toplevel) "<Configure>" (widget width height)
      (docline-configure-callback widget width height))
    (setq *cme-control-panel* panel)
    panel))

(defmethod menubar-menu-items2 ((panel cme-control-panel))
  `((file "File" ,(file-menu-item-list panel) :underline 0)
    (create-object "Objects" ,(object-creation-menu-item-list panel) :underline 3)
    (create-view "Views" ,(view-creation-menu-item-list panel) :width 20 )
    (panels "Panels" ,(panel-menu-item-list panel))
    ))

(defmethod menubar-menu-items2 ((panel cme-control-panel))
  `((file "File" ,(file-menu-item-list panel) :underline 0)
    (create-object "Objects" ,(object-creation-menu-item-list panel) :underline 0)
    (create-view "Views" ,(view-creation-menu-item-list panel) :underline 0 )
    (panels "Panels" ,(panel-menu-item-list panel) :underline 0)
    ))

;;(make-cme-control-panel-xxx3)
(defun make-cme-control-panel-xxx3 (&key (width 1500) (height 100) (panel-class 'cme-control-panel)
				   (tk-class *control-panel-tk-class*))
  (let* ((panel-proto (get-class-prototype panel-class))
	 (toplevel 
	  (lttk::with-lttk-widgets
	    (flet ((make-doc-widget (name)
		     (entry name :style "Flat.TEntry" :font "courier 10 normal"
			    :pack '(:side top :fill x :anchor nw ))))
	      (with-parent (toplevel :title (format nil "FREEDIUS Control Panel XXX (pid=~d)" (getpid)))
		(with-parent (frame 'mf :pack '(:side top :fill both :anchor nw :expand 1))
		  (with-parent (frame 'f2 :pack '(:side top :anchor nw :fill x :expand 1))
		    (make-doc-widget 'doc)
		    (make-doc-widget 'doc2)
		    (with-parent (frame 'mb :pack '(:side left :anchor nw))
		      (loop with packing = '(:side left :padx 8)
			    for (name text items . options) in (menubar-menu-items2 panel-proto)
			    do (apply 'lttkw::make-pulldown-menu items :name name :text text
				      :pack packing options)))
		    (with-parent (frame 'pc :pack '(:side right :anchor ne))
		      (loop for (name text . keyvals) 
			    in (panel-control-items panel-proto)
			    do (check-button name :text text :style "Toolbutton" :pack '(:side left) 
				     :tooltip (getf keyvals :documentation))))
		    ))))))
	 (panel (make-widget-panel toplevel 'cme-control-panel-callback :panel-class panel-class)))
    (create-tk-binding (ltk:widget-path toplevel) "<Configure>" (widget width height)
      (docline-configure-callback widget width height))
    (setq *cme-control-panel* panel)
    panel))

(defmethod menubar-menu-items4 ((panel cme-control-panel))
  `((,(file-menu-item-list panel) :name file :text "File" :underline 0)
    (,(object-creation-menu-item-list panel) :name create-object :text "Objects" :underline 0)
    (,(view-creation-menu-item-list panel) :name create-view :text "Views" :underline 0 )
    (,(panel-menu-item-list panel) :name panels :text "Panels" :underline 0)
    ))

(defmethod menubar-menu-items4 ((panel cme-control-panel))
  `((,(file-menu-item-list panel) :text "File" :underline 0)
    (,(object-creation-menu-item-list panel)  :text "Objects" :underline 0)
    (,(view-creation-menu-item-list panel) :text "Views" :underline 0 )
    (,(panel-menu-item-list panel) :text "Panels" :underline 0)
    ))


;;(make-cme-control-panel-xxx4)
(defun make-cme-control-panel-xxx4 (&key (width 500) (height 100) (panel-class 'cme-control-panel)
				    (tk-class *control-panel-tk-class*))
  (let* ((panel-proto (get-class-prototype panel-class))
	 (toplevel 
	  (lttk::with-lttk-widgets
	    (flet ((make-doc-widget (name)
		     (entry name :style "Flat.TEntry" :font "courier 10 normal"
			    :pack '(:side top  :fill x :anchor nw ))))
	      (with-parent (toplevel :title (format nil "FREEDIUS Control Panel XXX (pid=~d)" (getpid)))
		(with-parent (frame 'f2 :width width :pack '(:side top :anchor nw :fill x :expand 1))
		  (make-doc-widget 'doc)
		  (make-doc-widget 'doc2)
		  (with-parent (frame 'mb :pack '(:side left :anchor nw))
		    (loop with defaults = '(:takefocus 1 :pack (:side left :padx 8))
			  for (items . options) in (menubar-menu-items4 panel-proto)
			  do (apply 'lttkw::make-pulldown-menu items (append options defaults))))
		  (with-parent (frame 'pc :pack '(:side right :anchor ne))
		    (loop for (name text . keyvals) 
			  in (panel-control-items panel-proto)
			  do (check-button name :text text :style "Toolbutton" :pack '(:side left) 
				   :tooltip (getf keyvals :documentation))))
		  )))))
	 (panel (make-widget-panel toplevel 'cme-control-panel-callback :panel-class panel-class)))
    (create-tk-binding (ltk:widget-path toplevel) "<Configure>" (widget width height)
      (docline-configure-callback widget width height))
    (setq *cme-control-panel* panel)
    panel))

;;(ltk::configure (widget-clidget (widget *cme-control-panel*))
;;(ltk::format-wish "~a configure -width" (widget *cme-control-panel*))
;;(ltk::format-wish "~a configure -width 500" (widget *cme-control-panel*))
#+never
(defun make-cme-control-panel-xxx4 (&key (width 1500) (height 100) (panel-class 'cme-control-panel)
				   (tk-class *control-panel-tk-class*))
  (let* ((panel-proto (get-class-prototype panel-class))
	 (toplevel 
	  (lttk::with-lttk-widgets
	    (flet ((make-doc-widget (name)
		     (entry name :style "Flat.TEntry" :font "courier 10 normal"
			    :pack '(:side top :fill x :anchor nw ))))
	      (with-parent (toplevel :title (format nil "FREEDIUS Control Panel XXX (pid=~d)" (getpid)))
		(with-parent (frame 'mf :pack '(:side top :fill both :anchor nw :expand 1))
		  (with-parent (frame 'f2 :pack '(:side top :anchor nw :fill x :expand 1))
		    (make-doc-widget 'doc)
		    (make-doc-widget 'doc2)
		    (with-parent (frame 'mb :pack '(:side left :anchor nw))
		      (loop for (name text . items) in (menubar-menu-items panel-proto)
			    do (with-parent (pulldownmenu-button name :text text :underline 0 
					;:style  "Toolbutton" 
								 :pack '(:side left :padx 4))
				 (loop for (name text) in items
				       do (menubutton name :text text)))))
		    (with-parent (frame 'pc :pack '(:side right :anchor ne))
		      (lttk::make-radiobuttons '((none "None") (foo "Foo") (bar "Bar") (baz "Baz")))
		      (loop for (name text . keyvals) 
			    in (panel-control-items panel-proto)
			    do (check-button name :text text :style "Toolbutton" :pack '(:side left) 
				     :tooltip (getf keyvals :documentation))))
		    ))))))
	 (panel (make-widget-panel toplevel 'cme-control-panel-callback :panel-class panel-class)))
    (create-tk-binding (ltk:widget-path toplevel) "<Configure>" (widget width height)
      (docline-configure-callback widget width height))
    (setq *cme-control-panel* panel)
    panel))
  
#+never
(defun make-cme-control-panel-xxx5 (&key (width 1500) (height 100) (panel-class 'cme-control-panel)
				   (tk-class *control-panel-tk-class*))
  (let* ((panel-proto (get-class-prototype panel-class))
	 (toplevel 
	  (lttk::with-lttk-widgets
	    (flet ((make-doc-widget (name)
		     (entry name :style "Flat.TEntry" :font "courier 10 normal"
			    :pack '(:side top :fill x :anchor nw ))))
	      (with-parent (toplevel :title (format nil "FREEDIUS Control Panel XXX (pid=~d)" (getpid)))
		(with-parent (frame 'mf :pack '(:side top :fill both :anchor nw :expand 1))
		  (with-parent (frame 'f2 :pack '(:side top :anchor nw :fill x :expand 1))
		    (make-doc-widget 'doc)
		    (make-doc-widget 'doc2)
		    (with-parent (frame 'mb :pack '(:side left :anchor nw))
		      (loop for (name text . items) in (menubar-menu-items panel-proto)
			    do (with-parent (pulldownmenu-button name :text text :underline 0 
					;:style  "Toolbutton" 
								 :pack '(:side left :padx 4))
				 (loop for (name text) in items
				       do (menubutton name :text text)))))
		    (with-parent (frame 'pc :pack '(:side right :anchor nw))
		      (lttk::make-radiobuttons '((none "None") (foo "Foo") (bar "Bar") (baz "Baz")))
		      (with-parent (frame 'mbm :padding '(8 0 0 0) :pack '(:side left :anchor nw))
			(loop for (name text . keyvals) 
			      in (panel-control-items panel-proto)
			      do (check-button name :text text :style "Toolbutton" :pack '(:side left) 
				       :tooltip (getf keyvals :documentation)))))
		    ))))))
	 (panel (make-widget-panel toplevel 'cme-control-panel-callback :panel-class panel-class)))
    (create-tk-binding (ltk:widget-path toplevel) "<Configure>" (widget width height)
      (docline-configure-callback widget width height))
    (setq *cme-control-panel* panel)
    panel))
