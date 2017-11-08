(in-package :lisptk)



;;; item-list for house-object from CME-6

;;; This doesn't work here
;;;(setq *panel-controls-buttonargs*
;;;      ;; See $FREEDIUS/lisp/tk/Qcme.app-defaults for these specs.
;;;      (when nil '(-padx 3 -pady 1 -borderwidth 0 -highlightthickness 0))
;;;
;;;      *panel-controls*
;;;      `(panel_controls nil :button-list
;;;        :items (("Quit" quit-panel)
;;;                ("Update" update-panel)
;;;                ("Lock" lock-panel :button-type :toggle-button
;;;                 :documentation "Lock panel to current object."
;;;                 . ,*panel-controls-buttonargs*))
;;;        . ,(and *panel-controls-buttonargs*
;;;                `(:buttonargs ,*panel-controls-buttonargs*)))
;;;
;;;
;;;      *house-object-cvv-item-list*
;;;      (cons *panel-controls*
;;;            `((NAME "Name:" :STRING :group :preamble)
;;;              (EDIT-GRAPHICS-STYLE NIL :BUTTON :BUTTON-LABEL "Edit Graphics Style"
;;;               :WIDGET-ARGS ("alignment" 0) :group :preamble)
;;;              (annotation "ANNOTATION:" :group :initial-value nil
;;;               :group-elements (DESCRIPTION))
;;;              #+never
;;;              (DESCRIPTION "Description:" :MULTI-LINE-STRING :group annotation
;;;               :NLINES 3 :width 30 :scroll (list right) :wrap none
;;;               :weight 1)
;;;
;;;              ;;(DESCRIPTION-label "Description:" qlabel)
;;;              (DESCRIPTION nil :MULTI-LINE-STRING :group annotation
;;;               :NLINES 3 :width 30 :scroll (list right) :wrap none
;;;               :weight 1)
;;;
;;;              (hierarchy "HIERARCHY:" :group :initial-value nil
;;;               :group-elements (CHANGE-SUPERIOR FEATURE-SETS))
;;;        
;;;              (CHANGE-SUPERIOR "Superior:"
;;;               :EXCLUSIVE-LIST
;;;               :VISIBLE-ITEM-COUNT
;;;               3
;;;               :GROUP
;;;               HIERARCHY)
;;;              (FEATURE-SETS "Feature Sets:"
;;;               :MULTIPLE-CHOOSE-LIST
;;;               :VISIBLE-ITEM-COUNT
;;;               5 :weight 3
;;;               :GROUP
;;;               HIERARCHY)
;;;              (POSITION "LOCATION:" :group
;;;               :group-elements (WORLD COORD-MODE UNITS X Y Z LAST-SELECTED-HANDLE))
;;;              (WORLD NIL :LABEL :group POSITION
;;;               ;; :READ-ONLY T
;;;               :WIDGET-ARGS
;;;               ("shadowThickness" 0 "alignment" 0 0))
;;;              ;;NIL
;;;              (COORD-MODE "Coord Sys:" :ABBREV-ASSOC
;;;               :ALIST
;;;               ;;#+never
;;;               (("Local X Y Z          " local)
;;;                       ("NAD-27 Utm" nad27-utm)
;;;                       ("NAD-27 Lat Long" nad27-lat-long))
;;;               #+never
;;;               ("Local X Y Z          "
;;;                "NAD-27 Utm"
;;;                "NAD-27 Lat Long")
;;;               :GROUP POSITION)
;;;              (UNITS "Local Units:" :ASSOC :ALIST
;;;               (("Meters" METERS) ("Feet" FEET) ("?" NIL))
;;;               :INITIAL-VALUE meters
;;;               ;;,*CARTESIAN-COORDINATE-SYSTEM-CVV-MENU-ITEM-LIST-UNITS-DEFAULT*
;;;               :GROUP POSITION)
;;;              (X "X:" :STRING :GROUP POSITION)
;;;              (Y "Y:" :STRING :GROUP POSITION)
;;;              (Z "Z:" :STRING :GROUP POSITION)
;;;              (LAST-SELECTED-HANDLE "Vert #:"
;;;               :INTEGER
;;;               :DOCUMENTATION
;;;               "Selected Vertex Number"
;;;               :GROUP
;;;               POSITION)
;;;
;;;              (geometry "GEOMETRY:" :group
;;;               :group-elements (X-SIZE Y-SIZE Z-SIZE ROOF-TYPE ROOF-PITCH ROOF-OVERHANG))
;;;        ;;;NIL NIL NIL
;;;              (X-SIZE "X Size:" :FLOAT :group geometry)
;;;              (Y-SIZE "Y Size:" :FLOAT :group geometry)
;;;              (Z-SIZE "Z Size:" :FLOAT :group geometry)
;;;              (ROOF-TYPE "Roof Type:"
;;;               :ASSOC
;;;               :ALIST
;;;               (("Gable" GABLE) ("Hip" HIP) ("Shed" SHED))
;;;               :GROUP
;;;               GEOMETRY)
;;;              (ROOF-PITCH "Roof Pitch:" :FLOAT :DOCUMENTATION "Roof Pitch (dz/dx)" :GROUP
;;;               GEOMETRY)
;;;              (ROOF-OVERHANG "Roof Overhang:"
;;;               :FLOAT
;;;               :DOCUMENTATION
;;;               "Roof Overhang"
;;;               :GROUP
;;;               GEOMETRY)
;;;        
;;;              (misc "MISC:" :group :initial-value nil
;;;               :group-elements (OPEN-FOR-VERTEX-MODIFICATION))
;;;              
;;;              (OPEN-FOR-VERTEX-MODIFICATION "Verts Open:"
;;;               :YES-OR-NO
;;;               :DOCUMENTATION
;;;               "Vertices are Modifiable"
;;;               :GROUP
;;;               MISC)
;;;
;;;              #+experiment
;;;              (pulldown "Pull Me Down:" :menu-button
;;;               :button-label "I dare you" :borderwidth 2 :background red
;;;               :relief raised
;;;               :item-list (("Foo" foo) ("Bar" bar)))
;;;
;;;              #+experiment
;;;              (subpanel nil :cvv-1-column-frame :items
;;;               ((X-SIZE2 "X Size:" :FLOAT :group geometry)
;;;                (Y-SIZE2 "Y Size:" :FLOAT :group geometry)
;;;                (Z-SIZE2 "Z Size:" :FLOAT :group geometry))
;;;               )
;;;
;;;              #+experiment
;;;              (xframe nil qframe :script
;;;               ((qbutton b1 :text "Button1")
;;;                (qlabel l1 :text "Label1")
;;;                (pack_children (get_global default_parent) :anchor w)))
;;;              )))

;;; Same as before, but testing the automatic generation of the group control items.

(setq *panel-controls-buttonargs*
      ;; See $FREEDIUS/lisp/tk/Qcme.app-defaults for these specs.
      (when nil '(-padx 3 -pady 1 -borderwidth 0 -highlightthickness 0))

      *panel-controls*
      `(panel_controls nil :button-list :group :preamble
	:items ((quit-panel "Quit")
		(update-panel "Update")
		(lock-panel "Lock" :button-type :toggle-button
		 :documentation "Lock panel to current object."
		; :selectcolor green
		 . ,*panel-controls-buttonargs*))
	. ,(and *panel-controls-buttonargs*
		`(:buttonargs ,*panel-controls-buttonargs*)))


      *house-object-cvv-item-list*
      (list*  *panel-controls*
	    `((NAME "Name:" :STRING :group :preamble)
	      (EDIT-GRAPHICS-STYLE NIL :BUTTON :BUTTON-LABEL "Edit Graphics Style"
	       :WIDGET-ARGS ("alignment" 0) :group :preamble)
	      (DESCRIPTION nil :MULTI-LINE-STRING :group annotation
	       :NLINES 3 :width 30 :scroll (list right) :wrap none
	       :weight 1)

	      (CHANGE-SUPERIOR "Superior:"
	       :EXCLUSIVE-LIST
	       :VISIBLE-ITEM-COUNT 3
	       :GROUP HIERARCHY)
	      (FEATURE-SETS "Feature Sets:"
	       :MULTIPLE-CHOOSE-LIST
	       :VISIBLE-ITEM-COUNT 5
	       :weight 3
	       :GROUP HIERARCHY)
	      (WORLD NIL :LABEL :group POSITION
	       :WIDGET-ARGS ("shadowThickness" 0 "alignment" 0 0))
	      (COORD-MODE "Coord Sys:" :ABBREV-ASSOC
	       :ALIST ((local "Local X Y Z          ")
		       (nad27-utm "NAD-27 Utm")
		       (nad27-lat-long "NAD-27 Lat Long"))
	       ;:INITIAL-VALUE local
	       :GROUP POSITION)
	      (UNITS "Local Units:" :ASSOC :ALIST
	       ((METERS "Meters") (FEET "Feet" :documentation "U.S. Feet") (NIL "?"))
	       :INITIAL-VALUE meters
	       ;;,*CARTESIAN-COORDINATE-SYSTEM-CVV-MENU-ITEM-LIST-UNITS-DEFAULT*
	       :GROUP POSITION)
	      (X "X:" :STRING :GROUP POSITION)
	      (Y "Y:" :STRING :GROUP POSITION)
	      (Z "Z:" :STRING :GROUP POSITION)
	      (LAST-SELECTED-HANDLE "Vert #:" :INTEGER
	       :DOCUMENTATION "Selected Vertex Number"
	       :GROUP POSITION)

	      (X-SIZE "X Size:" :FLOAT :group geometry :documentation "X-size of object")
	      (Y-SIZE "Y Size:" :FLOAT :group geometry)
	      (Z-SIZE "Z Size:" :FLOAT :group geometry :background green)
	      (ROOF-TYPE "Roof Type:" :ASSOC
	       :ALIST ((GABLE "Gable") (HIP "Hip") (SHED "Shed"))
	       :GROUP GEOMETRY)
	      (ROOF-PITCH "Roof Pitch:" :FLOAT :DOCUMENTATION "Roof Pitch (dz/dx)"
	       :GROUP GEOMETRY)
	      (ROOF-OVERHANG "Roof Overhang:" :FLOAT
	       :DOCUMENTATION "Roof Overhang"
	       :GROUP GEOMETRY)
	
	      (OPEN-FOR-VERTEX-MODIFICATION "Verts Open:" :YES-OR-NO
	       :DOCUMENTATION "Vertices are Modifiable"
	       :GROUP MISC)

	      (foo "Foo" :group)
	      (foo1 "Foo1" :YES-OR-NO :group foo)
	      ;;#+experiment
	      (pulldown "Pull Me Down:" :menu-button
	       :button-label "I dare you" 
	       ;; :borderwidth 2  :background red :relief raised 
	       :tearoff nil
	       :item-list ((foo "Foo" :background green)
			   (bar "Bar" :columnbreak t :documentation "Foo t o you")))

	      )))

(defparameter *default-group-items*
  '(;;(:preamble nil :group)
    (annotation "ANNOTATION:" :group :initial-value nil)
    (hierarchy "HIERARCHY:" :group :initial-value nil)
    (POSITION "LOCATION:" :group)
    (geometry "GEOMETRY:" :group)
    (misc "MISC:" :group :initial-value nil)
    ))


(setq house-object-cvv-panel
      (make-cvv-panel *house-object-cvv-item-list*
		      :default-group-items *default-group-items*
		      :title "House"))

(setq *tk-verbose* t)
(setq *tk-verbose* nil)
(tcl-cmd `(source "/opt/IU/FREEDIUS/default/tk/library-ttk/qwidgets.tcl"))
(tcl-cmd `(source "/opt/IU/FREEDIUS/default/tk/library-ttk/composite-widgets.tcl"))
(tcl-cmd `(source "/opt/IU/FREEDIUS/default/tk/library-ttk/tk-utils.tcl"))
(extract-groups *house-object-cvv-item-list*)
(infer-group-button-items *house-object-cvv-item-list* nil)
(infer-group-button-items *house-object-cvv-item-list* *default-group-items*)


(multiple-value-bind (group-buttons non-group-buttons unhandled-groups)
	  (infer-group-button-items *house-object-cvv-item-list* *default-group-items*)
  (values (reorder-cvv-itemlist-by-groups group-buttons non-group-buttons)
	  unhandled-groups))




;;; modified version -- test rearrangement of item order
(setq *panel-controls-buttonargs*
      ;; See $FREEDIUS/lisp/tk/Qcme.app-defaults for these specs.
      (when nil '(-padx 3 -pady 1 -borderwidth 0 -highlightthickness 0))

      *panel-controls*
      `(panel_controls nil  :button-list
	:group :preamble 
	:items ((quit-panel "Quit")
		(update-panel "Update")
		(lock-panel "Lock" :button-type :toggle-button
		 :documentation "Lock panel to current object."
		 . ,*panel-controls-buttonargs*))
	. ,(and *panel-controls-buttonargs*
		`(:buttonargs ,*panel-controls-buttonargs*)))


      *house-object-cvv-item-list*
      (cons *panel-controls*
	    `((NAME "Name:" :STRING :group :preamble)
	      (EDIT-GRAPHICS-STYLE NIL :BUTTON :BUTTON-LABEL "Edit Graphics Style"
	       :WIDGET-ARGS ("alignment" 0) :group :preamble)
	      (annotation "ANNOTATION:" :group :initial-value nil
	       :group-elements (DESCRIPTION))
	      #+never
	      (DESCRIPTION "Description:" :MULTI-LINE-STRING :group annotation
	       :NLINES 3 :width 30 :scroll (list right) :wrap none
	       :weight 1)

	      ;;(DESCRIPTION-label "Description:" qlabel)
	      (DESCRIPTION nil :MULTI-LINE-STRING :group annotation
	       :NLINES 3 :width 30 :scroll (list right) :wrap none
	       :weight 1)

	      (hierarchy "HIERARCHY:" :group :initial-value nil
	       :group-elements (CHANGE-SUPERIOR FEATURE-SETS))
	
	      (CHANGE-SUPERIOR "Superior:"
	       :EXCLUSIVE-LIST
	       :VISIBLE-ITEM-COUNT
	       3
	       :GROUP
	       HIERARCHY)
	      ;; out of order items
	      (X-SIZE "X Size:" :FLOAT :group geometry)
	      (Y-SIZE "Y Size:" :FLOAT :group geometry)
	      (Z-SIZE "Z Size:" :FLOAT :group geometry)
	      (POSITION "LOCATION:" :group
	       :group-elements (WORLD COORD-MODE UNITS X Y Z LAST-SELECTED-HANDLE))
	      (WORLD NIL :LABEL :group POSITION
	       ;; :READ-ONLY T
	       :WIDGET-ARGS
	       ("shadowThickness" 0 "alignment" 0 0))
	      ;;NIL
	      (COORD-MODE "Coord Sys:" :ABBREV-ASSOC
	       :ALIST
	       ;;#+never
	       (("Local X Y Z          " local)
		       ("NAD-27 Utm" nad27-utm)
		       ("NAD-27 Lat Long" nad27-lat-long))
	       #+never
	       ("Local X Y Z          "
		"NAD-27 Utm"
		"NAD-27 Lat Long")
	       :GROUP POSITION)
	      (UNITS "Local Units:" :ASSOC :ALIST
	       (("Meters" METERS) ("Feet" FEET) ("?" NIL))
	       :INITIAL-VALUE meters
	       ;;,*CARTESIAN-COORDINATE-SYSTEM-CVV-MENU-ITEM-LIST-UNITS-DEFAULT*
	       :GROUP POSITION)
	      (X "X:" :STRING :GROUP POSITION)
	      (Y "Y:" :STRING :GROUP POSITION)
	      (Z "Z:" :STRING :GROUP POSITION)
	      (LAST-SELECTED-HANDLE "Vert #:"
	       :INTEGER
	       :DOCUMENTATION
	       "Selected Vertex Number"
	       :GROUP
	       POSITION)

	      ;; out of order item
	      (FEATURE-SETS "Feature Sets:"
	       :MULTIPLE-CHOOSE-LIST
	       :VISIBLE-ITEM-COUNT
	       5 :weight 3
	       :GROUP
	       HIERARCHY)
	      
	      (geometry "GEOMETRY:" :group
	       :group-elements (X-SIZE Y-SIZE Z-SIZE ROOF-TYPE ROOF-PITCH ROOF-OVERHANG))
	;;;NIL NIL NIL
	      (ROOF-TYPE "Roof Type:"
	       :ASSOC
	       :ALIST
	       (("Gable" GABLE) ("Hip" HIP) ("Shed" SHED))
	       :GROUP
	       GEOMETRY)
	      (ROOF-PITCH "Roof Pitch:" :FLOAT :DOCUMENTATION "Roof Pitch (dz/dx)" :GROUP
	       GEOMETRY)
	      (ROOF-OVERHANG "Roof Overhang:"
	       :FLOAT
	       :DOCUMENTATION
	       "Roof Overhang"
	       :GROUP
	       GEOMETRY)
	
	      (misc "MISC:" :group :initial-value nil
	       :group-elements (OPEN-FOR-VERTEX-MODIFICATION))
	      
	      (OPEN-FOR-VERTEX-MODIFICATION "Verts Open:"
	       :YES-OR-NO
	       :DOCUMENTATION
	       "Vertices are Modifiable"
	       :GROUP
	       MISC)

	      #+experiment
	      (pulldown "Pull Me Down:" :menu-button
	       :button-label "I dare you" :borderwidth 2 :background red
	       :relief raised
	       :item-list (("Foo" foo) ("Bar" bar)))

	      #+experiment
	      (subpanel nil :cvv-1-column-frame :items
	       ((X-SIZE2 "X Size:" :FLOAT :group geometry)
		(Y-SIZE2 "Y Size:" :FLOAT :group geometry)
		(Z-SIZE2 "Z Size:" :FLOAT :group geometry))
	       )

	      #+experiment
	      (xframe nil qframe :script
	       ((qbutton b1 :text "Button1")
		(qlabel l1 :text "Label1")
		(pack_children (get_global default_parent) :anchor w)))
	      )))

|#

;;; This is all you need to eval to create house-object-cvv-panel (assuming the forms at the front
;;; of the file have already been evalled)

(setq house-object-cvv-panel
      (make-cvv-panel *house-object-cvv-item-list* :title "House"))

(extract-groups *house-object-cvv-item-list*)
(reorder-cvv-itemlist-group-order *house-object-cvv-item-list*)

(setq house-object-cvv-panel
      (make-cvv-panel (reorder-cvv-itemlist-group-order *house-object-cvv-item-list*)
		      :title "House"))


(describe house-object-cvv-panel)
(widget house-object-cvv-panel)
(tk::tcl-cmd `(winfo children ,(widget house-object-cvv-panel)))
(tk::tcl-cmd `(group_control_widgets ,(widget house-object-cvv-panel)))
(tk::tcl-cmd `(initialize_group_control_state ,(widget house-object-cvv-panel)))
(tk::tcl-cmd `(initialize_group_control_state .top2))
(setq *tk-verbose* t)
(setq *tk-verbose* nil)

(set-toplevel-widget-position (widget house-object-cvv-panel) :mouse)
(progn tk::foo)
(pop-up-panel house-object-cvv-panel)

(setq *many-cvv-panels*
      (loop repeat 100
	    for panel = (make-cvv-panel *house-object-cvv-item-list* :title "House")
	    do (tk::do-events)
	    collect panel))

(loop for panel in *many-cvv-panels*
      do (tk::tk-wm "iconify" (widget panel)))

(loop for panel in *many-cvv-panels*
      do (tk::tk-wm "deiconify" (widget panel)))

(loop for panel in *many-cvv-panels*
      do (tk::tk-destroy (widget panel)))

(describe (nth 0 *many-cvv-panels*))


Tue Apr 29 2003

I just found out that tcl DOES NOT use a garbage collector.
It uses reference counts.

I am looking at the tcl libtaries in $FREEDIUS/tk/library
for things that could explain the performance problems.

Possible sources of performance problems:

   putprop obj ind val  -- Each obj has its own hash table -- SMOKING GUN !
                        -- destroy_qwidget_callback calls remprop_all to delete the hashtable

|#


#|

(progn
  (setq house-panel-script
	(capturing-tcl-script
	 (make-cvv-panel *house-object-cvv-item-list* :title "House")))
  nil)
(loop for x in house-panel-script do (pprint x))
(mapc #'pprint house-panel-script)
(pprint house-panel-script)
(length house-panel-script)
(nth 1 house-panel-script)
(nth 33 house-panel-script)

(pop-up-panel house-object-cvv-panel)
(widget (get-named-item house-object-cvv-panel 'panel_controls))
(tcl-eval (widget (get-named-item house-object-cvv-panel 'panel_controls))
	  'config)

(pop-up-panel house-object-cvv-panel)
(panel-package house-object-cvv-panel)

(widget (get-named-item house-object-cvv-panel 'OPEN-FOR-VERTEX-MODIFICATION))
(tcl-cmd (list (string-append
	   (widget (get-named-item house-object-cvv-panel 'OPEN-FOR-VERTEX-MODIFICATION))
	   ".yes")  'config))
(cvv-item-value (widget (get-named-item house-object-cvv-panel 'OPEN-FOR-VERTEX-MODIFICATION)))
(cvv-item-value (widget (get-named-item house-object-cvv-panel 'roof-type)))
(tcl-cmd '(widget_value .top2.open-for-vertex-modification))
(tcl-cmd '(get_global .top2.open-for-vertex-modification))
(tcl-cmd '(.top2.roof-type.gable config))
(tcl-eval 'get_global ".top16.open-for-vertex-modification")
(tcl-eval 'get_global ".top16.units")

(widget (cvv-item 'OPEN-FOR-VERTEX-MODIFICATION house-object-cvv-panel))

(tcl-eval 'set_global
	  (widget (cvv-item 'OPEN-FOR-VERTEX-MODIFICATION house-object-cvv-panel))
	  1)

(tcl-eval 'set_global
	  (widget (cvv-item 'units house-object-cvv-panel))
	  'feet)

(cvv-item-value (cvv-item 'OPEN-FOR-VERTEX-MODIFICATION house-object-cvv-panel))
(cvv-item-value (cvv-item 'units house-object-cvv-panel))
(cvv-item-value (cvv-item 'coord-mode house-object-cvv-panel))

(setf (cvv-item-value (cvv-item 'units house-object-cvv-panel))
      (tcl-constant NIL))

(setf (cvv-item-value (cvv-item 'OPEN-FOR-VERTEX-MODIFICATION house-object-cvv-panel))
     (tcl-constant NIL)
      )

(setf (cvv-item-value (cvv-item 'OPEN-FOR-VERTEX-MODIFICATION house-object-cvv-panel))
     (tcl-constant T)
      )
     
(cvv-item-value (cvv-item 'OPEN-FOR-VERTEX-MODIFICATION house-object-cvv-panel))


(cvv-item-value (cvv-item 'name house-object-cvv-panel))
(cvv-item-value (cvv-item 'description house-object-cvv-panel))
(cvv-item 'description house-object-cvv-panel)
(cvv-item-value (cvv-item 'roof-pitch house-object-cvv-panel))
(cvv-item-value (cvv-item 'coord-mode house-object-cvv-panel))
(cvv-item-value (cvv-item 'units house-object-cvv-panel))
(cvv-item 'coord-mode house-object-cvv-panel)
(widget (cvv-item 'coord-mode house-object-cvv-panel))

(tcl-eval (widget (cvv-item 'coord-mode house-object-cvv-panel))
	  'cget :textvariable)
(tcl-eval 'get_global 'coord-mode)
(tcl-eval 'set_global 'coord-mode 0)
(tcl-eval 'set_optionmenu_selected_item
	  (widget (cvv-item 'coord-mode house-object-cvv-panel))
	  0)

(tcl-eval 'set_optionmenu_items
	  (widget (cvv-item 'coord-mode house-object-cvv-panel))
	  `(list .,(loop for item in '("Foo 1" "Barr 2")
			collect `(list ,(tcl-maybe-brace-string item))))
	  "{}")

(tcl-eval 'set_optionmenu_items
	  (widget (cvv-item 'coord-mode house-object-cvv-panel))
	  `(list .,(loop for item in '("Foo 1" "Barr 2")
			collect `(list ,(tcl-maybe-brace-string item))))
	  "{}")

(tcl-eval 'qwidget_class (widget (cvv-item 'coord-mode house-object-cvv-panel)))

(tcl-list->toplevel-strings (tcl-eval 'widget_value (widget (cvv-item 'coord-mode house-object-cvv-panel))))


(set-listbox-items (widget(cvv-item 'feature-sets house-object-cvv-panel))
		   '("fs1" "fs2"))
(get-listbox-selected-item-indices (widget(cvv-item 'feature-sets house-object-cvv-panel)))

(tcl-eval 'get_optionmenu_item_text
	  (widget (cvv-item 'coord-mode house-object-cvv-panel))
	  0)

(cvv-item 'coord-mode house-object-cvv-panel)
(tcl-eval 'get_optionmenu_selected_item
	  (widget (cvv-item 'coord-mode house-object-cvv-panel)))
	  

(set-item-list (cvv-item 'coord-mode house-object-cvv-panel)
	       '("Foo 1" "Barr 2"))

(set-item-list (cvv-item 'feature-sets house-object-cvv-panel)
	       '("Foo 1" "Barr 2"))

(get-listbox-selected-items (cvv-item 'feature-sets house-object-cvv-panel))
(cvv-item 'feature-sets house-object-cvv-panel)

(selected-item (cvv-item 'feature-sets house-object-cvv-panel))
(selected-item (cvv-item 'coord-mode house-object-cvv-panel))
(cvv-item 'coord-mode house-object-cvv-panel)
 |#

       

#+never
(defmethod make-cvv-item (parent (class (eql :exclusive-list)) name label &rest args
				 &key button-label)
  (make-cvv-item-widget parent 'qbutton name label
		   (if button-label
		       (list* :text button-label (remf args :alist))
		       args)))

#|
(st:load-system :bare-cvv)

(progn 
  (tcl-eval 'qtoplevel '.a :title "foo" )
  (make-cvv-item '.a :button-list 'controls nil :items
		 '(("Foo" 1) ("Bar" 2)))
  (make-cvv-item '.a :string 'txt "Timezone")
  (make-cvv-item '.a :button 'but1 nil :button-label "DOIT" :anchor 'w)
  (make-cvv-item '.a :assoc 'ex1 "VertsOpen:"
		 :button_args '(list :indicatoron 0)
		 :alist '(("Yes" 1) ("No" 0)))
  ;;  (make-cvv-item '.a :group 'grp1 nil :group-elements '(lab1 DESCRIPTION))
  (make-cvv-item '.a :group '.a.grp1 nil :group-elements '(lab1 DESCRIPTION))
  (make-cvv-item '.a :label 'lab1 nil :initial-value "Label" :anchor 'w )
  (make-cvv-item '.a :multi-line-string 'DESCRIPTION "Description:" :height 3)
  (tcl-eval 'qgrid_2_column '.a)
  ;;(tcl-eval 'pack '.a.txt_label '.a.txt '.a.but1 :side 'left)
  )

(tk-destroy (widget (panel (gui::selected-pane))))

(setq *tk-verbose* t)
(tk-destroy '.a)

(progn 
  (tcl-eval 'qtoplevel '.a :text "foo" )
  (make-cvv-item '.a :string 'txt "Timezone" :height 4)
  (make-cvv-item '.a :button 'but1 nil :button-label "DOIT" )
  (make-cvv-item '.a :abbrev-assoc 'ex1 "VertsOpen:"
		 :alist '(("Yes" 1) ("No" 0)) :indicatoron 0)
  
  (tcl-eval 'qgrid_2_column '.a)
  ;;(tcl-eval 'pack '.a.txt_label '.a.txt '.a.but1 :side 'left)
  )

(progn 
  (tcl-eval 'qtoplevel '.a :text "foo" )
  (make-cvv-item '.a :string 'txt nil :height 4)
  (tcl-eval 'pack '.a.txt :side 'left))

(maybe-compile-file-load "$FREEDIUS/lisp/tk/cme-cvv-compat.lisp")
(tcl-cmd `(option readfile ,(tk::tcl-filename-filter "$FREEDIUS/lisp/tk/cme.app-defaults")))
(tcl-cmd `(option readfile ,(tk::tcl-filename-filter "$FREEDIUS/lisp/tk/Qcme.app-defaults")))

|#



#|
;;; original item-list
(setq *house-object-cvv-item-list*
      (cons *panel-controls*
	    '((NAME "Name:" :STRING)
	      (EDIT-GRAPHICS-STYLE NIL :BUTTON :BUTTON-LABEL "Edit Graphics Style"
	       :WIDGET-ARGS ("alignment" 0))
	      (CHANGE-SUPERIOR "Superior:"
	       :EXCLUSIVE-LIST
	       :VISIBLE-ITEM-COUNT
	       3
	       :GROUP
	       HIERARCHY)
	      (FEATURE-SETS "Feature Sets:"
	       :MULTIPLE-CHOOSE-LIST
	       :VISIBLE-ITEM-COUNT
	       5
	       :GROUP
	       HIERARCHY)
	      (WORLD NIL
	       :LABEL
	       ;; :READ-ONLY T
	       :WIDGET-ARGS
	       ("shadowThickness" 0 "alignment" 0 0))
	      (DESCRIPTION "Description:" :MULTI-LINE-STRING :NLINES 3)
	      ;;NIL
	      (COORD-MODE "Coord Sys:" :ABBREV-ASSOC :ALIST (("Local X Y Z          " NIL))
	       :GROUP POSITION)
	      (UNITS "Local Units:" :ASSOC :ALIST
	       (("Feet" FEET) ("Meters" METERS) ("?" NIL)) :INITIAL-VALUE
	       *CARTESIAN-COORDINATE-SYSTEM-CVV-MENU-ITEM-LIST-UNITS-DEFAULT* :GROUP
	       POSITION)
	      (X "X:" :STRING :GROUP POSITION)
	      (Y "Y:" :STRING :GROUP POSITION)
	      (Z "Z:" :STRING :GROUP POSITION)
	      (LAST-SELECTED-HANDLE "Vert #:"
	       :INTEGER
	       :DOCUMENTATION
	       "Selected Vertex Number"
	       :GROUP
	       POSITION)
	      (OPEN-FOR-VERTEX-MODIFICATION "Verts Open:"
	       :YES-OR-NO
	       :DOCUMENTATION
	       "Vertices are Modifiable"
	       :GROUP
	       MISC)
	;;;NIL NIL NIL
	      (X-SIZE "X Size:" :FLOAT)
	      (Y-SIZE "Y Size:" :FLOAT)
	      (Z-SIZE "Z Size:" :FLOAT)
	      (ROOF-TYPE "Roof Type:"
	       :ASSOC
	       :ALIST
	       (("Gable" :GABLE) ("Hip" :HIP) ("Shed" :SHED))
	       :GROUP
	       GEOMETRY)
	      (ROOF-PITCH "Roof Pitch:" :FLOAT :DOCUMENTATION "Roof Pitch (dz/dx)" :GROUP
	       GEOMETRY)
	      (ROOF-OVERHANG "Roof Overhang:"
	       :FLOAT
	       :DOCUMENTATION
	       "Roof Overhang"
	       :GROUP
	       GEOMETRY))))
|#





#|

Wed May 24 2006

(tcl-cmd '(toplevel .foo ))

;;; When destroyed, .foo has the X11_LISP_ERROR_HANDLER problem too.






|#
