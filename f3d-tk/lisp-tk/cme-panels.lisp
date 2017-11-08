(in-package :tk)

#|
;;; /usr/local/cme/lisp/lucid/irix5/lisp-clos

(load "$FREEDIUS/lisp/boot.lisp")
(st:load-system 'tk)
(in-package :lisptk)

(init-lisptk '((in-package :tk)
	       (tk-wm "withdraw" ".")
	       ))

(setq *tk-verbose* t)
(setq *tk-verbose* nil)

(tcl-cmd `(event generate
	   ,(widget (gui::view-window (gui::top-view)))
	   <<enter>> -warp 1 -x 50 -y 50))

(widget (gui::view-window (gui::top-view)))

(maybe-compile-file-load "$FREEDIUS/lisp/tk/cme-panels.lisp")

(make-elt-panel2)

(make-elt-panel2d)

(setq img (gui::view-image (gui::top-view)))
(gui::push-image img (gui::selected-window gui::*interactor*))

(gui::top-view)
(describe gui::*interactor*)

(write-tcl-script "~/tmp/elt-panel2.tcl" (make-elt-panel2 :capture t))

(pop-up-panel *elt-panel*)

(setq to-win (image-calc-pane 0 0 *elt-panel*))
(panel to-win)
(pane-frame (image-calc-pane 0 0 *elt-panel*))

(push-image (view-image (top-view)) to-win)

(get-listbox-selected-items
 (widget-named (widget *elt-panel*) 'layers))

(tcl-eval (widget-named (widget *elt-panel*) 'entry)
	  'configure :exportselection 0)

(cvv-item-value *elt-panel* 'layers)

;;; get the 0th item in the item-list
(tcl-eval (widget-named (widget *elt-panel*) 'layers) "get" 0)

(cvv-item-value (widget-named *elt-panel* 'entry))
(tcl-list-to-lisp (cvv-item-value (widget-named *elt-panel* 'entry)))
(tcl-list-to-toplevel-strings (cvv-item-value (widget-named *elt-panel* 'entry)))
(tcl-list-to-toplevel-strings (cvv-item-value (widget-named *elt-panel* 'int)))

(tcl-list-to-toplevel-strings
 (car (tcl-list-to-toplevel-strings (cvv-item-value (widget-named *elt-panel* 'layers)))))

(cvv-item *elt-panel* 'layers)
(cvv-item-value *elt-panel* 'layers)
(tcl-list-to-toplevel-strings (cvv-item-value (widget-named *elt-panel* 'layers)))
(tcl-list-to-toplevel-strings
 (tcl-eval 'widget_value (widget-named *elt-panel* 'layers)))
(tcl-eval 'widget_value (widget-named *elt-panel* 'layers))
(tcl-list-to-strings "{{All Roads} Buildings} qListbox" 2)
(tcl-list-to-strings "{All Roads} Buildings" 1)
(tcl-list-to-strings "{All Roads} Buildings" 999)
(tcl-list-to-toplevel-strings "{{All Roads} Buildings} qListbox")
(tcl-list-to-toplevel-strings2 "{{All Roads} Buildings} qListbox" 2)
(tcl-list-to-lisp "{{All Roads} Buildings}")
(tcl-list-to-strings2 "{{All Roads} Buildings}" 2)
(tcl-cmd `(qwidget_class ,(widget-named *elt-panel* 'int)))

(tcl-cmd `(option readfile ,(tk::tcl-filename-filter "$FREEDIUS/lisp/tk/cme.app-defaults")))

(get-listbox-selected-items (widget-named *elt-panel* 'layers))

(tcl-cmd '(proc identity (x) (return $x)))
(tcl-cmd '(getprop .elt13.cc.image-orientation textvariable))
(tcl-cmd '(identity ${image-orientation}))

(tcl-cmd '(get_global qcme_popup_state))
(tcl-cmd '(set_global qcme_popup_state ""))
(lisptk::tcl-eval-internal "after 60000 [qcme_callback nil texture-release-timeout 0 ]")
(gui::cancel-release-textures-after-delay)
|#



(defmethod elt-callback ((panel widget-panel) widget item-name event args)
  (break)
  (format t "elt-callback ~a ~a ~a ~a ~a~%" panel widget item-name event args))

#+never
(defmethod elt-callback ((panel widget-panel) widget item-name event args)
  (if (typep (gui::widget-clidget widget) 'gui::basic-window)
      (break) ; shouldn't get here (tk-callback panel widget item-name event args)
      (case item-name
	(otherwise (format t "elt-callback ~a ~a ~a ~a ~a~%" panel widget item-name event args))
	)
      ))

(defparameter *rotation-items*
  '(("Rot Default" rot-default) ("Rot 90" rot90)
     ("Rot 180" rot180) ("Rot 270" rot270) 
     ("Rot Z Up" rot-z-up) ("Rot North Up" rot-north-up)))

(defparameter *mag-items*
  '(("Fit" fit) ("/16" g4) ("/8" g3) ("/4" g2)
    ("/2" g1) ("1" g0) ("2" g-1) ("4" g-2)))


(defparameter *sri-logo-path* "/homedir/quam/pix/sri-logo.gif")

(defun make-elt-panel2 (&key capture (width 400) (height 400)
			(panel-name ".elt") (control-column-width 10)
			(nx 2) (ny 2)
			(borderwidth 1))
  (let* ((toplevel-name (or panel-name (gensym-toplevel-widget-path panel-name)))
	 (toplevel-widget toplevel-name)
	 (right-frame (merge-widget-pathname toplevel-name "rf")))
    
    (mv-bind (frame-script pane-alist)
	(frame-panes-tk-script right-frame
			       :frame-name "pane"
			       :nx nx :ny ny
			       :width width :height height
			       :borderwidth borderwidth)
      (let* ((controlcol (merge-widget-pathname toplevel-name "cc"))
	     (toprow (merge-widget-pathname right-frame "toprow"))
	     (elt-app-defaults
	      (tk::tcl-filename-filter (or config::*cme-app-defaults-file* "$FREEDIUS/lisp/tk/elt.app-defaults")))
	     (logo-path *sri-logo-path*)
	     (elt-panel-script
	      `((option readfile ,elt-app-defaults)
		(qtoplevel ,toplevel-name :class "Elt" :title "ELT Panel" )
		
		(set_global default_parent (qframe ,controlcol))
		;; buttons
		(qbuttonlist panel_controls ;; :buttonargs (list :padx 0 :pady 0)
			     :items (list (list quit-panel "Quit")
					  (list update-panel "Update")
					  (list qtogglebutton lock :text "Lock")))
		(image create photo sri_logo :file ,logo-path)
		(qlabel sri-logo :image sri_logo)
		(qtogglebutton 3d-centering :text "3D CENTERING" :documentation
			       "View change commands center image at 3d position in current view.")
		(qtogglebutton auto-z-up :text "AUTO ORIENT" :initial_value 1
			       :documentation "When Image is first Selected, Rotate Z Up")
		(qtogglebutton auto-zoom-to-fit :text "AUTO FIT"
			       :initial_value t :documentation
			       "When Image is first Selected
, Zoom To Fit")
		(qoptionmenu image-orientation
			     :items ,(cvv-to-qwidget-items *rotation-items*)
			     ;;:initial_value rot-z-up
			     :initial_value "Rot Z Up"
			     :documentation "Orientation of Image")
		(qbutton drag :text "DRAG")
		(qlabel image-mag-label :text "MAGNIFY:" :anchor w
			:documentation "Pick Image Magnification")
		(qradiogroup image-mag
			     :items '((FIT "Fit") (G4 "/16") (G3 "/8") (G2 "/4")
		   (G1 "/2") (G0 "1") (G-1 "2") (G-2 "4"))
			     :initial_value g0 :buttonargs (list :indicatoron false)
			     :documentation "Pick Image Magnification")
		(qlabel layers-label :text "LAYERS:" :anchor w)
		(qlistbox layers :height 10 :scroll (list right)
			  ;;-selectmode extended
			  :selectmode multiple
			  :weight 1 :width ,control-column-width)
		(qentry entry :initial_value "mumble bar")
		(qinteger int :initial_value 15)
		(qframe ,right-frame :weight 1)

		(set_global default_parent (qframe ,toprow))
		(qlabel pick-site-label :text "   SITE:")
		(qoptionmenu pick-site )
		(qlabel pick-image-label :text "  IMAGE:")
		(qoptionmenu pick-image)
		;;(qcanvas ,pane :width ,width :height ,height :weight 1)
		,@frame-script
		(qgrid_1_column ,controlcol)
		(pack_children ,toprow :side left)
		(qgrid_1_column ,right-frame)
		(grid ,controlcol :column 0 :row 0 :sticky news)
		(grid ,right-frame :column 1 :row 0 :sticky news)
		(grid rowconfigure ,toplevel-name 0 :weight 1)
		(grid columnconfigure ,toplevel-name 1 :weight 1)
		)))

	(if capture
	    (capturing-tcl-script (tcl-script elt-panel-script))
	    (progn
	      (tcl-script elt-panel-script)      
	      (loop for entry in pane-alist
		    for ((ix iy) . tkglwin-name) = entry
		    do (ignore ix iy)
		       (setf (cdr entry) (gui::make-window tkglwin-name :class gui::*default-pane-class*)))

	      (set-listbox-items
	       (merge-widget-pathname controlcol "layers_frm.layers")
	       '("All Roads" "Buildings" "Areas"))
	      (setq *elt-panel*
		    (make-widget-panel
		     toplevel-name 'elt-callback
		     :panel-class 'gui::window-panel ; 'widget-panel
		     :package :gui
		     :panel-args (list :nx nx :ny ny
				       :width width :height height
				       :pane-alist pane-alist)))

	      (loop for entry in pane-alist
		    for ((ix iy) . window) = entry
		    do (ignore ix iy)
		       (setf (gui::panel window) *elt-panel*))
	      ))
	))))

(defun make-elt-panel2c (&key capture (width 400) (height 400)
			 (tk-frame-class ".elt") (control-column-width 10)
			 (nx 2) (ny 2)
			 (borderwidth 1))
  (let* ((toplevel-name (gensym-toplevel-widget-path tk-frame-class))
	 (toplevel-widget toplevel-name)
	 (right-frame (merge-widget-pathname toplevel-name "rf")))
    (mv-bind (pane-frame-script pane-alist)
	(frame-panes-tk-script right-frame
			       :frame-name "pane"
			       :nx nx :ny ny
			       :parent toplevel-widget
			       :width width :height height
			       :borderwidth borderwidth)
      (let* ((controlcol (merge-widget-pathname toplevel-name "cc"))
	     (toprow (merge-widget-pathname right-frame "toprow"))
	     (pane (merge-widget-pathname right-frame "pane"))
	     (elt-app-defaults
	      (tk::tcl-filename-filter (or config::*cme-app-defaults-file* "$FREEDIUS/lisp/tk/elt.app-defaults")))
	     (logo-path (tk::tcl-filename-filter *sri-logo-path*))
	     (elt-panel-script
	      `((option readfile ,elt-app-defaults)
		;;(qtoplevel ,toplevel-name :class "Elt" :title "ELT Panel" )
		
		(set_global default_parent (qframe ,controlcol))
		;; buttons
		(qbuttonlist panel_controls;; :buttonargs (list :padx 0 :pady 0)
		 :items (list (list quit-panel "Quit")
			 (list update-panel "Update")
			 (list qtogglebutton lock :text "Lock")))
		(image create photo sri_logo :file ,logo-path)
		(qlabel sri-logo :image sri_logo)
		(qtogglebutton 3d-centering :text "3D CENTERING" :documentation
		 "View change commands center image at 3d position in current view.")
		(qtogglebutton auto-z-up :text "AUTO ORIENT" :initial_value 1
		 :documentation "When Image is first Selected, Rotate Z Up")
		(qtogglebutton auto-zoom-to-fit :text "AUTO FIT"
		 :initial_value t :documentation
		 "When Image is first Selected, Zoom To Fit")
		(qoptionmenu image-orientation
		 :items ,(cvv-to-qwidget-items *rotation-items*)
		 ;;:initial_value rot-z-up
		 :initial_value "Rot Z Up"
		 :documentation "Orientation of Image")
		(qbutton drag :text "DRAG")
		(qlabel image-mag-label :text "MAGNIFY:" :anchor w
		 :documentation "Pick Image Magnification")
		(qradiogroup image-mag
		 :items ,(cvv-to-qwidget-items *mag-items*)
		 :initial_value g0 :buttonargs (list :indicatoron false)
		 :documentation "Pick Image Magnification")
		(qlabel layers-label :text "LAYERS:" :anchor w)
		(qlistbox layers :height 10 :scroll (list right)
		 ;;-selectmode extended
		 :selectmode multiple
		 :weight 1 :width ,control-column-width)
		(qentry entry :initial_value "mumble bar")
		(qinteger int :initial_value 15)
		(qframe ,right-frame :weight 1)

		(set_global default_parent (qframe ,toprow))
		(qlabel pick-site-label :text "   SITE:")
		(qoptionmenu pick-site )
		(qlabel pick-image-label :text "  IMAGE:")
		(qoptionmenu pick-image)
		;;(qcanvas ,pane :width ,width :height ,height :weight 1)
		,@pane-frame-script
		(qgrid_1_column ,controlcol)
		(pack_children ,toprow :side left)
		(qgrid_1_column ,right-frame)
		(grid ,controlcol :column 0 :row 0 :sticky news)
		(grid ,right-frame :column 1 :row 0 :sticky news)
		(grid rowconfigure ,toplevel-name 0 :weight 1)
		(grid columnconfigure ,toplevel-name 1 :weight 1)
		)))
	(setq *elt-panel*

	      (gui::make-cme-frame "ELT" :nx nx :ny ny :width width :height height
				   :border-width borderwidth
				   :toplevel-widget toplevel-widget
				   :callback-function 'elt-callback
				   :FRAME-SCRIPT elt-panel-script :PANE-ALIST pane-alist
				   ))
	))))


(defun make-elt-panel2d (&key capture (width 400) (height 400)
			 (tk-frame-class ".elt") (control-column-width 10)
			 (nx 2) (ny 2)
			 (borderwidth 1))
  (let* ((toplevel-name (gensym-toplevel-widget-path tk-frame-class))
	 (toplevel-widget toplevel-name)
	 (right-frame (merge-widget-pathname toplevel-name "rf"))
	 )
    (mv-bind (pane-frame-script pane-alist)
	(frame-panes-tk-script right-frame
			       :frame-name "pane"
			       :nx nx :ny ny
			       :parent toplevel-widget
			       :width width :height height
			       :borderwidth borderwidth)
      (let* ((controlcol (merge-widget-pathname toplevel-name "cc"))
	     (toprow (merge-widget-pathname right-frame "toprow"))
	     (elt-app-defaults
	      (tk::tcl-filename-filter (or config::*cme-app-defaults-file* "$FREEDIUS/lisp/tk/elt.app-defaults")))
	     (logo-path (tk::tcl-filename-filter *sri-logo-path*))
	     (elt-panel-script
	      `((option readfile ,elt-app-defaults)
		;;(qtoplevel ,toplevel-name :class "Elt" :title "ELT Panel" )
		
		(set_global default_parent (qframe ,controlcol))
		;; buttons
		(qbuttonlist panel_controls;; :buttonargs '(:padx 0 :pady 0)
		 :items '((quit-panel "Quit")
			  (update-panel "Update")
			  (qtogglebutton lock :text "Lock")))
		(image create photo sri_logo :file ,logo-path)
		(qlabel sri-logo :image sri_logo)
		(qtogglebutton 3d-centering :text "3D CENTERING" :documentation
		 "View change commands center image
at 3d position in current view.")
		(qtogglebutton auto-z-up :text "AUTO ORIENT" :initial_value 1
		 :documentation "When Image is first Selected,
Rotate Z Up")
		(qtogglebutton auto-zoom-to-fit :text "AUTO FIT"
		 :initial_value t :documentation
		 "When Image is first Selected,
Zoom To Fit")
		(qoptionmenu image-orientation
		 :items			;,(cvv-to-qwidget-items *rotation-items*)
		 '((ROT-DEFAULT "Rot Default") (ROT90 "Rot 90")
		   (ROT180 "Rot 180") (ROT270 "Rot 270")
		   (ROT-Z-UP "Rot Z Up") (ROT-NORTH-UP "Rot North Up"))
		 ;;:initial_value rot-z-up
		 :initial_value "Rot Z Up"
		 :documentation "Orientation of Image")
		(qbutton drag :text "DRAG")
		(qlabel image-mag-label :text "MAGNIFY:" :anchor w
		 :documentation "Pick Image Magnification")
		(qradiogroup image-mag
		 :items			;,(cvv-to-qwidget-items *mag-items*)
		 '((FIT "Fit") (G4 "/16") (G3 "/8") (G2 "/4")
		   (G1 "/2") (G0 "1") (G-1 "2") (G-2 "4"))
		 :initial_value g0 :buttonargs '(:indicatoron false)
		 :documentation "Pick Image Magnification")
		(qlabel layers-label :text "LAYERS:" :anchor w)
		(qlistbox layers :height 10 :scroll '(right)
		 ;;-selectmode extended
		 :selectmode multiple
		 :weight 1 :width ,control-column-width)
		(qentry entry :initial_value "mumble bar")
		(qinteger int :initial_value 15)
		(qframe ,right-frame :weight 1)

		(set_global default_parent (qframe ,toprow))
		(qlabel pick-site-label :text "   SITE:")
		(qoptionmenu pick-site )
		(qlabel pick-image-label :text "  IMAGE:")
		(qoptionmenu pick-image)
		;;(qcanvas ,pane :width ,width :height ,height :weight 1)
		,@pane-frame-script
		(qgrid_1_column ,controlcol)
		(pack_children ,toprow :side left)
		(qgrid_1_column ,right-frame)
		(grid ,controlcol :column 0 :row 0 :sticky news)
		(grid ,right-frame :column 1 :row 0 :sticky news)
		(grid rowconfigure ,toplevel-name 0 :weight 1)
		(grid columnconfigure ,toplevel-name 1 :weight 1)
		)))
	(setq *elt-panel*

	      (gui::make-cme-frame "ELT" :nx nx :ny ny :width width :height height
				   :border-width borderwidth
				   :toplevel-widget toplevel-widget
				   :callback-function 'elt-callback
				   :FRAME-SCRIPT elt-panel-script :PANE-ALIST pane-alist
				   ))
	))))

#|
(progn *mag-items*)
(progn *rotation-items*)
(cvv-to-qwidget-items *mag-items*)
|#

;;; this is broken
(defun make-elt-panel2x (&key capture (pane-width 400) (pane-height 400)
			 (panel-name ".elt") (control-column-width 10)
			 (borderwidth 1))
  (let* ((toplevel-name (or panel-name (gensym-toplevel-widget-path panel-name)))
	 (toplevel-widget toplevel-name))
    (mv-bind (frame-script pane-alist)
	(frame-panes-tk-script toplevel-widget
			       :frame-name toplevel-name
			       :nx 1 :ny 1
			       :width pane-width :height pane-height
			       :borderwidth borderwidth)
      (loop for entry in pane-alist
	    for ((ix iy) . tkglwin-name) = entry
	    do (ignore ix iy)
	       (setf (cdr entry) (make-window tkglwin-name :class pane-class)))
      (let*((controlcol (merge-widget-pathname toplevel-name "cc"))
	    (right-frame (merge-widget-pathname toplevel-name "rf"))
	    (toprow (merge-widget-pathname right-frame "toprow"))
	    (pane (merge-widget-pathname right-frame "pane"))
	    (elt-app-defaults
	      (tk::tcl-filename-filter (or config::*cme-app-defaults-file* "$FREEDIUS/lisp/tk/elt.app-defaults")))
	    (logo-path (tk::tcl-filename-filter *sri-logo-path*))
	    (elt-panel-script
	     `((option readfile ,elt-app-defaults)
	       ;;(qtoplevel ,toplevel-name -class "Elt" -title "ELT Panel" )
	       (set_global default_parent (qframe ,controlcol))
	       ;; buttons
	       (qbuttonlist panel_controls ;; -buttonargs '(-padx 0 -pady 0)
		-items '((quit-panel "Quit")
			 (update-panel "Update")
			 (qtogglebutton lock -text "Lock")))
	       (image create photo sri_logo :file ,logo-path)
	       (qlabel sri-logo -image sri_logo)
	       (qtogglebutton 3d-centering -text "3D CENTERING" -documentation
		"View change commands center image at 3d position in current view.")
	       (qtogglebutton auto-z-up -text "AUTO ORIENT" -initial_value 1
		-documentation "When Image is first Selected, Rotate Z Up")
	       (qtogglebutton auto-zoom-to-fit -text "AUTO FIT"
		-initial_value t -documentation
		"When Image is first Selected, Zoom To Fit")
	       (qoptionmenu image-orientation -items ,*rotation-items*
		;;-initial_value rot-z-up
		-initial_value "Rot Z Up"
		-documentation "Orientation of Image")
	       (qbutton drag -text "DRAG")
	       (qlabel image-mag-label -text "MAGNIFY:" -anchor w
		-documentation "Pick Image Magnification")
	       (qradiogroup image-mag -items
		;; (list "Fit" "/16" "/8" "/4" "/2" "1" "2" "4")
		'((fit "Fit") (g4 "/16") (g3 "/8")
		  (g2 "/4") (g1 "/2") (g0 "1") (g-1 "2")
		  (g-2 "4"))
		-initial_value g0 -buttonargs (-indicatoron false)
		-documentation "Pick Image Magnification")
	       (qlabel layers-label -text "LAYERS:" -anchor w)
	       (qlistbox layers -height 10 -scroll (right)
		;;-selectmode extended
		-selectmode multiple
		-weight 1 -width ,control-column-width)
	       (qentry entry -initial_value "mumble bar")
	       (qinteger int -initial_value 15)
	       (qframe ,right-frame -weight 1)
	       (set_global default_parent (qframe ,toprow))
	       (qlabel pick-site-label -text "   SITE:")
	       (qoptionmenu pick-site )
	       (qlabel pick-image-label -text "  IMAGE:")
	       (qoptionmenu pick-image)
	       (qcanvas ,pane -width ,pane-width -height ,pane-height -weight 1)
	       (qgrid_1_column ,controlcol)
	       (pack_children ,toprow -side left)
	       (qgrid_1_column ,right-frame)
	       (grid ,controlcol -column 0 -row 0 -sticky news)
	       (grid ,right-frame -column 1 -row 0 -sticky news)
	       (grid rowconfigure ,toplevel-name 0 -weight 1)
	       (grid columnconfigure ,toplevel-name 1 -weight 1)
	       )))

	(if capture
	    (capturing-tcl-script (tcl-script elt-panel-script))
	    (progn
	      (tcl-script elt-panel-script)
	      (set-listbox-items (merge-widget-pathname controlcol "layers_frm.layers")
				 '("Roads" "Buildings" "Areas"))
	      (setq *elt-panel* (make-widget-panel toplevel-name 'elt-callback
						   :panel-class 'gui::window-panel ; 'widget-panel
						   ))
	      ))))))




#|
()

(progn 
  (setq *STACK-MENU-ITEM-LIST*
	'(("Name Image" NAME-IMAGE)
	  ("Inspect Image" INSPECT-IMAGE)
	  ("Describe Image" DESCRIBE-OBJECT)
	  ("Refresh Pane" REFRESH-PANE)
	  ("Select Pane" SELECT-PANE)
	  (:separator)
	  ("Fwd Cycle" CYCLE-PANE-STACK)
	  ("Rev Cycle" REVERSE-CYCLE-PANE-STACK)
	  ("Move View" MOVE-VIEW)
	  ("Copy View" COPY-VIEW)
	  ("Pop Stack" KILL-VIEW)
	  ("Unkill Top" UNKILL-VIEW)
	  ("Expunge Popped Views" EXPUNGE-KILLED-VIEWS)
	  ("Pop Expunge" POP-PANE-EXPUNGE)
	  (:separator)
	  ("Inspect Stack" INSPECT-STACK)
	  ("Copy Stack" COPY-VIEW-STACK)
	  ("Kill Stack" KILL-OBJECT-STACK)
	  ("Unkill Stack" UNKILL-OBJECT-STACK)
	  ("Kill Frame" KILL-FRAME)))

  (setq *GEOM-MENU-ITEM-LIST*
	'(("Recenter" RECENTER)
	  ("% Reposition" REPOSITION-PERCENTWISE)
	  (:separator)
	  ("Zoom In" ZOOM-IN)
	  ("Zoom Out" ZOOM-OUT)
	  ("Zoom To Fit" ZOOM-TO-FIT)
	  ("Zoom To Full Res" ZOOM-TO-FULL-RES)
	  ("Fast Zoom In" FAST-ZOOM-IN)
	  ("Fast Zoom Out" FAST-ZOOM-OUT)
	  (:separator)
	  ("Mirror X" FLIP-X)
	  ("Mirror Y" FLIP-Y)
	  ("Rotate CW" ROTATE)
	  ("Rotate CCW" ROTATE-CCW)
	  ("Scale Rotate" SCALE-ROTATE)
	  (:separator)
	  ("Window" WINDOW)))

  (setq *ARITH-MENU-ITEM-LIST*
	'(;;("Boole" :MENU *BOOLEAN-MENU-ITEM-LIST* :DOCUMENTATION "Boolean Operators")
	  ;;("FFT" :MENU *FFT-MENU-ITEM-LIST*)
	  ("Copy" COPY-IMAGE)
	  ("Float" FLOAT-IMAGE)
	  ("Fix" FIX-IMAGE)
	  ("Round" ROUND-IMAGE)
	  ("Resize Pixel" CHANGE-ELEMENT-TYPE)
	  ("Negate" NEGATE-IMAGE)
	  ("Linear Xform" LINEAR-TRANSFORM-IMAGE)
	  ("Clip" IMAGE-CLIP)
	  ("Threshold" THRESHOLD-IMAGE)
	  ("" :SEPARATOR NIL)
	  ("Add" ADD-IMAGES)
	  ("Subtract" DIFFERENCE-IMAGES)
	  ("Linear Combine" LINEAR-COMBINE-IMAGES)))

  (setq *ENHANCE-MENU-ITEM-LIST* nil)
  (setq *GRAPH-MENU-ITEM-LIST* nil)
  (setq *IO-MENU-ITEM-LIST* nil)
  (setq *MISC-MENU-ITEM-LIST* nil)
  (setq *OBJECT-CREATION-MENU-ITEM-LIST* nil)
  (setq *VIEW-TRANSFORM-MENU-ITEM-LIST* nil)
  (setq *PANELS-ITEM-LIST* nil)

  (setq menu-bar-buttons
	'(("Stacks" nil :button-list :items *STACK-MENU-ITEM-LIST*
	   :DOCUMENTATION "Manipulation of Stacks of Objects")
	  ("Geom" nil :button-list :items *GEOM-MENU-ITEM-LIST*
	   :DOCUMENTATION "Geometric Operations")
	  ("Arith" nil :button-list :items *ARITH-MENU-ITEM-LIST*
	   :DOCUMENTATION "Arithmetic Operators")
	  ("Enhance" nil :button-list :items *ENHANCE-MENU-ITEM-LIST*
	   :DOCUMENTATION "Image Enhancement Operators")
	  ("Graph" nil :button-list *GRAPH-MENU-ITEM-LIST*
	   :DOCUMENTATION "Graphical Operators")
	  (" I/O " nil :button-list *IO-MENU-ITEM-LIST*
	   :DOCUMENTATION "Image Input and Output")
	  ("Misc" nil :button-list *MISC-MENU-ITEM-LIST*
	   :DOCUMENTATION "Miscellany")
	  ("CreateObject" nil :button-list *OBJECT-CREATION-MENU-ITEM-LIST*
	   :DOCUMENTATION "Create CME Objects")
	  ("Transforms" nil :button-list *VIEW-TRANSFORM-MENU-ITEM-LIST*
	   :DOCUMENTATION "Create New Camera Transforms")
	  ("Panels" nil :button-list  *VIEW-MENU-ITEM-LIST*
	   :DOCUMENTATION "CME Panels")))

  )


(progn `((draw-area :qglxwin nil :height ,draw-area-height :width ,width)
	 (buttons-form nil :form
	  :item-list)))
	      
(progn `((mainmenubar nil :button-list :items ,menu-bar-buttons)
	 #+never
	 (togglebar nil :qbuttonlist :items
	  (("Tandem" tandem :toggle-button
	    :documentation "Invert the state of Tandem Mode.")
	   ("Desens" desensitize :button-type :toggle-button
	    :documentation "Desensitize All Site Models.")
	   ("Hide" hide :button-type :toggle-button
	    :documentation "Hide all Site Models.")
	   ("ABORT" abort :button
	    :documentation "Abort in the Middle of a Menu Command")
	   ("BREAK" break :button
	    :documentation "Interrupt Execution and Enter Lisp Debugger")
	   ))))
		


|#



#|
(setq *tk-verbose* t)
(setq *tk-verbose* nil)


(tcl-script '((qtoplevel .b)
	      (qframe .b.f)
	      (qlabel .b.f.l -width 40 -text "foo")
	      (qlistbox .b.lb -height 20 -width 5 -items (list 1 2 3 4 5 6))
	      (qcanvas .b.f.c -width 400 -height 400)
	      (pack .b.f.l .b.f.c -side top -anchor nw)
	      (pack .b.lb .b.f -side left -anchor nw)
	      ))

(tcl-eval 'destroy'.b )
(tcl-eval '.b.l 'configure '-text "foo")
(tcl-eval 'option 'get ".elt" 'background "")
(make-elt-panel :control-column-width 8)
|#

(defun make-elt-panel (&key capture (pane-width 400) (pane-height 400)
			    (panel-name ".elt") (control-column-width 10))
  (let* ((toplevel-name (or panel-name (gensym-toplevel-widget-path panel-name)))
	 (controlcol (merge-widget-pathname toplevel-name "cc"))
	 (right-frame (merge-widget-pathname toplevel-name "rf"))
	 (toprow (merge-widget-pathname right-frame "toprow"))
	 (pane (merge-widget-pathname right-frame "pane"))
	 (elt-app-defaults
	  (tk::tcl-filename-filter (or config::*cme-app-defaults-file* "$FREEDIUS/lisp/tk/elt.app-defaults")))
	 (logo-path (tk::tcl-filename-filter *sri-logo-path*))
	     (elt-panel-script
	  `((option readfile ,elt-app-defaults)
	    (qtoplevel ,toplevel-name -class "Elt" -title "ELT Panel" )
	    (set_global default_parent (qframe ,controlcol))
	    (qbuttonlist buttons :buttonargs (list -padx 0 -pady 0)
	     :items
	     (list (list quit-panel "Quit")
	      (list update-panel "Update")
	      (list qtogglebutton -text "Lock")))
	    (image create photo sri_logo -file ,logo-path)
	    (qlabel sri-logo -image sri_logo)
	    (qtogglebutton 3d-centering -text "3D CENTERING" :documentation
	     "View change commands center image at 3d position in current view.")
	    (qtogglebutton auto-z-up -text "AUTO ORIENT" :initial_value 1
	     :documentation "When Image is first Selected, Rotate Z Up"
	     )
	    (qtogglebutton auto-zoom-to-fit -text "AUTO FIT"
	     :initial_value t :documentation
	     "When Image is first Selected, Zoom To Fit"
	     )
	    (qoptionmenu image-orientation :items
	     (list (rot-default "Rot Default") (rot90 "Rot 90")
	      (rot180 "Rot 180")(rot270  "Rot 270") (rot-z-up "Rot Z Up")
	      (rot-north-up "Rot North Up"))
	     :initial_value rot-z-up
	     :documentation "Orientation of Image")
	    (qbutton drag :text "DRAG")
	    (qlabel image-mag-label -text "MAGNIFY:" :anchor w
	     :documentation "Pick Image Magnification")
	    (qradiogroup image-mag :items
	     (list "Fit" "/16" "/8" "/4" "/2" "1" "2" "4")
	     :initial_value "1"
	     :buttonargs (list -indicatoron false)
	     :documentation "Pick Image Magnification")
	    (qlabel layers-label :text "LAYERS:" :anchor w)
	    (qlistbox layers :height 10 :scroll (list right)
	     ;;:selectmode extended
	     :selectmode multiple
	     :weight 1 -width ,control-column-width)
	    
	    (qframe ,right-frame)
	    (set_global default_parent (qframe ,toprow))
	    (qlabel pick-site-label :text "   SITE:")
	    (qoptionmenu pick-site )
	    (qlabel pick-image-label :text "  IMAGE:")
	    (qoptionmenu pick-image)

	    (qcanvas ,pane -width ,pane-width -height ,pane-height)

	    (qgrid_1_column ,controlcol)
	    (pack_children ,toprow -side left)
	    (pack ,toprow -side top -anchor nw -fill x)
	    (pack ,pane -side top -anchor nw -expand 1 -fill both)
	    (pack ,controlcol -side left -anchor nw -fill y)
	    (pack ,right-frame -side left -anchor nw -fill both -expand 1)
	    )))


    (tcl-eval 'option 'readfile
	      (tk::tcl-filename-filter "$FREEDIUS/lisp/tk/elt.app-defaults"))

    (if capture
	(capturing-tcl-script (tcl-script elt-panel-script))
	(progn
	  (tcl-script elt-panel-script)
	  (set-listbox-items (merge-widget-pathname controlcol "layers_frm.layers")
			     '("Roads" "Buildings" "Areas"))
	  ))))

(defun make-elt-panel4 (&key capture (pane-width 400) (pane-height 400)
			     (panel-name ".elt") (control-column-width 10))
  (let* (;;(right-frame (merge-widget-pathname toplevel-name "rf"))
	 ;;(toprow (merge-widget-pathname right-frame "toprow"))
	 ;;(pane (merge-widget-pathname right-frame "pane"))
	 (elt-app-defaults
	  (tk::tcl-filename-filter (or config::*cme-app-defaults-file* "$FREEDIUS/lisp/tk/elt.app-defaults")))
	 (logo-path (tk::tcl-filename-filter *sri-logo-path*))
	 (elt-panel-script
	  `((option readfile ,elt-app-defaults)
	    (proc elt-panel4 (toplevel)
	     (set pane_width ,pane-width)
	      (set pane_height ,pane-height)
	      (set cc_width ,control-column-width)
	      (qtoplevel $toplevel -class "Elt" -title "ELT Panel" )
	      (set toprow $toplevel.toprow)
	      (set right_frame $toplevel.rf)
	      (set pane $right_frame.pane)
	      (set cc $toplevel.cc)
	      (set_global default_parent (qframe $cc))
	      (qbuttonlist buttons -buttonargs (list -padx 0 -pady 0)
	       -items (list (list quit-panel "Quit")
		       (list update-panel "Update")
		       (list qtogglebutton lock -text "Lock")))
	      (image create photo sri_logo -file ,logo-path)
	      (qlabel sri-logo -image sri_logo)
	      (qtogglebutton 3d-centering -text "3D CENTERING" -documentation
	       "View change commands center image at 3d position in current view.")
	      (qtogglebutton auto-z-up -text "AUTO ORIENT" -initial_value 1
	       -documentation "When Image is first Selected, Rotate Z Up")
	      (qtogglebutton auto-zoom-to-fit -text "AUTO FIT"
	       -initial_value t -documentation
	       "When Image is first Selected, Zoom To Fit")
	      (qoptionmenu image-orientation -items
	       (list "Rot Default" "Rot 90" "Rot 180" "Rot 270" "Rot Z Up" "Rot North Up")
	       -initial_value "Rot Z Up" -documentation "Orientation of Image")
	      (qbutton drag -text "DRAG")
	      (qlabel image-mag-label -text "MAGNIFY:" -anchor w
	       -documentation "Pick Image Magnification")
	      (qradiogroup image-mag -items
	       ;; (list "Fit" "/16" "/8" "/4" "/2" "1" "2" "4")
	       (list (list fit "Fit") (list g4 "/16") (list g3 "/8")
		(list g2 "/4") (list g1 "/2") (list g0 "1") (list g-1 "2")
		(list g-2 "4"))
	       -initial_value g0 -buttonargs (list -indicatoron false)
	       -documentation "Pick Image Magnification")
	      (qlabel layers-label -text "LAYERS:" -anchor w)
	      (qlistbox layers -height 10 -scroll (list right)
	       ;;-selectmode extended
	       -selectmode multiple
	       -weight 1 -width $cc_width)
	      (qframe $right_frame -weight 1)
	      (set_global default_parent (qframe $toprow))
	      (qlabel pick-site-label -text "   SITE:")
	      (qoptionmenu pick-site )
	      (qlabel pick-image-label -text "  IMAGE:")
	      (qoptionmenu pick-image)
	      (qcanvas $pane -width $pane_width -height $pane_height -weight 1)

	      (qgrid_1_column $cc)
	      (pack_children $toprow -side left)
	      (qgrid_1_column $right_frame)
	      (grid $cc -column 0 -row 0 -sticky news)
	      (grid $right_frame -column 1 -row 0 -sticky news)
	      (grid rowconfigure $toplevel 0 -weight 1)
	      (grid columnconfigure $toplevel 1 -weight 1)
	      (return $toplevel)
	      )))
	 )

    (if capture
	(capturing-tcl-script (tcl-script elt-panel-script))
	(progn
	  (tcl-script elt-panel-script)
	  (let ((toplevel-name  (tcl-eval 'elt-panel4 panel-name)))
	    #+never
	    (set-listbox-items (merge-widget-pathname controlcol "layers_frm.layers")
			       '("Roads" "Buildings" "Areas"))
	    (setq *elt-panel* (make-widget-panel toplevel-name 'elt-callback
						 :panel-class 'gui::window-panel ; 'widget-panel
		     ))
	    )))))

(defun load-logo-image (image-name image-path)
  (if (< (read-from-string (tcl-cmd `(lsearch :exact (image names) ,image-name))) 0)
      (tcl-cmd `(image create photo ,image-name :file ,(tk::tcl-filename-filter image-path)))))

(defun make-elt-panel-tcl-proc ()
  (let* ((elt-panel-script
	  `((proc init_elt_panels (app_defaults_path image_name image_path )
	     (option readfile $app_defaults_path)
	     (image create photo $image_name -file $image_path))	     
	    (proc default_elt_panel (toplevel)
	     (init_elt_panels ,(tk::tcl-filename-filter "$FREEDIUS/lisp/tk/elt.app-defaults")
	      sri_logo ,*sri-logo-path*)
	     (elt-panel  $toplevel 400 400 10 sri_logo))
	     
	    (proc elt-panel (toplevel pane_width pane_height cc_width logo_image)
	     (qtoplevel $toplevel -class "Elt" -title "ELT Panel" )
	     (set_global default_parent (set cc (qframe cc :parent $toplevel)))
	     (qbuttonlist buttons -buttonargs (list -padx 0 -pady 0)
	      -items (list (list quit-panel "Quit")
		      (list update-panel "Update")
		      (list qtogglebutton lock -text "Lock")))
	     (qlabel sri-logo -image $logo_image)
	     (qtogglebutton 3d-centering -text "3D CENTERING" -documentation
	      "View change commands center image at 3d position in current view.")
	     (qtogglebutton auto-z-up -text "AUTO ORIENT" -initial_value 1
	      -documentation "When Image is first Selected, Rotate Z Up")
	     (qtogglebutton auto-zoom-to-fit -text "AUTO FIT"
	      -initial_value t -documentation
	      "When Image is first Selected, Zoom To Fit")
	     (qoptionmenu image-orientation -items
	       (list "Rot Default" "Rot 90" "Rot 180" "Rot 270" "Rot Z Up" "Rot North Up")
	       -initial_value "Rot Z Up" -documentation "Orientation of Image")
	     (qbutton drag -text "DRAG")
	     (qradiogroup image-mag -items
	      (list (list fit "Fit") (list g4 "/16") (list g3 "/8")
	       (list g2 "/4") (list g1 "/2") (list g0 "1") (list g-1 "2")
	       (list g-2 "4"))
	      -initial_value g0 -buttonargs (list -indicatoron false)
	      -labeltext "MAGNIFY:" -labeldoc "Pick Image Magnification")
	     (qlistbox layers -labeltext "LAYERS:" -labeldoc "Site Model Feature Sets"
	      -height 10 -scroll (list right)
	      ;;-selectmode extended
	      -selectmode multiple
	      -weight 1 -width $cc_width)
	     (set rf (qframe right_frame :parent $toplevel -weight 1))
	     (set_global default_parent (set toprow (qframe toprow :parent $rf)))
	     (qlabel pick-site-label -text "   SITE:")
	     (qoptionmenu pick-site)
	     (qlabel pick-image-label -text "  IMAGE:")
	     (qoptionmenu pick-image)
	     (qcanvas pane :parent $rf -width $pane_width -height $pane_height -weight 1)

	     (qgrid_1_column $cc)
	     (pack_children $toprow -side left)
	     (qgrid_1_column $rf)
	     (grid $cc -column 0 -row 0 -sticky news)
	     (grid $rf -column 1 -row 0 -sticky news)
	     (grid rowconfigure $toplevel 0 -weight 1)
	     (grid columnconfigure $toplevel 1 -weight 1)
	     (return $toplevel)
	     )
	    )))
    (tcl-script elt-panel-script)
    ))


(defun make-elt-panel (&key  (panel-name ".elt2")
			     (pane-width 400) (pane-height 400)
			     (control-column-width 10)
			     (logo-image 'sri_logo)
			     (logo-path *sri-logo-path*)
			     )
  (if (equal (tcl-cmd `(winfo exists ,panel-name)) "1")
      (pop-up-panel panel-name)
      (progn
	(when (equal "-1" (tcl-cmd `(lsearch -exact (image names) ,logo-image)))
	  (tcl-cmd `(image create photo ,logo-image -file ,logo-path)))
	(let ((toplevel-name
	       (tcl-eval 'elt-panel panel-name pane-width pane-height
			 control-column-width logo-image)))
	  (set-listbox-items (widget-named toplevel-name "layers")
			     '("Roads" "Buildings" "Areas"))
	  (make-widget-panel toplevel-name 'elt-callback
			     :panel-class 'gui::window-panel ; 'widget-panel
		     ))
	)))
    


#|
(tcl-cmd '(option readfile "$FREEDIUS/lisp/tk/elt.app-defaults"))
(time (make-elt-panel-tcl-proc))
(time (make-elt-panel ))		; .22 secs
(time (item-alist-from-widget-tree ".elt")) ; .06 secs
(make-elt-panel :panel-name ".elt2"))

(write-tcl-script "~/tmp/elt-panel.tcl"
		  (capturing-tcl-script (make-elt-panel-tcl-proc)))

(tcl-cmd '(source "~/tmp/elt-panel.tcl"))))
(time (tcl-cmd '(default_elt_panel ".elt"))) ; .16 sec

(pprint (setq script (MAKE-ELT-PANEL2 :capture t)))

(loop for item in (item-alist-from-widget-tree ".elt")
      do (print item))


(widget-clidget ".elt")
(make-elt-panel4)
(tcl-eval 'elt-panel4 ".elt")
(tcl-eval 'elt-panel4 ".elt2")
(make-elt-panel2 :control-column-width 8)
(set-listbox-items (merge-widget-pathname ".elt.cc" "layers_frm.layers")
		     '("Roads" "Buildings" "Areassfddfdfdfdfdfdfdfdfd"))
(tcl-eval "." 'configure)
(tcl-eval '.elt 'configure)
(tcl-eval '.elt.rf 'configure)
(tcl-eval 'pack 'info '.elt.rf)
(tcl-eval 'pack 'propagate '.elt)
(tcl-eval 'pack 'propagate '.elt.rf)
(tcl-eval  '.elt.rf 'configure :background 'green)
(tcl-eval  '.elt.rf.pane 'configure)
(tcl-eval '.elt.rf.pane 'configure :background 'gray70)
(tcl-eval '.elt.controlcol.3d-centering  'configure)
(tcl-eval '.elt.controlcol.3d-centering  'configure "-selectcolor" 'red)
(tcl-eval '.elt.controlcol.image-orientation  'configure )
(tcl-list-to-strings (tcl-eval 'winfo 'children ".elt"))
(tcl-eval 'get_global ".elt.cc.image-mag")
(tcl-eval ".elt.cc.image-mag.g0" 'configure)
(tcl-eval 'widget_name "foo")

(widget-value 'layers *elt-panel*)
(widget-named *elt-panel* 'layers)
(tcl-eval (widget-named *elt-panel* 'pick-site) 'configure)
(tcl-eval 'source "/homedir/quam/cp/tk/library/tk-utils.tcl")
|#




(defun make-quanset-panel-tcl-proc ()
  (let ((script
	 `((proc quanset_panel (toplevel width)
	    (option readfile ,(tk::tcl-filename-filter "$FREEDIUS/lisp/tk/object-cvv.app-defaults"))
	    (set_global default_parent
	     (qtoplevel $toplevel -class "Obj" -title "Quanset Panel" ))
	    (qbuttonlist panel_controls
	     -buttonargs (list -padX 5 -borderwidth 0 -highlightthickness 0)
	     -items (list (list quit-panel "Quit")
		     (list update-panel "Update")
		     (list qtogglebutton lock -text "Lock"
			   -borderwidth 0 -highlightthickness 0)))
	    (qseparator sep -height 3)

	    (qentry name -labeltext "Name:" )
	    (qbutton edit-graphics-style -text "Edit Graphics Style")

	    (qgroupbutton annotation -text "ANNOTATION:  "
	     -group_widgets (list DESCRIPTION))
	    (qtext description -labeltext "Description:" -height 3 -width $width
	     -scroll (list right)
	     -weight 1000 -minheight 30
	     )
	    ;(qseparator gripsep -height 2 -bg black)
	    
	    (qgroupbutton hierarchy -text "HIERARCHY:  "
	     -group_widgets (list change-superior feature-sets)
	     :DOCUMENTATION "Open or Close the Menu Items for Object HIERARCHY")
	    (qlistbox change-superior -labeltext "Superior:" -height 3)
	    (qlistbox feature-sets -labeltext "Feature Sets:" -height 5
	     -selectmode multiple
	     -weight 1000 -minheight 40
	     )
	    
	    (qgroupbutton position -text "LOCATION:  "
	     -group_widgets (list world coord-mode units x y z last-selected-handle)
	     :DOCUMENTATION "Open or Close the Menu Items for Object LOCATION")
	    (qlabel world )
	    (qoptionmenu coord-mode -labeltext "Coord Sys:"
	     -items (list "Local X Y Z          " ))
	    (qradiogroup units -labeltext "Local Units:"
	     -items (list (list FEET "Feet") (list METERS "Meters") (list NIL "?"))
	     -initial_value METERS -buttonargs (list -indicator false))
	    (qentry x -labeltext "X:" )
	    (qentry y -labeltext "Y:" )
	    (qentry z -labeltext "Z:" )
	    (qinteger last-selected-handle -labeltext "Vert #:"
	     -labeldoc "Selected Vertex Number")

	    (qgroupbutton GEOMETRY -text "GEOMETRY:  "
	     -group_widgets (list X-SIZE Y-SIZE Z-SIZE)
	     :DOCUMENTATION "Open or Close the Menu Items for Object GEOMETRY")

	    (qfloat X-SIZE -labeltext "X Size:")
	    (qfloat Y-SIZE -labeltext "Y Size:")
	    (qfloat Z-SIZE -labeltext "Z Size:")

	    (qgroupbutton MISC -text "MISC:  "
	     -group_widgets (list OPEN-FOR-VERTEX-MODIFICATION)
	     :DOCUMENTATION "Open or Close the Miscellaneous Object Items")
	    
	    (qradiogroup OPEN-FOR-VERTEX-MODIFICATION -labeltext "Verts Open:"
	     -buttonargs (list -indicator false)
	     -items (list (list t "Yes") (list nil "No"))
	     -initial_value nil
	     -labeldoc "Vertices are Modifiable")
	    (qframe pad-bottom -weight 1)
	    (qgrid_2_column $toplevel -pady 1)
	    (return $toplevel)
	    ))))
    (tcl-script script)
    ))


;; non-grip version
(defun make-quanset-panel-tcl-proc ()
  (let ((script
	 `((proc quanset_panel (toplevel width)
	    (option readfile ,(tk::tcl-filename-filter "$FREEDIUS/lisp/tk/object-cvv.app-defaults"))
	    (qtoplevel $toplevel -class "Obj" -title "Quanset Panel" )
	    (set_global default_parent (set frm (qframe $toplevel.frm)))
	    (qbuttonlist panel_controls
	     -buttonargs (list -padX 5 -borderwidth 0 -highlightthickness 0)
	     -items (list (list quit-panel "Quit")
		     (list update-panel "Update")
		     (list qtogglebutton lock -text "Lock"
			   -borderwidth 0 -highlightthickness 0)))
	    (qseparator sep -height 3)

	    (qentry name -labeltext "Name:" )
	    (qbutton edit-graphics-style -text "Edit Graphics Style")

	    (qgroupbutton annotation -text "ANNOTATION:  "
	     -group_widgets (list DESCRIPTION))
	    (qtext description -labeltext "Description:" -height 3 -width $width
	     -scroll (list right)
	     -weight 1000 -minheight 30
	     )
	    ;(qseparator gripsep -height 2 -bg black)
	    
	    (qgroupbutton hierarchy -text "HIERARCHY:  "
	     -group_widgets (list change-superior feature-sets)
	     :DOCUMENTATION "Open or Close the Menu Items for Object HIERARCHY")
	    (qlistbox change-superior -labeltext "Superior:" -height 3)
	    (qlistbox feature-sets -labeltext "Feature Sets:" -height 5
	     -selectmode multiple
	     -weight 1000 -minheight 40
	     )
	    
	    (qgroupbutton position -text "LOCATION:  "
	     -group_widgets (list world coord-mode units x y z last-selected-handle)
	     :DOCUMENTATION "Open or Close the Menu Items for Object LOCATION")
	    (qlabel world )
	    (qoptionmenu coord-mode -labeltext "Coord Sys:"
	     -items (list "Local X Y Z          " ))
	    (qradiogroup units -labeltext "Local Units:"
	     -items (list (list FEET "Feet") (list METERS "Meters") (list NIL "?"))
	     -initial_value METERS -buttonargs (list -indicator false))
	    (qentry x -labeltext "X:" )
	    (qentry y -labeltext "Y:" )
	    (qentry z -labeltext "Z:" )
	    (qinteger last-selected-handle -labeltext "Vert #:"
	     -labeldoc "Selected Vertex Number")

	    (qgroupbutton GEOMETRY -text "GEOMETRY:  "
	     -group_widgets (list X-SIZE Y-SIZE Z-SIZE)
	     :DOCUMENTATION "Open or Close the Menu Items for Object GEOMETRY")

	    (qfloat X-SIZE -labeltext "X Size:")
	    (qfloat Y-SIZE -labeltext "Y Size:")
	    (qfloat Z-SIZE -labeltext "Z Size:")

	    (qgroupbutton MISC -text "MISC:  "
	     -group_widgets (list OPEN-FOR-VERTEX-MODIFICATION)
	     :DOCUMENTATION "Open or Close the Miscellaneous Object Items")
	    
	    (qradiogroup OPEN-FOR-VERTEX-MODIFICATION -labeltext "Verts Open:"
	     -buttonargs (list -indicator false)
	     -items (list (list t "Yes") (list nil "No"))
	     -initial_value nil
	     -labeldoc "Vertices are Modifiable")
	    (qframe pad-bottom -weight 1)
	    (qgrid_2_column $frm -pady 1)
	    (pack $frm -expand true -fill both)
	    (update)
	    (return $toplevel)
	    ))))
    (tcl-script script)
    ))

(defun make-quanset-panel-tcl-proc-grips2 ()
  (let ((script
	 `((proc quanset_panel (toplevel width)
	    (option readfile ,(tk::tcl-filename-filter "$FREEDIUS/lisp/tk/object-cvv.app-defaults"))
	    (qtoplevel $toplevel -class "Obj" -title "Quanset Panel" )
	    (wm resizable $toplevel 1 0)
	    (set_global default_parent (set frm (qframe $toplevel.frm)))
	    (qbuttonlist panel_controls
	     -buttonargs (list -padX 5 -borderwidth 0 -highlightthickness 0)
	     -items (list (list quit-panel "Quit")
		     (list update-panel "Update")
		     (list qtogglebutton lock -text "Lock"
			   -borderwidth 0 -highlightthickness 0)))
	    (qseparator sep -height 3)

	    (set_global default_parent (qframe prologfrm -parent $frm ))
	    (qentry name -labeltext "Name:" )
	    ;;(qbutton edit-graphics-style -text "Edit Graphics Style")
	    (qbuttonlist buttons -items
	     (list (list edit-graphics-style "Edit Graphics Style")))
	    (qgrid_2_column (get_global default_parent) -pady 1)
	    
	    (qgroupbutton annotation -parent $frm -text "ANNOTATION:  "
	     -group_widgets (list annofrm))
	    (set_global default_parent
	     (set annofrm (qframe annofrm -parent $frm )))
	    (qtext description -labeltext "Description:" -height 3 -width $width
	     -scroll (list right)
	     -weight 1000 -minheight 30
	     )
	    ;;(qseparator gripsep -height 2 -bg black)
	    (qgrip (get_global default_parent))
	    (qgrid_2_column (get_global default_parent) -pady 1)
	    
	    (qgroupbutton hierarchy -parent $frm -text "HIERARCHY:  "
	     -group_widgets (list hierfrm)
	     :DOCUMENTATION "Open or Close the Menu Items for Object HIERARCHY")
	    (set_global default_parent (qframe hierfrm -parent $frm ))
	    (qlistbox change-superior -labeltext "Superior:" -height 3)
	    (qlistbox feature-sets -labeltext "Feature Sets:" -height 5
	     -selectmode multiple
	     -weight 1000 -minheight 40
	     )
	    (qgrip (get_global default_parent))
	    (qgrid_2_column (get_global default_parent) -pady 1)
	    
	    (set_global default_parent (qframe posfrm -parent $frm ))
	    (qgroupbutton position -text "LOCATION:  "
	     -group_widgets (list world coord-mode units x y z last-selected-handle)
	     :DOCUMENTATION "Open or Close the Menu Items for Object LOCATION")
	    (qlabel world )
	    (qoptionmenu coord-mode -labeltext "Coord Sys:"
	     -items (list "Local X Y Z          " ))
	    (qradiogroup units -labeltext "Local Units:"
	     -items (list (list FEET "Feet") (list METERS "Meters") (list NIL "?"))
	     -initial_value METERS -buttonargs (list -indicator false))
	    (qentry x -labeltext "X:" )
	    (qentry y -labeltext "Y:" )
	    (qentry z -labeltext "Z:" )
	    (qinteger last-selected-handle -labeltext "Vert #:"
	     -labeldoc "Selected Vertex Number")
	    (qgrid_2_column (get_global default_parent) -pady 1)
	    
	    (set_global default_parent (qframe geofrm -parent $frm ))
	    (qgroupbutton GEOMETRY -text "GEOMETRY:  "
	     -group_widgets (list X-SIZE Y-SIZE Z-SIZE)
	     :DOCUMENTATION "Open or Close the Menu Items for Object GEOMETRY")

	    (qfloat X-SIZE -labeltext "X Size:")
	    (qfloat Y-SIZE -labeltext "Y Size:")
	    (qfloat Z-SIZE -labeltext "Z Size:")
	    (qgrid_2_column (get_global default_parent) -pady 1)
	    
	    (set_global default_parent (qframe miscfrm -parent $frm ))
	    (qgroupbutton MISC -text "MISC:  "
	     -group_widgets (list OPEN-FOR-VERTEX-MODIFICATION)
	     :DOCUMENTATION "Open or Close the Miscellaneous Object Items")
	    
	    (qradiogroup OPEN-FOR-VERTEX-MODIFICATION -labeltext "Verts Open:"
	     -buttonargs (list -indicator false)
	     -items (list (list t "Yes") (list nil "No"))
	     -initial_value nil
	     -labeldoc "Vertices are Modifiable")
	    (qframe pad-bottom -weight 1)
	    (qgrid_2_column (get_global default_parent) -pady 1)
	    
	    ;;(qgrid_1_column $frm -pady 1)
	    (qgrid_1_column $frm)
	    (pack $frm -expand true -fill both)
	    (manage_frame_grips $frm 1)
	    (return $toplevel)
	    ))))
    (tcl-script script)
    ))

;;;; fully gripped
(defun make-quanset-panel-tcl-proc-grips ()
  (let ((script
	 `((proc quanset_panel (toplevel width)
	    (option readfile ,(tk::tcl-filename-filter "$FREEDIUS/lisp/tk/object-cvv.app-defaults"))
	    (qtoplevel $toplevel -class "Obj" -title "Quanset Panel" )
	    (wm resizable $toplevel 1 0)
	    (set_global default_parent (set frm (qframe $toplevel.frm)))
	    (qbuttonlist panel_controls
	     -buttonargs (list -padX 5 -borderwidth 0 -highlightthickness 0)
	     -items (list (list quit-panel "Quit")
		     (list update-panel "Update")
		     (list qtogglebutton lock -text "Lock"
			   -borderwidth 0 -highlightthickness 0)))
	    (qseparator sep -height 3)

	    (set_global default_parent (qframe prologfrm -parent $frm ))
	    (qentry name -labeltext "Name:" )
	    ;;(qbutton edit-graphics-style -text "Edit Graphics Style")
	    (qbuttonlist buttons -items
	     (list (list edit-graphics-style "Edit Graphics Style")))
	    (qgrid_2_column (get_global default_parent) -pady 1)
	    
	    (set annobut (qbutton annotation -parent $frm -text "ANNOTATION:  "))
	    (set_global default_parent
	     (set annofrm (qframe annofrm -parent $frm )))
	    (qtext description -labeltext "Description:" -height 3 -width $width
	     -scroll (list right)
	     -weight 1000 -minheight 30
	     )
	    ;;(qseparator gripsep -height 2 -bg black)
	    (set annogrip (qgrip (get_global default_parent) -button $annobut))
	    (qgrid_2_column (get_global default_parent) -pady 1)
	    
	    (set hierbut (qbutton hierarchy -parent $frm -text "HIERARCHY:  "
			  :DOCUMENTATION
			  "Open or Close the Menu Items for Object HIERARCHY"))
	    (set_global default_parent (set hierfrm (qframe hierfrm -parent $frm )))
	    (qlistbox change-superior -labeltext "Superior:" -height 3)
	    (qlistbox feature-sets -labeltext "Feature Sets:" -height 5
	     -selectmode multiple
	     -weight 1000 -minheight 40
	     )
	    (set hiergrip (qgrip (get_global default_parent) -button $hierbut))
	    (qgrid_2_column (get_global default_parent) -pady 1)
	    
	    (set_global default_parent (qframe posfrm -parent $frm ))
	    (set posbut (qbutton position -parent $frm -text "LOCATION:  "
			 :DOCUMENTATION
			 "Open or Close the Menu Items for Object LOCATION"))
	    (qlabel world )
	    (qoptionmenu coord-mode -labeltext "Coord Sys:"
	     -items (list "Local X Y Z          " ))
	    (qradiogroup units -labeltext "Local Units:"
	     -items (list (list FEET "Feet") (list METERS "Meters") (list NIL "?"))
	     -initial_value METERS -buttonargs (list -indicator false))
	    (qentry x -labeltext "X:" )
	    (qentry y -labeltext "Y:" )
	    (qentry z -labeltext "Z:" )
	    (qinteger last-selected-handle -labeltext "Vert #:"
	     -labeldoc "Selected Vertex Number")
	    (qgrip (get_global default_parent) -button $posbut)
	    (qgrid_2_column (get_global default_parent) -pady 1)
	    
	    (set_global default_parent (qframe geofrm -parent $frm ))
	    (set geobut (qbutton GEOMETRY -parent $frm -text "GEOMETRY:  "
			 :DOCUMENTATION "Open or Close the Menu Items for Object GEOMETRY"))

	    (qfloat X-SIZE -labeltext "X Size:")
	    (qfloat Y-SIZE -labeltext "Y Size:")
	    (qfloat Z-SIZE -labeltext "Z Size:")
	    (qgrip (get_global default_parent) -button $geobut)
	    (qgrid_2_column (get_global default_parent) -pady 1)
	    
	    (set_global default_parent (qframe miscfrm -parent $frm ))
	    (set miscbut (qbutton MISC -parent $frm -text "MISC:  "
			  :DOCUMENTATION "Open or Close the Miscellaneous Object Items"))
	    
	    (qradiogroup OPEN-FOR-VERTEX-MODIFICATION -labeltext "Verts Open:"
	     -buttonargs (list -indicator false)
	     -items (list (list t "Yes") (list nil "No"))
	     -initial_value nil
	     -labeldoc "Vertices are Modifiable")
	    (qgrip (get_global default_parent) -button $miscbut)
	    (qgrid_2_column (get_global default_parent) -pady 1)
	    
	    (qgrid_1_column $frm)
	    (pack $frm -expand true -fill both)
	    (grid_uniform_column0_width $frm)
	    (initialize_grips2 $frm (list $annogrip $annofrm $hiergrip $hierfrm))
	    (return $toplevel)
	    ))))
    (tcl-script script)
    ))


(defun make-quanset-panel (&key  (panel-name ".quanset") (width 20))
  (if (equal (tcl-cmd `(winfo exists ,panel-name)) "1")
      (pop-up-panel panel-name)
      (progn
	(let ((toplevel-name
	       (tcl-eval 'quanset_panel panel-name width)))
	  (make-widget-panel toplevel-name 'elt-callback
			     ;; 'elt-callback is bogus here
			     ;;  should be a callback for object panels
			     :panel-class 'widget-panel
		     ))
	)))


#|
(tcl-cmd '(source /homedir/quam/cp/tk/library/layout.tcl))
(tcl-cmd '(source /homedir/quam/cp/tk/library/group-control.tcl))
(tcl-cmd '(source /homedir/quam/cp/tk/library/qwidgets.tcl))
(tcl-cmd '(source /homedir/quam/cp/tk/library/composite-widgets.tcl))
(make-quanset-panel-tcl-proc)
(make-quanset-panel-tcl-proc-grips)
(time (make-quanset-panel)) ; 0.24 secs for non-grip version

(make-quanset-panel-tcl-proc-grips)
(time (make-quanset-panel)) ; 0.31 secs for grip version
  
(time (tcl-cmd '(grid_uniform_column0_width .quanset.frm)))
(time (tcl-script '((initialize_grips .quanset.frm) (update)))); .09 secs
(time (tcl-script '((initialize_grips .quanset.frm) (update)))); .09 secs
(write-tcl-script "~/tmp/quanset-panel.tcl"
		  (capturing-tcl-script (make-quanset-panel-tcl-proc)))

(tcl-cmd '(.quanset.position configure -highlightthickness))
(tcl-cmd '(winfo class .quanset.position ))
(tcl-cmd '(.quanset.coord-mode_label configure -highlightthickness))
(tcl-cmd '(.quanset.coord-mode configure ))
(tcl-cmd '(winfo class .quanset.coord-mode))
(tcl-cmd '(winfo class .quanset.units))
(tcl-cmd '(.quanset.units configure))
(tcl-cmd '(.quanset.coord-mode configure -highlightthickness 0))
(tcl-cmd '(.quanset.description_frm.description_yscroll configure))

(tcl-cmd '(.quanset.description_frm configure -height 400))
(tcl-cmd '(.quanset.description_frm configure))
;;; this next works until the toplevel is resized, then has no effect
(tcl-cmd '(.quanset.description_frm.description configure -height 3))
(tcl-cmd '(.quanset.description_frm.description configure))
(tcl-cmd '(grid info .quanset.description_frm))
(tcl-cmd '(grid rowconfigure .quanset 5))
(tcl-cmd '(grid rowconfigure .quanset 5 -minsize 40))

(tcl-cmd '(grid info .quanset.feature-sets))
(tcl-cmd '(grid rowconfigure .quanset 9 -minsize 0))
(tcl-cmd '(.quanset.description_frm.description cget -font))
(tcl-cmd '(font metrics (.quanset.description_frm.description cget -font) -linespace))

(tcl-cmd '(qframe .quanset.grip -width 6 -height 6 -bg black))
(tcl-cmd '(winfo y .quanset.gripsep))
(tcl-cmd '(place .quanset.grip -in .quanset -relx .1 -y (expr (winfo y .quanset.gripsep) -1)
	   -anchor center))
(tcl-cmd '(destroy .quanset.grip))

(set-listbox-items ".quanset.feature-sets" '(a b c))

(tcl-cmd '(grid info .quanset.gripsep))
(tcl-cmd '(grid rowconfigure .quanset 6))
(tcl-cmd '(.quanset.gripsep configure))

(tcl-cmd '(putprop (widget_named .quanset description_frm) widget_weight 100))
(tcl-cmd '(putprop (widget_named .quanset feature-sets) widget_weight 1000))
(tcl-cmd '(qgrid_2_column (winfo parent (widget_named .quanset description_frm)) -pady 1))

(tcl-cmd '(putprop (widget_named .quanset description_frm) widget_weight 1000))
(tcl-cmd '(putprop (widget_named .quanset feature-sets) widget_weight 100))
(tcl-cmd '(qgrid_2_column (winfo parent (widget_named .quanset description_frm)) -pady 1))
(tcl-cmd '(widget_named .quanset description_frm))

(tcl-cmd '( non_slaved_children .quanset))
(tcl-cmd '(getp .quanset.doc_widget doc_widget_p xxx))
(tcl-cmd '(puts xxx))
(tcl-cmd '(winfo class .quanset.doc_widget))

(tcl-cmd '(pack .quanset.frm -expand true -fill both))
(tcl-cmd '(pack propagate .quanset false))
(tcl-cmd '(pack propagate .quanset true))
(tcl-cmd '(.quanset configure -height 800 -width 300))
(tcl-cmd '(.quanset configure -height 400 -width 200))

(tcl-cmd '(pack propagate .quanset false))
(tcl-cmd '(.quanset.frm configure -height 800))

(tcl-cmd '(pack propagate .quanset false))
(tcl-cmd '(grid propagate .quanset.frm false))
(tcl-cmd '(grid propagate .quanset.frm.annofrm false))
(tcl-cmd '(.quanset.frm.annofrm configure -height 100))
(tcl-cmd '(.quanset.frm.annofrm configure -height 60))

(tcl-cmd '(grid propagate .quanset.frm.hierfrm false))
(tcl-cmd '(.quanset.frm.hierfrm configure -height 100))



|#





(defun make-cme-control-panel ()
  (let* ((toplevel-name (gensym-toplevel-widget-path ".cme"))
	 (menu-bar (merge-widget-pathname toplevel-name "mb"))
	 (frm (merge-widget-pathname toplevel-name "f"))
	 (doc (merge-widget-pathname frm "doc"))
	 (control-panel-app-defaults
	  (tcl-filename-filter (tk::tcl-filename-filter "$FREEDIUS/lisp/tk/cme.app-defaults")))
	 (file_menu_items ''((load_image "Load Image")))
	 (view_menu_items '())
	 (site_menu_items '())
	 (image_menu_items '())
	 (script
	  `((option readfile ,control-panel-app-defaults)
	    (qtoplevel ,toplevel-name :class "Cme" :title "CME Control Panel" )
	    (qframe mb -parent ,toplevel-name)
	    (set_global default_parent ,menu-bar)
	    (qpulldownmenu file -text "File" -underline 0 -items ,file_menu_items)
	    (qpulldownmenu view -text "View" -underline 0 -items ,view_menu_items)
	    (qpulldownmenu site -text "Site" -underline 0 -items ,site_menu_items)
	    (qpulldownmenu image -text "Image" -underline 0 -items ,image_menu_items)
	    (qframe f -parent ,toplevel-name)
	    ;;(qentry doc -parent ,frm -borderwidth 0 -highlightthickness 0 -state disabled)
	    (qlabel doc -parent ,frm -anchor nw -borderwidth 0 -highlightthickness 0 )
	    ;;(pack_children ,menu-bar -side left -expand true -fill both )
	    (pack_children ,menu-bar -side left -fill x -anchor nw)
	    (pack ,frm -side top -anchor nw)
	    (pack ,menu-bar -side top -anchor nw)
 	    (pack ,doc -side left  -expand true  -fill x -anchor nw)
	    ;;(pack_children ,frm -side left -expand true -fill both )
	    )))
    (tcl-script script)
    (make-widget-panel toplevel-name 'elt-callback
		       :panel-class 'gui::cvv-panel
		       )))

(defvar *cme-control-panel*)

(defun set-doc-string (window string)
  (declare (ignore window))
  (let ((panel *cme-control-panel*))
    (setf (cvv-item-value (get-named-item panel 'doc)) string)))

(defun set-bucky-doc (window doc)
  (declare (ignore window))
  (let ((panel *cme-control-panel*)
	(string (if (stringp doc)
		    doc
		    (format nil "~{ ~a~20T ~}" doc))))
    
    (setf (cvv-item-value (get-named-item panel 'doc)) string)))

(defun set-bucky-doc (window doc)
  (declare (ignore window))
  (let ((panel *cme-control-panel*)
	(string (if (stringp doc)
		    doc
		    (loop with str = ""
			  with field-width = 20
			  ;;with spaces = (make-string field-width :initial-element #\space)
			  for s = (pop doc)
			  while s
			  for padding = (make-string (- field-width (length s)) :initial-element #\space)
			  when t ;doc 
			  do (setq str (string-append str s padding))
			  else do (setq str (string-append str s))
			  finally (return str)))))
    
    (setf (cvv-item-value (get-named-item panel 'doc)) string)))




#|
(setq *cme-control-panel* (make-cme-control-panel))

(cvv-item-value (get-named-item *cme-control-panel* 'doc))
(get-named-item *cme-control-panel* 'doc)
(setf (cvv-item-value (get-named-item *cme-control-panel* 'doc)) "Foobar is the name of crap")

(describe (gui::panel (gui::view-window (gui::top-view))))

(set-doc-string (gui::view-window (gui::top-view)) "This is it!")
(set-bucky-doc (gui::view-window (gui::top-view)) '("One" "Two" "Three"))
(tcl-cmd `(,(get-named-item *cme-control-panel* 'doc) cget -width))
(tcl-cmd `(,(get-named-item *cme-control-panel* 'doc) configure))
|#
