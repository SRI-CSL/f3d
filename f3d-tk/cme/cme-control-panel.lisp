(in-package :gui)

(eval-when (eval load compile)
(import '(tk::gensym-toplevel-widget-path tk::merge-widget-pathname
	  tk::recompute-doc-strings))
)

#|
This file still contains lots of calls to functions in the CME package.

Components of cme-control-panel:

menu-bar (file edit view ... help)

doc-line - shared for bucky doc and other command doc

controls - tandem, sensitive, hide


CME::SELECT-CME-SITE is the only dependency on the cme subsystem
|#

;;; Used for directory browsers.   
(defclass raw-arguments-cvv-panel (tk::cvv-panel) ())

(defclass cme-control-panel (tk::cvv-panel) ())


;;; leave the tcl/tk return arguments alone.
(defmethod tk::process-callback-args ((panel raw-arguments-cvv-panel) args)
  args)

;;; *************************   CME CONTROL PANEL  *************************

(defvar *CME-CONTROL-PANEL* nil)

;;; From CME
;;;(defparameter *object-creation-menu-item-list-of-columns*
;;;  `((("3d Composite" :eval (create-and-add-object
;;;                            '(create-and-add-3d-object 3d-composite-object)))
;;;     ("3d Text" :eval (create-and-add-object '(create-and-add-3d-object 3d-text-object)))
;;;     ("3d Crosshair" :eval (create-and-add-object '(create-and-add-3d-object 3d-crosshair-object)))
;;;     ("3d Point" :eval (create-and-add-object '(create-and-add-3d-object 3d-point-object)))
;;;
;;;     ("Open 3d Curve" :eval (create-and-add-object '(create-and-add-3d-object 3d-curve)))
;;;     ("Closed 3d Curve" :eval (create-and-add-object '(create-and-add-3d-object 3d-closed-curve)))
;;;     ("3d Network" :eval (create-and-add-object '(create-and-add-3d-object 3d-network)))
;;;     ("3d Ruler" :eval (create-and-add-object '(create-and-add-3d-object 3d-ruler-object)))
;;;
;;;     ;;("Flight Path" :eval (create-and-add-object '(create-and-add-3d-object flight-path)))
;;;                                        ;("Network" :eval (create-and-add-object '(create-and-add-3d-object we-network)))
;;;     ("3d Ribbon" :eval (create-and-add-object '(create-and-add-3d-object 3d-ribbon-curve)))
;;;     ;;("3d Ribbon Net" :eval (create-and-add-object '(create-and-add-3d-object 3d-ribbon-network)))
;;;     ("3d Curtain" :eval (create-and-add-object '(create-and-add-3d-object 3d-curtain)))
;;;                 
;;;     ;;("Feature Set" :eval (create-and-add-object '(create-and-add-composite-object 3d-feature-set)))
;;;
;;;     ;;("" :no-select nil)
;;;     ("" :separator nil)
;;;            
;;;     ("Box" :eval (create-and-add-object '(create-and-add-3d-object cube-object)))
;;;     ;;("Building" :eval (create-and-add-object '(create-and-add-3d-object building-object)))
;;;     ("House" :eval (create-and-add-object '(create-and-add-3d-object house-object)))
;;;     ("Extrusion" :eval (create-and-add-object '(create-and-add-3d-object extruded-object )))
;;;     ("Cylinder" :eval (create-and-add-object '(create-and-add-3d-object cylinder)))
;;;     ("Quonset" :eval (create-and-add-object '(create-and-add-3d-object half-cylinder)))
;;;     ("Superellipse" :eval (create-and-add-object '(create-and-add-3d-object superellipse)))
;;;     ("Superquadric" :eval (create-and-add-object '(create-and-add-3d-object superquadric)))
;;;           
;;;     ("DTM Quad Mesh" :eval (create-and-add-object '(create-and-add-3d-object quad-grid-dtm-object)))
;;;     ("DTM Tri Mesh" :eval (create-and-add-object '(create-and-add-3d-object tri-grid-dtm-object)))
;;;     ("Contour Map" :eval (com-create-contour-map))
;;;     ("Map Grid" :eval (com-create-map-grid))
;;;                 
;;;     )
;;;    ;; column 2
;;;    (("2d Composite" :eval (create-and-add-object '(create-and-add-2d-object 2d-composite-object)))
;;;     ("2d Text" :eval (create-and-add-object '(create-and-add-2d-object 2d-text-object)))
;;;     ("2d Crosshair" :eval (create-and-add-object '(create-and-add-2d-object 2d-crosshair-object)))
;;;     ("2d Point" :eval (create-and-add-object '(create-and-add-2d-object 2d-point-object)))
;;;     ("Open 2d Curve" :eval (create-and-add-object '(create-and-add-2d-object 2d-curve)))
;;;     ("Closed 2d Curve" :eval (create-and-add-object '(create-and-add-2d-object closed-2d-curve)))
;;;     ("2d Network" :eval (create-and-add-object '(create-and-add-2d-object 2d-network)))
;;;     ("2d Ruler" :eval (create-and-add-object '(create-and-add-2d-object 2d-ruler-object)))
;;;
;;;     ("2d Ribbon" :eval (create-and-add-object '(create-and-add-2d-object 2d-ribbon-curve)))
;;;     ("Image Window" :eval (create-and-add-object
;;;                            '(create-and-add-2d-object image-windowing-tool)))
;;;
;;;     ;;("" :no-select nil)
;;;     ("" :separator nil)
;;;     ("Window Text" :eval (create-and-add-object '(create-and-add-window-object window-text-object)))
;;;                                 
;;;
;;;     ;;("" :no-select nil)
;;;     ("" :separator nil)
;;;     ("North Arrow" :eval (create-and-add-object '(create-and-add-window-object north-arrow-object)))
;;;     ("Camera" :eval (create-and-add-object '(create-and-add-camera-object camera-model-object)))
;;;     ("Axis" :eval (create-and-add-object '(create-and-add-3d-object axis-object)))
;;;     ("Corner" :eval (create-and-add-object '(create-and-add-3d-object trihedral-corner )))
;;;     ("Sun Ray" :eval (create-and-add-object '(create-and-add-3d-object sun-ray-object)))
;;;     ("Star" :eval (create-and-add-object '(create-and-add-3d-object radiating-object)))
;;;     ("Conj Pt" :eval (create-and-add-object '(create-and-add-3d-object conjugate-point-object))
;;;                :documentation "Conjugate Point Object for Camera Model Derivation")
;;;     ("2d Conj Pt" :eval (create-and-add-object '(create-and-add-3d-object 2d-conjugate-point-object)))
;;;                 
;;;     ("Epi-Crosshair" :eval (create-and-add-object '(create-and-add-3d-object epipolar-crosshair)))
;;;                 
;;;           
;;;     )))

(defparameter *default-object-creation-menu-item-list*
  '(				 ;(3d-composite-object "3d Composite")
    (3d-text-object "3d Text")
    (3d-crosshair-object "3d Crosshair")
    (conjugate-point-object "Conjugate Pt")
					;(3d-point-object "3d Point")
    (3d-curve "Open 3d Curve")
    (3d-closed-curve "Closed 3d Curve")
					;(3d-network-object "3d Network")
					;("3d Ruler")
    (3d-ribbon-curve  "3d Ribbon")
					;("3d Curtain")
    (cube-object "Box")
    (house-object "House")
    (extruded-object "Extrusion")
    (cylinder "Cylinder")
    (half-cylinder "Quonset")
    (vertical-half-cylinder "Trap")
    (axis-object "Axes")
    (3d-ruler-object "3d Ruler")
    (bogus " " :columnbreak t) ; tk multicolumn menus have a formatting problem with tear-offs
					;(2d-composite-object "2d Composite" :columnbreak t)
					;(2d-text-object "2d Text" :columnbreak t)
    (2d-text-object "2d Text")
    (2d-crosshair-object "2d Crosshair")
					;(2d-point-object "2d Point")
    (2d-curve "Open 2d Curve")
    (2d-closed-curve "Closed 2d Curve")
    (2d-rectangle "2d Rectangle")
    (2d-ruler-object "2d Ruler")
    ))

(defmethod object-creation-menu-item-list ((panel cme-control-panel))
  *default-object-creation-menu-item-list*)

(custom:defcustom *ENABLE-TIX-DIR-BROWSER* nil
   (:groups (:lisptk))
  "Enable the use of the TIX Directory Tree File Browser from the Control Panel File Menu.")


(defmethod file-menu-item-list ((panel cme-control-panel))
  `((load-site "Load Site")
    ,@(when *enable-tix-dir-browser*
	    '((browse-images "Browse Images")))
    (load-image "Load Image")
    ;;(browse-sites "Browse Sites")
    (save-feature-set "Save Feature Set")
    (quit-freedius "Exit")))
	  
(defmethod view-creation-menu-item-list ((panel cme-control-panel))
  '((com-make-adjustable-view "Adjustable Copy" 
     :documentation "Make an Adjustable View with same Projection as Specified View")
    ))

(defmethod frame-creation-menu-item-list ((panel cme-control-panel))
  '((new-1x1-frame "New 1x1 Frame")
    (new-2x2-frame "New 2x2 Frame")
    (new-2x1-frame "New 2x1 Frame")
    (new-1x2-frame "New 1x2 Frame")))

(defmethod panel-menu-item-list ((panel cme-control-panel))
  `((3d-feature-sets-config "3D Feature Sets")
    (2d-feature-sets-config "2D Feature Sets")
	    
    ;; this looks ugly, but it supports a rich set of options
    (frame "Create Frame" :button_type cascade :items ,(frame-creation-menu-item-list panel))
    (selection-panel "Selection Info")
    (photometric-transform-panel "Photometric Transform")
    (inspect "Inspector")
    (preferences-panel "Preferences")
    ))
  
(defmethod menubar-menu-items ((panel cme-control-panel))
  `((file "File" ,@(file-menu-item-list panel))
    (create-object "Objects" ,@(object-creation-menu-item-list panel))
    (create-view "Views" ,@(view-creation-menu-item-list panel))
    (panels "Panels" ,@(panel-menu-item-list panel))
    ))

;(MAKE-CME-CONTROL-PANEL)

;;;
;;; It's not clear how we can remove menu items:
;;;
(defmethod add-menu-item ((panel cme-control-panel) top-menu-name item)
  (tcl-eval
   "add_menu_items"
   (tk::widget-named panel top-menu-name)
   (format nil "{ ~a }" item)
   "{}"
   ))

#|
(tk::convert-menu-item-list '(("Copy View" :accel <alpha-beta-right> :eval (com-copy-view *interactor*)))) ((LIST (LIST "EVAL" "(COM-COPY-VIEW COMMON-SYMBOLS:*INTERACTOR*)")
														  "Copy View"))
|#

;{0 label -button_type cascade -items itemlist -tearoff boolean}

#+never
(defmethod panel-control-items ((panel cme-control-panel))
  '((tandem "Tandem" :button_type qtogglebutton)
    (sensitive "Sensitive" :button_type qtogglebutton)
    (hide "Hide" :button_type qtogglebutton)
   ; #+(or agl cocoa) 
    (mouse-mode " Mouse " :button_type qtogglebutton
     :documentation "Toggle single-button mouse mode.")
    ))

#+(or agl cocoa)
(defmethod panel-control-items ((panel cme-control-panel))
  '((tandem "Tandem" :button_type qtogglebutton :width 8)
    (sensitive "Sensitive" :button_type qtogglebutton :width 11)
    (hide "Hide" :button_type qtogglebutton :width 6)
    (left "L" :button_type qtogglebutton :documentation "Left Mouse Button Mode" :width 2)
    (middle "M" :button_type qtogglebutton :documentation "Middle Mouse Button Mode" :width 2)
    (right "R" :button_type qtogglebutton :documentation "Right Mouse Button Mode" :width 2)
    ))

#-(or agl cocoa)
(defmethod panel-control-items ((panel cme-control-panel))
  '((tandem "Tandem" :button_type qtogglebutton)
    (sensitive "Sensitive" :button_type qtogglebutton)
    (hide "Hide" :button_type qtogglebutton)
    (left "L" :button_type qtogglebutton :documentation "Left Mouse Button Mode")
    (middle "M" :button_type qtogglebutton :documentation "Middle Mouse Button Mode")
    (right "R" :button_type qtogglebutton :documentation "Right Mouse Button Mode")
    ))

#-(or agl cocoa)
(defmethod panel-control-items ((panel cme-control-panel))
  '(;;(tandem "Tandem" :button_type qtogglebutton)
    ;;(sensitive "Sensitive" :button_type qtogglebutton)
    ;;(hide "Hide" :button_type qtogglebutton)
    (break "BREAK" :button_type qtogglebutton :documentation "Break (debugger)")
    (left "L" :button_type qtogglebutton :documentation "Left Mouse Button Mode")
    (middle "M" :button_type qtogglebutton :documentation "Middle Mouse Button Mode")
    (right "R" :button_type qtogglebutton :documentation "Right Mouse Button Mode")
    )) 





;(MAKE-CME-CONTROL-PANEL)

;;; *********************  CME CONTROL PANEL  *********************

;;; Mods for Aqua.  Makes the panel wider, adds the single-button mode
;;; toggle, and pads some widgets differently -CC

;;; LHQ Wed Dec 15 2004 - I have merged differences between the aqua and
;;; non-agua versions of make-cme-control-panel so that there is now only one
;;; version of the function.

;;; Gotta consolidate: The app-defaults that come bundled with
;;; Freedius cause strange things to happen on different platforms &
;;; different screens.  In short, there is no one completely
;;; consistent app defaults file, so I've injected a config variable
;;; that can refer to a site-local app-defaults file.  This should
;;; OVERRIDE the existing Freedius defaults.  Should these variables
;;; be merged? --CC

(defvar *control-panel-tk-class* "ControlPanel")

(custom:defcustom *FREEDIUS-APP-DEFAULTS-FILE* 
  (if (probe-file "$FREEDIUS_ARCH/lisp/freedius.app-defaults")
      "$FREEDIUS_ARCH/lisp/freedius.app-defaults"
      "$FREEDIUS/lisp-tk/freedius.app-defaults")
  (:groups (:lisptk :essential-settings))
  "Filename for the app-defaults used by most of tk widgets used in FREEDIUS.
To customize for a particular architecture, install a customized version of
$FREEDIUS/lisp/tk/freedius.app-defaults in $FREEDIUS_ARCH/lisp/<xxx>.app-defaults 
and specify a setting for this variable in $FREEDIUS_ARCH/lisp/runtime-options.lisp file.
")

;;; (defun make-cme-control-panel (&rest args &key (width 1500) (height 100) (panel-class 'cme-control-panel)
;;; 			       (tk-class *control-panel-tk-class*))
;;;   (tk::tk-load-app-defaults *freedius-app-defaults-file*)
;;;   (let* ((toplevel-name (gensym-toplevel-widget-path ".controlpanel"))
;;;          (main-frame (merge-widget-pathname toplevel-name "mf"))
;;;          (frm (merge-widget-pathname main-frame "f"))
;;;          (doc (merge-widget-pathname frm "doc")); bucky state
;;;          (doc2 (merge-widget-pathname frm "doc2")) ; other dynamic state
;;;          (frm2 (merge-widget-pathname main-frame "f2"))
;;;          (menubar (merge-widget-pathname frm2 "mb"))
;;;          (controls (merge-widget-pathname frm2 "pc"))
;;;          (panel-proto (get-class-prototype panel-class))
;;; 	 (cme-control-panel-script
;;;           `((qtoplevel ,toplevel-name :class ,tk-class :title 
;;; 		       ,(format nil "FREEDIUS Control Panel (pid=~d)" (getpid))
;;; 	     :width ,width :height ,height)
;;;             #+(or agl cocoa) (wm geometry ,toplevel-name 1200x60+10+20) ; FIXME -- magic numbers

;;;             (qframe mf -parent ,toplevel-name)
;;;             ;(qframe f2 -parent ,main-frame -width ,width -pady 2) ; -pady causes error in tk
;;; 	    (qframe f2 -parent ,main-frame -width ,width)
;;;             (qframe mb -parent ,frm2)
            
;;;             (set_global default_parent ,menubar)
;;; 	    ,@(loop for (name text . items) in (menubar-menu-items panel-proto)
;;; 		    for widget = (merge-widget-pathname menubar name)
;;; 		    collect `(qpulldownmenu ,widget -text ,text -items ',items
;;; 					    -underline 0 -padx 8))
;;; 	    ;; Modified - 
;;;             ;(qbutton inspect :text "INSPECT")
;;;             ;(qbutton quit :text "QUIT")
            
;;;             (qframe f -parent ,main-frame)
;;;             (qentry doc -parent ,frm -borderwidth 0 -highlightthickness 0 -width 60 )
;;;             (qentry doc2 -parent ,frm -borderwidth 0 -highlightthickness 0 -width 60 )

;;;             (qbuttonlist pc -parent ,frm2 :width 150 :buttonargs '(:padx 2 :pady 2)
;;; 			 :items ',(panel-control-items panel-proto))
            
;;;             (pack ,main-frame -side top -fill both -anchor nw -expand 1)
;;;             (pack ,frm -side top -anchor nw)
;;;             (pack ,frm2 -side top -fill both  -anchor nw -expand 1)
;;;             (pack ,menubar -side left -anchor nw)
;;;             #+(or agl cocoa) (pack_children ,menubar -side left -anchor nw -padx 2 -ipadx 4 -ipady 2)
;;; 	    #-(or agl cocoa) (pack_children ,menubar -side left -fill x -anchor nw)
;;;             #+(or agl cocoa) (pack ,controls -side right -anchor ne -padx 10 -ipadx 10 -ipady 10) 
;;; 	    #-(or agl cocoa) (pack ,controls -side right -anchor ne)
;;;             (pack ,doc -side top -fill x -anchor nw -padx 5)
;;;             (pack ,doc2 -side top -fill x -anchor nw -padx 5)
;;;             )))
;;;     (tcl-script cme-control-panel-script)
;;;     (setq *cme-control-panel*
;;;           (make-widget-panel toplevel-name 'cme-control-panel-callback
;;;                              :panel-class panel-class)
;;;           )
;;;     (tk:do-events)
;;;     (create-tk-binding ;(tk::widget-named *cme-control-panel* 'f )
;;;      (tk::widget *cme-control-panel*)
;;;                        "<Configure>" (widget width height)
;;;                        (docline-configure-callback widget width height))
;;;     ;; For some reason the Lisp apple-menu-bar prevails
;;;     #+(or agl cocoa) (make-cme-toplevel-menu *cme-control-paNel*)
;;;     *cme-control-panel*))

#|
;(MAKE-CME-CONTROL-PANEL)
;(docline-configure-callback (widget *cme-control-panel*) nil nil)
;(tcl-cmd `(,(tk::widget-named *cme-control-panel* 'doc) configure))
;(tcl-cmd `(font families))

(tcl-cmd `(wm geometry ,(widget *cme-control-panel*) "500x60"))
|#


(defparameter *flat-qentry-args* 
  (if (member :THEMED config::*tk-features*) 
      '(:font "courier 10 normal" :style "Flat.TEntry" ) ;; FIXME:  :font isn't supported?
      '(:font "courier 12 bold" :relief flat :borderwidth 0 :highlightthickness 0)))
    
(defvar *default-control-panel-class* 'cme-control-panel)

;;#+ttk-widgets
(defun make-cme-control-panel (&key (width 500) (height 60) (panel-class *default-control-panel-class*)
			       (tk-class *control-panel-tk-class*))
  (tk::tk-load-app-defaults *freedius-app-defaults-file*)
  (flet ((make-doc-widget (name)
	   `((qentry ,name ,@*flat-qentry-args*)
	     (pack ,name -side top -fill x -anchor nw ))))
    (let* ((toplevel-name (gensym-toplevel-widget-path ".controlpanel"))
	   (main-frame (merge-widget-pathname toplevel-name "mf"))
	   (frm (merge-widget-pathname main-frame "f"))
	   (doc (merge-widget-pathname frm "doc")) ; bucky state
	   (doc2 (merge-widget-pathname frm "doc2")) ; other dynamic state
	   (frm2 (merge-widget-pathname main-frame "f2"))
	   (menubar (merge-widget-pathname frm2 "mb"))
	   (controls (merge-widget-pathname frm2 "pc"))
	   (panel-proto (get-class-prototype panel-class))
	   (geometry-spec (format nil "~dx~d" width height)); do we want top-left also?
	   (cme-control-panel-script
	    `((qtoplevel ,toplevel-name :class ,tk-class :title 
			 ,(format nil "FREEDIUS Control Panel (pid=~d)" (getpid))
			 :width ,width :height ,height)
	      ;; #+(or agl cocoa) (wm geometry ,toplevel-name 1200x60+10+20) ; FIXME -- magic numbers
	      (wm geometry ,toplevel-name ,geometry-spec)
	      (qframe mf -parent ,toplevel-name)
	      ;;(qframe f2 -parent ,main-frame -width ,width -pady 2) ; -pady causes error in tk
	      (qframe f2 -parent ,main-frame -width ,width)
	      (qframe mb -parent ,frm2)
            
	      (set_global default_parent ,menubar)
	      ,@(loop for (name text . items) in (menubar-menu-items panel-proto)
		      for widget = (merge-widget-pathname menubar name)
		      collect `(qpulldownmenu ,widget -text ,text -items ',items
					      -underline 0 
					      ;;-padx 8
					      ))
	      ;; Modified - 
	      ;;(qbutton inspect :text "INSPECT")
	      ;;(qbutton quit :text "QUIT")
            
	      (qframe f -parent ,main-frame)
	      (pack ,main-frame -side top -fill both -anchor nw -expand 1)
	      (pack ,frm -side top -anchor nw -fill x -expand 1)
	      (pack ,frm2 -side top -fill both  -anchor nw -expand 1)
	      ,@(make-doc-widget doc)
	      ,@(make-doc-widget doc2)
	      (qbuttonlist pc -parent ,frm2 :width 150 ; :buttonargs '(:padx 2 :pady 2)
			   :items ',(panel-control-items panel-proto))
	      (pack ,menubar -side left -anchor nw)
	      #+(or agl cocoa) (pack_children ,menubar -side left -anchor nw -padx 2 -ipadx 4 -ipady 2)
	      #-(or agl cocoa) (pack_children ,menubar -side left -fill x -anchor nw)
	      #+(or agl cocoa) (pack ,controls -side right -anchor ne -padx 10 -ipadx 10 -ipady 10) 
	      #-(or agl cocoa) (pack ,controls -side right -anchor ne)
	      )))
      (tcl-script cme-control-panel-script)
      (setq *cme-control-panel*
	    (make-widget-panel toplevel-name 'cme-control-panel-callback
			       :panel-class panel-class)
	    )
      (tk:do-events)
      (create-tk-binding   ;(tk::widget-named *cme-control-panel* 'f )
	  (tk::widget *cme-control-panel*)
	  "<Configure>" (widget width height)
	(docline-configure-callback widget width height))
      ;; For some reason the Lisp apple-menu-bar prevails
      #+agl (make-cme-toplevel-menu *cme-control-panel*)
      *cme-control-panel*)))



;;; Allows Freedius functions to appear in the Mac OS X top-level menu
;;; bar.  Application shows up as "Freedius"...
;;;
#+never ; #+(or agl cocoa) ; quam version
(defmethod make-cme-toplevel-menu ((panel cme-control-panel))
  (let* ((menubar (gensym-toplevel-widget-path ".menuBar"))
	 (apple-menu (merge-widget-pathname menubar "apple"))
	 (cme-control-panel-script
	  `((menu ,menubar -tearoff 0) 
	    (,menuBar add cascade -menu ,apple-menu)
	    (menu ,apple-menu -tearoff 0)
	    ;(,apple-menu add command -label "About..." -command "aboutBox")
	    ,@(loop for (name text . items) in (menubar-menu-items panel)
		    for widget = (merge-widget-pathname menubar name)
		    collect `(,menubar add cascade -menu ,widget -label ,text -underline 0)
		    collect `(qmenu ,widget -items ',items))
	    ("." configure -menu ,menubar))))
    (tcl-script cme-control-panel-script)
    (setq *cme-apple-menu-bar*
	  (make-widget-panel menubar 'cme-control-panel-callback
			     :panel-class 'cme-control-panel))
    ))

#+agl  ; CC version
(defmethod make-cme-toplevel-menu ((panel cme-control-panel))
  (let* ((menubar (gensym-toplevel-widget-path ".menuBar"))
	 (cme-control-panel-script
	  `((qmenu ,menubar -tearoff 0)
	    ,@(loop for (name text . items) in (menubar-menu-items panel)
		    for widget = (merge-widget-pathname menubar name)
		    collect `(,menubar add cascade -menu ,widget -label ,text -underline 0)
		    collect `(qmenu ,widget -items ',items))
	    ("." configure -menu ,menubar))))
    (tcl-script cme-control-panel-script)
    (setq *cme-apple-menu-bar*
	  (make-widget-panel menubar 'cme-control-panel-callback
			     :panel-class 'cme-control-panel))
    ))


(defmethod make-cme-toplevel-menu ((panel cme-control-panel))
  (let* ((menubar (gensym-toplevel-widget-path ".menuBar"))
	 (cme-control-panel-script
	  `((qmenu ,menubar -tearoff 0)
	    ,@(loop for (name text . items) in (menubar-menu-items panel)
		    for widget = (merge-widget-pathname menubar name)
		    collect `(,menubar add cascade -menu ,widget -label ,text -underline 0)
		    collect `(qmenu ,widget -items ',items))
	    ("." configure -menu ,menubar))))
    (tcl-script cme-control-panel-script)
    (setq *cme-apple-menu-bar*
	  (make-widget-panel menubar 'cme-control-panel-callback
			     :panel-class 'cme-control-panel))
    ))


;;; use this version for toggle buttons
;;;(defmethod set-mouse-mode ((panel tk::widget-panel) widget button-name)
;;;  (let* ((button-state (cvv-item-value widget))
;;;         (mode (and button-state
;;;                    (case button-name
;;;                      (left 1)(middle 2) (right 3)))))   
;;;    (setf (single-button-mouse-mode) mode)
;;;    ;; turn off the other mouse mode buttons
;;;    (loop for button in '(left middle right)
;;;          unless (eql button button-name)
;;;            do (setf (cvv-item-value (widget-named panel button)) nil))
;;;    (format t "set-mouse-mode ~s ~s~%" button-name mode)
;;;    ))

(defmethod cme-control-panel-set-single-button-mouse-mode 
	   ((panel cme-control-panel) button-name &optional turn-on)
  (let* ((button-state (and button-name (cvv-item-value panel button-name)))
	 (mode (and (or button-state turn-on)
		    (case button-name
		      (left 1)(middle 2) (right 3)))))   
    (setq *single-button-mouse-button* mode)
    (setf *single-button-mouse-mode* mode)
    ;; turn off the other mouse mode buttons
    (loop for button in '(left middle right)
	  unless (eql button button-name)
	    do (setf (cvv-item-value panel button) nil))
    (when turn-on (setf (cvv-item-value panel button-name) t))
    (format t "set-mouse-mode ~s ~s~%" button-name mode)
    ))

;;; redefiniton of function in window.lisp 
(defun (setf single-button-mouse-button) (state &optional (panel *cme-control-panel*))
  (setq *single-button-mouse-button* state)
  (set-mouse-mode panel (case state (1 'left) (2 'middle) (3 'right)) t))


;;; ***********************   CME-CONTROL-PANEL-CALLBACK  ***********************
;(MAKE-CME-CONTROL-PANEL)

(defmethod cme-control-panel-callback ((panel cme-control-panel) widget item-name event args)
  (declare (special tk::*quit-repl*))
  (format t "cme-control-panel-callback ~a ~a ~a ~a ~a~%" panel widget item-name event args)
  (setq *foo* (list* panel widget event item-name args))
  (case event
    ((tk::button tk::menubutton)
     (case item-name
       (quit-freedius (format t "~%Exiting FREEDIUS....")(tk::quit-repl :quit-cme))
       (browse-images (make-image-directory-browser (selected-window *interactor*)))
       (load-image (cme-control-panel-load-image panel))
       (save-feature-set (cme-control-panel-save-feature-set panel))
       ;;(browse-sites (make-site-directory-browser (selected-window *interactor*)))
       ;;(load-site (cme-control-panel-load-site panel))
       (load-site (cme::select-cme-site))
       (inspect (tk-inspect))
       (break (break))
       ((tandem sensitive hide)
	(format t "Toggle Button: ~a~%" item-name))
       ;;(mouse-mode (setq *single-button-mouse-mode* (not *single-button-mouse-mode*)))
       ((left middle right off) (cme-control-panel-set-single-button-mouse-mode  panel item-name))
       (new-1x1-frame (make-cme-frame "FREEDIUS FRAME" :nx 1 :ny 1))
       (new-2x2-frame (make-cme-frame "FREEDIUS 2x2 FRAME" :nx 2 :ny 2))
       (new-2x1-frame (make-cme-frame "FREEDIUS 2x1 FRAME" :nx 2 :ny 1))
       (new-1x2-frame (make-cme-frame "FREEDIUS 1x2 FRAME" :nx 1 :ny 2))
       (3d-feature-sets-config (make-feature-set-configuration-panel :3d-p t))
       (2d-feature-sets-config (make-feature-set-configuration-panel :3d-p nil))
       (selection-panel (make-cme-selection-panel))
       (photometric-transform-panel (make-photometric-transform-panel))
       (preferences-panel (make-preferences-panel))
       (otherwise
	(cond ((equal (tk::widget-name widget) "create-object_menu")
	       (gui-create-object item-name))
	      ((equal (tk::widget-name widget) "create-view_menu")
	       (funcall item-name *interactor*))
	      (t (format t "cme-control-panel-callback unhandled item-name = ~a ~a~%"
			 widget item-name))))
       ))))



;;; ************************  DOCUMENTATION LINE  ************************
#|
(tk::item-alist-from-widget-tree (widget *cme-control-panel*))
(tk::widget-named *cme-control-panel* 'doc)
(set-documentation "")
(get-documentation)
(tk::tcl-cmd `(,(tk::widget-named *cme-control-panel* 'doc) configure))
(tk::tcl-cmd `(,(tk::widget-named *cme-control-panel* 'doc) config -width 60))
(read-from-string  (tk::tcl-cmd `(,(tk::widget-named *cme-control-panel* 'doc) cget -width)))
(tk::tcl-cmd `(,(tk::widget *cme-control-panel*) cget -width))
(tk::tcl-cmd `(winfo width ,(tk::widget *cme-control-panel*)))
(tk::tcl-cmd `(,(tk::widget-named *cme-control-panel* 'f) configure))
(tk::tcl-cmd `(font metrics (,(tk::widget-named *cme-control-panel* 'doc) cget -font)))
(tk::tcl-cmd `(font measure (,(tk::widget-named *cme-control-panel* 'doc) cget -font) "X"))
(create-tk-binding (tk::widget-named *cme-control-panel* 'doc )
		   "<Configure>" (widget width height)
		   (docline-configure-callback widget width height))
(create-tk-binding (tk::widget-named *cme-control-panel* 'f)
		   "<Configure>" (widget width height)
		   (docline-configure-callback widget width height))
(setf (documentation-width) nil)

(set-documentation2 "foobar")
|#

(defmethod  set-documentation (string)
  (tk::set-widget-value 'doc string *cme-control-panel*))

(defmethod get-documentation ()
  (tk::widget-value 'doc *cme-control-panel*))

(declaim (special *cme-selection-panel*))

(defmethod  set-documentation2 (string)
  (when *cme-selection-panel*
    (update-selection-panel))
  (tk::set-widget-value 'doc2 string *cme-control-panel*))

(defmethod get-documentation2 ()
  (tk::widget-value 'doc2 *cme-control-panel*))

(defmethod documentation-width ()
  (read-from-string  (tk::tcl-cmd `(,(tk::widget-named *cme-control-panel* 'doc) cget -width))))

(defmethod (setf documentation-width) (&optional new-width)
  (let ((doc-widget (tk::widget-named *cme-control-panel* 'doc))
	(doc2-widget (tk::widget-named *cme-control-panel* 'doc2))
	)
    ;;(format t "(setf documentation-width)~%")
    (unless new-width
      (let* ((test-string "0123456789")
	     ;(test-string "n")
	     (pixels-per-char (/ (read-from-string
				  (tk::tcl-cmd `(font measure (,doc-widget cget -font) ,test-string)))
				 (dfloat (length test-string))))
	     (control-panel-width (read-from-string
				   (tk::tcl-cmd `(winfo width ,(tk::widget *cme-control-panel*)))))
	     (padding 20))		; FIXME
	(setq new-width (floor (- control-panel-width padding) pixels-per-char))
	;(setq *foo* (list new-width control-panel-width pixels pixels-per-char test-string))
	))
    ;;(break)
    (tk::tcl-cmd `(,doc-widget config -width ,new-width))
    (when doc2-widget (tk::tcl-cmd `(,doc2-widget config -width ,new-width)))
    new-width))

;(docline-configure-callback (widget *cme-control-panel*) nil nil)
(defun docline-configure-callback (widget width height)
  (declare (ignorable width height))
  (when (and nil (equal widget (widget *cme-control-panel*)))
    ;;(format t "docline-configure-callback ~a ~a~%" widget (list width height))

    (tk::recompute-doc-strings (setf (documentation-width) nil))))

(defun tk-inspect ()
  (unless (find-package :tk-inspect)
    (st::load-system "freedius-inspect"))
  (funcall (intern "FREEDIUS-INSPECT" :tk-inspect)))

;;; ****************************  IMAGE BROWSER  ****************************
	
(defvar *image-browser-panel* nil)
;(setq *image-browser-panel* nil)

;(setq *default-browse-image-directories* nil)
(custom:defcustom *default-browse-image-directories* nil
  (:groups (:gui))
  "Default list of directories for the Image Directory Browser.")

(defmethod add-image-browser-directories (panel directories)
  (let ((w (widget panel)))
    (loop for dir in directories
	  for abs-dir = (namestring (probe-file dir))
	  when abs-dir
	  do (tcl-cmd `(tix_dir_browser_add_directory ,w ,abs-dir)))))

(defmethod clear-image-browser-directories (panel)
  (tcl-cmd `(tix_dir_browser_clear ,(widget panel))))

(defparameter *tix-dir-browser-loaded-p* nil)

;;(defparameter *tix-dir-browser-pathname* (tk::tcl-filename-filter "$FREEDIUS/tk/library/tix-dir-browse.tcl"))
(defparameter *tix-dir-browser-pathname* "tix-dir-browse.tcl")


(defun truename-string (path) (namestring (truename path)))

;;; Sources for tix are in /homedir/quam/downloads/tcltk/tix8.2.0 (tix8.2.0b1.tar.gz)
;;; Since the tix is dynamically loaded by tcl, invoking the image-browser-script
;;; below is all that is needed to load it.
;;; If the libtix8.2g.so or the is absent, tcl will complain.
;;; If $FREEDIUS/arch/<arch>/lib/tix subdirectory is absent, tcl will complain.
#+never ;; tix-dir-browse version
(defmethod make-image-directory-browser (window &key (browse-image-directories *default-browse-image-directories*))
  
  (unless (member *cme-control-panel-load-image-initial-directory* 
		  browse-image-directories :key #'probe-file)
    (push *cme-control-panel-load-image-initial-directory* browse-image-directories))
  (let ((panel *image-browser-panel*))
    (format t "make-image-directory-browser ~a~%" browse-image-directories)
    (if panel
	(progn
	  (pop-up-panel *image-browser-panel*)
	  (unless (equal (get-prop panel :directories) browse-image-directories)
	    (clear-image-browser-directories panel)
	    (add-image-browser-directories panel browse-image-directories))
	  )

	(let* ((toplevel-name (gensym-toplevel-widget-path ".cme"))
	       (image-browser-script
		`((tix-dir-browse ,toplevel-name "FREEDIUS Image Browser"
		   ,(namestring (probe-file (car browse-image-directories)))))
		 ))
	  (unless *tix-dir-browser-loaded-p*
	    ;;(tcl-cmd `(source ,*tix-dir-browser-pathname*))
	    (tcl-cmd `(package require "tixDirBrowse"))
	    (setq *tix-dir-browser-loaded-p* t))
	  (tcl-script image-browser-script)
	  (setq panel
		(make-widget-panel toplevel-name 'image-browser-panel-callback
				   :panel-class 'raw-arguments-cvv-panel)
		*image-browser-panel* panel)
	  (tk::set-toplevel-widget-position (widget panel) :mouse)
	  (add-image-browser-directories panel (cdr browse-image-directories))
	  ;;(tcl-cmd `(wm withdraw ,toplevel-name))
	  ))
    (setf (get-prop panel :window) window)
    (setf (get-prop panel :directories) browse-image-directories)
    ))

(setq *default-browse-image-directories* '("/"))

(defmethod make-image-directory-browser (window &key (browse-image-directories *default-browse-image-directories*))
  
  (unless (member *cme-control-panel-load-image-initial-directory* 
		  browse-image-directories :key #'probe-file)
    (push *cme-control-panel-load-image-initial-directory* browse-image-directories))
  (let ((panel *image-browser-panel*))
    (format t "make-image-directory-browser ~a~%" browse-image-directories)
    (if panel
	(progn
	  (pop-up-panel *image-browser-panel*)
	  (unless (equal (get-prop panel :directories) browse-image-directories)
	    (clear-image-browser-directories panel)
	    (add-image-browser-directories panel browse-image-directories))
	  )

	(let* ((toplevel-name (gensym-toplevel-widget-path ".cme"))
	       (image-browser-script
		`((ttk-dir-browse ,toplevel-name "FREEDIUS Image Browser"
		   ',(loop for path in browse-image-directories
			  collect (namestring (probe-file path)))))
		 ))
	  (unless *tix-dir-browser-loaded-p*
	    (tcl-cmd `(package require "ttkDirBrowse"))
	    (setq *tix-dir-browser-loaded-p* t))
	  (tcl-script image-browser-script)
	  (setq panel
		(make-widget-panel toplevel-name 'image-browser-panel-callback
				   :panel-class 'raw-arguments-cvv-panel)
		*image-browser-panel* panel)
	  (tk::set-toplevel-widget-position (widget panel) :mouse)
	  ;;(add-image-browser-directories panel (cdr browse-image-directories))
	  ;;(tcl-cmd `(wm withdraw ,toplevel-name))
	  ))
    (setf (get-prop panel :window) window)
    (setf (get-prop panel :directories) browse-image-directories)
    ))



(defmethod image-browser-panel-callback ((panel widget-panel) widget item-name event args)
  ;;(format t "image-browser-panel-callback ~a ~a ~a ~a ~s~%" panel widget item-name event args)
  ;;(setq *image-browser-panel-callback-args* (list panel widget item-name event args))
  (case event
    (tk::destroy (setq *image-browser-panel* nil))
    (tk::unmap (tcl-cmd `(wm withdraw ,(widget panel))))
    (tk::tix_dir_browse_activate (image-browser-panel-handle-load-image-selection panel args))
    ))

(defmethod image-browser-panel-handle-load-image-selection ((panel widget-panel) path)
  (let* ((image (cond ((img::pyramid-descriptor-p path)
		       (img::top-of-image-hierarchy (img::load-image-pyramid (pathname-directory-path path))))
		      ((img::pyramid-directory-p path)
		       (img::top-of-image-hierarchy (img::load-image-pyramid (pathname-as-directory path))))
			 
		      ((img::recognize-image-header path)
		       (img::load-image path)))))
    (format t "image-browser-panel-handle-load-image-selection ~a~%" image)
    (when image 
      (unless (selected-window *interactor*)
	(setf (selected-window *interactor*) (pick-a-window )))
      (push-image image (selected-window *interactor*)))))
;(pyramid-descriptor-p "/m/opt/data/site-2d-worlds/alv/alv-oblique-tower")
;(pyramid-directory-p "/m/opt/data/site-2d-worlds/alv/alv-oblique-tower"))
;(pathname-as-directory "/m/opt/data/site-2d-worlds/alv/alv-oblique-tower")
;(pathname-as-directory "/m/opt/data/site-2d-worlds/alv/alv-oblique-tower/")
  
(defun set-image-browser-directory (panel dir)
  (tcl-cmd `(tix_dir_browser_set_directory ,(widget panel) ,dir)))
  
;;(make-pathname :name nil :type nil :defaults "/opt/IU/radius/site-2d-worlds/alv/alv-oblique-tower/image.g0")



;;; **********************  LOAD-IMAGE PANEL  ***********************

;;; FIXME:  The should be initialized in some other way.
(custom:defcustom *cme-control-panel-load-image-initial-directory* "$HOME/"
  (:groups (:gui)) "Default directory for Load Image Panel")

(defparameter *tk_getOpenFile-filetypes*
  '(("Image files" (".img" ".tif" ".tiff" ".bmp" ".jpg" ".g*" "CME-DESCR.PYRAMID"))
    ("All files" "*") ))

(defparameter *tk_getOpenFile-filetypes*
  '(("All files" "*") ))

;;; FIXME: tk_getOpenFile does not support filenames containing environment variables.
(defmethod  cme-control-panel-load-image ((panel widget-panel))
  (let ((path (tcl-cmd `("tk_getOpenFile" -parent ,(widget *CME-CONTROL-PANEL*)
			 -title "Choose Filename for Load-Image"
			 -filetypes ',*tk_getOpenFile-filetypes*
			 -initialdir ,(tk::tcl-filename-filter *cme-control-panel-load-image-initial-directory*)))))
    (when (and (not (equal path ""))
	       (probe-file path))
      (setq *cme-control-panel-load-image-initial-directory* (pathname-directory-path path))
      (image-browser-panel-handle-load-image-selection panel path)
      )))

;(tk::tcl-cmd `(source ,(namestring (truename "$FREEDIUS/tk/library/tkfbox-hacks.tcl"))))

;;; ****************************  SAVE-FEATURE-SETS  ****************************

(defmethod image-browser-panel-handle-save-feature-set-selection ((panel widget-panel) path)
  )

;;; FIXME
(defparameter *cme-control-save-feature-set-initial-directory* "/tmp/") ; ????

;;; FIXME:  TK tk_getSaveFile does not support filenames containing environment variables.
(defmethod  cme-control-panel-save-feature-set ((panel cme-control-panel))
  (let* ((object (or (selected-object) (pick-an-object "Pick a Feature Set")))
	 (feature-set (car (object-feature-sets object))))
    (unless feature-set
      (error "Object ~a does not belong to any feature set" object))
    (let* ((default-path (or (get-prop feature-set :pathname)
			     *cme-control-save-feature-set-initial-directory*))
	   (default-directory (pathname-directory-path default-path))
	   (name-ext (namestring (make-pathname :defaults default-path :directory nil)))
	   (path (tcl-cmd `("tk_getSaveFile" -parent ,(widget *CME-CONTROL-PANEL*)
			    -title "Choose Filename for Save-Feature-Set"
			    -filetypes ',*tk_getOpenFile-filetypes*
			    -initialfile ,(or name-ext "")
			    -initialdir ,(tk::tcl-filename-filter default-directory)))))
      (unless (equal path "")
	(setq *cme-control-save-feature-set-initial-directory* (pathname-directory-path path))
	(save-feature-set feature-set path)
	(setf (get-prop feature-set :pathname) path)
	;;(setq *cme-control-panel-save-feature-set-pathname* path)
	))))
;(pathname-name *cme-control-save-feature-set-initial-directory*)
;(make-pathname :defaults *cme-control-save-feature-set-initial-directory* :directory nil)
;(setf (get-prop (car (object-feature-sets (selected-object))) :pathname) nil)




;;; ***********************  SITE DIRECTORY BROWSER  ***********************

;;; forget this for now
	
(defvar *site-browser-panel* nil)

(defparameter *default-browse-site-directories*
  '("/opt/IU/radius/sites"
    ))

(defmethod add-site-browser-directories (panel directories)
  (let ((w (widget panel)))
    (loop for dir in directories
	  do (tcl-cmd `(tix_dir_browser_add_directory ,w ,dir)))))

(defmethod clear-site-browser-directories (panel)
  (tcl-cmd `(tix_dir_browser_clear ,(widget panel))))

(defparameter *tix-dir-browser-loaded-p* nil)


(defmethod make-site-directory-browser (window &key (browse-site-directories *default-browse-site-directories*))
  (let ((panel *site-browser-panel*))
    (format t "make-site-directory-browser ~a~%" browse-site-directories)
    (if panel
	(progn
	  (pop-up-panel *site-browser-panel*)
	  (unless (equal (get-prop panel :directories) browse-site-directories)
	    (clear-site-browser-directories panel)
	    (add-site-browser-directories panel browse-site-directories))
	  )

	(let* ((toplevel-name (gensym-toplevel-widget-path ".cme"))
	       (site-browser-script
		`((tix-dir-browse ,toplevel-name "FREEDIUS Site Browser"
		   ,(car browse-site-directories)))
		 ))
	  (unless *tix-dir-browser-loaded-p*
	    (tcl-cmd `(source ,(tk::tcl-filename-filter *tix-dir-browser-pathname*)))
	    (setq *tix-dir-browser-loaded-p* t))
	  (tcl-script site-browser-script)
	  (setq panel
		(make-widget-panel toplevel-name 'site-browser-panel-callback
				   :panel-class 'raw-arguments-cvv-panel)
		*site-browser-panel* panel)
	  (add-site-browser-directories panel (cdr browse-site-directories))
	  ;;(tcl-cmd `(wm withdraw ,toplevel-name))
	  ))
    (setf (get-prop panel :window) window)
    (setf (get-prop panel :directories) browse-site-directories)
    ))


(defmethod site-browser-panel-callback ((panel widget-panel) widget item-name event args)
  (format t "site-browser-panel-callback ~a ~a ~a ~a ~s~%" panel widget item-name event args)
  (case event
    (destroy (setq *image-browser-panel* nil))
    (unmap (tcl-cmd `(wm withdraw ,(widget panel))))
    (tix_dir_browse_activate (site-browser-panel-handle-load-site-selection panel args))
    ))

;;;(defmethod load-site ((site-name string))
;;;  (loop for site-glue in *site-glue-list*
;;;        do (loop for name in (site-list site-glue)
;;;                 when (equal site-name name)
;;;                   return (generic-load-site site-name site-glue))))
  
(defmethod site-browser-panel-handle-load-site-selection ((panel widget-panel) path)
  (cme::load-site (pathname-name path)))



 
#|
*create-object*
(transform-vector (transform-path (top-view) (3d-world (top-view))) (cv 100.0 100.0 0.0))
(TRANSFORMS::INTERSECT-CAMERA-RAY-WITH-PLANE )
(TRANSFORMS::INTERSECT-CAMERA-RAY-WITH-z-PLANE )
(transform-path (top-view) (2d-world (top-view)))

(load (compile-file "$FREEDIUS/lisp/cme-compat/cme-control-panel.lisp"))
(setq tk::*tk-verbose* t)
(MAKE-CME-CONTROL-PANEL)
(setq *image-browser-panel* nil)
(cme::select-cme-site)
(progn
  (setq *image-browser-panel* nil)
  (tcl-cmd `(source ,(tk::tcl-filename-filter *tix-dir-browser-pathname*)))
  (LISPTK::make-image-directory-browser nil))

(tcl-cmd `(source ,(tk::tcl-filename-filter *tix-dir-browser-pathname*)))
(truename *tix-dir-browser-pathname*)
(widget *image-browser-panel*)


(let ((w (widget *image-browser-panel*)))
  (tcl-cmd `(tix_dir_browser_set_directory ,w "/home/rom1/quam/pix")))

(let ((w (widget *image-browser-panel*)))
  (tcl-cmd `(tix_dir_browser_set_directory ,w "/opt/IU/radius/site-2d-worlds/alv/alv-2-44/full")))

(let ((w (widget *image-browser-panel*)))
  (tcl-cmd `(tix_dir_browser_add_directory ,w "/home/rom1/quam/pix")))


(tcl-cmd `(wm withdraw ,(widget *image-browser-panel*)))
(widget *image-browser-panel*)

(tcl-cmd `("tk_getOpenFile" -parent ,(widget *CME-CONTROL-PANEL*) -filetypes ',*tk_getOpenFile-filetypes*
	   -initialdir "/homedir/quam/pix"))

(tcl-cmd '(info procs "tix-dir-browse"))
(progn *image-browser-panel*)
(widget *image-browser-panel*)


|#


 
 
;;; UPDATE-SELECTION-PANEL should be rewritten to use methods of the object class 
;;; for the bulk of the work.  

;;; UPDATE-SELECTION-PANEL has moved to selection-panel.lisp 
#+old
(progn

;;;  ****************************  CME-SELECTION-PANEL  ****************************

#|
(setq tk::*tk-verbose* t)
(setq tk::*tk-verbose* nil)
(make-cme-selection-panel)     ;   EVAL ME
(setq *cme-selection-panel* nil)

(progn *cme-selection-panel*)
(cvv-item *cme-selection-panel* 'object)
(item-alist *cme-selection-panel*)
(setf (cvv-item-value *cme-selection-panel* 'object-pos) "(cv 1.0 2.0 3.0)")
(format nil "~a_label" (widget (cvv-item *cme-selection-panel* 'fragment)))

(tk::set-item-label *cme-selection-panel* 'fragment "foo")
(update-selection-panel)
(format nil "~a" (or (selected-object) (view-image (top-view))))
|#

(defparameter *cme-selection-panel* nil)

;;;(defun make-cme-selection-panel ()
;;;  (let* ((cvv-item-list
;;;          `((world "World:" :label :anchor w)
;;;            (world-pos "World Pos:" :string )
;;;            (object "Object:" :label :anchor w)
;;;            (object-pos "Obj Pos:" :string )
;;;            (fragment "Fragment:" :string )
;;;            (pixel "Pixel:" :string)
;;;            )))
;;;    (setq *cme-selection-panel*
;;;          (make-cvv-panel cvv-item-list :title "FREEDIUS  Selection"))))

(defun make-cme-selection-panel ()
  (let* ((cvv-item-list
	  `((world "World:" :string ,@*flat-qentry-args*)
	    (world-pos "World Pos:" :string)
	    ;; (object "Object:" :string ,@*flat-qentry-args*)
	    (class "Class:" :string ,@*flat-qentry-args*)
	    (object-pos "Obj Pos:" :string )
	    (fragment "Fragment:" :string )
	    (pixel "Pixel:" :string)
	    )))
    (setq *cme-selection-panel*
	  (make-cvv-panel cvv-item-list 
			  :title "FREEDIUS Selection"
			  :resource-name "Obj"
			  :package :gui
			  ))))

(defmethod selection-panel-world-and-position ((object t) frag)
  (let* ((world (world object))
	 (world-class-label (typecase world 
			      (gl-2d-world "2d World:")
			      (gl-3d-world "3d World:")
			      (otherwise "<unknown world class>:")))
	 (obj-pos (obj::fragment-position frag))
	 (world-pos (transform-vector (object-to-world-transform object) obj-pos))
	 )
    (values world 
	    world-pos
	    obj-pos)))
    

(defun update-selection-panel ()
  (when *cme-selection-panel*
    (let* ((interactor *interactor*)
	   (panel *cme-selection-panel*)
	   (sel (selected-objects interactor)))
      (flet ((pos-string (vect)
	       (transforms::bind-vector-elements (x y z) vect
		 (format nil "(cv ~4,1f ~4,1f ~4,1f)" x y z))))
	(if sel
	    (let* ((object (car (car sel)))
		   (class-name (class-name (class-of object)))
		   (frag (cadr (car sel))))
	      (mv-bind (world world-pos obj-pos)
		  (selection-panel-world-and-position object frag)
		(let* ((world-class-label (typecase world 
					    (gl-2d-world "2d World:")
					    (gl-3d-world "3d World:")
					    (otherwise "<unknown world class>:")))
		       (name (name object))
		       frag-name 
		       (frag-descr
			(typecase frag
			  (obj::object-arc
			   (setq frag-name "Arc:")
			   (with-class-slot-values obj::object-arc
			       (start-vertex-id end-vertex-id pick-percent) frag
			     ;; It would be nice to show the length of the arc also
			     (format nil "~a ~a ~1,3f" start-vertex-id end-vertex-id pick-percent)))
			  (obj::object-vertex
			   (setq frag-name "Vertex:")
			   (obj::vertex-id frag))
			  (otherwise ""))))
		  (tk::set-item-label panel 'world world-class-label)
		  (tk::set-item-label panel 'fragment frag-name)
					;(tk::set-item-label panel 'object "Object:")
		  (tk::set-item-label panel 'class "Object:")
		  (tk::set-item-label panel 'object-pos "Object Pos:")
		  (tk::set-item-label panel 'pixel "")
		  (setf	;(cvv-item-value panel 'object) (or name "<unnamed-object>")
		   (cvv-item-value panel 'class) ;class-name
		   (format nil "~a" object) ; (print-object object nil)
		   (cvv-item-value panel 'fragment) frag-descr
		   (cvv-item-value panel 'object-pos) (pos-string obj-pos)
		   (cvv-item-value panel 'world) (name world)
		   (cvv-item-value panel 'world-pos) (pos-string world-pos)
		   (cvv-item-value panel 'pixel) "")
		  ;;(tk::unmap-cvv-item (cvv-item panel 'pixel))
		  )))
				  
	    ;; No object selected -- image position is used
	    (let* ((win (current-window interactor))
		   (view (top-view win))
		   (image (and view  (view-image view))))
	      (when view
		(let* ((pos (current-window-pos interactor))
		       (2d-world (2d-world view))
		       (2d-world-name (if 2d-world (name 2d-world) "" ))
		       (2d-pos
			(inverse-transform-vector (2d-to-window-transform view)
						  pos *2d-pos-vector*))
		       (image-pos 
			(inverse-transform-vector (image-to-2d-transform view)
						  *2d-pos-vector* *image-pos-vector*)))
		  (tk::set-item-label panel 'world "2d World:")
		  (tk::set-item-label panel 'fragment "Window Pos:")
					; (tk::set-item-label panel 'object "Image:")
		  (tk::set-item-label panel 'class "Image:")
		  (tk::set-item-label panel 'object-pos "Image Pos:")
		  (tk::set-item-label panel 'pixel "Pixel:")
					;(break)
		  (setf (cvv-item-value panel 'class) (if image (format nil "~a" image) "")
			(cvv-item-value panel 'fragment) (pos-string pos)
			(cvv-item-value panel 'object-pos) (if image (pos-string image-pos) "")
			(cvv-item-value panel 'world) 2d-world-name
			(cvv-item-value panel 'world-pos) (pos-string 2d-pos)
			(cvv-item-value panel 'pixel)
			(if image 
			    (bind-vector-elements (u v) image-pos (image-pixel-string image u v))
			    ""))
		  ;;(tk::map-cvv-item (cvv-item panel 'pixel))
	  
		  ))))))))

) ; end progn


;;; This should move to a new file

(defun ttk-theme-exists (theme &optional (pkg-prefix "ttk::"))
 (not (equal "-1" 
	     (tcl-cmd `(lsearch :exact (package names) ,(format nil "~atheme::~(~a~)" pkg-prefix theme))))))


(defvar *freedius-preferences-panel* nil)
   
;(tk::tcl-cmd '("ttk::themes"))
;(tk::tcl-cmd `(package names))
;(ttk-themes-item-list)
(defun ttk-themes-item-list ()
  (let* ((*package* (find-package :gui))
	 (tcl-package-names (tk::tcl-list-to-strings (tk::tcl-cmd `(package names))))
	 (ttk-pkg-prefix (cond ((member "ttk::theme::default" tcl-package-names :test 'string=)
				"ttk::")
			       ((member "tile::theme::default" tcl-package-names :test 'string=)
				"tile::")))
	 (ttk-theme-pkg-prefix (format nil "~atheme::" ttk-pkg-prefix))
	 (themes `((freedius "Freedius" )
		   (default "Default")
		   (alt "Revitalized")
		   (classic "Classic")
		   (winnative	"Windows Native")
		   (xpnative	"XP Native")
		   (aqua	"Aqua"))))
    ;;(setq *foo* (list ttk-pkg-prefix ttk-theme-pkg-prefix))
    (when ttk-pkg-prefix
      (loop for theme in (tcl-list-to-lisp 
			  (tk::tcl-cmd (if (eq ttk-pkg-prefix "tile::")
					   '("tile::availableThemes")
					   '("ttk::themes")))
			     :upcase t :package *package*)
	    unless (assoc theme themes)
	      do (setf themes (append themes (list (list theme (string-capitalize (symbol-name theme)))))))
      (loop for (theme name) in themes
	    when (member (format nil "~atheme::~(~a~)" ttk-pkg-prefix theme) 
			 tcl-package-names :test 'string=)
	      collect (list theme name)))
    ))

(defparameter *panel-controls*
  (let ((panel-controls-buttonargs nil))
    `(panel_controls nil  :button-list
		     :group :preamble 
		     :items ((quit-panel "Quit")
			     (update-panel "Update")
			     (lock-panel "Lock" :button-type :toggle-button
					 :documentation "Lock panel to current object."
					 ,@panel-controls-buttonargs))
		     ,@(and panel-controls-buttonargs
			    `(:buttonargs ,panel-controls-buttonargs)))))

(defun make-preferences-panel ()
  (let* ((themes-item-list (ttk-themes-item-list))
	 (panel-controls-buttonargs nil)
	 
	 (cvv-item-list
	  `((panel_controls nil  :button-list
			    :group :preamble 
			    :items ((quit-panel "Quit")
				    (update-panel "Update")
				    (lock-panel "Lock" :button-type :toggle-button
						:documentation "Lock panel to current object."
						,@panel-controls-buttonargs))
			    ,@(and panel-controls-buttonargs
				   `(:buttonargs ,panel-controls-buttonargs)))
            (alpha "Transparency" :slider :initial-value obj::*global-default-object-alpha* :from 0.1 :to 1.0 :resolution 0.01
                   :orient "horiz" :documentation  "Set transparency value for all objects.")
            (hfactor "Hidden Line Factor" :slider :initial-value gui::*hidden-line-offset-factor*
                    :from 0.1 :to 10.0 :resolution 0.01
                    :orient "horiz" :documentation  "Set glPolygonOffset factor for all objects.")
            (hunits "Hidden Line Units" :slider :initial-value gui::*hidden-line-offset-units*
                    :from 0.1 :to 10.0 :resolution 0.01
                    :orient "horiz" :documentation  "Set glPolygonOffset units for all objects.")
            (labels "Object Labels" :check-button :initial-value ,obj::*enable-object-labels* :documentation "Turn object labels on / off")
            (hidden "Hidden Line" :check-button :initial-value ,gui::*hidden-line-eliminate* :documentation "Turn hidden line removal on / off")
            (shading "Shading" :check-button :initial-value ,gui::*gl-shading-enabled* :documentation "Enable / disable GL shading")
            (foo "Foo" :check-button :initial-value ,gui::*gl-shading-enabled* :documentation "Enable / disable ")
	    ,@(when themes-item-list
		    `((theme "TTk Theme:" 
			     :abbrev-assoc ;;:assoc 
			     :items ,themes-item-list
			     :initial-value ,(current-ttk-theme))))))
				 
	 (panel (make-cvv-panel cvv-item-list 
				:title "FREEDIUS Preferences"
				:package :gui
				:callback-function 'freedius-preferences-panel-callback
				:cache-p nil
				)))
    (setq *freedius-preferences-panel* panel)
    ))

;(tk-destroy (widget *freedius-preferences-panel*))
;(cvv-item-value ".top9.topfrm.theme" )
;(tk::tcl-cmd `(widget_value ".top11.topfrm.theme"))
;(tk::tcl-cmd `(qwidget_class ".top10.topfrm.theme"))



(defun freedius-preferences-panel-callback (panel widget item-name event args)
;;  (format t "freedius-preferences-panel-callback ~{~a ~}~%" (list panel  widget item-name event args))
  (case item-name
    (THEME (let ((theme (cvv-item-value widget)))
	     (when theme (set-ttk-theme (cvv-item-value widget)))))
    (HFACTOR (if (get-prop panel :hfactor)
                 (setf gui::*hidden-line-offset-factor* (cvv-item-value widget))
                 (setf (get-prop panel :hfactor)
                       (setf (cvv-item-value widget) gui::*hidden-line-offset-factor*))))
    (HUNITS (if (get-prop panel :hunits)
                 (setf gui::*hidden-line-offset-units* (cvv-item-value widget))
                 (setf (get-prop panel :hunits)
                       (setf (cvv-item-value widget) gui::*hidden-line-offset-units*))))
    (ALPHA (if (get-prop panel :alpha)
	       (obj::set-default-object-alpha (cvv-item-value widget))
	       (progn ;; why isn't :initial-value working here?  Value gets reset on creation.
		 (format t "~%Setting for the first time.., alpha=~f" obj::*global-default-object-alpha*)
		 (setf (cvv-item-value widget) obj::*global-default-object-alpha*)
		 (setf (get-prop panel :alpha) obj::*global-default-object-alpha*))))
    (LABELS (setf obj::*enable-object-labels* (cvv-item-value widget)))
    (HIDDEN (setf gui::*hidden-line-eliminate* (cvv-item-value widget)))
    (SHADING (setf gui::*gl-shading-enabled* (cvv-item-value widget)))
    )
  ;; This forces the display lists to be rebuilt:
  (let ((*invalidate-all-object-sets* t))
    (declare (special *invalidate-all-object-sets*))
    (map-over-all-views (view) (redisplay view))
    ))


(defun current-ttk-theme ()
  'freedius
  ;;(read-from-string (tk::tcl-cmd ))
  )
 
(defun set-ttk-theme (theme)
  (tk::tcl-cmd `("ttk::setTheme" ,theme)))
#|
(ttk-theme-exists 'default "tile::theme::")
(format nil "ttk::theme::~(~a~)" 'default)
(tk::tcl-cmd `(package names))
(tk::tcl-cmd `("tile::availableThemes"))
$tile::currentTheme
(setq tk::*tk-verbose* t)
(tk::tcl-cmd `(source "/opt/IU/FREEDIUS/default/tk/library-ttk/composite-widgets.tcl"))
|#

;(tk::tcl-cmd `(list "$THEMELIST"))
;(make-preferences-panel)
;(setq tk::*tk-verbose* t) 
;(setq tk::*tk-verbose* nil) 

#+never
(st::add-system-initialization
 :f3d-tk
 '(gui::make-cme-control-panel))

