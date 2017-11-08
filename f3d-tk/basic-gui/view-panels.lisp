(in-package :gui)

#|
New:  Mon Nov  8 2004  LHQ

This file contains only a couple of calls to functions in CME package.

Feature set control panel similar to that in CME.

TODO:   Control of feature-set sensitivity.  
        FREEDIUS is currently lacking sensitivity controls in the pick machinery.

        2d-feature-set config panel.  FREEDIUS is currently lacking many 2d-object classes.

        Selected-feature-set machinery.  FREEDIUS is lacking menu for object creation.

|#

;;; Must be very careful with toggle buttons.  
;;; The tcl variable names must be unique to each togglebutton
(defparameter *fs-configuration-panel-counter* 0)

(defclass feature-set-configuration-panel (widget-panel)
    ((3d-p :initform t :initarg :3d-p)
     (panelid :initform nil :initarg :panelid)
     (max-index :initform -1)
     (fs-widgets-alist :initform nil)
     (selected-view :initform nil)
     (world :initform nil :accessor world)
     ))


(defmethod (setf selected-feature-set) (fs (panel feature-set-configuration-panel))
  (setf (selected-feature-set (world panel)) fs))

(defmethod selected-feature-set ((panel feature-set-configuration-panel))
  (selected-feature-set (world panel)))

(defmethod view-world ((panel feature-set-configuration-panel) view)
  (and view
       (with-class-slots feature-set-configuration-panel (3d-p) panel
	 (if 3d-p (3d-world view) (2d-world view)))))

(declaim (special *fs-config-panel*))

;(eval-cache-flush *fs-config-panel*)
;(make-feature-set-configuration-panel)
;(TK:CVV-ITEM-VALUE *fs-config-panel* 'WHICH_VIEWS)
;(TK:CVV-ITEM *fs-config-panel* 'WHICH_VIEWS)
;(TK:CVV-ITEM-VALUE (TK:CVV-ITEM *fs-config-panel* 'WHICH_VIEWS))

(custom::defcustom config::object-cvv-app-defaults-file nil ;"$FREEDIUS/lisp-tk/object-cvv.app-defaults"
  (:group :lisptk)
  "OBJECT-CVV-APP-DEFAULTS-FILE contains app-defaults which \"fine-tune\" the defaults
for photometric-transform-panels and feature-set-configuration panels.
The contents of this file should be merged into freedius.app-defaults.")

;;; Sat Mar 31 2007 - merged object-cvv.app-defaults into freedius.app-defaults

(defun make-feature-set-configuration-panel (&key (3d-p t))
  (let ((panel
	 (eval-cache (make-feature-set-configuration-panel 3d-p)
	   ;; Sat Mar 31 2007 - merged object-cvv.app-defaults into freedius.app-defaults
	   ;;(tk-load-app-defaults config::object-cvv-app-defaults-file)
	   (let* ((toplevel-widget (tk::gensym-toplevel-widget-path))
		  (topframe (merge-widget-pathname toplevel-widget "topfrm"))
		  (fs-frame (merge-widget-pathname topframe "fsfrm")))
	     (tcl-script `((qtoplevel ,toplevel-widget 
			    :title ,(if 3d-p "3D Feature Sets" "2D Feature Sets") :class "Obj")
			   (qframe ,topframe) ;  :padding "0"
			   (set_global default_parent ,topframe)
			   (qbuttonlist panel_controls
					:items '((quit-panel "Quit")
						 (update-panel "Update")
						 (select_view "Select View")))
			   ;;(qbutton select_view :text "Select View" :anchor w :labeltext "")
					; :relief can be one of  ridge groove raised sunken solid
			   (qlabel world :labeltext ,(if 3d-p "3D World: " "2D World: ") 
				    :anchor w :borderwidth 2 :relief solid)
			   (qradiogroup which_views :labeltext "Affected Views: " 
					:initial_value frame
					:items '((view "Selected" 
						  :documentation "Modify only the Selected View") 
						 (frame "Frame"
						  :documentation "Modify all views of Selected Frame")
						 (world "World"
						  :documentation "Modify all views of World")))
			   (qframe ,fs-frame)
			   (qbuttonlist panel_buttons
					:items '((new_fs "New Feature Set")))
			   (pack ,topframe :fill both :expand 1)
			   (qgrid_2_column ,topframe)
			   (grid ,(merge-widget-pathname topframe "world") -sticky w)
			   
			   ))
	     (let* ((panel (make-widget-panel toplevel-widget
					      'feature-set-configuration-panel-callback
					      :package :gui
					      :panel-class 'feature-set-configuration-panel)))
	       (with-class-slots feature-set-configuration-panel ((p3d-p 3d-p) panelid) panel
		 (setf panelid (incf *fs-configuration-panel-counter*)
		       p3d-p 3d-p))
	       (tk::set-toplevel-widget-position (widget panel) :mouse)
	       (setf (cvv-item-value (get-named-item panel 'frame)) 'frame)
	       panel)))))
  
    
    (pop-up-panel panel)
    (setq *fs-config-panel* panel)
    panel))

; ".top5.which_views"
;(setf (cvv-item-value (get-named-item *fs-config-panel* 'frame)) 'world)
;(tk::tcl-cmd `(getprop ,(get-named-item *fs-config-panel* 'which_views) variable))
;(tk::tcl-cmd `(set_global ,(get-named-item *fs-config-panel* 'which_views) 'world))
;(tk::tcl-cmd `(get_global ,(get-named-item *fs-config-panel* 'which_views)))

(defparameter *fs-config-menu-button-class* 'qtogglebutton)
;;(defparameter *fs-grid-args* '(:padx 0 :pady 0))
(defparameter *fs-grid-args* nil)

(defun add-feature-set-to-fs-config-panel (panel fs)
  (with-class-slots feature-set-configuration-panel (3d-p fs-widgets-alist panelid max-index) panel
    (let* ((toplevel-widget (widget panel))
	   ;(fs-frame (merge-widget-pathname toplevel-widget "fsfrm"))
	   (fs-frame (get-named-item panel 'fsfrm))
	   (entry (assoc fs fs-widgets-alist)))
      (unless entry
	(let* ((i (incf max-index))
	       (fs-name (format nil "~a.fsname~d" fs-frame i))
	       (fs-buttons (format nil "~a.fsbuts~d" fs-frame i))
	       (button-variable-suffix (format nil "_~a_~d" panelid i))
	       (fs-present-variable (format nil "fspresent~a" button-variable-suffix ))
	       (fs-sensitive-variable (format nil "fssensitive~a" button-variable-suffix ))
	       (fs-selected-variable (format nil "fsselected~a" button-variable-suffix ))	       
	       )
	  (setf fs-widgets-alist
		(append fs-widgets-alist (list (list fs fs-name fs-buttons) )))
	  (tcl-script
	   `((qlabel ,fs-name :text ,(name fs) :anchor e)
	     (qbuttonlist ,fs-buttons :items 
			  '((present "Pres" :button_type qtogglebutton :variable ,fs-present-variable)
			    (sensitive "Sens" :button_type qtogglebutton :variable ,fs-sensitive-variable)
			    (selected "Sel" :button_type qtogglebutton :variable ,fs-selected-variable)))
	     (grid ,fs-name ,fs-buttons ,@*fs-grid-args*)
	     (grid ,fs-name :sticky e))))))))

(defun add-allfs-to-fs-config-panel (panel)
  (with-class-slots feature-set-configuration-panel (3d-p fs-widgets-alist panelid max-index) panel
    (let* ((toplevel-widget (widget panel))
	   (fs-frame (get-named-item panel 'fsfrm))
	   ;(fs-frame (merge-widget-pathname toplevel-widget "fsfrm"))
	   (entry (assoc 0 fs-widgets-alist)))
      (unless entry
	(let* ((i (incf max-index))
	       (fs-name (format nil "~a.fsname~d" fs-frame i))
	       (fs-buttons (format nil "~a.fsbuts~d" fs-frame i))
	       (button-variable-suffix (format nil "_~a_~d" panelid i))
	       (fs-present-variable (format nil "fspresent~a" button-variable-suffix ))
	       (fs-sensitive-variable (format nil "fssensitive~a" button-variable-suffix ))
	       (fs-selected-variable (format nil "fsselected~a" button-variable-suffix ))	       
	       )
	  (setf fs-widgets-alist
		(append fs-widgets-alist (list (list 0 fs-name fs-buttons) )))
	  (tcl-script
	   `((qlabel ,fs-name :text "ALL" :anchor e)
	     ;; How ??
	     (qbuttonlist ,fs-buttons :items 
			 ; #+never
			  '((present "Pres" :button_type qtogglebutton)   ; :variable ,fs-present-variable
			    (sensitive "Sens" :button_type qtogglebutton) ; :variable ,fs-sensitive-variable
			    (selected "Sel" :button_type qtogglebutton))   ; :variable ,fs-selected-variable
			  #+never
			  '((present "Pres" :button_type qbutton) ;;  :variable ,fs-present-variable
			    (sensitive "Sens" :button_type qbutton) ;; :variable ,fs-sensitive-variable
			    (selected "<.>" :button_type qbutton)) ;; :variable ,fs-selected-variable
			  )
	     (grid ,fs-name ,fs-buttons ,@*fs-grid-args*)
	     (grid ,fs-name :sticky e))))))))

(defun remove-feature-set-from-fs-config-panel (panel fs)
  (with-class-slots feature-set-configuration-panel (3d-p fs-widgets-alist panelid) panel
    (let* ((entry (assoc fs fs-widgets-alist)))
      (when entry
	(setf fs-widgets-alist
	      (remove entry fs-widgets-alist :test #'equal))
	(destructuring-bind (fs-name-widget fs-buttons-widget) (cdr entry)
	  (tk::tk-destroy fs-name-widget)
	  (tk::tk-destroy fs-buttons-widget))))))

;;; This assumes that the elements of objects support the NAME generic-function.
(defun alpha-sort-objects (objects)
  (sort (copy-list objects) #'string-lessp :key 'name))
 
;;; Perhaps a hashtable implementation of this function would be better.
;;;(defun sets-equal (set1 set2) 
;;;  (and (= (length set1) (length set2))
;;;       (null (set-difference set1 set2))
;;;       (null (set-difference set2 set1))))

;;; alternate version which optionally compares using a canonical sort-function
(defun sets-equal (set1 set2 &optional sort-function) 
  (and (= (length set1) (length set2))
       (if sort-function
	   (equal (funcall sort-function set1) (funcall sort-function set2))
	   ;; fallback when there is no sort-function
	   (and (null (set-difference set1 set2))
		(null (set-difference set2 set1))))))

;; Update the feature-set configuration panel to that of the world or the view.
(defmethod update-fs-config-panel (panel &optional view)
  (with-class-slots feature-set-configuration-panel 
      (max-index selected-view 3d-p fs-widgets-alist panelid) panel
    (let* ((view (or view selected-view (setf selected-view (top-view))))
	   (world (view-world panel view))
	   (world-fss (feature-sets world))
	   (panel-fss (loop for (fs fs-name fs-buttons) in fs-widgets-alist
			    collect fs)))
      (unless (sets-equal world-fss panel-fss)
	;; Remove all of the existing feature-set buttons
	(loop for fs in panel-fss
	      do (remove-feature-set-from-fs-config-panel panel fs))
	(setf max-index -1) ; reset the counter.
	(loop for fs in (alpha-sort-objects world-fss)
	      do (add-feature-set-to-fs-config-panel panel fs)))
      ;; Now the panel contains the same feature-sets as the world
      
      ;; Set the togglebutton states corresponding to that of the view.
      (loop with view-fss = (object-sets view)
	    with selected-fs = (selected-feature-set panel)
	    for (fs fs-name fs-buttons) in fs-widgets-alist
	    for fs-present-in-view  = (member fs view-fss)
	    for present-widget = (merge-widget-pathname fs-buttons "present")
	    for sensitive-widget = (merge-widget-pathname fs-buttons "sensitive")
	    for selected-widget = (merge-widget-pathname fs-buttons "selected")
	    do (setf (cvv-item-value present-widget) (not (null fs-present-in-view)))
	       (setf (cvv-item-value sensitive-widget) (sensitive-p view fs))
	       (setf (cvv-item-value selected-widget) (eq fs selected-fs))
	    )
      (add-allfs-to-fs-config-panel panel)
      )))

;;; FIXME for multi-lvcs spanning.  Need a more encompassing definition of WORLD.
(defmethod worlds-compatible (view (world obj::gl-3d-world))
  (eq (3d-world view) world))

(defmethod worlds-compatible (view (world obj::gl-2d-world))
  (eq (2d-world view) world))

;;; update the feature-set configuration of the specified views to that of the panel.
(defmethod update-views-from-fs-config-panel (panel which &optional view)
  (with-class-slots feature-set-configuration-panel (selected-view 3d-p fs-widgets-alist panelid) panel
    (let* ((world (world panel))
	   (view (or view selected-view)))
      (unless (worlds-compatible view world)
	(error "View is not compatible with the selected view"))

      (let* ((active-feature-sets
	      (loop for (fs fs-name fs-buttons) in fs-widgets-alist
		    when  (cvv-item-value (merge-widget-pathname fs-buttons "present"))
		      collect fs))
	     (sensitive-feature-sets
	      (loop for (fs fs-name fs-buttons) in fs-widgets-alist
		    when  (cvv-item-value (merge-widget-pathname fs-buttons "sensitive"))
		      collect fs)))
	(flet ((set-feature-sets (view)
		 (unless (sets-equal (object-sets view) active-feature-sets)
		   (setf (object-sets view) active-feature-sets)
		   (setf (sensitive-feature-sets view) sensitive-feature-sets)
		   (redisplay view))))
	  (case which
	    (world (map-over-active-world-views (world view)
		     (set-feature-sets view)))
	    (frame (map-over-frame-views ((pane-frame (view-window view)) view) 
		     (set-feature-sets view)))
	    (view (set-feature-sets view))
	    ))))))

(defmethod get-fs-clicked (panel widget-clicked)
  (with-class-slots feature-set-configuration-panel (fs-widgets-alist) panel
    (loop with fs-buttons-clicked = (widget-parent widget-clicked)
	  for (fs fs-name fs-buttons) in fs-widgets-alist
	  when (string= fs-buttons-clicked fs-buttons)
	    return fs)))

(defmethod select-view ((panel tk::widget-panel) view)
  (with-class-slots feature-set-configuration-panel ( selected-view world) panel
    (setf selected-view view)
    (setf world (view-world panel view))
    (when world
      (setf (cvv-item-value panel 'world) (name world))
      (update-fs-config-panel panel))))

(defmethod make-new-feature-set-for-world ((world gl-3d-world) &rest initargs)
  (apply 'make-instance '3d-feature-set :world world initargs))

(defmethod make-new-feature-set-for-world ((world gl-2d-world) &rest initargs)
  (apply 'make-instance '2d-feature-set :world world initargs))

;;;(defmethod feature-set-configuration-panel-callback 
;;;           ((panel tk::widget-panel) widget item-name event args)
;;;  (with-class-slots feature-set-configuration-panel (3d-p fs-widgets-alist selected-view world) panel
;;;    #+never
;;;    (format t "feature-set-configuration-panel-callback ~a ~a ~s ~s ~a~%" 
;;;            panel widget item-name event args)
;;;    (let* ((*package* (find-package :gui)) ; FIXME -- problems with cvv-item-value
;;;           (fs (get-fs-clicked panel widget))
;;;           )
;;;      (case event
;;;        (tk::button
;;;         (let ((which-views (cvv-item-value (cvv-item panel 'which_views))))
;;;           (case item-name
;;;             (select_view 
;;;              (let* ((pane (pick-a-pane "Select A View"))
;;;                     (view (and pane (top-view pane))))
;;;                (when view
;;;                  (setq *fs-config-panel* panel)
;;;                  (select-view panel view))))
;;;             (update-panel (update-views-from-fs-config-panel panel which-views selected-view))
;;;             (new_fs (add-feature-set (make-new-feature-set-for-world world) world)
;;;                     (update-fs-config-panel panel))
;;;             (present
;;;              (let* ((present-in-panel (cvv-item-value widget)))
;;;                (flet ((modify-view-present-feature-sets (view)
;;;                         (if present-in-panel
;;;                             (pushnew fs (object-sets view))
;;;                             (setf (object-sets view) (remove fs (object-sets view) )))
;;;                         (redisplay view)))
;;;                  (unless fs (break "~a" fs))
;;;                  (case which-views
;;;                    (world (map-over-active-world-views (world view)
;;;                             (modify-view-present-feature-sets view)))
;;;                    (frame (map-over-frame-views ((pane-frame (view-window selected-view)) view)
;;;                             (when (worlds-compatible view world)
;;;                               (modify-view-present-feature-sets view))))
;;;                    (view (modify-view-present-feature-sets selected-view))))))
;;;             (sensitive
;;;              (let* ((sensitive-in-panel (cvv-item-value widget)))
;;;                (flet ((modify-view-sensitive-feature-sets (view)
;;;                         (if sensitive-in-panel
;;;                             (progn (pushnew fs (sensitive-feature-sets view))
;;;                                    (pushnew fs (object-sets view)))
;;;                             (setf (sensitive-feature-sets view) 
;;;                                   (remove fs (sensitive-feature-sets view))))))
;;;                  (unless fs (break "~a" fs))
;;;                  (case which-views
;;;                    (world (map-over-active-world-views (world view)
;;;                             (modify-view-sensitive-feature-sets view)))
;;;                    (frame (map-over-frame-views ((pane-frame (view-window selected-view)) view)
;;;                             (when (worlds-compatible view world)
;;;                               (modify-view-sensitive-feature-sets view))))
;;;                    (view (modify-view-sensitive-feature-sets selected-view))))))
;;;             (selected
;;;              (let* ((selected-in-panel (cvv-item-value widget)))
;;;                (setf (selected-feature-set panel) nil)
;;;                (when selected-in-panel ; make sure all of the others are off.
;;;                  (loop for (fs fs-name fs-buttons) in fs-widgets-alist
;;;                        for select-widget = (merge-widget-pathname fs-buttons "selected")
;;;                        unless (equal select-widget widget)
;;;                          do (setf (cvv-item-value select-widget) nil)
;;;                        else do (setf (selected-feature-set panel) fs))
;;;                  )))
;;;              
;;;             ))))))))

(defmethod feature-set-configuration-panel-callback 
	   ((panel tk::widget-panel) widget item-name event args)
  (with-class-slots feature-set-configuration-panel (3d-p fs-widgets-alist selected-view world) panel
    #+never
    (format t "feature-set-configuration-panel-callback ~a ~a ~s ~s ~a~%" 
	    panel widget item-name event args)
    (let* ((*package* (find-package :gui)) ; FIXME -- problems with cvv-item-value
	   (fs (get-fs-clicked panel widget)))
      (case event
	(tk::button
	 (let ((which-views (cvv-item-value panel 'which_views)))
	   (labels ((get-fs-buttons (fs)
		      (loop for (fs2 fs-name fs-buttons) in fs-widgets-alist
			    when (eq fs2 fs) return fs-buttons))
		    (set-present (present-in-panel fs)
		      (flet ((modify-view-present-feature-sets (view fs)
			       (if present-in-panel
				   (pushnew fs (object-sets view))
				   (setf (object-sets view) (remove fs (object-sets view) )))
			       (redisplay view)))
			(unless fs (break "~a" fs))
			(case which-views
			  (world (map-over-active-world-views (world view)
				   (modify-view-present-feature-sets view fs)))
			  (frame (map-over-frame-views ((pane-frame (view-window selected-view)) view)
				   (when (worlds-compatible view world)
				     (modify-view-present-feature-sets view fs))))
			  (view (modify-view-present-feature-sets selected-view fs)))))
		    (set-sensitive (sensitive-in-panel fs)
		      (flet ((modify-view-sensitive-feature-sets (view fs)
			       (if sensitive-in-panel
				   (progn (pushnew fs (sensitive-feature-sets view))
					  ;; sensitive implies present too!
					  (pushnew fs (object-sets view))
					  (let ((fs-buttons (get-fs-buttons fs)))
					    (setf (cvv-item-value 
						   (merge-widget-pathname fs-buttons "present"))
						  t
						  (cvv-item-value 
						   (merge-widget-pathname fs-buttons "sensitive"))
						  t))
					  )
				   (setf (sensitive-feature-sets view) 
					 (remove fs (sensitive-feature-sets view))))))
			(unless fs (break "~a" fs))
			(case which-views
			  (world (map-over-active-world-views (world view)
				   (modify-view-sensitive-feature-sets view fs)))
			  (frame (map-over-frame-views ((pane-frame (view-window selected-view)) view)
				   (when (worlds-compatible view world)
				     (modify-view-sensitive-feature-sets view fs))))
			  (view (modify-view-sensitive-feature-sets selected-view fs))))))
	     (case item-name
	       (select_view 
		(let* ((pane (pick-a-pane "Select A View"))
		       (view (and pane (top-view pane))))
		  (when view
		    (setq *fs-config-panel* panel)
		    (select-view panel view))))
	       (update-panel (update-views-from-fs-config-panel panel which-views selected-view))
	       (new_fs (add-feature-set (make-new-feature-set-for-world world) world)
		       (update-fs-config-panel panel))
	       ;; Wedge an "All" category in here - if the fs is a
	       ;; number, toggle all buttons in the panel:
	       (present (if (numberp fs)
			    (loop for (fs2 fs-name fs-buttons) in fs-widgets-alist
				  unless (numberp fs2)
				    do (set-present (cvv-item-value widget) fs2)
				       (setf (cvv-item-value (merge-widget-pathname fs-buttons "present"))
					     (cvv-item-value widget)))
			    (set-present (cvv-item-value widget) fs)))
	       (sensitive (if (numberp fs)
			    (loop for (fs2 fs-name fs-buttons) in fs-widgets-alist
				  unless (numberp fs2)
				    do (let ((val (cvv-item-value widget)))
					 (set-sensitive val fs2)
					 (setf (cvv-item-value (merge-widget-pathname fs-buttons "sensitive"))
					       val)))
			    (set-sensitive (cvv-item-value widget) fs)))
	       
	       (Selected
		(let* ((selected-in-panel (cvv-item-value widget)))
		  (unless (numberp fs)
		    (setf (selected-feature-set panel) nil)
		    (when selected-in-panel ; make sure all of the others are off.
		      (loop for (fs fs-name fs-buttons) in fs-widgets-alist
			    for select-widget = (merge-widget-pathname fs-buttons "selected")
			    unless (equal select-widget widget)
			      do (setf (cvv-item-value select-widget) nil)
			    else do (setf (selected-feature-set panel) fs))
		      (set-sensitive t fs)))))
	       ))))))))



#|
(selected-feature-set *fs-config-panel*)
(symbol-package 'tk::button)
(cvv-item *fs-config-panel* 'world)
;(describe *foo*)
;(tk-inspect::inspect-thing *fs-config-panel*)
;(setf (object-sets (top-view)) nil)
;(update-fs-config-panel *fs-config-panel*)
;(update-views-from-fs-config-panel *fs-config-panel* :world (top-view))

;(setq *fs-config-panel* (make-feature-set-configuration-panel))
;(remove-feature-set-from-fs-config-panel *fs-config-panel* (nth 0 (feature-sets (3d-world (top-view)))))
;(add-feature-set-to-fs-config-panel *fs-config-panel* (nth 0 (feature-sets (3d-world (top-view)))))

;(get-item-value )
;(tk::widget-value ".top17.fsbuts9.selected")
;(tk::tcl-list-to-list (tk::widget-value ".top15.fsbuts9.present")
;(get-fs-clicked *fs-config-panel* ".top5.fsbuts0.present")
;(fs-widgets-alist *fs-config-panel*)
;(tk::decompose-widget-name ".top5.fsname0.present")
;(map-over-active-world-views )
;(tk::tcl-cmd '(".top5.fsbuts11.present" cget :relief))
;(tk::tcl-cmd '(".top5.fsbuts11.present" cget :activeBorderWidth))
;(tk::tcl-cmd '(".top5.fsbuts11.present" cget :borderWidth)) = 2
;(tk::tcl-cmd '(".top5.fsbuts11.present" cget :highlightColor)) = 0

;(tk::tcl-cmd '(".top5.fsbuts11.present" cget :selectColor)) = palegreen1
;(tk::tcl-cmd '(".top5.fsbuts11.present" cget :highlightBackground)) = #e6e6e6
;(tk::tcl-cmd '(".top5.fsbuts11.present" cget :activebackground)) = yellow

;(tk::tcl-cmd '(".top5.fsbuts11.present" cget :activeforeground)) = 0

;(tk::tcl-cmd '(".top5.fsbuts11.present" configure :highlightThickness 0))
;(tk::tcl-cmd '(".top5.fsbuts11.present" configure :highlightThickness 0))
;(tk::tcl-cmd '(".top5.fsbuts11.present" cget :highlightThickness)) = 1

;(tk::widget-value ".top5.fsbuts11.present")
;(tk::cvv-item-value ".top5.fsbuts11.present")
;(setf (tk::cvv-item-value ".top5.fsbuts11.present") nil)
(tk::widget-parent ".top14.fsfrm.fsbuts9.present")
(describe (tk::cvv-item-value ".top14.which_views.3d_world"))
(describe (tk::cvv-item-value ".top14.which_views"))
(tk::cvv-item-value ".top14.fsfrm.fsbuts6.present")
(tk::cvv-item *fs-config-panel* 'which_views)
|#


(defmethod view-display-attr-callback (menu item keyword action value)
  (format t "~%view-display-attr-callback: ~%      menu=~a~%      item=~a~%      keyword=~a~%      action=~a~%      value=~a~%"
	  menu item keyword action value))





(defmethod  view-display-attr-callback (menu item (keyword (eql 'antialias)) action value)
  (let ((attr (get-prop menu :attributes)))
    (setf (anti-alias-lines attr) (not (anti-alias-lines attr)))
    (redisplay (get-prop menu :view))))

(defmethod  view-display-attr-callback (menu item (keyword (eql 'view-shading)) action value)
  (let ((attr (get-prop menu :attributes)))
    (setf (shading-enabled attr) (not (shading-enabled attr)))
    (redisplay (get-prop menu :view))))


(defun com-view-attributes-panel (interactor)
  (let* ((view (top-view (current-window interactor)))
	 (attr (display-attributes view))
	 (panel (make-cvv-panel
		 `((controls nil :button-list :group :preamble
		    :items ((quit-panel "Quit")))
		   (view-shading "Per-view Shading" :check-button :initial-value ,(shading-enabled attr))
		   (antialias "Anti-Aliasing" :check-button :initial-value ,(anti-alias-lines attr))

#||
		   ;; Groups, separators, and labels don't seem to work too well...
		   (fgalpha "FG Alpha" :slider  :initial-value 0.5 :from 0.0 :to 1.0 :resolution 0.01 :orient "horiz")
		   (fgred "FG Red" :slider  :initial-value 0.5 :from 0.0 :to 1.0 :resolution 0.01 :orient "horiz")
		   (fggreen "FG Green" :slider  :initial-value 0.5 :from 0.0 :to 1.0 :resolution 0.01 :orient "horiz")
		   (fgblue "FG Blue" :slider  :initial-value 0.5 :from 0.0 :to 1.0 :resolution 0.01 :orient "horiz")
		   (fgcolor "Foreground Color" :string)

		   ;; (bgcolor "Background Color" :label)
		   (bgalpha "BG Alpha" :slider  :initial-value 0.5 :from 0.0 :to 1.0 :resolution 0.01 :orient "horiz")
		   (bgblue "BG Blue" :slider  :initial-value 0.5 :from 0.0 :to 1.0 :resolution 0.01 :orient "horiz")
		   (bggreen "BG Green" :slider  :initial-value 0.5 :from 0.0 :to 1.0 :resolution 0.01 :orient "horiz")
		   (bgred "BG Red" :slider  :initial-value 0.5 :from 0.0 :to 1.0 :resolution 0.01 :orient "horiz")
		   (bgcolor "Background Color" :string)
||#
		   )
		 :title "View Display Attributes"
		 :callback-function 'view-display-attr-callback)))
    (setf (get-prop panel :attributes) attr)
    (setf (get-prop panel :view) view)
    panel))
		   
		   


#|
 BACKGROUND-COLOR          #(0.5 0.5 1.0 1.0)
 FOREGROUND-COLOR          #(0.9999847412109375 0.9999847412109375
                             0.9999847412109375 1.0)
 ANTI-ALIAS-LINES          NIL
 IMAGE-MODULATION-COLOR    #(1.0 1.0 1.0 1.0)
 SHADING-ENABLED           T
 DEFAULT-MATERIAL          #<MATERIAL-MODEL {592D2D0D}>
 DEFAULT-LIGHT-SOURCE      #<INFINITE-LIGHT-SOURCE {592D2D2D}>
|#
