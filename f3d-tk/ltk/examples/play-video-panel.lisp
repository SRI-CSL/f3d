#|
LHQ:  Thu May  3 2007

This file contains slightly different varients of MAKE-PLAY-VIDEO-PANEL
defined in freedius/systems/base/video-viewer/video-viewer.lisp
reimplemented using the rudimentary ltk-tile and ltk-cvv tools.

This represents work in progress towards a complete implementation of make-cvv-panel
using ltk.

|#

#|
(progn (st:load-system :ltk :initialize t)
       (st:load-system :vivid-viewer)
       (maybe-compile-file-load "$FREEDIUS/lisp/ltk/examples/play-video-panel.lisp")
)

(video::make-play-video-panel) ; the version in video-viewer.lisp using tk::make-cvv-panel.

|#


(in-package :video)

(setq video::*default-load-videos-directory* (lcl::getenv "VIVID_SRIV"))

;;(make-play-video-panel-xxx)
(defun make-play-video-panel-xxx (&key (width 720) (height 480) (title "Play Video XXX"))  
  (let* ((rownum -1)
	 pane-alist
	 (toplevel
	  (lttk::with-lttk-widgets
	    (labels 
		((add-button (name text doc &rest keyvals) 
		   (apply #'button name :text text  :tooltip doc :pack '(:side left) keyvals))
		 (add-buttons (items) (loop for item in items do (apply #'add-button item)))
		 (add-togglebutton (name text doc &rest keyvals) 
		   (apply #'check-button name :text text :tooltip doc :style "Toolbutton"
			  :pack '(:side left) keyvals))
		 (add-togglebuttons (items) 
		   (loop for item in items do (apply #'add-togglebutton item)))
		 (labelled-scale (row name label-text &rest args)
		   (label nil :text label-text :grid `(,row 0 :sticky e))
		   #+never;; ttk::scale doesn't show ticks or axis labels
		   (lttk::make-entry-scale :name name 
					   :entry-args '(:width 6)
					   :scale-args args
					   :grid `(,row 1 :sticky news :padx 4))

		   (lttk::make-entry-scale :name name 
					   :entry-args '(:width 6)
					   :scale-class 'ltk::scale 
					   :scale-args `(,@args
							 :font "courier 8 bold"
							 :orientation horizontal 
							 :showvalue 0 :width 10)
					   :grid `(,row 1 :sticky news :padx 4))))
	      (with-parent (toplevel :title title)
		(with-parent (frame 'topfrm :pack '(:fill both :expand true))
		  (with-parent (frame 'upper-menubar :grid `(,(incf rownum) 0 :sticky ew))
		    (with-parent (pulldownmenu-button 'file :text "File" :underline 0 
						      :pack '(:side left :padx 4))
		      (menubutton 'load-video :text "Load Video")
		      (menubutton 'quit :text "Quit"))
		    (with-parent (frame 'buttons :pack '(:side left :expand true))
		      (add-buttons 
		       '((first-frame  "<<" "Move to First Frame" :width 4)
			 (bakfrm "-F" "Move Backward <Step> Frames" :width 4)
			 (play-rev "<" "Play Video Backward" :width 3)
			 (pause "||" "Pause Video" :width 4)
			 (play-fwd ">" "Play Video Forward" :width 3)
			 (fwdfrm "+F" "Move Forward <Step> Frames" :width 4)
			 (last-frame ">>" "Move to Last Frame" :width 4))))
		    (with-parent (frame 'togglebuttons :pack '(:side right))
		      (add-togglebuttons 
		       '((parameters "Parameters" nil)
			 (wrap-around "Wrap" "Wrap around frame sequence")
			 (stabilize "Stabilize" nil)
			 (warp "Warp" "Warp to Reference Frame")
			 (camera-interp "Interpolate" "Interpolate missing camera matrices")))))
		  (with-parent (frame 'params :grid `(,(incf rownum) 0 :sticky ew))
		    (let ((rownum -1))
		      (labelled-scale (incf rownum) 'frame-number "Frame" :from 0 :to 100 :tickinterval 10)
		      (labelled-scale (incf rownum) 'start-frame "Start Frame" :from 0 :to 100)
		      (labelled-scale (incf rownum) 'end-frame "End Frame" :from 0 :to 100)
		      (labelled-scale (incf rownum) 'fps "Frames/Sec" :from 1 :to 30 :tickinterval 5)
		      (ltk::grid-columnconfigure ltk::*current-master* 1 :weight 1)
		      ))
		  (push (list* 'image-pane
			       (ltk::make-glframe ltk::*current-master* 'glwin
						  ;;:borderwidth 4 :relief "groove" 
						  :width width :height height
						  :grid `(,(incf rownum) 0 :sticky news)))
			pane-alist)
		  (ltk:grid-columnconfigure ltk::*current-master* 0 :weight 1)
		  (ltk:grid-rowconfigure ltk::*current-master* rownum :weight 1)
		  )))))
	 (panel (tk::make-widget-panel toplevel 'play-video-panel-callback
				       :panel-class 'play-video-panel 
				       :panel-args '(:resource-name "Qcme")
				       :package :video)))
    (setq *play-video-panel* panel)
    (setf (gui::frame-pane-alist panel) pane-alist)
    (loop for (id . image-pane) in pane-alist do (setf (gui::panel image-pane) panel))
    (tk::pop-up-panel panel)
    (set-panel-defaults panel)
    (push panel *play-video-panels*)
    (setf (get-prop panel :panel-id) (incf *play-video-panel-id-counter*))	    
    panel))

(defmethod toggle-widget-group ((panel play-video-panel) group-widget)
  (let ((value (tk::cvv-item-value group-widget))
	(widget (tk:widget-clidget (tk::widget-named panel 'params))))
    (if value
	(ltk::grid-map-widget widget)
	(ltk::grid-unmap-widget widget))))



;(make-play-video-panel-xxx2)

(defclass play-video-panel2 (play-video-panel) ())

(defun make-play-video-panel-xxx2 (&key (width 720) (height 480) (title "Play Video XXX2"))  
  (let* ((rownum -1)
	 pane-alist
	 (toplevel
	  (lttk::with-lttk-widgets
	    (labels 
		((add-button (name text doc &rest keyvals) 
		   (apply #'button name :text text  :tooltip doc :pack '(:side left) keyvals))
		 (add-buttons (items) (loop for item in items do (apply #'add-button item)))
		 (add-togglebutton (name text doc &rest keyvals) 
		   (apply #'check-button name :text text :tooltip doc :style "Toolbutton"
			  :pack '(:side left) keyvals))
		 (add-togglebuttons (items) 
		   (loop for item in items do (apply #'add-togglebutton item)))
		 (labelled-scale (row name label-text &rest args)
		   (label (format nil "~(~a~)_lab" name) :text label-text ;:grid `(,row 0 :sticky e)
			  )
		   #+never;; ttk::scale doesn't show ticks or axis labels
		   (lttk::make-entry-scale :name name 
					   :entry-args '(:width 6)
					   :scale-args args
					   :grid `(,row 1 :sticky news :padx 4))

		   (lttk::make-entry-scale :name name 
					   :entry-args '(:width 6)
					   :scale-class 'ltk::scale 
					   :scale-args `(,@args
							 :font "courier 8 bold"
							 :orientation horizontal 
							 :showvalue 0 :width 10)
					   ;:grid `(,row 1 :sticky news :padx 4)
					   )))
	      (with-parent (toplevel :title title)
		(with-parent (frame 'topfrm :pack '(:fill both :expand true) )
		  (with-parent (frame 'upper-menubar 
				      ;:grid `(,(incf rownum) 0 :columnspan 2 :sticky ew)
				      )
		    (with-parent (pulldownmenu-button 'file :text "File" :underline 0 
						      :pack '(:side left :padx 4))
		      (menubutton 'load-video :text "Load Video")
		      (menubutton 'quit :text "Quit"))
		    (with-parent (frame 'buttons :pack '(:side left :expand true))
		      (add-buttons 
		       '((first-frame  "<<" "Move to First Frame" :width 4)
			 (bakfrm "-F" "Move Backward <Step> Frames" :width 4)
			 (play-rev "<" "Play Video Backward" :width 3)
			 (pause "||" "Pause Video" :width 4)
			 (play-fwd ">" "Play Video Forward" :width 3)
			 (fwdfrm "+F" "Move Forward <Step> Frames" :width 4)
			 (last-frame ">>" "Move to Last Frame" :width 4))))
		    (with-parent (frame 'togglebuttons :pack '(:side right))
		      (add-togglebuttons 
		       '((parameters "Parameters" nil)
			 (wrap-around "Wrap" "Wrap around frame sequence")
			 (stabilize "Stabilize" nil)
			 (warp "Warp" "Warp to Reference Frame")
			 (camera-interp "Interpolate" "Interpolate missing camera matrices")))))
		  (labelled-scale (incf rownum) 'frame-number "Frame" :from 0 :to 100 :tickinterval 10)
		  (labelled-scale (incf rownum) 'start-frame "Start Frame" :from 0 :to 100)
		  (labelled-scale (incf rownum) 'end-frame "End Frame" :from 0 :to 100)
		  (labelled-scale (incf rownum) 'fps "Frames/Sec" :from 1 :to 30 :tickinterval 5)
		  
		  (push (list* 'image-pane
			       (ltk::make-glframe ltk::*current-master* 'glwin
						  ;;:borderwidth 4 :relief "groove" 
						  :width width :height height
						  ;:grid `(,(incf rownum) 0 :columnspan 2 :sticky news)
						  ))
			pane-alist)
		  ;;(ltk:grid-columnconfigure ltk::*current-master* 1 :weight 1)
		  ;;(ltk:grid-rowconfigure ltk::*current-master* rownum :weight 1)
		  )))))
	 (panel (tk::make-widget-panel toplevel 'play-video-panel-callback
				       :panel-class 'play-video-panel2 
				       :panel-args '(:resource-name "Qcme")
				       :package :video)))
    (setq *play-video-panel* panel)
    (ltk::set-grid-2-column (tk::widget-named panel 'topfrm)
			    '((upper-menubar 0) 
			      (frame-number_lab frame-number_frm 0)
			      (start-frame_lab start-frame_frm 0)
			      (end-frame_lab end-frame_frm 0) 
			      (fps_lab fps_frm 0)
			      (glwin 1)))
    (setf (gui::frame-pane-alist panel) pane-alist)
    (loop for (id . image-pane) in pane-alist do (setf (gui::panel image-pane) panel))
    (tk::pop-up-panel panel)
    (set-panel-defaults panel)
    (push panel *play-video-panels*)
    (setf (get-prop panel :panel-id) (incf *play-video-panel-id-counter*))
    panel))

(defmethod toggle-widget-group ((panel play-video-panel2) group-widget)
  (let ((value (tk::cvv-item-value group-widget)))
    (loop for name in '(start-frame_frm end-frame_frm fps_frm start-frame_lab end-frame_lab fps_lab)
	  for widget = (tk:widget-clidget (tk::widget-named panel name))
	  do (if value
		 (ltk::grid-map-widget widget)
		 (ltk::grid-unmap-widget widget)))))



;;; This is the preferred style, except need a way to automatically collect the group info.

;(make-play-video-panel-xxx3)

;(ltk::format-wish ".w2.topfrm.upper-menubar.togglebuttons.camera-interp configure")

(defclass play-video-panel3 (play-video-panel) ())

(defun make-play-video-panel-xxx3 (&key (width 720) (height 480) (title "Play Video XXX2"))  
  (let* ((rownum -1)
	 pane-alist
	 (toplevel
	  (lttk::with-lttk-widgets
	    (labels 
		((add-button (name text doc &rest keyvals) 
		   (apply #'button name :text text  :tooltip doc :pack '(:side left) keyvals))
		 (add-buttons (items) (loop for item in items do (apply #'add-button item)))
		 (add-togglebutton (name text doc &rest keyvals) 
		   (apply #'check-button name :text text :tooltip doc :style "Toolbutton"
			  :pack '(:side left) keyvals))
		 (add-togglebuttons (items) 
		   (loop for item in items do (apply #'add-togglebutton item)))
		 (labelled-scale (row name label-text &rest args)
		   (label (format nil "~(~a~)_lab" name) :text label-text :grid `(,row 0 :sticky e)
			  )
		   #+never;; ttk::scale doesn't show ticks or axis labels
		   (lttk::make-entry-scale :name name 
					   :entry-args '(:width 6)
					   :scale-args args
					   :grid `(,row 1 :sticky news :padx 4))

		   (lttk::make-entry-scale :name name 
					   :entry-args '(:width 6)
					   :scale-class 'ltk::scale 
					   :scale-args `(,@args
							 :font "courier 8 bold"
							 :orientation horizontal 
							 :showvalue 0 :width 10)
					   :grid `(,row 1 :sticky news :padx 4)
					   )))
	      (with-parent (toplevel :title title)
		(with-parent (frame 'topfrm :pack '(:fill both :expand true) )
		  (with-parent (frame 'upper-menubar 
				      :grid `(,(incf rownum) 0 :columnspan 2 :sticky ew)
				      )
		    (with-parent (pulldownmenu-button 'file :text "File" :underline 0 
						      :pack '(:side left :padx 4))
		      (menubutton 'load-video :text "Load Video")
		      (menubutton 'quit :text "Quit"))
		    (with-parent (frame 'buttons :pack '(:side left :expand true))
		      (add-buttons 
		       '((first-frame  "<<" "Move to First Frame" :width 4)
			 (bakfrm "-F" "Move Backward <Step> Frames" :width 4)
			 (play-rev "<" "Play Video Backward" :width 3)
			 (pause "||" "Pause Video" :width 4)
			 (play-fwd ">" "Play Video Forward" :width 3)
			 (fwdfrm "+F" "Move Forward <Step> Frames" :width 4)
			 (last-frame ">>" "Move to Last Frame" :width 4))))
		    (with-parent (frame 'togglebuttons :pack '(:side right))
		      (setf (get-prop (add-togglebutton 'parameters "Parameters" nil) :group-names)
			    '(start-frame_lab start-frame_frm
			      end-frame_lab end-frame_frm 
			      fps_lab  fps_frm
			      step_lab step_frm))
		      (add-togglebuttons 
		       '((wrap-around "Wrap" "Wrap around frame sequence")
			 (stabilize "Stabilize" nil)
			 (warp "Warp" "Warp to Reference Frame")
			 (camera-interp "Interpolate" "Interpolate missing camera matrices")))))
		  (labelled-scale (incf rownum) 'frame-number "Frame" :tickinterval 10)
		  (labelled-scale (incf rownum) 'start-frame "Start Frame")
		  (labelled-scale (incf rownum) 'end-frame "End Frame")
		  (labelled-scale (incf rownum) 'fps "Frames/Sec" :from 1 :to 30 :tickinterval 5)
		  (labelled-scale (incf rownum) 'step "Step" :from 1 :to 30 )
		      
		  (push (list* 'image-pane
			       (ltk::make-glframe ltk::*current-master* 'glwin
						  :borderwidth 2 ; :relief "groove" 
						  :width width :height height
						  :grid `(,(incf rownum) 0 :columnspan 2 :sticky news)
						  ))
			pane-alist)
		  ;; Allow all windgets in column 1 to expand horizontally
		  (ltk:grid-columnconfigure ltk::*current-master* 1 :weight 1)
		  ;; Allow image-pane to expand vertically (as well as horizontally)
		  (ltk:grid-rowconfigure ltk::*current-master* rownum :weight 1)
		  )))))
	 (panel (tk::make-widget-panel toplevel 'play-video-panel-callback
				       :panel-class 'play-video-panel3
				       :panel-args '(:resource-name "Qcme")
				       :package :video)))
    (setq *play-video-panel* panel)
    (setf (gui::frame-pane-alist panel) pane-alist)
    (loop for (id . image-pane) in pane-alist do (setf (gui::panel image-pane) panel))
    (tk::pop-up-panel panel)
    (set-panel-defaults panel)
    (push panel *play-video-panels*)
    (setf (get-prop panel :panel-id) (incf *play-video-panel-id-counter*))
	
    panel))


(defmethod toggle-widget-group ((panel t) group-widget)
  (loop with value = (tk::cvv-item-value group-widget)
	for name in (get-prop group-widget :group-names)
	for widget = (ltk::widget-named panel name)
	do (if value
	       (ltk::grid-map-widget widget)
	       (ltk::grid-unmap-widget widget))))


(defmacro with-widget-group (group-widget-name &body group-widget-forms)
  `(progn ,@group-widget-forms))

(defmacro make-widget-group (group-widget &body group-widget-forms)
  `(let ((group-control-widget ,group-widget))
     (setq *foo* (list* group-control-widget))
     (when (stringp group-control-widget) 
       (setq group-control-widget (tk::widget-clidget group-control-widget)))
     (setf (get-prop group-control-widget :group-widgets)
	   (list ,@group-widget-forms))))

;(make-play-video-panel-xxx4)
;(setq *play-video-panel* nil)
;(tk::widget-clidget (tk::cvv-item *play-video-panel* 'parameters))

(defun make-play-video-panel-xxx4 (&key (width 720) (height 480) (title "Play Video XXX2"))  
  (let* ((panel (make-instance 'play-video-panel3
			       :callback-function 'play-video-panel-callback
			       :package :video
			       :resource-name :resource-name))
	 (ltk::*current-panel* (setq *play-video-panel* panel))
	 (rownum -1)
	 (toplevel
	  (lttk::with-lttk-widgets
	    (labels 
		((add-button (name text doc &rest keyvals) 
		   (apply #'button name :text text  :tooltip doc :pack '(:side left) keyvals))
		 (add-buttons (items) (loop for item in items do (apply #'add-button item)))
		 (add-togglebutton (name text doc &rest keyvals) 
		   (apply #'check-button name :text text :tooltip doc :style "Toolbutton"
			  :pack '(:side left) keyvals))
		 (add-togglebuttons (items) 
		   (loop for item in items do (apply #'add-togglebutton item)))
		 (labelled-scale (row name label-text &rest args)
		   (label (ltk::concat-symbol-name name "_LAB")
			  :text label-text :grid `(,row 0 :sticky e)
			  )
		   #+never ;; ttk::scale doesn't show ticks or axis labels
		   (lttk::make-entry-scale :name name 
					   :entry-args '(:width 6)
					   :scale-args args
					   :grid `(,row 1 :sticky news :padx 4))

		   (lttk::make-entry-scale :name name 
					   :entry-args '(:width 6)
					   :scale-class 'ltk::scale 
					   :scale-args `(,@args
							 :font "courier 8 bold"
							 :orientation horizontal 
							 :showvalue 0 :width 10)
					   :grid `(,row 1 :sticky news :padx 4)
					   )))
	      (with-parent (toplevel :title title)
		(with-parent (frame 'topfrm :pack '(:fill both :expand true) )
		  (with-parent (frame 'upper-menubar 
				      :grid `(,(incf rownum) 0 :columnspan 2 :sticky ew)
				      )
		    (with-parent (pulldownmenu-button 'file :text "File" :underline 0 
						      :pack '(:side left :padx 4))
		      (menubutton 'load-video :text "Load Video")
		      (menubutton 'quit :text "Quit"))
		    (with-parent (frame 'buttons :pack '(:side left :expand true))
		      (add-buttons 
		       '((first-frame  "<<" "Move to First Frame" :width 4)
			 (bakfrm "-F" "Move Backward <Step> Frames" :width 4)
			 (play-rev "<" "Play Video Backward" :width 3)
			 (pause "||" "Pause Video" :width 4)
			 (play-fwd ">" "Play Video Forward" :width 3)
			 (fwdfrm "+F" "Move Forward <Step> Frames" :width 4)
			 (last-frame ">>" "Move to Last Frame" :width 4))))
		    (with-parent (frame 'togglebuttons :pack '(:side right))
		      (setf (get-prop (add-togglebutton 'parameters "Parameters" nil) :group-names)
			    '(start-frame_lab start-frame_frm
			      end-frame_lab end-frame_frm 
			      fps_lab  fps_frm
			      step_lab step_frm))
		      (add-togglebuttons 
		       '((wrap-around "Wrap" "Wrap around frame sequence")
			 (stabilize "Stabilize" nil)
			 (warp "Warp" "Warp to Reference Frame")
			 (camera-interp "Interpolate" "Interpolate missing camera matrices")))))
		  (labelled-scale (incf rownum) 'frame-number "Frame" :tickinterval 10)
		  (make-widget-group (tk::cvv-item panel 'parameters)
		    (labelled-scale (incf rownum) 'start-frame "Start Frame")
		    (labelled-scale (incf rownum) 'end-frame "End Frame")
		    (labelled-scale (incf rownum) 'fps "Frames/Sec" :from 1 :to 30 :tickinterval 5)
		    (labelled-scale (incf rownum) 'step "Step" :from 1 :to 30 )
		    )
		  (let ((image-pane 
			 (ltk::make-glframe ltk::*current-master* 'glwin
					    :borderwidth 2 ; :relief "groove" 
					    :width width :height height
					    :grid `(,(incf rownum) 0 :columnspan 2 :sticky news)
					    )))
		    (push (list* 'image-pane image-pane) (gui::frame-pane-alist panel))
		    (setf (gui::panel image-pane) panel))

		;; Allow all windgets in column 1 to expand horizontally
		(ltk:grid-columnconfigure ltk::*current-master* 1 :weight 1)
		;; Allow image-pane to expand vertically (as well as horizontally)
		(ltk:grid-rowconfigure ltk::*current-master* rownum :weight 1)
		))))))
    (setq *play-video-panel* panel)
    (tk::set-widget-panel-toplevel panel toplevel :get-widget-tree nil)
    (tk::pop-up-panel panel)
    (set-panel-defaults panel)
    (push panel *play-video-panels*)
    (setf (get-prop panel :panel-id) (incf *play-video-panel-id-counter*))
	
    panel))

