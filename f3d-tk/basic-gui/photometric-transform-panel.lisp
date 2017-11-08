(in-package :gui)


;;; **************  Photometric Transforms   ****************

(defgeneric photometric-transform (thing))

(define-soft-slot img::image :photometric-transform photometric-transform)
(define-soft-slot view :photometric-transform photometric-transform)
(define-soft-slot img::image :last-display-photometric-transform last-display-photometric-transform)

(defvar *photometric-transform-override* nil)

;;;*default-photometric-transform* interacts with *default-view-image-modulation-color*
(defparameter *default-photometric-transform* '(:linear 0.8d0 0.0d0))

(defmethod default-photometric-transform ((view view))
  *default-photometric-transform*)

(def-foreign-function (set_transfer_scale_bias (:name (freedius-prefix "set_transfer_scale_bias")))
  (scale :double-float)
  (bias :double-float))

;;; The image contains the last photometric-transform used to display it.

;;; IMAGE is the image selected from the image-pyramid by display-image
;;; depending of the image-to-2d-transform.  In general it is different from
;;; (view-image view).  

;;; MAYBE-PHOTOMETRIC-TRANSFORM-FOR-DISPLAY significantly slows down
;;; video-display if the resultant scale is not 1.0 or offset is not 0.0.
;;; Should use (glColor3dv (image-modulation-color ...)) instead.
 
;;;; FIXME:  This needs to be changed for the Lisp based display-image
#+never ; flushing the use of C image_prop
(defun maybe-photometric-transform-for-display (view image)
  (let* ((photometric-transform (or *photometric-transform-override* 
				    (photometric-transform view)))
	 (photometric-transform-changed-p
	  (not (equal photometric-transform (last-display-photometric-transform image))))
	 (default (default-photometric-transform view)))
    (when photometric-transform-changed-p
      (setf (last-display-photometric-transform image) photometric-transform)
					;(format t "release-image-textures~%")
      (img::release-image-textures image 0))

    ;; If the texture-maps have been released, the OpenGL scale-bias parameters need to be set
    ;; even if the photometric-transform has not changed.  
    ;; I do not understand why we can usually avoid calling set_transfer_scale_bias when the
    ;; texture-maps are known to be released.
    ;;(format t "photometric-transform ~a~%" photometric-transform)
    (destructuring-bind (type gain bias) (or photometric-transform default)
      (destructuring-bind (type2 default-gain default-bias) default
	(if (member (image-element-type image) '(single-float double-float (unsigned-byte 16))
		    :test 'equal)
	    (progn ;; This is ugly -- the c++ code needs to do it this way for these image types
	      (when photometric-transform-changed-p
		(if (equal type :linear)
		    (setf (img::image_prop image "scale") gain
			  (img::image_prop image "offset") bias)
		    (setf (img::image_prop image "scale") 0.0
			  (img::image_prop image "offset") 0.0)))
					;(format t "image_prop image scale ~a~%" image)
	      (set_transfer_scale_bias default-gain default-bias))
	    (if (equal type :linear)
		(progn ;(format t "set_transfer_scale_bias ~a~%" photometric-transform)
		  (set_transfer_scale_bias gain bias))
		(set_transfer_scale_bias default-gain default-bias)))))))

(defun image-needs-photometric-transform (image)
  (and (null (image-prop image :photometric-transform))
       (img::scalar-image-p image)
       (not (eql (image-element-size image) 8))
       ;; A hack, for now.  Should somehow designate band-interleaved images as non-scalar:
       (not (typep image 'img::band-interleaved-paged-image))
       (not (typep image 'img::band-interleaved-array-image))))

(defun maybe-photometric-transform-for-display (view image &optional (base-image image))
  (let* ((photometric-transform (or *photometric-transform-override* 
				    (photometric-transform view)
				    (photometric-transform base-image)))
	 (photometric-transform-changed-p
	  (not (equal photometric-transform (last-display-photometric-transform base-image))))
	 (default (default-photometric-transform view)))
    (when (and (not photometric-transform)
	       (image-needs-photometric-transform base-image))
      (setf photometric-transform (compute-photometric-transform-from-image-min-max base-image)
	    (photometric-transform base-image) photometric-transform
	    photometric-transform-changed-p t))    
    (when photometric-transform-changed-p
      (setf (last-display-photometric-transform base-image) photometric-transform)
      ;;(format t "release-image-textures~%")
      (img::release-image-textures image 0)
      )
    
    ;; If the texture-maps have been released, the OpenGL scale-bias parameters need to be set
    ;; even if the photometric-transform has not changed.  
    ;; I do not understand why we can usually avoid calling set_transfer_scale_bias when the
    ;; texture-maps are known to be released.
    ;;(format t "photometric-transform ~a~%" photometric-transform)
    (destructuring-bind (type gain bias) (or photometric-transform default)
      (destructuring-bind (type2 default-gain default-bias) default
	(when t ;photometric-transform-changed-p
	  (if (equal type :linear)
	      (progn ;(format t "set_transfer_scale_bias ~a~%" photometric-transform)
		(set_transfer_scale_bias gain bias))
	      (set_transfer_scale_bias default-gain default-bias)))))))


#|

(photometric-transform (top-view))

(photometric-transform (view-image (top-view)))

|#






(defparameter *photometric-transform-panel* nil)

;;;(defun make-photometric-transform-panel ()
;;;  (let* ((panel (make-cvv-panel 
;;;                `((view nil :button :BUTTON-LABEL "Select View")
;;;                  (gain "Gain" :slider :initial-value 1.0 
;;;                        :from -2.0 :to 2.0 :resolution 0.01 :orient "horiz")
;;;                  (offset "Offset" :slider :initial-value 0.0 
;;;                          :from -2.0 :to 2.0 :resolution 0.01 :orient "horiz")
;;;                  )
;;;                 :title "Photometric Transform"
;;;                 :resource-name "Obj"
;;;                 ;:resource-name "Cvv"
;;;                 ;:resource-name "Cme"
;;;                 :callback-function 'photometric-transform-panel-callback)))
;;;    (setq *photometric-transform-panel* panel)
;;;    panel))
    


;;;(defmethod photometric-transform-panel-callback ((panel tk::widget-panel) widget item-name event args)
;;; ; (format t "photometric-transform-panel-callback ~a ~a ~a ~a ~a~%" panel widget item-name event args)
;;;  (case event
;;;    ((tk::button tk::menubutton)
;;;     (case item-name
;;;       (view (let* ((pane (pick-a-pane "Select A View"))
;;;                     (view (and pane (top-view pane))))
;;;                (when view
;;;                  (setf (get-prop panel :view) view))))))
;;;    (tk::ENTRY_CHANGED 
;;;     (let ((view (get-prop panel :view)))
;;;       (when view
;;;         (setf (photometric-transform view) 
;;;               (list :linear 
;;;                     (cvv-item-value (widget-named panel 'gain))
;;;                     (cvv-item-value (widget-named panel 'offset))))
;;;         ;(format t "photometric-transform changed to ~a~%" (photometric-transform view))
;;;         (redisplay (get-prop panel :view)))))
;;;    ))

(defun make-photometric-transform-panel ()
  (tk-load-app-defaults config::object-cvv-app-defaults-file)
  (let* ((panel (make-cvv-panel 
		`((panel_controls nil :button-list
		   :items ((reset "Reset" :documentation "Reset Photometric Transform") 
			   (auto-min-max "Auto Min/Max" :documentation "Set min/max from image")
			   (select-view "Select View")))
		  (min "Min" :slider :initial-value 0.0 
			:from -4.0 :to 4.0 :resolution 0.01 :orient "horiz")
		  (max "Max" :slider :initial-value 1.0 
			  :from -4.0 :to 4.0 :resolution 0.01 :orient "horiz")
		  )
		 :title "Photometric Transform"
		 :resource-name "Obj"
		 ;:resource-name "Cvv"
		 ;:resource-name "Cme"
		 :callback-function 'photometric-transform-panel-callback)))
    (setq *photometric-transform-panel* panel)
    panel))

#+never ;; #+ttk-widgets
(defun make-photometric-transform-panel ()
  (tk-load-app-defaults config::object-cvv-app-defaults-file)
  (let* ((panel (make-cvv-panel 
		`((panel_controls nil :button-list
		   :items ((reset "Reset" :documentation "Reset Photometric Transform") 
			   (auto-min-max "Auto Min/Max" :documentation "Set min/max from image")
			   (select-view "Select View")))
		  (min "Min" :slider :initial-value 0.0 
			:from -2.0 :to 2.0  :orient "horiz")
		  (max "Max" :slider :initial-value 1.0 
			  :from -2.0 :to 2.0 :orient "horiz")
		  )
		 :title "Photometric Transform"
		 :resource-name "Obj"
		 ;:resource-name "Cvv"
		 ;:resource-name "Cme"
		 :callback-function 'photometric-transform-panel-callback)))
    (setq *photometric-transform-panel* panel)
    panel))
    
#+never ; version where same photometric-transform is used for all view of same image
(defmethod photometric-transform-panel-callback ((panel tk::widget-panel) widget item-name event args)
 ; (format t "photometric-transform-panel-callback ~a ~a ~a ~a ~a~%" panel widget item-name event args)
  (case event
    ((tk::button tk::menubutton)
     (case item-name
       (view (let* ((pane (pick-a-pane "Select A View"))
		    (view (and pane (top-view pane)))
		    (image (and view (view-image view)))
		    )
		(when image
		  (setf (get-prop panel :view) view)
		  (when (photometric-transform image)
		    (destructuring-bind (type scale offset) (photometric-transform view)
		      (mv-bind (imin imax) (normalized-image-element-min-max image) 
			(let* ((imax-imin (- imax imin))
			       (igain (/ imax-imin))
			       (ioff (- (* imin igain)))
			       (pgain (/ scale igain))
			       (pmax-pmin (/ pgain))
			       (poff (- offset (* pgain ioff)))
			       (pmin (- (/ poff pgain))))
			  (setf (cvv-item-value (widget-named panel 'min))  pmin
				(cvv-item-value (widget-named panel 'max)) (+ pmax-pmin pmin)))))
		    ))))))
    (tk::ENTRY_CHANGED 
     (let* ((view (get-prop panel :view))
	    (image (and view (view-image view))))
       (when image
	 (mv-bind (imin imax) (normalized-image-element-min-max image) 
	   (let* ((pmin (cvv-item-value (widget-named panel 'min)))
		  (pmax (cvv-item-value (widget-named panel 'max)))
		  (imax-imin (- imax imin))
		  (igain (/ imax-imin))
		  (ioff (- (* imin igain)))
		  (pmax-pmin (- pmax pmin))
		  (pgain (if (= pmax-pmin 0.0) 1.0 (/ pmax-pmin)))
		  (poff (- (* pmin pgain))))
	     (setf (photometric-transform image) 
		   (list :linear (* igain pgain)
			 (+ poff (* pgain ioff))
			 ))
	     #+never
	     (format t "photometric-transform changed to ~a ~a~%" 
		     (photometric-transform view)
		     (list imin imax pmin pmax igain pgain))
	     ))
	 ;; FIXME -- this needs to resisplay all views containing the image.
	 (redisplay (get-prop panel :view)))))
    ))

;(img::all_image_page_pool_stats 0)
;(img::image_page_pool_stats (image-id  (gui::view-image (gui::top-view t))) 0)
;(img::resize-image-page-pool (gui::view-image (gui::top-view t)) 1000)
;(img::resize-image-page-pool (gui::view-image (gui::top-view t)) 0)
;(img::resize-texid-page-pool 400)

;(fmakunbound 'set-panel-sliders)
(defmethod set-panel-sliders ((panel tk::widget-panel) (image t) photometric-transform)
  (let ((view (get-prop panel :view)))
    (when view
      (destructuring-bind (type scale offset) (or photometric-transform 
						  (default-photometric-transform view))
	(when (eql type :linear)
	  (mv-bind (imin imax) (normalized-image-element-min-max image) 
	    (let* ((imax-imin (let ((x (- imax imin))) (if (<= (abs x) 1.0e-10) 1.0 x)))
		   (igain (/ imax-imin))
		   (ioff (- (* imin igain)))
		   (pgain (/ scale igain))
		   (pmax-pmin (/ pgain))
		   (poff (- offset (* pgain ioff)))
		   (pmin (- (/ poff pgain))))
	      (setf (cvv-item-value (widget-named panel 'min))  pmin
		    (cvv-item-value (widget-named panel 'max)) (+ pmax-pmin pmin)))))))))

(defun ensure-nonzero (value &optional (min-mag 0.0001))
  (if (< (abs value) min-mag)
      min-mag
      value))


(defmethod photometric-transform-panel-callback ((panel tk::widget-panel) widget item-name event args)
  ;; (format t "photometric-transform-panel-callback ~a ~a ~a ~a ~a~%" panel widget item-name event args)
  (let* ((view (get-prop panel :view))
	   (image (and view (view-image view))))
      (case event
	((tk::button tk::menubutton)
	 (case item-name
	   (select-view (let* ((pane (pick-a-pane "Select A View"))
			       (view (and pane (top-view pane)))
			       (image (and view (view-image view)))
			       )
			  (when image
			    (setf (get-prop panel :view) view)
			    (set-panel-sliders panel image (photometric-transform view))
			    (redisplay view))))
	   (auto-min-max (when image
			   (set-panel-sliders 
			    panel
			    image
			    (setf (photometric-transform view) 
				  (compute-photometric-transform-from-image-min-max image)))
			   (redisplay view)))
	   (reset (when image
		    (setf (photometric-transform view) nil)
		    (set-panel-sliders panel image nil)
		    (redisplay view)))))
	(tk::ENTRY_CHANGED 
	 (when image
	   (mv-bind (imin imax) (normalized-image-element-min-max image) 
	     (let* ((pmin (cvv-item-value (widget-named panel 'min)))
		    (pmax (cvv-item-value (widget-named panel 'max)))
		    (imax-imin (ensure-nonzero (- imax imin)))
		    (igain (/ imax-imin))
		    (ioff (- (* imin igain)))
		    (pmax-pmin (ensure-nonzero (- pmax pmin)))
		    (pgain (if (= pmax-pmin 0.0) 1.0 (/ pmax-pmin)))
		    (poff (- (* pmin pgain)))
		    (photometric-transform (list :linear (* igain pgain)
						 (+ poff (* pgain ioff))
						 )))
	       (unless (equal (photometric-transform view) photometric-transform)
		 (setf (photometric-transform view) photometric-transform)
		 #+never
		 (format t "photometric-transform changed to ~a ~a~%" 
			 (photometric-transform view)
			 (list imin imax pmin pmax igain pgain))
	       
		 ;; FIXME -- this needs to redisplay all views containing the image.
		 ;;(format t "photometric-transform-panel-callback ~a~%" (photometric-transform view))
		 (redisplay view)
		 #+never
		 (let ((cnt (img::get_fill_tile_mipmaps_cnt)))
		   (format t "display-image ~a mipmaps filled~%"
			   (- (img::get_fill_tile_mipmaps_cnt) cnt)) (force-output t))
		 )))))
	)))



;(make-photometric-transform-panel) 





;(st:load-system :graph)


#|
(setq *img* (view-image (top-view)))
(setq *graph-pane* (selected-window))
(push-array-graph-view *graph-pane*
			    (img::8-bit-image-histogram *img* :level nil)
			    :graph-style :bar-graph
			    :display-attributes
			    (make-instance 'basic-view-display-attributes
				      :background-color (gl-color-named "black")
				      ;:background-color (gl-color-named "Gray60")
				      ;:background-color (gl-color-named "DeepSkyBlue4")
				      :foreground-color (gl-color-named "white")
				      ))

(clear-view-stack *graph-pane*)
(pop-view *graph-pane*)






|#
