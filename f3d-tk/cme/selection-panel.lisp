(in-package :gui)

;;; New Version Tue Oct 23 2007

(defparameter *cme-selection-panel* nil)

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


(defun selection-panel-pos-string (vect)
  (transforms::bind-vector-elements (x y z) vect
    (format nil "(cv ~4,1f ~4,1f ~4,1f)" x y z)))

#+never
(defmethod selection-panel-world-and-position ((object obj::basic-gl-object) frag)
  (let ((obj-pos (obj::fragment-position frag)))
    (values (to-coordinate-system (object-to-parent-transform (parent object))) ; Looks wrong
	    (transform-vector (object-to-world-transform object) obj-pos)
	    obj-pos)))
    
(defmethod selection-panel-world-and-position ((object obj::basic-gl-object) frag)
  (let ((obj-pos (obj::fragment-position frag)))
    (values (to-coordinate-system (object-to-parent-transform object))
	    (transform-vector (object-to-world-transform object) obj-pos)
	    obj-pos)))
    
;;;(fmakunbound 'set-selection-panel-object)
(defmethod set-selection-panel-object ((object obj::basic-gl-object) frag)
  (let* ((interactor *interactor*)
	 (panel *cme-selection-panel*)
	 (sel (selected-objects interactor))
	 (class-name (class-name (class-of object))))
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
		   (format nil "~a ~a ~5,3f" start-vertex-id end-vertex-id pick-percent)))
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
	(setf ;(cvv-item-value panel 'object) (or name "<unnamed-object>")
	 (cvv-item-value panel 'class)	    ;class-name
	 (format nil "~a" object)	; (print-object object nil)
	 (cvv-item-value panel 'fragment) frag-descr
	 (cvv-item-value panel 'object-pos) (selection-panel-pos-string obj-pos)
	 (cvv-item-value panel 'world) (name world)
	 (cvv-item-value panel 'world-pos) (selection-panel-pos-string world-pos)
	 (cvv-item-value panel 'pixel) "")
	;;(tk::unmap-cvv-item (cvv-item panel 'pixel))
	)))
  )

;;; specialized by video-viewer-view
(defmethod selection-panel-image-descr ((view view))
  (let ((2d-world (2d-world view)))
    (if 2d-world (name 2d-world) "" )))

;;;(fmakunbound 'set-selection-panel-image)
(defmethod set-selection-panel-image (image view)
  (let* ((interactor *interactor*)
	 (panel *cme-selection-panel*))
    (when view
      (let* ((pos (current-window-pos interactor))
	     (2d-world (2d-world view))
	     ;;(2d-world-name (if 2d-world (name 2d-world) "" ))
	     (2d-world-name (selection-panel-image-descr view))
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
	      (cvv-item-value panel 'fragment) (selection-panel-pos-string pos)
	      (cvv-item-value panel 'object-pos) (if image (selection-panel-pos-string image-pos) "")
	      (cvv-item-value panel 'world) 2d-world-name
	      (cvv-item-value panel 'world-pos) (selection-panel-pos-string 2d-pos)
	      (cvv-item-value panel 'pixel)
	      (if image 
		  (bind-vector-elements (u v) image-pos (image-pixel-string image u v))
		  ""))
	;;(tk::map-cvv-item (cvv-item panel 'pixel))
	))))

(defun update-selection-panel ()
  (when *cme-selection-panel*
    (let* ((interactor *interactor*)
	   (sel (selected-objects interactor)))
      (if sel
	  (set-selection-panel-object (car (car sel)) (cadr (car sel)))

	  (let* ((win (current-window interactor))
		 (view (top-view win))
		 (image (and view  (view-image view))))
	    (when view 
	      (set-selection-panel-image image view)))))))
