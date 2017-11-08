(in-package :gui)


(defclass user-interface-context ()())

(defclass basic-elt-popup-ui (user-interface-context) ())

(defvar *default-popup-ui* (make-instance 'basic-elt-popup-ui))
(defvar *current-user-interface-context* *default-popup-ui*)
(defvar *user-interface-context-override* nil)


(defmethod get-user-interface-context ((interactor interactor)
				       &optional (current-window (current-window interactor)))
  (if (null current-window)
      (format t "~%Select a window first!")
      (let ((current-view (top-view current-window))
	    (frame (pane-frame current-window)))
	(or *user-interface-context-override*
	    (and current-view (get-prop current-view :user-interface-context))
	    (get-prop current-window :user-interface-context)
	    (and frame (get-prop frame :user-interface-context))
	    *current-user-interface-context*))))



(defmethod full-object-popup-menu-item-list ((ui t) (object t))
  (object-popup-menu-item-list ui object))

;;; This is called when selected-objects is non-NIL. 
(defmethod full-object-popup-menu-item-list ((ui t) (objfrags cons))
  (when (> (length objfrags) 1)
    ;; It isn't clear what to do when there are multiple selected objects.  Perhaps only the
    ;; intersection of the allowed options should be available.
    (format t ";;; object-popup-menu-item-list: multiple selected objects ~a~%" objfrags))
  (let* ((objfrag (car objfrags))
	 (object (car objfrag)))
    ;;(format t "calling full-object-popup-menu-item-list ~a ~a~%" ui object)
    (full-object-popup-menu-item-list ui object)))

(defmethod object-popup-menu-item-list (ui object)
  `(("Null" nil)))

(defmethod object-popup-menu-item-list ((ui basic-elt-popup-ui) (object t))
  `( ;;("Cycle Stack" :eval (cycle-widget-stack (current-window *interactor*)))
    ("Copy View" :accel <alpha-beta-right> :eval (com-copy-view *interactor*))
    ("Move View" :accel <alpha-beta-middle>  :eval (com-move-view *interactor*))
    ("Pop View" :eval  (com-pop-view *interactor* ))))


(defmethod object-popup-menu-item-list ((ui basic-elt-popup-ui) (object view))
  (object-popup-menu-item-list ui (view-image object)))

#+never ; old
(defmethod object-popup-menu-item-list ((ui basic-elt-popup-ui) (object img::image))
  `(("<more>" :menu (("Cycle Stack" :eval (cycle-stack (current-window)))))
    ("Select" :eval (progn (when (selected-objects)
			     (unhighlight-selected-objects))
			   (start-drag *interactor* 'drag-select-object 'drag-select)))
    ("Drag" :accel <alpha-left> :image-drag 'drag-uv)
    ("Scale/Rot Image @Mouse" :accel <middle> :image-drag 'scale-rot-at-mouse)
    ("Scale/Rot Image @C" :accel <alpha-middle> :image-drag 'scale-rot-at-center)
    ("Zoom to Fit" :eval (zoom-to-fit *interactor*))
    ("Zoom to Max" :eval (zoom-1-to-1 *interactor*))
    ("Cycle Stack" :accel <alpha-beta-left> :eval (cycle-stack (current-window)))
    ;;("Move View" :eval (move-image-from (current-window)))
    ("Move View" :accel <alpha-beta-middle>  :eval (com-move-view *interactor*))
    ("Copy View" :accel <alpha-beta-right>  :eval (com-copy-view *interactor*))
    ("Pop View" :eval  (com-pop-view *interactor* ))
    ))

(defmethod image-ops-submenu ((object img::image))
  '(("Image Blur" :eval (com-unary-image-op 'img::fast-gauss-convolve *interactor*))
    ("Image Diff" :eval (com-binary-image-op 'img::image-subtract *interactor*))
    ("Image Add" :eval (com-binary-image-op 'img::image-add *interactor*))
    ("Zero Crossings" :eval (com-unary-image-op 'img::zero-crossing-image *interactor*))
    ("Sobel" :eval  (com-unary-image-op 'img::image-sobel *interactor*))
    ))

(defmethod image-ops-submenu :around ((object img::vector-image))
  (append
   (call-next-method)
   '(("Scalar" :eval (com-unary-image-op 'img::ensure-scalar-image *interactor*)))))

;;#+new ; experimental Tue Dec  7 2004
(defmethod object-popup-menu-item-list ((ui basic-elt-popup-ui) (object img::image))
  `(("<more>" :menu (("Cycle Stack" :eval (cycle-stack (current-window)))
		     ("Grab Window" :eval (com-grab-window *interactor*))
                     ))
    ("Image Ops" :menu ,(image-ops-submenu object))
    ("Select" :eval (progn (when (selected-objects)
			     (unhighlight-selected-objects))
			   (start-drag *interactor* 'drag-select-object 'drag-select)))
    ("Drag" :accel <middle> :image-drag 'drag-uv)
    (nil :accel tk::<button4> :eval (scroll-zoom-out *interactor*) :wheel-op t)
    (nil :accel tk::<button5> :eval (scroll-zoom-in *interactor*) :wheel-op t)
    ("Scale/Rot Image @Mouse" :accel <alpha-left> :image-drag 'scale-rot-at-mouse)
    ("Scale/Rot Image @C" :accel <alpha-middle> :image-drag 'scale-rot-at-center)
    ("Zoom to Fit" :eval (zoom-to-fit *interactor*))
    ("Zoom to Max" :eval (zoom-1-to-1 *interactor*))
    ("Cycle Stack" :accel <alpha-beta-left> :eval (cycle-stack (current-window)))
    ;;("Move View" :eval (move-image-from (current-window)))
    ("Move View" :accel <alpha-beta-middle>  :eval (com-move-view *interactor*))
    ("Copy View" :accel <alpha-beta-right>  :eval (com-copy-view *interactor*))
    ("Pop View" :eval  (com-pop-view *interactor* ))
    ("View Attributes" :eval (com-view-attributes-panel *interactor*))
    ("Tandem" :accel <gamma-left> :eval (com-toggle-tandem-view *interactor*))
    ))



(defmethod com-cvv-object-menu ((interactor interactor))
  )

(defclass cme-object-popup-ui (user-interface-context) ())

(defmethod basic-object-popup-menu-item-list ((ui cme-object-popup-ui) (object basic-gl-object))
  `(;;("Pin This Menu" :eval (com-pin-object-popup-menu *interactor*))
    ("Parameters" :accel <alpha-right> :eval (com-cvv-object-menu *interactor*))
    ("Copy Object" :eval (com-copy-object *interactor*))
    ("Radius Class" :eval (com-change-class *interactor*)) ;; Really ought to be embedded in cme-compat.
    ;;("Accelerator Menu" describe-accelerator-menu)
    ;; ("Commands" ,(object-popup-menu-extension-item-list ui object))
    #+never ("Select" :eval (com-select-object *interactor*) )
    #+never ("Quit/Return" stop-tracking-return-handle :documentation "Quit Object Modification and Return Object")
    ("" :separator)
    ))


(defmethod full-object-popup-menu-item-list  ((ui cme-object-popup-ui) (object basic-gl-object))
  (append `(("<more> ..." :menu ,(object-popup-menu-extension-item-list ui object)  :documentation "Extended Popup Menu"))
	  (basic-object-popup-menu-item-list ui object)
	  (object-popup-menu-item-list ui object)))


;;;
;;; Copy the object, displace it a bit in XY, place it in the same
;;; feature sets, and select it.  At that point, the user can do
;;; whatever they want with it.
;;;
(defun com-copy-object (interactor)
  (let* ((old-object (selected-object interactor))
	 (new-object (obj::copy-object old-object))
	 (fs-list (object-sets-containing-objects (list old-object))))
    (move-by (object-to-parent-transform new-object) (cv 1.0 1.0 0.0))
    (loop for fs in fs-list do (add-object new-object fs))
    (select-object new-object interactor)
    (redisplay (current-view interactor))
    ;; (start-drag interactor :drag-uv  'drag-select)
    ))


(defun com-window-image (interactor)
  (let ((new-pane (pick-a-pane "Pick a pane for the new image window.")))
    (when new-pane
      (push-image
       (window-image-with-rectangle
	(selected-object interactor)
	(view-image (current-view interactor)))
       new-pane))))


#|
(setq obj (caar (selected-objects)))
(setq ui-context (get-user-interface-context *interactor*))
(basic-object-popup-menu-item-list ui-context obj)
(object-popup-menu-item-list (get-user-interface-context *interactor*) (selected-object))
|#

;;; *******************************  KEYPRESS-CALLBACKS  *******************************

;;; Should all of the keypress-callback move to ui-classes.lisp, or somewhere else?
;;; Should the keypress-callbacks act on the selected-window or the window under the mouse?

;;; I suggest that all of Chris's keypress-callback bindings should
;;; have ui-context specialized on some mixin rather than T. - done (CC 12/21/2010)

(defmethod keypress-callback ((key (eql :l)) (ui-context cme-object-popup-ui))
  (setf (single-button-mouse-button) 1))

(defmethod keypress-callback ((key (eql :m)) (ui-context cme-object-popup-ui))
  (setf (single-button-mouse-button) 2))

(defmethod keypress-callback ((key (eql :r)) (ui-context cme-object-popup-ui))
 (basic-elt-popup-ui-popup-menu *interactor*))

(defvar *scroll-view-delta* 20.0)

(defmethod keypress-callback ((key (eql :up)) (ui-context cme-object-popup-ui))
  (scroll-view (top-view) 0.0 *scroll-view-delta*))

(defmethod keypress-callback ((key (eql :down)) (ui-context cme-object-popup-ui))
  (scroll-view (top-view) 0.0 (- *scroll-view-delta*)))

(defmethod keypress-callback ((key (eql :left)) (ui-context cme-object-popup-ui))
  (scroll-view (top-view) *scroll-view-delta* 0.0))

(defmethod keypress-callback ((key (eql :right)) (ui-context cme-object-popup-ui))
  (scroll-view (top-view) (- *scroll-view-delta*) 0.0))

(defmethod keypress-callback ((key (eql :minus)) (ui-context cme-object-popup-ui))
  (com-zoom-out *interactor*))

(defmethod keypress-callback ((key (eql :KP_SUBTRACT)) (ui-context cme-object-popup-ui))
  (com-zoom-out *interactor*))

(defmethod keypress-callback ((key (eql :plus)) (ui-context cme-object-popup-ui))
  (com-zoom-in *interactor*))

(defmethod keypress-callback ((key (eql :KP_ADD)) (ui-context cme-object-popup-ui))
  (com-zoom-in *interactor*))

(declaim (special *photometric-transform-override*))

(defmethod keypress-callback ((key (eql :d))  (ui-context cme-object-popup-ui))
  (print
   (setf *photometric-transform-override*
	 (unless *photometric-transform-override*
	   '(:linear 0.5 0.0))))
  (map-over-all-views (view)
   ;(img::release-image-pool-textures (view-image view))
   (window-damaged *interactor* (view-window view))))

(defparameter *unknown-keypress-verbose* nil)
;(setq *unknown-keypress-verbose* t)

(defmethod keypress-callback ((key t) (ui-context cme-object-popup-ui))
  (when *unknown-keypress-verbose*
    (format t "~%Unknown keypress key: ~a~%" key)))


;;;
;;; This causes the space bar to be a shortcut for selecting the whole
;;; composite object (if possible).  No-op if the object is stand-alone.

(defmethod keypress-callback ((key (eql :space)) (ui-context cme-object-popup-ui))
  (com-close-self *interactor*))

;;;  ***************************   EXTENDABLE USER INTERFACE CONTEXT  ******************************

;;; This class allows dynamic extension to the menu items of the popup menus by adding
;;; USER-UI-EXTENSION instances to the EXTENSIONS slot of the EXTENDABLE-USER-INTERFACE-CONTEXT.
;;; Each of these USER-UI-EXTENSIONs can provide additional items to the popup menu item lists.
;;; The EXTENSIONS slot can the dynamically modified and the menus will reflect the changes.

(defclass extendable-user-interface-context (user-interface-context)
    ((extensions :initform nil :initarg :extensions :accessor extensions))
  )


(defmethod object-popup-menu-item-list ((ui extendable-user-interface-context) object)
  (loop for iui in (extensions ui)
	append (object-popup-menu-item-list iui object)))



;;; This class retains the old accelerator-menu binding and adds popup menus
(defclass augmented-cme-ui (;; extendable-user-interface-context
			    ;;basic-object-accelerators
			    ;; old-cme-accelerators
			    ;; ic::old-ic-accelerators
			    cme-object-popup-ui
			    basic-elt-popup-ui
			    )
    ())

#+never ; old
(defmethod object-popup-menu-item-list ((ui augmented-cme-ui) (object synthetic-view))
  `(("<more>" :menu (("Cycle Stack" :eval (cycle-stack (current-window)))
		     ("Cycle Mode" :eval (cycle-display-mode *interactor*))
		     ))
    ("Drag" :accel <alpha-left> :image-drag 'drag-uv)
    ("Scale/Rot @Mouse" :accel <middle> :image-drag 'scale-rot-at-mouse)
    ("Scale/Rot @Center" :accel <alpha-middle> :image-drag 'scale-rot-at-center)
    ("Cycle Stack" :accel <alpha-beta-left> :eval (cycle-stack (current-window)))
    ;;("Move View" :eval (move-image-from (current-window)))
    ("Move View" :accel <alpha-beta-middle>  :eval (com-move-view *interactor*))
    ("Copy View" :accel <alpha-beta-right>  :eval (com-copy-view *interactor*))
    ("Pop View" :eval  (com-pop-view *interactor* ))
    ("View Tool" :eval (com-site-view-tool *interactor*))
    ))

;; new Sun Dec 12 2004
(defmethod object-popup-menu-item-list ((ui augmented-cme-ui) (object synthetic-view))
  `(("<more>" :menu (("Cycle Stack" :eval (cycle-stack (current-window)))
                     ("Cycle Mode" :eval (cycle-display-mode *interactor*))
		     ("Grab Window" :eval (com-grab-window *interactor*))
		     ))
    ("Drag" :accel <middle> :image-drag 'drag-uv)
    (nil :accel tk::<button4> :eval (scroll-zoom-out *interactor*) :wheel-op t)
    (nil :accel tk::<button5> :eval (scroll-zoom-in *interactor*) :wheel-op t)
    ("Scale/Rot @Mouse" :accel <alpha-left> :image-drag 'scale-rot-at-mouse)
    ("Scale/Rot @Center" :accel <alpha-middle> :image-drag 'scale-rot-at-center)
    ("Cycle Stack" :accel <alpha-beta-left> :eval (cycle-stack (current-window)))
    ;;("Move View" :eval (move-image-from (current-window)))
    ("Move View" :accel <alpha-beta-middle>  :eval (com-move-view *interactor*))
    ("Copy View" :accel <alpha-beta-right>  :eval (com-copy-view *interactor*))
    ("View Attributes" :eval (com-view-attributes-panel *interactor*))
    ("Pop View" :eval  (com-pop-view *interactor* ))
    ("Tandem" :accel <gamma-left> :eval (com-toggle-tandem-view *interactor*))
;;    ("View Tool" :eval (com-site-view-tool *interactor*)) ;; Should not be defined here.
    ))



;;;
;;; Why is this not the extendable UI context?
;;;

(setq *default-popup-ui*  (make-instance 'augmented-cme-ui))
(setq *current-user-interface-context* *default-popup-ui*)

;; (defparameter *default-ui-class* 'augmented-cme-ui)

#|
(eval-cache-flush-function 'get-mouse-event-table)
(lx::eval-cache-flush-function 'tk::make-menu)
|#

; cme (:control :meta :right) com-move-w-to-ground "Drop W"

(defmethod object-popup-menu-item-list  ((ui cme-object-popup-ui) (object gl-3d-object-mixin))
  '(("Drop To DTM" :accel <alpha-beta-right> :eval (com-move-w-to-ground *interactor*))
    ("Move On DTM" :accel <alpha-beta-left> :object-drag 'move-object-uv-on-dtm)
    ("Move XY" :accel <alpha-left> :object-drag 'move-object-uv)
    ("Move W" :accel <alpha-middle> :object-drag 'move-object-w)
    ;("Move Vert XY" :accel <beta-gamma-left> :object-drag 'move-object-vertex-uv)
    ;("Drop Vert to DTM" :accel <beta-gamma-left> :eval (com-move-object-vertex-uv-to-dtm *interactor*))
    ;("Move Vert On DTM" :accel <beta-gamma-left> :object-drag 'move-object-vertex-uv-on-dtm)
    ("" :separator)
    ("Roll" :accel <gamma-left> :object-drag 'roll-object)
    ("Rot Z" :accel <alpha-right>  :object-drag 'rotate-about-object-z)
    ("Z Up" :eval (com-canonicalize-orientation *interactor*))
    ))


(defmethod object-popup-menu-item-list  ((ui cme-object-popup-ui) (object obj::3d-curve))
  '(("Drop To DTM" :accel <alpha-beta-right> :eval (com-move-w-to-ground *interactor*))
    ("Move On DTM" :accel <alpha-beta-left> :object-drag 'move-object-uv-on-dtm)
    ("Drape Over DTM" com-move-every-w-to-ground)
    ("Move XY" :accel <alpha-left> :object-drag 'move-object-uv)
    ("Move W" :accel <alpha-middle> :object-drag 'move-object-w)
    ;("Move Vert XY" :accel <beta-gamma-left> :object-drag 'move-object-vertex-uv)
    ;("Drop Vert to DTM" :accel <beta-gamma-left> :eval (com-move-object-vertex-uv-to-dtm *interactor*))
    ;("Move Vert On DTM" :accel <beta-gamma-left> :object-drag 'move-object-vertex-uv-on-dtm)
    ("" :separator)
    ("Roll" :accel <gamma-left> :object-drag 'roll-object)
    ("Rot Z" :accel <alpha-right>  :object-drag 'rotate-about-object-z)
    ("Z Up" :eval (com-canonicalize-orientation *interactor*))
    ))


(defmethod object-popup-menu-item-list  ((ui cme-object-popup-ui) (object obj::3d-ribbon-curve))
  '(("Drop To DTM" :accel <alpha-beta-right> :eval (com-move-w-to-ground *interactor*))
    ("Move On DTM" :accel <alpha-beta-left> :object-drag 'move-object-uv-on-dtm)
    ("Drape Over DTM" :eval (com-move-every-w-to-ground *interactor*))
    ("Move XY" :accel <alpha-left> :object-drag 'move-object-uv)
    ("Move W" :accel <alpha-middle> :object-drag 'move-object-w)
    ;("Move Vert XY" :accel <beta-gamma-left> :object-drag 'move-object-vertex-uv)
    ;("Drop Vert to DTM" :accel <beta-gamma-left> :eval (com-move-object-vertex-uv-to-dtm *interactor*))
    ;("Move Vert On DTM" :accel <beta-gamma-left> :object-drag 'move-object-vertex-uv-on-dtm)
    ("" :separator)
    ("Roll" :accel <gamma-left> :object-drag 'roll-object)
    ("Rot Z" :accel <alpha-right>  :object-drag 'rotate-about-object-z)
    ("Z Up" :eval (com-canonicalize-orientation *interactor*))
    ))


(defmethod object-popup-menu-item-list  ((ui cme-object-popup-ui) (object gl-2d-object-mixin))
  '(("Move UV" :accel <alpha-left> :object-drag 'move-object-uv)
    ("Move Vert UV" :accel <beta-gamma-left> :object-drag 'move-object-vertex-uv)
    ("Rotate":accel <alpha-right> :object-drag 'rotate-about-object-z)
    ))


(defmethod object-popup-menu-item-list :around ((ui cme-object-popup-ui) (object obj::2d-rectangle))
  (append
   (call-next-method)
   '(("Image Window" :eval (com-window-image *interactor*)))))

(defmethod object-popup-menu-item-list  ((ui cme-object-popup-ui) (object obj::gl-xy-sizable-object-mixin))
  '(("Deselect Object" :eval (deselect-objects *interactor*))
    ("Move UV" :accel <alpha-left> :object-drag 'move-object-uv )
    ("Move Vert UV" :accel <beta-gamma-left> :object-drag 'move-object-vertex-uv)
    :separator ; ("" :separator)
    ("Rot Z" :accel <alpha-right>  :object-drag 'rotate-about-object-z)
    ;;  ("Z Up" :eval (com-canonicalize-orientation *interactor*))
    ("Scale/Rot" :accel <gamma-middle> :object-drag 'rotate-resize-object-xy)
    ("UV Size"  :accel <gamma-left> :object-drag 'resize-object-xy))
  )


(defmethod object-popup-menu-item-list  ((ui cme-object-popup-ui) (object obj::gl-xyz-sizable-object-mixin))
  '(("Deselect Object" :eval (deselect-objects *interactor*))
    ("Drop To DTM" :accel <alpha-beta-right> :eval (com-move-w-to-ground *interactor*))
    ("Move On DTM" :accel <alpha-beta-left> :object-drag 'move-object-uv-on-dtm)
    ("Move XY" :accel <alpha-left> :object-drag 'move-object-uv )
    ("Move W" :accel <alpha-middle> :object-drag 'move-object-w )
    ("Move Vert XY" :accel <beta-gamma-left> :object-drag 'move-object-vertex-uv)
    ("Move Vert W" :accel <beta-gamma-middle> :object-drag 'move-object-vertex-w)
    ("Drop Vert to DTM" :accel <beta-gamma-right> :eval (com-move-object-vertex-uv-to-dtm *interactor*))
    ;;("Move Vert On DTM" :accel <beta-gamma-left> :object-drag 'move-object-vertex-uv-on-dtm)
    :separator ; ("" :separator)
    ;;  ("Roll" :accel <alpha-gamma-left> :object-drag 'roll-object)
    ("Rot Z" :accel <alpha-right>  :object-drag 'rotate-about-object-z)
    ;;  ("Z Up" :eval (com-canonicalize-orientation *interactor*))
    ("Scale/Rot" :accel <gamma-middle> :object-drag 'rotate-resize-object-xy)
    ("XY Size"  :accel <gamma-left> :object-drag 'resize-object-xy)
    ("Z Size"  :accel <gamma-right> :object-drag 'resize-object-z))
  )

(defmethod object-popup-menu-item-list :around  ((ui cme-object-popup-ui) (object obj::house-object))
  (append (call-next-method)
          `(("Roof Pitch" :object-drag 'roof-pitch-drag))))

;(typep (selected-object) 'obj::gl-xyz-sizable-object-mixin)
  
;;; ****************************   OBJECT-POPUP-MENU-EXTENSION-ITEM-LIST  ****************************

(defmethod object-popup-menu-extension-item-list ((ui cme-object-popup-ui) (object basic-gl-object))
  '(("Accel Menu" :eval (describe-bucky-menu *interactor*) :documentation "Command Accelerator Menu (aka Bucky Menu)" )
    ("Open Parent" :eval (com-close-self *interactor*) :documentation "Make Parent Sensitive for Selection")
    ("Undo" :eval (com-undo *interactor*))
    ("Redo" :eval (com-redo *interactor*))
    ("Clone" :eval (com-clone-and-grab *interactor*))
    ("Blank" :eval (com-com-blank-object2 *interactor*))
    ("Delete" :eval (com-delete-object *interactor*))
    ("" :separator)
    ))

(defmethod object-popup-menu-extension-item-list :around ((ui cme-object-popup-ui) (object obj::gl-xy-sizable-object-mixin))
  (append (call-next-method)
	  '(("Scale" :object-drag 'resize-object))))

(defmethod object-popup-menu-extension-item-list :around ((ui cme-object-popup-ui) (object obj::gl-xyz-sizable-object-mixin))
  (append (call-next-method)
	  '(("Roll" :accel <alpha-gamma-left> :object-drag 'roll-object)
	 ;   ("Rot Z":accel <beta-right> :object-drag 'rotate-about-object-z)
	    ("Z Up" :eval (com-canonicalize-orientation *interactor*))
	    ("Scale" :object-drag 'resize-object)
	    ("Taper" :object-drag 'taper-object)
	    ("Sun Z" :object-drag 'move-sun-w))))

(defmethod object-popup-menu-item-list :around ((ui cme-object-popup-ui) (object obj::basic-curve))
  (append (call-next-method)
	  '(("Move Vert XY" :accel <beta-gamma-left> :object-drag 'move-object-vertex-uv)
	    ("Move Vert W" :accel <beta-gamma-middle> :object-drag 'move-object-vertex-w)
	    ("Add Vert" :accel <beta-left> :eval (com-add-vertex *interactor*))
	    ("Del Vert" :accel <beta-middle> :eval (com-delete-vertex *interactor*)))))

(defmethod object-popup-menu-item-list :around ((ui cme-object-popup-ui) (object obj::ribbon))
  (append (call-next-method)
	  '(("Width" :object-drag 'change-ribbon-width))))


#| THE REMAINDER OF THIS FILE IS IN COMMENTS

(defmethod full-object-popup-menu-item-list ((ui t) (object t))
  (object-popup-menu-item-list ui object))

(defclass cme-object-popup-ui (user-interface-context) ())

(defmethod basic-object-popup-menu-item-list ((ui cme-object-popup-ui) (object basic-object))
  `(;;("Pin This Menu" com-pin-object-popup-menu)
    ("Parameters" com-cvv-object-menu)
    ;;("Accelerator Menu" describe-accelerator-menu)
   ;; ("Commands" ,(object-popup-menu-extension-item-list ui object))
    #+never ("Select" com-select-object)
    ;;#+never
    ("Quit/Return" stop-tracking-return-handle
     :documentation "Quit Object Modification and Return Object")
    ("" :separator)
    ))

(defmethod full-object-popup-menu-item-list  ((ui cme-object-popup-ui) (object basic-object))
  (append `(("<more> ..." ,(object-popup-menu-extension-item-list ui object)  :documentation "Extended Popup Menu"))
	  (basic-object-popup-menu-item-list ui object)
	  (object-popup-menu-item-list ui object)))



(defmethod object-popup-menu-item-list  ((ui cme-object-popup-ui) (object basic-object))
  nil)

;;;(defparameter *command-documentation-alist*
;;;  '((com-move-w-to-ground "Place Object at Intersection of Ray from View with DEM")
;;;    (:uvxy "Move Object by Projecting UV Mouse Motion to World XY @ Constant Z.")
;;;    (com-move-z-to-ground "Drop Object Vertically to Surface.")
;;;    (com-move-z-to-ground "Drop Object Vertically to Surface.")
;;;    (:az-elev "Mouse X Rotates about World Z, Mouse Y Rotates about Object X")
;;;    (:z-rot "Rotate about World Z")
;;;    (:zp-rot "Rotate about Object Z")
;;;    (:uv-roll "Rotate Object as rolling ball")
;;;    (com-reset-orientation "Reset to Canonical Orientation (Z axis up).")
;;;    (:w-rot "Rotate Object about the Ray from View")
;;;    (com-canonicalize-shape "Restore Vertices to Default Positions for Object")
;;;    (:change-scale "Change x,y,z sizes.")
;;;    (com-open-vertices "Allow Vertex Modification")
;;;    (com-close-vertices "Close Vertex Modification")
;;;    (com-close-self "Open Superior to Object")
;;;    (com-add-vertex "Add a new Vertex")
;;;    (com-delete-vertex "Delete Vertex")
;;;    (com-reset-curve "Delete All Vertices Except the First")
;;;    (com-make-rectilinear "Make Top Face Rectilinear")
;;;    (com-make-horizontal "Make Every Vertex have same value of Z.")
;;;    (:uv-skew "Rotate camera v axis relative to u axis")
;;;    (:uv-aspect-ratio "Change uv aspect ratio")
;;;    (com-canonicalize-orientation "Reset to Canonical Orientation (Z axis up).")
;;;    ))

(defparameter *command-documentation-alist*
  '(#|(:CHANGE-ATTENUATION) (:CHANGE-BLUE) (:CHANGE-BRIGHTNESS)
    (:CHANGE-BRIGHTNESS-AND-CONTRAST) (:CHANGE-CONTRAST) (:CHANGE-GAMMA)
    (:CHANGE-GREEN) (:CHANGE-HUE) (:CHANGE-INTENSITY) (:CHANGE-OFFSET)
    (:CHANGE-RED) (:CHANGE-SATURATION) (:CHANGE-THRESHOLD) (AUTO-STRETCH)
    (COM-RESET-COLOR-MAP) (NEGATE-CONTRAST) (NORMALIZE-STRETCH-LIMITS)
    |#
    (:AZ-ELEV "Mouse X Rotates about World Z, Mouse Y Rotates about Object X")
    (:AZ-ELEV "Mouse X Adjusts Azimuth Angle, Mouse Y Adjusts Elevation Angle.")
    (:CHANGE-EXPTS "Change ALL Exponents.")
    (:CHANGE-ORIENTATION-AND-SIZE
     "Adjust XY Size and Orientation by Dragging Diagonally Opposite Vertex.")
    (:CHANGE-RADIUS-AND-LENGTH
     "Mouse X Changes Quanset Radius.  Mouse Y changes Quanset Length.")
    (:CHANGE-ROOF-PITCH "Change House Roof Pitch.")
    (:CHANGE-SCALE "Change Size of Object.")
    (:CHANGE-TAPER-RATE "Change Object Taper.")
    (:CHANGE-TOTAL-SCALE "Change Size of Sun Ray.")
    (:CHANGE-X-EXPT "Change X Exponent.")
    (:CHANGE-XY-EXPT "Change X and Y Exponents.")
    (:CHANGE-XY-SCALE "Change Cylinder Radius.")
    (:CHANGE-XY-SIZES
     "Change X and Y Sizes, by Dragging Diagonally Opposite Vertex.")
    (:CHANGE-Y-EXPT "Change Y Exponent.") (:CHANGE-Z-EXPT "Change Z Exponent.")
    (:CHANGE-Z-SIZE "Change Z Size.")
    (CLONE-AND-GRAB "Create an Identical Copy of Object.")
    (COM-ADD-VERTEX "Add a new Vertex.")
    (COM-BLANK-OBJECT "Temporarily Remove Object from Views.")
    (COM-BLANK-OBJECT2 "Temporarily Remove Object from Views.")
    (COM-CANONICALIZE-ORIENTATION "Reset to Canonical Orientation (Z axis up).")
    (COM-CANONICALIZE-SHAPE "Restore Vertices to Default Positions for Object.")
    (COM-CLOSE-SELF "Make Object Insensitive to the Mouse (Superior becomes sensitive).")
    (COM-CLOSE-VERTICES "Close Vertex Modification.")
    (com-cvv-object-menu "Popup Menu of Object Parameters.")
    (com-cvv-image-menu "Popup Menu of Image Parameters.")
    (COM-DELETE-2D-CONSTRAINT "Delete Image Position Constraint for this Image.")
    (COM-DELETE-VERTEX "Delete Vertex.")
    (COM-DELETE-VERTEX-OR-ARC "Delete Vertex or Arc.") (COM-DESELECT-AS-SUPERIOR)
    (COM-DESENSITIZE-IN-VIEW "Desensitize this Feature Set in this View.")
    (COM-EXTRUDE "Create Extruded Object from Curve.")
    (COM-HIDE-IN-VIEW "Hide this Feature Set in this View.")
    (COM-MAKE-HORIZONTAL "Make Top Face Horizontal.")
    (COM-MAKE-RECTILINEAR "Make Top Face Rectilinear.")
    (COM-MENU-CLICK "Object Popup Menu.")
    (COM-MERGE-VERTICES "Merge this Vertex with Nearest other Vertex.")
    (COM-MOVE-EVERY-W-TO-GROUND "Place Every Vertex on Terrain Surface Retaining 2d Positions in this View.")
    (COM-MOVE-W-TO-GROUND "Place Object on Terrain Surface Retaining 2d Position of this Vertex in this View.")
    (COM-MOVE-Z-TO-GROUND "Drop Object Vertically to Terrain Surface.")
    (COM-OPEN-INFERIOR "Make Inferiors of this Object Sensitive to the Mouse (this object becomes insensitive).")
    (COM-OPEN-VERTICES "Allow Vertex Modification.") (COM-POSITION-STARE-POINT)
    (COM-REMOVE-FACE-TEXTURE-MAP "Remove Texture Map Associated with Face.")
    (COM-REMOVE-OBJECT-TEXTURE-MAPS "Remove Texture Maps Associated with Object.")
    (COM-RESECTION-CONJUGATE-POINTS-IMPROVE "Adjust Camera Model using RESECTION Algorithm, Improving Current Estimate.")
    (COM-RESECTION-CONJUGATE-POINTS-MENU "Pop Up Menu for Camera Model RESECTION Algorithm.")
    (COM-RESET-CURVE "Delete All Vertices Except the First.")
    (COM-RESET-NETWORK "Delete All Vertices Except the First.")
    (COM-RESET-ORIENTATION "Reset to Canonical Orientation (Z axis up).")
    (COM-SELECT-AS-SUPERIOR)
    (COM-SENSITIZE-IN-VIEW "Ssensitize this Feature Set in this View.")
    (COM-SET-CONJUGATE-POSITION-IN-VIEW "Set the 2d Position of Conjugate Point in any View.")
    (COM-SET-FACE-TEXTURE-MAP "Associate Texture Map with Face.")
    (COM-SET-OBJECT-TEXTURE-MAPS "Associate Texture Maps with Object.")
    (COM-SET-POSITION-AT-VERTEX "Position Object to coincide with a Vertex of another Object.")
    (COM-SET-SELECTED-POINT-AT-VERTEX "Position Object to coincide with a Vertex of another Object.")
    (COM-SET-VIEW-SUN-VECTOR "Update the Sun Vector of this View.")
    (COM-SPLIT-VERTEX "Create New Vertices for Each Arc from this Vertex.")
    ;;(COM-STOP-TRACKING)
    (:CONJ-XY "Change 2d Position of Conjugate Point in this View.")
    (DELETE-OBJECT "Delete this Object.")
    (DESCRIBE-BUCKY-MENU "Pop up Object Accelerator Menu (aka Bucky Menu).")
    (:FOCAL-LENGTH "Adjust Camera Focal Length.")
    (MAKE-WINDOW "Create Sub-Image correspoingind to Window.")
    (:MOVE-CAM-W "Change the Distance of Camera from Stare Point.")
    (:MOVE-EDGE "Move Edge or Corner of Rectangle Object.")
    (:MOVE-TEXT "Change the Position of Text Object.")
    (:MOVE-Z "Change Object Z Position.")
    (MOVE-Z-TO-GROUND "Drop Object Vertically to Surface.")
    (:PRINC-PT "Change Camera Principal Point Position.")
    (REDO "REDO previous UNDO command.")
    (:ROTATE-SCALE-XY
     "Adjust XY Size and Orientation by Dragging Diagonally Opposite Vertex.")
    (STOP-TRACKING-RETURN-HANDLE "Stop Modifications to this Object, return Object Instance to Listener.")
    (:SUN-W
     "Adjust Object Height using Sun Model and Shadow on Terrain Surface, Burying Foundation.")
    (:TANDEM-SCROLL "Scroll Tandem Views")
    (UNDO "UNDO Previous Command on this Object.")
    (:UV-ASPECT-RATIO "Change uv aspect ratio in Camera Film Plane.")
    (:UV-ON-DTM "Move Object on Terrain Surface.")
    (:UV-ROLL "Rotate Object as Rolling Ball.")
    (:UV-SKEW "Rotate camera v axis relative to u axis")
    (:UVXY "Move Object in Plane of Constant Z.")
    (:VERTEX-UV-ON-DTM "Move this Vertex on Terrain Surface.")
    (:VERTEX-UVXY "Move this Vertex in Plane of Constant Z.")
    (:VERTEX-W "Move this Vertex along Camera Ray from this View.")
    (:VERTEX-Z "Change Vertex Z Position.")
    (:W "Move Object along Camera Ray from this View.")
    (:W-FOCAL-LENGTH
     "Change both Distance to Stare-Point and Focal-length, maintaining constant GSD at Stare-Point.")
    (:W-ROT "Rotate this Object about the Camera Ray from this View.")
    (:Z-ROT "Rotate this Object about World Z.")
    (:ZP-ROT "Rotate this Object about Object Z.")

    (IC::COM-SELECT-PANE "Select the pane pointed to by the mouse.")
    (IC::COM-REFRESH-PANE "Refresh the contents of the Specified Pane.")
    (IC::COM-IMAGE-CALC-MAIN-MENU "Top Level Menu")
    (IC::COM-REPOSITION-PERCENTWISE "Recenter the Image percentage-wise.  Hold button for scroll.")
    (IC::COM-REPOSITION-PERCENTWISE2 "Recenter the Image percentage-wise.")
    (ic::com-drag "Drag (scroll) the Image using Mouse.")
    (IC::COM-ZOOM-to-fit "Zoom-Out this Image until it fits inside this Window.")
    (IC::COM-ZOOM-to-fit-object "Zoom-Out this Image until object fits inside this Window.")
    (IC::COM-ZOOM-IN "Expand the Image by a factor of 2.")
    (IC::COM-ZOOM-OUT "Reduce Image by a factor of 2.")
    (IC::COM-ZOOM-to-full-res "Zoom this Image to Full Resolution.")
    (IC::COM-RECENTER "Center the mouse selected point. Hold button for scroll.")
    (IC::COM-RECENTER2 "Center the mouse selected point.")
    (IC::COM-COPY-VIEW "Copy View from the Source Pane to the Result Pane.")
    (IC::COM-COPY-VIEW2 "Copy View from this Pane to the Specified Pane.")
    (IC::COM-MOVE-VIEW "Pop Image from the Source Pane to the Result Pane.")
    (IC::COM-MOVE-VIEW2 "Pop Image from this Pane to the Specified Pane.")
    (IC::COM-CYCLE-PANE-STACK "Cycle the Stack by moving the Top of the Stack to the End.")
    (IC::COM-reverse-CYCLE-PANE-STACK "Reverse Cycle the Stack by moving the End of the Stack to the Top.")
    (IC::COM-KILL-VIEW "Remove the Top View from Stack")
    (IC::COM-POP-PANE-EXPUNGE "Remove the Top View from Stack and Expunge the Image")
    (IC::COM-X-SLICE "Graph a horizontal slice of the image at the mouse selected point.")
    (IC::COM-Y-SLICE "Graph a vertical slice of the image at the mouse selected point.")
    (IC::COM-HISTOGRAM "Graph the histogram of the mouse selected image.~ ;Rt for Menu")
    (IC::COM-SET-TANDEM)
    (IC::COM-FLASH-TANDEM)
    (IC::COM-CLEAR-TANDEM)
    (IC::COM-FAST-ZOOM-IN "Expand the Image by a factor of 2 by pixel replication.")
    (IC::COM-FAST-ZOOM-OUT "Reduce the Image by a factor of 2 using pixel sampling.")
    (IC::COM-REVERSE-CYCLE-PANE-STACK "Cycle the Stack by moving the End of Stack to the Top.")
    (IC::COM-DESCRIBE-OBJECT)
    (IC::COM-POP-SELECTED-IMAGE-TO-HERE-NO-SELECT "Pop View from the Selected Pane to the Result Pane, without changing selected-pane.")
    (IC::COM-WINDOW "Create Image Windowing Tool")
    (IC::COM-DESCRIBE-BUCKY-MENU "Menu of ImagCalc Bucky Commands")
    (IC::COM-STRETCH "Create Image Stretch Tool.")
    (IC::COM-AUTO-STRETCH "Stretch All Tandem Views using tail factor defined in Stretch Panel.")
    (IC::COM-NEGATE-IMAGE "Negate (Complement) Image Intensity")
    (IC::COM-FLIP-X "Mirror Image left-to-right")
    (IC::COM-FLIP-Y "Mirror Image top-to-bottom")
    (IC::COM-unROTATE "Restore this Image to its Original Orientation.")
    (IC::COM-ROTATE "Rotate Image 90 degrees clockwise.")
    (IC::COM-ROTATE-ccw "Rotate Image 90 degrees counter clockwise.")
    (IC::COM-ROTATE-180 "Rotate Image 180 degrees.")
    (ic::com-rotate-north-up "Rotate this Image so World North Direction points up in Window.")
    (ic::com-rotate-z-up "Rotate this Image so World Z-Axis points up in Window.")
    (IC::COM-INSPECT-STACK "Display the Image Stack of Pane.")
    (IC::COM-DESCRIBE-OBJECT)
    (IC::COM-INSPECT-IMAGE "Drop an Image into the Inspector")
    (IC::COM-VISIBLE-OBJECT "Make an Image from the Top View of a Pane")

    ))


#| ; must move this to a file in snakes package.
(when (find-package 'snakes)
  (setq *command-documentation-alist*
	(append *command-documentation-alist*
		'((SNAKES::2D-OPTIMIZE "Optimize the object in one view.")
		  (SNAKES::3D-OPTIMIZE "Optimize the object in several views.")
		  (SNAKES::COARSEN-CURVE "Coarsen curve.")
		  (SNAKES::COMPUTE-VERTEX-XYZ "Compute Z using stereo")q
		  (SNAKES::DRAW-EPI "Draw epi line")
		  (SNAKES::MERGE-CURVES "Merge 2 Curves.")
		  (SNAKES::SHORTEN-CURVE)
		  (SNAKES::SPLIT-CURVE "Create 2 Curves breaking Curve at this Vertex.")
		  ))))
|#

#|
(let* ((l (loop for method in (slot-value (symbol-function 'bucky-menu-list) 'clos::methods)
		for specializers = (slot-value method 'clos::specializers)
		;;collect specializers
		for object = (ignore-errors (make-instance (car specializers)))
		;; (class-prototype (car specializers))
		for bucky-menu-list = (and object(ic::condition-case () (bucky-menu-list object)
									(error () nil)))
		when bucky-menu-list
		  append bucky-menu-list
		))
       (l2 (loop for (buckys command label . rest) in l
		 collect (list* (format nil "~a ~a" command (or rest "")) command rest)))

       (l3 (sort l2 #'string-lessp :key #'car))
       (l4 (loop for (a b) on l3 by #'cdr
		 unless (equal a b)
		   collect a))
       (l5 (loop for (key command . rest) in l4
		 collect (list* command rest)))
       )
  
  (loop for thing in l5
	do (format t "~s~%" thing)))


(loop for  (fn name . rest)
      in (qui::convert-item-list ic::*ic-bucky-menu-list*)
      for documentation = (cadr (getf rest :documentation))
      for command = (getf rest :command)
      when (listp documentation)
	do (setq documentation (car documentation))
      unless (or (equal name "") (null command))
	do (format t "   ~s~%" (list* (cadr command) (and documentation (list documentation))))
      )


|#




(defmethod object-popup-menu-item-list  :around ((ui cme-object-popup-ui) (object 3d-object))
	   (append '(("Zoom to Fit Object" com-zoom-to-fit-object)
		     ("" :separator))
		   (call-next-method)))

(defmethod object-popup-menu-item-list  :around ((ui cme-object-popup-ui) (object 2d-object))
	   (append '(("Zoom to Fit Object" com-zoom-to-fit-object)
		     ("" :separator))
		   (call-next-method)))

  
(defmethod object-popup-menu-item-list  ((ui cme-object-popup-ui) (object 3d-object))
  '(("Drop To DTM" com-move-w-to-ground)
    ("Move On DTM" :uv-on-dtm )
    ("Move XY" :uvxy )
    ("Move W" :w )
    ("" :separator)
    ("Roll" :uv-roll)
    ("Rot Z":z-rot)
    ("Z Up" com-canonicalize-orientation)
    ))

(defmethod object-popup-menu-item-list  ((ui cme-object-popup-ui) (object 2d-object))
  '(("Move UV" :uvxy )
    ("Rotate" :z-rot)
    ))

(defmethod object-popup-menu-item-list  ((ui cme-object-popup-ui) (object obj::gl-xyz-sizable-object-mixin))
  '(("Drop To DTM" com-move-w-to-ground)
    ("Move On DTM" :uv-on-dtm )
    ("Move XY" :uvxy )
    ("Move W" :w )
    ("" :separator)
    ("Scale/Rot" :rotate-scale-xy)
    ("XY Size"  :change-xy-sizes)
    ("Z Size"  :change-z-size)
    ))

(defmethod object-popup-menu-item-list  ((ui cme-object-popup-ui) (object perspective-transform-object))
  '(("Move XY" :uvxy )
    ("Move W" :move-cam-w )
    ("" :separator)
    ("Roll" :uv-roll)
    ("Rot W" :w-rot)
    ("Z Up" com-canonicalize-orientation)
    ("" :separator)
    ("Focal Length" :focal-length)
	    
    ))

(defmethod object-popup-menu-item-list  ((ui cme-object-popup-ui)
					 (object perspective-transform-stare-point-object))
  '(("Drop To DTM" com-move-w-to-ground)
    ("Move On DTM" :uv-on-dtm )
    ("Move XY" :uvxy )
    ("Move W" :move-cam-w )
    ("" :separator)
    ("Roll" :uv-roll)
    ("Rot W" :w-rot)
    ("Az-Elev" :az-elev)
    ("Z Up" com-canonicalize-orientation)
    ("" :separator)
    ("Focal Length" :focal-length)
    ("Range/FL" :w-focal-length
     :documentation
     "Adjust Both Range to Stare Point and Focal Length, maintaining constant GSD.")
    ))

(defmethod object-popup-menu-item-list  ((ui cme-object-popup-ui) (object camera-model-object))
  (with-slots (last-selected-handle) object
    (if (typep last-selected-handle 'perspective-transform-stare-point-object )
	'(("Drop To DTM" com-move-w-to-ground)
	  ("Move On DTM" :uv-on-dtm )
	  ("Move XY" :uvxy )
	  ("Move W" :move-cam-w )
	  ("" :separator)
	  ("Roll" :uv-roll)
	  ("Rot W" :w-rot)
	  ("Az-Elev" :az-elev)
	  ("Z Up" com-canonicalize-orientation)
	  ("" :separator)
	  ("Focal Length" :focal-length)
	  ("Range/Focal Length" :w-focal-length)
	  )
	;; camera object
	'(("Move XY" :uvxy )
	  ("Move W" :move-cam-w )
	  ("" :separator)
	  ("Roll" :uv-roll)
	  ("Rot W" :w-rot)
	  ("Z Up" com-canonicalize-orientation)
	  ("" :separator)
	  ("Focal Length" :focal-length)
	  )
	)))

(defmethod object-popup-menu-item-list ((ui cme-object-popup-ui) (object 2d-curve))
  '(("Move Vert UV" :vertex-uvxy)
    ("Move Curve UV" :uvxy)
    ("" :separator)
    ("Rotate" :z-rot)
    ("Add Vert" com-add-vertex)
    ("Del Vert" com-delete-vertex )
    ))

(defmethod object-popup-menu-item-list ((ui cme-object-popup-ui) (object 3d-curve))
  '(("Drop Vert To DTM" com-move-w-to-ground)
    ("Drape Over DTM" com-move-every-w-to-ground)
    ("Move Vert On DTM" :vertex-uv-on-dtm)
    ("Move Vert XY" :vertex-uvxy)
    ("Move Vert W" :vertex-w)
    ("Move Vert Z" :vertex-z)
    ("" :separator)
    ("Add Vert" com-add-vertex)
    ("Del Vert" com-delete-vertex )
    ))

(defmethod object-popup-menu-item-list  ((ui cme-object-popup-ui) (object extruded-object))
  '(("Drop Vert To DTM" com-move-w-to-ground)
    ("Move Vert On DTM" :vertex-uv-on-dtm)
    ("Move Vert XY" :vertex-uvxy)
    ("Move Vert W" :vertex-w)
    ("Move Vert Z" :vertex-z)
    ("" :separator)
    ("Z Size"  :change-z-size)
    ("Add Vert" com-add-vertex)
    ("Del Vert" com-delete-vertex )
     
    ))


(defmethod object-popup-menu-item-list  ((ui cme-object-popup-ui)
					 (object 3d-network))
  '(("Drop Vert To DTM" com-move-w-to-ground)
    ("Move Vert On DTM" :vertex-uv-on-dtm)
    ("Move Vert XY" :vertex-uvxy)
    ("Move Vert W" :vertex-w)
    ("Move Vert Z" :vertex-z)
    ("" :separator)
    ("Add Vert/Arc" com-add-vertex)
    ("Delete Vert/Arc" com-delete-vertex-or-arc)
    ))

(defmethod object-popup-menu-item-list  ((ui cme-object-popup-ui)
					 (object 2d-network))
  '(("Move Vert UV" :vertex-uvxy)
    ("" :separator)
    ("Add Vert/Arc" com-add-vertex)
    ("Delete Vert/Arc" com-delete-vertex-or-arc)
    ))

(defmethod object-popup-menu-item-list :around ((ui cme-object-popup-ui) (object 3d-ribbon-curve))
  (append (call-next-method)
	  '(("Vertex Width" :change-width )
	    ;;("Ribbon Width" :change-ribbon-width )
	    )))


(defmethod object-popup-menu-item-list :around ((ui cme-object-popup-ui)
						(object image-windowing-tool))
  (append (call-next-method)
	  '(("Move" :uvxy)
	    ("Move Edge" :move-edge)
	    ("Make Window" make-window))))

(defmethod object-popup-menu-item-list :around ((ui cme-object-popup-ui)
						(object sun-ray-object))
  (append (call-next-method)
	  '(("Set Sun Ray" com-set-view-sun-vector)
	    ("Scale" :change-total-scale))))

(defmethod object-popup-menu-item-list :around ((ui cme-object-popup-ui)
						(object house-object))
  (append (call-next-method)
	  '(("Roof Pitch" :change-roof-pitch))))


;;; This needs to be constrained when the 3d position of the conjugate point is "write-locked". 
(defmethod object-popup-menu-item-list ((ui cme-object-popup-ui)
					(object conjugate-point-object))
  '(("Drop To DTM" com-move-w-to-ground)
    ("Move On DTM" :uv-on-dtm )
    ("Move XY" :uvxy )
    ("Move W" :w )
    ("Move Conj UV" :conj-xy
     :documentation "Adjust the Image Position of the Conjugate Point.")
    ("Set Conj UV" com-set-conjugate-position-in-view
     :documentation "Set the Image Position (in possibly another view) of the Conjugate Point.")
    ("Delete Conj UV" com-delete-2d-constraint
     :documentation "Remove the Conjugate Point Position for this Image.")
    ))

(defmethod object-popup-menu-item-list :around
	   ((ui cme-object-popup-ui) (object conjugate-inferior-mixin))
  (append (call-next-method )
	  '(("Move UV" :conj-xy)
	    ("Optimize" com-optimize)
	    ("@Image UV" com-set-conjugate-position-in-view)
	    ("Delete UV" com-delete-2d-conjugate-point))))

(defmethod object-popup-menu-item-list :around
	   ((ui cme-object-popup-ui) (object conjugate-parent-mixin))
  (append (call-next-method )
	  '(("Conj uv" com-move-conjugate-point))))

(defmethod object-popup-menu-item-list  :around ((ui cme-object-popup-ui) (object composite-object))
  (append (call-next-method)
	  '(("" :separator)
	    ("Add Inferior" com-add-inferior)
	    ("Remove Inferior" com-remove-inferior)
	    ("Open Inferior" com-open-inferior :documentation "Make Inferior Object In Collection Sensitive")
    ))
	    )

(defmethod object-popup-menu-item-list  :around ((ui cme-object-popup-ui) (object feature-set))
  (append (call-next-method)
	  '(("" :separator)
	    ("Add Inferior" com-add-inferior)
	    ("Remove Inferior" com-remove-inferior)
	    ("Open Inferior" com-open-inferior :documentation "Make Inferior Object In Collection Sensitive")
    ))
	    )

  
;;; ****************************   OBJECT-POPUP-MENU-EXTENSION-ITEM-LIST  ****************************

(defmethod object-popup-menu-extension-item-list ((ui cme-object-popup-ui) (object basic-object))
  '(("Accel Menu" describe-bucky-menu :documentation "Command Accelerator Menu (aka Bucky Menu)" )
    ("Open Parent" com-close-self :documentation "Make Parent Sensitive for Selection")
    ("Undo" undo)
    ("Redo" redo)
    ("Clone" clone-and-grab)
    ("Blank" com-blank-object2)
    ("Delete" delete-object)
    ("" :separator)
    ))

(defmethod object-popup-menu-extension-item-list ((ui cme-object-popup-ui) (object object-collection))
  '(("Accel Menu" describe-bucky-menu :documentation "Command Accelerator Menu (aka Bucky Menu)" )
    ;;("Open Inferior" com-open-inferior :documentation "Make Inferior Object In Collection Sensitive")
    ))

(defmethod object-popup-menu-extension-item-list ((ui cme-object-popup-ui) (object composite-object))
  '(("Accel Menu" describe-bucky-menu :documentation "Command Accelerator Menu (aka Bucky Menu)" )
    ("Open Parent" com-close-self :documentation "Make Parent Sensitive for Selection")
    ("Clone" clone-and-grab)
    ("Blank" com-blank-object2)
    ("Delete" delete-object)
    ;;("Open Inferior" com-open-inferior :documentation "Make Inferior Object In Collection Sensitive")
    ))

(defmethod object-popup-menu-extension-item-list :around ((ui cme-object-popup-ui) (object 3d-object))
  (append (call-next-method)
	  '(("@Vertex" com-set-selected-point-at-vertex))))

(defmethod object-popup-menu-extension-item-list :around ((ui cme-object-popup-ui) (object obj::gl-xyz-sizable-object-mixin))
  (append (call-next-method)
	  '(("Roll" :uv-roll)
	    ("Rot Z":z-rot)
	    ("Z Up" com-canonicalize-orientation)
	    ("Scale" :change-scale)
	    ("Taper" :change-taper-rate)
	    ("Sun Z" :sun-w))))

(defmethod object-popup-menu-extension-item-list :around ((ui cme-object-popup-ui) (object object-with-modifiable-vertices-mixin))
  (append (call-next-method)
	  '(("Open Vertices" com-open-vertices)
	    ("Close Vertices" com-close-vertices))))

(defmethod object-popup-menu-extension-item-list :around ((ui cme-object-popup-ui) (object 3d-curve))
  (append (call-next-method)
	  '(("Move Curve XY" :uvxy)
	    ("Move Curve W" :w)
	    ("Drop Curve W" com-move-every-w-to-ground )
	    ("Roll" :uv-roll)
	    ("Rot Z":z-rot)
	    ("Vert Visibility" com-invert-vertex-visibility
	     :documentation "Invert Vertex Visibility")
	    ("Reset" com-reset-curve)
	    )))
(defmethod object-popup-menu-extension-item-list :around ((ui cme-object-popup-ui) (object 2d-curve))
  (append (call-next-method)
	  '(("Move Curve UV" :uvxy)
	    ("Rotate":z-rot)
	    ("Vert Visibility" com-invert-vertex-visibility
	     :documentation "Invert Vertex Visibility")
	    ("Reset" com-reset-curve)
	    )))

(defmethod object-popup-menu-extension-item-list :around ((ui cme-object-popup-ui) (object 3d-network))
  (append (call-next-method)
	  '(("Merge Vertices" com-merge-vertices)
	    ("Split Vertex" com-split-vertex)
	    )))

(defmethod object-popup-menu-extension-item-list :around ((ui cme-object-popup-ui) (object basic-network))
  (append (call-next-method)
	  '(("Merge Vertices" com-merge-vertices)
	    ("Split Vertex" com-split-vertex)
	    )))


(defmethod object-popup-menu-extension-item-list :around
	   ((ui cme-object-popup-ui) (object 3d-closed-curve))
  (append (call-next-method)
	  '(("Extrude" com-extrude)
	    )))

(defmethod object-popup-menu-extension-item-list :around
	   ((ui cme-object-popup-ui) (object generic-ribbon-curve))
  (append (call-next-method)
	  '(("Ribbon Width" :change-ribbon-width )
	    )))

(defmethod object-popup-menu-extension-item-list :around
	   ((ui cme-object-popup-ui) (object extruded-object))
  (append (call-next-method)
	  '(("Move Curve XY" :uvxy)
	    ("Move Curve W" :w)
	    ("Drop Curve W" com-move-every-w-to-ground )
	    ("Roll" :uv-roll)
	    ("Rot Z":z-rot)
	    ("Make Horiz" com-make-horizontal)
	    ("Make Rect" com-make-rectilinear)
	    )))

(defmethod object-popup-menu-extension-item-list :around
	   ((ui cme-object-popup-ui) (object superquadric))
  (append (call-next-method)
	  '(("X exponent" :change-x-expt)
	    ("Y exponent" :change-y-expt)
	    ("Z exponent" :change-z-expt)
	    ("XYZ Exponents" :change-expts))))


(defmethod object-popup-menu-extension-item-list :around
	   ((ui cme-object-popup-ui) (object superellipse))
  (append (call-next-method)
	  '(("XY exponent" :change-xy-expt)
	    ("Z exponent" :change-z-expt)
	    ("XYZ Exponents" :change-expts))))

(defmethod object-popup-menu-extension-item-list :around
	   ((ui cme-object-popup-ui) (object cylinder))
  (append (call-next-method)
	  '(("XY Scale" :change-xy-scale))))

(defmethod object-popup-menu-extension-item-list :around
	   ((ui cme-object-popup-ui) (object half-cylinder))
  (append (call-next-method)
	  '(("Radius/Length" :change-radius-and-length)
	    ("Rotate/Scale" :change-orientation-and-size))))


(defmethod object-popup-menu-extension-item-list :around ((ui cme-object-popup-ui)
							  (object conjugate-point-object))
  (append (call-next-method)
	  '(("Resection Improve" com-resection-conjugate-points-improve)
	    ("Resection Menu" com-resection-conjugate-points-menu))))


;;; ******************************  NO-OBJECT-MODIFY-POPUP-UI  ******************************

(defclass no-object-modify-popup-ui (user-interface-context) ())

(defmethod object-popup-menu-item-list ((ui no-object-modify-popup-ui) (object basic-object))
  '(("Parameters" com-cvv-object-menu)
    ))


(defclass analyst-no-object-modify-popup-ui (no-object-modify-popup-ui) ())

(defmethod object-popup-menu-item-list ((ui analyst-no-object-modify-popup-ui) (object basic-object))
  '(("Parameters" com-cvv-object-menu)
    ("Attributes" com-view-object-attributes) ; ?????????????
    ))

(defmethod basic-object-popup-menu-item-list ((ui analyst-no-object-modify-popup-ui) (object basic-object))
  `(    ))

(defmethod object-popup-menu-extension-item-list ((ui no-object-modify-popup-ui) (object basic-object))
  '(("Accel Menu" describe-bucky-menu :documentation "Command Accelerator Menu (aka Bucky Menu)" )
    ("Blank" com-blank-object2)
    ("" :separator)
    ))
;;; ******************************  BASIC-ELT-POPUP-UI  ******************************
	
(defclass basic-elt-popup-ui (user-interface-context) ())

(defparameter *basic-elt-view-item-list*
  '(("Pop Stack" ic::com-kill-view)
    ("Cycle Stack" ic::com-cycle-pane-stack)
    ("Rev Cycle" ic::com-reverse-cycle-pane-stack)
    ("Copy View" ic::com-copy-view2)
    ("Full Screen" ic::com-copy-view-to-1x1-frame :documentation
     "Copy View to 1x1 Frame")
    ("Move View" ic::com-move-view2)
    ))


(defmethod object-popup-menu-item-list ((ui basic-elt-popup-ui) (object basic-image))
  `(("<more> ... " (("Accel Menu" ic::com-describe-bucky-menu
		     :documentation "Command Accelerator Menu (aka Bucky Menu)")
		    ;;("View Full Screen" ic::com-copy-view-to-1x1-frame :documentation "Copy View to 1x1 Frame")
		    ("Contrast Stretch" ic::com-stretch :documentation
		     "Use Histogram Stretch Tool to Enhance the Image Contrast")
		    ("Auto Stretch" ic::com-auto-stretch :documentation
		     "Stretch Image Constrast of Tandem Views.")
		    ("Window" ic::com-window :documentation
		     "Extract Window (Rectangular Sub-Image) of Image.")
		    ("Orient" (("Reset" ic::com-unrotate)
			       ("90" ic::com-rotate)
			       ("180" ic::com-rotate-180)
			       ("270" ic::com-rotate-ccw)
			       ("Z-Up" ic::com-rotate-z-up)
			       ("North-Up" ic::com-rotate-north-up)
			       ("Mirror-X" ic::com-flip-x)
			       ("Mirror-Y" ic::com-flip-y)
			       )
		     :documentation "Menu of Rotations and Flips.")
		    ("DeCache Graphics" com-flush-drawing-cache :documentation
		     "Flush Cache of Graphics Commands for 2d-World associated with this View.")

		    )
     :documentation "Extended Popup Menu")
    ("Parameters" com-cvv-image-menu)
    ("" :separator)
    ("Recenter" ic::com-recenter2)
    ("%Reposition" ic::com-reposition-percentwise2)
    ("Drag" ic::com-drag)
    ("" :separator)
    ("Zoom to Max" ic::com-zoom-to-full-res)
    ("Zoom In" ic::com-zoom-in)
    ("Zoom Out" ic::com-zoom-out)
    ("Zoom to Fit" ic::com-zoom-to-fit)
    ("" :separator)
    ,@*basic-elt-view-item-list*
    ))

(defmethod object-popup-menu-item-list ((ui basic-elt-popup-ui) (object t))
  `(("<more> ... " (("Accel Menu" ic::com-describe-bucky-menu
		     :documentation "Command Accelerator Menu (aka Bucky Menu)")
	   ))
    ,@*basic-elt-view-item-list*
    ))

;;; ******************************  ACCELERATOR MENU CLASSES  ******************************

;;;******************************  NO-OBJECT-MODIFY-ACCELERATORS  ******************************

(defclass no-object-modify-accelerators (user-interface-context) ())


(defmethod ui-bucky-menu-list ((ui no-object-modify-accelerators) (object basic-object))
  `(((left) stop-tracking-return-handle "Select Object")
    ((middle) com-do-nothing "")
    ((right) com-menu-click "Menu")
    ))

;;; ******************************  BASIC-OBJECT-ACCELERATORS  ******************************

(defclass basic-object-accelerators (user-interface-context) ())

(defmethod ui-bucky-menu-list ((ui basic-object-accelerators) (object basic-object ))
  `(((left) stop-tracking-return-handle "Select Object")
    ((middle) com-do-nothing "")
    ((right) com-menu-click "Menu")
    ))

(defmethod ui-bucky-menu-list ((ui basic-object-accelerators) (object 3d-object ))
  `(((left) stop-tracking-return-handle "Select Object")
    ((middle) com-do-nothing "")
    ((right) com-menu-click "Menu")
    ((control left)  :uvxy  "Move XY")
    ((control middle) :w "Move W")
    ((control right)  :uv-on-dtm  "Move On DTM")
    ))

(defmethod ui-bucky-menu-list ((ui basic-object-accelerators) (object conjugate-point-object ))
  `(((left) stop-tracking-return-handle "Select Object")
    ((middle) com-do-nothing "")
    ((right) com-menu-click "Menu")
    ((control left)  :conj-xy "Move Conj UV")
       ))


(defmethod ui-bucky-menu-list ((ui basic-object-accelerators) (object basic-curve ))
  `(((left) stop-tracking-return-handle "Select Object")
    ((middle) com-do-nothing "")
    ((right) com-menu-click "Menu")
    ((control left) :vertex-uvxy "Move Vert UV")
    ((meta left) com-add-vertex "Add Vert" "Add a new Vertex")
    ((meta middle) com-delete-vertex "Del Vert" "Delete Vertex")
    ))

(defmethod ui-bucky-menu-list :around ((ui basic-object-accelerators) (object 3d-curve ))
  (append (call-next-method)
	  '(((control left) :vertex-uvxy "Move Vert XY")
	    ((control middle) :vertex-w "Vert W")
	    ((control right) :vertex-uv-on-dtm "Vert on DTM"))))

(defmethod ui-bucky-menu-list :around ((ui basic-object-accelerators) (object basic-network ))
  (append (call-next-method)
	  '(((meta left) com-add-vertex "Add Vert/Arc" "Add a new Vertex or Arc")
	    ((meta middle) com-delete-vertex-or-arc "Del Vert/Arc" "Delete Vertex or Arc"))))

(defmethod ui-bucky-menu-list ((ui basic-object-accelerators) (object image-scrolling-object ))
    `((( left) com-stop-tracking  "")
      (( middle) com-stop-tracking "")
      (( right) com-stop-tracking "")
      ((control left) com-stop-tracking "")
      ((control right) com-stop-tracking "")
      ((meta left) :scroll "Scroll2")
      ((control middle) nil "")))

(defmethod ui-bucky-menu-list ((ui basic-object-accelerators) (object image-windowing-tool ))
  `((( left) :uvxy "Move")
    ((middle) :move-edge "Move Edge")
    (( right) com-menu-click "Menu")
    ((control left) make-window "Make Window")
    ))



;;; ******************************  BASIC-ELT-ACCELERATORS  ******************************

(defclass basic-elt-accelerators (user-interface-context) ())

(defmethod ui-bucky-menu-list ((ui basic-elt-accelerators) (object basic-image ))
  `(((left) ic::com-select-pane "Select Pane")
    ;;((left) ic::com-select-drag "Select+Drag")
    ;;((left) ic::com-drag "Drag")
    ((middle) ic::com-refresh-Pane "Refresh Pane" )
    ((right) ic::com-image-calc-main-menu "Menu")
    
    ;;((control left) ic::com-Reposition-percentwise "% Reposition")
    ((control left) ic::com-drag "Drag") 
    ((control middle) ic::com-zoom-in "Zoom In" )
    ((control right) ic::com-zoom-out "Zoom Out" )
    
    ((meta left) ic::com-Recenter2 "Recenter") 
    ((meta middle) ic::com-copy-view "Copy View" )
    ((meta right) ic::com-move-view "Move View" )

    ((control meta left) ic::com-Cycle-Pane-Stack "Cycle Stack" ) 
    ((control meta middle) ic::com-kill-view "Pop Stack" "Remove the Top View from Stack")
    ((control meta right) ic::com-Reverse-Cycle-Pane-Stack "Rev Cycle")
  
    ))

(defmethod ui-bucky-menu-list ((ui basic-elt-accelerators) (object t ))
  `(((left) ic::com-select-pane "Select Pane")
    ((middle) ic::com-refresh-Pane "Refresh Pane" )
    ((right) ic::com-image-calc-main-menu "Menu")
    ((meta middle) ic::com-copy-view "Copy To Here" )
    ((meta right) ic::com-move-view "Move To Here" )
    ))


;;;  ***************************   EXTENDABLE USER INTERFACE CONTEXT  ******************************

;;; This class allows dynamic extension to the menu items of the popup menus by adding
;;; USER-UI-EXTENSION instances to the EXTENSIONS slot of the EXTENDABLE-USER-INTERFACE-CONTEXT.
;;; Each of these USER-UI-EXTENSIONs can provide additional items to the popup menu item lists.
;;; The EXTENSIONS slot can the dynamically modified and the menus will reflect the changes.

(defclass extendable-user-interface-context (user-interface-context)
    ((extensions :initform nil :initarg :extensions :accessor extensions))
  )


;;; This is the guy that implements the extension to the popup menu item lists.
(defmethod full-object-popup-menu-item-list  ((ui extendable-user-interface-context)
					      (object basic-object))
  (append `(("<more> ..." ,(object-popup-menu-extension-item-list ui object)  :documentation "Extended Popup Menu"))
	  (basic-object-popup-menu-item-list ui object)
	  (object-popup-menu-item-list ui object)
	  (loop for ui2 in (extensions ui)
		append (object-popup-menu-item-list ui2 object))))

;;; This lets the stuff work on images too -- Thu Jul 23 1998 heller

(defmethod full-object-popup-menu-item-list ((ui extendable-user-interface-context)
					     (object basic-image))
  (append (object-popup-menu-item-list ui object)
	  (loop for ui2 in (extensions ui)
		append (object-popup-menu-item-list ui2 object))))


;;; The purpose of the caching is to avoid lots of consing of item-lists and ultimately
;;; a hash-table lookup to find an menu that matches the item list.
;;; The caching could be removed -- with uncertain performance impact.
#+unconverted
(defmethod object-popup-menu ((ui extendable-user-interface-context) (object t) mouse-window)
  (let* ((object-class (class-of object))
	 (extension-classes (loop for ui2 in (extensions ui)
				  collect (class-of ui2)))
	 (screen (screen mouse-window))
	 (menu
	  (eval-cache (ic::object-popup-menu ui object-class screen extension-classes)
	      ;;(format t "qui::make-popup-menu2 ~a ~A~%" ui object)
	      (make-popup-menu2
	       (translate-popup-object-menu-item-list
		object
		(append *object-popup-menu-label-item*
			(full-object-popup-menu-item-list ui object)))
	       :screen (ic::screen mouse-window)))))

    (let ((label-clidget (loop for item in (children menu)
			      when (typep item 'qui-label)
				return item)))
      (when label-clidget
	(set-cvv-item-value label-clidget (if object (short-name-string object) "VIEW"))))
    menu))


(defmethod add-user-interface-extension ((ui extendable-user-interface-context) ui-extension-class)
  (unless (loop for ui2 in (extensions ui)
		thereis (eq (type-of ui2) ui-extension-class))
    (setf (extensions ui) (append (extensions ui) (list (make-instance ui-extension-class))))))

(defmethod remove-user-interface-extension ((ui extendable-user-interface-context) ui-extension-class)
  (setf (extensions ui)
	(loop for ui2 in (extensions ui)
	      unless (eq (type-of ui2) ui-extension-class)
		collect ui2)))

(defmethod full-ui-bucky-menu-list ((ui t) (object t))
  (ui-bucky-menu-list ui object))
  
(defmethod full-ui-bucky-menu-list ((ui extendable-user-interface-context) (object t))
  (append (ui-bucky-menu-list ui object)
	  (loop for iu2 in (extensions ui)
		append (ui-bucky-menu-list iu2 object))))
  
;;; base class for extensions to the extendable-user-interface-context
(defclass user-ui-extension (user-interface-context) ())

(defmethod object-popup-menu-item-list ((ui user-ui-extension) (object t))
  nil)


(defclass user-ui-bucky-extension  (user-ui-extension) ())

(defmethod ui-bucky-menu-cache-key ((ui user-ui-bucky-extension))
  (class-of ui))



;;; ******************************  COMPLETE USER INTERFACE CLASSES  ******************************

;;; A complete UI class contains 4 subclasses:

;;;       popup class for CME objects
;;;       accelerator class for CME objects
;;;       popup class for image objects
;;;       accelerator class for image objects


;;; This class is compatible with old user interface - defined in another file
;;;(defclass old-cme-ui (old-cme-cvv-ui
;;;                      old-cme-accelerators
;;;                      ic::old-ic-popup-menu-context
;;;                      ic::old-ic-accelerators)
;;;    ())


;;; This class retains the old accelerator-menu binding and adds popup menus
(defclass augmented-cme-ui (extendable-user-interface-context
			    old-cme-accelerators
			    ic::old-ic-accelerators
			    cme-object-popup-ui
			    basic-elt-popup-ui
			    )
    ())


;;; This class defines minimal accelerator-menus and uses the new popup menus 
(defclass site-modeler-ui (extendable-user-interface-context
			   cme-object-popup-ui
			   basic-object-accelerators
			   basic-elt-popup-ui
			   basic-elt-accelerators)
    ())

(defclass site-viewer-ui (extendable-user-interface-context
			  analyst-no-object-modify-popup-ui
			  no-object-modify-accelerators
			  basic-elt-popup-ui
			  basic-elt-accelerators)
    ())

(defparameter *default-ui-class* 'augmented-cme-ui)

(defun-cached make-user-interface-context (ui-class)
  (make-instance ui-class))

(defun set-default-user-interface-context-class (ui-class)
  (setq *default-ui-class* ui-class
  	*current-user-interface-context* (make-user-interface-context ui-class)))

(set-default-user-interface-context-class 'augmented-cme-ui)


(defun com-snapshot-selected-view (interactor)
  (multiple-value-bind (w h)
      (dimensions (selected-pane interactor))
    (let ((snapshot (img::make-color-image (list w h))))
      (glreadpixels 0 0 w h GL::GL_RGB GL::GL_UNSIGNED_BYTE
		    (img::image-array (img::band-interleaved-image snapshot)))
      (img::image-flip-y snapshot)
      (push-image snapshot (pick-a-pane "Pick a pane for the snapshot")))))
    


(defmethod object-popup-menu-item-list :around ((ui augmented-cme-ui) (object img::image))
  (append (call-next-method)
	  '(("Snapshot" :eval (com-snapshot-selected-view *interactor*)))))
  



#|
(set-default-user-interface-context-class 'site-modeler-ui)
(set-default-user-interface-context-class 'site-viewer-ui)
(set-default-user-interface-context-class 'augmented-cme-ui)
(set-default-user-interface-context-class 'old-cme-ui)

;;; This next illustrates that panes (and views and frames) can control the IU context,
;;; in addition to the global variable *current-user-interface-context*.
;;; Each pane has a different :user-interface-context.  
(progn 
  (setf (get-prop (ic::image-calc-pane 1 1) :user-interface-context)
	(make-user-interface-context 'augmented-cme-ui))
  (setf (get-prop (ic::image-calc-pane 0 0) :user-interface-context)
	(make-user-interface-context 'site-viewer-ui))
  (setf (get-prop (ic::image-calc-pane 1 0) :user-interface-context)
	(make-user-interface-context 'old-cme-ui))
  )

(setq *user-interface-context-override* (make-user-interface-context 'site-viewer-ui))
(setq *user-interface-context-override* nil)
|#



#|
(progn (eval-cache::eval-cache-flush-function 'object-popup-menu)
       (eval-cache::eval-cache-flush-function 'ic::object-popup-menu)
       (eval-cache::eval-cache-flush-function 'qui::make-popup-menu2)
       (eval-cache::eval-cache-flush-function 'ic::make-menu)
       nil)
(eval-cache::eval-cache-flush-function 'ui-make-bucky-menu)
|#

#|
(defclass snake-extension (user-ui-extension) ())

(defmethod object-popup-menu-item-list ((ui snake-extension) (object 3d-curve))
  '(("Snake in the Grass" snake-grass)
    ("Other Snakes" (("Rattle" rattle-snake)
		     ("Sewer" sewer-snake))
     )))

(defclass foo1-extension (user-ui-extension) ())

(defmethod object-popup-menu-item-list ((ui foo1-extension) (object 3d-curve))
  '(("Foo1 - Good Curves, Baby" good-curves)
    ("Foo1 - other options" (("foo1a" foo1a) ("foo2a" foo2a))
     )))

(defclass bar1-extension (user-ui-extension) ())

(defmethod object-popup-menu-item-list ((ui bar1-extension) (object cube-object))
  '(("Bar1" bar1)
    ("Bar1 - other options" (("bar1a" bar1a)
			     ("bar2a" bar2a))
     )))

(defclass snake-bucky-extension (user-ui-bucky-extension) ())

(defmethod ui-bucky-menu-list :around ((ui snake-bucky-extension) (object 3d-curve ))
  (append `(((middle) cme-snakes-parameters "Snakes Parameters"))
	  (call-next-method)))

(add-user-interface-extension *current-user-interface-context* 'snake-bucky-extension)
(remove-user-interface-extension *current-user-interface-context* 'snake-bucky-extension)
(ui-bucky-menu-list *current-user-interface-context* cur)
(full-ui-bucky-menu-list *current-user-interface-context* cur)

(defclass funny-image-bucky-extension (user-ui-bucky-extension) ())
(defmethod ui-bucky-menu-list :around ((ui funny-image-bucky-extension) (object basic-image ))
  (append `(((middle) com-do-nothing "Funny"))
	  (call-next-method)))

(add-user-interface-extension *current-user-interface-context* 'funny-image-bucky-extension)
(remove-user-interface-extension *current-user-interface-context* 'funny-image-bucky-extension)
(ui-bucky-menu-list *current-user-interface-context* (view-image (top-view)))
(full-ui-bucky-menu-list *current-user-interface-context* (view-image (top-view)))

(inspect *current-user-interface-context*)
(update-object-bucky-menus)
(eval-cache::eval-cache-flush-function 'ui-make-bucky-menu)
|#

#|
(set-default-user-interface-context-class 'site-modeler-ui)

(add-user-interface-extension *current-user-interface-context* 'foo1-extension)
(remove-user-interface-extension *current-user-interface-context* 'foo1-extension)

(loop for ui2 in '(snake-extension foo1-extension bar1-extension)
      do (add-user-interface-extension *current-user-interface-context* ui2))


(maybe-compile-file-load "$FREEDIUS/lisp/basic-gui/ui-classes.lisp")
|#

#|
(progn (eval-cache::eval-cache-flush-function 'ic::make-menu)
       (setq menu
	     (ic::make-menu (progn	;convert-item-list
			      '(("foo0" :quote foo0)
				("foo1" :menu (("bar0" :quote bar0)
					       ("bar1" :menu (("bar0" :quote bar0)
							      ("bar1" :quote bar1)
							      ("" :separator nil)
							      ("bar2" :quote bar2)))
					       ("" :separator nil)
					       ("bar2" :quote bar2)))
				("" :separator nil)
				("foo2" :quote foo2))))))
(menu-choose menu)
	      

|#


|#
