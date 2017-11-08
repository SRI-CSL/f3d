(in-package :gui)



#|
(maybe-compile-file-load "$FREEDIUS/lisp/cme-compat/cvv-object-menus.lisp")
|#

#|
(progn (eval-cache::eval-cache-flush-function 'cached-cvv-panel-item-list)
       nil)

(progn (eval-cache::eval-cache-flush-function 'cached-cvv-panel-item-list)
       (xw::kill-cached-cvv-panels))

(progn (eval-cache::eval-cache-flush-function 'ic::make-choose-variable-values-menu)
       (eval-cache::eval-cache-flush-function 'qui::get-cvv-panel-resource)
       (eval-cache::eval-cache-flush-function 'cached-cvv-panel-item-list)
       nil)
|#

(defvar *default-menu-screen* nil)


#|
(setq *default-menu-screen* nil)
(setq *default-menu-screen* (ic::find-screen :bits-per-pixel 1))
|#

#|
;;; For now, there is no multi-screen support

(defun default-object-menu-screen (&optional screen)
  ;;(format t "(default-menu-screen ~a~%)" screen)
  (cond ((typep *default-menu-screen* 'ic::x-screen)
	 *default-menu-screen*)
	((null *default-menu-screen*)
	 (or screen (ic::mouse-screen )))
	((eq *default-menu-screen* 'other)
	 (loop with mouse-screen = (ic::mouse-screen )
	       for screen in ic::*screen-list*
	       unless (eq screen mouse-screen)
		 return screen))
	(t (error "Illegal value for *default-menu-screen* = ~a" *default-menu-screen*))))
|#

(defun mouse-screen ()
  *default-menu-screen*)

(defun default-object-menu-screen (&optional screen)
  (or screen (mouse-screen)))

(defun default-menu-screen (&optional screen)
  (ignore screen)
  (mouse-screen ))


;;; If an item exists on both lists, the occurrence in l2 prevails.
(defun union-item-lists (l1 l2)
  (nconc (loop for item in l1
		unless (assoc (car item) l2)
		  collect item)
	  l2))



(defgeneric update-object-from-cvv-panel (object cvv-panel item-name)
  (:method-combination or ))

(defclass base-cvv-object-panel (tk::cvv-panel) ())
(defclass cvv-object-panel (base-cvv-object-panel ) ())

(defmethod cvv-panel-item-list ((object basic-object) (panel-class t))
  nil)

(defmethod update-cvv-panel-from-object ((object obj::gl-object) fragment
					 (cvv-panel base-cvv-object-panel)
					 &optional what-changed)
  (ignore cvv-panel what-changed)
  )

(defmethod update-cvv-panel-from-object ((object obj::gl-object) fragment
					 (cvv-panel cvv-object-panel)
					 &optional what-changed)
  (ignore cvv-panel what-changed)
  )


(eval-when (load eval compile)
  
(defparameter *cvv-top-level-panel-class* 'cvv-object-panel)

) ; end eval-when


(defun cvv-generic-object-panel (objfrag &key
					(panel-class *cvv-top-level-panel-class*)
					superior-panel
					(mouse-screen (mouse-screen)) )
  (let* ((screen (default-object-menu-screen mouse-screen))
	 (panel  (make-cvv-object-panel (obj::object objfrag)
					:screen screen :panel-class panel-class )))
    ;;(setq *panel* panel)
    (when panel
      (when superior-panel
	(pushnew panel (get-prop superior-panel :inferior-panels))
	(setf (get-prop panel :superior-panel) superior-panel))
      
      (pop-up-panel panel)
      (tk::raise-window-toplevel panel)
      (set-object panel objfrag)
      )))

(defmethod top-of-panel-hierarchy ((cvv-panel base-cvv-object-panel))
  (loop for panel = cvv-panel then next-panel
	for next-panel = (get-prop panel :superior-panel)
	while next-panel
	finally (return panel)))


(defmethod update-item-in-panel-hierarchy ((cvv-panel base-cvv-object-panel) item-name item-value)
  ;;(format t "update-item-in-panel-hierarchy ~a ~A ~%" cvv-panel item-name)
  (tk::set-item-value cvv-panel item-name item-value)
  (loop for panel in (get-prop cvv-panel :inferior-panels)
	do (update-item-in-panel-hierarchy panel item-name item-value)))

;;; Binding *NEXT-CVV-PANEL-ITEM-LIST-METHOD-P* to NIL allows CVV-PANEL-ITEM-LIST-OVERRIDE
;;; methods to selectively invoke :AROUND methods.
(defvar *next-cvv-panel-item-list-method-p* t)




(defmethod cvv-panel-class-name ((object basic-object))
  (string (cvv-panel-class object)))

(defmethod cvv-panel-class ((object basic-object))
  nil)

#|
(proclaim '(special *cvv-object-panel-dash-length-item*
	    *cvv-object-panel-dash-spacing-item*
	    *stipple-patterns*
	    *cvv-object-panel-item-group-spec*
	    *cvv-object-panel-line-width-item*
	    ))

(defun cvv-panel-item-list-global-state-list ()
  ;; If additional global state dependencies are added to any of the cvv-panel-item-list methods
  ;; add the global to the following form.  We really want these globals to remaim EQ unless changed.
  
  (list ;;ic::*color-name-alu-alist*  ; not necessary to remember this -- menu is dynamically updated
	*cvv-object-panel-line-width-item*
	*cvv-object-panel-dash-length-item*
	*cvv-object-panel-dash-spacing-item*
	*stipple-patterns*
	*cvv-object-panel-item-group-spec*
	))
|#

(defun cvv-panel-item-list-global-state-list ()
  ;; If additional global state dependencies are added to any of the cvv-panel-item-list methods
  ;; add the global to the following form.  We really want these globals to remaim EQ unless changed.
  nil)

;;; cvv-panel-item-list-override provides a mechanism to build cvv-panel-item-lists
;;; which without being restricted to strict inheritance.
;;; This must be considered a PRAGMATIC KLUDGE.
;;; set (method cvv-panel-item-list-override PERSPECTIVE-TRANSFORM-OBJECT) for example.
(defmethod cvv-panel-item-list-override ((object basic-object))
  nil)


;;; This definition avoids the need for cache flushing:
(defun cached-cvv-panel-item-list (object panel-class screen )
  (multiple-value-bind (item-list cached-global-state-list)
      (eval-cache (cached-cvv-panel-item-list (cvv-panel-class object) panel-class screen)
	  (values (or (cvv-panel-item-list-override object)
		      (cvv-panel-item-list object panel-class ))
		  (cvv-panel-item-list-global-state-list)))
    (if (loop for cached-state in cached-global-state-list
	      for current-state in (cvv-panel-item-list-global-state-list)
	      always (eq cached-state current-state))
	item-list
	(progn (eval-cache-flush item-list)
	       (cached-cvv-panel-item-list object panel-class screen)))))


#+broken
(defun get-cvv-object-panel-from-resource (object object-class panel-class screen &rest args)
  (let ((resource (eval-cache
		   (get-cvv-panel-resource object-class panel-class screen)
		   (make-instance 'cvv-panel-resource))))
    (or (find-free-panel resource screen object )
	(add-panel resource
			(apply 'make-cvv-panel-internal *toolkit*
			       (cached-cvv-panel-item-list object panel-class screen)
			       :resource-p nil args)))))

(defparameter *cvv-object-panel-controls*
  `((panel-controls nil :button-list
     :items ((quit-panel "Quit")
	     (lock-panel "Lock"
	      :button-type :toggle-button
	      :documentation "Lock Panel for use by only this object")
	     (update-panel "Update"))
     :group :preamble
     )
    ;;,*cvv-panel-control-separator*
    ))

(defvar *last-cvv-panel*)
(declaim (special *default-group-items*))

;;; type T for Strat (images too)
(defmethod make-cvv-object-panel ((object t)
				  &key screen
				  (panel-class *cvv-top-level-panel-class*)
				  (default-group-items *default-group-items*)
				  (resource-p t))
  (setq screen (default-object-menu-screen screen))
  ;;(format t "make-cvv-object-panel screen = ~a~%" screen)
  (let* (;(item-group-specs (cvv-object-panel-item-group-spec object))
	 (object-class (cvv-panel-class object ))
	 (args (list :panel-class panel-class
		     :screen screen
		     ;; want the name of the menu to be the most specific class creating item list
		     :label " "		; label  ;  do not force a new panel for each object class

		     ;;:item-group-specs item-group-specs
		     :group-enable t
		     :default-group-items default-group-items
		
		     :callback-function 'UPDATE-OBJECT-FROM-PANEL-CALLBACK
		     ;;:pop-up nil
		     :pop-up t
		     ;;:panel-controls *cvv-object-panel-controls*
		     )) )
    (setq *last-cvv-panel*
	  (if nil ; resource-p
	      (apply 'get-cvv-object-panel-from-resource
		     object object-class panel-class screen args)
	      (eval-cache (cached-make-cvv-panel object-class panel-class screen default-group-items)
		  ;;(format t "Computing make-cvv-panel ~a~%" object)
		  (let* ((item-list (cached-cvv-panel-item-list object panel-class screen))
			 (panel
			  (apply 'make-cvv-panel (append *cvv-object-panel-controls* item-list)
				 args))
			 )
		    panel
		    ))))))

(defun update-object-from-panel-callback (cvv-panel widget item-name event args)
  (format t "update-object-from-panel-callback ~a~%" (list item-name event args (get-prop cvv-panel :object)))
  (case item-name
    (object (let* ((class (get-prop cvv-panel :panel-class))
		   (objfrag (gui::pick-an-object "Pick an Object"
					    :predicate #'(lambda(obj) (typep obj class))))
		   (object (obj::object objfrag))
		   )
	      (if (or (null class) (typep object class))
		  (set-object cvv-panel objfrag)
		  (gui::report-error-to-gui "Object of Wrong Class for this panel")
		  )))

    (otherwise
     (let ((object (get-prop cvv-panel :object)))
       (when object
	 (unless (eq event 'tk::destroy)
	   ;; errors are possible-here after a destroy-callback
	   (update-object-from-cvv-panel object cvv-panel item-name)))))))


(defmethod panel-class-title-string ((pane base-cvv-object-panel )) nil)

(allow-redefinition
 
(defmethod set-object ((panel base-cvv-object-panel) objfrag)
  (let ((object (if (consp objfrag) (car objfrag) objfrag))
	(fragment (if (consp objfrag) (cadr objfrag) nil)))
    (setf (get-prop panel :object) object)
    (setf (get-prop panel :fragment) fragment)
    (setf (get-prop panel :panel-class) (cvv-panel-class object) )
    (let* ((object-class-name (or (obj::short-name-string object) ""))
	   (panel-type-name (panel-class-title-string panel))
	   (label (if panel-type-name
		      (format nil "~a ~a" panel-type-name object-class-name )
		      object-class-name )))
      (tk::set-panel-title panel label))
    (when object
      (update-cvv-panel-from-object object fragment panel)
      (loop for inf-panel in (get-prop panel :inferior-panels)
	    do (set-object inf-panel objfrag))
      )))

) ; end allow-redefinition




(defmacro define-object-menu (class &key item-list update-panel-from-object-fn update-object-from-panel-fn (panel-class *cvv-top-level-panel-class* ))
  `(progn
     (defmethod cvv-panel-class ((object ,class))
       ',class)
     (defmethod cvv-panel-item-list :around ((object ,class)
					     (panel-class (eql ',panel-class ))
					     )
       (union-item-lists (and *next-cvv-panel-item-list-method-p* (call-next-method))
			 ,item-list))

     (defmethod update-cvv-panel-from-object :after ((object ,class) fragment
						     (cvv-panel ,panel-class) 
						     &optional what-changed)
       (funcall ,update-panel-from-object-fn object fragment cvv-panel what-changed))

     (defmethod update-object-from-cvv-panel or ((object ,class) (cvv-panel ,panel-class) item-name)
       (funcall ,update-object-from-panel-fn object cvv-panel item-name)
       )))

		   
(defun undefine-object-menu (class &optional (panel-class *cvv-top-level-panel-class* ))
  (list (when (eq panel-class *cvv-top-level-panel-class*)
	  (undefmethod `(cvv-panel-class (,class  ))))
	(undefmethod `(cvv-panel-item-list :around (,class ,panel-class )))
	(undefmethod `(update-cvv-panel-from-object :after (,class ,panel-class )))
	(undefmethod `(update-object-from-cvv-panel or (,class ,panel-class t)))))


;;; Default daemon methods:

(defmethod cvv-panel-item-list ((object gl-object) (class (eql *cvv-top-level-panel-class*)))
  nil)

#|
(lx::all-class-superiors (find-class 'basic-object))
|#

(defparameter *cvv-appearance* nil)

(defmethod cvv-panel-item-list :around ((object obj::gl-object)
					(panel-class t))
  (union-item-lists 
   `(

     (name "Name:" :string :group :preamble)
     ,(and (not *cvv-appearance*)
	   `(edit-graphics-style nil :button :button-label "Edit Graphics Style"
	     :group :preamble
	     ;;:widget-args ,(list qui::XmNalignment qui::XmALIGNMENT_BEGINNING)
	     ))
     ;;(change-superior "Superior:" :exclusive-list :visible-item-count 3 :group hierarchy )
     (feature-sets "Feature Sets:" :multiple-choose-list :visible-item-count 5 :group hierarchy )
     )
   (and *next-cvv-panel-item-list-method-p* (call-next-method))))


(defmethod update-cvv-panel-from-object :around ((object obj::gl-object) fragment
						 (cvv-panel base-cvv-object-panel)
						 &optional what-changed)
  (unless (eq object (get-prop cvv-panel :object))
    (set-object cvv-panel (list object fragment)))
  ;;(format t "update-cvv-panel-from-object ~a ~A ~%" cvv-panel what-changed )
  ;;(update-item-in-panel-hierarchy (top-of-panel-hierarchy cvv-panel) 'name (name object))
  (unless what-changed
    (set-item-value cvv-panel 'name (name object))
    #+incomplete
    (set-object-hierarchy-panel-feature-sets object cvv-panel))

  (prog1 (call-next-method)
    ;;(xflush cvv-panel)
    ) )


(defmethod update-object-from-cvv-panel or ((object obj::gl-object)
					    (cvv-panel base-cvv-object-panel ) item-name)
  
  (case item-name
    (update-panel (update-cvv-panel-from-object object (get-prop cvv-panel :fragment) cvv-panel ))
    (name (let ((name (get-item-value cvv-panel 'name)))
	    (setf (name object) name)
	    (update-item-in-panel-hierarchy (top-of-panel-hierarchy cvv-panel) 'name name))
	  t)
    (description (setf (get-prop object :description) (get-item-value cvv-panel 'description )) t)
    (feature-sets
     (edit-feature-sets-panel-update cvv-panel item-name )
     ;;(set-item-value cvv-panel 'feature-sets (object-feature-sets object))
     #+incomplete
     (set-object-hierarchy-panel-feature-sets object cvv-panel)
     )
    ;;(change-superior (edit-feature-sets-panel-update cvv-panel item-name ))
    
    ))

;; bogus
;;;(define-object-menu basic-object
;;;    ;;:panel-class base-cvv-object-panel
;;;    :item-list `((name "Name:" :string :group :preamble)
;;;                 ,(and (not *cvv-appearance*)
;;;                       `(edit-graphics-style nil :button :button-label "Edit Graphics Style"
;;;                         :group :preamble
;;;                         ;;:widget-args ,(list qui::XmNalignment qui::XmALIGNMENT_BEGINNING)
;;;                         ))
;;;                 ;;(change-superior "Superior:" :exclusive-list :visible-item-count 3 :group hierarchy )
;;;                 (feature-sets "Feature Sets:" :multiple-choose-list :visible-item-count 5 :group hierarchy )
;;;                 )
;;;    
;;;    :update-panel-from-object-fn
;;;    #'(lambda (object fragment cvv-panel what-changed)
;;;        (unless (eq object (get-prop cvv-panel :object))
;;;          (set-object cvv-panel (list object fragment)))
;;;        ;;(format t "update-cvv-panel-from-object ~a ~A ~%" cvv-panel what-changed )
;;;        ;;(update-item-in-panel-hierarchy (top-of-panel-hierarchy cvv-panel) 'name (name object))
;;;        (unless what-changed
;;;          (set-item-value cvv-panel 'name (name object))
;;;          #+incomplete
;;;          (set-object-hierarchy-panel-feature-sets object cvv-panel))
;;;
;;;        (prog1 (call-next-method)
;;;          ;;(xflush cvv-panel)
;;;          ) )
;;;
;;;    :update-object-from-panel-fn
;;;    #'(lambda (object cvv-panel item-name)
;;;        (case item-name
;;;          (name (let ((name (get-item-value cvv-panel 'name)))
;;;                  (setf (name object) name)
;;;                  (update-item-in-panel-hierarchy (top-of-panel-hierarchy cvv-panel) 'name name))
;;;                t)
;;;          (description (setf (get-prop object :description) (get-item-value cvv-panel 'description )) t)
;;;          (feature-sets
;;;           (edit-feature-sets-panel-update cvv-panel item-name )
;;;           #+incomplete
;;;           (set-object-hierarchy-panel-feature-sets object cvv-panel)
;;;           )
;;;          ;;(change-superior (edit-feature-sets-panel-update cvv-panel item-name ))
;;;    
;;;          ))    
;;;    )



;;;(defmethod get-coord-mode-alist ((object 3d-object))
;;;  (let ((format "8.2f"))
;;;    `((coord-mode "Coord Sys:" :abbrev-assoc :alist (("Local X Y Z          " nil))  :group position)
;;;      ;; units field may not be appropriate from some coord systems
;;;      (units "Local Units:" :assoc :alist
;;;       ((feet "Feet") (meters "Meters") (nil "?") )
;;;       :initial-value *cartesian-coordinate-system-cvv-menu-item-list-units-default*  :group position)
;;;      (x "X:" :float :group position :format ,format)
;;;      (y "Y:" :float :group position :format ,format)
;;;      (z "Z:" :float :group position :format ,format)
;;;      )))

;;;(defmethod get-coord-mode-alist ((object gl-object))
;;;  '((x "U:" :float :group position)
;;;    (y "V:" :float :group position)
;;;    ))

(defmethod get-coord-mode-alist ((object 3d-object))
  '((coord-mode "Coord Sys:" :abbrev-assoc :alist (("Local X Y Z          " nil))  :group position)
    ;; units field may not be appropriate from some coord systems
    (units "Local Units:" :assoc :alist
     ((feet "Feet") (meters "Meters") (nil "?") )
     :initial-value *cartesian-coordinate-system-cvv-menu-item-list-units-default*  :group position)
    (x "X:" :string :group position)
    (y "Y:" :string :group position)
    (z "Z:" :string :group position)
    ))

(defmethod get-coord-mode-alist ((object gl-object))
  '((x "U:" :string :group position)
    (y "V:" :string :group position)
    ))

(define-object-menu obj::gl-object
    :item-list (get-coord-mode-alist object)

    :update-panel-from-object-fn
    #'(lambda (object fragment cvv-panel what-changed)
	(ignore what-changed)
	(format t "update-panel-from-object-fn obj::gl-object ~a~%" what-changed)
	;;(format t "~a " what-changed)
	(unless what-changed
	  (let ((object-coord-mode (get-object-coord-mode object)))
	    (unless (equal object-coord-mode (get-item-value cvv-panel 'coord-mode))
	      ;;(format t "setting coord mode pulldown menu ~a~%" object-coord-mode )
	      (set-cvv-panel-3d-coordinate-mode cvv-panel object object-coord-mode))))

	(when (or (null what-changed) (eq what-changed 'position))
	  (flet ((update-position ()
		   (when fragment
		     (set-cvv-panel-coords cvv-panel object (obj::fragment-position fragment)))))
	    (update-position)
	    )))

    :update-object-from-panel-fn
    #'(lambda (object cvv-panel item-name)
	;;(format t "object-with-coord-frame  :update-object-from-panel-fn ~a~%" item-name)
	;;(break)
	(case item-name
	  (coord-mode
	   (let ((coord-mode (get-item-value cvv-panel 'coord-mode)))
	     (format t "object-with-coord-frame  :update-object-from-panel-fn ~a ~s~%" item-name coord-mode)
	     (set-object-coord-mode object coord-mode )
	     (set-cvv-panel-coord-labels cvv-panel object )
	     (update-cvv-panel-from-object object (get-prop cvv-panel :fragment) cvv-panel)
	     t))
	  ((feet meters)
	   ;;(format t "object-with-coord-frame  :update-object-from-panel-fn ~a~%" item-name)
	   (update-cvv-panel-from-object object (get-prop cvv-panel :fragment) cvv-panel 'position)
	   t)
	  ((x y z utm-x utm-y utm-z lat long geo-h)
	   (multiple-value-bind (x y z) (get-local-xyz-from-panel cvv-panel object item-name)
	     ;;(format t ":update-object-from-panel-fn ~a~%" (list x y z))
	     (update-object object (obj::move-to object (cv x y z)
				    (obj::fragment-position (get-prop cvv-panel :fragment))))
	     t))

	  ((:uvxy :uv-on-dtm :w) )
	  ))
    )

(defparameter *default-xyz-size-format* "%12.2f")

(define-object-menu obj::gl-xyz-sizable-object-mixin
    :item-list
  `((x-size "X Size:" :float :group geometry :format ,*default-xyz-size-format*)
    (y-size "Y Size:" :float :group geometry :format ,*default-xyz-size-format*)
    (z-size "Z Size:" :float :group geometry :format ,*default-xyz-size-format*))
  :update-panel-from-object-fn
  #'(lambda (object fragment cvv-panel what-changed)
      (ignore what-changed)
      (when (or (null what-changed) (eq what-changed 'size))
	(let ((k (3d-world-unit-to-panel cvv-panel)))
	  (with-slots (obj::sizes) object
	    (bind-vector-elements (x-size y-size z-size) obj::sizes
	      (set-item-value cvv-panel 'x-size (* k x-size))
	      (set-item-value cvv-panel 'y-size (* k y-size))
	      (set-item-value cvv-panel 'z-size (* k z-size)))
	    ))))
  :update-object-from-panel-fn
  #'(lambda (object cvv-panel item-name)
      (with-slots (x-size y-size z-size) object
	(case item-name
	  ((x-size y-size)
	   (update-object object
	       (let ((k (/ 1.0 (3d-world-unit-to-panel cvv-panel)))
		     (sizes (obj::sizes object)))
		 (setf (aref sizes 0) (* k (get-item-value cvv-panel 'x-size))
		       (aref sizes 1) (* k (get-item-value cvv-panel 'y-size)))))

	   t)
	  (z-size
	   (update-object object
	       (let ((k (/ 1.0 (3d-world-unit-to-panel cvv-panel)))
		     (sizes (obj::sizes object)))
		 (setf (aref sizes 2) (* k (get-item-value cvv-panel 'z-size)))))
	   t)
	  )))
  )

#|
(transforms::local-units-per-meter (parent obj))
 
(pcl::class-precedence-list (class-of obj))
(cvv-panel-item-list obj *cvv-top-level-panel-class*)
(progn *cvv-top-level-panel-class*)
(progn *next-cvv-panel-item-list-method-p*)

(trace cvv-panel-item-list)

(trace UNION-ITEM-LISTS)
(untrace)

(progn
  
(setq obj (caar (selected-objects *interactor*)))

(defparameter *default-group-items*
  '(;;(:preamble nil :group)
    (annotation "ANNOTATION:" :group :initial-value nil)
    (hierarchy "HIERARCHY:" :group :initial-value nil)
    (POSITION "LOCATION:" :group)
    (geometry "GEOMETRY:" :group)
    (misc "MISC:" :group :initial-value nil)
    ))


(setq house-object-cvv-panel
      (make-cvv-object-panel
       obj
       :default-group-items *default-group-items*
       :title "House"))
)
(update-cvv-panel-from-object obj (cadar (selected-objects *interactor*)) house-object-cvv-panel)

(cvv-panel-item-list obj *cvv-top-level-panel-class*)


(progn
  (eval-cache-flush-function 'cached-make-cvv-panel)
  (eval-cache-flush-function 'cached-cvv-panel-item-list))
(world  obj)
(make-3d-coord-mode-alist  (world  obj))
("Radius Alv 3d World" "CLARKE-1866 Long-Lat" "UTM Zone 13 in NAD-27")
(name obj)
(get-object-coord-mode obj)
(setf (get-prop (world  obj) :coord-mode) "UTM Zone 13 in NAD-27")

(set-cvv-panel-3d-coordinate-mode house-object-cvv-panel obj "UTM Zone 13 in NAD-27")
(tk::get-item-value house-object-cvv-panel 'coord-mode)
(tk::set-item-value house-object-cvv-panel 'coord-mode "UTM Zone 13 in NAD-27")
(get-item-value house-object-cvv-panel 'units))

(geographic-transform-alist (world  obj))
(world  obj)
(transforms::local-units-per-meter (world  obj))
(3d-world-unit-to-panel house-object-cvv-panel (transforms::local-units-per-meter (world  obj)))
(trace 3d-world-unit-to-panel)
(untrace 3d-world-unit-to-panel)
 
(cvv-menu-item-list (world  obj))

(describe (tk::get-named-item house-object-cvv-panel 'coord-mode))
(tk::widget-value (tk::widget (tk::get-named-item house-object-cvv-panel 'coord-mode)))



(trace set-cvv-panel-coords)
(untrace set-cvv-panel-coords)
(trace set-item-value)
(untrace set-item-value)
(trace cme::register-geographic-transform)
(pcl::class-precedence-list (find-class 'house-object-cvv-panel))
(trace tk::set-item-list)

(make-3d-coord-mode-alist  (world  obj))
(geographic-transform-alist (world  obj))
(setq tk::*tk-verbose* t)
(setq tk::*tk-verbose* nil)
(tk::item-class (tk::get-named-item house-object-cvv-panel 'coord-mode))
(tk::tcl-cmd `(source ,(namestring (truename "$F3D/tk/library/composite-widgets.tcl"))))
(tk::tcl-cmd `(source ,(namestring (truename "$F3D/tk/library/show-menu-doc.tcl"))))
(tk::tcl-cmd `(source ,(namestring (truename "$F3D/tk/library/qwidgets.tcl"))))
(tk::tcl-cmd `(source ,(namestring (truename "$F3D/tk/library/tk-utils.tcl"))))

(tk::tcl-cmd `(qwidget_class ,(tk::widget (tk::get-named-item house-object-cvv-panel 'coord-mode))))
|#


(defun get-object-coord-mode (object)
  (let* ((world (world object))
	 (mode (get-prop world :coord-mode)))
    (when (eq mode :undefined) (setq mode nil))
    (or mode
	(typecase object
	  (3d-object
	   (setf (get-prop world :coord-mode) (car (first (geographic-transform-alist world )))))
	  (otherwise :undefined) ))))

(defun set-object-coord-mode (object mode)
  (let ((world (world object)))
    (setf (get-prop world :coord-mode) mode)))

(defmethod transforms::local-units-per-meter ((object basic-object))
  (let* ((world (world object))
	 (coordinate-system (and world (coordinate-system world))))
    (when coordinate-system
      (transforms::local-units-per-meter coordinate-system))))


(defun 3d-world-unit-to-panel (panel &optional local-units-per-meter)
  (let* ((panel-units (get-item-value panel 'units))
	 (object (get-prop panel :object))
	 (world-units-per-meter (or local-units-per-meter
				    (and object (transforms::local-units-per-meter object)))))
    (when (consp world-units-per-meter) (setq world-units-per-meter (car world-units-per-meter)))
    ;;(break)
    ;;(format t "3d-world-unit-to-panel ~s~%" (list panel-units world-units-per-meter))
    ;;(setq foo panel-units)
    (if world-units-per-meter
	(case panel-units
	  (meters (values (/ world-units-per-meter) panel-units))
	  (feet (values (/ *feet-per-meter* world-units-per-meter) panel-units))
	  (otherwise 1.0)		; unknown
	  )
	(progn (set-item-value panel 'units nil)
	       1.0	)		; unknown
	)))


(defmethod set-cvv-panel-coord-labels (panel (object 3d-object) )
  (let* ((coord-mode (get-object-coord-mode object))
	 (geographic-transform-info (find-geographic-transform (world object) coord-mode))
	 (item-list (cdr (caddr geographic-transform-info)))
	 )
    ;;(format t "set-cvv-panel-coord-labels ~a~%" item-list)
    (loop for (component-item-name panel-item-label) in item-list
	  for panel-item-name in '(x y z)
	  do (ignore component-item-name )
	  do (tk::set-item-label panel panel-item-name panel-item-label)
	     
	  )
    ;;(qui::update-constraints panel )
    ))

(defun 3d-object-set-cvv-panel-coords
    (cvv-panel object position
     &optional
     (world (world object))
     (object-to-world-transform (object-to-world-transform object )) )
  (let ((coord-mode (get-object-coord-mode object)))
    (set-item-value cvv-panel 'coord-mode coord-mode)
    (let* ((geographic-transform-info (find-geographic-transform world coord-mode))
	   (geographic-transform (cadr geographic-transform-info))
	   (item-list (cdr (caddr geographic-transform-info)))
	   (coordinate-system (if geographic-transform
				  (to-coordinate-system geographic-transform)
				  world))
	   )
      ;;(break)
      #+incomplete
      (tk::sensitize-widget  (tk::get-named-item cvv-panel 'units)
			     (or (null coordinate-system)
				 (typep coordinate-system 'local-vertical-coordinate-system)))
      ;;(format t "3d-object-set-cvv-panel-coords ~s~%" (list coordinate-system k units-name))
      (flet ((map-units-name (name)
	       (case name
		 (meters "meters")
		 (feet "feet")
		 (otherwise (or name "") ))))
	(loop with vector = (transform-vector geographic-transform
					      (transform-vector object-to-world-transform position))
	      for i from 0 to 2
	      for comp = (aref vector i)
	      for cs-unit-name = (nth i (and coordinate-system
					     (transforms::component-units coordinate-system)))
	      for panel-item-name in '(x y z)
	      for (cs-item-name) = (or (pop item-list) (list panel-item-name))
	      ;;do (format t "~a " cs-item-name)
	      do (set-item-value
		     cvv-panel
		     panel-item-name
		     (cond ((memq cs-item-name '(lat long))
			    (transforms::to-deg-min-sec-string comp :abbrev t))
			   ((memq cs-item-name '(x y z ))
			    (multiple-value-bind (k units-name)
				(3d-world-unit-to-panel cvv-panel
							(transforms::local-units-per-meter coordinate-system))
			      (format nil  "~12,2f ~a" (* k comp) (map-units-name units-name))))
			       
			   (t (format nil  "~12,2f ~a" comp (map-units-name cs-unit-name)
				      ))))
	      )))))


(defmethod set-cvv-panel-coords (cvv-panel (object 3d-object) vertex)
  (3d-object-set-cvv-panel-coords cvv-panel object vertex))



;;; The cvv-menu-item-list methods are used by REGISTER-GEOGRAPHIC-TRANSFORM
;;; They are not called from this file, but the info they return is
;;; used in 3d-object-set-cvv-panel-coords and set-cvv-panel-coord-labels

(defparameter *cartesian-coordinate-system-cvv-menu-item-list-units-default* 'feet)

(defmethod cvv-menu-item-list ((coordinate-system transforms::cartesian-coordinate-system))
  (let ((name (name coordinate-system)))
    `((,name  ,name			;,(intern (string (gensym))) ; ,coordinate-system
	      "Local Coordinates (x, y, z) ")
      (x "X:" :float :format "%12.2f" :group position)
      (y "Y:" :float :format "%12.2f" :group position )
      (z "Z:" :float :format "%12.2f" :group position)
      (units "Units:" :assoc :alist
	     ((feet "Feet") (meters "Meters") (nil "?") )
	     :initial-value *cartesian-coordinate-system-cvv-menu-item-list-units-default*
	     :group position ))))

;;; gl-object inherits from transforms::basic-coordinate-system.

(defmethod cvv-menu-item-list ((coordinate-system transforms::basic-coordinate-system))
  (let ((name (name coordinate-system)))
    `((,name  ,name			;,(intern (string (gensym))) ; ,coordinate-system
	      "Local Coordinates (x, y, z) ")
      (x "X:" :float :format "%12.2f" :group position)
      (y "Y:" :float :format "%12.2f" :group position )
      (z "Z:" :float :format "%12.2f" :group position)
      (units "Units:" :assoc :alist
	     ((feet "Feet") (meters "Meters") (nil "?") )
	     :initial-value *cartesian-coordinate-system-cvv-menu-item-list-units-default*
	     :group position ))))

(defmethod cvv-menu-item-list ((coordinate-system transforms::lat-long-coordinate-system))
  (let ((name (name coordinate-system)))
    `((,name ,name			; ,coordinate-system
	     "Geographic Coordinates (degrees Latitude, degrees Longitude, meters Height)")
      (long "Long:" :string :group position)
      (lat "Lat:" :string :group position)
      (geo-h "Height:" :float :format "%4.2f"  :group position))))

(defmethod cvv-menu-item-list ((coordinate-system transforms::utm-coordinate-system))
  (let ((name (name coordinate-system)))
    `((,name ,name			;,coordinate-system
	     "Universal Transverse Mercator Coordinates (units are meters)")
      (utm-x "Utm X:" :float :format "%12.2f" :group position)
      (utm-y "Utm Y:" :float :format "%12.2f" :group position)
      (utm-z "Utm Z:" :float :format "%12.2f"  :group position))))



(defmethod make-3d-coord-mode-alist ((object 3d-world))
  (loop for (name transform item-list) in (geographic-transform-alist object)
	do (ignore name transform)
	collect (car item-list)))

(defmethod make-3d-coord-mode-alist ((object 3d-world))
  (loop for (name transform item-list) in (geographic-transform-alist object)
	do (ignore name transform)
	collect name))

(defmethod make-3d-coord-mode-alist ((object basic-world-mixin))
  nil)

(defmethod make-3d-coord-mode-alist ((object basic-object))
  (make-3d-coord-mode-alist (world object)))

(defmethod set-cvv-panel-3d-cordinate-mode
    (cvv-panel (object obj::gl-object ) object-coord-mode )
  (ignore cvv-panel object object-coord-mode)
  nil)

(defmethod set-cvv-panel-3d-coordinate-mode
    (cvv-panel (object 3d-object ) object-coord-mode )
  (tk::set-item-list (tk::get-named-item cvv-panel 'coord-mode)
		     (make-3d-coord-mode-alist object))
  (set-cvv-panel-coord-labels cvv-panel object )
  (set-item-value cvv-panel 'coord-mode object-coord-mode)
  (set-cvv-panel-coord-labels cvv-panel object ))

(defmethod set-cvv-panel-3d-coordinate-mode
    (cvv-panel (object t) object-coord-mode)
  (ignore cvv-panel object object-coord-mode)
  )
	       

(defmethod get-local-xyz-from-panel (cvv-panel (object 3d-object) item-name)
  (ignore item-name)
  (let* ((coord-mode (get-object-coord-mode object))
	 (geographic-transform-info (find-geographic-transform (world object) coord-mode))
	 (geographic-transform (cadr geographic-transform-info))
	 (cvv-item-list (cdr (caddr geographic-transform-info)))
	 (first-coord-sym (car (car cvv-item-list))))
    (case first-coord-sym
      (x
       (let ((k (/ 1.0 (3d-world-unit-to-panel cvv-panel))))
	 (values (* k (read-from-string (get-item-value cvv-panel 'x)))
		 (* k (read-from-string (get-item-value cvv-panel 'y)))
		 (* k (read-from-string (get-item-value cvv-panel 'z))))))
      (long
       (inverse-transform-vector geographic-transform
				 (cv
				  (transforms::from-deg-min-sec-string (get-item-value cvv-panel 'x))
				  (transforms::from-deg-min-sec-string (get-item-value cvv-panel 'y))
				  (read-from-string (get-item-value cvv-panel 'z)))))
	 
      (otherwise
       (inverse-transform-vector geographic-transform
				 (cv
				  (read-from-string (get-item-value cvv-panel 'x))
				  (read-from-string (get-item-value cvv-panel 'y))
				  (read-from-string (get-item-value cvv-panel 'z))))))))





;;; ***************  This remainder of this file in inside comments  *****************

#|

(define-object-menu basic-object
    :item-list `((world nil :label :read-only t)
		 (description "Description:" :multi-line-string :nlines 3)
		 )

    :update-panel-from-object-fn
    #'(lambda (object cvv-panel what-changed)
	(declare (ignore object cvv-panel what-changed)))

    :update-object-from-panel-fn
    #'(lambda (object cvv-panel item-name)
	(unless what-changed
	  (set-item-value cvv-panel 'world (name (world object)))
	  (set-item-value cvv-panel 'description (or (get-prop object :description) ""))
	  ))
    )


(defmethod update-object-from-cvv-panel or ((object basic-object) (cvv-panel cvv-object-panel) item-name)
  (case item-name
    (update-panel (setf (get-prop cvv-panel :object) nil)
		  ;;(set-object cvv-panel object )
		  (update-cvv-panel-from-object object cvv-panel )
		  )

    (name (setf (name object) (get-item-value cvv-panel 'name))
	  t)
    
    (edit-graphics-style
     (edit-object-graphics-style
      object
      (loop for pane = (pick-a-pane *interactor* "Pick a View")
	    when pane return (top-view pane))))
    (description (setf (get-prop object :description) (get-item-value cvv-panel 'description )) t)
    ))

;;; CVV-PANEL-ITEM-LIST-OVERRIDE provides a mechanism to build cvv-panel-item-lists
;;; which without being restricted to strict inheritance.
;;; This must be considered a PRAGMATIC KLUDGE.
;;; see (method cvv-panel-item-list-override PERSPECTIVE-TRANSFORM-OBJECT) for example.
(defmethod cvv-panel-item-list-override ((object basic-object))
  nil)

(defun make-cvv-panel-item-list-override (instance specializers-list)
  (let ((*next-cvv-panel-item-list-method-p* nil))
    (loop with generic-function = (symbol-function 'cvv-panel-item-list)
	  for specializers in specializers-list
	  for class-name = (if (listp specializers)
			       (car specializers)
			       specializers)
	  for panel-specializer = (or (and (listp specializers)
					   (cadr specializers))
				      `(eql ,*cvv-top-level-panel-class* ))
	  for class = (find-class class-name)
	  for panel-class-name = (if (listp panel-specializer)
				     (cadr panel-specializer)
				     panel-specializer)
	  ;;for class-instance = (clos::class-prototype class)
	  for method = (find-method generic-function '(:around)
			    (if (listp panel-specializer)
				(list class panel-specializer)
				(list class (find-class panel-class-name))))
			    
	  append (funcall (pcl::method-function method) instance panel-class-name ))))



#|   LHQ - Wed Jul  9 2003:  Ignore this for now

;;; **********************  CVV-OBJECT-PANEL GROUPS  **********************



(defparameter *cvv-object-menu-item-group-enable* t)
#||
(setq *cvv-object-menu-item-group-enable* nil)
||#


;;; Added the reader macros on the feature MOTIF-1.2 in the following
;;; defparameters as a workaround for a problem in Motif 1.1 that
;;; causes seg faults on the SGI.
;;;   Wed Jun 29 1994 heller@ai.sri.com


(defparameter *object-preamble-group*
	      '(:preamble name edit-graphics-style))

(proclaim '(special *object-annotation-group* *object-hierarcy-group*
	    *object-location-group* *object-geometry-group*
	    *object-appearance-group* *object-misc-group*))


;;; There is still some kind of problem with first time creation of cvv-panel on SGI.
;;;

;;; try it
(defparameter *set-nil-group-enable* nil)


(defun set-nil-group-enables (&optional (enable *set-nil-group-enable*) (enable2 enable))
  (setq *object-annotation-group*
	`((annotation "ANNOTATION:  " :group-enable :initial-value ,enable2
	   :documentation "Open or Close the Menu Items for Object ANNOTATION"  )
	  description))

  (setq *object-hierarcy-group*
	`((hierarchy "HIERARCHY:  " :group-enable :initial-value ,enable2
	   :documentation "Open or Close the Menu Items for Object HIERARCHY" )
	  ))

  (setq *object-location-group*
	`((position "LOCATION:  " :group-enable :initial-value ,enable
	   :documentation "Open or Close the Menu Items for Object LOCATION"
	   :button-list (("UV@Z" :uvxy :documentation "UV@Z"
			  :command (:object-command :uvxy))
			 ("UV-DTM":uv-on-dtm :documentation "UV-DTM" )
			 ("W" :w :documentation "W" )))
	  world COORD-MODE UNITS x y z LAST-SELECTED-HANDLE
	  omega phi kappa
	  ) )

  (setq *object-geometry-group*
	'((geometry "GEOMETRY:  " :group-enable :initial-value t
	   :documentation "Open or Close the Menu Items for Object GEOMETRY"
	   :button-list (("XY Size" :change-xy-sizes )
			 ("Rot/Scale" :rotate-scale-xy)
			 ("Z Size" :change-z-size)
			 ))
	  X-SIZE Y-SIZE Z-SIZE ) )
  (setq *object-appearance-group*
	(and *CVV-APPEARANCE*
	     `((appearance "APPEARANCE:  " :group-enable :initial-value ,enable
	   :documentation "Open or Close the Menu Items for Object APPEARANCE" )
	  ;;DRAWING-THRESHOLD
	  COLOR line-width  DASH-LENGTH DASH-SPACING)))
  
  (setq *object-misc-group*
	'((misc "MISC:  " :group-enable :initial-value t
	   :documentation "Open or Close the Miscellaneous Object Items" )
	  ))

  (setq *cvv-object-panel-item-group-spec*
	(list *object-preamble-group*
	      *object-annotation-group*
	      *object-hierarcy-group*
	      *object-location-group*
	      *object-geometry-group*
	      '(internal-cam-params)
	      '(resection)
	      *object-appearance-group*
	      ;; *object-never-enable-group*
	      *object-misc-group*
	      ))
  )

(SET-NIL-GROUP-ENABLES)

(defparameter *cvv-object-panel-item-group-spec*
	      (list *object-preamble-group*
		    *object-annotation-group*
		    *object-hierarcy-group*
		    *object-location-group*
		    *object-geometry-group*
		    '(internal-cam-params)
		    '(resection)
		    *object-appearance-group*
		   ;; *object-never-enable-group*
		    *object-misc-group*
		    ))

;;; type T for Strat (images too)
(defmethod cvv-object-panel-item-group-spec ((object t))
  *cvv-object-panel-item-group-spec*)

|#




(defmacro define-cvv-panel-classes (&rest class-list)
  `(progn . ,(loop for class in class-list
		   collect `(defmethod cvv-panel-class ((object ,class ))
			      ',class))))


;;; these are needed for correct object class to be used when caching cvv-item-lists.

(define-cvv-panel-classes 3d-curve)

;;(define-cvv-panel-classes 3d-network 2d-network 3d-curve 2d-curve )
;;(define-cvv-panel-classes 3d-closed-curve closed-2d-curve)
;;(define-cvv-panel-classes 3d-text-object 2d-text-object)

#|
(loop for class in '(3d-network 2d-network 3d-curve 2d-curve)
      do (undefmethod `(cvv-panel-class (,class))))
|#

#+basic-network-undefined
(defmethod cvv-object-panel-item-group-spec ((object basic-network ))
  (list* (car *cvv-object-panel-item-group-spec*)
	 '(:never resample spline-tension revert-vertices)
	 (cdr *cvv-object-panel-item-group-spec*)))





(defparameter *2d-world-position-alist*
    '((x "U:" :string :documentation "Sensor U position in pixels.")
      (y "V:" :string :documentation "Sensor V position in pixels.")
      ))

(defmethod get-coord-mode-alist ((object basic-object))
  *2d-world-position-alist*)

;;; new


;;; new

(defmethod set-cvv-panel-coords (cvv-panel (object basic-object) vertex)
  (let ((coord-mode (get-object-coord-mode object)))
    (set-item-value cvv-panel 'coord-mode coord-mode)
    ;;(format t "set-cvv-panel-coords basic-object~%")
    (loop with object-to-world-transform = (object-to-world-transform object)
	  with k = 1.0
	  with vector = (vertex-transform vertex object-to-world-transform)
	  with cs-unit-name = nil
	  for panel-item-name in '(x y)
	  for i from 0 to 1
	  for comp = (aref vector i)
	  for comp-string = (format nil  "~12,2f ~a" (* k comp)
						     (case cs-unit-name
						       (meters "meters")
						       (feet "feet")
						       (otherwise (or cs-unit-name "") ))
				    )
	  do (set-item-value cvv-panel panel-item-name comp-string)	     
	  )))

(defun get-numeric-value (number-or-string)
  (etypecase number-or-string
      (number number-or-string)
      (string (read-from-string number-or-string))))

(defmethod get-local-xyz-from-panel (cvv-panel (object basic-object ) item-name)
  (case item-name
    ((x y) (let ((k 1.0)) ; this may be wrong for 2d-objects 
	       (values (* k (get-numeric-value (get-item-value cvv-panel 'x)))
		       (* k (get-numeric-value (get-item-value cvv-panel 'y)))
		       0.0)))))

 

#|
(set-item-value qui::*last-cvv-panel* 'dash-length nil)
(get-item-value qui::*last-cvv-panel* 'units)
(qui::get-named-item qui::*last-cvv-panel* 'dash-length)
|#



(defparameter *cvv-object-panel-dash-length-item*
  (and *CVV-APPEARANCE*
       '(dash-length "Dash Length:"
	 :assoc :alist ((nil "Solid") (1 "1") (2 "2") (3 "3") (4 "4") (5 "5") (6 "6"))
	 )))

(defparameter *cvv-object-panel-dash-spacing-item*
  (and *CVV-APPEARANCE*
       '(dash-spacing "Dash Spacing:"
	 :assoc :alist ((1 "1") (2 "2") (3 "3") (4 "4")(5 "5") (6 "6") ))))

(defparameter *cvv-object-panel-line-width-item*
  (and *CVV-APPEARANCE*
       '(line-width "Line Width:"
	 :assoc :alist ((nil "  "  "Draw Lines using Global Default Line Width")
			(0 "0") (1 "1") (2 "2") (3 "3") (4 "4")(5 "5") (6 "6") )
	 :initial-value nil)))

(define-object-menu object-with-modifiable-vertices-mixin
    :item-list
  `((last-selected-handle "Vert #:" :integer :documentation "Selected Vertex Number"
     :group position)
    ;;(open-for-vertex-modification "Verts Open:" :assoc :alist ((t "Yes") (nil "No")))
    (open-for-vertex-modification "Verts Open:" :yes-or-no
     :documentation "Vertices are Modifiable" :group misc)
    ;;,(cvv-panel-color-item)		; not dynamically changed
    ,*cvv-object-panel-line-width-item*
    ,*cvv-object-panel-dash-length-item* ; not dynamically changed
    ,*cvv-object-panel-dash-spacing-item* ; not dynamically changed
    )
  :update-panel-from-object-fn
  #'(lambda (object cvv-panel what-changed)
      
      (ignore what-changed)
      (unless what-changed
	(set-item-value cvv-panel 'open-for-vertex-modification (open-for-vertex-modification object))
	;; THESE ITEMS ARE IN WRONG PLACE -- SHOULD BE SOMEWHERE RELATED TO WIRE FRAME DRAWING
	(set-item-value cvv-panel 'dash-length (getf (line-dash-style object) :dash-length))
	(set-item-value cvv-panel 'dash-spacing (getf (line-dash-style object) :dash-spacing))
	(set-item-value cvv-panel 'line-width (line-width object))
	(let ((vertex-num (vertex-id object (last-selected-handle object ))))
	  (when vertex-num
	    (set-item-value cvv-panel 'last-selected-handle vertex-num)))
	))
    
  :update-object-from-panel-fn
  #'(lambda (object cvv-panel item-name)
      (case item-name
	(open-for-vertex-modification
	 (update-object object
	     (set-open-for-vertex-modification object
					       (get-item-value cvv-panel 'open-for-vertex-modification)))
	 t)

	((dash-length dash-spacing line-width)
	 (update-object object
	     (let ((dash-length (get-item-value cvv-panel 'dash-length))
		   (line-width (get-item-value cvv-panel 'line-width)))
	       (set-object-graphics-style-attributes
		object
		:dash-style (when dash-length
			      (list :dash-length dash-length
				    :dash-spacing (get-item-value cvv-panel 'dash-spacing)))
		:line-width line-width)))
	 ))
      ))



(define-object-menu house-object
    :item-list
  '((roof-type "Roof Type:" :assoc :alist ((:gable "Gable") (:hip "Hip") (:shed "Shed"))
     :group geometry)
    (roof-pitch "Roof Pitch:" :float :documentation "Roof Pitch (dz/dx)" :group geometry)
    (roof-overhang "Roof Overhang:" :float :documentation "Roof Overhang" :group geometry)
    )
  
  :update-panel-from-object-fn
  #'(lambda (object cvv-panel what-changed)
      (ignore what-changed)
      (with-slots (roof-pitch roof-overhang roof-type) object
	(set-item-value cvv-panel 'roof-type roof-type)
	(set-item-value cvv-panel 'roof-pitch roof-pitch)
	(set-item-value cvv-panel 'roof-overhang roof-overhang)
	))
  :update-object-from-panel-fn
  #'(lambda (object cvv-panel item-name)
      (with-slots (roof-pitch) object
	(case item-name
	  (roof-type
	   (update-object object
	       (set-roof-type object (get-item-value cvv-panel 'roof-type)))
	   t)
	  (roof-pitch
	   (update-object object
	       (set-roof-pitch object (get-item-value cvv-panel 'roof-pitch)))
	   t)
	  (roof-overhang
	   (update-object object
	       (set-roof-overhang object (get-item-value cvv-panel 'roof-overhang)))
	   t)
	  ))
      ))


(defvar *stipple-patterns* nil)

(setq *stipple-patterns*
      '(;;("stipple-none" ((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0)))
	("stipple-solid" ((1 1 1 1) (1 1 1 1) (1 1 1 1) (1 1 1 1)))
	("stipple2" ((0 1 0 1) (1 0 1 0) (0 1 0 1) ( 1 0 1 0)))
	("stipple1" ((0 1 0 0) (0 0 0 1) (1 0 0 0) (0 0 1 0)))
	("stipple3" ((0 1 0 0) (0 0 0 0) (0 0 0 1) (0 0 0 0)))
	("stipple4" ((0 1 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0)))
	
	))

;;; Need to fix this so the alist has names for the stipples rather than random c-structs.
;;; Must fix the stipple-setting code to accept a symbol designating a stipple, and
;;; do the appropriate screen dependent translation.
(defun get-stipple-alist (&optional screen (stipple-patterns *stipple-patterns*))
  (setq screen (default-object-menu-screen screen))
  (or (get-prop screen :stipple-alist)
      (setf (get-prop screen :stipple-alist)
	    (list* '("No" nil)
		   ;;'("Solid" t)
		   (loop for (name pattern) in stipple-patterns
			 collect (qui::make-stipple-alist-item screen name pattern))))))


(define-object-menu basic-closed-curve
    :item-list
  `((stipple "Stipple:" :assoc :alist ,(get-stipple-alist) :group appearance)
    )
  
  :update-panel-from-object-fn
  #'(lambda (object cvv-panel what-changed)
      (ignore what-changed)
      ;;(format t "basic-curve :update-panel-from-object-fn ~A~%" what-changed)
      (unless what-changed
	(with-slots (closed-p last-selected-handle vertices-visible-p ) object
	  (set-item-value cvv-panel 'stipple (stipple object))
	  )))
  :update-object-from-panel-fn
  #'(lambda (object cvv-panel item-name)
      ;;(break) (format t "basic-curve :update-object-from-panel-fn ~a~%"
      ;;item-name)
      (case item-name
	(stipple (update-object object
		     ;;(setf (stipple object) (get-item-value cvv-panel 'stipple))
		     (set-object-graphics-style-attributes object :stipple (get-item-value cvv-panel 'stipple))
		     )
		 ))
      ))

(define-object-menu generic-ribbon-curve
    :item-list
  `((stipple "Stipple:" :assoc :alist ,(get-stipple-alist) :group appearance)
    )
  :update-panel-from-object-fn
  #'(lambda (object cvv-panel what-changed)
      (ignore what-changed)
      (unless what-changed
	(set-item-value cvv-panel 'stipple (stipple object))
	))

  :update-object-from-panel-fn
  #'(lambda (object cvv-panel item-name)
      (with-slots (closed-p last-selected-handle vertices-visible-p ) object
	(case item-name
	  (stipple (update-object object
		       ;;(setf (stipple object) (get-item-value cvv-panel 'stipple))
		       (set-object-graphics-style-attributes object :stipple (get-item-value cvv-panel 'stipple))
		       ))
	  ))
      ))


(defun get-color-slot-named (name &optional screen )
  (setq screen (default-object-menu-screen screen))
  ;;(ic::find-colormap-slot-named (ic::cme-color-map screen) name)
  (IC::GET-REGISTERED-COLOR-SLOT name) )

(define-object-menu basic-curve
    :item-list
  `((vertices-visible-p "Verts Visible:" :assoc :alist (("Yes" t) ("No" nil))
     ;;:group appearance
     )
    ;;(closed-p "Closed:" :assoc :alist (("Yes" t) ("No" nil)))
    
    
    ;; resampling is inappropriate for network objects (built on top of curves)
    (resample "Resample Dist:" :float-slider :min .1 :max 10.0 :format "%5g" :fraction-digits 1
     :documentation  "Resample Curve with this Distance Between Vertices.")
    (spline-tension "Spline Tension:" :float-slider  :min .1 :max 10.0 :fraction-digits 1 )
    (revert-vertices nil;; "Revert Vertices"
     :button :button-label "Revert Vertices"
     :documentation "Restore Vertices to State Prior to Resampling.")
    ;;(fill-p "Fill:" :yes-or-no :documentation "Fill interior of Closed Curves")
    ;;(last-selected-handle "Selected Vert:" :integer :documentation "Selected Vertex Number")
    ;;,(cvv-panel-color-item)		    ; not dynamically changed
    ,*cvv-object-panel-line-width-item*	; not dynamically changed
    ,*cvv-object-panel-dash-length-item* ; not dynamically changed
    ,*cvv-object-panel-dash-spacing-item* ; not dynamically changed
    )
  
  :update-panel-from-object-fn
  #'(lambda (object cvv-panel what-changed)
      (ignore what-changed)
      (unless what-changed
	;;(format t "basic-curve :update-panel-from-object-fn ~A~%" what-changed)
	(with-slots (closed-p last-selected-handle vertices-visible-p) object
	  (set-item-value cvv-panel 'closed-p closed-p)
	  (set-item-value cvv-panel 'vertices-visible-p vertices-visible-p)
	  (let ((vertex-num (vertex-num object last-selected-handle)))
	    (when vertex-num
	      (set-item-value cvv-panel 'last-selected-handle vertex-num)))
	  ;; we have a problem here since the length of the curve can be very large with lots of
	  ;; manually specified vertices.  
	
	  (qui::set-named-item-min-max cvv-panel
				       'resample 0.0
				       (* .25 (total-arc-length object))
				       ;;(/ (total-arc-length object) (length (vertices object)))
				       )
	  (set-item-value cvv-panel 'resample (resampling-interval object))
	  (set-item-value cvv-panel 'spline-tension (or (get-prop object :spline-tension) 1.0))
	  )))
  :update-object-from-panel-fn
  #'(lambda (object cvv-panel item-name)
      ;;(break)
      ;;(format t "basic-curve :update-object-from-panel-fn ~a~%" item-name)
      (with-slots (closed-p last-selected-handle vertices-visible-p ) object
	(case item-name
	  (closed-p
	   (update-object object (setf closed-p (get-item-value cvv-panel 'closed-p)))
	   t)
	  ;;(color (update-object object (setf (color object) (get-item-value cvv-panel 'color))))

	  (vertices-visible-p
	   (update-object object (setf vertices-visible-p (get-item-value cvv-panel 'vertices-visible-p)))
	   t)
	  ((resample spline-tension)
	   (let ((interval (get-item-value cvv-panel 'resample)))
	     (setf (get-prop object :spline-tension)
		   (float (get-item-value cvv-panel 'spline-tension)))
	     (when (and (numberp interval);;(> interval 0.0)
			)
	       (update-object object (resample-curve object (max interval 1.0) ))
	       (update-cvv-panel-from-object object cvv-panel  ))))
	  (revert-vertices (update-object object (revert-curve-vertices object))
			   (update-cvv-panel-from-object object cvv-panel  ) )
	  
	  ;;(last-selected-handle (update-object object (setf last-selected-handle (get-item-value cvv-panel 'last-selected-handle))))
	  ))
      ))





;;(undefmethod '(execute-object-message :after (basic-object t t t t t)))

(defun rebuild-cvv-object-panel (old-panel object)
  (let ((widget (xw::menu-widget old-panel)))
    ;;(xw::xsync (qui::Xtdisplay widget) 0) 
    (multiple-value-bind (x y) (qui::get_widget_root_position widget)
      (let ((top-offset 26) (left-offset 6) ; THIS IS A TOTAL CROCK 
	    ;; OLIT decorations around menu - must figure out how to get these from the menu
	    ;(top-offset 0) (left-offset 0)
	    )
	(xw::menu-widget-popdown widget)
	(let ((panel  (make-cvv-object-panel object)))
	  (when panel
	    ;;(format t "xw::set_menu_position ~a ~A~%" x y)
	    (if t
		(xw::pop-up-cvv-panel panel (- x left-offset) (- y top-offset))
		(let ((widget (xw::menu-widget panel)))
		  (xw::set_menu_position widget x y)
		  (xw::menu-widget-popup widget)))
	    (menu-to-front panel)
	    (set-object panel object)
	    ))))))


(defparameter *update-cvv-panel-execute-object-message-messages-to-ignore*
    '(delete-object com-menu-click ))

;;; this allows updates after each mouse click (rather than each mouse motion)
(defmethod execute-object-message :after ((object basic-object) message button window x y) 
  (ignore message button window x y)
  ;;(format t "execute-object-message ~a~%" message)
  (unless (memq message *update-cvv-panel-execute-object-message-messages-to-ignore*)
    ;; this needs to be smarter -- make-cvv-object-panel does a lot of computing
    ;; Setting object to *object-selection-status* is needed when open-parent or open-inferiors is executed.
    (setq object (or *object-selection-status* object))
    (let ((panel (eval-cache-probe (make-cvv-object-panel object :screen (screen window) ))))
      (when panel
	(when (and (xw::cvv-menu-pinned panel) (eq object (get-prop panel :object)))
	  (menu-to-front panel)
	  ;;(format t "execute-object-message ~a~%" message)
	  (update-cvv-panel-from-object object panel))))))

(defparameter *cvv-panel-update-while-tracking* t)
;;(setq *cvv-panel-update-while-tracking* nil)

(defparameter *cvv-panel-update-while-tracking-disallowed-move-modes*
    '(:uvxy :uvxy-drop-z :uv-on-dtm :move-z :w :sun-w :sun-vector :other-camera-vector))

(setq *cvv-panel-update-while-tracking-disallowed-move-modes* nil)

(defmethod maybe-update-cvv-panel ((object basic-object ) screen move-mode)
  (when (and *cvv-panel-update-while-tracking*
	     (not (memq move-mode *cvv-panel-update-while-tracking-disallowed-move-modes*)))
    (update-cvv-object-panel object screen move-mode t)))


(defmethod update-cvv-object-panel ((object basic-object ) screen move-mode panel-object-must-match)
  (let ((panel (eval-cache-probe (make-cvv-object-panel object :screen screen))))
    ;; this needs to be smarter -- make-cvv-object-panel does a lot of computing.
    ;; The method cvv-panel-item-list needs some kind of cache.
    (when (and panel (xw::cvv-menu-pinned panel)
	       (or (null panel-object-must-match) (eq object (get-prop panel :object))))
      (update-cvv-panel-from-object
       object panel
       (case move-mode
	 ((:uvxy :uvxy-drop-z :uv-on-dtm :move-z :w :sun-w
		 :vertex-uvxy :vertex-uvxy-drop-z :vertex-uv-on-dtm
		 :vertex-z :vertex-w
		 :conj-xy)
	  'position)
	 ((:change-xy-sizes :rotate-scale-xy 
			    :change-scale :change-taper-rate
			    :change-x-size :change-y-size :change-x-and-y-size :change-z-size)
	  'size)
	 ((:z-rot :zp-rot :uv-roll :w-rot :az-elev)
	  'orientation)
	 ((:change-x-expt :change-y-expt :change-z-expt :change-xy-expt :change-expts)
	  'exponents))
       ))))

(defparameter *continuous-cvv-object-menu-update* t)

;;; patch around Lucid precompile-generic-functions bug
(defmethod mouse-modify-object-around
	   ((object basic-object) view move-mode dx dy)
  (multiple-value-prog1
      (update-object object 
	  (mouse-modify-object object view move-mode dx dy)
	)
    (when *continuous-cvv-object-menu-update* ; (eq (get-prop view 'when-rendered) 'continuous)
      (maybe-update-cvv-panel object (screen (view-window view)) move-mode))))


#|
(setq panel nil)
(setq panel (make-ccv-panel tower ))

(update-cvv-panel-from-object tower panel)

(setq curve-panel (make-ccv-panel curve ))

(let ((panel (make-cvv-object-panel house :screen (ic::default-screen))))
  (xw::set-named-item-sensitive panel 'roof-pitch t))

(let ((panel (make-cvv-object-panel house :screen (ic::default-screen))))
  (xw::set-named-item-sensitive panel 'object t))


(let ((panel (make-cvv-object-panel pto :screen (ic::default-screen))))
  (update-cvv-panel-from-object pto panel))

(progn
  (eval-cache::eval-cache-flush-function 'xw::make-choose-variable-values-menu)
  nil)

(progn
  (eval-cache::eval-cache-flush-function 'ic::make-choose-variable-values-menu)
  nil)
|#



;;; ***********************  CVV-OBJECT-MENU FOR BASIC-IMAGE  ***********************


(defmethod cvv-panel-item-list ((object ic::basic-image) (panel-class t) )
  `((image "Image:" :button :documentation "Pick an Image")
    (name "Name:" :string )
    (description "Description:" :multi-line-string :nlines 3)
    (dimensions "Dimensions:" :lisp-value :read-only t
		:documentation "Read only")
    (zoom-factor "Zoom factor:" :float :read-only t
		 :documentation "Ratio of screen pixels to image pixels (Read only)")
    ))

(defmethod update-cvv-panel-from-object ((object ic::basic-image) cvv-panel &optional what-changed)
  (ignore cvv-panel what-changed)
  )

(defmethod update-cvv-panel-from-object :around ((object ic::basic-image) cvv-panel &optional what-changed)
  (unless (eq object (get-prop cvv-panel :object))
    (set-object cvv-panel object))
  (set-item-value cvv-panel 'name (get-prop object :name))
  (set-item-value cvv-panel 'description (or (get-prop object :description) ""))
  (set-item-value cvv-panel 'dimensions (ic::image-dimensions object))
  (set-item-value cvv-panel 'zoom-factor (virtual-pixel-ratio  (get-prop cvv-panel :view)))
  (call-next-method))



(defmethod update-object-from-cvv-panel or ((object ic::basic-image) cvv-panel item-name)
  (case item-name
    (name (setf (get-prop object :name) (get-item-value cvv-panel 'name))
	  t)						    
    (description (setf (get-prop object :description) (get-item-value cvv-panel 'description )) t)
    (image
     (let ((image (loop for pane = (ic::pick-a-pane (ic::get-interactor) "Pick an Image")
			when pane
			  do (setf (get-prop cvv-panel :view) (top-view pane))
			when pane
			  return (top-image pane))))
       (set-object cvv-panel image)
       (update-cvv-panel-from-object image cvv-panel )
       ))
   

    ))

;;; cvv-panel-item-list-override provides a mechanism to build cvv-panel-item-lists
;;; which without being restricted to strict inheritance.
;;; This must be considered a PRAGMATIC KLUDGE.
;;; set (method cvv-panel-item-list-override PERSPECTIVE-TRANSFORM-OBJECT) for example.
(defmethod cvv-panel-item-list-override ((object ic::basic-image ))
  nil)


(defmethod cvv-panel-class ((object ic::basic-image))
  nil)

;;;This fn returns the ratio of pixels in the display window to image pixels
(defun virtual-pixel-ratio (view)
  (and view
       (ic::transform-matrix-zoom-factor (2d-to-window-matrix view) )
       ))




|#

(defmethod possible-radius-classes ((obj obj::gl-3d-object-mixin))
  (gethash (type-of obj) smc::*radius-subclass-table*))


;;; The menu-choose function has some package issues.


(defun com-change-class (interactor)
  (let* ((object (selected-object interactor))
         (choices (loop for sym in (possible-radius-classes (selected-object interactor))
                       collect (list (string-downcase (symbol-name sym)) :quote (string-upcase (symbol-name sym)))))
         (radius-class-name (menu-choose choices)))
    (when radius-class-name
      (let ((type (intern (symbol-name radius-class-name) "SMC")))
        (change-class object type)
        (redisplay (current-view interactor))))))
