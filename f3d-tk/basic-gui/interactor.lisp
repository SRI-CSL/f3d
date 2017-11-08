(in-package :gui)

;;; No direct dependencies on TK or OpenGL.

;;; ******************  INTERACTOR  ******************

;;; change to (defstruct-class interactor () ...) for ordinary CLOS classes.
;;; change to (defstruct-class interactor (base-struct-class) ...) for struct classes.

;;; The difference between CURRENT-WINDOW and SELECTED-WINDOW is that
;;; a command may be invoked on a non-selected-window.  CURRENT-WINDOW
;;; then refers the the window on which the comamnd was invoked.

(defstruct-class interactor ()
  ((popup-drag-start :initform nil :accessor popup-drag-start)
   (popup-drag :initform nil :accessor popup-drag)
   (drag-op :initform nil :accessor drag-op)
   (drag-object-type :initform nil :accessor drag-object-type)
   (drag-start-window-pos :initform nil :accessor drag-start-window-pos)
   (current-window-pos :initform nil :accessor current-window-pos)
   (window-motion :initform :initform :accessor window-motion)
   (current-window :initform nil)
   ;; flush current-view and replace with (top-view current-window)
   ;; (current-view :initform nil :accessor current-view) 
   (selected-window :initform nil )
   (selected-objects :initform nil)
   (previously-highlighted-views :initform nil)
   (world-center-of-rotation :initform nil :accessor world-center-of-rotation)
   ;; feedback-cache should contain the list (view feedback-buffer).
   ;; Must be cleared whenever view is redrawn, and set
   ;; whenever feedback-buffer is computed.
   (feedback-cache :initform nil :accessor feedback-cache)
   ))

(defparameter *interactor* (make-instance 'interactor))

;;; The only reference to tk
(defun tk::wait-for-any-event-sleep-time ()
  (if (popup-drag *interactor*)
      0
      tk::*wait-for-any-event-sleep-time*))

(defun selected-window (&optional (interactor *interactor*))
  (with-class-slots interactor (selected-window) interactor
    selected-window))

(defun (setf selected-window) (new-window &optional (interactor *interactor*))
  (with-class-slots interactor (selected-window) interactor
    (setf selected-window new-window)))

(defun current-window (&optional (interactor *interactor*))
  (with-class-slots interactor (current-window) interactor
    current-window))

;;; this is specialized by temporal-view
(defmethod view-2d-descr (view)
  (2d-world view))
  
(import 'view-2d-descr :obj)

(defun (setf current-window) (win &optional (interactor *interactor*))
  (unless (or (null win) (typep win 'basic-window))
    (error "(setf (current-window interactor)) ...) : not a wbasic-indow or NULL"))
  (with-class-slots  interactor (current-window) interactor
    (setf current-window win)))

(defun current-view (&optional (interactor *interactor*))
  (top-view (current-window interactor)))

(defmethod clear-feedback-cache ()
  (setf (feedback-cache *interactor*) nil))

(defmethod selected-pane (&optional (interactor *interactor*))
  (selected-window interactor))

(defmethod selected-view (&optional (interactor *interactor*))
  (top-view (selected-pane interactor)))

(defmethod selected-objects (&optional (interactor *interactor*))
  (with-class-slots interactor (selected-objects) interactor
    selected-objects))

(defmethod (setf selected-objects) (objects &optional (interactor *interactor*))
  (with-class-slots interactor (selected-objects)
      interactor
    ;;(format t "(setf selected-objects) ~a~%" objects)
    (setf selected-objects objects)))

;;; This is wrong when bridging 3d-worlds.
;;;(defmethod selected-objects-in-world (world &optional (interactor *interactor*))
;;;  (loop for item in (selected-objects interactor)
;;;        for object = (if (consp item) (car item) item)
;;;        when (eq (world object) world)
;;;          collect object))

(defmethod selected-objects-in-world ((world gl-3d-world) &optional (interactor *interactor*))
  (loop for item in (selected-objects interactor)
	for object = (if (consp item) (car item) item)
	when (typep object 'gl-3d-object-mixin)
	  collect object))

(defmethod selected-objects-in-world ((world gl-2d-world) &optional (interactor *interactor*))
  (loop for item in (selected-objects interactor)
	for object = (if (consp item) (car item) item)
	when (typep object 'gl-2d-object-mixin)
	  collect object))
		 
;;; This version doesn't work with multi-LVCS environment.
;;;(defmethod selected-objfrags-in-world (world &optional (interactor *interactor*))
;;;  (loop for item in (selected-objects interactor)
;;;        for object = (if (consp item) (car item) item)
;;;        when (eq (world object) world)
;;;          collect item))
		 
;;;(defmethod selected-objfrags-in-world (world &optional (interactor *interactor*))
;;;  (loop with world-class = (typecase world
;;;                             (gl-3d-world 'gl-3d-world)
;;;                             (gl-2d-world 'gl-2d-world)
;;;                             )
;;;        for item in (selected-objects interactor)
;;;        for object = (if (consp item) (car item) item)
;;;        when (typep (world object) world-class)
;;;          collect item))

#+never
(progn
		 
(defmethod selected-objfrags-in-world ((world gl-3d-world) &optional (interactor *interactor*))
  (loop for item in (selected-objects interactor)
	for object = (if (consp item) (car item) item)
	when (typep (world object) 'gl-3d-world)  ; why is this test needed?
	  collect item))

(defmethod selected-objfrags-in-world ((world gl-2d-world) &optional (interactor *interactor*))
  (loop for item in (selected-objects interactor)
	for object = (if (consp item) (car item) item)
	when (eq (world object) world)
	  collect item))
	
) ;end progn

;#+experimental
(progn

(defmethod selected-objects-in-world ((world gl-3d-world) &optional (interactor *interactor*))
  (loop for item in (selected-objects interactor)
	for object = (if (consp item) (car item) item)
	when (typep object 'gl-3d-object-mixin)
	  collect object
	  and when (typep object '3d-composite-object)
		nconc (obj::all-children object)))

(defmethod selected-objects-in-world ((world gl-2d-world) &optional (interactor *interactor*))
  (loop for item in (selected-objects interactor)
	for object = (if (consp item) (car item) item)
	when (typep object 'gl-2d-object-mixin)
	  collect object
	  and when (typep object '2d-composite-object)
		nconc (obj::all-children object)))
		 
(defmethod selected-objfrags-in-world ((world gl-3d-world) &optional (interactor *interactor*))
  (loop for item in (selected-objects interactor)
	for object = (if (consp item) (car item) item)
	when (typep object 'gl-3d-object-mixin) 
	  collect item
	  and when (typep object '3d-composite-object)
		nconc (obj::all-children object)))

(defmethod selected-objfrags-in-world ((world gl-2d-world) &optional (interactor *interactor*))
  (loop for item in (selected-objects interactor)
	for object = (if (consp item) (car item) item)
	when (eq (world object) world)
	  collect item
	  and when (typep object '2d-composite-object)
		nconc (obj::all-children object)))

) ; end progn
	
(defmethod selected-objfrags-in-world ((world t) &optional (interactor *interactor*))
  nil)
  
(defmethod previously-highlighted-views (&optional (interactor *interactor*))
   (with-class-slots interactor (previously-highlighted-views) interactor
     previously-highlighted-views))

(defmethod (setf previously-highlighted-views)
	   (objects &optional (interactor *interactor*))
  (with-class-slots interactor (previously-highlighted-views) interactor
    (setf previously-highlighted-views objects)))

(defmethod selected-object (&optional (interactor *interactor*))
  (let ((item (car (selected-objects interactor))))
    (if (consp item) (car item) item)))

(defmethod selected-object-fragment (&optional (interactor *interactor*))
  (let ((item (car (selected-objects interactor))))
    (if (consp item) (cadr item) nil)))


(defmethod selected-object-position (&optional (interactor *interactor*))
  (let ((item (car (selected-objects interactor))))
    (if (consp item)
	(obj::fragment-position (cadr item))
	nil)))

;;; Which lvcs?  
(defmethod selected-object-world-position (&optional (interactor *interactor*) world)
  (let* ((item (car (selected-objects interactor)))
	 (frag-descr (and (consp item) (cadr item))))
    (when frag-descr
      (let* ((object (obj::object frag-descr))
	     (world (or world (object-view-world object (current-view interactor)))))
	(transform-vector (object-to-world-transform object world)
			  (obj::fragment-position frag-descr))))))

(defun object-world-position (object &optional world)
  (transform-vector (object-to-world-transform object world)
		    (obj::origin object)))

(defun object-parent-position (object)
  (transform-vector (object-to-parent-transform object)
		    (obj::origin object)))

;;; This doesn't work correctly when selected-object is a composite.
;;;(defmethod selected-object-parent-position (&optional (interactor *interactor*))
;;;  (let* ((item (car (selected-objects interactor)))
;;;         (frag-descr (and (consp item) (cadr item))))
;;;    (when frag-descr
;;;      (transform-vector (object-to-parent-transform (obj::object frag-descr))
;;;                        (obj::fragment-position frag-descr)))))

;;;(defmethod selected-object-parent-position (&optional (interactor *interactor*))
;;;  (let* ((item (car (selected-objects interactor)))
;;;         (frag-descr (and (consp item) (cadr item))))
;;;    (when frag-descr
;;;      (let ((selected-object (car item)))
;;;        (transform-vector (loop for obj = (obj::object frag-descr)
;;;                                  then (parent obj)
;;;                                until (eq obj (parent selected-object))
;;;                                collect (object-to-parent-transform obj))
;;;                          (obj::fragment-position frag-descr))))))



;;; This must carefully handle the case where selected-object is a composite
;;; and the (object frag-descr) is an inferior.
(defmethod selected-object-parent-position (&optional (interactor *interactor*))
  (let* ((item (car (selected-objects interactor)))
	 (frag-descr (and (consp item) (cadr item))))
    (when frag-descr
      (let ((selected-object (car item)))
	(transform-vector (cs-to-superior-cs-transform (obj::object frag-descr)
						       (parent selected-object))
			  (obj::fragment-position frag-descr))))))


(defmethod (setf selected-object) (object &optional (interactor *interactor*))
  (setf (selected-objects interactor) object))

(defmethod select-object (object interactor &optional (vertex-id 0))
  (setf (selected-objects interactor)
	(list (list object
		    (make-instance 'object-vertex
				   :object object
				   :vertex-id vertex-id)))))

(defmethod set-state ((interactor interactor) win window-pos)
  (with-class-slots interactor
	(current-window current-window-pos window-motion)
      interactor
    ;;(format t "set-state ~a~%" win)
    (setq current-window win
	  window-motion (vector-difference window-pos
					   (or current-window-pos window-pos))
	  current-window-pos window-pos
	  )))


#+old
(defmethod tandem-window-list ((view view))
  nil)

;;; Mod CIC 4/13/2006 - use the spatial group to get the tandem-window-list:
(defmethod tandem-window-list ((view view))
  (let* ((group (view-spatial-group view))
	 (views (view-group-views group)))
    (and group (listp views)
	 (loop for x in (view-group-views group)
	       collect (view-window x)))))

(defun (setf selected-window) (window  &optional (interactor *interactor*))
  (with-slots (selected-window) interactor
    (let* ((previously-selected-window selected-window))
      (when previously-selected-window
	(unhighlight-window-border previously-selected-window)
	(loop with view = (top-view previously-selected-window)
	      for window in (and view (tandem-window-list view))
	      do (unhighlight-window-border window)))
      (when window
	(highlight-window-border window nil)
	(loop with view = (top-view window)
	      for window in (and view (tandem-window-list view))
	      do (highlight-window-border window t)))
      (setf selected-window window))))


(defmethod freedius-warning ((stream t) (format-string string) &rest args)
  (apply 'format stream format-string args))
