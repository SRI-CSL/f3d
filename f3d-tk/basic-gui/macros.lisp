(in-package :gui)

;;; Contains direct dependencies on OpenGL.

;;; FIXME:  This file currently contains a lot more than macros.  
;;;         The methods really should move to elsewhere, particularly since some of
;;;         the methods could be specialized on classes not defined when this file is loaded.

(defmacro map-over-active-windows ((window) &body body)
  `(map-over-active-windows-internal #'(lambda (,window) . ,body)))

(defmacro map-over-all-windows ((window) &body body)
  `(map-over-all-windows-internal #'(lambda (,window) . ,body)))


(defmacro map-over-active-views ((view) &body body)
  `(map-over-active-windows
    (window)
    (let ((,view (top-view window)))
      (when ,view . ,body))))

(defmacro map-over-all-views ((view) &body body)
  `(map-over-all-views-internal #'(lambda (,view) . ,body)))

;;; This version deosn't work with a MULTI-LVCS environment.
;; MAP-OVER-ACTIVE-WORLD-VIEWS only considers views whose 3d-worlds match the
;;; given world.  This needs to be rethought.  We do not want to redisplay of
;;; views that are totally irrelevent.  What is a realistic way to decide if a view
;;; needs a redisplay?  The "right" test would be determine if the 3d-bounding-box
;;; of the object is visible in the view.  Perhaps we should test:
;;; (EVAL-CACHE-PROBE (CACHED-CS-TO-CS-TRANSFORM-MATRIX world (3d-world  view)))

;;;(defmacro map-over-active-world-views ((world view) &body body)
;;;  `(let ((world ,world))
;;;    (map-over-active-windows
;;;     (window)
;;;     (let ((,view (top-view window)))
;;;       (when (and ,view (or (eq world (3d-world ,view))
;;;                            (eq world (2d-world ,view))))
;;;         . ,body)))))

(defmethod map-over-active-world-views-internal ((world gl-2d-world) fn)
  (map-over-active-windows (window)
    (let ((view (top-view window)))
      (when (and view (eq world (2d-world view)))
	(funcall fn view)))))

(defmethod map-over-active-world-views-internal ((world gl-3d-world) fn)
  (map-over-active-windows (window)
    (let ((view (top-view window)))
      ;(format t "map-over-active-world-views-internal ~a ~a~%" world (3d-world view))
      (when (and view (or (eq world (3d-world view))
			  (lx::EVAL-CACHE-PROBE 
			    (transforms::CACHED-CS-TO-CS-TRANSFORM-MATRIX world (3d-world view)))
			  ))
	;(format t "map-over-active-world-views-internal ~a ~a~%" world (3d-world view))
	(funcall fn view)))))

(defmacro map-over-active-world-views ((world view) &body body)
  `(map-over-active-world-views-internal ,world #'(lambda (view) .,body)))

#|
(get 'transforms::CACHED-CS-TO-CS-TRANSFORM-MATRIX :function-cache)
(cme::list-hash-table (get 'transforms::CACHED-CS-TO-CS-TRANSFORM-MATRIX :function-cache))
|#

(defmethod map-over-all-world-views-internal ((world gl-2d-world) fn)
  (map-over-all-windows (window)
    (let ((view (top-view window)))
      (when (and view (eq world (2d-world view)))
	(funcall fn view)))))

(defmethod map-over-all-world-views-internal ((world gl-3d-world) fn)
  (map-over-all-windows (window)
    (let ((view (top-view window)))
      ;(format t "map-over-active-world-views-internal ~a ~a~%" world (3d-world view))
      (when (and view (or (eq world (3d-world view))
			  (lx::EVAL-CACHE-PROBE 
			    (transforms::CACHED-CS-TO-CS-TRANSFORM-MATRIX world (3d-world view)))
			  ))
	;(format t "map-over-active-world-views-internal ~a ~a~%" world (3d-world view))
	(funcall fn view)))))

(defmacro map-over-all-world-views ((world view) &body body)
  `(map-over-all-world-views-internal ,world #'(lambda (,view) .,body)))

;;;(defmacro map-over-active-object-views ((object view) &body body)
;;;  `(let ((world (world ,object)))
;;;    (map-over-active-windows
;;;     (window)
;;;     (let ((,view (top-view window)))
;;;       (when (and ,view (or (eq world (3d-world ,view))
;;;                            (eq world (2d-world ,view))))
;;;         . ,body)))))

(defmacro map-over-active-object-views ((object view) &body body)
  `(let ((world (world ,object)))
    (map-over-active-world-views (world ,view) .,body)))

(defmacro with-3d-drawing-environment (view &body body)
  `(let* ((view ,view))
    (when (3d-to-2d-projection view)
      (unwind-protect (progn
			;;(glMakeCurrent (view-window view))
			(glPushAttrib GL_ALL_ATTRIB_BITS)
			;; save 2d to NDC transform
			(set-3d-matrices view)
			(set-default-graphics-attributes view) ; should this be here?
			. ,body
			)
	(glPopAttrib)
	))))

(defmacro with-2d-drawing-environment (view &body body)
  `(let ((view ,view))
    (unwind-protect (progn
		      ;;(glMakeCurrent (view-window view))
		      (glPushAttrib GL_ALL_ATTRIB_BITS)
		      ;; save 2d to NDC transform
		      (set-2d-matrices view)
		      (set-default-graphics-attributes view) ; should this be here?
		      . ,body
		      )
      (glPopAttrib)
      )))

(defun with-documentation-internal (string thunk)
  (let ((old-doc (get-documentation)))
    ;;(format t ";;; WITH-DOCUMENTATION old-doc = ~a~%" old-doc)
    (set-documentation string)
    (prog1 (funcall thunk)
      (set-documentation old-doc))))

(defun with-documentation-internal (string thunk)
  (let ((old-doc (get-documentation)))
    ;;(format t ";;; WITH-DOCUMENTATION old-doc = ~a~%" old-doc)
    (set-documentation string)
    (unwind-protect (funcall thunk)
      (set-documentation old-doc))))

(defmacro with-documentation ((string) &body body)
  `(with-documentation-internal ,string
    #'(lambda() .,body)))

;(set-documentation "")
;;;
;;; Useful for objects that mix 2d and 3d (correspondence objects):
;;;
(defmacro with-gl-view-transform ((view) &body body)
  `(unwind-protect
    (progn (glMatrixMode GL_PROJECTION)(glPushMatrix)
	   (gui::set-2d-matrices ,view)
	   (glMatrixMode GL_MODELVIEW) (glPushMatrix) (glLoadIdentity)
	   . ,body)
    ;; clean up
    (progn (glMatrixMode GL_MODELVIEW) (glPopMatrix) (glMatrixMode GL_PROJECTION) (glPopMatrix))))


(defmacro with-selected-result-pane ((&optional (message "Pick a pane")) &body body)
  `(let ((pane (pick-a-pane ,message)))
    (when pane
      (push-image
       (progn ,@body)
       pane))))
