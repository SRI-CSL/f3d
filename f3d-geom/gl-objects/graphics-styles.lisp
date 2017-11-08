(in-package :obj)

;;; Should the be moved to the GL package and gl-ffi subdirectory?
;;; It is really all gl drawing-context related stuff, not OBJ specific.

(declaim (special *GL-ENABLE-STIPPLES* *RADIUS-CLASS-GRAPHICS-ATTRIBUTES-ALIST*))

;;;  ***************************  MATERIAL-MODEL  ***************************

(defstruct-class material-model
    (fasd-form-init-plist-struct )
  (diffuse-color
   ambient-color
   emission-color
   specular-color
   (shininess :initform 0.0)
   )
  (:default-initform nil) :initable-slots :accessible-slots)

(define-fasd-form-init-plist material-model
    (with-class-slots material-model
	  (diffuse-color ambient-color emission-color specular-color shininess) self
      `(:diffuse-color ',diffuse-color
	:ambient-color ',ambient-color
	:emission-color ',emission-color
	:specular-color ',specular-color
	:shininess ',shininess)))

;;; Where should this live?
(defparameter *default-material-model*
  (make-instance 'material-model
		 :diffuse-color (fv .6 .6 .6 1.0)
		 :ambient-color (fv .2 .2 .2 1.0)
		 :specular-color (fv .8 .8 .8 1.0)
		 :shininess 3.0))

(defparameter *zero-color* (fv 0.0 0.0 0.0 1.0))

;;; faceid is usually GL_FRONT_AND_BACK
;;; Change name to set-material-attributes ?
(defmethod set-attributes ((object material-model) faceid)
  (with-class-slots material-model
	(diffuse-color ambient-color emission-color specular-color shininess)
      object
    (glMaterialfv faceid GL_DIFFUSE (or diffuse-color *zero-color*))
    (glMaterialfv faceid GL_AMBIENT (or ambient-color *zero-color*))
    (glMaterialfv faceid GL_EMISSION (or emission-color *zero-color*))
    (glMaterialfv faceid GL_SPECULAR (or specular-color *zero-color*))
    (glMaterialfv faceid GL_SHININESS (fv shininess))
    ))


(defmethod set-attributes ((object t) ignore)
  ;;; A catch-all method - allows stubbed-out attributes found in IO-FORM files to be tolerated.
  nil)

;;; ***********************   GL-GRAPHICS-STYLE   ***********************

;;;        Should this inherit from FASD-FORM-INIT-PLIST-STRUCT?
(defstruct-class gl-graphics-style (fasd-form-property-list-mixin)
  (dash-style
   line-width
   color
   color-vector
   stipple
   font
   material-model
   )
  (:default-initform nil) :initable-slots :accessible-slots)

(defstruct-class graphics-style (gl-graphics-style) ())

;;;
;;; Encountered a problem on MacOSX / SBCL32 (10-20-09).
;;; Some color vectors have array-element-type of T.  Not sure why.
;;; This forces all color vectors to single float on graphics-style 
;;; creation:
;;;
(defun force-float-vector (vec)
  (if (or (not (arrayp vec))
	  (subtypep (array-element-type vec) 'number))
      vec
      (let ((new (make-array (length vec) :element-type 'single-float)))
	(loop for i from 0 below (length new)
	      do (setf (aref new i) (aref vec i)))
	new)))

(defmethod (setf color) :after (color (graphics-style gl-graphics-style))
  (when color
    (let ((cv (force-float-vector (make-gl-color color))))
      (when cv
	(setf (color-vector graphics-style) cv)))))

(defmethod initialize-instance :after ((graphics-style gl-graphics-style)
				       &key color
				       &allow-other-keys)
  (when color (setf (color graphics-style) color))
  ;; Force float vectors:
  (setf (color-vector graphics-style)
	(force-float-vector (color-vector graphics-style)))
  )

;;; These OpenGL calls go into display-lists.  Be careful that they are not
;;; conditional on dynamic state.

#| from CME

line-dash-style can be:

   the list: dash-length dash-spacing

   the list of keyword-value pairs:  :DASH-OFFSET :DASH-LENGTH :DASH-SPACING


(defun make-dashed-line-dashes-array (dash-list)
  (make-array (length dash-list) :element-type '(unsigned-byte 8)
	       :initial-contents dash-list))

(defun xw::XSetDashes-from-dashlist (xdisplay xgc dash-offset dashes)
  (let ((n (min (length dashes) (length *draw-dashed-line-dash-info-array*))))
    (loop for x in dashes
	  for i from 0 below n
	  do (setf (aref *draw-dashed-line-dash-info-array* i) x))
    (xw::XSetDashes xdisplay xgc dash-offset *draw-dashed-line-dash-info-array* n)))

(defmethod set-xgc-dashed-line-mode (xdisplay xgc &key dashes dash-length (dash-spacing 4) (dash-offset 0)
					      line-width)
  (unless line-width (setq line-width *default-line-width*))
  (cond (dash-length
	 (xw::XSetLineAttributes xdisplay xgc line-width xw::LineOnOffDash 0 0)
	 (xw::XSetDashes-from-dashlist xdisplay xgc dash-offset (list dash-length dash-spacing)))
	(dashes
	 (xw::XSetLineAttributes xdisplay xgc line-width xw::LineOnOffDash 0 0)
	 (xw::XSetDashes-from-dashlist xdisplay xgc dash-offset dashes))
	(t (xw::XSetLineAttributes xdisplay xgc line-width xw::LineSolid 0 0))))

(defun set-drawing-context-line-style (context line-dash-style &optional line-width)
  ;;(format t "set-drawing-context-line-style ~a~%" line-dash-style)
  (cond ((and (consp line-dash-style) (numberp (car line-dash-style)))
	 (set-xgc-dashed-line-mode (drawing-context-xdisplay context)
				   (drawing-context-xgc context)
				   :line-width line-width
				   :dashes line-dash-style))
	(line-dash-style
	 (apply 'set-xgc-dashed-line-mode (drawing-context-xdisplay context)
		(drawing-context-xgc context)
		:line-width line-width
		line-dash-style
		))
	(t (set-xgc-dashed-line-mode (drawing-context-xdisplay context)
				     (drawing-context-xgc context)
				     :line-width line-width
				     :dash-length nil)))
  (setf (drawing-context-line-dash-style context) line-dash-style)
  )
|#


(defun-cached glLineStipplePattern (dash-style)
  (destructuring-bind (dash-length dash-spacing) dash-style
    (let* ((factor (gcd dash-length dash-spacing))
	   (dash-length (floor dash-length factor))
	   (dash-spacing (floor dash-spacing factor))
	   (dash-period (+ dash-length dash-spacing)))
      (values factor
	      (loop with pattern = 0
		    for i from 0 by dash-period below 16
		    do (setq pattern (logior (ash (1- (lx::2^ dash-length)) dash-spacing)
					     (ash pattern dash-period)))
		    finally (return (logand #xffff pattern)))))))

;(glLineStipplePattern '(4 1))
;(format nil "~16r" (nth-value 1 (glLineStipplePattern '(2 6))))
;(format nil "~16r" (nth-value 1 (glLineStipplePattern '(5 7))))
;(format nil "~16r" (nth-value 1 (glLineStipplePattern '(1 3))))

(defmethod set-gl-attributes ((graphics-style gl-graphics-style))
  #+never  (when (and *highlighting-selected-objects*)(break))
  (with-class-slots gl-graphics-style (color-vector material-model line-width stipple dash-style)
      graphics-style
    ;;(format t "set-gl-attributes color-vector= ~a~%" (list color-vector material-model))
    (when (vectorp color-vector)
      (if (subtypep (array-element-type color-vector) 'double-float)
	  (glColor4dv color-vector)
	  (glColor4fv color-vector))
	(unless (= (aref color-vector 3) 1.0)
	  (glEnable GL_BLEND)
	  ;;(format t "+")
	  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
	  ;; should this turn off stippling, so that stipple pattern isn't needed?
	  (glDisable GL_POLYGON_STIPPLE)
	  ))
    (when material-model
      (set-attributes material-model GL_FRONT_AND_BACK))
    (when line-width
      (glLineWidth (dfloat line-width))
      ;;(format t "glLineWidth = ~a~%" (list gui::*force-graphics-style* line-width (glGetInteger GL_LINE_WIDTH)))
      )
    (when dash-style
      (glEnable GL_LINE_STIPPLE)
      (mv-bind (factor pattern) (glLineStipplePattern dash-style)
	(glLineStipple factor pattern)))

    (when (and stipple *gl-enable-stipples*)
      ;; Sat Aug 9 2003: It is now the responsibility of DRAW-OBJECT method to call (glEnable
      ;; GL_POLYGON_STIPPLE). This is because with the NVidia GeForce board, lines do not get drawn
      ;; when both line anti-aliasing and polygon-stippling are enabled.  Closed-curves
      ;; need to draw both boundary lines and stipple-filled interiors.

      ;; (glEnable GL_POLYGON_STIPPLE)
      (glPolygonMode GL_FRONT GL_FILL)
      (glPolygonStipple (make-glstipple stipple)))
    ))



;;; This doesn't really belong here - nonetheless, looks like we need
;;; some way of defaulting the graphics-styles for classes of base
;;; FREEDIUS objects as well as the RADIUS classes.

#+never
(let ((line-width *default-radius-class-graphics-style-line-width*))
  (setq *freedius-class-graphics-attributes-alist*
	`((house-object :color yellow)
	  (extruded-object :color yellow)
	  )))

(defparameter *default-object-graphics-styles-ht* nil)

(defun default-object-graphics-styles-ht ()
  (or *default-object-graphics-styles-ht*
      (setq *default-object-graphics-styles-ht*
	    (let ((ht (make-hash-table )))
	      (loop for (class . graphics-attributes)
		    in (and (boundp '*radius-class-graphics-attributes-alist*)
			    *radius-class-graphics-attributes-alist*)
		    do (setf (gethash class ht)
			     (apply 'make-instance 'gl-graphics-style graphics-attributes)))
	      ht))))

(defun set-object-class-graphics-attributes (type &rest args)
  (let ((ht (default-object-graphics-styles-ht)))
    (setf (gethash type ht) (apply 'make-instance 'gl-graphics-style args))))


(defvar *global-default-object-alpha* 0.7)

(defun set-default-object-alpha (&optional (alpha *global-default-object-alpha*) only-of-type)
  (loop with ht = (default-object-graphics-styles-ht)
	for type being the hash-keys of ht
	when (or (null only-of-type) (eq only-of-type type))
	  do (let ((gs (gethash type ht)))
	       (when gs
		 (setf (aref (color-vector gs) 3) alpha))))
  (setf *global-default-object-alpha* alpha)
  )

(defun simple-merge-graphics-styles (gs1 gs2 &optional (into-gs (make-instance 'gl-graphics-style)))
  (cond ((null gs1) gs2)
	((null gs2) gs1)
	(t (with-class-slot-values gl-graphics-style
		 ((ds1 dash-style) (lw1 line-width) (c1 color) (cv1 color-vector) 
		  (s1 stipple) (f1 font)) 
	       gs1
	     (with-class-slot-values gl-graphics-style
		   ((ds2 dash-style) (lw2 line-width) (c2 color) (cv2 color-vector)
		    (s2 stipple) (f2 font))
		 gs2
	       (with-class-slots gl-graphics-style 
		   (dash-style line-width color color-vector stipple font) 
		   into-gs
		 (setf dash-style  (or ds1 ds2) 
		       line-width (or lw1 lw2)
		       stipple (or s1 s2)
		       font (or f1 f2)
		       color-vector (or cv1 cv2)
		       color (cond (cv1 c1) (cv2 c2) (t (or c1 c2))))
		 into-gs))))))
