(in-package :gui)

#|
Support for the OpenGL lighting model.

Light Sources:
  basic-light-source infinite-light-source point-light-source spot-light-source

set-shading-attributes

set-default-graphics-attributes

set-lighting

enable-anti-aliasing

|#

#|


The following represents the entire lighting calculation in RGBA mode.

vertex color = emission[material]   + 

               ambient[light model] * ambient[material] +

   sum(i) spotlight-effect[i] * 

        ( ambient[light,i] * ambient[material] +

          (max l.n  0) * diffuse[light,i] * diffuse[material] +

          (max s.n 0)^shininess[material,i] * specular[light,i] * specular[material]) 

where l.n is dot product of light source direction with surface normal
      s.n is dot product of specular direction with surface normal.

Specular direction is computed as follows: 

   The sum of the two unit vectors that point between (1) the vertex and the
   light position and (2) the vertex and the viewpoint (assuming that
   GL_LIGHT_MODEL_LOCAL_VIEWER is true; if it's not true, the vector (0, 0, 1)
   is used as the second vector in the sum). This vector sum is normalized.  


NOTE:  If GL_LIGHT_MODEL_LOCAL_VIEWER is false, then we do not need to compute
       the vector to the viewpoint (eye direction).

       If the material (or light) specular component is zero, the we do not need 
       to compute the eye direction or specular direction.

       If we have an infinite light source, then we do not need to compute
       the vector to the light.

       If eye direction and light direction are both constants, then the 
       specular direction is also constant.

|#

;;;  ***************************  LIGHT-SOURCES  ***************************

(defstruct-class basic-light-source
    (fasd-form-init-plist-struct )
  ((ambient-color :initarg :ambient-color)
   (diffuse-color  :initarg :diffuse-color)
   (specular-color :initarg :specular-color)
   )
  (:default-initform nil) :initable-slots :accessible-slots)

(define-fasd-form-init-plist basic-light-source
    (with-class-slots basic-light-source (ambient-color diffuse-color specular-color)
	self
      `(:ambient-color ',ambient-color
	:diffuse-color ',diffuse-color
	:specular-color ',specular-color)))

(defstruct-class infinite-light-source (basic-light-source)
     ((direction :initform nil :initarg :direction))
     :initable-slots :accessible-slots)

(define-fasd-form-init-plist infinite-light-source
    (with-class-slots infinite-light-source (direction)
	self
      `(:direction ',direction)))

(defstruct-class point-light-source (basic-light-source )
     ((position :initform nil :initarg :position :accessor light-position)
      (attenuation-vector :initform nil :initarg :attenuation-vector)
      )
     :initable-slots :accessible-slots)

(define-fasd-form-init-plist point-light-source
    (with-class-slots point-light-source (position attenuation-vector)
	self
      `(:position ',position
	:attenuation-vector ',attenuation-vector)))

(defstruct-class spot-light-source (point-light-source )
     ((spot-direction :initform nil :initarg :spot-direction)
      (spot-exponent :initform 0.0 :initarg :spot-exponent)
      (spot-cutoff :initform 180.0 :initarg :spot-cutoff)
      cos-spot-cutoff
      )
     :initable-slots :accessible-slots)

(define-fasd-form-init-plist spot-light-source
    (with-class-slots spot-light-source (spot-direction spot-exponent spot-cutoff)
	self
      `(:spot-direction ',spot-direction
	:spot-exponent ',spot-exponent
	:spot-cutoff ',spot-cutoff)))

(defparameter *default-light-source*
  (make-instance 'basic-light-source
		 :diffuse-color (fv .6 .6 .6 1.0)
		 :ambient-color (fv .1 .1 .1 1.0)
		 :specular-color (fv .8 .8 .8 1.0)))

(defparameter *zero-color* (fv 0.0 0.0 0.0 1.0))

#|
(setq *gl-shading-enabled* t)
(setq *gl-shading-enabled* nil)
(setq *gl-shading-always-enabled* t)
(setq *gl-shading-always-enabled* nil)
(setf (shading-enabled (display-attributes (top-view))) t)
(setf (shading-enabled (display-attributes (top-view))) nil)
(describe (display-attributes (top-view)))
(describe (DEFAULT-LIGHT-SOURCE (display-attributes (top-view))))
(setf (ambient-color (DEFAULT-LIGHT-SOURCE (display-attributes (top-view))))
      (fv .3 .3 .3 1.0))
(setq tower (selected-object))
(setf (graphics-style tower)
      (make-instance 'gl-graphics-style
		     :color-vector (fv 0.0 1.0 1.0 1.0)
		     :material-model
		     (make-instance 'material-model
				    :diffuse-color (fv 0.0 1.0 0.0 .6)
				    :ambient-color (fv 0.0 1.0 1.0 .6))))

(graphics-style tower)
(object-graphics-style tower (top-view))
(sun-vector (top-view))
(describe (top-view))
(setf (get-prop (top-view) :sun-vector) (cv 0.6082805 -0.1486257 0.7796828 0.0))

(glGetFloat GL_DEPTH_SCALE)
(glGetFloat GL_DEPTH_BIAS)
|#


;;; lightid is usually GL_LIGHT0
(defmethod set-attributes ((object basic-light-source) lightid)
  (with-class-slots basic-light-source
	(diffuse-color ambient-color emission-color specular-color)
      object
    (glLightfv lightid GL_DIFFUSE (or diffuse-color *zero-color*))
    (glLightfv lightid GL_AMBIENT (or ambient-color *zero-color*))
    (glLightfv lightid GL_SPECULAR (or specular-color *zero-color*))
    (glEnable GL_LIGHTING)
    (glEnable lightid)
    ))


;;; These OpenGL calls DO NOT GO INTO DISPLAY-LISTS
;;; They are part of the per view redisplay setup.
;;; Must be careful about backing-store interactions.
(defmethod set-shading-attributes ((attributes view-display-attributes) view)
  (with-class-slots view-display-attributes
    (default-material default-light-source) attributes
    (let* ((sun-vector (sun-vector view))
	   (*zero-color* (fv 0.0 0.0 0.0 1.0))
	   )
;      (glEnable GL_DEPTH_TEST) ; z-buffering and shading should be orthogonal 
      (glPolygonMode GL_FRONT GL_FILL)
      (glPolygonMode GL_BACK GL_FILL)
      ;; glShadeModel needs to be set on an object-by-object basis
      ;; The default should be flat.
      (glShadeModel GL_FLAT) ;; (glShadeModel GL_SMOOTH) 
      (when (and default-light-source (set-3d-matrices view))
	;;(format t "Setting light source for sun-vector = ~a~%" sun-vector)
	(set-attributes default-light-source GL_LIGHT0)
	;; The modularity looks wrong here -- why is glLightfv done here rather 
	;; than as part of set-attributes?
	(glLightfv GL_LIGHT0 GL_POSITION
		   (or (math::bind-vector-elements (sunx suny sunz) sun-vector
			 (fv sunx suny sunz 0.0))
		       (fv  0.0 0.0 1.0 0.0))) ; infinite light-source directly overhead
	)

      (when default-material
	(set-attributes default-material GL_FRONT_AND_BACK)
	))))

;;; ************************  FALLBACK GL PARAMETER SETTINGS  ************************

;;; no current callers
;;;(defmethod set-lighting ((view view))
;;;  (when view
;;;    (let* ((sun-vector (sun-vector view))
;;;           ;;(amb-mat 0.2) (diff-mat .9)(spec-mat .8)
;;;           ;;(amb-lite .1) (diff-lite .9)(spec-lite 0.0)
;;;           (amb-mat 0.2) (diff-mat .6)(spec-mat .8)
;;;           (amb-lite .1) (diff-lite .6)(spec-lite .8)
;;;           )
;;;;      (glEnable GL_DEPTH_TEST) ; z-buffering and lighting should be orthogonal 
;;;      (glPolygonMode GL_FRONT GL_FILL)
;;;      (glPolygonMode GL_BACK GL_FILL)
;;;      (glLightfv GL_LIGHT0 GL_DIFFUSE (fv diff-lite diff-lite diff-lite 1.0))
;;;      (glLightfv GL_LIGHT0 GL_SPECULAR (fv spec-lite spec-lite spec-lite 1.0))
;;;      (glLightfv GL_LIGHT0 GL_AMBIENT (fv amb-lite amb-lite amb-lite 1.0))
;;;      (glLightfv GL_LIGHT0 GL_POSITION
;;;                 (or (math::bind-vector-elements (sunx suny sunz) sun-vector
;;;                       (fv sunx suny sunz 0.0))
;;;                     (fv  0.0 0.0 1.0 0.0)))
;;;      ;;(glLightfv GL_LIGHT0 GL_SPOT_DIRECTION (fv 0.0 0.0 -1.0))
;;;      (glEnable GL_LIGHTING)
;;;      (glEnable GL_LIGHT0)
;;;      (glShadeModel GL_SMOOTH)
;;;      (glMaterialfv GL_FRONT_AND_BACK GL_SPECULAR
;;;                    (fv spec-mat spec-mat spec-mat 1.0))
;;;      (glMaterialfv GL_FRONT_AND_BACK GL_AMBIENT
;;;                    (fv amb-mat amb-mat amb-mat 1.0))
;;;      (glMaterialfv GL_FRONT_AND_BACK GL_DIFFUSE
;;;                    (fv diff-mat diff-mat diff-mat 1.0))
;;;      ;;(glMaterialfv GL_FRONT_AND_BACK GL_SHININESS (fv 40.0))
;;;      (glMaterialfv GL_FRONT_AND_BACK GL_SHININESS (fv 3.0))
;;;      ;;(glEnable GL_COLOR_MATERIAL)
;;;      ;;(glEnable GL_NORMALIZE) ; no longer needed -- vertices are now scaled
;;;      ;;(let ((grey .7)) (glColor3d grey grey grey))
;;;
;;;
;;;            (glColor3d .7 .5 .6)
;;;      )))
#|
(glDisable GL_COLOR_MATERIAL)
|#

;;; THIS STUFF HAS NOTHING TO DO WITH LIGHTING - MOVE TO A DIFFERENT FILE

;;; This doesn't work if *gl-enable-stipples* = T.
(defun enable-anti-aliasing (flag)
  (cond (flag
	 (glEnable GL_BLEND)
	 (glEnable GL_LINE_SMOOTH)
	 (glHint GL_LINE_SMOOTH_HINT GL_DONT_CARE)
	 ;;(glHint GL_LINE_SMOOTH_HINT GL_NICEST)
	 (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
	 ;;(glBlendFunc GL_ONE GL_ZERO)
	 ;;(glBlendFunc GL_SRC_ALPHA GL_ONE)
	 )
	(t
	 (glDisable GL_BLEND)
	 (glDisable GL_LINE_SMOOTH)
	 )))

;;;(defun enable-anti-aliasing (flag)
;;;  (cond (flag
;;;         (glEnable GL_LINE_SMOOTH)
;;;        ;; (glHint GL_LINE_SMOOTH_HINT GL_DONT_CARE)
;;;          (glHint GL_LINE_SMOOTH_HINT GL_NICEST)
;;;         )
;;;        (t
;;;         (glDisable GL_BLEND)
;;;         (glDisable GL_LINE_SMOOTH)
;;;         )))
#|
(describe (display-attributes (top-view)))
(setf (anti-alias-lines (display-attributes (top-view))) t)
(setf (anti-alias-lines (display-attributes (top-view))) nil)
(describe (object-graphics-style  (caar (selected-objects)) (top-view)))
(setf (obj::line-width (object-graphics-style (caar (selected-objects)) (top-view))) 2)

(gl-state '())
(setq *gl-enable-stipples* nil)


|#

(defparameter *gl-enable-stipples* t)

;;; The modularity of this appears to be wrong --
;;; Might want to separate 2d and 3d attributes, since setting the shading
;;; attributes is really only meaningful for 3d-objects and requires setting
;;; the gl matrices.
;;; THIS HAS NOTHING TO DO WITH LIGHTING - MOVE THIS TO A DIFFERENT FILE
(defmethod set-default-graphics-attributes ((view basic-view))
  (let* ((attributes (display-attributes view)))
    (glColor4d 1.0d0 1.0d0 0.0d0 0.5d0)  ; yellow 
    (glPolygonMode GL_FRONT GL_LINE)
    (if t
	(glPolygonMode GL_BACK GL_FILL) ; what is this for? - perhaps for glCullFace GL_BACK?
	(glPolygonMode GL_BACK GL_LINE))
    (glFrontFace GL_CCW)
    (glCullFace GL_BACK) ; this doesn't work for lines, only for fill mode
    (glEnable GL_CULL_FACE)
    ;;(glDisable GL_CULL_FACE)
    (if (and *enable-depth-test* 
	     (get-prop (3d-to-2d-projection view) :near-far))
	(glEnable GL_DEPTH_TEST)
	(glDisable GL_DEPTH_TEST))
    ;;(glEnable GL_BLEND)
    ;;(glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)

    (glDisable GL_LIGHTING)
    (glDisable GL_POLYGON_STIPPLE)

    #+never
    (format t "set-default-graphics-attributes ~a ~a~%" 
	    view (and *enable-depth-test* (get-prop (3d-to-2d-projection view) :near-far)))
  
    (enable-anti-aliasing (anti-alias-lines attributes))))


(defmethod set-default-graphics-attributes :after ((view view))
  (let* ((attributes (display-attributes view)))
    (unless (glIsEnabled GL_VERTEX_ARRAY)
      (glEnableClientState GL_VERTEX_ARRAY))
    (if (and *gl-shading-enabled* (shading-enabled attributes))
	(progn
	  ;;(glEnable GL_BLEND) (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
	  ;; (when *highlighting-selected-objects* (break))
	  (set-shading-attributes attributes view))
	
	(when *gl-enable-stipples* ;(and *gl-enable-stipples* (not enable-anti-aliasing))
	  ;; This allows display-lists to support both lighting and wire-frames.
	  ;; A stipple of all zeros disables the shaded rendering of faces.
	  ;;(glEnable GL_POLYGON_STIPPLE)
	  ;; By default, polygon fill isn't visible unless object specifies
	  ;; a non-zero stipple pattern, or a color-alpha < 1.0
	  ;; LHQ Jul  1 2003:  When shaded rendering is enabled, this causes
	  ;; 3d-curve objects to be filled with black.  Not good.
	  ;;(glPolygonMode GL_FRONT GL_FILL)
	  (glPolygonStipple
	   (if t
	       (make-glstipple '((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0)))
	       (make-glstipple '((1 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0))))
	   ))
	)))


#|
(setq *anti-alias-lines* t)
(setq *gl-shading-enabled* t)
(setq *gl-shading-enabled* nil)
(setq *enable-depth-test* t)
(setq *gl-enable-stipples* nil)
(setq *gl-enable-stipples* t)
(top-view)
(time (redisplay (top-view) :update-backing-store nil))
;; .05 no stipples
(time (redisplay (top-view) :update-backing-store nil))
;; .06 with stipples 


(setq img (view-image (top-view)))
(setf (view-image (top-view)) nil)
(setf (view-image (top-view)) img)

(symbol-package '*gl-shading-enabled*)

|#



;;;(defmethod set-object-drawing-parameters ((view view))
;;;  (glColor3d 1.0 1.0 0.0)
;;;  (glPolygonMode GL_FRONT GL_LINE)
;;;  (glPolygonMode GL_BACK GL_LINE)
;;;  ;;(glFrontFace GL_CW)
;;;  (glFrontFace GL_CCW)
;;;  (glCullFace GL_BACK)
;;;  (glEnable GL_CULL_FACE)
;;;  (if *enable-depth-test*
;;;      (glEnable GL_DEPTH_TEST)
;;;      (glDisable GL_DEPTH_TEST))
;;;  (enable-anti-aliasing *anti-alias-lines*)
;;;  (glEnableClientState GL_VERTEX_ARRAY)
;;;  (if *gl-shading-enabled*
;;;      (set-lighting view)
;;;      (progn
;;;        (glEnable GL_POLYGON_STIPPLE)
;;;        ;; By default, polyg *gl-shading-enabled* on fill isn't visible unless object specifies
;;;        ;; a non-zero stipple pattern, or a color-alpha < 1.0
;;;        ;;(glPolygonMode GL_FRONT GL_FILL)
;;;        (glPolygonStipple
;;;         (make-glstipple '((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0)))
;;;         ;;(make-glstipple '((1 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0)))
;;;         ))
;;;      ))



