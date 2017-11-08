(in-package :transforms)


;;; unused
;;;(defun geoid-name-from-lat-long-to-geocentric-transform (lat-long-to-geocentric-transform)
;;;  (or (car (rassoc lat-long-to-geocentric-transform 
;;;                   transforms::*geoid-lat-long-to-geocentric-transform-alist*))
;;;      (error "GEOID-NAME-FROM-LAT-LONG-TO-GEOCENTRIC-TRANSFORM cannot find transform: ~a" 
;;;             lat-long-to-geocentric-transform)))

(defmethod lat-long-to-geocentric-transform ((object 3d-world))
  (let* ((lvcs-to-geocentric-transform (object-to-parent-transform object))
	 (geocentric-coordinate-system (to-coordinate-system lvcs-to-geocentric-transform))
	 (ellipsoid (transforms::ellipsoid geocentric-coordinate-system))
	 (lat-long-to-geocentric-transform (inverse-transform (transforms::gcc-to-gdc-transform ellipsoid))))
    lat-long-to-geocentric-transform))


  
(in-package :freedius-io)

;;; This should be renamed from io-attributes to io-key-vals
(defgeneric io-attributes (object))

(defmethod io-attributes ((object t))
  nil)

(defmacro define-io-attributes (class attribute-list)  
  (let ((object-symbol (intern "OBJECT" *package*)))
    ;;(format t "define-io-attributes ~a~%" (symbol-package object-symbol))
    ;; slime bug: debugging/macroexpand doesn't bind package correctly
    `(defmethod io-attributes :around ((,object-symbol ,class))
      ;; These attributes must precede less specific attributes in case of
      ;; we overriding one or more attributes from a less specific class.
      (append (call-next-method) ,attribute-list))))


;;; This really belongs in a different file

(defparameter *almost-zero* 1e-8)

(defun almost-zerop (x)
  (< (abs x) *almost-zero*))
   
;;; This isn't an IO-FORM method because the object-to-parent-transform keyvals are inlined
;;; in the io-form of basic-gl-object.  
(defmethod object-to-parent-transform-keyvals ((object-to-parent-rotation-matrix array)
					       &optional (Euler-angle-system :z-y-x-rotation))
  `(:origin (,(aref object-to-parent-rotation-matrix 0 3)
	     ,(aref object-to-parent-rotation-matrix 1 3)
	     ,(aref object-to-parent-rotation-matrix 2 3))
    ,@(math::Euler-angle-keyvals Euler-angle-system object-to-parent-rotation-matrix)))

(defmethod object-to-parent-transform-keyvals ((object-to-parent-transform
						transforms::4x4-coordinate-transform)
					       &optional (Euler-angle-system :z-y-x-rotation))
  (object-to-parent-transform-keyvals (transforms::transform-matrix object-to-parent-transform)
				      Euler-angle-system))


;;;(defmethod io-form ((lvcs local-vertical-coordinate-system))
;;;  (let* ((name (name lvcs))
;;;         (name-spec (when name `(:name ,name)))
;;;         (units (car (get-prop lvcs :component-units)))
;;;         (units-spec (when (and units (not (eq units 'meters))) `(:units ,units)))
;;;         )
;;;  (if (parent lvcs)
;;;      (bind-vector-elements (long lat elev)
;;;          (transform-vector (to-lat-long-transform lvcs) (cv 0.0 0.0 0.0))
;;;        `(lvcs (gdc ,(name (ellipsoid (parent lvcs)))) 
;;;                     ,lat ,long 
;;;                     ,@(when (> (abs elev) 1e-5) `(:elev ,elev))
;;;                     ,@name-spec ,@units-spec))
;;;
;;;      `(lvcs ,@name-spec ,@units-spec))))

;;;(defmethod io-form ((lvcs local-vertical-coordinate-system))
;;;  (let* ((name (name lvcs))
;;;         (name-spec (when name `(:name ,name)))
;;;         (units (car (get-prop lvcs :component-units)))
;;;         (units-spec (when (and units (not (eq units 'meters))) `(:units ,units)))
;;;         )
;;;  (if (parent lvcs)
;;;      (bind-vector-elements (long lat elev)
;;;          (transform-vector (to-lat-long-transform lvcs) (cv 0.0 0.0 0.0))
;;;        `(lvcs (gdc ,(name (ellipsoid (parent lvcs)))) 
;;;          :lat ,lat :long  ,long 
;;;          ,@(when (> (abs elev) 1e-5) `(:origin-elev ,elev))
;;;          ,@units-spec
;;;          ,@name-spec
;;;          ))
;;;
;;;      `(lvcs ,@name-spec ,@units-spec))))

(defmethod io-form ((lvcs local-vertical-coordinate-system))
  (let* ((name (name lvcs))
	 (name-spec (when name `(:name ,name)))
	 (units (car (get-prop lvcs :component-units)))
	 (units-spec (when (and units (not (eq units 'meters))) `(:units ,units)))
	 )
  (if (parent lvcs)
      (bind-vector-elements (long lat elev)
	  (transform-vector (to-lat-long-transform lvcs) (cv 0.0 0.0 0.0))
	`(lvcs (gdc ,(name (ellipsoid (parent lvcs)))) 
	  :lat ,lat :long  ,long 
	  ,@(when (> (abs elev) 1e-5) `(:origin-elev ,elev))
	  ,@units-spec
	  ,@name-spec
	  ))

      `(lvcs ,@name-spec ,@units-spec))))


(defmethod io-form ((frame-camera transforms::frame-camera))
  (mv-bind (camera-to-2d-matrix 3d-to-camera-matrix)
      (transforms::interior-and-exterior-matrices frame-camera)
    `(make-frame-camera 
      ,@(object-to-parent-transform-keyvals 
	 (math::invert-matrix 3d-to-camera-matrix)
	 ;; Need some smarts to automagically pick an Euler-angle-system depending
	 ;; on the camera-to-3d-matrix
	 (or (get-prop frame-camera :euler-angle-system) :omega-phi-kappa) )
      ,@(transforms::decompose-camera-to-2d-matrix camera-to-2d-matrix)
      )))

(defmethod io-form ((projection transforms::4x4-projection))
  (mv-bind (camera-to-2d-matrix 3d-to-camera-matrix)
      (transforms::decompose-projection-matrix (transforms::projection-matrix projection))
    `(make-frame-camera 
      ,@(object-to-parent-transform-keyvals 
	 (math::invert-matrix 3d-to-camera-matrix)
	 ;; Need some smarts to automagically pick an Euler-angle-system depending
	 ;; on the camera-to-3d-matrix
	 (or (get-prop projection :euler-angle-system) :omega-phi-kappa) )
      ,@(transforms::decompose-camera-to-2d-matrix camera-to-2d-matrix)
      )))
#|
(setq *frame-camera* 
      (FREEDIUS-MODELS-V0::FRAME-CAMERA 
       :PARENT (LVCS (GDC NAD27) :lat 39.50586158474837 :long -105.11441597675547 )
       :ORIGIN (0.0 0.0 6619.4059412861925)
       :OMEGA-PHI-KAPPA (-2.004457029560062 1.185066214677839 0.8097247922823747)
       :PRINCIPAL-POINT (1665.6791 1986.6626)
       :FOCAL-LENGTH -5067.3154
       :SKEW -0.0083902 :ASPECT-RATIO 1.0000105
       :NEAR-FAR (10000.0)))

(mv-bind (w2c params c2d) 
    (transforms::decompose-projection-matrix (transforms::projection-matrix
					      (gui::3d-to-2d-projection (gui::top-view))))
  (list (object-to-parent-transform-keyvals (math::invert-matrix w2c):omega-phi-kappa)
	params c2d))
					     

(setq *frame-camera-2d* 
      (FREEDIUS-MODELS-V0::FRAME-CAMERA 
       :PARENT (LVCS (GDC NAD27) :lat 39.50586158474837 :long -105.11441597675547 )
       :ORIGIN (1812.722 2137.357 21716.98)
       :OMEGA-PHI-KAPPA (1.9821771354116573 -1.2074598729393518 -0.7640449988313294)
       :PRINCIPAL-POINT (1665.6791166043886 1986.6625966352037)
       :FOCAL-LENGTH -5067.263398584347
       :SKEW -0.008390307459739016 :ASPECT-RATIO 1.000020863860185
       :NEAR-FAR ( 10000.0))
      *frame-camera* (gui::3d-to-2d-projection *frame-camera-2d*))

(setq *frame-camera-2d*
      (FREEDIUS-MODELS-V0::FRAME-CAMERA 
       :PARENT (LVCS (GDC NAD27) :lat 39.50586158474837 :long -105.11441597675547 )
       :ORIGIN (0.0 0.0 6619.4059412861925)
       :OMEGA-PHI-KAPPA (1.9879562123062011 -1.2125348719022815 -0.776326693689356)
       :PRINCIPAL-POINT (1665.6791 1986.6626)
       :FOCAL-LENGTH -5067.3154
       :SKEW -0.0083902 :ASPECT-RATIO 1.0000105)
      *frame-camera* (gui::3d-to-2d-projection *frame-camera-2d*))

(describe (transforms::3d-to-camera-matrix *frame-camera*))

(setq *frame-camera*
      (transforms::transform-to-camera-centered-lvcs 
       (gui::3d-to-2d-projection (gui::top-view)) 
       ;:local-units-per-meter *feet-per-meter* 
       ))

(io-form *frame-camera*)

(let ((pt (gui::selected-object-world-position)))
  (list (transform-vector *frame-camera* pt)
	(transform-vector (gui::3d-to-2d-projection (gui::top-view)) pt)))

(let* ((p0 (gui::3d-to-2d-projection (gui::top-view)))
       (p1 *frame-camera*)
       (pt0 (gui::selected-object-world-position))
       (pt1 (transform-vector 
	     (transforms::transform-path (from-coordinate-system p0)
					 (from-coordinate-system p1))
	     pt0))
       )
  (list (transform-vector p0 pt0)			  
	(transform-vector p1 pt1)
	pt0 pt1))


(transforms::transform-projection-matrix-to-camera-centered-lvcs 
 (gui::3d-to-2d-projection (gui::top-view)) 
 :local-units-per-meter *feet-per-meter* 
 )


(transforms::foo (gui::3d-to-2d-projection (gui::top-view)))

;;; agrees to within 1e-14  -- looks essentially perfect

;(io-form (gui::3d-to-2d-projection (gui::top-view)))

;(io-form (gui::3d-world (gui::top-view)))
|#
		    
(define-io-attributes feature-set
    `(:name ,(name object) ))
	
(defmethod io-form ((object feature-set))
  `(with-feature-set 
    ,(io-attributes object)
    ,@(loop for obj in (children object)
		       collect (io-form obj))))

(defmethod io-form ((object forward-referenced-object)) 
  nil)

(declaim (special *default-parent*))

#+old
(define-io-attributes obj::basic-gl-object
    (let ((name (obj::name object))
	  (attributes (obj::attributes object))
	  (graphics-style (obj::graphics-style object))
	  (children (children object)))
      `(,@(when name `(:name ,name))
	,@(when attributes `(:attributes ,attributes))
	,@(when graphics-style `(:graphics-style ,(io-attributes graphics-style)))
	,@(unless (eq (parent object) *default-parent*)
	    (if (typep (parent object) 'obj::basic-gl-object)
		;; we apparently have a malformed composite hierarchy
                ;; which is almost always going to be true at present.
		(error "(parent  object) ~a differs from *default-parent* ~a"
		       (parent object) *default-parent*)
		;; otherwise we should have an lvcs or 2d-world
		`(:parent ,(io-form (parent object)))))
	,@(object-to-parent-transform-keyvals (object-to-parent-transform object))
	,@(when children 
		(let ((*default-parent* object))
                  (print *default-parent*)
		  `(:children ,(loop for child in children collect (io-form child)))))
	)))


(define-io-attributes obj::basic-gl-object
    (let ((name (obj::name object))
          (parent (parent object))
	  (attributes (obj::attributes object))
	  (graphics-style (obj::graphics-style object))
	  (children (children object)))
      (when (or (not (typep parent 'OBJ::COMPOSITE-OBJECT))
                (eq parent *default-parent*))
      `(,@(when name `(:name ,name))
          ,@(when attributes `(:attributes ,attributes))
          ,@(when graphics-style `(:graphics-style ,(io-attributes graphics-style)))
          ,@(unless (eq (parent object) *default-parent*)
                    (if (typep (parent object) 'obj::basic-gl-object)
                        ;; we apparently have a malformed composite hierarchy
                        ;; which is almost always going to be true at present.
                        (format t  "(parent  object) ~a differs from *default-parent* ~a"
                               (parent object) *default-parent*)
                        ;; otherwise we should have an lvcs or 2d-world
                        `(:parent ,(io-form (parent object)))))
          ,@(object-to-parent-transform-keyvals (object-to-parent-transform object))
          ,@(when children 
                  (let ((*default-parent* object))
                    `(:children ,(loop for child in children collect (io-form child)))))
          ))))
   
(defmethod io-form ((object obj::basic-gl-object))
  (let ((parent (parent object)))
    (when (or (not (typep parent 'OBJ::COMPOSITE-OBJECT))
              (eq parent *default-parent*))
      `(create-object ,(class-name (class-of object)) .,(io-attributes object)))))

#+unfinished
(defmethod io-form ((2d-world obj::gl-2d-world))
  `(with-2d-world (,(name 2d-world) :3d-world ,(io-form (3d-world 2d-world)))))

;;(pcl::undefmethod io-form (obj::gl-object))

(define-io-attributes obj::gl-xyz-sizable-object-mixin
    (bind-vector-elements (x-size y-size z-size) (obj::sizes object)
      `(:sizes (,x-size ,y-size ,z-size))))

(defun list-vertex-array (array)
  (listarray array))


(define-io-attributes obj::extruded-object
    `(:top-vertices ,(list-vertex-array (obj::extrusion-top-vertices object))))

(define-io-attributes obj::basic-curve
    `(:vertices ,(list-vertex-array (obj::%vertex-array object))))

(define-io-attributes obj::house-object
    `(:roof-pitch ,(obj::roof-pitch object)
      :roof-type ,(obj::roof-type object)
      :roof-overhang ,(obj::roof-overhang object)
      :z-size ,(aref (obj::sizes object) 2)))

;;;
;;; Modified this to expand attributes using IO-FORM - some attributes will be expandable instances:
;;;
(defmacro conditional-io-attribute-initargs (&rest names)
  `(let (initargs)
    ,@(loop for name in names
	    collect `(when ,name #+never (print ,name) (push (io-form ,name) initargs) (push ,(lx::make-keyword name) initargs)))
    initargs))

(defun gl-color-io-form (color)
  (cond ((or (symbolp color) (stringp color))
	 (lx::make-keyword color))
	((consp color) color)
	((vectorp color)
	 (loop with precision = 1e-4
	       for i from 0 below (length color)
	       collect (round-to-multiple (elt color i) precision)))))
      
;(gl-color-io-form '(0.2 0.4 .5))
;(gl-color-io-form (fv 0.2 0.4 .5))
;(describe (gl::make-gl-color '(0.2 0.4 .5)))
;(gl-color-io-form (gl::make-gl-color '(0.2 0.4 .5)))
;(describe (gl::make-gl-color :red))

;;;(define-io-attributes obj::gl-graphics-style
;;;    (with-class-slots obj::gl-graphics-style
;;;          (color line-width dash-style stipple font material-model) object
;;;      `(,@(when color `(:color ,(gl-color-io-form color)))
;;;        ,@(when line-width `(:line-width ,line-width))
;;;        ,@(when dash-style `(:dash-style ,dash-style))
;;;        ,@(when stipple `(:stipple ,stipple))
;;;        ,@(when font `(:font ,font))
;;;        ,@(when material-model `(:material-model ,material-model))
;;;      )))

#+never ; slot package problems
(define-io-attributes obj::gl-graphics-style
    (with-class-slots obj::gl-graphics-style
	  (color line-width dash-style stipple font material-model) object
      (conditional-io-attribute-initargs color line-width dash-style stipple font material-model)))



(in-package :obj)

(freedius-io::define-io-attributes material-model
    (with-class-slots material-model (diffuse-color ambient-color emission-color specular-color shininess)
	object
      `(,@(when diffuse-color `(:diffuse-color ,diffuse-color))
	,@(when ambient-color `(:ambient-color ,ambient-color))
	,@(when emission-color `(:emission-color ,emission-color))
	,@(when specular-color `(:specular-color ,specular-color))
	,@(when shininess `(:shininess ,shininess))
	)))

(freedius-io::define-io-attributes gl-graphics-style
    (with-class-slots gl-graphics-style
	  (color color-vector line-width dash-style stipple font material-model) object
      (freedius-io::conditional-io-attribute-initargs color color-vector line-width dash-style stipple font material-model)))



(freedius-io::define-io-attributes time-interval
    (let ((begin (begin-time object))
	  (end (end-time object)))
      `(,@(when begin `(:begin-time (unix-to-local-timestamp ,(transforms::canonical-timestamp object begin))))
	,@(when end `(:end-time (unix-to-local-timestamp ,(transforms::canonical-timestamp object end)))))))


(in-package :freedius-io)


(defmethod io-form ((obj number)) obj)

(defmethod io-form ((obj symbol)) obj)

(defmethod io-form ((obj string)) obj)

(defmethod io-form ((obj pathname)) obj)

(defmethod io-form ((obj vector)) obj)

(defmethod io-form ((obj t))
  (format t "~%default io-form for ~a." obj)
  (list (intern (symbol-name (type-of obj)) :freedius-models-v0)
	(format nil "~a" obj)))



(defmethod io-form ((obj list))
  (loop for x in obj collect (io-form x)))
  
#+never
(defmethod io-form ((object obj::material-model))
  `(create-object ,(class-name (class-of object)) .,(io-attributes object)))

(eval-when (eval load compile)
(import 'obj::material-model *model-file-package*)
)

;;; This will create a copy of the material-model at every reference -
;;; probably not a big deal, but be forewarned...

(defmethod io-form ((object obj::material-model))
  `(create-object freedius-models-v0::material-model .,(io-attributes object)))


(freedius-io::define-io-attributes obj::conjugate-point-object
    `(:2d-point-list ,(loop for (2d-world . coords) in (obj::2d-point-list object)
                            when 2d-world
                             collect (cons (name 2d-world)
                                           (multiple-value-list
                                            (coordinate-vector-elements coords))))))


#|
(fmakunbound 'io-attributes)
(fmakunbound 'io-form)

(io-form (gui::selected-object))

(setq *obj*
      (CREATE-OBJECT SMC::RADIUS-PIPELINE
               :NAME
               "Pipeline-46"
               :ORIGIN
               (-209.985 4623.183 6059.475)
               :ATTRIBUTES
               (:DESCRIPTION "Pipeline xx yy zz")
               :GRAPHICS-STYLE
               (:STIPPLE ((0 1 0 0) (0 0 1 0) (0 0 0 0) (0 0 0 0)) :DASH-STYLE
			 (6 6) :LINE-WIDTH 4 :COLOR RED)
               :VERTICES
               ((0.0 0.0 0.0) (201.179 167.508 -9.364) (444.505 326.169 -4.261)
                (767.46 313.506 47.895))))

(setq *obj*
      (CREATE-OBJECT RADIUS-STREET
		     :NAME
		     "Street-13"
		     :ORIGIN
		     (405.39 4190.218 6012.574)
		     :GRAPHICS-STYLE
		     (:STIPPLE ((0 1 0 0) (0 0 1 0) (0 0 0 0) (0 0 0 0)) :DASH-STYLE
			       (6 6) :LINE-WIDTH 2 :COLOR RED)
		     :VERTICES
		     ((0.0 0.0 0.0 94.966) (-150.654 -71.521 -12.381 22.966)
		      (-679.397 -313.293 32.834 22.966)
		      (-734.519 -300.453 34.86 22.966)
		      (-782.362 -238.602 41.671 22.966)
		      (-860.007 -127.282 52.276 22.966)
		      (-1041.863 -21.41 65.361 22.966)
		      (-1087.582 76.02 82.398 22.966)
		      (-1074.802 162.794 81.138 22.966)
		      (-826.487 508.201 76.575 22.966)
		      (-599.149 941.477 41.687 22.966))))

(setq *obj*
 (CREATE-OBJECT SMC::RADIUS-PIPELINE
     :NAME           "Pipeline-46"
     :ORIGIN         (-209.985 4623.183 6059.475)
     :ATTRIBUTES     (:DESCRIPTION "Pipeline xx yy zz")
     :GRAPHICS-STYLE (:COLOR      :RED
                      :LINE-WIDTH 4
                      :DASH-STYLE (6 6)
                      :STIPPLE    ((0 1 0 0) (0 0 1 0) (0 0 0 0) (0 0 0 0)))
     :VERTICES       ((0.0 0.0 0.0)
                      (201.179 167.508 -9.364)
                      (444.505 326.169 -4.261)
                      (767.46 313.506 47.895))))


(describe *obj*)

freedius-io::*obj*

(setf (obj::graphics-style (gui::selected-object))
      (make-instance 'graphics-style  
			  :color 'red 
			  :line-width 4 
			  :dash-style '(6 6)
			  :stipple '((0 1 0 0) (0 0 1 0) (0 0 0 0) (0 0 0 0))
			  ))

(io-form (obj::graphics-style (gui::selected-object)))

(describe (GRAPHICS-STYLE :STIPPLE
                ((0 1 0 0) (0 0 1 0) (0 0 0 0) (0 0 0 0))
                :DASH-STYLE (6 6)
                :LINE-WIDTH 4
                :COLOR RED))

(obj::set-attribute (gui::selected-object) :description "Pipeline xx yy zz")
(obj::get-attribute (gui::selected-object) :description)

(io-form (3d-world (top-view)))

(io-form (lvcs :name "foo"))

(describe (object-to-parent-transform (gui::selected-object)))

(describe (gui::selected-object))

(listarray (obj::%vertex-array (gui::selected-object)))


|#
