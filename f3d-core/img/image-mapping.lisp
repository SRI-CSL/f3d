(in-package :img)

#|
(maybe-compile-file-load '("$FREEDIUS/lisp/img/image-mapping.lisp"))
|#

(eval-when (eval load compile)
(import '(lx::eval-cache-probe lx::*eval-cache-probe* lx::eval-cache-process-results))
)



(defstruct (linear-geom-transform-info (:type list))
  image
  matrix ; this needs to be changed to (transform-matrix (image-to-2d-transform image))
  x-dim
  y-dim)

;;; Wed Nov 24 2004  New version using 4x4 matrices compatible with 4x4-coordinate-transform.
;;; Note the the uses of a02 and a12 throughout this file really refer to (aref mat 0 3) (aref mat 1 3)
;;; Note that transform-matrix defines the transform from the new image to the parent-image.
(defun linear-geom-transform-coeffs (transform-matrix)
  (let ((mat transform-matrix))
    (values (aref mat 0 0) (aref mat 0 1) (aref mat 0 3)
	    (aref mat 1 0) (aref mat 1 1) (aref mat 1 3))))

;;; (defun transform-matrix-from-coeffs (matrix a00 a01 a02 a10 a11 a12)
;;;   (declare (optimize (speed 3) (safety 1)))
;;;   #+cmu (declare (optimize (ext::inhibit-warnings 3)))
;;;   (declare (type (or null (simple-array double-float (3 3))) matrix))
;;;   (let ((matrix (or matrix (math::make-4x4-identity-matrix))))
;;;     (declare (type (simple-array double-float (4 4)) matrix))
;;;     (macrolet ((dfloat (x) `(float ,x 0.0)))
;;;       (setf (aref matrix 0 0) (dfloat a00)
;;; 	    (aref matrix 0 1) (dfloat a01)
;;; 	    (aref matrix 0 3) (dfloat a02)
;;; 	    (aref matrix 1 0) (dfloat a10)
;;; 	    (aref matrix 1 1) (dfloat a11)
;;; 	    (aref matrix 1 3) (dfloat a12)))
;;;     matrix))

;;; New Sat May 10 2008

;; Set to NIL for old (broken) behavior.
(defparameter *image-linear-geom-map-compute-params-bottom-left-pixel-origin* nil) 
;; Set to T for new behavior (possibly broken in a different manner).
;;(defparameter *image-linear-geom-map-compute-params-bottom-left-pixel-origin* t)

(defun transform-matrix-from-coeffs (matrix a00 a01 a02 a10 a11 a12)
  (declare (optimize (speed 3) (safety 1)))
  #+cmu (declare (optimize (ext::inhibit-warnings 3)))
  (declare (type (or null (simple-array double-float (3 3))) matrix))
  (when *image-linear-geom-map-compute-params-bottom-left-pixel-origin*
    (when (or (< a00 0.0) (< a01 0.0)) (decf a02))
    (when (or (< a10 0.0) (< a11 0.0)) (decf a12)))
  (let ((matrix (or matrix (math::make-4x4-identity-matrix))))
    (declare (type (simple-array double-float (4 4)) matrix))
    (macrolet ((dfloat (x) `(float ,x 0.0)))
      (setf (aref matrix 0 0) (dfloat a00)
	    (aref matrix 0 1) (dfloat a01)
	    (aref matrix 0 3) (dfloat a02)
	    (aref matrix 1 0) (dfloat a10)
	    (aref matrix 1 1) (dfloat a11)
	    (aref matrix 1 3) (dfloat a12)))
    matrix))

;;; old version with 3x3 matrices
;;;(defun linear-geom-transform-coeffs (transform-matrix)
;;;  (let ((arr transform-matrix))
;;;    (values (aref arr 0 0) (aref arr 0 1) (aref arr 0 2)
;;;            (aref arr 1 0) (aref arr 1 1) (aref arr 1 2))))

;;; old version with 3x3 matrices
;;;(defun transform-matrix-from-coeffs (matrix a00 a01 a02 a10 a11 a12)
;;;  (declare (optimize (speed 3) (safety 1)))
;;;  #+cmu (declare (optimize (ext::inhibit-warnings 3)))
;;;  (declare (type (or null (simple-array double-float (3 3))) matrix))
;;;  (let ((matrix (or matrix (make-array '(3 3) :element-type 'double-float :initial-element 0.0))))
;;;    (declare (type (simple-array double-float (3 3)) matrix))
;;;    (macrolet ((dfloat (x) `(float ,x 0.0)))
;;;      (setf (aref matrix 0 0) (dfloat a00)
;;;            (aref matrix 0 1) (dfloat a01)
;;;            (aref matrix 0 2) (dfloat a02)
;;;            (aref matrix 1 0) (dfloat a10)
;;;            (aref matrix 1 1) (dfloat a11)
;;;            (aref matrix 1 2) (dfloat a12)
;;;            (aref matrix 2 0) 0.0
;;;            (aref matrix 2 1) 0.0
;;;            (aref matrix 2 2) 1.0))
;;;    matrix))




(defun simple-image-window-commuter (form &optional (scale 1))
  (image-window-commuter form :scale scale))

(defun non-scaling-image-window-commuter (form &optional (scale 1))
  (image-window-commuter form :scale scale :scaling-allowed nil)) 

;;; We are considering economizing the computation of (fn image).
;;; image = (IMAGE-LINEAR-GEOM-MAP window-of)
;;; and if fn has already been applied to window-of,
;;; then we can more cheaply compute (IMAGE-LINEAR-GEOM-MAP (fn window-of)).

;;; SCALING-ALLOWED allows for the previous IMAGE-LINEAR-GEOM-MAP to contain
;;; scaling.
;;; SCALE is the scaling

(defun image-window-commuter (form &key (scale 1) (scaling-allowed t))
  (let* ((fn (car form))
	 (image (cadr form))
	 (args (cddr form))
	 (transform (image-prop image :linear-geom-transform)))
    (when transform
      (let* ((window-of (linear-geom-transform-info-image transform))
	     target
	     (xdim (linear-geom-transform-info-x-dim transform))
	     (ydim (linear-geom-transform-info-y-dim transform)))
	(mv-bind (a00 a01 a02 a10 a11 a12) 
	    (linear-geom-transform-coeffs (linear-geom-transform-info-matrix transform))

	  ;;(break)
	  (when (and window-of
		     (or scaling-allowed
			 (and (= (abs (+ a00 a01)) 1) (= (abs (+ a10 a11)) 1))
			 ))
	    (setq target (eval-cache-probe (apply fn window-of args)))
	    ;;(break)
	    (when target
	      (let ((*eval-cache-probe* nil)) ; break possible recursion
		(eval-cache-process-results
		 form 
		 (list (image-linear-geom-map
			target
			a00 a01 a10 a11
			(* scale a02) ;;(+ (* scale a02) (if (> (+ a00 a01) 0) 0 (- scale 1)))
			(* scale a12) ;;(+ (* scale a12) (if (> (+ a10 a11) 0) 0 (- scale 1)))
			(* scale xdim) ;; (ceiling (* scale xdim))
			(* scale ydim) ;; (ceiling (* scale ydim))
			)))))))))))


#|
(defun-cached foo (x)
  (list x))

(setq x1 (foo 1))
(setq x2 (foo 1))

(eq x1 x2)

(eq (foo 1) (foo 1))



(gethash 1 (get 'foo :function-cache))

(maphash #'(lambda (key val) (format t "~a => ~a~%" key val)) (get 'foo :function-cache))

(untrace lx::quick-eval-cache-find-match)


(eq (lx::quick-eval-cache-find-match 'foo 1)
    (lx::quick-eval-cache-find-match 'foo 1))

|#

(defun image-geom-linear-xform-compute-result-bbox (image du/dx du/dy u0 dv/dx dv/dy v0)
  (let ((u-max (float (1- (image-x-dim image))))
	(v-max (float (1- (image-y-dim image)))))
    (loop with det = (float (- (* du/dx dv/dy) (* du/dy dv/dx)))
	  for (u . v) in `((0 . 0) (0 . ,v-max) (,u-max . 0) (,u-max . ,v-max))
	  for x = (/ (- (* (- u u0) dv/dy) (* (- v v0) du/dy)) det)
	  for y = (/ (- (* (- v v0) du/dx) (* (- u u0) dv/dx)) det)
	  minimize x into x-min
	  minimize y into y-min
	  maximize x into x-max
	  maximize y into y-max
	;;; x-dim y-dim x-start y-start
	  finally (let* ((x-start (truncate x-min))
			 (y-start (truncate y-min))
			 (u-start (+ u0 (+ (* du/dy y-start) (* du/dx x-start))))
			 (v-start (+ v0 (+ (* dv/dx x-start) (* dv/dy y-start)))))
		    (return (values (floor (1+ (- x-max x-min)))
				    (floor (1+ (- y-max y-min)))
				    u-start v-start))))))

(defun image-linear-geom-map-compute-params (image a00 a01 a10 a11 &optional a02 a12 xdim ydim)
  (let* ((x-dim (image-x-dim image))
	 (y-dim (image-y-dim image)))
    (cond ((and (zerop (* a00 a01)) (zerop (* a10 a11)))
	   ;; simple multiple of 90 degree rotation or flip.
	   (if (zerop a00)		; transpose-p
	       (setq a02 (or a02 (if (> a01 0) 0 x-dim))
		     a12 (or a12 (if (> a10 0) 0 y-dim))
		     xdim (or xdim (/ y-dim (abs a10)))
		     ydim (or ydim (/ x-dim (abs a01))))
	       ;; There is something flakey going on.
	       ;; On 2/17/88 - I had to change x-dim and y-dim below to (1- x-dim) and (1- y-dim)
	       ;; for no apparent reason.  After recompiling the system on 3/6/88, I had to revert
	       ;; these to x-dim and y-dim.

	       ;; Sat May 10 2008 LHQ: There are problems related to pixel coordinates vs. continuous
	       ;; coordinates.  
	       ;; If the pixel centers are at <i+.5, j+.5>, then the following is correct.
	       ;; If the pixel centers are at <i,j> then we need the (1- x-dim ), (1- y-dim ) values.
	       ;; INTERPOLATE-IREF assumes that pixel origin is bottom left corner, ie, the 2nd case,
	       ;; which means that the following is wrong.
	       (setq a02 (or a02 (if (> a00 0) 0 x-dim)) ; (1- x-dim )  
		     a12 (or a12 (if (> a11 0) 0 y-dim)) ; (1- y-dim )
		     xdim (or xdim (/ x-dim (abs a00)))
		     ydim (or ydim (/ y-dim (abs a11)))))
	   ;;(format t "image-linear-geom-map-bbox ~a~%" (list xdim ydim a02 a12))
	   )

	  (t
	   (unless (and xdim ydim a02 a12)
	     (multiple-value-setq (xdim ydim a02 a12)
	       (image-geom-linear-xform-compute-result-bbox image a00 a01 0 a10 a11 0)))
	   ;;(format t "image-geom-linear-xform-compute-result-bbox ~a~%" (list xdim ydim a02 a12))
	   ))
    (values a02 a12 xdim ydim)))



(defun superior-ok-for-image-linear-geom-map (image superior)
  (ignore image superior)
  t)

(defun image-linear-geom-map (image a00 a01 a10 a11 &optional a02 a12 xdim ydim
				    (compose-transforms-p t))
   
  (let* ((x-dim (image-x-dim image))
	 (y-dim (image-y-dim image))
	 (superior-info (and compose-transforms-p
			     (image-indirected-p image)	; compose transforms only
					; if image is indirected
					;(image-window-of image) ; image-window-of requires no scaling
			     (image-prop image :linear-geom-transform)))
	 identity
	 (thresh 1e-9)
	 )
    
    (when (< (abs a00) thresh) (setq a00 0.0))
    (when (< (abs a01) thresh) (setq a01 0.0))
    (when (< (abs a10) thresh) (setq a10 0.0))
    (when (< (abs a11) thresh) (setq a11 0.0))
    
    (unless (superior-ok-for-image-linear-geom-map image superior-info)
      (setq superior-info nil))

    (multiple-value-setq (a02 a12 xdim ydim)
      (image-linear-geom-map-compute-params image a00 a01 a10 a11  a02 a12 xdim ydim))
  
    (when superior-info
      ;; Compute transform relative to superior image instead of the image argument.
      (let ((matrix (transform-matrix-from-coeffs nil a00 a01 a02 a10 a11 a12)))
	(setq image (linear-geom-transform-info-image superior-info)
	      matrix (math:multiply-matrices (linear-geom-transform-info-matrix superior-info)
					     matrix matrix)
	      x-dim (image-x-dim image)
	      y-dim (image-y-dim image)) 
	(mv-setq (a00 a01 a02 a10 a11 a12) (linear-geom-transform-coeffs matrix))))

    (setq identity (and (= a00 1.0) (= a11 1.0) (= a01 0.0) (= a10 0.0)))

    (if (and identity (= xdim x-dim) (= y-dim ydim) (= a02 0.0) (= a12 0.0))
	image
	(image-linear-geom-map-internal image a00 a01 a02 a10 a11 a12 xdim ydim identity)
	)))

(defun transform-map (map fn range)
  (if (arrayp fn) 
      fn
      (loop with table = (make-stationary-vector range :element-type (array-element-type map))
	    for i from 0 below range
	    do (setf (aref table i) (aref map (floor (funcall fn i))))
	    finally (return table))))

;(fmakunbound 'map-image-geometry)

(defmethod map-image-geometry
	   ((image image) 
	    x-fun  y-fun			
	    &optional
	    xy-swap transform-mat
	    (x-range (image-x-dim image))
	    (y-range (image-y-dim image)))
  (setq x-range (floor x-range)
	y-range (floor y-range))
  (let ((new-image (clone-image image))
	(xmap (image-x-map image))
	(ymap (image-y-map image)))
    (when xy-swap (rotatef xmap ymap))
    ;; by default this next takes care of making map-hacked image non-bitbltable.
    (set-image-maps new-image
		    (transform-map xmap x-fun x-range)
		    (transform-map ymap y-fun y-range))
    (set-linear-geom-transform new-image
			       (make-linear-geom-transform-info
				:image image
				:matrix transform-mat
				:x-dim x-range :y-dim y-range))
    new-image))

(defmethod map-image-geometry :around 
	   ((image image) x-fun y-fun 
	    &optional xy-swap transform-mat x-range y-range)
  (declare (ignore x-fun y-fun xy-swap transform-mat x-range y-range))
  (let ((result (call-next-method)))
    ;; clone-image causes this next to be inherited.
    (remove-image-prop result :linear-geom-transform)
    result))

(defun window-map (map start-index count &optional (window-mode :copy))
  (when (eq window-mode :window)
    (error "Array displacement of image maps is no longer allowed."))
  (let ((new-map (make-stationary-vector count :element-type (array-element-type map))))
    (loop for to-i from 0 below count
	  for from-i from start-index
	  do (setf (aref new-map to-i) (aref map from-i)))
    new-map))

(defmethod window-self ((image image)
			ignore-image from-x from-y new-x-dim new-y-dim
			&optional new-x-map new-y-map)
  (ignore ignore-image)
  (setf (image-x-map image) (or new-x-map (window-map (image-x-map image) from-x new-x-dim))
	(image-y-map image) (or new-y-map (window-map (image-y-map image) from-y new-y-dim))
	(image-x-dim image) new-x-dim
	(image-y-dim image) new-y-dim))

;;; (method set-linear-geom-transform :after (image)) is defiend in image-gui-defs.lisp
(defmethod set-linear-geom-transform ((image image) transform-info)
  (setf (image-prop image :linear-geom-transform) transform-info))

#|
missing functions:
IMAGE-GEOM-LINEAR-XFORM
MAP-IMAGE-GEOMETRY
SET-LINEAR-GEOM-TRANSFORM
WINDOW-SELF
(IMAGE-BITBLTABLE IMAGE) (SETF IMAGE-BITBLTABLE)
IMAGE-WINDOW-OF (SETF IMAGE-WINDOW-OF)
Undefined type: BLOCKED-MAPPED-IMAGE-MIXIN

(eval-cache-flush-function 'image-linear-geom-map)
|#
(defmethod image-linear-geom-map-internal
	   ((image image)
	    a00 a01 a02 a10 a11 a12 xdim ydim identity)
  (setq xdim (floor xdim) ydim (floor ydim))
  (if (not (and (zerop (* a00 a01)) (zerop (* a10 a11)))) ; not a unitary transform 
      ;; image-geom-linear-xform should be defined in image-interpolation.lisp
      (image-geom-linear-xform image a00 a01 a02 a10 a11 a12 xdim ydim)

      
      ;; This caching is NOT CANONICAL  -- it should be on coordinates in the 2d-world, and, ideally,
      ;; the image should be the topmost parent in a chain of indirected images.
      (eval-cache (image-linear-geom-map image a00 a01 a10 a11 a02 a12 xdim ydim)
	  (let* ((transform-matrix (transform-matrix-from-coeffs nil a00 a01 a02 a10 a11 a12))
		 (result-image
		  (if (zerop a00)	; transpose-p
		      (let ((a12p (if (plusp a10) a12 (+ a12 a10)))
			    (a02p (if (plusp a01) a02 (+ a02 a01))))
			(map-image-geometry image
					    #'(lambda(x) (+ (* a10 x) a12p))
					    #'(lambda(y) (+ (* a01 y) a02p))
					    t transform-matrix xdim ydim)) 
		      (let ((a02p (if (plusp a00) a02 (+ a02 a00)))
			    (a12p (if (plusp a11) a12 (+ a12 a11))))
			(map-image-geometry image
					    #'(lambda(x) (+ (* a00 x) a02p))
					    #'(lambda(y) (+ (* a11 y) a12p))
					    nil transform-matrix xdim ydim)))))
	    #+never ; Sat Dec 25 2004 not needed ?
	    (when identity 
	      (setf (image-window-of result-image) (or (image-window-of image) image))
	      ;; Is this next needed? It looks like the map-image-geometry will to the right thing
	      (window-self result-image image (floor a02) (floor a12) (floor xdim) (floor ydim)
			   (image-x-map result-image)
			   (image-y-map result-image))
	      ;; In Allegro slot-exists-p causes error for structure-class objects
	      #+FIXME
	      (when (slot-exists-p result-image 'bitbltable)
		(setf (image-bitbltable result-image)
		      (and (slot-exists-p image 'bitbltable)
			   (image-bitbltable image)))))
	    ;; identity transformed images are bitbltable if their parent image is bitbltable
	    ;; otherwise we use on ON-THE-FLY-DISPLAY-IMAGE function for their display. 
	    #|(unless (and identity (send-if-handles  image :bitbltable))
	    (setf (image-prop result-image :display-image-function)
	    	  'on-the-fly-display-image)
	    )|#
	    
	    result-image))))

(defmethod image-window ((image image)
   &optional (from-x 0) (from-y 0) (xdim (image-x-dim image)) (ydim (image-y-dim image))
   (compose-transforms-p t))
  ;; (declare (arglist image from-x from-y x-dim y-dim))
  (let (result)
    (when (minusp from-x)
	  (setq xdim (+ xdim from-x) from-x 0))
    (when (minusp from-y)
	  (setq ydim (+ ydim from-y) from-y 0))

    (setq xdim (max 0 (min xdim (- (image-x-dim image) from-x)))
	  ydim (max 0 (min ydim (- (image-y-dim image) from-y))))

    (setq result (image-linear-geom-map image 1 0 0 1 from-x from-y xdim ydim
					compose-transforms-p))
    ;; clone-image does this next
    ;;(push result (image-prop image :inferiors))
    result))

;(gui::push-image (image-window (gui::view-image (gui::top-view)) 1000 1000 512 512) (gui::selected-window))
;(gui::push-image (image-window (gui::view-image (gui::top-view)) 200 200 256 256)(gui::selected-window))
;(image-x-dim (gui::view-image (gui::top-view)))
;(typep (gui::view-image (gui::top-view)) 'vector-image)
;(gui::pop-view (gui::selected-window))
;(eval-cache-flush-function 'image-linear-geom-map)
;(gui::push-image (image-transpose (gui::view-image (gui::top-view))) (gui::selected-window))
;(gui::push-image (image-scale (gui::view-image (gui::top-view)) .5) (gui::selected-window))

(defun image-neg-x (image)
  (image-linear-geom-map image -1 0 0 1))

(defun image-neg-y (image)
  (image-linear-geom-map image 1 0 0 -1))

(defun image-transpose (image)
  (image-linear-geom-map image 0 1 1 0))

(defun image-rotate (image)
  (image-linear-geom-map image 0 -1 1 0))

(defun image-rotate-180 (image)
  (image-linear-geom-map image -1 0 0 -1))

(defun image-rotate-ccw (image)
  (image-linear-geom-map image 0 1 -1 0))

(defun image-scale (image x-scale &optional (y-scale x-scale))
  (image-linear-geom-map image (/ 1 x-scale) 0 0 (/ 1 y-scale)))

;;; This is really same as image-scale where scale-factor = (/ decimation-factor).
(defun image-decimate (image  x-decimation-factor
		     &optional (y-decimation-factor x-decimation-factor) (x-off 0) (y-off 0))
  (image-linear-geom-map image x-decimation-factor 0 0 y-decimation-factor x-off y-off))





#|

(setq img (load-image "/homedir/quam/pix/rugby.pic"))

(setq img *)

(describe img)
(gui::push-image img (gui::selected-window gui::*interactor*))

(setq win1 (image-window img 100 100 100 100))
(describe win1)
(let ((*print-array* t))
  (pprint (image-x-map win1))(pprint (image-y-map win1)))

(gui::push-image win1 (gui::selected-window gui::*interactor*))
(gui::pop-view (gui::selected-window gui::*interactor*))


(setq img-trans (image-transpose img))
(gui::push-image img-trans (gui::selected-window gui::*interactor*))

(setq win1-trans (image-transpose win1))
(gui::push-image win1-trans (gui::selected-window gui::*interactor*))



(setq g0 (make-image '(101 100)))
(setq g0 (load-image "/homedir/quam/pix/rugby.pic"))
(gui::push-image * (gui::selected-window gui::*interactor*))
(setq g1 (zoom-out g0))
(setq w0 (image-window g0 25 25 50 50))
(setq w0 (image-window g0 81 112 200 300))
(setq w1 (zoom-out w0))
(setq w2 (zoom-out w1))
(eq (image-window-of w1) g1) = t  =>  WIN!
(image-indirected-to w1)
(describe w1)

(setq g1 (zoom-out g0))
(setq w1 (image-window g1 40 56 100 150))
(setq w0 (zoom-in w1))
(eq (image-window-of w0) g0)
(describe w0)
(list (image-window-of w0) g0)
(property-list w0)

(lx::quick-eval-cache-find-match 'zoom-out (list w0))

(image-to-2d-transform g0)
(describe g0)
(describe g1)
(eq g0 (top-of-image-pyramid g0))
(eq g0 (top-of-image-pyramid g1))

(image-pyramid-level g0)
(image-pyramid-level g1)

(eq (zoom-out g0) g1)

(disassemble (slot-value (find-method (symbol-function 'zoom-out) nil (list (find-class 'image)))
			'pcl::fast-function))
(slot-value (find-method (symbol-function 'zoom-out) nil (list (find-class 'image)))
	    'pcl::fast-function)

;;; This requires new pcl
(disassemble '(pcl::fast-method zoom-out (image)))

(trace non-scaling-image-window-commuter)
|#


(fmakunbound 'make-bordered-map)
;;; This code looks buggy --- called from MAP-DTM-TO-OCTANT in cme/dtm-intersect.lisp

;;; border-type other than :replicate is buggy.
(defun make-bordered-map (old-map border-size border-type)
  (declare (type image-map-type old-map))
  ;;(declare (type-reduce number fixnum))
  (let* ((old-dim (length old-map))
	 (left-border (if (listp border-size) (first border-size) border-size))
	 (right-border (if (listp border-size) (second border-size) border-size))
	 (new-dim (+ old-dim (+ left-border right-border)))
	 (new-map (make-foreign-vector new-dim :element-type 'image-map-element-type :initial-element 0)))
    (declare (type image-map-type new-map))
    (declare (fixnum old-dim left-border right-border new-dim))
    (if (= left-border 0)
	(math:copy-array-contents old-map new-map)
	(loop for i from 0 below old-dim
	      do (setf (aref new-map (+ left-border i))
		       (aref old-map i))))
    (loop for i fixnum from 0 below left-border do
      (setf (aref new-map i)
	    (case border-type
	      (:replicate (aref old-map 0))
	      (:wrap-around (aref old-map (mod (- i left-border) old-dim)))
	      (:reflect (aref old-map (- (+ left-border left-border) i)))
	      (otherwise 0))))
    (loop for i fixnum from 0 below right-border do
      (setf (aref new-map (+ i old-dim left-border))
	    (case border-type
	      (:replicate (aref old-map (1- old-dim)))
	      (:wrap-around (aref old-map (mod i old-dim)))
	      (:reflect (aref old-map (- old-dim 2 i)))
	      (otherwise 0))))
    new-map))
