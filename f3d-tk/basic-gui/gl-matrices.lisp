(in-package :gui)

#|
  
Implements: matrices compatible with OpenGL MODELVIEW matrix and PROJECTION_MATRIX

       default-2d-to-window-matrix
       make-2d-to-window-matrix
       window-to-ndc-matrix
       make-camera-to-2d-matrix
       make-adjustable-camera
       set-gl-3d-to-2d-projection
       set-3d-matrices
       set-2d-matrices
|#

;;;
;;; Make this a method, so we can specialize on non-image objects.
;;;

;;; This is the "zoom-to-fit" transform.
;;; If image = NIL, scale is 1:1.
;;;(defmethod default-2d-to-window-matrix (win image)
;;;  (mv-bind (width height) (dimensions win)
;;;    (unless width
;;;      (mv-setq (width height) (set-window-dimensions win)))
;;;    (let* ((xdim (dfloat (if image (img::image-x-dim image) width)))
;;;           (ydim (dfloat (if image (img::image-y-dim image) height)))
;;;           (xscale (/ width xdim))
;;;           (yscale (/ height ydim))
;;;           (scale (min xscale yscale))
;;;           (xoff (* .5 (- width (* scale xdim))))
;;;           (yoff (* .5 (- height (* -1.0 scale ydim))))
;;;           (mat (make-array '(4 4) :element-type 'double-float
;;;                            :initial-contents
;;;                            `((,scale 0.0 0.0 ,xoff)
;;;                              (0.0 ,(- scale) 0.0 ,yoff)
;;;                              (0.0 0.0 1.0 0.0)
;;;                              (0.0 0.0 0.0 1.0)))))
;;;      ;;(push (list mat win image (list width height)) *foomat*)
;;;      ;; (when (= width 1) (break)) ;; 
;;;      mat)))

;;; This is the "zoom-to-fit" transform.
;;; If image = NIL, scale is 1:1. 
;;; This version takes image-to-2d-transform into account and preserves 2d-world orientation.
#+never
(defmethod default-2d-to-window-matrix (win image &optional (auto-scale t))
  (mv-bind (width height) (dimensions win)
    (unless width (mv-setq (width height) (set-window-dimensions win)))
    (bind-vector-elements (x0 x1 y0 y1) 
	(if image
	    (transform-bounding-box (image-to-2d-transform image) (bounding-box image))
	    (cv 0.0 width 0.0 height))
      (let* ((xdim (- x1 x0))
	     (ydim (- y1 y0))
	     (xscale (/ width xdim))
	     (yscale (/ height ydim))
	     (scale (min xscale yscale))
	     (xoff (* .5 (- width (* scale (+ x0 x1)))))
	     (yoff (* .5 (- height (* -1.0 scale (+ y0 y1)))))
	     (2dwmat (make-and-fill-4x4-matrix 
		      scale 0.0 0.0 xoff
		      0.0 (- scale) 0.0 yoff
		      0.0 0.0 1.0 0.0
		      0.0 0.0 0.0 1.0)))
	2dwmat))))

;;; default-2d-to-window-matrix was modified in a manner incompatible with its previous usage.
;;; That modification should have received a different name.
;;; This defines 1st-quadrant raster coordinates, ie. origin is at bottom left corner of window.
(defmethod 1st-quadrant-2d-to-window-matrix (win)
  (mv-bind (xdim ydim) (dimensions win)
    (declare (ignorable xdim))
    (make-and-fill-4x4-matrix 
     1.0 0.0 0.0 0.0
     0.0 -1.0 0.0 ydim
     0.0 0.0 1.0 0.0
     0.0 0.0 0.0 1.0)))


;(fmakunbound 'default-2d-to-window-matrix)

;;; This is the "zoom-to-fit" transform.
;;; If image = NIL, scale is 1:1.
;;; This version takes image-to-2d-transform into account and preserves image orientation,
;;; including neg-x neg-y and transpose.
(defmethod default-2d-to-window-matrix (win image &optional (auto-scale t))
  (mv-bind (width height) (dimensions win)
    (unless width (mv-setq (width height) (set-window-dimensions win)))
    (if (not (and image (image-to-2d-transform image)))
	(let ((mat (make-4x4-identity-matrix)))
	  (setf (aref mat 0 3) (* .5 width)
		(aref mat 1 3) (* .5 height))
	  mat)
	(let* ((image-to-window-scale (if auto-scale
					  (min (/ (dfloat width)  (image-x-dim image))
					       (/ (dfloat height) (image-y-dim image)))
					  1.0))
	       (2dwmat (invert-matrix (transform-matrix (image-to-2d-transform image)))))
	  (loop for i from 0 to 3
		do (setf (aref 2dwmat 0 i) (* image-to-window-scale (aref 2dwmat 0 i))))
	  (loop for i from 0 to 3
		do (setf (aref 2dwmat 1 i) (- (* image-to-window-scale (aref 2dwmat 1 i)))))
	  (incf (aref 2dwmat 0 3) (- (* .5 width) (* .5 image-to-window-scale (image-x-dim image))))
	  (incf (aref 2dwmat 1 3) (- (* .5 height) (* -.5 image-to-window-scale (image-y-dim image))))
	  ;;(let ((*print-array* t)) (print 2dwmat))
	  2dwmat))))

#|
;;; CC defines this is freeduis-systems/base/freedius-patches/utils.lisp
;;; I find no callers in freedius-systems.
;;; This implements a 180 degree rotation when camera-flipped-p = T.  Weird.
;;; The need for camera-flipped-p  is probably an indication that
;;; the display-image methods are not taking into account the sign of image-block-y-dim.
(defmethod default-2d-to-window-matrix (win image &optional auto-scale)
  (mv-bind (width height) (dimensions win)
    (unless width
      (mv-setq (width height) (set-window-dimensions win)))
    (let* ((xdim (dfloat (if image (img::image-x-dim image) width)))
	   (ydim (dfloat (if image (img::image-y-dim image) height)))
	   (xscale (/ width xdim))
	   (yscale (/ height ydim))
	   (scale (min xscale yscale))
	   (xoff (* .5 (- width (* (if (camera-flipped-p image) -1.0 1.0) scale xdim))))
	   (yoff (* .5 (- height (* (if (camera-flipped-p image) 1.0 -1.0) scale ydim))))
	   (mat (make-array '(4 4) :element-type 'double-float
			    :initial-contents
			    (if (camera-flipped-p image)
				`((,(- scale) 0.0 0.0 ,xoff)
				  (0.0 ,scale 0.0 ,yoff)
				  (0.0 0.0 1.0 0.0)
				  (0.0 0.0 0.0 1.0))
				`((,scale 0.0 0.0 ,xoff)
				  (0.0 ,(- scale) 0.0 ,yoff)
				  (0.0 0.0 1.0 0.0)
				  (0.0 0.0 0.0 1.0))))))
	   
      ;;(push (list mat win image (list width height)) *foomat*)
      ;; (when (= width 1) (break)) ;; 
      mat)))
|#

;(let ((*print-array* t)) (describe(default-2d-to-window-matrix (gui::selected-window)(gui::view-image (gui::top-view)) )))

;(let ((*print-array* t)) (print (transform-matrix (image-to-2d-transform (gui::view-image (gui::top-view)) ))))


;(fmakunbound 'default-2d-to-window-transform)

(defmethod default-2d-to-window-transform ((window basic-window) image-or-null)
  (make-4x4-coordinate-transform
   (default-2d-to-window-matrix window image-or-null)))

;;; changed from passing the window to passing the view so that diffeent
;;; subclasses of view can to it differently.
(defmethod default-2d-to-window-transform ((view view) image-or-null)
  (make-4x4-coordinate-transform
   (default-2d-to-window-matrix (view-window view) image-or-null)))

;;; I do not understand the work "parity" in this context.
;;; Wouldn't "unity" be more appropriate?

;;; I used "parity" in the sense that image pixels are the same as
;;; screen pixels, although that's probably a misnomer if the image in
;;; question isn't at the bottom of the pyramid.  "Unity" would be a
;;; fine choice.

;;; This is the "zoom-1:1" transform centering the image in the window.
;;;(defun parity-2d-to-window-matrix (win image)
;;;  (mv-bind (width height) (dimensions win)
;;;    (unless width
;;;      (mv-setq (width height) (set-window-dimensions win)))
;;;    (let* ((xdim (dfloat (img::image-x-dim image)))
;;;           (ydim (dfloat (img::image-y-dim image)))
;;;           (scale 1.0)
;;;           (xoff (* .5 (- width xdim)))
;;;           (yoff (* .5 (- height (- ydim)))))
;;;      (make-array '(4 4) :element-type 'double-float
;;;                  :initial-contents
;;;                  `((,scale 0.0 0.0 ,xoff)
;;;                    (0.0 ,(- scale) 0.0 ,yoff)
;;;                    (0.0 0.0 1.0 0.0)
;;;                    (0.0 0.0 0.0 1.0))))))

(defun parity-2d-to-window-matrix (win image)
  (default-2d-to-window-matrix win image nil))

(defun parity-2d-to-window-transform (win image)
  (make-4x4-coordinate-transform
   (parity-2d-to-window-matrix win image)))




;;; Not sure this is the right place, but ...
;;; This is currently only used by basic-gui/terrain.lisp
(defun make-adjustable-camera (gndpos &key (height 15000.0) (1/f -.001) gsd win fov)
  (when gsd
    (setq 1/f (- (/ gsd height))))
  (when (and win fov)
    (mv-bind (w h) (dimensions win)
      (ignore h)
      (setq 1/f (/ (tan (* (/ pi 180.0) .5 fov)) (* .5 w)))
      (when gsd (setq height (/ gsd 1/f)))
      ))
  (let* ((focal-length (if (zerop 1/f) nil (/ 1/f)))
	 (3d-to-2d-projection
	  (make-frame-camera-from-matrices
	   ;; world-to-camera-matrix
	   (invert-matrix 
	    (make-object-to-parent-matrix
	     (vector-add gndpos (cv 0.0 0.0 height))
	     ;;:y-deg 180.0
	     ))
	   (make-camera-to-2d-matrix :principal-point (list 0.0 0.0 focal-length) 
				     :near-far (list (* .5 height)))
	   ))
	 )
    3d-to-2d-projection))

#|
(setq p (make-camera-to-2d-matrix :principal-point (list 1665.6791 1986.6626 -5067.3154) 
				  :r/f .5 :near-far '(10000.0 20000.0 )))
(4x4-project-vector p (cv 0.0 0.0 -10000.0 1.0)) 
(4x4-project-vector p (cv 0.0 0.0 -20000.0 1.0))
|#
#|
The inverse of interior-orientation-matrix is:

(make-and-fill-4x4-matrix 1.0 0.0 0.0 ,(- ppu)
			  0.0 1.0 0.0 ,(- ppv)
			  0.0 0.0 ,minv22 ,minv23
			  0.0 0.0 ,minv32 ,minv33)

where minv22 minv23 minv32 minv33 are the elements of
(math::invert-matrix
 (make-array '(2 2) :element-type 'double-float :initial-contents
	     `((,(aref m 22) ,(aref m 23)) (,(aref m 32) ,(aref m 33)))))

and m is the interior-orientation-matrix.

|#



;;; This is for reference only.
;;; Most of this stuff has moved into C libraries.

;;; Set window-to-ndc-matrix of current OpenGL window
;;;(defun set-window-to-ndc-matrix ()
;;;  (glMatrixMode GL_PROJECTION)
;;;  (glLoadIdentity)
;;;  (glMultMatrix (window_to_ndc_matrix))
;;;  )
    
;;; cset_2d_to_ndc_matrix is defined in GLdisplay-image.C
;;; Sets 2d-to-ndc-matrix of current OpenGL window
;;;(defun set-2d-to-ndc-matrix (2d-to-window-matrix)
;;;  (set-window-to-ndc-matrix)
;;;  (when 2d-to-window-matrix (glMultMatrix 2d-to-window-matrix))
;;;  )

;;;(defun get_window_dims ()
;;;  (let* ((vp (make-int-vector 4)))
;;;    (declare (type (simple-array (signed-byte 32) (*)) vp))
;;;    (glGetIntegerv GL_VIEWPORT vp)
;;;    (values (aref vp 2) (aref vp 3))))
	  
;;; Gets window_to_ndc_matrix of current OpenGL window.
;;;(defun window-to-ndc-matrix ()
;;;  (let* ((vp (make-int-vector 4))
;;;         (win-to-ndc (make-4x4-matrix)))
;;;    (declare (type (simple-array double-float (* *)) win-to-ndc))
;;;    (declare (type (simple-array (signed-byte 32) (*)) vp))
;;;             
;;;    (glGetIntegerv GL_VIEWPORT vp)
;;;    (setf (aref win-to-ndc 0 0) (/  2.0 (dfloat (aref vp 2)))
;;;          (aref win-to-ndc 1 1) (/ -2.0 (dfloat (aref vp 3)))
;;;          (aref win-to-ndc 0 3) -1.0
;;;          (aref win-to-ndc 1 3)  1.0
;;;          (aref win-to-ndc 2 2)  1.0
;;;          (aref win-to-ndc 3 3)  1.0)
;;;    win-to-ndc))

(defun maybe-set-gl-pick-matrix ()
  (when obj::*gl-selection-pick-matrix-params*
      (apply #'set-gl-pick-matrix *gl-selection-pick-matrix-params*)))

;;; Set pick-matrix of current OpenGL window
(defun set-gl-pick-matrix (x y size)
  (let ((vp (make-int-vector 4))
	;; glGetMatrix is needed because we need to pre-multiply
	;; the GL_PROJECTION_MATRIX by the result of gluPickMatrix
	(projection-matrix (glGetMatrix GL_PROJECTION_MATRIX)))
    (glGetIntegerv GL_VIEWPORT vp)
    (glMatrixMode GL_PROJECTION)
    (glLoadIdentity)
    ;;(format t "set-gl-pick-matrix ~a~%" (list x y size))
    (gluPickMatrix (dfloat x)
		   (dfloat (- (aref vp 3) (- y (* 2 (aref vp 1))) ))
		   size size vp)
    (glMultMatrix projection-matrix)))

;;; FIXME:  eliminate the assumption that SET-2D-TO-NDC-MATRIX has been called.
;;; Postmultiply GL_PROJECTION by the camera-to-2d-transform,
;;; ie. form the matrix product 2d->NDC * camera-to-2d-transform.
;;; This assumes SET-2D-TO-NDC-MATRIX has been called to
;;; set GL_PROJECTION to the transform from 2d coords to NDC (-1:+1) coords.
;;; Set GL_MODELVIEW to the world-to-camera-transform.
(defun set-gl-3d-to-2d-projection (3d-to-2d-projection)
  (mv-bind (interior exterior)  ;; and what do we do about nonlinear transformations??
      (transforms::interior-and-exterior-matrices 3d-to-2d-projection)
    ;;(format t "set-gl-3d-to-2d-projection ~a ~a~%" interior exterior)
    (when interior
      ;; This assumes that GL_PROJECTION_MATRIX has been set to 2d-to-ndc-matrix
      ;; by DISPLAY-IMAGE or SET-2D-TO-NDC-MATRIX
      (glMatrixMode GL_PROJECTION)
      (glMultMatrix interior))		; this is camera-to-2d-projection
    (load-gl-modelview-matrix exterior)))

#|
;;; New version that rescales 3rd row for compatibility with
;;; 4x4-project-vector.  When performing <u,v,w,s> = P<x,y,z,1>,
;;; OpenGL rescales w/s from -1:+1 to 0:+1.
;;; This version of set-gl-3d-to-2d-projection allows the interior matrix
;;; to always be set so that w/s in in the 0:+1 range.
;;; To set the OpenGL GL_PROJECTION_MATRIX, we must modify
;;; the 3rd row so that w/s is in the -1:+1 range.
;;; This depth remapping has moved to the window_to_ndc_matrix in GLDisplay-image.c++

;;;(defvar *set-gl-3d-to-2d-projection-tmp-matrix*
;;;  (make-4x4-identity-matrix))

;;; This depth remapping has moved to window_to_ndc_matrix in GLDisplay-image.c++
;;;(defun set-gl-3d-to-2d-projection (3d-to-2d-projection)
;;;  ;;(destructuring-bind (exterior interior) 3d-to-2d-projection)
;;;  (mv-bind (interior exterior) ;; and what do we do about nonlinear transformations??
;;;      (transforms::interior-and-exterior-matrices 3d-to-2d-projection)
;;;    (when interior
;;;      (copy-matrix interior *set-gl-3d-to-2d-projection-tmp-matrix*)
;;;      (let ((interior *set-gl-3d-to-2d-projection-tmp-matrix*))
;;;        (declare (type (simple-array double-float (4 4)) interior))
;;;        (loop for i fixnum from 0 below 4
;;;              do (setf (aref interior 2 i) (- (* 2.0 (aref interior 2 i)) (aref interior 3 i))))
;;;        ;; This assumes that GL_PROJECTION_MATRIX has been set to 2d-to-ndc-matrix
;;;        ;; by DISPLAY-IMAGE or SET-2D-TO-NDC-MATRIX
;;;        (glMatrixMode GL_PROJECTION)
;;;        (glMultMatrix interior)))       ; this is camera-to-2d-projection
;;;    (load-gl-modelview-matrix exterior)))
|#

(defun load-gl-modelview-matrix (mat)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
  (when mat (glMultMatrix mat))) 

#+never ; name conflicts with obj::set-modelview-matrix
(defun set-modelview-matrix (mat)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
  (when mat (glMultMatrix mat))
  )

;;; no callers
(defun load-gl-projection-matrix (mat)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (when mat (glMultMatrix mat)))
    
;;; wrong order of matrix multiplications
;;;(defun set-modelview-matrix-from-transform-list (transforms)
;;;  (when (cdr transforms)
;;;    (set-modelview-matrix-from-transform-list (cdr transforms)))
;;;  (glMultMatrix (transform-matrix (car transforms))))
    

(defun set-modelview-matrix-from-transform-list (transforms)
  (loop for trans in transforms
	do (glMultMatrix (transform-matrix trans))))


;;; Upon completion of this function, the transform from object coordinates to 
;;; LVCS coordinates of the view are established.  This means that the coordinate-system
;;; matches that of (from-coordinate-system (3d-to-2d-projection view)).
;;; If the chain of object-to-parent matrices from object to LVCS is T1, T2,
;;; ..., Tn, form the matrix product Tn * ... * T2 * T1.
;;; Called from with-gl-object-drawing-int in macros-gl-objects.lisp


;;;(defun obj::set-modelview-matrix (object)
;;;  (unless (eq object obj::*gl-object-drawing-superior*)
;;;    (let ((trans (object-to-view-world-transform object *current-view*)))
;;;      (if (consp trans)
;;;          (set-modelview-matrix-from-transform-list trans)
;;;          (glMultMatrix (transform-matrix trans))))))


;;;(fmakunbound 'obj::set-modelview-matrix)



;;;(defun obj::set-modelview-matrix (object)
;;;  (unless (eq object obj::*gl-object-drawing-superior*)
;;;    (loop for obj = object then (parent obj)
;;;          while (typep obj 'basic-gl-object)
;;;          do (glMultMatrix (transform-matrix (object-to-parent-transform obj)))
;;;          finally 
;;;       (when (typep object 'gl-3d-object-mixin)
;;;         (let ((view-world (3d-world *current-view*)))
;;;           (unless (eq obj view-world)
;;;             (glMultMatrix (transform-matrix 
;;;                            (transforms::optimize-transform 
;;;                             (transforms::canonical-coordinate-transform-path obj view-world))))))))))

(defparameter *obj* nil)

;;; wrong order matrix multiplication?
;;;(defun obj::set-modelview-matrix (object &optional view-world)
;;;  (when (eq object (or (gui::selected-object) *obj*))
;;;    (setq *mv-before* (glGetMatrix GL_MODELVIEW_MATRIX)))
;;;
;;;  (unless (eq object obj::*gl-object-drawing-superior*)
;;;    (loop for obj = object then (parent obj)
;;;          while (typep obj 'basic-gl-object)
;;;          do (glMultMatrix (transform-matrix (object-to-parent-transform obj)))
;;;          finally 
;;;       (setq *foo* (list object obj view-world))
;;;       ;;(format t "set-modelview-matrix ~a ~a ~a~%" object obj view-world)
;;;       (when view-world    ; (and view-world (typep object 'gl-3d-object-mixin))
;;;         (unless (eq obj view-world)
;;;           (format t "obj::set-modelview-matrix bridging between 3d-worlds~%")
;;;           (glMultMatrix (transform-matrix 
;;;                          (transforms::optimize-transform 
;;;                           (transforms::canonical-coordinate-transform-path obj view-world)))))))
;;;
;;;    (when (eq object (or (gui::selected-object) *obj*))
;;;      (let ((mvmat (glGetMatrix GL_MODELVIEW_MATRIX)))
;;;        (setq *mv-after* mvmat)
;;;        (pprint mvmat)
;;;        (pprint (matrix-times-vector mvmat (cv 0.0 0.0 0.0 1.0)))
;;;        ))
;;;
;;;    ))


;;;(defun set-modelview-matrix-int (object view-world)
;;;  (cond ((typep object 'basic-gl-object)
;;;         (set-modelview-matrix-int (parent object) view-world)
;;;         (glMultMatrix (transform-matrix (object-to-parent-transform object))))
;;;
;;;        ((and view-world (not (eq object view-world)))
;;;         ;;(format t "obj::set-modelview-matrix bridging between 3d-worlds ~a ~a~%" object view-world)
;;;         (glMultMatrix (transform-matrix 
;;;                        (transforms::optimize-transform 
;;;                         (transforms::canonical-coordinate-transform-path object view-world)))))))

(defun set-modelview-matrix-int (object view-world)
  (unless (eq object obj::*gl-object-drawing-superior*)
    (cond ((typep object 'basic-gl-object)
	   (set-modelview-matrix-int (parent object) view-world)
	   (glMultMatrix (transform-matrix (object-to-parent-transform object))))
	  ;; we get here when we have ascended to the world containing the object.
	  ((and view-world (not (eq object view-world)))
	   ;;(format t "obj::set-modelview-matrix bridging between 3d-worlds ~a ~a~%" object view-world)
	   (glMultMatrix (transforms::cached-cs-to-cs-transform-matrix object view-world))))))

(defun obj::set-modelview-matrix (object &optional view-world)
  (set-modelview-matrix-int object view-world))


#|
(defun gltst (m)
  (glMatrixMode GL_MODELVIEW)
  (glPushMatrix) (glLoadIdentity)
  (glMultMatrix m)
  (prog1 (glGetMatrix GL_MODELVIEW_MATRIX)
    (glPopMatrix)))

(defun gltst2 (object view-world)
  (glMatrixMode GL_MODELVIEW)
  (glPushMatrix) (glLoadIdentity)
  (set-modelview-matrix-int object view-world)
  (prog1 (glGetMatrix GL_MODELVIEW_MATRIX)
    (glPopMatrix)))
|#



;;; Set the OpenGL transform for pixel coordinates with origin at the bottom left.
;;; This assumes that glMakeCurrent has been called for the window
;;; This may be a bad idea.  Too many things are based on the assumption that
;;; window-coordinates are always X11 (Tk) 3rd quadrant window coordinates 
;;; rather than Opengl 1st quadrant window coordinates.  In order for the to integrate
;;; properly, there needs to be a method: window-to-OpenGL-window-transform
;;; that accounts for this difference.  Window coordinates in all TK callbacks
;;; must be mapped thru this transform.  --- lots of places for things to break.
(defun set-window-raster-transform (window)
  (glViewPort  0 0 (window-width window) (window-height window))
  (let ((window-to-ndc-matrix (1st-quadrant-window-to-ndc-matrix window)))
    (glMatrixMode GL_PROJECTION)
    (glLoadIdentity)
    (glMultMatrixd_transposed window-to-ndc-matrix)
    (glMatrixMode GL_MODELVIEW)
    (glLoadIdentity)
    ))
