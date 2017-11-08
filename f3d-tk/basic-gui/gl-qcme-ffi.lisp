(in-package :gui)


(def-foreign-function (set-2d-to-ndc-matrix (:name  (freedius-prefix "set_2d_to_ndc_matrix")))
    (2d_to_window_matrix :simple-array))

(defun obj::set-2d-to-ndc-matrix (2d_to_window_matrix)
  (set-2d-to-ndc-matrix 2d_to_window_matrix))

(def-foreign-function (window_to_ndc_matrix (:name  (freedius-prefix "window_to_ndc_matrix")))
    (mat :simple-array))

#| From $FREEDIUS/c/gl/GLdisplay-image.C

// Map window coordinates (0:dim) to normalized device coordinates (-1:+1)
// Also map w/s range from 0:+1 to -1:+1.  OpenGL then remaps it back to 0:+1.
double * window_to_ndc_matrix(mat4x4 m)
{
  int width, height;
  get_window_dims(&width, &height);
  bzero(m, 16*sizeof(double));
  m[0][0] =  2.0/width;    m[0][3] = -1.0;
  m[1][1] = -2.0/height;   m[1][3] = +1.0;
  //m[2][2] = 1.0;
  m[2][2] = 2.0; m[2][3] = -1.0;      // new Sat Mar 27 2004 for mapping 0:+1 to -1:+1
  m[3][3] = 1.0;
  return((double *) m);
}

|#

;;; Gets window_to_ndc_matrix of current OpenGL window
(defun window-to-ndc-matrix ()
  (let ((win-to-ndc (make-4x4-matrix)))
    (window_to_ndc_matrix win-to-ndc)
    win-to-ndc))

(defun window-to-ndc-transform-vector (window vector &optional into-vector)
  (declare (type (simple-array double-float (*)) vector))
  (declare (type (or null (simple-array double-float (*))) into-vector))
  (unless into-vector (setq into-vector (make-coordinate-vector 3)))
  (let ((width (dfloat (window-width window)))
	(height (dfloat (window-height window))))
    (bind-vector-elements (x y z) vector
      (setf (aref into-vector 0) (+ (* (/ 2.0 width) x) -1.0)
	    (aref into-vector 1) (+ (* (/ -2.0 height) y) 1.0)
	    (aref into-vector 2) z)
      into-vector)))

;;;(defun ndc-to-window-transform-vector (window vector &optional into-vector)
;;;  (declare (type (simple-array double-float (*)) vector))
;;;  (declare (type (or null (simple-array double-float (*))) into-vector))
;;;  (unless into-vector (setq into-vector (make-coordinate-vector 3)))
;;;  (let ((width (dfloat (window-width window)))
;;;        (height (dfloat (window-height window))))
;;;    (bind-vector-elements (x y z) vector
;;;      (setf (aref into-vector 0) (* width (+ (* .5 x) .5))
;;;            (aref into-vector 1) (* height (+ (* -.5 y) .5))
;;;            (aref into-vector 2) z)
;;;      into-vector)))

(defun ndc-to-window-transform-vector (window vector &optional into-vector)
  (declare (type (simple-array double-float (*)) vector))
  (declare (type (or null (simple-array double-float (*))) into-vector))
  (unless into-vector (setq into-vector (make-coordinate-vector 3)))
  (let ((width (dfloat (window-width window)))
	(height (dfloat (window-height window))))
    
    (bind-vector-elements (x y z) vector
      (setf (aref into-vector 0) (* .5 width (+ x 1.0))
	    (aref into-vector 1) (* .5 height (- 1.0 y))
	    (aref into-vector 2) z)
      into-vector)))
#|
(disassemble 'ndc-to-window-transform-vector)
|#


;;;  ************************  TEXTURE POOL CONTROLS  ************************

;;; This need to be changed to use system timers rather than tcl/tl.

;;; Where does this belong ?

;;; Code Release GL texture objects after an idle interval since last
;;; redisplay.

(defvar *release-textures-after-delay-timer-id*)

(defparameter *release-textures-after-delay-seconds* 15)
;;(defparameter *release-textures-after-delay-seconds* 60)

(defparameter *release-textures-after-delay-tk-string*
  (format nil "after ~a {qcme_callback nil texture-release-timeout 0}"
	  (floor (* 1000 *release-textures-after-delay-seconds*))))

(defun release-textures-after-delay-callback (&rest  args)
  (declare (ignorable args))
  ;(format t "release-textures-after-delay-callback ~a ~a~%" *release-textures-after-delay-timer-id* args)
  (cancel-release-textures-after-delay)
  (img::release-image-pool-textures nil 1))

#|
(release-image-pool-textures nil 1)
|#

(defun cancel-release-textures-after-delay ()
  (when (and (boundp '*release-textures-after-delay-timer-id*)
	     *release-textures-after-delay-timer-id*)
    (lisptk::tcl-eval-internal
     (lx::string-append2
      "after cancel "
      *release-textures-after-delay-timer-id*)
     :ignore)
    ;;(format t "~a~%" (lx::string-append2 "after cancel " *release-textures-after-delay-timer-id*))
    
    (setq *release-textures-after-delay-timer-id* nil)))


;;; Better yet, avoid making tcl calls for timers.
;;; Not sure how safe lisp timer interrupts would be vis-a-vis calls to foreign code.
;;; At least tcl timers are handled synchronously.

;;; I believe this machinery is only needed on OpenGL implementations like on the SRI O2 that use
;;; and lock main memory for storing tiles.
;;(defparameter *inhibit-release-textures-after-delay* t)

(defparameter *inhibit-release-textures-after-delay* nil)


(defun release-textures-after-delay (&optional delay)
  (unless *inhibit-release-textures-after-delay*
    ;;(setq delay (floor (* 1000 (or delay *release-textures-after-delay-seconds*))))
    ;; milliseconds
    ;; texture-release-timeout must be in tk package bacause of intern problems
    ;; in handling callbacks.
    (unless (boundp '*release-textures-after-delay-timer-id*)
      (install-tk-event-handler 'tk::texture-release-timeout 
				'release-textures-after-delay-callback))
    (cancel-release-textures-after-delay)
    (setq *release-textures-after-delay-timer-id*
	  (lisptk::tcl-eval-internal
	   (if delay
	       (format nil "after ~a {qcme_callback nil texture-release-timeout 0 }"
		       (floor (* 1000 delay)))
	       *release-textures-after-delay-tk-string*)
	   nil))
    ;;(format t "release-textures-after-delay ~a~%" *release-textures-after-delay-timer-id*)
    ))
