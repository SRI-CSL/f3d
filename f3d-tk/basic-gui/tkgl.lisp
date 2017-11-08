(in-package :tk)

;;; This is needed here since tk-pkg cannot import these before the gl package exists.
(eval-when (eval load compile)
  (import '(gl::glmakecurrent gl::glswapbuffers gl::color-name-to-gl)))


#|
;;; color name conversion
(tcl-list-to-lisp (tcl-cmd `(winfo rgb ,(gui::widget (gui::view-window (gui::top-view t))) "turquoise")))
(tcl-list-to-lisp (tcl-cmd '(winfo rgb .frm.f2.gl2 "turquoise")))
(color-name-to-gl "turquoise" ".frm.f2.gl2")
(color-name-to-gl :red)
|#


(defparameter *uninitialized-gl-color-vector-alist* nil)

(defun set-uninitialized-gl-color-vectors ()
  (loop while *uninitialized-gl-color-vector-alist*
	for (color tk-window vector) = (pop *uninitialized-gl-color-vector-alist*)
	do (%color-name-to-gl color tk-window vector)))

;;; This sucks -- cannot call %color-name-to-gl until tcl/tk is started.
;;;(st:add-system-initialization :tkgl '(set-uninitialized-gl-color-vectors))


(push #'set-uninitialized-gl-color-vectors tk::*tcl-tk-after-init-hooks*)

;;; CC mod (8/27/2010) - remove tk dependency from tk-color-rgb -
;;; function is now a misnomer, but performs a lookup on an internal
;;; mapping of color names to rgb values.

;;; We have way too many paths to getting RGB from color names...

;;;
;;; Why do these need to be single floats?  Why do we mix single and
;;; double float colors??
#+old
(defun %color-name-to-gl (color-name tk-window vector)
  (let ((spec (tk-color-rgb color-name))
        (k 255.0d0)
	;;(k (/  (float #x10000 1.0d0))))  ;; (float #x10000 1.0f0))))
        )
    ;; (declare (single-float k))
    (declare (double-float k))
    (if (eql (length spec) 3)
	(destructuring-bind (r g b) spec
	  (setf (aref vector 0) (* k r)
		(aref vector 1) (* k g)
		(aref vector 2) (* k b)
		(aref vector 3) 1.0d0) ;; 1.0f0)
	  vector)

	(progn (format t "The color ~a is unknown~%" color-name)
	       nil)
	)))


;;; Explicit dependence on Tcl/Tk to tell us what the colors are...  In theory,
;;; the tk-window parameter allows X-server and physical screen dependencies to
;;; be taken into account

;;; The color-vectors returned are considered to be immutable.
#+old
(defun-cached color-name-to-gl (color-name &key (tk-window "."))
  (let ((vector (make-array 4 :element-type 'double-float))) ;; was single-float
    (if (tk::tcl-tk-initialized-p)
	(%color-name-to-gl color-name tk-window vector)
	(push (list color-name tk-window vector) *uninitialized-gl-color-vector-alist*))
    vector))


;;;
;;; The tk-window arg has been removed since it's no longer needed.  I
;;; think there's only one caller for this function, so it should be safe:
(defun %color-name-to-gl (color-name vector)
  (let ((spec (lx::color-name-to-3d color-name)) ;; this produces a triplet suitable for calling glColor3d()
        )
    (if (eql (length spec) 3)
	(destructuring-bind (r g b) spec
	  (setf (aref vector 0) r
		(aref vector 1) g
		(aref vector 2) b
		(aref vector 3) 1.0d0) ;; 1.0f0)
	  vector)

	(progn (format t "The color ~a is unknown~%" color-name)
	       nil)
	)))


(defun-cached color-name-to-gl (color-name &key (tk-window "."))
  (let ((vector (make-array 4 :element-type 'double-float))) ;; was single-float
    (%color-name-to-gl color-name vector)
    vector))
