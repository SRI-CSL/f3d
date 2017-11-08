(in-package :gl)

;;;
;;; OpenGL is a shared resource.  When multiprocessing is used, we
;;; need to protect GL calls.

#+sb-thread
(defvar *opengl-mutex* (sb-thread::make-mutex :name "opengl-mutex"))

#-sb-thread
(defmacro with-gl-locked (&body body)
  `(progn ,@body))

#+sb-thread
(defmacro with-gl-locked (&body body)
  `(sb-thread::with-recursive-lock (*opengl-mutex*)
     ,@body))



;;; It's probably best to include mutexes in this form for those Lisps
;;; that support threading.

(defmacro with-gl-window ((window) &body body)
  `(with-gl-locked
     (unwind-protect
	  (progn
	    (glMakeCurrent ,window)
	    ,@body
	    (glFlush)
	    )
       (progn
         (glFinish)
	 (glUnMakeCurrent ,window)))))


