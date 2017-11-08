(in-package :gui)

(defvar obj::*object-feedback-mode* nil)

;;; OpenGL feedback produces OpenGL 2d (window) coordinates with origin in bottom-left rather than
;;; top-left.  I have chosen top-left window coordinates as standard because of Tk and X-windows
;;; conventions, so we must flip-y.
(defun flip-window-y (window vector)
  (let ((height (window-height window)))
    (declare (type double-float height))
    (bind-vector-elements (x y) vector
      (cv x (- height y 1.0)))))

(defun mkvects (coords)
  (loop for (x y) on coords by #'cddr
	collect (cv (dfloat (the single-float x)) (dfloat (the single-float y)))))

;;; *feedback-buffer* should be a slot in class interactor.
;;; Needs to be a list of the form: (view feedback-buffer).
;;; 
(defvar *feedback-buffer* nil)

;;; old version
(defmethod get-feedbuck-buffer ((interactor interactor) view)
  (or *feedback-buffer*
      (setq *feedback-buffer* (build-feedback-list interactor (view-window view)))))
  
(defmethod get-feedbuck-buffer ((interactor interactor) view)
  (let ((feedback (feedback-cache interactor)))
    (if (and feedback (eq (car feedback) view))
	(cadr feedback)
	(let ((feedback-buffer (build-feedback-list interactor (view-window view))))
	  (setf (feedback-cache interactor)
		(list view feedback-buffer))
	  feedback-buffer))))
	      

;;; This is lots faster than the version of DRAG-SELECT-OBJECT defined in pick.lisp because the
;;; *feedback-buffer* is generated only once at the BUTTONPRESS event and the subsequent MOTION
;;; events are relatively cheap to process since very few objects are involved.
(defmethod drag-select-object ((interactor interactor)
			       &optional
			       ;;(sensitivity-radius 10.0)
			       (sensitivity-radius (* 2.0 *object-selection-window-size*))
			       )
  (with-class-slot-values interactor (current-window current-window-pos ) interactor
    (let* ((window-pos current-window-pos)
	   (win current-window)
	   (view (top-view win)))
      (when view
	(let* ((objs (nearest-objects-in-feedback-list win window-pos
						       (get-feedbuck-buffer interactor view)
						       sensitivity-radius)))
	  ;;(format t "drag-select-object ~a ~a~%" win objs)
	      (bind-vector-elements (x y) window-pos
	    (let* ((*build-display-list-no-test* t))
	      (highlight-selected-objects
	       view
	       (process-multiple-selection-hits view x y
						(loop for obj in objs collect (list obj)))))))))))

(defmethod build-feedback-list ((interactor interactor) win)
  (let* ((view (top-view win))
	 (bufsize 100000)
	 (feedback-buf (make-array bufsize :element-type 'single-float :initial-element 0.0f0))
	 feedback-size
	 (old-selected-objects (selected-objects interactor))
	 (*transform-vertices-projection* (transform-vertices-projection view)))
    (gl::with-gl-window (win)
      (unwind-protect
	   (let ((obj::*object-feedback-mode* t)
		 (*always-immediate-render-p* t)
		 ;;(*build-display-list-no-test* t)
		 (*draw-objects-with-selectid* t)
		 )		
	     ;;(format t "build-feedback-list ~%")
	     (setf (selected-objects interactor) nil)
	     ;; (glMakeCurrent win)
	     (glPushAttrib GL_ALL_ATTRIB_BITS)
	     (set-default-graphics-attributes view) ; is this really needed?
	     ;;(glPolygonMode GL_FRONT GL_FILL) (glPolygonMode GL_BACK GL_FILL)
	     (glFeedbackBuffer bufsize GL_2D feedback-buf)
	     (glRenderMode GL_FEEDBACK)
	     (loop for object-set in (object-sets view)
		   do  (display-object-set view object-set interactor))		   
	     (glFlush))
	;; clean-up form 
	(progn (setq feedback-size (glRenderMode GL_RENDER))
	       (setf (selected-objects interactor) old-selected-objects)
	       (glPopAttrib))))
    
    (parse-feedback-list feedback-buf feedback-size)))


(defun parse-feedback-list (feedback-buf feedback-size)
  (declare (type (simple-array single-float (*)) feedback-buf))
  (declare (type fixnum feedback-size))
  (flet ((mkvec (x y) (cv (dfloat x) (dfloat y))))
    (loop with PASS-THRU-TOKEN single-float = (float GL_PASS_THROUGH_TOKEN 0.0f0)
	  with LINE-TOKEN single-float = (float GL_LINE_TOKEN 0.0f0)
	  with LINE-RESET-TOKEN single-float = (float GL_LINE_RESET_TOKEN 0.0f0)
	  with POINT-TOKEN single-float = (float GL_POINT_TOKEN 0.0f0)
	  with POLYGON-TOKEN single-float = (float GL_POLYGON_TOKEN 0.0f0)
	  with obj
	  with i fixnum = 0
	  while (< i feedback-size)
	  for token = (aref feedback-buf i)
	  when (= token PASS-THRU-TOKEN)
	    do (let ((id (round (aref feedback-buf (+ i 1)))))
		 ;;(setq obj (aref feedback-object-table id))
		 (setq obj (or (gethash id *object-select-name-ht*) id)))
	       (incf i 2)
	  else when (or (= TOKEN LINE-TOKEN) (= token LINE-RESET-TOKEN))
		 collect (list :line obj
				     (mkvec (aref feedback-buf (+ i 1)) (aref feedback-buf (+ i 2)))
				     (mkvec (aref feedback-buf (+ i 3)) (aref feedback-buf (+ i 4))))
		 and do (incf i 5)
	  else when (= token POINT-TOKEN)
		 collect (list :point obj (mkvec (aref feedback-buf (+ i 1)) (aref feedback-buf (+ i 2))))
		 and do (incf i 3)
	  else when (= token POLYGON-TOKEN)
		 when (typep obj 'obj::ribbon)
		   ;; Ribbons generate lines for centerline and polygons for area covered.
		   ;; Skip over the centerline.
		   do (incf i (+ 2 (* 2 (round (aref feedback-buf (+ i 1))))))
	         else
		   collect (list* :polygon obj
					   (loop with n fixnum = (round (aref feedback-buf (+ i 1)))
						 repeat n
						 initially (incf i 2)
						 collect (mkvec (aref feedback-buf i) (aref feedback-buf (1+ i)))
						 do (incf i 2)))

	  else do (error "unrecognized feedback token: ~a" token)

	  )))

(defun 2d-point-to-line-distance (vect vect1 vect2 d)
  (declare (optimize (speed 3) (safety 0)))
  (declare (double-float d))
  (and vect1 vect2
       (bind-vector-elements (u1 v1) vect1
	 (bind-vector-elements (u2 v2) vect2
	   (bind-vector-elements (u v) vect
	     (and (>= u (- (min u1 u2) d)) ; bounding box tests
		  (>= v (- (min v1 v2) d))
		  (< u (+ (max u1 u2) d))
		  (< v (+ (max v1 v2) d))
		    
		  (let* ((du (- u2 u1))
			 (dv (- v2 v1))
			 (d (+ (^2 du) (^2 dv)))
			 (s (/ (+ (* (- u u1) du)
				  (* (- v v1) dv))
			       (if (zerop d) 1.0 d)))
			 (u3 (+ u1 (* s du)))
			 (v3 (+ v1 (* s dv))))
		    (declare (double-float du dv d s u3 v3))
		    (when (<= 0.0 s 1.0)
		      (+ (^2 (- u u3)) (^2 (- v v3))))))
	     )))))
#|
(disassemble '2d-point-to-line-distance)
|#

(defun nearest-objects-in-feedback-list (window window-pos feedback-list sensitivity-radius)
  (declare (optimize (safety 1)(speed 3)))
  #+cmu (declare (ext:optimize-interface (safety 3)(speed 0)))
  (declare (inline ^2) (ftype (function (double-float) double-float ) ^2))
  (declare (ftype (function (number double-float) double-float ) float))
  (let ((wp (flip-window-y window window-pos)))
    (bind-vector-elements (wx wy) wp
      (let* ((nearest-objs-ht (make-hash-table))
	     (rad (the double-float (float sensitivity-radius 0.0d0)))
	     (rad2 (^2 rad))
	     ;;(rad2 (^2 (float sensitivity-radius 0.0f0)))
	     )
	(declare (type double-float rad rad2))
	(labels ((dist2 (x y)
		   (declare (type double-float x y))
		   (+ (^2 (- wx x)) (^2 (- wy y))))
		 (pt-dist (obj pt)
		   (bind-vector-elements (x y) pt
		     (let ((dist2 (dist2 x y)))
		       (declare (type double-float dist2))
		       (when (< dist2 rad2)
			 (let ((last-hit (gethash obj nearest-objs-ht)))
			   (when (or (null last-hit)
				     (< dist2 (the double-float last-hit)))
			     (setf (gethash obj nearest-objs-ht) dist2)))))
		     nil))
		 (line-dist (obj pw p0 p1)
		   (let ((dist2 (2d-point-to-line-distance pw p0 p1 rad2)))
		     (declare (type (or null double-float) dist2))
		     ;;(setq foo (list obj pw p0 p1 dist2 rad2))
		     (when (and dist2 (< dist2 rad2))
		       ;;(format t "~a ~%" (list obj pw p0 p1 dist2))
		       (let ((last-hit (gethash obj nearest-objs-ht)))
			 (when (or (null last-hit)
				   (< dist2 (the double-float last-hit)))
			   (setf (gethash obj nearest-objs-ht) dist2)))))
		   nil)
		 (poly-dist (obj pw pts)
		   (loop for (p0 . rest) on pts by #'cdr
			 do (line-dist obj pw p0 (car (or rest pts)))))
		 )
	
	  (loop for (type obj . vects) in feedback-list
		do (case type
		     (:point (pt-dist obj (car vects)))
		     (:line (line-dist obj wp (car vects) (cadr vects)))
		     (:polygon (poly-dist obj wp vects)
		      
		      ))))
	(let ((hits
	       (loop for obj being the hash-keys of nearest-objs-ht using (hash-value val)
		     ;;do (format t "~a ~a~%" obj val)
		     collect obj)))
	  ;;(when (or t hits) (format t "window pos = ~a~%" (list wx wy)))
	  ;;(when hits (format t "nhits=~a~%" (length hits)))	(setq foo hits)
	  hits)))))




#|
(defun selected-points-window-position ()
  (loop for (obj frag) in (SELECTED-OBJECTS *interactor*)
	with view = (top-view)
	;;for verts = (obj::vertex-array obj)
	;;for vertid = (obj::VERTEX-ID frag)
	collect 
	(transform-vector (list (object-to-world-transform obj)
				(3d-to-2d-projection view)
				(2d-to-window-transform view)
				)
			  (obj::fragment-position frag))
	))   
(disassemble 'nearest-object-in-feedback-list)
(list GL_LINE_TOKEN GL_POINT_TOKEN)
(fmakunbound 'build-visibility-buckets)

(progn *print-length*)
(setq *print-length* 100000)
(setq *print-array* t)

(setq *feedback-list* (BUILD-FEEDBACK-LIST *interactor* (selected-window *interactor*))))

(current-window *interactor*)
(setq *feedback-list* (BUILD-FEEDBACK-LIST *interactor* (current-window *interactor*))))

(time (loop repeat 1000 do (nearest-object-in-feedback-list (cv 19.0 13.3) *feedback-list*)))

(time (nearest-object-in-feedback-list (selected-window *interactor*) (car (selected-points-window-position)) *feedback-list*))
(time (nearest-objects-in-feedback-list (selected-window *interactor*)(car (selected-points-window-position)) *feedback-list* 4.0))

(let* ((pos (car (selected-points-window-position)))
       (win (selected-window *interactor*))
       (feedback (BUILD-FEEDBACK-LIST *interactor* win))       
       (objs (nearest-objects-in-feedback-list win pos feedback 20.0)))
  (bind-vector-elements (x y) pos
    (process-multiple-selection-hits (top-view) x y
				     (loop for obj in objs collect (list obj)))))

(SELECTED-OBJECTS *interactor*)
(describe (car (car (SELECTED-OBJECTS *interactor*))))

(declaim (optimize (safety 0)(speed 3)))
(defun foo1 (x) (declare (type single-float x)) (^2 x))

(defun foo2 (x) (declare (type double-float x)) (^2 x))

(defun foo3 (x) (declare (type fixnum x)) (^2 x))
(defun foo3a (x) (declare (type fixnum x)) (the fixnum (^2 x)))
(defun foo3b (x) (declare (type fixnum x)) (the fixnum (* x x)))

(disassemble 'foo1)
(disassemble 'foo2)
(disassemble 'foo3)
(disassemble 'foo3a)
(disassemble 'foo3b)

(describe *interactor*)
(describe (cadr (car (SELECTED-OBJECTS *interactor*))))

(setq *print-array* t)


(Selected-points-window-position)
|#

#|
(defun tst3 (l)
  (loop for x in l
	when (< x 0)
	  when (= x -1)
	    collect 'foo
	  else collect 'bar
	else collect 'baz))

(tst3 '(-1 -2 0 1))

(declaim (inline sqr))
(defun sqr (x) (* x x))

(cv (defun tst (x)
  (declare (type number x))
  (declare (ftype (function (number single-float) single-float ) float))
  (flet ((sqr (x) (* x x)))
    (declare (inline sqr) (ftype (function (single-float) single-float ) sqr))
    (let (;;(y (sqr (the single-float (float x 0.0f0))))
	  (y (sqr(float x 0.0f0)))
	  )
      (declare (type single-float y))
      (= y 1.0f0))))

(defun tst (x)
  (declare (type double-float x))
  (declare (ftype (function (number single-float) single-float ) float))
  (flet () ;((sqr (x) (* x x)))
    (declare (inline sqr) (ftype (function (single-float) single-float ) sqr))
    (let (;;(y (sqr (the single-float (float x 0.0f0))))
	  (y (sqr (float x 0.0f0)))
	  )
      (declare (type single-float y))
      ;;(the fixnum (truncate y))
      (= y 1.0f0)
      ))))

(defun tst (x)
  (declare (type double-float x))
  (declare (ftype (function (number single-float) single-float ) float))
  (flet () ;((^2 (x) (* x x)))
    (declare (inline ^2) (ftype (function (single-float) single-float ) ^2))
    (let (;;(y (^2 (the single-float (float x 0.0f0))))
	  (y (^2 (float x 0.0f0)))
	  )
      (declare (type single-float y))
      ;;(the fixnum (truncate y))
      (= y 1.0f0)
      )))

(disassemble 'tst)
|#

#|
(defun tst (window window-pos)
  (declare (optimize (safety 1)(speed 3)))
  #+cmu (declare (ext:optimize-interface (safety 3)(speed 0)))
  ;; (declare (inline ^2) (ftype (function (double-float) double-float ) ^2))
  ;; (declare (ftype (function (number double-float) double-float ) float))
  (bind-vector-elements (wx wy) (flip-window-y window window-pos)
    (labels ((^2 (x) (declare (type double-float x))
		 (* x x)))
      (declare (inline ^2))
      (labels ((dist2 (x y)
		 (declare (type double-float x y))
		 (+ (^2 (- wx x)) (^2 (- wy y)))))
	(= (dist2 wx wy) 0.0d0)))))

(disassemble 'tst)

(disassemble 'nearest-objects-in-feedback-list)

(defun tst2 (x)
  (declare (optimize (speed 3) (safety 0)))
  ;;(declare (type double-float x))
  (declare (type float x))
  (let ((i (truncate x)))
    (declare (type fixnum i))
    i))

(disassemble 'tst2)
|#
