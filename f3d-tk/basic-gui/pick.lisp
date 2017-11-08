(in-package :gui)

;;; Contains direct dependencies on OpenGL.
;;; Contains no direct dependencies TK.

;;; Begin Connolly code -- should this move to elsewhere?

;;; returns values window, (cv window-x window-y 0.0), mouse-event
;;;(defun pick-a-window (&optional (message "Pick a Window") (cursor "hand1 yellow"))
;;;  (let ((*most-recent-buttonpress* nil)
;;;        (*inhibit-mouse-actions* t))
;;;    (declare (special *most-recent-buttonpress* *inhibit-mouse-actions*))
;;;    (unwind-protect
;;;         (with-documentation (message)
;;;           (set-mouse-cursor-on-all-windows cursor) ; really want to do this to all frames
;;;           (loop while (or (not *most-recent-buttonpress*)
;;;                           (not (typep (car *most-recent-buttonpress*) 'tiled-window-panel)))
;;;                 do (tk::do-events)))
;;;      (set-mouse-cursor-on-all-windows ""))
;;;    
;;;    (values (widget-window (second *most-recent-buttonpress*))
;;;            (nth 3 *most-recent-buttonpress*) ; window-pos
;;;            (nth 2 *most-recent-buttonpress*) ; mouse-event
;;;            )))

;;; returns values window, (cv window-x window-y 0.0), mouse-event


;;; Winblows does NOT allow the color to be specified in the cursor:
;;;
(defvar *default-selection-cursor* #-mswindows "hand1 yellow" #+mswindows "hand1")

(defun gl-widget-name-p (widget-name)
  (let ((n (- (length widget-name) 3)))
    (string= widget-name "_gl" :start1 n)))

(defun pick-a-window (&optional (message "Pick a Window") (cursor *default-selection-cursor*))
  (let ((*most-recent-buttonpress* nil)
	(*inhibit-mouse-actions* t))
    (declare (special *most-recent-buttonpress* *inhibit-mouse-actions*))
    (unwind-protect
	 (with-documentation (message)
	   (set-mouse-cursor-on-all-windows cursor) ; really want to do this to all frames
	   (tk::handle-events-until
	    #'(lambda () (and *most-recent-buttonpress*
			      ;;(typep (car *most-recent-buttonpress*) 'tiled-window-panel)
			      (gl-widget-name-p (second *most-recent-buttonpress*) )
			      ))
	    nil))
      (set-mouse-cursor-on-all-windows ""))
    (values (widget-window (second *most-recent-buttonpress*))
	    (nth 3 *most-recent-buttonpress*) ; window-pos
	    (nth 2 *most-recent-buttonpress*) ; mouse-event
	    )))

(defun pick-an-object (&optional (message "Pick an Object") 
		       &key 
		       predicate 
		       (cursor *default-selection-cursor*))
  (let (;(*inhibit-mouse-actions* t)
	(interactor *interactor*))
    (declare (special *inhibit-mouse-actions*))
    (unwind-protect
	 (with-documentation (message)
	   (set-mouse-cursor-on-all-windows cursor) ; really want to do this to all frames
	   (setf (selected-objects interactor) nil)
	   (tk::handle-events-until
	    #'(lambda () 
		(unless (drag-op interactor) ; wait until drag-selection is complete
		  (let ((selected-object (selected-object interactor)))
		    (and selected-object 
			 (or (null predicate) (funcall predicate selected-object))))))
	    nil)
	   (selected-object interactor))
      (set-mouse-cursor-on-all-windows "")
      )))

(defun pick-an-object-or-view (&optional (message "Pick an Object of View") 
		       &key 
		       predicate 
		       (cursor *default-selection-cursor*))
  (let ((*most-recent-buttonpress* nil)
	;(*inhibit-mouse-actions* t)
	(interactor *interactor*))
    (declare (special *inhibit-mouse-actions*))
    (unwind-protect
	 (with-documentation (message)
	   (set-mouse-cursor-on-all-windows cursor) ; really want to do this to all frames
	   (setf (selected-objects interactor) nil)
	   (tk::handle-events-until
	    #'(lambda () 
		(unless (drag-op interactor) ; wait until drag-selection is complete
		  (let ((selected-object (selected-object interactor)))
		    (and (or selected-object 
			     (and *most-recent-buttonpress*
				  (typep (car *most-recent-buttonpress*) 'tiled-window-panel)))
			 (or (null predicate) (funcall predicate selected-object))))))
	    nil)
	   (or (selected-object interactor)
	       (values (widget-window (second *most-recent-buttonpress*))
		       (nth 3 *most-recent-buttonpress*) ; window-pos
		       (nth 2 *most-recent-buttonpress*) ; mouse-event
		       )))
      (set-mouse-cursor-on-all-windows "")
      )))

;(setq *obj* (pick-an-object))
;(setq *obj* (pick-an-object "Pick an Extruded Object" :predicate #'(lambda (obj) (typep obj 'obj::extruded-object))))
;(selected-object *interactor*)
;(describe *interactor*)
;(set-documentation "")

(defun pick-a-pane (&optional (message "Pick a Pane"))
  (pick-a-window message))

(defun pick-a-view (&optional (message "Pick a View"))
  (top-view (pick-a-window message)))

(defun pick-an-image (&optional (message "Pick an Image"))
  (let ((view (pick-a-view message)))
    (when view (view-image view))))

;;; returns values window, (cv window-x window-y 0.0), mouse-event
(defun pick-a-point (&optional (message "Pick a Point") (cursor "tcross yellow"))
  (pick-a-window message cursor))

;;; End Connolly code -- should this move to elsewhere?



;;; ****************************  GL OBJECT PICKING  ******************************

#|
*OBJECT-SELECT-ID-HT* contains mappings from integers to actual objects.
Integers that fail to map are assumed to indicate object fragments.
Thus, *OBJECT-SELECT-ID-COUNTER* must start at a value not used to indicate
object fragments.
|#

(defparameter *object-select-name-first-object-id* 1000000)
(defparameter *object-select-name-counter* *object-select-name-first-object-id*)

(defparameter *object-select-name-ht* (make-hash-table :test 'equal))

(defun get-object-select-name (object)
  (or (get-prop object :selectid)
      (setf (get-prop object :selectid)
	    (let ((id (incf *object-select-name-counter*)))
	      (setf (gethash id *object-select-name-ht*) object)
	      id))))

;;;(defun find-object-with-selectid (name-stack)
;;;  (if (and (consp name-stack) (null (cdr name-stack)))
;;;      (gethash (car name-stack) *object-select-name-ht*)
;;;      (loop for id in name-stack
;;;            collect (gethash id *object-select-name-ht*))))

;;;(defun find-object-with-selectid (name-stack)
;;;  (let ((objs (loop for name in name-stack
;;;                    collect (if (< name *object-select-name-first-object-id*)
;;;                                name ; fragment id
;;;                                (gethash name *object-select-name-ht*)))))
;;;    (values (loop for obj in objs
;;;                  unless (typep obj 'fixnum)
;;;                    return obj) ; first non-fixnum on name-stack
;;;            objs)))
		
(defparameter *find-object-with-selectid-verbose* nil)
;(setq *find-object-with-selectid-verbose* t)

(defun find-object-with-selectid (name-stack)
  (let ((top-object (loop for name in name-stack
			  when (>= name *object-select-name-first-object-id*)
			    return (gethash name *object-select-name-ht*)))
	(fragment-id (and (typep (car name-stack) 'fixnum) (car name-stack))))
    (when *find-object-with-selectid-verbose*
      (format t "find-object-with-selectid ~a ~a~%" top-object fragment-id))
    (values top-object fragment-id)))
	

;;;(defun set-selection-projection-matrix (x y size 2d-to-window-matrix)
;;;  (let ((vp (make-int-vector 4))
;;;        (2d-to-ndc (make-array '(4 4) :element-type 'double-float)))
;;;    (glGetIntegerv GL_VIEWPORT vp)
;;;    (set-2d-to-ndc-matrix 2d-to-window-matrix)
;;;    (glGetMatrix GL_PROJECTION_MATRIX 2d-to-ndc)
;;;    (glMatrixMode GL_PROJECTION)
;;;    (glLoadIdentity)
;;;    (gluPickMatrix (float x) (float (- (aref vp 3) y) ) size size vp)
;;;    (glMultMatrix 2d-to-ndc)))

;;; Used by draw-object methods to determine whether to fill interiors of polygons.
(defvar *object-selection-mode* nil)

;;; This is incomplete.  Needs a way to control which objects are
;;; mouse-sensitive.  Would be desirable to group objects into display lists
;;; according to mouse-sensitivity.

(defparameter *object-selection-window-size* 8.0)
;;(defparameter *object-selection-window-size* 2.0)
(defparameter *object-vertex-selection-window-size* 4.0)


#|
drag-select-object depends on (selected-objects interactor) being cleared
by highlight-selected-objects when no objects qualify for selection.
Otherwise, the selected-objects would be excluded from being drawn by 
|#

(defparameter *select-buffer-size* 128)

(defvar *select-buffer* nil)
;(setq *select-buffer-size* 128 *select-buffer*  nil)

;;; *gl-selection-pick-matrix-params* is used by some draw and draw-fragments methods.
(defvar *gl-selection-pick-matrix-params* nil)

;;; This is a drag-op
(defmethod drag-select-object (object interactor)
  (declare (ignore object))
  (with-class-slot-values interactor (current-window current-window-pos ) interactor
    (let* ((window-pos current-window-pos)
	   (win current-window)
	   (view (top-view win))
	   (*current-view* view)
	   (*transform-vertices-projection* (transform-vertices-projection view)))
      (when (and view (2d-to-window-matrix view))
	(bind-vector-elements (x y) window-pos
	  (let* ((bufsize *select-buffer-size*)
		 (selectbuf (or *select-buffer*
				(setq *select-buffer*
				      (make-array bufsize :element-type '(unsigned-byte 32)
						  :initial-element 0))))
		 (hits 0)
		 ;; prevent disruption of display-lists
		 (*build-display-list-no-test* t))
	    
	    (gl::with-gl-window (win)
	      (unwind-protect
		   ;; Some objects control selection using
		   ;; *object-selection-mode*, but it has no influence on objects
		   ;; in a display-list.  *object-selection-mode* should be eliminated.
		   (let* ((*object-selection-mode* t)
			  (selection-size *object-selection-window-size*)
			  ;; *gl-selection-pick-matrix-params* is needed by wierd objects 
			  ;; such as conjugate-point-object, that use WITH-GL-WINDOW-TRANSFORM
			  (*gl-selection-pick-matrix-params* (list x y selection-size))
			  )
		     ;; (glMakeCurrent win)
		     (glPushAttrib GL_ALL_ATTRIB_BITS)
		     (set-default-graphics-attributes view) ; is this really needed?
		     ;;(glGetIntegerv GL_VIEWPORT vp)
		     (glSelectBuffer bufsize selectbuf)
		     (glRenderMode GL_SELECT)
		     (glInitNames)
		     (glPushName 0)
		     (loop for object-set in (object-sets view)
			   when (sensitive-p view object-set)
			     do (pick-from-object-set view object-set interactor x y selection-size))
		   
		     (glFlush))
		;; CLEAN-UP FORM 
		(progn  (setq hits (glRenderMode GL_RENDER))
			(glPopAttrib))))

	    (when (< hits 0) ; selection buffer overflow
	      ;; double the size of the buffer
	      (setq *select-buffer-size* (* 2 *select-buffer-size*)
		    *select-buffer* nil)
	      (format t "drag-select-object select-buffer overflow, reallocate with ~a elements.~%"
		      *select-buffer-size*) 
	      )
	    
	    ;;(when (> hits 0) (format t "drag-select-object hits=~a~%" hits))
	    ;;(format t "drag-select-object hits=~a~%" hits)
	    (let* ((*build-display-list-no-test* t))
	      (highlight-selected-objects
	       view (process-gl-selection-hits view x y hits selectbuf))
	      )))))))

#|
Must establish some conventions about the use of the name-stack.  Obviously,
composite-objects use the name-stack to indicate the composite-hierarchy.
Primitives elsments of basic-objects, such as vertices and lines are at the
top of the name stack.  Thus the name-stack looks like:

    top:          [object fragment - vertex or line]
                  basic-object
                  [composite-object]
    bottom:       [composite-object]

|#

(defvar *process-gl-selection-hits-last-objs*)
(defparameter *process-gl-selection-hits-verbose* nil)
;(setq *process-gl-selection-hits-verbose* t)

(defun process-gl-selection-hits (view x y nhits selectbuf &optional 
				  (verbose *process-gl-selection-hits-verbose*))
  (setq x (dfloat x) y (dfloat y))
  (when (> nhits 0)
    (when (and verbose (> nhits 0))
      (format t "process-gl-selection-hits ~d hits: ~%" nhits))
    
    (flet ((zcalc (z) (* (/ z #xffffffff))) ; OpenGL examples use #x7fffffff
	   )
      (loop with ptr = 0
	    with (obj frag)
	    for i from 0 below nhits
	    for name-cnt = (aref selectbuf ptr)
	    for z1 = (zcalc (aref selectbuf (+ ptr 1)))
	    for z2 = (zcalc (aref selectbuf (+ ptr 2)))
	    do (let* ((name-stack (loop repeat name-cnt
					for j downfrom (+ ptr 2 name-cnt ) ; top-to-bottom order
					collect (aref selectbuf j))))
		 (multiple-value-setq (obj frag) (find-object-with-selectid name-stack)))
			      
	       (incf ptr (+ 3 name-cnt))
		
	       
	    when (and obj (selectable-p obj))
	      collect (list obj frag) into objfrags

	    finally (return (process-multiple-selection-hits view x y objfrags))))))

(defparameter *process-multiple-selection-hits-ht* (make-hash-table ))

;;; This will (eventually) allow objects to decline sensitivity.
(defmethod fragments-open ((object obj::basic-gl-object))
  t)

(declaim (special *process-gl-fragment-selection-hits*))

(defmethod with-gl-object-matrices (object view fn)
  (obj::with-gl-object-drawing-int object view nil 
				   #'(lambda () (funcall fn object))))
       
(defun process-multiple-selection-hits (view x y objfrags)
  (when objfrags
    (let* ((closed-objs nil)
	   (open-objs nil))
    
      (loop for entry in objfrags	; objs
	    for obj = (car entry)
	    unless (fragments-open obj)
	      do (push entry closed-objs)
	    else do (push entry open-objs))
      (setq *process-gl-fragment-selection-hits* 

	    (nconc (when open-objs
		     (let* ((win (view-window view))
			    (size *object-vertex-selection-window-size*)
			    (*gl-selection-pick-matrix-params* (list x y size))
			    (bufsize 100)
			    (selectbuf (make-array bufsize :element-type '(unsigned-byte 32)
						   :initial-element 0))
			    (hits 0))
		       (gl::with-gl-window (win)
			 (unwind-protect
			      (let ()		
				;; (glMakeCurrent win)
				(glPushAttrib GL_ALL_ATTRIB_BITS)
				(glSelectBuffer bufsize selectbuf)
				(glRenderMode GL_SELECT)
				(glInitNames)
				(glPushName 0)
				(loop for (obj) in objfrags
				      do (pick-object-fragment obj view x y size))	
				(glFlush))
			   ;; clean-up form 
			   (progn (setq hits (glRenderMode GL_RENDER))
				  (glPopAttrib)))
			 )
		       ;;(format t "process-multiple-selection-hits ~a~%" hits)
		       (process-gl-fragment-selection-hits view x y hits selectbuf)))

		   closed-objs)))))

(defmethod pick-object-fragment (obj view x y size)
  (when (set-gl-matrices (world obj) view)
    (set-gl-pick-matrix x y size)
    (glLoadName (get-object-select-name obj))
    (with-gl-object-matrices obj view #'obj::draw-fragments)))
  

(defun process-gl-fragment-selection-hits (view x y nhits selectbuf)
  (flet ((zcalc (z) (* (/ z #xffffffff))) ; OpenGL examples use #x7fffffff
	 )
    (loop with ptr = 0
	  with (obj frag)
	  for i from 0 below nhits
	  for name-cnt = (aref selectbuf ptr)
	  for ndc-z1 = (zcalc (aref selectbuf (+ ptr 1)))
	  for ndc-z2 = (zcalc (aref selectbuf (+ ptr 2)))
	  ;; pick-position is a combination of window-x window-y and ndc-z
	  ;; NO!  The 4x4-projections are now compatible with the OpenGL depth coordinate
	  ;;          We must undo the ndc-z mapping:  z = 2*ndc-z -1
	  ;;          for z1 = (- (* 2.0 ndc-z1) 1.0)
	  for z1 = ndc-z1
	  do (let* ((name-stack (loop repeat name-cnt
				      for j downfrom (+ ptr 2 name-cnt )  ; top-to-bottom order
				      collect (aref selectbuf j))))
	       (multiple-value-setq (obj frag) (find-object-with-selectid name-stack)))
			      
	     (incf ptr (+ 3 name-cnt))
	       
	  when obj
	    collect (list obj frag z1)
	  ;;(project-pick-to-object obj (cv x y z1) view)
	  
	      into objfrags

	  finally
       (return (most-specific-object-selection-hits view x y objfrags)))))

(defvar *object-selection-hits-ht* (make-hash-table))

(defun most-specific-object-selection-hits (view x y objfrags)
  (let ((ht *object-selection-hits-ht*))
    (clrhash ht)
    ;; first find all of the different objects in objfrags
    (loop for objfrag in objfrags
	  for (obj . rest) = objfrag
	  do (push objfrag (gethash obj ht))
	  ;;do (setf (gethash obj ht) (cons objfrag (gethash obj ht)))
	  )
    ;; next build a list of objects and for each object the most specific objfrag
    (loop for objfrags2 being the hash-values of ht
	  for (obj . rest) = (car objfrags2)
	  for frags+z = (loop for objfrag in objfrags2
			      collect (cdr objfrag))
	  collect (list obj (obj::make-fragment-descr obj view x y frags+z))
	  )))

(defparameter *default-selection-graphics-style* 
  (make-instance 'graphics-style :line-width 2 :color :green))

(defmethod selection-graphics-style (&optional object view)
  (ignore object view)
  *default-selection-graphics-style*)

;;;(defun highlight-selected-objects (view objects-and-positions)
;;;  (ignore view)
;;;  (unless (and (null (selected-objects)) (null objects-and-positions))
;;;    (let ((objects (loop for (obj) in objects-and-positions collect obj))
;;;          (*highlighting-selected-objects* t)
;;;          (*force-graphics-style* (selection-graphics-style))
;;;          (views nil)
;;;          (previous-views (previously-highlighted-views))
;;;          )
;;;      (format t "highlight-selected-objects ~a ~a~%" (selected-objects) objects)
;;;      ;; One side-effect of drag-select-object is to set the selected-window.
;;;      ;; I do not know why it is conditional.
;;;      (when (null objects) (setf (selected-window *interactor*) (view-window view)))
;;;      (unless (equal (selected-objects) objects)
;;;        ;; This sucks
;;;        ;;(format t "highlight-selected-objects~%")
;;;        (loop for obj in objects
;;;              for world = (world obj)
;;;              do (map-over-active-world-views (world view)
;;;                                              (pushnew view views)))
;;;        (setf (previously-highlighted-views) views)
;;;        (setf views (union views previous-views))
;;;
;;;        
;;;        (setf (selected-objects) objects-and-positions)
;;;      
;;;        ;;(format t "highlight-selected-objects ~a~%" objects)
;;;        (loop for view in views
;;;              do (redisplay view :from-backing-store t
;;;                            :update-backing-store nil))
;;;        )
;;;      (throw 'bypass-redisplay nil))))

;;; This also calls (setf (selected-objects) objects-and-positions)
(defun highlight-selected-objects (view objects-and-positions)
  (ignore view)
  (unless (and (null (selected-objects)) (null objects-and-positions))
    (let ((objects (loop for (obj) in objects-and-positions collect obj))
	  (*highlighting-selected-objects* t)
	  (*force-graphics-style* (selection-graphics-style))
	  (views nil)
	  (previous-views (previously-highlighted-views))
	  )
      ;;(format t "highlight-selected-objects ~a ~a ~a~%" view (selected-objects) objects)
      ;; One side-effect of drag-select-object is to set the selected-window.
      ;; I do not know why it is conditional.
      (when (null objects) (setf (selected-window *interactor*) (view-window view)))
      (unless (equal (selected-objects) objects)
	;; This sucks
	;;(format t "highlight-selected-objects~%")
	(loop for obj in objects
	      for world = (world obj)
	      do (map-over-active-world-views (world view)
					      (pushnew view views)))
	(setf (previously-highlighted-views) views)
	(setf views (union views previous-views))

	(let* ((update-backing-store (and (selected-objects) (null objects-and-positions)))
	       (*build-display-list-no-test* (not update-backing-store)))
	  ;; When deselecting objects, particularly with free motion select (no mouse buttons
	  ;; pressed), we must force full redisplay of all of the views. THIS IS GETTING UGLY.  TOO
	  ;; MANY GLOBALS AND WEIRD STATE TRANSITION RULES.  Lots of works still needed to get
	  ;; things right.
	  
	  (setf (selected-objects) objects-and-positions)
      
	  ;;(format t "highlight-selected-objects ~a ~a~%" objects update-backing-store)
	  (loop for view in views
		do (redisplay view :from-backing-store (not update-backing-store)))
	  )
	(update-doc-line2)
	(throw 'bypass-redisplay nil)))))

(defun unhighlight-selected-objects ()
  ;;(format t "unhighlight-selected-objects~%")
  (setf (selected-objects) nil)
  (let* ((interactor *interactor*)
	 (redisplay-required
	  ;; Must compute this list first, since redisplay changes the object-sets,
	  ;; making it impossible to determine if other than the first view
	  ;; requires redisplay.
	  (loop for view in (previously-highlighted-views)
		collect (redisplay-required view interactor))))
    (loop for view in (previously-highlighted-views)
	  for redisplay in redisplay-required
	  ;; what we are doing here is "adding" the selected-objects
	  ;; back into the backing-store.
	  do (redisplay view :from-backing-store (not redisplay))
	  )
    (setf (previously-highlighted-views) nil)
    (update-doc-line2)
    (glFlush)
    ))


;;;(defun update-doc-line2 ()
;;;  (let ((sel (selected-objects)))
;;;    (if sel
;;;        (let* ((obj (caar sel))
;;;               (frag (cadar sel))
;;;               (name (name obj))
;;;               (frag-descr
;;;                (typecase frag
;;;                  (obj::object-arc
;;;                   (with-class-slot-values obj::object-arc
;;;                         (start-vertex-id end-vertex-id pick-percent) frag
;;;                     (format nil "arc(~a ~a ~1,3f)" start-vertex-id end-vertex-id pick-percent)))
;;;                  (obj::object-vertex
;;;                   (format nil "vertex ~a" (obj::vertex-id frag)))
;;;                  (otherwise ""))))
;;;          (set-documentation2
;;;           (format nil "~a ~a" name frag-descr)))
;;;        (set-documentation2 "")
;;;        )))

;(selected-objects)

(defun update-doc-line2 ()
  (let ((sel (selected-objects)))
    (cond ((> (length sel) 1)
	   (set-documentation2 (format nil "~d objects are selected" (length sel))))
	  (sel
	   (let* ((obj (caar sel))
		  (frag (cadar sel))
		  (name (or (name obj) (obj::short-name-string obj)))
		  (frag-descr
		   (typecase frag
		     (obj::object-arc
		      (with-class-slot-values obj::object-arc
			(start-vertex-id end-vertex-id pick-percent) frag
			(or (ignore-errors (format nil "arc(~a ~a ~1,5f)" start-vertex-id end-vertex-id pick-percent))
			    "ERR")))
		     (obj::object-vertex
		      (format nil "vertex ~a" (obj::vertex-id frag)))
		     (otherwise ""))))
	     (set-documentation2
	      (format nil "~a ~a" name frag-descr))))
	  (t (set-documentation2 "")))))
