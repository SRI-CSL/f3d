(in-package :gui)

;;; Contains many direct dependencies on OpenGL.

#|
This file performs DISPLAY of views.

Major concepts implemented here are:

   DRAG-REDISPLAY:  Display of everything but the SELECTED-OBJECTS from BACKING-STORE.

   SELECTED-OBJECTS: Set of spatial-objects that have been selected and might be
       undergoing modification.

   OBJECT-SET: Aggregation of spatial-objects.

   DISPLAY-LIST: OpenGL display list generated with glNewList and glEndList.

(load (compile-file "$F3D/lisp/basic-gui/pbuffer-display.lisp"
		    :output-file "$F3D/arch/linux-cmucl-redhat-9/lisp/basic-gui/pbuffer-display.x86f"))

(load "$F3D/arch/linux-cmucl-redhat-9/lisp/basic-gui/pbuffer-display.x86f")

|#

;;; Controls whether z-buffer is used in object drawing.
(defparameter *enable-depth-test* t)



;; *render-selected-objects-in-front* enables copying back to front instead of swapbuffers.
;; selected-objects are drawn to the front buffer instead of the back buffer.
(defparameter *render-selected-objects-in-front* t) 
#|
(setq *render-selected-objects-in-front* nil)
|#


;;; Controls whether objects being dragged are drawn using the z-buffer test.
(defparameter *enable-dynamic-object-depth-test* nil)

;;; Controls whether GL_PROJECTION matrix near/far w clipping is used.
;;; This variable is defined in gl-matrices.lisp
;;; (defparameter *disable-near-far-clipping* nil)

;;; Controls whether GL_PROJECTION matrix far z clipping is used.
;;; This variable is defiend in gl-matrices.lisp
;;;(defparameter *disable-far-clipping* nil)

#|
(setq *enable-dynamic-object-depth-test* t)
(setq *enable-depth-test* nil)
|#
;;; Do not change this -- zbuffer-backing-store does not work, but
;;; this variable is tested.
(defconstant *enable-zbuffer-backing-store* nil)

(declaim (special *gl-shading-enabled*))


;;; *************************  VIEW REDISPLAY  *************************

#|
(setq tk::*repl-event-wait-hook* #'(lambda () (format t "tk::*repl-event-hook*~%")))

;;; This needs to be added to an initialization.
(setq tk::*repl-event-wait-hook* 'redisplay-damaged-views)

(cycle-stack (widget-window ".frm.f2.gl2"))
(cycle-stack (widget-window ".frm.f1.gl1"))


|#


#-cocoa
(st::add-system-initialization
 :basic-gui '(setq tk::*repl-event-wait-hook* 'redisplay-damaged-views))

#|
REDISPLAY-DAMAGED-VIEWS is called whenever another window is dragged over a
GLXwindow, causing visibility of pixels to change.

If the backing-store is complete, then we can do (redisplay view :FROM-BACKING-STORE t).  

|#

#|
(describe (top-view))
(describe (selected-window *interactor*))
(describe (car (view-stack (selected-window *interactor*))))
(describe (cadr (view-stack (selected-window *interactor*))))
|#

(defvar *redisplaying-damaged-views* nil)

(defmethod redisplay-damaged-views (&optional (interactor *interactor*))
  (ignore interactor)
  (unless *redisplaying-damaged-views*
    (let ((*redisplaying-damaged-views* t))
      (declare (special *redisplaying-damaged-views*))
      (map-over-active-windows
	  (window)
	(when (damaged-p window)
	  (redisplay window :FROM-BACKING-STORE t)
	  (setf (damaged-p window) nil) ; there are other places that should clear damaged-p ;
	  )))))

			
(defun obj::redisplay-views-containing-objects (objs)
  (map-over-active-views (view)
      (loop with redisplay-view = nil
	    for obj-set in (object-sets view)
	    when (loop for obj in objs
		       thereis (contains-object obj-set obj))
	      do (setq redisplay-view t)
		 (invalidate obj-set)
	    finally
	 (when redisplay-view
	   (redisplay (view-window view))))))			  

(defmethod redisplay-all-world-views
	   ((interactor interactor) world &key (drag-redisplay nil))
  (unless world (setq world (world (selected-object interactor))))
  ;;(format t "redisplay-all-world-views enter~%" )
  (map-over-active-world-views
   (world view)
    ;(format t "redisplay-all-world-views ~a~%" (list world view drag-redisplay))
    (if drag-redisplay
	(drag-redisplay (view-window view) view)
	(redisplay view))))

(defmethod draw-all-object-sets (view &optional (interactor *interactor*))
  (loop for object-set in (object-sets view)
	when (and (backing-store-p object-set)
		  ;(not (immediate-render-p object-set))
		  )
	  do (display-object-set view object-set interactor)
	     ;(format t "&")
	))


(defvar *current-view* nil)

;;;
;;; Can be toggled in Preferences.
;;;

(defparameter *hidden-line-eliminate* nil)

(defparameter *enable-draw-axes* nil)
#|
(setq *enable-draw-axes* t)
|#

(defun draw-axes (&optional (len 1.0))
  (when *enable-draw-axes*
;;    (glDrawBuffer GL_BACK) ;; why were we doing this?
    (glDisable GL_DEPTH_TEST)
    (glColor3d 1.0 0.0 0.0)
    (glBegin GL_LINES)
    (glVertex3d 0.0 0.0 0.0)
    (glVertex3d len 0.0 0.0)
    (glVertex3d 0.0 0.0 0.0)
    (glVertex3d 0.0 len 0.0)
    (glVertex3d 0.0 0.0 0.0)
    (glVertex3d 0.0 0.0 len)
    (glEnd)))

(defparameter *checking-gl-errors* nil)

(defun check-gl-errors (string)
  (when *checking-gl-errors*
    (glFlush)
    (handle_gl_errors string)))

;;; This probably isn't needed
(defun OpenGL-reset-state2 ()
  (when t
    (glDisable GL_STENCIL_TEST)
    (glDisable GL_DEPTH_TEST)
    (glDisable GL_ALPHA_TEST )
    (glDisable GL_BLEND)
    (glDisable GL_COLOR_LOGIC_OP)
    ))

#+never
(defun OpenGL-reset-state ()
  (format t "OpenGL-reset-state~%")
  (map-over-active-windows
   (window)
   (glMakeCurrent window)
   (let ((glerr (glGetError)))
     (unless (eql glerr gl:GL_NO_ERROR)
       (format t "OpenGL-reset-state: previous error = ~a~%" (gl::gluErrorString glerr))))

   ;; pop the attribute stack until it underflows
   (loop do (glPopAttrib) 
	 while (eql (glGetError) gl:GL_NO_ERROR))
   (OpenGL-reset-state2)
   ;; pop all of the matrix stacks until they underflow
   (loop for matrix-mode in (list GL_MODELVIEW GL_PROJECTION GL_TEXTURE GL_COLOR)
	 do (glMatrixMode matrix-mode)
	    (loop do (glPopMatrix)
		  while (eql (glGetError) gl:GL_NO_ERROR)))
   ))

(defun report-gl-errors ()
  (loop for glerr = (glGetError)
	until (eql glerr gl:GL_NO_ERROR)
	do (format t "report-gl-errors: ~a~%" (gl::gluErrorString glerr))))


;;; For window classes that shared a single OpengL context, it is only necessary to
;;; do this for the current context.
;;; FIXME:  Ask whether a single context is shared.
(defun OpenGL-reset-state ()
  (format t "OpenGL-reset-state~%")
  (let ((glerr (glGetError)))
    (unless (eql glerr gl:GL_NO_ERROR)
      (format t "OpenGL-reset-state: previous error = ~a~%" (gl::gluErrorString glerr))))

  ;; pop the attribute stack until it underflows
  (loop do (glPopAttrib) 
	while (eql (glGetError) gl:GL_NO_ERROR))
  (OpenGL-reset-state2)
  ;; pop all of the matrix stacks until they underflow
  (loop for matrix-mode in (list GL_MODELVIEW GL_PROJECTION GL_TEXTURE GL_COLOR)
	do (glMatrixMode matrix-mode)
	   (loop do (glPopMatrix)
		 while (eql (glGetError) gl:GL_NO_ERROR)))
  (normalize-gl-state)
  )

(defvar *gl-default-alpha* 1.0)

(defun normalize-gl-state ()
 ; (glDisable GL_DEPTH_TEST)
  (glDisable GL_POLYGON_STIPPLE)
  (glMatrixMode GL_MODELVIEW) (glLoadIdentity)
  (glMatrixMode GL_PROJECTION) (glLoadIdentity)
  (glMatrixMode GL_TEXTURE) (glLoadIdentity)
  (glRasterPos2d -1.0 -1.0)
  (glPixelZoom 1.0 1.0)
  (glColor4d 1.0 1.0 1.0 *gl-default-alpha*))

#|
(defun OpenGL-reset-state ())
|#



(defmacro with-gl-drawing ((window form) &rest body)
  `(let ((,window ,form))
    (with-gl-drawing% ,window #'(lambda (,window) .,body))))

(defun with-gl-drawing% (window function)
  (clear-window window)
  (funcall function window)
  (copy-to-front window)
  (glFinish)
  (gl-unmake-current window))

  
;;; The window system dependent rendering methods.
;;; Most of these should move to their respective files, where the
;;; window class is defined:
;;; tk-glx.lisp, tk-wgl.lisp tk-agl.lisp, tk-cocoa.lisp

;;; The generic-function GL-MAKE-CURRENT has the following contract: The
;;; backing-store buffer (either a back-buffer or a pbuffer) of the window is
;;; made current for OpenGL drawing.

(defmethod gl-make-current ((window tkgl-window))
  (glMakeCurrent window) 
  (handle_gl_errors "(glMakeCurrent window) in gl-make-current ")
  (glDrawBuffer GL_BACK)
  (handle_gl_errors "(glDrawBuffer GL_BACK) in gl-make-current ")
  )


;;; Because Cocoa, in particular, forces us to lock and unlock the GL
;;; view, we will need this method.  It can be a no-op in other window
;;; environments.  Since we sometimes want to trace the most primitive
;;; make/unmake current calls, we always need to invoke here:

(defmethod gl-unmake-current ((window tkgl-window))
  (glUnMakeCurrent window))


;;; FIXME  COLOR is a double-float vector.  Elsewhere colors are single-float vectors.
(defun clear-window-int (color)
  (glDisable GL_POLYGON_STIPPLE)
  (glClearColor4dv color) (glDepthMask 1)
  (glClear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT)))

;;; The generic-function CLEAR-WINDOW has the following contract: The
;;; backing-store buffer of the window (either a back-buffer or a pbuffer) is
;;; cleared to the specified color.  Then the backing-store buffer is copied to
;;; the front buffer.  OpenGL is left in a state allowing writes to the
;;; backing-store buffer.

#+old
(defmethod clear-window ((window tkgl-window) &optional (color (background-color window)))
  (gl-make-current window)
  (clear-window-int color)
  (copy-to-front window nil)
  (glDrawBuffer GL_BACK) ; exit with drawing in back-buffer selected
  (handle_gl_errors "after copy-to-front in clear-window ")
  (gl-unmake-current window)
  )

(defmethod clear-window ((window tkgl-window) &optional (color (background-color window)))
  (gl-make-current window)
  (clear-window-int color)
  (copy-to-front window nil)
  (glDrawBuffer GL_BACK) ; exit with drawing in back-buffer selected
  (handle_gl_errors "after copy-to-front in clear-window ")
  ;; (gl-unmake-current window)
  )

(defmethod clear-window ((window gl-pbuffer-window-mixin )
			 &optional (color (background-color window)))

  (gl-make-current window)
  (glFinish)				; necessary for cross-machine 
  (clear-window-int color) 
  (copy-to-front window)
  (gl-make-current window) ; exit with drawing in pbuffer selected
  )

;;;
;;; I'm not entirely sure that this is right:
;;(defmethod glMakeCurrent :after ((window gl-pbuffer-window-mixin))
 ; (setq gui::*foo* window)   (break)
 ; (glFinish) ; this is needed for cross machine
;;  (gl::glXMakeCurrent (xdisplay window) (xwindow window) (glcontext window))
;;  )

;;; The generic-function COPY-TO-FRONT has the following contract: The
;;; backing-store buffer of the specified window is copied to the front-buffer
;;; OpenGL is left in the state for drawing to the front-buffer.


;;;
;;; These functions all want to call MakeCurrent: 
;;;
;;; COPY-TO-FRONT
;;; CLEAR-WINDOW
;;; RENDER-WINDOW

;;; NEW CONTRACTS:

;;; The functions COPY-TO-FRONT and CLEAR-WINDOW assume that
;;; MakeCurrent has already been called.

;;; It seems useful to define a basic GL-WINDOW class that deals with
;;; OpenGL and any constraints that arise in different GL
;;; implementations.  For now, use TKGL-WINDOW as the basic class...


;;; The fundamental purpose of this method is to copy the back buffer
;;; of a window into its front buffer.  This could be a self-contained
;;; operation:

;;;
;;; Nasty, but since RENDER-WINDOW is used when and only when OpenGL
;;; is to be rendered to the window, this should be safe:

(defmethod copy-to-front ((window tkgl-window) &optional (unmake-current nil))
;;  #+mswindows (glFinish) ;; ?
  (glFinish)
;;  (gl-make-current window) ;; This appears to be an extra call to gl-make-current
  (normalize-gl-state)	   ;; This is (apparently) important for indirect rendering
  (glReadBuffer GL_BACK)
  (glDrawBuffer GL_FRONT)
  (mv-bind (width height) (dimensions window)
    (glCopyPixels 0 0 width height GL_COLOR))
;;  (when unmake-current (gl-unmake-current window))
  )



;;; Redefined in gl-gui for threadsafe GL:
(defun render-selected-objects-in-front-buffer (view)
  ;;(glFinish)
  (let ((window (view-window view)))
    (gl-make-current window)
      ;; Double-check this on Snow Leopard...
      (mv-bind (width height) (dimensions window)
	(glViewPort  0 0 width height)
	)
      (glDrawBuffer GL_FRONT)
      (render-dynamic-objects view)
      (glFlush)				; this is important
      )
  )

;;; (defmethod copy-to-front ((window gl-pbuffer-window-mixin))) is defined in tk-glx.lisp

(defmethod drag-redisplay ((window tkgl-window) view)
  (declare (ignore view))
  (render-window window  t))

;;;(defmethod drag-redisplay ((window tkgl-window) view)
;;;  (redisplay view :from-backing-store t))

;;; Note that the bbuffer and pbuffer versions of drag-redisplay are identical.
;;;(defmethod drag-redisplay ((window gl-buffered-window-mixin) view)
;;;  (let* ((*current-view* view)
;;;         (window (view-window view)))
;;;    (copy-to-front window)
;;;    (render-selected-objects-in-front-buffer view)))

;;#+sbcl
;;(defmethod render-window :around ((window t) from-backing-store)
;;  (sb-sys::without-interrupts
;;      (call-next-method)  ) )


;;; This vanilla version does no drawing in the front buffer and has absolutely
;;; no backing store.  Every call to redisplay or drag-redisplay requires a
;;; complete redraw.
(defmethod render-window ((window tkgl-window) from-backing-store)
  (declare (ignore from-backing-store))
;;  (format t "~%RENDER-WINDOW on tkgl-window") (force-output)
  (let* ((view (top-view window))
	 (*current-view* view))
    (clear-feedback-cache)

    (with-gl-locked
      (gl-make-current window)
      (glDrawBuffer GL_BACK)

      (render-stationary-objects view)
      (render-dynamic-objects view)
      (copy-to-front window nil)
      (glFinish))
    (debug-print "B")
    ))




;;;

(defmethod render-window ((window gl-buffered-window-mixin) from-backing-store)
;;  (format t "~%RENDER-WINDOW on gl-buffered-window-mixin") (force-output)
  (let* ((view (top-view window))
	 (*current-view* view))
    (with-gl-locked
      (when (and from-backing-store
                 (backing-store-valid-p window))
        (copy-to-front window)
        (render-selected-objects-in-front-buffer view)
        (debug-print "b")
        (return-from render-window nil))

      (clear-feedback-cache)
      ;;    (format t "~%Cleared feedback cache.") (force-output)
      (gl-make-current window)
      (glFinish)				; necessary for cross-machine 

      (render-stationary-objects view)
      (copy-to-front window)
      (render-selected-objects-in-front-buffer view)
      )

    (setf (back-buffer-state window) (visible-p window))
    ;; (gl-unmake-current window)
    (debug-print "B")
    ))



;;;
;;; Override these methods:
;;;
#||



#+cocoa
(defmethod render-window ((window tk-cocoa-bbuffer-window) from-backing-store)
  (format t "~%RENDER-WINDOW on tk-cocoa-bbuffer-window") (force-output)
  (let* ((view (top-view window))
	 (*current-view* view))
    (when (and from-backing-store
	       (backing-store-valid-p window))
      (copy-to-front window)
      (render-selected-objects-in-front-buffer view)
      (debug-print "b")
      (return-from render-window nil))

    (clear-feedback-cache)
    ;;    (format t "~%Cleared feedback cache.") (force-output)

    (render-stationary-objects view)
    (handle_gl_errors "after render-stationary-objects ")
    (gl::with-gl-window (window)
      (glReadBuffer GL_BACK)
      (handle_gl_errors "after glReadBuffer (render-window) ")
      (glDrawBuffer GL_FRONT)
      (handle_gl_errors "after glDrawBuffer (render-window)")
      (mv-bind (width height) (dimensions window)
	(glCopyPixels 0 0 width height GL_COLOR)))
    (handle_gl_errors "after copy-to-front ")
    (render-selected-objects-in-front-buffer view)
    (handle_gl_errors "after render-selected-objects-in-front-buffer ")
    (setf (back-buffer-state window) (visible-p window))
    (debug-print "B")
    ))


#+cocoa
(defmethod clear-window ((window tk-cocoa-bbuffer-window )
			 &optional (color (background-color window)))
  (format t "~%CLEAR-WINDOW on tk-cocoa-bbuffer-window") (force-output)

  (gl-make-current window)
  (glFinish)				; necessary for cross-machine 
  (clear-window-int color) 
  (copy-to-front window)
  (gl-make-current window) ; exit with drawing in pbuffer selected
  )


#+cocoa
(defmethod copy-to-front ((window tk-cocoa-bbuffer-window))
  (format t "~%COPY-TO-FRONT on tk-cocoa-bbuffer-window") (force-output)
  (gl-make-current window) ;; This appears to be an extra call to gl-make-current
  (normalize-gl-state)	 ; This is (apparently) important for indirect rendering
  (handle_gl_errors "after normalize-gl-state ")
  (glReadBuffer GL_BACK)
  (handle_gl_errors "after glReadBuffer (copy-to-front) ")
  (glDrawBuffer GL_FRONT)
  (handle_gl_errors "after glDrawBuffer ")
  (mv-bind (width height) (dimensions window)
    (glCopyPixels 0 0 width height GL_COLOR))
  (handle_gl_errors "after glCopyPixels ")
  )
||#



(defmethod render-stationary-objects ((view view))
  ;;  (format t "render-stationary-objects ~a~%" view) (force-output)
  ;;(break)
  (let* ((interactor *interactor*)
	 (img (view-image view))
	 (2d-to-window-matrix (2d-to-window-matrix view))
	 (gl-bits (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT GL_STENCIL_BUFFER_BIT))
	 (window (view-window view))
	 )

    (gl::with-gl-window (window)
      (mv-bind (width height) (dimensions window)
	(glViewPort  0 0 width height)
	)

      (unwind-protect-case ()
	  (progn
	    (glPushAttrib GL_ALL_ATTRIB_BITS)
	    (glClearColor4dv (background-color (display-attributes view)) )
	    (glClear gl-bits)
	    ;; DISPLAY THE IMAGE
	    (when img (display-image view img 2d-to-window-matrix))
	    (draw-axes 10.0) ; This will draw in the image coordinate system -- weird scale factor
	    (set-default-graphics-attributes view)
	    
	    (when (and *hidden-line-eliminate* (not *gl-shading-enabled*))
	      (render-all-object-sets-with-hidden-line-removal view interactor))
	    
	    ;; To generate depth maps, a call to (glEnable GL_DEPTH_TEST) needs to be inserted here:
	    (draw-all-object-sets view interactor))
	  ;;cleanup form
	  (:abort (OpenGL-reset-state))
	  (:normal (glPopAttrib)))
      )))

(defmethod render-dynamic-objects ((view basic-view))
  nil)

(defmethod redisplay ((view basic-view) &key from-backing-store &allow-other-keys)
  (let ((window (view-window view))
	(*current-view* view))
    (declare (special *current-view*))
    (when (drawable-p window)
      (render-window window from-backing-store))))

(defmethod redisplay ((view view) &key (recompute-textures nil) from-backing-store)
  ;; This will allow GL to recompute the pixel values based on the new
  ;; transfer parameters.  We don't need this unless the photometric
  ;; transform is changed.
  (when recompute-textures
    (let ((image (view-image view)))
      (when image
	(img::release-image-pool-textures image 0))))

  (let ((window (view-window view))
	(*current-view* view))
    (declare (special *current-view*))

    ;; This is needed to avoid drawing before GLWIN-CONFIGURE-CALLBACK is
    ;; called.  LHQ Sun Jul 7 2002: DRAWABLE-P means that the window is
    ;; configured and ready to accept drawing operations.

    (when (drawable-p window)
      (render-window window from-backing-store) 
      (release-textures-after-delay)
      )  ; this might be the wrong place for setting this timer.
    ))

(defmethod redisplay ((window tkgl-window) &rest args &key &allow-other-keys)
  (when (drawable-p window)
    (let ((view (top-view window)))
      (if view
	  (apply 'redisplay view args)
          (clear-window window)
          ))))


(defvar *hidden-line-offset-factor* 1.5d0)
(defvar *hidden-line-offset-units* 2.5d0)

;;; This is the first phase of a 2 step hidden line removal algorithm.
;;; The phase renders the objects without setting the color buffer,
;;; but generating the correct depth values for an ordinary depth-tested
;;; render step.
(defun render-all-object-sets-with-hidden-line-removal  (view interactor &optional
                                                         (factor *hidden-line-offset-factor*)
                                                         (units *hidden-line-offset-units*))
  (declare (double-float factor units))
  (unwind-protect
       (progn (glPolygonMode GL_FRONT GL_FILL)
	      (glColorMask 0 0 0 0)	; disable writing to color buffer
	      (glDisable GL_POLYGON_STIPPLE)
	      ;;(gl::glPolygonOffset 1.0d0 2.0d0 ) ; not sure about these values
	      ;;(gl::glPolygonOffset 1.5d0 2.5d0 ) ; not sure about these values
              (gl::glPolygonOffset factor units ) ; not sure about these values
	      (glEnable gl::GL_POLYGON_OFFSET_FILL)
	      (draw-all-object-sets view interactor))
    ;; clean-up forms
    (glEnable GL_POLYGON_STIPPLE)
    (glDisable gl::GL_POLYGON_OFFSET_FILL)
    (glColorMask 1 1 1 1)
    (glPolygonMode GL_FRONT GL_LINE))
  )

(defparameter gui::*inhibit-object-display* nil)

;;; Something's broken on Mac OS X:
;;; FIXME Fri Nov  5 2004 LHQ:  Chris,  We need top get to the bottom of this
;;; Is this still required?
;;; Mon Nov 15 2004 -- Chris Connolly verifies that the problem is gone.
;;;(defparameter gui::*always-immediate-render-p* #-macosx nil #+macosx t)
;;;
;;; Took me long enough, but I am determined to keep this NIL so that
;;; we can exploit static-object display lists (-CC):

(defparameter gui::*always-immediate-render-p* nil)


#|
(setq gui::*inhibit-object-display* t)
(setq gui::*inhibit-object-display* nil)
(setq gui::*always-immediate-render-p* t)

|#

;;; Some projections are not linear, e.g. RPCs.  In these cases,
;;; object vertices must be transformed into modified object coordinates and
;;; the results cached.  Each object has an alist of transformed-vertex-arrays
;;; indexed by the non-4x4-projection and the vertex-array.  See the function
;;; obj::maybe-transform-vertex-array

(defvar *transform-vertices-projection* nil)

(defun transform-vertices-projection (view)
  (let* ((projection (3d-to-2d-projection view))
	 (surrogate-projection (and projection (surrogate-projection projection))))
    (and surrogate-projection
	 (list projection surrogate-projection))))

(defvar *chatty* nil)

#+merged-object-sets
(defmacro render-immediate-and-display-list-objects (predicate immediate-form display-list-form)
  (declare (ignore predicate))
  `(progn ,immediate-form ,display-list-form))

#-merged-object-sets
(defmacro render-immediate-and-display-list-objects (predicate immediate-form display-list-form)
  `(if ,predicate ,immediate-form ,display-list-form))

;;;
(defmethod display-object-set ((view view) object-set interactor)
  (unless *inhibit-object-display*
    (let* (exclude-objects
	   (*transform-vertices-projection* (transform-vertices-projection view))	  )
      (when *chatty*
	(format t "display-object-set ~a ~a ~%"
		view (and (get-prop object-set :feature-set)
			  (name (get-prop object-set :feature-set)))))
      #+never
      (format t "display-object-set ~a ~a ~%"
	      view
	      (or (and (get-prop object-set :feature-set)
		       (name (get-prop object-set :feature-set)))
		  (get-prop object-set :name)))
      
      (with-slot-values (world immediate-render-p) object-set
	(unless world (error "object set has no world"))
	(when  (cond ((eq world (2d-world view))
		     (when (set-2d-matrices view)
		       (setq exclude-objects (selected-objects-in-world
					      world interactor))
		       t))
		    (;;(eq world (3d-world view))
		     (typep world 'gl-3d-world)
		     ;; This allows 3d-world to be different from (3d-world view)
		     (when (set-3d-matrices view)
		       (setq exclude-objects (selected-objects-in-world
					      world interactor))
		       t))
		    )
	  
          #+never
          (format t "~%object-set = ~a: immediate-render-p = ~a ; *always-immediate-render-p* = ~a"
                  object-set immediate-render-p *always-immediate-render-p*)
	  (render-immediate-and-display-list-objects
	   (or immediate-render-p *always-immediate-render-p*)
	   (immediate-render-object-set object-set exclude-objects view)
           ;; Not sure if this is right, but it seems to me that
           ;; immediate-render object sets should not go through this
           ;; process.  The deeper problem is that graph objects yield
           ;; an error here (GL error: invalid value)
	   (unless (immediate-render-p object-set)
             ;; (format t "display-object-set glCallList ~a ~a~%" view object-set)
	     (glFlush) (handle_gl_errors "display-object-set 0")
	     (ignore-errors
	       (glCallList (build-display-list object-set exclude-objects view)))
	     (glFlush) (handle_gl_errors "display-object-set 1")))
	  )))))

;;; new-excluded-object-list is needed in order for the "back-buffer"
;;; to be rendered without dynamic-objects.
(defmethod immediate-render-object-set ((object-set object-set) new-excluded-object-list view)
  (declare (ignorable view))
  ;;(format t "immediate-render-object-set ~a ~a ~%" object-set  view)
  (let* ((new-excluded-objects
	  (selected-objects-in-object-set object-set new-excluded-object-list)))
    (map-over-object-set (obj object-set t)
      (unless (memq obj new-excluded-objects)
	;;(format t "immediate-render-object-set ~a ~a ~%" obj view)
	(draw-object-around obj view))))

  #+never ;;  removed Sat Nov  6 2004 
  (let ((display-list-state (get-object-set-display-list-state object-set view)))
    (setf (excluded-objects display-list-state) new-excluded-objects
	  (invalid display-list-state) nil))
  )

(defvar *build-display-list-no-test* nil)
(defvar obj::*building-display-list*)

(defun get-object-set-display-list-id (object-set)
  (with-slots (display-list-id) object-set
    (or display-list-id
	(setf display-list-id (glGenLists 1)))))


(defvar *last-new-excluded-objects* nil)
(defvar *invalidate-all-object-sets* nil)

;;;(defmethod build-display-list ((object-set object-set) new-excluded-object-list view)
;;;  (let* ((display-list-state (get-object-set-display-list-state object-set view)))
;;;    (with-class-slot-values display-list-state
;;;        (excluded-objects invalid display-list-id) display-list-state
;;;      (unless *build-display-list-no-test*
;;;        (let ((new-excluded-objects
;;;               (selected-objects-in-object-set object-set new-excluded-object-list)))
;;;          (when (or invalid *invalidate-all-object-sets*
;;;                    (not (equal excluded-objects new-excluded-objects)))
;;;                                        ;(setq *object-set* object-set)
;;;            (when new-excluded-objects (setq *last-new-excluded-objects* new-excluded-objects))
;;;            ;;(format t "build-display-list ~a ~a ~a ~a ~%" view display-list-id new-excluded-objects *transform-vertices-projection*)
;;;            (glNewList display-list-id GL_COMPILE)
;;;            (map-over-object-set
;;;             object-set
;;;             #'(lambda(obj)
;;;                 (unless (memq obj new-excluded-objects)
;;;                   (draw-object-around obj view)))
;;;             nil)
;;;            (glEndList)
;;;            (setf (excluded-objects display-list-state) new-excluded-objects
;;;                  (invalid display-list-state) nil))))
;;;      display-list-id)))

(defvar *building-display-list* nil)

;;;(defmethod build-display-list ((object-set object-set) new-excluded-object-list view)
;;;  (let* ((display-list-state (get-object-set-display-list-state object-set view))
;;;         (display-list-id (display-list-id display-list-state))
;;;         (*building-display-list* t))
;;;    (unless *build-display-list-no-test*
;;;      (let ((new-excluded-objects (selected-objects-in-object-set object-set new-excluded-object-list)))
;;;        (when (regenerate-display-list-p view display-list-state new-excluded-objects)
;;;          (glNewList display-list-id GL_COMPILE)
;;;          ;(format t "build-display-list ~a~%" view)
;;;          (map-over-object-set (obj object-set nil)
;;;            (unless (memq obj new-excluded-objects)
;;;              (draw-object-around obj view)))
;;;          (glEndList)
;;;          (set-cookie display-list-state view)
;;;          (setf (excluded-objects display-list-state) new-excluded-objects
;;;                (invalid display-list-state) nil))))
;;;    display-list-id))

(defmethod build-display-list ((object-set object-set) new-excluded-object-list view)
  (if *build-display-list-no-test*
      (display-list-id (get-object-set-display-list-state object-set view))
      (let ((new-excluded-objects (selected-objects-in-object-set object-set new-excluded-object-list)))
	(mv-bind (valid-p display-list-state) 
	    (display-list-valid-p view object-set new-excluded-objects)
	  (let ((display-list-id (display-list-id display-list-state))
		(*building-display-list* t))
	    (unless valid-p
	      (glNewList display-list-id GL_COMPILE)
	      ;; (format t "build-display-list ~a ~a~%" view object-set)
	      (map-over-object-set (obj object-set nil)
		(unless (memq obj new-excluded-objects)
		  (draw-object-around obj view)))
	      (glEndList)
	      (set-cookie view object-set display-list-state)
	      (setf (excluded-objects display-list-state) new-excluded-objects
		    (invalid display-list-state) nil))
	    display-list-id)))))

;(setq *obj-set* *)
;(map-over-object-set (obj *obj-set* nil) (when (typep obj 'obj::crosshair-object) (print obj)))
;(map-over-object-set (obj *obj-set* nil) (when (typep obj 'obj::crosshair-object)(break "~a" obj)))

;;; These methods should move to gl-objects.lisp
(defmethod set-gl-matrices ((world gl-2d-world) view)
  (set-2d-matrices view))

(defmethod set-gl-matrices ((world gl-3d-world) view)
  (set-3d-matrices view))

#+unused?
(defmethod set-gl-matrices ((world gl-world) view)
  nil)

(defmethod pick-from-object-set ((view view) object-set interactor x y size)
  (let* (exclude-objects
	 (*transform-vertices-projection* (transform-vertices-projection view)))
    (with-slot-values (world immediate-render-p) object-set
      (when (set-gl-matrices world view)
	(set-gl-pick-matrix x y size)
	(setq exclude-objects (selected-objects-in-world world interactor))
	;; (format t "pick-from-object-set ~a~%" immediate-render-p)
	(render-immediate-and-display-list-objects
	 (or immediate-render-p *always-immediate-render-p*)
	 (map-over-object-set (obj object-set t)
	   (draw-object-around obj view))
	 (glCallList (build-display-list object-set exclude-objects view)))
	))))

(defparameter *force-graphics-style* nil)

;;; When *highlighting-selected-objects* = T, gl-shading is disabled so that DRAG-SELECT-OBJECT can
;;; perform wire-frame highlighting of shaded objects without doing a full redisplay.
;;; Unfortunately, this means that shaded objects display differently (wire-frame + shaded) during
;;; DRAG-SELECT-OBJECT than when redisplayed as selected-objects (shaded only).  In order to solve
;;; this problem, on views where gl-shading is enabled, selected-objects must be displayed with both
;;; gl-shading enabled and gl-shading disabled.
(defparameter *highlighting-selected-objects* nil)
#|
(setq *highlighting-selected-objects* t)
|#

(defparameter *highlighting-graphics-style*
  (make-instance 'gl-graphics-style :color "cyan"))

(defmethod default-highlighting-graphics-style ()
  *highlighting-graphics-style*)
  
(defmethod render-dynamic-objects ((view view))
  (let* ((interactor *interactor*)
	 ;;(force-graphics-style nil)
	 (force-graphics-style (default-highlighting-graphics-style ))
	 (selected-3d-objfrags (selected-objfrags-in-world (3d-world view) interactor))
	 (selected-2d-objfrags (selected-objfrags-in-world (2d-world view) interactor))
	 (disable-depth-test (or (not *enable-dynamic-object-depth-test*)
				; (select-drag-p interactor)
				 ))
	 (*enable-depth-test* (not disable-depth-test))
	 ;;(*enable-depth-test* nil)
	 (*transform-vertices-projection* (transform-vertices-projection view))	   
	 )
    (unwind-protect-case ()
	(let ()
	  ;;(format t "render-dynamic-objects ~a ~a~%" view selected-3d-objfrags)
	  ;;(glMakeCurrent (view-window view))
	  (glPushAttrib GL_ALL_ATTRIB_BITS)
	  ;;(set-default-graphics-attributes view) 
	  (when disable-depth-test
	    (glDisable GL_DEPTH_TEST))
	  (glDepthMask 0)		; disable writing to z-buffer

	  (let ((non-backing-store-object-sets
		 (loop for object-set in (object-sets view)
		       unless (and (backing-store-p object-set)
				   ;(not (immediate-render-p object-set))
				   )
			 collect object-set)))
	    (when non-backing-store-object-sets
	      ;;(format t "render-dynamic-objects non-backing-store-object-sets~%")
	      (unwind-protect
		   (progn
		     (glPushAttrib GL_ALL_ATTRIB_BITS)
		     (set-default-graphics-attributes view)
		     (loop for object-set in non-backing-store-object-sets
			   do (display-object-set view object-set interactor)))
		;; cleanup form
		(glPopAttrib)))

	    ;; Now draw all selected objects
	    (flet ((display-selected (shading-enabled)
		     (let ((*gl-shading-enabled* shading-enabled))
		       (declare (special *gl-shading-enabled*))
		       (set-default-graphics-attributes view)
		       (when (and selected-3d-objfrags (set-3d-matrices view))
			 ;;(format t "draw-objects~a~%" (view-window view))
			 (draw-objects selected-3d-objfrags
				       :force-graphics-style force-graphics-style))
		       (when (and selected-2d-objfrags (set-2d-matrices view))
			 (draw-objects selected-2d-objfrags
				       :force-graphics-style force-graphics-style))
		       ;;(format t "(display-selected ~a)~%" shading-enabled)
		       )))
	      (if *gl-shading-enabled*
		  (progn
		    ;; (format t "render-dynamic-objects ~a~%" force-graphics-style)
		    (unless *highlighting-selected-objects*
		      (display-selected t))
		    (display-selected nil))
		  
		  (display-selected *gl-shading-enabled*)))))
      ;; cleanup forms
      (:always (when nil ; disable-depth-test  ; this looks wrong 
		 (glEnable GL_DEPTH_TEST))
	       (glDepthMask 1)
	       (glPopAttrib))
      (:abort (OpenGL-reset-state)))))


;;; **************************  IMAGE DISPLAY  **************************
#|
(format nil "~x" (aref (cadr (get-prop ".frm.f2.gl2" :window-state)) 101 100))
(tex_map-pyramid-level (2d-to-window-matrix ".frm.f2.gl2"))
(redisplay-from-backing-store ".frm.f2.gl2")
(aref (nth 2 (get-prop ".frm.f2.gl2" :window-state)) 100 100)
(format nil "~x" (aref (nth 1 (get-prop ".frm.f1.gl1" :window-state)) 100 100))
(redisplay-from-backing-store ".frm.f1.gl1")

(defun test-zb (&optional (x 300) (y 100))
  (list (aref (nth 2 (get-prop ".frm.f2.gl2" :window-state)) x y)
	(aref depths x y)))
|#

#|
(let ((popup-drag t) (drag-op 'drag-uv))
  (compute-display-image-min-level ".frm.f2.gl2" alv-2-44))
|#

(defparameter *enable-blend-hack* nil)
#|
(setq *enable-blend-hack* nil)
|#

#|
;;; This was an experiment to test texture mapping with texture coordinates
;;; extending outside the texture coordinate unit square.  The hope was that by
;;; using GL_CLAMP_TO_BORDER with GL_TEXTURE_BORDER_COLOR having alpha=0, we
;;; could cheaply partition irregular polygon grids without worring about
;;; fracturing the polygons at texture tile boundaries.   The experiment failed,
;;; apparently GL_CLAMP on the O2 does not behave correctly.

(defun blend-hack (flag)
  (when *enable-blend-hack* 
    (if flag
	(progn
	  (glClearColor 1.0 1.0 1.0 1.0)
	  (glClear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
	  (glColor3d .9 .9 .8 ) ; blending modulates this color
	  (glEnable GL_BLEND)
	  ;;(glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_ADD)
	  ;;(glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
	  )

	(progn
	  (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
	  (glDisable GL_BLEND))
	)))

(defun blend-hack (flag)
  (if (and flag *enable-blend-hack*)
      (progn
	(glClearColor 1.0 1.0 1.0 1.0)
	(glClear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
	(glColor3d .9 .9 .8 )		; blending modulates this color
	(glEnable GL_BLEND)
	;;(glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_ADD)
	;;(glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
	)

      (progn
	(glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
	(glDisable GL_BLEND))
      ))

;;(setq *enable-blend-hack* t)

(defun blend-hack (flag)
  (if (and flag *enable-blend-hack*)
      (progn
	(glClearColor 0.0 0.0 0.0 0.0)
	(glClear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
	(glColor3d 1.0 1.0 .0 )		; blending modulates this color
	(glEnable GL_BLEND)
	;;(glBlendFunc GL_SRC_ALPHA GL_ONE)
	(glBlendFunc GL_ONE GL_ONE)
	
	;;(glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_ADD)
	;;(glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
	)

      (progn
	;;(glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
	(glDisable GL_BLEND))
      ))
|#



 
(defparameter *display-image-min-level* nil)
;; (defparameter *gl-display-image-mode* 1)  ; 0 = glTexMap, 1 = glDrawPixels
(defparameter *gl-display-image-mode* 0)     ; 0 = glTexMap, 1 = glDrawPixels

(defvar *inhibit-image-display* nil)

#|
(setq *gl-display-image-mode* 1)
(setq *max-drag-pixels* (* 2048 2048))
(setq *max-drag-pixels* (* 512 1024))
(setq *max-drag-pixels* (* 512 512))
(setq *max-drag-pixels* (* 512 256))
(setq *max-drag-pixels* (* 512 256))
(progn *max-drag-pixels*)

(let ((reduce (display-image-reduce-level (selected-window *interactor*) t))
      (desired (tex-map-pyramid-level (2d-to-window-matrix (selected-view *interactor*)))))
  (list reduce desired (+ reduce desired)))

(img::resize-image-page-pool (gui::view-image (gui::top-view t)) 1000)
(img::resize-image-page-pool (gui::view-image (gui::top-view t)) 100)
(gui::resize-texid-page-pool 100)
(img::image_page_pool_stats (img::image-id (gui::view-image (gui::top-view t))) 0)
(gui::image_tex_pool_stats (img::image-id (gui::view-image (gui::top-view t))) 0)

(setq *gl-display-image-mode* 1
      *rotation-per-screen-motion* 0.0)
(list *rotation-per-screen-motion* (/ 360.0 1280))

(setq *gl-display-image-mode* 0
      *rotation-per-screen-motion* (/ 360.0 1280))
(setq gui::*inhibit-image-display* t)
(describe (view-image (top-view)))
(loop for img in img::*paged-image-list*
      collect (img::image-id img))
(alien::sap-int (alien::alien-sap (img::image-id (view-image (top-view)))))
(img::image-id (view-image (top-view)))
(let ((cimg (img::image-id (view-image (top-view)))))
  (list (img::c-image-x-dim cimg) (img::c-image-y-dim cimg) (img::c-image-element-size cimg)
	
	(img::c-image-class-code cimg)
	(img::c-image-x-map cimg) (img::c-image-y-map cimg)
	(img::c-image-block-x-dim cimg) (img::c-image-block-y-dim cimg)
	(img::c-image-block-size cimg) (img::c-image-blocks-wide cimg) (img::c-image-blocks-hi cimg)))

(let ((cimg (img::image-id (view-image (top-view)))))
  (list (lcl::foreign-pointer-address cimg) (alien::sap-int (alien::alien-sap cimg))(%pointer cimg)))
(img::describe-c-image (img::image-id (view-image (top-view))))
(type-of (img::image-id (view-image (top-view))))
(print-object (view-image (top-view)) *standard-output*)
(describe (view-image (top-view)) )
(progn (view-image (top-view)))
(img::image-block-dims (view-image (top-view)))

(loop for img in img::*paged-image-list*
      do (describe img))
|#


;;(defvar *display_tiled_image-cvt-mode* -1) ; -1 => luminance-to-rgba-inhibit
(defvar *display_tiled_image-cvt-mode* 0)
#|
(setq *display_tiled_image-cvt-mode* -1) ; works ok on NVidia GeForce 4
(setq *display_tiled_image-cvt-mode* 0)
|#

(defun tex-map-pyramid-level (mat &optional radix  (off 0.0))
  (unless radix (setq radix 2.0))
  (let* ((scale (euclidean-length (aref mat 0 0) (aref mat 0 1)))
	 (level (- off (log scale radix))))
    ;;(format t "(~4f, ~4f) " (/ scale) level)
    level)) ; previously returned (floor level)

;;; FIXME:  This needs to inquire OpenGL about size of texture memory.  Not sure how to do that
;;; FIXME:  Make this a parameter set from a config-file.
(defparameter *max-drag-pixels*
  #+irix (* 256 256) ; SGI O2
  #-irix (* 512 512) ; Matrox G450 ; (* 512 256)
  ) 

#|
Good values for NVidia 6600 with 256MB memory
(setq *max-drag-pixels* (* 1600 1200))
Need to make sure pages pools and texture pools are big enough
(img::resize-texid-page-pool 200)
(img::resize-image-page-pool (gui::view-image (gui::top-view t)) 100) ; 100 tiles 512x512
|#

;;; This needs to decide what level can be interactively displayed
;;; without page thrashing.
;;; Depends on window size and image size.
(defun compute-display-image-min-level (window img)
  (if (and (popup-drag *interactor*)
	   (eq (drag-object-type *interactor*) 'image))
      (mv-bind (width height) (dimensions window)
	(let* ((image-size (* (image-x-dim img) (image-y-dim img)))
	       (win-size (* width height))
	       (min-size (min image-size win-size))
	       (level (max 0 (log2 (sqrt (/ (dfloat min-size)
					    *max-drag-pixels*))))))
	  ;;(setq foo (list image-size win-size min-size level))
	  level))
      nil))

(defun image-drag-in-progress ()
  (and (popup-drag *interactor*)
       (eq (drag-object-type *interactor*) 'image)))

;;; In order to maintain image drag performance, the number of pixels
;;; in texmaps must not exceed the size of the texture map memory.
;;; We must reduce the mip-map level in order avoid texture memory paging delays.
(defun display-image-reduce-level (window &optional
				   (image-drag-p (image-drag-in-progress)))
  (if image-drag-p
      (mv-bind (width height) (dimensions window)
	(let* ((win-size (* width height))
	       (level (max 0.0 (* .5 (log (/ (dfloat win-size) *max-drag-pixels*) 2.0)))))
	  ;;(setq foo (list image-size win-size min-size level))
	  level))
      0.0))

;;; specialize this for vector-images.
(defmethod image-for-display ((image img::image))
  image)

;;; In many cases, the image/transform pair used below can result in
;;; "degenerate" images, with 0 or 1 for an image dimension.  This is
;;; because level is computed without regard to the minimum image
;;; resolution, so clip the level here:

(defun limit-image-pyramid-level (image level)
  (declare (fixnum level))
  (min (- (integer-length (image-x-dim image)) 2)
       (- (integer-length (image-y-dim image)) 2)
       level))

;;; LHQ Sun Sep  5 2004  DISPLAY-IMAGE needs work.
;;; It should really be enforced that base-image is the highest res image in the pyramid.
;;; 2d-to-window-matrix should be obtained from VIEW.

;(fmakunbound 'select-display-image-image)
(defmethod select-display-image-image ((view view) base-image 2d-to-window-matrix)
 (let* ((window (view-window view))
	(reduce-levels (display-image-reduce-level window))
	(image-to-window-matrix 
	 (multiply-matrices 2d-to-window-matrix 
			    (transform-matrix (image-to-2d-transform base-image))))
	(desired-level (tex-map-pyramid-level image-to-window-matrix))
	(level (limit-image-pyramid-level base-image (floor (+ desired-level reduce-levels))))
	(img (img::pyramid-level (image-for-display base-image) level)))
   ;;(setq *foo* (list view base-image img (image-for-display base-image))) 
   ;(break)
   (values img level)))


#|
(setq *foo* nil)
(img::image-id *foo*)

(let ((*print-array* t))(print *foo88*))
(image-id (car *foo88*))
(view-image (top-view))
|#

#|
(excl::find-source-file )
(top-view)
(display-attributes (top-view))
(image-modulation-color  (display-attributes (top-view)))
(background-color (display-attributes (top-view)))
(setq extensions:*bytes-consed-between-gcs* (floor 40e6))
(setq ext::*gc-verbose* t)
(setq ext::*gc-verbose* nil)
|#

;;; ********************************  OBJECT DISPLAY  ********************************

(defparameter *draw-objects-with-selectid* t)
#|
(setq *draw-objects-with-selectid* nil)
|#

(defun draw-objects (objects &key
			     force-graphics-style 
			     (selectids *draw-objects-with-selectid*))
  ;; This is done in set-object-drawing-parameters
  ;;(glEnableClientState GL_VERTEX_ARRAY)
  (loop for obj in objects
	do (draw-object-around obj *current-view* force-graphics-style selectids)))

;;; No callers
;;;(defun draw-objfrags (objfrags &key
;;;                      force-graphics-style 
;;;                      (selectids *draw-objects-with-selectid*))
;;;  ;; This is done in set-object-drawing-parameters
;;;  ;;(glEnableClientState GL_VERTEX_ARRAY)
;;;  (loop for objfrag in objfrags
;;;        do (draw-object-around objfrag view force-graphics-style selectids)))
#|
(describe (children (get-prop (car (object-sets (top-view))) :feature-set)))
(children (get-prop (car (object-sets (top-view))) :feature-set))
(loop with l
      for obj in (children (get-prop (car (object-sets (top-view))) :feature-set))
      for cls = (type-of obj)
      unless (memq cls l)
	do (push cls l)
	   (format t "~s~%" cls)
      finally (return l))
|#





(defmethod draw-object-around ((objfrag t) view &optional force-graphics-style
			       (selectids *draw-objects-with-selectid*))
  (let* ((object (obj::object objfrag))
	 (fragment (obj::fragment objfrag)))
    (if fragment
	(let* ((gs (object-graphics-style object nil)))
	  (obj::highlight-object object fragment *current-view*
				 gs (or force-graphics-style gs) selectids))
	(draw-object-around object view force-graphics-style selectids))))

(defparameter *tmp-graphics-style* (make-instance 'obj::gl-graphics-style))

(defmethod draw-object-around ((object obj::basic-gl-object) view &optional force-graphics-style
			       (selectids *draw-objects-with-selectid*))
  (let* ((gs (object-graphics-style object nil))
	 ;(forced-gs (or force-graphics-style gs))
	 (forced-gs (obj::simple-merge-graphics-styles force-graphics-style gs *tmp-graphics-style*))
	 (*force-graphics-style* force-graphics-style)
	 )
    ;(format t "draw-object-around ~a~%" (list object forced-gs force-graphics-style selectids))
    (obj::with-gl-object-drawing (object view forced-gs)
      (when selectids
	(let ((id (get-object-select-name object)))
	  (glLoadName id)
	  (glPassThrough (float id 0.0f0)) ; this looks unnecessary -- GL_FEEDBACK rendermode is not used.
	  ))
      (draw-object object view)
      (when (or obj::*enable-object-labels* (get-prop object :draw-label))
	(obj::draw-object-label object gs force-graphics-style)))))

;(setf (get-prop (selected-object) :draw-label) (not (get-prop (selected-object) :draw-label)))


#+never
(defmethod draw-object-around ((object obj::3d-composite-object) view &optional force-graphics-style
			       (selectids *draw-objects-with-selectid*))
  ())


#|
(gethash 'tk::TEXTURE-RELEASE-TIMEOUT tk::*tk-callback-event-handler-hash-table*)
(transforms::projection-matrix (3d-to-2d-projection (top-view)))

(2d-to-window-matrix (top-view))

(transforms::transform-matrix (image-to-2d-transform (view-image (top-view)) ))

(default-2d-to-window-matrix (view-window (top-view)) (view-image (top-view t)))

(progn *foomat*)


(window-width ".top3.frm.1-1.1-1_gl")
(view-window (top-view))

(window-width ".top3.frm.1-1.1-1")

(background-color (display-attributes (top-view)))
|#

;;;
;;; Maintain the graphics styles for selection labels:
;;;

;;; These should probably become slots of *interactor*
(defvar *selection-graphics-style-stack* nil)
(defvar *selection-graphics-style*)

;;;
;;; In case other styles are desired.
;;;
(defun push-selection-graphics-style (&optional (font '("Helvetica" 12)) (color-vector (fv 0.5 0.5 0.0 1.0)))
  (push *selection-graphics-style* *selection-graphics-style-stack*)
  (setf *selection-graphics-style*
	(make-instance 'obj::graphics-style
		       :line-width 2
		       :font font
		       :color-vector color-vector)))

(defun pop-selection-graphics-style ()
  (when *selection-graphics-style-stack*
    (setf *selection-graphics-style*
	  (pop *selection-graphics-style-stack*))))

(declaim (special *object-vertex-selection-window-size*))

(defun set-selection-window (&optional (size 4.0))
  (setf *object-vertex-selection-window-size* size))



#|
(defun test-maybe-photometric-transform-for-display (n)
  (let* ((view (top-view))
	 (img (img::band-interleaved-image (view-image view)))
	 )
    (gl-make-current (view-window view)  )
    (loop repeat n
	  do (maybe-photometric-transform-for-display view img))))

(time (test-maybe-photometric-transform-for-display 1000))
(setq *print-array* nil)
|#


#|
(declaim (special *w2r*))
(loop for trans in *w2r* do (math::print-matrix (transform-matrix trans)))
(fmakunbound 'display-raster-image)
|#

(defvar *image-modulation-scale* 1.0)


(defparameter *display-raster-image-scalar-draw-pixels-type-alist*
  `(((unsigned-byte 8) ,GL_UNSIGNED_BYTE)
    ((unsigned-byte 16) ,GL_UNSIGNED_SHORT)
    (single-float ,GL_FLOAT)
    (double-float ,GL_DOUBLE)))
    
(defmethod display-raster-image-display-params ((img img::scalar-image))
  (values img
	  1
	  GL_LUMINANCE 
	  (let ((eltype (image-element-type img)))
	    (or (cadr (assoc eltype *display-raster-image-scalar-draw-pixels-type-alist*
			     :test #'equal))
		(error "unsuported element-type ~a" eltype)))))

(defmethod display-raster-image-display-params ((img img::color-image))
  (values (img::band-interleaved-image img) 
	  3
	  GL_RGB
	  GL_UNSIGNED_BYTE))

;;; image-to-2d-transform and 2d-to-window-transform must have no rotation or skew. Ie. the
;;; off diagonal elements of the upper 3x3-matrix must be zero.
(defmethod display-raster-image ((view view) &key (img (view-image view)) image-to-2d-transform)
  (mv-bind (img bytes-per-pixel format type)
      (display-raster-image-display-params img)
    (let* ((xdim-1 (- (image-x-dim img) 1.0))
	   (ydim-1 (- (image-y-dim img) 1.0))
	   (2d-to-window-transform (2d-to-window-transform view))
	   (2d-to-window-matrix (2d-to-window-matrix view))
	   (raster-to-image-transform (make-4x4-coordinate-transform 
				       (make-and-fill-4x4-matrix 1.0 0.0 0.0 0.0
								 0.0 -1.0 0.0 ydim-1
								 0.0 0.0 1.0 0.0
								 0.0 0.0 0.0 1.0)))
	   (image-to-2d-transform (or image-to-2d-transform (image-to-2d-transform view)))
	   (image-to-2d-matrix (transform-matrix image-to-2d-transform))
	   (raster-to-window-transform (list raster-to-image-transform
					     image-to-2d-transform 
					     2d-to-window-transform))
	   (window (view-window view))
	   (padded-xdim (img::image-padded-block-x-dim img))
	   (wx-1 (- (window-width window) 1.0))
	   (wy-1 (- (window-height window) 1.0)))
      (gl-make-current window)  
      (load-gl-projection-matrix (3rd-quadrant-window-to-ndc-matrix window))
      (load-gl-modelview-matrix nil)
      (bind-vector-elements (xul xlr yul ylr) ; bbox in raster coordinates
	  (transform-bounding-box (inverse-transform raster-to-window-transform)
				  (cv 0.0 wx-1 0.0 wy-1))
	(let* (				;(scale (/ wx-1 (- xlr xul)))
	       (xscale (* (aref image-to-2d-matrix 0 0) (aref 2d-to-window-matrix 0 0)))
	       (yscale (* (aref image-to-2d-matrix 1 1) (aref 2d-to-window-matrix 1 1)))
	       (x0 (ceiling (max 0.0 xul))) ; clip raster box to raster dims
	       (y0 (ceiling (max 0.0 yul)))
	       (x1 (floor (min xlr xdim-1)))
	       (y1 (floor (min ylr ydim-1)))
	       ;;(tile-offset (* bytes-per-pixel (+ x0 (* y0 padded-xdim))))
	       (width  (1+ (- x1 x0)))
	       (height (1+ (- y1 y0))))
	  ;;(setq *w2r* raster-to-window-transform)
	  (when (and (> width 0) (> height 0))
	    (bind-vector-elements (wx0 wy0) 
		(transform-vector raster-to-window-transform (cv (dfloat x0) (dfloat y0) 0.0))
	      (glRasterPos3d wx0 wy0 .5)
	      #+never
	      (format t "gl::glDrawPixels-offset ~a~%"
		      (list (list wx-1 wy-1)(list xdim-1 ydim-1)(list xul yul)(list xlr ylr) 
			    scale (list x0 y0)(list x1 y1)(list wx0 wy0)(list width height) format type))
	      (glPushClientAttrib GL_CLIENT_PIXEL_STORE_BIT)
	      ;; MAYBE-PHOTOMETRIC-TRANSFORM-FOR-DISPLAY significantly slows down display if
	      ;; the resultant scale is not 1.0 or offset is not 0.0. Use image-modulation-color instead.
					;(maybe-photometric-transform-for-display view img) 
	      ;; FIXME: Photometric-transform-panel needs to adjust 
	      ;;        (image-modulation-color (display-attributes view))
	      ;; No, this doesn't affect the glDrawPixels path.
	      ;;(glColor3dv (image-modulation-color (display-attributes view)))
	      ;; scale_bias parameters other than these really slow things down.
	      ;; (set_transfer_scale_bias *image-modulation-scale* 0.0) 
	      (glPixelStorei GL_UNPACK_ROW_LENGTH padded-xdim)
	      (glPixelStorei GL_UNPACK_SKIP_PIXELS x0)
	      (glPixelStorei GL_UNPACK_SKIP_ROWS y0)	    
					;(glPixelZoom scale (- scale))
	      (glPixelZoom xscale yscale)
	      (maybe-photometric-transform-for-display view img)
	      (glDrawPixels width height format type (img::image-array img) )
	      (glPopClientAttrib)
	      (gl-unmake-current window)
	      )))))))


;(setf (image-modulation-color (display-attributes (top-view)))  (cv .8 .8 .8 1.0))

;;;
;;; Offscreen methods: A lot of this duplicates the tk-gl window
;;; methods, suggesting that we probably want a GL abstraction layer
;;; that handles GL-specific stuff.
;;;
;;; This might be a good starting point for a McClim port, since this
;;; window class is divorced from Tcl/Tk.
;;;

#-mswindows
(defmethod gl-make-current ((window gl-offscreen-window))
  (glMakeCurrent window) 
  (handle_gl_errors "(glMakeCurrent window) in gl-make-current ")
  (glDrawBuffer GL_BACK)
  (handle_gl_errors "(glDrawBuffer GL_BACK) in gl-make-current ")
  )


#-mswindows
(defmethod copy-to-front ((window gl-offscreen-window) &optional (unmake-current nil))
  (glFinish)
  (glMakeCurrent window)
  (normalize-gl-state)	   ;; This is (apparently) important for indirect rendering
  (glReadBuffer GL_BACK)
  (glDrawBuffer GL_FRONT)
  (mv-bind (width height) (dimensions window)
    (glCopyPixels 0 0 width height GL_COLOR))
  (when unmake-current (glUnmakeCurrent window))
  )


#-mswindows
(defmethod render-window ((window gl-offscreen-window) from-backing-store)
;;  (format t "~%RENDER-WINDOW on gl-offscreen-window") (force-output)
  (let* ((view (top-view window))
	 (*current-view* view))
    (when (and from-backing-store
	       (backing-store-valid-p window))
      (copy-to-front window)
      (render-selected-objects-in-front-buffer view)
      (return-from render-window nil))

    (clear-feedback-cache)
    ;; (format t "~%Cleared feedback cache.") (force-output)

    ;; Handled by the RENDER-WINDOW :AROUND method for BASIC-WINDOW:
    ;; (glMakeCurrent window)
    ;; (glFinish)				; necessary for cross-machine 

    (glDisable GL_DEPTH_TEST)

    (glDrawBuffer GL_BACK)
    (render-stationary-objects view)
    (copy-to-front window)
    (render-selected-objects-in-front-buffer view)
    (setf (back-buffer-state window) nil) ;; (visible-p window))
    ))


;;;
;;; Should this be an around method?  Probably not.  We should fold
;;; this into the existing RENDER-WINDOW methods:
;;;
(defmethod render-window :around ((window basic-window) from-backing-store)
  (gl::with-gl-window (window) (call-next-method)))


#-mswindows
(defmethod redisplay ((window gl-offscreen-window) &rest args &key &allow-other-keys)
  (let ((view (top-view window)))
    (if view
        (apply 'redisplay view args)
        (clear-window window)
        )))
