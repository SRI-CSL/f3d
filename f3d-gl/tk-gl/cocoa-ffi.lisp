(in-package :gl)

(def-foreign-synonym-type dummy-pointer :unsigned-long)
(def-foreign-synonym-type fns-context dummy-pointer)
(def-foreign-synonym-type fns-view dummy-pointer)
(def-foreign-synonym-type fns-drawable dummy-pointer)
(def-foreign-synonym-type fns-pbuffer dummy-pointer)

(defconstant GL_UNPACK_CLIENT_STORAGE_APPLE  #x85B2)


(lcl::def-foreign-struct fns_rendering_spec
    (RgbaFlag :type :signed-32bit)
    (ColorBits :type :signed-32bit)
    (DoubleFlag :type :signed-32bit)
    (DepthSize :type :signed-32bit)
    (AccumBits :type :signed-32bit)
    (AlphaSize :type :signed-32bit)
    (StencilSize :type :signed-32bit)
    (OverlayFlag :type :signed-32bit)
    (StereoFlag :type :signed-32bit)
    (AuxNumber :type :signed-32bit))


;;;
;;; AGL is deprecated and will not work in 64-bit mode.  This ffi
;;; defines Cocoa functions.  We generally change "agl" to "fns"
;;;
;;; aglMakeCurrent(context) - sets the current OpenGL graphics context
;;; aglSwapBuffers(window_id, context, width, height) - swap front and back buffers
;;; aglPreDisplay(window_id, context, width, height) - Pre-display preparation hooks
;;; aglMakeRenderingSpec() - ??
;;; aglCreateContext(window_id, pixel_format, shared_context, width, height)
;;; aglChooseAndSetPixelFormat

;;; Freedius-specific AGL functions are given the aglXXX name, but
;;; declared as FREEDIUS_GLOBAL:

(def-foreign-function (fnsMakeCurrent (:name (freedius-prefix "fnsMakeCurrent")))
    (nsview fns-view)
  (ctx fns-context))

(def-foreign-function (fnsUnMakeViewCurrent (:name (freedius-prefix "fnsUnMakeViewCurrent")))
    (view fns-view))

(def-foreign-function (fnsMakeViewCurrent (:name (freedius-prefix "fnsMakeViewCurrent")))
;;    (window-id fns-drawable)
    (view fns-view)
  (ctx fns-context))

(def-foreign-function (fnsSwapBuffers (:name (freedius-prefix "fnsSwapBuffers"))
				      (:return-type :int))
    #+old (window-id fns-drawable)
  (view fns-context))


#+never
(def-foreign-function (fnsPreDisplay (:name (freedius-prefix "fnsPreDisplay"))
				      (:return-type :int))
  (window-id fns-drawable)
  (ctx fns-context)
  (width :int)
  (height :int))


(def-foreign-function (fnsMakeRenderingSpec
			(:name (freedius-prefix "fnsMakeRenderingSpec"))
			(:return-type (:pointer fns_rendering_spec)))
  )


(def-foreign-function (fnsCreateContext
		       (:name (freedius-prefix "fnsCreateContext"))
		       (:return-type fns-context))
  (spec (:pointer fns_rendering_spec)))



(def-foreign-function (fnsCreatePBuffer
		       (:name (freedius-prefix "fnsCreatePBuffer"))
		       (:return-type fns-pbuffer))
    (ctx fns-context)
  (width :int)
  (height :int))


(def-foreign-function (fnsMakePBufferCurrent
		       (:name (freedius-prefix "fnsMakePBufferCurrent"))
		       (:return-type :int))
    (pbuffer fns-pbuffer)
  (ctx fns-context))


#+undef
(def-foreign-function (fnsCreateNSView
		       (:name (freedius-prefix "fnsCreateNSView"))
		       (:return-type fns-view))
    (window-id fns-drawable)
  )



(def-foreign-function (fnsCreateView
		       (:name (freedius-prefix "fnsCreateView"))
		       (:return-type fns-view))
    (window-id fns-drawable)
  )


(def-foreign-function (fnsCreateOpenGLView
		       (:name (freedius-prefix "fnsCreateOpenGLView"))
		       (:return-type fns-view))
    (window-id fns-drawable)
  (spec (:pointer fns_rendering_spec))
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  )



(def-foreign-function (fnsGetViewBounds
		       (:name (freedius-prefix "fnsGetViewBounds"))
		       (:return-type :int))
    (view fns-view)
  (bounds (:array :double-float)))


(def-foreign-function (fnsSetViewBounds
		       (:name (freedius-prefix "fnsSetViewBounds"))
		       (:return-type :int))
    (view fns-view)
  (x :double-float)
  (y :double-float)
  (w :double-float)
  (h :double-float)
  )


(def-foreign-function (fnsSetViewFrame
		       (:name (freedius-prefix "fnsSetViewFrame"))
		       (:return-type :int))
    (view fns-view)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  )

#+never
(def-foreign-function (fnsAfterDrawing
		       (:name (freedius-prefix "fnsAfterDrawing"))
		       (:return-type :int))
    (view fns-view)
  )

#+never
(def-foreign-function (fnsChooseAndSetPixelformat
			(:name (freedius-prefix "fnsChooseAndSetPixelformat"))
			(:return-type :int))
  (spec (:pointer fns_rendering_spec)))


		       
;;; This accepts a keyword based attribute list for making a rendering context.
(defun set-fns_rendering_spec (spec opengl-attribute-list)
  (let ((attrs opengl-attribute-list)
	(red-size 0) (grn-size 0) (blu-size 0)
	(accum-red-size 0) (accum-grn-size 0) (accum-blu-size 0))

    (setf (fns_rendering_spec-RgbaFlag spec) 0
	  (fns_rendering_spec-ColorBits spec) 0
	  (fns_rendering_spec-DoubleFlag spec) 0
	  (fns_rendering_spec-DepthSize spec) 0
	  (fns_rendering_spec-AccumBits spec) 0
	  (fns_rendering_spec-AlphaSize spec) 0
	  (fns_rendering_spec-StencilSize spec) 0
	  (fns_rendering_spec-OverlayFlag spec) 0
	  (fns_rendering_spec-StereoFlag spec) 0
	  (fns_rendering_spec-AuxNumber spec) 0)
	  
    (loop for attr = (pop attrs)
	  while attr	  
	  do (case attr
	       (:rgba
		(setf (fns_rendering_spec-RgbaFlag spec) 1))
	       (:alpha-size
		(setf (fns_rendering_spec-AlphaSize spec) (pop attrs)))
	       (:depth-size
		(setf (fns_rendering_spec-DepthSize spec) (pop attrs)))
	       (:stencil-size
		(setf (fns_rendering_spec-StencilSize spec) (pop attrs)))
	       (:doublebuffer
		(setf (fns_rendering_spec-DoubleFlag spec) 1))
	       (:overlay
		(setf (fns_rendering_spec-OverlayFlag spec) 1))
	       (:stereo
		(setf (fns_rendering_spec-StereoFlag spec) 1))
	       (:aux-buffers
		(setf (fns_rendering_spec-AuxNumber spec) (pop attrs)))
	       
	       (:red-size (setf red-size (pop attrs)))
	       (:green-size (setf grn-size (pop attrs)))
	       (:blue-size (setf blu-size (pop attrs)))
	       (:accum-red-size (setf accum-red-size (pop attrs)))
	       (:accum-green-size (setf accum-grn-size (pop attrs)))
	       (:accum-blue-size (setf accum-blu-size (pop attrs)))))

    (setf (fns_rendering_spec-ColorBits spec)
	  (+ red-size grn-size blu-size)
	  (fns_rendering_spec-AccumBits spec)
	  (+ accum-red-size accum-grn-size accum-blu-size))

    (format t "~%set-fns_rendering_spec: ~a~%" (get-fns_rendering_spec spec))

    spec))


(defun get-fns_rendering_spec (spec)
  (list :rgba        (fns_rendering_spec-RgbaFlag spec)
	:alpha-size  0
	:depth-size  (fns_rendering_spec-DepthSize spec)
	:stencil-size (fns_rendering_spec-StencilSize spec)
	:doublebuffer (if (= (fns_rendering_spec-DoubleFlag spec) 1) t nil)
	:overlay      (if (= (fns_rendering_spec-OverlayFlag spec) 1) t nil)
	:stereo       (if (= (fns_rendering_spec-StereoFlag spec) 1) t nil)
	:aux-buffers  (fns_rendering_spec-AuxNumber spec)
	:red-size     8
	:green-size   8
	:blue-size    8
	:color-size   (fns_rendering_spec-ColorBits spec)
	))



(def-foreign-function (fnsHideCursor (:name (freedius-prefix "fnsHideCursor"))
				      (:return-type :int))
  )

(def-foreign-function (fnsShowCursor (:name (freedius-prefix "fnsShowCursor"))
				      (:return-type :int))
  )

#+qua
(def-foreign-function (fnsSetMouseCoalescing (:name (freedius-prefix "fnsSetMouseCoalescing"))
				      (:return-type :int))
    (state :int)
  )

(def-foreign-function (fnsWarpCursor (:name (freedius-prefix "fnsWarpCursor"))
				      (:return-type :int))
  (screen-x :int)
  (screen-y :int)
  )

#+never
(def-foreign-function (fnsAssociateMouseAndMouseCursorPosition
		       (:name (freedius-prefix "fnsAssociateMouseAndMouseCursorPosition"))
				     (:return-type :int))
    (connect :int)
  )

#+never
(def-foreign-function (fnsGetLastMouseDelta (:name (freedius-prefix "fnsGetLastMouseDelta")))
    (deltas :simple-array) ; (:simple-array (signed-byte 32))
)

(def-foreign-function (fnsactivateme (:name (freedius-prefix "fnsActivateMe")))
    )


;;;
;;; If we get here, :cocoa should already be on the *features* list,
;;; but leave this for now:
;;;
(eval-when (eval load)
  (pushnew :cocoa *features*))
