(in-package :gl)

(def-foreign-synonym-type dummy-pointer :unsigned-long)
(def-foreign-synonym-type agl-context dummy-pointer)
(def-foreign-synonym-type agl-drawable dummy-pointer)
(defconstant GL_UNPACK_CLIENT_STORAGE_APPLE  #x85B2)


(lcl::def-foreign-struct agl_rendering_spec
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
;;; AGL is deprecated and will not work in 64-bit mode.  Cocoa should be used instead.
;;; Here is a list of agl ffi entry points:
;;;
;;; aglMakeCurrent(context) - sets the current OpenGL graphics context
;;; aglSwapBuffers(window_id, context, width, height) - swap front and back buffers
;;; aglPreDisplay(window_id, context, width, height) - Pre-display preparation hooks
;;; aglMakeRenderingSpec() - ??
;;; aglCreateContext(window_id, pixel_format, shared_context, width, height)
;;; aglChooseAndSetPixelFormat

;;; Freedius-specific AGL functions are given the aglXXX name, but
;;; declared as FREEDIUS_GLOBAL:

(def-foreign-function (aglMakeCurrent (:name "aglSetCurrentContext"))
  (ctx agl-context))

(def-foreign-function (aglSwapBuffers (:name (freedius-prefix "aglSwapBuffers"))
				      (:return-type :int))
  (window-id agl-drawable)
  (ctx agl-context)
  (width :int)
  (height :int))


(def-foreign-function (aglPreDisplay (:name (freedius-prefix "aglPreDisplay"))
				      (:return-type :int))
  (window-id agl-drawable)
  (ctx agl-context)
  (width :int)
  (height :int))


(def-foreign-function (aglMakeRenderingSpec
			(:name (freedius-prefix "aglMakeRenderingSpec"))
			(:return-type (:pointer agl_rendering_spec)))
  )


(def-foreign-function (aglCreateContext
		       (:name (freedius-prefix "aglCreateContext"))
		       (:return-type agl-context))
  (window-id agl-drawable)
  (pixel-format :int)
  (share agl-context)
  (width :int)
  (height :int))


(def-foreign-function (aglChooseAndSetPixelformat
			(:name (freedius-prefix "aglChooseAndSetPixelformat"))
			(:return-type :int))
  (spec (:pointer agl_rendering_spec)))


		       
;;; This accepts a keyword based attribute list for making a rendering context.
(defun set-agl_rendering_spec (spec opengl-attribute-list)
  (let ((attrs opengl-attribute-list)
	(red-size 0) (grn-size 0) (blu-size 0)
	(accum-red-size 0) (accum-grn-size 0) (accum-blu-size 0))

    (setf (agl_rendering_spec-RgbaFlag spec) 0
	  (agl_rendering_spec-ColorBits spec) 0
	  (agl_rendering_spec-DoubleFlag spec) 0
	  (agl_rendering_spec-DepthSize spec) 0
	  (agl_rendering_spec-AccumBits spec) 0
	  (agl_rendering_spec-AlphaSize spec) 0
	  (agl_rendering_spec-StencilSize spec) 0
	  (agl_rendering_spec-OverlayFlag spec) 0
	  (agl_rendering_spec-StereoFlag spec) 0
	  (agl_rendering_spec-AuxNumber spec) 0)
	  
    (loop for attr = (pop attrs)
	  while attr	  
	  do (case attr
	       (:rgba
		(setf (agl_rendering_spec-RgbaFlag spec) 1))
	       (:alpha-size
		(setf (agl_rendering_spec-AlphaSize spec) (pop attrs)))
	       (:depth-size
		(setf (agl_rendering_spec-DepthSize spec) (pop attrs)))
	       (:stencil-size
		(setf (agl_rendering_spec-StencilSize spec) (pop attrs)))
	       (:doublebuffer
		(setf (agl_rendering_spec-DoubleFlag spec) 1))
	       (:overlay
		(setf (agl_rendering_spec-OverlayFlag spec) 1))
	       (:stereo
		(setf (agl_rendering_spec-StereoFlag spec) 1))
	       (:aux-buffers
		(setf (agl_rendering_spec-AuxNumber spec) (pop attrs)))
	       
	       (:red-size (setf red-size (pop attrs)))
	       (:green-size (setf grn-size (pop attrs)))
	       (:blue-size (setf blu-size (pop attrs)))
	       (:accum-red-size (setf accum-red-size (pop attrs)))
	       (:accum-green-size (setf accum-grn-size (pop attrs)))
	       (:accum-blue-size (setf accum-blu-size (pop attrs)))))

    (setf (agl_rendering_spec-ColorBits spec)
	  (+ red-size grn-size blu-size)
	  (agl_rendering_spec-AccumBits spec)
	  (+ accum-red-size accum-grn-size accum-blu-size))

   ;; (format t "agl_rendering_spec ~a~%" (get-agl_rendering_spec spec))

    spec))


(defun get-agl_rendering_spec (spec)
  (list :rgba        (agl_rendering_spec-RgbaFlag spec)
	:alpha-size  0
	:depth-size  (agl_rendering_spec-DepthSize spec)
	:stencil-size (agl_rendering_spec-StencilSize spec)
	:doublebuffer (if (= (agl_rendering_spec-DoubleFlag spec) 1) t nil)
	:overlay      (if (= (agl_rendering_spec-OverlayFlag spec) 1) t nil)
	:stereo       (if (= (agl_rendering_spec-StereoFlag spec) 1) t nil)
	:aux-buffers  (agl_rendering_spec-AuxNumber spec)
	:red-size     0
	:green-size   0
	:blue-size    0
	:color-size   (agl_rendering_spec-ColorBits spec)
	))



(def-foreign-function (aglHideCursor (:name (freedius-prefix "aglHideCursor"))
				      (:return-type :int))
  )

(def-foreign-function (aglShowCursor (:name (freedius-prefix "aglShowCursor"))
				      (:return-type :int))
  )

#+qua
(def-foreign-function (aglSetMouseCoalescing (:name (freedius-prefix "aglSetMouseCoalescing"))
				      (:return-type :int))
    (state :int)
  )

(def-foreign-function (aglWarpCursor (:name (freedius-prefix "aglWarpCursor"))
				      (:return-type :int))
  (screen-x :int)
  (screen-y :int)
  )

(def-foreign-function (aglAssociateMouseAndMouseCursorPosition
		       (:name (freedius-prefix "aglAssociateMouseAndMouseCursorPosition"))
				     (:return-type :int))
    (connect :int)
  )

(def-foreign-function (aglGetLastMouseDelta (:name (freedius-prefix "aglGetLastMouseDelta")))
    (deltas :simple-array) ; (:simple-array (signed-byte 32))
)


;;; A no-op under Aqua:
;;(defun xdisplay-and-screen-num (string) (values 0 0))

#|

(defun get-mouse-deltas ()
  (let ((deltas (make-array 2 :element-type '(signed-byte 32))))
    (aglGetLastMouseDelta deltas)
    (values (aref deltas 0) (aref deltas 1))))

(defmacro with-mouse-grabbed (&body body)
  `(unwind-protect 
	(progn (aglAssociateMouseAndMouseCursorPosition 0)
	       . ,body)
     (aglAssociateMouseAndMouseCursorPosition 1)))


(let ((tk::*events-verbose* t))
  (with-mouse-grabbed 
    (loop do (tk::wait-for-any-event tk::TCL_ALL_EVENTS )
	     (mv-bind (dx dy) (get-mouse-deltas)
	       (unless (and (= dx 0) (= dy 0))
		 (format t "~a ~%" (list dx dy)))))))


(setq ext::*gc-verbose* nil)
(setq ext::*gc-verbose* t)
(ext::gc :full t)
(ext::gc)

(swank::background-message "FOO")
|#
