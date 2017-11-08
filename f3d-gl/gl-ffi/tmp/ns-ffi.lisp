(in-package :gl)

(def-foreign-synonym-type dummy-pointer :unsigned-long)
(def-foreign-synonym-type ns-context dummy-pointer)
(def-foreign-synonym-type ns-drawable dummy-pointer)
(defconstant GL_UNPACK_CLIENT_STORAGE_APPLE  #x85B2)


(lcl::def-foreign-struct ns_rendering_spec
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
;;; aglChooseAndSetPixelFormat(agl_rendering_spec)


;;; Freedius-specific NS functions are given the nsXXX name, but
;;; declared as FREEDIUS_GLOBAL:

(def-foreign-function (nsMakeCurrent (:name "nsSetCurrentContext"))
  (ctx ns-context))

(def-foreign-function (nsSwapBuffers (:name (freedius-prefix "nsSwapBuffers"))
				      (:return-type :int))
  (window-id ns-drawable)
  (ctx ns-context)
  (width :int)
  (height :int))


(def-foreign-function (nsPreDisplay (:name (freedius-prefix "nsPreDisplay"))
				      (:return-type :int))
  (window-id ns-drawable)
  (ctx ns-context)
  (width :int)
  (height :int))


(def-foreign-function (nsMakeRenderingSpec
			(:name (freedius-prefix "nsMakeRenderingSpec"))
			(:return-type (:pointer ns_rendering_spec)))
  )


(def-foreign-function (nsCreateContext
		       (:name (freedius-prefix "nsCreateContext"))
		       (:return-type ns-context))
  (window-id ns-drawable)
  (pixel-format :int)
  (share ns-context)
  (width :int)
  (height :int))


(def-foreign-function (nsChooseAndSetPixelformat
			(:name (freedius-prefix "nsChooseAndSetPixelformat"))
			(:return-type :int))
  (spec (:pointer ns_rendering_spec)))


		       
;;; This accepts a keyword based attribute list for making a rendering context.
(defun set-ns_rendering_spec (spec opengl-attribute-list)
  (let ((attrs opengl-attribute-list)
	(red-size 0) (grn-size 0) (blu-size 0)
	(accum-red-size 0) (accum-grn-size 0) (accum-blu-size 0))

    (setf (ns_rendering_spec-RgbaFlag spec) 0
	  (ns_rendering_spec-ColorBits spec) 0
	  (ns_rendering_spec-DoubleFlag spec) 0
	  (ns_rendering_spec-DepthSize spec) 0
	  (ns_rendering_spec-AccumBits spec) 0
	  (ns_rendering_spec-AlphaSize spec) 0
	  (ns_rendering_spec-StencilSize spec) 0
	  (ns_rendering_spec-OverlayFlag spec) 0
	  (ns_rendering_spec-StereoFlag spec) 0
	  (ns_rendering_spec-AuxNumber spec) 0)
	  
    (loop for attr = (pop attrs)
	  while attr	  
	  do (case attr
	       (:rgba
		(setf (ns_rendering_spec-RgbaFlag spec) 1))
	       (:alpha-size
		(setf (ns_rendering_spec-AlphaSize spec) (pop attrs)))
	       (:depth-size
		(setf (ns_rendering_spec-DepthSize spec) (pop attrs)))
	       (:stencil-size
		(setf (ns_rendering_spec-StencilSize spec) (pop attrs)))
	       (:doublebuffer
		(setf (ns_rendering_spec-DoubleFlag spec) 1))
	       (:overlay
		(setf (ns_rendering_spec-OverlayFlag spec) 1))
	       (:stereo
		(setf (ns_rendering_spec-StereoFlag spec) 1))
	       (:aux-buffers
		(setf (ns_rendering_spec-AuxNumber spec) (pop attrs)))
	       
	       (:red-size (setf red-size (pop attrs)))
	       (:green-size (setf grn-size (pop attrs)))
	       (:blue-size (setf blu-size (pop attrs)))
	       (:accum-red-size (setf accum-red-size (pop attrs)))
	       (:accum-green-size (setf accum-grn-size (pop attrs)))
	       (:accum-blue-size (setf accum-blu-size (pop attrs)))))

    (setf (ns_rendering_spec-ColorBits spec)
	  (+ red-size grn-size blu-size)
	  (ns_rendering_spec-AccumBits spec)
	  (+ accum-red-size accum-grn-size accum-blu-size))

   ;; (format t "ns_rendering_spec ~a~%" (get-ns_rendering_spec spec))

    spec))


(defun get-ns_rendering_spec (spec)
  (list :rgba        (ns_rendering_spec-RgbaFlag spec)
	:alpha-size  0
	:depth-size  (ns_rendering_spec-DepthSize spec)
	:stencil-size (ns_rendering_spec-StencilSize spec)
	:doublebuffer (if (= (ns_rendering_spec-DoubleFlag spec) 1) t nil)
	:overlay      (if (= (ns_rendering_spec-OverlayFlag spec) 1) t nil)
	:stereo       (if (= (ns_rendering_spec-StereoFlag spec) 1) t nil)
	:aux-buffers  (ns_rendering_spec-AuxNumber spec)
	:red-size     0
	:green-size   0
	:blue-size    0
	:color-size   (ns_rendering_spec-ColorBits spec)
	))



;;;
;;; The CoreGraphics API is still usable, but we may want to see if NS
;;; offers any cleaner ways to deal with the mouse:

(def-foreign-function (nsHideCursor (:name (freedius-prefix "nsHideCursor"))
				      (:return-type :int))
  )

(def-foreign-function (nsShowCursor (:name (freedius-prefix "nsShowCursor"))
				      (:return-type :int))
  )


#+aqua
(def-foreign-function (nsSetMouseCoalescing (:name (freedius-prefix "nsSetMouseCoalescing"))
				      (:return-type :int))
    (state :int)
  )

(def-foreign-function (nsWarpCursor (:name (freedius-prefix "nsWarpCursor"))
				      (:return-type :int))
  (screen-x :int)
  (screen-y :int)
  )

(def-foreign-function (nsAssociateMouseAndMouseCursorPosition
		       (:name (freedius-prefix "nsAssociateMouseAndMouseCursorPosition"))
				     (:return-type :int))
    (connect :int)
  )

(def-foreign-function (nsGetLastMouseDelta (:name (freedius-prefix "nsGetLastMouseDelta")))
    (deltas :simple-array) ; (:simple-array (signed-byte 32))
)


;;; A no-op under Aqua:
;;(defun xdisplay-and-screen-num (string) (values 0 0))

#|

(defun get-mouse-deltas ()
  (let ((deltas (make-array 2 :element-type '(signed-byte 32))))
    (nsGetLastMouseDelta deltas)
    (values (aref deltas 0) (aref deltas 1))))

(defmacro with-mouse-grabbed (&body body)
  `(unwind-protect 
	(progn (nsAssociateMouseAndMouseCursorPosition 0)
	       . ,body)
     (ns AssociateMouseAndMouseCursorPosition 1)))


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
