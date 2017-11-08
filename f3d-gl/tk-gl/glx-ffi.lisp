(in-package :gl)

;;; This really needn't be in the gl package.

;;; Why isn't this in the file x11-error-handler.lisp?
(st::add-system-initialization :tkgl '(install-lisp-error-handler))

#|
These constants from /usr/include/GL/glx.h
|#

(defconstant GLX_USE_GL 1)
(defconstant GLX_BUFFER_SIZE 2)
(defconstant GLX_LEVEL 3)
(defconstant GLX_RGBA 4)
(defconstant GLX_DOUBLEBUFFER 5)
(defconstant GLX_STEREO 6)
(defconstant GLX_AUX_BUFFERS 7)
(defconstant GLX_RED_SIZE 8)
(defconstant GLX_GREEN_SIZE 9)
(defconstant GLX_BLUE_SIZE 10)
(defconstant GLX_ALPHA_SIZE 11)
(defconstant GLX_DEPTH_SIZE 12)
(defconstant GLX_STENCIL_SIZE 13)
(defconstant GLX_ACCUM_RED_SIZE 14)
(defconstant GLX_ACCUM_GREEN_SIZE 15)
(defconstant GLX_ACCUM_BLUE_SIZE 16)
(defconstant GLX_X_VISUAL_TYPE #x22)

(defconstant GLX_TRUE_COLOR #x8002)
(defconstant GLX_DIRECT_COLOR #x8003)
(defconstant GLX_PSEUDO_COLOR #x8004 )
(defconstant GLX_STATIC_COLOR #x8005)
(defconstant GLX_GRAY_SCALE #x8006)
(defconstant GLX_STATIC_GRAY #x8007)

(defconstant GLX_PRESERVED_CONTENTS #x801B)
(defconstant GLX_LARGEST_PBUFFER #x801C)
(defconstant GLX_PBUFFER_HEIGHT #x8040)
(defconstant GLX_PBUFFER_WIDTH #x8041)
(defconstant GLX_HEIGHT #x801E)
(defconstant GLX_WIDTH #x801D)

(defconstant GLX_MAX_PBUFFER_WIDTH #x8016)
(defconstant GLX_MAX_PBUFFER_HEIGHT #x8017)
(defconstant GLX_MAX_PBUFFER_PIXELS #x8018)
(defconstant GLX_VISUAL_ID #x800B)

(defconstant GLX_RGBA_TYPE #x8014)
(defconstant GLX_RENDER_TYPE #x8011)

(defconstant GLX_WINDOW_BIT #x00000001)
(defconstant GLX_PIXMAP_BIT #x00000002)
(defconstant GLX_PBUFFER_BIT #x00000004)


(defparameter *default-glx-attribute-list-depth-16*
  (list gl::GLX_RGBA
	gl::GLX_RED_SIZE 5
	gl::GLX_GREEN_SIZE 5
	gl::GLX_BLUE_SIZE 5
	;;gl::GLX_ALPHA_SIZE 1
	gl::GLX_DOUBLEBUFFER
	gl::GLX_DEPTH_SIZE 16))

#-macosx
(defparameter *default-glx-attribute-list-depth-24*
  (list gl::GLX_RGBA
	gl::GLX_RED_SIZE 5
	gl::GLX_GREEN_SIZE 5
	gl::GLX_BLUE_SIZE 5
	;;gl::GLX_ALPHA_SIZE 1
	gl::GLX_DOUBLEBUFFER
	gl::GLX_DEPTH_SIZE 24))

#+macosx
(defparameter *default-glx-attribute-list-depth-24*
  (list gl::GLX_RGBA
	gl::GLX_RED_SIZE 8
	gl::GLX_GREEN_SIZE 8
	gl::GLX_BLUE_SIZE 8
	;;gl::GLX_ALPHA_SIZE 1
	gl::GLX_DOUBLEBUFFER
	gl::GLX_DEPTH_SIZE 24))

(defparameter *default-glx-attribute-list-depth-32*
  (list gl::GLX_RGBA
	gl::GLX_RED_SIZE 8
	gl::GLX_GREEN_SIZE 8
	gl::GLX_BLUE_SIZE 8
	;;gl::GLX_ALPHA_SIZE 1
	gl::GLX_DOUBLEBUFFER
	gl::GLX_DEPTH_SIZE 32))


;;; Set of attribute choices in preferred order
(defmethod default-glx-attribute-lists ((window t))
  (list *default-glx-attribute-list-depth-32*
	*default-glx-attribute-list-depth-24*
	*default-glx-attribute-list-depth-16*))

;;; should these be pointers or :unsigned-byte32? 
(def-foreign-synonym-type GLXContext dummy-pointer)
(def-foreign-synonym-type XVisualInfo dummy-pointer)
(def-foreign-synonym-type XVisual dummy-pointer)
(def-foreign-synonym-type XDisplay dummy-pointer)
(def-foreign-synonym-type XWindow dummy-pointer)

(def-foreign-function (glXChooseVisual_int (:name "glXChooseVisual")
					   (:return-type XVisualInfo))
  (display XDisplay)
  (screen-num :int)
  (attr-list :array))

(def-foreign-function (glXCreateContext_int (:name "glXCreateContext")
					    (:return-type GLXContext))
  (display XDisplay)
  (xvisualinfo XVisualInfo)
  (sharelist GLXContext)		; 0 or GLXContext
  (direct :int))

(def-foreign-function (glX-is-direct (:name "glXIsDirect"))
  (display XDisplay)
  (context GLXContext))

(def-foreign-function (XFree (:name "XFree") (:return-type :null))
  (pointer dummy-pointer))

(def-foreign-function (glXMakeCurrent (:name "glXMakeCurrent"))
  (display XDisplay)
  (window XWindow)
  (context GLXContext))

(def-foreign-function (glXMakeContextCurrent (:name "glXMakeContextCurrent"))
  (display XDisplay)
  (draw XWindow)
  (read XWindow)
  (context GLXContext))

(def-foreign-function (glXSwapBuffers (:name "glXSwapBuffers"))
  (display XDisplay)
  (window XWindow))

(def-foreign-function (XCreatePixmap (:name "XCreatePixmap")
                                     (:return-type :int))
  (display XDisplay)
  (screen-num :int)
  (width :int)
  (height :int)
  (depth :int))

(def-foreign-function (XRootWindow (:name "XRootWindow")
                                     (:return-type XWindow))
  (display XDisplay)
  (screen-num :int))

#+unused
(def-foreign-function (glXGetCurrentDisplay (:name "glXGetCurrentDisplay")
					    (:return-type :int)))
#+unused
(def-foreign-function (glXGetCurrentWindow (:name "glXGetCurrentDrawable")
					   (:return-type :int)))
#+unused
(def-foreign-function (glXGetCurrentContext (:name "glXGetCurrentContext")
					   (:return-type :int)))



(def-foreign-function (F3D_visinfo_visual (:name (freedius-prefix "visinfo_visual")) (:return-type XVisual))
  (visinfo XVisualInfo))

(def-foreign-function (F3D_visinfo_visualid (:name (freedius-prefix "visinfo_visualid")) (:return-type :int))
  (visinfo XVisualInfo))

;;; not used
(def-foreign-function (F3D_XCreateWindow (:name (freedius-prefix "XCreateWindow"))
					 (:return-type XWindow))
  (display XDisplay)
  (parent XWindow)
  (width :int)
  (height :int)
  (visinfo XVisualInfo))

(def-foreign-function (XMapWindow (:name "XMapWindow")
				  (:return-type :int))
  (display XDisplay)
  (window Xwindow))
  




(def-foreign-function (XOpenDisplay_int (:name "XOpenDisplay") (:return-type XDisplay))
  (display-name :simple-string))

(defun-cached XOpenDisplay (display-name)
  (XOpenDisplay_int display-name))

;;; make this smarter
(defun extract-screen-spec (string)
  string)

(defun-cached xdisplay-and-screen-num (screen-spec)
  (values (XOpenDisplay screen-spec)
	  (read-from-string screen-spec nil 0 :start (1+ (position #\. screen-spec :from-end t)))))

;;; xdisplay should be in a different package?
;;;(defun tk::xdisplay (&optional (screen ":0"))
;;;  (XOpenDisplay (cond ((numberp screen)
;;;                       (format nil ":~d" screen))
;;;                      ((stringp screen) (extract-screen-spec screen))
;;;                      (t (error "tk::xdisplay: invalid screen-spec")))))


;;; This probably should be cached.  
;;;(defun glXChooseVisual (attr-list &optional xdisplay (screen-num 0))
;;;  (unless xdisplay (setq xdisplay (tk::xdisplay screen-num)))
;;;  (let ((attrs (make-array (1+ (length attr-list)) :element-type '(signed-byte 32))))
;;;    (loop for attr in attr-list
;;;          for i from 0
;;;          do (setf (aref attrs i) attr))
;;;    (setf (aref attrs (length attr-list)) 0) ; NULL terminate the vector
;;;
;;;    (glXChooseVisual_int xdisplay screen-num attrs)))

(defun make-glx-attribute-array (attr-list)
  (let ((attrs (make-array0 (1+ (length attr-list)) :element-type '(signed-byte 32))))
    (loop for attr in attr-list
	  for i from 0
	  do (setf (aref attrs i) attr))
    (setf (aref attrs (length attr-list)) 0) ; NULL terminate the vector
    attrs))

#|
(eval-cache-flush-function 'glXChooseVisual)
|#

;;; Sun Feb  1 2004 LHQ:  attr-list can now be a list of attr-lists.
(defun glXChooseVisual (attr-lists &optional xdisplay (screen-num 0))
  ;; Make sure we have a list of attr-lists.
  (format t "glXChooseVisual:  attr-lists = ~a ; xdisplay = ~a" attr-lists xdisplay)
  (eval-cache (glXChooseVisual attr-lists screen-num)
      (when (numberp (car attr-lists))
	(setq attr-lists (list attr-lists)))
    ;;(unless xdisplay (setq xdisplay (tk::xdisplay screen-num)))
    (unless xdisplay (error "glXChooseVisual must supply xdisplay~%"))
    (loop for attr-list in attr-lists
	  for visualinfo
	    = (glXChooseVisual_int xdisplay screen-num (print (make-glx-attribute-array attr-list)))
	  unless (= visualinfo 0)
	    return visualinfo			 
	  finally (return 0))))

;;; This probably should be cached
;;;(defun glXChooseVisualid (visual-specs &optional xdisplay (screen-num 0))
;;;  (let ((visualinfo (glXChooseVisual visual-specs xdisplay screen-num)))
;;;    (if (= visualinfo 0)
;;;        0
;;;        (prog1 (F3D_visinfo_visualid visualinfo)
;;;          (XFree visualinfo)))))

(defun glXChooseVisualid (visual-specs &optional xdisplay (screen-num 0))
  (let ((visualinfo (glXChooseVisual visual-specs xdisplay screen-num)))
    (if (= visualinfo 0)
	0
	(F3D_visinfo_visualid visualinfo))))


;;; This isn't cached.  We generate a new glX context for each window
;;; since their OpenGL states can be changed independently.
(defun glXCreateContext (&key xvisualinfo attr-list xdisplay (screen-num 0) 
			 (sharelist 0) (direct (gl-is-direct-p))
			 error-ok)
  (setq direct (if direct 1 0))
  (let ((xdisplay (or xdisplay (error "glXCreateContext must supply xdisplay~%")))
	(context (if xvisualinfo
		     (glXCreateContext_int xdisplay xvisualinfo sharelist direct)
	
		     (let ((xvisualinfo (glXChooseVisual attr-list xdisplay screen-num)))
		       (if (eql xvisualinfo 0)
			   (if error-ok
			       0
			       (error "glXCreateContext: Failed for attr-list = ~a~%" attr-list))
			   (glXCreateContext_int xdisplay xvisualinfo sharelist direct))))))
    (if (zerop context)
	(error "glXCreateContext failed: ~a ~a ~a " xdisplay xvisualinfo sharelist)
	context)))

;;; This version is cached.  We share the same context for everything.
(defun glXCreateContext (&key xvisualinfo attr-list xdisplay (screen-num 0) 
			 (sharelist 0) (direct (gl-is-direct-p))
			 error-ok)
  (setq direct (if direct 1 0))
  (eval-cache (glXCreateContext xvisualinfo)
      (let* ((xdisplay (or xdisplay (error "glXCreateContext must supply xdisplay~%")))
	     (context (if xvisualinfo
			  (glXCreateContext_int xdisplay xvisualinfo sharelist direct)
	
			  (let ((xvisualinfo (glXChooseVisual attr-list xdisplay screen-num)))
			    (if (eql xvisualinfo 0)
				(if error-ok
				    0
				    (error "glXCreateContext: Failed for attr-list = ~a~%" attr-list))
				(glXCreateContext_int xdisplay xvisualinfo sharelist direct))))))
	(if (zerop context)
	    (progn (setq *glXCreateContext* (list xvisualinfo attr-list direct screen-num))
		   (error "glXCreateContext failed: ~a ~a ~a " xdisplay xvisualinfo sharelist))
	    context))))

(defun-cached get-global-glx-sharelist-context (visinfo screen-spec)
  ;;(format t "get-global-glx-sharelist-context ~a ~a~%" visinfo screen-spec)
  (multiple-value-bind (xdisplay screen-num)
      (xdisplay-and-screen-num screen-spec)
    (glXCreateContext :xvisualinfo visinfo
		      :xdisplay xdisplay
		      :screen-num screen-num
		      :sharelist 0)))


  


;;; New Thu Feb 26 2004
;;; This requires glx-1.3 with pbuffer extensions.

;;; should these be pointers or :unsigned-byte32?
(def-foreign-synonym-type GLXPbuffer dummy-pointer)
(def-foreign-synonym-type GLXWindow dummy-pointer)
(def-foreign-synonym-type GLXFBConfig dummy-pointer)
(def-foreign-synonym-type GLXPixmap dummy-pointer)

(def-foreign-function (glXCreateNewContext (:name "glXCreateNewContext")
					    (:return-type GLXContext))
  (display XDisplay)
  (config GLXFBConfig)
  (rendertype :int)
  (sharelist GLXContext)		; 0 or GLXContext
  (direct :int))

(def-foreign-function (glXCopyContext (:name "glXCopyContext"))
  (display XDisplay)
  (source GLXContext)
  (dest GLXContext)
  (mask :unsigned-32bit))

(def-foreign-function (glXChooseFBConfig_int (:name "glXChooseFBConfig")
					     (:return-type GLXFBConfig))
  (display xdisplay)
  (screen :int)
  (attr-list :array)			; :array (:unsigned-32bit)
  (nelements :array)		       ; :array (:unsigned-32bit)   return value
  )

(def-foreign-function (glXGetFBConfigs_int (:name "glXGetFBConfigs")
					   (:return-type GLXFBConfig))
  (display xdisplay)
  (screen :int)
  (nelements :array)		      ; :array (:unsigned-32bit)    return value
  )


(def-foreign-function (glXQueryDrawable_int (:name "glXQueryDrawable")
					(:return-type :int))
  (display xdisplay)
  (drawable XWindow)
  (attrib :int)
  (value :array))

(defun glXQueryDrawable (xdisplay drawable attrib)
  (and (zerop (glXQueryDrawable_int xdisplay drawable attrib
				    (glXGetFBConfigs-array)))
       (aref (glXGetFBConfigs-array) 0)))
  

(def-foreign-function (glXGetFBConfigAttrib_int (:name "glXGetFBConfigAttrib")
					    (:return-type :int))
  (display xdisplay)
  (config GLXFBConfig)
  (attrib :int)
  (value :array) ; :array (:unsigned-32bit)   return value
  )

(defun glXGetFBConfigAttrib (xdisplay GLXFBConfig attrib)
  (let ((array (glXGetFBConfigs-array)))
    (and (zerop (glXGetFBConfigAttrib_int xdisplay GLXFBConfig attrib array))
	 (aref array 0))))

(def-foreign-function (glXGetVisualFromFBConfig (:name "glXGetVisualFromFBConfig")
						(:return-type XVisualInfo))
  (display xdisplay)
  (config GLXFBConfig))

(def-foreign-function (getGLXFBConfig_from_array
			(:name "getGLXFBConfig_from_array")
			(:return-type GLXFBConfig))
  (array GLXFBConfig) ; FIXME this is a pointer to an array of GLXFBConfig
  (index :int))

(defvar *glXGetFBConfigs-array* nil)
;(setq *glXGetFBConfigs-array* nil)
;(glXGetFBConfigs-array)
(defun glXGetFBConfigs-array ()
  (or *glXGetFBConfigs-array*
      (setq *glXGetFBConfigs-array* (make-foreign-vector 1 :element-type '(signed-byte 32)))))


(defparameter *glXGetFBConfigs-list-attributes*
  '(GLX_LEVEL GLX_RED_SIZE GLX_GREEN_SIZE GLX_BLUE_SIZE
    GLX_ALPHA_SIZE GLX_DOUBLEBUFFER GLX_DEPTH_SIZE
    GLX_STEREO GLX_STENCIL_SIZE GLX_ACCUM_RED_SIZE
    GLX_X_VISUAL_TYPE GLX_VISUAL_ID GLX_RENDER_TYPE))


(defun-cached glXGetFBConfigs (xdisplay &key attrs (screen 0))
  (let* ((array (glXGetFBConfigs-array))
	 (configs (if attrs
		      (glXChooseFBConfig_int xdisplay screen
					     (make-glx-attribute-array attrs)
					     array)
		      (glXGetFBConfigs_int xdisplay screen array)))
	 (nconfigs (aref array 0)))
    (loop for i from 0 beLow nconfigs
	  collect (getGLXFBConfig_from_array configs i))))

;(eval-cache-flush-function 'glXGetFBConfigs)
(defun-cached glXGetFBConfigs (xdisplay &key attrs (screen 0))
  (let* ((nconfigs-array 
	  (make-array 10 :element-type '(unsigned-byte 32))) ; not sure why 10 elements are allocated
	 (attr-arr (make-glx-attribute-array attrs))
	 (configs (if attrs
		      #+sbcl(sb-alien::with-pinned-objects (attr-arr)
			      (glXChooseFBConfig_int xdisplay screen attr-arr nconfigs-array))
		      #-sbcl (glXChooseFBConfig_int xdisplay screen attr-arr nconfigs-array)
		      (glXGetFBConfigs_int xdisplay screen nconfigs-array)))
	 (nconfigs (aref nconfigs-array 0)))
    (when (zerop configs) (error "glXChooseFBConfig_int failed"))
    (setq *foo* (list configs nconfigs-array))
    (loop for i from 0 beLow nconfigs
	  collect (getGLXFBConfig_from_array configs i))))

(defun fbconfig-cost (config xdisplay)
  (flet ((get-attr (attr) (glXGetFBConfigAttrib xdisplay config attr)))
    (let ((double-buffer (get-attr GLX_DOUBLEBUFFER))
	  (rgba-size (+ (get-attr GLX_RED_SIZE) (get-attr GLX_GREEN_SIZE)
			(get-attr GLX_BLUE_SIZE) (get-attr GLX_ALPHA_SIZE))))
	  
      (+ (* (1+ double-buffer) rgba-size)
	 (get-attr GLX_DEPTH_SIZE)
	 (get-attr GLX_STENCIL_SIZE)))))
	    

(defun-cached cost-sort-FBConfigs (xdisplay &key attrs (screen 0))
  (loop for config in (glXGetFBConfigs xdisplay :attrs attrs :screen screen)
	for cost = (fbconfig-cost config xdisplay)
	collect (list cost config) into config-costs
	finally
     (return (loop for (cost config) in (sort config-costs #'< :key #'car)
		   collect config))))

(defun min-cost-FBConfig (xdisplay &key (attrs *FBConfig-rgb8-attributes*)
			  (screen 0))
  (car (cost-sort-FBConfigs xdisplay :attrs attrs :screen screen)))

#|
(xdisplay-and-screen-num ":0.0")
;;; This causes segfault in both CMUCL and SBCL
(defun describe-glXFBConfig (config xdisplay)
  (loop for attr-name in *glXGetFBConfigs-list-attributes*
	for attr-val = (glXGetFBConfigAttrib xdisplay config
						      (symbol-value attr-name))
	collect attr-name
	collect (cond ((eql attr-name 'GLX_X_VISUAL_TYPE)
		       (cond ((eql attr-val GLX_TRUE_COLOR)
			      'GLX_TRUE_COLOR)
			     ((eql attr-val GLX_DIRECT_COLOR)
			      'GLX_DIRECT_COLOR)
			     (t attr-val)))
		      (t attr-val))))

(defun describe-glXFBConfigs (configs xdisplay)
  (loop for config in configs
	collect (list* 'XFBConfig config
				  (describe-glXFBConfig config xdisplay))))
(gl::describe-glXFBConfig
 (gui::default-glXVisualInfo (make-instance 'gui::tk-glx-bbuffer-window) ":0.0"))
|#

(defparameter *glXGetFBConfigs-choose-attributes*
  (list	;;GLX_RGBA
   GLX_RED_SIZE 8
   GLX_GREEN_SIZE 8
   GLX_BLUE_SIZE 8
   ;;GLX_ALPHA_SIZE 0
   GLX_DOUBLEBUFFER 1
   GLX_DEPTH_SIZE 24
   GLX_X_VISUAL_TYPE GLX_TRUE_COLOR
   GLX_STENCIL_SIZE 0
   
   ))

(defparameter *FBConfig-rgb8-arrributtes*
  (list	;;GLX_RGBA
   GLX_RED_SIZE 8
   GLX_GREEN_SIZE 8
   GLX_BLUE_SIZE 8
   ;;GLX_ALPHA_SIZE 0
   GLX_DOUBLEBUFFER 0
   GLX_DEPTH_SIZE 0
   GLX_X_VISUAL_TYPE GLX_TRUE_COLOR
   GLX_STENCIL_SIZE 0
   
   ))

(defparameter *FBConfig-rgb8-attributes*
  (list	;;GLX_RGBA
   GLX_RED_SIZE 8
   GLX_GREEN_SIZE 8
   GLX_BLUE_SIZE 8
   ;;GLX_ALPHA_SIZE 0
   GLX_DOUBLEBUFFER 0
   GLX_DEPTH_SIZE 0
   GLX_X_VISUAL_TYPE GLX_TRUE_COLOR
   GLX_STENCIL_SIZE 0
   
   ))

(defparameter *FBConfig-rgb8-double-attributes*
  (list	;;GLX_RGBA
   GLX_RED_SIZE 8
   GLX_GREEN_SIZE 8
   GLX_BLUE_SIZE 8
   ;;GLX_ALPHA_SIZE 0
   GLX_DOUBLEBUFFER 1
   GLX_DEPTH_SIZE 0
   GLX_X_VISUAL_TYPE GLX_TRUE_COLOR
   GLX_STENCIL_SIZE 0
   
   ))
(defparameter *FBConfig-rgb8-depth24-attributes*
  (list	;;GLX_RGBA
   GLX_RED_SIZE 8
   GLX_GREEN_SIZE 8
   GLX_BLUE_SIZE 8
   ;;GLX_ALPHA_SIZE 0
   GLX_DOUBLEBUFFER 0
   GLX_DEPTH_SIZE 24
   GLX_X_VISUAL_TYPE GLX_TRUE_COLOR
   GLX_STENCIL_SIZE 0
   
   ))

(defparameter *FBConfig-double-rgb8-depth24-attributes*
  (list	;;GLX_RGBA
   GLX_RED_SIZE 8
   GLX_GREEN_SIZE 8
   GLX_BLUE_SIZE 8
   ;;GLX_ALPHA_SIZE 0
   GLX_DOUBLEBUFFER 1
   GLX_DEPTH_SIZE 24
   GLX_X_VISUAL_TYPE GLX_TRUE_COLOR
   GLX_STENCIL_SIZE 0
   
   ))


(defparameter *FBConfig-double-rgb8-depth24-attributes*
  (list	;;GLX_RGBA
   GLX_RED_SIZE 8
   GLX_GREEN_SIZE 8
   GLX_BLUE_SIZE 8
   ;;GLX_ALPHA_SIZE 0
   GLX_DOUBLEBUFFER 1
   GLX_DEPTH_SIZE 24
   GLX_X_VISUAL_TYPE GLX_TRUE_COLOR
   GLX_STENCIL_SIZE 0
   
   ))

(defun pbuffer-dimensions (xdisplay pbuffer)
  (values (glXQueryDrawable xdisplay pbuffer GLX_WIDTH)
	  (glXQueryDrawable xdisplay pbuffer GLX_HEIGHT)))

#|

(eval-cache-flush-function 'glXGetFBConfigs)

(glXGetFBConfigs (tk::xdisplay))

(setq FBConfigs
      (glXGetFBConfigs (tk::xdisplay) :attrs *glXGetFBConfigs-choose-attributes*))

(describe-glXFBConfigs (glXGetFBConfigs (tk::xdisplay) :attrs *glXGetFBConfigs-choose-attributes*))
(describe-glXFBConfigs (glXGetFBConfigs (tk::xdisplay) :attrs *glXGetFBConfigs-choose-attributes2*))

(describe-glXFBConfigs (cost-sort-FBConfigs (tk::xdisplay) :attrs *FBConfig-rgb8-attributes*))

(cost-sort-FBConfigs (tk::xdisplay) :attrs *FBConfig-rgb8-attributes*)
(describe-glXFBConfig
 (min-cost-FBConfig (tk::xdisplay) :attrs *FBConfig-rgb8-attributes*))

(describe-glXFBConfigs 
 (glXGetFBConfigs (tk::xdisplay)
		  :attrs (list ;;GLX_RGBA
			  GLX_RED_SIZE 8
			  GLX_GREEN_SIZE 8
			  GLX_BLUE_SIZE 8
			  ;;GLX_ALPHA_SIZE 0
			  GLX_DOUBLEBUFFER 1
			 ;; GLX_DEPTH_SIZE 24
			 ;; GLX_X_VISUAL_TYPE GLX_TRUE_COLOR
			  GLX_STENCIL_SIZE 0
   
			  )))

(setq pbuffer
      (glXCreatePbuffer (tk::xdisplay)
			(nth 0 FBConfigs)
			(make-glx-attribute-array
			 (list GLX_PBUFFER_WIDTH 512
			       GLX_PBUFFER_HEIGHT 512))))


(glXGetFBConfigAttrib (tk::xdisplay) (nth 0 FBConfigs)
		      GLX_MAX_PBUFFER_PIXELS)

(glXDestroyPbuffer (tk::xdisplay) pbuffer)

(setq pbuffers
      (loop repeat 100
	    for pbuffer = (glXCreatePbuffer (tk::xdisplay)
					    (nth 0 FBConfigs)
					    (make-glx-attribute-array
					     (list GLX_PBUFFER_WIDTH 512
						   GLX_PBUFFER_HEIGHT 512)))
	    until (zerop pbuffer)
	    collect pbuffer))

(length pbuffers)

(loop for pbuffer in pbuffers
      do (glXDestroyPbuffer (tk::xdisplay) pbuffer))
|#
  
(def-foreign-function (glXCreatePbuffer (:name "glXCreatePbuffer")
					(:return-type GLXPbuffer))
  (display xdisplay)
  (config GLXFBConfig)
  (attr-list :array))

(def-foreign-function (glXDestroyPbuffer (:name "glXDestroyPbuffer"))
  (display xdisplay)
  (pbuffer GLXPbuffer))

(def-foreign-function (glXDestroyWindow (:name "glXDestroyWindow"))
  (display xdisplay)
  (window GLXWindow))

(def-foreign-function (glXCreatePixmap (:name "glXCreatePixmap")
				       (:return-type GLXPixmap))
  (display xdisplay)
  (config GLXFBConfig)
  (pixmap GLXPixmap)
  (attr-list :int))

(def-foreign-function (glXDestroyPixmap (:name "glXDestroyPixmap"))
  (display xdisplay)
  (pixmap GLXPixmap))

(def-foreign-function (glXGetCurrentDisplay (:name "glXGetCurrentDisplay")
					    (:return-type xdisplay))
  )

(defun getFBConfig (xdisplay &optional (attrs *glXGetFBConfigs-choose-attributes*))
  (car (glXGetFBConfigs xdisplay  :attrs attrs)))


(defun make-pbuffer (xdisplay config width height)
  ;;(format t "make-pbuffer visualid = ~a~%" (list width height(gl::glXGetFBConfigAttrib xdisplay config gl::GLX_VISUAL_ID )))
  (glXCreatePbuffer xdisplay
		    config
		    (make-glx-attribute-array
		     (list GLX_PBUFFER_WIDTH width
			   GLX_PBUFFER_HEIGHT height))))

(def-foreign-function (XFlush (:name "XFlush"))
  (display xdisplay))

(def-foreign-function (XSync (:name "XSync"))
  (display xdisplay)
  (discard :int))

(defun make-pbuffer (xdisplay config width height)
  (let ((pbuffer (glXCreatePbuffer xdisplay
				   config
				   (make-glx-attribute-array
				    (list GLX_PBUFFER_WIDTH width
					  GLX_PBUFFER_HEIGHT height)))))
    #+never
    (format t "make-pbuffer~a ~a~%"
	    (list width height(gl::glXGetFBConfigAttrib xdisplay config gl::GLX_VISUAL_ID ))
	    pbuffer)
    ;;(XSync xdisplay 0)
    pbuffer))

(defun make-pbuffer-context (screen-spec config visualinfo &optional sharelist)
  (multiple-value-bind (xdisplay screen-num)
      (xdisplay-and-screen-num screen-spec)
    (declare (ignore screen-num))
    (unless sharelist
      (setq sharelist
	    (get-global-glx-sharelist-context visualinfo screen-spec)))
    (glXCreateNewContext xdisplay
			 config
			 GLX_RGBA_TYPE
			 sharelist 
			 (if (gl-is-direct-p) 1 0))))

#+broken
(defun make-pbuffer-and-context (xdisplay config visualinfo width height
				 &optional sharelist )
  (values (make-pbuffer xdisplay config width height)
	  (make-pbuffer-context xdisplay config visualinfo sharelist)))


#| ;; not useful

(defconstant XSetBackingStore_NotUseful 0)
(defconstant XSetBackingStore_WhenMapped 1)
(defconstant XSetBackingStore_Always 2)
					   
(def-foreign-function (XSetBackingStore (:name (freedius-prefix "XSetBackingStore")))
    (display xdisplay) (window xwindow) (flag :signed-32bit))
|#


#| The remainder of this file is inside comments

(trace get-global-glx-sharelist-context)
(XOpenDisplay ":0.0")
(tk::xdisplay )

 
(setq vis
      (glXChooseVisual (list GLX_RGBA
			     GLX_RED_SIZE 5
			     GLX_GREEN_SIZE 5
			     GLX_BLUE_SIZE 5
			     GLX_DOUBLEBUFFER)))
(setq vis
      (glXChooseVisual (list GLX_RGBA
			     GLX_RED_SIZE 8
			     GLX_GREEN_SIZE 8
			     GLX_BLUE_SIZE 8
			     GLX_ALPHA_SIZE 8
			     GLX_DOUBLEBUFFER
			     GLX_DEPTH_SIZE 24
			     GLX_ACCUM_RED_SIZE 16
			     GLX_ACCUM_GREEN_SIZE 16
			     GLX_ACCUM_BLUE_SIZE 16
			     )))


(tk::widget-xwindow (gui::widget (gui::selected-window gui::*interactor*)))

(setq win
      (F3D_XCreateWindow (tk::xdisplay)
			 (tk::widget-xwindow (gui::widget (gui::selected-window gui::*interactor*)))
			 100 100 visinfo))

(gui::widget (gui::selected-window gui::*interactor*))

(tk::tcl-cmd `(frame .frm4.frm.0-0.0-0_gl.foo -width 100 -height 100 ))
(tk::widget-screen (gui::widget (gui::selected-window gui::*interactor*)))

(tk::tcl-cmd `(frame .frm4.frm.0-0.0-0_gl.foo2 -width 100 -height 100
	       -visual ,(F3D_visinfo_visualid visinfo)))

 
(loop for attr-name in '(GLX_RED_SIZE GLX_GREEN_SIZE GLX_BLUE_SIZE GLX_ALPHA_SIZE GLX_DEPTH_SIZE
			 GLX_ACCUM_RED_SIZE GLX_ACCUM_GREEN_SIZE GLX_ACCUM_BLUE_SIZE)
      for attr = (symbol-value attr-name)
      collect (list attr-name (glXGetConfig vis attr)))

(setq glcontext (glXCreateContext vis)) 


;; This works for SGI O2 (I think)
(glXCreateContext :attr-list (list GLX_RGBA
				   GLX_RED_SIZE 5
				   GLX_GREEN_SIZE 5
				   GLX_BLUE_SIZE 5
				   GLX_ALPHA_SIZE 5
				   GLX_DOUBLEBUFFER
				   GLX_DEPTH_SIZE 24
				   ))

;; This works for NVidia card on Linux
(glXCreateContext :attr-list (list GLX_RGBA
				   GLX_RED_SIZE 8
				   GLX_GREEN_SIZE 8
				   GLX_BLUE_SIZE 8
				   GLX_ALPHA_SIZE 8
				   GLX_DOUBLEBUFFER
				   GLX_DEPTH_SIZE 24
				   ))
;; This works for NVidia card on Linux
(glXCreateContext :attr-list (list GLX_RGBA
				   GLX_RED_SIZE 8
				   GLX_GREEN_SIZE 8
				   GLX_BLUE_SIZE 8
				   GLX_ALPHA_SIZE 8
				   GLX_DOUBLEBUFFER
				   GLX_DEPTH_SIZE 24
				   GLX_ACCUM_RED_SIZE 16
				   GLX_ACCUM_GREEN_SIZE 16
				   GLX_ACCUM_BLUE_SIZE 16
				   ))




(glXQueryExtensionsString )


(tk::widget-xwindow (gui::widget (gui::selected-window gui::*interactor*)))

|#



