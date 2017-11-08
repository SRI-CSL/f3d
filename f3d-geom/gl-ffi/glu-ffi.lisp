(in-package :gl)

;;; ALL OF THIS SHOULD USE MKVAR AND DEF-FOREIGN-CONSTANT
;;; 
;;; TessCallback
(defconstant GLU_TESS_BEGIN                       100100)
(defconstant GLU_BEGIN                            100100)
(defconstant GLU_TESS_VERTEX                      100101)
(defconstant GLU_VERTEX                           100101)
(defconstant GLU_TESS_END                         100102)
(defconstant GLU_END                              100102)
(defconstant GLU_TESS_ERROR                       100103)
(defconstant GLU_TESS_EDGE_FLAG                   100104)
(defconstant GLU_EDGE_FLAG                        100104)
(defconstant GLU_TESS_COMBINE                     100105)
(defconstant GLU_TESS_BEGIN_DATA                  100106)
(defconstant GLU_TESS_VERTEX_DATA                 100107)
(defconstant GLU_TESS_END_DATA                    100108)
(defconstant GLU_TESS_ERROR_DATA                  100109)
(defconstant GLU_TESS_EDGE_FLAG_DATA              100110)
(defconstant GLU_TESS_COMBINE_DATA                100111)

;;; TessContour 
(defconstant GLU_CW                               100120)
(defconstant GLU_CCW                              100121)
(defconstant GLU_INTERIOR                         100122)
(defconstant GLU_EXTERIOR                         100123)
(defconstant GLU_UNKNOWN                          100124)

;;; TessProperty 
(defconstant GLU_TESS_WINDING_RULE                100140)
(defconstant GLU_TESS_BOUNDARY_ONLY               100141)
(defconstant GLU_TESS_TOLERANCE                   100142)

;;; TessWinding
(defconstant GLU_TESS_WINDING_ODD                 100130)
(defconstant GLU_TESS_WINDING_NONZERO             100131)
(defconstant GLU_TESS_WINDING_POSITIVE            100132)
(defconstant GLU_TESS_WINDING_NEGATIVE            100133)
(defconstant GLU_TESS_WINDING_ABS_GEQ_TWO         100134)

#+allegro
(def-foreign-struct GLUtesselator (dummy :type :unsigned-long))
#+cmu
(alien:def-alien-type GLUtesselator
    (alien:struct GLUtesselator (dummy c-call:unsigned-long)))
#+sbcl
(sb-alien:define-alien-type GLUtesselator
    (sb-alien:struct GLUtesselator (dummy sb-alien:unsigned-long)))

(def-foreign-function (gluDeleteTess (:name "gluDeleteTess"))
    (tess (:pointer GLUtesselator)))
(def-foreign-function (gluEndPolygon (:name "gluEndPolygon"))
    (tess (:pointer GLUtesselator)))

#+broken_in_redhat72
(def-foreign-function (gluGetTessProperty (:name "gluGetTessProperty"))
    (tess (:pointer GLUtesselator))
  (which GLenum)
  (data :simple-array))

(def-foreign-function (gluNextContour (:name "gluNextContour"))
    (tess (:pointer GLUtesselator))
  (type GLenum))
#+broken_in_redhat72
(def-foreign-function (gluTessBeginContour (:name "gluTessBeginContour"))
    (tess (:pointer GLUtesselator)))
#+broken_in_redhat72
(def-foreign-function (gluTessBeginPolygon (:name "gluTessBeginPolygon"))
    (tess (:pointer GLUtesselator))
  (data :simple-array))
(def-foreign-function (gluTessCallback (:name "gluTessCallback"))
    (tess (:pointer GLUtesselator))
  (which GLenum)
  (CallBackFunc :int))
#+broken_in_redhat72
(def-foreign-function (gluTessEndContour (:name "gluTessEndContour"))
    (tess (:pointer GLUtesselator)))
#+broken_in_redhat72
(def-foreign-function (gluTessEndPolygon (:name "gluTessEndPolygon"))
    (tess (:pointer GLUtesselator)))
#+broken_in_redhat72
(def-foreign-function (gluTessNormal (:name "gluTessNormal"))
    (tess (:pointer GLUtesselator))
  (x GLdouble)
  (y GLdouble)
  (z GLdouble))

#+broken_in_redhat72
(def-foreign-function (gluTessProperty (:name "gluTessProperty"))
    (tess (:pointer GLUtesselator))
  (which GLenum)
  (data GLdouble))

(def-foreign-function (gluTessVertex (:name "gluTessVertex"))
    (tess (:pointer GLUtesselator))
  (location :simple-array)
  (data :simple-array))

(def-foreign-function (gluNewTess (:name "gluNewTess")
				  (:return-type (:pointer GLUtesselator))))

(def-foreign-function (gluPerspective (:name "gluPerspective"))
  (fov :double-float)
  (aspect :double-float)
  (near :double-float)
  (far :double-float))



#+allegro
(def-foreign-struct GLUquadricObj (dummy :type :unsigned-long))
#+cmu
(alien:def-alien-type GLUquadricObj
    (alien:struct GLUquadricObj (dummy c-call:unsigned-long)))
#+sbcl
(sb-alien:define-alien-type GLUquadricObj
    (sb-alien:struct GLUquadricObj (dummy sb-alien:unsigned-long)))

(def-foreign-function (gluNewQuadric (:name "gluNewQuadric")
				     (:return-type (:pointer GLUquadricObj))))

(def-foreign-function (gluQuadricDrawStyle (:name "gluQuadricDrawStyle"))
    (quad (:pointer GLUquadricObj))
  (draw_style :int))

(def-foreign-function (gluQuadricOrientation (:name "gluQuadricOrientation"))
    (quad (:pointer GLUquadricObj))
  (orientation :int))

(def-foreign-function (gluQuadricNormals (:name "gluQuadricNormals"))
    (quad (:pointer GLUquadricObj))
  (normal_style :int))

(def-foreign-function (gluSphere (:name "gluSphere"))
    (quad (:pointer GLUquadricObj))
  (radius :double-float)
  (slices :int)
  (stacks :int) )

(def-foreign-function (gluCylinder (:name "gluCylinder"))
    (quad (:pointer GLUquadricObj))
  (base :double-float)
  (top :double-float)
  (height :double-float)
  (slices :int)
  (stacks :int) )

(def-foreign-function (gluDisk (:name "gluDisk"))
    (quad (:pointer GLUquadricObj))
  (inner :double-float)
  (outer :double-float)
  (slices :int)
  (loops :int) )

(def-foreign-function (gluPickMatrix  (:name "gluPickMatrix"))
    (x :double-float)
    (y :double-float)
    (delx :double-float)
    (dely :double-float)
    (viewport :simple-array) ; (:vector :double-float 4)
)

(def-foreign-constants "GLU_POINT" "GLU_LINE" "GLU_FILL" "GLU_SILHOUETTE"
		       "GLU_SMOOTH" "GLU_FLAT" "GLU_NONE" "GLU_OUTSIDE" "GLU_INSIDE")

(defparameter *quadObj* nil)

(defun get_quadric ()
  (or *quadObj*
      (let ((quad (gluNewQuadric)))
	(when (zerop (foreign-pointer-address quad))
	  (error "gluNewQuadric failed"))
	(setq *quadObj* quad))))

(def-foreign-function (gluErrorString (:name "gluErrorString") (:return-type :simple-string))
    (name :int))




 
#-(and sbcl mswindows)
(def-foreign-function (gluBuild2DMipmapLevels (:name "gluBuild2DMipmapLevels") 
					      (:return-type :int))
    (target     GLenum)
  (internalFormat  GLint)
    (width      GLsizei)
    (height     GLsizei)
    (format     GLenum)
    (type       GLenum)
    (level GLint)
    (base GLint)
    (max GLint)
    (pixels :simple-array))


 
(def-foreign-function (gluBuild2DMipmaps (:name "gluBuild2DMipmaps") 
					      (:return-type :int))
    (target     GLenum)
  (internalFormat  GLint)
    (width      GLsizei)
    (height     GLsizei)
    (format     GLenum)
    (type       GLenum)
    (pixels :simple-array))



#-(and sbcl mswindows)
(def-foreign-function (gluCheckExtension (:name "gluCheckExtension")
					 (:return-type :int))
    (extName :simple-string)
  (extString :simple-string)) 

