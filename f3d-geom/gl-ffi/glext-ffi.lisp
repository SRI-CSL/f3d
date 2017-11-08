(in-package :GL)

;;; Not sure what to do about this.  None of the extensions listed
;;; here are visible in the default OpenGL DLL supplied with Windoze
;;; 98.  For the moment, I ignore them under windoze.  Is this ok?
;;; 

;;;
;;; Many of the extensions defined here are not available under AGL either.
;;;
#+mswindows
(def-foreign-function (wglGetProcAddress (:name "wglGetProcAddress"))
  string)

#-(or mswindows agl cocoa)
(def-foreign-function (glGenTexturesEXT (:name "glGenTexturesEXT"))
    (n GLuint)
  (int-array :simple-array))

#-(or mswindows agl cocoa)
(def-foreign-function (glDeleteTexturesEXT (:name "glDeleteTexturesEXT"))
    (n GLuint)
  (int-array :simple-array))

#-(or mswindows agl cocoa)
(def-foreign-function (glIsTextureEXT (:name "glIsTextureEXT"))
    (texid GLuint))


#-(or mswindows agl cocoa)
(def-foreign-function (glTexImage3DEXT (:name "glTexImage3DEXT"))
    (target     GLenum)
    (level      GLint)
    (internal_format GLint)
    (width      GLsizei)
    (height     GLsizei)
    (depth      GLsizei)
    (border     GLint)
    (format     GLenum)
    (type       GLenum)
    (pixels :simple-array) ;(pixels     (:pointer GLvoid))
    )

(defconstant GL_LUMINANCE4_EXT #x803f)
(defconstant GL_LUMINANCE8_EXT #x8040)
(defconstant GL_LUMINANCE12_EXT #x8041)
(defconstant GL_LUMINANCE16_EXT #x8042)
(defconstant GL_TEXTURE_RED_SIZE_EXT #X805C)
(defconstant GL_TEXTURE_GREEN_SIZE_EXT #X805D)
(defconstant GL_TEXTURE_BLUE_SIZE_EXT #X805E)
(defconstant GL_TEXTURE_ALPHA_SIZE_EXT #X805F)
(defconstant GL_TEXTURE_LUMINANCE_SIZE_EXT #X8060)
(defconstant GL_TEXTURE_INTENSITY_SIZE_EXT #X8061)
(defconstant signed :DEFINED)
(defconstant GL_EXT_blend_color 1)
(defconstant GL_EXT_blend_logic_op 1)
(defconstant GL_EXT_blend_minmax 1)
(defconstant GL_EXT_blend_subtract 1)
(defconstant GL_EXT_polygon_offset 1)

(defconstant GL_CLAMP_TO_EDGE_SGIS #x812F)
(defconstant GL_CLAMP_TO_BORDER_SGIS #x812D)


(defconstant GL_MAX_3D_TEXTURE_SIZE_EXT #x8073)

#-(or mswindows agl cocoa)
(def-foreign-function (glBindTextureEXT (:name "glBindTextureEXT"))
    (target GLenum) ; GL_TEXTURE_1D, GL_TEXTURE_2D ...
  (texid GLuint))

#-(or mswindows agl cocoa)
(def-foreign-function (glPrioritizeTexturesEXT (:name "glPrioritizeTexturesEXT"))
    (n GLuint)
  (texids :simple-array)			;  GLuint array
  (priorities :simple-array)			; GLclampf array
  )


#-(or mswindows agl cocoa)
(def-foreign-function (glAreTexturesResidentEXT (:name "glAreTexturesResidentEXT"))
    (n GLuint)
  (texids :simple-array) ;  GLuint array
  (residences :simple-array)			; GLboolean (:unsigned-8bit) array
  )


(defun glGetTexLevelParameter (target level pname &optional length)
  (let ((arr (make-array0 (or length 4) :element-type '(signed-byte 32))))
    (glGetTexLevelParameteriv target level pname arr)
    (if length 
	arr
	(values (aref arr 0)
		arr))))
;;#-mswindows
#+fixme
(def-foreign-function (glTexSubImage2DEXT  (:name "glTexSubImage2DEXT"))
    target level xoffset yoffset width height format type
    pixels ; pointer to array
    )
#|
(glGetTexLevelParameter GL_TEXTURE_2D 0 GL_TEXTURE_RED_SIZE_EXT) ; =0
(glGetTexLevelParameter GL_TEXTURE_2D 0 GL_TEXTURE_LUMINANCE_SIZE_EXT) ; = 16
|#



(defconstant GL_CONVOLUTION_1D_EXT #x8010)
(defconstant GL_CONVOLUTION_2D_EXT #x8011)
(defconstant GL_SEPARABLE_2D_EXT #x8012)
(defconstant GL_CONVOLUTION_BORDER_MODE_EXT #x8013)
(defconstant GL_CONVOLUTION_FILTER_SCALE_EXT #x8014)
(defconstant GL_CONVOLUTION_FILTER_BIAS_EXT #x8015)
(defconstant GL_REDUCE_EXT #x8016)
(defconstant GL_CONVOLUTION_FORMAT_EXT #x8017)
(defconstant GL_CONVOLUTION_WIDTH_EXT #x8018)
(defconstant GL_CONVOLUTION_HEIGHT_EXT #x8019)
(defconstant GL_MAX_CONVOLUTION_WIDTH_EXT        #x801A)
(defconstant GL_MAX_CONVOLUTION_HEIGHT_EXT       #x801B)
(defconstant GL_POST_CONVOLUTION_RED_SCALE_EXT #x801C)
(defconstant GL_POST_CONVOLUTION_GREEN_SCALE_EXT #x801D)
(defconstant GL_POST_CONVOLUTION_BLUE_SCALE_EXT #x801E)
(defconstant GL_POST_CONVOLUTION_ALPHA_SCALE_EXT #x801F)
(defconstant GL_POST_CONVOLUTION_RED_BIAS_EXT #x8020)
(defconstant GL_POST_CONVOLUTION_GREEN_BIAS_EXT #x8021)
(defconstant GL_POST_CONVOLUTION_BLUE_BIAS_EXT #x8022)
(defconstant GL_POST_CONVOLUTION_ALPHA_BIAS_EXT #x8023)

#+never
(progn 
#-mswindows 
(def-foreign-function (glConvolutionFilter1DEXT (:name "glConvolutionFilter1DEXT"))
    target internalformat width format type
    array)

#-mswindows
(def-foreign-function (glConvolutionFilter2DEXT (:name "glConvolutionFilter2DEXT"))
    target internalformat width height format type
    array)

#-mswindows
(def-foreign-function (glGetConvolutionParameterivEXT
		       (:name "glGetConvolutionParameterivEXT"))
    target pname
    int-arr)

#-mswindows
(defun glGetConvolutionParameter (target name)
  (let ((arr (make-array0 4 :element-type '(signed-byte 32))))
    (glGetConvolutionParameterivEXT target name arr)
    (aref arr 0)))
) ; end #+never progn

#|
(glGetConvolutionParameter GL_CONVOLUTION_2D_EXT GL_MAX_CONVOLUTION_WIDTH_EXT) ; 11

|#


;;; The O2 is currently not compliant with OpenGL 1.1

#-(or mswindows agl cocoa)
(def-foreign-function (glVertexPointerEXT
		       (:name "glVertexPointerEXT"))
    (size GLint)
  (type GLenum)
  (stride GLsizei)
  (count GLsizei)
  (array :simple-array)			; array type
  )

#-(or mswindows agl cocoa)
(def-foreign-function (glTexCoordPointerEXT
		       (:name "glTexCoordPointerEXT"))
    (size GLint)
  (type GLenum)
  (stride GLsizei)
  (count GLsizei)
  (array :simple-array)			; array type
  )

#-(or mswindows agl cocoa)
(def-foreign-function (glColorPointerEXT
		       (:name "glColorPointerEXT"))
    (size GLint)
  (type GLenum)
  (stride GLsizei)
  (count GLsizei)
  (array :simple-array)			; array type
  )

#-(or mswindows agl cocoa)
(def-foreign-function (glNormalPointerEXT
		       (:name "glNormalPointerEXT"))
  (type GLenum)
  (stride GLsizei)
  (count GLsizei)
  (array :simple-array)			; array type
  )

#-(or mswindows agl cocoa)
(def-foreign-function (glEdgeFlagPointerEXT
		       (:name "glEdgeFlagPointerEXT"))
  (stride GLsizei)
  (count GLsizei)
  (array :simple-array)			; array type
  )

#-(or mswindows agl cocoa)
(def-foreign-function (glIndexPointerEXT
		       (:name "glIndexPointerEXT"))
    (type GLenum)
  (stride GLsizei)
  (count GLsizei)
  (array :simple-array)					; array type
  )

#-(or mswindows agl cocoa)
(def-foreign-function (glDrawArraysEXT
		       (:name #-(or solaris2 agl cocoa) "glDrawArraysEXT"
			      #+(or solaris2 agl cocoa) "glDrawArrays"))
    (mode GLenum)
  (first GLint)
  (count  GLsizei))

#-(or mswindows agl cocoa)
(def-foreign-function (glArrayElementEXT
		       (:name "glArrayElementEXT"))
    (i GLint))


#|
(glGetString GL_VERSION)
(glGetString GL_EXTENSIONS)
(search "generate_mipmap" (glGetString GL_EXTENSIONS))

The current (Mon Feb 16 1998) state of the SGI OpenGL library
indicates that it is not OpenGL 1.1.  Ie, glxinfo -fbcinfo returns the
following:
glxinfo
display: :0
server glx vendor string: SGI
server glx version string: 1.1 Irix 6.3
server glx extensions (GLX_):
    EXT_import_context, EXT_visual_info, EXT_visual_rating,
    SGI_make_current_read, SGI_swap_control, SGI_video_sync, SGIS_multisample,
    SGIX_dm_pbuffer, SGIX_fbconfig, SGIX_pbuffer, SGIX_video_resize,
    SGIX_video_source.
client glx version 1.1
client glx extensions (GLX_):
    EXT_import_context, EXT_visual_info, EXT_visual_rating,
    SGI_make_current_read, SGI_swap_control, SGI_video_sync, SGIS_multisample,
    SGIX_dm_pbuffer, SGIX_fbconfig, SGIX_pbuffer, SGIX_video_resize,
    SGIX_video_source.
OpenGL vendor string: SGI
OpenGL renderer string: CRIME
OpenGL version string: 1.0 Irix 6.3
OpenGL extensions (GL_):
    EXT_abgr, EXT_blend_color, EXT_blend_logic_op, EXT_blend_minmax,
    EXT_blend_subtract, EXT_convolution, EXT_copy_texture, EXT_histogram,
    EXT_packed_pixels, EXT_polygon_offset, EXT_subtexture, EXT_texture,
    EXT_texture3D, EXT_texture_object, EXT_vertex_array, SGI_color_matrix,
    SGI_color_table, SGI_texture_color_table, SGIS_texture_edge_clamp,
    SGIX_interlace, SGIX_texture_scale_bias.

   visual  x  bf lv rg d st  r  g  b a  ax dp st accum buffs  ms 
 id dep cl sp sz l  ci b ro sz sz sz sz bf th cl  r  g  b  a ns b
-----------------------------------------------------------------
0x20  8 pc  .  8  . c  .  .  .  .  .  .  .  .  .  .  .  .  .  . .
0x21  8 pc  .  8  . c  .  .  .  .  .  .  . 24  8  .  .  .  .  . .
0x23  8 tc  .  8  . r  .  .  3  3  2  .  .  .  . 16 16 16 16  . .
0x24  8 tc  .  8  . r  .  .  3  3  2  .  . 24  8 16 16 16 16  . .
0x25  8 pc  y  8  1 c  .  .  .  .  .  .  .  .  .  .  .  .  .  . .
0x26 12 pc  . 12  . c  .  .  .  .  .  .  .  .  .  .  .  .  .  . .
0x27 12 pc  . 12  . c  .  .  .  .  .  .  . 24  8  .  .  .  .  . .
0x28 12 pc  . 12  . c  y  .  .  .  .  .  .  .  .  .  .  .  .  . .
0x29 12 pc  . 12  . c  y  .  .  .  .  .  . 24  8  .  .  .  .  . .
0x2a 15 tc  . 15  . r  .  .  5  5  5  .  .  .  . 16 16 16 16  . .
0x2b 15 tc  . 16  . r  .  .  5  5  5  1  .  .  . 16 16 16 16  . .
0x2c 15 tc  . 15  . r  .  .  5  5  5  .  . 24  8 16 16 16 16  . .
0x2d 15 tc  . 16  . r  .  .  5  5  5  1  . 24  8 16 16 16 16  . .
0x2e 15 tc  . 15  . r  y  .  5  5  5  .  .  .  . 16 16 16 16  . .
0x2f 15 tc  . 15  . r  y  .  5  5  5  .  . 24  8 16 16 16 16  . .
0x31 24 tc  . 24  . r  .  .  8  8  8  .  .  .  . 16 16 16 16  . .
0x32 24 tc  . 24  . r  .  .  8  8  8  .  . 24  8 16 16 16 16  . .
0x33 24 tc  . 32  . r  .  .  8  8  8  8  .  .  . 16 16 16 16  . .
0x34 24 tc  . 32  . r  .  .  8  8  8  8  . 24  8 16 16 16 16  . .
0xffffffff -1 ??  .  8  . r  .  .  8  .  .  .  .  .  .  .  .  .  .  . .
0xffffffff -1 ??  . 16  . r  .  . 16  .  .  .  .  .  .  .  .  .  .  . .
0xffffffff -1 ??  . 64  . r  .  . 16 16 16 16  .  .  .  .  .  .  .  . .
|#


;;; The SGI OpenGL library does not support glEnableClientState glDisableClientState
;;; and glDrawElements

#+irix
(defun glEnableClientState (arrayEnum)
  (glEnable arrayEnum))

#+irix
(defun glDisableClientState (arrayEnum)
  (glDisable arrayEnum))


#-irix
(def-foreign-function (glEnableClientState (:name "glEnableClientState"))
    (cap GLenum))

#-irix
(def-foreign-function (glDisableClientState (:name "glDisableClientState"))
    (cap GLenum))

#-irix
(def-foreign-function (glDrawElements (:name "glDrawElements"))
    (mode GLenum)
  (count  GLsizei)
  (type GLenum)
  (indices :simple-array)				; array int
  )

;;; The SGI OpenGL library appear not to support the OpenGL 1.1 versions
;; of these functions

#+mswindows
(def-foreign-function (glDrawArrays
		       (:name "glDrawArrays"))
    (mode GLenum)
  (first GLint)
  (count  GLsizei))

#-mswindows
(def-foreign-function (glDrawArrays
		       (:name #-(or solaris2 agl cocoa) "glDrawArraysEXT"
			      #+(or solaris2 agl cocoa) "glDrawArrays"))
    (mode GLenum)
  (first GLint)
  (count  GLsizei))

;;; Don't redefine these in Windoze...they don't seem to be available.
 
;;; Do we need to add cocoa-image-panes??

#-(or mswindows agl cocoa)
(allow-redefinition
 
(defun glVertexPointer (size type stride array)
  (glVertexPointerEXT size type stride 0 array))

(defun glTexCoordPointer (size type stride array)
  (glTexCoordPointerEXT size type stride 0 array))

(defun glColorPointer (size type stride array)
  (glColorPointerEXT size type stride 0 array))

(defun glIndexPointer (type stride array)
  (glIndexPointerEXT type stride 0 array))

(defun glNormalPointer (type stride array)
  (glNormalPointerEXT type stride 0 array))

(defun glEdgeFlagPointer (stride array)
  (glEdgeFlagPointerEXT stride 0 array))

) ; end allow-redefinition 


#-(and sbcl mswindows)
(def-foreign-function (glCompressedTexImage2D-non-simple
                       (:name "glCompressedTexImage2D")
                       (:return-type :null))
    (target     GLenum)
    (level      GLint)
    (internalFormat  GLint)
    (width      GLsizei)
    (height     GLsizei)
    (border     GLint)
    (format     GLenum)
    (type       GLenum)
    (pixels :array) ; allow non-simple arrays too
    )
