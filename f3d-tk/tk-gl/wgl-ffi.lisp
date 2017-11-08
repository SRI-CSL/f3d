(in-package :gl)

(def-foreign-synonym-type dummy-pointer :unsigned-long)

;;; should these be pointers or :unsigned-long?
(def-foreign-synonym-type wgl-windowid dummy-pointer)
(def-foreign-synonym-type wgl-hdc dummy-pointer)
(def-foreign-synonym-type wgl-hglrc dummy-pointer)

(def-foreign-function (wglMakeCurrent (:name "wglMakeCurrent"))
  (hdc wgl-hdc)
  (hglrc wgl-hglrc))

(def-foreign-function (wglSwapBuffers (:name "wglSwapBuffers")
				      (:return-type :int))
  (hglrc wgl-hglrc))

(def-foreign-function (wglGetDC (:name (freedius-prefix "GetDC"))
				(:return-type wgl-hdc))
  (id wgl-windowid))

(def-foreign-function (wglGetDCEx (:name (freedius-prefix "GetDCEx"))
				  (:return-type wgl-hdc))
  (id wgl-windowid)
  (flags :int))

(def-foreign-function (wglGetDCExError (:name (freedius-prefix "GetDCExError"))
				       (:return-type :int)))

(lcl::def-foreign-struct wgl_rendering_spec
    (RgbaFlag :type :signed-32bit)
    (ColorBits :type :signed-32bit)
    (DoubleFlag :type :signed-32bit)
    (DepthSize :type :signed-32bit)
    (AccumBits :type :signed-32bit)
    (AlphaSize :type :signed-32bit)
    (StencilSize :type :signed-32bit)
    (OverlayFlag :type :signed-32bit)
    (StereoFlag :type :signed-32bit)
    (AuxNumber :type :signed-32bit)
    (accel :type :signed-32bit))

#+doesntwork
(def-foreign-function (make_wglRenderingSpec
			(:name (freedius-prefix "make_wglRenderingSpec"))
			(:return-type (:pointer wgl_rendering_spec)))
  )


(def-foreign-function (make_wglRenderingSpec
			(:name "make_wglRenderingSpec")
			(:return-type (:pointer wgl_rendering_spec)))
  )



;;; This accepts a keyword based attribute list for making a rendering context.
(defun set-wgl_rendering_spec (spec opengl-attribute-list)
  (let ((attrs opengl-attribute-list)
	(red-size 0) (grn-size 0) (blu-size 0)
	(accum-red-size 0) (accum-grn-size 0) (accum-blu-size 0))

    (setf (wgl_rendering_spec-RgbaFlag spec) 0
	  (wgl_rendering_spec-ColorBits spec) 0
	  (wgl_rendering_spec-DoubleFlag spec) 0
	  (wgl_rendering_spec-DepthSize spec) 0
	  (wgl_rendering_spec-AccumBits spec) 0
	  (wgl_rendering_spec-AlphaSize spec) 0
	  (wgl_rendering_spec-StencilSize spec) 0
	  (wgl_rendering_spec-OverlayFlag spec) 0
	  (wgl_rendering_spec-StereoFlag spec) 0
	  (wgl_rendering_spec-AuxNumber spec) 0)
	  
    (loop for attr = (pop attrs)
	  while attr	  
	  do (case attr
	       (:rgba
		(setf (wgl_rendering_spec-RgbaFlag spec) 1))
	       (:alpha-size
		(setf (wgl_rendering_spec-AlphaSize spec) (pop attrs)))
	       (:depth-size
		(setf (wgl_rendering_spec-DepthSize spec) (pop attrs)))
	       (:stencil-size
		(setf (wgl_rendering_spec-StencilSize spec) (pop attrs)))
	       (:doublebuffer
		(setf (wgl_rendering_spec-DoubleFlag spec) 1))
	       (:overlay
		(setf (wgl_rendering_spec-OverlayFlag spec) 1))
	       (:stereo
		(setf (wgl_rendering_spec-StereoFlag spec) 1))
	       (:aux-buffers
		(setf (wgl_rendering_spec-AuxNumber spec) (pop attrs)))
	       
	       (:red-size (setf red-size (pop attrs)))
	       (:green-size (setf grn-size (pop attrs)))
	       (:blue-size (setf blu-size (pop attrs)))
	       (:accum-red-size (setf accum-red-size (pop attrs)))
	       (:accum-green-size (setf accum-grn-size (pop attrs)))
	       (:accum-blue-size (setf accum-blu-size (pop attrs)))))

    (setf (wgl_rendering_spec-ColorBits spec)
	  (+ red-size grn-size blu-size)
	  (wgl_rendering_spec-AccumBits spec)
	  (+ accum-red-size accum-grn-size accum-blu-size))

    spec))


(defun get-wgl_rendering_spec (spec)
  (list :rgba        (= (wgl_rendering_spec-RgbaFlag spec) 1)
	:alpha-size  0
	:depth-size  (wgl_rendering_spec-DepthSize spec)
	:stencil-size (wgl_rendering_spec-StencilSize spec)
	:doublebuffer (if (= (wgl_rendering_spec-DoubleFlag spec) 1) t nil)
	:overlay      (if (= (wgl_rendering_spec-OverlayFlag spec) 1) t nil)
	:stereo       (if (= (wgl_rendering_spec-StereoFlag spec) 1) t nil)
	:aux-buffers  (wgl_rendering_spec-AuxNumber spec)
	:red-size     0
	:green-size   0
	:blue-size    0
	:color-size   (wgl_rendering_spec-ColorBits spec)
	:accelerated  (wgl_rendering_spec-Accel spec)
	))


#+doesntwork
(def-foreign-function (wgl_chooseAndSetPixelformat
			(:name (freedius-prefix "wgl_chooseAndSetPixelformat"))
			(:return-type :int))
  (hdc wgl-hdc)
  (spec (:pointer wgl_rendering_spec)))

(def-foreign-function (wgl_chooseAndSetPixelformat
			(:name (freedius-prefix "chooseAndSetPixelformat"))
			(:return-type :int))
  (hdc wgl-hdc)
  (spec (:pointer wgl_rendering_spec)))


(def-foreign-function (wgl_CreateContextFromID
		       (:name (freedius-prefix "CreateContextFromID"))
		       (:return-type wgl-hglrc))
  (id wgl-windowid))


;; This is actually defined in liblisptk, not libFREEDIUS:
#+never
(def-foreign-function (wgl_CreateWindow
		       (:name (freedius-prefix "CreateWindow"))
		       (:return-type wgl-hdc))
  (id wgl-windowid)
  (x :int)
  (y :int)
  (width :int)
  (height :int))


(def-foreign-function (wglCreateContext
			(:name "wglCreateContext")
			(:return-type wgl-hglrc))
  (hdc wgl-hdc))

(def-foreign-function (wglShareLists (:name "wglShareLists"))
  (hglrc1 wgl-hglrc)
  (hglrc2 wgl-hglrc))
  

;; This is NOT the Windows native function - it's a wrapper that fills
;; in the current spec for the window and returns the pixel format
;; number:
(def-foreign-function (wglGetPixelFormat (:name (freedius-prefix "GetPixelFormat"))
					 (:return-type :int))
  (hdc wgl-hdc)
  (spec (:pointer wgl_rendering_spec)))




(def-foreign-function (wgl_GetLastError (:name "glGetError")
					(:return-type :unsigned-32bit))
  )


#||
;;;
;;; Dump of OPENGL32.DLL on Lager:
;;;
C:\freedius\freedius>dumpbin/exports c:\WINDOWS\system32\opengl32.dll
dumpbin/exports c:\WINDOWS\system32\opengl32.dll
Microsoft (R) COFF/PE Dumper Version 8.00.50727.42
Copyright (C) Microsoft Corporation.  All rights reserved.


Dump of file c:\WINDOWS\system32\opengl32.dll

File Type: DLL

  Section contains the following exports for OPENGL32.dll

    00000000 characteristics
    480242E3 time date stamp Sun Apr 13 10:29:07 2008
        0.00 version
           1 ordinal base
         368 number of functions
         368 number of names

    ordinal hint RVA      name

          1    0 0001A6DA GlmfBeginGlsBlock
          2    1 0001A868 GlmfCloseMetaFile
          3    2 0001A807 GlmfEndGlsBlock
          4    3 0001A830 GlmfEndPlayback
          5    4 0001A5DB GlmfInitPlayback
          6    5 0001A711 GlmfPlayGlsRecord
          7    6 0000458C glAccum
          8    7 000047FC glAlphaFunc
          9    8 000051F0 glAreTexturesResident
         10    9 00002F98 glArrayElement
         11    A 000028F0 glBegin
         12    B 00002FA4 glBindTexture
         13    C 0000325C glBitmap
         14    D 00004830 glBlendFunc
         15    E 000028D8 glCallList
         16    F 000028E4 glCallLists
         17   10 00003124 glClear
         18   11 00003158 glClearAccum
         19   12 000031C0 glClearColor
         20   13 00003228 glClearDepth
         21   14 0000318C glClearIndex
         22   15 000031F4 glClearStencil
         23   16 00003AFC glClipPlane
         24   17 000028FC glColor3b
         25   18 00002908 glColor3bv
         26   19 00002914 glColor3d
         27   1A 00002920 glColor3dv
         28   1B 0000292C glColor3f
         29   1C 00002938 glColor3fv
         30   1D 00002944 glColor3i
         31   1E 00002950 glColor3iv
         32   1F 0000295C glColor3s
         33   20 00002968 glColor3sv
         34   21 00002974 glColor3ub
         35   22 00002980 glColor3ubv
         36   23 0000298C glColor3ui
         37   24 00002998 glColor3uiv
         38   25 000029A4 glColor3us
         39   26 000029B0 glColor3usv
         40   27 000029BC glColor4b
         41   28 000029C8 glColor4bv
         42   29 000029D4 glColor4d
         43   2A 000029E0 glColor4dv
         44   2B 000029EC glColor4f
         45   2C 000029F8 glColor4fv
         46   2D 00002A04 glColor4i
         47   2E 00002A10 glColor4iv
         48   2F 00002A1C glColor4s
         49   30 00002A28 glColor4sv
         50   31 00002A34 glColor4ub
         51   32 00002A40 glColor4ubv
         52   33 00002A4C glColor4ui
         53   34 00002A58 glColor4uiv
         54   35 00002A64 glColor4us
         55   36 00002A70 glColor4usv
         56   37 000044F0 glColorMask
         57   38 00003B30 glColorMaterial
         58   39 00002FB0 glColorPointer
         59   3A 000032F4 glCopyPixels
         60   3B 00005224 glCopyTexImage1D
         61   3C 00005258 glCopyTexImage2D
         62   3D 0000528C glCopyTexSubImage1D
         63   3E 000052C0 glCopyTexSubImage2D
         64   3F 00003B64 glCullFace
         65   40 00025008 glDebugEntry
         66   41 0000358C glDeleteLists
         67   42 000052F4 glDeleteTextures
         68   43 00004900 glDepthFunc
         69   44 00004524 glDepthMask
         70   45 00005120 glDepthRange
         71   46 00002E48 glDisable
         72   47 00002FBC glDisableClientState
         73   48 00002FC8 glDrawArrays
         74   49 00004488 glDrawBuffer
         75   4A 00002FD4 glDrawElements
         76   4B 0000335C glDrawPixels
         77   4C 00002A7C glEdgeFlag
         78   4D 00002FE0 glEdgeFlagPointer
         79   4E 00002A88 glEdgeFlagv
         80   4F 00002E54 glEnable
         81   50 00002FEC glEnableClientState
         82   51 00002A94 glEnd
         83   52 0000355C glEndList
         84   53 00002E78 glEvalCoord1d
         85   54 00002E84 glEvalCoord1dv
         86   55 00002E90 glEvalCoord1f
         87   56 00002E9C glEvalCoord1fv
         88   57 00002EA8 glEvalCoord2d
         89   58 00002EB4 glEvalCoord2dv
         90   59 00002EC0 glEvalCoord2f
         91   5A 00002ECC glEvalCoord2fv
         92   5B 00004794 glEvalMesh1
         93   5C 000047C8 glEvalMesh2
         94   5D 00002ED8 glEvalPoint1
         95   5E 00002EE4 glEvalPoint2
         96   5F 000042E8 glFeedbackBuffer
         97   60 00003FA8 glFinish
         98   61 000045C0 glFlush
         99   62 00003B98 glFogf
        100   63 00003BCC glFogfv
        101   64 00003C00 glFogi
        102   65 00003C34 glFogiv
        103   66 00003C68 glFrontFace
        104   67 00005154 glFrustum
        105   68 000035BC glGenLists
        106   69 00005328 glGenTextures
        107   6A 00004B08 glGetBooleanv
        108   6B 00004B3C glGetClipPlane
        109   6C 00004B70 glGetDoublev
        110   6D 00004BA4 glGetError
        111   6E 00004BD8 glGetFloatv
        112   6F 00004C0C glGetIntegerv
        113   70 00004C40 glGetLightfv
        114   71 00004C74 glGetLightiv
        115   72 00004CA8 glGetMapdv
        116   73 00004CDC glGetMapfv
        117   74 00004D10 glGetMapiv
        118   75 00004D44 glGetMaterialfv
        119   76 00004D78 glGetMaterialiv
        120   77 00004DAC glGetPixelMapfv
        121   78 00004DE0 glGetPixelMapuiv
        122   79 00004E14 glGetPixelMapusv
        123   7A 00003058 glGetPointerv
        124   7B 00004E48 glGetPolygonStipple
        125   7C 00004E7C glGetString
        126   7D 00004EB0 glGetTexEnvfv
        127   7E 00004EE4 glGetTexEnviv
        128   7F 00004F18 glGetTexGendv
        129   80 00004F4C glGetTexGenfv
        130   81 00004F80 glGetTexGeniv
        131   82 00004FB4 glGetTexImage
        132   83 00005050 glGetTexLevelParameterfv
        133   84 00005084 glGetTexLevelParameteriv
        134   85 00004FE8 glGetTexParameterfv
        135   86 0000501C glGetTexParameteriv
        136   87 00003C9C glHint
        137   88 00004558 glIndexMask
        138   89 00002FF8 glIndexPointer
        139   8A 00002AA0 glIndexd
        140   8B 00002AAC glIndexdv
        141   8C 00002AB8 glIndexf
        142   8D 00002AC4 glIndexfv
        143   8E 00002AD0 glIndexi
        144   8F 00002ADC glIndexiv
        145   90 00002AE8 glIndexs
        146   91 00002AF4 glIndexsv
        147   92 00003004 glIndexub
        148   93 00003010 glIndexubv
        149   94 00004384 glInitNames
        150   95 0000301C glInterleavedArrays
        151   96 000050B8 glIsEnabled
        152   97 000050EC glIsList
        153   98 0000535C glIsTexture
        154   99 00003DA0 glLightModelf
        155   9A 00003DD4 glLightModelfv
        156   9B 00003E08 glLightModeli
        157   9C 00003E3C glLightModeliv
        158   9D 00003CD0 glLightf
        159   9E 00003D04 glLightfv
        160   9F 00003D38 glLighti
        161   A0 00003D6C glLightiv
        162   A1 00003E70 glLineStipple
        163   A2 00003EA4 glLineWidth
        164   A3 000035EC glListBase
        165   A4 00002EF0 glLoadIdentity
        166   A5 00002F08 glLoadMatrixd
        167   A6 00002EFC glLoadMatrixf
        168   A7 000043B8 glLoadName
        169   A8 00004864 glLogicOp
        170   A9 000045F4 glMap1d
        171   AA 00004628 glMap1f
        172   AB 0000465C glMap2d
        173   AC 00004690 glMap2f
        174   AD 000046C4 glMapGrid1d
        175   AE 000046F8 glMapGrid1f
        176   AF 0000472C glMapGrid2d
        177   B0 00004760 glMapGrid2f
        178   B1 00002E18 glMaterialf
        179   B2 00002E24 glMaterialfv
        180   B3 00002E30 glMateriali
        181   B4 00002E3C glMaterialiv
        182   B5 00002F14 glMatrixMode
        183   B6 00002F2C glMultMatrixd
        184   B7 00002F20 glMultMatrixf
        185   B8 00003530 glNewList
        186   B9 00002B00 glNormal3b
        187   BA 00002B0C glNormal3bv
        188   BB 00002B18 glNormal3d
        189   BC 00002B24 glNormal3dv
        190   BD 00002B30 glNormal3f
        191   BE 00002B3C glNormal3fv
        192   BF 00002B48 glNormal3i
        193   C0 00002B54 glNormal3iv
        194   C1 00002B60 glNormal3s
        195   C2 00002B6C glNormal3sv
        196   C3 00003028 glNormalPointer
        197   C4 00005188 glOrtho
        198   C5 000043EC glPassThrough
        199   C6 00004A38 glPixelMapfv
        200   C7 00004A6C glPixelMapuiv
        201   C8 00004AA0 glPixelMapusv
        202   C9 000049D0 glPixelStoref
        203   CA 00004A04 glPixelStorei
        204   CB 00004968 glPixelTransferf
        205   CC 0000499C glPixelTransferi
        206   CD 00004934 glPixelZoom
        207   CE 00003ED8 glPointSize
        208   CF 00003F0C glPolygonMode
        209   D0 00003034 glPolygonOffset
        210   D1 00003F40 glPolygonStipple
        211   D2 00002E60 glPopAttrib
        212   D3 00003064 glPopClientAttrib
        213   D4 00002F38 glPopMatrix
        214   D5 00004420 glPopName
        215   D6 00005390 glPrioritizeTextures
        216   D7 00002E6C glPushAttrib
        217   D8 00003070 glPushClientAttrib
        218   D9 00002F44 glPushMatrix
        219   DA 00004454 glPushName
        220   DB 0000361C glRasterPos2d
        221   DC 00003650 glRasterPos2dv
        222   DD 00003684 glRasterPos2f
        223   DE 000036B8 glRasterPos2fv
        224   DF 000036EC glRasterPos2i
        225   E0 00003720 glRasterPos2iv
        226   E1 00003754 glRasterPos2s
        227   E2 00003788 glRasterPos2sv
        228   E3 000037BC glRasterPos3d
        229   E4 000037F0 glRasterPos3dv
        230   E5 00003824 glRasterPos3f
        231   E6 00003858 glRasterPos3fv
        232   E7 0000388C glRasterPos3i
        233   E8 000038C0 glRasterPos3iv
        234   E9 000038F4 glRasterPos3s
        235   EA 00003928 glRasterPos3sv
        236   EB 0000395C glRasterPos4d
        237   EC 00003990 glRasterPos4dv
        238   ED 000039C4 glRasterPos4f
        239   EE 000039F8 glRasterPos4fv
        240   EF 00003A2C glRasterPos4i
        241   F0 00003A60 glRasterPos4iv
        242   F1 00003A94 glRasterPos4s
        243   F2 00003AC8 glRasterPos4sv
        244   F3 00004AD4 glReadBuffer
        245   F4 00003328 glReadPixels
        246   F5 00003390 glRectd
        247   F6 000033C4 glRectdv
        248   F7 000033F8 glRectf
        249   F8 0000342C glRectfv
        250   F9 00003460 glRecti
        251   FA 00003494 glRectiv
        252   FB 000034C8 glRects
        253   FC 000034FC glRectsv
        254   FD 00004350 glRenderMode
        255   FE 00002F50 glRotated
        256   FF 00002F5C glRotatef
        257  100 00002F68 glScaled
        258  101 00002F74 glScalef
        259  102 00003F74 glScissor
        260  103 0000431C glSelectBuffer
        261  104 00003FDC glShadeModel
        262  105 00004898 glStencilFunc
        263  106 000044BC glStencilMask
        264  107 000048CC glStencilOp
        265  108 00002B78 glTexCoord1d
        266  109 00002B84 glTexCoord1dv
        267  10A 00002B90 glTexCoord1f
        268  10B 00002B9C glTexCoord1fv
        269  10C 00002BA8 glTexCoord1i
        270  10D 00002BB4 glTexCoord1iv
        271  10E 00002BC0 glTexCoord1s
        272  10F 00002BCC glTexCoord1sv
        273  110 00002BD8 glTexCoord2d
        274  111 00002BE4 glTexCoord2dv
        275  112 00002BF0 glTexCoord2f
        276  113 00002BFC glTexCoord2fv
        277  114 00002C08 glTexCoord2i
        278  115 00002C14 glTexCoord2iv
        279  116 00002C20 glTexCoord2s
        280  117 00002C2C glTexCoord2sv
        281  118 00002C38 glTexCoord3d
        282  119 00002C44 glTexCoord3dv
        283  11A 00002C50 glTexCoord3f
        284  11B 00002C5C glTexCoord3fv
        285  11C 00002C68 glTexCoord3i
        286  11D 00002C74 glTexCoord3iv
        287  11E 00002C80 glTexCoord3s
        288  11F 00002C8C glTexCoord3sv
        289  120 00002C98 glTexCoord4d
        290  121 00002CA4 glTexCoord4dv
        291  122 00002CB0 glTexCoord4f
        292  123 00002CBC glTexCoord4fv
        293  124 00002CC8 glTexCoord4i
        294  125 00002CD4 glTexCoord4iv
        295  126 00002CE0 glTexCoord4s
        296  127 00002CEC glTexCoord4sv
        297  128 00003040 glTexCoordPointer
        298  129 000040E0 glTexEnvf
        299  12A 00004114 glTexEnvfv
        300  12B 00004148 glTexEnvi
        301  12C 0000417C glTexEnviv
        302  12D 000041B0 glTexGend
        303  12E 000041E4 glTexGendv
        304  12F 00004218 glTexGenf
        305  130 0000424C glTexGenfv
        306  131 00004280 glTexGeni
        307  132 000042B4 glTexGeniv
        308  133 0000328C glTexImage1D
        309  134 000032C0 glTexImage2D
        310  135 00004010 glTexParameterf
        311  136 00004044 glTexParameterfv
        312  137 00004078 glTexParameteri
        313  138 000040AC glTexParameteriv
        314  139 000053C4 glTexSubImage1D
        315  13A 000053F8 glTexSubImage2D
        316  13B 00002F80 glTranslated
        317  13C 00002F8C glTranslatef
        318  13D 00002CF8 glVertex2d
        319  13E 00002D04 glVertex2dv
        320  13F 00002D10 glVertex2f
        321  140 00002D1C glVertex2fv
        322  141 00002D28 glVertex2i
        323  142 00002D34 glVertex2iv
        324  143 00002D40 glVertex2s
        325  144 00002D4C glVertex2sv
        326  145 00002D58 glVertex3d
        327  146 00002D64 glVertex3dv
        328  147 00002D70 glVertex3f
        329  148 00002D7C glVertex3fv
        330  149 00002D88 glVertex3i
        331  14A 00002D94 glVertex3iv
        332  14B 00002DA0 glVertex3s
        333  14C 00002DAC glVertex3sv
        334  14D 00002DB8 glVertex4d
        335  14E 00002DC4 glVertex4dv
        336  14F 00002DD0 glVertex4f
        337  150 00002DDC glVertex4fv
        338  151 00002DE8 glVertex4i
        339  152 00002DF4 glVertex4iv
        340  153 00002E00 glVertex4s
        341  154 00002E0C glVertex4sv
        342  155 0000304C glVertexPointer
        343  156 000051BC glViewport
        344  157 00026D6A wglChoosePixelFormat
        345  158 0001C25D wglCopyContext
        346  159 0001BACE wglCreateContext
        347  15A 0001B994 wglCreateLayerContext
        348  15B 0001BB1C wglDeleteContext
        349  15C 0001917D wglDescribeLayerPlane
        350  15D 00026581 wglDescribePixelFormat
        351  15E 0001BBAD wglGetCurrentContext
        352  15F 0001BBC9 wglGetCurrentDC
        353  160 0001C38A wglGetDefaultProcAddress
        354  161 00019286 wglGetLayerPaletteEntries
        355  162 00025DF1 wglGetPixelFormat
        356  163 0001C3A2 wglGetProcAddress
        357  164 00019BD5 wglMakeCurrent
        358  165 000192EC wglRealizeLayerPalette
        359  166 00019211 wglSetLayerPaletteEntries
        360  167 00026BC0 wglSetPixelFormat
        361  168 0001C051 wglShareLists
        362  169 000266A4 wglSwapBuffers
        363  16A 00019355 wglSwapLayerBuffers
        364  16B 0001A95D wglSwapMultipleBuffers
        365  16C 0001C348 wglUseFontBitmapsA
        366  16D 0001C369 wglUseFontBitmapsW
        367  16E 0000E70C wglUseFontOutlinesA
        368  16F 0000E742 wglUseFontOutlinesW

  Summary

       21000 .data
        5000 .reloc
        1000 .rsrc
       A4000 .text
||#