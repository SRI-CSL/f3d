;; -*- Mode:Lisp; Syntax: Common-Lisp; Package:GL -*-

;;; This used to be  Automatically generated Lisp code from C file 
;;; /tmp_mnt/home/clam1/quam/Mesa/quam/gl.h on Wed 17-Jan-96 16:06:45.

;;; MANY MANUAL EDITS WERE REQUIRED BECAUSE OF INCORRECT HANDLING OF (VOID) ARGLISTS
;;; and signed-32bit and newer OpenGL features.

;;; Wed Apr 18 2007
;;; FIXME:  Need selectively define only features supported by the current OpenGL version.
;;;         Not sure how to do that because OpenGL version. is determined for the 
;;;         "current" OpenGL connection.

(in-package :GL)


#| ***************************  BUGS  ***************************
LCL doesn't pass single float to C.
Many of the declarations have args declared as GLclampf, GLfloat or GLshort, etc.

Must add functions to glffi.c to convert from double to single.

Foreign function declarations that need argument conversions added to glffi.c
have been commented out.
|#

(eval-when (load eval compile)
  ;; In Allegro, these must be available at compile time for the #. reader macro) 
(defconstant GL_FALSE                               0)
(defconstant GL_TRUE                                1)
(defconstant GL_BYTE                             5120)
(defconstant GL_UNSIGNED_BYTE                    5121)
(defconstant GL_SHORT                            5122)
(defconstant GL_UNSIGNED_SHORT                   5123)
(defconstant GL_INT                              5124)
(defconstant GL_UNSIGNED_INT                     5125)
(defconstant GL_FLOAT                            5126)
(defconstant GL_DOUBLE #x140A)
(defconstant GL_2_BYTES                          5127)
(defconstant GL_3_BYTES                          5128)
(defconstant GL_4_BYTES                          5129)
(defconstant GL_LINES                               1)
(defconstant GL_POINTS                              0)
(defconstant GL_LINE_STRIP                          3)
(defconstant GL_LINE_LOOP                           2)
(defconstant GL_TRIANGLES                           4)
(defconstant GL_TRIANGLE_STRIP                      5)
(defconstant GL_TRIANGLE_FAN                        6)
(defconstant GL_QUADS                               7)
(defconstant GL_QUAD_STRIP                          8)
(defconstant GL_POLYGON                             9)
(defconstant GL_EDGE_FLAG                        2883)
(defconstant GL_MATRIX_MODE                      2976)
(defconstant GL_MODELVIEW                        5888)
(defconstant GL_PROJECTION                       5889)
(defconstant GL_TEXTURE                          5890)
(defconstant GL_POINT_SMOOTH                     2832)
(defconstant GL_POINT_SIZE                       2833)
(defconstant GL_POINT_SIZE_GRANULARITY           2835)
(defconstant GL_POINT_SIZE_RANGE                 2834)
(defconstant GL_LINE_SMOOTH                      2848)
(defconstant GL_LINE_STIPPLE                     2852)
(defconstant GL_LINE_STIPPLE_PATTERN             2853)
(defconstant GL_LINE_STIPPLE_REPEAT              2854)
(defconstant GL_LINE_WIDTH                       2849)
(defconstant GL_LINE_WIDTH_GRANULARITY           2851)
(defconstant GL_LINE_WIDTH_RANGE                 2850)
(defconstant GL_POINT                            6912)
(defconstant GL_LINE                             6913)
(defconstant GL_FILL                             6914)
(defconstant GL_CCW                              2305)
(defconstant GL_CW                               2304)
(defconstant GL_FRONT                            1028)
(defconstant GL_BACK                             1029)
(defconstant GL_CULL_FACE                        2884)
(defconstant GL_CULL_FACE_MODE                   2885)
(defconstant GL_POLYGON_SMOOTH                   2881)
(defconstant GL_POLYGON_STIPPLE                  2882)
(defconstant GL_FRONT_FACE                       2886)
(defconstant GL_POLYGON_MODE                     2880)
(defconstant GL_COMPILE                          4864)
(defconstant GL_COMPILE_AND_EXECUTE              4865)
(defconstant GL_LIST_BASE                        2866)
(defconstant GL_LIST_INDEX                       2867)
(defconstant GL_LIST_MODE                        2864)
(defconstant GL_NEVER                             512)
(defconstant GL_LESS                              513)
(defconstant GL_GEQUAL                            518)
(defconstant GL_LEQUAL                            515)
(defconstant GL_GREATER                           516)
(defconstant GL_NOTEQUAL                          517)
(defconstant GL_EQUAL                             514)
(defconstant GL_ALWAYS                            519)
(defconstant GL_DEPTH_TEST                       2929)
(defconstant GL_DEPTH_BITS                       3414)
(defconstant GL_DEPTH_CLEAR_VALUE                2931)
(defconstant GL_DEPTH_FUNC                       2932)
(defconstant GL_DEPTH_RANGE                      2928)
(defconstant GL_DEPTH_WRITEMASK                  2930)
(defconstant GL_DEPTH_COMPONENT                  6402)
(defconstant GL_LIGHTING                         2896)
(defconstant GL_LIGHT0                          16384)
(defconstant GL_LIGHT1                          16385)
(defconstant GL_LIGHT2                          16386)
(defconstant GL_LIGHT3                          16387)
(defconstant GL_LIGHT4                          16388)
(defconstant GL_LIGHT5                          16389)
(defconstant GL_LIGHT6                          16390)
(defconstant GL_LIGHT7                          16391)
(defconstant GL_SPOT_EXPONENT                    4613)
(defconstant GL_SPOT_CUTOFF                      4614)
(defconstant GL_CONSTANT_ATTENUATION             4615)
(defconstant GL_LINEAR_ATTENUATION               4616)
(defconstant GL_QUADRATIC_ATTENUATION            4617)
(defconstant GL_AMBIENT                          4608)
(defconstant GL_DIFFUSE                          4609)
(defconstant GL_SPECULAR                         4610)
(defconstant GL_SHININESS                        5633)
(defconstant GL_EMISSION                         5632)
(defconstant GL_POSITION                         4611)
(defconstant GL_SPOT_DIRECTION                   4612)
(defconstant GL_AMBIENT_AND_DIFFUSE              5634)
(defconstant GL_COLOR_INDEXES                    5635)
(defconstant GL_LIGHT_MODEL_TWO_SIDE             2898)
(defconstant GL_LIGHT_MODEL_LOCAL_VIEWER         2897)
(defconstant GL_LIGHT_MODEL_AMBIENT              2899)
(defconstant GL_FRONT_AND_BACK                   1032)
(defconstant GL_SHADE_MODEL                      2900)
(defconstant GL_FLAT                             7424)
(defconstant GL_SMOOTH                           7425)
(defconstant GL_COLOR_MATERIAL                   2903)
(defconstant GL_COLOR_MATERIAL_FACE              2901)
(defconstant GL_COLOR_MATERIAL_PARAMETER         2902)
(defconstant GL_NORMALIZE                        2977)
(defconstant GL_CLIP_PLANE0                     12288)
(defconstant GL_CLIP_PLANE1                     12289)
(defconstant GL_CLIP_PLANE2                     12290)
(defconstant GL_CLIP_PLANE3                     12291)
(defconstant GL_CLIP_PLANE4                     12292)
(defconstant GL_CLIP_PLANE5                     12293)
(defconstant GL_ACCUM_RED_BITS                   3416)
(defconstant GL_ACCUM_GREEN_BITS                 3417)
(defconstant GL_ACCUM_BLUE_BITS                  3418)
(defconstant GL_ACCUM_ALPHA_BITS                 3419)
(defconstant GL_ACCUM_CLEAR_VALUE                2944)
(defconstant GL_ACCUM                             256)
(defconstant GL_ADD                               260)
(defconstant GL_LOAD                              257)
(defconstant GL_MULT                              259)
(defconstant GL_RETURN                            258)
(defconstant GL_ALPHA_TEST                       3008)
(defconstant GL_ALPHA_TEST_REF                   3010)
(defconstant GL_ALPHA_TEST_FUNC                  3009)
(defconstant GL_BLEND                            3042)
(defconstant GL_BLEND_SRC                        3041)
(defconstant GL_BLEND_DST                        3040)
(defconstant GL_ZERO                                0)
(defconstant GL_ONE                                 1)
(defconstant GL_SRC_COLOR                         768)
(defconstant GL_ONE_MINUS_SRC_COLOR               769)
(defconstant GL_DST_COLOR                         774)
(defconstant GL_ONE_MINUS_DST_COLOR               775)
(defconstant GL_SRC_ALPHA                         770)
(defconstant GL_ONE_MINUS_SRC_ALPHA               771)
(defconstant GL_DST_ALPHA                         772)
(defconstant GL_ONE_MINUS_DST_ALPHA               773)
(defconstant GL_SRC_ALPHA_SATURATE                776)
(defconstant GL_FEEDBACK                         7169)
(defconstant GL_RENDER                           7168)
(defconstant GL_SELECT                           7170)
(defconstant GL_2D                               1536)
(defconstant GL_3D                               1537)
(defconstant GL_3D_COLOR                         1538)
(defconstant GL_3D_COLOR_TEXTURE                 1539)
(defconstant GL_4D_COLOR_TEXTURE                 1540)
(defconstant GL_POINT_TOKEN                      1793)
(defconstant GL_LINE_TOKEN                       1794)
(defconstant GL_LINE_RESET_TOKEN                 1799)
(defconstant GL_POLYGON_TOKEN                    1795)
(defconstant GL_BITMAP_TOKEN                     1796)
(defconstant GL_DRAW_PIXEL_TOKEN                 1797)
(defconstant GL_COPY_PIXEL_TOKEN                 1798)
(defconstant GL_PASS_THROUGH_TOKEN               1792)
(defconstant GL_FOG                              2912)
(defconstant GL_FOG_MODE                         2917)
(defconstant GL_FOG_DENSITY                      2914)
(defconstant GL_FOG_COLOR                        2918)
(defconstant GL_FOG_INDEX                        2913)
(defconstant GL_FOG_START                        2915)
(defconstant GL_FOG_END                          2916)
(defconstant GL_LINEAR                           9729)
(defconstant GL_EXP                              2048)
(defconstant GL_EXP2                             2049)
(defconstant GL_LOGIC_OP                         3057)
(defconstant GL_COLOR_LOGIC_OP                   3058)
(defconstant GL_LOGIC_OP_MODE                    3056)
(defconstant GL_CLEAR                            5376)
(defconstant GL_SET                              5391)
(defconstant GL_COPY                             5379)
(defconstant GL_COPY_INVERTED                    5388)
(defconstant GL_NOOP                             5381)
(defconstant GL_INVERT                           5386)
(defconstant GL_AND                              5377)
(defconstant GL_NAND                             5390)
(defconstant GL_OR                               5383)
(defconstant GL_NOR                              5384)
(defconstant GL_XOR                              5382)
(defconstant GL_EQUIV                            5385)
(defconstant GL_AND_REVERSE                      5378)
(defconstant GL_AND_INVERTED                     5380)
(defconstant GL_OR_REVERSE                       5387)
(defconstant GL_OR_INVERTED                      5389)
(defconstant GL_STENCIL_TEST                     2960)
(defconstant GL_STENCIL_WRITEMASK                2968)
(defconstant GL_STENCIL_BITS                     3415)
(defconstant GL_STENCIL_FUNC                     2962)
(defconstant GL_STENCIL_VALUE_MASK               2963)
(defconstant GL_STENCIL_REF                      2967)
(defconstant GL_STENCIL_FAIL                     2964)
(defconstant GL_STENCIL_PASS_DEPTH_PASS          2966)
(defconstant GL_STENCIL_PASS_DEPTH_FAIL          2965)
(defconstant GL_STENCIL_CLEAR_VALUE              2961)
(defconstant GL_STENCIL_INDEX                    6401)
(defconstant GL_KEEP                             7680)
(defconstant GL_REPLACE                          7681)
(defconstant GL_INCR                             7682)
(defconstant GL_DECR                             7683)
(defconstant GL_NONE                                0)
(defconstant GL_LEFT                             1030)
(defconstant GL_RIGHT                            1031)
(defconstant GL_FRONT_LEFT                       1024)
(defconstant GL_FRONT_RIGHT                      1025)
(defconstant GL_BACK_LEFT                        1026)
(defconstant GL_BACK_RIGHT                       1027)
(defconstant GL_AUX0                             1033)
(defconstant GL_AUX1                             1034)
(defconstant GL_AUX2                             1035)
(defconstant GL_AUX3                             1036)
(defconstant GL_COLOR_INDEX                      6400)
(defconstant GL_RED                              6403)
(defconstant GL_GREEN                            6404)
(defconstant GL_BLUE                             6405)
(defconstant GL_ALPHA                            6406)
(defconstant GL_LUMINANCE                        6409)
(defconstant GL_LUMINANCE_ALPHA                  6410)
(defconstant GL_ALPHA_BITS                       3413)
(defconstant GL_RED_BITS                         3410)
(defconstant GL_GREEN_BITS                       3411)
(defconstant GL_BLUE_BITS                        3412)
(defconstant GL_INDEX_BITS                       3409)
(defconstant GL_SUBPIXEL_BITS                    3408)
(defconstant GL_AUX_BUFFERS                      3072)
(defconstant GL_READ_BUFFER                      3074)
(defconstant GL_DRAW_BUFFER                      3073)
(defconstant GL_DOUBLEBUFFER                     3122)
(defconstant GL_STEREO                           3123)
(defconstant GL_BITMAP                           6656)
(defconstant GL_COLOR                            6144)
(defconstant GL_DEPTH                            6145)
(defconstant GL_STENCIL                          6146)
(defconstant GL_DITHER                           3024)
(defconstant GL_RGB                              6407)
(defconstant GL_RGBA                             6408)
(defconstant GL_ABGR_EXT                         #x8000)
(defconstant GL_RGB4_EXT                         #x804F)
(defconstant GL_RGB5_A1_EXT                      #x8057)
(defconstant GL_MAX_MODELVIEW_STACK_DEPTH        3382)
(defconstant GL_MAX_PROJECTION_STACK_DEPTH       3384)
(defconstant GL_MAX_TEXTURE_STACK_DEPTH          3385)
(defconstant GL_MAX_ATTRIB_STACK_DEPTH           3381)
(defconstant GL_MAX_NAME_STACK_DEPTH             3383)
(defconstant GL_MAX_LIST_NESTING                 2865)
(defconstant GL_MAX_LIGHTS                       3377)
(defconstant GL_MAX_CLIP_PLANES                  3378)
(defconstant GL_MAX_VIEWPORT_DIMS                3386)
(defconstant GL_MAX_PIXEL_MAP_TABLE              3380)
(defconstant GL_MAX_EVAL_ORDER                   3376)
(defconstant GL_MAX_TEXTURE_SIZE                 3379)
(defconstant GL_ATTRIB_STACK_DEPTH               2992)
(defconstant GL_COLOR_CLEAR_VALUE                3106)
(defconstant GL_COLOR_WRITEMASK                  3107)
(defconstant GL_CURRENT_INDEX                    2817)
(defconstant GL_CURRENT_COLOR                    2816)
(defconstant GL_CURRENT_NORMAL                   2818)
(defconstant GL_CURRENT_RASTER_COLOR             2820)
(defconstant GL_CURRENT_RASTER_DISTANCE          2825)
(defconstant GL_CURRENT_RASTER_INDEX             2821)
(defconstant GL_CURRENT_RASTER_POSITION          2823)
(defconstant GL_CURRENT_RASTER_TEXTURE_COORDS    2822)
(defconstant GL_CURRENT_RASTER_POSITION_VALID    2824)
(defconstant GL_CURRENT_TEXTURE_COORDS           2819)
(defconstant GL_INDEX_CLEAR_VALUE                3104)
(defconstant GL_INDEX_MODE                       3120)
(defconstant GL_INDEX_WRITEMASK                  3105)
(defconstant GL_MODELVIEW_MATRIX                 2982)
(defconstant GL_MODELVIEW_STACK_DEPTH            2979)
(defconstant GL_NAME_STACK_DEPTH                 3440)
(defconstant GL_PROJECTION_MATRIX                2983)
(defconstant GL_PROJECTION_STACK_DEPTH           2980)
(defconstant GL_RENDER_MODE                      3136)
(defconstant GL_RGBA_MODE                        3121)
(defconstant GL_TEXTURE_MATRIX                   2984)
(defconstant GL_TEXTURE_STACK_DEPTH              2981)
(defconstant GL_VIEWPORT                         2978)
(defconstant GL_AUTO_NORMAL                      3456)
(defconstant GL_MAP1_COLOR_4                     3472)
(defconstant GL_MAP1_GRID_DOMAIN                 3536)
(defconstant GL_MAP1_GRID_SEGMENTS               3537)
(defconstant GL_MAP1_INDEX                       3473)
(defconstant GL_MAP1_NORMAL                      3474)
(defconstant GL_MAP1_TEXTURE_COORD_1             3475)
(defconstant GL_MAP1_TEXTURE_COORD_2             3476)
(defconstant GL_MAP1_TEXTURE_COORD_3             3477)
(defconstant GL_MAP1_TEXTURE_COORD_4             3478)
(defconstant GL_MAP1_VERTEX_3                    3479)
(defconstant GL_MAP1_VERTEX_4                    3480)
(defconstant GL_MAP2_COLOR_4                     3504)
(defconstant GL_MAP2_GRID_DOMAIN                 3538)
(defconstant GL_MAP2_GRID_SEGMENTS               3539)
(defconstant GL_MAP2_INDEX                       3505)
(defconstant GL_MAP2_NORMAL                      3506)
(defconstant GL_MAP2_TEXTURE_COORD_1             3507)
(defconstant GL_MAP2_TEXTURE_COORD_2             3508)
(defconstant GL_MAP2_TEXTURE_COORD_3             3509)
(defconstant GL_MAP2_TEXTURE_COORD_4             3510)
(defconstant GL_MAP2_VERTEX_3                    3511)
(defconstant GL_MAP2_VERTEX_4                    3512)
(defconstant GL_COEFF                            2560)
(defconstant GL_DOMAIN                           2562)
(defconstant GL_ORDER                            2561)
(defconstant GL_FOG_HINT                         3156)
(defconstant GL_LINE_SMOOTH_HINT                 3154)
(defconstant GL_PERSPECTIVE_CORRECTION_HINT      3152)
(defconstant GL_POINT_SMOOTH_HINT                3153)
(defconstant GL_POLYGON_SMOOTH_HINT              3155)
(defconstant GL_DONT_CARE                        4352)
(defconstant GL_FASTEST                          4353)
(defconstant GL_NICEST                           4354)
(defconstant GL_SCISSOR_TEST                     3089)
(defconstant GL_SCISSOR_BOX                      3088)
(defconstant GL_MAP_COLOR                        3344)
(defconstant GL_MAP_STENCIL                      3345)
(defconstant GL_INDEX_SHIFT                      3346)
(defconstant GL_INDEX_OFFSET                     3347)
(defconstant GL_RED_SCALE                        3348)
(defconstant GL_RED_BIAS                         3349)
(defconstant GL_GREEN_SCALE                      3352)
(defconstant GL_GREEN_BIAS                       3353)
(defconstant GL_BLUE_SCALE                       3354)
(defconstant GL_BLUE_BIAS                        3355)
(defconstant GL_ALPHA_SCALE                      3356)
(defconstant GL_ALPHA_BIAS                       3357)
(defconstant GL_DEPTH_SCALE                      3358)
(defconstant GL_DEPTH_BIAS                       3359)
(defconstant GL_PIXEL_MAP_S_TO_S_SIZE            3249)
(defconstant GL_PIXEL_MAP_I_TO_I_SIZE            3248)
(defconstant GL_PIXEL_MAP_I_TO_R_SIZE            3250)
(defconstant GL_PIXEL_MAP_I_TO_G_SIZE            3251)
(defconstant GL_PIXEL_MAP_I_TO_B_SIZE            3252)
(defconstant GL_PIXEL_MAP_I_TO_A_SIZE            3253)
(defconstant GL_PIXEL_MAP_R_TO_R_SIZE            3254)
(defconstant GL_PIXEL_MAP_G_TO_G_SIZE            3255)
(defconstant GL_PIXEL_MAP_B_TO_B_SIZE            3256)
(defconstant GL_PIXEL_MAP_A_TO_A_SIZE            3257)
(defconstant GL_PIXEL_MAP_S_TO_S                 3185)
(defconstant GL_PIXEL_MAP_I_TO_I                 3184)
(defconstant GL_PIXEL_MAP_I_TO_R                 3186)
(defconstant GL_PIXEL_MAP_I_TO_G                 3187)
(defconstant GL_PIXEL_MAP_I_TO_B                 3188)
(defconstant GL_PIXEL_MAP_I_TO_A                 3189)
(defconstant GL_PIXEL_MAP_R_TO_R                 3190)
(defconstant GL_PIXEL_MAP_G_TO_G                 3191)
(defconstant GL_PIXEL_MAP_B_TO_B                 3192)
(defconstant GL_PIXEL_MAP_A_TO_A                 3193)
(defconstant GL_PACK_ALIGNMENT                   3333)
(defconstant GL_PACK_LSB_FIRST                   3329)
(defconstant GL_PACK_ROW_LENGTH                  3330)
(defconstant GL_PACK_SKIP_PIXELS                 3332)
(defconstant GL_PACK_SKIP_ROWS                   3331)
(defconstant GL_PACK_SWAP_BYTES                  3328)
(defconstant GL_UNPACK_ALIGNMENT                 3317)
(defconstant GL_UNPACK_LSB_FIRST                 3313)
(defconstant GL_UNPACK_ROW_LENGTH                3314)
(defconstant GL_UNPACK_SKIP_PIXELS               3316)
(defconstant GL_UNPACK_SKIP_ROWS                 3315)
(defconstant GL_UNPACK_SWAP_BYTES                3312)
(defconstant GL_ZOOM_X                           3350)
(defconstant GL_ZOOM_Y                           3351)
(defconstant GL_TEXTURE_ENV                      8960)
(defconstant GL_TEXTURE_ENV_MODE                 8704)
(defconstant GL_TEXTURE_1D                       3552)
(defconstant GL_TEXTURE_2D                       3553)
(defconstant GL_TEXTURE_WRAP_S                  10242)
(defconstant GL_TEXTURE_WRAP_T                  10243)
(defconstant GL_TEXTURE_MAG_FILTER              10240)
(defconstant GL_TEXTURE_MIN_FILTER              10241)
(defconstant GL_TEXTURE_ENV_COLOR                8705)
(defconstant GL_TEXTURE_GEN_S                    3168)
(defconstant GL_TEXTURE_GEN_T                    3169)
(defconstant GL_TEXTURE_GEN_MODE                 9472)
(defconstant GL_TEXTURE_BORDER_COLOR             4100)
(defconstant GL_TEXTURE_WIDTH                    4096)
(defconstant GL_TEXTURE_HEIGHT                   4097)
(defconstant GL_TEXTURE_BORDER                   4101)
(defconstant GL_TEXTURE_COMPONENTS               4099)
(defconstant GL_NEAREST_MIPMAP_NEAREST           9984)
(defconstant GL_NEAREST_MIPMAP_LINEAR            9986)
(defconstant GL_LINEAR_MIPMAP_NEAREST            9985)
(defconstant GL_LINEAR_MIPMAP_LINEAR             9987)
(defconstant GL_OBJECT_LINEAR                    9217)
(defconstant GL_OBJECT_PLANE                     9473)
(defconstant GL_EYE_LINEAR                       9216)
(defconstant GL_EYE_PLANE                        9474)
(defconstant GL_SPHERE_MAP                       9218)
(defconstant GL_DECAL                            8449)
(defconstant GL_MODULATE                         8448)
(defconstant GL_NEAREST                          9728)
(defconstant GL_REPEAT                          10497)
(defconstant GL_CLAMP                           10496)
(defconstant GL_S                                8192)
(defconstant GL_T                                8193)
(defconstant GL_R                                8194)
(defconstant GL_Q                                8195)
(defconstant GL_TEXTURE_GEN_R                    3170)
(defconstant GL_TEXTURE_GEN_Q                    3171)
(defconstant GL_VENDOR                           7936)
(defconstant GL_RENDERER                         7937)
(defconstant GL_VERSION                          7938)
(defconstant GL_EXTENSIONS                       7939)
(defconstant GL_INVALID_VALUE                    1281)
(defconstant GL_INVALID_ENUM                     1280)
(defconstant GL_INVALID_OPERATION                1282)
(defconstant GL_STACK_OVERFLOW                   1283)
(defconstant GL_STACK_UNDERFLOW                  1284)
(defconstant GL_OUT_OF_MEMORY                    1285)
(defconstant GL_CONSTANT_COLOR_EXT              32769)
(defconstant GL_ONE_MINUS_CONSTANT_COLOR_EXT    32770)
(defconstant GL_CONSTANT_ALPHA_EXT              32771)
(defconstant GL_ONE_MINUS_CONSTANT_ALPHA_EXT    32772)
(defconstant GL_BLEND_EQUATION_EXT              32777)
(defconstant GL_MIN_EXT                         32775)
(defconstant GL_MAX_EXT                         32776)
(defconstant GL_FUNC_ADD_EXT                    32774)
(defconstant GL_FUNC_SUBTRACT_EXT               32778)
(defconstant GL_FUNC_REVERSE_SUBTRACT_EXT       32779)
(defconstant GL_BLEND_COLOR_EXT                 32773)
(defconstant GL_REPLACE_EXT                     32866)
(defconstant GL_POLYGON_OFFSET_EXT              32823)
(defconstant GL_POLYGON_OFFSET_FILL             32823)
(defconstant GL_POLYGON_OFFSET_LINE             #x2a02)
(defconstant GL_POLYGON_OFFSET_POINT            #x2A01)
(defconstant GL_POLYGON_OFFSET_FACTOR           #x8038)
(defconstant GL_POLYGON_OFFSET_UNITS            #x2A00)
(defconstant GL_POLYGON_OFFSET_FACTOR_EXT       32824)
(defconstant GL_POLYGON_OFFSET_BIAS_EXT         32825)

(defconstant GL_TEXTURE_BASE_LEVEL #x813C)
(defconstant GL_TEXTURE_MAX_LEVEL #x813D)
(defconstant GL_TEXTURE_INTERNAL_FORMAT #x1003)
(defconstant GL_GENERATE_MIPMAP #x8191)
(defconstant GL_GENERATE_MIPMAP_HINT #x8192)

;; pixel internal formats
(defconstant GL_LUMINANCE4 #x803F)
(defconstant GL_LUMINANCE8 #x8040)
(defconstant GL_LUMINANCE12 #x8041)
(defconstant GL_LUMINANCE16 #x8042)
(defconstant GL_RGB8 #x8051)
(defconstant GL_RGBA8 #x8058)
(defconstant GL_R3_G3_B2 #x2A10)
(defconstant GL_RGB10_A2 #x8059)
(defconstant GL_RGB5_A1 #x8057)
(defconstant GL_RGB5 #x8050)
(defconstant GL_PROXY_TEXTURE_2D #x8064)
(defconstant GL_TEXTURE_RED_SIZE #x805C)
(defconstant GL_TEXTURE_GREEN_SIZE #x805D)
(defconstant GL_TEXTURE_BLUE_SIZE #x805E)
(defconstant GL_TEXTURE_ALPHA_SIZE #x805F)
(defconstant GL_TEXTURE_LUMINANCE_SIZE #x8060)

;;;  OpenGL 1.3

(defconstant GL_COMPRESSED_RGB  #x84ED)
(defconstant GL_COMPRESSED_RGBA #x84EE)
(defconstant GL_COMPRESSED_LUMINANCE #x84EA)
(defconstant GL_NUM_COMPRESSED_TEXTURE_FORMATS #x86A2)
(defconstant GL_COMPRESSED_TEXTURE_FORMATS #x86A3)

) ;; end eval-when


(deftype GLenum ()
  '(member
     #.GL_FALSE
     #.GL_TRUE
     #.GL_BYTE
     #.GL_UNSIGNED_BYTE
     #.GL_SHORT
     #.GL_UNSIGNED_SHORT
     #.GL_INT
     #.GL_UNSIGNED_INT
     #.GL_FLOAT
     #.GL_2_BYTES
     #.GL_3_BYTES
     #.GL_4_BYTES
     #.GL_LINES
     #.GL_POINTS
     #.GL_LINE_STRIP
     #.GL_LINE_LOOP
     #.GL_TRIANGLES
     #.GL_TRIANGLE_STRIP
     #.GL_TRIANGLE_FAN
     #.GL_QUADS
     #.GL_QUAD_STRIP
     #.GL_POLYGON
     #.GL_EDGE_FLAG
     #.GL_MATRIX_MODE
     #.GL_MODELVIEW
     #.GL_PROJECTION
     #.GL_TEXTURE
     #.GL_POINT_SMOOTH
     #.GL_POINT_SIZE
     #.GL_POINT_SIZE_GRANULARITY
     #.GL_POINT_SIZE_RANGE
     #.GL_LINE_SMOOTH
     #.GL_LINE_STIPPLE
     #.GL_LINE_STIPPLE_PATTERN
     #.GL_LINE_STIPPLE_REPEAT
     #.GL_LINE_WIDTH
     #.GL_LINE_WIDTH_GRANULARITY
     #.GL_LINE_WIDTH_RANGE
     #.GL_POINT
     #.GL_LINE
     #.GL_FILL
     #.GL_CCW
     #.GL_CW
     #.GL_FRONT
     #.GL_BACK
     #.GL_CULL_FACE
     #.GL_CULL_FACE_MODE
     #.GL_POLYGON_SMOOTH
     #.GL_POLYGON_STIPPLE
     #.GL_FRONT_FACE
     #.GL_POLYGON_MODE
     #.GL_COMPILE
     #.GL_COMPILE_AND_EXECUTE
     #.GL_LIST_BASE
     #.GL_LIST_INDEX
     #.GL_LIST_MODE
     #.GL_NEVER
     #.GL_LESS
     #.GL_GEQUAL
     #.GL_LEQUAL
     #.GL_GREATER
     #.GL_NOTEQUAL
     #.GL_EQUAL
     #.GL_ALWAYS
     #.GL_DEPTH_TEST
     #.GL_DEPTH_BITS
     #.GL_DEPTH_CLEAR_VALUE
     #.GL_DEPTH_FUNC
     #.GL_DEPTH_RANGE
     #.GL_DEPTH_WRITEMASK
     #.GL_DEPTH_COMPONENT
     #.GL_LIGHTING
     #.GL_LIGHT0
     #.GL_LIGHT1
     #.GL_LIGHT2
     #.GL_LIGHT3
     #.GL_LIGHT4
     #.GL_LIGHT5
     #.GL_LIGHT6
     #.GL_LIGHT7
     #.GL_SPOT_EXPONENT
     #.GL_SPOT_CUTOFF
     #.GL_CONSTANT_ATTENUATION
     #.GL_LINEAR_ATTENUATION
     #.GL_QUADRATIC_ATTENUATION
     #.GL_AMBIENT
     #.GL_DIFFUSE
     #.GL_SPECULAR
     #.GL_SHININESS
     #.GL_EMISSION
     #.GL_POSITION
     #.GL_SPOT_DIRECTION
     #.GL_AMBIENT_AND_DIFFUSE
     #.GL_COLOR_INDEXES
     #.GL_LIGHT_MODEL_TWO_SIDE
     #.GL_LIGHT_MODEL_LOCAL_VIEWER
     #.GL_LIGHT_MODEL_AMBIENT
     #.GL_FRONT_AND_BACK
     #.GL_SHADE_MODEL
     #.GL_FLAT
     #.GL_SMOOTH
     #.GL_COLOR_MATERIAL
     #.GL_COLOR_MATERIAL_FACE
     #.GL_COLOR_MATERIAL_PARAMETER
     #.GL_NORMALIZE
     #.GL_CLIP_PLANE0
     #.GL_CLIP_PLANE1
     #.GL_CLIP_PLANE2
     #.GL_CLIP_PLANE3
     #.GL_CLIP_PLANE4
     #.GL_CLIP_PLANE5
     #.GL_ACCUM_RED_BITS
     #.GL_ACCUM_GREEN_BITS
     #.GL_ACCUM_BLUE_BITS
     #.GL_ACCUM_ALPHA_BITS
     #.GL_ACCUM_CLEAR_VALUE
     #.GL_ACCUM
     #.GL_ADD
     #.GL_LOAD
     #.GL_MULT
     #.GL_RETURN
     #.GL_ALPHA_TEST
     #.GL_ALPHA_TEST_REF
     #.GL_ALPHA_TEST_FUNC
     #.GL_BLEND
     #.GL_BLEND_SRC
     #.GL_BLEND_DST
     #.GL_ZERO
     #.GL_ONE
     #.GL_SRC_COLOR
     #.GL_ONE_MINUS_SRC_COLOR
     #.GL_DST_COLOR
     #.GL_ONE_MINUS_DST_COLOR
     #.GL_SRC_ALPHA
     #.GL_ONE_MINUS_SRC_ALPHA
     #.GL_DST_ALPHA
     #.GL_ONE_MINUS_DST_ALPHA
     #.GL_SRC_ALPHA_SATURATE
     #.GL_FEEDBACK
     #.GL_RENDER
     #.GL_SELECT
     #.GL_2D
     #.GL_3D
     #.GL_3D_COLOR
     #.GL_3D_COLOR_TEXTURE
     #.GL_4D_COLOR_TEXTURE
     #.GL_POINT_TOKEN
     #.GL_LINE_TOKEN
     #.GL_LINE_RESET_TOKEN
     #.GL_POLYGON_TOKEN
     #.GL_BITMAP_TOKEN
     #.GL_DRAW_PIXEL_TOKEN
     #.GL_COPY_PIXEL_TOKEN
     #.GL_PASS_THROUGH_TOKEN
     #.GL_FOG
     #.GL_FOG_MODE
     #.GL_FOG_DENSITY
     #.GL_FOG_COLOR
     #.GL_FOG_INDEX
     #.GL_FOG_START
     #.GL_FOG_END
     #.GL_LINEAR
     #.GL_EXP
     #.GL_EXP2
     #.GL_LOGIC_OP
     #.GL_COLOR_LOGIC_OP  
     #.GL_LOGIC_OP_MODE
     #.GL_CLEAR
     #.GL_SET
     #.GL_COPY
     #.GL_COPY_INVERTED
     #.GL_NOOP
     #.GL_INVERT
     #.GL_AND
     #.GL_NAND
     #.GL_OR
     #.GL_NOR
     #.GL_XOR
     #.GL_EQUIV
     #.GL_AND_REVERSE
     #.GL_AND_INVERTED
     #.GL_OR_REVERSE
     #.GL_OR_INVERTED
     #.GL_STENCIL_TEST
     #.GL_STENCIL_WRITEMASK
     #.GL_STENCIL_BITS
     #.GL_STENCIL_FUNC
     #.GL_STENCIL_VALUE_MASK
     #.GL_STENCIL_REF
     #.GL_STENCIL_FAIL
     #.GL_STENCIL_PASS_DEPTH_PASS
     #.GL_STENCIL_PASS_DEPTH_FAIL
     #.GL_STENCIL_CLEAR_VALUE
     #.GL_STENCIL_INDEX
     #.GL_KEEP
     #.GL_REPLACE
     #.GL_INCR
     #.GL_DECR
     #.GL_NONE
     #.GL_LEFT
     #.GL_RIGHT
     #.GL_FRONT_LEFT
     #.GL_FRONT_RIGHT
     #.GL_BACK_LEFT
     #.GL_BACK_RIGHT
     #.GL_AUX0
     #.GL_AUX1
     #.GL_AUX2
     #.GL_AUX3
     #.GL_COLOR_INDEX
     #.GL_RED
     #.GL_GREEN
     #.GL_BLUE
     #.GL_ALPHA
     #.GL_LUMINANCE
     #.GL_LUMINANCE_ALPHA
     #.GL_ALPHA_BITS
     #.GL_RED_BITS
     #.GL_GREEN_BITS
     #.GL_BLUE_BITS
     #.GL_INDEX_BITS
     #.GL_SUBPIXEL_BITS
     #.GL_AUX_BUFFERS
     #.GL_READ_BUFFER
     #.GL_DRAW_BUFFER
     #.GL_DOUBLEBUFFER
     #.GL_STEREO
     #.GL_BITMAP
     #.GL_COLOR
     #.GL_DEPTH
     #.GL_STENCIL
     #.GL_DITHER
     #.GL_RGB
     #.GL_RGBA
     #.GL_MAX_MODELVIEW_STACK_DEPTH
     #.GL_MAX_PROJECTION_STACK_DEPTH
     #.GL_MAX_TEXTURE_STACK_DEPTH
     #.GL_MAX_ATTRIB_STACK_DEPTH
     #.GL_MAX_NAME_STACK_DEPTH
     #.GL_MAX_LIST_NESTING
     #.GL_MAX_LIGHTS
     #.GL_MAX_CLIP_PLANES
     #.GL_MAX_VIEWPORT_DIMS
     #.GL_MAX_PIXEL_MAP_TABLE
     #.GL_MAX_EVAL_ORDER
     #.GL_MAX_TEXTURE_SIZE
     #.GL_ATTRIB_STACK_DEPTH
     #.GL_COLOR_CLEAR_VALUE
     #.GL_COLOR_WRITEMASK
     #.GL_CURRENT_INDEX
     #.GL_CURRENT_COLOR
     #.GL_CURRENT_NORMAL
     #.GL_CURRENT_RASTER_COLOR
     #.GL_CURRENT_RASTER_DISTANCE
     #.GL_CURRENT_RASTER_INDEX
     #.GL_CURRENT_RASTER_POSITION
     #.GL_CURRENT_RASTER_TEXTURE_COORDS
     #.GL_CURRENT_RASTER_POSITION_VALID
     #.GL_CURRENT_TEXTURE_COORDS
     #.GL_INDEX_CLEAR_VALUE
     #.GL_INDEX_MODE
     #.GL_INDEX_WRITEMASK
     #.GL_MODELVIEW_MATRIX
     #.GL_MODELVIEW_STACK_DEPTH
     #.GL_NAME_STACK_DEPTH
     #.GL_PROJECTION_MATRIX
     #.GL_PROJECTION_STACK_DEPTH
     #.GL_RENDER_MODE
     #.GL_RGBA_MODE
     #.GL_TEXTURE_MATRIX
     #.GL_TEXTURE_STACK_DEPTH
     #.GL_VIEWPORT
     #.GL_AUTO_NORMAL
     #.GL_MAP1_COLOR_4
     #.GL_MAP1_GRID_DOMAIN
     #.GL_MAP1_GRID_SEGMENTS
     #.GL_MAP1_INDEX
     #.GL_MAP1_NORMAL
     #.GL_MAP1_TEXTURE_COORD_1
     #.GL_MAP1_TEXTURE_COORD_2
     #.GL_MAP1_TEXTURE_COORD_3
     #.GL_MAP1_TEXTURE_COORD_4
     #.GL_MAP1_VERTEX_3
     #.GL_MAP1_VERTEX_4
     #.GL_MAP2_COLOR_4
     #.GL_MAP2_GRID_DOMAIN
     #.GL_MAP2_GRID_SEGMENTS
     #.GL_MAP2_INDEX
     #.GL_MAP2_NORMAL
     #.GL_MAP2_TEXTURE_COORD_1
     #.GL_MAP2_TEXTURE_COORD_2
     #.GL_MAP2_TEXTURE_COORD_3
     #.GL_MAP2_TEXTURE_COORD_4
     #.GL_MAP2_VERTEX_3
     #.GL_MAP2_VERTEX_4
     #.GL_COEFF
     #.GL_DOMAIN
     #.GL_ORDER
     #.GL_FOG_HINT
     #.GL_LINE_SMOOTH_HINT
     #.GL_PERSPECTIVE_CORRECTION_HINT
     #.GL_POINT_SMOOTH_HINT
     #.GL_POLYGON_SMOOTH_HINT
     #.GL_DONT_CARE
     #.GL_FASTEST
     #.GL_NICEST
     #.GL_SCISSOR_TEST
     #.GL_SCISSOR_BOX
     #.GL_MAP_COLOR
     #.GL_MAP_STENCIL
     #.GL_INDEX_SHIFT
     #.GL_INDEX_OFFSET
     #.GL_RED_SCALE
     #.GL_RED_BIAS
     #.GL_GREEN_SCALE
     #.GL_GREEN_BIAS
     #.GL_BLUE_SCALE
     #.GL_BLUE_BIAS
     #.GL_ALPHA_SCALE
     #.GL_ALPHA_BIAS
     #.GL_DEPTH_SCALE
     #.GL_DEPTH_BIAS
     #.GL_PIXEL_MAP_S_TO_S_SIZE
     #.GL_PIXEL_MAP_I_TO_I_SIZE
     #.GL_PIXEL_MAP_I_TO_R_SIZE
     #.GL_PIXEL_MAP_I_TO_G_SIZE
     #.GL_PIXEL_MAP_I_TO_B_SIZE
     #.GL_PIXEL_MAP_I_TO_A_SIZE
     #.GL_PIXEL_MAP_R_TO_R_SIZE
     #.GL_PIXEL_MAP_G_TO_G_SIZE
     #.GL_PIXEL_MAP_B_TO_B_SIZE
     #.GL_PIXEL_MAP_A_TO_A_SIZE
     #.GL_PIXEL_MAP_S_TO_S
     #.GL_PIXEL_MAP_I_TO_I
     #.GL_PIXEL_MAP_I_TO_R
     #.GL_PIXEL_MAP_I_TO_G
     #.GL_PIXEL_MAP_I_TO_B
     #.GL_PIXEL_MAP_I_TO_A
     #.GL_PIXEL_MAP_R_TO_R
     #.GL_PIXEL_MAP_G_TO_G
     #.GL_PIXEL_MAP_B_TO_B
     #.GL_PIXEL_MAP_A_TO_A
     #.GL_PACK_ALIGNMENT
     #.GL_PACK_LSB_FIRST
     #.GL_PACK_ROW_LENGTH
     #.GL_PACK_SKIP_PIXELS
     #.GL_PACK_SKIP_ROWS
     #.GL_PACK_SWAP_BYTES
     #.GL_UNPACK_ALIGNMENT
     #.GL_UNPACK_LSB_FIRST
     #.GL_UNPACK_ROW_LENGTH
     #.GL_UNPACK_SKIP_PIXELS
     #.GL_UNPACK_SKIP_ROWS
     #.GL_UNPACK_SWAP_BYTES
     #.GL_ZOOM_X
     #.GL_ZOOM_Y
     #.GL_TEXTURE_ENV
     #.GL_TEXTURE_ENV_MODE
     #.GL_TEXTURE_1D
     #.GL_TEXTURE_2D
     #.GL_TEXTURE_WRAP_S
     #.GL_TEXTURE_WRAP_T
     #.GL_TEXTURE_MAG_FILTER
     #.GL_TEXTURE_MIN_FILTER
     #.GL_TEXTURE_ENV_COLOR
     #.GL_TEXTURE_GEN_S
     #.GL_TEXTURE_GEN_T
     #.GL_TEXTURE_GEN_MODE
     #.GL_TEXTURE_BORDER_COLOR
     #.GL_TEXTURE_WIDTH
     #.GL_TEXTURE_HEIGHT
     #.GL_TEXTURE_BORDER
     #.GL_TEXTURE_COMPONENTS
     #.GL_NEAREST_MIPMAP_NEAREST
     #.GL_NEAREST_MIPMAP_LINEAR
     #.GL_LINEAR_MIPMAP_NEAREST
     #.GL_LINEAR_MIPMAP_LINEAR
     #.GL_OBJECT_LINEAR
     #.GL_OBJECT_PLANE
     #.GL_EYE_LINEAR
     #.GL_EYE_PLANE
     #.GL_SPHERE_MAP
     #.GL_DECAL
     #.GL_MODULATE
     #.GL_NEAREST
     #.GL_REPEAT
     #.GL_CLAMP
     #.GL_S
     #.GL_T
     #.GL_R
     #.GL_Q
     #.GL_TEXTURE_GEN_R
     #.GL_TEXTURE_GEN_Q
     #.GL_VENDOR
     #.GL_RENDERER
     #.GL_VERSION
     #.GL_EXTENSIONS
     #.GL_INVALID_VALUE
     #.GL_INVALID_ENUM
     #.GL_INVALID_OPERATION
     #.GL_STACK_OVERFLOW
     #.GL_STACK_UNDERFLOW
     #.GL_OUT_OF_MEMORY
     #.GL_CONSTANT_COLOR_EXT
     #.GL_ONE_MINUS_CONSTANT_COLOR_EXT
     #.GL_CONSTANT_ALPHA_EXT
     #.GL_ONE_MINUS_CONSTANT_ALPHA_EXT
     #.GL_BLEND_EQUATION_EXT
     #.GL_MIN_EXT
     #.GL_MAX_EXT
     #.GL_FUNC_ADD_EXT
     #.GL_FUNC_SUBTRACT_EXT
     #.GL_FUNC_REVERSE_SUBTRACT_EXT
     #.GL_BLEND_COLOR_EXT
     #.GL_REPLACE_EXT
     #.GL_POLYGON_OFFSET_EXT
     #.GL_POLYGON_OFFSET_FACTOR_EXT
     #.GL_POLYGON_OFFSET_BIAS_EXT))



;;(def-foreign-synonym-type GLenum :signed-32bit)
(def-foreign-synonym-type GLenum :unsigned-32bit)

(eval-when (load eval compile)
(defconstant GL_CURRENT_BIT                 1)
(defconstant GL_POINT_BIT                   2)
(defconstant GL_LINE_BIT                    4)
(defconstant GL_POLYGON_BIT                 8)
(defconstant GL_POLYGON_STIPPLE_BIT        16)
(defconstant GL_PIXEL_MODE_BIT             32)
(defconstant GL_LIGHTING_BIT               64)
(defconstant GL_FOG_BIT                   128)
(defconstant GL_DEPTH_BUFFER_BIT          256)
(defconstant GL_ACCUM_BUFFER_BIT          512)
(defconstant GL_STENCIL_BUFFER_BIT       1024)
(defconstant GL_VIEWPORT_BIT             2048)
(defconstant GL_TRANSFORM_BIT            4096)
(defconstant GL_ENABLE_BIT               8192)
(defconstant GL_COLOR_BUFFER_BIT        16384)
(defconstant GL_HINT_BIT                32768)
(defconstant GL_EVAL_BIT                65536)
(defconstant GL_LIST_BIT               131072)
(defconstant GL_TEXTURE_BIT            262144)
(defconstant GL_SCISSOR_BIT            524288)
(defconstant GL_ALL_ATTRIB_BITS       1048575)

(defconstant GL_CLIENT_PIXEL_STORE_BIT		1)
(defconstant GL_CLIENT_VERTEX_ARRAY_BIT		2)
(defconstant GL_ALL_CLIENT_ATTRIB_BITS 		#xFFFFFFFF)

) ;end eval-when


(deftype GLbitfield ()
  '(member
     #.GL_CURRENT_BIT
     #.GL_POINT_BIT
     #.GL_LINE_BIT
     #.GL_POLYGON_BIT
     #.GL_POLYGON_STIPPLE_BIT
     #.GL_PIXEL_MODE_BIT
     #.GL_LIGHTING_BIT
     #.GL_FOG_BIT
     #.GL_DEPTH_BUFFER_BIT
     #.GL_ACCUM_BUFFER_BIT
     #.GL_STENCIL_BUFFER_BIT
     #.GL_VIEWPORT_BIT
     #.GL_TRANSFORM_BIT
     #.GL_ENABLE_BIT
     #.GL_COLOR_BUFFER_BIT
     #.GL_HINT_BIT
     #.GL_EVAL_BIT
     #.GL_LIST_BIT
     #.GL_TEXTURE_BIT
     #.GL_SCISSOR_BIT
     #.GL_ALL_ATTRIB_BITS))

(def-foreign-synonym-type GLmatrixd (:simple-array :double-float))
(def-foreign-synonym-type GLmatrixf (:simple-array :single-float))
(def-foreign-synonym-type GLvectord (:simple-array :double-float))
(def-foreign-synonym-type GLvectorf (:simple-array :single-float))
(def-foreign-synonym-type GLvectori (:simple-array :int))

#|
(lcl::lstruct-to-astruct '(:simple-array :double))
(lcl::def-foreign-synonym-type GLmatrixd (:simple-array :double))
(alien::expand-foreign-type 'GLvectord)
(gethash 'GLvectord alien::*user-defined-foreign-types*)
(alien::record-pointer-type  'GLvectord nil)
(def-foreign-synonym-type GLmatrixd (:simple-array :double-float))
(def-foreign-function (foo)
    (x GLmatrixd))
  |#

(def-foreign-synonym-type GLbitfield :signed-32bit)
;;(def-foreign-synonym-type GLvoid :null)
(def-foreign-synonym-type GLboolean :unsigned-8bit)
;;(def-foreign-synonym-type GLbooleanA :unsigned-int)
(def-foreign-synonym-type GLbooleanA :unsigned-32bit)
;;(def-foreign-synonym-type GLbooleanA :long)
(def-foreign-synonym-type GLbyte :signed-8bit)
(def-foreign-synonym-type GLbyteA :int)
(def-foreign-synonym-type GLshort :signed-16bit)
(def-foreign-synonym-type GLshortA :int)
;;(def-foreign-synonym-type GLint :signed-32bit)
(def-foreign-synonym-type GLint :int)
(def-foreign-synonym-type GLubyte :unsigned-8bit)
(def-foreign-synonym-type GLubyteA :unsigned-32bit)
(def-foreign-synonym-type GLushort :unsigned-16bit)
(def-foreign-synonym-type GLushortA :unsigned-32bit)
(def-foreign-synonym-type GLuint :unsigned-32bit)
(def-foreign-synonym-type GLsizei :signed-32bit)
(def-foreign-synonym-type GLfloat :single-float)
(def-foreign-synonym-type GLclampf :single-float)
(def-foreign-synonym-type GLdouble :double-float)
(def-foreign-synonym-type GLclampd :double-float)

;;; double-float versions of GL functions
;;; FIXME:  QFFI needs to be changed to coerce numbers to the declared numeric type

(def-foreign-function (glClearIndex
                       (:name (freedius-prefix "glClearIndex"))
                       (:return-type :null))
    (c :double-float)
  )


;;;(def-foreign-function (glClearColor
;;;                       (:name (freedius-prefix "glClearColor"))
;;;                       (:return-type :null))
;;;    (red   GLclampf)
;;;    (green  GLclampf)
;;;    (blue  GLclampf)
;;;    (alpha  GLclampf))

(def-foreign-function (glClearColor
                       (:name (freedius-prefix "glClearColor"))
                       (:return-type :null))
    (red   :double-float)
    (green  :double-float)
    (blue  :double-float)
    (alpha  :double-float))


(def-foreign-function (glClear
                       (:name "glClear")
                       (:return-type :null))
    (mask  GLbitfield))


(def-foreign-function (glIndexMask
                       (:name "glIndexMask")
                       (:return-type :null))
    (mask  GLuint))


(def-foreign-function (glColorMask
                       (:name "glColorMask")
                       (:return-type :null))
    (red   GLbooleanA)
    (green  GLbooleanA)
    (blue  GLbooleanA)
    (alpha  GLbooleanA))


;;;(def-foreign-function (glAlphaFunc
;;;                       (:name "glAlphaFunc")
;;;                       (:return-type :null))
;;;    (func  GLenum)
;;;    (ref  GLclampf))


(def-foreign-function (glBlendFunc
                       (:name "glBlendFunc")
                       (:return-type :null))
    (sfactor  GLenum)
    (dfactor  GLenum))

(def-foreign-function (glAlphaFunc
                       (:name "glAlphaFunc")
                       (:return-type :null))
    (func  GLenum)
    (ref  glClampf))


(def-foreign-function (glLogicOp
                       (:name "glLogicOp")
                       (:return-type :null))
    (opcode  GLenum))


(def-foreign-function (glCullFace
                       (:name "glCullFace")
                       (:return-type :null))
    (mode  GLenum))


(def-foreign-function (glFrontFace
                       (:name "glFrontFace")
                       (:return-type :null))
    (mode  GLenum))


;;;(def-foreign-function (glPointSize
;;;                       (:name "glPointSize")
;;;                       (:return-type :null))
;;;    (size  GLfloat))

(def-foreign-function (glPointSize
                       (:name (freedius-prefix "glPointSize"))
                       (:return-type :null))
    (size  GLdouble))


;;;(def-foreign-function (glLineWidth
;;;                       (:name "glLineWidth")
;;;                       (:return-type :null))
;;;    (width  GLfloat))

(def-foreign-function (glLineWidth
                       (:name (freedius-prefix "glLineWidth"))
                       (:return-type :null))
    (width  GLdouble))

(def-foreign-function (glLineStipple
                       (:name "glLineStipple")
                       (:return-type :null))
    (factor  GLint)
    (pattern  GLushortA))


(def-foreign-function (glPolygonMode
                       (:name "glPolygonMode")
                       (:return-type :null))
    (face  GLenum)
    (mode  GLenum))


(def-foreign-function (glPolygonStipple
                       (:name "glPolygonStipple")
                       (:return-type :null))
    (mask :simple-array)			;(:pointer GLubyte)
  )


(def-foreign-function (glGetPolygonStipple
                       (:name "glGetPolygonStipple")
                       (:return-type :null))
    (mask :simple-array)			;(:pointer GLubyte)
  )


(def-foreign-function (glEdgeFlag
                       (:name "glEdgeFlag")
                       (:return-type :null))
    (flag  GLbooleanA))


(def-foreign-function (glEdgeFlagv
                       (:name "glEdgeFlagv")
                       (:return-type :null))
    (flag :simple-array)  ;(:pointer GLboolean)
  )


(def-foreign-function (glScissor
                       (:name "glScissor")
                       (:return-type :null))
    (x      GLint)
    (y      GLint)
    (width  GLsizei)
    (height  GLsizei))

(def-foreign-function (glClipPlane
                       (:name "glClipPlane")
                       (:return-type :null))
    (plane    GLenum)
    (equation :simple-array)  ;(:pointer GLdouble)
    )


(def-foreign-function (glGetClipPlane
                       (:name "glGetClipPlane")
                       (:return-type :null))
    (plane    GLenum)
    (equation :simple-array)			;(:pointer GLdouble)
    )


(def-foreign-function (glDrawBuffer
                       (:name "glDrawBuffer")
                       (:return-type :null))
    (mode  GLenum))


(def-foreign-function (glReadBuffer
                       (:name "glReadBuffer")
                       (:return-type :null))
    (mode  GLenum))


(def-foreign-function (glEnable
                       (:name "glEnable")
                       (:return-type :null))
    (cap  GLenum))


(def-foreign-function (glDisable
                       (:name "glDisable")
                       (:return-type :null))
    (cap  GLenum))


(def-foreign-function (glIsEnabled-int
                       (:name "glIsEnabled")
                       (:return-type GLbooleanA))
    (cap  GLenum))

(defun glIsEnabled (cap)
  (not (eql 0 (glIsEnabled-int cap))))


(def-foreign-function (glGetBooleanv
                       (:name "glGetBooleanv")
                       (:return-type :null))
    (pname  GLenum)
    (params :simple-array) ;params  ;(:pointer GLboolean)
    
    )


(def-foreign-function (glGetDoublev
                       (:name "glGetDoublev")
                       (:return-type :null))
    (pname  GLenum)
    (params :simple-array) ;params  ;(:pointer GLdouble)
    
    )


(def-foreign-function (glGetFloatv
                       (:name "glGetFloatv")
                       (:return-type :null))
    (pname  GLenum)
    (params :simple-array)			;params  ;(:pointer GLfloat)
    )


(def-foreign-function (glGetIntegerv
                       (:name "glGetIntegerv")
                       (:return-type :null))
    (pname  GLenum)
    (params :simple-array)			;params  ;(:pointer GLint)
    
    )


(def-foreign-function (glPushAttrib
                       (:name "glPushAttrib")
                       (:return-type :null))
    (mask  GLbitfield))


(def-foreign-function (glPopAttrib
                       (:name "glPopAttrib")
                       (:return-type :null))
    )

(def-foreign-function (glPushClientAttrib
                       (:name "glPushClientAttrib")
                       (:return-type :null))
    (mask  GLbitfield))


(def-foreign-function (glPopClientAttrib
                       (:name "glPopClientAttrib")
                       (:return-type :null))
    )


(def-foreign-function (glRenderMode
                       (:name "glRenderMode")
                       (:return-type GLint))
    (mode  GLenum))


(def-foreign-function (glGetError
                       (:name "glGetError")
                       (:return-type GLenum))
    )


(def-foreign-function (glGetString
                       (:name "glGetString")
                       ;;(:return-type (:pointer GLubyteA))
		       (:return-type :simple-string)
		       )
    (name  GLenum))


(def-foreign-function (glFinish
                       (:name "glFinish")
                       (:return-type :null))
    )


(def-foreign-function (glFlush
                       (:name "glFlush")
                       (:return-type :null))
    )


(def-foreign-function (glHint
                       (:name "glHint")
                       (:return-type :null))
    (target  GLenum)
    (mode   GLenum))


(def-foreign-function (glClearDepth
                       (:name "glClearDepth")
                       (:return-type :null))
    (depth  GLclampd))


(def-foreign-function (glDepthFunc
                       (:name "glDepthFunc")
                       (:return-type :null))
    (func  GLenum))


(def-foreign-function (glDepthMask
                       (:name "glDepthMask")
                       (:return-type :null))
    (flag  GLbooleanA))


(def-foreign-function (glDepthRange
                       (:name "glDepthRange")
                       (:return-type :null))
    (near_val  GLclampd)
    (far_val  GLclampd))


;;;(def-foreign-function (glClearAccum
;;;                       (:name "glClearAccum")
;;;                       (:return-type :null))
;;;    (red   GLfloat)
;;;    (green  GLfloat)
;;;    (blue  GLfloat)
;;;    (alpha  GLfloat))

(def-foreign-function (glClearAccum
                       (:name (freedius-prefix "glClearAccum"))
                       (:return-type :null))
    (red   GLdouble)
    (green  GLdouble)
    (blue  GLdouble)
    (alpha  GLdouble))


;;;(def-foreign-function (glAccum
;;;                       (:name "glAccum")
;;;                       (:return-type :null))
;;;    (op    GLenum)
;;;    (value  GLfloat))


(def-foreign-function (glMatrixMode
                       (:name "glMatrixMode")
                       (:return-type :null))
    (mode  GLenum))


(def-foreign-function (glOrtho
                       (:name "glOrtho")
                       (:return-type :null))
    (left     GLdouble)
    (right    GLdouble)
    (bottom   GLdouble)
    (top      GLdouble)
    (near_val  GLdouble)
    (far_val  GLdouble))


(def-foreign-function (glFrustum
                       (:name "glFrustum")
                       (:return-type :null))
    (left     GLdouble)
    (right    GLdouble)
    (bottom   GLdouble)
    (top      GLdouble)
    (near_val  GLdouble)
    (far_val  GLdouble))


(def-foreign-function (glViewport
                       (:name "glViewport")
                       (:return-type :null))
    (x      GLint)
    (y      GLint)
    (width  GLsizei)
    (height  GLsizei))


(def-foreign-function (glPushMatrix
                       (:name "glPushMatrix")
                       (:return-type :null))
    )


(def-foreign-function (glPopMatrix
                       (:name "glPopMatrix")
                       (:return-type :null))
    ;;(ARG_G21  :null)
    )


(def-foreign-function (glLoadIdentity
                       (:name "glLoadIdentity")
                       (:return-type :null))
    ;;(ARG_G22  :null)
    )


(def-foreign-function (glLoadMatrixd
                       (:name "glLoadMatrixd")
                       (:return-type :null))
    (m GLmatrixd) ;(:pointer GLdouble)
  )


(def-foreign-function (glLoadMatrixf
                       (:name "glLoadMatrixf")
                       (:return-type :null))
    (m GLmatrixf)			;(:pointer GLfloat)
  )


(def-foreign-function (glMultMatrixd
                       (:name "glMultMatrixd")
                       (:return-type :null))
    (m GLmatrixd)			;(:pointer GLdouble)
  )


(def-foreign-function (glMultMatrixf
                       (:name "glMultMatrixf")
                       (:return-type :null))
    (m GLmatrixd)			;(:pointer GLfloat)
  )


(def-foreign-function (glRotated
                       (:name "glRotated")
                       (:return-type :null))
    (angle  GLdouble)
    (x     GLdouble)
    (y     GLdouble)
    (z     GLdouble))


;;;(def-foreign-function (glRotatef
;;;                       (:name "glRotatef")
;;;                       (:return-type :null))
;;;    (angle  GLfloat)
;;;    (x     GLfloat)
;;;    (y     GLfloat)
;;;    (z     GLfloat))


(def-foreign-function (glScaled
                       (:name "glScaled")
                       (:return-type :null))
    (x  GLdouble)
    (y  GLdouble)
    (z  GLdouble))


;;;(def-foreign-function (glScalef
;;;                       (:name "glScalef")
;;;                       (:return-type :null))
;;;    (x  GLfloat)
;;;    (y  GLfloat)
;;;    (z  GLfloat))


(def-foreign-function (glTranslated
                       (:name "glTranslated")
                       (:return-type :null))
    (x  GLdouble)
    (y  GLdouble)
    (z  GLdouble))


;;;(def-foreign-function (glTranslatef
;;;                       (:name "glTranslatef")
;;;                       (:return-type :null))
;;;    (x  GLfloat)
;;;    (y  GLfloat)
;;;    (z  GLfloat))

;;;
;;; Should this be for windows, or for everybody?

(def-foreign-function (glIsList-internal
                       (:name "glIsList")
                       (:return-type GLbooleanA))
    (list  GLuint))

(defun glIsList (list-number)
  (logand (glIsList-internal list-number) #xFF))


(def-foreign-function (glDeleteLists
                       (:name "glDeleteLists")
                       (:return-type :null))
    (list  GLuint)
    (range  GLsizei))


(def-foreign-function (glGenLists
                       (:name "glGenLists")
                       (:return-type GLuint))
    (range  GLsizei))


(def-foreign-function (glNewList
                       (:name "glNewList")
                       (:return-type :null))
    (list  GLuint)
    (mode  GLenum))


(def-foreign-function (glEndList
                       (:name "glEndList")
                       (:return-type :null))
    ;;(ARG_G23  :null)
    )


(def-foreign-function (glCallList
                       (:name "glCallList")
                       (:return-type :null))
    (list  GLuint))


(def-foreign-function (glCallLists
                       (:name "glCallLists")
                       (:return-type :null))
    (n     GLsizei)
    (type  GLenum)
    (lists :simple-array)  ;(:pointer GLvoid)
    )


(def-foreign-function (glListBase
                       (:name "glListBase")
                       (:return-type :null))
    (base  GLuint))


(def-foreign-function (glBegin
                       (:name "glBegin")
                       (:return-type :null))
    (mode  GLenum))


(def-foreign-function (glEnd
                       (:name "glEnd")
                       (:return-type :null))
    ;;(ARG_G24  :null)
    )


(def-foreign-function (glVertex2d
                       (:name "glVertex2d")
                       (:return-type :null))
    (x  GLdouble)
    (y  GLdouble))


;;;(def-foreign-function (glVertex2f
;;;                       (:name "glVertex2f")
;;;                       (:return-type :null))
;;;    (x  GLfloat)
;;;    (y  GLfloat))


(def-foreign-function (glVertex2i
                       (:name "glVertex2i")
                       (:return-type :null))
    (x  GLint)
    (y  GLint))


;;;(def-foreign-function (glVertex2s
;;;                       (:name "glVertex2s")
;;;                       (:return-type :null))
;;;    (x  GLshortA)
;;;    (y  GLshortA))


(def-foreign-function (glVertex3d
                       (:name "glVertex3d")
                       (:return-type :null))
    (x  GLdouble)
    (y  GLdouble)
    (z  GLdouble))


;;;(def-foreign-function (glVertex3f
;;;                       (:name "glVertex3f")
;;;                       (:return-type :null))
;;;    (x  GLfloat)
;;;    (y  GLfloat)
;;;    (z  GLfloat))


(def-foreign-function (glVertex3i
                       (:name "glVertex3i")
                       (:return-type :null))
    (x  GLint)
    (y  GLint)
    (z  GLint))


;;;(def-foreign-function (glVertex3s
;;;                       (:name "glVertex3s")
;;;                       (:return-type :null))
;;;    (x  GLshortA)
;;;    (y  GLshortA)
;;;    (z  GLshortA))


(def-foreign-function (glVertex4d
                       (:name "glVertex4d")
                       (:return-type :null))
    (x  GLdouble)
    (y  GLdouble)
    (z  GLdouble)
    (w  GLdouble))


;;;(def-foreign-function (glVertex4f
;;;                       (:name "glVertex4f")
;;;                       (:return-type :null))
;;;    (x  GLfloat)
;;;    (y  GLfloat)
;;;    (z  GLfloat)
;;;    (w  GLfloat))


(def-foreign-function (glVertex4i
                       (:name "glVertex4i")
                       (:return-type :null))
    (x  GLint)
    (y  GLint)
    (z  GLint)
    (w  GLint))


;;;(def-foreign-function (glVertex4s
;;;                       (:name "glVertex4s")
;;;                       (:return-type :null))
;;;    (x  GLshortA)
;;;    (y  GLshortA)
;;;    (z  GLshortA)
;;;    (w  GLshortA))


(def-foreign-function (glVertex2dv
                       (:name "glVertex2dv")
                       (:return-type :null))
    (v GLvectord)  ;(:pointer GLdouble)
  )


(def-foreign-function (glVertex2fv
                       (:name "glVertex2fv")
                       (:return-type :null))
    (v GLvectorf)			;(:pointer GLfloat)
  )


(def-foreign-function (glVertex2iv
                       (:name "glVertex2iv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLint)
  )


(def-foreign-function (glVertex2sv
                       (:name "glVertex2sv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLshort)
  )


(def-foreign-function (glVertex3dv
                       (:name "glVertex3dv")
                       (:return-type :null))
    (v GLvectord)			;(:pointer GLdouble)
  )


(def-foreign-function (glVertex3fv
                       (:name "glVertex3fv")
                       (:return-type :null))
    (v GLvectorf)			;(:pointer GLfloat)
  )


(def-foreign-function (glVertex3iv
                       (:name "glVertex3iv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLint)
  )


(def-foreign-function (glVertex3sv
                       (:name "glVertex3sv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLshort)
  )


(def-foreign-function (glVertex4dv
                       (:name "glVertex4dv")
                       (:return-type :null))
    (v GLvectord)			;(:pointer GLdouble)
  )


(def-foreign-function (glVertex4fv
                       (:name "glVertex4fv")
                       (:return-type :null))
    (v GLvectorf)			;(:pointer GLfloat)
  )


(def-foreign-function (glVertex4iv
                       (:name "glVertex4iv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLint)
  )


(def-foreign-function (glVertex4sv
                       (:name "glVertex4sv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLshort)
  )


(def-foreign-function (glNormal3b
                       (:name "glNormal3b")
                       (:return-type :null))
    (nx  GLbyteA)
    (ny  GLbyteA)
    (nz  GLbyteA))


(def-foreign-function (glNormal3d
                       (:name "glNormal3d")
                       (:return-type :null))
    (nx  GLdouble)
    (ny  GLdouble)
    (nz  GLdouble))


;;;(def-foreign-function (glNormal3f
;;;                       (:name "glNormal3f")
;;;                       (:return-type :null))
;;;    (nx  GLfloat)
;;;    (ny  GLfloat)
;;;    (nz  GLfloat))


(def-foreign-function (glNormal3i
                       (:name "glNormal3i")
                       (:return-type :null))
    (nx  GLint)
    (ny  GLint)
    (nz  GLint))


;;;(def-foreign-function (glNormal3s
;;;                       (:name "glNormal3s")
;;;                       (:return-type :null))
;;;    (nx  GLshortA)
;;;    (ny  GLshortA)
;;;    (nz  GLshortA))


(def-foreign-function (glNormal3bv
                       (:name "glNormal3bv")
                       (:return-type :null))
    (v :simple-array)  ;(:pointer GLbyte)
  )


(def-foreign-function (glNormal3dv
                       (:name "glNormal3dv")
                       (:return-type :null))
    (v GLvectord)			;(:pointer GLdouble)
  )


(def-foreign-function (glNormal3fv
                       (:name "glNormal3fv")
                       (:return-type :null))
    (v GLvectorf)			;(:pointer GLfloat)
  )


(def-foreign-function (glNormal3iv
                       (:name "glNormal3iv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLint)
  )


(def-foreign-function (glNormal3sv
                       (:name "glNormal3sv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLshort)
  )


(def-foreign-function (glIndexd
                       (:name "glIndexd")
                       (:return-type :null))
    (c  GLdouble))


;;;(def-foreign-function (glIndexf
;;;                       (:name "glIndexf")
;;;                       (:return-type :null))
;;;    (c  GLfloat))


(def-foreign-function (glIndexi
                       (:name "glIndexi")
                       (:return-type :null))
    (c  GLint))


(def-foreign-function (glIndexs
                       (:name "glIndexs")
                       (:return-type :null))
    (c  GLshortA))


(def-foreign-function (glIndexdv
                       (:name "glIndexdv")
                       (:return-type :null))
    (c :simple-array) ;(:pointer GLdouble)
  )


(def-foreign-function (glIndexfv
                       (:name "glIndexfv")
                       (:return-type :null))
    (c :simple-array)				;(:pointer GLfloat)
  )


(def-foreign-function (glIndexiv
                       (:name "glIndexiv")
                       (:return-type :null))
    (c :simple-array)				;(:pointer GLint)
  )


(def-foreign-function (glIndexsv
                       (:name "glIndexsv")
                       (:return-type :null))
    (c :simple-array)				;(:pointer GLshort)
  )


(def-foreign-function (glColor3b
                       (:name "glColor3b")
                       (:return-type :null))
    (red   GLbyteA)
    (green  GLbyteA)
    (blue  GLbyteA))


(def-foreign-function (glColor3d
                       (:name "glColor3d")
                       (:return-type :null))
    (red   GLdouble)
    (green  GLdouble)
    (blue  GLdouble))


(def-foreign-function (glColor3f
                       (:name "glColor3f")
                       (:return-type :null))
    (red   GLfloat)
  (green  GLfloat)
  (blue  GLfloat))


(def-foreign-function (glColor3i
                       (:name "glColor3i")
                       (:return-type :null))
    (red   GLint)
    (green  GLint)
    (blue  GLint))


;;;(def-foreign-function (glColor3s
;;;                       (:name "glColor3s")
;;;                       (:return-type :null))
;;;    (red   GLshortA)
;;;    (green  GLshortA)
;;;    (blue  GLshortA))


;;;(def-foreign-function (glColor3ub
;;;                       (:name "glColor3ub")
;;;                       (:return-type :null))
;;;    (red   GLubyteA)
;;;    (green  GLubyteA)
;;;    (blue  GLubyteA))


;;;(def-foreign-function (glColor3ui
;;;                       (:name "glColor3ui")
;;;                       (:return-type :null))
;;;    (red   GLuint)
;;;    (green  GLuint)
;;;    (blue  GLuint))


;;;(def-foreign-function (glColor3us
;;;                       (:name "glColor3us")
;;;                       (:return-type :null))
;;;    (red   GLushortA)
;;;    (green  GLushortA)
;;;    (blue  GLushortA))


;;;(def-foreign-function (glColor4b
;;;                       (:name "glColor4b")
;;;                       (:return-type :null))
;;;    (red   GLbyteA)
;;;    (green  GLbyteA)
;;;    (blue  GLbyteA)
;;;    (alpha  GLbyteA))


(def-foreign-function (glColor4d
                       (:name "glColor4d")
                       (:return-type :null))
    (red   GLdouble)
    (green  GLdouble)
    (blue  GLdouble)
    (alpha  GLdouble))

(def-foreign-function (glColor4f
                       (:name "glColor4f")
                       (:return-type :null))
    (red   GLfloat)
    (green  GLfloat)
    (blue  GLfloat)
    (alpha  GLfloat))


(def-foreign-function (glColor4i
                       (:name "glColor4i")
                       (:return-type :null))
    (red   GLint)
    (green  GLint)
    (blue  GLint)
    (alpha  GLint))


;;;(def-foreign-function (glColor4s
;;;                       (:name "glColor4s")
;;;                       (:return-type :null))
;;;    (red   GLshortA)
;;;    (green  GLshortA)
;;;    (blue  GLshortA)
;;;    (alpha  GLshortA))


;;;(def-foreign-function (glColor4ub
;;;                       (:name "glColor4ub")
;;;                       (:return-type :null))
;;;    (red   GLubyteA)
;;;    (green  GLubyteA)
;;;    (blue  GLubyteA)
;;;    (alpha  GLubyteA))


;;;(def-foreign-function (glColor4ui
;;;                       (:name "glColor4ui")
;;;                       (:return-type :null))
;;;    (red   GLuint)
;;;    (green  GLuint)
;;;    (blue  GLuint)
;;;    (alpha  GLuint))


;;;(def-foreign-function (glColor4us
;;;                       (:name "glColor4us")
;;;                       (:return-type :null))
;;;    (red   GLushortA)
;;;    (green  GLushortA)
;;;    (blue  GLushortA)
;;;    (alpha  GLushortA))


(def-foreign-function (glColor3bv
                       (:name "glColor3bv")
                       (:return-type :null))
    (v :simple-array) ;(:pointer GLbyte)
  )


(def-foreign-function (glColor3dv
                       (:name "glColor3dv")
                       (:return-type :null))
    (v GLvectord)
  )


(def-foreign-function (glColor3fv
                       (:name "glColor3fv")
                       (:return-type :null))
   ; v  ;(:pointer GLfloat)
  (v GLvectorf)
  )


(def-foreign-function (glColor3iv
                       (:name "glColor3iv")
                       (:return-type :null))
    (v :simple-array)  ;(:pointer GLint)
  )


(def-foreign-function (glColor3sv
                       (:name "glColor3sv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLshort)
  )


(def-foreign-function (glColor3ubv
                       (:name "glColor3ubv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLubyte)
  )


(def-foreign-function (glColor3uiv
                       (:name "glColor3uiv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLuint)
  )


(def-foreign-function (glColor3usv
                       (:name "glColor3usv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLushort)
  )


(def-foreign-function (glColor4bv
                       (:name "glColor4bv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLbyte)
  )


(def-foreign-function (glColor4dv
                       (:name "glColor4dv")
                       (:return-type :null))
    (v GLvectord)
  )


(def-foreign-function (glColor4fv
                       (:name "glColor4fv")
                       (:return-type :null))
  (v GLvectorf)
  )


(def-foreign-function (glColor4iv
                       (:name "glColor4iv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLint)
  )


(def-foreign-function (glColor4sv
                       (:name "glColor4sv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLshort)
  )


(def-foreign-function (glColor4ubv
                       (:name "glColor4ubv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLubyte)
  )


(def-foreign-function (glColor4uiv
                       (:name "glColor4uiv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLuint)
  )


(def-foreign-function (glColor4usv
                       (:name "glColor4usv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLushort)
  )


(def-foreign-function (glTexCoord1d
                       (:name "glTexCoord1d")
                       (:return-type :null))
    (s  GLdouble))


;;;(def-foreign-function (glTexCoord1f
;;;                       (:name "glTexCoord1f")
;;;                       (:return-type :null))
;;;    (s  GLfloat))


(def-foreign-function (glTexCoord1i
                       (:name "glTexCoord1i")
                       (:return-type :null))
    (s  GLint))


;;;(def-foreign-function (glTexCoord1s
;;;                       (:name "glTexCoord1s")
;;;                       (:return-type :null))
;;;    (s  GLshortA))


(def-foreign-function (glTexCoord2d
                       (:name "glTexCoord2d")
                       (:return-type :null))
    (s  GLdouble)
    (tt  GLdouble))


;;;(def-foreign-function (glTexCoord2f
;;;                       (:name "glTexCoord2f")
;;;                       (:return-type :null))
;;;    (s  GLfloat)
;;;    (tt  GLfloat))


(def-foreign-function (glTexCoord2i
                       (:name "glTexCoord2i")
                       (:return-type :null))
    (s  GLint)
    (tt  GLint))


;;;(def-foreign-function (glTexCoord2s
;;;                       (:name "glTexCoord2s")
;;;                       (:return-type :null))
;;;    (s  GLshortA)
;;;    (tt  GLshortA))


(def-foreign-function (glTexCoord3d
                       (:name "glTexCoord3d")
                       (:return-type :null))
    (s  GLdouble)
    (tt  GLdouble)
    (r  GLdouble))


;;;(def-foreign-function (glTexCoord3f
;;;                       (:name "glTexCoord3f")
;;;                       (:return-type :null))
;;;    (s  GLfloat)
;;;    (tt  GLfloat)
;;;    (r  GLfloat))


(def-foreign-function (glTexCoord3i
                       (:name "glTexCoord3i")
                       (:return-type :null))
    (s  GLint)
    (tt  GLint)
    (r  GLint))


;;;(def-foreign-function (glTexCoord3s
;;;                       (:name "glTexCoord3s")
;;;                       (:return-type :null))
;;;    (s  GLshortA)
;;;    (tt  GLshortA)
;;;    (r  GLshortA))


(def-foreign-function (glTexCoord4d
                       (:name "glTexCoord4d")
                       (:return-type :null))
    (s  GLdouble)
    (tt  GLdouble)
    (r  GLdouble)
    (q  GLdouble))


;;;(def-foreign-function (glTexCoord4f
;;;                       (:name "glTexCoord4f")
;;;                       (:return-type :null))
;;;    (s  GLfloat)
;;;    (tt  GLfloat)
;;;    (r  GLfloat)
;;;    (q  GLfloat))


(def-foreign-function (glTexCoord4i
                       (:name "glTexCoord4i")
                       (:return-type :null))
    (s  GLint)
    (tt  GLint)
    (r  GLint)
    (q  GLint))


;;;(def-foreign-function (glTexCoord4s
;;;                       (:name "glTexCoord4s")
;;;                       (:return-type :null))
;;;    (s  GLshortA)
;;;    (tt  GLshortA)
;;;    (r  GLshortA)
;;;    (q  GLshortA))


(def-foreign-function (glTexCoord1dv
                       (:name "glTexCoord1dv")
                       (:return-type :null))
    (v GLvectord) 
  )


(def-foreign-function (glTexCoord1fv
                       (:name "glTexCoord1fv")
                       (:return-type :null))
    (v GLvectorf)			;(:pointer GLfloat)
  )


(def-foreign-function (glTexCoord1iv
                       (:name "glTexCoord1iv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLint)
  )


(def-foreign-function (glTexCoord1sv
                       (:name "glTexCoord1sv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLshort)
  )


(def-foreign-function (glTexCoord2dv
                       (:name "glTexCoord2dv")
                       (:return-type :null))
    (v GLvectord)			;(:pointer GLdouble)
  )


(def-foreign-function (glTexCoord2fv
                       (:name "glTexCoord2fv")
                       (:return-type :null))
    (v GLvectorf)			;(:pointer GLfloat)
  )


(def-foreign-function (glTexCoord2iv
                       (:name "glTexCoord2iv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLint)
  )


(def-foreign-function (glTexCoord2sv
                       (:name "glTexCoord2sv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLshort)
  )


(def-foreign-function (glTexCoord3dv
                       (:name "glTexCoord3dv")
                       (:return-type :null))
    (v GLvectord)				;(:pointer GLdouble)
  )


(def-foreign-function (glTexCoord3fv
                       (:name "glTexCoord3fv")
                       (:return-type :null))
    (v GLvectorf)			;(:pointer GLfloat)
  )


(def-foreign-function (glTexCoord3iv
                       (:name "glTexCoord3iv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLint)
  )


(def-foreign-function (glTexCoord3sv
                       (:name "glTexCoord3sv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLshort)
  )


(def-foreign-function (glTexCoord4dv
                       (:name "glTexCoord4dv")
                       (:return-type :null))
    (v GLvectord)			;(:pointer GLdouble)
  )


(def-foreign-function (glTexCoord4fv
                       (:name "glTexCoord4fv")
                       (:return-type :null))
    (v GLvectorf)			;(:pointer GLfloat)
  )


(def-foreign-function (glTexCoord4iv
                       (:name "glTexCoord4iv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLint)
  )


(def-foreign-function (glTexCoord4sv
                       (:name "glTexCoord4sv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLshort)
  )


(def-foreign-function (glRasterPos2d
                       (:name "glRasterPos2d")
                       (:return-type :null))
    (x  GLdouble)
    (y  GLdouble))


;;;(def-foreign-function (glRasterPos2f
;;;                       (:name "glRasterPos2f")
;;;                       (:return-type :null))
;;;    (x  GLfloat)
;;;    (y  GLfloat))


(def-foreign-function (glRasterPos2i
                       (:name "glRasterPos2i")
                       (:return-type :null))
    (x  GLint)
    (y  GLint))


;;;(def-foreign-function (glRasterPos2s
;;;                       (:name "glRasterPos2s")
;;;                       (:return-type :null))
;;;    (x  GLshortA)
;;;    (y  GLshortA))


(def-foreign-function (glRasterPos3d
                       (:name "glRasterPos3d")
                       (:return-type :null))
    (x  GLdouble)
    (y  GLdouble)
    (z  GLdouble))


;;;(def-foreign-function (glRasterPos3f
;;;                       (:name "glRasterPos3f")
;;;                       (:return-type :null))
;;;    (x  GLfloat)
;;;    (y  GLfloat)
;;;    (z  GLfloat))


(def-foreign-function (glRasterPos3i
                       (:name "glRasterPos3i")
                       (:return-type :null))
    (x  GLint)
    (y  GLint)
    (z  GLint))


;;;(def-foreign-function (glRasterPos3s
;;;                       (:name "glRasterPos3s")
;;;                       (:return-type :null))
;;;    (x  GLshortA)
;;;    (y  GLshortA)
;;;    (z  GLshortA))


(def-foreign-function (glRasterPos4d
                       (:name "glRasterPos4d")
                       (:return-type :null))
    (x  GLdouble)
    (y  GLdouble)
    (z  GLdouble)
    (w  GLdouble))


;;;(def-foreign-function (glRasterPos4f
;;;                       (:name "glRasterPos4f")
;;;                       (:return-type :null))
;;;    (x  GLfloat)
;;;    (y  GLfloat)
;;;    (z  GLfloat)
;;;    (w  GLfloat))


(def-foreign-function (glRasterPos4i
                       (:name "glRasterPos4i")
                       (:return-type :null))
    (x  GLint)
    (y  GLint)
    (z  GLint)
    (w  GLint))


;;;(def-foreign-function (glRasterPos4s
;;;                       (:name "glRasterPos4s")
;;;                       (:return-type :null))
;;;    (x  GLshortA)
;;;    (y  GLshortA)
;;;    (z  GLshortA)
;;;    (w  GLshortA))


(def-foreign-function (glRasterPos2dv
                       (:name "glRasterPos2dv")
                       (:return-type :null))
    (v GLvectord)  ;(:pointer GLdouble)
  )


(def-foreign-function (glRasterPos2fv
                       (:name "glRasterPos2fv")
                       (:return-type :null))
    (v GLvectorf)			;(:pointer GLfloat)
  )


(def-foreign-function (glRasterPos2iv
                       (:name "glRasterPos2iv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLint)
  )


(def-foreign-function (glRasterPos2sv
                       (:name "glRasterPos2sv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLshort)
  )


(def-foreign-function (glRasterPos3dv
                       (:name "glRasterPos3dv")
                       (:return-type :null))
    (v GLvectord)			;(:pointer GLdouble)
  )


(def-foreign-function (glRasterPos3fv
                       (:name "glRasterPos3fv")
                       (:return-type :null))
    (v GLvectorf)			;(:pointer GLfloat)
  )


(def-foreign-function (glRasterPos3iv
                       (:name "glRasterPos3iv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLint)
  )


(def-foreign-function (glRasterPos3sv
                       (:name "glRasterPos3sv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLshort)
  )


(def-foreign-function (glRasterPos4dv
                       (:name "glRasterPos4dv")
                       (:return-type :null))
    (v GLvectord)			;(:pointer GLdouble)
  )


(def-foreign-function (glRasterPos4fv
                       (:name "glRasterPos4fv")
                       (:return-type :null))
    (v GLvectorf)			;(:pointer GLfloat)
  )


(def-foreign-function (glRasterPos4iv
                       (:name "glRasterPos4iv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLint)
  )


(def-foreign-function (glRasterPos4sv
                       (:name "glRasterPos4sv")
                       (:return-type :null))
    (v :simple-array)				;(:pointer GLshort)
  )


(def-foreign-function (glRectd
                       (:name "glRectd")
                       (:return-type :null))
    (x1  GLdouble)
    (y1  GLdouble)
    (x2  GLdouble)
    (y2  GLdouble))


;;;(def-foreign-function (glRectf
;;;                       (:name "glRectf")
;;;                       (:return-type :null))
;;;    (x1  GLfloat)
;;;    (y1  GLfloat)
;;;    (x2  GLfloat)
;;;    (y2  GLfloat))


(def-foreign-function (glRecti
                       (:name "glRecti")
                       (:return-type :null))
    (x1  GLint)
    (y1  GLint)
    (x2  GLint)
    (y2  GLint))


;;;(def-foreign-function (glRects
;;;                       (:name "glRects")
;;;                       (:return-type :null))
;;;    (x1  GLshortA)
;;;    (y1  GLshortA)
;;;    (x2  GLshortA)
;;;    (y2  GLshortA))


(def-foreign-function (glRectdv
                       (:name "glRectdv")
                       (:return-type :null))
    (v1 :simple-array)  ;(:pointer GLdouble)
  
    (v2 :simple-array)				;(:pointer GLdouble)
    )


(def-foreign-function (glRectfv
                       (:name "glRectfv")
                       (:return-type :null))
    (v1 :simple-array)				;(:pointer GLfloat)
  
    (v2 :simple-array)				;(:pointer GLfloat)
    )


(def-foreign-function (glRectiv
                       (:name "glRectiv")
                       (:return-type :null))
    (v1 :simple-array)				;(:pointer GLint)
  
    (v2 :simple-array)				;(:pointer GLint)
    )


(def-foreign-function (glRectsv
                       (:name "glRectsv")
                       (:return-type :null))
    (v1 :simple-array)				;(:pointer GLshort)
  
    (v2 :simple-array)				;(:pointer GLshort)
    )


(def-foreign-function (glShadeModel
                       (:name "glShadeModel")
                       (:return-type :null))
    (mode  GLenum))


;;;(def-foreign-function (glLightf
;;;                       (:name "glLightf")
;;;                       (:return-type :null))
;;;    (light  GLenum)
;;;    (pname  GLenum)
;;;    (param  GLfloat))


(def-foreign-function (glLighti
                       (:name "glLighti")
                       (:return-type :null))
    (light  GLenum)
    (pname  GLenum)
    (param  GLint))


(def-foreign-function (glLightfv
                       (:name "glLightfv")
                       (:return-type :null))
    (light  GLenum)
    (pname  GLenum)
    (params :simple-array)  ;(:pointer GLfloat)
    )


(def-foreign-function (glLightiv
                       (:name "glLightiv")
                       (:return-type :null))
    (light  GLenum)
    (pname  GLenum)
    (params :simple-array)  ;(:pointer GLint)
    )


(def-foreign-function (glGetLightfv
                       (:name "glGetLightfv")
                       (:return-type :null))
    (light  GLenum)
    (pname  GLenum)
    (params :simple-array)  ;(:pointer GLfloat)
    )


(def-foreign-function (glGetLightiv
                       (:name "glGetLightiv")
                       (:return-type :null))
    (light  GLenum)
    (pname  GLenum)
    (params :simple-array)  ;(:pointer GLint)
    )


;;;(def-foreign-function (glLightModelf
;;;                       (:name "glLightModelf")
;;;                       (:return-type :null))
;;;    (pname  GLenum)
;;;    (param  GLfloat))


(def-foreign-function (glLightModeli
                       (:name "glLightModeli")
                       (:return-type :null))
    (pname  GLenum)
    (param  GLint))


(def-foreign-function (glLightModelfv
                       (:name "glLightModelfv")
                       (:return-type :null))
    (pname  GLenum)
    (params :simple-array)  ;(:pointer GLfloat)
    )


(def-foreign-function (glLightModeliv
                       (:name "glLightModeliv")
                       (:return-type :null))
    (pname  GLenum)
    (params :simple-array)  ;(:pointer GLint)
    )


;;;(def-foreign-function (glMaterialf
;;;                       (:name "glMaterialf")
;;;                       (:return-type :null))
;;;    (face  GLenum)
;;;    (pname  GLenum)
;;;    (param  GLfloat))


(def-foreign-function (glMateriali
                       (:name "glMateriali")
                       (:return-type :null))
    (face  GLenum)
    (pname  GLenum)
    (param  GLint))


(def-foreign-function (glMaterialfv
                       (:name "glMaterialfv")
                       (:return-type :null))
    (face   GLenum)
    (pname  GLenum)
    (params :simple-array)  ;(:pointer GLfloat)
    )


(def-foreign-function (glMaterialiv
                       (:name "glMaterialiv")
                       (:return-type :null))
    (face   GLenum)
    (pname  GLenum)
    (params :simple-array)			;(:pointer GLint)
    )


(def-foreign-function (glGetMaterialfv
                       (:name "glGetMaterialfv")
                       (:return-type :null))
    (face   GLenum)
    (pname  GLenum)
    (params :simple-array)			;(:pointer GLfloat)
    )


(def-foreign-function (glGetMaterialiv
                       (:name "glGetMaterialiv")
                       (:return-type :null))
    (face   GLenum)
    (pname  GLenum)
    (params :simple-array)			;(:pointer GLint)
    )


(def-foreign-function (glColorMaterial
                       (:name "glColorMaterial")
                       (:return-type :null))
    (face  GLenum)
    (mode  GLenum))


(def-foreign-function (glPixelZoom-int
                       (:name (freedius-prefix "glPixelZoom"))
                       (:return-type :null))
    (xfactor  GLDouble)
    (yfactor  GLDouble)
    )

(defun glPixelZoom (xfactor yfactor)
  (glPixelZoom-int (float xfactor 1.0d0) (float yfactor 1.0d0)))



;;;(def-foreign-function (glPixelStoref
;;;                       (:name "glPixelStoref")
;;;                       (:return-type :null))
;;;    (pname  GLenum)
;;;    (param  GLfloat))


(def-foreign-function (glPixelStorei
                       (:name "glPixelStorei")
                       (:return-type :null))
    (pname  GLenum)
    (param  GLint))


;;;(def-foreign-function (glPixelTransferf
;;;                       (:name "glPixelTransferf")
;;;                       (:return-type :null))
;;;    (pname  GLenum)
;;;    (param  GLfloat))


(def-foreign-function (glPixelTransferi
                       (:name "glPixelTransferi")
                       (:return-type :null))
    (pname  GLenum)
    (param  GLint))


(def-foreign-function (glPixelMapfv
                       (:name "glPixelMapfv")
                       (:return-type :null))
    (map     GLenum)
    (mapsize  GLint)
    (values :simple-array)  ;(:pointer GLfloat)
    )


(def-foreign-function (glPixelMapuiv
                       (:name "glPixelMapuiv")
                       (:return-type :null))
    (map     GLenum)
    (mapsize  GLint)
    (values :simple-array)			;(:pointer GLuint)
    )


(def-foreign-function (glPixelMapusv
                       (:name "glPixelMapusv")
                       (:return-type :null))
    (map     GLenum)
    (mapsize  GLint)
    (values :simple-array)			;(:pointer GLushort)
    )


(def-foreign-function (glGetPixelMapfv
                       (:name "glGetPixelMapfv")
                       (:return-type :null))
    (map    GLenum)
    (values :simple-array)			;(:pointer GLfloat)
    )


(def-foreign-function (glGetPixelMapuiv
                       (:name "glGetPixelMapuiv")
                       (:return-type :null))
    (map    GLenum)
    (values :simple-array)			;(:pointer GLuint)
    )


(def-foreign-function (glGetPixelMapusv
                       (:name "glGetPixelMapusv")
                       (:return-type :null))
    (map    GLenum)
    (values :simple-array)			;(:pointer GLushort)
    )


(def-foreign-function (glBitmap
			(:name "glBitmap")
			(:return-type :null))
  (width  GLsizei)
  (height  GLsizei)
  (xorig  GLfloat)
  (yorig  GLfloat)
  (xmove  GLfloat)
  (ymove  GLfloat)
  (bitmap  (:simple-array-or-null :unsigned-8bit))		;(:pointer GLubyte)
  )

(def-foreign-function (glReadPixels
                       (:name "glReadPixels")
                       (:return-type :null))
    (x      GLint)
    (y      GLint)
    (width  GLsizei)
    (height  GLsizei)
    (format  GLenum)
    (type   GLenum)
    (pixels :simple-array)  ;(:pointer GLvoid)
    )


(def-foreign-function (glDrawPixels
                       (:name "glDrawPixels")
                       (:return-type :null))
    (width  GLsizei)
    (height  GLsizei)
    (format  GLenum)
    (type   GLenum)
    (pixels :simple-array)			;(:pointer GLvoid)
    )


(def-foreign-function (glCopyPixels
                       (:name "glCopyPixels")
                       (:return-type :null))
    (x      GLint)
    (y      GLint)
    (width  GLsizei)
    (height  GLsizei)
    (type   GLenum))


(def-foreign-function (glStencilFunc
                       (:name "glStencilFunc")
                       (:return-type :null))
    (func  GLenum)
    (ref  GLint)
    (mask  GLuint))


(def-foreign-function (glStencilMask
                       (:name "glStencilMask")
                       (:return-type :null))
    (mask  GLuint))


(def-foreign-function (glStencilOp
                       (:name "glStencilOp")
                       (:return-type :null))
    (fail  GLenum)
    (zfail  GLenum)
    (zpass  GLenum))


(def-foreign-function (glClearStencil
                       (:name "glClearStencil")
                       (:return-type :null))
    (s  GLint))


(def-foreign-function (glTexGend
                       (:name "glTexGend")
                       (:return-type :null))
    (coord  GLenum)
    (pname  GLenum)
    (param  GLdouble))


;;;(def-foreign-function (glTexGenf
;;;                       (:name "glTexGenf")
;;;                       (:return-type :null))
;;;    (coord  GLenum)
;;;    (pname  GLenum)
;;;    (param  GLfloat))


(def-foreign-function (glTexGeni
                       (:name "glTexGeni")
                       (:return-type :null))
    (coord  GLenum)
    (pname  GLenum)
    (param  GLint))


(def-foreign-function (glTexGendv
                       (:name "glTexGendv")
                       (:return-type :null))
    (coord  GLenum)
    (pname  GLenum)
    (params :simple-array)  ;(:pointer GLdouble)
    )


(def-foreign-function (glTexGenfv
                       (:name "glTexGenfv")
                       (:return-type :null))
    (coord  GLenum)
    (pname  GLenum)
    (params :simple-array)			;(:pointer GLfloat)
    )


(def-foreign-function (glTexGeniv
                       (:name "glTexGeniv")
                       (:return-type :null))
    (coord  GLenum)
    (pname  GLenum)
    (params :simple-array)			;(:pointer GLint)
    )


(def-foreign-function (glGetTexGendv
                       (:name "glGetTexGendv")
                       (:return-type :null))
    (coord  GLenum)
    (pname  GLenum)
    (params :simple-array)			;(:pointer GLdouble)
    )


(def-foreign-function (glGetTexGenfv
                       (:name "glGetTexGenfv")
                       (:return-type :null))
    (coord  GLenum)
    (pname  GLenum)
    (params :simple-array)			;(:pointer GLfloat)
    )


(def-foreign-function (glGetTexGeniv
                       (:name "glGetTexGeniv")
                       (:return-type :null))
    (coord  GLenum)
    (pname  GLenum)
    (params :simple-array)			;(:pointer GLint)
    )


;;;(def-foreign-function (glTexEnvf
;;;                       (:name "glTexEnvf")
;;;                       (:return-type :null))
;;;    (target  GLenum)
;;;    (pname  GLenum)
;;;    (param  GLfloat))


(def-foreign-function (glTexEnvi
                       (:name "glTexEnvi")
                       (:return-type :null))
    (target  GLenum)
    (pname  GLenum)
    (param  GLint))


(def-foreign-function (glTexEnvfv
                       (:name "glTexEnvfv")
                       (:return-type :null))
    (target  GLenum)
    (pname  GLenum)
    (params :simple-array)			;(:pointer GLfloat)
    )


(def-foreign-function (glTexEnviv
                       (:name "glTexEnviv")
                       (:return-type :null))
    (target  GLenum)
    (pname  GLenum)
    (params :simple-array)			;(:pointer GLint)
    )


(def-foreign-function (glGetTexEnvfv
                       (:name "glGetTexEnvfv")
                       (:return-type :null))
    (target  GLenum)
    (pname  GLenum)
    (params :simple-array)			;(:pointer GLfloat)
    )


(def-foreign-function (glGetTexEnviv
                       (:name "glGetTexEnviv")
                       (:return-type :null))
    (target  GLenum)
    (pname  GLenum)
    (params :simple-array)			;(:pointer GLint)
    )


;;;(def-foreign-function (glTexParameterf
;;;                       (:name "glTexParameterf")
;;;                       (:return-type :null))
;;;    (target  GLenum)
;;;    (pname  GLenum)
;;;    (param  GLfloat))


(def-foreign-function (glTexParameteri
                       (:name "glTexParameteri")
                       (:return-type :null))
    (target  GLenum)
    (pname  GLenum)
    (param  GLint))


(def-foreign-function (glTexParameterfv
                       (:name "glTexParameterfv")
                       (:return-type :null))
    (target  GLenum)
    (pname  GLenum)
    (params :simple-array)			;(:pointer GLfloat)
    )


(def-foreign-function (glTexParameteriv
                       (:name "glTexParameteriv")
                       (:return-type :null))
    (target  GLenum)
    (pname  GLenum)
    (params :simple-array)			;(:pointer GLint)
    )


(def-foreign-function (glGetTexParameterfv
                       (:name "glGetTexParameterfv")
                       (:return-type :null))
    (target  GLenum)
    (pname  GLenum)
    (params :simple-array)			;(:pointer GLfloat)
    )


(def-foreign-function (glGetTexParameteriv
                       (:name "glGetTexParameteriv")
                       (:return-type :null))
    (target  GLenum)
    (pname  GLenum)
    (params :simple-array)			;(:pointer GLint)
    )


(def-foreign-function (glGetTexLevelParameterfv
                       (:name "glGetTexLevelParameterfv")
                       (:return-type :null))
    (target  GLenum)
    (level  GLint)
    (pname  GLenum)
    (params :simple-array)			;(:pointer GLfloat)
    )


(def-foreign-function (glGetTexLevelParameteriv
                       (:name "glGetTexLevelParameteriv")
                       (:return-type :null))
    (target  GLenum)
    (level  GLint)
    (pname  GLenum)
    (params :simple-array)			;(:pointer GLint)
    )


(def-foreign-function (glTexImage1D
                       (:name "glTexImage1D")
                       (:return-type :null))
    (target     GLenum)
    (level      GLint)
    (components  GLint)
    (width      GLsizei)
    (border     GLint)
    (format     GLenum)
    (type       GLenum)
    (pixels :simple-array)     ;(:pointer GLvoid)
    )


(def-foreign-function (glTexImage2D
                       (:name "glTexImage2D")
                       (:return-type :null))
    (target     GLenum)
    (level      GLint)
    (internalFormat  GLint)
    (width      GLsizei)
    (height     GLsizei)
    (border     GLint)
    (format     GLenum)
    (type       GLenum)
    (pixels :simple-array-or-null)			;(:pointer GLvoid)
    )

(def-foreign-function (glTexImage2D-non-simple
                       (:name "glTexImage2D")
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

(def-foreign-function (glGetTexImage
                       (:name "glGetTexImage")
                       (:return-type :null))
    (target  GLenum)
    (level  GLint)
    (format  GLenum)
    (type   GLenum)
    (pixels :simple-array)			;(:pointer GLvoid)
    )


(def-foreign-function (glMap1d
                       (:name "glMap1d")
                       (:return-type :null))
    (target  GLenum)
    (u1     GLdouble)
    (u2     GLdouble)
    (stride  GLint)
    (order  GLint)
    (points :simple-array);(:pointer GLdouble)
    )


;;;(def-foreign-function (glMap1f
;;;                       (:name "glMap1f")
;;;                       (:return-type :null))
;;;    (target  GLenum)
;;;    (u1     GLfloat)
;;;    (u2     GLfloat)
;;;    (stride  GLint)
;;;    (order  GLint)
;;;    (points  (:pointer GLfloat)))



(def-foreign-function (glMap2d
                       (:name "glMap2d")
                       (:return-type :null))
    (target  GLenum)
    (u1      GLdouble)
    (u2      GLdouble)
    (ustride  GLint)
    (uorder  GLint)
    (v1      GLdouble)
    (v2      GLdouble)
    (vstride  GLint)
    (vorder  GLint)
    (points :simple-array)			;(:pointer GLdouble)
    )


;;;(def-foreign-function (glMap2f
;;;                       (:name "glMap2f")
;;;                       (:return-type :null))
;;;    (target  GLenum)
;;;    (u1      GLfloat)
;;;    (u2      GLfloat)
;;;    (ustride  GLint)
;;;    (uorder  GLint)
;;;    (v1      GLfloat)
;;;    (v2      GLfloat)
;;;    (vstride  GLint)
;;;    (vorder  GLint)
;;;    (points  (:pointer GLfloat)))


(def-foreign-function (glGetMapdv
                       (:name "glGetMapdv")
                       (:return-type :null))
    (target  GLenum)
    (query  GLenum)
    (v :simple-array)     ;(:pointer GLdouble)
    )


(def-foreign-function (glGetMapfv
                       (:name "glGetMapfv")
                       (:return-type :null))
    (target  GLenum)
    (query  GLenum)
    (v :simple-array)				;(:pointer GLfloat)
    )


(def-foreign-function (glGetMapiv
                       (:name "glGetMapiv")
                       (:return-type :null))
    (target  GLenum)
    (query  GLenum)
    (v :simple-array)				;(:pointer GLint)
    )


(def-foreign-function (glEvalCoord1d
                       (:name "glEvalCoord1d")
                       (:return-type :null))
    (u  GLdouble))


;;;(def-foreign-function (glEvalCoord1f
;;;                       (:name "glEvalCoord1f")
;;;                       (:return-type :null))
;;;    (u  GLfloat))


(def-foreign-function (glEvalCoord1dv
                       (:name "glEvalCoord1dv")
                       (:return-type :null))
    (u :simple-array)  ;(:pointer GLdouble)
  )


(def-foreign-function (glEvalCoord1fv
                       (:name "glEvalCoord1fv")
                       (:return-type :null))
    (u :simple-array)				;(:pointer GLfloat)
  )


(def-foreign-function (glEvalCoord2d
                       (:name "glEvalCoord2d")
                       (:return-type :null))
    (u  GLdouble)
    (v  GLdouble))


;;;(def-foreign-function (glEvalCoord2f
;;;                       (:name "glEvalCoord2f")
;;;                       (:return-type :null))
;;;    (u  GLfloat)
;;;    (v  GLfloat))


(def-foreign-function (glEvalCoord2dv
                       (:name "glEvalCoord2dv")
                       (:return-type :null))
    (u :simple-array)				;(:pointer GLdouble)
  )


(def-foreign-function (glEvalCoord2fv
                       (:name "glEvalCoord2fv")
                       (:return-type :null))
    (u :simple-array)				;(:pointer GLfloat)
  )


(def-foreign-function (glMapGrid1d
                       (:name "glMapGrid1d")
                       (:return-type :null))
    (un  GLint)
    (u1  GLdouble)
    (u2  GLdouble))


;;;(def-foreign-function (glMapGrid1f
;;;                       (:name "glMapGrid1f")
;;;                       (:return-type :null))
;;;    (un  GLint)
;;;    (u1  GLfloat)
;;;    (u2  GLfloat))


(def-foreign-function (glMapGrid2d
                       (:name "glMapGrid2d")
                       (:return-type :null))
    (un  GLint)
    (u1  GLdouble)
    (u2  GLdouble)
    (vn  GLint)
    (v1  GLdouble)
    (v2  GLdouble))


;;;(def-foreign-function (glMapGrid2f
;;;                       (:name "glMapGrid2f")
;;;                       (:return-type :null))
;;;    (un  GLint)
;;;    (u1  GLfloat)
;;;    (u2  GLfloat)
;;;    (vn  GLint)
;;;    (v1  GLfloat)
;;;    (v2  GLfloat))


(def-foreign-function (glEvalPoint1
                       (:name "glEvalPoint1")
                       (:return-type :null))
    (i  GLint))


(def-foreign-function (glEvalPoint2
                       (:name "glEvalPoint2")
                       (:return-type :null))
    (i  GLint)
    (j  GLint))


(def-foreign-function (glEvalMesh1
                       (:name "glEvalMesh1")
                       (:return-type :null))
    (mode  GLenum)
    (i1   GLint)
    (i2   GLint))


(def-foreign-function (glEvalMesh2
                       (:name "glEvalMesh2")
                       (:return-type :null))
    (mode  GLenum)
    (i1   GLint)
    (i2   GLint)
    (j1   GLint)
    (j2   GLint))


;;;(def-foreign-function (glFogf
;;;                       (:name "glFogf")
;;;                       (:return-type :null))
;;;    (pname  GLenum)
;;;    (param  GLfloat))


(def-foreign-function (glFogi
                       (:name "glFogi")
                       (:return-type :null))
    (pname  GLenum)
    (param  GLint))


(def-foreign-function (glFogfv
                       (:name "glFogfv")
                       (:return-type :null))
    (pname  GLenum)
    (params :simple-array)  ;(:pointer GLfloat)
    )


(def-foreign-function (glFogiv
                       (:name "glFogiv")
                       (:return-type :null))
    (pname  GLenum)
    (params :simple-array)			;(:pointer GLint)
    )


(def-foreign-function (glFeedbackBuffer
                       (:name "glFeedbackBuffer")
                       (:return-type :null))
    (size   GLsizei)
    (type   GLenum)
    (buffer :simple-array)  ;(:pointer GLfloat)
    )


(def-foreign-function (glPassThrough
                       (:name "glPassThrough")
                       (:return-type :null))
    (token  GLfloat))


(def-foreign-function (glSelectBuffer
                       (:name "glSelectBuffer")
                       (:return-type :null))
    (size   GLsizei)
    (buffer :simple-array)  ;(:pointer GLuint)
    )


(def-foreign-function (glInitNames
                       (:name "glInitNames")
                       (:return-type :null))
    ;;(ARG_G25  :null)
    )


(def-foreign-function (glLoadName
                       (:name "glLoadName")
                       (:return-type :null))
    (name  GLuint))


(def-foreign-function (glPushName
                       (:name "glPushName")
                       (:return-type :null))
    (name  GLuint))


(def-foreign-function (glPopName
                       (:name "glPopName")
                       (:return-type :null))
    ;;(ARG_G26  :null)
    )


#-(and sbcl mswindows)
(def-foreign-function (glBlendEquationEXT
                       (:name "glBlendEquationEXT")
                       (:return-type :null))
    (mode  GLenum))

;;; This depends on GL_ARB_imaging extension
#-(and sbcl mswindows)
(def-foreign-function (glBlendColor
                       (:name "glBlendColor")
                       (:return-type :null))
    (red   GLfloat)
    (green  GLfloat)
    (blue  GLfloat)
    (alpha  GLfloat))

#+do-not-use
(def-foreign-function (glBlendColorEXT
                       (:name (freedius-prefix "glBlendColorEXT"))
                       (:return-type :null))
    (red   GLclampf)
    (green  GLclampf)
    (blue  GLclampf)
    (alpha  GLclampf))

#+irix
(def-foreign-function (glPolygonOffsetEXT
                       (:name (freedius-prefix "glPolygonOffsetEXT"))
                       (:return-type :null))
    (factor  GLdouble)
    (bias   GLdouble))

#-irix
(def-foreign-function (glPolygonOffset
                       (:name (freedius-prefix "glPolygonOffset"))
                       (:return-type :null))
    (factor  GLdouble)
  (bias   GLdouble))


(defconstant GL_H :DEFINED)
;;(defconstant MESA :DEFINED)
(defconstant GL_NO_ERROR GL_FALSE)

(def-foreign-function (glDrawArrays
		       (:name "glDrawArrays"))
    (mode GLenum)
  (first GLint)
  (count  GLsizei))

(def-foreign-function (glVertexPointer
		       (:name "glVertexPointer"))
    (size GLint)
  (type GLenum)
  (stride GLsizei)
  (array :simple-array)
  )

(def-foreign-function (glTexCoordPointer
		       (:name "glTexCoordPointer"))
    (size GLint)
  (type GLenum)
  (stride GLsizei)
  (array :simple-array)
  )

(def-foreign-function (glColorPointer
		       (:name "glColorPointer"))
    (size GLint)
  (type GLenum)
  (stride GLsizei)
  (array :simple-array)
  )

(def-foreign-function (glNormalPointer
		       (:name "glNormalPointer"))
  (type GLenum)
  (stride GLsizei)
  (array :simple-array)
  )
(def-foreign-function (glEdgeFlagPointer
		       (:name "glColorPointer"))
    (stride GLsizei)
  (array :simple-array)
  )

#| these appear to be broken in SGI library

(def-foreign-function (glDrawElements
		       (:name "glDrawElements"))
    (mode GLenum)
  (count  GLsizei)
  (type GLenum)
  (indices :simple-array)
  )

(def-foreign-function (glEnableClientState (:name "glEnableClientState"))
    (cap GLenum))

(def-foreign-function (glDisableClientState (:name "glDisableClientState"))
    (cap GLenum))
|#

(defconstant GL_VERTEX_ARRAY #X8074)
(defconstant GL_NORMAL_ARRAY #x8075 )
(defconstant GL_COLOR_ARRAY #x8076 )
(defconstant GL_INDEX_ARRAY #x8077)
(defconstant GL_TEXTURE_COORD_ARRAY #x8078)
(defconstant GL_EDGE_FLAG_ARRAY #x8079)

(def-foreign-function (handle_gl_errors (:name (freedius-prefix "handle_gl_errors")))
  (string :simple-string))


(def-foreign-function (glGenTextures (:name "glGenTextures"))
  (n GLsizei)
  (textures :simple-array))

(def-foreign-function (glDeleteTextures (:name "glDeleteTextures"))
  (n GLsizei)
  (textures :simple-array))

(def-foreign-function (glBindTexture (:name "glBindTexture"))
  (target GLenum)
  (texture GLuint))

(defconstant GL_CLAMP_TO_EDGE #x812F )
(defconstant GL_CLAMP_TO_BORDER #x812D)

(def-foreign-function (glAreTexturesResident (:name "glAreTexturesResident")
					     (:return-type GLboolean))
    (n GLuint)
  (textures :simple-array) ; GLuint *textures = (simple-array (unsigned-byte 32))
  (residences :simple-array) ; GLboolean *residences = (simple-array (unsigned-byte 8))
  )

(def-foreign-function (glPrioritizeTextures (:name "glPrioritizeTextures"))
    (n GLuint)
  (textures :simple-array) ; GLuint *textures = (simple-array (unsigned-byte 32))
  (priorities :simple-array) ; GLclampf *priorities = (simple-array :single-float)
  ) 
