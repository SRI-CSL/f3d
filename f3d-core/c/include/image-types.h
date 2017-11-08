#ifndef	__image_types_h
#define __image_types_h


#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

// Should this be C accessable ?  Currently this is C++ only.

#ifdef xxx__cplusplus
extern "C" {
#endif

typedef struct RGB8_PIXEL {
  unsigned char r;
  unsigned char g;
  unsigned char b;
} RGB8_PIXEL;

typedef struct RGBA8_PIXEL {
  unsigned char r;
  unsigned char g;
  unsigned char b;
  unsigned char a;  
} RGBA8_PIXEL;

typedef unsigned char uchar;

  typedef unsigned short ushort;


typedef enum __image_element_type__ {
  IMG_UNSIGNED_1BIT,
  IMG_UNSIGNED_2BIT,
  IMG_UNSIGNED_4BIT,
  IMG_UNSIGNED_8BIT,
  IMG_UNSIGNED_16BIT,
  IMG_UNSIGNED_32BIT,
  IMG_SIGNED_1BIT,
  IMG_SIGNED_2BIT,
  IMG_SIGNED_4BIT,
  IMG_SIGNED_8BIT,
  IMG_SIGNED_16BIT,
  IMG_SIGNED_32BIT,
  IMG_SINGLE_FLOAT,
  IMG_DOUBLE_FLOAT,
  IMG_RGB8,
  IMG_RGBA8,
  //IMG_RGB16,
  //IMG_RGBA16,
  IMG_GENERAL_TYPE
  } IMAGE_ELEMENT_TYPE;


typedef enum __image_class_type__ {
  UNKNOWN_IMAGE_CLASS,
  ARRAY_IMAGE_CLASS,
  FILE_IMAGE_CLASS,
  LAZY_IMAGE_CLASS
} IMAGE_CLASS;

typedef enum __image_element_numeric_type__ {
  IMG_UNSIGNED_INT,
  IMG_SIGNED_INT,
  IMG_IEEEFP,
  IMG_MIXED_TYPES
} IMAGE_ELEMENT_NUMERIC_TYPE;

typedef enum __image_element_photometic_type__ {
  IMG_MIN_IS_BLACK,
  IMG_RGB
} IMAGE_ELEMENT_PHOTOMETRIC_TYPE;

typedef enum __file_image_format__ {
  UNKNOWN_FILE_IMAGE_FORMAT,
  IU_TESTBED_FILE_IMAGE_FORMAT,
  TIFF_FILE_IMAGE_FORMAT
} FILE_IMAGE_FORMAT;

#define IMAGE_GET_PUT_OK 0
#define IMAGE_GET_PUT_BAD_BOUNDS -1
#define IMAGE_GET_PUT_UNSUPPORTED_BUFFER_TYPE -2

// Array Element Type for image xmap, ymap.
  // Should this be unsigned long instead of unsigned int ?
  // No, on x86-64 long means 64bit.
typedef unsigned int MAP_ELEMENT_TYPE;

#ifdef HAVE_ALLEGRO
#define MAP_ELEMENT_TYPE_T
#endif /* HAVE_ALLEGRO */

// Shift to convert Lisp image-map element to a C integer.
#ifdef MAP_ELEMENT_TYPE_T

// Must shift by 2 for Lisp type T arrays.
#define MAP_ELEMENT_SHIFT(index) ((index)>>2)
#define MAP_ELEMENT_WRITE_SHIFT(index) ((index)<<2)

#else /* ! MAP_ELEMENT_TYPE_T */

// No shifts for Lisp (unsigned-byte 32) type arrays.
#define MAP_ELEMENT_SHIFT(index) (index)
#define MAP_ELEMENT_WRITE_SHIFT(index) (index)

#endif /* ! MAP_ELEMENT_TYPE_T */

#ifdef xxx__cplusplus
} 
#endif

END_NAMESPACE_FREEDIUS

#endif /* ! __image_types_h */
