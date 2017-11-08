/*

This file remains in C since it defines mostly stuff accessed from Lisp.

I guess it would be easy enough to put extern "C" around the Lisp accessed things
and convert the file to c++.

*/

#include <tiffio.h>
#include "lisp-ffi-macros.h"

//#include "misc.h"

void FREEDIUS_free(void *ptr);
void *FREEDIUS_galloc(int nbytes);
#define XALLOC(type,nelems)((type *)FREEDIUS_galloc((nelems)*sizeof(type)))


MKVAR(TIFFTAG_IMAGEWIDTH)
MKVAR(TIFFTAG_IMAGELENGTH)
MKVAR(TIFFTAG_BITSPERSAMPLE)
MKVAR(TIFFTAG_SAMPLESPERPIXEL)
MKVAR(TIFFTAG_TILEWIDTH) 
MKVAR(TIFFTAG_TILELENGTH) 
MKVAR(TIFFTAG_ROWSPERSTRIP)
MKVAR(TIFFTAG_TILEOFFSETS)

MKVAR(TIFFTAG_COMPRESSION)
MKVAR(COMPRESSION_NONE)
MKVAR(COMPRESSION_CCITTRLE)
MKVAR(COMPRESSION_CCITTFAX3)
MKVAR(COMPRESSION_CCITTFAX4)
MKVAR(COMPRESSION_LZW)
MKVAR(COMPRESSION_OJPEG)
MKVAR(COMPRESSION_JPEG)
MKVAR(COMPRESSION_DEFLATE)
MKVAR(COMPRESSION_DCS)
MKVAR(COMPRESSION_JBIG)
MKVAR(COMPRESSION_PACKBITS)



MKVAR(TIFFTAG_SAMPLEFORMAT)
MKVAR(SAMPLEFORMAT_UINT)
MKVAR(SAMPLEFORMAT_INT)
MKVAR(SAMPLEFORMAT_IEEEFP)
MKVAR(SAMPLEFORMAT_VOID)

MKVAR(TIFFTAG_ORIENTATION)
MKVAR(ORIENTATION_TOPLEFT)
MKVAR(ORIENTATION_TOPRIGHT)
MKVAR(ORIENTATION_BOTRIGHT)
MKVAR(ORIENTATION_BOTLEFT)
MKVAR(ORIENTATION_LEFTTOP)
MKVAR(ORIENTATION_RIGHTTOP)
MKVAR(ORIENTATION_RIGHTBOT)
MKVAR(ORIENTATION_LEFTBOT)

MKVAR( TIFFTAG_JPEGCOLORMODE)
MKVAR( JPEGCOLORMODE_RAW)
MKVAR( JPEGCOLORMODE_RGB)
MKVAR( TIFFTAG_JPEGQUALITY)

MKVAR(TIFFTAG_PLANARCONFIG)
MKVAR(PLANARCONFIG_CONTIG)
MKVAR(PLANARCONFIG_SEPARATE)


MKVAR(TIFFTAG_PHOTOMETRIC)
MKVAR(PHOTOMETRIC_MINISWHITE)
MKVAR(PHOTOMETRIC_MINISBLACK)
MKVAR(PHOTOMETRIC_RGB)
MKVAR(PHOTOMETRIC_PALETTE)
MKVAR(PHOTOMETRIC_MASK)
MKVAR(PHOTOMETRIC_SEPARATED)
MKVAR(PHOTOMETRIC_YCBCR)
MKVAR(PHOTOMETRIC_CIELAB)

/* String slots */
MKVAR(TIFFTAG_IMAGEDESCRIPTION)

MKVAR(TIFFTAG_ARTIST)
MKVAR(TIFFTAG_DATETIME)
MKVAR(TIFFTAG_DOCUMENTNAME)
MKVAR(TIFFTAG_HOSTCOMPUTER)
MKVAR(TIFFTAG_INKNAMES)
MKVAR(TIFFTAG_MAKE)
MKVAR(TIFFTAG_MODEL)
MKVAR(TIFFTAG_PAGENAME)
MKVAR(TIFFTAG_SOFTWARE)
MKVAR(TIFFTAG_TARGETPRINTER)

MKVAR(TIFFTAG_YCBCRSUBSAMPLING)
MKVAR(TIFFTAG_REFERENCEBLACKWHITE)

char *tiffgetfield_string (TIFF* tif, ttag_t tag)
{
  char *str;
  if (TIFFGetFieldDefaulted(tif, tag, &str) == 1)
    return(str);
      else return(0);
}
     
#define U32ELEMENT_SHIFT 0

//static
void array_copy_u16_to_u32 (unsigned short arr16[], unsigned int arr32[], int n)
{
  int i;
  for (i =0; i<n; i++) 
    arr32[i] = arr16[i] <<U32ELEMENT_SHIFT; // What is the shift for?  Is this a leftover from Lucid?
}

//static
void array_copy_u32_to_u16 (unsigned int arr32[], unsigned short arr16[], int n)
{
  int i;
  for (i =0; i<n; i++) 
    arr16[i] = arr32[i] >>U32ELEMENT_SHIFT;
}


//static
void array_copy_u8_to_u32 (unsigned char arr8[], unsigned int arr32[], int n)
{
  int i;
  for (i =0; i<n; i++)
    arr32[i] = arr8[i] <<U32ELEMENT_SHIFT;
}

//static
void array_copy_u32_to_u8 (unsigned int arr32[], unsigned char arr8[], int n)
{
  int i;
  for (i =0; i<n; i++)
    arr8[i] = arr32[i] >>U32ELEMENT_SHIFT;
}


void TIFFGetField_uint32_array (TIFF* tif, ttag_t tag, unsigned int *arr, int n)
{
  unsigned int *arr2; 
  int i;
  TIFFGetFieldDefaulted(tif, tag, &arr2);
  for (i=0; i<n; i++)
    arr[i]=arr2[i];
}

void TIFFGetField_float_array (TIFF* tif, ttag_t tag, float *arr, int n)
{
  float *arr2; 
  int i;
  TIFFGetFieldDefaulted(tif, tag, &arr2);
  for (i=0; i<n; i++)
    arr[i]=arr2[i];
}


void TIFFGetField_uint16_array (TIFF* tif, ttag_t tag, unsigned short *arr, int n)
{
  unsigned short *arr2; 
  int i;
  TIFFGetFieldDefaulted(tif, tag, &arr2);
  for (i=0; i<n; i++)
    arr[i]=arr2[i];
}

#if 0
#ifndef u_short
typedef unsigned short u_short;
#endif
#ifndef u_char
typedef unsigned char u_char;
#endif
#endif

typedef unsigned short ushort2;
typedef unsigned char uchar2;

static int checkcmap(int n, ushort2 *r, ushort2 *g, ushort2 *b)
{
  while (n-- >= 0)
    if (*r++ >= 256 || *g++ >= 256 || *b++ >= 256) return (16);

  return (8);
}

#if 0
int TIFFReadColormap (TIFF* tif, u_char *rmap, u_char *gmap, u_char *bmap)
{
  int x, range; 
  unsigned short bitspersample;
  unsigned short *redcmap, *greencmap, *bluecmap;
  TIFFGetField(tif, TIFFTAG_BITSPERSAMPLE, &bitspersample);
  if (!TIFFGetField(tif, TIFFTAG_COLORMAP,
		    &redcmap, &greencmap, &bluecmap)) {
      return (0);
    }
  range = (1<<bitspersample)-1;
  if (checkcmap(range, redcmap, greencmap, bluecmap) == 16) {

#define	CVT(x)		((((int) x) * 255) / ((1L<<16)-1))

    for (x = range; x >= 0; x--) {
	rmap[x] = CVT(redcmap[x]);
	gmap[x] = CVT(greencmap[x]);
	bmap[x] = CVT(bluecmap[x]);
      }
    } else {
      for (x = range; x >= 0; x--) {
	rmap[x] = redcmap[x];
	gmap[x] = greencmap[x];
	bmap[x] = bluecmap[x];
      }
    }
  return 1;
 
}
#endif

int TIFFReadColormap (TIFF* tif, uchar2 *rmap, uchar2 *gmap, uchar2 *bmap)
{
  int x, range; 
  unsigned short bitspersample;
  TIFFGetField(tif, TIFFTAG_BITSPERSAMPLE, &bitspersample);
  {
    int n = 1 << bitspersample;
    unsigned short *redcmap   = XALLOC(unsigned short, n);
    unsigned short *greencmap = XALLOC(unsigned short, n);
    unsigned short *bluecmap  = XALLOC(unsigned short, n);
    int flag;
    if (!TIFFGetField(tif, TIFFTAG_COLORMAP, redcmap, greencmap, bluecmap)) {
      FREEDIUS_free(redcmap); FREEDIUS_free(greencmap); FREEDIUS_free(bluecmap);
      return (0);
    }

    range = (1<<bitspersample)-1;
    if (checkcmap(range, redcmap, greencmap, bluecmap) == 16) {

#define	CVT(x)		((((int) x) * 255) / ((1L<<16)-1))

      for (x = range; x >= 0; x--) {
	rmap[x] = CVT(redcmap[x]);
	gmap[x] = CVT(greencmap[x]);
	bmap[x] = CVT(bluecmap[x]);
      }
    } else {
      for (x = range; x >= 0; x--) {
	rmap[x] = redcmap[x];
	gmap[x] = greencmap[x];
	bmap[x] = bluecmap[x];
      }
    }
    FREEDIUS_free(redcmap); FREEDIUS_free(greencmap); FREEDIUS_free(bluecmap);
  }
  return 1;
 
}
