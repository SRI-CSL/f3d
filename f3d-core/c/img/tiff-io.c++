// -*- Mode: C++; c-basic-offset: 2 -*-

// $Id: tiff-io.c++,v 1.30.2.2.2.9 2009/04/17 23:22:05 quam Exp $

#include <sys/types.h>
#ifndef _WIN32
#include <sys/uio.h>
#include <sys/file.h>
#endif
#include <stdio.h>
#ifndef _WIN32
#include <unistd.h>
#endif
#include <fcntl.h>

#ifdef __MINGW32__
// Supplies alloca:
#include <malloc.h>
#endif

#include "array-image.h"
#include "cme-error.h"
//#include <stdlib.h>
#include "image.h"
#include "file-image.h"
#include "image-io.h"
#include "iu-testbed-io.h"
#include "misc.h"
#include "color.h"

#include "tiff-io.h"

#include "array-image.h"
#include "lazy-image.h"

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

#define GEOTIFFTAG_MODEL_PIXEL_SCALE  33550
#define GEOTIFFTAG_MODEL_TIE_POINT    33922
#define GEOTIFFTAG_GEO_KEY_DIRECTORY  34735
#define GEOTIFFTAG_GEO_ASCII_PARAMS   34737



// setClass(tiff_image_header, basic_image_header);

#define TIFF_IMAGE_HEADER_MAGIC_NUMBER 42
int tiff_image_header::recognize_header (int fd)
{ 
  char buf[4];
  while (lseek(fd, 0L, SEEK_SET) == -1L)
    tprintf(3, "tiff_image_header: lseek fails.  Trying again.\n");

  fullread(fd,buf,4);
  tprintf(3,"Checking TIFF...\n");

  if (strncmp(buf,"II", 2) == 0  && buf[2]==TIFF_IMAGE_HEADER_MAGIC_NUMBER
       && buf[3]==0) {
    tprintf(3,"       this is a TIFF file.\n");
    return(1); // little endian format
  }

  if (strncmp(buf,"MM", 2) == 0  && buf[2]==0
      && buf[3]==TIFF_IMAGE_HEADER_MAGIC_NUMBER) {
    tprintf(3,"       this is a TIFF file.\n");
    return(1); // big endian format
  }

  tprintf(3,"       (not TIFF)\n");
  return(0);
}

#define BITS_PER_WORD 32

int tiff_image_header::read_header (TIFF* tif)
{ 
  int tiledp;
  TIFFGetField(tif, TIFFTAG_SAMPLESPERPIXEL, &samplesperpixel);
  extrasamples = 0;
  bitspersample = 8;
  //  TIFFGetField(tif, TIFFTAG_EXTRASAMPLES, &extrasamples);
  //  TIFFGetField(tif, TIFFTAG_BITSPERSAMPLE, &bitspersample);
  TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &xdim);
  TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &ydim);

  // Avoid variable array declarations and just use stack allocation -
  // much simpler and more generally compliant:
  uint16 *bps = (uint16 *) alloca(samplesperpixel * sizeof(uint16));
  uint16 *sampleformat = (uint16 *) alloca(samplesperpixel * sizeof(uint16));
  

  TIFFGetField(tif, TIFFTAG_PLANARCONFIG, &planarconfig);
  TIFFGetField(tif, TIFFTAG_SAMPLEFORMAT, sampleformat);
  TIFFGetField(tif, TIFFTAG_BITSPERSAMPLE, bps);
  TIFFGetField(tif, TIFFTAG_PHOTOMETRIC, &photo_interp);
  // Special cases: samplesperpixel between 1 and 4, and bps=8 implies
  // that we can pack the result into a 16 or 32-bit image.  In
  // particular, 3 samplesperpixel implies color, although we should
  // check the photometric properties to be sure.
  element_size=bps[0];  // FIXME
  vector_image_p = 0;
  element_type = image_element_type_from_size(element_size, 
					      (sampleformat[0]==SAMPLEFORMAT_INT),  // signed integer sata
					      (sampleformat[0]==SAMPLEFORMAT_IEEEFP));
#if 0

  // image_element_type_from_size is broken for color-images
  if (samplesperpixel > 1) 
    if (photo_interp == PHOTOMETRIC_RGB) {
      if (element_size==8 && samplesperpixel==3) {
	element_type = IMG_RGB8; element_size = 24; samplesperpixel=1;
      }
      else if (element_size==8 && samplesperpixel==4) {
	element_type = IMG_RGBA8; element_size = 32; samplesperpixel=1;
      }
      
      //else if (element_size==16 && samplesperpixel==3) element_type = IMG_RGB16;
      //else if (element_size==16 && samplesperpixel==4) element_type = IMG_RGBA16; 
      else vector_image_p = 1;
    } else vector_image_p = 1;

  if (photo_interp != PHOTOMETRIC_PALETTE) use_cmap = 0;
  else {
    use_cmap = 1;
    TIFFGetField(tif, TIFFTAG_COLORMAP, &(cmap[0]), &(cmap[1]), &(cmap[2]));
  }

#else

  use_cmap = 0;
  vector_image_p = 0;

  if (samplesperpixel > 1) {
    switch (photo_interp) {
    case PHOTOMETRIC_PALETTE:
      use_cmap = 1;
      TIFFGetField(tif, TIFFTAG_COLORMAP, &(cmap[0]), &(cmap[1]), &(cmap[2]));
      break;

    case PHOTOMETRIC_MINISBLACK:
    case PHOTOMETRIC_MINISWHITE:
      tprintf(3,"Scalar photometric properties.\n");
      // This is a no-op right now.  Figure out how to get it working:
      vector_image_p = 1;
      // This forces a 1-band image on output:
      if (planarconfig == PLANARCONFIG_SEPARATE) {
	samplesperpixel = 1;
	vector_image_p = 0;
      }
      break;

    case PHOTOMETRIC_RGB:
    case PHOTOMETRIC_YCBCR:   // Usually JPEG.  Forces color conversion.
      if (element_size==8 && samplesperpixel==3) {
	element_type = IMG_RGB8; element_size = 24; samplesperpixel=1;
      }
      else if (element_size==8 && samplesperpixel==4) {
	element_type = IMG_RGBA8; element_size = 32; samplesperpixel=1;
      }
      break;

    default:
      vector_image_p = 1;
    }
  }

#endif

  tiledp=TIFFIsTiled(tif);
  if (tiledp) {
    TIFFGetField(tif, TIFFTAG_TILEWIDTH, &block_xdim);
    TIFFGetField(tif, TIFFTAG_TILELENGTH, &block_ydim);
  } else {
    block_xdim=xdim;
    block_ydim=ydim;    
  }
  // FIXME: This next loses for 24bit RGB images
  padded_block_xdim = pad_to_multiple(block_xdim, BITS_PER_WORD/element_size);
  block_size=block_xdim*block_ydim;
  blocks_wide=(xdim-1)/block_xdim+1; // change to use pad_to_multiple
  blocks_hi  =(ydim-1)/block_ydim+1;
  block_ydim = -block_ydim;
  header_length=0;
 
 return(1);
}

int tiff_image_header::read_header (int fd)
{ TIFF* tif = TIFFOpen(path, "rm");
  if (tif == 0) {
    warning("tiff_image_header::read_header cannot open file %s\n", path);
    return(0);
  }
  read_header(tif);
  TIFFClose(tif);
  return(1);
}

extern "C" {
int FREEDIUS_GLOBAL(tiff_file_header_params) (char *path, int params[10]) {
  TIFF* tif = TIFFOpen(path, "rm");
  if (tif == 0) {
    warning("tiff_file_header_params::read_header cannot open file %s\n", path);
    return(0);
  }
  tiff_image_header *hdr = tiff_image_header1;
  hdr->read_header(tif);
  toff_t *tile_offsets; 
  TIFFGetFieldDefaulted(tif, TIFFTAG_TILEOFFSETS, &tile_offsets);
  
  params[0] = (int) hdr->element_type;
  params[1] = hdr->xdim;
  params[2] = hdr->ydim;
  params[3] = hdr->block_xdim;
  params[4] = hdr->block_ydim;
  params[5] = tile_offsets[0];  
  // params[6] = 0; // magic flag bits -- currently undefined
  TIFFClose(tif);
  return 1;
}

} // end extern "C"




char* tiff_image_header::read_property_list_string (char* path)
{
  TIFF* tif = TIFFOpen(path, "rm");
  if (tif == 0) {
    warning("tiff_image_header::read_property_list_string cannot open file %s\n",  path);
    return(0);
  }
  char expected_hdr[] = "SRI CME PROPERTY-LIST";
  int hdr_len = strlen(expected_hdr);
  char *creation_date=0; 
  char *description =0;
  char *geo_ascii_params =0;
  TIFFGetField(tif, TIFFTAG_DATETIME, &creation_date);
  TIFFGetField(tif, TIFFTAG_IMAGEDESCRIPTION, &description);
  TIFFGetField(tif, GEOTIFFTAG_GEO_ASCII_PARAMS, &geo_ascii_params);
  int descr_string_len = description ? strlen(description):0;
  int date_string_len = creation_date ?  strlen(creation_date) : 0;
  char *pl_string = ZALLOC(char, 100+descr_string_len+date_string_len);
  char *p = pl_string;
  if (description) {
    if (strncmp(expected_hdr, description, strlen(expected_hdr)) == 0)
      p += sprintf(p, "%s", description);
    else
      p += sprintf(p, "DESCRIPTION = %s\n", description);
  }
  if (creation_date)
    sprintf(p, "CREATION-DATE = %s\n", creation_date);
  if (geo_ascii_params)
    sprintf(p, "GEO-ASCII-PARAMS = %s\n", geo_ascii_params);

 
  // Is it necessary to free any of the free any of the strings from TIFFGetField?
  // I get seg-fault or other problems with the following calls to free.
  // free(creation_date); free(description);
  TIFFClose(tif);
  return pl_string;
}


#define NOUNROLL 1
#ifdef NOUNROLL
template <class fromtype, class totype>
void array_copy(fromtype from, totype to, int n)
{
  int i;
  for (i=0; i<n; i++) to[i]=from[i];
}
#else
template <class fromtype, class totype>
void array_copy(fromtype *fp, totype *tp, int i)
{
  if (i>=8) {
    do {
      *(tp  ) = (totype) *(fp);
      *(tp+1) = (totype) *(fp+1);
      *(tp+2) = (totype) *(fp+2);
      *(tp+3) = (totype) *(fp+3);
      *(tp+4) = (totype) *(fp+4);
      *(tp+5) = (totype) *(fp+5);
      *(tp+6) = (totype) *(fp+6);
      *(tp+7) = (totype) *(fp+7);
      i = i-8; tp +=8; fp +=8;}
    while (i >= 8);}
  if (i > 0)
    {do {*tp++ = (totype) *fp++; i--;}
     while (i > 0);}
}
#endif

void foo2()
{}

template <class FROMTYPE, class TOTYPE> 
void copy_tiff_tile_to_image (image* img, FROMTYPE* buf, TOTYPE* obuf, int samples_per_tile, 
			      int x, int y1, int nx, int bx, int ny, int spp)
{
  int bp, y, n;
  bx = bx*spp; nx = nx*spp;  // compute number of samples from pixels using samplesperpixel
  foo2();
  if ((void *)buf != (void *)obuf)
    array_copy(buf, obuf, samples_per_tile); 
  for (bp=0, y=y1, n=ny; n>0; n--, y--, bp+=bx) {
    int flag = img->putline(obuf+bp, x, y, nx);
    if (flag < 0) error("copy_tiff_tile_to_image ~a putline error");
  }
}


image * tiff_image_header::load_band_interleaved_tiled_tiff_image(TIFF* tif)
{
  int x, y, y1;
  image *img;
  // Something is very broken with TIFFTileSize in the TIFF library.
  // It appears to return 4 x the correct size.  
  //int tilesize = TIFFTileSize(tif);
  int spp = samplesperpixel;
  int samples_per_tile = block_size*samplesperpixel;
  int bytes_per_sample = element_size>>3;
  int bytes_per_tile = samples_per_tile*bytes_per_sample;
  tprintf(3, "load_band_interleaved_tiled_tiff_image: tilesize=%d, spp=%d\n", bytes_per_tile,samplesperpixel);

  if (into_image) img = into_image;
  else img = make_image(xdim, ydim, element_type, samplesperpixel);
            // img = make_band_interleaved_array_image(xdim, ydim, element_type, samplesperpixel);
  {
    int bx = block_xdim;
    int by = abs(block_ydim);
    // Codewarrior flags the above as errors...
#define SLOP 16 // Doesn't help
    char *buf0 = XALLOC(char, bytes_per_tile+SLOP);
    int *ibuf0 = XALLOC(int, samples_per_tile+SLOP);

    char *buf = XALLOC(char, bytes_per_tile+SLOP);
    int *ibuf = XALLOC(int, samples_per_tile+SLOP);
    xxfree(buf0);
    xxfree(ibuf0);
    
    if (1)
      tprintf(3, "loading tiled image (%d %d %d %d) image from %s %d\n.",
	      xdim, ydim, element_size, samplesperpixel, path, bytes_per_tile);
    for (y=0, y1=ydim-1; y<ydim; y+=by, y1-=by) 
      for (x=0; x<xdim; x+=bx) {
	int nx = MIN(bx, xdim-x);
	int ny = MIN(by, y1+1);
	TIFFReadTile(tif, buf, x, y, 0, 0);
	switch (element_type) {
	case IMG_RGB8:  
	  copy_tiff_tile_to_image(img, (RGB8_PIXEL*) buf, (RGB8_PIXEL*) buf, samples_per_tile, x, y1, nx, bx, ny, spp);
	  break;
	case IMG_RGBA8:
	  copy_tiff_tile_to_image(img, (RGBA8_PIXEL*) buf, (RGBA8_PIXEL*) buf, samples_per_tile, x, y1, nx, bx, ny, spp);
	  break;
	case IMG_DOUBLE_FLOAT:
	  copy_tiff_tile_to_image(img, (double *)buf, (double *)buf, samples_per_tile, x, y1, nx, bx, ny, spp);
	  break;
	case IMG_SINGLE_FLOAT:
	  copy_tiff_tile_to_image(img, (float *)buf, (float *)buf, samples_per_tile, x, y1, nx, bx, ny, spp);
	  break;
	case IMG_SIGNED_8BIT:  
	case IMG_UNSIGNED_8BIT:  
	  copy_tiff_tile_to_image(img, (char *)buf, ibuf, samples_per_tile, x, y1, nx, bx, ny, spp);
	  break;
	case IMG_SIGNED_16BIT: 
	case IMG_UNSIGNED_16BIT: 
	  copy_tiff_tile_to_image(img, (ushort *)buf, ibuf, samples_per_tile, x, y1, nx, bx, ny, spp);
	  break;
	case IMG_SIGNED_32BIT: 
	case IMG_UNSIGNED_32BIT: 
	  copy_tiff_tile_to_image(img, (unsigned int *)buf, ibuf, samples_per_tile, x, y1, nx, bx, ny, spp);
	  break;
	default: error("load_tiled_tiff_image: illegal bps = %d", element_size);
	} // end switch
      } // end for x for y
	 
    if (use_cmap) {
      int len = 1 << element_size;
      unsigned short **copy = copy_colormap(cmap, len);
      set_image_colormap(img, copy);
    }
    xxfree(buf);
    xxfree(ibuf);
    return img;
  }
}

image * tiff_image_header::load_band_sequential_tiled_tiff_image(TIFF* tif)
{
  error("Not yet implemented");
  return 0;
}

image * tiff_image_header::load_tiled_tiff_image(TIFF* tif)
{
  //  uint16 planarconfig;
  //  TIFFGetField(tif, TIFFTAG_PLANARCONFIG, &planarconfig);

  if (planarconfig == PLANARCONFIG_CONTIG)
    return load_band_interleaved_tiled_tiff_image(tif);
  else
    return load_band_sequential_tiled_tiff_image(tif);
}

/*
If a row interleave effect is desired, a writer might write out the data as
PlanarConfiguration=2—separate sample planes—but break up the planes into
multiple strips (one row per strip, perhaps) and interleave the strips.

RowsPerStrip=1 PlanarConfiguration=2
*/

extern "C" {
  void FREEDIUS_brk (const char *str)
{
  int i;
  i=1;
}

} // end extern "C"

#if 0 // support for getline/putline band argument is broken .
template <class buftype>
void packed_putline (image *img, buftype *pbuf, int x0, int y)
{
  int x, nx, b;
  int n = img->xdim - x0;
  int spp = img->samples_per_pixel;
  FREEDIUS_brk("packed_putline");
  if (spp == 1) {
    b = 0;  // band 0 by default.
    if (0) // (image_element_type(img) == IMG_UNSIGNED_8BIT)
      img->putline((uchar *)pbuf, x0, y, 0, 0, b);
    else img->putline(pbuf, x0, y, 0, 0, b);
  } else {
    // buftype buf[n];
    //buftype *buf = XALLOC(buftype, n);
    buftype *buf = (buftype *)alloca(n * sizeof(buftype));
    for (b=0; b<spp; b++) {
      // pixel interleaved samples into bands of vector image 
      for (x=0, nx=b; x < n; x++, nx+=spp) buf[x] = pbuf[nx];
      if (0) //(image_element_type(img) == IMG_UNSIGNED_8BIT)
	img->putline((uchar *)buf, x0, y, 0, 0, b);
      else img->putline(buf, x0, y, 0, 0, b);
    }
    //xxfree(buf);
  }
}
#else
template <class buftype>
void packed_putline (image *img, buftype *pbuf, int x0, int y)
{
  // putline works fine with band-interleaved pbuf and scalar pbuf.
  img->putline(pbuf, x0, y, 0, 0);
}

#endif

/*
TIFF Directory at offset 0x8a7c20
  Image Width: 12288 Image Length: 3072
  Resolution: 72, 72 (unitless)
  Bits/Sample: 8
  Compression Scheme: JPEG
  Photometric Interpretation: YCbCr
  FillOrder: msb-to-lsb
  YCbCr Subsampling: 2, 2
  Orientation: row 0 top, col 0 lhs
  Samples/Pixel: 3
  Rows/Strip: 16
  Planar Configuration: single image plane
  Reference Black/White:
     0:     0   255
     1:   128   255
     2:   128   255
  DocumentName: ahbgsprn.tif
  Software: ImageMagick 6.0.7 08/28/06 Q16 http://www.imagemagick.org
  Tag 34264: 0.999990,-0.004555,0.000000,543567.078283,-0.004555,-0.999990,0.000000,3363476.497272,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,1.000000
  Tag 34735: 1,1,0,0
  JPEG Tables: (574 bytes)
*/


//
// Read a TIFF image that has compression (typically jpeg):
//
// There appear to be no callers to this function
image * tiff_image_header::load_untiled_compressed_tiff_image (TIFF* tif)
{
  tstrip_t strip;
  int i,j, v;
  // if (vector_image_p) return load_untiled_tiff_vector_image(tif);
  int bps = element_size;
  int row, y, k;
  int spp = samplesperpixel;
  int bufsize = TIFFStripSize(tif);
  int nstrips = TIFFNumberOfStrips(tif);
  int bytes_per_row = xdim * spp;
  int rows_per_strip = nstrips / bytes_per_row; // This looks totally bogus -- should it be bufsize/bytes_per_row?
  // uchar buf[bufsize];
  uchar *buf = XALLOC(uchar, bufsize);
  uchar *rbuf;
  // int ibuf[xdim*spp];    
  int *ibuf = XALLOC(int, xdim*spp);
  image *img;
  int xmid = 3*128;
  //double dbuf[xdim*spp];
  double *dbuf = XALLOC(double, xdim*spp);
  
  if (into_image) img = into_image;
  else img = make_image(xdim, ydim, element_type, samplesperpixel);

  tprintf(3, "load_untiled_compressed_tiff_image: loading (%d %d %d %d %d) image from %s %d\n.",
	  xdim, ydim, element_size, samplesperpixel, element_type, path, bufsize);

  tprintf(5, "load_untiled_compressed_tiff_image: Allocated buffer of length %d (xdim*spp = %d).\n.", bufsize, xdim*spp);

  row = 0;
  y = ydim - 1;
  for (strip=0; strip < nstrips; strip++) {

    tprintf(5, "load_untiled_compressed_tiff_image: calling TIFFReadEncodedStrip(tif, %d, buf, -1).\n", strip);

    if (TIFFReadEncodedStrip(tif, strip, buf, -1) == -1) {
      fprintf(stderr, "TIFFReadEncodedStrip(0x%p, %d, 0x%p, -1) returned -1 (error).\n", tif, strip, buf);
      return NULL;
    }

    rbuf = buf;
    for (k = 0; k < rows_per_strip; k++) {
      switch (element_type) {
      case IMG_RGB8:
	img->putline((RGB8_PIXEL*) buf, 0, y);
	// fprintf(stderr, "load_untiled_compressed_tiff_image RGB8 line=%d, line[%d]=%f\n", row, xmid, buf[xmid]);
	break;
      case IMG_RGBA8: img->putline((RGBA8_PIXEL*) buf, 0, y); break;
      case IMG_SINGLE_FLOAT: 
	array_copy((float *)buf, dbuf,xdim*spp); 
	// fprintf(stderr, "load_untiled_compressed_tiff_image float line=%d, line[%d]=%f\n", row, xmid, dbuf[xmid]);
	packed_putline(img, dbuf, 0, y); 
	break;
      case IMG_DOUBLE_FLOAT: packed_putline(img, (double *)buf, 0, y); break;
      case IMG_UNSIGNED_8BIT:
	// Hack for Leica GeoTIFFs:
	packed_putline(img, (uchar *)buf, 0, y);
	break;
      case IMG_UNSIGNED_16BIT: 
	array_copy((ushort *)buf, ibuf, spp*xdim); packed_putline(img, (int *)ibuf, 0, y); break;
	//packed_putline(img, (ushort *)ibuf, 0, y); break;
      case IMG_SIGNED_16BIT: 
	array_copy((short *)buf, ibuf, spp*xdim); packed_putline(img, (int *)ibuf, 0, y); break;
      case IMG_SIGNED_32BIT: 
      case IMG_UNSIGNED_32BIT: 
	img->putline( (int*) buf, 0, y); break;
	// This is broken - CC - complains about undefined putline from void* buf into unsigned 32bit image:
	// packed_putline(img, (uint *)buf, 0, y); break;
      default: error("load_untiled_compressed_tiff_image: illegal bps = %d", bps);
      }
      y--;
      row++;
      buf += bytes_per_row;
    }
  }
  //fprintf(stderr, "load_untiled_compressed_tiff_image putlines done\n");

    // Can't close tiled tiff images, but we should close the untiled images:
  TIFFClose(tif);
  if (use_cmap) {
    int len = 1 << element_size;
    ushort **copy = copy_colormap(cmap, len);
    set_image_colormap(img, copy);
  }
  xxfree(buf);  xxfree(ibuf); xxfree(dbuf);
  tprintf(3, "load_untiled_compressed_tiff_image: Returning\n");
  return((image *)img);
}

#if 0 // old
// This assumes TIFFTAG_PLANARCONFIG == PLANARCONFIG_CONTIG
image * tiff_image_header::load_untiled_tiff_image (TIFF* tif)
{
  uint16 compression, mode;
  TIFFGetField(tif, TIFFTAG_COMPRESSION, &compression);
  if (compression == COMPRESSION_JPEG) TIFFSetField(tif, TIFFTAG_JPEGCOLORMODE, JPEGCOLORMODE_RGB);

  int i,j, v;
  // if (vector_image_p) return load_untiled_tiff_vector_image(tif);
  int bps = element_size;
  int row, y;
  int spp = samplesperpixel;
  int bufsize = TIFFScanlineSize(tif);
  // uchar buf[bufsize];
  uchar *buf = XALLOC(uchar, bufsize);
  // int ibuf[xdim*spp];    
  int *ibuf = XALLOC(int, xdim*spp);
  image *img;
  int xmid = 3*128;
  //double dbuf[xdim*spp];
  double *dbuf = XALLOC(double, xdim*spp);
  
  if (into_image) img = into_image;
  else img = make_image(xdim, ydim, element_type, samplesperpixel);
  // make_band_interleaved_array_image(xdim, ydim, element_type, samplesperpixel);

  tprintf(3, "load_untiled_tiff_image: loading (%d %d %d %d %d) image from %s %d\n.",
	  xdim, ydim, element_size, samplesperpixel, element_type, path, bufsize);
  //fprintf(stderr, "load_untiled_tiff_image created image of element_type=%d samplesperpixel=%d bufsize=%d\n", element_type, spp, bufsize);

  tprintf(5, "load_untiled_tiff_image: Allocated buffer of length %d (xdim*spp = %d).\n.", bufsize, xdim*spp);

  for (row=0,y=ydim-1; row<ydim; row++, y--) {

    tprintf(5, "load_untiled_tiff_image: calling TIFFReadScanLine(tif, buf, %d, 0).\n", row);

    if (TIFFReadScanline(tif, buf, row, 0) == -1) {
      fprintf(stderr, "TIFFReadScanLine(0x%p, 0x%p, %d, 0) returned -1 (error).\n", tif, buf, row);
      return NULL;
    }

    switch (element_type) {
    case IMG_RGB8:
      img->putline((RGB8_PIXEL*) buf, 0, y);
      // fprintf(stderr, "load_untiled_tiff_image RGB8 line=%d, line[%d]=%f\n", row, xmid, buf[xmid]);
      break;
    case IMG_RGBA8: img->putline((RGBA8_PIXEL*) buf, 0, y); break;
    case IMG_SINGLE_FLOAT: 
      array_copy((float *)buf, dbuf,xdim*spp); 
      // fprintf(stderr, "load_untiled_tiff_image float line=%d, line[%d]=%f\n", row, xmid, dbuf[xmid]);
      packed_putline(img, dbuf, 0, y); 
      break;
    case IMG_DOUBLE_FLOAT: packed_putline(img, (double *)buf, 0, y); break;
    case IMG_UNSIGNED_8BIT:
      // Hack for Leica GeoTIFFs:
      packed_putline(img, (uchar *)buf, 0, y);
      break;
    case IMG_UNSIGNED_16BIT: 
      array_copy((ushort *)buf, ibuf, spp*xdim); packed_putline(img, (int *)ibuf, 0, y); break;
      //packed_putline(img, (ushort *)ibuf, 0, y); break;
    case IMG_SIGNED_16BIT: 
      array_copy((short *)buf, ibuf, spp*xdim); packed_putline(img, (int *)ibuf, 0, y); break;
    case IMG_SIGNED_32BIT: 
    case IMG_UNSIGNED_32BIT: 
      img->putline( (int*) buf, 0, y); break;
      // This is broken - CC - complains about undefined putline from void* buf into unsigned 32bit image:
      // packed_putline(img, (uint *)buf, 0, y); break;
    default: error("load_untiled_tiff_image: illegal bps = %d", bps);
    }
  }
  //fprintf(stderr, "load_untiled_tiff_image putlines done\n");

    // Can't close tiled tiff images, but we should close the untiled images:
  TIFFClose(tif);
  if (use_cmap) {
    int len = 1 << element_size;
    ushort **copy = copy_colormap(cmap, len);
    set_image_colormap(img, copy);
  }
  xxfree(buf);  xxfree(ibuf); xxfree(dbuf);
  tprintf(3, "load_untiled_tiff_image: Returning\n");
  return((image *)img);
}
#else
// LHQ Rewrite Tue Jul 14 2009 to make use of more recent putline capabilities
// This assumes TIFFTAG_PLANARCONFIG == PLANARCONFIG_CONTIG
image * tiff_image_header::load_untiled_tiff_image (TIFF* tif)
{
  uint16 compression, mode;
  TIFFGetField(tif, TIFFTAG_COMPRESSION, &compression);
  if (compression == COMPRESSION_JPEG) TIFFSetField(tif, TIFFTAG_JPEGCOLORMODE, JPEGCOLORMODE_RGB);

  int i,j, v;
  // if (vector_image_p) return load_untiled_tiff_vector_image(tif);
  int bps = element_size;
  int row, y;
  int spp = samplesperpixel;
  int bufsize = TIFFScanlineSize(tif);
  uchar *buf = (uchar *) alloca(bufsize * sizeof(uchar)); // use stack allocation
  image *img;
  int xmid = 3*128;
  
  if (into_image) img = into_image;
  else img = make_image(xdim, ydim, element_type, samplesperpixel);
  // make_band_interleaved_array_image(xdim, ydim, element_type, samplesperpixel);

  tprintf(3, "load_untiled_tiff_image: loading (%d %d %d %d %d) image from %s %d\n.",
	  xdim, ydim, element_size, samplesperpixel, element_type, path, bufsize);
  //fprintf(stderr, "load_untiled_tiff_image created image of element_type=%d samplesperpixel=%d bufsize=%d\n", element_type, spp, bufsize);

  tprintf(5, "load_untiled_tiff_image: Allocated buffer of length %d (xdim*spp = %d).\n.", bufsize, xdim*spp);

  for (row=0,y=ydim-1; row<ydim; row++, y--) {

    tprintf(5, "load_untiled_tiff_image: calling TIFFReadScanLine(tif, buf, %d, 0).\n", row);

    if (TIFFReadScanline(tif, buf, row, 0) == -1) {
      fprintf(stderr, "TIFFReadScanLine(0x%p, 0x%p, %d, 0) returned -1 (error).\n", tif, buf, row);
      return NULL;
    }

    switch (element_type) {
    case IMG_RGB8:
      img->putline((RGB8_PIXEL*) buf, 0, y);
      // fprintf(stderr, "load_untiled_tiff_image RGB8 line=%d, line[%d]=%f\n", row, xmid, buf[xmid]);
      break;
    case IMG_RGBA8: 
      img->putline((RGBA8_PIXEL*) buf, 0, y); 
      break;
    case IMG_SINGLE_FLOAT: 
      // fprintf(stderr, "load_untiled_tiff_image float line=%d, line[%d]=%f\n", row, xmid, dbuf[xmid]);
      packed_putline(img, (float *)buf, 0, y); 
      break;
    case IMG_DOUBLE_FLOAT: packed_putline(img, (double *)buf, 0, y); break;
    case IMG_UNSIGNED_8BIT:
      // Hack for Leica GeoTIFFs:
      packed_putline(img, (uchar *)buf, 0, y);
      break;
    case IMG_UNSIGNED_16BIT: 
      packed_putline(img, (unsigned short *)buf, 0, y); break;
    case IMG_SIGNED_16BIT: 
      packed_putline(img, (short *) buf, 0, y); break;
    case IMG_SIGNED_32BIT: 
    case IMG_UNSIGNED_32BIT: 
      packed_putline(img, (int*) buf, 0, y); break;
      // This is broken - CC - complains about undefined putline from void* buf into unsigned 32bit image:
      // packed_putline(img, (uint *)buf, 0, y); break;
    default: error("load_untiled_tiff_image: illegal bps = %d", bps);
    }
  }
  //fprintf(stderr, "load_untiled_tiff_image putlines done\n");

    // Can't close tiled tiff images, but we should close the untiled images:
  TIFFClose(tif);
  if (use_cmap) {
    int len = 1 << element_size;
    ushort **copy = copy_colormap(cmap, len);
    set_image_colormap(img, copy);
  }
  tprintf(3, "load_untiled_tiff_image: Returning\n");
  return((image *)img);
}
#endif



class tiff_lazy_image_page_handler: public lazy_image_page_handler
{public:
  TIFF* tif;

 // Sequential spare page: Only used when we need to marshal
 // band-sequential RGB images into band-interleaved form:
  unsigned char *spage;
  int nbytes;
  int components;

  tiff_lazy_image_page_handler(TIFF* tif2):lazy_image_page_handler() {tif=tif2;}
  tiff_lazy_image_page_handler(image_page_pool* page_pool, int npages, TIFF* tif2)
    :lazy_image_page_handler(page_pool, npages) {tif=tif2;}
  void tile_builder(int x0, int y0, int x1, int y1, char *page);
//  void tiff_lazy_image_page_handler::write_page(int page_number);  
  void write_page(int page_number);
  virtual int save_image(char* path);
  //virtual OFF_T page_filepos(int page_number);
};


image * tiff_image_header::load_tiff_lazy_image (TIFF* tif, int write_permit)
{
  int bytes_per_page = (block_size*element_size*samplesperpixel)>>3;
  image_page_pool* page_pool = (image_page_pool*) get_page_pool(bytes_per_page, -1);

  fprintf(stderr, "load_tiff_lazy_image element_size=%d, element_type=%d, spp=%d, bytes_per_page=%d\n", element_size, (int)element_type, samplesperpixel, bytes_per_page);
  tiff_lazy_image_page_handler *ph = new tiff_lazy_image_page_handler(page_pool, blocks_wide*blocks_hi, tif);

  ph->components = 1;
  // Spare page for loading tiles component-wise:
  if (planarconfig == PLANARCONFIG_CONTIG) {
    tprintf(8,"tiff_image_header::load_tiff_lazy_image: Contiguous data.\n");
    ph->nbytes = 0;
    ph->spage = NULL;
  } else {
    tprintf(8,"tiff_image_header::load_tiff_lazy_image: band sequential data.  Reading first band only (for now).\n");
    // This seems like a kooky idea.  We should generate a band sequential color image instead.
    ph->nbytes = bytes_per_page;
    ph->spage = XALLOC(unsigned char, ph->nbytes);
    if (element_type == IMG_RGB8) ph->components = 3;
    else if (element_type == IMG_RGBA8) ph->components = 4;
    else ph->components = samplesperpixel;
  }

  ph->set_write_permit(write_permit);
  tprintf(8, "tiff_image_header::load_tiff_lazy_image: page_handler = 0x%x ; page_size = %d\n", ph, page_pool->page_size);
  return ((image *)make_lazy_image(xdim, ydim, element_type, ph, block_xdim, block_ydim));
}



void tiff_lazy_image_page_handler::write_page(int page_number)
{
  image_page_queue_entry* entry = (image_page_queue_entry*)page_map_entry(this, page_number);
  if (write_permit &&(entry->status & PAGE_DIRTY_BIT)) {
    int x0, y0, x1, y1;
    img->image_page_number_to_rectangle(page_number, x0, y0, x1, y1);
    if (TIFFWriteTile(tif, entry->page, x0, img->ydim -1 -y1, 0, 0) == -1)
      error("tiff_lazy_image_page_handler::write_page TIFFWriteTile error");
  }
  entry->status = PAGE_READ_OK_BIT;
  this->set_page_status(page_number, entry->status);
}


// This needs to handle band sequential images:
// This is called by lazy_image_page_handler::read_page
void tiff_lazy_image_page_handler::tile_builder(int x0, int y0, int x1, int y1, char *page)
{
  int i,j;
  // For band sequential RGB images, we need to call this 3 times,
  // once per sample, and shuffle the results back into the given
  // "page".  An alternative to this approach is to read these as
  // separate color components, and communicate that information back
  // to FREEDIUS.  It seems simpler at present to interleave the data
  // on the fly, so that the target representation in FREEDIUS is
  // always band-interleaved.

  tprintf(6,"tiff_lazy_image_page_handler::tile_builder(%d,%d,%d,%d,page) - nbytes = %d, components = %d\n",
	  x0, y0, x1, y1, nbytes, components);
  if (nbytes==0) {
    // Contiguous, so just load it:
    if (TIFFReadTile(tif, page, x0, img->ydim -1 -y1, 0, 0) == -1) {
      printf("tiff_lazy_image_page_handler::tile_builder: x0=%d, img->ydim=%d, y1=%d\n", x0, img->ydim, y1);
      error("tiff_lazy_image_page_handler::tile_builder  TIFFReadTile error");
    }
  } else {
    // nbytes > 0 implies that a spare page has been created for shuffling the data.
    for (j = 0; j < components; j++) {
      int k;
      // Read each component and interleave.  Result is a vector image
      // where each pixel's vector is contiguous:
      if (TIFFReadTile(tif, spage, x0, img->ydim -1 -y1, 0, j) == -1) {
	printf("tiff_lazy_image_page_handler::tile_builder: (%d) x0=%d, img->ydim=%d, y1=%d component 0\n", j, x0, img->ydim, y1);
	error("tiff_lazy_image_page_handler::tile_builder  (%d) TIFFReadTile error", j);
      }
      //for (i = 0; i < nbytes/components; i++) page[i*components+j] = spage[i];
      for (i=0, k=j; j < nbytes; i++, j+=components) page[k] = spage[i];
    }
  }
}


extern int max_array_image_size;

image *tiff_image_header::load_image (int fd, char *path)
{ TIFF* tif = TIFFOpen(path, "rm");
  if (tif == 0) {
    warning("tiff_image_header::load_image cannot open file %s\n", path);
    return(0);
  }
  int xdim, ydim, tiledp, size_in_bytes, large_p;
  uint16 samplesperpixel;
  TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &xdim);
  TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &ydim);
  TIFFGetField(tif, TIFFTAG_SAMPLESPERPIXEL, &samplesperpixel);
  tiledp=TIFFIsTiled(tif);
  // Be careful to avoid overflows for large images.
  // 32 bit int may still be too small.
  size_in_bytes = ((xdim * element_size * samplesperpixel) >> 3) * ydim;
  large_p = size_in_bytes > max_array_image_size;
  tiledp = TIFFIsTiled(tif);
  if (tiledp) {
    if (large_p)
      return load_tiff_lazy_image(tif, 0);
    else return load_tiled_tiff_image(tif);
  } else
    return load_untiled_tiff_image(tif);
}



TIFF *
write_tiff_image_header(char *path, int xdim, int ydim, int pixel_size, int bx, int by,
			int format, int spp, int photometric_type,
			int compression_mode = COMPRESSION_NONE, int jpeg_quality = 75)
{
  tprintf(5,"Writing TIFF image header: path=%s; xdim=%d, ydim=%d\n", path, bx, by);
  TIFF *tif = TIFFOpen(path, "wm");
  if (tif == 0) {
    // warning("tiff_image_header::write_tiff_image_header cannot open file %s", path);
    //return(0);
    error("tiff_image_header::write_tiff_image_header cannot open file %s\n", path);
  }
  TIFFSetField(tif, TIFFTAG_IMAGEWIDTH, xdim);
  TIFFSetField(tif, TIFFTAG_IMAGELENGTH, ydim);
  TIFFSetField(tif, TIFFTAG_SAMPLESPERPIXEL, spp);
  TIFFSetField(tif, TIFFTAG_ORIENTATION, ORIENTATION_TOPLEFT);
#if 0 // TIFF is broken -- TIFFGetField returns arrays for these, but TIFFSetField only accepts scalars
  uint16 pixel_sizes[spp], formats[spp];
  for (int i=0; i<spp; i++) {
    pixel_sizes[i]=pixel_size;
    formats[i]=format;
  }
  TIFFSetField(tif, TIFFTAG_BITSPERSAMPLE, pixel_sizes);
  TIFFSetField(tif, TIFFTAG_SAMPLEFORMAT, formats);
#else
  TIFFSetField(tif, TIFFTAG_BITSPERSAMPLE, pixel_size);
  TIFFSetField(tif, TIFFTAG_SAMPLEFORMAT, format);
#endif
  TIFFSetField(tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
  TIFFSetField(tif, TIFFTAG_ROWSPERSTRIP, 1);
  //  TIFFSetField(tif, TIFFTAG_STRIPBYTECOUNTS, xdim*pixel_size*spp);
  // This next is a blatant assumption that more than 1 sample per
  // pixel implies color, which is not true.
  if (bx != xdim || by != ydim) {
    tprintf(5,"Setting lazy TIFF image block sizes: bx=%d, by=%d\n", bx, by);
    TIFFSetField(tif, TIFFTAG_TILEWIDTH, bx); 
    TIFFSetField(tif, TIFFTAG_TILELENGTH, by);
  }
  TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, photometric_type);
  TIFFSetField(tif, TIFFTAG_COMPRESSION, compression_mode);
  if (compression_mode==COMPRESSION_JPEG)
    TIFFSetField(tif, TIFFTAG_JPEGQUALITY, jpeg_quality);
  return(tif);
}

image * make_tiff_lazy_image (char *path, int xdim, int ydim, IMAGE_ELEMENT_TYPE element_type,
			      int block_xdim, int block_ydim,
			      int format, int spp, int photometric_type,
			      int compression_mode = COMPRESSION_NONE, int jpeg_quality = 75)
{
  int element_size = image_element_size_from_type(element_type);
  int pixel_size = element_size*spp;
  int abs_block_ydim = abs(block_ydim);
  int blocks_wide=(xdim-1)/block_xdim+1;
  int blocks_hi  =(ydim-1)/abs_block_ydim+1;
  int bytes_per_page = (block_xdim * abs_block_ydim * pixel_size)>>3;
  //fprintf(stderr, "make_tiff_lazy_image element_size=%d, spp=%d, bytes_per_page=%d\n", element_size, spp, bytes_per_page);
  image_page_pool* page_pool = (image_page_pool*) get_page_pool(bytes_per_page, -1);
  TIFF *tif = write_tiff_image_header(path, xdim, ydim, pixel_size, block_xdim, abs_block_ydim, 
				      format, spp, photometric_type, compression_mode, jpeg_quality);
  tiff_lazy_image_page_handler *ph = new tiff_lazy_image_page_handler(page_pool, blocks_wide*blocks_hi, tif);
  return ((image *)make_lazy_image(xdim, ydim, element_type, ph, block_xdim, block_ydim));


}

extern "C" {

image* FREEDIUS_GLOBAL(make_tiff_lazy_image) 
  (char *path, int xdim, int ydim, IMAGE_ELEMENT_TYPE element_type,
   int block_xdim, int block_ydim,
   int format, int spp, int photometric_type,
   int compression_mode = COMPRESSION_NONE, int jpeg_quality = 75)
{
  return make_tiff_lazy_image(path, xdim, ydim, element_type, block_xdim, block_ydim,
			      format, spp, photometric_type, compression_mode, jpeg_quality);
}

} // end extern "C"


#if 0
extern
int image_element_properties (IMAGE_ELEMENT_TYPE element_type, 
			      int& spp, int& bps, int &format, int& photometric_type)
{
  format = SAMPLEFORMAT_UINT; photometric_type = PHOTOMETRIC_MINISBLACK; // defaults

  switch (element_type) {
  case IMG_UNSIGNED_8BIT:  case IMG_SIGNED_8BIT:  spp = 1;  bps =   8; break;
  case IMG_UNSIGNED_16BIT: case IMG_SIGNED_16BIT: spp = 1;  bps =  16; break;
  case IMG_UNSIGNED_32BIT: case IMG_SIGNED_32BIT: spp = 1;  bps =  32; break;
  case IMG_SINGLE_FLOAT:                          spp = 1;  bps =  32; break;
  case IMG_DOUBLE_FLOAT:                          spp = 1;  bps =  64; break;
  case IMG_RGB8: photometric_type=PHOTOMETRIC_RGB; spp = 3;  bps =   8; break;
  case IMG_RGBA8:photometric_type=PHOTOMETRIC_RGB; spp = 4;  bps =   8; break;
  default:  error("image_element_properties : I really don't know what to do with this element_type: %d",
		  element_type);
  }

  switch (element_type) {
  case IMG_SIGNED_1BIT: case IMG_SIGNED_2BIT: case IMG_SIGNED_4BIT:
  case IMG_SIGNED_8BIT: case IMG_SIGNED_16BIT: case IMG_SIGNED_32BIT:
    format = SAMPLEFORMAT_INT; 
    break;
  case IMG_SINGLE_FLOAT: case IMG_DOUBLE_FLOAT:
    format = SAMPLEFORMAT_IEEEFP; 
    break;
  default:
    format = SAMPLEFORMAT_VOID; 
    break;
  }
  return 1; // eventually want to return 1=win, 0=lose
}
#endif


#if 0 // old
extern int
save_untiled_tiff_image(image *img, string path, int error_ok, int compression_mode, int jpeg_quality) 
{
  int i, j, k, y, bps, spp, bufsize, format, photometric_type;
  uint16 **cmap;
  TIFF *tif;
  IMAGE_ELEMENT_TYPE element_type = image_element_type(img);
  
  image_element_properties(element_type, spp, bps, format, photometric_type);

  cmap = (uint16**) get_image_colormap(img);

  if (spp == 1 && cmap) photometric_type = PHOTOMETRIC_PALETTE;

  tif = write_tiff_image_header(path, img->xdim, img->ydim, bps, img->xdim, img->ydim, format,
				spp, photometric_type, compression_mode, jpeg_quality);

  if (compression_mode == COMPRESSION_JPEG) {
    TIFFSetField(tif, TIFFTAG_ROWSPERSTRIP, 8);
  }

  if (cmap) {
    TIFFSetField(tif, TIFFTAG_COLORMAP, cmap[0], cmap[1], cmap[2]);
  }

  if (format == SAMPLEFORMAT_IEEEFP) {
    if (element_type == IMG_SINGLE_FLOAT)
      bufsize = img->xdim * sizeof(float);
    else bufsize = img->xdim * sizeof(double);
  }
  else bufsize = img->xdim * sizeof(int);

  int obufsize = TIFFScanlineSize(tif);

  // int ibuf[bufsize];
  int *ibuf = XALLOC(int, bufsize);
  double *dibuf = (double*) ibuf;
  float *fibuf = (float*) ibuf;

  // char obuf[obufsize];
  char *obuf =  XALLOC(char, obufsize);
  short *sobuf = (short*) obuf;
  int *iobuf = (int*) obuf;
  double *dobuf = (double*) obuf;
  float *fobuf = (float*) obuf;

  // y = img->ydim - 1;
  int xdim = img->xdim;
  for (i = img->ydim - 1; i >= 0; i--) {
    if (spp == 1) {
      if (element_type == IMG_DOUBLE_FLOAT) {
	  img->getline(dibuf, 0, i);
	  array_copy(dibuf, dobuf, xdim); // not sure this copy is needed.
      } else if (element_type == IMG_SINGLE_FLOAT) {
	img->getline(fibuf, 0, i);
	array_copy(fibuf, fobuf, xdim); // not sure this copy is needed.
      } else {
	img->getline(ibuf, 0, i);
	if (bps == 8)       array_copy(ibuf, obuf, xdim);
	else if (bps == 16) array_copy(ibuf, sobuf, xdim);
	else if (bps == 32) array_copy(ibuf, iobuf, xdim);
        else error("save_untiled_tiff_image: illegal bps = %d", bps);
      }
    }
    // Color images
    else {
      switch (element_type) {
      case IMG_RGB8:
	img->getline((RGB8_PIXEL*) obuf, 0, i);
	break;
      case IMG_RGBA8:
	img->getline((RGBA8_PIXEL*) obuf, 0, i);
	break;
      }
    }
  
    if (TIFFWriteScanline(tif, obuf, (img->ydim - i) - 1, 0) == -1) {
      error("save_untiled_tiff_image: Bad oops at line %d.", i);
    }
  }
  xxfree(ibuf); xxfree(obuf);
  TIFFClose(tif);
  return 1;
}

#elseif 1

extern int
save_untiled_tiff_image(image *img, string path, int error_ok, int compression_mode, int jpeg_quality) 
{
  int i, j, k, y, bps, spp, bufsize, format, photometric_type;
  uint16 **cmap;
  TIFF *tif;
  IMAGE_ELEMENT_TYPE element_type = image_element_type(img);
  
  image_element_properties(element_type, spp, bps, format, photometric_type);

  cmap = (uint16**) get_image_colormap(img);

  if (spp == 1 && cmap) photometric_type = PHOTOMETRIC_PALETTE;

  tif = write_tiff_image_header(path, img->xdim, img->ydim, bps, img->xdim, img->ydim, format,
				spp, photometric_type, compression_mode, jpeg_quality);

  if (compression_mode == COMPRESSION_JPEG) {
    TIFFSetField(tif, TIFFTAG_ROWSPERSTRIP, 8);
  }

  if (cmap) {
    TIFFSetField(tif, TIFFTAG_COLORMAP, cmap[0], cmap[1], cmap[2]);
  }

  bufsize = (img->xdim * spp * bps) >>3; // buffer size in bytes

  // int ibuf[bufsize];
  //uchar *buf = XALLOC(uchar, bufsize);
  uchar *buf = (uchar *) alloca(bufsize * sizeof(uchar)); // use stack allocation
  
  // y = img->ydim - 1;
  int xdim = img->xdim;
  for (i = img->ydim - 1; i >= 0; i--) {
    switch (element_type) {
    case IMG_UNSIGNED_8BIT:  case IMG_SIGNED_8BIT:
      img->getline((uchar *)buf, 0, i); break;
    case IMG_UNSIGNED_16BIT:  
      img->getline((ushort *)buf, 0, i); break;
    case IMG_SIGNED_16BIT:
      img->getline((short *)buf, 0, i); break;
    case IMG_UNSIGNED_32BIT:  case IMG_SIGNED_32BIT:
      img->getline((int *)buf, 0, i); break;
    case IMG_SINGLE_FLOAT:
      img->getline((float *)buf, 0, i); break;
    case IMG_DOUBLE_FLOAT:
      img->getline((double *)buf, 0, i); break;
    case IMG_RGB8:
      img->getline((RGB8_PIXEL *)buf, 0, i); break;
    case IMG_RGBA8:
      img->getline((RGB8_PIXEL *)buf, 0, i); break;
    default: error("save_untiled_tiff_image bad element type%d", element_type);
    }
    if (TIFFWriteScanline(tif, buf, (img->ydim - i) - 1, 0) == -1) {
      error("save_untiled_tiff_image: Bad oops at line %d.", i);
    }
  }
  //xxfree(buf);
  TIFFClose(tif);
  return 1;
}


#else // old

#if 0

extern "C" {
  void FREEDIUS_brk (const char *str)
{
  int i;
  i=1;
}

} // end extern "C"

#endif

template <class T>
int copy_to_untiled_tiff(image *img, TIFF *tif, int buf_nsamples, T *foo)
{
  T *buf = XALLOC(T, buf_nsamples);
  int ydim = img->ydim;
  for (int y = ydim - 1; y >= 0; y--) {
    //FREEDIUS_brk("copy_to_untiled_tiff");
    img->getline(buf, 0, y);
    if (TIFFWriteScanline(tif, (char *)buf, ydim - y - 1, 0) == -1) 
      error("save_untiled_tiff_image: Bad oops at line %d.", y);
  }
  xxfree(buf);
  return 1;
}


extern int
save_untiled_tiff_image(image *img, string path, int error_ok, int compression_mode, int jpeg_quality) 
{
  int i, j, k, y, bps, spp, bufsize, format, photometric_type;
  uint16 **cmap;
  TIFF *tif;
  IMAGE_ELEMENT_TYPE element_type = image_element_type(img);
  
  image_element_properties(element_type, spp, bps, format, photometric_type);

  cmap = (uint16**) get_image_colormap(img);

  if (spp == 1 && cmap) photometric_type = PHOTOMETRIC_PALETTE;

  tif = write_tiff_image_header(path, img->xdim, img->ydim, bps, img->xdim, img->ydim, format,
				spp, photometric_type, compression_mode, jpeg_quality);

  if (compression_mode == COMPRESSION_JPEG) {
    TIFFSetField(tif, TIFFTAG_ROWSPERSTRIP, 8);
  }

  if (cmap) {
    TIFFSetField(tif, TIFFTAG_COLORMAP, cmap[0], cmap[1], cmap[2]);
  }

  bufsize = (img->xdim * spp * bps) >> 3;
  // sizes in bytes
  int obufsize = TIFFScanlineSize(tif);
  
  if (bufsize != obufsize) error("bufsize(%d) != obufsize(%d)", bufsize, obufsize);
  int buf_nsamps = img->xdim * spp;
  
  switch (element_type) {
  case IMG_UNSIGNED_8BIT:  case IMG_SIGNED_8BIT:
    copy_to_untiled_tiff(img, tif, buf_nsamps, (uchar *)0); break;
  case IMG_UNSIGNED_16BIT:  case IMG_SIGNED_16BIT:
    copy_to_untiled_tiff(img, tif, buf_nsamps, (ushort *)0); break;
  case IMG_UNSIGNED_32BIT:  case IMG_SIGNED_32BIT:
    copy_to_untiled_tiff(img, tif, buf_nsamps, (int *)0); break;
  case IMG_SINGLE_FLOAT:
    copy_to_untiled_tiff(img, tif, buf_nsamps, (float *)0); break;
  case IMG_DOUBLE_FLOAT:
    copy_to_untiled_tiff(img, tif, buf_nsamps, (double *)0); break;
  case IMG_RGB8:
    copy_to_untiled_tiff(img, tif, buf_nsamps, (RGB8_PIXEL *)0); break;
  case IMG_RGBA8:
    copy_to_untiled_tiff(img, tif, buf_nsamps, (RGBA8_PIXEL *)0); break;
  default: error("save_untiled_tiff_image bad element type%d", element_type);
  }
  
  TIFFClose(tif);
  return 1;
}

#endif 



#ifdef WINDOWS
extern "C" {
char *strdup(const char *str);
}
#endif

int
tiff_lazy_image_page_handler::save_image (char *path)
{
  const char *pathname = strdup(TIFFFileName(tif));
  if (write_permit) {
    
    flush_map(this);
    TIFFClose(tif);
    if (strcmp(path, pathname) != 0) {
      warning("tiff_lazy_image_page_handler::save_image cannot currently rename tif image files.\n");
    }
    set_write_permit(0);
    tif = TIFFOpen(pathname, "rm");
    if (tif == 0) 
      error("tiff_image_header::save_image failed to reopen tif file %s", path);
    return(0);
  }
  else { // saving an existing read-only tif image file just means copying it.
    if (strcmp(path, pathname) != 0) {
      warning("tiff_lazy_image_page_handler::save_image is copying %s to %s\n", pathname, path);
      copy_file(pathname, path);
    }
    return(0);
  }  
}


tiff_image_header *tiff_image_header1 = 0;

#include <stdarg.h>

void tiff_image_header_error_handler(const char* module, const char* fmt, va_list ap) {
  printf("TIFF Error, Module %s:\n", module);
  vprintf(fmt, ap);
}


int init_tiff_array_image_header(void)
{
  if (! tiff_image_header1) tiff_image_header1 = new tiff_image_header;
  TIFFSetErrorHandler(tiff_image_header_error_handler);
  add_image_header(tiff_image_header1);
  return(1);
}


extern "C" {

TIFF* FREEDIUS_GLOBAL(get_tif) (paged_image_base *image)
{
  return ((tiff_lazy_image_page_handler *)image->page_handler)->tif;
}

void FREEDIUS_GLOBAL(save_untiled_tiff_image)(image *img, string path, int error_ok, 
					      int compression_mode, int jpeg_quality) {
  save_untiled_tiff_image(img, path, error_ok, compression_mode, jpeg_quality);
}

} // end extern "C"


/*****************  tiff_file_image_page_handler  ****************************/

// New Thu Apr 15 2004

#include "file-image.h"

class tiff_file_image_page_handler: public file_image_page_handler
{public:
  OFF_T *tile_offsets;
  tiff_file_image_page_handler(image_page_pool* page_pool, int npages)
    :file_image_page_handler(page_pool, npages){
    //tile_offsets = new off_t[npages];
    tile_offsets = (OFF_T *) zalloc(npages*sizeof(OFF_T));
  }
  virtual OFF_T page_filepos(int page_number);
  // virtual int map_image_to_file (paged_image_base *img, int iomode, string pathname, int header_length);
};

OFF_T
tiff_file_image_page_handler::page_filepos(int page_number)
{
  return(tile_offsets[page_number]);
}

#if 0
int
tiff_file_image_page_handler::map_image_to_file (paged_image_base *img, int iomode, string pathname, int header_length)
{
  TIFF* tif = TIFFOpen(pathname, "rm");
  read_header(tif);
  TIFFGetField(tif, TIFFTAG_TILEOFFSETS, )
 // This is really the only part relevent to file mapping.
    set_write_permit(!RDONLY_P(iomode));
  stream = general_open(pathname, iomode);
  if (stream < 0)
    error("map_image_to_file: the file %s does not exist.", pathname);
  this->pathname = pathname;
  return(0);
}
#endif

int tiff_file_image_default_block_xdim = 256;
int tiff_file_image_default_block_ydim = -256;

string allocate_file_image_pathname();
int map_file_image_to_file (paged_image_base *img, int iomode, string pathname, int header_length);

typedef file_image_page_handler* (file_image_page_handler_maker) (image_page_pool *page_pool, int npages);
void make_page_handler (paged_image_base *img, int iomode, file_image_page_handler_maker *ph_maker = 0);

file_image_page_handler* 
create_tiff_file_image_page_handler(image_page_pool *page_pool, int npages)
{
  return new tiff_file_image_page_handler(page_pool, npages);
}

extern paged_image_base *
make_tiff_file_image (int xdim, int ydim,
		      IMAGE_ELEMENT_TYPE element_type,
		      int samples_per_pixel,
		      int block_xdim, int block_ydim, int padded_block_xdim,
		      char *pathname)
{
  if (block_xdim == 0) block_xdim = tiff_file_image_default_block_xdim;
  if (block_ydim == 0) block_ydim = tiff_file_image_default_block_ydim;
  if (padded_block_xdim == 0) padded_block_xdim = block_xdim;
  int abs_block_ydim = abs(block_ydim);
  int blocks_wide=(xdim-1)/block_xdim+1;
  int blocks_hi  =(ydim-1)/abs_block_ydim+1;
  int npages = blocks_wide * blocks_hi;
  if (! pathname) 
    pathname = allocate_file_image_pathname();
  else pathname = strdup(pathname);

  int spp, element_size, format, photometric_type;
  image_element_properties(element_type, spp, element_size, format, photometric_type);
  spp = spp * samples_per_pixel;

  TIFF *tif = write_tiff_image_header(pathname, xdim, ydim, element_size, block_xdim, abs_block_ydim, 
				      format, spp, photometric_type, COMPRESSION_NONE, 100);

  int bits_per_pixel = element_size*spp;
  int bytes_per_tile = (block_xdim * abs_block_ydim * bits_per_pixel)>>3;
  //char* tile = new char[bytes_per_tile];
  char* tile = (char*) alloca(bytes_per_tile * sizeof(char));
  for (int y=0; y<ydim; y+=abs_block_ydim) 
    for (int x=0; x<xdim; x+=block_xdim)
      // YUK -- writes all of the tiles immediately, but this is neccessary in order for tile_offsets
      // to be defined.
      TIFFWriteTile(tif, tile, x, ydim -1 -y, 0, 0);  
  //free(tile);

  paged_image_base *img = make_paged_image(xdim, ydim, element_type, spp, block_xdim, block_ydim, padded_block_xdim);
  if (! img) error("make_paged_image failed");

  make_page_handler(img, O_RDWR, *create_tiff_file_image_page_handler);

  TIFFClose(tif);

  // read the TIFFTAG_TILEOFFSETS into tiff_file_image_page_handler tile_offsets
  {
    tif = TIFFOpen(pathname, "rm");
    // bigtiff supports int64 tile_offsets, ordinary tiff has int32 tile_offsets
    // toff_t (defined in tiffio.h) is the right declaration to use.
    toff_t *tile_offsets;  
    OFF_T *ph_tile_offsets = ((tiff_file_image_page_handler*) img->page_handler)->tile_offsets;

    TIFFGetFieldDefaulted(tif, TIFFTAG_TILEOFFSETS, &tile_offsets);
    for (int i=0; i<npages; i++)
      ph_tile_offsets[i] = tile_offsets[i];

    TIFFClose(tif);
  }
   
  map_file_image_to_file(img, O_RDWR, pathname, 0);

  return(img);
}

extern "C" {

cimage *
FREEDIUS_GLOBAL(make_tiff_file_image) (int xdim, int ydim,
				       IMAGE_ELEMENT_TYPE element_type,
				       int samples_per_pixel,
				       int block_xdim, int block_ydim, int padded_block_xdim,
				       char *pathname)
{
  return (cimage *)make_tiff_file_image(xdim, ydim, element_type, samples_per_pixel, 
					block_xdim, block_ydim, padded_block_xdim, pathname);
}


} // end extern "C"



END_NAMESPACE_FREEDIUS
