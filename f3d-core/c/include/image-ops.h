#ifndef __image_ops_h
#define __image_ops_h

#include "image.h"

#include "misc.h"
#include "cme-error.h"
#include "minmax.h"
#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

typedef unsigned int uint32;
typedef unsigned char uchar;
typedef unsigned int uint;

extern image * fast_gauss_convolve(image *img, image *into, int level, double ka=.375, image* spare=0);
extern image * gauss_convolve_decimate (image *img, double ka = .375, image *into_image=0);
extern image *gauss_convolve_image(image *img, double ka = .375, image *into_image = 0, int level = 1, image *spare = 0);
//extern void build_image_pyramid (image *img, char *base_path, int to_level, int from_level=0);
//extern image * copy_image (image *img, image *to_image=NULL);

RGBA8_PIXEL *luminance_to_rgba_map();
RGBA8_PIXEL* luminance_to_rgba(int tile_size, uchar *lum, RGBA8_PIXEL *rgba=0);
RGB8_PIXEL* luminance_to_rgb(int n, uchar *lum, RGB8_PIXEL *rgb=0);
uchar* luminance_subimage_to_luminance(image *img, int x0, int y0, int nx, int ny,
						    uchar *tile_array,
						    int tile_width, int tile_height);

uchar* luminance_subimage_linearly_mapped_to_luminance(image *img, int x0, int y0, int nx, int ny,
						       uchar *tile_array,
						       int tile_width, int tile_height,
						       float src_range, float dest_range, 
						       float default_scale, float default_offset);
RGBA8_PIXEL* luminance_subimage_to_rgba(image *img, int x0, int y0, int nx, int ny,
					       RGBA8_PIXEL *tile_array,
					       int tile_width, int tile_height, image *alpha=0);
RGB8_PIXEL* luminance_subimage_to_rgb(image *img, int x0, int y0, int nx, int ny,
					      RGB8_PIXEL *tile_array,
					      int tile_width, int tile_height);
RGBA8_PIXEL* rgba_subimage_to_rgba(image *img, int x0, int y0, int nx, int ny,
				   RGBA8_PIXEL *tile_array,
				   int tile_width, int tile_height);
RGB8_PIXEL* rgb_subimage_to_rgb(image *img, int x0, int y0, int nx, int ny,
					RGB8_PIXEL *tile_array,
					int tile_width, int tile_height);

ushort* luminance_to_rgb5_a1(int n, uchar *lum, ushort *rgba=0);
ushort* rgba_to_rgb5_a1(int n, uchar *rgba, ushort *rgb5a1=0);
ushort* rgb_to_rgb5_a1(int n, uchar *rgb, ushort *rgb5a1=0);
ushort* luminance_subimage_to_rgb5_a1(image *img, int x0, int y0, int nx, int ny,
				     ushort *tile_array,
				     int tile_width, int tile_height);

extern image *box_filter_decimate2 (image *img, image *into_image);

template <class eltype>
eltype *
box_filter_decimate2_tile (eltype* tile, eltype* outtile, int bx, int by)
{
  int bx2 = bx>>1;
  int by2 = by>>1;
 
  if (! outtile) outtile = XALLOC(eltype, bx2*by2);
  eltype* row0= tile;
  eltype* row1= tile+bx;
  eltype* outrow = outtile;
  int x, y, x2, y2;
  for (y=0, y2=0; y2<by2; y2++, y+=2) {
    for (x=0, x2=0; x2<bx2; x2++, x+=2) 
     outrow[x2] = (row0[x]+row0[x+1]+row1[x]+row1[x+1]) >> 2;
    row0+=2*bx; row1+=2*bx; outrow+=bx2;
  }
  return outtile;
}

template <class eltype>
eltype *
box_filter_decimate2_rgb_tile (eltype* tile, eltype* outtile, int bx, int by)
{
  int bx2 = bx>>1;
  int by2 = by>>1;
 
  eltype* row0= tile;
  eltype* row1= row0+bx;
  eltype* outrow;
  int x, y, x2, y2, x3;

  if (! outtile) outtile = XALLOC(eltype, bx2*by2);
  outrow = outtile;
  for (y=0, y2=0; y<by2; y++, y2+=2 ) {
    for (x=0, x2=0; x<bx2; x++, x2+=2) {    
      outrow[x].r = (row0[x2].r + row0[x2+1].r + row1[x2].r + row1[x2+1].r) >>2;
      outrow[x].g = (row0[x2].g + row0[x2+1].g + row1[x2].g + row1[x2+1].g) >>2;
      outrow[x].b = (row0[x2].b + row0[x2+1].b + row1[x2].b + row1[x2+1].b) >>2;
    }
    row0+=2*bx; row1+=2*bx; outrow+=bx2;
  }
  //fprintf(stderr, "exit box_filter_decimate2_rgba_map_tile\n");
  return outtile;
}

template <class eltype>
eltype *
extract_subimage(image *image, int x0, int y0, int nx, int ny, 
		 eltype *array, int tile_width = 0, int tile_height = 0)
{
  int y;
  int arraypos = 0;
  if (tile_width == 0) tile_width = nx;
  if (tile_height == 0) tile_height = ny;
  if (! array)
    array = XALLOC(eltype, tile_width*tile_height);

  for (y = y0+ny-1; y >= y0; y--, arraypos+=tile_width)
    /* This next is generally wrong -- only works for uchar images */
     
    if (image->getline (array, x0, y, nx, arraypos) < 0)
      error ("getline failed for line ~d\n", y);
     
  return array;
}


// Handle 16-bit images -- this version has two extra args, the shift
// and offset for converting a 16-bit image to an 8-bit luminance.
// With clipping if necessary.  Only problem here is that I'm not
// sure what all eltype can instantiate to, so types that are > 8 bits
// should probably be treated differently, but what the hell....

#if 1
template <class eltype>
eltype *
extract_linearly_mapped_subimage(image *image, int x0, int y0, int nx, int ny, 
		       eltype *array, int tile_width = 0, int tile_height = 0, 
		       int dest_range = 0, float scale=1/256, float offset=0)
{
  int y,j;
  int arraypos = 0;
  if (tile_width == 0) tile_width = nx;
  if (tile_height == 0) tile_height = ny;
  if (! array)
    array = XALLOC(eltype,  tile_width*tile_height);
  
  double *buf = XALLOC(double, nx);

  tprintf(5, "extract_linearly_mapped_subimage dest_range=%d; scale=%f; offset=%f\n", dest_range, scale, offset);

  for (y = y0+ny-1; y >= y0; y--, arraypos+=tile_width) {
    if (image->getline (buf, x0, y, nx) < 0)
      error ("getline failed for line ~d\n", y);
    for (j = 0; j < nx; j++) 
      array[arraypos+j] = (eltype) MAX(0, MIN(dest_range-1, (int)((buf[j]*scale) + offset)));
  }  
  xxfree(buf);
     
  return array;
}
#else
template <class eltype>
eltype *
extract_linearly_mapped_subimage(image *image, int x0, int y0, int nx, int ny, 
		       eltype *array, int tile_width = 0, int tile_height = 0, 
		       int dest_range=256, float scale=1/256, float offset=0)
{
  int y,j;
  int arraypos = 0;
  if (tile_width == 0) tile_width = nx;
  if (tile_height == 0) tile_height = ny;
  if (! array)
    array = XALLOC(eltype,  tile_width*tile_height);
  
  double *buf = XALLOC(double, nx);

  tprintf(5, "extract_linearly_mapped_subimage dest_range=%d; scale=%f; offset=%f\n", dest_range, scale, offset);
  scale = 0.1;

  for (y = y0+ny-1; y >= y0; y--, arraypos+=tile_width) {
    if (image->getline (buf, x0, y, nx) < 0)
      error ("getline failed for line ~d\n", y);
    for (j = 0; j < nx; j++) 
      array[arraypos+j] = (uchar) MAX(0, MIN(dest_range-1, (int)((buf[j]*scale) + offset)));
  }  
  xxfree(buf);
     
  return array;
}
#endif

template <class eltype>
static eltype*
extract_rgb_subimage(image *image, int x0, int y0, int nx, int ny, 
		     eltype* array, int tile_width = 0, int tile_height = 0)
{
  int k, r, g, b;
  int wrd;
  int y;
  int arraypos = 0;
  int ystart = y = y0+ny-1;
  if (tile_width == 0) tile_width = nx;
  if (tile_height == 0) tile_height = ny;
  if (! array)
    array = XALLOC(eltype, tile_width*tile_height);
  if (y0 < 0) y0 = 0;
  for (y = ystart; y >= y0; y--, arraypos+=tile_width) {
    if (image->getline (array, x0, y, nx, arraypos) < 0)
      error ("getline failed for line ~d\n", y);
  }

#ifndef DARWIN
#ifdef WORDS_BIGENDIAN
  flip_rgb8_endian(array, tile_width*tile_height);
#endif
#endif
 
  return array;
}

#define UNROLL_LOOPS

#if !defined(UNROLL_LOOPS)
// The calls to galloc and free here SUCKS.  
template <class ARRAY_TYPE, class BUFFER_TYPE>
static void *
extract_subimage_mapped (image *image, int x0, int y0, int nx, int ny, 
			 ARRAY_TYPE *array,
			 BUFFER_TYPE *buf,
			 ARRAY_TYPE *map,
			 int tile_width = 0, int tile_height = 0)
{
  int x, y;
  int arraypos = 0;
  int create_buf = (! buf);

  if (tile_width == 0) tile_width = nx;
  if (tile_height == 0) tile_height = ny;
  if (! array)
    array = XALLOC(ARRAY_TYPE, tile_width*tile_height);
  if (create_buf) 
    buf = XALLOC(BUFFER_TYPE, nx);
  
  for (y = y0+ny-1; y >= y0; y--, arraypos+=tile_width) {
    //    if (image->getline(buf, x0, y, nx) < 0)
    if (image->getline(buf, x0, y, nx, 0,1,0) < 0)
      error ("getline failed for line %d\n", y);
    
    for (x = 0; x < nx; x++) 
      array[arraypos+x] = map[buf[x]];
   }
  if (create_buf) xxfree(buf);
  return array;
}
#else // loop unrolled
// The calls to galloc and free here SUCKS.  
template <class ARRAY_TYPE, class BUFFER_TYPE>
static void *
extract_subimage_mapped (image *image, int x0, int y0, int nx, int ny, 
			 ARRAY_TYPE *array,
			 BUFFER_TYPE *buf,
			 ARRAY_TYPE *map,
			 int tile_width = 0, int tile_height = 0)
{
  int x, y;
  int arraypos = 0;
  ARRAY_TYPE *ap;
  int create_buf = (! buf);

  if (tile_width == 0) tile_width = nx;
  if (tile_height == 0) tile_height = ny;
  if (! array)
    array = XALLOC(ARRAY_TYPE, tile_width*tile_height);
  if (create_buf) 
    buf = XALLOC(BUFFER_TYPE, nx);
  
  for (y = y0+ny-1; y >= y0; y--, arraypos+=tile_width) {
    //    if (image->getline(buf, x0, y, nx) < 0)
    if (image->getline(buf, x0, y, nx, 0,1,0) < 0)
      error ("getline failed for line %d\n", y);
    
    ap = array+arraypos;
    for (x = 0; x < nx-3; x+=4) {
      ap[x]   = map[buf[x]];
      ap[x+1] = map[buf[x+1]];
      ap[x+2] = map[buf[x+2]];
      ap[x+3] = map[buf[x+3]];
    }
    for (; x < nx; x++)
      ap[x] = map[buf[x]];
  }
  if (create_buf) xxfree(buf);
  return array;
}
#endif

static void * fill_subimage_alpha (image *alpha, int x0, int y0, int nx, int ny,
				   RGBA8_PIXEL *array,
				   int tile_width = 0, int tile_height = 0)
{
  // alpha MUST be an unsigned 8bit image containing alpha channel values.
  // int buf[nx];
  int *buf = XALLOC(int, nx);
  int x, y;
  int arraypos = 0;
 
  if (tile_width == 0) tile_width = nx;
  if (tile_height == 0) tile_height = ny;
  
  for (y = y0+ny-1; y >= y0; y--, arraypos+=tile_width) {
    if (alpha->getline(buf, x0, y, nx) < 0)
      error ("getline failed for line %d\n", y);
    
    for (x = 0; x < nx; x++)
      array[arraypos+x].a = (0xFF & buf[x]);
  }
  xxfree(buf);
  return array;
}

END_NAMESPACE_FREEDIUS

#endif /* ! __image_ops_h */ 
