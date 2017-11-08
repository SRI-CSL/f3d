#ifdef __GNUG__
#pragma implementation
#endif

//#include <stdlib.h>
#include "image.h"
#include "misc.h"
#include "cme-error.h"
#include "tiff-io.h"
#include <stdio.h>

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

IMAGE_ELEMENT_TYPE image::element_type() {return IMG_GENERAL_TYPE;} // FIXME

void describe_image (blocked_mapped_image *img)
{
  warning("describe_image: dims:(%d,%d) bps:%d blk-dims:(%d,%d) %p",
	  img->xdim, img->ydim, image_element_size(img), img->block_xdim, img->block_ydim, img);
}


void * get_image_tile (blocked_mapped_image *img, int x, int y)
{
  return img->get_image_tile(x, y);
}


setClass(image, Object);

// This is unfinished and only works for scalar images
extern
IMAGE_ELEMENT_TYPE image_element_type_from_size(int element_size, int signed_p,
						int float_p)
{
  if (! float_p) {
    if (! signed_p) {
      switch (element_size)
	{case 8: return(IMG_UNSIGNED_8BIT); 
	case 16: return(IMG_UNSIGNED_16BIT); 
	case 32: return(IMG_UNSIGNED_32BIT); 
	}
    } else  // signed_p
      switch (element_size)
	{case 8: return(IMG_SIGNED_8BIT); 
	case 16: return(IMG_SIGNED_16BIT); 
	case 32: return(IMG_SIGNED_32BIT); 
	}
  } else // float_p 
    switch (element_size)
      {case 64: return(IMG_DOUBLE_FLOAT); 
      case 32: return(IMG_SINGLE_FLOAT); 
      }
  return((IMAGE_ELEMENT_TYPE) -1);
}

extern
int image_element_size_from_type(IMAGE_ELEMENT_TYPE element_type)
{switch (element_type) {
  case IMG_UNSIGNED_1BIT: return(1);
  case IMG_UNSIGNED_2BIT: return(2);
  case IMG_UNSIGNED_4BIT: return(4);
  case IMG_UNSIGNED_8BIT: return(8);
  case IMG_UNSIGNED_16BIT: return(16);
  case IMG_UNSIGNED_32BIT: return(32);
  case IMG_SIGNED_1BIT: return(1);
  case IMG_SIGNED_2BIT: return(2);
  case IMG_SIGNED_4BIT: return(4);
  case IMG_SIGNED_8BIT: return(8);
  case IMG_SIGNED_16BIT: return(16);
  case IMG_SIGNED_32BIT: return(32);
  case IMG_SINGLE_FLOAT: return(32);
  case IMG_DOUBLE_FLOAT: return(64);
  case IMG_GENERAL_TYPE: return(32);
  case IMG_RGB8: return(24);
  case IMG_RGBA8: return(32);
    //  case IMG_RGB16: return(48);
    //case IMG_RGBA16: return(64);
  default: return(-1);
  }}

extern
const char* image_element_type_name (IMAGE_ELEMENT_TYPE element_type)
{
  switch(element_type) {
  case IMG_UNSIGNED_1BIT: return("IMG_UNSIGNED_1BIT");
  case IMG_UNSIGNED_2BIT: return("IMG_UNSIGNED_2BIT");
  case IMG_UNSIGNED_4BIT: return("IMG_UNSIGNED_4BIT");
  case IMG_UNSIGNED_8BIT: return("IMG_UNSIGNED_8BIT");
  case IMG_UNSIGNED_16BIT: return("IMG_UNSIGNED_16BIT");
  case IMG_UNSIGNED_32BIT: return("IMG_UNSIGNED_32BIT");
  case IMG_SIGNED_1BIT: return("IMG_SIGNED_1BIT");
  case IMG_SIGNED_2BIT: return("IMG_SIGNED_2BIT");
  case IMG_SIGNED_4BIT: return("IMG_SIGNED_4BIT");
  case IMG_SIGNED_8BIT: return("IMG_SIGNED_8BIT");
  case IMG_SIGNED_16BIT: return("IMG_SIGNED_16BIT");
  case IMG_SIGNED_32BIT: return("IMG_SIGNED_32BIT");
  case IMG_SINGLE_FLOAT: return("IMG_SINGLE_FLOAT"); 
  case IMG_DOUBLE_FLOAT: return("IMG_DOUBLE_FLOAT"); 
  case IMG_RGB8 : return("IMG_RGB8"); 
  case IMG_RGBA8 : return("IMG_RGBA8"); 
    //case IMG_RGB16 : return("IMG_RGB16"); 
    //case IMG_RGBA16 : return("IMG_RGBA16"); 
  case IMG_GENERAL_TYPE : return("IMG_GENERAL_TYPE"); 
  default: return ("IMG_UNKNOWN_TYPE");
 }
}

extern
const char* image_class_name (IMAGE_CLASS cls)
{
  switch (cls) {
  case ARRAY_IMAGE_CLASS: return "array_image"; 
  case FILE_IMAGE_CLASS: return "file_image";
  case LAZY_IMAGE_CLASS: return "lazy_image"; 
  default:  return "unknown_image_class";
  }
}


extern
int image_element_properties (IMAGE_ELEMENT_TYPE element_type, 
			      int& spp, int& bps, 
			      int& format, 
			      int& photometric_type)
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
    //  case IMG_RGB16: photometric_type=PHOTOMETRIC_RGB; spp = 3;  bps =   16; break;
    //case IMG_RGBA16:photometric_type=PHOTOMETRIC_RGB; spp = 4;  bps =   16; break;
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
  }
  return 1; // eventually want to return 1=win, 0=lose
}



int generic_image_method_error (image *img, const char *fn_sig)
{
  error("%s undefined for %s<%s>", 
	fn_sig,
	image_class_name (img->class_code()),
	image_element_type_name(image_element_type(img))); 
   return(1);
}

double 
image::interpolate_iref(double x, double y)
{generic_image_method_error(this,"interpolate_iref"); return 0;}

int image::iref(int x, int y)
{return generic_image_method_error(this, "iref");}

void image::iset(int x, int y, int val)
{generic_image_method_error(this, "iset(int)");}

double image::diref(int x, int y)
{generic_image_method_error(this, "diref"); return 0;}

void image::iset(int x, int y, double val)
  {generic_image_method_error(this, "iset(double)");}

int image::getline (int *buf, int x, int y, int n, int to_index, int dx, int band)
{return generic_image_method_error(this, "getline (int*)");}

int image::getline (double *buf, int x, int y, int n, int to_index, int dx, int band)
{return generic_image_method_error(this, "getline (double*)");}

int image::getline (uchar *buf, int x, int y, int n, int to_index, int dx, int band)
{return generic_image_method_error(this, "getline_uchar (uchar*)");}

int image::getline (ushort *buf, int x, int y, int n, int to_index, int dx, int band)
{return generic_image_method_error(this, "getline_ushort (ushort*)");}

int image::getline (short *buf, int x, int y, int n, int to_index, int dx, int band)
{return generic_image_method_error(this, "getline_short (short*)");}

int image::getline (float *buf, int x, int y, int n, int to_index, int dx, int band)
{return generic_image_method_error(this, "getline_float (float*)");}

int image::getline (void *buf, int x, int y, int n, int to_index, int dx, int band)
{return generic_image_method_error(this, "getline (void*)");}

int image::putline (int *buf, int x, int y, int n, int to_index, int band)
{return generic_image_method_error(this, "putline (int*)");}

int image::putline (double *buf, int x, int y, int n, int to_index, int band)
{return generic_image_method_error(this, "putline (double*)");}

int image::putline (uchar *buf, int x, int y, int n, int to_index, int band)
{return generic_image_method_error(this, "putline_uchar (uchar*)");}

int image::putline (void *buf, int x, int y, int n, int to_index, int band)
{return generic_image_method_error(this, "putline (void*)");}

int image::getcolumn (int *buf, int x, int y, int n, int to_index, int dx, int band)
{return generic_image_method_error(this, "getcolumn (int*)");}

int image::putcolumn (int *buf, int x, int y, int n, int to_index, int band)
{return generic_image_method_error(this, "putcolumn (int*)");}

int image::getcolumn (double *buf, int x, int y, int n, int to_index, int dx, int band)
{return generic_image_method_error(this, "getcolumn (double*)");}

int image::putcolumn (double *buf, int x, int y, int n, int to_index, int band)
{return generic_image_method_error(this, "putcolumn (double*)");}

int image::getcolumn (float *buf, int x, int y, int n, int to_index, int dx, int band)
{return generic_image_method_error(this, "getcolumn (float*)");}

int image::putcolumn (float *buf, int x, int y, int n, int to_index, int band)
{return generic_image_method_error(this, "putcolumn (float*)");}

#if 0
int image::getline (unsigned char *buf, int x, int y, int n, int to_index, int dx)
 {error("image::getline undefined"); return(buf[x+y+n+to_index+dx]);}
int image::getline (unsigned short *buf, int x, int y, int n, int to_index, int dx)
 {error("image::getline undefined"); return(buf[x+y+n+to_index+dx]);} 
int image::getline (float *buf, int x, int y, int n, int to_index, int dx)
 {error("image::getline undefined"); return((int)buf[x+y+n+to_index+dx]);} 
int image::putline (unsigned char *buf, int x, int y, int n, int to_index)
 {error("image::putline undefined"); return(buf[x+y+n+to_index]);} 
int image::putline (unsigned short *buf, int x, int y, int n, int to_index)
 {error("image::putline undefined"); return(buf[x+y+n+to_index]);} 
int image::putline (float *buf, int x, int y, int n, int to_index)
 {error("image::putline undefined"); return((int)buf[x+y+n+to_index]);} 
#endif

int image::save_image (char *path, int error_ok)
 {error("image::save_image undefined"); return(path[0]);} 

image* 
image::map_image (int xdim, int ydim, IMAGE_ELEMENT_TYPE el_type,
		  int samples_per_pixel,
		  MAP_ELEMENT_TYPE *xmap, MAP_ELEMENT_TYPE *ymap)
{return 0;}

int image::add_to_working_set (int nrows, int ncols, int npages) {return 0;}
int image::remove_from_working_set (int nrows, int ncols, int npages) {return 0;}

require_image_pages::require_image_pages(image *img, int npages, int nrows, int ncols)
:img(img), nrows(nrows), ncols(ncols), npages(npages)
{
  img->add_to_working_set(npages,  nrows, ncols);
}

require_image_pages::~require_image_pages()
{
  img->remove_from_working_set(npages, nrows, ncols);
}

require_image_rows::require_image_rows(image *img, int nrows)
:img(img), nrows(nrows)
{
  img->add_to_working_set(0, nrows, 0);
}

require_image_rows::~require_image_rows()
{
  img->remove_from_working_set(0, nrows, 0);
}

require_image_cols::require_image_cols(image *img, int ncols)
:img(img), ncols(ncols)
{
  img->add_to_working_set(0, 0, ncols);
}

require_image_cols::~require_image_cols()
{
  img->remove_from_working_set(0, 0, ncols);
}


#ifdef LISP_IMAGE_ARRAYS

mapped_image::mapped_image (int sx, int sy, IMAGE_ELEMENT_TYPE el_type) 
  : image(sx, sy, el_type)
{}

#else /* ! LISP_IMAGE_ARRAYS */

mapped_image::mapped_image (int sx, int sy, IMAGE_ELEMENT_TYPE el_type) 
  : image(sx, sy, el_type)
{
  xmap = XALLOC(MAP_ELEMENT_TYPE, sx);
  ymap = XALLOC(MAP_ELEMENT_TYPE, sy);
}

#endif /* ! LISP_IMAGE_ARRAYS */

blocked_mapped_image::blocked_mapped_image
  (int sx, int sy, IMAGE_ELEMENT_TYPE el_type, int bx, int by, int pbx, int no_alloc )
  :mapped_image(sx, sy, el_type)
{
  ignore(bx, by, pbx, no_alloc);
}

void blocked_mapped_image::init_block_dims(int bx, int by, int pbx, int samples_per_pixel)
{
  block_xdim = bx; block_ydim = by;
  bx = abs(block_xdim); by = abs(block_ydim);
  if (pbx==0) {
    int pixel_size = image_element_size_from_type(image_element_type(this))*samples_per_pixel;
    if (pixel_size <= 16)
      pbx = pad_to_multiple(bx, BITS_PER_WORD/pixel_size);
    else pbx = bx; // what is the correct padding for RGB8 (24 bits_per_pixel)?
  }
  padded_block_xdim = pbx;
#if !defined(LHQ20060701B)
  _block_size = padded_block_xdim*by; // should this be padded to a word boundary?
  _blocks_wide = ceiling (xdim, bx);
  _blocks_hi   = ceiling (ydim, by);
#endif
}	

int
bytes_per_tile (blocked_mapped_image *img)
{
  return((img->block_size()*image_element_size(img)*img->samples_per_pixel)>>3);// FIXME BIGNUM
}

void* 
blocked_mapped_image::get_image_tile (int x, int y)
{
  //error("blocked_mapped_image::get_image_tile is undefined");
  return (void *) 0;
}

int 
blocked_mapped_image::iref_page_number (int x, int y) 
{
  //error("blocked_mapped_image::iref_page_number is undefined");
  return  0;
}

void 
blocked_mapped_image::set_page_pool_size (int npages, int shrink) 
{
  tprintf(5, "blocked_mapped_image::set_page_pool_size: NOTHING DONE!\n");
}

/*
CANONICAL TILE INDEXING:

x is broken into xhi = x/bx, xlo = x % bx
y is broken into yhi = y/by, ylo = y % by

xmap[x] = xhi*block_size + xlo
ymap[y] = yhi*block_size*blocks_wide + ylo*pbx

*/

// spp calculation is only correct for array-images.
void blocked_mapped_image::default_construct_maps(int blk_size)
{
  int x, y, xhi, xlo, yhi, ylo, k, val;
  int bx = abs(block_xdim); 
  int by = abs(block_ydim);
  int pbx = padded_block_xdim;
  int spp = samples_per_pixel;
  int blks_wide = blocks_wide();
  //int spp = 1;
  if (! xmap) {
    xmap = new MAP_ELEMENT_TYPE[xdim];
    ymap = new MAP_ELEMENT_TYPE[ydim];
  } 
  for (x=0; x<xdim; x++) {
    xhi = x / bx; xlo = x-xhi*bx;
    k = (block_xdim>0)? x : (xdim-1-x);
    val = MAP_ELEMENT_WRITE_SHIFT(spp*(xlo + xhi*blk_size));
    xmap[k] = val;
  }
  for (y=0; y<ydim; y++) {
    yhi = y / by; ylo = y-yhi*by;
    k = (block_ydim>0)? y : (ydim-1-y);
    val = MAP_ELEMENT_WRITE_SHIFT(spp*(pbx*ylo + yhi*blk_size*blks_wide));
    ymap[k] = val;
  }
}

img_choose_fn_t choose_image_class_fn;

// I believe that the only caller to this is iu_testbed_file_image_header::validate_file_image_hdr_dims
// defined in file-image-io.c++
extern int 
choose_image_class (int xdim, int ydim, int block_xdim, int block_ydim, 
		    int block_size, int pixel_size)
{
  return(choose_image_class_fn(xdim, ydim, block_xdim, block_ydim,
			       block_size, pixel_size));
}


/*
make_image
  make_image_fn
    make_file_or_array_image

      make_file_image
        make_iu_testbed_file_image
        make_tiff_file_image
*         make_paged_image
            new paged_image<T>
            make_band_interleaved_paged_image
              new band_interleaved_paged_image<T>

*     make_array_image
        new array_image<T>
        make_band_interleaved_array_image 
          new band_interleaved_array_image<T>

make_tiff_lazy_image
load_tiff_lazy_image
  make_lazy_image 
    make_paged_image
 */

 
img_make_fn_t make_image_fn;

extern image *
make_image (int xdim, int ydim, 
	    IMAGE_ELEMENT_TYPE element_type,
	    int samples_per_pixel,
	    int block_xdim, int block_ydim, int padded_block_xdim)
{
  tprintf(2, "make_image: redirecting to make_image_fn = %x\n", make_image_fn);
  image *img = make_image_fn(xdim, ydim, element_type, samples_per_pixel,
			     block_xdim, block_ydim, padded_block_xdim);
  return img;
}

extern image *
make_raster_image (int xdim, int ydim, IMAGE_ELEMENT_TYPE element_type, int samples_per_pixel,
		   int padded_block_xdim)
{
  //  tprintf(2, "make_image_fn = %x\n", make_image_fn);
  if (padded_block_xdim == 0) padded_block_xdim = xdim;
  image *img = make_image_fn(xdim, ydim, element_type, samples_per_pixel,
			     xdim, -ydim, padded_block_xdim);
  return img;
}

#if 0
image *copy_image (image *img, image *to_image)
{ 
  int xdim=image_xdim(img);
  int ydim=image_ydim(img);
  if (! to_image) to_image = similar_image(img);

  // double buf[xdim];
  double *buf = XALLOC(double, xdim);
  int y;
  for (y=0; y<ydim;  y++) {
    getline(img, buf, 0, y);
    putline(to_image, buf, 0, y);
   }
   xxfree(buf);
  return(to_image);
}
#endif


#if defined(ENABLE_IMAGE_PROPS)
void image_put_int_prop(image *img, PROPKEY key, int value) {
  put_prop(&(img->property_list), key, (void*) value);
}


int image_get_int_prop(image *img, PROPKEY key) {
  return (int) get_prop(img->property_list, key);
}
#endif

END_NAMESPACE_FREEDIUS
