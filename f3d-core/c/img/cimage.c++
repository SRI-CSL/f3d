// This is a c language interface to C++ functionality.

// Why is this a .C file rather than .c?
// Answer: Because it makes calls to C++ methods.

// Why isn't this in the .C files containing the C++ definitions?
// For C++ only usage, there is no need for these definitions.

#include "image.h"
#include "image-io.h"
#include "image-ops.h"
#include "iu-testbed-io.h"
#include "cimage.h"
#include "cme-error.h"


#define FILE_IMAGE 1
#define TIFF_IMAGE 1
#define BMP_IMAGE 1

#include "array-image.h"

#ifdef FILE_IMAGE
#include "file-image.h"
#endif

#ifdef TIFF_IMAGE
#include "tiff-io.h"
#endif

#ifdef BMP_IMAGE
#include "bmp-io.h"
#endif

#include "list.h"

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

 
//#define LISP_CLASS_SLOT_HACKS

#if defined(LISP_CLASS_SLOT_HACKS) // Lisp class slot hacks

// Experiments with calling image functions passing Lisp image structure


#if defined(HAVE_CMUCL)  || defined(HAVE_SBCL)
#define LISP_CLASS_SLOT_OFFSET 7
#define LISP_ARRAY_HEADER_OFFSET -1
#endif // defined(HAVE_CMUCL)  || defined(HAVE_SBCL)

typedef void * LISP_OBJECT; 

typedef struct lisp_array_image_struct {
  //char pad[7];
  array_image_base *cimage;
  int xdim;
  int ydim;
  char *xmap;
  char *ymap;
  LISP_OBJECT element_type;
  int initial_value;
} lisp_array_image;


#define FIXNUM2INT(fixnum) ((fixnum)>>2)

extern "C" {

  int lisp_array_image_xmap_ref (char *img, int i)
  {return ((MAP_ELEMENT_TYPE *)(((lisp_array_image *) (img+LISP_CLASS_SLOT_OFFSET))->xmap - LISP_ARRAY_HEADER_OFFSET))[i];
  }
    
unsigned int lhq_slot_ref_u32(char *ptr, int offset) {
  return *((unsigned int *)(ptr + offset));
}

  int lisp_class_fixnum_slot (char *ptr, int slot_num)
  { 
    return FIXNUM2INT(*(int *) (ptr+LISP_CLASS_SLOT_OFFSET + 4*slot_num));
  }

  char* lisp_class_array_slot (char *ptr, int slot_num)
  { 
    return ((char *) (*(ptr+LISP_CLASS_SLOT_OFFSET + 4*slot_num)) - LISP_ARRAY_HEADER_OFFSET);
  }

int lisp_array_image_xdim (char *img) {
  return FIXNUM2INT(((lisp_array_image *) (img+LISP_CLASS_SLOT_OFFSET))->xdim);
}

} // end extern "C"

array_image_base *
set_array_image_slots (array_image_base *img, lisp_array_image *limg)
{
  img->xdim = limg->xdim;
  img->ydim = limg->ydim;
  img->xmap = limg->xmap;
  img->ymap = limg->ymap;
  img->element_type = limg->element_type;
  img->initial_value = limg->initial_value;
  img->array = limg->array;
  img->ydim = limg->ydim;

}

#endif // defined(LISP_CLASS_SLOT_HACKS)



extern "C" {

int FREEDIUS_GLOBAL(image_xdim)(cimage *img) {
  return(image_xdim((image *) img));
}

int FREEDIUS_GLOBAL(image_ydim)(cimage *img) {
  return(image_ydim((image *) img));
}

int FREEDIUS_GLOBAL(image_element_size)(cimage *img) {
  return(image_element_size((image *) img));
}

IMAGE_ELEMENT_TYPE FREEDIUS_GLOBAL(image_element_type)(cimage *img){
  return(image_element_type((image *) img));
}

IMAGE_CLASS FREEDIUS_GLOBAL(image_class_code)(cimage *img){
  return(image_class_code((image *) img));
}


int FREEDIUS_GLOBAL(image_blocks_wide)(cimage *img){
  return(((blocked_mapped_image *) img)->blocks_wide());
}

int FREEDIUS_GLOBAL(image_blocks_hi)(cimage *img){
  return(((blocked_mapped_image *) img)->blocks_hi());
}

int FREEDIUS_GLOBAL(image_block_size)(cimage *img){
  return(((blocked_mapped_image *) img)->block_size());
}


cimage *FREEDIUS_GLOBAL(make_image)(int xdim, int ydim, IMAGE_ELEMENT_TYPE element_type,
				    int sampels_per_pixel) 
  {
    return ((cimage *) make_image(xdim, ydim, element_type, sampels_per_pixel));
  }


cimage *FREEDIUS_GLOBAL(make_image_blocked)(int xdim, int ydim, IMAGE_ELEMENT_TYPE element_type,
					    int sampels_per_pixel,
					    int block_xdim, int block_ydim, int padded_block_xdim)
  {
    return ((cimage *) make_image(xdim, ydim, element_type, sampels_per_pixel,
				 block_xdim, block_ydim, padded_block_xdim));
  }


cimage *FREEDIUS_GLOBAL(make_array_image)
      (int xdim, int ydim, IMAGE_ELEMENT_TYPE element_type, int samples_per_pixel,
       int block_xdim, int block_ydim, int padded_block_xdim) 
  {
    return ((cimage *) make_array_image(xdim, ydim, element_type, samples_per_pixel,
					block_xdim, block_ydim, padded_block_xdim, 0));
  }

cimage *FREEDIUS_GLOBAL(make_file_image)
      (int xdim, int ydim, IMAGE_ELEMENT_TYPE element_type, int samples_per_pixel,
       int block_xdim, int block_ydim, int padded_block_xdim, char* pathname) 
  {
    return ((cimage *) make_file_image(xdim, ydim, element_type, samples_per_pixel,
					block_xdim, block_ydim, padded_block_xdim, pathname));
  }

#if 0 // need to #include "lazy-image.h"
cimage *FREEDIUS_GLOBAL(make_lazy_image)
      (int xdim, int ydim, IMAGE_ELEMENT_TYPE element_type, 
       lazy_image_page_handler *page_handler,
       int block_xdim, int block_ydim) 
  {
    return ((cimage *) make_lazy_image(xdim, ydim, element_type, page_handler,
				       block_xdim, block_ydim));
  }
#endif

#if 0
cimage *FREEDIUS_GLOBAL(make_band_interleaved_array_image)
      (int xdim, int ydim, IMAGE_ELEMENT_TYPE element_type, int samples_per_pixel,
       int block_xdim, int block_ydim, int padded_block_xdim) 
  {
    return ((cimage *) make_band_interleaved_array_image(xdim, ydim, element_type, samples_per_pixel,
							  block_xdim, block_ydim, padded_block_xdim));
  }

cimage *FREEDIUS_GLOBAL(make_band_interleaved_paged_image)
      (int xdim, int ydim, IMAGE_ELEMENT_TYPE element_type, int samples_per_pixel,
       int block_xdim, int block_ydim, int padded_block_xdim) 
  {
    return ((cimage *) make_band_interleaved_paged_image(xdim, ydim, element_type, samples_per_pixel,
							  block_xdim, block_ydim, padded_block_xdim));
  }
#endif

void FREEDIUS_GLOBAL(iset)(cimage *img, int x, int y, int val){
  ((image *) img)->iset(x,y,val);
}

void FREEDIUS_GLOBAL(diset)(cimage *img, int x, int y, double val){
  ((image *) img)->iset(x,y,val);
}

int FREEDIUS_GLOBAL(iref)(cimage *img, int x, int y){
  return (((image *) img)->iref(x,y));
}

double FREEDIUS_GLOBAL(diref)(cimage *img, int x, int y) {
  return (((image *) img)->diref(x,y));
}

double FREEDIUS_GLOBAL(interpolate_iref)(cimage *img, double x, double y){
  return (((image *) img)->interpolate_iref(x,y));
}

} // end extern "C"



extern string default_file_image_directory_pathname;

// not used.
extern "C" {

#if 0
  void FREEDIUS_GLOBAL(init_image_io)(int flags, string default_file_image_directory)
{
  image_header_list =0;
  init_iu_testbed_array_image_header();
#ifdef FILE_IMAGE  
  init_make_file_image_fn();// this is needed to force loading of file-images
  default_file_image_directory_pathname = default_file_image_directory;
#endif
#ifdef TIFF_IMAGE
  init_tiff_array_image_header();// this is needed to force loading of tiff
#endif
#ifdef BMP_IMAGE
  init_bmp_array_image_header();// this is needed to force loading of BMP
#endif
}
#endif // not used

cimage *FREEDIUS_GLOBAL(load_image)(char *path)
{
  tprintf(3,"load_image: invoking load_image(%s)\n", path);
  return((cimage *)load_image(path));
}

char *FREEDIUS_GLOBAL(read_property_list_string)(char *path)
{
  tprintf(3,"read_property_list_string : invoking read_property_list_string(%s)\n", path);
  return(read_property_list_string(path));
}

cimage *FREEDIUS_GLOBAL(reload_image)(char *path, cimage* img)
{
  tprintf(3,"reload_image: invoking reload_image(%s)\n", path);
  return((cimage *)reload_image(path, (image*) img, 1));
}

int FREEDIUS_GLOBAL(recognize_image_header) (char *path)
{
  return(find_image_header(path) != 0);
}

basic_image_header* FREEDIUS_GLOBAL(read_image_header) (char *path)
{
  return(find_read_image_header(path));
}

int FREEDIUS_GLOBAL(save_image)(cimage *img, char *path)
{
  return(save_image((image *)img, path));
}

} // end extern "C"


int iu_testbed_save_image (array_image_base *img, string path, int error_ok = 0);

extern "C" {

int FREEDIUS_GLOBAL(save_iu_testbed_image)(cimage *img, char *path, int error_ok)
{
  return(iu_testbed_save_image((array_image_base *)img, path, error_ok));
}

int FREEDIUS_GLOBAL(save_tiff_image)(cimage *img, char *path, int error_ok,
				     int compression_mode, int jpeg_quality)
{
  return(save_untiled_tiff_image((image *)img, path, error_ok, compression_mode, jpeg_quality));
}

cimage * FREEDIUS_GLOBAL(gauss_convolve_decimate)(cimage *img, double ka)
{
  return((cimage *)gauss_convolve_decimate((image *)img, ka));
}

  cimage * FREEDIUS_GLOBAL(fast_gauss_convolve)(cimage *img, cimage *into_image, int level, double ka, cimage *scratch)
{
  return((cimage *) fast_gauss_convolve((image *)img, (image *)into_image, level, ka, (image*) scratch));
}


#if defined(ENABLE_IMAGE_PROPS)


LISTELEM FREEDIUS_GLOBAL(image_prop) (cimage *img, PROPKEY prop)
{ 
  return image_prop((image *) img, prop);
}

int FREEDIUS_GLOBAL(image_prop_int) (image *img, PROPKEY prop)
{ 
  return get_int_prop(img->property_list, prop);
}

float FREEDIUS_GLOBAL(image_prop_float) (image *img, PROPKEY prop)
{ 
  return get_float_prop(img->property_list, prop);
}

void FREEDIUS_GLOBAL(image_putprop) (cimage *img, PROPKEY prop, LISTELEM val)
{
  image_putprop((image *) img, prop, val);
}

void FREEDIUS_GLOBAL(image_putprop_int) (image *img, PROPKEY prop, int val)
{
  put_int_prop(&img->property_list, prop, val);
}

void FREEDIUS_GLOBAL(image_putprop_float) (image *img, PROPKEY prop, float val)
{
  put_float_prop(&img->property_list, prop, val);
}


#endif // defined ENABLE_IMAGE_PROPS

void FREEDIUS_GLOBAL(image_set_dynamic_range) (image *img, double scale, double offset)
{
  put_float_prop(&(img->property_list), "scale", scale);
  put_float_prop(&(img->property_list), "offset", offset);
}


} // end extern "C"

typedef enum _image_pyramid_filter_type_ {
  IMAGE_PYRAMID_DEFAULT_FILTER,
  IMAGE_PYRAMID_BOX_FILTER,
  IMAGE_PYRAMID_GAUSS_FILTER_MIN_BLUR,
  IMAGE_PYRAMID_GAUSS_FILTER_MIN_ALIAS
} image_pyramid_filter_type;
  

image *
image_filter_decimate2 (image *from_image, image_pyramid_filter_type pyramid_type,
			image *into_image=0)
{
  switch (pyramid_type) {
  case IMAGE_PYRAMID_BOX_FILTER:
    tprintf(4, "box_filter_decimate2\n");
    return box_filter_decimate2(from_image, into_image); break;
  case IMAGE_PYRAMID_GAUSS_FILTER_MIN_BLUR:
    tprintf(4, "gauss_convolve_decimate MIN_BLUR\n");
    return gauss_convolve_decimate(from_image, .4296875, into_image); break;
  case IMAGE_PYRAMID_GAUSS_FILTER_MIN_ALIAS:
    tprintf(4, "gauss_convolve_decimate MIN_ALIAS\n");
    return gauss_convolve_decimate(from_image, .359375, into_image); break;
  default: error("unknown pyramid_type = %d", pyramid_type);
  } 
  return 0;
}

extern "C" {
cimage * FREEDIUS_GLOBAL(image_filter_decimate2) (cimage *img, int pyramid_type,
				   cimage *into_image)
{
  return ((cimage *)image_filter_decimate2((image *)img, 
					   (image_pyramid_filter_type)pyramid_type, 
					   (image *) into_image));
}

} // end extern "C"


END_NAMESPACE_FREEDIUS

