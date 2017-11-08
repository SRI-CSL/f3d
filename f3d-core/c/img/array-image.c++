#ifdef __GNUG__
#pragma implementation
#endif

#include "array-image.h"
#include "misc.h"
#include "cme-error.h"

#include  <string.h>
#include <stdio.h>
#include <stdlib.h>
//#ifdef HAVE_MALLOC_H
//#include <malloc.h>
//#endif

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS


setClass(array_image_base, image);

int array_image_base::iref_page_number (int x, int y) {
    MAP_ELEMENT_TYPE index = iref_index(x, y); 
    return index/block_size();
  }

// With OpenGL textures, do not want to exceed 1kx1k block size.
int array_image_max_block_dim = 1024;

#if 1

// zero defaults allow array images to become raster images.
// This requires more work for display, but is more storage efficient and allows
// raster oriented code to access them.
int array_image_default_block_xdim = 0;
int array_image_default_block_ydim = 0;

#else
// These block dims chosen for compability with the OpenGl texture tile cache.
// Other block dims are permitted, but will make image display somewhat slower.
int array_image_default_block_xdim = 256;
int array_image_default_block_ydim = -256;
#endif

void array_image_default_block_dims(int sx, int sy, int *bx, int *by)
{
  if (array_image_default_block_xdim == 0)
    {*bx=sx; *by=-sy;}
  else {
    *bx = array_image_default_block_xdim; 
    *by = array_image_default_block_ydim;
  }
}

array_image_base::array_image_base
   (int sx, int sy, IMAGE_ELEMENT_TYPE el_type, int samples_per_pixel, int bx, int by, int pbx, int no_alloc)
  : blocked_mapped_image(sx, sy, el_type, bx, by, pbx, no_alloc)
{
  //class_code = ARRAY_IMAGE_CLASS;
  this->samples_per_pixel = samples_per_pixel;
  if (bx==0 || by==0) array_image_default_block_dims(sx, sy, &bx, &by);
  init_block_dims(bx, by, pbx, samples_per_pixel);
}

void * array_image_base::image_array()
{error("array_image_base::image_array");
 return((char *) 0);
}

void array_image_base::set_image_array(void *arr)
{error("array_image_base::set_image_array");
 
}
// This is length in bytes
int array_image_base::image_array_length_bytes() 
{return (((block_size()* image_element_size(this)) >>3) * blocks_wide()*blocks_hi());
}

int array_image_choose_image_class ()
{return (ARRAY_IMAGE_CLASS);
}


#if !defined(USE_CPP_INITS)
int init_array_image_fn ()
{if (! choose_image_class_fn)// do not stomp on another initialization
  choose_image_class_fn = (img_choose_fn_t)  array_image_choose_image_class;
  make_image_fn = (img_make_fn_t) make_array_image;
  return(1);
}
#else
class array_image_fn_init {
 public:
  array_image_fn_init() {
    if (! choose_image_class_fn)// do not stomp on another initialization
      choose_image_class_fn = (img_choose_fn_t)  array_image_choose_image_class;
    make_image_fn = (img_make_fn_t) make_array_image;
  }
};

array_image_fn_init aifi = array_image_fn_init();
#endif //!defined(USE_CPP_INITS)


#if 0
image*
array_image_base::map_image (int xdim, int ydim, IMAGE_ELEMENT_TYPE elem_type,
			     int samples_per_pixel,
			     MAP_ELEMENT_TYPE *xmap, MAP_ELEMENT_TYPE *ymap)
{
  array_image_base* clone 
    = (array_image_base*)
    make_array_image(xdim, ydim, elem_type,  samples_per_pixel,
		     block_xdim, block_ydim, padded_block_xdim, 1);
  clone->xmap = xmap; 
  clone->ymap = ymap; 
  clone->set_image_array(image_array());
  return clone;
}
#else
extern "C" {
image *FREEDIUS_GLOBAL(lisp_make_array_image)
  (int xdim, int ydim, 
   IMAGE_ELEMENT_TYPE element_type,
   int samples_per_pixel,
   int block_xdim, int block_ydim, int padded_block_xdim,
   void *array, MAP_ELEMENT_TYPE *xmap, MAP_ELEMENT_TYPE *ymap);
} // end extern "C"

image*
array_image_base::map_image (int xdim, int ydim, IMAGE_ELEMENT_TYPE elem_type,
			     int samples_per_pixel,
			     MAP_ELEMENT_TYPE *xmap, MAP_ELEMENT_TYPE *ymap)
{
  array_image_base* clone 
    = (array_image_base*)
    FREEDIUS_GLOBAL(lisp_make_array_image)(xdim, ydim, elem_type,  samples_per_pixel,
					   block_xdim, block_ydim, padded_block_xdim, 
					   image_array(), xmap, ymap);
  //clone->xmap = xmap; 
  //clone->ymap = ymap; 
  //clone->set_image_array(image_array());
  return clone;
}
#endif

/*

// Language/compiler/linker can't find the array_image method referred
// to here!! - CC, running on x86_64 compiling in 32-bit mode.

image *make_uchar_array_image_instance(int xdim, int ydim, unsigned char *array) {
  return new array_image<unsigned char>(xdim, ydim, IMG_UNSIGNED_8BIT, 1, xdim, -ydim, xdim, 1, array);
}
*/



#include "lisp-callback.h"
   
extern "C" {

// ***********************  IMAGE CALLBACKS TO LISP  *********************** 


// This is needed in order to generate the callback to Lisp.
DEFCALLBACK(void, FREEDIUS_GLOBAL(make_lisp_array_image),
	    (void* img, int xdim, int ydim, int imgsize, int element_type_code, int samples_per_pixel),
	    (img, xdim, ydim, imgsize, element_type_code, samples_per_pixel));

// ***********************  Lisp Callable Functions  *********************************

image* 
FREEDIUS_GLOBAL(map_array_image) (array_image_base* image, int xdim, int ydim, 
				  IMAGE_ELEMENT_TYPE elem_type,
				  int samples_per_pixel,
				  MAP_ELEMENT_TYPE *xmap, MAP_ELEMENT_TYPE *ymap)
{
  return image->map_image(xdim, ydim, elem_type, samples_per_pixel,  xmap, ymap);
}


// This is called from LISP.
void FREEDIUS_GLOBAL(set_array_image_arrays) (array_image_base *img, 
					      MAP_ELEMENT_TYPE *xmap, MAP_ELEMENT_TYPE *ymap, 
					      int xdim, int ydim,
					      void* arr, int maps_hacked)
{
  img->xmap = xmap;
  img->ymap = ymap;
  img->xdim = xdim;
  img->ydim = ydim;
  img->set_image_array(arr);
  if (maps_hacked) 
    img->flags|= IMAGE_MAPS_HACKED;
  else img->flags&= IMAGE_MAPS_HACKED;
}
void FREEDIUS_GLOBAL(update_array_image_arrays) (array_image_base *img, 
						 MAP_ELEMENT_TYPE *xmap, MAP_ELEMENT_TYPE *ymap, 
						 void* arr)
{
  img->xmap = xmap;
  img->ymap = ymap;
  img->set_image_array(arr);
}



// This is called from LISP.  UNUSED
void *FREEDIUS_GLOBAL(array_image_array) (array_image_base *img)
{ return img->image_array();}

} // end extern "C"



END_NAMESPACE_FREEDIUS
