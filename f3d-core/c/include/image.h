#ifndef	__image_h
#define __image_h

#include "image-types.h"
#include "base-class.h"
#include "namespace.h"

#define ENABLE_IMAGE_PROPS

/*
I would like to eliminate property_list from images.

NOT USED when ENABLE_DISPLAY_IMAGE is undefined.
  luminance_subimage_linearly_mapped_to_luminance in gl/texture-maps.c++ uses get_float_prop
  to access scale and offset photometric-transform parameters.  Perhaps the image class needs
  an explicit slot for photometric_transform.

tiff-io.c++ calls set_image_colormap defined in color.c++ which uses image->property_list.
for "color_map".  I propose moving all (or most) of tiff-io to Lisp, and puting the colormap 
of the property-list of the Lisp image struct.  
*/

#if defined(ENABLE_IMAGE_PROPS)
#define __list_ops_h // prevent defining the list operators
#include "list.h"
#undef __list_ops_h
#endif

BEGIN_NAMESPACE_FREEDIUS

#if 0 //defined(ENABLE_IMAGE_PROPS)

typedef void *LISTELEM;
#if 0
typedef struct pointer_list_struct {
  LISTELEM car;
  struct pointer_list_struct *cdr;
} Pointer_List;
#endif
typedef Pointer_List *POINTER_LIST;
typedef Pointer_List *INT_LIST; /* This is really bogus  */
typedef char* PROPKEY;

#endif // 0 defined(ENABLE_IMAGE_PROPS)

#define LISP_IMAGE_ARRAYS

// BITS_PER_WORD should be defined in config.h
#define BITS_PER_WORD 32 // FIXME AMD64

extern Class *image_class;

class image :public Object {
 public:
#if defined(ENABLE_IMAGE_PROPS)
  POINTER_LIST property_list;
#endif
  int xdim; int ydim; 
  int flags;
  // I would like to ensure that scalar images always have samples_per_pixel = 1,
  // and only band_interleaved images really make use of samples_per_pixel > 1.
  int samples_per_pixel; 
  //  int element_size; // this slot has been removed -- use image_element_size(image *img)
  //  IMAGE_ELEMENT_TYPE element_type;  // changed to a virtual function
  //  IMAGE_CLASS class_code;           // changed to a virtual function

  virtual IMAGE_ELEMENT_TYPE element_type();
  virtual IMAGE_CLASS class_code() {return UNKNOWN_IMAGE_CLASS;} 

  image(int sx, int sy, IMAGE_ELEMENT_TYPE type):Object() {
#if defined(ENABLE_IMAGE_PROPS)
    property_list = 0;
#endif
    xdim= sx; ydim=sy; 
    // element_type=type; 
    // element_size = image_element_size_from_type(type);
    flags = 0;
    samples_per_pixel = 1;
  }
  image():Object(){samples_per_pixel=1;}
  virtual int iref(int x, int y);
  virtual double interpolate_iref(double x, double y);
  virtual double diref(int x, int y);
  virtual void iset(int x, int y, int val);
  virtual void iset(int x, int y, double val);
  virtual int getline (double *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int getline (float *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int getline (uchar *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int getline (ushort *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int getline (short *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int getline (int *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  
  virtual int putline (double *buf, int x, int y, int n=0, int to_index=0, int band=0);
  virtual int putline (uchar *buf, int x, int y, int n=0, int to_index=0, int band=0);
  virtual int putline (int *buf, int x, int y, int n=0, int to_index=0, int band=0);
  
  virtual int getline (void *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int putline (void *buf, int x, int y, int n=0, int to_index=0, int band=0);
  // The void cases define getline and putline of for buf same element type as image.
  // THIS IS AN UNSAFE AND UGLY HACK AND SHOULD BE FIXED.  
  // For instance, the following could be implemented using a switch on element_type.
  // int getline (RGB8_PIXEL *buf, int x, int y, int n=0, int to_index=0, int dx=1);
  // int getline (RGBA8_PIXEL *buf, int x, int y, int n=0, int to_index=0, int dx=1);
  // Another option is to define classes for scan-line-buffers so that the element-type of the
  // buffer can be compared to the element of the image. 
  
  virtual int getcolumn (int *buf, int x, int y, int n=0, int to_index=0, int dy=1, int band=0);
  virtual int getcolumn (double *buf, int x, int y, int n=0, int to_index=0, int dy=1, int band=0);
  virtual int getcolumn (float *buf, int x, int y, int n=0, int to_index=0, int dy=1, int band=0);
  virtual int putcolumn (int *buf, int x, int y, int n=0, int to_index=0, int band=0);
  virtual int putcolumn (double *buf, int x, int y, int n=0, int to_index=0, int band=0);
  virtual int putcolumn (float *buf, int x, int y, int n=0, int to_index=0, int band=0);
  virtual int save_image (char *path, int error_ok = 0);  
  virtual image* map_image (int xdim, int ydim, IMAGE_ELEMENT_TYPE el_type,
			    int samples_per_pixel,
			    MAP_ELEMENT_TYPE *xmap, MAP_ELEMENT_TYPE *ymap);
  virtual int add_to_working_set (int npages = 0, int nrows = 1, int ncols = 0);
  virtual int remove_from_working_set (int npages = 0, int nrows = 1, int ncols = 0);

 };

typedef int (*iref_fn_t)(image *,int,int);
typedef double (*diref_fn_t)(image *,int,int);
typedef double (*interpolate_iref_fn_t)(image *,double,double);
typedef void (*iset_fn_t)(image *,int,int,int);
typedef void (*diset_fn_t)(image *,int,int,double);

typedef image *IMAGE;
typedef IMAGE (*img_make_fn_t)(int,int,int,int,int,int,int);
extern img_make_fn_t make_image_fn;

typedef int (*img_choose_fn_t)(int,int,int,int,int,int);
extern img_choose_fn_t choose_image_class_fn;

class mapped_image:
  public image
{public:
   MAP_ELEMENT_TYPE *xmap;
   MAP_ELEMENT_TYPE *ymap;
   mapped_image(int sx, int sy, IMAGE_ELEMENT_TYPE el_type);
   mapped_image(){} // not sure about this
   inline MAP_ELEMENT_TYPE iref_index(int x, int y) const 
     {return MAP_ELEMENT_SHIFT(xmap[x]+ymap[y]);}
};	

/*
// enable removing block_size blocks_wide blocks_hi slots from blocked_mapped_image
//#define LHQ20060701B

LHQ20060701B fails because blocks_wide blocks_hi calculations are wrong when
image is a window of a larger image.

This suggests that blocks_wide and blocks_hi should be members of another class instance
that is an image_page_handler or an image_array_descriptor.  This is the guy
that needs reference counts (or equivalent) for GC.

This should not affect the iref or getline code since it doesn't reference these vars.
It would be nice if block_xdim, block_ydim and padded_block_xdim were also in the 
data descriptor.

block_size calculation is ok?  

THE IMAGE CLASS HIERARCHY NEEDS TO BE REWORKED.
*/

class blocked_mapped_image:
  public mapped_image
{public:
  int block_xdim;
  int block_ydim;
  int padded_block_xdim;
#if !defined(LHQ20060701B)
  int _block_size, _blocks_wide, _blocks_hi;
  int block_size() {return _block_size;}
  int blocks_wide() {return _blocks_wide;}
  int blocks_hi() {return _blocks_hi;}
#else
  int block_size() {return padded_block_xdim*abs(block_ydim);}
  int blocks_wide() {return ceiling(xdim,abs(block_xdim));}
  int blocks_hi() {return ceiling(ydim,abs(block_ydim));}
#endif
  blocked_mapped_image(int sx, int sy, IMAGE_ELEMENT_TYPE el_type, 
		       int bx=0, int by=0, int pbx=0, int no_alloc=0);
  blocked_mapped_image(){}  // not sure about this
  void init_block_dims(int bx, int by, int pbx, int samples_per_pixel);
  void default_construct_maps(int blk_size);
  // why would this need to be qualified?
  //void blocked_mapped_image::image_page_number_to_rectangle (int blknum, int& x0, int& y0, int& x1, int& y1);
  void image_page_number_to_rectangle (int blknum, int& x0, int& y0, int& x1, int& y1);
  virtual void* get_image_tile (int x, int y);
  virtual int iref_page_number (int x, int y);
  virtual void set_page_pool_size (int npages, int shrink=0);
};

int bytes_per_tile (blocked_mapped_image *img);


#define WITH_IMAGE_ELEMENTS(img, body)\
{int img##_xdim=img->xdim, img##_ydim=img->ydim;\
   MAP_ELEMENT_TYPE *img##_xmap, *img##_ymap;\
   img##_xmap=img->xmap; img##_ymap=img->ymap;\
   body}

int choose_image_class(int xdim, int ydim, int block_xdim, int block_ydim, int block_size, int pixel_size);

image * make_image (int xdim, int ydim, 
		    IMAGE_ELEMENT_TYPE element_type = IMG_UNSIGNED_8BIT,
		    int samples_per_pixel = 1,
		    int block_xdim = 0, int block_ydim = 0,
		    int padded_block_xdim = 0);

image * make_raster_image (int xdim, int ydim, 
			   IMAGE_ELEMENT_TYPE element_type = IMG_UNSIGNED_8BIT,
			   int samples_per_pixel = 1,
			   int padded_block_xdim = 0);

image* load_image (char * path, int error_ok = 0);

image* reload_image (char * path, image* im, int error_ok = 0);

int save_image (image *img, char * path, int error_ok = 0);

inline int getline(image *img, int *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0)
{return(img->getline(buf, x, y, n, to_index, dx, band));}

inline int getline(image *img, double *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0)
{return(img->getline(buf, x, y, n, to_index, dx, band));}

inline int putline(image *img, int *buf, int x, int y, int n=0, int to_index=0, int band=0)
{return(img->putline(buf, x, y, n, to_index, band));}

inline int putline(image *img, double *buf, int x, int y, int n=0, int to_index=0, int band=0)
{return(img->putline(buf, x, y, n, to_index, band));}

// This next is needed because of problems adding getline with buffer type
// of unsigned char  to just array_image<unsigned char> and paged_image<unsigned char>
// Templates want every class to have exactly the same methods, even when they
// do not make sense.
//inline int getline(image *img, unsigned char *buf, int x, int y, 
//		   int n=0, int to_index=0, int dx=1, int band=0);

 
#ifdef NEVER
#include "qcme_vector.h"
inline int getline(image *img, uchar *buf, int x, int y, int n=0, int to_index=0, int dx=1)
{return(img->getline(buf, x, y, n, to_index, dx));}

inline int getline(image *img, ushort *buf, int x, int y, int n=0, int to_index=0, int dx=1)
{return(img->getline(buf, x, y, n, to_index, dx));}

inline int getline(image *img, float *buf, int x, int y, int n=0, int to_index=0, int dx=1)
{return(img->getline(buf, x, y, n, to_index, dx));}

inline int getline(image *img, basic_qcme_vector *buf, int x, int y, int n=0, int to_index=0, int dx=1)
{switch (buf->type) 
  {case IMG_UNSIGNED_8BIT:
    return(getline(img, (uchar *) buf, x, y, n, to_index, dx));
  case IMG_UNSIGNED_16BIT:
    return(getline(img, (ushort *) buf, x, y, n, to_index, dx));
  case IMG_UNSIGNED_32BIT:
    return(getline(img, (int *) buf, x, y, n, to_index, dx));
  case IMG_SINGLE_FLOAT:
    return(getline(img, (float *) buf, x, y, n, to_index, dx));
  case IMG_DOUBLE_FLOAT:
    return(getline(img, (double *) buf, x, y, n, to_index, dx));
  //default: error("Unhandled image element_type.");
  }}

inline int putline(image *img, uchar *buf, int x, int y, int n=0, int to_index=0)
{return(img->putline(buf, x, y, n, to_index));}

inline int putline(image *img, ushort *buf, int x, int y, int n=0, int to_index=0)
{return(img->putline(buf, x, y, n, to_index));}

inline int putline(image *img, float *buf, int x, int y, int n=0, int to_index=0)
{return(img->putline(buf, x, y, n, to_index));}

inline int putline(image *img, basic_qcme_vector *buf, int x, int y, int n=0, int to_index=0)
{switch (buf->type) 
  {case IMG_UNSIGNED_8BIT:
    return(putline(img, (uchar *) buf, x, y, n, to_index));
  case IMG_UNSIGNED_16BIT:
    return(putline(img, (ushort *) buf, x, y, n, to_index));
  case IMG_SIGNED_32BIT:
    return(putline(img, (int *) buf, x, y, n, to_index));
  case IMG_UNSIGNED_32BIT:
    return(putline(img, (uint *) buf, x, y, n, to_index));
  case IMG_SINGLE_FLOAT:
    return(putline(img, (float *) buf, x, y, n, to_index));
  case IMG_DOUBLE_FLOAT:
    return(putline(img, (double *) buf, x, y, n, to_index));
  //default: error("Unhandled image element_type.");
  }}
#endif // NEVER

inline int image_xdim(image *img) {return(img->xdim);}
inline int image_ydim(image *img) {return(img->ydim);}
inline IMAGE_ELEMENT_TYPE image_element_type(image *img) {return(img->element_type());}
inline IMAGE_CLASS image_class_code(image *img) {return(img->class_code());}

int image_element_size_from_type(IMAGE_ELEMENT_TYPE element_type);
const char* image_element_type_name (IMAGE_ELEMENT_TYPE element_type);
int image_element_properties (IMAGE_ELEMENT_TYPE element_type, 
			      int& spp, int& bps, int &format, int& photometric_type);
IMAGE_ELEMENT_TYPE image_element_type_from_size(int element_size, 
						int signed_p=0, int float_p=0);

//inline int image_element_size(image *img) {return(img->element_size);}
inline int image_element_size(image *img) {return image_element_size_from_type(image_element_type(img));}


inline void iset(image *img, int x, int y, int val)
{img->iset(x, y, val);
}

inline void iset(image *img, int x, int y, double val)
{img->iset(x, y, val);
}

inline int iref(image *img, int x, int y)
{return(img->iref(x, y));
}

inline double diref(image *img, int x, int y)
{return(img->diref(x, y));
}

#if 0 //defined(ENABLE_IMAGE_PROPS)

inline LISTELEM image_prop (image *image, PROPKEY prop)
{ return get_prop(image->property_list, prop);
}

inline void image_putprop (image *image, PROPKEY prop, LISTELEM val)
{ put_prop(&image->property_list, prop, val);
}
#endif

inline static void flip_rgb8_endian (RGB8_PIXEL *buf, int n)
{
  int i, t;
  for (i=0; i<n; i++){
    t = buf[i].r;
    buf[i].r = buf[i].b;
    buf[i].b = t;
  }}
   
inline static void flip_rgb8_endian (RGBA8_PIXEL *buf, int n)
{
  int i, t;
  for (i=0; i<n; i++){
    t = buf[i].r;
    buf[i].r = buf[i].a;
    buf[i].a = t;
    t = buf[i].g;
    buf[i].g = buf[i].b;
    buf[i].b = t;
  }}
   
void describe_image (blocked_mapped_image *img);
void * get_image_tile (blocked_mapped_image *img, int x, int y);


#if 0
inline int
getline1
 (image *img, int *buf, int x, int y, int n = 0, int to_index = 0, int dx=1)
{return( img->getline(buf, x, y, n, to_index, dx));
}
#endif

// this belongs in a different file -- really private stuff
#define IMAGE_MAPS_HACKED 1

typedef struct cimage_struct {} cimage;


class require_image_pages {
 public:
  image *img;
  int nrows; 
  int ncols; 
  int npages; 
  require_image_pages(image *img, int npages = 0, int nrows = 0, int ncols = 0);
  ~require_image_pages();
};

class require_image_rows {
 public:
  image *img;
  int nrows; 
  require_image_rows(image *img, int nrows = 1);
  ~require_image_rows();
};

class require_image_cols {
 public:
  image *img;
  int ncols; 
  require_image_cols(image *img, int ncols = 1);
  ~require_image_cols();
};

END_NAMESPACE_FREEDIUS

#undef ENABLE_IMAGE_PROPS

#endif /* ! __image_h */
