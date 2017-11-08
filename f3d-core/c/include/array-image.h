#ifndef	__array_image_h
#define __array_image_h

#include "image.h"
#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

extern Class *array_image_Class;

class array_image_base:
  public blocked_mapped_image
{public:
  IMAGE_CLASS class_code() {return ARRAY_IMAGE_CLASS;}
  array_image_base(int sx, int sy, IMAGE_ELEMENT_TYPE el_type, int samples_per_pixel, int bx, int by, int pbx, int no_alloc);
  virtual void* image_array(); 
  virtual void set_image_array(void *array);
  virtual void* get_image_tile (int x, int y) {
    error("array_image_base::get_image_tile is undefined");
    return (void *) 0;}
  virtual int iref_page_number (int x, int y);
  int image_array_length_bytes();
  virtual int save_image (char *path, int error_ok = 0);
  virtual int getline (uchar *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int putline (uchar *buf, int x, int y, int n=0, int to_index=0, int band=0);
  virtual int getline (ushort *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int getline (short *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  //  virtual int getline (float *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int putline (ushort *buf, int x, int y, int n=0, int to_index=0, int band=0);
  virtual image* map_image (int xdim, int ydim, IMAGE_ELEMENT_TYPE el_type,
			    int samples_per_pixel,
			    MAP_ELEMENT_TYPE *xmap, MAP_ELEMENT_TYPE *ymap);
};
   
template <class eltype>
class array_image: public array_image_base
{public:
  IMAGE_ELEMENT_TYPE element_type();
  eltype *array;
  eltype *make_image_array() {
   return (eltype *) make_vector(image_array_length_bytes());
  }
  array_image(int sx, int sy, IMAGE_ELEMENT_TYPE elemtype, int samples_per_pixel,
	      int bx, int by, int pbx, int no_alloc, eltype *a = (eltype *) 0);
  array_image(int sx, int sy, IMAGE_ELEMENT_TYPE elemtype, int samples_per_pixel,
	      int bx, int by, int pbx, eltype *a, MAP_ELEMENT_TYPE *xmap, MAP_ELEMENT_TYPE *ymap);
  // get_image_tile assumes x and y are aligned with tile origin.
  virtual void* get_image_tile (int x, int y) {
    return ((void *)&array[iref_index(x,y)]);}

  inline int _iref(int x,int y) const {return ((int) array[iref_index(x,y)]);}
  inline double _diref(int x,int y) const {return ((double) array[iref_index(x,y)]);}
  inline void _iset(int x, int y, int val) {array[iref_index(x,y)]= (eltype) val;}
  inline void _iset(int x, int y, double val) {array[iref_index(x,y)]= (eltype) val;}
  inline int _aref(int off) const {return ((int) array[off]);}
  inline double _interpolate_iref (double x, double y) {
    int ix = (int) x; int iy = (int) y;
    double wx = x - (double) ix; 
    double wy = y - (double) iy;
    double v00 = _diref (ix, iy)   , v10 = _diref (ix+1, iy);
    double v01 = _diref (ix, iy+1) , v11 = _diref (ix+1, iy+1);
    double interp0 = v00 + wx*(v10 - v00);
    double interp1 = v01 + wx*(v11 - v01);
    return (interp0 + wy*(interp1 - interp0));
  }
  int iref(int x, int y);
  double interpolate_iref(double x, double y);
  double diref(int x, int y);
  void iset(int x, int y, int val);
  void iset(int x, int y, double val);

  virtual void* image_array();
  virtual void set_image_array(void *array);
  virtual int getline (int *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int putline (int *buf, int x, int y, int n=0, int to_index=0, int band=0);
  virtual int getline (double *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int putline (double *buf, int x, int y, int n=0, int to_index=0, int band=0);
  virtual int getline (float *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int putline (float *buf, int x, int y, int n=0, int to_index=0, int band=0);
  virtual int getcolumn (int *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int getcolumn (double *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int getcolumn (float *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int putcolumn (int *buf, int x, int y, int n=0, int to_index=0, int band=0);
  virtual int putcolumn (double *buf, int x, int y, int n=0, int to_index=0, int band=0);
  virtual int putcolumn (float *buf, int x, int y, int n=0, int to_index=0, int band=0);

  // Mod (CIC) 10/26/2007 - we appear to need this in the array_image
  // class as well.  I hate C++ with a passion:

  //The void cases define getline and putline of for buf same element
  //type as image.  
  // THIS IS AN UNSAFE AND UGLY HACK AND SHOULD BE FIXED.

  int getline (void *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0)
    {return getline((eltype *)buf, x, y, n, to_index, dx);}
  int putline (void *buf, int x, int y, int n=0, int to_index=0, int band=0)
    {return putline((eltype *)buf, x, y, n, to_index);} 

  };

template <class eltype>
class band_interleaved_array_image: public array_image_base
{public:
  IMAGE_ELEMENT_TYPE element_type();
  eltype *array;
  eltype *make_image_array() {
   return (eltype *) make_vector(image_array_length_bytes());
  }
  band_interleaved_array_image(int sx, int sy, IMAGE_ELEMENT_TYPE elemtype, int samples_per_pixel,
	      int bx, int by, int pbx, int no_alloc, eltype *a = (eltype *) 0);
  band_interleaved_array_image(int sx, int sy, IMAGE_ELEMENT_TYPE elemtype, int samples_per_pixel,
	      int bx, int by, int pbx, eltype *array, MAP_ELEMENT_TYPE *xmap, MAP_ELEMENT_TYPE *ymap);
  // get_image_tile assumes x and y are aligned with tile origin.
  virtual void* get_image_tile (int x, int y) {
    return ((void *)&array[iref_index(x,y)]);}

  inline void _viref(int x,int y, eltype val) {val = array[iref_index(x,y)];}
  inline void _iset(int x, int y, eltype val) {array[iref_index(x,y)]= val;}
  inline eltype* _aref(int off) const {return (&array[off]);}
  void viref(int x, int y, eltype val);
  void iset(int x, int y, eltype val);

  virtual void* image_array();
  virtual void set_image_array(void *array);
  // The void cases define getline and putline of for buf same element type as image.
  // THIS IS AN UNSAFE AND UGLY HACK AND SHOULD BE FIXED.
  virtual int getline (void *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0)
    {return getline((eltype *)buf, x, y, n, to_index, dx);}
  virtual int putline (void *buf, int x, int y, int n=0, int to_index=0, int band=0)
    {return putline((eltype *)buf, x, y, n, to_index);} 

  virtual int getline (eltype *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int putline (eltype *buf, int x, int y, int n=0, int to_index=0, int band=0);
  virtual int getcolumn (eltype *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int putcolumn (eltype *buf, int x, int y, int n=0, int to_index=0, int band=0);

};


image * make_array_image (int xdim, int ydim, 
			  IMAGE_ELEMENT_TYPE element_type = IMG_UNSIGNED_8BIT,
			  int samples_per_pixel = 1,
			  int block_xdim = 0, int block_ydim = 0, 
			  int padded_block_xdim = 0, int no_alloc = 0
			  );

image * make_band_interleaved_array_image (int xdim, int ydim, 
					   IMAGE_ELEMENT_TYPE element_type = IMG_UNSIGNED_8BIT,
					   int samples_per_pixel = 0,
					   int block_xdim = 0, int block_ydim = 0, 
					   int padded_block_xdim = 0, int no_alloc = 0);

int init_array_image_fn();

void array_image_default_block_dims(int sx, int sy, int *bx, int *by);

#ifdef LISP_IMAGE_ARRAYS

#define make_lisp_array_image FREEDIUS_GLOBAL(make_lisp_array_image)

extern "C" {

void make_lisp_array_image (void* img, int xdim, int ydim, int imgsize, int element_type_code, int samples_per_pixel);

} // end extern "C"

#endif /* LISP_IMAGE_ARRAYS */



#if 0 // unused

#define GENERATE_GENERIC_UNARY_ARRAY_IMAGE_SWITCH(BODY)\
 switch (img->element_type)\
   {case IMG_UNSIGNED_8BIT: \
     {array_image<unsigned char> *img = (array_image<unsigned char> *) img0;\
     BODY;}\
   case IMG_UNSIGNED_16BIT: \
     {array_image<unsigned short> *img = (array_image<unsigned short> *) img0;\
     BODY;}\
   case IMG_UNSIGNED_32BIT: \
     {array_image<unsigned int> *img = (array_image<int> *) img0;\
     BODY;}\
   case IMG_SIGNED_16BIT: \
     {array_image<signed short> *img = (array_image<signed short> *) img0;\
     BODY;}\
   case IMG_SIGNED_32BIT: \
     {array_image<signed int> *img = (array_image<signed int> *) img0;\
     BODY;}\
   case IMG_RGBA8: \
     {array_image<RGBA8_PIXEL> *img = (array_image<RGBA8_PIXEL> *) img0;\
     BODY;}\
   case IMG_RGB8: \
     {array_image<RGB8_PIXEL> *img = (array_image<RGB8_PIXEL> *) img0;\
     BODY;}\
   case IMG_SINGLE_FLOAT:\
          {array_image<float> *img = (array_image<float> *) img0;\
     BODY;}\
   case IMG_DOUBLE_FLOAT:\
          {array_image<double> *img = (array_image<double> *) img0;\
     BODY;}\
   default: error("Unhandled image element_type.");\
   }

// arglist of FN_HDR must begin with "image *img"
#define GENERATE_GENERIC_UNARY_ARRAY_IMAGE_OPERATOR(FN_HDR,BODY)\
image * FN_HDR\
{image *img0 = img;\
 image *result_image;\
 GENERATE_GENERIC_UNARY_ARRAY_IMAGE_SWITCH(BODY);\
 return(result_image);\
}


// Is this next used?
#undef GENERATE_GENERIC_UNARY_IMAGE_OPERATOR
#define GENERATE_GENERIC_UNARY_IMAGE_OPERATOR(FN_HDR,BODY)\
GENERATE_GENERIC_UNARY_ARRAY_IMAGE_OPERATOR(FN_HDR,BODY)

#endif // unused



END_NAMESPACE_FREEDIUS

#endif /* ! __array_image_h */
