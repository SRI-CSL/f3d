#include "array-image.h"
#include "namespace.h"

// This file appears to clean wrt the modularity of the img subsystem, ie. 

BEGIN_NAMESPACE_FREEDIUS

template <class T>
void* array_image<T>::image_array() 
{return((void *) array);}

template <class T>
void array_image<T>::set_image_array(void* arr) 
{array = (T *) arr;}

template <class T>
int 
array_image<T>::iref(int x, int y) 
{return _iref(x,y);}

template <class T>
double 
array_image<T>::diref(int x, int y) 
{return (double)_diref(x,y);}

template <class T>
void 
array_image<T>::iset(int x, int y, int val) 
{_iset(x, y, val);}

template <class T>
void 
array_image<T>::iset(int x, int y, double val) 
{_iset(x, y, val);}

template <class T>
double 
array_image<T>::interpolate_iref(double x, double y)
 {return _interpolate_iref(x,y);}

#if 0
// This is needed on SGI version -- otherwise the previous templates do not
// get instantiated.

template <class T>
void donothing (array_image<T> *img)
{
  int i = img->iref(0,0);
  double d = img->diref(0,0);
  double d2 = img->interpolate_iref(0.1, 2.3);
  img->iset(0,0,i);
  img->iset(0,0,d);
  img->set_image_array(img->image_array());  
}

// Force the template fns to be generated.
void donothing(array_image<unsigned char> *img8u, 
	       array_image<unsigned short> *img16u, 
	       array_image<unsigned int> *img32u, 
	       array_image<int> *img32,
	       array_image<float> *imgf,
	       array_image<double> *imgd)
{
  donothing(img8u);
  donothing(img16u);
  donothing(img32u);
  donothing(img32);
  donothing(imgf);
  donothing(imgd);
}
#endif
	

template <class T>
array_image<T>::array_image(int sx, int sy, IMAGE_ELEMENT_TYPE elemtype, int samples_per_pixel,
			    int bx, int by, int pbx, 
			    T *array, MAP_ELEMENT_TYPE *xmap, MAP_ELEMENT_TYPE *ymap)
  : array_image_base(sx, sy, elemtype, samples_per_pixel, bx, by, pbx, 0)
{
  this->array = array;
  this->xmap = xmap;
  this->ymap = ymap;
  this->default_construct_maps(block_size());
}

// This this class really needed?  Yes, in order to have viref viset accessors, which are not used?
template <class T>
band_interleaved_array_image<T>::band_interleaved_array_image
   (int sx, int sy, IMAGE_ELEMENT_TYPE elemtype, int samples_per_pixel,
    int bx, int by, int pbx, T *array, MAP_ELEMENT_TYPE *xmap, MAP_ELEMENT_TYPE *ymap)
  : array_image_base(sx, sy, elemtype, samples_per_pixel, bx, by, pbx, 0)
{
  this->array = array;
  this->xmap = xmap;
  this->ymap = ymap;
  this->default_construct_maps(block_size());
}

extern "C" {

  // called from Lisp to actually make the image
image *FREEDIUS_GLOBAL(lisp_make_array_image)
  (int xdim, int ydim, 
   IMAGE_ELEMENT_TYPE element_type,
   int samples_per_pixel,
   int block_xdim, int block_ydim, int padded_block_xdim,
   void *array, MAP_ELEMENT_TYPE *xmap, MAP_ELEMENT_TYPE *ymap)
{
  tprintf(4,"In lisp_make_array_image\n");
  int ele_size = image_element_size_from_type(element_type);
  tprintf(5,"   ele_size = %d\n", ele_size);
  tprintf(5,"   block_xdim = %d\n", block_xdim);
  tprintf(5,"   block_ydim = %d\n", block_ydim);

  if (block_xdim==0 || block_ydim==0) 
    array_image_default_block_dims(xdim, ydim, &block_xdim, &block_ydim);
  if (padded_block_xdim == 0) 
    padded_block_xdim = pad_to_multiple(block_xdim, BITS_PER_WORD/ele_size);
  switch (element_type) {
  case IMG_UNSIGNED_8BIT: 
    tprintf(3, "element type is IMG_UNSIGNED_8BIT\n");
    return new array_image<unsigned char>(xdim, ydim, element_type, samples_per_pixel, block_xdim, block_ydim, padded_block_xdim, (unsigned char *)array, xmap, ymap);
  case IMG_UNSIGNED_16BIT:
    tprintf(3, "element type is IMG_UNSIGNED_8BIT\n");
    return new array_image<unsigned short>(xdim, ydim, element_type, samples_per_pixel, block_xdim, block_ydim, padded_block_xdim, (unsigned short *)array, xmap, ymap);
  case IMG_SIGNED_16BIT:
    tprintf(3, "element type is IMG_UNSIGNED_8BIT\n");
    return new array_image<short>(xdim, ydim, element_type, samples_per_pixel, block_xdim, block_ydim, padded_block_xdim, (signed short *)array, xmap, ymap);
  case IMG_SIGNED_32BIT:
    tprintf(3, "element type is IMG_UNSIGNED_8BIT\n");
    return new array_image<signed int>(xdim, ydim, element_type, samples_per_pixel, block_xdim, block_ydim, padded_block_xdim, (signed int *)array, xmap, ymap);
  case IMG_UNSIGNED_32BIT:
    tprintf(3, "element type is IMG_UNSIGNED_8BIT\n");
    return new array_image<unsigned int>(xdim, ydim, element_type, samples_per_pixel, block_xdim, block_ydim, padded_block_xdim, (unsigned int *)array, xmap, ymap);
  case IMG_SINGLE_FLOAT:
    tprintf(3, "element type is IMG_UNSIGNED_8BIT\n");
    return new array_image<float>(xdim, ydim, element_type, samples_per_pixel, block_xdim, block_ydim, padded_block_xdim, (float *)array, xmap, ymap);
  case IMG_DOUBLE_FLOAT:
    tprintf(3, "element type is IMG_UNSIGNED_8BIT\n");
    return new array_image<double>(xdim, ydim, element_type, samples_per_pixel, block_xdim, block_ydim, padded_block_xdim, (double *)array, xmap, ymap);
  case IMG_RGB8:
    return new band_interleaved_array_image<RGB8_PIXEL>(xdim, ydim, element_type, 1, block_xdim, block_ydim, padded_block_xdim, (RGB8_PIXEL *)array, xmap, ymap);
  case IMG_RGBA8:
    return new band_interleaved_array_image<RGBA8_PIXEL>(xdim, ydim, element_type, 1, block_xdim, block_ydim, padded_block_xdim, (RGBA8_PIXEL *)array, xmap, ymap);  default: 
    tprintf(3, "element type is IMG_UNSIGNED_8BIT\n");
    //return make_band_interleaved_array_image(xdim, ydim, element_type, samples_per_pixel, block_xdim, block_ydim, padded_block_xdim, no_alloc);
  }
}

} // end extern "C"

#include "lisp-callback.h"

// This is needed in order to generate the callback to Lisp.
DEFCALLBACK(image *, FREEDIUS_GLOBAL(make_array_image_callback),
	    (int xdim, int ydim, 
	     IMAGE_ELEMENT_TYPE element_type,
	     int samples_per_pixel,
	     int block_xdim, int block_ydim, int padded_block_xdim),
	    (xdim, ydim, element_type, samples_per_pixel, block_xdim, block_ydim, padded_block_xdim));

// Call lisp to make decisions about block sizes, etc.
// FIXME:  broken when used by array_image_base::map_image with no_alloc=1 -- all decisions have been made
image * make_array_image
(int xdim, int ydim, 
 IMAGE_ELEMENT_TYPE element_type,
 int samples_per_pixel,
 int block_xdim, int block_ydim, int padded_block_xdim, int no_alloc)
{
  return FREEDIUS_GLOBAL(make_array_image_callback)(xdim, ydim, element_type, samples_per_pixel, block_xdim, block_ydim, padded_block_xdim);
}



// this sucks
#define GEN_ELEMENT_TYPE(image_class, type_code)	\
 template <>  IMAGE_ELEMENT_TYPE image_class::element_type () {return type_code;}

GEN_ELEMENT_TYPE(array_image<unsigned char>, IMG_UNSIGNED_8BIT);
GEN_ELEMENT_TYPE(array_image<unsigned short>, IMG_UNSIGNED_16BIT);
GEN_ELEMENT_TYPE(array_image<signed short>,IMG_SIGNED_16BIT);
GEN_ELEMENT_TYPE(array_image<signed int>,IMG_SIGNED_32BIT);
GEN_ELEMENT_TYPE(array_image<unsigned int>,IMG_UNSIGNED_32BIT);
GEN_ELEMENT_TYPE(array_image<float>,IMG_SINGLE_FLOAT);
GEN_ELEMENT_TYPE(array_image<double>,IMG_DOUBLE_FLOAT);
GEN_ELEMENT_TYPE(band_interleaved_array_image<RGB8_PIXEL>,IMG_RGB8);
GEN_ELEMENT_TYPE(band_interleaved_array_image<RGBA8_PIXEL>,IMG_RGBA8);
  
#undef GEN_ELEMENT_TYPE


#define COPY_ELEMENTS_SPREAD_SRC(fp, tp, n, TO_TYPE, dx)\
{int i2 = n;\
 if (i2 >= 8)\
   {do { *(tp)   = (TO_TYPE) *(fp); fp+=dx;\
	 *(tp+1) = (TO_TYPE) *(fp); fp+=dx;\
	 *(tp+2) = (TO_TYPE) *(fp); fp+=dx;\
	 *(tp+3) = (TO_TYPE) *(fp); fp+=dx;\
	 *(tp+4) = (TO_TYPE) *(fp); fp+=dx;\
	 *(tp+5) = (TO_TYPE) *(fp); fp+=dx;\
	 *(tp+6) = (TO_TYPE) *(fp); fp+=dx;\
	 *(tp+7) = (TO_TYPE) *(fp); fp+=dx;\
	 i2 = i2-8; tp +=8;}\
       while (i2 >= 8);}\
 if (i2 > 0)\
     {do {*tp++ = (TO_TYPE) *fp; i2--; fp+=dx;}\
         while (i2 > 0);}}

#define COPY_IMAGE_LINE(fp, tp, n, TO_TYPE)\
{int i2 = n;\
 if (i2 >= 8)\
   {do { *(tp)   = (TO_TYPE) *(fp);\
	 *(tp+1) = (TO_TYPE) *(fp+1);\
	 *(tp+2) = (TO_TYPE) *(fp+2);\
	 *(tp+3) = (TO_TYPE) *(fp+3);\
	 *(tp+4) = (TO_TYPE) *(fp+4);\
	 *(tp+5) = (TO_TYPE) *(fp+5);\
	 *(tp+6) = (TO_TYPE) *(fp+6);\
	 *(tp+7) = (TO_TYPE) *(fp+7);\
	 i2 = i2-8; tp +=8; fp +=8;}\
       while (i2 >= 8);}\
 if (i2 > 0)\
     {do {*tp++ = (TO_TYPE) *fp++; i2--;}\
        while (i2 > 0);}}

// for setting breakpoints
void foo()
{}
#if 0
template <class image_type, class ELEMENT_TYPE, class BUFFER_TYPE>
//inline
int array_getlinecol(image_type *img, ELEMENT_TYPE *ap, 
			    MAP_ELEMENT_TYPE *xp, MAP_ELEMENT_TYPE yoff, 
			    BUFFER_TYPE *buf, int n, int dx, int blkcpy)
{
  int n8, i, j; 
  int x=0;
  int spp = img->samples_per_pixel;
  MAP_ELEMENT_TYPE *xm = xp;
  MAP_ELEMENT_TYPE *xmp = xm;
  BUFFER_TYPE *tp = buf;
  int block_size = img->block_size();

  if (0 == 1) ignore(img); // what the ...?
  i = 0;
  ap = ap + MAP_ELEMENT_SHIFT(yoff);

  /* Sparc compiles to 29 instrs per iteration 3.625 instrs/pixel = 3n+5 */ 
  /* Pentium compiles to 44 instrs per */
  //  foo();

  if (blkcpy == 1 && dx==1) {
    if (spp==1) { // optimized line access for scalar images
      unsigned int ncpy;
      ELEMENT_TYPE *pptmp;
      int bx = img->block_xdim;
      ncpy = bx - (MAP_ELEMENT_SHIFT(xmp[0]) % block_size);
      // ncpy is the number of pixels remaining in first block
      while (n > 0) {
	if (ncpy > n) ncpy = n;
	pptmp = ap+MAP_ELEMENT_SHIFT(xmp[0]); 
	COPY_IMAGE_LINE(pptmp, tp, ncpy, BUFFER_TYPE);
	xmp+= ncpy; n = n - ncpy; ncpy = bx; 
      }} 
    else { // spp > 1 optimized line access with elements spread
      unsigned int ncpy;
      ELEMENT_TYPE *pptmp;
      int bx = img->block_xdim;
      //      ncpy = bx - (MAP_ELEMENT_SHIFT(xmp[0]) % (spp*block_size))/spp;
      ncpy = bx - (MAP_ELEMENT_SHIFT(xmp[0])/spp) % block_size;
      // ncpy is the number of pixels remaining in first block
      while (n > 0) {
	if (ncpy > n) ncpy = n;
	pptmp = ap+MAP_ELEMENT_SHIFT(xmp[0]); 
	COPY_ELEMENTS_SPREAD_SRC(pptmp, tp, ncpy, BUFFER_TYPE, spp);
	xmp+= ncpy; n = n - ncpy; ncpy = bx; 
      }}}
  // FIXME?  This can probably be made to work for spp>1
  else if (blkcpy==2 && dx==1 &&spp==1) { // optimized column access
    int ncpy;
    ELEMENT_TYPE *pptmp;
    int by = - img->block_ydim;
    int pbx = img->padded_block_xdim;
    int del =-pbx; 
    // change to int del = -pbx*spp; ?
    ncpy = 1+(MAP_ELEMENT_SHIFT(xmp[0]) % block_size)/pbx;  
    // change to ncpy = 1+((MAP_ELEMENT_SHIFT(xmp[0])/spp) % block_size)/pbx; ?
    while (n > 0) {
      if (ncpy > n) ncpy = n;
      pptmp = ap+MAP_ELEMENT_SHIFT(xmp[0]);
      COPY_ELEMENTS_SPREAD_SRC(pptmp, tp, ncpy, BUFFER_TYPE, del);
      xmp+=ncpy; n = n - ncpy; ncpy = by; 
    }}
  else { 
    //    ap = ap + MAP_ELEMENT_SHIFT(yoff);
    n8 = n>>3; n = n-(n8<<3);
    if (dx==1) {
      for (j=n8; j>0 ; j--) {
	*(tp)   = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xmp[0]));
	*(tp+1) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xmp[1]));
	*(tp+2) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xmp[2]));
	*(tp+3) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xmp[3]));
	*(tp+4) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xmp[4]));
	*(tp+5) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xmp[5]));
	*(tp+6) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xmp[6]));
	*(tp+7) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xmp[7]));
	tp+=8;xmp+=8;
      } 
    } else { // This indexing scheme wins on Sparc - 3 instrs per pixels
      MAP_ELEMENT_TYPE *xm1 = xm+dx;
      MAP_ELEMENT_TYPE *xm2 = xm1+dx;
      MAP_ELEMENT_TYPE *xm3 = xm2+dx;
      MAP_ELEMENT_TYPE *xm4 = xm3+dx;
      MAP_ELEMENT_TYPE *xm5 = xm4+dx;
      MAP_ELEMENT_TYPE *xm6 = xm5+dx;
      MAP_ELEMENT_TYPE *xm7 = xm6+dx;
      int dx8 = dx << 3;
      for (j=n8; j>0 ; j--) {
	*(tp)   = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xm[x]));
	*(tp+1) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xm1[x]));
	*(tp+2) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xm2[x]));
	*(tp+3) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xm3[x]));
	*(tp+4) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xm4[x]));
	*(tp+5) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xm5[x]));
	*(tp+6) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xm6[x]));
	*(tp+7) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xm7[x]));
	tp+=8;x+=dx8;
      }
      xmp = xm+x;
    }
    for(; n>0; n--) {
      *tp++ = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xmp[0]));
      xmp+=dx;
    }}
  return IMAGE_GET_PUT_OK;
}
#else
template <class image_type, class ELEMENT_TYPE, class BUFFER_TYPE>
//inline
int array_getlinecol(image_type *img, ELEMENT_TYPE *ap, 
			    MAP_ELEMENT_TYPE *xp, MAP_ELEMENT_TYPE yoff, 
			    BUFFER_TYPE *buf, int n, int dx, int blkcpy)
{
  int n8, i, j; 
  int x=0;
  int spp = img->samples_per_pixel;
  MAP_ELEMENT_TYPE *xm = xp;
  MAP_ELEMENT_TYPE *xmp = xm;
  BUFFER_TYPE *tp = buf;
  int block_size = img->block_size();

  if (0 == 1) ignore(img); // what the ...?
  i = 0;
  ap = ap + MAP_ELEMENT_SHIFT(yoff);

  /* Sparc compiles to 29 instrs per iteration 3.625 instrs/pixel = 3n+5 */ 
  /* Pentium compiles to 44 instrs per */
  //  foo();
  if (spp==1) {
    if (blkcpy == 1 && dx==1) {
      unsigned int ncpy;
      ELEMENT_TYPE *pptmp;
      int bx = img->block_xdim;
      ncpy = bx - (MAP_ELEMENT_SHIFT(xmp[0]) % block_size);
      // ncpy is the number of pixels remaining in first block
      while (n > 0) {
	if (ncpy > n) ncpy = n;
	pptmp = ap+MAP_ELEMENT_SHIFT(xmp[0]); 
	COPY_IMAGE_LINE(pptmp, tp, ncpy, BUFFER_TYPE);
	xmp+= ncpy; n = n - ncpy; ncpy = bx; 
      }}
    // FIXME?  This can probably be made to work for spp>1
    else if (blkcpy==2 && dx==1) { // optimized column access
      int ncpy;
      ELEMENT_TYPE *pptmp;
      int by = - img->block_ydim;
      int pbx = img->padded_block_xdim;
      int del =-pbx; 
      // change to int del = -pbx*spp; ?
      ncpy = 1+(MAP_ELEMENT_SHIFT(xmp[0]) % block_size)/pbx;  
      // change to ncpy = 1+((MAP_ELEMENT_SHIFT(xmp[0])/spp) % block_size)/pbx; ?
      while (n > 0) {
	if (ncpy > n) ncpy = n;
	pptmp = ap+MAP_ELEMENT_SHIFT(xmp[0]);
	COPY_ELEMENTS_SPREAD_SRC(pptmp, tp, ncpy, BUFFER_TYPE, del);
	xmp+=ncpy; n = n - ncpy; ncpy = by; 
      }}
    else { 
      //    ap = ap + MAP_ELEMENT_SHIFT(yoff);
      n8 = n>>3; n = n-(n8<<3);
      if (dx==1) {
	for (j=n8; j>0 ; j--) {
	  *(tp)   = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xmp[0]));
	  *(tp+1) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xmp[1]));
	  *(tp+2) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xmp[2]));
	  *(tp+3) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xmp[3]));
	  *(tp+4) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xmp[4]));
	  *(tp+5) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xmp[5]));
	  *(tp+6) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xmp[6]));
	  *(tp+7) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xmp[7]));
	  tp+=8;xmp+=8;
	} 
      } else { // This indexing scheme wins on Sparc - 3 instrs per pixels
	MAP_ELEMENT_TYPE *xm1 = xm+dx;
	MAP_ELEMENT_TYPE *xm2 = xm1+dx;
	MAP_ELEMENT_TYPE *xm3 = xm2+dx;
	MAP_ELEMENT_TYPE *xm4 = xm3+dx;
	MAP_ELEMENT_TYPE *xm5 = xm4+dx;
	MAP_ELEMENT_TYPE *xm6 = xm5+dx;
	MAP_ELEMENT_TYPE *xm7 = xm6+dx;
	int dx8 = dx << 3;
	for (j=n8; j>0 ; j--) {
	  *(tp)   = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xm[x]));
	  *(tp+1) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xm1[x]));
	  *(tp+2) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xm2[x]));
	  *(tp+3) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xm3[x]));
	  *(tp+4) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xm4[x]));
	  *(tp+5) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xm5[x]));
	  *(tp+6) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xm6[x]));
	  *(tp+7) = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xm7[x]));
	  tp+=8;x+=dx8;
	}
	xmp = xm+x;
      }
      for(; n>0; n--) {
	*tp++ = (BUFFER_TYPE) *(ap+MAP_ELEMENT_SHIFT(xmp[0]));
	xmp+=dx;
      }}
  }
  else // spp>1
    {   
      for(; n>0; n--) {
	ELEMENT_TYPE *from_ptr = ap+MAP_ELEMENT_SHIFT(*xmp++);
	for (int b=0; b<spp; b++)
	  *tp++ = (BUFFER_TYPE) *(from_ptr++);
      }
    }

  return IMAGE_GET_PUT_OK;
}
#endif

/*
Sat Feb 16 2002
This has not been optimized to access tiles efficiently when BLKCPY=1 OR BLKCPY=2
*/

template <class image_type, class ELEMENT_TYPE, class BUFFER_TYPE>
//inline
int array_putlinecol(image_type *img, ELEMENT_TYPE *ap, 
			    MAP_ELEMENT_TYPE *xp, MAP_ELEMENT_TYPE yoff,
			    BUFFER_TYPE *buf, int n, int blkcpy)
{
  int n8, i, j; 
  int x=0;
  int spp=img->samples_per_pixel;
  MAP_ELEMENT_TYPE *xm = xp;
  MAP_ELEMENT_TYPE *xmp;
  BUFFER_TYPE *bp;
  if (0 == 1) ignore(img);
  i = 0;
  ap = ap + MAP_ELEMENT_SHIFT(yoff);
  /* Sparc compiles to xx instrs per iteration  xx instrs/pixel */ 
  /* Pentium compiles to xx instrs per */
  bp = buf+i;
  xmp= xm+x;
  if (spp==1) {  
    n8 = n>>3; n = n-(n8<<3);

    // This is the most general case
    for (j=n8; j>0 ; j--) {
      *(ap+MAP_ELEMENT_SHIFT(xmp[0])) = (ELEMENT_TYPE)bp[0];
      *(ap+MAP_ELEMENT_SHIFT(xmp[1])) = (ELEMENT_TYPE)bp[1];
      *(ap+MAP_ELEMENT_SHIFT(xmp[2])) = (ELEMENT_TYPE)bp[2];
      *(ap+MAP_ELEMENT_SHIFT(xmp[3])) = (ELEMENT_TYPE)bp[3];
      *(ap+MAP_ELEMENT_SHIFT(xmp[4])) = (ELEMENT_TYPE)bp[4];
      *(ap+MAP_ELEMENT_SHIFT(xmp[5])) = (ELEMENT_TYPE)bp[5];
      *(ap+MAP_ELEMENT_SHIFT(xmp[6])) = (ELEMENT_TYPE)bp[6];
      *(ap+MAP_ELEMENT_SHIFT(xmp[7])) = (ELEMENT_TYPE)bp[7];
      bp+=8; xmp+=8;
    }
    for(; n>0; n--) {
      *(ap+MAP_ELEMENT_SHIFT(*xmp++)) = (ELEMENT_TYPE) *bp++;
    }
  } else // spp>1
    for(; n>0; n--) {
      ELEMENT_TYPE *to_ptr = ap+MAP_ELEMENT_SHIFT(*xmp++);
      for (int b=0; b<spp; b++)
	*(to_ptr++) = (ELEMENT_TYPE) *bp++;
    }


  return IMAGE_GET_PUT_OK;
}


#define DEFINE_GETLINE(T, BUF_TYPE)		\
template <> int T::getline (BUF_TYPE *buf, int x, int y, int n, int to_index, int dx, int band)\
{int blkcpy=0;if ((flags & IMAGE_MAPS_HACKED) ==0) blkcpy=1;\
 if (n==0) n=xdim-to_index;\
 if (y<0 || y>=ydim || x<0 || (x + dx*n) > xdim) \
   return IMAGE_GET_PUT_BAD_BOUNDS;\
 return(array_getlinecol(this, array+band, &xmap[x], ymap[y], &buf[to_index], n, dx, blkcpy));\
}

#define DEFINE_PUTLINE(T, BUF_TYPE)\
template <> int T::putline (BUF_TYPE *buf, int x, int y, int n, int to_index, int band)\
{int blkcpy=0;if ((flags & IMAGE_MAPS_HACKED) ==0) blkcpy=1;\
 if (n==0) n=xdim-to_index;\
 if (y<0 || y>=ydim || x<0 || (x + n) > xdim) \
   return IMAGE_GET_PUT_BAD_BOUNDS;\
 return(array_putlinecol(this, array+band, &xmap[x], ymap[y], &buf[to_index], n, blkcpy));\
}

#define DEFINE_GETPUT(T, BUF_TYPE)\
  DEFINE_GETLINE(T, BUF_TYPE)\
  DEFINE_PUTLINE(T, BUF_TYPE)
    
#define DEFINE_GETPUTS(T)\
  DEFINE_GETPUT(array_image<T>, int)\
  DEFINE_GETPUT(array_image<T>, double)\
  DEFINE_GETPUT(array_image<T>, float)
   
DEFINE_GETPUTS(unsigned char)
DEFINE_GETPUTS(char)
DEFINE_GETPUTS(unsigned short)
DEFINE_GETPUTS(short)
DEFINE_GETPUTS(unsigned int)
DEFINE_GETPUTS(int)
DEFINE_GETPUTS(float)
DEFINE_GETPUTS(double)

#define DEFINE_GETCOLUMN(T, BUF_TYPE)\
template <> int T::getcolumn (BUF_TYPE *buf, int x, int y, int n, int to_index, int dy, int band)\
{int blkcpy=0;if (flags & IMAGE_MAPS_HACKED ==0) blkcpy=2;\
 if (n==0) n=ydim-to_index;\
 if (x<0 || x>=xdim || y<0 || (y + dy*n) > ydim) \
   return IMAGE_GET_PUT_BAD_BOUNDS;\
 return(array_getlinecol(this, array+band, &ymap[y], xmap[x], &buf[to_index], n, dy, blkcpy));\
} 

#define DEFINE_PUTCOLUMN(T, BUF_TYPE)\
template <> int T::putcolumn (BUF_TYPE *buf, int x, int y, int n, int to_index, int band)\
{int blkcpy=0;if (flags & IMAGE_MAPS_HACKED ==0) blkcpy=2;\
 if (n==0) n=ydim-to_index;\
 if (x<0 || x>=xdim || y<0 || (y + n) > ydim) \
   return IMAGE_GET_PUT_BAD_BOUNDS;\
 return(array_putlinecol(this, array+band, &ymap[y], xmap[x], &buf[to_index], n, blkcpy));\
} 

#undef DEFINE_GETPUT
#define DEFINE_GETPUT(T, BUF_TYPE)\
  DEFINE_GETCOLUMN(T, BUF_TYPE)\
  DEFINE_PUTCOLUMN(T, BUF_TYPE)

DEFINE_GETPUTS(unsigned char)
DEFINE_GETPUTS(char)
DEFINE_GETPUTS(unsigned short)
DEFINE_GETPUTS(short)
DEFINE_GETPUTS(unsigned int)
DEFINE_GETPUTS(int)
DEFINE_GETPUTS(float)
DEFINE_GETPUTS(double)

// BAND-INTERLEAVED IMAGE ACCESSORS 

DEFINE_GETLINE(band_interleaved_array_image<RGB8_PIXEL>, RGB8_PIXEL)
DEFINE_GETLINE(band_interleaved_array_image<RGBA8_PIXEL>, RGBA8_PIXEL)
DEFINE_PUTLINE(band_interleaved_array_image<RGB8_PIXEL>, RGB8_PIXEL)
DEFINE_PUTLINE(band_interleaved_array_image<RGBA8_PIXEL>, RGBA8_PIXEL)

DEFINE_GETCOLUMN(band_interleaved_array_image<RGB8_PIXEL>, RGB8_PIXEL)
DEFINE_GETCOLUMN(band_interleaved_array_image<RGBA8_PIXEL>, RGBA8_PIXEL)
DEFINE_PUTCOLUMN(band_interleaved_array_image<RGB8_PIXEL>, RGB8_PIXEL)
DEFINE_PUTCOLUMN(band_interleaved_array_image<RGBA8_PIXEL>, RGBA8_PIXEL)



// Some ad hoc versions of getline/putline.  
// These could be virtual functions of the fully specified classes if I were
// willing to populate the complete cross product of element_types and buffer_types.

template <class ARRAY_IMAGE_CLASS, class BUFFER_TYPE>
int getline_xxx 
  (ARRAY_IMAGE_CLASS *img, BUFFER_TYPE *buf, int x, int y, int n, int to_index, int dx, int band)
{int blkcpy=0;if ((img->flags & IMAGE_MAPS_HACKED) ==0) blkcpy=1;
 if (n==0) n=img->xdim-to_index;
 if (y<0 || y>=img->ydim || x<0 || (x + dx*n) > img->xdim)
   return IMAGE_GET_PUT_BAD_BOUNDS;
 return(array_getlinecol(img, img->array+band, &img->xmap[x],img->ymap[y],&buf[to_index], n,dx, blkcpy));
} 

template <class ARRAY_IMAGE_CLASS, class BUFFER_TYPE>
int putline_xxx
  (ARRAY_IMAGE_CLASS *img, BUFFER_TYPE *buf, int x, int y, int n, int to_index, int band)
{int blkcpy=0;if ((img->flags & IMAGE_MAPS_HACKED) ==0) blkcpy=1;
 if (n==0) n=img->xdim-to_index;
 if (y<0 || y>=img->ydim || x<0 || (x + n) > img->xdim) 
   return IMAGE_GET_PUT_BAD_BOUNDS;
 return(array_putlinecol(img, img->array+band, &img->xmap[x], img->ymap[y], &buf[to_index], n, blkcpy));
} 

int array_image_base::getline (unsigned char *buf, int x, int y, int n, int to_index, int dx, int band)
{switch (element_type()) 
  {case IMG_UNSIGNED_8BIT:
    return(getline_xxx((array_image<unsigned char> *)this, buf, x, y, n, to_index, dx, band));
  case IMG_UNSIGNED_16BIT: // do we really want 16->8 throwing away the high 8 bits?
    return(getline_xxx((array_image<unsigned short> *)this, buf, x, y, n, to_index, dx, band));
  default: error("Unhandled image element_type= %s", image_element_type_name(element_type()));
  }
return 0;
}

int array_image_base::getline (unsigned short *buf, int x, int y, int n, int to_index, int dx, int band)
{switch (element_type()) 
  {case IMG_UNSIGNED_16BIT:
    return(getline_xxx((array_image<unsigned short> *)this, buf, x, y, n, to_index, dx, band));
  default: error("Unhandled image element_type= %s", image_element_type_name(element_type()));
  }
return 0;
}

int array_image_base::getline (short *buf, int x, int y, int n, int to_index, int dx, int band)
{switch (element_type()) 
  {case IMG_SIGNED_16BIT:
    return(getline_xxx((array_image<signed short> *)this, buf, x, y, n, to_index, dx, band));
  default: error("Unhandled image element_type= %s", image_element_type_name(element_type()));
  }
return 0;
}

int array_image_base::putline (unsigned char *buf, int x, int y, int n, int to_index, int band)
{ switch (element_type()) 
  {case IMG_UNSIGNED_8BIT:
    return(putline_xxx((array_image<unsigned char> *)this, buf, x, y, n, to_index, band));
  case IMG_UNSIGNED_16BIT:
    return(putline_xxx((array_image<unsigned short> *)this, buf, x, y, n, to_index, band));
  case IMG_RGB8:
    return(putline_xxx((array_image<unsigned char> *)this, buf, x, y, n, to_index, band));
  default: error("Unhandled image element_type= %s", image_element_type_name(element_type()));
  }
return 0;
}

int array_image_base::putline (unsigned short *buf, int x, int y, 
		   int n, int to_index, int band)
{switch (element_type()) 
  {case IMG_UNSIGNED_16BIT:
    return(putline_xxx((array_image<unsigned short> *)this, buf, x, y, n, to_index, band));
  default: error("Unhandled image element_type= %s", image_element_type_name(element_type()));
  }
return 0;
}

#if 0
int array_image_base::getline (float *buf, int x, int y, 
		   int n, int to_index, int dx, int band)
{switch (element_type()) 
  {case IMG_SINGLE_FLOAT:
    return(getline_xxx((array_image<float> *)this, buf, x, y, n, to_index, dx, band));
  default: error("Unhandled image element_type= %s", image_element_type_name(element_type()));
  }
return 0;
}

int array_image_base::putline (float *buf, int x, int y, 
		   int n, int to_index, int band)
{switch (element_type()) 
  {case IMG_SINGLE_FLOAT:
    return(putline_xxx((array_image<float> *)this, buf, x, y, n, to_index, band));
  default: error("Unhandled image element_type= %s", image_element_type_name(element_type()));
  }
return 0;
}
#endif

// BAND-INTERLEAVED IMAGE METHODS

template <class T>
void* band_interleaved_array_image<T>::image_array() 
{return((void *) array);}

template <class T>
void band_interleaved_array_image<T>::set_image_array(void* arr) 
{array = (T *) arr;}

template <class T>
void
band_interleaved_array_image<T>::viref(int x, int y, T val) 
{_iref(x, y, val);}

template <class T>
void 
band_interleaved_array_image<T>::iset(int x, int y, T val) 
{_iset(x, y, val);}


END_NAMESPACE_FREEDIUS
