
#ifdef __GNUG__
#pragma implementation
#endif

#include "paged-image.h"

#include "cme-error.h"
#include "misc.h"

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

#include "paged-image-subclasses.h"

// blk_size is 1<<tile_offset_bits
void paged_image_base::default_construct_maps(int blk_size)
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
    val = MAP_ELEMENT_WRITE_SHIFT(spp*xlo + xhi*blk_size);
    xmap[k] = val;
  }
  for (y=0; y<ydim; y++) {
    yhi = y / by; ylo = y-yhi*by;
    k = (block_ydim>0)? y : (ydim-1-y);
    val = MAP_ELEMENT_WRITE_SHIFT(spp*pbx*ylo + yhi*blk_size*blks_wide);
    ymap[k] = val;
  }
}

template <class T>
T paged_image<T>::example_element;

template <class T>
T band_interleaved_paged_image<T>::example_element;

template <class ELEMENT_TYPE>
paged_image<ELEMENT_TYPE>::paged_image (int sx, int sy, 
					IMAGE_ELEMENT_TYPE el_type, int samples_per_pixel,
					int bx, int by, int pbx, 
					int no_alloc, int offset_bits)
  :paged_image_base(sx, sy, el_type, samples_per_pixel, bx, by, pbx, no_alloc, offset_bits)
{ 
  if (no_alloc == 0) {
    if (offset_bits==0) offset_bits=cmelog2(samples_per_pixel*block_size());
#if defined(LISP_IMAGE_ARRAYS)
    make_lisp_paged_image(this, sx, sy, el_type, samples_per_pixel, offset_bits);
#endif 
    this->default_construct_maps(1<<TILE_OFFSET_BITS(this));
  }
}
 
/*
make_paged_image creates a paged_image of the appropriate dimensions and element-type.
make_paged_image DOES NOT create a page_handler.  That is the responsibility of the caller.
paged_image is a base class, not an instantiable class.

*/

#define GEN_ELEMENT_TYPE(specializer, type_code)	\
  template <> IMAGE_ELEMENT_TYPE specializer::element_type () {return type_code;}

GEN_ELEMENT_TYPE(paged_image<unsigned char>, IMG_UNSIGNED_8BIT);
GEN_ELEMENT_TYPE(paged_image<unsigned short>, IMG_UNSIGNED_16BIT);
GEN_ELEMENT_TYPE(paged_image<signed short>,IMG_SIGNED_16BIT);
GEN_ELEMENT_TYPE(paged_image<signed int>,IMG_SIGNED_32BIT);
GEN_ELEMENT_TYPE(paged_image<unsigned int>,IMG_UNSIGNED_32BIT);
GEN_ELEMENT_TYPE(paged_image<float>,IMG_SINGLE_FLOAT);
GEN_ELEMENT_TYPE(paged_image<double>,IMG_DOUBLE_FLOAT);
GEN_ELEMENT_TYPE(band_interleaved_paged_image<RGB8_PIXEL>,IMG_RGB8);
GEN_ELEMENT_TYPE(band_interleaved_paged_image<RGBA8_PIXEL>,IMG_RGBA8);
   
#undef GEN_ELEMENT_TYPE

extern paged_image_base *
make_paged_image (int xdim, int ydim, IMAGE_ELEMENT_TYPE element_type, int samples_per_pixel,
		  int block_xdim, int block_ydim, int padded_block_xdim, int no_alloc, int offset_bits)
{
  switch (element_type) {
  case IMG_UNSIGNED_8BIT: 
    return(new paged_image<unsigned char>(xdim, ydim, element_type, samples_per_pixel, block_xdim, block_ydim, padded_block_xdim, no_alloc, offset_bits));
  case IMG_UNSIGNED_16BIT:
    return(new paged_image<unsigned short>(xdim, ydim, element_type, samples_per_pixel, block_xdim, block_ydim, padded_block_xdim, no_alloc, offset_bits));
  case IMG_SIGNED_16BIT:
    return(new paged_image<signed short>(xdim, ydim, element_type, samples_per_pixel, block_xdim, block_ydim, padded_block_xdim, no_alloc, offset_bits));
  case IMG_SIGNED_32BIT:
    return(new paged_image<int>(xdim, ydim, element_type, samples_per_pixel, block_xdim, block_ydim, padded_block_xdim, no_alloc, offset_bits));
  case IMG_UNSIGNED_32BIT:
    return(new paged_image<unsigned int>(xdim, ydim, element_type, samples_per_pixel, block_xdim, block_ydim, padded_block_xdim, no_alloc, offset_bits));
  case IMG_SINGLE_FLOAT:
    return(new paged_image<float>(xdim, ydim, element_type, samples_per_pixel, block_xdim, block_ydim, padded_block_xdim, no_alloc, offset_bits));
  case IMG_DOUBLE_FLOAT:
    return(new paged_image<double>(xdim, ydim, element_type, samples_per_pixel, block_xdim, block_ydim, padded_block_xdim, no_alloc, offset_bits));

  default: 
    return (make_band_interleaved_paged_image(xdim, ydim, element_type, samples_per_pixel, block_xdim, block_ydim, padded_block_xdim, no_alloc, offset_bits));
  }
}



// ******************   GETLINE AND PUTLINE METHODS   ****************** 

// This was developed for easier debugging.
// Using references to pointer vars is ugly.  Perhaps the caller
// should be required to increment the pointers instead.
template <class ELEMENT_TYPE, class BUFFER_TYPE>
void copy_image_line (ELEMENT_TYPE *&fp, BUFFER_TYPE *&tp, int n)
{
  int i2 = n;					
  if (i2 >= 8) {
    do {
      *(tp)   = (BUFFER_TYPE) *(fp);
      *(tp+1) = (BUFFER_TYPE) *(fp+1);
      *(tp+2) = (BUFFER_TYPE) *(fp+2);
      *(tp+3) = (BUFFER_TYPE) *(fp+3);
      *(tp+4) = (BUFFER_TYPE) *(fp+4);
      *(tp+5) = (BUFFER_TYPE) *(fp+5);
      *(tp+6) = (BUFFER_TYPE) *(fp+6);
      *(tp+7) = (BUFFER_TYPE) *(fp+7);
      i2 = i2-8; tp +=8; fp +=8;
    } while (i2 >= 8);
  }
  if (i2 > 0) {
    do {
      *tp++ = (BUFFER_TYPE) *fp++; 
      i2--;
    } while (i2 > 0);
  }
}

template <class ELEMENT_TYPE, class BUFFER_TYPE>
void copy_image_line_spread_src (ELEMENT_TYPE *&fp, BUFFER_TYPE *&tp, int n, int dx)
{
  int i2 = n;					
  if (i2 >= 8) {
    do {
      *(tp)   = (BUFFER_TYPE) *(fp); fp+=dx;
      *(tp+1) = (BUFFER_TYPE) *(fp); fp+=dx;
      *(tp+2) = (BUFFER_TYPE) *(fp); fp+=dx;
      *(tp+3) = (BUFFER_TYPE) *(fp); fp+=dx;
      *(tp+4) = (BUFFER_TYPE) *(fp); fp+=dx;
      *(tp+5) = (BUFFER_TYPE) *(fp); fp+=dx;
      *(tp+6) = (BUFFER_TYPE) *(fp); fp+=dx;
      *(tp+7) = (BUFFER_TYPE) *(fp); fp+=dx;
      i2 = i2-8; tp +=8;
    } while (i2 >= 8);
  }
  if (i2 > 0) {
    do {*tp++ = (BUFFER_TYPE) *fp; i2--; fp+=dx; }
    while (i2 > 0);
  }
}
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

#define COPY_ELEMENTS_SPREAD_DST(fp, tp, n, TO_TYPE, dx)\
{int i2 = n;\
 if (i2 >= 8)\
   {do { *(tp) = (TO_TYPE) *(fp  ); tp+=dx;\
	 *(tp) = (TO_TYPE) *(fp+1); tp+=dx;\
	 *(tp) = (TO_TYPE) *(fp+2); tp+=dx;\
	 *(tp) = (TO_TYPE) *(fp+3); tp+=dx;\
	 *(tp) = (TO_TYPE) *(fp+4); tp+=dx;\
	 *(tp) = (TO_TYPE) *(fp+5); tp+=dx;\
	 *(tp) = (TO_TYPE) *(fp+6); tp+=dx;\
	 *(tp) = (TO_TYPE) *(fp+7); tp+=dx;\
	 i2 = i2-8; fp +=8;}\
       while (i2 >= 8);}\
 if (i2 > 0)\
     {do {*tp = (TO_TYPE) *fp++; i2--; tp+=dx;}\
         while (i2 > 0);}}

// specific to 32 bit paged images
#define SETUP_OFFMASK \
  int offmask = TILE_OFFSET_MASK(img);		\
  int blknumshift = TILE_OFFSET_BITS(img);

// specific to 32 bit paged images
#define COMPUTE_BLKNUM_AND_OFFSET \
   {pixnum = MAP_ELEMENT_SHIFT(yoff + *(xp)); \
    blknum = pixnum>>blknumshift;\
    offset = pixnum & offmask;}


// specific to 32 bit paged images
#define INCR_XP(dx) xp = xp +(dx);

/*
NOTE (1) -- avoiding division for getcolumn case   ncpy = 1+offset/bx

The COPY_ELEMENTS_SPREAD_SRC and COPY_ELEMENTS_SPREAD_DST macros
can be modified to interate from offset downto 0 by -bx, while incrementing ncpy.

This is only important on brain damaged architectures which do not provide 
integer division instructions.

For example:  


#define COPY_ELEMENTS_SPREAD_SRC(fp, tp, offset, TO_TYPE, dx)\
{int i = offset+bx;  ncpy = 0; \
 if (i >= (bx<<3))\
   {do { *(tp)   = (TO_TYPE) *(fp); fp+=dx;\
	 *(tp+1) = (TO_TYPE) *(fp); fp+=dx;\
	 *(tp+2) = (TO_TYPE) *(fp); fp+=dx;\
	 *(tp+3) = (TO_TYPE) *(fp); fp+=dx;\
	 *(tp+4) = (TO_TYPE) *(fp); fp+=dx;\
	 *(tp+5) = (TO_TYPE) *(fp); fp+=dx;\
	 *(tp+6) = (TO_TYPE) *(fp); fp+=dx;\
	 *(tp+7) = (TO_TYPE) *(fp); fp+=dx;\
         i = i-(bx<<3); tp +=8; ncpy+=8}\
       while (i >= 8);}\
 if (i > 0)\
     {do {*tp++ = (TO_TYPE) *fp; i-=bx; ncpy++; fp+=dx;}\
         while (i > 0);}}

*/


// change this next to an inline function of paged_image
#define READ_GETPAGE(fp, element_type)\
{image_page_queue_entry * entry = entry_map[blknum];\
 if (((entry->status) & PAGE_READ_OK_BIT) == 0)\
 entry = (image_page_queue_entry *)page_handler->read_page_fault(blknum);\
 fp = (element_type *) entry->page;}
 
// bp is buf pointer
// pp is page pointer
// blkcpy=0 means pixel-at-a-time access
// blkcpy=1 means access by line
// blkcpy=2 means access by column
template <class IMAGE_TYPE, class ELEMENT_TYPE, class BUFFER_TYPE>
//inline 
int paged_getlinecol
  (IMAGE_TYPE *img, ELEMENT_TYPE &elem,  MAP_ELEMENT_TYPE *xp, MAP_ELEMENT_TYPE yoff, BUFFER_TYPE *buf, int n, int dx, int blkcpy)
{
  int i; 
  int spp = img->samples_per_pixel;
  unsigned int pixnum, blknum, offset;
  int xoff, blstart, ncpy;
  int bx = img->block_xdim;
  int pbx = img->padded_block_xdim;
  ELEMENT_TYPE *ap, *pp, *pptmp;
  image_page_handler *page_handler = img->page_handler;
  page_map* page_map = page_handler;
  image_page_queue_entry * *entry_map = (image_page_queue_entry * *)page_map->array;
  BUFFER_TYPE *bp = buf;
  SETUP_OFFMASK;
  if (spp==1) {
    if (blkcpy == 1 && dx ==1) { // optimized row access
      COMPUTE_BLKNUM_AND_OFFSET; 
      xoff = 0; if (offset > 0) xoff = offset % pbx; 
      blstart = offset - xoff; // begin of row in block
      ncpy = bx - xoff;
      while (n > 0) {
	READ_GETPAGE(pp, ELEMENT_TYPE);
	if (ncpy > n) ncpy = n;
	pptmp = pp+offset; 
	//printf("paged_getlinecol: fp=%p, tp=%p, n=%d, offmask=%d, blknumshift=%d, blknum=%d\n", pptmp, bp, ncpy, offmask, blknumshift, blknum);
	COPY_IMAGE_LINE(pptmp, bp, ncpy, BUFFER_TYPE);
	//copy_image_line(pptmp, bp, ncpy);
	offset = blstart; blknum++;
	n = n - ncpy; ncpy = bx; 
      }} 
    else if (blkcpy == 2 && dx==1) { // optimized column access
      int by = -img->block_ydim;
      int del=-pbx;
      COMPUTE_BLKNUM_AND_OFFSET; 
      ncpy = 1+offset/pbx;	// we can avoid this division  see note (1) 
      while (n > 0) {
	READ_GETPAGE(pp, ELEMENT_TYPE);
	if (ncpy > n) ncpy = n;
	pptmp = pp+offset;
	COPY_ELEMENTS_SPREAD_SRC(pptmp, bp, ncpy, BUFFER_TYPE, del);
	INCR_XP(ncpy);COMPUTE_BLKNUM_AND_OFFSET;
	n = n - ncpy; ncpy = by;
      }}
    else { // general case: do it the slow way
      // really want loop unrolling here
      for (i = 0; i<n; i++) {
	COMPUTE_BLKNUM_AND_OFFSET; INCR_XP(dx);
	READ_GETPAGE(ap, ELEMENT_TYPE);
	*bp++ = (BUFFER_TYPE) *(ap+(offset));
      }
    }}
  else // spp>1
    {
      for (i = 0; i<n; i++) {
	COMPUTE_BLKNUM_AND_OFFSET; INCR_XP(dx);
	READ_GETPAGE(ap, ELEMENT_TYPE);
	for (int b=0; b<spp; b++)
	  *bp++ = (BUFFER_TYPE) *(ap+(offset)+b);
      }
    }
  return IMAGE_GET_PUT_OK;
}
    
void brk(void){}


#define WRITE_GETPAGE(fp, IMAGE_ELEMENT_TYPE)\
{image_page_queue_entry * entry = entry_map[blknum];\
 if (((entry->status) & PAGE_WRITE_OK_BIT) == 0)\
    entry = (image_page_queue_entry *)page_handler->write_page_fault(blknum);\
 fp = (IMAGE_ELEMENT_TYPE *) entry->page;}

// blkcpy=0 means pixel-at-a-time access
// blkcpy=1 means access by line
// blkcpy=2 means access by column
template <class IMAGE_TYPE, class ELEMENT_TYPE, class BUFFER_TYPE>
//inline
int paged_putlinecol
  (IMAGE_TYPE *img, ELEMENT_TYPE &elem,  MAP_ELEMENT_TYPE *xp, MAP_ELEMENT_TYPE yoff, BUFFER_TYPE *buf, int n, int blkcpy)
{
  int i; 
  int spp = img->samples_per_pixel; 
  unsigned int pixnum, blknum, offset;
  int xoff, blstart, ncpy;
  int bx = img->block_xdim;
  int pbx = img->padded_block_xdim;
  ELEMENT_TYPE *ap, *pp, *pptmp;
  basic_page_handler *page_handler = img->page_handler;
  page_map* page_map = page_handler;
  image_page_queue_entry * *entry_map = (image_page_queue_entry * *)page_map->array;
  BUFFER_TYPE *bp = buf;
  SETUP_OFFMASK;
  //brk();
  if (spp == 1) {
    if (blkcpy == 1) { // access by line
      COMPUTE_BLKNUM_AND_OFFSET; 
      xoff = 0; if (offset > 0) xoff = offset % pbx;
      blstart = offset - xoff;
      ncpy = bx - xoff;
      while (n > 0) {
	WRITE_GETPAGE(pp, ELEMENT_TYPE);
	if (ncpy > n) ncpy = n;
	pptmp = pp+offset;
	COPY_IMAGE_LINE(bp, pptmp, ncpy, ELEMENT_TYPE);
	offset = blstart; blknum++;
	n = n - ncpy; ncpy = bx; 
      }} 
  
    else if (blkcpy==2 && spp==1) { // optimized column access
      int by = -img->block_ydim;
      int del=-pbx;
      COMPUTE_BLKNUM_AND_OFFSET; 
      ncpy = 1+offset/pbx;	// we can avoid this division  see note (1)
      while (n > 0) {
	WRITE_GETPAGE(pp, ELEMENT_TYPE);
	if (ncpy > n) ncpy = n;
	pptmp = pp+offset;
	COPY_ELEMENTS_SPREAD_DST(bp, pptmp, ncpy, ELEMENT_TYPE, del);
	INCR_XP(ncpy);COMPUTE_BLKNUM_AND_OFFSET;
	n = n - ncpy; ncpy = by;
      }}
    else  { // general case: do it the slow way
      for (i = 0; i<n; i++) {
	COMPUTE_BLKNUM_AND_OFFSET; INCR_XP(1);
	WRITE_GETPAGE(ap, ELEMENT_TYPE);
	*(ap+(offset)) = (ELEMENT_TYPE) *bp++;
      }}}
  else { // spp>1
    for (i = 0; i<n; i++) {
      COMPUTE_BLKNUM_AND_OFFSET; INCR_XP(1);
      WRITE_GETPAGE(ap, ELEMENT_TYPE);
      //if (ap == 0) brk();
      for (int b=0; b<spp; b++)
	*(ap+(offset)+b) = (ELEMENT_TYPE) *bp++;
    }
  }
  return IMAGE_GET_PUT_OK;
}

#if 0
int paged_image<unsigned char>::getline
  (int *buf, int x, int y, int n, int to_index, int dx, int samp)
{int blkcpy=0;
// elem used here as specializer for paged_getlinecol 
 unsigned char elem=0;
 if (n==0) n=xdim-to_index;
 if (y<0 || y>=ydim || x<0 || (x + dx*n) > xdim) 
   return IMAGE_GET_PUT_BAD_BOUNDS;
 return(paged_getlinecol(this, elem, &xmap[x], ymap[y], &buf[to_index], n, dx, blkcpy));
} 
#endif


#define DEFINE_GETLINE(T, BUF_TYPE)\
template <> int T::getline \
  (BUF_TYPE *buf, int x, int y, int n, int to_index, int dx, int samp)\
{int blkcpy=0;if ((flags & IMAGE_MAPS_HACKED) == 0) blkcpy=1;\
 if (n==0) n=xdim-to_index;\
 if (y<0 || y>=ydim || x<0 || (x + dx*n) > xdim) \
   return IMAGE_GET_PUT_BAD_BOUNDS;\
 return(paged_getlinecol(this, example_element, &xmap[x], ymap[y], &buf[to_index], n, dx, blkcpy)); \
} 


#define DEFINE_PUTLINE(T, BUF_TYPE)\
template <> int T::putline (BUF_TYPE *buf, int x, int y, int n, int to_index, int samp)\
{int blkcpy=0;if ((flags & IMAGE_MAPS_HACKED) ==0) blkcpy=1;\
 if (n==0) n=xdim-to_index;\
 if (y<0 || y>=ydim || x<0 || (x + n) > xdim) \
   return IMAGE_GET_PUT_BAD_BOUNDS;\
 return(paged_putlinecol(this, example_element, &xmap[x], ymap[y], &buf[to_index], n, blkcpy));\
} 

#define DEFINE_GETPUT(T, BUF_TYPE)\
  DEFINE_GETLINE(T, BUF_TYPE)\
  DEFINE_PUTLINE(T, BUF_TYPE)
    
#define DEFINE_GETPUTS(T)\
  DEFINE_GETPUT(paged_image<T>, int)\
  DEFINE_GETPUT(paged_image<T>, double)\
  DEFINE_GETPUT(paged_image<T>, float)
   
DEFINE_GETPUTS(unsigned char)
DEFINE_GETPUTS(char)
DEFINE_GETPUTS(unsigned short)
DEFINE_GETPUTS(short)
DEFINE_GETPUTS(int)
DEFINE_GETPUTS(unsigned int)
DEFINE_GETPUTS(float)
DEFINE_GETPUTS(double)

#define DEFINE_GETCOLUMN(T, BUF_TYPE)\
template <> int T::getcolumn \
  (BUF_TYPE *buf, int x, int y, int n, int to_index, int dy, int samp)\
{int blkcpy=0;if (flags & IMAGE_MAPS_HACKED ==0) blkcpy=2;\
 if (n==0) n=ydim-to_index;\
 if (x<0 || x>=xdim || y<0 || (y + dy*n) > ydim) \
   return IMAGE_GET_PUT_BAD_BOUNDS;\
 return(paged_getlinecol(this, example_element, &ymap[y], xmap[x], &buf[to_index], n, dy, blkcpy));\
} 


#define DEFINE_PUTCOLUMN(T, BUF_TYPE)\
template <> int T::putcolumn (BUF_TYPE *buf, int x, int y, int n, int to_index, int samp)\
{int blkcpy=0;if (flags & IMAGE_MAPS_HACKED ==0) blkcpy=2;\
 if (n==0) n=ydim-to_index;\
 if (x<0 || x>=xdim || y<0 || (y + n) > ydim) \
   return IMAGE_GET_PUT_BAD_BOUNDS;\
 return(paged_putlinecol(this, example_element, &ymap[y], xmap[x], &buf[to_index], n, blkcpy));\
} 

#undef DEFINE_GETPUT
#define DEFINE_GETPUT(T, BUF_TYPE)\
  DEFINE_GETCOLUMN(T, BUF_TYPE)\
  DEFINE_PUTCOLUMN(T, BUF_TYPE)
   
DEFINE_GETPUTS(unsigned char)
DEFINE_GETPUTS(char)
DEFINE_GETPUTS(unsigned short)
DEFINE_GETPUTS(short)
DEFINE_GETPUTS(int)
DEFINE_GETPUTS(unsigned int)
DEFINE_GETPUTS(float)
DEFINE_GETPUTS(double)


// The implementation of getline_uchar and putline_uchar is different from
// the implementation of getline and putline because getline_uchar and putline_uchar
// are only supported for only a limited number of image_element_types.
// I don't like this difference, but ...

template <class ELEM_TYPE, class BUFFER_TYPE>
int getline_xxx
(paged_image<ELEM_TYPE> *img, BUFFER_TYPE *buf, int x, int y, int n, int to_index, int dx)
{
  int blkcpy=0;if ((img->flags & IMAGE_MAPS_HACKED) == 0) blkcpy=1;
  if (n==0) n=img->xdim-to_index;
  if (y<0 || y>=img->ydim || x<0 || (x + dx*n) > img->xdim)
    return IMAGE_GET_PUT_BAD_BOUNDS;
  return(paged_getlinecol(img, img->example_element, &img->xmap[x], img->ymap[y], &buf[to_index],
			  n, dx, blkcpy));
} 

template <class ELEM_TYPE, class BUFFER_TYPE>
int putline_xxx
  (paged_image<ELEM_TYPE> *img,  BUFFER_TYPE *buf, 
   int x, int y, int n, int to_index)
{
  int blkcpy=0;if ((img->flags & IMAGE_MAPS_HACKED) == 0) blkcpy=1;
  if (n==0) n=img->xdim-to_index;
  if (y<0 || y>=img->ydim || x<0 || (x + n) > img->xdim) 
    return IMAGE_GET_PUT_BAD_BOUNDS;
  return(paged_putlinecol(img, img->example_element, &img->xmap[x], img->ymap[y], &buf[to_index],
			  n, blkcpy));
} 

int paged_image_base::getline (uchar *buf, int x, int y, 
		   int n, int to_index, int dx, int samp)
{
  switch (image_element_type(this)) {
  case IMG_UNSIGNED_8BIT: 
    return(getline_xxx((paged_image<unsigned char> *)this, buf, x, y, n, to_index, dx));
  case IMG_UNSIGNED_16BIT:  // why support this?
    return(getline_xxx((paged_image<unsigned short> *)this, buf, x, y, n, to_index, dx)); 
  default: //error("Unhandled image element_type.");
    return IMAGE_GET_PUT_UNSUPPORTED_BUFFER_TYPE;
  }
  return 0;
}

int paged_image_base::getline (ushort *buf, int x, int y, 
		   int n, int to_index, int dx, int samp)
{
  switch (image_element_type(this)) {
  case IMG_UNSIGNED_16BIT: 
    return(getline_xxx((paged_image<unsigned short> *)this, buf, x, y, n, to_index, dx)); 
  default: //error("Unhandled image element_type.");
    return IMAGE_GET_PUT_UNSUPPORTED_BUFFER_TYPE;
  }
return 0;
}

int paged_image_base::getline (short *buf, int x, int y, 
		   int n, int to_index, int dx, int samp)
{
  switch (image_element_type(this)) {
  case IMG_SIGNED_16BIT: 
    return(getline_xxx((paged_image<signed short> *)this, buf, x, y, n, to_index, dx)); 
  default: //error("Unhandled image element_type.");
    return IMAGE_GET_PUT_UNSUPPORTED_BUFFER_TYPE;
  }
  return 0;
}


int paged_image_base::getline (float *buf, int x, int y, 
		   int n, int to_index, int dx, int band)
{
  switch (image_element_type(this)) {
  case IMG_SINGLE_FLOAT:
    return(getline_xxx((paged_image<float> *)this,  buf, x, y, n, to_index, dx));
  case IMG_DOUBLE_FLOAT:
    return(getline_xxx((paged_image<double> *)this, buf, x, y, n, to_index, dx));
  default: //error("Unhandled image element_type= %s", image_element_type_name(element_type()));
    return IMAGE_GET_PUT_UNSUPPORTED_BUFFER_TYPE;
  }
  return 0;
}


int paged_image_base::putline (uchar *buf, int x, int y, 
		   int n, int to_index, int samp)
{
  switch (image_element_type(this)) {
  case IMG_UNSIGNED_8BIT: 
    return(putline_xxx((paged_image<unsigned char> *)this, 
		       buf, x, y, n, to_index));
  case IMG_UNSIGNED_16BIT:  // why support this?
    return(putline_xxx((paged_image<unsigned short> *)this,
		       buf, x, y, n, to_index)); 
  default: // error("Unhandled image element_type.");
    return IMAGE_GET_PUT_UNSUPPORTED_BUFFER_TYPE;
  }
  return 0;
}



#if 0
DEFINE_GETCOLUMN(unsigned char, int)
DEFINE_GETCOLUMN(unsigned char, double)
DEFINE_PUTCOLUMN(unsigned char, int)
DEFINE_PUTCOLUMN(unsigned char, double)

DEFINE_GETCOLUMN(unsigned short, int)
DEFINE_GETCOLUMN(unsigned short, double)
DEFINE_PUTCOLUMN(unsigned short, int)
DEFINE_PUTCOLUMN(unsigned short, double)

DEFINE_GETCOLUMN(signed short, int)
DEFINE_GETCOLUMN(signed short, double)
DEFINE_PUTCOLUMN(signed short, int)
DEFINE_PUTCOLUMN(signed short, double)

DEFINE_GETCOLUMN(int, int)
DEFINE_GETCOLUMN(int, double)
DEFINE_PUTCOLUMN(int, int)
DEFINE_PUTCOLUMN(int, double)

DEFINE_GETCOLUMN(unsigned int, int)
DEFINE_GETCOLUMN(unsigned int, double)
DEFINE_PUTCOLUMN(unsigned int, int)
DEFINE_PUTCOLUMN(unsigned int, double)

DEFINE_GETCOLUMN(float, int)
DEFINE_GETCOLUMN(float, double)
DEFINE_PUTCOLUMN(float, int)
DEFINE_PUTCOLUMN(float, double)

DEFINE_GETCOLUMN(double, int)
DEFINE_GETCOLUMN(double, double)
DEFINE_PUTCOLUMN(double, int)
DEFINE_PUTCOLUMN(double, double)
#endif

template <class eltype>
int 
paged_image<eltype>::iref(int x, int y)
 {return _iref(x,y);}

template <class eltype>
double 
paged_image<eltype>::interpolate_iref(double x, double y)
 {return _interpolate_iref(x,y);}

template <class eltype>
double 
paged_image<eltype>::diref(int x, int y)
 {return _diref(x,y);}

template <class eltype>
void 
paged_image<eltype>::iset(int x, int y, int val)
 {_iset(x,y,val);}

template <class eltype>
void 
paged_image<eltype>::iset(int x, int y, double val)
 {_iset(x,y,val);}


template <class eltype>
void donothing (paged_image<eltype> *img)
{
  int i = img->iref(0,0);
  double d = img->diref(0,0);
  double d2 = img->interpolate_iref(0.1, 2.3);
  img->iset(0,0,i);
  img->iset(0,0,d);
}

// This is needed on SGI version -- otherwise the previous templates do not
// get instantiated.

void donothing(paged_image<unsigned char> *img8u, 
	       paged_image<unsigned short> *img16u, 
	       paged_image<unsigned int> *img32u, 
	       paged_image<int> *img32,
	       paged_image<float> *imgf,
	       paged_image<double> *imgd)
{
  donothing(img8u);
  donothing(img16u);
  donothing(img32u);
  donothing(img32);
  donothing(imgf);
  donothing(imgd);
}
	


// PAGED VECTOR IMAGE ACCESSORS 

template <class eltype>
void
band_interleaved_paged_image<eltype>::viref(int x, int y, eltype val) 
{_iref(x, y, val);}

template <class eltype>
void 
band_interleaved_paged_image<eltype>::iset(int x, int y, eltype val) 
{_iset(x, y, val);}

template <class eltype>
band_interleaved_paged_image<eltype>::band_interleaved_paged_image
 (int sx, int sy, IMAGE_ELEMENT_TYPE el_type, int samples_per_pixel,
  int bx, int by, int pbx, int no_alloc, int offset_bits)
  :paged_image_base(sx, sy, el_type, samples_per_pixel, bx, by, pbx, no_alloc, offset_bits)
{ 
  if (no_alloc == 0) {
    if (offset_bits==0) offset_bits=cmelog2(samples_per_pixel*block_size());
#if defined(LISP_IMAGE_ARRAYS)
    make_lisp_paged_image(this, sx, sy, el_type, samples_per_pixel, offset_bits);
#endif 
  if (samples_per_pixel>1) 
    fprintf(stderr, "creating band_interleaved_paged_image: xmap[0]=%d\n", xmap[0]);
  this->default_construct_maps(1<<offset_bits);
  }
}
 

paged_image_base * 
make_band_interleaved_paged_image (int xdim, int ydim, 
				   IMAGE_ELEMENT_TYPE element_type, int samples_per_pixel, 
				   int block_xdim, int block_ydim, int padded_block_xdim, 
				   int no_alloc, int offset_bits)
{
  int ele_size = image_element_size_from_type(element_type);
  switch (element_type) {
  case IMG_RGB8:
    return(new band_interleaved_paged_image<RGB8_PIXEL>(xdim, ydim, element_type, 1, block_xdim, block_ydim, padded_block_xdim, no_alloc, offset_bits));
  case IMG_RGBA8:
    return(new band_interleaved_paged_image<RGBA8_PIXEL>(xdim, ydim, element_type, 1, block_xdim, block_ydim, padded_block_xdim, no_alloc, offset_bits));
  default: return 0;
  }
}

#if 0
#undef DEFINE_GETLINE
#define DEFINE_GETLINE(T, BUF_TYPE)\
template <> int band_interleaved_paged_image<T>::getline \
  (BUF_TYPE *buf, int x, int y, int n, int to_index, int dx, int samp)\
{int blkcpy=0;if ((flags & IMAGE_MAPS_HACKED) == 0) blkcpy=1;\
 if (n==0) n=xdim-to_index;\
 if (y<0 || y>=ydim || x<0 || (x + dx*n) > xdim) \
   return IMAGE_GET_PUT_BAD_BOUNDS;\
 return(paged_getlinecol(this, (T *) 0, &xmap[x], ymap[y], &buf[to_index], n, dx, blkcpy));\
} 

#undef DEFINE_PUTLINE
#define DEFINE_PUTLINE(T, BUF_TYPE)\
template <> int band_interleaved_paged_image<T>::putline (BUF_TYPE *buf, int x, int y, int n, int to_index, int samp)\
{int blkcpy=0;if ((flags & IMAGE_MAPS_HACKED) ==0) blkcpy=1;\
 if (n==0) n=xdim-to_index;\
 if (y<0 || y>=ydim || x<0 || (x + n) > xdim) \
   return IMAGE_GET_PUT_BAD_BOUNDS;\
 return(paged_putlinecol(this, (T *) 0, &xmap[x], ymap[y], &buf[to_index], n, blkcpy));\
} 

#undef DEFINE_GETCOLUMN
#define DEFINE_GETCOLUMN(T, BUF_TYPE)\
template <> int band_interleaved_paged_image<T>::getcolumn \
  (BUF_TYPE *buf, int x, int y, int n, int to_index, int dy, int samp)\
{int blkcpy=0;if ((flags & IMAGE_MAPS_HACKED) ==0) blkcpy=2;\
 if (n==0) n=ydim-to_index;\
 if (x<0 || x>=xdim || y<0 || (y + dy*n) > ydim) \
   return IMAGE_GET_PUT_BAD_BOUNDS;\
 return(paged_getlinecol(this, (T *) 0, &ymap[y], xmap[x], &buf[to_index], n, dy, blkcpy));\
} 

#undef DEFINE_PUTCOLUMN
#define DEFINE_PUTCOLUMN(T, BUF_TYPE)\
template <> int band_interleaved_paged_image<T>::putcolumn (BUF_TYPE *buf, int x, int y, int n, int to_index, int samp)\
{int blkcpy=0;if ((flags & IMAGE_MAPS_HACKED) ==0) blkcpy=2;\
 if (n==0) n=ydim-to_index;\
 if (x<0 || x>=xdim || y<0 || (y + n) > ydim) \
   return IMAGE_GET_PUT_BAD_BOUNDS;\
 return(paged_putlinecol(this, (T *) 0, &ymap[y], xmap[x], &buf[to_index], n, blkcpy));\
} 
#endif


DEFINE_GETLINE(band_interleaved_paged_image<RGB8_PIXEL>, RGB8_PIXEL)
DEFINE_GETLINE(band_interleaved_paged_image<RGBA8_PIXEL>, RGBA8_PIXEL)
DEFINE_PUTLINE(band_interleaved_paged_image<RGB8_PIXEL>, RGB8_PIXEL)
DEFINE_PUTLINE(band_interleaved_paged_image<RGBA8_PIXEL>, RGBA8_PIXEL)


DEFINE_GETCOLUMN(band_interleaved_paged_image<RGB8_PIXEL>, RGB8_PIXEL)
DEFINE_GETCOLUMN(band_interleaved_paged_image<RGBA8_PIXEL>, RGBA8_PIXEL)
DEFINE_PUTCOLUMN(band_interleaved_paged_image<RGB8_PIXEL>, RGB8_PIXEL)
DEFINE_PUTCOLUMN(band_interleaved_paged_image<RGBA8_PIXEL>, RGBA8_PIXEL)

END_NAMESPACE_FREEDIUS
