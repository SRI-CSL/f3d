#include "image.h"
#include "image-ops.h"

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

// BUFFER_TYPE is expected to be a float or double 
template <class BUFFER_TYPE>  
extern image *
box_filter_decimate2_image_all_floats (image *img, BUFFER_TYPE xxx, image *into_image)
{
  int reduce = 2;
  int xdim = image_xdim(img);
  int ydim = image_ydim(img);
  int into_xdim = xdim>>1;
  int into_ydim = ydim>>1;
  BUFFER_TYPE scale = .25;
  
  //if (!into_image) into_image = (image *) make_image(into_xdim, into_ydim, image_element_type(img));
  // BUFFER_TYPE inbuf0[xdim];
  BUFFER_TYPE *inbuf0 = XALLOC(BUFFER_TYPE, xdim);
  // BUFFER_TYPE inbuf1[xdim];
  BUFFER_TYPE *inbuf1 = XALLOC(BUFFER_TYPE, xdim);
  // BUFFER_TYPE outbuf[into_xdim];
  BUFFER_TYPE *outbuf = XALLOC(BUFFER_TYPE, into_xdim);

  require_image_rows foo1(img);
  require_image_rows foo2(into_image);

  int x, x2, y, y2;
  for (y=0, y2=0; y<into_ydim; y++, y2+=2 ) {
    getline(img, inbuf0, 0, y2);
    getline(img, inbuf1, 0, y2+1);
    for (x=0, x2=0; x<into_xdim; x++, x2+=2) 
      outbuf[x] = (inbuf0[x2]+inbuf0[x2+1]+inbuf1[x2]+inbuf1[x2+1]) *scale;
    putline(into_image, outbuf, 0, y);
  }
  xxfree(inbuf0); xxfree(inbuf1); xxfree(outbuf);
  return into_image;      
}

template <class BUFFER_TYPE>  
extern image *
box_filter_decimate2_image_all_ints (image *img, BUFFER_TYPE xxx, image *into_image)
{
  int reduce = 2;
  int xdim = image_xdim(img);
  int ydim = image_ydim(img);
  int into_xdim = xdim>>1;
  int into_ydim = ydim>>1;
  
  //if (!into_image) into_image = (image *) make_image(into_xdim, into_ydim, image_element_type(img));
  // BUFFER_TYPE inbuf0[xdim];
  BUFFER_TYPE *inbuf0 = XALLOC(BUFFER_TYPE, xdim);
  // BUFFER_TYPE inbuf1[xdim];
  BUFFER_TYPE *inbuf1 = XALLOC(BUFFER_TYPE, xdim);
  // BUFFER_TYPE outbuf[into_xdim];
  BUFFER_TYPE *outbuf = XALLOC(BUFFER_TYPE, into_xdim);
  require_image_rows foo1(img);
  require_image_rows foo2(into_image);
  
  int x, x2, y, y2;
  for (y=0, y2=0; y<into_ydim; y++, y2+=2 ) {
    getline(img, inbuf0, 0, y2);
    getline(img, inbuf1, 0, y2+1);
    for (x=0, x2=0; x<into_xdim; x++, x2+=2) 
      outbuf[x] = (inbuf0[x2]+inbuf0[x2+1]+inbuf1[x2]+inbuf1[x2+1]) >>2;
    putline(into_image, outbuf, 0, y);
  }
  xxfree(inbuf0); xxfree(inbuf1); xxfree(outbuf);
  return into_image;      
}


template <class ELEMENT_TYPE>
extern image *
box_filter_decimate2_rgb (image *img, ELEMENT_TYPE *dummy, image *into_image)
{
  int reduce = 2;
  int xdim = image_xdim(img);
  int ydim = image_ydim(img);
  int into_xdim = xdim>>1;
  int into_ydim = ydim>>1;
  
  //if (!into_image) into_image = make_image(into_xdim, into_ydim, image_element_type(img));

  // ELEMENT_TYPE inbuf0[xdim];
  ELEMENT_TYPE *inbuf0 = XALLOC(ELEMENT_TYPE, xdim);
  // ELEMENT_TYPE inbuf1[xdim];
  ELEMENT_TYPE *inbuf1 = XALLOC(ELEMENT_TYPE, xdim);
  // ELEMENT_TYPE outbuf[into_xdim];
  ELEMENT_TYPE *outbuf = XALLOC(ELEMENT_TYPE, into_xdim);

  require_image_rows foo1(img);
  require_image_rows foo2(into_image);
  int x, x2, y, y2;
  //int *pt0, *pt1, *opt;
  for (y=0, y2=0; y<into_ydim; y++, y2+=2 ) {
    img->getline(inbuf0, 0, y2);
    img->getline(inbuf1, 0, y2+1);
    for (x=0, x2=0; x<into_xdim; x++, x2+=2) {
      outbuf[x].r = (inbuf0[x2].r + inbuf0[x2+1].r + inbuf1[x2].r + inbuf1[x2+1].r) >>2;
      outbuf[x].g = (inbuf0[x2].g + inbuf0[x2+1].g + inbuf1[x2].g + inbuf1[x2+1].g) >>2;
      outbuf[x].b = (inbuf0[x2].b + inbuf0[x2+1].b + inbuf1[x2].b + inbuf1[x2+1].b) >>2;
    }
    into_image->putline(outbuf, 0, y);
  }
  xxfree(inbuf0); xxfree(inbuf1); xxfree(outbuf);
  
  return into_image;      
}

extern image *
box_filter_decimate2 (image *img, image *into_image)
{
  switch (image_element_type(img)) {
  case IMG_SINGLE_FLOAT: case IMG_DOUBLE_FLOAT:
    { 
      return box_filter_decimate2_image_all_floats(img, (double) 4.0, into_image);
    }
  case IMG_RGBA8: {
    RGBA8_PIXEL pix;
    //fprintf(stderr, "box_filter_decimate2 IMG_RGBA8 \n");
    // FIXME:  This doesn't set the alpha channel of the output
    return box_filter_decimate2_rgb(img, &pix, into_image);
  }
  case IMG_RGB8: {
    RGB8_PIXEL pix;
    // fprintf(stderr, "box_filter_decimate2 IMG_RGB8 \n");
    return box_filter_decimate2_rgb(img, &pix, into_image);
  }
 
  case IMG_UNSIGNED_8BIT: case IMG_SIGNED_8BIT:
  case IMG_UNSIGNED_16BIT: case IMG_SIGNED_16BIT:
  case IMG_UNSIGNED_32BIT: case IMG_SIGNED_32BIT:
    //fprintf(stderr, "box_filter_decimate2 rgba \n");
    return box_filter_decimate2_image_all_ints(img, (int) 0, into_image);
  
  default:
    error("box_filter_decimate2 illegal image element_type");
    return(0);    
  }
}

END_NAMESPACE_FREEDIUS
