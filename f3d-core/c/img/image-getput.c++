#include "image.h"
#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

#define MIN(x,y) (((x)<(y))? (x)  : (y))
#define MAX(x,y) (((x)>(y))? (x)  : (y))

int image_getline(image* img, IMAGE_ELEMENT_TYPE buffer_type, void *buf, 
		  int x, int y, int n, int to_index, int dx, int samp)
{
  if (x+n >= image_xdim(img)) n = image_xdim(img)-x;
  switch (buffer_type) {
  case IMG_SIGNED_32BIT: 
    return (img->getline((int *)buf, x, y, n, to_index, dx, samp));
  case IMG_DOUBLE_FLOAT:
    return (img->getline((double *)buf, x, y, n, to_index, dx, samp));
  case IMG_SINGLE_FLOAT:
    return (img->getline((float *)buf, x, y, n, to_index, dx, samp));
  case IMG_UNSIGNED_8BIT:
    return (img->getline((uchar *)buf, x, y, n, to_index, dx, samp));
  case IMG_RGB8:
    switch (image_element_type(img)) {
    case IMG_RGB8: return (img->getline((RGB8_PIXEL *)buf, x, y, n, to_index, dx, samp));
    default: //error("image_getline_rgb8 called on image that is not and IMG_RGB8 image");
      return IMAGE_GET_PUT_UNSUPPORTED_BUFFER_TYPE;
    }
  case IMG_RGBA8:
    switch (image_element_type(img)) {
    case IMG_RGBA8: return (img->getline((RGBA8_PIXEL *)buf, x, y, n, to_index, dx, samp));
    default: //error("image_getline_rgb8 called on image that is not and IMG_RGBA8 image");
      return IMAGE_GET_PUT_UNSUPPORTED_BUFFER_TYPE;
    }
  case IMG_UNSIGNED_16BIT:
    return (img->getline ((ushort *)buf, x, y, n, to_index, dx, samp));
  case IMG_SIGNED_16BIT:
    return (img->getline ((short *)buf, x, y, n, to_index, dx, samp));
  default: //error("image_getline with unsupported buffer_type %d", buffer_type);
    return IMAGE_GET_PUT_UNSUPPORTED_BUFFER_TYPE;
  }
  return IMAGE_GET_PUT_OK;
}

int image_putline(image* img, IMAGE_ELEMENT_TYPE buffer_type, void *buf, 
		  int x, int y, int n, int to_index, int samp)
{
  if (x+n >= image_xdim(img)) n = image_xdim(img)-x;
  switch (buffer_type) {
  case IMG_SIGNED_32BIT: 
    return (img->putline((int *)buf, x, y, n, to_index, samp));
  case IMG_DOUBLE_FLOAT:
    return (img->putline((double *)buf, x, y, n, to_index, samp));
  case IMG_SINGLE_FLOAT:
    return (img->putline((float *)buf, x, y, n, to_index, samp));
  case IMG_UNSIGNED_8BIT:
    return (img->putline((uchar *)buf, x, y, n, to_index, samp));
  case IMG_RGB8:
    switch (image_element_type(img)) {
    case IMG_RGB8: return (img->putline((RGB8_PIXEL *)buf, x, y, n, to_index, samp));
    default: //error("image_putline_rgb8 called on image that is not and IMG_RGB8 image");
      return IMAGE_GET_PUT_UNSUPPORTED_BUFFER_TYPE;
    }
  case IMG_RGBA8:
    switch (image_element_type(img)) {
    case IMG_RGBA8: return (img->putline((RGBA8_PIXEL *)buf, x, y, n, to_index, samp));
    default: //error("image_putline_rgb8 called on image that is not and IMG_RGBA8 image");
      return IMAGE_GET_PUT_UNSUPPORTED_BUFFER_TYPE;
    }
  case IMG_UNSIGNED_16BIT:
    return (img->putline ((ushort *)buf, x, y, n, to_index, samp));
  case IMG_SIGNED_16BIT:
    return (img->putline ((short *)buf, x, y, n, to_index, samp));
  default: //error("image_putline with unsupported buffer_type %d", buffer_type);
    return IMAGE_GET_PUT_UNSUPPORTED_BUFFER_TYPE;
  }
  return IMAGE_GET_PUT_OK;
}

int image_getcolumn(image* img, IMAGE_ELEMENT_TYPE buffer_type, void *buf, 
		    int x, int y, int n, int to_index, int dy, int samp)
{
  if (x+n >= image_xdim(img)) n = image_xdim(img)-x;
  switch (buffer_type) {
  case IMG_SIGNED_32BIT: 
    return (img->getcolumn((int *)buf, x, y, n, to_index, dy, samp));
  case IMG_DOUBLE_FLOAT:
    return (img->getcolumn((double *)buf, x, y, n, to_index, dy, samp));
#if 0 // unfinished
  case IMG_SINGLE_FLOAT:
    return (img->getcolumn((float *)buf, x, y, n, to_index, dy, samp));
  case IMG_UNSIGNED_8BIT:
    return (img->getcolumn((uchar *)buf, x, y, n, to_index, dy, samp));
  case IMG_RGB8:
    switch (image_element_type(img)) {
    case IMG_RGB8: return (img->getcolumn((RGB8_PIXEL *)buf, x, y, n, to_index, dy, samp));
    default: //error("image_getcolumn_rgb8 called on image that is not and IMG_RGB8 image");
      return IMAGE_GET_PUT_UNSUPPORTED_BUFFER_TYPE;
    }
  case IMG_RGBA8:
    switch (image_element_type(img)) {
    case IMG_RGBA8: return (img->getcolumn((RGBA8_PIXEL *)buf, x, y, n, to_index, dy, samp));
    default: //error("image_getcolumn_rgb8 called on image that is not and IMG_RGBA8 image");
      return IMAGE_GET_PUT_UNSUPPORTED_BUFFER_TYPE;
    }
  case IMG_UNSIGNED_16BIT:
    return (img->getcolumn ((ushort *)buf, x, y, n, to_index, dy, samp));
  case IMG_SIGNED_16BIT:
    return (img->getcolumn ((short *)buf, x, y, n, to_index, dy, samp));
#endif
  default: //error("image_getcolumn with unsupported buffer_type %d", buffer_type);
    return IMAGE_GET_PUT_UNSUPPORTED_BUFFER_TYPE;
  }
  return IMAGE_GET_PUT_OK;
}

int image_putcolumn(image* img, IMAGE_ELEMENT_TYPE buffer_type, void *buf, 
		  int x, int y, int n, int to_index, int samp)
{
  if (x+n >= image_xdim(img)) n = image_xdim(img)-x;
  switch (buffer_type) {
  case IMG_SIGNED_32BIT: 
    return (img->putcolumn((int *)buf, x, y, n, to_index, samp));
  case IMG_DOUBLE_FLOAT:
    return (img->putcolumn((double *)buf, x, y, n, to_index, samp));
#if 0 // unfinished
  case IMG_RGB8:
    switch (image_element_type(img)) {
    case IMG_RGB8: return (img->putcolumn((RGB8_PIXEL *)buf, x, y, n, to_index, samp));
    default: //error("image_putcolumn_rgb8 called on image that is not and IMG_RGB8 image");
      return IMAGE_GET_PUT_UNSUPPORTED_BUFFER_TYPE;
    }
  case IMG_RGBA8:
    switch (image_element_type(img)) {
    case IMG_RGBA8: return (img->putcolumn((RGBA8_PIXEL *)buf, x, y, n, to_index, samp));
    default: //error("image_putcolumn_rgb8 called on image that is not and IMG_RGBA8 image");
      return IMAGE_GET_PUT_UNSUPPORTED_BUFFER_TYPE;
    }
  case IMG_SINGLE_FLOAT:
    return (img->putcolumn((float *)buf, x, y, n, to_index, samp));
  case IMG_UNSIGNED_8BIT:
    return (img->putcolumn((uchar *)buf, x, y, n, to_index, samp));
  case IMG_UNSIGNED_16BIT:
    return (img->putcolumn ((ushort *)buf, x, y, n, to_index, samp));
  case IMG_SIGNED_16BIT:
    return (img->putcolumn ((short *)buf, x, y, n, to_index, samp));
#endif
  default: //error("image_putcolumn with unsupported buffer_type %d", buffer_type);
    return IMAGE_GET_PUT_UNSUPPORTED_BUFFER_TYPE;
  }
  return IMAGE_GET_PUT_OK;
}




int image_get_rectangle(blocked_mapped_image* img, IMAGE_ELEMENT_TYPE buffer_type, void *buf, 
			 int xleft, int ybot, int nx, int ny, int nbands, int buf_start, int buf_row_length)
{
  // need extensive bounds tests
  if (ybot < 0) {ny+= ybot; ybot=0;}
  int cpl = nbands*nx;
  int ystart = MIN(image_ydim((image *)img) -1, ybot+ny-1);
  for (int y=ystart, pix_off=buf_start; y>=ybot; y--, pix_off+=buf_row_length) {
    int flag = image_getline(img, buffer_type, buf, xleft, y, cpl, pix_off, 1, 0);
    if (flag != IMAGE_GET_PUT_OK)
      return flag;
  }
  return IMAGE_GET_PUT_OK;
}

int image_put_rectangle(blocked_mapped_image* img, IMAGE_ELEMENT_TYPE buffer_type, void *buf, 
			 int xleft, int ybot, int nx, int ny, int nbands, int buf_start, int buf_row_length)
{
  // need extensive bounds tests
  if (ybot < 0) {ny+= ybot; ybot=0;}
  int cpl = nbands*nx;
  int ystart = MIN(image_ydim((image *)img) -1, ybot+ny-1);
  for (int y=ystart, pix_off=buf_start; y>=ybot; y--, pix_off+=buf_row_length) {
    int flag = image_putline(img, buffer_type, buf, xleft, y, cpl, pix_off, 0);
    if (flag != IMAGE_GET_PUT_OK)
      return flag;
  }
  return IMAGE_GET_PUT_OK;
}

template <class ELEMENT_TYPE>
inline void copy_pixel_left_ (ELEMENT_TYPE *buf, int nb)
{
  ELEMENT_TYPE pixel = buf[0];
  for (int i=1; i<=nb; i++)
    buf[-i]=pixel;
}

template <class ELEMENT_TYPE>
inline void copy_pixel_right_ (ELEMENT_TYPE *buf, int nb)
{
  ELEMENT_TYPE pixel = buf[0];
  for (int i=1; i<=nb; i++)
    buf[i]=pixel;
}

void copy_pixel_left(IMAGE_ELEMENT_TYPE buffer_type, void *buf, int buf_off, int nb)
{
  switch (buffer_type) {
  case IMG_UNSIGNED_8BIT: copy_pixel_left_(((uchar *)       buf)+buf_off, nb); break;
  case IMG_RGB8:          copy_pixel_left_(((RGB8_PIXEL *)  buf)+buf_off, nb); break;
  case IMG_RGBA8:         copy_pixel_left_(((RGBA8_PIXEL *) buf)+buf_off, nb); break;
  }
}

void copy_pixel_right(IMAGE_ELEMENT_TYPE buffer_type, void *buf, int buf_off, int nb)
{
  switch (buffer_type) {
  case IMG_UNSIGNED_8BIT: copy_pixel_right_(((uchar *)       buf)+buf_off, nb); break;
  case IMG_RGB8:          copy_pixel_right_(((RGB8_PIXEL *)  buf)+buf_off, nb); break;
  case IMG_RGBA8:         copy_pixel_right_(((RGBA8_PIXEL *) buf)+buf_off, nb); break;
  }
}


int image_get_rectangle_bordered(blocked_mapped_image* img, IMAGE_ELEMENT_TYPE buffer_type, void *buf, 
				  int xleft, int ybot, int nx, int ny, int border, 
				  int nbands, int buf_start, int buf_row_length)
{
  // need extensive bounds tests
  if (ybot < 0) {ny+= ybot; ybot=0;}

  int top_to_bottom_p = img->block_ydim < 0;
  int xdim = image_xdim(img);
  int xdim_1 = xdim -1;
  int ydim_1 = image_ydim(img) -1;
  int lbdr = (xleft >=border)? border:0;
  int xstart = xleft-lbdr;
  int rbdr = (xleft+nx+border < xdim_1)? border:0;
  int nxp2b = nx+border+border;
  int ppl = nx + lbdr + rbdr;
  int npix = MIN(ppl, xdim-xstart);
  //  int cpl = nbands*ppl;
  int ytop = MIN(ydim_1, ybot+ny-1);
  int fill_lbdr = (lbdr == 0);
  int fill_rbdr = (rbdr == 0);
  int dy = top_to_bottom_p? -1:1;
  int n = ytop-ybot+border+border+1;
  int ystart = top_to_bottom_p? ytop+border : ybot-border;
  for (int y=ystart, pix_off=buf_start+border-lbdr; n>0;n--, y+=dy, pix_off+=buf_row_length) {
    //image_getline(img, buffer_type, buf, xstart, MAX(0,MIN(y,ydim_1)), cpl, pix_off, 1, 0);
    int flag = image_getline(img, buffer_type, buf, xstart, MAX(0,MIN(y,ydim_1)), npix, pix_off, 1, 0);
    if (flag != IMAGE_GET_PUT_OK)
      return flag;
    // FIXME: Handle the left and right edge pixel replication
    if (fill_lbdr) copy_pixel_left (buffer_type, buf, pix_off, border);
    if (fill_rbdr) copy_pixel_right(buffer_type, buf, pix_off+npix-1, border);
  }
  return IMAGE_GET_PUT_OK;
}



extern "C" {

int FREEDIUS_GLOBAL(image_getline)(cimage* cimg, IMAGE_ELEMENT_TYPE buffer_type, void *buf, 
				     int x, int y, int n, int to_index, int dx, int samp)
{
  return image_getline((image *) cimg, buffer_type, buf, x, y, n, to_index, dx, samp);
}

int FREEDIUS_GLOBAL(image_putline)(cimage* cimg, IMAGE_ELEMENT_TYPE buffer_type, void *buf, 
				     int x, int y, int n, int to_index, int samp)
{
  return image_putline((image *) cimg, buffer_type, buf, x, y, n, to_index, samp);
}

int FREEDIUS_GLOBAL(image_getcolumn)(cimage* cimg, IMAGE_ELEMENT_TYPE buffer_type, void *buf, 
				     int x, int y, int n, int to_index, int dy, int samp)
{
  return image_getcolumn((image *) cimg, buffer_type, buf, x, y, n, to_index, dy, samp);
}

int FREEDIUS_GLOBAL(image_putcolumn)(cimage* cimg, IMAGE_ELEMENT_TYPE buffer_type, void *buf, 
				     int x, int y, int n, int to_index, int samp)
{
  return image_putcolumn((image *) cimg, buffer_type, buf, x, y, n, to_index, samp);
}


int FREEDIUS_GLOBAL(image_get_rectangle)(cimage* cimg, IMAGE_ELEMENT_TYPE buffer_type, void *buf, 
					  int xleft, int ybot, int nx, int ny, int nbands, 
					  int buf_start, int buf_row_length)
{ 
  return image_get_rectangle((blocked_mapped_image *)cimg, buffer_type, buf, 
			     xleft, ybot, nx, ny, nbands, buf_start, buf_row_length);
}

int FREEDIUS_GLOBAL(image_get_rectangle_bordered)(cimage* cimg, IMAGE_ELEMENT_TYPE buffer_type, void *buf, 
						   int xleft, int ybot, int nx, int ny, int border, 
						   int nbands, int buf_start, int buf_row_length)
{
  return image_get_rectangle_bordered((blocked_mapped_image *) cimg, 
				      buffer_type, buf, xleft, ybot, nx, ny, border,
				      nbands, buf_start, buf_row_length);
}

int FREEDIUS_GLOBAL(image_put_rectangle)(cimage* cimg, IMAGE_ELEMENT_TYPE buffer_type, void *buf, 
					  int xleft, int ybot, int nx, int ny, int nbands, 
					  int buf_start, int buf_row_length)
{ 
  return image_put_rectangle((blocked_mapped_image *)cimg, buffer_type, buf, 
			     xleft, ybot, nx, ny, nbands, buf_start, buf_row_length);
}


} // end extern "C"


END_NAMESPACE_FREEDIUS
