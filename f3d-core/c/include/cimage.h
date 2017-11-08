#ifndef __cimage_h
#define __cimage_h

#include "image-types.h"
#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

extern "C" {

/* 
typedef struct cimage_struct {} cimage; 
moved to image.h  -- wrong -- This sucks
*/

int image_xdim(cimage *img);
int image_ydim(cimage *img);
int image_element_size(cimage *img);
IMAGE_ELEMENT_TYPE image_element_type(cimage *img);

cimage *make_cimage(int xdim, int ydim, IMAGE_ELEMENT_TYPE element_type);

void iset(cimage *img, int x, int y, int val);

void diset(cimage *img, int x, int y, double val);

int iref(cimage *img, int x, int y);

double diref(cimage *img, int x, int y);

double interpolate_iref(cimage *img, double x, double y);

cimage *cload_image(char *path);
  cimage *creload_image(char *path, cimage* img);

int crecognize_image_header (char *path);

int csave_image(cimage *img, char *path);

cimage * cgauss_convolve_decimate(cimage *img, double ka);

void init_image_io(int flags);

#if 0
//#include "list.h"

LISTELEM cimage_prop (cimage *image, PROPKEY prop);

void cimage_putprop (cimage *image, PROPKEY prop, LISTELEM val);
#endif

} // end extern "C"

END_NAMESPACE_FREEDIUS

#endif /* !cimage_h */
