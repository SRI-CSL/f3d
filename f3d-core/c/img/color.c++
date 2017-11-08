//
// Copyright (C) 2001 by Christopher Connolly, SRI International
//
// L*u*v* and L*ab transformations from RGB color.  Questionable
// practice here: I scale L* to [0,255], whereas the definition maps
// to [0,100].  I scale to 255 for viewing as a greyscale luminance,
// but really, I should be using [0,100], and implement a display
// scaling.  Scaling to 255 also gives us about 1 extra bit of
// precision.
//

#include <math.h>
#include <image.h>
#include <color.h>
#include <misc.h>
#include "list.h"

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

#if 0
image *similar_image(image *img) {
  int xdim=image_xdim(img);
  int ydim=image_ydim(img);
  image *to_image = make_image(xdim, ydim, image_element_type(img));
  return (to_image);
}
#endif

static int lstar_lut_8bit[256];
static double lstar_lut_double[256];
double pow3_lut[256];
static int color_tables_initialized = 0;

double y_to_lstar(double y, double yn) {
  return (( 116.0 * pow( (y / yn), 0.33333333)) - 16.0);
}


// Considering that we would have to go through this nonsense every
// time we wanted to use L*u*v*, it may make more sense to define a
// class, and allow the instance initialization to generate the LUT:

void init_color_tables() {
  int i;

  lstar_lut_double[0] = 0.0;
  lstar_lut_8bit[0] = 0;
  pow3_lut[0] = 0.0;
  for (i = 1; i < 256; i++) {
    lstar_lut_double[i] = y_to_lstar((double) i, 255.0);
    lstar_lut_8bit[i] = (int) (0.5 + lstar_lut_double[i] * 2.55);
    pow3_lut[i] = pow(((double) i / 255.0), 0.33333333);
  }
  color_tables_initialized = 1;
  // fprintf(stderr,"color_tables_initialized\n");
}

#if 1 // defined(USE_CPP_INITS)

// New Sat May 16 2009  -- use C++ run-time initializers instead of depending on freedius_so_init
class color_table_init {
public:
  color_table_init() {init_color_tables();}
};

color_table_init *cti = new color_table_init();

#endif // defined(USE_CPP_INITS)
 


int y_to_lstar_value(double y) {
  return lstar_lut_8bit[(int) y];
}

#if 1
// A horrible hack:
int color_p(image* in) {
  return image_element_size(in) == 24 || image_element_size(in) == 32;
}
#else
// Should color_p be replaced by
int color_p(image* in) {
  switch(image_element_type(in)) {
  IMG_RGB8: IMG_RGBA8: return 1;
  default: return 0;
  }    
}
#endif

inline void unpack_color(int p, double rgb[3]) {
  rgb[0] = (double) RED(p);
  rgb[1] = (double) GREEN(p);
  rgb[2] = (double) BLUE(p);
}

static double rgbn[3] = {255.0, 255.0, 255.0};
static double xyzn[3] = {255.0, 255.0, 255.0};

void set_whitepoint(double r, double g, double b) {
  rgbn[0] = r;
  rgbn[1] = g;
  rgbn[2] = b;

  rgb_to_xyz(rgbn, xyzn);
}


static double a_gain = 1.0;
static double b_gain = 1.0;

image* rgb_to_lstar_image(image *in, image *lstar, image *aimage, image *bimage) 
{
  int i, j, ab_p, max;
  double rgb[3], xyz[3], dl, ab[2], offset;

  if (!color_p(in)) return 0;
  ab_p = (aimage && bimage);

  // This is the only image that will be automatically created:
  if (lstar == 0) error("lstar into_image must be supplied"); // lstar = similar_image(in);

  RGB8_PIXEL *buf = XALLOC(RGB8_PIXEL, in->xdim);
  //RGB8_PIXEL buf[in->xdim];
  int *lbuf = XALLOC(int, in->xdim);
  //int lbuf[in->xdim];
  int ab_len = ab_p?in->xdim:0;
  //int abuf[ab_len];
  int *abuf = XALLOC(int, ab_len);
  //int bbuf[ab_len];
  int *bbuf = XALLOC(int, ab_len);
  if (ab_p) {
    switch (image_element_type(aimage)) {
    case IMG_UNSIGNED_8BIT: offset = 128.0; max = 255; break;
    case IMG_UNSIGNED_16BIT: offset = 32768.0; max = 65535; break;
    default:
      printf("A and B images should be unsigned 8 or 16 bit images.\n");
      offset = 128.0; max = 255;
      break;
    }
  }
      
  for (i = 0; i < in->ydim; i++) {
    in->getline(buf, 0, i);

    for (j = 0; j < in->xdim; j++) {
      rgb[0] = buf[j].r;
      rgb[1] = buf[j].g;
      rgb[2] = buf[j].b;
      // unpack_color(buf[j], rgb);
      rgb_to_xyz(rgb, xyz);

      if (xyz[1] > 255.0) lbuf[j] = 255;
      else if (xyz[1] < 0.0) lbuf[j] = 0;
      else lbuf[j] = lstar_lut_8bit[(int) xyz[1]];

      // If we're given the a and b images, then fill them:
      if (ab_p) {
	xyz_to_ab(xyz, xyzn, ab);
	// Something of a hack.  There isn't necessarily that much
	// information here, but it allows us to expand the
	// "practical" dynamic range of the ab coordinates.  We also
	// clamp the AB coordinates when we would otherwise overflow:
	abuf[j] = (int) (offset + a_gain * ab[0]);
	if (abuf[j] > max) abuf[j] = max;
	bbuf[j] = (int) (offset + b_gain * ab[1]);
	if (bbuf[j] > max) bbuf[j] = max;
      }
    }
    lstar->putline(lbuf, 0, i);

    if (ab_p) {
      aimage->putline(abuf, 0, i);
      bimage->putline(bbuf, 0, i);
    }

  }
  xxfree(buf);  xxfree(lbuf);

  if (ab_p) {
    xxfree(abuf);
    xxfree(bbuf);
  }

  return lstar;
}



void lab_to_rgb(int lab[3], int rgb[3]) {
  double dxyz[3], drgb[3], Q;

  Q = ((((double) lab[0]) / 2.55) + 16.0) / 116.0;
  dxyz[1] = 255.0 * Q * Q * Q;

  dxyz[0] = Q + (((double) lab[1] - 128.0) / 500.0);
  dxyz[0] = 255.0 * dxyz[0] * dxyz[0] * dxyz[0];

  dxyz[2] = Q - (((double) lab[2] - 128.0) / 200.0);
  dxyz[2] = 255.0 * dxyz[2] * dxyz[2] * dxyz[2];

  xyz_to_rgb(dxyz, drgb);

  rgb[0] = (int) drgb[0];
  rgb[1] = (int) drgb[1];
  rgb[2] = (int) drgb[2];

  int i;
  for (i = 0; i < 3; i++)
    if (rgb[i] < 0) rgb[i] = 0;
    else if (rgb[i] > 255) rgb[i] = 255;

}

void rgb_to_lab(int rgb[3], int lab[3]) {
  double dxyz[3], drgb[3];
  int ab[2];

  drgb[0] = rgb[0];
  drgb[1] = rgb[1];
  drgb[2] = rgb[2];

  rgb_to_xyz(drgb, dxyz);

  lab[0] = y_to_lstar_value(dxyz[1]);
  xyz_to_ab_int(dxyz, xyzn, ab);
  lab[1] = ab[0];
  lab[2] = ab[1];
}

unsigned short **get_image_colormap(image *img) {
  return (unsigned short**) get_prop(img->property_list, "color_map");
}

void set_image_colormap(image *img, unsigned short** cmap) {
  put_prop( &(img->property_list), "color_map", cmap);
}

unsigned short **make_colormap(int nentries) {
  unsigned short **cmap;

  cmap =    XALLOC(unsigned short*, 3);
  cmap[0] = XALLOC(unsigned short, nentries);
  cmap[1] = XALLOC(unsigned short, nentries);
  cmap[2] = XALLOC(unsigned short, nentries);
  return cmap;
}



unsigned short **copy_colormap(unsigned short **cmap, int nentries) {
  int i;
  unsigned short **copy = make_colormap(nentries);

  for (i = 0; i < nentries; i++) {
    copy[0][i] = cmap[0][i];
    copy[1][i] = cmap[1][i];
    copy[2][i] = cmap[2][i];
  }
  return copy;
}



void free_colormap_memory(unsigned short **cmap) {
  xxfree(cmap[0]);
  xxfree(cmap[1]);
  xxfree(cmap[2]);
  xxfree(cmap);
}


image* rgb8_to_yuv_images(image *in, image *y, image *u, image *v) {
  int i, j, rgb[3], yuv[3];

  if (!color_p(in)) return 0;

  RGB8_PIXEL *buf = XALLOC(RGB8_PIXEL,in->xdim);
  int *ybuf = XALLOC(int, in->xdim);
  int *ubuf = XALLOC(int, in->xdim);
  int *vbuf = XALLOC(int, in->xdim);
  
  //RGB8_PIXEL buf[in->xdim];
  //int ybuf[in->xdim];
  //int ubuf[in->xdim];
  //int vbuf[in->xdim];
  for (i = 0; i < in->ydim; i++) {
    in->getline(buf, 0, i);

    for (j = 0; j < in->xdim; j++) {
      rgb[0] = buf[j].r;
      rgb[1] = buf[j].g;
      rgb[2] = buf[j].b;
      // unpack_color(buf[j], rgb);
      RGB2YUV(rgb[0], rgb[1], rgb[2], yuv[0], yuv[1], yuv[2]);

      ybuf[j] = yuv[0];
      ubuf[j] = yuv[1];
      vbuf[j] = yuv[2];
    }
    y->putline(ybuf, 0, i);
    u->putline(ubuf, 0, i);
    v->putline(vbuf, 0, i);
  }
  xxfree(buf);
  xxfree(ybuf);
  xxfree(ubuf);
  xxfree(vbuf);

  return y;
}

// ******************************  Lisp Callable Functions

extern "C" {

void FREEDIUS_GLOBAL(set_ab_gains)(double ag, double bg) {
  a_gain = ag;
  b_gain = bg;
}


cimage* FREEDIUS_GLOBAL(rgb8_to_yuv_images)(cimage *in, cimage *y, cimage *u, cimage *v) {
  return (cimage*) rgb8_to_yuv_images( (image*) in,
				       (image*) y,
				       (image*) u,
				       (image*) v);
}

} // end extern "C"


END_NAMESPACE_FREEDIUS

