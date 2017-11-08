//
// Copyright (C) 2001 by Christopher Connolly, SRI International
//
#ifndef	__color_h
#define __color_h

#include <image.h>

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

// Dammit.  These are identical now.  At least this code is now
// completely self-contained.  Please remember to use THESE macros
// only when unpacking color.

#define RGB2YUV(r, g, b, y, u, v)\
  y = (306*r + 601*g + 117*b)  >> 10;\
  u = ((-172*r - 340*g + 512*b) >> 10)  + 128;\
  v = ((512*r - 429*g - 83*b) >> 10) + 128;\
  y = y < 0 ? 0 : y;\
  u = u < 0 ? 0 : u;\
  v = v < 0 ? 0 : v;\
  y = y > 255 ? 255 : y;\
  u = u > 255 ? 255 : u;\
  v = v > 255 ? 255 : v


#ifdef WORDS_BIGENDIAN

// SGI, for example.

#define RED(p)   ((p      ) & 0x00FF)
#define GREEN(p) ((p >>  8) & 0x00FF)
#define BLUE(p)  ((p >> 16) & 0x00FF)

#define PIXEL(r,g,b) (r | (g << 8) | (b << 16))

#else

// Intel, for example.

#define RED(p)   ( p        & 0x00FF)
#define GREEN(p) ((p >>  8) & 0x00FF)
#define BLUE(p)  ((p >> 16) & 0x00FF)

#define PIXEL(r,g,b) (r | (g << 8) | (b << 16))

#endif

extern double pow3_lut[256];

inline void rgb_to_xyz(double* rgb, double* xyz) {
  xyz[0] = (0.412453 * rgb[0]) + (0.357580 * rgb[1]) + (0.180423 * rgb[2]);
  xyz[1] = (0.212671 * rgb[0]) + (0.715160 * rgb[1]) + (0.072169 * rgb[2]);
  xyz[2] = (0.019334 * rgb[0]) + (0.119193 * rgb[1]) + (0.950227 * rgb[2]);
}

inline void xyz_to_rgb(double* xyz, double* rgb) {
  rgb[0] = (3.2404813432005266 * xyz[0]) - (1.5371515162713183 * xyz[1]) - (0.4985363261688878 * xyz[2]);
  rgb[1] = (-0.9692549499965684 * xyz[0])+ (1.8759900014898907 * xyz[1]) + (0.04155592655829284 * xyz[2]);
  rgb[2] = (0.05564663913517715 * xyz[0])- (0.20404133836651123 * xyz[1])+ (1.0573110696453443 * xyz[2]);
}


inline void uv_prime(double* xyz, double* uv) {
  double den = xyz[0] + 15.0 * xyz[1] + 3.0 * xyz[2];

  if (den > 0.0) {
    uv[0] = (4.0 * xyz[0]) / den;
    uv[1] = (9.0 * xyz[1]) / den;
  } else {
    uv[0] = 0.0;
    uv[1] = 0.0;
  }
}


inline void xyz_to_ab(double *xyz, double *xyzn, double *ab) {
  double xratio = 255.0 * xyz[0] / xyzn[0];
  double yratio = 255.0 * xyz[1] / xyzn[1];
  double zratio = 255.0 * xyz[2] / xyzn[2];

  double xr = pow3_lut[ (int) xratio ];
  double yr = pow3_lut[ (int) yratio ];
  double zr = pow3_lut[ (int) zratio ];

  ab[0] = 500.0 * (xr - yr); // Range is [-500,500]
  ab[1] = 200.0 * (yr - zr); // Range is [-200,200]
}




inline void xyz_to_ab_int(double *xyz, double *xyzn, int *ab) {
  double xratio = 255.0 * xyz[0] / xyzn[0];
  double yratio = 255.0 * xyz[1] / xyzn[1];
  double zratio = 255.0 * xyz[2] / xyzn[2];

  double xr = pow3_lut[ (int) xratio ];
  double yr = pow3_lut[ (int) yratio ];
  double zr = pow3_lut[ (int) zratio ];

  ab[0] = (int) (128.0 + 500.0 * (xr - yr));
  ab[1] = (int) (128.0 + 200.0 * (yr - zr));
}



inline void uvstar(double *uv, double *unvn, double l, double *uvstar) {
  uvstar[0] = 13.0 * l * (uv[0] - unvn[0]);
  uvstar[1] = 13.0 * l * (uv[1] - unvn[1]);
}

unsigned short **get_image_colormap(image*);
void set_image_colormap(image*, unsigned short**);
void set_whitepoint(double, double, double);
// it would be good to make these names consistent!!
image* rgb_to_lstar_image(image*, image* = 0, image* = 0, image* = 0);
image* rgb8_to_yuv_images(image*, image*, image*, image*);
void lab_to_rgb(int*, int*);
void rgb_to_lab(int*, int*);
unsigned short **get_image_colormap(image*);
void set_image_colormap(image*, unsigned short**);
void free_colormap_memory(unsigned short **);
unsigned short **make_colormap(int);
unsigned short **copy_colormap(unsigned short**, int);
int color_p(image*);

END_NAMESPACE_FREEDIUS

#endif /* ! __color_h */

