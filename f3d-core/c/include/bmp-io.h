#ifndef	__bmp_image_io_h
#define __bmp_image_io_h

#include "image-io.h"
#include "image-types.h"

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

extern Class *bmp_image_header_class;

class bmp_image_header:
public basic_image_header
{public:
  RGB8_PIXEL *cmap;
  short bpp;  /* bits per pixel in the file. */
  int offset;
  int color_important;

  bmp_image_header(){};
//  virtual void set_header_slots (blocked_mapped_image *img);
  virtual int read_header(int fd);
  virtual int recognize_header(int fd);
//  virtual void write_header(int fd);
  image *load_image(int fd, char *path);
};

extern bmp_image_header *bmp_image_header1;

extern int init_bmp_array_image_header(void);

END_NAMESPACE_FREEDIUS

#endif /* ! __bmp_image_io_h */



