#ifndef	__ppm_image_io_h
#define __ppm_image_io_h

#include "image-io.h"

BEGIN_NAMESPACE_FREEDIUS

extern Class *ppm_image_header_class;
extern Class *pgm_image_header_class;


class ppm_image_header:
public basic_image_header
{public:
  int maxval;
  int use_cmap;
  unsigned short* cmap[3];

  ppm_image_header(){};
  virtual int read_header(int fd);
  virtual int recognize_header(int fd);
  image *load_image(int fd, char *path);
  image *save_image(char *path);
};

extern ppm_image_header *ppm_image_header1;



extern int init_pgm_array_image_header(void);
class pgm_image_header:
public basic_image_header
{public:
  int maxval;

  pgm_image_header(){};
  virtual int read_header(int fd);
  virtual int recognize_header(int fd);
  image *load_image(int fd, char *path);
  image *save_image(char *path);
};

extern pgm_image_header *pgm_image_header1;

extern int init_ppm_image_header();

END_NAMESPACE_FREEDIUS

#endif /* ! __ppm_image_io_h */



