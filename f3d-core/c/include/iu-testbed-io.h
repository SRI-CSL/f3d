#ifndef	__iu_testbed_image_io_h
#define __iu_testbed_image_io_h

#include "image-io.h"

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

extern Class *iu_testbed_image_header_class;

#if 0
class iu_testbed_image_header: public basic_image_header
{public:
  int block_xdim;
  int block_ydim;
  int padded_block_xdim;
  int block_size;
  int blocks_wide;
  int blocks_hi;

  iu_testbed_image_header():basic_image_header() {}
  virtual void set_header_slots (blocked_mapped_image *img);
  virtual int read_header(int fd);
  virtual int recognize_header(int fd);
  virtual void write_header(int fd);
  image *load_image(int fd, char *path);
  int property_list_file_position ();
  char* read_property_list_string (char *path);
};
#endif
class iu_testbed_image_header: public basic_blocked_image_header
{public:
  iu_testbed_image_header():basic_blocked_image_header() {}
  virtual void set_header_slots (blocked_mapped_image *img);
  virtual int read_header(int fd);
  virtual int recognize_header(int fd);
  virtual void write_header(int fd);
  image *load_image(int fd, char *path);
  int property_list_file_position ();
  char* read_property_list_string (char *path);
};



extern iu_testbed_image_header *iu_testbed_image_header1;
extern int init_iu_testbed_array_image_header();

int init_iu_testbed_array_image_header();

END_NAMESPACE_FREEDIUS

#endif /* ! __iu_testbed_image_io_h */

