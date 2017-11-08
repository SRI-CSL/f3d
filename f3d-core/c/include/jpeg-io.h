#ifndef	__jpeg_image_io_h
#define __jpeg_image_io_h

#include "image-io.h"

extern "C" {
  //#ifndef DARWIN
  //#define XMD_H
  //#define HAVE_BOOLEAN
  //#endif

  //#if defined(DARWIN) || defined(_WIN32)
  /* Assumes that 'make install-headers' has been done for the jpeg library: */
#include "jpeglib.h"
  //#else
  //#include "gjpeglib.h"
  //#endif
}

#include <setjmp.h>

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

extern Class *jpeg_image_header_class;

struct my_error_mgr {
  struct jpeg_error_mgr pub;	/* "public" fields */

  jmp_buf setjmp_buffer;	/* for return to caller */
};

typedef struct my_error_mgr * my_error_ptr;

class jpeg_image_header: 
public basic_image_header
{public:
  int samplesperpixel;
  struct jpeg_decompress_struct cinfo;
  int row_stride;
  struct my_error_mgr jerr;
  FILE *infile;

  jpeg_image_header(){};
  virtual int read_header(int fd);
  virtual int recognize_header(int fd);
//  virtual void write_header(int fd);
  image *load_image(int fd, char *path);

template <class eltype>
    void read_jpeg_rgb_data(image *img, JSAMPARRAY buffer, eltype *obuf) {
      int y = ydim - 1;
      eltype *rgb_buf = (eltype *) buffer[0];
      while (cinfo.output_scanline < cinfo.output_height) {
	/* jpeg_read_scanlines expects an array of pointers to scanlines.
	 * Here the array is only one element long, but you could ask for
	 * more than one scanline at a time if that's more convenient.
	 */
	(void) jpeg_read_scanlines(&cinfo, buffer, 1);
	/* Assume put_scanline_someplace wants a pointer and sample count. */
	img->putline(rgb_buf, 0, y);
	y--;
      }
}

};



extern jpeg_image_header *jpeg_image_header1;

extern int init_jpeg_image_header();

END_NAMESPACE_FREEDIUS

#endif /* ! __jpeg_image_io_h */
