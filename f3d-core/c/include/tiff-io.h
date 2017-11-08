#ifndef	__tiff_image_io_h
#define __tiff_image_io_h

#include <tiffio.h>

#include "image-io.h"

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

extern Class *tiff_image_header_class;

#if 0
class tiff_image_header:
public basic_image_header
{public:
  uint16 samplesperpixel;
  uint16 extrasamples;
  uint16 bitspersample;
  uint16 photo_interp;
  uint16 planarconfig;

  int pixel_skip;
  int block_xdim;
  int block_ydim;
  int padded_block_xdim;
  int block_size;
  int blocks_wide;
  int blocks_hi;
  int use_cmap;
  unsigned short* cmap[3];
  int vector_image_p;

  tiff_image_header(){
    samplesperpixel = photo_interp = 0;
    block_xdim = block_ydim = padded_block_xdim = block_size = blocks_wide = blocks_hi = 0;
    use_cmap = 0;
    vector_image_p = 0;
  };
//  virtual void set_header_slots (blocked_mapped_image *img);
  virtual int read_header(TIFF* tif);
  virtual int read_header(int fd);
  virtual int recognize_header(int fd);
//  virtual void write_header(int fd);
  image *load_image(int fd, char *path);
  image * load_untiled_tiff_image (TIFF* tif);
  image * load_untiled_compressed_tiff_image (TIFF* tif);
  image * load_untiled_tiff_vector_image (TIFF* tif);
  image * load_band_sequential_tiled_tiff_image (TIFF* tif);
  image * load_band_interleaved_tiled_tiff_image (TIFF* tif);
  image * load_tiled_tiff_image (TIFF* tif);
  image * load_tiff_lazy_image (TIFF* tif, int write_permit);
  char* read_property_list_string (char* path);
};
#endif


class tiff_image_header:
public basic_blocked_image_header
{public:
  uint16 samplesperpixel;
  uint16 extrasamples;
  uint16 bitspersample;
  uint16 photo_interp;
  uint16 planarconfig;

  int use_cmap;
  unsigned short* cmap[3];
  int vector_image_p;

  tiff_image_header(){
    samplesperpixel = photo_interp = 0;
    block_xdim = block_ydim = padded_block_xdim = block_size = blocks_wide = blocks_hi = 0;
    use_cmap = 0;
    vector_image_p = 0;
  };
//  virtual void set_header_slots (blocked_mapped_image *img);
  virtual int read_header(TIFF* tif);
  virtual int read_header(int fd);
  virtual int recognize_header(int fd);
//  virtual void write_header(int fd);
  image *load_image(int fd, char *path);
  image * load_untiled_tiff_image (TIFF* tif);
  image * load_untiled_compressed_tiff_image (TIFF* tif);
  image * load_untiled_tiff_vector_image (TIFF* tif);
  image * load_band_sequential_tiled_tiff_image (TIFF* tif);
  image * load_band_interleaved_tiled_tiff_image (TIFF* tif);
  image * load_tiled_tiff_image (TIFF* tif);
  image * load_tiff_lazy_image (TIFF* tif, int write_permit);
  char* read_property_list_string (char* path);
};


extern tiff_image_header *tiff_image_header1;

extern int init_tiff_array_image_header(void);

extern int
save_untiled_tiff_image(image *img, string path, int error_ok = 0, 
			int compression_mode = COMPRESSION_NONE, int jpeg_quality = 75);

END_NAMESPACE_FREEDIUS

#endif /* ! __tiff_image_io_h */



