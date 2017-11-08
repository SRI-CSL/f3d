#ifndef	__file_image_h
#define __file_image_h

#ifdef __GNUG__
#pragma interface
#endif

#include "image.h"
#include "paged-image.h"
#include "cme-error.h"

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

#define READ_OK 1
#define WRITE_OK 2
typedef char *string;

/* Also defined in image-io.h - bad news! */ 

#ifndef OFF_T
#if defined(_WIN32)
  typedef __int64 OFF_T;
#else
  typedef off_t OFF_T;
#endif
#endif



class file_image_page_handler: public image_page_handler
{public:
  int stream;
  char *pathname;
  int first_tile_offset;  // FIXME: should be of type off_t
  //IMAGE_ELEMENT_TYPE element_type_code;  // not used
  // int endian;		// 0=big_endian, 1=little_endian   UNUSED ?
  //  virtual IMAGE_CLASS class_code() {return FILE_IMAGE_CLASS;}
  file_image_page_handler(image_page_pool* page_pool, int npages);

  page_queue_entry* probe_page_fault (int blknum);
  void read_page(int blknum);
  void write_page(int blknum);

  virtual OFF_T page_filepos(int page_number);

  void force_page_initialization();
  void unmap_file(int closep = 1); // closep: 1=yes, 2=yes-abort, 0=no
  virtual int save_image (paged_image_base *img, char *path);
  virtual int map_image_to_file (paged_image_base *img, int iomode, string pathname, 
				 int header_length);
};


paged_image_base *iu_testbed_load_file_image (string path);
// paged_image_base *iu_testbed_load_file_image (file_image_base *img, string path);

int init_iu_testbed_file_image_header();
int init_image_choose_image_class_fn();

paged_image_base *
make_file_image (int xdim, int ydim,
		 IMAGE_ELEMENT_TYPE element_type = IMG_UNSIGNED_8BIT,
		 int samples_per_pixel = 1,
		 int block_xdim = 0, int block_ydim = 0,int padded_block_xdim = 0,
		 char *pathname = (char *) 0);

extern int init_make_file_image_fn ();

int init_make_file_image_fn();

END_NAMESPACE_FREEDIUS

#endif /* ! __file_image_h */
