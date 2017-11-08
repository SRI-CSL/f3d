#ifndef	__lazy_image_h
#define __lazy_image_h

#include "paged-image.h"

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

class lazy_image_page_handler: public image_page_handler
{public:
  paged_image_base *img;

  lazy_image_page_handler(){}
  lazy_image_page_handler(image_page_pool* page_pool, int npages)
    : image_page_handler(page_pool, npages)   {
    write_permit = 0;
    img = 0;
  }
  void read_page(int blknum);
  void write_page(int blknum);
  virtual void init_page_handler(paged_image_base *img);  
  virtual void tile_builder(int x0, int y0, int x1, int y1, char *page);
};

typedef void (* tile_builder_fn)(paged_image_base *, int,int,int,int,char *);

paged_image_base *
make_lazy_image(int xdim, int ydim,
		IMAGE_ELEMENT_TYPE element_type = IMG_UNSIGNED_8BIT,
		lazy_image_page_handler *page_handler = (lazy_image_page_handler *) 0,
		int block_xdim = 0, int block_ydim = 0
		);

image *
lazy_retile_image (image *img, int bx, int by,
		   IMAGE_ELEMENT_TYPE element_type, 
		   void *map = 0);

END_NAMESPACE_FREEDIUS

#endif /* ! __lazy_image_h */
