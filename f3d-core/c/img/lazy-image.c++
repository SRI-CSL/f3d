#include "image.h"
#include "paged-image.h"
#include "lazy-image.h"
#include "cme-error.h"
#include "misc.h"
#include "image-ops.h"

#include <stdio.h>

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS


void lazy_image_page_handler::init_page_handler(paged_image_base *img0)
{
  dprintf((5, "lazy_image_page_handler::init_page_handler(img0=0x%x)\n", img0));
  img = img0;
  img->page_handler =  this;
  //fprintf (stderr, "init_page_handler %p %p %d %d \n", img, this, img->blocks_wide(), img->blocks_hi()); 
  
#ifdef LISP_IMAGE_ARRAYS
  make_lisp_page_handler_block_map(img, this, img->blocks_wide()*img->blocks_hi());
    //    img->block_map = block_map;
#endif
}


void lazy_image_page_handler::tile_builder(int x0, int y0, int x1, int y1, char *page)
{error("lazy_image_page_handler::tile_builder is undefined.\n");
}


#if 0 /* defined in misc/minmax.h */
#define MIN(x,y) (((x)<(y))?(x):(y))
#define MAX(x,y) (((x)>(y))?(x):(y))
#endif

void blocked_mapped_image::image_page_number_to_rectangle (int pagenum, int& x0, int& y0, int& x1, int& y1)
{
  int blky = pagenum/blocks_wide();
  int blkx = pagenum%blocks_wide();
  x0 = blkx*block_xdim;
  x1 = MIN(x0+block_xdim, xdim) -1;
  if (block_ydim < 0) {
    y1 = ydim -1 + blky*block_ydim;
    y0 = MAX(0, y1+1+block_ydim);
    }
  else {
    y0 = blky*block_ydim;
    y1 = MIN(y0+block_ydim, ydim) -1;
  }
}


static inline void 
fill_array (char *arr, int n, int value)
{
  for(int i=0; i<n; i++) arr[i]=value;
}

void lazy_image_page_handler::read_page(int page_number)
{
  image_page_pool* page_pool = (image_page_pool*) this->page_pool;
  page_queue_entry* entry = page_map_entry(this, page_number);
  char *array = ((image_page_queue_entry*) entry)->page;
  int bytes_per_page = page_pool->page_size;

  if (entry == NIL) error("lazy_image_page_handler::read_page got entry=NIL");
  if (page_initialization_map && page_initialization_map[page_number] == 0) {
    fill_array(array, bytes_per_page, 0);
    entry->status |= PAGE_DIRTY_BIT | PAGE_READ_OK_BIT;
    page_initialization_map[page_number] = 1;
  } else {
    entry->status = PAGE_READ_OK_BIT; 
    page_pool->read_count++;
    //entry->owner->page_pool->read_count++;
    { // This is only part specific to lazy_images
      int x0, y0, x1, y1;
      img->image_page_number_to_rectangle(page_number, x0, y0, x1, y1);
      //      dprintf((6, "lazy_image_page_handler::read_page: x0=%d, y0=%d.\n", x0, y0));
      tprintf(6, "lazy_image_page_handler::read_page: x0=%d, y0=%d, x1=%d, y1=%d.\n", x0, y0, x1, y1);
      this->tile_builder(x0, y0, x1, y1, array);
    }
  }
  set_page_status(page_number, entry->status);
}

// This simply abandons use of the page with no side effects.  Some subclasses
// of lazy_image_page_handler need to specialize write_page to save the page
// contents somewhere.
void lazy_image_page_handler::write_page(int page_number)
{
  Page_Map *pagemap = this;
  image_page_queue_entry* entry = (image_page_queue_entry*)page_map_entry(pagemap, page_number);
  entry->status = PAGE_READ_OK_BIT;
  this->set_page_status(page_number, entry->status);
}


void describe_page_map (lazy_image_page_handler *ph) {
  dprintf((2, "page_handler (image_page_handler) = %p\n", ph));
  //  dprintf((2, "  img = %p\n", ph->img));
  dprintf((2, "  map_size = %d\n", ph->map_size));
  dprintf((2, "  bytes_per_page = %d\n", ph->bytes_per_page));
  dprintf((2, "  block_map = %p\n", ph->block_map));
  dprintf((2, "  page_pool = %p\n", ph->page_pool));
  dprintf((2, "  array = %p\n", ph->array));
  dprintf((2, "  write_permit = %d\n", ph->write_permit));
  dprintf((2, "  active_tile_count = %d\n", ph->active_tile_count));
  dprintf((2, "  page_initialization_map = %p\n", ph->page_initialization_map));
}

// FIXME needs samples_per_pixel
extern paged_image_base *
make_lazy_image (int xdim, int ydim,
		 IMAGE_ELEMENT_TYPE element_type,
		 lazy_image_page_handler *page_handler,
		 int block_xdim, int block_ydim
		 )
{
  int samples_per_pixel = 1;
  paged_image_base *img;
  dprintf((6, "make_lazy_image: %d x %d, element_type = %d, page_handler=0x%x, block dims = %d x %d\n",
	   xdim, ydim, element_type, page_handler, block_xdim, block_ydim));
   
  if (block_xdim == 0) block_xdim = 128;
  if (block_ydim == 0) block_ydim = -64;
  
  img = make_paged_image(xdim, ydim, element_type, 1, block_xdim, block_ydim);// samples_per_pixel
  int bytes_per_page = bytes_per_tile(img);
  int npages = img->blocks_wide()*img->blocks_hi();
  if (! page_handler) {
    image_page_pool* page_pool = (image_page_pool*) get_page_pool(bytes_per_page, -1);
    page_handler = new lazy_image_page_handler(page_pool, npages);
  } else if (! page_handler->page_pool) {
    page_handler->page_pool = (image_page_pool*) get_page_pool(bytes_per_page, -1);
    page_handler->map_size = npages;
    page_handler->bytes_per_page = bytes_per_page;
  }
    
  //fprintf(stderr, "make_lazy_image: bytes_per_page=%d page_handler=%p\n", bytes_per_page, page_handler);
  page_handler->init_page_handler(img);
  //printf("make_lazy_image: tile_offset_bits = %d\n", page_handler->tile_offset_bits);
  describe_page_map(page_handler);
  // leave the class code alone for iref dispatching
  // img->class_code = LAZY_IMAGE_CLASS;
  return(img);
}




class lazy_retile_image_page_handler : public lazy_image_page_handler
{public:
  image *from_image;
  int *buf;
  void *map;
  lazy_retile_image_page_handler(image *from_img, void *map0) 
    :lazy_image_page_handler() {from_image=from_img; map=map0;}
  void tile_builder(int x0, int y0, int x1, int y1, char *page);
  virtual void init_page_handler(paged_image_base *img0, int iomode);
};

void lazy_retile_image_page_handler::init_page_handler(paged_image_base *img0, int iomode)
{
  lazy_image_page_handler::init_page_handler(img0);
  buf = XALLOC(int, img0->block_xdim);
}

void lazy_retile_image_page_handler::tile_builder(int x0, int y0, int x1, int y1, 
						  char *page)
{
  int nx = x1-x0+1;
  int ny = y1-y0+1;
  int bx = img->block_xdim;
  IMAGE_ELEMENT_TYPE el_type = image_element_type(img);
  IMAGE_ELEMENT_TYPE from_el_type = image_element_type(from_image);
  if (map) {
    switch (el_type) {
    case IMG_UNSIGNED_32BIT:
	switch (from_el_type) {
	case IMG_UNSIGNED_8BIT: 
	  extract_subimage_mapped(from_image, x0, y0, nx, ny, 
				  (int *) page,
				  (int *) buf, 
				  (int *) map,
				  bx);
	  break;
	default: error("lazy_retile_image: unsupported from_image element type %d", from_el_type);
	}
	break;
    default: error("lazy_retile_image: unsupported to_image element type %d", el_type);
    }
  } else {
    switch (el_type) {
    case IMG_UNSIGNED_8BIT: 
      dprintf((3, "lazy_retile_image tile: %d %d %d %d\n", x0, y0, nx, ny));
      extract_subimage(from_image, x0, y0, nx, ny, (unsigned char *)page, bx);
      break;
    default: error("lazy_retile_image: unsupported element type %d", el_type);
    }
  }
}

image *
lazy_retile_image (image *img, int bx, int by,
		   IMAGE_ELEMENT_TYPE element_type, 
		   void *map)
{
  lazy_image_page_handler *ph = new lazy_retile_image_page_handler(img, map);
  dprintf((8, "lazy_retile_image: page_handler = 0x%x\n", ph));
  return ((image *)make_lazy_image(image_xdim(img), image_ydim(img), 
				   element_type, 
				   ph,
				   bx, by));
}


extern "C" {

cimage*
FREEDIUS_GLOBAL(lazy_retile_image) (cimage *img, int bx, int by,
		     IMAGE_ELEMENT_TYPE element_type, 
		     void *map)
{
  return (cimage*) lazy_retile_image ((image*) img, bx, by, element_type, map);
}

} // extern "C"


#include "lisp-callback.h"

DEFVOIDCALLBACK(void, FREEDIUS_GLOBAL(lazy_image_fill_tile),
		(void* c_image, int page_number, int x0, int y0, int x1, int y1),
		(c_image, page_number, x0, y0, x1, y1));

class lisp_lazy_image_page_handler: public lazy_image_page_handler
{public:
  lisp_lazy_image_page_handler(image_page_pool* page_pool, int npages)
    : lazy_image_page_handler(page_pool, npages) {}
  void read_page(int page_number);
#if 0
  void tile_builder(int x0, int y0, int x1, int y1, char *page)
  {
    FREEDIUS_GLOBAL(lazy_image_fill_tile)((cimage *) img, x0, y0, x1, y1, page);
  }
#endif
};


void lisp_lazy_image_page_handler::read_page(int page_number)
{
  image_page_pool* page_pool = (image_page_pool*) this->page_pool;
  page_queue_entry* entry = page_map_entry(this, page_number);
  char *array = ((image_page_queue_entry*) entry)->page;
  int bytes_per_page = page_pool->page_size;

  if (entry == NIL) error("lazy_image_page_handler::read_page got entry=NIL");
  if (page_initialization_map && page_initialization_map[page_number] == 0) {
    fill_array(array, bytes_per_page, 0);
    entry->status |= PAGE_DIRTY_BIT | PAGE_READ_OK_BIT;
    page_initialization_map[page_number] = 1;
  } else {
    entry->status = PAGE_READ_OK_BIT; 
    page_pool->read_count++;
    { // This is only part specific to lazy_images
      int x0, y0, x1, y1;
      img->image_page_number_to_rectangle(page_number, x0, y0, x1, y1);
      //      dprintf((6, "lazy_image_page_handler::read_page: x0=%d, y0=%d.\n", x0, y0));
      tprintf(6, "lazy_image_page_handler::read_page: x0=%d, y0=%d, x1=%d, y1=%d.\n", x0, y0, x1, y1);
      FREEDIUS_GLOBAL(lazy_image_fill_tile)((cimage *) img, page_number, x0, y0, x1, y1);
    }
  }
  set_page_status(page_number, entry->status);
}

extern "C" {
cimage*
FREEDIUS_GLOBAL(make_lisp_lazy_image)(int xdim, int ydim, 
				      IMAGE_ELEMENT_TYPE element_type = IMG_UNSIGNED_8BIT,
				      int block_xdim = 0, int block_ydim = 0
				      )
{
  int block_size = block_xdim * abs(block_ydim);
  int samplesperpixel = 1;
  int element_size = image_element_size_from_type(element_type);
  int bytes_per_page = (block_size*element_size*samplesperpixel)>>3;
  int blocks_wide = ceiling(xdim,abs(block_xdim));
  int blocks_hi = ceiling(ydim,abs(block_ydim));			  
  image_page_pool* page_pool = (image_page_pool*) get_page_pool(bytes_per_page, -1);
  lazy_image_page_handler *ph = new lisp_lazy_image_page_handler(page_pool, blocks_wide*blocks_hi);
  //ph->set_write_permit(1);
  paged_image_base *img = make_lazy_image(xdim, ydim, element_type, ph, block_xdim, block_ydim);
  ph->img = img;
  return ((cimage*) img);
}

} // end extern "C"


END_NAMESPACE_FREEDIUS
