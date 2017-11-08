#include "array-image.h"

#ifdef __GNUG__
#pragma implementation
#endif

#include "paged-image.h"

#include "cme-error.h"
#include "misc.h"
#include "list.h"

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

typedef unsigned long address;

image_page_handler::image_page_handler()
{
  //img = 0;
}

int
image_page_handler::save_image (paged_image_base *img, char *path)
{
  return 0;
}

image_page_handler::image_page_handler(image_page_pool* page_pool, int npages, int write_permit) 
  :basic_page_handler(page_pool, npages, write_permit)
{
  bytes_per_page = page_pool->page_size;
  block_map = 0;
  //  img = 0;
}

page_queue_entry *
image_page_handler::read_page_fault (int page_number)
{
  page_queue_entry * entry = page_map_entry(this, page_number);
  int status = entry->status;
  page_pool->probe_count++;
  tprintf(12, "image_page_handler::read_page_fault(%d) entry=%x, null page=%x, status=%d\n", page_number, entry, page_pool->null_page, status);
  if (status & PAGE_AGED_BIT)
    rejuvenate_page(this, entry);
  else if (entry == page_pool->null_page)
    entry = get_page (page_number);
  else 
    error("image_page_handler::read_page_fault: read attempted from page with wierd status %d", status);
  tprintf(12, "image_page_handler::read_page_fault(%d) setting status\n", page_number);
  set_page_status(page_number, entry->status);
  return(entry);
}

page_queue_entry *
image_page_handler::write_page_fault (int page_number)
{
  page_queue_entry * entry = page_map_entry(this, page_number);
  int status = entry->status;
  page_pool->probe_count++;

  if (! write_permit)
    error("image_page_handler::write_page_fault - image is not open for write.");

  if (status & PAGE_AGED_BIT) {
    rejuvenate_page(this, entry);
    entry->status = PAGE_WRITE_OK_BIT | PAGE_READ_OK_BIT | PAGE_DIRTY_BIT;
  } else if (entry == page_pool->null_page) {
    entry = get_page (page_number);
    entry->status = PAGE_WRITE_OK_BIT | PAGE_READ_OK_BIT | PAGE_DIRTY_BIT;
  } else entry->status |= PAGE_WRITE_OK_BIT | PAGE_READ_OK_BIT | PAGE_DIRTY_BIT;
  set_page_status(page_number, entry->status);
  return(entry);
}




setClass(paged_image_base, image);

int resize_image_page_pool (paged_image_base *image, int size_change_count)
{
  image->page_handler->page_pool->resize_page_pool(size_change_count);
  return image->page_handler->page_pool->pool_size;
}

// This should be a virtual function.
void image_page_pool_stats (paged_image_base *image, int resetp=1)
{
  image_page_pool *pool = (image_page_pool *)image->page_handler->page_pool;
  warning("Image Page Pool page-size= %d\n", pool->page_size);
  page_pool_stats(pool, resetp);
}

POINTER_LIST image_page_pool_list;

void all_image_page_pool_stats (int resetp=1)
{
  POINTER_LIST page_pool_list = image_page_pool_list;
  while (page_pool_list) {
    image_page_pool *pool = (image_page_pool *)page_pool_list->car;
    warning("Image Page Pool page-size= %d\n", pool->page_size);
    page_pool_stats(pool, resetp);
    page_pool_list = page_pool_list->cdr;
  }
}



int image_page_handler::map_image_to_file (paged_image_base *img, int iomode, string pathname, int header_length) 
{return 0;}

void describe_page_map (page_map *ph) {
  tprintf(0, "page_handler (page_map) = 0x%x\n", ph);
  /* tprintf(0, "  img = 0x%x\n", ph->img); */
  tprintf(0, "  map_size = %d\n", ph->map_size);
  tprintf(0, "  page_pool = %p\n", ph->page_pool);
  tprintf(0, "  array = %p\n", ph->array);
  tprintf(0, "  write_permit = %d\n", ph->write_permit);
  tprintf(0, "  active_tile_count = %d\n", ph->active_tile_count);
  tprintf(0, "  page_initialization_map = %p\n", ph->page_initialization_map);
}

void describe_page_map (image_page_handler *ph) {
  tprintf(0, "page_handler (image_page_handler) = 0x%x\n", ph);
  /* tprintf(0, "  img = 0x%x\n", ph->img); */
  tprintf(0, "  map_size = %d\n", ph->map_size);
  tprintf(0, "  bytes_per_page = %d\n", ph->bytes_per_page);
  tprintf(0, "  block_map = %p\n", ph->block_map);
  tprintf(0, "  page_pool = %p\n", ph->page_pool);
  tprintf(0, "  array = %p\n", ph->array);
  tprintf(0, "  write_permit = %d\n", ph->write_permit);
  tprintf(0, "  active_tile_count = %d\n", ph->active_tile_count);
  tprintf(0, "  page_initialization_map = %p\n", ph->page_initialization_map);
}



image_page_queue_entry::image_page_queue_entry(image_page_pool *pool)
{
  int bytes_per_page = pool->page_size;
#ifdef LISP_IMAGE_ARRAYS 
  make_image_page_queue_entry_arrays(this, bytes_per_page);
#else // ! LISP_IMAGE_ARRAYS 
  page = (char *) ((bytes_per_page > 0) ? zalloc(bytes_per_page) : 0);
#endif // ! LISP_IMAGE_ARRAYS
}

#if defined(RUNTIME_LISP_ARRAY_HEADER_OFFSET)

int LISP_ARRAY_HEADER_OFFSET = 0;

extern "C" {

void FREEDIUS_GLOBAL(set_lisp_array_header_offset) (int offset)
{
  LISP_ARRAY_HEADER_OFFSET = offset;
  
}
}

#endif // defined(RUNTIME_LISP_ARRAY_HEADER_OFFSET)


// Called when destroying page in page_pool
void
image_page_queue_entry::free_page_data()
{
#if defined(LISP_IMAGE_ARRAYS)
  unmake_image_page_queue_entry_arrays (page + LISP_ARRAY_HEADER_OFFSET);
#else // !defined(LISP_IMAGE_ARRAYS)
  if (page) xxfree(page); // This looks wrong for LISP_IMAGE_ARRAYS?
#endif
  page = 0;
}

void
image_page_pool::free_page_data (page_queue_entry *entry)
{
  ((image_page_queue_entry*) entry)->free_page_data();
}

//POINTER_LIST image_page_pool_list;

image_page_pool::image_page_pool(int pool_size_pages, int bytes_per_page)
  : page_pool(pool_size_pages) 
{
  page_size = bytes_per_page;
  name = 0;
  null_page = new image_page_queue_entry(this);
  resize_page_pool (pool_size_pages);
  image_page_pool_list = cons((char *)this, image_page_pool_list);
  tprintf(4, "new image_page_pool %p %d bytes_per_page %d pages\n", 
	   this, bytes_per_page,pool_size_pages);
}

extern int default_file_image_page_pool_bytes;
int default_file_image_page_pool_bytes = 10000000;
//int default_file_image_page_pool_bytes = 2000000;

// This function looks bogus --- too much knowledge of tile size assumptions 
int get_page_pool_size (int bytes_per_page)
{  return(default_file_image_page_pool_bytes/bytes_per_page);
}
   
void image_page_pool::describe(FILE* strm)
{
  fprintf(strm,"%d bytes per page", page_size);
}

page_queue_entry *image_page_pool::new_page_queue_entry()
{return new image_page_queue_entry(this);
}

// if pool_size ==0 (unspecified) use the pool as-is.
// if pool_size ==-1 use get_page_pool_size(bytes_per_page)
page_pool *get_page_pool (int bytes_per_page, int pool_size) 
{
  POINTER_LIST page_pool_list = image_page_pool_list;
  if (pool_size == -1) pool_size = get_page_pool_size(bytes_per_page);
  //  tprintf(7, "get_page_pool: bytes_per_page = %d, pool_size=%d\n", bytes_per_page, pool_size);
  while (page_pool_list) {
    image_page_pool *pool = (image_page_pool *) page_pool_list->car;
    if (pool->page_size == bytes_per_page) {
      if (pool_size>0 && pool_size > pool->pool_size) // do not shrink existing pool
	set_page_pool_size(pool, pool_size);
      return(pool);
    }
    page_pool_list = page_pool_list->cdr;
  }
  //tprintf(7, "get_page_pool: creating new image_page_pool.\n");
  if (pool_size) return(new image_page_pool(pool_size, bytes_per_page));
  return(NIL);
}

// This probably needs to be a virtual function
char *get_page_entry_array (page_map* page_map, image_page_queue_entry* entry)
{
  page_map->page_pool->probe_count++;
  if (entry !=  page_map->page_pool->null_page)
    return (entry->page);
  else return(NIL);
}

void release_pages (paged_image_base *image)
{ 
  if (image->page_handler)
    flush_map(image->page_handler);
}

paged_image_base::paged_image_base ()
{}

extern int paged_image_default_block_xdim;
extern int paged_image_default_block_ydim;

int paged_image_default_block_xdim = 256;
int paged_image_default_block_ydim = 256;

paged_image_base::paged_image_base
  (int sx, int sy, IMAGE_ELEMENT_TYPE el_type, int samples_per_pixel, int bx, int by, int pbx, int no_alloc, 
   int offset_bits)
    :blocked_mapped_image(sx, sy, el_type, bx, by, pbx, no_alloc)

{
  page_handler = 0;
  //class_code = FILE_IMAGE_CLASS; // lazy_image must set this itself
  this->samples_per_pixel = samples_per_pixel;
  if (bx==0 || by==0) {
    bx = paged_image_default_block_xdim; 
    by = paged_image_default_block_ydim;
  }
  init_block_dims(bx, by, pbx, samples_per_pixel);

  if (offset_bits==0)
    // block_size is pixels/block, not bytes/block, not samples/block
    // FIXME:  This is wrong for band_interleaved_paged_images whose maps index samples not pixels
    offset_bits=cmelog2(samples_per_pixel*block_size());
  TILE_OFFSET_BITS(this)=offset_bits;
 //  block_map = 0;
  
}


// ******************   GETLINE AND PUTLINE METHODS   ****************** 

void* paged_image_base::get_image_tile (int x, int y) 
{
  tprintf(8, "paged_image_base::get_image_tile(%d,%d)\n", x, y);
  MAP_ELEMENT_TYPE index = iref_index(x, y); 
  page_map* page_map = page_handler;
  int blknum = index >> TILE_OFFSET_BITS(this);
  tprintf(8, "paged_image_base::get_image_tile: blknum = %d, page_map = 0x%x\n",
	  blknum, page_map);

  describe_page_map(page_map);

  image_page_queue_entry * entry 
    = (image_page_queue_entry *) page_map->array[blknum];
  tprintf(8, "paged_image_base::get_image_tile: entry = 0x%x\n", entry);
  if (!entry) {
    warning("Queue entry is zero.  Something's wrong.");
    return 0;
  }

  tprintf(8, "paged_image_base::get_image_tile: entry->status = 0x%x\n", entry->status);
  tprintf(8, "paged_image_base::get_image_tile: page_handler = 0x%x\n", page_handler);
  if (((entry->status) & PAGE_READ_OK_BIT) == 0)
    return ((image_page_queue_entry *)page_handler->read_page_fault(blknum))->page;

  entry->owner->page_pool->probe_count++;
  entry->owner->page_pool->read_count++;
  tprintf(8, "paged_image_base::get_image_tile: return 0x%x\n", entry->page);
  return (entry->page);
}

int paged_image_base::iref_page_number (int x, int y) 
{
  MAP_ELEMENT_TYPE index = iref_index(x, y); 
  return index >> TILE_OFFSET_BITS(this);
}

//
// MODIFICATION UNDER DURESS!!
// THE FOLLOWING IS UNDER CONSTRUCTION!!
//
void paged_image_base::set_page_pool_size (int npages, int shrink) 
{
  tprintf(5, "paged_image_base::set_page_pool_size(%d, %d)\n", npages, shrink);
  page_pool *pool = page_handler->page_pool;
  tprintf(5, "paged_image_base::set_page_pool_size pool = %p\n",  pool);

  // NEW CODE:  pool can be zero.  Why?  If it is, try creating a new pool...
  if (!pool)
    tprintf(3, "paged_image_base::set_page_pool_size ERROR - pool = 0! page_handler=0x%x.\n",
	    page_handler);

  if (pool) {
    int size_change = npages - pool->pool_size;
    tprintf(3, "paged_image_base::set_page_pool_size oldsize =%d, new-size=%d\n",
	    pool->pool_size, npages);
    if (shrink || size_change>0)
      pool->resize_page_pool(size_change);
  }
}



// ***********************  IMAGE CALLBACKS TO LISP  *********************** 


#include "lisp-callback.h"

DEFVOIDCALLBACK(void, FREEDIUS_GLOBAL(make_lisp_paged_image),
		(void* c_image, int xdim, int ydim, int element_type_code, int samples_per_pixel, int tile_offset_bits),
		  (c_image, xdim, ydim, element_type_code, samples_per_pixel,  tile_offset_bits));

DEFVOIDCALLBACK(void, FREEDIUS_GLOBAL(make_lisp_page_handler_block_map),
		(void* c_image, void* page_handler, int npages),
		(c_image, page_handler, npages));

DEFVOIDCALLBACK(void, FREEDIUS_GLOBAL(make_image_page_queue_entry_arrays),
		(image_page_queue_entry* entry, int bytes_per_page),
		(entry, bytes_per_page));

DEFVOIDCALLBACK(void, FREEDIUS_GLOBAL(unmake_image_page_queue_entry_arrays),
		(char* array),
		(array));

void 
image_page_handler::set_page_status (int page_number, int status)
{
#ifdef LISP_IMAGE_ARRAYS
  if (block_map) {
    page_queue_entry *null_page = page_pool->null_page;
    image_page_queue_entry* entry 
      = (image_page_queue_entry*) array[page_number];
    if ((status & (PAGE_READ_OK_BIT | PAGE_WRITE_OK_BIT))
	&& (entry != null_page))
      // entry->page contains the address of the first element of a Lisp allocated array.
      // Adding LISP_ARRAY_HEADER_OFFSET to this address creates the proper Lisp tag for the array.
      block_map[page_number] = LISP_ARRAY_HEADER_OFFSET + entry->page;
    else
      block_map[page_number]  = 0;
  } 
  else warning("image_page_handler::set_page_status: block_map = 0\n");
#endif
}


image*
paged_image_base::map_image (int xdim, int ydim, IMAGE_ELEMENT_TYPE el_type, int samples_per_pixel,
			MAP_ELEMENT_TYPE *xmap, MAP_ELEMENT_TYPE *ymap)
{
  paged_image_base* clone 
    = (paged_image_base*)
    make_paged_image(xdim, ydim, el_type, samples_per_pixel,
		     block_xdim, block_ydim, padded_block_xdim, 1, TILE_OFFSET_BITS(this));
  clone->xmap = xmap; 
  clone->ymap = ymap; 
  clone->page_handler = page_handler;
  //  clone->block_map = block_map;
  clone->flags = flags;
  return clone;
}

//  ****************************  Lisp Callable Functions  ************************


extern "C" {

// This is called from LISP.
void FREEDIUS_GLOBAL(set_paged_image_arrays) (paged_image_base *img, 
			     MAP_ELEMENT_TYPE *xmap, 
			     MAP_ELEMENT_TYPE *ymap,
			     int xdim, int ydim,
			     int maps_hacked)
{
  img->xmap = xmap;
  img->ymap = ymap;
  img->xdim = xdim;
  img->ydim = ydim;
  if (maps_hacked) 
    img->flags|= IMAGE_MAPS_HACKED;
  else img->flags&= ~IMAGE_MAPS_HACKED;

  // Block_map is wrong here.  This image might by a mapped_image to
  // a pre-existing paged_image, requiring the use of it's block-map.
  // Block_map really belongs to the page_handler (page_map).  Each
  //img->block_map = block_map;
  //image_page_handler *ph = (image_page_handler *)img->page_handler;
  //if (ph) ph->block_map = block_map;
}

// This is called from LISP.
void FREEDIUS_GLOBAL(set_image_page_queue_entry_arrays) (image_page_queue_entry *entry,
					void *lisp_array, char *array)
{
  entry->page = array;
  // entry->lisp_array = lisp_array;
}

// This is called from LISP.
void FREEDIUS_GLOBAL(set_page_handler_block_map) (image_page_handler *ph, void **block_map)
{
  ph->block_map = block_map;
}

// This is called from LISP to update the arrays after a possible gc.
void FREEDIUS_GLOBAL(update_paged_image_arrays) (paged_image_base *img,  
						 MAP_ELEMENT_TYPE *xmap, MAP_ELEMENT_TYPE *ymap,
						 void **block_map) 
{
  img->xmap = xmap;
  img->ymap = ymap;						 
  //  img->block_map = block_map;
  img->page_handler->block_map = block_map;
}

int FREEDIUS_GLOBAL(paged_image_read_only) (paged_image_base *img)
{
  return img->page_handler->write_permit == 0;
}

void FREEDIUS_GLOBAL(paged_image_iref_fault) (paged_image_base *img, int page_number)
{ 
  img->page_handler->read_page_fault(page_number);}

void FREEDIUS_GLOBAL(paged_image_iset_fault) (paged_image_base *img, int page_number)
{ 
  img->page_handler->write_page_fault(page_number);}


//typedef struct cimage_struct {} cimage;
void FREEDIUS_GLOBAL(release_image_pages) (cimage *image)
{
  release_pages((paged_image_base *) image);
}

char * FREEDIUS_GLOBAL(get_tileaddr)(cimage *img, int tilex, int tiley)
{
  return(char *)((paged_image_base *)img)->get_image_tile(tilex,tiley);
}

int FREEDIUS_GLOBAL(resize_image_page_pool) (cimage *image0, int size_change_count)
{
  image *img = (image *) image0;
  switch (img->class_code()) {
  case FILE_IMAGE_CLASS: case LAZY_IMAGE_CLASS:
    return resize_image_page_pool((paged_image_base *)img, size_change_count); 
    break;
  default: error("resize_cimage_page_pool: image is not a paged image");
  }
  return 0;
}

page_pool *FREEDIUS_GLOBAL(get_page_pool) (int bytes_per_page, int pool_size)
{
  return get_page_pool (bytes_per_page, pool_size);
}

void FREEDIUS_GLOBAL(image_page_pool_stats) (cimage *image, int resetp)
{
  image_page_pool_stats ((paged_image_base*) image, resetp);
}

void FREEDIUS_GLOBAL(all_image_page_pool_stats) (int resetp)
{
  all_image_page_pool_stats (resetp);
}


image* 
FREEDIUS_GLOBAL(map_paged_image) (paged_image_base* image, int xdim, int ydim, IMAGE_ELEMENT_TYPE el_type,
				  int samples_per_pixel,
				  MAP_ELEMENT_TYPE *xmap, MAP_ELEMENT_TYPE *ymap)
{
  return image->map_image(xdim, ydim, el_type, samples_per_pixel, xmap, ymap);
}

void
FREEDIUS_GLOBAL(set_paged_image_tile_offset_bits) (paged_image_base* image, int tile_offset_bits)
{
  TILE_OFFSET_BITS(image) = tile_offset_bits;
  //TILE_OFFSET_MASK(image)=(1 << tile_offset_bits) -1;
}

// Test that MAP_ELEMENT_SHIFT correctly accesses fixnums.
// Called from test-lisp-array-repn defined in $FREEDIUS/lisp/img/paged-image.lisp

int FREEDIUS_GLOBAL(fixnum_aref_T)(int *lisp_array, int i)
{
  return MAP_ELEMENT_SHIFT(lisp_array[i]);
}

// Reference the jth element of a Lisp (unsigned-byte 8) array 
// that is the ith element of a Lisp type T array.
int FREEDIUS_GLOBAL(aref_T_aref_8)(uchar **lisp_array, int i, int j)
{
  uchar *arr8b = lisp_array[i] - LISP_ARRAY_HEADER_OFFSET;
  //uchar *arr8b = lisp_array[i];
  return arr8b[j];
}

char* FREEDIUS_GLOBAL(array_address) (char *array)
{ 
  return  array;
}

#define PAGER_DEBUG
#ifdef PAGER_DEBUG

int 
FREEDIUS_GLOBAL(test_page_maps) (paged_image_base *img)
{
  image_page_handler* ph = (image_page_handler*)img->page_handler;
  page_map* pagemap = ph;
  page_pool *page_pool = ph->page_pool;
  void **block_map = ph->block_map;
  int map_size = ph->map_size;
  page_queue_entry ** array = ph->array;
  page_queue_entry *null_page = page_pool->null_page;
  int badcnt = 0;

  
  for (int i=0; i<map_size; i++) {
    page_queue_entry *entry = array[i];
    if (entry == null_page) {
      if (block_map[i] != 0) {
	warning("test_page_maps: pagemap[%d]=null_page, but block_map = %p, status = %d\n",
		i, block_map[i], entry->status);
	badcnt++;
      }      
    } else
      if (block_map[i] == 0) {
	if (entry->status & (PAGE_READ_OK_BIT  | PAGE_WRITE_OK_BIT)) 
	warning("test_page_maps: pagemap[%d]=%p, but block_map = %p, status = %d\n",
		i, ((image_page_queue_entry*) entry)->page, block_map[i], entry->status);
      } else 
	if (block_map[i] != 
	  LISP_ARRAY_HEADER_OFFSET + ((image_page_queue_entry*) entry)->page) {
	warning("test_page_maps: pagemap[%d]=%p, but block_map = %p, status = %d\n",
		i, ((image_page_queue_entry*) entry)->page, block_map[i], entry->status);
	badcnt++;
      }
  }
  return badcnt;
} 

#endif /* PAGER_DEBUG */

} // end extern "C"




int paged_image_base::add_to_working_set (int npages, int nrows, int ncols)
{
  page_pool *pool = page_handler->page_pool;
  if (npages == 0) {
    npages = ((nrows==0 && ncols==0)?
	      1+MAX(blocks_wide(), blocks_hi()):
	      nrows*(1+blocks_wide())+ncols*(1+blocks_hi()));
  }
  pool->working_set_count += npages;
  if (pool->pool_size < pool->working_set_count) {
    fprintf(stderr, "add_to_working_set is expanding page pool %d bytes to contain %d pages\n",page_handler->bytes_per_page, pool->working_set_count);
    set_page_pool_size(pool->working_set_count, 0);
  }
  return pool->working_set_count;
}

int paged_image_base::remove_from_working_set (int npages, int nrows, int ncols)
{
  page_pool *pool = page_handler->page_pool;
  if (npages == 0) {
    npages = ((nrows==0 && ncols==0)?
	      1+MAX(blocks_wide(), blocks_hi()):
	      nrows*(1+blocks_wide())+ncols*(1+blocks_hi()));
  }
  pool->working_set_count -= npages;
  if (pool->working_set_count < 0) pool->working_set_count = 0;
  //fprintf(stderr, "remove_from_working_set is reducing working set %d bytes by %d pages\n",page_handler->bytes_per_page, npages);
  return pool->working_set_count;
}

#if 0
int add_to_working_set(image *img, int npages, int nrows, int ncols)
{
  return img->add_to_working_set(npages, nrows, ncols);
}

int remove_from_working_set(image *img, int npages, int nrows, int ncols)
{
  return img->add_to_working_set(npages, nrows, ncols);
}
#endif

extern "C" {

int FREEDIUS_GLOBAL(add_to_working_set) (image *img, int npages, int nrows, int ncols)
{
  return img->add_to_working_set(npages, nrows, ncols);
}

int FREEDIUS_GLOBAL(remove_from_working_set)(image *img, int npages, int nrows, int ncols)
{
  return img->remove_from_working_set(npages, nrows, ncols);
}

} // end extern "C"


END_NAMESPACE_FREEDIUS
