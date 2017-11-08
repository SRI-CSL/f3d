#ifndef	__paged_image_h
#define __paged_image_h

#include "page_handler.h"

#include "image.h"
//#include "cme-error.h"
#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

typedef char *string;

typedef class image_page_pool Image_Page_Pool;

class image_page_queue_entry :public page_queue_entry {
public:
  char *page;			// pointer to array of pixels   
  image_page_queue_entry(Image_Page_Pool *pool);
  //  virtual void free_page_data();
  void free_page_data();
};

class image_page_pool :public page_pool {
public:
  int page_size; // bytes_per_page
  char *name;
  // element_type_code is really bogus since pool pages are just bags of bytes
  // that can be cast to any element_type.
  IMAGE_ELEMENT_TYPE element_type_code; 
  image_page_pool(int pool_size_pages, int bytes_per_page);
  //virtual void resize_page_pool(int page_change_count);
  //virtual page_queue_entry *find_free_page_entry();
  virtual page_queue_entry *new_page_queue_entry();
  //  virtual void free_page_queue_entry(page_queue_entry *entry);
  virtual void describe(FILE* strm);  
  virtual void free_page_data (page_queue_entry *entry);
};

class image_page_handler :public basic_page_handler {
public:
  int bytes_per_page;
  void **block_map;
  //paged_image_base *img;
  image_page_handler();
  image_page_handler(image_page_pool* page_pool, int npages, int write_permit = 0);
  void set_page_status (int page_number, int status);
  virtual int map_image_to_file (paged_image_base *img, int iomode, string pathname, 
				 int header_length);
  page_queue_entry *read_page_fault (int page_number);
  page_queue_entry *write_page_fault (int page_number);
  virtual int save_image (paged_image_base *img, char *path);
  //  virtual IMAGE_CLASS class_code() {return UNKNOWN_IMAGE_CLASS;}
};


#define TILE_OFFSET_BITS(img) (img->tile_offset_bits)
#define TILE_OFFSET_MASK(img) ((1 << TILE_OFFSET_BITS(img)) -1 )

extern Class *paged_image_class;

class paged_image_base: public blocked_mapped_image
{public:
  image_page_handler *page_handler; 
  long tile_offset_bits; // These should move to image_page_handler
  //  void **block_map; // same as entry in image_page_handler  -- NO LONGER USED

  IMAGE_CLASS class_code() {return FILE_IMAGE_CLASS;}

  paged_image_base(int sx, int sy, IMAGE_ELEMENT_TYPE el_type, int samples_per_pixel, int bx, int by, int pbx, int no_alloc=0, int offset_bits=0);
  paged_image_base();
  int save_image(char * path, int error_ok = 0);
  virtual void* get_image_tile (int x, int y);
  virtual int iref_page_number (int x, int y); 
  virtual void set_page_pool_size (int npages, int shrink=0);
  int getline (uchar *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  int getline (ushort *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  int getline (short *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  int getline (float *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  int putline (uchar *buf, int x, int y, int n=0, int to_index=0, int band=0);
  virtual image* map_image (int xdim, int ydim, IMAGE_ELEMENT_TYPE el_type, int samples_per_pixel,
			    MAP_ELEMENT_TYPE *xmap, MAP_ELEMENT_TYPE *ymap);

  virtual int add_to_working_set (int npages = 0, int nrows = 1, int ncols = 0);
  virtual int remove_from_working_set (int npages = 0, int nrows = 1, int ncols = 0);
  //virtual IMAGE_CLASS class_code() {return page_handler->class_code();}
  void default_construct_maps(int blk_size);
};


// paged-image-subclasses.h are #included in paged-image-accessors.C
// in order to avoid multiple vtables in IRIX CC
// #include "paged-image-subclasses.h"

paged_image_base *
make_paged_image (int xdim, int ydim,
		 IMAGE_ELEMENT_TYPE element_type = IMG_UNSIGNED_8BIT, int samples_per_pixel = 1, 
		 int block_xdim = 0, int block_ydim = 0, int padded_block_xdim = 0,
		  int no_alloc = 0, int offset_bits = 0);
paged_image_base *
make_band_interleaved_paged_image (int xdim, int ydim,
				   IMAGE_ELEMENT_TYPE element_type,
				   int samples_per_pixel,
				   int block_xdim = 0, int block_ydim = 0, int padded_block_xdim = 0,
				   int no_alloc = 0, int offset_bits = 0);

int add_to_working_set (paged_image_base *img, int npages = 0);
int remove_from_working_set (paged_image_base *img, int npages = 0);

#ifdef LISP_IMAGE_ARRAYS

#define make_lisp_paged_image FREEDIUS_GLOBAL(make_lisp_paged_image)
#define make_image_page_queue_entry_arrays FREEDIUS_GLOBAL(make_image_page_queue_entry_arrays)
#define make_lisp_page_handler_block_map FREEDIUS_GLOBAL(make_lisp_page_handler_block_map)
#define unmake_image_page_queue_entry_arrays FREEDIUS_GLOBAL(unmake_image_page_queue_entry_arrays)

extern "C" {

void
make_lisp_paged_image (void* img, int xdim, int ydim, 
		       int element_type_code, int samples_per_pixel, int tile_offset_bits);

void 
make_image_page_queue_entry_arrays (image_page_queue_entry* entry, int bytes_per_page);

void 
unmake_image_page_queue_entry_arrays (char *array);

void 
make_lisp_page_handler_block_map ( void* c_image, void* page_handler, int npages);


} /* end extern "C" */

#endif /* LISP_IMAGE_ARRAYS */


#if 0 // unused
#define GENERATE_GENERIC_UNARY_PAGED_IMAGE_SWITCH(BODY)\
  switch (image_element_type(img)) {		       \
   case IMG_UNSIGNED_8BIT: \
     {paged_image<unsigned char> *img = (paged_image<unsigned char> *) img0;\
     BODY;}\
   case IMG_UNSIGNED_16BIT: \
     {paged_image<unsigned short> *img = (paged_image<unsigned short> *) img0;\
     BODY;}\
   case IMG_UNSIGNED_32BIT: \
     {paged_image<unsigned int> *img = (paged_image<unsigned int> *) img0;\
     BODY;}\
   case IMG_SIGNED_32BIT: \
     {paged_image<signed int> *img = (paged_image<signed int> *) img0;\
     BODY;}\
   case IMG_RGBA8: \
     {paged_image<RGBA8_PIXEL> *img = (paged_image<RGBA8_PIXEL> *) img0;\
     BODY;}\
   case IMG_RGB8: \
     {paged_image<RGB8_PIXEL> *img = (paged_image<RGB8_PIXEL> *) img0;\
     BODY;}\
   case IMG_DOUBLE_FLOAT:\
          {paged_image<double> *img = (paged_image<double> *) img0;\
     BODY;}\
   default: error("Unhnandled image element_type.");\
   }

#define GENERATE_GENERIC_UNARY_PAGED_IMAGE_OPERATOR(FN_HDR,BODY)\
image * FN_HDR\
{image *img0 = img;\
 image *result_image;\
 GENERATE_GENERIC_UNARY_PAGED_IMAGE_SWITCH(BODY);\
 return(result_image);\
}

#undef GENERATE_GENERIC_UNARY_IMAGE_OPERATOR

#define GENERATE_GENERIC_UNARY_IMAGE_OPERATOR(FN_HDR,BODY)\
image * FN_HDR\
{image *img0 = img;\
 image *result_image;\
 switch (img->class_code())						\
   {case ARRAY_IMAGE_CLASS: GENERATE_GENERIC_UNARY_ARRAY_IMAGE_SWITCH(BODY); break;\
    case FILE_IMAGE_CLASS:  GENERATE_GENERIC_UNARY_PAGED_IMAGE_SWITCH(BODY); break;\
   default: error("Unhandled image class_code=%d.", img->class_code());	\
   }\
  return(result_image);\
}
#endif // unused


END_NAMESPACE_FREEDIUS

#endif /* ! __paged_image_h */

