/* 
Tue Dec 26 2006 LHQ  FIXME: 
  Most of the "smarts" here should move to LISP, eliminating this file. 
  The reason they exist here was to support standalone (ie. without LISP apps).
  
*/

//#include "image.h"
#include "file-image.h"
#include "image-io.h"
#include "iu-testbed-io.h"
#include "cme-error.h"
#include "misc.h"

#include <stdlib.h>
#include <sys/types.h>
#ifndef _WIN32
#include <sys/uio.h>
#include <sys/file.h>
#include <unistd.h>
#endif
#include <stdio.h>
#include <fcntl.h>

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

paged_image_base * make_paged_image (int xdim, int ydim, IMAGE_ELEMENT_TYPE element_type, 
      int samples_per_pixel, int block_xdim, int block_ydim, int padded_block_xdim);

int map_file_image_to_file (paged_image_base *img, int iomode, string pathname, int header_length);



//  *******************   FILE_IMAGE IO    *******************

class iu_testbed_file_image_header:
public iu_testbed_image_header
{public:
  iu_testbed_file_image_header(){};
  int recognize_header(int fd);
  int validate_file_image_hdr_dims();
  image *load_image(int fd, char *path);
};

#if 1

int file_image_allowed_block_dims[] = {8*1024, 256*256, 0};

int block_size_ok (int block_size)
{
  for (int i=0; file_image_allowed_block_dims[i]; i++) 
    if (block_size == file_image_allowed_block_dims[i])
      return 1;
  return 0;
}

#else

#include "list.h"

extern INT_LIST file_image_allowed_block_dims;
//INT_LIST file_image_allowed_block_dims = ncons((char *) (8*1024)); // This is weird
INT_LIST file_image_allowed_block_dims = 0;

int block_size_ok (int block_size)
{
  return (memq((PROPKEY) block_size, file_image_allowed_block_dims)?1:0);
}
#endif

extern int acceptable_file_image_block_size (int block_size, int pixel_size)
{
  int block_size_in_bytes = block_size * pixel_size >>3;
  return (block_size_ok(block_size_in_bytes)
	  || (get_page_pool(block_size_in_bytes,get_page_pool_size(block_size_in_bytes))?1:0));
}

// sizes in bytes

//int max_array_image_size =  2048 * 2049;
//int max_array_image_size = 2 * 2048 * 2049;
//int max_huge_array_image_size = 4* 2500*2500;

// ARRAY-TOTAL-SIZE-LIMIT in Lisp determines these limits.
// f---ing Allegro has ARRAY-TOTAL-SIZE-LIMIT of 16 MB.  - FIXED IN NEWER ALLEGRO VERSION
// Mon Dec 25 2006, LHQ: No longer true.  Allegro 7.0 ARRAY-TOTAL-SIZE-LIMIT = (1- (ash 1 29)) = .5GB-1
// CMUCL also has TOTAL = (1- (ash 1 29))

// Not sure why the #if 1 case was added
#if defined(REALLY_STUPID_LISP)
int max_array_image_size = 8 * 1024 * 1024;
int max_huge_array_image_size =  16 * 1024 * 1024 -1; 
#else
#if defined(HAVE_ALLEGRO)   //no longer a problem 
int max_array_image_size = 16 * 1024 * 1024 -1;
int max_huge_array_image_size = 16 * 1024 * 1024 -1; // Allegro sucks -- 16M elements max arrays
#else
int max_array_image_size = 17 * 1024 * 1024;
int max_huge_array_image_size = 64 * 1024 * 1024;
#endif
#endif

extern int max_array_image_bytes;

int file_image_choose_image_class (int xdim, int ydim, 
				   int block_xdim, int block_ydim, int block_size, 
				   int pixel_size)
{
  int block_size_in_bytes = ceiling(block_size*pixel_size, 8);
  int total_bytes = ydim*((xdim* pixel_size)/8);
  int largep = total_bytes > max_array_image_size;
  int hugep = total_bytes > max_huge_array_image_size;
  int file_image_acceptablep = largep && acceptable_file_image_block_size(block_size, pixel_size);

#if 0
  tprintf(3, "file_image_choose_image_class block_size_in_bytes=%d,  largep=%d, hugep=%d, file_image_acceptablep=%d\n",
	  block_size_in_bytes,  largep, hugep, file_image_acceptablep);
#endif
  if (file_image_acceptablep)
    return(FILE_IMAGE_CLASS);
  else if (hugep)
    {error("file_image_choose_image_class unaceptable block dims");return(1);}
  else return(ARRAY_IMAGE_CLASS);
}

#if !defined(USE_CPP_INITS)
int init_image_choose_image_class_fn ()
{
  choose_image_class_fn = (img_choose_fn_t) file_image_choose_image_class;
  return(1);
}
#else
class choose_image_class_fn_init {
 public:
  choose_image_class_fn_init() {choose_image_class_fn = (img_choose_fn_t) file_image_choose_image_class;}
};

choose_image_class_fn_init cicfi = choose_image_class_fn_init ();
#endif // !defined(USE_CPP_INITS)

int any_file_image__block_size4_ok = 1;

int iu_testbed_file_image_header::validate_file_image_hdr_dims()
{
  int block_size_in_bytes = ceiling(block_size * element_size, 8);
  int acceptable_block_size = any_file_image__block_size4_ok 
    || 0 != block_size_ok(block_size_in_bytes)
    || 0 != get_page_pool(block_size_in_bytes);
  //  int page_pool_exists = get_page_pool(block_size_in_bytes)? 1: 0);

  if (! acceptable_block_size)
    return(0);
  
  return (acceptable_block_size &&
	  (choose_image_class(xdim, ydim, block_xdim, block_ydim, block_size, element_size)
	  == FILE_IMAGE_CLASS));
}

Class * iu_testbed_file_image_header_class;
iu_testbed_image_header *iu_testbed_file_image_header1 = 0;

int init_iu_testbed_file_image_header()
{
  tprintf(3,"init_iu_testbed_file_image_header\n");
  
  iu_testbed_file_image_header1 = new iu_testbed_file_image_header;
  add_image_header(iu_testbed_file_image_header1);
  
  return(1);
}

// Return true when we have an iu_testbed_image_header image AND the dimensions cause 
// choose_image_class to select a FILE_IMAGE_CLASS
int iu_testbed_file_image_header::recognize_header(int fd)
{
  iu_testbed_image_header *hdr = iu_testbed_file_image_header1;
  if (! iu_testbed_image_header1->recognize_header(fd))
    return(0);
  hdr->read_header(fd);
  return (validate_file_image_hdr_dims());
}
  
image *iu_testbed_file_image_header::load_image (int fd, char *path)
{ 
  paged_image_base *img 
    = make_paged_image(xdim, ydim, element_type, 1, block_xdim, block_ydim);
  tprintf(3, "loading file_image from %s\n", path);
  tprintf(3, "iu_testbed_load_file_image calling map_image_to_file\n");
  map_file_image_to_file(img, O_RDONLY | O_BINARY, path, header_length);
  return((paged_image_base *)img);
}

extern paged_image_base *
iu_testbed_load_file_image (string path)
{
  int fd, ok;
  iu_testbed_image_header *hdr = new(iu_testbed_image_header);
  // paged_image_base *img;
  fd=read_open(path,1);
  ok = hdr->read_header(fd);
  if (! ok)
    return((paged_image_base *) NULL);
  return((paged_image_base *) hdr->load_image(fd, path));
}

#if 0
// This needs to be generalized to deal with a collection of image formats
extern image *
load_image (char * path)
{int fd, ok;
 iu_testbed_image_header *hdr = new(iu_testbed_image_header);
 paged_image_base *img;
 fd=read_open(path,1);
 ok = hdr->read_header(fd);
 if (! ok)
   return((paged_image_base *) NULL);
 {int img_nbytes = hdr->ydim*((hdr->xdim*hdr->element_size)>>3);
 if (img_nbytes < 

return((image *) iu_testbed_load_image(path));
}
#endif

#ifndef CHRIS_VERSION
int
paged_image_base::save_image(char * path, int error_ok)
{
  if (! page_handler)
    error("paged_image_base::save_image  page_handler is NULL\n");
  
  return (page_handler->save_image(this, path));

}
#else
// Chris:   Different subclasses of paged_image_base must handle the guts of this method,
// as implemented by is specialized page_handler.  I am not sure what you were trying to do here. 
      
int
paged_image_base::save_image(char * path, int error_ok)
{
  file_image_page_handler *ph = (file_image_page_handler *) page_handler;
  int write_permit =  ph->write_permit;
  char *path0 = ph->pathname;
  //iu_testbed_image_header *hdr = new(iu_testbed_image_header);
  //hdr->set_header_slots((blocked_mapped_image *) this);
 
  if (! ph)
    error("paged_image_base::save_image  page_handler is NULL\n");

  if (write_permit)
    {int fd = ph->stream;
      flush_map(ph);
      close(fd);
    }

  if (strcmp(ph->pathname, path) !=0) {
    ph->unmap_file();
    // attempt to rename the file
    if (!write_permit || rename(path0, path)!=0) {
      tprintf(3, "paged_image_base::save_image copying %s to %s\n", path0, path);
      copy_file(path0, path);
      // should we delete the old one ?
    }
    else tprintf(3, "paged_image_base::save_image renamed %s to %s\n", path0, path);
  }

  // reopen image read-only
  if (write_permit)
    map_image_to_file(this, O_RDONLY | O_BINARY, path, ph->first_tile_offset);

  return(0);
}
#endif

// ******************************  Lisp Callable Functions  *********************************

extern "C" {

int FREEDIUS_GLOBAL(array_image_size_limit)(int new_size = 0) {
  int old_size = max_array_image_size;
  if (new_size) 
    max_array_image_size = max_array_image_bytes = new_size;
  return old_size;
}
} // end extern "C"

END_NAMESPACE_FREEDIUS

