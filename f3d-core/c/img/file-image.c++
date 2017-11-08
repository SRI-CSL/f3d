// -*- Mode: C++; c-basic-offset: 2 -*-

// $Id: file-image.c++,v 1.19.2.2.2.6 2009/04/17 23:22:04 quam Exp $

/*
This should be merged with iu-testbed-io.c++ since (I think) everything here is
specific to iu-testbed format file images.

*/

// LHQ Sun Mar 18 2007  -- this is apparently needed, otherwise I get an undefined global in libFREEDIUS
#ifdef __GNUG__
#pragma implementation
#endif

// #include "image.h"
#include "array-image.h"
#include "paged-image.h"
#include "file-image.h"
//#include "image-io.h"
#include "iu-testbed-io.h"
#include "cme-error.h"
#include "misc.h"

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#ifndef _WIN32
#include <sys/uio.h>
#include <sys/file.h>
#include <unistd.h>
#endif
#include <stdio.h>

#include "namespace.h"


BEGIN_NAMESPACE_FREEDIUS


void write_page(file_image_page_handler *page_handler, int page_number)
  {page_handler->write_page(page_number);
}


// Should this move to a common place, like misc.c++?

#ifdef _WIN32
extern "C" {
  char *strdup(const char *s) {
    char *result;
    int i, m;

    m = strlen(s) + 1;
    result = XALLOC(char, m);
    for (i = 0; i < m; i++) result[i] = s[i];
    return result;
  }
} // end extern "C"
#endif /* _WIN32 */

// ******************** file_image_page_handler  ******************** 

template <class T, class T2>
void 
fill_array (T *arr, int n, T2 value)
{
  for(int i=0; i<n; i++) arr[i]=(T)value;
}

int verbose = 0;

file_image_page_handler::file_image_page_handler(image_page_pool* page_pool, 
						 int npages)
    : image_page_handler(page_pool, npages)
{
  //endian = 0;
  stream = 0;
  pathname = 0;
  tprintf(4, "new file_image_page_handler %d pages\n", npages);
}

// same as read_page_fault except returns NULL if get_page would be required.
page_queue_entry *
file_image_page_handler::probe_page_fault (int page_number)
{
  page_queue_entry * entry = page_map_entry(this, page_number);
  int status = entry->status;
  page_pool->probe_count++;
  tprintf(6, "probe_page_fault: status of page %d: 0x%x\n", page_number, status);
  if (status & PAGE_AGED_BIT)
    rejuvenate_page(this, entry);
  else if (entry == page_pool->null_page)
    return(0);
  else 
    error("read_page_fault: read attempted from page with wierd status %d", status);
  return(entry);
}


OFF_T file_image_page_handler::page_filepos(int page_number)
{
  return(first_tile_offset + bytes_per_page* page_number);
}

extern "C" {
  // To support hacks with mmap to file images.
long FREEDIUS_GLOBAL(file_image_page_filepos) (cimage *image, int page_number) {
  file_image_page_handler *ph = (file_image_page_handler*) ((paged_image_base *) image) -> page_handler;
  return ph->page_filepos(page_number);
}

} // end extern "C"

#if 0

extern "C" {
  void FREEDIUS_brk (const char *str)
{
}

} // end extern "C"
#endif

#include <errno.h>

void
file_image_page_handler::read_page (int page_number)
{
  image_page_pool* page_pool = (image_page_pool*) this->page_pool;
  image_page_queue_entry* entry = (image_page_queue_entry*)page_map_entry(this, page_number);
  char *array = entry->page;
  // can the bytes_per_page be different in the page_pool vs the page_handler?
  int bytes_per_page = page_pool->page_size;

  if (verbose) 
    warning("file_image_page_handler::read_page %d\n", page_number);
  if (entry == NIL) error("file_image_page_handler::read_page got entry=NIL");

  if (page_initialization_map && page_initialization_map[page_number] == 0) {
    fill_array(array, bytes_per_page, 0);
    entry->status |= PAGE_DIRTY_BIT | PAGE_READ_OK_BIT;
    page_initialization_map[page_number] = 1;
  } else {
    entry->status = PAGE_READ_OK_BIT;
    page_pool->read_count++;

    OFF_T filepos = page_filepos(page_number);

    int bytes_read, retry_count=0, rem=0;

#if defined(_WIN32) // && !defined(MINGW)
    OFF_T seek_offset = _lseeki64(stream, filepos, SEEK_SET);
#else
    OFF_T seek_offset = lseek(stream, filepos, SEEK_SET);
#endif

    if (seek_offset < 0)
      //FREEDIUS_brk("seek error");
      error("file_image_page_handler::read_page - seek error: pos=%d errno=%d\n", filepos, errno);
    #if 0 // errno codes
       #define	EINVAL		22	/* Invalid argument */
       #define	EBADF		 9	/* Bad file number */
       #define	EOVERFLOW	75	/* Value too large for defined data type */
       #define	ESPIPE		29	/* Illegal seek */
    #endif

    // bytes_per_page is probably the wrong thing.  Should be the actual number of
    // bytes per tile in the file.
    // The page_pool pages might be bigger.
    // Sometimes (especially in Windoze) read doesn't catch everything:
    bytes_read = fullread(stream, array, bytes_per_page);
    
    if (bytes_read != bytes_per_page) {
	tprintf(3, "file_image_page_handler::read_page - read error: pos=%lu, len =%lu, bytes read=%lu\n", 
		filepos, bytes_per_page, bytes_read);
	error("file_image_page_handler::read_page - read error: pos=%d, len =%d, bytes read=%d\n", 
	      filepos, bytes_per_page, bytes_read);
    } else if (0)
      tprintf(3, "file_image_page_handler::read_page - read ok: pagenum=%d, pos=%lu, len =%lu, bytes read=%lu\n", 
	      page_number, filepos, bytes_per_page, bytes_read);
  }
  set_page_status(page_number, entry->status);
}

void
file_image_page_handler::write_page (int page_number)
{
  image_page_queue_entry* entry = (image_page_queue_entry*)page_map_entry(this, page_number);
  if (write_permit && (entry->status & PAGE_DIRTY_BIT)) {
    int bytes_per_page = ((image_page_pool *) page_pool)->page_size;
    page_pool->write_count++;
    //#ifdef _WIN32
#ifdef _WIN32 // && !defined(MINGW)
    if (_lseeki64(stream, page_filepos(page_number), SEEK_SET) == -1L)
#else
    if (lseek(stream, page_filepos(page_number), SEEK_SET) == -1L)
#endif
      error("Seek to page filepos failed!!  page_number=%d\n", page_number);
    int bytes_written = write(stream, entry->page, bytes_per_page);
    if (bytes_written != bytes_per_page)
      error("file_image_page_handler::write_page  - write error");
    if (verbose) warning("file_image_page_handler::write_page %d\n", page_number);
    //tprintf(3, "file_image_page_handler::write_page %d\n", page_number);
    // generic from here
    entry->status = PAGE_READ_OK_BIT;
    set_page_status(page_number, entry->status);
    if (verbose) 
      warning("file_image_page_handler::write_page %d\n", page_number);
  }
}


void file_image_page_handler::force_page_initialization()
{
  //char *page_initialization_map = page_initialization_map;
  image_page_pool* page_pool = (image_page_pool*)this->page_pool;
  int blknum;
  if (page_initialization_map)
    for (blknum=0; blknum<map_size; blknum++)
      if (page_initialization_map[blknum] == 0) {
	page_pool->probe_count++;
	get_page(blknum);
      }
}


void file_image_page_handler::unmap_file(int closep)
{
  int old_write_permit = write_permit;
  if (closep != 2) { // do not abort
    force_page_initialization();
    set_write_permit(0);
  }
  free_page_map(this);
  if (closep) {
    if (closep == 2) {
      set_write_permit(old_write_permit);
      error("file_image_page_handler::unmap_file closep abort not fully implemented\n");
    } 
    else if (stream)
      close(stream);

    stream = 0;
  }
}




// ********************  FILE IMAGE CREATION  ********************  
typedef file_image_page_handler* (file_image_page_handler_maker) (image_page_pool *page_pool, int npages);



// This is specific to file_images -- change the name accordingly
extern void 
make_page_handler (paged_image_base *img, int iomode, file_image_page_handler_maker *ph_maker = 0)
{
  int bytes_per_page = bytes_per_tile(img);
  tprintf(5, "make_page_handler: bytes_per_page = %d\n", bytes_per_page);
  image_page_pool* page_pool 
    = (image_page_pool*)get_page_pool(bytes_per_page, get_page_pool_size(bytes_per_page));
  int npages = img->blocks_wide()*img->blocks_hi();
  tprintf(5, "make_page_handler: npages = %d\n", npages);
  file_image_page_handler *ph;

  if (ph_maker == 0) 
    ph = new file_image_page_handler(page_pool, npages);
  else ph = (*ph_maker) (page_pool, npages);

  tprintf(5, "make_page_handler: page handler ph = %x\n", ph);
  img->page_handler =  ph;
  //ph->img = img;
  //ph->element_type_code = image_element_type(img);
  //  ph->set_write_permit(iomode != O_RDONLY);
  ph->set_write_permit(!RDONLY_P(iomode));
#ifdef LISP_IMAGE_ARRAYS
  //void make_lisp_page_handler_block_map(paged_image_base*, image_page_handler*, int);
  make_lisp_page_handler_block_map(img, ph, npages);
  //  img->block_map = ph->block_map;
#endif
  tprintf(5, "make_page_handler: done\n");

}

int
file_image_page_handler::map_image_to_file (paged_image_base *img, int iomode, string pathname, int header_length)
{
   // This is really the only part relevent to file mapping.
  first_tile_offset = header_length;
  //  set_write_permit(iomode != O_RDONLY);
  set_write_permit(!RDONLY_P(iomode));

  // We define O_BINARY as 0x0 in Linux
  stream = general_open(pathname, iomode | O_BINARY);

  if (stream <= 0)
    error("map_image_to_file: the file %s does not exist.", pathname);
  this->pathname = pathname;
  return(0);
}

extern int
map_file_image_to_file (paged_image_base *img, int iomode, string pathname, int header_length)
{
  int pathname_copied = 0;
  if (! img->page_handler) {
    pathname_copied = 1;
    // Must copy, since callback can cause GC, moving the pathname.
    pathname = strdup(pathname);
    make_page_handler(img, iomode);
  }

  file_image_page_handler *ph = (file_image_page_handler*) img->page_handler;
  // This is really the only part relevent to file mapping.
  ph->first_tile_offset = header_length;
  //  ph->write_permit = iomode != O_RDONLY;
  ph->write_permit = !RDONLY_P(iomode);

  ph->stream = general_open(pathname, iomode | O_BINARY);
  
  if (ph->stream < 0)
    error("map_image_to_file: the file %s does not exist.", pathname);
  ph->pathname = pathname;
  //if (pathname_copied) xxfree(pathname); // Is this really correct -- doesn't pathname persist in ph?
  return(0);
}


int
file_image_page_handler::save_image (paged_image_base *img, char *path)
{
  char *path0 = strdup(pathname);
  int old_write_permit = write_permit;
  if (write_permit) {
    int fd = stream;
    flush_map(this);
    close(fd);
  }
  tprintf(3,"file_image_page_handler::save_image old-pathname=%s\n", pathname);
  if (strcmp(pathname, path) !=0) {
    unmap_file(); // the stream is also closed and write_permit zeroed.
    // attempt to rename the file
    if (old_write_permit) {
      int flg = rename(path0, path);
      if (flg==-1) {
	tprintf(3,"renamed failed in file_image_page_handler::save_image.  errno=%s\n", strerror(errno));
	copy_file(path0, path);
	fprintf(stderr, "paged_image_base::save_image copying %s to %s\n", path0, path);
      } else
	fprintf(stderr, "file_image_page_handler::save_image renamed %s to %s\n", path0, path);
    } else {
      copy_file(path0, path);
      fprintf(stderr, "paged_image_base::save_image copying %s to %s\n", path0, path);
      // should we delete the old one ?
    }
  }

  /* FIXME -- this looks wrong for case where image is renamed or copied.
       The image is not remapped and the stream is not reopened.
       
  */
  
  // reopen image read-only.  Q: is first_tile_offset ok on Windows?
  if (write_permit)
    map_image_to_file(img, O_RDONLY | O_BINARY, path, first_tile_offset); 

  return(0);

}

extern string default_file_image_directory_pathname;

string default_file_image_directory_pathname = (string) 0;

extern "C" {

void FREEDIUS_GLOBAL(set_default_file_image_directory) (string path) {
  default_file_image_directory_pathname = strdup(path);
}

} // end extern "C"

string 
default_file_image_directory_path ()
{
  //static string default_file_image_directory_pathname = (string) 0;
  if (default_file_image_directory_pathname)
    return(default_file_image_directory_pathname);
  else {
    default_file_image_directory_pathname = getenv("CME_TMP_FILE_IMAGE_DIR");

    printf("default path (len=%d) = %s\n",
	   (int) strlen(default_file_image_directory_pathname),
	   default_file_image_directory_pathname);

    if (! default_file_image_directory_pathname)
#ifdef _WIN32
      default_file_image_directory_pathname = "C:/WINDOWS/TEMP";
#else
      error("Environment variable CME_TMP_FILE_IMAGE_DIR is not defined.\n");
#endif
   // make sure the directory exists -- FIXME - should this use stat() instead of open?
   int dirfd = read_open(default_file_image_directory_pathname,0);
   if (dirfd < 0) 
     error("default_file_image_directory_path: Directory %d does not exist", 
	   default_file_image_directory_pathname);
   close(dirfd);	
   return(default_file_image_directory_pathname);
  } 
}

int file_image_allocation_counter = 0;

internal string
str_copy(string s2)
{
  string s1 = (string) galloc(strlen(s2)+1);
  return(strcpy(s1,s2));
}

// Codewarrior 5.0 does NOT support stack allocation.  Wish it did...
#if defined(CODEWARRIOR) || defined(_WIN32) // C++ always supports stack allocation
extern string allocate_file_image_pathname()
{
  string dir = default_file_image_directory_path();
  string localhost = local_hostname();
  char *path=XALLOC(char, strlen(dir)+strlen(localhost)+(11+6+6+20+100));
  char *path2;
  file_image_allocation_counter++;
  sprintf(path, "%s/%s-%d-%d.fileimg",
	  dir,
	  localhost,
	  GetCurrentProcessId(), 
	  file_image_allocation_counter);
  tprintf(5, "allocate_file_image_pathname: pathname is %s\n", path);
  path2=str_copy(path);
  xxfree(path);
  return(path2);
}
#else
//#ifdef __GNUG__ 
extern string allocate_file_image_pathname()
{
  string dir = default_file_image_directory_path();
  string localhost = local_hostname();

  tprintf(7, "allocate_file_image_pathname: dir is %s\n", dir);
  tprintf(7, "allocate_file_image_pathname: localhost is %s\n", localhost);
  tprintf(7, "allocate_file_image_pathname: pid is %d\n", getpid());

  char path[strlen(dir)+strlen(localhost)+(11+6+8+100)];  // is this long enough?
  file_image_allocation_counter++;
  sprintf(path, "%s/%s-%d-%d.fileimg",
	  dir,
	  localhost,
	  getpid(), 
	  file_image_allocation_counter);
  tprintf(5, "allocate_file_image_pathname: pathname is %s\n", path);
  return(str_copy(path));
}
#endif

extern int file_image_default_block_xdim;
extern int file_image_default_block_ydim;

int file_image_default_header_length = 1024;// gcc warning here
int file_image_default_block_xdim = 256;
int file_image_default_block_ydim = -256;

extern paged_image_base *
make_iu_testbed_file_image (int xdim, int ydim,
			    IMAGE_ELEMENT_TYPE element_type,
			    int samples_per_pixel,
			    int block_xdim, int block_ydim, int padded_block_xdim,
			    char *pathname)
{
  paged_image_base *img;
  int mkfile_status, header_length;
#if defined(_WIN32) // && !defined(MINGW)
  __int64 file_length;
#else
  off_t file_length;
#endif		
  if (block_xdim == 0) block_xdim = file_image_default_block_xdim;
  if (block_ydim == 0) block_ydim = file_image_default_block_ydim;
  tprintf(4, "make_file_image: block_xdim = %d, block_ydim = %d\n", block_xdim, block_ydim);
  if (padded_block_xdim == 0) padded_block_xdim = block_xdim;
  header_length = file_image_default_header_length;
  if (! pathname) pathname = allocate_file_image_pathname();
  tprintf(3, "make_file_image %s %d %d %d %d %d %d\n",
	  pathname,  xdim, ydim, element_type, 
	  block_xdim, block_ydim, padded_block_xdim);
  img = make_paged_image(xdim, ydim, element_type, samples_per_pixel, block_xdim, block_ydim);
  if (! img) error("make_paged_image failed");

  file_length = header_length + img->blocks_wide() * img->blocks_hi() * bytes_per_tile(img);
  mkfile_status = mkfile(pathname, file_length, 'b');
  if (mkfile_status != 0)
    {error("make_file_image: mkfile failed\n");
    return ( 0);}
  
  map_file_image_to_file(img, O_RDWR | O_BINARY, pathname, header_length);
  tprintf(3, "make_file_image: mapped.\n");
  { 
    // THIS IS THE SPECIALIZATION TO IU-TESTBED FORMAT
    iu_testbed_image_header *hdr = new(iu_testbed_image_header);
    file_image_page_handler *ph = (file_image_page_handler *) img->page_handler;
    int fd = ph->stream;
    tprintf(4, "make_file_image: setting header slots\n");
    hdr->set_header_slots((blocked_mapped_image *) img);
    hdr->header_length = header_length;
    tprintf(4, "make_file_image: writing header\n");
    hdr->write_header(fd);
  }
  tprintf(4, "make_file_image: done\n");
  return(img);
}

extern FILE_IMAGE_FORMAT file_image_default_format;
// Codes are 1 = iu_testbed format, 2 = TIFF format
FILE_IMAGE_FORMAT file_image_default_format = TIFF_FILE_IMAGE_FORMAT;

extern "C" {

int FREEDIUS_GLOBAL(set_file_image_default_format) (FILE_IMAGE_FORMAT new_format)
{
  FILE_IMAGE_FORMAT old = file_image_default_format;
  file_image_default_format = new_format;
  return(old);
}

} // end extern "C"

paged_image_base *
make_tiff_file_image (int xdim, int ydim,
		      IMAGE_ELEMENT_TYPE element_type,
		      int samples_per_pixel,
		      int block_xdim, int block_ydim, int padded_block_xdim,
		      char *pathname);

extern paged_image_base *
make_file_image (int xdim, int ydim,
		 IMAGE_ELEMENT_TYPE element_type,
		 int samples_per_pixel,
		 int block_xdim, int block_ydim, int padded_block_xdim,
		 char *pathname)
{
  switch (file_image_default_format) {
  case 1: return make_iu_testbed_file_image(xdim, ydim, element_type, samples_per_pixel,
					    block_xdim, block_ydim, padded_block_xdim, pathname);
  case 2: return make_tiff_file_image(xdim, ydim, element_type, samples_per_pixel,
					    block_xdim, block_ydim, padded_block_xdim, pathname);
  default: error("make_file_image illegal file format %d", file_image_default_format);
  }
}




/* ARRRGH -- we have 
    max_array_image_bytes in this file,
    max_array_image_size and max_huge_array_image_size in file-image-io.c++

The problem is that iu_testbed_file_image_header::validate_file_image_hdr_dims
uses choose_image_class and max_array_image_size to decide about file_image acceptability
but make_file_or_array_image uses a different uses max_array_image_bytes to decide.

LHQ Sat Apr 13 2002: I am changing code here to use max_array_image_size

LHQ Thu Apr  1 2004: There are probably good reasons for separate limits.
                     This problem is being reduced by allowing LISP the 
		     choice of calling make_array_image or make_file_image.
*/

// FLUSH THIS
extern int max_array_image_bytes;
//int max_array_image_bytes = 512 * 700;
int max_array_image_bytes = 2048 * 2049;
//int max_array_image_bytes = 2 * 2048 * 2049;

// Thu Apr 15 2004 FIXME:  Add support for make_tiff_file_image

extern image *
make_file_or_array_image (int xdim, int ydim, 
			  IMAGE_ELEMENT_TYPE element_type = IMG_UNSIGNED_8BIT,
			  int samples_per_pixels=1,
			  int block_xdim = 0, int block_ydim = 0,
			  int padded_block_xdim = 0)
{
  int element_size = image_element_size_from_type(element_type)*samples_per_pixels;
  int nbytes = ydim*((xdim*element_size)>>3);
  // uggh -- this forces array images to have these block dims.
  if (nbytes > max_array_image_bytes) {
    tprintf(3, "make_image is creating a file_image\n");
    return((image *) make_file_image(xdim, ydim, element_type, samples_per_pixels,
				     block_xdim, block_ydim, padded_block_xdim));
  }
  else {
    tprintf(3, "make_image is creating an array_image\n");
    return((image *) make_array_image(xdim, ydim, element_type, samples_per_pixels,
				      block_xdim, block_ydim,
				      padded_block_xdim));
  }
}


// New July 2008 -- let Lisp code decide about defaults related to making images.
#define LISP_MAKE_IMAGE

#if defined(LISP_MAKE_IMAGE)

#include "lisp-callback.h"

DEFCALLBACK(image *, FREEDIUS_GLOBAL(make_image_callback),
	    (int xdim, int ydim, 
	     IMAGE_ELEMENT_TYPE element_type,
	     int samples_per_pixel,
	     int block_xdim, int block_ydim, int padded_block_xdim),
	    (xdim, ydim, element_type, samples_per_pixel, block_xdim, block_ydim, padded_block_xdim))

extern int init_make_file_image_fn ()
{
  fprintf(stderr, "make_image_fn = FREEDIUS_GLOBAL(make_image_callback)\n");
  make_image_fn = (img_make_fn_t) FREEDIUS_GLOBAL(make_image_callback); 
  return(1);
}

#else
#if !defined(USE_CPP_INITS)
extern int init_make_file_image_fn ()
{
  make_image_fn = (img_make_fn_t) make_file_or_array_image; 
  return(1);
}
#else

class file_image_fn_init {
 public:
  file_image_fn_init() {
    printf("file_image_fn_init\n");
    make_image_fn = (img_make_fn_t) make_file_or_array_image; }
};

file_image_fn_init fifi = file_image_fn_init();

#endif //!defined(USE_CPP_INITS)

#endif // defined(LISP_MAKE_IMAGE)

END_NAMESPACE_FREEDIUS
