#include "iu-testbed-io.h"
#include "misc.h"
#include "image-io.h"
#include "list.h"
#include "cme-error.h"
#include <stdio.h>
#ifndef _WIN32
#include <unistd.h>
#endif

#include <fcntl.h>

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

setClass(basic_image_header, Object);

image *basic_image_header::load_image(int fd, char *path)
{error("basic_image_header::load_image is not defined.\n");
 return((image *) 0);
}

extern POINTER_LIST image_header_list;
// problems here with initializers -- how do we guarantee that image_header_list is zeroed
// before add_image+header is called.  Get rid of all of this crap in C++ and do it in Lisp.
POINTER_LIST image_header_list = 0;

#if 1
void add_image_header (basic_image_header *new_ndr)
{
  if (! memq((char *) new_ndr, image_header_list))
    image_header_list = nconc(image_header_list, ncons((char *) new_ndr));
}
#else
void add_image_header (basic_image_header *new_ndr)
{ POINTER_LIST p1;
 tprintf(4, "start add_image_header\n");
  p1 = XALLOC(1, Pointer_List);
  tprintf(4, "galloc ok %p\n", p1);fflush(stderr);
  memq((char *) new_ndr, image_header_list);
  tprintf(4, "memq ok\n");fflush(stderr);
  p1 = ncons((char *) new_ndr);
  tprintf(4, "ncons ok %p\n", p1);fflush(stderr);
  tprintf(4, "%p, last(ncons xx)=%p\n", image_header_list, last(p1));
  
  nconc(image_header_list, ncons(new_ndr));
  tprintf(4, "nconc ok\n");
  if (! memq((char *) new_ndr, image_header_list))
    image_header_list = nconc(image_header_list, ncons(new_ndr));
  tprintf(4, "add_image_header ok\n");
}
#endif

basic_image_header *
find_image_header(int fd)
{
  POINTER_LIST l = image_header_list;
  basic_image_header *hdr;
  for (l = image_header_list; l !=NIL; l = l->cdr) {
    hdr = (basic_image_header *) l->car;
    if (hdr->recognize_header(fd)) {
      return(hdr);
    }
  }
  return(NIL);
}

#if 0 // unused Mon Aug  2 2004 -- eliminate the only caller to className
void print_image_header_list ()
{
  POINTER_LIST l = image_header_list;
  basic_image_header *hdr;
  fprintf(stderr, "Known image header classes:\n");
  for (l = image_header_list; l !=NIL; l = l->cdr) {
    hdr = (basic_image_header *) l->car;
    fprintf(stderr, "%s\n", hdr->className());
    }
  }

  void string_test (char *s)
  {
    fprintf(stderr, "%s\n", s);
  }
#endif

extern basic_image_header*
find_read_image_header (char * path, int error_ok)
{
  int fd;
  fd=read_open(path,1);
  if (fd<0) {
    if (error_ok == 0) {
      error("load_image cannot find file %s.\n", path);
      return ((basic_image_header *) 0);
    } else {
      warning("load_image cannot find file %s.\n", path);
      return ((basic_image_header *) 0);
    }
  }
  basic_image_header *hdr = find_image_header(fd);
  if (! hdr) {
    warning("load_image unrecognized image header in file %s\n.", path);
    error("");
  }
  hdr->path = path;
  if (hdr->read_header(fd) == 0) 
    error("load_image read_header failed");
  close(fd);
  return hdr;
}


extern image *
load_image (char * path, int error_ok)
{
  int fd;
  fd=read_open(path,1);
  if (fd<0) {
    close(fd);
    if (error_ok == 0) {
      error("load_image cannot find file %s.\n", path);
      return ((image *) 0);
    } else {
      warning("load_image cannot find file %s.\n", path);
      return ((image *) 0);
    }
  }
  basic_image_header *hdr = find_image_header(fd);
  if (! hdr) {
    close(fd);
    //warning("load_image unrecognized image header in file %s\n.", path);
    error("load_image unrecognized image header in file %s\n.", path);
  }
  hdr->into_image = NULL;
  hdr->path = path;
  if (hdr->read_header(fd) == 0) {
    close(fd);
    error("load_image read_header failed");
  }
  
  image* img = hdr->load_image(fd, path);

  // FIXME should the fd be closed here?
  close(fd);
  return(img);
}
 
extern basic_image_header*
find_image_header (char * path)
{
  int fd;
  fd=read_open(path,1);
  if (fd<0) return ((basic_image_header *) 0);
  basic_image_header *hdr = find_image_header(fd);
  close(fd);
  return hdr;
}


extern "C" {
int FREEDIUS_GLOBAL(file_image_header_params) (char *path, int params[7]) {
  basic_blocked_image_header *hdr = (basic_blocked_image_header *)find_read_image_header(path);
  if (hdr == 0) return 0;

  params[0] = (int) hdr->element_type;
  params[1] = hdr->xdim;
  params[2] = hdr->ydim;
  params[3] = hdr->block_xdim;
  params[4] = hdr->block_ydim;
  params[5] = 0;  // unknown FIXME
  params[6] = 0; // magic flag bits -- currently undefined
  return 1;
}

} // end extern "C"

// Only works if the image exactly matches the file characteristics:

extern image *
reload_image (char * path, image *im, int error_ok)
{
  int fd;
  fd=read_open(path,1);
  if (fd<0) {
    close(fd);
    if (error_ok == 0) {
      error("load_image cannot find file %s.\n", path);
      return ((image *) 0);
    } else {
      warning("load_image cannot find file %s.\n", path);
      return ((image *) 0);
    }
  }
  basic_image_header *hdr = find_image_header(fd);

  if (! hdr) {
    close(fd);
    warning("load_image unrecognized image header in file %s\n.", path);
    error("");
  }

  // Here, we should be checking that into_image is consistent with
  // the file contents...
  hdr->into_image = im;
  hdr->path=path;
  hdr->read_header(fd);
  hdr->load_image(fd, path);
  hdr->into_image = NULL; // LHQ 20060701 do not retain this pointer.
  close(fd);
  return im;
}
 
// Need to be able to control the file format for saving.

extern int
save_image (image *img, char *path, int error_ok)
{
  return(img->save_image(path, error_ok));
}



char* read_property_list_string (char *path)
{
  int fd;
  fd=read_open(path,1);
  if (fd<0) {
    close(fd);
    warning("read_property_list_string cannot find file %s.\n", path);
    return (0);
  }
  basic_image_header *hdr = find_image_header(fd);
  if (! hdr) {
    warning("read_property_list_string unrecognized image header in file %s\n.", path);
    close(fd);
    return(0);
  }
  hdr->into_image = NULL;
  hdr->path = path;
  if (hdr->read_header(fd) == 0) {
    close(fd);
    error("read_property_list_string read_header failed");
  }
  close(fd);
  char* pl_string = hdr->read_property_list_string(path); 
  return(pl_string);
}

 
END_NAMESPACE_FREEDIUS




// Code moved here from array-image.c++, which was the wrong place for this code.
// The modularity is still bad, since iu-testbed format and tiff-io are hardwired.

#include "array-image.h"

BEGIN_NAMESPACE_FREEDIUS

extern int save_image_default_format;
// Codes are 1 = iu_testbed format, 2 = TIFF format
int save_image_default_format = 2;

extern "C" {

int FREEDIUS_GLOBAL(set_save_image_default_format) (int new_format)
{
  int old = save_image_default_format;
  save_image_default_format = new_format;
  return(old);
}

} // end extern "C"

// Uggh -- bad modularity having dependency on tiff.h here
#include <tiff.h>

int iu_testbed_save_image (array_image_base *img, string path, int error_ok = 0);
int save_untiled_tiff_image (image *img, string path, int error_ok = 0,
			     int compression_mode = COMPRESSION_NONE, int jpeg_quality = 75);


int array_image_base::save_image(char *path, int error_ok)
{
  switch (save_image_default_format) {
  case 1: 
    return(iu_testbed_save_image(this, path, error_ok));
  case 2: 
    return(save_untiled_tiff_image(this, path, error_ok));
  default: error("array_image_base::save_image illegal save_image format %d", save_image_default_format);
  }
}

END_NAMESPACE_FREEDIUS
