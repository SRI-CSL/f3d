#ifndef	__image_io_h
#define __image_io_h


#if defined( _WIN32 ) || defined( MINGW )

#define OFF_T     __int64
#define FILE_SEEK _lseeki64

#else // defined( _WIN32 ) || defined( MINGW )

#define OFF_T     off_t
#define FILE_SEEK lseek // Not sure this is the right one to use.

#endif // defined( _WIN32 ) || defined( MINGW )


#include "image.h"

#define __list_ops_h // prevent defining the list operators
#include "list.h"
#undef __list_ops_h

//#include "array_image.h"

#include "base-class.h"

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

extern POINTER_LIST image_header_list;

extern Class *basic_image_header_class;

// FIXME -- need 

class basic_image_header :public Object
{public:
  //  static Class* clas;
  //virtual Class* getClass(void);
  char *path;
  int header_length;
  int xdim; int ydim; 
  int element_size; 
  image *into_image; // ugly communication var used by reload_image

  IMAGE_ELEMENT_TYPE element_type;
  ENDIAN endian;
  basic_image_header():Object(){}
  virtual int recognize_header(int fd);
  virtual void set_header_slots (blocked_mapped_image *img);
  virtual int read_header(int fd);
  virtual void write_header(int fd);
  virtual image *load_image(int fd, char *path);
  virtual char* read_property_list_string(char *path){
    return 0;}
};

class basic_blocked_image_header :public basic_image_header
{public:
  int block_xdim;
  int block_ydim;
  int padded_block_xdim;
  int block_size;
  int blocks_wide;
  int blocks_hi;
  int pixel_skip; // file offset to first pixel of image
  basic_blocked_image_header():basic_image_header() {}
};

// these are defined in image.h -- why are these here too?
image *load_image (char * path, int error_ok);
basic_image_header *find_image_header (char * path);
basic_image_header* find_read_image_header (char * path, int error_ok=1);

int save_image (image *img, char *path, int error_ok);
  
void add_image_header (basic_image_header *new_ndr);

char* read_property_list_string (char *path);

END_NAMESPACE_FREEDIUS

#endif /* ! __image_io_h */
