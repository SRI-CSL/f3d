#include "array-image.h"
#include "cme-error.h"

#include <string.h>
#include <stdio.h>
#include <fcntl.h>

#include <sys/types.h>
#ifndef _WIN32
#include <sys/uio.h>
#include <sys/file.h>
#include <unistd.h>
#endif

#include <stdio.h>

#include "base-class.h"
#include "image-io.h"
#include "iu-testbed-io.h"

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS


// Not sure what file these should go in/

void basic_image_header::set_header_slots (blocked_mapped_image *img)
{error("basic_image_header::set_header_slots  is undefined");
}

int basic_image_header::read_header(int fd)
{error("basic_image_header::read_header  is undefined");
 return(1);
}

int basic_image_header::recognize_header(int fd)
{error("basic_image_header::recognize_header  is undefined");
 return(1);
}

void basic_image_header::write_header(int fd)
{error("basic_image_header::write_header  is undefined");
}


setClass(iu_testbed_image_header, basic_image_header);

#define string char *

/* Defines for the TB01 header */
/* Offsets into the header for each field */

#define TB_BASIC_HDR_LENGTH 120
#define TB_HDR_SZ 1024
#define IMG_TYPE 0
#define HDR_TYPE 8
#define HDRLEN 16
#define SCANDIR 24
#define SIGNED 32
/* The following 2 fields are unused by, but are written to 
   maintain compatibility with IU and LISPM "Testbed" formats */
#define NBAND 40
#define BITBAND 48

#define BITPIX 56
#define XSIZ 64
#define YSIZ 72
#define XBLK 80
#define YBLK 88
#define NXBLK 96
#define NYBLK 104
#define SCALE 112
#define OFFSET 128

#define FILL 144


int read_int_field8(char *buf)
{
  char str[9];
  strncpy(str, buf, 8);
  str[8]='\0';
  return((int)atoi(str));
}

//iu_testbed_image_header::iu_testbed_image_header(){};

int iu_testbed_image_header::recognize_header (int fd)
{ 
  char buf[TB_BASIC_HDR_LENGTH];
  while (lseek(fd, 0L, SEEK_SET) == -1L) tprintf(3, "iu_testbed: lseek fails.  retrying...\n");

  fullread(fd,buf,TB_BASIC_HDR_LENGTH);
  tprintf(3, "iu_testbed_image_header::recognize_header: %s\n", buf);
  if (strncmp(buf,"SRI TB01IMGHDR", 14) != 0) {
    tprintf(3, "        (not IU testbed)\n");
    return(0);
  }
  
  tprintf(3, "        this is an IU testbed file.\n");
  return(1);
}

void print_image_header(iu_testbed_image_header *hdr)
{
  tprintf(2, "iu_testbed_image_header: (%d %d) %d (%d %d)\n",
	 hdr->xdim, hdr->ydim, hdr->element_size, hdr->block_xdim, hdr->block_ydim);
}

#define BITS_PER_WORD 32

int iu_testbed_image_header::read_header (int fd)
{ 
  char buf[TB_BASIC_HDR_LENGTH];
  int block_size_2;
  int signed_p=0;
  int float_p=0;
  if (lseek(fd, 0L, SEEK_SET) == -1L) 
    tprintf(3, "iu: lseek fails...\n");

  int element_size_spec;

  if (fullread(fd,buf,TB_BASIC_HDR_LENGTH) != TB_BASIC_HDR_LENGTH) 
    return(0);
  if (strncmp(buf,"SRI TB01IMGHDR", 14) != 0)
    return(0);

  // Some changes made below to reflect original lisp version.  There
  // were some discrepancies:

  element_size_spec = read_int_field8(&buf[BITPIX]);
  element_size = abs(element_size_spec);

  // Changed <8 to <=8
  endian = (element_size<=8) ? machine_endian() : ( (element_size_spec<0) ? CME_BIG_ENDIAN : CME_LITTLE_ENDIAN );

  tprintf(3, "iu_testbed_image_header: Endian = %d\n", endian);
  float_p = strncmp(&buf[SIGNED], "IEEEREAL", 8) == 0;
  signed_p = strncmp(&buf[SIGNED], "TWOSCOMP", 8) == 0;
  element_type = image_element_type_from_size(element_size, signed_p, float_p);
  xdim = read_int_field8(&buf[XSIZ]);
  ydim = read_int_field8(&buf[YSIZ]);
  tprintf(3, "iu_testbed_image_header: xdim,ydim = %d,%d\n", xdim, ydim);
  block_xdim = (8* read_int_field8(&buf[XBLK]))/element_size;

  // Lisp version just sets padded-block-xdim to block-xdim, but this
  // is probably equivalent
  padded_block_xdim = pad_to_multiple(block_xdim, BITS_PER_WORD/element_size);

  block_ydim = read_int_field8(&buf[YBLK]);
  header_length = read_int_field8(&buf[HDRLEN]);
  blocks_wide = read_int_field8(&buf[NXBLK]);
  blocks_hi = read_int_field8(&buf[NYBLK]);
  block_size = block_xdim * block_ydim;

  // Lisp version has some extra stuff here:
  block_size_2 = read_int_field8(&buf[SCALE]);
  if (block_size_2 > block_size) block_size = block_size_2;

  if (strncmp(&buf[SCANDIR],"RL", 2) == 0) block_xdim = -block_xdim;
  if (strncmp(&buf[SCANDIR+2],"TB", 2) == 0) block_ydim = -block_ydim;
  tprintf(3, "iu_testbed_image_header: block_xdim=%d, padded_block_xdim=%d, block_ydim=%d\n",
	  block_xdim, padded_block_xdim, block_ydim);
  tprintf(3, "iu_testbed_image_header: header_length=%d, blocks_wide=%d, blocks_hi=%d, block_size=%d\n",
	  header_length, blocks_wide, blocks_hi, block_size);
  //print_image_header(this);
  // fprintf(stderr, "iu_testbed_image_header::read_header %s fd=%d, %d, %d\n", path, fd, xdim, ydim);
  return(1);
}  

extern "C" {
int FREEDIUS_GLOBAL(iu_testbed_image_file_header_params) (char *path, int params[10]) 
{
  int fd=read_open(path);
  iu_testbed_image_header hdr;
  hdr.read_header(fd);
  
  params[0] = (int) hdr.element_type;
  params[1] = hdr.xdim;
  params[2] = hdr.ydim;
  params[3] = hdr.block_xdim;
  params[4] = hdr.block_ydim;
  params[5] = hdr.padded_block_xdim;
  close(fd);
  return 1;
}

} // end extern "C"


int iu_testbed_image_header::property_list_file_position ()
{
  int data_elems = block_size * blocks_wide * blocks_hi;
  // FIXME -- could have overflow here -- need 64-bit arithmetic
  return (header_length + (data_elems*element_size)/8); 
} 

char* iu_testbed_image_header::read_property_list_string (char* path)
{
  int fd;
  fd=read_open(path,1);
  int pl_file_pos = property_list_file_position();
  off_t file_length = lseek(fd, 0L, SEEK_END);
  
  if (file_length <=  pl_file_pos+24){
    close(fd);
    return 0; // no room for a property_list_string
  }
  lseek(fd, pl_file_pos, SEEK_SET);
  char hdr_string[24];
  if (fullread(fd, hdr_string, 24) < 0 || strncmp(hdr_string, "SRI TB01PROPDOC ", 16) != 0){
    close(fd);
    return 0;
  }
  int pl_length;
  sscanf(hdr_string+16, "%8d", &pl_length);
  pl_length -= 24;
  if (pl_length&1) pl_length++; // in order to read old image files

  // A large number of iu-testbed images were incorrectly written with pl_length == 24
  // but actually contain property lists.
  if (pl_length == 0) {
    off_t file_end = lseek(fd, 0, SEEK_END);
    pl_length = file_end - (pl_file_pos + 24);
    lseek(fd, pl_file_pos+24, SEEK_SET);
  }

  char *property_list_string = ZALLOC(char, pl_length);
  int k = fullread(fd, property_list_string, pl_length);
  if (k < pl_length) property_list_string[k] = 0;
  close(fd);
  return property_list_string;
}


void iu_testbed_image_header::set_header_slots (blocked_mapped_image *img)
{
  xdim = img->xdim;
  ydim = img->ydim;
  element_size = image_element_size(img);
  block_xdim = img->block_xdim;
  block_ydim = img->block_ydim;
  padded_block_xdim = img->padded_block_xdim;
  blocks_wide = img->blocks_wide();
  blocks_hi = img->blocks_hi();
  block_size = img->block_size();
  header_length = 1024;
}


void iu_write_header_break(){}

void iu_testbed_image_header::write_header (int fd)
{ int header_length = 1024;
  char buf[1024];
  int big_endian = (machine_endian() == CME_BIG_ENDIAN);
  int i;
  while (lseek(fd, 0L, SEEK_SET) == -1L) tprintf(3, "lseek fails.");
  iu_write_header_break();
  sprintf(buf,"%s%8d%s%s%s%s%8d%8d%8d%8d%8d%8d%8d%8d%8d%8d",
		   "SRI TB01IMGHDR  ",
		   header_length,
		   (block_xdim<0)?"RL":"LR",
		   (block_ydim<0)?"TB":"BT",
		   "SCAN",
		   (element_type == IMG_SINGLE_FLOAT ||element_type == IMG_DOUBLE_FLOAT)
		   ?"IEEEREAL":"UNSIGNED",
		   1,
		   element_size,
		   (element_size <= 8) ? element_size:
	                   (big_endian ? -element_size: element_size),
		   xdim,
		   ydim,
		   (abs(block_xdim)*element_size)>>3,
		   abs(block_ydim),
		   ceiling(xdim,abs(block_xdim)),
		   ceiling(ydim,abs(block_ydim)),
		   (abs(block_xdim*block_ydim) == block_size)?0:block_size
		   );
  for (i=strlen(buf); i<header_length; i++) buf[i]=(char) ' ';
  iu_write_header_break();
  write(fd,buf, header_length);
  iu_write_header_break();
  
} 

#define SWAP(x, y) {int tmp; tmp = x; x = y; y = tmp;}

void swap_endian_16 (unsigned short *arr, int nbytes)
{
  int i;
  unsigned char *carr = (unsigned char*) arr;
  if (nbytes%2 !=0) error("swap_endian_16 bad array length %d\n", nbytes);
  for (i = 0; i<nbytes; i+=2)
    SWAP(carr[i], carr[i+1]); 
}

void swap_endian_32 (unsigned int *arr, int nbytes)
{
  int i;
  unsigned char *carr = (unsigned char*) arr;
  //fprintf(stderr, "swap_endian_32 %d\n", nbytes);
  if (nbytes%4 !=0) error("swap_endian_32 bad array length %d\n", nbytes);
  for (i = 0; i<nbytes; i+=4) {
    SWAP(carr[i], carr[i+3]); 
    SWAP(carr[i+1], carr[i+2]); 
  }
}

void swap_endian_single_float (float *arr, int nbytes)
{
  int i;
  unsigned char *carr = (unsigned char*) arr;
  //fprintf(stderr, "swap_endian_single_float %d\n", nbytes);
  if (nbytes%4 !=0) error("swap_endian_single_float bad array length %d\n", nbytes);
  for (i = 0; i<nbytes; i+=4) {
    SWAP(carr[i], carr[i+3]); 
    SWAP(carr[i+1], carr[i+2]); 
  }
}

void swap_endian_double_float (double *darr, int nbytes)
{
  int i;
  unsigned char *carr = (unsigned char*) darr;
  if (nbytes%8 !=0) error("swap_endian_64 bad array length %d\n", nbytes);
  for (i = 0; i<nbytes; i+=8) {
    SWAP(carr[i]  , carr[i+7]); 
    SWAP(carr[i+1], carr[i+6]); 
    SWAP(carr[i+2], carr[i+5]); 
    SWAP(carr[i+3], carr[i+4]); 
  }
}

extern "C" {

  void FREEDIUS_GLOBAL(swap_endian_16) (unsigned short *arr, int nbytes)
  {
    swap_endian_16(arr, nbytes);
  }
  void FREEDIUS_GLOBAL(swap_endian_32) (unsigned int *arr, int nbytes)
  {
    swap_endian_32(arr, nbytes);
  }
  void FREEDIUS_GLOBAL(swap_endian_single_float) (float *arr, int nbytes)
  {
    swap_endian_single_float(arr, nbytes);
  }
  void FREEDIUS_GLOBAL(swap_endian_double_float) (double *arr, int nbytes)
  {
    swap_endian_double_float(arr, nbytes);
  }
  
} // end extern "C"


void swap_image_array_endian (array_image_base *img)
{
  int array_length = img->image_array_length_bytes();
  void * array = img->image_array();
  switch (image_element_type(img)) {
  case IMG_UNSIGNED_16BIT: case IMG_SIGNED_16BIT:
    swap_endian_16((unsigned short *)array, array_length);
    break;
    // What about IMG_RGBA8 and IMG_RGB8 ?
  case IMG_UNSIGNED_32BIT: IMG_SIGNED_32BIT:
    swap_endian_32((unsigned int *)array, array_length);
    break;
  case IMG_SINGLE_FLOAT:
    swap_endian_single_float((float *)array, array_length);
    break;
  case IMG_DOUBLE_FLOAT:
   swap_endian_double_float((double *)array, array_length);
    break;
  }
}


image *iu_testbed_image_header::load_image (int fd, char *path)
{
  array_image_base *img;
  char *dest;
  int n, k;

  tprintf(2, "loading array_image from %s\n", path);
  // Not sure what was going on here.  padded_block_xdim is nonzero
  // for ALV images.  Why was it ignored here??
  //
  // Need to be able to reload an image here.
  if (into_image) img = (array_image_base*) into_image;
  else img = (array_image_base *) 
	 //    make_array_image(xdim, ydim, element_type, block_xdim, block_ydim);
	 make_array_image(xdim, ydim, element_type, 1, block_xdim, block_ydim, padded_block_xdim);

  if (lseek(fd, (off_t) header_length, SEEK_SET) != header_length)
    error("Unable to lseek past header.");

  n = img->image_array_length_bytes();
  k = fullread(fd, (char*) (img->image_array()), n);
  if (k != n) error("iu_testbed_image_header::load_image: got only %d of %d bytes into image array.\n", k, n);
    
  if (endian != machine_endian() && element_size>8) {
    warning("iu_testbed_image_header::load_image swapping endian %s\n", path);
    swap_image_array_endian(img);
  }
  close(fd);
  return((image *)img);
}
  

extern
int iu_testbed_save_image (array_image_base *img, string path, int error_ok = 0)
{
  int fd=write_open(path);
  iu_testbed_image_header *hdr = new(iu_testbed_image_header);
  if (fd < 0) 
    {if (error_ok == 0)
	error("array_image_base::save_image: cannot create file %s", path);
      else return(fd);
    }
  hdr->set_header_slots((blocked_mapped_image *) img);
  hdr->write_header(fd);
  if (lseek(fd, (off_t) (hdr->header_length), SEEK_SET) != hdr->header_length) {
    if (error_ok == 0) error("Cannot lseek to skip the header.");
    else return(-1);
  }
  write(fd, (char*) (img->image_array()), img->image_array_length_bytes());
  close(fd);
  return(0);
}



#if 0

//typedef int (*intfn)();
typedef int (*ireffn)(image *img, int x, int y);

main(int argc, char **argv)
  {int fd, fd2, ok; 
  char *path = "/homedir/quam/pix/rugby.pic";
  array_image<unsigned char> *img;
  iu_testbed_image_header *hdr = new(iu_testbed_image_header);
  
  if (argc>1) path=argv[1];
  /*
  fprintf(stderr, "path=%s\n",path);
  fd=read_open(path);
  hdr->fd = fd;
  ok = hdr->read_header();
  fprintf(stderr, "ok= %d, xdim=%d, ydim=%d\n", ok, hdr->xdim, hdr->ydim);
  */
  img = (array_image<unsigned char>  *) load_image(path);

  fprintf(stderr, "ok= %d, xdim=%d, ydim=%d bps=%d, array = %p\n", 
	 ok, img->xdim, img->ydim, image_element_size(img), img->image_array());
  {int i, x; int x0= 100; int n = 20; int y = 100;  
  int buf[100];
  ireffn img_ireffn = (ireffn) &array_image<unsigned char>::iref;
  fprintf(stderr, "ireffn=%p, ireffn(%d,%d)=%d\n", img_ireffn, x0, y, img_ireffn(img,x0,y));
  fprintf(stderr, "foo(100,100)=%d\n", foo(img,100,100));

  for (x = x0, i=0; i<n; x++, i++)
    {fprintf(stderr, "iref(%d,%d)=%d\n", x, y, img->iref(x, y));
    }
  // getline1(img, buf, x0, y, n);
  //  img->getline(buf, x0, y, n);
  // getline1(img, buf, x0, y, n);
  img->getline(buf, x0, y, n);
  for (i=0; i<n; i++)
    {fprintf(stderr, "buf[%d]=%d\n", x0+i, buf[i]);
    }
  img->putline(buf, x0, y, n);
 
  // iu_testbed_save_image(img, "/homedir/quam/tmp/rugby2.pic");
 img->save_image("/homedir/quam/tmp/rugby2.pic");

  }}

  
#endif




iu_testbed_image_header *iu_testbed_image_header1 = 0;

int init_iu_testbed_array_image_header()
{
  if (! iu_testbed_image_header1) 
    iu_testbed_image_header1 = new iu_testbed_image_header;
  tprintf(2, "init_iu_testbed_file_image_header\n");
  add_image_header(iu_testbed_image_header1);
  return(1);
}

END_NAMESPACE_FREEDIUS
