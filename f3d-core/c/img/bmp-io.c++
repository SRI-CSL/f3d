
#include <sys/types.h>
#ifndef _WIN32
#include <sys/uio.h>
#include <sys/file.h>
#include <unistd.h>
#endif
#include <stdio.h>
#include <fcntl.h>

#include "image.h"
#include "file-image.h"
#include "image-io.h"
#include "iu-testbed-io.h"
#include "misc.h"
#include "array-image.h"
#include "cme-error.h"

#include "bmp-io.h"

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

#define BMP_IMAGE_HEADER_MAGIC_NUMBER 19778

static void swap_endian_16 (short *arr, int n=2)
{
  int i;
  unsigned char *carr = (unsigned char*) arr;
  if (n%2 !=0) error("swap_endian_16 bad array length %d\n", n);
  for (i = 0; i<n; i+=2)
    *arr++ = carr[i] | (carr[i+1]<<8);
}

static void swap_endian_32 (int *arr, int n=4)
{
  int i;
  unsigned char *carr = (unsigned char*) arr;
  if (n%4 !=0) error("swap_endian_32 bad array length %d\n", n);
  for (i = 0; i<n; i+=4)
    *arr++ = (carr[i]) | (carr[i+1]<<8) | (carr[i+2]<<16) | (carr[i+3]<<24);
}

int bmp_image_header::recognize_header (int fd)
{ 
  char buf[4];
  lseek(fd, 0L, SEEK_SET);
  read(fd,buf,2);
  tprintf(3,"Checking BMP (file bytes = %c%c)\n", buf[0], buf[1]);
  if (buf[0] == 'B' && buf[1] == 'M') {
    tprintf(3,"       this is a BMP file.\n");
    return(1);
  }

  tprintf(3, "        (not BMP)\n");
  return(0);
}

#define BITS_PER_WORD 32
#define WIN_OS2_OLD 12
#define WIN_NEW     40
#define OS2_NEW     64

void check_cmap (RGB8_PIXEL *cmap, int ncolors)
{
  for (int i=0; i<ncolors; i++) 
     if ((cmap[i].r != cmap[i].g) || (cmap[i].r != cmap[i].b))
	fprintf (stderr, "bmp-io cmap[%d] = %d %d %d\n", i, cmap[i].r, cmap[i].g, cmap[i].b);
}

#include <sys/types.h>

int bmp_image_header::read_header (int fd)
{
  int size, reserved, infosize;
  short depth;
  int comp, imsize, xppm, yppm, ncolors, nimp;

  cmap = 0;

  read(fd, (char*)&size, 4);
  read(fd, (char*)&reserved, 4);
  read(fd, (char*)&offset, 4);

  read(fd, (char*)&infosize, 4);

  if (machine_endian() == CME_BIG_ENDIAN) {
    swap_endian_32(&size);
    swap_endian_32(&offset);
    swap_endian_32(&infosize);
  }
  if (infosize != WIN_NEW && infosize != OS2_NEW) {
    printf("bmp_image_header::read_header -- old BMP format not supported.\n");
    return 0;
  } 

  read(fd, (char*)&xdim, 4);
  read(fd, (char*)&ydim, 4);
  read(fd, (char*)&depth, 2);
  read(fd, (char*)&bpp, 2);
  read(fd, (char*)&comp, 4);
  read(fd, (char*)&imsize, 4);	/* ignore */
  read(fd, (char*)&xppm, 4);	/* ignore */
  read(fd, (char*)&yppm, 4);	/* ignore */
  read(fd, (char*)&ncolors, 4);
  read(fd, (char*)&color_important, 4);

  if (machine_endian() == CME_BIG_ENDIAN) {
    swap_endian_32(&xdim);
    swap_endian_32(&ydim);
    swap_endian_16(&depth);
    swap_endian_16(&bpp);
    swap_endian_32(&comp);
    swap_endian_32(&imsize);
    swap_endian_32(&xppm);
    swap_endian_32(&yppm);
    swap_endian_32(&ncolors);
    swap_endian_32(&color_important);
  }

  tprintf(4,"BMP file: size=%d, xdim=%d, ydim=%d, depth=%d, bpp=%d, color important=%d\n",
	  size, xdim, ydim, depth, bpp, color_important);

  /* If comp==0, no compression. */
  if (comp != 0) {
    error("bmp_image_header::read_header - compression not implemented.");
    return(0);
  }
  lseek(fd, (off_t) infosize+14, SEEK_SET);

  /* Then we must infer colormap size from bpp (bits per pixel) */
  if (ncolors == 0) ncolors = 1 << bpp;
  tprintf(4,"BMP file: ncolors = %d...reading colormap...\n", ncolors);
  if (bpp < 9) {
    // FIXME Must consider machine_endian  -- 
    //struct ARGB {char b; char g; char r; char a; };
    struct ARGB {char r; char g; char b; char a; };
    //struct ARGB cmap0[ncolors];
    struct ARGB *cmap0 = XALLOC(struct ARGB, ncolors);
    cmap = XALLOC(RGB8_PIXEL, ncolors);
    read(fd, (char*) cmap0, ncolors*sizeof(struct ARGB));
    tprintf(4,"BMP file: ncolors = %d...reading colormap...\n", ncolors);
    for (int i=0; i<ncolors; i++) {
      cmap[i].r = cmap0[i].r;
      cmap[i].g = cmap0[i].g;
      cmap[i].b = cmap0[i].b;
    }
    xxfree(cmap0);
    //check_cmap(cmap, ncolors);
  }

  /* Always coerce to an RGBA-format color image. */
  //  element_size = 32;
  //element_type = image_element_type_from_size(element_size, 0, 0);
  /*
  if (color_important) element_type = IMG_RGB8;
  else if (bpp == 8) element_type = IMG_UNSIGNED_8BIT;
  else if (bpp == 16) element_type = IMG_UNSIGNED_16BIT;
  else if (bpp == 32) element_type = IMG_UNSIGNED_32BIT;
  */
  element_type = IMG_RGB8;
  int k = lseek(fd, (off_t) offset, SEEK_SET);
  tprintf(4,"BMP file: lseek to offset %d returns %d\n", ncolors, k);

  return(1);
}


#define NOUNROLL 1
#ifdef NOUNROLL
template <class fromtype, class totype>
void array_copy(fromtype from, totype to, int n)
{
  int i;
  for (i=0; i<n; i++) to[i]=from[i];
}
#else
template <class fromtype, class totype>
void array_copy(fromtype *fp, totype *tp, int i)
{
  if (i>=8) {
    do {
      *(tp  ) = (totype) *(fp);
      *(tp+1) = (totype) *(fp+1);
      *(tp+2) = (totype) *(fp+2);
      *(tp+3) = (totype) *(fp+3);
      *(tp+4) = (totype) *(fp+4);
      *(tp+5) = (totype) *(fp+5);
      *(tp+6) = (totype) *(fp+6);
      *(tp+7) = (totype) *(fp+7);
      i = i-8; tp +=8; fp +=8;}
    while (i >= 8);}
  if (i > 0)
    {do {*tp++ = (totype) *fp++; i--;}
     while (i > 0);}
}
#endif



extern int max_array_image_size;

image *bmp_image_header::load_image (int fd, char *path)
{ 
  int i, ii, j, k;
  int tiledp, size_in_bytes, large_p, truelen, esize;
  image* img;
  if (into_image) img = into_image;
  else img  = make_image(xdim, ydim, element_type);

  // Be careful to avoid overflows for large images.
  // 32 bit int may still be too small.
  // Right now, element size is always 24 (RGB8).
  esize = image_element_size_from_type(element_type);
  size_in_bytes = (xdim * ydim * 3);

  large_p = size_in_bytes > max_array_image_size;

  tprintf(2, "bmp_image_header::load_image %s  largep=%d size=%d max_array_image_size=%d\n",
	  path, large_p, size_in_bytes, max_array_image_size);
  tprintf(2, "xdim=%d, ydim=%d, bps=%d\n", xdim, ydim, bpp);
  
  lseek(fd, (off_t) offset, SEEK_SET);
  if (bpp == 8) {
    truelen = pad_to_multiple(xdim, 4); // bmp scanlines are padded to a multiple of 4 bytes
    uchar *buf=XALLOC(uchar, truelen);
    //char buf[truelen];
    RGB8_PIXEL *ibuf=XALLOC(RGB8_PIXEL, xdim);
    //fprintf (stderr, "BMP file: loading image colormap = %p\n", cmap);
    //check_cmap(cmap, 1 << bpp);
    for (j = 0; j < ydim; j++) {
      read(fd, buf, truelen);
      if (cmap) 
#if 1
	for (i = 0; i < xdim; i++) ibuf[i] = cmap[buf[i]];
#else 
	for (i = 0; i < xdim; i++) {
	  int c; c = buf[i];
	  ibuf[i].r = cmap[c].r;
	  ibuf[i].g = cmap[c].g;
	  ibuf[i].b = cmap[c].b;
	}
#endif
      else {
	for (i = 0; i < xdim; i++) {
	  // this could be sped up with a table
	  int c = buf[i];
	  ibuf[i].r = c;
	  ibuf[i].g = c;
	  ibuf[i].b = c;
	}
      }
      img->putline(ibuf, 0, j);
    }
    //check_cmap(cmap, 1 << bpp);
    xxfree(buf);
    xxfree(ibuf);
  } else if (bpp == 24) {
    truelen = pad_to_multiple(sizeof(RGB8_PIXEL)*xdim, 4); // bmp scanlines are padded to a multiple of 4 bytes

    RGB8_PIXEL *buf=(RGB8_PIXEL *) galloc(truelen);
    //RGB8_PIXEL buf[xdim];

    for (j = 0; j < ydim; j++) {
      read(fd, (char*) buf, truelen);
      flip_rgb8_endian(buf, xdim);
      img->putline(buf, 0, j);
    }
    xxfree(buf);
  } else {
    error("bpp is not 8 or 24.  Not implemented yet.");
    close(fd);
    return(0);
  }
  close(fd);

  return img;
}
 

void write_bmp_image_header(char *path, int xdim, int ydim, int element_size, int bx, int by,
			int compression_mode)
{
  printf("write_bmp_image_header: not implemented.\n");
}


bmp_image_header *bmp_image_header1 = 0;

int init_bmp_array_image_header()
{
  if (! bmp_image_header1) bmp_image_header1 = new bmp_image_header;
  tprintf(2,"Adding BMP format to image header list.\n");
  add_image_header(bmp_image_header1);
  return(1);
}

END_NAMESPACE_FREEDIUS
