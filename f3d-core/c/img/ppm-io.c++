//
// PPM I/O FFI for Freedius images... - C. Connolly 6/12/2002
//
// I'd recommend shifting this C code into core FREEDIUS to "unmix"
// this system.  It's a popular image format and there's no reason to
// keep it separate.
//
extern "C" {
#include <sys/types.h>
#ifndef _MSC_VER
#include <unistd.h>
#endif
#include <stdio.h>
#include <ctype.h>
}


#include "image.h"
#include "array-image.h"
#include "color.h"

// Is this next available on all targets??

#include "base-class.h"
#include "image-io.h"
#include "ppm-io.h"

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

int ppm_image_header::recognize_header (int fd) {
  char buf[3];
  lseek(fd, 0L, SEEK_SET);
  fullread(fd, buf, 3);
  // printf("ppm recognize_header: %c %c\n", buf[0], buf[1]);
  //  printf("Next char: %d...isspace returns %d.\n", buf[2], isspace(buf[2]));
  return (buf[0] == 'P' && buf[1] == '6' && isspace(buf[2]));
}




int ppm_image_header::read_header (int fd) {
  int ch;
  int maxval;
  FILE *f = fopen(path, "r");

  fscanf(f, "P6");
  tprintf(4, (char *) "PPM file: P6 header recognized.\n");
  
  // Is this enough?
  while ((ch=fgetc(f)) && isspace(ch)) continue;
  if (ch == '#') while ((ch=fgetc(f)) != '\n') continue;
  if (isdigit(ch)) ungetc(ch, f);

  if (fscanf(f, "%d", &xdim) < 1) {
    fclose(f);
    return 0;
  }

  while ((ch=fgetc(f)) && isspace(ch)) continue;
  if (ch == '#') while ((ch=fgetc(f)) != '\n') continue;
  if (isdigit(ch)) ungetc(ch, f);

  if (fscanf(f, "%d", &ydim) < 1) {
    fclose(f);
    return 0;
  }

  while ((ch=fgetc(f)) && isspace(ch)) continue;
  if (ch == '#') while ((ch=fgetc(f)) != '\n') continue;
  if (isdigit(ch)) ungetc(ch, f);

  if (fscanf(f, "%d ", &maxval) < 1) {
    fclose(f);
    return 0;
  }
  
  tprintf(4, "PPM file: %d by %d, %d colors.\n", xdim, ydim, maxval);
  if (maxval > 255) {
    printf("Only 8-bit color channels (maxval < 255) can be written at this time.\n");
    return 0;
  }

  element_size = 24;
  element_type = IMG_RGB8;
  tprintf(4, "PPM file: element_size = %d.\n", element_size);
  header_length = ftell(f);

  fclose(f);

  return 1;
}



image *ppm_image_header::load_image (int fd, char *path) { 
  array_image_base *img;
  int n, k;

  if (into_image) img = (array_image_base*) into_image;
  else img = (array_image_base*)  make_image(xdim, ydim, element_type);

  if (lseek(fd, (off_t) header_length, SEEK_SET) != header_length)
    error("Unable to lseek past header.");

  // Is this architecture-independent??
  n = img->image_array_length_bytes();
  k = fullread(fd, (char*) (img->image_array()), n);
  if (k != n) error("ppm_image_header::load_image: got only %d of %d bytes into image array.\n", k, n);

  close(fd);
  return((image*) img);
}



extern "C" {

int save_ppm_image_RGB8 (image *im, char *file) {
  int i, j, k;
  RGB8_PIXEL *buf;
  FILE *f;

  if (!color_p(im)) {
    printf("You must supply a color image to save_ppm_image_RGB8.\n");
    return 0;
  } else {
    f = fopen(file, "w");

    buf = (RGB8_PIXEL*) galloc(im->xdim * sizeof(RGB8_PIXEL));

    fprintf(f, "P6\n# Written by SRI Freedius v1.0\n%d\n%d\n%d\n", im->xdim, im->ydim, 255);

    // Is this right?  Bottom up?
    for (i = im->ydim - 1; i >= 0; i--) {
      im->getline(buf, 0, i);
      fwrite(buf, sizeof(RGB8_PIXEL), im->xdim, f);
    }
    xxfree(buf);
    fclose(f);
    return 1;
  }
}

} // End extern "C"


/*
int ppm_image_header::save_image (char *path) {
  if (color_p(into_image)) save_ppm_image_RGB8(into_image, path);
}
*/


int pgm_image_header::recognize_header (int fd) {
  char buf[3];
  lseek(fd, 0L, SEEK_SET);
  fullread(fd, buf, 3);
  // printf("pgm recognize_header: %c %c\n", buf[0], buf[1]);
  //  printf("Next char: %d...isspace returns %d.\n", buf[2], isspace(buf[2]));
  return (buf[0] == 'P' && buf[1] == '5' && isspace(buf[2]));
}




int pgm_image_header::read_header (int fd) {
  int ch;
  int maxval;
  FILE *f = fopen(path, "r");

  fscanf(f, "P5");
  tprintf(4, "PGM file: P5 header recognized.\n");
  
  // Is this enough?
  while ((ch=fgetc(f)) && isspace(ch)) continue;
  if (ch == '#') while ((ch=fgetc(f)) != '\n') continue;
  if (isdigit(ch)) ungetc(ch, f);

  if (fscanf(f, "%d", &xdim) < 1) {
    fclose(f);
    return 0;
  }

  while ((ch=fgetc(f)) && isspace(ch)) continue;
  if (ch == '#') while ((ch=fgetc(f)) != '\n') continue;
  if (isdigit(ch)) ungetc(ch, f);

  if (fscanf(f, "%d", &ydim) < 1) {
    fclose(f);
    return 0;
  }

  while ((ch=fgetc(f)) && isspace(ch)) continue;
  if (ch == '#') while ((ch=fgetc(f)) != '\n') continue;
  if (isdigit(ch)) ungetc(ch, f);

  if (fscanf(f, "%d ", &maxval) < 1) {
    fclose(f);
    return 0;
  }
  
  tprintf(4, "PGM file: %d by %d, %d colors.\n", xdim, ydim, maxval);

  if (maxval < 256) {
    element_size = 8;
    element_type = IMG_UNSIGNED_8BIT;
  } else if (maxval < 65536) {
    element_size = 16;
    element_type = IMG_UNSIGNED_16BIT;
  } else { // not strictly kosher...
    element_size = 32;
    element_type = IMG_UNSIGNED_32BIT;
  }

  tprintf(4, "PGM file: element_size = %d.\n", element_size);
  header_length = ftell(f);

  fclose(f);

  return 1;
}



image *pgm_image_header::load_image (int fd, char *path) { 
  array_image_base *img;
  int n, k;

  if (into_image) img = (array_image_base*) into_image;
  else img = (array_image_base*)  make_image(xdim, ydim, element_type);

  if (lseek(fd, (off_t) header_length, SEEK_SET) != header_length)
    error("Unable to lseek past header.");

  // Is this architecture-independent??
  n = img->image_array_length_bytes();
  k = fullread(fd, (char*) (img->image_array()), n);
  if (k != n) error("pgm_image_header::load_image: got only %d of %d bytes into image array.\n", k, n);

  close(fd);
  return((image*) img);
}



extern "C" {

int save_pgm_image (image *im, char *file) {
  int i, j, k;
  int *buf;
  unsigned char *cbuf;
  unsigned short *sbuf;
  FILE *f;

  if (color_p(im)) {
    printf("You cannot supply a color image to save_pgm_image.\n");
    return 0;
  } else {
    f = fopen(file, "w");

    int *buf = (int*) galloc(im->xdim * sizeof(int));

    fprintf(f, "P5\n# Written by SRI Freedius v1.0\n%d\n%d\n%d\n", im->xdim, im->ydim, (1 << image_element_size(im)) - 1 );
    if (image_element_size(im) == 8) cbuf = (unsigned char*) galloc(im->xdim * sizeof(char));
    else if (image_element_size(im) == 16) sbuf = (unsigned short*) galloc(im->xdim * sizeof(short));
    else {
      printf("Unsupported element size %d.\n", image_element_size(im));
      xxfree(buf);
      return 0;
    }

    for (i = im->ydim-1; i >= 0; i--) {
      im->getline(buf, 0, i);
      if (image_element_size(im) == 8) {
	for (j = 0; j < im->xdim; j++) cbuf[j] = buf[j];
	fwrite(cbuf, sizeof(char), im->xdim, f);
      } else if (image_element_size(im) == 16) {
	for (j = 0; j < im->xdim; j++) sbuf[j] = buf[j];
	fwrite(sbuf, sizeof(short), im->xdim, f);
      } else {
	fwrite(buf, sizeof(int), im->xdim, f);
      }
    }
    xxfree(buf);
    if (image_element_size(im) == 8) xxfree(cbuf);
    else if (image_element_size(im) == 16) xxfree(sbuf);
    fclose(f);
    return 1;
  }
}

} // End extern "C"




pgm_image_header *pgm_image_header1 = 0;
ppm_image_header *ppm_image_header1 = 0;

int init_ppm_image_header()
{
  if (! ppm_image_header1) ppm_image_header1 = new ppm_image_header;
  if (! pgm_image_header1) pgm_image_header1 = new pgm_image_header;
  add_image_header(ppm_image_header1);
  add_image_header(pgm_image_header1);
  return(1);
}

extern "C" {
  int ppm_io_init () { return init_ppm_image_header(); }
}

END_NAMESPACE_FREEDIUS

