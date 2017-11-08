#include "array-image.h"
#include "cme-error.h"

//#include <stdlib.h>
#include "image.h"
#include "file-image.h"
#include "image-io.h"
#include "iu-testbed-io.h"
#include "misc.h"

#include <sys/types.h>
#ifndef _WIN32
#include <sys/uio.h>
#include <sys/file.h>
#include <unistd.h>
#endif
#include <stdio.h>
#include <fcntl.h>


#define SAVEJPEG 1

#include "jpeg-io.h"

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

#ifdef CODEWARRIOR2
// This isn't compiling under codewarrior:

// LHQ Mon Apr 14 2003:  This is broken for scalar images.
template <class ELTYPE>
void jpeg_image_header::read_jpeg_rgb_data(image *img, JSAMPARRAY buffer, ELTYPE *obuf)
{
  int y = ydim - 1;
  ELTYPE *rgb_buf = (ELTYPE *) buffer[0];
  IMAGE_ELEMENT_TYPE  element_type;
  switch (samplesperpixel) {
  case 1: element_type = IMG_UNSIGNED_8BIT;   break;
  case 2: element_type = IMG_UNSIGNED_16BIT;  break;
  case 3: element_type = IMG_RGB8;  break; // fprintf(stderr, "Making a RGB8 color image.\n"); break;
  case 4: element_type = IMG_RGBA8;  break; // fprintf(stderr, "Making a RGBA8 color image.\n"); break;
  default: error("jpeg_image_header::load_image: illegal samplesperpixel=%d", samplesperpixel);
  }
  while (cinfo.output_scanline < cinfo.output_height) {
    /* jpeg_read_scanlines expects an array of pointers to scanlines.
     * Here the array is only one element long, but you could ask for
     * more than one scanline at a time if that's more convenient.
     */
    (void) jpeg_read_scanlines(&cinfo, buffer, 1);
    /* Assume put_scanline_someplace wants a pointer and sample count. */
    switch (element_type) {
    case IMG_RGB8: case IMG_RGBA8:
      img->putline(rgb_buf, 0, y); break;
    case IMG_UNSIGNED_8BIT: 
      img->putline_uchar (rgb_buf, 0, y); break;
    case IMG_UNSIGNED_16BIT:   
      img->putline_ushort (rgb_buf, 0, y); break;
    }
    y--;
  }
}


#endif

METHODDEF(void)
my_error_exit (j_common_ptr cinfo)
{
  /* cinfo->err really points to a my_error_mgr struct, so coerce pointer */
  my_error_ptr myerr = (my_error_ptr) cinfo->err;

  /* Always display the message. */
  /* We could postpone this until after returning, if we chose. */
  (*cinfo->err->output_message) (cinfo);

  /* Return control to the setjmp point */
  longjmp(myerr->setjmp_buffer, 1);
}

// BAD MODEL: socketp is a hack that will have to go away, and assumes
// that recognize_header is always applied to

int jpeg_image_header::recognize_header (int fd)
{ 
  unsigned char buf[4];
  while (lseek(fd, 0L, SEEK_SET) == -1L) tprintf(3, "lseek fails.  Trying again.\n");
  fullread(fd, (char*) buf, 4);
  tprintf(3,"Checking JPEG...\n");

  if (buf[0] == 255 && buf[1] == 216 && buf[2] == 255) {
    tprintf(3,"       this is a JPEG file.\n");
    return(1);
  }

  tprintf(3,"       (not JPEG)\n");
  return(0);
}

int jpeg_image_header::read_header (int fd) {
  infile = 0;

  lseek(fd, 0L, SEEK_SET);
  // Some of this is shamelessly stolen from example.c in the libjpeg
  // distribution.

  /* Step 1: allocate and initialize JPEG decompression object */

  /* We set up the normal JPEG error routines, then override error_exit. */
  cinfo.err = jpeg_std_error(&jerr.pub);
  jerr.pub.error_exit = my_error_exit;
  /* Establish the setjmp return context for my_error_exit to use. */
  if (setjmp(jerr.setjmp_buffer)) {
    /* If we get here, the JPEG code has signaled an error.
     * We need to clean up the JPEG object, close the input file, and return.
     */
    jpeg_destroy_decompress(&cinfo);
    // fclose(infile); // Not established yet...
    return 0;
  }
  /* Now we can initialize the JPEG decompression object. */
  jpeg_create_decompress(&cinfo);

  /* Step 2: specify data source (eg, a file) */

  infile = fdopen(fd, "r");

  jpeg_stdio_src(&cinfo, infile);

  /* Step 3: read file parameters with jpeg_read_header() */

  (void) jpeg_read_header(&cinfo, TRUE);
  /* We can ignore the return value from jpeg_read_header since
   *   (a) suspension is not possible with the stdio data source, and
   *   (b) we passed TRUE to reject a tables-only JPEG file as an error.
   * See libjpeg.doc for more info.
   */

  /* Step 4: set parameters for decompression */

  /* In this example, we don't need to change any of the defaults set by
   * jpeg_read_header(), so we do nothing here.
   */

  /* Step 5: Start decompressor */

  (void) jpeg_start_decompress(&cinfo);
  /* We can ignore the return value since suspension is not possible
   * with the stdio data source.
   */

  /* We may need to do some setup of our own at this point before reading
   * the data.  After jpeg_start_decompress() we have the correct scaled
   * output image dimensions available, as well as the output colormap
   * if we asked for color quantization.
   * In this example, we need to make an output work buffer of the right size.
   */ 
  /* JSAMPLEs per row in output buffer */
  row_stride = cinfo.output_width * cinfo.output_components;

  xdim = cinfo.output_width;
  ydim = cinfo.output_height;
  samplesperpixel = cinfo.output_components;
  // fprintf(stderr, "xdim = %d, ydim = %d, samples per pixel = %d\n", xdim, ydim, samplesperpixel);
  return 1;
}


image *jpeg_image_header::load_image (int fd, char *path) {
  int i, j, k, nbits;
  image *img;
  JSAMPARRAY buffer;		/* Output row buffer */
  IMAGE_ELEMENT_TYPE  element_type;
  //  fprintf(stderr,"jpeg_image_header::load_image samplesperpixel = %d\n", samplesperpixel);
  switch (samplesperpixel) {
  case 1: element_type = IMG_UNSIGNED_8BIT;   break;
  case 2: element_type = IMG_UNSIGNED_16BIT;  break;
  case 3: element_type = IMG_RGB8;  break; // fprintf(stderr, "Making a RGB8 color image.\n"); break;
  case 4: element_type = IMG_RGBA8;  break; // fprintf(stderr, "Making a RGBA8 color image.\n"); break;
  default: error("jpeg_image_header::load_image: illegal samplesperpixel=%d", samplesperpixel);
  }

  if (into_image) {
    img = into_image;
    //    fprintf(stderr, "Reloading image.\n");
  } else {
    // LHQ:  block_y_dim needs to be negative;  display code buggy otherwise.
    //img = make_array_image(xdim, ydim, element_type, 1, xdim, -ydim, 0, 0);
    img = make_image(xdim, ydim, element_type);
    // printf("Made %d by %d image %x.\n", xdim, ydim, (unsigned int) img);
  }

  /* Make a one-row-high sample array that will go away when done with image */ 
  buffer = (*cinfo.mem->alloc_sarray)
		((j_common_ptr) &cinfo, JPOOL_IMAGE, row_stride, 1);

  /* Step 6: while (scan lines remain to be read) */
  /*           jpeg_read_scanlines(...); */


  switch (element_type) {
  case IMG_RGB8: read_jpeg_rgb_data(img, buffer, (RGB8_PIXEL *) 0); break;
  case IMG_RGBA8: read_jpeg_rgb_data(img, buffer, (RGBA8_PIXEL *) 0); break;
  case IMG_UNSIGNED_8BIT: read_jpeg_rgb_data(img, buffer, (unsigned char *) 0); break;
  case IMG_UNSIGNED_32BIT: read_jpeg_rgb_data(img, buffer, (unsigned int *) 0); break;
  default: error("jpeg_image_header::load_image: illegal image_element_type=%s",
		     image_element_type_name(element_type));
  } // end switch

  /* Step 7: Finish decompression */

  (void) jpeg_finish_decompress(&cinfo);
  /* We can ignore the return value since suspension is not possible
   * with the stdio data source.
   */

  /* Step 8: Release JPEG decompression object */

  /* This is an important step since it will release a good deal of memory. */
  jpeg_destroy_decompress(&cinfo);

  /* After finish_decompress, we can close the input file.
   * Here we postpone it until after no more JPEG errors are possible,
   * so as to simplify the setjmp error logic above.  (Actually, I don't
   * think that jpeg_destroy can do an error exit, but why assume anything...)
   */
  fclose(infile);
  
  /* At this point you may want to check to see whether any corrupt-data
   * warnings occurred (test whether jerr.pub.num_warnings is nonzero).
   */


  /* And we're done! */
  return(img);
}


jpeg_image_header *jpeg_image_header1 = 0;

extern int init_jpeg_image_header()
{
  if (! jpeg_image_header1) jpeg_image_header1 = new jpeg_image_header;
  tprintf(2,"init_jpeg_image_header\n");
  add_image_header(jpeg_image_header1);
  return(1);
}

#ifdef SAVEJPEG

extern int
save_jpeg_image(image *img, char *path, int quality)
{
  IMAGE_ELEMENT_TYPE element_type;
  struct jpeg_error_mgr jerr;
  jmp_buf setjmp_buffer;	/* for return to caller */
  struct jpeg_compress_struct cinfo;
  JSAMPROW buffer[1];	/* pointer to JSAMPLE row[s] */
  //  JSAMPARRAY buffer;
  RGB8_PIXEL *rgbbuf;
  int *ibuf;
  char *cbuf;
  int i;
  int comp;

  element_type = img->element_type();

  /* For RGB we set components to 3, and use JCS_RGB.  No support for
   * YUV here.  Only 8-bit RGB or monochrome for jpeg.
   */

  // Not sure why we have to do this twice:
  switch (element_type) {
  case IMG_SIGNED_8BIT:
  case IMG_UNSIGNED_8BIT:  comp = 1;  cinfo.in_color_space = JCS_GRAYSCALE; break;

  case IMG_RGB8:           comp = 3;  cinfo.in_color_space = JCS_RGB;       break;

  default: error("save_jpeg_image: illegal element type=%d", img->element_type());
    return -1;
  }

  FILE *f = fopen(path, "wb");

  cinfo.err = jpeg_std_error(&jerr);
  jerr.error_exit = my_error_exit;
  jpeg_create_compress(&cinfo);

  cinfo.num_components = cinfo.input_components = comp;
  //  fprintf(stderr, "save_jpeg_image: %d components.\n", comp); fflush(stderr);

  int row_stride = img->xdim * comp;

  //  fprintf(stderr, "save_jpeg_image: row_stride = %d.\n", row_stride); fflush(stderr);

  /* Specify data destination for compression */
  /* There is code for compressing into memory buffers, but we won't do that here: */
  //  jpeg_mem_dest(&cinfo, jbuf, MAXJBUFLEN);

  jpeg_stdio_dest(&cinfo, f);
  //  fprintf(stderr, "save_jpeg_image: stdio_dest = %lx\n", f); fflush(stderr);

  jpeg_set_defaults(&cinfo);
  //  fprintf(stderr, "save_jpeg_image: set defaults\n"); fflush(stderr);
  
  jpeg_set_quality (&cinfo,  quality, TRUE);
  //  fprintf(stderr, "save_jpeg_image: quality = %d\n", quality); fflush(stderr);

  cinfo.image_width = img->xdim;
  cinfo.image_height = img->ydim;
  cinfo.dct_method = JDCT_ISLOW;

  switch (element_type) {
  case IMG_SIGNED_8BIT:
  case IMG_UNSIGNED_8BIT:  comp = 1;  cinfo.in_color_space = JCS_GRAYSCALE; break;

  case IMG_RGB8:           comp = 3;  cinfo.in_color_space = JCS_RGB;       break;

  default: error("save_jpeg_image: illegal element type=%d", img->element_type());
    return -1;
  }
  /* Now that we know input colorspace, fix colorspace-dependent defaults */
  jpeg_default_colorspace(&cinfo);

  /* Start compressor */
  jpeg_start_compress(&cinfo, TRUE);

  /* Make a one-row-high sample array that will go away when done with image */ 
 
  if (element_type == IMG_RGB8) {
    rgbbuf = (RGB8_PIXEL*) XALLOC(RGB8_PIXEL, img->xdim);
    buffer[0] = (JSAMPROW) rgbbuf;
  } else {
    ibuf = XALLOC(int, img->xdim);
    cbuf = XALLOC(char, img->xdim);
    buffer[0] = (JSAMPROW) cbuf;
  }

  /* Process data */
  while (cinfo.next_scanline < cinfo.image_height) {
    // We may have to flip this buffer before writing:
    if (element_type == IMG_RGB8) img->getline(rgbbuf, 0, cinfo.next_scanline);
    else {
      img->getline(ibuf, 0, ((img->ydim-1) - cinfo.next_scanline));
      for (int k = 0; k < img->xdim; k++) cbuf[k] = ibuf[k];
    }

    (void) jpeg_write_scanlines(&cinfo, buffer, 1);

  }

#if 0  
  if (cinfo.in_color_space == JCS_GRAYSCALE) fprintf(stderr, "Grayscale output complete.\n");
  else if (cinfo.in_color_space == JCS_RGB) fprintf(stderr, "RGB output complete.\n");
  else  fprintf(stderr, "Some kind of YUV output complete.\n");
#endif

  /* Finish compression and release memory */
  //  (*src_mgr->finish_input) (&cinfo, src_mgr);
  jpeg_finish_compress(&cinfo);
  /* Close files, if we opened them */
  if (f != stdout)  fclose(f);

  jpeg_destroy_compress(&cinfo);
  //  fprintf(stderr, "save_jpeg_image: Done!\n"); fflush(stderr);

  if (element_type == IMG_RGB8) xxfree(rgbbuf);
  else {
    xxfree(cbuf);
    xxfree(ibuf);
  }
  return 0;			/* suppress no-return-value warnings */
}
extern "C" {


int
FREEDIUS_GLOBAL(save_jpeg_image) (cimage *img, char *path, int quality)
{
  return save_jpeg_image((array_image_base *) img, path, quality);
}

}
#endif


END_NAMESPACE_FREEDIUS
