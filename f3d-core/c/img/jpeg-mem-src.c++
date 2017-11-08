/*
 * 
 *
 * Adapted from jdatasrc.c ((C) 1994-1996, Thomas G. Lane.)
 * by Chris Connolly, SRI International.
 *
 * This is a memory data source manager.  Given a buffer containing a
 * jpeg-compressed image, uncompress it.
 *
 */

/* this is not a core library module, so it doesn't define JPEG_INTERNALS */
#include "array-image.h"
#include "cme-error.h"

//#include <stdlib.h>
#include <setjmp.h>
#include "image.h"
extern "C" {
#include "jinclude.h"
#include "jpeglib.h"
#include "jerror.h"
}
#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

/* Expanded data source object for mem input */

typedef struct {
  struct jpeg_source_mgr pub;	/* public fields */

  unsigned char *inbuf;         /* Source buffer */
  int nbytes;
  JOCTET * buffer;		/* start of buffer */
  boolean start_of_file;	/* have we gotten any data yet? */
} my_source_mgr;

typedef my_source_mgr * my_src_ptr;

#define INPUT_BUF_SIZE  1024	/* choose an efficiently fread'able size */


/*
 * Initialize source --- called by jpeg_read_header
 * before any data is actually read.
 */

METHODDEF(void)
init_source (j_decompress_ptr cinfo)
{
  my_src_ptr src = (my_src_ptr) cinfo->src;

  /* We reset the empty-input-file flag for each image,
   * but we don't clear the input buffer.
   * This is correct behavior for reading a series of images from one source.
   */
  src->start_of_file = TRUE;
}


/*
 * Fill the input buffer --- called whenever buffer is emptied.
 *
 * In typical applications, this should read fresh data into the buffer
 * (ignoring the current state of next_input_byte & bytes_in_buffer),
 * reset the pointer & count to the start of the buffer, and return TRUE
 * indicating that the buffer has been reloaded.  It is not necessary to
 * fill the buffer entirely, only to obtain at least one more byte.
 *
 * There is no such thing as an EOF return.  If the end of the file has been
 * reached, the routine has a choice of ERREXIT() or inserting fake data into
 * the buffer.  In most cases, generating a warning message and inserting a
 * fake EOI marker is the best course of action --- this will allow the
 * decompressor to output however much of the image is there.  However,
 * the resulting error message is misleading if the real problem is an empty
 * input file, so we handle that case specially.
 *
 * In applications that need to be able to suspend compression due to input
 * not being available yet, a FALSE return indicates that no more data can be
 * obtained right now, but more may be forthcoming later.  In this situation,
 * the decompressor will return to its caller (with an indication of the
 * number of scanlines it has read, if any).  The application should resume
 * decompression after it has loaded more data into the input buffer.  Note
 * that there are substantial restrictions on the use of suspension --- see
 * the documentation.
 *
 * When suspending, the decompressor will back up to a convenient restart point
 * (typically the start of the current MCU). next_input_byte & bytes_in_buffer
 * indicate where the restart point will be if the current call returns FALSE.
 * Data beyond this point must be rescanned after resumption, so move it to
 * the front of the buffer rather than discarding it.
 */

METHODDEF(boolean)
fill_input_buffer (j_decompress_ptr cinfo)
{
  // Here, we're just swapping pointers around.  We've been given a
  // pointer to jpeg-compressed data in memory (deposited in inbuf),
  // so just use that.
  int i;
  my_src_ptr src = (my_src_ptr) cinfo->src;
  size_t nbytes;

  src->buffer = (JOCTET*) src->inbuf;

  nbytes = src->nbytes;

  if (nbytes <= 0) {
    if (src->start_of_file)	/* Treat empty input file as fatal error */
      ERREXIT(cinfo, JERR_INPUT_EMPTY);
    WARNMS(cinfo, JWRN_JPEG_EOF);
    /* Insert a fake EOI marker */
    src->buffer[0] = (JOCTET) 0xFF;
    src->buffer[1] = (JOCTET) JPEG_EOI;
    nbytes = 2;
  }

  if (src->start_of_file) {
    for (i = 0; i < nbytes && !src->buffer[i]; i++) continue;
  }

  src->pub.next_input_byte = src->buffer+i;
  src->pub.bytes_in_buffer = nbytes-i;
  src->start_of_file = FALSE;

  return TRUE;
}


/*
 * Skip data --- used to skip over a potentially large amount of
 * uninteresting data (such as an APPn marker).
 *
 * Writers of suspendable-input applications must note that skip_input_data
 * is not granted the right to give a suspension return.  If the skip extends
 * beyond the data currently in the buffer, the buffer can be marked empty so
 * that the next read will cause a fill_input_buffer call that can suspend.
 * Arranging for additional bytes to be discarded before reloading the input
 * buffer is the application writer's problem.
 */

METHODDEF(void)
skip_input_data (j_decompress_ptr cinfo, long num_bytes)
{
  my_src_ptr src = (my_src_ptr) cinfo->src;

  /* Just a dumb implementation for now.  Could use fseek() except
   * it doesn't work on pipes.  Not clear that being smart is worth
   * any trouble anyway --- large skips are infrequent.
   */
  if (num_bytes > 0) {
    while (num_bytes > (long) src->pub.bytes_in_buffer) {
      num_bytes -= (long) src->pub.bytes_in_buffer;
      (void) fill_input_buffer(cinfo);
      /* note we assume that fill_input_buffer will never return FALSE,
       * so suspension need not be handled.
       */
    }
    src->pub.next_input_byte += (size_t) num_bytes;
    src->pub.bytes_in_buffer -= (size_t) num_bytes;
  }
}


/*
 * An additional method that can be provided by data source modules is the
 * resync_to_restart method for error recovery in the presence of RST markers.
 * For the moment, this source module just uses the default resync method
 * provided by the JPEG library.  That method assumes that no backtracking
 * is possible.
 */


/*
 * Terminate source --- called by jpeg_finish_decompress
 * after all data has been read.  Often a no-op.
 *
 * NB: *not* called by jpeg_abort or jpeg_destroy; surrounding
 * application must deal with any cleanup that should happen even
 * for error exit.
 */

METHODDEF(void)
term_source (j_decompress_ptr cinfo)
{
  /* no work necessary here */
}


/*
 * Prepare for input from a filedes or socket.
 * The caller must have already opened the mem, and is responsible
 * for closing it after finishing decompression.
 */

GLOBAL(void)
jpeg_mem_src (j_decompress_ptr cinfo, unsigned char *data, int size)
{
  my_src_ptr src;

  /* The source object and input buffer are made permanent so that a series
   * of JPEG images can be read from the same file by calling jpeg_mem_src
   * only before the first one.  (If we discarded the buffer at the end of
   * one image, we'd likely lose the start of the next one.)
   * This makes it unsafe to use this manager and a different source
   * manager serially with the same JPEG object.  Caveat programmer.
   */
  if (cinfo->src == NULL) {	/* first time for this JPEG object? */
    cinfo->src = (struct jpeg_source_mgr *)
      (*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_PERMANENT,
				  SIZEOF(my_source_mgr));
    src = (my_src_ptr) cinfo->src;
    src->buffer = (JOCTET *)
      (*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_PERMANENT,
				  INPUT_BUF_SIZE * SIZEOF(JOCTET));
  }

  src = (my_src_ptr) cinfo->src;
  src->pub.init_source = init_source;
  src->pub.fill_input_buffer = fill_input_buffer;
  src->pub.skip_input_data = skip_input_data;
  src->pub.resync_to_restart = jpeg_resync_to_restart; /* use default method */
  src->pub.term_source = term_source;
  src->inbuf = data;
  src->nbytes = size;
  src->pub.bytes_in_buffer = 0; /* forces fill_input_buffer on first read */
  src->pub.next_input_byte = NULL; /* until buffer loaded */
}


struct my_error_mgr {
  struct jpeg_error_mgr pub;    /* "public" fields */

  jmp_buf setjmp_buffer;        /* for return to caller */
};

typedef struct my_error_mgr * my_error_ptr;



METHODDEF(void)
my_error_exit (j_common_ptr cinfo)
{
  /* cinfo->err really points to a my_error_mgr struct, so coerce pointer */
  my_error_ptr myerr = (my_error_ptr) cinfo->err;

  warning("jpeg-api: error.\n");
  /* Always display the message. */
  /* We could postpone this until after returning, if we chose. */
  (*cinfo->err->output_message) (cinfo);


  /* Return control to the setjmp point */
  longjmp(myerr->setjmp_buffer, 1);
}


// Given a JPEG buffer in buf, decode the contents into the given image.
image *
decode_image_from_jpeg_buffer(uchar *jbuf, int bufsize, image *into_image=NULL)
{
  IMAGE_ELEMENT_TYPE  element_type;
  int i,j,k; 
  struct jpeg_decompress_struct cinfo;
  struct my_error_mgr jerr;
  JSAMPARRAY buffer;		/* Output row buffer */
  RGB8_PIXEL *rgbbuf;
  char *bytebuf;
  int *rowbuf;

  cinfo.err = jpeg_std_error(&jerr.pub);
  jerr.pub.error_exit = my_error_exit;
  /* Establish the setjmp return context for my_error_exit to use. */
  if (setjmp(jerr.setjmp_buffer)) {
    /* If we get here, the JPEG code has signaled an error.
     * We need to clean up the JPEG object, close the input file, and return.
     */
    jpeg_destroy_decompress(&cinfo);
    return 0;
  }
  /* Now we can initialize the JPEG decompression object. */
  jpeg_create_decompress(&cinfo);

  jpeg_mem_src(&cinfo, jbuf, bufsize);
  /* Step 3: read file parameters with jpeg_read_header() */

  (void) jpeg_read_header(&cinfo, TRUE);
  /* We can ignore the return value from jpeg_read_header here. */

  /* Step 5: Start decompressor */

  (void) jpeg_start_decompress(&cinfo);
  /* We can ignore the return value since suspension is not possible. */

  /* We may need to do some setup of our own at this point before reading
   * the data.  After jpeg_start_decompress() we have the correct scaled
   * output image dimensions available, as well as the output colormap
   * if we asked for color quantization.
   * In this example, we need to make an output work buffer of the right size.
   */ 
  /* JSAMPLEs per row in output buffer */
  int row_stride = cinfo.output_width * cinfo.output_components;

  int nxdim = cinfo.output_width;
  int nydim = cinfo.output_height;

  if (into_image) {
    element_type = image_element_type(into_image);
    // Need more checking here, no?
  } else {
    switch (cinfo.out_color_space) {
    case JCS_RGB:
      element_type = IMG_RGB8;
      break;
    case JCS_GRAYSCALE:
      element_type = IMG_UNSIGNED_8BIT;
      break;
    otherwise:
      warning("make_image_from_jpeg_buffer: Can only handle RGB and grayscale images for now.");
      return NULL;
    }

    into_image = make_image(nxdim, nydim, element_type, 1);
  }

  if (into_image->xdim != nxdim || into_image->ydim != nydim) {
    warning("make_image_from_jpeg_buffer: image size mismatch!\n");
    return NULL;
  } else {

    /* Make a one-row-high sample array that will go away when done with image */
    buffer = (*cinfo.mem->alloc_sarray)
      ((j_common_ptr) &cinfo, JPOOL_IMAGE, row_stride, 1);

    /* Step 6: while (scan lines remain to be read) */
    /*           jpeg_read_scanlines(...); */

    for (i = nydim-1; i >= 0; i--) {
      (void) jpeg_read_scanlines(&cinfo, buffer, 1);
      rgbbuf = (RGB8_PIXEL*) buffer[0];
      if (element_type == IMG_RGB8) {
	into_image->putline(rgbbuf, 0, i);
      } else {
	into_image->putline((uchar*) rgbbuf, 0, i);
      }
      //      memcpy(outbuf, inbuf, row_stride);
      //      outbuf += row_stride;
    }

    /* Step 7: Finish decompression */

    (void) jpeg_finish_decompress(&cinfo);
    /* We can ignore the return value since suspension is not possible
     * with the stdio data source.
     */

    /* Step 8: Release JPEG decompression object */

    /* This is an important step since it will release a good deal of memory. */
  }
  jpeg_destroy_decompress(&cinfo);

  return into_image;
}


// Given a JPEG buffer in buf, put the DCT coefficients into the given image.
image *
decode_coefs_from_jpeg_buffer(uchar *jbuf, int bufsize, image *into_image=NULL)
{
  IMAGE_ELEMENT_TYPE  element_type;
  int i,j,k; 
  struct jpeg_decompress_struct cinfo;
  struct my_error_mgr jerr;
  JSAMPARRAY buffer;		/* Output row buffer */

  cinfo.err = jpeg_std_error(&jerr.pub);
  cinfo.buffered_image = TRUE;

  jerr.pub.error_exit = my_error_exit;
  /* Establish the setjmp return context for my_error_exit to use. */
  if (setjmp(jerr.setjmp_buffer)) {
    /* If we get here, the JPEG code has signaled an error.
     * We need to clean up the JPEG object, close the input file, and return.
     */
    jpeg_destroy_decompress(&cinfo);
    return 0;
  }
  /* Now we can initialize the JPEG decompression object. */
  jpeg_create_decompress(&cinfo);

  jpeg_mem_src(&cinfo, jbuf, bufsize);
  /* Step 3: read file parameters with jpeg_read_header() */

  (void) jpeg_read_header(&cinfo, TRUE);
  /* We can ignore the return value from jpeg_read_header here. */

  /* Step 5: Start decompressor */

  (void) jpeg_start_decompress(&cinfo);
  /* We can ignore the return value since suspension is not possible. */

  /* We may need to do some setup of our own at this point before reading
   * the data.  After jpeg_start_decompress() we have the correct scaled
   * output image dimensions available, as well as the output colormap
   * if we asked for color quantization.
   * In this example, we need to make an output work buffer of the right size.
   */ 
  /* JSAMPLEs per row in output buffer */
  int row_stride = cinfo.output_width * cinfo.output_components;

  int nxdim = cinfo.output_width;
  int nydim = cinfo.output_height;

  if (!into_image) {
    /* Hmmm.... not sure about this logic: */
    if (cinfo.output_components == 3) element_type = IMG_RGB8;
    else element_type = IMG_UNSIGNED_8BIT;

    into_image = make_image(nxdim, nydim, element_type, 1);
  }

  if (into_image->xdim != nxdim || into_image->ydim != nydim) {
    warning("make_image_from_jpeg_buffer: image size mismatch!\n");
    return NULL;
  } else {

    /* Make a one-row-high sample array that will go away when done with image */
    buffer = (*cinfo.mem->alloc_sarray)
      ((j_common_ptr) &cinfo, JPOOL_IMAGE, row_stride, 1);

    /* Step 6: while (scan lines remain to be read) */
    /*           jpeg_read_scanlines(...); */
    for (i = nydim-1; i >= 0; i--)
      (void) jpeg_read_scanlines(&cinfo, buffer, 1);

    /* Step 7: Finish decompression */

    (void) jpeg_finish_output(&cinfo);

    uchar *inbuf;
    jvirt_barray_ptr *coef_arrays;

    coef_arrays = jpeg_read_coefficients(&cinfo);
    inbuf = (uchar*) coef_arrays[0];

    for (i = nydim-1; i >= 0; i--) {
      into_image->putline(inbuf, 0, i);
      inbuf += nxdim;
    }

    (void) jpeg_finish_decompress(&cinfo);
    /* We can ignore the return value since suspension is not possible
     * with the stdio data source.
     */

    /* Step 8: Release JPEG decompression object */

    /* This is an important step since it will release a good deal of memory. */
  }
  jpeg_destroy_decompress(&cinfo);

  return into_image;
}


extern "C" {

cimage *
FREEDIUS_GLOBAL(make_image_from_jpeg_buffer) (uchar *jbuf, int bufsize, cimage *into_image=NULL)
{
  return (cimage*) decode_image_from_jpeg_buffer(jbuf, bufsize, (array_image_base *) into_image);
}

cimage *
FREEDIUS_GLOBAL(make_coef_image_from_jpeg_buffer) (uchar *jbuf, int bufsize, cimage *into_image=NULL)
{
  return (cimage *) decode_coefs_from_jpeg_buffer(jbuf, bufsize, (array_image_base *) into_image);
}

}

END_NAMESPACE_FREEDIUS
