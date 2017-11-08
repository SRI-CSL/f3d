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

/*
 * This output manager interfaces jpeg I/O with memory buffers.
 * -Chris Connolly
 * 11/15/2002
 */

/* Expanded data destination object for output to memory */

typedef struct {
  struct jpeg_destination_mgr pub; /* public fields */

  unsigned char *outbuf;		/* target stream */
  int maxsize;
  int length;
  JOCTET * buffer;		/* start of buffer */
} mem_destination_mgr;

typedef mem_destination_mgr * mem_dest_ptr;

#define OUTPUT_BUF_SIZE  4096	/* choose an efficiently fwrite'able size */

static int judp_errno = 0;

#define JUDP_ERR_CANTEMPTY     1
#define JUDP_ERR_TOOMUCHDATA   2

GLOBAL(int) jpeg_mem_errno() {
  return judp_errno;
}


/*
 * Initialize destination --- called by jpeg_start_compress
 * before any data is actually written.
 */

METHODDEF(void)
init_destination (j_compress_ptr cinfo)
{
  mem_dest_ptr dest = (mem_dest_ptr) cinfo->dest;

  judp_errno = 0;

  dest->buffer = (JOCTET *)
    (*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_IMAGE,
				OUTPUT_BUF_SIZE * SIZEOF(JOCTET));

  dest->pub.next_output_byte = dest->buffer;
  dest->pub.free_in_buffer = OUTPUT_BUF_SIZE;
}


/*
 * Empty the output buffer --- called whenever buffer fills up.
 *
 * In typical applications, this should write the entire output buffer
 * (ignoring the current state of next_output_byte & free_in_buffer),
 * reset the pointer & count to the start of the buffer, and return TRUE
 * indicating that the buffer has been dumped.
 *
 * In applications that need to be able to suspend compression due to output
 * overrun, a FALSE return indicates that the buffer cannot be emptied now.
 * In this situation, the compressor will return to its caller (possibly with
 * an indication that it has not accepted all the supplied scanlines).  The
 * application should resume compression after it has made more room in the
 * output buffer.  Note that there are substantial restrictions on the use of
 * suspension --- see the documentation.
 *
 * When suspending, the compressor will back up to a convenient restart point
 * (typically the start of the current MCU). next_output_byte & free_in_buffer
 * indicate where the restart point will be if the current call returns FALSE.
 * Data beyond this point will be regenerated after resumption, so do not
 * write it out when emptying the buffer externally.
 */

METHODDEF(boolean)
empty_output_buffer (j_compress_ptr cinfo)
{
  int i, j;
  mem_dest_ptr dest = (mem_dest_ptr) cinfo->dest;

  j = dest->length;
  for (i = 0; i < OUTPUT_BUF_SIZE && j < dest->maxsize; i++) {
    dest->outbuf[j++] = dest->buffer[i];
  }

  dest->length = j;

  if (j >= dest->maxsize) {
    // printf("jpeg-mem-dst.c: empty_output_buffer exceeded the output buffer length.\n");
    judp_errno = JUDP_ERR_CANTEMPTY;
    return FALSE;
  }

  dest->pub.next_output_byte = dest->buffer;
  dest->pub.free_in_buffer = OUTPUT_BUF_SIZE;

  return TRUE;
}


/*
 * Terminate destination --- called by jpeg_finish_compress
 * after all data has been written.  Usually needs to flush buffer.
 *
 * NB: *not* called by jpeg_abort or jpeg_destroy; surrounding
 * application must deal with any cleanup that should happen even
 * for error exit.
 */

METHODDEF(void)
term_destination (j_compress_ptr cinfo)
{
  int i, j;
  mem_dest_ptr dest = (mem_dest_ptr) cinfo->dest;
  size_t datacount = OUTPUT_BUF_SIZE - dest->pub.free_in_buffer;

  /* Write any data remaining in the buffer */
  if (datacount > 0) {
    j = dest->length;
    for (i = 0; i < datacount && j < dest->maxsize; i++)
      dest->outbuf[j++] = dest->buffer[i];

    dest->length = j;
    if (j >= dest->maxsize) {
      // printf("jpeg-mem-dst.c: term_destination exceeded the output buffer length.\n");
      judp_errno = JUDP_ERR_TOOMUCHDATA;
    }
  }
}


/*
 * Prepare for output to a stdio stream.
 * The caller must have already opened the stream, and is responsible
 * for closing it after finishing compression.
 */

GLOBAL(void)
jpeg_mem_dest (j_compress_ptr cinfo, unsigned char* data, int size)
{
  mem_dest_ptr dest;

  /* The destination object is made permanent so that multiple JPEG images
   * can be written to the same file without re-executing jpeg_stdio_dest.
   * This makes it dangerous to use this manager and a different destination
   * manager serially with the same JPEG object, because their private object
   * sizes may be different.  Caveat programmer.
   */
  if (cinfo->dest == NULL) {	/* first time for this JPEG object? */
    cinfo->dest = (struct jpeg_destination_mgr *)
      (*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_PERMANENT,
				  SIZEOF(mem_destination_mgr));
  }

  dest = (mem_dest_ptr) cinfo->dest;
  dest->pub.init_destination = init_destination;
  dest->pub.empty_output_buffer = empty_output_buffer;
  dest->pub.term_destination = term_destination;
  dest->outbuf = data;
  dest->maxsize = size;
  dest->length = 0;
}

GLOBAL(int) jpeg_mem_packet_size(j_compress_ptr cinfo) {
  mem_dest_ptr dest;
  dest =  (mem_dest_ptr) cinfo->dest;
  return dest->length;
}

int
encode_image_into_jpeg_buffer(uchar *jbuf, int buflen, image *from_image, int quality)
{
  int i,j,k;
  int comp;
  struct jpeg_compress_struct cinfo;
  struct jpeg_error_mgr jerr;
  JSAMPARRAY buffer;		/* Output row buffer */
  int xdim;
  int *cbuf;
  int len;

  //  printf("Initializing cinfo...\n");
  cinfo.err = jpeg_std_error(&jerr);
  jpeg_create_compress(&cinfo);
  xdim = cinfo.image_width = from_image->xdim;
  cinfo.image_height = from_image->ydim;

  // For RGB or YUV we simply set components to 3, and use JCS_RGB
  // or JCS_YCbCr.  With YUV 411 or 422, care must be taken to
  // properly replicate the U and V pixels when feeding scanlines to
  // the JPEG compressor.  On decompression, these will end up being
  // full-frame components.

  switch (image_element_type(from_image)) {
  case IMG_RGB8: cinfo.in_color_space = JCS_RGB;
    comp = cinfo.input_components = 3;
    cbuf = (int *) XALLOC(RGB8_PIXEL, xdim);
    break;

  case IMG_UNSIGNED_8BIT:
  case IMG_SIGNED_8BIT:
    cinfo.in_color_space = JCS_GRAYSCALE;
    comp = cinfo.input_components = 1;
    cbuf = XALLOC(int, xdim);
    break;
 
  otherwise:
    return -1;
  }

  int row_stride = xdim * comp;

  jpeg_mem_dest(&cinfo, jbuf, buflen);
  jpeg_set_defaults(&cinfo);

  jpeg_set_quality (&cinfo,  quality, FALSE);

  jpeg_start_compress(&cinfo, TRUE);

  // Arg.  Here we set aside a second uchar buffer suitable for
  // framing in libjpeg.  Need to generalize this:

  buffer = (*cinfo.mem->alloc_sarray)
      ((j_common_ptr) &cinfo, JPOOL_IMAGE, row_stride, 1);
  uchar *into_buf = buffer[0];

  if (cinfo.in_color_space == JCS_RGB) {
    //    printf("Color compression.\n");
    
    for (i = 0; i < from_image->ydim; i++) {
      k = 0;
      from_image->getline( (RGB8_PIXEL *)cbuf, 0, i);
      
      memcpy(into_buf, cbuf, row_stride);
      jpeg_write_scanlines(&cinfo, buffer, 1);
    }
  } else {
    //    printf("Monochrome compression.\n");

    for (i = 0; i < from_image->ydim; i++) {
      from_image->getline(cbuf, 0, i);

      // Can't use memcpy here because each element of cbuf is really
      // an int:
      for (j = 0; j < xdim; j++) buffer[0][j] = (uchar) cbuf[j];
      jpeg_write_scanlines(&cinfo, buffer, 1);
    }
  }

  jpeg_finish_compress(&cinfo);

  len = jpeg_mem_packet_size(&cinfo);
  // printf("packet length = %d.\n", len);
  jpeg_destroy_compress(&cinfo);

  xxfree(cbuf);

  if (jpeg_mem_errno() > 0) return -2;

  return len;
}

extern "C" {

int
FREEDIUS_GLOBAL(make_jpeg_buffer_from_image) (cimage *from_image, uchar *jbuf, int bufsize, int quality)
{
  return encode_image_into_jpeg_buffer(jbuf, bufsize, (array_image_base *) from_image, quality);
}

}

END_NAMESPACE_FREEDIUS
