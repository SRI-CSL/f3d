#include <errno.h>
#ifndef _WIN32
#include <strings.h>
#endif
#include <stdio.h>

#include "namespace.h"
#include <mymalloc.h>
#include "misc.h"

// FIXME:  Lots of magic numbers here that might be architecture dependent.
// VECTOR_DATA_OFFSET,  alignment bytes, 

BEGIN_NAMESPACE_FREEDIUS

extern "C" {


int
FREEDIUS_GLOBAL(mem_read)(int *arr)
{
  return (arr[0]);
}

// offset from lisp repn to start of vector data
// FIXME:  This should be extracted from CMUCL internals.h
#define VECTOR_DATA_OFFSET 1
#define VECTOR_HEADER_LENGTH 8
#define MAKE_FIXNUM(x)((x)<<2)

typedef struct __LispSimpleVector__ {
  unsigned int type;   /* The typecode returned from lisp::%vector-type-code */
  unsigned int length; /* A fixnum, ie. left shifted 2 bits */
  } LispSimpleVector;



#ifdef _WIN32
  // Bad hack, but galloc isn't resolved under win32 f3d-modular...
void *fvgalloc(int nbytes) {
  void *ptr;

  // This looks bogus.  For n_bytes less than a page, galloc_pages never increments.
  // Do we really want galloc_bytes += nbytes; if (galloc_bytes/1024 > 392320) ...?

  // More importantly, galloc_pages never decrements when memory is
  // freed.  What's the point of this??
  galloc_sanity_check(nbytes);
  nbytes = nbytes + 32; // FIXME -- debugging buffer overrun problems
  ptr = malloc(nbytes); // Should this be changed to use posix_memalign ?

  if (!ptr) printf("galloc failed to allocate %d bytes.\n", nbytes);
  return ptr;
}
#endif

/* length is the number of array elements
   typecode is the 1st value returned from (lisp::%vector-type-code array-element-type)
*/
static char *
make_simple_vector(int length, int typecode, int nbytes)
{
  LispSimpleVector *vect;
  char *vect_ptr;
  void *malloc_ptr;
  int malloc_offset;
  int status;
 
  /* 2 words extra for header info + 1 byte malloc_offset + 7 bytes for 8 byte alignment. */
#if !defined(HAVE_POSIX_MEMALIGN)

#ifdef _WIN32
  malloc_ptr = (void *)fvgalloc(nbytes+16); 
#else
  malloc_ptr = (void *)galloc(nbytes+16); 
#endif
  vect_ptr = (char *)malloc_ptr;
  if (malloc_ptr == 0) return(0);
  vect_ptr = (char *)(((long)vect_ptr+8) & ~7); // force double word alignment
#else  
  galloc_sanity_check(nbytes+16);
  status = posix_memalign(&malloc_ptr, 8, nbytes+16);
  if (status != 0) {
    if (status == EINVAL) 
      fprintf(stderr, "make_simple_vector: invalid alignment\n");
    return(0);
  }
  vect_ptr = ((char *) malloc_ptr)+8;
#endif  
  malloc_offset = vect_ptr - (char *)malloc_ptr;
  *(vect_ptr-1) = malloc_offset; // save the malloc_offset immediately before the lisp header.
  vect = (LispSimpleVector *) vect_ptr;

  /* set LispSimpleVector header fields */
  vect->type = typecode;
  vect->length = MAKE_FIXNUM(length);
  return (((char *) vect) + VECTOR_HEADER_LENGTH); /* return pointer to first byte of data */
}


/* (typecode elemsize) are the values returned from (lisp::%vector-type-code array-element-type) */
int *
FREEDIUS_GLOBAL(make_integer_simple_vector)(int length, int typecode, int elemsize, int initial_element)
{
  char *vect;
  int nbits = length*elemsize;
  int nbytes = nbits >> 3;
  if (nbits & 31) nbytes+=4; /* pad out fractional word to full word*/
 
  vect = make_simple_vector (length, typecode, nbytes);
  if (vect == 0) return(0);

  if (initial_element == 0)
    memset (vect, 0, nbytes);             /* clear the vector contents */
  else {
    int i, nwds = nbytes>>2, *ptr = (int *)vect;
    int elems_per_word = 32/elemsize;
    /* this is wrong for double-float initializations */
    for (i=1; i<elems_per_word; i++)
      initial_element |= (initial_element << elemsize);
    for (; nwds>0; nwds--, ptr++)
      *ptr = initial_element;
  }
  return ((int *)(vect-VECTOR_DATA_OFFSET));
}

float *
FREEDIUS_GLOBAL(make_single_float_simple_vector)(int length, int typecode, double dinitial_element)
{
  char *vect;
  float initial_element = dinitial_element;
  int nbytes = length<<2;
  vect = make_simple_vector (length, typecode, nbytes);
  if (vect == 0) return((float *) 0);

  if (initial_element == 0.0)
    memset (vect, 0, nbytes);             /* clear the vector contents */
  else {
    int i, cnt;
    float *ptr = (float *) vect;
    for (cnt=length; cnt>0; cnt--, ptr++)
      *ptr = initial_element;
  }

  return ((float *) (vect-VECTOR_DATA_OFFSET));
}

double *
FREEDIUS_GLOBAL(make_double_float_simple_vector)(int length, int typecode, double initial_element)
{
  char *vect;
  /* Fri Nov 14 1997 LHQ  Goddamn C operator precedence.  Just tracked down bug here, 
     due to precedence of << vs. +.  Code used to be
     int nbytes = length<<3 + 4;  which means int nbytes = length<<(3 + 4);
  */
  int nbytes = (length<<3); 
  vect = make_simple_vector (length, typecode, nbytes);
  if (vect == 0) return((double *) 0);

  if (initial_element == 0.0)
    memset (vect, 0, nbytes);             /* clear the vector contents */
  else {
    int i, cnt;
    double *ptr = (double *) (vect);
    for (cnt = length; cnt>0; cnt--, ptr++)
      *ptr = initial_element;
  }

  return ((double *) (vect-VECTOR_DATA_OFFSET));
}

char *
FREEDIUS_GLOBAL(make_lisp_simple_vector)(int length, int typecode, int initial_element)
{
  char *vect;
  int nbytes = length<<2;
  vect = make_simple_vector (length, typecode, nbytes);
  if (vect == 0) return((char *) 0);

  if (initial_element == 0)
    memset (vect, 0, nbytes);             /* clear the vector contents */
  else {
    int i, cnt;
    int *ptr = (int *) vect;
    for (cnt=length; cnt>0; cnt--, ptr++)
      *ptr = initial_element;
  }
  return (vect-VECTOR_DATA_OFFSET);
}


/* can we do some kind of error checking to verify that we really have a foreign vector rather
than a dynamically allocated vector */
     
// lisp_addr must be the tagged Lisp object, not the address of the first element of the array.
void
FREEDIUS_GLOBAL(unmake_simple_vector) (char *lisp_addr)
{
  char *malloc_ptr, *vect_ptr;
  // Pointer to start of Lisp header
  vect_ptr = lisp_addr + VECTOR_DATA_OFFSET - VECTOR_HEADER_LENGTH;
  malloc_ptr = vect_ptr - (*(vect_ptr-1)&0Xff); // mask to allow high 24 bits to be used for other things
  //  xxfree (malloc_ptr);
  free (malloc_ptr);
}

  
// lisp_addr must be the tagged Lisp object, not the address of the first element of the array.
int 
FREEDIUS_GLOBAL(simple_vector_typecode) (char *lisp_addr)
{
  LispSimpleVector *vect;
  vect = (LispSimpleVector *) (lisp_addr + VECTOR_DATA_OFFSET - VECTOR_HEADER_LENGTH);
  return (vect->type);
}


} // end "extern "C"


END_NAMESPACE_FREEDIUS

