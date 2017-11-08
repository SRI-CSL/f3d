
/*
SB-VM:N-LOWTAG-BITS = 4
SB-VM:OTHER-POINTER-LOWTAG = 15

Needs galloc and xxfree from misc.c++
*/


#include <stdlib.h>
// #include <mymalloc.h>

#include <errno.h>
#ifndef _WIN32
#include <strings.h>
#endif
#include <stdio.h>

#include "misc.h"
#include <string.h>
#include "namespace.h"


// FIXME:  Lots of magic numbers here that might be architecture dependent.
// OTHER_POINTER_LOWTAG_OFFSET,  alignment bytes, 

BEGIN_NAMESPACE_FREEDIUS

extern "C" {

int
FREEDIUS_GLOBAL(mem_read)(int *arr)
{
  return (arr[0]);
}

// offset from lisp repn to start of vector data
// FIXME:  This should be extracted from CMUCL internals.h
#define OTHER_POINTER_LOWTAG_OFFSET 1
#define N_LOWTAG_BITS 4
#define LISP_OBJECT_ALIGNMENT (1 << N_LOWTAG_BITS)

#define MAKE_FIXNUM(x)((x)<<(N_LOWTAG_BITS-1)) // x86-64 lowtag is 4 bits, but there are odd and even fixnums

typedef struct __LispSimpleVector__ {
  unsigned long type;   /* The typecode returned from lisp::%vector-type-code */
  unsigned long length; /* A fixnum, ie. left shifted 3 bits */
  } LispSimpleVector;

#define VECTOR_HEADER_LENGTH sizeof(__LispSimpleVector__)

/* length is the number of array elements
   typecode is the 1st value returned from (lisp::%vector-type-code array-element-type)
*/
static char *
make_simple_vector(int length, int typecode, int nbytes)
{
  LispSimpleVector *vect;
  char *vect_hdr_ptr;
  void *malloc_ptr;
  int malloc_offset;
  int status;
  //#define EXTRA_BYTES LISP_OBJECT_ALIGNMENT // FIXME DEBUG MEMORY CORRUPTION
  //#define EXTRA_PREAMBLE 8 // (LISP_OBJECT_ALIGNMENT>>1)
#define EXTRA_BYTES 0
#define EXTRA_PREAMBLE 0 
  int alloc_nbytes = nbytes + LISP_OBJECT_ALIGNMENT + VECTOR_HEADER_LENGTH + EXTRA_BYTES;
  /* extra for header info + 1 byte malloc_offset + 7 bytes for 8 byte alignment. */
#if !defined(HAVE_POSIX_MEMALIGN)
  malloc_ptr = (void *)galloc(alloc_nbytes); 
  if (malloc_ptr == 0) return(0);
  vect_hdr_ptr = (char *)malloc_ptr;
  vect_hdr_ptr = (char *)(((unsigned long)vect_hdr_ptr+LISP_OBJECT_ALIGNMENT+EXTRA_PREAMBLE) & ~(LISP_OBJECT_ALIGNMENT-1)); // force 16 byte alignment
#else  
  galloc_sanity_check(alloc_nbytes);
  status = posix_memalign(&malloc_ptr, LISP_OBJECT_ALIGNMENT, alloc_nbytes);
  if (status != 0) {
    if (status == EINVAL) 
      fprintf(stderr, "make_simple_vector: invalid alignment\n");
    return(0);
  }
  vect_hdr_ptr = ((char *) malloc_ptr)+LISP_OBJECT_ALIGNMENT;
#endif  
  malloc_offset = vect_hdr_ptr - (char *)malloc_ptr;
  *(vect_hdr_ptr-1) = malloc_offset; // save the malloc_offset immediately before the lisp header.
  vect = (LispSimpleVector *) vect_hdr_ptr;

  /* set LispSimpleVector header fields */
  vect->type = typecode;
  vect->length = MAKE_FIXNUM(length);
  return (((char *) vect) + VECTOR_HEADER_LENGTH); /* return pointer to first byte of data */
}


/* (typecode elemsize) are the values returned from (lisp::%vector-type-code array-element-type) */
char *
FREEDIUS_GLOBAL(make_integer_simple_vector)(int length, int typecode, int elemsize, int initial_element)
{
  char *vect;
  long nbits = length*elemsize;
  // int nbytes = nbits >> 3;
  //if (nbits & 63) nbytes+=8; /* pad out fractional word to multiple of 8 bytes*/
  int nbytes = ((nbits + 63) & ~63) >> 3;
 
  vect = make_simple_vector (length, typecode, nbytes);
  if (vect == 0) return(0);

  if (initial_element == 0)
    memset (vect, 0, nbytes);             /* clear the vector contents */
  else {
    int i, nwds = nbytes>>3;
    unsigned long *ptr = (unsigned long *)vect;
    int elems_per_word = 64/elemsize;
    /* this is wrong for double-float initializations */
    for (i=1; i<elems_per_word; i++)
      initial_element |= (initial_element << elemsize);
    for (; nwds>0; nwds--, ptr++)
      *ptr = initial_element;
  }
  return ((char *)(vect-OTHER_POINTER_LOWTAG_OFFSET));
}

float *
FREEDIUS_GLOBAL(make_single_float_simple_vector)(int length, int typecode, double dinitial_element)
{
  char *vect;
  float initial_element = dinitial_element;
  //int nbytes = length<<2;
  //if (nbytes & 4) nbytes+=4;
  int nbytes = ((length + 1)<<2) & ~7;

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

  return ((float *) (vect-OTHER_POINTER_LOWTAG_OFFSET));
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

  return ((double *) (vect-OTHER_POINTER_LOWTAG_OFFSET));
}

char *
FREEDIUS_GLOBAL(make_lisp_simple_vector)(int length, int typecode, int initial_element)
{
  char *vect;
  int nbytes = length<<3;
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
  return (vect-OTHER_POINTER_LOWTAG_OFFSET);
}


/* can we do some kind of error checking to verify that we really have a foreign vector rather
than a dynamically allocated vector */
     
// lisp_addr must be the tagged Lisp object, not the address of the first element of the array.
void
FREEDIUS_GLOBAL(unmake_simple_vector) (char *lisp_addr)
{
  char *malloc_ptr; 
  char *vect_hdr_ptr;
  // Pointer to start of Lisp header
  vect_hdr_ptr = lisp_addr + OTHER_POINTER_LOWTAG_OFFSET - sizeof(__LispSimpleVector__);
  malloc_ptr = vect_hdr_ptr - (*(vect_hdr_ptr-1)&0Xff); // mask to allow high 24 bits to be used for other things
  xxfree (malloc_ptr);
}

  
// lisp_addr must be the tagged Lisp object, not the address of the first element of the array.
int 
FREEDIUS_GLOBAL(simple_vector_typecode) (char *lisp_addr)
{
  LispSimpleVector *vect;
  vect = (LispSimpleVector *) (lisp_addr + OTHER_POINTER_LOWTAG_OFFSET - sizeof(__LispSimpleVector__));
  return (vect->type);
}


} // end "extern "C"


END_NAMESPACE_FREEDIUS

