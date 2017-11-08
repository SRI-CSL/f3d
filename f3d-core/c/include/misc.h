#ifndef __misc_h
#define __misc_h

/* This header file includes definitions for functions in several files:  
   misc.c++ io.c++ run-program.c++
*/

#if defined( _WIN32 ) || defined( MINGW )

#include <windows.h>
#include <io.h>
typedef unsigned short ushort;
typedef unsigned char uchar;

#else // defined( _WIN32 ) || defined( MINGW )

#define O_BINARY 0x0
#include <sys/types.h>

#endif // defined( _WIN32 ) || defined( MINGW )

#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>

#include "cme-error.h"
#include "minmax.h"
#include "namespace.h"


BEGIN_NAMESPACE_FREEDIUS

#define NIL 0
#define TRUE 1
#define FALSE 0
#define internal static
#define INLINE static inline
typedef char *string;

// Functions defined in misc.c++

enum _endian_ {
  CME_LITTLE_ENDIAN = 0,
  CME_BIG_ENDIAN = 1
};

typedef enum _endian_ ENDIAN;
ENDIAN machine_endian (void);

string local_hostname(void);

void xxfree (void* ptr);
extern int galloc_pages;
void *galloc(int nbytes);
// zalloc is same as galloc, but zeros the array
void *zalloc(int nbytes);

#define XALLOC(type,nelems)((type *)galloc((nelems)*sizeof(type)))
#define ZALLOC(type,nelems)((type *)zalloc((nelems)*sizeof(type)))

INLINE void *make_vector(int nbytes)
{ 
  void *v = galloc(nbytes);
  if (v == NULL) 
    error("make_vector: galloc failed to allocate %d bytes: ", nbytes);
  return v;
}

#if defined(__GNUG__)
#define STACK_ALLOCATE(type, name, size) type name[size]
#define STACK_DEALLOCATE(name)
#else
#define STACK_ALLOCATE(type, name, size) type *name = (type *) galloc(size*sizeof(type))
#define STACK_DEALLOCATE(name) free(name)
#endif

#ifdef _WIN32
extern "C" {
void PrintLastError(const char *);
} // end extern "C" 
#endif 

extern "C" {

void galloc_sanity_check (int nbytes);
// These are accessed from tiff-ffi.c
void FREEDIUS_GLOBAL(free)(void *ptr);
void* FREEDIUS_GLOBAL(galloc)(int nbytes);

// This is called from Lisp, but isn't really used.
ENDIAN FREEDIUS_GLOBAL(machine_endian) (void);

  // These should use FREEDIUS_GLOBAL
int fullread(int, char*, int);    /* defined in misc.c++ */
int fullwrite(int, char*, int);   /* defined in misc.c++ */

}// end extern "C"




// Functions defined in io.c++

extern "C" {

extern char *expand_pathname (char *pathname);

} // end extern "C"

// Functions defined in run-program.c++

extern "C" {

int run_program(char **argv, char **envp, int waitp);
int run_program_wait(char **argv);
int mkfile(char *path, unsigned int size, int units);
int copy_file(const char *from, const char *to);

} // end extern "C"



// inline functions

INLINE void ignore(...) {}

// This is wrong for negative numbers.
INLINE int ceiling(int n, int m)
{
  return (1+ ((n-1)/ m));
}

INLINE int pad_to_multiple (int n, int m)
{ 
  if (m == 0) return n;
  return (m * ceiling(n,m));
}

INLINE int truncate_to_multiple (int n, int m)
{ 
  if (m == 0) return n;
  return (m * (n/m));
}

// Sorry -- log2 is defined in Codewarrior.

INLINE int cmelog2(int n)
{
  int i = 0;
  if (n < 0) n = -n;
  n=n-1;
  while (n) {n = n>>1; i++;}
  return(i);
}

INLINE int 
integer_length(int n)
{
  int i;
 for (i=0; n!=0; i++)
   n=n>>1;
 return(i);
}

INLINE int pad_to_pwr_of_2(int n)
{
  return 1 << cmelog2(n);
}

template <class fromT, class toT>
void copy (fromT *from, toT *to, int n, int fromi=0, int toi=0, int from_incr = 1, int to_incr = 1) 
{
  from += fromi; 
  to += toi;
  if (from_incr == 1 && to_incr == 1)
    for (int i=0; i<n; i++) 
      to[i] = from[i]; // speed this up with bcopy
  else if (from_incr == 1)
    for (int i=0; i<n; i++) {
      *to = from[i];
      to+=to_incr; 
    }
  else if (to_incr == 1)
    for (int i=0; i<n; i++) {
      to[i] = *from;
      from+=from_incr;
    }
  else 
    for (int i=n; i>0; i--) {
      *to = *from;
      to+=to_incr; 
      from+=from_incr;
    }
}


// extern int DEFAULT_OPEN_MODE; /* = 0700 */
#define DEFAULT_OPEN_MODE 0700

// Usually not defined in Un*x:
#ifndef O_BINARY
#define O_BINARY 0
#endif

// This looks wrong -- should it be ((x) & O_RDONLY) ??
#define RDONLY_P(x) ((x)==(O_RDONLY|O_BINARY))

INLINE int
read_open (char *pathname, int error_ok = 0)
{
  int val;
  pathname = expand_pathname(pathname);
  // Dicey -- do we ever use read_open to open a text file?
#ifdef _MSC_VER
  val = _open(pathname, O_RDONLY | O_BINARY, DEFAULT_OPEN_MODE);
#else
  val = open(pathname, O_RDONLY | O_BINARY, DEFAULT_OPEN_MODE);
#endif

  if (error_ok == 0 && val <0)
    error("read_open: the file %s does not exist.", pathname);
  return(val);
}

// For non-Unix we need to look carefully at the use of O_BINARY in the following functions.

INLINE int
read_open_error_ok (char *pathname)
{
  int val;
  pathname = expand_pathname(pathname);

#ifdef _MSC_VER
  val = _open(pathname, O_RDONLY | O_BINARY, DEFAULT_OPEN_MODE);
#else
  val = open(pathname, O_RDONLY | O_BINARY, DEFAULT_OPEN_MODE);
#endif

  return(val);
}

INLINE int
write_open(char *pathname, int error_ok = 0)
{
  int val;
  pathname = expand_pathname(pathname);
#ifdef _MSC_VER
  val = _open(pathname, O_CREAT | O_WRONLY | O_TRUNC | O_BINARY, DEFAULT_OPEN_MODE);
#else
  val = open(pathname, O_CREAT | O_WRONLY | O_TRUNC | O_BINARY, DEFAULT_OPEN_MODE);
#endif

  if (error_ok == 0 && val <0)
    error("write_open: the file %s cannot be opened for write.", pathname);
  return(val); 
}

INLINE int
general_open(char *pathname, int iomode, int error_ok = 0)
{
  int val;
  pathname = expand_pathname(pathname);

#ifdef _MSC_VER
  val = _open(pathname, iomode, DEFAULT_OPEN_MODE);
#else
  val = open(pathname, iomode, DEFAULT_OPEN_MODE);
#endif
  if (error_ok == 0 && val <0)
    error("open: the file %s cannot be opened for mode %d.", pathname, iomode);
  return(val); 
}



END_NAMESPACE_FREEDIUS

#endif /* ! __misc_h */ 





