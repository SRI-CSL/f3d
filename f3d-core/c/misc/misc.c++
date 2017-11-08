// -*- Mode: C++; c-basic-offset: 2 -*-

// $Id: misc.c++,v 1.20.2.1.2.6 2009/04/11 18:05:50 quam Exp $

// This file contains a pile of mostly unrelated utility functions.


#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#ifndef _WIN32
#include <unistd.h>
#include <sys/time.h>
#endif
#include <time.h>
#include <errno.h>

#include "misc.h"

#include "lisp-callback.h"
//#include "trace-output.h"

BEGIN_NAMESPACE_FREEDIUS

// #define STDERR stderr

// Why is this a global rather than a #define ?
//int DEFAULT_OPEN_MODE = 0700;

char * local_hostname(void)
{
  static char * my_local_hostname = (char *) 0;
  if (! my_local_hostname)
    my_local_hostname =  (char *)getenv("HOST");
  if (! my_local_hostname) my_local_hostname = (char *)"localhost";
  return(my_local_hostname);
}

#if 0
ENDIAN machine_endian () {
 short n = 1;
 char *p = (char *) &n;
 return (*p) ? CME_LITTLE_ENDIAN : CME_BIG_ENDIAN;
}

#else

ENDIAN machine_endian () {
// Use the declaration provided by configure in config.h
#ifdef WORDS_BIGENDIAN  
  return CME_BIG_ENDIAN;
#else
  return CME_LITTLE_ENDIAN;
#endif
}

#endif

extern "C" {

ENDIAN FREEDIUS_GLOBAL(machine_endian) () {
  return machine_endian();
}

} // end extern "C"



// tprintf and debug_trace_level should probably move to cme-error.c++

static int debug_trace_level = 0;

void tprintf(int level, const char *format, ...) {
  static char outbuf[512];
  va_list args;

  if (level < debug_trace_level) {
    va_start(args, format);
    vsprintf(outbuf, format, args);
    va_end(args);
    warning(outbuf);
  }
}

extern "C" {

int FREEDIUS_GLOBAL(set_write_trace_debug_level)(int n) {
  debug_trace_level = n;
  return n;
}

} // end extern "C" 



#if defined(_WIN32) && !defined(CYGWIN)
void sleep_ms(unsigned int msec) {
  Sleep(msec);
}
#else
void sleep_ms(unsigned int msec) {
  usleep(msec*1000);
}
#endif

/* Defaults: we try up to 20 times to get the right number of bytes,
   and we wait 10ms between attempts. */

static int retry_max = 50;
static int retry_wait = 10;  /* in milliseconds. */

// Called from Lisp ???  where
int set_read_retries (int k) {
  retry_max = k;
  return retry_max;
}

// Called from Lisp ???  where
int set_read_retry_interval(int k) {
  retry_wait = k;
  return retry_wait;
}

extern "C" {

/* Read with retries to make sure all nbytes bytes are obtained. */

int fullread(int fd, char *buf, int nbytes) {
  int so_far, i, k;

  so_far = read(fd, buf, nbytes);

  i = 0;
  while (so_far < nbytes && i < retry_max) {
    if (so_far < 0) so_far = 0;

    k = read(fd, buf+so_far, nbytes-so_far);

    /* Return on error or EOF: */
    if (k < 0) return k;

    so_far += k;
    i++;
    sleep_ms(retry_wait);
  }

  if (so_far < nbytes)
    tprintf(4, "fullread: only %d bytes out of %d requested were read.\n", so_far, nbytes);

  return so_far;
}

int fullwrite(int fd, char *buf, int nbytes) {
  int so_far, i, k;

  so_far = write(fd, buf, nbytes);

  i = 0;
  while (so_far < nbytes && i < retry_max) {
    k = write(fd, buf+so_far, nbytes-so_far);

    /* Return on error or EOF: */
    if (k < 0) return k;

    so_far += k;
    i++;
    sleep_ms(retry_wait);
  }

  if (so_far < nbytes)
    tprintf(4, "fullwrite: only %d bytes out of %d requested were written.\n", so_far, nbytes);

  return so_far;
}

} // end extern "C"


// memory allocation

// Only counts galloc vs xxfree imbalance:
int xxcounter = 0;

extern "C" {

int xxcounter_value() {
  return xxcounter;
}

} // end extern "C"


void xxfree (void* ptr)
{
  xxcounter--;
  free(ptr);
}

int galloc_pages = 0;

extern "C" {
void galloc_sanity_check (int nbytes)
{
  xxcounter++;
  //  galloc_pages += nbytes / 1024;
  if (galloc_pages > 392320) printf("trying to allocate more than 392320 pages through malloc...\n");
}

} // end extern "C"

void *galloc(int nbytes) {
  void *ptr;

  // This looks bogus.  For n_bytes less than a page, galloc_pages never increments.
  // Do we really want galloc_bytes += nbytes; if (galloc_bytes/1024 > 392320) ...?

  // More importantly, galloc_pages never decrements when memory is
  // freed.  What's the point of this??
  galloc_sanity_check(nbytes);
  nbytes = nbytes + 32; // FIXME -- debugging buffer overrun problems
  ptr = malloc(nbytes); // Should this be changed to use posix_memalign ?

  if (!ptr) error("galloc failed to allocate %d bytes.\n", nbytes);
  return ptr;
}

void * 
zalloc(int nbytes)
{
  void *ptr;
  ptr = (char *) galloc(nbytes);
  memset((void*) ptr, 0, nbytes);
  return(ptr);
}


extern "C" {

// These are called from tiff-ffi.c

void FREEDIUS_GLOBAL(free)(void *ptr) {
  xxfree(ptr);
};

void *FREEDIUS_GLOBAL(galloc)(int nbytes) 
{
  return galloc(nbytes);
}

void *FREEDIUS_GLOBAL(zalloc)(int nbytes) 
{
  return zalloc(nbytes);
}


#ifdef _WIN32
int fd_seek(int fd, unsigned int offset)
{
  /* This is pretty stupid. */
  if (_lseek(fd, (long)offset, SEEK_SET) == -1L) {
	perror("_lseek");
	return 0;
  }
  return 1;
}  
#else
int fd_seek(int fd, unsigned int offset) {
  
  if (lseek(fd, (off_t) offset, SEEK_SET) == -1) {
    perror("in fd_seek");
    return 0;
  }
  return 1;
}
#endif

// should this move to cme-error.c++ ?

#if defined(MINGW) || defined(_WIN32)
void PrintLastError(const char *string)
{
  LPVOID lpMsgBuf;
  DWORD dwLastError;

  dwLastError = GetLastError();
  FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
				FORMAT_MESSAGE_FROM_SYSTEM |
				FORMAT_MESSAGE_IGNORE_INSERTS,
				NULL,
				dwLastError,
				MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
				(LPTSTR) &lpMsgBuf,
				0,
				NULL);
  if (string != NULL)
	printf("%s: %s", string, lpMsgBuf);
  else
	printf("%s", lpMsgBuf);
  LocalFree(lpMsgBuf);
}
#endif


} // end extern "C"



extern "C" {

//
// This is needed to get millisecond-resolution timestamps -- moved
// from video subsystem into FREEDIUS core. -- CC 4/3/03
//
#ifndef _WIN32
unsigned int day_millisecond_time(void) {
  /* This returns the number of milliseconds since midnight of the
     current day, local time..*/
  struct timeval tv;
  struct timezone tz;
  if (gettimeofday(&tv,&tz) == -1) {
    printf("Error: %d -- ", errno);
    if (errno == EINVAL) printf("Timezone (or something else) is invalid.\n");
    else if (errno == EFAULT) printf("One of tv or tz points outside accessible address space.\n");
    else printf("Bad hair day.\n");
    perror("Bad call to gettimeofday.");
    return 0;
  } else {
    // This first one is the number of msec in the current hour!
    //    return ((tv.tv_sec % 3600) * 1000 + (tv.tv_usec / 1000));
    return ((tv.tv_sec % 86400) * 1000 + (tv.tv_usec / 1000));
  }
}

//
// Moved from freedius-extras to FREEDIUS core -- this is specific to
// certain Unix systems, but is a good idea and we should try to
// figure out how to emulate it on other systems.  nanosleep is a
// fairly "safe" sleep function (doesn't rely on signals, for
// example).  -CC 4/3/03

int fr_nanosleep (unsigned int seconds, unsigned int nanoseconds) {
  struct timespec t_in, t_out;
  t_in.tv_sec = seconds;
  t_in.tv_nsec = nanoseconds;

  if (nanosleep(&t_in, &t_out) != 0) return -1;
  else return (int) t_out.tv_sec;
}

#else

unsigned int day_millisecond_time(void) {
  printf("WARNING: day_millisecond_time() is not properly defined in Windows.  Returning 0.\n");
  return 0;
}
	
int fr_nanosleep(unsigned int seconds, unsigned int nanoseconds) {
  sleep_ms(1000*seconds + nanoseconds / 1000);
  return 0;
}	

#endif

} // end extern "C"



END_NAMESPACE_FREEDIUS
