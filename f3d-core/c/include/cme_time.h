#ifndef __time2_h
#define __time2_h

// timeval is defined in Un*x and MinGW:

#if defined(MINGW) || !defined(_WIN32)
#include <sys/time.h>
#endif


#if defined(_WIN32) && !defined(MINGW)

// Not sure if windows w/o MINGW defines this:
#if 0
struct timeval {
  unsigned long tv_sec;
  unsigned long tv_usec;
};
#endif

struct timezone {
  int tz_minuteswest;
  int tz_dsttime;
};

#endif

  // This doesn't work in CodeWarrior -- My guess is that some phases
  // of compilation are not case sensitive.
typedef struct timeval Timeval;
typedef struct timeval * TIMEVALUE;

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS
  //#ifndef REDHATLOSSAGE
  //int getrusage(int who, struct rusage *rusage);
  //#endif



TIMEVALUE time_diff(TIMEVALUE tv1, TIMEVALUE tv2, TIMEVALUE tv_diff);

TIMEVALUE time_add(TIMEVALUE tv1, TIMEVALUE tv2, TIMEVALUE tv_plus);

void zero_time (TIMEVALUE tv);

TIMEVALUE run_time(TIMEVALUE tv);

TIMEVALUE sys_time(TIMEVALUE tv);

TIMEVALUE elapsed_time(TIMEVALUE tv);

double time_secs (TIMEVALUE tv);

END_NAMESPACE_FREEDIUS

#endif /* ! __time2_h */ 
