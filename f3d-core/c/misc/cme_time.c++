// -*- Mode: C++; c-basic-offset: 2 -*-

// $Id: cme_time.c++,v 1.3.8.2 2008/01/09 06:47:17 connolly Exp $

#ifdef MINGW
//#define _ASSERTE(x) {}
#endif

// I wonder if these should be moved to the include file:
#if !defined(_WIN32) && !defined(MINGW)
#include <sys/resource.h>
#endif

#if defined(_WIN32) || defined(MINGW)
#include <windows.h>
#include <psapi.h>
//#ifndef MINGW
//#include <crtdbg.h>
//#endif
#define _ASSERTE(x) {}
#endif

#include "misc.h"
#include "cme_time.h"
#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

#if defined(_WIN32) || defined(MINGW)
#define UNIX_EPOCH 0x19DB1DED53E8000LL

#define RUSAGE_SELF 0
#define RUSAGE_CHILDREN -1

struct rusage {
  struct timeval ru_utime;
  struct timeval ru_stime;
  long ru_maxrss;
  long ru_ixrss;
  long ru_idrss;
  long ru_isrss;
  long ru_minflt;
  long ru_majflt;
  long ru_nswap;
  long ru_inblock;
  long ru_oublock;
  long ru_msgsnd;
  long ru_msgrcv;
  long ru_nsignals;
  long ru_nvcsw;
  long ru_nivcsw;
};

#endif

TIMEVALUE 
time_diff(TIMEVALUE tv1, TIMEVALUE tv2, TIMEVALUE tv_diff)
{
  int sec_diff = tv1->tv_sec - tv2->tv_sec;
  int usec_diff = tv1->tv_usec - tv2->tv_usec;
  if (usec_diff < 0) 
    {sec_diff--;
    usec_diff+=1000000;}
  if (! tv_diff) tv_diff = tv1;
  tv_diff->tv_sec = sec_diff;
  tv_diff->tv_usec = usec_diff;
  return(tv_diff);
}

TIMEVALUE 
time_add(TIMEVALUE tv1, TIMEVALUE tv2, TIMEVALUE tv_plus)
{
  int sec_plus = tv1->tv_sec + tv2->tv_sec;
  int usec_plus = tv1->tv_usec + tv2->tv_usec;
  if (usec_plus < 0) 
    {sec_plus--;
    usec_plus+=1000000;}
  if (usec_plus >1000000) 
    {sec_plus++;
    usec_plus-=1000000;}
  if (! tv_plus) tv_plus = tv1;
  tv_plus->tv_sec = sec_plus;
  tv_plus->tv_usec = usec_plus;
  return(tv_plus);
}

void
zero_time (TIMEVALUE tv)
{ 
  tv->tv_sec = 0;
  tv->tv_usec = 0;
}


//
// If necessary, define gettimeofday:
//
#if defined(_WIN32)

#if !defined(MINGW)
int
gettimeofday(struct timeval *tv, struct timezone *tz)
{
  FILETIME ft;
  LARGE_INTEGER li;
  __int64 acc;

  _ASSERTE(tv != NULL);

  // This is allowed to be NULL:
  //  _ASSERTE(tz != NULL);

  GetSystemTimeAsFileTime(&ft);
  li.LowPart  = ft.dwLowDateTime;
  li.HighPart = ft.dwHighDateTime;
  acc = li.QuadPart;
  acc -= UNIX_EPOCH; 
  acc /= 10;
  tv->tv_sec  = (long)(acc / 1000000);
  tv->tv_usec = (long)(acc % 1000000);
  if (tz) {
    tz->tz_minuteswest = 0;
    tz->tz_dsttime = 0;
  }

  return 0;
}
#endif

static void
filetime_to_timeval(LPFILETIME ft, struct timeval *tv)
{
  ULARGE_INTEGER uli;

  _ASSERTE(ft != NULL);
  _ASSERTE(tv != NULL);
  uli.LowPart = ft->dwLowDateTime;
  uli.HighPart = ft->dwHighDateTime;
  tv->tv_sec = (long)((uli.QuadPart - UNIX_EPOCH) / 10000000L);
  tv->tv_usec = (long)((uli.QuadPart - UNIX_EPOCH) % 10000000L);
}

int
getrusage(int who, struct rusage *rusage)
{
  PROCESS_MEMORY_COUNTERS pmc;
  FILETIME ftCreationTime;
  FILETIME ftExitTime;
  FILETIME ftKernelTime;
  FILETIME ftUserTime;
  HANDLE hProcess;
  BOOL bResult;

  _ASSERTE(who == RUSAGE_SELF);
  _ASSERTE(rusage != NULL);
  hProcess = GetCurrentProcess();
  bResult = GetProcessTimes(hProcess,
							&ftCreationTime,
							&ftExitTime,
							&ftKernelTime,
							&ftUserTime);
  if (bResult == 0)
  {
	PrintLastError("GetProcessTimes");
	DebugBreak();
  }
  bResult = GetProcessMemoryInfo(hProcess, &pmc, sizeof(pmc));
  if (bResult == 0)
  {
	PrintLastError("GetProcessMemoryInfo");
	DebugBreak();
  }
  ZeroMemory(rusage, sizeof(struct rusage));
  filetime_to_timeval(&ftUserTime, &rusage->ru_utime);
  filetime_to_timeval(&ftKernelTime, &rusage->ru_stime);
  rusage->ru_majflt = pmc.PageFaultCount;

  return 0;
}

TIMEVALUE 
run_time(TIMEVALUE tv)
{
  HANDLE hProcess;
  BOOL bResult;
  FILETIME ftCreationTime;
  FILETIME ftExitTime;
  FILETIME ftKernelTime;
  FILETIME ftUserTime;

  hProcess = GetCurrentProcess();
  bResult = GetProcessTimes(hProcess,
							&ftCreationTime,
							&ftExitTime,
							&ftKernelTime,
							&ftUserTime);
  if (bResult == 0)
  {
	PrintLastError("GetProcessTimes");
	DebugBreak();
  }

  filetime_to_timeval(&ftUserTime, tv);
  return(tv);
}

TIMEVALUE 
sys_time(TIMEVALUE tv)
{
  HANDLE hProcess;
  BOOL bResult;
  FILETIME ftCreationTime;
  FILETIME ftExitTime;
  FILETIME ftKernelTime;
  FILETIME ftUserTime;

  hProcess = GetCurrentProcess();
  bResult = GetProcessTimes(hProcess,
							&ftCreationTime,
							&ftExitTime,
							&ftKernelTime,
							&ftUserTime);
  if (bResult == 0)
  {
	PrintLastError("GetProcessTimes");
	DebugBreak();
  }

  filetime_to_timeval(&ftKernelTime, tv);
  return(tv);
}
#else /* _WIN32 */


TIMEVALUE 
run_time(TIMEVALUE tv)
{
  struct rusage rusage;
  getrusage(RUSAGE_SELF, &rusage );
  tv->tv_sec = rusage.ru_utime.tv_sec;
  tv->tv_usec = rusage.ru_utime.tv_usec;
  return(tv);
}

TIMEVALUE 
sys_time(TIMEVALUE tv)
{
  struct rusage rusage;
  getrusage(RUSAGE_SELF, &rusage );
  tv->tv_sec = rusage.ru_stime.tv_sec;
  tv->tv_usec = rusage.ru_stime.tv_usec;
  return(tv);
}
#endif /* _WIN32 */


TIMEVALUE 
elapsed_time(TIMEVALUE tv)
{
  gettimeofday(tv, 0);
  return(tv);
}


double 
time_secs (TIMEVALUE tv)
{
  return(tv->tv_sec + 1e-6*tv->tv_usec);
}

END_NAMESPACE_FREEDIUS
