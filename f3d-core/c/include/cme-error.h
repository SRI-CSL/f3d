// -*- Mode: C++; c-basic-offset: 2 -*-

// $Id: cme-error.h,v 1.3.8.2 2009/04/11 18:05:50 quam Exp $

#ifndef __break_h
#define __break_h

#include <stdarg.h>

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

#ifdef HAVE_EXCEPTIONS
class BreakError {
public:
  char *msg;
  BreakError(char* msg);
};
#endif  // HAVE_EXCEPTIONS

void error (const char* format, ...);
void cerror (const char* format, ...);
void warning (const char* format, ...);
void message (const char* format, ...);
void tprintf(int, const char*, ...);

/* Macro to allow conditional expansion of trace output. */
/* Careful: stdio.h on RedHat 9 defines dprintf. */
#ifndef NO_TRACE
#define dprintf(x) tprintf x
#else
#define dprintf(x)
#endif

typedef void (*ErrorHandler)(const char* fmt, va_list ap);
typedef void (*WarningHandler)(const char* fmt, va_list ap);

ErrorHandler setErrorHandler (ErrorHandler handler); 
ErrorHandler setCErrorHandler (ErrorHandler handler); 
WarningHandler setWarningHandler (WarningHandler handler);

END_NAMESPACE_FREEDIUS

#endif /* ! __break_h */ 
