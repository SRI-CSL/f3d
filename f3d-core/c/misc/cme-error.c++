#include <stdlib.h>
#include <sys/types.h>
#include <stdarg.h>
#include "misc.h"
#include "cme-error.h"

#include <stdio.h>
//#include <mymalloc.h>
#include <signal.h>
#ifndef _WIN32
#include <unistd.h>
#endif

#include "lisp-callback.h"

#include "namespace.h"

#ifdef MINGW
extern "C" long _ftol(double);
extern "C" long _ftol2(double x) { return _ftol(x); }
#endif

BEGIN_NAMESPACE_FREEDIUS


// Once again, a stupid workaround for a wInDOeZ (or allegro)
// deficiency.  No easy way to send C output to the Lisp listener.
// Create a "fake" stderr stream that sends output to a log file...
static FILE* stderrfile = stderr;
static int logging_on = 0;


#define STDERR stderrfile

extern void start_warning_log(char *file) {
  if (!logging_on) {
    FILE *nfile = fopen(file, "w");
    stderrfile = nfile;
  }
  logging_on = 1;
}

class start_warning_log_init {
 public:
  start_warning_log_init(char *file) {
    start_warning_log(file);
  }
};

// use static-initializer?  No, order of initializers is not predictable.
// start_warning_log_init swli = start_warning_log_init("c:/freedius-stderr.log");


extern void stop_warning_log() {
  if (logging_on) {
    fclose(stderrfile);
    stderrfile = stderr;
  }
  logging_on = 0;
}


#ifdef HAVE_EXCEPTIONS
BreakError::BreakError(const char* msg) 
  : msg(msg) {}
#endif // HAVE_EXCEPTIONS

/*
signal a SIGTRAP.  
gdb will allow one to continue from SIGTRAP.
otherwise core dump.
If there is a catch handler for BreakError, a continue
from gdb will pass control to the handler.
*/

extern void default_error_handler (const char* format, va_list args)
{
  fprintf(STDERR, "Error: ");
  vfprintf(STDERR, format, args);
#if defined(_WIN32)
  fprintf(STDERR, "kill() not available under Windows.\n");
#else
  kill(getpid(), SIGTRAP);
#endif
  // continuing from gdb causes throw to a try catch handler.
#if defined(HAVE_EXCEPTIONS)
  throw BreakError("Error");
#endif
}

extern void default_cerror_handler (const char* format, va_list args)
{
  fprintf(STDERR, "Continuable Error: ");
  vfprintf(STDERR, format, args);
   // This needs more smarts:  If we do not have gdb, we want to give the
  // user the option to continue.
#if defined(_WIN32)
  fprintf(STDERR, "kill() not available under Windows.\n");
#else
  kill(getpid(), SIGTRAP);
#endif
  // continuing from gdb causes return 
 
}

extern void default_warning_handler (const char* format, va_list args)
{
  fprintf(STDERR, "Warning: ");
  vfprintf(STDERR, format, args);
}

internal ErrorHandler error_handler = default_error_handler;

ErrorHandler setErrorHandler (ErrorHandler new_handler)
{
  ErrorHandler old_handler = error_handler;
  error_handler = new_handler;
  return old_handler;
}

internal ErrorHandler cerror_handler = default_cerror_handler;

ErrorHandler setCErrorHandler (ErrorHandler new_handler)
{
  ErrorHandler old_handler = cerror_handler;
  cerror_handler = new_handler;
  return old_handler;
}

internal WarningHandler warning_handler = default_warning_handler;

WarningHandler setWarningHandler (WarningHandler new_handler)
{
  WarningHandler old_handler = warning_handler;
  warning_handler = new_handler;
  return old_handler;
}

extern void error (const char* format, ...)
{
  if (error_handler) {
    va_list args;
    va_start(args, format);
    (* error_handler) (format, args);
    va_end(args);
  }
}




typedef void (Lisp_Error_Handler) (char* error_msg);

#if defined(CERROR_CALLBACKS)

/*
It is not generally safe to continue from a callback from C.  The problem is that we sometimes
pass Lisp objects (such as arrays) into C that are allocated in dynamic space.  It an error
callback to Lisp occurs, Lisp might garbage collect objects that are still accessible in C.
*/

/*
These should probably be disabled for all Lisps.
In general it is not safe to resume from a callback to Lisp.
*/

extern void cerror (const char* format, ...)
{
  if (cerror_handler) {
    va_list args;
    va_start(args, format);
    (* cerror_handler) (format, args);
    va_end(args);
  }
}

extern void warning (const char* format, ...)
{
  va_list args;
  va_start(args, format);
  if (warning_handler)
    (* warning_handler) (format, args);

  vfprintf(STDERR, format, args);
  fflush(STDERR);

  va_end(args); 
}

#else // ! CERROR_CALLBACKS

extern void warning (const char* format, ...)
{
  va_list args;
  va_start(args, format);
  vfprintf(STDERR, format, args);
  fflush(STDERR);
  va_end(args); 
}

// This sucks. Really should handle arbitrary length error strings.
#define MAX_ERROR_STRING 1000

#if defined(OLD_ALLEGRO_CALLBACKS)
int lisp_cerror_handler_index;

extern void lisp_cerror_handler (char* format, va_list args)
{
  char error_string[MAX_ERROR_STRING];
  vsnprintf(error_string, MAX_ERROR_STRING, format, args);
  Lisp_Error_Handler *callback_addr 
    = (Lisp_Error_Handler *) lisp_call_address(lisp_cerror_handler_index);
  (* callback_addr) (error_string);
}

int lisp_warning_handler_index;

extern void lisp_warning_handler (char* format, va_list args)
{
  char error_string[MAX_ERROR_STRING];
  vsnprintf(error_string, MAX_ERROR_STRING, format, args);
  Lisp_Error_Handler *callback_addr 
    = (Lisp_Error_Handler *) lisp_call_address(lisp_warning_handler_index);
  (* callback_addr) (error_string);
}

#endif // OLD_ALLEGRO_CALLBACKS

#endif // ! CERROR_CALLBACKS



extern void message (const char* format, ...)
{
  va_list args;
  va_start(args, format);
  vfprintf(STDERR, format, args);
  fflush(STDERR);
  va_end(args);
}

// *********************  LISP ERROR CALLBACKS  *********************

#define lisp_error_callback FREEDIUS_GLOBAL(lisp_error_callback)

extern "C" {

DEFVOIDCALLBACK(void, lisp_error_callback, (const char* error_string), (error_string));

} // end extern "C" 


extern void lisp_error_handler (const char* format, va_list args)
{
  // This sucks. Really should handle arbitrary length error strings.
  //  int vsnprintf(char *buffer, size_t count, const char *format, va_list argptr);
  char error_string[MAX_ERROR_STRING];
  // Something is wrong with this - not all MSVC installations know about vsnprintf:
#ifdef _WIN32
  memcpy(error_string, format, strlen(format)+1);
#else
  vsnprintf(error_string, MAX_ERROR_STRING, format, args);
#endif
  lisp_error_callback(error_string); // lisp_error_callback never returns
}

extern "C" {
void FREEDIUS_GLOBAL(setLispErrorHandlers) ()
{
  error_handler = lisp_error_handler;
#if defined(CERROR_CALLBACKS)
  cerror_handler = lisp_cerror_handler;
  warning_handler = lisp_warning_handler;
#endif
}
} // end extern "C" 

END_NAMESPACE_FREEDIUS
