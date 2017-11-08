
/*    Should convert to C++?  */

/* Potential problems with $FREEDIUS/c/includes versions of tcl.h tk.h
   mismatching the libraries actually used.
*/


#include <tcl.h>
#include <tk.h>

#include "lisp-callback.h"

 /* Just to get FREEDIUS_GLOBAL macro */
#include "namespace.h"

#include "lisp-ffi-macros.h"

/* #define MKVAR(name) int C_##name = (int)name; */

MKVAR(TCL_DONT_WAIT)
MKVAR(TCL_WINDOW_EVENTS)
MKVAR(TCL_FILE_EVENTS)
MKVAR(TCL_TIMER_EVENTS)
MKVAR(TCL_IDLE_EVENTS)
MKVAR(TCL_ALL_EVENTS)

MKVAR(TCL_STATIC)
MKVAR(TCL_VOLATILE)
MKVAR(TCL_DYNAMIC)

MKVAR(TCL_LINK_INT)
MKVAR(TCL_LINK_DOUBLE)
MKVAR(TCL_LINK_BOOLEAN)
MKVAR(TCL_LINK_STRING)

MKVAR(TCL_OK)
MKVAR(TCL_ERROR)
MKVAR(TCL_GLOBAL_ONLY)

int FREEDIUS_GLOBAL(Tcl_PollEvent)(void)
{
  Tcl_Time timer;
  timer.sec = 0;
  timer.usec = 0;
  return Tcl_WaitForEvent(&timer);
}

void FREEDIUS_GLOBAL(Tcl_SetMaxBlockTime)(int sec, int usec)
{
  Tcl_Time timer;
  timer.sec = sec;
  timer.usec = usec;
  Tcl_SetMaxBlockTime(&timer);
}

int FREEDIUS_GLOBAL(Tcl_DoOneEventWithTimeout)(int flags, int sec, int usec)
{
  Tcl_Time timer;
  timer.sec = sec;
  timer.usec = usec;
  Tcl_SetMaxBlockTime(&timer);
  return Tcl_DoOneEvent(flags);
}

DEFCALLBACK(int, FREEDIUS_GLOBAL(invoke_tcl_command), 
	    (int id, void *tcl_interp, int argc, char **argv),
	    (id, tcl_interp, argc, argv));

DEFCALLBACK(int, FREEDIUS_GLOBAL(delete_tcl_command), (int id), (id));

char* FREEDIUS_GLOBAL(invoke_tcl_command_arg_ref) (char **argv, int i)
{
  return argv[i];
}

/* The myriad of include dependencies and X conflicts are making it
   difficult to pull out the "native" NSView corresponding to a Tk
   window.  This is needed for OpenGL.
*/


/*  *****************************  dead CODE IN REMAINDER OF FILE  ************************** */

#define ALLEGRO_BUGFIX1 1

#if defined(HAVE_ALLEGRO) && defined(ALLEGRO_BUGFIX1)

#define internal static

#include <setjmp.h>
#include <signal.h>

internal jmp_buf catch_sigint;
internal jmp_buf catch_sigalrm;
internal void (*old_sigint_handler)(int);
internal void (*old_sigalrm_handler)(int);

internal void restore_signal_handlers (void)
{
  if (old_sigint_handler)
    signal(SIGINT, old_sigint_handler); // restore the original sigint handler
  if (old_sigalrm_handler)
    signal(SIGALRM, old_sigalrm_handler); // restore the original sigalrm handler
}
 
internal void sigint_handler(int signum)
{
    restore_signal_handlers();
    sigsetmask(siggetmask() || sigmask(SIGINT));
    longjmp(catch_sigint, -1);
} 

internal void sigalrm_handler(int signum)
{
    restore_signal_handlers();
    sigsetmask(siggetmask() || sigmask(SIGALRM));
    longjmp(catch_sigalrm, -2);
} 

/* A version of Tcl_DoOneEvent with sigint enabled */
int FREEDIUS_GLOBAL(Tcl_DoOneEvent) (int flags)
{
  auto int return_value;
  void (*handler)(int signum);
  old_sigint_handler = 0;
  old_sigalrm_handler = 0;
  if ((return_value = setjmp(catch_sigint)) != 0) {
    //fprintf(stderr, "Tcl_DoOneEvent interrupted by SIGINT\n");
    return return_value; // We get here on sigint 
  }

  if ((return_value = setjmp(catch_sigalrm)) != 0) {
    return return_value; // We get here on sigalrm
  }
  handler = signal(SIGINT, sigint_handler);
  if (handler != sigint_handler) old_sigint_handler = handler;
  handler = signal(SIGALRM, sigalrm_handler);
  if (handler != sigalrm_handler) old_sigalrm_handler = handler;
  return_value = Tcl_DoOneEvent(flags);
  restore_signal_handlers();
  return return_value;
} 

#endif /* HAVE_ALLEGRO */

#if 0 // I isn't clear that these are useful, since we do not 
      // have a pointer to the tkwin struct.

typedef void *tkWin;

Display *FREEDIUS_GLOBAL(tk_Display) (tkWin tkwin)
{
  return Tk_Display(tkwin);
}

int FREEDIUS_GLOBAL(tk_ScreenNumber) (tkWin tkwin)
{
  return Tk_ScreenNumber(tkwin);
}

char *FREEDIUS_GLOBAL(tk_PathName) (tkWin tkwin)
{
  return Tk_PathName(tkwin);
}

Window FREEDIUS_GLOBAL(tk_WindowId) (tkWin tkwin)
{
  return Tk_WindowId(tkwin);
}

Visual* FREEDIUS_GLOBAL(tk_visual) (tkWin tkwin)
{
  return Tk_Visual(tkwin);
}

#endif // not useful

#if 0 /* dead ? */

#include <stdio.h>
#include <string.h>

/* LHQ: I do not remember why this was once needed, but it appears to be unused now. */

#ifdef CODEWARRIOR
char *invoke_tcl_command_arg_ref (int i){}
#else
#ifdef DARWIN
/* char *invoke_tcl_command_arg_ref (int i) __attribute__ ((weak)); */
#else
char *invoke_tcl_command_arg_ref (int i) __attribute__ ((weak));
#endif
#endif

#endif /* dead ? */


#ifdef NEVER

#include <errno.h>
/* #include <unistd.h> */
#include <sys/select.h>
#include <sys/time.h>

int
my_select(int width, fd_set *ins, fd_set *outs, fd_set *errs, struct timeval *timeout)
     {int r, err;
  
retry:
 r=select(width, ins, outs, errs, timeout);
 err = errno;
  
 /* if (r == -1 && errno == EINTR) goto retry; */
      
 if (r == -1 && errno != EINTR) 
   {perror("select");
    printf("select returns: %d, with error %d\n", r, err);
  }	
 return(r);
}	



#include <tcl.h>
#include <tk.h>

int Tk_file_descriptor (Tcl_Interp *interp)
{ 
  Tk_Window win = Tk_MainWindow(interp);
  Display *display;
  display = Tk_Display(win);
  return (XConnectionNumber(display));
}

#endif /* NEVER */


#if 0 /* Unused */

#include <stdarg.h>

char* va_Tcl_Merge (int argc, ...)
{
  va_list strings;
  char **argv;
  va_start(strings, argc);
#if defined(__GNUC_VA_LIST) || defined(SOLARIS)
  argv = (char **) va_arg(strings, char **);
#else
  argv = (char **) &(va_arg(strings, char **));
#endif
  return(Tcl_Merge(argc, argv));
}
#endif /* unused */


/* number of bits per byte.  Defined on some systems, not on others,
 * but basically always 8: */

#ifndef NBBY
#define NBBY 8
#endif /* Unused */

#if 0 /* Unused */


/* tkDisplayList has been replaced by TkGetDisplayList() in the code
below.  -CC 9/4/00 */

#ifdef _WIN32
typedef long fd_mask;
#endif /* _WIN32 */

char* tcl_interp_result (Tcl_Interp *interp)
{ 
  return(interp->result);
}


int FREEDIUS_GLOBAL(set_tk_display_connection_fdmask) (fd_mask *readMask, int mask_size_words)
{
  TkDisplay *dispPtr;
  int fd, index, bit;
  int numFdBits = 0;
  for (dispPtr = TkGetDisplayList(); dispPtr != NULL;
	 dispPtr = dispPtr->nextPtr) {
	fd = ConnectionNumber(dispPtr->display);
	index = fd/(NBBY*sizeof(fd_mask));
	bit = 1 << (fd%(NBBY*sizeof(fd_mask)));
	if (index  >= mask_size_words) return(-1);
	readMask[index] |= bit;
	if (numFdBits <= fd) {
	    numFdBits = fd+1;
	}}
  return(numFdBits);
}

#ifndef _WIN32
#include <strings.h>
#endif
#include <string.h>

char *copy_to_tcl_string (char *string)
{
  int n = 1+strlen(string);
  char *tcl_string = Tcl_Alloc(n);
  memcpy(tcl_string, string, n);
  return tcl_string;
}

Window tk_xwindow (Tcl_Interp *interp, char* widget_name)
{
  TkWindow *winPtr = (TkWindow *) Tk_NameToWindow(interp, widget_name, 0);
  return Tk_WindowId(winPtr);
}

Display* tk_xdisplay (Tcl_Interp *interp, char* widget_name)
{
  TkWindow *winPtr = (TkWindow *) Tk_NameToWindow(interp, widget_name, 0);
  return Tk_Display(winPtr);
}

#endif /* Unused */


#ifdef DEBUG
void FREEDIUS_GLOBAL(set_string_array_element) (char **arr, char *str, int index)
{arr[index]=str;
}

char * FREEDIUS_GLOBAL(get_string_array_element) (char **arr, int index)
{return(arr[index]);
}

int FREEDIUS_GLOBAL(foreign_string_length)  (char *str)
{return (strlen(str));
}

void FREEDIUS_GLOBAL(print_arg) (int arg)
{ fprintf(stderr, "%d\n", arg);
}

void FREEDIUS_GLOBAL(print_string_array) (char **arr, int n)
{ 
  int i;
  for(i=0; i<n; i++) fprintf(stderr, "%d   %s\n", strlen(arr[i]), arr[i]);
}


#endif /* DEBUG */
