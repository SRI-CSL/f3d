// This file is compiled and linked by c/links/Makefile

// It must be conditionalized according to architecture

#if !defined(_WIN32) && !defined(AGL) && !defined(COCOA)

#include <X11/Xlib.h>
#include <GL/glx.h>
#include "lisp-callback.h"
#include "namespace.h"

extern "C" {

DEFVOIDCALLBACK(void, FREEDIUS_GLOBAL(x11_lisp_error_handler), 
		(char* error_text,
		 int error_code,
		 int request_code,
		 int minor_code,
		 unsigned long resourceid
		 //XID resourceid
		 ),
		(error_text, error_code, request_code, minor_code, resourceid));

#define x11_lisp_error_handler FREEDIUS_GLOBAL(x11_lisp_error_handler)

//static 
int x11_lisp_error_handler0 (Display *display, XErrorEvent *error)
{
  #define MAXLEN 1200
  char msg[MAXLEN];
  XGetErrorText (display, error->error_code, msg, MAXLEN);
  x11_lisp_error_handler(msg, error->error_code, error->request_code,
			  error->minor_code, error->resourceid);
  return 0;
}

void FREEDIUS_GLOBAL(set_x11_lisp_error_handler) (void)
{
  XSetErrorHandler(&x11_lisp_error_handler0);
}

void
FREEDIUS_GLOBAL(XSetBackingStore) (Display *display, Window window, int flag)
{
  XSetWindowAttributes attr;
  attr.backing_store = flag;
  XChangeWindowAttributes(display, window, CWBackingStore, &attr);
}


int FREEDIUS_GLOBAL(glXGetConfig)(Display *dpy, XVisualInfo *vis, int attrib)
{
  int value;
  int error_code = glXGetConfig(dpy, vis, attrib, &value);
  if (error_code)
    return (-1000 - error_code);
  else return (value);
}    


#define ALL_EVENTS_MASK \
    KeyPressMask|KeyReleaseMask|ButtonPressMask|ButtonReleaseMask|	\
    EnterWindowMask|LeaveWindowMask|PointerMotionMask|ExposureMask|	\
    VisibilityChangeMask|FocusChangeMask|PropertyChangeMask|ColormapChangeMask


Window  FREEDIUS_GLOBAL(XCreateWindow) (Display *dpy, Window parent, int width, int height,
			   XVisualInfo *visinfo)
{
  Colormap cmap;
  XSetWindowAttributes swa;
  // cmap = get_rgb_colormap( dpy, scrnum, visinfo );
  // swa.colormap = cmap;
  swa.border_pixel = 0;
  swa.event_mask = ALL_EVENTS_MASK;
  return XCreateWindow(dpy, parent, 0, 0, width, height, 
		       0, visinfo->depth, 
		       InputOutput, visinfo->visual,
		       //CWBorderPixel | CWColormap | CWEventMask,
		       CWBorderPixel | CWEventMask,
		       &swa);
}

Visual* FREEDIUS_GLOBAL(visinfo_visual) (XVisualInfo *visinfo)
{
  return visinfo->visual;
}

VisualID FREEDIUS_GLOBAL(visinfo_visualid) (XVisualInfo *visinfo)
{
  return visinfo->visualid;
}


GLXFBConfig getGLXFBConfig_from_array (GLXFBConfig configs[], int index)
{
  return configs[index];
}


} // end extern "C"

#endif //!defined(_WIN32) && !defined(CGL)
