// This file is compiled and linked by c/links/Makefile

// It must be conditionalized according to architecture

#if defined(_WIN32)

#include <windows.h>
#include <winnt.h>
#include <assert.h>

// some of these are probably unnecessary
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

// *************   chooseAndSetPixelformat  *************

struct glRenderingSpec
{
  int RgbaFlag;		/* configuration flags (ala GLX parameters) */
  int ColorBits;
  int DoubleFlag;
  int DepthSize;
  int AccumBits;
  int AlphaSize;
  int StencilSize;
  int OverlayFlag;
  int StereoFlag;
  int AuxNumber;
  int Accel;
};

extern "C" {

struct glRenderingSpec *
make_wglRenderingSpec()
{ return (struct glRenderingSpec *) malloc(sizeof(glRenderingSpec));
}

} // end extern "C"

void set_pixelformatdescriptor (PIXELFORMATDESCRIPTOR *pfd, 
				struct glRenderingSpec *spec)
{
  pfd->nSize = sizeof(PIXELFORMATDESCRIPTOR);
  pfd->nVersion = 1;
  pfd->dwFlags = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL;

  if (spec->DoubleFlag) 
    pfd->dwFlags |= PFD_DOUBLEBUFFER;
  
  if (spec->StereoFlag) 
    pfd->dwFlags |= PFD_STEREO;

  pfd->cColorBits = spec->ColorBits;

  pfd->iPixelType = spec->RgbaFlag ? PFD_TYPE_RGBA : PFD_TYPE_COLORINDEX;
  /* Alpha bitplanes are not supported in the current generic OpenGL
   * implementation, but may be supported by specific hardware devices.
   */
  pfd->cAlphaBits = spec->AlphaSize;

  pfd->cAccumBits = spec->AccumBits;

  pfd->cDepthBits = spec->DepthSize;

  pfd->cStencilBits = spec->StencilSize;

  /* Auxiliary buffers are not supported in the current generic OpenGL
   * implementation, but may be supported by specific hardware devices.
   */
  pfd->cAuxBuffers = spec->AuxNumber;
  pfd->iLayerType = PFD_MAIN_PLANE;
}



void read_pixelformatdescriptor(PIXELFORMATDESCRIPTOR *pfd, 
				struct glRenderingSpec *spec)
{
  spec->DoubleFlag = (pfd->dwFlags & PFD_DOUBLEBUFFER) ? 1 : 0;
  spec->StereoFlag = (pfd->dwFlags & PFD_STEREO) ? 0 : 1;   // ??? isn't this the other way around?
  if (pfd->dwFlags & PFD_GENERIC_FORMAT) spec->Accel = 2;
  else spec->Accel = 0;

  if (pfd->dwFlags & PFD_GENERIC_ACCELERATED) spec->Accel |= 1;

  spec->ColorBits = pfd->cColorBits;

  spec->RgbaFlag = (pfd->iPixelType == PFD_TYPE_RGBA) ? 1 : 0;

  /* Alpha bitplanes are not supported in the current generic OpenGL
   * implementation, but may be supported by specific hardware devices.
   */
  spec->AlphaSize = pfd->cAlphaBits;

  spec->AccumBits = pfd->cAccumBits;
  
  spec->DepthSize = pfd->cDepthBits;

  spec->StencilSize = pfd->cStencilBits;

  /* Auxiliary buffers are not supported in the current generic OpenGL
   * implementation, but may be supported by specific hardware devices.
   */
  spec->AuxNumber = pfd->cAuxBuffers;

}

extern "C" {

  // return 0 on failure, the chosed pixel format on success.
  // The contents of spec are set to the actual values chosen.
  // 
int
FREEDIUS_GLOBAL(chooseAndSetPixelformat) (HDC GLHdc, struct glRenderingSpec *spec)
{
  PIXELFORMATDESCRIPTOR pfd;
  int pixelformat;

  set_pixelformatdescriptor(&pfd, spec);
  
  if ((pixelformat = ChoosePixelFormat(GLHdc, &pfd)) == 0 )
    return 0;
  if (SetPixelFormat(GLHdc, pixelformat, &pfd) == FALSE) 
    return 0;

  /* Get the actual pixel format */
  DescribePixelFormat(GLHdc, pixelformat, sizeof(pfd), &pfd);

  // Modify the values in spec according to what was actually obtained
  read_pixelformatdescriptor(&pfd, spec);

  
  return(pixelformat);
}

} // end extern "C"




extern "C" {

static int LastGetDCError=0;

// I am not sure whether GetDC is a function or a macro.
// Thus I am wrapping the call with a function to be called from Lisp.
HDC FREEDIUS_GLOBAL(GetDC) (HWND hwnd)
{
  return GetDC(hwnd);
}


HDC FREEDIUS_GLOBAL(GetDCEx) (HWND hwnd, int flags) {
  HDC rc;
  HRGN hrgn = CreateRectRgn(0,0,0,0);
  int regionType = GetWindowRgn(hwnd, hrgn);

  if (flags == 1) rc = GetDCEx(hwnd, hrgn, DCX_WINDOW);
  else rc = GetDCEx(hwnd, hrgn, DCX_PARENTCLIP);

  LastGetDCError = GetLastError();
  return rc;
}



int FREEDIUS_GLOBAL(GetDCExError) () { return LastGetDCError; }


int FREEDIUS_GLOBAL(GetPixelFormat) (HDC hdc, struct glRenderingSpec *spec) {
  PIXELFORMATDESCRIPTOR pfd;
  int pixelformat;
  pixelformat = GetPixelFormat(hdc);
  if (pixelformat == 0) {
    return GetLastError();
  }

  DescribePixelFormat(hdc, pixelformat, sizeof(pfd), &pfd);

  // Modify the values in spec according to what was actually obtained
  read_pixelformatdescriptor(&pfd, spec);
  return pixelformat;
}
  


HGLRC FREEDIUS_GLOBAL(CreateContextFromID) (HWND hwnd) {
  HDC hdc;
  hdc = GetDC(hwnd);
  return wglCreateContext(hdc);
}



}





END_NAMESPACE_FREEDIUS

#endif // defined(_WIN32)


