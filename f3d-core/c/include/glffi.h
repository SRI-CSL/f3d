#ifndef gl_ffi_h
#define gl_ffi_h

#include "matrix-transforms.h"
#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

int handle_gl_errors(char *error_comment);
void glMultMatrixd_transposed(double * m);

void set_2d_to_ndc_matrix (mat4x4 mat_2d_to_window);
double * window_to_ndc_matrix(mat4x4 m);

#if 0
extern "C" {

  // These functions are called from Lisp (and C++).


void cme_glClearColor (double red, double green, double blue, double alpha );
void cme_glClearIndex (double index );
void cme_glPixelZoom (double xfactor, double yfactor );
void transpose_4x4_double (double m1[4][4], double m2[4][4]);
void cme_glBlendColorEXT (double red, double grn, double blu, double alpha);
void cme_glPolygonOffsetEXT (double factor, double bias);
void cme_glClearAccum (double red, double grn, double blu, double alpha);

void transpose_4x4_double (double m1[4][4], double m2[4][4]);
void glMultMatrixd_transposed(double * m);



} // end extern "C"
#endif

END_NAMESPACE_FREEDIUS

#endif // ! gl_ffi_h 
