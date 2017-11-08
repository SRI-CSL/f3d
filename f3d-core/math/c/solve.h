#ifndef __solve_h
#define __solve_h

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

#ifdef __cplusplus
extern "C" {
#endif

// These functions can be called from Lisp.

int math_invert_double_matrix (double *mat, int n, double *mat_inverse);
int math_ludcmp(double *a, int n, int *indx, double *d, double tiny, double *vv);
int math_lubksb(double *a,int n,int *indx, double b[], double tiny);
int math_invert (double *a, int n, double *inv, int *indx, double tiny, double *col);
void math_tred2(double *a, int n, double *d, double *e, int eignvectors_p);
void math_tqli(double *d, double *e, int n, double *z);
void math_svdcmp(double *a, int m, int n, double *w, double *v, double *rv1);
void math_svbksb(double *u, double *w, double *v, int m, int n, double *b, double *x, double *tmp);
double math_gammln(double xx);
double *math_invert_matrix (double *mat, int n, double *inv);
int math_invert_homogeneous_4x4_matrix (double m[4][4], double minv[4][4]);
int math_invert_3x3_matrix (double m[3][3], double inv[3][3]);

#ifdef __cplusplus
}
#endif


#ifdef __cplusplus
double *math_invert_matrix (double *mat, int n);
#endif

END_NAMESPACE_FREEDIUS

#endif /* ! __solve_h */
