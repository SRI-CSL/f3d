#ifndef __f3d_math_h
#define __f3d_math_h
 
#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

#ifdef __cplusplus
extern "C" {
#endif

// These functions can be called from Lisp.

void math_transpose_matrix (int nrows, int ncols, double *a, double *b);
void math_add_matrices (int n1, int n2, double *a, double *b0, double *c);
void math_matrix_times_vector (int n1, int n2, double *a, double *b, double *c);
void math_multiply_matrices (int n1, int n2, int n3, double *a, double *b, double *c);
void math_multiply_matrices_v2 (int n1, int n2, int n3, double *a, double *b, double *c);
void math_multiply_matrices_transposed2 (int n1, int n2, int n3, double *a, double *b, double *c);
void math_multiply_matrices_transposed1 (int n1, int n2, int n3, double *a, double *b, double *c);
double *math_invert_matrix (double *mat, int n, double *inv);

#ifdef __cplusplus
}
#endif

END_NAMESPACE_FREEDIUS

#endif /* !  __f3d_math_h */
