//#define FLOAT double
//#include <math.h>
//#include <stdio.h>
//#include <stdlib.h>

// matrices.h wraps extern "C" around the Lisp accessible functions
#include "matrices.h" 
#include "namespace.h"
//#include "cme-error.h"
#include <mymalloc.h>

#if !defined(F3DMODULAR)
#include "misc.h"
#else
#include <string.h>
BEGIN_NAMESPACE_FREEDIUS
#define XALLOC(type,nelems)((type *)malloc((nelems)*sizeof(type)))
#define xxfree(p) free(p)
END_NAMESPACE_FREEDIUS
#endif

#ifndef _WIN32
//#include <strings.h>
#endif

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

void math_transpose_matrix (int nrows, int ncols, double *a, double *b)
{ 
  int spanb = nrows;
  int i, j, bindex;
  int aindex = 0;
  double tmp;
  if (a == b) 
    /* Transpose in place.  Must have nrows==ncols  */
    for (i = 0; i< nrows; i++) 
      for (j = 0, aindex=i*ncols, bindex=i; j < i; j++, aindex++, bindex+=spanb) {
	tmp = b[bindex];
	b[bindex] = a[aindex];
	a[aindex] = tmp;
      }
  else 
    for (i = 0; i< nrows; i++) 
      for (j = 0, bindex=i; j < ncols; j++, aindex++, bindex+=spanb)
	b[bindex] = a[aindex];
  
}

// The use of stack allocation of matrices is avoided because of the possible (probable) limits
// to the stack size.

void math_matrix_times_vector (int n1, int n2, double *a, double *b, double *c)
{ 
  if (c == b) {
    //double c_tmp[n1];
    double *c_tmp = XALLOC(double, n1);
    
    math_matrix_times_vector(n1, n2, a, b, c_tmp);
    memcpy(c, c_tmp, n1*sizeof(double));
    xxfree(c_tmp);
    return;
  }
    
  int span1=n2;			/* a.ncols */
  int i1, i, k, ka;
  double sum;
  double zero = 0.0;
  for (i = 0, i1 = 0; i < n1 ; i++, i1 += span1)
    {
      sum = zero;
      for (k = 0, ka = i1; k < n2; k++, ka++)
	sum += a[ka] * b[k];
      c[i] = sum;
    }
}

/* a[n1][n2] * b[n2][n3] => c[n1][n3]  */
void math_multiply_matrices (int n1, int n2, int n3, double *a, double *b, double *c)
{ 
  if (c==a || c==b) {
    // double c_tmp[n1*n3];
    double *c_tmp = XALLOC(double, n1*n3);
    math_multiply_matrices(n1, n2, n3, a, b, c_tmp);
    memcpy(c, c_tmp, n1*n3*sizeof(double));
    xxfree(c_tmp);
    return;
  }
  int span1=n2;
  int span2=n3;
  int span3=n3;
  double *blastrow = b+(n2-1)*span2;
  int i1, i3, j, k1, k2, cnt1, k;
  double sum;
  double zero = 0.0;
  for (cnt1 = 0, i1 = 0, i3 = 0; cnt1 < n1 ; cnt1++, i1 += span1, i3 += span3) {
    for (j = 0; j < n3; j++) {
      double *ap = a+i1; 
      sum = zero;
      for (k = n2-1, k2 = j; k >= 0; k--, k2 -= span2)
	sum += ap[k] * blastrow[k2];
      c[i3+j] = sum;
    }
  }
}

/* a[n1][n2] * b[n2][n3] => c[n1][n3]  
Attempt to minimize page thrashing and cache misses.
Based on LAPACK implementation.

For a 1000x1000 matrix this is about 4x faster than other version.
*/
extern void math_multiply_matrices_v2 (int n1, int n2, int n3, double *a, double *b, double *c)
{ 
  if (c==a || c==b) {
    //double c_tmp[n1*n3];
    double *c_tmp = XALLOC(double, n1*n3);
    math_multiply_matrices_v2(n1, n2, n3, a, b, c_tmp);
    memcpy(c, c_tmp, n1*n3*sizeof(double));
    xxfree(c_tmp);
    return;
  }
  int span1=n2;
  int span2=n3;
  int span3=n3;
  int i1, i3, j, k1, k2, cnt1, cnt2, i4, k3;
  double sum;
  double zero = 0.0;
  double aik;
  for (cnt1 = 0, i1 = 0, i3 = 0; cnt1 < n1 ; cnt1++, i1 += span1, i3 += span3) {
    for (j = 0; j < n3; j++) c[i3+j] = zero;
    for (cnt2 = n2-1, k1 = i1, k2 = 0; cnt2 >= 0; cnt2--, k1++, k2 += span2) {
	aik = a[k1];
	if (aik != 0) {
#if 0	
	for (j = 0, i4=i3, k3=k2; j < n3; j++, k3++, i4++) 
	  c[i4] += aik * b[k3];
#else
	double *cp = c+i3;
	double *bp = b+k2;
	for (j = 0; j < n3; j++) 
	  cp[j] += aik * bp[j];
#endif
	}
    
    }}
}

/* a[n1][n2] * b[n3][n2] => c[n1][n3] */
void math_multiply_matrices_transposed2 (int n1, int n2, int n3, 
					 double *a, double *b, double *c)
{ 
  if (c==a || c==b) {
    //double c_tmp[n1*n3];
    double *c_tmp = XALLOC(double, n1*n3);
    math_multiply_matrices_transposed2(n1, n2, n3, a, b, c_tmp);
    memcpy(c, c_tmp, n1*n3*sizeof(double));
    xxfree(c_tmp);
    return;
  }
  
  int span1=n2;
  int span2=n2;
  int span3=n3;
  int i, i1, i3, j, j2, k;
  double sum;
  double zero = 0.0;
  for (i = 0, i1 = 0, i3 = 0; i < n1 ; i++, i1 += span1, i3 += span3) {
    for (j = 0, j2=0; j < n3; j++, j2+=span2) {
      sum = zero;
      double *ap = a+i1;
      double *bp = b+j2;
      for (k = 0; k < n2; k++)
	sum += ap[k] * bp[k];
      c[i3+j] = sum;
    }
  }
}


/* a[n2][n1] * b[n2][n3] => c[n1][n3] */
extern void math_multiply_matrices_transposed1 (int n1, int n2, int n3, 
					   double *a, double *b0, double *c)
{ 
  if (c==a || c==b0) {
    //double c_tmp[n1*n3];
    double *c_tmp = XALLOC(double, n1*n3);
    math_multiply_matrices_transposed1(n1, n2, n3, a, b0, c_tmp);
    memcpy(c, c_tmp, n1*n3*sizeof(double));
    xxfree(c_tmp);
    return;
  }
  int span1=n1;
  int span2=n3;
  int span3=n3;
  int i, i1, i3, j, k1, cnt2;
  double sum;
  double zero = 0.0;
  double *b;
  for (i=0, i1 = 0, i3 = 0; i < n1 ; i++, i1 += span1, i3 += span3) {
    for (j = 0; j < n3; j++) {
      sum = zero;
      for (cnt2 = n2-1, k1 = i, b=b0+j; cnt2 >= 0; cnt2--, k1+=span1, b+=span2)
	sum += a[k1] * *b;
      c[i3+j] = sum;
    }
  }
}


void math_add_matrices (int n1, int n2, double *a, double *b, double *c)
{ int i, n;
  n = n1*n2; /* Assumes a, b, and c have same dims */
  for (i=0; i<n; i++)
    c[i] = a[i]+b[i];
}


END_NAMESPACE_FREEDIUS
