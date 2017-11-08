/*
This file contains modified versions of the Numerical Recipes linear systems
functions.


    ludcmp, lubksb   LU Decomposition and backsubstitution

    invert           matrix inversion using ludcmp & lubksb

    tred2            Householder reduction of a real symmetric matrix

    tqli             QL Algorithm for computing Eigenvalues and Eigenvectors
                     of a real, symmetric tridiagonal matrix.  Use tred2 to genrate
		     inputs.

    svdcmp, svbksb   Singular Value Decomposition and backsubstitution

    All of these functions have been converted to use single-dimension arrays with ZERO-BASED
INDEXING.  This makes it possible to use the routines with Lisp arrays without converting back and
forth between NR's array of arrays with ONE-BASED INDEXING.


Also included in this file are some unrelated functions:

    gammaln          Lanczos approximation to the ln Gamma Function

    interfaces to invert

    3x3 matrix ops:  determinant, inversion, matrix_times_vector
    determinant of s 3x3 matrix

    inversion of a 4x4 homogeneous matrix

   


TODO:  
       Split this file into NR solvers vs other stuff.
       Change file name to matrix-solvers.c++ and move 3x3 matrix stuff to matrix.c++.
       Move gammaln to a new file math-misc.

*/

#include <math.h>  // need sqrt
#include <stdio.h> // need fprintf
#include <mymalloc.h>

// solve.h wraps extern "C" around the Lisp accessible functions
#include "solve.h"
#include "namespace.h"

#if !defined(F3DMODULAR)
#include "misc.h"
#else
#include <string.h> // need memset
BEGIN_NAMESPACE_FREEDIUS
#define XALLOC(type,nelems)((type *)malloc((nelems)*sizeof(type)))
#define xxfree(p) free(p)
END_NAMESPACE_FREEDIUS
#endif


BEGIN_NAMESPACE_FREEDIUS

#ifndef MATH_NRERROR

int nerror_verbose = 0;

// FIXME:   should use Lisp warning or cerror callback
static void
math_nrerror (const char *mess) {
  if (nerror_verbose)
    fprintf(stderr, mess);
}

#endif

// MAX and MIN are defined in minmax.h
#ifndef MAX
template <class FLOAT>
inline FLOAT MAX(FLOAT x, FLOAT y)
{
  if (x > y)
    return x;
  else return y;
}
#endif

#ifndef MIN
template <class FLOAT>
inline FLOAT MIN(FLOAT x, FLOAT y)
{
  if (x < y)
    return x;
  else return y;
}
#endif

template <class FLOAT>
inline FLOAT ABS (FLOAT x)
{
  return ((x<0.0) ? -x : x);
}

// LU DECOMPOSITION

template <class FLOAT>
int lu_decompose(FLOAT *a, int n, int *indx, FLOAT *d, FLOAT tiny, FLOAT *vv)
{
   int i,ni,imax, nimax,j,nj,k,nk;
   FLOAT big,dum,sum,temp;
   *d=1.0;
   for (i=0, ni=0; i<n; i++, ni+=n) {
      big=0.0;
      for (j=0; j<n; j++)
	 if ((temp=ABS(a[ni+j])) > big) big=temp;
      if (big<=tiny) {
	 math_nrerror("lu_decompose (1): Singular matrix");
	 return(0);
      }
      vv[i]=1.0/big;
   }
   for (j=0, nj=0; j<n; j++, nj+=n) {
      for (i=0, ni=0; i<j; i++, ni+=n) {
	 sum=a[ni+j];
	 for (k=0,nk=0; k<i; k++, nk+=n) sum -= a[ni+k]*a[nk+j];
	 a[ni+j]=sum;
      }
      big=0.0;
      for (i=j, ni=nj; i<n; i++, ni+=n) {
	 sum=a[ni+j];
	 for (k=0, nk=0; k<j; k++, nk+=n)
	    sum -= a[ni+k]*a[nk+j];
	 a[ni+j]=sum;
	 if ( (dum=vv[i]*ABS(sum)) >= big) {
	    big=dum;
	    imax=i;
	 }
      }
      if (j != imax) {
	nimax=n*imax;
	for (k=0; k<n; k++) {
	    dum=a[nimax+k];
	    a[nimax+k]=a[nj+k];
	    a[nj+k]=dum;
	 }
	 /*   nrlib sources seem to be in error here
              *d = (imax-j)%2 ? *d:-(*d);  
         */
         *d = - (*d);
	 vv[imax]=vv[j];
      }
      indx[j]=imax;
#if 0
      if (a[nj+j] == 0.0) a[nj+j]=tiny;
#else
      if (ABS(a[nj+j])<=tiny) {
	char str[100];
	sprintf(str,"lu_decompose (2): Singular matrix, %g", a[nj+j]);
	math_nrerror(str);
	return(0);
      }
#endif
      if (j != n-1) {
	dum=1.0/(a[nj+j]);
	 for (i=j+1, ni=nj+n; i<n ;i++, ni+=n) 
	   a[ni+j] *= dum;
      }
   }
   return(1);
}


template <class FLOAT>
int lu_backsubstitute(FLOAT *a, int n, int *indx, FLOAT b[], FLOAT tiny)
{
   int i,ni,ii= -1,ip,j;
   FLOAT sum,a_ii;

   for (i=0,ni=0; i<n; i++,ni+=n) {
      ip=indx[i];
      sum=b[ip];
      b[ip]=b[i];
      if (ii>=0)
	for (j=ii;j<i;j++) sum -= a[ni+j]*b[j];
      else if (sum) ii=i;
      b[i]=sum;
   }
   for (i=n-1,ni=n*i ;i>=0; i--, ni=ni-n) {
      a_ii=a[ni+i];
      if(ABS(a_ii)>tiny){
	 sum=b[i];
	 for (j=i+1;j<n;j++) sum -= a[ni+j]*b[j];
	 b[i]=sum/a_ii;
      }
      else{
	 math_nrerror("lu_backsubstitute: Singular matrix");
	 return(0);
      }
   }
   return(1);
}

#ifndef _WIN32
#include <strings.h> // need memset
#endif

template <class FLOAT>
extern int _invert_matrix (FLOAT *a0, int n, FLOAT *inv, int *indx, FLOAT tiny, FLOAT *col)
{
  FLOAT d, zero = 0.0;
  int i, nij,j;
  // FLOAT a[n*n];
  FLOAT *a = XALLOC(FLOAT, n*n);
 
  for (i=0; i<n*n; i++) a[i]=a0[i];
  memset(inv, 0, n*n*sizeof(FLOAT));
  if (lu_decompose(a, n, indx, &d, tiny, col) == 0) return 0;

  for (j=0; j<n; j++)
    {for (i=0; i<n; i++) col[i] = zero;
      col[j] = 1.0;
      lu_backsubstitute(a,n,indx,col, tiny);
      for (i=0,nij=j;i<n;i++,nij+=n) 
	inv[nij] = col[i];
    }
  xxfree(a);
  return(1);
}


// EIGENVALUES AND EIGENVECTORS

template <class FLOAT>
void Householder_reduce(FLOAT *a, int n, FLOAT *d, FLOAT *e, int eignvectors_p)
{
  int l,k,j,i; int ni,nj,nk;
  FLOAT scale,hh,h,g,f;

  for (i=n-1,ni=n*i; i>=1; i--,ni-=n) {
    l=i-1;
    h=scale=0.0;
    if (l > 0) {
      for (k=0;k<=l;k++)
	scale += ABS(a[ni+k]);
      if (scale == 0.0)
	e[i]=a[ni+l];
      else {
	for (k=0;k<=l;k++) {
	  a[ni+k] /= scale;
	  h += a[ni+k]*a[ni+k];
	}
	f=a[ni+l];
	g = f>0 ? -sqrt(h) : sqrt(h);
	e[i]=scale*g;
	h -= f*g;
	a[ni+l]=f-g;
	f=0.0;
	for (j=0,nj=0; j<=l; j++, nj+=n) {
	  /* Next statement can be omitted if eigenvectors not wanted */
	  if (eignvectors_p) a[nj+i]=a[ni+j]/h;
	  g=0.0;
	  for (k=0; k<=j; k++)
	    g += a[nj+k]*a[ni+k];
	  for (k=j+1,nk=nj+n; k<=l; k++,nk+=n)
	    g += a[nk+j]*a[ni+k];
	  e[j]=g/h;
	  f += e[j]*a[ni+j];
	}
	hh=f/(h+h);
	for (j=0,nj=0; j<=l; j++,nj+=n) {
	  f=a[ni+j];
	  e[j]=g=e[j]-hh*f;
	  for (k=0;k<=j;k++)
	    a[nj+k] -= (f*e[k]+g*a[ni+k]);
	}
      }
    } else
      e[i]=a[ni+l];
    d[i]=h;
  }
	
  e[0]=0.0;
  /* Contents of this loop can be omitted if eigenvectors not
     wanted except for statement d[i]=a[ni+i]; */
  if (eignvectors_p)
    {d[0]=0.0;
      for (i=0,ni=0;i<n;i++,ni+=n) {
	l=i-1;
	if (d[i]) {
	  for (j=0,nj=0;j<=l;j++,nj+=n) {
	    g=0.0;
	    for (k=0,nk=0;k<=l;k++,nk+=n)
	      g += a[ni+k]*a[nk+j];
	    for (k=0,nk=0;k<=l;k++,nk+=n)
	      a[nk+j] -= g*a[nk+i];
	  }
	}
	d[i]=a[ni+i];
	a[ni+i]=1.0;
	for (j=0,nj=0;j<=l;j++,nj+=n) a[nj+i]=a[ni+j]=0.0;
      }}
  else for (i=0,ni=0;i<n;i++,ni+=n) d[i]=a[ni+i];
}




#define SIGN(a,b) ((b)<0 ? -ABS(a) : ABS(a))

template <class FLOAT>
void eigensolve(FLOAT *d, FLOAT *e, int n, FLOAT *z)
{
  int m,l,iter,i,k; int nk;
  FLOAT s,r,p,g,f,dd,c,b;

  for (i=1;i<n;i++) e[i-1]=e[i];
  e[n]=0.0;
  for (l=0;l<n;l++) {
    iter=0;
    do {
      for (m=l;m<n-1;m++) {
	dd=ABS(d[m])+ABS(d[m+1]);
	if ((FLOAT)(ABS(e[m])+dd) == dd) break;
      }
      if (m != l) {
	if (iter++ == 30) math_nrerror("eigensolve: Too many iterations in TQLI");
	g=(d[l+1]-d[l])/(2.0*e[l]);
	r=sqrt((g*g)+1.0);
	g=d[m]-d[l]+e[l]/(g+SIGN(r,g));
	s=c=1.0;
	p=0.0;
	for (i=m-1;i>=l;i--) {
	  f=s*e[i];
	  b=c*e[i];
	  if (ABS(f) >= ABS(g)) {
	    c=g/f;
	    r=sqrt((c*c)+1.0);
	    e[i+1]=f*r;
	    c *= (s=1.0/r);
	  } else {
	    s=f/g;
	    r=sqrt((s*s)+1.0);
	    e[i+1]=g*r;
	    s *= (c=1.0/r);
	  }
	  g=d[i+1]-p;
	  r=(d[i]-g)*s+2.0*c*b;
	  p=s*r;
	  d[i+1]=g+p;
	  g=c*r-b;
	  /* Next loop can be omitted if eigenvectors not wanted */
	  for (k=0,nk=0; k<n; k++,nk+=n) {
	    f=z[nk+i+1];
	    z[nk+i+1]=s*z[nk+i]+c*f;
	    z[nk+i]=c*z[nk+i]-s*f;
	  }
	}
	d[l]=d[l]-p;
	e[l]=g;
	e[m]=0.0;
      }
    } while (m != l);
  }
}


// SINGULAR VALUE DECOMPOSITION

template <class FLOAT>
inline FLOAT PYTHAG(FLOAT a, FLOAT b)
{
  FLOAT at,bt,ct, tmp;
  at=ABS(a);
  bt=ABS(b);
  if (at > bt) {tmp = at; at=bt; bt=tmp;}
  if (bt == 0) return(0.0);
  ct = at/bt;
  return (bt*sqrt(1.0+ct*ct));
}


template <class FLOAT>
void sv_decompose(FLOAT *a, int m, int n, FLOAT *w, FLOAT *v, FLOAT *rv1)
{
  int flag,i,its,j,jj,k,o,nm; int nk,ni,no,nj,njj;
  FLOAT c,f,h,s,x,y,z;
  FLOAT anorm=0.0,g=0.0,scale=0.0;

  if (m < n) math_nrerror("sv_decompose: You must augment A with extra zero rows");
  for (i=0,ni=0;i<n;i++,ni+=n) {
    o=i+1;no=ni+n;
    rv1[i]=scale*g;
    g=s=scale=0.0;
    if (i < m) {
      for (k=i,nk=ni;k<m;k++,nk+=n) scale += ABS(a[nk+i]);
      if (scale) {
	for (k=i,nk=ni;k<m;k++,nk+=n) {
	  a[nk+i] /= scale;
	  s += a[nk+i]*a[nk+i];
	}
	f=a[ni+i];
	g = -SIGN(sqrt(s),f);
	h=f*g-s;
	a[ni+i]=f-g;
	if (i != (n-1)) {
	  for (j=o;j<n;j++) {
	    for (s=0.0,k=i,nk=ni;k<m;k++,nk+=n) s += a[nk+i]*a[nk+j];
	    f=s/h;
	    for (k=i,nk=ni;k<m;k++,nk+=n) a[nk+j] += f*a[nk+i];
	  }
	}
	for (k=i,nk=ni;k<m;k++,nk+=n) a[nk+i] *= scale;
      }
    }
    w[i]=scale*g;
    g=s=scale=0.0;
    if (i < m && i != (n-1)) {
      for (k=o;k<n;k++) scale += ABS(a[ni+k]);
      if (scale) {
	for (k=o;k<n;k++) {
	  a[ni+k] /= scale;
	  s += a[ni+k]*a[ni+k];
	}
	f=a[ni+o];
	g = -SIGN(sqrt(s),f);
	h=f*g-s;
	a[ni+o]=f-g;
	for (k=o;k<n;k++) rv1[k]=a[ni+k]/h;
	if (i != (m-1)) {
	  for (j=o,nj=no;j<m;j++,nj+=n) {
	    for (s=0.0,k=o;k<n;k++) s += a[nj+k]*a[ni+k];
	    for (k=o;k<n;k++) a[nj+k] += s*rv1[k];
	  }
	}
	for (k=o;k<n;k++) a[ni+k] *= scale;
      }
    }
    anorm=MAX(anorm,(ABS(w[i])+ABS(rv1[i])));
  }
  for (i=n-1,ni=n*(n-1);i>=0;i--,ni-=n) {
    no=ni+n;
    if (i < (n-1)) {
      if (g) {
	for (j=o,nj=no;j<n;j++,nj+=n)
	  v[nj+i]=(a[ni+j]/a[ni+o])/g;
	for (j=o;j<n;j++) {
	  for (s=0.0,k=o,nk=no;k<n;k++,nk+=n) s += a[ni+k]*v[nk+j];
	  for (k=o,nk=no;k<n;k++,nk+=n) v[nk+j] += s*v[nk+i];
	}
      }
      for (j=o,nj=no;j<n;j++,nj+=n) v[ni+j]=v[nj+i]=0.0;
    }
    v[ni+i]=1.0;
    g=rv1[i];
    o=i;
  }
  for (i=n-1,ni=n*i;i>=0;i--,ni-=n) {
    o=i+1;
    no=ni+n;
    g=w[i];
    if (i < (n-1))
      for (j=o;j<n;j++) a[ni+j]=0.0;
    if (g) {
      g=1.0/g;
      if (i != (n-1)) {
	for (j=o;j<n;j++) {
	  for (s=0.0,k=o,nk=no;k<m;k++,nk+=n) s += a[nk+i]*a[nk+j];
	  f=(s/a[ni+i])*g;
	  for (k=i,nk=ni;k<m;k++,nk+=n) a[nk+j] += f*a[nk+i];
	}
      }
      for (j=i,nj=ni;j<m;j++,nj+=n) a[nj+i] *= g;
    } else {
      for (j=i,nj=ni;j<m;j++,nj+=n) a[nj+i]=0.0;
    }
    ++a[ni+i];
  }
  for (k=n-1;k>=0;k--) {
    for (its=1;its<=30;its++) {
      flag=1;
      for (o=k;o>=0;o--) {
	nm=o-1;
	if ((ABS(rv1[o])+anorm) == anorm) {
	  flag=0;
	  break;
	}
	if ((ABS(w[nm])+anorm) == anorm) break;
      }
      if (flag) {
	c=0.0;
	s=1.0;
	for (i=o;i<=k;i++) {
	  f=s*rv1[i];
	  rv1[i]=c*rv1[i];
	  if ((ABS(f)+anorm) == anorm) break;
	  g=w[i];
	  h=PYTHAG(f,g);
	  w[i]=h;
	  h=1.0/h;
	  c=g*h;
	  s=(-f*h);
	  for (j=0,nj=0;j<m;j++,nj+=n) {
	    y=a[nj+nm];
	    z=a[nj+i];
	    a[nj+nm]=y*c+z*s;
	    a[nj+i]=z*c-y*s;
	  }
	}
      }
      z=w[k];
      if (o == k) {
	if (z < 0.0) {
	  w[k] = -z;
	  for (j=0,nj=0;j<n;j++,nj+=n) v[nj+k]=(-v[nj+k]);
	}
	break;
      }
      if (its == 30) math_nrerror("sv_decompose: No convergence in 30 iterations");
      x=w[o];
      nm=k-1;
      y=w[nm];
      g=rv1[nm];
      h=rv1[k];
      f=((y-z)*(y+z)+(g-h)*(g+h))/(2.0*h*y);
      g=PYTHAG(f,1.0);
      f=((x-z)*(x+z)+h*((y/(f+SIGN(g,f)))-h))/x;
      c=s=1.0;
      for (j=o;j<=nm;j++) {
	i=j+1;
	g=rv1[i];
	y=w[i];
	h=s*g;
	g=c*g;
	z=PYTHAG(f,h);
	rv1[j]=z;
	c=f/z;
	s=h/z;
	f=x*c+g*s;
	g=g*c-x*s;
	h=y*s;
	y=y*c;
	for (jj=0,njj=0;jj<n;jj++,njj+=n) {
	  x=v[njj+j];
	  z=v[njj+i];
	  v[njj+j]=x*c+z*s;
	  v[njj+i]=z*c-x*s;
	}
	z=PYTHAG(f,h);
	w[j]=z;
	if (z) {
	  z=1.0/z;
	  c=f*z;
	  s=h*z;
	}
	f=(c*g)+(s*y);
	x=(c*y)-(s*g);
	for (jj=0,njj=0;jj<m;jj++,njj+=n) {
	  y=a[njj+j];
	  z=a[njj+i];
	  a[njj+j]=y*c+z*s;
	  a[njj+i]=z*c-y*s;
	}
      }
      rv1[o]=0.0;
      rv1[k]=f;
      w[k]=x;
    }
  }
}


template <class FLOAT>
void sv_backsubstitute(FLOAT *u, FLOAT *w, FLOAT *v, int m, int n, FLOAT *b, FLOAT *x, FLOAT *tmp)
{int jj,j,i; int ni,nj;
 FLOAT s;
 for (j=0;j<n;j++) {
   s=0.0;
   if (w[j]) {
     for (i=0,ni=0;i<m;i++,ni+=n) s += u[ni+j]*b[i];
     s /= w[j];
   }
   tmp[j]=s;
 }
 for (j=0,nj=0;j<n;j++,nj+=n) {
   s=0.0;
   for (jj=0;jj<n;jj++) s += v[nj+jj]*tmp[jj];
   x[j]=s;
 }
} 




template <class FLOAT>
FLOAT gammln(FLOAT xx)
{
  FLOAT x,tmp,ser;
  static FLOAT cof[6]={76.18009173,-86.50532033,24.01409822,
		       -1.231739516,0.120858003e-2,-0.536382e-5};
  int j;

  x=xx-1.0;
  tmp=x+5.5;
  tmp -= (x+0.5)*log(tmp);
  ser=1.0;
  for (j=0;j<=5;j++) {
    x += 1.0;
    ser += cof[j]/x;
  }
  return -tmp+log(2.50662827465*ser);
}



int
math_invert_double_matrix (double *mat, int n, double *mat_inverse)
{
  // int index[n];
  int *index = XALLOC(int, n);
  int r;
  
  // double col[n];
  double *col = XALLOC(double, n);
  
  double tiny = 0.0;
  r = _invert_matrix (mat, n, mat_inverse, index, tiny, col);
  xxfree(col); xxfree(index);

  return r;
}


double *
math_invert_matrix (double *mat, int n, double *inv)
{if (inv == NULL)
   inv =  XALLOC(double, n*n);
 if (math_invert_double_matrix(mat, n, inv) == 0)
   return 0; // singular.
 return(inv);
}

#if 0
double *
math_invert_matrix (double *mat, int n)
{
  double *inv = XALLOC(double, n*n);
  if (math_invert_double_matrix(mat, n, inv) == 0) {
    xxfree(inv);
    return 0; // singular.
  }
  return(inv);
}
#else
double *
math_invert_matrix (double *mat, int n) {
  return math_invert_matrix(mat, n, NULL);
}
#endif
	 


#define det2x2(m00,m01,m10,m11) (m00*m11-m01*m10)

int math_invert_3x3_matrix (double m[3][3], double inv[3][3])
{
  double m00 = m[0][0];
  double m10 = m[1][0];
  double m20 = m[2][0];
  double m01 = m[0][1];
  double m11 = m[1][1];
  double m21 = m[2][1];
  double m02 = m[0][2];
  double m12 = m[1][2];
  double m22 = m[2][2];
  double c00 = det2x2(m11,m12,m21,m22);
  double c01 = det2x2(m12,m10,m22,m20);
  double c02 = det2x2(m10,m11,m20,m21);
  double c10 = det2x2(m21,m22,m01,m02);
  double c11 = det2x2(m22,m20,m02,m00);
  double c12 = det2x2(m20,m21,m00,m01);
  double c20 = det2x2(m01,m02,m11,m12);
  double c21 = det2x2(m02,m00,m12,m10);
  double c22 = det2x2(m00,m01,m10,m11);
  double det = m00*c00 + m01*c01 + m02*c02;
  double detinv;
  if (det == 0.0) return(0);
  detinv = 1.0/det;
  
  inv[0][0] = detinv*c00;
  inv[1][0] = detinv*c01;
  inv[2][0] = detinv*c02;
  inv[0][1] = detinv*c10;
  inv[1][1] = detinv*c11;
  inv[2][1] = detinv*c12;
  inv[0][2] = detinv*c20;
  inv[1][2] = detinv*c21;
  inv[2][2] = detinv*c22;
  return(1);
}

int math_invert_3x3_submatrix (double m[4][4], double inv[4][4])
{
  double m00=m[0][0];
  double m10=m[1][0];
  double m20=m[2][0];
  double m01=m[0][1];
  double m11=m[1][1];
  double m21=m[2][1];
  double m02=m[0][2];
  double m12=m[1][2];
  double m22=m[2][2];
  double c00 = det2x2(m11,m12,m21,m22);
  double c01 = det2x2(m12,m10,m22,m20);
  double c02 = det2x2(m10,m11,m20,m21);
  double c10 = det2x2(m21,m22,m01,m02);
  double c11 = det2x2(m22,m20,m02,m00);
  double c12 = det2x2(m20,m21,m00,m01);
  double c20 = det2x2(m01,m02,m11,m12);
  double c21 = det2x2(m02,m00,m12,m10);
  double c22 = det2x2(m00,m01,m10,m11);
  double det = m00*c00 + m01*c01 + m02*c02;
  double detinv;
  if (det == 0.0) return(0);
  detinv = 1.0/det;
  
  inv[0][0] = detinv*c00;
  inv[1][0] = detinv*c01;
  inv[2][0] = detinv*c02;
  inv[0][1] = detinv*c10;
  inv[1][1] = detinv*c11;
  inv[2][1] = detinv*c12;
  inv[0][2] = detinv*c20;
  inv[1][2] = detinv*c21;
  inv[2][2] = detinv*c22;
  return(1);
}

void math_multiply_3x3_times_vector (double m[3][3], double from[3], double to[3])
{
  to[0] = m[0][0]*from[0] + m[0][1]*from[1] +m[0][2]*from[2];
  to[1] = m[1][0]*from[0] + m[1][1]*from[1] +m[1][2]*from[2];
  to[2] = m[2][0]*from[0] + m[2][1]*from[1] +m[2][2]*from[2];
}


void math_multiply_3x3_submatrix_times_vector (double m[4][4], 
					  double from[3], double to[3])
{
  to[0] = m[0][0]*from[0] + m[0][1]*from[1] +m[0][2]*from[2];
  to[1] = m[1][0]*from[0] + m[1][1]*from[1] +m[1][2]*from[2];
  to[2] = m[2][0]*from[0] + m[2][1]*from[1] +m[2][2]*from[2];
}



#if 0
/* assumes that row 4 of mat is 0 0 0 1 */
int math_invert_homogeneous_4x4_matrix (double m[4][4], double minv[4][4])
{
  int i,j;
  double sum;
  
  if (! math_invert_3x3_submatrix(m,minv)) return 0;/* singular matrix */

  for (i=0;i<3;i++) {
    sum = 0.0;
    for (j=0;j<3;j++) 
      sum += minv[i][j]*m[j][3];
    minv[i][3] = -sum;
    minv[3][i] = 0.0;
  }
  minv[3][3]=1.0;
  return 1;
}
#endif

/* assumes that row 4 of mat is 0 0 0 1 */

int math_invert_homogeneous_4x4_matrix (double m[4][4], double minv[4][4])
{
  double from[3], to[3];
 
  if (! math_invert_3x3_submatrix(m,minv)) return 0;/* singular matrix */
  from[0] = m[0][3];
  from[1] = m[1][3];
  from[2] = m[2][3];
  math_multiply_3x3_submatrix_times_vector(minv, from, to);
  minv[0][3] = -to[0]; minv[1][3] = -to[1]; minv[2][3] = -to[2]; 
  minv[3][0] = 0.0;  minv[3][1] = 0.0;  minv[3][2] = 0.0;  minv[3][3] = 1.0;
  return 1;
}




// C interface


int math_ludcmp(double *a, int n, int *indx, double *d, double tiny, double *vv)
{return lu_decompose(a, n, indx, d, tiny, vv);
}

int math_lubksb(double *a, int n, int *indx, double b[], double tiny)
{return lu_backsubstitute(a, n, indx, b, tiny);
}

int math_invert (double *a, int n, double *inv, int *indx, double tiny, double *col)
{return _invert_matrix(a, n, inv, indx, tiny, col);
}

void math_tred2(double *a, int n, double *d, double *e, int eignvectors_p)
{return Householder_reduce(a, n, d, e, eignvectors_p);
}

void math_tqli(double *d, double *e, int n, double *z)
{return eigensolve(d, e, n, z);
}

void math_svdcmp(double *a, int m, int n, double *w, double *v, double *rv1)
{return sv_decompose(a, m, n, w, v, rv1);
}

void math_svbksb(double *u, double *w, double *v, int m, int n, double *b, double *x, double *tmp)
{return sv_backsubstitute(u, w, v, m, n, b, x, tmp);
}

double math_gammln(double xx)
{return gammln(xx);
}

END_NAMESPACE_FREEDIUS
