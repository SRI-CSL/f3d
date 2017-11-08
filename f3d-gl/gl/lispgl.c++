#include <math.h>

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

typedef double vect2[2];
typedef double vect3[3];
typedef double vect4[4];
typedef double mat4x4[4][4];

double normalize_vector (double *v, int n)
{
  int i;
  double len = 0.0;
  
  for (i=0; i<n; i++) len+=v[i]*v[i];
  if (len==0) return (0.0);
  len = 1/sqrt(len);
  for (i=0; i<n; i++) v[i]=v[i]*len;
  return(len);
}

// C++ is a total crock !!!
// It is illegal to overload based on vector length decls.
// Ie, euclidean_length(vect2 v) and euclidean_length(vect3 v)
// are considered the same.  In particular they are euclidean_length(double *v)

double euclidean_length2(vect2 v)
{ 
  return (sqrt(v[0]*v[0] + v[1]*v[1]));
}

double euclidean_length3(vect3 v)
{ 
  return (sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]));
}

double normv2 (vect2 v)
{
  double len;
  len= euclidean_length2(v);
  if (len==0) return (0.0);
  len = 1/len;
  v[0]*=len; v[1]*=len; 
  return(len);
}

double normv3 (vect3 v)
{
  double len;
  len= euclidean_length3(v);
  if (len==0) return (0.0);
  len = 1/len;
  v[0]*=len; v[1]*=len; v[2]*=len; 
  return(len);
}

void trans4x4 (mat4x4 m, vect3 v, vect3 v2)
{
  v2[0] = m[0][0]*v[0] + m[0][1]*v[1] + m[0][2]*v[2] + m[0][3];
  v2[1] = m[1][0]*v[0] + m[1][1]*v[1] + m[1][2]*v[2] + m[1][3];
  v2[2] = m[2][0]*v[0] + m[2][1]*v[1] + m[2][2]*v[2] + m[2][3];
}

void trans4x4_2 (mat4x4 m, vect3 v, vect2 v2)
{
  v2[0] = m[0][0]*v[0] + m[0][1]*v[1] + m[0][2]*v[2] + m[0][3];
  v2[1] = m[1][0]*v[0] + m[1][1]*v[1] + m[1][2]*v[2] + m[1][3];
}

void vdiff2 (vect3 v1, vect3 v2, vect2 w)
{
  w[0] = v1[0] - v2[0];
  w[1] = v1[1] - v2[1];
}

void vadd2 (vect2 v1, vect2 v2, vect2 w)
{
  w[0] = v1[0] + v2[0];
  w[1] = v1[1] + v2[1];
}

void vdiff3 (vect3 v1, vect3 v2, vect3 w)
{
  w[0] = v1[0] - v2[0];
  w[1] = v1[1] - v2[1];
  w[2] = v1[2] - v2[2];
}


void vadd3 (vect3 v1, vect3 v2, vect3 w)
{
  w[0] = v1[0] + v2[0];
  w[1] = v1[1] + v2[1];
  w[2] = v1[2] + v2[2];
}

void vscale3 (double s, vect3 v, vect3 w)
{
  w[0] = s*v[0];
  w[1] = s*v[1];
  w[2] = s*v[2];
}

void vscale2 (double s, vect2 v, vect2 w)
{
  w[0] = s*v[0];
  w[1] = s*v[1];
}



extern "C" {

void FREEDIUS_GLOBAL(compute_ribbon_face_vertex) (mat4x4 obj2world,
				   vect4 v1, vect4 v2, vect4 v3, 
				   vect3 vleft, vect3 vright)

{
  vect3 w1, w2, w3;
  vect2 v21, v23, v13, n;
  double l21, l23, ln;
 
  trans4x4(obj2world, v1, w1);
  trans4x4(obj2world, v2, w2);
  trans4x4(obj2world, v3, w3);
  
  vdiff2(w1, w2, v21);
  vdiff2(w3, w2, v23);
  
  l21 = normv2(v21);
  l23 = normv2(v23);
  if (l21==0) {v21[0]=-v23[0];v21[1]=-v23[1];}
  if (l23==0) {v23[0]=-v21[0];v23[1]=-v21[1];}
  
  vadd2 (v21, v23, n);
  ln = normv2(n);
  if (ln==0) {n[0]=v21[1]; n[1]=-v21[0];}
  
  vdiff2 (v23, v21, v13);
 { 
    double cos_theta_x2 = euclidean_length2(v13);
    double w = (cos_theta_x2 ==0)? 0 : v2[3]/cos_theta_x2; // ribbon width at vertex 2
    if ((v13[0]*n[1] - v13[1]*n[0]) < 0) w = -w;
    vleft[0]=  w2[0] - w*n[0]; vleft[1]=  w2[1] - w*n[1]; vleft[2]  = w2[2];
    vright[0]= w2[0] + w*n[0]; vright[1]= w2[1] + w*n[1]; vright[2] = w2[2];
 }
}

void FREEDIUS_GLOBAL(compute_ribbon_edge_vertices) (mat4x4 obj2world,
				double verts[][4],
				int nverts,
				double edge_verts [][3])
{
  int i, j, v1i, v2i, v3i;
  for (i=0, j=0; i<nverts; i++, j+=2) {
    v1i = (i==0) ? 0 : i-1;
    v2i = i;
    v3i = (i==nverts-1)? nverts-1 : i+1;
    FREEDIUS_GLOBAL(compute_ribbon_face_vertex)(obj2world, 
				 verts[v1i], verts[v2i], verts[v3i],
				 edge_verts[j+1], edge_verts[j] );
  }
}    

} // end extern "C"
 
END_NAMESPACE_FREEDIUS
