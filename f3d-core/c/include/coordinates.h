#ifndef __coordinates_h
#define __coordinates_h

#include "misc.h"

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

typedef double coordinate_element;
typedef coordinate_element coordinate_vector;
typedef coordinate_vector *Coordinate_Vector;
typedef coordinate_element cv2[2];
typedef coordinate_element cv3[3];

typedef coordinate_element bbox2d[4];
typedef coordinate_element bbox3d[6];

#ifdef __cplusplus

inline internal coordinate_vector *
copy_vector (const coordinate_vector *const from, coordinate_vector *to, int n)
{int i;
 if (from == NULL) return(NULL);
 for (i=0; i<n; i++) to[i]=from[i];
 return(to);
}

inline internal coordinate_vector *
set_cv (coordinate_vector *cv, coordinate_element x, coordinate_element y, coordinate_element z)
{cv[0]=x; cv[1]=y; cv[2]=z;
 return(cv);
}
 
inline internal coordinate_vector *
set_cv (coordinate_vector *cv, coordinate_element x, coordinate_element y)
{cv[0]=x; cv[1]=y;
 return(cv);
}


inline internal double 
dot_product (coordinate_vector cv1[3], coordinate_vector *cv2)
{return(cv1[0]*cv2[0] + cv1[1]*cv2[1] + cv1[2]*cv2[2]);
}


#endif /* __cplusplus */

END_NAMESPACE_FREEDIUS

#endif /* ! __coordinates_h */
