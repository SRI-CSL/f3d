#include <stdio.h>

#ifdef _WIN32
#include <windows.h>
#endif

#include "gl-headers.h"

#include "cme-error.h"
#include "lisp-ffi-macros.h"
#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

typedef double coordinate_element;
typedef coordinate_element mat4x4[4][4]; 

#define swap(i,j) tmp=m1[i][j];m1[i][j]=m1[j][i];m1[j][i]=tmp;
#define copy(i,j) m2[j][i]=m1[i][j];

// warning is defined by c/misc/cme-error.c++ -- currently linked into libfreedius
// libfreedius must be loaded before libf3dglffi

int handle_gl_errors(char *error_comment)
{
  GLenum err;
  int errcnt = 0;
  while ((err=glGetError()) != GL_NO_ERROR) {
    errcnt++;
    warning("GL error: %s\n   Called from:%s\n", gluErrorString(err), error_comment);
  }
  return errcnt;
}

void transpose_4x4_double (double m1[4][4], double m2[4][4])
{
  if (m1 == m2)
    {double tmp;
    swap(0,1);
    swap(0,2);
    swap(0,3);
    swap(1,2);
    swap(1,3);
    swap(2,3);
    }
  else 
    {
      copy(0,0); copy(0,1); copy(0,2); copy(0,3); 
      copy(1,0); copy(1,1); copy(1,2); copy(1,3); 
      copy(2,0); copy(2,1); copy(2,2); copy(2,3); 
      copy(3,0); copy(3,1); copy(3,2); copy(3,3); 
    }}


void glMultMatrixd_transposed(double *m)
{
  mat4x4 mt;
  transpose_4x4_double((double (*)[4]) m, mt);
  glMultMatrixd((double *)mt);
}


void set_transfer_scale_bias(double scale, double bias) {
  glPixelTransferf(GL_MAP_COLOR, GL_FALSE);
  glPixelTransferf(GL_RED_SCALE, (float) scale);
  glPixelTransferf(GL_GREEN_SCALE, (float) scale);
  glPixelTransferf(GL_BLUE_SCALE, (float) scale);

  glPixelTransferf(GL_RED_BIAS, (float) bias);
  glPixelTransferf(GL_GREEN_BIAS, (float) bias);
  glPixelTransferf(GL_BLUE_BIAS, (float) bias);
}




extern "C" {


int FREEDIUS_GLOBAL(glDrawPixels_offset) (GLsizei width, GLsizei height, GLenum format, GLenum type, 
					  const GLvoid *pixels, GLenum pixels_offset)
{
  glDrawPixels(width, height, format, type, ((char *)pixels)+pixels_offset);
  return 0;
}

int FREEDIUS_GLOBAL(glTexImage2D_offset) (GLenum target, GLint level, GLint internalFormat, 
					  GLsizei width, GLsizei height, GLint border,
					  GLenum format, GLenum type, 
					  const GLvoid *pixels, GLenum pixels_offset)
{
  glTexImage2D(target, level, internalFormat, width, height, border, format, type,
	       ((char *)pixels)+pixels_offset);
  return 0;
}




int FREEDIUS_GLOBAL(handle_gl_errors)(char *error_comment)
{
  return handle_gl_errors(error_comment);
}

void FREEDIUS_GLOBAL(transpose_4x4_double) (double m1[4][4], double m2[4][4])
{
  transpose_4x4_double(m1, m2);
}

void FREEDIUS_GLOBAL(glMultMatrixd_transposed)(double *m)
{
  glMultMatrixd_transposed(m);
}

void FREEDIUS_GLOBAL(set_transfer_scale_bias)(double scale, double bias)
{
  set_transfer_scale_bias(scale, bias);
}


void FREEDIUS_GLOBAL(glClearColor) (double red, double green, double blue, double alpha )
{
  glClearColor(red, green, blue, alpha);
} 


void FREEDIUS_GLOBAL(glClearIndex) (double index )
{
  glClearIndex(index);
} 

void FREEDIUS_GLOBAL(glPixelZoom) (double xfactor, double yfactor )
{
  glPixelZoom(xfactor, yfactor);
} 

void FREEDIUS_GLOBAL(glLineWidth) (double width)
{
  glLineWidth(width);
}

void FREEDIUS_GLOBAL(glPointSize) (double width)
{
  glPointSize(width);
}


void FREEDIUS_GLOBAL(draw_polygon) (double verts[][3], 
				    unsigned short *indices, int n,
				    double normals[][3], int normal_index)
{
  glBegin(GL_POLYGON);
  if (normals)
    glNormal3dv(normals[normal_index]);
  else glNormal3d(0.0, 0.0, 0.0);
  
  if (indices)
    for (int i = 0; i<n; i++) 
      glVertex3dv(verts[indices[i]]);
  else
    for (int i = 0; i<n; i++) 
      glVertex3dv(verts[i]);
  glEnd();
}

void FREEDIUS_GLOBAL(draw_line_strip) (double verts[][3], int n)
{
  glBegin(GL_LINE_STRIP);
  for (int i = 0; i<n; i++) 
      glVertex3dv(verts[i]);
  glEnd();
}

// Need a better way to check for extensions at compile time.  The
// DARWIN flag is too general, since these may be available on X11 but
// not on Carbon / Cocoa.

#if 0 // eliminate the use of glBlendColorEXT
// Why did this turn into an "#if 0" ??
#if  defined(WIN32) || defined(DARWIN)

void
FREEDIUS_GLOBAL(glBlendColorEXT) (double red, double grn, double blu, double alpha)
{}

#else 

void
FREEDIUS_GLOBAL(glBlendColorEXT) (double red, double grn, double blu, double alpha)
{
  #ifndef GL_EXT_blend_color
  glBlendColor((GLclampf) red, (GLclampf) grn,
	       (GLclampf) blu, (GLclampf) alpha);
  #else
  glBlendColorEXT((GLclampf) red, (GLclampf) grn,
		  (GLclampf) blu, (GLclampf) alpha);
  #endif
}

#endif // WIN32 or DARWIN
#endif // eliminate the use of glBlendColorEXT

void 
FREEDIUS_GLOBAL(glPolygonOffset) (double factor, double bias)
{
  glPolygonOffset(factor, bias);
} 

void  
FREEDIUS_GLOBAL(glClearAccum) (double red, double grn, double blu, double alpha)
{
  glClearAccum (red, grn, blu, alpha);
}



void FREEDIUS_GLOBAL(glClearColor4dv) (double color[4])
{
  glClearColor(color[0], color[1], color[2], color[3]);
}


// the result coords are window coordinates 
int FREEDIUS_GLOBAL(gluProject_to_window) (double from[3], double to[3])
{
  double modelmatrix[16];
  double projmatrix[16];
  //  int viewport[4];
  GLint viewport[4];
  int flag;
  glGetDoublev(GL_MODELVIEW_MATRIX, modelmatrix);
  glGetDoublev(GL_PROJECTION_MATRIX, projmatrix);
  glGetIntegerv(GL_VIEWPORT, viewport);
  flag = gluProject(from[0],from[1],from[2],
		    modelmatrix, projmatrix, viewport,
		    &to[0],&to[1],&to[2]);
  //  to[1] = viewport[3] - 1 - to[1]; // flip y-origin relative to top of window

  to[1] = viewport[3] - to[1]; // flip y-origin relative to top of window
  return flag;
}

// from coords are window coordinates 
// to coords are world coordinates 
int FREEDIUS_GLOBAL(gluProject_to_world) (double from[3], double to[3])
{
  double modelmatrix[16];
  double projmatrix[16];
  // int viewport[4];
  GLint viewport[4];
  int flag;
  glGetDoublev(GL_MODELVIEW_MATRIX, modelmatrix);
  glGetDoublev(GL_PROJECTION_MATRIX, projmatrix);
  glGetIntegerv(GL_VIEWPORT, viewport);
  flag = gluUnProject(from[0],
		      // flip y-coord relative to top of window 
		      //viewport[3]-1-from[1], 
		      viewport[3]-from[1], 
		      from[2],
		      modelmatrix, projmatrix, viewport,
		      &to[0],&to[1],&to[2]);
  return flag;
}



void FREEDIUS_GLOBAL(glDrawquad_strip) (double verts[][3], GLsizei nfaces)
{
  int face, vert;
  for (face=0,vert=0; face<nfaces; face++, vert+=2) {
    glBegin(GL_POLYGON);
    glVertex3dv(verts[vert]);
    glVertex3dv(verts[vert+1]);
    glVertex3dv(verts[vert+3]);
    glVertex3dv(verts[vert+2]);
    glEnd();
  }   
}

void FREEDIUS_GLOBAL(glDrawquad_strip_indexed) (double verts[][3], 
						GLsizei nfaces,
						const unsigned short *indices)
{
  int face, vert;
  for (face=0,vert=0; face<nfaces; face++, vert+=2) {
    glBegin(GL_POLYGON);
    glVertex3dv(verts[indices[vert]]);
    glVertex3dv(verts[indices[vert+1]]);
    glVertex3dv(verts[indices[vert+3]]);
    glVertex3dv(verts[indices[vert+2]]);
    glEnd();
  }    
}

void FREEDIUS_GLOBAL(glDrawquad_strip_with_face_normals) (double verts[][3], 
							  double face_normals[][3], 
							  GLsizei nfaces)
{
  int face, vert;
  for (face=0,vert=0; face<nfaces; face++, vert+=2) {
    glBegin(GL_POLYGON);
    glNormal3dv(face_normals[face]);
    glVertex3dv(verts[vert]);
    glVertex3dv(verts[vert+1]);
    glVertex3dv(verts[vert+3]);
    glVertex3dv(verts[vert+2]);
    glEnd();
  }   
}
void FREEDIUS_GLOBAL(glDrawquad_strip_indexed_with_face_normals) (double verts[][3], 
								  double face_normals[][3], 
								  GLsizei nfaces,
								  const unsigned short *indices)
{
  int face, vert;
  for (face=0,vert=0; face<nfaces; face++, vert+=2) {
    glBegin(GL_POLYGON);
    glNormal3dv(face_normals[face]);
    glVertex3dv(verts[indices[vert]]);
    glVertex3dv(verts[indices[vert+1]]);
    glVertex3dv(verts[indices[vert+3]]);
    glVertex3dv(verts[indices[vert+2]]);
    glEnd();
  }    
}

void FREEDIUS_GLOBAL(glDrawquad_strip_with_vertex_normals) (double verts[][3], 
							    double normals[][3], 
							    GLsizei nfaces)
{
  int face, vert;
  for (face=0,vert=0; face<nfaces; face++, vert+=2) {
    glBegin(GL_POLYGON);
    glNormal3dv(normals[vert]); glVertex3dv(verts[vert]); 
    glNormal3dv(normals[vert+1]); glVertex3dv(verts[vert+1]);
    glNormal3dv(normals[vert+2]); glVertex3dv(verts[vert+3]);
    glNormal3dv(normals[vert+3]); glVertex3dv(verts[vert+2]);
    glEnd();
  }   
}

void FREEDIUS_GLOBAL(glDrawquad_strip_indexed_with_vertex_normals) (double verts[][3], 
								  double normals[][3], 
								  GLsizei nfaces,
								  const unsigned short *indices)
{
  int face, vert;
  for (face=0,vert=0; face<nfaces; face++, vert+=2) {
    glBegin(GL_POLYGON);
    glNormal3dv(normals[indices[vert]]);   glVertex3dv(verts[indices[vert]]);
    glNormal3dv(normals[indices[vert+1]]); glVertex3dv(verts[indices[vert+1]]);
    glNormal3dv(normals[indices[vert+3]]); glVertex3dv(verts[indices[vert+3]]);
    glNormal3dv(normals[indices[vert+2]]); glVertex3dv(verts[indices[vert+2]]);
    glEnd();
  }    
}



#if defined(GLU_VERSION_1_2) || defined(GLU_VERSION_1_3) || (!defined(MESA) && !defined(LINUX))
// Is there any situation where this conditional does not succeed?  I would like to flush
// the conditional and the else part.


#ifdef WIN32
#ifdef  CODEWARRIOR
typedef __stdcall GLvoid (*tesscallback)();
#else
typedef void (CALLBACK * tesscallback)();
  //typedef void (*tesscallback)();
#endif // CODEWARRIOR
#else
#if defined(SOLARIS) /* || defined(AGL) */
  //#if defined(SOLARIS)
typedef GLvoid (*tesscallback)(...);
#else
typedef GLvoid (*tesscallback)();
#endif // SOLARIS
#endif // _WIN32

// This currently only supports only double vertices and no other
// vertex properties such as normal, color and texcoords.
// Would be easy to generalize.

void FREEDIUS_GLOBAL(draw_polygon_tesselated) (double verts[][3], 
				  unsigned short *indices, int n,
				  double normals[][3], int normal_index)
{
  static GLUtesselator* tess = 0;
  int i, j;
  if (! tess) {
    tess = gluNewTess();
    gluTessCallback(tess, GLU_TESS_BEGIN, (tesscallback) glBegin);
    gluTessCallback(tess, GLU_TESS_END, (tesscallback) glEnd);
    gluTessCallback(tess, GLU_TESS_VERTEX, (tesscallback) glVertex3dv);
    gluTessCallback(tess, GLU_TESS_EDGE_FLAG, (tesscallback)glEdgeFlag);
  }
 
  gluTessProperty(tess, GLU_TESS_WINDING_RULE, GLU_TESS_WINDING_POSITIVE);
  gluTessBeginPolygon(tess,0);
  gluTessBeginContour(tess);
 
  if (normals) 
    gluTessNormal(tess, normals[normal_index][0], normals[normal_index][1], normals[normal_index][2]);
  else gluTessNormal(tess, 0.0, 0.0, 0.0);
  
  if (indices) {
    for(j=0;j<n; j++) {
      i = indices[j];
      gluTessVertex(tess, verts[i], verts[i]);
    }
  } else 
    for(i=0;i<n; i++) {
      gluTessVertex(tess, verts[i], verts[i]);
    }
  gluTessEndContour(tess);
  gluTessEndPolygon(tess);
  // For some reason, on NVidia GEforce 4 MX I need to set glEdgeFlag after
  // drawing a tesselated polygon.
  glEdgeFlag(GL_TRUE);  // This is apparently missing in GLU polygon tesselation ???
}

#else // MESA_3_0 is defined (GLU_VERSION_1_2 || GLU_VERSION_1_3 || (!defined(MESA) && !defined(LINUX))

#error "Totally bogus GLU version, punt!"

// This is for the old GLU_1_1 callbacks - probably no longer needed.
#ifndef CALLBACK
#define CALLBACK GLCALLBACK
#endif

typedef void (*tesscallback)();

void FREEDIUS_GLOBAL(draw_polygon_tesselated) (double verts[][3], 
				  unsigned short *indices, int n,
				  double normals[][3], int normal_index)
{
  static GLUtriangulatorObj *tess = 0;
  int i, j;
  if (! tess) {
    tess=gluNewTess();
    gluTessCallback(tess, GLU_TESS_BEGIN, (tesscallback) glBegin);
    gluTessCallback(tess, GLU_TESS_END, (tesscallback)glEnd);
    gluTessCallback(tess, GLU_TESS_VERTEX, (tesscallback)glVertex3dv);
    gluTessCallback(tess, GLU_TESS_EDGE_FLAG, (tesscallback)glEdgeFlag);	
  }
  gluBeginPolygon(tess);  
  gluNextContour(tess, GLU_UNKNOWN);
  if (normals) 
    glNormal3dv(normals[normal_index]);      // NORMALS APPEAR TO BE BROKEN 
  else glNormal3d(0.0, 0.0, 0.0);

  if (indices) {
    for(j=0;j<n; j++) {
      i = indices[j];
      gluTessVertex(tess, verts[i], verts[i]);
    }
  } else 
    for(i=0;i<n; i++) {
      gluTessVertex(tess, verts[i], verts[i]);
    }
  gluEndPolygon(tess);
}

#endif // MESA

MKVAR(GLU_POINT)
MKVAR(GLU_LINE)
MKVAR(GLU_FILL)
MKVAR(GLU_SILHOUETTE)
MKVAR(GLU_SMOOTH)
MKVAR(GLU_FLAT)
MKVAR(GLU_NONE)

MKVAR(GLU_OUTSIDE)
MKVAR(GLU_INSIDE)

} // end extern "C"




void get_window_dims(int *width, int *height)
{ 
  GLint vp[4];
  glGetIntegerv(GL_VIEWPORT, vp);
  *width  = vp[2];
  *height = vp[3];
}

// Map window coordinates (0:dim) to normalized device coordinates (-1:+1)
// Also map w/s range from 0:+1 to -1:+1.  OpenGL then remaps it back to 0:+1.
double * window_to_ndc_matrix(mat4x4 m)
{
  int width, height;
  {
    double *m1d = (double *) m;
    for (int i=0; i<16; i++) m1d[i]=0.0;
  }
  //bzero(m, 16*sizeof(double));
  get_window_dims(&width, &height);

  if (width <= 1) width = 1;
  if (height <= 1) height = 1;

  m[0][0] =  2.0/width;    m[0][3] = -1.0;
  m[1][1] = -2.0/height;   m[1][3] = +1.0;
  //m[2][2] = 1.0;
  m[2][2] = 2.0; m[2][3] = -1.0;      // new Sat Mar 27 2004 for mapping 0:+1 to -1:+1
  m[3][3] = 1.0;
  return((double *) m);
}

// This sets GL_PROJECTION to 2d_to_ndc transform
void set_2d_to_ndc_matrix (mat4x4 mat_2d_to_window)
{ 
  // depend on resize callback to update the viewport of the context.
  //set_glviewport(win);
  mat4x4 w2ndc;

  // tprintf(11, "set_2d_to_ndc_matrix: calling   glMatrixMode (GL_PROJECTION);\n");
  glMatrixMode (GL_PROJECTION);
  //tprintf(11, "set_2d_to_ndc_matrix: calling   glLoadIdentity();\n");
  glLoadIdentity();
  //tprintf(11, "set_2d_to_ndc_matrix: calling   glMultMatrixd_transposed((double *) window_to_ndc_matrix(w2ndc));\n");
  glMultMatrixd_transposed((double *) window_to_ndc_matrix(w2ndc));
  //tprintf(11, "set_2d_to_ndc_matrix: calling   glMultMatrixd_transposed((double *) mat_2d_to_window);\n");
  glMultMatrixd_transposed((double *) mat_2d_to_window);
}

extern "C" {

void FREEDIUS_GLOBAL(set_2d_to_ndc_matrix) (mat4x4 mat_2d_to_window)
{
  set_2d_to_ndc_matrix(mat_2d_to_window);
}


// map window coordinates (0:dim) to normalized device coordinates (-1:+1)
double * FREEDIUS_GLOBAL(window_to_ndc_matrix)(mat4x4 m)
{
  return window_to_ndc_matrix(m);
}

} // end extern "C"


END_NAMESPACE_FREEDIUS


