#include "image.h"
#include "image-ops.h"

#ifdef __GNUG__
#pragma implementation "gauss-conv"
#pragma interface "gauss-conv"
#endif

#include <stdlib.h>
#include "cme-error.h" 

#include "cme_time.h"

#include <stdio.h>

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

template <class BUFFER_TYPE>
inline void copy_line(BUFFER_TYPE *from_buf, BUFFER_TYPE *to_buf, int n)
{
  for (int i=0; i<n; i++) to_buf[i]=from_buf[i];
}

//
// This next gets an image line into the center of `buf', then 
// pads `buf' with the values at the borders.
//
template <class BUFFER_TYPE>
void getline_padded (image *img, BUFFER_TYPE *buf, int y, int left_bdr, int right_bdr)
{
  int i; 
  int rt = image_xdim(img)-1+left_bdr;
  getline(img, buf, 0, y, image_xdim(img), left_bdr);
  for (i=0; i<left_bdr; i++) buf[i]=buf[left_bdr];
  for (i=1; i<=right_bdr; i++) buf[rt+i]=buf[rt];
}

template <class BUFFER_TYPE>
void getline_padded1 (image *img, BUFFER_TYPE *buf, int y, int left_bdr, int right_bdr)
{
  int i; 
  int rt = image_xdim(img)-1+left_bdr;
  getline(img, buf, 0, y, image_xdim(img), 1);
  for (i=0; i<left_bdr; i++) buf[i]=buf[left_bdr];
  for (i=1; i<=right_bdr; i++) buf[rt+i]=buf[rt];
}
 

// **************    float and double versions   ************** 

inline double 
GAUSS_CONV(double x0, double x1, double x2, double ka, double kb, double kc, int ignore)
{
  return (ka*x0 + kb*x1 + kc*x2);
}
// gauss_line1 is only called from gauss_convolve_decimate2 with BUFFER_TYPE = double
template <class BUFFER_TYPE>
inline void gauss_line1 (int into_xdim, BUFFER_TYPE *inbuf, BUFFER_TYPE *outbuf,
		  BUFFER_TYPE ka, BUFFER_TYPE kb, BUFFER_TYPE kc, int extra)
{
  BUFFER_TYPE xm2, xm1, x0, x1, x2;
  int from_i, to_i;
  x0 = inbuf[0]; x1 = inbuf[1]; x2 = inbuf[2];
  for (from_i = 3, to_i = 0; to_i < into_xdim; to_i++, from_i+=2) {
    xm2 = x0; xm1 = x1; x0 = x2;
    x1 = inbuf[from_i];
    x2 = inbuf[from_i+1];
    //    outbuf[to_i] = ka*x0 + kb*(xm1+x1) + kc*(xm2+x2);
    outbuf[to_i] = GAUSS_CONV(x0, xm1+x1, xm2+x2, ka, kb, kc, extra);
    }
}

// GAUSSLINE is only called from gauss_convolve_decimate2 with BUFFER_TYPE = double
#define GAUSSLINE(j, outbuf, repbuf)\
 if (j < ydim)\
  {getline_padded(img, inbuf, j, start_bdr, end_bdr);\
  gauss_line1(into_xdim, inbuf, outbuf, intermediate_ka, kb, kc, intermediate_shift);}\
  else if (repbuf) copy_line(repbuf, outbuf, into_xdim);


// gauss_convolve_decimate2 is only called with BUFFER_TYPE = double
template <class BUFFER_TYPE>  
image *
gauss_convolve_decimate2 (image *img, BUFFER_TYPE ka, BUFFER_TYPE kb, BUFFER_TYPE kc, 
			  int result_shift, int intermediate_shift, BUFFER_TYPE intermediate_ka,
			  image *into_image)
{
  int reduce = 2;
  int samp_spacing = 1;
  int xdim = image_xdim(img);
  int ydim = image_ydim(img);
  int into_xdim = xdim/reduce;
  int into_ydim = ydim/reduce;
  //BUFFER_TYPE inbuf[xdim + 4*samp_spacing];
  BUFFER_TYPE *inbuf = XALLOC(BUFFER_TYPE, xdim + 4*samp_spacing);
  //BUFFER_TYPE outbuf[xdim], buf0a[xdim], buf1a[xdim], buf2a[xdim], buf3a[xdim], buf4a[xdim];
  BUFFER_TYPE *outbuf = XALLOC(BUFFER_TYPE, xdim);
  BUFFER_TYPE *buf0a = XALLOC(BUFFER_TYPE, xdim);
  BUFFER_TYPE *buf1a = XALLOC(BUFFER_TYPE, xdim);
  BUFFER_TYPE *buf2a = XALLOC(BUFFER_TYPE, xdim);
  BUFFER_TYPE *buf3a = XALLOC(BUFFER_TYPE, xdim);
  BUFFER_TYPE *buf4a = XALLOC(BUFFER_TYPE, xdim);

  BUFFER_TYPE *buf0= buf0a;
  BUFFER_TYPE *buf1= buf1a;
  BUFFER_TYPE *buf2= buf2a;
  BUFFER_TYPE *buf3= buf3a;
  BUFFER_TYPE *buf4= buf4a;
  BUFFER_TYPE *buft;
  int start_bdr = 1;
  int end_bdr = 2;
  int x, y;
  BUFFER_TYPE *nullbuf=0;
  Timeval tv1, tv2; 

  if (into_image->xdim > into_xdim || into_image->ydim > into_ydim 
      || image_element_type(img) != image_element_type(into_image))
    return 0;

  require_image_rows foo1(img);       // calls img->add_to_working_set 
  require_image_rows foo2(into_image);// calls into_image->add_to_working_set 

  run_time(&tv1);
  GAUSSLINE(0, buf3, nullbuf);
  GAUSSLINE(1, buf4, nullbuf);
  copy_line(buf3, buf2, into_xdim);
 
  for (y=0; y<into_ydim; y++) {
    buft=buf0; buf0=buf2; buf2=buf4; buf4=buf1; buf1=buf3; buf3=buft;
    GAUSSLINE(y+y+2, buf3, buf2);
    GAUSSLINE(y+y+3, buf4, buf2);
    // Convolve Vertically
    for (x = 0; x<into_xdim; x++)
      //outbuf[x] = ka*buf2[x] + kb*(buf1[x]+buf3[x]) + kc*(buf0[x]+buf4[x]);
      outbuf[x] = GAUSS_CONV(buf2[x], buf1[x]+buf3[x], buf0[x]+buf4[x], ka, kb, kc, result_shift);

    putline(into_image, outbuf, 0, y);
    }
#if 0
  time_diff(run_time(&tv2),&tv1,0);
  fprintf(stderr, "gauss_convolve_decimate time (excluding make_image)(%dx%d)=%f\n",
	  xdim, ydim,  time_secs(&tv2));
#endif
  xxfree(inbuf); xxfree(outbuf); xxfree(buf0a); xxfree(buf1a); xxfree(buf2a); xxfree(buf3a); xxfree(buf4a);
  return(into_image);
}
 
// ***********************  INTEGER VERSIONS   ***********************  

#define GAUSS_1D_LOOP(KC)\
{for (from_i = 3, to_i = 0; to_i < xdim; to_i++, from_i+=2)\
   {xm2 = x0; xm1 = x1; x0 = x2;\
    x1 = buf[from_i];\
    x2 = buf[from_i+1];\
    outbuf[to_i] = (round_in + ((xm1+x1+x0+x0)<<log2_kb) + (KC)*(xm2+x2-x0-x0)) >> result_shift;\
   }}

/* This multiply optimization is only needed on brain-damaged machines like Sparc2 */
void
gauss_1d_loop_int (int xdim, int *buf, int *outbuf, int kc, int result_shift, int log2_kb, int round_in)
{
  int xm2, xm1, x0, x1, x2;
  int from_i, to_i;
  x0 = buf[0]; x1 = buf[1]; x2 = buf[2];
  switch (kc) {
  case 1: GAUSS_1D_LOOP(1);break;
  case 18: GAUSS_1D_LOOP(18);break;
  case 9: GAUSS_1D_LOOP(9);break;
  default: GAUSS_1D_LOOP(kc);break;
  }
}

#define GAUSS_REDUCE_LOOP(KC)\
  {for (x = 0; x<xdim; x++)\
    {v0 = buf0[x];v0=v0+v0;\
    outbuf[x] = (round_in + ((bufm1[x]+buf1[x]+v0)<<log2_kb) + (KC)*(bufm2[x]+buf2[x]-v0)) >> result_shift;}}

void 
gauss_reduce_loop_long (int xdim, int round_in, int result_shift, int log2_kb, int kc,
			  int *bufm2, int *bufm1, int *buf0, int *buf1, int *buf2, int *outbuf)
{
  int x, v0;
  switch (kc) {
  case 1: GAUSS_REDUCE_LOOP(1); break;    /* kc=1, ka=6, fast case */
  case 18: GAUSS_REDUCE_LOOP(18); break;   /* kc=18, ka=92, min blur */
  case 9: GAUSS_REDUCE_LOOP(9); break;   /* kc=9, ka=110, min alias */
  default: GAUSS_REDUCE_LOOP(kc); break;   /* general case */
  }
}

#define GAUSSLINE3(j, outbuf, repbuf)\
 if (j < ydim)\
  {getline_padded(img, inbuf, j, start_bdr, end_bdr);\
  gauss_1d_loop_int(into_xdim, inbuf, outbuf, kc, intermediate_result_shift, log2_kb, intermediate_round_in);}\
  else if (repbuf) copy_line(repbuf, outbuf, into_xdim);
 
// gauss_convolve_decimate3 is always called with BUFFER_TYPE = int
template <class BUFFER_TYPE>  
extern image *
gauss_convolve_decimate3 (image *img, BUFFER_TYPE round_in, int log2_kb, int kc, int result_shift, 
			  int intermediate_result_shift, int intermediate_round_in, image *into_image)
{
  int reduce = 2;
  int samp_spacing = 1;
  int xdim = image_xdim(img);
  int ydim = image_ydim(img);
  int into_xdim = xdim/reduce;
  int into_ydim = ydim/reduce;
  //BUFFER_TYPE inbuf[xdim + 4*samp_spacing];
  BUFFER_TYPE *inbuf = XALLOC(BUFFER_TYPE, xdim + 4*samp_spacing);
  //BUFFER_TYPE outbuf[xdim], buf0a[xdim], buf1a[xdim], buf2a[xdim], buf3a[xdim], buf4a[xdim];
  BUFFER_TYPE *outbuf = XALLOC(BUFFER_TYPE, xdim);
  BUFFER_TYPE *buf0a = XALLOC(BUFFER_TYPE, xdim);
  BUFFER_TYPE *buf1a = XALLOC(BUFFER_TYPE, xdim);
  BUFFER_TYPE *buf2a = XALLOC(BUFFER_TYPE, xdim);
  BUFFER_TYPE *buf3a = XALLOC(BUFFER_TYPE, xdim);
  BUFFER_TYPE *buf4a = XALLOC(BUFFER_TYPE, xdim);
 
 
  BUFFER_TYPE *buf0 = buf0a;
  BUFFER_TYPE *buf1 = buf1a;
  BUFFER_TYPE *buf2 = buf2a;
  BUFFER_TYPE *buf3 = buf3a;
  BUFFER_TYPE *buf4 = buf4a;
  BUFFER_TYPE *buft;
  int start_bdr = 1;
  int end_bdr = 2;
  int y;
  BUFFER_TYPE *nullbuf=0;

  if (!into_image) // This can probably now be eliminated.
    into_image = (image *) make_image(into_xdim, into_ydim, image_element_type(img));
  else if (into_image->xdim > into_xdim || into_image->ydim > into_ydim 
	   || image_element_type(img) != image_element_type(into_image))
    return 0;

  require_image_rows foo1(img);
  require_image_rows foo2(into_image);

  tprintf(5,"gauss_convolve_decimate3: made an image...\n");
#ifdef GDBTEST
  y=x/(start_bdr-1);
  cerror("gauss_convolve_decimate3 x/0=%d\n", y);
#endif
  GAUSSLINE3(0, buf3, nullbuf);
  GAUSSLINE3(1, buf4, nullbuf);
  copy_line(buf3, buf2, into_xdim);

  tprintf(5,"gauss_convolve_decimate3: preliminaries done.\n");
  tprintf(5,"gauss_convolve_decimate3: convolve vertically...\n");
 
  for (y=0; y<into_ydim; y++) {
    buft=buf0; buf0=buf2; buf2=buf4; buf4=buf1; buf1=buf3; buf3=buft;
    GAUSSLINE3(y+y+2, buf3, buf2);
    GAUSSLINE3(y+y+3, buf4, buf2);
    // Convolve Vertically
    gauss_reduce_loop_long(into_xdim, round_in, result_shift, log2_kb, kc, 
			   buf0, buf1, buf2, buf3, buf4, outbuf);
    putline (into_image, outbuf, 0, y);
  }
  tprintf(5,"gauss_convolve_decimate3: done\n");
  xxfree(inbuf); xxfree(outbuf); xxfree(buf0a); xxfree(buf1a); xxfree(buf2a); xxfree(buf3a); xxfree(buf4a);
  return(into_image);
}



#define GAUSS_1D_LOOP2(KC)\
 {for (from_i = xoff+ss4, to_i = xoff; to_i < xdim; to_i+=ss, from_i+=ss)\
   {xm2 = xm1; xm1 = x0; x0 = x1; x1 = x2;\
    x2 = buf[from_i];\
    outbuf[to_i] = (round_in + ((xm1+x1+x0+x0)<<log2_kb) + (KC)*(xm2+x2-x0-x0))\
      >>result_shift;}}

void 
gauss_1d_loop_long_var_step (int xdim, int *buf, int *outbuf, int kc, int result_shift,
			     int log2_kb, int round_in, int ss)
{
  //long xm2, xm1, x0, x1, x2;
  unsigned int xm2, xm1, x0, x1, x2;
  int from_i, to_i, xoff, ss2, ss3, ss4;
  // result_shift +=2; /* buf contains Lisp type T, therefore must shift right 2 extra bits */
  ss2 = ss+ss; ss3=ss2+ss; ss4=ss3+ss;
  for (xoff = 0; xoff<ss; xoff++) {
    xm1 = buf[xoff]; x0 = buf[xoff+ss]; x1 = buf[xoff+ss2]; x2 = buf[xoff+ss3]; 
    switch (kc)
      {case 1: GAUSS_1D_LOOP2(1);break;
      case 18: GAUSS_1D_LOOP2(18);break;
      case 9: GAUSS_1D_LOOP2(9);break;
      default: GAUSS_1D_LOOP2(kc);break;
      }
  }
}


#define GAUSSLINE4(j, outbuf, repbuf)\
 if (j < ydim)\
  {getline_padded(img, inbuf, j, start_bdr, end_bdr);\
   gauss_1d_loop_long_var_step(into_xdim, inbuf, outbuf, kc, intermediate_result_shift,\
			       log2_kb, intermediate_round_in, spacing);}\
  else if (repbuf) copy_line(repbuf, outbuf, into_xdim);
 
// Always called with BUFFER_TYPE = int 
template <class BUFFER_TYPE>  
extern image *
gauss_convolve_full_int(image *img, double ka, int spacing, image *into_image, BUFFER_TYPE xxx=0)
{
  int fast = (ka == .375)?1:0;
  int k = (fast)? 4:8;
  double ka0 = ka;
  double factor = (double) (1 << k);
  int kc = (int) (factor*(.5*(.5-ka0)));
  int element_size = image_element_size(img);
  int result_element_size = element_size;
  int intermediate_result_shift = (fast || (element_size+k+k< 31))?0:k;
  int result_shift = -(result_element_size - element_size - k - k + intermediate_result_shift);
  int round_in = 1 << (result_shift-1); 
  int intermediate_round_in = 1 << (intermediate_result_shift-1); 
  int log2_kb = k-2;

  int reduce = 1;
  int samp_spacing = 1;
  int xdim = image_xdim(img);
  int ydim = image_ydim(img);
  int into_xdim = xdim/reduce;
  int into_ydim = ydim/reduce;
  int insize = xdim + 5*spacing;
  int buflen = 5 * spacing;
  // BUFFER_TYPE inbuf[insize], outbuf[xdim];
  BUFFER_TYPE *inbuf = XALLOC(BUFFER_TYPE, insize);
  BUFFER_TYPE *outbuf = XALLOC(BUFFER_TYPE, xdim);
  //BUFFER_TYPE *bufs[buflen];
  BUFFER_TYPE **bufs = XALLOC(BUFFER_TYPE*,buflen);
  BUFFER_TYPE *buf, *buf0, *buf1, *buf2, *buf3, *buf4;
  BUFFER_TYPE *buft;
  int sp = spacing;
  int sp2 = sp+spacing;
  int sp3 = sp2+spacing;
  int sp4 = sp3+spacing;
  int start_bdr = sp2;
  int end_bdr   = sp2;
  int y, y1, y2, i;
  BUFFER_TYPE *nullbuf=0;

  
  //  require_image_rows foo1(img);
  //  require_image_rows foo2(into_image);

  for (i = 0; i < insize; i++) inbuf[i] = 0;

  for (i = 0; i < buflen; i++) bufs[i] = XALLOC(BUFFER_TYPE, xdim);
 
  tprintf(6,"Allocated %d buffers...priming the pipeline...\n", buflen);
  for (y = 0; y < sp; y++) {
    buf = bufs[y + sp2];
    GAUSSLINE4(y, buf, nullbuf);
    copy_line(buf, bufs[y], into_xdim);
    copy_line(buf, bufs[y + sp], into_xdim);
    GAUSSLINE4(y + sp, bufs[y+sp3], buf);
  }
  tprintf(6,"Done...now smoothing...(sp2=%d; sp3=%d; sp4=%d)\n", sp2, sp3, sp4);
 
  for (y1 = 0; y1 < ydim; y1 += sp) {
    for (y2 = 0; y2 < sp; y2++) {
      y = y1 + y2;
      if (y < ydim) {
	buf0 = bufs[y2]; buf1 = bufs[y2+sp]; buf2 = bufs[y2+sp2]; buf3 = bufs[y2+sp3]; buf4 = bufs[y2+sp4];

	GAUSSLINE4(y+sp2, buf4, buf3);

	// Convolve Vertically
	gauss_reduce_loop_long(into_xdim, round_in, result_shift, log2_kb, kc, 
			       buf0, buf1, buf2, buf3, buf4, outbuf);
	putline (into_image, outbuf, 0, y);

	bufs[y2] = buf1; bufs[y2+sp] = buf2; bufs[y2+sp2] = buf3; bufs[y2+sp3] = buf4; bufs[y2+sp4] = buf0;
      }
    }
  }
  tprintf(6,"Done smoothing...\n");
  if (0) {
    fprintf(stderr, "gauss_convolve_full_int: ");
    for (i = 0; i < buflen; i++) fprintf(stderr, "%p ", bufs[i]);
    fprintf(stderr, "\n");
  }
  for (i = 0; i < buflen; i++) xxfree(bufs[i]);
  tprintf(5,"gauss_convolve_full_int: done\n");
  xxfree(bufs); xxfree(inbuf); xxfree(outbuf);

  return(into_image);
}


// Float versions

template <class BUFFER_TYPE>  
void 
gauss_horiz_float_var_step (int xdim, BUFFER_TYPE *buf, BUFFER_TYPE *outbuf, BUFFER_TYPE kc, int ss)
{
  BUFFER_TYPE xm2, xm1, x0, x1, x2, x0px0;
  int from_i, to_i, xoff, ss2, ss3, ss4;
  ss2 = ss+ss; ss3=ss2+ss; ss4=ss3+ss;
  for (xoff = 0; xoff<ss; xoff++) {
    xm1 = buf[xoff]; x0 = buf[xoff+ss]; x1 = buf[xoff+ss2]; x2 = buf[xoff+ss3]; 
    for (from_i = xoff+ss4, to_i = xoff; to_i < xdim; to_i+=ss, from_i+=ss) {
      xm2 = xm1; xm1 = x0; x0 = x1; x0px0=x0+x0; x1 = x2; x2 = buf[from_i];	
      outbuf[to_i] = .25*(xm1+x1+x0px0) + kc*(xm2+x2-x0px0);
     }
  }
}

template <class BUFFER_TYPE>  
void 
gauss_vert_float (int xdim, BUFFER_TYPE kc, BUFFER_TYPE *bufm2, BUFFER_TYPE *bufm1, BUFFER_TYPE *buf0, BUFFER_TYPE *buf1, BUFFER_TYPE *buf2, BUFFER_TYPE *outbuf)
{
  int x;
  BUFFER_TYPE v0;
  for (x = 0; x<xdim; x++) {
    v0 = buf0[x];v0=v0+v0;
    outbuf[x] = .25*(bufm1[x]+buf1[x]+v0) + kc*(bufm2[x]+buf2[x]-v0);
  }
}

template <class BUFFER_TYPE>  
extern image *
gauss_convolve_full_float(image *img, BUFFER_TYPE ka, int spacing, image *into_image)
{
  int reduce = 1;
  int samp_spacing = 1;
  int xdim = image_xdim(img);
  int ydim = image_ydim(img);
  int into_xdim = xdim/reduce;
  int into_ydim = ydim/reduce;
  int insize = xdim + 5*spacing;
  int buflen = 5 * spacing;
  BUFFER_TYPE *inbuf = XALLOC(BUFFER_TYPE, insize);
  BUFFER_TYPE *outbuf = XALLOC(BUFFER_TYPE, xdim);
  //BUFFER_TYPE *bufs[buflen];
  BUFFER_TYPE **bufs = XALLOC(BUFFER_TYPE*,buflen);
  BUFFER_TYPE *buf, *buf0, *buf1, *buf2, *buf3, *buf4;
  BUFFER_TYPE *buft;
  int sp = spacing;
  int sp2 = sp+spacing;
  int sp3 = sp2+spacing;
  int sp4 = sp3+spacing;
  int start_bdr = sp2;
  int end_bdr   = sp2;
  int y, y1, y2, i;
  BUFFER_TYPE *nullbuf=0;
  BUFFER_TYPE kc = .5*(.5-ka);

  require_image_rows foo1(img);
  require_image_rows foo2(into_image);

  for (i = 0; i < insize; i++) inbuf[i] = 0;

  for (i = 0; i < buflen; i++) bufs[i] = XALLOC(BUFFER_TYPE, xdim);
 
#define GAUSS_HORIZ_FLOAT(y, outbuf, repbuf)	\
  if (y < ydim) {\
    getline_padded(img, inbuf, y, start_bdr, end_bdr);			\
    gauss_horiz_float_var_step(into_xdim, inbuf, outbuf, kc, spacing); \
   } else if (repbuf) copy_line(repbuf, outbuf, into_xdim);
 
  for (y = 0; y < sp; y++) {
    buf = bufs[y + sp2];
    GAUSS_HORIZ_FLOAT(y, buf, nullbuf);
    copy_line(buf, bufs[y], into_xdim);
    copy_line(buf, bufs[y + sp], into_xdim);
    GAUSS_HORIZ_FLOAT(y + sp, bufs[y+sp3], buf);
  }
 
  for (y1 = 0; y1 < ydim; y1 += sp) {
    for (y2 = 0; y2 < sp; y2++) {
      y = y1 + y2;
      if (y < ydim) {
	buf0 = bufs[y2]; buf1 = bufs[y2+sp]; buf2 = bufs[y2+sp2]; buf3 = bufs[y2+sp3]; buf4 = bufs[y2+sp4];

	GAUSS_HORIZ_FLOAT(y+sp2, buf4, buf3);

	// Convolve Vertically
	gauss_vert_float(into_xdim, kc, buf0, buf1, buf2, buf3, buf4, outbuf);
	putline (into_image, outbuf, 0, y);

	bufs[y2] = buf1; bufs[y2+sp] = buf2; bufs[y2+sp2] = buf3; bufs[y2+sp3] = buf4; bufs[y2+sp4] = buf0;
      }
    }
  }
  for (i = 0; i < buflen; i++) xxfree(bufs[i]);
  xxfree(bufs); xxfree(inbuf); xxfree(outbuf);

  return(into_image);
}





// Problems here: Sometimes we segfault when generating an image pyramid.  Why??

extern image *
gauss_convolve_decimate (image *img, double ka, image *into_image)
{
  switch (image_element_type(img)) {
  case IMG_DOUBLE_FLOAT:
  case IMG_SINGLE_FLOAT:
    tprintf(4,"gauss_convolve_decimate: float image.\n");
    return gauss_convolve_decimate2(img, ka, (double) .25, (double) (.5*(.5-ka)), (int) 0, (int) 0, ka, into_image); 
  default: {
    int fast = (ka == .375)?1:0;
    int k = (fast)? 4:8;
    double ka0 = ka;
    double factor = (double) (1 << k);
    //    int ka =(int) (factor*ka0);
    //int kb = (int) (factor*.25);
    tprintf(4,"gauss_convolve_decimate: int image.\n");
    int kc = (int) (factor*(.5*(.5-ka0)));
    int element_size = image_element_size(img);
    int result_element_size = element_size;
    int intermediate_shift = (fast || (element_size+k+k< 31))?0:k;
    int result_shift = -(result_element_size - element_size - k - k +intermediate_shift);
    int round_in = 1 << (result_shift-1); // FIXME -- what happens when result_shift=0 ? 
    int intermediate_round_in = 1 << (intermediate_shift-1); 
    int log_kb = k-2;
    return(gauss_convolve_decimate3(img, round_in, log_kb, kc, result_shift, intermediate_shift, intermediate_round_in, into_image));
    }
  }
}

extern image *
gauss_convolve_full(image *img, double ka, int spacing, image *into_image)
{
  switch (image_element_type(img)) {
  case IMG_DOUBLE_FLOAT:
  case IMG_SINGLE_FLOAT:
    return gauss_convolve_full_float(img, ka, spacing, into_image);
  default: 
    return gauss_convolve_full_int (img, ka, spacing, into_image, (int) 1);
  }
}

// Gauss convolution without decimation.
extern image *
gauss_convolve_image (image *img, double ka, image *into_image, int level, image *spare)
{
  int xdim = img->xdim;
  int ydim = img->ydim;
  
  gauss_convolve_full(img, ka, 1, into_image);

  if (level > 1) {
    for (int iter = 1; iter < level; iter++)
      gauss_convolve_full(into_image, ka,  1 << iter, into_image);
    }

  return(into_image);
}



extern image *
fast_gauss_convolve(image *img, image *into, int level, double ka, image *scratch) {
  return gauss_convolve_image(img, ka, into, level, scratch);
}

#if 0
// No callers.
extern void
build_image_pyramid (image *img, char *base_path, int to_level, int from_level)
{int level;
 for (level = from_level; level<=to_level; level++ ) {
   // char path[strlen(base_path)+5];
   char *path = XALLOC(char, strlen(base_path)+5);
    sprintf(path, "%s%d", base_path, level);
    save_image(img, path);
    xxfree(path);
    if (level<to_level)
      img = gauss_convolve_decimate(img);
   }
}
#endif

END_NAMESPACE_FREEDIUS





/*

x0 x1 x2 x3 x4 x5 x6 x7 x8 

kc*(x0+x4) + kb*(x1+x3) + ka*x2

kc*(x2+x6) + kb*(x3+x5) + ka*x4

kc*(x4+x8) + kb*(x5+x7) + ka*x6

for (i=i0, j=0; i<n; i+=4, j+=2) {
  x3=buf[i+3]; x4=buf[i+4]; x5=buf[i+5]; x6=buf[i+6];
  obuf[j]   = kc*(x0+x4) + kb*(x1+x3) + ka*x2;
  obuf[j+1] = kc*(x2+x6) + kb*(x3+x5) + ka*x4;
  x0=x4; x1=x5; x2=x6;
}


// prime the loop
x0=buf[0];x1=buf[1];x2=buf[2];
// (4 mult 10 add) * n/4
for (i=0, j=0; i<n; i+=4, j+=2) {
  x3=buf[i+3]; x4=buf[i+4]; x5=buf[i+5]; x6=buf[i+6];
  x22=x2+x2; x44 = x4+x4;
  obuf[j]   = .25*(x1+x3+x22) + kc*(x0+x4-x22);
  obuf[j+1] = .25*(x3+x5+x44) + kc*(x2+x6-x44);
  // slide tmps 
  x0=x4; x1=x5; x2=x6;
}

// prime the loop
x0=buf[0];x1=buf[1];x2=buf[2];
// (2 mult 5 add) * n/2
for (i=0, j=0; i<n; i+=2, j++) {
  x3=buf[i+3]; x4=buf[i+4];
  x22=x2+x2;
  obuf[j]   = .25*(x1+x3+x22) + kc*(x0+x4-x22);
  // slide tmps 
  x0=x2; x1=x3; x2=x4;
}


*/
