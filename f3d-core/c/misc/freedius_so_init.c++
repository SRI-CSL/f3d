// -*- Mode: C++; c-basic-offset: 2

// $Id: freedius_so_init.c++,v 1.10.8.1 2007/03/19 05:07:41 quam Exp $

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>

#include "base-class.h"
#include "array-image.h"
#include "file-image.h"
#include "image-io.h"
#include "image.h"

// Don't include these.  In the case of jpeg, especially, there are other conflicting includes involved.
#if 1
#include "iu-testbed-io.h"
#include "tiff-io.h"
#include "bmp-io.h"
#include "jpeg-io.h"
#include "ppm-io.h"
#endif


#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

/* Having trouble with jpeg under wINdOzE */
#ifdef _WIN32
extern void start_warning_log(char*);
#else
#endif


void init_color_tables();

#if 0
extern int init_iu_testbed_array_image_header();
extern  int init_iu_testbed_file_image_header();
extern  int init_jpeg_image_header();
extern  int init_bmp_array_image_header();
extern  int init_tiff_array_image_header();
extern  int init_jpeg_image_header();
extern  int init_ppm_image_header();
#endif

extern "C" {

  /* Sat May 16 2009 LHQ
     I am working on eliminating freedius_so_init so that the system is more modular.

     There are problems with anything related to image_header_list since one cannot control the 
     order of C++ initializers.  Need to move find_image_header functionality into Lisp so that 
     image_header_list can be eliminated.  

     WRONG WRONG WRONG  Cannot predict the order of C static initializers.
     The remaining initializers init_array_image_fn init_make_file_image_fn 
     init_image_choose_image_class_fn and init_color_tables can be handled with 
     C++ static initializers.
 
     When make_image and image-io are moved into Lisp, everything here goes away.
   */

extern int FREEDIUS_GLOBAL(freedius_so_init) (void)
{

  if (1) {
    tprintf (2, "freedius_so_init start\n");
 
#ifdef _WIN32
    // use static-initializer instead?  No, order of initializers is not predictable.
    start_warning_log("c:/freedius-stderr.log");
#endif
 
#if !defined(USE_CPP_INITS)
    init_array_image_fn();
    init_make_file_image_fn();
    init_image_choose_image_class_fn();
    //init_color_tables();  // let the initializer in color.c++ do this
#endif // !defined(USE_CPP_INITS)
 
    image_header_list = 0;
    // The image_header_list will be ordered according to the order of these calls.
    // add_image_header nconcs to the end of the list.
    tprintf (2, "freedius_so_init - initializing image headers...\n");
    init_iu_testbed_file_image_header(); 
    init_iu_testbed_array_image_header();
    init_tiff_array_image_header();
    init_bmp_array_image_header();
    init_jpeg_image_header();
    init_ppm_image_header();
    tprintf (2, "freedius_so_init - ...done.\n");
   
    tprintf (2, "freedius_so_init completed\n");
  }
  return 1;
}

} // end extern "C"

END_NAMESPACE_FREEDIUS
