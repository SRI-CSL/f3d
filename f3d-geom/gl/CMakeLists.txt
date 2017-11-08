cmake_minimum_required(VERSION 2.8)

# f3dglffi - C helper functions and definitions for the f3d OpenGL API.

if( WIN32 )
set(libname libf3dglffi)
else( WIN32 )
set(libname f3dglffi)
endif( WIN32 )

project(${libname})

# This library's source directory sits two levels down from its
# sibling f3d-core, which contains the common cmake setup and the
# c/include directory:

include("../../f3d-core/f3d-cmake-setup.cmake")

include_directories(../../f3d-core/c/include ${INSTALL_PREFIX}/c)

# With this conditional in place, do we need glxffi.c++ in the Cocoa
# or Carbon versions?  Should we add a windows variable?

if ( FREEDIUS_USE_COCOA )
  add_library(${libname} SHARED 
		       glffi.c++ 
		       glxffi.c++ 
		       wglffi.c++ 
		       cocoa-gl-ffi.m
		       lispgl.c++ )
elseif ( FREEDIUS_USE_CARBON )
  add_library(${libname} SHARED 
		       glffi.c++ 
		       glxffi.c++ 
		       wglffi.c++ 
      		       aglffi.c++ 
		       lispgl.c++ )
else ( FREEDIUS_USE_COCOA )
  add_library(${libname} SHARED 
		       glffi.c++ 
		       glxffi.c++ 
		       wglffi.c++ 
		       lispgl.c++ )
endif ( FREEDIUS_USE_COCOA )

target_link_flags(${libname} "-Wall")

if(WIN32)
  find_library(PSAPI PSAPI)
  message("!! PSAPI = " ${PSAPI})
  set(F3DLIB "${CMAKE_CURRENT_BINARY_DIR}/../../f3d-core/c/libfreedius.lib")
else(WIN32)
  set(PSAPI "")
  set(F3DLIB "")
endif(WIN32)

# We still need to know where to find Tk includes.  We will need to
# use these to pull out native window information, but otherwise we
# should NOT have to link to the Tcl/Tk libs...

include(FindTCL)
include(FindOpenGL)

set(CARBON "")
if(APPLE)
    if ( FREEDIUS_USE_CARBON )
      set(CARBON "-framework Carbon")
    elseif ( FREEDIUS_USE_COCOA )
      set(COCOA "-framework Cocoa")
    elseif ( FREEDIUS_USE_X11 )
      FIND_PATH ( TCL_LIBRARY_PATH libtcl
        /opt/local/lib
        /sw/lib
      )

      FIND_PATH( X11_INCLUDE_DIR GL
        /opt/local/include
        /usr/X11/include
        /usr/include/X11
	/opt/local/include/X11
      )

      FIND_PATH( X11GL_LIB_PATH libGL.dylib
        /opt/local/lib
        /usr/X11/lib
        /usr/lib/X11
	/opt/local/lib/X11
      )
      
      message( "Using X11 include dir " ${X11_INCLUDE_DIR} )
      INCLUDE_DIRECTORIES( ${X11_INCLUDE_DIR} )

      message( "Using GL libraries " ${X11GL_LIB_PATH} )
      SET (OPENGL_LIBRARIES "-L${X11GL_LIB_PATH} -lGL -lGLU")
#     set( OPENGL_LIBRARIES "-L${X11_LIB_DIR} -lGL -lGLU" )
      set( COCOA "" )
#      set( GLUT_LIBRARIES "-lglut" )
    endif ( FREEDIUS_USE_CARBON )
endif(APPLE)

# this in needed to get -m32 as needed from LINK_FLAGS
# target_link_flags(${libname} "") # target_link_flags also adds -m32 flag when needed

if(WIN32)
  set(GLFFI_DEF "${CMAKE_CURRENT_SOURCE_DIR}/f3dglffi.def")
  FILE(TO_CMAKE_PATH ${GLFFI_DEF} GLFFI_DEF_CMAKE)
  message("** Exporting symbols from  ${GLFFI_DEF_CMAKE}")
  target_link_flags(${libname} "/DEF:\"${GLFFI_DEF_CMAKE}\"")
else(WIN32)
  if(APPLE)
    target_link_flags(${libname} "-undefined warning -flat_namespace") # target_link_flags also adds -m32 flag when needed
  endif(APPLE)
endif(WIN32)

message("** OpenGL: ${OPENGL_LIBRARIES}")

target_link_libraries(${libname} ${F3DLIB} ${OPENGL_LIBRARIES} ${GLUT_LIBRARIES} ${COCOA} ${PSAPI})

if(WIN32)
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${libname}.dll DESTINATION ${LIBRARY_INSTALL_DIR} RENAME lib${libname}.dll)
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${libname}.lib DESTINATION ${LIBRARY_INSTALL_DIR} RENAME lib${libname}.lib)
else(WIN32)
  install(TARGETS ${libname} LIBRARY DESTINATION ${LIBRARY_INSTALL_DIR})
endif(WIN32)
