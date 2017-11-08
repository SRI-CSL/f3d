cmake_minimum_required(VERSION 2.8)

#if(WIN32)
#set(libname liblisptk)
#else(WIN32)
set(libname lisptk)
#endif(WIN32)

project(${libname})

include("../f3d-cmake-setup.cmake")
include_directories(../c/include ${INSTALL_PREFIX}/c )

add_library(${libname} SHARED lisptk.c)
#target_link_libraries(${libname} tk tcl)
#target_link_flags(${libname} "-Wall") # target_link_flags also adds -m32 flag when needed

include(FindTCL)
#
# Dangerous: this will also include the Framework include files.  
# MacOSX may have both Cocoa and X11:


include_directories( ${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH} )

#
# link options / libraries / frameworks must be conditionalized on OS:
#
if(APPLE)
  target_link_flags(${libname} "-Wl,-flat_namespace,-single_module -Wall -undefined warning")
  include_directories( ${TK_INCLUDE_PATH_PARENT}/PrivateHeaders )
# This must always be done on APPLE:
#  target_link_libraries(${libname} ${TCL_LIBRARY} ${TK_LIBRARY} )
else(APPLE)
  if(WIN32)
    if ( MINGW )
      target_link_flags(${libname} "-Wl,--export-all-symbols")
    else ( MINGW )
        if( LISP STREQUAL "allegro" )
    	  set(LISPTK_DEF "${CMAKE_CURRENT_SOURCE_DIR}/lisptk-acl.def")
	else ( LISP STREQUAL "allegro" )
	  set(LISPTK_DEF "${CMAKE_CURRENT_SOURCE_DIR}/lisptk-sbcl.def")
	endif ( LISP STREQUAL "allegro" )
	FILE(TO_CMAKE_PATH ${LISPTK_DEF} LISPTK_DEF_CMAKE)
        message("** Exporting symbols from  ${LISPTK_DEF_CMAKE}")
        target_link_flags(${libname} "/DEF:\"${LISPTK_DEF_CMAKE}\"")
    endif ( MINGW )
  else(WIN32)
    target_link_flags(${libname} "-Wall") # target_link_flags also adds -m32 flag when needed
  endif(WIN32)
endif(APPLE)

message("** TCL_LIBRARY = ${TCL_LIBRARY}")
message("** TK_LIBRARY = ${TK_LIBRARY}")

#if (APPLE)
#target_link_libraries(${libname} ${TCL_LIBRARY} ${TK_LIBRARY} )
#endif (APPLE)

# set(NOLINK_TCL_TK $ENV{NOLINK_TCL_TK})
if (WIN32)
set(NOLINK_TCL_TK false)
else (WIN32)
set(NOLINK_TCL_TK true)
endif (WIN32)


if (NOT NOLINK_TCL_TK)
target_link_libraries(${libname} ${TCL_LIBRARY} ${TK_LIBRARY} )
endif(NOT NOLINK_TCL_TK)

#if (APPLE)
#target_link_libraries(${libname} ${TCL_LIBRARY} ${TK_LIBRARY} )
#endif(APPLE)

if(WIN32)
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/lisptk.dll DESTINATION ${LIBRARY_INSTALL_DIR} RENAME liblisptk.dll)
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/lisptk.lib DESTINATION ${LIBRARY_INSTALL_DIR} RENAME liblisptk.lib)
else(WIN32)
  install(TARGETS ${libname} LIBRARY DESTINATION ${LIBRARY_INSTALL_DIR})
endif(WIN32)

# cd $FREEDIUS_ARCH/c/lisptk
#cmake -DF3D=$FREEDIUS -DINSTALL_PREFIX=$FREEDIUS_ARCH -DCMAKE_VERBOSE_MAKEFILE=1 -DLISP=sbcl64 $FREEDIUS/c/lisptk
#rm -rf CMakeCache.txt CMakeFiles cmake_install.cmake install_manifest.txt Makefile
