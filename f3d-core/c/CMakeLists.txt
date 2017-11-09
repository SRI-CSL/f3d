cmake_minimum_required(VERSION 2.8)

if ( WIN32 AND (NOT MINGW) )
set(libname libfreedius)
else ( WIN32 AND (NOT MINGW) )
set(libname freedius)
endif ( WIN32 AND (NOT MINGW) )

if ( WIN32 )
message( "Compiling " ${libname} " for Windows." )
endif ( WIN32 )

project(${libname})

# Streamlined cross-platform cmake file for FREEDIUS.  This file accomodates Linux, Mac OS X, and Windows.

#rm -rf CMakeCache.txt CMakeFiles cmake_install.cmake install_manifest.txt Makefile

# This version is meant to be run from within lisp by cl-cmake (asdf
# extensions that allow you to use cmake).  See cl-cmake to see how
# cmake is invoked "fresh".  

if(WIN32)
  find_library(PSAPI PSAPI)
  message("!! PSAPI = " ${PSAPI})
else(WIN32)
  set(PSAPI "")
#  set(ARCHIVE "")
endif(WIN32)

include("../f3d-cmake-setup.cmake")

if (NOT INSTALL_PREFIX)
  set(INSTALL_PREFIX ${F3DA})
endif(NOT INSTALL_PREFIX)

#add_subdirectory(lisptk)
#add_subdirectory(libfreedius)

# These are no longer necessary:

#include(FindTIFF)
#include(FindJPEG)

# If we're using local copies, do this:
set(TIFF_FOUND 1)
set(JPEG_FOUND 1)

# These are fallbacks - if you cannot find the TIFF and JPEG libs and
# include files, then we have to assume it's WIndows & place the
# required files in places that are already on the search paths.

if(NOT TIFF_FOUND)
  message("**  TIFF not found.  Please copy TIFF include files into ${INSTALL_PREFIX}/include")
  message("**                   Please copy libtiff and zlib into ${INSTALL_PREFIX}/lib")
endif(NOT TIFF_FOUND)

if(NOT JPEG_FOUND)
  message("**  JPEG not found.  Please copy JPEG include files into ${INSTALL_PREFIX}/include")
  message("**                   Please copy libjpeg into ${INSTALL_PREFIX}/lib")
endif(NOT JPEG_FOUND)

if(WIN32)
  if(NOT TIFF_FOUND)
    set(TIFF_LIBRARIES "${LIBRARY_INSTALL_DIR}/tiff.lib")
    set(TIFF_INCLUDE_DIR "${INSTALL_PREFIX}/include")
  endif(NOT TIFF_FOUND)
  if(NOT JPEG_FOUND)
    set(JPEG_LIBRARIES "${LIBRARY_INSTALL_DIR}/jpeg.lib")
    set(JPEG_INCLUDE_DIR "${INSTALL_PREFIX}/include")
  endif(NOT JPEG_FOUND)
endif(WIN32)


# Set the include directories for cme-local TIFF and JPEG:

set(TIFF_INCLUDE_DIR ./tiff/)
set(JPEG_INCLUDE_DIR ./jpeg/)
set(ZLIB_INCLUDE_DIR ./zlib/)


include_directories(./include ${ZLIB_INCLUDE_DIR} ${JPEG_INCLUDE_DIR} ${TIFF_INCLUDE_DIR} ${CMAKE_CURRENT_BINARY_DIR})


SET(MISC_SRCS
#	misc/base-class.c++   # the only things in base-class.c++ are unnecessary new/delete operators
	misc/freedius_so_init.c++ 
	misc/cme-error.c++ 
	misc/list.c++ 
	misc/io.c++ 
	misc/misc.c++ 
	misc/run-program.c++ 
	misc/cme_time.c++)

SET(IMG_SRCS
	img/image.c++ 
	img/array-image-accessors.c++ 
	img/array-image.c++ 
	img/page_handler.c++
	img/paged-image.c++ 
	img/paged-image-accessors.c++ 
	img/file-image.c++  # where does this belong -- combination of image-basics and image-io
        img/image-getput.c++ 
	img/lazy-image.c++ 
	img/cimage.c++ 
	img/gauss-convolve.c++ 
	img/box-filter-decimate.c++ 
	img/color.c++
	)

SET(IMG_IO_SRCS
	img/image-io.c++ 
	img/file-image-io.c++ 
	img/iu-testbed-io.c++ 
	img/tiff-io.c++ 
	img/tiff-ffi.c 
	img/sunras.c++ 
	img/bmp-io.c++
	img/jpeg-io.c++
        img/jpeg-mem-src.c++
        img/jpeg-mem-dst.c++
	img/ppm-io.c++
	)

SET(JPEG_SRCS
  jpeg/jcapimin.c
  jpeg/jcapistd.c
  jpeg/jccoefct.c
  jpeg/jccolor.c
  jpeg/jcdctmgr.c
  jpeg/jchuff.c
  jpeg/jcinit.c
  jpeg/jcmainct.c
  jpeg/jcmarker.c
  jpeg/jcmaster.c
  jpeg/jcomapi.c
  jpeg/jcparam.c
  jpeg/jcphuff.c
  jpeg/jcprepct.c
  jpeg/jcsample.c
  jpeg/jctrans.c
  jpeg/jdapimin.c
  jpeg/jdapistd.c
  jpeg/jdatadst.c
  jpeg/jdatasrc.c
  jpeg/jdcoefct.c
  jpeg/jdcolor.c
  jpeg/jddctmgr.c
  jpeg/jdhuff.c
  jpeg/jdinput.c
  jpeg/jdmainct.c
  jpeg/jdmarker.c
  jpeg/jdmaster.c
  jpeg/jdmerge.c
  jpeg/jdphuff.c
  jpeg/jdpostct.c
  jpeg/jdsample.c
  jpeg/jdtrans.c
  jpeg/jerror.c
  jpeg/jfdctflt.c
  jpeg/jfdctfst.c
  jpeg/jfdctint.c
  jpeg/jidctflt.c
  jpeg/jidctfst.c
  jpeg/jidctint.c
  jpeg/jidctred.c
  jpeg/jquant1.c
  jpeg/jquant2.c
  jpeg/jutils.c
  jpeg/jmemmgr.c
  jpeg/jmemnobs.c
  )

include(CheckFunctionExists)
include(CheckIncludeFile)

#
# Configuration for TIFF:
#
check_include_file(assert.h HAVE_ASSERT_H)
check_include_file(fcntl.h HAVE_FCNTL_H)
check_include_file(io.h HAVE_IO_H)
check_function_exists(jbg_newlen HAVE_JBG_NEWLEN)
check_function_exists(mmap HAVE_MMAP)
check_include_file(search.h HAVE_SEARCH_H)
check_include_file(string.h HAVE_STRING_H)
check_include_file(sys/types.h HAVE_SYS_TYPES_H)
check_include_file(unistd.h HAVE_UNISTD_H)

if(WIN32)
  set(USE_WIN32_FILEIO 1)
  set(TIFFIO_SRC tiff/tif_win32.c)
else(WIN32)
  set(TIFFIO_SRC tiff/tif_unix.c)
endif(WIN32)

configure_file("tiff/tif_config.h.cmakein"
               "${CMAKE_CURRENT_BINARY_DIR}/tif_config.h" @ONLY)



SET(TIFF_SRCS
    tiff/tif_aux.c
    tiff/tif_close.c
    tiff/tif_codec.c
    tiff/tif_color.c
    tiff/tif_compress.c
    tiff/tif_dir.c
    tiff/tif_dirinfo.c
    tiff/tif_dirread.c
    tiff/tif_dirwrite.c
    tiff/tif_dumpmode.c
    tiff/tif_error.c
    tiff/tif_extension.c
    tiff/tif_fax3.c
    tiff/tif_fax3sm.c
    tiff/tif_flush.c
    tiff/tif_getimage.c
    tiff/tif_jpeg.c
    tiff/tif_luv.c
    tiff/tif_lzw.c
    tiff/tif_next.c
    tiff/tif_ojpeg.c
    tiff/tif_open.c
    tiff/tif_packbits.c
    tiff/tif_pixarlog.c
    tiff/tif_predict.c
    tiff/tif_print.c
    tiff/tif_read.c
    tiff/tif_stream.cxx
    tiff/tif_strip.c
    tiff/tif_swab.c
    tiff/tif_thunder.c
    tiff/tif_tile.c
    tiff/tif_version.c
    tiff/tif_warning.c
    tiff/tif_write.c
    tiff/tif_zip.c
    tiff/t4.h
    tiff/tif_dir.h
    tiff/tif_fax3.h
    tiff/tiff.h
    tiff/tiffio.h
    tiff/tiffiop.h
    tiff/tiffvers.h
    tiff/tif_predict.h
    tiff/uvcode.h
    tiff/tiffio.hxx
    "${CMAKE_CURRENT_BINARY_DIR}/tif_config.h"
    )


SET(ZLIB_SRCS
  zlib/adler32.c
  zlib/compress.c
  zlib/crc32.c
  zlib/gzio.c
  zlib/uncompr.c
  zlib/deflate.c
  zlib/trees.c
  zlib/zutil.c
  zlib/inflate.c
  zlib/infback.c
  zlib/inftrees.c
  zlib/inffast.c
  )



# C++ sucks -- -DUSE_CPP_INITS doesn't work because one cannot predict the order of static initializers.
# add_definitions(-DUSE_CPP_INITS=1)

add_library(${libname} SHARED
  ${ZLIB_SRCS}
  ${JPEG_SRCS}
  ${TIFF_SRCS}
  ${TIFFIO_SRC}
  ${MISC_SRCS} 
  ${IMG_SRCS}
  ${IMG_IO_SRCS}
  )


# On various systems, some of these will be null - and now that TIFF
# and JPEG are built-in, those will always be null:

target_link_libraries(${libname} ${JPEG_LIBRARIES} ${TIFF_LIBRARIES} ${OPENGL_LIBRARIES} ${CARBON} ${PSAPI} )


#
# Link options / libraries / frameworks must be conditionalized on
# OS... OpenGL is known by different names on each of Linux, MacOSX,
# and Win Doze.  Once we dice out the needed tests, this can/should be
# simplified:
#

if(WIN32)
  if( LISP MATCHES "sbcl*" )
    set(FREEDIUS_DEF "${CMAKE_CURRENT_SOURCE_DIR}/freedius-sbcl.def")
  else ( LISP MATCHES "sbcl*" )
    set(FREEDIUS_DEF "${CMAKE_CURRENT_SOURCE_DIR}/freedius-acl.def")
  endif ( LISP MATCHES "sbcl*" )
  FILE(TO_CMAKE_PATH ${FREEDIUS_DEF} DEFFILE)
  message("** Exporting symbols from  ${DEFFILE}")
  if (MINGW)
    target_link_flags(${libname} "-Wl,--output-def,\"${DEFFILE}\"")
  else (MINGW)
    target_link_flags(${libname} "/DEF:\"${DEFFILE}\"")
  endif (MINGW)
  message("** Library install dir = ${LIBRARY_INSTALL_DIR}")
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${libname}.dll DESTINATION ${LIBRARY_INSTALL_DIR})
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${libname}.lib DESTINATION ${LIBRARY_INSTALL_DIR})
else(WIN32)
  # this in needed to get -m32 as needed from LINK_FLAGS
  target_link_flags(${libname} "") # target_link_flags also adds -m32 flag when needed
endif(WIN32)


if(WIN32)
#  install(TARGETS ${libname}      LIBRARY DESTINATION lib       ARCHIVE DESTINATION lib )
#  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${libname}.dll DESTINATION ${LIBRARY_INSTALL_DIR} RENAME lib${libname}.dll)
#  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${libname}.lib DESTINATION ${LIBRARY_INSTALL_DIR} RENAME lib${libname}.lib)
else(WIN32)
  install(TARGETS ${libname} LIBRARY DESTINATION ${LIBRARY_INSTALL_DIR})
  ADD_CUSTOM_TARGET(tar
    tar --exclude=.svn --exclude=arch -zcvf ${CMAKE_CURRENT_BINARY_DIR}/freedius.tgz  .
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/..
    VERBATIM)
endif(WIN32)

