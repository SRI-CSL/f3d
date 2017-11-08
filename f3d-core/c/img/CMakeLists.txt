set(libname f3dimage)

project(${libname})

include("${F3D}/c/f3d-cmake-setup.cmake")

include_directories(${F3D}/c/include ${INSTALL_PREFIX}/c)

add_library(${libname} SHARED 
		     image.c++ 
		     array-image.c++ array-image-accessors.c++
		     paged-image.c++ paged-image-accessors.c++ page_handler.c++
		     lazy-image.c++
		      )

target_link_flags(${libname} "-Wall")

if(WIN32)
  install(TARGETS ${libname} LIBRARY ARCHIVE DESTINATION ${LIBRARY_INSTALL_DIR})
else(WIN32)
  install(TARGETS ${libname} LIBRARY DESTINATION ${LIBRARY_INSTALL_DIR})
endif(WIN32)

# cmake . -DF3D=$FREEDIUS -DINSTALL_PREFIX=$FREEDIUS_ARCH -DCMAKE_VERBOSE_MAKEFILE=1 -DCMAKE_BUILD_TYPE=debug -DLISP=sbcl64
