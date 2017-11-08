set(libname f3dmisc)

project(${libname})

include("${F3D}/c/f3d-cmake-setup.cmake")

include_directories(${F3D}/c/include ${INSTALL_PREFIX}/c)

add_library(${libname} SHARED freedius_so_init.c++ cme-error.c++ misc.c++ io.c++ run-program.c+ list.c++ )

target_link_flags(${libname} "-Wall")

if(WIN32)
  install(TARGETS ${libname} LIBRARY ARCHIVE DESTINATION ${LIBRARY_INSTALL_DIR})
else(WIN32)
  install(TARGETS ${libname} LIBRARY DESTINATION ${LIBRARY_INSTALL_DIR})
endif(WIN32)

