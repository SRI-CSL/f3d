cmake_minimum_required(VERSION 2.8)

#if( WIN32 )
#set(libname libf3dmath)
#else ( WIN32 )
set(libname f3dmath)
#endif ( WIN32 )

project(${libname})

# General cmake setup directives live at the top level cl-cme-tk
# source directory:
include("../f3d-cmake-setup.cmake")

# Convention: All FREEDIUS-wide includes live in the main FREEDIUS c
# source directory:

include_directories(../c/include)

add_definitions(-DF3DMODULAR=1)

add_library(${libname} SHARED matrices.c++ solve.c++)


if(WIN32)
  if ( MINGW )
    target_link_flags( ${libname} "-Wl,--export-all-symbols" )
  else ( MINGW )
    set(MATH_DEF "${CMAKE_CURRENT_SOURCE_DIR}/math.def")
    FILE(TO_CMAKE_PATH ${MATH_DEF} MATH_DEF_CMAKE)
    target_link_flags(${libname} "/DEF:\"${MATH_DEF_CMAKE}\"")
  endif ( MINGW )
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/libf3dmath.dll DESTINATION ${LIBRARY_INSTALL_DIR})
else(WIN32)
  target_link_flags(${libname} "-Wall")
  install(TARGETS ${libname} LIBRARY DESTINATION ${LIBRARY_INSTALL_DIR})
endif(WIN32)

