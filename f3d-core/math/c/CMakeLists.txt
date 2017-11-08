set(libname f3dmath)

project(${libname})

include("${F3D}/c/f3d-cmake-setup.cmake")

include_directories(${F3D}/c/include ${INSTALL_PREFIX}/c)

add_definitions(-DF3DMODULAR=1)

add_library(${libname} SHARED matrices.c++ solve.c++)


if(WIN32)
  set(MATH_DEF "$ENV{FREEDIUS}\\c\\math.def")
  FILE(TO_CMAKE_PATH ${MATH_DEF} MATH_DEF_CMAKE)
  target_link_flags(${libname} "/DEF:${MATH_DEF_CMAKE}")
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/f3dmath.dll DESTINATION ${LIBRARY_INSTALL_DIR} RENAME libf3dmath.dll)
else(WIN32)
  target_link_flags(${libname} "-Wall")
  install(TARGETS ${libname} LIBRARY DESTINATION ${LIBRARY_INSTALL_DIR})
endif(WIN32)

