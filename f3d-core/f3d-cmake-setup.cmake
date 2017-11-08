#-*-cmake-*-
#
# This simple CMake extension makes sure that builds get
# created inside a BUILD/os-osversion-arch directory
# and that executables, obj files and lib files get placed
# correctly in subdirectories.
#  
#  Recognized variables:  LISP ARCH INSTALL_ARCHDIR INSTALL_PREFIX LIBRARY_INSTALL_DIR
#
#  The target Lisp system is determined by the value of the variable LISP.
#
#  Library files are installed in the subdirectory ${LIBRARY_INSTALL_DIR}
#
#  If not specified, LIBRARY_INSTALL_DIR is set to CMAKE_INSTALL_PREFIX.
#
#
#  CMAKE_INSTALL_PREFIX is set the the value
#    $INSTALL_PREFIX                                 if INSTALL_PREFIX is supplied
#    ${INSTALL_ARCHDIR}/${CMAKE_SYSTEM_NAME}-${LISP} if INSTALL_ARCHDIR is supplied 
#                                                       and INSTALL_PREFIX is not supplied
#    ${CMAKE_SOURCE_DIR}/arch/${CMAKE_SYSTEM_NAME}-${LISP} otherwise
#
#  Examples:
#  
#   cmake . -DLISP=cmucl -DINSTALL_PREFIX=$FREEDIUS_ARCH -DCMAKE_VERBOSE_MAKEFILE=1 -DCMAKE_BUILD_TYPE=debug
#     
#   cmake . -DLISP=sbcl64 -DINSTALL_ARCHDIR=/tmp/cmake-tests/arch
#
# See freedius-systems/proprietary/aerovironment/CMakeLists.txt for more examples.


# On APPLE, we need a single variable that tells us whether we should
# try to find native Tcl/Tk and OpenGL, or to use the X11 versions.
# FREEDIUS_USE_X11 will force this and use X11 instead of Cocoa or Carbon.

# with nested cmake directories add_definitions will add multiple times.
remove_definitions(-DHAVE_SBCL64 -DHAVE_SBCL -DHAVE_CMUCL -DHAVE_ALLEGRO -DHAVE_ALLEGRO64)
remove_definitions(-DUNIX -DLINUX -DSUNOS -DDARWIN -D_WIN32 -DMINGW)
remove_definitions(-DF3DMODULAR)
remove_definitions(-m32)

# __________________________________________

# Macro to check Lisp type and to guess whether we need 64 or 32 bit
# compiles:

macro(check_lisp)
  set(LISP $ENV{LISP})

#  set(CMAKE_BUILD_ARCH 32) 
#  set(CMAKE_BUILD_ARCH 64)

# this tries to guess LISP if it's not provided.  Ho ho ho...  
  if(NOT LISP)
    if(APPLE)
      set(LISP "sbcl64") # Default on Mac is now SBCL 64-bit
    elseif(UNIX)
      if(OS_ARCH MATCHES ".*x86_64.*")
	set(LISP "sbcl64")
	else(OS_ARCH MATCHES ".*x86_64.*")
	set(LISP "sbcl32")
      endif(OS_ARCH MATCHES ".*x86_64.*")
    elseif(WIN32)
      set(LISP "sbcl32")
    endif(APPLE)
   endif(NOT LISP)

# Given LISP, now try to guess other relevant flags:
  if(LISP STREQUAL "sbcl64")
    set(CMAKE_BUILD_ARCH 64)
    add_definitions(-DHAVE_SBCL64 -DHAVE_SBCL)
  elseif(LISP STREQUAL "sbcl32")
    set(CMAKE_BUILD_ARCH 32)
    add_definitions(-DHAVE_SBCL)
  elseif(LISP STREQUAL "cmucl")
    set(CMAKE_BUILD_ARCH 32)
    add_definitions(-DHAVE_CMUCL)
  elseif(LISP STREQUAL "allegro")
    set(CMAKE_BUILD_ARCH 32)
    add_definitions(-DHAVE_ALLEGRO)
  elseif(LISP STREQUAL "acl64")
    set(CMAKE_BUILD_ARCH 64)
    add_definitions(-DHAVE_ALLEGRO -DHAVE_ALLEGRO64)
  endif(LISP STREQUAL "sbcl64")

  if(NOT LISP MATCHES "^(sbcl32|sbcl64|cmucl|allegro|acl64)$")
    message(FATAL_ERROR 
      "LISP must be one of sbcl32 sbcl64 cmucl allegro")
  endif(NOT LISP MATCHES "^(sbcl32|sbcl64|cmucl|allegro|acl64)$")

endmacro(check_lisp)

check_lisp()

# __________________________________________
# Macro to check architecture
#

# So if check_lisp tells us to do 32 or 64 bit, why do we need this???

macro(check_architecture)    

#  if(NOT CMAKE_BUILD_ARCH)
#    set(CMAKE_BUILD_ARCH $ENV{CMAKE_BUILD_ARCH})
#  endif(NOT CMAKE_BUILD_ARCH)

  if(NOT CMAKE_BUILD_ARCH)
    set(CMAKE_BUILD_ARCH "Native")
  endif(NOT CMAKE_BUILD_ARCH)

  if(NOT CMAKE_BUILD_ARCH MATCHES "^(Native|64|32)$")
    message(FATAL_ERROR 
      "CMAKE_BUILD_ARCH set but invalid.  "
      "Only Native, 64 and 32 are valid settings")
  endif(NOT CMAKE_BUILD_ARCH MATCHES "^(Native|64|32)$")

  #
  # Set Native architecture based on void* size
  #
  include(CheckTypeSize)
  check_type_size(void*  SIZEOF_VOID_PTR)

  if(${SIZEOF_VOID_PTR} MATCHES "^8$")
    set(OS_32_BITS 0)
    set(OS_64_BITS 1)
    set(CMAKE_NATIVE_ARCH 64)
  else(${SIZEOF_VOID_PTR} MATCHES "^8$")
    set(OS_32_BITS 1)
    set(OS_64_BITS 0)
    set(CMAKE_NATIVE_ARCH 32)
  endif(${SIZEOF_VOID_PTR} MATCHES "^8$")

  if(UNIX)
    add_definitions(-DUNIX)
    execute_process(
      COMMAND uname -a
      OUTPUT_VARIABLE OS_ARCH 
     )

    #
    # For Linux
    #
    if(OS_ARCH MATCHES ".*Linux.*")
      add_definitions(-DLINUX)

      if(OS_ARCH MATCHES ".*x86_64.*")
	set(OS_32_BITS 1)
      elseif(OS_ARCH MATCHES ".*ia64.*")
	set(OS_32_BITS 0)
      endif(OS_ARCH MATCHES ".*x86_64.*")
    endif(OS_ARCH MATCHES ".*Linux.*")

    #
    # For SUN
    #
    if(OS_ARCH MATCHES ".*SunOS.*")
      add_definitions(-DSUNOS)
      execute_process(
	COMMAND isainfo -v
	OUTPUT_VARIABLE SUNOS_ARCH 
	)

      IF (SUNOS_ARCH MATCHES ".*64-bit.*")
	set(OS_32_BITS 1)
      endif(SUNOS_ARCH MATCHES ".*64-bit.*")

    endif(OS_ARCH MATCHES ".*SunOS.*")

    if(APPLE)
# Consider using the __APPLE__ define, which I think is defaulted on Macs.    
      add_definitions(-DDARWIN)
# Should this be forced?

      if ( FREEDIUS_USE_CARBON )
        add_definitions(-DAGL)
        add_definitions(-F/System/Library/Frameworks )
      else ( FREEDIUS_USE_CARBON )
        if ( FREEDIUS_USE_COCOA )
          add_definitions(-DCOCOA)
          add_definitions(-F/System/Library/Frameworks )
        else ( FREEDIUS_USE_COCOA )
	  set( FREEDIUS_USE_X11 TRUE )  # ?
        endif ( FREEDIUS_USE_COCOA )
      endif ( FREEDIUS_USE_CARBON )

      set(OS_32_BITS 1)

      #
      # @todo: add Apple 64-bit OS detection here
      #
    endif(APPLE)

  else(UNIX)
    #
    # @todo: add windows 64-bit OS detection here
    #

  endif(UNIX)


  if(CMAKE_BUILD_ARCH STREQUAL "Native")
    set(CMAKE_BUILD_ARCH ${CMAKE_NATIVE_ARCH})
  endif(CMAKE_BUILD_ARCH STREQUAL "Native")

  if(CMAKE_BUILD_ARCH EQUAL 32)
    message(STATUS "Building for 32-bit!")

    if(NOT OS_32_BITS)
      message(FATAL_ERROR 
	"Sorry, but this platform cannot compile 32-bit applications")
    endif(NOT OS_32_BITS)

    message(STATUS "check compiler")
    if(CMAKE_COMPILER_IS_GNUCXX)
      add_definitions(-m32)
      set(LINK_FLAGS "-m32 ${LINK_FLAGS}")
      #message(STATUS "-m32 ${LINK_FLAGS}")
    else(CMAKE_COMPILER_IS_GNUCXX)
    message(STATUS "checked compiler")

      #
      # @todo: add 32-bit compile flags for non-GNU compilers here
      #

    endif(CMAKE_COMPILER_IS_GNUCXX)

  endif(CMAKE_BUILD_ARCH EQUAL 32)

endmacro(check_architecture)

check_architecture()

include(TestBigEndian)
test_big_endian(WORDS_BIGENDIAN)

if(${WORDS_BIGENDIAN})
  add_definitions(WORDS_BIGENDIAN)
endif(${WORDS_BIGENDIAN})


# Is this used anymore?

add_definitions(-DF3DMODULAR) # temporary flag while transitioning to multiple libraries 



#include(CheckIncludeFiles)
#check_include_files( tiffio.h HAVE_TIFFIO_H  )
#check_include_files( jconfig.h HAVE_JCONFIG_H )

if(WIN32)
  message(STATUS "Windows-dependent flags...")
  add_definitions(/D_WIN32)
  add_definitions(-D_CRT_SECURE_NO_WARNINGS=1)
endif(WIN32)

#
# Store build type
#
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING
      "Choose the type of build, options are: None Debug Release RelWithDebInfo MinSizeRel."
      FORCE)
endif(NOT CMAKE_BUILD_TYPE)


if(NOT CMAKE_SYSTEM)
  message(FATAL_ERROR "CMAKE_SYSTEM was not set")
endif(NOT CMAKE_SYSTEM)

#
# @bug in cmake2.5 in windows (workaround)
#
#if(NOT CMAKE_SYSTEM_VERSION)
#  set(CMAKE_SYSTEM_VERSION "5.1")
#  set(CMAKE_SYSTEM "${CMAKE_SYSTEM}-${CMAKE_SYSTEM_VERSION}")
#endif(NOT CMAKE_SYSTEM_VERSION)


if(NOT CMAKE_BUILD_TYPE)
#  MESSAGE(FATAL_ERROR "CMAKE_BUILD_TYPE was not set")
   set(CMAKE_BUILD_TYPE Release)
endif(NOT CMAKE_BUILD_TYPE)

if(NOT CMAKE_BUILD_ARCH)
  message(FATAL_ERROR "CMAKE_BUILD_ARCH was not set")
endif(NOT CMAKE_BUILD_ARCH)



set(INSTALL_ARCHDIR $ENV{INSTALL_ARCHDIR})
set(INSTALL_PREFIX $ENV{INSTALL_PREFIX})

if(INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX ${INSTALL_PREFIX})
elseif(INSTALL_ARCHDIR)
  set(CMAKE_INSTALL_PREFIX "${INSTALL_ARCHDIR}/${CMAKE_SYSTEM_NAME}-${LISP}")
else()
#  set(CMAKE_INSTALL_PREFIX ".")
  set(CMAKE_INSTALL_PREFIX "${CMAKE_SOURCE_DIR}/arch/${CMAKE_SYSTEM_NAME}-${LISP}")
endif(INSTALL_PREFIX)
  
# Looks as if we need to canonicalize paths into "cmake" form
# (especially on Windoze, since it uses '\' as the dir delimiter):

FILE(TO_CMAKE_PATH ${CMAKE_INSTALL_PREFIX} CMAKE_INSTALL_PREFIX)

set(LIBRARY_INSTALL_DIR $ENV{LIBRARY_INSTALL_DIR})
if(NOT LIBRARY_INSTALL_DIR) 
  set(LIBRARY_INSTALL_DIR ${CMAKE_INSTALL_PREFIX})
endif(NOT LIBRARY_INSTALL_DIR) 

#message(STATUS "CMAKE_INSTALL_PREFIX = ${CMAKE_INSTALL_PREFIX}")

if(0 EQUAL 1) # assume that install is used to retain the libs and bins in another place
     set(BUILD_DIR "${CMAKE_SOURCE_DIR}/arch/${CMAKE_SYSTEM_NAME}-${LISP}")
     set(PROJECT_BINARY_DIR "${BUILD_DIR}")
#    set(CMAKE_BINARY_DIR "${BUILD_DIR}/obj")
     set(EXECUTABLE_OUTPUT_PATH "${BUILD_DIR}/bin")
     set(LIBRARY_OUTPUT_PATH "${BUILD_DIR}/lib")
     set(CMAKE_LIBRARY_PATH ${LIBRARY_OUTPUT_PATH} ${CMAKE_LIBRARY_PATH})

     file(MAKE_DIRECTORY "${BUILD_DIR}")
     file(MAKE_DIRECTORY "${PROJECT_BINARY_DIR}")
     file(MAKE_DIRECTORY "${EXECUTABLE_OUTPUT_PATH}")
     file(MAKE_DIRECTORY "${LIBRARY_OUTPUT_PATH}")
endif(0 EQUAL 1)

#
# Macro used to easily add linker flags to a target
#
macro(target_link_flags0 TARGET FLAGS)
  get_target_property(OLD_FLAGS ${TARGET} LINK_FLAGS)
  set_target_properties(
    ${TARGET}
    PROPERTIES 
    LINK_FLAGS "${FLAGS} ${OLD_FLAGS}"
   )
endmacro(target_link_flags0)

macro(target_link_flags TARGET FLAGS)
  #get_target_property(OLD_FLAGS ${TARGET} LINK_FLAGS)
  set_target_properties(
    ${TARGET}
    PROPERTIES 
    LINK_FLAGS "${FLAGS} ${LINK_FLAGS}"
   )
endmacro(target_link_flags)
