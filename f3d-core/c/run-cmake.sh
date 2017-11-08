#!/bin/sh

# Usage:  run-cmake.sh <f3d> <f3da> <lisp> <arch> <source_dir> <build-dir> <install-dir> . <options>


# To clean the cmake temp files
# \rm -rf $build_dir
# \rm -rf $F3DSYS/arch/linux-sbcl64/c/proprietary/aerovironment

#set -o verbose #echo on

F3D=$1
shift 1;
F3DA=$1
shift 1;
lisp=$1
shift 1;
arch=$1
shift 1;
source_dir=$1
shift 1;
build_dir=$1
shift 1;
library_install_dir=$1
shift 1;
options=$*

mkdir -p $build_dir

cd $build_dir

echo "cmake $source_dir -DF3D=$F3D -DF3DA=$F3DA -DLISP=$lisp -DLIBRARY_INSTALL_DIR=$library_install_dir -DCMAKE_VERBOSE_MAKEFILE=1 $options"

cmake       $source_dir -DF3D=$F3D -DF3DA=$F3DA -DLISP=$lisp -DLIBRARY_INSTALL_DIR=$library_install_dir -DCMAKE_VERBOSE_MAKEFILE=1 $options

make install

