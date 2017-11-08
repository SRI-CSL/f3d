#!/bin/sh

# Usage:  run-cmake2.sh <build-dir> <source_dir> <options>

# $F3D/c/run-cmake2.sh $F3DSYS/arch/linux-sbcl64/build/proprietary/aerovironment -DF3D=$FREEDIUS -DF3DA=$F3DA -DLISP=sbcl64 -DLIBRARY_INSTALL_DIR=$F3DSYS/arch/linux-sbcl64/lib $F3DSYS/proprietary/aerovironment 

export PATH=/usr/local/bin:$PATH

# To clean the cmake temp files
# \rm -rf $build_dir
# \rm -rf $F3DSYS/arch/linux-sbcl64/c/proprietary/aerovironment

#set -o verbose #echo on

build_dir=$1
shift 1;
source_dir=$1
shift 1;
options=$*

mkdir -p $build_dir

cd $build_dir

echo "cmake $source_dir $options"

cmake $source_dir  $options

make install

