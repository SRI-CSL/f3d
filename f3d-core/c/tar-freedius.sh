#!/bin/sh
#
# Creates a full dump of essential FREEDIUS files.  Excludes arch,
# build, and .svn directories.
#
cd ..
SRCDIR=`pwd`
RELDIR=`basename $SRCDIR`
cd ..
tar -zcvf /tmp/freedius-dist.tgz -h --exclude "*~" --exclude .svn --exclude arch --exclude build $RELDIR
