# -*- Mode: Makefile -*-

# $Id$

!include <win32.mak>

!ifdef ObjDir
ObjDir = $(ObjDir)
!else
ObjDir = .
!endif

!ifdef SrcDir
SrcDir = $(SrcDir)
!else
SrcDir = .
!endif

Objs = \
  $(ObjDir)\adler32.obj \
  $(ObjDir)\compress.obj \
  $(ObjDir)\crc32.obj \
  $(ObjDir)\deflate.obj \
  $(ObjDir)\gzio.obj \
  $(ObjDir)\infback.obj  \
  $(ObjDir)\inffast.obj \
  $(ObjDir)\inflate.obj \
  $(ObjDir)\inftrees.obj \
  $(ObjDir)\trees.obj \
  $(ObjDir)\uncompr.obj \
  $(ObjDir)\zutil.obj 

Target = $(ObjDir)\zlib.lib

{$(SrcDir)}.c{$(ObjDir)}.obj::
	$(cc)  $(cflags) $(cvarsdll) -Fd$(ObjDir)\ -c -TC $<
#	$(cc) $(cdebug) $(cflags) $(cvarsdll) -Fd$(ObjDir)\ -c -TC $<

all: $(Target)

$(Target): $(Objs)
	$(implib) -out:$@ $(Objs) 

clean:
	@-for %%f in ($(Objs)) do @(if exist %%f del %%f)
	@-if exist $(Target) del $(Target)
	@-if exist $(ObjDir)\*.pdb del $(ObjDir)\*.pdb

# zlib.mak EOF
