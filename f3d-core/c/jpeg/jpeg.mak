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
  $(ObjDir)\jcapimin.obj \
  $(ObjDir)\jcapistd.obj \
  $(ObjDir)\jctrans.obj \
  $(ObjDir)\jcparam.obj \
  $(ObjDir)\jdatadst.obj \
  $(ObjDir)\jcinit.obj \
  $(ObjDir)\jcmaster.obj \
  $(ObjDir)\jcmarker.obj \
  $(ObjDir)\jcmainct.obj \
  $(ObjDir)\jcprepct.obj \
  $(ObjDir)\jccoefct.obj \
  $(ObjDir)\jccolor.obj \
  $(ObjDir)\jcsample.obj \
  $(ObjDir)\jchuff.obj \
  $(ObjDir)\jcphuff.obj \
  $(ObjDir)\jcdctmgr.obj \
  $(ObjDir)\jfdctfst.obj \
  $(ObjDir)\jfdctflt.obj \
  $(ObjDir)\jfdctint.obj \
  $(ObjDir)\jdapimin.obj \
  $(ObjDir)\jdapistd.obj \
  $(ObjDir)\jdtrans.obj \
  $(ObjDir)\jdatasrc.obj \
  $(ObjDir)\jdmaster.obj \
  $(ObjDir)\jdinput.obj \
  $(ObjDir)\jdmarker.obj \
  $(ObjDir)\jdhuff.obj \
  $(ObjDir)\jdphuff.obj \
  $(ObjDir)\jdmainct.obj \
  $(ObjDir)\jdcoefct.obj \
  $(ObjDir)\jdpostct.obj \
  $(ObjDir)\jddctmgr.obj \
  $(ObjDir)\jidctfst.obj \
  $(ObjDir)\jidctflt.obj \
  $(ObjDir)\jidctint.obj \
  $(ObjDir)\jidctred.obj \
  $(ObjDir)\jdsample.obj \
  $(ObjDir)\jdcolor.obj \
  $(ObjDir)\jquant1.obj \
  $(ObjDir)\jquant2.obj \
  $(ObjDir)\jdmerge.obj \
  $(ObjDir)\jcomapi.obj \
  $(ObjDir)\jutils.obj \
  $(ObjDir)\jerror.obj \
  $(ObjDir)\jmemmgr.obj \
  $(ObjDir)\jmemnobs.obj

Target = $(ObjDir)\jpeg.lib

{$(SrcDir)}.c{$(ObjDir)}.obj::
	$(cc)  $(cflags) -I. $(cvarsdll) -Fd$(ObjDir)\ -c -TC $<
#	$(cc)  $(cdebug) $(cflags) -I. $(cvarsdll) -Fd$(ObjDir)\ -c -TC $<

all: $(Target)

$(SrcDir)\jconfig.h:
	@if exist $@ del $@
	@copy $(SrcDir)\jconfig.vc $@

$(Target): $(SrcDir)\jconfig.h $(Objs)
	$(implib) -out:$@ $(Objs)

clean:
	@-for %%f in ($(Objs)) do @(if exist %%f del %%f)
	@-if exist $(SrcDir)\jconfig.h del $(SrcDir)\jconfig.h
	@-if exist $(Target) del $(Target)
	@-if exist $(ObjDir)\*.pdb del $(ObjDir)\*.pdb

# jpeg.mak EOF
