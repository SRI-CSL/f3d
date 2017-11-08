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

!include nmake.opt

Objs = \
  $(ObjDir)\tif_aux.obj \
  $(ObjDir)\tif_close.obj \
  $(ObjDir)\tif_codec.obj \
  $(ObjDir)\tif_color.obj \
  $(ObjDir)\tif_compress.obj \
  $(ObjDir)\tif_dir.obj \
  $(ObjDir)\tif_dirinfo.obj \
  $(ObjDir)\tif_dirread.obj \
  $(ObjDir)\tif_dirwrite.obj \
  $(ObjDir)\tif_dumpmode.obj \
  $(ObjDir)\tif_error.obj \
  $(ObjDir)\tif_extension.obj \
  $(ObjDir)\tif_fax3.obj \
  $(ObjDir)\tif_fax3sm.obj \
  $(ObjDir)\tif_getimage.obj \
  $(ObjDir)\tif_jpeg.obj \
  $(ObjDir)\tif_ojpeg.obj \
  $(ObjDir)\tif_flush.obj \
  $(ObjDir)\tif_luv.obj \
  $(ObjDir)\tif_lzw.obj \
  $(ObjDir)\tif_next.obj \
  $(ObjDir)\tif_open.obj \
  $(ObjDir)\tif_packbits.obj \
  $(ObjDir)\tif_pixarlog.obj \
  $(ObjDir)\tif_predict.obj \
  $(ObjDir)\tif_print.obj \
  $(ObjDir)\tif_read.obj \
  $(ObjDir)\tif_stream.obj \
  $(ObjDir)\tif_swab.obj \
  $(ObjDir)\tif_strip.obj \
  $(ObjDir)\tif_thunder.obj \
  $(ObjDir)\tif_tile.obj \
  $(ObjDir)\tif_version.obj \
  $(ObjDir)\tif_warning.obj \
  $(ObjDir)\tif_write.obj \
  $(ObjDir)\tif_zip.obj \
  $(ObjDir)\tif_win32.obj

Target = $(ObjDir)\tiff.lib

INCLUDE=$(INCLUDE);$(SrcDir)\..\zlib;$(SrcDir)\..\jpeg;

{$(SrcDir)}.cxx{$(ObjDir)}.obj::
	$(cc)  $(cflags) /EHsc $(cvarsdll) -Fd$(ObjDir)\ -c -TP $<
#	$(cc) $(cdebug) $(cflags) /EHsc $(cvarsdll) -Fd$(ObjDir)\ -c -TP $<

{$(SrcDir)}.c{$(ObjDir)}.obj::
	$(cc) $(cflags) $(cvarsdll) -Fd$(ObjDir)\ -c -TC $<
#	$(cc) $(cdebug) $(cflags) $(cvarsdll) -Fd$(ObjDir)\ -c -TC $<

all: $(Target)

$(SrcDir)\tif_config.h:
	copy $(SrcDir)\tif_config.h.vc $@

$(SrcDir)\tiffconf.h:
	copy $(SrcDir)\tiffconf.h.vc $@

$(Target): $(SrcDir)\tiffconf.h $(SrcDir)\tif_config.h $(Objs)
	$(implib) /out:$@ $(Objs) $(LIBS)

clean:
	@-for %%f in ($(Objs)) do @(if exist %%f del %%f)
	@-if exist $(Target) del $(Target)
	@-if exist $(ObjDir)\*.pdb del $(ObjDir)\*.pdb

# tiff.mak EOF
