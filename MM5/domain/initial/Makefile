# Makefile for directory domain/initial

DEVTOP = ../..
include ../../configure.user

CURRENT_DIR = $(DEVTOP)/domain/initial

OBJS =\
	init.o \
	initts.o \
	param.o \
	paramr.o

SRC =\
	init.i \
	initts.i \
	param.i \
	paramr.i

SRCF =\
	init.f \
	initts.f \
	param.f \
	paramr.f

LIBTARGET = util
TARGETDIR = ../../

all:: $(OBJS)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS)

code:: $(SRC)

little_f:: $(SRCF) $(OBJS)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS)

# -------------------------------------------------------------------------
# common rules for all Makefiles - do not edit

emptyrule::

clean::
	$(RM_CMD) "#"*

# -------------------------------------------------------------------------
# DO NOT DELETE THIS LINE -- make depend depends on it.

init.o: ../../include/defines.incl ../../include/parame.incl
init.o: ../../include/param2.incl ../../include/param3.incl
init.o: ../../include/addr0.incl ../../include/point3d.incl
init.o: ../../include/point2d.incl ../../include/various.incl
init.o: ../../include/nestl.incl ../../include/varia.incl
init.o: ../../include/dusolve1.incl ../../include/fddagd.incl
init.o: ../../include/fddaob.incl ../../include/nonhyd.incl
init.o: ../../include/nonhydb.incl ../../include/nhcnst.incl
init.o: ../../include/addrcu.incl ../../include/pmoist.incl
init.o: ../../include/soil.incl ../../include/soilcnst.incl
init.o: ../../include/soilp.incl ../../include/paramveg_USGS.incl
init.o: ../../include/rpstar.incl ../../include/landuse.incl
init.o: ../../include/parfdda.incl ../../include/uprad.incl
init.o: ../../include/navypb.incl ../../include/pbltke.incl
init.o: ../../include/zdiffu.incl ../../include/zdiffu2.incl
initts.o: ../../include/parame.incl ../../include/param2.incl
initts.o: ../../include/param3.incl ../../include/point2d.incl
param.o: ../../include/defines.incl ../../include/parame.incl
param.o: ../../include/param2.incl ../../include/param3.incl
param.o: ../../include/pmoist.incl ../../include/landuse.incl
param.o: ../../include/addr0.incl ../../include/various.incl
param.o: ../../include/hdrv3.incl ../../include/nhcnst.incl
param.o: ../../include/fddagd.incl ../../include/fddaob.incl
param.o: ../../include/soilcnst.incl ../../include/parfdda.incl
param.o: ../../include/pbltb.incl ../../include/pargrcu.incl
param.o: ../../include/parkfcu.incl ../../include/parbmcu.incl
param.o: ../../include/parakcu.incl ../../include/landinp.incl
param.o: ../../include/chardate.incl ../../include/bucket.incl 
param.o: ../../include/parpx.incl ../../include/paramveg_USGS.incl
param.o: ../../include/paramsoil_STAS.incl ../../include/paramgen_LSM.incl
