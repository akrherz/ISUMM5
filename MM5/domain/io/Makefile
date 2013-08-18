# Makefile for directory domain/io

DEVTOP = ../..
include ../../configure.user

CURRENT_DIR = $(DEVTOP)/domain/io

OBJS =\
	conadv.o \
	conmas.o \
	dm_io.o \
	initsav.o \
	mapsmp.o \
	outprt.o \
	output.o \
	outsav.o \
	outtap.o \
	outts.o \
	rdinit.o \
	rdter.o \
	rho_mlt.o \
	savread.o \
	shutdo.o \
	tmass.o \
	vtran.o \
        write_fieldrec.o  \
        write_big_header.o \
        write_flag.o

SRC = \
	conadv.i \
	conmas.i \
	dm_io.i \
	initsav.i \
	mapsmp.i \
	outprt.i \
	output.i \
	outsav.i \
	outtap.i \
	outts.i \
	rdinit.i \
	rdter.i \
	rho_mlt.i \
	savread.i \
	shutdo.i \
	tmass.i \
	vtran.i \
        write_fieldrec.i  \
        write_big_header.i \
        write_flag.i


SRCF = \
	conadv.f \
	conmas.f \
	dm_io.f \
	initsav.f \
	mapsmp.f \
	outprt.f \
	output.f \
	outsav.f \
	outtap.f \
	outts.f \
	rdinit.f \
	rdter.f \
	rho_mlt.f \
	savread.f \
	shutdo.f \
	tmass.f \
	vtran.f \
        write_fieldrec.f \
        write_big_header.f \
        write_flag.f

LIBTARGET = util
TARGETDIR = ../../

ALLFILES = $(SRCS)

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

conadv.o: ../../include/parame.incl ../../include/param3.incl
conadv.o: ../../include/point3d.incl ../../include/point2d.incl
conadv.o: ../../include/various.incl ../../include/rpstar.incl
conmas.o: ../../include/parame.incl ../../include/param3.incl
conmas.o: ../../include/point3d.incl ../../include/point2d.incl
conmas.o: ../../include/various.incl
initsav.o: ../../include/parame.incl ../../include/param2.incl
initsav.o: ../../include/param3.incl ../../include/pmoist.incl
initsav.o: ../../include/addr0.incl ../../include/various.incl
initsav.o: ../../include/variousn.incl ../../include/nestl.incl
initsav.o: ../../include/dusolve1.incl ../../include/addrcu.incl
initsav.o: ../../include/defines.incl ../../include/rpstar.incl
initsav.o: ../../include/point2d.incl ../../include/uprad.incl
initsav.o: ../../include/nhcnst.incl
outprt.o: ../../include/parame.incl ../../include/addr0.incl
outprt.o: ../../include/param2.incl ../../include/param3.incl
outprt.o: ../../include/point3d.incl ../../include/point2d.incl
outprt.o: ../../include/various.incl ../../include/varia.incl
outprt.o: ../../include/pointbc.incl ../../include/pmoist.incl
outprt.o: ../../include/nonhyd.incl ../../include/nonhydb.incl
output.o: ../../include/defines.incl ../../include/parame.incl
output.o: ../../include/addr0.incl ../../include/param2.incl
output.o: ../../include/various.incl ../../include/varia.incl
output.o: ../../include/addrcu.incl ../../include/parfdda.incl
outtap.o: ../../include/parame.incl ../../include/addr0.incl
outtap.o: ../../include/point3d.incl ../../include/navypb.incl
outtap.o: ../../include/point2d.incl ../../include/various.incl
outtap.o: ../../include/nestl.incl ../../include/hdrv3.incl
outtap.o: ../../include/nonhyd.incl ../../include/nhcnst.incl
outtap.o: ../../include/fddagd.incl ../../include/pmoist.incl
outtap.o: ../../include/varia.incl ../../include/param2.incl
outtap.o: ../../include/param3.incl ../../include/parfdda.incl
outtap.o: ../../include/fogstuf.incl  ../../include/paramveg_USGS.incl
outtap.o: ../../include/pbltke.incl ../../include/landuse.incl
outts.o: ../../include/parame.incl ../../include/param2.incl
outts.o: ../../include/param3.incl ../../include/point2d.incl
outts.o: ../../include/point3d.incl ../../include/rpstar.incl
outts.o: ../../include/nonhyd.incl 
rdinit.o: ../../include/parame.incl ../../include/param2.incl
rdinit.o: ../../include/param3.incl ../../include/addr0.incl
rdinit.o: ../../include/point3d.incl ../../include/point2d.incl
rdinit.o: ../../include/various.incl ../../include/nestl.incl
rdinit.o: ../../include/varia.incl ../../include/dusolve1.incl
rdinit.o: ../../include/nonhyd.incl ../../include/nonhydb.incl
rdinit.o: ../../include/nhcnst.incl ../../include/hdrv3.incl
rdinit.o: ../../include/landinp.incl ../../include/soilp.incl
rdinit.o: ../../include/soilcnst.incl ../../include/chardate.incl
rdinit.o: ../../include/soil.incl ../../include/paramsoil_STAS.incl
rdinit.o: ../../include/paramveg_USGS.incl ../../include/paramgen_LSM.incl
rdinit.o: ../../include/pmoist.incl
rdter.o: ../../include/parame.incl ../../include/hdrv3.incl
rdter.o: ../../include/various.incl ../../include/nestl.incl
shutdo.o: ../../include/parame.incl ../../include/addr0.incl
shutdo.o: ../../include/param2.incl ../../include/various.incl
shutdo.o: ../../include/varia.incl
tmass.o: ../../include/parame.incl
vtran.o: ../../include/parame.incl
