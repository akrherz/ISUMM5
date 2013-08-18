# Makefile for directory physics/pbl_sfc/pxpbl

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/pbl_sfc/pxpbl

OBJS =\
	pxpbl.o \
    radsfc.o \
    smass.o \
    eddymm4.o \
    sfcmm4.o \
    vdiff.o \
    acm.o \
    qflux.o \
    surfpx.o \
    soilav.o \
    vegeland.o

SRC =\
	pxpbl.i \
    radsfc.i \
    smass.i \
    eddymm4.i \
    sfcmm4.i \
    vdiff.i \
    acm.i \
    qflux.i \
    surfpx.i \
    soilav.i \
    vegeland.i

SRCF =\
	pxpbl.f \
    radsfc.f \
    smass.f \
    eddymm4.f \
    sfcmm4.f \
    vdiff.f \
    acm.f \
    qflux.f \
    surfpx.f \
    soilav.f \
    vegeland.f

LIBTARGET = util
TARGETDIR = ../../../

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

pxpbl.o: ../../../include/parame.incl ../../../include/rpstar.incl
pxpbl.o: ../../../include/varia.incl ../../../include/dusolve1.incl
pxpbl.o: ../../../include/param2.incl ../../../include/param3.incl
pxpbl.o: ../../../include/pmoist.incl ../../../include/point3d.incl
pxpbl.o: ../../../include/point2d.incl ../../../include/various.incl
pxpbl.o: ../../../include/nonhyd.incl ../../../include/nhcnst.incl
pxpbl.o: ../../../include/sum.incl    ../../../include/parfdda.incl
pxpbl.o: ../../../include/fddagd.incl
pxpbl.o: ../../../include/comicl.incl
pxpbl.o: ../../../include/parpx.incl
pxpbl.o: ../../../include/soilp.incl
acm.o:     ../../../include/parame.incl ../../../include/comicl.incl
eddymm4.o: ../../../include/parame.incl  ../../../include/comicl.incl
qflux.o:   ../../../include/parame.incl  ../../../include/comicl.incl
qflux.o:   ../../../include/soilp.incl
radsfc.o:  ../../../include/parame.incl  ../../../include/param2.incl
radsfc.o:  ../../../include/param3.incl  ../../../include/pmoist.incl
radsfc.o:  ../../../include/point3d.incl  ../../../include/point2d.incl 
radsfc.o:  ../../../include/various.incl ../../../include/comicl.incl
radsfc.o:  ../../../include/soilp.incl
sfcmm4.o:  ../../../include/parame.incl ../../../include/comicl.incl
sfcmm4.o:  ../../../include/soilp.incl
surfpx.o: ../../../include/parame.incl  ../../../include/comicl.incl
surfpx.o: ../../../include/soilp.incl 
smass.o:   ../../../include/parame.incl ../../../include/comicl.incl
smass.o:   ../../../include/soilp.incl  
vdiff.o:   ../../../include/parame.incl  ../../../include/comicl.incl
soilav.o:  ../../../include/parame.incl 
vegeland.o: ../../../include/parame.incl
vegeland.o: ../../../include/parpx.incl

