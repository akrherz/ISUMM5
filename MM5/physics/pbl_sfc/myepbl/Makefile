# Makefile for directory physics/pbl_sfc/mrfpbl

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/pbl_sfc/mrfpbl

OBJS =\
	difcof.o \
	mixlen.o \
	prodq2.o \
	sfcdif.o \
	myepbl.o \
	vdifh.o \
	vdifq.o \
	vdifv.o

SRC =\
	difcof.i \
	mixlen.i \
	prodq2.i \
	sfcdif.i \
	myepbl.i \
	vdifh.i \
	vdifq.i \
	vdifv.i


SRCF =\
	difcof.f \
	mixlen.f \
	prodq2.f \
	sfcdif.f \
	myepbl.f \
	vdifh.f \
	vdifq.f \
	vdifv.f

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

difcof.o: ../../../include/parame.incl
mixlen.o: ../../../include/parame.incl
myepbl.o: ../../../include/parame.incl ../../../include/param3.incl
myepbl.o: ../../../include/pmoist.incl ../../../include/nhcnst.incl
myepbl.o: ../../../include/point3d.incl ../../../include/point2d.incl
myepbl.o: ../../../include/navypb.incl ../../../include/rpstar.incl
myepbl.o: ../../../include/soil.incl ../../../include/nonhyd.incl
myepbl.o: ../../../include/landuse.incl
prodq2.o: ../../../include/parame.incl
sfcdif.o: ../../../include/parame.incl
vdifh.o: ../../../include/parame.incl
vdifq.o: ../../../include/parame.incl
vdifv.o: ../../../include/parame.incl
