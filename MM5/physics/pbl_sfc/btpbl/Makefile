# Makefile for directory physics/pbl_sfc/btpbl

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = ./btpbl

OBJS =\
	bound.o \
	erase.o \
	esatpb.o \
	hoskeep.o \
	initpb.o \
	navypb.o \
	outpb.o \
	uvcomp.o

SRC =\
	bound.i \
	erase.i \
	esatpb.i \
	hoskeep.i \
	initpb.i \
	navypb.i \
	outpb.i \
	uvcomp.i

SRCF =\
	bound.f \
	erase.f \
	esatpb.f \
	hoskeep.f \
	initpb.f \
	navypb.f \
	outpb.f \
	uvcomp.f

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

bound.o: ../../../include/parame.incl ../../../include/btblk1.incl
bound.o: ../../../include/btblk2.incl
erase.o: ../../../include/parame.incl ../../../include/btblk1.incl
erase.o: ../../../include/btblk2.incl
hoskeep.o: ../../../include/parame.incl ../../../include/point2d.incl
hoskeep.o: ../../../include/btblk1.incl ../../../include/btblk2.incl
hoskeep.o: ../../../include/param2.incl
initpb.o: ../../../include/parame.incl ../../../include/point3d.incl
initpb.o: ../../../include/point2d.incl ../../../include/navypb.incl
initpb.o: ../../../include/btblk2.incl
navypb.o: ../../../include/parame.incl ../../../include/point2d.incl
navypb.o: ../../../include/nhcnst.incl ../../../include/btblk1.incl
navypb.o: ../../../include/btblk2.incl
outpb.o: ../../../include/parame.incl ../../../include/point3d.incl
outpb.o: ../../../include/point2d.incl ../../../include/navypb.incl
outpb.o: ../../../include/btblk2.incl
