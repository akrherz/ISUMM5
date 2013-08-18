# Makefile for directory physics/pbl_sfc/bulk

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/pbl_sfc/bulk

OBJS =\
	blkpbl.o

SRC =\
	blkpbl.i

SRCF =\
	blkpbl.f

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

blkpbl.o: ../../../include/parame.incl ../../../include/rpstar.incl
blkpbl.o: ../../../include/varia.incl ../../../include/dusolve1.incl
blkpbl.o: ../../../include/param2.incl ../../../include/param3.incl
blkpbl.o: ../../../include/pmoist.incl ../../../include/point3d.incl
blkpbl.o: ../../../include/point2d.incl ../../../include/various.incl
blkpbl.o: ../../../include/nonhyd.incl ../../../include/nhcnst.incl
blkpbl.o: ../../../include/sum.incl
