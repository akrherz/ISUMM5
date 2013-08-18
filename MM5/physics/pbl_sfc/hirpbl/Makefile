# Makefile for directory physics/pbl_sfc/hirpbl

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/pbl_sfc/hirpbl

OBJS =\
	hirpbl.o

SRC =\
	hirpbl.i

SRCF =\
	hirpbl.f

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

hirpbl.o: ../../../include/parame.incl ../../../include/rpstar.incl
hirpbl.o: ../../../include/varia.incl ../../../include/dusolve1.incl
hirpbl.o: ../../../include/param2.incl ../../../include/param3.incl
hirpbl.o: ../../../include/pmoist.incl ../../../include/point3d.incl
hirpbl.o: ../../../include/point2d.incl ../../../include/various.incl
hirpbl.o: ../../../include/nonhyd.incl ../../../include/nhcnst.incl
hirpbl.o: ../../../include/soil.incl ../../../include/soilcnst.incl
hirpbl.o: ../../../include/addrcu.incl ../../../include/sum.incl
