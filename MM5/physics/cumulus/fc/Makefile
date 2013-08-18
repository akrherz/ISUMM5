# Makefile for directory physics/cumulus/fc

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/cumulus/fc

OBJS =\
	cupara5.o \
	fcpara.o \
	dtfrz.o \
	tp.o

SRC =\
	cupara5.i \
	fcpara.i \
	dtfrz.i \
	tp.i

SRCF =\
	cupara5.f \
	fcpara.f \
	dtfrz.f \
	tp.f

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

cupara5.o: ../../../include/parame.incl ../../../include/various.incl
cupara5.o: ../../../include/param3.incl ../../../include/addrcu.incl
cupara5.o: ../../../include/pmoist.incl ../../../include/nonhyd.incl
cupara5.o: ../../../include/nhcnst.incl
fcpara.o: ../../../include/parame.incl ../../../include/various.incl
fcpara.o: ../../../include/param3.incl ../../../include/point3d.incl
fcpara.o: ../../../include/point2d.incl ../../../include/varia.incl
fcpara.o: ../../../include/addrcu.incl ../../../include/nonhyd.incl
fcpara.o: ../../../include/rpstar.incl
