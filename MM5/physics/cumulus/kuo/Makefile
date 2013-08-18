# Makefile for directory physics/cumulus/kuo

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/cumulus/kuo

OBJS =\
	cupara2.o

SRC =\
	cupara2.i

SRCF =\
	cupara2.f

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

cupara2.o: ../../../include/parame.incl ../../../include/varia.incl
cupara2.o: ../../../include/dusolve1.incl ../../../include/param2.incl
cupara2.o: ../../../include/param3.incl ../../../include/pmoist.incl
cupara2.o: ../../../include/point3d.incl ../../../include/point2d.incl
cupara2.o: ../../../include/various.incl ../../../include/nonhyd.incl
cupara2.o: ../../../include/rpstar.incl ../../../include/parakcu.incl
