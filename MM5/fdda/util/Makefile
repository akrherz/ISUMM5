# Makefile for directory fdda/util

DEVTOP = ../..
include ../../configure.user

CURRENT_DIR = $(DEVTOP)/fdda/util

OBJS =\
	setfd.o \
	fdaoff.o \
	unity.o

SRC =\
	setfd.i \
	fdaoff.i \
	unity.i

SRCF =\
	setfd.f \
	fdaoff.f \
	unity.f

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

fdaoff.o: ../../include/parame.incl ../../include/param2.incl
fdaoff.o: ../../include/parfdda.incl
setfd.o: ../../include/defines.incl ../../include/parame.incl
setfd.o: ../../include/addr0.incl ../../include/varia.incl
setfd.o: ../../include/param2.incl ../../include/param3.incl
setfd.o: ../../include/pmoist.incl ../../include/point3d.incl
setfd.o: ../../include/point2d.incl ../../include/pointbc.incl
setfd.o: ../../include/various.incl ../../include/dusolve1.incl
setfd.o: ../../include/fddagd.incl ../../include/fddaob.incl
setfd.o: ../../include/parfdda.incl
unity.o: ../../include/parame.incl
