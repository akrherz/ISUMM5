# Makefile for directory ./physics/radiation/ccm2

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/radiation/util

OBJS =\
	inirad.o \
	o3data.o \
	solar1.o

SRC =\
	inirad.i \
	o3data.i \
	solar1.i

SRCF =\
	inirad.f \
	o3data.f \
	solar1.f

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

solar1.o: ../../../include/parame.incl ../../../include/param3.incl
inirad.o: ../../../include/parame.incl ../../../include/parccm2.incl
inirad.o: ../../../include/various.incl
o3data.o: ../../../include/parame.incl ../../../include/param3.incl
o3data.o: ../../../include/point2d.incl ../../../include/point3d.incl
o3data.o: ../../../include/parccm2.incl ../../../include/various.incl
o3data.o: ../../../include/nonhyd.incl

