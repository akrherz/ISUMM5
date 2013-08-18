# Makefile for directory physics/radiation/cloud

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/radiation/cloud

OBJS =\
	lwrad.o \
	swrad.o

SRC =\
	lwrad.i \
	swrad.i

SRCF =\
	lwrad.f \
	swrad.f

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

lwrad.o: ../../../include/parame.incl ../../../include/point3d.incl
lwrad.o: ../../../include/point2d.incl ../../../include/various.incl
lwrad.o: ../../../include/radiat.incl ../../../include/nonhyd.incl
lwrad.o: ../../../include/nhcnst.incl ../../../include/param2.incl
lwrad.o: ../../../include/param3.incl ../../../include/rpstar.incl
swrad.o: ../../../include/parame.incl ../../../include/point3d.incl
swrad.o: ../../../include/point2d.incl ../../../include/various.incl
swrad.o: ../../../include/radiat.incl ../../../include/nonhyd.incl
swrad.o: ../../../include/nhcnst.incl ../../../include/param2.incl
swrad.o: ../../../include/param3.incl ../../../include/rpstar.incl
