# Makefile for directory physics/advection/simple

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/advection/simple

OBJS =\
	hadv.o \
	vadv.o \
	vadv2.o

SRC =\
	hadv.i \
	vadv.i \
	vadv2.i

SRCF =\
	hadv.f \
	vadv.f \
	vadv2.f

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

hadv.o: ../../../include/parame.incl ../../../include/param3.incl
hadv.o: ../../../include/various.incl
vadv.o: ../../../include/parame.incl ../../../include/param3.incl
vadv.o: ../../../include/various.incl ../../../include/nhcnst.incl
vadv2.o: ../../../include/parame.incl ../../../include/param3.incl
vadv2.o: ../../../include/various.incl ../../../include/nhcnst.incl
