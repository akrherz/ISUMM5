# Makefile for directory physics/radiation/sfc

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/radiation/sfc

OBJS =\
	sfcrad.o \
	trans.o \
	transm.o

SRC =\
	sfcrad.i \
	trans.i \
	transm.i

SRCF =\
	sfcrad.f \
	trans.f \
	transm.f

LIBTARGET = util
TARGETDIR = ../../../

ALLFILES = $(SRCS)

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

sfcrad.o: ../../../include/parame.incl
sfcrad.o: ../../../include/varia.incl ../../../include/param2.incl
sfcrad.o: ../../../include/param3.incl ../../../include/pmoist.incl
sfcrad.o: ../../../include/point2d.incl ../../../include/various.incl
sfcrad.o: ../../../include/addrcu.incl
sfcrad.o: ../../../include/defines.incl
transm.o: ../../../include/parame.incl ../../../include/param3.incl
transm.o: ../../../include/nonhyd.incl
trans.o: ../../../include/parame.incl ../../../include/param3.incl
