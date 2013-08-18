# Makefile for directory reisner2

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/explicit/schultz

OBJS =\
	schultz.o \
	schultz_mic.o

SRC =\
	schultz.i \
	schultz_mic.i

SRCF =\
	schultz.f \
	schultz_mic.f

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

schultz.o: ../../../include/parame.incl ../../../include/varia.incl
schultz.o: ../../../include/param3.incl ../../../include/param2.incl
schultz.o: ../../../include/pmoist.incl ../../../include/point3d.incl
schultz.o: ../../../include/point2d.incl ../../../include/various.incl
schultz.o: ../../../include/nonhyd.incl ../../../include/nhcnst.incl
schultz.o: ../../../include/rpstar.incl
