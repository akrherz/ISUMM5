# Makefile for directory reisner2

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/explicit/gsfc

OBJS =\
	falflux.o \
	godmic.o \
	satice.o

SRC =\
	falflux.i \
	godmic.i \
	satice.i

SRCF =\
	falflux.f \
	godmic.f \
	satice.f

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

falflux.o: ../../../include/parame.incl
godmic.o: ../../../include/parame.incl ../../../include/param2.incl
godmic.o: ../../../include/pmoist.incl ../../../include/varia.incl
godmic.o: ../../../include/point3d.incl ../../../include/point2d.incl
godmic.o: ../../../include/various.incl ../../../include/nonhyd.incl
godmic.o: ../../../include/nhcnst.incl ../../../include/rpstar.incl
satice.o: ../../../include/parame.incl
