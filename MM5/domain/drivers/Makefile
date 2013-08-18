# Makefile for directory domain/drivers

DEVTOP = ../..
include ../../configure.user

CURRENT_DIR = $(DEVTOP)/domain/drivers

OBJS =\
	nstlev1.o \
	nstlev2.o \
	nstlev3.o \
	nstlev4.o \
	nstlev5.o

SRC =\
	nstlev1.i \
	nstlev2.i \
	nstlev3.i \
	nstlev4.i \
	nstlev5.i

SRCF =\
	nstlev1.f \
	nstlev2.f \
	nstlev3.f \
	nstlev4.f \
	nstlev5.f

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

nstlev1.o: ../../include/defines.incl ../../include/parame.incl
nstlev1.o: ../../include/param2.incl ../../include/param3.incl
nstlev1.o: ../../include/addr0.incl ../../include/pointbc.incl
nstlev1.o: ../../include/nonhydb.incl ../../include/various.incl
nstlev1.o: ../../include/variousn.incl
nstlev2.o: ../../include/defines.incl ../../include/parame.incl
nstlev2.o: ../../include/param2.incl ../../include/param3.incl
nstlev2.o: ../../include/varia.incl ../../include/addr0.incl
nstlev2.o: ../../include/various.incl ../../include/variousn.incl
nstlev3.o: ../../include/defines.incl ../../include/parame.incl
nstlev3.o: ../../include/param2.incl ../../include/param3.incl
nstlev3.o: ../../include/varia.incl ../../include/addr0.incl
nstlev3.o: ../../include/various.incl ../../include/variousn.incl
nstlev4.o: ../../include/defines.incl ../../include/parame.incl
nstlev4.o: ../../include/param2.incl ../../include/param3.incl
nstlev4.o: ../../include/varia.incl ../../include/addr0.incl
nstlev4.o: ../../include/various.incl ../../include/variousn.incl
nstlev5.o: ../../include/defines.incl ../../include/parame.incl
nstlev5.o: ../../include/param2.incl ../../include/param3.incl
nstlev5.o: ../../include/varia.incl ../../include/addr0.incl
nstlev5.o: ../../include/various.incl ../../include/variousn.incl
