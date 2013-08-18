# Makefile for directory physics/cumulus/kf

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/cumulus/bm

OBJS =\
	bmpara.o \
	cupara7.o \
	lutbl.o \
	spline.o \
	tpfc.o

SRC =\
	bmpara.i \
	cupara7.i \
	lutbl.i \
	spline.i \
	tpfc.i

SRCF =\
	bmpara.f \
	cupara7.f \
	lutbl.f \
	spline.f \
	tpfc.f

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

bmpara.o: ../../../include/parame.incl ../../../include/rpstar.incl
bmpara.o: ../../../include/param2.incl ../../../include/pmoist.incl
bmpara.o: ../../../include/various.incl
cupara7.o: ../../../include/parame.incl ../../../include/param3.incl
cupara7.o: ../../../include/various.incl ../../../include/parbmcu.incl
