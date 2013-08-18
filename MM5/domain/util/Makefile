# Makefile for directory domain/util

DEVTOP = ../..
include ../../configure.user

CURRENT_DIR = $(DEVTOP)/domain/util

OBJS =\
	dcpl3d.o \
	dcpl3dwnd.o \
	couple.o \
	decouple.o \
	date.o \
	dots.o \
	equate.o \
	fill.o \
	fillcrs.o \
	skipf.o \
	smt2.o \
	smther.o \
	xtdot.o

SRC =\
	dcpl3d.i \
	dcpl3dwnd.i \
	couple.i \
	decouple.i \
	date.i \
	dots.i \
	equate.i \
	fill.i \
	fillcrs.i \
	skipf.i \
	smt2.i \
	smther.i \
	xtdot.i

SRCF =\
	dcpl3d.f \
	dcpl3dwnd.f \
	couple.f \
	decouple.f \
	date.f \
	dots.f \
	equate.f \
	fill.f \
	fillcrs.f \
	skipf.f \
	smt2.f \
	smther.f \
	xtdot.f

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

dcpl3d.o: ../../include/parame.incl
dcpl3dwnd.o: ../../include/parame.incl
smt2.o: ../../include/parame.incl
xtdot.o: ../../include/parame.incl
fill.o: ../../include/parame.incl
