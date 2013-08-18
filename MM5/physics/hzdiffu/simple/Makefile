# Makefile for directory physics/hzdiffu/simple

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/hzdiffu/simple

OBJS =\
	coef_diffu.o \
	diffth.o \
	diffthd.o \
	diffmoi.o \
	diffintp.o \
	diffu.o

SRC =\
	coef_diffu.i \
	diffth.i \
	diffthd.i \
	diffmoi.i \
	diffintp.i \
	diffu.i

SRCF =\
	coef_diffu.f \
	diffth.f \
	diffthd.f \
	diffmoi.f \
	diffintp.f \
	diffu.f

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

coef_diffu.o: ../../../include/parame.incl
diffu.o: ../../../include/parame.incl
diffth.o: ../../../include/parame.incl
diffthd.o: ../../../include/parame.incl
diffmoi.o: ../../../include/parame.incl
diffintp.o: ../../../include/parame.incl
