# Makefile for directory physics/cumulus/as

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/cumulus/as

OBJS =\
	kerhel.o \
	entr.o \
	cupara4.o \
	cloudw.o \
	clodwd.o \
	arasch.o \
	araout.o \
	aramb.o \
	soundd.o \
	zx4lp.o

SRC =\
	kerhel.i \
	entr.i \
	cupara4.i \
	cloudw.i \
	clodwd.i \
	arasch.i \
	araout.i \
	aramb.i \
	soundd.i \
	zx4lp.i
SRCF =\
	kerhel.f \
	entr.f \
	cupara4.f \
	cloudw.f \
	clodwd.f \
	arasch.f \
	araout.f \
	aramb.f \
	soundd.f \
	zx4lp.f

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

arasch.o: ../../../include/parame.incl ../../../include/pmoist.incl
arasch.o: ../../../include/param2.incl ../../../include/param3.incl
cupara4.o: ../../../include/parame.incl ../../../include/param2.incl
cupara4.o: ../../../include/param3.incl ../../../include/pmoist.incl
cupara4.o: ../../../include/various.incl ../../../include/nonhyd.incl
cupara4.o: ../../../include/addras.incl
