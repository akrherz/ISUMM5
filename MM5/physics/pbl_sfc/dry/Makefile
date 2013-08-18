# Makefile for directory physics/pbl_sfc/dry

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/pbl_sfc/dry

OBJS =\
	cadjmx.o \
	convad.o \
	gauss.o

SRC =\
	cadjmx.i \
	convad.i \
	gauss.i

SRCF =\
	cadjmx.f \
	convad.f \
	gauss.f

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

convad.o: ../../../include/parame.incl ../../../include/rpstar.incl
convad.o: ../../../include/param3.incl ../../../include/param2.incl
convad.o: ../../../include/pmoist.incl ../../../include/nonhyd.incl
