# Makefile for directory physics/explicit/nonconv

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/explicit/nonconv

OBJS =\
	nconvp.o

SRC =\
	nconvp.i

SRCF =\
	nconvp.f

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

nconvp.o: ../../../include/parame.incl ../../../include/param3.incl
nconvp.o: ../../../include/param2.incl
nconvp.o: ../../../include/pmoist.incl ../../../include/rpstar.incl
