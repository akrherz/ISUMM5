# Makefile for directory physics/cumulus/grell

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/cumulus/grell

OBJS =\
	cup.o \
	cupara3.o \
	maximi.o \
	minimi.o

SRC =\
	cup.i \
	cupara3.i \
	maximi.i \
	minimi.i

SRCF =\
	cup.f \
	cupara3.f \
	maximi.f \
	minimi.f

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

cup.o: ../../../include/parame.incl 
cup.o: ../../../include/parccm2.incl
cup.o: ../../../include/pargrcu.incl
cup.o: ../../../include/defines.incl
cupara3.o: ../../../include/parame.incl ../../../include/param2.incl
cupara3.o: ../../../include/parccm2.incl
cupara3.o: ../../../include/defines.incl
cupara3.o: ../../../include/nonhyd.incl
cupara3.o: ../../../include/pargrcu.incl
