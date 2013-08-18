# Makefile for directory physics/cumulus/kf

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/cumulus/kf

OBJS =\
	cupara6.o \
	dtfrznew.o \
	kfpara.o \
	tpdd.o \
	tpmix.o

SRC =\
	cupara6.i \
	dtfrznew.i \
	kfpara.i \
	tpdd.i \
	tpmix.i

SRCF =\
	cupara6.f \
	dtfrznew.f \
	kfpara.f \
	tpdd.f \
	tpmix.f

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

cupara6.o: ../../../include/parame.incl ../../../include/various.incl
cupara6.o: ../../../include/param3.incl ../../../include/addrcu.incl
cupara6.o: ../../../include/pmoist.incl ../../../include/nhcnst.incl
kfpara.o: ../../../include/parame.incl ../../../include/various.incl
kfpara.o: ../../../include/param2.incl ../../../include/param3.incl
kfpara.o: ../../../include/pmoist.incl ../../../include/varia.incl
kfpara.o: ../../../include/addrcu.incl
kfpara.o: ../../../include/rpstar.incl ../../../include/parkfcu.incl
