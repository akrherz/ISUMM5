# Makefile for directory physics/cumulus/kf2

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/cumulus/kf2

OBJS =\
	cupara8.o \
	dtfrz2.o \
	kfpara2.o \
	kfdrive.o \
	lutab.o \
	tpmix2dd.o \
	tpmix2.o \
	tp_cape.o

SRC =\
	cupara8.i \
	dtfrz2.i \
	kfpara2.i \
	kfdrive.i \
	lutab.i \
	tpmix2dd.i \
	tpmix2.i \
	tp_cape.i

SRCF =\
	cupara8.f \
	dtfrz2.f \
	kfpara2.f \
	kfdrive.f \
	lutab.f \
	tpmix2dd.f \
	tpmix2.f \
	tp_cape.f

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

cupara8.o: ../../../include/parame.incl ../../../include/various.incl
cupara8.o: ../../../include/param3.incl ../../../include/addrcu.incl
cupara8.o: ../../../include/pmoist.incl ../../../include/nonhyd.incl
cupara8.o: ../../../include/nhcnst.incl
kfdrive.o: ../../../include/parame.incl ../../../include/various.incl
kfdrive.o: ../../../include/param2.incl ../../../include/param3.incl
kfdrive.o: ../../../include/pmoist.incl ../../../include/varia.incl
kfdrive.o: ../../../include/rpstar.incl ../../../include/addrcu.incl
kfdrive.o: ../../../include/parkfcu.incl
kfpara2.o: ../../../include/parame.incl ../../../include/various.incl
kfpara2.o: ../../../include/param2.incl ../../../include/param3.incl
kfpara2.o: ../../../include/pmoist.incl ../../../include/varia.incl
kfpara2.o: ../../../include/addrcu.incl
kfpara2.o: ../../../include/rpstar.incl ../../../include/parkfcu.incl
