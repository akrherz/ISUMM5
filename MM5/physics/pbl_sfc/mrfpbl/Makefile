# Makefile for directory physics/pbl_sfc/mrfpbl

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/pbl_sfc/mrfpbl

OBJS =\
	mrfpbl.o \
	tridi2.o

SRC =\
	mrfpbl.i \
	tridi2.i

SRCF =\
	mrfpbl.f \
	tridi2.f

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

mrfpbl.o: ../../../include/parame.incl
mrfpbl.o: ../../../include/varia.incl
mrfpbl.o: ../../../include/param2.incl ../../../include/param3.incl
mrfpbl.o: ../../../include/various.incl
mrfpbl.o: ../../../include/soilcnst.incl
mrfpbl.o: ../../../include/pbltb.incl
mrfpbl.o: ../../../include/sum.incl
mrfpbl.o: ../../../include/defines.incl
tridi2.o: ../../../include/parame.incl
tridi2.o: ../../../include/defines.incl
