# Makefile for directory physics/pbl_sfc/util

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/pbl_sfc/util

OBJS =\
	slab.o \
	setvegfr.o 

SRC =\
	slab.i \
	setvegfr.i 

SRCF =\
	slab.f \
	setvegfr.f 

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

slab.o: ../../../include/parame.incl ../../../include/param2.incl
slab.o: ../../../include/param3.incl ../../../include/pmoist.incl
slab.o: ../../../include/varia.incl ../../../include/nonhyd.incl
slab.o: ../../../include/point3d.incl ../../../include/point2d.incl
slab.o: ../../../include/various.incl ../../../include/soil.incl
slab.o: ../../../include/soilcnst.incl ../../../include/rpstar.incl
slab.o: ../../../include/landuse.incl ../../../include/bucket.incl
slab.o: ../../../include/defines.incl
setvegfr.o: ../../../include/parame.incl ../../../include/soilp.incl
