# Makefile for directory physics/pbl_sfc/noahlsm

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/pbl_sfc/noahlsm

OBJS =\
	surfce.o \
	sflx.o 
SRC =\
	surfce.i \
	sflx.i 

SRCF =\
	surfce.f \
	sflx.f 

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

surfce.o: ../../../include/parame.incl ../../../include/rpstar.incl
surfce.o: ../../../include/soilp.incl ../../../include/landuse.incl
surfce.o: ../../../include/various.incl ../../../include/param3.incl
surfce.o: ../../../include/soilcnst.incl ../../../include/paramveg_USGS.incl
surfce.o: ../../../include/paramsoil_STAS.incl
sflx.o: ../../../include/paramsoil_STAS.incl ../../../include/paramveg_USGS.incl
sflx.o: ../../../include/paramgen_LSM.incl

