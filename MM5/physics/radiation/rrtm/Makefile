# Makefile for directory ./physics/radiation/rrtm

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/radiation/rrtm

OBJS =\
	mm5atm.o \
	rrtm.o \
	rrtm_gasabs.o \
	rrtm_init.o \
	rrtm_k_g.o \
	rrtm_rtrn.o \
	rrtm_setcoef.o \
	rrtm_taumol.o


SRC =\
	mm5atm.i \
	rrtm.i \
	rrtm_gasabs.i \
	rrtm_init.i \
	rrtm_k_g.i \
	rrtm_rtrn.i \
	rrtm_setcoef.i \
	rrtm_taumol.i

SRCF =\
	mm5atm.f \
	rrtm.f \
	rrtm_gasabs.f \
	rrtm_init.f \
	rrtm_k_g.f \
	rrtm_rtrn.f \
	rrtm_setcoef.f \
	rrtm_taumol.f

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

rrtm.o: ../../../include/parame.incl 
mm5atm.o: ../../../include/parame.incl ../../../include/point3d.incl
mm5atm.o: ../../../include/point2d.incl ../../../include/param3.incl
