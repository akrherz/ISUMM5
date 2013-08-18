# Makefile for directory ./physics/radiation/ccm2

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/radiation/ccm2

OBJS =\
	cldems.o \
	colmod.o \
	fetchd.o \
	getabe.o \
	getdat.o \
	putabe.o \
	radabs.o \
	radclr.o \
	radclw.o \
	radcsw.o \
	radctl.o \
	radded.o \
	radems.o \
	radini.o \
	radinp.o \
	radout.o \
	radtpl.o \
	resetr.o \
	stored.o \
	wheneq.o \
	whenfgt.o \
	zenitm.o


SRC =\
	cldems.i \
	colmod.i \
	fetchd.i \
	getabe.i \
	getdat.i \
	putabe.i \
	radabs.i \
	radclr.i \
	radclw.i \
	radcsw.i \
	radctl.i \
	radded.i \
	radems.i \
	radini.i \
	radinp.i \
	radout.i \
	radtpl.i \
	resetr.i \
	stored.i \
	wheneq.i \
	whenfgt.i \
	zenitm.i

SRCF =\
	cldems.f \
	colmod.f \
	fetchd.f \
	getabe.f \
	getdat.f \
	putabe.f \
	radabs.f \
	radclr.f \
	radclw.f \
	radcsw.f \
	radctl.f \
	radded.f \
	radems.f \
	radini.f \
	radinp.f \
	radout.f \
	radtpl.f \
	resetr.f \
	stored.f \
	wheneq.f \
	whenfgt.f \
	zenitm.f

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

cldems.o: ../../../include/parame.incl ../../../include/parccm2.incl
cldems.o: ../../../include/radccm2.incl
colmod.o: ../../../include/parame.incl ../../../include/param3.incl
colmod.o: ../../../include/parccm2.incl ../../../include/various.incl
getdat.o: ../../../include/parame.incl ../../../include/param3.incl
getdat.o: ../../../include/point2d.incl ../../../include/point3d.incl
getdat.o: ../../../include/pmoist.incl ../../../include/parccm2.incl
getdat.o: ../../../include/radccm2.incl ../../../include/nonhyd.incl
radabs.o: ../../../include/parame.incl ../../../include/parccm2.incl
radabs.o: ../../../include/radccm2.incl
radclr.o: ../../../include/parame.incl ../../../include/param3.incl
radclr.o: ../../../include/parccm2.incl ../../../include/radccm2.incl
radclw.o: ../../../include/parame.incl ../../../include/parccm2.incl
radclw.o: ../../../include/radccm2.incl ../../../include/various.incl
radcsw.o: ../../../include/parame.incl ../../../include/param3.incl
radcsw.o: ../../../include/parccm2.incl ../../../include/radccm2.incl
radcsw.o: ../../../include/various.incl ../../../include/point2d.incl
radctl.o: ../../../include/parame.incl ../../../include/param3.incl
radctl.o: ../../../include/parccm2.incl ../../../include/radccm2.incl
radctl.o: ../../../include/various.incl
radded.o: ../../../include/parame.incl ../../../include/parccm2.incl
radded.o: ../../../include/radccm2.incl ../../../include/various.incl
radems.o: ../../../include/parame.incl ../../../include/parccm2.incl
radems.o: ../../../include/radccm2.incl
radini.o: ../../../include/parame.incl ../../../include/parccm2.incl
radini.o: ../../../include/radccm2.incl
radinp.o: ../../../include/parame.incl ../../../include/parccm2.incl
radinp.o: ../../../include/radccm2.incl
radout.o: ../../../include/parame.incl ../../../include/parccm2.incl
radout.o: ../../../include/radccm2.incl ../../../include/radiat.incl
radout.o: ../../../include/various.incl
radtpl.o: ../../../include/parame.incl ../../../include/parccm2.incl
radtpl.o: ../../../include/radccm2.incl
zenitm.o: ../../../include/parame.incl ../../../include/param3.incl
zenitm.o: ../../../include/parccm2.incl ../../../include/point2d.incl
zenitm.o: ../../../include/various.incl ../../../include/param2.incl
