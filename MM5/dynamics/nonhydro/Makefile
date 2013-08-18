# Makefile for directory dynamics/nonhydro

DEVTOP = ../..
include ../../configure.user

CURRENT_DIR = $(DEVTOP)/dynamics/nonhydro

OBJS =\
	solve.o \
	sound.o

SRC =\
	solve.i \
	sound.i

SRCF =\
	solve.f \
	sound.f

LIBTARGET = util
TARGETDIR = ../../

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

solve.o: ../../include/defines.incl ../../include/parame.incl
solve.o: ../../include/addr0.incl ../../include/parccm2.incl
solve.o: ../../include/varia.incl ../../include/param2.incl
solve.o: ../../include/parfdda.incl ../../include/param3.incl
solve.o: ../../include/pmoist.incl ../../include/point3d.incl
solve.o: ../../include/point2d.incl ../../include/pointbc.incl
solve.o: ../../include/various.incl 
solve.o: ../../include/fddagd.incl ../../include/fddaob.incl
solve.o: ../../include/radiat.incl ../../include/navypb.incl
solve.o: ../../include/nonhyd.incl ../../include/nonhydb.incl
solve.o: ../../include/nhcnst.incl ../../include/rpstar.incl
solve.o: ../../include/addrcu.incl ../../include/sum.incl
solve.o: ../../include/fogstuf.incl ../../include/addrfog.incl
solve.o: ../../include/pbltke.incl  ../../include/zdiffu.incl
solve.o: ../../include/comicl.incl  ../../include/zdiffu2.incl
solve.o: ../../include/landuse.incl ../../include/soil.incl
sound.o: ../../include/parame.incl ../../include/param2.incl
sound.o: ../../include/param3.incl ../../include/pmoist.incl
sound.o: ../../include/nonhyd.incl ../../include/nonhydb.incl
sound.o: ../../include/nhcnst.incl ../../include/uprad.incl
sound.o: ../../include/rpstar.incl
