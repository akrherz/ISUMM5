# Makefile for directory fdda/grid

DEVTOP = ../..
include ../../configure.user

CURRENT_DIR = $(DEVTOP)/fdda/grid

OBJS =\
	nudgd.o \
	setupgd.o \
	qsatgd.o \
	nopro.o \
	julgmt.o \
	intpsgd.o \
	in4dgd.o \
	conv3.o \
	bufvdgd.o \
	bufslgd.o \
	blw.o \
	blnudgd.o \
	blbrgd.o

SRC =\
	nudgd.i \
	setupgd.i \
	qsatgd.i \
	nopro.i \
	julgmt.i \
	intpsgd.i \
	in4dgd.i \
	conv3.i \
	bufvdgd.i \
	bufslgd.i \
	blw.i \
	blnudgd.i \
	blbrgd.i

SRCF =\
	nudgd.f \
	setupgd.f \
	qsatgd.f \
	nopro.f \
	julgmt.f \
	intpsgd.f \
	in4dgd.f \
	conv3.f \
	bufvdgd.f \
	bufslgd.f \
	blw.f \
	blnudgd.f \
	blbrgd.f

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

blnudgd.o: ../../include/parame.incl ../../include/param2.incl
blnudgd.o: ../../include/various.incl ../../include/parfdda.incl
blw.o: ../../include/parame.incl
bufslgd.o: ../../include/parame.incl ../../include/param2.incl
bufslgd.o: ../../include/various.incl ../../include/parfdda.incl
bufvdgd.o: ../../include/parame.incl ../../include/param2.incl
bufvdgd.o: ../../include/various.incl ../../include/parfdda.incl
in4dgd.o: ../../include/parame.incl ../../include/param2.incl
in4dgd.o: ../../include/param3.incl ../../include/fddagd.incl
in4dgd.o: ../../include/various.incl ../../include/hdrv3.incl
in4dgd.o: ../../include/parfdda.incl
intpsgd.o: ../../include/parame.incl ../../include/param2.incl
intpsgd.o: ../../include/various.incl ../../include/parfdda.incl
nudgd.o: ../../include/parame.incl ../../include/param2.incl
nudgd.o: ../../include/various.incl ../../include/parfdda.incl
qsatgd.o: ../../include/parame.incl ../../include/various.incl
qsatgd.o: ../../include/pmoist.incl ../../include/rpstar.incl
setupgd.o: ../../include/parame.incl ../../include/param2.incl
setupgd.o: ../../include/param3.incl ../../include/various.incl
setupgd.o: ../../include/fddagd.incl ../../include/parfdda.incl
