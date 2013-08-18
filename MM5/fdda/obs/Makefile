# Makefile for directory fdda/obs

DEVTOP = ../..
include ../../configure.user

CURRENT_DIR = $(DEVTOP)/fdda/obs

OBJS =\
	errob.o \
	in4dob.o \
	nudob.o

SRC =\
	errob.i \
	in4dob.i \
	nudob.i

SRCF =\
	errob.f \
	in4dob.f \
	nudob.f

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

errob.o: ../../include/parame.incl ../../include/fddaob.incl
errob.o: ../../include/param2.incl ../../include/various.incl
errob.o: ../../include/rpstar.incl ../../include/parfdda.incl
in4dob.o: ../../include/parame.incl ../../include/param2.incl
in4dob.o: ../../include/param3.incl ../../include/fddaob.incl
in4dob.o: ../../include/parfdda.incl
nudob.o: ../../include/parame.incl ../../include/fddaob.incl
nudob.o: ../../include/param2.incl ../../include/various.incl
nudob.o: ../../include/parfdda.incl
