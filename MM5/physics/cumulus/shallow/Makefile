# Makefile for directory physics/cumulus/shallow

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/cumulus/shallow

OBJS =\
	cloudws.o \
	entrs.o \
	kerhels.o \
	shallcu.o \
	shallow.o

OBJS_ISHALLO1 =\
	araouts.o

SRC =\
	cloudws.i \
	entrs.i \
	kerhels.i \
	shallcu.i \
	shallow.i

SRC_ISHALLO1 =\
	araouts.i

SRCF =\
	cloudws.f \
	entrs.f \
	kerhels.f \
	shallcu.f \
	shallow.f

SRCF_ISHALLO1 =\
	araouts.f

LIBTARGET = util
TARGETDIR = ../../../

all:: $(OBJS)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS)

ishallo1:: $(OBJS) $(OBJS_ISHALLO1)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS) $(OBJS_ISHALLO1)

code: $(SRC)

ishallo1_code:: $(SRC) $(SRC_ISHALLO1)

little_f:: $(SRCF) $(OBJS)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS)
ishallo1_little_f:: $(SRCF) $(SRCF_ISHALLO1) $(OBJS) $(OBJS_ISHALLO1)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS) $(OBJS_ISHALLO1)

# -------------------------------------------------------------------------
# common rules for all Makefiles - do not edit

emptyrule::

clean::
	$(RM_CMD) "#"*

# -------------------------------------------------------------------------
# DO NOT DELETE THIS LINE -- make depend depends on it.

shallcu.o: ../../../include/parame.incl ../../../include/param2.incl
shallcu.o: ../../../include/param3.incl ../../../include/pmoist.incl
shallcu.o: ../../../include/various.incl
shallow.o: ../../../include/parame.incl ../../../include/param2.incl
shallow.o: ../../../include/param3.incl ../../../include/pmoist.incl
