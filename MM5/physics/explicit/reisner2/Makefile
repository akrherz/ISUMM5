# Makefile for directory reisner2

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/explicit/reisner2

OBJS =\
	exmoisg.o rslf.o

SRC =\
	exmoisg.i rslf.i

SRCF =\
	exmoisg.f rslf.f

LIBTARGET = util
TARGETDIR = ../../../

ALLFILES = $(SRCS)

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

exmoisg.o: ../../../include/parame.incl ../../../include/varia.incl
exmoisg.o: ../../../include/param3.incl ../../../include/param2.incl
exmoisg.o: ../../../include/pmoist.incl ../../../include/point3d.incl
exmoisg.o: ../../../include/point2d.incl ../../../include/various.incl
exmoisg.o: ../../../include/nhcnst.incl
exmoisg.o: ../../../include/rpstar.incl ../../../include/jrg.incl
