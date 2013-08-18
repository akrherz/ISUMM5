# Makefile for directory util

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/explicit/shared

OBJS =\
	consat.o \
	gamma.o

SRC =\
	consat.i \
	gamma.i

SRCF =\
	consat.f \
	gamma.f

OBJS_TABLE =\
	settbl.o

SRC_TABLE =\
	settbl.i

SRCF_TABLE =\
	settbl.f

LIBTARGET = util
TARGETDIR = ../../../

table: $(OBJS_TABLE)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS_TABLE)

all: $(OBJS)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS)

code:: $(SRC) $(SRC_TABLE)

little_f: $(SRCF) $(OBJS)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS)

table_little_f: $(SRCF_TABLE) $(OBJS_TABLE)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS_TABLE)

# -------------------------------------------------------------------------
# common rules for all Makefiles - do not edit

emptyrule::

clean::
	$(RM_CMD) "#"*

# -------------------------------------------------------------------------
# DO NOT DELETE THIS LINE -- make depend depends on it.

settbl.o: ../../../include/hdtabl.incl ../../../include/jrg.incl
