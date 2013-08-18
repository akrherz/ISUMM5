# Makefile for directory physics/explicit/simple

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/explicit/simple

OBJS =\
	exmoiss.o

OBJS_LEX =\
	lexmoiss.o

SRC =\
	exmoiss.i

SRC_LEX =\
	lexmoiss.i

SRCF =\
	exmoiss.f

SRCF_LEX =\
	lexmoiss.f

LIBTARGET = util
TARGETDIR = ../../../

all:: $(OBJS)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS)

lex:: lexmoiss.o
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS_LEX)

code:: $(SRC)

lex_code:: $(SRC_LEX)

little_f:: $(SRCF) $(OBJS)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS)

lex_little_f:: $(SRCF_LEX) $(OBJS_LEX)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS_LEX)


# -------------------------------------------------------------------------
# common rules for all Makefiles - do not edit

emptyrule::

clean::
	$(RM_CMD) "#"*

# -------------------------------------------------------------------------
# DO NOT DELETE THIS LINE -- make depend depends on it.

exmoiss.o: ../../../include/parame.incl ../../../include/varia.incl
exmoiss.o: ../../../include/param3.incl ../../../include/param2.incl
exmoiss.o: ../../../include/pmoist.incl ../../../include/point3d.incl
exmoiss.o: ../../../include/point2d.incl ../../../include/various.incl
exmoiss.o: ../../../include/dusolve1.incl ../../../include/nonhyd.incl
exmoiss.o: ../../../include/nhcnst.incl ../../../include/rpstar.incl
lexmoiss.o: ../../../include/parame.incl ../../../include/varia.incl
lexmoiss.o: ../../../include/param3.incl ../../../include/pmoist.incl
lexmoiss.o: ../../../include/point3d.incl ../../../include/point2d.incl
lexmoiss.o: ../../../include/various.incl ../../../include/dusolve1.incl
lexmoiss.o: ../../../include/nonhyd.incl ../../../include/nhcnst.incl
lexmoiss.o: ../../../include/rpstar.incl ../../../include/hdtabl.incl
lexmoiss.o: ../../../include/functb.incl ../../../include/param2.incl
