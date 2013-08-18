# Makefile for directory reisner1

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physcis/explicit/reisner1

OBJS =\
	exmoisr.o

OBJS_LEX =\
	lexmoisr.o

OBJS_ZEX =\
	zexmoisr.o

SRC =\
	exmoisr.i

SRC_LEX =\
	lexmoisr.i

SRC_ZEX =\
	zexmoisr.i

SRCF =\
	exmoisr.f

SRCF_LEX =\
	lexmoisr.f

SRCF_ZEX =\
	zexmoisr.f

LIBTARGET = util
TARGETDIR = ../../../

ALLFILES = $(SRCS)

all:: $(OBJS)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS)

lex:: lexmoisr.o
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS_LEX)

zex:: zexmoisr.o
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS_ZEX)

code:: $(SRC)

lex_code:: $(SRC_LEX)

zex_code:: $(SRC_ZEX)

little_f:: $(SRCF) $(OBJS)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS)

lex_little_f:: $(SRCF_LEX) $(OBJS_LEX)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS_LEX)

zex_little_f:: $(SRCF_ZEX) $(OBJS_ZEX)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(ONJS_ZEX)

# -------------------------------------------------------------------------
# common rules for all Makefiles - do not edit

emptyrule::

clean::
	$(RM_CMD) "#"*

# -------------------------------------------------------------------------
# DO NOT DELETE THIS LINE -- make depend depends on it.

exmoisr.o: ../../../include/parame.incl ../../../include/varia.incl
exmoisr.o: ../../../include/param3.incl ../../../include/param2.incl
exmoisr.o: ../../../include/pmoist.incl ../../../include/nhcnst.incl
exmoisr.o: ../../../include/various.incl
exmoisr.o: ../../../include/defines.incl ../../../include/jrg.incl
lexmoisr.o: ../../../include/parame.incl ../../../include/varia.incl
lexmoisr.o: ../../../include/param3.incl ../../../include/pmoist.incl
lexmoisr.o: ../../../include/various.incl ../../../include/nonhyd.incl
lexmoisr.o: ../../../include/nhcnst.incl ../../../include/rpstar.incl
lexmoisr.o: ../../../include/hdtabl.incl ../../../include/functb.incl
lexmoisr.o: ../../../include/param2.incl ../../../include/jrg.incl
lexmoisr.o: ../../../include/nhcnst.incl
zexmoisr.o: ../../../include/parame.incl ../../../include/varia.incl
zexmoisr.o: ../../../include/param3.incl ../../../include/param2.incl
zexmoisr.o: ../../../include/pmoist.incl ../../../include/nhcnst.incl
zexmoisr.o: ../../../include/various.incl
zexmoisr.o: ../../../include/defines.incl ../../../include/jrg.incl
