# Makefile for directory physics/pbl_sfc/gspbl

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/pbl_sfc/gspbl

OBJS =\
	gspbl.o

SRC =\
	gspbl.i

SRCF =\
	gspbl.f

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

gspbl.o: ../../../include/defines.incl
gspbl.o: ../../../include/parame.incl ../../../include/rpstar.incl
gspbl.o: ../../../include/varia.incl ../../../include/dusolve1.incl
gspbl.o: ../../../include/param2.incl ../../../include/param3.incl
gspbl.o: ../../../include/pmoist.incl ../../../include/point3d.incl
gspbl.o: ../../../include/point2d.incl ../../../include/various.incl
gspbl.o: ../../../include/nonhyd.incl ../../../include/nhcnst.incl
gspbl.o: ../../../include/addrcu.incl ../../../include/radiat.incl
gspbl.o: ../../../include/fogstuf.incl ../../../include/fog1d.incl
gspbl.o: ../../../include/pbltke.incl  ../../../include/soil.incl
gspbl.o: ../../../include/surface.incl ../../../include/surfslab.incl
gspbl.o: ../../../include/addrfog.incl 
