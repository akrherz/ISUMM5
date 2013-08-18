# Makefile for directory domain/boundary

DEVTOP = ../..
include ../../configure.user

CURRENT_DIR = $(DEVTOP)/domain/boundary

OBJS =\
	bdyin.o \
	bdyrst.o \
	bdyten.o \
	bdyval.o \
	lbdyin.o \
	nudge.o 

SRC =\
	bdyin.i \
	bdyrst.i \
	bdyten.i \
	bdyval.i \
	lbdyin.i \
	nudge.i 

SRCF =\
	bdyin.f \
	bdyrst.f \
	bdyten.f \
	bdyval.f \
	lbdyin.f \
	nudge.f 

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

bdyin.o: ../../include/parame.incl ../../include/addr0.incl
bdyin.o: ../../include/various.incl ../../include/nonhydb.incl
bdyin.o: ../../include/pointbc.incl ../../include/hdrv3.incl
bdyin.o: ../../include/chardate.incl ../../include/nhcnst.incl
bdyrst.o: ../../include/parame.incl
bdyten.o: ../../include/parame.incl
bdyval.o: ../../include/parame.incl ../../include/various.incl
lbdyin.o: ../../include/parame.incl ../../include/chardate.incl
lbdyin.o: ../../include/various.incl ../../include/hdrv3.incl
lbdyin.o: ../../include/variousn.incl ../../include/nestl.incl
lbdyin.o: ../../include/point2d.incl ../../include/soilp.incl
lbdyin.o: ../../include/point2dn.incl ../../include/soilpn.incl
lbdyin.o: ../../include/addr0.incl
nudge.o: ../../include/parame.incl
