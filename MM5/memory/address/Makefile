# Makefile for directory memory/address
# Dan Hansen,NCAR
#

DEVTOP = ../..
include ../../configure.user

CURRENT_DIR = $(DEVTOP)/memory/address

OBJS =\
	addall.o \
	addrx1c.o

OBJS_MAXNES = addrx1n.o

SRC =\
	addall.i \
	addrx1c.i

SRC_MAXNES = addrx1n.i

SRCF =\
	addall.f \
	addrx1c.f

SRCF_MAXNES = addrx1n.f

LIBTARGET = util
TARGETDIR = ../../

all:: $(OBJS)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS)

maxnes:: $(OBJS) $(OBJS_MAXNES)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS) $(OBJS_MAXNES)

code:: $(SRC)

maxnes_code:: $(SRC) $(SRC_MAXNES)

little_f:: $(SRCF) $(OBJS)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS)

maxnes_little_f:: $(SRCF) $(SRCF_MAXNES) $(OBJS) $(OBJS_MAXNES)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS) $(OBJS_MAXNES)

# -------------------------------------------------------------------------
# common rules for all Makefiles - do not edit

emptyrule::

clean::
	$(RM_CMD) "#"*

# -------------------------------------------------------------------------
# DO NOT DELETE THIS LINE -- make depend depends on it.

addall.o: ../../include/parame.incl ../../include/addr0.incl
addrx1c.o: ../../include/parame.incl ../../include/point3d.incl
addrx1c.o: ../../include/point2d.incl ../../include/pointbc.incl
addrx1c.o: ../../include/various.incl 
addrx1c.o: ../../include/addrcu.incl
addrx1c.o: ../../include/addras.incl ../../include/fddagd.incl
addrx1c.o: ../../include/fddaob.incl ../../include/navypb.incl
addrx1c.o: ../../include/nonhyd.incl ../../include/nonhydb.incl
addrx1c.o: ../../include/nhcnst.incl ../../include/soil.incl
addrx1c.o: ../../include/soilp.incl
addrx1c.o: ../../include/pbltke.incl ../../include/addrfog.incl
addrx1c.o: ../../include/radiat.incl ../../include/uprad.incl
addrx1n.o: ../../include/parame.incl ../../include/point3dn.incl
addrx1n.o: ../../include/point2dn.incl ../../include/pointbcn.incl
addrx1n.o: ../../include/variousn.incl ../../include/nestl.incl
addrx1n.o: ../../include/addrcun.incl ../../include/addrasn.incl
addrx1n.o: ../../include/pnavyn.incl ../../include/nnnhyd.incl
addrx1n.o: ../../include/nnnhydb.incl ../../include/nncnst.incl
addrx1n.o: ../../include/soiln.incl  ../../include/soilpn.incl
addrx1n.o: ../../include/pbltken.incl ../../include/addrfogn.incl

