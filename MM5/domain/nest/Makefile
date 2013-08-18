# Makefile for directory domain/nest

DEVTOP = ../..
include ../../configure.user

CURRENT_DIR = $(DEVTOP)/domain/nest

OBJS =\
	subch.o \
	stotndi.o \
	stotndt.o \
	sint.o \
	sintx.o \
	sinty.o \
	initnest.o \
	filslb.o \
	feedbk.o \
	exchani.o \
	exchanj.o \
	exaint.o \
	chknst.o \
	bdyovl1.o \
	ovlchk.o \
	nestlsm.o

SRC =\
	subch.i \
	stotndi.i \
	stotndt.i \
	sint.i \
	sintx.i \
	sinty.i \
	initnest.i \
	filslb.i \
	feedbk.i \
	exchani.i \
	exchanj.i \
	exaint.i \
	chknst.i \
	bdyovl1.i \
	ovlchk.i \
	nestlsm.i

SRCF =\
	subch.f \
	stotndi.f \
	stotndt.f \
	sint.f \
	sintx.f \
	sinty.f \
	initnest.f \
	filslb.f \
	feedbk.f \
	exchani.f \
	exchanj.f \
	exaint.f \
	chknst.f \
	bdyovl1.f \
	ovlchk.f \
	nestlsm.f

LIBTARGET = util
TARGETDIR = ../../

all:: $(OBJS)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS)

code:: $(SRC)

little_f:: $(SRCF) $(OBJS)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS)

# -------------------------------------------------------------------------
emptyrule::

clean::
	$(RM_CMD) "#"*

# -------------------------------------------------------------------------
# DO NOT DELETE THIS LINE -- make depend depends on it.

bdyovl1.o: ../../include/parame.incl ../../include/addr0.incl
bdyovl1.o: ../../include/pointbc.incl ../../include/pointbcn.incl
bdyovl1.o: ../../include/point3d.incl ../../include/point2d.incl
bdyovl1.o: ../../include/point3dn.incl ../../include/point2dn.incl
bdyovl1.o: ../../include/nonhydb.incl ../../include/nnnhydb.incl
bdyovl1.o: ../../include/nonhyd.incl ../../include/nnnhyd.incl
bdyovl1.o: ../../include/param2.incl ../../include/param3.incl
bdyovl1.o: ../../include/nestl.incl ../../include/various.incl
bdyovl1.o: ../../include/variousn.incl ../../include/varia.incl
chknst.o: ../../include/parame.incl ../../include/varia.incl
chknst.o: ../../include/param2.incl ../../include/param3.incl
chknst.o: ../../include/pmoist.incl ../../include/addr0.incl
chknst.o: ../../include/various.incl ../../include/variousn.incl
chknst.o: ../../include/nestl.incl ../../include/defines.incl
exaint.o: ../../include/parame.incl
exchani.o: ../../include/parame.incl
exchanj.o: ../../include/parame.incl
feedbk.o: ../../include/parame.incl ../../include/param2.incl
feedbk.o: ../../include/param3.incl ../../include/addr0.incl
feedbk.o: ../../include/point3d.incl ../../include/point2d.incl
feedbk.o: ../../include/nonhyd.incl ../../include/navypb.incl
feedbk.o: ../../include/soil.incl ../../include/nnnhyd.incl
feedbk.o: ../../include/various.incl ../../include/point3dn.incl
feedbk.o: ../../include/point2dn.incl ../../include/variousn.incl
feedbk.o: ../../include/nestl.incl ../../include/varia.incl
feedbk.o: ../../include/pmoist.incl ../../include/pnavyn.incl
feedbk.o: ../../include/soiln.incl ../../include/rpstar.incl
feedbk.o: ../../include/rpstarn.incl ../../include/nhcnst.incl
feedbk.o: ../../include/nncnst.incl
feedbk.o: ../../include/zdiffu.incl
initnest.o: ../../include/defines.incl ../../include/parame.incl
initnest.o: ../../include/pmoist.incl ../../include/param2.incl
initnest.o: ../../include/param3.incl ../../include/addr0.incl
initnest.o: ../../include/varia.incl ../../include/point3d.incl
initnest.o: ../../include/point2d.incl ../../include/navypb.incl
initnest.o: ../../include/various.incl ../../include/fddagd.incl
initnest.o: ../../include/fddaob.incl ../../include/dusolve1.incl
initnest.o: ../../include/nestl.incl ../../include/point3dn.incl
initnest.o: ../../include/point2dn.incl ../../include/variousn.incl
initnest.o: ../../include/nonhyd.incl ../../include/nhcnst.incl
initnest.o: ../../include/nnnhyd.incl ../../include/nncnst.incl
initnest.o: ../../include/pnavyn.incl ../../include/addrcun.incl
initnest.o: ../../include/rpstar.incl ../../include/landuse.incl
initnest.o: ../../include/soil.incl ../../include/soiln.incl 
initnest.o: ../../include/soilp.incl ../../include/paramveg_USGS.incl
initnest.o: ../../include/parfdda.incl ../../include/rpstarn.incl
initnest.o: ../../include/uprad.incl ../../include/soilpn.incl
initnest.o: ../../include/pbltke.incl ../../include/pbltken.incl
ovlchk.o: ../../include/parame.incl ../../include/nestl.incl
ovlchk.o: ../../include/param2.incl
sint.o: ../../include/parame.incl
sintx.o: ../../include/parame.incl
sinty.o: ../../include/parame.incl
stotndi.o: ../../include/parame.incl ../../include/addr0.incl
stotndi.o: ../../include/param2.incl ../../include/param3.incl
stotndi.o: ../../include/varia.incl ../../include/point3d.incl
stotndi.o: ../../include/point2d.incl ../../include/nonhyd.incl
stotndi.o: ../../include/various.incl ../../include/nestl.incl
stotndi.o: ../../include/point3dn.incl ../../include/point2dn.incl
stotndi.o: ../../include/pointbcn.incl ../../include/nnnhyd.incl
stotndi.o: ../../include/nnnhydb.incl ../../include/variousn.incl
stotndi.o: ../../include/pmoist.incl ../../include/pbltke.incl
stotndi.o: ../../include/pbltken.incl
stotndi.o: ../../include/soilp.incl ../../include/soilpn.incl
stotndt.o: ../../include/parame.incl ../../include/addr0.incl
stotndt.o: ../../include/point3d.incl ../../include/point2d.incl
stotndt.o: ../../include/nonhyd.incl ../../include/various.incl
stotndt.o: ../../include/nestl.incl ../../include/point3dn.incl
stotndt.o: ../../include/point2dn.incl ../../include/pointbcn.incl
stotndt.o: ../../include/nnnhyd.incl ../../include/nnnhydb.incl
stotndt.o: ../../include/variousn.incl ../../include/param2.incl
stotndt.o: ../../include/param3.incl ../../include/varia.incl
stotndt.o: ../../include/pmoist.incl
subch.o: ../../include/parame.incl ../../include/nestl.incl
subch.o: ../../include/variousn.incl
