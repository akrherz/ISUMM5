#	Low level Makefile for utility library.

include ../../configure.rules
include ../../macros_pregrid

OBJS=   build_hdate.o geth_newdate.o geth_idts.o swap.o table.o parse_table.o \
	gbytesys.o gribcode.o pregrid_version.o read_pregrid_namelist.o cio.o

LIBTARGET = pgu
TARGETDIR = ./
NCARG_LIBS=	-L$(NCARG_ROOT)/lib \
	-lncarg -lncarg_gks -lncarg_c -lX11 -lm

all :	lib$(LIBTARGET).a

lib$(LIBTARGET).a: $(OBJS)
	$(RM) $(TARGETDIR)lib$(LIBTARGET).a
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS)
	$(RANLIB) $(TARGETDIR)lib$(LIBTARGET).a

gribprint:	gribprint.o libpgu.a
	$(RANLIB) $(LOCAL_LIBRARIES)
	$(FC) -o $(@) $(?:.f=.o) $(LOCAL_LIBRARIES) $(LDFLAGS)
	$(RM) gribprint.o

build_mosaic:	build_mosaic.o
	$(RANLIB) $(LOCAL_LIBRARIES)
	$(FC) -o $(@) $(?:.f=.o) $(LOCAL_LIBRARIES)
	$(RM) build_mosaic.o

plotfmt:	plotfmt.o pregrid_version.o
	$(FC) -o $(@) plotfmt.o pregrid_version.o $(NCARG_LIBS)

# -------------------------------------------------------------------------
# common rules for all Makefiles - do not edit

clean::
	$(RM) $(RM_LIST) lib$(LIBTARGET).a

# -------------------------------------------------------------------------
# DO NOT DELETE THIS LINE -- make depend depends on it.
build_hdate.o: 			../../macros_pregrid 
geth_newdate.o: 		../../macros_pregrid 
geth_idts.o: 			../../macros_pregrid 
swap.o: 			../../macros_pregrid 
table.o: 			../../macros_pregrid 
parse_table.o: 			../../macros_pregrid 
gbytesys.o: 			../../macros_pregrid 
gribcode.o: 			../../macros_pregrid 
pregrid_version.o: 		../../macros_pregrid 
read_pregrid_namelist.o: 	../../macros_pregrid 
cio.o: 				../../macros_pregrid 
