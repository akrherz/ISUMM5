#	Low level Makefile for the archived NCEP data program.

include ../../configure.rules
include ../../macros_pregrid

OBJS=   gridinfo.o pregrid_ng.o storage.o output.o \
        sstint.o  datint.o rd_grib.o snowprop.o

CMD=	pregrid_ncep.exe

all :	$(CMD)

$(CMD):: $(OBJS) ../util/libpgu.a
	$(RM) $@
	$(RANLIB) $(LOCAL_LIBRARIES)
	$(FC) -o $@ $(OBJS) $(LDFLAGS) $(LOCAL_LIBRARIES)

# -------------------------------------------------------------------------
# common rules for all Makefiles - do not edit

clean::
	$(RM) $(RM_LIST) $(CMD)

# -------------------------------------------------------------------------
# DO NOT DELETE THIS LINE -- make depend depends on it.

datint.o:	storage.o ../../macros_pregrid
output.o:	storage.o ../../macros_pregrid
pregrid_ng.o:	storage.o ../../macros_pregrid
rd_grib.o:	storage.o ../../macros_pregrid
gridinfo.o:	../../macros_pregrid
storage.o:	../../macros_pregrid
sstint.o:	../../macros_pregrid
