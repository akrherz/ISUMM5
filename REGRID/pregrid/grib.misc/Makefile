#	Low level Makefile for miscellaneous GRIB files.

include ../../configure.rules
include ../../macros_pregrid

OBJS=   gridinfo.o new_storage.o filelist.o pregrid_grib.o output.o rrpr.o \
        rd_grib.o file_delete.o datint.o

CMD=	pregrid_grib.exe

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

pregrid_grib.o:	gridinfo.o ../../macros_pregrid
output.o:	gridinfo.o ../../macros_pregrid
rd_grib.o:	gridinfo.o ../../macros_pregrid
rrpr.o:		gridinfo.o ../../macros_pregrid
gridinfo.o:	../../macros_pregrid
new_storage.o:	../../macros_pregrid
filelist.o:	../../macros_pregrid
file_delete.o:	../../macros_pregrid
datint.o:	../../macros_pregrid
