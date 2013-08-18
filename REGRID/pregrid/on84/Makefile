#	Low level Makefile for the Office Note 84 format program.

include ../../configure.rules
include ../../macros_pregrid

OBJS=   gridinfo.o new_storage.o pregrid_on84.o rd_on84.o \
	ron84_record.o output.o sstint.o snowprop.o \
	unpack_on84.o tv_to_t.o datint.o

CMD=	pregrid_on84.exe

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
rd_on84.o:	con84.incl ../../macros_pregrid
gridinfo.o: 	../../macros_pregrid
new_storage.o:	../../macros_pregrid
pregrid_on84.o:	../../macros_pregrid
ron84_record.o:	../../macros_pregrid
output.o:	../../macros_pregrid
sstint.o:	../../macros_pregrid
snowprop.o:	../../macros_pregrid
unpack_on84.o:	../../macros_pregrid
tv_to_t.o:	../../macros_pregrid
datint.o:	../../macros_pregrid
