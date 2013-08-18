#	Low level Makefile for the Navy SST program.

include ../../configure.rules
include ../../macros_pregrid

OBJS=   gridinfo.o pregrid_navy.o rd_navy.o \
	output.o sstint.o \
	storage.o

CMD=	pregrid_navy.exe

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
gridinfo.o:	../../macros_pregrid
pregrid_navy.o:	../../macros_pregrid
rd_navy.o:	../../macros_pregrid
output.o:	../../macros_pregrid
sstint.o:	../../macros_pregrid
storage.o:	../../macros_pregrid
