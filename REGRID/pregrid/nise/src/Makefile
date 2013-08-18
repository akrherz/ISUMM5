#	Low level Makefile for seaicefr program.

include ../../../configure.rules
include ../../../macros_pregrid

.IGNORE:

MODULES	=	module_output_data.o module_namelist_stuff.o module_nsidc.o module_support.o 

OBJS = 		seaicefr.o

all :		seaicefr \
		$(OBJS)  $(MODULES) \
		../../../macros_pregrid ./Makefile ../Makefile

seaicefr:	$(OBJS) $(MODULES) \
		../../../configure.rules ./Makefile ../Makefile
		$(RANLIB) ../$(LOCAL_LIBRARIES)
		$(FC) -o $@ $(LDFLAGS) $(OBJS) $(MODULES)  ../$(LOCAL_LIBRARIES)

seaicefr.o:	module_support.o $(MODULES) \
		../../../configure.rules ./Makefile ../Makefile

module_output_data.o:  module_nsidc.o \
		../../../configure.rules ./Makefile ../Makefile

module_namelist_stuff.o:  ../../../configure.rules ./Makefile ../Makefile

module_nsidc.o:  module_support.o \
		../../../configure.rules ./Makefile ../Makefile

module_support.o:  ../../../configure.rules ./Makefile ../Makefile

clean:
		$(RM) $(RM_LIST) seaicefr
