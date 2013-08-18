#	Mid level Makefile for the regridder program.

include ../macros_regridder

.IGNORE:

all:
		( cd src ; $(MAKE) all )
		( $(RM) regridder ; $(LN) src/regridder . )

clean :
		( $(CD) src ; $(MAKE) clean "RM = $(RM)" "RM_LIST = $(RM_LIST) regridder" ) 
		$(RM) $(RM_LIST) regridder
