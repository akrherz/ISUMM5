#	Mid-level Makefile for SEAICEFR

include ../../macros_pregrid

.IGNORE:

all:
	( cd src ; $(MAKE) all )
	( $(RM) seaicefr ; $(LN) src/seaicefr . )

clean :
	( $(CD) src ; $(MAKE) clean "RM = $(RM)" "RM_LIST = $(RM_LIST) seaicefr" )
	$(RM) $(RM_LIST) seaicefr

