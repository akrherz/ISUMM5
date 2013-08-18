#	Top-level Makefile for INTERPF

#	Macros, these should be generic for all machines

.IGNORE:

AR	=	ar ru
CD	=	cd
LN	=	ln -s
MAKE	=	make -i -f Makefile
RM	=	/bin/rm -f
RM_LIST	=	*.o *.M core *.kmo *.mod

#	Targets for supported architectures

default:
	uname -a > .tmpfile
	@grep crayx1 .tmpfile ; \
	if [ $$? = 0 ] ; then echo "Compiling for Cray X1"                                                      ; \
		echo "AR                =       $(AR)"                          >  macros_interpf       ; \
		echo "RM                =       $(RM)"                          >> macros_interpf       ; \
		echo "RM_LIST           =       $(RM_LIST)"                     >> macros_interpf       ; \
		echo "CD                =       $(CD)"                          >> macros_interpf       ; \
		echo "LN                =       $(LN)"                          >> macros_interpf       ; \
		echo "MAKE              =       $(MAKE)"                        >> macros_interpf       ; \
		echo "SHELL             =       /bin/sh"                        >> macros_interpf       ; \
		echo "TOUCH             =       touch"                          >> macros_interpf       ; \
		echo "CPP               =       cpp"                            >> macros_interpf       ; \
		echo "CPPFLAGS          =       -I. -C -P -DSGI -DBIT32 -UCRAY" >> macros_interpf       ; \
		echo "FC                =       ftn"                            >> macros_interpf       ; \
		echo "FCFLAGS           =       -f free -O2 -ra"                >> macros_interpf       ; \
		echo "LDFLAGS           =       "                               >> macros_interpf       ; \
		echo "CCFLAGS           =       -DSGI -DBIT32 -I."              >> macros_interpf       ; \
		echo "LOCAL_LIBRARIES   =       "                               >> macros_interpf       ; \
		( $(CD) src ; $(MAKE) all )                                                             ; \
	else @grep CRAY .tmpfile ; \
	if [ $$? = 0 ] ; then echo "Compiling for Cray"							; \
		echo "AR		=	$(AR)"				>  macros_interpf	; \
		echo "RM		=	$(RM)"				>> macros_interpf	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_interpf	; \
		echo "CD		=	$(CD)"				>> macros_interpf	; \
		echo "LN		=	$(LN)"				>> macros_interpf	; \
		echo "MAKE		=	$(MAKE)"			>> macros_interpf	; \
		echo "SHELL		=	/bin/sh"			>> macros_interpf	; \
		echo "TOUCH		=	touch"				>> macros_interpf	; \
		echo "CPP		=	cpp"				>> macros_interpf	; \
		echo "CPPFLAGS		=	-I. -C -P -DCRAY"		>> macros_interpf	; \
		echo "FC		=	f90"				>> macros_interpf	; \
		echo "FCFLAGS		=	-f free -x omp"			>> macros_interpf	; \
		echo "LDFLAGS		=	-Ca"				>> macros_interpf	; \
		echo "CCFLAGS		=	-DCRAY -I."			>> macros_interpf	; \
		echo "LOCAL_LIBRARIES	=	"				>> macros_interpf	; \
		( $(CD) src ; $(MAKE) all )								; \
	else grep OSF .tmpfile										; \
	if [ $$? = 0 ] ; then echo "Compiling for Compaq"						; \
		echo "AR		=	$(AR)"				>  macros_interpf	; \
		echo "RM		=	$(RM)"				>> macros_interpf	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_interpf	; \
		echo "CD		=	$(CD)"				>> macros_interpf	; \
		echo "LN		=	$(LN)"				>> macros_interpf	; \
		echo "MAKE		=	$(MAKE)"			>> macros_interpf	; \
		echo "SHELL		=	/bin/sh"			>> macros_interpf	; \
		echo "TOUCH		=	touch"				>> macros_interpf	; \
		echo "CPP		=	/usr/bin/cpp"			>> macros_interpf	; \
		echo "CPPFLAGS		=	-I. -C -P -DDEC"		>> macros_interpf	; \
		echo "FC		=	f90"				>> macros_interpf	; \
		echo "FCFLAGS		=	-automatic -convert big_endian -fast -fpe -free -pipeline -O4 -std -tune ev56 -u" >> macros_interpf	; \
		echo "LDFLAGS		=	-fast -O4"			>> macros_interpf	; \
		echo "CCFLAGS		=	-DDEC -I."			>> macros_interpf	; \
		echo "LOCAL_LIBRARIES	=	"				>> macros_interpf	; \
		( $(CD) src ; $(MAKE) all )							; \
	else grep HP .tmpfile										; \
	if [ $$? = 0 ] ; then echo "Compiling for HP"							; \
		echo "AR		=	$(AR)"				>  macros_interpf	; \
		echo "RM		=	$(RM)"				>> macros_interpf	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_interpf	; \
		echo "CD		=	$(CD)"				>> macros_interpf	; \
		echo "LN		=	$(LN)"				>> macros_interpf	; \
		echo "MAKE		=	$(MAKE)"			>> macros_interpf	; \
		echo "SHELL		=	/bin/sh"			>> macros_interpf	; \
		echo "TOUCH		=	touch"				>> macros_interpf	; \
		echo "CPP		=	/opt/langtools/lbin/cpp"	>> macros_interpf	; \
		echo "CPPFLAGS		=	-I. -C -P -DHP"			>> macros_interpf	; \
		echo "FC		=	f90"				>> macros_interpf	; \
		echo "FCFLAGS		=	+langlvl=90 +source=free"	>> macros_interpf	; \
		echo "LDFLAGS		=	" 				>> macros_interpf	; \
		echo "CCFLAGS		=	-DHP -I."			>> macros_interpf	; \
		echo "LOCAL_LIBRARIES	=	"				>> macros_interpf	; \
		( $(CD) src ; $(MAKE) all )							; \
	else grep AIX .tmpfile										; \
	if [ $$? = 0 ] ; then echo "Compiling for IBM"							; \
		echo "AR		=	$(AR)"				>  macros_interpf	; \
		echo "RM		=	$(RM)"				>> macros_interpf	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_interpf	; \
		echo "CD		=	$(CD)"				>> macros_interpf	; \
		echo "LN		=	$(LN)"				>> macros_interpf	; \
		echo "MAKE		=	$(MAKE)"			>> macros_interpf	; \
		echo "SHELL		=	/bin/sh"			>> macros_interpf	; \
		echo "TOUCH		=	touch"				>> macros_interpf	; \
		echo "CPP		=	/usr/lib/cpp"			>> macros_interpf	; \
		echo "CPPFLAGS		=	-I. -C -P -DIBM"		>> macros_interpf	; \
		echo "FC		=	xlf90"				>> macros_interpf	; \
		echo "FCFLAGS		=	-qfree=f90 -qlanglvl=90pure -qarch=auto -qnosave -qmaxmem=-1 -q64 -qspillsize=20000 -Q"	>> macros_interpf	; \
		echo "LDFLAGS		=	-q64" 				>> macros_interpf	; \
		echo "CCFLAGS		=	-DIBM -I."			>> macros_interpf	; \
		echo "LOCAL_LIBRARIES	=	"				>> macros_interpf	; \
		( $(CD) src ; $(MAKE) all )							; \
	else grep Darwin .tmpfile									; \
	if [ $$? = 0 ] ; then echo "Compiling for MAC"							; \
		echo "AR		=	$(AR)"				>  macros_interpf	; \
		echo "RM		=	$(RM)"				>> macros_interpf	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_interpf	; \
		echo "CD		=	$(CD)"				>> macros_interpf	; \
		echo "LN		=	$(LN)"				>> macros_interpf	; \
		echo "MAKE		=	$(MAKE)"			>> macros_interpf	; \
		echo "SHELL		=	/bin/sh"			>> macros_interpf	; \
		echo "TOUCH		=	touch"				>> macros_interpf	; \
		echo "CPP		=	/usr/bin/cpp"			>> macros_interpf	; \
		echo "CPPFLAGS		=	-I. -C -P -DIBM -xassembler-with-cpp"		>> macros_interpf	; \
		echo "FC		=	xlf90"				>> macros_interpf	; \
		echo "FCFLAGS		=	-qfree=f90 -qarch=auto"		>> macros_interpf	; \
		echo "LDFLAGS		=	-Wl,-stack_size,10000000,-stack_addr,0xc0000000" >> macros_interpf	; \
		echo "CCFLAGS		=	-DNOUNDERSCORE -I."		>> macros_interpf	; \
		echo "LOCAL_LIBRARIES	=	"				>> macros_interpf	; \
		( $(CD) src ; $(MAKE) all )							; \
	else grep IRIX .tmpfile										; \
	if [ $$? = 0 ] ; then echo "Compiling for SGI"							; \
		echo "AR		=	$(AR)"				>  macros_interpf	; \
		echo "RM		=	$(RM)"				>> macros_interpf	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_interpf	; \
		echo "CD		=	$(CD)"				>> macros_interpf	; \
		echo "LN		=	$(LN)"				>> macros_interpf	; \
		echo "MAKE		=	$(MAKE)"			>> macros_interpf	; \
		echo "SHELL		=	/bin/sh"			>> macros_interpf	; \
		echo "TOUCH		=	touch"				>> macros_interpf	; \
		echo "CPP		=	/lib/cpp"			>> macros_interpf	; \
		echo "CPPFLAGS		=	-I. -C -P -DSGI"		>> macros_interpf	; \
		echo "FC		=	f90"				>> macros_interpf	; \
		echo "FCFLAGS		=	-freeform -n32 -O2 -I."		>> macros_interpf	; \
		echo "LDFLAGS		=	-n32 -O2 -lfastm"		>> macros_interpf	; \
		echo "CCFLAGS		=	-DSGI -I. -n32"			>> macros_interpf	; \
		echo "LOCAL_LIBRARIES	=	"				>> macros_interpf	; \
		( $(CD) src ; $(MAKE) all )							; \
	else grep SUN .tmpfile										; \
	if [ $$? = 0 ] ; then echo "Compiling for SUN"							; \
		echo "AR		=	$(AR)"				>  macros_interpf	; \
		echo "RM		=	$(RM)"				>> macros_interpf	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_interpf	; \
		echo "CD		=	$(CD)"				>> macros_interpf	; \
		echo "LN		=	$(LN)"				>> macros_interpf	; \
		echo "MAKE		=	$(MAKE)"			>> macros_interpf	; \
		echo "SHELL		=	/bin/sh"			>> macros_interpf	; \
		echo "TOUCH		=	touch"				>> macros_interpf	; \
		echo "CPP		=	/usr/ccs/lib/cpp"		>> macros_interpf	; \
		echo "CPPFLAGS		=	-I. -C -P -DSUN"		>> macros_interpf	; \
		echo "FC		=	f90"				>> macros_interpf	; \
		echo "FCFLAGS		=	-ansi -free"			>> macros_interpf	; \
		echo "LDFLAGS		=	" 				>> macros_interpf	; \
		echo "CCFLAGS		=	-DSUN -I."			>> macros_interpf	; \
		echo "LOCAL_LIBRARIES	=	"				>> macros_interpf	; \
		( $(CD) src ; $(MAKE) all	) 							; \
	else grep Linux .tmpfile									; \
	if [ $$? = 0 ] ; then echo "Compiling for Linux"						; \
		echo "AR		=	$(AR)"				>  macros_interpf	; \
		echo "RM		=	$(RM)"				>> macros_interpf	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_interpf	; \
		echo "CD		=	$(CD)"				>> macros_interpf	; \
		echo "LN		=	$(LN)"				>> macros_interpf	; \
		echo "MAKE		=	$(MAKE)"			>> macros_interpf	; \
		echo "SHELL		=	/bin/sh"			>> macros_interpf	; \
		echo "TOUCH		=	touch"				>> macros_interpf	; \
		echo "CPP		=	/lib/cpp"			>> macros_interpf	; \
		echo "CPPFLAGS		=	-I. -C -P -DDEC -traditional"	>> macros_interpf	; \
		echo "FC		=	pgf90"				>> macros_interpf	; \
		echo "FCFLAGS		=	-Mfreeform -pc 32 -byteswapio"	>> macros_interpf	; \
		echo "LDFLAGS		=	" 				>> macros_interpf	; \
		echo "CCFLAGS		=	-DDEC -I."			>> macros_interpf	; \
		echo "LOCAL_LIBRARIES	=	"				>> macros_interpf	; \
		( $(CD) src ; $(MAKE) all	) 							; \
	else grep UXP .tmpfile										; \
	if [ $$? = 0 ] ; then echo "Compiling for Fujitsu"						; \
		echo "AR		=	$(AR)"				>  macros_interpf	; \
		echo "RM		=	$(RM)"				>> macros_interpf	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_interpf	; \
		echo "CD		=	$(CD)"				>> macros_interpf	; \
		echo "LN		=	$(LN)"				>> macros_interpf	; \
		echo "MAKE		=	$(MAKE)"			>> macros_interpf	; \
		echo "SHELL		=	/bin/sh"			>> macros_interpf	; \
		echo "TOUCH		=	touch"				>> macros_interpf	; \
		echo "CPP		=	/lib/cpp"			>> macros_interpf	; \
		echo "CPPFLAGS		=	-I. -C -P -DVPP -DBIT32"	>> macros_interpf	; \
		echo "FC		=	frt"				>> macros_interpf	; \
		echo "FCFLAGS		=	-Free -X9 -Am -sc -Kfast -Kfreealloc -Karraystack3"	>> macros_interpf	; \
		echo "LDFLAGS		=	-J" 				>> macros_interpf	; \
		echo "CCFLAGS		=	-DVPP -DBIT32 -I."		>> macros_interpf	; \
		echo "LOCAL_LIBRARIES	=	"				>> macros_interpf	; \
		( $(CD) src ; $(MAKE) all	) 							; \
	else echo "Do not know how to compile for the `cat .tmpfile` machine." 				; \
	fi ; \
	fi ; \
	fi ; \
	fi ; \
	fi ; \
	fi ; \
	fi ; \
	fi ; \
	fi ; \
	fi
	( $(RM) interpf ; $(LN) src/interpf . )

intel:
	echo "Compiling for Linux with INTEL compiler"						; \
		echo "AR		=	$(AR)"				>  macros_interpf	; \
		echo "RM		=	$(RM)"				>> macros_interpf	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_interpf	; \
		echo "CD		=	$(CD)"				>> macros_interpf	; \
		echo "LN		=	$(LN)"				>> macros_interpf	; \
		echo "MAKE		=	$(MAKE)"			>> macros_interpf	; \
		echo "SHELL		=	/bin/sh"			>> macros_interpf	; \
		echo "TOUCH		=	touch"				>> macros_interpf	; \
		echo "CPP		=	/lib/cpp"			>> macros_interpf	; \
		echo "CPPFLAGS		=	-I. -C -P -DDEC -traditional"	>> macros_interpf	; \
		echo "FC		=	ifort"				>> macros_interpf	; \
		echo "FCFLAGS		=	-FR -pc 32 -convert big_endian"	>> macros_interpf	; \
		echo "LDFLAGS		=	" 				>> macros_interpf	; \
		echo "CCFLAGS		=	-DDEC -I."			>> macros_interpf	; \
		echo "LOCAL_LIBRARIES	=	"				>> macros_interpf	; \
		( $(CD) src ; $(MAKE) all	) 							; \
	( $(RM) interpf ; $(LN) src/interpf . )

clean:
	( $(CD) src ; $(MAKE) clean "CD = $(CD)" "RM = $(RM)" "RM_LIST = $(RM_LIST)" )
	$(RM) $(RM_LIST) .tmpfile
