#	Top-level Makefile for REGRID

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
	grep crayx1 .tmpfile ; \
	if [ $$? = 0 ] ; then echo "Compiling for Cray X1"						; \
		echo "AR		=	$(AR)"				>  macros_pregrid	; \
		echo "RM		=	$(RM)"				>> macros_pregrid	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_pregrid	; \
		echo "CD		=	$(CD)"				>> macros_pregrid	; \
		echo "LN		=	$(LN)"				>> macros_pregrid	; \
		echo "MAKE		=	$(MAKE)"			>> macros_pregrid	; \
		echo "SHELL		=	/bin/sh"			>> macros_pregrid	; \
		echo "TOUCH		=	touch"				>> macros_pregrid	; \
		echo "CPP		=	cpp"				>> macros_pregrid	; \
		echo "CPPFLAGS		=	-I. -C -P -DSGI -DBIT32 -UCRAY -Dcrayx1"	>> macros_pregrid	; \
		echo "FC		=	ftn"				>> macros_pregrid	; \
		echo "FCFLAGS		=	-f free -p ../util -p ."	>> macros_pregrid	; \
		echo "LDFLAGS		=	"				>> macros_pregrid	; \
		echo "CCFLAGS		=	-DSGI -DBIT32 -I."		>> macros_pregrid	; \
		echo "LOCAL_LIBRARIES	=	../util/libpgu.a"		>> macros_pregrid	; \
		echo "RANLIB 		=	echo"				>> macros_pregrid	; \
		( $(CD) pregrid ; $(MAKE) all )								; \
		echo "AR		=	$(AR)"				>  macros_regridder	; \
		echo "RM		=	$(RM)"				>> macros_regridder	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_regridder	; \
		echo "CD		=	$(CD)"				>> macros_regridder	; \
		echo "LN		=	$(LN)"				>> macros_regridder	; \
		echo "MAKE		=	$(MAKE)"			>> macros_regridder	; \
		echo "SHELL		=	/bin/sh"			>> macros_regridder	; \
		echo "TOUCH		=	touch"				>> macros_regridder	; \
		echo "CPP		=	cpp"				>> macros_regridder	; \
		echo "CPPFLAGS		=	-I. -C -P -DSGI -DBIT32 -UCRAY -Dcrayx1"	>> macros_regridder	; \
		echo "FC		=	ftn"				>> macros_regridder	; \
		echo "FCFLAGS		=	-f free -Ossp"			>> macros_regridder	; \
		echo "LDFLAGS		=	-Ossp"				>> macros_regridder	; \
		echo "CCFLAGS		=	-DSGI -DBIT32 -I."		>> macros_regridder	; \
		echo "LOCAL_LIBRARIES	=	"				>> macros_regridder	; \
		( $(CD) regridder ; $(MAKE) all )							; \
	else grep CRAY .tmpfile ; \
	if [ $$? = 0 ] ; then echo "Compiling for Cray"							; \
		echo "AR		=	$(AR)"				>  macros_pregrid	; \
		echo "RM		=	$(RM)"				>> macros_pregrid	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_pregrid	; \
		echo "CD		=	$(CD)"				>> macros_pregrid	; \
		echo "LN		=	$(LN)"				>> macros_pregrid	; \
		echo "MAKE		=	$(MAKE)"			>> macros_pregrid	; \
		echo "SHELL		=	/bin/sh"			>> macros_pregrid	; \
		echo "TOUCH		=	touch"				>> macros_pregrid	; \
		echo "CPP		=	cpp"				>> macros_pregrid	; \
		echo "CPPFLAGS		=	-I. -C -P -DCRAY -DBIT64"	>> macros_pregrid	; \
		echo "FC		=	f90"				>> macros_pregrid	; \
		echo "FCFLAGS		=	-f free -x omp -p ../util"	>> macros_pregrid	; \
		echo "LDFLAGS		=	-Ca"				>> macros_pregrid	; \
		echo "CCFLAGS		=	-DCRAY -DBIT64 -I."		>> macros_pregrid	; \
		echo "LOCAL_LIBRARIES	=	../util/libpgu.a"		>> macros_pregrid	; \
		echo "RANLIB 		=	echo"				>> macros_pregrid	; \
		( $(CD) pregrid ; $(MAKE) all )								; \
		echo "AR		=	$(AR)"				>  macros_regridder	; \
		echo "RM		=	$(RM)"				>> macros_regridder	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_regridder	; \
		echo "CD		=	$(CD)"				>> macros_regridder	; \
		echo "LN		=	$(LN)"				>> macros_regridder	; \
		echo "MAKE		=	$(MAKE)"			>> macros_regridder	; \
		echo "SHELL		=	/bin/sh"			>> macros_regridder	; \
		echo "TOUCH		=	touch"				>> macros_regridder	; \
		echo "CPP		=	cpp"				>> macros_regridder	; \
		echo "CPPFLAGS		=	-I. -C -P -DCRAY -DBIT64"	>> macros_regridder	; \
		echo "FC		=	f90"				>> macros_regridder	; \
		echo "FCFLAGS		=	-f free -x omp"			>> macros_regridder	; \
		echo "LDFLAGS		=	-Ca"				>> macros_regridder	; \
		echo "CCFLAGS		=	-DCRAY -DBIT64 -I."		>> macros_regridder	; \
		echo "LOCAL_LIBRARIES	=	"				>> macros_regridder	; \
		( $(CD) regridder ; $(MAKE) all )							; \
	else grep OSF .tmpfile										; \
	if [ $$? = 0 ] ; then echo "Compiling for Compaq"						; \
		echo "AR		=	$(AR)"				>  macros_pregrid	; \
		echo "RM		=	$(RM)"				>> macros_pregrid	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_pregrid	; \
		echo "CD		=	$(CD)"				>> macros_pregrid	; \
		echo "LN		=	$(LN)"				>> macros_pregrid	; \
		echo "MAKE		=	$(MAKE)"			>> macros_pregrid	; \
		echo "SHELL		=	/bin/sh"			>> macros_pregrid	; \
		echo "TOUCH		=	touch"				>> macros_pregrid	; \
		echo "CPP		=	/usr/bin/cpp"			>> macros_pregrid	; \
		echo "CPPFLAGS		=	-I. -C -P -DDEC -DBIT -DBIT32"	>> macros_pregrid	; \
		echo "FC		=	f90"				>> macros_pregrid	; \
		echo "FCFLAGS		=	-automatic -convert big_endian -fast -assume accuracy_sensitive -fpe -free -I../util " >> macros_pregrid	; \
		echo "LDFLAGS		=	-fast"				>> macros_pregrid	; \
		echo "CCFLAGS		=	-DDEC -DBIT -DBIT32 -I."		>> macros_pregrid	; \
		echo "LOCAL_LIBRARIES	=	../util/libpgu.a"		>> macros_pregrid	; \
		echo "RANLIB 		=	echo"				>> macros_pregrid	; \
		( $(CD) pregrid ; $(MAKE) all )								; \
		echo "AR		=	$(AR)"				>  macros_regridder	; \
		echo "RM		=	$(RM)"				>> macros_regridder	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_regridder	; \
		echo "CD		=	$(CD)"				>> macros_regridder	; \
		echo "LN		=	$(LN)"				>> macros_regridder	; \
		echo "MAKE		=	$(MAKE)"			>> macros_regridder	; \
		echo "SHELL		=	/bin/sh"			>> macros_regridder	; \
		echo "TOUCH		=	touch"				>> macros_regridder	; \
		echo "CPP		=	/usr/bin/cpp"			>> macros_regridder	; \
		echo "CPPFLAGS		=	-I. -C -P -DDEC -DBIT32"	>> macros_regridder	; \
		echo "FC		=	f90"				>> macros_regridder	; \
		echo "FCFLAGS		=	-automatic -convert big_endian -fast -fpe -free" >> macros_regridder	; \
		echo "LDFLAGS		=	-fast"				>> macros_regridder	; \
		echo "CCFLAGS		=	-DDEC -DBIT32 -I."		>> macros_regridder	; \
		echo "LOCAL_LIBRARIES	=	"				>> macros_regridder	; \
		( $(CD) regridder ; $(MAKE) all )							; \
	else grep HP .tmpfile										; \
	if [ $$? = 0 ] ; then echo "Compiling for HP"							; \
		echo "AR		=	$(AR)"				>  macros_pregrid	; \
		echo "RM		=	$(RM)"				>> macros_pregrid	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_pregrid	; \
		echo "CD		=	$(CD)"				>> macros_pregrid	; \
		echo "LN		=	$(LN)"				>> macros_pregrid	; \
		echo "MAKE		=	$(MAKE)"			>> macros_pregrid	; \
		echo "SHELL		=	/bin/sh"			>> macros_pregrid	; \
		echo "TOUCH		=	touch"				>> macros_pregrid	; \
		echo "CPP		=	/opt/langtools/lbin/cpp"	>> macros_pregrid	; \
		echo "CPPFLAGS		=	-I. -C -P -DHP -DBIT32"		>> macros_pregrid	; \
		echo "FC		=	f90"				>> macros_pregrid	; \
		echo "FCFLAGS		=	+langlvl=90 +source=free -I../util">> macros_pregrid	; \
		echo "LDFLAGS		=	" 				>> macros_pregrid	; \
		echo "CCFLAGS		=	-DHP -DBIT32 -I."		>> macros_pregrid	; \
		echo "LOCAL_LIBRARIES	=	../util/libpgu.a"		>> macros_pregrid	; \
		echo "RANLIB 		=	echo"				>> macros_pregrid	; \
		( $(CD) pregrid ; $(MAKE) all )								; \
		echo "AR		=	$(AR)"				>  macros_regridder	; \
		echo "RM		=	$(RM)"				>> macros_regridder	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_regridder	; \
		echo "CD		=	$(CD)"				>> macros_regridder	; \
		echo "LN		=	$(LN)"				>> macros_regridder	; \
		echo "MAKE		=	$(MAKE)"			>> macros_regridder	; \
		echo "SHELL		=	/bin/sh"			>> macros_regridder	; \
		echo "TOUCH		=	touch"				>> macros_regridder	; \
		echo "CPP		=	/opt/langtools/lbin/cpp"	>> macros_regridder	; \
		echo "CPPFLAGS		=	-I. -C -P -DHP -DBIT32"		>> macros_regridder	; \
		echo "FC		=	f90"				>> macros_regridder	; \
		echo "FCFLAGS		=	+langlvl=90 +source=free"	>> macros_regridder	; \
		echo "LDFLAGS		=	" 				>> macros_regridder	; \
		echo "CCFLAGS		=	-DHP -DBIT32 -I."		>> macros_regridder	; \
		echo "LOCAL_LIBRARIES	=	"				>> macros_regridder	; \
		( $(CD) regridder ; $(MAKE) all )							; \
	else grep AIX .tmpfile										; \
	if [ $$? = 0 ] ; then echo "Compiling for IBM"							; \
		echo "AR		=	$(AR)"				>  macros_pregrid	; \
		echo "RM		=	$(RM)"				>> macros_pregrid	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_pregrid	; \
		echo "CD		=	$(CD)"				>> macros_pregrid	; \
		echo "LN		=	$(LN)"				>> macros_pregrid	; \
		echo "MAKE		=	$(MAKE)"			>> macros_pregrid	; \
		echo "SHELL		=	/bin/sh"			>> macros_pregrid	; \
		echo "TOUCH		=	touch"				>> macros_pregrid	; \
		echo "CPP		=	/usr/lib/cpp"			>> macros_pregrid	; \
		echo "CPPFLAGS		=	-I. -C -P -DIBM -DBIT32"	>> macros_pregrid	; \
		echo "FC		=	xlf90"				>> macros_pregrid	; \
		echo "FCFLAGS		=	-qfree=f90 -qlanglvl=90pure -I../util"	>> macros_pregrid	; \
		echo "LDFLAGS		=	" 				>> macros_pregrid	; \
		echo "CCFLAGS		=	-DIBM -DBIT32 -I."		>> macros_pregrid	; \
		echo "LOCAL_LIBRARIES	=	../util/libpgu.a"		>> macros_pregrid	; \
		echo "RANLIB 		=	echo"				>> macros_pregrid	; \
		( $(CD) pregrid ; $(MAKE) all )								; \
		echo "AR		=	$(AR)"				>  macros_regridder	; \
		echo "RM		=	$(RM)"				>> macros_regridder	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_regridder	; \
		echo "CD		=	$(CD)"				>> macros_regridder	; \
		echo "LN		=	$(LN)"				>> macros_regridder	; \
		echo "MAKE		=	$(MAKE)"			>> macros_regridder	; \
		echo "SHELL		=	/bin/sh"			>> macros_regridder	; \
		echo "TOUCH		=	touch"				>> macros_regridder	; \
		echo "CPP		=	/usr/lib/cpp"			>> macros_regridder	; \
		echo "CPPFLAGS		=	-I. -C -P -DIBM -DBIT32"	>> macros_regridder	; \
		echo "FC		=	xlf90"				>> macros_regridder	; \
		echo "FCFLAGS		=	-qfree=f90 -qlanglvl=90pure -u"	>> macros_regridder	; \
		echo "LDFLAGS		=	" 				>> macros_regridder	; \
		echo "CCFLAGS		=	-DIBM -DBIT32 -I."		>> macros_regridder	; \
		echo "LOCAL_LIBRARIES	=	"				>> macros_regridder	; \
		( $(CD) regridder ; $(MAKE) all )							; \
	else grep Darwin .tmpfile										; \
	if [ $$? = 0 ] ; then echo "Compiling for MAC"							; \
		echo "AR		=	$(AR)"				>  macros_pregrid	; \
		echo "RM		=	$(RM)"				>> macros_pregrid	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_pregrid	; \
		echo "CD		=	$(CD)"				>> macros_pregrid	; \
		echo "LN		=	$(LN)"				>> macros_pregrid	; \
		echo "MAKE		=	$(MAKE)"			>> macros_pregrid	; \
		echo "SHELL		=	/bin/sh"			>> macros_pregrid	; \
		echo "TOUCH		=	touch"				>> macros_pregrid	; \
		echo "CPP		=	/usr/bin/cpp"			>> macros_pregrid	; \
		echo "CPPFLAGS		=	-I. -C -P -DBIT32 -xassembler-with-cpp"	>> macros_pregrid	; \
		echo "FC		=	xlf90"				>> macros_pregrid	; \
		echo "FCFLAGS		=	-qfree=f90 -qarch=auto -I../util" >> macros_pregrid	; \
		echo "LDFLAGS		=	-Wl,-stack_size,10000000,-stack_addr,0xc0000000" >> macros_pregrid	; \
		echo "CCFLAGS		=	-DBIT32 -I."		>> macros_pregrid	; \
		echo "LOCAL_LIBRARIES	=	../util/libpgu.a"		>> macros_pregrid	; \
		echo "RANLIB 		=	ranlib"				>> macros_pregrid	; \
		( $(CD) pregrid ; $(MAKE) all )								; \
		echo "AR		=	$(AR)"				>  macros_regridder	; \
		echo "RM		=	$(RM)"				>> macros_regridder	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_regridder	; \
		echo "CD		=	$(CD)"				>> macros_regridder	; \
		echo "LN		=	$(LN)"				>> macros_regridder	; \
		echo "MAKE		=	$(MAKE)"			>> macros_regridder	; \
		echo "SHELL		=	/bin/sh"			>> macros_regridder	; \
		echo "TOUCH		=	touch"				>> macros_regridder	; \
		echo "CPP		=	/usr/bin/cpp"			>> macros_regridder	; \
		echo "CPPFLAGS		=	-I. -C -P -DIBM -DBIT32 -xassembler-with-cpp"	>> macros_regridder	; \
		echo "FC		=	xlf90"				>> macros_regridder	; \
		echo "FCFLAGS		=	-qfree=f90 -qarch=auto"	>> macros_regridder	; \
		echo "LDFLAGS		=	-Wl,-stack_size,10000000,-stack_addr,0xc0000000" >> macros_regridder	; \
		echo "CCFLAGS		=	-DIBM -DBIT32 -DNOUNDERSCORE -I." >> macros_regridder	; \
		echo "LOCAL_LIBRARIES	=	"				>> macros_regridder	; \
		( $(CD) regridder ; $(MAKE) all )							; \
	else grep IRIX .tmpfile										; \
	if [ $$? = 0 ] ; then echo "Compiling for SGI"							; \
		echo "AR		=	$(AR)"				>  macros_pregrid	; \
		echo "RM		=	$(RM)"				>> macros_pregrid	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_pregrid	; \
		echo "CD		=	$(CD)"				>> macros_pregrid	; \
		echo "LN		=	$(LN)"				>> macros_pregrid	; \
		echo "MAKE		=	$(MAKE)"			>> macros_pregrid	; \
		echo "SHELL		=	/bin/sh"			>> macros_pregrid	; \
		echo "TOUCH		=	touch"				>> macros_pregrid	; \
		echo "CPP		=	/lib/cpp"			>> macros_pregrid	; \
		echo "CPPFLAGS		=	-I. -C -P -DSGI -DBIT32"	>> macros_pregrid	; \
		echo "FC		=	f90"				>> macros_pregrid	; \
		echo "FCFLAGS		=	-freeform -n32 -I../util"	>> macros_pregrid	; \
		echo "LDFLAGS		=	-n32" 				>> macros_pregrid	; \
		echo "CCFLAGS		=	-DSGI -DBIT32 -I. -n32"		>> macros_pregrid	; \
		echo "LOCAL_LIBRARIES	=	../util/libpgu.a"		>> macros_pregrid	; \
		echo "RANLIB 		=	echo"				>> macros_pregrid	; \
		( $(CD) pregrid ; $(MAKE) all )								; \
		echo "AR		=	$(AR)"				>  macros_regridder	; \
		echo "RM		=	$(RM)"				>> macros_regridder	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_regridder	; \
		echo "CD		=	$(CD)"				>> macros_regridder	; \
		echo "LN		=	$(LN)"				>> macros_regridder	; \
		echo "MAKE		=	$(MAKE)"			>> macros_regridder	; \
		echo "SHELL		=	/bin/sh"			>> macros_regridder	; \
		echo "TOUCH		=	touch"				>> macros_regridder	; \
		echo "CPP		=	/lib/cpp"			>> macros_regridder	; \
		echo "CPPFLAGS		=	-I. -C -P -DSGI -DBIT32"	>> macros_regridder	; \
		echo "FC		=	f90"				>> macros_regridder	; \
		echo "FCFLAGS		=	-freeform -n32 -I."		>> macros_regridder	; \
		echo "LDFLAGS		=	-n32" 				>> macros_regridder	; \
		echo "CCFLAGS		=	-DSGI -DBIT32 -I. -n32"		>> macros_regridder	; \
		echo "LOCAL_LIBRARIES	=	"				>> macros_regridder	; \
		( $(CD) regridder ; $(MAKE) all )							; \
	else grep SUN .tmpfile										; \
	if [ $$? = 0 ] ; then echo "Compiling for SUN"							; \
		echo "AR		=	$(AR)"				>  macros_pregrid	; \
		echo "RM		=	$(RM)"				>> macros_pregrid	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_pregrid	; \
		echo "CD		=	$(CD)"				>> macros_pregrid	; \
		echo "LN		=	$(LN)"				>> macros_pregrid	; \
		echo "MAKE		=	$(MAKE)"			>> macros_pregrid	; \
		echo "SHELL		=	/bin/sh"			>> macros_pregrid	; \
		echo "TOUCH		=	touch"				>> macros_pregrid	; \
		echo "CPP		=	/usr/ccs/lib/cpp"		>> macros_pregrid	; \
		echo "CPPFLAGS		=	-I. -C -P -DSUN -DBIT32"	>> macros_pregrid	; \
		echo "FC		=	f90"				>> macros_pregrid	; \
		echo "FCFLAGS		=	-ansi -free -stackvar -M../util" >> macros_pregrid	; \
		echo "LDFLAGS		=	" 				>> macros_pregrid	; \
		echo "CCFLAGS		=	-DSUN -DBIT32 -I."		>> macros_pregrid	; \
		echo "LOCAL_LIBRARIES	=	../util/libpgu.a"		>> macros_pregrid	; \
		echo "RANLIB 		=	echo"				>> macros_pregrid	; \
		( $(CD) pregrid ; $(MAKE) all )								; \
		echo "AR		=	$(AR)"				>  macros_regridder	; \
		echo "RM		=	$(RM)"				>> macros_regridder	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_regridder	; \
		echo "CD		=	$(CD)"				>> macros_regridder	; \
		echo "LN		=	$(LN)"				>> macros_regridder	; \
		echo "MAKE		=	$(MAKE)"			>> macros_regridder	; \
		echo "SHELL		=	/bin/sh"			>> macros_regridder	; \
		echo "TOUCH		=	touch"				>> macros_regridder	; \
		echo "CPP		=	/usr/ccs/lib/cpp"		>> macros_regridder	; \
		echo "CPPFLAGS		=	-I. -C -P -DSUN -DBIT32"	>> macros_regridder	; \
		echo "FC		=	f90"				>> macros_regridder	; \
		echo "FCFLAGS		=	-ansi -free -stackvar"		>> macros_regridder	; \
		echo "LDFLAGS		=	" 				>> macros_regridder	; \
		echo "CCFLAGS		=	-DSUN -DBIT32 -I."		>> macros_regridder	; \
		echo "LOCAL_LIBRARIES	=	"				>> macros_regridder	; \
		( $(CD) regridder ; $(MAKE) all	) 							; \
	else grep Linux .tmpfile									; \
	if [ $$? = 0 ] ; then echo "Compiling for Linux"						; \
		echo "AR		=	$(AR)"				>  macros_pregrid	; \
		echo "RM		=	$(RM)"				>> macros_pregrid	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_pregrid	; \
		echo "CD		=	$(CD)"				>> macros_pregrid	; \
		echo "LN		=	$(LN)"				>> macros_pregrid	; \
		echo "MAKE		=	$(MAKE)"			>> macros_pregrid	; \
		echo "SHELL		=	/bin/sh"			>> macros_pregrid	; \
		echo "TOUCH		=	touch"				>> macros_pregrid	; \
		echo "CPP		=	/lib/cpp"			>> macros_pregrid	; \
		echo "CPPFLAGS		=	-I. -C -P -DDEC -DBIT32 -traditional"	>> macros_pregrid	; \
		echo "FC		=	pgf90"				>> macros_pregrid	; \
		echo "FCFLAGS		=	-Mfreeform -byteswapio -I../util" >> macros_pregrid	; \
		echo "LDFLAGS		=	" 				>> macros_pregrid	; \
		echo "CCFLAGS		=	-DDEC -DBIT32 -I."		>> macros_pregrid	; \
		echo "LOCAL_LIBRARIES	=	../util/libpgu.a"		>> macros_pregrid	; \
		echo "RANLIB 		=	echo"				>> macros_pregrid	; \
		( $(CD) pregrid ; $(MAKE) all )								; \
		echo "AR		=	$(AR)"				>  macros_regridder	; \
		echo "RM		=	$(RM)"				>> macros_regridder	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_regridder	; \
		echo "CD		=	$(CD)"				>> macros_regridder	; \
		echo "LN		=	$(LN)"				>> macros_regridder	; \
		echo "MAKE		=	$(MAKE)"			>> macros_regridder	; \
		echo "SHELL		=	/bin/sh"			>> macros_regridder	; \
		echo "TOUCH		=	touch"				>> macros_regridder	; \
		echo "CPP		=	/lib/cpp"			>> macros_regridder	; \
		echo "CPPFLAGS		=	-I. -C -P -DDEC -DBIT32 -traditional"	>> macros_regridder	; \
		echo "FC		=	pgf90"				>> macros_regridder	; \
		echo "FCFLAGS		=	-Mfreeform -pc 32 -byteswapio"	>> macros_regridder	; \
		echo "LDFLAGS		=	" 				>> macros_regridder	; \
		echo "CCFLAGS		=	-DDEC -DBIT32 -I."		>> macros_regridder	; \
		echo "LOCAL_LIBRARIES	=	"				>> macros_regridder	; \
		( $(CD) regridder ; $(MAKE) all	) 							; \
	else grep UXP .tmpfile										; \
	if [ $$? = 0 ] ; then echo "Compiling for Fujitsu"						; \
		echo "AR		=	$(AR)"				>  macros_pregrid	; \
		echo "RM		=	$(RM)"				>> macros_pregrid	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_pregrid	; \
		echo "CD		=	$(CD)"				>> macros_pregrid	; \
		echo "LN		=	$(LN)"				>> macros_pregrid	; \
		echo "MAKE		=	$(MAKE)"			>> macros_pregrid	; \
		echo "SHELL		=	/bin/sh"			>> macros_pregrid	; \
		echo "TOUCH		=	touch"				>> macros_pregrid	; \
		echo "CPP		=	/lib/cpp"			>> macros_pregrid	; \
		echo "CPPFLAGS		=	-I. -C -P -DVPP -DBIT32"	>> macros_pregrid	; \
		echo "FC		=	frt"				>> macros_pregrid	; \
		echo "FCFLAGS		=	-Free -X9 -Am -sc -Kfreealloc -Karraystack3 -I../util"	>> macros_pregrid	; \
		echo "LDFLAGS		=	-J" 				>> macros_pregrid	; \
		echo "CCFLAGS		=	-DVPP -DBIT32 -I."		>> macros_pregrid	; \
		echo "LOCAL_LIBRARIES	=	../util/libpgu.a"		>> macros_pregrid	; \
		echo "RANLIB 		=	echo"				>> macros_pregrid	; \
		( $(CD) pregrid ; $(MAKE) all )								; \
		echo "AR		=	$(AR)"				>  macros_regridder	; \
		echo "RM		=	$(RM)"				>> macros_regridder	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_regridder	; \
		echo "CD		=	$(CD)"				>> macros_regridder	; \
		echo "LN		=	$(LN)"				>> macros_regridder	; \
		echo "MAKE		=	$(MAKE)"			>> macros_regridder	; \
		echo "SHELL		=	/bin/sh"			>> macros_regridder	; \
		echo "TOUCH		=	touch"				>> macros_regridder	; \
		echo "CPP		=	/lib/cpp"			>> macros_regridder	; \
		echo "CPPFLAGS		=	-I. -C -P -DVPP -DBIT32"	>> macros_regridder	; \
		echo "FC		=	frt"				>> macros_regridder	; \
		echo "FCFLAGS		=	-Free -X9 -Am -sc -Kfreealloc -Karraystack3"	>> macros_regridder	; \
		echo "LDFLAGS		=	-J" 				>> macros_regridder	; \
		echo "CCFLAGS		=	-DVPP -DBIT32 -I."		>> macros_regridder	; \
		echo "LOCAL_LIBRARIES	=	"				>> macros_regridder	; \
		( $(CD) regridder ; $(MAKE) all	) 							; \
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

intel:
	echo "Compiling for Linux using INTEL compiler"						; \
		echo "AR		=	$(AR)"				>  macros_pregrid	; \
		echo "RM		=	$(RM)"				>> macros_pregrid	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_pregrid	; \
		echo "CD		=	$(CD)"				>> macros_pregrid	; \
		echo "LN		=	$(LN)"				>> macros_pregrid	; \
		echo "MAKE		=	$(MAKE)"			>> macros_pregrid	; \
		echo "SHELL		=	/bin/sh"			>> macros_pregrid	; \
		echo "TOUCH		=	touch"				>> macros_pregrid	; \
		echo "CPP		=	/lib/cpp"			>> macros_pregrid	; \
		echo "CPPFLAGS		=	-I. -C -P -DDEC -DBIT32 -traditional"	>> macros_pregrid	; \
		echo "FC		=	ifort"				>> macros_pregrid	; \
		echo "FCFLAGS		=	-FR -I../util -convert big_endian" >> macros_pregrid	; \
		echo "LDFLAGS		=	-i_dynamic" 			>> macros_pregrid	; \
		echo "CCFLAGS		=	-DDEC -DBIT32 -I."		>> macros_pregrid	; \
		echo "LOCAL_LIBRARIES	=	../util/libpgu.a"		>> macros_pregrid	; \
		echo "RANLIB 		=	echo"				>> macros_pregrid	; \
		( $(CD) pregrid ; $(MAKE) all )								; \
		echo "AR		=	$(AR)"				>  macros_regridder	; \
		echo "RM		=	$(RM)"				>> macros_regridder	; \
		echo "RM_LIST		=	$(RM_LIST)"			>> macros_regridder	; \
		echo "CD		=	$(CD)"				>> macros_regridder	; \
		echo "LN		=	$(LN)"				>> macros_regridder	; \
		echo "MAKE		=	$(MAKE)"			>> macros_regridder	; \
		echo "SHELL		=	/bin/sh"			>> macros_regridder	; \
		echo "TOUCH		=	touch"				>> macros_regridder	; \
		echo "CPP		=	/lib/cpp"			>> macros_regridder	; \
		echo "CPPFLAGS		=	-I. -C -P -DDEC -DBIT32 -traditional"	>> macros_regridder	; \
		echo "FC		=	ifort"				>> macros_regridder	; \
		echo "FCFLAGS		=	-FR -pc 32  -convert big_endian">> macros_regridder	; \
		echo "LDFLAGS		=	-i_dynamic" 			>> macros_regridder	; \
		echo "CCFLAGS		=	-DDEC -DBIT32 -I."		>> macros_regridder	; \
		echo "LOCAL_LIBRARIES	=	"				>> macros_regridder	; \
		( $(CD) regridder ; $(MAKE) all	) 							; \

clean:
	( $(CD) pregrid   ; $(MAKE) clean "CD = $(CD)" "RM = $(RM)" "RM_LIST = $(RM_LIST)" )
	( $(CD) regridder ; $(MAKE) clean "CD = $(CD)" "RM = $(RM)" "RM_LIST = $(RM_LIST)" )
	$(RM) $(RM_LIST) .tmpfile
