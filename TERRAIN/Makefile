#	Top-level Makefile for TERRAIN

#	Macros, these should be generic for all machines

.IGNORE:

AR	=	ar ru
CD	=	cd
LN	=	ln -s
MAKE	=	make -i -f Makefile
RM	=	/bin/rm -f
RM_LIST	=	*.o *.f core .tmpfile terrain.exe data_area.exe rdem.exe rdnml
RM_LIST2=	*.o *.f core .tmpfile make*.terrain.out terrain.namelist terrain.print.out
RM_Data =       DEM_*_GLOBAL LWMASK* SOILCAT.* SOILTEMP.* VEG-FRACTION.* VEG-USGS.* VEG-SIB.* new_30sdata* LANDUSE*
NCARGRAPHICS    =       NCARG
#NCARGRAPHICS   =       NONCARG
## Note: Don't forget to remove the libraries (or RHS) in the LOCAL_LIBRARIES 
##       line when not using NCAR Graphics.

#	Targets for supported architectures

default:
	uname -a > .tmpfile
	grep crayx1 .tmpfile ; \
	if [ $$? = 0 ]; then echo "Compiling for CRAY X1" ;                \
		( $(CD) src ; make -i -f Makefile.x1 all                                \
		"RM             = $(RM)"        "RM_LIST        = $(RM_LIST)"   \
		"LN             = $(LN)"        "MACH           = crayx1"         \
		"MAKE           = $(MAKE)"      "CPP            = cpp" \
		"CPPFLAGS       = -eZ -I. -D$(NCARGRAPHICS) -DRECLENBYTE -UCRAY"        \
		"FC             = ftn"          "FCFLAGS        = -I. -ra "     \
		"LDOPTIONS      = "             "CFLAGS         = "             \
		"LOCAL_LIBRARIES= " ) ; \
	else \
	grep CRAY .tmpfile ; \
	if [ $$? = 0 ]; then echo "Compiling for CRAY" ; 		\
		( $(CD) src ; $(MAKE) all				\
		"RM		= $(RM)" 	"RM_LIST	= $(RM_LIST)"	\
		"LN		= $(LN)" 	"MACH		= CRAY"		\
		"MAKE		= $(MAKE)"	"CPP		= /opt/ctl/bin/cpp" \
		"CPPFLAGS	= -I. -C -P -D$(NCARGRAPHICS) -DRECLENBYTE"	\
		"FC		= f90" 		"FCFLAGS	= -I."		\
		"LDOPTIONS      = " 		"CFLAGS         = "		\
		"LOCAL_LIBRARIES= -L/usr/local/lib -lncarg -lncarg_gks -lncarg_c -lX11 -lm" ) ; \
	else \
		grep OSF .tmpfile ; \
	if [ $$? = 0 ]; then echo "Compiling for Compaq" ; 		\
		( $(CD) src ; $(MAKE) all 				\
		"RM		= $(RM)" 	"RM_LIST	= $(RM_LIST)"	\
		"LN		= $(LN)" 	"MACH		= DEC"		\
		"MAKE		= $(MAKE)"	"CPP		= /usr/bin/cpp" \
		"CPPFLAGS	= -I. -C -P -D$(NCARGRAPHICS)"			\
		"FC		= f77"		"FCFLAGS	= -I. -convert big_endian -fpe"	\
		"LDOPTIONS      = "		"CFLAGS         = "		\
		"LOCAL_LIBRARIES= -L/usr/local/ncarg/lib -lncarg -lncarg_gks -lncarg_c -lX11 -lm" ) ; \
	else \
		grep IRIX .tmpfile ; \
	if [ $$? = 0 ]; then echo "Compiling for SGI" ; 		\
		( $(CD) src ; $(MAKE) all				\
		"RM		= $(RM)" 	"RM_LIST	= $(RM_LIST)"	\
		"LN		= $(LN)" 	"MACH		= SGI" 		\
		"MAKE		= $(MAKE)"	"CPP		= /lib/cpp"	\
	 	"CPPFLAGS	= -I. -C -P -D$(NCARGRAPHICS)"			\
		"FC		= f77" 		"FCFLAGS	= -I. -n32"	\
		"LDOPTIONS      = -n32"		"CFLAGS         = -I. -n32"	\
		"LOCAL_LIBRARIES= -L/usr/local/ncarg/lib -L/usr/local/lib -lncarg -lncarg_gks -lncarg_c -lX11 -lm" ) ; \
	else \
		grep HP .tmpfile ; \
	if [ $$? = 0 ]; then echo "Compiling for HP" ; 			\
		( $(CD) src ; $(MAKE) all				\
		"RM		= $(RM)" 	"RM_LIST	= $(RM_LIST)"	\
		"LN		= $(LN)" 	"MACH		= HP"		\
		"MAKE		= $(MAKE)"	"CPP		= /opt/langtools/lbin/cpp" \
		"CPPFLAGS= -I. -C -P -D$(NCARGRAPHICS) -DRECLENBYTE"		\
		"FC		= f77" 		"FCFLAGS	= -I. -O"	\
		"LDOPTIONS	= " 		"CFLAGS		= -Aa"		\
		"LOCAL_LIBRARIES= -L/usr/local/ncarg/lib -L/usr/local/lib -lncarg -lncarg_gks -lncarg_c -lX11 -lm" ) ; \
	else \
		grep SUN .tmpfile ; \
	if [ $$? = 0 ]; then echo "Compiling for SUN" ; 		\
		( $(CD) src ; $(MAKE) all				\
		"RM		= $(RM)" 	"RM_LIST	= $(RM_LIST)"	\
		"LN		= $(LN)" 	"MACH		= SUN"		\
		"MAKE		= $(MAKE)"	"CPP		= /usr/ccs/lib/cpp" \
		"CPPFLAGS=-I. -C -P -D$(NCARGRAPHICS) -DRECLENBYTE"		\
		"FC		= f77" 		"FCFLAGS	= -I."		\
		"LDOPTIONS	= "    		"CFLAGS		= -I."		\
		"LOCAL_LIBRARIES= -L/usr/local/ncarg/lib -L/usr/openwin/lib -L/usr/dt/lib -lncarg -lncarg_gks -lncarg_c -lX11 -lm" ) ; \
	else \
		grep AIX .tmpfile ; \
	if [ $$? = 0 ]; then echo "Compiling for IBM" ;			\
		( $(CD) src ; $(MAKE) all				\
		"RM		= $(RM)" 	"RM_LIST	= $(RM_LIST)"	\
		"LN		= $(LN)" 	"MACH		= IBM"		\
		"MAKE		= $(MAKE)"	"CPP		= /usr/lib/cpp" \
		"CPPFLAGS	= -I. -C -P -D$(NCARGRAPHICS) -DRECLENBYTE"	\
		"FC		= xlf"		"FCFLAGS	= -I. -O -qmaxmem=-1"\
		"LDOPTIONS	= -bmaxstack:512000000 -bmaxdata:2000000000"	 "CFLAGS		= -I."		\
		"LOCAL_LIBRARIES= -L/usr/local/lib32/r4i4 -lncarg -lncarg_gks -lncarg_c -lX11 -lm" ) ; \
	else \
		grep Darwin .tmpfile ; \
	if [ $$? = 0 ]; then echo "Compiling for MAC" ;			\
		( $(CD) src ; $(MAKE) all				\
		"RM		= $(RM)" 	"RM_LIST	= $(RM_LIST)"	\
		"LN		= $(LN)" 	"MACH		= IBM"		\
		"MAKE		= $(MAKE)"	"CPP		= /usr/bin/cpp" \
		"CPPFLAGS	= -I. -C -P -DNONCARG -DRECLENBYTE -xassembler-with-cpp"	\
		"FC		= xlf"		"FCFLAGS	= -I. -O "\
		"LDOPTIONS	= -Wl,-stack_size,10000000,-stack_addr,0xc0000000"	 "CFLAGS		= -I."		\
		"LOCAL_LIBRARIES= -L/usr/local/ncarg/lib -lncarg -lncarg_gks -lncarg_c -L/usr/X11R6/lib -lX11 -lm" ) ; \
	else \
		grep Linux .tmpfile ; \
	if [ $$? = 0 ]; then echo "Compiling for Linux" ;		\
		( $(CD) src ; $(MAKE) all				\
		"RM		= $(RM)" 	"RM_LIST	= $(RM_LIST)"	\
		"LN		= $(LN)" 	"MACH		= SGI"		\
		"MAKE		= $(MAKE)"	"CPP		= /lib/cpp"	\
		"CPPFLAGS	= -I. -C -traditional -D$(NCARGRAPHICS) -DRECLENBYTE"	\
		"FC		= pgf90"	"FCFLAGS	= -I. -byteswapio"\
		"LDOPTIONS	= " 		"CFLAGS		= -I."		\
		"LOCAL_LIBRARIES= -L$(NCARG_ROOT)/lib -L/usr/X11R6/lib -lncarg -lncarg_gks -lncarg_c -lX11 -L$(PGI)/linux86/lib -L/usr/lib -lf2c" ) ; \
	else echo "Do not know how to compile for the `cat .tmpfile` machine." \
		fi ; \
		fi ; \
		fi ; \
		fi ; \
		fi ; \
		fi ; \
		fi ; \
		fi ; \
		fi ; \
	fi ; \
		( $(RM) terrain.exe ; $(LN) src/terrain.exe . ) ;

derecho:
	echo "Compiling with Portland Compiler on Derecho" ;		\
	( $(CD) src ; $(MAKE) all				\
	"RM		= $(RM)" 	"RM_LIST	= $(RM_LIST)"	\
	"LN		= $(LN)" 	"MACH		= SGI"		\
	"MAKE		= $(MAKE)"	"CPP		= /lib/cpp"	\
	"CPPFLAGS	= -I. -C -traditional -D$(NCARGRAPHICS) -DRECLENBYTE"	\
	"FC		= pgf90"	"FCFLAGS	= -I. -byteswapio"\
	"LDOPTIONS	= " 		"CFLAGS		= -I."		\
	"LOCAL_LIBRARIES= -L$(NCARG_ROOT)/lib -L/usr/X11R6/lib64 -lncarg -lncarg_gks -lncarg_c -lX11 -L/usr/lib64 -lg2c" ) ;

intel:
	echo "Compiling for Linux using INTEL compiler"
	( $(CD) src ; $(MAKE) all				\
	"RM		= $(RM)" 	"RM_LIST	= $(RM_LIST)"	\
	"LN		= $(LN)" 	"MACH		= SGI"		\
	"MAKE		= $(MAKE)"	"CPP		= /lib/cpp"	\
	"CPPFLAGS	= -I. -C -traditional -D$(NCARGRAPHICS) "	\
	"FC		= ifort  "	"FCFLAGS	= -I. -w90 -w95 -convert big_endian " \
	"LDOPTIONS	= -i_dynamic" 		"CFLAGS		= -I."		\
	"LOCAL_LIBRARIES= -L$(NCARG_ROOT)/lib -L/usr/X11R6/lib -lncarg -lncarg_gks -lncarg_c -lX11 -lgfortran -lpng" ) ; \
		( $(RM) terrain.exe ; $(LN) src/terrain.exe . ) ;

terrain.deck:
	uname -a > .tmpfile
	grep OSF .tmpfile ; \
	if [ $$? = 0 ]; then \
		echo "Making terrain deck for Compaq" ; 		\
		( cp Templates/terrain.deck.dec terrain.deck ) ;	\
	else \
		grep crayx1 .tmpfile ; \
	if [ $$? = 0 ]; then \
		echo "Making terrain deck for CRAY X1" ;                \
		( cp Templates/terrain.deck.x1 terrain.deck ) ; \
	else \
		grep CRAY .tmpfile ; \
	if [ $$? = 0 ]; then \
		echo "Making terrain deck for CRAY" ; 			\
		( cp Templates/terrain.deck.cray terrain.deck ) ;	\
        else \
		grep IRIX .tmpfile ; \
	if [ $$? = 0 ]; then \
		echo "Making terrain deck for SGI" ; 			\
		( cp Templates/terrain.deck.sgi terrain.deck ) ;	\
        else \
		grep HP .tmpfile ; \
	if [ $$? = 0 ]; then \
		echo "Making terrain deck for HP" ; 			\
		( cp Templates/terrain.deck.hp terrain.deck ) ;		\
        else \
		grep SUN .tmpfile ; \
	if [ $$? = 0 ]; then \
		echo "Making terrain deck for SUN" ; 			\
		( cp Templates/terrain.deck.sun terrain.deck ) ;	\
        else \
		grep AIX .tmpfile ; \
	if [ $$? = 0 ]; then \
		echo "Making terrain deck for IBM" ; 			\
		( cp Templates/terrain.deck.ibm terrain.deck ) ;	\
        else \
		grep Darwin .tmpfile ; \
	if [ $$? = 0 ]; then \
		echo "Making terrain deck for MAC" ; 			\
		( cp Templates/terrain.deck.mac terrain.deck ) ;	\
        else \
		grep Linux .tmpfile ; \
	if [ $$? = 0 ]; then \
		echo "Making terrain deck for Linux" ; 			\
		( cp Templates/terrain.deck.sgi terrain.deck ) ;	\
		( cp Templates/terrain.deck.intel terrain.deck.intel ) ;	\
	fi; \
	fi; \
	fi; \
	fi; \
	fi; \
	fi; \
	fi; \
	fi; \
	fi;

code:
	( $(CD) src ; $(MAKE) code					\
	"MAKE			=	$(MAKE)"			\
	"CPP			=	/usr/bin/cpp"			\
	"CPPFLAGS		=	-I. -C -P -DDEC"		)

clean:
	( $(CD) src   ; $(MAKE) clean "CD = $(CD)" "RM = $(RM)" "RM_LIST = $(RM_LIST)" )
	$(RM) $(RM_LIST2)

dataclean:
	( $(CD) Data ; $(RM) $(RM_Data) ; $(CD) .. ; $(RM) -r Data30s )
