# Makefile for top directory 

DEVTOP = .
include ./configure.user

all:
# pjj/cray - X1 cross compiler
	(cd Util; if [ "$(X1_CROSS_COMP)" != "" ] ; then $(MAKE) CC="$(X1_CROSS_COMP)" CFLAGS=$(X1_CROSS_CFLG) ; else $(MAKE) ; fi )
	./parseconfig; \
	(cd include; $(MAKE)); \
	(cd memory; $(MAKE)); \
	(cd fdda; $(MAKE)); \
	(cd domain; $(MAKE));\
	(cd physics; $(MAKE));\
	(cd dynamics; $(MAKE));\
	(cd Run; $(MAKE));
code:
	find . -name \*.i -exec rm {} \; ; \
# pjj/cray - X1 cross compiler
	(cd Util; if [ "$(X1_CROSS_COMP)" != "" ] ; then $(MAKE) CC="$(X1_CROSS_COMP)" CFLAGS=$(X1_CROSS_CFLG) ; else $(MAKE) ; fi )
	./parseconfig; \
	(cd include; $(MAKE)); \
	(cd include; $(MAKE) code); \
	(cd memory; $(MAKE) code); \
	(cd fdda; $(MAKE) code); \
	(cd domain; $(MAKE) code);\
	(cd physics; $(MAKE) code);\
	(cd dynamics; $(MAKE) code);\
	(cd Run; $(MAKE) code);
little_f:
	(cd Util; $(MAKE)); \
	./parseconfig; \
	(cd include; $(MAKE)); \
	(cd memory; $(MAKE) little_f); \
	(cd fdda; $(MAKE) little_f); \
	(cd domain; $(MAKE) little_f); \
	(cd physics; $(MAKE) little_f); \
	(cd dynamics; $(MAKE) little_f); \
	(cd Run; $(MAKE) little_f);


mm5.deck:
	./Util/makedeck.csh $(RUNTIME_SYSTEM);

clean:
	(cd Util; $(MAKE) clean); \
	(cd include; $(MAKE) clean); \
	(cd memory; $(MAKE) clean); \
	(cd fdda; $(MAKE) clean); \
	(cd physics; $(MAKE) clean);\
	(cd domain; $(MAKE) clean);\
	(cd dynamics; $(MAKE) clean);\
	(cd Run; $(MAKE) clean); \
	if [ -f libutil.a ]; then $(RM) libutil.a; fi; 

rm_obj:
	(cd Util; $(MAKE) clean); \
	(cd include; $(MAKE) clean); \
	(cd memory; $(MAKE) clean); \
	(cd fdda; $(MAKE) clean); \
	(cd physics; $(MAKE) clean);\
	(cd domain; $(MAKE) clean);\
	(cd dynamics; $(MAKE) clean);\
	(cd Run; $(MAKE) rm_obj); \
	if [ -f libutil.a ]; then $(RM) libutil.a; fi; 

LineNumberer:
	$(CC) -o ./LineNumberer Util/LineNumberer.c;

mmlif:
	(cd Run; $(MAKE) mmlif);
	
###  Additions for MPP
#
#  To clean after changes to configure.user, type 'make mpclean'.
#  To uninstall everything relating to MPP option, 'make uninstall'.
#  To partially remake installation, remove MPP/mpp_install and 'make mpp'.
#

mpclean:
	(cd include; $(MAKE) clean); \
	(cd MPP/build ; /bin/rm -fr *.o *.f *.dm *.b *.lis)

mpp: MPP/mpp_install
# pjj/cray - X1 cross compiler
	(cd Util; if [ "$(X1_CROSS_COMP)" != "" ] ; then $(MAKE) CC="$(X1_CROSS_COMP)" CFLAGS=$(X1_CROSS_CFLG) ; else $(MAKE) ; fi )
	./parseconfig
	(cd include; $(MAKE))
	(cd include; $(MAKE) code ;  cd ../pick ; csh ../MPP/cutincludes $(CUT) )
	(sed '/t touch anything below this line/,$$d' configure.user \
                > ./MPP/conf.mpp)
	(cd MPP; if [ "$(X1_CROSS_COMP)" != "" ] ; then $(MAKE) CC="$(X1_CROSS_COMP)" CFLAGS=$(X1_CROSS_CFLG) col_cutter ; else $(MAKE) col_cutter ; fi)
	(cd MPP/build;                \
		/bin/rm -f .tmpobjs ; \
                $(CPP) -D$(RUNTIME_SYSTEM) -I../../pick ../mpp_objects_all > .tmpobjs ; \
                $(MAKE) -f Makefile.$(MPP_LAYER) parame ; )
	(cd MPP/build;                \
		$(MAKE) -f Makefile.$(MPP_LAYER) all)
#                $(MAKE) -P -f Makefile.$(MPP_LAYER) all)

MPP/mpp_install:
	(cd include; $(MAKE) code ;  cd ../pick ; csh ../MPP/cutincludes $(CUT) )
	(cd MPP/RSL/RSL ; if [ "$(RUNTIME_SYSTEM)" = "alpha" -o "$(RUNTIME_SYSTEM)" = "alphavector" ]  ; then $(MAKE) CC="$(MCC)" FC="$(MFC)" UNDERSCORE=$(UNDERSCORE) $(MPP_TARGET) ; else if [ "$(RUNTIME_SYSTEM)" = "linux" ] ; then $(MAKE) CC="$(MCC)" FC="$(MFC)" LINUX_MPIHOME=$(LINUX_MPIHOME) $(MPP_TARGET) ; else $(MAKE) $(MPP_TARGET) ; fi ; fi  )
	(cd MPP/FLIC ; if [ "$(X1_CROSS_COMP)" != "" ] ; then $(MAKE) CC="$(X1_CROSS_COMP)" CFLAGS=$(X1_CROSS_CFLG) ; else $(MAKE) CC=cc ; $(MAKE) clean ; fi )
	(cd MPP/FLIC/FLIC ; if [ "$(X1_CROSS_COMP)" != "" ] ; then $(MAKE) CC="$(X1_CROSS_COMP)" CFLAGS=$(X1_CROSS_CFLG) ; else $(MAKE) CC=cc ; fi ; \
	  $(MAKE) clean ; \
	  /bin/rm -f flic ; \
          sed s+INSTALL_STRING_FLICDIR+`pwd`+ flic.csh > flic ; \
	  chmod +x flic )
	(csh MPP/Makelinks $(MPP_LAYER) $(MPP_TARGET) )
	touch MPP/mpp_install

uninstall:
	(cd include; $(MAKE) clean)
	(cd memory; $(MAKE) clean)
	(cd fdda; $(MAKE) clean)
	(cd physics; $(MAKE) clean)
	(cd domain; $(MAKE) clean)
	(cd dynamics; $(MAKE) clean)
	(cd Run; $(MAKE) clean)
	if [ -f libutil.a ]; then $(RM) libutil.a; fi
	(cd MPP/FLIC/FLIC; /bin/rm -f dm flic reassemble ; $(MAKE) clean )
	(cd MPP/FLIC; $(MAKE) clean ; /bin/rm -fr bin )
	(cd MPP/RSL/RSL; $(MAKE) superclean )
	/bin/rm -f MPP/FLIC/h/*.h
	/bin/rm -fr MPP/build
	/bin/rm -f parseconfig
	/bin/rm -f MPP/col_cutter
	/bin/rm -f Run/mm5.exe
	/bin/rm -f Run/mm5.mpp
	/bin/rm -f pick/*.incl *.h
	/bin/rm -f MPP/mpp_install

mptar:
	@echo 'Making mm5mpp.tar'
	tar cf mm5mpp.tar CHANGES MPP                              \
	                  Makefile README.MPP ReleaseNotes         \
	                  Run/Makefile Run/README Run/ehtran       \
	                  Run/mm5.F                                \
	                  configure.user configure.user.linux      \
	                  doc domain dynamics fdda include memory  \
	                  physics Util util utility_programs       \
	                  Templates pick

