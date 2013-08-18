# Makefile for directory Run

DEVTOP = ..
include ../configure.user
include ../include/config.INCL

CURRENT_DIR = $(DEVTOP)/Run

OBJS =\
	mm5.o
SRC =\
	mm5.i

SRCF =\
	mm5.f

LIBTARGET = util
TARGETDIR = ../../

ALLFILES = $(SRCS)
MMLIF = mmlif

LDLIBS = ../libutil.a $(SYS_LIBRARIES) $(EXTRA_LIBRARIES)

PROGRAM = mm5.exe


all:: mm5.exe

code:: $(SRC)

mm5.exe: $(OBJS) $(DEPLIBS) $(LDLIBS)
	 $(RM) $@
	$(RANLIB) $(LDLIBS)
	$(FC) -o $@ $(OBJS) $(LDOPTIONS) $(LOCAL_LIBRARIES) $(LDLIBS) $(EXTRA_LOAD_FLAGS)

little_f:: $(SRCF) mm5.exe

mmlif::
	-@if [ -f mmlif ]; then $(RM) mmlif; fi
	cat < oparam > mmlif
	cat < lparam >> mmlif
	echo " IFRAD = `echo $(FRAD)|cut -d, -f1`,">>$(MMLIF)
	echo " ICUPA  = $(ICUPA),">>$(MMLIF)
	echo " IMPHYS = $(IMPHYS),">>$(MMLIF)
	echo " IBLTYP = $(IBLTYP),">>$(MMLIF)
	echo " ISHALLO = $(ISHALLO),">>$(MMLIF)
	echo " IPOLAR = $(IPOLAR),">>$(MMLIF)
	echo " ISOIL = $(ISOIL),">>$(MMLIF)
	if [ "$(RUNTIME_SYSTEM)" = "IBM" ]; then \
	  echo " / ">>$(MMLIF); \
	elif [ "$(RUNTIME_SYSTEM)" = "sp2" ]; then \
	  echo " / ">>$(MMLIF); \
	elif [ "$(RUNTIME_SYSTEM)" = "macxlf" ]; then \
	  echo " / ">>$(MMLIF); \
	elif [ "$(RUNTIME_SYSTEM)" = "HP" ]; then \
	  echo ' $$END '>>$(MMLIF); \
	else \
	  echo " &END ">>$(MMLIF); \
	fi;
	cat < nparam >> mmlif
	cat < pparam >> mmlif
	cat < fparam >> mmlif
#//END mmlif


# -------------------------------------------------------------------------
# common rules for all Makefiles - do not edit

emptyrule::

clean::
	$(RM_CMD) "#"* $(PROGRAM)

rm_obj::
	$(RM_CMD)

# -------------------------------------------------------------------------
# DO NOT DELETE THIS LINE -- make depend depends on it.

mm5.o: ../include/defines.incl ../include/parame.incl ../include/param2.incl
mm5.o: ../include/param3.incl ../include/addr0.incl ../include/various.incl
mm5.o: ../include/dusolve1.incl ../include/variousn.incl
mm5.o: ../include/addrcu.incl
