# Makefile for directory physics/cumulus/shared

DEVTOP = ../../..
include ../../../configure.user

CURRENT_DIR = $(DEVTOP)/physics/cumulus/shared

OBJS =\
	heipre.o \
	maxim.o \
	minim.o \
	moiene.o \
	precip.o \
	zunc.o \
	condload.o \
	envirtht.o \
	prof5.o \
	kfbmdata.o

SRC =\
	heipre.i \
	maxim.i \
	minim.i \
	moiene.i \
	precip.i \
	zunc.i \
	condload.i \
	envirtht.i \
	prof5.i \
	kfbmdata.i

SRCF =\
	heipre.f \
	maxim.f \
	minim.f \
	moiene.f \
	precip.f \
	zunc.f \
	condload.f \
	envirtht.f \
	prof5.f \
	kfbmdata.f

OBJS_AS_SHALLOW =\
	heipre.o \
	maxim.o \
	minim.o \
	moiene.o \
	precip.o \
	zunc.o 

SRC_AS_SHALLOW =\
	heipre.i \
	maxim.i \
	minim.i \
	moiene.i \
	precip.i \
	zunc.i 

SRCF_AS_SHALLOW =\
	heipre.f \
	maxim.f \
	minim.f \
	moiene.f \
	precip.f \
	zunc.f 

OBJS_KF_BM =\
	kfbmdata.o

SRC_KF_BM =\
	kfbmdata.i

SRCF_KF_BM =\
	kfbmdata.f

OBJS_KF =\
	condload.o \
	envirtht.o \
	prof5.o

SRC_KF =\
	condload.i \
	envirtht.i \
	prof5.i

SRCF_KF =\
	condload.f \
	envirtht.f \
	prof5.f

LIBTARGET = util
TARGETDIR = ../../../

all:: $(OBJS)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS)

as_shallow:: $(OBJS_AS_SHALLOW)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS_AS_SHALLOW)

kf_bm:: $(OBJS_KF_BM)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS_KF_BM)

kf:: $(OBJS_KF)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS_KF)

code:: $(SRC)

as_shallow_code:: $(SRC_AS_SHALLOW)

kf_bm_code:: $(SRC_KF_BM)

kf_code:: $(SRC_KF)

little_f:: $(SRCF)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS)

as_shallow_little_f:: $(SRCF_AS_SHALLOW) $(OBJS_AS_SHALLOW)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS_AS_SHALLOW)

kf_bm_little_f:: $(SRCF_KF_BM) $(OBJS_KF_BM)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS_KF_BM)

kf_little_f:: $(SRCF_KF) $(OBJS_KF)
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS_KF)

# -------------------------------------------------------------------------
# common rules for all Makefiles - do not edit

emptyrule::

clean::
	$(RM_CMD) "#"*

# -------------------------------------------------------------------------
# DO NOT DELETE THIS LINE -- make depend depends on it.
