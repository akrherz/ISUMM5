# Makefile for directory include
# 1. Creates "parame.incl" using parameters set in configure.user

DEVTOP = ..
include ../configure.user
include ../include/config.INCL

CURRENT_DIR = $(DEVTOP)/include
PINCL = parame.incl.tmp
PARAME = parame.incl

# -------------------------------------------------------------------------
all::
	@echo "C PARAME">$(PINCL)
	@echo "C">>$(PINCL)
	@echo "C">>$(PINCL)
	@echo "C--- ADDITIONAL MEMORY REQUIREMENTS FOR RUNS ,">>$(PINCL)
	@echo "C--- GRIDDED FDDA RUNS (IFDDAG=1) AND OBS FDDA RUNS (IFDDAO=1), ">>$(PINCL)
	@echo "C--- NONHYDROSTATIC RUNS (INHYD=1), HIGHER ORDER PBL RUNS (INAV=1),">>$(PINCL)
	@echo "C--- EXPLICIT MOISTURE SCHEME (IEXMS=1), ARAKAWA-SCHUBERT">>$(PINCL)
	@echo "C--- CONVECTIVE PARAMETERIZATION (IARASC=1), ATMOSPHERIC">>$(PINCL)
	@echo "C--- RADIATION (IRDDIM=1), MIXED-PHASE ICE SCHEME (IICE=1).">>$(PINCL)
	@echo "C--- GRAUPEL SCHEME (IICEG=1), KAIN-FRITSCH AND FRITSCH-CHAPPELL.">>$(PINCL)
	@echo "C--- CONVECTIVE PARAMETERIZATIONS (IKFFC=1), AND GAYNO-SEAMAN PBL (IGSPBL=1).">>$(PINCL)
	@echo "C--- MULTI-LAYER SOIL (ISLDIM=1,MLX=6 OR 8), NOAH LAND SFC (ILDDIM=1,MLX=4). ">>$(PINCL)
	@echo "C">>$(PINCL)
	@echo "C">>$(PINCL)
	@echo "      INTEGER IARASC,IEXMS,IFDDAG,IFDDAO,IICE,IICEG,IKFFC,ILDDIM,INAV" >>$(PINCL)
	@echo "      INTEGER INAV2,INAV3,IGSPBL,INHYD,IRDDIM,ISLDIM,MLX" >>$(PINCL)
	@echo "      PARAMETER (IFDDAG=$(FDDAGD),IFDDAO=$(FDDAOBS),INHYD=1,INAV=$(NAV),INAV2=$(NAV2),INAV3=$(NAV3),">>$(PINCL)
	@echo "     1 IICE=$(ICE),IICEG=$(ICEG),IEXMS=$(EXMS),IKFFC=$(KFFC),IARASC=$(ARASC),IRDDIM=$(RDDIM),">>$(PINCL)
	@echo "     2 IGSPBL=$(GSPBL),ISLDIM=$(SLDIM),ILDDIM=$(LDDIM),MLX=$(MLX))">>$(PINCL)
	@echo "C">>$(PINCL)
	@echo "C--- NUMBER OF DOMAINS (MAXNES)">>$(PINCL)
	@echo "      INTEGER MAXNES">>$(PINCL)
	@echo "      PARAMETER (MAXNES=$(MAXNES))">>$(PINCL)
	@echo "C">>$(PINCL)
	@echo "C">>$(PINCL)
	@echo "C--- MAXIMUM DIMENSIONS OF ANY DOMAIN">>$(PINCL)
	@echo "C">>$(PINCL)
	@echo "      INTEGER MIX,MJX,MKX">>$(PINCL)
	@echo "      PARAMETER (MIX=$(MIX),MJX=$(MJX),MKX=$(MKX))">>$(PINCL)
	@echo "C">>$(PINCL)
	@echo "C">>$(PINCL)
	@echo "C--- MAXIMUM NUMBER OF NEST MOVES FOR A DOMAIN ">>$(PINCL)
	@echo "C">>$(PINCL)
	@echo "      INTEGER MAXMV">>$(PINCL)
	@echo "      PARAMETER (MAXMV=10)">>$(PINCL)
	cat parame >> $(PINCL)
	@../Util/incldiff.sh $(PINCL) $(PARAME)
	@../Util/incldiff.sh defines.incl.tmp defines.incl

emptyrule::

code::
	cp *.incl ../pick

clean::
	$(RM) $(CURRENT_DIR)/parame.incl

# -------------------------------------------------------------------------
