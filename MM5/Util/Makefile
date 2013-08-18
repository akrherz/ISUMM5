# Makefile for directory utility_programs ("all")
# 1. "parseconfig" is made if out of date
# 2. linesplitter72 is only made if linesplitter.c is new
#    It is used if the lookup table is used (IMPHYS=4)
# Contact: D. Hansen,NCAR 1998

DEVTOP = ..
include ../configure.user

all:	../parseconfig

linesplitter72: linesplitter72.c
	$(CC) $(CFLAGS) -o linesplitter72 ./linesplitter72.c;

../parseconfig: parseconfig.c
	$(CC) $(CFLAGS) -o ../parseconfig ./parseconfig.c

# -------------------------------------------------------------------------
# common rules for all Makefiles - do not edit

emptyrule::

clean::
	$(RM_CMD) "#"* linesplitter72 ../parseconfig

# -------------------------------------------------------------------------
