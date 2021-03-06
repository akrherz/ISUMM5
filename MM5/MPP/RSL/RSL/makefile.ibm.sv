
# this is a makefile for RSL that maps the package down to the
# MPI message passing primitives on the IBM SP[12] and nets of wkstations

include makefile.core
OBJ = $(CORE_OBJ) rsl_mpi_compat.o mpi_init_f.o debug.o vicopy.o
HDR = $(CORE_HDR)
TAR = $(CORE_TAR)

CC = cc
FC = xlf

MPIHOME=/usr/local/mpi
IDIR=$(MPIHOME)/include

LIB = 

CFLAGS = -I$(IDIR) -DNOUNDERSCORE -DMPI -g # -O # -g
FFLAGS = -g

warning :
	@ echo 'This makefile is not a top level makefile'
	@ echo 'and is not intended for direct use.  Please'
	@ echo 'type "make" by itself for assistance.'

all : rsl.inc librsl.a

rsl.inc : $(HDR) rsl.inc_base
	cat $(HDR) | sed $(SED_LINE) |\
	grep '^#.*define.*\/\* FORTRAN \*\/' | \
	awk '{printf("      integer %s\n      parameter(%s=%s)\n",$$2,$$2,$$3)}' | \
	cat rsl.inc_base - > rsl.inc

librsl.a : $(OBJ)
	ar cr librsl.a $(OBJ)
	ranlib librsl.a

.c.o :
	$(CC) -c $(CFLAGS) $(CONFIG_OPTS) $<

.f.o :
	$(FC) -c $(FFLAGS) $<

clean :
	/bin/rm -f *.o

###

$(OBJ) : $(HDR)
