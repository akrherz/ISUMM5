#	Lower level Makefile for interpf

include ../configure.rules
include ../macros_interpf

.IGNORE:

SRC		=	interpf.F

OBJS		=	$(SRC:.F=.o)

MODULES		=	module_bdy.o module_date_pack.o module_diags.o module_file.o module_header_data.o \
			module_hydro_interp.o module_all_io.o \
			module_lateral_bdy.o module_nh_interp.o module_phys_consts.o module_util.o

all:			interpf \
			$(OBJS) $(MODULES) \
			../configure.rules ./Makefile ../Makefile

interpf:		../configure.rules ./Makefile ../Makefile \
			$(OBJS) $(MODULES)
			$(FC) -o $@ $(LDFLAGS) $(OBJS) $(MODULES)


interpf.o:		../configure.rules ./Makefile ../Makefile \
			module_bdy.o module_date_pack.o \
			module_diags.o module_file.o module_header_data.o module_hydro_interp.o \
			module_all_io.o module_lateral_bdy.o \
			module_nh_interp.o module_phys_consts.o \
			module_util.o

module_bdy.o:		../configure.rules ./Makefile ../Makefile 

module_date_pack.o:	../configure.rules ./Makefile ../Makefile

module_diags.o:		../configure.rules ./Makefile ../Makefile \
			module_phys_consts.o module_util.o

module_file.o:		../configure.rules ./Makefile ../Makefile 

module_header_data.o:	../configure.rules ./Makefile ../Makefile

module_hydro_interp.o:	../configure.rules ./Makefile ../Makefile \
			module_util.o

module_all_io.o:	../configure.rules ./Makefile ../Makefile \
			module_header_data.o module_util.o module_date_pack.o

module_lateral_bdy.o:	../configure.rules ./Makefile ../Makefile \
			module_all_io.o module_bdy.o module_date_pack.o module_header_data.o

module_nh_interp.o:	../configure.rules ./Makefile ../Makefile \
			module_phys_consts.o

module_phys_consts.o:	../configure.rules ./Makefile ../Makefile 

module_util.o:		../configure.rules ./Makefile ../Makefile

clean:
			$(RM) $(FOBJS) $(MODULES) $(OBJS) interpf core
			$(RM) *.mod
			$(RM) *.kmo
			$(RM) *.ctmi
