#	Low level Makefile for regridder program.

include ../../configure.rules
include ../../macros_regridder

.IGNORE:

MODULES	=	module_constants.o module_date_pack.o module_diags.o module_file_data.o  \
		module_first_guess_data.o module_gauss.o \
		module_gridded_data.o module_header_data.o module_horiz_interp.o module_link_list_info.o \
		module_map_utils.o \
		module_namelist_info.o module_terrain_data.o module_tc_bogus.o \
		module_util.o

OBJS = 		proc_grid_store.o proc_ingest_first_guess.o proc_list_to_array.o \
		proc_make_dot_point_data.o proc_make_big_header.o \
		proc_make_small_header.o \
		proc_map_to_met_winds.o proc_met_to_map_winds.o \
		proc_namelist.o proc_output.o proc_tc_bogus.o \
		proc_read_terrain.o proc_zap_space_array.o \
		proc_zap_space_list.o regridder.o

all :		regridder \
		$(OBJS)  $(MODULES) \
		../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

regridder:	$(OBJS) $(MODULES) \
		../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile
		$(FC) -o $@ $(LDFLAGS) $(OBJS) $(MODULES) 

regridder.o:	module_namelist_info.o module_first_guess_data.o module_date_pack.o \
		../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile


proc_grid_store.o:   module_link_list_info.o module_namelist_info.o \
		../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

proc_ingest_first_guess.o:   module_diags.o module_first_guess_data.o module_header_data.o \
		module_terrain_data.o module_file_data.o module_namelist_info.o \
		module_gridded_data.o module_horiz_interp.o module_link_list_info.o \
		module_constants.o module_gauss.o \
		module_map_utils.o \
		../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

proc_list_to_array.o:   module_header_data.o module_namelist_info.o module_link_list_info.o \
		module_gridded_data.o module_terrain_data.o module_link_list_info.o \
		module_util.o module_diags.o \
		../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

proc_make_dot_point_data.o:   module_header_data.o module_namelist_info.o module_util.o \
		module_terrain_data.o module_gridded_data.o module_constants.o \
		../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

proc_make_big_header.o:   module_header_data.o module_namelist_info.o \
		../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

proc_make_small_header.o:   module_header_data.o module_namelist_info.o \
		module_gridded_data.o module_first_guess_data.o \
		../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

proc_map_to_met_winds.o:   module_header_data.o module_gridded_data.o module_namelist_info.o \
		module_constants.o \
		../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

proc_met_to_map_winds.o:   module_header_data.o module_gridded_data.o module_namelist_info.o \
		module_constants.o \
		../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

proc_namelist.o:   module_namelist_info.o module_file_data.o module_date_pack.o \
		../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

proc_output.o:   module_header_data.o module_namelist_info.o module_file_data.o  \
		module_gridded_data.o module_constants.o module_first_guess_data.o \
		../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

proc_read_terrain.o:   module_header_data.o module_terrain_data.o module_file_data.o \
		module_namelist_info.o module_constants.o \
		../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

proc_tc_bogus.o:   module_constants.o module_header_data.o module_file_data.o \
		module_first_guess_data.o module_gridded_data.o module_namelist_info.o \
		module_tc_bogus.o \
		../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

proc_zap_space_array.o:   module_namelist_info.o module_terrain_data.o module_gridded_data.o  \
		../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

proc_zap_space_list.o:   module_link_list_info.o \
		../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

module_constants.o:  ../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

module_date_pack.o:  ../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

module_diags.o:	module_constants.o module_util.o \
		../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

module_file_data.o:  ../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

module_first_guess_data.o:  ../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

module_gauss.o: ../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

module_gridded_data.o:  module_header_data.o \
			../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

module_header_data.o:  ../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

module_horiz_interp.o:  ../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

module_link_list_info.o:  ../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

module_map_utils.o:  ../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

module_namelist_info.o:  ../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

module_tc_bogus.o:	module_constants.o module_diags.o module_header_data.o \
			module_namelist_info.o module_map_utils.o \
			module_terrain_data.o module_util.o \
			../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

module_terrain_data.o:  module_header_data.o \
			../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

module_util.o:  module_constants.o \
		../../configure.rules ../../macros_regridder ./Makefile ../Makefile ../../Makefile

clean:
		$(RM) $(RM_LIST) regridder
