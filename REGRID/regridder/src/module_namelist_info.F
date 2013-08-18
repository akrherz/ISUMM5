MODULE namelist_info

   TYPE r1
      CHARACTER (LEN=19) :: starting_date
      CHARACTER (LEN=19) :: ending_date
      INTEGER :: time_increment_second
      INTEGER :: yyyy_st  , mm_st  , dd_st  , hh_st  , min_st  , sec_st
      INTEGER :: yyyy_end , mm_end , dd_end , hh_end , min_end , sec_end
      INTEGER :: yyyy_now , mm_now , dd_now , hh_now , min_now , sec_now
      INTEGER :: total_times
      CHARACTER (LEN=19) :: current_date
      INTEGER :: starting_date_int , current_date_int , time_period_count
   END TYPE r1

   TYPE r2
      REAL :: ptop
      INTEGER :: number_of_new_levels
      REAL , DIMENSION(1000) :: new_levels
      REAL :: sst_to_ice_threshold
      LOGICAL :: linear_interpolation
      REAL :: min_RH_value
   END TYPE r2

   TYPE r3
      CHARACTER (LEN=100) , DIMENSION(100) :: root
      CHARACTER (LEN=100) , DIMENSION(100) :: constants_full_name
      CHARACTER (LEN=100)                  :: terrain_file_name
      INTEGER :: max_files_per_single_time
      INTEGER :: max_files_constants
   END TYPE r3

   TYPE r4
      LOGICAL :: print_echo 
      LOGICAL :: print_debug
      LOGICAL :: print_mask
      LOGICAL :: print_interp
      LOGICAL :: print_link_list_store
      LOGICAL :: print_array_store
      LOGICAL :: print_header
      LOGICAL :: print_output
      LOGICAL :: print_file
      LOGICAL :: print_tc
      LOGICAL :: print_f77_info
   END TYPE r4

   INTEGER , PARAMETER :: num_max = 100
   TYPE r5
      LOGICAL :: insert_bogus_storm
      INTEGER :: num_storm
      REAL , DIMENSION(num_max) :: latc_loc 
      REAL , DIMENSION(num_max) :: lonc_loc
      REAL , DIMENSION(num_max) :: vmax
      REAL :: rmax 
      REAL :: r_search 
      REAL :: r_vor 
      REAL :: humidity_max
      REAL :: beta 
      REAL :: devpc 
      REAL :: vmax_ratio
      INTEGER :: vert_variation
   END TYPE r5

   TYPE (r1) :: rec1
   TYPE (r2) :: rec2
   TYPE (r3) :: rec3
   TYPE (r4) :: rec4
   TYPE (r5) :: rec5

END MODULE namelist_info
