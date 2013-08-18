SUBROUTINE proc_namelist

   USE date_pack
   USE namelist_info
   USE file_data

   IMPLICIT NONE

   INTEGER                :: start_year       , &
                             start_month      , &
                             start_day        , &
                             start_hour       , &
                             start_minute = 0 , &
                             start_second = 0
   INTEGER                :: end_year         , &
                             end_month        , &
                             end_day          , &
                             end_hour         , &
                             end_minute   = 0 , &
                             end_second   = 0
   INTEGER                :: interval
   REAL                   :: ptop_in_Pa , sst_to_ice_threshold
   REAL                   :: min_RH_value                       
   REAL , DIMENSION(1000) :: new_levels_in_Pa = -999
   LOGICAL                :: linear_interpolation
   INTEGER                :: i , j
   CHARACTER(LEN=100),DIMENSION(100) :: root                = (/ ( "                    ",I=1,100 ) /)
   CHARACTER(LEN=100),DIMENSION(100) :: constants_full_name = (/ ( "                    ",I=1,100 ) /)
   CHARACTER(LEN=100)                :: terrain_file_name   =      "                    " 
   LOGICAL                :: print_echo , print_debug , print_mask , &
                             print_interp , print_link_list_store , &
                             print_array_store , print_header , print_output , &
                             print_file , print_tc , print_f77_info
   LOGICAL                :: insert_bogus_storm
   INTEGER                :: num_storm , vert_variation
   REAL , DIMENSION(num_max)  :: latc_loc , lonc_loc , vmax_meters_per_second
   REAL                   :: rmax , r_search , r_vor , humidity_max , beta , devpc , vmax_ratio

   NAMELIST / record1 / start_year   , start_month  , start_day    , &
                        start_hour   , start_minute , start_second , &
                        end_year     , end_month    , end_day      , &
                        end_hour     , end_minute   , end_second   , &
                        interval
   NAMELIST / record2 / ptop_in_Pa , new_levels_in_Pa , sst_to_ice_threshold , linear_interpolation, min_RH_value
   NAMELIST / record3 / root , constants_full_name , terrain_file_name
   NAMELIST / record4 / print_echo , print_debug , print_mask , &
                        print_interp , print_link_list_store , &
                        print_array_store , print_header , print_output , &
                        print_file , print_tc , print_f77_info
   NAMELIST / record5 / insert_bogus_storm , num_storm , latc_loc , lonc_loc , vmax_meters_per_second , &
                        rmax , r_search , r_vor , humidity_max , beta , devpc ,  vert_variation , vmax_ratio

   !  This data is the local stuff for the time loop.  We
   !  compute the incremental times from the starting and
   !  ending times defined in the NAMELIST. 

   INTEGER :: cy1,mm1,dd1,hh1,mi1,ss1
   INTEGER :: cy2,mm2,dd2,hh2,mi2,ss2
   CHARACTER (LEN=19) :: current_date , next_date

   !  To compute how many new levels are to be added to the analysis
   !  through vertical interpolation, we need an index.

   INTEGER :: new_levels_index , skip5 , storm_loop

   !  Find a spare UNIT number for the NAMELIST file.

   namelist_file = -1
   namelist_unit : DO file_index = file_index_min , file_index_max
      INQUIRE (UNIT=file_index, OPENED=already_opened)
      IF ( .NOT. already_opened) THEN
         namelist_file = file_index
         EXIT namelist_unit
      END IF
   END DO namelist_unit   
   
   IF ( namelist_file .EQ. -1 ) THEN
      PRINT '(A)','Could not find a spare unit for the NAMELIST data.'
      STOP 'namelist_file_unit'
   END IF

   !  Play it safe and REWIND the file.

   REWIND ( UNIT=namelist_file )

   PRINT '(A,A/)','Trying to open file: ',file_name_namelist

   !  OPEN the NAMELIST file with the given name and UNIT.

   OPEN ( UNIT=namelist_file , &
          FILE='namelist.input' , &
          STATUS = 'OLD' , &
          FORM = 'FORMATTED' , &
          IOSTAT = open_status )
   
   IF ( open_status .NE. 0 ) THEN
      PRINT '(A)' , 'Error opening NAMELIST file.'
      STOP 'namelist_file_open'
   END IF

   !  Undefined values for some fields are OK.

   sst_to_ice_threshold = -9999
   linear_interpolation = .TRUE.
   insert_bogus_storm   = .FALSE.
   min_RH_value         = 5.0

   !  Hidden values for the TC bogus scheme.

   rmax           =   90000.0
   r_search       =  400000.0
   r_vor          =  300000.0
   humidity_max   =      95.0 
   vmax_ratio     =       0.75
   beta           =       0.5
   devpc          =      40.0
   vert_variation =       1

   !  Read each record from the NAMELIST file.

   READ ( UNIT = namelist_file , NML = record1 )
   READ ( UNIT = namelist_file , NML = record2 )
   READ ( UNIT = namelist_file , NML = record3 )
   READ ( UNIT = namelist_file , NML = record4 )
   READ ( UNIT = namelist_file , NML = record5 , IOSTAT=skip5 )

   PRINT '(A)','Successfully READ the NAMELIST file.'

   IF ( print_echo ) THEN
      WRITE ( UNIT = 6 , NML = record1 )
   END IF

   new_levels_index = 1
   DO WHILE ( new_levels_in_Pa(new_levels_index) .GT. 0 )
      new_levels_index = new_levels_index + 1
   END DO
   new_levels_index = new_levels_index - 1 
   IF ( print_echo ) THEN
      WRITE ( UNIT = 6 , FMT='(A)' ) ' &RECORD2'
      WRITE ( UNIT = 6 , FMT='(A,F10.2)' ) ' PTOP_IN_PA = ',ptop_in_Pa
      IF ( new_levels_index .GT. 0 ) THEN
         WRITE ( UNIT = 6 , ADVANCE='NO' , FMT='(A)' ) ' NEW_LEVELS_IN_PA = '
         WRITE ( UNIT = 6 , FMT='(6F10.2)' ) new_levels_in_Pa(1:new_levels_index)
      END IF
      WRITE ( UNIT = 6 , FMT='(A,F10.2)' ) ' SST_TO_ICE_THRESHOLD = ',sst_to_ice_threshold
      WRITE ( UNIT = 6 , FMT='(A,L7)' ) ' LINEAR_INTERPOLATION = ',linear_interpolation
      WRITE ( UNIT = 6 , FMT='(A)' ) ' /'
   END IF

   rec3%max_files_constants = 1
   DO WHILE ( constants_full_name(rec3%max_files_constants)(1:20) .NE. '                    ' )
      rec3%max_files_constants = rec3%max_files_constants + 1
   END DO
   rec3%max_files_constants = rec3%max_files_constants - 1
   rec3%max_files_per_single_time = 1
   DO WHILE ( root(rec3%max_files_per_single_time)(1:20) .NE. '                    ' )
      rec3%max_files_per_single_time = rec3%max_files_per_single_time + 1
   END DO
   rec3%max_files_per_single_time = rec3%max_files_per_single_time - 1
   IF ( rec3%max_files_per_single_time .GT. 0 ) THEN
      IF ( print_echo ) THEN
         WRITE ( UNIT = 6 , FMT='(A)' ) ' &RECORD3'
         WRITE ( UNIT = 6 , ADVANCE='NO' , FMT='(A)' ) ' ROOT = '
         WRITE ( UNIT = 6 , FMT='(A,1X)' ) root(1:rec3%max_files_per_single_time)
         IF ( rec3%max_files_per_single_time .GT. 0 ) THEN
            WRITE ( UNIT = 6 , ADVANCE='NO' , FMT='(A)' ) ' CONSTANTS_FULL_NAME = '
            WRITE ( UNIT = 6 , FMT='(A,1X)' ) constants_full_name(1:rec3%max_files_constants)
         END IF
         WRITE ( UNIT = 6 , FMT='(A,A)' ) ' TERRAIN_FILE = ',TRIM(terrain_file_name)
         WRITE ( UNIT = 6 , FMT='(A)' ) ' /'
      END IF
   ELSE
      PRINT '(A)','You must supply at least a single root name for the first guess file.'
      STOP 'No_name_given_for_first_guess_file'
   END IF
   IF ( ( rec3%max_files_per_single_time .EQ. 0 ) .AND. ( print_echo ) ) THEN
      PRINT '(A)','No constants file found (such as for SST or SNOW), continuing.'
   END IF
   IF ( terrain_file_name(1:20) .EQ. '                    ' ) THEN
      PRINT '(A)','A terrain file name must be supplied in the namelist.'
      STOP 'no_terrain_file_name'
   END IF

   IF ( print_echo ) THEN
      WRITE ( UNIT = 6 , NML = record4 )
   END IF

   IF ( print_echo ) THEN
      IF ( skip5 .EQ. 0 ) THEN
         WRITE ( UNIT = 6 , FMT='(A)' ) ' &RECORD5'
         WRITE ( UNIT = 6 , FMT='(A,L7)' ) ' INSERT_BOGUS_STORM = ',insert_bogus_storm
         WRITE ( UNIT = 6 , FMT='(A,I3)' ) ' NUM_STORM = ',num_storm
         WRITE ( UNIT = 6 , ADVANCE='NO' , FMT='(A)' ) ' LATC_LOC = '
         DO storm_loop = 1 , num_storm-1
            WRITE ( UNIT = 6 , ADVANCE = 'NO' , FMT='(F9.4,A)' ) latc_loc(storm_loop) , ","
         END DO
         WRITE ( UNIT = 6 , FMT='(F9.4)' ) latc_loc(num_storm)
         WRITE ( UNIT = 6 , ADVANCE='NO' , FMT='(A)' ) ' LONC_LOC = '
         DO storm_loop = 1 , num_storm-1
            WRITE ( UNIT = 6 , ADVANCE = 'NO' , FMT='(F9.4,A)' ) lonc_loc(storm_loop) , ","
         END DO
         WRITE ( UNIT = 6 , FMT='(F9.4)' ) lonc_loc(num_storm)
         WRITE ( UNIT = 6 , ADVANCE='NO' , FMT='(A)' ) ' VMAX_METERS_PER_SECOND = '
         DO storm_loop = 1 , num_storm-1
            WRITE ( UNIT = 6 , ADVANCE = 'NO' , FMT='(F9.4,A)' ) vmax_meters_per_second(storm_loop) , ","
         END DO
         WRITE ( UNIT = 6 , FMT='(F9.4)' ) vmax_meters_per_second(num_storm)
         WRITE ( UNIT = 6 , FMT='(A,F8.1)' ) ' RMAX = ',rmax
         WRITE ( UNIT = 6 , FMT='(A,F8.1)' ) ' R_SEARCH = ',r_search
         WRITE ( UNIT = 6 , FMT='(A,F8.1)' ) ' R_VOR = ',r_vor
         WRITE ( UNIT = 6 , FMT='(A,F6.2)' ) ' HUMIDITY_MAX ',humidity_max
         WRITE ( UNIT = 6 , FMT='(A,F4.2)' ) ' VMAX_RATIO = ',vmax_ratio
         WRITE ( UNIT = 6 , FMT='(A,F3.1)' ) ' BETA = ',beta
         WRITE ( UNIT = 6 , FMT='(A,F4.1)' ) ' DEVPC = ',devpc
         WRITE ( UNIT = 6 , FMT='(A,I1)' ) ' VERT_VARIATION = ',vert_variation
         WRITE ( UNIT = 6 , FMT='(A)' ) ' /'
      END IF
   END IF

   !  Assign each local NAMELIST variable to the module name.

   rec1%yyyy_st                   = start_year
   rec1%mm_st                     = start_month
   rec1%dd_st                     = start_day
   rec1%hh_st                     = start_hour
   rec1%min_st                    = start_minute
   rec1%sec_st                    = start_second
   rec1%yyyy_end                  = end_year
   rec1%mm_end                    = end_month
   rec1%dd_end                    = end_day
   rec1%hh_end                    = end_hour
   rec1%min_end                   = end_minute
   rec1%sec_end                   = end_second
   rec1%time_increment_second     = interval
   WRITE (rec1%starting_date,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)') &
   rec1%yyyy_st , rec1%mm_st , rec1%dd_st , rec1%hh_st , rec1%min_st , rec1%sec_st
   WRITE (rec1%ending_date,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)') &
   rec1%yyyy_end , rec1%mm_end , rec1%dd_end , rec1%hh_end , rec1%min_end , rec1%sec_end

   rec2%ptop                      = ptop_in_Pa
   rec2%number_of_new_levels      = new_levels_index
   rec2%new_levels                = new_levels_in_Pa
   rec2%sst_to_ice_threshold      = sst_to_ice_threshold
   rec2%linear_interpolation      = linear_interpolation
   rec2%min_RH_value              = min_RH_value        

   rec3%root                      = root
   rec3%constants_full_name       = constants_full_name
   rec3%terrain_file_name         = terrain_file_name
   
   rec4%print_echo                = print_echo 
   rec4%print_debug               = print_debug 
   rec4%print_mask                = print_mask 
   rec4%print_interp              = print_interp 
   rec4%print_link_list_store     = print_link_list_store 
   rec4%print_array_store         = print_array_store 
   rec4%print_header              = print_header 
   rec4%print_output              = print_output 
   rec4%print_file                = print_file 
   rec4%print_tc                  = print_tc
   rec4%print_f77_info            = print_f77_info

   rec5%insert_bogus_storm        = insert_bogus_storm
   rec5%num_storm                 = num_storm
   rec5%latc_loc                  = latc_loc
   rec5%lonc_loc                  = lonc_loc
   rec5%vmax                      = vmax_meters_per_second
   rec5%rmax                      = rmax
   rec5%r_search                  = r_search
   rec5%r_vor                     = r_vor
   rec5%humidity_max              = humidity_max
   rec5%vmax_ratio                = vmax_ratio
   rec5%beta                      = beta
   rec5%devpc                     = devpc
   rec5%vert_variation            = vert_variation
   IF ( skip5 .GT. 0 ) THEN
      rec5%insert_bogus_storm = .FALSE.
   END IF

   !  We are finished with the NAMELIST file.

   CLOSE (UNIT=namelist_file)

   IF ( rec4%print_file ) THEN
      PRINT '(A,A/)' , 'CLOSE file: ',file_name_namelist
   END IF

   !  There are a few checks we can perfom on the input data.

   !  Did the user use a 4-digit year, it is required ya know.

   IF ( ( rec1%starting_date(1:2) .LT. '19' ) .OR. &
        ( rec1%starting_date(1:2) .GT. '20' ) .OR. &
        ( rec1%ending_date(1:2)   .LT. '19' ) .OR. &
        ( rec1%ending_date(1:2)   .GT. '20' ) ) THEN

      PRINT '(A)','The namelist dates require a 4 digit year that starts' 
      PRINT '(A)','with either "19" or "20".'
      STOP '4_digit_year'
   END IF

   !  Is the ending time before the starting time?

   IF ( rec1%ending_date .LT. rec1%starting_date ) THEN
      PRINT '(A,A,A,A,A)','The ending date ',rec1%ending_date,' is prior to the starting date ',rec1%starting_date,'.'
      STOP 'prior_ending_date'
   END IF

   !  Are any of the new pressure levels duplicated?

   DO i = 1 , rec2%number_of_new_levels - 1
      DO j = i+1 , rec2%number_of_new_levels 
         IF ( ABS ( rec2%new_levels(i) - rec2%new_levels(j) ) .LT. 1 ) THEN
            PRINT '(A)','Found two identical pressure levels in new_levels_in_Pa.'
            PRINT '(A,F10.1,A)','Remove one of the values of ',rec2%new_levels(i),'.'
            STOP 'duplicate_levels_in_namelist'
         END IF
      END DO
   END DO

   !  The input pressure arrays need to be in bottom-up order.

   DO i = 1 , rec2%number_of_new_levels - 1
      IF ( rec2%new_levels(i) .LT. rec2%new_levels(i+1) ) THEN
            PRINT '(A)','The pressure levels in the new_levels_in_Pa array must be ordered bottom up.'
            PRINT '(A,F10.1,A,F10.1,A)','The pressure ',rec2%new_levels(i),' is less than ',rec2%new_levels(i+1),'.'
            STOP 'out_of_order_levels_in_namelist'
      END IF
   END DO

   !  Did the gentle user ask for a new level which is geometrically above the
   !  top pressure?  If so, then the gentle user is in error and we should proceed
   !  with the assumption that notification is preferred to continuing down this
   !  seg-fault riddled path.

   IF ( rec2%number_of_new_levels .GT. 0 ) THEN
      IF ( rec2%ptop .GT. rec2%new_levels(rec2%number_of_new_levels) ) THEN
         PRINT '(A,F7.1,A,F7.1,A)','You asked for a PTOP_in_Pa, ',rec2%ptop, &
                                   ', which is above a new level, ',rec2%new_levels(rec2%number_of_new_levels),'.'
         PRINT '(A,F7.1,A,F7.1,A)','Either set the PTOP_in_Pa value to ',rec2%new_levels(rec2%number_of_new_levels), &
                                   ', or remove all new_level_in_Pa above ',rec2%ptop,'.'
         STOP 'PTOP_larger_than_new_levels'
      END IF
   END IF

   !  Print out a nice message letting folks know which particular time
   !  periods we plan to process in this program.  The current date is
   !  set to the requested starting_date, then it is incremented by the
   !  time interval specified in the namelist.  Once the incremented date
   !  is past the requested ending_date, we know we have reached the end
   !  of the number of time periods to process.  

   current_date = rec1%starting_date
   
   rec1%total_times = 0
   time_loop : DO WHILE ( current_date .LE. rec1%ending_date )

      rec1%total_times = rec1%total_times + 1
      IF ( rec4%print_echo ) THEN
         PRINT '(A,A)','Time to process:              ',current_date
      END IF

      CALL geth_newdate ( next_date , current_date , rec1%time_increment_second ) 

      current_date = next_date

   END DO time_loop

   IF ( rec4%print_echo ) THEN
      PRINT '(A,I8)','The total number of times periods to process (as given in the NAMELIST) is ',&
      rec1%total_times
   END IF

END SUBROUTINE proc_namelist
