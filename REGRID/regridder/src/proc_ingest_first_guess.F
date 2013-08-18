SUBROUTINE proc_ingest_first_guess

   USE constants
   USE diags
   USE first_guess_data
   USE file_data
   USE gauss
   USE gridded_data
   USE header_data
   USE horiz_interp
   USE link_list_info
   USE map_utils
   USE namelist_info
   USE terrain_data

   IMPLICIT NONE

   TYPE(proj_info) :: proj

   REAL , DIMENSION(:,:) , ALLOCATABLE :: array , array_backwards , &
                                          dum2d , longitude , latitude , tempgrd , landuse , terrain
   REAL :: xlo , yla , dlon , dlat , known_i , known_j
   INTEGER :: ilo , jla , i , j , icd , icd_old
   LOGICAL :: initial_fg_read = .TRUE.
   LOGICAL :: found_any_files = .FALSE.

   INTEGER :: close_count , i_close , j_close
   REAL :: close_data

   REAL     :: point_x              ! x location of point with unknown latitude, usually center
   REAL , DIMENSION (:,:), ALLOCATABLE :: xloc , yloc
   REAL     :: angle

   LOGICAL  :: converged , similar , wrap
   REAL     :: xlonc_mod

   CHARACTER (LEN=24) ::       hdate_input
   CHARACTER (LEN= 9) ::       field_input
   CHARACTER (LEN=25) ::       units_input
   CHARACTER (LEN=46) :: description_input

   LOGICAL  :: constant_file

   !  For each time period that we ingest, set up the different
   !  list of names.  This allows us to count how many 3d/2d fields
   !  we have encountered.

   all_names_sfc = '                                                                               '
   all_names_up  = '                                                                               '
   num3d_fg = 0
   num2d_fg = 0
   num1d_fg = 0

   !  Initialize these known_i and known_j values.  If this is a projected
   !  data set, these will be set.  If this is just your regular run of the
   !  mill lat/lon spacing, this is all we get.

   known_i = 1
   known_j = 1

   !  We know how big the model grid should be, so we now ALLOCATE it
   !  as the correct size array.  Note that this is done once per program
   !  run.
      
   initial_fg_read = .TRUE.
   IF ( initial_fg_read ) THEN
      ALLOCATE(dum2d(jxf,ixf))
      ALLOCATE(landuse(jxf,ixf))
      ALLOCATE(tempgrd(jxf,ixf))
      ALLOCATE(terrain(jxf,ixf))
      ALLOCATE(latitude(jxf,ixf))
      ALLOCATE(longitude(jxf,ixf))
      ALLOCATE(xloc(jxf,ixf))
      ALLOCATE(yloc(jxf,ixf))
      initial_fg_read = .FALSE.
   END IF

   !  Loop over the different files that are needed for each 
   !  time period.  For example, this could include different places to get
   !  SST if it was not included in the file with the other meteorological
   !  variables.

   found_any_files = .FALSE.
   diff_fg_files : DO diff_files_per_single_time = 1 , rec3%max_files_per_single_time + &
                                                       rec3%max_files_constants

      !  Get a UNIT number to OPEN.

      first_guess_file = -1
      first_guess_unit : DO file_index = file_index_min , file_index_max
         INQUIRE (UNIT=file_index, OPENED=already_opened)
         IF ( .NOT. already_opened) THEN
            first_guess_file = file_index
            EXIT first_guess_unit
         END IF
      END DO first_guess_unit
   
      IF ( first_guess_file .EQ. -1 ) THEN
         PRINT '(A)','Could not find a spare unit for the FIRST GUESS data.'
         STOP 'first_guess_file_unit'
      END IF
   
      !  F90 allows "as is" positioning, so make sure that does not
      !  slip in some how.

      REWIND ( UNIT=first_guess_file )
   
      !  Build the correct file name.  This is inside a loop which will
      !  allow a search of all possible "roots" of the file name.
      
      file_name = '                                                                                ' // &
                  '                                                    '
      IF ( diff_files_per_single_time .LE. rec3%max_files_constants ) THEN
         file_name_len = LEN_TRIM(rec3%constants_full_name(diff_files_per_single_time))
         file_name(1:file_name_len) = TRIM(rec3%constants_full_name(diff_files_per_single_time))
         IF (file_name(1:1) .EQ. ' ' ) THEN
            PRINT '(A)','Your constant file names may have "spaces" at the beginning.  Remove them.'
            STOP 'zap_the_spaces_in_const_filename'
         END IF
         constant_file = .TRUE.
         IF ( rec4%print_file ) THEN
            PRINT '(A,A)','Trying to OPEN constant file: ',TRIM(file_name)
         END IF
      ELSE
         file_name_len = LEN_TRIM(rec3%root(diff_files_per_single_time-rec3%max_files_constants))
         file_name(1:file_name_len) = TRIM(rec3%root(diff_files_per_single_time-rec3%max_files_constants))
         IF (file_name(1:1) .EQ. ' ' ) THEN
            PRINT '(A)','Your file names may have "spaces" at the beginning.  Remove them.'
            STOP 'zap_the_spaces_in_filename'
         END IF
         IF ( (rec1%time_increment_second/60)*60 .NE. rec1%time_increment_second ) THEN
            file_name(file_name_len+1:) = ':' // rec1%current_date(1:19)
         ELSE IF ( (rec1%time_increment_second/3600)*3600 .NE. rec1%time_increment_second ) THEN
            file_name(file_name_len+1:) = ':' // rec1%current_date(1:16)
         ELSE
            file_name(file_name_len+1:) = ':' // rec1%current_date(1:13)
         END IF
         constant_file = .FALSE.
         IF ( rec4%print_file ) THEN
            PRINT '(A,A)','Trying to OPEN first-guess file: ',TRIM(file_name)
         END IF
      END IF

      OPEN ( UNIT   = first_guess_file , &
             FILE   = TRIM(file_name)  , &
             STATUS = 'OLD'            , &
             FORM   = 'UNFORMATTED'    , &
             IOSTAT = open_status )
   
      IF ( open_status .NE. 0 ) THEN
         PRINT '(A,A)' , 'Error OPENing first_guess file: ',TRIM(file_name)
         CYCLE diff_fg_files
      ELSE
         found_any_files = .TRUE.
         PRINT '(A,A)' , 'Successfully OPENed first_guess file: ',TRIM(file_name)
      END IF
   
      !  These are some initializations to help speed up the interpolation
      !  if the domain requires the similar latitude longitude computation
      !  for each level, for each variable.

      idim_old     = -1
      jdim_old     = -1
      llflag_old   = -1
      lat1_old     = -1
      lon1_old     = -1
      deltax_old   = -1
      deltay_old   = -1
      xlonc_old    = -1
      truelat1_old = -1
      truelat2_old = -1
      icd_old      = -1
      icd          = -2
   
      !  This is the big loop over all of the data in this particular
      !  file.  There is an outer loop over different files.

      this_fg_file_only : DO
   
         !  Read header data from FIRST GUESS file.
      
         READ ( UNIT=first_guess_file , IOSTAT=eof_status) version

         !  Was this the end of the data set?
   
         IF ( eof_status .LT. 0 ) THEN
            PRINT '(A)', 'Found EOF in the FIRST GUESS file, exiting READ loop.'
            EXIT this_fg_file_only
         END IF

         !  Pull in another record of the header now that we know what the "format" will
         !  look like.  This "hdate" gets the "hdate_input" stuff is so that the CHARACTER
         !  strings all are multiples of full bytes.  We do not want to offend anyone.

         IF      ( version .EQ. 1 ) THEN
            PRINT '(A)','This is an old format (IVERSION=1), the character strings may be incorrect.'
            READ ( UNIT=first_guess_file ) &
            hdate_input,        field_input, units_input, description_input, level, idim, jdim, llflag
            xfcst = 0.
            hdate       =       hdate_input(1:19)
            field       =       field_input(1:8 )
            units       =       field_input(9:9 ) // units_input(1:15) // '        '
            description =       units_input(16:25) // description_input(1:36)
         ELSE IF ( version .EQ. 2 ) THEN
            READ ( UNIT=first_guess_file ) &
            hdate_input, xfcst, field_input, units_input, description_input, level, idim, jdim, llflag
            xfcst       =       xfcst * 60.
            hdate       =       hdate_input(1:19)
            field       =       field_input(1:8 )
            units       =       field_input(9:9 ) // units_input(1:15) // '        '
            description =       units_input(16:25) // description_input(1:36)
         ELSE IF ( version .EQ. 3 ) THEN
            READ ( UNIT=first_guess_file ) &
            hdate_input, xfcst, field_input, units_input, description_input, level, idim, jdim, llflag
            xfcst       =       xfcst * 60.
            hdate       =       hdate_input(1:19)
            field       =       field_input(1:8 )
            units       =       units_input(1:24)
            description = description_input(1:46)
         ELSE
            PRINT '(A)','This program fully supports IVERSION=3; less so on IVERSION=2; and IVERSION=1 is old.'
            PRINT '(A,I4,A)','You tried version ',version,'.  No can do.'
            STOP 'wrong_version_of_input'
         END IF

         !  We can test the llflag input as well.  We can only deal with a couple of
         !  projections currently.  Others will slide through, but be handled incorrectly
         !  without this test.
         !  Current choices are:
         !          0 - regular latitude/longitude projection
         !          1 - Mercator projection
         !          3 - Lambert conformal projection
         !          4 - Gaussian projection
         !          5 - polar stereographic projection
  
         IF ( ( llflag .NE. 0 ) .AND. &
              ( llflag .NE. 1 ) .AND. &
              ( llflag .NE. 3 ) .AND. &
              ( llflag .NE. 4 ) .AND. &
              ( llflag .NE. 5 ) ) THEN
            PRINT '(A)','This program can only handle data with specific projections.'
            PRINT '(A,I8,A)','The input data uses projection ',llflag,', which is not acceptable.'
            STOP 'Projection_type_unknown'   
         END IF

         IF ( rec4%print_echo ) THEN
            PRINT '(A,1X,F10.3,1X,A,1X,I8,2I4,I2)' , hdate, xfcst, field, NINT(level), idim, jdim, llflag
         END IF

         !  Was this the time that we expected to find?  We only care about this for first-guess
         !  fields that are not constant (the single input time would be assumed to be valid through
         !  the processing times requested).

         IF      ( ( hdate(1:13) .NE. rec1%current_date(1:13) ) .AND. ( .NOT. constant_file ) ) THEN
            PRINT '(A,A,A)','Expected to find date = ',rec1%current_date(1:13),' in the input file.'
            PRINT '(A,A,A)','Instead, the date ',hdate(1:13),' was in the header.'
            STOP 'Wrong_date_in_first_guess_header'
         ELSE IF   ( constant_file ) THEN
            hdate(1:19) = rec1%current_date(1:19)
            IF ( rec4%print_echo ) THEN
               PRINT '(A,A,A)','Constant file assumed valid for ',rec1%current_date(1:13),'.'
            END IF
         END IF

         !  We know enough info from this READ statement to increment the
         !  counters for the 3d/2d data.  This information is required on output
         !  in the header.  If this is a "new" variable name, we increment
         !  either the "sfc" or the "up" counter.  We can take care of
         !  duplicates at the end of this routine.

         IF ( ( NINT(level) .EQ. 201300 ) .OR. ( NINT(level) .EQ. 200100 ) ) THEN

            found_name = .FALSE.
            find_name_sfc : DO max_fg_variables_index = 1 , max_fg_variables
               IF ( all_names_sfc(max_fg_variables_index)(1:8) .EQ. '        ' ) THEN
                  all_names_sfc(max_fg_variables_index) = field // ' ' // units // ' ' // description
                  num2d_fg = num2d_fg + 1
                  found_name = .TRUE.
                  EXIT find_name_sfc
               ELSE IF ( field .EQ. all_names_sfc(max_fg_variables_index)(1:8) ) THEN
                  found_name = .TRUE.
                  EXIT find_name_sfc
               ELSE IF ( field .NE. all_names_sfc(max_fg_variables_index)(1:8) ) THEN
                  found_name = .FALSE.
               END IF 
            END DO find_name_sfc

            !  If we didn't find the name, something went wrong.

            IF ( .NOT. found_name ) THEN
               PRINT '(A)','Error in finding names for 3d/2d search for surface data.'
               STOP 'search_num2d'
            END IF

         END IF

         !  Same search in the upper-air data.

         IF ( NINT(level) .EQ. 100000 ) THEN

            found_name = .FALSE.
            find_name_up : DO max_fg_variables_index = 1 , max_fg_variables
               IF ( all_names_up(max_fg_variables_index)(1:8) .EQ. '        ' ) THEN
                  all_names_up(max_fg_variables_index) = field // ' ' // units // ' ' // description 
                  num3d_fg = num3d_fg + 1
                  found_name = .TRUE.
                  EXIT find_name_up
               ELSE IF ( field .EQ. all_names_up(max_fg_variables_index)(1:8) ) THEN
                  found_name = .TRUE.
                  EXIT find_name_up
               ELSE IF ( field .NE. all_names_up(max_fg_variables_index)(1:8) ) THEN
                  found_name = .FALSE.
               END IF 
            END DO find_name_up

            !  If we didn't find the name, something went wrong.

            IF ( .NOT. found_name ) THEN
               PRINT '(A)','Error in finding names for 3d/2d search for upper-air data.'
               STOP 'search_num3d'
            END IF

         END IF

         !  Continue READs, if the first couple lines are there, the rest of the lines
         !  are supposed to be there as well, so no error traps.  If this is a domain that
         !  is not constant latitude/longitude, then we need to eventually compute an 
         !  additional important value - namely the center latitude.
   
         IF      ( llflag .EQ. 0 ) THEN
            READ ( UNIT=first_guess_file ) startlat, startlon, deltalat, deltalon

            !  The startlon value needs to be consistent with the way that the program
            !  produces the gridded longitude field for the output domain.  The 
            !  longitudes will be between (inclusively) -180 and +180.

            IF ( ( startlon .GT. 180 ) .AND. ( startlon .LE. 360 ) ) THEN
               startlon = startlon - 360
            END IF

            IF ( rec4%print_echo ) THEN
               PRINT '(4F10.4)',           startlat, startlon, deltalat, deltalon
            END IF

            IF ( ( field(1:8) .EQ. 'U       ' ) .OR. &
                 ( field(1:8) .EQ. 'V       ' ) ) THEN
               wind_llflag   = llflag
            END IF

            known_i = 1
            known_j = 1

         ELSE IF ( llflag .EQ. 1 ) THEN
            READ ( UNIT=first_guess_file ) lat1, lon1, deltax, deltay, truelat1
            IF ( rec4%print_echo ) THEN
               PRINT '(6F10.4)',           lat1, lon1, deltax, deltay, truelat1
            END IF
            cone_fac = 0.0
            truelat2 = -99.

            angle = 360. * deltax * (idim-1) / &
                    ( 2. * pi * earth_radius_m/1000. * COS(truelat1*radians_per_degree) )
            lon2 = lon1 + angle
            IF ( lon2 .GT. 180 ) THEN
               lon2 = lon2 - 360
            END IF

            IF ( lon1 .LT. lon2 ) THEN
               xlonc = ( lon2 + lon1 ) / 2.
            ELSE
               xlonc = ( ( 360 + lon2 ) + lon1 ) / 2.
            END IF
            IF ( xlonc .GT. 180 ) THEN
               xlonc = xlonc - 360
            END IF
    
            !  Hey, was this the same as last time?  Can we avoid an expensive computation?
            !  What we want to find out is if this is the same projection information, we
            !  do not care about the specifics of the field or level.

            IF ( ( idim_old .EQ. idim ) .AND. (  jdim_old .EQ. jdim ) .AND. ( llflag_old .EQ. llflag ) .AND. &
                 ( lat1_old .EQ. lat1 ) .AND. ( lon1_old .EQ. lon1 ) .AND.  &
                 ( deltax_old .EQ. deltax ) .AND. ( deltay_old .EQ. deltay ) .AND. ( xlonc_old .EQ. xlonc ) .AND.  &
                 ( truelat1_old .EQ. truelat1 ) ) THEN
               similar = .TRUE.
            ELSE
               similar = .FALSE.
            END IF

            !  There are a few values that we need to compute from the available data.
 
            IF ( .NOT. similar ) THEN
               IF ( lon1  .LT. -180. ) lon1  = lon1  + 360.
               IF ( lon1  .GT.  180. ) lon1  = lon1  - 360.
               IF ( xlonc .LT. -180. ) xlonc = xlonc + 360.
               IF ( xlonc .GT.  180. ) xlonc = xlonc - 360.
               IF      ( ( deltax .GT. 0 ) .AND. ( deltay .GT. 0 ) ) THEN
                  known_i = 1.
                  known_j = 1.
               ELSE IF ( ( deltax .GT. 0 ) .AND. ( deltay .LT. 0 ) ) THEN
                  known_i = 1.
                  known_j = jdim
               ELSE IF ( ( deltax .LT. 0 ) .AND. ( deltay .GT. 0 ) ) THEN
                  known_i = idim
                  known_j = 1.
               ELSE IF ( ( deltax .LT. 0 ) .AND. ( deltay .LT. 0 ) ) THEN
                  known_i = idim
                  known_j = jdim
               END IF
if(nint(known_i*known_j).gt.1) then
stop 'fix_this_mercator'
endif
               CALL map_set(proj_merc,lat1,lon1,1.,1.,deltax*1000.,xlonc,truelat1,truelat2,proj)
            END IF
   
            IF ( ( field(1:8) .EQ. 'U       ' ) .OR. &
                 ( field(1:8) .EQ. 'V       ' ) ) THEN
               wind_llflag   = llflag
               wind_xlonc    = xlonc
               wind_xlatc    = xlatc
               wind_cone_fac = cone_fac
            END IF

         ELSE IF ( llflag .EQ. 3 ) THEN
            READ ( UNIT=first_guess_file ) lat1, lon1, deltax, deltay, xlonc, truelat1, truelat2
            IF ( rec4%print_echo ) THEN
               PRINT '(7F10.4)',           lat1, lon1, deltax, deltay, xlonc, truelat1, truelat2
            END IF
    
            !  Hey, was this the same as last time?  Can we avoid an expensive computation?
            !  What we want to find out is if this is the same projection information, we
            !  do not care about the specifics of the field or level.

            IF ( ( idim_old .EQ. idim ) .AND. (  jdim_old .EQ. jdim ) .AND. ( llflag_old .EQ. llflag ) .AND. &
                 ( lat1_old .EQ. lat1 ) .AND. ( lon1_old .EQ. lon1 ) .AND.  &
                 ( deltax_old .EQ. deltax ) .AND. ( deltay_old .EQ. deltay ) .AND. ( xlonc_old .EQ. xlonc ) .AND.  &
                 ( truelat1_old .EQ. truelat1 ) .AND. (  truelat2_old .EQ. truelat2 ) ) THEN
               similar = .TRUE.
            ELSE
               similar = .FALSE.
            END IF

            !  There are a few values that we need to compute from the available data.
 
            IF ( .NOT. similar ) THEN
               IF ( lon1  .LT. -180. ) lon1  = lon1  + 360.
               IF ( lon1  .GT.  180. ) lon1  = lon1  - 360.
               IF ( xlonc .LT. -180. ) xlonc = xlonc + 360.
               IF ( xlonc .GT.  180. ) xlonc = xlonc - 360.
               IF      ( ( deltax .GT. 0 ) .AND. ( deltay .GT. 0 ) ) THEN
                  known_i = 1.
                  known_j = 1.
               ELSE IF ( ( deltax .GT. 0 ) .AND. ( deltay .LT. 0 ) ) THEN
                  known_i = 1.
                  known_j = jdim
               ELSE IF ( ( deltax .LT. 0 ) .AND. ( deltay .GT. 0 ) ) THEN
                  known_i = idim
                  known_j = 1.
               ELSE IF ( ( deltax .LT. 0 ) .AND. ( deltay .LT. 0 ) ) THEN
                  known_i = idim
                  known_j = jdim
               END IF
if(nint(known_i*known_j).gt.1) then
stop 'fix_this_lambert'
endif
               CALL map_set(proj_lc,lat1,lon1,1.,1.,deltax*1000.,xlonc,truelat1,truelat2,proj)
            END IF
       
            IF ( ( field(1:8) .EQ. 'U       ' ) .OR. &
                 ( field(1:8) .EQ. 'V       ' ) ) THEN
               wind_llflag   = llflag
               wind_xlonc    = xlonc
               wind_xlatc    = xlatc
               wind_cone_fac = proj%cone
            END IF

         ELSE IF ( llflag .EQ. 4 ) THEN
            READ ( UNIT=first_guess_file ) startlat, startlon, deltalon , nlat

            !  The startlon value needs to be consistent with the way that the program
            !  produces the gridded longitude field for the output domain.  The 
            !  longitudes will be between (inclusively) -180 and +180.

            IF ( ( startlon .GT. 180 ) .AND. ( startlon .LE. 360 ) ) THEN
               startlon = startlon - 360
            END IF

            IF ( rec4%print_echo ) THEN
               PRINT '(4F10.4)',           startlat, startlon, deltalon , nlat
            END IF

            IF ( ( field(1:8) .EQ. 'U       ' ) .OR. &
                 ( field(1:8) .EQ. 'V       ' ) ) THEN
               wind_llflag   = llflag
            END IF

            known_i = 1
            known_j = 1

         ELSE IF ( llflag .EQ. 5 ) THEN
            READ ( UNIT=first_guess_file ) lat1, lon1, deltax, deltay, xlonc, truelat1

            !  If this is a *special* AFWA domain of seaice, fix the center longitude to
            !  what is considered an MM5 standard.  The specifics: it is a se ice field,
            !  it is polar stereographic, it has a negatice true lat, the (1,1) location
            !  is in the upper left hand corner.  Based on these, swap the central longitude.

            !  This replaces the mod sent to AFWA 23 April 2003.
            
            IF ( ( field(1:6) .EQ. 'SEAICE' ) .AND. &
                 ( truelat1 .LT. 0. ) .AND. &
                 ( deltax   .GT. 0. ) .AND. &
                 ( deltay   .LT. 0. ) )     THEN
               PRINT *,'AFWA OLD LONC=',xlonc
               xlonc = xlonc - 180.
            END IF

            IF ( rec4%print_echo ) THEN
               PRINT '(6F10.4)',           lat1, lon1, deltax, deltay, xlonc, truelat1
            END IF
            cone_fac = 1.0
            truelat2 = -99.
    
            !  Hey, was this the same as last time?  Can we avoid an expensive computation?
            !  What we want to find out is if this is the same projection information, we
            !  do not care about the specifics of the field or level.

            IF ( ( idim_old .EQ. idim ) .AND. (  jdim_old .EQ. jdim ) .AND. ( llflag_old .EQ. llflag ) .AND. &
                 ( lat1_old .EQ. lat1 ) .AND. ( lon1_old .EQ. lon1 ) .AND.  &
                 ( deltax_old .EQ. deltax ) .AND. ( deltay_old .EQ. deltay ) .AND. ( xlonc_old .EQ. xlonc ) .AND.  &
                 ( truelat1_old .EQ. truelat1 ) ) THEN
               similar = .TRUE.
            ELSE
               similar = .FALSE.
            END IF
   
            !  There are a few values that we need to compute from the available data.
 
            IF ( .NOT. similar ) THEN
               IF ( lon1  .LT. -180. ) lon1  = lon1  + 360.
               IF ( lon1  .GT.  180. ) lon1  = lon1  - 360.
               IF ( xlonc .LT. -180. ) xlonc = xlonc + 360.
               IF ( xlonc .GT.  180. ) xlonc = xlonc - 360.
               IF      ( ( deltax .GT. 0 ) .AND. ( deltay .GT. 0 ) ) THEN
                  known_i = 1.
                  known_j = 1.
               ELSE IF ( ( deltax .GT. 0 ) .AND. ( deltay .LT. 0 ) ) THEN
                  known_i = 1.
                  known_j = jdim
               ELSE IF ( ( deltax .LT. 0 ) .AND. ( deltay .GT. 0 ) ) THEN
                  known_i = idim
                  known_j = 1.
               ELSE IF ( ( deltax .LT. 0 ) .AND. ( deltay .LT. 0 ) ) THEN
                  known_i = idim
                  known_j = jdim
               END IF
               CALL map_set(proj_ps,lat1,lon1,known_i,known_j,ABS(deltax*1000.),xlonc,truelat1,truelat2,proj)
            END IF
       
            IF ( ( field(1:8) .EQ. 'U       ' ) .OR. &
                 ( field(1:8) .EQ. 'V       ' ) ) THEN
               wind_llflag   = llflag
               wind_xlonc    = xlonc
               wind_xlatc    = xlatc
               wind_cone_fac = cone_fac
            END IF

         END IF
      
         !  We know how big the input array will be, so we ALLOCATE the
         !  correct size array.  Since the array size can change, this is ALLOCATED
         !  and DEALLOCATEd for each READ statement.
      
         IF ( ( idim .NE. idim_old ) .OR. ( jdim .EQ. jdim_old ) ) THEN
            IF ( ALLOCATED ( array ) ) THEN
               DEALLOCATE ( array ) 
            END IF
            ALLOCATE (array(idim,jdim))
         END IF
      
         !  Read the 2d array.
      
         READ ( UNIT=first_guess_file ) array

         !  There are data sets that set HUGE POSITIVE flag values for masked fields.  Our 
         !  humble convention, of course, is to instead have HUGE NEGATIVE flags.  

         WHERE ( array .GT. 1.E19 )
            array = -1.E20
         END WHERE

         !  If the projected data is flipped upside down or side to side, then we'll
         !  just handle it here expeditiously.  I cannot fathom changing indices 
         !  below in each of the interpolation blocks just to handle this situation.  
         !  First, this is only if the data is projected (polar only right now), and 
         !  second it is only if the known_i or known_j values are not = 1.

         IF ( ( llflag .EQ. 5 ) .AND. &
              ( ( NINT(known_i) .NE. 1 ) .OR. ( NINT(known_j) .NE. 1 ) ) ) THEN

            IF ( ALLOCATED ( array_backwards ) ) THEN
               DEALLOCATE ( array_backwards ) 
            END IF
            ALLOCATE (array_backwards(idim,jdim))

            !  There are four possible origin locations that may be chosen for
            !  domain, let's call them A, B, C, and D.  If the origin is set as
            !  point A, then we do nothing (we are not even in this IF test!).
            !  We handle each of the three remaining flips.    

            !          C                             D
            !           -----------------------------
            !           |                           |
            !           |                           |
            !           |                           |
            !           |                           |
            !           |                           |
            !           |                           |
            !           |                           |
            !           |                           |
            !           |                           |
            !           |                           |
            !           |                           |
            !           -----------------------------
            !          A                             B

            !  Flip B : known_i = dim, known_j = 1

            IF      ( ( NINT(known_i) .EQ. idim ) .AND. ( NINT(known_j) .EQ.    1 ) ) THEN
               DO j = 1 , jdim
                  DO i = 1 , idim
                     array_backwards(idim-i+1,j) = array(i,j)
                  END DO
               END DO

            !  Flip C : known_i = 1, known_j = jdim

            ELSE IF ( ( NINT(known_i) .EQ.    1 ) .AND. ( NINT(known_j) .EQ. jdim ) ) THEN
               DO j = 1 , jdim
                  DO i = 1 , idim
                     array_backwards(i,jdim-j+1) = array(i,j)
                  END DO
               END DO

            !  Flip D : known_i = dim, known_j = jdim

            ELSE IF ( ( NINT(known_i) .EQ. idim ) .AND. ( NINT(known_j) .EQ. jdim ) ) THEN
               DO j = 1 , jdim
                  DO i = 1 , idim
                     array_backwards(idim-i+1,jdim-j+1) = array(i,j)
                  END DO
               END DO

            END IF

            !  Put the flipped data back in the array with which we wish to play.

            array = array_backwards

         END IF

         !  Some quicky print statements of the input data, just the middle column, 
         !  to bolster our confidence that this is being handled in a reasonable fashion.
      
         IF ( rec4%print_echo ) THEN
            PRINT '(8F10.1)',array(idim/2,1:jdim:5)
         END IF

         !  If this level is "higher up" than (physically above) the ptop defined in the 
         !  NAMELIST, go back to the start of this READ loop.  The data 
         !  for this entry has been completely ingested, and would now be
         !  overwritten by the next entry (if this is the case where the data is higher
         !  up than the requested ptop).  Note that this is after the 
         !  EOF tests, so no infinite loop happens.

         IF ( level .LT. rec2%ptop ) THEN

            IF ( rec4%print_interp) THEN
               PRINT '(A,F7.0,A,F7.0,A,A,A)','The NAMELIST ptop variable (',rec2%ptop, &
                                             ' Pa) requires that this level (',level,' Pa) of ', &
                                             field,' be omitted.'
            END IF

            !  Get to the bottom of the loop and get the next data from the file.

            CYCLE this_fg_file_only

         END IF

         !  If this is a surface pressure field, we do not want it.  The sea
         !  level pressure is important, the surface pressure is a function
         !  of the terrain elevation of the first guess data.  Besides being
         !  fairly useless, if we end up with two PRESSURE levels (sea level
         !  and surface), the darn program thinks that PRESSURE is a 3d field.

         IF ( ( NINT(level) .EQ. 200100 ) .AND. ( field .EQ. 'PRESSURE' ) ) THEN
            PRINT '(A)','Not processing surface pressure as a 2d array.'

            !  Get to the bottom of the loop and get the next data from the file.

            CYCLE this_fg_file_only
         END IF

         !  Version 2 of the masked tests:  
   
         !  Some of the first guess data sets have masked data.  If this
         !  is a masked field, interpolating has problems if one of the
         !  4 surrounding points is a flag value (typically zero - real helpful).
         !  So, check to see if this is a known masked field, and if so, 
         !  check to see if there are some flag values, and if so, then do some 
         !  simple things to see how we get the data.  The easiest case is when
         !  all 4 surrounding points are real data (tests #1 and #2), we just use 
         !  the real data (surprise!).  When there are no valid data values in the
         !  surrounding 4 points (tests #3 and #4), then we look to expand the 
         !  search to 16 points.  Failing that, we assign a fixed value (15 C for
         !  soil temperature, 0 for both soil moisture and sea ice).  The last tests
         !  are when there are some (not 4 and not 0) values to be used for the 
         !  masked interpolation (tests #5 and #6).  In this case we use as much
         !  data as is possible.  The last two tests (#7 and #8) are the null data
         !  cases for when we have a land grid point with a water based field (or
         !  a water point with a land based field).  These values are set to 0. 

         !  Version 3 of the tests:  

         !  If there is a location with sea ice, then we have to allow there to be 
         !  a soil temperature.  

         !  Version 4 of the tests:  

         !  Allow the TEMPGRD, if it exists, to help with islands.  If TEMPGRD is
         !  not available, use the function of the latitude for the soil temps.

         IF ( ( field(1:5) .EQ. 'SOILT'    ) .OR. &
              ( field(1:7) .EQ. 'SOILHGT'  ) .OR. &
              ( field(1:5) .EQ. 'SOILM'    ) .OR. &
              ( field(1:5) .EQ. 'SOILW'    ) .OR. &
              ( field(1:6) .EQ. 'MONALB'   ) .OR. &
              ( field(1:8) .EQ. 'ALBSNOMX' ) .OR. &
              ( field(1:3) .EQ. 'SEA'      ) .OR. &
              ( field(1:7) .EQ. 'CANOPYM'  ) .OR. &
              ( field(1:7) .EQ. 'LANDSEA'  ) .OR. &
              ( field(1:5) .EQ. 'SNOWH'    ) .OR. &
              ( field(1:5) .EQ. 'WEASD'    ) ) THEN

            !  Store the latitude, longitude, elevation, land use and the deep soil 
            !  temperature from the TERRAIN file into some more attractively named local 
            !  variables.

            latitude  = ter_data(latitude_cross_index)%array
            longitude = ter_data(longitude_cross_index)%array
            terrain   = ter_data(terrain_index)%array
            IF ( tempgrd_index .GT. 0 ) THEN
               tempgrd   = ter_data(tempgrd_index)%array
            END IF
            landuse   = ter_data(land_use_index)%array
            icd_old   = icd
            icd       = 1
    
            !  For the regular lat/lon grids, the longitude field needs to be
            !  massaged to allow correct horizontal interpolation across 
            !  Greenwich and the date line.

            IF ( llflag .EQ. 0 )THEN
               IF      ( deltalon .LT. 0 ) THEN
                  WHERE ( longitude .GT. startlon )
                     longitude = longitude - 360.
                  END WHERE
               ELSE IF ( deltalon .GT. 0 ) THEN
                  WHERE ( longitude .LT. startlon )
                     longitude = longitude + 360.
                  END WHERE
               END IF
            END IF
               
            !  Loop through each of the model grid points.

! pjj/cray
!$OMP parallel do default(shared) &
!$OMP private(i,j,yla,xlo,ilo,jla,dlon,dlat,xlonc_mod,deltalat,deltalon,close_count,close_data)
            mask_i_loop : DO i = 1, ixf
               mask_j_loop : DO j = 1, jxf

                  yla       = latitude(j,i)
                  xlo       = longitude(j,i)

                  !  Make the interpolation specific to the first guess input projection.

                  IF      ( llflag .EQ. 0 ) THEN
                     ilo       = MAX ( MIN(FLOOR((xlo-startlon)/deltalon) + 1, idim-1) , 1 )
                     jla       = MAX ( MIN(FLOOR((yla-startlat)/deltalat) + 1, jdim-1) , 1 )
                     dlon      = xlo - ( startlon + (ilo-1) * deltalon )
                     IF ( ABS(dlon) .GT. ABS(deltalon) ) THEN
                        PRINT '(A,I3,A,I3,A,A)','Extrapolating at point (',i,',',j,'), variable = ',field(1:8)
                        dlon   = 1.
                     END IF
                     dlat      = yla - ( startlat + (jla-1) * deltalat )

                  ELSE IF ( ( llflag .EQ. 1 ) .OR. ( llflag .EQ. 3 ) .OR. ( llflag .EQ. 5 ) ) THEN
                     IF      (xlonc .GT.  180 ) THEN
                        xlonc_mod = xlonc - 360.
                     ELSE IF (xlonc .LT. -180 ) THEN
                        xlonc_mod = xlonc + 360.
                     ELSE
                        xlonc_mod = xlonc
                     END IF
                     IF ( ( .NOT. similar ) .OR. ( icd .NE. icd_old ) ) THEN
                        IF ( ( llflag .EQ. 1 ) .OR. &
                             ( llflag .EQ. 3 ) .OR. &
                             ( llflag .EQ. 5 ) ) THEN
                           CALL latlon_to_ij(proj,yla,xlo,xloc(j,i) , yloc(j,i))
                        END IF
                        IF ( ( NINT(xloc(j,i)) .GT. idim ) .OR. &
                             ( NINT(yloc(j,i)) .GT. jdim ) .OR. &
                             ( NINT(xloc(j,i)) .LT.    1 ) .OR. &
                             ( NINT(yloc(j,i)) .LT.    1 ) ) THEN
                           PRINT '(A)','The chosen domain from the terrain data is not completely contained &
                                       &within the input first guess domain.' 
                           PRINT '(A,I8,A,A,A)','This is the ',NINT(level),' Pa level of ',field(1:8),'.'
                           STOP 'Model_domain_outside_first_guess_mask'
                        END IF
                     END IF
                     ilo       = MIN(FLOOR(xloc(j,i)) , idim-1)
                     jla       = MIN(FLOOR(yloc(j,i)) , jdim-1)
                     dlon      = xloc(j,i) - ilo
                     dlat      = yloc(j,i) - jla
                     deltalat  = 1.
                     deltalon  = 1.
                  END IF

                  !  Test #1a: No flag values, this is over land and this is a land field (specifically,
                  !  this is the soil temperature or the soil height).

                  IF      ( ( array(ilo  ,jla  ) .GT. big_negative ) .AND. & 
                            ( array(ilo+1,jla  ) .GT. big_negative ) .AND. & 
                            ( array(ilo  ,jla+1) .GT. big_negative ) .AND. & 
                            ( array(ilo+1,jla+1) .GT. big_negative ) .AND. &
!                           ( ter_data(land_mask_index)%array(j,i) .EQ. 1 ) .AND. & 
                            ( ( field(1:5) .EQ. 'SOILT'    ) .OR. ( field(1:7) .EQ. 'SOILHGT'  ) .OR. &
                              ( field(1:7) .EQ. 'CANOPYM'  ) .OR. ( field(1:7) .EQ. 'LANDSEA'  ) .OR. &
                              ( field(1:5) .EQ. 'SNOWH'    ) .OR. ( field(1:5) .EQ. 'WEASD'    ) .OR. &
                              ( field(1:6) .EQ. 'MONALB'   ) .OR. ( field(1:8) .EQ. 'ALBSNOMX' ) ) ) THEN
                     IF      ( ( ilo .GE. 1 ) .AND. ( jla .GE. 1 ) ) THEN
                        dum2d(j,i)= ( ( deltalon-dlon ) * ( ( deltalat-dlat ) * array ( ilo   , jla   )     &
                                    +                                  dlat   * array ( ilo   , jla+1 ) )   &
                                    +            dlon   * ( ( deltalat-dlat ) * array ( ilo+1 , jla   )     &
                                    +                                  dlat   * array ( ilo+1 , jla+1 ) ) ) &
                                    / ( deltalat * deltalon )
                     ELSE IF ( ( ilo .LT. 1 ) .AND. ( jla .GE. 1 ) ) THEN
                        dum2d(j,i)= array ( 1   , jla   )
                        PRINT '(A,I3,A,I3,A,A)','Extrapolating at point (',i,',',j,'), variable = ',field(1:8)
                     ELSE IF ( ( ilo .GE. 1 ) .AND. ( jla .LT. 1 ) ) THEN
                        dum2d(j,i)= array ( ilo , 1     )
                        PRINT '(A,I3,A,I3,A,A)','Extrapolating at point (',i,',',j,'), variable = ',field(1:8)
                     ELSE IF ( ( ilo .LT. 1 ) .AND. ( jla .LT. 1 ) ) THEN
                        dum2d(j,i)= array ( 1   , 1     )
                        PRINT '(A,I3,A,I3,A,A)','Extrapolating at point (',i,',',j,'), variable = ',field(1:8)
                     END IF
                  
                  !  Test #1b: No flag values, this is over land and this is a land field (specifically, this is
                  !  the soil moisture).  A check is set to make sure that no value larger than 0.5 gets into
                  !  the average.

                  ELSE IF ( ( array(ilo  ,jla  ) .GT. big_negative ) .AND. & 
                            ( array(ilo  ,jla  ) .LE. 0.5            ) .AND. & 
                            ( array(ilo+1,jla  ) .GT. big_negative ) .AND. & 
                            ( array(ilo+1,jla  ) .LE. 0.5            ) .AND. & 
                            ( array(ilo  ,jla+1) .GT. big_negative ) .AND. & 
                            ( array(ilo  ,jla+1) .LE. 0.5            ) .AND. & 
                            ( array(ilo+1,jla+1) .GT. big_negative ) .AND. &
                            ( array(ilo+1,jla+1) .LE. 0.5            ) .AND. &
                            ( ter_data(land_mask_index)%array(j,i) .EQ. 1  ) .AND. & 
                            ( ( field(1:5) .EQ. 'SOILM' ) .OR. ( field(1:5) .EQ. 'SOILW' ) ) ) THEN
                     IF      ( ( ilo .GE. 1 ) .AND. ( jla .GE. 1 ) ) THEN
                        dum2d(j,i)= ( ( deltalon-dlon ) * ( ( deltalat-dlat ) * array ( ilo   , jla   )     &
                                    +                                  dlat   * array ( ilo   , jla+1 ) )   &
                                    +            dlon   * ( ( deltalat-dlat ) * array ( ilo+1 , jla   )     &
                                    +                                  dlat   * array ( ilo+1 , jla+1 ) ) ) &
                                    / ( deltalat * deltalon )
                     ELSE IF ( ( ilo .LT. 1 ) .AND. ( jla .GE. 1 ) ) THEN
                        dum2d(j,i)= array ( 1   , jla   )
                        PRINT '(A,I3,A,I3,A,A)','Extrapolating at point (',i,',',j,'), variable = ',field(1:8)
                     ELSE IF ( ( ilo .GE. 1 ) .AND. ( jla .LT. 1 ) ) THEN
                        dum2d(j,i)= array ( ilo , 1     )
                        PRINT '(A,I3,A,I3,A,A)','Extrapolating at point (',i,',',j,'), variable = ',field(1:8)
                     ELSE IF ( ( ilo .LT. 1 ) .AND. ( jla .LT. 1 ) ) THEN
                        dum2d(j,i)= array ( 1   , 1     )
                        PRINT '(A,I3,A,I3,A,A)','Extrapolating at point (',i,',',j,'), variable = ',field(1:8)
                     END IF
                  
                  !  Test #1c: This is where the soil moisture is set to 1.0 over the water.

                  ELSE IF ( ( ter_data(land_mask_index)%array(j,i) .EQ. 0       ) .AND. & 
                            ( ( field(1:5) .EQ. 'SOILM' ) .OR. ( field(1:5) .EQ. 'SOILW' ) ) ) THEN
                     dum2d(j,i) = 1.
                  
                  !  Test #2: No flag values, this is over water and this is a water field.
   
                  ELSE IF ( ( array(ilo  ,jla  ) .GT. big_negative ) .AND. & 
                            ( array(ilo+1,jla  ) .GT. big_negative ) .AND. & 
                            ( array(ilo  ,jla+1) .GT. big_negative ) .AND. & 
                            ( array(ilo+1,jla+1) .GT. big_negative ) .AND. &
                            ( ter_data(land_mask_index)%array(j,i) .EQ. 0 )  .AND. &
                            ( field(1:6) .EQ. 'SEAICE'   ) ) THEN
                     IF      ( ( ilo .GE. 1 ) .AND. ( jla .GE. 1 ) ) THEN
                        dum2d(j,i)= ( ( deltalon-dlon ) * ( ( deltalat-dlat ) * array ( ilo   , jla   )     &
                                    +                                  dlat   * array ( ilo   , jla+1 ) )   &
                                    +            dlon   * ( ( deltalat-dlat ) * array ( ilo+1 , jla   )     &
                                    +                                  dlat   * array ( ilo+1 , jla+1 ) ) ) &
                                    / ( deltalat * deltalon )
                     ELSE IF ( ( ilo .LT. 1 ) .AND. ( jla .GE. 1 ) ) THEN
                        dum2d(j,i)= array ( 1   , jla   )
                        PRINT '(A,I3,A,I3,A,A)','Extrapolating at point (',i,',',j,'), variable = ',field(1:8)
                     ELSE IF ( ( ilo .GE. 1 ) .AND. ( jla .LT. 1 ) ) THEN
                        dum2d(j,i)= array ( ilo , 1     )
                        PRINT '(A,I3,A,I3,A,A)','Extrapolating at point (',i,',',j,'), variable = ',field(1:8)
                     ELSE IF ( ( ilo .LT. 1 ) .AND. ( jla .LT. 1 ) ) THEN
                        dum2d(j,i)= array ( 1   , 1     )
                        PRINT '(A,I3,A,I3,A,A)','Extrapolating at point (',i,',',j,'), variable = ',field(1:8)
                     END IF

                  !  Test #3: All flag values, this is over land and this is a land field.

                  ELSE IF ( ( array(ilo  ,jla  ) .LT. big_negative ) .AND. & 
                            ( array(ilo+1,jla  ) .LT. big_negative ) .AND. & 
                            ( array(ilo  ,jla+1) .LT. big_negative ) .AND. & 
                            ( array(ilo+1,jla+1) .LT. big_negative ) .AND. &
!                           ( ter_data(land_mask_index)%array(j,i) .EQ. 1 ) .AND. &
                            ( ( field(1:5) .EQ. 'SOILT'    ) .OR. ( field(1:7) .EQ. 'SOILHGT'  ) .OR. &
                              ( field(1:6) .EQ. 'MONALB'   ) .OR. ( field(1:8) .EQ. 'ALBSNOMX' ) .OR. &
                              ( field(1:7) .EQ. 'CANOPYM'  ) .OR. ( field(1:7) .EQ. 'LANDSEA'  ) .OR. &
                              ( field(1:5) .EQ. 'SNOWH'    ) .OR. ( field(1:5) .EQ. 'WEASD'    ) ) ) THEN

                     !  Check to see how many values of real data are within the surrounding
                     !  16 point square.

                     close_count = 0 
                     close_data  = 0
                     DO j_close = MAX (1,jla-1) , MIN(jdim,jla+2)                  
                        DO i_close = MAX (1,ilo-1) , MIN(idim,ilo+2)                  
                           IF ( array(i_close,j_close) .GT. big_negative ) THEN
                              close_count = close_count + 1
                              close_data = close_data + array(i_close,j_close)
                           END IF
                        END DO
                     END DO

                     !  Test #3a: There is someone close to use as data.  Use the average of all
                     !  of the available (yet distant) data.

                     !  But wait, if this is the albedo field, and we are over water, who CARES
                     !  how many points are valid, we set this to a default value of 8%.

                     IF      ( ( close_count .GT. 0 ) .AND. ( field(1:6) .EQ. 'MONALB'   ) .AND. &
                               ( ter_data(land_mask_index)%array(j,i) .EQ. 0 ) ) THEN
                        dum2d(j,i) = 8.

                     ELSE IF ( ( close_count .GT. 0 ) .AND. ( field(1:8) .EQ. 'ALBSNOMX' ) .AND. &
                               ( ter_data(land_mask_index)%array(j,i) .EQ. 0 ) ) THEN
                        dum2d(j,i) = 8.

                     ELSE IF ( ( close_count .GT. 0 ) .AND. &
                               ( ( field(1:7) .EQ. 'CANOPYM'  ) .OR. ( field(1:7) .EQ. 'LANDSEA'  ) .OR. &
                                 ( field(1:5) .EQ. 'SNOWH'    ) .OR. ( field(1:5) .EQ. 'WEASD'    ) ) .AND. &
                               ( ter_data(land_mask_index)%array(j,i) .EQ. 0 ) ) THEN
                        dum2d(j,i) = 0.

                     ELSE IF ( close_count .GT. 0 ) THEN

                        dum2d(j,i) = close_data / REAL ( close_count) 
                        IF ( rec4%print_mask ) THEN
                           PRINT '(A,A,A,I4,A,I4,A)', &
                           'Island-type value for ',field(1:8),' found for (j,i)=(',i,',',j,').'
                        END IF

                     !  Test #3b: There is nobody close to this grid point, and it is soil temperature.
                     !  There is a predetermined value of soil temperature that is used which is a 
                     !  function of latitude.

                     ELSE IF ( ( close_count .EQ. 0 ) .AND. ( field(1:5) .EQ. 'SOILT' ) ) THEN

                        IF ( ( landuse(j,i) .NE. bhi(23,1) ) .AND. ( tempgrd_index .GT. 0 ) ) THEN
                           dum2d(j,i) = tempgrd(j,i) + 0.0065 * terrain(j,i)
                        ELSE
                           CALL missing_soil ( yla , dum2d(j,i) , terrain(j,i) )
                           dum2d(j,i) = dum2d(j,i) + 0.0065 * terrain(j,i)
                        END IF

                     !  Test #3c: There is nobody close to this grid point, and it is the soil
                     !  height.  Guess what, our pre-determined value here is just the terrain
                     !  elevation.  Why do this?  The soil temps are going to be adjusted later based
                     !  on the difference in elevation between the soil height and the model's elevation.
                     !  If we make them identical in this island location, then the adjustment is zero.
                     !  Since the annual-mean, deep-soil temperature is already adjusted to the 
                     !  elevation, we are set.

                     ELSE IF ( ( close_count .EQ. 0 ) .AND. ( field(1:7) .EQ. 'SOILHGT' ) ) THEN
                        dum2d(j,i) = terrain(j,i)

                     !  This is an island type situation, and we are setting the maximum albedo.
                     !  Very few sub-grid scale islands are covered with snow, so let's just set
                     !  a value of 16% for each month and a max of 60%.  If we are over water,
                     !  set the albedo to 8%.

                     ELSE IF ( ( close_count .EQ. 0 ) .AND. ( field(1:6) .EQ. 'MONALB'   ) .AND. &
                               ( ter_data(land_mask_index)%array(j,i) .EQ. 1 ) ) THEN
                        dum2d(j,i) = 16.

                     ELSE IF ( ( close_count .EQ. 0 ) .AND. ( field(1:6) .EQ. 'MONALB'   ) .AND. &
                               ( ter_data(land_mask_index)%array(j,i) .EQ. 0 ) ) THEN
                        dum2d(j,i) = 8.

                     ELSE IF ( ( close_count .EQ. 0 ) .AND. ( field(1:8) .EQ. 'ALBSNOMX' ) .AND. &
                               ( ter_data(land_mask_index)%array(j,i) .EQ. 1 ) ) THEN
                        dum2d(j,i) = 60.

                     ELSE IF ( ( close_count .EQ. 0 ) .AND. ( field(1:8) .EQ. 'ALBSNOMX' ) .AND. &
                               ( ter_data(land_mask_index)%array(j,i) .EQ. 0 ) ) THEN
                        dum2d(j,i) = 8.

                     ELSE IF ( ( close_count .EQ. 0 ) .AND. ( field(1:7) .EQ. 'LANDSEA'  ) .AND. &
                               ( ter_data(land_mask_index)%array(j,i) .EQ. 1 ) ) THEN
                        dum2d(j,i) = 1.

                     ELSE IF ( ( close_count .EQ. 0 ) .AND. &
                               ( ( field(1:7) .EQ. 'CANOPYM'  ) .OR. &
                                 ( field(1:5) .EQ. 'SNOWH'    ) .OR. ( field(1:5) .EQ. 'WEASD'    ) ) .AND. &
                               ( ter_data(land_mask_index)%array(j,i) .EQ. 1 ) ) THEN
                        dum2d(j,i) = 0.

                     ELSE IF ( ( close_count .EQ. 0 ) .AND. &
                               ( ( field(1:7) .EQ. 'CANOPYM'  ) .OR. ( field(1:7) .EQ. 'LANDSEA'  ) .OR. &
                                 ( field(1:5) .EQ. 'SNOWH'    ) .OR. ( field(1:5) .EQ. 'WEASD'    ) ) .AND. &
                               ( ter_data(land_mask_index)%array(j,i) .EQ. 0 ) ) THEN
                        dum2d(j,i) = 0.

                     END IF

                  !  Test #3.1: All flag values, this is over land and this is a land field, specifically,
                  !  a test for soil moisture.

                  ELSE IF ( ( ( array(ilo  ,jla  ) .LE. big_negative ) .OR. ( array(ilo  ,jla  ) .GE. 0.9 ) ) .AND. & 
                            ( ( array(ilo+1,jla  ) .LE. big_negative ) .OR. ( array(ilo+1,jla  ) .GE. 0.9 ) ) .AND. & 
                            ( ( array(ilo  ,jla+1) .LE. big_negative ) .OR. ( array(ilo  ,jla+1) .GE. 0.9 ) ) .AND. & 
                            ( ( array(ilo+1,jla+1) .LE. big_negative ) .OR. ( array(ilo+1,jla+1) .GE. 0.9 ) ) .AND. & 
                            ( ter_data(land_mask_index)%array(j,i) .EQ. 1 ) .AND. &
                            ( ( field(1:5) .EQ. 'SOILM' ) .OR. ( field(1:5) .EQ. 'SOILW' ) ) ) THEN

                     !  Check to see how many values of real data are within the surrounding
                     !  16 point square.

                     close_count = 0 
                     close_data  = 0
                     DO j_close = MAX (1,jla-1) , MIN(jdim,jla+2)                  
                        DO i_close = MAX (1,ilo-1) , MIN(idim,ilo+2)                  
                           IF      ( ( array(i_close,j_close) .GT. big_negative ) .AND. &
                                     ( array(i_close,j_close) .LT. 0.5 ) ) THEN
                              close_count = close_count + 1
                              close_data = close_data + array(i_close,j_close)
                           ELSE IF ( ( array(i_close,j_close) .GE. 0.5            ) .AND. &
                                     ( array(i_close,j_close) .LT. 0.9 ) ) THEN
                              close_count = close_count + 1
                              close_data = close_data + 0.3
                           END IF
                        END DO
                     END DO

                     !  Test #3.1a: There is someone close to use as data.  Use the average of all
                     !  of the available (yet distant) data.

                     IF      ( close_count .GT. 0 ) THEN

                        dum2d(j,i) = close_data / REAL ( close_count) 
                        IF ( rec4%print_mask ) THEN
                           PRINT '(A,A,A,I4,A,I4,A)', &
                           'Island-type value for ',field(1:8),' found for (j,i)=(',i,',',j,').'
                        END IF

                     !  Test #3.1b: There is nobody close to this grid point, and it is soil moisture.
                     !  There is a predetermined value of 0.3 that is used.

                     ELSE IF ( close_count .EQ. 0 ) THEN

                        dum2d(j,i) = 0.3
                        IF ( rec4%print_mask ) THEN
                           PRINT '(A,I4,A,I4,A)', &
                           'Warning: Had to put in a bogus moisture value at (j,i) = (',i,',',j,').'
                        END IF
         
                     END IF

                  !  Test #4: All flag values, this is over water and this is a water field, a test for
                  !  the seaice field.

                  ELSE IF ( ( array(ilo  ,jla  ) .LT. big_negative ) .AND. & 
                            ( array(ilo+1,jla  ) .LT. big_negative ) .AND. & 
                            ( array(ilo  ,jla+1) .LT. big_negative ) .AND. & 
                            ( array(ilo+1,jla+1) .LT. big_negative ) .AND. &
                            ( ter_data(land_mask_index)%array(j,i) .EQ. 0 ) .AND. &
                            ( field(1:6) .EQ. 'SEAICE'   ) ) THEN

                     !  Is anyone within an additional grid point?

                     close_count = 0 
                     close_data      = 0
                     DO j_close = MAX (1,jla-1) , MIN(jdim,jla+2)                  
                        DO i_close = MAX (1,ilo-1) , MIN(idim,ilo+2)                  
                           IF ( array(i_close,j_close) .GT. big_negative ) THEN
                              close_count = close_count + 1
                              close_data = close_data + array(i_close,j_close)
                           END IF
                        END DO
                     END DO

                     !  Test #4a: There is someone close to use as data.

                     IF      ( close_count .GT. 0 ) THEN

                        dum2d(j,i) = close_data / REAL ( close_count) 
                        IF ( rec4%print_mask ) THEN
                           PRINT '(A,A,A,I4,A,I4,A)', &
                           'Lake-type value for ',field(1:8),' found for (j,i)=(',i,',',j,').'
                        END IF

                     !  Test #4b: There are no close points to use as data for the sea ice.

                     ELSE IF ( ( close_count .EQ. 0 ) .AND. ( field(1:6) .EQ. 'SEAICE'   ) ) THEN

                        dum2d(j,i) = 0
                        IF ( rec4%print_mask ) THEN
                           PRINT '(A,I4,A,I4,A)', &
                           'Warning: Had to put in a bogus sea ice value at (j,i) = (',i,',',j,').'
                        END IF

                     END IF

                  !  Test #5a: Some flag values, this is over land and this is a land field, for either
                  !  soil temperature or soil height.

                  ELSE IF ( ( ( array(ilo  ,jla  ) .LT. big_negative )   .OR. & 
                              ( array(ilo+1,jla  ) .LT. big_negative )   .OR. & 
                              ( array(ilo  ,jla+1) .LT. big_negative )   .OR. & 
                              ( array(ilo+1,jla+1) .LT. big_negative ) ) .AND. &
!                             ( ter_data(land_mask_index)%array(j,i) .EQ. 1 ) .AND. &
                              ( ( field(1:5) .EQ. 'SOILT'    ) .OR. ( field(1:7) .EQ. 'SOILHGT'  ) .OR. &
                                ( field(1:6) .EQ. 'MONALB'   ) .OR. ( field(1:8) .EQ. 'ALBSNOMX' ) .OR. &
                                ( field(1:7) .EQ. 'CANOPYM'  ) .OR. ( field(1:7) .EQ. 'LANDSEA'  ) .OR. &
                                ( field(1:5) .EQ. 'SNOWH'    ) .OR. ( field(1:5) .EQ. 'WEASD'    ) ) ) THEN

                     !  Count how many real data points exist in the surrounding nearest
                     !  box of 4 points.  It has to be at least 1 since there was already a test
                     !  for zero real-data points.

                     close_count = 0 
                     close_data  = 0
                     DO j_close = MAX (1,jla) , MIN(jdim,jla+1)                  
                        DO i_close = MAX (1,ilo) , MIN(idim,ilo+1)                  
                           IF ( array(i_close,j_close) .Ge. big_negative ) THEN
                              close_count = close_count + 1
                              close_data = close_data + array(i_close,j_close)
                           END IF
                        END DO
                     END DO

                     dum2d(j,i) = close_data / REAL ( close_count) 
                     IF ( rec4%print_mask ) THEN
                        PRINT '(A,A,A,I4,A,I4,A)', &
                        'Coastal-type value for ',field(1:8),' found for (j,i)=(',i,',',j,').'
                     END IF

                  !  Test #5b: Some flag values, this is over land and this is a land field, specifically, this is the
                  !  coastal areas for soil moisture.

                  ELSE IF ( ( ter_data(land_mask_index)%array(j,i) .EQ. 1 )  .AND. &
                            ( ( field(1:5) .EQ. 'SOILM' ) .OR. ( field(1:5) .EQ. 'SOILW' ) ) ) THEN

                     !  Count how many real data points exist in the surrounding nearest
                     !  box of 4 points.  It has to be at least 1 since there was already a test
                     !  for zero real-data points.

                     close_count = 0 
                     close_data  = 0
                     DO j_close = MAX (1,jla) , MIN(jdim,jla+1)                  
                        DO i_close = MAX (1,ilo) , MIN(idim,ilo+1)                  
                           IF      ( ( array(i_close,j_close) .GE. big_negative ) .AND. &
                                ( array(i_close,j_close)       .LE. 0.5            ) ) THEN
                              close_count = close_count + 1
                              close_data = close_data + array(i_close,j_close)
                           ELSE IF ( ( array(i_close,j_close) .GT. 0.5            ) .AND. &
                                ( array(i_close,j_close)       .LE. 0.9            ) ) THEN
                              close_count = close_count + 1
                              close_data = close_data + 0.3
                           END IF
                        END DO
                     END DO

                     dum2d(j,i) = close_data / REAL ( close_count) 
                     IF ( rec4%print_mask ) THEN
                        PRINT '(A,A,A,I4,A,I4,A)', &
                        'Coastal-type value for ',field(1:8),' found for (j,i)=(',i,',',j,').'
                     END IF

                  !  Test #6: Some flag values, this is over water and this is a water field, specifically seaice.

                  ELSE IF ( ( ( array(ilo  ,jla  ) .LT. big_negative )   .OR. & 
                              ( array(ilo+1,jla  ) .LT. big_negative )   .OR. & 
                              ( array(ilo  ,jla+1) .LT. big_negative )   .OR. & 
                              ( array(ilo+1,jla+1) .LT. big_negative ) ) .AND. &
                              ( ter_data(land_mask_index)%array(j,i) .EQ. 0 ) .AND. &
                              ( field(1:3) .EQ. 'SEA' ) ) THEN

                     !  Count how many real data points exist in the surrounding nearest
                     !  box of 4 points.  It has to be at least 1 since there was already a test
                     !  for zero real-data points for sea ice above.

                     close_count = 0 
                     close_data  = 0
                     DO j_close = MAX (1,jla) , MIN(jdim,jla+1)                  
                        DO i_close = MAX (1,ilo) , MIN(idim,ilo+1)                  
                           IF ( array(i_close,j_close) .GT. big_negative ) THEN
                              close_count = close_count + 1
                              close_data = close_data + array(i_close,j_close)
                           END IF
                        END DO
                     END DO

                     dum2d(j,i) = close_data / REAL ( close_count) 
                     IF ( rec4%print_mask ) THEN
                        PRINT '(A,A,A,I4,A,I4,A)', &
                        'Coastal-type value for ',field(1:8),' found for (j,i)=(',i,',',j,').'
                     END IF

!  Mask test version #2: This is shut off to allow soil temps under the sea ice.  Who'd'a thunk?
!
!                 !  Test #7: This is a land field and this is over water.
! 
!                 ELSE IF ( ( ter_data(land_mask_index)%array(j,i) .EQ. 0 ) .AND. &
!                           ( ( field(1:4) .EQ. 'SOIL' ) ) ) THEN
!
!                    dum2d(j,i) = 0

                  !  Test #8: This is a water field and this is over land.

                  ELSE IF ( ( ter_data(land_mask_index)%array(j,i) .EQ. 1 ) .AND. &
                            ( ( field(1:6) .EQ. 'SEAICE'   ) ) ) THEN
                     dum2d(j,i) = 0

                  ELSE
                     PRINT '(A,A,A,2I4,A)','You dropped through the IF tests for the masked interpolation: ',field(1:8),' at (j,i)=',j,i,'.'
                     PRINT '(A)','This is a situation to which attention needs to be given.'
                     STOP 'Drop_through_mask_interpolation'

                  END IF

               END DO mask_j_loop
            END DO mask_i_loop

         !  If the data do not assume any sort of masking, we can do a 4 point interpolation.  
         !  This guy is considerably easier to follow.

         ELSE

            !  Store the latitude and longitude from the TERRAIN file into some
            !  more attractively named local variables.  We can split off the dot and cross
            !  point variables for the interpolation locations.
    
            IF      ( ( field(1:8) .EQ. 'U       ' ) .OR. &
                      ( field(1:8) .EQ. 'V       ' ) ) THEN
               latitude  = ter_data(latitude_dot_index)%array
               longitude = ter_data(longitude_dot_index)%array
               icd_old   = icd
               icd       = 0
    
            ELSE
               latitude  = ter_data(latitude_cross_index)%array
               longitude = ter_data(longitude_cross_index)%array
               icd_old   = icd
               icd       = 1
          
            END IF
    
            !  For the regular lat/lon grids, the longitude field needs to be
            !  massaged to allow correct horizontal interpolation across 
            !  Greenwich and the date line.

            IF ( llflag .EQ. 0 )THEN
               IF      ( deltalon .LT. 0 ) THEN
                  WHERE ( longitude .GT. startlon )
                     longitude = longitude - 360.
                  END WHERE
               ELSE IF ( deltalon .GT. 0 ) THEN
                  WHERE ( longitude .LT. startlon )
                     longitude = longitude + 360.
                  END WHERE
               END IF
            END IF

            !  Loop through each of the model grid points for the unmasked fields.
         
! pjj/cray
!$OMP parallel do default(shared) &
!$OMP private(i,j,yla,xlo,ilo,jla,dlon,dlat,xlonc_mod,deltalat,deltalon,wrap,deltalon_save)
            interpolate_i_loop : DO i = 1, ixf
               interpolate_j_loop : DO j = 1, jxf
         
                  !  Calculate values of first guess at the model grid point locations:
                  !  note that i is in west-east (or x) direction, and j in south-north
                  !  (or y) direction.  This is a weighted mean in both the x and y
                  !  directions.  The interpolation is done in latitude/longitude space
                  !  for a regular latitude longitude projection, and the interpolation
                  !  uses grid point space for a Lambert or polar stereographic projection.
                  
                  !  For the regular latitude longitude projection:
                  !  The latitude/longitude of each of the grid points in the regular
                  !  lat/lon grid is known: (startlat/deltalat, startlon/deltalon).  The
                  !  lat/lon of each model grid point has been previously computed.

                  !  For the Lambert and polar stereographic projections: 
                  !  The individual latitude and longitude of the eventual output domain 
                  !  are used to compute the (i,j) index into the input array for interpolation.
                 
                  !
                  !                    (ilo,jla+1)   (ilo+1,jla+1)
                  !               -        -------------
                  ! deltalat-dlat |        |           |
                  !               |        |           |
                  !               -        |  *        |
                  !          dlat |        | (i,j)     |
                  !               |        |           |
                  !               -        -------------
                  !                    (ilo,jla)     (ilo+1,jla)
                  !
                  !                        |--|--------|
                  !                       dlon  deltalon-dlon
            
                  yla       = latitude(j,i)
                  xlo       = longitude(j,i)

                  !  Make the interpolation specific to the first guess input projection.  For
                  !  a regular latitude longitude projection, the set of values (deltalat,
                  !  deltalon, dlat, dlon, startlat, startlon, xlo and yla) are latitudes and
                  !  longitudes.  The size of the above grid box is deltalat by deltalon.  For 
                  !  the Lambert and polar projections, the set of values (dlat, dlon, deltalat 
                  !  and deltalon) are grid point values.  The above grid box is then assumed to 
                  !  be 1 grid unit on each side.

                  wrap = .FALSE.
                  IF      ( llflag .EQ. 0 ) THEN
                     xloc(j,i) = (xlo-startlon)/deltalon + 1.
                     yloc(j,i) = (yla-startlat)/deltalat + 1.
                     ilo       = MIN(FLOOR((xlo-startlon)/deltalon) + 1, idim-1)
                     jla       = MIN(FLOOR((yla-startlat)/deltalat) + 1, jdim-1)
                     dlon      = xlo - ( startlon + (ilo-1) * deltalon )
                     IF      ( ( deltalon .GT. 0 ) .AND. &
                               ( FLOOR((xlo-startlon)/deltalon) + 1 .GE. idim ) .AND. &
                               ( xlo + deltalon .GE. startlon + 360 ) ) THEN
                        IF ( rec4%print_interp) THEN
                           PRINT '(A,I3,A,I3,A,A)','Wrapping at point (',i,',',j,'), variable = ',field(1:8)
                        END IF
                        dlon      = xlo - ( startlon + (idim-1) * deltalon )
                        wrap = .TRUE.
                        ilo = idim
                     ELSE IF ( ( deltalon .LT. 0 ) .AND. &
                               ( FLOOR((xlo-startlon)/deltalon) + 1 .GE. idim ) .AND. &
                               ( xlo + deltalon .LE. startlon - 360 ) ) THEN
                        IF ( rec4%print_interp) THEN
                           PRINT '(A,I3,A,I3,A,A)','Wrapping at point (',i,',',j,'), variable = ',field(1:8)
                        END IF
                        dlon      = xlo - ( startlon + (idim-1) * deltalon )
                        wrap = .TRUE.
                        ilo = idim
                     ELSE IF ( ABS(dlon) .GT. ABS(deltalon) ) THEN
                        PRINT '(A,I3,A,I3,A,A)','Regular lat/lon horizontal extrapolation at point (',i,',',j,'), variable = ',field(1:8)
                        dlon   = 1.
                     END IF
                     dlat      = yla - ( startlat + (jla-1) * deltalat )

                  ELSE IF ( llflag .EQ. 4 ) THEN
                     CALL llxy_gauss( yla , xlo , xloc(j,i) , yloc(j,i) , wrap , &
                                      idim , startlat , startlon , deltalon , nlat )

                     IF ( .NOT. wrap ) THEN
                        ilo       = MIN(FLOOR(xloc(j,i)) , idim-1)
                     ELSE
                        ilo       = idim
                     END IF
                     
                     jla       = MIN(FLOOR(yloc(j,i)) , jdim-1)
                     dlon      = xloc(j,i) - ilo
                     dlat      = yloc(j,i) - jla
                     deltalat  = 1.
                     deltalon_save = deltalon
                     deltalon  = 1.

                  ELSE IF ( ( llflag .EQ. 1 ) .OR. ( llflag .EQ. 3 ) .OR. ( llflag .EQ. 5 ) ) THEN
                     IF      (xlonc .GT.  180 ) THEN
                        xlonc_mod = xlonc - 360.
                     ELSE IF (xlonc .LT. -180 ) THEN
                        xlonc_mod = xlonc + 360.
                     ELSE
                        xlonc_mod = xlonc
                     END IF
                     IF ( ( .NOT. similar ) .OR. ( icd .NE. icd_old ) ) THEN
                        IF ( ( llflag .EQ. 1 ) .OR. &
                             ( llflag .EQ. 3 ) .OR. &
                             ( llflag .EQ. 5 ) ) THEN
                           CALL latlon_to_ij(proj,yla,xlo,xloc(j,i) , yloc(j,i))
                        END IF
                        IF ( ( NINT(xloc(j,i)) .GT. idim ) .OR. &
                             ( NINT(yloc(j,i)) .GT. jdim ) .OR. &
                             ( NINT(xloc(j,i)) .LT.    1 ) .OR. &
                             ( NINT(yloc(j,i)) .LT.    1 ) ) THEN
                           PRINT '(A)','The chosen domain from the terrain data is not completely contained &
                                       &within the input first guess domain.' 
                           PRINT '(A,I8,A,A,A)','This is the ',NINT(level),' Pa level of ',field(1:8),'.'
                           STOP 'Model_domain_outside_first_guess'
                        END IF
                     END IF
                     ilo       = MIN(FLOOR(xloc(j,i)) , idim-1)
                     jla       = MIN(FLOOR(yloc(j,i)) , jdim-1)
                     dlon      = xloc(j,i) - ilo
                     dlat      = yloc(j,i) - jla
                     deltalat  = 1.
                     deltalon  = 1.

                  END IF

                  IF ( ( rec2%linear_interpolation ) .AND. ( .NOT. wrap ) ) THEN
                     IF      ( ( ilo .GE. 1 ) .AND. ( jla .GE. 1 ) ) THEN
                        dum2d(j,i)= ( ( deltalon-dlon ) * ( ( deltalat-dlat ) * array ( ilo   , jla   )     &
                                    +                                  dlat   * array ( ilo   , jla+1 ) )   &
                                    +            dlon   * ( ( deltalat-dlat ) * array ( ilo+1 , jla   )     &
                                    +                                  dlat   * array ( ilo+1 , jla+1 ) ) ) &
                                    / ( deltalat * deltalon )
                     ELSE IF ( ( ilo .LT. 1 ) .AND. ( jla .GE. 1 ) ) THEN
                        dum2d(j,i)= array ( 1   , jla   )
                        PRINT '(A,I3,A,I3,A,A)','Extrapolating at point (',i,',',j,'), variable = ',field(1:8)
                     ELSE IF ( ( ilo .GE. 1 ) .AND. ( jla .LT. 1 ) ) THEN
                        dum2d(j,i)= array ( ilo , 1     )
                        PRINT '(A,I3,A,I3,A,A)','Extrapolating at point (',i,',',j,'), variable = ',field(1:8)
                     ELSE IF ( ( ilo .LT. 1 ) .AND. ( jla .LT. 1 ) ) THEN
                        dum2d(j,i)= array ( 1   , 1     )
                        PRINT '(A,I3,A,I3,A,A)','Extrapolating at point (',i,',',j,'), variable = ',field(1:8)
                     END IF
                  ELSE IF ( ( .NOT. rec2%linear_interpolation ) .AND. ( .NOT. wrap ) ) THEN
                     IF      ( ( ilo .GE. 1 ) .AND. ( jla .GE. 1 ) ) THEN
                        dum2d(j,i) = bint ( xloc(j,i) , yloc(j,i) , array , idim , jdim , 0 ) 
                     ELSE IF ( ( ilo .LT. 1 ) .AND. ( jla .GE. 1 ) ) THEN
                        dum2d(j,i)= array ( 1   , jla   )
                        PRINT '(A,I3,A,I3,A,A)','Extrapolating at point (',i,',',j,'), variable = ',field(1:8)
                     ELSE IF ( ( ilo .GE. 1 ) .AND. ( jla .LT. 1 ) ) THEN
                        dum2d(j,i)= array ( ilo , 1     )
                        PRINT '(A,I3,A,I3,A,A)','Extrapolating at point (',i,',',j,'), variable = ',field(1:8)
                     ELSE IF ( ( ilo .LT. 1 ) .AND. ( jla .LT. 1 ) ) THEN
                        dum2d(j,i)= array ( 1   , 1     )
                        PRINT '(A,I3,A,I3,A,A)','Extrapolating at point (',i,',',j,'), variable = ',field(1:8)
                     END IF
                  ELSE IF ( wrap ) THEN
                        IF      ( ilo .EQ. 0 ) THEN
                           PRINT '(A)','Wrapping for ilo=0.'
                           ilo = 1
                        ELSE IF ( ilo .LT. 0 ) THEN
                           PRINT '(A)','Wrapping troubles for ilo.'
                           STOP 'Wrapping_woes_ilo'
                        END IF
                        IF      ( jla .EQ. 0 ) THEN
                           PRINT '(A)','Wrapping for jla=0.'
                           jla = 1
                        ELSE IF ( jla .LT. 0 ) THEN
                           PRINT '(A)','Wrapping troubles for jla.'
                           STOP 'Wrapping_woes_jla'
                        END IF
                        dum2d(j,i)= ( ( deltalon-dlon ) * ( ( deltalat-dlat ) * array ( ilo   , jla   )     &
                                    +                                  dlat   * array ( ilo   , jla+1 ) )   &
                                    +            dlon   * ( ( deltalat-dlat ) * array (     1 , jla   )     &
                                    +                                  dlat   * array (     1 , jla+1 ) ) ) &
                                    / ( deltalat * deltalon )
                  END IF

                  IF ( llflag .EQ. 4 ) THEN
                     deltalon = deltalon_save
                  END IF

               END DO interpolate_j_loop
            END DO interpolate_i_loop
   
         END IF
   
         IF ( rec4%print_interp ) THEN
            PRINT '(//,A,1X,I8)',field,NINT(level)
            DO j=jxf,1,-10
               PRINT '(10f9.1)',dum2d(j,1:ixf:10)
           END DO
         END IF
   
         !  Store this 2d data on the model grid.
   
         CALL proc_grid_store ( field // ' ' // units // ' ' // description , level , dum2d , ixf , jxf ) 
    
         !  These are the old projection values.  Save them for a comparison.  If they
         !  are repeated, then we do not have to recompute the center latitude
         !  again.
   
         idim_old = idim
         jdim_old = jdim
         llflag_old = llflag
         IF ( ( llflag .EQ. 1 ) .OR. ( llflag .EQ. 3 ) .OR. ( llflag .EQ. 5 ) ) THEN
            lat1_old = lat1
            lon1_old = lon1
            deltax_old = deltax
            deltay_old = deltay
            xlonc_old = xlonc
            truelat1_old = truelat1
            truelat2_old = truelat2
         END IF
   
      END DO this_fg_file_only

      !  CLOSE this file.  We can reuse this UNIT again if required.

      CLOSE (UNIT=first_guess_file)

      IF ( rec4%print_file ) THEN
         PRINT '(A,A/)' , 'CLOSE file: ',TRIM(file_name)
      END IF

   END DO diff_fg_files

   !  Is this array still allocated?  If so, deallocate it at your earliest convenience.

   IF ( ALLOCATED ( array ) ) THEN
      DEALLOCATE (array)
   END IF

   !  Check to see if we have at least OPENed up a single file.  This is
   !  to provide a gentle and controlled exit from the program.

   IF ( .NOT. found_any_files ) THEN
      PRINT '(A)','Could not find any files to OPEN.'
      PRINT '(A)','You may want to check the filenames and the dates in the NAMELIST file.'
      STOP 'No_files_to_open'
   END IF

   !  We have ingested all of the first guess data for this time.
   !  We can now go through the list of stored names and determine
   !  how many 3d/2d variables there are.  The only replications
   !  are if there is a field in the 3d and in the 2d array (such
   !  as temperature).  We remove the name from the 2d array and
   !  decrement the 2d counter.

   check_2d : DO fg_variables_sfc_index = 1 , max_fg_variables

      ! If this is blank, EXIT this loop.

      IF ( all_names_sfc(fg_variables_sfc_index)(1:8) .EQ. '        ' ) THEN
         EXIT check_2d
      END IF

      check_3d : DO fg_variables_up_index = 1 , max_fg_variables

         ! If this is blank, EXIT this loop.
   
         IF ( all_names_up(fg_variables_up_index)(1:8) .EQ. '        ' ) THEN
            EXIT check_3d
         END IF

         !  Are the fields the same?

         IF ( all_names_up(fg_variables_up_index) .EQ. all_names_sfc(fg_variables_sfc_index) ) THEN
            IF ( rec4%print_debug ) THEN
               PRINT '(A,A,A)','Removing ',all_names_sfc(fg_variables_sfc_index)(1:8),' from the 2d list.'
            END IF
            all_names_sfc(fg_variables_sfc_index)(1:8) = 'XXXXXXXX'
            num2d_fg = num2d_fg - 1
         END IF

      END DO check_3d

   END DO check_2d

   PRINT '(A,I4,A,I4,A)','Found ',num3d_fg,' 3d arrays and ',num2d_fg,' 2d arrays for this time.'

   !  Figure out how many levels there are in the 3d data.  This can
   !  be different for different variables, so pick the maximum of all of the numbers.

   current_var => head
   find_max_levels : DO WHILE ( ASSOCIATED(current_var) ) 
      number_of_original_levels = MAX ( number_of_original_levels , current_var%num_levels )
      current_var => current_var%next_variable
   END DO find_max_levels   

   IF ( rec4%print_debug ) THEN
      PRINT '(A,I4,A)','Found a maximum of ',number_of_original_levels,' for the 3d data.'
   END IF

   !  Zap the ALLOCATEd space for the model domain values.

   DEALLOCATE(dum2d)
   DEALLOCATE(landuse)
   DEALLOCATE(tempgrd)
   DEALLOCATE(latitude)
   DEALLOCATE(longitude)
   DEALLOCATE(xloc)
   DEALLOCATE(yloc)

END SUBROUTINE proc_ingest_first_guess
