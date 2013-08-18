SUBROUTINE proc_output

   USE header_data
   USE namelist_info
   USE file_data
   USE gridded_data
   USE constants
   USE first_guess_data

   IMPLICIT NONE

   INTEGER :: i , j , name_loop
   INTEGER :: iimx , jjmx
   REAL , DIMENSION(jxf,ixf,number_of_original_levels+rec2%number_of_new_levels) :: dum3d
   REAL , DIMENSION(jxf,ixf) :: dum2d

   TYPE(sh) :: sh_dummy

   INTEGER :: index_land , index_seaice , index_tseasfc , index_skintemp
   LOGICAL :: test_land  , test_seaice
   CHARACTER (LEN=14) :: name

   WRITE ( name , '("REGRID_DOMAIN",I1)' ) bhi(13,1)
   
   IF ( .NOT. output_initialized ) THEN 

      output_initialized = .TRUE.

      output_file = -1
      output_unit : DO file_index = file_index_min , file_index_max
         INQUIRE (UNIT=file_index, OPENED=already_opened)
         IF ( .NOT. already_opened) THEN
            output_file = file_index
            EXIT output_unit
         END IF
      END DO output_unit   
      
      IF ( output_file .EQ. -1 ) THEN
         PRINT '(A)','Could not find a spare unit for the REGRID data.'
         STOP 'output_file_unit'
      END IF

      IF ( rec1%current_date(1:13) .EQ. rec1%starting_date(1:13) ) THEN
         REWIND ( UNIT=output_file )
   
         IF ( rec4%print_file ) THEN
            PRINT '(A,A,A/)','Trying to OPEN file: ',name,'.'
         END IF
      END IF
   
      OPEN ( UNIT=output_file , &
             FILE=name , &
             STATUS = 'UNKNOWN' , &
             FORM = 'UNFORMATTED' , &
             IOSTAT = open_status )
      
      IF ( open_status .NE. 0 ) THEN
         PRINT '(A,A,A)' , 'Error opening ',name,' file.'
         STOP 'output_file_open'
      END IF

      !  Put the big header on the data set for the REGRID program.

      WRITE ( UNIT=output_file ) bh_flag

      WRITE ( UNIT=output_file ) bhi , bhr , bhic , bhrc
 
   END IF

   !  Loop over all of the available 3d names.  Skip a few of them.

   all_3d_names : DO name_loop = 1 , fg_variables_up_index
      IF ( ( all_names_up(name_loop)(1:8) .EQ. 'T DOT   ' ) .OR. &
           ( all_names_up(name_loop)(1:8) .EQ. 'RH DOT  ' ) .OR. &
           ( all_names_up(name_loop)(1:8) .EQ. 'HGT DOT ' ) ) THEN
         CYCLE all_3d_names
      ELSE
         dum3d = all_3d(:,:,:,name_loop) 
         sh_dummy = sh_up(name_loop)

         WRITE ( UNIT=output_file ) sh_flag
         WRITE ( UNIT=output_file ) sh_dummy%num_dims , sh_dummy%start_dims , sh_dummy%end_dims , sh_dummy%xtime , &
         sh_dummy%staggering , sh_dummy%ordering , sh_dummy%current_date , &
         sh_dummy%name , sh_dummy%units , sh_dummy%description
         WRITE ( UNIT=output_file ) dum3d      

         IF (rec4%print_output) THEN
            PRINT '(A,A,A,f10.2,1X,A)','REGRID output for variable ',&
                   sh_dummy%name,' value = ',dum3d(1,1,1) , sh_dummy%units
         END IF
      END IF
   END DO all_3d_names 

   !  All of the data has been transferred to the all_2d array and the
   !  names are in a name storage array.  We need to do a final check of 
   !  the masked fields.  Because we need to have several fields available
   !  to properly mask the data, we cannot do it on input (when there is
   !  only a single field at a single level).  We need to check if the
   !  SEAICE and the LANDMASK fields exist.  If they both do, then we can 
   !  try to find the SOILTxxx and the SOILMxxx fields.  Both of the SOIL
   !  based fields are defined as zero if both the land mask is water and
   !  there is no sea ice at that grid point.  The counter example is
   !  Hudson Bay which is water, has sea ice, and therefore will have a
   !  soil temperature.

   
   test_land = .FALSE.
   test_seaice = .FALSE.
   index_tseasfc  = -1
   index_skintemp = -1
   mask_2d_search : DO name_loop = 1 , fg_variables_sfc_index
      IF      ( all_names_sfc(name_loop)(1:8) .EQ. 'LANDMASK' ) THEN
         test_land = .TRUE.
         index_land = name_loop
      ELSE IF ( all_names_sfc(name_loop)(1:8) .EQ. 'SEAICE  ' ) THEN
         test_seaice = .TRUE.
         index_seaice = name_loop
      ELSE IF ( all_names_sfc(name_loop)(1:8) .EQ. 'TSEASFC ' ) THEN
         index_tseasfc= name_loop
      ELSE IF ( all_names_sfc(name_loop)(1:8) .EQ. 'SKINTEMP' ) THEN
         index_skintemp = name_loop
      END IF
   END DO mask_2d_search

   !  Do we have to pretend we have the SST?

   IF ( ( index_tseasfc .EQ. -1 ) .AND. ( index_skintemp .NE. -1 ) ) THEN
      index_tseasfc = index_skintemp
   END IF

   !  If we found the two required fields to clean up the coastal areas
   !  of the masked fields, let's proceed.  Both of the test fields must
   !  be zero, then that implies that the soil fields are not going to 
   !  be used.  Therefore, we can set them to the SST over water.

   IF      ( test_land .AND. test_seaice ) THEN
      mask_2d_clean_up : DO name_loop = 1 , fg_variables_sfc_index
         IF      ( ( all_names_sfc(name_loop)(1:8) .GE. 'SOILT000' )   .AND. &
                   ( all_names_sfc(name_loop)(1:8) .LE. 'SOILT999' ) ) THEN
            IF ( rec4%print_mask ) THEN
               PRINT '(A,A,A)','Cleaning up the final output of the masked data ',all_names_sfc(name_loop)(1:8),'.'
            END IF

            DO i = 1 , ixf
               DO j = 1 , jxf
                  IF ( ( all_2d(j,i,index_land)   .LT. small_positive ) .AND. &
                       ( all_2d(j,i,index_seaice) .LT. small_positive ) ) THEN
                     all_2d(j,i,name_loop) = all_2d(j,i,index_tseasfc)
                  END IF
               END DO
            END DO

         END IF

      END DO mask_2d_clean_up
   ELSE IF ( test_land ) THEN
      mask_2d_clean_up_2 : DO name_loop = 1 , fg_variables_sfc_index
         IF      ( ( all_names_sfc(name_loop)(1:8) .GE. 'SOILT000' )   .AND. &
                   ( all_names_sfc(name_loop)(1:8) .LE. 'SOILT999' ) ) THEN
            IF ( rec4%print_mask ) THEN
               PRINT '(A,A,A)','Cleaning up the final output of the masked data ',all_names_sfc(name_loop)(1:8),'.'
            END IF

            DO i = 1 , ixf
               DO j = 1 , jxf
                  IF ( all_2d(j,i,index_land)   .LT. small_positive ) THEN
                     all_2d(j,i,name_loop) = all_2d(j,i,index_tseasfc)
                  END IF
               END DO
            END DO

         END IF

      END DO mask_2d_clean_up_2
   END IF
     
   !  Loop over all of the available 2d names.  Skip a few of them in the
   !  output stream.

   all_2d_names : DO name_loop = 1 , fg_variables_sfc_index
      IF ( ( all_names_sfc(name_loop)(1:8) .EQ. 'SNOWCOVD' ) .OR. &
           ( all_names_sfc(name_loop)(1:8) .EQ. 'CORIOLCR' ) .OR. &
           ( all_names_sfc(name_loop)(1:8) .EQ. 'SOILHGTD' ) .OR. &
           ( all_names_sfc(name_loop)(1:8) .EQ. 'SOILHGT ' ) .OR. &
           ( all_names_sfc(name_loop)(1:8) .EQ. 'WEASDDOT' ) .OR. &
           ( all_names_sfc(name_loop)(1:8) .EQ. 'LANDSEA ' ) .OR. &
           ( all_names_sfc(name_loop)(1:8) .EQ. 'LANDSEAD' ) .OR. &
           ( all_names_sfc(name_loop)(1:8) .EQ. 'TSEASFD ' ) ) THEN
         CYCLE all_2d_names
      ELSE
         dum2d = all_2d(:,:,name_loop) 
         sh_dummy = sh_sfc(name_loop)

         WRITE ( UNIT=output_file ) sh_flag
         WRITE ( UNIT=output_file ) sh_dummy%num_dims , sh_dummy%start_dims , sh_dummy%end_dims , sh_dummy%xtime , &
         sh_dummy%staggering , sh_dummy%ordering , sh_dummy%current_date , &
         sh_dummy%name , sh_dummy%units , sh_dummy%description
         WRITE ( UNIT=output_file ) dum2d      

         IF (rec4%print_output) THEN
            PRINT '(A,A,A,f10.2,1X,A)','REGRID output for variable ',&
                   sh_dummy%name,' value = ',dum2d(1,1) , sh_dummy%units
         END IF
      END IF
   END DO all_2d_names 

   !  We need to supply the number of vertical levels and the pressure levels
   !  associated with each slice.  This is our only 1d array that is output.
                                                                                   
   WRITE ( UNIT=output_file ) sh_flag
 
   !  Build the 1d small header.

   sh_dummy%num_dims      = 1
   sh_dummy%start_dims(1) = 1
   sh_dummy%start_dims(2) = 1
   sh_dummy%start_dims(3) = 1
   sh_dummy%start_dims(4) = 1
   sh_dummy%end_dims(1)   = rec2%number_of_new_levels + number_of_original_levels
   sh_dummy%end_dims(2)   = 1
   sh_dummy%end_dims(3)   = 1
   sh_dummy%end_dims(4)   = 1
   sh_dummy%xtime         = xfcst
   sh_dummy%staggering    = 'P   '
   sh_dummy%ordering      = 'P   '
   sh_dummy%current_date  = rec1%current_date // '.0000'
   sh_dummy%name          = 'PRESSURE '
   sh_dummy%units         = 'Pa                       '
   sh_dummy%description   = 'bottom up pressure levels (100100 = SFC)      '
   
   WRITE ( UNIT=output_file ) sh_dummy%num_dims , sh_dummy%start_dims , sh_dummy%end_dims , sh_dummy%xtime , &
   sh_dummy%staggering , sh_dummy%ordering , sh_dummy%current_date , &
   sh_dummy%name , sh_dummy%units , sh_dummy%description
   WRITE ( UNIT=output_file ) 100100.,REAL(pressure(2:))

   IF (rec4%print_output) THEN
      PRINT '(A,A,A,f10.2,1X,A)','REGRID output for variable ',&
             sh_dummy%name,' value = ',100100. , sh_dummy%units
   END IF

   !  Write the end of time flag.

   WRITE ( UNIT=output_file ) eot_flag

   !  If this is the ending time, then ...

   IF ( ( ( rec1%current_date(1:13) .EQ. rec1%ending_date(1:13) ) .AND. ( MOD(rec1%time_increment_second,3600) .EQ. 0 ) ) .OR. &
        ( ( rec1%current_date(1:16) .EQ. rec1%ending_date(1:16) ) .AND. ( MOD(rec1%time_increment_second,  60) .EQ. 0 ) ) .OR. &
          ( rec1%current_date(1:19) .EQ. rec1%ending_date(1:19) ) ) THEN                                                     

      !  Cave into the weenies still using RAWINS and help them along.
   
      IF ( rec4%print_f77_info ) THEN
         PRINT '(/A)','*******************************************************************'
         PRINT '(A)','                  Attention RAWINS users!'
         PRINT '(A)','Here is a handy PARAMETER statement you can use for RAWINS.'
         PRINT '(A)','Increase the LMX value by the number of new pressure levels that you want RAWINS to add.'
         IF ( ( bhi(8,1) .EQ. 1 ) .AND. ( bhi(15,1) .EQ. 0 ) ) THEN
            iimx = bhi( 9,1)
            jjmx = bhi(10,1)
         ELSE
            iimx = bhi(16,1)
            jjmx = bhi(17,1)
         END IF
         PRINT '(A,I4,A,I4,A,I4,A)','      PARAMETER ( IMX = ',iimx,', JMX = ',jjmx,', LMX = ',bhi(12,2),')'
         PRINT '(A/)','*******************************************************************'
      END IF

      ! Warning if min_RH_value below 1% or above 10%
      IF ( rec2%min_RH_value < 1.0 .OR. rec2%min_RH_value > 10.0) THEN
         PRINT '(A)','                                         '
         PRINT '(/A)','*******************************************************************'
         PRINT '(A)','                  Attention              '
           IF ( rec2%min_RH_value < 1.0 ) THEN
             PRINT '(A)','You have set the min RH value to be below 1%'
             PRINT '(A)','Please note that this will have no effect since '
             PRINT '(A)','later programs will reset this to a min of 1%   '
           ENDIF
           IF ( rec2%min_RH_value > 10.0 ) THEN
             PRINT '(A,F5.2)','You have set the min RH value to ',rec2%min_RH_value
             PRINT '(A)','Please note that this may result in over prediction of moisture'
             PRINT '(A)','Recommended values are between 1 and 10%        '
           ENDIF
         PRINT '(A/)','*******************************************************************'
      END IF

      !  We can CLOSE this file, since there is no more writing to it.

      CLOSE (UNIT=output_file)

      IF ( rec4%print_file ) THEN
         PRINT '(A,A,A/)' , 'CLOSE file: ',name,'.'
      END IF
   END IF
   
END SUBROUTINE proc_output
