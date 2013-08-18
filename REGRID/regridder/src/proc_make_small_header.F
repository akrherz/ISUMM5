SUBROUTINE proc_make_small_header

   USE header_data
   USE namelist_info
   USE gridded_data
   USE first_guess_data

   IMPLICIT NONE

   INTEGER :: i

   !  The small header has to be built for each variable.  We run through
   !  each of the 3d and 2d variables and fill in the required values.  There
   !  is no need to keep count of which variables we are using, as we can cull
   !  the data when we write the fields.

   header_3d : DO i = 1 , fg_variables_up_index
      IF    ( ( all_names_up(i)(1:8) .EQ. 'T DOT   ') .OR. &
              ( all_names_up(i)(1:8) .EQ. 'HGT DOT ') .OR. &
              ( all_names_up(i)(1:8) .EQ. 'RH DOT  ') ) THEN
         sh_up(i)%name         = 'SKIP3D   '
         CYCLE header_3d
      ELSE IF ( all_names_up(i)(1:8) .EQ. 'T       ') THEN
         sh_up(i)%num_dims     = 3
         sh_up(i)%start_dims(1)= 1
         sh_up(i)%start_dims(2)= 1
         sh_up(i)%start_dims(3)= 1
         sh_up(i)%start_dims(4)= 1
         sh_up(i)%end_dims(1)  = jxf
         sh_up(i)%end_dims(2)  = ixf
         sh_up(i)%end_dims(3)  = number_of_original_levels+rec2%number_of_new_levels
         sh_up(i)%end_dims(4)  = 1
         sh_up(i)%xtime        = xfcst
         sh_up(i)%staggering   = 'C   '
         sh_up(i)%ordering     = 'YXP '
         sh_up(i)%current_date = rec1%current_date // '.0000'
         sh_up(i)%name         = all_names_up(i)( 1: 9)
         sh_up(i)%units        = 'K                        '
         sh_up(i)%description  = 'Cross point temperature                       '
      ELSE IF ( all_names_up(i)(1:8) .EQ. 'U       ') THEN
         sh_up(i)%num_dims     = 3
         sh_up(i)%start_dims(1)= 1
         sh_up(i)%start_dims(2)= 1
         sh_up(i)%start_dims(3)= 1
         sh_up(i)%start_dims(4)= 1
         sh_up(i)%end_dims(1)  = jxf
         sh_up(i)%end_dims(2)  = ixf
         sh_up(i)%end_dims(3)  = number_of_original_levels+rec2%number_of_new_levels
         sh_up(i)%end_dims(4)  = 1
         sh_up(i)%xtime        = xfcst
         sh_up(i)%staggering   = 'D   '
         sh_up(i)%ordering     = 'YXP '
         sh_up(i)%current_date = rec1%current_date // '.0000'
         sh_up(i)%name         = all_names_up(i)( 1: 9)
         sh_up(i)%units        = 'm/s                      '
         sh_up(i)%description  = 'Dot point map relative U velocity component   '
      ELSE IF ( all_names_up(i)(1:8) .EQ. 'V       ') THEN
         sh_up(i)%num_dims     = 3
         sh_up(i)%start_dims(1)= 1
         sh_up(i)%start_dims(2)= 1
         sh_up(i)%start_dims(3)= 1
         sh_up(i)%start_dims(4)= 1
         sh_up(i)%end_dims(1)  = jxf
         sh_up(i)%end_dims(2)  = ixf
         sh_up(i)%end_dims(3)  = number_of_original_levels+rec2%number_of_new_levels
         sh_up(i)%end_dims(4)  = 1
         sh_up(i)%xtime        = xfcst
         sh_up(i)%staggering   = 'D   '
         sh_up(i)%ordering     = 'YXP '
         sh_up(i)%current_date = rec1%current_date // '.0000'
         sh_up(i)%name         = all_names_up(i)( 1: 9)
         sh_up(i)%units        = 'm/s                      '
         sh_up(i)%description  = 'Dot point map relative V velocity component   '
      ELSE IF ( all_names_up(i)(1:8) .EQ. 'HGT     ') THEN
         sh_up(i)%num_dims     = 3
         sh_up(i)%start_dims(1)= 1
         sh_up(i)%start_dims(2)= 1
         sh_up(i)%start_dims(3)= 1
         sh_up(i)%start_dims(4)= 1
         sh_up(i)%end_dims(1)  = jxf
         sh_up(i)%end_dims(2)  = ixf
         sh_up(i)%end_dims(3)  = number_of_original_levels+rec2%number_of_new_levels
         sh_up(i)%end_dims(4)  = 1
         sh_up(i)%xtime        = xfcst
         sh_up(i)%staggering   = 'C   '
         sh_up(i)%ordering     = 'YXP '
         sh_up(i)%current_date = rec1%current_date // '.0000'
         sh_up(i)%name         = 'H        '
         sh_up(i)%units        = 'm                        '
         sh_up(i)%description  = 'Cross point geopotential height               '
      ELSE IF ( all_names_up(i)(1:8) .EQ. 'RH      ') THEN
         sh_up(i)%num_dims     = 3
         sh_up(i)%start_dims(1)= 1
         sh_up(i)%start_dims(2)= 1
         sh_up(i)%start_dims(3)= 1
         sh_up(i)%start_dims(4)= 1
         sh_up(i)%end_dims(1)  = jxf
         sh_up(i)%end_dims(2)  = ixf
         sh_up(i)%end_dims(3)  = number_of_original_levels+rec2%number_of_new_levels
         sh_up(i)%end_dims(4)  = 1
         sh_up(i)%xtime        = xfcst
         sh_up(i)%staggering   = 'C   '
         sh_up(i)%ordering     = 'YXP '
         sh_up(i)%current_date = rec1%current_date // '.0000'
         sh_up(i)%name         = all_names_up(i)( 1: 9)
         sh_up(i)%units        = '%                        '
         sh_up(i)%description  = 'Cross point relative humidity                 '
      ELSE
         sh_up(i)%num_dims     = 3
         sh_up(i)%start_dims(1)= 1
         sh_up(i)%start_dims(2)= 1
         sh_up(i)%start_dims(3)= 1
         sh_up(i)%start_dims(4)= 1
         sh_up(i)%end_dims(1)  = jxf
         sh_up(i)%end_dims(2)  = ixf
         sh_up(i)%end_dims(3)  = number_of_original_levels+rec2%number_of_new_levels
         sh_up(i)%end_dims(4)  = 1
         sh_up(i)%xtime        = xfcst
         sh_up(i)%staggering   = 'C   '
         sh_up(i)%ordering     = 'YXP '
         sh_up(i)%current_date = rec1%current_date // '.0000'
         sh_up(i)%name         = all_names_up(i)( 1: 9)
         sh_up(i)%units        = all_names_up(i)(10:34)
         sh_up(i)%description  = all_names_up(i)(35:80)
      END IF
   END DO header_3d
                                                                                 
   !  Do the 2d data arrays.  The dot point sea surface temperature,
   !  snow cover and the cross point coriolis parameter are discarded.

   header_2d : DO i = 1 , fg_variables_sfc_index
      IF      ( ( all_names_sfc(i)(1:8) .EQ. 'TSEASFD ' ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'CORIOLCR' ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'SNOWCOVD' ) ) THEN
         sh_sfc(i)%name         = 'SKIP2D   '
         CYCLE header_2d
      ELSE IF ( ( all_names_sfc(i)(1:8) .EQ. 'TERRAIN ' ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'LAND USE' ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'LANDMASK' ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'MAPFACCR' ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'LATITCRS' ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'LONGICRS' ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'SNOWCOVR' ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'PSEALVLC' ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'TSEASFC ' ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'SKINTEMP' ) .OR. &
                ( all_names_sfc(i)(1:5) .EQ. 'SOILT'    ) .OR. &
                ( all_names_sfc(i)(1:5) .EQ. 'SOILM'    ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'SOILHGT ' ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'LANDSEA ' ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'SEAICE  ' ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'SOILINDX' ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'VEGEINDX' ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'TEMPGRD ' ) .OR. &
                ( all_names_sfc(i)(1:6) .EQ. 'VEGFRC'   ) .OR. &
                ( all_names_sfc(i)(1:6) .EQ. 'VEGCAT'   ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'WEASD   ' ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'SNOWCOVR' ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'SNODEPTH' ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'SNODPTH ' ) .OR. &
                ( all_names_sfc(i)(1:6) .EQ. 'SOLCAT'   ) ) THEN
         sh_sfc(i)%num_dims     = 2
         sh_sfc(i)%start_dims(1)= 1
         sh_sfc(i)%start_dims(2)= 1
         sh_sfc(i)%start_dims(3)= 1
         sh_sfc(i)%start_dims(4)= 1
         sh_sfc(i)%end_dims(1)  = jxf
         sh_sfc(i)%end_dims(2)  = ixf
         sh_sfc(i)%end_dims(3)  = 1
         sh_sfc(i)%end_dims(4)  = 1
         sh_sfc(i)%xtime        = xfcst
         sh_sfc(i)%staggering   = 'C   '
         sh_sfc(i)%ordering     = 'YX  '
         sh_sfc(i)%current_date = rec1%current_date // '.0000'
         sh_sfc(i)%name         = all_names_sfc(i)( 1: 9)
         sh_sfc(i)%units        = all_names_sfc(i)(10:34)
         sh_sfc(i)%description  = all_names_sfc(i)(35:80)

      ELSE IF ( ( all_names_sfc(i)(1:8) .EQ. 'MAPFACDT' ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'CORIOLIS' ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'LATITDOT' ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'LONGIDOT' ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'PSEALVLD' ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'SOILHGTD' ) .OR. &
                ( all_names_sfc(i)(1:8) .EQ. 'LANDSEAD' ) ) THEN
         sh_sfc(i)%num_dims     = 2
         sh_sfc(i)%start_dims(1)= 1
         sh_sfc(i)%start_dims(2)= 1
         sh_sfc(i)%start_dims(3)= 1
         sh_sfc(i)%start_dims(4)= 1
         sh_sfc(i)%end_dims(1)  = jxf
         sh_sfc(i)%end_dims(2)  = ixf
         sh_sfc(i)%end_dims(3)  = 1
         sh_sfc(i)%end_dims(4)  = 1
         sh_sfc(i)%xtime        = xfcst
         sh_sfc(i)%staggering   = 'D   '
         sh_sfc(i)%ordering     = 'YX  '
         sh_sfc(i)%current_date = rec1%current_date // '.0000'
         sh_sfc(i)%name         = all_names_sfc(i)( 1: 9)
         sh_sfc(i)%units        = all_names_sfc(i)(10:34)
         sh_sfc(i)%description  = all_names_sfc(i)(35:80)

      ELSE
         sh_sfc(i)%num_dims     = 2
         sh_sfc(i)%start_dims(1)= 1
         sh_sfc(i)%start_dims(2)= 1
         sh_sfc(i)%start_dims(3)= 1
         sh_sfc(i)%start_dims(4)= 1
         sh_sfc(i)%end_dims(1)  = jxf
         sh_sfc(i)%end_dims(2)  = ixf
         sh_sfc(i)%end_dims(3)  = 1
         sh_sfc(i)%end_dims(4)  = 1
         sh_sfc(i)%xtime        = xfcst
         sh_sfc(i)%staggering   = 'C   '
         sh_sfc(i)%ordering     = 'YX  '
         sh_sfc(i)%current_date = rec1%current_date // '.0000'
         sh_sfc(i)%name         = all_names_sfc(i)( 1: 9)
         sh_sfc(i)%units        = all_names_sfc(i)(10:34)
         sh_sfc(i)%description  = all_names_sfc(i)(35:80)

      END IF
   END DO header_2d

END SUBROUTINE proc_make_small_header
