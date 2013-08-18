SUBROUTINE proc_make_dot_point_data

   USE header_data
   USE namelist_info
   USE terrain_data
   USE gridded_data
   USE constants
   USE util

   IMPLICIT NONE

   REAL , DIMENSION(jxf,ixf) :: dum2d
   INTEGER :: i , k , count , snow_location , snow_location_depth
   LOGICAL :: snow_test_1 , snow_test_2 , snow_test_3 , &
              up_test_u , up_test_v, up_test_t , up_test_r , up_test_z

   LOGICAL :: terrain_test , land_use_test , map_x_test , map_d_test , &
              coriolis_test , lat_x_test , lon_x_test , lat_d_test , lon_d_test , &
              snow_test , slp_x_test , slp_d_test , sst_test

   INTEGER :: index_soilt010 , index_soilt040 , index_soilt100 , index_soilt200 , index_soilt400 , &
              index_soilm010 , index_soilm040 , index_soilm100 , index_soilm200 , index_soilm400 , &
              index_soilw010 , index_soilw040 , index_soilw100 , index_soilw200 , index_soilw400 , &
              index_tseasfc  , index_landuse  , index_seaice   , index_tempgrd  , index_soilhgt  , &
              index_skintemp , index_terrain  , index_seaicefr

   INTEGER :: index_monalb01 , index_monalb02 , index_monalb03 , index_monalb04 , &
              index_monalb05 , index_monalb06 , index_monalb07 , index_monalb08 , &
              index_monalb09 , index_monalb10 , index_monalb11 , index_monalb12 , &
              index_albsnomx , index_albedo   , index_canopym

   !  The job of this routine is to pick up all of the left-over
   !  arrays that should be available for a standard modeling system
   !  output file, and make sure that they are here.

   !  We need upper air u, v, t, rh and z.  This test allows us to determine
   !  if there is enough data to generate the 3d information for the 
   !  DATAGRID file.  The data sufficiency test for the surface data has to come
   !  after some data interpretation.

   up_test_u = .FALSE.
   up_test_v = .FALSE.
   up_test_t = .FALSE.
   up_test_r = .FALSE.
   up_test_z = .FALSE.
   test_up : DO i = 1 , fg_variables_up_index
      IF ( all_names_up(i)(1:8) .EQ. 'U       ' ) up_test_u = .TRUE.
      IF ( all_names_up(i)(1:8) .EQ. 'V       ' ) up_test_v = .TRUE.
      IF ( all_names_up(i)(1:8) .EQ. 'T       ' ) up_test_t = .TRUE.
      IF ( all_names_up(i)(1:8) .EQ. 'RH      ' ) up_test_r = .TRUE.
      IF ( all_names_up(i)(1:8) .EQ. 'HGT     ' ) up_test_z = .TRUE.
   END DO test_up
   IF ( ( .NOT. up_test_u ) .OR. &
        ( .NOT. up_test_v ) .OR. &
        ( .NOT. up_test_t ) .OR. &
        ( .NOT. up_test_r ) .OR. &
        ( .NOT. up_test_z ) ) THEN
      PRINT '(A,L8)','up_test_u =', up_test_u
      PRINT '(A,L8)','up_test_v =', up_test_v
      PRINT '(A,L8)','up_test_t =', up_test_t
      PRINT '(A,L8)','up_test_r =', up_test_r
      PRINT '(A,L8)','up_test_z =', up_test_z
      PRINT '(A)','Could not find enough upper air data to continue.'
      STOP 'Not_enough_upper_air_data'
   END IF

   !  Set snow to either 1 (yep, snow), 0 (no snow), -99 (snow not used).
   !  If there is an input field WEASD (water equivalent accumulated
   !  snow depth), set that flag for later possible use in building the
   !  snow cover field.

   snow_test_1 = .FALSE.
   snow_test_2 = .FALSE.
   snow_test_3 = .FALSE.
   test_snow : DO i = 1 , fg_variables_sfc_index
      IF      ( all_names_sfc(i)(1:8) .EQ. 'SNOWCOVR' ) THEN
         dum2d = all_2d(:,:,i)
         WHERE (dum2d .LT. 0.  ) dum2d = -99. 
         WHERE (dum2d .GE. 0.5 ) dum2d =   1.
         WHERE ( (dum2d .LT. 0.5 ) .AND. (dum2d .GE. 0.  ) ) dum2d =   0.
         all_2d(:,:,i) = dum2d
         snow_test_1 = .TRUE.
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'WEASD   ' ) THEN
         snow_test_2 = .TRUE.
         snow_location = i
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'SNODEPTH' ) THEN
         snow_test_3 = .TRUE.
         snow_location_depth = i
      END IF
   END DO test_snow

   !  Manufacture a snow cover field if one does not exist, but a 
   !  liquid equivalent accumulated snow depth field does exist.  If
   !  neither the snow cover field nor the water equivalent
   !  accumulated depth exist, create a snow cover field of "no data".

   IF      ( ( .NOT. snow_test_1 ) .AND. snow_test_2 ) THEN
      PRINT '(A)','No snow cover data available.  Manufacturing one from WEASD.'
      dum2d = all_2d(:,:,snow_location)
      WHERE ( dum2d .GE. 10 )  ! 10 mm = 1 cm liquid = 10 cm snow
         all_2d(:,:,fg_variables_sfc_index+1) = 1
      ELSEWHERE
         all_2d(:,:,fg_variables_sfc_index+1) = 0
      ENDWHERE
      all_names_sfc(fg_variables_sfc_index+1)( 1: 9)='SNOWCOVR '
      all_names_sfc(fg_variables_sfc_index+1)(10:34) = '0/1 Flag                 '
      all_names_sfc(fg_variables_sfc_index+1)(35:80) = 'Cross point snow cover flag: 1=snow, 0=no snow'
      fg_variables_sfc_index = fg_variables_sfc_index + 1
   ELSE IF ( ( .NOT. snow_test_1 ) .AND. snow_test_3 ) THEN
      PRINT '(A)','No snow cover data available.  Manufacturing one from SNODEPTH.'
      dum2d = all_2d(:,:,snow_location_depth)
      WHERE ( dum2d .GE. 0.10 )  ! 0.1 m = 10 cm of snow
         all_2d(:,:,fg_variables_sfc_index+1) = 1
      ELSEWHERE
         all_2d(:,:,fg_variables_sfc_index+1) = 0
      ENDWHERE
      all_names_sfc(fg_variables_sfc_index+1)( 1: 9)='SNOWCOVR '
      all_names_sfc(fg_variables_sfc_index+1)(10:34) = '0/1 Flag                 '
      all_names_sfc(fg_variables_sfc_index+1)(35:80) = 'Cross point snow cover flag: 1=snow, 0=no snow'
      fg_variables_sfc_index = fg_variables_sfc_index + 1
   ELSE IF ( ( .NOT. snow_test_1 ) .AND. ( .NOT. snow_test_2 ) ) THEN
      PRINT '(A)','No snow cover data available.  Creating an artificial field of "no data".'
      all_2d(:,:,fg_variables_sfc_index+1) = -99
      all_names_sfc(fg_variables_sfc_index+1)( 1: 9) = 'SNOWCOVR '
      all_names_sfc(fg_variables_sfc_index+1)(10:34) = '0/1 Flag                 '
      all_names_sfc(fg_variables_sfc_index+1)(35:80) = 'Cross point snow cover flag: 1=snow, 0=no snow'
      fg_variables_sfc_index = fg_variables_sfc_index + 1
   END IF
 
   !  First, several of the original variables should be
   !  renamed to be consistent with the traditional modeling
   !  system naming conventions.

   count = 0
   rename : DO i = 1 , fg_variables_sfc_index
      IF      ( ( all_names_sfc(i)(1:8) .EQ. 'PRESSURE' ) .OR.  &
                ( all_names_sfc(i)(1:8) .EQ. 'PMSL    ' ) ) THEN
         all_names_sfc(i)(1:8) = 'PSEALVLC'
         dum2d = all_2d(:,:,i)
         CALL crs2dot(dum2d,jxf,ixf)
         count = count + 1
         all_2d(:,:,fg_variables_sfc_index+count) = dum2d
         all_names_sfc(fg_variables_sfc_index+count) = 'PSEALVLD' // all_names_sfc(i)(9:80)
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'SST     ' ) THEN
         all_names_sfc(i)(1:8) = 'TSEASFC '
         dum2d = all_2d(:,:,i)
         CALL crs2dot(dum2d,jxf,ixf)
         count = count + 1
         all_2d(:,:,fg_variables_sfc_index+count) = dum2d
         all_names_sfc(fg_variables_sfc_index+count) = 'TSEASFD ' // all_names_sfc(i)(9:80)
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'SNOWCOVR' ) THEN
         dum2d = all_2d(:,:,i)
         CALL crs2dot(dum2d,jxf,ixf)
         WHERE (dum2d .LT. 0.  ) dum2d = -99. 
         WHERE (dum2d .GT. 0.5 ) dum2d =   1.
         WHERE ( (dum2d .LT. 0.5 ) .AND. (dum2d .GE. 0.  ) ) dum2d =   0.
         count = count + 1
         all_2d(:,:,fg_variables_sfc_index+count) = dum2d
         all_names_sfc(fg_variables_sfc_index+count) = 'SNOWCOVD' // all_names_sfc(i)(9:80)
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'WEASD   '    ) THEN
         dum2d = all_2d(:,:,i)
         CALL crs2dot(dum2d,jxf,ixf)
         count = count + 1
         all_2d(:,:,fg_variables_sfc_index+count) = dum2d
         all_names_sfc(fg_variables_sfc_index+count) = 'WEASDDOT' // all_names_sfc(i)(9:80)
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'SOILHGT '    ) THEN
         dum2d = all_2d(:,:,i)
         CALL crs2dot(dum2d,jxf,ixf)
         count = count + 1
         all_2d(:,:,fg_variables_sfc_index+count) = dum2d
         all_names_sfc(fg_variables_sfc_index+count) = 'SOILHGTD' // all_names_sfc(i)(9:80)
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'LANDSEA '    ) THEN
         dum2d = all_2d(:,:,i)
         CALL crs2dot(dum2d,jxf,ixf)
         WHERE (dum2d .LT. 0.5 ) dum2d = 0
         WHERE (dum2d .GE. 0.5 ) dum2d = 1
         count = count + 1
         all_2d(:,:,fg_variables_sfc_index+count) = dum2d
         all_names_sfc(fg_variables_sfc_index+count) = 'LANDSEAD' // all_names_sfc(i)(9:80)
      END IF
   END DO rename

   fg_variables_sfc_index = fg_variables_sfc_index+count

   !  Now, we can run a sufficiency test on the 2d fields.  This constitutes the 
   !  minimum data that the DATAGRID program is required to produce for the 
   !  surface.

   terrain_test  = .TRUE. 
   land_use_test = .TRUE. 
   map_x_test    = .TRUE. 
   map_d_test    = .TRUE. 
   coriolis_test = .TRUE. 
   lat_x_test    = .TRUE. 
   lon_x_test    = .TRUE. 
   lat_d_test    = .TRUE. 
   lon_d_test    = .TRUE. 
   snow_test     = .FALSE. 
   slp_x_test    = .FALSE. 
   slp_d_test    = .FALSE. 
   sst_test      = .FALSE. 
   test_2d : DO i = 1 , fg_variables_sfc_index
      IF ( all_names_sfc(i)(1:8) .EQ. 'TERRAIN ' ) terrain_test   = .TRUE.
      IF ( all_names_sfc(i)(1:8) .EQ. 'LAND USE' ) land_use_test  = .TRUE.
      IF ( all_names_sfc(i)(1:8) .EQ. 'MAPFACCR' ) map_x_test     = .TRUE.
      IF ( all_names_sfc(i)(1:8) .EQ. 'MAPFACDT' ) map_d_test     = .TRUE.
      IF ( all_names_sfc(i)(1:8) .EQ. 'CORIOLIS' ) coriolis_test  = .TRUE.
      IF ( all_names_sfc(i)(1:8) .EQ. 'LATITCRS' ) lat_x_test     = .TRUE.
      IF ( all_names_sfc(i)(1:8) .EQ. 'LONGICRS' ) lon_x_test     = .TRUE.
      IF ( all_names_sfc(i)(1:8) .EQ. 'LATITDOT' ) lat_d_test     = .TRUE.
      IF ( all_names_sfc(i)(1:8) .EQ. 'LONGIDOT' ) lon_d_test     = .TRUE.
      IF ( all_names_sfc(i)(1:8) .EQ. 'SNOWCOVR' ) snow_test      = .TRUE.
      IF ( all_names_sfc(i)(1:8) .EQ. 'PSEALVLC' ) slp_x_test     = .TRUE.
      IF ( all_names_sfc(i)(1:8) .EQ. 'PSEALVLD' ) slp_d_test     = .TRUE.
      IF ( ( all_names_sfc(i)(1:8) .EQ. 'TSEASFC ' ) .OR. ( all_names_sfc(i)(1:8) .EQ. 'SKINTEMP' ) ) sst_test       = .TRUE.
   END DO test_2d
   IF ( ( .NOT. terrain_test    ) .OR. &
        ( .NOT. land_use_test   ) .OR. &
        ( .NOT. map_x_test      ) .OR. &
        ( .NOT. map_d_test      ) .OR. &
        ( .NOT. coriolis_test   ) .OR. &
        ( .NOT. lat_x_test      ) .OR. &
        ( .NOT. lon_x_test      ) .OR. &
        ( .NOT. lat_d_test      ) .OR. &
        ( .NOT. lon_d_test      ) .OR. &
        ( .NOT. snow_test       ) .OR. &
        ( .NOT. slp_x_test      ) .OR. &
        ( .NOT. slp_d_test      ) .OR. &
        ( .NOT. sst_test        ) ) THEN
      PRINT '(/A)','There is not a sufficient number of 2d fields for continuing.'
      PRINT '(A/)','Following is a list of the mandatory 2d fields, with a T/F marking the data as available.'
      PRINT '(A,L8)','TERRAIN  terrain_test =',   terrain_test   
      PRINT '(A,L8)','LAND USE land_use_test =',  land_use_test  
      PRINT '(A,L8)','MAPFACCR map_x_test =',     map_x_test     
      PRINT '(A,L8)','MAPFACDT map_d_test =',     map_d_test     
      PRINT '(A,L8)','CORIOLIS coriolis_test =',  coriolis_test  
      PRINT '(A,L8)','LATITCRS lat_x_test =',     lat_x_test     
      PRINT '(A,L8)','LONGICRS lon_x_test =',     lon_x_test     
      PRINT '(A,L8)','LATITDOT lat_d_test =',     lat_d_test     
      PRINT '(A,L8)','LONGIDOT lon_d_test =',     lon_d_test     
      PRINT '(A,L8)','SNOWCOVR snow_test =',      snow_test      
      PRINT '(A,L8)','PSEALVLC slp_x_test =',     slp_x_test     
      PRINT '(A,L8)','PSEALVLD slp_d_test =',     slp_d_test     
      PRINT '(A,L8)','TSEASFC or SKINTEMP sst_test =',       sst_test
      PRINT '(/A)','Could not find enough surface data to continue.'
      PRINT '(A)','The above data that is labeled "F" is not available and is REQUIRED.'
      STOP 'Not_enough_surface_data'
   END IF

   !  Load up all of the 2d arrays that came in from the 
   !  TERRAIN file (and/or were computed based on the projection 
   !  information).

   DO i = 1 , num2d_ter_data
      fg_variables_sfc_index = fg_variables_sfc_index + 1
      all_2d(:,:,fg_variables_sfc_index) = ter_data(i)%array
      all_names_sfc(fg_variables_sfc_index) = ter_data(i)%name_info
   END DO

   !  Clean the relative humidity by making sure that the minimum RH
   !  is set to the user supplied min value set in record 2 of the namelist
   !  Also go ahead and constrain the maximum as well.

   clean_rh : DO i = 1 , fg_variables_up_index
      IF ( all_names_up(i)(1:8) .EQ. 'RH      ' ) THEN
         WHERE ( all_3d(:,:,:,i) .LT.   rec2%min_RH_value ) all_3d(:,:,:,i) =   rec2%min_RH_value
         WHERE ( all_3d(:,:,:,i) .GT. 100. ) all_3d(:,:,:,i) = 100.
         EXIT clean_rh
      END IF
   END DO clean_rh

   !  Next, we need to fix the "surface" layer of the geopotential
   !  height.  Either it is "correct", meaning that it came with the
   !  surface elevation of the original model, or it is "incorrect", and
   !  we stuffed the 100000 Pa level into the surface.  Either way, we
   !  substitute the terrain elevation for the surface value of the
   !  height field.

   find_height : DO i = 1 , fg_variables_up_index
      IF ( all_names_up(i)(1:8) .EQ. 'HGT     ' ) THEN
         all_3d(:,:,1,i) = ter_data(terrain_index)%array
         EXIT find_height
      END IF
   END DO find_height

   !  If we have soil temperatures, we can make sure that where the location
   !  is over water and seaice is present, that the soil temperatures are
   !  the same as the sst.  First we check to see if we have enough data for
   !  this consistency check.

   index_soilm010 = -1
   index_soilm040 = -1
   index_soilm100 = -1
   index_soilm200 = -1
   index_soilm400 = -1
   index_soilw010 = -1
   index_soilw040 = -1
   index_soilw100 = -1
   index_soilw200 = -1
   index_soilw400 = -1
   index_soilt010 = -1
   index_soilt040 = -1
   index_soilt100 = -1
   index_soilt200 = -1
   index_soilt400 = -1
   index_tseasfc  = -1
   index_landuse  = -1
   index_seaice   = -1
   index_seaicefr = -1
   index_tempgrd  = -1
   index_soilhgt  = -1
   index_terrain  = -1
  
   index_monalb01 = -1
   index_monalb02 = -1
   index_monalb03 = -1
   index_monalb04 = -1
   index_monalb05 = -1
   index_monalb06 = -1
   index_monalb07 = -1
   index_monalb08 = -1
   index_monalb09 = -1
   index_monalb10 = -1
   index_monalb11 = -1
   index_monalb12 = -1
   index_albsnomx = -1
   index_albedo   = -1
   index_canopym  = -1

   soil_1 : DO i = 1 , fg_variables_sfc_index
      IF      ( all_names_sfc(i)(1:8) .EQ. 'SOILT010' ) THEN
         index_soilt010 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'SOILT040' ) THEN
         index_soilt040 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'SOILT100' ) THEN
         index_soilt100 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'SOILT200' ) THEN
         index_soilt200 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'SOILT400' ) THEN
         index_soilt400 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'SOILM010' ) THEN
         index_soilm010 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'SOILM040' ) THEN
         index_soilm040 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'SOILM100' ) THEN
         index_soilm100 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'SOILM200' ) THEN
         index_soilm200 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'SOILM400' ) THEN
         index_soilm400 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'SOILW010' ) THEN
         index_soilw010 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'SOILW040' ) THEN
         index_soilw040 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'SOILW100' ) THEN
         index_soilw100 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'SOILW200' ) THEN
         index_soilw200 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'SOILW400' ) THEN
         index_soilw400 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'SKINTEMP' ) THEN
         index_skintemp  = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'TSEASFC ' ) THEN
         index_tseasfc  = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'LAND USE' ) THEN
         index_landuse  = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'SEAICE  ' ) THEN
         index_seaice   = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'SEAICEFR' ) THEN
         index_seaicefr = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'SOILHGT ' ) THEN
         index_soilhgt  = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'TERRAIN ' ) THEN
         index_terrain  = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'TEMPGRD ' ) THEN
         index_tempgrd  = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'MONALB01' ) THEN
         index_monalb01 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'MONALB02' ) THEN
         index_monalb02 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'MONALB03' ) THEN
         index_monalb03 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'MONALB04' ) THEN
         index_monalb04 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'MONALB05' ) THEN
         index_monalb05 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'MONALB06' ) THEN
         index_monalb06 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'MONALB07' ) THEN
         index_monalb07 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'MONALB08' ) THEN
         index_monalb08 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'MONALB09' ) THEN
         index_monalb09 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'MONALB10' ) THEN
         index_monalb10 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'MONALB11' ) THEN
         index_monalb11 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'MONALB12' ) THEN
         index_monalb12 = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'ALBSNOMX' ) THEN
         index_albsnomx = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'ALBEDO  ' ) THEN
         index_albedo   = i   
      ELSE IF ( all_names_sfc(i)(1:8) .EQ. 'CANOPYM ' ) THEN
         index_canopym  = i   
      END IF
   END DO soil_1

   !  Now, if we have a skin temperature but we do not have a real SST, 
   !  then we pretend that the skin temperature is the SST.

   IF ( ( index_skintemp .NE. -1 ) .AND. ( index_tseasfc .EQ. -1 ) ) THEN
      index_tseasfc = index_skintemp
      PRINT '(A)','No real SST data found, but we can use the skin temperature instead.'
   END IF

   !  All of the albedo fields need to have a water value of 8%.

   IF ( index_monalb01 .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) )
         all_2d(:,:,index_monalb01) = 8.
      END WHERE
   END IF
   IF ( index_monalb02 .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) )
         all_2d(:,:,index_monalb02) = 8.
      END WHERE
   END IF
   IF ( index_monalb03 .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) )
         all_2d(:,:,index_monalb03) = 8.
      END WHERE
   END IF
   IF ( index_monalb04 .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) )
         all_2d(:,:,index_monalb04) = 8.
      END WHERE
   END IF
   IF ( index_monalb05 .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) )
         all_2d(:,:,index_monalb05) = 8.
      END WHERE
   END IF
   IF ( index_monalb06 .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) )
         all_2d(:,:,index_monalb06) = 8.
      END WHERE
   END IF
   IF ( index_monalb07 .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) )
         all_2d(:,:,index_monalb07) = 8.
      END WHERE
   END IF
   IF ( index_monalb08 .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) )
         all_2d(:,:,index_monalb08) = 8.
      END WHERE
   END IF
   IF ( index_monalb09 .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) )
         all_2d(:,:,index_monalb09) = 8.
      END WHERE
   END IF
   IF ( index_monalb10 .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) )
         all_2d(:,:,index_monalb10) = 8.
      END WHERE
   END IF
   IF ( index_monalb11 .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) )
         all_2d(:,:,index_monalb11) = 8.
      END WHERE
   END IF
   IF ( index_monalb12 .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) )
         all_2d(:,:,index_monalb12) = 8.
      END WHERE
   END IF
   IF ( index_albsnomx .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) )
         all_2d(:,:,index_albsnomx) = 8.
      END WHERE
   END IF
   IF ( index_albedo   .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) )
         all_2d(:,:,index_albedo  ) = 8.
      END WHERE
   END IF
   IF ( index_canopym  .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) )
         all_2d(:,:,index_canopym ) = 0.
      END WHERE
   END IF

   !  Please, no negative values of seaice.

   IF ( index_seaice   .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_seaice  ) .LT. 0. ) 
         all_2d(:,:,index_seaice  ) = 0.
      END WHERE
   END IF

   IF ( index_seaicefr .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_seaicefr) .LT. 0. ) 
         all_2d(:,:,index_seaicefr) = 0.
      END WHERE
   END IF
      
   !  If there is no sea ice field, then we
   !  create a sea ice field from the locations where the SST is less than
   !  a given threshold.  There are three IF tests, one for each type of 
   !  available soil input source.  We also need to make sure that the IF
   !  test has the necessary data: sst and landuse categories.

   IF      ( ( bhic(23,1)(1:4) .EQ. 'OLD ' ) .AND. &
             ( index_tseasfc   .NE. -1     ) .AND. &
             ( index_landuse   .NE. -1     ) ) THEN
      WHERE ( ( all_2d(:,:,index_landuse) .EQ. bhi(23,1)            ) .AND. &
              ( all_2d(:,:,index_tseasfc) .LT. rec2%sst_to_ice_threshold ) )
         all_2d(:,:,index_landuse) = 11
      END WHERE
      IF ( index_seaice .NE. -1 ) THEN
         WHERE ( ( all_2d(:,:,index_landuse) .EQ. bhi(23,1)            ) .AND. &
                 ( all_2d(:,:,index_tseasfc) .LT. rec2%sst_to_ice_threshold ) )
            all_2d(:,:,index_seaice) = 1
         END WHERE
      END IF
   ELSE IF ( ( bhic(23,1)(1:4) .EQ. 'USGS' ) .AND. &
             ( index_tseasfc   .NE. -1     ) .AND. &
             ( index_landuse   .NE. -1     ) ) THEN
      WHERE ( ( all_2d(:,:,index_landuse) .EQ. bhi(23,1)            ) .AND. &
              ( all_2d(:,:,index_tseasfc) .LT. rec2%sst_to_ice_threshold ) )
         all_2d(:,:,index_landuse) = 24
      END WHERE
      IF ( index_seaice .NE. -1 ) THEN
         WHERE ( ( all_2d(:,:,index_landuse) .EQ. bhi(23,1)            ) .AND. &
                 ( all_2d(:,:,index_tseasfc) .LT. rec2%sst_to_ice_threshold ) )
            all_2d(:,:,index_seaice) = 1
         END WHERE
      END IF
   ELSE IF ( ( bhic(23,1)(1:4) .EQ. 'SiB ' ) .AND. &
             ( index_tseasfc   .NE. -1     ) .AND. &
             ( index_landuse   .NE. -1     ) ) THEN
      WHERE ( ( all_2d(:,:,index_landuse) .EQ. bhi(23,1)            ) .AND. &
              ( all_2d(:,:,index_tseasfc) .LT. rec2%sst_to_ice_threshold ) )
         all_2d(:,:,index_landuse) = 16
      END WHERE
      IF ( index_seaice .NE. -1 ) THEN
         WHERE ( ( all_2d(:,:,index_landuse) .EQ. bhi(23,1)            ) .AND. &
                 ( all_2d(:,:,index_tseasfc) .LT. rec2%sst_to_ice_threshold ) )
            all_2d(:,:,index_seaice) = 1
         END WHERE
      END IF
   END IF

   !  Adjust soil temperatures based on difference between the interpoated soil elevation and 
   !  and the model terrain.  Use a -6 K km{-1}  lapse rate.

   IF      ( ( index_soilhgt .NE. -1 ) .AND. &
             ( index_seaice  .NE. -1 ) ) THEN

      IF ( index_soilt010 .NE. -1 ) THEN
         WHERE ( ( all_2d(:,:,index_landuse) .NE. bhi(23,1) ) .OR. &
                 ( all_2d(:,:,index_seaice)  .EQ. 1         ) ) 
            all_2d(:,:,index_soilt010) = all_2d(:,:,index_soilt010) &
                                         -0.0065 * ( ter_data(terrain_index)%array - all_2d(:,:,index_soilhgt) )
         END WHERE
      END IF

      IF ( index_soilt040 .NE. -1 ) THEN
         WHERE ( ( all_2d(:,:,index_landuse) .NE. bhi(23,1) ) .OR. &
                 ( all_2d(:,:,index_seaice)  .EQ. 1         ) ) 
            all_2d(:,:,index_soilt040) = all_2d(:,:,index_soilt040) &
                                         -0.0065 * ( ter_data(terrain_index)%array - all_2d(:,:,index_soilhgt) )
         END WHERE
      END IF

      IF ( index_soilt100 .NE. -1 ) THEN
         WHERE ( ( all_2d(:,:,index_landuse) .NE. bhi(23,1) ) .OR. &
                 ( all_2d(:,:,index_seaice)  .EQ. 1         ) ) 
            all_2d(:,:,index_soilt100) = all_2d(:,:,index_soilt100) &
                                         -0.0065 * ( ter_data(terrain_index)%array - all_2d(:,:,index_soilhgt) )
         END WHERE
      END IF

      IF ( index_soilt200 .NE. -1 ) THEN
         WHERE ( ( all_2d(:,:,index_landuse) .NE. bhi(23,1) ) .OR. &
                 ( all_2d(:,:,index_seaice)  .EQ. 1         ) ) 
            all_2d(:,:,index_soilt200) = all_2d(:,:,index_soilt200) &
                                         -0.0065 * ( ter_data(terrain_index)%array - all_2d(:,:,index_soilhgt) )
         END WHERE
      END IF

      IF ( index_soilt400 .NE. -1 ) THEN
         WHERE ( ( all_2d(:,:,index_landuse) .NE. bhi(23,1) ) .OR. &
                 ( all_2d(:,:,index_seaice)  .EQ. 1         ) ) 
            all_2d(:,:,index_soilt400) = all_2d(:,:,index_soilt400) &
                                         -0.0065 * ( ter_data(terrain_index)%array - all_2d(:,:,index_soilhgt) )
         END WHERE
      END IF

   ELSE IF ( ( index_soilhgt .NE. -1 ) .AND. &
             ( index_seaice  .EQ. -1 ) ) THEN

      IF ( index_soilt010 .NE. -1 ) THEN
         WHERE   ( all_2d(:,:,index_landuse) .NE. bhi(23,1) ) 
            all_2d(:,:,index_soilt010) = all_2d(:,:,index_soilt010) &
                                         -0.0065 * ( ter_data(terrain_index)%array - all_2d(:,:,index_soilhgt) )
         END WHERE
      END IF

      IF ( index_soilt040 .NE. -1 ) THEN
         WHERE   ( all_2d(:,:,index_landuse) .NE. bhi(23,1) ) 
            all_2d(:,:,index_soilt040) = all_2d(:,:,index_soilt040) &
                                         -0.0065 * ( ter_data(terrain_index)%array - all_2d(:,:,index_soilhgt) )
         END WHERE
      END IF

      IF ( index_soilt100 .NE. -1 ) THEN
         WHERE   ( all_2d(:,:,index_landuse) .NE. bhi(23,1) ) 
            all_2d(:,:,index_soilt100) = all_2d(:,:,index_soilt100) &
                                         -0.0065 * ( ter_data(terrain_index)%array - all_2d(:,:,index_soilhgt) )
         END WHERE
      END IF

      IF ( index_soilt200 .NE. -1 ) THEN
         WHERE   ( all_2d(:,:,index_landuse) .NE. bhi(23,1) ) 
            all_2d(:,:,index_soilt200) = all_2d(:,:,index_soilt200) &
                                         -0.0065 * ( ter_data(terrain_index)%array - all_2d(:,:,index_soilhgt) )
         END WHERE
      END IF

      IF ( index_soilt400 .NE. -1 ) THEN
         WHERE   ( all_2d(:,:,index_landuse) .NE. bhi(23,1) ) 
            all_2d(:,:,index_soilt400) = all_2d(:,:,index_soilt400) &
                                         -0.0065 * ( ter_data(terrain_index)%array - all_2d(:,:,index_soilhgt) )
         END WHERE
      END IF

   END IF
         
   !  Another fix for soil temperature at the expected levels.  If there is seaice, set the
   !  soil temp to the sst over water.

   IF ( ( index_tseasfc .NE. -1 ) .AND. &
        ( index_landuse .NE. -1 ) .AND. &
        ( index_seaice  .NE. -1 ) ) THEN

      IF ( index_soilt010 .NE. -1 ) THEN
         WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) )
            all_2d(:,:,index_soilt010) = all_2d(:,:,index_tseasfc)
         END WHERE
         IF ( index_tempgrd .NE. -1 ) THEN 
            WHERE ( all_2d(:,:,index_soilt010) .LT. 200 ) 
               all_2d(:,:,index_soilt010) = all_2d(:,:,index_tempgrd)
            END WHERE
         END IF
      END IF

      IF ( index_soilt040 .NE. -1 ) THEN
         WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) )
            all_2d(:,:,index_soilt040) = all_2d(:,:,index_tseasfc)
         END WHERE
         IF ( index_tempgrd .NE. -1 ) THEN 
            WHERE ( all_2d(:,:,index_soilt040) .LT. 200 ) 
               all_2d(:,:,index_soilt040) = all_2d(:,:,index_tempgrd)
            END WHERE
         END IF
      END IF

      IF ( index_soilt100 .NE. -1 ) THEN
         WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) )
            all_2d(:,:,index_soilt100) = all_2d(:,:,index_tseasfc)
         END WHERE
         IF ( index_tempgrd .NE. -1 ) THEN 
            WHERE ( all_2d(:,:,index_soilt100) .LT. 200 ) 
               all_2d(:,:,index_soilt100) = all_2d(:,:,index_tempgrd)
            END WHERE
         END IF
      END IF

      IF ( index_soilt200 .NE. -1 ) THEN
         WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) )
            all_2d(:,:,index_soilt200) = all_2d(:,:,index_tseasfc)
         END WHERE
         IF ( index_tempgrd .NE. -1 ) THEN 
            WHERE ( all_2d(:,:,index_soilt200) .LT. 200 ) 
               all_2d(:,:,index_soilt200) = all_2d(:,:,index_tempgrd)
            END WHERE
         END IF
      END IF

      IF ( index_soilt400 .NE. -1 ) THEN
         WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) )
            all_2d(:,:,index_soilt400) = all_2d(:,:,index_tseasfc)
         END WHERE
         IF ( index_tempgrd .NE. -1 ) THEN 
            WHERE ( all_2d(:,:,index_soilt400) .LT. 200 ) 
               all_2d(:,:,index_soilt400) = all_2d(:,:,index_tempgrd)
            END WHERE
         END IF
      END IF

   END IF

   !  Fix soil moisture data over the water, set it to 1.

   IF ( index_soilm010 .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) ) 
         all_2d(:,:,index_soilm010) = 1.
      END WHERE
   END IF

   IF ( index_soilm040 .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) ) 
         all_2d(:,:,index_soilm040) = 1.
      END WHERE
   END IF

   IF ( index_soilm100 .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) ) 
         all_2d(:,:,index_soilm100) = 1.
      END WHERE
   END IF

   IF ( index_soilm200 .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) ) 
         all_2d(:,:,index_soilm200) = 1.
      END WHERE
   END IF

   IF ( index_soilm400 .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) ) 
         all_2d(:,:,index_soilm400) = 1.
      END WHERE
   END IF

   IF ( index_soilw010 .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) ) 
         all_2d(:,:,index_soilw010) = 1.
      END WHERE
   END IF

   IF ( index_soilw040 .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) ) 
         all_2d(:,:,index_soilw040) = 1.
      END WHERE
   END IF

   IF ( index_soilw100 .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) ) 
         all_2d(:,:,index_soilw100) = 1.
      END WHERE
   END IF

   IF ( index_soilw200 .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) ) 
         all_2d(:,:,index_soilw200) = 1.
      END WHERE
   END IF

   IF ( index_soilw400 .NE. -1 ) THEN
      WHERE ( all_2d(:,:,index_landuse) .EQ. bhi(23,1) ) 
         all_2d(:,:,index_soilw400) = 1.
      END WHERE
   END IF

   IF ( rec4%print_array_store ) THEN
      DO i = 1 , fg_variables_sfc_index
         PRINT '(A,I8,A,A)','Surface 2d: variable #',i,', variable name = ',all_names_sfc(i)(1:8)
      END DO
      DO i = 1 , fg_variables_up_index
         PRINT '(A,I8,A,A)','Up air  3d: variable #',i,', variable name = ',all_names_up(i)(1:8)
      END DO
   END IF

END SUBROUTINE proc_make_dot_point_data
