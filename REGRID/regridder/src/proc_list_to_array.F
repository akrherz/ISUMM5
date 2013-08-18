SUBROUTINE proc_list_to_array

   USE diags
   USE header_data
   USE namelist_info
   USE link_list_info   
   USE gridded_data
   USE terrain_data

   IMPLICIT NONE

   !  These variables are used for the storage and vertical
   !  interpolation of the 3d data.

   INTEGER :: fg_vertical_index , new_levels_index
   REAL , DIMENSION(jxf,ixf) :: slab_bottom, slab_top, slab_middle
   REAL                      :: pressure_bottom, pressure_top, pressure_middle
   LOGICAL                   :: vertically_interpolate = .FALSE.
   LOGICAL                   :: any_surface_data   , &
                                any_surface_data_t , &
                                any_surface_data_u , &
                                any_surface_data_v , &
                                any_surface_data_r , &
                                any_surface_data_z

   !  Pressure sorting fields.

   REAL , ALLOCATABLE , DIMENSION(:) :: combined_pressure_levels , dummy
   INTEGER :: count , dummy_count , inner , outer , number_of_combined_levels , i
   REAL :: hold

   !  Flags used for searching for vertical coordinate limiting cases.

   INTEGER :: num_same_press , nnl 
   LOGICAL :: need_ptop_level

   !  Data needed to generate missing surface fields.

   INTEGER :: loop 
   INTEGER :: index_t = -1 , index_u = -1 , index_v = -1 , index_r = -1 , index_h = -1
   INTEGER :: index_z = -1 , index_p = -1
   INTEGER :: ii , jj , kk 
   REAL , ALLOCATABLE , DIMENSION(:,:,:) :: q
   REAL , ALLOCATABLE , DIMENSION(:,:)   :: psfc

   !  Initialize the two name arrays (both for surface and upper-air).

   all_names_sfc = '                                                                               '
   all_names_up  = '                                                                               '
   fg_variables_sfc_index = 0 
   fg_variables_up_index  = 0 
   new_levels_index       = 0 

   !  Find out how many levels are in the 3d data.  It is possible
   !  that some fields do not have as many levels as other fields
   !  (such as RH, there may not be a surface value).  Loop through
   !  all of the variable names and find the maximum of the number
   !  of levels.

   any_surface_data_t = .FALSE.
   any_surface_data_u = .FALSE.
   any_surface_data_v = .FALSE.
   any_surface_data_r = .FALSE.
   any_surface_data_z = .FALSE.
   any_surface_data   = .FALSE.
   current_var => head
   number_of_original_levels = 0
   find_max_levels : DO WHILE ( ASSOCIATED(current_var) )
      number_of_original_levels = MAX ( number_of_original_levels , current_var%num_levels )
      IF ( rec4%print_debug ) THEN
         PRINT '(A,I8,A,A)','Found ',current_var%num_levels,' levels of data for variable ',current_var%name
      END IF
      IF ( current_var%num_levels .GT. 1 ) THEN
         IF      ( ( ABS(current_var%slab%level-200100.) .LT. 1 ) .AND. ( current_var%name(1:8) .EQ. 'T       ' ) ) THEN
            any_surface_data_t = .TRUE.
         ELSE IF ( ( ABS(current_var%slab%level-200100.) .LT. 1 ) .AND. ( current_var%name(1:8) .EQ. 'U       ' ) ) THEN
            any_surface_data_u = .TRUE.
         ELSE IF ( ( ABS(current_var%slab%level-200100.) .LT. 1 ) .AND. ( current_var%name(1:8) .EQ. 'V       ' ) ) THEN
            any_surface_data_v = .TRUE.
         ELSE IF ( ( ABS(current_var%slab%level-200100.) .LT. 1 ) .AND. ( current_var%name(1:8) .EQ. 'RH      ' ) ) THEN
            any_surface_data_r = .TRUE.
         ELSE IF ( ( ABS(current_var%slab%level-200100.) .LT. 1 ) .AND. ( current_var%name(1:8) .EQ. 'HGT     ' ) ) THEN
            any_surface_data_z = .TRUE.
         END IF
      END IF
      current_var => current_var%next_variable
   END DO find_max_levels  
   IF ( rec4%print_array_store ) THEN
      PRINT '(A,I8,A)','Found ',number_of_original_levels,' as maximum number of original vertical levels.'
   END IF

   !  If there is no surface data, we need to let the folks know.

   any_surface_data = any_surface_data_t .OR. &
                      any_surface_data_u .OR. &
                      any_surface_data_v .OR. &
                      any_surface_data_r .OR. &
                      any_surface_data_z

   IF ( .NOT. ( any_surface_data_t .AND. &
                any_surface_data_u .AND. &
                any_surface_data_v .AND. &
                any_surface_data_r ) ) THEN
      PRINT '(A)',' '
      PRINT '(A)','*************************************************************************'
      PRINT '(A)','There is some missing surface data in the 3d arrays in your data set.'
      PRINT '(A)','The surrounding pressure level data is being interpolated to the surface.'
      IF ( .NOT. any_surface_data_t ) THEN
         PRINT '(A)','Surface temperature is missing.'
      END IF
      IF ( .NOT. any_surface_data_u ) THEN
         PRINT '(A)','Surface wind (u-component) is missing.'
      END IF
      IF ( .NOT. any_surface_data_v ) THEN
         PRINT '(A)','Surface wind (v-component) is missing.'
      END IF
      IF ( .NOT. any_surface_data_r ) THEN
         PRINT '(A)','Surface relative humidity is missing.'
      END IF
      PRINT '(A)','*************************************************************************'
      PRINT '(A)',' '
   END IF

   !  Create a pressure array that can hold maximum number of proposed levels: 
   !  original + new + 1 (possibly needed for the surface in case one is not available).

   ALLOCATE ( combined_pressure_levels(number_of_original_levels+rec2%number_of_new_levels+1))
   combined_pressure_levels = -1
   ALLOCATE ( dummy                   (number_of_original_levels+rec2%number_of_new_levels+1))
   dummy                    = -1

   !  Fill in the new + original pressure levels in the dummy array.  These aren't sorted or checked
   !  for duplicates, we just want a list.  We can sort afterwards.

   dummy(1:rec2%number_of_new_levels) = rec2%new_levels(1:rec2%number_of_new_levels)
   current_var => head
   find_original_p_levels : DO WHILE ( ASSOCIATED(current_var) ) 
      IF ( number_of_original_levels .EQ. current_var%num_levels ) THEN
         dummy_count = 0
         current_slab => current_var%slab
         find_all_p : DO WHILE ( ASSOCIATED ( current_slab ) ) 
            dummy_count = dummy_count + 1
            dummy(rec2%number_of_new_levels+dummy_count) = current_slab%level
            current_slab => current_slab%next_slab
         END DO find_all_p
         EXIT find_original_p_levels
      ELSE
         current_var => current_var%next_variable 
      END IF
   END DO find_original_p_levels

   !  Now we sort the dummy array.  This is such a short vector, that a simple bubble
   !  sort method can be used.

   outer_sort : DO outer = 1 , number_of_original_levels + rec2%number_of_new_levels - 1
      inner_sort : DO inner = outer+1 , number_of_original_levels + rec2%number_of_new_levels
         IF ( dummy(inner) .GT. dummy(outer) ) THEN
            hold = dummy(outer)
            dummy(outer) = dummy(inner)
            dummy(inner) = hold
         END IF
      END DO inner_sort
   END DO outer_sort

   !  Stick the sorted pressures into a final array, without duplicates.

   IF      ( .NOT. any_surface_data ) THEN
      combined_pressure_levels(1) = 200100.
      count = 1
      DO i = 1 , number_of_original_levels + rec2%number_of_new_levels
         IF ( ABS(dummy(i) - combined_pressure_levels(count) ) .GT. 1. ) THEN
            count = count + 1
            combined_pressure_levels(count) = dummy(i) 
         END IF
      END DO
   ELSE
      count = 1
      DO i = 1 , number_of_original_levels + rec2%number_of_new_levels
         IF ( ABS(dummy(i) - combined_pressure_levels(count) ) .GT. 1. ) THEN
            count = count + 1
            combined_pressure_levels(count) = dummy(i) 
         END IF
      END DO
      count = count - 1
   END IF

   number_of_combined_levels = count

   !  Correct the pressure level data for a few things.  1) If we have a 
   !  level and the same level is requested as a new level, decrement the
   !  number of original levels; 2) make sure that ptop is one of the 
   !  levels in the final data (it can't be a "new" level).

   current_var => head
   num_same_press = 0
   need_ptop_level = .TRUE.
   
   !  Loop over all of the variables.

   find_p_peculiarities : DO WHILE ( ASSOCIATED(current_var) ) 

      !  Find the variable which has the most vertical levels.  If there are
      !  several with the same number, we just find the first one.

      IF ( number_of_original_levels .EQ. current_var%num_levels ) THEN

         !  Loop over each first-guess level for this variable.

         current_slab => current_var%slab
         first_guess_p_loop : DO WHILE ( ASSOCIATED ( current_slab ) ) 

            !  Loop over each requested pressure level from the namelist.

            find_dups : DO nnl = 1 , rec2%number_of_new_levels

               !  Do we have a requested level that already exists?

               IF (  ABS ( current_slab%level - rec2%new_levels(nnl) ) .LT. 1 ) THEN
                  num_same_press = num_same_press + 1
                  IF ( rec4%print_array_store ) THEN
                     PRINT '(A,I8,A)','Found duplicate pressure, one from namelist and the other &
                     &from the first guess ',NINT(current_slab%level),' Pa.'
                  END IF
               END IF

            END DO find_dups

            !  Another thing, did we find a first-guess level for ptop?

            IF (  ABS ( current_slab%level - rec2%ptop ) .LT. 1 ) THEN
               need_ptop_level = .FALSE.
               IF ( rec4%print_array_store ) THEN
                  PRINT '(A)','Found first-guess level for ptop.'
               END IF
            END IF

            current_slab => current_slab%next_slab

         END DO first_guess_p_loop 

         EXIT find_p_peculiarities 

      ELSE 
         current_var => current_var%next_variable 

      END IF
   END DO find_p_peculiarities

   !  If there is no surface data for the 3d arrays, we need to add an extra level.
  
   IF ( .NOT. any_surface_data ) THEN
      number_of_original_levels = number_of_original_levels + 1 
   END IF

   !  If there are any replicated levels, reduce the number of original levels.

   number_of_original_levels = number_of_original_levels - num_same_press 

   !  If ptop is not in the first-guess levels, we need to stop.

   IF ( need_ptop_level ) THEN
      PRINT '(A,I8,A)','The requested PTOP variable in the namelist is ',NINT(rec2%ptop),' Pa.'
      PRINT '(A)','This must be in the first-guess field levels.'
      STOP 'change_ptop'
   END IF

   !  The vertical array of pressure is ALLOCATEd.  This is the sum
   !  of the data from the first guess file and the new levels from the
   !  NAMELIST.  Note that the surface level is included in the count
   !  from the first guess file.  

   ALLOCATE (pressure(number_of_original_levels+rec2%number_of_new_levels) )

   !  ALLOCATE the space required for the 3d data.  One array for
   !  each variable.  The vertical dimension is the sum of the
   !  input levels (from the first guess fields themselves) and the
   !  requested new levels (from the NAMELIST).  The additional 3 variables
   !  are to allow the temperature, relative humidity and geopotential
   !  height to be interpolated to cross points.

   ALLOCATE ( all_3d(jxf,ixf,number_of_original_levels+rec2%number_of_new_levels,num3d_fg+3) )

   !  ALLOCATE the space for the 2d arrays.  This is a bit trickier
   !  since some of the arrays (for hysterical reasons) are available
   !  at two different physical locations due to the staggering.

   ALLOCATE ( all_2d(jxf,ixf,num2d_ter_data+num2d_fg*3) )

   !  Initialize some of the required counters to zero.

   fg_variables_sfc_index = 0
   fg_variables_up_index  = 0
   fg_vertical_index      = 0

   !  We are going to loop over all of the variables, so start
   !  by pointing to the first one.  We loop until we are no
   !  longer pointing to an ASSOCIATEd variable.

   current_var => head
   search_all_variables : DO WHILE ( ASSOCIATED(current_var) ) 

      !  If there is only a single level, this is 2d data.  As
      !  is seen below, the 2d data is a piece of cake to store.

      twod_or_threed : IF ( current_var%num_levels .EQ. 1 ) THEN

         !  Find where to stick this data.  Fill an array of the
         !  name, then store the 2d data in the same location of
         !  the ALLOCATEd 2d array.  Increment the index of found
         !  data, then load the data, then save the name of the data.
         !  This is easy.

         fg_variables_sfc_index = fg_variables_sfc_index + 1
         all_2d(:,:,fg_variables_sfc_index) = current_var%slab%field              
         all_names_sfc(fg_variables_sfc_index) = current_var%name
         
         IF ( rec4%print_array_store ) THEN
            PRINT '(A,I8,A,A)','Store data in arrays:   Found 2d data index #',fg_variables_sfc_index, &
                             ' named ',all_names_sfc(fg_variables_sfc_index)(1:8)
         END IF
      
      !  If there is more than a single level, this is a 3d array.  As
      !  is judged by the following rest of the IF test, this is a bit
      !  more involved.  There are vertical levels to find, possible
      !  interpolations and maybe missing surface values.  Fasten your
      !  seat belt.

      ELSE IF ( current_var%num_levels .GT. 1 ) THEN

         !  Increment the index of found upper-air data.  This is a 
         !  count of the total number of 3d fields in the data set.

         fg_variables_up_index = fg_variables_up_index + 1

         !  Save the name of this 3d array.

         all_names_up(fg_variables_up_index) = current_var%name

         !  Point to the first level of this variable.  The data is 
         !  stored so that the largest pressure levels come first (ie,
         !  the surface).  The flag value for the surface pressure is
         !  200100 Pa.

         current_slab => current_var%slab

         !  Fill in the surface value into the first level of
         !  this variable.  Save the accompanying pressure level
         !  that defines the surface.  All of the data in the linked list
         !  are stored in 2d slices.  We build the 3d array with one slice 
         !  at a time, then move to the next variable name.
         
         fg_vertical_index = 1
         all_3d(:,:,fg_vertical_index,fg_variables_up_index) = current_slab%field

         !  Because this may be a level other than the surface, we force
         !  the pressure level to be defined as the surface.  The first 
         !  level in the array has to be the surface if this is a 3d array.

         pressure(fg_vertical_index) = 200100
         
         IF ( rec4%print_array_store ) THEN
            PRINT '(A,I8,A,A,A,I8)','Store data in arrays:   Found 3d data index #',fg_variables_up_index, &
                                   ' named ',all_names_up(fg_variables_up_index)(1:8),' pressure = ', &
                                     pressure(fg_vertical_index)
         END IF

         !  If this is the actual 200100 Pa level, this was indeed
         !  the surface data that we just loaded.  In that case we
         !  can increment the level.  Otherwise, we keep the level
         !  the same and reuse the data at the next level.  This could
         !  happen, for example, if there was no surface RH field.

         IF ( NINT(current_slab%level) .EQ. 200100 ) THEN
            current_slab => current_slab%next_slab
         END IF

         !  Initializations required for vertical interpolation.

         slab_bottom = current_slab%field
         pressure_bottom = current_slab%level
         vertically_interpolate = .TRUE.

         !  Now that we have the surface out of the way, let's loop
         !  through all of the other levels.  We have to check for the
         !  possibility of needing to do a vertical interpolation.  For
         !  each level (interpolated or not), we will increment the 
         !  vertical index, store the 3d data's 2d slice, and save the
         !  pressure level for this slice.

         get_all_levels : DO WHILE ( ASSOCIATED(current_slab) )

            !  Increment vertical location of the 3d array level.  This
            !  is effectively the vertical index in the 3d array.  Since we
            !  are in this loop, we know that there is another data level
            !  to process.
 
            fg_vertical_index = fg_vertical_index + 1

            !  Check for vertical interpolation.  This is a test that surrounds a  
            !  loop through all of the vertical levels to be interpolated to.
            !  The only "oops" is if a user tries to interpolate between the
            !  surface (200100 Pa) and 100000 Pa (for example).  This would be seen
            !  as a valid computation.  Hopefully, no one tries.

            do_vertical_interpolation : IF ( vertically_interpolate ) THEN

               !  To vertically interpolate, we need the surrounding pressure
               !  levels. We saved the previous pressure (pressure_bottom) at
               !  the end of this loop.  Here is the current pressure (pressure_top).

               pressure_top    = current_slab%level

               !  Loop over all possible new levels to interpolate to (skip the surface, index = 1).

               check_for_new_levels : DO new_levels_index = 2 , number_of_combined_levels

                  !  The candidate new pressure level.

                  pressure_middle = combined_pressure_levels(new_levels_index)

                  !  If the new vertical level is between the previous level and the 
                  !  current level, then perform the vertical interpolation linear in 
                  !  pressure (for horiz winds and RH), or linear in ln(p) (for temperature
                  !  and height).
   
                  IF      ( ( pressure_bottom .GT. pressure_middle ) .AND. &
                            ( pressure_middle .GT. pressure_top    ) ) THEN
   
                     slab_top = current_slab%field

                     IF      ( ( current_var%name(1:8) .EQ. 'U       ' ) .OR. &
                               ( current_var%name(1:8) .EQ. 'V       ' ) .OR. &
                               ( current_var%name(1:8) .EQ. 'RH      ' )      ) THEN
                        slab_middle = ( (pressure_bottom-pressure_middle)*slab_top      + &               
                                        (pressure_middle-pressure_top   )*slab_bottom ) / &               
                                        (pressure_bottom-pressure_top   )
                     ELSE IF ( ( current_var%name(1:8) .EQ. 'T       ' ) .OR. &
                               ( current_var%name(1:8) .EQ. 'HGT     ' )      ) THEN
                        slab_middle = ( (LOG(pressure_bottom)-LOG(pressure_middle))*slab_top      + &               
                                        (LOG(pressure_middle)-LOG(pressure_top   ))*slab_bottom ) / &               
                                        (LOG(pressure_bottom)-LOG(pressure_top   ))
                     ELSE
                        slab_middle = ( (pressure_bottom-pressure_middle)*slab_top      + &               
                                        (pressure_middle-pressure_top   )*slab_bottom ) / &               
                                        (pressure_bottom-pressure_top   )
                     END IF
   
                     !  A successful venture through a weighted mean.  Now, save the 
                     !  level for this pressure, the actual interpolated data, and finally
                     !  increment the vertical index to reflect the new level.

                     pressure(fg_vertical_index) = NINT(pressure_middle)
                     all_3d(:,:,fg_vertical_index,fg_variables_up_index) = slab_middle
         
                     IF ( rec4%print_array_store ) THEN
                        PRINT '(A,I8,A,A,A,I8)','Store data in arrays:   Found 3d data index #', &
                                                 fg_variables_up_index, &
                                               ' named ',all_names_up(fg_variables_up_index)(1:8), &
                                               ' pressure = ', pressure(fg_vertical_index)
                     END IF

                     fg_vertical_index = fg_vertical_index + 1

                  !  Insure that the data that was requested as new levels was not available
                  !  in the original data set.  For example, if we ask for 60000 Pa as a new level, but
                  !  the first guess data already provides it, there will be problems in the count of 
                  !  the vertical levels.  We have to check each variable since we do not have to have
                  !  the same number of levels for any given field.

                  ELSE IF ( ( ABS ( pressure_bottom - pressure_middle ) .LT. 1 ) .OR. &
                            ( ABS ( pressure_middle - pressure_top )    .LT. 1 ) ) THEN

                     IF ( rec4%print_array_store ) THEN
                        PRINT '(A)','Found a duplicated vertical level in the requested new '
                        PRINT '(A)','levels and in the first guess field.'
                     END IF
                     CYCLE check_for_new_levels

                  END IF
    
               END DO check_for_new_levels

            END IF do_vertical_interpolation

            !  Store the 3d data and vertical location for this level.  Whether or
            !  not there was a vertical interpolation, we still have the current level
            !  in the linked list to extract.  Save the pressure level and this slice
            !  of the 3d data.

            pressure(fg_vertical_index) = NINT(current_slab%level)
            all_3d(:,:,fg_vertical_index,fg_variables_up_index) = current_slab%field
         
            IF ( rec4%print_array_store ) THEN
               PRINT '(A,I8,A,A,A,I8)','Store data in arrays:   Found 3d data index #', &
                                        fg_variables_up_index, &
                                      ' named ',all_names_up(fg_variables_up_index)(1:8), &
                                      ' pressure = ', pressure(fg_vertical_index)
            END IF

            !  If we are doing vertical interpolations, well, then it is a requirement
            !  to save things for the next go round.  This is the "lower" level that
            !  will be used to check the trap of the middle level.

            IF ( vertically_interpolate ) THEN
               pressure_bottom = current_slab%level
               slab_bottom     = current_slab%field
            END IF

            !  Point to the next slab, which in this case, is point to the 
            !  next vertical level.

            current_slab => current_slab%next_slab

         END DO get_all_levels

      END IF twod_or_threed

      !  We have done all of the levels for this variable.  Point to
      !  the next variable.

      current_var => current_var%next_variable

   END DO search_all_variables

   !  All of the data is now stored in the new easy-to-open arrays.  If we do not have
   !  any surface data, this is the place we can vertically interpolate in a trivial
   !  fashion, as we have everything that we want.

   IF ( ( .NOT. any_surface_data_t ) .OR. &
        ( .NOT. any_surface_data_u ) .OR. &
        ( .NOT. any_surface_data_v ) .OR. &
        ( .NOT. any_surface_data_r ) ) THEN

      !  Where, oh where are the 3d fields?
      !  Oh where, oh where can they be?

      find_names_3d : DO loop = 1 , fg_variables_up_index
         IF      ( all_names_up(loop)(1:3) .EQ. 'T  ' ) THEN
            index_t = loop
         ELSE IF ( all_names_up(loop)(1:3) .EQ. 'U  ' ) THEN
            index_u = loop
         ELSE IF ( all_names_up(loop)(1:3) .EQ. 'V  ' ) THEN
            index_v = loop
         ELSE IF ( all_names_up(loop)(1:3) .EQ. 'RH ' ) THEN
            index_r = loop
         ELSE IF ( all_names_up(loop)(1:3) .EQ. 'HGT' ) THEN
            index_h = loop
         END IF
      END DO find_names_3d

      !  Did we find all of the anticipated 3d arrays?

      IF ( ( index_t .EQ. -1 ) .OR. &
           ( index_u .EQ. -1 ) .OR. &
           ( index_v .EQ. -1 ) .OR. &
           ( index_r .EQ. -1 ) .OR. &
           ( index_h .EQ. -1 ) ) THEN
         PRINT '(A)','Troubles finding the necessary 3d arrays to make the surface fields.'
         PRINT '(A,I2,A)','Temperature         index = ',index_t,'.'
         PRINT '(A,I2,A)','U-wind              index = ',index_u,'.'
         PRINT '(A,I2,A)','V-wind              index = ',index_v,'.'
         PRINT '(A,I2,A)','Relative Humidity   index = ',index_r,'.'
         PRINT '(A,I2,A)','Geopotential Height index = ',index_h,'.'
         STOP 'Could_not_find_3d_fields'
      END IF

      !  And now the 2d arrays.

      find_names_ter : DO loop = 1 , SIZE(ter_data)
         IF      ( ter_data(loop)%name_info(1:8) .EQ. 'TERRAIN ' ) THEN
            index_z = loop
         END IF
      END DO find_names_ter

      find_names_2d : DO loop = 1 , fg_variables_sfc_index
         IF      ( ( all_names_sfc(loop)(1:8) .EQ. 'PMSL    ' ) .OR. &
                   ( all_names_sfc(loop)(1:8) .EQ. 'PRESSURE' ) ) THEN
            index_p = loop
         END IF
      END DO find_names_2d

      !  Did we find all of the anticipated arrays?

      IF ( ( index_z .EQ. -1 ) .OR. &
           ( index_p .EQ. -1 ) ) THEN
         PRINT '(A)','Troubles finding the necessary 2d arrays to make the surface fields.'
         PRINT '(A,I2,A)','Terrain              index = ',index_z,'.'
         PRINT '(A,I2,A)','Sea Level Pressure   index = ',index_p,'.'
         STOP 'Could_not_find_2d_fields'
      END IF

      !  How big is our expected data to be?

      ii = SIZE ( all_3d , DIM=1 ) 
      jj = SIZE ( all_3d , DIM=2 ) 
      kk = SIZE ( all_3d , DIM=3 ) 

      !  We need a dummy array for mixing ratio and for the surface pressure.

      ALLOCATE(q(ii,jj,kk))
      ALLOCATE(psfc(ii,jj))

      !  First, we need to get the mixing ratio on pressure surfaces, but not at the surface level (so
      !  we start at index value 2).

      CALL mxratprs ( all_3d(:,:,:,index_r) , all_3d(:,:,:,index_t) , combined_pressure_levels , &
                      ii , jj , kk , q )

      !  With the mixing ratio, we can compute the surface pressure.

      CALL sfcprs ( all_3d(:,:,2:,index_t) , q(:,:,2:) , all_3d(:,:,2:,index_h) , &
                    all_2d(:,:,index_p) , ter_data(index_z)%array , combined_pressure_levels(2:) , &
                    ii , jj , kk , psfc )

      !  Compute the surface values through linear in pressure interpolation.
 
      IF ( .NOT. any_surface_data_t ) THEN
         CALL make_surface ( all_3d(:,:,:,index_t) , psfc , combined_pressure_levels , ii , jj , kk , 1 , all_3d(:,:,1,index_t) ) 
      END IF

      IF ( .NOT. any_surface_data_u ) THEN
         CALL make_surface ( all_3d(:,:,:,index_u) , psfc , combined_pressure_levels , ii , jj , kk , 0 , all_3d(:,:,1,index_u) ) 
      END IF

      IF ( .NOT. any_surface_data_v ) THEN
         CALL make_surface ( all_3d(:,:,:,index_v) , psfc , combined_pressure_levels , ii , jj , kk , 0 , all_3d(:,:,1,index_v) ) 
      END IF

      IF ( .NOT. any_surface_data_r ) THEN
         CALL make_surface ( all_3d(:,:,:,index_r) , psfc , combined_pressure_levels , ii , jj , kk , 1 , all_3d(:,:,1,index_r) ) 
      END IF

      !  Get rid of the dummy allocated arrays that were used in the surface computation.

      DEALLOCATE (q)
      DEALLOCATE (psfc)

   END IF

   !  Zap the temporary space used for the pressure array.

   DEALLOCATE (combined_pressure_levels)
   DEALLOCATE (dummy)
   
END SUBROUTINE proc_list_to_array
