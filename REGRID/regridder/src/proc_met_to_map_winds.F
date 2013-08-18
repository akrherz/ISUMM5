SUBROUTINE proc_met_to_map_winds

   USE header_data
   USE gridded_data
   USE namelist_info
   USE constants

   IMPLICIT NONE

   REAL , DIMENSION(jxf,ixf) :: u_met, v_met
   REAL , DIMENSION(jxf,ixf) :: alpha , diff
   INTEGER :: i, k , u_index , v_index , l_index
   REAL , PARAMETER :: rad_per_deg = pi / 180.

   !  This is not necessary for a Mercator projection.

   IF ( projection(1:8) .NE. 'MERCATOR' ) THEN

      !  Which array is u and which is v?
   
      DO i = 1 , fg_variables_up_index
         IF      ( all_names_up(i)(1:8) .EQ. 'U       ' ) THEN
            u_index = i
         ELSE IF ( all_names_up(i)(1:8) .EQ. 'V       ' ) THEN
            v_index = i
         END IF
      END DO
      
      !  Where is the dot point longitude?
   
      DO i = 1 , fg_variables_sfc_index
         IF      ( all_names_sfc(i)(1:8) .EQ. 'LONGIDOT' ) THEN
            l_index = i
         END IF
      END DO
   
      !  Store the important angles.
   
      diff = ( center_longitude - all_2d(:,:,l_index) ) 
      where ( diff .GT.  180. ) diff = diff - 360.
      where ( diff .LT. -180. ) diff = diff + 360.

      alpha = diff * cone_factor * rad_per_deg * SIGN ( 1. , center_latitude ) 
   
      DO k = 1 , number_of_original_levels+rec2%number_of_new_levels
   
         !  Load up the met wind fields on each pressure level.
   
         u_met = all_3d(:,:,k,u_index)
         v_met = all_3d(:,:,k,v_index)

         !  Compute rotated winds.
   
         all_3d(:,:,k,u_index) = v_met * SIN(alpha) + u_met * COS(alpha) 
         all_3d(:,:,k,v_index) = v_met * COS(alpha) - u_met * SIN(alpha) 

      END DO

   END IF

END SUBROUTINE proc_met_to_map_winds
