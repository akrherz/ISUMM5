SUBROUTINE proc_map_to_met_winds ( xlonc , xn , xlatc )

   USE header_data
   USE gridded_data
   USE namelist_info
   USE constants

   IMPLICIT NONE

   REAL , DIMENSION(jxf,ixf) :: u_map, v_map
   REAL , DIMENSION(jxf,ixf) :: alpha , diff
   INTEGER :: i, k , u_index , v_index , l_index
   REAL , PARAMETER :: rad_per_deg = pi / 180.

   !  Input data.

   REAL :: xlonc , xn , xlatc

   !  Which array is u and which is v?

   DO i = 1 , fg_variables_up_index
      IF      ( all_names_up(i)(1:8) .EQ. 'U       ' ) THEN
         u_index = i
      ELSE IF ( all_names_up(i)(1:8) .EQ. 'V       ' ) THEN
         v_index = i
      END IF
   END DO
   
   !  Where is the dot point longitude?

   l_index = -1
   DO i = 1 , fg_variables_sfc_index
      IF      ( all_names_sfc(i)(1:8) .EQ. 'LONGIDOT' ) THEN
         l_index = i
      END IF
   END DO

   IF ( l_index .EQ. -1 ) THEN
      PRINT '(A)','Could not find the dot point longitude.'
      PRINT '(A)','This should not happen.'
      STOP 'Where_oh_where_is_the_longitude'
   END IF

   !  Store the important angles.

   diff = ( all_2d(:,:,l_index) -xlonc ) 

   WHERE ( diff .GT.  180. ) diff = diff - 360.
   WHERE ( diff .LT. -180. ) diff = diff + 360.

   alpha = diff * xn * rad_per_deg * SIGN ( 1. , xlatc ) 

   DO k = 1 , number_of_original_levels+rec2%number_of_new_levels

      !  Load up the map wind fields.

      u_map = all_3d(:,:,k,u_index)
      v_map = all_3d(:,:,k,v_index)

      !  Compute unrotated winds.

      all_3d(:,:,k,u_index) = v_map * SIN(alpha) + u_map * COS(alpha) 
      all_3d(:,:,k,v_index) = v_map * COS(alpha) - u_map * SIN(alpha) 

   END DO

END SUBROUTINE proc_map_to_met_winds
