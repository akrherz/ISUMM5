SUBROUTINE proc_zap_space_array

   USE namelist_info
   USE terrain_data
   USE gridded_data

   IMPLICIT NONE

   INTEGER :: i

   IF ( ( ( rec1%current_date(1:13) .EQ. rec1%ending_date(1:13) ) .AND. ( MOD(rec1%time_increment_second,3600) .EQ. 0 ) ) .OR. &
        ( ( rec1%current_date(1:16) .EQ. rec1%ending_date(1:16) ) .AND. ( MOD(rec1%time_increment_second,  60) .EQ. 0 ) ) .OR. &
          ( rec1%current_date(1:19) .EQ. rec1%ending_date(1:19) ) ) THEN                                                       

      DO i = 1 , num2d_ter_data
         DEALLOCATE (ter_data(i)%array)
      END DO
      DEALLOCATE (ter_data)
      
      IF (rec4%print_debug) THEN
         PRINT '(A)','DeALLOCATED space for TERRAIN file data.' 
      END IF
   END IF

   DEALLOCATE (all_3d)
   DEALLOCATE (all_2d)
   DEALLOCATE (pressure)

   IF (rec4%print_debug) THEN
      PRINT '(A)','DeALLOCATED space from 3d, 2d and pressure arrays for this time period.'
   END IF

END SUBROUTINE proc_zap_space_array
