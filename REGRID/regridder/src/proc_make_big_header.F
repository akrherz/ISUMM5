SUBROUTINE proc_make_big_header

   USE header_data
   USE namelist_info

   IMPLICIT NONE

   INTEGER :: i , j

   !  Fill in the integer values for the big header.

   bhi ( 1, 1 ) = 2
   bhic( 1, 1 ) = 'Program name: REGRID, First-guess pressure-level analysis or forecast'

   bhi ( 2, 2 ) = 1
   bhic( 2, 2 ) = 'REGRID Version 3 MM5 System Format Edition Number'
 
   bhi ( 3, 2 ) = 7
   bhic( 3, 2 ) = 'REGRID Program Version Number'
 
   bhi ( 4, 2 ) = 0
   bhic( 4, 2 ) = 'REGRID Program Minor Revision Number'
 
   bhi ( 5, 2 ) = rec1%yyyy_st
   bhic( 5, 2 ) = 'Four digit year of the start time'
 
   bhi ( 6, 2 ) = rec1%mm_st
   bhic( 6, 2 ) = 'Two digit month (01 through 12) of the start time'
 
   bhi ( 7, 2 ) = rec1%dd_st
   bhic( 7, 2 ) = 'Two digit day (01 through 31) of the start time'
 
   bhi ( 8, 2 ) = rec1%hh_st
   bhic( 8, 2 ) = 'Two digit hour (00 through 23) of the start time'
 
   bhi ( 9, 2 ) = 0
   bhic( 9, 2 ) = 'Two digit minute (00 through 59) of the start time'
 
   bhi (10, 2 ) = 0
   bhic(10, 2 ) = 'Two digit second (00 through 59) of the start time'
 
   bhi (11, 2 ) = 0
   bhic(11, 2 ) = 'Four digit ten thousandth of a second (0000 through 9999) of the start time'
 
   bhi (12, 2 ) = number_of_original_levels+rec2%number_of_new_levels
   bhic(12, 2 ) = 'Anticipated number of vertical levels in 3d data'
 
   !  Now do the reals for the big header.
 
   bhr ( 1, 2 ) = rec1%time_increment_second
   bhrc( 1, 2 ) = 'Time increment between analysis times (s)'
 
   bhr ( 2, 2 ) = rec2%ptop
   bhrc( 2, 2 ) = 'Top pressure used in analysis, pressure defining model lid (Pa)'
 
   bhr ( 3, 2 ) = rec2%min_RH_value
   bhrc( 3, 2 ) = 'Minimum value used for RH on pressure surfaces'

   !  If requested, we can dump the values of the big header.

   IF ( rec4%print_header ) THEN

      prog_loop : DO j = 1 , 2
         IF      ( j .EQ. 1 ) THEN
            PRINT '(/A)','INTEGER VALUES FROM THE TERRAIN BIG HEADER'
         ELSE IF ( j .EQ. 2 ) THEN
            PRINT '(/A)','INTEGER VALUES FROM THE REGRID BIG HEADER'
         END IF
         DO i =1 , 50
            IF ( bhi(i,j) .GT. -500  ) THEN
               PRINT '(A,I4,A,I4,A,2x,I8,1X,A)','bhi(',i,',',j,')=',bhi(i,j),bhic(i,j)
            END IF
         END DO
   
         IF      ( j .EQ. 1 ) THEN
            PRINT '(/A)','REAL VALUES FROM THE TERRAIN BIG HEADER'
         ELSE IF ( j .EQ. 2 ) THEN
            PRINT '(/A)','REAL VALUES FROM THE REGRID BIG HEADER'
         END IF
         DO i =1 , 20
            IF ( bhr(i,j) .GT. -500. ) THEN
               PRINT '(A,I4,A,I4,A,f10.2,1X,A)','bhr(',i,',',j,')=',bhr(i,j),bhrc(i,j)
            END IF
         END DO
      END DO prog_loop
   END IF

END SUBROUTINE proc_make_big_header
