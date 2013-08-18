MODULE header_data

   INTEGER            , DIMENSION(50,20) :: bhi
   CHARACTER (LEN=80) , DIMENSION(50,20) :: bhic
   REAL               , DIMENSION(20,20) :: bhr
   CHARACTER (LEN=80) , DIMENSION(20,20) :: bhrc

   TYPE sh
      INTEGER                :: num_dims
      INTEGER , DIMENSION(4) :: start_dims
      INTEGER , DIMENSION(4) :: end_dims
      REAL                   :: xtime
      CHARACTER (LEN= 4)     :: staggering
      CHARACTER (LEN= 4)     :: ordering
      CHARACTER (LEN=24)     :: current_date
      CHARACTER (LEN= 9)     :: name
      CHARACTER (LEN=25)     :: units
      CHARACTER (LEN=46)     :: description
   END TYPE sh

   TYPE(sh) :: small_header

   INTEGER , PARAMETER :: bh_flag  = 0 , &
                          sh_flag  = 1 , &
                          eot_flag = 2

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE read_bh ( unit )

      IMPLICIT NONE

      !  Input variables.

      INTEGER , INTENT(IN) :: unit

      !  Local variables.

      INTEGER :: flag
      INTEGER :: OK

      !  The first thing for any read is the flag.

      READ ( unit , IOSTAT = OK ) flag

      IF ( OK .NE. 0 ) THEN
         PRINT '(A,I5,A)','Troubles with big header flag read.  Error #',OK,'.'
         STOP 'bh_flag_read_error'
      END IF

      !  We got the flag, is it the right one?

      IF ( flag .NE. bh_flag ) THEN
         PRINT '(A,I1,A)','Wrong flag found:',flag,'.'
         STOP 'bh_flag_error'
      END IF

      !  So, we have the big header flag, so let's assume the big
      !  header is next.  No more error tests on the reads, just when
      !  the flags are handled.  We assume that if the flag is there, 
      !  the next record is as well.

      READ ( unit ) bhi , bhr , bhic , bhrc

   END SUBROUTINE read_bh

END MODULE header_data

