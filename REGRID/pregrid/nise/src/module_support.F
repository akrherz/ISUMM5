MODULE support

   INTEGER , PARAMETER :: ix = 721 , jx = 721
#ifdef BIT
   INTEGER , PARAMETER :: record_len = (4*ix*jx+2634+132)/4 + 1
#else
   INTEGER , PARAMETER :: record_len =  4*ix*jx+2634+132
#endif
   REAL , DIMENSION(ix,jx,2) :: seaice
!  REAL , PARAMETER :: flag = 0.
   REAL , PARAMETER :: flag = -1.E20

CONTAINS

   SUBROUTINE read_seaice ( filename ) 

      IMPLICIT NONE

      CHARACTER (LEN=1) :: data1(ix*jx),data2(ix*jx),data3(ix*jx),data4(ix*jx)
      CHARACTER (LEN=1) :: header1(2634),header2(132)
      INTEGER :: i , j , l
      CHARACTER (LEN=28) :: filename

      LOGICAL :: any_other_ice
      INTEGER , PARAMETER :: idelta = 13 ! for the 721x721 domain
      REAL :: seaice_avg
      INTEGER :: icount

      !  Read SSM/I sea ice file 

      OPEN ( UNIT=16, &
             FILE=filename, &
             STATUS='OLD', &
             RECL=record_len, &
             ACCESS='DIRECT', &
             FORM='UNFORMATTED')

      ! record_len: record length for SSMI sea ice data
      ! data1 : Northern hemisphere sea ice data age
      ! data2 : Northern hemisphere sea ice / snow cover data
      ! data3 : Southern hemisphere sea ice data age
      ! data4 : Southern hemisphere sea ice / snow cover data
      ! Header1: HDF-EOS header
      ! header2: HDF-EOS header

      READ(16,REC=1) header1,data1,data2,header2,data3,data4
      CLOSE(UNIT=16)

      !  Store SSM/I Northern and Southern hemisphere sea ice data in array seaice
      !  Only 1 to 100% is sea ice.  The values of 0 are land, and above are other
      !  snow things, 255 is open water.

      !  ye old seaice array values as follows
      !  0       : snow free land
      !  1-100   : sea ice concentration (%)
      !  101     : permanent ice
      !  102     : not used
      !  103     : dry snow
      !  104     : wet snow
      !  105-251 : not used
      !  252     : mixed pixels (coastline)
      !  253     : suspect ice value
      !  254     : undefined
      !  255     : ocean

      !  North pole

      DO j=1,jx
         DO i=1,ix
            l=jx*(i-1)+j
            seaice(i,j,1)=REAL(ICHAR(data2(l)))
            IF   ( seaice(i,j,1) .GT. 254.5 ) THEN
               seaice(i,j,1) = 0.
            ELSE IF ( ( seaice(i,j,1) .LT. 1 ) .OR. ( seaice(i,j,1) .GT. 100. ) ) THEN
               seaice(i,j,1) = flag
            ELSE
               seaice(i,j,1) = seaice(i,j,1) / 100.
            END IF
         ENDDO
      ENDDO

      !  North pole has a 3 degree hole.

      any_other_ice = .FALSE.
      icount = 0
      seaice_avg = 0.

      DO j = jx/2-idelta , jx/2+idelta
         DO i = ix/2-idelta , ix/2+idelta
            IF ( ( seaice(i,j,1) .GT. 0 ) .AND. ( seaice(i,j,1) .LE. 1 ) ) THEN
               seaice_avg =  seaice_avg + seaice(i,j,1)
               icount = icount + 1
               any_other_ice = .TRUE.
            END IF
         END DO
      END DO

      IF ( any_other_ice ) THEN
         seaice_avg =  seaice_avg / REAL ( icount ) 
print *,'seaice_avg = ',seaice_avg

         DO i = ix/2-idelta , ix/2+idelta
            DO j = jx/2-idelta , jx/2+idelta
               seaice(i,j,1) = seaice_avg
            END DO
         END DO
      END IF

      !  South pole

      DO j=1,jx
         DO i=1,ix
            l=jx*(i-1)+j
            seaice(i,j,2)=REAL(ICHAR(data4(l)))
            IF   ( seaice(i,j,2) .GT. 254.5 ) THEN
               seaice(i,j,2) = 0.
            ELSE IF ( ( seaice(i,j,2) .LT. 1 ) .OR. ( seaice(i,j,2) .GT. 100. ) ) THEN
               seaice(i,j,2) = flag
            ELSE
               seaice(i,j,2) = seaice(i,j,2) / 100.
            END IF
         ENDDO
      ENDDO

   END SUBROUTINE read_seaice

END MODULE support

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
