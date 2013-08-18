MODULE gauss

   INTEGER , PARAMETER , PRIVATE :: HIGH = 8
   LOGICAL :: done_this_gauss_before 
   REAL , ALLOCATABLE , DIMENSION(:) :: gauss_lat


CONTAINS

   SUBROUTINE gausll ( nlat , lat_sp )

      IMPLICIT NONE
   
      INTEGER                            :: nlat , i
      REAL (KIND=HIGH) , PARAMETER       :: pi = 3.141592653589793
      REAL (KIND=HIGH)                   :: sum1, sum2, sum3, sum4, xn, a, b
      REAL (KIND=HIGH) , DIMENSION(nlat) :: cosc , gwt , sinc , colat , wos2 , lat , mlat
      REAL             , DIMENSION(nlat) :: lat_sp
   
      CALL lggaus(nlat, cosc, gwt, sinc, colat, wos2)
   
      DO i = 1, nlat
         lat(i) = acos(sinc(i)) * 180._HIGH / pi
         IF (i.gt.nlat/2) lat(i) = -lat(i)
      END DO
   
      lat_sp = REAL(lat)

   END SUBROUTINE gausll

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE LGGAUS( NLAT, COSC, GWT, SINC, COLAT, WOS2 )
 
      IMPLICIT NONE

      !  LGGAUS finds the Gaussian latitudes by finding the roots of the
      !  ordinary Legendre polynomial of degree NLAT using Newton's
      !  iteration method.
      
      !  On entry:
            integer NLAT ! the number of latitudes (degree of the polynomial)
      
      !  On exit: for each Gaussian latitude
      !     COSC   - cos(colatitude) or sin(latitude)
      !     GWT    - the Gaussian weights
      !     SINC   - sin(colatitude) or cos(latitude)
      !     COLAT  - the colatitudes in radians
      !     WOS2   - Gaussian weight over sin**2(colatitude)

      REAL (KIND=HIGH) , DIMENSION(nlat) :: COSC , GWT , SINC , COLAT  , WOS2 
      REAL (KIND=HIGH) , PARAMETER       :: pi = 3.141592653589793

      !  Convergence criterion for iteration of cos latitude

      REAL , PARAMETER :: XLIM  = 1.0E-14

      INTEGER :: nzero, i, j
      REAL (KIND=HIGH) :: fi, fi1, a, b, g, gm, gp, gt, delta, c, d

      !  The number of zeros between pole and equator

      NZERO = NLAT/2

      !  Set first guess for cos(colat)

      DO I=1,NZERO
         COSC(I) = SIN( (I-0.5)*PI/NLAT + PI*0.5 )
      END DO

      !  Constants for determining the derivative of the polynomial
      FI  = NLAT
      FI1 = FI+1.0
      A   = FI*FI1 / SQRT(4.0*FI1*FI1-1.0)
      B   = FI1*FI / SQRT(4.0*FI*FI-1.0)

      !  Loop over latitudes, iterating the search for each root

      DO I=1,NZERO
         J=0

         !  Determine the value of the ordinary Legendre polynomial for
         !  the current guess root

            DO
            CALL LGORD( G, COSC(I), NLAT )
   
            !  Determine the derivative of the polynomial at this point
   
            CALL LGORD( GM, COSC(I), NLAT-1 )
            CALL LGORD( GP, COSC(I), NLAT+1 )
            GT = (COSC(I)*COSC(I)-1.0) / (A*GP-B*GM)
   
            !  Update the estimate of the root
   
            DELTA   = G*GT
            COSC(I) = COSC(I) - DELTA
   
            !  If convergence criterion has not been met, keep trying
   
            J = J+1
            IF( ABS(DELTA).GT.XLIM ) CYCLE
   
   !        PRINT*,' LAT NO.',I,J,' ITERATIONS'
   
            !  Determine the Gaussian weights
   
            C      = 2.0 *( 1.0-COSC(I)*COSC(I) )
            CALL LGORD( D, COSC(I), NLAT-1 )
            D      = D*D*FI*FI
            GWT(I) = C *( FI-0.5 ) / D
            EXIT

         END DO

      END DO

      !  Determine the colatitudes and sin(colat) and weights over sin**2

      DO I=1,NZERO
         COLAT(I)= ACOS(COSC(I))
         SINC(I) = SIN(COLAT(I))
         WOS2(I) = GWT(I) /( SINC(I)*SINC(I) )
      END DO

      !  If NLAT is odd, set values at the equator

      IF( MOD(NLAT,2) .NE. 0 ) THEN
         I       = NZERO+1
         COSC(I) = 0.0
         C       = 2.0
         CALL LGORD( D, COSC(I), NLAT-1 )
         D       = D*D*FI*FI
         GWT(I)  = C *( FI-0.5 ) / D
         COLAT(I)= PI*0.5
         SINC(I) = 1.0
         WOS2(I) = GWT(I)
      END IF

      !  Determine the southern hemisphere values by symmetry

      DO I=NLAT-NZERO+1,NLAT
         COSC(I) =-COSC(NLAT+1-I)
         GWT(I)  = GWT(NLAT+1-I)
         COLAT(I)= PI-COLAT(NLAT+1-I)
         SINC(I) = SINC(NLAT+1-I)
         WOS2(I) = WOS2(NLAT+1-I)
      END DO

!     PRINT*,'NLAT=',NLAT
!     PRINT*,'COLATS'
!     PRINT '(1X,I3,F16.12,2X,F8.2)',(I,COLAT(I),COLAT(I)*180./PI,I=1,NLAT)
!     PRINT*,'COS(COLAT), SIN(COLAT)'
!     PRINT '(1X,I3,F16.12,2X,F16.12)',(I,COSC(I),SINC(I),I=1,NLAT)
!     PRINT*,'WEIGHT, GWT/COS**2'
!     PRINT '(1X,I3,F16.12,2X,F16.12)',(I,GWT(I),WOS2(I),I=1,NLAT)

   END SUBROUTINE lggaus

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE LGORD( F, COSC, N )

      IMPLICIT NONE

      !  LGORD calculates the value of an ordinary Legendre polynomial at a
      !  latitude.
      
      !  On entry:
      !     COSC - cos(colatitude)
      !     N      - the degree of the polynomial
      
      !  On exit:
      !     F      - the value of the Legendre polynomial of degree N at
      !              latitude asin(COSC)

      REAL (KIND=HIGH) :: s1, c4, a, b, fk, f, cosc, colat, c1, fn, ang
      INTEGER :: n, k

      !  Determine the colatitude

      COLAT = ACOS(COSC)

      C1 = SQRT(2.0_HIGH)
      DO K=1,N
         C1 = C1 * SQRT( 1.0 - 1.0/(4*K*K) )
      END DO

      FN = N
      ANG= FN * COLAT
      S1 = 0.0
      C4 = 1.0
      A  =-1.0
      B  = 0.0
      DO K=0,N,2
         IF (K.EQ.N) C4 = 0.5 * C4
         S1 = S1 + C4 * COS(ANG)
         A  = A + 2.0
         B  = B + 1.0
         FK = K
         ANG= COLAT * (FN-FK-2.0)
         C4 = ( A * (FN-B+1.0) / ( B * (FN+FN-A) ) ) * C4
      END DO 

      F = S1 * C1

   END SUBROUTINE lgord

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE llxy_gauss ( lat , lon , x , y , wrap , & 
                           idim , startlat , startlon , deltalon , nlat ) 

      IMPLICIT NONE

      REAL    , INTENT(IN)  :: lat , lon
      REAL    , INTENT(IN)  :: startlat , startlon , deltalon
      INTEGER , INTENT(IN)  :: idim , nlat
      REAL    , INTENT(OUT) :: x , y
      LOGICAL , INTENT(OUT) :: wrap

      INTEGER :: i , i_low
      LOGICAL :: found = .FALSE.
      REAL    :: diff_1 , diff_nlat

      !  Set a flag that we have processed this particular Gaussian grid so
      !  that we can avoid an expensive computation.

      done_this_gauss_before = .TRUE.

      !  Initialize the array that will hold the Gaussian latitudes.

      IF ( ALLOCATED ( gauss_lat ) ) THEN
         DEALLOCATE ( gauss_lat ) 
      END IF

      !  Get the needed space for our array.

      ALLOCATE ( gauss_lat(nlat*2) )

      !  Compute the Gaussian latitudes.

      CALL gausll( nlat*2 , gauss_lat ) 

      !  Now, these could be upside down from what we want, so let's check.
      !  We take advantage of the equatorial symmetry to remove any sort of 
      !  array re-ordering.

      IF ( ABS(gauss_lat(1) - startlat) .GT. 0.01 ) THEN
         gauss_lat = -1. * gauss_lat
      END IF
         
      !  Just a sanity check.

      IF ( ABS(gauss_lat(1) - startlat) .GT. 0.01 ) THEN
         PRINT '(A)','Oops, something is not right with the Gaussian latitude computation.'
         PRINT '(A,F8.3,A)','The input data gave the starting latitude as ',startlat,'.'
         PRINT '(A,F8.3,A)','This routine computed the starting latitude as +-',ABS(gauss_lat(1)),'.'
         PRINT '(A,F8.3,A)','The difference is larger than 0.01 degrees, which is not expected.'
         STOP 'Gaussian_latitude_computation'
      END IF

      !  The easy one first, get the x location.  The calling routine has already made
      !  sure that the necessary assumptions concerning the sign of the deltalon and the
      !  relative east/west'ness of the longitude and the starting longitude are consistent
      !  to allow this easy computation.

      x = ( lon - startlon ) / deltalon + 1.

      !  Since this is a global data set, we need to be concerned about wrapping the
      !  fields around the globe.

      IF      ( ( deltalon .GT. 0 ) .AND. &
                ( FLOOR((lon-startlon)/deltalon) + 1 .GE. idim ) .AND. &
                ( lon + deltalon .GE. startlon + 360 ) ) THEN
         wrap = .TRUE.
         x = idim
      ELSE IF ( ( deltalon .LT. 0 ) .AND. &
                ( FLOOR((lon-startlon)/deltalon) + 1 .GE. idim ) .AND. &
                ( lon + deltalon .LE. startlon - 360 ) ) THEN
         wrap = .TRUE.
         x = idim
      END IF

      !  Yet another quicky test, can we find bounding values?  If not, then we may be
      !  dealing with putting data to a polar projection, so just give them them maximal
      !  value for the location.  This is an OK assumption for the interpolation across the
      !  top of the pole, given how close the longitude lines are.

      IF ( ABS(lat) .GT. ABS(gauss_lat(1)) ) THEN

         diff_1    = lat - gauss_lat(1)
         diff_nlat = lat - gauss_lat(nlat*2)

         IF ( ABS(diff_1) .LT. ABS(diff_nlat) ) THEN
            y = 1
         ELSE
            y = nlat*2
         END IF

      !  If the latitude is between the two bounding values, we have to search and interpolate.

      ELSE

         DO i = 1 , nlat*2 -1
            IF ( ( gauss_lat(i) - lat ) * ( gauss_lat(i+1) - lat ) .LE. 0 ) THEN
               found = .TRUE.
               i_low = i
               EXIT
            END IF
         END DO

         !  Everything still OK?
  
         IF ( .NOT. found ) THEN
            PRINT '(A)','Troubles in river city.  No bounding values of latitude found in the Gaussian routines.'
            STOP 'Gee_no_bounding_lats_Gaussian'
         END IF

         y = ( ( gauss_lat(i_low) - lat                ) * ( i_low + 1 ) + &
               ( lat              - gauss_lat(i_low+1) ) * ( i_low     ) ) / &
               ( gauss_lat(i_low) - gauss_lat(i_low+1) )

      END IF

   END SUBROUTINE llxy_gauss 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE gauss
