      subroutine gausll(NLAT, startlat, deltalat)
      implicit none

      integer nlat
      real startlat
      real deltalat
      double precision pi
      parameter (PI = 3.141592653589793)
      double precision cosc(180)
      double precision gwt(180)
      double precision sinc(180)
      double precision colat(180)
      double precision wos2(180)
      double precision lat(180), mlat(180)
      
      integer i, iskip
      double precision sum1, sum2, sum3, sum4, xn, a, b

      call lggaus(nlat, cosc, gwt, sinc, colat, wos2)

      do i = 1, nlat
         lat(i) = acos(sinc(i)) * 180. / pi
         if (i.gt.nlat/2) lat(i) = -lat(i)
      enddo

      iskip = 15

      sum1 = 0.
      sum2 = 0.
      sum3 = 0.
      sum4 = 0.
      xn = float(nlat-iskip*2)
      do i = iskip+1, nlat-iskip
         sum1 = sum1 + lat(i)*float(i)
         sum2 = sum2 + float(i)
         sum3 = sum3 + lat(i)
         sum4 = sum4 + float(i)**2
      enddo

      b = (xn*sum1 - sum2*sum3) / (xn*sum4 - sum2**2)
      a = (sum3 - b * sum2) / xn

      do i = 1, nlat
         mlat(i) = a + float(i)*b
      enddo


      startlat = sngl(mlat(1))
      deltalat = -sngl(b)

      end

      SUBROUTINE LGGAUS( NLAT, COSC, GWT, SINC, COLAT, WOS2 )
      implicit none
!
!  LGGAUS finds the Gaussian latitudes by finding the roots of the
!  ordinary Legendre polynomial of degree NLAT using Newton's
!  iteration method.
!
!  On entry:
      integer NLAT ! the number of latitudes (degree of the polynomial)
!
!  On exit: for each Gaussian latitude
!     COSC   - cos(colatitude) or sin(latitude)
!     GWT    - the Gaussian weights
!     SINC   - sin(colatitude) or cos(latitude)
!     COLAT  - the colatitudes in radians
!     WOS2   - Gaussian weight over sin**2(colatitude)
!
      double precision COSC(180), GWT(180), SINC(180), COLAT(180) &
              , WOS2(180)
!
!-----------------------------------------------------------------------
      double precision xlim, pi
      parameter (PI = 3.141592653589793)
!
!    -convergence criterion for iteration of cos latitude
      parameter (XLIM  = 1.0E-14)
      integer nzero, i, j
      double precision fi, fi1, a, b, g, gm, gp, gt, delta, c, d
!
!    -the number of zeros between pole and equator
      NZERO = NLAT/2
!
!    -set first guess for cos(colat)
      DO 10 I=1,NZERO
         COSC(I) = SIN( (I-0.5)*PI/NLAT + PI*0.5 )
   10 CONTINUE
!
!    -constants for determining the derivative of the polynomial
      FI  = NLAT
      FI1 = FI+1.0
      A   = FI*FI1 / SQRT(4.0*FI1*FI1-1.0)
      B   = FI1*FI / SQRT(4.0*FI*FI-1.0)
!
!    -loop over latitudes, iterating the search for each root
      DO 40 I=1,NZERO
         J=0
!
!       -determine the value of the ordinary Legendre polynomial for
!       -the current guess root
   30    CALL LGORD( G, COSC(I), NLAT )
!
!       -determine the derivative of the polynomial at this point
         CALL LGORD( GM, COSC(I), NLAT-1 )
         CALL LGORD( GP, COSC(I), NLAT+1 )
         GT = (COSC(I)*COSC(I)-1.0) / (A*GP-B*GM)
!
!       -update the estimate of the root
         DELTA   = G*GT
         COSC(I) = COSC(I) - DELTA
!
!       -if convergence criterion has not been met, keep trying
         J = J+1
         IF( ABS(DELTA).GT.XLIM ) GO TO 30
!        PRINT*,' LAT NO.',I,J,' ITERATIONS'
!
!       -determine the Gaussian weights
         C      = 2.0 *( 1.0-COSC(I)*COSC(I) )
         CALL LGORD( D, COSC(I), NLAT-1 )
         D      = D*D*FI*FI
         GWT(I) = C *( FI-0.5 ) / D
   40 CONTINUE
!
!    -determine the colatitudes and sin(colat) and weights over sin**2
      DO 50 I=1,NZERO
         COLAT(I)= ACOS(COSC(I))
         SINC(I) = SIN(COLAT(I))
         WOS2(I) = GWT(I) /( SINC(I)*SINC(I) )
   50 CONTINUE
!
!    -if NLAT is odd, set values at the equator
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
!
!    -determine the southern hemisphere values by symmetry
      DO 60 I=NLAT-NZERO+1,NLAT
         COSC(I) =-COSC(NLAT+1-I)
         GWT(I)  = GWT(NLAT+1-I)
         COLAT(I)= PI-COLAT(NLAT+1-I)
         SINC(I) = SINC(NLAT+1-I)
         WOS2(I) = WOS2(NLAT+1-I)
   60 CONTINUE
!     PRINT*,'NLAT=',NLAT
!     PRINT*,'COLATS'
!     PRINT 101,(I,COLAT(I),COLAT(I)*180./PI,I=1,NLAT)
  101 FORMAT(1X,I3,F16.12,2X,F8.2)
  102 FORMAT(1X,I3,F16.12,2X,F16.12)
!     PRINT*,'COS(COLAT), SIN(COLAT)'
!     PRINT 102,(I,COSC(I),SINC(I),I=1,NLAT)
!     PRINT*,'WEIGHT, GWT/COS**2'
!     PRINT 102,(I,GWT(I),WOS2(I),I=1,NLAT)
!
      RETURN
      END
      SUBROUTINE LGORD( F, COSC, N )
      implicit none
!
!  LGORD calculates the value of an ordinary Legendre polynomial at a
!  latitude.
!
!  On entry:
!     COSC - cos(colatitude)
!     N      - the degree of the polynomial
!
!  On exit:
!     F      - the value of the Legendre polynomial of degree N at
!              latitude asin(COSC)
      double precision s1, c4, a, b, fk, f, cosc, colat, c1, fn, ang
      integer n, k
!
!------------------------------------------------------------------------
!
!    -determine the colatitude
      COLAT = ACOS(COSC)
!
      C1 = SQRT(2.0)
      DO 20 K=1,N
         C1 = C1 * SQRT( 1.0 - 1.0/(4*K*K) )
   20 CONTINUE
!
      FN = N
      ANG= FN * COLAT
      S1 = 0.0
      C4 = 1.0
      A  =-1.0
      B  = 0.0
      DO 30 K=0,N,2
         IF (K.EQ.N) C4 = 0.5 * C4
         S1 = S1 + C4 * COS(ANG)
         A  = A + 2.0
         B  = B + 1.0
         FK = K
         ANG= COLAT * (FN-FK-2.0)
         C4 = ( A * (FN-B+1.0) / ( B * (FN+FN-A) ) ) * C4
   30 CONTINUE
      F = S1 * C1
!
      RETURN
      END
