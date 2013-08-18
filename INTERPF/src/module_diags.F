MODULE diags

   USE util
   USE phys_consts

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE mxratprs (rh, t, ppa, imxm, jmxm, kx, WRTH2O , q)

      IMPLICIT NONE

      INTEGER                     :: I
      INTEGER                     :: IMXM
      INTEGER                     :: J
      INTEGER                     :: JMXM
      INTEGER                     :: K
      INTEGER                     :: KX

      LOGICAL                     :: wrth2o

      REAL                        :: EW
      REAL                        :: PPA         ( : )
      REAL                        :: P           ( KX )
      REAL                        :: Q           ( : , : , : )
      REAL                        :: Q1
      REAL                        :: RH          ( : , : , : )
      REAL                        :: T           ( : , : , : )
      REAL                        :: T1

      REAL,         PARAMETER     :: T_REF       = 0.0
      REAL,         PARAMETER     :: MW_AIR      = 28.966
      REAL,         PARAMETER     :: MW_VAP      = 18.0152

      REAL,         PARAMETER     :: A0       = 6.107799961
      REAL,         PARAMETER     :: A1       = 4.436518521e-01
      REAL,         PARAMETER     :: A2       = 1.428945805e-02
      REAL,         PARAMETER     :: A3       = 2.650648471e-04
      REAL,         PARAMETER     :: A4       = 3.031240396e-06
      REAL,         PARAMETER     :: A5       = 2.034080948e-08
      REAL,         PARAMETER     :: A6       = 6.136820929e-11

      REAL,         PARAMETER     :: ES0 = 6.1121

      REAL,         PARAMETER     :: C1       = 9.09718
      REAL,         PARAMETER     :: C2       = 3.56654
      REAL,         PARAMETER     :: C3       = 0.876793
      REAL,         PARAMETER     :: EIS      = 6.1071
      REAL                        :: RHS
      REAL,         PARAMETER     :: TF       = 273.16
      REAL                        :: TK

      REAL                        :: ES
      REAL                        :: QS
      REAL,         PARAMETER     :: EPS         = 0.622
      REAL,         PARAMETER     :: SVP1        = 0.6112
      REAL,         PARAMETER     :: SVP2        = 17.67
      REAL,         PARAMETER     :: SVP3        = 29.65
      REAL,         PARAMETER     :: SVPT0       = 273.15

      !  This function is designed to compute (q) from basic variables
      !  p (mb), t(K) and rh(0-100%) to give (q) in (kg/kg).  The reference
      !  temperature t_ref (c) is used to describe the temperature at which
      !  the liquid and ice phase change occurs.

      p = ppa * 0.01

      DO k = 1, kx
         DO j = 1, jmxm
            DO i = 1, imxm
                  rh(i,j,k) = MIN ( MAX ( rh(i,j,k) ,  1. ) , 100. ) 
            END DO
         END DO
      END DO

      IF ( wrth2o ) THEN
         DO k = 1, kx
            DO j = 1, jmxm
               DO i = 1, imxm
                  es = svp1 * 10. * EXP(svp2 * (t(i,j,k) - svpt0) / (t(i,j,k) - svp3))
                  qs = eps * es / (p(k) - es)
                  q(i,j,k) = MAX(0.01 * rh(i,j,k) * qs,0.0)
               END DO
            END DO
         END DO

      ELSE
         DO k = 1, kx
            DO j = 1, jmxm
               DO i = 1, imxm

                  t1 = t(i,j,k) - 273.16

                  !  Obviously dry.

                  IF ( t1 .LT. -200. ) THEN
                     q(i,j,k) = 0

                  ELSE

                     !  First compute the ambient vapor pressure of water

                     IF ( ( t1 .GE. t_ref ) .AND. ( t1 .GE. -47.) ) THEN    ! liq phase ESLO
                        ew = a0 + t1 * (a1 + t1 * (a2 + t1 * (a3 + t1 * (a4 + t1 * (a5 + t1 *  a6)))))

!else if (t1 .le. t_ref .and. t1 .ge. -47.) then !liq phas poor ES
                     ELSE IF ( ( t1 .GE. t_ref ) .AND. ( t1 .LT. -47. ) ) then !liq phas poor ES
                        ew = es0 * EXP(17.67 * t1 / ( t1 + 243.5))

                     ELSE                                         ! ice phase
                        tk = t(i,j,k)
                        rhs = -c1 * (tf / tk - 1.) - c2 * ALOG10(tf / tk) + &
                              c3 * (1. - tk / tf) +      ALOG10(eis)
                        ew = 10. ** rhs

                     END IF

                     ew = MAX ( ew , 0. )

                     !  Now sat vap pres obtained compute local vapor pressure
  
                     ew = ew * rh(i,j,k) * 0.01

                     !  Now compute the specific humidity using the partial vapor
                     !  pressures of water vapor (ew) and dry air (p-ew).

                     q1 = mw_vap * ew
                     q1 = q1 / (q1 + mw_air * (p(k) - ew))

                     q(i,j,k) = q1 / (1. - q1 )

                   END IF

               END DO
            END DO
         END DO

      ENDIF

   END SUBROUTINE mxratprs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE mxratsig (rh, t, sighup, p, ptop, imxm, jmxm, kxs, WRTH2O , q)

      IMPLICIT NONE

      INTEGER                     :: I
      INTEGER                     :: IMXM
      INTEGER                     :: J
      INTEGER                     :: JMXM
      INTEGER                     :: K
      INTEGER                     :: KXS

      LOGICAL                     :: wrth2o

      REAL                        :: EW
      REAL                        :: P           ( : , : )
      REAL                        :: PTOP
      REAL                        :: Q           ( : , : , : )
      REAL                        :: Q1
      REAL                        :: RH          ( : , : , : )
      REAL                        :: SIGHUP      ( : )
      REAL                        :: T           ( : , : , : )
      REAL                        :: T1

      REAL,         PARAMETER     :: T_REF       = 0.0
      REAL,         PARAMETER     :: MW_AIR      = 28.966
      REAL,         PARAMETER     :: MW_VAP      = 18.0152

      REAL,         PARAMETER     :: A0       = 6.107799961
      REAL,         PARAMETER     :: A1       = 4.436518521e-01
      REAL,         PARAMETER     :: A2       = 1.428945805e-02
      REAL,         PARAMETER     :: A3       = 2.650648471e-04
      REAL,         PARAMETER     :: A4       = 3.031240396e-06
      REAL,         PARAMETER     :: A5       = 2.034080948e-08
      REAL,         PARAMETER     :: A6       = 6.136820929e-11

      REAL,         PARAMETER     :: ES0 = 6.1121

      REAL,         PARAMETER     :: C1       = 9.09718
      REAL,         PARAMETER     :: C2       = 3.56654
      REAL,         PARAMETER     :: C3       = 0.876793
      REAL,         PARAMETER     :: EIS      = 6.1071
      REAL                        :: RHS
      REAL,         PARAMETER     :: TF       = 273.16
      REAL                        :: TK

      REAL                        :: ES
      REAL                        :: QS
      REAL,         PARAMETER     :: EPS         = 0.622
      REAL,         PARAMETER     :: SVP1        = 0.6112
      REAL,         PARAMETER     :: SVP2        = 17.67
      REAL,         PARAMETER     :: SVP3        = 29.65
      REAL,         PARAMETER     :: SVPT0       = 273.15

      !  This function is designed to compute (q) from basic variables
      !  p (mb), t(K) and rh(0-100%) to give (q) in (kg/kg).  The reference
      !  temperature t_ref (c) is used to describe the temperature at which
      !  the liquid and ice phase change occurs.

      DO k = 1, kxs
         DO j = 1, jmxm
            DO i = 1, imxm
                  rh(i,j,k) = MIN ( MAX ( rh(i,j,k) ,  1. ) , 100. ) 
            END DO
         END DO
      END DO

      IF ( wrth2o ) THEN
         DO k = 1, kxs
            DO j = 1, jmxm
               DO i = 1, imxm
                  es=svp1*10.*EXP(svp2*(t(i,j,k)-svpt0)/(t(i,j,k)-svp3))
                  qs=eps*es/(sighup(k)*p(i,j)/100.+ptop/100.-es)
                  q(i,j,k)=MAX(.01*rh(i,j,k)*qs,0.0)
               END DO
            END DO
         END DO

      ELSE
         DO K = 1, KXS
            DO J = 1, JMXM
               DO I = 1, IMXM

                  t1 = t(i,j,k) - 273.16

                  !  Obviously dry.

                  IF ( t1 .lt. -200. ) THEN
                     q(i,j,k) = 0

                  ELSE

                     !  First compute the ambient vapor pressure of water

                     IF ( ( t1 .GE. t_ref ) .AND. ( t1 .GE. -47.) ) THEN    ! liq phase ESLO
                        ew = a0 + t1 * (a1 + t1 * (a2 + t1 * (a3 + t1 * (a4 + t1 * (a5 + t1 *  a6)))))

!         elseif (t1 .le. t_ref .and. t1 .ge. -47.) then !liq phas poor ES
                     ELSE IF ( ( t1 .GE. t_ref ) .AND. ( t1 .LT. -47. ) ) then !liq phas poor ES
                        ew = es0 * exp(17.67 * t1 / ( t1 + 243.5))

                     ELSE
                        tk = t(i,j,k)
                        rhs = -c1 * (tf / tk - 1.) - c2 * alog10(tf / tk) +  c3 * (1. - tk / tf) +      alog10(eis)
                        ew = 10. ** rhs

                     END IF

                     ew = MAX ( ew , 0. )

                     !  Now sat vap pres obtained compute local vapor pressure
  
                     ew = ew * rh(i,j,k) * 0.01

                     !  Now compute the specific humidity using the partial vapor
                     !  pressures of water vapor (ew) and dry air (p-ew).  The
                     !  constants assume that the pressure is in hPa, so we divide
                     !  the pressures by 100.

                     q1 = mw_vap * ew
                     q1 = q1 / (q1 + mw_air * (p(i,j)/100.*sighup(k)+ptop/100. - ew))

                     q(i,j,k) = q1 / (1. - q1 )

                  END IF

               END DO
            END DO
         END DO

      END IF

   END SUBROUTINE mxratsig

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!KWM REMOVED   SUBROUTINE sfcprs (t, q, height, pslv, ter, p, imx, jmx, kx, psfc)
!KWM START
   SUBROUTINE sfcprs (t, q, height, pslv, ter, avgsfct, p, imx, jmx, kx, psfc, psfc_method, less_than_24h)
!KWM END

      !  Computes the surface pressure using the input height,
      !  temperature and q (already computed from relative
      !  humidity) on p surfaces.  Sea level pressure is used
      !  to extrapolate a first guess.

      IMPLICIT NONE

      REAL,          PARAMETER    :: GAMMA     = 6.5E-3
      REAL,          PARAMETER    :: PCONST    = 10000.0
      REAL,          PARAMETER    :: gammarg   = gamma * r / g

      INTEGER                     :: I
      INTEGER                     :: IMX
      INTEGER                     :: IMXM
      INTEGER                     :: J
      INTEGER                     :: JMX
      INTEGER                     :: JMXM
      INTEGER                     :: K
      INTEGER                     :: K500
      INTEGER                     :: K700
      INTEGER                     :: K850
      INTEGER                     :: KX
      INTEGER                     :: KINTERP

      LOGICAL                     :: L1
      LOGICAL                     :: L2
      LOGICAL                     :: L3

      REAL                        :: GAMMA78     ( IMX , JMX )
      REAL                        :: GAMMA57     ( IMX , JMX )
      REAL                        :: HEIGHT      ( : , : , : )
      REAL                        :: HT          ( IMX , JMX )
      REAL                        :: P           ( : )
      REAL                        :: P1          ( IMX , JMX )
      REAL                        :: P57
      REAL                        :: P78
      REAL                        :: PSFC        ( : , : )
      REAL                        :: PSLV        ( : , : )
      REAL                        :: Q           ( : , : , : )
      REAL, PARAMETER             :: ROV2 = R / 2.
      REAL                        :: T           ( : , : , : )
      REAL                        :: T1          ( IMX , JMX )
      REAL                        :: T500        ( IMX , JMX )
      REAL                        :: T700        ( IMX , JMX )
      REAL                        :: T850        ( IMX , JMX )
      REAL                        :: TBAR
      REAL                        :: TC
      REAL                        :: TER         ( : , : )
      REAL                        :: TFIXED      ( IMX , JMX )
      REAL                        :: TSFC        ( IMX , JMX )
      REAL                        :: TSLV        ( IMX , JMX )
!KWM START
      INTEGER , INTENT ( IN )     :: PSFC_METHOD
      LOGICAL , INTENT ( IN )     :: LESS_THAN_24H
      REAL    , INTENT ( IN )     :: AVGSFCT     ( IMX , JMX )
!KWM END

      IMXM = IMX - 1                        ! CROSS POINT DIMENSIONS
      JMXM = JMX - 1
      TC   = 273.15 + 17.5

      !  Find the locations of the 850, 700 and 500 mb levels.

      K850 = 0                              ! FIND K AT: P=850
      K700 = 0                              !            P=700
      K500 = 0                              !            P=500

      DO K = 1, KX
         IF      (NINT(p(k)) .EQ. 85000) THEN
            K850 = K
         ELSE IF (NINT(p(k)) .EQ. 70000) THEN
            K700 = K
         ELSE IF (NINT(p(k)) .EQ. 50000) THEN
            K500 = K
         END IF
      END DO

      IF ( ( K850 .EQ. 0 ) .OR. ( K700 .EQ. 0 ) .OR. ( K500 .EQ. 0 ) ) THEN
         PRINT '(A)','Error in finding p level for 850, 700 or 500 hPa.'
         DO K = 1, KX
            PRINT '(A,I3,A,F10.2)','K = ',K,'  PRESSURE = ',P(K)
         END DO
         PRINT '(A)','Expected 850, 700, and 500 mb values, at least.'
         STOP 'not_enough_levels'
      END IF
    
      !  The 850 hPa level is called something special, and interpolated
      !  to cross points.  And then, we fill those last rows and columns.

      ht(:,:) = height(:,:,k850)
      CALL dot2crs (ht, imx, jmx)

      ht(:,jmx) = ht(:,jmxm)
      ht(imx,:) = ht(imxm,:)
      ter(:,jmx) = ter(:,jmxm)
      ter(imx,:) = ter(imxm,:)
      pslv(:,jmx) = pslv(:,jmxm)
      pslv(imx,:) = pslv(imxm,:)


      !  The variable ht is now -ter/ht(850 hPa).  The plot thickens.

      ht(:,:) = -ter(:,:) / ht(:,:)

      !  Make an isothermal assumption to get a first guess at the surface
      !  pressure.  This is to tell us which levels to use for the lapse
      !  rates in a bit.

      psfc = pslv * (pslv / 85000.) ** ht

      !  Get a pressure more than 100 hPa above the surface - P1.  The
      !  P1 is the top of the level that we will use for our lapse rate
      !  computations.

      DO j = 1, jmx
         DO i = 1, imx
            IF      ( ( PSFC(I,J) - 95000. ) .GE. 0. ) THEN
               P1(I,J) = 85000.
            ELSE IF ( ( PSFC(I,J) - 70000. ) .GE. 0. ) THEN
               P1(I,J) = PSFC(I,J) - 10000.
            ELSE
               P1(I,J) = 50000.
            END IF
         END DO
      END DO

      !  Compute virtual temperatures for k850, k700, and k500 layers.  Now
      !  you see why we wanted Q on pressure levels, it all is beginning   
      !  to make sense.

      t850(:,:) = t(:,:,k850) * (1. + 0.608 * q(:,:,k850))
      t700(:,:) = t(:,:,k700) * (1. + 0.608 * q(:,:,k700))
      t500(:,:) = t(:,:,k500) * (1. + 0.608 * q(:,:,k500))

      !  Compute two lapse rates between these three levels.  These are
      !  environmental values for each (i,j).

      gamma78 = ALOG(t850 / t700)  / ALOG (85000. / 70000.)
      gamma57 = ALOG(t700 / t500)  / ALOG (70000. / 50000.)

      DO j = 1, jmx
         DO i = 1, imx
            IF      ( ( psfc(i,j) - 95000. ) .GE. 0. ) THEN
               t1(i,j) = t850(i,j)
            ELSE IF ( ( psfc(i,j) - 85000. ) .GE. 0. ) THEN
               t1(i,j) = t700(i,j) * (p1(i,j) / 70000.) ** gamma78(i,j)
            ELSE IF ( ( psfc(i,j) - 70000. ) .GE. 0.) THEN 
               t1(i,j) = t500(i,j) * (p1(i,j) / 50000.) ** gamma57(i,j)
            ELSE
               t1(i,j) = t500(i,j)
            ENDIF
         END DO 
      END DO 

      !  From our temperature way up in the air, we extrapolate down to
      !  the sea level to get a guess at the sea level temperature.

      tslv = t1 * (pslv / p1) ** (gammarg)

      !  The new surface temperature is computed from the with new sea level 
      !  temperature, just using the elevation and a lapse rate.  This lapse 
      !  rate is -6.5 K/km.

      tsfc = tslv - gamma * ter

      !  A correction to the sea-level temperature, in case it is too warm.

      TFIXED = TC - 0.005 * (TSFC - TC) ** 2

      DO j = 1, jmx  
         DO i = 1, imx
            l1 = tslv(i,j) .LT. tc
            l2 = tsfc(i,j) .LE. tc
            l3 = .NOT. l1
            IF      ( l2 .AND. l3 ) THEN
               tslv(i,j) = tc
            ELSE IF ( ( .NOT. l2 ) .AND. l3 ) THEN
               tslv(i,j) = tfixed(i,j)
            END IF
         END DO
      END DO

      !  Finally, we can get to the surface pressure.

!KWM REMOVED      p1 = -ter * g / ( rov2 * ( tsfc + tslv ) )
!KWM REMOVED      psfc = pslv * EXP(p1)
!KWM START
      IF ( ( psfc_method .eq. 1 ) .AND. ( .NOT. less_than_24h ) ) THEN
         ! Throw in a quick replacement of tsfc
         psfc = pslv * ( 1.0 + gamma * ter / avgsfct ) ** ( - g / ( r * gamma ) )
      ELSE
         p1 = - ter * g / ( rov2 * ( tsfc + tslv ) )
         psfc = pslv * EXP ( p1 )
      END IF
!KWM END

      !  Surface pressure and sea-level pressure are the same at sea level.

      WHERE ( ABS ( ter )  .LT. 0.1 ) psfc = pslv

   END SUBROUTINE sfcprs

END MODULE diags
