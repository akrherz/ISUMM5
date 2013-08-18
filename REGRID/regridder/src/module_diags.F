MODULE diags

   USE constants
   USE util
   USE namelist_info

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE diverg(u,v,dmf,xmf,i1,j1,k1,ds,div)

   !  Computes divergence
   !  div = m*m (du/dx + dv/dy)

      IMPLICIT NONE

      INTEGER :: i1, j1 , k1

      REAL , DIMENSION(i1,j1,k1) :: u, v, div
      REAL , DIMENSION(i1,j1   ) :: xmf, dmf
      REAL :: ds

      REAL :: ds2r , u1 , u2 , u3 , u4 , v1 , v2 , v3 , v4
      INTEGER :: i , j , k

      ds2r = 1./(2.*ds)

      DO k = 1, k1
         DO j = 1, j1-1
            DO i = 1, i1-1
               u1=u(i  ,j  ,k)/dmf(i  ,j  )
               u2=u(i+1,j  ,k)/dmf(i+1,j  )
               u3=u(i  ,j+1,k)/dmf(i  ,j+1)
               u4=u(i+1,j+1,k)/dmf(i+1,j+1)
               v1=v(i  ,j  ,k)/dmf(i  ,j  )
               v2=v(i+1,j  ,k)/dmf(i+1,j  )
               v2=v(i+1,j  ,k)/dmf(i+1,j  )
               v3=v(i  ,j+1,k)/dmf(i  ,j+1)
               v4=v(i+1,j+1,k)/dmf(i+1,j+1)
               div(i,j,k) = xmf(i,j)*xmf(i,j)*ds2r*((u3-u1+u4-u2)+(v2-v1+v4-v3))
            END DO
         END DO
      END DO

   END SUBROUTINE diverg

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE geowind(height,xmf,dmf,cor,imx,jmx,kx,ds,ug,vg)

   !  Computes the geostrophic wind components from the height gradient.
   !  There is no Coriolis parameter used - this is the tropics.

      IMPLICIT NONE

      !     input       height   geopotential               cross    3d
      !                 xmf      map factors                cross    2d
      !                 dmf      map factors                dot      2d
      !                 imx      dot point dimension n-s
      !                 jmx      dot point dimension e-w
      !                 kx       number of vertical levels
      !
      !     output      ug       u component of geo wind    cross    3d
      !                 vg       v component of geo wind    cross    3D

      INTEGER :: imx , jmx , kx
      REAL :: ds
      REAL , DIMENSION(imx,jmx,kx) :: height
      REAL , DIMENSION(imx,jmx   ) :: xmf , dmf , cor

      REAL , DIMENSION(imx,jmx,kx) :: ug , vg

      REAL :: ds2r , h1 , h2 , h3 , h4
      INTEGER :: i , j , k

      ds2r=1./(2.*ds)

      DO k=1,kx
         DO j=2,jmx-1
            DO i=2,imx-1
               h1=height(i-1,j-1,k)
               h2=height(i  ,j-1,k)
               h3=height(i-1,j  ,k)
               h4=height(i  ,j  ,k)
!              ug(i,j,k)=-1.*g*dmf(i,j)/cor(i,j)*ds2r*(h4-h3+h2-h1)
!              vg(i,j,k)=    g*dmf(i,j)/cor(i,j)*ds2r*(h4-h2+h3-h1)
               ug(i,j,k)=-1.*dmf(i,j)*ds2r*(h4-h3+h2-h1)
               vg(i,j,k)=    dmf(i,j)*ds2r*(h4-h2+h3-h1)
            END DO
         END DO
      END DO

      CALL fillit(ug,imx,jmx,kx,imx,jmx,2,imx-1,2,jmx-1)
      CALL fillit(vg,imx,jmx,kx,imx,jmx,2,imx-1,2,jmx-1)

   END SUBROUTINE geowind

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE missing_soil(xlat,tempg,elevation)

      !  Use latitude to define soil temperature for missing values.
      !  This routine and coefficients were obtained from Yong-Run Guo, Sep 1999.

      IMPLICIT NONE

      REAL :: xlat , tempg , elevation , angle 

      REAL , PARAMETER :: c0= 0.24206E+03 
      REAL , PARAMETER :: c1= 0.59736E+02 
      REAL , PARAMETER :: c2= 0.19445E+01

      angle = 0.5*3.1415926*((89.5-xlat)/89.5)
      tempg = c0 + c1*sin(angle) + c2*cos(angle) - 0.0065 * elevation

   END SUBROUTINE missing_soil

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE mxratprs (rh, t, ppa, ix, jx, kx, q)

      
      IMPLICIT NONE

      INTEGER                     :: i , ix , j , jx , k , kx 

      REAL                        :: ppa         ( : )
      REAL                        :: p           ( KX )
      REAL                        :: q           ( : , : , : )
      REAL                        :: rh          ( : , : , : )
      REAL                        :: t           ( : , : , : )

      REAL                        :: es
      REAL                        :: qs

      !  This function is designed to compute (q) from basic variables
      !  p (mb), t(K) and rh(0-100%) to give (q) in (kg/kg).

      p = ppa * 0.01

      DO k = 1, kx
         DO j = 1, jx - 1
            DO i = 1, ix - 1
                  rh(i,j,k) = MIN ( MAX ( rh(i,j,k) , rec2%min_RH_value ) , 100. ) 
            END DO
         END DO
      END DO

      rh(ix,:,:) = rh(ix-1,:,:)
      rh(:,jx,:) = rh(:,jx-1,:)
      t (ix,:,:) = t (ix-1,:,:)
      t (:,jx,:) = t (:,jx-1,:)

      DO k = 1, kx
         DO j = 1, jx
            DO i = 1, ix
               es = svp1 * 10. * EXP(svp2 * (t(i,j,k) - svpt0) / (t(i,j,k) - svp3))
               qs = eps * es / (p(k) - es)
               q(i,j,k) = MAX(0.01 * rh(i,j,k) * qs,0.0)
            END DO
         END DO
      END DO

   END SUBROUTINE mxratprs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE qvtorh ( q , t , p , k00, imx , jmx , kxs , rh )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: imx , jmx , kxs , k00
      REAL , INTENT(IN) , DIMENSION(:,:,:) :: q , t
      REAL , INTENT(IN) , DIMENSION(:) :: p

      REAL , INTENT(OUT) , DIMENSION(:,:,:) :: rh

      !  Local variables.

      INTEGER :: i , j , k
      REAL :: es , qs

      DO k = k00 , kxs
         DO j = 1 , jmx - 1
            DO i = 1 , imx - 1
               es = svp1*10.*EXP(svp2*(t(i,j,k)-svpt0)/(t(i,j,k)-svp3))
               qs = eps*es/(0.01*p(k) - es)
               rh(i,j,k) = MIN ( 100. , MAX ( 100.*q(i,j,k)/qs ,  rec2%min_RH_value ) )
            END DO
         END DO
      END DO

   END SUBROUTINE qvtorh

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE sfcprs (t, q, height, pslv, ter, p, ix, jx, kx, psfc)

      !  Computes the surface pressure using the input height,
      !  temperature and q (already computed from relative
      !  humidity) on p surfaces.  Sea level pressure is used
      !  to extrapolate a first guess.

      IMPLICIT NONE

      REAL,          PARAMETER    :: gamma     = 6.5E-3
      REAL,          PARAMETER    :: pconst    = 10000.0
      REAL,          PARAMETER    :: gammarg   = gamma * r / g

      INTEGER                     :: I , ix , ixm , j , jx , jxm , k , k500 , k700 , k850 , kx , kinterp

      LOGICAL                     :: l1 , l2 , l3

      REAL                        :: gamma78     ( IX , JX )
      REAL                        :: gamma57     ( IX , JX )
      REAL                        :: height      ( : , : , : )
      REAL                        :: ht          ( IX , JX )
      REAL                        :: p           ( : )
      REAL                        :: p1          ( IX , JX )
      REAL                        :: p57
      REAL                        :: p78
      REAL                        :: psfc        ( : , : )
      REAL                        :: pslv        ( : , : )
      REAL                        :: q           ( : , : , : )
      REAL, PARAMETER             :: rov2 = r / 2.
      REAL                        :: t           ( : , : , : )
      REAL                        :: t1          ( IX , JX )
      REAL                        :: t500        ( IX , JX )
      REAL                        :: t700        ( IX , JX )
      REAL                        :: t850        ( IX , JX )
      REAL                        :: tbar
      REAL                        :: tc
      REAL                        :: ter         ( : , : )
      REAL                        :: tfixed      ( IX , JX )
      REAL                        :: tsfc        ( IX , JX )
      REAL                        :: tslv        ( IX , JX )

      ixm = ix - 1
      jxm = jx - 1
      tc   = 273.15 + 17.5

      !  Find the locations of the 850, 700 and 500 mb levels.

      k850 = 0                              ! FIND K AT: P=850
      k700 = 0                              !            P=700
      k500 = 0                              !            P=500

      DO k = 1, kx
         IF      (NINT(p(k)) .EQ. 85000) THEN
            k850 = k
         ELSE IF (NINT(p(k)) .EQ. 70000) THEN
            k700 = k
         ELSE IF (NINT(p(k)) .EQ. 50000) THEN
            k500 = k
         END IF
      END DO

      IF ( ( k850 .EQ. 0 ) .OR. ( k700 .EQ. 0 ) .OR. ( k500 .EQ. 0 ) ) THEN
         PRINT '(A)','Error in finding p level for 850, 700 or 500 hPa.'
         DO k = 1, kx
            PRINT '(A,I3,A,F10.2)','K = ',k,'  PRESSURE = ',p(k)
         END DO
         PRINT '(A)','Expected 850, 700, and 500 mb values, at least.'
         STOP 'not_enough_levels'
      END IF
    
      !  The 850 hPa level is called something special, and interpolated
      !  to cross points.  And then, we fill those last rows and columns.

      ht(:,:) = height(:,:,k850)

      ht(:,jx) = ht(:,jxm)
      ht(ix,:) = ht(ixm,:)
      ter(:,jx) = ter(:,jxm)
      ter(ix,:) = ter(ixm,:)
      pslv(:,jx) = pslv(:,jxm)
      pslv(ix,:) = pslv(ixm,:)

      !  The variable ht is now -ter/ht(850 hPa).  The plot thickens.

      ht(:,:) = -ter(:,:) / ht(:,:)

      !  Make an isothermal assumption to get a first guess at the surface
      !  pressure.  This is to tell us which levels to use for the lapse
      !  rates in a bit.

      psfc = pslv * (pslv / 85000.) ** ht

      !  Get a pressure more than 100 hPa above the surface - P1.  The
      !  P1 is the top of the level that we will use for our lapse rate
      !  computations.

      DO j = 1, jx
         DO i = 1, ix
            IF      ( ( psfc(i,j) - 95000. ) .GE. 0. ) THEN
               p1(I,J) = 85000.
            ELSE IF ( ( psfc(i,j) - 70000. ) .GE. 0. ) THEN
               p1(i,j) = psfc(i,j) - 10000.
            ELSE
               p1(i,j) = 50000.
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

      gamma78 = LOG(t850 / t700)  / LOG (85000. / 70000.)
      gamma57 = LOG(t700 / t500)  / LOG (70000. / 50000.)

      DO j = 1, jx
         DO i = 1, ix
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

      DO j = 1, jx  
         DO i = 1, ix
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

      p1 = -ter * g / ( rov2 * ( tsfc + tslv ) )
      psfc = pslv * EXP(p1)

      !  Surface pressure and sea-level pressure are the same at sea level.

      WHERE ( ABS ( ter )  .LT. 0.1 ) psfc = pslv

   END SUBROUTINE sfcprs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE vor(u,v,dmf,xmf,i1,j1,k1,ds,vort)

      !  Compute k component of del cross velocity
      !  vort = m*m (dv/dx - du/dy), where u and v are coupled
      !  with map factors (dot point) and m is the map factors
      !  on cross points

      IMPLICIT NONE

      INTEGER :: i1 , j1 , k1

      REAL , DIMENSION(i1,j1,k1) :: u, v, vort
      REAL , DIMENSION(i1,j1   ) :: xmf, dmf

      REAL :: ds

      REAL :: ds2r , u1 , u2 , u3 , u4 , v1 , v2 , v3 , v4
      INTEGER :: i , j , k

      ds2r=1./(2.*ds)

      DO k=1,k1
         DO j=1,j1-1
            DO i=1,i1-1
               u1=u(i  ,j  ,k)/dmf(i  ,j  )
               u2=u(i+1,j  ,k)/dmf(i+1,j  )
               u3=u(i  ,j+1,k)/dmf(i  ,j+1)
               u4=u(i+1,j+1,k)/dmf(i+1,j+1)
               v1=v(i  ,j  ,k)/dmf(i  ,j  )
               v2=v(i+1,j  ,k)/dmf(i+1,j  )
               v3=v(i  ,j+1,k)/dmf(i  ,j+1)
               v4=v(i+1,j+1,k)/dmf(i+1,j+1)
               vort(i,j,k)=xmf(i,j)*xmf(i,j)*ds2r*((v4-v2+v3-v1)-(u2-u1+u4-u3))
            END DO
         END DO
      END DO

      CALL fillit(vort,i1,j1,k1,i1,j1,1,i1-1,1,j1-1)

   END SUBROUTINE vor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE diags
