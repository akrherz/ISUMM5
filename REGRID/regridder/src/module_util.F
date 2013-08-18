MODULE util

   USE constants

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE balance (f,xlat,psi,ix,jx,ds,out)

   !  Calculates the forcing terms in balance equation

   IMPLICIT NONE

      !  f       coriolis force
      !  xlat    latitude
      !  psi     stream function
      !  ix, jx  grid points in east west, north south direction, respectively
      !  ds      grid distance
      !  out     output array
  
      INTEGER :: ix , jx
      REAL , DIMENSION(ix,jx) :: f,xlat,psi,out
      REAL :: ds

      REAL :: psixx , psiyy , psiy , psixy 
      REAL :: dssq , ds2 , dssq4

      INTEGER :: i , j

      dssq  = ds * ds
      ds2   = ds * 2.
      dssq4 = ds * ds * 4.

      DO i=2,ix-2
         DO j=2,jx-2
            psixx = ( psi(i,j+1) + psi(i,j-1) - 2.*psi(i,j) ) / dssq
            psiyy = ( psi(i+1,j) + psi(i-1,j) - 2.*psi(i,j) ) / dssq
            psiy  = ( psi(i+1,j) - psi(i-1,j) ) / ds2
            psixy = ( psi(i+1,j+1)+psi(i-1,j-1)-psi(i-1,j+1)-psi(i+1,j-1)) / dssq4
            out(i,j)=0.25*(f(i,j)+f(i+1,j)+f(i+1,j+1)+f(i,j+1))*(psixx+psiyy)    &
                   +psiy*(f(i+1,j+1)+f(i+1,j)-f(i,j)-f(i,j+1))/ ds2              &
                   -2.*(psixy*psixy-psixx*psiyy)
         END DO
      END DO

      CALL fill(out,ix,jx,ix,jx,2,ix-2,2,jx-2)

   END SUBROUTINE balance

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE crs2dot(field,dim1,dim2)
   
      IMPLICIT NONE

      INTEGER :: dim1 , dim2
      REAL , DIMENSION(dim1,dim2) :: field,dummy
      INTEGER :: i , j 
      
      dummy(2:dim1-1,2:dim2-1)           = ( field(1:dim1-2,1:dim2-2) + &
                                             field(1:dim1-2,2:dim2-1) + &
                                             field(2:dim1-1,1:dim2-2) + &
                                             field(2:dim1-1,2:dim2-1) ) * 0.25
   
      dummy(2:dim1-1,1:dim2:dim2-1)      = ( field(1:dim1-2,1:dim2-1:dim2-2) + &
                                             field(2:dim1-1,1:dim2-1:dim2-2) ) * 0.5
   
      dummy(1:dim1:dim1-1,2:dim2-1)      = ( field(1:dim1-1:dim1-2,1:dim2-2) + &
                                             field(1:dim1-1:dim1-2,2:dim2-1) ) * 0.5
   
      dummy(1:dim1:dim1-1,1:dim2:dim2-1) =   field(1:dim1-1:dim1-2,1:dim2-1:dim2-2)
   
      field                              =   dummy
   
   END SUBROUTINE crs2dot

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE dot2crs(field,dim1,dim2)

      IMPLICIT NONE

      INTEGER :: dim1 , dim2
      REAL , DIMENSION(dim1,dim2) :: field

      INTEGER :: i , j 

      DO j = 1 , dim2 - 1
         DO i = 1 , dim1 - 1
            field(i,j) = ( field(i  ,j  ) + & 
                           field(i+1,j  ) + & 
                           field(i  ,j+1) + & 
                           field(i+1,j+1) ) * 0.25
         END DO
      END DO

   END SUBROUTINE dot2crs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE expand(slab,istart,jstart,itot,jtot)

   !  Fill the nearest data to the empty rows or columns of a slab

      IMPLICIT NONE

      INTEGER :: istart, jstart, itot, jtot
      REAL , DIMENSION(itot,jtot) :: slab

      INTEGER :: i1, j1, i, j

      DO j1=jstart,jtot-1
         DO i=1,istart
            slab(i,j1+1)=slab(i,j1)
         END DO
      END DO

      DO i1=istart,itot-1
         DO j=1,jtot
            slab(i1+1,j)=slab(i1,j)
         END DO
      END DO

   END SUBROUTINE expand

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE fill (f, ix, jx, imx, jmx, ifirst, ilast, jfirst, jlast)

      IMPLICIT NONE

      INTEGER                     :: I
      INTEGER                     :: IFIRST
      INTEGER                     :: ILAST
      INTEGER                     :: IMX
      INTEGER                     :: IX
      INTEGER                     :: J
      INTEGER                     :: JFIRST
      INTEGER                     :: JLAST
      INTEGER                     :: JMX
      INTEGER                     :: JX

      REAL                        :: F           ( : , : )

      DO j = jfirst, jlast
         DO i = 1, ifirst - 1
            f(i,j) = f(ifirst,j)
         END DO
         DO i = ilast + 1, imx
            f(i,j) = f(ilast,j)
         END DO
      END DO

      DO j = 1, jfirst - 1
         f(:,j) = f(:,jfirst)
      END DO
      DO j = jlast + 1, jmx
         f(:,j) = f(:,jlast)
      END DO

   END SUBROUTINE fill

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE fillit (f, ix, jx, kx, imx, jmx, ifirst, ilast, jfirst, jlast)

      IMPLICIT NONE

      INTEGER                     :: i
      INTEGER                     :: ifirst
      INTEGER                     :: ilast
      INTEGER                     :: imx
      INTEGER                     :: ix
      INTEGER                     :: j
      INTEGER                     :: jfirst
      INTEGER                     :: jlast
      INTEGER                     :: jmx
      INTEGER                     :: jx
      INTEGER                     :: k
      INTEGER                     :: kx

      REAL                        :: f           ( : , : , :)

      DO k = 1 , kx
         DO j = jfirst, jlast
            DO i = 1, ifirst - 1
               f(i,j,k) = f(ifirst,j,k)
            END DO
            DO i = ilast + 1, imx
               f(i,j,k) = f(ilast,j,k)
            END DO
         END DO
   
         DO j = 1, jfirst - 1
            f(:,j,k) = f(:,jfirst,k)
         END DO
         DO j = jlast + 1, jmx
            f(:,j,k) = f(:,jlast,k)
         END DO
      END DO

   END SUBROUTINE fillit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE make_surface ( field , psfc , p , ix , jx , kx , icrsdot , field_sfc )

      IMPLICIT NONE

      !  Input variables.

      INTEGER , INTENT(IN) :: ix , jx , kx , icrsdot
      REAL , INTENT(IN) , DIMENSION(:,:,:) :: field
      REAL , INTENT(IN) , DIMENSION(:,:)   :: psfc
      REAL , INTENT(IN) , DIMENSION(:)     :: p

      !  Output variable.

      REAL , INTENT(OUT) , DIMENSION(:,:)   :: field_sfc

      !  Stack variables.

      INTEGER :: i , j , k
      REAL , DIMENSION(ix,jx)   :: psfcd

      !  Dot point option first.

      IF ( icrsdot .EQ. 0 ) THEN
         psfcd = psfc
         CALL crs2dot ( psfcd , ix , jx ) 
         DO i = 1 , ix
            DO j = 1 , jx
               IF ( psfcd(i,j) .GT. p(2) ) THEN
                  field_sfc(i,j) = field(i,j,2)
               ELSE
                  inner_dot : DO k = 2 , kx - 1
                     IF ( (psfcd(i,j)-p(k)) * (psfcd(i,j)-p(k+1)) .LE. 0. ) THEN
                        field_sfc(i,j) = ( field(i,j,k  )*(p(k+1)    -psfcd(i,j)) +   &
                                           field(i,j,k+1)*(psfcd(i,j)-p(k)      ) ) / &
                                                          (p(k+1)    -p(k)      )
                        EXIT inner_dot
                     END IF
                  END DO inner_dot
               END IF
            END DO
         END DO

      !  Or else cross point data.

      ELSE IF ( icrsdot .EQ. 1 ) THEN
         DO i = 1 , ix - 1
            DO j = 1 , jx - 1
               IF ( psfc(i,j) .GT. p(2) ) THEN
                  field_sfc(i,j) = field(i,j,2)
               ELSE
                  inner_cross : DO k = 2 , kx - 1
                     IF ( (psfc(i,j)-p(k)) * (psfc(i,j)-p(k+1)) .LE. 0. ) THEN
                        field_sfc(i,j) = ( field(i,j,k  )*(p(k+1)   -psfc(i,j)) +   &
                                           field(i,j,k+1)*(psfc(i,j)-p(k)     ) ) / &
                                                          (p(k+1)   -p(k)     )
                        EXIT inner_cross
                     END IF
                  END DO inner_cross
               END IF
            END DO
         END DO
      END IF

   END SUBROUTINE make_surface

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE rankine(dx,dy,ds,nlvl,vwgt,rmax,vmax,uu,vv,psi,vor)

   !  Define analytical bogus vortex

      IMPLICIT NONE

      INTEGER nlvl
      REAL , DIMENSION(nlvl) :: uu, vv, psi, vor
      REAL , DIMENSION(nlvl) :: vwgt
      REAL :: dx,dy,ds,rmax,vmax
 
      REAL , PARAMETER :: alpha1= 1.
      REAL , PARAMETER :: alpha2= -0.75

      INTEGER :: k
      REAL :: vr , ang , rr , term1 , bb , term2 , alpha

      !  Wind component

      DO k=1,nlvl
         rr = SQRT(dx**2+dy**2)*ds
         IF ( rr .LT. rmax ) THEN
            alpha = 1.
         ELSE IF ( rr .GE. rmax ) THEN
            alpha = alpha2
         END IF
         vr = vmax * (rr/rmax)**(alpha)
         IF ( dx.GE.0. ) THEN
            ang = (pi/2.) - ATAN2(dy,MAX(dx,1.e-6))
            uu(k) = vwgt(k)*(-vr*COS(ang))
            vv(k) = vwgt(k)*( vr*SIN(ang))
         ELSE IF ( dx.LT.0. ) THEN
            ang = ((3.*pi)/2.) + ATAN2(dy,dx)
            uu(k) = vwgt(k)*(-vr*COS(ang))
            vv(k) = vwgt(k)*(-vr*SIN(ang))
         END IF
      END DO

      !  psi

      DO k=1,nlvl
         rr = SQRT(dx**2+dy**2)*ds
         IF ( rr .LT. rmax ) THEN
            psi(k) = vwgt(k) * (vmax*rr*rr)/(2.*rmax)
         ELSE IF ( rr .GE. rmax ) THEN
            IF (alpha1.EQ.1.0 .AND. alpha2.eq.-1.0) THEN
               psi(k) = vwgt(k) * vmax*rmax*(0.5+LOG(rr/rmax))
            ELSE IF (alpha1.EQ.1.0 .AND. alpha2.NE.-1.0) THEN
               term1 = vmax/(rmax**alpha1)*(rmax**(alpha1+1)/(alpha1+1))
               bb    = (rr**(alpha2+1)/(alpha2+1))-(rmax**(alpha2+1))/(alpha2+1)
               term2 = vmax/(rmax**alpha2)*bb
               psi(k) = vwgt(k) * (term1 + term2)
            END IF
         END IF
      END DO

      ! vort

      DO k=1,nlvl
         rr = SQRT(dx**2+dy**2)*ds
         IF ( rr .LT. rmax ) THEN
            vor(k) = vwgt(k) * (2.*vmax)/rmax
         ELSE IF ( rr .GE. rmax ) THEN
            vor(k) = vwgt(k) * ( (vmax/rmax**alpha2)*(rr**(alpha2-1.))*(1.+alpha2) )
         END IF
      END DO

   END SUBROUTINE rankine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE relax (chi, ff, rd, imx, jmx, ds, smallres, alpha)

      IMPLICIT NONE

      INTEGER, PARAMETER    :: mm = 20000

      INTEGER               :: i
      INTEGER               :: ie
      INTEGER               :: imx
      INTEGER               :: iter
      INTEGER               :: j
      INTEGER               :: je
      INTEGER               :: jm
      INTEGER               :: jmx
      INTEGER               :: mi

      REAL                  :: alpha
      REAL                  :: alphaov4
      REAL                  :: chi         ( : , : )
      REAL                  :: chimx       ( jmx ) 
      REAL                  :: ds
      REAL                  :: epx
      REAL                  :: fac
      REAL                  :: ff          ( : , : )
      REAL                  :: rd          ( : , : )
      REAL                  :: rdmax       ( jmx )
      REAL                  :: smallres

      LOGICAL               :: converged = .FALSE.

      fac = ds * ds
      alphaov4 = alpha * 0.25

      ie=imx-2
      je=jmx-2

      DO j = 1, jmx
         DO i = 1, imx
            ff(i,j) = fac * ff(i,j)
            rd(i,j) = 0.0
         END DO
      END DO

      iter_loop : DO iter = 1, mm
         mi = iter
         chimx = 0.0


         DO j = 2, je
            DO i = 2, ie
               chimx(j) = MAX(ABS(chi(i,j)),chimx(j))
            END DO
         END DO

         epx = MAXVAL(chimx) * SMALLRES * 4.0 / alpha

         DO j = 2, je
            DO i = 2, ie
               rd(i,j) = chi(i,j+1) + chi(i,j-1) + chi(i+1,j) + chi(i-1,j) - 4.0 * chi(i,j) - ff(i,j)
               chi(i,j) = chi(i,j) + rd(i,j) * alphaov4
            END DO
         END DO

         rdmax = 0.0

         DO j = 2, je
            DO i = 2, ie
               rdmax(j) = MAX(ABS(rd(i,j)),rdmax(j))
            END DO
         END DO

         IF (MAXVAL(rdmax) .lt. epx) THEN
            converged = .TRUE.
            EXIT iter_loop
         END IF

      END DO iter_loop

      IF (converged ) THEN
!        PRINT '(A,I5,A)','Relaxation converged in ',mi,' iterations.'
      ELSE
         PRINT '(A,I5,A)','Relaxation did not converge in',mm,' iterations.'
         STOP 'no_converge'
      END IF

   END SUBROUTINE relax

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE rh_cleanup ( rh , iew , jns , rh_min , rh_max )

      IMPLICIT NONE
   
      INTEGER                        :: iew , jns
      REAL , DIMENSION ( iew , jns ) :: rh
      REAL                           :: rh_min , rh_max
   
      WHERE ( rh .GT. rh_max ) rh = rh_max
      WHERE ( rh .LT. rh_min ) rh = rh_min
   
   END SUBROUTINE rh_cleanup

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE smoother_desmoother ( slab , imx , jmx , passes , crsdot )
   
      IMPLICIT NONE
   
      INTEGER                        :: imx , jmx , passes , crsdot
      REAL , DIMENSION ( imx , jmx ) :: slab , & 
                                        slabnew
   
      REAL , DIMENSION ( 2 )         :: xnu
      INTEGER                        :: i , j , loop , n 
   
      xnu  =  (/ 0.50 , -0.52 /)
   
      !  The odd number passes of this are the "smoother", the even
      !  number passes are the "de-smoother" (note the differnt signs on xnu).
   
      smoothing_passes : DO loop = 1 , passes * 2
   
         n  =  2 - MOD ( loop , 2 )
    
         DO i = 2 , imx - 1 - crsdot
            DO j = 2 , jmx - 1 - crsdot
               slabnew(i,j) = slab(i,j) + xnu(n) *  & 
               ((slab(i,j+1) + slab(i,j-1)) * 0.5-slab(i,j))
            END DO
         END DO
    
         DO i = 2 , imx - 1 - crsdot
            DO j = 2 , jmx - 1 - crsdot
               slab(i,j) = slabnew(i,j)
            END DO
         END DO
    
         DO j = 2 , jmx - 1 - crsdot
            DO i = 2 , imx - 1 - crsdot
               slabnew(i,j) = slab(i,j) + xnu(n) *  &
               ((slab(i+1,j) + slab(i-1,j)) * 0.5-slab(i,j))
            END DO
         END DO
    
         DO i = 2 , imx - 1 - crsdot
            DO j = 2 , jmx - 1 - crsdot
               slab(i,j) = slabnew(i,j)
            END DO
         END DO
    
      END DO smoothing_passes
   
   END SUBROUTINE smoother_desmoother

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE smooth_5 ( field , iew , jns , passes , crsdot )

      IMPLICIT NONE
   
      INTEGER                        :: iew , jns , &
                                        passes    , &
                                        crsdot
      REAL , DIMENSION ( iew , jns ) :: field
   
      REAL , DIMENSION ( iew , jns ) :: temp
      INTEGER                        :: i , j , num_passes
   
      !  How may passes of this smoother are we using.
   
      smoothing_passes : DO num_passes = 1 , passes
   
         !  Apply 5-point stencil smoother on interior of the domain.
      
         DO j = 2 , jns - 1 - crsdot
            DO i = 2 , iew - 1 - crsdot
               temp(i,j) = ( field(i  ,j  ) * 4. +  & 
                             field(i+1,j  )      +  & 
                             field(i-1,j  )      +  & 
                             field(i  ,j+1)      +  & 
                             field(i  ,j-1)      )  * 0.125
            END DO
         END DO
   
         !  Apply 3-point stencil smoother on the boundaries.
      
         i = 1
         DO j = 2 , jns - 1 - crsdot
            temp(i,j) = ( field(i  ,j  ) * 2. +  & 
                          field(i  ,j+1)      +  & 
                          field(i  ,j-1)      )  * 0.25
         END DO
   
         i = iew - crsdot
         DO j = 2 , jns - 1 - crsdot
            temp(i,j) = ( field(i  ,j  ) * 2. +  & 
                          field(i  ,j+1)      +  & 
                          field(i  ,j-1)      )  * 0.25
         END DO
      
         j = 1
         DO i = 2 , iew - 1 - crsdot
            temp(i,j) = ( field(i  ,j  ) * 2. +  & 
                          field(i+1,j  )      +  & 
                          field(i-1,j  )      ) * 0.25
         END DO
      
         j = jns - crsdot
         DO i = 2 , iew - 1 - crsdot
            temp(i,j) = ( field(i  ,j  ) * 2. +  & 
                          field(i+1,j  )      +  & 
                          field(i-1,j  )      ) * 0.25
         END DO
      
         !  Store smoothed field back into original array.
      
         DO j = 2 , jns - 1 - crsdot
            DO i = 2 , iew - 1 - crsdot
               field(i,j) = temp(i,j)
            END DO
         END DO
      
         !  Store smoothed boundary field back into original array.
      
         DO j = 2 , jns - 1 - crsdot
            field(1         ,j) = temp(1         ,j)
            field(iew-crsdot,j) = temp(iew-crsdot,j)
         END DO
      
         DO i = 2 , iew - 1 - crsdot
            field(i,1         ) = temp(i,1         )
            field(i,jns-crsdot) = temp(i,jns-crsdot)
         END DO
   
      END DO smoothing_passes
   
   END SUBROUTINE smooth_5

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE util
