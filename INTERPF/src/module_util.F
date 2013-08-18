MODULE util

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
   SUBROUTINE check (kxs, newcoord)
     
   !  Check that the number of sigma layers is correct
   !  and in descending order, given the namelist values.
     
      IMPLICIT NONE

      !  Input variables.
    
      INTEGER                      :: KXS
    
      REAL                         :: NEWCOORD   ( : )
    
      !  Local variables.
    
      INTEGER                      :: K
      INTEGER                      :: KILL_IT = 0
    
      !  Check first sigma value newcoord(1) should be 1.0
     
      IF ((newcoord(1) .LT. 0.999999) .OR. (newcoord(1) .GT. 1.000001)) THEN
        PRINT '(A,F6.4)','Full sigma value at model bottom is ', newcoord(1)
        kill_it = kill_it + 1
      ENDIF
     
      !  Check last sigma value, newcoord(kxs+1) should be 0.0
     
      IF ((newcoord(kxs+1) .LT. -0.000001) .OR. (newcoord(kxs+1) .GT.  0.000001)) THEN
        PRINT '(A,F6.4)','Full sigma value at model top is ', newcoord(kxs+1)
        kill_it = kill_it + 1
      ENDIF
    
      !  Descending input sigma value order check.
    
      DO k = 1, kxs
         IF (newcoord(k+1) .GE. newcoord(k)) THEN
            PRINT '(A)','Sigma values are not in descending order.'
            kill_it = kill_it + 1
         ENDIF
      END DO
     
      !  If any test was bad, stop.
     
      IF ( kill_it .GT. 0 ) THEN 
         PRINT '(A)','Troubles with sigma data.  Fix the namelist.input file.'
         STOP 'sigma_woes'
      END IF
    
   END SUBROUTINE CHECK

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

   SUBROUTINE clean_rh ( rh , iew , jns , kx , rh_min , rh_max )

      IMPLICIT NONE
   
      INTEGER                             :: iew , jns , kx
      REAL , DIMENSION ( iew , jns , kx ) :: rh
      REAL                                :: rh_min , rh_max
   
      rh(iew,:,:) = rh(iew-1,:,:)
      rh(:,jns,:) = rh(:,jns-1,:)

      WHERE ( rh .GT. rh_max ) rh = rh_max
      WHERE ( rh .LT. rh_min ) rh = rh_min
   
   END SUBROUTINE clean_rh

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE invert2 (dum3d, imx, jmx, kx)

      IMPLICIT NONE

      INTEGER                     :: I
      INTEGER                     :: IMX
      INTEGER                     :: J
      INTEGER                     :: JMX
      INTEGER                     :: K
      INTEGER                     :: KK
      INTEGER                     :: KX

      REAL                        :: DUM3D       ( : , : , : )
      REAL                        :: OUT

      DO K = 1, KX/2
         kk = kx - k + 1
         DO j = 1 , jmx
            DO i = 1 , imx
               out = dum3d(i,j,k)
               dum3d(i,j,k) = dum3d(i,j,kk)
               dum3d(i,j,kk) = out
            END DO
         END DO
      END DO

   END SUBROUTINE invert2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

   SUBROUTINE meandiv (u, v, psd, dm, sigh, imx, jmx, kxs, ds, imxm, jmxm)

      IMPLICIT NONE

      INTEGER                     :: I
      INTEGER                     :: IMX
      INTEGER                     :: IMXM
      INTEGER                     :: J
      INTEGER                     :: JMX
      INTEGER                     :: JMXM
      INTEGER                     :: K
      INTEGER                     :: KXS

      REAL                        :: CHI         ( IMX , JMX )
      REAL                        :: DIV         ( IMX , JMX )
      REAL                        :: DM          ( : , : )
      REAL                        :: DS
      REAL                        :: DSG         ( KXS )
      REAL                        :: DUDX        ( IMX , JMX )
      REAL                        :: DVDY        ( IMX , JMX )
      REAL                        :: F           ( IMX , JMX )
      REAL                        :: ONEOV2DS
      REAL                        :: PSD         ( : , : )
      REAL                        :: SIGH        ( : )
      REAL                        :: SIGF        ( KXS + 1 )
      REAL                        :: U           ( : , : , : )
      REAL                        :: UDIVERG     ( IMX , JMX )
      REAL                        :: USLB        ( IMX , JMX )
      REAL                        :: V           ( : , : , : )
      REAL                        :: VDIVERG     ( IMX , JMX )
      REAL                        :: VSLB        ( IMX , JMX )
      REAL                        :: WEIGHT      ( KXS )

      ONEOV2DS = 1. / (2. * DS)

      !  Integrate p* v/m, compute div, to dot point, to (x,y) format

      sigf(1) = 0.0
      DO k = 1, kxs
         sigf(k+1) = 2. * sigh(k) - sigf(k)
         dsg(k) = sigf(k+1) - sigf(k)
      END DO

!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!csd$ parallel do private(i,j)
      DO j = 1 , jmx
         DO i = 1 , imx
            uslb(i,j) = 0.0
            vslb(i,j) = 0.0
         END DO
      END DO
!csd$ end parallel do

!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j , k )
!csd$ parallel do private(i,j,k)
      DO j = 1 , jmx
         DO i = 1 , imx
            DO k = 1, kxs
               uslb(i,j) = uslb(i,j) + u(i,j,k) * dsg(k)
               vslb(i,j) = vslb(i,j) + v(i,j,k) * dsg(k)
            END DO
         END DO
      END DO
!csd$ end parallel do

!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!csd$ parallel do private(i,j)
      DO j = 1 , jmx
         DO i = 1 , imx
            uslb(i,j) = uslb(i,j) * psd(i,j) / dm(i,j)
            vslb(i,j) = vslb(i,j) * psd(i,j) / dm(i,j)
         END DO
      END DO
!csd$ end parallel do

!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!csd$ parallel do private(i,j)
      DO j = 1 , jmxm
         DO i = 1 , imxm
            dudx(i,j) = uslb(i+1,j+1)-uslb(i+1,j)+uslb(i,j+1)-uslb(i,j)
            dvdy(i,j) = vslb(i+1,j+1)-vslb(i,j+1)+vslb(i+1,j)-vslb(i,j)
         END DO
      END DO
!csd$ end parallel do

!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!csd$ parallel do private(i,j)
      DO j = 1, jmxm
         DO i = 1, imxm
            div(i,j) = oneov2ds * (dudx(i,j) + dvdy(i,j))
         END DO
      END DO
!csd$ end parallel do

      !  Iteratively solve laplacian from good first guess.  
   
!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!csd$ parallel do private(i,j)
      DO j = 1 , jmx
         DO i = 1 , imx
            chi(i,j) = 0.0
         END DO
      END DO
!csd$ end parallel do
      CALL relax (chi, div, f, imx, jmx, imx-2, jmx-2, ds)

      !  Get divergent component of wind, 2d field on dot points.

!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!csd$ parallel do private(i,j)
      DO j = 2, jmxm
         do i = 2, imxm
            udiverg(i,j) = (chi(i  ,j  ) - chi(i  ,j-1) + chi(i-1,j  ) - chi(i-1,j-1)) * oneov2ds
         END DO
      END DO
!csd$ end parallel do

!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!csd$ parallel do private(i,j)
      DO j = 2, jmxm
         DO i = 2, imxm
            vdiverg(i,j) = (chi(i  ,j  ) - chi(i-1,j  ) + chi(i  ,j-1) - chi(i-1,j-1)) * oneov2ds
         END DO
      END DO
!csd$ end parallel do

      CALL fill (udiverg, imx, jmx, imx, jmx, 2, imxm, 2, jmxm)
      CALL fill (vdiverg, imx, jmx, imx, jmx, 2, imxm, 2, jmxm)

      !  Remove mean divergent component.

!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!csd$ parallel do private(i,j)
      DO j = 1, jmx 
         DO i = 1, imx 
            udiverg(i,j) = udiverg(i,j) * dm(i,j) / psd(i,j) 
            vdiverg(i,j) = vdiverg(i,j) * dm(i,j) / psd(i,j) 
         END DO
      END DO
!csd$ end parallel do

!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j , k , weight )
!csd$ parallel do private(i,j,k,weight)
      DO j = 1, jmx 
         DO i = 1, imx 
            DO k = 1, kxs
               weight(k) = 2. * (1. - sigh(k))
               u(i,j,k) = u(i,j,k) - weight(k) * udiverg(i,j)
               v(i,j,k) = v(i,j,k) - weight(k) * vdiverg(i,j)
            END DO
         END DO
      END DO
!csd$ end parallel do

   END SUBROUTINE meandiv

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE relax (chi, ff, rd, imx, jmx, ie, je, ds)

      IMPLICIT NONE

#ifdef XCRAY 
      REAL,          PARAMETER    :: SMALLRES = 1.0E-9
#else
      REAL,          PARAMETER    :: SMALLRES = 1.0E-6
#endif

      INTEGER,       PARAMETER    :: MM = 20000
      REAL,          PARAMETER    :: ALPHA = 1.8
      REAL,          PARAMETER    :: ALPHAOV4 = 1.8 / 4.0

      INTEGER                     :: I
      INTEGER                     :: IE
      INTEGER                     :: IMX
      INTEGER                     :: ITER
      INTEGER                     :: J
      INTEGER                     :: JE
      INTEGER                     :: JM
      INTEGER                     :: JMX
      INTEGER                     :: MI

      REAL                        :: CHI         ( : , : )
      REAL                        :: CHIMX       ( jmx ) 
      REAL                        :: DS
      REAL                        :: EPX
      REAL                        :: FAC
      REAL                        :: FF          ( : , : )
      REAL                        :: RD          ( : , : )
      REAL                        :: RDMAX       ( jmx )

      LOGICAL                     :: converged = .FALSE.

      fac = 2.0 * ds * ds
      rd = 0.0
!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!csd$ parallel do private(i,j)
      DO j = 1, je + 1
         DO i = 1, ie + 1
            ff(i,j) = fac * ff(i,j)
         END DO
      END DO
!csd$ end parallel do

      iter_loop : DO iter = 1, mm
         mi = iter
         chimx = 0.0


!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!csd$ parallel do private(i,j)
         DO j = 2, je
            DO i = 2, ie
               chimx(j) = MAX(ABS(chi(i,j)),chimx(j))
            END DO
         END DO
!csd$ end parallel do

         epx = MAXVAL(chimx) * SMALLRES * 4.0 / alpha

!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!csd$ parallel do private(i,j)
         DO j = 2, je
            DO i = 2, ie
               rd(i,j) = chi(i+1,j+1) + chi(i-1,j+1) + chi(i+1,j-1) + chi(i-1,j-1) - 4.0 * chi(i,j) - ff(i,j)
               chi(i,j) = chi(i,j) + rd(i,j) * alphaov4
            END DO
         END DO
!csd$ end parallel do

         rdmax = 0.0

!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!csd$ parallel do private(i,j)
         DO j = 2, je
            DO i = 2, ie
               rdmax(j) = MAX(ABS(rd(i,j)),rdmax(j))
            END DO
         END DO
!csd$ end parallel do

         IF (MAXVAL(rdmax) .lt. epx) THEN
            converged = .TRUE.
            EXIT iter_loop
         END IF

      END DO iter_loop

      IF (converged ) THEN
         PRINT '(A,I5,A)','Relaxation converged in ',mi,' iterations.'
      ELSE
         PRINT '(A,I5,A)','Relaxation did not converge in',mm,' iterations.'
         STOP 'no_converge'
      END IF

   END SUBROUTINE relax

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE util
