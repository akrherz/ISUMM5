MODULE nh_interp

   USE phys_consts

CONTAINS

   SUBROUTINE nhbase (ter, imx, jmx, kx, ps0, pr0, t0, rho0, a, ptop, pt, p0, tlp, ts0, tiso)

   IMPLICIT NONE

      !  Compute the nonhydrostatic base state.
 

      INTEGER                     :: I
      INTEGER                     :: IMX
      INTEGER                     :: J
      INTEGER                     :: JMX
      INTEGER                     :: K
      INTEGER                     :: KX

      REAL                        :: A           ( : )
      REAL                        :: AC
      REAL                        :: ALNP
      REAL                        :: B
      REAL                        :: piso
      REAL                        :: P0
      REAL                        :: PRES0
      REAL                        :: PR0         ( : , : , : )
      REAL                        :: PS0         ( : , : )
      REAL                        :: PT
      REAL                        :: PTOP
      REAL                        :: RHO0        ( : , : , : )
      REAL                        :: TER         ( : , : )
      REAL                        :: TLP
      REAL                        :: T0          ( : , : , : )
      REAL                        :: tiso
      REAL                        :: TS0
      REAL                        :: ziso
 
      pt = ptop

      !  Define ps0 from terrain heights and t0 profile.

!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j , k , ac , b , alnp , pres0 )
!csd$ parallel do private(i,j,k,ac,b,alnp,pres0)
      DO j = 1, jmx - 1
         DO i = 1, imx - 1

            !  Height is multiplied by g.

            ac = g * ter(i,j) / (2. * tlp * r)
            b = ts0 / tlp
            alnp = -b + SQRT(b * b - 4 * ac)
            ps0(i,j) = p0 * EXP(alnp) - pt

            !  Define reference state temperature at model points.

            DO k = 1, kx
               pr0(i,j,k) = ps0(i,j) * a(k) + pt
               pres0 = pr0(i,j,k)
               t0(i,j,k) = MAX ( ts0 + tlp * ALOG(pres0 / p0) , tiso ) 
            END DO
         END DO
      END DO
!csd$ end parallel do

      !  Fill in the boundaries.

      ps0(:,jmx) = ps0(:,jmx-1)
      ps0(imx,:) = ps0(imx-1,:)
      pr0(:,jmx,:) = pr0(:,jmx-1,:)
      pr0(imx,:,:) = pr0(imx-1,:,:)
      t0(:,jmx,:) = t0(:,jmx-1,:)
      t0(imx,:,:) = t0(imx-1,:,:)

      rho0 = pr0 / r / t0

   END SUBROUTINE nhbase


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE nhinterp (f, tv, ter, ps, ps0, sigma, intmeth, imx, jmx, kxs, idot, a, ptop, pt, p0, tlp, ts0, tiso)

      IMPLICIT NONE

      !  Interpolate the hydrostatic mminput to nonhydrostatic coordinate.

      INTEGER                     :: I
      INTEGER                     :: IDOT
      INTEGER                     :: IM
      INTEGER                     :: IMX
      INTEGER                     :: INTMETH
      INTEGER                     :: IP
      INTEGER                     :: J
      INTEGER                     :: JM
      INTEGER                     :: JMX
      INTEGER                     :: JP
      INTEGER                     :: K
      INTEGER                     :: KXS
      INTEGER                     :: L
      INTEGER                     :: LL

      REAL                        :: A           ( : )
      REAL                        :: ALNP
      REAL                        :: ALNQVN
      REAL                        :: F           ( : , : , : )
      REAL                        :: FL
      REAL                        :: FN          ( IMX, JMX, KXS )
      REAL                        :: FU
      REAL                        :: piso
      REAL                        :: P0
      REAL                        :: PR0
      REAL                        :: PR0DOT
      REAL                        :: PS          ( : , : )
      REAL                        :: PS0         ( : , : )
      REAL                        :: PT
      REAL                        :: PTOP
      REAL                        :: SIGMA       ( : )
      REAL                        :: TER         ( : , : )
      REAL                        :: tiso
      REAL                        :: TLP
      REAL                        :: TS0
      REAL                        :: TV          ( : , : , : )
      REAL                        :: TVAV
      REAL                        :: Z           ( IMX, JMX, KXS )
      REAL                        :: ziso
      REAL                        :: Z0          ( IMX, JMX, KXS )
      REAL                        :: Z0Q         ( KXS+1 )
      REAL                        :: ZU
      REAL                        :: ZQ          ( KXS+1 )
      REAL                        :: ZL

      piso = p0 * EXP ( ( tiso - ts0 ) / tlp )
      IF (idot .EQ. 1) THEN
!$OMP PARALLEL DO DEFAULT ( SHARED ) &
!$OMP PRIVATE ( i , j , k , pr0 , alnp , ziso , zq , l )
!csd$ parallel do private(i,j,k,pr0,alnp,ziso,zq,l)
         DO j = 1, jmx - 1
            DO i = 1, imx - 1
               DO k = 1, kxs
                  pr0 = ps0(i,j) * a(k) + pt
                  IF ( pr0 .LT. piso ) THEN
                     alnp = ALOG(piso / p0)
                     ziso      = - ( r * tlp / ( 2. * g) * alnp * alnp + r * ts0 / g * alnp)
                     z0(i,j,k) = ziso - r * tiso / g * ALOG ( pr0 / piso ) 
                  ELSE
                     alnp = ALOG(pr0 / p0)
                     z0(i,j,k) = - (r * tlp / (2. * g) * alnp * alnp + r * ts0 / g * alnp)
                  END IF
               END DO

               !  Calculate heights of input temperature sounding for interpolation
               !  to nonhydrostatic model levels.
   
               zq(kxs+1) = ter(i,j)

               DO L = kxs + 1, 1, -1
                  IF (l .NE. kxs+1) THEN
                     zq(l) = zq(l+1) - r / g * tv(i,j,l) * ALOG((sigma(l)*ps(i,j)+ptop)/(sigma(l+1)*ps(i,j)+ptop))
                     z(i,j,l) = 0.5 * (zq(l) + zq(l+1))
                  ENDIF
               END DO
            END DO
         END DO
!csd$ end parallel do

      ELSE

         !  Interpolate dot-point variables (u,v), ps0,ter,ps are dot point values.
   
!$OMP PARALLEL DO DEFAULT ( SHARED ) &
!$OMP PRIVATE ( i , j , k , pr0dot , alnp , ziso , zq , ip , im , jp , jm , l , tvav )
!csd$ parallel do private(i,j,k,pr0dot,alnp,ziso,zq,ip,im,jp,jm,l,tvav)
         DO j = 1, jmx
            DO i = 1, imx
               DO k = 1, kxs
                  pr0dot = ps0(i,j) * a(k) + pt
                  IF ( pr0dot .LT. piso ) THEN
                     alnp = ALOG(piso / p0)
                     ziso      = - ( r * tlp / ( 2. * g) * alnp * alnp + r * ts0 / g * alnp)
                     z0(i,j,k) = ziso - r * tiso / g * ALOG ( pr0dot / piso ) 
                  ELSE
                     alnp = ALOG(pr0dot / p0)
                     z0(i,j,k) = - ( r * tlp / ( 2. * g) * alnp * alnp + r * ts0 / g * alnp)
                  END IF
               END DO
               ip = MIN(i  ,imx-1)
               im = MAX(i-1,1    )
               jp = MIN(j  ,jmx-1)
               jm = MAX(j-1,1    )
               l = kxs + 1
               zq(l) = ter(i,j)

               !  Use tv (uninterpolated values) for z calculation.

               DO l = kxs, 1, -1
                  tvav = 0.25 * (tv(ip,jp,l) + tv(ip,jm,l) + tv(im,jp,l) + tv(im,jm,l))
                  zq(l) = zq(l+1) - r / g * tvav * ALOG((sigma(l  ) * ps(i,j) + ptop ) / (sigma(l+1) * ps(i,j) + ptop ))
                  z(i,j,l) = 0.5 * (zq(l) + zq(l+1))
               END DO
            END DO
         END DO
!csd$ end parallel do
      END IF

      ! Interpolate from z to z0 levels.

!$OMP PARALLEL DO DEFAULT ( SHARED ) &
!$OMP PRIVATE ( i , j , k , ll , l , zu , zl , fu , fl , alnqvn )
!csd$ parallel do private(i,j,k,ll,l,zu,zl,fu,fl,alnqvn)
      DO j = 1, jmx - idot
         DO i = 1, imx - idot
            DO k = 1, kxs
               DO ll = 1, kxs - 1
                  l = ll
                  IF (z(i,j,l+1) .lt. z0(i,j,k)) EXIT
               END DO
               zu = z(i,j,l)
               zl = z(i,j,l+1)
               IF      (intmeth .EQ. 1) THEN
                  fu = f(i,j,l)
                  fl = f(i,j,l+1)
                  fn(i,j,k) = (fu * (z0(i,j,k) - zl ) + fl * (zu - z0(i,j,k))) / (zu - zl)
               ELSE IF (intmeth .EQ. 2) THEN
                  f(i,j,l) = MAX ( f(i,j,l) , 1.E-10 )
                  f(i,j,l+1) = MAX ( f(i,j,l+1) , 1.E-10 )
                  IF (z0(i,j,k) .GT. zu) THEN
                     fn(i,j,k) = f(i,j,l)
                  ELSE
                     fu = ALOG(f(i,j,l  ))
                     fl = ALOG(f(i,j,l+1))
                     alnqvn = (fu * (z0(i,j,k) - zl ) + fl * (zu - z0(i,j,k))) / (zu - zl)
                     fn(i,j,k) = EXP(alnqvn)
                  END IF
               END IF
            END DO 
            DO k = 1, kxs
               f(i,j,k) = fn(i,j,k)
            END DO 
         END DO 
      END DO 
!csd$ end parallel do

      IF (idot .EQ. 1) THEN
         f(:,jmx,:) = f(:,jmx-1,:)
         f(imx,:,:) = f(imx-1,:,:)
      END IF

   END SUBROUTINE nhinterp

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE nhpp (t, pr0, t0, tv, ps, ps0, sigma, imx, jmx, kxs, pt, pp)

      IMPLICIT NONE

      !  Compute the pressure perturbation pp.

      INTEGER                     :: I
      INTEGER                     :: IMX
      INTEGER                     :: J
      INTEGER                     :: JMX
      INTEGER                     :: K
      INTEGER                     :: KXS

      REAL                        :: AA
      REAL                        :: BB
      REAL                        :: CC
      REAL                        :: CHECK
      REAL                        :: CHECKL
      REAL                        :: CHECKR
      REAL                        :: DELP0
      REAL                        :: P0SURF
      REAL                        :: PP          ( : , : , : )
      REAL                        :: PR0         ( : , : , : )
      REAL                        :: PS          ( : , : )
      REAL                        :: PS0         ( : , : )
      REAL                        :: PSP
      REAL                        :: PT
      REAL                        :: PTMP        ( IMX, JMX, KXS )
      REAL                        :: SIGMA       ( : )
      REAL                        :: T           ( : , : , : )
      REAL                        :: T0          ( : , : , : )
      REAL                        :: TK
      REAL                        :: TKP1
      REAL                        :: TV          ( : , : , : )
      REAL                        :: TVK
      REAL                        :: TVKP1
      REAL                        :: TVPOT
      REAL                        :: WTL
      REAL                        :: WTU

      !  Calculate p' at bottom and integrate upwards (hydrostatic balance).
   
      DO j = 1, jmx - 1
         DO i = 1, imx - 1
   
            !  Correct pressure perturbation field (mass) to be consistent 
            !  with psa assuming density perturbation remains constant in 
            !  lowest half-layer.   Start with pp at surface.
  
            p0surf = ps0(i,j) + pt
            psp = ps(i,j) - ps0(i,j)
            delp0 = p0surf - pr0(i,j,kxs)
            tvk = tv(i,j,kxs)
            tk = t(i,j,kxs)
            tvpot = (tvk - t0(i,j,kxs)) / tK
            ptmp(i,j,kxs) = (tvpot * delp0 + psp) / (1. + delp0 / pr0(i,j,kxs))

            DO k = kxs - 1, 1, -1
               tvkp1 = tVK
               tvk = tv(i,j,k)
               tkp1 = tk
               tk = t(i,j,k)
               wtl = (sigma(k+1) - sigma(k  )) / (sigma(k+2) - sigma(k))
               wtu = (sigma(k+2) - sigma(k+1)) / (sigma(k+2) - sigma(k))
               aa = g / (pr0(i,j,k+1) - pr0(i,j,k))
               bb = g * wtl / pr0(i,j,k+1) * t0(i,j,k+1) / tkp1
               cc = g * wtu / pr0(i,j,k  ) * t0(i,j,k  ) / tK
               tvpot = wtl * ((tvkp1 - t0(i,j,k+1)) / tkp1) + wtu * ((tvk - t0(i,j,k  )) / tk)
               ptmp(i,j,k) = (g * tvpot + ptmp(i,j,k+1) * (aa - bb)) / (aa + cc)
            END DO 
         END DO 
      END DO 

      ptmp(:,jmx,:) = ptmp(:,jmx-1,:)
      ptmp(imx,:,:) = ptmp(imx-1,:,:)

      !  Do vertical gradient checK

      DO k = 1, kxs - 1
         DO j = 1, jmx - 1
            DO i = 1, imx - 1
               wtl = (sigma(k+1) - sigma(k  )) / (sigma(k+2) - sigma(k  ))
               wtu = (sigma(k+2) - sigma(k+1)) / (sigma(k+2) - sigma(k  ))
               tvpot = wtl * (tv(i,j,k+1) - t0(i,j,k+1)) / t(i,j,k+1) + wtu * (tv(i,j,k  ) - t0(i,j,k  )) / t(i,j,k  )
               checkl = (ptmp(i,j,k+1) - ptmp(i,j,k)) / (pr0 (i,j,k+1) - pr0 (i,j,k))
               checkr = tvpot &
                      - wtl *   t0(i,j,k+1) /   t(i,j,k+1) * ptmp(i,j,k+1) / pr0(i,j,k+1) &
                      - wtu *   t0(i,j,k)   /   t(i,j,k) *ptmp(i,j,k)      / pr0(i,j,k)
               check = checkl + checkr
               IF (ABS(check) .GT. 1.E-2) THEN
                  PRINT '(A,3I4,3g12.6)','NHPP vert gradient check ',i,j,k,check,checkl,checkr
                  STOP 'nhpp_did_not_converge'
               END IF
            END DO 
         END DO 
      END DO 

      PP = PTMP

   END SUBROUTINE nhpp

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE nhw (u, v, tv, rho0, ps, psdot, ps0, xmsfx, ter, sigma, &
                   imx, jmx, kxs, w, wtop, ds, a, dsigma, ptop, pt, p0, tlp, ts0, tiso)

      IMPLICIT NONE

      !  Compute the nonhydrostatic initial vertical velocity (w) from the horizontal wind fields (u and v).

      INTEGER                     :: I
      INTEGER                     :: IM
      INTEGER                     :: IMX
      INTEGER                     :: IP
      INTEGER                     :: J
      INTEGER                     :: JM
      INTEGER                     :: JMX
      INTEGER                     :: JP
      INTEGER                     :: K
      INTEGER                     :: KM
      INTEGER                     :: KP
      INTEGER                     :: KXS
      INTEGER                     :: L
      INTEGER                     :: LL
      INTEGER                     :: LM
      INTEGER                     :: LP

      REAL                        :: A           ( : )
      REAL                        :: ALNPQ
      REAL                        :: DS
      REAL                        :: DSIGMA      ( : )
      REAL                        :: DX2
      REAL                        :: OMEGA       ( KXS+1 )
      REAL                        :: OMEGAL
      REAL                        :: OMEGAN      ( KXS+1 )
      REAL                        :: OMEGAU
      REAL                        :: piso
      REAL                        :: P0
      REAL                        :: PS          ( : , : )
      REAL                        :: PS0         ( : , : )
      REAL                        :: PSDOT       ( : , : )
      REAL                        :: pr0
      REAL                        :: PT
      REAL                        :: PTOP
      REAL                        :: QDT         ( KXS+1 )
      REAL                        :: RHO
      REAL                        :: RHO0        ( : , : , : )
      REAL                        :: SIGMA       ( : )
      REAL                        :: TER         ( : , : )
      REAL                        :: tiso
      REAL                        :: TLP
      REAL                        :: TS0
      REAL                        :: TV          ( : , : , : )
      REAL                        :: U           ( : , : , : )
      REAL                        :: UBAR
      REAL                        :: V           ( : , : , : )
      REAL                        :: VBAR
      REAL                        :: W           ( : , : , : )
      REAL                        :: WTOP        ( : , : )
      REAL                        :: WTMP        ( IMX, JMX, KXS+1 )
      REAL                        :: XMSFX       ( : , : )
      REAL                        :: ziso
      REAL                        :: Z0Q         ( KXS+1 )
      REAL                        :: ZL
      REAL                        :: ZQ          ( KXS+1 )
      REAL                        :: ZU

      dx2 = 2. * ds
      piso = p0 * EXP ( ( tiso - ts0 ) / tlp )

!$OMP PARALLEL DO DEFAULT ( SHARED ) &
!$OMP PRIVATE ( i , j , k , qdt , z0q , pr0 , alnpq , ziso , zq , ip , im , jp , jm , lp , lm , &
!$OMP           ubar , vbar , omega , kp , km , l , ll , zu , zl , omegau , omegal , omegan , rho )
!cmeys!csd$ parallel do private(i,j,k,qdt,z0q,pr0,alnpq,ziso,zq,ip,im,jp,jm,lp,lm,ubar,vbar,omega,kp,km,l,ll,zu,zl,omegau,omegal,megan,rho)
      DO j = 1, jmx - 1
         DO i = 1, imx - 1
            qdt(kxs+1) = 0.
            z0q(kxs+1) = ter(i,j)
            IF ( ps0(i,j) + pt .LT. piso ) THEN
               PRINT '(A,F5.1,A,F6.1,A)','The chosen value of Tiso, ',tiso,' K occurs at ',piso,' hPa.'
               PRINT '(A)','This value of pressure is .GT. the reference surface pressure.'
!              STOP 'Tiso_too_warm'
            END IF
            DO k = 1, kxs
               pr0 = ps0(i,j) * sigma(k) + pt
               IF ( pr0 .LT. piso ) THEN
                  alnpq = ALOG(piso / p0)
                  ziso = - ( r * tlp / ( 2. * g) * alnpq * alnpq + r * ts0 / g * alnpq)
                  z0q(k) = ziso - r * tiso / g * ALOG ( pr0 / piso ) 
               ELSE
                  alnpq = ALOG((ps0(i,j) * sigma(k) + pt) / p0)
                  z0q(k) = - (r * tlp / (2. * g) * alnpq * alnpq + r * ts0 / g * alnpq)
               END IF
            END DO
            zq(kxs+1) = ter(i,j)
            qdt(kxs+1) = 0.
            DO l = kxs + 1, 1, -1
               IF (l .NE. kxs + 1) THEN
                  zq(l) = zq(l+1) - r / g * tv(i,j,l) * ALOG((sigma(l)   * ps(i,j) + ptop ) / &
                                                             (sigma(l+1) * ps(i,j) + ptop ))
                  qdt(l) = qdt(l+1) &
                         + (u(i+1,j+1,l) * psdot(i+1,j+1) &
                         +  u(i  ,j+1,l) * psdot(i  ,j+1) &
                         -  u(i+1,j  ,l) * psdot(i+1,j  ) &
                         -  u(i  ,j  ,l) * psdot(i  ,j  ) &
                         +  v(i+1,j+1,l) * psdot(i+1,j+1) &
                         +  v(i+1,j  ,l) * psdot(i+1,j  ) &
                         -  v(i  ,j+1,l) * psdot(i  ,j+1) &
                         -  v(i  ,  j,l) * psdot(i  ,j  )) / &
                            dx2 * xmsfx(i,j) * xmsfx(i,j)  * dsigma(l) / ps(i,j)
               ENDIF

               ip = MIN(i+1,imx-1)
               im = MAX(i-1,1)
               jp = MIN(j+1,jmx-1)
               jm = MAX(j-1,1)
               lp = MIN(l,kxs)
               lm = MAX(l-1,1)

               ubar = 0.125 * (u(i  ,j  ,lp) + u(i  ,j  ,lm) &
                    +          u(i+1,j  ,lp) + u(i+1,j  ,lm) &
                    +          u(i  ,j+1,lp) + u(i  ,j+1,lm) &
                    +          u(i+1,j+1,lp) + u(i+1,j+1,lm))
               vbar = 0.125 * (v(i  ,j  ,lp) + v(i  ,j  ,lm) &
                    +          v(i+1,j  ,lp) + v(i+1,j  ,lm) &
                    +          v(i  ,j+1,lp) + v(i  ,j+1,lm) &
                    +          v(i+1,j+1,lp) + v(i+1,j+1,lm))
  
               !  Calculate omega (msfx not inverted).

               omega(l) = ps(i,j) * qdt(l) + sigma(l) &
                        * ((ps(i ,jp) - ps(i ,jm)) * ubar +  (ps(ip,j ) - ps(im,j )) * vbar) &
                        / dx2 * xmsfx(i,j)

            END DO

            !  Vertical velocity from interpolated omega, zero at top.

            DO k = 2, kxs + 1
               kp = MIN(k,kxs)
               km = MAX(k-1,1)
               IF (k .EQ. kxs+1) km = kxs - 1
               DO ll = 1, kxs
                  l = ll
                  IF (zq(l+1) .LT. z0q(k))  EXIT
               END DO

               zu = zq(l)
               zl = zq(l+1)
               omegau = omega(l)
               omegal = omega(l+1)
               omegan(k) = (omegau * (z0q(k) - zl ) + omegal * (zu - z0q(k))) / (zu - zl)

               !  W =~ -OMEGA/RHO0/G *1000*PS0/1000. (OMEGA IN CB)

               rho = (rho0(i,j,km) * (a(kp) - sigma(k)) + rho0(i,j,kp) * (sigma(k) - a(km) )) &
                   / (a(kp) - a(km))
               wtmp(i,j,k) = - omegan(k) / rho / G
            END DO

            wtmp(i,j,1) = 0.
         END DO
      END DO
!cmeys!csd$ end parallel do

      DO j = 1, jmx-1
         DO i = 1, imx-1
            wtop(i,j) = wtmp(i,j,1)
            DO k = 2, kxs + 1
               w(i,j,k-1) = wtmp(i,j,k)
            END DO
         END DO
      END DO

      wtop(:,jmx) = wtop(:,jmx-1)
      wtop(imx,:) = wtop(imx-1,:)
      w(:,jmx,:) = w(:,jmx-1,:)
      w(imx,:,:) = w(imx-1,:,:)

   END SUBROUTINE nhw

END MODULE NH_INTERP
