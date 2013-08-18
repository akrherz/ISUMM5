MODULE hydro_interp

   USE util

CONTAINS

   SUBROUTINE insert (porig, sfc_p, imx, jmx, kx, ksafe, pout, ifirst, icrsdot)

      IMPLICIT NONE


      INTEGER                     :: I
      INTEGER                     :: IFIRST
      INTEGER                     :: ICRSDOT
      INTEGER                     :: IMX
      INTEGER                     :: J
      INTEGER                     :: JMX
      INTEGER                     :: JMXM
      INTEGER                     :: K
      INTEGER                     :: KSF
      INTEGER                     :: KX

      REAL                        :: KSAFE       ( : , : )
      REAL                        :: PORIG       ( : , : , : )
      REAL                        :: POUT        ( : , : , : )
      REAL                        :: SFC_P       ( : , : )

      IF (IFIRST .EQ. 0. ) THEN

         KSAFE = 1.

         !  Compute ksafe to store the k level for each i,j where p at k+1 > sfc_p(i,j) >= p at k.
   
!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j , k ) 
!csd$ parallel do private(i,j,k)
         DO j = 1, jmx - icrsdot
            DO i = 1, imx - icrsdot
               DO k = 1, kx - 1
                  IF ( ( porig(i,j,k)   .LE. sfc_p(i,j) ) .AND. &
                       ( porig(i,j,k+1) .GT. sfc_p(i,j) )  ) THEN
                     ksafe(i,j) = k
                     EXIT
                  END IF
               END DO
            END DO
!csd$ end parallel do
         END DO
   
         IF (icrsdot .EQ. 1 ) THEN
            ksafe(:,jmx) = ksafe(:,jmx - 1)
            ksafe(imx,:) = ksafe(imx - 1,:)
         END IF

      END IF

      !  Insert p, u, v, theta, rh surface into the 3d domain.

!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j , k , ksf ) 
!csd$ parallel do private(i,j,k,ksf)
      DO j = 1, jmx - icrsdot
         DO i = 1, imx - icrsdot
            ksf = NINT(ksafe(i,j))

            IF (ksf .EQ. 1 ) THEN        ! SURFACE VALUE IS ON THE BOTTOM
               DO k = 1, kx
                  pout(i,j,k) = porig(i,j,k)
               END DO
               pout(i,j,kx + 1) = sfc_p(i,j)

            ELSE                    ! INSERT SURFACE VALUE IN THE MIDDLE

               DO k = 1, ksf
                  pout(i,j,k) = porig(i,j,k)
               END DO
               pout(i,j,ksf + 1) = sfc_p(i,j)

               DO k = ksf + 1, kx
                  pout(i,j,k + 1) = porig(i,j,k)
               END DO
            END IF
         END DO
      END DO
!csd$ end parallel do

      IF (icrsdot .EQ. 1 ) THEN
         pout(:,jmx,:) = pout(:,jmx - 1,:)
         pout(imx,:,:) = pout(imx - 1,:,:)
      END IF

   END SUBROUTINE insert

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE intdrvr (fold, fsfc, sfc_p, imx, jmx, kx, &
                       icrdtold, icrdtnew, iusesfc, ialready, intmeth, &
                       sighup, pstarcrs, ptop, pup, fnew, kxs,   &
                       imxm, jmxm, ksafec, ksafed, ppdlin,  &
                       ppdlog, ppclin, ppclog, psdlin, psdlog, psclin, &
                       psclog, ptrapdot, ptrapcrs, pcorig, iptrpok, &
                       isfc)

      
      IMPLICIT NONE

      INTEGER                     :: I
      INTEGER                     :: IALREADY
      INTEGER                     :: ICRDTNEW
      INTEGER                     :: ICRDTOLD
      INTEGER                     :: IMX
      INTEGER                     :: IMXM
      INTEGER                     :: INTMETH
      INTEGER                     :: IPTRPOK
      INTEGER                     :: ISFC
      INTEGER                     :: IUSESFC
      INTEGER                     :: J
      INTEGER                     :: JMX
      INTEGER                     :: JMXM
      INTEGER                     :: K
      INTEGER                     :: KX
      INTEGER                     :: KXS

      REAL                        :: dum2d       ( imx , jmx )
      REAL                        :: FNEW        ( : , : , : )
      REAL                        :: FOLD        ( : , : , : )
      REAL                        :: FSFC        ( : , : )
      REAL                        :: KSAFEC      ( : , : )
      REAL                        :: KSAFED      ( : , : )
      REAL                        :: PCORIG      ( : , : , : )
      REAL                        :: PDORIG      ( IMX , JMX , KX )
      REAL                        :: PPCLIN      ( : , : , : )
      REAL                        :: PPCLOG      ( : , : , : )
      REAL                        :: PPDLIN      ( : , : , : )
      REAL                        :: PPDLOG      ( : , : , : )
      REAL                        :: PSCLIN      ( : , : , : )
      REAL                        :: PSCLOG      ( : , : , : )
      REAL                        :: PSDLIN      ( : , : , : )
      REAL                        :: PSDLOG      ( : , : , : )
      REAL                        :: PSTARCRS    ( : , : )
      REAL                        :: PTOP
      REAL                        :: PTRAPCRS    ( : , : , : )
      REAL                        :: PTRAPDOT    ( : , : , : )
      REAL                        :: PUP         ( : )
      REAL                        :: SFC_P       ( : , : )
      REAL                        :: SFCPDOT     ( IMX , JMX )
      REAL                        :: SIGHUP      ( : )
      REAL                        :: TMP         ( IMX , JMX , KX + 1 )

      !  Calculate 3d pressure fields if first time through this time period.

      IF (ialready .EQ. 0 ) THEN

         ialready = 1

         !  Construct 3d pressure on sigma level array (cross point)
   
         DO k = 1, kxs
            psclin(:,:,k) = sighup(k) * pstarcrs(:,:) + ptop
         END DO
   
         !  Insert surface pressure (sfc_p) into 3d pressure field (pcorig),
         !  do the cross point field used for linear interpolation first.
   
         CALL insert (pcorig, sfc_p, imx, jmx, kx, ksafec, ppclin, 0, 1)
   
         !  Now the dot point field (ppdlin) for linear interpolation
   
         pdorig = pcorig
         DO k = 1 , kx
            CALL crs2dot (pdorig(1,1,k), imx, jmx)
         END DO
         sfcpdot = sfc_p
         CALL crs2dot (sfcpdot, imx, jmx)
         CALL insert (pdorig, sfcpdot, imx, jmx, kx, ksafed, ppdlin, 0, 0)
   
         !  Configure the pressure field for logrithmic interpolation.
   
         ppclog = ALOG(ppclin)
         ppdlog = ALOG(ppdlin)
         psclog = ALOG(psclin)
   
         !  Make the dot point fields for the sigma side of interp.
   
         psdlin = psclin
         psdlog = psclog
         DO k = 1 , kxs
            dum2d = psdlin(:,:,k)
            CALL crs2dot (dum2d, imx, jmx)
            psdlin(:,:,k) = dum2d
            dum2d = psdlog(:,:,k)
            CALL crs2dot (dum2d, imx, jmx)
            psdlog(:,:,k) = dum2d
         END DO

      END IF

      !  Create a surface field for interpolation if necessary

      IF (iusesfc .EQ. 0 ) then                 ! make surface data
         IF      (icrdtnew .EQ. 0 ) THEN              ! DOT POINT, LIN INTERP
            CALL makesfc (fold, ppdlin, ksafec, ksafed, imx, jmx, kx, icrdtold, fsfc)
         ELSE IF (intmeth .EQ. 1 ) THEN           ! CROSS POINT, LIN INTERP
            CALL makesfc (fold, ppclin, ksafec, ksafed, imx, jmx, kx, icrdtold, fsfc)
         ELSE IF (intmeth .EQ. 2 ) THEN           ! CROSS POINT, LOG INTERP
            CALL makesfc (fold, ppclog, ksafec, ksafed, imx, jmx, kx, icrdtold, fsfc)
         END IF
      END IF

      !  Put surface value (fsfc) into the full 3d field (fold).
      !  NOTE:  Insertion of fsfc into fold increases the array by one
      !  vertical dimension.  Store the answer in tmp.

      IF      (icrdtold .EQ. 1 ) THEN
         CALL insert (fold, fsfc, imx, jmx, kx, ksafec, tmp, 1, icrdtold)
      ELSE IF (icrdtold .EQ. 0 ) THEN
         CALL insert (fold, fsfc, imx, jmx, kx, ksafed, tmp, 1, icrdtold)
      END IF

      !  Put input variable on the same grid stagger as output variable.

      IF (icrdtold .NE. icrdtnew ) THEN
         IF (icrdtnew .EQ. 1 ) THEN
            DO k = 1 , kx
               CALL dot2crs (tmp(1,1,k), imx, jmx)
            END DO
            tmp(:,jmx,:) = tmp(:,jmxm,:)
            tmp(imx,:,:) = tmp(imxm,:,:)
         ELSE
            DO k = 1 , kx
               CALL crs2dot (tmp(1,1,k), imx, jmx)
            END DO
         END IF
      END IF

      !  Interpolate field to new coordinate system.

      IF      (icrdtnew .EQ. 0 ) THEN       ! DOT POINT, LINEAR INTERP
         CALL prs2sig (tmp, ppdlin, imx, jmx, kx + 1, icrdtnew, fnew, psdlin, kxs, ptrapdot, iptrpok, ksafed, isfc )
      ELSE IF (intmeth .EQ. 1 ) THEN    ! CROSS POINT, LINEAR INTERP
         CALL prs2sig (tmp, ppclin, imx, jmx, kx + 1, icrdtnew, fnew, psclin, kxs, ptrapcrs, iptrpok, ksafec, isfc )
      ELSE IF (intmeth .EQ. 2 ) THEN    ! CROSS POINT, LOGRITHMIC INTERP
         CALL prs2sig (tmp, ppclog, imx, jmx, kx + 1, icrdtnew, fnew, psclog, kxs, ptrapcrs, iptrpok, ksafec, isfc )
      END IF

   END SUBROUTINE intdrvr

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE makesfc (f, p, ksafec, ksafed, imx, jmx, kx, icrsdot, fsfc)

      IMPLICIT NONE

      INTEGER                     :: I
      INTEGER                     :: ICRSDOT
      INTEGER                     :: IMX
      INTEGER                     :: J
      INTEGER                     :: JMX
      INTEGER                     :: KBOT
      INTEGER                     :: KSFC
      INTEGER                     :: KTEMP
      INTEGER                     :: KTOP
      INTEGER                     :: KX

      REAL                        :: DELP
      REAL                        :: F           ( : , : , : )
      REAL                        :: FSFC        ( : , : )
      REAL                        :: KSAFEC      ( : , : )
      REAL                        :: KSAFED      ( : , : )
      REAL                        :: P           ( : , : , : )

      !  Create a surface value.

      IF (icrsdot .EQ. 1 ) THEN
         DO j = 1, jmx - icrsdot
            DO i = 1, imx - icrsdot

               ktemp = NINT(ksafec(i,j))

               IF (ktemp .EQ. 1 ) THEN
                  ktop = kx
               ELSE
                  ktop = ktemp
               END IF

               IF (ktop .EQ. kx ) THEN
                  ksfc = kx
                  kbot = kx
               ELSE
                  ksfc = ktop + 1
                  kbot = ktop + 2
               END IF

               IF (ktop .EQ. kbot ) THEN
                  delp = 1.
               ELSE
                  delp = p(i,j,ktop) - p(i,j,kbot)
               END IF

               fsfc(i,j) =  f(i,j,ktop) - (f(i,j,ktop)  - f(i,j,ksfc)) * (p(i,j,ktop) -  p(i,j,ksfc)) / delp

            END DO
         END DO

         fsfc(:,jmx) = fsfc(:,jmx-1)
         fsfc(imx,:) = fsfc(imx-1,:)

      ELSE IF (icrsdot .EQ. 0 ) THEN
         DO j = 1, jmx
            DO i = 1, imx

               ktemp = NINT(ksafed(i,j))

               IF (ktemp .EQ. 1 ) THEN
                 ktop = kx
               ELSE
                 ktop = ktemp
               END IF

               IF (ktop .EQ. kx ) THEN
                 ksfc = kx
                 kbot = kx
               ELSE
                 ksfc = ktop + 1
                 kbot = ktop + 2
               END IF

               IF (ktop .EQ. kbot ) THEN
                 delp = 1.
               ELSE
                 delp = p(i,j,ktop) - p(i,j,kbot)
               END IF

               fsfc(i,j) =  f(i,j,ktop) - (f(i,j,ktop)  - f(i,j,ksfc)) * (p(i,j,ktop) -  p(i,j,ksfc)) / delp

            END DO 
         END DO 
      END IF

   END SUBROUTINE makesfc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE prs2sig (fold, pold, imx, jmx, kxold, icrsdot, fnew, &
                       pnew, kxnew, ptrap, iptrpok, ksafe , isfc )

      IMPLICIT NONE

      INTEGER                     :: I
      INTEGER                     :: ICRSDOT
      INTEGER                     :: IMX
      INTEGER                     :: IPTRPOK
      INTEGER                     :: ISFC
      INTEGER                     :: J
      INTEGER                     :: JMX
      INTEGER                     :: KB
      INTEGER                     :: KLOOP
      INTEGER                     :: KN
      INTEGER                     :: KO
      INTEGER                     :: KT
      INTEGER                     :: KTEMP
      INTEGER                     :: KXNEW
      INTEGER                     :: KXOLD

      REAL                        :: FNEW        ( : , : , : )
      REAL                        :: FOLD        ( : , : , : )
      REAL                        :: KSAFE       ( : , : ) 
      REAL                        :: PNEW        ( : , : , : )
      REAL                        :: POLD        ( : , : , : )
      REAL                        :: PTRAP       ( : , : , : )

      !  Trap new pressure data between old pressure levels.

      IF (iptrpok .EQ. 0 ) THEN
!$OMP PARALLEL DO DEFAULT ( SHARED ) &
!$OMP PRIVATE ( i , j , kloop , kn , ktemp , ko )
!cmeys!csd$ parallel do private(i,j,kloop,kn,ktemp,ko)
         DO j = 1, jmx - icrsdot
            DO i = 1, imx - icrsdot
               kloop = 1
               DO kn = 1, kxnew
                  ktemp = 0 
                  !  Find old k location above the new coordinate at this i,j for each new level.  
                  IF (pold(i,j,kloop) .LE. pnew(i,j,kn) ) THEN
                     DO ko = kloop, kxold - 1
                        IF ((pold(i,j,ko)   .LE. pnew(i,j,kn)) .AND. &
                            (pold(i,j,ko+1) .GT. pnew(i,j,kn))) THEN
                           ktemp = ko
                        END IF
                     END DO
                     ptrap(i,j,kn) = ktemp
                     kloop = ktemp
                  ELSE
                     PRINT '(A,5i5)','THIS SHOULD NOT APPEAR, I, J, KN, KO, kloop= ', I,J,KN,KO,kloop
                     STOP 'out_of_order_interpolation'
                  END IF
               END DO
            END DO
         END DO
!cmeys!csd$ end parallel do

         IF (icrsdot .EQ. 1 ) THEN
            ptrap(:,jmx,:) = ptrap(:,jmx-1,:)
            ptrap(imx,:,:) = ptrap(imx-1,:,:)
         END IF

      END IF

      !  Do the vertical interpolation for everything defined above the "surface".

!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j , kn , kt , kb )
!csd$ parallel do private( i , j , kn , kt , kb )
      DO j = 1 , jmx - icrsdot
         DO kn = 1 , kxnew - isfc
            DO i = 1 , imx - icrsdot
               kt = NINT(ptrap(i,j,kn))                ! K TOP
               kb = kt + 1                             ! K BOTTOM
               fnew(i,j,kn) =  fold(i,j,kt) - (fold(i,j,kt) - fold(i,j,kb)) * &
                               (pold(i,j,kt) - pnew(i,j,kn)) / (pold(i,j,kt) - pold(i,j,kb))
            END DO
         END DO
      END DO
!csd$ end parallel do
  
      !  Do the vertical interpolation within the "surface".

!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j , kn , kt , kb )
!csd$ parallel do private(i , j , kn , kt , kb )
      DO j=1,jmx-icrsdot                              
         DO kn=kxnew-(isfc-1),kxnew-1                     
            DO i=1,imx-icrsdot                             
               kt=INT(ptrap(i,j,kxnew-isfc)+0.001)   ! K TOP   
               IF ( ABS(1.-ksafe(i,j)).lt.0.1 ) THEN             
                  kb=kxold                                   
               ELSE                                         
                  kb=ksafe(i,j)+1               ! K BOTTOM 
               END IF                                      
               IF ( kb.LE.kt ) THEN                         
                  PRINT '(A,4I5)','dave i,j,k=',i,j,kn,kb           
                  kt=kb-1                             
               END IF                                 
               fnew(i,j,kn)=fold(i,j,kt) - (fold(i,j,kt)-fold(i,j,kb))* &
                            (pold(i,j,kt)-pnew(i,j,kn))/(pold(i,j,kt)-pold(i,j,kb))
            END DO
         END DO
      END DO
!csd$ end parallel do

      !  Is the lowest sigma level interpolated or set equal to the "surface" value.

      IF ( isfc.NE.0 ) THEN                                         
!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j , kb )
!csd$ parallel do private(i,j,kb)
         DO j=1,jmx-icrsdot                                
            DO i=1,imx-icrsdot                               
               IF ( ABS(1.-ksafe(i,j)).LT.0.1 ) THEN                
                  kb=kxold                                      
               ELSE                                            
                  kb=ksafe(i,j)+1                             
               END IF                                         
               fnew(i,j,kxnew)=fold(i,j,kb)                 
            END DO
         END DO
!csd$ end parallel do
      END IF                                           

      IF (icrsdot .EQ. 1 ) THEN
         fnew(:,jmx,:) = fnew(:,jmx-1,:)
         fnew(imx,:,:) = fnew(imx-1,:,:)
      END IF

   END SUBROUTINE prs2sig

END MODULE hydro_interp
