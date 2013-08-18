MODULE tc_bogus
   
      USE constants
      USE diags
      USE map_utils
      USE namelist_info
      USE terrain_data
      USE util

CONTAINS

   SUBROUTINE do_bogus ( ix , jx , kx , hdate ,                                       &
                         u11 , v11 , t11 , rh11 , phi11 , pslxx , psldd ,             &
                         ter , mfx , mfd , cor , xlat , latd , xlon , lond , ipres ,  &
                         num_storm , latc_loc , lonc_loc ,                            &
                         vmax )

      IMPLICIT NONE 
   
      INTEGER , INTENT(IN) :: ix , jx , kx
      CHARACTER (LEN=16) , INTENT(IN) :: hdate
      
      !  First guess 3d and 2d fields.  
   
      REAL , DIMENSION (ix,jx,kx) , INTENT(INOUT) :: u11 , v11 , t11 , rh11 , phi11 
      REAL , DIMENSION (ix,jx) , INTENT(INOUT)   :: pslxx , psldd
      REAL , DIMENSION (ix,jx) , INTENT(IN) :: ter , mfx , mfd , cor , xlat , latd , xlon , lond
      
      !  Namelist variables coming through the argument list.
   
      INTEGER , INTENT(IN) :: ipres(kx)
      INTEGER , INTENT(IN) :: num_storm
      REAL , DIMENSION (num_max) , INTENT(IN) :: latc_loc , lonc_loc , vmax
      
      REAL , DIMENSION (ix,jx,kx) :: u1 , v1 , t1 , rh1 , phi1
      REAL , DIMENSION (ix,jx,kx) :: q1 , psi1
      REAL , DIMENSION (ix,jx)    :: pslx , psld
      
      !  Background fields.
   
      REAL , DIMENSION (ix,jx,kx) :: u0, v0, t0, t00, rh0, q0,       &
                                       phi0, psi0, chi

      !  Piles of stinking local variables - who knows what they are!

      INTEGER :: k , nstrm , k00 , &
                 kfrm , kto , k85 , iyr , ndate , n_iter , i , &
                 j , i_mvc , j_mvc , itr , nct 
 
      REAL :: xlatc , xlonc , conef , tlat1 , tlat2 , &
              cnst , ds , &
              p85 , beta , rho , devpc , &
              pprm , phip0 , x0 , y0 , vmx , xico , xjco , &
              xicn , xjcn , disx , disy , xlo , alpha , &
              epsilon , vormx , rad , sum_q , avg_q , q_old , ror , q_new , &
              rh_max , sum_rh , avg_rh , rh_min , rhbkg , rhbog , &
              r_ratio , dph , ps
      
      REAL , PARAMETER :: alphar  = 1.8
      REAL :: rmax 
      REAL :: r_search
      REAL :: r_vor
      REAL :: r_vor2
      REAL :: vmax_ratio

      CHARACTER (LEN=16) :: datec

      !  TC bogus
   
      REAL utcr(ix,jx,kx),  vtcr(ix,jx,kx)
      REAL utcp(ix,jx,kx),  vtcp(ix,jx,kx)
      REAL psitc(ix,jx,kx), psiv(kx)
      REAL vortc(ix,jx,kx), vorv(kx)
      REAL tptc(ix,jx,kx),  rhtc(ix,jx,kx)
      REAL phiptc(ix,jx,kx)
      
      !  Perturbations
   
      REAL psipos(ix,jx,kx), upos(ix,jx,kx), vpos(ix,jx,kx),     &
           tpos(ix,jx,kx),   psi(ix,jx,kx),                      &
           phipos(ix,jx,kx), phip(ix,jx,kx)
      
      !  Final fields.
   
      REAL u2(ix,jx,kx),  v2(ix,jx,kx),                          &
           t2(ix,jx,kx),                                         &
           z2(ix,jx,kx),  phi2(ix,jx,kx),                        &
           rh2(ix,jx,kx), q2(ix,jx,kx)
      
      !  Data
   
      REAL rhmx(kx), vwgt(kx), p(kx)
      
      !  Work arrays
   
      REAL uu(kx), vv(kx)
      REAL vort(ix,jx,kx), div(ix,jx,kx)
      REAL vortsv(ix,jx,kx)
      REAL theta(ix,jx,kx), t_reduce(ix,jx,kx)
      REAL ug(ix,jx,kx),   vg(ix,jx,kx),  vorg(ix,jx,kx)
      REAL uchi(ix,jx,kx), vchi(ix,jx,kx)
      REAL delpx(ix,jx)
      
      REAL outold(ix,jx), outnew(ix,jx)
      REAL rd(ix,jx),     ff(ix,jx)
      REAL tmp1(ix,jx),   tmp2(ix,jx) 
      INTEGER strmci(kx), strmcj(kx)
      
      CHARACTER*2  jproj
      LOGICAL :: l_tcbogus

      !  This is the entire map projection enchilada.

      TYPE(proj_info) :: proj
      REAL :: lat1 , lon1

      !  Pick out the handful of useful constants buried in the TERRAIN header and stick them
      !  into local variable names with some semblance of mnemonisticism.

      xlatc= bhr(2,1)
      xlonc= bhr(3,1)
      conef= bhr(4,1)
      tlat1= bhr(5,1)
      tlat2= bhr(6,1)
      ds   = bhr(9,1)
      
      !  Turn the cryptic map projection integers into equally cryptic 2-letter strings.

      IF      ( bhi(7,1) .EQ. 1) THEN
        jproj = 'LC'
      ELSE IF ( bhi(7,1) .EQ. 2) THEN
        jproj = 'ST'
      ELSE IF ( bhi(7,1) .EQ. 3) THEN
        jproj = 'ME'
      END IF

      !  We need the lower left corner of the lat/lon arrays (i.e., grid point (1,1) in the
      !  ususal GRIB fashion) so that we don't need any of that extraneous cone-factor or nesting
      !  information.  

      lat1 = ter_data(latitude_dot_index)%array(1,1)
      lon1 = ter_data(longitude_dot_index)%array(1,1)

      !  Set up initializations for map projection (defined from TERRAIN header) so that the lat/lon
      !  of the tropical storm can be put into model (i,j) space.  This needs to be done once per 
      !  map projection definition.  Since this is the domain that we are "GOING TO", it is a once
      !  per regridder requirement.  If the user somehow ends up calling this routine for several
      !  time periods, there is no problemos, just a bit of overhead with redundant calls.

      IF( jproj .EQ. 'ME' )THEN
         IF ( lon1  .LT. -180. ) lon1  = lon1  + 360.
         IF ( lon1  .GT.  180. ) lon1  = lon1  - 360.
         IF ( xlonc .LT. -180. ) xlonc = xlonc + 360.
         IF ( xlonc .GT.  180. ) xlonc = xlonc - 360.
         CALL map_set ( proj_merc , lat1 , lon1 , 1., 1., ds , xlonc , tlat1 , tlat2 , proj )
      ELSE IF ( jproj .EQ. 'LC' ) THEN
         CALL map_set ( proj_lc   , lat1 , lon1 , 1., 1., ds , xlonc , tlat1 , tlat2 , proj )
      ELSE IF ( jproj .EQ. 'ST' ) THEN
         CALL map_set ( proj_ps   , lat1 , lon1 , 1., 1., ds , xlonc , tlat1 , tlat2 , proj )
      END IF
   
      !  Contants
   
      cnst  = 0.
   
      !  Skads o' radii parameters (m) for the bogus vortex building.

      rmax = rec5%rmax
      r_search = rec5%r_search
      r_vor = rec5%r_vor
      r_vor2 = r_vor * 4
      beta = rec5%beta
      devpc= rec5%devpc
      vmax_ratio = rec5%vmax_ratio
      
      !  Put the vertical column of pressures into hPa from Pa.  The first level is defined as the surface.
      !  The second level is usually 1000 mb.  The last level, kx, is defined as ptop.

      DO k=1,kx
         p(k) = REAL(ipres(k))*0.01
      END DO

      !  Initialize the vertical profiles for humidity and weighting.
  
      IF ( ( rec2%ptop .EQ. 40000. ) .OR. ( rec2%ptop .EQ. 60000. ) ) THEN
         PRINT '(A)','Hold on pardner, your value for PTOP is gonna cause problems for the TC bogus option.'
         PRINT '(A)','Make it higher up than 400 mb.'
         STOP 'ptop_woes_for_tc_bogus'
      END IF

      IF      ( rec5%vert_variation .EQ. 1 ) THEN
         DO k=1,kx
            IF ( p(k) .GT. 400. ) THEN
               rhmx(k) = rec5%humidity_max
            ELSE
               rhmx(k) = rec5%humidity_max * MAX( 0.1 , (p(k)-rec2%ptop/100.)/(400.-rec2%ptop/100.) )
            END IF
            IF ( p(k) .GT. 600. ) THEN
               vwgt(k) = 1.0
            ELSE IF ( p(k) .LE. 100. ) THEN
               vwgt(k) = 0.0001
            ELSE
               vwgt(k) = MAX ( 0.0001 , (p(k)-rec2%ptop/100.)/(600.-rec2%ptop/100.) )
            END IF
         END DO
      ELSE IF ( rec5%vert_variation .EQ. 2 ) THEN
         IF ( kx .eq. 24 ) THEN
            rhmx = (/ 95.,       95., 95., 95., 95., 95., 95., 95.,      &
                      95., 95.,  95., 95., 95., 90., 85., 80., 75.,      &
                      70., 66.,  60., 39., 10., 10., 10./)
            vwgt = (/ 1.0000,         1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 0.9850,      &
                      0.9680, 0.9500, 0.9290, 0.9060, 0.8810, 0.8500, 0.7580, 0.6500, 0.5100,      &
                      0.3500, 0.2120, 0.0500, 0.0270, 0.0001, 0.0001, 0.0001/)
         ELSE
            PRINT '(A)','Number of vertical levels assumed to be 24 for AFWA TC bogus option'
            STOP 'AFWA_TC_BOGUS_LEVEL_ERROR'
         END IF
      END IF

      !  Save initial fields
   
      u1   = u11
      v1   = v11
      t1   = t11
      rh1  = rh11
      phi1 = phi11*g
      
      pslx = pslxx*0.01 
      psld = psldd*0.01 

      DO k=1,kx
         CALL expand ( rh1(1,1,k) , ix-1 , jx-1 , ix , jx )
         CALL expand ( t1 (1,1,k) , ix-1 , jx-1 , ix , jx )
      END DO
   
      !  Loop over the number of storms to process.
   
      l_tcbogus = .FALSE.
      all_storms : DO nstrm=1,num_storm
      
         k00  = 2
         kfrm = k00
         p85  = 850.
      
         kto  = kfrm
         DO k=kfrm+1,kx
            IF ( p(k) .GE. p85 ) THEN
               kto = kto + 1
            END IF
         END DO
         k85 = kto 
      
         !  Parameters for max wind
   
         rho  = 1.2
         pprm = devpc*100.
         phip0= pprm/rho 
      
         !  Get the bogus location - is it in the domain?
   
         CALL latlon_to_ij ( proj , latc_loc(nstrm) , lonc_loc(nstrm) , x0 , y0 )
      
         IF ( ( x0 .LT. 1. ) .OR. ( x0 .GT. REAL(jx-1) ) .OR. &
              ( y0 .LT. 1. ) .OR. ( y0 .GT. REAL(ix-1) ) ) THEN
            IF ( rec4%print_tc ) THEN
               PRINT '(A,I3,A,A,A)','         Storm position is outside the computational domain.'
               PRINT '(A,2F6.2,A)' ,'         Storm postion: (x,y) = ',x0,y0,'.'
            END IF
            CYCLE all_storms
         END IF

         l_tcbogus = .TRUE.
   
         !  Bogus vortex specifications, vmax (m/s); rmax (m);
   
         vmx = vmax(nstrm) * vmax_ratio
         IF (  latc_loc(nstrm) .LT. 0.  ) THEN
            vmx = -vmx
         END IF
   
         IF (  vmax(nstrm) .LE. 0.  ) THEN
            vmx = SQRT( 2.*(1-beta)*ABS(phip0) )  
         END IF

         xico    = y0
         xjco    = x0
         xicn    = xico
         xjcn    = xjco
         n_iter  = 1
   
         !  Start computing.

         IF ( rec4%print_tc ) THEN
            PRINT '(/,A,I3,A,A,A)'     ,'---> TC: Processing storm number= ',nstrm,' for date= ',hdate(1:16),'.'
            PRINT '(A,F6.2,A,F7.2,A)'  ,'         Storm center lat= ',latc_loc(nstrm),' lon= ',lonc_loc(nstrm),'.'
            PRINT '(A,2F6.2,A)'        ,'         Storm center grid position (x,y)= ',xjcn,xicn,'.'
            PRINT '(A,F5.2,F9.2,A)'    ,'         Storm max wind (m/s) and max radius (m)= ',vmx,rmax,'.'
            PRINT '(A,F5.2,A)'         ,'         Estmated central press dev (mb)= ',devpc,'.'
         END IF

         !  Initialize storm center to (1,1)

         DO k=1,kx
            strmci(k) = 1
            strmcj(k) = 1
         END DO

         !  Define complete field of bogus storm

         DO i=1,ix
            DO j=1,jx
               disx = REAL(j) - xjcn
               disy = REAL(i) - xicn
               CALL rankine(disx,disy,ds,kx,vwgt,rmax,vmx,uu,vv,psiv,vorv)
               DO k=1,kx
                  utcp(i,j,k)  = uu(k)
                  vtcp(i,j,k)  = vv(k)
                  psitc(i,j,k) = psiv(k)
                  vortc(i,j,k) = vorv(k)
               END DO
            END DO
         END DO

         !  Rotate wind to map proj

         DO i=1,ix-1
            DO j=1,jx-1
               xlo = xlonc-lond(i,j)
               IF ( xlo .GT. 180.)xlo = xlo-360.
               IF ( xlo .LT.-180.)xlo = xlo+360.
   
               alpha = xlo*conef*degran*SIGN(1.,xlatc)
               DO k=1,kx
                  utcr(i,j,k) = vtcp(i,j,k)*SIN(alpha)+utcp(i,j,k)*COS(alpha)
                  vtcr(i,j,k) = vtcp(i,j,k)*COS(alpha)-utcp(i,j,k)*SIN(alpha)
               END DO
            END DO
         END DO

         DO k=1,kx
            DO i=1,ix-1
               utcr(i,jx,k) = utcr(i,jx-1,k)
               vtcr(i,jx,k) = vtcr(i,jx-1,k)
            END DO
            DO j=1,jx-1
               utcr(ix,j,k) = utcr(ix-1,j,k)
               vtcr(ix,j,k) = vtcr(ix-1,j,k)
            END DO
   
            utcr( 1,jx,k) = utcr(   2,jx-1,k)
            utcr(ix, 1,k) = utcr(ix-1,   2,k)
            utcr(ix,jx,k) = utcr(ix-1,jx-1,k)
   
            vtcr( 1,jx,k) = vtcr(   2,jx-1,k)
            vtcr(ix, 1,k) = vtcr(ix-1,   2,k)
            vtcr(ix,jx,k) = vtcr(ix-1,jx-1,k)
         END DO

         !  Compute vorticity of FG 

         CALL vor(u1,v1,mfd,mfx,ix,jx,kx,ds,vort)

         !  Compute divergence of FG

         CALL diverg(u1,v1,mfd,mfx,ix,jx,kx,ds,div) 

         !  Compute mixing ratio of FG

         CALL mxratprs(rh1,t1,p*100.,ix,jx,kx,q1)
         q1(:,:,1) = q1(:,:,2)

         DO k=1,kx
            CALL expand ( q1(1,1,k) , ix-1 , jx-1 , ix , jx )
         END DO

         !  Compute initial streamfunction - PSI1 
   
         vortsv = vort
         q0 = q1
   
         !  Solve for streamfunction.

         DO k=1,kx 
            DO i=1,ix
               DO j=1,jx
                  ff(i,j) = vort(i,j,k)
                  tmp1(i,j)= 0.0
               END DO
            END DO
            epsilon = 1.E-2
!              epsilon = 1.E-5
            CALL relax(tmp1,ff,rd,ix,jx,ds,epsilon,alphar)
            DO i=1,ix
               DO j=1,jx
                  psi1(i,j,k) = tmp1(i,j)
               END DO
            END DO
         END DO

         DO k=1,kx
            IF ( latc_loc(nstrm) .GE. 0. ) THEN
               vormx = -1.e10
            ELSE
               vormx =  1.e10
            END IF
   
            i_mvc = 1
            j_mvc = 1
   
            DO i=1,ix
               DO j=1,jx
                  rad = SQRT((REAL(i)-xico)**2.+(REAL(j)-xjco)**2.)*ds
                  IF ( rad .LE. r_search ) THEN
                     IF ( latc_loc(nstrm) .GE. 0. ) THEN
                        IF ( vortsv(i,j,k) .GT. vormx ) THEN
                           vormx = vortsv(i,j,k)
                           i_mvc = i
                           j_mvc = j
                        END IF
                     ELSE IF (latc_loc(nstrm) .LT. 0. ) THEN
                        IF ( vortsv(i,j,k) .LT. vormx ) THEN
                           vormx = vortsv(i,j,k)
                           i_mvc = i
                           j_mvc = j
                        END IF
                     END IF
                  END IF
               END DO
            END DO
            strmci(k) = i_mvc 
            strmcj(k) = j_mvc
   
            DO i=1,ix
               DO j=1,jx
                  rad = SQRT(REAL((i-i_mvc)**2.+(j-j_mvc)**2.))*ds
                  IF ( rad .GT. r_vor ) THEN
                     vort(i,j,k) = 0.
                     div(i,j,k)  = 0.
                  END IF
               END DO
            END DO
   
            DO itr=1,n_iter
               sum_q = 0.
               nct = 0
               DO i=1,ix
                  DO j=1,jx
                     rad = SQRT(REAL(i-i_mvc)**2.+REAL(j-j_mvc)**2.)*ds
                     IF ( (rad .LT. r_vor2).AND.(rad .GE. 0.8*r_vor2) ) THEN
                        sum_q = sum_q + q0(i,j,k)
                        nct = nct + 1
                     END IF
                  END DO
               END DO
               avg_q = sum_q/MAX(REAL(nct),1.)
   
               DO i=1,ix
                  DO j=1,jx
                     q_old = q0(i,j,k)
                     rad = SQRT(REAL(i-i_mvc)**2.+REAL(j-j_mvc)**2.)*ds
                     IF ( rad .LT. r_vor2 ) THEN
                        ror = rad/r_vor2
                        q_new = ((1.-ror)*avg_q) + (ror*q_old)
                        q0(i,j,k) = q_new
                     END IF
                  END DO
               END DO
            END DO
   
         END DO

         !  Compute divergent wind

         DO k=1,kx
            DO i=1,ix
               DO j=1,jx
                   ff(i,j) = div(i,j,k)
                   tmp1(i,j)= 0.0
               END DO
            END DO
            epsilon = 1.e-2
!              epsilon = 1.E-5
            CALL relax(tmp1,ff,rd,ix,jx,ds,epsilon,alphar)
            DO i=1,ix
               DO j=1,jx
                   chi(i,j,k) = tmp1(i,j)
               END DO
            END DO
         END DO
   
         DO k=1,kx
            DO i=2,ix-1
               DO j=2,jx-1
                  uchi(i,j,k) = ((chi(i  ,j  ,k)+chi(i-1,j  ,k))- (chi(i  ,j-1,k)+chi(i-1,j-1,k)))/(2.*ds)
                  vchi(i,j,k) = ((chi(i  ,j  ,k)+chi(i  ,j-1,k))- (chi(i-1,j  ,k)+chi(i-1,j-1,k)))/(2.*ds)
               END DO
            END DO
   
            DO i=2,ix-1
               uchi(i,1,k)  = (chi(i   ,   2,k)-chi(i   ,   1,k))/(2.*ds)
               uchi(i,jx,k) = (chi(i   ,jx  ,k)-chi(i   ,jx-1,k))/(2.*ds)
               vchi(i,1,k)  = (chi(i   ,   2,k)-chi(i -1,   2,k))/(2.*ds)
               vchi(i,jx,k) = (chi(i   ,jx-1,k)-chi(i -1,jx-1,k))/(2.*ds)
            END DO
            DO j=2,jx-1
               uchi(1,j,k)  = (chi(   2,j   ,k)-chi(   2,j -1,k))/(2.*ds)
               uchi(ix,j,k) = (chi(ix-1,j   ,k)-chi(ix-1,j -1,k))/(2.*ds)
               vchi(1,j,k)  = (chi(   2,j -1,k)-chi(   1,j -1,k))/(2.*ds)
               vchi(ix,j,k) = (chi(ix  ,j -1,k)-chi(ix-1,j -1,k))/(2.*ds)
            END DO
   
            uchi( 1, 1,k) = uchi(   2,   2,k)
            uchi( 1,jx,k) = uchi(   2,jx-1,k)
            uchi(ix, 1,k) = uchi(ix-1,   2,k)
            uchi(ix,jx,k) = uchi(ix-1,jx-1,k)
   
            vchi( 1, 1,k) = vchi(   2,   2,k)
            vchi( 1,jx,k) = vchi(   2,jx-1,k)
            vchi(ix, 1,k) = vchi(ix-1,   2,k)
            vchi(ix,jx,k) = vchi(ix-1,jx-1,k)
   
         END DO

         !  Compute background streamfunction (PSI0) and perturbation field (PSI)

         DO k=1,kx 
            DO i=1,ix
               DO j=1,jx
                  ff(i,j)=vort(i,j,k)
                  tmp1(i,j)=0.0
               END DO
            END DO
            epsilon = 1.e-2
!              epsilon = 1.E-5
            CALL relax(tmp1,ff,rd,ix,jx,ds,epsilon,alphar)
            DO i=1,ix
               DO j=1,jx
                  psi(i,j,k)=tmp1(i,j)
               END DO
            END DO
         END DO
   
         DO k=1,kx
            DO i=2,ix-1
               DO j=2,jx-1
                  psi0(i,j,k) = psi1(i,j,k)-psi(i,j,k)
               END DO
            END DO
         END DO
   
         DO k=k00,kx
            DO i=1,ix
               DO j=1,jx
                  psipos(i,j,k)=psi(i,j,k)
               END DO
            END DO
         END DO

         !  Background wind field after the old storm is removed.

         DO k=1,kx
            DO i=2,ix-1
               DO j=2,jx-1
                  upos(i,j,k) = -((psi(i  ,j  ,k)+psi(i  ,j-1,k))-(psi(i-1,j-1,k)+psi(i-1,j  ,k)))/(2.*ds)
                  vpos(i,j,k) =  ((psi(i  ,j  ,k)+psi(i-1,j  ,k))-(psi(i-1,j-1,k)+psi(i  ,j-1,k)))/(2.*ds)
               END DO
            END DO
   
            DO i=2,ix-1
               upos(i,1,k)  = -(psi(i   ,   2,k)-psi(i -1,   2,k))/(2.*ds)
               upos(i,jx,k) = -(psi(i   ,jx-1,k)-psi(i -1,jx-1,k))/(2.*ds)
               vpos(i,1,k)  =  (psi(i   ,   2,k)-psi(i   ,   1,k))/(2.*ds)
               vpos(i,jx,k) =  (psi(i   ,jx  ,k)-psi(i   ,jx-1,k))/(2.*ds)
            END DO
            DO j=2,jx-1
               upos(1,j,k)  = -(psi(   2,j   ,k)-psi(   1,j   ,k))/(2.*ds)
               upos(ix,j,k) = -(psi(ix  ,j   ,k)-psi(ix-1,j   ,k))/(2.*ds)
               vpos(1,j,k)  =  (psi(   2,j   ,k)-psi(   2,j -1,k))/(2.*ds)
               vpos(ix,j,k) =  (psi(ix-1,j   ,k)-psi(ix-1,j -1,k))/(2.*ds)
            END DO
   
            upos( 1, 1,k) = upos(   2,   2,k)
            upos( 1,jx,k) = upos(   2,jx-1,k)
            upos(ix, 1,k) = upos(ix-1,   2,k)
            upos(ix,jx,k) = upos(ix-1,jx-1,k)
   
            vpos( 1, 1,k) = vpos(   2,   2,k)
            vpos( 1,jx,k) = vpos(   2,jx-1,k)
            vpos(ix, 1,k) = vpos(ix-1,   2,k)
            vpos(ix,jx,k) = vpos(ix-1,jx-1,k)
   
         END DO
   
         !  Background u, v fields.

         DO k=1,kx
            DO i=1,ix
               DO j=1,jx
                  u0(i,j,k) = u1(i,j,k)-(upos(i,j,k)+uchi(i,j,k))
                  v0(i,j,k) = v1(i,j,k)-(vpos(i,j,k)+vchi(i,j,k))
               END DO
            END DO
         END DO
   
         !  Geostrophic vorticity.

         CALL geowind(phi1,mfx,mfd,cor,ix,jx,kx,ds,ug,vg)
         CALL vor(ug,vg,mfd,mfx,ix,jx,kx,ds,vorg)

         DO k=1,kx
            i_mvc = strmci(k)
            j_mvc = strmcj(k)
   
            DO i=1,ix
               DO j=1,jx
                  rad = SQRT(REAL(i-i_mvc)**2.+REAL(j-j_mvc)**2.)*ds
                  IF ( rad .GT. r_vor ) THEN
                     vorg(i,j,k) = 0.
                  END IF
               END DO
            END DO
         END DO
   
         DO k=k00,kx
            DO i=1,ix
               DO j=1,jx
                  ff(i,j) = vorg(i,j,k)
                  tmp1(i,j)= 0.0
               END DO
            END DO
            epsilon = 1.e-3
!              epsilon = 1.E-5
            CALL relax(tmp1,ff,rd,ix,jx,ds,epsilon,alphar)
            DO i=1,ix
               DO j=1,jx
                  phip(i,j,k) = tmp1(i,j)
               END DO
            END DO
         END DO
   
         !  Background geopotential.

         DO k=k00,kx
            DO i=1,ix
               DO j=1,jx
                  phi0(i,j,k)   = phi1(i,j,k) - phip(i,j,k)
               END DO
            END DO
         END DO

         !  Background temperature

         DO k=k00,kx 
            DO i=1,ix
               DO j=1,jx
                  IF      ( k .EQ.  2 ) THEN
                     tpos(i,j,k) = (-1./r)*(phip(i,j,k+1)-phip(i,j,k  ))/LOG(p(k+1)/p(k  ))
                  ELSE IF ( k .EQ. kx ) THEN
                     tpos(i,j,k) = (-1./r)*(phip(i,j,k  )-phip(i,j,k-1))/LOG(p(k  )/p(k-1))
                  ELSE
                     tpos(i,j,k) = (-1./r)*(phip(i,j,k+1)-phip(i,j,k-1))/LOG(p(k+1)/p(k-1))
                  END IF

                  t0(i,j,k) = t1(i,j,k)-tpos(i,j,k)
                  t00(i,j,k) = t0(i,j,k)
               END DO
            END DO
         END DO
   
         !  Background RH.

         CALL qvtorh (q0,t0,p*100.,k00,ix,jx,kx,rh0)
         DO k=k00,kx
            CALL expand ( rh0(1,1,k) , ix-1 , jx-1 , ix , jx )
         END DO
   
         DO k=k00,kx
            rh_max= rhmx(k)
            i_mvc = strmci(k)
            j_mvc = strmcj(k)
   
            sum_rh = 0.
            nct = 0
            DO i=1,ix
               DO j=1,jx
                  rad = SQRT(REAL(i-i_mvc)**2.+REAL(j-j_mvc)**2.)*ds
                  IF ( (rad .LT. r_vor2).AND.(rad .GE. 0.8*r_vor2) ) THEN
                     sum_rh = sum_rh + rh0(i,j,k)
                     nct = nct + 1
                  END IF
               END DO
            END DO
            avg_rh = sum_rh/MAX(REAL(nct),1.)
   
            DO i=1,ix
               DO j=1,jx
                  rh_min = avg_rh 
                  rad = SQRT((REAL(i)-xico)**2.+(REAL(j)-xjco)**2.)*ds
                  IF ( rad .LE. rmax ) THEN
                     rhtc(i,j,k) = rh_max
                  ELSE
                     rhtc(i,j,k) = (rmax/rad)*rh_max+(1.-(rmax/rad))*rh_min
                  END IF
               END DO
            END DO
         END DO

         ! adjust T0

         DO k=k00,kx
            DO i=1,ix
               DO j=1,jx
                  theta(i,j,k) = t1(i,j,k)*(1000./p(k))**rovcp
               END DO
            END DO
         END DO
   
         i_mvc = strmci(k00)
         j_mvc = strmcj(k00)
   
         DO k=kfrm,kto
   
            DO i=1,ix
               DO j=1,jx
                  rad = SQRT(REAL(i-i_mvc)**2.+REAL(j-j_mvc)**2.)*ds
                  IF ( rad .LT. r_vor2 ) THEN
                     t_reduce(i,j,k) = theta(i,j,k85)-0.03*(p(k)-p(k85))
                     t0(i,j,k) = t00(i,j,k)*(rad/r_vor2) + (((p(k)/1000.)**rovcp)*t_reduce(i,j,k))*(1.-(rad/r_vor2))
                  END IF
               END DO
            END DO
   
         END DO

         !  New RH.

         DO k=k00,kx
            DO i=1,ix
               DO j=1,jx
                  rhbkg = rh0(i,j,k)
                  rhbog = rhtc(i,j,k)
                  rad = SQRT((REAL(i)-xico)**2.+(REAL(j)-xjco)**2.)*ds
                  IF ( (rad.GT.rmax) .AND. (rad.LE.r_vor2) ) THEN
                     r_ratio = (rad-rmax)/(r_vor2-rmax)
                     rh2(i,j,k) = ((1.-r_ratio)*rhbog) + (r_ratio*rhbkg)
                  ELSE IF (rad .LE. rmax ) THEN
                     rh2(i,j,k) = rhbog
                  ELSE
                     rh2(i,j,k) = rhbkg
                  END IF
               END DO
            END DO
         END DO
   
         !  New wind field.

         DO k=1,kx
            DO i=1,ix
               DO j=1,jx
                  u2(i,j,k) = u0(i,j,k)+utcr(i,j,k)
                  v2(i,j,k) = v0(i,j,k)+vtcr(i,j,k)
               END DO
            END DO
         END DO

         !  Geopotential perturbation

         DO k=1,kx
            DO i=1,ix
               DO j=1,jx
                  tmp1(i,j)=psitc(i,j,k)
               END DO
            END DO
            CALL balance(cor,xlat,tmp1,ix,jx,ds,outold)
            DO i=1,ix
               DO j=1,jx
                  ff(i,j)=outold(i,j)
                  tmp1(i,j)=0.0
               END DO
            END DO
            epsilon = 1.e-3
!              epsilon = 1.E-5
            CALL relax (tmp1,ff,rd,ix,jx,ds,epsilon,alphar)
            DO i=1,ix
               DO j=1,jx
                  phiptc(i,j,k) = tmp1(i,j)
               END DO
            END DO
         END DO
   
         !  New geopotential field.

         DO k=1,kx
            DO i=1,ix
               DO j=1,jx
                  phi2(i,j,k)  = phi0(i,j,k) + phiptc(i,j,k)
               END DO
            END DO
         END DO

         !  New temperature field.

         DO k=k00,kx
            DO i=1,ix
               DO j=1,jx
                  IF      ( k .EQ.  2 ) THEN
                     tptc(i,j,k)=(-1./r)*(phiptc(i,j,k+1)-phiptc(i,j,k  ))/LOG(p(k+1)/p(k  ))
                  ELSE IF ( k .EQ. kx ) THEN
                     tptc(i,j,k)=(-1./r)*(phiptc(i,j,k  )-phiptc(i,j,k-1))/LOG(p(k  )/p(k-1))
                  ELSE
                     tptc(i,j,k)=(-1./r)*(phiptc(i,j,k+1)-phiptc(i,j,k-1))/LOG(p(k+1)/p(k-1))
                  END IF
                  t2(i,j,k) = t0(i,j,k) + tptc(i,j,k)
   
               END DO
            END DO
         END DO

         !  Surface pressure change.

         DO i=1,ix
            DO j=1,jx
               dph = phi2(i,j,k00)-phi1(i,j,k00)
               delpx(i,j) = rho*dph*0.01
            END DO
         END DO
 
         !  New SLP.

         DO i=1,ix
            DO j=1,jx
               pslx(i,j) = pslx(i,j)+delpx(i,j)
            END DO
         END DO
         tmp1=delpx
         CALL crs2dot(tmp1,ix,jx)
         DO i=1,ix
            DO j=1,jx
               psld(i,j) = psld(i,j)+tmp1(i,j)
            END DO
         END DO

         !  Set new geopotential at surface to terrain elevation.

         DO i=1,ix-1
            DO j=1,jx-1
               z2(i,j,1) = ter(i,j) 
            END DO
         END DO
         CALL expand(z2(1,1,1),ix-1,jx-1,ix,jx)
   
         !  Geopotential back to height.

         DO k=k00,kx
            DO i=1,ix
               DO j=1,jx
                  z2(i,j,k) = phi2(i,j,k)/g 
               END DO
            END DO
            CALL expand(z2(1,1,k),ix-1,jx-1,ix,jx)
         END DO

         !  New surface temperature, assuming same theta as from 1000 mb.
         DO i=1,ix
            DO j=1,jx
               ps = pslx(i,j)
               t2(i,j,1) = t2(i,j,k00)*((ps/1000.)**rovcp)
            END DO
         END DO

         !  Set surface RH to the value from 1000 mb.

         DO i=1,ix
            DO j=1,jx
               rh2(i,j,1) = rh2(i,j,k00)
            END DO
         END DO

         !  Modification of tropical storm complete.
   
         IF ( rec4%print_tc ) THEN
            PRINT '(A,I3,A)'       ,'         Bogus storm number ',nstrm,' completed.'
         END IF

         !  Put new values back into the variables for adding in the next storm.

         DO k=1,kx
            DO i=1,ix
               DO j=1,jx
                  u1(i,j,k)   = u2(i,j,k)
                  v1(i,j,k)   = v2(i,j,k)
                  t1(i,j,k)   = t2(i,j,k)
                  rh1(i,j,k)  = rh2(i,j,k)
                  phi1(i,j,k) = phi2(i,j,k)
               END DO
            END DO
         END DO
   
      END DO all_storms

      !   All storms completed
   
      IF ( l_tcbogus  ) THEN
      
         !  Convert to pressures to Pa.
   
         DO i=1,ix
            DO j=1,jx
               pslxx(i,j) = pslx(i,j)*100.
               psldd(i,j) = psld(i,j)*100.
            END DO
         END DO
      
         !  Stick the final variables in to the arrays that are passed out of this routine.

         DO k=1,kx
            DO i=1,ix
               DO j=1,jx
                  u11  (i,j,k) = u2 (i,j,k)
                  v11  (i,j,k) = v2 (i,j,k)
                  t11  (i,j,k) = t2 (i,j,k)
                  rh11 (i,j,k) = rh2(i,j,k)
                  phi11(i,j,k) = z2 (i,j,k)
               END DO
            END DO
         END DO
      
         IF ( rec4%print_tc ) THEN
            PRINT '(A)','         Overwrite first guess with new bogus fields.'
         END IF
      
      ELSE
         IF ( rec4%print_tc ) THEN
            PRINT '(A,A,A)','         No storm bogussing done for date = ',hdate(1:16),'.'
         END IF
      END IF
      
   END SUBROUTINE do_bogus

END MODULE tc_bogus
