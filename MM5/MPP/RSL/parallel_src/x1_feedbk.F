define(DOT_GRID_INTERIOR,`(bi(i,j,RSL_DBDY) .ge. ($1))')
define(CROSS_GRID_INTERIOR,`(bi(i,j,RSL_DBDY_X) .ge. ($1))')

      SUBROUTINE FEEDBK (INEST,IYY,JXX)                                          FEEDBK.1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          FEEDBK.2
C                                                                     C          FEEDBK.3
C                                                                     C          FEEDBK.4
C     THIS SUBROUTINE COMPUTES THE FEEDBACK EFFECTS FROM THE          C          FEEDBK.5
C     NESTED DOMAIN TO THE LARGE DOMAIN.                              C          FEEDBK.6
C                                                                     C          FEEDBK.7
C                                                                     C          FEEDBK.8
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          FEEDBK.9
      IMPLICIT NONE                                                              Cajb_MAR2001
C Input arguments:                                                               Cajb_MAR2001
      integer inest,iyy,jxx                                                      Cajb_MAR2001
C                                                                                Cajb_MAR2001
#     include <rsl.inc>
#     include <rslcom.inc>
      FLIC_RUN_DECL
#     include <parame.incl>
#     include <param2.incl>
#     include <param3.incl>
#     include <addr0.incl>
#     include <point3d.incl>
#     include <point2d.incl>
#     include <nonhyd.incl>
#     include <navypb.incl>
#     include <soil.incl>
#     include <soilp.incl>
#     include <nnnhyd.incl>
#     include <various.incl>
#     include <point3dn.incl>
#     include <point2dn.incl>
#     include <variousn.incl>
#     include <nestl.incl>
#     include <varia.incl>
#     include <pmoist.incl>
#     include <pnavyn.incl>
#     include <soiln.incl>
#     include <soilpn.incl>
#     include <rpstar.incl>
#     include <nhcnst.incl>
#     include <pbltke.incl>
#     include <pbltken.incl>
#     include <zdiffu.incl>
#define FULL_DECLARE
#     include <mpp_dusolve3.incl>
                                                                                 FEEDBK.29
      REAL HSC1(MIX,MJX),HSC2(MIX,MJX)                                           Cajb_MAR2001
      REAL HSC3(MIX,MJX),HSC4(MIX,MJX)                                           Cajb_MAR2001
      REAL HSC6(MIX,MJX)                                                         Cajb_MAR2001
      REAL HSC5(MIX,MJX)                                                         Cajb_MAR2001

      integer i,j,pig,pjg,cm,cn,nig,njg,retval
#include <packdown.incl>
      real dummy
      real valid_mask(mix,mjx), valid_m
      real cross_mask(mix,mjx), cross_m
      real nh_mask(mix,mjx), cross_nh
      real dt_mask(mix,mjx),    dt_m
      real fudge_mask(mix,mjx), fudge_m
      real follow, nofollow
      integer bi(mix,mjx,CROSS_BDY_INFO_LEN)
      integer pid, nid
      real temp_sav(mix,mjx)
      real temp_val(mkx,2)

      logical ff
      real XNU(2)
      integer pass

C Declarations for implicit none                                                 Cajb_MAR2001
      integer in,icoars,ic                                                       Cajb_MAR2001
      integer k,kp,msize                                                         Cajb_MAR2001
      real    pval,p                                                             Cajb_MAR2001
      real    rainc_x,rainnc_x,mavail_x                                          Cajb_MAR2001
      real    snowc_x
      real    uval,vval                                                          Cajb_MAR2001
      real    tofollow                                                           Cajb_MAR2001
      real    asv,aplus,cell                                                     Cajb_MAR2001



      IN=INEST
      ICOARS = NUMNC(INEST)
      IC=ICOARS
      pid = domains(icoars)
      nid = domains(inest)
      call rsl_get_bdy_larray2( nid, bi, CROSS_BDY_INFO_LEN, MIX, MJX )

C Ready the parent by decoupling the fields
      CALL ADDRX1C(IAXALL(1,ICOARS))
      CALL MP_INITDOMAIN(ICOARS)
      call rsl_exch_stencil( nid, sten_rainfbk(inest) )
c
c Decouple all.  Using the nests _sav arrays as scratch storage for smoother.
c The ability to do this depends on the sav variables being reset to their
c proper values at the onset of a time step in a domain.  This is done in the
c mp version of stotndi in this code.  See the file mp_stotndt.F (it contains
c the mp versions of both routines).  Note also that this will not work if
c the static allocation size of data structures for each domain differs (but
c this cannot happen because they are all mix by mjx).
c

        if ((ITPDIF.gt.1).and.(IOVERW(inest).EQ.2)) then
          print*, 'Opening new domain with IOVERW=2 and z-diffusion'
          print*, 'requires recomputing interpolation coefficients'
          do i=1,numnc(inest)
            diffpar(i)=.true.
          enddo
        endif
c
c
      if ( inhyd .eq. 1 ) then
        FLIC_DO_N(j,1,jl-1)
          FLIC_DO_M(i,1,il-1)
            wa_sav(i,j,kl+1,in) = wa(i,j,kl+1) / psa(i,j)
          FLIC_ENDDO
        FLIC_ENDDO
      endif
      do k = 1, kl
        FLIC_DO_N(j,1,jl-1)
          FLIC_DO_M(i,1,il-1)
            pval = psa(i,j)
            ta_sav(i,j,k,in) = ta(i,j,k) / pval
            if ( inhyd .eq. 1 ) then
              wa_sav(i,j,k,in) = wa(i,j,k) / pval
              ppa_sav(i,j,k,in) = ppa(i,j,k) / pval
            endif
            if ( idry(inest) .eq. 0 ) then
              qva_sav(i,j,k,in) = qva(i,j,k) / pval
              if ( imoist(inest) .eq. 2 ) then
                qca_sav(i,j,k,in) = qca(i,j,k) / pval
                qra_sav(i,j,k,in) = qra(i,j,k) / pval
                if ( iice .eq. 1 ) then
                  qia_sav(i,j,k,in) = qia(i,j,k) / pval
                  qnia_sav(i,j,k,in) = qnia(i,j,k) / pval
                  if ( iiceg .eq. 1 ) then
                    qga_sav(i,j,k,in) = qga(i,j,k) / pval
                    qnca_sav(i,j,k,in) = qnca(i,j,k) / pval
                  endif
                endif
              endif
            endif
          FLIC_ENDDO
        FLIC_ENDDO
        FLIC_DO_N(j,1,jl)
          FLIC_DO_M(i,1,il)
            pval = rpdota(i,j)
            ua_sav(i,j,k,in) = ua(i,j,k) * pval
            va_sav(i,j,k,in) = va(i,j,k) * pval
          FLIC_ENDDO
        FLIC_ENDDO
      enddo

      CALL ADDRX1C(IAXALL(1,INEST))
      CALL MP_INITDOMAIN(INEST)

      kl = mkx

      call merge_size( 1, msize )
      msize = msize + 14         ! room for valid data and mask info

      call rsl_to_parent_info( pid, nid, msize*RWORDSIZE,
     +                         i,j,nig,njg,cm,cn,pig,pjg,retval )

      follow = 1.0              ! message contains data
      nofollow = -1.0           ! no data in message

      do while (retval .eq. 1)

        p = (cn-1)*irax+cm
        if ( p .eq. 5 ) then  ! nest point that provides cross point feedback
          if ( .not. DOT_GRID_INTERIOR(4) ) then
            call rsl_to_parent_msg( RWORDSIZE, nofollow )
          else
            call rsl_to_parent_msg( RWORDSIZE, follow )
            if ( CROSS_GRID_INTERIOR(4) ) then
              cross_m = 1.0
            else
              cross_m = -1.0
            endif
            call rsl_to_parent_msg( RWORDSIZE, cross_m )
            if ( CROSS_GRID_INTERIOR(1) ) then
              cross_nh = 1.0
            else
              cross_nh = -1.0
            endif
            call rsl_to_parent_msg( RWORDSIZE, cross_nh )

C NEW FOR R11
C THIS IS INITIALIZED TO ALL TRUE IN PARAM.F
            IF (LFBTER(INEST)) THEN
              call rsl_to_parent_msg( RWORDSIZE, ht(i,j) )
              call rsl_to_parent_msg( RWORDSIZE, satbrt(i,j) )
              call rsl_to_parent_msg( RWORDSIZE, xland(i,j) )
              call rsl_to_parent_msg( RWORDSIZE, psa(i,j) )
              call rsl_to_parent_msg( RWORDSIZE, psb(i,j) )
              call rsl_to_parent_msg( RWORDSIZE, rpsa(i,j) )
              call rsl_to_parent_msg( RWORDSIZE, rpsb(i,j) )
              call rsl_to_parent_msg( RWORDSIZE, rpsc(i,j) )
              call rsl_to_parent_msg( RWORDSIZE, ps0(i,j) )
              call rsl_to_parent_msgx
     +           ( kl, RWORDSIZE, mixnh*mjxnh, t0(i,j,1) )
              call rsl_to_parent_msgx
     +           ( kl, RWORDSIZE, mixnh*mjxnh, pr0(i,j,1) )
              call rsl_to_parent_msgx
     +           ( kl, RWORDSIZE, mixnh*mjxnh, rho0(i,j,1) )
            ENDIF

            if (itgflg(inest).eq.1) then
              call rsl_to_parent_msg( RWORDSIZE, tga(i,j) )
            endif
            call rsl_to_parent_msgx
     +        ( kl, RWORDSIZE, mix*mjx, ta(i,j,1) )
            call rsl_to_parent_msgx
     +        ( kl+1, RWORDSIZE, mixnh*mjxnh, wa(i,j,1) )
            call rsl_to_parent_msgx
     +        ( kl, RWORDSIZE, mixnh*mjxnh, ppa(i,j,1) )
            if ( idry(inest) .eq. 0 ) then
              call rsl_to_parent_msgx
     +          ( kl, RWORDSIZE, mix*mjx, qva(i,j,1) )
              if ( imoist(inest) .eq. 2 ) then
                call rsl_to_parent_msgx
     +            ( kl, RWORDSIZE, mixm*mjxm, qca(i,j,1) )
                call rsl_to_parent_msgx
     +            ( kl, RWORDSIZE, mixm*mjxm, qra(i,j,1) )
                if ( iice .eq. 1 ) then
                  call rsl_to_parent_msgx
     +              ( kl, RWORDSIZE, mixic*mjxic, qia(i,j,1) )
                  call rsl_to_parent_msgx
     +              ( kl, RWORDSIZE, mixic*mjxic, qnia(i,j,1) )
                  if ( iiceg .eq. 1 ) then
                    call rsl_to_parent_msgx
     +                ( kl, RWORDSIZE, mixig*mjxig, qga(i,j,1) )
                    call rsl_to_parent_msgx
     +                ( kl, RWORDSIZE, mixig*mjxig, qnca(i,j,1) )
                  endif
                endif
              endif
            endif
            if (inav.eq.1) then
              call rsl_to_parent_msgx
     +          ( kl, RWORDSIZE, mixv*mjxv, turke(i,j,1) )
              call rsl_to_parent_msgx
     +          ( kl, RWORDSIZE, mixv2*mjxv2, pthlsb(i,j,1) )
              call rsl_to_parent_msgx
     +          ( kl, RWORDSIZE, mixv2*mjxv2, pqwsb(i,j,1) )
              call rsl_to_parent_msgx
     +          ( kl, RWORDSIZE, mixv2*mjxv2, pthlqwb(i,j,1) )
            endif
C Add TKEA feedback
            if (igspbl.eq.1) then
              call rsl_to_parent_msgx
     +          ( mlx, RWORDSIZE, mixs*mjxs, tkea(i,j,1) )
            endif
            if (isoil.eq.1) then
              call rsl_to_parent_msgx
     +          ( mlx, RWORDSIZE, mixg*mjxg, tsla(i,j,1) )

            endif
            RAINC_X =  (RAINC(i,j)+RAINC(i+1,j)+
     +                  RAINC(i-1,j)+RAINC(i,j+1)+RAINC(i,j-1)+
     +                  RAINC(i+1,j+1)+RAINC(i+1,j-1)+
     +                  RAINC(i-1,j+1)+RAINC(i-1,j-1))/9.
            call rsl_to_parent_msg( RWORDSIZE, RAINC_X )
            RAINNC_X =  (RAINNC(i,j)+RAINNC(i+1,j)+
     +                  RAINNC(i-1,j)+RAINNC(i,j+1)+RAINNC(i,j-1)+
     +                  RAINNC(i+1,j+1)+RAINNC(i+1,j-1)+
     +                  RAINNC(i-1,j+1)+RAINNC(i-1,j-1))/9.
            call rsl_to_parent_msg( RWORDSIZE, RAINNC_X )
C added 2000/09/01 jm
            if (imoiav(inest).ge.1) then
              MAVAIL_X =  (MAVAIL(i,j)+MAVAIL(i+1,j)+
     +                    MAVAIL(i-1,j)+MAVAIL(i,j+1)+MAVAIL(i,j-1)+
     +                    MAVAIL(i+1,j+1)+MAVAIL(i+1,j-1)+
     +                    MAVAIL(i-1,j+1)+MAVAIL(i-1,j-1))/9.
              call rsl_to_parent_msg( RWORDSIZE, MAVAIL_X )
            endif
C added snowc and snoa 2001/11/09 ajb
            if (ifsnow(inest).eq.2) then
              snowc_X =  (snowc(i,j)+snowc(i+1,j)+
     +                    snowc(i-1,j)+snowc(i,j+1)+snowc(i,j-1)+
     +                    snowc(i+1,j+1)+snowc(i+1,j-1)+
     +                    snowc(i-1,j+1)+snowc(i-1,j-1))/9.
              call rsl_to_parent_msg( RWORDSIZE, snowc_X )
            endif
C added seaice 20030720
            if (iexsi.ge.1) then
              call rsl_to_parent_msg( RWORDSIZE, seaice(i,j) )
              call rsl_to_parent_msg( RWORDSIZE, tgsi(i,j) )
            endif
C removed above snoa, added single-point lsm feedback 2002/10/25 jd
            if (isoil.eq.2) then
              call rsl_to_parent_msg( RWORDSIZE, snoa(i,j) )
              call rsl_to_parent_msg( RWORDSIZE, snowha(i,j) )
              call rsl_to_parent_msg( RWORDSIZE, cmca(i,j) )
              call rsl_to_parent_msg( RWORDSIZE, snowc(i,j) )
              call rsl_to_parent_msgx
     +          ( mlx, RWORDSIZE, mixl*mjxl, stca(i,j,1) )
              call rsl_to_parent_msgx
     +          ( mlx, RWORDSIZE, mixl*mjxl, smca(i,j,1) )
              call rsl_to_parent_msgx
     +          ( mlx, RWORDSIZE, mixl*mjxl, swca(i,j,1) )
            endif
          endif
        else if ( p .eq. 1 ) then ! nest point that provides dot fdbk
          if ( DOT_GRID_INTERIOR(3) ) then
            dt_m = 1.0
          else
            dt_m = -1.0
          endif
          call rsl_to_parent_msg( RWORDSIZE, dt_m )
          if ( (bi(i,j,RSL_DBDY) .eq. 1) ) then
            fudge_m = 1.0
          else
            fudge_m = -1.0
          endif
          call rsl_to_parent_msg( RWORDSIZE, fudge_m )

C Feed back decoupled values of wind velocities (R11)
          if ( dt_m .gt. 0.0 ) then
            pval = rpdota(i,j)
            do k = 1, kl
              temp_val(k,1) = ua(i,j,k)*pval
              temp_val(k,2) = va(i,j,k)*pval
            enddo
            call rsl_to_parent_msg( 2 * mkx * RWORDSIZE, temp_val(1,1) )
          endif
        endif
        call rsl_to_parent_info( pid, nid, msize*RWORDSIZE,
     +                          i,j,nig,njg,cm,cn,pig,pjg,retval )
      enddo
C
      call rsl_merge_msgs
C
      do j = 1, mjx
        do i = 1, mix
          valid_mask(i,j) = -1
          cross_mask(i,j) = -1
          dt_mask(i,j) = -1
          fudge_mask(i,j) = -1
          nh_mask(i,j) = -1.
        enddo
      enddo
     
      CALL MP_INITDOMAIN(ICOARS)
      CALL ADDRX1C(IAXALL(1,ICOARS))

      call rsl_from_child_info( i,j,pig,pjg,cm,cn,nig,njg,retval )
      do while (retval .eq. 1)
        p = (cn-1)*irax+cm
        if ( p .eq. 5 ) then
          valid_mask(i,j) = 1.0
          call rsl_from_child_msg( RWORDSIZE, tofollow )
          if ( tofollow .eq. 1.0 ) then
            call rsl_from_child_msg( RWORDSIZE, cross_mask(i,j) )
            call rsl_from_child_msg( RWORDSIZE, nh_mask(i,j) )

C NEW FOR R11
            IF (LFBTER(INEST)) THEN
              call rsl_from_child_msg( RWORDSIZE, ht(i,j) )
              call rsl_from_child_msg( RWORDSIZE, satbrt(i,j) )
              call rsl_from_child_msg( RWORDSIZE, xland(i,j) )
              call rsl_from_child_msg( RWORDSIZE, psa(i,j) )
              call rsl_from_child_msg( RWORDSIZE, psb(i,j) )
              call rsl_from_child_msg( RWORDSIZE, rpsa(i,j) )
              call rsl_from_child_msg( RWORDSIZE, rpsb(i,j) )
              call rsl_from_child_msg( RWORDSIZE, rpsc(i,j) )
              call rsl_from_child_msg( RWORDSIZE, ps0(i,j) )
              call rsl_from_child_msgx
     +          ( kl, RWORDSIZE, mixnh*mjxnh, t0(i,j,1) )
              call rsl_from_child_msgx
     +          ( kl, RWORDSIZE, mixnh*mjxnh, pr0(i,j,1) )
              call rsl_from_child_msgx
     +          ( kl, RWORDSIZE, mixnh*mjxnh, rho0(i,j,1) )
            ENDIF

            pval = psa(i,j)
            if (itgflg(inest).eq.1) then
              call rsl_from_child_msg( RWORDSIZE, tga(i,j) )
            endif
            call rsl_from_child_msgx
     +        ( kl, RWORDSIZE, mix*mjx, ta_sav(i,j,1,in) )
            do k = 1, kl
              ta_sav(i,j,k,in) = ta_sav(i,j,k,in) / pval
            enddo
            call rsl_from_child_msgx
     +        ( kl+1, RWORDSIZE, mix*mjx, wa_sav(i,j,1,in) )
            do k = 1, kl+1
              wa_sav(i,j,k,in) = wa_sav(i,j,k,in) / pval
            enddo
            call rsl_from_child_msgx
     +        ( kl, RWORDSIZE, mix*mjx, ppa_sav(i,j,1,in) )
            do k = 1, kl
              ppa_sav(i,j,k,in) = ppa_sav(i,j,k,in) / pval
            enddo
            if ( idry(inest) .eq. 0 ) then
              call rsl_from_child_msgx
     +          ( kl, RWORDSIZE, mix*mjx, qva_sav(i,j,1,in) )
              do k = 1, kl
                qva_sav(i,j,k,in) = qva_sav(i,j,k,in) / pval
              enddo
              if ( imoist(inest) .eq. 2 ) then
                call rsl_from_child_msgx
     +            ( kl, RWORDSIZE, mix*mjx, qca_sav(i,j,1,in) )
                do k = 1, kl
                  qca_sav(i,j,k,in) = qca_sav(i,j,k,in) / pval
                enddo
                call rsl_from_child_msgx
     +              ( kl, RWORDSIZE, mix*mjx, qra_sav(i,j,1,in) )
                do k = 1, kl
                  qra_sav(i,j,k,in) = qra_sav(i,j,k,in) / pval
                enddo
                if ( iice .eq. 1 ) then
                  call rsl_from_child_msgx
     +              ( kl, RWORDSIZE, mix*mjx, qia_sav(i,j,1,in) )
                  do k = 1, kl
                    qia_sav(i,j,k,in) = qia_sav(i,j,k,in) / pval
                  enddo
                  call rsl_from_child_msgx
     +              ( kl, RWORDSIZE, mix*mjx, qnia_sav(i,j,1,in) )
                  do k = 1, kl
                    qnia_sav(i,j,k,in) = qnia_sav(i,j,k,in) / pval
                  enddo
                  if ( iiceg .eq. 1 ) then
                    call rsl_from_child_msgx
     +                ( kl, RWORDSIZE, mix*mjx, qga_sav(i,j,1,in) )
                    do k = 1, kl
                      qga_sav(i,j,k,in) = qga_sav(i,j,k,in) / pval
                    enddo
                    call rsl_from_child_msgx
     +                ( kl, RWORDSIZE, mix*mjx, qnca_sav(i,j,1,in) )
                    do k = 1, kl
                      qnca_sav(i,j,k,in) = qnca_sav(i,j,k,in) / pval
                    enddo
                  endif
                endif
              endif
            endif
            if (inav.eq.1) then
              call rsl_from_child_msgx
     +          ( kl, RWORDSIZE, mixv*mjxv, turke(i,j,1) )
              call rsl_from_child_msgx
     +          ( kl, RWORDSIZE, mixv2*mjxv2, pthlsb(i,j,1) )
              call rsl_from_child_msgx
     +          ( kl, RWORDSIZE, mixv2*mjxv2, pqwsb(i,j,1) )
              call rsl_from_child_msgx
     +          ( kl, RWORDSIZE, mixv2*mjxv2, pthlqwb(i,j,1) )
            endif
C Add TKEA feedback
            if (igspbl.eq.1) then
              call rsl_from_child_msgx
     +           ( mlx, RWORDSIZE, mixg*mjxg, tkea(i,j,1) )
            endif
            if (isoil.eq.1) then
              call rsl_from_child_msgx
     +           ( mlx, RWORDSIZE, mixs*mjxs, tsla(i,j,1) )
            endif
            call rsl_from_child_msg( RWORDSIZE, rainc(i,j) )
            call rsl_from_child_msg( RWORDSIZE, rainnc(i,j) )
            if (imoiav(inest).ge.1) then
              call rsl_from_child_msg( RWORDSIZE, mavail(i,j) )
            endif
            if (ifsnow(inest).eq.2) then
              call rsl_from_child_msg( RWORDSIZE, snowc(i,j) )
            endif
            if (iexsi.ge.1) then
              call rsl_from_child_msg( RWORDSIZE, seaice(i,j) )
              call rsl_from_child_msg( RWORDSIZE, tgsi(i,j) )
            endif
            if (isoil.eq.2) then
              call rsl_from_child_msg( RWORDSIZE, snoa(i,j) )
              call rsl_from_child_msg( RWORDSIZE, snowha(i,j) )
              call rsl_from_child_msg( RWORDSIZE, cmca(i,j) )
              call rsl_from_child_msg( RWORDSIZE, snowc(i,j) )
              call rsl_from_child_msgx
     +           ( mlx, RWORDSIZE, mixl*mjxl, stca(i,j,1) )
              call rsl_from_child_msgx
     +           ( mlx, RWORDSIZE, mixl*mjxl, smca(i,j,1) )
              call rsl_from_child_msgx
     +           ( mlx, RWORDSIZE, mixl*mjxl, swca(i,j,1) )
            endif
          endif
        else if ( p .eq. 1 ) then ! nest point that provides dot fdbk
          call rsl_from_child_msg( RWORDSIZE, dt_mask(i,j) )
          call rsl_from_child_msg( RWORDSIZE, fudge_mask(i,j) )
          if ( dt_mask(i,j) .gt. 0. ) then
            call rsl_from_child_msg( 2 * mkx * RWORDSIZE, temp_val(1,1) )
            do k = 1, kl
              ua_sav(i,j,k,in) = temp_val(k,1)
              va_sav(i,j,k,in) = temp_val(k,2)
            enddo
          endif
        endif
        call rsl_from_child_info( i,j,pig,pjg,cm,cn,nig,njg,retval )
      enddo

      IF ( LFBTER( INEST ) ) THEN
        call rsl_exch_stencil( domains(icoars), sten_init(icoars) )
        CALL DOTS(PSA,PDOTA,IL,JL,MIX,MJX)                                         25MAY98.11
        FLIC_DO_N(j,1,jl)
          FLIC_DO_M(i,1,il)
            RPDOTA(I,J)=1./PDOTA(I,J)                                              25MAY98.14
            RPDOTB(I,J)=RPDOTA(I,J)                                                25MAY98.15
             PDOTB(I,J)= PDOTA(I,J)                                                25MAY98.16
          FLIC_ENDDO
        FLIC_ENDDO
        call rsl_exch_stencil( domains(icoars), sten_init(icoars) )
      ENDIF

#if 0
      glen(1) = il
      glen(2) = jl
      llen(1) = mix
      llen(2) = mjx
      call rsl_write(55,IO2D_IJ,
     +   nh_mask,domains(icoars),RSL_REAL,glen,llen)
      call rsl_write(55,IO2D_IJ,
     +   cross_mask,domains(icoars),RSL_REAL,glen,llen)
      call rsl_write(55,IO2D_IJ,
     +   dt_mask,domains(icoars),RSL_REAL,glen,llen)
#endif


C-------------------------------------- IFEED 3 --------------------------------
      IF (IFEED .EQ. 3 ) THEN


        XNU(1)=0.50
        XNU(2)=-0.52
        DO PASS = 1,2
          DO KP=1,2

define(IFEED3_SMOOTH_I,`
c$omp parallel do private(i,j)
      do j = 1, mjx
         do i = 1, mix
            temp_sav(i,j) = $1 _sav(i,j,$2,in)
         end do
      end do
c$omp parallel do private(i,j)
      FLIC_DO_N(j,1,jl)
      FLIC_DO_M(i,1,il)
        if ( $3(i,j) .eq. 1.0 ) then
          $1 _sav(i,j,$2,in) = temp_sav(i,j) + xnu(kp)*
     +  ((temp_sav(i,j-1)+temp_sav(i,j+1))*0.5-temp_sav(i,j))
        endif
      FLIC_ENDDO
      FLIC_ENDDO')
                 
define(IFEED3_SMOOTH_INH,`
c$omp parallel do private(i,j)
      do j = 1, mjx
         do i = 1, mix
            temp_sav(i,j) = $1 _sav(i,j,$2,in)
         end do
      end do
c$omp parallel do private(i,j)
      FLIC_DO_N(j,1,jl)
      FLIC_DO_M(i,1,il)
        if ( $3(i,j+1) .eq. 1.0  .or.
     +       $3(i+1,j) .eq. 1.0  .or.
     +       $3(i+1,j+1) .eq. 1.0  .or.
     +       $3(i,j) .eq. 1.0
     +) then
          $1 _sav(i,j,$2,in) = temp_sav(i,j) + xnu(kp)*
     +  ((temp_sav(i,j-1)+temp_sav(i,j+1))*0.5-temp_sav(i,j))
        endif
      FLIC_ENDDO
      FLIC_ENDDO')

define(IFEED3_SMOOTH_I1,`
c$omp parallel do private(i,j)
      do j = 1, mjx
         do i = 1, mix
            temp_sav(i,j) = $1(i,j,$2)
         end do
      end do
c$omp parallel do private(i,j)
      FLIC_DO_M(i,1,il)
      FLIC_DO_N(j,1,jl)
        if ( $3(i,j) .eq. 1.0 ) then
          $1(i,j,$2) = temp_sav(i,j) + xnu(kp)*
     +  ((temp_sav(i,j-1)+temp_sav(i,j+1))*0.5-temp_sav(i,j))
        endif
      FLIC_ENDDO
      FLIC_ENDDO')

      call rsl_exch_stencil(domains(icoars),sten_smth_ew(icoars,inest))

            IFEED3_SMOOTH_INH(wa,mkx+1,valid_mask)
            do k = 1,mkx
              IFEED3_SMOOTH_INH(ppa,k,valid_mask)
              IFEED3_SMOOTH_INH(wa,k,valid_mask)
              IFEED3_SMOOTH_I(ta,k,cross_mask)
              if ( idry(icoars).eq.0 ) then
                IFEED3_SMOOTH_I(qva,k,cross_mask)
                if (imoist(icoars).eq.2) then
                  IFEED3_SMOOTH_I(qca,k,cross_mask)
                  IFEED3_SMOOTH_I(qra,k,cross_mask)
                  if ( iice .eq. 1 ) then
                    IFEED3_SMOOTH_I(qia,k,cross_mask)
                    IFEED3_SMOOTH_I(qnia,k,cross_mask)
                    if ( iiceg .eq. 1 ) then
                      IFEED3_SMOOTH_I(qga,k,cross_mask)
                      IFEED3_SMOOTH_I(qnca,k,cross_mask)
                    endif
                  endif
                endif
              endif

              IFEED3_SMOOTH_I(ua,k,dt_mask)
              IFEED3_SMOOTH_I(va,k,dt_mask)
              if (inav.eq.1) then
                IFEED3_SMOOTH_I1(turke,k,cross_mask)
                IFEED3_SMOOTH_I1(pthlsb,k,cross_mask)
                IFEED3_SMOOTH_I1(pqwsb,k,cross_mask)
                IFEED3_SMOOTH_I1(pthlqwb,k,cross_mask)
              endif
C Add TKEA feedback
              if (igspbl.eq.1) then
                IFEED3_SMOOTH_I1(tkea,k,cross_mask)
              endif
            enddo

define(IFEED3_SMOOTH_J,`
c$omp parallel do private(i,j)
      do j = 1, mjx
         do i = 1, mix
            temp_sav(i,j) = $1 _sav(i,j,$2,in)
         end do
      end do
c$omp parallel do private(i,j)
      FLIC_DO_N(j,1,jl)
      FLIC_DO_M(i,1,il)
        if ( $3(i,j) .eq. 1.0 ) then
          $1 _sav(i,j,$2,in) = temp_sav(i,j) + xnu(kp)*
     +  ((temp_sav(i-1,j)+temp_sav(i+1,j))*0.5-temp_sav(i,j))
        endif
      FLIC_ENDDO
      FLIC_ENDDO')
                 
define(IFEED3_SMOOTH_JNH,`
c$omp parallel do private(i,j)
      do j = 1, mjx
         do i = 1, mix
            temp_sav(i,j) = $1 _sav(i,j,$2,in)
         end do
      end do
c$omp parallel do private(i,j)
      FLIC_DO_N(j,1,jl)
      FLIC_DO_M(i,1,il)
        if ( $3(i,j+1) .eq. 1.0  .or.
     +       $3(i+1,j) .eq. 1.0  .or.
     +       $3(i+1,j+1) .eq. 1.0  .or.
     +       $3(i,j) .eq. 1.0
     +) then
          $1 _sav(i,j,$2,in) = temp_sav(i,j) + xnu(kp)*
     +  ((temp_sav(i-1,j)+temp_sav(i+1,j))*0.5-temp_sav(i,j))
        endif
      FLIC_ENDDO
      FLIC_ENDDO')
                 
define(IFEED3_SMOOTH_J1,`
c$omp parallel do private(i,j)
      do j = 1, mjx
         do i = 1, mix
            temp_sav(i,j) = $1(i,j,$2)
         end do
      end do
c$omp parallel do private(i,j)
      FLIC_DO_N(j,1,jl)
      FLIC_DO_M(i,1,il)
        if ( $3(i,j) .eq. 1.0 ) then
          $1(i,j,$2) = temp_sav(i,j) + xnu(kp)*
     +  ((temp_sav(i-1,j)+temp_sav(i+1,j))*0.5-temp_sav(i,j))
        endif
      FLIC_ENDDO
      FLIC_ENDDO')

      call rsl_exch_stencil(domains(icoars),sten_smth_ew(icoars,inest))

            IFEED3_SMOOTH_JNH(wa,mkx+1,valid_mask)
            do k = 1,mkx
              IFEED3_SMOOTH_JNH(ppa,k,valid_mask)
              IFEED3_SMOOTH_JNH(wa,k,valid_mask)
              IFEED3_SMOOTH_J(ta,k,cross_mask)
              if ( idry(icoars).eq.0 ) then
                IFEED3_SMOOTH_J(qva,k,cross_mask)
                if (imoist(icoars).eq.2) then
                  IFEED3_SMOOTH_J(qca,k,cross_mask)
                  IFEED3_SMOOTH_J(qra,k,cross_mask)
                  if ( iice .eq. 1 ) then
                    IFEED3_SMOOTH_J(qia,k,cross_mask)
                    IFEED3_SMOOTH_J(qnia,k,cross_mask)
                    if ( iiceg .eq. 1 ) then
                      IFEED3_SMOOTH_J(qga,k,cross_mask)
                      IFEED3_SMOOTH_J(qnca,k,cross_mask)
                    endif
                  endif
                endif
              endif
              IFEED3_SMOOTH_J(ua,k,dt_mask)
              IFEED3_SMOOTH_J(va,k,dt_mask)
              if (inav.eq.1) then
                IFEED3_SMOOTH_J1(turke,k,cross_mask)
                IFEED3_SMOOTH_J1(pthlsb,k,cross_mask)
                IFEED3_SMOOTH_J1(pqwsb,k,cross_mask)
                IFEED3_SMOOTH_J1(pthlqwb,k,cross_mask)
              endif
C Add TKEA feedback
              if (igspbl.eq.1) then
                IFEED3_SMOOTH_J1(tkea,k,cross_mask)
              endif
            enddo

          ENDDO
        ENDDO

        FLIC_DO_N(j,1,jl)
          FLIC_DO_M(i,1,il)
            if ( cross_mask(i,j) .eq. 1.0 ) then
            wa(i,j,mkx+1) = wa_sav(i,j,mkx+1,in)
            do k = 1,mkx
              ppa(i,j,k) = ppa_sav(i,j,k,in)
              wa (i,j,k) = wa_sav (i,j,k,in)
              ta (i,j,k) = ta_sav (i,j,k,in)
              if ( idry(icoars).eq.0 ) then
                qva (i,j,k) = qva_sav (i,j,k,in)
                if (imoist(icoars).eq.2) then
                  qca (i,j,k) = qca_sav (i,j,k,in)
                  qra (i,j,k) = qra_sav (i,j,k,in)
                  if ( iice .eq. 1 ) then
                    qia (i,j,k) = qia_sav (i,j,k,in)
                    qnia (i,j,k) = qnia_sav (i,j,k,in)
                    if ( iiceg .eq. 1 ) then
                      qga (i,j,k) = qga_sav (i,j,k,in)
                      qnca (i,j,k) = qnca_sav (i,j,k,in)
                    endif
                  endif
                endif
              endif
            enddo
            endif
            if ( dt_mask(i,j) .eq. 1.0 ) then
            do k = 1,mkx
              ua (i,j,k) = ua_sav (i,j,k,in)
              va (i,j,k) = va_sav (i,j,k,in)
            enddo
            endif
C Add TKEA feedback
            if (igspbl.eq.1) then
            do k = 1,mkx
               IF(TKEA(I,J,K).LT.1.E-06)TKEA(I,J,K)=1.E-06
            enddo
            endif
          FLIC_ENDDO
        FLIC_ENDDO

      ENDIF
C END OF SMOOTHER FOR IFEED=3

C-------------------------------------- IFEED 4 --------------------------------

      IF ( IFEED .EQ. 4 ) THEN
      call rsl_exch_stencil(domains(icoars),sten_smth_ew(icoars,inest))

define(SMTHEW3,`(.25*($1_sav($2,$3-1,$4,in)+
     + $1_sav($2,$3,$4,in)*2.+
     + $1_sav($2,$3+1,$4,in)))')

define(SMTHEW3A,`(.25*($1($2,$3-1,$4)+
     + $1($2,$3,$4)*2.+
     + $1($2,$3+1,$4)))')

      FLIC_DO_N(j,1,jl)
        FLIC_DO_M(i,1,il)
          if ( valid_mask(i,j) .eq. 1.0 ) then
            w3d(i,j,mkx+1) =  SMTHEW3(wa,i,j,(mkx+1))
          endif
        FLIC_ENDDO
      FLIC_ENDDO
      do k = 1,mkx
        FLIC_DO_N(j,1,jl)
          FLIC_DO_M(i,1,il)
            if ( valid_mask(i,j) .eq. 1.0 ) then
              pp3d(i,j,k) =  SMTHEW3(ppa,i,j,k)
              w3d(i,j,k) =  SMTHEW3(wa,i,j,k)
              t3d(i,j,k) = SMTHEW3(ta,i,j,k)
              if ( idry(icoars).eq.0 ) then
                qv3d(i,j,k) = SMTHEW3(qva,i,j,k)
                if (imoist(icoars).eq.2) then
                  qc3d(i,j,k) = SMTHEW3(qca,i,j,k)
                  qr3d(i,j,k) = SMTHEW3(qra,i,j,k)
                  if ( iice .eq. 1 ) then
                    qi3d(i,j,k) = SMTHEW3(qia,i,j,k)
                    qni3d(i,j,k) = SMTHEW3(qnia,i,j,k)
                    if ( iiceg .eq. 1 ) then
                      qg3d(i,j,k) = SMTHEW3(qga,i,j,k)
                      qnc3d(i,j,k) = SMTHEW3(qnca,i,j,k)
                    endif
                  endif
                endif
              endif
              u3d(i,j,k) = SMTHEW3(ua,i,j,k)
              v3d(i,j,k) = SMTHEW3(va,i,j,k)
              if (inav.eq.1) then
                turke_sav(i,j,k) = SMTHEW3A(turke,i,j,k)
                pthlsb_sav(i,j,k) = SMTHEW3A(pthlsb,i,j,k)
                pqwsb_sav(i,j,k) = SMTHEW3A(pqwsb,i,j,k)
                pthlqwb_sav(i,j,k) = SMTHEW3A(pthlqwb,i,j,k)
              endif
C Add TKEA feedback
              if (igspbl.eq.1)then
                tkea_sav(i,j,k) = SMTHEW3A(tkea,i,j,k)
              endif
            endif
          FLIC_ENDDO
        FLIC_ENDDO
      enddo

      call rsl_exch_stencil( domains(icoars), sten_smth_ns(icoars) )

define(SMTHNS3,`(.25*($1($2-1,$3,$4)+
     +  2.*$1($2,$3,$4)+
     +  $1($2+1,$3,$4)))')
define(SMTHNS3A,`(.25*($1_sav($2-1,$3,$4)+
     +  2.*$1_sav($2,$3,$4)+
     +  $1_sav($2+1,$3,$4)))')

      FLIC_DO_N(j,1,jl)
        FLIC_DO_M(i,1,il)
          if ( cross_mask(i,j) .eq. 1.0 ) then
            wa(i,j,mkx+1) =  SMTHNS3(w3d,i,j,(mkx+1))
          endif
        FLIC_ENDDO
      FLIC_ENDDO
      do k = 1,mkx
        FLIC_DO_N(j,1,jl)
          FLIC_DO_M(i,1,il)
            if ( cross_mask(i,j) .eq. 1.0 ) then
              ppa(i,j,k) =  SMTHNS3(pp3d,i,j,k)
              wa(i,j,k) =  SMTHNS3(w3d,i,j,k)
              ta(i,j,k) = SMTHNS3(t3d,i,j,k)
              if ( idry(icoars).eq.0 ) then
                qva(i,j,k) = SMTHNS3(qv3d,i,j,k)
                if (imoist(icoars).eq.2) then
                  qca(i,j,k) = SMTHNS3(qc3d,i,j,k)
                  qra(i,j,k) = SMTHNS3(qr3d,i,j,k)
                  if ( iice .eq. 1 ) then
                    qia(i,j,k) = SMTHNS3(qi3d,i,j,k)
                    qnia(i,j,k) = SMTHNS3(qni3d,i,j,k)
                    if ( iiceg .eq. 1 ) then
                      qga(i,j,k) = SMTHNS3(qg3d,i,j,k)
                      qnca(i,j,k) = SMTHNS3(qnc3d,i,j,k)
                    endif
                  endif
                endif
              endif
              if (inav.eq.1) then
                turke(i,j,k) = SMTHNS3A(turke,i,j,k)
                pthlsb(i,j,k) = SMTHNS3A(pthlsb,i,j,k)
                pqwsb(i,j,k) = SMTHNS3A(pqwsb,i,j,k)
                pthlqwb(i,j,k) = SMTHNS3A(pthlqwb,i,j,k)
              endif
C Add TKEA feedback
              if (igspbl.eq.1) then
                tkea(i,j,k) = SMTHNS3A(tkea,i,j,k)
              endif
            endif
          FLIC_ENDDO
        FLIC_ENDDO
      enddo
      do k = 1,mkx
        FLIC_DO_N(j,1,jl)
          FLIC_DO_M(i,1,il)
            if ( dt_mask(i,j) .eq. 1.0 ) then
              ua(i,j,k) = SMTHNS3(u3d,i,j,k)
              va(i,j,k) = SMTHNS3(v3d,i,j,k)
            endif
          FLIC_ENDDO
        FLIC_ENDDO
      enddo
      ENDIF  
C END OF SMOOTHER FOR IFEED=4
      

      FLIC_DO_N(j,1,jl)
        FLIC_DO_M(i,1,il)
          if ( cross_mask(i,j) .eq. 1.0 ) then
            wa(i,j,mkx+1) = wa(i,j,mkx+1) * psa(i,j)
          endif
        FLIC_ENDDO
      FLIC_ENDDO
      do k = 1,mkx
        FLIC_DO_N(j,1,jl)
          FLIC_DO_M(i,1,il)
            if ( cross_mask(i,j) .eq. 1.0 ) then
              ppa(i,j,k) =  ppa(i,j,k) * psa(i,j)
              wa(i,j,k) =  wa(i,j,k) * psa(i,j)
              ta(i,j,k) = ta(i,j,k) * psa(i,j)
              if ( idry(icoars).eq.0 ) then
                if (qva(i,j,k).le.0.) qva(i,j,k) = 1.E-14
                qva(i,j,k) =  qva(i,j,k) * psa(i,j)
                if (imoist(icoars).eq.2) then
                  if (qca(i,j,k).lt.0.) qca(i,j,k) = 0.
                  qca(i,j,k) = qca(i,j,k) * psa(i,j)
                  if (qra(i,j,k).lt.0.) qra(i,j,k) = 0.
                  qra(i,j,k) = qra(i,j,k) * psa(i,j)
                  if ( iice .eq. 1 ) then
                    if (qia(i,j,k).lt.0.) qia(i,j,k) = 0.
                    qia(i,j,k) = qia(i,j,k) * psa(i,j)
                    if (qnia(i,j,k).lt.0.) qnia(i,j,k) = 0.
                    qnia(i,j,k) = qnia(i,j,k) * psa(i,j)
                    if ( iiceg .eq. 1 ) then
                      if (qga(i,j,k).lt.0.) qga(i,j,k) = 0.
                      qga(i,j,k) = qga(i,j,k) * psa(i,j)
                      if (qnca(i,j,k).lt.0.) qnca(i,j,k) = 0.
                      qnca(i,j,k) = qnca(i,j,k) * psa(i,j)
                    endif
                  endif
                endif
              endif
              if (inav.eq.1) then
                IF(TURKE(I,J,K).LT.1.E-06)TURKE(I,J,K)=1.E-06
              endif
            endif
          FLIC_ENDDO
        FLIC_ENDDO
      enddo
      do k = 1,kl
        FLIC_DO_N(j,1,jl)
          FLIC_DO_M(i,1,il)
C           this is only for bit repro with non-parallel code
            if ( fudge_mask(i,j) .eq. 1.0) then
              ua(i,j,k) = ua(i,j,k) * rpdota(i,j)
              ua(i,j,k) = ua(i,j,k) * pdota(i,j)
              va(i,j,k) = va(i,j,k) * rpdota(i,j)
              va(i,j,k) = va(i,j,k) * pdota(i,j)
            endif
            if ( dt_mask(i,j) .eq. 1.0 ) then
              ua(i,j,k) = ua(i,j,k) * pdota(i,j)
              va(i,j,k) = va(i,j,k) * pdota(i,j)
            endif
          FLIC_ENDDO
        FLIC_ENDDO
      enddo

C ADDED FOR R11
      LFBTER(INEST) = .FALSE.
      LFBTER(ICOARS) = .TRUE.
C

      return
      end


