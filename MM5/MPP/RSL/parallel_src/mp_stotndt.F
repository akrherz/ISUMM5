C DISTRIBUTED MEMORY VERSION  OF STOTNDI
C DO NOT USE FLIC ON THIS ROUTINE
C
C M4 packing macros
C
define(PACK2D,
      `do ii = -IOR,IOR
        do jj = -IOR,IOR
          $1 _var (ii,jj) = $1 (i+ii,j+jj)
        enddo
      enddo'
      )
define(PACK3D,
      `dokk=1,$3
      do ii = -IOR,IOR
      do jj = -IOR,IOR
        $1 _var (kk,ii,jj) = $1 (i+ii,j+jj,kk)* $2(i+ii,j+jj)
      enddo
      enddo
      enddo
      dokk=1,$3
      do ii = -IOR,IOR
      do jj = -IOR,IOR
        $1 _sav_var(kk,ii,jj) = $1 _sav(i+ii,j+jj,kk,ic)*
     +   $2 (i+ii,j+jj)
      enddo
      enddo
      enddo'
      )

define(SINT2D,
      `call sintF( $1 _var(-IOR,-IOR),x,p,
     +           ax1i,ax2i,ax3i,ax4i,ax1j,ax2j,ax3j,ax4j)
      callrsl_to_child_msg(RWORDSIZE,x)'
      )

define(SINT3D,
      `call sintFv( $1 _var,xv,$2,$2,p )
      callrsl_to_child_msg(($2)*RWORDSIZE,xv)
      call sintFv( $1 _sav_var,xv,$2,$2,p )
      callrsl_to_child_msg(($2)*RWORDSIZE,xv)'
      )

C
C M4 unpack macro definitions
C
define(SB_M4,
      `if ( ig.eq. $3 ) then
        do k = 1, $2
          $1 sbt( j,k, $3 ) = tend(k)
        enddo
        do k = 1, $2
          $1 sb( j,k, $3 ) = $1 a (i,j,k)
        enddo
      endif'
       )
define(NB_M4,
      `if ( ig.eq. il-$3 ) then
        do k = 1, $2
          $1 nbt( j,k, $3 ) = tend(k)
        enddo
        do k = 1, $2
          $1 nb( j,k, $3 ) = $1 a (i,j,k)
        enddo
      endif'
       )
define(WB_M4,
      `if ( jg.eq. $3 ) then
        do k = 1, $2
          $1 wbt( i,k, $3 ) = tend(k)
        enddo
        do k = 1, $2
          $1 wb( i,k, $3 ) = $1 a (i,j,k)
        enddo
      endif'
       )
define(EB_M4,
      `if ( jg.eq. jl-$3 ) then
        do k = 1, $2
          $1 ebt( i,k, $3 ) = tend(k)
        enddo
        do k = 1, $2
          $1 eb( i,k, $3 ) = $1 a (i,j,k)
        enddo
      endif'
       )

define(UNPACK,
      `call rsl_from_parent_msg(($2)*RWORDSIZE,var)
      call rsl_from_parent_msg(($2)*RWORDSIZE,var_sav)
      do k = 1, $2
        $1 a (i,j,k) = var_sav(k) * psa(i,j)
        var(k) = var(k) * psa(i,j)
        tend(k) = (var(k) - $1 a (i,j,k)) * rdtcrs
      enddo
      SB_M4( $1, $2, 1 )
      SB_M4( $1, $2, 2 )
      NB_M4( $1, $2, 1 )
      NB_M4( $1, $2, 2 )
      WB_M4( $1, $2, 1 )
      WB_M4( $1, $2, 2 )
      EB_M4( $1, $2, 1 )
      EB_M4( $1, $2, 2 )'
      )

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE STOTNDT (INEST,IYYN,JXXN,IYY,JXX)
C                                                                       
C---------------------------------------------------------------------- 
C                                                                       
#     include <rsl.inc>
#     include <rslcom.inc>
      FLIC_RUN_DECL
#     include <parame.incl>                                             
#     include <addr0.incl>                                              
#     include <param2.incl>                                             
#     include <param3.incl>                                             
#     include <varia.incl>                                              
#     include <point3d.incl>                                            
#     include <point2d.incl>                                            
#     include <nonhyd.incl>                                             
#     include <various.incl>                                            
#     include <nestl.incl>                                              
c#     include <point3dn.incl>                                           
c#     include <point2dn.incl>                                           
c#     include <pointbcn.incl>                                           
#     include <pointbc.incl>                                           
#     include <nnnhyd.incl>                                             
c#     include <nnnhydb.incl>                                            
#     include <variousn.incl>                                           
#     include <nonhydb.incl>                                            
#     include <pmoist.incl>                                             
#     include <pbltke.incl>
#     include <soilp.incl>
#define FULL_DECLARE
#     include <mpp_dusolve3.incl>
      PARAMETER (ONE12=1./12.,ONE24=1./24.)
      logical new_cd_point, new_cd_point2
      integer i,j,pig,pjg,cm,cn,nig,njg,retval
      integer p
      integer msize
      real ax,ax1i,ax2i,ax3i,ax4i,ax1j,ax2j,ax3j,ax4j
      real dtcrs
      real rdtcrs
      COMMON /DEPAR2/ XIG(9),XJG(9),IG0(MJX,MIX),JG0(MJX,MIX)
#include <packdown.incl>

      real v( -IOR:IOR, -IOR:IOR )
      real x
      real xv( mkx+1 ), xv2(mlx)
      real var(mkx+1), var_sav(mkx+1), tend(mkx+1), tend2

C
      IN=INEST
      ICOARS = NUMNC(INEST)
      IC=ICOARS
      CALL MP_INITDOMAIN(ICOARS)
      CALL ADDRX1C(IAXALL(1,ICOARS))
      dtcrs = .5*dt
      rdtcrs = 1./dtcrs
      CALL ADDRX1N(IAXALL(1,INEST))

      call DOTS(psa,hscr1,IL,JL,MIX,MJX)
      FLIC_DO_N(j,1,jl)
        FLIC_DO_M(i,1,il)
          hscr3(i,j) = 1./hscr1(i,j)
          hscr2(i,j) = 1./psa(i,j)
        FLIC_ENDDO
      FLIC_ENDDO

      call rsl_exch_stencil( domains(icoars), sten_sint(icoars))

c
c determine size of message
c
      call bcast_size( 1, msize )
c
c seed packing loop
c
      pig_prev = -1
      pjg_prev = -1
      pig_prev2 = -1
      pjg_prev2 = -1
      call rsl_to_child_info( domains(icoars), domains(inest),
     +                        msize*RWORDSIZE,
     +                        i,j,pig,pjg,cm,cn,nig,njg,retval )
c
c main packing loop
c
      do while ( retval .eq. 1 )
        p = (cm-1)*irax+cn

C note that these aren't used in the new version of the parallel sint
C dropped to maintain bit reproducibility w/ serial v2 version.
#if 0
        AX = XIG(P)
        AX1I=AX*ONE12
        AX2I=AX*AX*ONE24
        AX3I=AX*AX*AX*ONE12
        AX4I=AX*AX*AX*AX*ONE24
        AX = XJG(P)
        AX1J=AX*ONE12
        AX2J=AX*AX*ONE24
        AX3J=AX*AX*AX*ONE12
        AX4J=AX*AX*AX*AX*ONE24
#endif

c
c--- first the cross variables
c
        if (((nig .ge. 1      .and. nig .le. 2  ) .or.
     $       (nig .ge. inl-2  .and. nig .le. inl-1 ) .or.
     $       (njg .ge. 1      .and. njg .le. 2  ) .or.
     $       (njg .ge. jnl-2  .and. njg .le. jnl-1 )) .and.
     $       (nig .ge. 1 .and. nig .le. inl-1 ) .and.
     $       (njg .ge. 1 .and. njg .le. jnl-1 )) then ! nested cross bou

          if(pig_prev.eq.pig .and. pjg_prev.eq.pjg) then
            new_cd_point = .false.
          else
            new_cd_point = .true.
          endif
          pig_prev = pig
          pjg_prev = pjg
c
c--- do 3-d cross fields, decouple with hscr2 (inv of psa)
c
          if(new_cd_point) then
            PACK3D(ta,hscr2,mkx)
            if(idry(inest).eq.0)then
              PACK3D(qva,hscr2,mkx)
              if(imoist(inest).eq.2)then
                PACK3D(qca,hscr2,mkx)
                PACK3D(qra,hscr2,mkx)
                if(iice.eq.1)then
                  PACK3D(qia,hscr2,mkx)
                  PACK3D(qnia,hscr2,mkx)
                  if(iiceg.eq.1)then
                    PACK3D(qga,hscr2,mkx)
                    PACK3D(qnca,hscr2,mkx)
                  endif
                endif
              endif
            endif
            PACK3D(ppa,hscr2,mkx)
            PACK3D(wa,hscr2,mkx+1)
            if ( ibltyp( icoars ) .eq. 6 ) then
      dokk=1,mkx+1
      do ii = -IOR,IOR
      do jj = -IOR,IOR
        tkea_sav_var(kk,ii,jj) = tkea_sav(i+ii,j+jj,kk)
      enddo
      enddo
      enddo
            endif
            PACK2D(rainc_sav)
            PACK2D(rainnc_sav)
C 000901
            IF(IMOIAV(inest).EQ.2)THEN
              PACK2D(mavail_sav)
            ENDIF
C 20001024
            IF(ISOIL.EQ.2 .OR. ISOIL.EQ.3) THEN
      do kk=1,mlx
      do ii = -IOR,IOR
      do jj = -IOR,IOR
        stca_sav_var(kk,ii,jj) = stca_sav(i+ii,j+jj,kk)
        smca_sav_var(kk,ii,jj) = smca_sav(i+ii,j+jj,kk)
        swca_sav_var(kk,ii,jj) = swca_sav(i+ii,j+jj,kk)
      enddo
      enddo
      enddo
            ENDIF

          endif        ! new_cd_point


          SINT3D(ta,mkx)
          if(idry(inest).eq.0)then
            SINT3D(qva,mkx)
            if(imoist(inest).eq.2)then
              SINT3D(qca,mkx)
              SINT3D(qra,mkx)
              if(iice.eq.1)then
                SINT3D(qia,mkx)
                SINT3D(qnia,mkx)
                if(iiceg.eq.1)then
                  SINT3D(qga,mkx)
                  SINT3D(qnca,mkx)
                endif
              endif
            endif
          endif
          SINT3D(ppa,mkx)
          SINT3D(wa,mkx+1)
          if ( ibltyp( icoars ) .eq. 6 ) then
      call sintFv( tkea_sav_var,xv,mkx+1,mkx+1,p )
      callrsl_to_child_msg((mkx+1)*RWORDSIZE,xv)
          endif

          SINT2D(rainc_sav)
          SINT2D(rainnc_sav)
C 000901
          IF(IMOIAV(inest).EQ.2)THEN
            SINT2D(mavail_sav)
          ENDIF
C 20001024
          IF(ISOIL.EQ.2 .OR. ISOIL.EQ.3) THEN
      call sintFv( stca_sav_var,xv2,mlx,mlx,p)
      call rsl_to_child_msg((mlx)*RWORDSIZE,xv2)
      call sintFv( smca_sav_var,xv2,mlx,mlx,p)
      call rsl_to_child_msg((mlx)*RWORDSIZE,xv2)
      call sintFv( swca_sav_var,xv2,mlx,mlx,p)
      call rsl_to_child_msg((mlx)*RWORDSIZE,xv2)
          ENDIF
        endif                     ! cross boundary

c
c Do the dot variables, but decouple with hscr1
c

c NOTE: THE NESTED COORDINATES ARE CURRENTLY OFF BY ONE (ONE HIGHER)
c BECAUSE OF THE STAGGERED GRID.   THIS WILL BE FIXED IN A SEPARATE STEP
c (MOVEDOT_C).  FOR NOW, WE JUST USE THE "WRONG" INDEXES FOR THE CROSS G

        if(((nig .ge. 2    .and. nig .le. 3  ) .or.
     $    (nig .ge. inl    .and. nig .le. inl+1 ) .or.
     $    (njg .ge. 2      .and. njg .le. 3  ) .or.
     $    (njg .ge. jnl    .and. njg .le. jnl+1 )) .and.
     $    (nig .ge. 2 .and. nig .le. inl+1 ) .and.
     $    (njg .ge. 2 .and. njg .le. jnl+1 )) then   ! nested dot bounda

          if(pig_prev2.eq.pig .and. pjg_prev2.eq.pjg) then
            new_cd_point2 = .false.
          else
            new_cd_point2 = .true.
          endif
          pig_prev2 = pig
          pjg_prev2 = pjg

          if(new_cd_point2) then
#if 0
            PACK3D(ua,hscr1,mkx)
            PACK3D(va,hscr1,mkx)
#else
C this is for bit for bit agreement w/ serial code.  In the case
C of the old time level, pressure is divided out.  In the case
C of the new time livel, it is multiplied by the inverse.
      dokk=1,mkx
      do ii = -2,2
      do jj = -2,2
        ua _var (kk,ii,jj) = ua (i+ii,j+jj,kk)* hscr3(i+ii,j+jj)
      enddo
      enddo
      enddo
      dokk=1,mkx
      do ii = -2,2
      do jj = -2,2
        ua _sav_var(kk,ii,jj) = ua _sav(i+ii,j+jj,kk,ic)/
     +   hscr1 (i+ii,j+jj)
      enddo
      enddo
      enddo

      dokk=1,mkx
      do ii = -2,2
      do jj = -2,2
        va _var (kk,ii,jj) = va (i+ii,j+jj,kk)* hscr3(i+ii,j+jj)
      enddo
      enddo
      enddo
      dokk=1,mkx
      do ii = -2,2
      do jj = -2,2
        va _sav_var(kk,ii,jj) = va _sav(i+ii,j+jj,kk,ic)/
     +   hscr1 (i+ii,j+jj)
      enddo
      enddo
      enddo
#endif
          endif                  ! new_cd_point
          SINT3D(ua,mkx)
          SINT3D(va,mkx)

        endif                     ! dot boundary
        call rsl_to_child_info( domains(icoars), domains(inest),
     +                          msize*RWORDSIZE,
     +                          i,j,pig,pjg,cm,cn,nig,njg,retval )
      enddo

      call rsl_bcast_msgs

      CALL ADDRX1C(IAXALL(1,INEST))

      call rsl_from_parent_info(i,j,nig,njg,cm,cn,pig,pjg,retval)
      do while ( retval .eq. 1 )
        ig = nig
        jg = njg

        if (((ig .ge. 1     .and. ig .le. 2  ) .or.
     $       (ig .ge. il-2  .and. ig .le. il-1 ) .or.
     $       (jg .ge. 1     .and. jg .le. 2  ) .or.
     $       (jg .ge. jl-2  .and. jg .le. jl-1 ))
     $       .and. (ig.ge.1.and.ig.le.(il-1))
     $       .and. (jg.ge.1.and.jg.le.(jl-1))) then

          UNPACK(t,mkx)
          if(idry(inest).eq.0)then
            UNPACK(qv,mkx)
            if(imoist(inest).eq.2)then
              UNPACK(qc,mkx)
              UNPACK(qr,mkx)
              if (iice.eq.1)then
                UNPACK(qi,mkx)
                UNPACK(qni,mkx)
                if(iiceg.eq.1)then
                  UNPACK(qg,mkx)
                  UNPACK(qnc,mkx)
                endif
              endif
            endif
          endif
          UNPACK(pp,mkx)
          UNPACK(w,(mkx+1))
          if ( ibltyp( icoars ) .eq. 6 ) then
      call rsl_from_parent_msg((mkx+1)*RWORDSIZE,var_sav)
      do k = 1, mkx+1
        tkea (i,j,k) = var_sav(k)
      enddo
          endif

          call rsl_from_parent_msg(RWORDSIZE,rainc(i,j))
          call rsl_from_parent_msg(RWORDSIZE,rainnc(i,j))
C 000901
          IF(IMOIAV(inest).EQ.2)THEN
            call rsl_from_parent_msg(RWORDSIZE,mavail(i,j))
          ENDIF

C 20001024
          IF(ISOIL.EQ.2 .OR. ISOIL.EQ.3) THEN

          call rsl_from_parent_msg((mlx)*RWORDSIZE,var_sav)
          do k = 1, mlx
            stca (i,j,k) = var_sav(k)
            stcb (i,j,k) = var_sav(k)
          enddo
          call rsl_from_parent_msg((mlx)*RWORDSIZE,var_sav)
          do k = 1, mlx
            smca (i,j,k) = var_sav(k)
            smcb (i,j,k) = var_sav(k)
          enddo
          call rsl_from_parent_msg((mlx)*RWORDSIZE,var_sav)
          do k = 1, mlx
            swca (i,j,k) = var_sav(k)
            swcb (i,j,k) = var_sav(k)
          enddo
          ENDIF

        endif

        if (((ig .ge. 2   .and. ig .le. 3  ) .or.
     $       (ig .ge. il  .and. ig .le. il+1 ) .or.
     $       (jg .ge. 2   .and. jg .le. 3  ) .or.
     $       (jg .ge. jl  .and. jg .le. jl+1 ))
     $       .and. (ig.ge.2 .and. ig .le. il+1 )
     $       .and. (jg.ge.2 .and. jg .le. jl+1 )) then
C this next bit will not work if this file is run through FLIC -- you
C would have to protect the indices with NOFLIC
          do k=1,MKX
            call rsl_from_parent_msg( RWORDSIZE, ucd(i,j,k) )
          enddo
          do k=1,MKX
            call rsl_from_parent_msg( RWORDSIZE, ucc(i,j,k) )
          enddo
          do k=1,MKX
            call rsl_from_parent_msg( RWORDSIZE, vcd(i,j,k) )
          enddo
          do k=1,MKX
            call rsl_from_parent_msg( RWORDSIZE, vcc(i,j,k) )
          enddo
        endif
        call rsl_from_parent_info(i,j,nig,njg,cm,cn,pig,pjg,retval)
      enddo

C the rest of this happens on the nest

      CALL MP_INITDOMAIN(INEST)

      call rsl_exch_stencil( domains(inest), sten_movedot_c(inest) )

      do k = 1, mkx
        FLIC_DO_N(j,1,jl)
          jg = FLIC_L2G_N(j)
          FLIC_DO_M(i,1,il)
            ig = FLIC_L2G_M(i)
            if ( (ig .ge. 1    .and. ig .le. 2  ) .or.
     +           (ig .ge. il-1 .and. ig .le. il ) .or.
     +           (jg .ge. 1    .and. jg .le. 2  ) .or.
     +           (jg .ge. jl-1 .and. jg .le. jl ) ) then
            ucd(i,j,k) = ucd(i+1,j+1,k)
            vcd(i,j,k) = vcd(i+1,j+1,k)
            ucc(i,j,k) = ucc(i+1,j+1,k)
            vcc(i,j,k) = vcc(i+1,j+1,k)
            endif
          FLIC_ENDDO
        FLIC_ENDDO
      enddo

      call DOTS(psa,hscr1,IL,JL,MIX,MJX)

      do k = 1, mkx
        FLIC_DO_N(j,1,jl)
          jg = FLIC_L2G_N(j)
          FLIC_DO_M(i,1,il)
            ig = FLIC_L2G_M(i)
            if ( (ig .ge. 1    .and. ig .le. 2  ) .or.
     +           (ig .ge. il-1 .and. ig .le. il ) .or.
     +           (jg .ge. 1    .and. jg .le. 2  ) .or.
     +           (jg .ge. jl-1 .and. jg .le. jl ) ) then
              uc1 = ucd(i,j,k)
              vc1 = vcd(i,j,k)
              ua(i,j,k) = ucc(i,j,k) * hscr1(i,j)
              va(i,j,k) = vcc(i,j,k) * hscr1(i,j)
              tend2 = (uc1*hscr1(i,j)-ua(i,j,k))*rdtcrs
              if ( ig.eq.(1)) then
                usbt(j,k,1) = tend2
                usb(j,k,1) = ua(i,j,k)
              endif
              if ( ig.eq.(2)) then
                usbt(j,k,2) = tend2
                usb(j,k,2) = ua(i,j,k)
              endif
              if ( ig.eq.(il-1)) then
                unbt(j,k,2) = tend2
                unb(j,k,2) = ua(i,j,k)
              endif
              if ( ig.eq.(il)) then
                unbt(j,k,1) = tend2
                unb(j,k,1) = ua(i,j,k)
              endif
              if ( jg.eq.(1)) then
                uwbt(i,k,1) = tend2
                uwb(i,k,1) = ua(i,j,k)
              endif
              if ( jg.eq.(2)) then
                uwbt(i,k,2) = tend2
                uwb(i,k,2) = ua(i,j,k)
              endif
              if ( jg.eq.(jl-1)) then
                uebt(i,k,2) = tend2
                ueb(i,k,2) = ua(i,j,k)
              endif
              if ( jg.eq.(jl)) then
                uebt(i,k,1) = tend2
                ueb(i,k,1) = ua(i,j,k)
              endif
              tend2 = (vc1*hscr1(i,j)-va(i,j,k))*rdtcrs
              if ( ig.eq.(1)) then
                vsbt(j,k,1) = tend2
                vsb(j,k,1) = va(i,j,k)
              endif
              if ( ig.eq.(2)) then
                vsbt(j,k,2) = tend2
                vsb(j,k,2) = va(i,j,k)
              endif
              if ( ig.eq.(il-1)) then
              vnbt(j,k,2) = tend2
                vnb(j,k,2) = va(i,j,k)
              endif
              if ( ig.eq.(il)) then
                vnbt(j,k,1) = tend2
                vnb(j,k,1) = va(i,j,k)
              endif
              if ( jg.eq.(1)) then
                vwbt(i,k,1) = tend2
                vwb(i,k,1) = va(i,j,k)
              endif
              if ( jg.eq.(2)) then
                vwbt(i,k,2) = tend2
                vwb(i,k,2) = va(i,j,k)
              endif
              if ( jg.eq.(jl-1)) then
                vebt(i,k,2) = tend2
                veb(i,k,2) = va(i,j,k)
              endif
              if ( jg.eq.(jl)) then
                vebt(i,k,1) = tend2
                veb(i,k,1) = va(i,j,k)
              endif
              endif
          FLIC_ENDDO
        FLIC_ENDDO
      enddo

      CALL MP_INITDOMAIN(ICOARS)
      CALL ADDRX1C(IAXALL(1,ICOARS))
      CALL ADDRX1N(IAXALL(1,INEST))

      return
      end

CCCCCCC STUB CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE STOTNDI (INEST,IYYN,JXXN,IYY,JXX)
#     include <rsl.inc>
#     include <rslcom.inc>
#     include <parame.incl>
      ICOARS = NUMNC(INEST)
      CALL MP_INITDOMAIN(ICOARS)
      CALL MP_INIT_SAVVARS(ICOARS)
      return
      end

      
#if 1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C hopefully faster version of sint used by parallel code
C modified to give bit repro with the V2 version of sint
C
C
      subroutine sintF(xinp,xf,ifm,ax1i,ax2i,ax3i,ax4i,
     +                             ax1j,ax2j,ax3j,ax4j)
#     include "rsl.inc"
#     include "parame.incl"
      parameter(nf=9,ifct=1,ior=IOR)
c                                                                       
c                                                                       
c                                                                       
      dimension y(-2:2),z(-2:2),
     1                   f(0:1)
      data  ep/ 1.e-10/
      parameter(nonos=1)
      PARAMETER(ONE12=1./12.,ONE24=1./24.)
      dimension  fl(0:1),xinp(-2:+2,-2:2)
      COMMON /DEPAR2/ XIG(9),XJG(9),IG0(MJX,MIX),JG0(MJX,MIX)
      real      mxm,mn
c                                                                       
      donor(y1,y2,ax)=(y1*amax1(0.,sign(1.,ax))-
     1                y2*amin1(0.,sign(1.,ax)))*ax
      tr2(y1,y2,ax)=ax*.5*(y1+y2)-ax**2*.5*(y2-y1)
c                                                                       
c                                                                       
      TR4(YM1,Y0,YP1,YP2,A)=A*ONE12*(7.*(YP1+Y0)-(YP2+YM1))
     1 -A*A*ONE24*(15.*(YP1-Y0)-(YP2-YM1))-A*A*A*ONE12*((YP1+Y0)
     2 -(YP2+YM1))+A*A*A*A*ONE24*(3.*(YP1-Y0)-(YP2-YM1))       

c                                                                       
c                                                                       
      pp(x)=amax1(0.,x)
      pn(x)=amin1(0.,x)

      if ( ifm .eq. 5 ) then
        xf = xinp(0,0)
        return
      endif

c                                                                       
c  here starts rezidual advection                                       
c                                                                       
      do 50 j=-ior,ior
      do 51 i=-ior,ior
      y(i)=xinp(j,i)
   51 continue
c                                                                       
      fl(0)=donor(y(-1),y(0),xig(ifm))
      fl(1)=donor(y( 0),y(1),xig(ifm))
      w=y(0)-(fl(1)-fl(0))
      mxm=amax1(y(-1),y(0),y(1),w)
      mn=amin1(y(-1),y(0),y(1),w)
c                                                                       
c      if(ior.eq.1) then
c      f(0)=tr2(y(-1),y(0),xig(ifm))
c      f(1)=tr2(y( 0),y(1),xig(ifm))
c      endif
      if(ior.eq.2) then
      f(0)=tr4(y(-2),y(-1),y(0),y(1),xig(ifm))
      f(1)=tr4(y(-1),y( 0),y(1),y(2),xig(ifm))
      endif
c                                                                       
      f(0)=f(0)-fl(0)
      f(1)=f(1)-fl(1)
      ov=(mxm-w)/(-pn(f(1))+pp(f(0))+ep)
      un=(w-mn)/( pp(f(1))-pn(f(0))+ep)
      f(0)=pp(f(0))*amin1(1.,ov)+pn(f(0))*amin1(1.,un)
      f(1)=pp(f(1))*amin1(1.,un)+pn(f(1))*amin1(1.,ov)
      y(0)=w-(f(1)-f(0))
c                                                                       

      z(j)=y(0)
c                                                                       
   50 continue
c                                                                       
c                                                                       
      fl(0)=donor(z(-1),z(0),xjg(ifm))
      fl(1)=donor(z( 0),z(1),xjg(ifm))
      w=z(0)-(fl(1)-fl(0))
      mxm=amax1(z(-1),z(0),z(1),w)
      mn=amin1(z(-1),z(0),z(1),w)
c                                                                       
      if(ior.eq.1) then
       f(0)=tr2(z(-1),z(0),xjg(ifm))
       f(1)=tr2(z( 0),z(1),xjg(ifm))
      endif
      if(ior.eq.2) then
       f(0)=tr4(z(-2),z(-1),z(0),z(1),xjg(ifm))
       f(1)=tr4(z(-1),z( 0),z(1),z(2),xjg(ifm))
      endif
c                                                                       
      f(0)=f(0)-fl(0)
      f(1)=f(1)-fl(1)
      ov=(mxm-w)/(-pn(f(1))+pp(f(0))+ep)
      un=(w-mn)/( pp(f(1))-pn(f(0))+ep)
      f(0)=pp(f(0))*amin1(1.,ov)+pn(f(0))*amin1(1.,un)
      f(1)=pp(f(1))*amin1(1.,un)+pn(f(1))*amin1(1.,ov)
      continue
      xf=w-(f(1)-f(0))
c                                                                       
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Vector version
C
      subroutine sintFv(xinp,xf,kl,kxx,ifm )
#     include "rsl.inc"
#     include "parame.incl"
      parameter(nf=9,ifct=1,ior=IOR)
c                                                                       
c                                                                       
c                                                                       
      dimension xf(kxx), xinp(kxx,-2:+2,-2:+2)
      dimension y(mkx+1,-2:2),z(mkx+1,-2:2),
     1                   f(mkx+1,0:1)
      data  ep/ 1.e-10/
      parameter(nonos=1)
      PARAMETER(ONE12=1./12.,ONE24=1./24.)
      dimension  fl(mkx+1,0:1)
      COMMON /DEPAR2/ XIG(9),XJG(9),IG0(MJX,MIX),JG0(MJX,MIX)
      real      mxm,mn
c                                                                       
      donor(y1,y2,ax)=(y1*amax1(0.,sign(1.,ax))-
     1                y2*amin1(0.,sign(1.,ax)))*ax
      tr2(y1,y2,ax)=ax*.5*(y1+y2)-ax**2*.5*(y2-y1)
c                                                                       
c                                                                       
      TR4(YM1,Y0,YP1,YP2,A)=A*ONE12*(7.*(YP1+Y0)-(YP2+YM1))
     1 -A*A*ONE24*(15.*(YP1-Y0)-(YP2-YM1))-A*A*A*ONE12*((YP1+Y0)
     2 -(YP2+YM1))+A*A*A*A*ONE24*(3.*(YP1-Y0)-(YP2-YM1))       

c                                                                       
c                                                                       
      pp(x)=amax1(0.,x)
      pn(x)=amin1(0.,x)

      if ( ifm .eq. 5 ) then
        do k = 1, kl
          xf(k) = xinp(k,0,0)
        enddo
        return
      endif

c                                                                       
c  here starts rezidual advection                                       
c                                                                       
      do 50 j=-ior,ior

      do 51 i=-ior,ior
      do 51 k = 1, kl
        y(k,i)=xinp(k,j,i)
   51 continue
c                                                                       
      do k = 1, kl
        fl(k,0)=donor(y(k,-1),y(k,0),xig(ifm))
        fl(k,1)=donor(y(k, 0),y(k,1),xig(ifm))
        w=y(k,0)-(fl(k,1)-fl(k,0))
        mxm=amax1(y(k,-1),y(k,0),y(k,1),w)
        mn=amin1(y(k,-1),y(k,0),y(k,1),w)
        f(k,0)=tr4(y(k,-2),y(k,-1),y(k,0),y(k,1),xig(ifm))
        f(k,1)=tr4(y(k,-1),y(k, 0),y(k,1),y(k,2),xig(ifm))
c                                                                       
        f(k,0)=f(k,0)-fl(k,0)
        f(k,1)=f(k,1)-fl(k,1)
        ov=(mxm-w)/(-pn(f(k,1))+pp(f(k,0))+ep)
        un=(w-mn)/( pp(f(k,1))-pn(f(k,0))+ep)
        f(k,0)=pp(f(k,0))*amin1(1.,ov)+pn(f(k,0))*amin1(1.,un)
        f(k,1)=pp(f(k,1))*amin1(1.,un)+pn(f(k,1))*amin1(1.,ov)
        y(k,0)=w-(f(k,1)-f(k,0))
c                                                                       
        z(k,j)=y(k,0)

      enddo
c                                                                       
   50 continue
c                                                                       
c                                                                       
      do k = 1, kl
        fl(k,0)=donor(z(k,-1),z(k,0),xjg(ifm))
        fl(k,1)=donor(z(k, 0),z(k,1),xjg(ifm))
        w=z(k,0)-(fl(k,1)-fl(k,0))
        mxm=amax1(z(k,-1),z(k,0),z(k,1),w)
        mn=amin1(z(k,-1),z(k,0),z(k,1),w)
c                                                                       
        f(k,0)=tr4(z(k,-2),z(k,-1),z(k,0),z(k,1),xjg(ifm))
        f(k,1)=tr4(z(k,-1),z(k, 0),z(k,1),z(k,2),xjg(ifm))
c                                                                       
        f(k,0)=f(k,0)-fl(k,0)
        f(k,1)=f(k,1)-fl(k,1)
        ov=(mxm-w)/(-pn(f(k,1))+pp(f(k,0))+ep)
        un=(w-mn)/( pp(f(k,1))-pn(f(k,0))+ep)
        f(k,0)=pp(f(k,0))*amin1(1.,ov)+pn(f(k,0))*amin1(1.,un)
        f(k,1)=pp(f(k,1))*amin1(1.,un)+pn(f(k,1))*amin1(1.,ov)
        continue
        xf(k)=w-(f(k,1)-f(k,0))
      enddo
c                                                                       
      return
      end
#else
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C hopefully faster version of sint used by parallel code
C

      subroutine sintF(xinp,xf,ifm,ax1i,ax2i,ax3i,ax4i,
     +                             ax1j,ax2j,ax3j,ax4j)
#     include "rsl.inc"
#     include "parame.incl"
      parameter(nf=9,ifct=1,ior=IOR)
c                                                                       
c                                                                       
c                                                                       
      dimension y(-2:2),z(-2:2),
     1                   f(0:1)
      data  ep/ 1.e-10/
      parameter(nonos=1)
      dimension  fl(0:1),xinp(-2:+2,-2:2)
      COMMON /DEPAR2/ XIG(9),XJG(9),IG0(MJX,MIX),JG0(MJX,MIX)
      real      mxm,mn
c                                                                       
      donor(y1,y2,ax)=(y1*amax1(0.,sign(1.,ax))-
     1                y2*amin1(0.,sign(1.,ax)))*ax
      tr2(y1,y2,ax)=ax*.5*(y1+y2)-ax**2*.5*(y2-y1)
c                                                                       
c                                                                       
      tr4i(ym1,y0,yp1,yp2)=ax1i*(7.*(yp1+y0)-(yp2+ym1))
     1 -ax2i*(15.*(yp1-y0)-(yp2-ym1))-ax3i*((yp1+y0)
     2 -(yp2+ym1))+ax4i*(3.*(yp1-y0)-(yp2-ym1))

      tr4j(ym1,y0,yp1,yp2)=ax1j*(7.*(yp1+y0)-(yp2+ym1))
     1 -ax2j*(15.*(yp1-y0)-(yp2-ym1))-ax3j*((yp1+y0)
     2 -(yp2+ym1))+ax4j*(3.*(yp1-y0)-(yp2-ym1))
c                                                                       
c                                                                       
      pp(x)=amax1(0.,x)
      pn(x)=amin1(0.,x)

      if ( ifm .eq. 5 ) then
        xf = xinp(0,0)
        return
      endif

c                                                                       
c  here starts rezidual advection                                       
c                                                                       
      do 50 j=-ior,ior
      do 51 i=-ior,ior
      y(i)=xinp(j,i)
   51 continue
c                                                                       
      fl(0)=donor(y(-1),y(0),xig(ifm))
      fl(1)=donor(y( 0),y(1),xig(ifm))
      w=y(0)-(fl(1)-fl(0))
      mxm=amax1(y(-1),y(0),y(1),w)
      mn=amin1(y(-1),y(0),y(1),w)
c                                                                       
c      if(ior.eq.1) then
c      f(0)=tr2(y(-1),y(0),xig(ifm))
c      f(1)=tr2(y( 0),y(1),xig(ifm))
c      endif
      if(ior.eq.2) then
      f(0)=tr4i(y(-2),y(-1),y(0),y(1))
      f(1)=tr4i(y(-1),y( 0),y(1),y(2))
      endif
c                                                                       
      f(0)=f(0)-fl(0)
      f(1)=f(1)-fl(1)
      ov=(mxm-w)/(-pn(f(1))+pp(f(0))+ep)
      un=(w-mn)/( pp(f(1))-pn(f(0))+ep)
      f(0)=pp(f(0))*amin1(1.,ov)+pn(f(0))*amin1(1.,un)
      f(1)=pp(f(1))*amin1(1.,un)+pn(f(1))*amin1(1.,ov)
      y(0)=w-(f(1)-f(0))
c                                                                       

      z(j)=y(0)
c                                                                       
   50 continue
c                                                                       
c                                                                       
      fl(0)=donor(z(-1),z(0),xjg(ifm))
      fl(1)=donor(z( 0),z(1),xjg(ifm))
      w=z(0)-(fl(1)-fl(0))
      mxm=amax1(z(-1),z(0),z(1),w)
      mn=amin1(z(-1),z(0),z(1),w)
c                                                                       
      if(ior.eq.1) then
       f(0)=tr2(z(-1),z(0),xjg(ifm))
       f(1)=tr2(z( 0),z(1),xjg(ifm))
      endif
      if(ior.eq.2) then
       f(0)=tr4j(z(-2),z(-1),z(0),z(1))
       f(1)=tr4j(z(-1),z( 0),z(1),z(2))
      endif
c                                                                       
      f(0)=f(0)-fl(0)
      f(1)=f(1)-fl(1)
      ov=(mxm-w)/(-pn(f(1))+pp(f(0))+ep)
      un=(w-mn)/( pp(f(1))-pn(f(0))+ep)
      f(0)=pp(f(0))*amin1(1.,ov)+pn(f(0))*amin1(1.,un)
      f(1)=pp(f(1))*amin1(1.,un)+pn(f(1))*amin1(1.,ov)
      continue
      xf=w-(f(1)-f(0))
c                                                                       
      return
      end
#endif


      SUBROUTINE MP_INIT_SAVVARS(INEST)
C                                                                       
C---------------------------------------------------------------------- 
C                                                                       
#     include <rsl.inc>
#     include <rslcom.inc>
      FLIC_RUN_DECL
#     include <parame.incl>                                             
#     include <addr0.incl>                                              
#     include <param2.incl>                                             
#     include <param3.incl>                                             
#     include <varia.incl>                                              
#     include <point3d.incl>                                            
#     include <point2d.incl>                                            
#     include <nonhyd.incl>                                             
#     include <various.incl>                                            
#     include <nestl.incl>                                              
#     include <nnnhyd.incl>                                             
#     include <nnnhydb.incl>                                            
#     include <pmoist.incl>                                             
#     include <pbltke.incl>
#     include <soilp.incl>
#     include <defines.incl>


      logical new_cd_point, new_cd_point2
      integer i,j,pig,pjg,cm,cn,nig,njg,retval
      integer msize
      real ax,ax1i,ax2i,ax3i,ax4i,ax1j,ax2j,ax3j,ax4j
      COMMON /DEPAR2/ XIG(9),XJG(9),IG0(MJX,MIX),JG0(MJX,MIX)

#include <packdown.incl>
      
      do k = 1, mkx
        FLIC_DO_N(j,1,jl)
          FLIC_DO_M(i,1,il)
            ta_sav(i,j,k,inest)   = ta(i,j,k)
            qva_sav(i,j,k,inest)  = qva(i,j,k)
            qca_sav(i,j,k,inest)  = qca(i,j,k)
            qra_sav(i,j,k,inest)  = qra(i,j,k)
            qia_sav(i,j,k,inest)  = qia(i,j,k)
            qnia_sav(i,j,k,inest) = qnia(i,j,k)
            qga_sav(i,j,k,inest)  = qga(i,j,k)
            qnca_sav(i,j,k,inest) = qnca(i,j,k)
            ua_sav(i,j,k,inest)   = ua(i,j,k)
            va_sav(i,j,k,inest)   = va(i,j,k)
            ppa_sav(i,j,k,inest)  = ppa(i,j,k)
            wa_sav(i,j,k,inest)   = wa(i,j,k)
            if ( ibltyp(inest) .eq. 6 ) then
	      tkea_sav(i,j,k) = tkea(i,j,k)
            endif
          FLIC_ENDDO
        FLIC_ENDDO
      enddo

      FLIC_DO_N(j,1,jl)
        FLIC_DO_M(i,1,il)
          wa_sav(i,j,mkx+1,inest) = wa(i,j,mkx+1)
#ifdef IBLT6
          if ( ibltyp(inest) .eq. 6 ) then
          tkea_sav(i,j,mkx+1) = tkea(i,j,mkx+1)
          endif
#endif
          rainc_sav(i,j) = rainc(i,j)
          rainnc_sav(i,j) = rainnc(i,j)
C 000901
          IF(IMOIAV(INEST).EQ.2)THEN
            mavail_sav(i,j) = mavail(i,j)
          ENDIF
        FLIC_ENDDO
      FLIC_ENDDO

C 001024  
      IF(ISOIL.EQ.2 .OR. ISOIL.EQ.3)THEN
        do k = 1, mlx
          FLIC_DO_N(j,1,jl)
            FLIC_DO_M(i,1,il)
              stca_sav(i,j,k) = stca(i,j,k)
              smca_sav(i,j,k) = smca(i,j,k)
              swca_sav(i,j,k) = swca(i,j,k)
            FLIC_ENDDO
          FLIC_ENDDO
        enddo
       ENDIF

      return
      end

