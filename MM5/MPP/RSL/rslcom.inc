      integer nproc, nproc_lt, nproc_ln, myproc, mpp_io_node
      common /rsl/  nproc, nproc_lt, nproc_ln,  myproc, mpp_io_node

      integer domains(RSL_MAXDOMAINS)
      integer sten_a(RSL_MAXDOMAINS)    ,sten_b(RSL_MAXDOMAINS)
      integer sten_c(RSL_MAXDOMAINS)
      integer sten_d(RSL_MAXDOMAINS)
      integer sten_e(RSL_MAXDOMAINS)
      integer sten_sa(RSL_MAXDOMAINS)   ,sten_sb(RSL_MAXDOMAINS)
      integer sten_sa1(RSL_MAXDOMAINS)
      integer sten_init(RSL_MAXDOMAINS) ,sten_bdy(RSL_MAXDOMAINS)
      integer sten_fdda_0(RSL_MAXDOMAINS)
      integer sten_fdda_1(RSL_MAXDOMAINS)
      integer sten_fdobs(RSL_MAXDOMAINS)

      common /rsl/ domains, sten_a, sten_b, sten_c, sten_sa, sten_sb
      common /rsl/ sten_d, sten_e
      common /rsl/ sten_sa1
      common /rsl/ sten_init, sten_bdy
      common /rsl/ sten_fdda_0
      common /rsl/ sten_fdda_1
      common /rsl/ sten_fdobs

cjm 960104 new stencil used for extra communication step if ORIG_PBL
cjm is defined to put hirpbl back in its original place in solve3.
      integer sten_ca(RSL_MAXDOMAINS)
      common /rsl/  sten_ca

cccccccccccccccccccccccccc added for nested grids ccccccccccccccccc

      integer sten_movedot_a(RSL_MAXDOMAINS)
      integer sten_movedot_c(RSL_MAXDOMAINS)
      integer sten_movedot_ini(RSL_MAXDOMAINS)
      integer sten_backdot(RSL_MAXDOMAINS)
      integer sten_sint(RSL_MAXDOMAINS)         ! stencil for semi-Lag. interp
      integer sten_sint_lbdy(RSL_MAXDOMAINS)    ! added for periodic lower bdy forcing
      integer sten_sint_init(RSL_MAXDOMAINS)    ! stencil for semi-Lag. interp (init)
      integer sten_ps2dot(RSL_MAXDOMAINS)       ! for pressure interpolation (down)
      integer sten_ps2dot2(RSL_MAXDOMAINS)      ! for pressure interpolation (up)
      integer sten_smth_ew(RSL_MAXDOMAINS,RSL_MAXDOMAINS) 
                                                ! for smoothing of nested values
      integer sten_smth_ns(RSL_MAXDOMAINS)      ! for smoothing of nested values
      integer sten_smth_3(RSL_MAXDOMAINS)       ! for smoothing (IFEED=3)
      integer sten_finishnestinit(RSL_MAXDOMAINS) ! to finish intializing nests
      integer sten_rainfbk(RSL_MAXDOMAINS)      ! feedback of rainfall

      common /rsl/  sten_movedot_a, sten_movedot_c
      common /rsl/  sten_movedot_ini, sten_backdot
      common /rsl/  sten_sint, sten_sint_lbdy, sten_sint_init
      common /rsl/  sten_ps2dot, sten_ps2dot2
      common /rsl/  sten_smth_ew, sten_smth_ns, sten_smth_3
      common /rsl/  sten_finishnestinit
      common /rsl/  sten_rainfbk

      integer decomp,glen,llen,llenk,glenk
      common /rsl/ DECOMP(3),GLEN(3),LLEN(3),GLENK(3),LLENK(3)

#if (IFDDA == 1)
      integer sten_fdda_1(RSL_MAXDOMAINS)
      common /rsl/  sten_fdda_1
#endif


      integer sten_upr(RSL_MAXDOMAINS)
      common /rsl/ sten_upr

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


