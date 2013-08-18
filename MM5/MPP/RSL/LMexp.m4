define( rsl_JJX_x, 512 )

define(noflic,$1)
define(noflic1,$1)
define(noflic2,`$1,$2')
define(noflic3,`$1,$2,$3')
define(noflic4,`$1,$2,$3,$4')
define(NOFLIC,$1)
define(NOFLIC1,$1)
define(NOFLIC2,`$1,$2')
define(NOFLIC3,`$1,$2,$3')
define(NOFLIC4,`$1,$2,$3,$4')

define( FLIC_RUN_DECL, 
      `
       integer
     + xinest_x0,nrun_x0,
     + js_x0(rsl_JJX_x),je_x0(rsl_JJX_x),
     + is_x0(rsl_JJX_x),ie_x0(rsl_JJX_x),
     + idif_x0,jdif_x0
       integer
     + xinest_x1,nrun_x1,
     + js_x1(rsl_JJX_x),je_x1(rsl_JJX_x),
     + is_x1(rsl_JJX_x),ie_x1(rsl_JJX_x),
     + idif_x1,jdif_x1
       integer
     + xinest_x2,nrun_x2,
     + js_x2(rsl_JJX_x),je_x2(rsl_JJX_x),
     + is_x2(rsl_JJX_x),ie_x2(rsl_JJX_x),
     + idif_x2,jdif_x2
       integer
     + xinest_x3,nrun_x3,
     + js_x3(rsl_JJX_x),je_x3(rsl_JJX_x),
     + is_x3(rsl_JJX_x),ie_x3(rsl_JJX_x),
     + idif_x3,jdif_x3
       integer
     + xinest_x8,nrun_x8,
     + js_x8(rsl_JJX_x),je_x8(rsl_JJX_x),
     + is_x8(rsl_JJX_x),ie_x8(rsl_JJX_x),
     + idif_x8,jdif_x8

      integer ndum_xx(150)

       common /rsl_run_decl/
     + xinest_x0,nrun_x0,
     + js_x0,je_x0,
     + is_x0,ie_x0,
     + idif_x0,jdif_x0
       common /rsl_run_decl/
     + xinest_x1,nrun_x1,
     + js_x1,je_x1,
     + is_x1,ie_x1,
     + idif_x1,jdif_x1
       common /rsl_run_decl/
     + xinest_x2,nrun_x2,
     + js_x2,je_x2,
     + is_x2,ie_x2,
     + idif_x2,jdif_x2
       common /rsl_run_decl/
     + xinest_x3,nrun_x3,
     + js_x3,je_x3,
     + is_x3,ie_x3,
     + idif_x3,jdif_x3
       common /rsl_run_decl/
     + xinest_x8,nrun_x8,
     + js_x8,je_x8,
     + is_x8,ie_x8,
     + idif_x8,jdif_x8
       integer jl_x, il_x, idif, jdif
       common /rsl_run_decl/ jl_x, il_x, idif, jdif, ndum_xx')

define( FLIC_INIT_RUNVARS, 
      call rsl_reg_run_infop( $1, 0, rsl_JJX_x,
     +xinest_x0,
     +is_x0,ie_x0,js_x0,je_x0,
     +idif_x0,jdif_x0)
      idif = idif_x0
      jdif = jdif_x0
      call rsl_reg_run_infop( $1, 1, rsl_JJX_x,
     +xinest_x1,
     +is_x1,ie_x1,js_x1,je_x1,
     +idif_x1,jdif_x1)
      call rsl_reg_run_infop( $1, 2, rsl_JJX_x,
     +xinest_x2,
     +is_x2,ie_x2,js_x2,je_x2,
     +idif_x2,jdif_x2)
      call rsl_reg_run_infop( $1, 3, rsl_JJX_x,
     +xinest_x3,
     +is_x3,ie_x3,js_x3,je_x3,
     +idif_x3,jdif_x3)
      call rsl_reg_run_infop( $1, 8, rsl_JJX_x,
     +xinest_x8,
     +is_x8,ie_x8,js_x8,je_x8,
     +idif_x8,jdif_x8))

define(MAX_RUNPAD,2)

#define USE_RUNPAD 0

define(C_FLIC_RUNPAD,
`#undef USE_RUNPAD
#define USE_RUNPAD $1')

define( FLIC_DO_N,
        `ifelse(len($4),0,
          ifdef(`INSIDE_MLOOP',``FLIC_DO_JMAJ($1,$2,$3)'',``FLIC_DO_JMAJ($1,$2,$3)''),
    ``doo'')')

define( FLIC_DO_M,
        `ifelse(len($4),0,
          ifdef(`INSIDE_NLOOP',``FLIC_DO_IMAJ($1,$2,$3)'',``FLIC_DO_IMAJ($1,$2,$3)''),
    ``doo'')')

define( FLIC_DO_JMAJ,
       do $1=js_x USE_RUNPAD ( $2 ) `,' je_x USE_RUNPAD ( $3 )
       `pushdef( `FLIC_MACRO_CLOSER',
       enddo  )'
       )

define( FLIC_DO_IMAJ,
       do $1=is_x USE_RUNPAD ( $2 ) `,' ie_x USE_RUNPAD ( $3 )
       `pushdef( `FLIC_MACRO_CLOSER',
       enddo  )'
       )

define( FLIC_ENDDO, 
        `FLIC_MACRO_CLOSER' `popdef(`FLIC_MACRO_CLOSER')' )

define( FLIC_COLLAPSE_DO_M,  )
define( FLIC_COLLAPSE_DO_N,  )
define( FLIC_COLLAPSE_ENDDO,  )

define( FLIC_G2L_N, (min(max(1,($1)+jdif_x0),$2)) )
define( FLIC_G2L_M, (min(max(1,($1)+idif_x0),$2)) )

define( FLIC_L2G_N, (($1)-jdif_x0) )
define( FLIC_L2G_M, (($1)-idif_x0) )

define(IS_M,(FLIC_L2G_M($1).eq.($2)))
define(IS_N,(FLIC_L2G_N($1).eq.($2)))

