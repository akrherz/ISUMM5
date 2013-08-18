define( rsl_JJX_x, 512 )

define(noflic,$1)
define(noflic1,$1)
define(noflic2,$1,',',$2)
define(noflic3,$1,',',$2,',',$3)
define(noflic4,$1,',',$2,',',$3,',',$4)
define(NOFLIC,$1)
define(NOFLIC1,$1)
define(NOFLIC2,$1,',',$2)
define(NOFLIC3,$1,',',$2,',',$3)
define(NOFLIC4,$1,',',$2,',',$3,',',$4)

define( FLIC_RUN_DECL, 
      `integer ig, jg, idif, jdif
      integer firstj_x, lastj_x, nr_t
      integer idif_x1,idif_x2,idif_x3,idif_x4
      integer jdif_x1,jdif_x2,jdif_x3,jdif_x4
      integer n_dum
      integer n_rp_i0
      integer n_rp_i1
      integer n_rp_i2
      integer n_rp_i3
      integer n_rp_i4
      integer n_rp_j0
      integer n_rp_j1
      integer n_rp_j2
      integer n_rp_j3
      integer n_rp_j4

      parameter ( n_rp_i0 = 0 )
      parameter ( n_rp_i1 = 1 )
      parameter ( n_rp_i2 = 2 )
      parameter ( n_rp_i3 = 1 )
      parameter ( n_rp_i4 = 2 )

      parameter ( n_rp_j0 = 0 )
      parameter ( n_rp_j1 = 1 )
      parameter ( n_rp_j2 = 2 )
      parameter ( n_rp_j3 = 0 )
      parameter ( n_rp_j4 = 0 )

      integer xinest_x0,
     +nrun_x0,js_x0(rsl_JJX_x),is_x0(rsl_JJX_x),ie_x0(rsl_JJX_x),
     +jg2n_x(500),
     +idif_x0,jdif_x0,nr_x
      integer xinest_x1,
     +nrun_x1,js_x1(rsl_JJX_x),is_x1(rsl_JJX_x),ie_x1(rsl_JJX_x)
      integer xinest_x2,
     +nrun_x2,js_x2(rsl_JJX_x),is_x2(rsl_JJX_x),ie_x2(rsl_JJX_x)
      integer xinest_x3,
     +nrun_x3,js_x3(rsl_JJX_x),is_x3(rsl_JJX_x),ie_x3(rsl_JJX_x)
      integer xinest_x4,
     +nrun_x4,js_x4(rsl_JJX_x),is_x4(rsl_JJX_x),ie_x4(rsl_JJX_x)

      integer
     +nruni_x0,is2_x0(rsl_JJX_x),js2_x0(rsl_JJX_x),je2_x0(rsl_JJX_x),
     +ig2n_x(500)
      integer
     +nruni_x1,is2_x1(rsl_JJX_x),js2_x1(rsl_JJX_x),je2_x1(rsl_JJX_x)
      integer
     +nruni_x2,is2_x2(rsl_JJX_x),js2_x2(rsl_JJX_x),je2_x2(rsl_JJX_x)
      integer
     +nruni_x3,is2_x3(rsl_JJX_x),js2_x3(rsl_JJX_x),je2_x3(rsl_JJX_x)
      integer
     +nruni_x4,is2_x4(rsl_JJX_x),js2_x4(rsl_JJX_x),je2_x4(rsl_JJX_x)

      integer ndum_xx(150)

      common /rsl_run_decl/
     +ig,jg,
     +idif,jdif,
     +xinest_x0,nrun_x0,
     +js_x0,is_x0,ie_x0,
     +idif_x0,jdif_x0,nr_x,jg2n_x,
     +nruni_x0,is2_x0,js2_x0,je2_x0,
     +ig2n_x
      common /rsl_run_decl/
     +xinest_x1,nrun_x1,
     +js_x1,is_x1,ie_x1,
     +nruni_x1,is2_x1,js2_x1,je2_x1
      common /rsl_run_decl/
     +xinest_x2,nrun_x2,
     +js_x2,is_x2,ie_x2,
     +nruni_x2,is2_x2,js2_x2,je2_x2
      common /rsl_run_decl/
     +xinest_x3,nrun_x3,
     +js_x3,is_x3,ie_x3,
     +nruni_x3,is2_x3,js2_x3,je2_x3
      common /rsl_run_decl/
     +xinest_x4,nrun_x4,
     +js_x4,is_x4,ie_x4,
     +nruni_x4,is2_x4,js2_x4,je2_x4
      integer jl_x, il_x
      common /rsl_run_decl/ jl_x, il_x, ndum_xx')

define( FLIC_INIT_RUNVARS, 
      call rsl_get_run_infop( $1, 0, rsl_JJX_x,
     +xinest_x0,nrun_x0,nruni_x0,
     +js_x0,is_x0,ie_x0,
     +is2_x0,js2_x0,je2_x0,
     +idif_x0,jdif_x0,jg2n_x,ig2n_x)
      idif = idif_x0
      jdif = jdif_x0
      call rsl_get_run_infop( $1, 1, rsl_JJX_x,
     +xinest_x1,nrun_x1,nruni_x1,
     +js_x1,is_x1,ie_x1,
     +is2_x1,js2_x1,je2_x1,
     +idif_x1,jdif_x1,ndum_xx,ndum_xx)
      call rsl_get_run_infop( $1, 2, rsl_JJX_x,
     +xinest_x2,nrun_x2,nruni_x2,
     +js_x2,is_x2,ie_x2,
     +is2_x2,js2_x2,je2_x2,
     +idif_x2,jdif_x2,ndum_xx,ndum_xx)
      call rsl_get_run_infop( $1, 3, rsl_JJX_x,
     +xinest_x3,nrun_x3,nruni_x3,
     +js_x3,is_x3,ie_x3,
     +is2_x3,js2_x3,je2_x3,
     +idif_x3,jdif_x3,ndum_xx,ndum_xx)
      call rsl_get_run_infop( $1, 4, rsl_JJX_x,
     +xinest_x4,nrun_x4,nruni_x4,
     +js_x4,is_x4,ie_x4,
     +is2_x4,js2_x4,je2_x4,
     +idif_x4,jdif_x4,ndum_xx,ndum_xx))

define( I_IS, (ig.eq.($1)) )

define( J_IS, (jg.eq.($1)) )

define( IJ_IS, (I_IS($1).and.J_IS($2)))

define( J_IN_RANGE,
      (jg.ge.($1).and.
     + jg.le.($2)))

define( I_IN_RANGE,
      (ig.ge.($1).and.
     + ig.le.($2)))

define( FLIC_DOT_INTERIOR,
      (I_IN_RANGE(($1),il_x-($1)+1).and.
     +(J_IN_RANGE(($1),jl_x-($1)+1))))

define( FLIC_CROSS_INTERIOR, 
      (I_IN_RANGE(($1),il_x-($1)).and.
     +(J_IN_RANGE(($1),jl_x-($1)))))

define( J_IN_RANGE1,
        `pushdef( `FLIC_MACRO_CLOSER', endif )'
        if(J_IN_RANGE($1,$2))then)

define( I_IN_RANGE1,
        `pushdef( `FLIC_MACRO_CLOSER', endif )'
        if(I_IN_RANGE($1,$2))then)

define(MAX_RUNPAD,2)

#define USE_RUNPAD 0

define(C_FLIC_RUNPAD,
`#undef USE_RUNPAD
#define USE_RUNPAD $1')

define( FLIC_DO_N,
        `ifelse(len($4),0,
          ifdef(`INSIDE_MLOOP',``FLIC_DO_JMIN($1,$2,$3)'',``FLIC_DO_JMAJ($1,$2,$3)''),
    ``doo'')')

define( FLIC_DO_M,
        `ifelse(len($4),0,
          ifdef(`INSIDE_NLOOP',``FLIC_DO_IMIN($1,$2,$3)'',``FLIC_DO_IMAJ($1,$2,$3)''),
    ``doo'')')

define( FLIC_DO_JMAJ,
       do nr_x=1+MAX_RUNPAD-n_rp_j USE_RUNPAD `,'nrun_x USE_RUNPAD +(MAX_RUNPAD+n_rp_j USE_RUNPAD )
       $1=js_x USE_RUNPAD (nr_x)
       jg=$1-jdif_x0
       `define(`INSIDE_NLOOP',1)'
       if ( J_IN_RANGE( $2, $3 ) ) then
       `pushdef( `FLIC_MACRO_CLOSER',
       endif
       enddo   `undefine(`INSIDE_NLOOP')'  )'
       )

define( FLIC_DO_IMAJ,
       do nr_x=1+MAX_RUNPAD-n_rp_i USE_RUNPAD `,'nruni_x USE_RUNPAD +(MAX_RUNPAD+n_rp_i USE_RUNPAD)
       $1=is2_x USE_RUNPAD (nr_x)
       ig=$1-idif_x0
       `define(`INSIDE_MLOOP',1)'
       `ifelse(len($3),0,
       `pushdef( `FLIC_MACRO_CLOSER', enddo `undefine(`INSIDE_MLOOP')')',
       `if ( I_IN_RANGE( $2, $3 ) ) then
       pushdef( `FLIC_MACRO_CLOSER',
       endif
       enddo   `undefine(`INSIDE_MLOOP')'  )'
       )')

define( FLIC_ENDDO, 
        `FLIC_MACRO_CLOSER' `popdef(`FLIC_MACRO_CLOSER')' )

define( FLIC_DO_IMIN,
       do ig=max(is_x USE_RUNPAD (nr_x)-idif_x0,$2)`,'
     + min(ie_x USE_RUNPAD (nr_x)-idif_x0,$3)
       $1=ig+idif_x0
       `pushdef( `FLIC_MACRO_CLOSER',enddo)'
        )

define( FLIC_DO_JMIN,
       do jg=max(js2_x USE_RUNPAD (nr_x)-jdif_x0,$2)`,'
     + min(je2_x USE_RUNPAD (nr_x)-jdif_x0,$3)
       $1=jg+jdif_x0
       `pushdef( `FLIC_MACRO_CLOSER',enddo)'
        )

define( FLIC_G2L_N, (min(max(1,($1)+jdif_x0),$2)) )
define( FLIC_G2L_M, (min(max(1,($1)+idif_x0),$2)) )

define( FLIC_L2G_N, (($1)-jdif_x0) )
define( FLIC_L2G_M, (($1)-idif_x0) )


