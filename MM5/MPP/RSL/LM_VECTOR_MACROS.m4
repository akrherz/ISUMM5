undefine( `FLIC_DO_M' )
undefine( `FLIC_DO_IMAJ' )

define( `FLIC_DO_M',
        `ifelse(len($4),0,
          ifdef(`INSIDE_NLOOP',``FLIC_DO_IMAJ($1,$2,$3)'',``FLIC_DO_IMAJ($1,$2,$3)''),
    ``doo'')')

define( `FLIC_DO_IMAJ',
       do $1= ist `,' ien
       `pushdef( `FLIC_MACRO_CLOSER',
       enddo  )'
       )

define( D_DUMARG_G, idum_dum )
define( D_DUMARG_M, $1 )
define( D_DUMARG_N, $1 )
define( D_ARG_M, $1 )
define( D_ARG_N, $1 )
define( D_ARG_G, idum_dum )
define( D_DECL_M, mix*mjx )
define( D_DECL_N, 1 )

