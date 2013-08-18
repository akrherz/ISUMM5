#include <stdio.h>

#define ARGSS  \
     inest, \
     xtime,nts,its,jts, \
     tscrn, qva_x_rpsa, \
     uats_x_rpsa, vats_x_rpsa, \
     psa, ppa_x_rpsa, \
     rainc,rainnc,clw, \
     glw,hfx,qfx, \
     gsw_div_1_minus_alb,tga

#ifdef T3E
OUTTS_C ( ARGSS )
#else
#  ifdef NOUNDERSCORE
outts_c( ARGSS )
#  else
#    ifdef F2CSTYLE
outts_c__( ARGSS )
#    else
outts_c_( ARGSS )
#    endif
#  endif
#endif
     int * inest ; 
     float * xtime ; 
     int * nts, *its, *jts ;
     float * tscrn, * qva_x_rpsa ;
     float * uats_x_rpsa, * vats_x_rpsa ;
     float * psa, * ppa_x_rpsa ;
     float * rainc, * rainnc, * clw ;
     float * glw, * hfx, * qfx ;
     float * gsw_div_1_minus_alb, * tga ;
{
   fprintf( stderr , "DUMPTS %d %f %d %d %d ",*inest,*xtime,*nts,*its,*jts  ) ;
   fprintf( stderr ,        "%f %f "      ,*tscrn, *qva_x_rpsa ) ;
   fprintf( stderr ,        "%f %f "      ,*uats_x_rpsa, *vats_x_rpsa ) ;
   fprintf( stderr ,        "%f %f "      ,*psa, *ppa_x_rpsa ) ;
   fprintf( stderr ,        "%f %f %f "   ,*rainc,*rainnc,*clw ) ;
   fprintf( stderr ,        "%f %f %f "   ,*glw,*hfx,*qfx ) ;
   fprintf( stderr ,        "%f %f\n"     ,*gsw_div_1_minus_alb,*tga ) ;
}
