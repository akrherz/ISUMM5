#include <stdio.h>
#include <stdlib.h>
#include <float.h>

c_nan( x )
  float * x ;
{
/*   fprintf(stderr,"signalling nan: %08x\n",SSNAN) ; */
   *x = FLT_SNAN ;
}

#if 1
c_testfloat( x, ret )
  float *x ;
  int *ret ;
{
  float snan, qnan, infi ;
  bcopy( &SSNAN, &snan, sizeof(float)) ;
  bcopy( &SQNAN, &qnan, sizeof(float)) ;
  bcopy( &SINFINITY, &infi, sizeof(float)) ;
  *ret = 0 ;
  if      ( *x ==  snan )
    { *ret = 1 ; }
  else if ( *x ==  qnan )
    { *ret = 2 ; }
  else if ( *x ==  infi )
    { *ret = 3 ; }

  return ;
}
#else
c_testfloat( x, ret )
  float * x ;
  int * ret ;
{
  return ;
}
#endif

