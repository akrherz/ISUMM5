#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MAXDOM_MAKE  1
#define MAXPROC_MAKE 128
#include "rsl.h"

/* #define INDEX_2(A,B,NB)       ( (B) + (A)*(NB) ) */

#define BDY 3

static float s_eps = -9.0 ;  /* initial value */
static float meghz ;
static float proc_speeds[RSL_MAXPROC*2] ; /* larger for safety */

static int mix = -1 , mjx = -1 ;

static int inest ;
#ifdef NOUNDERSCORE 
# ifndef T3E
inest_stat ( setget, val )
# else
INEST_STAT ( setget, val )
# endif
#else
# ifdef F2CSTYLE
inest_stat__ ( setget, val )
# else
inest_stat_ ( setget, val )
# endif
#endif
  int * setget ;
  int * val ;
{
  if ( *setget == 1 ) {
    inest = *val ;
  }
  else
  {
    *val = inest ;
  }
}

#ifdef NOUNDERSCORE
# ifndef T3E
statmem_stat ( mix_p, mjx_p )
# else
STATMEM_STAT ( mix_p, mjx_p )
# endif
#else
# ifdef F2CSTYLE
statmem_stat__ ( mix_p, mjx_p )
# else
statmem_stat_ ( mix_p, mjx_p )
# endif
#endif
  int * mix_p ;
  int * mjx_p ;
{
  mix = *mix_p ;
  mjx = *mjx_p ;
}

#define EPS 0.05
int
alc( pspeeds, p, pwork, m, result, limitto )
  float pspeeds[] ;     /* p array of processor speeds */
  int p ;
  float pwork[] ;       /* m array of work */
  int result[] ;        /* p array of cell counts for each proc */
  int limitto ;         /* no p gets more cells than this, k? */
{
  int i, imax, iters ;
  float max, mean, imbalance, increment ;
  
  for ( iters = 0 ; 1 ; iters++ ) /* effectively infinite, so watch it */
  {
    if ( alc1( pspeeds, p, pwork, m, result, limitto ) )
    {
      /* it would have violated the limitto -- try smoothing out the
	 imbalance in processor speeds and try again.   This is, of course,
	 unrealistic, but it prevents the remainder from being slogged onto
	 one processor.  */
      max = -9999.0  ;
      mean = 0.0 ;
      for ( i = 0 ; i < p ; i++ )
      {
	if ( max < pspeeds[i] )  { max = pspeeds[i] ; imax = i ; }
	mean = mean + pspeeds[i] ;
      }
      mean = mean / p ;
      if ( max != 0.0 )
        imbalance = ( 1.0 - mean / max ) ;
      else
      {
	/* we done the best we could; return dejectedly */
        fprintf(stderr,
        "lb_alg.c (A) returning without having generated a good decomp after %d iters\n",iters) ;
        return(iters) ;
      }
      if ( imbalance < EPS )
      {
        for ( i = 0 ; i < p ; i++ ) pspeeds[i] = 1.0 ;

        /* make the blighters all equal and come what may */
        if( alc1( pspeeds, p, pwork, m, result, limitto ) )
        {
          /* go for broke */
          for ( i = 0 ; i < m ; i++ ) pwork[i] = 1.0 ;
          if( alc1( pspeeds, p, pwork, m, result, limitto ) )
          {
            /* we done the best we could */
            fprintf(stderr,
            "lb_alg.c (B) exiting without having generated a good decomp after %d iters\n",iters) ;
            fprintf(stderr,"limitto = %d, rsl_padarea %d\n",limitto,rsl_padarea)
 ;
            for ( i = 0 ; i < p ; i++ )
              fprintf(stderr,"pspeeds[%d] = %f result[%d] = %d\n",i,pspeeds[i],i
,result[i]) ;
            exit(9) ;
          }
        }
        return(iters) ;
      }

      /* try taking a little off the top and give it to the other guys */ 
      pspeeds[imax] = pspeeds[imax] * ( 1.0 - EPS * imbalance ) ;
      increment = pspeeds[imax]*EPS*imbalance / (p-1) ;
      for ( i = 0 ; i < p ; i++ )
      {
	if ( p != imax )
	{
	  /* avoid creating a new monster (throwing conservation to the wind) */
	  if ( (pspeeds[i]+increment) < pspeeds[imax] )
	    pspeeds[i] = pspeeds[i] + increment ;
	  else
	    pspeeds[i] = pspeeds[imax] ;
	}
      }
    }
    else
    {
      /* success */
      return(iters) ;
    }
  }
}

int
alc1( pspeeds, p, pwork, m, result, limitto )
  float pspeeds[] ;	/* p array of processor speeds */
  int p ;
  float pwork[] ;       /* m array of work */
  int m ;
  int result[] ;        /* p array of cell counts for each proc */
  int limitto ;		/* no p gets more cells than this, k? */
{
  int i, j, num ;
  float power, powerfrac ;
  float work, workfrac, w ;
  p-- ;
  if ( p < 0 )
  {
    if ( m <= 0 ) return(0);
    return(1) ;   /* this could occur if all the points had not been assigned */
  }

  for ( i = 0, power = 0.0 ; i <= p ; i++ )
    { power = power + pspeeds[i] ; }
  powerfrac = pspeeds[0]/power ;

  for ( i = 0, work = 0.0 ; i < m ; i++ )
    { work = work + pwork[i] ; }
  workfrac = powerfrac * work ;

  for ( i = 0, w = 0.0 ; i < m && i < limitto ; i++ )
  { 
    if ((w = w + pwork[i]) > workfrac ) break ;
  }
  if ( i > limitto ) return(1) ;

  num = i ;
  /* Kludge's for the real world:  on the last processor, if we're off by
     one, give the last proc the remainder */
  if ( p == 0 && (m - num) == 1 ) { num++ ; }
  result[0] = num ;
  
  /* recurse */
  return(alc1( pspeeds+1, p, pwork+num, m-num, result+1, limitto )) ;
}


decomp_load( wrk, weights, m, n, py, px )
  int wrk[], m, n, py, px ;
  float weights[] ;
{
  int *wk ;
  int res, tmpy, tmpx ;
  int x, y, x1, y1, ncells, nprocs, n_p, n_py, n_px, i, j, p, icnt ;
  int ye, ys, xs, xe ;
  int proc_x, proc_y ;
  int *pmesh ;
  float *wts, loadperproc, w_px, w_py, w, wtot, totalweight, totalspeed, totalwork ;
  float yspeed[RSL_MAXPROC*2], xspeed[RSL_MAXPROC*2], wy[RSL_MAXPROC*2] ;
  int result[RSL_MAXPROC*2], result_x[RSL_MAXPROC*2], result_y[RSL_MAXPROC*2] ;
  int wide ;
  int retval ;

  printf("DECOMP_LOAD CALLED: INEST=%d, M=%d, N=%d, PY=%d, PX=%d, MIX=%d, MJX=%d\n",
          inest,m,n,py,px,mix,mjx) ;

  wk = RSL_MALLOC ( int, m*n*2 ) ;
  pmesh = RSL_MALLOC ( int, px*py*2 ) ;
  wts = RSL_MALLOC ( float, m*n*2 ) ;

  nprocs = px * py ;

  for ( i = 0 ; i < py ; i++ )
  {
    yspeed[i] = 0.0 ;
    for ( j = 0 ; j < px ; j++ )
      yspeed[i] = yspeed[i] + proc_speeds[INDEX_2(i,j,px)] ;
  }
  for ( y = 0 ; y < m ; y++ )
  {
    wts[y] = 0.0 ;
    for ( x = 0 ; x < n ; x++ )
      if ( wrk[INDEX_2(x,y,m)] != RSL_INVALID )
      {
        wts[y] = wts[y] + weights[INDEX_2(x,y,m)] ;
      }
  }
  retval = alc( yspeed, py, wts, m, result_y, mix - 2*rsl_padarea ) ;

  /* result contains y decomp */

  tmpy = 0 ;
  for ( i = 0 ; i < py ; i++ )
  {
    ys = tmpy ;
    ye = tmpy + result_y[i] ;
    tmpy = ye ;
    for ( j = 0 ; j < px ; j++ )
    {
      xspeed[j] = proc_speeds[INDEX_2(i,j,px)] ;
    }
    for ( x = 0 ; x < n ; x++ )
    {
      wts[x] = 0.0 ;
      for ( y = ys ; y < ye ; y++ )
      {
        wts[x] = wts[x] + weights[INDEX_2(x,y,m)] ;
      }
    }
    retval = alc( xspeed, px, wts, n, result_x, mjx - 2*rsl_padarea ) ;
    for ( j = 0 ; j < px ; j++ )
    {
      result[INDEX_2(i,j,px)] = result_x[j] ;
    }
  }
  tmpy = 0 ;
  for ( i = 0 ; i < py ; i++ )
  {
    ys = tmpy ;
    ye = tmpy + result_y[i] ;
    tmpy = ye ;
    tmpx = 0 ;
    for ( j = 0 ; j < px ; j++ )
    {
      xs = tmpx ;
      xe = tmpx + result[INDEX_2(i,j,px)] ;
      tmpx = xe ;
      if ( ys < 0 || ys > m || ye < 0 || ye > m  ||
           xs < 0 || xs > n || xe < 0 || xe > n ) 
      {
       fprintf(stderr,"lb_alg.c : bad decomposition -- aborting\n ") ;
       fprintf(stderr,"   ys %d, ye %d, m %d\n", ys,ye,m) ;
       fprintf(stderr,"   xs %d, xe %d, n %d\n", xs,xe,n) ;
       fprintf(stderr,"   rsl_padarea %d\n",rsl_padarea) ;
       exit(3) ;
      }
      for ( y = ys ; y < ye ; y++ )
        for ( x = xs ; x < xe ; x++ )
          if ( wrk[INDEX_2(x,y,m)] != RSL_INVALID )
          {
	    wrk[INDEX_2(x,y,m)] = i * px + j ;
          }
    }
  }

  RSL_FREE (wk) ;
  RSL_FREE (wts) ;
  RSL_FREE (pmesh) ;

  return ;
}

/* 
   This routine expects info_p to be floating piont data containing
   an il*jl frame of timings for each timer zone, plus a frame (the
   first one) that is an il*jl array of the current processor mapping.

   It goes through a calculates an optimal mapping for each timer zone,
   then the best one is decided by voting (the current mapping also
   gets a vote).  The mapping that wins (and exceeds the current 
   mapping by an epsilon) is adopted.  Quality of the mapping is
   determined by calculating the time it would take on the slowest
   processor for all timer zones.

   The routine assings 0.0 to the first element of the info_p array
   if there is a new mapping that should be adopted.  A value of
   -1.0 is assigned if there is no better mapping.
*/


#ifdef NOUNDERSCORE
#  ifndef T3E
mapping ( w1, w2, info_p, m_p, n_p, py_p, px_p )
#  else
MAPPING ( w1, w2, info_p, m_p, n_p, py_p, px_p )
#  endif
#else
mapping_( w1, w2, info_p, m_p, n_p, py_p, px_p )
#endif
  int_p w1, w2, m_p, n_p, py_p, px_p ;
  float *info_p ;
{
  int i, j, m, n, tz ;
  int *maps, *mp ;
  float curr_runtime, min_new_runtime, rt ;
  float *agg, acc ;
  int new_mapping ;
  int retval ;
  int trim ;
  int bdy ;
  int msize ;
  float bdyweight ;
  int hist[RSL_MAXPROC] ;

  if ( inest == 1 )
  {
    /* this occurs only the first time (mother domain) */
    trim = 0 ;
#if ! defined(vpp) && ! defined(SGI_IA64)
    mhz(&meghz) ;
#else
    meghz = 100.0 ;  /* dummy value */
#endif
#ifndef MPI
-*-*-*- call to bare MPI in lb_alg.c -*-*-*-
#else
#  ifndef STUBS
    MPI_Gather(&meghz,1,MPI_FLOAT,proc_speeds,1,MPI_FLOAT,0,MPI_COMM_WORLD);
    MPI_Comm_size(MPI_COMM_WORLD,&msize);
    MPI_Bcast(proc_speeds,msize,MPI_FLOAT,0,MPI_COMM_WORLD);
#  else
    proc_speeds[0] = meghz ;
#  endif
#endif
  /* at this point, all processors have the vector of processor speeds
     and can compute the mapping */
    for ( i = 0 ; i < rsl_nproc_all ; i++ )
    {
      fprintf(stderr,"Processor %3d: Clock %.4f\n",i,proc_speeds[i]) ;
    }
#ifdef rs6000
/* this is kindof a kludgy test to insert some back of the
   envelope (and more or less emperical) speed data into the
   algorithm based on whether the chip is power 2 or p2sc */
    for ( i = 0 ; i < rsl_nproc_all ; i++ )
    {
      if ( proc_speeds[i] > 100.0 )
        proc_speeds[i] = 63.0 ;  /* call it p2sc */
      else
        proc_speeds[i] = 42.0 ;  /* call it p2sc */
    }
    fprintf(stderr,"Adjustment for IBM SP2:\n") ;
    for ( i = 0 ; i < rsl_nproc_all ; i++ )
    {
      fprintf(stderr,"Processor %3d: Clock %.4f\n",i,proc_speeds[i]) ;
    }
#endif
  }
  else
  {
    trim = 2 ;
  }

  fprintf(stderr,"Inest = %d\n",inest) ;
  fprintf(stderr,"Trim = %d\n",trim) ;

  m = *m_p ;
  n = *n_p ;
  maps = RSL_MALLOC ( int, m*n ) ;
  agg  = RSL_MALLOC ( float, m*n ) ;

  {
    char *x ;
    if (( x = getenv( "MM_BDYWEIGHT" )) != NULL )
    {
       bdyweight = atof( x ) ;
    }
    else
    {
       bdyweight = 0.3 ;
    }
    fprintf(stderr,"bdyweight = %f\n",bdyweight ) ;
  }

/* copy current mapping into first position of maps
   (truncating float data to int data) */
  for ( j = 0 ; j < n ; j++ )
    for ( i = 0 ; i < m ; i++ )
      agg[INDEX_2(j,i,m)] = 0.0 ;
  for ( j = 0 ; j < n-trim ; j++ )
    for ( i = 0 ; i < m-trim ; i++ )
      agg[INDEX_2(j,i,m)] = bdyweight ;   
  bdy = BDY ;
  for ( j = bdy ; j < n-trim-bdy ; j++ )
    for ( i = bdy ; i < m-trim-bdy ; i++ )
      agg[INDEX_2(j,i,m)] = 1.0 ;

  for ( j = 0 ; j < n ; j++ )
    for ( i = 0 ; i < m ; i++ )
      w2[INDEX_2(j,i,m)] = w1[INDEX_2(j,i,m)] ;
  decomp_load( w2,agg,m,n,*py_p,*px_p ) ;

/* for AFWA I/O node capability ... converted to a namelist thing in v3 */
  if ( rsl_io_node != 1 ) 
  {
    for ( j = 0 ; j < n ; j++ )
      for ( i = 0 ; i < m ; i++ )
        maps[INDEX_2(j,i,m)] = w2[INDEX_2(j,i,m)] ;
  }
  else
  {
    for ( j = 0 ; j < n ; j++ )
      for ( i = 0 ; i < m ; i++ )
        maps[INDEX_2(j,i,m)] = w2[INDEX_2(j,i,m)]+1 ;
  }

  printf("RSL_MAXPROC = %d\n",RSL_MAXPROC ) ;
  for ( i = 0 ; i < RSL_MAXPROC ; i++ ) hist[i] = 0 ;

  for ( i = m-1 ; i >= 0 ; i-- )
    for ( j = 0 ; j < n ; j++ )
      hist[maps[INDEX_2(j,i,m)]]++ ;

  for ( i = 0 ; i <  *px_p * *py_p ; i++ ) printf("%2d.  %5d\n",i,hist[i]) ;

  new_mapping = 0 ;
  for ( j = 0 ; j < n ; j++ )
    for ( i = 0 ; i < m ; i++ )
      w2[INDEX_2(j,i,m)] = maps[INDEX_2(j,i,m)] ;
  retval = 0 ;

  RSL_FREE (maps) ;
  RSL_FREE (agg) ;

    fprintf(stderr,"Returning %d from mapping\n", retval) ;
  return(retval) ;
}


