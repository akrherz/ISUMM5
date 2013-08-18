#include <stdio.h>
#include <stdlib.h>
#ifndef STUBS
#define MAXDOM_MAKE  1
#define MAXPROC_MAKE 128
#include "rsl.h"
#endif

#ifdef NOUNDERSCORE 
# ifndef T3E
kill_model ( )
# else
KILL_MODEL ( )
# endif
#else
# ifdef F2CSTYLE
kill_model__ ( )
# else
kill_model_ ( )
# endif
#endif
{
#ifndef STUBS
  MPI_Abort( MPI_COMM_WORLD, 9 ) ;
#else
  exit(9) ;
#endif
}

