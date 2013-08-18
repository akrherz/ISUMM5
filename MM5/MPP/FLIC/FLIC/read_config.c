/***********************************************************************
     
                              COPYRIGHT
     
     The following is a notice of limited availability of the code and 
     Government license and disclaimer which must be included in the 
     prologue of the code and in all source listings of the code.
     
     Copyright notice
       (c) 1977  University of Chicago
     
     Permission is hereby granted to use, reproduce, prepare 
     derivative works, and to redistribute to others at no charge.  If 
     you distribute a copy or copies of the Software, or you modify a 
     copy or copies of the Software or any portion of it, thus forming 
     a work based on the Software and make and/or distribute copies of 
     such work, you must meet the following conditions:
     
          a) If you make a copy of the Software (modified or verbatim) 
             it must include the copyright notice and Government       
             license and disclaimer.
     
          b) You must cause the modified Software to carry prominent   
             notices stating that you changed specified portions of    
             the Software.
     
     This software was authored by:
     
     Argonne National Laboratory
     J. Michalakes: (630) 252-6646; email: michalak@mcs.anl.gov
     Mathematics and Computer Science Division
     Argonne National Laboratory, Argonne, IL  60439
     
     ARGONNE NATIONAL LABORATORY (ANL), WITH FACILITIES IN THE STATES 
     OF ILLINOIS AND IDAHO, IS OWNED BY THE UNITED STATES GOVERNMENT, 
     AND OPERATED BY THE UNIVERSITY OF CHICAGO UNDER PROVISION OF A 
     CONTRACT WITH THE DEPARTMENT OF ENERGY.
     
                      GOVERNMENT LICENSE AND DISCLAIMER
     
     This computer code material was prepared, in part, as an account 
     of work sponsored by an agency of the United States Government.
     The Government is granted for itself and others acting on its 
     behalf a paid-up, nonexclusive, irrevocable worldwide license in 
     this data to reproduce, prepare derivative works, distribute 
     copies to the public, perform publicly and display publicly, and 
     to permit others to do so.  NEITHER THE UNITED STATES GOVERNMENT 
     NOR ANY AGENCY THEREOF, NOR THE UNIVERSITY OF CHICAGO, NOR ANY OF 
     THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR 
     ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, 
     COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, 
     PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD 
     NOT INFRINGE PRIVATELY OWNED RIGHTS.

***************************************************************************/
#include <stdio.h>
#include "n32.h"
#include "sym.h"
#include "dm.h"


sym_nodeptr
store_dim( dimstr )
  char * dimstr ;
{
  void* my_malloc() ;

  if ( dimstr == NULL ) return ;
  if ( *dimstr == '\0' ) return ;
  if ( cursor_dimtab >= MAXDIMSPECS )
  {
    fprintf(stderr,
     "DM: %d too many dimensions specified.\n",cursor_dimtab) ;
    fprintf(stderr,
     " Modify the the constant MAXDIMSPECS in dm.h and recompile.\n") ;
    exit(2) ;
  }
  dimtab[cursor_dimtab] = (char*)my_malloc(strlen(dimstr)+1) ;
  strcpy(dimtab[cursor_dimtab],dimstr) ;
  cursor_dimtab++ ;
  return(sym_add(dimstr)) ;
}

read_config()
{
  sym_nodeptr x ;
  char * p ;
  int i ; 
  char mdstr[8192] ;
  char ndstr[8192] ;
  void*strtok();
  void*strtok();

  strcpy(mdstr,mdimstr) ;
  strcpy(ndstr,ndimstr) ;

  cursor_dimtab = 0 ;
  for ( i = 0 ; i < MAXDIMSPECS ; i++ )
    dimtab[i] = NULL ;

  for (p = (char *)strtok(mdstr,",") ;
       p != NULL ;
       p = (char *)strtok(NULL,","))
  {
    x = store_dim( p ) ; x->dim = MDIM ;
  }
  for (p = (char *)strtok(ndstr,",") ;
       p != NULL ;
       p = (char *)strtok(NULL,","))
  {
    x = store_dim( p ) ; x->dim = NDIM ;
  }

#if 0
  x = store_dim( "mix" ) ; x->dim = MDIM ;
  x = store_dim( "mixm" ) ; x->dim = MDIM ;
  x = store_dim( "mixa" ) ; x->dim = MDIM ;
  x = store_dim( "mixr" ) ; x->dim = MDIM ;
  x = store_dim( "mixs" ) ; x->dim = MDIM ;
  x = store_dim( "mixic" ) ; x->dim = MDIM ;
  x = store_dim( "mixicg" ) ; x->dim = MDIM ;
  x = store_dim( "mixig" ) ; x->dim = MDIM ;
  x = store_dim( "mixfo" ) ; x->dim = MDIM ;
  x = store_dim( "mixfg" ) ; x->dim = MDIM ;
  x = store_dim( "mixnh" ) ; x->dim = MDIM ;
  x = store_dim( "mixc" ) ; x->dim = MDIM ;
  x = store_dim( "mixv" ) ; x->dim = MDIM ;
  x = store_dim( "id3" ) ; x->dim = MDIM ;     /* dots.F */

  x = store_dim( "mjx" ) ; x->dim = NDIM ;
  x = store_dim( "mjxm" ) ; x->dim = NDIM ;
  x = store_dim( "mjxa" ) ; x->dim = NDIM ;
  x = store_dim( "mjxr" ) ; x->dim = NDIM ;
  x = store_dim( "mjxs" ) ; x->dim = NDIM ;
  x = store_dim( "mjxic" ) ; x->dim = NDIM ;
  x = store_dim( "mjxicg" ) ; x->dim = NDIM ;
  x = store_dim( "mjxig" ) ; x->dim = NDIM ;
  x = store_dim( "mjxfo" ) ; x->dim = NDIM ;
  x = store_dim( "mjxfg" ) ; x->dim = NDIM ;
  x = store_dim( "mjxnh" ) ; x->dim = NDIM ;
  x = store_dim( "mjxc" ) ; x->dim = NDIM ;
  x = store_dim( "mjxv" ) ; x->dim = NDIM ;
  x = store_dim( "id4" ) ; x->dim = NDIM ;     /* dots.F */
#endif
}

