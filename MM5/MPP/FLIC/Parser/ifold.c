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
/* ifold.c SCCS(4.1 91/02/18 15:40:13) */

/* ifold.c

   given an integer expression, fold it if it contains
   only constants and pass back the value.  If it succeeds,
   return 0.  If it fails for any reason, return -1.

*/

#include <stdio.h>
#include "n32.h"

#define IFOLD( A, B )    if ( ifold( A, B ) == -1 ) return(-1)

static int status_code ;  /* for passing back success or fail */

ifold( node, retval )
NodePtr node ;
int *retval ;
{
    NodePtr getparamval() ;
    int lhs, rhs, class ;
    if (node == NULL) return(-1) ;

    switch( MINORKIND( node ) )
    {
    case binopK :
	IFOLD( CHILD(node,0), &lhs ) ;
	IFOLD( CHILD(node,2), &rhs ) ;
        switch( MINORKIND(  CHILD(node,1) ) )
	{
	case TPLUS:  *retval = lhs + rhs ; return(0) ;
	case TMINUS: *retval = lhs - rhs ; return(0) ;
	case TSLASH: *retval = lhs / rhs ; return(0) ;
	case TSTAR:  *retval = lhs * rhs ; return(0) ;
	default:  return(-1) ;
	}
    case unopK :
	IFOLD( CHILD(node,1), &lhs ) ;
        switch( MINORKIND(  CHILD(node,0) ) )
	{
	case TPLUS:  *retval = lhs ; 	return(0) ;
	case TMINUS: *retval = -lhs ; 	return(0) ;
	default:  return(-1) ;
	}
    case parenK :
	IFOLD( CHILD(node,1), &lhs ) ;
	*retval = lhs ;
	return(0) ;
    case TICON :
	*retval = atoi( TOKSTRING( node ) ) ;
	return(0) ;
/*
    case THEXCON :
    case TOCTCON :
*/
    case idrefK :
	if ( getclass(node) == PARAMETER 
	     && !strcmp(gettype(node),"integer"))
	{
	    NodePtr paramvalnode ;	/* will point to parameter value */
	    paramvalnode = getparamval( CHILD( node, 0 ) ) ;
	    IFOLD( paramvalnode, &lhs ) ;
	    *retval = lhs ;
	    return(0) ;
	}
	else
	{
	    return(-1) ;
	}
	break ;
    default :
	return(-1) ;
	break ;
    }
    return(-1) ;
}


NodePtr
getparamval( node )
NodePtr node ;
{
    if (node == NULL) return(NULL) ;
    if (node->u.Token == NULL) return(NULL) ;
    if (node->u.Token->symptr == NULL) return(NULL) ;
    if (node->u.Token->symptr->type == NULL) return(NULL) ;

    return( node->u.Token->symptr->type->TYPE(paramval) ) ;
}
