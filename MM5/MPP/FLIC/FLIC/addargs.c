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

void *malloc(), *newTokNode() ;

addargs( node )
  NodePtr node ;
{
  NodePtr p, q, prev_p ;
  sym_nodeptr x ;
  int childno ;
  char tmp[256], tmp2[256] ;
 
  if ( node == NULL ) return ;
  if ( ! (MINORKIND(node) == subroutineK ||
          MINORKIND(node) == functionK) ) return ;
  childno = (MINORKIND(node) == functionK)?4:3;
  if ((p = CHILD(node,childno)) == NULL ) return;
  if ( ! MINORKIND(p) == dummyargsK ) return ;
  if ((q = CHILD(p,1)) == NULL ) 
  {
    /* in case no arguments */
    if ( sw_addgenericargs )
    {
      NodePtr NewNode ;
      NewNode = newExNode( 9009 ) ;
      PARENT(NewNode) = p ;
      SIBLING(NewNode) = NULL ;
      ELDER(NewNode) = NULL ;
      MINORKIND(NewNode) = 9009 ;    /* morph the node */
      sprintf(tmp,"D_DUMARG_G") ;
      NewNode->user_def = (NodePtr) malloc(strlen(tmp)+1) ;
      strcpy(NewNode->user_def,tmp) ;
      CHILD(p,1) = NewNode ;
    }
    return ;
  }
  for ( p = q ; p != NULL ; prev_p = p , p = SIBLING(p) )
  {
    if ( MINORKIND(p) == TNAME ) 
    {
      strcpy( tmp2, TOKSTRING(p) ) ;
      if ((x = sym_get( tmp2 )) != NULL )
      {
        if      ( x->dim == MDIM || x->dim == NDIM )
        {
          MINORKIND(p) = 9009 ;    /* morph the node */
	  sprintf(tmp,"D_DUMARG_%c(%s)",(x->dim==MDIM)?'M':'N',tmp2) ;
	  p->user_def = (NodePtr) malloc(strlen(tmp)+1) ;
	  strcpy(p->user_def,tmp) ;
        }
      }
    }
  }
  p = prev_p ;
  if ( p != NULL )
  {
    if ( SIBLING(p) == NULL )
    {
      NodePtr NewNode ;
      TokInfoPtr q ;
      q = (TokInfoPtr)malloc(sizeof(*q)) ;
      NewNode = (NodePtr) newTokNode( q ) ;
      SIBLING(NewNode) = NULL ;
      ELDER(NewNode) = p ;
      MINORKIND(NewNode) = 9009 ;    /* morph the node */
      sprintf(tmp,",D_DUMARG_G") ;
      NewNode->user_def = (NodePtr) malloc(strlen(tmp)+1) ;
      strcpy(NewNode->user_def,tmp) ;
      SIBLING(p) = NewNode ;
    }
  }
}

