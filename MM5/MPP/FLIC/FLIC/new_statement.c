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
new_statement( node )
  NodePtr node ;
{
  if (node == NULL)  return ;

  if ( part == DECLARATIONS )
  {
    handle_declarations( node ) ;
  }
  if ( MAJORKIND( node ) == defK &&
       MINORKIND( node ) != statfunK &&
       MINORKIND( node ) != formatK )
  {
    last_d = node ;
  }
  if ( MAJORKIND( node ) != defK ) 
  {
    part = EXECUTABLE ;
    if ( first_e == NULL ) first_e = node ;
  }
  if ( part != DECLARATIONS )
  {
    handle_executable( node ) ;
  }
}

/* any idrefs that are actual arguments to a
   function, subroutine  call, or I/O statement
   should not be collapsed */

static mark_idrefs( node )
  NodePtr node ;
{
  sym_nodeptr x, id ;
  int storage ;

  if (MINORKIND(node) == idrefK) 
  {
    if ( getclass(node) == ARRAY && getstorage(node) == LOCAL )
    {
      id = sym_add(TOKSTRING(CHILD(node,0))) ;
      id->marked = MARKED_VAL ;
    }
  }
}

/* we want to mark arguments to function calls which are found
   in assignment statements and whatnot ... */
consider_subtree_to_mark_idrefs( node )
  NodePtr node ;
{
  int class ;
  if (MINORKIND(node) == idrefK) 
  {
    class = getclass(node) ;
    if ( class == EFUNCTION || class == IFUNCTION || class == SFUNCTION )
    {
      walk_depth(CHILD(node,1),mark_idrefs) ;
    }
  }
}

mark_actual_arguments( node )
  NodePtr node ;
{
  switch (MINORKIND(node))
  {
  case callK: 
    walk_depth(CHILD(node,4),mark_idrefs) ;
    break ;
  case assignK:  /* looking for function calls */
    walk_depth(CHILD(node,3),consider_subtree_to_mark_idrefs) ;
    break ;
  case readK:
  case writeK:
    walk_depth(CHILD(node,3),mark_idrefs) ;
    break ;
  default:
    break ;
  }
}



