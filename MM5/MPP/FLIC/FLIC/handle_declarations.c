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

static int dimstat = 0 ;  /* handle_declarations sets to 1 if iddecl node is
                             found in a dimension statment */

handle_declarations( node )
  NodePtr node ;
{
  NodePtr p, elder ;
  int i ;

  if (node == NULL)  return ;
  dimstat = 0 ;
  switch (MINORKIND(node))
  {
  case functionK:
  case subroutineK:
    if ( sw_addargs || sw_addgenericargs ) addargs( node ) ;
    break ;
  case commonK :
    handle_declarations( CHILD( node, 2 )) ; /* recurse on morecommonK node */
    break ;
  case morecommonK :
    for ( p = node ; p != NULL; p = SIBLING(p) )
    {
      if ( MINORKIND(p) == TCOMMA ) continue ;
      handle_iddecl_list( CHILD( p, 3 )) ;
    }
    break ;
  case dimensionK :
    dimstat = 1 ;
  case typedeclK :
    handle_iddecl_list( CHILD( node, 2 )) ;
    break ;    
  case pointerK :
    handle_pointer_list( CHILD(node,2) ) ;
    break ;
  default :
    break ;
  }
  /* get rid of dangling dimension statements that may result from
     collapsing dimensions */
  if (MINORKIND(node) == dimensionK && ( sw_collapse_m || sw_collapse_n ))
  {
    if (CHILD(node,2)==NULL)
    {
      if (elder=ELDER(node))
      {
        SIBLING(elder) = SIBLING(node) ;
        if (SIBLING(node))
          ELDER(SIBLING(node)) = elder ;
      }
      else /* must be number one */
      {
        /* find me in the parent's list */
        for ( i = 0 ; i < maxChildren ; i++ )
        {
          if ( CHILD(PARENT(node),i) == node )
          {
            CHILD(PARENT(node),i) = SIBLING(node) ;
            if (SIBLING(node))
              ELDER(SIBLING(node)) = NULL ;
            break ;
          }
        }
      }
    }
  }
}
    

handle_pointer_list( node )
  NodePtr node ;
{
  int idx ;
  NodePtr p, paren, paren2, idref, subs, subitem ;
  sym_nodeptr x, id ;

  if ( node == NULL ) return ;
  for ( paren = node ; paren != NULL ; paren=SIBLING(paren) )
  {
    if (MINORKIND(paren) == TCOMMA ) continue ;
    handle_iddecl_list( CHILD( paren, 1 )) ;
  }
}

handle_iddecl_list( node )
  NodePtr node ;
{
  int idx, snip, storage ;
  NodePtr p, iddecl, subscript, subitem ;
  NodePtr elder, sib ;
  sym_nodeptr x, id ;
  SymPtr sym ;
  void *getsym(), *malloc() ;

  if ( node == NULL ) return ;
  for ( iddecl = node ; iddecl != NULL ; iddecl=SIBLING(iddecl) )
  {
    if ( MINORKIND(iddecl) == TCOMMA ) continue ;
    if ( /* there are subscripts */ (subscript=CHILD(iddecl,1)) != NULL )
    {
      id = sym_add(TOKSTRING(CHILD(iddecl,0))) ;
     /* explanation: getsym is N32's symbol table is part of the parser),
        whereas sym_add and sym_get are a dm-specific symbol table.  We're
        accessing the parser symtab here to get at storage info for the
        variable in question  */
      sym = (SymPtr) getsym( iddecl, 0 ) ;
      storage = getstorage(sym->type) ;
      idx = 0 ;
      for ( subitem = CHILD(subscript,1) ; subitem != NULL ; subitem = SIBLING(subitem) )
      {
        if ( MINORKIND(subitem) == TCOMMA ) continue ;
        if ( CHILD(subitem,2) != NULL )
        {
          if ( MINORKIND(CHILD(subitem,2)) == idrefK )
          {
            /* get the symbol table entry for the defined constant and see what 
               dimension it represents */
            if ((x = sym_get(TOKSTRING(CHILD(CHILD(subitem,2),0)))) != NULL )
            {
              if ( sw_addargs && CHILD(subitem,0) == NULL )
              {
                NodePtr q ;
                char tmp[256] ;
                q = CHILD(CHILD(subitem,2),0) ; /* morphing q, a TNAME node */
                MINORKIND(q) = 9009 ;
                sprintf(tmp,"D_DECL_%c(%s)",x->dim==MDIM?'M':'N',
                                            TOKSTRING(q)) ;
                q->user_def = (NodePtr)malloc(strlen(tmp)+1) ;
                strcpy(q->user_def,tmp) ;
              }

              id->dims[idx] = x->dim ;
              strcpy(id->dimname[idx],TOKSTRING(CHILD(CHILD(subitem,2),0))) ;
              snip = 0 ;
              if ( x->dim == MDIM ) 
              {
                id->MDEX = idx ;
		/* variables that are used in calls or IO statement
  		   would have been marked at this point.  Marking is
                   done in a pre-traversal initiated in dm.c  */
                if ( sw_collapse_m && storage == LOCAL &&
                     id->marked != MARKED_VAL ) snip = 1 ;
              }
              if ( x->dim == NDIM ) 
              {
                id->NDEX = idx ;
                if ( sw_collapse_n && storage == LOCAL &&
                     id->marked != MARKED_VAL ) snip = 1 ;
              }
              if ( snip )
              {
                strcpy(id->dimname[idx],"REMOVED") ;
                /* snip it out of the list */
                if (elder=ELDER(subitem))
                {
                  SIBLING(elder) = SIBLING(subitem) ;
                  if (SIBLING(subitem))
                    ELDER(SIBLING(subitem)) = elder ;
                }
                else /* must be number one */
                {
                  CHILD(PARENT(subitem),1) = SIBLING(subitem) ;
                  if (SIBLING(subitem)) 
                    ELDER(SIBLING(subitem)) = NULL ;
                }
              }
            }
          }
        }
        idx++ ;
      }
      id->ndims = idx ;
      if ( sw_collapse_m || sw_collapse_n )
      {
        /* go through and get rid of unwanted leavings like strings of commas 
           or () -- remove declarations that have demoted down to scalar from
           dimension statements.  Will need to look at getting rid of dangling
           dimension statements as well. */
        for ( subitem = CHILD(subscript,1) ; subitem != NULL ; subitem = SIBLING(subitem) )
        {
          if ( MINORKIND(subitem) == TCOMMA ) 
          {
            if (elder=ELDER(subitem))
            {
              if (SIBLING(subitem))
              {
                if (MINORKIND(SIBLING(subitem)) == TCOMMA)
                {
                  SIBLING(elder) = SIBLING(subitem) ;
                  if (SIBLING(subitem))
                    ELDER(SIBLING(subitem)) = elder ;
                }
              }
	      else  /* trailing */
	      {
	        SIBLING(elder) = NULL ;
	      }
            }
            else /* must be number one */
            {
              CHILD(PARENT(subitem),1) = SIBLING(subitem) ;
              if (SIBLING(subitem)) 
                ELDER(SIBLING(subitem)) = NULL ;
            }
          }
        }
      }
    }
  }
  if ( sw_collapse_m || sw_collapse_n )
  {
    /* go through and get rid of () on identifiers that have been demoted
       to scalar.  If this is a dimension statement, drop them completely. */
    for ( iddecl = node ; iddecl != NULL ; iddecl=SIBLING(iddecl) )
    {
      if ( MINORKIND(iddecl) == TCOMMA ) continue ;
      if ( /* there are subscripts */ (subscript=CHILD(iddecl,1)) != NULL )
      {
        if ( CHILD(subscript,1) == NULL )  /* there were but no longer */
        {
          if ( dimstat )
          {
            if (elder=ELDER(iddecl))
            {
              SIBLING(elder) = SIBLING(iddecl) ;
              if (SIBLING(iddecl))
                ELDER(SIBLING(iddecl)) = elder ;
            }
            else /* must be number one */
            {
              CHILD(PARENT(iddecl),2) = SIBLING(iddecl) ;
              if (SIBLING(iddecl))
                ELDER(SIBLING(iddecl)) = NULL ;
            }
          }
          else
            CHILD(iddecl,1) = NULL ;
        }
      }
    }
    /* get rid of runs of commas */
    for ( iddecl = node ; iddecl != NULL ; iddecl=SIBLING(iddecl) )
    {
      if ( MINORKIND(iddecl) == TCOMMA )
      {
        if (elder=ELDER(iddecl))
        {
          if (SIBLING(iddecl))
          {
            if (MINORKIND(SIBLING(iddecl)) == TCOMMA)
            {
              SIBLING(elder) = SIBLING(iddecl) ;
              if (SIBLING(iddecl))
                ELDER(SIBLING(iddecl)) = elder ;
            }
          }
	  else  /* trailing */
	  {
	    SIBLING(elder) = NULL ;
	  }
        }
        else /* must be number one */
        {
          CHILD(PARENT(iddecl),2) = SIBLING(iddecl) ;
          if (SIBLING(iddecl))
            ELDER(SIBLING(iddecl)) = NULL ;
        }
      }
    }
  }
}

