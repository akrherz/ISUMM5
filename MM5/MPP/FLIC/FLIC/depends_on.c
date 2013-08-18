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
#include <stdlib.h>
#include "n32.h"
#include "sym.h"
#include "dm.h"
#include "twst.h"

NodePtr statement_node ( node )
  NodePtr node ;
/* 2001 01 09; return the node that is the parent statement node.
   This was added because of a problem that turned up when we tried 
   running FLIC on code that had been processed by TAMC. TAMC sometimes
   split the idices of a variable reference across two lines so that the
   index line number would be greater than the line number of the array
   reference, and the test "linex <= currline" below would fail. The 
   correct thing is to base the line numbers on the line number of the
   statement. So this routine traces back through the parent pointers
   of a node until it finds the enclosing statement node, and then returns
   this. The program can then determine the line number from looking at
   the first token of this statement node */
{
  if ( node == NULL ) return ( NULL ) ;
  if ( STATEMENT( node ) ) return ( node ) ;
  return ( statement_node ( PARENT ( node ) ) ) ;
}

void *firsttok(), *gettype() ;

int
depends_on( expr, LI, currline, retnode, sw )
  NodePtr expr ;
  char * LI ;
  int currline ;
  NodePtr retnode[2] ; /* returns the dependent(s) */
  int sw ; /* if sw != 0 check logical expressions */

  /* This switch determines whether we're calling the routine on an
  expression that we expect to be logical (at some level) or for an
  expression that we do not expect to be logical.  Return the
  expression that has been determined to have a dependence in it.  If
  the switch is set to zero, then we should ignore expressions that are
  logical.  If it is 1, then the expression is known to be a logical
  expression, but we want to get at the arithmetic operands and test
  their dependence.  In this case, we want to return the topmost
  arithmetic node that has a dependence in it.  */

{
  NodePtr firstt, firstt2, set, nam, stmnt_node ;
  NodePtr re[DEPENDS_MAXRET] ;
  sym_nodeptr x ;
  int linex, retval, itok, r, reti ;
  int notlogical ;
  int dum ;
  int i ;

  for ( itok = 0 ; itok < DEPENDS_MAXRET ; itok++ ) re[itok] = NULL ;
  retval = 0 ;
  reti = 0 ;
  *retnode = NULL ;
  if ( expr == NULL ) return(0) ;                          /* RETURN */

  linex = 0 ;

  if ( (stmnt_node = statement_node ( expr )) ==  NULL )
    stmnt_node = expr ;

  if ((firstt = (NodePtr) firsttok(stmnt_node)) != NULL )
    linex = atoi(TOKBEGIN(firstt)) ;
#if 0
fprintf(stderr,"Depends on %s currline %d linex %d\n  ",LI,currline, linex) ;
print_info(stderr,expr) ;
#endif
  if      (MINORKIND(expr) == binopK)
  {
    if ((notlogical=strcmp(gettype(expr),"logical")) || sw )
    {
      *retnode = expr ;
      r = depends_on(CHILD(expr,0),LI,linex,re,sw) ;
      if ( r == 1 )
      {
	retval = 1 ;
	if (sw && !notlogical)
	  for ( itok = 0 ; itok < DEPENDS_MAXRET ; itok++ )
          {
	    if ( re[itok] != NULL )
	    { retnode[reti++] = re[itok] ;
            }
          }
      }
      r = depends_on(CHILD(expr,2),LI,linex,re,sw) ;
      if ( r == 1 )
      {
	retval = 1 ;
	if (sw && !notlogical)
	  for ( itok = 0 ; itok < DEPENDS_MAXRET ; itok++ )
          {
	    if ( re[itok] != NULL )
            { retnode[reti++] = re[itok] ;
            }
          }
      }
    }
  }
  else if   (MINORKIND(expr) == unopK)
  {
    if ((notlogical=strcmp(gettype(expr),"logical")) || sw )
    {
      *retnode = expr ;
      r = depends_on(CHILD(expr,1),LI,linex,re,sw) ;
      if ( r == 1 )
      {
	retval = 1 ;
	if (sw && !notlogical)
	  for ( itok = 0 ; itok < DEPENDS_MAXRET ; itok++ )
	    if ( re[itok] != NULL ) retnode[reti++] = re[itok] ;
      }
    }
  }
  else if ( MINORKIND(expr) == idrefK )
  {
   if ( linex <= currline )
   {
    if ( getclass(expr) == VARIABLE || getclass(expr) == PARAMETER )
    {
      *retnode = CHILD(expr,0) ;
      if (!strcmp(LI,TOKSTRING(CHILD(expr,0))))
      {
        return(1) ;  /* RETURN */
      }
      if ((x = sym_get(TOKSTRING(CHILD(expr,0)))) != NULL )
      {
        if ((set = (NodePtr) x->assigned) != NULL)
        {
          firstt2 = (NodePtr) firsttok(set) ;

	  /* this gives us the first token of the node for the
	     statement that sets the variable we're tracing.  If the
	     node is an expression of itself (say, a counter being
	     incremented II = II+1) an infinite loop may occur.  Just
	     return that it doesn't depend on a loop variable, since
	     the program shouldn't be computing a loop variable anyway.  The
	     only problem with that reasoning is that it is all right
	     to do arithmetic on something that was set from the loop
	     variable.  In this case, however, we're just gonna say
	     "don't do that." */

          if ( atoi(TOKBEGIN(firstt2)) < currline )
	  {
            switch (MINORKIND(set))
            {
            case doK :
              nam = CHILD(set,4) ;
              if (!strcmp(LI,TOKSTRING(nam))) 
              {
                retval = 1 ;
              }
              break ;
            case doenddoK :
              nam = CHILD(set,3) ;
              if (!strcmp(LI,TOKSTRING(nam)))
              {
                retval = 1 ;
              }
              break ;
            case assignK :
                                                              /* RECURSE */
              retval =
		 depends_on(CHILD(set,3),LI,atoi(TOKBEGIN(firstt2)),re,sw) ;
              break ;
            default :
              break ;
            }
	  }
        }
      }
    }
   }
  }
  else
  {
    for ( i = 0 ; i < maxChildren ; i++ )
    {
      if ( depends_on(CHILD(expr,i),LI,linex, re, sw ) == 1 )
      {
	for ( itok = 0 ; itok < DEPENDS_MAXRET ; itok++ )
        {
          retnode[itok] = re[itok] ;
        }
        retval = 1 ;
      }
    }
  }
  return(retval) ;                                         /* RETURN */
}



