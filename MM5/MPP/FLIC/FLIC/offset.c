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

void * firsttok() ;

int
offset( expr, LI, currline )
  NodePtr expr ;
  char * LI ;
  int currline ;
{
  NodePtr firstt, firstt2, set, nam ;
  sym_nodeptr x ;
  int linex, retval, itok, r, reti ;
  int notlogical ;
  int dum ;
  int i ;

  retval = 0 ;
  reti = 0 ;
  if ( expr == NULL )
     return(0) ;                                           /* RETURN */

  if ( ! ifold( expr, &retval ))
     return(retval) ;               /* a constant expression; RETURN */

  linex = 0 ;
  if ((firstt = (NodePtr) firsttok(expr)) != NULL )
    linex = atoi(TOKBEGIN(firstt)) ;

  if      (MINORKIND(expr) == binopK)
  {
    if (MINORKIND(CHILD(expr,1)) == TPLUS )
    {
      retval =
	(offset(CHILD(expr,0),LI,currline) + offset(CHILD(expr,2),LI,currline)) ;
    }
    else
    if (MINORKIND(CHILD(expr,1)) == TMINUS )
    {
      retval =
	(offset(CHILD(expr,0),LI,currline) - offset(CHILD(expr,2),LI,currline)) ;
    }
  }
  else if   (MINORKIND(expr) == unopK)
  {
    if (MINORKIND(CHILD(expr,0)) == TMINUS )
    {
      retval = - offset(CHILD(expr,1),LI,currline) ;
    }
  }
  else if ( MINORKIND(expr) == idrefK )
  {
   if ( linex <= currline )
   {
    if ( getclass(expr) == VARIABLE )
    {
      if (!strcmp(LI,TOKSTRING(CHILD(expr,0))))
      {
        return(0) ;                                       /* RETURN */
      }
      if ((x = sym_get(TOKSTRING(CHILD(expr,0)))) != NULL )
      {
        if ((set = (NodePtr) x->assigned) != NULL)
        {
          firstt2 = (NodePtr) firsttok(set) ;
          if ( atoi(TOKBEGIN(firstt2)) < currline )
	  {
            switch (MINORKIND(set))
            {
            case doK :
              nam = CHILD(set,4) ;
              if (!strcmp(LI,TOKSTRING(nam))) 
              {
                retval = 0 ;
              }
              break ;
            case doenddoK :
              nam = CHILD(set,3) ;
              if (!strcmp(LI,TOKSTRING(nam)))
              {
                retval = 0 ;
              }
              break ;
            case assignK :
              retval =
		 offset(CHILD(set,3),LI,atoi(TOKBEGIN(firstt2))) ;
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
  return(retval) ;                                         /* RETURN */
}

