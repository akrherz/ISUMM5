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
new_module( node )
  NodePtr node ;
{
    NodePtr p ;
    int index ;
    loop_t * LOOP ;
    char tempstr[2096] ;
    void * my_malloc() ;

    sym_init() ;
    read_config() ;

    walk_statements( node, NULL, mark_actual_arguments, 0 ) ;

    part = DECLARATIONS ;
    loop_level = 0 ;
    LOOP = (loop_t *)my_malloc(sizeof(loop_t)) ;
    LOOP->ARLIST = NULL;
    LOOP->LI_IS = 0 ;
    LOOP->loop = NULL ;
    strcpy(LOOP->LI,"NOT_IN_LOOP") ; 
    LOOPLIST = LOOP ;
    if (node == NULL)  return ;
                                /* get name of this module from child */
    tvarcnt = 0 ;
    statement_line_number = -1 ;
    first_e = NULL ;
    last_d  = NULL ;
    thisif  = NULL ;
    iflev  = 0 ;
    p = node ;
    if (( p = CHILD( p, 0 )) != NULL )
    {
        first_statement = SIBLING(p) ;  /* 970225 */
        /* set index to the correct child number for the type of program
           module this is.  All but function nodes have the name of the 
           module at child 2.  Function nodes are at child 3.  */
        switch ( MINORKIND( p ) )
        {
        case functionK : 
                strcpy( modulekind, "function" ) ;
                index = 3 ;
                break ;
        case mainK :
                strcpy( modulekind, "program" ) ;
                index = 2 ;
                break ;
        case blockdataK :
                strcpy( modulekind, "blockdata" ) ;
                index = 2 ;
                break ;
        case subroutineK :
/*              if ( sw_addargs ) */
                strcpy( modulekind, "subroutine" ) ;
                index = 2 ;
                break ;
        }
        if (( p = CHILD( p, index )) != NULL )
        {
            strcpy( modulename, TOKSTRING( p ) ) ;
        }
        else
        {
            strcpy( modulename, "unnamed" ) ;
        }
    }
    else
    {
        user_err( "AST error", node ) ;
    }
    /* 
        Figure out if this module has an inside_m or
        inside_n specification from the command line
        or from the environment (see also dm.c).
        Sytax is comma separated list of [module:]var
        (no spaces).
    */
    set_inside_sw( sw_IN_M_LOOP, modulename, &in_mvar ) ;
    set_inside_sw( sw_IN_N_LOOP, modulename, &in_nvar ) ;

    /* bug -- right now this allows for only one external loop!
       If N is set it will supercede N in the logic below. */
    if ( in_mvar != NULL ) 
    {
      sprintf(tempstr,"define(INSIDE_MLOOP)\n") ;
      strcat( header, tempstr ) ;
      strcpy(LOOP->LI,in_mvar) ;
      LOOP->LI_IS = MDIM ;
    }
    if ( in_nvar != NULL )
    {
      sprintf(tempstr,"define(INSIDE_NLOOP)\n") ;
      strcat( header, tempstr ) ;
      strcpy(LOOP->LI,in_nvar) ;
      LOOP->LI_IS = NDIM ;
    }
    do_header(node) ;

    /*printf("%s|%s|%s\n",sw_IN_N_LOOP,modulename,in_nvar) ;*/
}

set_inside_sw( SW, module, sw )
  char * SW ;               /* input string to be parsed */
  char * module ;           /* name of module */
  char ** sw ;              /* output string or NULL */
{
  char *p,*q, tempstr[2096] ;
  void * my_malloc(), * strtok(), *index() ;

  *sw = NULL ;
  if ( SW == NULL ) return ;                /*RETURN*/
  if ( strlen(SW) == 0 ) return ;		/* RETURN */
  strcpy( tempstr, SW ) ;
  for (p=(char*)strtok(tempstr,",");p!=NULL;p=(char*)strtok(NULL,","))
  {
    if ((q=(char*)index(p,':')) != NULL)
      { *q++ = '\0' ; }
    else
      { q = p ; p = NULL ; }
    /* at this point p points to the module name if one is specified, */
    /* and q points to the var name. */
    if      ( p == NULL )
      { *sw = (char*)my_malloc(strlen(q)+1) ; strcpy(*sw,q) ; }
    else if (!strcmp(p,module))
      { *sw = (char*)my_malloc(strlen(q)+1) ; strcpy(*sw,q) ; return ; }
  }
  return ;                                  /*RETURN*/
}


