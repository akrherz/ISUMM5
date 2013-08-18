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
/* symbol_table.c SCCS(4.1 91/02/18 15:39:57) */

/* symbol_table.c

    Implementation dependent routines for using symbol_table_generic.c
    in this parser.

*/

#include <stdio.h>
#include "node.h"

extern SymPtr symget() ;

static char ** symtab ;  /* 2-19-90 */


init_sym() /* 2-19-90, initialize symbol table package */
{
    create_ht( &symtab ) ;
    if (symtab == NULL) comp_err("init_sym(): could not create hash table") ;
}

char **
yield_sym()  /* 2-19-90, return a pointer to the hash table in use so that
		it may be saved or whatever the caller wants to do with it. */
{
    return(symtab) ;
}

/* Routine called from parser */

SymPtr
SYMGET( name )
char * name ;
{
    return( symget( name, newnode, nodename, nodenext, symtab, 1 ) ) ;
}

/* added 2-19-90, SYMGETp will allows the caller to provide a hash table,
   rather than SYMGET, which uses the one that's in current use. */
SymPtr
SYMGETp( name, ht )
char * name ;
char ** ht  ;
{
    if (ht == NULL) comp_err("SYMGETp() called with invalid hash table pointer") ;
    return( symget( name, newnode, nodename, nodenext, ht ) ) ;
}

SymPtr
newnode()
{
    SymPtr myMalloc(), p ;
    p = myMalloc( sizeof( struct symnode ) ) ;
    p->name = NULL ;
    p->next = NULL ;
    p->label_do_refs = 0 ;

    p->type = newTypeNode() ;   /* symbol table node carries ptr to type node */

    p->type->TYPE(type) = NULL ;	/* initialize type node fields */
    p->type->TYPE(dimensions) = NULL ;
    p->type->TYPE(common) = NULL ;
    p->type->TYPE(class) = UNKNOWN ;
    p->type->TYPE(storage) = UNKNOWN ;
    p->type->TYPE(lhs) = FALSE ;

    return( p ) ;
}

char **
nodename(p)
SymPtr p ;
{
    char ** x ;
    x = &(p->name) ;
    return( x ) ;
}

SymPtr *
nodenext(p)
SymPtr p ;
{
    SymPtr *x ;
    x = &(p->next) ;
    return( x ) ;
}

