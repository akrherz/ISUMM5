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
/* f_main.c SCCS(4.1 91/02/18 15:39:44) */

/* 
   f_main.c
   routines called from various semantic actions specified within 
   the f.main grammar rules file.
*/

#include <stdio.h>
#include "node.h"
#include "f_y.h"


NodePtr
make_program_node( A, B )
NodePtr A, B ;
{
    NodePtr p ;
    void * yield_sym() ;
    p = one_kid( newDefNode( programK ), B ) ;
    p->DEF(u.program.symtab) = (char **) yield_sym() ;
    init_sym() ;
    return( link_nodes( -1, A, p ) ) ; 
}

NodePtr
make_main_node( A, B, C, D  ) 
NodePtr A, B, C, D  ;
{
    return( link_nodes(-1,
	three_kids( newDefNode( mainK ), A, B, C ),
	D )  ) ;
}

NodePtr
make_function_node( A, B, C, D, E, F )
NodePtr A, B, C, D, E, F ;
{
    return( link_nodes(-1,
	five_kids( newDefNode( functionK ), A, B, C, D, E ),
	F )  ) ;
}

NodePtr
make_subroutine_node( A, B, C, D, E )
NodePtr A, B, C, D, E ;
{
    return( link_nodes(-1,
    	four_kids( newDefNode( subroutineK ), A, B, C, D ),
	E )  ) ;
}

NodePtr
make_blockdata_node( A, B, C, D )
NodePtr A, B, C, D ;
{
    return( link_nodes(-1,
    	three_kids( newDefNode( blockdataK ), A, B, C ),
	D )  ) ;
}

NodePtr
make_body_list( A, B, C, D)
NodePtr A, B, C, D ;
{
    return( link_nodes(-1,
	A,
	link_nodes( -1,
	    B,
	    link_nodes( -1,
		C,
		D )))  ) ;
}

NodePtr
make_body( A, B )
NodePtr A, B ;
{
    return( link_nodes(-1, A, B )  ) ;
}

NodePtr
make_first_executable( A, B )
NodePtr A, B ;
{
    return( add_label_to_statement(A,B)  ) ;
}

NodePtr
make_definition_statements( A, B )
NodePtr A, B ;
{
    return( add_label_to_statement(A,B)  ) ;
}

NodePtr
make_executable_statements( A, B )
NodePtr A, B ;
{
    return( add_label_to_statement(A,B)  ) ;
}

NodePtr
finish_logicalif_statement( A, B )
NodePtr A, B ;
{
    /* let the statement attached to node child 5 has the
       same label as the if statement */
    if (B->nodeChild[5] != NULL) {
	if (B->nodeChild[5]->nodeChild[0] == NULL) {
	    B->nodeChild[5]->nodeChild[0] = A ;
	}
    }
    return( add_label_to_statement(A,B)  ) ;
}


NodePtr
add_label_to_statement( A, B )
NodePtr A, B ;
{
    if ( B != NULL )
        B->nodeChild[0] = A ;
    else
	syn_err("add_label_to_statement, null statement") ;
    return( B ) ;
}

/* link_nodes() is made necessary by the fact that yacc favors left
   recursion.  Right recursion in grammar rules is permissable but
   it causes the parser's stacks to grow and eventually overflow.
   Thus, most of the grammar rules in this tool are written left
   recursive.  This results in difficulty, though, for putting together
   nodes in their proper order.  The following routine,
   is the fix for this.  The argument child specifies which
   nodeChild to use as the next-pointer in list.  If the 
   child is less than zero, then the sibling pointer is used.
   
   2-20-90: added ability to jump over a great many nodes by
   following the long-sibling pointers.  This will reduce the
   number of page faults from traversing long linked lists of
   nodes through sibling pointers every time we wish to add
   one to the end of the list.
*/
NodePtr
link_nodes(child, A, B )
int child ;
NodePtr A, B ;
{
    char temp[80] ;
    NodePtr p ;
    if (A != NULL) {
	p = A ;
	if (child >= 0) {
	    if (child >= maxChildren) {
		sprintf(temp,"link_nodes, bad child number %d",child) ;
		comp_err(temp) ;
	    }
	    while( p->nodeChild[child] != NULL ) {
	        p = p->nodeChild[child] ;
	    }
            p->nodeChild[child] = B ;
	} else {
	    do {
		while( p->longSibling != NULL ) {
		    p = p->longSibling ;
		}
	        while( p->nodeSibling != NULL ) {
	            p = p->nodeSibling ;
	        }
	    } while ( p->nodeSibling != NULL && p->longSibling !=NULL ) ;
            p->nodeSibling = B ;
	    A->longSibling = B ;
	}
	return(A) ;
    } else {
        return(B) ;
    }
}


