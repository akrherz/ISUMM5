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
/* traverse.c SCCS(4.1 91/02/18 15:40:07) */

/* traverse.c

    Routines for traversing the AST in various ways and applying
    functions to each node in the traversal.

*/

#include <stdio.h>
#include "tokens.h"
#include "node.h"

/*************************************************************/
/* walk_depth()

    This routine covers all nodes of an abstract syntax tree in a depth
    first traversal.  It applies the user supplied function f to each
    node in the traversal.

*/

walk_depth( node, f, order )
NodePtr node   ;
void 	(*f)() ;
int order      ;	/* -1 = preorder, 1 = postorder (DEFAULT) */
{
    register int i ;
    register NodePtr p ;

    if ((p = node) == NULL) return ;

    if (order == -1) (*f)( node ) ;

    for ( i = 0 ; i < maxChildren ; i++ ) {
	if (p->nodeChild[i] != NULL ) {
	    walk_depth( p->nodeChild[i], f, order ) ;
	}
    }

    if (order != -1) (*f)( node ) ;

    if (p->nodeSibling != NULL) {
        walk_depth( p->nodeSibling, f, order ) ;
    }
}

    



/*************************************************************/
/* walk_statements()

    This routine gives a traversal of an AST at the top-level.
    Specifically, it touches only the linked list of programK
    nodes and, for each programK node, the linked list of 
    statement nodes pointed to by child 0 of the programK nodes.

    If children of the statement nodes are to be handled, it must
    be done by the functions passed to walk_statements() themselves.
    The f1 parameter is a pointer to a function to be applied
    to programK nodes as they come up.  The f2 is a pointer to 
    the function to be applied to the statement nodes.  Either
    may be null.

    Returns 0 on success.
*/

walk_statements( ast, f1, f2, depth )
NodePtr	ast ;		/* pointer to abstact syntax tree */
void 	(*f1)(), 	/* pointer to function for programK nodes */
	(*f2)() ;	/* pointer for statement nodes in modules */
int 	depth ;		/* level of nesting to go into ifs and dos */
{
    NodePtr p, q ;

    /* outer loop traverses the linked list of programK nodes */
    for ( p = ast ; p != NULL ; p = p->nodeSibling ) {

	if ( p->GENERIC( GenericKind ) != programK ) {
	   ast_err("walk_statements(): programK expected",p) ;
	   return(1) ;
	}

	if ( f1 != NULL ) (*f1)(p) ;	/* call f1 on programK node */

	/* inner loop traverses linked list of statement nodes
	   depending from each programK node examined on outer loop */

	walk_statements1( p->nodeChild[0], f2, depth, 0 ) ;

    }
}

walk_expression( node, f1, f2 )
NodePtr node ;		/* pointer to root node of expression */
void (*f1)(),		/* pointer to function for operator nodes */
     (*f2)() ;		/* pointer to function for operand nodes */
{
	if ( node == NULL ) return ;

	switch ( MINORKIND( node ) )
	{
	case binopK :
	    walk_expression( CHILD( node, 0), f1, f2 ) ;
	    if ( f1 != NULL ) (*f1)( CHILD(node,1)) ;
	    walk_expression( CHILD( node, 2), f1, f2 ) ;
	    break ;
	case unopK :
	    if ( f1 != NULL ) (*f1)( CHILD(node,0)) ;
	    walk_expression( CHILD( node, 1), f1, f2 ) ;
	    break ;
	case parenK :
	    walk_expression( CHILD( node, 1), f1, f2 ) ;
	    break ;
	case TICON :
	case THEXCON :
	case TOCTCON :
	case TRCON :
	case TDCON :
	case THOLLERITH :
	case TSTRING :
	case complexconstK :
	case idrefK :
	case dotK :
	    if ( f2 != NULL ) (*f2)( node ) ;
	    break ;
	default :
	    break ;
	}
	return ;
}

int walk_level ;	/* user accessible nesting counter */

walk_statements1( node, f2, depth, level )
NodePtr node ;           /* pointer to abstact syntax tree */
void    (*f2)() ;       /* pointer for statement nodes in modules */
int     depth ;         /* how deep nesting to go into ifs and dos */
int	level ;		/* current level */
{
	NodePtr q ;

	if ( node == NULL || level > depth ) return ;


	for ( q = node; q != NULL ; q = q->nodeSibling ) {
	    walk_level = level ;
	    if ( f2 != NULL ) {
		(*f2)(q) ;  /* call f2 on statement node */
	        switch ( q->GENERIC(GenericKind) ) {
		    case iflogicalK : 
			/* consider statement on next level down */
			walk_statements1( q->nodeChild[5], f2, 
					  depth, level+1 );
			break ;
		    case ifthenelseK :
			walk_statements1( q->nodeChild[6], f2,
					  depth, level+1 ) ;
			walk_statements1( q->nodeChild[8], f2,
					  depth, level+1 ) ;
		     	break ;
		    case ifthenelseifK :
			walk_statements1( q->nodeChild[6], f2,
					  depth, level+1 ) ;
			walk_statements1( q->nodeChild[7], f2,
					  depth, level+1 ) ;
		     	break ;
		    case doK :
			walk_statements1( q->nodeChild[7], f2,
					  depth, level+1 ) ;
			break ;
		    case doenddoK :
			walk_statements1( q->nodeChild[6], f2,
					  depth, level+1 ) ;
			break ;
		    case dowhileK :
			walk_statements1( q->nodeChild[5], f2,
					  depth, level+1 ) ;
			break ;
		}
	    }
	}
}
