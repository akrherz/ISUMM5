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
/* f_expressions.c SCCS(4.1 91/02/18 15:39:43) */


/* f_expressions.c 

   routines that go with grammar rules in f.expressions

*/

#include <stdio.h>
#include "node.h"
#include "f_y.h"

NodePtr
make_idref_node( A, B, C )
NodePtr A,	/* name */
	B,	/* subscript (maybe null) */
	C ;	/* substring (maybe null) */
		/* dot (null if E null) */
		/* dotted child (maybe null) */
{
    return( three_kids( newExNode( idrefK ), A, B, C )  ) ;
}

NodePtr
make_dot_node( A, B, C )
NodePtr A,	/* lhs of dot */
	B,	/* Dot token */
	C ;	/* rhs of dot */
{
    return( three_kids( newExNode( dotK ), A, B, C )) ;
}

NodePtr
fieldref_fix( A )
NodePtr A ;	/* 3rd child of dotK -- all idrefs in here get
		   changed to fieldrefs */
{
    if ( A == NULL ) return(A) ;
    if ( A->GENERIC(GenericKind) == fieldrefK ) {
	return(A) ;
    }
    if ( A->GENERIC(GenericKind) == idrefK ) {
	A->GENERIC(GenericKind) = fieldrefK ;
	return(A) ;
    }
    if ( A->GENERIC(GenericKind) == dotK ) {
	fieldref_fix( A->nodeChild[0] ) ; /* recurse lhs */
	fieldref_fix( A->nodeChild[2] ) ; /* recurse rhs */
	return(A) ;
    }
    ast_err("bad node depending from dotK",A) ;
    comp_err("fieldref_fix(): bad node depending from dotK" ) ;
}

NodePtr
make_exparen_node( A, B, C )
NodePtr A,      /* ( */
        B,      /* whatever */
        C ;     /* ) */
{
    return( three_kids( newExNode( parenK ), A, B, C )  ) ;
}


NodePtr
make_substring_part( A, B, C, D, E )
NodePtr A,	/* ( */
	B,	/* expression (maybe null) */
	C,	/* colon */
	D, 	/* expression (maybe null) */
	E ;	/* ) */
{
    return( five_kids( newExNode( substringpartK), A, B, C, D, E )  ) ;
}

NodePtr
make_exbinop_node( A, B, C )
NodePtr A,      /* lhs */
        B,      /* operator */
        C ;     /* rhs */
{
    return( three_kids(newExNode( binopK ), A, B, C )  ) ;
}

NodePtr
make_exunop_node( A, B )
NodePtr A,      /* operator */
        B ;     /* operand */
{
    return( two_kids( newExNode( unopK ), A, B )  ) ;
}

NodePtr
make_complex_const( A, B, C, D, E )
NodePtr A,	/* ( */
	B,	/* expression */
	C,	/* , */
	D, 	/* expression */
	E ;	/* ) */
{
    return( five_kids( newExNode( complexconstK ), A, B, C, D, E )  ) ;
}

