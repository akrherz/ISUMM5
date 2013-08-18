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
/* %M% SCCS(%R%.%L% %D% %T%) */


/* 
   f_executables.c
   routines called from various semantic actions specified within 
   the f.executables grammar rules file.
   In many cases, here, the first child is left null.  A label
   will be attached to this child further up in the parse.
*/

#include <stdio.h>
#include "node.h"
#include "f_y.h"

NodePtr
make_assignment_statement(A,B,C)
NodePtr A, 	/*  left hand side */
	B,	/*  equal sign */
	C ;	/*  right hand side */
{
    /* at this point, since we cannot ascertain for certain
       what is intended, we set these as major kind defK nodes
       so that if they represent statement function declarations, 
       they will not, inadvertantly, toggle type checking routines
       into executable.  Later, during type checking, these will
       be changed to exK nodes when it is determined for certain
       that they really are assignment statements. 3-27-90 */
    return( four_kids( newDefNode( assignK ), NULL,
	A, B, C )  ) ;
}

NodePtr
make_label_assignment( A, B, C, D )
NodePtr A,	/* assign keyword */
	B,	/* label reference */
	C,	/* to keyword */
	D ;	/* name */
{
    return( five_kids( newExNode( assignlabelK ), NULL,
	A, B, C, D )  ) ;
}

NodePtr
make_call_statement(A,B,C,D,E)
NodePtr A,	/* call keyword */
	B,	/* subroutine name */
	C,	/* left paren (maybe null)*/
	D,	/* arguments (maybe null)*/
	E ;	/* right paren (maybe null)*/
{
    return( six_kids( newExNode( callK ), NULL,
   	A, B, C, D, E )  ) ;
}

NodePtr
make_call_arguments( A, B, C )
NodePtr A,	/* a list of argument nodes (maybe NULL) */
	B,	/* a comma (null if A is null */
	C ;	/* an argument */
{
    return( link_nodes(-1,
	A,
	link_nodes(-1, B, C ))  ) ;
}

NodePtr
make_altreturn_node( A, B )
NodePtr A,	/* star token */
	B ;	/* label reference */
{
    return( two_kids( newExNode( altreturnK ), A, B )  ) ;
}

NodePtr
make_do_statement( A, B, C, D, E, F, G, H, I )
NodePtr A,	/* do keyword */
	B,	/* do label reference */
	C,	/* optional comma (maybe null) */
	D,	/* name of counter variable */
	E,	/* equal sign */
	F,	/* do ranges */
	G,	/* do body */
	H,	/* sought label */
	I ;	/* executable statement */
{
    if ( I != NULL ) 
	add_label_to_statement( H, I ) ;
    else
	comp_err("do statement") ;
    return( eight_kids( newExNode( doK ), NULL,
	A, B, C, D, E, F, link_nodes(-1, G, I ))  ) ;
}

NodePtr
make_doenddo_statement( A, B, C, D, E, F, G, H )
NodePtr A,	/* do keyword */
	B,	/* optional comma (maybe null) */
	C,	/* counter variable name */
	D,	/* equal sign */
	E,	/* do ranges */
	F,	/* do body */
	G,	/* label */
	H ;	/* enddo keyword */
{
    add_label_to_statement( G, H ) ;
    return( seven_kids( newExNode(doenddoK), NULL,
	A, B, C, D, E, link_nodes(-1, F, H))  ) ;
}

NodePtr
make_dowhile_statement( A, B, C, D, E, F, G )
NodePtr	A,	/* dowhile keyword */
	B, 	/* ( */
	C,	/* logical expression */
	D, 	/* ( */
	E,	/* do body */
	F,	/* label */
	G ;	/* enddo keyword */
{
    add_label_to_statement( F, G ) ;
    return( six_kids( newExNode(dowhileK), NULL,
	A, B, C, D, link_nodes(-1, E, G) )  ) ;
}

NodePtr
make_do_body( A, B )
NodePtr A,	/* earlier statements (maybe null) */
	B ;	/* newest statement */
{
    return( link_nodes(-1,A,B)  ) ;
}

NodePtr
make_dorange_node( A, B, C, D, E )
NodePtr A,	/* start expression */
	B,	/* comma */
	C,	/* end expression */
	D,	/* comma (null if E null) */
	E ;	/* increment expression (maybe null) */
{
    return( five_kids( newExNode( dorangeK ), A, B, C, D, E )  ) ;
}

NodePtr
make_enddo_statement( A )
NodePtr A ;	/* token enddo */
{
    return( two_kids( newExNode( enddoK ), NULL, A )  ) ;
}

NodePtr
make_goto_statement( A, B )
NodePtr A,	/* goto keyword */
	B ;	/* label reference */
{
    return( three_kids( newExNode( gotoK ), NULL,
	A, B )  ) ;
}

NodePtr
make_gotocomputed_statement( A, B, C, D, E, F )
NodePtr A,	/* goto keyword */
	B,	/* left paren */
	C,	/* goto label list */
	D,	/* right paren */
	E,	/* optional comma (maybe null) */
	F ;
{
    return( seven_kids( newExNode( gotocomputedK ), NULL,
    	A, B, C, D, E, F )  ) ;
}

NodePtr
make_gotoassigned_statement( A, B, C, D, E, F )
NodePtr A,	/* goto keyword */
	B,	/* name */
	C,	/* optional comma (maybe null) */
	D,	/* left paren (maybe null) */
	E,	/* goto label list (maybe null) */
	F ;	/* right paren (maybe null) */
{
    return( seven_kids( newExNode( gotoassignedK ), NULL,
	A, B, C, D, E, F )  ) ;
}

NodePtr
make_gotolabel_list( A, B, C )
NodePtr A,	/* earlier part of list (maybe null) */
	B,	/* comma (null if A is null) */
	C ;	/* this label reference */
{
    return( link_nodes( -1,
	A,
	link_nodes( -1, B, C ))  ) ;
}

NodePtr
make_ifarith_statement( A, B, C, D, E, F, G, H, I )
NodePtr A,	/* if keyword */
	B,	/* left paren */
	C,	/* expression */
	D,	/* right paren */
	E, 	/* label reference */
	F,	/* comma */
	G,	/* label reference */
	H,	/* comma */
	I ;	/* label reference */
{
    return( ten_kids( newExNode( ifarithK ), NULL,
	A, B, C, D, E, F, G, H, I )  ) ;
}

NodePtr
make_iflogical_statement( A, B, C, D, E )
NodePtr A,	/* if keyword */
	B,	/* ( */
	C,	/* expression */
	D,	/* ) */
	E ;	/* executable statement */
{
    return( six_kids( newExNode( iflogicalK ), NULL,
	A, B, C, D, E )  ) ;
}

NodePtr
make_ifthenelse_statement( A, B, C, D, E, F, G, H, I, J, K )
NodePtr A,	/* if keyword */ 
	B,	/* ( */
	C,	/* expression */
	D,	/* ) */
	E,	/* then keyword */
	F,	/* then body (maybe null) */
	G,	/* label (maybe null)*/
	H,	/* else keyword (maybe null) */
	I,	/* else body (maybe null) */
	J,	/* label */
	K ;	/* endif statement */
{
    if ( H != NULL)
	add_label_to_statement( G, H ) ;
    if ( K != NULL)
	add_label_to_statement( J, K ) ;
    else
	comp_err("ifthenelse_statement, null endif statement") ;
    return( ten_kids( newExNode( ifthenelseK ), NULL,
	A, B,  C, D, E, F, H, I, K )  ) ;
}

NodePtr
make_ifthenelseif_statement( A, B, C, D, E, F, G, H )
NodePtr A,	/* if keyword */
	B,	/* ( */
	C,	/* expression */
	D,	/* ) */
	E,	/* then keyword */
	F,	/* then body (maybe null) */
	G,	/* label */
	H ;	/* elseif statement */
{
    if ( H != NULL )
	add_label_to_statement( G, H ) ;
    else
	comp_err("ifthenelseif statement, null elseif statement") ;
    return( eight_kids( newExNode( ifthenelseifK ), NULL,
	A, B, C, D, E, F, H )  ) ;
}

NodePtr
make_conditional_body( A, B, C )
NodePtr A,	/* previous executable statements (maybe null) */
	B,	/* label for this statement (maybe null) */
	C ;	/* this statement (maybe null) */
{
    if ( C != NULL )
	add_label_to_statement( B, C ) ;
    link_nodes(-1, A, C ) ;
}

NodePtr
make_return_statement( A, B )
NodePtr A,	/* return keyword */
	B ;	/* expression */
{
    return( three_kids( newExNode( returnK ), NULL, A, B )  ) ;
}

NodePtr
make_stop_statement( A, B )
NodePtr A,	/* stop keyword */
	B ;	/* labelref or string */
{
    return( three_kids( newExNode( stopK ), NULL, A, B )  ) ;
}

NodePtr
make_pause_statement( A, B )
NodePtr A,	/* pause keyword */
	B ;	/* labelref or string */
{
    return( three_kids( newExNode( pauseK ), NULL, A, B )  ) ;
}

/* this takes care of blank lines with empty labels 
   note, though, that it is an error if the labels are
   non-blank.  Note also that this is one place, where for
   simplicity's sake, the grammar is using RIGHT recursion.*/ 
NodePtr
make_null_executable( A, B )
NodePtr A,	/* a blank label for a null line */
	B ;	/* subsequent label, maybe for a good line */
{
    return( link_nodes(-1,
	one_kid( newDefNode( nullK ), A ),
	B )  ) ;
}

NodePtr
make_do_label_ref( A )
NodePtr A ;	/* label reference */
{
    if (A == NULL) 
    	comp_err("make_do_label_ref, bad label reference") ;
    if (A->u.Token == NULL)
	comp_err("make_do_label_ref, bad labeltoken reference") ;

    seeklabel(A->u.Token->symptr) ;
    return(A) ;
}

/* an integer constant has been expressed as a reference to 
   a label.  We will continue using this node, but we have the
   symbol reference pointer in the TICON token node point to 
   a symbol table node for this label, too.
*/
NodePtr
make_label_ref(A)
NodePtr A ;	/* integer constant reference */
{
    char digit_string[20] ;

    if (A == NULL)
	comp_err("make_label_ref, bad label reference") ;
    if (A->u.Token == NULL)
	comp_err("make_label_ref, bad labeltoken reference") ;

    sprintf(digit_string,"%d",atoi( GETTOKSTRING( A->u.Token ) )) ;
    A->u.Token->symptr = SYMGET( digit_string ) ;
    return(A) ;
}


/*  I/O statements */

NodePtr
make_ber_statement( A, B )
NodePtr A,	/* backspaceK, endfileK, or rewindK node */
	B ;	/* io-control, format_spec, or power */
{
    A->nodeChild[2] = B ;
    return(A) ;
}

NodePtr
make_read_statement( A, B, C )
NodePtr A, 	/* read token */
	B,	/* io-control, format_spec(maybe null)*/
	C ;	/* input_list (maybe null) */
{
    return( four_kids( newExNode(readK), NULL, A, B, C )  ) ;
}

NodePtr
make_write_statement( A, B, C )
NodePtr A,      /* write keyword */
        B,      /* io-control, format_spec (maybe null)*/
        C ;     /* input_list (maybe null) */
{
    return( four_kids( newExNode(writeK), NULL, A, B, C )  ) ;
}

NodePtr
make_print_statement( A, B, C, D )
NodePtr A,	/* print keyword */
	B,	/* format_spec */
	C,	/* comma (null if D is null) */
	D ;	/* output_list */
{
    return( five_kids( newExNode(printK), NULL, A, B, C, D )  ) ;
}

NodePtr
make_oci_statement( A, B )
NodePtr A,	/* openK, closeK, inquireK */
	B ;	/* io-control */
{
    A->nodeChild[2] = B ;
    return(A) ;
}

NodePtr
make_iocontrol_node( A, B, C )
NodePtr A,	/* ( */
	B,	/* ctllist */
	C ;	/* ) */
{
    return( three_kids( newExNode( iocontrolK ), A, B, C )  ) ;
}

NodePtr
make_ctllist( A, B, C )
NodePtr A,	/* previous items (maybe null)  */
	B, 	/* comma (maybe null) */
	C ;	/* this item */
{
    return( link_nodes( -1,
	A,
	link_nodes( -1, B, C ))  ) ;
}

NodePtr
make_iocontrolitem_node( A, B )
NodePtr A,	/* name (maybenull) */
	B ;	/* expression, star, or power */
{
    return( two_kids( newExNode( iocontrolitemK ), A, B )  ) ;
}

NodePtr
make_input_list( A, B, C )
NodePtr	A,	/* previous input elements */
	B,	/* comma (maybe null) */
	C ;	/* input element */
{
    return( link_nodes(-1,
	A,
	link_nodes( -1, B, C ))  ) ;
}

NodePtr
make_implieddoin_node( A, B, C, D, E, F, G )
NodePtr A,	/* ( */
	B,	/* input list */
	C,	/* , */
	D,	/* count variable name */
	E,	/* = */
	F,	/* do ranges */
	G ;	/* ) */
{
    return( seven_kids( newExNode( implieddoinK ),
	A, B, C, D, E, F, G )  ) ;
} 

NodePtr
make_implieddoout_node( A, B, C, D, E, F, G )
NodePtr A,	/* ( */
	B,	/* expression */
	C,	/* , */
	D,	/* count variable name */
	E,	/* = */
	F,	/* do ranges */
	G ;	/* ) */
{
    return( seven_kids( newExNode( implieddooutK ),
	A, B, C, D, E, F, G )  ) ;
} 

/* 3-16-90, to include keyword tokens with some of the
   more simple ExNodes, such as rewindK, continueK, etc. */

NodePtr
makeBareExNode( minorkind, B )
ExKnd minorkind ;
NodePtr B ;
{
    NodePtr p ;
    p = newExNode( minorkind ) ;
    p->nodeChild[1] = B ;
    return( p ) ;
}

/* 6-19-91, place holder node for features not yet implemented in AST but
   which the parser should recognize to avoid bombing */

NodePtr
make_dumb_node()
{
   return( no_kids(newExNode( unimplementedK ) ) ) ;
}
