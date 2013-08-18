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

/* f_definitions.c 

   routines that go with grammar rules in f.definitions

*/

#include <stdio.h>
#include "node.h"
#include "f_y.h"
#include "tokens.h"

NodePtr
make_struct_statement( A, B, C, D, E, F, G, H )
NodePtr A,	/* structure keyword */
	B,	/* slash (maybe null) */
	C,	/* structure name (maybe null) */
	D,	/* slash (maybe null) */
	E,	/* field declaration list (maybe null) */
	F,	/* body of structure */
	G,	/* label */
	H ;	/* endstructure keyword */
{
    NodePtr p ;
    if (H!=NULL)
	H->nodeChild[0] = G ;
    else
	comp_err("make_struct_statement, bad endstructure node") ;

    p = newTypeNode( structureK ) ;

    if (C != NULL) {
	if ( C->u.Token->symptr == NULL ) {
	    comp_err(
	       "make_struct_statement, undefined symbol for struct name") ;
	} else {
	    p->u.Type->u.structure.name = C->u.Token->symptr ;
	}
    }
	
    return( eight_kids( p, NULL,
	A, B, C, D, E, F, H )  ) ;
}

NodePtr
make_record_statement( A, B, C, D, E )
NodePtr A,	/* record keyword */
	B,	/* slash */
	C,	/* name */
	D,	/* slash */
	E ;	/* id decl list */
{
    NodePtr p ;
    p = newDefNode( recordK ) ;
    if (C != NULL) {
        if ( C->u.Token->symptr == NULL ) {
            comp_err(
               "make_record_statement, undefined symbol for rec name") ;
       } else {
            p->u.Def->u.record.name = C->u.Token->symptr ;
        }
    }
    return( six_kids( p, NULL,
	A, B, C, D, E )  ) ;
}

NodePtr
make_union_statement( A, B, C, D )
NodePtr A,	/* union keyword */
	B,	/* map list */
	C,	/* label */
	D ;	/* endunion keyword */
{
    if (D != NULL)
	D->nodeChild[0] = C ;
    else
	comp_err("make_union_statement, bad endunion node") ;
    return( four_kids( newTypeNode( unionK ), NULL,
	A, B, D )  ) ;
}

NodePtr
make_map_statement( A, B, C, D, E )
NodePtr A,	/* label */
	B,	/* map keyword */
	C,	/* struct body */
	D,	/* label */
	E ;	/* endmap keyword */
{
    if (E!=NULL)
	D->nodeChild[0] = D ;
    else
	comp_err("make_map_statement, bad endmap node") ;
    return( four_kids( newTypeNode( mapK ), A, B, C, E )  ) ;
}

NodePtr
make_common_statement( A, B, C, D, E, F, G )
NodePtr A,	/* common keyword */
	B,	/* slash (maybe null) */
	C,	/* name (maybe null) */
	D,	/* slash (maybe null) */
	E,	/* id_decl_list */
	F,	/* optional comma (maybe null) */
	G ;	/* more common */
{
    return( three_kids( newDefNode( commonK ), NULL, A,
	    make_more_common( B, C, D, E , F, G ))  ) ;
}

NodePtr
make_more_common( A, B, C, D, E, F )
NodePtr A,	/* slash (maybe null) */
	B,	/* name (maybe null) */
	C,	/* slash (maybe null) */
	D,	/* id_decl_list */
	E,	/* optional comma */
	F ;	/* more common */
{
    NodePtr p ;
    p = newDefNode( morecommonK ) ;

    if (B != NULL) {
        if ( B->u.Token->symptr == NULL ) {
            comp_err(
               "make_more_common, undefined symbol for named common") ;
        } else {
            p->u.Def->u.common.name = B->u.Token->symptr ;
        }
    }

    return( link_nodes( -1,
        five_kids( p, A, B, C, D, E ),
	F )  ) ;
}

NodePtr
make_namelist_statement( A, B, C, D, E, F, G )
NodePtr A,	/* namelist keyword */
	B,	/* slash (maybe null) */
	C,	/* name (maybe null) */
	D,	/* slash (maybe null) */
	E,	/* id_decl_list */
	F,	/* optional comma (maybe null) */
	G ;	/* more common */
{
    return( three_kids( newDefNode( namelistK ), NULL, A,
	    make_more_namelist( B, C, D, E , F, G ))  ) ;
}

NodePtr
make_more_namelist( A, B, C, D, E, F )
NodePtr A,	/* slash (maybe null) */
	B,	/* name (maybe null) */
	C,	/* slash (maybe null) */
	D,	/* id_decl_list */
	E,	/* optional comma */
	F ;	/* more namelist */
{
    NodePtr p ;
    p = newDefNode( morenamelistK ) ;

    if (B != NULL) {
        if ( B->u.Token->symptr == NULL ) {
            comp_err(
               "make_more_namelist, undefined symbol for named common") ;
        } else {
            p->u.Def->u.namelist.name = B->u.Token->symptr ;
        }
    }

    return( link_nodes( -1,
        five_kids( p, A, B, C, D, E ),
	F )  ) ;
}

NodePtr
make_iddecl_node( A, B )
NodePtr A,	/* name */
	B ;	/* subscript (maybe null) */
{
    return( two_kids( newDefNode( iddeclK ), A, B )  ) ;
}

NodePtr
make_fielddecl_node( A, B )
NodePtr A, 	/* name */
	B ;	/* subscript (maybe null) */
{
    return( two_kids( newDefNode( fielddeclK ), A, B )  ) ;
}

NodePtr
make_id_decl_list( A, B, C )
NodePtr A,	/* previous list */
	B,	/* comma (maybe null) */
	C ;	/* this id_decl */
{
    return( link_nodes( -1,
	A,
	link_nodes( -1, B, C ))  ) ;
}

NodePtr
make_data_statement( A, B )
NodePtr A,	/* datakeyword */
	B ;	/* data list */
{
    return( three_kids( newDefNode( dataK ), NULL, A, B )  ) ;
}

NodePtr
make_datalistitem_node( A, B, C, D )
NodePtr A,	/* data name list */
	B,	/* slash */
	C,	/* data value list */
	D ;	/* slash */
{
    return( four_kids( newDefNode( datalistitemK ), A, B, C, D )  ) ;
}

NodePtr
make_datarep_node( A, B, C )
NodePtr A,	/* data repeater */
	B,	/* star */
	C ;	/* data_value */
{
    return( three_kids( newDefNode( datarepK ), A, B, C )  ) ;
}

NodePtr
make_didl_node( A, B, C, D, E, F )
NodePtr A,	/* ( */
	B,	/* data name list */
	C,	/* name */
	D,	/* = */
	E,	/* ranges */
	F ;	/* ) */
{
    return( six_kids( newDefNode( didlK ), A, B, C, D, E, F )  ) ;
}

NodePtr
make_didlrange_node( A, B, C, D, E )
NodePtr A,	/* begin expression */
	B,	/* comma */
	C,	/* end expression */
	D,	/* comma (null if E is null) */
	E ;	/* increment expression (maybe null) */
{
    return( five_kids( newDefNode( didlrangeK ), A, B, C, D, E )  ) ;
}

NodePtr
make_dimension_node( A, B )
NodePtr A,	/* dimension keyword */
	B ;	/* array declarator list */
{
    return( three_kids( newDefNode( dimensionK ), NULL, A, B )  ) ;
}

NodePtr
make_subscript_node( A, B, C )
NodePtr A, 	/* ( */
	B,	/* subscript list */
	C ;	/* ) */
{
    return( three_kids( newDefNode( subscriptK ), A, B, C )  ) ;
}

NodePtr
make_subscriptitem( A, B, C )
NodePtr A,	/* lower range (maybe null) */
	B,	/* colon (null if A null) */
	C ;	/* upper range */
{
    return( three_kids( newDefNode( subscriptitemK ), A, B, C )  ) ;
}

NodePtr
make_end_statement( A )
NodePtr A ;	/* end keyword */
{
    return( two_kids( newExNode( endK ), NULL, A )  ) ;
}

NodePtr
make_entry_statement( A, B, C )
NodePtr A,	/* entry keyword */
	B,	/* name */
	C ;	/* dummy arguments */
{
    return( four_kids( newDefNode( entryK ), NULL, A, B, C )  ) ;
}

NodePtr
make_equiv_statement( A, B ) 
NodePtr A, 	/* equiv keyword */
	B ;	/* list */
{
    return( three_kids( newDefNode( equivalenceK ), NULL, A, B )  ) ;
}

NodePtr
make_equive_node( A, B, C )
NodePtr A,	/* ( */
	B,	/* equiv entity list */
	C ;	/* ) */
{
    return( three_kids( newDefNode( equivenodeK ), A, B, C )  ) ;
}

NodePtr
make_pointer_statement( A, B )
NodePtr A,      /* pointer keyword */
        B ;     /* list */
{
    return( three_kids( newDefNode( pointerK ), NULL, A, B )  ) ;
}
    
NodePtr
make_external_node( A, B )
NodePtr A,	/* external keyword */
	B ;	/* list */
{
    return( three_kids( newDefNode( externalK ), NULL, A, B )  ) ;
}

NodePtr
make_include_statement( A, B )
NodePtr A,	/* include keyword */
	B ;	/* string  (maybe null) */
{
    return( three_kids( newDefNode( includeK ), NULL, A, B )  ) ;
}

NodePtr
make_format_statement( A, B )
NodePtr A,	/* format keyword */
	B ;	/* format node from make_format_node */
{
    if ( B != NULL )
	B->nodeChild[1] = A ;
    else
	comp_err("make_format_statement, bad format node" ) ;
    return(B) ;
}

NodePtr
make_format_node( A, B, C )
NodePtr A,	/* ( or () */
	B,	/* format specification list (maybe null) */
	C ;	/* ) (maybe null) */
{
    return( five_kids( newDefNode( formatK ), NULL, NULL, A, B, C )  ) ;
}

NodePtr
make_dummy_args( A, B, C )
NodePtr	A,	/* ( or () */
	B,	/* dummmy arg list (maybe null) */
	C ;	/* ) */
{
    return( three_kids( newDefNode( dummyargsK ), A, B, C )  ) ;
}

NodePtr
make_implicit_statement( A, B )
NodePtr A,	/* implicit keyword */
	B ;	/* implicitlist or none */
{
    return( three_kids( newDefNode( implicitK ), NULL, A, B )  ) ;
}

NodePtr
make_implspec_node( A, B, C, D )
NodePtr A,	/* type declaration node */
	B,	/* ( */
	C,	/* implicit range list */
	D ;	/* ) */
{
    return( four_kids( newDefNode( implspecK ), A, B, C, D )  ) ;
}

NodePtr
make_implrange_node( A, B, C )
NodePtr A,	/* low range */
	B,	/* minus token (null if C null) */
	C ;	/* high range (maybe null) */
/* note that these ranges come in as TNAME tokens, only the
   first letter of which are of concern.
   3-15-90.  Added code to fill special low and high fields of
   node with appropriate character or null.  */
{
    NodePtr p ;
    char c ;
    p = newDefNode( implrangeK ) ;
    p->u.Def->u.implrange.low = '\0' ;
    p->u.Def->u.implrange.high = '\0' ;
    if ( A != NULL ) {
	if ( A->nodeKind == tokenK && A->u.Token->TokenKind == TNAME ) {
	    p->u.Def->u.implrange.low = *(GETTOKSTRING(A->u.Token)) ;
	}
    } else {
	comp_err("f_definitions.c: make_implrange_node().  Child 0 NULL.") ;
    }
    if ( C != NULL ) {
	if ( C->nodeKind == tokenK && C->u.Token->TokenKind == TNAME ) {
	    p->u.Def->u.implrange.high = *(GETTOKSTRING(C->u.Token)) ;
	}
    }
    return( three_kids( p, A, B, C )  ) ;
}

NodePtr
make_intrinsic_node( A, B )
NodePtr A,	/* intrinsic token */
	B ;	/* intrinsic list */
{
    return( three_kids( newDefNode(intrinsicK), NULL, A, B )  ) ;
}

NodePtr
make_parameter_node( A, B, C, D )
NodePtr A,	/* parameter keyword */
	B,	/* ( */
	C,	/* paramlist */
	D ;	/* ) */
{
    return( five_kids( newDefNode(parameterK), NULL, A, B, C, D )  ) ;
}

NodePtr
make_paramitem_node( A, B, C )
NodePtr A,	/* name */
	B,	/* equals */
	C ;	/* expression */
/* even though the expression must be constant, we will defer checking
   this until later */
{
    return( three_kids( newDefNode(paramitemK), A, B, C )  ) ;
}

NodePtr
make_save_statement( A, B )
NodePtr A,	/* save keyword */
	B ;	/* save list */
{
    return( three_kids( newDefNode(saveK), NULL, A, B )  ) ;
}

NodePtr
make_save_item( A, B , C )
NodePtr	A,	/* slash (maybe null) */
	B,	/* name */
	C ;	/* slash (maybe null) */
{
    return( three_kids( newDefNode(saveitemK), A, B, C )  ) ;
}

NodePtr
make_type_statement( A, B )
NodePtr	A,	/* typespec node */
	B ;	/* itemlist */
{
    return( three_kids( newDefNode( typedeclK ), NULL, A, B )  ) ;
}

NodePtr
make_type_node( A, B, C )
NodePtr A,	/* type token ( e.g. TINTEGER ) */
	B,	/* length specification node (maybe null) */
	C ;	/* optional comma (only character type) (maybe null) */
{
    return( three_kids( newDefNode(typespecK), A, B, C )  ) ;
}

NodePtr
make_typeitem_node( A, B )
NodePtr A,	/* id_declaration node */
	B ;	/* length specification (character type only) */
{
    if (A == NULL)
	comp_err("null decl passed to make_typeitem_node()") ;
    A->u.Def->u.decl.length = B ;
    return(A) ;
}

NodePtr
make_length_spec( A, B, C, D )
NodePtr A,	/* star */
	B,	/* left paren (null if C not a star ) */
	C,	/* star or expression */
	D ;	/* right paren (null if C not a star ) */
{
    return( four_kids( newDefNode( lengthspecK ), A, B, C, D )  ) ;
}

NodePtr
make_defparen_node( A, B, C )
NodePtr A, 	/* ( */
	B,	/* whatever */
	C ;	/* ) */
{
    return( three_kids( newDefNode( parenK ), A, B, C )  ) ;
}

NodePtr
make_defbinop_node( A, B, C )
NodePtr A,	/* lhs */
	B, 	/* operator */
	C ;	/* rhs */
{
    return( three_kids(newDefNode( binopK ), A, B, C )  ) ;
}

NodePtr
make_defunop_node( A, B )
NodePtr A, 	/* operator */
	B ;	/* operand */
{
    return( two_kids( newDefNode( unopK ), A, B )  ) ;
}

