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

#include <stdio.h>
#include "node.h"
#include "tokens.h"

#define initptrs(n) { register int i ; \
			  p->nodeSibling = n ; \
			  p->longSibling = n ; \
			  for (i = 0 ; i < maxChildren ; i ++  ) \
			      p->nodeChild[i] = n ; \
			}

NodePtr standard_int = NULL ;
NodePtr standard_real = NULL ;
NodePtr standard_logical = NULL ;
NodePtr standard_double = NULL ;
NodePtr standard_complex = NULL ;
NodePtr standard_character = NULL ;

void * malloc(), *myMalloc() ;

NodePtr newNode()
{
    NodePtr p ;
    p = (NodePtr) myMalloc( sizeof( *p ) ) ;
    initptrs(NULL) ;
    return(p) ;
}

NodePtr newExNode( kind )
ExKnd kind ;
{
    NodePtr p ;
    p = newNode() ;
    p->nodeKind = exK ;
    p->u.Ex = (ExPtr) myMalloc( sizeof( *(p->u.Ex) ) ) ;
    p->u.Ex->ExKind = kind ;
    return(p) ;
}

NodePtr newDefNode( kind )
DefKnd kind ;
{
    NodePtr p ;
    int i ;

    p = newNode() ;
    p->nodeKind = defK ;
    p->u.Def = (DefPtr) myMalloc( sizeof( *(p->u.Def) ) ) ;
    p->u.Def->DefKind = kind ;
    p->u.Def->DefType = NULL ;
    for (i = 0 ; i < IMPLSIZE ; i++) {
	p->u.Def->u.program.implicit[i] = NULL ;
    }
    return(p) ;
}

NodePtr newTypeNode( kind )
TypeKnd kind ;
{
    NodePtr p ;
    p = newNode() ;
    p->nodeKind = typeK ;
    p->u.Type = (TypePtr) myMalloc( sizeof( *(p->u.Type) ) ) ;
    p->nodeKind = typeK ;
    p->u.Type->TypeKind = kind ;
    return(p) ;
}

/* note, this one doesn't allocate a subrecord because this is 
   already assumed to exist and and a pointer to which is passed as 
   an argument */
NodePtr newTokNode( q )
TokInfoPtr q ;
{
    NodePtr p ;
    p = newNode() ;
    p->nodeKind = tokenK ;
    p->u.Token = q ;
    return(p) ;
}

static char inttemp[132] ;

make_standard_int()
{
    NodePtr p ;
    TokInfoPtr q ;
    q = (struct TokenRecType * ) myMalloc(sizeof(struct TokenRecType)) ;
    sprintf(inttemp,"0:internal:%d:0:0:0:0:TINTEGER:integer",TINTEGER) ;
    q->inp_line = inttemp ;
    q->TokenKind = TINTEGER ;
    setfields( q->inp_line, q->field) ;
    p = newTokNode( q ) ;
    standard_int = one_kid( newDefNode( typespecK ), p ) ;
}

static char realtemp[132] ;

make_standard_real()
{
    NodePtr p ;
    TokInfoPtr q ;
    q = (struct TokenRecType * ) myMalloc(sizeof(struct TokenRecType)) ;
    sprintf(realtemp,"0:internal:%d:0:0:0:0:TREAL:real",TREAL) ;
    q->inp_line = realtemp ;
    q->TokenKind = TREAL ;
    setfields( q->inp_line, q->field) ;
    p = newTokNode( q ) ;
    standard_real = one_kid( newDefNode( typespecK ), p ) ;
}

static char doubletemp[132] ;

make_standard_double()
{
    NodePtr p ;
    TokInfoPtr q ;
    q = (struct TokenRecType * ) myMalloc(sizeof(struct TokenRecType)) ;
    sprintf(doubletemp,"0:internal:%d:0:0:0:0:TDOUBLE:doubleprecision",TDOUBLE) ;
    q->inp_line = doubletemp ;
    q->TokenKind = TDOUBLE ;
    setfields( q->inp_line, q->field) ;
    p = newTokNode( q ) ;
    standard_double = one_kid( newDefNode( typespecK ), p ) ;
}

static char complextemp[132] ;

make_standard_complex()
{
    NodePtr p ;
    TokInfoPtr q ;
    q = (struct TokenRecType * ) myMalloc(sizeof(struct TokenRecType)) ;
    sprintf(complextemp,"0:internal:%d:0:0:0:0:TCOMPLEX:complex",TCOMPLEX) ;
    q->inp_line = complextemp ;
    q->TokenKind = TCOMPLEX ;
    setfields( q->inp_line, q->field) ;
    p = newTokNode( q ) ;
    standard_complex = one_kid( newDefNode( typespecK ), p ) ;
}

static char logicaltemp[132] ;

make_standard_logical()
{
    NodePtr p ;
    TokInfoPtr q ;
    q = (struct TokenRecType * ) myMalloc(sizeof(struct TokenRecType)) ;
    sprintf(logicaltemp,"0:internal:%d:0:0:0:0:TLOGICAL:logical",TLOGICAL) ;
    q->inp_line = logicaltemp ;
    q->TokenKind = TLOGICAL ;
    setfields( q->inp_line, q->field) ;
    p = newTokNode( q ) ;
    standard_logical = one_kid( newDefNode( typespecK ), p ) ;
}

static char charactertemp[132] ;

make_standard_character()
{
    NodePtr p ;
    TokInfoPtr q ;
    q = (struct TokenRecType * ) myMalloc(sizeof(struct TokenRecType)) ;
    sprintf(charactertemp,"0:internal:%d:0:0:0:0:TCHARACTER:character",TCHARACTER) ;
    q->inp_line = charactertemp ;
    q->TokenKind = TCHARACTER ;
    setfields( q->inp_line, q->field) ;
    p = newTokNode( q ) ;
    standard_character = one_kid( newDefNode( typespecK ), p ) ;
}

NodePtr
sixteen_kids(X,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P)
NodePtr X,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P ;
{
          X->nodeChild[0] = A ;
          X->nodeChild[1] = B ;
          X->nodeChild[2] = C ;
          X->nodeChild[3] = D ;
          X->nodeChild[4] = E ;
          X->nodeChild[5] = F ;
          X->nodeChild[6] = G ;
          X->nodeChild[7] = H ;
          X->nodeChild[8] = I ;
          X->nodeChild[9] = J ;
          X->nodeChild[10] = K ;
          X->nodeChild[11] = L ;
          X->nodeChild[12] = M ;
          X->nodeChild[13] = N ;
          X->nodeChild[14] = O ;
          X->nodeChild[15] = P ;
          return(X) ;
}


/* getsym()	added 3-20-90

    Given a pointer to a node that has a tokenK(TNAME) node as a 
    child, and given the number of that child, return the symbol
    table pointer from that node.  For example, one could call
    this on a functionK node to get the symbol table node
    for the name of the function thus:

	getsym( p, 3 )

    Note that child three of a functionK node is the pointer to a
    tokenK( TNAME ) node standing for the name of this function.
*/

SymPtr
getsym( node, child )
NodePtr node ;
int child ;
{
    /* note that comp_err causes fatal abort of compile */

    if ( node == NULL )
	ast_err("getsym(): called with null node",node) ;
    if ( child >= maxChildren ) 
	ast_err("getsym(): called with invalid child number",node) ;
    if ( node->nodeChild[ child ] == NULL )
	ast_err("getsym(): called with null child",node) ;
    if ( node->nodeChild[ child ]->nodeKind != tokenK )
	ast_err("getsym(): child not a token node",node->nodeChild[child]) ;
    if ( node->nodeChild[ child ]->u.Token->TokenKind != TNAME )
	ast_err("getsym(): child token not a name token",
	    node->nodeChild[ child ]) ;

    /* if we get here, we should have what we want */

    return( node->nodeChild[ child ]->u.Token->symptr ) ;
}

/*************************************************************/
/*****  routines for handling linked lists of List nodes *****/
/*************************************************************/

/* added 3-21-90, with addition of typechecking phase.
   These nodes are used in the creation of secondary linked lists 
   of struct Node nodes.  */
ListPtr newListNode()
{
    ListPtr p ;
    p = (ListPtr) myMalloc( sizeof( *p ) ) ;
    p->u.node = NULL ;
    p->next = NULL ;
    return( p ) ;
}

/* prepend_list()
     Add a Node node to a List list by making a new List node with
     the Node node attached, and then prepend that to the list
     passed as argument list.  If list is null, create the list. 

     Node may be either a pointer to a symbol node or a nodeptr node.
     While, because we have to type the pointer something, node is
     NodePtr, it doesn't really matter to these routines.
*/
prepend_list( list, node )
ListPtr *list ;
NodePtr node ;
{
     ListPtr p ;
     if ( node == NULL ) return ;
     p = newListNode() ;
     p->u.node = node ;
     p->next = *list ;
     *list = p ;
}

/* append_list()
     same as prepend_list except node goes at the end of the list.
*/
append_list( list, node )
ListPtr *list ;
NodePtr node ;
{
    ListPtr p, q ;
    if ( node == NULL ) return ;
    p = newListNode() ;
    p->u.node = node ;
    q = *list ;
    if (q != NULL) {			/* fix 910126 (was q == NULL)*/
        while (q->next != NULL) 
	    q = q->next ;
	q->next = p ;
    } else {
	*list = p ;
    }
}


/* 
    descend_tree()

    General purpose node retrieval routine.

    Arguments:
	node	pointer to a node
	path	string specifying path to desired descendant
		of this node.  Children are specified
		by hex digit.  Siblings are 's'.

    Example: path = "1ass2"
	Find the second child of the sibling of the sibling of 
	the tenth child of the first child of this node.

    If this routine fails to find the node, it calls ast_err
    with the message "descend_tree() -- can't find node" and
    returns NULL.  That's all it can do, unfortunately, since
    it doesn't know any more about what we're looking for.
    And, it's wierd but it should make life easier.
*/

NodePtr 
descend_tree( node, path )
NodePtr node ;
char * path ;
{
    char * p ;
    int i ;
    p = path ;

    if (node == NULL) {
	ast_err("null node to descend_tree",NULL) ;
	return(NULL) ;
    }
    if (p == NULL) {
	ast_err("no path specified for descend_tree()", node) ;
	return(NULL) ;
    }
    if (*p == '\0' ) {
	return( node ) ;
    }
    if (*p == 's' || *p == 'S' ) {
	return( descend_tree( node->nodeSibling, p+1 )) ;
    }
    if (*p >= '0' && *p <= '9') {
	i = *p - '0' ;
	return( descend_tree( node->nodeChild[i], p+1 )) ;
    }
    if (*p >= 'a' && *p <= 'f') {
	i = *p - 'a' + 10 ;
	return( descend_tree( node->nodeChild[i], p+1 )) ;
    }
    if (*p >= 'A' && *p <= 'F') {
	i = *p - 'A' + 10 ;
	return( descend_tree( node->nodeChild[i], p+1 )) ;
    }
    ast_err("bad path string to descend_tree9", node) ;
    return(NULL) ;
}


/*
   routines for accessing various things about structures 
*/

NodePtr
find_field( structure, name )
NodePtr structure ;
char * name ;
{
    NodePtr p, q, r ;
    char * fn ;

    if ((structure == NULL) || (name == NULL)) {
	ast_err("field_type passed null",structure) ;
	return(NULL) ;
    }

    for(p = structure->nodeChild[6] ; p != NULL ; p = p->nodeSibling ) {
	switch ( p->GENERIC(GenericKind) ) {
	    case typedeclK :
		for( q=p->nodeChild[2];q!=NULL;q=q->nodeSibling) {
		    /* q should point to a fielddeclK node */
		    if ( q->nodeKind == tokenK && q->GENERIC(GenericKind) == TCOMMA) {
			continue ;
		    }
		    if ((r = q->nodeChild[0]) == NULL) {
			ast_err("null name for fielddeclK",q) ;
			continue ;
		    }
		    fn = GETTOKSTRING(r->u.Token) ;
		    if (strcmp(fn,name) == 0) { /* found it */
			 return( q ) ;
		    }
		}
	    case recordK :
		for( q=p->nodeChild[5];q!=NULL;q=q->nodeSibling) {
		    /* q should point to a fielddeclK node */
		    if ( q->nodeKind == tokenK && q->GENERIC(GenericKind) == TCOMMA) {
			continue ;
		    }
		    if ((r = q->nodeChild[0]) == NULL) {
			ast_err("null name for fielddeclK",q) ;
			continue ;
		    }
		    fn = GETTOKSTRING(r->u.Token) ;
		    if (strcmp(fn,name) == 0) { /* found it */
			 return( q ) ;
		    }
		}
	    case structureK :
		for( q=p->nodeChild[5];q!=NULL;q=q->nodeSibling) {
		    /* q should point to a fielddeclK node */
		    if ( q->nodeKind == tokenK && q->GENERIC(GenericKind) == TCOMMA) {
			continue ;
		    }
		    if ((r = q->nodeChild[0]) == NULL) {
			ast_err("null name for fielddeclK",q) ;
			continue ;
		    }
		    fn = GETTOKSTRING(r->u.Token) ;
		    if (strcmp(fn,name) == 0) { /* found it */
			 return( q ) ;
		    }
		}
	    default : break ;
	}
    }
    return( NULL ) ;
}
			



		    
    
