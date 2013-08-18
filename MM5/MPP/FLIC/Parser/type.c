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

/* type.c

   Contains routines which resolve type information for id references
   in AST.

*/

#include <stdio.h>
#include "tokens.h"
#include "node.h"


 static  void typeprogram() ;
 static  void typestatements() ;
 static typeheadnodes() ;
 static typedefnodes()  ;

void * malloc(), *myMalloc() ;

char * typeintr() ;
char * standardintr() ;
char * typeintr_ptr() ;
char * standardintr_ptr() ;
void * getintr() ;
char * rm_len() ;
void * getintr() ;

NodePtr standard_type(), coerce3(), type_implied(), impliedtype() ;
char * gettype() ;
SymPtr getsym() ;

/* typetree()

   Walks through the tree.  While in the declaration part of a module,
   adds explicitly declared type, dimension, and storage information
   to the symbol table nodes.  Once executable portion is reached, the
   implicit typing rules are used for variable, array, and function
   references that have not yet been assigned types.

   Uses walk_statements() (traverse.c) to traverse top levels of tree.
*/

typetree( tree )
NodePtr tree ;
{
   void typeprogram() ;
    void typestatements() ;

    init_type() ;
    walk_statements( tree, typeprogram, typestatements, 99 ) ;
}

init_type()
{
    make_standard_int() ;
    make_standard_real() ;
    make_standard_logical() ;
    make_standard_double() ;
    make_standard_character() ;
    make_standard_complex() ;
}

/* data structures used within this module for typechecking */

static int executable ;		/* toggles between sections of program */
static NodePtr programnode ;	/* pointer to current program node */
static NodePtr headnode ;	/* pointer to current head statement node */

static void
typeprogram( node )
NodePtr node ;
{
    char c ;

    executable = 0 ;
    programnode = node ;

    /* set up default implicit declarations in implicit table of
       program node */

    for (c = 'a' ; c <= 'h' ; c++) {
	node->u.Def->u.program.implicit[IMPL(c)] = standard_real ;
    }
    for (c = 'i' ; c <= 'n' ; c++) {
	node->u.Def->u.program.implicit[IMPL(c)] = standard_int ;
    }
    for (c = 'o' ; c <= 'z' ; c++) {
	node->u.Def->u.program.implicit[IMPL(c)] = standard_real ;
    }
}

static void
typestatements( node )
NodePtr node ;
{
    headnode = node ;
    if ( node == NULL ) return ;

    switch ( node->GENERIC(GenericKind) ) {

/* nodes which start a linked list of statements */

	case mainK :
	case functionK :
	case subroutineK :
	case blockdataK :  typeheadnodes( node ) ; break ;

/* nodes which represent definition statements in linked list of statements */

	case typedeclK	:
	case commonK	: 
	case namelistK	: 
	case dimensionK	:
	case equivalenceK	:
	case externalK	:
	case implicitK	:
	case intrinsicK	:
	case parameterK	:
	case pointerK	:
	case recordK	:
	case saveK	:
	case structureK	:  typedefnodes( node ) ; break ;

/* nodes which represent statements that may only appear in executable part */

	case assignlabelK	:
	case backspaceK	:
	case callK	:
	case closeK	:
	case continueK	:
	case doK	:
	case doenddoK	:
	case dowhileK	:
	case endK	:
	case enddoK	:
	case endfileK	:
	case gotoK	:
	case gotoassignedK	:
	case gotocomputedK	:
	case ifarithK	:
	case iflogicalK	:
	case ifthenelseK	:
	case ifthenelseifK	:
	case inquireK	:
	case nullK	:
	case openK	:
	case pauseK	:
	case printK	:
	case readK	:
	case returnK	:
	case rewindK	:
	case stopK	:
	case writeK	:  
			    executable = 1 ;
			    typeexnodes( node ) ; break ;

/* nodes which represent statements that may appear in either part */

	case dataK 	:  typedefnodes( node ) ; break ;
	case formatK	:  typedefnodes( node ) ; break ;
	case includeK	:  typedefnodes( node ) ; break ;
	case entryK 	:  typeheadnodes( node ) ; break ;
	/* when assignK appears in declaration section it is actually
	   a statement function, but at this point in the processing,
	   we can't tell that yet. */
	case assignK	:  typeexnodes( node ) ; break ;

        case errorK  : break ;
	case unimplementedK : break ;
/* error */
	default : ast_err("typestatements(): unknown node",node) ; break ;
    }
}

static
typeheadnodes( node )
NodePtr node ;
{
    SymPtr sym, getsym() ;
    NodePtr impliedtype() ;

    if (node == NULL) ast_err("typeheadnodes(): null node.",NULL) ;

/* 4/22/92 */
    if ( MINORKIND( SIBLING( node ) ) == implicitK )
    {
	typedefnodes( SIBLING( node ) ) ;
    }

    switch ( node->GENERIC(GenericKind) ) {

	case functionK : 
	  /* the function name becomes assignable in the program.
	     make entry in symbol table to this effect. */
	    if ((sym = getsym( node, 3 )) == NULL) {
		ast_err("typeheadnodes(): getsym() returns null",node);
		break ;
	    }
	    sym->type->TYPE(class) = RETURNID ;
	  /* assign type.  If not specified, do implicitly. */
	    if ( node->nodeChild[1] != NULL )
		sym->type->TYPE(type) = node->nodeChild[1] ;
	    else
		sym->type->TYPE(type) = impliedtype( sym->name ) ;
	    typedummyargs( node->nodeChild[4] ) ; 
	    node->GENERIC(GenericType) = sym->type ;
	    break ;

	case subroutineK : 
 	  /* the subroutine name cannot be used in the program.
	     make a note to this effect in symbol table */
	    if ((sym = getsym( node, 2 )) == NULL) {
		ast_err("typeheadnodes(): getsym() returns null",node);
		break ;
	    }
	    sym->type->TYPE(class) = SUBROUTINE ;
	    typedummyargs( node->nodeChild[3] ) ;
	    node->GENERIC(GenericType) = sym->type ;
	    break ;

	case entryK : 
 	  /* the entry name cannot be used in the program.
	     make a note to this effect in symbol table */
	    if ((sym = getsym( node, 2 )) == NULL) {
		ast_err("typeheadnodes(): getsym() returns null",node);
		break ;
	    }
	    sym->type->TYPE(class) = ENTRY ;
	    typedummyargs( node->nodeChild[3] ) ;
	    node->GENERIC(GenericType) = sym->type ;
	    break ;
	case blockdataK : break ;
	case mainK : break ;
	default : ast_err("typeheadnodes(): unexpected AST node",node) ;
		  break ;
    }
}
    
static
typedefnodes( defnode ) 
NodePtr defnode ;
{
    if (defnode == NULL) 
	ast_err("called with null node pointer",NULL) ;

    switch ( defnode->DEF( DefKind ) ) {

/* 4/17/92 */
	case dataK      :
    {
        NodePtr moredata;
        NodePtr declist ;
        NodePtr token ;
        SymPtr sym ;
        NodePtr type ;

        for ( moredata = CHILD( defnode, 2 ) ;
              moredata ;
              moredata = SIBLING( moredata ) )
        {

            for ( declist = CHILD( moredata, 0 ) ;
                  declist ;
                  declist = SIBLING( declist ) )
            {
                if ( MINORKIND( declist ) == TCOMMA )
                    continue ;
                if ( MINORKIND( declist ) == idrefK )
                {
                    if (( token = CHILD( declist, 0 )) == NULL )
                        continue ;
                    if ( MINORKIND( token ) != TNAME )
                        continue ;
                    if (( sym = token->u.Token->symptr ) == NULL )
                        continue ;
                    if (( type = sym->type ) == NULL )
                        continue ;

                    if ( sym->type->TYPE(type) == NULL )
                    {
                        sym->type->TYPE(type) = impliedtype(sym->name);
                    }
                }
            }
        }
    }
/* 4/17/92 */
            break ;


	case commonK	: 
	    {
	       /* go through list and put dimension and storage info in
		  symbol table for each */ 
		SymPtr sym ;		/* symbol table pointer */
		NodePtr morecommon ;	/* pointer to actual common dec */
		NodePtr commonname ;	/* pointer to node common name */
		NodePtr p ;		/* for traversal */

		morecommon = defnode->nodeChild[2] ;
		while (morecommon != NULL) {
		    commonname = morecommon->nodeChild[1] ;
		    p = morecommon->nodeChild[3] ;
		    while ( p != NULL ) {
			if (p->nodeKind == tokenK
			    && p->u.Token->TokenKind == TCOMMA) {
			    /* skip */
			} else
			if (p->GENERIC( GenericKind ) == iddeclK) {
			    sym = getsym( p, 0 ) ;
			    if (sym == NULL) {
				ast_err("null sym ptr", defnode) ;
			 	morecommon = morecommon->nodeSibling ;
				continue ;
			    }
			    /* 4/17/92 */
			    if ( sym->type->TYPE(type) == NULL )
			    {
			        sym->type->TYPE(type) = impliedtype(sym->name);
			    }
			    /* 4/17/92 */
			    sym->type->TYPE(storage) = COMMON ;
			    sym->type->TYPE(common) = commonname ; /*maybe null*/
			    if ( p->nodeChild[1] != NULL )
			    {
			      sym->type->TYPE(dimensions) = p->nodeChild[1] ;
			      sym->type->TYPE(class) = ARRAY ;
			    }
			} else {
			    ast_err("bad node in common list", defnode);
			}
			p = p->nodeSibling ;
		    }
		    morecommon = morecommon->nodeSibling ;
		}
	    }
	    break ;

	case namelistK :
            user_warn( "namelist typing not implemented",defnode) ;
	    break ;
	case dimensionK	:
            {
              /* go through list of nodes and put type info in symbol
                 table for each */
                SymPtr sym ;            /* symbol table pointer */
                NodePtr p ;             /* for list traversal */

                p = defnode->nodeChild[2] ;

                while (p != NULL) {
                    if (p->nodeKind == tokenK
                        && p->u.Token->TokenKind == TCOMMA) {
                        /* skip */
                    } else
                    if (p->GENERIC( GenericKind ) == iddeclK) {
                        sym = getsym( p, 0 ) ;
                        if (sym == NULL) {
                            ast_err("null sym ptr",defnode) ;
			    p = p->nodeSibling ;
			    continue ;
			} 
                        /* 4/17/92 */
                        if ( sym->type->TYPE(type) == NULL )
                        {
                            sym->type->TYPE(type) = impliedtype(sym->name);
                        }
                        /* 4/17/92 */

                        if((sym->type->TYPE(dimensions) = p->nodeChild[1])!=NULL)
			    sym->type->TYPE(class) = ARRAY ;
			defnode->GENERIC(GenericType) = sym->type ;
                    } else {
                        ast_err("bad node in dimension list", defnode);
                    }
                    p = p->nodeSibling ;
                }
            }
            break ;

/* 4/17/92 */
/* for the time being, the type assignment as it applies to a pointer
   is kludged.  There should be a new type threaded through the parser
   and pointers should be given this type.  For now (as I stumble through
   the implicit type eliminator back end, implnone, I am satisfied that
   it not come back as an implied type.  Thus, I will set it to a made
   up integer type node.  This will at least fail the is_implied_type()
   test. */
	case pointerK           :
	case equivalenceK	:
            {
		int firstref ;
		SymPtr sym ;
		NodePtr p, p1, p2, p3 ;
#if 1
		static NodePtr dummyptr_type = NULL ;

/* KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE */
		if ( dummyptr_type == NULL )
		{
{
    NodePtr p, newTokNode() ;
    TokInfoPtr q ;
    static char t__7[256] ;
    q = (struct TokenRecType * ) myMalloc(sizeof(struct TokenRecType)) ;
    sprintf(t__7,"0:internal:%d:0:0:0:0:TINTEGER:integer",TINTEGER) ;
    q->inp_line = t__7 ;
    q->TokenKind = TINTEGER ;
    setfields( q->inp_line, q->field) ;
    p = (NodePtr) newTokNode( q ) ;
    dummyptr_type = one_kid( newDefNode( typespecK ), p ) ;
}
		}
/* KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE */
#endif
                
		for ( p = defnode->nodeChild[2] ;  /* list of parenK nodes */
                      p ;
		      p = SIBLING(p) )
		{
		    if ( MINORKIND( p ) == TCOMMA ) continue ;
		    if ( MINORKIND( p ) == parenK )
		    {
		        firstref = 1 ;
			/* traverse list of idref nodes */
		        for ( p1 = CHILD( p, 1 ) ;
			      p1 ;
			      p1 = SIBLING( p1 ) )
		        {
			    if ( MINORKIND( p1 ) == TCOMMA ) continue ;
			    if (p1->GENERIC( GenericKind ) == idrefK)
			    {
			        if (( sym = getsym( p1, 0 )) == NULL )
			        {
			 	    ast_err("null sym ptr",defnode) ;
				    continue ;
			        }
			      if ( MINORKIND( defnode ) != pointerK || !firstref )
			      {
			        if ( sym->type->TYPE(type) == NULL )
			        {
				    sym->type->TYPE(type) = impliedtype(sym->name);
			        }
			      }
			      else
			      {
				/* assert: is pointerK and firstref == 1 */
#if 1
				    sym->type->TYPE(type) = dummyptr_type ;
#else
				    sym->type->TYPE(type) = NULL ;
#endif
			      }
			      firstref = 0 ;
				/* for pointers, the appearance of a subscripted
				   identifier implies its dimensioning as an array.
				   For equivalences, however, the identifier must
				   be dimensioned separately */
				if ( MINORKIND( defnode ) == pointerK )
				{
				    /* note that the index information for the idref must
				       be converted to a proper subscript node representation.
				       This is an N32, not a FORTRAN representational constraint */
				    if (( p2 = CHILD( p1, 1 )) != NULL )
				    {
					sym->type->TYPE(dimensions) = p2 ;  /* fix this at later date*/

				        sym->type->TYPE(class) = ARRAY ;
				    }
				    sym->type->TYPE(storage) = COMMON ; /* this should be fixed to POINTER later */
				}
			    }
/* NEW NEW NEW NEW 970204 -- I changed the minor kind of the id nodes in a pointer
   statement to be iddeclK, instead (because idref's won't parse on things 
   that have subrange indices -- maybe the ultimate solution is to recognize
   those as idref's; but in any case these should be decls, here, and not
   refs anyway.  */
                    else if (p1->GENERIC( GenericKind ) == iddeclK) {
                        sym = getsym( p1, 0 ) ;
                        if (sym == NULL) {
                            ast_err("null sym ptr",defnode) ;
                            p1 = p1->nodeSibling ;
                            continue ;
                        }
                        /* 4/17/92 */
                        if ( sym->type->TYPE(type) == NULL )
                        {
                            sym->type->TYPE(type) = impliedtype(sym->name);
                        }
                        /* 4/17/92 */

                        if((sym->type->TYPE(dimensions) = p1->nodeChild[1])!=NULL)
                            sym->type->TYPE(class) = ARRAY ;
                        defnode->GENERIC(GenericType) = sym->type ;
                        if ( MINORKIND( defnode ) == pointerK )
                        {
                           /* note that the index information for the idref must
                              be converted to a proper subscript node representation.
                              This is an N32, not a FORTRAN representational constraint */
                           if (( p2 = CHILD( p1, 1 )) != NULL )
                           {
                             sym->type->TYPE(dimensions) = p2 ;  /* fix this at later date*/
                             sym->type->TYPE(class) = ARRAY ;
                           }
                           sym->type->TYPE(storage) = COMMON ; /* this should be fixed to POINTER later */
                        }

                    }
/* NEW NEW NEW NEW 970204 */

			}
		    }
		}
	    }
            break ; 
/* 4/17/92 */

	case externalK	:
	    {
		/* if an identifier appears in an external declaration
		   that it is an efunction, superceding any other
		   declaration */
                SymPtr sym ;            /* symbol table pointer */
                NodePtr p ;             /* for list traversal */

                p = defnode->nodeChild[2] ;

                while (p != NULL) {
                    if (p->nodeKind == tokenK
                        && p->u.Token->TokenKind == TCOMMA) {
                        /* skip */
                    } else
                    if (p->nodeKind == tokenK 
			&& p->u.Token->TokenKind == TNAME) {
			sym = p->u.Token->symptr ;
                        if (sym == NULL) {
                            ast_err("null sym ptr",p) ;
			    p = p->nodeSibling ;
			    continue ;
			}
			if (sym->type->TYPE(class) == IFUNCTION) {
			    type_err("can't declare external and intrinsic",p) ;
			} else {
			    sym->type->TYPE(class) = EFUNCTION ;
			}
                        /* 4/17/92 */
                        if ( sym->type->TYPE(type) == NULL )
                        {
                            sym->type->TYPE(type) = impliedtype(sym->name);
                        }
                        /* 4/17/92 */
			sym->type->TYPE(dimensions) = NULL ;
			sym->type->TYPE(common) = NULL ;
			p->GENERIC(GenericType) = sym->type ;
                    } else {
                        ast_err("bad node in external list",
				defnode);
                    }
                    p = p->nodeSibling ;
                }

	    }
	    break ; 

	case formatK	: break ; 

	case implicitK	:
	    {
		int i ;
		NodePtr *imparray, p, q ;
		NodePtr typespec ;
		char high, low, c ;

		if (programnode == NULL) {
		    ast_err("no programK node for this module",defnode) ;
		    break ;
		}
		imparray = programnode->DEF(u.program.implicit) ;

		/* p starts at first implspecK node or TNONE */
		for (p=defnode->nodeChild[2];p!=NULL;p=p->nodeSibling) {
		    if ( p->nodeKind == tokenK 
			 && p->u.Token->TokenKind == TCOMMA ) {
			continue ;
		    } else
		    if ( p->nodeKind == tokenK 
			 && p->u.Token->TokenKind == TNONE ) {
			for (i = 0 ; i < IMPLSIZE ; i ++) {
			    imparray[i] = NULL ;
			}
			break ;
		    } else
		    if ((typespec = p->nodeChild[0]) == NULL) {
			ast_err("no type specified for implicit",p) ;
			continue ;
		    } else
		    if ((q = p->nodeChild[2]) == NULL) {
			ast_err("no implicit range",p) ;
			continue ;
		    } else
		    if ( typespec->nodeKind == tokenK &&
			    typespec->u.Token->TokenKind == TUNDEFINED ) 
		    {
			high = q->DEF(u.implrange.high) ;
			low  = q->DEF(u.implrange.low ) ;
			c = low ;
			do 
			{
			   imparray[IMPL(c)] = NULL ;
			   c++ ;
			} while (c <= high) ;
		    } 
		    else 
		    {
			NodePtr s_type  ;
			NodePtr standard_type() ;
			char * gettype() ;

			high = q->DEF(u.implrange.high) ;
			low  = q->DEF(u.implrange.low ) ;
			c = low ;

			/* 4/22/92 -- it is still implied type, so based on the
			   typespec given with the implicit statement, assign
			   a standard type to the node */
			if ((s_type = standard_type(gettype(typespec))) == NULL )
			{
			    s_type = typespec ;
			}

			do {
			    imparray[IMPL(c)] = s_type ;
			    c++ ;
			} while (c <= high) ;
		    }
		}
  	    } 
	    break ; 

	case includeK	: break ; 

	case intrinsicK	:
	    {
		/* if an identifier appears in an intrinsic declaration
		   that it is an ifunction, superceding any other
		   declaration  except an external; ansi says the
		   a variable should never appear in both in a single
		   program unit. */
                SymPtr sym ;            /* symbol table pointer */
                NodePtr p ;             /* for list traversal */

                p = defnode->nodeChild[2] ;

                while (p != NULL) {
                    if (p->nodeKind == tokenK
                        && p->u.Token->TokenKind == TCOMMA) {
                        /* skip */
                    } else
                    if (p->nodeKind == tokenK 
			&& p->u.Token->TokenKind == TNAME) {
			sym = p->u.Token->symptr ;
                        if (sym == NULL) {
                            ast_err("null sym ptr",p) ;
			    p = p->nodeSibling ;
			    continue ;
			}
			if (sym->type->TYPE(class) == EFUNCTION) {
			    type_err("can't declare external and intrinsic",p) ;
			} else {
			    sym->type->TYPE(class) = IFUNCTION ;
			}
			sym->type->TYPE(dimensions) = NULL ;
			sym->type->TYPE(common) = NULL ;
			p->GENERIC(GenericType) = sym->type ;
                    } else {
                        ast_err("bad node in intrinsic list", defnode);
                    }
                    p = p->nodeSibling ;
                }

	    }
	    break ; 

	case parameterK	:
	    {
		NodePtr item ; 	/* pointer for traversing item list
				   of paramitemK nodes */
		NodePtr id ;	/* pointer to symbol name of parameter */
		SymPtr  sym ;	/* pointer to symbol table node */
		NodePtr value ;	/* pointer to value of parameter */
		int decstyle ;	/* indicates a dec style param (no parens)*/

		if (defnode->nodeChild[2] == NULL) decstyle = 1 ;
		else decstyle = 0 ;

		for(item=defnode->nodeChild[3];	/* 1rst paramitemK */
		    item != NULL ;
		    item = item->nodeSibling )
		{
		    id    = item->nodeChild[0] ;
		    value = item->nodeChild[2] ;
		    typeexpression( value ) ;
		    if ( item->nodeKind == tokenK &&
 			     item->u.Token->TokenKind == TCOMMA ) {
			/* nothing; skip commas */
		    } else
		    if ( item->GENERIC(GenericKind) != paramitemK ) {
			ast_err("wrong node in parameter list", item) ;
		    } else
		    if ( id->nodeKind != tokenK || id->u.Token->TokenKind != TNAME ) {
		        ast_err("non-name on lhs of parameter", id ) ;
		    } else
		    if ((sym = id->u.Token->symptr) == NULL ) {
			ast_err("no symbol node",id) ;
		    } else {
			/* everything OK -- sym set in previous test */
		        sym->type->TYPE(class) = PARAMETER ;
			sym->type->TYPE(storage) = LOCAL ;
			if ((sym->type->TYPE(paramval) = value ) == NULL) {
			    ast_err("missing parameter value",item) ;
			}
			if ( decstyle ) {
			/* type of untyped parameter comes from type of value */
			    if (sym->type->TYPE(type) == NULL) {
			        sym->type->TYPE(type) = 
			          value->GENERIC(GenericType)->TYPE(type);
			    }
			} else {
			/* type of parameter comes from normal rules for
			   typing an identifier */
			    if (sym->type->TYPE(type) == NULL) {
				sym->type->TYPE(type)=impliedtype(sym->name);
			    }
			}
			item->GENERIC(GenericType) = sym->type ;
		    }
		}
	    }
	    break ; 

	case recordK	:
	    {
	      /* go through list of nodes and put type info in symbol
		 table for each */
		SymPtr sym, structsym ;		/* symbol table pointers */
		NodePtr type_def ;	/* points to structure definition */
 		NodePtr p ;		/* for list traversal */

		if ((structsym = getsym( defnode, 3 )) == NULL) {
		    ast_err("no symbol entry for structure name",
			defnode->nodeChild[3]) ;
		} else
		if ((type_def = structsym->structdef) == NULL ) {
		    type_err("no structure declared",defnode->nodeChild[3]) ;
		}
		
		for (p = defnode->nodeChild[5] ; p != NULL ; p=p->nodeSibling ) {
                    if (p->nodeKind == tokenK 
                        && p->u.Token->TokenKind == TCOMMA) {
                        /* skip */
                    } else {
                        if ( p->GENERIC( GenericKind ) == iddeclK ) {
                            sym = getsym( p, 0 ) ;
                            if (sym == NULL) {
                                ast_err("null sym ptr",p) ;
				continue ;
			    }
                            sym->type->TYPE(type) = type_def ;
                            if ((sym->type->TYPE(dimensions) = p->nodeChild[1])!=NULL)
                                sym->type->TYPE(class) = ARRAY ;
                            sym->type->TYPE(length) = p->nodeChild[1] ; /* maybe null */
			    p->GENERIC(GenericType) = sym->type ;
			} else
                        if ( p->GENERIC( GenericKind ) == fielddeclK ) {
                            /* can only occur within a structure */
                            NodePtr t ;
                            t = newTypeNode() ;
                            t->TYPE(type) = type_def ;
                            if ((t->TYPE(dimensions) = p->nodeChild[1])!=NULL) {
                                t->TYPE(class) = ARRAY ;
                            } else {
                                t->TYPE(class) = VARIABLE ;
                            }
                            t->TYPE(length) = p->nodeChild[1] ; /* maybe null */
                            p->GENERIC(GenericType) = t ;
                        } else {
                            ast_err("wrong node in declaration list",p) ;
			}
		    }
                }
            }
            break ; 

	case saveK	: break ; 

	case structureK	:
	    {
		SymPtr structsym ;
		NodePtr p, q ;
		
                if ((structsym = getsym( defnode, 3 )) == NULL) {
                    ast_err("no symbol entry for structure name",
                        defnode->nodeChild[3]) ;
                } else
		if ( structsym->structdef != NULL ) {
   		    type_err("redeclaration of structure name",
				defnode->nodeChild[3]) ;
		} else {
		    structsym->structdef = defnode ;
		    /* since structures can contain structures and
		       all the structure names are known at top level,
		       we must traverse the list and handle recursively
		       any substructures. */
		    p = defnode->nodeChild[6] ;
		    while ( p != NULL ) {
			switch ( p->GENERIC(GenericKind) ) {
			    case structureK :
				typedefnodes( p ) ;  /* recurse */
				for (q=p->nodeChild[5];q!=NULL;q=q->nodeSibling) {
				    if(q->nodeKind == tokenK || q->u.Token->TokenKind==TCOMMA){
					continue ;
				    } else
                                    if ( q->GENERIC( GenericKind ) == fielddeclK ) {
                                        /* can only occur within a structure */
                                        NodePtr t ;
                                        t = newTypeNode() ;
                                        t->TYPE(type) = p ;
                                        if ((t->TYPE(dimensions) = q->nodeChild[1])!=NULL) {
                                            t->TYPE(class) = ARRAY ;
                                        } else {
                                	    t->TYPE(class) = VARIABLE ;
                            		}
                                        t->TYPE(length) = q->nodeChild[1] ; /* maybe null */
                                        q->GENERIC(GenericType) = t ;
                                    } else {
                                        ast_err("wrong node in declaration list",q) ;
                                    }
				}
				break ;
			    case typedeclK :
			    case recordK :
				typedefnodes( p ) ;  /* recurse */
				break ;
			    case unionK :
		/*		typefields( p, defnode ) ; */
				break ;
			    default :
				ast_err("invalid construct in structure statement",p) ;
				break ;
			}
			p = p->nodeSibling ;
		    }
		}
	    }
	    break ; 

	case typedeclK	:
	    {
	      /* go through list of nodes and put type info in symbol
		 table for each */
		SymPtr sym ;		/* symbol table pointer */
		NodePtr typespec ;	/* points to type being declared */
 		NodePtr p ;		/* for list traversal */

		typespec = defnode->nodeChild[1] ;
		for (p=defnode->nodeChild[2] ; p!=NULL; p=p->nodeSibling) {
		    if (p->nodeKind == tokenK 
			&& p->u.Token->TokenKind == TCOMMA) {
			/* skip */
		    } else
                    if ( p->GENERIC( GenericKind ) == iddeclK ) {
                        sym = getsym( p, 0 ) ;
                        if (sym == NULL)  {
                            ast_err("null sym ptr",p) ;
			    continue ;
			}
                        sym->type->TYPE(type) = typespec ;
                        if ((sym->type->TYPE(dimensions) = p->nodeChild[1])!=NULL)
                            sym->type->TYPE(class) = ARRAY ;
                        sym->type->TYPE(length) = p->nodeChild[1] ; /* maybe null */
			p->GENERIC(GenericType) = sym->type ;
		    } else
		    if ( p->GENERIC( GenericKind ) == fielddeclK ) {
			/* can only occur within a structure */
			NodePtr t ;
			t = newTypeNode() ;
			t->TYPE(type) = typespec ;
			if ((t->TYPE(dimensions) = p->nodeChild[1])!=NULL) {
			    t->TYPE(class) = ARRAY ;
			} else {
			    t->TYPE(class) = VARIABLE ;
			}
			t->TYPE(length) = p->nodeChild[1] ; /* maybe null */
			p->GENERIC(GenericType) = t ;
                    } else {
                        ast_err("typeitemK wrong child 0",p) ;
                    }
		}
	    }  
	    break ;
	default : break ;
    }
}

typefields( node, parent )
NodePtr node,
	parent ;
{
    if (node == NULL) {
	ast_err("null node passed to typefields()", NULL)  ;
	return ;
    }
    if (parent == NULL) {
	ast_err("null parent passed to typefields()", NULL) ;
    }
	
    switch ( node->GENERIC(GenericKind) ) {
	case typedeclK :
	    break ;
	case recordK :
	    break ;
	case unionK :
	    break ;
	default : 
	    ast_err("invalid kind of node passed to typefields()", node) ;
	    break ;
    }
}

static int doing_lhs = 0 ;	/* used by typeexpression() */

typeexnodes( exnode )
NodePtr exnode ;
{
    if (exnode == NULL) 
	ast_err("typeexstats(): called with null node pointer",NULL) ;

    switch ( exnode->EX( ExKind ) ) {
	case assignK	: 
	    {
	    /* any subscripted identifier appearing on the lhs of an
	       assignment statement cannot be an e-funct or i-funct.
	       It may be an array or, if we are still in the definition
	       part of a module, an s-funct. i
	    */
		SymPtr sym ;
	        NodePtr lhs, rhs ;
		lhs = exnode->nodeChild[1] ;
		rhs = exnode->nodeChild[3] ;

	/* set special flag to inform typeexpression not to allow any idrefs
  	   with class == RETURNID */
		doing_lhs = 1 ;
		typeexpression(lhs, NULL) ;
		doing_lhs = 0 ;
	/* unset special flag */
		typeexpression(rhs, NULL) ;

		while ( lhs->GENERIC(GenericKind) == dotK ) {	/* dotted variable reference */
		    lhs = lhs->nodeChild[0] ;
		    executable = 1 ;
		}

		sym = getsym( lhs, 0 ) ;
		if (sym == NULL)
		    ast_err("typeexnodes(): null sym on lhs of assignK",lhs) ;
		sym->type->TYPE(lhs) = 1 ;

		if ( executable ) {
		    if ( lhs->nodeChild[1] != NULL ) { /*has a subscript*/
			if (sym->type->TYPE(class) != ARRAY) {
			    sym->type->TYPE(class) = ARRAY ;
#if 0
			    type_err("No dimension specified for array",
					lhs->nodeChild[0]) ;
#endif
			}
		    } else { /* not subscripted */
			if (sym->type->TYPE(class) == ARRAY) {
#if 0
			    type_err("Missing array subscript",
					lhs->nodeChild[0]) ;
#endif
			} else
			if (sym->type->TYPE(class) == UNKNOWN 
			    || sym->type->TYPE(class) == VARIABLE) {
				sym->type->TYPE(class) = VARIABLE ;
			} else
			if (sym->type->TYPE(class) == RETURNID) {
			    /* nothing; ok as is */
			} else {
			    type_err("bad lhs of assignment",
				      lhs->nodeChild[0]) ;
			}
		    }
		    if (sym->type->TYPE(storage) == UNKNOWN )
			sym->type->TYPE(storage) = LOCAL ;
		    exnode->nodeKind = exK ;
		} else { /* could be a statement function if subscripted */
		    if ( lhs->nodeChild[1] != NULL ) { /*has subscript*/ 
			if (sym->type->TYPE(class) == UNKNOWN) {
			    /* it is a statement function */
			    sym->type->TYPE(class) = SFUNCTION ;
			    sym->type->TYPE(storage) = LOCAL ;
			    exnode->nodeKind = defK ;
			    exnode->u.Def->DefKind = statfunK ;
			} else {
			    /* must be an assignment statement.
			       toggle us over to executable */
			    executable = 1 ;
			    /* now take another look at it (recurse) */
			    typeexnodes( exnode ) ;
			}
		    } else {
			executable = 1 ;
			typeexnodes( exnode ) ;
		    }
		}
	    }
	    break ;
	case assignlabelK	: break ;
	case backspaceK	: break ;
	case callK	: 
	    {
	    NodePtr p ;
	    typeexpression( exnode->nodeChild[2], NULL ) ;
	    exnode->nodeChild[2]->GENERIC(GenericType)->TYPE(class) = EFUNCTION ;
	    for (p=exnode->nodeChild[4];p!=NULL;p=p->nodeSibling) {
		if (p->nodeKind == tokenK && p->u.Token->TokenKind == TNAME) {
		    continue ;
		}
		if (p->GENERIC(GenericKind) == altreturnK) {
		    continue ;
		}
		typeexpression( p ) ;
	    }
	    }
	    break ;
	case closeK	: break ;
	case continueK	: break ;
	case doK	:
	    typeexpression( exnode->nodeChild[4] ) ;
	    typeexpression( exnode->nodeChild[6] ) ;
	    break ;
	case doenddoK :
	    typeexpression( exnode->nodeChild[3] ) ;
	    typeexpression( exnode->nodeChild[5] ) ;
	    break ;
	case dowhileK :
	    typeexpression( exnode->nodeChild[3] ) ;
	    break ;
	case endK	: break ;
	case enddoK	: break ;
	case endfileK	: break ;
	case gotoK	: break ;
	case gotoassignedK	: break ;
	case gotocomputedK	:
	    typeexpression( exnode->nodeChild[6] ) ;
	    if ( !sametype(exnode->nodeChild[6],standard_int) ) {
		type_err("must have integer expression for computed goto",exnode) ;
	    }
	    break ;
	case ifarithK	:
	    typeexpression( exnode->nodeChild[3] ) ;
	    break ;
	case iflogicalK	:
	    typeexpression( exnode->nodeChild[3] ) ;
	    break ;
	case ifthenelseK	:
	    typeexpression( exnode->nodeChild[3] ) ;
	    break ;
	case ifthenelseifK	:
	    typeexpression( exnode->nodeChild[3] ) ;
	    break ;
	case inquireK	: break ;
	case nullK	: break ;
	case openK	: break ;
	case pauseK	: break ;
	case printK	:
	    {
	    NodePtr p ;
	    for (p = exnode->nodeChild[4]; p!=NULL ; p=p->nodeSibling ) {
		if (p->nodeKind == tokenK && p->u.Token->TokenKind == TCOMMA) {
		    continue ;
		}
		typeexpression( p ) ;
	    }
	    }
	    break ;
	case returnK	: break ;
	case rewindK	: break ;
	case stopK	: break ;
	case readK	:
	case writeK	:
	    {
	    NodePtr p ;
	    for (p = exnode->nodeChild[3]; p!=NULL ; p=p->nodeSibling ) {
		if (p->nodeKind == tokenK && p->u.Token->TokenKind == TCOMMA) {
		    continue ;
		}
		typeexpression( p ) ;
	    }
	    }
	    break ;
	default : break ;
    }

}

typedummyargs( dargs )
NodePtr dargs ;
{
    NodePtr p, tnode ;
    int impltype ;

    if (dargs == NULL) return ;

    if (dargs->GENERIC(GenericKind) != dummyargsK) {
	ast_err("typedummyargs(): not a dummyargK node",dargs) ;
    }

    p = dargs->nodeChild[1] ;

    while( p != NULL ) {
	if ( p->nodeKind != tokenK ) {
	    ast_err("typedummyargs(): non-token in argument list",dargs) ;
	}
	if ( p->u.Token->TokenKind == TNAME ) {
	    if (p->u.Token->symptr == NULL) {
		ast_err("typedummyargs(): null symbol pointer",dargs) ;
	    }
	    p->u.Token->symptr->type->TYPE(storage) = DUMMYARG ;
	    p->u.Token->symptr->type->TYPE(type) = impliedtype( p->u.Token->symptr->name ) ;
	    p->GENERIC(GenericType) = p->u.Token->symptr->type ;
	}
	p = p->nodeSibling ;
    }
}

typeexpression( node, structure )
NodePtr node ;
NodePtr structure ;  /* only used for dotKs , pass NULL otherwise */
{
    SymPtr sym ;
    NodePtr p, typenode ;
    NodePtr coerce3() ;
    NodePtr standard_type() ;

    if (node == NULL ) return ;
    if (node->GENERIC(GenericKind) == fieldrefK ) {
	if ( structure == NULL ) {
	    ast_err("typeexpression called for fielddeclk without a structure reference",node) ;
	    return ;
	} else 
	if ( structure->GENERIC(GenericKind) != structureK ) {
	    ast_err("unknown structure for field declaration",structure) ;
	    return ;
	} else
	if ( structure->nodeChild[3] != NULL ) {
	    if ( structure->nodeChild[3]->nodeKind != tokenK ||
		 structure->nodeChild[3]->GENERIC(GenericKind) != TNAME ) {
		ast_err("bad name node for structure", structure->nodeChild[3] ) ;
		return ;
	    }
	}
    }

    if (node->nodeKind == tokenK) {
    node->GENERIC(GenericType) = newTypeNode() ;
    switch ( node->u.Token->TokenKind ) {
	case TICON : 	node->GENERIC(GenericType)->TYPE(type) = standard_int ;break ;
	case TRCON : 	node->GENERIC(GenericType)->TYPE(type) = standard_real ;break ;
	case TDCON : 	node->GENERIC(GenericType)->TYPE(type) = standard_double ;break ;
	case TTRUE : 	node->GENERIC(GenericType)->TYPE(type) = standard_logical ;break ;
	case TFALSE : 	node->GENERIC(GenericType)->TYPE(type) = standard_logical ;break ;
	case TSTRING :	node->GENERIC(GenericType)->TYPE(type) = standard_character ;break ;
#if 0
	case TNAME :	node->GENERIC(GenericType)/*->TYPE(type)*/ = 
				node->u.Token->symptr->type/*->TYPE(type)*/ ; break ;
#endif
        /* 4/20/92 */
	case TNAME :
	    if ( node->u.Token->symptr->type->TYPE(type) == NULL )
	    {
		node->u.Token->symptr->type->TYPE(type) = 
			impliedtype( node->u.Token->symptr->name ) ;
	    }
	    node->GENERIC(GenericType) = node->u.Token->symptr->type ;
	    break ;
	default : break ;
    }
    } else {
    switch ( node->GENERIC( GenericKind ) ) {
	case idrefK :
	    sym = node->nodeChild[0]->u.Token->symptr ;
	    if ( sym->type->TYPE(type) == NULL) {
		sym->type->TYPE(type) = impliedtype( sym->name ) ;
	    }
            if (sym->type->TYPE(storage) == UNKNOWN )
	    {  /* 910131 */
                        sym->type->TYPE(storage) = LOCAL ;
	    }
	    node->GENERIC(GenericType) = sym->type ;
            if (node->nodeChild[1] != NULL)
	    {
	        NodePtr p ;
		    /* p should point to a parenK node */
                for (p=node->nodeChild[1]->nodeChild[1];p!=NULL;p=p->nodeSibling) {
                    if (p->nodeKind == tokenK && p->u.Token->TokenKind == TNAME) {
                        continue ;
                    }
                    typeexpression( p ) ;
 		}
	    }
/* 4/20/92 added to typecheck substringpartK expressions */
	    if ( CHILD( node,2 ) != NULL)
	    {
		if ( MINORKIND( CHILD( node, 2 ) ) == substringpartK )
		{
		    NodePtr p ;
		    typeexpression( CHILD( CHILD( node, 2 ), 1 ) ) ;
		    typeexpression( CHILD( CHILD( node, 2 ), 3 ) ) ;
		}
	    }

	    /* if something appears in an expression and it has a subscript, then
	       it has to be either an array or a function call.  If it's an array,
	       it will have been declared as such and its class will be known.
	       Otherwise, it will be unknown.
	       Also, make sure we aren't doing a lhs before we 
	       blithly call it a efunction because it might be an sfunction. */
	    if ( sym->type->TYPE(class) == UNKNOWN ) {
		if ( node->nodeChild[1] != NULL)  /* has a subscript */ {
		    if (!doing_lhs) {
			if ( isintr( sym->name ) ) {
			    sym->type->TYPE(class) = IFUNCTION ;
			    sym->type->TYPE(type) = 
				standard_type( typeintr(sym->name) ) ;
			} else {
		            sym->type->TYPE(class) = EFUNCTION ;
			}
		    }
	        } else { /* no subscript */
	    /* if something appears in an expression without a subscript then it 
	       must be a variable or parameter.  If it's a parameter it will have
	       been declared as such.  The only exception here would be if the
	       name of a function is appearing in an argument list.  We will have
	       to handle those separately.  */
	 	    sym->type->TYPE(class) = VARIABLE ;
		}
	    }
#ifdef COMMENTOUT
	    /* comented out 4-13-90, because RETURNID's are allowed in 
	       expressions (I was thinking of Pascal originally).  */
	    else 
		/* the only routine which sets doing_lhs is typeexnodes */
	    if (( sym->type->TYPE(class) == RETURNID ) && !doing_lhs) {
		type_err("name of function may not appear in expression",node) ;
	    }
#endif
	    break ;
	case dotK :
	    typeexpression( node->nodeChild[0], structure ) ;
	    /* let p point to the type of the lhs node, which should
	       be a pointer to a structureK node */
	    p = node->nodeChild[0]->GENERIC(GenericType)->TYPE(type) ;
	    if ( p->GENERIC(GenericKind) != structureK ) {
		type_err("not a record",node->nodeChild[0]->nodeChild[0]) ;
		break ;
	    }
	    node->EX(u.dot.structure) =
		node->nodeChild[0]->GENERIC(GenericType)->TYPE(type) ;
	    typeexpression( node->nodeChild[2], node->EX(u.dot.structure)) ;
	    /* dot gets type of its rhs */
	    node->GENERIC(GenericType) = node->nodeChild[2]->GENERIC(GenericType) ;
	    break ;
	case fieldrefK : {
	    char temp[256] ;
	    p = find_field( structure, GETTOKSTRING(node->nodeChild[0]->u.Token)) ;
	    if (p == NULL) {
		if (structure->nodeChild[3] != NULL) {
		    sprintf(temp,"field %s not in structure %s",
				GETTOKSTRING(node->nodeChild[0]->u.Token ),
				GETTOKSTRING(structure->nodeChild[3]->u.Token)
				) ;
		} else {
		    sprintf(temp,"field %s not in unnamed structure",
                                GETTOKSTRING(node->nodeChild[0]->u.Token )) ;
		}
		ast_err(temp, p) ;
	    } else {
	        node->GENERIC(GenericType) = p->GENERIC(GenericType) ;
	    }}
	    break ;
	case complexconstK :
	    {
	    NodePtr realp, imagp ;
	    if ((realp = node->nodeChild[1]) == NULL) {
		ast_err("no real part in complex constant",node) ;
		break ;
	    }
	    if ((imagp = node->nodeChild[3]) == NULL) {
		ast_err("no imaginary part in complex constant",node) ;
		break ;
	    }
	    typeexpression( realp ) ;
	    typeexpression( imagp ) ;
            node->GENERIC(GenericType) = newTypeNode() ;
	    node->GENERIC(GenericType)->TYPE(type) = standard_complex ;
	    }
	case binopK :
	    {
	    NodePtr lhs, rhs, oper ;
	    if ((lhs = node->nodeChild[0]) == NULL) {
		ast_err("no left hand side of binary operator",node) ;
		break ;
	    }
	    if ((oper = node->nodeChild[1]) == NULL) {
		ast_err("no operator in binary expression",node) ;
		break ;
	    }
	    if ((rhs = node->nodeChild[2]) == NULL) {
		ast_err("no right hand side of binary operator",node) ;
		break ;
	    }

	    typeexpression( lhs ) ;
	    typeexpression( rhs ) ;

	    switch ( oper->u.Token->TokenKind ) {
	    case TPLUS :
	    case TMINUS :
	    case TSTAR :
	    case TSLASH :
	    case TPOWER :
	        coerce3( node, lhs->GENERIC(GenericType), rhs->GENERIC(GenericType) ) ;
		break ;
	    case TAND :
	    case TOR :
	    case TXOR :
		node->GENERIC(GenericType) = newTypeNode() ;
		node->GENERIC(GenericType)->TYPE(type) = standard_logical ;
		break ;
	    case TEQ :
	    case TNE :
	    case TLT :
	    case TGE :
	    case TGT :
	    case TLE :
		node->GENERIC(GenericType) = newTypeNode() ;
		node->GENERIC(GenericType)->TYPE(type) = standard_logical ;
		break ;
	    case TCONCAT :
		node->GENERIC(GenericType) = newTypeNode() ;
		node->GENERIC(GenericType)->TYPE(type) = standard_character ;
		break ;
	    default : break ;
	    }

	    /*
	    node->GENERIC(GenericType) = coerce( lhs->GENERIC(GenericType ),
						 rhs->GENERIC(GenericType )) ;
	    node->GENERIC(GenericType) = coerce( lhs, rhs ) ;
	    */
	    }
	    break ;
	case parenK :
	    if ( node->nodeChild[1] != NULL ) {
	        typeexpression( node->nodeChild[1] ) ;
	        node->GENERIC(GenericType) = node->nodeChild[1]->GENERIC(GenericType) ;
	    }
	    break ;
	case unopK :
	    {
	    NodePtr operand ;
	    if ((operand = node->nodeChild[1]) == NULL) {
                ast_err("no operand for unary operator",node) ;
                break ;
            }
	    typeexpression( operand ) ;
	    node->GENERIC(GenericType) = operand->GENERIC(GenericType) ;
	    }
	    break ;
	case dorangeK :
	    {
	    /* it is not entirely plausible to give these nodes type, yet
	       we may wish to check type validity on opposite sides of an
	       equal in a do loop.  So we go ahead and give these nodes the
	       type of their arguments even though we generally will not 
	       use the derived type */
	    NodePtr strt, stp, incr ;
	    if ((strt = node->nodeChild[0]) == NULL) {
		ast_err("no starting range specified in do range", node) ;
		break ;
	    } else 
		typeexpression( strt ) ;
	    if ((stp  = node->nodeChild[2]) == NULL) {
		ast_err("no stopping range specified in do range", node) ;
		break ;
	    } else
		typeexpression( stp ) ;

	    coerce3( node, strt->GENERIC(GenericType), stp->GENERIC(GenericType) ) ;
	    }
	    break ;
	case implieddoinK :
	case implieddooutK :
	    {
	    NodePtr p ;
	    for (p = node->nodeChild[1]; p!= NULL; p = p->nodeSibling) {
		typeexpression( p ) ;
	    }
	    typeexpression( node->nodeChild[3] ) ; 	/* counter */
	    typeexpression( node->nodeChild[5] ) ; 	/* ranges */
	    }
	    break ;
	default : ast_err("bad node to typeexpression()",node) ;
    }
    }
}

NodePtr
impliedtype( name )
char * name  ;
{
    if (programnode != NULL) {
	return( programnode->DEF(u.program.implicit)[IMPL(*name)]) ;
    } else {
	ast_err("no program node",NULL) ;
	return(NULL) ;
    }
}

NodePtr
type_implied( programnode, name )
NodePtr programnode ;
char * name  ;
{
    if (programnode != NULL) {
	return( programnode->DEF(u.program.implicit)[IMPL(*name)]) ;
    } else {
	ast_err("no program node",NULL) ;
	return(NULL) ;
    }
}

sametype( node1, node2 )
NodePtr node1, node2 ;
{
    char * t1, * t2 ;
    char * gettype() ;

    if ( node1 == node2 ) return(1) ;

    if ( node1 == NULL || node2 == NULL ) return(0) ;

    if ((t1 = gettype( node1 )) == NULL ) {
	ast_err("can't determine type of node",node1) ;
    }
    if ((t2 = gettype( node2 )) == NULL ) {
	ast_err("can't determine type of node",node1) ;
    }
    if (strcmp( t1, t2 ) == 0) return(1) ;
    else return(0) ;
}


char * 
gettype( node )
NodePtr node ;
{
    if (node == NULL) return("unknown") ;

    switch (node->nodeKind) {
	case tokenK :
	    switch (node->u.Token->TokenKind) {
	        case TBYTE :
	        case TCHARACTER :
	        case TCOMPLEX :
	        case TDOUBLE :
	        case TINTEGER :
	        case TLOGICAL :
	        case TREAL :
		    return( GETTOKSTRING( node->u.Token ) ) ;
		    break ;
		/*case TSTRING :*/
	        default :
		    return( gettype(node->GENERIC(GenericType))) ;
		    break ;
		/*
	        default :
		    return("unknown") ;
		    break ;
		*/
	    } break ;

	case typeK :
	    switch (node->GENERIC(GenericKind)) {
		case structureK :
		    if (node->nodeChild[3] != NULL) {
                        return(GETTOKSTRING(node->nodeChild[3]->u.Token)) ;
                    } else {
                        return("unknown") ;
                    }
		    break ;
		default :
		    return( gettype( node->TYPE(type) ) ) ;
		    break ;
	    }
	default :
	    switch (node->GENERIC(GenericKind)) {
		case typespecK :
		    return( gettype( node->nodeChild[0] )) ;
		    break ;
		default :
		    return( gettype( node->GENERIC(GenericType) ) ) ;
		    break ;
	    }
    }
}


NodePtr
standard_type( name )
char * name ;
{
    if ( name == NULL ) return( NULL ) ;

    if ( strcmp( name, "integer" ) == 0 ) {
	return( standard_int ) ;
    } else 
    if ( strcmp( name, "real" ) == 0 ) {
	return( standard_real ) ;
    } else
    if ( strncmp( name, "double", 6 ) == 0 ) {
	return( standard_double ) ;
    } else
    if ( strcmp( name, "logical" ) == 0 ) {
	return( standard_logical ) ;
    } else
    if ( strcmp( name, "character" ) == 0 ) {
	return( standard_character ) ;
    } else {
	return( NULL ) ;
    }
}

int
getclass( node )
NodePtr node ;
{
    if (node == NULL) return(UNKNOWN) ;
    switch (node->nodeKind) {
	case typeK :
		    return( node->TYPE(class) ) ;
		    break ;
	default :
		    return( getclass( node->GENERIC(GenericType) ) ) ;
		    break ;
    }
}

int
getstorage( node )
NodePtr node ;
{
    if (node == NULL) return(UNKNOWN) ;
    switch (node->nodeKind) {
	case typeK :
		    return( node->TYPE(storage) ) ;
		    break ;
	default :
		    return( getstorage( node->GENERIC(GenericType) ) ) ;
		    break ;
    }
}


static char *numtypeprec[] = { "doubleprecision", "real", "integer" } ;
#define NUMERIC 3
#define CMPLEN 4

static int
DEX(S)
char * S ;
{ register int i ;
  for (i=0;i<NUMERIC;i++) {
      if (strncmp(S,numtypeprec[i],CMPLEN) == 0 ) return(i) ;
  }
  return(NUMERIC) ;
}


/* coerce()
   Given two type nodes, return the type that predominates.
*/
NodePtr 
coerce3( node, t1, t2 )
NodePtr node, t1, t2 ;
{
    char temp[80] ;
    char * s1, * s2 ;
    int i1, i2 ;

    if (node == NULL) {
	ast_err("null node passed to coerce3()",NULL) ;
	return ;
    }

    i1 = DEX(s1 = gettype( t1 )) ;
    i2 = DEX(s2 = gettype( t2 )) ;

    if (strcmp(s1,s2)==0) {
	node->GENERIC(GenericType) = t1 ;
	return ;
    }

    if (i1 >= NUMERIC || i2 >= NUMERIC) {
	sprintf(temp,"possibly incompatible types. cannot coerce %s and %s",s1, s2) ;
	type_err(temp,node) ;
/*	return ;     commented out 4-30-90, pass a type even if it's wrong */
    }

    if (i1 > i2)
	node->GENERIC(GenericType) = t2 ;
    else 
	node->GENERIC(GenericType) = t1 ;
}
