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
    printtree.c

    routine to print the AST produced by a parse

*/
#include <stdio.h>
#include "node.h"
#include "tokens.h"
#include "macros.h"

#define INDENTCREMENT 3

char * minorKind() ;  /* defined in minor.c */
char * showtype() ;

printtree( file, AST )
FILE *file ;
NodePtr AST ;
{
    fprintf(file,"Abstract_Syntax_Tree\n") ;
    printtree1( file, AST, 0, -2 ) ;
}

printtree1( file, AST, indent, id )
FILE *file ;
NodePtr AST ;
int indent, id ;
{
    int i ;

    if (AST != NULL) {
	if ( MAJORKIND(AST) == tokenK ) {
	    fprintf( file, "%4d", atoi(TOKBEGIN( AST )) ) ;
	    i = 4 ;
	} else {
	    i = 0 ;
	}
	for( ; i<indent ; i++ ) putc( ' ', file ) ;
	if ( id >= 0 ) printf(" %d ",id ) ;
	else if ( id == -1 ) printf(" . ") ;
	else  printf("   ") ;
	print_info( file, AST ) ;
	for( i=0 ; i<maxChildren; i++ ) 
	    if (AST->nodeChild[i])
		printtree1( file, AST->nodeChild[i], indent+INDENTCREMENT, i ) ;
	if (AST->nodeSibling)
	    printtree1( file, AST->nodeSibling, indent, -1 ) ;
    }
}

print_info( file, node )
FILE * file ;
NodePtr node ;
{
    char * majorKind(), * minorKind(), * otherinfo() ;

    if ( node == NULL ) return ;
    fprintf(file,"%s( %s ) %s\n",
        majorKind( node ),
	minorKind( node ),
	otherinfo( node ) ) ;
    fflush(file) ;
}

#define XTRA( Z ) \
	    { NodePtr t ; \
	    t = Z->GENERIC(GenericType) ; \
	    if ( t != NULL ) { \
    		char * showtype() , * showclass(), * showstorage() ; \
		strcat(temp," TYPE = ") ; \
		strcat(temp,showtype(t->TYPE(type))) ; \
		strcat(temp,", CLASS = ") ; \
		strcat(temp,showclass(t->TYPE(class))) ; \
		strcat(temp,", STORAGE = ") ; \
		strcat(temp,showstorage(t->TYPE(storage))) ; \
	    } return(temp) ; }

char *
otherinfo( node )
NodePtr node ;
{
    char temp[4096] ;
    SymPtr sym, getsym() ;

    temp[0] = '\0' ;

    if ( node->nodeKind == tokenK ) {
	strcpy( temp,GETTOKSTRING( node->u.Token )) ;
	switch( node->u.Token->TokenKind ) {
	    case TNAMEEQ :
	      strcat( temp, "=" ) ;
	      return(temp) ;
	      break ;
        /* constants with type */
            case TICON :
            case TRCON :
            case TDCON :
	    case TNAME :
            case TTRUE :
            case TFALSE :
            case TSTRING : XTRA( node ) ; break ;
	    default : return(temp) ;
	}
    }

    /* then it must be either an defK, exK, or typeK.
       some of these nodes have additional info. */
    switch( node->GENERIC( GenericKind ) ) {

	/* other nodes with type */
        case iddeclK :
        case fielddeclK :
	case idrefK :
	case dotK :
	case fieldrefK :
	case functionK :
	case subroutineK :
	case binopK :
	case unopK :
	case parenK :
	/*
	case mainK : 	XTRA(node) ; 	break ;
	*/
	case entryK : 	XTRA(node) ; 	break ;
        case programK : {
	    /* return list of implicit definitions */
            NodePtr p, prev ;
	    char low[2], high[2], line[80], c ;
	    high[1] = '\0' ; low[1] = '\0' ;
	    prev = node->DEF(u.program.implicit)[IMPL('a')] ;
	    line[0] = '\0' ;
	    temp[0] = '\0' ;
	    low[0] = 'a' ;
	    c = 'a' ;
	    do {
		p = node->DEF(u.program.implicit)[IMPL(c)] ;
		if ( !sametype(p, prev) ) {
		    if (prev != NULL ) {
                        strcat(temp,"\n          implicit ") ;
                        strcat(temp,showtype(prev)) ;
			strcat(temp," ") ;
			line[0] = '\0' ;
			strcat(line,low) ;
			if (c-1 != low[0]) {
			    high[0] = c-1 ;
			    strcat(line,"-") ;
			    strcat(line,high) ;
			}
			strcat(temp,line) ;
		    }
		    low[0] = c ;
		}
		prev = p ;
		c++ ;
            } while ( c-1 <= 'z' ) ;
            return(temp) ;
	    } break ;
	default : return("") ; break ;
    }
}
	    
char *
showtype( def )
NodePtr def ;
{
    char temp[40] ;
    if ( def == NULL ) return( "?" ) ;

    switch ( def->nodeKind ) {
	case tokenK :
	    switch ( def->u.Token->TokenKind ) {
		case TINTEGER : return("int") ;
		case TCHARACTER : return("char") ;
		case TREAL : return("real") ;
		case TDOUBLE : return("double") ;
		case TCOMPLEX : return("complex") ;
		case TLOGICAL : return("logical" ) ;
		default : return("?") ;
	    } break ;
	case defK :
	    switch ( def->u.Def->DefKind ) {
		case typedeclK : 
		    return(showtype(def->nodeChild[1])) ;
		case typespecK :
		    return(showtype(def->nodeChild[0])) ;
		default : return("?") ;
	    } break ;
	case typeK :
	    switch ( def->u.Type->TypeKind ) {
		case structureK :
		    {
		    if (def->nodeChild[3] != NULL) {
			return(GETTOKSTRING(def->nodeChild[3]->u.Token)) ;
		    } else {
			return("?") ;
		    }
		    }
		default : return(showtype(def->u.Type->type)) ;
#if 0
		default : return("?") ;
#endif
	    } break ;
	default : return("?") ;
    }
}

/* 4/16/92  boolean function.  true if type is implied.  false otherwise */
int
is_implicit_type( def )
NodePtr def ;
{
    if ( def == NULL ) 
    {
        return( -1 ) ;   /* cannot determine */
    }

    switch ( def->nodeKind ) {
	case defK :
	    switch ( def->u.Def->DefKind ) 
	    {
		case typedeclK : 
		    return(is_implicit_type(def->nodeChild[1])) ;
		case typespecK :
		    return (    def == standard_int 
			     || def == standard_real
			     || def == standard_logical
			     || def == standard_double
			     || def == standard_complex
			     || def == standard_character ) ;
		default : 
		    return(0) ;
	    } 
	    break ;
	case typeK :
	    return(is_implicit_type(def->u.Type->type)) ;
	    break ;
	default :
	    return(0) ;
    }
}


char *
showclass( class )	/* ( good advice in any situation ) */
int class ;
{
    /* the defined constants are found in node.h */
    switch ( class ) {
	case VARIABLE : return( "var" ) ; break ;
	case ARRAY: return( "arr" ) ; break ;
	case IFUNCTION: return( "ifun" ) ; break ;
	case SFUNCTION: return( "sfun" ) ; break ;
	case EFUNCTION: return( "efun" ) ; break ;
	case RETURNID: return( "ret id" ) ; break ;
	case SUBROUTINE: return( "sub" ) ; break ;
	case ENTRY: return( "ent" ) ; break ;
	case PARAMETER : return("param") ; break ;
	default : return("?") ; break ;
    }
}

char *
showstorage( storage )
int storage ;
{
    /* the defined constants are found in node.h */
    switch ( storage ) {
	case LOCAL : return("loc") ; break ;
	case SAVE : return("save") ; break ;
	case COMMON : return("comm") ; break ;
	case DUMMYARG : return("dumarg") ; break ;
	default : return("?") ; break ;
    }
}

char * 
majorKind( node )
NodePtr node ;
{
    switch (node->nodeKind) {
	case defK : return("defK") ;
	case exK : return("exK") ;
	case typeK : return("typeK") ;
	case tokenK : return("tokenK") ;
	default : return("unknown") ;
    }
}

