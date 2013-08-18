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

/* file: node.h

    created 11-22-89	michalak@atlantis.ees.anl.gov

*/

/* 
    Header info for symbol table module.

*/

#include "edt.h"

typedef struct symnode * SymPtr ;
typedef struct Node *NodePtr ;
typedef struct DefRec * DefPtr ;
typedef struct ExRec * ExPtr ;
typedef struct TypeRec * TypePtr ;
typedef struct GenericRec * GenericPtr ;


/* see documentation for references to categories of symnode info, 3.20.90 */

struct symnode {
    char * name ;		/* lexeme */
    SymPtr next ;		/* pointer to next node in symbol table */
    int  label_do_refs ;	/* number do loops terminating on this label */

    /* category A */		/* common names */

    /* category B */		/* structure names */
    NodePtr structdef ;

    NodePtr type ;		/* pointer to type node */

    /* category D */		/* sfunction dummy args */

} ;

#define TRUE 		1
#define FALSE		0
#define UNKNOWN		-1 /* intial value */

/* class values, used in symnode category C, 3-20-90 */

#define VARIABLE 	1001 /* simple (zero-D) variable */
#define ARRAY		1002 /* array */
#define	IFUNCTION	1003 /* intrinsic function */
#define SFUNCTION	1004 /* statement function */
#define EFUNCTION 	1005 /* external function */
#define RETURNID	1006 /* name of function used to return vals */
#define SUBROUTINE	1007 /* subroutine */
#define ENTRY		1008 /* entry */
#define PARAMETER	1009  /* defined constant */

/* storage values, used in symnode category C, 3-20-90 */
#define LOCAL		1010   /* local */
#define SAVE		1011  /* save local */
#define COMMON		1012  /* common */
#define DUMMYARG	1013  /* dummy argument (functions and subs only) */

#ifndef SYMBOL_TABLE
extern SymPtr SYMGET() ;
extern SymPtr newnode() ;
extern char ** nodename() ;
extern SymPtr * nodenext() ;
#endif

extern NodePtr standard_int ;
extern NodePtr standard_real ;
extern NodePtr standard_double ;
extern NodePtr standard_complex ;
extern NodePtr standard_logical ;
extern NodePtr standard_character ;

/*

    Header for scanner module 

*/

#define	    FTOKSEQ    0
#define     FFILE     1
#define     FTOKCODE  2
#define     FBEGINCARD        3
#define     FBEGINCOL 4
#define     FENDCARD  5
#define     FENDCOL   6
#define     FDESCRIP  7
#define     FTOKSTRING        8

#define NUMFIELDS 9

typedef struct TokenRecType * TokInfoPtr ;

struct TokenRecType {
    TokenKnd TokenKind ;
    NodePtr  TokenType ;
    TokInfoPtr next ;
    char * inp_line ;
    char * field[NUMFIELDS] ;
    SymPtr symptr ;		/* for nodes with symbolic info */
} ;

#define	    GETTOKSEQ(p)	p->field[FTOKSEQ]
#define     GETFILE(p)		p->field[FFILE]
#define     GETTOKCODE(p)	p->field[FTOKCODE]
#define     IGETTOKCODE(p) 	atoi(p->field[FTOKCODE])
#define     GETBEGINCARD(p)	p->field[FBEGINCARD]
#define     GETBEGINCOL(p)	p->field[FBEGINCOL]
#define     GETENDCARD(p)	p->field[FENDCARD]
#define     GETENDCOL(p)	p->field[FENDCOL]
#define     GETDESCRIP(p)	p->field[FDESCRIP]
#define     GETTOKSTRING(p)	p->field[FTOKSTRING]

/* 
    List Nodes.   (added 3-21-90, with type checking phase)

    The following nodes are used solely to make secondary
    linked lists of struct Node nodes.  They consist of only
    two fields, a NodePtr and a next field pointing to the
    next list node of type struct list.

    New list nodes can be gotten with a call to newListNode()
    defined in node.c.
*/

typedef struct List * ListPtr ;

struct List {
    union {
        NodePtr node ;
	SymPtr  sym ;
    } u ;
    ListPtr next ;
} ;

/* 

    Header info for AST nodes

*/

#define maxChildren 16 

#define boolean int

/* size of array for implicit definitions */
#define IMPLSIZE 27 	/* 27th pointer always null */
/* macro to use for indexing implicit array, below */
#define IMPL(c)   ((c >= 'a' && c <= 'z')?(c - 'a'):IMPLSIZE-1)

struct DefRec {
    DefKnd  DefKind ;
    NodePtr DefType ;
    union {
	struct {
	    char ** symtab ;
		/* array of pointers to implicit type definitions */
	    NodePtr implicit[IMPLSIZE] ; 
	} program ;
	struct {
	    SymPtr name ;
	    NodePtr length ;
	    NodePtr parent ;
	} decl ;  /* for both fielddeclK and iddeclK nodes */
	struct {
	    SymPtr name ;
	} record ;
	struct {
	    SymPtr name ;
	} common ;
	struct {		/* added 910305 */
	    SymPtr name ;
	} namelist ;
	struct {
	    char low ;
	    char high ;
	} implrange ;
    } u ;
} ; 

struct ExRec {
    ExKnd  ExKind ;
    NodePtr ExType ;
    union {
	struct {
	    NodePtr def ;
	} id ;
	struct {
	    SymPtr message ;
	} stop ;
	struct {
	    NodePtr structure ;
	} dot ;
    } u ;
} ; 

struct TypeRec {
    TypeKnd  TypeKind ;
    NodePtr TypeLen ;

    /* category C ( variables, arrays, functions, etc ) information */
    NodePtr type ;		/* pointer to type declaration */
    NodePtr dimensions ;	/* pointer to dimension declaration */
    NodePtr length ;		/* length specification */
    NodePtr common ;		/* pointer to common name, if any */
    NodePtr paramval ;		/* pointer to value of parameter symbols */
    int	    class ;		/* indicator of class (see below) */
    int	    storage ;		/* indicator of storage (see below) */
    int	    lhs ;		/* boolean.  1, id on LHS or input list */

    union {
	struct {
	    SymPtr name ;
	} structure ;
    } u ;
} ; 

struct GenericRec {
    int     GenericKind ;
    NodePtr GenericType ;
} ;

struct Node {
    NodePtr nodeChild[ maxChildren ] ;
    NodePtr nodeParent ;	/* added 5-29-90 */
    NodePtr longSibling ;	/* 2-20-90, see comment in 
				   link_nodes() (f_main.c)
				   5-29-90.  This pointer
				   doubles as back pointer to
				   prev sibling in final AST */
    NodePtr nodeSibling ;
    NodePtr user_def ;		/* user defined pointer if needed */
    int     user_int ;		/* user defined tag field if needed */
    NodeKnd nodeKind ;		/* tag for major kind of node this is */
    union {
	TokInfoPtr Token ;
	ExPtr  Ex ;
	DefPtr Def ;
	TypePtr Type ;
	GenericPtr Generic ;
    } u ;
} ;

NodePtr newExNode(), newDefNode(), newTypeNode() ;
NodePtr descend_tree() ;
NodePtr find_field() ;

/* macros used by the routines in f_*.c files */

#define EX(s)  u.Ex->s
#define DEF(s) u.Def->s
#define TYPE(s) u.Type->s
#define GENERIC(s) u.Generic->s

#define EXNODE(q) (((q)->nodeKind==exK)?1:0)
#define DEFNODE(q) (((q)->nodeKind)==defK)?1:0)
#define TYPENODE(q) (((q)->nodeKind==typeK)?1:0)
#define TOKNODE(q) (((q)->nodeKind==tokenK)?1:0)

extern NodePtr sixteen_kids() ;

#define fifteen_kids(X,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O )\
	sixteen_kids(X,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,NULL)
#define fourteen_kids(X,A,B,C,D,E,F,G,H,I,J,K,L,M,N )\
	fifteen_kids(X,A,B,C,D,E,F,G,H,I,J,K,L,M,N,NULL )
#define thirteen_kids(X,A,B,C,D,E,F,G,H,I,J,K,L,M )\
	fourteen_kids(X,A,B,C,D,E,F,G,H,I,J,K,L,M,NULL )
#define twelve_kids(X,A,B,C,D,E,F,G,H,I,J,K,L )\
	thirteen_kids(X,A,B,C,D,E,F,G,H,I,J,K,L,NULL )
#define eleven_kids(X,A,B,C,D,E,F,G,H,I,J,K )\
	twelve_kids(X,A,B,C,D,E,F,G,H,I,J,K,NULL )
#define ten_kids(X,A,B,C,D,E,F,G,H,I,J )\
	eleven_kids(X,A,B,C,D,E,F,G,H,I,J,NULL )
#define nine_kids(X,A,B,C,D,E,F,G,H,I )\
	ten_kids(X,A,B,C,D,E,F,G,H,I,NULL )
#define eight_kids(X,A,B,C,D,E,F,G,H )\
	nine_kids(X,A,B,C,D,E,F,G,H,NULL )
#define seven_kids(X,A,B,C,D,E,F,G )\
	eight_kids(X,A,B,C,D,E,F,G,NULL )
#define six_kids(X,A,B,C,D,E,F )\
  	seven_kids(X,A,B,C,D,E,F,NULL )
#define five_kids(X,A,B,C,D,E )\
  	six_kids(X,A,B,C,D,E,NULL )
#define four_kids(X,A,B,C,D )\
  	five_kids(X,A,B,C,D,NULL )
#define three_kids(X,A,B,C )\
  	four_kids(X,A,B,C,NULL )
#define two_kids(X,A,B )\
  	three_kids(X,A,B,NULL)
#define one_kid(X,A )  \
  	two_kids(X,A,NULL )
#define no_kids(X) \
	one_kid(X,NULL)


/* added 5-29-90, external declaration for walk_level global
   variable declared in traverse.c in connect with the 
   walk_statements tree walking routine.  This gives tool develepers
   access to the current nesting level.  Top level = 0 . */

extern int walk_level ;

/* added 4-26-90.... Node handling macros to make things
   easier and more transparent for tool developers */

#include "macros.h"
