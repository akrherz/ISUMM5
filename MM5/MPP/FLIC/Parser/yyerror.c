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
/* yyerror.c SCCS(2.1 90/03/23 14:44:18) */

/* yyerror()

called from parser.  Included here instead in of grammar spec to
prevent need for lengthy recompiles when modifying.

*/
#include <stdio.h>
#include "node.h"
#include "y.tab.h"

extern FILE *errfile ;
extern char inp_line[] ;
extern char *field[] ;

int warn = 0  ;	/* if set issue warnings instead of errors */
int seekcomma = 0 ; /* set in f.format if looking for absent comma */

yyerror(s)
char * s ;
{
    switch (warn) {
	case 1 : 
#if 0
	    /* these are too annoying and they don't hurt anything 91-06-19 */
	    fprintf(stderr,"%s: ", field[FFILE] ) ;
	    fprintf(stderr,"WARNING: missing comma in format statement,") ; 
	    fprintf(stderr," line %s, col %s\n" ,field[FBEGINCARD]
						,field[FBEGINCOL] ) ;
#endif
	    break ;
	case 2 :
	    fprintf(stderr,"%s: ", field[FFILE] ) ;
	    fprintf(stderr,"WARNING: Bad or non-standard format spec,") ;
            fprintf(stderr," line %s, col %s\n" ,field[FBEGINCARD]
                                                ,field[FBEGINCOL] ) ;
            break ;
        default : 
	    err( "parser", s ) ;
	    break ;
    }
}

err(w,s)
char *w, *s ;
{
    char message[256] ;
    sprintf(message,
     "\n\n%s:\nFile: %s, Line: %s, Column: %s, Token: %s, Value: %s\n",
	 s,
	 field[FFILE],
	 field[FBEGINCARD],
	 field[FBEGINCOL],
	 field[FDESCRIP],
	 field[FTOKSTRING]
	 ) ;
    fprintf(errfile,"Error: %s:\n %s",w,message) ;
    exit(2) ;
}

comp_err(s)
char * s ;
{
    err("fatal compiler error", s) ;
    exit(1) ;
}

syn_err(s)
char * s ;
{
    err("syntax",s) ;
}

lexerr(s)
char * s ;
{
    err("lexical",s) ;
}

/* ast_err() error routine used by typechecking pass (type.c:typetree())
   and subsidiary routines which deal with the Abstract Syntax
   Tree and its nodes.   Added 3-23-90.
*/

ast_err(s, p)
char * s;	/* message */
NodePtr p ;	/* pointer to offending node */
{
    fprintf(errfile,"AST Error: %s\n",s ) ;
    if (p != NULL) {
	print_info( stderr, p ) ;   /* declared in printtree.c */
    }
}

type_err( s, p )
char * s;	/* message */
NodePtr p ;	/* pointer to offending node */
{
    int i ;
    if ( p != NULL ) {
	switch ( p->nodeKind ) {
	case tokenK :
	    if (p->u.Token->TokenKind == TNAME) {
		fprintf(errfile,"%s: ", TOKFILE(p)) ;
                fprintf(errfile,"type warning: %s ",s) ;
		fprintf(errfile," on line %s.  ",GETBEGINCARD(p->u.Token));
		fprintf(errfile,"Ident = %s\n", GETTOKSTRING(p->u.Token)) ;
		return(1) ;
	    } else {
		fprintf(errfile,"%s: ", TOKFILE(p)) ;
                fprintf(errfile,"type warning: %s ",s) ;
		fprintf(errfile," on line %s.\n",GETBEGINCARD(p->u.Token));
		return(1) ;
	    }
	    break ;
	case typeK :
	    break ;
	default : 
	    /* try to find a token node somewhere among the children of this
	       node, since any token node would contain a line reference */
	    for (i=0; i< maxChildren ; i++ ) {
		if (type_err( s, p->nodeChild[i] )) return(1) ;
	    } break ;
        }
    }
    fprintf(errfile,"type warning: %s\n",s) ;
    return(0) ;
}

/* general purpose error routine tool developers can call, 4-30-90 */

static int user_mess( messtype, s, p )
char *messtype, *s ;
NodePtr p ;
{
    int i ;
    if ( p != NULL ) {
	switch ( p->nodeKind ) {
	case tokenK :
	    if (p->u.Token->TokenKind == TNAME) {
                fprintf(errfile,"%s%s ",messtype,s) ;
		fprintf(errfile,"Line %s.  ",GETBEGINCARD(p->u.Token));
		fprintf(errfile,"Ident = %s\n", GETTOKSTRING(p->u.Token)) ;
		return(1) ;
	    } else {
                fprintf(errfile,"%s%s ",messtype,s) ;
		fprintf(errfile," Line %s.\n",GETBEGINCARD(p->u.Token));
		return(1) ;
	    }
	    break ;
	case typeK :
	    break ;
	default : 
	    /* try to find a token node somewhere among the children of this
	       node, since any token node would contain a line reference */
	    for (i=0; i< maxChildren ; i++ ) {
		if (user_mess( messtype, s, p->nodeChild[i] )) return(1) ;
	    } break ;
        }
    }
    fprintf(errfile,"%s%s ",messtype,s) ;
    return(0) ;
}

user_err( s, p )
char * s;       /* message */
NodePtr p ;     /* pointer to offending node */
{
    return(user_mess("error: ",s,p)) ;
}

user_warn( s, p )
char * s;       /* message */
NodePtr p ;     /* pointer to offending node */
{
    return(user_mess("warning: ",s,p)) ;
}

