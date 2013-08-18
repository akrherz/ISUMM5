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

/* scanner.c

This routine is not a linked part of the scanner, but functions as the
scanner for routines such as the parser.  It provides a 'yylex()' routine
which reads the file of token data output by the actual scanner.
As far as a yacc generated parser is concerned, this routine is 
indistinguishable from an actual lex generated scanner.  It is included
in the scanner source directory because changes to the actual scanner
may necessitate changes to this routine, but externally, the whole thing
should be a black box (if desired, it is possible to recombine the
scanner and parser into a single module by substituting the actual
scanning yylex for this one.)

In this way, the actual scanner may be kept entirely distinct from
the parser, eliminating the intricate parser -- scanner dependancies
that plague many FORTRAN recognizers.

This routine assumes that infile has been declared globally and set elsewhere.

At the present time (January 1990), the routine expects input data of one
token per line in the following format:

     tokseq:file:tokcode:begincard:begincol:endcard:endcol:descrip:tokstring

     tokseq	token sequence number (i, for the ith token)
     file	name of source file from which this token was read
     tokcode	integer value of token as defined in y.tab.h
     begincard	number of card where token started
     begincol	number of column where token started
     endcard	number of card where token ended
     endcol	number of column where token ended
     descrip	name of token (for debugging purposes only)
     tokstring	string which scanner recognized as being token

The tokstring is not necessarily as it appears in the source file.  It
will have been compressed to remove white space, all upper case characters
will have been changed to lower case except in the case of comments and
quoted strings.   Quoted strings are returned WITH delimiting quote marks.

Except when a comment appears in the middle of a statement, comment 
tokens appear where they are found.  When a comment occurs within a 
single FORTRAN statement, the comment token appears after all tokens for
the statement have been displayed.  The location information for starting
and ending cards and columns, however, remains intact.

Lines for tokens from included files appear at the point of the include.
Tokens for the include statements themselves are preserved and presented
immediately prior to the included tokens.

*/

#include <stdio.h>
#include "defines.h"
/* next line causes the definition for yylval to be included */
#include "node.h"
#include "y.tab.h"
#include "pushback.h"

#ifndef STANDALONE
extern File *infile ;		/* defined in pushback.h */
extern int in_io_control ;
extern int in_format_spec ;	/* defined in f.y */
#else
File *infile ;
FILE *errfile ;
YYSTYPE yylval ;
int in_io_control = 0 ;
#endif

char yytext[YYLMAX] ;
int  card_begin, card_end ;
int  col_begin, col_end ;

#define LINESIZE 	2 * CARDLEN

char inp_line[LINESIZE]  ;  /* big */
char dummyline[LINESIZE] ;
char *field[NUMFIELDS] ;

TokInfoPtr tokenlisthead, tokenlisttail ;

void * myMalloc() ;
    
yylex()
{
    int i,
	tok ;
    int inlength ;
    char * yytext ;
    TokInfoPtr tokenrec ;
    NodePtr newTokNode() ;

    	/*  read in next line */
top:
    if ( pb_fgets( inp_line, LINESIZE, infile ) == 0 ) return(TEOF) ;

    inlength = strlen( inp_line ) ;
    tokenrec = ( struct TokenRecType * ) myMalloc( sizeof (struct TokenRecType ) ) ;
    tokenrec->inp_line = ( char * ) myMalloc( inlength + 1 ) ;

    strcpy( tokenrec->inp_line , inp_line) ;
    setfields( tokenrec->inp_line, tokenrec->field) ;
    for (i=0;i<NUMFIELDS;i++)field[i]=tokenrec->field[i] ;

    /* add to list */
    if (tokenlisttail == NULL) {
	tokenlisthead = tokenlisttail = tokenrec ;
    } else {
	tokenlisttail->next = tokenrec ;
	tokenlisttail = tokenrec ;
    }

    tok = atoi(GETTOKCODE(tokenrec)) ;
    yytext = GETTOKSTRING(tokenrec) ;

    switch (tok) {
	case TLABEL  : {
		    SymPtr p ;
		    int i = 0  ;
                    sscanf(yytext , "%d", &i ) ;  /* ensures uniformity */
		    sprintf(yytext, "%d", i) ;    /* for string compares */
		    p = SYMGET( yytext ) ; 
		    if (p->label_do_refs > 0) { 
			fixcontinues(p->label_do_refs,yytext) ;
			tok = TSOUGHTLABEL ;
		        p->label_do_refs = -1 ;
		    }
		    tokenrec->symptr = p ;
		  } break ;
	case TEND    :
        case TNAME   : {
		    if (in_io_control) {
			if (lookahead() == TEQUALS) {
			    pb_fgets( dummyline, LINESIZE, infile ) ;   /* discard next token */
			    tok = TNAMEEQ ;
			}
		    }
		    tokenrec->symptr = SYMGET( yytext ) ;
		  } break ;
	case TCOMMENT :
	case TBLANK : {  /* ignore */
		    goto top ;
		  } break ;
	default : break ;
    }
/* 5/8/96 */
    if ( tok == TCOMMENT || tok == TBLANK )
    {
      tok = TEOF ;
    }
    tokenrec->TokenKind = tok ;
    yylval.node = newTokNode(tokenrec) ;
    return( tok ) ;
}


/* setfields
   sets the field array to point to the individual fields of inp_line
*/
setfields( inp_line, field ) 
char inp_line[] ;
char *field[] ;
{
    char temp[512] ;
    int i ;
    char * p ;

    strcpy(temp,inp_line) ;
    i = 0 ;
    p = inp_line ;
    strip_nl( inp_line ) ;

    while ( i < NUMFIELDS )
    {
        field[i] = p ;
	i++ ;
	if ( i < NUMFIELDS )
	{
          while (*p != ':' && *p != '\0')
          {
              p++ ;
          }
	}
	else
	{
          while (*p != '\0')	/* colons are allowed in the lexeme itself */
          {
              p++ ;
          }
	}

    /* assert : *p is ':' or \0 (end of string) */

        if ( *p == ':' )
        {
            *p = '\0' ;
        }
        else
        {
            break ;   /* quit while loop whether done or not */
        }
        p++ ;
    }
/* assert : *p is '\0', i == NUMFIELDS, or both */
/* if i < NUMFIELDS, an error has occured; i can be greater than
   NUMFIELDS if the string contained a : */

    if ( i < NUMFIELDS )
    {
	fprintf(stderr,"i = %d, NUMFIELDS = %d\n", i, NUMFIELDS ) ;
        sprintf(temp,"%s < setfields() choked",temp) ;
        comp_err(temp) ;
        exit(1) ;
    }

#if 0
	/*  set pointers to each field, and change ':' to null */
    char temp[256] ;
    int i = 0 ;
    char *p = inp_line ;
    int startfield = 1 ;
    strcpy(temp,inp_line) ;
    while ( *p && i <= NUMFIELDS ) {
	if (*p == ':') {
	    if (startfield) {
		if (i < NUMFIELDS)
		    field[i++] = p ;
	    }
	    startfield = 1 ;
	    *p = '\0' ;
	} else if (*p == '\n') { /* pb_fgets includes the newline; dump it */
	    *p = '\0' ;
	} else if (startfield) {
	    startfield = 0 ;
	    if (i < NUMFIELDS) {
	        field[i++] = p ;
	    } else {
		/*
		sprintf(temp,"%s < setfields() choked",temp) ;
		comp_err(temp) ;
		*/
		break ;
	    }
	}
	p++ ;
    }
#ifdef DEBUG
    for (i = 0 ; i < NUMFIELDS ; i ++ ) 
	printf("%s\n",field[i]) ;
#endif
#endif
}

/* return the integer value of the next token */
lookahead()
{
    int tok ;
    char look[LINESIZE] ;
    char save[LINESIZE] ;
    char *fld[NUMFIELDS] ;
    register char *p ;

    if (pb_fgets( look, LINESIZE, infile ) == 0 ) return(0) ;

    strcpy(save,look) ;
    setfields(look, fld) ;
    tok = atoi(fld[FTOKCODE]) ;
    pb_unfgets(save,LINESIZE,infile) ;
    return(tok) ;
}

/* set up the input so that the scanner thinks it is reading n-1
   consecutive SOUGHTLABEL-continue statements */
fixcontinues( n, label )
int n ;
char * label ;
{
    int i ;
    char tcontinue[60] ;
    char tsoughtlabel[60] ;
    char store[LINESIZE] ;
    char * m ;

/* don't bother unless n > 1 */

    if (n <= 1) return ;

    if (((i = lookahead()) == TLABEL) || ( i == 0)) {
        int j ;
        char tcontinue[60] ;
        char tsoughtlabel[60] ;
        sprintf(tcontinue,   "0:fake:%d:0:0:0:0:TCONTINUE:\n",TCONTINUE ) ;
        sprintf(tsoughtlabel,"0:fake:%d:0:0:0:0:TSOUGHTLABEL:%s\n",TSOUGHTLABEL,label) ;
        for (j = 0 ; j < n-1 ; j++) {
	    if (pb_unfgets(tcontinue,LINESIZE,infile)<0) 
		comp_err("pb_unfgets() in fixcontinues()") ;
	    if (pb_unfgets(tsoughtlabel,LINESIZE,infile)<0) 
		comp_err("pb_unfgets() in fixcontinues()") ;
	}
    } else {
        pb_fgets( store, LINESIZE, infile ) ;
	fixcontinues( n, label ) ;  /* recurse */
	if (pb_unfgets(store,LINESIZE,infile)<0) 
	    comp_err("pb_unfgets() in fixcontinues()") ;
    }
}

/* seeklabel()
   Called from parser in semantic action for DO label_ref etc.
   The arguemtn sym is a pointer to the symbol table node for
   the label in question.
*/
seeklabel ( sym )
SymPtr sym ;
{
    int do_ref ;

    if (sym == NULL) 
        comp_err("seeklabel(): symbol table returned null label") ;

    do_ref = sym->label_do_refs ;

/* once the label has been matched, the scanner marks this field with
   a -1 to prevent its use again be a do loop. */
    if (do_ref < 0) {
        syn_err("improper label reference in DO statement") ;
        return ;
    }
    do_ref++ ;
    sym->label_do_refs = do_ref ;
}

/* 6-25-91; error recover on syntax errors -- clear input to
   next label token */
n32_errorfix()
{
    int la = lookahead() ;
    /*
    fprintf(stderr,"n32_errorfix(): lookahead returns %d\n%s\n",la,inp_line ) ;
    */
    while ( la != TLABEL )
    {
	la = yylex() ;
	/*
        fprintf(stderr,"n32_errorfix(): yylex returns %d\n%s\n",la,inp_line ) ;
	*/
	la = lookahead() ;
	/*
        fprintf(stderr,"n32_errorfix(): lookahead returns %d\n%s\n",la,inp_line ) ;
	*/
    }
}
