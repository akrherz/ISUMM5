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
/* getstat.c SCCS(1.2 90/06/06 16:37:18) */

#include <stdio.h>
#include "n32.h"

#define LINES 40
#define LEN   256

static char temp[LINES][LEN] ;

char **
getstat(node, lines, buf)
NodePtr node ;
int    lines ;	/* number of lines consider printing (0 is this line only) */
char   * buf ;
{
    int lineno, i ;
    NodePtr tok, firsttok() ;

    *buf = '\0' ;
    if (node == NULL) return ;

			/* get line number from first token */
    if ((tok = firsttok( node )) == NULL)
	return((char **)temp) ;
    lineno = atoi(TOKBEGIN(tok)) ;

    initTemp() ;
    getstat1( node, lines, temp, lineno ) ;
    temptobuf( buf,temp ) ;
}

getstat1(node,lines,temp,lineno)		/* recursive */
NodePtr node ;
int  lines ;
char temp[][LEN] ;
int  lineno ;
{
    int i ;
    char token_str[256] ;
    NodePtr p ;
    if (node == NULL) return ;
    if (MAJORKIND(node) == tokenK) {
	strcpy( token_str, TOKSTRING( node )) ;
	if ( MINORKIND(node) == TLABEL && !strcmp(token_str, "0") )
	{
	    strcpy(token_str," ") ;
	}
        if (atoi(TOKBEGIN(node)) <= lineno + lines) {
	    if (atoi(TOKBEGIN(node))-lineno >= 0) {
                strinsert(temp[atoi(TOKBEGIN(node))-lineno],token_str,atoi(TOKCOLS(node))) ;
	    }
        }
    }
    for ( i = 0 ; i < maxChildren ; i++ ) {
	if ((p = CHILD(node,i)) == NULL) continue ;
	getstat1(p,lines,temp,lineno) ;
    }
    if ( !STATEMENT(SIBLING( node ))) {
	getstat1(SIBLING(node),lines,temp,lineno) ;
    }
    return ;
}


strinsert( temp, str, col ) 
char *temp,
     *str ;
int  col ;
{
    char * p, *q ;
    p = temp + col - 1 ;
    q = str ;
    for ( ; *q ; q++ )
	*p++ = *q ;
}

NodePtr
firsttok(node)
NodePtr node ;
{
    int i ;
    NodePtr rett ;
    if (node == NULL) return(NULL) ;
    if (MAJORKIND(node) == tokenK) return( node ) ;
    for (i = 0 ; i < maxChildren ; i++ ) {
        if ((rett =  firsttok(CHILD(node,i))) == NULL ) continue ;
	if (MAJORKIND( rett ) == tokenK) 
	    return(rett)  ;
    }
    return(NULL) ;
}

NodePtr
lasttok(node)
NodePtr node ;
{
    int i ;
    NodePtr rett, sib ;
    if (node == NULL) return(NULL) ;
    if ((sib = SIBLING(node)) != NULL) {
	if (!STATEMENT(sib)) {
	    if (( rett = lasttok(sib)) == NULL) return  ;
	    if (MAJORKIND( rett ) == tokenK) 
		return(rett) ;
	}
    }
    if (MAJORKIND(node) == tokenK) return( node ) ;
    for (i = maxChildren-1 ; i >= 0 ; i--) {
	if ((rett = lasttok( CHILD(node, i) )) == NULL) continue ;
	if (MAJORKIND( rett ) == tokenK) 
	    return(rett) ;
    }
}


initTemp()
{
    int i, j ;
    for (i=0;i<LINES;i++)
    for (j=0;j<LEN;j++)
	temp[i][j] = ' ' ;
}


temptobuf( buf, temp )
char buf[] ;
char temp[][LEN] ;
{
	/* cursors */
    int tp;	/* pointer to line in temp */
    int tc;	/* pointer to column in temp */
    int te;	/* last column with anything */
    int bp;	/* pointer into buf */
    int bp1 ;	/* 2nd pointer into buf */

    bp = 0 ;
    for ( tp = 0 ; tp < LINES ; tp++ ) {
	for (te = LEN-1; te >= 0 ; te-- ) {
	    if (temp[tp][te] != ' ')
		break ;
	}
	for (tc = 0 ; tc <= te ; tc++ ) {
	    /* add continuations for all but first line */
	    if ( tp > 0 && tc == 5 && temp[tp][tc] == ' ' )
		temp[tp][tc] = '+' ;
	    buf[bp++] = temp[tp][tc] ;
	}
	buf[bp++] = '\n' ;
    }
    bp-- ;
    for ( bp1 = bp ; bp >= 0 ; bp -- ) {
	if ( buf[bp] != ' ' &&
	     buf[bp] != '\n' ) continue ;
	if ( buf[bp] == '\n' ) {
	    bp1 = bp ; 
	    continue ;
	}
	break ;
    }
    if ( bp >= 0 ) {
	buf[bp1+1] = '\0' ;
    } else {
	buf[0] = '\0' ;
    }
}


char *
strip_nl(s)
char * s ;
{
    char * p ;
    for (p = s ; *p ; p++ ) ;
    for (; *p != '\n' && p >= s ; p-- ) ;
    if ( *p == '\n') *p = '\0' ; 
    return(s) ;
}

char *
strip_lead(s)
char * s ;
{
    char * p ;
    for (p = s ; *p ; p++ ) {
	if (*p != ' ') break ;
    }
    return(p) ;
}
