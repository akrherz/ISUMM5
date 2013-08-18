/* %M% SCCS(%R%.%L% %D% %T%) */

/* file: lexin.c
      routines get linked into the lexer and replace input() and unput()

	created:	11-22-89, michalak@atlantis.ees.anl.gov
*/

#include <stdio.h>
#include "tokens.h"
#include "defines.h"
#include "h.h"

#define PUSHBACKSTACKSIZE  1024
/***************************/
/* globally accessible data */
int firsttok ;	/* indicates if on first token of a statement */
int inparen ;	/* indicates level of parentheses >= 0 */

/***************************/
/* data structure contains label and 1 complete statement minus comments and
   plus all continuations, icol is current position in card, ncol is
   pointer to end of card.  */
static char card [ NCONT * CARDLEN ] ;
static char pbstack [ PUSHBACKSTACKSIZE ] ;
static int pbsp = 0 ;
static int iftype ;		/* set by chkcontext(), used by getkeyword() */
static int icol = 0 , ncol = 0 ;
/***************************/

/***************************/
static int firsttime = 1 ;	/* for first card */
/***************************/


static struct keyword {
    char name[20] ;
    int  token ;
    struct keyword * next ;
} *keytable[26] ;

static int firstkey = 1 ;

static struct keyword keywords[] =
{
	{ "assign",  TASSIGN, NULL},
	{ "automatic",  TAUTOMATIC, NULL},
	{ "backspace",  TBACKSPACE, NULL},
	{ "blockdata",  TBLOCK, NULL},
	{ "byte",  TBYTE, NULL},
	{ "call",  TCALL, NULL},
	{ "character",  TCHARACTER, NULL},
	{ "close",  TCLOSE, NULL},
	{ "common",  TCOMMON, NULL},
	{ "complex",  TCOMPLEX, NULL},
	{ "continue",  TCONTINUE, NULL},
	{ "data",  TDATA, NULL},
	{ "decode",  TDECODE, NULL},
	{ "dimension",  TDIMENSION, NULL},
	{ "doubleprecision",  TDOUBLE, NULL},
	{ "doublecomplex", TDCOMPLEX, NULL},
	{ "dowhile", TDOWHILE, NULL },
	{ "do", TDO, NULL },
	{ "elseif",  TELSEIF, NULL},
	{ "else",  TELSE, NULL},
	{ "encode",  TENCODE, NULL},
	{ "endfile",  TENDFILE, NULL},
	{ "endif",  TENDIF, NULL},
	{ "end",  TEND, NULL},
	{ "enddo", TENDDO, NULL} ,
	{ "endstructure", TENDSTRUCT, NULL} ,
	{ "endunion", TENDUNION, NULL } ,
	{ "endmap", TENDMAP, NULL } ,
	{ "entry",  TENTRY, NULL},
	{ "equivalence",  TEQUIV, NULL},
	{ "external",  TEXTERNAL, NULL},
	{ "format",  TFORMAT, NULL},
	{ "function",  TFUNCTION, NULL},
	{ "goto",  TGOTO, NULL},
	{ "if", TIF, NULL},
	{ "implicit",  TIMPLICIT, NULL},
	{ "include",  TINCLUDE, NULL},
	{ "inquire",  TINQUIRE, NULL},
	{ "intrinsic",  TINTRINSIC, NULL},
	{ "integer",  TINTEGER, NULL},
	{ "logical",  TLOGICAL, NULL},
	{ "map", TMAP, NULL},
	{ "namelist", TNAMELIST, NULL},
	{ "none", TNONE, NULL},
	{ "open",  TOPEN, NULL},
	{ "parameter",  TPARAM, NULL},
	{ "pause",  TPAUSE, NULL},
	{ "pointer",  TPOINTER, NULL}, /* cray extension added 4/15/92 */
	{ "print",  TPRINT, NULL},
	{ "program",  TPROGRAM, NULL},
	{ "punch",  TPUNCH, NULL},
	{ "read",  TREAD, NULL},
	{ "real",  TREAL, NULL},
	{ "record", TRECORD, NULL},
	{ "return",  TRETURN, NULL},
	{ "rewind",  TREWIND, NULL},
	{ "save",  TSAVE, NULL},
	{ "static",  TSTATIC, NULL},
	{ "stop",  TSTOP, NULL},
	{ "structure", TSTRUCT, NULL},
	{ "subroutine",  TSUBROUTINE, NULL},
	{ "then",  TTHEN, NULL},
	{ "to", TTO, NULL},
	{ "undefined", TUNDEFINED, NULL},
	{ "union", TUNION, NULL},
	{ "while", TWHILE, NULL },
	{ "write",  TWRITE, NULL},
	{ 0, 0, NULL}
};

initkeytab()
{
    int i ;
    struct keyword *p, *q ;
    for ( p = keywords ; p->token ; p ++ ) {
	i = p->name[0] - 'a' ;
	q = keytable[i] ;
	keytable[i] = p ;
	p->next = q ;
    }
}

/* chkcontext()
   The purpuse of this routine is to check for a number things that
   are context sensitive in the FORTRAN grammer.
   1.  If a line does not start with the keyword "DO" or "IF(", then an
       exposed equal sign (not in quotes or parens) means that this
       is an assignment statement and the first token will NOT contain
       a keyword.  If there is no exposed equal sign, then the first
       token is guaranteed to contain a keyword.
   2.  If a line does contain a DO, then exposed equal signs are expected,
       but we must also be sure that after the exposed equal sign comes
       a comma so as to distinguish DO 10 I = 1.10 from DO 10 I= 1,10 .
       If both conditions are met, then we consider the first token to 
       contain a keyword, namely DO.
   3.  If a line starts with an IF(, then it remains to be determined if
       it is an arithmatic or logical if.  This can be done later, with
       another full scan of the line, but we're scanning here anyway.  If
       the first character after parens are closed is a digit, then it's 
       arithmatic.  Otherwise, it's logical.  This is indicated by setting
       a variable iftype, which will be accessed later by gettoken() when
       it tries to determin which type of if it's looking at.
*/
int chkcontext(s,j)
char s[] ;
int j ;
{
    int anif = 0,
	ado = 0,
	inquote = 0,
	goteq = 0,
	paren = 0 ;
    int i ;

    if ((s[0] == 'd') && (s[1] == 'o')) 
        ado = 1 ;
    else if ((s[0] == 'i') && (s[1] == 'f') && (s[2] == '(')) {
	anif = 1 ;
    }

    iftype = 0 ;
    for ( i = 0 ; i <= j ; i++ ) {
	if ((s[i] == '\'') || (s[i] == '"')) {
	    if (inquote == s[i]) inquote = 0 ;
	    else inquote = s[i] ;
	}
	if (inquote == 0) {
	    if (s[i] == '(')
		paren++ ;
	    else if (s[i] == ')') 
		paren-- ;
	    else if ((!anif) && (s[i] == '=') && (paren == 0))
		if (ado) goteq = 1 ;
		else return(0) ;
	    else if ((ado) && (goteq) && (s[i] == ',') && (paren==0))
		return(1) ;
	    else if ((anif) && (paren == 0) && (i > 2)) {
		if ((s[i] >= '0') && (s[i] <= '9')) iftype = TARITHIF ;
		else iftype = TLOGIF ;
		return(1) ;
	    }
	}
    }
    if (ado) return(0) ;
    return(1) ;
}


/* getkeyword()
   if a keyword starts string s, return the number of characters that
   represent the longest matching keyword and pass back its token 
   value as tok or, if no match, return 0 and TNAME.
*/

int getkeyword( s, n, tok ) 
char *s ; 	/* input string */
int n ;		/* length of string */
int *tok ;	/* token value */
{
    int i,toklen,toklen2,prevlen ;
    struct keyword *p, *q ;

    if (firstkey) {firstkey = 0 ; initkeytab() ;}

    i = s[0] - 'a' ;
    p = keytable[i] ;
    prevlen = -1 ;		/* -1 indicates no match */
    toklen = strlen(s) ;
    while ( p ) {
	toklen2 = strlen(p->name) ;
	if ((toklen>=toklen2)&&(strncmp(s, p->name, toklen2)) == 0) {
	    if (toklen2 > prevlen) {
		prevlen = toklen2 ;
		q = p ;
	    }
	}
	p = p->next ;
    }
    if (prevlen == -1) {
	*tok = TNAME ;
        return( 0 ) ;
    } else if (q == NULL) {
	comp_err("impossible state in getkeyword (lexin.c)") ;
    } else {
	*tok = q->token ;
	return(prevlen) ;
    }
}
