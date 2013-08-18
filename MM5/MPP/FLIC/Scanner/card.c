/* %M% SCCS(%R%.%L% %D% %T%) */

/* 
    card.c

    Routines to accesss input stream to scanner as if it were
    a deck or decks of cards.  This entire module is accessed
    through the routine FGETC, which returns the next character,
    its position (column) on the card, and the card number.
    FGETC is called by the input() routines in ioque.c, which
    in turn are called by the lex generated scanner, yylex().

    In addition, each new statement is prepended with a newline
    and a one-character context code to tell the scanner how
    it should handle the following statement.

    Codes:

	k	-- the first string of this statement is
		a keyword other than an if or an implicit.
	a	-- the following statement is an aritmetic if
	l	-- the following statement is a logical if
	m	-- the following statement is an implicit statement
	i	-- the first string of this statement is a 
		symbolic identifier (such as in an assignment
		statement). 
	x	-- this statement or sequence of statements
		are comments or of unknown context.

    The routines perform certain manipulations on the input stream.  

    a.  All alphanumerics are converted to lower case by the crunch()
    routine except in comments, string literals, and hollerith strings 
    (note: this is not the case yet for hollerith strings -- they
    are erroneously converted to lower case and crunched.)

    b.  All white space is removed by the crunch() routine except in
    in labels, comments, string literals, and holleriths (again the holleriths
    are not being handled correctly yet.)

    c.  Original card numbers and positions for each input character
    is preserved in parallel arrays.  (see the cardque data structures)
    
    d.  Continuations are dissolved so that to the scanner, all statements
    look as though they had been on a single card.  ! delimited comments
    and comments which appear in the middle of a multi-card statement
    are always pushed to the end of the statement.  Position and cardnumber
    information for the characters which make up these comments are
    preserved.

    As an example, the FORTRAN fragment:

    |      do 10
    |C midstatement comment
    |     +I = ! another comment
    |     +1,10

    would be correctly returned by successive calls to FGETC as:

    char: Nk      do10I=1,10NxC midstatement commentNx! another comment
    pos :   12345678AB79789A  123456789ABCDEF0123456  BCDEF0123456789AB
    card:   1111111111334444  2222222222222222222222  33333333333333333

    Newline is shown as N above, positions are shown in unsigned hex.

    *** Handling Logical IF ***

    Logical if statements are somewhat more difficult because they involve
    a statement which contains a completely new statement, which in turn
    may also contain a completely new statement should it also be a 
    logical if.  For example:  

	IF (.true.) IF (.true.) IFA = 2,

    correctly interpreted, should assign the variable identifier IFA
    with the value 2.  The routines  in card.c must signal the calling
    scanner that a new statement has been found in the current logical
    if statment and the context of the new statement must be indicated.

    This is done by inserting a dummy \n and token code into the logical if
    statement after the parentheses.  To further aid the scanner, a dummy
    label "0CNTX " is also included.  The following is how the example
    above would appear to the caller of FGETC.

        Nl      if(.true.)Nl0CNTX if(.true.)Ni0CNTX ifa=2
        .................................................

    (Dots added to make spaces more easily visible.)

    *** Handling implicit ***

    An implicit statement has the property that every non-parenthesized
    character string is a keyword.  By marking the statement with the
    context code "m", we alert the scanner to this.
*/


#include <stdio.h>
#include "defines.h"

#define QUESIZE     NCONT * CARDLEN * 10        /* lots */
#define INITQ       cardque.queh = 0 ; cardque.quet = 0 ;
#define INITCOMQ    commentque.queh = 0 ; commentque.quet = 0 ;

#define NUMTRY	5    /* number of times to ask malloc for memory before giving up */

extern FILE *infile ;
extern char infilename[] ;
static struct cardq {
    int queh, quet ;
    char val[QUESIZE] ;
    int pos[QUESIZE] ;
    int cardno[QUESIZE] ;
} cardque, commentque ;
static int storedflag = 0 ;

static int cardnumber = 1 ;
static int context ;

static struct card {
    char val[ CARDLEN ] ;
    int  pos[ CARDLEN ] ;
    int context ;
    int cardnumber ;
    int ncol ;
} card, storedcard ;

#define isdigit(c)  ((c>='0')&&(c<='9'))
#define comment  (!isdigit(card.val[0]) && (card.val[0] != ' '))
#define continuation (card.val[5] != ' ' && card.val[5] != '0' )
#define blank   seeifblank(card.val)
#define reteof { cardque.queh = 0 ; cardque.quet = 2 ; \
		 cardque.cardno[0] = -1 ; \
		 cardque.pos[0] = -1 ;\
		 cardque.val[0] = '\n' ;\
		 cardque.val[1] = 0 ;\
		 return ;\
		}

/* following variables used by crunch() and fixcontext() */
static char inquote_crunch = 0 ;   /*   added 3-26-90 to allow for
					strings to go across continuations */
static char firstc ;
static int rep ;
/* end variables used by crunch and fixcontext */


FGETC(c,p,cd)
char *c ;
int *p, *cd ;
{
    if (cardque.queh == cardque.quet) {
	getnewstatement() ;
#ifdef DEBUG
	{ int i = cardque.queh ;
	  fprintf(stderr,"\nNEW CARDQUE\n") ;
	  for (;i < cardque.quet ; i++) {putc(cardque.val[i],stderr);} ;
	  putc('\n',stderr) ;
	}
#endif
    }
    *cd = cardque.cardno[cardque.queh] ;
    *p = cardque.pos[cardque.queh] ;
    *c = cardque.val[cardque.queh++] ;
#ifdef DEBUG
    printf("FGETC returns %c (%x)\n",*c,*c) ;
#endif
}

getnewstatement()
{
    INITQ ;
    INITCOMQ ;
    if (readcd() == EOF) reteof ;

    if (comment) {
	enque() ; return ;
    }

    if (blank) {
	enque() ; return ;
    }

    if (continuation) {
	lexerr("illegal continuation ignored") ;
	return ;
    }

    if (1) {
	setfirstc() ; /* 3-27-90 */
	crunch() ; enque() ; getnextcard() ; quecomments() ; fixcontext() ;
    }
}

getnextcard()
{
    if (readcd() == EOF	) return ;

    if (blank) {
	getnextcard() ; return ;
    }

    if (comment) { 
	storecomment() ; getnextcard() ; 
	return ;
    }

    if (continuation) {
	crunch() ; quecont() ; getnextcard() ;
	return ;
    }

    if (1) {
	storefornext() ;
	return ;
    }
}

/* routines used by card.c  */
#define inctail ( cardque.quet>=QUESIZE?comp_err("cardque overflow in card.c"):cardque.quet++ )
#define incommtail ( commentque.quet>=QUESIZE?comp_err("commentque overflow in card.c"):commentque.quet++ )
    
enque() {
    int i ; 
    cardque.pos[cardque.quet] = -1 ;
    cardque.cardno[cardque.quet] = card.cardnumber ;
    cardque.val[inctail] = '\n' ;
    cardque.pos[cardque.quet] = 0 ; 
    cardque.cardno[cardque.quet] = card.cardnumber ;
    cardque.val[inctail] = card.context ;
    for ( i = 0 ; i < card.ncol ; i++ ) {
	cardque.pos[cardque.quet] = card.pos[i] ;
	cardque.cardno[cardque.quet] = card.cardnumber ;
	cardque.val[inctail] = card.val[i] ;
    }
}

quecont() {
    int i ; 
    for ( i = LabelN ; i < card.ncol ; i++ ) {
	cardque.pos[cardque.quet] = card.pos[i] ;
	cardque.cardno[cardque.quet] = card.cardnumber ;
	cardque.val[inctail] = card.val[i] ;
    }
}

storecomment() {
    int i ; 
    commentque.pos[commentque.quet] = -1 ;
    commentque.cardno[commentque.quet] = card.cardnumber ;
    commentque.val[incommtail] = '\n' ;
    commentque.pos[commentque.quet] = 0 ; 
    commentque.cardno[commentque.quet] = card.cardnumber ;
    commentque.val[incommtail] = card.context ;
    for ( i = 0 ; i < card.ncol ; i++ ) {
	commentque.pos[commentque.quet] = card.pos[i] ;
	commentque.cardno[commentque.quet] = card.cardnumber ;
	commentque.val[incommtail] = card.val[i] ;
    }
}

quecomments() {
    int i ;
    for (i = commentque.queh ; i < commentque.quet ; i++ ) {
	cardque.val[cardque.quet] = commentque.val[i] ;
	cardque.pos[cardque.quet] = commentque.pos[i] ;
	cardque.cardno[inctail] = commentque.cardno[i] ;
    }
}

storefornext() 
{
    register int i ;
    storedflag = 1 ;
    storedcard.ncol = card.ncol ;
    storedcard.cardnumber = card.cardnumber ;
    storedcard.context = card.context ;
    for (i=0 ; i < CardLen ; i++ ) {
	storedcard.val[i] = card.val[i] ;
	storedcard.pos[i] = card.pos[i] ;
    }
}

#define iscomment(c) (!isdigit(c) && (c != ' '))
fixcontext()
{
    fixcontext1(cardque.queh) ;
}

fixcontext1(head)
int head ;	/* at top level call, head = cardque.head */
{
    int i = head ;
    int cardstart ;
    int anif = 0,
	ado = 0,
	adouble = 0,
	animplicit = 0,
	aparam = 0,
	quotelevel = 0 ,	/* added 4-13-90 */
	inquote = 0,
	goteq = 0,
	paren = 0 ;
    char context ;

/* added 3-26-90.  Allowing string literals to span continuation cards.
   To check for unclosed quotes, then, we check to make sure that 
   crunch() hasn't been left hanging waiting for a quote.  If everything
   is correct, crunch()'s quote minding variable, inquote_crunch, should
   be equal to nil at this point, since when fixcontext is called, all
   continuations should be accounted for and in the cardque.  If quote
   is not nil, then a string has been left dangling without a closing
   quote.   Report the error and continue. */

    if (inquote_crunch != '\0') {
	char temp[80] ;
	sprintf(temp,"unclosed quote near line %d",cardque.cardno[2]) ;
	inquote_crunch = '\0' ;
	lexerr(temp) ;
    }
/* end of addition, 3-26-90 */

/* added 3-27-90.  Hollerith constants must not be crunched, but may
   span a continuation card.  The number of remaining characters to
   be hollerith'd between cards is stored in the global variable rep,
   which is set by crunch1().  However, if we get to this point (no
   more continuations and rep is non-zero, then something is wrong...
   the hollerith specified more characters than could be found.
   Report the error and continue. */

    if ( rep != 0 ) {
	char temp[80] ;
	sprintf(temp,"hollerith short by %d chars near line %d",
			rep, cardque.cardno[2]) ;
	lexerr(temp) ;
    }
/* end of addition, 3-27-90 */
    while ( i < cardque.quet ) {

        while ( cardque.val[i] == '\n' ) {
            if (( i+2 ) < cardque.quet) {
	        i += 2  ;
	        /* skip past comments */
	        if (iscomment(cardque.val[i])) {
		     while (( i < cardque.quet) && (cardque.val[i] != '\n'))
			 i++ ;
	        }
	    } else {
		context = 'k' ; 	/* a keyword by default */
		goto out ;
	    }
	}

	if ( i>=cardque.quet ) {
	    if (ado && !adouble ) context = 'i';
	    else context = 'k' ;
	    goto out ;
	}

	i += LabelN ;
	cardstart = i ;

	if ((cardque.val[i] == 'd') && (cardque.val[i+1] == 'o') &&
	    ! ( 
		cardque.val[i+2] == 'w' &&
		cardque.val[i+3] == 'h' &&
		cardque.val[i+4] == 'i' &&
		cardque.val[i+5] == 'l' &&
		cardque.val[i+6] == 'e' &&
		cardque.val[i+7] == '('
	      )
	    )
	{
            ado = 1 ;
            if ((cardque.val[i+ 2] == 'u' &&
                 cardque.val[i+ 3] == 'b' &&
                 cardque.val[i+ 4] == 'l' &&
                 cardque.val[i+ 5] == 'e' &&
                 cardque.val[i+ 6] == 'p' &&
                 cardque.val[i+ 7] == 'r' &&
                 cardque.val[i+ 8] == 'e' &&
                 cardque.val[i+ 9] == 'c' &&
                 cardque.val[i+10] == 'i' &&
                 cardque.val[i+11] == 's' &&
                 cardque.val[i+12] == 'i' &&
                 cardque.val[i+13] == 'o' &&
                 cardque.val[i+14] == 'n' 
	       ) ||
               ( cardque.val[i+ 2] == 'u' &&
                 cardque.val[i+ 3] == 'b' &&
                 cardque.val[i+ 4] == 'l' &&
                 cardque.val[i+ 5] == 'e' &&
                 cardque.val[i+ 6] == 'c' &&
                 cardque.val[i+ 7] == 'o' &&
                 cardque.val[i+ 8] == 'm' &&
                 cardque.val[i+ 9] == 'p' &&
                 cardque.val[i+10] == 'l' &&
                 cardque.val[i+11] == 'e' &&
                 cardque.val[i+12] == 'x'
	       ))
	        adouble = 1 ;
	}
        else if ((cardque.val[i] == 'i') && (cardque.val[i+1] == 'f')
		&& (cardque.val[i+2] == '(')) {
	    anif = 1 ;
        } else if ((cardque.val[i  ] == 'i') &&
		   (cardque.val[i+1] == 'm') &&
		   (cardque.val[i+2] == 'p') &&
		   (cardque.val[i+3] == 'l') &&
		   (cardque.val[i+4] == 'i') &&
		   (cardque.val[i+5] == 'c') &&
		   (cardque.val[i+6] == 'i') &&
		   (cardque.val[i+7] == 't')) {
	    animplicit = 1 ;
	    context = 'm' ;
	    goto out ;
	} else if ((cardque.val[i  ] == 'p') &&
		   (cardque.val[i+1] == 'a') &&
		   (cardque.val[i+2] == 'r') &&
		   (cardque.val[i+3] == 'a') &&
		   (cardque.val[i+4] == 'm') &&
		   (cardque.val[i+5] == 'e') &&
		   (cardque.val[i+6] == 't') &&
		   (cardque.val[i+7] == 'e') &&
		   (cardque.val[i+8] == 'r')) {
	    /* must prevent VMS style param statements (no parens)
	       from being mistaken for assignment statements. */
	    aparam = 1 ;
	    context = 'k' ;
	    goto out ;
	}

#ifdef COMMMENTOUT
fprintf(stderr,"ZAP ZAP\n") ;
#endif
        while ((i < cardque.quet) && (cardque.val[i] != '\n')) {
#ifdef COMMMENTOUT
fprintf(stderr,"ZAP %c ado = %d   adouble = %d\n", cardque.val[i], ado, adouble) ;
#endif
	    if ((cardque.val[i] == '\'') || (cardque.val[i] == '"')) {
	       /*  replaced this code 4-13-90.... it didn't handle nested quotes
	        *    if (inquote == cardque.val[i]) inquote = 0 ;
	        *    else inquote = cardque.val[i] ;
	        *}
	        */
		if (inquote == cardque.val[i]) {
		    if (quotelevel <= 1) {
			quotelevel = 0 ;
			inquote = 0 ;
		    } else {
			quotelevel -- ;
			switch ( inquote ) {
			    case '\'' : inquote = '"' ; break ;
			    case '"'  : inquote = '\'' ; break ;
			    /* this should be impossible */
			    default : lexerr("illegal quote character") ; exit(1) ; break ;
			}
		    }
		} else
		    quotelevel++ ;
		    inquote = cardque.val[i] ;
	    }
	    if (inquote == 0) {
	        if (cardque.val[i] == '(')
		    paren++ ;
	        else if (cardque.val[i] == ')') 
		    paren-- ;
	        else if ((!anif) && (cardque.val[i] == '=') && (paren == 0))
		    if (ado) {
    			goteq = 1 ;
			adouble = 0 ;
		    }  else {
#ifdef COMMENTOUT
			fprintf(stderr,"it came from the first one\n%s\n",
			    cardque.val) ;
#endif
			context = 'i' ;  /* starts without keyword */
			goto out ;
		    }
	        else if ((ado) && (goteq) && (cardque.val[i] == ',')
			&& (paren==0)) {
		    context = 'k' ;  /* starts with keyword */
		    goto out ;
		}
	        else if ((anif) && (paren == 0) && ((i-cardstart) > 2)) {
		    if ((cardque.val[i] >= '0')
		       && (cardque.val[i] <= '9')) {
			context = 'a' ; /*arith if */
			goto out ;
		    }
		    else {
			context = 'l'; /*logical if */
			/* open up an 8 character hole here in cardque so that 
			   we can insert a dummy label signaling context to the
			   lexer.  It will key on teh label "\nx0CNTX " */
			if (cardque.quet+8 >= QUESIZE)
			    comp_err(
			      "fixcontext(): cardque overflow on logical if") ;
			else {
			    int p, q ;
			    p = cardque.quet ;
			    cardque.quet += 8 ;
			    q = cardque.quet ;
			    /* open up the space by moving everything 8 
			       chars up */
			    while ( p > i ) {
				-- p ; -- q ;
				cardque.val[q] = cardque.val[p] ;
				cardque.pos[q] = cardque.pos[p] ;
				cardque.cardno[q] = cardque.cardno[p] ;
			    }
			    /* create the dummy label */
			    cardque.val[i  ] = '\n' ;
			    cardque.val[i+1] = 'x' ;
			    cardque.val[i+2] = '0' ;
			    cardque.val[i+3] = 'C' ;
			    cardque.val[i+4] = 'N' ;
			    cardque.val[i+5] = 'T' ;
			    cardque.val[i+6] = 'X' ;
			    cardque.val[i+7] = ' ' ;
			    
			    /* now call fixcontext1() recursively, telling it to
			       start at the label we just made */
			    fixcontext1(i) ;
			    goto out ; 
			    /*
			    break ; /* don't need to recurse */
			}
		    }
	        }
	    }
	    i++ ;
        }
    }
    if (ado) {
	if ( adouble ) 
  	    context = 'k' ;
	else
	    context = 'i' ;
#ifdef COMMENTOUT
	fprintf(stderr,"ado is %d, cardque.quet is %d  ", ado, cardque.quet) ;
	fprintf(stderr,"it came from the second one\n%s\n", cardque.val) ;
#endif
    }
    else context = 'k' ;
out:
#ifdef COMMMENTOUT
fprintf(stderr,"ZAP ZAP ZAP context is %c\n", context) ;
#endif
    if (cardque.val[head] == '\n') {
	cardque.val[head+1] = context ;
    } else {
	comp_err("impossible state") ;
    }
}

#define MASK(C) if (C!=EOF) C &= 0x7f

/* 
    readcd()
*/
readcd()
{
    register int i, ii, seenbang = 0 ;
    register int c, c2 ;

    if (storedflag) {
	storedflag = 0 ;
	card.ncol = storedcard.ncol ;
	card.cardnumber = storedcard.cardnumber ;
	card.context = storedcard.context ;
	for (i=0 ; i < CardLen ; i++ ) {
	    card.val[i] = storedcard.val[i] ;
	    card.pos[i] = storedcard.pos[i] ;
	}
#ifdef DEBUG
	{ int i ; 
	  fprintf(stderr,"readcd() returns cardlen: %d, card:\n>", card.ncol ) ;
	  for (i=0;i<CardLen;i++) fputc(card.val[i], stderr) ;
	  fprintf(stderr,"<\n") ;
	}
#endif
	return(!EOF) ;
    }
    for ( i = 0 ; i < CardLen ; i++ ) {
	 card.val[i] = ' ' ; /* initially cards are all spaces */
    }
    i = 0 ;
    ii = 1 ;

    c = getc(infile) ; 
    MASK(c) ;
    while ( (c != EOF) && (c != '\n') && (i < CardLen) ) {
	/* tab in cols 1-6 skips to col 7 */
	seenbang = c == '!' ;
	card.pos[i] = ii++ ; /* mark positions */
	if ((c=='\t')&&(i<=LabelN)&&(!seenbang)) {
	    if (i == 0) {
		/* the first character after a leading tab can signal
		   a continuation if it is a numeric.   Put the character
		   in the normal place for continuations (col 6) and 
		   then treat as usual.  3-27-90 */
		c2 = getc(infile) ;
		MASK(c) ;
		if ( c2 >= '0' && c2 <= '9' ) {
		    card.val[5] = c2 ;
		} else {
		    ungetc( c2, infile ) ;
		}
	    }
	    i = LabelN ;
	} else
	    card.val[i] = c ; /* don't bother with saving tabs */
	i++ ;
	c = getc(infile) ;
	MASK(c) ;
    }
    card.ncol = i ;
    card.cardnumber = cardnumber++ ;
    card.context = 'x' ; /* unknown */
	/* if stoped by length of card, discard remainder of this line */
    while ( (c != EOF) && (c != '\n') ) {c = getc(infile) ; MASK(c) ;}
#ifdef DEBUG
	{ int i ; 
	  fprintf(stderr,"readcd() returns cardlen: %d, card:\n>", card.ncol ) ;
	  for (i=0;i<CardLen;i++) fputc(card.val[i], stderr) ;
	  fprintf(stderr,"<\n") ;
	}
#endif
    if ((c == EOF) && ( i == 0 )) return (EOF) ;
    return( ! EOF ) ;
}

/*  
    crunch1()
	get rid of spaces in the statement part of the card

    revised 3-26-90 to use the variable inquote_crunch instead
    of a local variable for keeping track of string literals.
    This is because literals which spanned continuation cards
    could confuse crunch unless it remembered from one card
    to the next whether or not it was in a quote.  Fixcontext1()
    above has been altered to check for unclosed quote marks.
*/
crunch()
{
    crunch1(&card) ;
}

crunch1( card ) 
struct card *card ;
{
    char tolower() ;
    register int i ;
    register char *p , *q, *end ;
    register int *x, *y ;
    int j, savedncol ;

    p = q = card->val + LabelN ; /* pointer to crunched card */
    x = y = card->pos + LabelN ; /* ptrs to indeces */
    end = card->val + card->ncol ;
    savedncol = card->ncol ;

    /* if ! in cols 2 - 6 of label, comment to eol */
    for (i = 1 ; i < LabelN ; i++ )
	if ( card->val[i] == '!' ) {
	    return ; 
	}

    /* if rep != 0, it means that this is a continuation and the
       previous card contained a continued hollerith constant.
       take care of it without crunching.  added 3-27-90. */

    card->ncol = LabelN ;
    for ( ; rep>0 ; rep--) {
        /* don't go off of the end */
        if (q < end) {
            *p++ = *q++ ;
            *x++ = *y++ ;
            card->ncol++ ;
        } else 
            break ;
    }

    /* for rest of line, while not in a string constant, eliminate
       spaces, change to lower case, and make comment to eol after ! */
    if (q < end) {
        while ( q < end ) {
	    if (inquote_crunch == 0) {
	        while ((q < end)&&((*q==' ')||(*q=='\t'))) {q++;y++;} ; 
	        if ((*q == '\'') || (*q == '"')) inquote_crunch =  *q ;
		if (*q == '!') {
		    /*store the comment as if it were on 
  		      a line by itself.  Actual position and
		      card number information is still available
		      in the pos and cardnumber fields */
		    commentque.pos[commentque.quet] = -1 ;
		    commentque.cardno[commentque.quet] = card->cardnumber ;
		    commentque.val[commentque.quet++] = '\n' ;
		    commentque.pos[commentque.quet] = 0 ;
		    commentque.cardno[commentque.quet] = card->cardnumber ;
		    commentque.val[commentque.quet++] = 'x' ;
		    while (q < end) {
   			commentque.pos[commentque.quet] = *y++ ;
        		commentque.cardno[commentque.quet] = card->cardnumber ;
        		commentque.val[commentque.quet++] = *q ;
			*q++ = ' ' ;
		    }
		    return ;
		}

		/* check for holleriths before smooshing, 3-27-90 */
		if ( *q == 'h' || *q == 'H' ) {
		    char *r ;
		    int place = 1, i ;
		    /* rep is defined globally above */
		    r = p-1 ;
		    if ( *r < '0' || *r > '9' )  {
			/* forget it, not a hollerith */
		    } else {
			rep = 0 ;
			while (( r >= (card->val+LabelN)) &&
				*r >= '0' && *r <= '9' ) {
			    rep = rep + (*r-'0')* place ;
			    place *= 10 ;
			    r-- ;
			}
                        /* *r must be a punctuation character */
                        if ( (r < card->val+LabelN )
			    || ( *r=='*' && (firstc=='d' || firstc=='D'))
                            || (*r == '/')
                            || (*r == '(')
                            || (*r == ',')
                            || (*r == '=')
                            || (*r == '.') 
                           ) {   /* got one */
                            *p++ = tolower(*q++) ;  /* move the h */
                            *x++ = *y++ ;
                            card->ncol++ ;
                            /* now move the constant */
                            for ( ; rep>0 ; rep--) {
                                /* don't go off of the end */
                                if (q < end) {
                                    *p++ = *q++ ;
                                    *x++ = *y++ ;
                                    card->ncol++ ;
                                } else 
                                    break ;
                            }
			    continue ;
                        } else {
			    rep = 0 ;
			}
		    }
		} /* end 3-27-90 */

	        if (q < end) {
		    *p++ = tolower(*q++) ; 
		    *x++ = *y++ ;
		    card->ncol++ ;
		}
	    } else {
		if (*q == inquote_crunch) {
		    if ( q + 1 < end ) {
		        if (*(q+1) != inquote_crunch)
			    inquote_crunch = 0 ;
		        else {
			    *p++ = *q++ ;
			    *x++ = *y++ ;
			    card->ncol++ ;
			}
		    } else {
		       /* 3-26-90.  fixcontext must see a zero in 
		          inquote_crunch if it is really closed. */
			inquote_crunch = 0 ;
		    }
		} 
		    
		*p++ = *q++ ;
		*x++ = *y++ ;
		card->ncol++ ;
	    }
        }
	/* blank out rest of card */
	while (p<end) *p++ = ' ' ;
    }
}

/* added 3-27-90.  Sets global variable firstc with the
   first non-white character of the statement.  This is
   used by crunch1() to determine if it is looking at 
   a data statement when processing a suspected hollerith string.
*/
setfirstc()
{
    setfirstc1(&card) ;
}

setfirstc1(card)
struct card *card ;
{
    char * q, * end ; 

    q = card->val + LabelN ;
    end = card->val + card->ncol ;

    while ( q < end ) {
	if ( *q == ' ' || *q == '\t' ) q++ ;
	else break ;
    }
    if ( q < end ) {
	firstc = *q ;
    }
}

char tolower(c)
char c ;
{
    if (( c >= 'A' ) && ( c <= 'Z' )) return( c - 'A' + 'a' ) ;
    return(c) ;
}

struct deck {
    FILE *infile ;
    char infilename[256] ;
    struct card storedcard ;
    struct cardq cardque ;
    struct cardq commentque ;
    int storedflag ;
    int cardnumber ;
    int pbsp ;  /* pushback stack pointer -- in ioque.c */
    struct deck *next ;
} ;

static struct deck *deckstack = NULL ;

pushincl()
{
    struct deck *malloc(), *p ;

#ifdef DEBUG
    fprintf(stderr,"size of a deck is %d (%x)\n",sizeof(*p),sizeof(*p)) ;
    fprintf(stderr,"size of a single que is %d (%x)\n",sizeof(struct cardq),sizeof(struct cardq)) ;
#endif
    if ((p = malloc(sizeof(*p))) == NULL)  {
	int i ;
	/* try a couple more times */
	for (i=0;i<NUMTRY;i++) {
	    fprintf(stderr,"WARNING: malloc() failed in pushincl().  Trying again\n") ;
	    sleep(1) ;
	    if ((p = malloc(sizeof(*p))) != NULL) {
		 fprintf(stderr,"\tOK:  malloc() succeeded after %d tries\n",i+1) ;
		 goto okok ;
	    }
	}
	comp_err("pushincl(): malloc() failed: include push stack error") ;
    }
okok:
    p->infile = infile ;
    strcpy(p->infilename, infilename) ;
    p->storedcard = storedcard ;
    p->storedflag = storedflag ;
    p->cardque = cardque ;
    p->commentque = commentque ;
    p->cardnumber = cardnumber ;
    p->next = deckstack ;
    deckstack = p ;
    INITQ ;
    INITCOMQ ;
    initpushback() ;  /* this resets the pushback stacks (ioque.c) */
    storedflag = 0 ;
    cardnumber = 1 ;
}

extern int pos ;

popincl()
{
    struct deck *p ;
    INITQ ;
    INITCOMQ ;
    initpushback() ;

    p = deckstack ;
    if (p == NULL) {
	comp_err("include pop stack error: empty stack") ;
    }
    if (infile != NULL) fclose(infile) ;
    infile = p->infile ;
    strcpy(infilename,p->infilename) ;
    storedcard = p->storedcard ;
    storedflag = p->storedflag ;
    cardque = p->cardque ;
    commentque = p->commentque ;
    cardnumber = p->cardnumber ;
    deckstack = p->next ;
    free(p) ;
    /* this next is a kludge.  When we finished reading the include
       statement that caused the original push, the lex based scanner
       pushed back the newline that started this next line.  Since 
       we don't save the pushback stacks (would require too much memory)
       when we jump to an include or back again, this first character
       gets lost -- but it is still at the front of the cardque.  
       Just push back head pointer and pretend we never looked at it.
       To be on the safe side, though, we'll check and make sure
       that it really points to a newline.  */
    if (cardque.val[cardque.queh-1]=='\n')
    {
        cardque.queh-- ;
        pos-- ;
    }
    else
	comp_err("popincl(), impossible state") ;
}

/* return status of include stack */
emptyincludestack()
{
    return( ( deckstack == NULL ) ) ;
}




seeifblank(crd)
char crd[] ;
{
    int i ;
    for (i=0; i<CardLen ; i++) {
	if (crd[i] != ' ') return(0) ;
    }
    return(1) ;
}
