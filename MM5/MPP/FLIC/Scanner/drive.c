/* %M% SCCS(%R%.%L% %D% %T%) */

#include <stdio.h>
#include <stdlib.h>
#include "tokens.h"
#include "defines.h"
#include "h.h"

int CardLen ;		/* variable equiv to constant CARDLEN */
int LabelN ;		/* variable equiv to constant LABELN */
int StatN ;		/* variable equiv to constant STATN */
int RestN ;		/* variable equiv to constant RESTN */

char infilename[256] ;
FILE * infile ;
char * outfilename ;
FILE * outfile ;
char * errfilename ;
FILE * errfile ;
char * thiscommand ;


char yytext[YYLMAX];
int yyleng ;

char cwd[256] ;

int yyposition[1000] ;
int yycardno[1000] ;
int pos = 0 ;

int in_proc = 0 ;	/* state variable 
				0, out of module
				1, first statement of module
				>1, successive statements */
int idnumber = 0 ;	/* count of TNAME tokens in line (label is 0) */
int watch_for_function = 0 ; /* state variable to show if 
				we are in a possible function declaration.
				This is necessary to allow the scanner
				to properly discrimiante function statements
				from type declarations. */
int watch_for_star = 0 ;    /*  see comment below in lexit() 90-10-31 */
int watch_for_integer = 0 ; /*  state variable to show whether we (5-7-90)
                                should allow a real or double constant
                                to be reported by the scanner yylex */

int watch_for_starparenstar = 0 ; /*  92-03-18 */

static char context ;
static int thistok ;
       int lex_lasttok = -1 ;
static char lex_firstc ;
extern int firsttok ;   /* declared in lexin.c */
extern int inparen ;    /* declared in lexin.c */

static int begin ;
int tok = 0 ;
#define end (yyposition[pos-1]>=yyposition[begin]?pos-1:begin)

#if 0
static char code[] = "n32.scanner token output" ;  /* 4/17/92 */
#endif

		/* new main routine with command line processing added 4-10-90 */
main( argc, argv, env )
int argc ;
char *argv[], *env[] ;
{
    /* read through and process options, setting switches as appropriate.
       as written, the last non-hyphenated token on the command line is taken to be
       the input file name 
    */

    int extend_sw = 0 ;
    infilename[0] = '\0' ;
    thiscommand = *argv ;
    argv++ ;
    while (*argv) {
	if (*argv[0] == '-') { 	/* an option */
	    if (!strcmp(*argv,"-extend")) {
		extend_sw = 1 ;
	    } else
	    if (!strcmp(*argv,"-o")) {
		outfilename = *++argv ;
	    } else
            if (!strcmp(*argv,"-h")) {
                printhelp() ;
                exit(0) ;
            }
	} else {
	    strcpy( infilename, *argv ) ;
	}
	argv++ ;
    }

    if (extend_sw) {
	CardLen = CARDLEN ;
	LabelN = LABELN ;
	StatN = STATN ;
	RestN = RESTN ;
    } else {	/* default -- standard FORTRAN line specs */
        CardLen = 72 ;
	LabelN = LABELN ;
	StatN = 66 ;
        RestN = CardLen - LabelN - StatN ;
    }
	

    /* open the input filename.   
       If a name was given but unopenable, abort.
       If no input filename was ever given then assume standard input.
    */
    if (infilename[0] != '\0') {
        if (( infile = fopen( infilename, "r" ) ) == NULL ) {
	    fprintf(stderr,"cannot open input file %s\n",infilename) ;
	    exit(1) ;
	}
    } else {
	infile = stdin ;
    }

    errfile = stderr ;

    /* open the output filename.    */

    if (outfilename != NULL) {
        if (( outfile = fopen( outfilename, "w" ) ) == NULL ) {
	    fprintf(stderr,"cannot open output file %s\n",outfilename) ;
	    exit(1) ;
	}
    } else {
	outfile = stdout ;
    }
#if 0
    fprintf(outfile,"%s\n",code) ;
#endif

    /* program body */

    lexit() ;
    exit(0) ;

}

yyless(n)
int n ;
{
  char * p, *q ;

  p = &(yytext[n]) ;
  for ( q = p ; *q ; q++ ) ;   /* q points to end */
  q-- ;
  for ( ; q >= p ; q-- )
  {
    unput(*q) ;
    if ( yyleng > 0 ) yyleng-- ;
  }
  yytext[yyleng] = '\0' ;
}


printhelp()
{
    printf("\nUsage: %s [options] [file]\n\n",thiscommand) ;
    printf("options:\n") ;
    printf("   -h           print this summary\n") ;
    printf("   -o file      specify an output file (def. stdout)\n") ;
    printf("   -extend      allow extended source lines up to %d cols\n", CARDLEN) ;
    printf("\n") ;
}

lexit()
{
    FILE *openinclude() ;
    extern char yytext[] ;
    extern int yyleng ;
    char temp[100] ;
    int cont = 0 ;
    int i ;
    int again = 0 ;

    lex_lasttok = 0 ;
    do {
        while (tok = yylex()) 
	{
	    /* monitor state of program */
	    if (tok == TLABEL) 
	    {
		in_proc++ ;
		idnumber = 0 ;  /* reset id counter */
                watch_for_integer  = 0 ;  /* new line, so reset */
	    } 
	    else if (tok == TEND) 
	    {
		in_proc = 0 ;
	    } 

/* added 92-03-18
   in the first line of a program, a declaration such as 
       CHARACTER*(*) FUNCTION
   is possible. Handle this appropriately 
*/
	    else if ( watch_for_starparenstar )
	    {
		if ( lex_lasttok == TCHARACTER && tok == TSTAR )
		{
		    watch_for_function = 1 ;
		    watch_for_star = 0 ;
		}
		else
		if ( lex_lasttok == TSTAR && tok == TLPAR )
		{
		    watch_for_function = 1 ;
		}
		else
		if ( lex_lasttok == TLPAR && tok == TSTAR )
		{
		    watch_for_function = 1 ;
		    watch_for_star = 0 ;
		}
		else
		if ( lex_lasttok == TSTAR && tok == TRPAR )
		{
		    watch_for_function = 1 ;
		    watch_for_starparenstar = 0 ;
		}
		else
		{
		    if (tok == TSTAR) 
	            {
		        watch_for_star = 0 ;
	            } 
	            else if (tok == TICON) 
	            {
		        watch_for_integer = 0 ;
	            } 
		    else
		    {
		        watch_for_function = 0 ;
		    }
		    watch_for_starparenstar = 0 ;
		}
	    }

/* end of add 92-03-18 */
	    else if (tok == TNAME) 
	    {
		idnumber++ ;
/* added 90-10-31
   In first line of program, such things as LOGICAL * 4 FUNCTION X ( ... )
   are possible.  Keep watch_for_function alive as long as this is 
   possible */
	    } 
	    else if (tok == TSTAR) 
	    {
		watch_for_star = 0 ;
	    } 
	    else if (tok == TICON) 
	    {
		watch_for_integer = 0 ;
	    } 

	    else 
	    {
		watch_for_star = 0 ;
		watch_for_function = 0 ;
		watch_for_integer = 0 ;
		watch_for_starparenstar = 0 ;
		if (tok == TINTEGER ||
                        tok == TREAL ||
                        tok == TDOUBLE ||
                        tok == TCOMPLEX ||
                        tok == TLOGICAL ||
                        tok == TCHARACTER ||
                        tok == TBYTE) 
		{
                    if ( in_proc == 1 ) 
		    {
			 if ( tok == TCHARACTER ) /* 92-03-18 */
			     watch_for_starparenstar = 1 ;
                         watch_for_function = 1 ;
			 watch_for_star = 1 ;
		    }
                    watch_for_integer = 1 ;
	        }
            }

	    lex_lasttok = tok ;

/* end of adds and changes 90-10-31 */
		
	    if (tok == TINCLUDE) {
                outputline() ;
                if (!(tok = yylex())) break ;
                if (tok == TSTRING) {
                    outputline() ;
                    yytext[yyleng-1] = '\0' ; /* get rid of qoutes */
                                    /* recurse to get include */
                    pushincl() ;
                    if ((infile = openinclude( yytext+1 )) == NULL ) {
                        sprintf(temp,"can't open %s", yytext+1) ;
                        lexerr(temp) ;
                        popincl() ;
                    } else {
                        strcpy(infilename,yytext+1) ;
                    }
                }
                pos = 0 ;
                continue ;
            } else if (tok == TFORMAT) {
		outputline() ;
		pos = 0 ;
		scanformat() ;
	    } else {
                outputline() ;
                pos = 0 ;
	    }
        }
        if (!emptyincludestack()) {
            popincl() ;
            again = 1 ;
        } else {
            again = 0 ;
        }
    } while (again) ;
}

static int tokensequence = 1 ;

outputline()
{
    int i ;
    extern int yyleng ;
    extern char yytext[] ;

    fprintf(outfile,"%d:",tokensequence++) ;
    fprintf(outfile,"%s:",infilename) ;
    fprintf(outfile,"%d:",tok) ;
    for (begin=0;yyposition[begin]<1&&begin<pos-1;begin++) ;
    fprintf(outfile,"%d:%d:",yycardno[begin],yyposition[begin]) ;
    fprintf(outfile,"%d:%d:",yycardno[end],yyposition[end]) ;
    printtok(outfile,tok) ;
    putc(':',outfile) ;
    if (tok != TBLANK)
        for (i = begin ; i < yyleng ; i++) {
            putc(yytext[i],outfile) ;
        }
    putc('\n',outfile) ;
}

/* openinclude()
   make sure when calling this routine that infilename is still
   set to the name of the file which contains the include statement. */
FILE *
openinclude( name )
char * name ;
{
    FILE * fp ;
    char *p, *q ;
    char temp[256], temp2[256] ;

    /* find directory name of current file (if any) */
    p = temp ;
    for (q = infilename ; *q && (p < temp + 255) ; )
        *p++ = *q++ ;
    *p = '\0' ;
    if (p == temp) return(stdin) ; /* null str is standard in */
                                   /* find rightmost slash */
    for (; p != temp && *p != '/' ; p-- ) ;
    if (p == temp) {
	return( fopen( name, "r") ) ;
    } else {
            /* the string between infilename and p is a directory name */
        *p = '\0' ;
	sprintf(temp2,"%s/%s",temp,name) ;
	fp = fopen( temp2, "r" ) ;
	if (fp != NULL) {
	    strcpy(name,temp2) ;   /* send back the complete name */
	    return (fp) ;
	}
    }

    /* if we get here, then the file was not in the directory of the 
       most recently included file */
    return(NULL) ;  /* give up */
}

/*#include "lex.c"*/
# include "stdio.h"
# define U(x) x
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define output(c) putc(c,stdout)
# define yymore() (yymorfg=1)
# define ECHO fprintf(stdout, "%s",yytext)
# define REJECT { nstr = yyreject(); goto yyfussy;}
int yyleng; extern char yytext[];
int yymorfg;
extern char *yysptr, yysbuf[];
int yytchar;
/* FILE *yyin = {stdin}, *yyout = {stdout}; */
extern int yylineno;
struct yysvf { 
	struct yywork *yystoff;
	struct yysvf *yyother;
	int *yystops;};
struct yysvf *yyestate;
extern struct yysvf yysvec[], *yybgin;
# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:
{
	    unput('\n') ;
	    return(TBLANK) ;
	    }
break;
case 2:
{}
break;
case 3:
{ 
	    int i ;
	    if (yytext[yyleng] == '\n') unput('\n') ;
	    return(TCOMMENT) ;
	    }
break;
case 4:
{ 
	    int i ;
	    if (yytext[yyleng] == '\n') unput('\n') ;
	    return(TCOMMENT) ;
	    }
break;
case 5:
{ 
	    int i, seenbang = 0  ;
	    for (i = 2; i < LABELN+2 ; i++) {
		if (yytext[i] == '!') {seenbang = i ; break ;}
	    }
	    if (seenbang > 0) 
	        for (i = LABELN+1 ; i >= seenbang ; i--) {
		    unput(yytext[i]) ;
	   	    yyleng-- ;
	        }
	    /* get numeric value of label or zero */
	    if (yytext[1] == 'a' ||
		yytext[1] == 'l' || 
		yytext[1] == 'm' || 
		yytext[1] == 'k')
	        context = yytext[1] ;
	    { /* sample next character and set it to lex_firstc */
	        lex_firstc = input() ;
		unput(lex_firstc) ;
	    }
	    return(TLABEL) ;
	  }
break;
case 6:
{
            if (yytext[1] == 'x' ||
                yytext[1] == 'a' ||
                yytext[1] == 'i' ||
                yytext[1] == 'k' ||
                yytext[1] == 'l' ||
                yytext[1] == 'm')
                context = yytext[1] ;
	    }
break;
case 7:
{ 
	    int i, seenbang = 0  ;
	    for (i = 2; i < LABELN+2 ; i++) {
		if (yytext[i] == '!') {seenbang = i ; break ;}
	    }
	    if (seenbang > 0) {
	        for (i = LABELN+1 ; i >= seenbang ; i--) {
		    unput(yytext[i]) ;
	   	    yyleng-- ;
	        }
	        if (yytext[1] == 'a' || yytext[1] == 'l' || yytext[1] == 'k' || 
		    yytext[1] == 'm' )
	            context = yytext[1] ;
	        return(TLABEL) ;
	    }
	    if (yytext[1] == 'a' || yytext[1] == 'l' || yytext[1] == 'k' || 
		yytext[1] == 'm' )
		comp_err("bad continuation card passed to yylex") ;
	    return(TCONTCARD) ;
	  }
break;
case 8:
{		/* maybe hollerith (don't forget about data) */
	    char temp[40], temp2[80] ;
	    char c ;
	    int i, n ;

	    yytext[yyleng] = 0 ;
	    if ( (lex_lasttok == TSTAR && lex_firstc == 'd') 
                 || lex_lasttok == TLPAR
                 || lex_lasttok == TDOT
                 || lex_lasttok == TEQ
                 || lex_lasttok == TNE
                 || lex_lasttok == TLT
                 || lex_lasttok == TGT
                 || lex_lasttok == TGE
                 || lex_lasttok == TLE
                 || lex_lasttok == TEQV
                 || lex_lasttok == TAND
                 || lex_lasttok == TOR
                 || lex_lasttok == TNOT
                 || lex_lasttok == TXOR
                 || lex_lasttok == TEQUALS
                 || lex_lasttok == TCOMMA
                 || lex_lasttok == TSLASH ) {
	        n = atoi( yytext ) ;
	        for ( i = 0 ; i < n ; i++ ) {
		    c = input() ;
		    if ( c != '\n' ) {
		        yytext[yyleng++] = c ;
		    } else {
		        unput('\n') ;
		        strncpy( temp, yytext, yyleng>=40?40:yyleng) ;
		        temp[yyleng>=40?40-1:yyleng]=0 ;
		        sprintf(temp2,"bad hollerith string: %s", temp) ;
		        lexerr(temp2) ;
		        return(0) ;
		    }
	        }
	        return( THOLLERITH ) ;
	    } else {
		unput('h') ;
		yyleng-- ;
		return(TICON) ;
	    }
	}
break;
case 9:
case 10:
{ 
        /* integers are 1 or more digits (D).  The first rule
           prevents a string such as 35.EQ.I from registering as
           a real 35.0 
	*/
	  return(TICON) ;
	  }
break;
case 11:
case 12:
 {
		/* hex constant */
           if (watch_for_integer) {
               char * p ; int i = 0 ;
               for (p = yytext ; *p >= '0' && *p <= '9' ; p++) i++ ;
               yyless(i) ;
               yyleng = i ;
               return(TICON) ;
           }
	  return(THEXCON) ;
          }
break;
case 13:
          case 14:
case 15:
 {
		/* octal constant */
           if (watch_for_integer) {
               char * p ; int i = 0 ;
               for (p = yytext ; *p >= '0' && *p <= '9' ; p++) i++ ;
               yyless(i) ;
               yyleng = i ;
               return(TICON) ;
           }
	  return(TOCTCON) ;
	  }
break;
case 16:
case 17:
case 18:
{ 
	/* reals (single prec) are 
	   a) a whole part, a dot, an opt. fraction, and an opt. exponent
	   b) an opt. whole part, a dot, a fraction, and an opt. exponent
	   c) a whole part and an exponent
	   In the case of reals with exponents, the exponent delimiter must
	   be a small 'e' to signify single precision.
	*/
	/* 5-7-90
	   the variable watch_for_integer is set in lexit() of drive.main
	   for any line that starts with INTEGER, REAL, etc.  There
	   cannot be real constants on this line, but something like
	   REAL*8 E1, can fool the scanner into reporting a real constant
	   8d1.  Prevent this.  Same patch below for doubles.
	   90-10-31 -- same patch above for HEX and OCT constants.
	*/
	   if (watch_for_integer) { 
	       char * p ; int i = 0 ;
	       for (p = yytext ; *p >= '0' && *p <= '9' ; p++) i++ ;
	       yyless(i) ;
               yyleng = i ; 
	       return(TICON) ;
	   }
	   return(TRCON) ;
	   }
break;
case 19:
case 20:
case 21:
{  
	/* reals (double prec) are 
	   a) a whole part, a dot, an opt. fraction, and a MAND. exponent
	   b) an opt. whole part, a dot, a fraction, and a MAND. exponent
	   c) a whole part and an exponent
	   In the case of reals with exponents, the exponent delimiter must
	   be a small 'd' to signify double precision.  In this case we
	   make the exponent mandatory because we want reals without 
	   exponents to default single precision.
	*/
	   if (watch_for_integer) { 
	       char * p ; int i = 0 ;
	       for (p = yytext ; *p >= '0' && *p <= '9' ; p++) i++ ;
	       yyless(i) ;
               yyleng = i ; 
	       return(TICON) ;
	   }
	   return(TDCON);
	   }
break;
case 22:
{
	/* identifiers and keywords
	   Must start with lower case a-z or underscore then have 0 or
	   more a-z, underscore, or digit chars.  We rely on the
	   setting of context to infer whether this is an identifier or
	   a keyword.  In the case of logical and block ifs, where the
	   next non-parenthesized identifier will be a keyword, we do
	   not reset the context variable.  In the case of implicit
	   statements (context m), where every non-parenthesized
	   identifier must be a keyword (a type name), we also do not
	   reset the context.
	*/
        int nummatched = 0 ;
	int i ;
	char temp[80] ;
#ifdef DEBUG
	yytext[yyleng]=0;
	fprintf(stderr,"%c, %s\n",context,yytext ) ;
#endif
	if ((context == 'a' || context == 'l' || context == 'k') && !inparen) {
	    nummatched = getkeyword(yytext,yyleng,&thistok) ;
	    if (thistok == TIF)  {
		if (context == 'a') {
		    thistok = TARITHIF ;
		} else {
		    thistok = TLOGIF ;
		}
	    }
	    if ((thistok != TASSIGN) &&
		(thistok != TLOGIF) && 
		(thistok != TELSEIF)) context = 'x' ;
	} else if (( context == 'm' ) && !inparen ) {
	    nummatched = getkeyword(yytext,yyleng,&thistok) ;
	} else {
	    /* scanner is ready to call this a TNAME, but let's
	       be sure that it's not a TFUNCTION TNAME.  This can
	       be the case if this is the first statement of a
	       new module, and the first TNAME found in the statement.
	       Watch_for_function gets set by lexit() in drive.main
	       if this situation exists. */
	    thistok = TNAME ;
	    if (watch_for_function) { /* possible function */
		nummatched = getkeyword(yytext,yyleng,&thistok) ;
		if (thistok != TFUNCTION) {
		    nummatched = 0 ;
	    	    thistok = TNAME ;
		}
	    }
	}
	if (nummatched > 0 ) {
            yyless( nummatched ) ;
	}
	return(thistok) ;
	}
break;
case 23:
case 24:
{
	/* string literals
	   can be delimited either by " or '.  These are included
	   in a string by "" or '', but from the scanner's point of
	   view these are two consecutive strings.  If we read a string
	   and the next character is the same string delimiter (" or ')
	   as the previous string, we use yymore() to get it too
	   and tack it onto the end of yytext, after first eliminating
	   the delimiter at the end of the first string so in the 
	   resultant value, instances of the delimiter in the string 
	   itself will be single.  As written here, the string values 
	   are returned with enclosing quotes.  That could be altered 
	   if desired.
	*/
	int i ; char c; 
	/* look ahead.  if next character is also a quote, then
	   then we actually have a quote in a string. */
        c = input() ; unput(c) ;
	if (((yytext[yyleng-1]=='\'') && (c == '\'' )) ||
	    ((yytext[yyleng-1]=='"') && (c == '"' ))) {
	    yyleng-- ;  /* get rid of delim at end of 1st string */
	    yymore() ;
	}
	else {
	    return(TSTRING) ;
	}
	}
break;
case 25:
return(TEMPTYPAR) ;
break;
case 26:
{ inparen++ ;
	  return(TLPAR) ;
	}
break;
case 27:
	{ if ( inparen > 0 ) inparen-- ;
	  else lexerr("unbalanced parentheses") ;
	  return(TRPAR) ;
	}
break;
case 28:
return(TTRUE) ;
break;
case 29:
return(TFALSE) ;
break;
case 30:
return(TTRUE) ;
break;
case 31:
return(TFALSE) ;
break;
case 32:
return(TAND) ;
break;
case 33:
return(TOR) ;
break;
case 34:
return(TXOR) ;
break;
case 35:
return(TNEQV) ;
break;
case 36:
return(TEQV) ;
break;
case 37:
return(TNOT) ;
break;
case 38:
return(TEQ) ;
break;
case 39:
return(TLT) ;
break;
case 40:
return(TGT) ;
break;
case 41:
return(TLE) ;
break;
case 42:
return(TGE) ;
break;
case 43:
return(TNE) ;
break;
case 44:
return(TCURRENCY) ;
break;
case 45:
return(TEQUALS) ;
break;
case 46:
return(TCOMMA) ;
break;
case 47:
return(TPLUS) ;
break;
case 48:
return(TMINUS) ;
break;
case 49:
	return(TPOWER) ;
break;
case 50:
return(TPERCENT) ;
break;
case 51:
return(TDOT) ;
break;
case 52:
	return(TSTAR) ;
break;
case 53:
return(TCONCAT) ;
break;
case 54:
return(TSLASH) ;
break;
case 55:
return(TCOLON) ;
break;
case -1:
break;
default:
fprintf(stdout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */
int yyvstop[] = {
0,

2,
0,

4,
0,

44,
0,

22,
50,
0,

26,
0,

27,
0,

52,
0,

47,
0,

46,
0,

48,
0,

51,
0,

54,
0,

10,
-9,
0,

55,
0,

45,
0,

22,
0,

22,
0,

22,
0,

23,
0,

24,
0,

25,
0,

49,
0,

17,
0,

53,
0,

16,
0,

13,
0,

8,
0,

1,
0,

3,
0,

31,
0,

30,
0,

16,
17,
0,

21,
0,

18,
0,

15,
0,

12,
0,

20,
0,

17,
0,

38,
0,

42,
0,

40,
0,

41,
0,

39,
0,

43,
0,

33,
0,

9,
0,

19,
0,

16,
0,

15,
0,

14,
0,

12,
0,

11,
0,

32,
0,

36,
0,

37,
0,

34,
0,

19,
20,
0,

16,
17,
0,

35,
0,

28,
0,

29,
0,

7,
0,

1,
7,
0,

5,
0,

5,
0,

6,
0,
0};
# define YYTYPE int
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	1,3,	
0,0,	0,0,	25,50,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,4,	1,5,	
25,25,	1,6,	1,7,	0,0,	
1,8,	1,9,	1,10,	1,11,	
1,12,	1,13,	1,14,	1,15,	
1,16,	1,17,	1,17,	1,17,	
1,17,	1,17,	1,17,	1,17,	
1,17,	1,17,	1,17,	1,18,	
9,30,	11,31,	1,19,	16,42,	
20,48,	21,49,	23,48,	24,49,	
61,93,	40,68,	58,90,	62,94,	
63,95,	64,96,	67,100,	0,0,	
35,59,	0,0,	0,0,	0,0,	
1,20,	15,32,	15,32,	15,32,	
15,32,	15,32,	15,32,	15,32,	
15,32,	15,32,	15,32,	1,21,	
89,118,	91,119,	65,97,	99,122,	
1,22,	102,124,	1,22,	1,22,	
1,22,	1,22,	1,22,	1,22,	
1,22,	1,22,	1,22,	1,22,	
1,22,	1,22,	1,22,	1,22,	
1,23,	1,22,	1,22,	1,22,	
1,22,	1,22,	1,22,	1,22,	
1,22,	1,22,	1,22,	1,24,	
3,25,	33,57,	34,58,	35,60,	
38,65,	39,67,	15,33,	36,61,	
37,63,	3,0,	15,34,	15,35,	
15,36,	40,69,	38,66,	41,70,	
57,89,	15,37,	58,91,	15,38,	
15,39,	60,92,	36,62,	37,64,	
66,99,	15,40,	69,101,	70,102,	
54,0,	15,41,	82,50,	3,25,	
79,110,	3,25,	80,112,	3,25,	
3,25,	65,98,	3,25,	79,111,	
92,120,	80,113,	3,25,	84,0,	
4,4,	98,121,	101,123,	3,25,	
3,25,	83,0,	54,54,	110,110,	
82,115,	4,0,	32,32,	32,32,	
32,32,	32,32,	32,32,	32,32,	
32,32,	32,32,	32,32,	32,32,	
3,25,	3,25,	54,54,	54,54,	
82,116,	82,116,	112,112,	83,116,	
120,133,	121,134,	123,135,	4,4,	
133,140,	4,4,	3,25,	4,4,	
4,4,	117,0,	4,4,	0,0,	
0,0,	138,144,	4,4,	83,116,	
83,116,	3,25,	137,142,	4,4,	
4,4,	115,50,	3,25,	0,0,	
3,26,	3,25,	3,25,	3,25,	
3,25,	138,144,	3,25,	0,0,	
3,26,	0,0,	32,55,	32,56,	
4,4,	4,4,	3,25,	84,117,	
137,143,	0,0,	5,5,	115,130,	
0,0,	0,0,	139,145,	3,27,	
0,0,	3,25,	4,4,	5,0,	
0,0,	0,0,	132,0,	0,0,	
137,144,	26,50,	0,0,	115,131,	
115,131,	4,4,	139,0,	0,0,	
0,0,	0,0,	4,4,	0,0,	
4,4,	4,4,	4,4,	4,4,	
4,4,	5,5,	4,4,	5,28,	
4,4,	5,5,	5,5,	26,51,	
5,5,	0,0,	4,4,	117,132,	
5,5,	0,0,	0,0,	0,0,	
0,0,	5,5,	5,5,	4,4,	
0,0,	4,4,	0,0,	26,52,	
26,53,	26,53,	26,53,	26,53,	
26,53,	26,53,	26,53,	26,53,	
26,53,	0,0,	5,5,	5,5,	
0,0,	7,22,	48,79,	48,79,	
48,79,	48,79,	48,79,	48,79,	
48,79,	48,79,	48,79,	48,79,	
5,5,	7,22,	7,22,	7,22,	
7,22,	7,22,	7,22,	7,22,	
7,22,	7,22,	7,22,	5,5,	
132,139,	0,0,	0,0,	0,0,	
5,5,	0,0,	5,5,	5,5,	
5,5,	5,5,	5,5,	0,0,	
5,5,	0,0,	5,5,	0,0,	
0,0,	0,0,	0,0,	0,0,	
5,5,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	5,5,	0,0,	5,5,	
0,0,	0,0,	0,0,	0,0,	
7,22,	0,0,	7,22,	7,22,	
7,22,	7,22,	7,22,	7,22,	
7,22,	7,22,	7,22,	7,22,	
7,22,	7,22,	7,22,	7,22,	
7,22,	7,22,	7,22,	7,22,	
7,22,	7,22,	7,22,	7,22,	
7,22,	7,22,	7,22,	7,22,	
8,8,	116,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	130,50,	
17,43,	8,0,	17,17,	17,17,	
17,17,	17,17,	17,17,	17,17,	
17,17,	17,17,	17,17,	17,17,	
0,0,	0,0,	0,0,	116,131,	
0,0,	0,0,	0,0,	0,0,	
17,44,	130,137,	131,0,	8,8,	
0,0,	8,8,	0,0,	8,8,	
8,8,	0,0,	8,29,	116,131,	
116,131,	0,0,	8,8,	0,0,	
27,54,	130,138,	130,138,	8,8,	
8,8,	0,0,	0,0,	0,0,	
131,138,	27,50,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
17,44,	0,0,	17,45,	17,46,	
8,8,	8,8,	17,47,	0,0,	
131,138,	131,138,	0,0,	0,0,	
0,0,	0,0,	0,0,	27,51,	
0,0,	27,54,	8,8,	27,54,	
27,54,	0,0,	27,54,	0,0,	
0,0,	0,0,	27,54,	0,0,	
0,0,	8,8,	0,0,	27,52,	
27,53,	0,0,	8,8,	0,0,	
8,8,	8,8,	8,8,	8,8,	
8,8,	0,0,	8,8,	0,0,	
8,8,	0,0,	0,0,	0,0,	
27,54,	27,54,	8,8,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	8,8,	
0,0,	8,8,	27,54,	43,71,	
43,71,	43,71,	43,71,	43,71,	
43,71,	43,71,	43,71,	43,71,	
43,71,	27,54,	0,0,	0,0,	
0,0,	0,0,	27,54,	0,0,	
27,54,	27,54,	27,54,	27,54,	
27,54,	0,0,	27,54,	0,0,	
27,54,	0,0,	0,0,	0,0,	
0,0,	0,0,	27,54,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	27,54,	
0,0,	27,54,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
43,72,	43,72,	43,72,	43,73,	
43,74,	43,72,	43,72,	43,72,	
43,72,	43,72,	43,72,	43,72,	
43,72,	43,72,	43,72,	43,72,	
43,72,	43,72,	43,72,	43,72,	
43,72,	43,72,	43,72,	43,72,	
43,72,	43,72,	45,75,	0,0,	
45,75,	0,0,	0,0,	45,76,	
45,76,	45,76,	45,76,	45,76,	
45,76,	45,76,	45,76,	45,76,	
45,76,	46,77,	0,0,	46,77,	
0,0,	0,0,	46,78,	46,78,	
46,78,	46,78,	46,78,	46,78,	
46,78,	46,78,	46,78,	46,78,	
49,80,	49,80,	49,80,	49,80,	
49,80,	49,80,	49,80,	49,80,	
49,80,	49,80,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	49,80,	49,80,	49,80,	
49,80,	49,80,	49,80,	55,85,	
51,81,	55,85,	0,0,	0,0,	
55,86,	55,86,	55,86,	55,86,	
55,86,	55,86,	55,86,	55,86,	
55,86,	55,86,	71,71,	71,71,	
71,71,	71,71,	71,71,	71,71,	
71,71,	71,71,	71,71,	71,71,	
0,0,	49,80,	49,80,	49,80,	
49,80,	49,80,	49,80,	51,82,	
0,0,	51,81,	0,0,	51,81,	
51,81,	0,0,	51,81,	0,0,	
0,0,	0,0,	51,81,	0,0,	
56,87,	0,0,	56,87,	51,83,	
51,83,	56,88,	56,88,	56,88,	
56,88,	56,88,	56,88,	56,88,	
56,88,	56,88,	56,88,	0,0,	
0,0,	0,0,	0,0,	0,0,	
51,81,	51,81,	71,103,	71,104,	
0,0,	0,0,	52,81,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	51,81,	52,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	51,81,	0,0,	0,0,	
0,0,	0,0,	51,81,	0,0,	
51,81,	51,81,	51,81,	51,81,	
51,81,	52,83,	51,81,	52,81,	
51,81,	52,81,	52,81,	0,0,	
52,81,	0,0,	51,81,	0,0,	
52,81,	0,0,	53,81,	0,0,	
0,0,	52,83,	52,83,	51,81,	
0,0,	51,81,	0,0,	53,0,	
75,76,	75,76,	75,76,	75,76,	
75,76,	75,76,	75,76,	75,76,	
75,76,	75,76,	52,81,	52,81,	
52,84,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	53,83,	0,0,	53,81,	
52,81,	53,81,	53,81,	0,0,	
53,81,	0,0,	0,0,	0,0,	
53,81,	0,0,	0,0,	52,81,	
0,0,	53,83,	53,83,	0,0,	
52,81,	0,0,	52,81,	52,81,	
52,81,	52,81,	52,81,	0,0,	
52,81,	0,0,	52,81,	0,0,	
0,0,	0,0,	53,81,	53,81,	
52,81,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
73,106,	52,81,	73,106,	52,81,	
53,81,	73,107,	73,107,	73,107,	
73,107,	73,107,	73,107,	73,107,	
73,107,	73,107,	73,107,	53,81,	
0,0,	0,0,	0,0,	0,0,	
53,81,	0,0,	53,81,	53,81,	
53,81,	53,81,	53,81,	0,0,	
53,81,	0,0,	53,81,	0,0,	
0,0,	0,0,	0,0,	0,0,	
53,81,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	53,81,	0,0,	53,81,	
72,105,	72,105,	72,105,	72,105,	
72,105,	72,105,	72,105,	72,105,	
72,105,	72,105,	72,105,	72,105,	
72,105,	72,105,	72,105,	72,105,	
72,105,	72,105,	72,105,	72,105,	
72,105,	72,105,	72,105,	72,105,	
72,105,	72,105,	74,108,	0,0,	
74,108,	0,0,	0,0,	74,109,	
74,109,	74,109,	74,109,	74,109,	
74,109,	74,109,	74,109,	74,109,	
74,109,	77,78,	77,78,	77,78,	
77,78,	77,78,	77,78,	77,78,	
77,78,	77,78,	77,78,	81,114,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
81,0,	85,86,	85,86,	85,86,	
85,86,	85,86,	85,86,	85,86,	
85,86,	85,86,	85,86,	87,88,	
87,88,	87,88,	87,88,	87,88,	
87,88,	87,88,	87,88,	87,88,	
87,88,	0,0,	81,114,	0,0,	
81,114,	0,0,	81,114,	81,114,	
0,0,	81,114,	0,0,	0,0,	
0,0,	81,114,	0,0,	103,125,	
0,0,	103,125,	81,114,	81,114,	
103,126,	103,126,	103,126,	103,126,	
103,126,	103,126,	103,126,	103,126,	
103,126,	103,126,	0,0,	0,0,	
104,127,	0,0,	104,127,	81,114,	
81,114,	104,128,	104,128,	104,128,	
104,128,	104,128,	104,128,	104,128,	
104,128,	104,128,	104,128,	0,0,	
0,0,	81,114,	106,107,	106,107,	
106,107,	106,107,	106,107,	106,107,	
106,107,	106,107,	106,107,	106,107,	
81,114,	0,0,	0,0,	0,0,	
0,0,	81,114,	0,0,	81,114,	
81,114,	81,114,	81,114,	81,114,	
0,0,	81,114,	114,129,	81,114,	
0,0,	0,0,	0,0,	0,0,	
0,0,	81,114,	0,0,	114,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	81,114,	0,0,	
81,114,	108,109,	108,109,	108,109,	
108,109,	108,109,	108,109,	108,109,	
108,109,	108,109,	108,109,	0,0,	
0,0,	114,129,	0,0,	114,129,	
0,0,	114,129,	114,129,	0,0,	
114,129,	0,0,	0,0,	0,0,	
114,129,	0,0,	0,0,	0,0,	
0,0,	114,129,	114,129,	125,126,	
125,126,	125,126,	125,126,	125,126,	
125,126,	125,126,	125,126,	125,126,	
125,126,	0,0,	0,0,	0,0,	
0,0,	0,0,	114,129,	114,129,	
127,128,	127,128,	127,128,	127,128,	
127,128,	127,128,	127,128,	127,128,	
127,128,	127,128,	129,136,	0,0,	
114,129,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	129,0,	
0,0,	0,0,	0,0,	114,129,	
0,0,	0,0,	0,0,	0,0,	
114,129,	0,0,	114,129,	114,129,	
114,129,	114,129,	114,129,	0,0,	
114,129,	0,0,	114,129,	0,0,	
0,0,	129,136,	0,0,	129,136,	
114,129,	129,136,	129,136,	0,0,	
129,136,	0,0,	0,0,	0,0,	
129,136,	114,129,	136,141,	114,129,	
0,0,	129,136,	129,136,	0,0,	
0,0,	0,0,	0,0,	136,141,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	129,136,	129,136,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	136,0,	0,0,	136,141,	
129,136,	136,141,	136,141,	0,0,	
136,141,	0,0,	0,0,	0,0,	
136,141,	0,0,	0,0,	129,136,	
0,0,	136,0,	136,141,	0,0,	
129,136,	0,0,	129,136,	129,136,	
129,136,	129,136,	129,136,	0,0,	
129,136,	0,0,	129,136,	0,0,	
0,0,	0,0,	136,141,	136,141,	
129,136,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	129,136,	0,0,	129,136,	
136,141,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	136,141,	
0,0,	0,0,	0,0,	0,0,	
136,141,	0,0,	136,141,	136,141,	
136,141,	136,141,	136,141,	0,0,	
136,141,	0,0,	136,141,	0,0,	
0,0,	0,0,	0,0,	0,0,	
136,141,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	136,141,	0,0,	136,141,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+1,	0,		0,	
yycrank+0,	yysvec+1,	0,	
yycrank+-123,	0,		yyvstop+1,
yycrank+-167,	0,		yyvstop+3,
yycrank+-237,	0,		0,	
yycrank+0,	0,		yyvstop+5,
yycrank+269,	0,		yyvstop+7,
yycrank+-391,	0,		0,	
yycrank+19,	0,		yyvstop+10,
yycrank+0,	0,		yyvstop+12,
yycrank+19,	0,		yyvstop+14,
yycrank+0,	0,		yyvstop+16,
yycrank+0,	0,		yyvstop+18,
yycrank+0,	0,		yyvstop+20,
yycrank+33,	0,		yyvstop+22,
yycrank+16,	0,		yyvstop+24,
yycrank+354,	0,		yyvstop+26,
yycrank+0,	0,		yyvstop+29,
yycrank+0,	0,		yyvstop+31,
yycrank+25,	0,		0,	
yycrank+26,	0,		0,	
yycrank+0,	yysvec+7,	yyvstop+33,
yycrank+27,	yysvec+7,	yyvstop+35,
yycrank+28,	yysvec+7,	yyvstop+37,
yycrank+4,	0,		0,	
yycrank+243,	0,		0,	
yycrank+-435,	0,		0,	
yycrank+0,	0,		yyvstop+39,
yycrank+0,	0,		yyvstop+41,
yycrank+0,	0,		yyvstop+43,
yycrank+0,	0,		yyvstop+45,
yycrank+130,	0,		yyvstop+47,
yycrank+15,	0,		0,	
yycrank+13,	0,		0,	
yycrank+30,	0,		0,	
yycrank+30,	0,		0,	
yycrank+31,	0,		0,	
yycrank+27,	0,		0,	
yycrank+15,	0,		0,	
yycrank+23,	0,		0,	
yycrank+28,	0,		0,	
yycrank+0,	0,		yyvstop+49,
yycrank+467,	0,		yyvstop+51,
yycrank+0,	0,		yyvstop+53,
yycrank+547,	0,		0,	
yycrank+562,	0,		0,	
yycrank+0,	0,		yyvstop+55,
yycrank+258,	0,		0,	
yycrank+572,	0,		0,	
yycrank+0,	0,		yyvstop+57,
yycrank+-643,	yysvec+27,	0,	
yycrank+-713,	0,		0,	
yycrank+-757,	0,		0,	
yycrank+-142,	yysvec+27,	yyvstop+59,
yycrank+600,	0,		0,	
yycrank+645,	0,		0,	
yycrank+40,	0,		0,	
yycrank+24,	0,		0,	
yycrank+0,	0,		yyvstop+61,
yycrank+37,	0,		0,	
yycrank+22,	0,		0,	
yycrank+25,	0,		0,	
yycrank+26,	0,		0,	
yycrank+27,	0,		0,	
yycrank+48,	0,		0,	
yycrank+32,	0,		0,	
yycrank+28,	0,		0,	
yycrank+0,	0,		yyvstop+63,
yycrank+33,	0,		0,	
yycrank+37,	0,		0,	
yycrank+610,	0,		yyvstop+65,
yycrank+783,	0,		0,	
yycrank+789,	yysvec+72,	0,	
yycrank+863,	yysvec+72,	0,	
yycrank+720,	0,		0,	
yycrank+0,	yysvec+75,	yyvstop+68,
yycrank+873,	0,		0,	
yycrank+0,	yysvec+77,	yyvstop+70,
yycrank+124,	yysvec+48,	yyvstop+72,
yycrank+126,	yysvec+49,	yyvstop+74,
yycrank+-930,	0,		0,	
yycrank+-144,	yysvec+81,	0,	
yycrank+-163,	yysvec+81,	0,	
yycrank+-157,	yysvec+81,	0,	
yycrank+893,	0,		0,	
yycrank+0,	yysvec+85,	yyvstop+76,
yycrank+903,	0,		0,	
yycrank+0,	yysvec+87,	yyvstop+78,
yycrank+46,	0,		0,	
yycrank+0,	0,		yyvstop+80,
yycrank+47,	0,		0,	
yycrank+49,	0,		0,	
yycrank+0,	0,		yyvstop+82,
yycrank+0,	0,		yyvstop+84,
yycrank+0,	0,		yyvstop+86,
yycrank+0,	0,		yyvstop+88,
yycrank+0,	0,		yyvstop+90,
yycrank+51,	0,		0,	
yycrank+49,	0,		0,	
yycrank+0,	0,		yyvstop+92,
yycrank+69,	0,		0,	
yycrank+51,	0,		0,	
yycrank+932,	0,		0,	
yycrank+949,	0,		0,	
yycrank+0,	0,		yyvstop+94,
yycrank+962,	0,		0,	
yycrank+0,	yysvec+106,	yyvstop+96,
yycrank+1005,	0,		0,	
yycrank+0,	yysvec+108,	yyvstop+98,
yycrank+143,	0,		yyvstop+100,
yycrank+0,	0,		yyvstop+102,
yycrank+162,	0,		yyvstop+104,
yycrank+0,	0,		yyvstop+106,
yycrank+-1033,	0,		0,	
yycrank+-207,	yysvec+114,	0,	
yycrank+-383,	yysvec+114,	0,	
yycrank+-195,	yysvec+114,	0,	
yycrank+0,	0,		yyvstop+108,
yycrank+0,	0,		yyvstop+110,
yycrank+95,	0,		0,	
yycrank+151,	0,		0,	
yycrank+0,	0,		yyvstop+112,
yycrank+152,	0,		0,	
yycrank+0,	0,		yyvstop+114,
yycrank+1035,	0,		0,	
yycrank+0,	yysvec+125,	yyvstop+116,
yycrank+1052,	0,		0,	
yycrank+0,	yysvec+127,	yyvstop+119,
yycrank+-1109,	0,		0,	
yycrank+-389,	yysvec+129,	0,	
yycrank+-412,	yysvec+129,	0,	
yycrank+-240,	yysvec+129,	0,	
yycrank+154,	0,		0,	
yycrank+0,	0,		yyvstop+122,
yycrank+0,	0,		yyvstop+124,
yycrank+-1153,	0,		0,	
yycrank+-204,	yysvec+136,	0,	
yycrank+-177,	yysvec+136,	0,	
yycrank+-210,	yysvec+136,	0,	
yycrank+0,	0,		yyvstop+126,
yycrank+0,	0,		yyvstop+128,
yycrank+0,	0,		yyvstop+130,
yycrank+0,	yysvec+25,	yyvstop+133,
yycrank+0,	0,		yyvstop+135,
yycrank+0,	0,		yyvstop+137,
0,	0,	0};
struct yywork *yytop = yycrank+1275;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,012 ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
040 ,01  ,'"' ,01  ,'$' ,'%' ,01  ,047 ,
01  ,01  ,01  ,'+' ,01  ,'+' ,01  ,01  ,
'0' ,'1' ,'1' ,'1' ,'1' ,'1' ,'1' ,'1' ,
'1' ,'1' ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,'A' ,'B' ,'A' ,'A' ,'A' ,'A' ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,'O' ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,'Z' ,01  ,01  ,01  ,01  ,'_' ,
01  ,'a' ,'b' ,'c' ,'d' ,'e' ,'c' ,'g' ,
'g' ,'i' ,'g' ,'i' ,'i' ,'i' ,'g' ,'o' ,
'g' ,'g' ,'g' ,'g' ,'g' ,'g' ,'g' ,'g' ,
'x' ,'g' ,'z' ,01  ,01  ,01  ,01  ,01  ,
0};
char yyextra[] = {
0,0,0,0,0,0,0,0,
0,1,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0};
#ifndef lint
static	char ncform_sccsid[] = "@(#)ncform 1.6 88/02/08 SMI"; /* from S5R2 1.2 */
#endif

int yylineno =1;
# define YYU(x) x
# define NLSTATE yyprevious=YYNEWLINE
char yytext[YYLMAX];
struct yysvf *yylstate [YYLMAX], **yylsp, **yyolsp;
char yysbuf[YYLMAX];
char *yysptr = yysbuf;
int *yyfnd;
extern struct yysvf *yyestate;
int yyprevious = YYNEWLINE;
yylook(){
	register struct yysvf *yystate, **lsp;
	register struct yywork *yyt;
	struct yysvf *yyz;
	int yych, yyfirst;
	struct yywork *yyr;
# ifdef LEXDEBUG
	int debug;
# endif
	char *yylastch;
	/* start off machines */
# ifdef LEXDEBUG
	debug = 0;
# endif
	yyfirst=1;
	if (!yymorfg)
		yylastch = yytext;
	else {
		yymorfg=0;
		yylastch = yytext+yyleng;
		}
	for(;;){
		lsp = yylstate;
		yyestate = yystate = yybgin;
		if (yyprevious==YYNEWLINE) yystate++;
		for (;;){
# ifdef LEXDEBUG
			if(debug)fprintf(stdout,"state %d\n",yystate-yysvec-1);
# endif
			yyt = yystate->yystoff;
			if(yyt == yycrank && !yyfirst){  /* may not be any transitions */
				yyz = yystate->yyother;
				if(yyz == 0)break;
				if(yyz->yystoff == yycrank)break;
				}
			*yylastch++ = yych = input();
			yyfirst=0;
		tryagain:
# ifdef LEXDEBUG
			if(debug){
				fprintf(stdout,"char ");
				allprint(yych);
				putchar('\n');
				}
# endif
			yyr = yyt;
			if ( (int)yyt > (int)yycrank){
				yyt = yyr + yych;
				if (yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
# ifdef YYOPTIM
			else if((int)yyt < (int)yycrank) {		/* r < yycrank */
				yyt = yyr = yycrank+(yycrank-yyt);
# ifdef LEXDEBUG
				if(debug)fprintf(sdtout,"compressed state\n");
# endif
				yyt = yyt + yych;
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				yyt = yyr + YYU(yymatch[yych]);
# ifdef LEXDEBUG
				if(debug){
					fprintf(stdout,"try fall back character ");
					allprint(YYU(yymatch[yych]));
					putchar('\n');
					}
# endif
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transition */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
			if ((yystate = yystate->yyother) && (yyt= yystate->yystoff) != yycrank){
# ifdef LEXDEBUG
				if(debug)fprintf(stdout,"fall back to state %d\n",yystate-yysvec-1);
# endif
				goto tryagain;
				}
# endif
			else
				{unput(*--yylastch);break;}
		contin:
# ifdef LEXDEBUG
			if(debug){
				fprintf(stdout,"state %d char ",yystate-yysvec-1);
				allprint(yych);
				putchar('\n');
				}
# endif
			;
			}
# ifdef LEXDEBUG
		if(debug){
			fprintf(stdout,"stopped at %d with ",*(lsp-1)-yysvec-1);
			allprint(yych);
			putchar('\n');
			}
# endif
		while (lsp-- > yylstate){
			*yylastch-- = 0;
			if (*lsp != 0 && (yyfnd= (*lsp)->yystops) && *yyfnd > 0){
				yyolsp = lsp;
				if(yyextra[*yyfnd]){		/* must backup */
					while(yyback((*lsp)->yystops,-*yyfnd) != 1 && lsp > yylstate){
						lsp--;
						unput(*yylastch--);
						}
					}
				yyprevious = YYU(*yylastch);
				yylsp = lsp;
				yyleng = yylastch-yytext+1;
				yytext[yyleng] = 0;
# ifdef LEXDEBUG
				if(debug){
					fprintf(stdout,"\nmatch ");
					sprint(yytext);
					fprintf(stdout," action %d\n",*yyfnd);
					}
# endif
				return(*yyfnd++);
				}
			unput(*yylastch);
			}
		if (yytext[0] == 0  /* && feof(stdin) */)
			{
			yysptr=yysbuf;
			return(0);
			}
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
		yylastch=yytext;
# ifdef LEXDEBUG
		if(debug)putchar('\n');
# endif
		}
	}
yyback(p, m)
	int *p;
{
if (p==0) return(0);
while (*p)
	{
	if (*p++ == m)
		return(1);
	}
return(0);
}
	/* the following are only used in the lex library */
yyinput(){
	return(input());
	}
yyoutput(c)
  int c; {
	output(c);
	}
yyunput(c)
   int c; {
	unput(c);
	}

yywrap() { return(1) ; }

