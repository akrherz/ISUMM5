/* ioque.c SCCS(4.1 91/02/18 15:42:58) */

/* input() and unput() for lexer */

#include <stdio.h>
#include "defines.h"

/* every character of a fortran program has three attributes
    1.  value
    2.  position in the card (i.e. 1-80)
    3.  card number
   For pushback, we keep three stacks, with the same pointer, pb.
   MAXSTACK is declared in defines.h
*/

int pb = 0 ; /* pushback stack pointer */
int valque[MAXSTACK] ;
int posque[MAXSTACK] ;
int cardnoque[MAXSTACK] ;

extern int yyposition[] ; /* declared in lexer */
extern int yycardno[] ;
extern int pos ; 

/* input()

   This routine is called by lexer.  It returns the next character
   from the input stream, or the next character on the pushback stack.
   It stores in the global arrays yyposition and yycard the position and
   card number of the character in question.
*/
char input()
{
   int nextposition(), nextcardno();
   char nextcharacter() ;
   char c ;
   if ((pb == 0) && (pos < YYLMAX)) {
       c = nextcharacter() ; /* this must be called first */
       yyposition[pos] = nextposition() ;
       yycardno[pos++] = nextcardno() ;
       return(c) ;
   } else if ((pb > 0) && (pos < YYLMAX)) {
       yyposition[pos] = posque[--pb] ;
       yycardno[pos++] = cardnoque[pb] ;
       return( valque[pb] ) ;
   } else {
       char temp[100] ;
       sprintf(temp,"%s pb %d pos %d","input() pushback stack error",pb,pos) ;
       comp_err(temp) ;
   }
}

unput(c)
char c ;
{
   if ((pb < MAXSTACK) && (pos > 0))  {
       posque[pb] = yyposition[--pos] ;
       cardnoque[pb] = yycardno[pos] ;
       valque[pb++] = c ;
   } else {
       comp_err("unput() pushback stack error") ;
   }

}

static int posit, cardno ;

char nextcharacter()
{
    char cval ;
    FGETC(&cval, &posit, &cardno) ;
    return(cval) ;
}

/* these last two could easily be macros */
int nextposition()
{
    return(posit) ;
}

int nextcardno()
{
    return(cardno) ;
}

/* this can be called to reset the pushback stack to
   empty, thus resetting the scanner to a fresh state.
   This is desireable after finishing with an include
   file.  Call is from popincl() in card1.c
*/
initpushback()
{
    pb = 0 ;
}
getpbsp()
{
    return(pb) ;
}
setpbsp(n)
int n ;
{
    pb = n ;
}
