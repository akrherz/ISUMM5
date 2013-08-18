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
/*
93-03-25

prback.c

	This file contains a definition of "user_backend()" that
	prints to standard output a facimile of the code the
	scanner/parser saw.

	It also contains revised versions of getstat, named getstat_masked,
	which allow specification of a "child mask"; that is, an integer
	array of size maxChildren, which can be used to mask off 
	the children of a node we do not want to consider.  This is
	needed for several of the exK nodes, such as doK and ifthenelseK,
	which have sub-bodies.  It also prevents the printing of
	"endif" prematurely, in the case of ifthenelseK.  NOTE:
	getstat with a child mask seems like a useful form function in
	general; should consider adding and documenting.

	This set of routines also makes use of the undocumented
	"walk_statements1()" routine, which is part of N32's parselib.a.
	It is the recursive routine that is called by walk_statements()
	(which is documented) to handle the linked list of statements
	depending from a moduleK node.  NOTE:  this is a very useful
	routine in general, and should be included in the docs.

	The back-end program here handles several of the exK nodes --
	those with sub-body's, lists of statements, as children --
	as special cases.  NOTE: some sort of macro should be provided
	that allows easy testing for this condition.

	P. Whiting has found a bug in the N32 code itself: colon
	tokens don't print because the colon is used as a delimiter
	in the token recs returned by the scanner.  Ouch.   A more
	general fix that changes this delimeter must be made, but
	this is an extensive change and will require a new version 
	of N32.  Therefore, prback, contains a fairly simple kludge
	that tests to see of a token is TCOLON, and if it is, just
	goes ahead and prints one.   This is not exercised in the
	sample program x.f, but will be very important for data-parallel
	Fortran array syntax.

*/

#include <stdio.h>
#include "n32.h"

#define TWST_MAIN
#include "twst.h"
#include "dm.h"

#define LINES 40
#define LEN   256

static int in_exec = 0 ;
static char buff[LINES*LEN] ;
static int indent = 0 ;
static int indent_sv ;
static int dont_break = -20 ;
static int if_logical = 0 ;       /* flag for logical if */
static int suppress_indent[30] ;  /* kludge -- suppress indentation
				     of last statement in a loop body
				     (because it is a continue or enddo) */
static int sv_L = 0 ;
static char temp[LINES][LEN] ;
static char temp2[LINES*LEN] ;
static char *p2 ;
static int lineno ;
static int more_to_go ;
static int continue_traversal ;
static int curs  ;
static int cutcurs ;
static int no_strings ;		/* flag telling whether there are strings */
static char spaces[] = 
"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                " ;


int statementnum = 0 ;
int modulenum = 0 ;
char **getstat_masked() ;

char * strip_lead(), *strip_nl()  ;

prback( node )
NodePtr node ;  /* djs@dr-dave.com, 9709 */
{
    int prback_new_module(), prback_statement() ;

    walk_statements( node, prback_new_module, prback_statement, 0 ) ;
    outbuf("\n") ;
}

prback_new_module( node )
NodePtr node ;
{
    int i ;
    if ( node == NULL ) return ;
    in_exec = 0 ;
    indent = 0 ;
    for ( i = 0 ; i < 30 ; i++ )
    {
      suppress_indent[i] = 0 ;
    }
    modulenum++ ;
}

string_search( node )
  NodePtr node ;
{
  if ( node == NULL ) return ;
  if ( MINORKIND(node) == TSTRING ) no_strings = 0 ;
}

cap_it( node )
  NodePtr node ;
{
  if ( node == NULL ) return ;
  if ( MAJORKIND(node) == tokenK && MINORKIND(node) != TSTRING )
    cap_token( MINORKIND(node), TOKSTRING(node) ) ;
}

prback_statement( node )
NodePtr node ;
{
    NodePtr s ;
    NodePtr firsttok(), lasttok() ;
    int i, bef ;
    int suppressed ;
    int childmask[maxChildren] ;
    char nstr[10] ;

    if ( node == NULL ) return ;

    if ( EXNODE( node ) )
    {
      if ( in_exec == 0 )
      {
	outbuf( "CFLIC END DECLARATIONS\n" ) ;
      }
      in_exec = 1 ;
    }
    if ( sw_E && !in_exec ) return ;

    no_strings = 1 ;

/* this supports the -P and -H options of dm */
    if ( node->user_int == HEADERHERE && header[0] != '\0' )
    {
      outbuf( header ) ;
    }

/* FORTRAN includes are already expanded (provided they
   were in the original input).  Don't output the include
   statements. */
    if ( MINORKIND(node) == includeK ) return ;

    switch ( MINORKIND(node) )
      {
      case callK :
      {
	NodePtr x ;
        char sstat[1024] ;
        char templine[1024] ;
        /* check for the name of a directive */

        if (!strcmp("rsl_runpad",TOKSTRING(CHILD(node,2))))
        {
          if ( CHILD(node,4) != NULL )
          {
	    x = SIBLING( CHILD(node,4) ) ;
	    SIBLING( CHILD(node,4) ) = NULL ;
            getstat( CHILD(node,4), 1, sstat ) ;
	    SIBLING( CHILD(node,4) ) = x ;
            sprintf(templine,"C_RSL_RUNPAD(%s)\n",
                    strip_nl(strip_lead(sstat)) ) ;
            outbuf(templine) ;
	    return ;				/* RETURN */
          }
        }
        else
        {
          s = SIBLING( node ) ;
          SIBLING(node) = NULL ;
          walk_depth(node, string_search, 1 ) ;
          SIBLING( node ) = s ;
        }
      }
      break ;
    case formatK :
    case assignK :
    case statfunK :
    case printK :
    case dataK :
    case writeK :
      s = SIBLING( node ) ;
      SIBLING(node) = NULL ;
      walk_depth(node, string_search, 1 ) ;
      SIBLING( node ) = s ;
      break ;
    default :
      break ;
    }
      
    {

      bef = indent ;
      suppressed = 0 ;
      if ( STATEMENT(node) &&
	   SIBLING(node) == NULL &&
	   suppress_indent[indent/2] &&
	   indent-2 >= 0 ) 
      {
        suppressed = 1 ;
        indent -= 2 ;
      }

      for (i = 0 ; i < maxChildren ; i++ ) childmask[i] = 1 ;
      switch (MINORKIND(node))
      {

	  case statfunK :
	  case assignK :
	      if ( sw_RF )
	      {

	        for (i = 3 ; i < maxChildren ; i++ ) childmask[i] = 0 ;
                getstat_masked(node,LINES,buff,childmask) ;
	        strip_nl( buff ) ;
	        outbuf(buff) ;
	        indent_sv = indent ;
	        indent = strlen(buff)-((sw_L==1)?11:6) ;
	        indent = (indent>0)?indent:0 ;
                sv_L = sw_L ;
                sw_L = 0 ;
	        format_rhs( CHILD(node,3) ) ;
                sw_L = sv_L ;
	        sv_L = 0 ;
	        indent = indent_sv ;
	      }
	      else
	      {
                getstat_masked(node,LINES,buff,childmask) ;
                statementnum++ ;
	        outbuf(buff) ;
	      }
	      break ;
          case ifthenelseK :

              if ( sw_RF )
	      {
                do_others( node, 2, 5, childmask ) ;
                statementnum++ ;
	      }
	      else
	      {
	        for ( i = 6 ; i < maxChildren ; i++ ) childmask[i] = 0 ;
                getstat_masked(node,LINES,buff,childmask) ;
                outbuf(buff) ;
	      }

	      indent += 2 ;
	      walk_statements1(CHILD(node,6),prback_statement, 0, 0 ) ;
	      indent -= 2 ;
              if ( CHILD(node,7) ) {
	         if ( sw_L )
		 { sprintf(nstr,"%4d\t",atoi(TOKBEGIN(CHILD(node,7))) ) ;
		   outbuf(nstr) ; }
	         makeindent();outbuf("      ELSE\n"); }
              statementnum++ ;
	      indent += 2 ;
	      walk_statements1(CHILD(node,8),prback_statement, 0, 0 ) ;
	      indent -= 2 ;
              if ( CHILD(node,9) ) {
	         if ( sw_L )
		 { sprintf(nstr,"%4d\t",atoi(TOKBEGIN(CHILD(node,9))) ) ;
		   outbuf(nstr) ; }
	         makeindent();outbuf("      ENDIF\n"); }
              statementnum++ ;
              break ;
          case ifthenelseifK :
              if ( sw_RF )
	      {
                do_others( node, 2, 5, childmask ) ;
                statementnum++ ;
	      }
	      else
	      {
  	        for ( i = 6 ; i < maxChildren ; i++ ) childmask[i] = 0 ;
                getstat_masked(node,LINES,buff,childmask) ;
                outbuf(buff) ;
	      }

	      indent += 2 ;
	      walk_statements1(CHILD(node,6),prback_statement, 0, 0 ) ;
	      indent -= 2 ;
	      { int supp_save ;
	        supp_save = suppress_indent[indent/2] ;
	        suppress_indent[indent/2] = 0 ;
	        walk_statements1(CHILD(node,7),prback_statement, 0, 0 ) ;
	        suppress_indent[indent/2] = supp_save ;
	      }
              break ;
          case doK :
	      /* this business is all just to add a space after the
	         integer constant and before the loop variable */
	      {
	        char buf2[256] ;
	        char buf3[256] ;
	        for ( i = 4 ; i < maxChildren ; i++ ) childmask[i] = 0 ;
                getstat_masked(node,LINES,buf2,childmask) ;
	        strip_nl(buf2) ;
                for (i = 0 ; i < 4 ; i++ ) childmask[i] = 0 ;
                for (i = 4 ; i < 7 ; i++ ) childmask[i] = 1 ;
	        for ( i = 7 ; i < maxChildren ; i++ ) childmask[i] = 0 ;
	        sv_L = sw_L ;
	        sw_L = 0 ;
                getstat_masked(node,LINES,buf3,childmask) ;
	        sw_L = sv_L ;
	        sv_L = 0 ;
	        strcpy(buff,buf2) ;	/* the DO 10 */
	        strcat(buff," ") ;	/* the much desired space */
	        strcat(buff,buf3) ;	/* the i=1,100 */
	      }
              statementnum++ ;
	      outbuf(buff) ;
	      indent += 2 ;
	      suppress_indent[indent/2] = 1 ;
	      walk_statements1(CHILD(node,7),prback_statement, 0, 0 ) ;
	      suppress_indent[indent/2] = 0 ;
	      indent -= 2 ;
              break ;
          case doenddoK :
	      for ( i = 6 ; i < maxChildren ; i++ ) childmask[i] = 0 ;
              getstat_masked(node,LINES,buff,childmask) ;
              statementnum++ ;
	      outbuf(buff) ;
	      indent += 2 ;
	      suppress_indent[indent/2] = 1 ;
	      walk_statements1(CHILD(node,6),prback_statement, 0, 0 ) ;
	      suppress_indent[indent/2] = 0 ;
	      indent -= 2 ;
              break ;
          case 9000 :
              outbuf( node->user_def ) ;
              statementnum++ ;
              indent += 2 ;
              suppress_indent[indent/2] = 1 ;
              walk_statements1(CHILD(node,1),prback_statement, 0, 0 ) ;
              suppress_indent[indent/2] = 0 ;
              indent -= 2 ;
              break ;
          case 9001 :
          case 9100 :
              outbuf( node->user_def ) ;
              break ;
          case dowhileK :
	      if ( sw_RF )
	      {
	        for ( i = 5 ; i < maxChildren ; i++ ) childmask[i] = 0 ;
                getstat_masked(node,LINES,buff,childmask) ;
	        outbuf(buff) ;
	      }
	      else
	      {
                do_others( node, 2, 4, childmask ) ;
                statementnum++ ;
	      }
	      indent += 2 ;
	      walk_statements1(CHILD(node,5),prback_statement, 0, 0 ) ;
	      indent -= 2 ;
              break ;
          case parameterK :
              if ( sw_RF )
              {
                int indent_sv ;
                for (i = 3 ; i < maxChildren ; i++ ) childmask[i] = 0 ;
                getstat_masked(node,LINES,buff,childmask) ;
                strip_nl( buff ) ;
                outbuf(buff) ;
                indent_sv = indent ;
                indent = strlen(buff)-((sw_L==1)?11:6) ;
                indent = (indent>0)?indent:0 ;
                sv_L = sw_L ;
                sw_L = 0 ;
                format_kids( node, 3, 4 ) ;
                sw_L = sv_L ;
                sv_L = 0 ;
                indent = indent_sv ;
              }
              else
              {
                getstat_masked(node,LINES,buff,childmask) ;
                statementnum++ ;
                outbuf(buff) ;
              }
              break ;
          case ifarithK :
              do_others( node, 2, 9, childmask ) ;
              break ;
          case iflogicalK :
	      if_logical = 1 ;
              do_others( node, 2, 5, childmask ) ;
	      if_logical = 0 ;
              break ;
	  case returnK :
	  case continueK :
              do_others( node, 1, 1, childmask ) ;
              break ;
	  case formatK :
              do_others( node, 2, 4, childmask ) ;
              break ;
	  case callK :
              do_others( node, 2, 5, childmask ) ;
              break ;
	  case readK :
	  case writeK :
	  case entryK :
	  case subroutineK :
              do_others( node, 3, 3, childmask ) ;
              break ;
	  case printK :
              do_others( node, 3, 4, childmask ) ;
              break ;
	  case functionK :
              do_others( node, 4, 4, childmask ) ;
              break ;
          case pointerK :
	      dont_break = parenK ;
              do_others( node, 2, 2, childmask ) ;
	      dont_break = -20 ;
	      break ;
          case gotocomputedK :
              do_others( node, 2, 6, childmask ) ;
              break ;
	  case programK :
              do_others( node, 2, 2, childmask ) ;
              break ;
	  case commonK :
	  case dimensionK :
	  case dataK :
	  case typedeclK :
	  default :
              do_others( node, 2, 2, childmask ) ;
              break ;
      }
      if ( suppressed ) indent += 2 ;
    }
}

do_others( node, b, e, childmask )
  NodePtr node ;
  int b, e ;
  int childmask[] ;
{
  int i ;

            if ( sw_RF )
            {
              int indent_sv ;
              for (i = b ; i < maxChildren ; i++ ) childmask[i] = 0 ;
              getstat_masked(node,LINES,buff,childmask) ;
              strip_nl( buff ) ;
              outbuf(buff) ;
              indent_sv = indent ;
              indent = strlen(buff)-((sw_L==1)?11:6) ;
              indent = (indent>0)?indent:0 ;
              sv_L = sw_L ;
              sw_L = 0 ;
              format_kids( node,b,e ) ;
              sw_L = sv_L ;
              sv_L = 0 ;
              indent = indent_sv ;
            }
            else
            {
              getstat_masked(node,LINES,buff,childmask) ;
              statementnum++ ;
              outbuf(buff) ;
            }
}


makeindent()
{
  int i ;
  for ( i = 0 ; i < indent ; i++ ) putc(' ',outfile) ;
}

outbuf( buff )
  char * buff ;
{
  fprintf(outfile,"%s",buff) ;
}

char **
getstat_masked(node, lines, buf, childmask )
NodePtr node ;
int    lines ;	/* number of lines consider printing (0 is this line only) */
char   * buf ;
int    childmask[] ;
{
    int i ;
    NodePtr tok, firsttok() ;

    temp2[0] = '\0' ;
    p2 = temp2 ;
    *buf = '\0' ;
    if (node == NULL) return ;

			/* get line number from first token */
    if ((tok = firsttok( node )) == NULL)
	return((char **)temp) ;
    lineno = atoi(TOKBEGIN(tok)) ;

    myinitTemp() ;
    getstat_masked1( node, lines, temp, lineno, childmask ) ;
    buf[0] = '\0' ;
    if ( sw_L && lineno > 0 )
    {
      sprintf(buf, "%4d\t",lineno ) ;
    }
    strcat( buf,temp2 ) ;
    strcat(buf,"\n") ;

}

getstat_masked1(node,lines,temp,lineno,childmask)		/* recursive */
NodePtr node ;
int  lines ;
char temp[][LEN] ;
int  lineno ;
int  childmask[] ;
{
    int i ;
    char token_str[256] ;
    int newchildmask[maxChildren] ;
    NodePtr p ;
    if (node == NULL) return ;

    if (MAJORKIND(node) == tokenK) {
        if ( MINORKIND(node) == 9009  || MINORKIND(node) == 9001 && node->user_def != NULL )
        {
            strcpy( token_str, node->user_def ) ;
        } else
	if ( MINORKIND(node) == TCOLON )
	{
 	    strcpy( token_str, ":" ) ;   /* kludge -- scanner encodes token
					    records with ':' delimiter;
					    consequently, it will not print
					    so we force it to here */
	}
	else if ( MINORKIND(node) == TLABEL ||
		  MINORKIND(node) == TSOUGHTLABEL )
	{
	  if ( if_logical > 1 )
	  {
	    token_str[0] = '\0' ;
	  }
	  else if ( !strcmp(TOKFILE(node), "fake" ) )
	  {
	    strncpy( token_str, spaces, indent+6 ) ;
	    token_str[indent+6] = '\0' ;
	  }
	  else if ( !strcmp(TOKSTRING(node), "0") )
	  {
	    strncpy( token_str, spaces, indent+6 ) ;
	    token_str[indent+6] = '\0' ;
	  }
	  else
	  {
	    sprintf(token_str,"%5s ", TOKSTRING( node ) ) ;
	    strncat( token_str, spaces, indent ) ;
	  }
	  if ( if_logical == 1 ) if_logical++ ;
	}
	else if ( MINORKIND(node) == TNAMEEQ )
	{
 	    strcpy( token_str, TOKSTRING( node )) ;
	    strcat( token_str, "=" ) ;
	}
	else
	{
 	    strcpy( token_str, TOKSTRING( node )) ;
	}
        if (atoi(TOKBEGIN(node)) <= lineno + lines) {
	    cap_token( MINORKIND(node), token_str ) ;
	    strcat( temp2, token_str ) ;
	    if ( space_after_token( MINORKIND(node) ) )
	    {
	      strcat( temp2, " " ) ;
	    }
	    if (atoi(TOKBEGIN(node))-lineno >= 0) {
                strinsert(temp[atoi(TOKBEGIN(node))-lineno],token_str,atoi(TOKCOLS(node))) ;
	    }
        }
    }

    for ( i = 0 ; i < maxChildren ; i++ ) newchildmask[i] = 1 ;

    for ( i = 0 ; i < maxChildren ; i++ ) 
    {
	if ( ! childmask[i] ) continue ;
	if ((p = CHILD(node,i)) == NULL) continue ;
	getstat_masked1(p,lines,temp,lineno,newchildmask) ;
    }
    if ( !STATEMENT(SIBLING( node ))) 
    {
	getstat_masked1(SIBLING(node),lines,temp,lineno,newchildmask) ;
    }
    /*
    else{ printf("a statement\n") ;}
    */
    return ;
}

myinitTemp()
{
    int i, j ;
    for (i=0;i<LINES;i++)
    for (j=0;j<LEN;j++)
        temp[i][j] = ' ' ;
}

mark_clear( node )
  NodePtr node ;
{
  if ( node != NULL ) USERINT(node) = 0 ;
}
mark_set( node )
  NodePtr node ;
{
  if ( node != NULL ) USERINT(node) = 1 ;
}

static int respect ;

as_far_as_fits( node )
  NodePtr node ;
{
  char * p, buf2[256] ;
  int n, i ;
  NodePtr s ;
  int childmask[maxChildren] ;

  if ( continue_traversal )
  {
    if ( respect &&
	 (MINORKIND(node) == dont_break ||
	  MINORKIND(node) == idrefK ||
	  MINORKIND(node) == iddeclK ))
    {
      if ( ! USERINT(node) )	/* if node not already visited */
      {
        for (i = 0 ; i < maxChildren ; i++ ) childmask[i] = 1 ;
        s = SIBLING(node) ;
        SIBLING(node) = NULL ;
	getstat_masked( node, LINES, buf2, childmask ) ;
	strip_nl( buf2 ) ;
        n = strlen ( buf2 ) ;
        if ( curs+n <= 72 )
	{
          strcat(buff,buf2) ;
          curs += n ;
	  walk_depth( node, mark_set, 1 ) ;
	  respect = 1 ;
	}
        else
        {
#if 0
fprintf(stderr,"buff |%s|\nbuf2 |%s|\nn %d\nindent + 6 + n %d\n",buff,buf2,n,indent + 6 + n) ;
#endif
	  USERINT(node) = 1 ;
	  respect = 0 ;
	  continue_traversal = 0 ;
        }
        SIBLING(node) = s ;
      }
    }
    else if ((MAJORKIND(node) == tokenK || MINORKIND(node) == 9001 )&&
	      MINORKIND(node) != TLABEL  &&
	      MINORKIND(node) != TSOUGHTLABEL )
    {
      if ( ! USERINT(node) )	/* if node not already visited */
      {
        s = SIBLING(node) ;
        SIBLING(node) = NULL ;

if (MINORKIND(node)==9009 || MINORKIND(node)==9001)
        p = (char *)(node->user_def) ;
else
        p = TOKSTRING( node ) ;

if (MINORKIND(node)==9001) p = (char *)strip_lead(p) ;
        n = strlen ( p ) ;
	if ( space_after_token( MINORKIND(node) ) ) n++ ;
	if ( MINORKIND( node ) == TNAMEEQ ) n++ ; /* room for equal sign */
        if ( curs+n <= 72 )
        {
	  char tok[256] ;
	  strcpy( tok, p ) ;
	  if ( MINORKIND(node) != TSTRING ) cap_token( MINORKIND(node),tok ) ;
          strcat(buff,tok) ;
	  if ( MINORKIND( node ) == TNAMEEQ ) strcat( buff, "=" ) ;
	  if ( space_after_token( MINORKIND(node) ) ) strcat( buff, " " ) ;
          curs += n ;
	  walk_depth( node, mark_set, 1 ) ;
	  respect = 1 ;
        }
        else
        {
          if ( indent + 6 + n <= 72 )  /* see if it'll fit next line */
	  {
	    continue_traversal = 0 ;
	  }
	  else if ( 6 + n <= 72 )  /* 970225 won't be pretty but... */
	  {
	    indent = 0 ;
	    continue_traversal = 0 ;
	  }
	  else
	  {
	    fprintf(stderr,"prback.c: Stuck: \n|%s|\n",p) ;
	    exit(2) ;
	  }
        }
        SIBLING(node) = s ;
      }
    }
  }
}

format_kids( node, b, e ) 
  NodePtr node ;
  int b, e ;
{
  int i ;
  buff[0] = '\0' ;
  curs = indent+6 ;
#if 0
  for ( i = b ; i <= e-1 ; i++ )
  {
    format_rhs_base( CHILD( node , i), 0 ) ;
  }
  if ( e >= b ) format_rhs_base( CHILD( node , e), 1 ) ;
#else
  for ( i = b ; i <= e ; i++ )
  {
    format_rhs_base( CHILD( node , i), 0 ) ;
  }
  strcat(buff,"\n") ;
#endif
  outbuf(buff) ;
}

format_kids_nonl( node, b, s ) 
  NodePtr node ;
  int b, s ;
{
  int i ;
  buff[0] = '\0' ;
  curs = indent+6 ;
  for ( i = b ; i <= s ; i++ )
  {
    format_rhs_base( CHILD( node , i ), 0 ) ;
  }
  outbuf(buff) ;
}

format_rhs( node )
  NodePtr node ;
{
  buff[0] = '\0' ;
  curs = indent+6 ;
  format_rhs_base( node, 1 ) ;
  outbuf(buff) ;
}

format_rhs_nonl( node )
  NodePtr node ;
{
  buff[0] = '\0' ;
  curs = indent+6 ;
  format_rhs_base( node, 0 ) ;
  outbuf(buff) ;
}

format_rhs_base( node, nl )
  NodePtr node ;
  int nl ;
{
  int childmask[maxChildren] ;
  int i ;
  int first ;
  char buf2[256] ;
  char nstr[10] ;

  if ( node == NULL ) return  ;

  for (i = 0 ; i < maxChildren ; i++ ) childmask[i] = 1 ;

  first = 1 ;
  walk_depth( node, mark_clear, 1 ) ;
  more_to_go = 1 ;
  respect = 1 ;
  while ( more_to_go )
  {
    if ( first )
    {
      first = 0 ;
    }
    else
    {
      if ( sv_L )
      {
	sprintf(nstr,"%4d\t",lineno ) ;
	strcat(buff,nstr) ;
      }
      strcat(buff,"     +") ;
      strncat(buff, spaces, indent ) ;
      curs = indent+6 ;
    }
    continue_traversal = 1 ;
    walk_depth( node, as_far_as_fits, -1 ) ;
    more_to_go = ( ! continue_traversal ) ;
    strcat(buff,"\n") ;
  }
  if ( !nl ) strip_nl(buff) ;
}


cap_token( k, s )
  int k ;
  char *s ;
{
  char *p ;
  if ( sw_AU && k != TSTRING ) k = TAUTOMATIC ;
  if ( sw_AL && k != TSTRING ) k = TNAME ;
  switch (k)
  {
    case TARITHIF :
    case TAUTOMATIC :
    case TBACKSPACE :
    case TBLOCK :
    case TBYTE :
    case TCALL :
    case TCHARACTER :
    case TCLOSE :
    case TCOMMON :
    case TCOMPGOTO :
    case TCOMPLEX :
    case TCONTINUE :
    case TDATA :
    case TDCOMPLEX :
    case TDIMENSION :
    case TDO :
    case TDOUBLE :
    case TDOWHILE :
    case TELSE :
    case TELSEIF :
    case TEMPTYPAR :
    case TENCODE :
    case TDECODE :
    case TEND :
    case TENDDO :
    case TENDFILE :
    case TENDIF :
    case TENDMAP :
    case TENDSTRUCT :
    case TENDUNION :
    case TENTRY :
    case TEXTERNAL :
    case TFALSE :
    case TFORMAT :
    case TFUNCTION :
    case TGOTO :
    case TIMPLICIT :
    case TINCLUDE :
    case TINQUIRE :
    case TINTEGER :
    case TINTRINSIC :
    case TLOGICAL :
    case TLOGIF :
    case TNAMELIST :
    case TOPEN :
    case TPARAM :
    case TPAUSE :
    case TPOINTER :
    case TPRINT :
    case TPROGRAM :
    case TPUNCH :
    case TREAD :
    case TREAL :
    case TRECORD :
    case TRETURN :
    case TREWIND :
    case TSAVE :
    case TSTATIC :
    case TSTOP :
    case TSTRUCT :
    case TSUBROUTINE :
    case TTHEN :
    case TTRUE :
    case TUNION :
    case TWHILE :
    case TWRITE :
      for ( p = s ; *p ; p++ )
      {
        if ( *p >= 'a' && *p <= 'z' ) *p = *p - 'a' + 'A' ;
      }
      return(1) ;
      break ;
    default :
      return(0) ;
  }
  return(0) ;
}

space_after_token( k )
  int k ;
{
  switch (k)
  {
    case TA :					return(0) ; break ;
    case TAND :					return(0) ; break ;
    case TANGLEEXP :				return(0) ; break ;
    case TARITHIF :				return(0) ; break ;
    case TASGOTO :				return(0) ; break ;
    case TASSIGN :				return(0) ; break ;
    case TAUTOMATIC :		return(1) ; break ;
    case TBACKSPACE :		return(1) ; break ;
    case TBITCON :		return(1) ; break ;
    case TBLANK :		return(1) ; break ;
    case TBLOCK :		return(1) ; break ;
    case TBN :					return(0) ; break ;
    case TBYTE :		return(1) ; break ;
    case TBZ :					return(0) ; break ;
    case TCALL :		return(1) ; break ;
    case TCHARACTER :		return(1) ; break ;
    case TCLOSE :		return(1) ; break ;
    case TCOLON :				return(0) ; break ;
    case TCOMMA :				return(0) ; break ;
    case TCOMMENT :				return(0) ; break ;
    case TCOMMON :		return(1) ; break ;
    case TCOMPGOTO :		return(1) ; break ;
    case TCOMPLEX :		return(1) ; break ;
    case TCONCAT :				return(0) ; break ;
    case TCONTCARD :				return(0) ; break ;
    case TCONTINUE :		return(1) ; break ;
    case TCURRENCY :				return(0) ; break ;
    case TD :					return(0) ; break ;
    case TDATA :		return(1) ; break ;
    case TDCOMPLEX :		return(1) ; break ;
    case TDCON :				return(0) ; break ;
    case TDIMENSION :		return(1) ; break ;
    case TDO :			return(1) ; break ;
    case TDOT :					return(0) ; break ;
    case TDOUBLE :		return(1) ; break ;
    case TDOWHILE :		return(1) ; break ;
    case TE :					return(0) ; break ;
    case TELSE :				return(0) ; break ;
    case TELSEIF :				return(0) ; break ;
    case TEMPTYPAR :				return(0) ; break ;
    case TENCODE :		return(1) ; break ;
    case TDECODE :		return(1) ; break ;
    case TEND :			return(1) ; break ;
    case TENDDO :		return(1) ; break ;
    case TENDFILE :		return(1) ; break ;
    case TENDIF :				return(0) ; break ;
    case TENDMAP :		return(1) ; break ;
    case TENDSTRUCT :		return(1) ; break ;
    case TENDUNION :		return(1) ; break ;
    case TENTRY :		return(1) ; break ;
    case TEOF :					return(0) ; break ;
    case TEOS :					return(0) ; break ;
    case TEQ :					return(0) ; break ;
    case TEQUALS :				return(0) ; break ;
    case TEQUIV :				return(0) ; break ;
    case TEQV :					return(0) ; break ;
    case TEXTERNAL :		return(1) ; break ;
    case TF :					return(0) ; break ;
    case TFALSE :				return(0) ; break ;
    case TFIELD :				return(0) ; break ;
    case TFMTA :				return(0) ; break ;
    case TFMTB :				return(0) ; break ;
    case TFMTD :				return(0) ; break ;
    case TFMTE :				return(0) ; break ;
    case TFMTF :				return(0) ; break ;
    case TFMTG :				return(0) ; break ;
    case TFMTH :				return(0) ; break ;
    case TFMTI :				return(0) ; break ;
    case TFMTL :				return(0) ; break ;
    case TFMTP :				return(0) ; break ;
    case TFMTS :				return(0) ; break ;
    case TFMTT :				return(0) ; break ;
    case TFMTX :				return(0) ; break ;
    case TFORMAT :		return(1) ; break ;
    case TFUNCTION :		return(1) ; break ;
    case TG :					return(0) ; break ;
    case TGE :					return(0) ; break ;
    case TGOTO :		return(1) ; break ;
    case TGT :					return(0) ; break ;
    case THEXCON :				return(0) ; break ;
    case THOLLERITH :				return(0) ; break ;
    case TI :					return(0) ; break ;
    case TICON :				return(0) ; break ;
    case TIF :					return(0) ; break ;
    case TIMPLICIT :		return(1) ; break ;
    case TINCLUDE :		return(1) ; break ;
    case TINQUIRE :		return(1) ; break ;
    case TINTEGER :		return(1) ; break ;
    case TINTRINSIC :		return(1) ; break ;
    case TL :					return(0) ; break ;
    case TLABEL :				return(0) ; break ;
    case TLANGLE :				return(0) ; break ;
    case TLE :					return(0) ; break ;
    case TLET :					return(0) ; break ;
    case TLETTER :				return(0) ; break ;
    case TLOGICAL :		return(1) ; break ;
    case TLOGIF :				return(0) ; break ;
    case TLPAR :				return(0) ; break ;
    case TLT :					return(0) ; break ;
    case TMAP :					return(0) ; break ;
    case TMINUS :				return(0) ; break ;
    case TNAME :				return(0) ; break ;
    case TNAMEEQ :				return(0) ; break ;
    case TNAMELIST :		return(1) ; break ;
    case TNE :					return(0) ; break ;
    case TNEQV :				return(0) ; break ;
    case TNONE :		return(1) ; break ;
    case TNOT :					return(0) ; break ;
    case TO :					return(0) ; break ;
    case TOCTCON :				return(0) ; break ;
    case TOPEN :		return(1) ; break ;
    case TOR :					return(0) ; break ;
    case TP :					return(0) ; break ;
    case TPARAM :		return(1) ; break ;
    case TPAUSE :				return(0) ; break ;
    case TPERCENT :				return(0) ; break ;
    case TPLUS :				return(0) ; break ;
    case TPOINTER :		return(1) ; break ;
    case TPOWER :				return(0) ; break ;
    case TPRINT :				return(0) ; break ;
    case TPROGRAM :		return(1) ; break ;
    case TPUNCH :		return(1) ; break ;
    case TQ :					return(0) ; break ;
    case TRANGLE :				return(0) ; break ;
    case TRCON :				return(0) ; break ;
    case TREAD :		return(1) ; break ;
    case TREAL :	        return(1) ; break ;
    case TRECORD :		return(1) ; break ;
    case TRETURN :		return(1) ; break ;
    case TREWIND :		return(1) ; break ;
    case TRPAR :				return(0) ; break ;
    case TS :					return(0) ; break ;
    case TSAVE :		return(1) ; break ;
    case TSCALE :				return(0) ; break ;
    case TSLASH :				return(0) ; break ;
    case TSOUGHTLABEL :				return(0) ; break ;
    case TSP :					return(0) ; break ;
    case TSS :					return(0) ; break ;
    case TSTAR :				return(0) ; break ;
    case TSTATIC :		return(1) ; break ;
    case TSTOP :		return(1) ; break ;
    case TSTRING :				return(0) ; break ;
    case TSTRUCT :		return(1) ; break ;
    case TSUBROUTINE :		return(1) ; break ;
    case TT :					return(0) ; break ;
    case TTHEN :		return(1) ; break ;
    case TTL :					return(0) ; break ;
    case TTO :					return(0) ; break ;
    case TTR :					return(0) ; break ;
    case TTRUE :				return(0) ; break ;
    case TUNDEFINED :				return(0) ; break ;
    case TUNION :				return(0) ; break ;
    case TUNKNOWN :				return(0) ; break ;
    case TWHILE :		return(1) ; break ;
    case TWRITE :		return(1) ; break ;
    case TX :					return(0) ; break ;
    case TXOR :					return(0) ; break ;
    case TZ :					return(0) ; break ;
    default : 					return(0) ; break ;
  }
  return(0) ;
}

static char inp_line[4096] ;

getfrombuff( s )
  char * s ;
{
  char * p ;
  int retval ;
  int n ;
  n = 0 ;
  for ( p = &(buff[cutcurs]) ; *p && *p != '\n' ; p++ )
  {
    s[n++] = *p ;
  }
  s[n] = *p ;
  if ( *p == '\n' )
    s[++n] = '\0' ;
  cutcurs += n ;
#if 0
fprintf(stderr,"getfrombuff returns n=%d cutcur=%d\n|%s|\n>%s<\n",n,cutcurs,inp_line,&(buff[cutcurs])) ;
#endif
  return(n) ;
}

#define INIT 0
#define ENDED 1

col_cut()
{
    int column ;
    int lineterm ;
    int colp ;

    while( getfrombuff( inp_line ) != NULL )
    {
	strip_nl(inp_line) ;
	if ( inp_line[0] == 'c' || inp_line[0] == 'C' )
	{
	    outbuf(inp_line) ;
	    continue ;
	}
	remove_bang(inp_line) ;
        lineterm = INIT ;
	colp = 0 ;
	column = 1 ;
again:
	for ( ; column <= 72 ; column++, colp++ )
	{
	    if ( inp_line[colp] == '\t' )
	    {
		fprintf(stderr,"can't have tabs: use expand first.\n") ;
		exit(2) ;
	    }
	    else if ( inp_line[colp] != '\0' )
	    {
	        putc(inp_line[colp],outfile) ;
	    }
	    else
	    {
		lineterm = ENDED ;
		break ;
	    }
	}
	fprintf(outfile,"\n") ;
	if ( lineterm != ENDED )
	{
	    outbuf("     +") ;
	    column = 6 ;
	    goto again ;
	}
    }
    return(0) ;
}

remove_bang(s)
char * s ;
{
    int inquote ;
    char * p ;

    inquote = 0 ;

    for ( p = s ; *p ; p++ )
    {
	if ( *p == '\'' )  /* naive assumption that only ' are used */
	{
	    inquote = (inquote + 1) % 2 ;
	}
	else if ( *p == '!' &&  inquote == 0 )
	{
	    *p = '\0' ;	  /* here's one! get rid of it!!!!! */
	    return ;
	}
    }
    return ;
}

do_header( node )
  NodePtr node ;
{
  NodePtr firstnode, secondnode, p ;
  if (header[0] == '\0') return ;
  if ( node == NULL ) return ;
  if (( firstnode = CHILD(node,0)) == NULL ) return ;
  if (( secondnode = SIBLING(firstnode)) == NULL ) return ;
  p = secondnode ;
  if (MINORKIND(secondnode) == implicitK )
    if (( p = SIBLING(secondnode)) == NULL ) return ;
#if 0
  headerhere = p ;   /* set a marker so that when the traversal gets
                        to this point it knows to output the headerstring.
                        (i was too lazy to insert a node into the tree) */
#else
/*
   Laziness comes to bite me as always -- the way it was done before
   made it so only the last routine in a multi-subroutine file would
   get a header.  And everyone's trying to get ahead.  Er...
   So, instead, I'll use the user defined field in the node.
   This *should* be okay, because the parser library automatically
   zeros storage when it allocates nodes (so there's no chance that
   the arbitrary value of HEADERHERE would come up at random.
   But I am still trusting the library to zero storage because I'm
   too lazy to zero it explicitly myself.  Always fixing the symptoms...
*/
   p->user_int = HEADERHERE ;
#endif
}
