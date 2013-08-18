/*
  col_cutter.c

  This is a utility for compiling MPMM on IBM RS/6000 workstations.
  It allows extended Fortran source lines (something other compilers give with a
  command line option).  Any makefile needing this utility should
  make it automatically by compiling this file.

*/

#include <stdio.h>

char line[4096] ;

#define INIT 0
#define ENDED 1

main()
{
    int column ;
    int lineterm ;
    int colp ;

    while( gets( line ) != NULL )
    {
	if ( line[0] == 'c' || line[0] == 'C' )
	{
	    printf("%s\n",line) ;
	    continue ;
	}
	remove_bang(line) ;
        lineterm = INIT ;
	colp = 0 ;
	column = 1 ;
again:
	for ( ; column <= 72 ; column++, colp++ )
	{
	    if ( line[colp] == '\t' )
	    {
		fprintf(stderr,"can't have tabs: use expand first.\n") ;
		exit(2) ;
	    }
	    else if ( line[colp] != '\0' )
	    {
		putchar(line[colp]) ;
	    }
	    else
	    {
		lineterm = ENDED ;
		break ;
	    }
	}
	printf("\n") ;
	if ( lineterm != ENDED )
	{
	    printf("     $") ;
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
