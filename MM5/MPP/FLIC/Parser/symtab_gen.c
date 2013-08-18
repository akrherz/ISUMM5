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

/*  symtab.c 

Symbol Table Handler -- Generic

The routine symget() returns a pointer to a C structure matching a
given lexeme.  If the lexeme does not already exist in the symbol
table, the routine will create a new symbol structure, store it, and
then return a pointer to the newly created structure.

It is up to the calling module to declare the symbol structure as
well as several routines for manipulating the symbol structure.  The
routines are passed to symget as pointers.

	name	   type		description

	newnode()   *char	returns a pointer to a symbol structure.

	nodename()  **char	retrieves the lexeme name from a symbol
				structure, returned as a pointer to a 
				character array.
	
	nodenext()  **char	retrieves pointer to the next field of
				the symbol structure (the next field
				is itself a pointer to a symbol structure)

For a sample main or calling program see the end of this file.

****
  REVISED 2-19-90.  Added code to make hashtable interchangible.
	new routine: create_ht()	creates new hashtable
	rev routine: symget()		added parameter to pass hash table
*/

#include <stdio.h>

#define HASHSIZE 1024

/*  commented out 2-29-90
static char * symtab[HASHSIZE] ;	
*/

void * malloc(), * myMalloc() ;

char * symget(name,newnode,nodename,nodenext,symtab,flag)
char *name ;
char *(*newnode)(), **(*nodename)(), **(*nodenext)() ;
char *symtab[] ;
int flag ;		/* 1 is create if not there, 0 return NULL if not there */
{
    int index ; 
    int found ;
    register char *s ;
    register char *t ;
    char **x ;
    char *p ;

    index = hash( name ) ;
    p = symtab[index] ;
    found = 0 ;

    while (p) {
        s = name ;
	t = *(*nodename)(p) ;
	while (*s && *t && *s == *t ) {
	    s++ ;
	    t++ ;
	}
	if (!*s && !*t) {
	    found = 1 ;
	    break ;
	}
	p = *(*nodenext)(p) ;
    }

    if (!found ) {
      if (flag ) {
        p = (*newnode)() ;
        x =  (*nodename)(p) ;
        *x = (char *) malloc(strlen(name)+1) ;
        strcpy(*x,name) ;
        x =  (*nodenext)(p) ;
        *x = symtab[index] ;
        symtab[index] = p ;
      } else {
        return(NULL) ;
      }
    }

    return(p) ;
}

hash(name)
char * name ;
{
    register int result = 0  ;
    register char * p = name ;

    while (*p)
	result = 3*result + (int)*p++ ;
    
    result = result % HASHSIZE ;
    while (result < 0)
	result = result + HASHSIZE ;
    return(result) ;
}


/* added 2-19-90, attaches a new hash table to pointer  */

create_ht( p )
char *** p ; 
{
    *p = (char **) myMalloc( HASHSIZE * sizeof( char * ) ) ;
}


/* added 4-15-92.

This is a generic routine that, given a hash table pointer,
will traverse the hash table and apply a caller supplied
function to each entry

*/

sym_traverse( ht, nodenext, f )
char *ht[] ;
char **(*nodenext)() ;
void (*f)() ;
{
    char * p, **x ;
    int i ;
    for ( i = 0 ; i < HASHSIZE ; i++ )
    {
	if ( ( p = ht[i] ) != NULL )
	{
	    while ( p )
	    {
		 (*f)(p) ;
		 x = (*nodenext)(p) ;
		 p = *x ;
	    }
	}
    }
}

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/

#ifdef COMMENTOUTSAMPLE
/* sample_main.c

    sample main program for symget() in the file symtab.c

*/

#include <stdio.h>

struct symnode {
    char * name ;
    struct symnode *next ;
} ;

extern struct symnode * symget() ;

struct symnode *
newnode()
{
    struct symnode * malloc() ;
    return( malloc( sizeof( struct symnode ) ) ) ;
}

char **
nodename(p)
struct symnode *p ;
{
    char ** x ;
    x = &(p->name) ;
    return( x ) ;
}

struct symnode **
nodenext(p)
struct symnode *p ;
{
    struct symnode **x ;
    x = &(p->next) ;
    return( x ) ;
}

#endif

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/

