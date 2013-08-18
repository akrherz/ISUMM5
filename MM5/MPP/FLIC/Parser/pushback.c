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

/* pushback.c

   Routines to replace stdio character and string I/O routines with
   those that will allow character and string pushback.
   Uses a File structure which consists of a pointer to a
   FILE structure in stdio.h and a pointer to a stack structure as
   defined in pushback.h.

*/

#include <stdio.h>
#include "pushback.h"

File *
pb_fopen( s, m )
char *s, *m ;
{
    FILE * fp1 ;
    File * fp ;
    void * myMalloc() ;
    
    if (strcmp(s,"stdin")) {  /* i.e. s != stdin */
        if ((fp1 = fopen( s, m ) ) == NULL ) {
            fprintf(stderr,"pb_fopen: cannot open %s with mode %s\n",s,m) ;
            return (NULL) ;
        }
    } else {   /* s is stdin */
	fp1 = stdin ;
    }

    if ((fp = (File *) myMalloc(sizeof(File))) == NULL) {
	fprintf(stderr,"pb_fopen: cannot create File structure\n") ;
        return(NULL) ;
    }
    fp->fp = fp1 ;
    if ((fp->sid = 
         (struct File_Stack *) myMalloc(sizeof(struct File_Stack))) == NULL) {
	fprintf(stderr,"pb_fopen: cannot create pushback stack struct\n") ;
        return(NULL) ;
    }
    fp->sid->sp = 0 ;  /* initialize pushback stack pointer */
    return(fp) ;
}

pb_fclose( fp )
File * fp ;
{
    if (fp) {fclose( fp->fp ) ; return(!EOF) ;} 
    else return(EOF) ;
}

int
pb_getc( fp )
File * fp ;
{
    int popc() ;
    if ( emptystack( fp->sid ) ) 
	return( getc( fp->fp ) ) ;
    else 
	return( popc( fp->sid ) ) ;
}

pb_ungetc( c, fp )
char c ;
File * fp ;
{
    return( pushc( c, fp->sid ) ) ;
}

char *
pb_fgets( s, n, fp )
char *s ;
int n ;
File * fp ;
{
    register int i ;
    register int c ;

    for (i=0; i<n-1 ; i++) {
        if ((c=pb_getc(fp))==EOF) {
	    s[i] = '\0' ;
	    return(NULL) ;
	} else 
	if (c == '\n') {
	    s[i] = c ;
	    s[i+1] = '\0' ;
	    return(s) ;
	} else {
	    s[i] = c ;
	}
    }
}

pb_unfgets( s, n, fp )
char * s ;
int n ;
File * fp ;
{
    register char * p ;
    register int i = 0;
    p = s ;
    while ((*p != '\0') && (i++ < n)) p++ ;
    while (p > s) pb_ungetc(*--p,fp) ;
    return(0) ;
}

int
popc(sid)
struct File_Stack * sid ;
{
    if (sid->sp > 0) return( sid->stack[--(sid->sp)] ) ;
    else return(EOF) ;
}

int
pushc(c, sid)
char c ;
struct File_Stack * sid ;
{
    if (sid->sp < PB_STACKSIZE) return( sid->stack[(sid->sp)++]=c ) ;
    else return(EOF) ;
}

int
emptystack(sid)
struct File_Stack * sid ;
{
    return( (sid->sp > 0)?0:1 ) ;
}

