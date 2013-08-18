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
/* edface.c SCCS(4.1 91/02/18 15:40:13) */

/* ed interface routines.  

   These routines to output lines of ed commands from N32.

*/

#include <stdio.h>
#include "n32.h"

/* static FILE * OF = stdout ; */
static char delim = '\n' ;

edsetfile( outfile )
FILE * outfile ;
{
#if 0
    stdout = outfile ;
#endif
}

setdelim( d )
char d ;
{
    delim = d ;
}

/* edIdel
   Generate ed command to: 

   Delete line(s) whose range is specified by beginning
   and ending integers
*/
edIdel( beg, end )
int beg,	/* beginning line number */
    end ;	/* ending line number or 0 */
{
    if (beg < 1 || end < 0) {
	fprintf(errfile,"edIdel(): bad range spec beg=%d, end=%d\n",beg,end) ;
    } else 
    if (end != 0 && beg <= end) {
        fprintf(stdout,"%d,%dd\n",beg,end) ;
    } else
    if (end == 0) {
	fprintf(stdout,"%dd\n",beg) ;
    } else {
	fprintf(errfile,"edIdel(): bad range spec beg=%d, end=%d\n",beg,end) ;
    }
}

/* edAdel
   Generate ed command to:

   Delete line(s) whose range is specified by beginning
   and ending strings
*/

edAdel( beg, end )
char * beg,
     * end ;
{
    if (beg == NULL) {
	fprintf(errfile,"edAdel(): no beginning string\n") ;
    } else
    if (end != NULL) {
	fprintf(stdout,"/%s/,/%s/d\n",beg,end) ;
    } else
    if (end == NULL) {
	fprintf(stdout,"/%s/d",beg) ;
    } else {
	fprintf(errfile,"edAdel(): bad range spec beg=%s, end=%s\n",beg,end) ;
    }
}

edIins( at, str )
int at ;		/* line number of insertion */
char * str ;
{
    if (at < 1) {
	fprintf(errfile,"edIins: bad line number %d\n",at ) ;
    } else
    if (str == NULL) {
	fprintf(errfile,"edIins: no insertion string specified\n") ;
    } else {
	fprintf(stdout,"%di%c%s%c.\n",at,delim,str,delim) ;
    }
}

edAins( at, str )
char * at ;                /* line number of insertion */
char * str ;
{
    if (at == NULL ) {
        fprintf(errfile,"edIins: bad line spec\n" ) ;
    } else
    if (str == NULL) {
        fprintf(errfile,"edIins: no insertion string specified\n") ;
    } else {
        fprintf(stdout,"/%s/i%c%s%c.\n",at,delim,str,delim) ;
    }
}

edIapnd( at, str )
int at ;                /* line number of append */
char * str ;
{
    if (at < 1) {
        fprintf(errfile,"edIapnd: bad line number %d\n",at ) ;
    } else
    if (str == NULL) {
        fprintf(errfile,"edIapnd: no append string specified\n") ;
    } else {
        fprintf(stdout,"%da%c%s%c.\n",at,delim,str,delim) ;
    }
}

edAapnd( at, str )
char * at ;                /* line number of append */
char * str ;
{
    if (at == NULL ) {
        fprintf(errfile,"edIapnd: bad line spec\n" ) ;
    } else
    if (str == NULL) {
        fprintf(errfile,"edIapnd: no append string specified\n") ;
    } else {
        fprintf(stdout,"/%s/a%c%s%c.\n",at,delim,str,delim) ;
    }
}

edAchng( beg, end, str )
char * beg, * end ;
char * str ;
{
    if (beg == NULL) {
        fprintf(errfile,"edAchng(): no beginning string\n") ;
    } else
    if (end != NULL) {
        fprintf(stdout,"/%s/,/%s/c%c%s%c.\n",beg,end,delim,str,delim) ;
    } else
    if (end == NULL) {
        fprintf(stdout,"/%s/c%c%s%c.\n",beg,delim,str,delim) ;
    } else {
        fprintf(errfile,"edAchng(): bad range spec beg=%s,end=%s\n",beg,end) ;
    }
}

edIchng( beg, end, str )
int beg,        /* beginning line number */
    end ;       /* ending line number or 0 */
char * str ;
{
    if (beg < 1 || end < 0) {
        fprintf(errfile,"edIchng(): bad range spec beg=%d,end=%d\n",beg,end) ;
    } else
    if (end != 0 && beg <= end) {
        fprintf(stdout,"%d,%dc%c%s%c.\n",beg,end,delim,str,delim) ;
    } else
    if (end == 0) {
        fprintf(stdout,"%dc%c%s%c.\n",beg,delim,str,delim) ;
    } else {
        fprintf(errfile,"edIchng(): bad range spec beg=%d,end=%d\n",beg,end) ;
    }
}

edIsub( beg, end, str1, str2, glob )
int beg,
    end ;
char * str1, * str2, * glob ;
{
    if (str1 == NULL || str2 == NULL) {
        fprintf(errfile,"edIsub(): null string in str1 or str2\n") ;
    } else
    if (beg < 1 || end < 0) {
        fprintf(errfile,"edIsub(): bad range spec beg=%d,end=%d\n",beg,end) ;
    }
    if (end != 0 && beg <= end) {
        fprintf(stdout,"%d,%ds/%s/%s/%s\n",beg,end,str1,str2,glob) ;
    } else
    if (end == 0) {
        fprintf(stdout,"%ds/%s/%s/%s\n",beg,str1,str2,glob) ;
    } else {
        fprintf(errfile,"edIsub(): bad range spec beg=%d,end=%d\n",beg,end) ;
    }
}

edcommand( s )
char * s ;
{
    fprintf(stdout,"%s\n") ;
}
