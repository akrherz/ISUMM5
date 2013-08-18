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

#include <stdio.h>
#include "pushback.h"
#include "node.h"

/*#include "n32.h"*/

extern NodePtr Abstract_Syntax_Tree ;

char * infilename ;
File * infile ;
char * outfilename ;
FILE * outfile ;
char * errfilename ;
FILE * errfile ;
char * thiscommand ;

extern int yydebug ;
extern int partrace ;

/* list of option switches here */

int sw_type = 1 ;
int sw_printtree = 0 ;
int sw_debug = 0 ;
int sw_back = 1 ;
extern int sw_ansi ;  /* defined in intrinsics.c, 4-27-90  */

main( argc, argv, env )
int argc ;
char *argv[], *env[] ;
{
    char temp[256] ;
    int code ;
    int  save_argc = argc ;
    char **save_argv = argv ;
    char **save_env  = env ;

    /* read through and process options, setting switches as appropriate.
       as written, the last non-hyphenated token on the command line is taken 
       to be the input file name 
    */
    sw_ansi = 0 ;

    thiscommand = *argv ;
    argv++ ;
    while (*argv) {
	if (*argv[0] == '-') { 	/* an option */
	    if (!strcmp(*argv,"-tree")) {
		sw_printtree = 1 ;
	    } else 
	    if (!strcmp(*argv,"-notree")) {
		sw_printtree = 0 ;
	    } else 
	    if (!strcmp(*argv,"-parsetrace")) {
		sw_debug = 1 ;
	    } else 
	    if (!strcmp(*argv,"-notype")) {
		sw_type = 0 ;
	    } else 
	    if (!strcmp(*argv,"-noback")) {
		sw_back = 0 ;
	    } else 
	    if (!strcmp(*argv,"-ansi")) {
		sw_ansi = 1 ;
	    } else 
	    if (!strcmp(*argv,"-o")) {
		outfilename = *++argv ;
	    } else
            if (!strcmp(*argv,"-h")) {
                printhelp(thiscommand) ;
                exit(0) ;
            }
	} else {
	    infilename = *argv ;
	}
	argv++ ;
    }

    errfile = stderr ;

    if (infilename != NULL) {
        if (( infile = pb_fopen( infilename, "r" )) == NULL )  {
	    /* pb prints its own error messag to stdout */
            exit(1) ;
        }
    } else {
	if (( infile = pb_fopen( "stdin", "r" )) == NULL ) {
	    /* pb prints its own error messag to stdout */
            exit(1) ;
        }
    }

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
/* 4-17-92.  If the input is from a file, check to be sure that
   the input came from n32.scanner.  If it didn't then just pass
   control directly to the backend.  */
    if ( test_input( infile ) )
    {
#endif

    init_sym() ;	/* initialize symbol table package */
    if (sw_debug) partrace = sw_debug ;
    if ((code = parser()) != 0) {
	fprintf(stderr,"Parser Fails\n") ;
	exit( code ) ;
    }

    fix_backptrs() ;  /* added 5-29-90, to fix nodeParent and
		       back pointers in sibling lists (longSibling) */
    if (sw_type)      typetree( Abstract_Syntax_Tree ) ;
    if (sw_printtree) printtree( outfile, Abstract_Syntax_Tree ) ;

#if 0 
    } /* end of input test */
#endif

    if (sw_back)      code = user_backend(save_argc, save_argv, save_env) ;

    exit( code ) ;
}

#if 0
static char code[] = "n32.scanner token output" ;

/* added 4/17/92 */
int
test_input( fp )
File * fp ;
{
    char tmp[256] ;

    if ( pb_fgets( tmp, 255, fp ) == NULL ) return(0) ;

    if (!strncmp( tmp, code, strlen(code)) )
    {
	return(1) ;	/* code string found.  it is from scanner */
    }
    else
    {
	if ( fp->fp == stdin )
	{
	    pb_unfgets( tmp, 255, fp ) ;
	}
	else
	{
	    pb_fclose( fp ) ;
	}
	return(0) ;     /* code string not found.  not from scanner */
    }
}
#endif
