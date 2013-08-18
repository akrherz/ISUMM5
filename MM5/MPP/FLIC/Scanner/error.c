/* error.c SCCS(4.1 91/02/18 15:42:54) */

#include <stdio.h>
#include "h.h"

comp_err(s)
char * s ;
{
    fprintf(errfile,"Error: compiler error in %s\n",s) ;
    exit(1) ;
}

lexerr(s)
char * s ;
{
    fprintf(errfile,"Error: scanner: %s\n",s) ;
}

syn_err(s)
char * s ;
{
    fprintf(errfile,"Error: parser: %s\n",s) ;
}
