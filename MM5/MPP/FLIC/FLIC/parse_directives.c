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
#include <stdio.h>
#include "n32.h"
#include "sym.h"
#include "dm.h"

static int first = 1 ;
char * strip_lead(), *strip_nl() ;

parse_directives( filename )
  char * filename ;
{
  FILE *fp ;
  char *p ;
  char instr[1024] ;

  if (( fp = fopen(filename,"r")) == NULL )
  {
    fprintf(stderr,"Cannot open %s for reading.\n",filename ) ;
    exit(33) ;
  }
  while ( fgets(instr,1024,fp) != NULL )
  {
    for (p=instr;*p;p++)
      if ( *p >= 'A' && *p <= 'Z' ) *p = *p - 'A' + 'a' ;
    if (strncmp( instr, "cflic", 5 )) continue ;
    /* below here, it's a directive */
    p = (char *)strip_lead(strip_nl(instr+5)) ;
    if (!strncmp(p,"m=",2)) {
      if (strlen(sw_IN_M_LOOP) > 0) strcat(sw_IN_M_LOOP,",") ;
      strcat(sw_IN_M_LOOP,p+2) ;
    }
    if (!strncmp(p,"n=",2)) {
      if (strlen(sw_IN_N_LOOP) > 0) strcat(sw_IN_N_LOOP,",") ;
      strcat(sw_IN_N_LOOP,p+2) ;
    }
    if (!strncmp(p,"mdim=",5)) {
      if (strlen(mdimstr) > 0) strcat(mdimstr,",") ;
      strcat(mdimstr,p+5) ;
    }
    if (!strncmp(p,"ndim=",5)) {
      if (strlen(ndimstr) > 0) strcat(ndimstr,",") ;
      strcat(ndimstr,p+5) ;
    }
  }

  fclose(fp) ;
}

