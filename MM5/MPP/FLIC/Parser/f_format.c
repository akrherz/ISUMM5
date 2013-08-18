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
/* f_format.c SCCS(4.1 91/02/18 15:39:36) */

/* f_format.c

   routines that go with grammar rules in f.format

*/

#include <stdio.h>
#include "node.h"
#include "f_y.h"

NodePtr
make_nonreal_spec( A, B )
NodePtr	A,	/* integer repeat spec (maybe null) */
	B ;	/* spec node */
{
    return( two_kids( newDefNode( nonrealspecK ), A, B )  ) ;
}

NodePtr
make_real_spec( A, B, C, D )
NodePtr	A,	/* scale spec (maybe null) */
	B,	/* scale keyword (maybe null) */
	C,	/* integer repeat spec (maybe null) */
	D ;	/* spec node */
{
    return( four_kids( newDefNode( realspecK ), A, B, C, D )  ) ;
}

NodePtr
make_ispec_node( A, B, C, D )
NodePtr A,	/* token I */
	B,	/* integer (field width) */
	C,	/* dot (maybe null) */
	D ;	/* integer (min field width -- maybe null) */
{
    return( four_kids( newDefNode( ispecK ), A, B, C, D )  ) ;
}

NodePtr
make_lspec_node( A, B )
NodePtr A,	/* token L */
	B ;	/* integer width  */
{
    return( two_kids( newDefNode( lspecK ), A, B )  ) ;
}

NodePtr
make_aspec_node( A, B )
NodePtr A, 	/* token A */
	B ;	/* width (maybe null) */
{
    return( two_kids( newDefNode( aspecK ), A, B )  ) ;
}

NodePtr
make_fspec_node( A, B, C, D )
NodePtr A,	/* F keyword */
	B,	/* integer field width */
	C,	/* dot */
	D ;	/* integer fractional width */
{
    return( seven_kids( newDefNode(fspecK),NULL,NULL,NULL,A,B,C,D)  ) ;
}

NodePtr
make_dspec_node( A, B, C, D )
NodePtr A,	/* D keyword */
	B,	/* integer field width */
	C,	/* dot */
	D ;	/* integer fractional width */
{
    return( seven_kids( newDefNode(dspecK),NULL,NULL,NULL,
	A,B,C,D)  ) ;
}

NodePtr
make_espec_node( A, B, C, D, E, F )
NodePtr A,	/* E keyword */
	B,	/* integer field width */
	C,	/* dot */
	D,	/* integer fractional width */
	E,	/* E keyword (maybe null) */
	F ;	/* integer exponent width (maybe null) */
{
    return( nine_kids( newDefNode(especK),NULL,NULL,NULL,
	A,B,C,D,E,F )  ) ;
}

NodePtr
make_gspec_node( A, B, C, D, E, F )
NodePtr A,	/* G keyword */
	B,	/* integer field width */
	C,	/* dot */
	D,	/* integer fractional width */
	E,	/* E keyword (maybe null) */
	F ;	/* integer exponent width (maybe null) */
{
    return( nine_kids( newDefNode(gspecK),NULL,NULL,NULL,
	A,B,C,D,E,F )  ) ;
}

NodePtr
make_tspec_node( A, B )
NodePtr A,	/* T, TL, or TR keyword */
	B ;	/* integer tab count */
{
    return( two_kids( newDefNode(tspecK), A, B )  ) ;
}

NodePtr
make_xspec_node( A, B )
NodePtr A,	/* X keyword */
	B ;	/* integer tab count */
{
    return( two_kids( newDefNode(xspecK), A, B )  ) ;
}


