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

/* macros.h SCCS(4.1 91/02/18 15:40:10) */

/* node handling macros to make life easier and a little
   more transparent for tool developers.  This is automatically
   included by including node.h.
*/

#define	CHILD(N,I)	(N->nodeChild[I])
#define SIBLING(N)	(N->nodeSibling)
#define PARENT(N)	(N->nodeParent)
#define ELDER(N)	(N->longSibling)
#define USERDEF(N)	(N->user_def)
#define USERINT(N)	(N->user_int)
#define	MAJORKIND(N)	(N->nodeKind)
#define	MINORKIND(N)	(N->u.Generic->GenericKind)
#define	EXFLD(N,F)	(N->u.Ex->F)
#define	DEFFLD(N,F)	(N->u.Def->F)
#define	TYPEFLD(N,F)	(N->u.Type->F)
	
#define	TOKSEQ(N)	(N->u.Token->field[FTOKSEQ])
#define	TOKFILE(N)	(N->u.Token->field[FFILE])
#define	TOKCODE(N)	(N->u.Token->field[FTOKCODE])
#define	ITOKCODE(N)	(atoi(N->u.Token->field[FTOKCODE]))
#define	TOKBEGIN(N)	(N->u.Token->field[FBEGINCARD])
#define	TOKCOLS(N)	(N->u.Token->field[FBEGINCOL])
#define	TOKEND(N)	(N->u.Token->field[FENDCARD])
#define	TOKCOLE(N)	(N->u.Token->field[FENDCOL])
#define	TOKDESCRIP(N)	(N->u.Token->field[FDESCRIP])
#define	TOKSTRING(N)	(N->u.Token->field[FTOKSTRING])
#define TOKSYMPTR(N)	(N->u.Token->symptr)

#define STATEMENT(N)	\
  (N==NULL?0:(CHILD(N,0)==NULL?0:((MINORKIND(CHILD(N,0))==TLABEL)|| \
   (MINORKIND(CHILD(N,0))==TSOUGHTLABEL)?1:0)))
