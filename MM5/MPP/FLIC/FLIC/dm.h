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
#ifndef DM_H

#define DM_H
#ifndef MAIN_ROUTINE
# define GLOBAL extern
#else
# define GLOBAL
#endif

#include "sym.h"

#define MAXDIMSPECS 500 

#define DECLARATIONS 1
#define EXECUTABLE 2

/* this is the maximum number of nodes that
   depends on is capable of returning.  Each node
   might be a separate term in a boolean expression */
#define DEPENDS_MAXRET 10

#define ODIM 0
#define MDIM 1
#define NDIM 2

/* an unlikely value -- plus the symbol table entries
   this is used in are supposed to be zeroed out when they're
   generated. */
#define MARKED_VAL 9918817


typedef struct array {
  struct array * next ;
  sym_nodeptr SYM ;
  char INDEX[7][20] ;
  unsigned char Index_IS[7] ;
  NodePtr Ref ;
  NodePtr Index[7] ;
  int ndim ;
  int iflev ;
} array_t ;

typedef struct loop {
  struct loop * next ;
  char LI[64] ;		/* loop index variable */
  unsigned char LI_IS ; /* dimensionality of loop */
  array_t * ARLIST ;	/* list of array references in the loop */
  NodePtr loop ;        /* pointer to this loop statement */
  int indentation ;
} loop_t ;

#define HEADERHERE 9844637

GLOBAL loop_t * LOOPLIST, * LOOPHEAD ;
GLOBAL char header[8092] ;      /* buffer for header lines */
GLOBAL NodePtr headerhere ;     /* buffer for header lines */
GLOBAL char modulename[256] ;   /* name of current routine */
GLOBAL char modulekind[256] ;   /* name of current routine */
GLOBAL char thiscom[256] ;
GLOBAL char toolname[256] ;
GLOBAL char *dimtab[MAXDIMSPECS] ;
GLOBAL char mdimstr[8192] ;
GLOBAL char ndimstr[8192] ;
GLOBAL char sw_IN_M_LOOP[8192] ;
GLOBAL char sw_IN_N_LOOP[8192] ;
GLOBAL int  sw_allloops ;
GLOBAL int  sw_collapse_loops_m ;
GLOBAL int  sw_change_stops ;
GLOBAL char sw_change_stops_string[1024] ;
GLOBAL int  sw_collapse_loops_n ;
GLOBAL int  sw_collapse_m ;
GLOBAL int  sw_collapse_n ;
GLOBAL int  sw_tag_enddos ;
GLOBAL int  sw_noconvert ;
GLOBAL int  sw_quiet ;
GLOBAL char dirfilename[512] ;
GLOBAL int cursor_dimtab ;
GLOBAL int part ;		/* DECLARATIONS or EXECUTABLE */
GLOBAL int loop_level ;
GLOBAL int tvarcnt ;
GLOBAL NodePtr first_statement,	/* 970225 */
	       first_e,		/* Markers in program: first exec statement */
	       last_d ;		/* and last non-statfun decl statement */
GLOBAL NodePtr thisif ;		/* pointer to if statement above */
GLOBAL int iflev ;		/* if nesting level */
GLOBAL char *in_mvar ;		/* name of external loop variable */
GLOBAL char *in_nvar ;		/* name of external loop variable */

GLOBAL int  sw_write_db ;
GLOBAL int  sw_addargs ;
GLOBAL int  sw_addgenericargs ;
GLOBAL int  statement_line_number ;
GLOBAL int  last_statement_line_number ;
GLOBAL FILE * db_file ;

/* routine declarations */
int new_module( ) ;
int new_statement( ) ;
int mark_actual_arguments( ) ;
int handle_refs( ) ;
int collapse_idrefs( ) ;
int li_loc2glob( ) ;
NodePtr last_def( ) ;
NodePtr last_def1( ) ;
NodePtr copy_tok_node( ) ;

#endif

