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
#ifndef TWST_H
#define TWST_H

#ifdef TWST_MAIN
  int sw_N = 2 ;
  int sw_V = 0 ;		/* remove all array indices */
  int sw_E = 0 ;		/* only show executable part of program */
  int sw_L = 0 ;		/* preprend source line #s */
  int sw_S = 0 ;		/* remove indices from arrays */
  int sw_RF = 1 ;		/* format rhs's of assigns */
  int sw_AU = 1 ;		/* all upper case output */
  int sw_AL = 0 ;		/* all lower case output */
  int sw_UNPOINT = 0 ;		/* change pointer declarations to common */
  int sw_edit = 1 ;
  int there_was_output = 0 ;
  FILE * tempfile ;
#else
  extern int sw_N ;
  extern int sw_V ;
  extern int sw_E ;
  extern int sw_L ;
  extern int sw_S ;
  extern int sw_RF ;
  extern int sw_AU ;
  extern int sw_AL ;
  extern int sw_UNPOINT ;
  extern int sw_edit ;
  extern int there_was_output ;
  extern FILE * tempfile ;
#endif


#endif /* put nothing after this line */
