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

/* f_executables.h */

NodePtr make_altreturn_node() ;
NodePtr make_assignment_statement() ;
NodePtr make_ber_statement() ;
NodePtr make_call_arguments() ;
NodePtr make_call_statement() ;
NodePtr make_conditional_body() ;
NodePtr make_ctllist() ;
NodePtr make_do_body() ;
NodePtr make_do_label_ref() ;
NodePtr make_do_statement() ;
NodePtr make_doenddo_statement() ;
NodePtr make_dorange_node() ;
NodePtr make_dowhile_statement() ;
NodePtr make_enddo_statement() ;
NodePtr make_goto_statement() ;
NodePtr make_gotoassigned_statement() ;
NodePtr make_gotocomputed_statement() ;
NodePtr make_gotolabel_list() ;
NodePtr make_ifarith_statement() ;
NodePtr make_iflogical_statement() ;
NodePtr make_ifthenelse_statement() ;
NodePtr make_ifthenelseif_statement() ;
NodePtr make_implieddoin_node() ;
NodePtr make_implieddoout_node() ;
NodePtr make_input_list() ;
NodePtr make_iocontrol_node() ;
NodePtr make_iocontrolitem_node() ;
NodePtr make_label_assignment() ;
NodePtr make_label_ref() ;
NodePtr make_null_executable() ;
NodePtr make_oci_statement() ;
NodePtr make_pause_statement() ;
NodePtr make_print_statement() ;
NodePtr make_read_statement() ;
NodePtr make_return_statement() ;
NodePtr make_stop_statement() ;
NodePtr make_write_statement() ;
NodePtr makeBareExNode() ;
NodePtr make_dumb_node() ;
