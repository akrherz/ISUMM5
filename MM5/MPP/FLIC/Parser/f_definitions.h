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

/* f_definitions.h */

NodePtr make_struct_statement() ;
NodePtr make_record_statement() ;
NodePtr make_union_statement() ;
NodePtr make_map_statement() ;
NodePtr make_blockdata_node() ;
NodePtr make_common_statement() ;
NodePtr make_more_common() ;
NodePtr make_namelist_statement() ;
NodePtr make_more_namelist() ;
NodePtr make_iddecl_node() ;
NodePtr make_fielddecl_node() ;
NodePtr make_id_decl_list() ;
NodePtr make_data_statement() ;
NodePtr make_datalistitem_node() ;
NodePtr make_datarep_node() ;
NodePtr make_didl_node() ;
NodePtr make_didlrange_node() ;
NodePtr make_dimension_node() ;
NodePtr make_subscript_node() ;
NodePtr make_subscriptitem() ;
NodePtr make_end_statement() ;
NodePtr make_entry_statement() ;
NodePtr make_equiv_statement()  ;
NodePtr make_pointer_statement()  ;
NodePtr make_equive_node() ;
NodePtr make_external_node() ;
NodePtr make_format_statement() ;
NodePtr make_format_node() ;
NodePtr make_dummy_args() ;
NodePtr make_implicit_statement() ;
NodePtr make_implspec_node() ;
NodePtr make_implrange_node() ;
NodePtr make_include_statement() ;
NodePtr make_intrinsic_node() ;
NodePtr make_parameter_node() ;
NodePtr make_paramitem_node() ;
NodePtr make_save_statement() ;
NodePtr make_save_item() ;
NodePtr make_type_statement() ;
NodePtr make_type_node() ;
NodePtr make_typeitem_node() ;
NodePtr make_length_spec() ;
NodePtr make_defparen_node() ;
NodePtr make_defbinop_node() ;
NodePtr make_defunop_node() ;
