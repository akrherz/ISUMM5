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
/*
    minor.c

    routine to print the AST produced by a parse

*/
#include <stdio.h>
#include "node.h"

char *
minorKind( node )
NodePtr node ;
{
    switch (node->nodeKind) {
        case defK : break ;
        case exK : break ;
        case typeK : break ;
        case tokenK : if( node->u.Token )
                        return(GETDESCRIP(node->u.Token)) ;
                      else
                        return("unknownK -- MAYBE BAD NODE") ;
                      break ;
        default : return("unknownK" ) ;
    }

    switch (node->u.Generic->GenericKind) {
  case altreturnK: return("altreturnK" ) ; break ; 
  case assignK: return("assignK" ) ; break ; 
  case assignlabelK: return("assignlabelK" ) ; break ; 
  case backspaceK: return("backspaceK" ) ; break ; 
  case binopK: return("binopK" ) ; break ; 
  case blockdataK: return("blockdataK" ) ; break ; 
  case callK: return("callK" ) ; break ; 
  case closeK: return("closeK" ) ; break ; 
  case commonK: return("commonK" ) ; break ; 
  case complexconstK: return("complexconstK" ) ; break ; 
  case continueK: return("continueK" ) ; break ; 
  case dataK: return("dataK" ) ; break ; 
  case datalistitemK: return("datalistitemK" ) ; break ; 
  case datarepK: return("datarepK" ) ; break ; 
  case didlK: return("didlK" ) ; break ; 
  case didlrangeK: return("didlrangeK" ) ; break ; 
  case dimensionK: return("dimensionK" ) ; break ; 
  case doK: return("doK" ) ; break ; 
  case doenddoK: return("doenddoK" ) ; break ; 
  case dorangeK: return("dorangeK" ) ; break ; 
  case dotK: return("dotK" ) ; break ; 
  case dowhileK: return("dowhileK" ) ; break ; 
  case dummyargsK: return("dummyargsK" ) ; break ; 
  case endK: return("endK" ) ; break ; 
  case enddoK: return("enddoK" ) ; break ; 
  case endfileK: return("endfileK" ) ; break ; 
  case entryK: return("entryK" ) ; break ; 
  case equivalenceK: return("equivalenceK" ) ; break ; 
  case equivenodeK: return("equivenodeK" ) ; break ; 
  case errorK: return("errorK" ) ; break ; 
  case externalK: return("externalK" ) ; break ; 
  case fielddeclK: return("fielddeclK" ) ; break ; 
  case fieldrefK: return("fieldrefK" ) ; break ; 
  case formatK: return("formatK" ) ; break ; 
  case functionK: return("functionK" ) ; break ; 
  case gotoK: return("gotoK" ) ; break ; 
  case gotoassignedK: return("gotoassignedK" ) ; break ; 
  case gotocomputedK: return("gotocomputedK" ) ; break ; 
  case iddeclK: return("iddeclK" ) ; break ; 
  case idrefK: return("idrefK" ) ; break ; 
  case ifarithK: return("ifarithK" ) ; break ; 
  case iflogicalK: return("iflogicalK" ) ; break ; 
  case ifthenelseK: return("ifthenelseK" ) ; break ; 
  case ifthenelseifK: return("ifthenelseifK" ) ; break ; 
  case implicitK: return("implicitK" ) ; break ; 
  case implieddoinK: return("implieddoinK" ) ; break ; 
  case implieddooutK: return("implieddooutK" ) ; break ; 
  case implrangeK: return("implrangeK" ) ; break ; 
  case implspecK: return("implspecK" ) ; break ; 
  case includeK: return("includeK" ) ; break ; 
  case intrinsicK: return("intrinsicK" ) ; break ; 
  case inquireK: return("inquireK" ) ; break ; 
  case iocontrolK: return("iocontrolK" ) ; break ; 
  case iocontrolitemK: return("iocontrolitemK" ) ; break ; 
  case lengthspecK: return("lengthspecK" ) ; break ; 
  case mainK: return("mainK" ) ; break ; 
  case mapK: return("mapK" ) ; break ; 
  case morecommonK: return("morecommonK" ) ; break ; 
  case morenamelistK: return("morenamelistK" ) ; break ; 
  case namelistK: return("namelistK" ) ; break ; 
  case nullK: return("nullK" ) ; break ; 
  case openK: return("openK" ) ; break ; 
  case parameterK: return("parameterK" ) ; break ; 
  case paramitemK: return("paramitemK" ) ; break ; 
  case parenK: return("parenK" ) ; break ; 
  case pauseK: return("pauseK" ) ; break ; 
  case pointerK: return("pointerK" ) ; break ; 
  case printK: return("printK" ) ; break ; 
  case programK: return("programK" ) ; break ; 
  case readK: return("readK" ) ; break ; 
  case recordK: return("recordK" ) ; break ; 
  case returnK: return("returnK" ) ; break ; 
  case rewindK: return("rewindK" ) ; break ; 
  case saveK: return("saveK" ) ; break ; 
  case saveitemK: return("saveitemK" ) ; break ; 
  case statfunK: return("statfunK" ) ; break ; 
  case stopK: return("stopK" ) ; break ; 
  case structureK: return("structureK" ) ; break ; 
  case subroutineK: return("subroutineK" ) ; break ; 
  case subscriptK: return("subscriptK" ) ; break ; 
  case subscriptitemK: return("subscriptitemK" ) ; break ; 
  case substringpartK: return("substringpartK" ) ; break ; 
  case typedeclK: return("typedeclK" ) ; break ; 
  case typeitemK: return("typeitemK" ) ; break ; 
  case typespecK: return("typespecK" ) ; break ; 
  case unimplementedK: return("unimplementedK" ) ; break ; 
  case unionK: return("unionK" ) ; break ; 
  case unopK: return("unopK" ) ; break ; 
  case writeK: return("writeK" ) ; break ; 
  case nonrealspecK: return("nonrealspecK" ) ; break ; 
  case realspecK: return("realspecK" ) ; break ; 
  case ispecK: return("ispecK" ) ; break ; 
  case lspecK: return("lspecK" ) ; break ; 
  case aspecK: return("aspecK" ) ; break ; 
  case fspecK: return("fspecK" ) ; break ; 
  case dspecK: return("dspecK" ) ; break ; 
  case especK: return("especK" ) ; break ; 
  case gspecK: return("gspecK" ) ; break ; 
  case tspecK: return("tspecK" ) ; break ; 
  case xspecK: return("xspecK" ) ; break ; 
    default : return("unknown" ) ;
    }
}
