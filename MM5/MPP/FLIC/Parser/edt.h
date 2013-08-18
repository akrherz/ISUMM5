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
/* edt.h

   Enumerated Data Types

   created  11-22-89	michalak@atlantis.ees.anl.gov
*/

/* majornodes SCCS(4.1 91/02/18 15:39:47) */

/* major kinds of AST nodes */

#define                defK      8000
#define                 exK      8001
#define               typeK      8002
#define              tokenK      8003

/* %M% SCCS(%R%.%L% %D% %T%) */

/* Minor Kinds of  AST Nodes */

#define          altreturnK      8004
#define             assignK      8005
#define        assignlabelK      8006
#define          backspaceK      8007
#define              binopK      8008
#define          blockdataK      8009
#define               callK      8010
#define              closeK      8011
#define             commonK      8012
#define       complexconstK      8013
#define           continueK      8014
#define               dataK      8015
#define       datalistitemK      8016
#define            datarepK      8017
#define               didlK      8018
#define          didlrangeK      8019
#define          dimensionK      8020
#define                 doK      8021
#define            doenddoK      8022
#define            dorangeK      8023
#define                dotK      8024
#define            dowhileK      8025
#define          dummyargsK      8026
#define                endK      8027
#define              enddoK      8028
#define            endfileK      8029
#define              entryK      8030
#define        equivalenceK      8031
#define         equivenodeK      8032
#define              errorK      8033
#define           externalK      8034
#define          fielddeclK      8035
#define           fieldrefK      8036
#define             formatK      8037
#define           functionK      8038
#define               gotoK      8039
#define       gotoassignedK      8040
#define       gotocomputedK      8041
#define             iddeclK      8042
#define              idrefK      8043
#define            ifarithK      8044
#define          iflogicalK      8045
#define         ifthenelseK      8046
#define       ifthenelseifK      8047
#define           implicitK      8048
#define        implieddoinK      8049
#define       implieddooutK      8050
#define          implrangeK      8051
#define           implspecK      8052
#define            includeK      8053
#define          intrinsicK      8054
#define            inquireK      8055
#define          iocontrolK      8056
#define      iocontrolitemK      8057
#define         lengthspecK      8058
#define               mainK      8059
#define                mapK      8060
#define         morecommonK      8061
#define       morenamelistK      8062
#define           namelistK      8063
#define               nullK      8064
#define               openK      8065
#define          parameterK      8066
#define          paramitemK      8067
#define              parenK      8068
#define              pauseK      8069
#define            pointerK      8070
#define              printK      8071
#define            programK      8072
#define               readK      8073
#define             recordK      8074
#define             returnK      8075
#define             rewindK      8076
#define               saveK      8077
#define           saveitemK      8078
#define            statfunK      8079
#define               stopK      8080
#define          structureK      8081
#define         subroutineK      8082
#define          subscriptK      8083
#define      subscriptitemK      8084
#define      substringpartK      8085
#define           typedeclK      8086
#define           typeitemK      8087
#define           typespecK      8088
#define      unimplementedK      8089
#define              unionK      8090
#define               unopK      8091
#define              writeK      8092

/* format node types */

#define        nonrealspecK      8093
#define           realspecK      8094
#define              ispecK      8095
#define              lspecK      8096
#define              aspecK      8097
#define              fspecK      8098
#define              dspecK      8099
#define              especK      8100
#define              gspecK      8101
#define              tspecK      8102
#define              xspecK      8103
typedef int NodeKnd ;
typedef int DefKnd ;
typedef int TypeKnd ;
typedef int ExKnd ;
typedef int TokenKnd ;
