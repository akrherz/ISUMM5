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
#define MAIN_ROUTINE

#include <stdio.h>
#include <stdlib.h>
#include "n32.h"
#include "sym.h"
#include "dm.h"
#include "twst.h"

user_backend( argc, argv, env )
    int argc ;
char *argv[], *env[] ;
{
    char temp[1024] ;
    char rootnames[1024] ;
    char *rootname ;
    char *p ;
    int leaf ;

    sprintf(toolname,"FLIC") ;

    header[0] = '\0' ;
    mdimstr[0] = '\0' ;
    ndimstr[0] = '\0' ;
    sw_IN_M_LOOP[0] = '\0' ;
    sw_IN_N_LOOP[0] = '\0' ;
    sw_allloops = 0 ;
    sw_collapse_loops_m = 0 ;
    sw_collapse_loops_n = 0 ;
    sw_collapse_m = 0 ;
    sw_collapse_n = 0 ;
    sw_write_db = 0 ;
    sw_addargs = 0 ;
    sw_addgenericargs = 0 ;
    sw_change_stops = 0 ;
    sw_change_stops_string[0] = '\0' ;
    sw_tag_enddos = 0 ;
    sw_quiet = 0 ;

    strcpy( thiscom, argv[0] ) ;
    argv++ ;
    while (*argv) {
        if (*argv[0] == '-') {  /* an option */
            if (!strncmp(*argv,"-m=",3)) {
              if (strlen(sw_IN_M_LOOP) > 0) strcat(sw_IN_M_LOOP,",") ;
              strcat(sw_IN_M_LOOP,*argv+3) ;
            }
            if (!strncmp(*argv,"-n=",3)) {
              if (strlen(sw_IN_N_LOOP) > 0) strcat(sw_IN_N_LOOP,",") ;
              strcat(sw_IN_N_LOOP,*argv+3) ;
            }
            if (!strncmp(*argv,"-mdim=",6)) {
              if (strlen(mdimstr) > 0) strcat(mdimstr,",") ;
              strcat(mdimstr,*argv+6) ;
            }
            if (!strncmp(*argv,"-ndim=",6)) {
              if (strlen(ndimstr) > 0) strcat(ndimstr,",") ;
              strcat(ndimstr,*argv+6) ;
            }
            if (!strncmp(*argv,"-H=",3)) {
              sprintf(temp,"      %s\n",*argv+3) ;
              strcat(header,temp) ;
            }
            if (!strncmp(*argv,"-STOP=",6)) {
              sprintf(sw_change_stops_string,"%s\n",*argv+6) ;
              strip_nl(sw_change_stops_string) ;
              sw_change_stops = 1 ;
            }
            if (!strncmp(*argv,"-AA",3)) {
              sw_addargs = 1 ;
            }
            if (!strncmp(*argv,"-AA2",3)) {
              sw_addgenericargs = 1 ;
            }
            /* write a db file for this routine */
            if (!strncmp(*argv,"-db=",4)) {
              if ((db_file=fopen(*argv+4,"w")) != NULL)
                sw_write_db = 1 ;
            }
            /* ordinarily, flic ignores loops whose ranges are
               specified by the loop declaratator (eg, if the loop
               is 1,MIX and MIX is also used to declare the decomposed
               dimension in model arrays); this causes
               flic to transform them too */
            if (!strcmp(*argv,"-A")) {
              sw_allloops = 1 ;
            }
            if (!strncmp(*argv,"-cn",3)) {
              sw_collapse_loops_n = 1 ;
            }
            if (!strncmp(*argv,"-cm",3)) {
              sw_collapse_loops_m = 1 ;
            }
            if (!strncmp(*argv,"-Cn",3)) {
              sw_collapse_loops_n = 1 ;
              sw_collapse_n = 1 ;
            } 
            if (!strncmp(*argv,"-Cm",3)) {
              sw_collapse_loops_m = 1 ;
              sw_collapse_m = 1 ;
            } 
/* added 990202, at Tim Sheehan's suggestion -- tack a _M or _N to the
   end of a flic inserted end do macro */
            if (!strncmp(*argv,"-tagenddo",9)) {
              sw_tag_enddos = 1 ;
            } 
            /* cut down on the amount of diagnostic stuff */
            if (!strcmp(*argv,"-q")) {
              sw_quiet = 1 ;
            }
            /* this allows inclusion of cpp directives in the header,
               but avoids the messiness of trying to get a # through
               a makefile */
            if (!strncmp(*argv,"-CPP=",5)) {
              sprintf(temp,"#%s\n",*argv+5) ;
              strcat(header,temp) ;
            }
            if (!strncmp(*argv,"-toolname=",10)) {
              sprintf(toolname,*argv+10) ;
            }
            /* include line numbers */
            if (!strncmp(*argv,"-L",2))
            {
              sw_L = 1 ;
            }
            if (!strcmp(*argv,"-blah")) {
               /* dummy */
            }
            if (!strncmp(*argv,"-F=",3)) {
              parse_directives(*argv+3) ;
            }
        }
        argv++ ;
    }

    if ( sw_change_stops )
    {
      if ( sw_change_stops_string[0] == '\0' )
      {
      fprintf(stderr,"FLIC ERROR -- No string specified with -STOP= argument\n") ; exit(2) ;
      }
      if ( strlen(sw_change_stops_string) > 20 )
      {
      fprintf(stderr,"FLIC ERROR -- String specified with -STOP= argument must be 20 characters or shorter\n") ; exit(2) ;
      }
    }


    if ((p = getenv("FLIC_M"))!=NULL)
      if ( strlen(sw_IN_M_LOOP) == 0 ) sprintf(sw_IN_M_LOOP,p) ;
    if ((p = getenv("FLIC_N"))!=NULL)
      if ( strlen(sw_IN_N_LOOP) == 0 ) sprintf(sw_IN_N_LOOP,p) ;
    if ((p = getenv("FLIC_MDIM"))!=NULL)
      if ( strlen(mdimstr) == 0 ) sprintf(mdimstr,p) ;
    if ((p = getenv("FLIC_NDIM"))!=NULL)
      if ( strlen(ndimstr) == 0 ) sprintf(ndimstr,p) ;

    
    if (!sw_quiet)
    {
    fprintf(stderr,"\n--- %s   FLIC: Preprocessor For MM5 ---\n", thiscom ) ;
    fprintf(stderr,"Created by J. Michalakes, ANL.  January 1997\n\n") ;
    }

    sym_init() ;

    read_config() ;  /* dummy routine; constants are hard coded */

    walk_statements( Abstract_Syntax_Tree, new_module, new_statement, 0 ) ;

/* debuggal */
#if 0
    {
      sym_nodeptr x ;
      x = sym_get( "a" ) ; show_entry(x) ;
      x = sym_get( "b" ) ; show_entry(x) ;
      x = sym_get( "mix" ) ; show_entry(x) ;
      x = sym_get( "mjx" ) ; show_entry(x) ;
    }
#endif

    if ( sw_collapse_m || sw_collapse_n ) 
    {
      walk_depth( Abstract_Syntax_Tree, collapse_idrefs, 1 ) ;
    }

    prback( Abstract_Syntax_Tree ) ;

    return(0) ;

}

user_help(thiscom)
    char * thiscom ;
{

fprintf(stderr,"--- FLIC specific options\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr," -extend       Allow extended source lines (to 132 characters).\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr," -m=list\n") ;
fprintf(stderr," -n=list       Specify  externally  set  loop  indices  for  slab-callable  routines\n") ;
fprintf(stderr,"               called from within an M or N loop.  List is a comma separated\n") ;
fprintf(stderr,"               list  of  [routine:]loop-var  items.  Loop-var  is  the  name  of  the\n") ;
fprintf(stderr,"               externally set loop variable as it is known within the routine.\n") ;
fprintf(stderr,"               Routine  is  the  name  of  the  routine  the  specification  applies\n") ;
fprintf(stderr,"               to.   If  routine  is  not  given,  the  specification  applies  to  all\n") ;
fprintf(stderr,"               routines in the source file.\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr," -mdim=list\n") ;
fprintf(stderr," -ndim=list    Specify a comma-separated list of identifiers that are used to\n") ;
fprintf(stderr,"               specify  the  M  or  N  decomposed  dimension  when  arrays  are\n") ;
fprintf(stderr,"               declared.\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr," -F=file       Specify a file containing FLIC directive information.  Specifi-\n") ;
fprintf(stderr,"               cations in the file will supersede command line settings.\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr," -H=string     Include string, indented to column 7, at the beginning of the\n") ;
fprintf(stderr,"               declarations section of the module.\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr," -CPP=string   Include string, prepended with a # character, at the begin-\n") ;
fprintf(stderr,"               ning of the declarations section of the module.\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr," -toolname=string\n") ;
fprintf(stderr,"               Use a different string than FLIC in the macro names.\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr,"--- Some newer options that have been added since the FLIC tech memo was release.\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr," -tagenddo     Have FLIC generate dimension specific FLIC_ENDDO_M and FLIC_ENDDO_N\n") ;
fprintf(stderr,"               macros at the close of a translated loop (default is to just\n") ;
fprintf(stderr,"               generate FLIC_ENDDO, which then requires the post-flic macro\n") ;
fprintf(stderr,"               expension to remember state information about loop\n") ;
fprintf(stderr,"               nesting).\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr," -q            Quiet operation.\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr," -A            Ordinarily, flic ignores loops whose ranges are\n") ;
fprintf(stderr,"               specified by the loop declaratator (eg, if the loop is\n") ;
fprintf(stderr,"               from 1 to MIX and MIX is also used to declare the\n") ;
fprintf(stderr,"               decomposed dimension in model arrays); this causes flic\n") ;
fprintf(stderr,"               to transform them too.\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr," -STOP=string  Have FLIC remove STOP statements in the code and replace them\n") ;
fprintf(stderr,"               with calls to a subroutine specified by string.  If the\n") ;
fprintf(stderr,"               STOP statement has an argument and the argument is a a\n") ;
fprintf(stderr,"               label, the label is converted to a string and passed to\n") ;
fprintf(stderr,"               the routine as an argument.  If the argument is a\n") ;
fprintf(stderr,"               string, the string is passed to the routine.  The user assumes\n") ;
fprintf(stderr,"               responsiblity for providing the routine: frequently this\n") ;
fprintf(stderr,"               will be some kind of shutdown routine that calls MPI_ABORT.\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr," -cm, -cn      Collapse (remove) loops in the m/n dimension.\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr," -Cm, -Cn      Collapse (remove) loops in the m/n dimension and eliminate\n") ;
fprintf(stderr,"               indices from definitions and references to local arrays\n") ;
fprintf(stderr,"               in those dimensions.  Eg. A local array X(I,K) will become\n") ;
fprintf(stderr,"               just X(K) if -Cm is specified.  An array X(I) will become\n") ;
fprintf(stderr,"               just a scalar X.\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr," \n") ;
fprintf(stderr,"--- Macros and markers generated into the code by FLIC:\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr," FLIC_DO_M( var, start, end )   Replaces  DO  loops  over  the  M  decomposed\n") ;
fprintf(stderr,"                                dimension.  Var is the loop variable; start and\n") ;
fprintf(stderr,"                                end are the starting and ending global indices.\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr," FLIC_DO_N( var, start, end )   Replaces  DO  loops  over  the  N  decomposed\n") ;
fprintf(stderr,"                                dimension.\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr," FLIC_ENDDO                     Replaces  ENDDO  or  labeled  CONTINUE\n") ;
fprintf(stderr,"                                statement at the end of a converted loop.\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr," FLIC_ENDDO_M\n") ;
fprintf(stderr," FLIC_ENDDO_N                   Same as FLIC_ENDDO except these are matched to the\n") ;
fprintf(stderr,"                                corresponding FLIC_DO_M or FLIC_DO_N macros.\n") ;
fprintf(stderr,"                                These are generated instead of FLIC_ENDDO if\n") ;
fprintf(stderr,"                                the -tagenddo option is specified.\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr," FLIC_G2L_M(index-expr)         Converts  index-expr  from  global  to  local  for\n") ;
fprintf(stderr,"                                M indices.\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr," FLIC_G2L_N(index-expr)         Converts index-expr from global to local for N\n") ;
fprintf(stderr,"                                indices.\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr," FLIC_L2G_M(index-expr)         Converts  index-expr  from  local  to  global  for\n") ;
fprintf(stderr,"                                M indices.\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr," FLIC_L2G_N(index-expr)         Converts index-expr from local to global for N\n") ;
fprintf(stderr,"                                indices.\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr," define(INSIDE_MLOOP)           This  M4  definition  is  generated  by  FLIC  for\n") ;
fprintf(stderr,"                                routines  that  are  callable  within  a  loop  over\n") ;
fprintf(stderr,"                                M.  It  is  a  flag  to  the  preprocessor  that  the\n") ;
fprintf(stderr,"                                FLIC_DO_N macros may need to be expanded\n") ;
fprintf(stderr,"                                differently.\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr," define(INSIDE_NLOOP)           This  M4  definition  is  generated  by  FLIC\n") ;
fprintf(stderr,"                                for  routines  that  are  callable  within  a  loop\n") ;
fprintf(stderr,"                                over  N.  It  is  a  flag  to  the  preprocessor  that\n") ;
fprintf(stderr,"                                the  FLIC_DO_M  macros  may  need  to  be  ex-\n") ;
fprintf(stderr,"                                panded differently.\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr," CFLIC END DECLARATIONS         This comment is placed at the end of the declaration\n") ;
fprintf(stderr,"                                section of the routine.  It provides a ready marker\n") ;
fprintf(stderr,"                                for pattern matching tools to recognize when they\n") ;
fprintf(stderr,"                                have reached the end of the declarations section.\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr,"--- Directives  File\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr,"FLIC may read the transformation specifications from a file whose name\n") ;
fprintf(stderr,"is specified using the -F option on the command line.  The\n") ;
fprintf(stderr,"specifications in the file will supersede those from the command line\n") ;
fprintf(stderr,"or the environment.  FLIC directives begin with C in the first column\n") ;
fprintf(stderr,"so that they may be treated as comments, should the user opt to include\n") ;
fprintf(stderr,"these in the Fortran source.  In this case, the -F option must specify\n") ;
fprintf(stderr,"the source file itself; otherwise the directives will be ignored.  The\n") ;
fprintf(stderr,"syntax for the directives is as follows:\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr," cflic m=list\n") ;
fprintf(stderr," cflic n=list        Same effect as the -m and -n command line options.\n") ;
fprintf(stderr," cflic mdim=list\n") ;
fprintf(stderr," cflic ndim=list     Same effect as the -mdim and -ndim command line options.\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr,"--- FLIC information\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr,"   http://www.mcs.anl.gov/Projects/FLIC\n") ;
fprintf(stderr,"\n") ;
fprintf(stderr,"\n") ;

}
