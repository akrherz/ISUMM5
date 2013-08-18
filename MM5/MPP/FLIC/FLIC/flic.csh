#!/bin/csh -f
########################################################################
#     
#                              COPYRIGHT
#     
#     The following is a notice of limited availability of the code and 
#     Government license and disclaimer which must be included in the 
#     prologue of the code and in all source listings of the code.
#     
#     Copyright notice
#       (c) 1977  University of Chicago
#     
#     Permission is hereby granted to use, reproduce, prepare 
#     derivative works, and to redistribute to others at no charge.  If 
#     you distribute a copy or copies of the Software, or you modify a 
#     copy or copies of the Software or any portion of it, thus forming 
#     a work based on the Software and make and/or distribute copies of 
#     such work, you must meet the following conditions:
#     
#          a) If you make a copy of the Software (modified or verbatim) 
#             it must include the copyright notice and Government       
#             license and disclaimer.
#     
#          b) You must cause the modified Software to carry prominent   
#             notices stating that you changed specified portions of    
#             the Software.
#     
#     This software was authored by:
#     
#     Argonne National Laboratory
#     J. Michalakes: (630) 252-6646; email: michalak@mcs.anl.gov
#     Mathematics and Computer Science Division
#     Argonne National Laboratory, Argonne, IL  60439
#     
#     ARGONNE NATIONAL LABORATORY (ANL), WITH FACILITIES IN THE STATES 
#     OF ILLINOIS AND IDAHO, IS OWNED BY THE UNITED STATES GOVERNMENT, 
#     AND OPERATED BY THE UNIVERSITY OF CHICAGO UNDER PROVISION OF A 
#     CONTRACT WITH THE DEPARTMENT OF ENERGY.
#     
#                      GOVERNMENT LICENSE AND DISCLAIMER
#     
#     This computer code material was prepared, in part, as an account 
#     of work sponsored by an agency of the United States Government.
#     The Government is granted for itself and others acting on its 
#     behalf a paid-up, nonexclusive, irrevocable worldwide license in 
#     this data to reproduce, prepare derivative works, distribute 
#     copies to the public, perform publicly and display publicly, and 
#     to permit others to do so.  NEITHER THE UNITED STATES GOVERNMENT 
#     NOR ANY AGENCY THEREOF, NOR THE UNIVERSITY OF CHICAGO, NOR ANY OF 
#     THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR 
#     ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, 
#     COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, 
#     PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD 
#     NOT INFRINGE PRIVATELY OWNED RIGHTS.
#
############################################################################

# FLIC: Fortran Loop and Interface Converter (Driver Script)

# UNIX commands used; set these to the appropriate value on your system
# (if they're already in your shell command execution path, you don't
# need to change these)

# This script requires that the environment variable FLICDIR
# be set or that the user have the path to the FLIC directory
# in their $HOME/.flicrc file.

set retval=0
set HOSTNAME=hostname
set GREP=grep
set FGREP=fgrep
set AWK=awk
set SED=sed
set CUT=cut
set PASTE=paste
set SORT=sort
set CAT=cat
set SH=sh
set HEAD=head
set HARDRM="/bin/rm -f"
set ECHO=echo
set ED=ed

# Temp directory

set TMP=/tmp

# it should not be necessary to modify anything below this line.

onintr cleanup

if ( $?noclobber) unset noclobber

set thiscommand = $0

setenv FLICDIR INSTALL_STRING_FLICDIR

if ( ! $?FLICDIR) then
  if ( $?HOME ) then
    if ( -f $HOME/.flicrc ) then
      source $HOME/.flicrc
    endif
  endif
endif

if ( ! $?FLICDIR) then
    /bin/sh -c "echo FLICDIR environment variable not set.  Abort. >&2"
    set badsetup
    goto usage
endif

# commands used from the FLICDIR directory

#set SCANNER=${FLICDIR}/n32.scanner
set SCANNER=${FLICDIR}/../bin/n32.scanner
set PARSER="${FLICDIR}/dm -q"
set REASSEMBLE="${FLICDIR}/reassemble"

#scanner switches

set s1_extend
set s1_nocomments

#parser switches

set s1_pargs = (-L -RF)

set state = 0

set f1
foreach arg ($argv)
  set opt = `$ECHO "$arg" | $GREP '^-'`
  if ( "$opt" == "" ) then
    if      ( $state == 0 ) then
      set f1=$arg
    else
      set retval=1
      goto usage
    endif
    @ state += 1
  else
    if      ( $state == 0 ) then
      if ( "$arg" == "-extend" ) then
	set s1_extend = -extend
      else if ( "$arg" == "-nocomments" ) then
	set s1_nocomments = yes
      else if ( "$arg" == "-help" || "$arg" == "-h" ) then
	goto usage
      else
        set s1_pargs  = ( $s1_pargs $arg )
      endif
    else
      set retval=1
      goto usage
    endif
  endif
end

if ( $state > 1 ) goto usage

$CAT > $TMP/bleshyou.$$ << 'H1'
BEGIN {latch = 0}
/C_FLIC_END_NOFLIC/ { latch = 0 }
{ 
  if ( latch == 1 )
  {
    print "CFLICBYE " $0
  }
  else
  {
    print $0
  }
}
/C_FLIC_BEGIN_NOFLIC/ { latch = 1 }
'H1'


$SH -c "echo scanning $f1 >&2"

$AWK -f $TMP/bleshyou.$$ $f1 > $TMP/aaa.$$

$FGREP    CFLICBYE $TMP/aaa.$$ > $TMP/bbb.$$

$SCANNER $s1_extend $TMP/aaa.$$ > $TMP/flic_scanned.$$
if ( $status != 0 ) then
  $SH -c "echo Some error scanning.  $thiscommand aborting. >&2"
  set retval = 1
  goto cleanup
endif
$SH -c "echo parsing $f1 >&2"
$PARSER $s1_pargs $TMP/flic_scanned.$$  > $TMP/flic_dat.$$
if ( $status != 0 ) then
  $SH -c "echo Some error parsing.  $thiscommand aborting. >&2"
  set retval = 1
  goto cleanup
endif
$SH -c "echo finished parsing $f1 >&2"

if ( $s1_nocomments != yes ) then
  $FGREP TCOMMENT $TMP/flic_scanned.$$ |\
    $AWK -F: '{print $4}' > $TMP/flic_cnum.$$
  $FGREP TCOMMENT $TMP/flic_scanned.$$ |\
    $SED 's/^.*TCOMMENT://' > $TMP/flic_coms2.$$
  $PASTE $TMP/flic_cnum.$$ $TMP/flic_coms2.$$ > $TMP/flic_coms.$$
  $HARDRM $TMP/flic_cnum.$$ $TMP/flic_coms2.$$
# 010425, jm; kludge for Solaris sort command that segfaults 
# if TMP/flic_coms is zero length. Reported by:
# Walters Michael K LtCol AFIT/ENP <Michael.Walters@afit.edu>
  echo " " >> $TMP/flic_coms.$$
#
  $SORT -nm +0 -1 $TMP/flic_dat.$$ $TMP/flic_coms.$$ | $CUT -f2- | \
       $REASSEMBLE $TMP/bbb.$$ | $SED 's/CFLICBYE //'
else
  $CUT -f2- $TMP/flic_dat.$$ | $SED 's/CFLICBYE //'
endif


goto cleanup

usage:

$HOSTNAME | $FGREP mcs.anl.gov
if ( $status == 0 ) then
  set additional="On the ANL/MCS system, this is /home/michalak/FLIC."
else
  set additional=""
endif

$CAT << H

Usage: $thiscommand:t [ options ][ file1 ]

--- Options

 -extend       Allow extended source lines (to 132 characters).

 -m=list
 -n=list       Specify  externally  set  loop  indices  for  slab-callable  routines
               called from within an M or N loop.  List is a comma separated
               list  of  [routine:]loop-var  items.  Loop-var  is  the  name  of  the
               externally set loop variable as it is known within the routine.
               Routine  is  the  name  of  the  routine  the  specification  applies
               to.   If  routine  is  not  given,  the  specification  applies  to  all
               routines in the source file.

 -mdim=list
 -ndim=list    Specify a comma-separated list of identifiers that are used to
               specify  the  M  or  N  decomposed  dimension  when  arrays  are
               declared.

 -F=file       Specify a file containing FLIC directive information.  Specifi-
               cations in the file will supersede command line settings.

 -H=string     Include string, indented to column 7, at the beginning of the
               declarations section of the module.

 -CPP=string   Include string, prepended with a # character, at the begin-
               ning of the declarations section of the module.

 -toolname=string
               Use a different string than FLIC in the macro names.

 Some newer options that have been added since the FLIC tech memo was released:

 -tagenddo     Have FLIC generate dimension specific FLIC_ENDDO_M and FLIC_ENDDO_N
               macros at the close of a translated loop (default is to just
               generate FLIC_ENDDO, which then requires the post-flic macro
               expension to remember state information about loop
               nesting).

 -q            Quiet operation.

 -A            Ordinarily, flic ignores loops whose ranges are
               specified by the loop declaratator (eg, if the loop is
               from 1 to MIX and MIX is also used to declare the
               decomposed dimension in model arrays); this causes flic
               to transform them too.

 -STOP=string  Have FLIC remove STOP statements in the code and replace them
               with calls to a subroutine specified by string.  If the
               STOP statement has an argument and the argument is a a
               label, the label is converted to a string and passed to
               the routine as an argument.  If the argument is a
               string, the string is passed to the routine.  The user assumes
               responsiblity for providing the routine: frequently this
               will be some kind of shutdown routine that calls MPI_ABORT.

 -cm, -cn      Collapse (remove) loops in the m/n dimension.

 -Cm, -Cn      Collapse (remove) loops in the m/n dimension and eliminate
               indices from definitions and references to local arrays
               in those dimensions.  Eg. A local array X(I,K) will become
               just X(K) if -Cm is specified.  An array X(I) will become
               just a scalar X.



 
--- Macros and markers generated into the code by FLIC:


 FLIC_DO_M( var, start, end )   Replaces  DO  loops  over  the  M  decomposed
                                dimension.  Var is the loop variable; start and
                                end are the starting and ending global indices.

 FLIC_DO_N( var, start, end )   Replaces  DO  loops  over  the  N  decomposed
                                dimension.

 FLIC_ENDDO                     Replaces  ENDDO  or  labeled  CONTINUE
                                statement at the end of a converted loop.

 FLIC_ENDDO_M
 FLIC_ENDDO_N                   Same as FLIC_ENDDO except these are matched to the
                                corresponding FLIC_DO_M or FLIC_DO_N macros.
                                These are generated instead of FLIC_ENDDO if
                                the -tagenddo option is specified.

 FLIC_G2L_M(index-expr)         Converts  index-expr  from  global  to  local  for
                                M indices.

 FLIC_G2L_N(index-expr)         Converts index-expr from global to local for N
                                indices.

 FLIC_L2G_M(index-expr)         Converts  index-expr  from  local  to  global  for
                                M indices.

 FLIC_L2G_N(index-expr)         Converts index-expr from local to global for N
                                indices.

 define(INSIDE_MLOOP)           This  M4  definition  is  generated  by  FLIC  for
                                routines  that  are  callable  within  a  loop  over
                                M.  It  is  a  flag  to  the  preprocessor  that  the
                                FLIC_DO_N macros may need to be expanded
                                differently.

 define(INSIDE_NLOOP)           This  M4  definition  is  generated  by  FLIC
                                for  routines  that  are  callable  within  a  loop
                                over  N.  It  is  a  flag  to  the  preprocessor  that
                                the  FLIC_DO_M  macros  may  need  to  be  ex-
                                panded differently.

 CFLIC END DECLARATIONS         This comment is placed at the end of the declaration
                                section of the routine.  It provides a ready marker
                                for pattern matching tools to recognize when they
                                have reached the end of the declarations section.

--- Directives  File

FLIC may read the transformation specifications from a file whose name
is specified using the -F option on the command line.  The
specifications in the file will supersede those from the command line
or the environment.  FLIC directives begin with C in the first column
so that they may be treated as comments, should the user opt to include
these in the Fortran source.  In this case, the -F option must specify
the source file itself; otherwise the directives will be ignored.  The
syntax for the directives is as follows:

 cflic m=list
 cflic n=list        Same effect as the -m and -n command line options.
 cflic mdim=list
 cflic ndim=list     Same effect as the -mdim and -ndim command line options.

--- FLIC information

   http://www.mcs.anl.gov/Projects/FLIC

H

if ( $?badsetup ) then
set retval  = 2
$CAT << H2
If you get a message:

    echo FLICDIR environment variable not set.  Abort.

You need to set the FLICDIR variable to the path of the FLIC installation
directiory.  $additional

H2
endif

cleanup:
$HARDRM $TMP/flic_scanned.$$ $TMP/flic_cnum.$$ $TMP/flic_coms2.$$ \
	$TMP/flic_coms.$$    $TMP/flic_dat.$$  $TMP/bleshyou.$$   \
        $TMP/aaa.$$  $TMP/bbb.$$

outtahere:
if ( $?retval ) exit $retval
exit 0

