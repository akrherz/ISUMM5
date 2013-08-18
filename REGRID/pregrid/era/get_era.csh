#!/bin/csh
set echo

# The necessary arguments:

set startdate = $1 # 1987-08-25+12     # YYYY-MM-DD+HH
set enddate =   $2 # 1987-08-26+00
set itimint =   $3 # 6: data time interval

#set Host = kmanning@wahoo.mmm:/mmmtmp/kmanning

###############################################################################
######################                         ################################
######################  END USER MODIFICATION  ################################
######################                         ################################
###############################################################################

# cd $TMPDIR

# Set up the list "mdate".  "mdate" will hold all the times we 
# need to get.

set curn = `echo ${startdate} | cut -c 1-4,6-7,9-10,12-13`
set endn =   `echo ${enddate} | cut -c 1-4,6-7,9-10,12-13`

if ( $curn < 1979010000 ) then
   echo 'ERA data begins in Jan 1979. Please choose another dataset'
   exit
endif
if ( $curn > 1993123112 ) then
   echo 'ERA data ends in Dec 1993. Please choose another dataset'
   exit
endif

set num = 0
set idth = -$itimint
set mdate = ( )
while ( $curn < $endn )
   @ idth = $idth + $itimint
   @ num ++
   set ndate = `../util/geth_newdate ${startdate} ${idth}`
   set mdate = ( ${mdate} ${ndate} )
   set curn = `echo ${ndate} | cut -c 1-4,6-7,9-10,12-13`
end

echo "mdate = $mdate"

# Set up the ftp command-script to get the archive lists.  First, set 
# up to login anonymously to ncardata.ucar.edu

cat >! ftp.cmd << EOF1
open ncardata.ucar.edu
user anonymous ${user}@ucar.edu
get datasets/ds115.0/MSS-file-list sfc.master
get datasets/ds115.1/MSS-file-list upa.master
quit
EOF1

# Now invoke ftp to get the archive lists.

if ( ! -e upa.master ) then
   ftp -n < ftp.cmd
   rm ftp.cmd
endif

set MNAM = ( jan feb mar apr may jun jul aug sep oct nov dec )
set NAMM = ( jan jan jan jan jan jan jul jul jul jul jul jul )

# Construct the dates as they appear in the upper-air archive titles.
# Use awk (nawk) to get the actual MSS filenames from the archive lists.


foreach ndate ( $mdate ) 

   set YYYY = `echo $ndate | cut -b 1-4`
   set Mm = `echo $ndate | cut -b 6-7`
   set HH   = `echo $ndate | cut -b 12-13`

# Get the upper-air analysis files.
   set EraFile = `nawk -v NAM=${YYYY}${MNAM[$Mm]} -v HH=${HH}Z \\
      '{ if (( $2 == NAM ) && ($3 == HH )) { print $1 } }' upa.master `

   set local = ERA_GRIB_UPA.${YYYY}${MNAM[$Mm]}${HH}Z
   if ( ! -e $local ) then 
      echo "msread -f BI $local /DSS/${EraFile}"
      msread -f BI $local /DSS/${EraFile}
#     rcp $local ${Host}
   endif

# Get the surface analysis files.
   set EraFile = `nawk -v NAM=${YYYY}${NAMM[$Mm]} -v HH=${HH}Z \\
      '{ if (( substr($2,1,7) == NAM ) && ($3 == HH )) { print $1 } }' \\
       sfc.master `

   set local = ERA_GRIB_SFC.${YYYY}${NAMM[$Mm]}${HH}Z
   if ( ! -e $local ) then 
      echo "msread -f BI $local /DSS/${EraFile}"
      msread -f BI $local /DSS/${EraFile}
#     rcp $local ${Host}
   endif

end
