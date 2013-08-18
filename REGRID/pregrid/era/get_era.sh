# QSUB -r GETERA                        # request name
# QSUB -q reg                           # job queue class
# QSUB -eo                              # stdout and stderr together
# QSUB -lM 2Mw                          # maximum memory
# QSUB -lT 600                          # time limit
# QSUB -lt 600                          # time limit
# QSUB -mb                              # mail me when it starts
# QSUB -me                              # mail me when it ends
# QSUB                                  # no more qsub commands

# The necessary arguments:

# Read the necessary arguments from the namelist file "pregrid.namelist":

set cmnd = `nawk -f ../util/gn.awpr pregrid.namelist`
$cmnd

@ START_MONTH = $START_MONTH
@ START_DAY = $START_DAY
@ START_HOUR = $START_HOUR

if ( $START_MONTH <= 9 ) set START_MONTH = 0${START_MONTH}
if ( $START_DAY <= 9 ) set START_DAY = 0${START_DAY}
if ( $START_HOUR <= 9 ) set START_HOUR = 0${START_HOUR}

set startdate = ${START_YEAR}-${START_MONTH}-${START_DAY}+${START_HOUR}
set ndates = $NTIMES
set itimint = $DELTA_TIME


# Set up the list "mdate".  "mdate" will hold all the times we 
# need to get.

set num = 0
set idth = -$itimint
set mdate = ( )
while ( $num < $ndates )
   @ idth = $idth + $itimint
   @ num ++
   set ndate = `../util/geth_newdate ${startdate} ${idth}`
   set mdate = ( ${mdate} ${ndate} )
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
   endif

# Get the surface analysis files.
   set EraFile = `nawk -v NAM=${YYYY}${NAMM[$Mm]} -v HH=${HH}Z \\
      '{ if (( substr($2,1,7) == NAM ) && ($3 == HH )) { print $1 } }' \\
       sfc.master `

   set local = ERA_GRIB_SFC.${YYYY}${NAMM[$Mm]}${HH}Z
   if ( ! -e $local ) then 
      echo "msread -f BI $local /DSS/${EraFile}"
      msread -f BI $local /DSS/${EraFile}
   endif

end

