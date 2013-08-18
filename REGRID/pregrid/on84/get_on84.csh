#!/bin/csh

set echo

# The necessary arguments:

set startdate = $1    # YYYY-MM-DD+HH
set enddate =   $2    # YYYY-MM-DD+HH
set itimint =   $3    # 12

set curn = `echo ${startdate} | cut -c 1-4,6-7,9-10,12-13`
set endn =   `echo ${enddate} | cut -c 1-4,6-7,9-10,12-13`

# Set up the list "mdate".  "mdate" will hold all the times we 
# need to get.

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
get datasets/ds082.0/MSS-file-list on84.master
quit
EOF1

# Now invoke ftp to get the archive lists.

if ( ! -e on84.master ) then
   ftp -n < ftp.cmd
   rm ftp.cmd
endif

set MNAM = ( JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC )

cat >! gnc.awpr <<EOF
   {if ( ( substr(\$1,1,1) != "Y") && ( substr(\$1,1,1) != "K") ) next}
   { hold = \$2 }
   { gsub( "jan", "01", \$2)
     gsub( "feb", "02", \$2)
     gsub( "mar", "03", \$2)
     gsub( "apr", "04", \$2)
     gsub( "may", "05", \$2)
     gsub( "jun", "06", \$2)
     gsub( "jul", "07", \$2)
     gsub( "aug", "08", \$2)
     gsub( "sep", "09", \$2)
     gsub( "oct", "10", \$2)
     gsub( "nov", "11", \$2)
     gsub( "dec", "12", \$2) }
     { if (( substr(\$2,1,8) <= IDATE) && (substr(\$2, 10, 8) >= IDATE) )
       {
	 {print substr(hold,1,9), \$1}
       }
    } 
EOF
endif

# Construct the dates as they appear in the archive titles.  Use awk
# (nawk) to get the actual MSS filenames from the archive lists.  We
# change the text-string month in the archive list to a two-digit 
# number, to make comparisons among dates easier.

foreach ndate ( $mdate ) 

   set YYYY = `echo $ndate | cut -b 1-4`
   set Mm = `echo $ndate | cut -b 6-7`
   set Dd = `echo $ndate | cut -b 9-10`


# Get the upper-air analysis files.


   set MssFile = `nawk -v IDATE=${YYYY}${Mm}${Dd} -f gnc.awpr on84.master`
   if ($MssFile[2] == ) then
cat <<EOF
**********************************************************
**********************************************************
*******                                            *******
*******  ERROR:                                    *******
*******  Date ${YYYY}${Mm}${Dd} not found in archive list.  *******
*******                                            *******
**********************************************************
**********************************************************

Here's the archive list:

EOF
      cat on84.master
      exit (1)
   endif
   set Local = NCEP_ON84.${YYYY}${Mm}
   set Remote = /DSS/$MssFile[2]

   if ( ! -e $Local ) then 
      echo "msread -f BI $Local $Remote"
      msread -f BI $Local $Remote
#      rcp $Local ${Host}
   endif

end

rm gnc.awpr
