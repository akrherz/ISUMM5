#!/bin/csh
set echo

# The necessary arguments:

set startdate = $1 # 1998-12-01+00
set enddate =   $2 # 1998-12-02+00
set itimint =   $3 # 6

# set Host = kmanning@wahoo.mmm:/mmmtmp/kmanning

###############################################################################
######################                         ################################
######################  END USER MODIFICATION  ################################
######################                         ################################
###############################################################################

# cd ${TMPDIR}/REGRID/pregrid/nnrp

# if ( ! -e /tmp/kmanning/NNRP ) mkdir -p /tmp/kmanning/NNRP
# cd /tmp/kmanning/NNRP

# Set up the lists "mdate" and "sfcdate".  "mdate" will hold all the 
# upper-air times we need to get.  "sfcdate" will hold all the surface
# times we need to get.  "mdate" and "sfcdate" are not the same, because
# some of the surface analyses are actually 6-hour forecasts.

set curn = `echo ${startdate} | cut -c 1-4,6-7,9-10,12-13`
set endn =   `echo ${enddate} | cut -c 1-4,6-7,9-10,12-13`

set num = 0
set idth = -$itimint
set mdate = ( )
set sfcdate = ( )
while ( $curn < $endn )
   @ idth = $idth + $itimint
   @ num ++
   set ndate = `../util/geth_newdate ${startdate} ${idth}`
   set sdate = `../util/geth_newdate ${ndate} -6`
   set mdate = ( ${mdate} ${ndate} )
   set sfcdate = ( ${sfcdate} ${sdate} )
   set curn = `echo ${ndate} | cut -c 1-4,6-7,9-10,12-13`
end

echo "mdate = $mdate"
echo "sfcdate = $sfcdate"

# Set up the ftp command-script to get the archive lists.  First, set 
# up to login anonymously to ncardata.ucar.edu

cat >! ftp.cmd << EOF1
open ncardata.ucar.edu
user anonymous ${user}@ucar.edu
EOF1

# Construct the dates as they appear in the upper-air archive titles.

set YearSave = 0

set DssFile3d = ( )
set DssFile2d = ( )

foreach ndate ( $mdate ) 

#   @ YYYY = ${ndate} / 1000000
#   @ YYYY = 1900 + ${YYYY}
    set YYYY = `echo $ndate | cut -b 1-4`
#   @ YYMM = ${ndate} / 10000
    set YYMM = `echo $ndate | cut -b 3-4,6-7`

   if ( $YYYY != $YearSave ) then
      set YearSave = $YYYY

      set File3d = ${YYYY}_A.list
      if ( $YYYY == 1976 || $YYYY == 1977 || $YYYY == 1978 ) then
        set File3d = ${YYYY}rerun_A.list
      endif
      if ( $YYYY == 1997 || $YYYY == 1998 || $YYYY == 1999 || $YYYY == 2000 ) then
        set File3d = ${YYYY}tovsrerun_A.list
      endif

      cat >> ftp.cmd << EOF2
ls datasets/ds090.0/inventories
get datasets/ds090.0/inventories/${File3d} ${File3d}
EOF2

   endif

end

# Construct the dates as they appear in the surface archive titles.

set YearSave = 0
foreach ndate ( $sfcdate ) 

#    @ YYYY = ${ndate} / 1000000
#    @ YYYY = 1900 + ${YYYY}
    set YYYY = `echo $ndate | cut -b 1-4`
#    @ YYMM = ${ndate} / 10000
    set YYMM = `echo $ndate | cut -b 3-4,6-7`

   if ( $YYYY != $YearSave ) then
      set YearSave = $YYYY

      set File3d = ${YYYY}_A.list
      if ( $YYYY == 1976 || $YYYY == 1977 || $YYYY == 1978 ) then
        set File3d = ${YYYY}rerun_A.list
      endif
      if ( $YYYY == 1997 || $YYYY == 1998 || $YYYY == 1999 || $YYYY == 2000 ) then
        set File3d = ${YYYY}tovsrerun_A.list
      endif
      set File2d = ${YYYY}.2D

      cat >> ftp.cmd << EOF2
ls datasets/ds090.0/inventories
get datasets/ds090.0/inventories/${File3d} ${File2d}
EOF2

   endif

end

# Close off the ftp command-script.

cat >> ftp.cmd << EOF3
quit
EOF3

# Now invoke ftp to get the archive lists.

ftp -n < ftp.cmd
echo "status = $status"
rm ftp.cmd

# Use awk (nawk) to get the actual MSS filenames from the archive lists.

   # Get the upper-air analysis tar files.

set YearSave = 0
foreach ndate ( $mdate ) 

#    @ YYYY = ${ndate} / 1000000
#    @ YYYY = 1900 + ${YYYY}
#    @ YYMM = ${ndate} / 10000
    set YYYY = `echo $ndate | cut -b 1-4`
    set YYMM = `echo $ndate | cut -b 3-4,6-7`

   if ( $YYMM != $YearSave ) then
      set YearSave = $YYMM

      set File3d = ${YYYY}_A.list
      if ( $YYYY == 1976 || $YYYY == 1977 || $YYYY == 1978 ) then
        set File3d = ${YYYY}rerun_A.list
      endif
      if ( $YYYY == 1997 || $YYYY == 1998 || $YYYY == 1999 || $YYYY == 2000 ) then
        set File3d = ${YYYY}tovsrerun_A.list
      endif

      if ( ! -e $File3d ) then
         echo "Archive listing file $File3d not found"
         exit (2)
      endif



      set ssFile3d = `nawk -v YYMM=$YYMM '{ if ( $6 == YYMM".pgb.f00" ) { print substr($1,3,6) } }' $File3d `

      if ( $ssFile3d == ) then
cat <<EOF
********************************************************
********************************************************
*******                                          *******
*******  ERROR:                                  *******
*******  Date $YYMM not found in archive list.   *******
*******                                          *******
********************************************************
********************************************************

Here's the archive list:

EOF
	 cat $File3d
         exit (1)
      endif

      set local = NNRP_GRIB_UPA.${YYMM}
      if ( ! -e $local ) then 
         echo "msread -f BI $local /DSS/${ssFile3d}"
         msread -f BI $local /DSS/${ssFile3d}
      endif

   endif

end

   # Get the surface analysis tar files.

set YearSave = 0
foreach ndate ( $sfcdate ) 
#    @ YYYY = ${ndate} / 1000000
#    @ YYYY = 1900 + ${YYYY}
#    @ YYMM = ${ndate} / 10000
    set YYYY = `echo $ndate | cut -b 1-4`
    set YYMM = `echo $ndate | cut -b 3-4,6-7`

   if ( $YYMM != $YearSave ) then
      set YearSave = $YYMM

      set File2d = ${YYYY}.2D
      set ssFile2d = `nawk -v YYMM=$YYMM  '{ if ( $6 == YYMM".2D" ) { print substr($1,3,6) } }' $File2d `
      set DssFile2d = ( $DssFile2d $ssFile2d )

      set local = NNRP_GRIB_SFC.${YYMM}
      if ( ! -e $local ) then 
         echo "msread -f BI $local /DSS/${ssFile2d}"
         msread -f BI $local /DSS/${ssFile2d}
      endif

   endif

end

# Untar the upper-air analyses that we want from the tarfiles.

foreach  ndate ( $mdate )

#   @ YYMM = ${ndate} / 10000
   set YYMM = `echo $ndate | cut -b 3-4,6-7`
   set mdd = `echo $ndate | cut -b 3-4,6-7,9-10,12-13`
   set flwant = 'pgb.f00'${mdd}
   if ( ! -e $flwant ) then
      tar xvf NNRP_GRIB_UPA.${YYMM} ${flwant}
      if ( ! -e ${flwant} ) tar xvf NNRP_GRIB_UPA.${YYMM} ./${flwant}
#      rcp ${flwant} ${Host}
   endif
end

# Untar the surface analyses that we want from the tarfiles.

foreach ndate ( $sfcdate )
#   @ YYMM = $ndate / 10000
    set YYMM = `echo $ndate | cut -b 3-4,6-7`
#    set sdate = `get_newdate ${ndate} 6`
   set sdate = `../util/geth_newdate ${ndate} 6`

  if ( ! -e FLWANT_SFC.${YYMM} ) then

     set flwant = ( )
     set flwant = ( $flwant 'TMPhag.2.'${YYMM} )  # Temperature at 2 m AGL
     set flwant = ( $flwant 'TMPsfc.'${YYMM} )
     set flwant = ( $flwant 'SOILWdlr.10.'${YYMM} )
     set flwant = ( $flwant 'SOILWdlr.200.'${YYMM} )
     set flwant = ( $flwant 'SPFHhag.2.'${YYMM} )
     set flwant = ( $flwant 'TMPdlr.10.'${YYMM} )
     set flwant = ( $flwant 'TMPdlr.200.'${YYMM} )
     set flwant = ( $flwant 'TMPdpl.300.'${YYMM} )
     set flwant = ( $flwant 'ICECsfc.'${YYMM} )
     set flwant = ( $flwant 'LANDsfc.'${YYMM} )     # Land(1)/Sea(0) Mask
     set flwant = ( $flwant 'PRESsfc.'${YYMM} )
     set flwant = ( $flwant 'UGRDhag.10.'${YYMM} )  # U at 10 m AGL
     set flwant = ( $flwant 'VGRDhag.10.'${YYMM} )  # V at 10 m AGL
     set flwant = ( $flwant 'WEASDsfc.'${YYMM} )    # Water Equivalent Snow Depth
     echo $flwant

     touch FLWANT_SFC.${YYMM}
     foreach file ( $flwant )
        if ( ! -e ${file} ) tar xvf NNRP_GRIB_SFC.${YYMM} ${file}
        if ( ! -e ${file} ) tar xvf NNRP_GRIB_SFC.${YYMM} ./${file}
#	rcp ${file} ${Host}/SFCNNRP_${file:t}
        cat ./${file} >> FLWANT_SFC.${YYMM}
	mv ./$file ./SFCNNRP_${file}
     end

  endif

end
