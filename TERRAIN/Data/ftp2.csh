#!/bin/csh -f
#
#      set default script variables
#
set echo
#
#      Uncomment appropriate ftp switches if using this
#        script outside the terrain.deck
#
#      1. leave this commented if you are making maps only
#
#set IfProcData
#
#      2. uncomment any of these options for ftping 
#
#set IfLowResData
#set If2minData
#set If30secData
#
#      3. uncomment one of these options for ftping landuse data
#
#set LandUse = OLD
#set LandUse = USGS
#set LandUse = SiB
#
#      3. uncomment this if ftping land-surface model data
#
#set IfLandSurface  
#
#      Default value if not using ftp from the deck
#
#set DataType = ( 0 0 0 0 0 0 )
#
#ln -s src/rdnml.exe a.out
#
if ( $?DataType ) then
   echo 'ftp only'
else
   echo 'ftp from terrain.deck'
#  f90 -o rdnml rdnml.F
   set DataType = `src/rdnml < terrain.namelist`
   echo $DataType
   if ( $DataType[1] == 1 ) set IfLowResData
   if ( $DataType[2] == 1 ) set If2minData
   if ( $DataType[3] == 1 ) set If30secData
   if ( $DataType[4] == 1 ) set IfProcData
   if ( $DataType[5] == 0 ) set LandUse = OLD
   if ( $DataType[5] == 1 ) set LandUse = USGS
   if ( $DataType[5] == 2 ) set LandUse = SiB
   if ( $DataType[6] == 1 ) set IfLandSurface
endif
#
#      Do ftp inside Data directory
#
cd Data
#
#  If IFTER = .TRUE., or IfProcData
#
if ( $?IfProcData ) then
   echo 'Beginning to ftp terrain data'
   echo 'First, create the ftp script based on switches set'
#
#     ftp script begins ...
#
   set dhost=ftp.ucar.edu
   cat >! ftp.tmp << EOF
   user anonymous ${USER}@
   bi
   cd mesouser/MM5V3/TERRAIN_DATA
EOF
#
#      Add deep soil temp and vegetation fraction data to the script
#
   if ( $?IfLandSurface && ( ( ! -e SOILTEMP.60 ) || ( ! -e VEG-FRACTION.10 ) ) ) then
      echo 'we need additional data for Land-Surface model'
      cat >! ftp_land << EOF
      get SOILTEMP.60
      get VEG-FRACTION.10.gz
EOF
      cat ftp_land >> ftp.tmp
   endif
#
#      Add low-resolution terrain/landuse source data to the script
#
   if ( $?IfLowResData ) then

      if ( ! -e DEM_30M_GLOBAL ) then
         cat >! ftp_low << EOF
         get TER.TAR.gz
EOF
         cat ftp_low >> ftp.tmp
      endif

      if ( $LandUse == USGS && ( ! -e LWMASK-USGS.30 ) ) then
         echo 'we need low resolution USGS land-water mask data'
         cat >! ftp_lw1 << EOF
         get LANDWATERMASK-USGS.TAR.gz
EOF
         cat ftp_lw1 >> ftp.tmp

      else if ( $LandUse == SiB && ( ! -e LWMASK-SIB.30 ) ) then 
         echo 'we need low resolution SiB land-water mask data'
         cat >! ftp_lw1 << EOF
         get LANDWATERMASK-SIB.TAR.gz
EOF
         cat ftp_lw1 >> ftp.tmp
      endif

      if ( ( $LandUse == OLD ) && ( ! -e LANDUSE.30 ) ) then
         echo 'we need low resolution OLD landuse data'
         cat >! ftp_veg1 << EOF
         get LANDUSE-OLD.TAR.gz
EOF
         cat ftp_veg1 >> ftp.tmp

      else if ( ( $LandUse == USGS ) && ( ! -e VEG-USGS.30 ) ) then
         echo 'we need low resolution USGS vegetation data'
         cat >! ftp_veg1 << EOF
         get VEGETATION-USGS.TAR.gz
EOF
         cat ftp_veg1 >> ftp.tmp

      else if ( ( $LandUse == SiB ) && ( ! -e VEG-SIB.30 ) ) then
         echo 'we need low resolution SiB vegetation data'
         cat >! ftp_veg1 << EOF
         get VEGETATION-SIB.TAR.gz
EOF
         cat ftp_veg1 >> ftp.tmp
      endif

      if ( $?IfLandSurface && ( ! -e SOILCATB.30 ) ) then
         echo 'we need additional data for Land-Surface model'
         cat >! ftp_land1 << EOF
         get SOILCATB.TAR.gz
EOF
         cat ftp_land1 >> ftp.tmp
      endif

      if ( -e ftp_land || -e ftp_land1 || -e ftp_low || -e ftp_lw1 || -e ftp_veg1 ) then

         cat >! ftp_end << EOF
         quit
EOF
         cat ftp_end >> ftp.tmp
         rm -f ftp_end

         cat ftp.tmp
	 echo "ftp low resolution data"
         echo "About to contact $dhost ..."
         echo "ftping low resolution data..."
         ftp -v -n -i $dhost < ftp.tmp >&! ftp.output
         rm -f ftp.tmp ftp.output
         rm -f ftp_land ftp_land1 ftp_low ftp_lw1 ftp_veg1

         echo "untarring and decompressing..."
         if ( -e TER.TAR.gz ) then
             gunzip -c TER.TAR.gz | tar xvf -
             rm -rf TER.TAR.gz
         endif
         if ( -e LANDUSE-OLD.TAR.gz ) then
             gunzip -c LANDUSE-OLD.TAR.gz | tar xvf -
             rm -rf LANDUSE-OLD.TAR.gz
         endif
         if ( -e LANDWATERMASK-USGS.TAR.gz ) then
             gunzip -c LANDWATERMASK-USGS.TAR.gz | tar xvf -
             rm -rf LANDWATERMASK-USGS.TAR.gz
         endif
         if ( -e LANDWATERMASK-SIB.TAR.gz ) then
             gunzip -c LANDWATERMASK-SIB.TAR.gz | tar xvf -
             rm -rf LANDWATERMASK-SIB.TAR.gz
         endif
         if ( -e VEGETATION-USGS.TAR.gz ) then
             gunzip -c VEGETATION-USGS.TAR.gz    | tar xvf -
             rm -rf VEGETATION-USGS.TAR.gz
         endif
         if ( -e VEGETATION-SIB.TAR.gz ) then
             gunzip -c VEGETATION-SIB.TAR.gz    | tar xvf -
             rm -rf VEGETATION-SIB.TAR.gz
         endif
         if ( -e SOILCATB.TAR.gz ) then
             gunzip -c SOILCATB.TAR.gz | tar xvf -
             rm -rf SOILCATB.TAR.gz
         endif
         if ( -e VEG-FRACTION.10.gz ) then
             gunzip VEG-FRACTION.10.gz
         endif
      else
         echo 'no neet to go to ftp site, data required are local'
      endif

   endif
#
   if ( $?If2minData ) then

      if ( ! -e ftp.tmp ) then
         set dhost=ftp.ucar.edu
         cat >! ftp.tmp << EOF
         user anonymous ${USER}@
         bi
         cd mesouser/MM5V3/TERRAIN_DATA
EOF
      endif

      if ( ! -e DEM_02M_GLOBAL ) then
         echo 'ftp 2 min terrain data'
         cat >! ftp_02m << EOF
         get DEM_02M_GLOBAL.gz
EOF
         cat ftp_02m >> ftp.tmp
      endif

      if ( $LandUse == USGS && ( ! -e LWMASK-USGS.02 ) ) then
         echo 'ftp USGS 2 min land-water mask data'
         cat >! ftp_lw2 << EOF
         get LWMASK-USGS.02.gz
EOF
         cat ftp_lw2 >> ftp.tmp
      else if ( $LandUse == SiB && ( ! -e LWMASK-SIB.02 ) ) then
         echo 'ftp SiB 2 min land-water mask data'
         cat >! ftp_lw2 << EOF
         get LWMASK-SIB.02.gz
EOF
         cat ftp_lw2 >> ftp.tmp
      endif
#
      if ( $LandUse == OLD && ( ! -e LANDUSE.10 ) ) then
         echo 'ftp OLD landuse data'
         cat >! ftp_veg2 << EOF
         get LANDUSE-OLD.TAR.gz
EOF
         cat ftp_veg2 >> ftp.tmp

      else if ( $LandUse == USGS && ( ! -e VEG-USGS.02 ) ) then
         echo 'ftp USGS 2 min vegetation data'
         cat >! ftp_veg2 << EOF
         get VEG-USGS.02.gz
EOF
         cat ftp_veg2 >> ftp.tmp

      else if ( $LandUse == SiB && ( ! -e VEG-SIB.02 ) ) then
         echo 'ftp SiB 2 min vegetation data'
         cat >! ftp_veg2 << EOF
         get VEG-SIB.02.gz
EOF
         cat ftp_veg2 >> ftp.tmp

      endif
#
      if ( $?IfLandSurface && ( ! -e SOILCATB.02 ) ) then
         echo 'we need additional data for Land-Surface model'
         cat >! ftp_land2 << EOF
         get SOILCATB.02.gz
EOF
         cat ftp_land2 >> ftp.tmp
      endif

      if ( -e ftp_02m || -e ftp_lw2 || -e ftp_veg2 || -e ftp_land2 ) then

         cat >! ftp_end << EOF
         quit
EOF
         cat ftp_end >> ftp.tmp
         rm -f ftp_end

         cat ftp.tmp
         echo "ftp 2 min resolution data"
         echo "About to contact $dhost ..."
         echo "ftping ..."
         ftp -v -n -i $dhost < ftp.tmp >&! ftp.output
         rm -f ftp.tmp ftp.output
         rm -f ftp_02m ftp_lw2 ftp_veg2 ftp_land2

         echo "untarring and decompressing..."
         if ( -e LANDUSE-OLD.TAR.gz ) then
             gunzip -c LANDUSE-OLD.TAR.gz | tar xvf -
             rm -rf LANDUSE-OLD.TAR.gz
         endif
         if ( -e DEM_02M_GLOBAL.gz ) then
             gunzip DEM_02M_GLOBAL.gz
         endif
         if ( -e LWMASK-USGS.02.gz ) then
             gunzip LWMASK-USGS.02.gz
         endif
         if ( -e LWMASK-SIB.02.gz ) then
             gunzip LWMASK-SIB.02.gz
         endif
         if ( -e VEG-USGS.02.gz ) then
             gunzip VEG-USGS.02.gz
         endif
         if ( -e VEG-SIB.02.gz ) then
             gunzip VEG-SIB.02.gz
         endif
         if ( -e SOILCATB.02.gz ) then
             gunzip SOILCATB.02.gz
         endif
         if ( -e VEG-FRACTION.10.gz ) then
             gunzip VEG-FRACTION.10.gz
         endif
      else
          echo 'no need to go to ftp site, data required are local'
      endif
   endif
#
   if ( $?If30secData ) then

      if ( ! -e ftp.tmp ) then
         set dhost=ftp.ucar.edu
         cat >! ftp.tmp << EOF
         user anonymous ${USER}@
         bi
         cd mesouser/MM5V3/TERRAIN_DATA
EOF
      endif

      if ( $LandUse == USGS && ( ! -e LWMASK-USGS.30s ) ) then
         echo 'ftp USGS 30 sec land-water mask data'
         cat >! ftp_lw3 << EOF
         get LWMASK-USGS.30s.gz
EOF
         cat ftp_lw3 >> ftp.tmp

      else if ( $LandUse == SiB && ( ! -e LWMASK-SIB.30s ) ) then
         echo 'ftp SiB 30 sec land-water mask data'
         cat >! ftp_lw3 << EOF
         get LWMASK-SIB.30s.gz
EOF
         cat ftp_lw3 >> ftp.tmp

      endif
#
      if ( $LandUse == OLD && ( ! -e LANDUSE.10 ) ) then
         echo 'ftp OLD landuse data'
         cat >! ftp_veg3 << EOF
         get LANDUSE-OLD.TAR.gz
EOF
         cat ftp_veg3 >> ftp.tmp

      else if ( $LandUse == USGS && ( ! -e VEG-USGS.30s ) ) then
         echo 'ftp USGS 30 sec land-water mask data'
         cat >! ftp_veg3 << EOF
         get VEG-USGS.30s.gz
EOF
         cat ftp_veg3 >> ftp.tmp

      else if ( $LandUse == SiB && ( ! -e VEG-SIB.30s ) ) then
         echo 'ftp SiB 30 sec land-water mask data'
         cat >! ftp_veg3 << EOF
         get VEG-SIB.30s.gz
EOF
         cat ftp_veg3 >> ftp.tmp

      endif
#
      if ( $?IfLandSurface && ( ! -e SOILCATB.30s ) ) then
         echo 'we need additional data for Land-Surface model'
         cat >! ftp_land3 << EOF
         get SOILCATB.30s.gz
EOF
         cat ftp_land3 >> ftp.tmp
      endif

      if ( -e ftp_lw3 || -e ftp_veg3 || -e ftp_land3 ) then

         cat >! ftp_end << EOF
         quit
EOF
         cat ftp_end >> ftp.tmp
         rm -f ftp_end

         cat ftp.tmp
         echo "ftp 30 sec data"
         echo "About to contact $dhost ..."
         echo "ftping ..."
         ftp -v -n -i $dhost < ftp.tmp >&! ftp.output
         rm -f ftp.tmp ftp.output
         rm -f ftp_lw3 ftp_veg3 ftp_land3

         echo "untarring and decompressing..."
         if ( -e LANDUSE-OLD.TAR.gz ) then
             gunzip -c LANDUSE-OLD.TAR.gz | tar xvf -
             rm -rf LANDUSE-OLD.TAR.gz
         endif
         if ( -e LWMASK-USGS.30s.gz ) then
             gunzip LWMASK-USGS.30s.gz
         endif
         if ( -e LWMASK-SIB.30s.gz ) then
             gunzip LWMASK-SIB.30s.gz
         endif
         if ( -e VEG-USGS.30s.gz ) then
             gunzip VEG-USGS.30s.gz 
         endif
         if ( -e VEG-SIB.30s.gz ) then
             gunzip VEG-SIB.30s.gz
         endif
         if ( -e SOILCATB.30s.gz ) then
             gunzip SOILCATB.30s.gz
         endif
         if ( -e VEG-FRACTION.10.gz ) then
             gunzip VEG-FRACTION.10.gz
         endif
      else 
          echo 'no need to go to ftp site, data required are local'
      endif

   rm -f ftp.tmp

endif
#
# End of IfProcData check
#
