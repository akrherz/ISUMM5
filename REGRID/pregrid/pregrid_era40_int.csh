#!/bin/csh -f
#  set echo

#
# Put your input files for pregrid into the directory you specify as DataDir:
#

set DataDir = /user/REGRID


#
# ERA 40 Invariant data is stored in GRIB format
#
  
    set SRCSOIL = GRIB    

#  InSoil:  Set InSoil only if the soil analyses are from files 
#  not listed in InFiles.  If SRCSOIL has the value "GRIB", then the 
#  Vtables you specify below in the script variable VTSOIL will be
#  used to interpret the files you specify in the ${InSoil} variable.

    set InSoil = (  ${DataDir}/U01487 )

#
#  Build the Namelist
#
if ( -e ./pregrid.namelist ) then
   rm ./pregrid.namelist
endif
cat << End_Of_Namelist | sed -e 's/#.*//; s/  *$//' > ./pregrid.namelist
&record1
#
# Set the starting date of the time period you want to process:
#
 START_YEAR  = 1957   # Year (Four digits)
 START_MONTH = 09     # Month ( 01 - 12 )
 START_DAY   = 01     # Day ( 01 - 31 )
 START_HOUR  = 12     # Hour ( 00 - 23 )

 END_YEAR  = 1957   # Year (Four digits)
 END_MONTH = 09     # Month ( 01 - 12 )
 END_DAY   = 01     # Day ( 01 - 31 )
 END_HOUR  = 12     # Hour ( 00 - 23 )
#
# Define the time interval to process.
#
 INTERVAL =  43200 # Time interval (seconds) to process.
		   # This is most sanely the same as the time interval for
                   # which the analyses were archived, but you can really
                   # set this to just about anything, and pregrid will
                   # interpolate in time and/or skip over time periods for
                   # your regridding pleasure.

/
End_Of_Namelist

#
   set VTSOIL = ( grib.misc/Vtable.ERA4SIVT )

########################################################################
########################################################################
######                                                            ######
######                  END USER MODIFICATION                     ######
######                                                            ######
########################################################################
########################################################################

if ( ! $?SRC3D ) then
   set SRC3D
endif
if ( ! $?SRCSST ) then
   set SRCSST
endif
if ( ! $?SRCSNOW ) then
   set SRCSNOW
endif
if ( ! $?SRCSOIL ) then
   set SRCSOIL
endif
if ( ! $?VTSOIL ) then
   set VTSOIL
endif
if ( ! $?VTSNOW ) then
   set VTSNOW
endif
if ( ! $?VTSST ) then
   set VTSST
endif
if ( ! $?VT3D ) then
   set VT3D
endif
if ( ! $?InFiles ) then
   set InFiles = ()
endif
if ( ! $?InSST ) then
   set InSST = ()
endif
if ( ! $?InSnow ) then
   set InSnow = ()
endif
if ( ! $?InSoil ) then
   set InSoil = ()
endif

if ( $SRCSST == $SRC3D) then
   if ( $#InSST == 0 ) then
      set InSST = ( ${InFiles} )
   endif
endif

if ( $SRCSNOW == $SRC3D) then
   if ( $#InSnow == 0 ) then
      set InSnow = ( ${InFiles} )
   endif
endif

if ( $SRCSOIL == $SRC3D) then
   if ( $#InSoil == 0 ) then
      set InSoil = ( ${InFiles} )
   endif
endif

set LETTERS = ( A B C D E F G H I J K L M N O P Q R S T U V W X Y Z )

foreach SourceType (SOIL) 

  printf "\nProcessing for SourceType = %s\n\n" $SourceType

###############################################################################

  if ( ( ( $SourceType == SOIL ) && ( $SRCSOIL == GRIB) ) ) then

     printf "\n\nStarting GRIB processing for type %s\n\n" $SourceType 

#
#  Go down to the "grib.misc" directory.
#
     echo "cd `pwd`/grib.misc"
     cd grib.misc
#
#  Remove whatever files may be leftover from a prior job.  Redirect 
#  printout to supress warnings if there is nothing to remove.
#
     rm FILE:*              >&! /dev/null
     rm GRIBFILE*           >&! /dev/null
     rm -f Vtable
     rm -f pregrid.namelist
#
#  Build the Vtable:
#
     touch Vtable
     if ( $SourceType == SOIL ) then
        foreach file ( $VTSOIL ) 
           cat ../$file >> Vtable
        end
     endif
#
#  Link the requested files to "GRIBFILE.AA", "GRIBFILE.AB", etc.
#
     set NUM = 0
     set num = 1

     if ( $SourceType == SOIL ) then
        foreach file ( $InSoil ) 
           @ NUM ++
           if ( $NUM == 27 ) then
              set NUM = 1
              @ num ++
           endif
	   printf "       ln -s   %s   %s\n" $file GRIBFILE.${LETTERS[$num]}${LETTERS[$NUM]}
           ln -s ${file} GRIBFILE.${LETTERS[$num]}${LETTERS[$NUM]}
        end
     endif
#
# Link the pregrid.namelist file and run the program.
#
     ln -s ../pregrid.namelist pregrid.namelist
     ./pregrid_grib.exe
#
# Move the output files up to the "pregrid" directory.
#
     if ( $SourceType == SOIL ) then
        foreach file ( FILE:* )
	  printf "mv %s   %s\n" $file ../${SourceType}_${file}
          mv $file ../SIVT_${file}
        end
     endif
#
# Go back to the "pregrid" directory.
#
     echo "cd `pwd`/.."
     cd ..

     printf "\n\nDone with GRIB processing for type %s\n\n" $SourceType 
   endif

#
# Print out five lines of # as a delimiter between ${SourceType}s
#
   repeat 5 printf \
"################################################################################\n"

end
printf "\n"
