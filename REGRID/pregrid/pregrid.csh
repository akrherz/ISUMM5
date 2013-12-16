#!/bin/csh -f
#  set echo

#
# Put your input files for pregrid into the directory you specify as DataDir:
#

set DataDir = $1

#
# Specify the source of 3-d analyses
#


#   set SRC3D = ON84  # Old ON84-formatted NCEP GDAS analyses
#  set SRC3D = NCEP  # Newer GRIB-formatted NCEP GDAS analyses
  set SRC3D = GRIB  # Many GRIB-format datasets


#  InFiles:  Tell the program where you have put the analysis files, 
#  and what you have called them.   If SRC3D has the value "GRIB", 
#  then the Vtables you specify below in the script variable VT3D will 
#  be used to interpret the files you specify in the ${InFiles} variable.

set InFiles = ( ${DataDir}/*.grib )
#
# Specify the source of SST analyses
#

#  set SRCSST = ON84
#  set SRCSST = NCEP
#  set SRCSST = NAVY
  set SRCSST = $SRC3D

#
#  InSST: Tell the program where the files with SST analyses are.  Do 
#  this only if SST analyses are coming from files not named above in
#  InFiles.  If SRCSST has the value "GRIB", then the Vtables you 
#  specify below in the script variable VTSST will be used to interpret 
#  the files you specify in the ${InSST} variable.
#

  set InSST = ( )

#
# Select the source of snow-cover analyses (entirely optional)
#
  
   set SRCSNOW = $SRC3D
#  set SRCSNOW = ON84
#  set SRCSNOW = GRIB

#  InSnow:  Set InSnow only if the snow-cover analyses are from files 
#  not listed in InFiles.  If SRCSNOW has the value "GRIB", then the 
#  Vtables you specify below in the script variable VTSNOW will be used 
#  to interpret the files you specify in the ${InSnow} variable.

   set InSnow = ()

#
# Select the source of soil model analyses (entirely optional)
#
  
   set SRCSOIL = $SRC3D

#  InSoil:  Set InSoil only if the soil analyses are from files 
#  not listed in InFiles.  If SRCSOIL has the value "GRIB", then the 
#  Vtables you specify below in the script variable VTSOIL will be
#  used to interpret the files you specify in the ${InSoil} variable.

   set InSoil = ()

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
START_YEAR  = $2   # Year (Four digits)
START_MONTH = $3     # Month ( 01 - 12 )
START_DAY   = $4     # Day ( 01 - 31 )
START_HOUR  = $5     # Hour ( 00 - 23 )

END_YEAR  = $6   # Year (Four digits)
END_MONTH = $7     # Month ( 01 - 12 )
END_DAY   = $8     # Day ( 01 - 31 )
END_HOUR  = $9     # Hour ( 00 - 23 )
#
# Define the time interval to process.
#
INTERVAL =  10800 # Time interval (seconds) to process.
		   # This is most sanely the same as the time interval for
                   # which the analyses were archived, but you can really
                   # set this to just about anything, and pregrid will
                   # interpolate in time and/or skip over time periods for
                   # your regridding pleasure.

/
End_Of_Namelist

#
#  Tell the pregrid programs which Vtables to use.  Do this only 
#  if you have selected GRIB-formatted input using SRC___ = GRIB above.
#  The directories referenced here are relative to REGRID/pregrid/.  
#
#  The Vtable files specified in VT3D will be applied to the files
#  specified in the InFiles variable.  Similarly, the Vtable files 
#  specified in VTSST, VTSNOW, and VTSOIL will be applied to the files 
#  listed above in InSST, InSNOW, and InSoil, respectively.
#  
   set VT3D = ( grib.misc/Vtable.AVN3D )
   set VTSST = ( grib.misc/Vtable.AVNSST )
   set VTSNOW = ( grib.misc/Vtable.AVNSNOW )
   set VTSOIL = ( grib.misc/Vtable.AVNSOIL )

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

foreach SourceType ( 3D SST SNOW SOIL) 

  printf "\nProcessing for SourceType = %s\n\n" $SourceType

  if ( ( $SourceType == SOIL ) && ( $SRCSOIL == ON84) ) then
    printf "\n\nSoil fields not available in ON84 Dataset.\n"
    printf "Do not request soil fields or select another source for soil fields.\n\n"
    exit (1)
  endif

  if ( ( $SourceType == SOIL ) && ( $SRCSOIL == NCEP) ) then
    printf "\n\nSoil fields not available in NCEP GDAS Dataset."
    printf "Do not request soil fields or select another source for soil fields.\n\n"
    exit (1)
  endif

###############################################################################

  if ( ( ( $SourceType == 3D   ) && ( $SRC3D   == ON84) ) || \
       ( ( $SourceType == SST  ) && ( $SRCSST  == ON84) ) || \
       ( ( $SourceType == SNOW ) && ( $SRCSNOW == ON84) ) ) then

    printf "\n\nStarting ON84 processing for type %s\n\n" $SourceType 
#
#  Go down to the "on84" directory.
#
    printf "cd %s\n\n" `pwd`/on84
    cd on84
#
#  Remove whatever files may be leftover from a prior job.  Redirect 
#  printout to supress warnings if there is nothing to remove.
#
    rm ON84FILE* >&! /dev/null
    rm PSST:*    >&! /dev/null
    rm PSNOW:*   >&! /dev/null
    rm -f pregrid.namelist
    rm -f Vtable
#
#  Build the Vtable:
#
    touch Vtable
    if ($SourceType == 3D) then
       cat Vtable.ON84 >> Vtable
    else if ($SourceType == SST) then
       cat Vtable.SST >> Vtable
    else if ($SourceType == SNOW) then
       cat Vtable.SNOW >> Vtable
    endif
#
#  Link the requested input files to "ON84FILE.A", "ON84FILE.B", etc.
#
    set Num = 0
    if ($SourceType == 3D) then
       foreach file ( $InFiles )
          @ Num ++
	  printf "       ln -s   %s   %s\n" $file ON84FILE${LETTERS[$Num]}
          ln -s $file ON84FILE${LETTERS[$Num]}
       end
    endif

    if ($SourceType == SST) then
       foreach file ( $InSST )
          @ Num ++
	  printf "       ln -s   %s   %s\n" $file ON84FILE${LETTERS[$Num]}
          ln -s $file ON84FILE${LETTERS[$Num]}
       end
    endif

    if ($SourceType == SNOW) then
       foreach file ( $InSnow )
          @ Num ++
	  printf "       ln -s   %s   %s\n" $file ON84FILE${LETTERS[$Num]}
          ln -s $file ON84FILE${LETTERS[$Num]}
       end
    endif
#
# Link the pregrid.namelist file and run the program.
#
    ln -s ../pregrid.namelist pregrid.namelist
    ./pregrid_on84.exe 
#
# Move the output up to the "pregrid" directory.
#
    if ($SourceType == 3D) then
       foreach file ( ON84:* )
	  printf "       mv %s ../%s\n" $file $file
          mv $file ..
       end
    else if ($SourceType == SST) then
       foreach file ( SST:* )
	  printf "       mv %s ../ON84_%s\n" $file $file
          mv $file ../ON84_$file
       end
    else if ($SourceType == SNOW) then
       foreach file ( SNOW:* )
	  printf "       mv %s ../ON84_%s\n" $file $file
          mv $file ../ON84_$file
       end
    endif
#
# Go back up to the "pregrid" directory.
#
    printf "\ncd %s\n" `pwd`/..
    cd ..

    printf "\nDone with ON84 processing for type %s\n\n" $SourceType 
  endif

###############################################################################

  if ( ( ( $SourceType == 3D   ) && ( $SRC3D   == NCEP) ) || \
       ( ( $SourceType == SST  ) && ( $SRCSST  == NCEP) ) || \
       ( ( $SourceType == SNOW ) && ( $SRCSNOW == NCEP) ) ) then
#
#  Go down to the "ncep.grib" directory.
#
    printf "\ncd %s\n" `pwd`/ncep.grib
    cd ncep.grib
#
#  Remove whatever files may be leftover from a prior job.  Redirect 
#  printout to supress warnings if there is nothing to remove.
#
    rm GRIBFILE*             >&! /dev/null
    rm -f pregrid.namelist
    rm -f Vtable
#
#  Build the Vtable:
#
    touch Vtable
    if ($SourceType == 3D) then
       cat Vtable.NCEP >> Vtable
    else if ($SourceType == SST) then
       cat Vtable.SST >> Vtable
    else if ($SourceType == SNOW) then
       cat Vtable.SNOW >> Vtable
    endif
#
#  Link the requested input files to "GRIBFILE.A", "GRIBFILE.B", etc.
#
    set Num = 0
    if ( $SourceType == 3D ) then
       foreach file ( $InFiles )
          @ Num ++
          ln -s $file GRIBFILE${LETTERS[$Num]}
       end
    endif
    if ($SourceType == SST) then
       foreach file ( $InSST )
          @ Num ++
          ln -s $file GRIBFILE${LETTERS[$Num]}
       end
    endif
    if ($SourceType == SNOW) then
       foreach file ( $InSnow )
          @ Num ++
          ln -s $file GRIBFILE${LETTERS[$Num]}
       end
    endif
#
# Link the pregrid.namelist file and run the program.
#
    ln -s ../pregrid.namelist pregrid.namelist
    ./pregrid_ncep.exe 
#
# Move the output up to the "pregrid" directory.
#
    if ($SourceType == 3D) then
       mv NCEP:* ..
    else if ($SourceType == SST) then
       foreach file ( SST:* )
          mv $file ../NCEP_$file
       end
    else if ($SourceType == SNOW) then 
       foreach file ( SNOW:* )
          mv $file ../NCEP_$file
       end
    endif
#
# Go back up to the "pregrid" directory.
#
    echo "cd `pwd`/.."
    cd ..

  endif

###############################################################################

  if ( ($SourceType == SST) && ( $SRCSST == NAVY ) ) then

    printf "\n\nStarting NAVYSST processing.\n\n"
#
#  Go down to the "navysst" directory.
#
     echo "cd `pwd`/navysst"
     cd navysst
#
#  Remove whatever files may be leftover from a prior job.  Redirect 
#  printout to supress warnings if there is nothing to remove.
#
     rm -f pregrid.namelist
     rm NAVYFILE*        >&! /dev/null
#
#  Link the requested files to "NAVYFILE.A", "NAVYFILE.B", etc.
#
     set Num = 0
     foreach file ( $InSST ) 
        @ Num ++
        ln -s ${file} NAVYFILE${LETTERS[$Num]}
     end
#
# Link the pregrid.namelist file and run the program.
#
     ln -s ../pregrid.namelist pregrid.namelist
     ./pregrid_navy.exe

#
# Move the output files up to the "pregrid" directory.
#
     foreach file ( SST:* )
        mv $file ../NAVY_$file
     end
#
# Go back up to the "pregrid" directory.
#
     echo "cd `pwd`/.."
     cd ..

     printf "\n\nDone with NAVYSST processing.\n\n"

  endif

###############################################################################

  if ( ( ( $SourceType == 3D   ) && ( $SRC3D   == GRIB) ) || \
       ( ( $SourceType == SST  ) && ( $SRCSST  == GRIB) ) || \
       ( ( $SourceType == SNOW ) && ( $SRCSNOW == GRIB) ) || \
       ( ( $SourceType == SOIL ) && ( $SRCSOIL == GRIB) ) ) then

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
     if ( $SourceType == 3D ) then
        foreach file ( $VT3D )
           cat ../$file >> Vtable
        end
     else if ( $SourceType == SST ) then
        foreach file ( $VTSST )
           cat ../$file >> Vtable
        end
     else if ( $SourceType == SNOW ) then
        foreach file ( $VTSNOW )
           cat ../$file >> Vtable
        end
     else if ( $SourceType == SOIL ) then
        foreach file ( $VTSOIL ) 
           cat ../$file >> Vtable
        end
     endif
#
#  Link the requested files to "GRIBFILE.AA", "GRIBFILE.AB", etc.
#
     set NUM = 0
     set num = 1

     if ( $SourceType == 3D ) then
        foreach file ( $InFiles ) 
           @ NUM ++
           if ( $NUM == 27 ) then
              set NUM = 1
              @ num ++
           endif
	   printf "       ln -s   %s   %s\n" $file GRIBFILE.${LETTERS[$num]}${LETTERS[$NUM]}
           ln -s ${file} GRIBFILE.${LETTERS[$num]}${LETTERS[$NUM]}
        end
     else if ( $SourceType == SST ) then
        foreach file ( $InSST ) 
           @ NUM ++
           if ( $NUM == 27 ) then
              set NUM = 1
              @ num ++
           endif
	   printf "       ln -s   %s   %s\n" $file GRIBFILE.${LETTERS[$num]}${LETTERS[$NUM]}
           ln -s ${file} GRIBFILE.${LETTERS[$num]}${LETTERS[$NUM]}
        end
     else if ( $SourceType == SNOW ) then
        foreach file ( $InSnow ) 
           @ NUM ++
           if ( $NUM == 27 ) then
              set NUM = 1
              @ num ++
           endif
	   printf "       ln -s   %s   %s\n" $file GRIBFILE.${LETTERS[$num]}${LETTERS[$NUM]}
           ln -s ${file} GRIBFILE.${LETTERS[$num]}${LETTERS[$NUM]}
        end
     else if ( $SourceType == SOIL ) then
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
     if ( $SourceType == 3D ) then
        mv FILE:* ..
     else
        foreach file ( FILE:* )
	  printf "mv %s   %s\n" $file ../${SourceType}_${file}
          mv $file ../${SourceType}_${file}
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
