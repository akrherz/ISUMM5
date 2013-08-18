#! /bin/csh -f
#
# This script is the prototype making a cleaner configure.user
# include file as well as customizing a system for a known
# OS.
#
# Over time we will add various operating systems.
# SGI:     R4400 chip, IRIX 6.2
#
# Written by Dan Hansen, 1997

setenv cf configure.user
if ( -e configure.user) then
  if ( -e configure.user.backup) then
    ;
  else 
    echo "Backing up configure.user to configure.user.backup..."
    cp ./configure.user ./configure.user.backup
  endif
else 
  echo "I do not see the configure.user file. \
Are you running this script from the top-level MM5 directory? \
If not, cd to the MM5 directory and run ./Util/makeconfigure.csh."
exit 0
endif

set os = `uname`
if ($os == IRIX) then
  set rel = `uname -r`
  if ($rel == 6.2) then
    hinv | grep 4400 >! /dev/null
    if ($status == 0) then
      echo "---> We think that this is an SGI with an R4400 chip and IRIX 6.2 <---"
    else
      hinv | grep 5000 >! /dev/null
      if ($status == 0) then
        echo "---> We think that this is an SGI with an R5000 chip and IRIX 6.2 <---"
      else
        goto unknownos
      endif
    endif
#===================================
cat >! configure.sgi << EOF
5,7d
10,14d
s/RUNTIME_SYSTEM = "CRAY"/RUNTIME_SYSTEM = "SGI_R4000"/
36,79d
s/#FC/FC/
s/#CF/CF/
s/#CP/CP/
s/#LD/LD/
s/#LOCAL_LIB/LOCAL_LIB/
s/#MAKE/MAKE/
91,160d
EOF
#===================================
    sed -f configure.sgi $cf > ${cf}.tmp
    goto ok
  else
    goto unknownos
  endif
else
  goto unknownos
endif
#
unknownos:
  echo "We have not implemented a branch for this OS yet... \
you will have to edit the configure.user file by hand. \
If you modify this script for your particular OS, please send \
the diffs to mesouser@ucar.edu so that others can profit from \
your work."
exit 0

ok:
 echo "We have done some basic modifications to the configure.user \
file.  You will still want to look it over and make such personal \
modifications as you find desireable or necessary. \
The backup copy of your original configure.user file is configure.user.backup. \
Thank you for flying MM5 V2 and have a safe forecast."
mv ${cf}.tmp ${cf}
exit 0
