#! /bin/csh -f

if ( ${#argv} != 1 ) then
 echo "usage: makedeck.csh RUNTIME_SYSTEM"
 exit 0
endif

#################################################
# TROUBLESHOOTING:
#   On Linux Redhat 4.2 I've occassionally seen
#   problems with the "while" loop below (that is,
#   a problem with the csh).  If you encounter
#   such a problem, changing "#! /bin/csh -f" to
#   "#! /bin/tcsh -f" will probably solve it.
#################################################

set RUNTIME_SYSTEM = $1

if ( "$RUNTIME_SYSTEM" == "SGI_R8000" || "$RUNTIME_SYSTEM" == "SGI_Origin" ) then 
  echo "Making mm5.deck for SGI" 
  set sufx = "sgi"
else if ( "$RUNTIME_SYSTEM" == "SGI_R4400" ) then 
  echo "Making mm5.deck for SGI"; 
  set sufx = "sgi"
else if ( "$RUNTIME_SYSTEM" == "SGI_R4000" ) then 
  echo "Making mm5.deck for SGI"; 
  set sufx = "sgi"
else if ( "$RUNTIME_SYSTEM" == "CRAY_IA" ) then 
  set sufx = "cray-ia-bsh"
  echo "Making mm5.deck for interactive Cray job"; 
else if ( "$RUNTIME_SYSTEM" == "CRAY" ) then 
  echo "Making mm5.deck for Cray"; 
  set sufx = "cray"
else if ( "$RUNTIME_SYSTEM" == "PC_PGF77" ) then 
  echo "Making mm5.deck for Linux using PG F77 compiler"; 
  set sufx = "linuxPGF77"
else if ( "$RUNTIME_SYSTEM" == "SUN" ) then 
  echo "Making mm5.deck for Sun"; 
  set sufx = "sun"
else if ( "$RUNTIME_SYSTEM" == "DEC_ALPHA" ) then 
  echo "Making mm5.deck for DEC"; 
  set sufx = "dec"
else if ( "$RUNTIME_SYSTEM" == "alpha" ) then 
  echo "Making mm5.deck for DEC"; 
  set sufx = "decmpp"
else if ( "$RUNTIME_SYSTEM" == "IBM" ) then 
  echo "Making mm5.deck for IBM"; 
  set sufx = "ibm"
else if ( "$RUNTIME_SYSTEM" == "HP" ) then 
  echo "Making mm5.deck for HP"; 
  set sufx = "hp"
else if ( "$RUNTIME_SYSTEM" == "HP-SPP" ) then 
  echo "Making mm5.deck for HP-SPP"; 
  set sufx = "hp-batch"
else if ( "$RUNTIME_SYSTEM" == "hp" ) then 
  echo "Making mm5.deck for HP MPP"; 
  set sufx = "hp-mpp"
else if ( "$RUNTIME_SYSTEM" == "HP-SPP_IA" ) then 
  echo "Making mm5.deck for HP-SPP_IA"; 
  set sufx = "hp"
else if ( "$RUNTIME_SYSTEM" == "sp2" ) then 
  echo "Making mm5.deck for IBM MPP"
  set sufx = "ibm-mpp"
# set sufx = "ibm-omp"
else if ( "$RUNTIME_SYSTEM" == "vpp" ) then 
  echo "Making mm5.deck for MPP"
  set sufx = "mpp"
else if ( "$RUNTIME_SYSTEM" == "t3e" ) then 
  echo "Making mm5.deck for MPP"
  set sufx = "mpp"
else if ( "$RUNTIME_SYSTEM" == "alpha" ) then 
  echo "Making mm5.deck for MPP"
  set sufx = "mpp"
else if ( "$RUNTIME_SYSTEM" == "linux" ) then 
  echo "Making mm5.deck for MPP"
  set sufx = "mpp"
else if ( "$RUNTIME_SYSTEM" == "macxlf" ) then 
  echo "Making mm5.deck for MAC"
  set sufx = "macxlf"
else
  echo "The RUNTIME_SYSTEM variable in configure.user is INCORRECT"
  if ( -e mm5.deck ) rm mm5.deck
  exit 1
endif

set deckname = ./Templates/mm5.deck.${sufx}
echo "Using template file $deckname"

if ( -e mm5.deck.${sufx}.tmp ) rm mm5.deck.${sufx}.tmp
set noglob
@ nl = 1  
while (1)     
  # read a line from the file      
  set f = "`sed -n $nl,${nl}p $deckname`"
  if ("$f" == "#ENDTEMPLATE" ) break
  set itest=`echo $f | awk '{ print $1 }'`
  if ("$itest" == "#INCLUDE" ) then
    set itest=`echo $f | awk '{ print $2 }'`
    if ( -e ./Templates/${itest} ) then
      echo "Including file ./Templates/$itest"
      cat ./Templates/${itest} >> mm5.deck.${sufx}.tmp
    else
      echo "Could not find include file ./Templates/$itest"
      exit 0
    endif
  else
    echo "$f" >> mm5.deck.${sufx}.tmp
  endif
  @ nl++
end
set glob

cp mm5.deck.${sufx}.tmp mm5.deck
chmod +x mm5.deck

