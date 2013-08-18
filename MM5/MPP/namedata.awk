# awk script to read in the file namelist.data and convert
# into packing and unpacking routines for fread_config_data.F
# 5/21/96, michalak@mcs.anl.gov

# Here is some sample input to this program (note that the # characters
# in column 1 should be removed):

##---beginning of sample output---
## data that must be read in from the namelist and
## distributed to the other processors
## name        type    ndim     dims
#ibats           i       0
#ixtimr          i       0
#endstep         i       0
#levidn          i       1       maxnes
#inportnum       i       1       maxnes
#outportnum      i       1       maxnes
#ibltyp          i       1       maxnes
#diftim          r       2       maxnes  2
#dpsmx           r       0
#iwind           i       2       maxnes  2
#iwinds          i       2       maxnes  2
#npfg            i       0
#ifrest          l       0
#ifsave          l       0
##--- end of sample input data ---

# Here is output from the sample:

#---beginning of sample output---
#% awk -f namedata.awk DIR=b PACKFILE=foo DECLFILE=bar xyz
#% cat foo
#C
#C THIS FILE WAS AUTOMATICALLY GENERATED
#C CHANGES TO THIS FILE MAY BE LOST
#C
#      ibats = ibuf( icurs )
#      icurs = icurs + 1
#      ixtimr = ibuf( icurs )
#      icurs = icurs + 1
#      endstep = ibuf( icurs )
#      icurs = icurs + 1
#      do ipack1=1,maxnes
#      levidn(ipack1) = ibuf( icurs )
#      icurs = icurs + 1
#      enddo
#      do ipack1=1,maxnes
#   ...etc...
#% cat bar
#C
#C THIS FILE WAS AUTOMATICALLY GENERATED
#C CHANGES TO THIS FILE MAY BE LOST
#C
#      integer icurs, ibuflen
#      parameter (ibuflen=
#     $ 4
#     $ +(4*maxnes)
#     $ +(2*2*maxnes)
#     $ )
#      integer ibuf( ibuflen )
#C
#      integer rcurs, rbuflen
#      parameter (rbuflen=
#     $ (1*2*maxnes)
#     $ +1
#     $ )
#      real rbuf( rbuflen )
#C
#      integer lcurs, lbuflen
#      parameter (lbuflen=
#     $ 2
#     $ )
#      logical lbuf( lbuflen )
#C
#--- end of sample output ---
############################################################

BEGIN { initial = 1 }

{ 
  if ( initial == 1 )
  {
    initial = 0
    if ( PACKFILE == "" )
    {
      print "PACKFILE must be specified.   Eg:"
      err = "yes"
    }
    if ( DECLFILE == "" )
    {
      print "DECLFILE must be specified.   Eg:"
      err = "yes"
    }
    if ( DIR != "f" && DIR != "b" && DIR != "F" && DIR != "B")
    {
      print "DIR must be specified (f or b) eg:"
      err = "yes"
    }
    if ( err == "yes" )
    {
      print "   awk -f file.awk DIR=b PACKFILE=file DECLFILE=file list"
      exit
    }
    print "C"   > PACKFILE
    print "C THIS FILE WAS AUTOMATICALLY GENERATED"   > PACKFILE
    print "C CHANGES TO THIS FILE MAY BE LOST"   > PACKFILE
    print "C"   > PACKFILE
    print "      icurs = 1"  > PACKFILE
    print "      rcurs = 1"  > PACKFILE
    print "      lcurs = 1"  > PACKFILE
    print "C"   > DECLFILE
    print "C THIS FILE WAS AUTOMATICALLY GENERATED"   > DECLFILE
    print "C CHANGES TO THIS FILE MAY BE LOST"   > DECLFILE
    print "C"   > DECLFILE
  }
}

{ndim = $3}

{
  if ( ndim > 0 )
  {
    for ( i = ndim ; i > 0 ; i-- )
    {
      print "      do ipack" i "=1,"$(i+3)  > PACKFILE
      if ( i == ndim )
        lstr = $(i+3)
      else
        lstr = lstr "*" $(i+3)
    }
  }
  else
  {
    lstr = 1
  }
  if ( ranges[$2 lstr] == 0 )
    rstrs[ nrstrs++ ] = $2 lstr ;
  ranges[ $2 lstr ]++ ;
  if ( types[ $2 ] == 0 )
    tstrs[ ntstrs++ ] = $2 ;
  types[ $2 ]++ ;

  if ( DIR == "f"  || DIR == "F" )
  {
    printf( "      %sbuf( %scurs ) = %s", $2, $2, $1 )  > PACKFILE
    for ( i = 1 ; i <= ndim ; i++ )
    {
      if ( i == 1 ) 
        printf("(ipack1") > PACKFILE ;
      else
        printf(",ipack%d",i) > PACKFILE ;
    }
    if ( ndim > 0 ) printf(")") > PACKFILE ;
    printf("\n") > PACKFILE ;
  }
  else if ( DIR == "b" || DIR == "B" )
  {
    printf( "      %s", $1 )  > PACKFILE
    for ( i = 1 ; i <= ndim ; i++ )
    {
      if ( i == 1 ) 
        printf("(ipack1") > PACKFILE ;
      else
        printf(",ipack%d",i) > PACKFILE ;
    }
    if ( ndim > 0 ) printf(")") > PACKFILE ;
    printf( " = %sbuf( %scurs )\n", $2, $2 )  > PACKFILE
  }
  else
  {
    print "DIR must be specified (f or b) eg:"
    print "   awk -f file.awk DIR=b PACKFILE=file list"
    exit
  }

  print "      " $2 "curs = " $2 "curs + 1" > PACKFILE

  for ( i = ndim ; i > 0 ; i-- )
  {
    print "      enddo" > PACKFILE
  }

}

END {
  print "      integer ipack1,ipack2,ipack3,ipack4,ipack5,ipack6,ipack7" > DECLFILE
  for ( j = 0 ; j < ntstrs ; j++ )
  {
    printf( "      integer %scurs, %sbuflen\n", tstrs[j], tstrs[j] ) > DECLFILE
    printf( "      parameter (%sbuflen=\n", tstrs[j] ) > DECLFILE
    first = 1
    for ( i = 0 ; i < nrstrs ; i++ )
    {
      t1 = substr( rstrs[i] , 1, 1 )
      if ( t1 == tstrs[j] )
      {
        r1 = substr( rstrs[i] , 2 )
        if ( first == 1 )
        {
          d = "     $ "
          first = 0 
        }
        else
          d = "     $ +"
        if ( r1 == "1" )
          print d ranges[ rstrs[ i ] ]  > DECLFILE
        else
          print d "(" ranges[ rstrs[ i ] ] "*" r1 ")"  > DECLFILE
      }
    }
    print "     $ )"  > DECLFILE
    if      ( tstrs[j] == "i" || tstrs[j] == "I" )
      tt = "integer"
    else if ( tstrs[j] == "r" || tstrs[j] == "R" )
      tt = "real"
    else if ( tstrs[j] == "l" || tstrs[j] == "L" )
      tt = "logical"
    else if ( tstrs[j] == "d" || tstrs[j] == "D" )
      tt = "double precision"
    else if ( tstrs[j] == "c" || tstrs[j] == "C" )
      tt = "complex"
    else
      tt = "(unknown: " tstrs[j] ")"
    printf( "      %s %sbuf( %sbuflen )\n", tt, tstrs[j], tstrs[j] ) > DECLFILE
    print "C" > DECLFILE
  }
}
