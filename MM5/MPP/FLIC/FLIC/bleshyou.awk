BEGIN {latch = 0}
/C_FLIC_BEGIN_NOFLIC/ { latch = 1 }
{ 
  if ( latch == 1 )
  {
    print "CFLICBYE " $0
  }
  else
  {
    print $0
  }
}
/C_FLIC_END_NOFLIC/ { latch = 0 }
