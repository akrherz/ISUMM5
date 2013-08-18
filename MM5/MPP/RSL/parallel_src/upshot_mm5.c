
#ifndef T3E
# ifdef NOUNDERSCORE
mm5_initlog()
# else
mm5_initlog_()
# endif
#else
MM5_INITLOG()
#endif
{
#ifdef UPSHOT
  MPE_Init_log() ;
  MPE_Describe_state( 20, 30, "A", "violet") ;
  MPE_Describe_state( 21, 31, "B", "orange") ;
  MPE_Describe_state( 22, 32, "C", "red") ;
  MPE_Describe_state( 23, 33, "D", "yellow") ;
  MPE_Describe_state( 24, 34, "E", "brown") ;
  MPE_Describe_state( 101, 201, "SA", "grey") ;
  MPE_Describe_state( 102, 202, "SB", "black") ;
  MPE_Describe_state( 103, 203, "SC", "grey") ;
  MPE_Describe_state( 104, 204, "PK", "violet") ;
  MPE_Describe_state( 105, 205, "UPK", "black") ;
/* this next state is defined within RSL itself */
  MPE_Describe_state( 15, 16, "Stencil", "green:vlines3") ;
#endif
}

#ifndef T3E
# ifdef NOUNDERSCORE
mm5_finishlog()
# else
mm5_finishlog_()
# endif
#else
MM5_FINISHLOG()
#endif
{
#ifdef UPSHOT
  MPE_Finish_log("mm5_log") ;
#endif
}

#ifndef T3E
# ifdef NOUNDERSCORE
mm5_logevent(e)
# else
mm5_logevent_(e)
# endif
#else
MM5_LOGEVENT(e)
#endif
  int * e ;
{
#ifdef UPSHOT
  MPE_Log_event( *e, 0, "event" ) ;
#endif
}

