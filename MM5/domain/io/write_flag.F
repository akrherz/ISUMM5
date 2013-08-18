      SUBROUTINE WRITE_FLAG( IUTL, FLAG )                                        WRITE_FLAG.1
      IMPLICIT NONE                                                              WRITE_FLAG.2
      INTEGER IUTL, FLAG                                                         WRITE_FLAG.3
      LOGICAL DM_IONODE                                                          WRITE_FLAG.4
      EXTERNAL DM_IONODE                                                         WRITE_FLAG.5
#ifndef MPP1                                                                     WRITE_FLAG.6
      WRITE (IUTL)FLAG                                                           WRITE_FLAG.7
#else                                                                            WRITE_FLAG.8
      CALL RSL_WRITE_1D_DATA(IUTL,FLAG,1,RSL_INTEGER)                            WRITE_FLAG.9
#endif                                                                           WRITE_FLAG.10
      RETURN                                                                     WRITE_FLAG.11
      END                                                                        WRITE_FLAG.12
                                                                                 WRITE_FLAG.13
