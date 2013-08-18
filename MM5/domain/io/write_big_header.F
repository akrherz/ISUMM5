      SUBROUTINE WRITE_BIG_HEADER( IUNIT )                                       WRITE_BIG_HEADER.1
      IMPLICIT NONE                                                              WRITE_BIG_HEADER.2
#include <hdrv3.incl>                                                            WRITE_BIG_HEADER.3
      INTEGER IUNIT                                                              WRITE_BIG_HEADER.4
      LOGICAL DM_IONODE                                                          WRITE_BIG_HEADER.5
      EXTERNAL DM_IONODE                                                         WRITE_BIG_HEADER.6
C                                                                                WRITE_BIG_HEADER.7
#ifndef MPP1                                                                     WRITE_BIG_HEADER.8
      WRITE (IUNIT)JBHI,JBHR,JBHIC,JBHRC                                         WRITE_BIG_HEADER.9
#else                                                                            WRITE_BIG_HEADER.10
C NOTE THAT ON CERTAIN MACHINES (T3E, ALPHA) JBHI AND JBHR MAY                   24SEP99.293
C BE CHANGED/DESTROYED.  RSL SHOULD BE MODIFIED TO MAKE THIS SAFE                24SEP99.294
C BUT RIGHT NOW, THESE ARE NOT REUSED ANYWAY, SO OKAY FOR NOW (v3.1)             24SEP99.295
      CALL RSL_WRITE_MM5V3_BIG_HEADER(                                           WRITE_BIG_HEADER.11
     +           IUNIT,                                                          WRITE_BIG_HEADER.12
     +           JBHI,NUMINT*NUMPROGS,                                           WRITE_BIG_HEADER.13
     +           JBHR,NUMREAL*NUMPROGS,                                          WRITE_BIG_HEADER.14
     +           JBHIC,80*NUMINT*NUMPROGS,                                       WRITE_BIG_HEADER.15
     +           JBHRC,80*NUMREAL*NUMPROGS,                                      WRITE_BIG_HEADER.16
     +           IWORDSIZE,RWORDSIZE )                                           WRITE_BIG_HEADER.17
#endif                                                                           WRITE_BIG_HEADER.18
      RETURN                                                                     WRITE_BIG_HEADER.19
      END                                                                        WRITE_BIG_HEADER.20
                                                                                 WRITE_BIG_HEADER.21
