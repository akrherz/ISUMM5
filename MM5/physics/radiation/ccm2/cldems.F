C-----------------------------------------------------------------------         CLDEMS.1
C-----------------------------------------------------------------------         CLDEMS.2
      SUBROUTINE CLDEMS(CLWP,CIWP,CONWP,EMIS)                                    05DEC01.1319
C                                                                                CLDEMS.4
C COMPUTE CLOUD EMISSIVITY USING CLOUD LIQUID WATER PATH (G/M**2)                CLDEMS.5
C                                                                                CLDEMS.6
C     INCLUDE 'PARAME'                                                           CLDEMS.7
C     INCLUDE 'RADCCM2.DATA'                                                     CLDEMS.8
#     include <parame.incl>                                                      CLDEMS.9
#     include <parccm2.incl>                                                     CLDEMS.10
#     include <radccm2.incl>                                                     CLDEMS.11
#ifdef MPP1                                                                      CLDEMS.12
#     include <various.incl>                                                     CLDEMS.13
#endif                                                                           CLDEMS.14
C-----------------------------------------------------------------------         CLDEMS.15
                                                                                 CLDEMS.16
                                                                                 CLDEMS.17
C                                                                                CLDEMS.18
C INPUT ARGUMENT                                                                 CLDEMS.19
C                                                                                CLDEMS.20
      REAL CLWP(IPLOND,IPLEV)    ! CLOUD LIQUID WATER PATH (G/M**2)              CLDEMS.21
      REAL CIWP(IPLOND,IPLEV)    ! CLOUD ICE WATER PATH (G/M**2)                 05DEC01.1320
      REAL CONWP(IPLOND,IPLEV)   ! CONVECTIVE CLOUD WATER PATH (G/M**2)          05DEC01.1321
C                                                                                CLDEMS.22
C OUTPUT ARGUMENT                                                                CLDEMS.23
C                                                                                CLDEMS.24
      REAL EMIS(IPLOND,IPLEV)    ! CLOUD EMISSIVITY (FRACTION)                   CLDEMS.25
C                                                                                CLDEMS.26
C LOCAL WORKSPACE                                                                CLDEMS.27
C                                                                                CLDEMS.28
      INTEGER          I,  ! LONGITUDE INDEX                                     CLDEMS.29
     $                 K   ! LEVEL INDEX                                         CLDEMS.30
C                                                                                CLDEMS.31
      REAL KABS         ! LONGWAVE ABSORPTION COEFFICIANT (M**2/G)               CLDEMS.32
      DATA KABS  / 0.1 /                                                         CLDEMS.33
C                                                                                CLDEMS.34
C                                                                                CLDEMS.35
      DO 40 K=1,IPLEV                                                            CLDEMS.36
#ifndef MPP1                                                                     CLDEMS.37
        DO 30 I=1,IPLON                                                          CLDEMS.38
#else                                                                            CLDEMS.39
        DO 30 I=1,ILX                                                            CLDEMS.40
#endif                                                                           CLDEMS.41
          EMIS(I,K)=1.-(EXP(-KABS*(CLWP(I,K)+CONWP(I,K)))*                       05DEC01.1322
     +              EXP(-0.0735*CIWP(I,K)))                                      05DEC01.1323
   30   CONTINUE                                                                 CLDEMS.43
                                                                                 CLDEMS.44
   40 CONTINUE                                                                   CLDEMS.45
C                                                                                CLDEMS.46
      RETURN                                                                     CLDEMS.47
      END                                                                        CLDEMS.48
                                                                                 CLDEMS.49
