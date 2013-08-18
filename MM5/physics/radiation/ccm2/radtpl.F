C-----------------------------------------------------------------------         RADTPL.1
C-----------------------------------------------------------------------         RADTPL.2
      SUBROUTINE RADTPL(TNM    ,TG     ,QNM    ,PBR    ,PNM    ,                 RADTPL.3
     $                  PLH2O  ,TPLNKA ,S2C    ,S2T    ,W      ,                 RADTPL.4
     $                  TPLNKE ,TINT   ,TINT4  ,TLAYR  ,TLAYR4 ,                 RADTPL.5
     $                  PMLN   ,PILN   )                                         RADTPL.6
C                                                                                RADTPL.7
C COMPUTE TEMPERATURES AND PATH LENGTHS FOR LONGWAVE RADIATION                   RADTPL.8
C                                                                                RADTPL.9
C     INCLUDE 'PARAME'                                                           RADTPL.10
C     INCLUDE 'RADCCM2.DATA'                                                     RADTPL.11
#     include <parame.incl>                                                      RADTPL.12
#     include <parccm2.incl>                                                     RADTPL.13
#     include <radccm2.incl>                                                     RADTPL.14
#ifdef MPP1                                                                      RADTPL.15
#     include <various.incl>                                                     RADTPL.16
#endif                                                                           RADTPL.17
C-----------------------------------------------------------------------         RADTPL.18
C                                                                                RADTPL.19
C INPUT ARGUMENTS                                                                RADTPL.20
C                                                                                RADTPL.21
      REAL TNM(IPLOND,IPLEV),     ! MODEL LEVEL TEMPERATURES                     RADTPL.22
     $     TG(IPLOND),           ! SURFACE SKIN TEMPERATURE                      RADTPL.23
     $     QNM(IPLOND,IPLEV),     ! MODEL LEVEL SPECIFIC HUMIDITY                RADTPL.24
     $     PBR(IPLOND,IPLEV),     ! PRSR AT MODEL MID-LEVELS (DYNES/CM2)         RADTPL.25
     $     PNM(IPLOND,IPLEVP),    ! PRSR AT MODEL INTERFACES (DYNES/CM2)         RADTPL.26
     $     PLH2O(IPLOND,IPLEVP)   ! PRS WGHTD H2O PATH                           RADTPL.27
C                                                                                RADTPL.28
C OUTPUT ARGUMENTS                                                               RADTPL.29
C                                                                                RADTPL.30
      REAL TPLNKA(IPLOND,IPLEVP), ! LEVEL TMP FROM INTERFACE TMPS                RADTPL.31
     $     S2C(IPLOND,IPLEVP),    ! H2O CONTINUUM PATH LENGTH                    RADTPL.32
     $     S2T(IPLOND,IPLEVP),    ! H2O TMP AND PRS WGHTD PATH LENGTH            RADTPL.33
     $     W(IPLOND,IPLEVP),      ! H2O PRS WGHTD PATH LENGTH                    RADTPL.34
     $     TPLNKE(IPLOND),       ! EQUAL TO TPLNKA                               RADTPL.35
     $     TINT(IPLOND,IPLEVP),   ! LAYER INTERFACE TEMPERATURE                  RADTPL.36
     $     TINT4(IPLOND,IPLEVP),  ! TINT TO THE 4TH POWER                        RADTPL.37
     $     TLAYR(IPLOND,IPLEVP),  ! K-1 LEVEL TEMPERATURE                        RADTPL.38
     $     TLAYR4(IPLOND,IPLEVP), ! TLAYR TO THE 4TH POWER                       RADTPL.39
     $     PMLN(IPLOND,IPLEV),    ! LN(PMIDM1)                                   RADTPL.40
     $     PILN(IPLOND,IPLEVP)    ! LN(PINTM1)                                   RADTPL.41
C                                                                                RADTPL.42
C LOCAL VARIABLES                                                                RADTPL.43
C                                                                                RADTPL.44
      INTEGER   I,   ! LONGITUDE INDEX                                           RADTPL.45
     $          K    ! LEVEL INDEX                                               RADTPL.46
                                                                                 RADTPL.47
      REAL R296,     ! INVERSE STANDARD TEMPERATURE FOR H2O CONTINUUM            RADTPL.48
     $     REPSIL,   ! INVERSE RATIO MOL WEIGHT H2O TO DRY AIR                   RADTPL.49
     $         DY,   ! THICKNESS OF LAYER FOR TMP INTERPOLATION                  RADTPL.50
     $       DPNM,   ! PRESSURE THICKNESS OF LAYER                               RADTPL.51
     $     DPNMSQ,   ! PRS SQUARED DIFFERENCE ACROSS LAYER                       RADTPL.52
     $       RTNM    ! INVERSE LEVEL TEMPERATURE                                 RADTPL.53
C                                                                                RADTPL.54
#ifdef MPP1                                                                      RADTPL.55
#  define IPLON I  LX                                                            RADTPL.56
#  define IPLOND I  LX                                                           RADTPL.57
#endif                                                                           RADTPL.58
C                                                                                RADTPL.59
      R296=1./296.                                                               RADTPL.60
      REPSIL=1./EPSILO                                                           RADTPL.61
C                                                                                RADTPL.62
C SET THE TOP AND BOTTOM INTERMEDIATE LEVEL TEMPERATURES,                        RADTPL.63
C TOP LEVEL PLANCK TEMPERATURE AND TOP LAYER TEMP**4.                            RADTPL.64
C                                                                                RADTPL.65
C TINT IS LOWER INTERFACE TEMPERATURE                                            RADTPL.66
C (NOT AVAILABLE FOR BOTTOM LAYER, SO USE GROUND TEMPERATURE)                    RADTPL.67
C                                                                                RADTPL.68
      DO 30 I=1,IPLON                                                            RADTPL.69
        TINT(I,IPLEVP)=TG(I)                                                     RADTPL.70
        TINT4(I,IPLEVP)=TINT(I,IPLEVP)**4                                        RADTPL.71
        TPLNKA(I,1)=TNM(I,1)                                                     RADTPL.72
        TINT(I,1)=TPLNKA(I,1)                                                    RADTPL.73
        TLAYR4(I,1)=TPLNKA(I,1)**4                                               RADTPL.74
        TINT4(I,1)=TLAYR4(I,1)                                                   RADTPL.75
   30 CONTINUE                                                                   RADTPL.76
C                                                                                RADTPL.77
C INTERMEDIATE LEVEL TEMPERATURES ARE CALCULATED BASED ON THE TEM                RADTPL.78
C AT THE FULL LEVEL BELOW LESS DY*DELTA T (BETWEEN THE FULL LEVEL                RADTPL.79
C                                                                                RADTPL.80
      DO 50 K=2,IPLEV                                                            RADTPL.81
        DO 40 I=1,IPLON                                                          RADTPL.82
          DY=(PILN(I,K)-PMLN(I,K))/(PMLN(I,K-1)-PMLN(I,K))                       RADTPL.83
          TINT(I,K)=TNM(I,K)-DY*(TNM(I,K)-TNM(I,K-1))                            RADTPL.84
          TINT4(I,K)=TINT(I,K)**4                                                RADTPL.85
   40   CONTINUE                                                                 RADTPL.86
   50 CONTINUE                                                                   RADTPL.87
C                                                                                RADTPL.88
C NOW SET THE LAYER TEMP=FULL LEVEL TEMPERATURES AND ESTABLISH A                 RADTPL.89
C PLANCK TEMPERATURE FOR ABSORPTION (TPLNKA) WHICH IS THE AVERAGE                RADTPL.90
C THE INTERMEDIATE LEVEL TEMPERATURES.  NOTE THAT TPLNKA IS NOT                  RADTPL.91
C EQUAL TO THE FULL LEVEL TEMPERATURES.                                          RADTPL.92
C                                                                                RADTPL.93
      DO 70 K=2,IPLEVP                                                           RADTPL.94
        DO 60 I=1,IPLON                                                          RADTPL.95
          TLAYR(I,K)=TNM(I,K-1)                                                  RADTPL.96
          TLAYR4(I,K)=TLAYR(I,K)**4                                              RADTPL.97
          TPLNKA(I,K)=.5*(TINT(I,K)+TINT(I,K-1))                                 RADTPL.98
   60   CONTINUE                                                                 RADTPL.99
   70 CONTINUE                                                                   RADTPL.100
C                                                                                RADTPL.101
C CALCULATE TPLANK FOR EMISSIVITY CALCULATION.                                   RADTPL.102
C ASSUME ISOTHERMAL TPLNKE I.E. ALL LEVELS=TTOP.                                 RADTPL.103
C                                                                                RADTPL.104
      DO 80 I=1,IPLON                                                            RADTPL.105
        TPLNKE(I)=TPLNKA(I,1)                                                    RADTPL.106
        TLAYR(I,1)=TINT(I,1)                                                     RADTPL.107
   80 CONTINUE                                                                   RADTPL.108
C                                                                                RADTPL.109
C NOW COMPUTE H2O PATH FIELDS:                                                   RADTPL.110
C                                                                                RADTPL.111
      DO 90 I=1,IPLON                                                            RADTPL.112
        S2T(I,1)=PLH2O(I,1)*TNM(I,1)                                             RADTPL.113
        W(I,1)=(PLH2O(I,1)*2.)/PNM(I,1)                                          RADTPL.114
        S2C(I,1)=PLH2O(I,1)*QNM(I,1)*REPSIL                                      RADTPL.115
   90 CONTINUE                                                                   RADTPL.116
      DO 110 K=1,IPLEV                                                           RADTPL.117
        DO 100 I=1,IPLON                                                         RADTPL.118
          DPNM=PNM(I,K+1)-PNM(I,K)                                               RADTPL.119
          DPNMSQ=PNM(I,K+1)**2-PNM(I,K)**2                                       RADTPL.120
          RTNM=1./TNM(I,K)                                                       RADTPL.121
          S2T(I,K+1)=S2T(I,K)+RGSSLP*DPNMSQ*QNM(I,K)*TNM(I,K)                    RADTPL.122
          W(I,K+1)=W(I,K)+RGA*QNM(I,K)*DPNM                                      RADTPL.123
          S2C(I,K+1)=S2C(I,K)+RGSSLP*DPNMSQ*QNM(I,K)*                            RADTPL.124
     +               EXP(1800.*(RTNM-R296))*QNM(I,K)*REPSIL                      RADTPL.125
  100   CONTINUE                                                                 RADTPL.126
  110 CONTINUE                                                                   RADTPL.127
      RETURN                                                                     RADTPL.128
      END                                                                        RADTPL.129
                                                                                 RADTPL.130
