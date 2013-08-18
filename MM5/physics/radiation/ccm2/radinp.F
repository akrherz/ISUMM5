                                                                                 RADINP.1
C---------------------------------------------------------------------           RADINP.2
C---------------------------------------------------------------------           RADINP.3
      SUBROUTINE RADINP(PMID    ,PINT    ,H2OMMR ,CLD   ,                        RADINP.4
     $                  PLOS    ,PMIDRD  ,PINTRD  ,PLCO2  ,PLH2O ,               RADINP.5
     $                  TCLRSF  ,ECCF    ,O3MMR)                                 RADINP.6
C                                                                                RADINP.7
C SETS LATITUDE AND TIME DEPENDENT ARRAYS FOR INPUT TO RADIATION                 RADINP.8
C                                                                                RADINP.9
C WORKS ON A LONGITUDE/HEIGHT SLICE OF DATA FOR EACH LATITUDE BAND               RADINP.10
C                                                                                RADINP.11
C COMPUTES SOLAR INPUT (EARTH-SUN DISTANCE FACTOR AND COSINE SOLAR               RADINP.12
C ZENITH ANGLE), CONVERTS MODEL PRESSURES TO CGS, COMPUTES                       RADINP.13
C PATH LENGTH ARRAYS NEEDED FOR THE LONGWAVE RADIATION CODE,                     RADINP.14
C AND COMPUTES OZONE MIXING RATIO.                                               RADINP.15
C                                                                                RADINP.16
C     INCLUDE 'PARAME'                                                           RADINP.17
C     INCLUDE 'RADCCM2.DATA'                                                     RADINP.18
#     include <parame.incl>                                                      RADINP.19
#     include <parccm2.incl>                                                     RADINP.20
#     include <radccm2.incl>                                                     RADINP.21
#ifdef MPP1                                                                      RADINP.22
#     include <various.incl>                                                     RADINP.23
#endif                                                                           RADINP.24
C                                                                                RADINP.25
C INPUT ARGUMENTS                                                                RADINP.26
C                                                                                RADINP.27
      REAL PMID(IPLOND,IPLEV), ! PRESSURE AT MODEL MID-LEVELS (PASCALS)          RADINP.28
     $   PINT(IPLOND,IPLEVP), ! PRESSURE AT MODEL INTERFACES (PASCALS)           RADINP.29
     $     H2OMMR(IPLOND,IPLEV), ! MODEL MOISTURE FIELD                          RADINP.30
     $     CLD(IPLOND,IPLEVP),   ! FRACTIONAL CLOUD COVER                        RADINP.31
     $     PLOS(IPLOND,IPLEVP)   ! O3 PATH LENGTH (CM)                           RADINP.32
C                                                                                RADINP.33
C OUTPUT ARGUMENTS                                                               RADINP.34
C                                                                                RADINP.35
      REAL PMIDRD(IPLOND,IPLEV), ! PRESSUREATMODELMID-LEVELS(DYNES/CM*           RADINP.36
     $ PINTRD(IPLOND,IPLEVP),! PRESSURE AT MODEL INTERFACES (DYNES/CM*           RADINP.37
     $ PLCO2(IPLOND,IPLEVP), ! VERT. PTH LNGTH OF CO2 (PRS-WEIGHTED)             RADINP.38
     $ PLH2O(IPLOND,IPLEVP), ! VERT. PTH LNGTH OF H2O VAP. (PRS-WEIGHT           RADINP.39
     $ TCLRSF(IPLOND,IPLEVP) ! PRODUCT OF CLR-SKY FRACTIONS FROM TOP             RADINP.40
C                              ! OF ATMOS. TO LVL K.                             RADINP.41
      REAL ECCF,               ! EARTH-SUN DISTANCE FACTOR                       RADINP.42
     $     O3MMR(IPLOND,IPLEV)   ! OZONE MASS MIXING RATIO                       RADINP.43
C                                                                                RADINP.44
C LOCAL VARIABLES                                                                RADINP.45
C                                                                                RADINP.46
      INTEGER   I,  ! LONGITUDE LOOP INDEX                                       RADINP.47
     $          K   ! VERTICAL LOOP INDEX                                        RADINP.48
C                                                                                RADINP.49
      REAL    PHI,  ! GREENWICH CALENDAR DAY + LOCAL TIME + LONGITUDE OF         RADINP.50
     $      THETA,  ! EARTH ORBIT SEASONAL ANGLE IN RADIANS                      RADINP.51
     $      DELTA,  ! SOLAR DECLINATION ANGLE  IN RADIANS                        RADINP.52
     $       SINC,  ! SINE   OF LATITUDE                                         RADINP.53
     $       COSC,  ! COSINE OF LATITUDE                                         RADINP.54
     $       SIND,  ! SINE   OF DECLINATION                                      RADINP.55
     $       COSD,  ! COSINE OF DECLINATION                                      RADINP.56
     $         V0,  ! VOLUME OF A GAS AT STP (CM**3/MOL)                         RADINP.57
     $         P0,  ! STANDARD PRESSURE (DYNES/CM**2)                            RADINP.58
     $        AMD,  ! EFFECTIVE MOLECULAR WEIGHT OF DRY AIR (G/MOL)              RADINP.59
     $        AMO,  ! MOLECULAR WEIGHT OF OZONE (G/MOL)                          RADINP.60
     $      AMCO2,  ! MOLECULAR WEIGHT OF CO2   (G/MOL)                          RADINP.61
     $        CPL,  ! CONSTANT IN OZONE PATH LENGTH TO MIXING RATIO              RADINP.62
     $      CPWPL,  ! CONSTANT IN CO2 MIXING RATIO TO PATH LENGTH CONVER         RADINP.63
     $       VMMR   ! OZONE VOLUME MIXING RATIO                                  RADINP.64
C                                                                                RADINP.65
      DATA V0    /  22413.6   /                                                  RADINP.66
      DATA P0    /  1.01325E6 /                                                  RADINP.67
      DATA AMD   /  28.9644   /                                                  RADINP.68
      DATA AMO   /  48.0000   /                                                  RADINP.69
      DATA AMCO2 /  44.0000   /                                                  RADINP.70
C                                                                                RADINP.71
                                                                                 RADINP.72
#ifdef MPP1                                                                      RADINP.73
#  define IPLON I  LX                                                            RADINP.74
#  define IPLOND I  LX                                                           RADINP.75
#endif                                                                           RADINP.76
C                                                                                RADINP.77
C COMPUTE SOLAR DISTANCE FACTOR AND COSINE SOLAR ZENITH ANGLE USI                RADINP.78
C DAY VALUE WHERE A ROUND DAY (SUCH AS 213.0) REFERS TO 0Z AT                    RADINP.79
C GREENWICH LONGITUDE.                                                           RADINP.80
C                                                                                RADINP.81
      ECCF=1.                                                                    RADINP.82
C                                                                                RADINP.83
C CONVERT PRESSURE FROM PASCALS TO DYNES/CM2                                     RADINP.84
C                                                                                RADINP.85
      DO 30 K=1,IPLEV                                                            RADINP.86
        DO 20 I=1,IPLON                                                          RADINP.87
          PMIDRD(I,K)=PMID(I,K)*10.0                                             RADINP.88
          PINTRD(I,K)=PINT(I,K)*10.0                                             RADINP.89
   20   CONTINUE                                                                 RADINP.90
   30 CONTINUE                                                                   RADINP.91
      DO 35 I=1,IPLON                                                            RADINP.92
        PINTRD(I,IPLEVP)=PINT(I,IPLEVP)*10.0                                     RADINP.93
   35 CONTINUE                                                                   RADINP.94
C                                                                                RADINP.95
C COMPUTE PATH QUANTITIES USED IN THE LONGWAVE RADIATION:                        RADINP.96
C                                                                                RADINP.97
      VMMR=AMCO2/AMD                                                             RADINP.98
      CPWPL=VMMR*0.5/(GRAVIT*P0)                                                 RADINP.99
      DO 40 I=1,IPLON                                                            RADINP.100
        PLH2O(I,1)=RGSSLP*H2OMMR(I,1)*PINTRD(I,1)*PINTRD(I,1)                    RADINP.101
        PLCO2(I,1)=CO2VMR*CPWPL*PINTRD(I,1)*PINTRD(I,1)                          RADINP.102
        TCLRSF(I,1)=1.                                                           RADINP.103
   40 CONTINUE                                                                   RADINP.104
      DO 50 K=1,IPLEV                                                            RADINP.105
        DO 55 I=1,IPLON                                                          RADINP.106
          PLH2O(I,K+1)=PLH2O(I,K)+RGSSLP*(PINTRD(I,K+1)**2-PINTRD(I,K)**         RADINP.107
     +                 2)*H2OMMR(I,K)                                            RADINP.108
          PLCO2(I,K+1)=CO2VMR*CPWPL*PINTRD(I,K+1)**2                             RADINP.109
          TCLRSF(I,K+1)=TCLRSF(I,K)*(1.-CLD(I,K+1))                              RADINP.110
   55   CONTINUE                                                                 RADINP.111
   50 CONTINUE                                                                   RADINP.112
C                                                                                RADINP.113
C COMPUTE OZONE MIXING RATIO FROM PATH LENGTHS:                                  RADINP.114
C                                                                                RADINP.115
C CONSTANTS FOR FOLLOWING SUMS:                                                  RADINP.116
C                                                                                RADINP.117
      CPL=V0/(AMD*GRAVIT)                                                        RADINP.118
      VMMR=AMD/AMO                                                               RADINP.119
C                                                                                RADINP.120
      DO 60 K=1,IPLEV                                                            RADINP.121
        DO 65 I=1,IPLON                                                          RADINP.122
C                                                                                RADINP.123
          O3MMR(I,K)=(PLOS(I,K+1)-PLOS(I,K))/(CPL*VMMR*(PINTRD(I,K+1)-           RADINP.124
     +               PINTRD(I,K)))                                               RADINP.125
C                                                                                RADINP.126
   65   CONTINUE                                                                 RADINP.127
   60 CONTINUE                                                                   RADINP.128
C                                                                                RADINP.129
C DONE                                                                           RADINP.130
C                                                                                RADINP.131
      RETURN                                                                     RADINP.132
      END                                                                        RADINP.133
                                                                                 RADINP.134
