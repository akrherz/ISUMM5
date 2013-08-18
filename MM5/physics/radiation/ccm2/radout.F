CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC             RADOUT.1
C-----------------------------------------------------------------------         RADOUT.2
      SUBROUTINE RADOUT(SOLIN  ,SABTP  ,FRSA   ,CLRST  ,CLRSS  ,                 RADOUT.3
     $                  QRS    ,FIRTP  ,FRLA   ,CLRLT  ,CLRLS  ,                 RADOUT.4
     $                  QRL, SLWD  ,SRFRAD, JSLC,IUTRAD,NRADISP,                 RADOUT.5
     $                  KTAUW, O3MMR, H2OMMR, CLD, CLWP)                         RADOUT.6
C                                                                                RADOUT.7
C COPY RADIATION OUTPUT QUANTITIES TO MODEL BUFFER                               RADOUT.8
C                                                                                RADOUT.9
C CHANGE UNITS OF THE RADIATIVE FLUXES FROM CGS TO MKS                           RADOUT.10
C                                                                                RADOUT.11
C COMPUTE THE TOTAL RADIATIVE HEAT FLUX AT THE SURFACE FOR                       RADOUT.12
C THE SURFACE TEMPERATURE COMPUTATION                                            RADOUT.13
C                                                                                RADOUT.14
C     INCLUDE 'PARAME'                                                           RADOUT.15
C     INCLUDE 'RADCCM2.DATA'                                                     RADOUT.16
C     INCLUDE 'BATS.CB'                                                          RADOUT.17
C     INCLUDE 'BATS.CB2'                                                         RADOUT.18
C     INCLUDE 'RAD.CB'                                                           RADOUT.19
#     include <parame.incl>                                                      RADOUT.20
#     include <parccm2.incl>                                                     RADOUT.21
#     include <radccm2.incl>                                                     RADOUT.22
#     include <radiat.incl>                                                      RADOUT.23
#     include <various.incl>                                                     RADOUT.24
C      COMMON /VEGRAD/ SABVEG(IPLOND),SOLIS(IPLOND),ALBDIR(IPLOND)               RADOUT.25
C     $   ,ALBDIF(IPLOND),ALBVS(IPLOND),ALBVL(IPLOND),COSZRS(IPLOND)             RADOUT.26
C     $   ,ALDIRS(IPLOND),ALDIRL(IPLOND),ALDIFS(IPLOND),ALDIFL(IPLOND)           RADOUT.27
C     $   ,ALBVSD(IPLOND),ALBVLD(IPLOND)                                         RADOUT.28
C                                                                                RADOUT.29
C INPUT/OUTPUT ARGUMENTS                                                         RADOUT.30
C                                                                                RADOUT.31
      REAL SOLIN(IPLOND),     ! INSTANTANEOUS INCIDENT SOLAR                     RADOUT.32
     $     SABTP(IPLOND),     ! TOP OUTWARD SOLAR FLUX                           07NOV00.2181
     $     FRSA(IPLOND),      ! SURFACE ABSORBED SOLAR FLUX                      RADOUT.34
     $     CLRST(IPLOND),     ! CLEAR SKY TOTAL COLUMN ABS SOLAR FLUX            RADOUT.35
     $     CLRSS(IPLOND),     ! CLEAR SKY SURFACE ABSORBED SOLAR FLUX            RADOUT.36
     $     QRS(IPLOND,IPLEV),  ! SOLAR HEATING RATE                              RADOUT.37
     $     FIRTP(IPLOND),     ! UPWARD LW FLX AT MODEL TOP                       07NOV00.2182
     $     FRLA(IPLOND),      ! LONGWAVE COOLING OF SURFACE (UP-DWN FLX)         RADOUT.39
     $     CLRLT(IPLOND),     ! CLR SKY NET UP FLX TOP OF MODEL (UP-DWN          RADOUT.40
     $     CLRLS(IPLOND),     ! CLR SKY LW COOLING OF SRF (UP-DWN FLX)           RADOUT.41
     $     QRL(IPLOND,IPLEV),  ! LONGWAVE COOLING RATE                           RADOUT.42
     $     SLWD(IPLOND),      ! SURFACE LONGWAVE DOWN FLUX                       RADOUT.43
     $     SRFRAD(IPLOND),     ! SURFACE RADIATIVE HEAT FLUX (FRSA-FRLA)         RADOUT.44
     $     O3MMR(IPLOND,IPLEV), ! OZONE MIXING RATIO                             RADOUT.45
     $     H2OMMR(IPLOND,IPLEV), ! WATER VAPOR MIXING RATIO                      RADOUT.46
     $     CLD(IPLOND,IPLEVP),   ! CLOUD FRACTIONAL COVER                        RADOUT.47
     $     CLWP(IPLOND,IPLEV)  ! CLOUD LIQUID WATER PATH                         RADOUT.48
C                                                                                RADOUT.49
C LOCAL VARIABLES                                                                RADOUT.50
C                                                                                RADOUT.51
      INTEGER       I    ! LONGITUDE INDEX                                       RADOUT.52
C                                                                                RADOUT.53
      REAL CGSMKS     ! CONVERSION FACTOR FOR FLUXES FROM CGS TO MKS             RADOUT.54
C                                                                                RADOUT.55
      DATA CGSMKS / 1.E-3 /                                                      RADOUT.56
C                                                                                RADOUT.57
                                                                                 RADOUT.58
#ifdef MPP1                                                                      RADOUT.59
#  define IPLON I  LX                                                            RADOUT.60
#  define IPLOND I  LX                                                           RADOUT.61
#endif                                                                           RADOUT.62
C COMPUTE TOTAL RADIATIVE HEATING FLUX FOR THE SURFACE,                          RADOUT.63
C CONVERTING UNITS FROM CGS TO MKS:                                              RADOUT.64
C                                                                                RADOUT.65
      DO 10 I=1,IPLON                                                            RADOUT.66
        SRFRAD(I)=(FRSA(I)-FRLA(I))*CGSMKS                                       RADOUT.67
   10 CONTINUE                                                                   RADOUT.68
C                                                                                RADOUT.69
C CONVERT UNITS FROM CGS TO MKS IN SOLAR FLUXES:                                 RADOUT.70
C                                                                                RADOUT.71
      DO 20 I=1,IPLON                                                            RADOUT.72
        SOLIN(I)=SOLIN(I)*CGSMKS                                                 RADOUT.73
        SABTP(I)=SABTP(I)*CGSMKS                                                 RADOUT.74
        FRSA(I)=FRSA(I)*CGSMKS                                                   RADOUT.75
        CLRST(I)=CLRST(I)*CGSMKS                                                 RADOUT.76
        CLRSS(I)=CLRSS(I)*CGSMKS                                                 RADOUT.77
   20 CONTINUE                                                                   RADOUT.78
C                                                                                RADOUT.79
C CONVERT UNITS FROM CGS TO MKS IN LONGWAVE FLUXES:                              RADOUT.80
C                                                                                RADOUT.81
      DO 30 I=1,IPLON                                                            RADOUT.82
        FIRTP(I)=FIRTP(I)*CGSMKS                                                 RADOUT.83
        FRLA(I)=FRLA(I)*CGSMKS                                                   RADOUT.84
        CLRLT(I)=CLRLT(I)*CGSMKS                                                 RADOUT.85
        CLRLS(I)=CLRLS(I)*CGSMKS                                                 RADOUT.86
   30 CONTINUE                                                                   RADOUT.87
C------                                                                          RADOUT.88
C------TOTAL HEATING RATE IN DEG/S                                               RADOUT.89
C------                                                                          RADOUT.90
      DO 19 NLL=1,IPLEV                                                          RADOUT.91
        DO 19 N=1,IPLOND                                                         RADOUT.92
          RTTEN(N,JSLC,NLL)=QRS(N,NLL)+QRL(N,NLL)                                RADOUT.93
   19   CONTINUE                                                                 RADOUT.94
                                                                                 RADOUT.95
C------                                                                          RADOUT.96
C      PRINT * ,"QRS"    , QRS(2,2),QRL(2,2)                                     RADOUT.97
C      PRINT * ,"HEATRT" , RTTEN(2,JSLC,2)                                       RADOUT.98
C      PRINT * ,"FRLA"   , FRLA(2),FRLA(3)                                       RADOUT.99
C      PRINT * ,"FRSA"   , FRSA(2),FRSA(3)                                       RADOUT.100
C------SURFACE ABSORBED SOLAR FLUX IN WATTS/M2                                   RADOUT.101
C------                                                                          RADOUT.102
C     DO 24 N=1,IPLOND                                                           RADOUT.103
C      FSW2D(N,JSLC)=FRSA(N)                                                     RADOUT.104
C24    CONTINUE                                                                  RADOUT.105
C------                                                                          RADOUT.106
C------NET UP LONGWAVE FLUX AT THE SURFACE                                       RADOUT.107
C------                                                                          RADOUT.108
C     DO 27 N=1,IPLOND                                                           RADOUT.109
C      FLW2D(N,JSLC)=FRLA(N)                                                     RADOUT.110
C27    CONTINUE                                                                  RADOUT.111
C------                                                                          RADOUT.112
C------FOR COUPLING WITH BATS                                                    RADOUT.113
C------                                                                          RADOUT.114
C   FOR NOW ASSUME SABVEG (SOLAR ABSORBED BY VEGETATION) IS EQUAL                RADOUT.115
C   TO FRSA (SOLAR ABSORBED BY SURFACE). POSSIBLE PROBLEMS ARE                   RADOUT.116
C   OVER SPARSELY VEGETATED AREAS IN WHICH VEGETATION AND GROUND                 RADOUT.117
C   ALBEDO ARE SIGNIFICANTLY DIFFERENT                                           RADOUT.118
C     DO 44 N=1,IPLOND                                                           RADOUT.119
C      SABV2D(N,JSLC)=SABVEG(N)                                                  RADOUT.120
C      SOL2D(N,JSLC)=SOLIS(N)                                                    RADOUT.121
C      SINC2D(N,JSLC)=SOLIN(N)                                                   RADOUT.122
C44    CONTINUE                                                                  RADOUT.123
C                                                                                RADOUT.124
C     IF((MOD(KTAU+1,NRADISP).EQ.0).OR.(KTAU.EQ.0)) THEN                         RADOUT.125
C      KTAUO = KTAU + 1                                                          RADOUT.126
C      WRITE(IUTRAD)KTAUO,O3MMR,H2OMMR,CLD,CLWP,QRS,QRL,FRSA,FRLA,               RADOUT.127
C    1             CLRST,CLRSS,CLRLT,CLRLS                                       RADOUT.128
C      IF(JSLC.EQ.2) PRINT *, 'WRITING RAD FIELDS AT KTAU = ', KTAUO             RADOUT.129
C     ENDIF                                                                      RADOUT.130
      RETURN                                                                     RADOUT.131
      END                                                                        RADOUT.132
                                                                                 RADOUT.133
