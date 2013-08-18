C---------------------------------------------------------------------           RADCTL.1
C-----------------------------------------------------------------------         RADCTL.2
      SUBROUTINE RADCTL(TS     ,                                                 RADCTL.3
     $                  TG     ,PSURF ,PMID   ,PINT   ,PMLN   ,                  RADCTL.4
     $                  PILN   ,T      ,H2OMMR ,CLD    ,EFFCLD ,                 RADCTL.5
     $                  CLWP,CIWP,CONWP,PLOL,PLOS,SOLIN,SABTP  ,FRSA ,           05DEC01.1487
     $                  CLRST  ,CLRSS  ,QRS    ,FIRTP  ,FRLA ,                   RADCTL.7
     $                  CLRLT  ,CLRLS  ,QRL    ,SRFRAD ,                         RADCTL.8
     $                  JSLC,KTAUW,IUTRAD,NRADISP)                               RADCTL.9
C                                                                                RADCTL.10
C MAIN COMPUTATIONAL ENTRY FOR RADIATION COMPUTATION                             RADCTL.11
C                                                                                RADCTL.12
C COMPUTATIONS DONE FOR ONE LATITUDE LINE                                        RADCTL.13
C                                                                                RADCTL.14
C RADIATION USES CGS UNITS, SO CONVERSIONS MUST BE DONE FROM                     RADCTL.15
C MODEL FIELDS TO RADIATION FIELDS                                               RADCTL.16
C                                                                                RADCTL.17
C                                                                                RADCTL.18
C CALLING SEQUENCE:                                                              RADCTL.19
C                                                                                RADCTL.20
C     .                                                                          RADCTL.21
C                                                                                RADCTL.22
C    RADINI          INITIALIZES RADIATION CONSTANTS, OZONE,                     RADCTL.23
C                    AND SURFACE DATA                                            RADCTL.24
C     .                                                                          RADCTL.25
C                                                                                RADCTL.26
C    RADOZ1          UPDATES OZONE AND DOES TIME INTERPOLATION                   RADCTL.27
C                                                                                RADCTL.28
C     .                                                                          RADCTL.29
C                                                                                RADCTL.30
C    PHYS            CALLS PHYSICS ROUTINES                                      RADCTL.31
C                                                                                RADCTL.32
C      RADCTL        INTERFACE FOR RADIATION SCHEME                              RADCTL.33
C                                                                                RADCTL.34
C        RADINP      CONVERTS UNITS OF MODEL FIELDS AND COMPUTES OZONE           RADCTL.35
C                    MIXING RATIO FOR SOLAR SCHEME                               RADCTL.36
C                                                                                RADCTL.37
C        RADCSW      PERFORMS SOLAR COMPUTATION                                  RADCTL.38
C                                                                                RADCTL.39
C          RADALB    COMPUTES SURFACE ALBEDOS (NOT IN MM4)                       RADCTL.40
C          RADDED    COMPUTES DELTA-EDDINGTON SOLUTION                           RADCTL.41
C          RADCLR    COMPUTES DIAGNOSTIC CLEAR SKY FLUXES                        RADCTL.42
C                                                                                RADCTL.43
C        RADCLW      PERFORMS LONGWAVE COMPUTATION                               RADCTL.44
C                                                                                RADCTL.45
C          RADTPL    COMPUTES PATH QUANTITIES                                    RADCTL.46
C          RADEMS    COMPUTES EMISSIVITY                                         RADCTL.47
C          RADABS    COMPUTES ABSORPTIVITY                                       RADCTL.48
C                                                                                RADCTL.49
C        RADOUT      CONVERTS RADIATION FLUXES TO MKS UNITS; COMPUTES            RADCTL.50
C                    SURFACE RADIATIVE FLUX FOR SURFACE COMPUTATIONS             RADCTL.51
C                                                                                RADCTL.52
C                                                                                RADCTL.53
C     INCLUDE 'PARAME'                                                           RADCTL.54
C     INCLUDE 'RADCCM2.DATA'                                                     RADCTL.55
#     include <parame.incl>                                                      RADCTL.56
#     include <param3.incl>                                                      RADCTL.57
#     include <parccm2.incl>                                                     RADCTL.58
#     include <radccm2.incl>                                                     RADCTL.59
#     include <various.incl>                                                     RADCTL.60
C-----------------------------------------------------------------------         RADCTL.61
C                                                                                RADCTL.62
C                                                                                RADCTL.63
C INPUT ARGUMENTS                                                                RADCTL.64
C                                                                                RADCTL.65
      REAL TS(IPLOND),          ! SURFACE AIR TEMPERATURE                        RADCTL.66
     $     TG(IPLOND),          ! SURFACE (SKIN) TEMPERATURE                     RADCTL.67
     $     PSURF(IPLOND),          ! SURFACE PRESSURE                            RADCTL.68
     $     PMID(IPLOND,IPLEV),   ! MODEL LEVEL PRESSURES                         RADCTL.69
     $     PINT(IPLOND,IPLEVP),  ! MODEL INTERFACE PRESSURES                     RADCTL.70
     $     PMLN(IPLOND,IPLEV),   ! NATURAL LOG OF PMID                           RADCTL.71
     $     PILN(IPLOND,IPLEVP),  ! NATURAL LOG OF PINT                           RADCTL.72
     $     T(IPLOND,IPLEV),      ! MODEL LEVEL TEMPERATURES                      RADCTL.73
     $     H2OMMR(IPLOND,IPLEV), ! MODEL LEVEL SPECIFIC HUMIDITY                 RADCTL.74
     $     CLD(IPLOND,IPLEVP),   ! FRACTIONAL CLOUD COVER                        RADCTL.75
     $     EFFCLD(IPLOND,IPLEVP),! EFFECTIVE FRACTIONAL CLOUD COVER              RADCTL.76
     $     CLWP(IPLOND,IPLEV),   ! CLOUD LIQUID WATER PATH                       RADCTL.77
     $     CIWP(IPLOND,IPLEV),   ! CLOUD ICE WATER PATH                          05DEC01.1488
     $     CONWP(IPLOND,IPLEV),  ! CONVECTIVE CLOUD ICE WATER PATH               05DEC01.1489
     $     PLOL(IPLOND,IPLEVP),  ! O3 PRESSURE WEIGHTED PATH LENGTHS (CM         RADCTL.78
     $     PLOS(IPLOND,IPLEVP)   ! O3 PATH LENGTHS (CM)                          RADCTL.79
C                                                                                RADCTL.80
C OUTPUT ARGUMENTS                                                               RADCTL.81
C                                                                                RADCTL.82
C OUTPUT SOLAR                                                                   RADCTL.83
C                                                                                RADCTL.84
      REAL SOLIN(IPLOND),      ! SOLAR INCIDENT FLUX                             RADCTL.85
     $     SABTP(IPLOND),      ! TOP OUTWARD SOLAR FLUX                          07NOV00.2179
     $     FRSA(IPLOND),       ! SURFACE ABSORBED SOLAR FLUX                     RADCTL.87
     $     CLRST(IPLOND),      ! CLR SKY TOTAL COLUMN ABS SOLAR FLUX             RADCTL.88
     $     CLRSS(IPLOND),      ! CLR SKY SURFACE ABS SOLAR FLUX                  RADCTL.89
     $     QRS(IPLOND,IPLEV)    ! SOLAR HEATING RATE                             RADCTL.90
C                                                                                RADCTL.91
C OUTPUT LONGWAVE                                                                RADCTL.92
C                                                                                RADCTL.93
      REAL FIRTP(IPLOND),      ! UPWARD LW FLX AT MODEL TOP                      07NOV00.2180
     $     FRLA(IPLOND),       ! SRF LONGWAVE COOLING (UP-DWN) FLUX              RADCTL.95
     $     CLRLT(IPLOND),      ! CLR SKY LW FLX AT MODEL TOP                     RADCTL.96
     $     CLRLS(IPLOND),      ! CLR SKY LW FLX AT SRF (UP-DWN)                  RADCTL.97
     $     QRL(IPLOND,IPLEV),   ! LONGWAVE COOLING RATE                          RADCTL.98
     $     SLWD(IPLOND)        ! SRF DOWN LONGWAVE FLUX                          RADCTL.99
C                                                                                RADCTL.100
C SURFACE RADIATIVE HEATING                                                      RADCTL.101
C                                                                                RADCTL.102
      REAL SRFRAD(IPLOND)       ! SRF RADIATIVE HEAT FLUX                        RADCTL.103
C                                                                                RADCTL.104
C LOCAL WORKSPACE                                                                RADCTL.105
C                                                                                RADCTL.106
      REAL PBR(IPLOND,IPLEV),    ! MODEL MID-LEVEL PRESSURES (DYNES/CM2)         RADCTL.107
     $     PNM(IPLOND,IPLEVP),   ! MODEL INTERFACE PRESSURES (DYNES/CM2)         RADCTL.108
     $     O3MMR(IPLOND,IPLEV),  ! OZONE MASS MIXING RATIO                       RADCTL.109
     $     PLCO2(IPLOND,IPLEVP), ! PRS WEIGHTED CO2 PATH                         RADCTL.110
     $     PLH2O(IPLOND,IPLEVP), ! PRS WEIGHTED H2O PATH                         RADCTL.111
     $     TCLRSF(IPLOND,IPLEVP),! TOTAL CLEAR SKY FRACTION, LEVEL TO SP         RADCTL.112
     $     COSZRS(IPLOND),       ! COS SOLAR ZENITH ANGLE                        RADCTL.113
     $     ECCF                ! EARTH/SUN DISTANCE FACTOR                       RADCTL.114
C                                                                                RADCTL.115
C      EXTERNAL RADINP,         ! COMPUTES LATITUDE DEPENDENT RADIATION          RADCTL.116
C     $         RADCSW,         ! COMPUTES SOLAR RADIATION                       RADCTL.117
C     $         RADCLW,         ! COMPUTES LONGWAVE RADIATION                    RADCTL.118
C     $         RADOUT          ! SETS RADIATION OUTPUT AND CONVERTS UNI         RADCTL.119
C                                                                                RADCTL.120
C                                                                                RADCTL.121
C                                                                                RADCTL.122
C                                                                                RADCTL.123
C SET LATITUDE DEPENDENT RADIATION INPUT                                         RADCTL.124
C                                                                                RADCTL.125
      CALL RADINP(PMID,PINT,H2OMMR,CLD,PLOS,PBR,PNM,PLCO2,PLH2O,TCLRSF,          RADCTL.126
     +     ECCF,O3MMR)                                                           RADCTL.127
C                                                                                RADCTL.128
C SOLAR RADIATION COMPUTATION                                                    RADCTL.129
C                                                                                RADCTL.130
      CALL ZENITM(JSLC,COSZRS)                                                   RADCTL.131
      CALL RADCSW(COSZRS,PNM,H2OMMR,CLD,CLWP,CIWP,CONWP,O3MMR,ECCF,              05DEC01.1490
     +     SOLIN,QRS,FRSA,SABTP,CLRSS,CLRST,KTAUW,JSLC)                          05DEC01.1491
C                                                                                RADCTL.134
C                                                                                RADCTL.135
C LONGWAVE RADIATION COMPUTATION                                                 RADCTL.136
C                                                                                RADCTL.137
      CALL RADCLW(JSLC,TG,PLOL,PLOS,T,H2OMMR,PBR,PNM,PMLN,PILN,PLCO2,            RADCTL.138
     +     PLH2O,EFFCLD,TCLRSF,QRL,FRLA,FIRTP,CLRLS,CLRLT,SLWD)                  RADCTL.139
C                                                                                RADCTL.140
C SET OUTPUT RADIATION FIELDS                                                    RADCTL.141
C                                                                                RADCTL.142
      CALL RADOUT(SOLIN,SABTP,FRSA,CLRST,CLRSS,QRS,FIRTP,FRLA,CLRLT,             RADCTL.143
     +     CLRLS,QRL,SLWD,SRFRAD,JSLC,IUTRAD,NRADISP,KTAUW,O3MMR,H2OMMR,         RADCTL.144
     +     CLD,CLWP)                                                             RADCTL.145
C                                                                                RADCTL.146
C                                                                                RADCTL.147
      RETURN                                                                     RADCTL.148
      END                                                                        RADCTL.149
                                                                                 RADCTL.150
