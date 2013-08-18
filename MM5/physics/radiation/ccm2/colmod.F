                                                                                 COLMOD.1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         COLMOD.2
C                                                                                COLMOD.3
      SUBROUTINE COLMOD(JSLC,KTAUW,IUTRAD,NRADISP,CLDFRA,FRLA,FRSA,              COLMOD.4
     -                  FIRTP,SABTP,T3D,QV3D,PP3D,QC3D,QR3D,QI3D,                05DEC01.1324
     -                  QNI3D,MCPHYS,ICLOUDE)                                    05DEC01.1325
C                                                                                COLMOD.6
C               CCM2 RADIATION COLUMN MODEL                                      COLMOD.7
C               BRUCE BRIEGLEB  JANUARY 1992                                     COLMOD.8
C                                                                                COLMOD.9
C                                                                                COLMOD.10
C THIS PROGRAM PROVIDES INPUT AND OUTPUT FOR CCM2 RADIATION ROUTINES             COLMOD.11
C                                                                                COLMOD.12
C                                                                                COLMOD.13
C THREE INTERFACE ROUTINES ARE INVOKED:                                          COLMOD.14
C                                                                                COLMOD.15
C    STPARM   SETS CERTAIN MODEL PARAMETERS                                      COLMOD.16
C    GETDAT   READS INPUT AND COMPUTES AUXILLIARY RADIATION INPUTS               COLMOD.17
C    RADINI   MODIFIED FORM OF CCM2 RADINI; SETS RADIATION CONSTANTS             COLMOD.18
C                                                                                COLMOD.19
C FROM 'RADCTL' AND ON DOWN, THE RADIATION CALL IS THE SAME AS USED              COLMOD.20
C IN CCM2                                                                        COLMOD.21
C                                                                                COLMOD.22
C AFTER RADCTL, THERE IS ONE DIAGNOSTIC ROUTINE AND PRINT STATEMENTS             COLMOD.23
C FOR OUTPUT.                                                                    COLMOD.24
C                                                                                COLMOD.25
C TO SEE WHAT PARAMETERS THE USER CAN CONTROL, SEE AN EXAMPLE INPUT              COLMOD.26
C FILE; ESSENTIALLY, WHATEVER THE RADIATION NEEDS IN THE MODEL IS WHAT           COLMOD.27
C CAN BE CHANGED;  TO MODIFY OTHER ASPECTS OF THE MODEL, THE USER                COLMOD.28
C MUST MODIFY THE RADATION ROUTINES THEMSELVES                                   COLMOD.29
C                                                                                COLMOD.30
C USE SPECIAL SOURCES; OTHERWISE, PLX15                                          COLMOD.31
C                                                                                COLMOD.32
C                                                                                COLMOD.33
C-----------------------------------------------------------------------         COLMOD.34
C#     INCLUDE <PARAME>                                                          COLMOD.35
C#     INCLUDE <RADCCM2.DATA>                                                    COLMOD.36
#     include <parame.incl>                                                      COLMOD.37
#     include <param3.incl>                                                      COLMOD.38
#     include <parccm2.incl>                                                     COLMOD.39
#     include <various.incl>                                                     COLMOD.40
C-----------------------------------------------------------------------         COLMOD.41
C INPUT ARGUMENTS                                                                COLMOD.42
C                                                                                COLMOD.43
      DIMENSION T3D(MIX,MJX,MKX),QV3D(MIX,MJX,MKX),PP3D(MIX,MJX,MKX)             COLMOD.44
      REAL QC3D,QR3D,QI3D,QNI3D                                                  05DEC01.1326
      DIMENSION QC3D(MIXM,MJXM,MKXM),QR3D(MIXM,MJXM,MKXM)                        05DEC01.1327
      DIMENSION QI3D(MIXIC,MJXIC,MKXIC),QNI3D(MIXIC,MJXIC,MKXIC)                 05DEC01.1328
      REAL CLDFRA(IPLOND,IPLEVP) ! FRACTIONAL CONVECTIVE CLOUD COVER             COLMOD.45
C                                                                                COLMOD.46
C LOCAL ARGUMENTS                                                                COLMOD.47
C                                                                                COLMOD.48
      REAL TS(IPLOND),          ! SURFACE AIR TEMPERATURE                        COLMOD.49
     $     TG(IPLOND),          ! SURFACE (SKIN) TEMPERATURE                     COLMOD.50
     $     PSURF(IPLOND),          ! SURFACE PRESSURE                            COLMOD.51
     $     PMID(IPLOND,IPLEV),   ! MODEL LEVEL PRESSURES                         COLMOD.52
     $     PINT(IPLOND,IPLEVP),  ! MODEL INTERFACE PRESSURES                     COLMOD.53
     $     PMLN(IPLOND,IPLEV),   ! NATURAL LOG OF PMID                           COLMOD.54
     $     PILN(IPLOND,IPLEVP),  ! NATURAL LOG OF PINT                           COLMOD.55
     $     T(IPLOND,IPLEV),      ! MODEL LEVEL TEMPERATURES                      COLMOD.56
     $     H2OMMR(IPLOND,IPLEV), ! MODEL LEVEL SPECIFIC HUMIDITY                 COLMOD.57
     $     CLDFRC(IPLOND,IPLEVP),! FRACTIONAL CLOUD COVER                        COLMOD.58
     $     EFFCLD(IPLOND,IPLEVP),! EFFECTIVE FRACTIONAL CLOUD COVER              COLMOD.59
     $     CLWP(IPLOND,IPLEV),   ! CLOUD LIQUID WATER PATH                       COLMOD.60
     $     CIWP(IPLOND,IPLEV),   ! CLOUD ICE WATER PATH                          05DEC01.1329
     $     CONWP(IPLOND,IPLEV),  ! CONVECTIVE CLOUD WATER PATH                   05DEC01.1330
     $     PLOL(IPLOND,IPLEVP),  ! O3 PRESSURE WEIGHTED PATH LENGTHS (CM         COLMOD.61
     $     PLOS(IPLOND,IPLEVP)   ! O3 PATH LENGTHS (CM)                          COLMOD.62
      REAL O3PROF(IPLOND,IPLEVP) ! OZONE PROFILE                                 COLMOD.63
C                                                                                COLMOD.64
C OUTPUT SOLAR                                                                   COLMOD.65
C                                                                                COLMOD.66
      REAL SOLIN(IPLOND),      ! SOLAR INCIDENT FLUX                             COLMOD.67
     $     SABTP(IPLOND),      ! TOP OUTWARD SOLAR FLUX                          07NOV00.2170
     $     FRSA(IPLOND),       ! SURFACE ABSORBED SOLAR FLUX                     COLMOD.69
     $     CLRST(IPLOND),      ! CLR SKY TOTAL COLUMN ABS SOLAR FLUX             COLMOD.70
     $     CLRSS(IPLOND),      ! CLR SKY SURFACE ABS SOLAR FLUX                  COLMOD.71
     $     QRS(IPLOND,IPLEV)    ! SOLAR HEATING RATE                             COLMOD.72
C                                                                                COLMOD.73
C OUTPUT LONGWAVE                                                                COLMOD.74
C                                                                                COLMOD.75
      REAL FIRTP(IPLOND),      ! UPWARD LW FLX AT MODEL TOP                      07NOV00.2171
     $     FRLA(IPLOND),       ! SRF LONGWAVE COOLING (UP-DWN) FLUX              COLMOD.77
     $     CLRLT(IPLOND),      ! CLR SKY LW FLX AT MODEL TOP                     COLMOD.78
     $     CLRLS(IPLOND),      ! CLR SKY LW FLX AT SRF (UP-DWN)                  COLMOD.79
     $     QRL(IPLOND,IPLEV),   ! LONGWAVE COOLING RATE                          COLMOD.80
     $     SLWD(IPLOND)        ! SRF DOWN LONGWAVE FLUX                          COLMOD.81
C                                                                                COLMOD.82
C SURFACE RADIATIVE HEATING                                                      COLMOD.83
C                                                                                COLMOD.84
      REAL SRFRAD(IPLOND)       ! SRF RADIATIVE HEAT FLUX                        COLMOD.85
C-----------------------------------------------------------------------         COLMOD.86
      INTEGER MCPHYS           ! microphysics parameterization                   05DEC01.1331
      INTEGER ICLOUDE          ! ICLOUD(N): cloud effect on radiation            05DEC01.1332
C                                                                                COLMOD.87
C                                                                                COLMOD.88
      KTAUW=KTAU                                                                 COLMOD.89
      IUTRAD=56                                                                  COLMOD.90
      RADISP=3.                                                                  COLMOD.91
      NRADISP=NINT(RADISP*3600./DT)                                              COLMOD.92
      CALL INIRAD(JSLC,O3PROF)                                                   COLMOD.93
      CALL GETDAT(CLDFRA,O3PROF,GRAVX,CPAIRX,EPSILOX,STEBOLX,TS,TG,PSURF         COLMOD.94
     +     ,PMID,PINT,PMLN,PILN,T,H2OMMR,PLOL,PLOS,CLDFRC,CLWP,CIWP              05DEC01.1333
     +     ,CONWP,EFFCLD,JSLC,ILX,DX,MCPHYS,T3D,QV3D,PP3D,QC3D,QR3D              05DEC01.1334
     +     ,QI3D,QNI3D,ICLOUDE)                                                  05DEC01.1335
C                                                                                COLMOD.97
C PERFORM RADIATION INITIALIZATION                                               COLMOD.98
C                                                                                COLMOD.99
C     CALL RADINI(GRAVX,CPAIRX,EPSILOX,STEBOLX)                                  COLMOD.100
C                                                                                COLMOD.101
C COMPUTE RADIATION; FOLLOWING CALL IS THE SAME AS IN CCM2                       COLMOD.102
C                                                                                COLMOD.103
C-----------------------------------------------------------------------         COLMOD.104
      CALL RADCTL(TS,TG,PSURF,PMID,PINT,PMLN,PILN,T,H2OMMR,CLDFRC,EFFCLD         COLMOD.105
     +     ,CLWP,CIWP,CONWP,PLOL,PLOS,SOLIN,SABTP,FRSA,CLRST,CLRSS,QRS           05DEC01.1336
     +     ,FIRTP,FRLA,CLRLT,CLRLS,QRL,SRFRAD,JSLC,KTAUW,IUTRAD,NRADISP)         05DEC01.1337
C-----------------------------------------------------------------------         COLMOD.108
C                                                                                COLMOD.109
C                                                                                COLMOD.110
      RETURN                                                                     COLMOD.111
      END                                                                        COLMOD.112
                                                                                 COLMOD.113
