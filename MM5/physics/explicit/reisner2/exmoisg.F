C+---+-----------------------------------------------------------------+         19DEC02.1424
#undef CALL_EVERY_OTHER_TSTEP 
C+---+-----------------------------------------------------------------+         19DEC02.1426
      SUBROUTINE EXMOISG(T3DTEN,QV3DTEN,QC3DTEN,QR3DTEN,                         EXMOISG.1
     &                   QI3DTEN,QNI3DTEN,QG3DTEN,QNC3DTEN,                      EXMOISG.2
     &                   T3D,QV3D,QC3D,QR3D,QI3D,QNI3D,                          EXMOISG.3
     &                   QG3D,QNC3D,PP3D,PSB,RAINNC,SNOWNCT,                     EXMOISG.4
     &                   IN,J,IST,IEN                                            19DEC02.1427
#if defined (CALL_EVERY_OTHER_TSTEP)
     +                  ,tex_ten, qvex_ten, qcex_ten, qrex_ten                   19DEC02.1429
     +                  ,qiex_ten, qniex_ten, qgex_ten, qncex_ten                19DEC02.1430
#endif                                                                           23DEC04.3283
     +)                                                                          19DEC02.1432
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          EXMOISG.6
C                                                                     C          EXMOISG.7
C                                                                     C          EXMOISG.8
C     THIS SUBROUTINE COMPUTES THE MOISTURE TENDENCIES                C          EXMOISG.9
C     THE PROGNOSTIC EQUATIONS INCLUDE                                C          EXMOISG.10
C     WATER VAPOR (QVTEN), CLOUD WATER (QCTEN), RAIN WATER (QRTEN),   C          EXMOISG.11
C     CLOUD ICE (QITEN), SNOW (QNITEN), GRAUPEL (QGTEN), AND          C          EXMOISG.12
C     NUMBER CON OF CLOUD ICE (QNCTEN)                                C          EXMOISG.13
C                                                                     C          EXMOISG.14
C     IN = 1 : FOR LARGE DOMAIN                                       C          EXMOISG.15
C        = 2 : FOR NESTED DOMAIN                                      C          EXMOISG.16
C                                                                     C          EXMOISG.17
C     ALL THE OTHER ARGUMENTS ARE PASSED FROM SUBROUTINE "SOLVE" AND  C          EXMOISG.18
C     EXPLAINED IN "SOLVE".                                           C          EXMOISG.19
C                                                                     C          EXMOISG.20
C     THE SCHEME IS MODIFIED AGAIN FOR RELEASE-3-6 BY GREG THOMPSON,  C          19DEC02.1433
C     AND ROY RASMUSSEN INCLUDING INPUT FROM JOHN BROWN (NOAA/FSL)    C          EXMOISG.22
C     MAJOR CHANGES INCLUDE                                           C          EXMOISG.23
C       BUG FIXES TO PREVIOUS RELEASE                                 C          EXMOISG.24
C       ICE NUMBER CONCENTRATION CORRECTED (PROGNOSED ONCE AGAIN)     C          EXMOISG.25
C       AUTOCONVERSION CLW->RAIN CHANGED TO 0.35 G/KG  (FROM 0.1)     C          EXMOISG.26
C       FREEZING OF CLOUD DROPS NOW INITIATES NCON CORRECTLY          C          EXMOISG.27
C       ACCORDING TO MIN ICE MASS INSTEAD OF CNP NUMBER               C          EXMOISG.28
C       PRI,PRD,PREI,PREG NOW ADJUSTED _BEFORE_ COMPUTING DEPAC+OTHER C          EXMOISG.29
C       INIT OF CLOUD WATER NOW USES CONSISTENT RSLF FUNCTION         C          EXMOISG.30
C       FEW VARIABLE NAMES CHANGED/CLARIFIED                          C          EXMOISG.31
C    2 NOV 2000 MEETING WITH JOHN BROWN                               C          EXMOISG.32
C       MULTIPLIED THE FOLLOWING CONSTANTS FOUND IN JRG COMMON BLOCK  C          EXMOISG.33
C       BY RONV SINCE THEY CONTAIN RON IN NUMERATOR: FRD1,CIR,CIRF,   C          EXMOISG.34
C       CSR,CRS,CRG,ACRCR,DEPR1                                       C          EXMOISG.35
C       ADDED SUM PSSACW+PGSACW=PSACW LINE ABOVE PISPL SINCE EACH VAL C          EXMOISG.36
C       COULD HAVE CHANGED IN CONSERVATION CHECK                      C          EXMOISG.37
C       CHANGED AB TO ABW IN DENOMINATOR OF PMLTEV,PMLTGE SINCE IT IS C          EXMOISG.38
C       WATER EVAPORATING OFF THE SNOW/GRAUPEL NOT ICE SUBLIMATING    C          EXMOISG.39
C       CONSTRAINED DUM11 GREATER THAN ZERO IN FINAL DO LOOP          C          EXMOISG.40
C       USE DRY RHO AS SUGGESTED BY JOHN BROWN                        C          EXMOISG.41
C    17 FEB 2001                                                      C          EXMOISG.42
C       SET XNC=0 OUTSIDE IF STATEMENT; DISALLOW DEPOSITIONAL GROWTH  C          EXMOISG.43
C       OF RAIN; COMPUTE SLOGN OUTSIDE IF STATEMENT; COMPUTE A MIN    C          EXMOISG.44
C       DIAMETER OF ICE FOR ICE FALLSPEED CALCULATION; REPLACE        C          EXMOISG.45
C       SUM_TERMS FOR DUM11 IN CONSERVATION CHECKS AND USE SCALAR     C          EXMOISG.46
C       VARIABLE FOR RATIO.                                           C          EXMOISG.47
C                                                                     C          EXMOISG.48
C    RELEASE 3.5:                                                     C          EXMOISG.49
C       ADD IFSNOW OPTION                                             C          EXMOISG.50
C    RELEASE 3.6:                                                     C          19DEC02.1434
C    30 JUL 2002                                                      C          19DEC02.1435
C       BUG FIXES FOR RON2 AND NCON UNITS; FUDGEF CHANGED 1.0 TO 0.5; C          19DEC02.1436
C       MOVE DIACE_min, RHO_not AND RONV CONSTANTS TO PARAMR/JRG;     C          19DEC02.1437
C       AUTOCONVERSION DONE EARLIER; COMMENT IIWARM VARS AND GOTOS;   C          19DEC02.1438
C       COMMENT OUT PRACS/PSACR AND ALPSNOW/ALPRAIN, SET ALPHARS=0.0; C          19DEC02.1439
C       SUBSTITUTE TEMPERATURE-DEPENDENT SONV (Y-INTERCEPT EXP        C          19DEC02.1440
C       DISTRIB FOR SNOW) FOR PREV MASS-DEPENDENT SONV.               C          19DEC02.1441
C    06 NOV 2002                                                      C          19DEC02.1442
C       CHANGE SNOW-TO-GRAUPEL CONVERSION.  PREVIOUSLY, BASED ON      C          19DEC02.1443
C       MURAKAMI, RIMING GROWTH (PSACW) NEEDED TO EXCEED DEPOSITIONAL C          19DEC02.1444
C       (PREI) GROWTH.  NOW PSACW MUST BE 2.5 TIMES GREATER THAN PREI.C          19DEC02.1445
C       ALSO SWAPPED KESSLER AUTOCONVERSION FOR BERRY AND REINHARDT   C          19DEC02.1446
C       SCHEME IMPLEMENTED AS IN WALKO ET AL. (1995).                 C          19DEC02.1447
C    02 DEC 2002                                                      C          19DEC02.1448
C       INCLUDE SCHMIDT NUMBER TO ONE-THIRD POWER (0.84) IN PREI,     C          19DEC02.1449
C       PRE, PMLTEV (BUG FIX BY DR. GERHARD KRAMM)                    C          19DEC02.1450
C    15 MAY 2003                                                      C          23DEC04.3284
C       PSACW MUST BE 5.0 TIMES GREATER THAN PREI BEFORE CREATING     C          23DEC04.3285
C       GRAUPEL.  MAJOR BUG FIX TO BERRY/REINHARDT AUTOCONV AFTER     C          23DEC04.3286
C       DISCOVERING MISTAKES IN JOURNAL PAPERS.  REQUIRE MIN DIAMETER C          23DEC04.3287
C       OF ICE GREATER THAN 100 MICRONS BEFORE RIMING ICE BASED ON    C          23DEC04.3288
C       PRUPPACHER AND KLETT, 1997 (PAGE 600).  ICE MULTIPLICATION    C          23DEC04.3289
C       SECTION: CHANGED PSACW TO PGACW PER DISC WITH I. GERESDI.     C          23DEC04.3290
C    01 SEP 2003                                                      C          23DEC04.3291
C       PSACW MUST BE 3.0 TIMES GREATER THAN PREI FOR GRAUPEL INIT.   C          23DEC04.3292
C       CHANGED AFTER MORE TESTING AGAINST GERESDI BIN MODEL AND TO   C          23DEC04.3293
C       MATCH THE THOMPSON, RASMUSSEN, MANNING MWR-PART 1 PAPER.      C          23DEC04.3294
C    RELEASE 3.7:                                                     C          23DEC04.3295
C    06 DEC 2004                                                      C          23DEC04.3296
C       Mods to the rain y-intercept value, RONV, (parameters in      C          23DEC04.3297
C       paramr.F) to eliminate "rain-gush" effect.                    C          23DEC04.3298
C       Added parameter xnu calculated from CNP to make maritime CCN  C          23DEC04.3299
C       use broad drop spectra but narrow the distrib (decrease       C          23DEC04.3300
C       dispersion) as CNP increases to continental values.  Net      C          23DEC04.3301
C       effect: increase autoconversion for maritime CNP values;      C          23DEC04.3302
C       little change for continental.                                C          23DEC04.3303
C       Graupel intercept parameter, GONV, was not correct for gamma  C          23DEC04.3304
C       size distrib as described in MWR Part1.  Went back to old exp C          23DEC04.3305
C       distrib and not allow GONV to exceed old M-P value of 4.E6.   C          23DEC04.3306
C       Assigned min fallspeed of cloud ice to 0.3 m/s.               C          23DEC04.3307
C       Changed aggregation of cloud ice according to new fallspeed.  C          23DEC04.3308
C       A few other minor bug fixes.                                  C          23DEC04.3309
C                                                                     C          EXMOISG.51
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          EXMOISG.52
#     include <parame.incl>                                                      EXMOISG.53
#     include <varia.incl>                                                       EXMOISG.54
#     include <param3.incl>                                                      EXMOISG.55
#     include <param2.incl>                                                      EXMOISG.56
#     include <pmoist.incl>                                                      EXMOISG.57
#     include <various.incl>                                                     EXMOISG.58
#     include <rpstar.incl>                                                      EXMOISG.59
#     include <jrg.incl>                                                         EXMOISG.60
#     include <nhcnst.incl>                                                      EXMOISG.61
                                                                                 EXMOISG.62
      COMMON/BER/ ABER1(31),ABER2(31)                                            EXMOISG.63
C LOOKUP TABLE FOR A1 AND A2 IN BERGERGON PROCESS                                EXMOISG.64
                                                                                 EXMOISG.65
C INPUT ARRAYS                                                                   EXMOISG.66
                                                                                 EXMOISG.67
      REAL  T3D(MIX,MJX,MKX),        QV3D(MIX,MJX,MKX),                          EXMOISG.68
     &     QC3D(MIXM,MJXM,MKXM),     QR3D(MIXM,MJXM,MKXM),                       EXMOISG.69
     &     QI3D(MIXIC,MJXIC,MKXIC), QNI3D(MIXIC,MJXIC,MKXIC),                    EXMOISG.70
     &     QG3D(MIXIG,MJXIG,MKXIG), QNC3D(MIXIG,MJXIG,MKXIG),                    EXMOISG.71
     &     PP3D(MIX,MJX,MKX),         PSB(MIX,MJX)                               EXMOISG.72
      REAL RAINNC(MIX,MJX), SNOWNCT(MIX,MJX)                                     EXMOISG.73
                                                                                 EXMOISG.74
C ARRAYS FOR BASE VARIABLES                                                      EXMOISG.75
                                                                                 EXMOISG.76
      REAL TAOUT(MIX,MKX),PRES(MIX,MKX),RHO(MIX,MKX),                            EXMOISG.77
     1     T3DTEN(MIX,MJX,MKX),SCR7(MIX,MKX),SCR6(MIX,MKX)                       EXMOISG.78
     1    ,RHOd(MIX,MKX)                                                         EXMOISG.79
                                                                                 EXMOISG.80
C ARRAYS FOR QV                                                                  EXMOISG.81
                                                                                 EXMOISG.82
      REAL QAOUT(MIX,MKX),QVQVS(MIX,MKX),QVQVSI(MIX,MKX),                        EXMOISG.83
     1     QVS(MIX,MKX),QVSI(MIX,MKX),AB(MIX,MKX),                               EXMOISG.84
     1     ABI(MIX,MKX),PREG(MIX,MKX),QV3DTEN(MIX,MJX,MKX)                       EXMOISG.85
     1     ,ABW(MIX,MKX)                                                         EXMOISG.86
                                                                                 EXMOISG.87
C ARRAYS FOR QC                                                                  EXMOISG.88
                                                                                 EXMOISG.89
      REAL CLOUD(MIX,MKX),PRC(MIX,MKX),                                          EXMOISG.90
     1     PIFCW(MIX,MKX),PSACW(MIX,MKX),PGACW(MIX,MKX),                         EXMOISG.91
     1     PRA(MIX,MKX),QC3DTEN(MIX,MJX,MKX),SCR3(MIX,MKX)                       EXMOISG.92
                                                                                 EXMOISG.93
C ARRAYS FOR QR                                                                  EXMOISG.94
                                                                                 EXMOISG.95
      REAL RAIN(MIX,MKX),SLOR(MIX,MKX),PIACR(MIX,MKX),                           EXMOISG.96
     1     PGACR(MIX,MKX),PRE(MIX,MKX),                                          EXMOISG.97
     1     QR3DTEN(MIX,MJX,MKX),SCR4R(MIX,MKX),FR(MKX),                          EXMOISG.98
     1     FALOUTR(MKX) ,RONV(MIX,MKX)                                           EXMOISG.99
                                                                                 EXMOISG.100
C ARRAYS FOR QI                                                                  EXMOISG.101
                                                                                 EXMOISG.102
      REAL ICE(MIX,MKX),PRI(MIX,MKX),PSCNI(MIX,MKX),                             EXMOISG.103
     1     PRAI(MIX,MKX),PRACI(MIX,MKX),PISPL(MIX,MKX),                          EXMOISG.104
     1     PRD(MIX,MKX),QI3DTEN(MIX,MJX,MKX),PIACW(MIX,MKX)                      EXMOISG.105
      REAL SCR4I(MIX,MKX), FI(MKX), FALOUTI(MKX)                                 EXMOISG.106
                                                                                 EXMOISG.107
C ARRAYS FOR QS                                                                  EXMOISG.108
                                                                                 EXMOISG.109
      REAL SNOW(MIX,MKX),SLOS(MIX,MKX),PSACR(MIX,MKX),                           EXMOISG.110
     1     PGSACW(MIX,MKX),PREI(MIX,MKX),                                        EXMOISG.111
     1     PSMLT(MIX,MKX),PMLTEV(MIX,MKX),QNI3DTEN(MIX,MJX,MKX),                 EXMOISG.112
     1     SCR4S(MIX,MKX),FS(MKX),FALOUTS(MKX),                                  EXMOISG.113
     1     PRACS(MIX,MKX),PSSACW(MIX,MKX)                                        EXMOISG.114
     1     ,SONV(MIX,MKX)                                                        EXMOISG.115
                                                                                 EXMOISG.116
C ARRAYS FOR QG                                                                  EXMOISG.117
                                                                                 EXMOISG.118
      REAL GRAUPEL(MIX,MKX),PGMLT(MIX,MKX),PMLTGE(MIX,MKX),                      EXMOISG.119
     1     PGACRM(MIX,MKX),PGACWM(MIX,MKX),                                      EXMOISG.120
     1     QG3DTEN(MIX,MJX,MKX),SCR4G(MIX,MKX),FG(MKX),                          EXMOISG.121
     1     FALOUTG(MKX),SLOG(MIX,MKX),PGFR(MIX,MKX),PICNG(MIX,MKX),              EXMOISG.122
     1     PGIACW(MIX,MKX),PGEMB(MIX,MKX)                                        EXMOISG.123
     1     ,GONV(MIX,MKX)                                                        EXMOISG.124
                                                                                 EXMOISG.125
C ARRAYS FOR NCI                                                                 EXMOISG.126
                                                                                 EXMOISG.127
      REAL NCON(MIX,MKX),QNC3DTEN(MIX,MJX,MKX),                                  EXMOISG.128
     1     NI_RS(MIX,MKX),                                                       EXMOISG.129
     1     NI_AG(MIX,MKX),NI_CG(MIX,MKX),NI_DE(MIX,MKX),                         EXMOISG.130
     1     NI_EV(MIX,MKX)                                                        EXMOISG.131
      REAL SCR4N(MIX,MKX),FALOUTN(MKX)                                           EXMOISG.132
                                                                                 EXMOISG.133
C EXTRA ARRAYS                                                                   EXMOISG.134
                                                                                 EXMOISG.135
      REAL DUM11(MIX,MKX),DUM21(MIX,MKX),DUM31(MIX,MKX),                         EXMOISG.136
     1     SCR4(MIX,MKX),SCR8(MIX,MKX),RHOFAC(MKX)                               EXMOISG.137
C     LOGICAL IIWARM                                                             19DEC02.1451
C+---+-----------------------------------------------------------------+         19DEC02.1452
#if defined (CALL_EVERY_OTHER_TSTEP) 
C TEMP TIMESTEP HOLDING VARIABLE FOR CALLING ROUTINE EVERY OTHER TSTEP           19DEC02.1454
      REAL tex_ten(MIX,MJX,MKX),        qvex_ten(MIX,MJX,MKX),                   19DEC02.1455
     &     qcex_ten(MIXM,MJXM,MKXM),    qrex_ten(MIXM,MJXM,MKXM),                19DEC02.1456
     &     qiex_ten(MIXIC,MJXIC,MKXIC), qniex_ten(MIXIC,MJXIC,MKXIC),            19DEC02.1457
     &     qgex_ten(MIXIG,MJXIG,MKXIG), qncex_ten(MIXIG,MJXIG,MKXIG)             19DEC02.1458
      REAL DTSAVE                                                                19DEC02.1459
      INTEGER IEXFRQ                                                             19DEC02.1460
#endif                                                                           19DEC02.1461
C+---+-----------------------------------------------------------------+         19DEC02.1462
C+---+-----------------------------------------------------------------+         EXMOISG.141
C     IIWARM=.TRUE.                                                              19DEC02.1463
C     IF(IIWARM)TO=173.0                                                         19DEC02.1464
      R1=1.E-15                                                                  EXMOISG.152
      xnu = amax1(0.0, (CNP*1E-6 - 100.)/100.)                                   23DEC04.3311
      DO 15 K=1,KL                                                               EXMOISG.153
        DO 14 I=IST,IEN                                                          EXMOISG.154
Cgt       IF(ABS(QI3DTEN(i,j,k)).lt.R1) QI3DTEN(i,j,k)=0.                        23DEC04.3312
Cgt       IF(ABS(QC3DTEN(i,j,k)).lt.R1) QC3DTEN(i,j,k)=0.                        23DEC04.3313
Cgt       IF(ABS(QNI3DTEN(i,j,k)).lt.R1) QNI3DTEN(i,j,k)=0.                      23DEC04.3314
Cgt       IF(ABS(QR3DTEN(i,j,k)).lt.R1) QR3DTEN(i,j,k)=0.                        23DEC04.3315
Cgt       IF(ABS(QG3DTEN(i,j,k)).lt.R1) QG3DTEN(i,j,k)=0.                        23DEC04.3316
Cgt       IF(ABS(QNC3DTEN(i,j,k)).lt.R1) QNC3DTEN(i,j,k)=0.                      23DEC04.3317
          TAOUT(I,K)=T3D(I,J,K)                                                  EXMOISG.161
          QAOUT(I,K)=AMAX1(R1,QV3D(I,J,K))                                       EXMOISG.162
          PRES(I,K)=(A(K)*PSB(I,J)+PTOP)*1.E3+PP3D(I,J,K)                        EXMOISG.163
          RHO(I,K)=PRES(I,K)/(R*TAOUT(I,K))                                      EXMOISG.164
          pd = .62198*PRES(I,K)/(QAOUT(I,K) + .62198)                            EXMOISG.165
          RHOd(I,K)=pd/(R*TAOUT(I,K))                                            EXMOISG.166
C SATURATION MIXING RATIOS OVER WATER AND ICE (FLATAU&WALKO)                     EXMOISG.167
          QVS(I,K) = RSLF(PRES(I,K),TAOUT(I,K))                                  EXMOISG.168
          QVSI(I,K) = RSIF(PRES(I,K),TAOUT(I,K))                                 EXMOISG.169
          QVQVS(I,K) = QAOUT(I,K)/QVS(I,K)                                       EXMOISG.170
          QVQVSI(I,K) = QAOUT(I,K)/QVSI(I,K)                                     EXMOISG.171
C DIFFUSIVITY OF WATER VAPOR  (PRUPPACHER & KLETT: 13-3)                         EXMOISG.172
          DUM11(I,K)=2.11E-5*(TAOUT(I,K)/TO)**1.94*(101325./PRES(I,K))           EXMOISG.173
C DYNAMIC VISCOSITY OF AIR  (PRUPPACHER & KLETT: 10-141)                         EXMOISG.174
          TEMP_C = TAOUT(I,K)-TO                                                 EXMOISG.175
          IF (TEMP_C .GE. 0.0) THEN                                              EXMOISG.176
            DUM21(I,K)=(1.718+0.0049*TEMP_C)*1.0E-5                              EXMOISG.177
          ELSE                                                                   EXMOISG.178
            DUM21(I,K)=(1.718+0.0049*TEMP_C-1.2E-5*TEMP_C*TEMP_C)*1.0E-5         EXMOISG.179
          ENDIF                                                                  EXMOISG.180
C V8 LV FORMULA                                                                  EXMOISG.181
          DUM31(I,K)=3.1484E6-XLV1*TAOUT(I,K)                                    EXMOISG.182
C THERMAL CONDUCTIVITY OF AIR  (PRUPPACHER & KLETT: 13-18A)                      EXMOISG.183
          SCR4(I,K)=(5.69 + 0.0168*TEMP_C) * 1.0E-5 *418.936                     EXMOISG.184
          IF(TAOUT(I,K).GT.TO)THEN                                               EXMOISG.185
            PRA(I,K)=DUM31(I,K)                                                  EXMOISG.186
          ELSE                                                                   EXMOISG.187
            PRA(I,K)=XLS                                                         EXMOISG.188
          ENDIF                                                                  EXMOISG.189
C A + B IN FORUMLA (B7) OF DUDHIA (1992) FOR T > TO                              EXMOISG.190
          AB(I,K)=RHO(I,K)*XLS*XLS/(SCR4(I,K)*RV*TAOUT(I,K)                      EXMOISG.191
     +         *TAOUT(I,K))+1./(QVS(I,K)*DUM11(I,K))                             EXMOISG.192
C A + B FOR T < TO   (FOR EQN. A.36)                                             EXMOISG.193
          ABI(I,K)=RHO(I,K)*XLS*XLS/(SCR4(I,K)*RV*TAOUT(I,K)                     EXMOISG.194
     +         *TAOUT(I,K))+1./(QVSI(I,K)*DUM11(I,K))                            EXMOISG.195
C A + B FOR RAIN EVAP AND T < TO                                                 EXMOISG.196
          ABW(I,K)=RHO(I,K)*DUM31(I,K)*DUM31(I,K)/(SCR4(I,K)*RV                  EXMOISG.197
     +          *TAOUT(I,K)*TAOUT(I,K))+1./(QVS(I,K)*DUM11(I,K))                 EXMOISG.198
C LATENT HEAT EFFECTS (REDUCES DEPOS GROWTH OF SNOW/GRAUPEL)                     EXMOISG.199
C...(COTTON&ANTHES 1989 4-37)                                                    EXMOISG.200
          DUM11(I,K) = RHO(I,K)*XLS*3.34E5/(SCR4(I,K)*RV                         EXMOISG.201
     +         *TAOUT(I,K)*TAOUT(I,K)*ABI(I,K))                                  EXMOISG.202
          DUM11(I,K) = AMIN1(1.0, AMAX1(DUM11(I,K),0.0))                         EXMOISG.203
 14     CONTINUE                                                                 EXMOISG.204
 15   CONTINUE                                                                   EXMOISG.205
                                                                                 EXMOISG.206
C---BEGIN PRODUCTION TERMS CALCULATION:                                          EXMOISG.207
C    ALL THE PRODUCTION TERMS ARE BASED ON T-1 (I.E. XXB) VARIABLES              EXMOISG.208
      DO 20 K=1,KL                                                               EXMOISG.209
        DO 19 I=IST,IEN                                                          EXMOISG.210
           CLOUD(I,K)=AMAX1(R1,QC3D(I,J,K))                                      EXMOISG.211
           ICE(I,K)=AMAX1(R1,QI3D(I,J,K))                                        EXMOISG.212
           RAIN(I,K)=AMAX1(R1,QR3D(I,J,K))                                       EXMOISG.213
           SNOW(I,K)=AMAX1(R1,QNI3D(I,J,K))                                      EXMOISG.214
           NCON(I,K)=AMAX1(R1,QNC3D(I,J,K))*RHOd(I,K)                            19DEC02.1465
           GRAUPEL(I,K)=AMAX1(R1,QG3D(I,J,K))                                    EXMOISG.216
                                                                                 EXMOISG.217
CCCCC      SONV(I,K) = SON                                                       19DEC02.1466
CCCCC      IF (SNOW(I,K).GT. R1) THEN                                            19DEC02.1467
CCCCC         SONV(I,K) = (CONST1A*(DRAIN/(RHO(I,K)*SNOW(I,K)*CONST1B)           19DEC02.1468
CCCCC+             *(PI*DSNOW/(RHO(I,K)*SNOW(I,K)))**(BS/4))**0.94)              19DEC02.1469
CCCCC+             **(4./(4-0.94*BS))                                            19DEC02.1470
CCCCC         SONV(I,K) = MIN(SONV(I,K),SON)                                     19DEC02.1471
CCCCC      ENDIF                                                                 19DEC02.1472
                                                                                 19DEC02.1473
C.. New SONV formulation based on Fig. 7, curve_3 of Houze et al 1979            19DEC02.1474
           temp_C = amin1(-0.001, TAOUT(I,K)-273.15)                             19DEC02.1475
           sonv(i,k) = amin1(2.0E8, 2.0E6*exp(-0.12*temp_C))                     19DEC02.1476
                                                                                 19DEC02.1477
           SONV(I,K) = SONV(I,K)/SON                                             EXMOISG.225
                                                                                 EXMOISG.226
           GONV(I,K) = GON                                                       EXMOISG.227
           IF (GRAUPEL(I,K).GT. R1) THEN                                         EXMOISG.228
              GONV(I,K) = 2.38*(PI*DGRAUPEL/                                     EXMOISG.229
     +             (RHO(I,K)*GRAUPEL(I,K)))**0.92                                EXMOISG.230
              GONV(I,K) = MAX(1.E4, MIN(GONV(I,K),GON))                          EXMOISG.231
           ENDIF                                                                 EXMOISG.232
           GONV(I,K) = GONV(I,K)/GON                                             EXMOISG.233
                                                                                 EXMOISG.234
           RONV(I,K) = RON2                                                      EXMOISG.235
           IF (RAIN(I,K).GT. R1) THEN                                            EXMOISG.236
               ronv(i,k) = ron_const1r*tanh((ron_qr0-RAIN(I,K))                  EXMOISG.241
     +                /ron_delqr0) + ron_const2r                                 EXMOISG.242
           ENDIF                                                                 EXMOISG.243
           RONV(I,K) = RONV(I,K)/RON                                             19DEC02.1478
                                                                                 EXMOISG.245
           SLOR(I,K)=(RHO(I,K)*RAIN(I,K)/(TOPR*RONV(I,K)))**0.25                 EXMOISG.246
           SLOS(I,K)=(RHO(I,K)*SNOW(I,K)/(TOPS*SONV(I,K)))**0.25                 EXMOISG.247
           SLOG(I,K)=(RHO(I,K)*GRAUPEL(I,K)/(TOPG*GONV(I,K)))**0.25              EXMOISG.248
                                                                                 EXMOISG.249
 19     CONTINUE                                                                 EXMOISG.250
 20   CONTINUE                                                                   EXMOISG.251
                                                                                 EXMOISG.252
C PROCESSES TO BE DOMAIN AVERAGED AND WRITTEN OUT AT EVERY TIME STEP             EXMOISG.253
      DO 22 K=1,KL                                                               EXMOISG.254
         DO 21 I=IST,IEN                                                         EXMOISG.255
            PRI(I,K)=0.                                                          EXMOISG.256
            PIFCW(I,K)=0.                                                        EXMOISG.257
            PGFR(I,K)=0.                                                         EXMOISG.258
            PSCNI(I,K)=0.                                                        EXMOISG.259
            PRAI(I,K)=0.                                                         EXMOISG.260
            PRACI(I,K)=0.                                                        EXMOISG.261
            PIACR(I,K)=0.                                                        EXMOISG.262
            PSACR(I,K)=0.                                                        EXMOISG.263
            PRACS(I,K)=0.                                                        EXMOISG.264
            PSACW(I,K)=0.                                                        EXMOISG.265
            PGSACW(I,K)=0.                                                       EXMOISG.266
            PGACW(I,K)=0.                                                        EXMOISG.267
            PGACR(I,K)=0.                                                        EXMOISG.268
            PREG(I,K)=0.                                                         EXMOISG.269
            PRD(I,K)=0.                                                          EXMOISG.270
            PREI(I,K)=0.                                                         EXMOISG.271
            PRC(I,K)=0.                                                          EXMOISG.272
            PRA(I,K)=0.                                                          EXMOISG.273
            PRE(I,K)=0.                                                          EXMOISG.274
            PSMLT(I,K)=0.                                                        EXMOISG.275
            PMLTEV(I,K)=0.                                                       EXMOISG.276
            PGMLT(I,K)=0.                                                        EXMOISG.277
            PMLTGE(I,K)=0.                                                       EXMOISG.278
            PGACRM(I,K)=0.                                                       EXMOISG.279
            PGACWM(I,K)=0.                                                       EXMOISG.280
            PIACW(I,K)=0.                                                        EXMOISG.281
            PICNG(I,K)=0.                                                        EXMOISG.282
            PGIACW(I,K)=0.                                                       EXMOISG.283
            NI_AG(I,K)=0.                                                        EXMOISG.284
            NI_RS(I,K)=0.                                                        EXMOISG.285
            NI_CG(I,K)=0.                                                        EXMOISG.286
            NI_DE(I,K)=0.                                                        EXMOISG.287
            NI_EV(I,K)=0.                                                        EXMOISG.288
            PISPL(I,K)=0.                                                        EXMOISG.289
            PSSACW(I,K)=0.                                                       EXMOISG.290
            PGEMB(I,K)=0.                                                        EXMOISG.291
 21      CONTINUE                                                                EXMOISG.292
 22   CONTINUE                                                                   EXMOISG.293
      dtsave = dt                                                                19DEC02.1479
C+---+-----------------------------------------------------------------+         19DEC02.1480
#if defined (CALL_EVERY_OTHER_TSTEP)
C TEMP TIMESTEP HOLDING VARIABLE FOR CALLING ROUTINE EVERY OTHER TSTEP           19DEC02.1482
      iexfrq = 3                                                                 19DEC02.1483
      if (mod(ktau-ktaur,iexfrq).eq.0) then                                      19DEC02.1484
                                                                                 19DEC02.1485
        dtsave = dt*iexfrq                                                       19DEC02.1486
                                                                                 19DEC02.1487
        do k = 1, KL                                                             19DEC02.1488
           do i = IST, IEN                                                       19DEC02.1489
              tex_ten(i,j,k)   = t3dten(i,j,k)                                   19DEC02.1490
              qvex_ten(i,j,k)  = qv3dten(i,j,k)                                  19DEC02.1491
              qcex_ten(i,j,k)  = qc3dten(i,j,k)                                  19DEC02.1492
              qrex_ten(i,j,k)  = qr3dten(i,j,k)                                  19DEC02.1493
              qiex_ten(i,j,k)  = qi3dten(i,j,k)                                  19DEC02.1494
              qniex_ten(i,j,k) = qni3dten(i,j,k)                                 19DEC02.1495
              qgex_ten(i,j,k)  = qg3dten(i,j,k)                                  19DEC02.1496
              qncex_ten(i,j,k) = qnc3dten(i,j,k)                                 19DEC02.1497
           enddo                                                                 19DEC02.1498
        enddo                                                                    19DEC02.1499
#endif                                                                           19DEC02.1500
C+---+-----------------------------------------------------------------+         19DEC02.1501
                                                                                 EXMOISG.294
C FORMULAS COME FROM DUDHIA (1989, JAS, 46),                                     EXMOISG.295
C RUTLEDGE AND HOBBS (1983, JAS, 40), {RH1},                                     EXMOISG.296
C RUTLEDGE AND HOBBS (1984, JAS, 41), {RH},                                      EXMOISG.297
C MURAKAMI (1989, JMSJ, 68),                                                     EXMOISG.298
C AND LIN ET AL. (1983, JCAP, 22)                                                EXMOISG.299
C HSIE ET AL. (1980, JAP, 19)                                                    EXMOISG.300
C KOENIG (1972, MWR, 100)                                                        EXMOISG.301
C AND METEOROLOGICAL RESEARCH INSTITUTE OF JAPAN (#28, 1991)                     EXMOISG.302
C AND REISNER ET AL. (1998, QJRMS 124, 1071-1107)                                EXMOISG.303
                                                                                 EXMOISG.304
      DO 30 K=1,KL                                                               EXMOISG.306
         DO 29 I=IST,IEN                                                         EXMOISG.307
            XMI=ICE(I,K)*RHOd(I,K)/NCON(I,K)                                     EXMOISG.308
            RHO_FAC = (RHO_NOT/RHO(I,K))**.5                                     EXMOISG.309
                                                                                 EXMOISG.310
            VT2R=(FRAIN*SLOR(I,K)**BR)*RHO_FAC                                   EXMOISG.311
            VT2R=AMIN1(VT2R, 9.0)                                                23DEC04.3319
            VT2S=(FSNOW*SLOS(I,K)**BS)*RHO_FAC                                   EXMOISG.312
            VT2S=AMIN1(VT2S, 2.0)                                                23DEC04.3320
            VT2G=(FGRAUPEL*SLOG(I,K)**BG)*RHO_FAC                                EXMOISG.313
            VT2G=AMIN1(VT2G, 10.0)                                               23DEC04.3321
            DIACE=(6.*RHOd(I,K)*ICE(I,K)/(PI*DICE*NCON(I,K)))**0.3333            23DEC04.3322
            DIACE=AMAX1(AMIN1(1.99*XR0S, DIACE), DIACE_min)                      23DEC04.3323
            VT2I=700. * DIACE * RHO_FAC                                          23DEC04.3324
            VT2I=AMAX1(AMIN1(1.0, VT2I), 0.3)                                    23DEC04.3325
            A1 = 0.                                                              EXMOISG.317
            A2 = 0.                                                              EXMOISG.318
            XNC = 0.                                                             EXMOISG.319
            IT=NINT(TAOUT(I,K) - 273.16)                                         EXMOISG.320
            DCLO = (6.*RHOd(I,K)*CLOUD(I,K)/(PI*DRAIN*CNP))**0.3333              EXMOISG.321
            DCLO = AMAX1(1.e-6, DCLO)                                            23DEC04.3326
                                                                                 19DEC02.1502
C---AUTOCONVERSION OF CLOUD WATER TO RAINWATER  (R-A.60)                         19DEC02.1503
CC            PRC(I,K)=AMAX1(0.,QCK1*(CLOUD(I,K)-QCTH))                          19DEC02.1504
CC          PRC(I,K)=AMAX1(0.,QCK1*(CLOUD(I,K)-0.00035))                         19DEC02.1505
                                                                                 19DEC02.1506
C+---+-----------------------------------------------------------------+         23DEC04.3327
CC      ! Berry and Reinhardt autoconversion (1974, Part II) as in               23DEC04.3328
CC      ! Walko et al., (1995) but WITH TYPOS CORRECTED IN BOTH!!!               23DEC04.3329
            BandR_L2 = 0.027*cloud(I,K)*(6.25E18*(DCLO**4)*(1.+xnu)              23DEC04.3330
     +                 **(-0.5) - 0.4)                                           23DEC04.3331
            BandR_T2  = 3.72/(rho(I,K)*cloud(I,K)) / (0.5E6*DCLO                 23DEC04.3332
     +                       *(1.+xnu)**(-1./6.) - 7.5)                          23DEC04.3333
            if (BandR_L2.gt.0.0 .and. BandR_T2.gt.0.0)                           23DEC04.3334
     +      PRC(I,K) = BandR_L2/BandR_T2                                         23DEC04.3335
C+---+-----------------------------------------------------------------+         23DEC04.3336
                                                                                 19DEC02.1517
            IF(RAIN(I,K).GT.R1)THEN                                              19DEC02.1518
               IF(CLOUD(I,K).GT.R1) THEN                                         19DEC02.1519
               DRNO = 4.*SLOR(I,K)                                               19DEC02.1520
               STOKE_R = DCLO*DCLO*VT2R*DRAIN/(9.*DUM21(I,K)*DRNO)               19DEC02.1521
               EFF_CR = STOKE_R*STOKE_R/((STOKE_R+0.5)*(STOKE_R+0.5))            19DEC02.1522
               EFF_CR = MAX(0.0, MIN(1.0, EFF_CR))                               19DEC02.1523
C---ACCRETION OF CLOUD WATER BY RAINWATER  (R-A.61)                              19DEC02.1524
               PRA(I,K)=ACRCR*RONV(I,K)*EFF_CR/EFCR*SLOR(I,K)**BACRCR            19DEC02.1525
     +              *CLOUD(I,K)                                                  19DEC02.1526
               ENDIF                                                             19DEC02.1527
                                                                                 19DEC02.1528
C...EVAPORATION OF RAINWATER  (R-A.62) [No depositional growth of rain - 17Feb2  23DEC04.3337
               PRE(I,K)=DEPR1*RONV(I,K)*(QAOUT(I,K)/QVS(I,K)-1.)*(               19DEC02.1530
     +              0.78*SLOR(I,K)*SLOR(I,K)+((DEPR2*RHO(I,K)                    19DEC02.1531
     +              /DUM21(I,K))**0.5)*DEPR3*0.84*SLOR(I,K)**(DEPR4))            19DEC02.1532
     +              /ABW(I,K)                                                    19DEC02.1533
               PRE(I,K) = amin1(PRE(I,K), 0.0)                                   19DEC02.1534
            ENDIF                                                                19DEC02.1535
C           IF(IIWARM)GOTO 702                                                   19DEC02.1536
                                                                                 19DEC02.1537
            SUPICE=(QAOUT(I,K)-QVSI(I,K))/DTSAVE                                 19DEC02.1538
C INITIATION OF CLOUD ICE  (R-A.21)                                              EXMOISG.325
C [EITHER COOPER OR FLETCHER DEPENDING ON CONSTANTS IN PARAMR.F]                 EXMOISG.326
                                                                                 EXMOISG.327
            IF( (QVQVSI(I,K).GT.1.05.AND.TAOUT(I,K).LT.273.15) .OR.              EXMOISG.328
     +            (QVQVS(I,K).GT.1.0.AND.TAOUT(I,K).LT.268.15) ) THEN            EXMOISG.329
               XNC=TNO*EXP(ATO*(TO-AMAX1(TAOUT(I,K),240.)))                      23DEC04.3338
               PRI(I,K)=AMAX1(0.,XM01/RHOd(I,K)*(XNC-NCON(I,K))/DTSAVE)          19DEC02.1539
            ENDIF                                                                EXMOISG.332
C INITIATION OF CLOUD ICE VIA MEYERS EQN 2.4                                     EXMOISG.333
C            IF ( (QVQVSI(I,K).GT.1.0.AND.TAOUT(I,K).LE.268.0) .OR               EXMOISG.334
C     +            . (QVQVS(I,K).GT.1.0.AND.TAOUT(I,K).LE.271.0) ) THEN          EXMOISG.335
C               XNC = (EXP(-0.639 + 12.96*(QVQVSI(I,K)-1.0)))*1000.0             EXMOISG.336
C               PRI(I,K)=AMAX1(0.,XM01/RHOd(I,K)*(XNC-NCON(I,K))/DTSAVE)         19DEC02.1540
C            ENDIF                                                               EXMOISG.338
C FREEZING OF CLOUD DROPLETS, (R-A.22)                                           EXMOISG.339
            IF(CLOUD(I,K).GT.R1.AND.TAOUT(I,K).LT.268.15)THEN                    EXMOISG.340
               PIFCW(I,K)=BP*(EXP(AP*(TO-TAOUT(I,K)))-1.)*RHOd(I,K)*             EXMOISG.341
     +              CLOUD(I,K)**2/(CNP*DRAIN)                                    EXMOISG.342
            ENDIF                                                                EXMOISG.343
            IF(RAIN(I,K).GT.R1)THEN                                              EXMOISG.344
C FREEZING OF RAIN, LIN ET AL. (45) (R-A.56)                                     EXMOISG.345
               IF(TAOUT(I,K).LT.263.15) THEN                                     EXMOISG.346
                  PGFR(I,K)=SLOR(I,K)**7*FRD1*RONV(I,K)*(DRAIN/RHOd(I,K)         EXMOISG.347
     +                 )*(EXP(FRA1*(TO-TAOUT(I,K)))-1.)                          EXMOISG.348
               ENDIF                                                             EXMOISG.349
C..COLLECTION OF CLOUD ICE BY RAIN, RH (A5)  (R-A.41)                            EXMOISG.350
               IF(ICE(I,K).GT.R1)THEN                                            EXMOISG.351
                  PRACI(I,K)=CIR*RONV(I,K)*ICE(I,K)*(-0.267*2.                   EXMOISG.352
     +                 *SLOR(I,K)**3+5.15E3*6.*SLOR(I,K)**4-1.0225E6*24.         EXMOISG.353
     +                 *SLOR(I,K)**5+7.55E7*120.*SLOR(I,K)**6)                   EXMOISG.354
                  PRACI(I,K)=AMAX1(0.,PRACI(I,K))                                EXMOISG.355
               ENDIF                                                             EXMOISG.356
C FREEZING OF RAIN BY COLLISION WITH ICE, RH (A7)  (R-A.42)                      EXMOISG.357
               IF(ICE(I,K).GT.R1)THEN                                            EXMOISG.358
                  PIACR(I,K)=CIRF*RONV(I,K)*NCON(I,K)/RHOd(I,K)*(-0.267*         EXMOISG.359
     +                 120.*SLOR(I,K)**6+5.15E3*                                 EXMOISG.360
     +                 720.*SLOR(I,K)**7-1.0225E6*5040.*SLOR(I,K)**8+            EXMOISG.361
     +                 7.55E7*40320.*SLOR(I,K)**9)                               EXMOISG.362
                  PIACR(I,K)=AMAX1(0.,PIACR(I,K))                                EXMOISG.363
               ENDIF                                                             EXMOISG.364
C COLLECTION OF RAIN BY SNOW, RH (A8)  (R-A.47)                                  EXMOISG.365
               IF(SNOW(I,K).GT.R1)THEN                                           23DEC04.3339
                  PSACR(I,K)=(CSR*RONV(I,K)*SONV(I,K)/RHOd(I,K))*                23DEC04.3340
     +                 SQRT((ALPHA1*VT2R-BETA1*VT2S)**2+GAMMA1*VT2R              23DEC04.3341
     +                 *VT2S)*(5.*SLOR(I,K)**6*SLOS(I,K)+2.*SLOR(I,K)**5         23DEC04.3342
     +                 *SLOS(I,K)**2+0.5*SLOR(I,K)**4*SLOS(I,K)**3)              23DEC04.3343
C COLLECTION OF SNOW BY RAIN, RH (A9)  (R-A.48)                                  EXMOISG.371
                  PRACS(I,K)=(CRS*RONV(I,K)*SONV(I,K)/RHOd(I,K))*                23DEC04.3344
     +                 SQRT((ALPHA1*VT2R-BETA1*VT2S)**2+GAMMA1*VT2R              23DEC04.3345
     +                 *VT2S)*(5.*SLOS(I,K)**6*SLOR(I,K)+2.*SLOS(I,K)**5         23DEC04.3346
     +                 *SLOR(I,K)**2+0.5*SLOS(I,K)**4*SLOR(I,K)**3)              23DEC04.3347
               ENDIF                                                             23DEC04.3348
            ENDIF                                                                EXMOISG.377
            IF(SNOW(I,K).GT.R1)THEN                                              EXMOISG.378
C...ACCRETION OF CLOUD ICE BY SNOW, DUDHIA (B15)  (R-A.38)                       EXMOISG.379
               IF(ICE(I,K).GT.R1.AND.TAOUT(I,K).LT.TO) THEN                      EXMOISG.380
                  PRAI(I,K)=ACRIS*SONV(I,K)*SLOS(I,K)**BACRIS*ICE(I,K)           EXMOISG.381
               ENDIF                                                             EXMOISG.382
C COLLECTION OF CLOUD WATER BY SNOW, RH1 (A22)  (R-A.46)                         EXMOISG.383
               IF(CLOUD(I,K).GT.R1) THEN                                         EXMOISG.384
                  DSNO = 4.*SLOS(I,K)                                            EXMOISG.397
                  STOKE_S = DCLO*DCLO*VT2S*DRAIN/(9.*DUM21(I,K)*DSNO)            EXMOISG.398
                  EFF_CS = STOKE_S*STOKE_S/((STOKE_S+0.4)*(STOKE_S+0.4))         EXMOISG.399
                  EFF_CS = MAX(0.0, MIN(1.0, EFF_CS))                            EXMOISG.400
                  PSACW(I,K)=ACRCS*EFF_CS/EFCS*SONV(I,K)                         EXMOISG.401
     +                 *SLOS(I,K)**BACRCS*CLOUD(I,K)                             EXMOISG.402
               ENDIF                                                             EXMOISG.404
            ENDIF                                                                EXMOISG.405
                                                                                 EXMOISG.406
C COLLECTION OF CLOUD WATER BY GRAUPEL, RH (A11)  (R-A.59)                       EXMOISG.407
            IF(GRAUPEL(I,K).GT.R1)THEN                                           EXMOISG.408
C              SLOGN = 0.75785828 * (SLOG(I,K)**0.8)                             23DEC04.3349
               IF (CLOUD(I,K).GT.R1) THEN                                        EXMOISG.410
                  DGRO = 4.*SLOG(I,K)                                            23DEC04.3350
                  STOKE_G = DCLO*DCLO*VT2G*DRAIN/(9.*DUM21(I,K)*DGRO)            EXMOISG.415
                  EFF_CG = STOKE_G*STOKE_G/((STOKE_G+0.5)*(STOKE_G+0.5))         EXMOISG.416
                  EFF_CG = MAX(0.0, MIN(1.0, EFF_CG))                            EXMOISG.417
                  PGACW(I,K)=ACRCG*GONV(I,K)*EFF_CG/EFGC*                        23DEC04.3351
     +                 SLOG(I,K)**BACRCG*CLOUD(I,K)                              23DEC04.3352
               ENDIF                                                             EXMOISG.428
                                                                                 EXMOISG.429
C COLLECTION OF RAIN BY GRAUPEL, RH (A13)  (MISSING IN R)                        EXMOISG.430
               IF (RAIN(I,K).GT.R1) THEN                                         EXMOISG.431
                  PGACR(I,K)=(CRG*RONV(I,K)*GONV(I,K)/RHOd(I,K))                 EXMOISG.432
     +                 *ABS(VT2G-VT2R)*(5.*SLOR(I,K)**6*SLOG(I,K)+2.             EXMOISG.433
     +                 *SLOR(I,K)**5*SLOG(I,K)**2+0.5*SLOR(I,K)**4               EXMOISG.434
     +                 *SLOG(I,K)**3)                                            EXMOISG.435
               ENDIF                                                             EXMOISG.436
            ENDIF                                                                EXMOISG.437
C ICE MULTIPLICATION PROCESS, MRI (11-21)  (R-A.24,25)                           EXMOISG.438
            IF(PGACW(I,K).GT.0..AND.TAOUT(I,K).GE.265..AND.TAOUT(I,K).LE         EXMOISG.439
     +            .270.) THEN                                                    EXMOISG.440
               TF=0.                                                             EXMOISG.441
               IF(TAOUT(I,K).GE.268..AND.TAOUT(I,K).LE.270.)                     EXMOISG.442
     +              TF=(TAOUT(I,K)-270.)/(268.-270.)                             EXMOISG.443
               IF(TAOUT(I,K).GE.265..AND.TAOUT(I,K).LT.268.)                     EXMOISG.444
     +              TF=(TAOUT(I,K)-265.)/(268.-265.)                             EXMOISG.445
               NI_RS(I,K)=RHOd(I,K)*3.5E8*TF*PGACW(I,K)                          23DEC04.3353
               PISPL(I,K)=NI_RS(I,K)*XM01/RHOd(I,K)                              EXMOISG.447
            ENDIF                                                                EXMOISG.448
C DEPOSITION OF CLOUD ICE: MRI (11-24)  (R-A.26)                                 EXMOISG.449
            IF (ICE(I,K).GT.R1.AND.TAOUT(I,K).LT.TO) THEN                        EXMOISG.450
               IF(IT.LE.-1.AND.IT.GT.-31)THEN                                    EXMOISG.451
                  A2=ABER2(ABS(IT))                                              EXMOISG.452
                  A1=ABER1(ABS(IT))*(1.*0.001)**(1.-A2)                          EXMOISG.453
               ELSEIF(IT.LE.-31)THEN                                             EXMOISG.454
                  A2=ABER2(31)                                                   EXMOISG.455
                  A1=ABER1(31)*(1.*0.001)**(1.-A2)                               EXMOISG.456
               ENDIF                                                             EXMOISG.457
               PRD(I,K)=(QAOUT(I,K)-QVSI(I,K))/(QVS(I,K)-QVSI(I,K))              EXMOISG.458
     +              *A1*XMI**A2*NCON(I,K)/RHOd(I,K)                              EXMOISG.459
            ENDIF                                                                EXMOISG.460
C...DEPOSITION/SUBLIMATION OF SNOW: DUDHIA (B14)  (R-A.36)                       EXMOISG.461
            IF(SNOW(I,K).GT.R1.AND.TAOUT(I,K).LT.TO)THEN                         EXMOISG.462
               PREI(I,K)=DEPS1*SONV(I,K)*(QAOUT(I,K)/QVSI(I,K)-1.)               EXMOISG.471
     +              *(0.65*SLOS(I,K)*SLOS(I,K)+((DEPS2*RHO(I,K)                  EXMOISG.472
     +              /DUM21(I,K))**0.5)*DEPS3*0.84*SLOS(I,K)**(DEPS4))            19DEC02.1551
     +              /ABI(I,K) - DUM11(I,K)*PSACW(I,K)                            EXMOISG.474
                                                                                 EXMOISG.475
C+---+-----------------------------------------------------------------+         23DEC04.3354
C  changed snow to graupel conversion to require riming growth exceed            23DEC04.3355
C  depositional growth by factor 3.0 (Murakami required a 1.0 factor             23DEC04.3356
C  while RH1984 required qc>0.5 and qs>0.1 - similar to 5.0 factor)              23DEC04.3357
               if (psacw(i,k).gt.3.0*prei(i,k)) then                             23DEC04.3358
                  pgemb(i,k)=dsnow/(dgraupel-dsnow) * psacw(i,k)                 19DEC02.1557
                  pgsacw(i,k)=psacw(i,k)-pgemb(i,k)                              19DEC02.1558
                  pssacw(i,k)=0.                                                 19DEC02.1559
               else                                                              19DEC02.1560
                  pgemb(i,k)=0.                                                  19DEC02.1561
                  pgsacw(i,k)=0.                                                 19DEC02.1562
                  pssacw(i,k) = psacw(i,k)                                       19DEC02.1563
               endif                                                             19DEC02.1564
C+---+-----------------------------------------------------------------+         19DEC02.1565
            ENDIF                                                                EXMOISG.490
                                                                                 EXMOISG.491
C SUBLIMATION/DEPOSITIONAL GROWTH OF GRAUPEL, RH (A17)  (R-A.57)                 EXMOISG.492
            IF(GRAUPEL(I,K).GT.R1.AND.TAOUT(I,K).LT.TO)THEN                      EXMOISG.493
               PREG(I,K)=DEPG1*GONV(I,K)*(QAOUT(I,K)/QVSI(I,K)-1.)               EXMOISG.495
     +              *(0.78*SLOG(I,K)*SLOG(I,K)+((DEPG2*RHO(I,K)                  23DEC04.3359
     +              /DUM21(I,K))**0.5)*DEPG3*SLOG(I,K)**(DEPG4))                 23DEC04.3360
     +              /ABI(I,K) - DUM11(I,K)*PGACW(I,K)                            EXMOISG.498
            ENDIF                                                                EXMOISG.504
            FUDGEF = 0.95                                                        23DEC04.3361
            SUM_DEP = PRI(I,K)+PRD(I,K)+PREI(I,K)+PREG(I,K)                      EXMOISG.506
            IF( (SUPICE.GT.0. .AND. SUM_DEP.GT.SUPICE*FUDGEF) .                  EXMOISG.507
     +      OR. (SUPICE.LT.0. .AND. SUM_DEP.LT.SUPICE*FUDGEF) ) THEN             EXMOISG.508
               PRI(I,K) = FUDGEF*PRI(I,K)*SUPICE/SUM_DEP                         EXMOISG.509
               PRD(I,K) = FUDGEF*PRD(I,K)*SUPICE/SUM_DEP                         EXMOISG.510
               PREI(I,K) = FUDGEF*PREI(I,K)*SUPICE/SUM_DEP                       EXMOISG.511
               PREG(I,K) = FUDGEF*PREG(I,K)*SUPICE/SUM_DEP                       EXMOISG.512
            ENDIF                                                                EXMOISG.513
C RIMING OF CLOUD ICE, MRI (11-25A)   (R-A.27,28A)                               EXMOISG.514
            UDCLO=3.E7*DCLO*DCLO                                                 EXMOISG.515
            RPHI=DCLO*(DRAIN*VT2I/(3.24E-4*DIACE))**0.5                          EXMOISG.516
            IF(RPHI.GE.0.2704.AND.CLOUD(I,K).GT.R1.AND                           EXMOISG.517
     +           . DIACE.GT.100.E-6.AND.TAOUT(I,K).LT.TO)THEN                    23DEC04.3362
               EIC1=0.572*ALOG10(RPHI-0.25)+0.967                                EXMOISG.519
               EIC1=AMIN1(EIC1,0.5)                                              23DEC04.3363
               PIACW1=PI/4.*NCON(I,K)*(DIACE+DCLO)**2*EIC1                       EXMOISG.520
     +              *ABS(VT2I-UDCLO)*CLOUD(I,K)                                  EXMOISG.521
               PRDTEMP=AMAX1(PRD(I,K),0.0)                                       EXMOISG.522
               PIACW(I,K)=AMIN1(PIACW1,PRDTEMP)                                  EXMOISG.523
C AMOUNT OF RIME CONVERTED INTO GRAUPEL, MRI (11-38)  (R-A.28B)                  EXMOISG.524
               PGIACW(I,K) = AMAX1(0.0, PIACW1-PRDTEMP)                          EXMOISG.525
               CLIC=AMAX1((PIACW1-PRDTEMP)/NCON(I,K),0.0)                        EXMOISG.526
C AMOUNT OF CLOUD ICE CONVERTED INTO GRAUPEL, MRI (11-36)  (R-A.39)              EXMOISG.527
               DMGI=XM0G-XMI                                                     EXMOISG.528
               IF(DMGI.GT.0.)THEN                                                EXMOISG.529
                  PICNG(I,K)=CLIC*ICE(I,K)*RHOd(I,K)/DMGI                        EXMOISG.530
               ENDIF                                                             EXMOISG.531
C REDUCE NUMBER CONC OF CLOUD ICE SINCE CONVERTED INTO GRAUPEL  (R-A.40)         EXMOISG.532
               NI_CG(I,K)=RHOd(I,K)/XM0G*(PICNG(I,K)+PGIACW(I,K))                EXMOISG.533
            ENDIF                                                                EXMOISG.534
            IF(ICE(I,K).GT.R1.AND.TAOUT(I,K).LT.TO)THEN                          EXMOISG.535
C CONVERSION OF CLOUD ICE TO SNOW, MRI (11-30)  (R-A.30A,30B)                    EXMOISG.536
               DEPAC = 0.                                                        EXMOISG.537
               IF (PRD(I,K).GT.0.0) THEN                                         EXMOISG.538
                  IF(XMI.LE.0.5*XM0S)THEN                                        EXMOISG.539
                     DEPAC=XMI/(XM0S-XMI)*(PRD(I,K)+PIACW(I,K))                  EXMOISG.540
                  ELSE                                                           EXMOISG.541
                     DEPAC=PRD(I,K)+PIACW(I,K)+(1.-0.5*XM0S/XMI)                 EXMOISG.542
     +                    *ICE(I,K)/(2.*DTSAVE)                                  23DEC04.3364
                  ENDIF                                                          EXMOISG.544
C REDUCE NI DUE TO ICE GROWING TO SNOW SIZES                                     EXMOISG.545
                  NI_DE(I,K) = DEPAC*(RHOd(I,K)/XM0S)                            EXMOISG.546
               ELSE                                                              EXMOISG.547
C REDUCE NI DUE TO EVAPORATION (MISSING IN R reference)                          EXMOISG.548
                  NI_EV(I,K) = -PRD(I,K)*(RHOd(I,K)/XMI)                         EXMOISG.549
               ENDIF                                                             EXMOISG.550
               PSAGI = 0.                                                        EXMOISG.551
C REDUCE NI DUE TO SMALL XTALS COLLIDING PRODUCE LARGER ONES (STILL ICE)         EXMOISG.552
C THIS IS MISSING IN REISNER PAPER BUT IS IN MRI (M-39)                          EXMOISG.553
C              C1_AG = C1*RHOd(I,K)*ICE(I,K)*RHO_FAC                             23DEC04.3365
               C1_AG = PI/6.*0.1*0.25*VT2I*DIACE*DIACE                           23DEC04.3366
C              NI_AG(I,K) = 0.5*C1_AG*NCON(I,K)                                  23DEC04.3367
               NI_AG(I,K) = 0.5*C1_AG*NCON(I,K)*NCON(I,K)                        23DEC04.3368
               IF (DIACE.LT.2.*XR0S) THEN                                        EXMOISG.556
C                 DTAU=-2.0/C1_AG*ALOG10((DIACE/(2.*XR0S))**3)                   23DEC04.3369
                  DTAU=-2.0/(C1_AG*NCON(I,K))                                    23DEC04.3370
     +                           *ALOG10((DIACE/(2.*XR0S))**3)                   23DEC04.3371
                  PSAGI=ICE(I,K)/DTAU                                            EXMOISG.558
               ENDIF                                                             EXMOISG.559
               PSCNI(I,K)=DEPAC+PSAGI                                            EXMOISG.560
C ADD TO ABOVE AMOUNT OF AGGREG THAT CONVERTS ICE TO SNOW (R-A.31)               EXMOISG.561
               NI_AG(I,K) = NI_AG(I,K) + PSAGI*(RHOd(I,K)/XMI)                   EXMOISG.562
            ENDIF                                                                EXMOISG.563
            IF (TAOUT(I,K).GT.TO) THEN                                           EXMOISG.594
C REMEMBER THE SIGNS ARE REVERSED TO-TAOUT                                       EXMOISG.595
C GO BACK AND CHECK FOR CONSERVATION                                             EXMOISG.596
               XLATF=XLS-DUM31(I,K)                                              EXMOISG.597
               IF(SNOW(I,K).GT.R1)THEN                                           EXMOISG.598
C MELTING OF SNOW, RH1 (A23)  (R-A.37)                                           EXMOISG.599
                  PSMLT(I,K)=PSM1*SONV(I,K)*SCR4(I,K)/XLATF*                     EXMOISG.600
     +                 (TO-TAOUT(I,K))*(0.65*SLOS(I,K)*SLOS(I,K)+                EXMOISG.601
     +                 ((PSM2*RHO(I,K)/DUM21(I,K))**0.5)*PSM3                    EXMOISG.602
     +                 *SLOS(I,K)**PSM4)                                         EXMOISG.603
                                                                                 EXMOISG.604
C EVAPORATION OF MELTING SNOW FOR T > TO, RH1 (A27)                              EXMOISG.605
                  IF(QAOUT(I,K)-QVS(I,K).LT.0.)THEN                              EXMOISG.606
                     PMLTEV(I,K)=DEPS1*SONV(I,K)*(QAOUT(I,K)/QVS(I,K)            EXMOISG.607
     +                    -1.)*(0.65*SLOS(I,K)*SLOS(I,K)+((DEPS2                 EXMOISG.608
     +                    *RHO(I,K)/DUM21(I,K))**0.5)*DEPS3*0.84                 19DEC02.1568
     +                    *SLOS(I,K)**(DEPS4))/ABW(I,K)                          EXMOISG.610
                  ENDIF                                                          EXMOISG.613
               ENDIF                                                             EXMOISG.614
               IF(GRAUPEL(I,K).GT.R1)THEN                                        EXMOISG.615
C MELTING OF GRAUPEL, RH (A18)  (R-A.58)                                         EXMOISG.616
                  PGMLT(I,K)=PGM1*GONV(I,K)*SCR4(I,K)/XLATF*                     EXMOISG.617
     +                 (TO-TAOUT(I,K))*(0.78*SLOG(I,K)*SLOG(I,K)+                EXMOISG.618
     +                 ((PGM2*RHO(I,K)/DUM21(I,K))**0.5)*PGM3                    EXMOISG.619
     +                 *SLOG(I,K)**PGM4)                                         EXMOISG.620
                                                                                 EXMOISG.621
C EVAPORATION OF MELTING GRAUPEL, RH (A19)                                       EXMOISG.622
                  IF(QAOUT(I,K)-QVS(I,K).LT.0.)THEN                              EXMOISG.623
                     PMLTGE(I,K)=DEPG1*GONV(I,K)*(QAOUT(I,K)                     EXMOISG.624
     +                    /QVS(I,K)-1.)*(0.78*SLOG(I,K)*SLOG(I,K)                EXMOISG.625
     +                    +((DEPG2*RHO(I,K)/DUM21(I,K))**0.5)*DEPG3              EXMOISG.626
     +                    *SLOG(I,K)**(DEPG4))/ABW(I,K)                          EXMOISG.627
                  ENDIF                                                          EXMOISG.630
C SHEDDING OF ACCRETED WATER, RH (A20)                                           EXMOISG.631
C SUM OF PGACR(I,K) AND PGACW(I,K) ADDED TO TEND                                 EXMOISG.632
C ENHANCED MELTING OF GRAUPEL BY COLLECTION OF RAIN WATER, RH (A21)              EXMOISG.633
C ENHANCED MELTING OF GRAUPEL BY COLLECTION OF CLOUD WATER, RH (A22)             EXMOISG.634
C SIGNS ARE REVERSED  (TO-TAOUT)                                                 EXMOISG.635
                  PGACRM(I,K)=CW/XLATF*(TO-TAOUT(I,K))*PGACR(I,K)                EXMOISG.636
                  PGACWM(I,K)=CW/XLATF*(TO-TAOUT(I,K))*PGACW(I,K)                EXMOISG.637
               ENDIF                                                             EXMOISG.638
            ENDIF                                                                EXMOISG.639
C702        CONTINUE                                                             19DEC02.1569
 29      CONTINUE                                                                EXMOISG.641
 30   CONTINUE                                                                   EXMOISG.642
                                                                                 EXMOISG.643
      DO 40 K=1,KL                                                               EXMOISG.644
         DO 39 I=IST,IEN                                                         EXMOISG.645
            PRE(I,K)=AMAX1(-RAIN(I,K)/DTSAVE,PRE(I,K))                           19DEC02.1570
            PREI(I,K)=AMAX1(-SNOW(I,K)/DTSAVE,PREI(I,K))                         19DEC02.1571
            PREG(I,K)=AMAX1(-GRAUPEL(I,K)/DTSAVE,PREG(I,K))                      19DEC02.1572
            PRD(I,K)=AMAX1(-ICE(I,K)/DTSAVE,PRD(I,K))                            19DEC02.1573
C CHECKS TO SEE WHETHER GRAUPEL OR SNOW IS FORMED, SEE RH  (R-A.51)              EXMOISG.650
            ALPSNOW=AMAX1(1.E-25,DSNOW**2*(4.*SLOS(I,K))**6)                     23DEC04.3372
            ALPRAIN=AMAX1(1.E-25,DRAIN**2*(4.*SLOR(I,K))**6)                     23DEC04.3373
            ALPHARS=ALPSNOW/(ALPSNOW+ALPRAIN)                                    23DEC04.3374
CCCCC       alphars = 0.0                                                        23DEC04.3375
            D1=1.                                                                EXMOISG.654
            IF (RAIN(I,K).GT.R1) THEN                                            EXMOISG.655
               thetah = 0.5*pi*(1. - 1.e4*min(rain(i,k),1.e-4))                  EXMOISG.656
               D1 = SIN(thetah)                                                  EXMOISG.657
            ENDIF                                                                EXMOISG.658
                                                                                 EXMOISG.659
            IF(TAOUT(I,K).LE.TO)THEN                                             EXMOISG.660
CONSERVATION OF QC                                                               EXMOISG.661
               sum_terms=(PRC(I,K)+PRA(I,K)+PSACW(I,K)+PGACW(I,K)+               EXMOISG.662
     +              PIFCW(I,K)+PIACW(I,K)+PGIACW(I,K))*DTSAVE                    19DEC02.1578
               IF(sum_terms.GE.CLOUD(I,K).AND.CLOUD(I,K).GT.R1)THEN              EXMOISG.664
                  ratio = cloud(i,k)/sum_terms                                   EXMOISG.665
                  PIFCW(I,K)=ratio*PIFCW(I,K)                                    EXMOISG.666
                  PRC(I,K)=ratio*PRC(I,K)                                        EXMOISG.667
                  PRA(I,K)=ratio*PRA(I,K)                                        EXMOISG.668
                  PSACW(I,K)=ratio*PSACW(I,K)                                    EXMOISG.669
                  PGSACW(I,K)=ratio*PGSACW(I,K)                                  EXMOISG.670
                  PSSACW(I,K)=ratio*PSSACW(I,K)                                  EXMOISG.671
                  PGEMB(I,K)=ratio*PGEMB(I,K)                                    EXMOISG.672
                  PGACW(I,K)=ratio*PGACW(I,K)                                    EXMOISG.673
                  PIACW(I,K)=ratio*PIACW(I,K)                                    EXMOISG.674
                  PGIACW(I,K)=ratio*PGIACW(I,K)                                  EXMOISG.675
               ENDIF                                                             EXMOISG.676
CONSERVATION OF CLOUD ICE                                                        EXMOISG.677
               sum_terms=(PSCNI(I,K)+PRAI(I,K)-PRI(I,K)-PRD(I,K)                 EXMOISG.678
     +              +PRACI(I,K)-PIFCW(I,K)-PIACW(I,K)                            EXMOISG.679
     +              +PICNG(I,K)-PISPL(I,K))*DTSAVE                               19DEC02.1579
               IF(sum_terms.GE.ICE(I,K).AND.ICE(I,K).GT.R1)THEN                  EXMOISG.681
                  ratio = ice(i,k)/sum_terms                                     EXMOISG.682
                  PSCNI(I,K)=ratio*PSCNI(I,K)                                    EXMOISG.683
                  PRAI(I,K)=ratio*PRAI(I,K)                                      EXMOISG.684
                  PRI(I,K)=ratio*PRI(I,K)                                        EXMOISG.685
                  PRD(I,K)=ratio*PRD(I,K)                                        EXMOISG.686
                  PRACI(I,K)=ratio*PRACI(I,K)                                    EXMOISG.687
                  PIFCW(I,K)=ratio*PIFCW(I,K)                                    EXMOISG.688
                  PIACW(I,K)=ratio*PIACW(I,K)                                    EXMOISG.689
                  PICNG(I,K)=ratio*PICNG(I,K)                                    EXMOISG.690
                  PISPL(I,K)=ratio*PISPL(I,K)                                    EXMOISG.691
               ENDIF                                                             EXMOISG.692
CONSERVATION OF RAIN                                                             EXMOISG.693
               sum_terms=(PIACR(I,K)+PSACR(I,K)+PGACR(I,K)-PRC(I,K)              EXMOISG.694
     +              -PRA(I,K)+PGFR(I,K)-PRE(I,K))*DTSAVE                         19DEC02.1580
               IF(sum_terms.GE.RAIN(I,K).AND.RAIN(I,K).GT.R1)THEN                EXMOISG.696
                  ratio = rain(i,k)/sum_terms                                    EXMOISG.697
                  PIACR(I,K)=ratio*PIACR(I,K)                                    EXMOISG.698
                  PSACR(I,K)=ratio*PSACR(I,K)                                    EXMOISG.699
                  PGACR(I,K)=ratio*PGACR(I,K)                                    EXMOISG.700
                  PGFR(I,K)=ratio*PGFR(I,K)                                      EXMOISG.701
                  PRC(I,K)=ratio*PRC(I,K)                                        EXMOISG.702
                  PRA(I,K)=ratio*PRA(I,K)                                        EXMOISG.703
                  PRE(I,K)=ratio*PRE(I,K)                                        EXMOISG.704
               ENDIF                                                             EXMOISG.705
CONSERVATION OF SNOW                                                             EXMOISG.706
               sum_terms=(PRACS(I,K)*(1.-ALPHARS)+PGEMB(I,K)                     EXMOISG.707
     +              -PREI(I,K)-PSCNI(I,K)-PRAI(I,K)-                             EXMOISG.708
     +              PIACR(I,K)*D1-PRACI(I,K)*D1-PSACR(I,K)*ALPHARS-              EXMOISG.709
     +              PSSACW(I,K))*DTSAVE                                          19DEC02.1581
               IF(sum_terms.GE.SNOW(I,K).AND.SNOW(I,K).GT.R1)THEN                EXMOISG.711
                  ratio = snow(i,k)/sum_terms                                    EXMOISG.712
                  PRACS(I,K)=ratio*PRACS(I,K)                                    EXMOISG.713
                  PGEMB(I,K)=ratio*PGEMB(I,K)                                    EXMOISG.714
                  PSCNI(I,K)=ratio*PSCNI(I,K)                                    EXMOISG.715
                  PRAI(I,K)=ratio*PRAI(I,K)                                      EXMOISG.716
                  PIACR(I,K)=ratio*PIACR(I,K)                                    EXMOISG.717
                  PRACI(I,K)=ratio*PRACI(I,K)                                    EXMOISG.718
                  PSACR(I,K)=ratio*PSACR(I,K)                                    EXMOISG.719
                  PSSACW(I,K)=ratio*PSSACW(I,K)                                  EXMOISG.720
                  PREI(I,K)=ratio*PREI(I,K)                                      EXMOISG.721
               ENDIF                                                             EXMOISG.722
CONSERVATION OF GRAUPEL                                                          EXMOISG.723
               sum_terms=-(PGACW(I,K)+PGACR(I,K)+                                EXMOISG.724
     +              PREG(I,K)+(1.-D1)*PRACI(I,K)+(1.-D1)*                        EXMOISG.725
     +              PIACR(I,K)+(1.-ALPHARS)*PSACR(I,K)+(1.-ALPHARS)*             EXMOISG.726
     +              PRACS(I,K)+PGEMB(I,K)+PGSACW(I,K)+                           EXMOISG.727
     +              PGFR(I,K)+PICNG(I,K)+PGIACW(I,K))*DTSAVE                     19DEC02.1582
               IF(sum_terms.GE.GRAUPEL(I,K).AND.GRAUPEL(I,K).GT.R1)THEN          EXMOISG.729
                  ratio = graupel(i,k)/sum_terms                                 EXMOISG.730
                  PGACW(I,K)=ratio*PGACW(I,K)                                    EXMOISG.731
                  PGACR(I,K)=ratio*PGACR(I,K)                                    EXMOISG.732
                  PREG(I,K)=ratio*PREG(I,K)                                      EXMOISG.733
                  PRACI(I,K)=ratio*PRACI(I,K)                                    EXMOISG.734
                  PIACR(I,K)=ratio*PIACR(I,K)                                    EXMOISG.735
                  PSACR(I,K)=ratio*PSACR(I,K)                                    EXMOISG.736
                  PRACS(I,K)=ratio*PRACS(I,K)                                    EXMOISG.737
                  PGEMB(I,K)=ratio*PGEMB(I,K)                                    EXMOISG.738
                  PGSACW(I,K)=ratio*PGSACW(I,K)                                  EXMOISG.739
                  PGFR(I,K)=ratio*PGFR(I,K)                                      EXMOISG.740
                  PICNG(I,K)=ratio*PICNG(I,K)                                    EXMOISG.741
                  PGIACW(I,K)=ratio*PGIACW(I,K)                                  EXMOISG.742
               ENDIF                                                             EXMOISG.743
                                                                                 EXMOISG.744
               PSACW(I,K) = PSSACW(I,K) + PGSACW(I,K) + PGEMB(I,K)               19DEC02.1583
               PISPL_FS = 0.                                                     EXMOISG.748
               PISPL_FG = 0.                                                     EXMOISG.749
               IF (PISPL(I,K).GT.R1 .AND.                                        EXMOISG.750
     +               (PSACW(I,K)+PGACW(I,K)).GT.0.) THEN                         EXMOISG.751
                  PISPL_FS=PISPL(I,K)*(PSACW(I,K)/(PSACW(I,K)                    EXMOISG.752
     +                     +PGACW(I,K)))                                         EXMOISG.753
                  PISPL_FG=PISPL(I,K)*(PGACW(I,K)/(PSACW(I,K)                    EXMOISG.754
     +                     +PGACW(I,K)))                                         EXMOISG.755
               ELSE                                                              EXMOISG.756
                  PISPL(I,K) = 0.0                                               EXMOISG.757
               ENDIF                                                             EXMOISG.758
                                                                                 EXMOISG.759
C WATER VAPOR TENDENCY                                                           EXMOISG.760
               QV3DTEN(I,J,K)=QV3DTEN(I,J,K)-(PRE(I,K)+PREI(I,K)                 EXMOISG.761
     +              +PRI(I,K)+PRD(I,K)+PREG(I,K))*PSB(I,J)                       EXMOISG.762
C CLOUD WATER TENDENCY                                                           EXMOISG.763
               QC3DTEN(I,J,K)=QC3DTEN(I,J,K)-(PRC(I,K)+PRA(I,K)                  EXMOISG.764
     +              +PIACW(I,K)+PSACW(I,K)+PGACW(I,K)+PIFCW(I,K)                 EXMOISG.765
     +              +PGIACW(I,K))*PSB(I,J)                                       EXMOISG.766
C RAIN TENDENCY                                                                  EXMOISG.767
               QR3DTEN(I,J,K)=QR3DTEN(I,J,K)+(PRC(I,K)+PRA(I,K)                  EXMOISG.768
     +              +PRE(I,K)-PGACR(I,K)-PSACR(I,K)-PIACR(I,K)                   EXMOISG.769
     +              -PGFR(I,K))*PSB(I,J)                                         EXMOISG.770
C CLOUD ICE TENDENCY                                                             EXMOISG.771
               QI3DTEN(I,J,K)=QI3DTEN(I,J,K)-(PSCNI(I,K)+PRAI(I,K)               EXMOISG.772
     +              -PRI(I,K)-PRD(I,K)+PRACI(I,K)-PIFCW(I,K)                     EXMOISG.773
     +              -PIACW(I,K)+PICNG(I,K)-PISPL(I,K))*PSB(I,J)                  EXMOISG.774
C SNOW TENDENCY                                                                  EXMOISG.775
               QNI3DTEN(I,J,K)=QNI3DTEN(I,J,K)+(PREI(I,K)+PSCNI(I,K)             EXMOISG.776
     +              +PRAI(I,K)+D1*PRACI(I,K)+D1*PIACR(I,K)+ALPHARS*              EXMOISG.777
     +              PSACR(I,K)-(1.-ALPHARS)*PRACS(I,K)+PSSACW(I,K)               EXMOISG.778
     +              -PGEMB(I,K)-PISPL_FS)*PSB(I,J)                               EXMOISG.779
                                                                                 EXMOISG.780
C GRAUPEL TENDENCY                                                               EXMOISG.781
               QG3DTEN(I,J,K)=QG3DTEN(I,J,K)+(PGACW(I,K)-PISPL_FG                EXMOISG.782
     +              +PGACR(I,K)+PREG(I,K)+(1.-D1)*PRACI(I,K)                     EXMOISG.783
     +              +(1.-D1)*PIACR(I,K)+(1.-ALPHARS)*PSACR(I,K)                  EXMOISG.784
     +              +(1.-ALPHARS)*PRACS(I,K)+PGEMB(I,K)+PGSACW(I,K)              EXMOISG.785
     +              +PGFR(I,K)+PICNG(I,K)+PGIACW(I,K))*PSB(I,J)                  EXMOISG.786
C NUMBER OF CLOUD ICE TEND                                                       EXMOISG.787
C QNCTEN SHOULD BE #/KG                                                          19DEC02.1584
               QNC3DTEN(I,J,K)=QNC3DTEN(I,J,K)+(((PRI(I,K)                       19DEC02.1585
     +              +PIFCW(I,K))*RHOd(I,K)/XM01-(PRAI(I,K)                       EXMOISG.790
     +              +PRACI(I,K))                                                 EXMOISG.791
     +              *NCON(I,K)/ICE(I,K)+NI_RS(I,K)-NI_CG(I,K)                    EXMOISG.792
     +              -NI_AG(I,K)-NI_DE(I,K)-NI_EV(I,K))*PSB(I,J)                  EXMOISG.793
     +              )/RHOd(I,K)                                                  19DEC02.1586
                                                                                 EXMOISG.794
C SET CONSTRAINTS  (R-A.68)                                                      EXMOISG.795
               TEMP_ICE = AMAX1(R1,QI3D(I,J,K)+DTSAVE*QI3DTEN(I,J,K)             19DEC02.1587
     +              *RPSB(I,J))                                                  EXMOISG.797
               TEMP_NC  = AMAX1(R1,QNC3D(I,J,K)+DTSAVE*QNC3DTEN(I,J,K)           19DEC02.1588
     +              *RPSB(I,J))                                                  EXMOISG.799
               IF (TEMP_ICE.GT.R1) THEN                                          EXMOISG.800
CCCCC             RLU=RHOd(I,K)*TEMP_ICE/XM01                                    19DEC02.1589
                  RLU=TEMP_ICE/XM01                                              19DEC02.1590
                  IF(TEMP_NC.GT.RLU) QNC3DTEN(I,J,K)=(PSB(I,J)*RLU               EXMOISG.802
     +                 - PSB(I,J)*QNC3D(I,J,K))/DTSAVE                           19DEC02.1591
               ELSEIF (TEMP_ICE.LE.R1 .AND. TEMP_NC.GT.R1) THEN                  EXMOISG.804
                  QNC3DTEN(I,J,K) = -1.*(TEMP_NC/DTSAVE)*PSB(I,J)                19DEC02.1592
               ENDIF                                                             EXMOISG.806
               XLATF=XLS-DUM31(I,K)                                              EXMOISG.807
               TEMP=-XLS*(PREI(I,K)+PRD(I,K)+PRI(I,K)+PREG(I,K))                 EXMOISG.808
     +              *PSB(I,J)-DUM31(I,K)*PRE(I,K)*PSB(I,J)-XLATF                 EXMOISG.809
     +              *(PSACW(I,K)+PIACR(I,K)+PSACR(I,K)+PGACR(I,K)                EXMOISG.810
     +              +PGACW(I,K)+PIFCW(I,K)+PIACW(I,K)+PGFR(I,K)                  EXMOISG.811
     +              +PGIACW(I,K))*PSB(I,J)                                       EXMOISG.812
               PRD(I,K)=CP*(1.+0.887*QAOUT(I,K))                                 EXMOISG.813
               T3DTEN(I,J,K)=T3DTEN(I,J,K)-TEMP/PRD(I,K)*(1-IFDRY)               EXMOISG.814
C+---+-----------------------------------------------------------------+         EXMOISG.815
C-- ABOVE 0 C CASE BELOW HERE                                                    EXMOISG.816
C+---+-----------------------------------------------------------------+         EXMOISG.817
            ELSE                                                                 EXMOISG.818
CONSERVATION OF QC                                                               EXMOISG.819
               sum_terms=(PRC(I,K)+PRA(I,K)+PSACW(I,K)+PGACW(I,K))               19DEC02.1593
     -                    *DTSAVE                                                19DEC02.1594
               IF(sum_terms.GE.CLOUD(I,K).AND.CLOUD(I,K).GT.R1)THEN              EXMOISG.821
                  ratio = cloud(i,k)/sum_terms                                   EXMOISG.822
                  PRC(I,K)=ratio*PRC(I,K)                                        EXMOISG.823
                  PRA(I,K)=ratio*PRA(I,K)                                        EXMOISG.824
                  PSACW(I,K)=ratio*PSACW(I,K)                                    EXMOISG.825
                  PGACW(I,K)=ratio*PGACW(I,K)                                    EXMOISG.826
               ENDIF                                                             EXMOISG.827
CONSERVATION OF QR                                                               EXMOISG.828
               sum_terms=-(PRC(I,K)+PRA(I,K)+PRE(I,K)-PGMLT(I,K)                 EXMOISG.829
     +              -PSMLT(I,K)-PGACRM(I,K)-PGACWM(I,K)+PSACW(I,K)+              EXMOISG.830
     +              PRACS(I,K)+PGACW(I,K))*DTSAVE                                19DEC02.1595
               IF(sum_terms.GE.RAIN(I,K).AND.RAIN(I,K).GT.R1)THEN                EXMOISG.832
                  ratio = rain(i,k)/sum_terms                                    EXMOISG.833
                  PRC(I,K)=ratio*PRC(I,K)                                        EXMOISG.834
                  PRA(I,K)=ratio*PRA(I,K)                                        EXMOISG.835
                  PRE(I,K)=ratio*PRE(I,K)                                        EXMOISG.836
                  PGMLT(I,K)=ratio*PGMLT(I,K)                                    EXMOISG.837
                  PSMLT(I,K)=ratio*PSMLT(I,K)                                    EXMOISG.838
                  PGACRM(I,K)=ratio*PGACRM(I,K)                                  EXMOISG.839
                  PGACWM(I,K)=ratio*PGACWM(I,K)                                  EXMOISG.840
                  PSACW(I,K)=ratio*PSACW(I,K)                                    EXMOISG.841
                  PRACS(I,K)=ratio*PRACS(I,K)                                    EXMOISG.842
                  PGACW(I,K)=ratio*PGACW(I,K)                                    EXMOISG.843
               ENDIF                                                             EXMOISG.844
C              IF(IIWARM)GOTO 703                                                19DEC02.1596
CONSERVATION SNOW                                                                EXMOISG.846
               sum_terms=(PRACS(I,K)-PMLTEV(I,K)-PSMLT(I,K))*DTSAVE              19DEC02.1597
               IF(sum_terms.GE.SNOW(I,K).AND.SNOW(I,K).GT.R1)THEN                EXMOISG.848
                  ratio = snow(i,k)/sum_terms                                    EXMOISG.849
                  PMLTEV(I,K)=ratio*PMLTEV(I,K)                                  EXMOISG.850
                  PSMLT(I,K)=ratio*PSMLT(I,K)                                    EXMOISG.851
                  PRACS(I,K)=ratio*PRACS(I,K)                                    EXMOISG.852
               ENDIF                                                             EXMOISG.853
CONSERVATION OF GRAUPEL                                                          EXMOISG.854
               sum_terms=(-PGMLT(I,K)-PGACRM(I,K)-PGACWM(I,K)                    EXMOISG.855
     +              -PMLTGE(I,K))*DTSAVE                                         19DEC02.1598
               IF(sum_terms.GE.GRAUPEL(I,K).AND.GRAUPEL(I,K).GT.R1)THEN          EXMOISG.857
                  ratio = graupel(i,k)/sum_terms                                 EXMOISG.858
                  PGMLT(I,K)=ratio*PGMLT(I,K)                                    EXMOISG.859
                  PGACRM(I,K)=ratio*PGACRM(I,K)                                  EXMOISG.860
                  PGACWM(I,K)=ratio*PGACWM(I,K)                                  EXMOISG.861
                  PMLTGE(I,K)=ratio*PMLTGE(I,K)                                  EXMOISG.862
               ENDIF                                                             EXMOISG.863
C703           CONTINUE                                                          19DEC02.1599
                                                                                 EXMOISG.865
C WATER VAPOR TENDENCY                                                           EXMOISG.866
               QV3DTEN(I,J,K)=QV3DTEN(I,J,K)-(PRE(I,K)+PMLTEV(I,K)               EXMOISG.867
     +              +PMLTGE(I,K))*PSB(I,J)                                       EXMOISG.868
C CLOUD WATER TENDENCY                                                           EXMOISG.869
               QC3DTEN(I,J,K)=QC3DTEN(I,J,K)-(PRC(I,K)+PRA(I,K)                  EXMOISG.870
     +              +PSACW(I,K)+PGACW(I,K))*PSB(I,J)                             EXMOISG.871
C RAIN TENDENCY                                                                  EXMOISG.872
               QR3DTEN(I,J,K)=QR3DTEN(I,J,K)+(PRC(I,K)+PRA(I,K)                  EXMOISG.873
     +              +PRE(I,K)-PGMLT(I,K)-PSMLT(I,K)-PGACRM(I,K)                  EXMOISG.874
     +              -PGACWM(I,K)+PSACW(I,K)+PRACS(I,K)                           EXMOISG.875
     +              +PGACW(I,K))*PSB(I,J)                                        EXMOISG.876
C              IF(IIWARM)GOTO 704                                                19DEC02.1600
C SNOW TENDENCY                                                                  EXMOISG.878
               QNI3DTEN(I,J,K)=QNI3DTEN(I,J,K)+(PSMLT(I,K)                       EXMOISG.879
     +              -PRACS(I,K)+PMLTEV(I,K))*PSB(I,J)                            EXMOISG.880
C GRAUPEL TENDENCY                                                               EXMOISG.881
               QG3DTEN(I,J,K)=QG3DTEN(I,J,K)+(PGMLT(I,K)                         EXMOISG.882
     +              +PGACRM(I,K)+PGACWM(I,K)+PMLTGE(I,K))*PSB(I,J)               EXMOISG.883
C704           CONTINUE                                                          19DEC02.1601
               XLATF=XLS-DUM31(I,K)                                              EXMOISG.885
               TEMP=-XLATF*(PGMLT(I,K)+PSMLT(I,K)+PGACWM(I,K)                    EXMOISG.886
     +              +PGACRM(I,K)-PRACS(I,K))*PSB(I,J)-DUM31(I,K)                 EXMOISG.887
     +              *(PRE(I,K)+PMLTEV(I,K)+PMLTGE(I,K))*PSB(I,J)                 EXMOISG.888
               PRD(I,K)=CP*(1.+0.887*QAOUT(I,K))                                 EXMOISG.889
               T3DTEN(I,J,K)=T3DTEN(I,J,K)-TEMP/PRD(I,K)*(1-IFDRY)               EXMOISG.890
            ENDIF                                                                EXMOISG.891
 39      CONTINUE                                                                EXMOISG.892
 40   CONTINUE                                                                   EXMOISG.893
C+---+-----------------------------------------------------------------+         19DEC02.1602
#if defined (CALL_EVERY_OTHER_TSTEP)
C TEMP TIMESTEP HOLDING VARIABLE FOR CALLING ROUTINE EVERY OTHER TSTEP           19DEC02.1604
C.. Next section deals with the every other timestep issue                       19DEC02.1605
                                                                                 19DEC02.1606
      do k = 1, KL                                                               19DEC02.1607
         do i = IST, IEN                                                         19DEC02.1608
            tex_ten(i,j,k)   = t3dten(i,j,k)   - tex_ten(i,j,k)                  19DEC02.1609
            qvex_ten(i,j,k)  = qv3dten(i,j,k)  - qvex_ten(i,j,k)                 19DEC02.1610
            qcex_ten(i,j,k)  = qc3dten(i,j,k)  - qcex_ten(i,j,k)                 19DEC02.1611
            qrex_ten(i,j,k)  = qr3dten(i,j,k)  - qrex_ten(i,j,k)                 19DEC02.1612
            qiex_ten(i,j,k)  = qi3dten(i,j,k)  - qiex_ten(i,j,k)                 19DEC02.1613
            qniex_ten(i,j,k) = qni3dten(i,j,k) - qniex_ten(i,j,k)                19DEC02.1614
            qgex_ten(i,j,k)  = qg3dten(i,j,k)  - qgex_ten(i,j,k)                 19DEC02.1615
            qncex_ten(i,j,k) = qnc3dten(i,j,k) - qncex_ten(i,j,k)                19DEC02.1616
         enddo                                                                   19DEC02.1617
      enddo                                                                      19DEC02.1618
                                                                                 19DEC02.1619
      else                                                                       19DEC02.1620
                                                                                 19DEC02.1621
      do k = 1, KL                                                               19DEC02.1622
         do i = IST, IEN                                                         19DEC02.1623
            t3dten(i,j,k)   = t3dten(i,j,k)   + tex_ten(i,j,k)                   19DEC02.1624
            qv3dten(i,j,k)  = qv3dten(i,j,k)  + qvex_ten(i,j,k)                  19DEC02.1625
            qc3dten(i,j,k)  = qc3dten(i,j,k)  + qcex_ten(i,j,k)                  19DEC02.1626
            qr3dten(i,j,k)  = qr3dten(i,j,k)  + qrex_ten(i,j,k)                  19DEC02.1627
            qi3dten(i,j,k)  = qi3dten(i,j,k)  + qiex_ten(i,j,k)                  19DEC02.1628
            qni3dten(i,j,k) = qni3dten(i,j,k) + qniex_ten(i,j,k)                 19DEC02.1629
            qg3dten(i,j,k)  = qg3dten(i,j,k)  + qgex_ten(i,j,k)                  19DEC02.1630
            qnc3dten(i,j,k) = qnc3dten(i,j,k) + qncex_ten(i,j,k)                 19DEC02.1631
         enddo                                                                   19DEC02.1632
      enddo                                                                      19DEC02.1633
                                                                                 19DEC02.1634
      endif                                                                      19DEC02.1635
                                                                                 19DEC02.1636
#endif                                                                           19DEC02.1637
C+---+-----------------------------------------------------------------+         19DEC02.1638
                                                                                 EXMOISG.894
CLOUD NEXT PART IS TO GET RID OF EXCESS WATER VAPOR IF                           EXMOISG.895
C SS WITH RESPECT TO WATER, AS IN ANTHES AND WARNER                              EXMOISG.896
C FOLLOWS EXMOISS OF DUDHIA WITH NO MODIFICATION                                 EXMOISG.897
C---COMPUTE T, QV, AND QC AT TAU+1 WITHOUT CONDENSATIONAL TERM:                  EXMOISG.898
      DO 50 K=1,KL                                                               EXMOISG.899
         DO 49 I=IST,IEN                                                         EXMOISG.900
            DUM11(I,K)=QV3D(I,J,K)+DT*QV3DTEN(I,J,K)*RPSC(I,J)                   EXMOISG.901
            SCR4(I,K)=AMAX1(1.E-12,DUM11(I,K))                                   EXMOISG.902
            DUM21(I,K)=QC3D(I,J,K)+DT*QC3DTEN(I,J,K)*RPSC(I,J)                   EXMOISG.903
            SCR3(I,K)=AMAX1(0.,DUM21(I,K))                                       EXMOISG.904
            SCR7(I,K)=T3D(I,J,K)+DT*T3DTEN(I,J,K)*RPSC(I,J)                      EXMOISG.905
 49      CONTINUE                                                                EXMOISG.906
 50   CONTINUE                                                                   EXMOISG.907
                                                                                 EXMOISG.908
C----COMPUTE THE CONDENSATIONAL TERM:                                            EXMOISG.909
      DO 60 K=1,KL                                                               EXMOISG.910
         DO 59 I=IST,IEN                                                         EXMOISG.911
            PRD(I,K)=CP*(1.+0.887*QAOUT(I,K))                                    19DEC02.1639
            DUM21(I,K)=DUM31(I,K)*DUM31(I,K)/(RV*PRD(I,K))                       EXMOISG.912
            PRC(I,K)=RSLF(PRES(I,K),SCR7(I,K))                                   EXMOISG.913
 59      CONTINUE                                                                EXMOISG.914
 60   CONTINUE                                                                   EXMOISG.915
                                                                                 EXMOISG.916
      DO 70 K=1,KL                                                               EXMOISG.917
         DO 69 I=IST,IEN                                                         EXMOISG.918
            R2 = R1*PSB(I,J)                                                     EXMOISG.919
            SCR8(I,K)=(SCR4(I,K)-PRC(I,K))/(1.+DUM21(I,K)*PRC(I,K)               EXMOISG.920
     +           /(SCR7(I,K)*SCR7(I,K)))                                         EXMOISG.921
            DUM11(I,K)=SCR3(I,K)+SCR8(I,K)                                       EXMOISG.922
            IF(DUM11(I,K).GE.0)THEN                                              EXMOISG.923
               SCR6(I,K)=SCR8(I,K)/DT                                            EXMOISG.924
            ELSE                                                                 EXMOISG.925
               SCR6(I,K)=-SCR3(I,K)/DT                                           EXMOISG.926
            ENDIF                                                                EXMOISG.927
            QV3DTEN(I,J,K)=QV3DTEN(I,J,K)-PSB(I,J)*SCR6(I,K)                     EXMOISG.928
C...INITIATION OF CLOUD WATER:                                                   EXMOISG.929
            QC3DTEN(I,J,K)=QC3DTEN(I,J,K)+PSB(I,J)*SCR6(I,K)                     EXMOISG.930
            DUM21(I,K)=DUM31(I,K)/PRD(I,K)                                       EXMOISG.931
            T3DTEN(I,J,K)=T3DTEN(I,J,K)+PSB(I,J)*SCR6(I,K)                       EXMOISG.932
     +             *DUM21(I,K)*(1-IFDRY)                                         EXMOISG.933
C--COMPUTE P*T AND P*QR (WITHOUT FALLOUT TERM) AT TAU+1:                         EXMOISG.934
            SCR4R(I,K)=AMAX1(R2,QR3D(I,J,K)*PSB(I,J)                             EXMOISG.935
     +                  +DT*QR3DTEN(I,J,K))                                      EXMOISG.936
            SCR4S(I,K)=AMAX1(R2,QNI3D(I,J,K)*PSB(I,J)                            EXMOISG.937
     +                  +DT*QNI3DTEN(I,J,K))                                     EXMOISG.938
            SCR4G(I,K)=AMAX1(R2,QG3D(I,J,K)*PSB(I,J)                             EXMOISG.939
     +                  +DT*QG3DTEN(I,J,K))                                      EXMOISG.940
            SCR4I(I,K)=AMAX1(R2,QI3D(I,J,K)*PSB(I,J)                             EXMOISG.941
     +                  +DT*QI3DTEN(I,J,K))                                      EXMOISG.942
            SCR4N(I,K)=AMAX1(R2,QNC3D(I,J,K)*PSB(I,J)                            EXMOISG.943
     +                  +DT*QNC3DTEN(I,J,K))                                     EXMOISG.944
            SCR6(I,K)=T3D(I,J,K)+DT*T3DTEN(I,J,K)*RPSC(I,J)                      EXMOISG.945
 69      CONTINUE                                                                EXMOISG.946
 70   CONTINUE                                                                   EXMOISG.947
                                                                                 EXMOISG.948
C--COMPUTE THE FALLOUT TERMS:   (USE MINIMUM FALL SPEED OF MIXING RATIO=1.E-7)   EXMOISG.949
C     COMPUTE FALL TERM WITH SHORTER TIME STEPS WHERE VFALL>DZ/DT                EXMOISG.950
      R3 = 1.E-7                                                                 EXMOISG.951
      DO 80 I=IST,IEN                                                            EXMOISG.952
         NSTEP=0                                                                 EXMOISG.953
         DO 90 K=1,KL                                                            EXMOISG.954
            RHO3=PRES(I,K)/(R*SCR6(I,K))                                         EXMOISG.955
            RHO_FAC=(RHO_NOT/RHO3)**.5                                           EXMOISG.956
            UPDT_S = AMAX1(R3,SCR4S(I,K)*RPSC(I,J))                              EXMOISG.957
            UPDT_G = AMAX1(R3,SCR4G(I,K)*RPSC(I,J))                              EXMOISG.958
            UPDT_R = AMAX1(R3,SCR4R(I,K)*RPSC(I,J))                              EXMOISG.959
            UPDT_I = AMAX1(R3,SCR4I(I,K)*RPSC(I,J))                              EXMOISG.960
            UPDT_N = AMAX1(R3,SCR4N(I,K)*RPSC(I,J))                              EXMOISG.961
                                                                                 EXMOISG.962
CCCCC       SONV(I,K) = SON                                                      19DEC02.1640
CCCCC       IF (UPDT_S.GT. R3) THEN                                              19DEC02.1641
CCCCC          SONV(I,K) = (CONST1A*(DRAIN/(RHO3*UPDT_S*CONST1B)                 19DEC02.1642
CCCCC+              *(PI*DSNOW/(RHO3*UPDT_S))**(BS/4))**0.94)                    19DEC02.1643
CCCCC+              **(4./(4-0.94*BS))                                           19DEC02.1644
CCCCC          SONV(I,K) = MIN(SONV(I,K),SON)                                    19DEC02.1645
CCCCC       ENDIF                                                                19DEC02.1646
                                                                                 19DEC02.1647
C.. New SONV formulation based on Fig. 7, curve_3 of Houze et al 1979            19DEC02.1648
           temp_C = amin1(-0.001, SCR6(I,K)-273.15)                              19DEC02.1649
           sonv(i,k) = amin1(2.0E8, 2.0E6*exp(-0.12*temp_C))                     19DEC02.1650
                                                                                 19DEC02.1651
            SONV(I,K) = SONV(I,K)/SON                                            EXMOISG.970
                                                                                 EXMOISG.971
            GONV(I,K) = GON                                                      EXMOISG.972
            IF (UPDT_G.GT. R3) THEN                                              EXMOISG.973
               GONV(I,K) = 2.38*(PI*DGRAUPEL/                                    EXMOISG.974
     +              (RHO3*UPDT_G))**0.92                                         EXMOISG.975
               GONV(I,K) = MAX(1.E4, MIN(GONV(I,K),GON))                         EXMOISG.976
            ENDIF                                                                EXMOISG.977
            GONV(I,K) = GONV(I,K)/GON                                            EXMOISG.978
                                                                                 EXMOISG.979
            RONV(I,K) = RON2                                                     EXMOISG.980
            IF (UPDT_R.GT. R3) THEN                                              EXMOISG.981
C.. New RONV formulation                                                         EXMOISG.982
               ronv(i,k) = ron_const1r*tanh((ron_qr0-UPDT_R)                     EXMOISG.983
     +                /ron_delqr0) + ron_const2r                                 EXMOISG.984
            ENDIF                                                                EXMOISG.985
            RONV(I,K) = RONV(I,K)/RON                                            19DEC02.1652
                                                                                 EXMOISG.987
            SLOR1=(RHO3*UPDT_R/(TOPR*RONV(I,K)))**0.25                           EXMOISG.988
            SLOS1=(RHO3*UPDT_S/(TOPS*SONV(I,K)))**0.25                           EXMOISG.989
            SLOG1=(RHO3*UPDT_G/(TOPG*GONV(I,K)))**0.25                           EXMOISG.990
            VT2R=(FRAIN*SLOR1**BR)*RHO_FAC                                       EXMOISG.991
            VT2R=AMIN1(VT2R, 9.0)                                                23DEC04.3377
            VT2S=(FSNOW*SLOS1**BS)*RHO_FAC                                       EXMOISG.992
            VT2S=AMIN1(VT2S, 2.0)                                                23DEC04.3378
            VT2G=(FGRAUPEL*SLOG1**BG)*RHO_FAC                                    EXMOISG.993
            VT2G=AMIN1(VT2G, 10.0)                                               23DEC04.3379
            DIAMI=(6.*RHOd(I,K)*UPDT_I/(PI*DICE*UPDT_N))**0.3333                 23DEC04.3380
            DIAMI=AMAX1(AMIN1(0.001, DIAMI), DIACE_min)                          23DEC04.3381
            VT2I=700. * DIAMI * RHO_FAC                                          23DEC04.3382
            VT2I=AMAX1(AMIN1(1.0,VT2I), 0.3)                                     23DEC04.3383
                                                                                 EXMOISG.997
C UNITS ARE G-M/S2, RHO3-KG/M3,VT2-M/S,PSC-CB*1000=PA                            EXMOISG.998
            RHOFAC(K)=RHO0(I,J,K)/RHO3                                           EXMOISG.999
            FR(K)=G*VT2R*RHO3/(PSB(I,J)*1000.)                                   EXMOISG.1000
            FS(K)=G*VT2S*RHO3/(PSB(I,J)*1000.)                                   EXMOISG.1001
            FG(K)=G*VT2G*RHO3/(PSB(I,J)*1000.)                                   EXMOISG.1002
            FI(K)=G*VT2I*RHO3/(PSB(I,J)*1000.)                                   EXMOISG.1003
            RGVM=AMAX1(FR(K),FS(K),FG(K),FI(K))                                  EXMOISG.1004
C 1 IS TO ROUND UP, REPRESENTS NUMBER OF STEPS                                   EXMOISG.1005
            IF (RGVM.GT.0.0)                                                     EXMOISG.1006
     +           NSTEP=MAX0(INT(RGVM*DT/DSIGMA(K)+1.),NSTEP)                     EXMOISG.1007
 90      CONTINUE                                                                EXMOISG.1008
                                                                                 EXMOISG.1009
      DO 100 N=1,NSTEP                                                           EXMOISG.1010
         DO 110 K=1,KL                                                           EXMOISG.1011
            FALOUTR(K)=FR(K)*SCR4R(I,K)                                          EXMOISG.1012
            FALOUTS(K)=FS(K)*SCR4S(I,K)                                          EXMOISG.1013
            FALOUTG(K)=FG(K)*SCR4G(I,K)                                          EXMOISG.1014
            FALOUTI(K)=FI(K)*SCR4I(I,K)                                          EXMOISG.1015
            FALOUTN(K)=FI(K)*SCR4N(I,K)                                          EXMOISG.1016
 110     CONTINUE                                                                EXMOISG.1017
C FOR TOP OF MODEL                                                               EXMOISG.1018
         K=1                                                                     EXMOISG.1019
         FALTNDR=RHOFAC(K)*FALOUTR(K)/DSIGMA(K)                                  EXMOISG.1020
         FALTNDS=RHOFAC(K)*FALOUTS(K)/DSIGMA(K)                                  EXMOISG.1021
         FALTNDG=RHOFAC(K)*FALOUTG(K)/DSIGMA(K)                                  EXMOISG.1022
         FALTNDI=RHOFAC(K)*FALOUTI(K)/DSIGMA(K)                                  EXMOISG.1023
         FALTNDN=RHOFAC(K)*FALOUTN(K)/DSIGMA(K)                                  EXMOISG.1024
         QR3DTEN(I,J,K)=QR3DTEN(I,J,K)-FALTNDR/NSTEP                             EXMOISG.1025
         QNI3DTEN(I,J,K)=QNI3DTEN(I,J,K)-FALTNDS/NSTEP                           EXMOISG.1026
         QG3DTEN(I,J,K)=QG3DTEN(I,J,K)-FALTNDG/NSTEP                             EXMOISG.1027
         QI3DTEN(I,J,K)=QI3DTEN(I,J,K)-FALTNDI/NSTEP                             EXMOISG.1028
         QNC3DTEN(I,J,K)=QNC3DTEN(I,J,K)-FALTNDN/NSTEP                           EXMOISG.1029
         SCR4R(I,K)=SCR4R(I,K)-FALTNDR*DT/NSTEP                                  EXMOISG.1030
         SCR4S(I,K)=SCR4S(I,K)-FALTNDS*DT/NSTEP                                  EXMOISG.1031
         SCR4G(I,K)=SCR4G(I,K)-FALTNDG*DT/NSTEP                                  EXMOISG.1032
         SCR4I(I,K)=SCR4I(I,K)-FALTNDI*DT/NSTEP                                  EXMOISG.1033
         SCR4N(I,K)=SCR4N(I,K)-FALTNDN*DT/NSTEP                                  EXMOISG.1034
         DO 120 K=2,KL                                                           EXMOISG.1035
            FALTNDR=RHOFAC(K)*(FALOUTR(K)-FALOUTR(K-1))/DSIGMA(K)                EXMOISG.1036
            FALTNDS=RHOFAC(K)*(FALOUTS(K)-FALOUTS(K-1))/DSIGMA(K)                EXMOISG.1037
            FALTNDG=RHOFAC(K)*(FALOUTG(K)-FALOUTG(K-1))/DSIGMA(K)                EXMOISG.1038
            FALTNDI=RHOFAC(K)*(FALOUTI(K)-FALOUTI(K-1))/DSIGMA(K)                EXMOISG.1039
            FALTNDN=RHOFAC(K)*(FALOUTN(K)-FALOUTN(K-1))/DSIGMA(K)                EXMOISG.1040
            QR3DTEN(I,J,K)=QR3DTEN(I,J,K)-FALTNDR/NSTEP                          EXMOISG.1041
            QNI3DTEN(I,J,K)=QNI3DTEN(I,J,K)-FALTNDS/NSTEP                        EXMOISG.1042
            QG3DTEN(I,J,K)=QG3DTEN(I,J,K)-FALTNDG/NSTEP                          EXMOISG.1043
            QI3DTEN(I,J,K)=QI3DTEN(I,J,K)-FALTNDI/NSTEP                          EXMOISG.1044
            QNC3DTEN(I,J,K)=QNC3DTEN(I,J,K)-FALTNDN/NSTEP                        EXMOISG.1045
            SCR4R(I,K)=SCR4R(I,K)-FALTNDR*DT/NSTEP                               EXMOISG.1046
            SCR4S(I,K)=SCR4S(I,K)-FALTNDS*DT/NSTEP                               EXMOISG.1047
            SCR4G(I,K)=SCR4G(I,K)-FALTNDG*DT/NSTEP                               EXMOISG.1048
            SCR4I(I,K)=SCR4I(I,K)-FALTNDI*DT/NSTEP                               EXMOISG.1049
            SCR4N(I,K)=SCR4N(I,K)-FALTNDN*DT/NSTEP                               EXMOISG.1050
 120     CONTINUE                                                                EXMOISG.1051
C     ACCUMULATED RAIN                                                           EXMOISG.1052
C 6000 CONVERTS FROM SEC TO MIN AND FROM M TO CM                                 EXMOISG.1053
         RAINNC(I,J)=RAINNC(I,J)+(FALOUTR(KL)+FALOUTS(KL)+FALOUTG(KL)            EXMOISG.1054
     +        +FALOUTI(KL)                                                       EXMOISG.1055
     +        )*DTMIN*6000./G/NSTEP                                              EXMOISG.1056
         SNOWNCT(I,J)=(FALOUTS(KL)+FALOUTI(KL)+FALOUTG(KL))                      EXMOISG.1057
     +                *DTMIN*6000./G/NSTEP                                       EXMOISG.1058
                                                                                 EXMOISG.1059
 100  CONTINUE                                                                   EXMOISG.1060
                                                                                 EXMOISG.1061
 80   CONTINUE                                                                   EXMOISG.1062
                                                                                 EXMOISG.1063
C     IF(IIWARM)GOTO 705                                                         19DEC02.1653
      DO 95 I=IST,IEN                                                            EXMOISG.1065
         DO 92 K=2,KL                                                            EXMOISG.1066
C MELTING OF CLOUD ICE                                                           EXMOISG.1067
            RHO3=PRES(I,K)/(R*SCR6(I,K))                                         EXMOISG.1068
            IF(SCR6(I,K).GT.TO.AND.(QI3DTEN(I,J,K).GT.0.                         EXMOISG.1069
     +           OR.QI3D(I,J,K).GT.0.)) THEN                                     EXMOISG.1070
               XLATF=XLS-DUM31(I,K)                                              EXMOISG.1071
               DUM11(I,K)=AMAX1(QI3D(I,J,K)*PSB(I,J)+DT*QI3DTEN(I,J,K)           EXMOISG.1072
     +              ,0.)                                                         EXMOISG.1073
               QI3DTEN(I,J,K)=-QI3D(I,J,K)*PSB(I,J)/DT                           EXMOISG.1074
               QC3DTEN(I,J,K)=QC3DTEN(I,J,K)+DUM11(I,K)/DT                       EXMOISG.1075
               QNC3DTEN(I,J,K)=-QNC3D(I,J,K)*PSB(I,J)/DT                         EXMOISG.1076
               T3DTEN(I,J,K)=T3DTEN(I,J,K)-XLATF/PRD(I,K)                        EXMOISG.1077
     +              *DUM11(I,K)/DT*(1-IFDRY)                                     EXMOISG.1078
            ENDIF                                                                EXMOISG.1079
C HOMOGENOUS FREEZING OF CLOUD ICE  (R-A.23)                                     EXMOISG.1080
            IF(SCR6(I,K).LT.HGFR.AND.(QC3DTEN(I,J,K).GT.0.OR.                    EXMOISG.1081
     +           QC3D(I,J,K).GT.0.)) THEN                                        EXMOISG.1082
               XLATF=XLS-DUM31(I,K)                                              EXMOISG.1083
               DUM11(I,K)=AMAX1(QC3D(I,J,K)*PSB(I,J)+DT*QC3DTEN(I,J,K)           EXMOISG.1084
     +              ,0.)                                                         EXMOISG.1085
               QC3DTEN(I,J,K)=-QC3D(I,J,K)*PSB(I,J)/DT                           EXMOISG.1086
               QI3DTEN(I,J,K)=QI3DTEN(I,J,K)+DUM11(I,K)/DT                       EXMOISG.1087
CCCCC          QNC3DTEN(I,J,K)=QNC3DTEN(I,J,K)+(RHO3*DUM11(I,K)                  19DEC02.1654
CCCCC+              /XM01)/DT                                                    19DEC02.1655
               QNC3DTEN(I,J,K)=QNC3DTEN(I,J,K)+(DUM11(I,K)/XM01)/DT              19DEC02.1656
               T3DTEN(I,J,K)=T3DTEN(I,J,K)+XLATF/PRD(I,K)                        EXMOISG.1090
     +              *DUM11(I,K)/DT*(1-IFDRY)                                     EXMOISG.1091
            ENDIF                                                                EXMOISG.1092
 92      CONTINUE                                                                EXMOISG.1093
 95   CONTINUE                                                                   EXMOISG.1094
C705  CONTINUE                                                                   19DEC02.1657
                                                                                 EXMOISG.1096
      RETURN                                                                     EXMOISG.1097
      END                                                                        EXMOISG.1098
