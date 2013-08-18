#include <defines.incl>                                                          07NOV00.1482
      SUBROUTINE EXMOISR(QC3DTEN,QR3DTEN,QI3DTEN,QNI3DTEN,QV3DTEN,               EXMOISR.1
     &                   T3DTEN,T3D,QV3D,QC3D,QR3D,QI3D,QNI3D,PP3D,              EXMOISR.2
     &                   PSB,RPSB,RAINNC,SNOWNCT,IN,J,IST,IEN)                   05DEC01.717
      IMPLICIT NONE                                                              07NOV00.1484
                                                                                 EXMOISR.4
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          EXMOISR.5
C                                                                     C          EXMOISR.6
C                                                                     C          EXMOISR.7
C     THIS SUBROUTINE COMPUTES THE MOISTURE TENDENCIES                C          EXMOISR.8
C     THE PROGNOSTIC EQUATIONS INCLUDE                                C          EXMOISR.9
C     WATER VAPOR (QV3DTEN), CLOUD WATER (QC3DTEN), RAIN WATER        C          EXMOISR.10
C     (QR3DTEN), CLOUD ICE (QI3DTEN), AND SNOW (QNI3DTEN)             C          EXMOISR.11
C                                                                     C          EXMOISR.12
C     IN = 1 : FOR LARGE DOMAIN                                       C          EXMOISR.13
C        = 2 : FOR NESTED DOMAIN                                      C          EXMOISR.14
C                                                                     C          EXMOISR.15
C     ALL THE OTHER ARGUMENTS ARE PASSED FROM SUBROUTINE "SOLVE" AND  C          EXMOISR.16
C     EXPLAINED IN "SOLVE".                                           C          EXMOISR.17
C                                                                     C          EXMOISR.18
C     CHANGES                                                         C          EXMOISR.19
C       JRG COMMON BLOCK MADE CONSISTENT WITH EXMOISG                 C          EXMOISR.20
C       VALUE OF CNP CHANGED                                          C          EXMOISR.21
C                                                                     C          19DEC02.1364
C     ADD FLAG (MEYERS) FOR USE OF MEYERS ICE NUCLEI CONC. EQ         C          19DEC02.1365
C     IN PLACE OF FLETCHER EQ                                         C          19DEC02.1366
C                                                                     C          19DEC02.1367
C                                                                     C          EXMOISR.22
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          EXMOISR.23
C_FLIC_BEGIN_NOFLIC                                                              07NOV00.1485
#     include <parame.incl>                                                      EXMOISR.25
#     include <varia.incl>                                                       EXMOISR.26
#     include <param3.incl>                                                      EXMOISR.27
#     include <param2.incl>                                                      EXMOISR.28
#     include <pmoist.incl>                                                      EXMOISR.29
#     include <various.incl>                                                     EXMOISR.30
#     include <nhcnst.incl>                                                      05DEC01.718
C_FLIC_END_NOFLIC                                                                07NOV00.1486
#     include <jrg.incl>                                                         EXMOISR.33
      REAL RPSB(MIX,MJX)                                                         07NOV00.1487
      REAL RPSC(MIX,MJX)                                                         07NOV00.1488
                                                                                 EXMOISR.34
C INPUT ARRAYS                                                                   EXMOISR.35
                                                                                 EXMOISR.36
      REAL  T3D(MIX,MJX,MKX),        QV3D(MIX,MJX,MKX),                          EXMOISR.37
     &      U3D(MIX,MJX,MKX),         V3D(MIX,MJX,MKX),                          EXMOISR.38
     &     QC3D(MIXM,MJXM,MKXM),     QR3D(MIXM,MJXM,MKXM),                       EXMOISR.39
     &     QI3D(MIXIC,MJXIC,MKXIC), QNI3D(MIXIC,MJXIC,MKXIC),                    EXMOISR.40
     &     PP3D(MIX,MJX,MKX),         PSB(MIX,MJX)                               EXMOISR.41
      REAL RAINNC(MIX,MJX),      SNOWNCT(MIX,MJX)                                05DEC01.719
                                                                                 EXMOISR.43
C ARRAYS FOR BASE VARIABLES                                                      EXMOISR.44
                                                                                 EXMOISR.45
      REAL TAOUT(MIX,MKX),PRES(MIX,MKX),RHO(MIX,MKX),                            EXMOISR.46
     1     T3DTEN(MIX,MJX,MKX),SCR7(MIX,MKX),SCR6(MIX,MKX)                       EXMOISR.47
                                                                                 07NOV00.1489
#ifndef VECTOR                                                                   07NOV00.1490
      REAL FR(MKX),FS(MKX),FI(MKX),RHOFAC(MKX)                                   05DEC01.720
      REAL FALOUTR(MKX),FALOUTS(MKX),FALOUTI(MKX)                                07NOV00.1492
#else                                                                            07NOV00.1493
      REAL FR(MIX,MKX),FS(MIX,MKX),FI(MIX,MKX),RHOFAC(MIX,MKX)                   05DEC01.721
      REAL FALOUTR(MIX,MKX),FALOUTS(MIX,MKX),FALOUTI(MIX,MKX)                    07NOV00.1495
#endif                                                                           07NOV00.1496
                                                                                 EXMOISR.48
C ARRAYS FOR QV                                                                  EXMOISR.49
                                                                                 EXMOISR.50
      REAL QAOUT(MIX,MKX),QVQVS(MIX,MKX),QVQVSI(MIX,MKX),                        EXMOISR.51
     1     QVS(MIX,MKX),QVSI(MIX,MKX),AB(MIX,MKX),                               EXMOISR.52
     1     ABI(MIX,MKX),ABW(MIX,MKX),QV3DTEN(MIX,MJX,MKX)                        EXMOISR.53
                                                                                 EXMOISR.54
C ARRAYS FOR QC                                                                  EXMOISR.55
                                                                                 EXMOISR.56
      REAL CLOUD(MIX,MKX),PRC(MIX,MKX),                                          EXMOISR.57
     1     NUFCI(MIX,MKX),PSACW(MIX,MKX),                                        EXMOISR.58
     1     PRA(MIX,MKX),QC3DTEN(MIXM,MJXM,MKXM),SCR3(MIX,MKX)                    EXMOISR.59
                                                                                 EXMOISR.60
C ARRAYS FOR QR                                                                  EXMOISR.61
                                                                                 EXMOISR.62
      REAL RAIN(MIX,MKX),SLOR(MIX,MKX),                                          EXMOISR.63
     1     PRE(MIX,MKX),                                                         EXMOISR.64
     1     QR3DTEN(MIXM,MJXM,MKXM),SCR4R(MIX,MKX)                                07NOV00.1497
                                                                                 EXMOISR.67
C ARRAYS FOR QI                                                                  EXMOISR.68
                                                                                 EXMOISR.69
      REAL ICE(MIX,MKX),PRI(MIX,MKX),PRCI(MIX,MKX),                              EXMOISR.70
     1     PRAI(MIX,MKX),                                                        EXMOISR.71
     1     PRD(MIX,MKX),QI3DTEN(MIXIC,MJXIC,MKXIC)                               EXMOISR.72
      REAL SCR4I(MIX,MKX)                                                        07NOV00.1498
                                                                                 EXMOISR.74
C ARRAYS FOR QS                                                                  EXMOISR.75
                                                                                 EXMOISR.76
      REAL SNOW(MIX,MKX),SLOS(MIX,MKX),                                          EXMOISR.77
     1     PREI(MIX,MKX),                                                        EXMOISR.78
     1     PSMLT(MIX,MKX),PMLTEV(MIX,MKX),QNI3DTEN(MIXIC,MJXIC,MKXIC),           EXMOISR.79
     1     SCR4S(MIX,MKX)                                                        07NOV00.1499
      REAL SONV(MIX,MKX)                                                         EXMOISR.81
                                                                                 EXMOISR.82
                                                                                 EXMOISR.83
C EXTRA ARRAYS                                                                   EXMOISR.84
                                                                                 EXMOISR.85
      REAL DUM11(MIX,MKX),DUM21(MIX,MKX),DUM31(MIX,MKX),                         EXMOISR.86
     1     SCR4(MIX,MKX),SCR8(MIX,MKX)                                           EXMOISR.87
C DECLARATIONS FOR IMPLICIT NONE                                                 07NOV00.1500
      INTEGER I,J,K                                                              07NOV00.1501
      INTEGER IST,IEN,IN,ISAT,N                                                  07NOV00.1502
#ifdef VECTOR                                                                    07NOV00.1503
      INTEGER NSTEP(MIX)                                                         07NOV00.1504
      LOGICAL MORETODO                                                           07NOV00.1505
#else                                                                            07NOV00.1506
      INTEGER NSTEP                                                              07NOV00.1507
#endif                                                                           07NOV00.1508
      REAL EVS,EIS,QVS1,QVI1,XNC,XMI,SUPICE,DI,XLATF,TEMP                        07NOV00.1509
      REAL RHO2,SLOR1,SLOS1,VT2R,VT2S,RHO3,VSED,RGVM                             07NOV00.1510
      REAL FALTNDR,FALTNDS,FALTNDI                                               07NOV00.1511
      real r1, r2                                                                19DEC02.1368
#ifdef KMA                                                                       07NOV00.1512
      real dt_rcp                                                                07NOV00.1513
      real rhosno, tmp1, tmp2                                                    07NOV00.1514
      real dum11_rcp                                                             07NOV00.1515
      real dsigma_rcp(mkx)                                                       07NOV00.1516
      real rcp_nstep                                                             07NOV00.1517
      real g_rcp                                                                 07NOV00.1518
#endif                                                                           07NOV00.1519
C                                                                                EXMOISR.88
C                                                                                19DEC02.1369
C ADD FLAG FOR MEYERS ICE NUCLEI CONC. EQ                                        19DEC02.1370
      INTEGER MEYERS                                                             19DEC02.1371
C MEYERS=1 : USE MEYERS ICE NUCLEI CONC. EQ.                                     19DEC02.1372
C MEYERS=0 : USE FLETCHER or cooper ICE NUCLEI CONC. EQ.                         19DEC02.1373
      MEYERS= 0                                                                  19DEC02.1374
      IF(IPOLAR.EQ.1)MEYERS=1                                                    19DEC02.1375
      r1 = 1.e-25                                                                19DEC02.1376
#ifdef KMA                                                                       07NOV00.1520
      dt_rcp = 1.0/dt                                                            07NOV00.1521
      g_rcp = 1.0/g                                                              05DEC01.722
#endif                                                                           07NOV00.1522
#ifndef VECTOR                                                                   07NOV00.1523
      DO I = IST, IEN                                                            07NOV00.1524
          RPSC(I,J) = RPSB(I,J)                                                  07NOV00.1525
      ENDDO                                                                      07NOV00.1526
#else                                                                            07NOV00.1527
      DO I = 1, MIX*MJX                                                          07NOV00.1528
          RPSC(I,J) = RPSB(I,J)                                                  07NOV00.1529
      ENDDO                                                                      07NOV00.1530
#endif                                                                           07NOV00.1531
                                                                                 07NOV00.1532
      DO 15 K=1,KX                                                               EXMOISR.89
        DO 15 I=2,ILXM                                                           EXMOISR.90
          TAOUT(I,K)=T3D(I,J,K)                                                  EXMOISR.91
          QAOUT(I,K)=QV3D(I,J,K)                                                 EXMOISR.92
C     V8 SVP FORMULA                                                             EXMOISR.93
C SATURATION VAPOR PRESSURE FOR T > TO (273 K)                                   EXMOISR.94
          EVS=SVP1*EXP(SVP2*(TAOUT(I,K)-SVPT0)/(TAOUT(I,K)-SVP3))                EXMOISR.95
C SATURATION VAPOR PRESSURE FOR T < TO (273 K)                                   EXMOISR.96
          EIS=.611*EXP(22.514-6.15E3/TAOUT(I,K))                                 EXMOISR.97
C QVS FOR T > TO                                                                 EXMOISR.98
          QVS1=.622*EVS/(PSB(I,J)*A(K)+PTOP+PP3D(I,J,K)*0.001-EVS)               EXMOISR.99
C QVS FOR T < TO                                                                 EXMOISR.100
          QVI1=.622*EIS/(PSB(I,J)*A(K)+PTOP+PP3D(I,J,K)*0.001-EIS)               EXMOISR.101
C QV/QVS FOR T > TO                                                              EXMOISR.102
          QVQVS(I,K)=QV3D(I,J,K)/QVS1                                            EXMOISR.103
C QV/QVS FOR T < TO                                                              EXMOISR.104
          QVQVSI(I,K)=QV3D(I,J,K)/QVI1                                           EXMOISR.105
   15   CONTINUE                                                                 EXMOISR.106
                                                                                 EXMOISR.107
C---BEGIN PRODUCTION TERMS CALCULATION:                                          EXMOISR.108
C    ALL THE PRODUCTION TERMS ARE BASED ON T-1 (I.E. XXB) VARIABLES              EXMOISR.109
      DO 20 K=1,KL                                                               EXMOISR.110
        DO 20 I=IST,IEN                                                          EXMOISR.111
          PRES(I,K)=(A(K)*PSB(I,J)+PTOP)*1000.+PP3D(I,J,K)                       EXMOISR.112
          RHO(I,K)=PRES(I,K)/(R*TAOUT(I,K))                                      EXMOISR.113
          CLOUD(I,K)=AMAX1(r1,QC3D(I,J,K))                                       19DEC02.1377
          ICE(I,K)=AMAX1(r1,QI3D(I,J,K))                                         19DEC02.1378
          RAIN(I,K)=AMAX1(r1,QR3D(I,J,K))                                        19DEC02.1379
          SNOW(I,K)=AMAX1(r1,QNI3D(I,J,K))                                       19DEC02.1380
          IF(SNOW(I,K).LE.1.E-8)THEN                                             EXMOISR.118
            SONV(I,K)=SON                                                        EXMOISR.119
          ELSE                                                                   EXMOISR.120
#ifdef KMA                                                                       07NOV00.1533
            rhosno= rho(i,k)*snow(i,k)                                           07NOV00.1534
            tmp1=exp(log(pi*dsnow/rhosno) * (bs * .25))                          07NOV00.1535
            tmp2=exp(log(const1a*(drain/(rhosno*const1b)*tmp1))*0.94)            07NOV00.1536
            SONV(I,K)=exp( log(tmp2) * (4.0 / (4-.94 * bs + 4.0)))               07NOV00.1537
#else                                                                            07NOV00.1538
                                                                                 07NOV00.1539
            SONV(I,K)=(CONST1A*(DRAIN/(RHO(I,K)*SNOW(I,K)*CONST1B)*(PI*          EXMOISR.121
     +                DSNOW/(RHO(I,K)*SNOW(I,K)))**(BS/4))**0.94)**(4./(         EXMOISR.122
     +                4.-0.94*BS))                                               07NOV00.1540
#endif                                                                           07NOV00.1541
          ENDIF                                                                  EXMOISR.124
          IF(SONV(I,K).GE.SON)SONV(I,K)=SON                                      EXMOISR.125
          SONV(I,K)=SONV(I,K)/SON                                                EXMOISR.126
          QVS(I,K)=QAOUT(I,K)/QVQVS(I,K)                                         EXMOISR.127
          QVSI(I,K)=QAOUT(I,K)/QVQVSI(I,K)                                       EXMOISR.128
          SLOR(I,K)=(RHO(I,K)*RAIN(I,K)/TOPR)**0.25                              EXMOISR.129
          SLOS(I,K)=(RHO(I,K)*SNOW(I,K)/(TOPS*SONV(I,K)))**0.25                  EXMOISR.130
          DUM11(I,K)=8.794E-5*TAOUT(I,K)**1.81/PRES(I,K)                         EXMOISR.131
          DUM21(I,K)=1.496E-6*TAOUT(I,K)**1.5/(TAOUT(I,K)+120.)                  EXMOISR.132
C     V8 LV FORMULA                                                              EXMOISR.133
          DUM31(I,K)=3.1484E6-XLV1*TAOUT(I,K)                                    EXMOISR.134
C THERMAL CONDUCTIVITY OF AIR                                                    EXMOISR.135
          SCR4(I,K)=1.414E3*DUM21(I,K)                                           EXMOISR.136
C A + B IN FORMULA (B7) OF DUDHIA (1992) FOR T > TO                              EXMOISR.137
          AB(I,K)=RHO(I,K)*XLS*XLS/(SCR4(I,K)*RV*TAOUT(I,K)*TAOUT(I,K))+         EXMOISR.138
     +            1./(QVS(I,K)*DUM11(I,K))                                       EXMOISR.139
C A + B FOR RAIN EVAP AND T < TO                                                 EXMOISR.140
          ABW(I,K)=RHO(I,K)*DUM31(I,K)*DUM31(I,K)/(SCR4(I,K)*RV*                 EXMOISR.141
     +             TAOUT(I,K)*TAOUT(I,K))+1./(QVS(I,K)*DUM11(I,K))               EXMOISR.142
C A + B FOR T < TO                                                               EXMOISR.143
          ABI(I,K)=RHO(I,K)*XLS*XLS/(SCR4(I,K)*RV*TAOUT(I,K)*TAOUT(I,K))         EXMOISR.144
     +             +1./(QVSI(I,K)*DUM11(I,K))                                    EXMOISR.145
C DYNAMIC VISCOSITY OF AIR                                                       EXMOISR.146
          PRE(I,K)=DUM21(I,K)/RHO(I,K)                                           EXMOISR.147
C THE SCHMIDT NUMBER                                                             EXMOISR.148
          PRC(I,K)=PRE(I,K)/DUM11(I,K)                                           EXMOISR.149
   20   CONTINUE                                                                 EXMOISR.150
                                                                                 EXMOISR.151
      DO 22 K=1,KL                                                               EXMOISR.152
        DO 22 I=IST,IEN                                                          EXMOISR.153
#ifdef KMA                                                                       07NOV00.1542
          dum11(i,k) = exp(.3333*log(prc(i,k))) / sqrt(pre(i,k))                 07NOV00.1543
#else                                                                            07NOV00.1544
          DUM11(I,K)=PRC(I,K)**.3333/PRE(I,K)**.5                                EXMOISR.154
#endif                                                                           07NOV00.1545
          PRI(I,K)=0.                                                            EXMOISR.155
          NUFCI(I,K)=0.                                                          EXMOISR.156
          PRCI(I,K)=0.                                                           EXMOISR.157
          PRAI(I,K)=0.                                                           EXMOISR.158
          PSACW(I,K)=0.                                                          EXMOISR.159
          PRD(I,K)=0.                                                            EXMOISR.160
          PREI(I,K)=0.                                                           EXMOISR.161
          PRC(I,K)=0.                                                            EXMOISR.162
          PRA(I,K)=0.                                                            EXMOISR.163
          PRE(I,K)=0.                                                            EXMOISR.164
          PSMLT(I,K)=0.                                                          EXMOISR.165
          PMLTEV(I,K)=0.                                                         EXMOISR.166
   22   CONTINUE                                                                 EXMOISR.167
                                                                                 EXMOISR.168
C FORMULAS COME FROM DUDHIA (1989, JAS, 46),                                     EXMOISR.169
C RUTLEDGE AND HOBBS (1983, JAS, 40), {RH},                                      EXMOISR.170
C AND METEOROLOGICAL RESEARCH INSTITUTE {MRI} OF JAPAN (#28, 1991)               EXMOISR.171
      DO 30 K=1,KL                                                               EXMOISR.172
        DO 30 I=IST,IEN                                                          EXMOISR.173
          ISAT=0                                                                 EXMOISR.174
          IF(MEYERS.EQ.1)THEN                                                    19DEC02.1381
C MEYERS ICE CONC. EQ.                                                           19DEC02.1382
            XNC=1000.*EXP(-0.639+0.1296*(100.*                                   19DEC02.1383
     +        (QVQVSI(I,K)-1.)))/RHO(I,K)                                        19DEC02.1384
           XNC=AMAX1(XNC,10000./RHO(I,K))                                        19DEC02.1385
          ELSE                                                                   19DEC02.1386
C FLETCHER CURVE                                                                 EXMOISR.175
            XNC=TNO*EXP(ATO*(TO-TAOUT(I,K)))/RHO(I,K)                            19DEC02.1387
          ENDIF                                                                  19DEC02.1388
          XMI=ICE(I,K)/XNC                                                       EXMOISR.177
          IF(TAOUT(I,K).GT.TO)XNC=0.                                             EXMOISR.178
C INITIATION OF CLOUD ICE, DUDHIA (B4)                                           EXMOISR.179
#ifdef KMA                                                                       07NOV00.1546
          SUPICE=(QAOUT(I,K)-QVSI(I,K)) * dt_rcp                                 07NOV00.1547
#else                                                                            07NOV00.1548
          SUPICE=(QAOUT(I,K)-QVSI(I,K))/DT                                       EXMOISR.180
#endif                                                                           07NOV00.1549
          IF(SUPICE.GT.0.)THEN                                                   EXMOISR.181
#ifdef KMA                                                                       07NOV00.1550
            PRI(I,K)=AMAX1(0.,(XM01*XNC-ICE(I,K))*dt_rcp)                        07NOV00.1551
#else                                                                            07NOV00.1552
            PRI(I,K)=AMAX1(0.,(XM01*XNC-ICE(I,K))/DT)                            EXMOISR.182
#endif                                                                           07NOV00.1553
            PRI(I,K)=AMIN1(PRI(I,K),SUPICE)                                      EXMOISR.183
            IF(PRI(I,K).GT.SUPICE*.99)ISAT=1                                     EXMOISR.184
          ENDIF                                                                  EXMOISR.185
          IF(TAOUT(I,K).LT.TO)THEN                                               EXMOISR.186
C FREEZING OF CLOUD DROPLETS, MRI (11-19)                                        EXMOISR.187
            IF(CLOUD(I,K).GT.r1)THEN                                             19DEC02.1389
              NUFCI(I,K)=BP*(EXP(AP*(TO-TAOUT(I,K)))-1.)*RHO(I,K)*               EXMOISR.189
     +                   CLOUD(I,K)**2/(CNP*DRAIN)                               EXMOISR.190
            ELSE                                                                 EXMOISR.191
              NUFCI(I,K)=0.                                                      EXMOISR.192
            ENDIF                                                                EXMOISR.193
          ENDIF                                                                  EXMOISR.194
C...ACCRETION OF CLOUD ICE BY SNOW, DUDHIA (B15):                                EXMOISR.195
          IF(SNOW(I,K).LE.r1)THEN                                                19DEC02.1390
            PRAI(I,K)=0.                                                         EXMOISR.197
          ELSE                                                                   EXMOISR.198
#ifdef KMA                                                                       07NOV00.1554
            prai(i,k)=acris * exp(log(slos(i,k))*bacris) *                       07NOV00.1555
     &               ice(i,k)*sonv(i,k)                                          07NOV00.1556
#else                                                                            07NOV00.1557
            PRAI(I,K)=ACRIS*SLOS(I,K)**BACRIS*ICE(I,K)*SONV(I,K)                 EXMOISR.199
#endif                                                                           07NOV00.1558
          ENDIF                                                                  EXMOISR.200
COLLECTION OF CLOUD WATER BY SNOW, RH (A22)                                      EXMOISR.201
          IF(SNOW(I,K).le.r1)THEN                                                19DEC02.1391
            PSACW(I,K)=0.                                                        EXMOISR.203
          ELSE                                                                   EXMOISR.204
#ifdef KMA                                                                       07NOV00.1559
            psacw(i,k)=acrcs * exp(log(slos(i,k))*bacrcs) *                      07NOV00.1560
     &                cloud(i,k)*sonv(i,k)                                       07NOV00.1561
#else                                                                            07NOV00.1562
            PSACW(I,K)=ACRCS*SLOS(I,K)**BACRCS*CLOUD(I,K)*SONV(I,K)              EXMOISR.205
#endif                                                                           07NOV00.1563
          ENDIF                                                                  EXMOISR.206
C...DEPOSITION/SUBLIMATION OF CLOUD ICE: DUDHIA (B7)                             EXMOISR.207
#ifdef KMA                                                                       07NOV00.1564
          DI=16.3*sqrt(XMI)                                                      07NOV00.1565
#else                                                                            07NOV00.1566
          DI=16.3*XMI**0.5                                                       EXMOISR.208
#endif                                                                           07NOV00.1567
          IF(ISAT.EQ.1)THEN                                                      EXMOISR.209
            PRD(I,K)=0.                                                          EXMOISR.210
          ELSE                                                                   EXMOISR.211
            PRD(I,K)=4.*DI*RHO(I,K)*SUPICE*DT*XNC/(QVSI(I,K)*ABI(I,K))           EXMOISR.212
          ENDIF                                                                  EXMOISR.213
          IF(PRD(I,K).LT.0.)THEN                                                 EXMOISR.214
            PRD(I,K)=AMAX1(PRD(I,K),SUPICE/2.)                                   EXMOISR.215
          ELSE                                                                   EXMOISR.216
            PRD(I,K)=AMIN1(PRD(I,K),SUPICE/2.)                                   EXMOISR.217
          ENDIF                                                                  EXMOISR.218
          IF(ABS(PRD(I,K)+PRI(I,K)).GT.ABS(.99*SUPICE))ISAT=1                    EXMOISR.219
C...AUTOCONVERSION OF CLOUD ICE TO SNOW, DUDHIA  (B9):                           19DEC02.1392
#ifdef KMA                                                                       07NOV00.1568
          PRCI(I,K)=AMAX1(0.,(ICE(I,K)-XSMAX*XNC)*DT_rcp)                        07NOV00.1569
#else                                                                            07NOV00.1570
          PRCI(I,K)=AMAX1(0.,(ICE(I,K)-XSMAX*XNC)/DT)                            EXMOISR.221
#endif                                                                           07NOV00.1571
C...DEPOSITION/SUBLIMATION OF SNOW: DUDHIA (B14)                                 EXMOISR.222
C DAVE                                                                           EXMOISR.223
C     IF(SNOW(I,K).GT.0.)                                                        EXMOISR.224
C    - PREI(I,K)=DAVET(0.,DEPS1*(QAOUT(I,K)/QVSI(I,K)-1.)                        EXMOISR.225
C    1 *(0.65*SLOS(I,K)*SLOS(I,K)                                                EXMOISR.226
C    1 +DEPS2**0.5*DEPS3*DUM11(I,K)                                              EXMOISR.227
C    1 *SLOS(I,K)**(DEPS4))/ABI(I,K),ISAT.EQ.1)                                  EXMOISR.228
          IF((SNOW(I,K).GT.r1).AND.(ISAT.EQ.1))THEN                              19DEC02.1393
            PREI(I,K)=0.                                                         EXMOISR.230
          ELSEIF(SNOW(I,K).GT.r1)THEN                                            19DEC02.1394
            PREI(I,K)=SONV(I,K)*DEPS1*(QAOUT(I,K)/QVSI(I,K)-1.)*(0.65*           EXMOISR.232
#ifdef KMA                                                                       07NOV00.1572
     +                SLOS(I,K)*SLOS(I,K)+sqrt(DEPS2)*DEPS3*DUM11(I,K)*          07NOV00.1573
     +                exp(log(SLOS(I,K))*(DEPS4)))/ABI(I,K)                      07NOV00.1574
#else                                                                            07NOV00.1575
     +                SLOS(I,K)*SLOS(I,K)+DEPS2**0.5*DEPS3*DUM11(I,K)*           EXMOISR.233
     +                SLOS(I,K)**(DEPS4))/ABI(I,K)                               EXMOISR.234
#endif                                                                           07NOV00.1576
          ENDIF                                                                  EXMOISR.235
          IF(PREI(I,K).LT.0.)THEN                                                EXMOISR.236
            PREI(I,K)=AMAX1(PREI(I,K),SUPICE/2.)                                 EXMOISR.237
          ELSE                                                                   EXMOISR.238
            PREI(I,K)=AMIN1(PREI(I,K),SUPICE/2.)                                 EXMOISR.239
          ENDIF                                                                  EXMOISR.240
C---AUTOCONVERSION OF CLOUD WATER TO RAINWATER: DUDHIA (B10)                     EXMOISR.241
          PRC(I,K)=AMAX1(0.,QCK1*(CLOUD(I,K)-QCTH))                              EXMOISR.242
C---ACCRETION OF CLOUD WATER BY RAINWATER: DUDHIA (B15)                          EXMOISR.243
          IF(RAIN(I,K).le.r1)THEN                                                19DEC02.1395
            PRA(I,K)=0.                                                          EXMOISR.245
          ELSE                                                                   EXMOISR.246
#ifdef KMA                                                                       07NOV00.1577
            PRA(I,K)=ACRCR* exp(log(SLOR(I,K))*BACRCR)*CLOUD(I,K)                07NOV00.1578
#else                                                                            07NOV00.1579
            PRA(I,K)=ACRCR*SLOR(I,K)**BACRCR*CLOUD(I,K)                          EXMOISR.247
#endif                                                                           07NOV00.1580
          ENDIF                                                                  EXMOISR.248
C...EVAPORATION OF RAINWATER: DUDHIA (B14)                                       EXMOISR.249
          IF(RAIN(I,K).GT.r1)                                                    19DEC02.1396
     &    PRE(I,K)=DEPR1*(QAOUT(I,K)/QVS(I,K)-1.)*(                              19DEC02.1397
#ifdef KMA                                                                       07NOV00.1581
     +      0.78*SLOR(I,K)*SLOR(I,K)+sqrt(DEPR2)*DEPR3*DUM11(I,K)*               07NOV00.1582
     +      exp(log(SLOR(I,K))*(DEPR4)))/ABW(I,K)                                07NOV00.1583
#else                                                                            07NOV00.1584
     +      0.78*SLOR(I,K)*SLOR(I,K)+DEPR2**0.5*DEPR3*DUM11(I,K)*                EXMOISR.251
     +      SLOR(I,K)**(DEPR4))/ABW(I,K)                                         EXMOISR.252
#endif                                                                           07NOV00.1585
C MELTING OF SNOW: RH (A25)                                                      EXMOISR.253
          XLATF=XLS-DUM31(I,K)                                                   EXMOISR.254
C DAVE                                                                           EXMOISR.255
C     PSMLT(I,K)=DAVEZ(0.,PSM1*SCR4(I,K)/(RHO(I,K)*XLATF)                        EXMOISR.256
C    1     *(TO-TAOUT(I,K))*(0.65*SLOS(I,K)*SLOS(I,K)                            EXMOISR.257
C    2     +DUM11(I,K)*PSM3*PSM2**0.5*SLOS(I,K)**PSM4)                           EXMOISR.258
C    3     ,SNOW(I,K))                                                           EXMOISR.259
          IF(SNOW(I,K).le.r1)THEN                                                19DEC02.1398
            PSMLT(I,K)=0.                                                        EXMOISR.261
          ELSE                                                                   EXMOISR.262
            PSMLT(I,K)=SONV(I,K)*PSM1*SCR4(I,K)/(RHO(I,K)*XLATF)*(TO-            EXMOISR.263
     +                 TAOUT(I,K))*(0.65*SLOS(I,K)*SLOS(I,K)+DUM11(I,K)*         EXMOISR.264
     +                 PSM3*PSM2**0.5*SLOS(I,K)**PSM4)                           EXMOISR.265
          ENDIF                                                                  EXMOISR.266
C EVAPORATION OF MELTING SNOW FOR T > TO, RH (A27)                               EXMOISR.267
C DAVE                                                                           EXMOISR.268
C     IF(SNOW(I,K).GT.0.)                                                        EXMOISR.269
C    - PMLTEV(I,K)=DAVET(0.,DEPS1*(QAOUT(I,K)/QVS(I,K)-1.)                       EXMOISR.270
C    1 *(0.65*SLOS(I,K)*SLOS(I,K)                                                EXMOISR.271
C    1 +DEPS2**0.5*DEPS3*DUM11(I,K)                                              EXMOISR.272
C    1 *SLOS(I,K)**(DEPS4))/AB(I,K),QAOUT(I,K)-QVS(I,K).GE.0.)                   EXMOISR.273
          IF((SNOW(I,K).GT.r1).AND.(QAOUT(I,K)-QVS(I,K).GE.0.))THEN              19DEC02.1399
            PMLTEV(I,K)=0.                                                       EXMOISR.275
          ELSEIF(SNOW(I,K).GT.r1)THEN                                            19DEC02.1400
#ifdef KMA                                                                       07NOV00.1586
          PMLTEV(I,K)=SONV(I,K)*DEPS1*(QAOUT(I,K)/QVS(I,K)-1.)*(0.65*            07NOV00.1587
     +            SLOS(I,K)*SLOS(I,K)+sqrt(DEPS2)*DEPS3*DUM11(I,K)*              07NOV00.1588
     +            exp(log( SLOS(I,K))*(DEPS4)))/AB(I,K)                          07NOV00.1589
#else                                                                            07NOV00.1590
            PMLTEV(I,K)=SONV(I,K)*DEPS1*(QAOUT(I,K)/QVS(I,K)-1.)*(0.65*          EXMOISR.277
     +                  SLOS(I,K)*SLOS(I,K)+DEPS2**0.5*DEPS3*DUM11(I,K)*         EXMOISR.278
     +                  SLOS(I,K)**(DEPS4))/AB(I,K)                              EXMOISR.279
#endif                                                                           07NOV00.1591
          ENDIF                                                                  EXMOISR.280
   30   CONTINUE                                                                 EXMOISR.283
                                                                                 EXMOISR.284
      DO 40 K=1,KL                                                               EXMOISR.285
        DO 40 I=IST,IEN                                                          EXMOISR.286
#ifdef KMA                                                                       07NOV00.1592
          PRE(I,K)=AMAX1(-RAIN(I,K)*DT_rcp,PRE(I,K))                             07NOV00.1593
          PREI(I,K)=AMAX1(-SNOW(I,K)*DT_rcp,PREI(I,K))                           07NOV00.1594
#else                                                                            07NOV00.1595
          PRE(I,K)=AMAX1(-RAIN(I,K)/DT,PRE(I,K))                                 EXMOISR.288
          PREI(I,K)=AMAX1(-SNOW(I,K)/DT,PREI(I,K))                               EXMOISR.289
#endif                                                                           07NOV00.1596
          IF(IEVAP(IN).LT.0)PRE(I,K)=AMAX1(PRE(I,K),0.)                          EXMOISR.291
          IF(IEVAP(IN).LT.0)PREI(I,K)=AMAX1(PREI(I,K),0.)                        EXMOISR.292
          IF(TAOUT(I,K).LE.TO)THEN                                               EXMOISR.293
CONSERVATION OF QC                                                               EXMOISR.294
            DUM11(I,K)=(PRC(I,K)+PRA(I,K)+PSACW(I,K)+NUFCI(I,K))*DT              EXMOISR.295
            IF(DUM11(I,K).GT.CLOUD(I,K))THEN                                     EXMOISR.296
#ifdef KMA                                                                       07NOV00.1597
              dum11_rcp = 1.0/dum11(i,k)                                         07NOV00.1598
              NUFCI(I,K)=CLOUD(I,K)*NUFCI(I,K) * dum11_rcp                       07NOV00.1599
              PRC(I,K)=CLOUD(I,K)*PRC(I,K) * dum11_rcp                           07NOV00.1600
              PRA(I,K)=CLOUD(I,K)*PRA(I,K) * dum11_rcp                           07NOV00.1601
              PSACW(I,K)=CLOUD(I,K)*PSACW(I,K) * dum11_rcp                       07NOV00.1602
#else                                                                            07NOV00.1603
              NUFCI(I,K)=CLOUD(I,K)*NUFCI(I,K)/DUM11(I,K)                        EXMOISR.297
              PRC(I,K)=CLOUD(I,K)*PRC(I,K)/DUM11(I,K)                            EXMOISR.298
              PRA(I,K)=CLOUD(I,K)*PRA(I,K)/DUM11(I,K)                            EXMOISR.299
              PSACW(I,K)=CLOUD(I,K)*PSACW(I,K)/DUM11(I,K)                        EXMOISR.300
#endif                                                                           07NOV00.1604
            ENDIF                                                                EXMOISR.301
CONSERVATION OF CLOUD ICE                                                        EXMOISR.302
            DUM11(I,K)=(PRCI(I,K)+PRAI(I,K)-PRI(I,K)-PRD(I,K)-NUFCI(I,K)         EXMOISR.303
     +                 )*DT                                                      EXMOISR.304
            IF(DUM11(I,K).GT.ICE(I,K))THEN                                       EXMOISR.305
#ifdef KMA                                                                       07NOV00.1605
              dum11_rcp = 1.0/dum11(i,k)                                         07NOV00.1606
              PRCI(I,K)=ICE(I,K)*PRCI(I,K) * dum11_rcp                           07NOV00.1607
              PRAI(I,K)=ICE(I,K)*PRAI(I,K) * dum11_rcp                           07NOV00.1608
              PRI(I,K)=ICE(I,K)*PRI(I,K) * dum11_rcp                             07NOV00.1609
              PRD(I,K)=ICE(I,K)*PRD(I,K) * dum11_rcp                             07NOV00.1610
              NUFCI(I,K)=ICE(I,K)*NUFCI(I,K) * dum11_rcp                         07NOV00.1611
#else                                                                            07NOV00.1612
              PRCI(I,K)=ICE(I,K)*PRCI(I,K)/DUM11(I,K)                            EXMOISR.306
              PRAI(I,K)=ICE(I,K)*PRAI(I,K)/DUM11(I,K)                            EXMOISR.307
              PRI(I,K)=ICE(I,K)*PRI(I,K)/DUM11(I,K)                              EXMOISR.308
              PRD(I,K)=ICE(I,K)*PRD(I,K)/DUM11(I,K)                              EXMOISR.309
              NUFCI(I,K)=ICE(I,K)*NUFCI(I,K)/DUM11(I,K)                          EXMOISR.310
#endif                                                                           07NOV00.1613
            ENDIF                                                                EXMOISR.311
            DUM11(I,K)=-(PRE(I,K)+PREI(I,K)+PRI(I,K)+PRD(I,K))*PSB(I,J)          EXMOISR.312
C WATER VAPOR TENDENCY                                                           EXMOISR.313
            QV3DTEN(I,J,K)=QV3DTEN(I,J,K)+DUM11(I,K)                             EXMOISR.314
C CLOUD WATER TENDENCY                                                           EXMOISR.315
            QC3DTEN(I,J,K)=QC3DTEN(I,J,K)-(PRC(I,K)+PRA(I,K)+PSACW(I,K)+         EXMOISR.316
     +                     NUFCI(I,K))*PSB(I,J)                                  EXMOISR.317
C RAIN TENDENCY                                                                  EXMOISR.318
            QR3DTEN(I,J,K)=QR3DTEN(I,J,K)+(PRC(I,K)+PRA(I,K)+PRE(I,K))*          EXMOISR.319
     +                     PSB(I,J)                                              EXMOISR.320
C CLOUD ICE TENDENCY                                                             EXMOISR.321
            QI3DTEN(I,J,K)=QI3DTEN(I,J,K)-(PRCI(I,K)+PRAI(I,K)-PRI(I,K)-         EXMOISR.322
     +                     PRD(I,K)-NUFCI(I,K))*PSB(I,J)                         EXMOISR.323
C SNOW TENDENCY                                                                  EXMOISR.324
            QNI3DTEN(I,J,K)=QNI3DTEN(I,J,K)+(PREI(I,K)+PRCI(I,K)+                EXMOISR.325
     +                      PRAI(I,K)+PSACW(I,K))*PSB(I,J)                       EXMOISR.326
            XLATF=XLS-DUM31(I,K)                                                 EXMOISR.327
            TEMP=-XLS*(PREI(I,K)+PRD(I,K)+PRI(I,K))*PSB(I,J)-DUM31(I,K)*         EXMOISR.328
     +           PRE(I,K)*PSB(I,J)-XLATF*(PSACW(I,K)+NUFCI(I,K))*                EXMOISR.329
     +           PSB(I,J)                                                        EXMOISR.330
            PRD(I,K)=CP*(1.+0.887*QAOUT(I,K))                                    EXMOISR.331
            IF(IEVAP(IN).EQ.0)TEMP=AMIN1(TEMP,0.)                                EXMOISR.332
            T3DTEN(I,J,K)=T3DTEN(I,J,K)-TEMP/PRD(I,K)*(1-IFDRY)                  EXMOISR.333
          ELSE                                                                   EXMOISR.334
CONSERVATION OF QC                                                               EXMOISR.335
            DUM11(I,K)=(PRC(I,K)+PRA(I,K)+PSACW(I,K))*DT                         EXMOISR.336
            IF(DUM11(I,K).GT.CLOUD(I,K))THEN                                     EXMOISR.337
#ifdef KMA                                                                       07NOV00.1614
              dum11_rcp = 1.0/dum11(i,k)                                         07NOV00.1615
              PRC(I,K)=CLOUD(I,K)*PRC(I,K) * dum11_rcp                           07NOV00.1616
              PRA(I,K)=CLOUD(I,K)*PRA(I,K) * dum11_rcp                           07NOV00.1617
              PSACW(I,K)=CLOUD(I,K)*PSACW(I,K) * dum11_rcp                       07NOV00.1618
#else                                                                            07NOV00.1619
              PRC(I,K)=CLOUD(I,K)*PRC(I,K)/DUM11(I,K)                            EXMOISR.338
              PRA(I,K)=CLOUD(I,K)*PRA(I,K)/DUM11(I,K)                            EXMOISR.339
              PSACW(I,K)=CLOUD(I,K)*PSACW(I,K)/DUM11(I,K)                        EXMOISR.340
#endif                                                                           07NOV00.1620
            ENDIF                                                                EXMOISR.341
CONSERVATION SNOW                                                                EXMOISR.342
            DUM11(I,K)=(-PMLTEV(I,K)-PSMLT(I,K))*DT                              EXMOISR.343
            IF(DUM11(I,K).GT.SNOW(I,K))THEN                                      EXMOISR.344
              PMLTEV(I,K)=SNOW(I,K)*PMLTEV(I,K)/DUM11(I,K)                       EXMOISR.345
              PSMLT(I,K)=SNOW(I,K)*PSMLT(I,K)/DUM11(I,K)                         EXMOISR.346
            ENDIF                                                                EXMOISR.347
            DUM11(I,K)=-(PRE(I,K)+PMLTEV(I,K))*PSB(I,J)                          EXMOISR.348
C WATER VAPOR TENDENCY                                                           EXMOISR.349
            QV3DTEN(I,J,K)=QV3DTEN(I,J,K)+DUM11(I,K)                             EXMOISR.350
C CLOUD WATER TENDENCY                                                           EXMOISR.351
            QC3DTEN(I,J,K)=QC3DTEN(I,J,K)-(PRC(I,K)+PRA(I,K)+PSACW(I,K))         EXMOISR.352
     +                     *PSB(I,J)                                             EXMOISR.353
C RAIN TENDENCY                                                                  EXMOISR.354
            QR3DTEN(I,J,K)=QR3DTEN(I,J,K)+(PRC(I,K)+PRA(I,K)+PRE(I,K)-           EXMOISR.355
     +                     PSMLT(I,K)+PSACW(I,K))*PSB(I,J)                       EXMOISR.356
C SNOW TENDENCY                                                                  EXMOISR.357
            QNI3DTEN(I,J,K)=QNI3DTEN(I,J,K)+(PSMLT(I,K)+PMLTEV(I,K))*            EXMOISR.358
     +                      PSB(I,J)                                             EXMOISR.359
            XLATF=XLS-DUM31(I,K)                                                 EXMOISR.360
            TEMP=-XLATF*(PSMLT(I,K))*PSB(I,J)-DUM31(I,K)*(PRE(I,K)+              EXMOISR.361
     +           PMLTEV(I,K))*PSB(I,J)                                           EXMOISR.362
            PRD(I,K)=CP*(1.+0.887*QAOUT(I,K))                                    EXMOISR.363
            IF(IEVAP(IN).EQ.0)TEMP=AMIN1(TEMP,0.)                                EXMOISR.364
            T3DTEN(I,J,K)=T3DTEN(I,J,K)-TEMP/PRD(I,K)*(1-IFDRY)                  EXMOISR.365
          ENDIF                                                                  EXMOISR.366
   40   CONTINUE                                                                 EXMOISR.367
                                                                                 EXMOISR.368
CLOUD NEXT PART IS TO GET RID OF EXCESS WATER VAPOR IF                           EXMOISR.369
C SS WITH RESPECT TO WATER, AS IN ANTHES AND WARNER                              EXMOISR.370
C FOLLOWS EXMOISS OF DUDHIA WITH NO MODIFICATION                                 EXMOISR.371
C---COMPUTE T, QV, AND QC AT TAU+1 WITHOUT CONDENSATIONAL TERM:                  EXMOISR.372
      DO 50 K=1,KL                                                               EXMOISR.373
        DO 50 I=IST,IEN                                                          EXMOISR.374
          DUM11(I,K)=QV3D(I,J,K)+DT*QV3DTEN(I,J,K)*RPSC(I,J)                     EXMOISR.375
          SCR4(I,K)=AMAX1(1.E-12,DUM11(I,K))                                     EXMOISR.376
          DUM21(I,K)=QC3D(I,J,K)+DT*QC3DTEN(I,J,K)*RPSC(I,J)                     EXMOISR.377
          SCR3(I,K)=AMAX1(0.,DUM21(I,K))                                         EXMOISR.378
          PRE(I,K)=(A(K)*PSB(I,J)+PTOP)*1000.+PP3D(I,J,K)                        EXMOISR.379
          SCR7(I,K)=T3D(I,J,K)+DT*T3DTEN(I,J,K)*RPSC(I,J)                        EXMOISR.380
   50   CONTINUE                                                                 EXMOISR.381
                                                                                 EXMOISR.382
C----COMPUTE THE CONDENSATIONAL TERM:                                            EXMOISR.383
      DO 60 K=1,KL                                                               EXMOISR.384
        DO 60 I=IST,IEN                                                          EXMOISR.385
          DUM11(I,K)=1.E3*SVP1*                                                  EXMOISR.386
     +               EXP(SVP2*(SCR7(I,K)-SVPT0)/(SCR7(I,K)-SVP3))                EXMOISR.387
          DUM21(I,K)=DUM31(I,K)*DUM31(I,K)/(RV*PRD(I,K))                         EXMOISR.388
          PRC(I,K)=.622*DUM11(I,K)/(PRE(I,K)-DUM11(I,K))                         EXMOISR.389
   60   CONTINUE                                                                 EXMOISR.390
                                                                                 EXMOISR.391
      DO 70 K=1,KL                                                               EXMOISR.392
        DO 70 I=IST,IEN                                                          EXMOISR.393
          r2 = r1 * psb(i,j)                                                     23DEC04.3263
          SCR8(I,K)=(SCR4(I,K)-PRC(I,K))/(1.+DUM21(I,K)*PRC(I,K)/(               EXMOISR.394
     +              SCR7(I,K)*SCR7(I,K)))                                        EXMOISR.395
          DUM11(I,K)=SCR3(I,K)+SCR8(I,K)                                         EXMOISR.396
          IF(DUM11(I,K).GE.0)THEN                                                EXMOISR.397
            SCR6(I,K)=SCR8(I,K)/DT                                               EXMOISR.398
          ELSE                                                                   EXMOISR.399
            SCR6(I,K)=-SCR3(I,K)/DT                                              EXMOISR.400
          ENDIF                                                                  EXMOISR.401
          QV3DTEN(I,J,K)=QV3DTEN(I,J,K)-PSB(I,J)*SCR6(I,K)                       EXMOISR.402
C...INITIATION OF CLOUD WATER:                                                   EXMOISR.403
          QC3DTEN(I,J,K)=QC3DTEN(I,J,K)+PSB(I,J)*SCR6(I,K)                       EXMOISR.404
          DUM21(I,K)=DUM31(I,K)/PRD(I,K)                                         EXMOISR.405
          IF(IEVAP(IN).EQ.0)SCR6(I,K)=AMAX1(SCR6(I,K),0.)                        EXMOISR.406
          T3DTEN(I,J,K)=T3DTEN(I,J,K)+PSB(I,J)*SCR6(I,K)*DUM21(I,K)*(1-          EXMOISR.407
     +                  IFDRY)                                                   EXMOISR.408
C--COMPUTE P*T AND P*QR (WITHOUT FALLOUT TERM) AT TAU+1:                         EXMOISR.409
          SCR4R(I,K)=                                                            EXMOISR.410
     +               AMAX1(r2,QR3D(I,J,K)*PSB(I,J)+DT*QR3DTEN(I,J,K)             19DEC02.1402
     +               )                                                           EXMOISR.412
          SCR4S(I,K)=                                                            EXMOISR.413
     +               AMAX1(r2,QNI3D(I,J,K)*PSB(I,J)+DT*                          19DEC02.1403
     +               QNI3DTEN(I,J,K))                                            EXMOISR.415
          SCR4I(I,K)=                                                            EXMOISR.416
     +               AMAX1(r2,QI3D(I,J,K)*PSB(I,J)+DT*QI3DTEN(I,J,K)             19DEC02.1404
     +               )                                                           EXMOISR.418
          SCR7(I,K)=T3D(I,J,K)*PSB(I,J)+DT*T3DTEN(I,J,K)                         EXMOISR.419
          SCR6(I,K)=SCR7(I,K)*RPSC(I,J)                                          EXMOISR.420
   70   CONTINUE                                                                 EXMOISR.421
                                                                                 EXMOISR.422
C--COMPUTE THE FALLOUT TERMS:                                                    EXMOISR.423
C     COMPUTE FALL TERM WITH SHORTER TIME STEPS WHERE VFALL>DZ/DT                EXMOISR.424
#ifdef KMA                                                                       07NOV00.1621
      do k = 1,kl                                                                07NOV00.1622
      dsigma_rcp(k) = 1.0/dsigma(k)                                              07NOV00.1623
      end do                                                                     07NOV00.1624
#endif                                                                           07NOV00.1625
#ifdef VECTOR                                                                    07NOV00.1626
#  include <vec_exmoisr_80.incl>                                                 07NOV00.1627
#else                                                                            07NOV00.1628
      DO 80 I=IST,IEN                                                            EXMOISR.425
        NSTEP=1                                                                  EXMOISR.426
        DO 90 K=1,KL                                                             EXMOISR.427
C IN THE NONHYDROSTATIC MODEL PSC IS THE SAME AS PSB, PSA                        EXMOISR.428
C SIGMA=(PRESSURE-PTOP)/(PSURFACE-PTOP)                                          EXMOISR.429
C NOTE THAT TEMPERATURE=T*PSC, P UNITS OF KILOPASCALS/M2                         EXMOISR.430
          RHO2=(A(K)*PSB(I,J)+PTOP+PP3D(I,J,K)*0.001)/(R*SCR7(I,K))              EXMOISR.431
          SLOR1=(1000.*RHO2*SCR4R(I,K)/TOPR)**(0.25*BR)                          EXMOISR.432
          SLOS1=(1000.*RHO2*SCR4S(I,K)/(TOPS*SONV(I,K)))**(0.25*BS)              EXMOISR.433
          VT2R=FRAIN*SLOR1                                                       EXMOISR.434
          VT2S=FSNOW*SLOS1                                                       EXMOISR.435
          RHO3=((A(K)*PSB(I,J)+PTOP)*1000.+PP3D(I,J,K))/(R*SCR7(I,K)*            EXMOISR.436
     +         RPSC(I,J))                                                        EXMOISR.437
C  SEDIMENTATION FORMULA OF HEYMSFIELD AND DONNER (1990 JAS)                     EXMOISR.438
          IF(ICE(I,K).GT.r1)THEN                                                 19DEC02.1405
            VSED=3.29*(RHO3*ICE(I,K))**0.16                                      EXMOISR.440
          ELSE                                                                   EXMOISR.441
            VSED=0.                                                              EXMOISR.442
          ENDIF                                                                  EXMOISR.443
C UNITS ARE G-M/S2, RHO2-S2/M2,VT2-M/S                                           EXMOISR.444
          RHOFAC(K)=RHO0(I,J,K)/(PSB(I,J)*1000.)/RHO2                            05DEC01.723
          FR(K)=G*RHO2*VT2R                                                      EXMOISR.445
          FS(K)=G*RHO2*VT2S                                                      EXMOISR.446
          FI(K)=G*RHO2*VSED                                                      EXMOISR.447
C      RGVM=AMAX1(FR(K),FS(K))                                                   EXMOISR.448
          RGVM=AMAX1(FR(K),FS(K),FI(K))                                          EXMOISR.449
C 1 IS TO ROUND UP, REPRESENTS NUMBER OF STEPS                                   EXMOISR.450
          NSTEP=MAX0(IFIX(RGVM*DT/DSIGMA(K)+1.),NSTEP)                           EXMOISR.451
   90   CONTINUE                                                                 EXMOISR.452
        DO 100 N=1,NSTEP                                                         EXMOISR.453
          IF(N.GT.1000)THEN                                                      25JAN00.486
            PRINT *, 'STOPPING IN EXMOISR, NEST = ',IN,' I = ',I,                25JAN00.487
     +               ' J = ',J                                                   25JAN00.488
            STOP 'IN SUB. EXMOISR, NSTEP TOO LARGE, PROBABLY NAN'                25JAN00.489
          END IF                                                                 25JAN00.490
          DO 110 K=1,KL                                                          EXMOISR.456
            FALOUTR(K)=FR(K)*SCR4R(I,K)                                          EXMOISR.457
            FALOUTS(K)=FS(K)*SCR4S(I,K)                                          EXMOISR.458
            FALOUTI(K)=FI(K)*SCR4I(I,K)                                          EXMOISR.459
  110     CONTINUE                                                               EXMOISR.460
C FOR TOP OF MODEL                                                               EXMOISR.461
          K=1                                                                    EXMOISR.462
#ifdef KMA                                                                       07NOV00.1629
          rcp_nstep=1.0/NSTEP                                                    05DEC01.724
          FALTNDR=RHOFAC(K)*FALOUTR(K)*DSIGMA_RCP(K)                             05DEC01.725
          FALTNDS=RHOFAC(K)*FALOUTS(K)*DSIGMA_RCP(K)                             05DEC01.726
          FALTNDI=RHOFAC(K)*FALOUTI(K)*DSIGMA_RCP(K)                             05DEC01.727
          QR3DTEN(I,J,K)=QR3DTEN(I,J,K)-FALTNDR*RCP_NSTEP                        07NOV00.1634
          QNI3DTEN(I,J,K)=QNI3DTEN(I,J,K)-FALTNDS*RCP_NSTEP                      07NOV00.1635
          QI3DTEN(I,J,K)=QI3DTEN(I,J,K)-FALTNDI*RCP_NSTEP                        07NOV00.1636
          SCR4R(I,K)=SCR4R(I,K)-FALTNDR*DT*RCP_NSTEP                             07NOV00.1637
          SCR4S(I,K)=SCR4S(I,K)-FALTNDS*DT*RCP_NSTEP                             07NOV00.1638
          SCR4I(I,K)=SCR4I(I,K)-FALTNDI*DT*RCP_NSTEP                             07NOV00.1639
          DO 120 K=2,KL                                                          07NOV00.1640
            FALTNDR=RHOFAC(K)*(FALOUTR(K)-FALOUTR(K-1))*DSIGMA_RCP(K)            05DEC01.728
            FALTNDS=RHOFAC(K)*(FALOUTS(K)-FALOUTS(K-1))*DSIGMA_RCP(K)            05DEC01.729
            FALTNDI=RHOFAC(K)*(FALOUTI(K)-FALOUTI(K-1))*DSIGMA_RCP(K)            05DEC01.730
            QR3DTEN(I,J,K)=QR3DTEN(I,J,K)-FALTNDR*RCP_NSTEP                      07NOV00.1644
            QNI3DTEN(I,J,K)=QNI3DTEN(I,J,K)-FALTNDS*RCP_NSTEP                    07NOV00.1645
            QI3DTEN(I,J,K)=QI3DTEN(I,J,K)-FALTNDI*RCP_NSTEP                      07NOV00.1646
            SCR4R(I,K)=SCR4R(I,K)-FALTNDR*DT*RCP_NSTEP                           07NOV00.1647
            SCR4S(I,K)=SCR4S(I,K)-FALTNDS*DT*RCP_NSTEP                           07NOV00.1648
            SCR4I(I,K)=SCR4I(I,K)-FALTNDI*DT*RCP_NSTEP                           07NOV00.1649
            FR(K)=AMAX1(FR(K)/DSIGMA(K),FR(K-1)*                                 07NOV00.1650
     +                                DSIGMA_RCP(K-1))*DSIGMA(K)                 07NOV00.1651
            FS(K)=AMAX1(FS(K)/DSIGMA(K),FS(K-1)*                                 07NOV00.1652
     +                                DSIGMA_RCP(K-1))*DSIGMA(K)                 07NOV00.1653
            FI(K)=AMAX1(FI(K)/DSIGMA(K),FI(K-1)*                                 07NOV00.1654
     +                                DSIGMA_RCP(K-1))*DSIGMA(K)                 07NOV00.1655
#else                                                                            07NOV00.1656
          FALTNDR=RHOFAC(K)*FALOUTR(K)/DSIGMA(K)                                 05DEC01.731
          FALTNDS=RHOFAC(K)*FALOUTS(K)/DSIGMA(K)                                 05DEC01.732
          FALTNDI=RHOFAC(K)*FALOUTI(K)/DSIGMA(K)                                 05DEC01.733
          QR3DTEN(I,J,K)=QR3DTEN(I,J,K)-FALTNDR/NSTEP                            EXMOISR.466
          QNI3DTEN(I,J,K)=QNI3DTEN(I,J,K)-FALTNDS/NSTEP                          EXMOISR.467
          QI3DTEN(I,J,K)=QI3DTEN(I,J,K)-FALTNDI/NSTEP                            EXMOISR.468
          SCR4R(I,K)=SCR4R(I,K)-FALTNDR*DT/NSTEP                                 EXMOISR.469
          SCR4S(I,K)=SCR4S(I,K)-FALTNDS*DT/NSTEP                                 EXMOISR.470
          SCR4I(I,K)=SCR4I(I,K)-FALTNDI*DT/NSTEP                                 EXMOISR.471
          DO 120 K=2,KL                                                          EXMOISR.472
            FALTNDR=RHOFAC(K)*(FALOUTR(K)-FALOUTR(K-1))/DSIGMA(K)                05DEC01.734
            FALTNDS=RHOFAC(K)*(FALOUTS(K)-FALOUTS(K-1))/DSIGMA(K)                05DEC01.735
            FALTNDI=RHOFAC(K)*(FALOUTI(K)-FALOUTI(K-1))/DSIGMA(K)                05DEC01.736
            QR3DTEN(I,J,K)=QR3DTEN(I,J,K)-FALTNDR/NSTEP                          EXMOISR.476
            QNI3DTEN(I,J,K)=QNI3DTEN(I,J,K)-FALTNDS/NSTEP                        EXMOISR.477
            QI3DTEN(I,J,K)=QI3DTEN(I,J,K)-FALTNDI/NSTEP                          EXMOISR.478
            SCR4R(I,K)=SCR4R(I,K)-FALTNDR*DT/NSTEP                               EXMOISR.479
            SCR4S(I,K)=SCR4S(I,K)-FALTNDS*DT/NSTEP                               EXMOISR.480
            SCR4I(I,K)=SCR4I(I,K)-FALTNDI*DT/NSTEP                               EXMOISR.481
            FR(K)=AMAX1(FR(K)/DSIGMA(K),FR(K-1)/DSIGMA(K-1))*DSIGMA(K)           EXMOISR.482
            FS(K)=AMAX1(FS(K)/DSIGMA(K),FS(K-1)/DSIGMA(K-1))*DSIGMA(K)           EXMOISR.483
            FI(K)=AMAX1(FI(K)/DSIGMA(K),FI(K-1)/DSIGMA(K-1))*DSIGMA(K)           EXMOISR.484
#endif                                                                           07NOV00.1657
  120     CONTINUE                                                               EXMOISR.485
C     ACCUMULATED RAIN                                                           EXMOISR.486
#ifdef KMA                                                                       07NOV00.1658
      RAINNC(I,J)=RAINNC(I,J)+(FALOUTR(KL)+FALOUTS(KL)+FALOUTI(KL))*             05DEC01.737
     +                DTMIN*6000.*G_RCP*RCP_NSTEP                                07NOV00.1661
c      snownct stores current timestep snowfall for ifsnow=2 option              05DEC01.738
      SNOWNCT(I,J)=(FALOUTS(KL)+FALOUTI(KL))*DTMIN*6000.*G_RCP*RCP_NSTEP         19DEC02.1406
#else                                                                            07NOV00.1662
      RAINNC(I,J)=RAINNC(I,J)+(FALOUTR(KL)+FALOUTS(KL)+FALOUTI(KL))*             05DEC01.740
     +                DTMIN*6000./G/NSTEP                                        EXMOISR.491
c      snownct stores current timestep snowfall for ifsnow=2 option              05DEC01.741
      SNOWNCT(I,J)=(FALOUTS(KL)+FALOUTI(KL))*DTMIN*6000./G/NSTEP                 05DEC01.742
#endif                                                                           07NOV00.1664
  100   CONTINUE                                                                 EXMOISR.492
   80 CONTINUE                                                                   EXMOISR.493
#endif                                                                           07NOV00.1665
      DO 95 I=IST,IEN                                                            EXMOISR.494
        DO 92 K=2,KL                                                             EXMOISR.495
C MELTING OF CLOUD ICE                                                           EXMOISR.496
          IF(SCR6(I,K).GT.TO.AND.(QI3DTEN(I,J,K).GT.0.OR.QI3D(I,J,K).GT.         EXMOISR.497
     +      0.))THEN                                                             EXMOISR.498
            XLATF=XLS-DUM31(I,K)                                                 EXMOISR.499
            DUM11(I,K)=QI3D(I,J,K)*PSB(I,J)+DT*QI3DTEN(I,J,K)                    EXMOISR.500
            QI3DTEN(I,J,K)=-QI3D(I,J,K)*PSB(I,J)/DT                              EXMOISR.501
            QC3DTEN(I,J,K)=QC3DTEN(I,J,K)+DUM11(I,K)/DT                          EXMOISR.502
            T3DTEN(I,J,K)=T3DTEN(I,J,K)-XLATF/PRD(I,K)*DUM11(I,K)/DT*(1-         EXMOISR.503
     +                    IFDRY)                                                 EXMOISR.504
          ENDIF                                                                  EXMOISR.505
C HOMOGENOUS FREEZING OF CLOUD ICE                                               EXMOISR.506
          IF(SCR6(I,K).LT.HGFR.AND.(QC3DTEN(I,J,K).GT.0.OR.QC3D(I,J,K)           EXMOISR.507
     +      .GT.0.))THEN                                                         EXMOISR.508
            XLATF=XLS-DUM31(I,K)                                                 EXMOISR.509
            DUM11(I,K)=QC3D(I,J,K)*PSB(I,J)+DT*QC3DTEN(I,J,K)                    EXMOISR.510
            QC3DTEN(I,J,K)=-QC3D(I,J,K)*PSB(I,J)/DT                              EXMOISR.511
            QI3DTEN(I,J,K)=QI3DTEN(I,J,K)+DUM11(I,K)/DT                          EXMOISR.512
            T3DTEN(I,J,K)=T3DTEN(I,J,K)+XLATF/PRD(I,K)*DUM11(I,K)/DT*(1-         EXMOISR.513
     +                    IFDRY)                                                 EXMOISR.514
          ENDIF                                                                  EXMOISR.515
   92   CONTINUE                                                                 EXMOISR.516
   95 CONTINUE                                                                   EXMOISR.517
      RETURN                                                                     EXMOISR.518
      END                                                                        EXMOISR.519
                                                                                 EXMOISR.520
