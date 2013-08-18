      SUBROUTINE LEXMOISR(QC3DTEN,QR3DTEN,QI3DTEN,QNI3DTEN,QV3DTEN,              LEXMOISR.1
     &                   T3DTEN,T3D,QV3D,QC3D,QR3D,QI3D,QNI3D,PP3D,              LEXMOISR.2
     &                   PSB,RAINNC,SNOWNCT,IN,J,IST,IEN)                        05DEC01.743
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          LEXMOISR.4
C                                                                     C          LEXMOISR.5
C                                                                     C          LEXMOISR.6
C     THIS SUBROUTINE COMPUTES THE MOISTURE TENDENCIES                C          LEXMOISR.7
C     THE PROGNOSTIC EQUATIONS INCLUDE                                C          LEXMOISR.8
C     WATER VAPOR (QV3DTEN), CLOUD WATER (QC3DTEN),                              LEXMOISR.9
C     RAIN WATER (QR3DTEN), CLOUD ICE (QI3DTEN), AND SNOW (QNI3DTEN)  C          LEXMOISR.10
C                                                                     C          LEXMOISR.11
C     IN = 1 : FOR LARGE DOMAIN                                       C          LEXMOISR.12
C        = 2 : FOR NESTED DOMAIN                                      C          LEXMOISR.13
C                                                                     C          LEXMOISR.14
C     ALL THE OTHER ARGUMENTS ARE PASSED FROM SUBROUTINE "SOLVE" AND  C          LEXMOISR.15
C     EXPLAINED IN "SOLVE".                                           C          LEXMOISR.16
C                                                                     C          19DEC02.1407
C     ADD FLAG (MEYERS) FOR USE OF MEYERS ICE NUCLEI CONC. EQ         C          19DEC02.1408
C     IN PLACE OF FLETCHER EQ                                         C          19DEC02.1409
C                                                                     C          19DEC02.1410
C                                                                     C          LEXMOISR.17
C                                                                     C          LEXMOISR.18
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          LEXMOISR.19
                                                                                 LEXMOISR.20
#     include <parame.incl>                                                      LEXMOISR.21
#     include <varia.incl>                                                       LEXMOISR.22
#     include <param3.incl>                                                      LEXMOISR.23
#     include <param2.incl>                                                      LEXMOISR.24
#     include <pmoist.incl>                                                      LEXMOISR.25
#     include <various.incl>                                                     LEXMOISR.26
#     include <nonhyd.incl>                                                      LEXMOISR.27
#     include <nhcnst.incl>                                                      LEXMOISR.28
#     include <rpstar.incl>                                                      LEXMOISR.29
#     include <jrg.incl>                                                         LEXMOISR.30
                                                                                 LEXMOISR.31
C INPUT ARRAYS                                                                   LEXMOISR.32
                                                                                 LEXMOISR.33
      REAL  T3D(MIX,MJX,MKX),        QV3D(MIX,MJX,MKX),                          LEXMOISR.34
     &      U3D(MIX,MJX,MKX),         V3D(MIX,MJX,MKX),                          LEXMOISR.35
     &     QC3D(MIXM,MJXM,MKXM),     QR3D(MIXM,MJXM,MKXM),                       LEXMOISR.36
     &     QI3D(MIXIC,MJXIC,MKXIC), QNI3D(MIXIC,MJXIC,MKXIC),                    LEXMOISR.37
     &     PP3D(MIX,MJX,MKX),         PSB(MIX,MJX)                               LEXMOISR.38
      REAL RAINNC(MIX,MJX), SNOWNCT(MIX,MJX)                                     05DEC01.744
                                                                                 LEXMOISR.40
C ARRAYS FOR BASE VARIABLES                                                      LEXMOISR.41
                                                                                 LEXMOISR.42
      REAL TAOUT(MIX,MKX),PRES(MIX,MKX),RHO(MIX,MKX),                            LEXMOISR.43
     1     T3DTEN(MIX,MJX,MKX),SCR7(MIX,MKX),SCR6(MIX,MKX)                       LEXMOISR.44
                                                                                 LEXMOISR.45
C ARRAYS FOR QV                                                                  LEXMOISR.46
                                                                                 LEXMOISR.47
      REAL QAOUT(MIX,MKX),QVQVS(MIX,MKX),QVQVSI(MIX,MKX),                        LEXMOISR.48
     1     QVS(MIX,MKX),QVSI(MIX,MKX),AB(MIX,MKX),                               LEXMOISR.49
     1     ABI(MIX,MKX),ABW(MIX,MKX),QV3DTEN(MIX,MJX,MKX)                        LEXMOISR.50
                                                                                 LEXMOISR.51
C ARRAYS FOR QC                                                                  LEXMOISR.52
                                                                                 LEXMOISR.53
      REAL CLOUD(MIX,MKX),PRC(MIX,MKX),                                          LEXMOISR.54
     1     NUFCI(MIX,MKX),PSACW(MIX,MKX),                                        LEXMOISR.55
     1     PRA(MIX,MKX),QC3DTEN(MIX,MJX,MKX),SCR3(MIX,MKX)                       LEXMOISR.56
                                                                                 LEXMOISR.57
C ARRAYS FOR QR                                                                  LEXMOISR.58
                                                                                 LEXMOISR.59
      REAL RAIN(MIX,MKX),SLOR(MIX,MKX),                                          LEXMOISR.60
     1     PRE(MIX,MKX),                                                         LEXMOISR.61
     1     QR3DTEN(MIX,MJX,MKX),SCR4R(MIX,MKX),FR(MKX),                          LEXMOISR.62
     1     FALOUTR(MKX),RHOFAC(MKX)                                              05DEC01.745
                                                                                 LEXMOISR.64
C ARRAYS FOR QI                                                                  LEXMOISR.65
                                                                                 LEXMOISR.66
      REAL ICE(MIX,MKX),PRI(MIX,MKX),PRCI(MIX,MKX),                              LEXMOISR.67
     1     PRAI(MIX,MKX),                                                        LEXMOISR.68
     1     PRD(MIX,MKX),QI3DTEN(MIX,MJX,MKX)                                     LEXMOISR.69
      REAL SCR4I(MIX,MKX),FI(MKX),FALOUTI(MKX)                                   LEXMOISR.70
                                                                                 LEXMOISR.71
C ARRAYS FOR QS                                                                  LEXMOISR.72
                                                                                 LEXMOISR.73
      REAL SNOW(MIX,MKX),SLOS(MIX,MKX),                                          LEXMOISR.74
     1     PREI(MIX,MKX),                                                        LEXMOISR.75
     1     PSMLT(MIX,MKX),PMLTEV(MIX,MKX),QNI3DTEN(MIX,MJX,MKX),                 LEXMOISR.76
     1     SCR4S(MIX,MKX),FS(MKX),FALOUTS(MKX)                                   LEXMOISR.77
      REAL SONV(MIX,MKX)                                                         LEXMOISR.78
                                                                                 LEXMOISR.79
C EXTRA ARRAYS                                                                   LEXMOISR.80
                                                                                 LEXMOISR.81
      REAL DUM11(MIX,MKX),DUM21(MIX,MKX),DUM31(MIX,MKX),                         LEXMOISR.82
     1     SCR4(MIX,MKX),SCR8(MIX,MKX)                                           LEXMOISR.83
                                                                                 LEXMOISR.84
C ADD FLAG FOR MEYERS ICE NUCLEI CONC. EQ                                        19DEC02.1411
      INTEGER MEYERS                                                             19DEC02.1412
                                                                                 23DEC04.3264
      real r1, r2                                                                23DEC04.3265
#     include <hdtabl.incl>                                                      23DEC04.3266
#     include <functb.incl>                                                      23DEC04.3267
C MEYERS=1 : USE MEYERS ICE NUCLEI CONC. EQ.                                     19DEC02.1413
C MEYERS=0 : USE FLETCHER ICE NUCLEI CONC. EQ.                                   19DEC02.1414
      MEYERS=0                                                                   19DEC02.1415
      IF(IPOLAR.EQ.1)MEYERS=1                                                    19DEC02.1416
      r1 = 1.e-25                                                                23DEC04.3268
                                                                                 LEXMOISR.87
      DO 15 K=1,KX                                                               LEXMOISR.88
        DO 15 I=2,ILXM                                                           LEXMOISR.89
          TAOUT(I,K)=T3D(I,J,K)                                                  LEXMOISR.90
          QAOUT(I,K)=QV3D(I,J,K)                                                 LEXMOISR.91
C     V8 SVP FORMULA                                                             LEXMOISR.92
C SATURATION VAPOR PRESSURE FOR T > TO (273 K)                                   LEXMOISR.93
C                                                                                LEXMOISR.94
C-- SUE MODS                                                                     LEXMOISR.95
          EVS=SATVPW(TAOUT(I,K))                                                 LEXMOISR.96
          EIS=SATVPI(TAOUT(I,K))                                                 LEXMOISR.97
C QVS FOR T > TO                                                                 LEXMOISR.98
          QVS1=.622*EVS/(PSB(I,J)*A(K)+PTOP+PP3D(I,J,K)*0.001-EVS)               LEXMOISR.99
C QVS FOR T < TO                                                                 LEXMOISR.100
          QVI1=.622*EIS/(PSB(I,J)*A(K)+PTOP+PP3D(I,J,K)*0.001-EIS)               LEXMOISR.101
C QV/QVS FOR T > TO                                                              LEXMOISR.102
          QVQVS(I,K)=QV3D(I,J,K)/QVS1                                            LEXMOISR.103
C QV/QVS FOR T < TO                                                              LEXMOISR.104
          QVQVSI(I,K)=QV3D(I,J,K)/QVI1                                           LEXMOISR.105
   15   CONTINUE                                                                 LEXMOISR.106
                                                                                 LEXMOISR.107
C---BEGIN PRODUCTION TERMS CALCULATION:                                          LEXMOISR.108
C    ALL THE PRODUCTION TERMS ARE BASED ON T-1 (I.E. XXB) VARIABLES              LEXMOISR.109
      DO 20 K=1,KL                                                               LEXMOISR.110
        DO 20 I=IST,IEN                                                          LEXMOISR.111
          PRES(I,K)=(A(K)*PSB(I,J)+PTOP)*1000.+PP3D(I,J,K)                       LEXMOISR.112
          RHO(I,K)=PRES(I,K)/(R*TAOUT(I,K))                                      LEXMOISR.113
          CLOUD(I,K)=AMAX1(r1,QC3D(I,J,K))                                       23DEC04.3269
          ICE(I,K)=AMAX1(r1,QI3D(I,J,K))                                         23DEC04.3270
          RAIN(I,K)=AMAX1(r1,QR3D(I,J,K))                                        23DEC04.3271
          SNOW(I,K)=AMAX1(r1,QNI3D(I,J,K))                                       23DEC04.3272
C                                                                                LEXMOISR.118
C         SCRSNOW=1.0E7*RHO(I,K)*SNOW(I,K)                                       LEXMOISR.119
C         SCRRAIN=1.0E7*RHO(I,K)*RAIN(I,K)                                       LEXMOISR.120
          SCRSNOW=AMIN1(SCALE*RHO(I,K)*SNOW(I,K),2.E-2*SCALE-1.)                 LEXMOISR.121
          SCRRAIN=AMIN1(SCALE*RHO(I,K)*RAIN(I,K),2.E-2*SCALE-1.)                 LEXMOISR.122
          IF(SNOW(I,K).LE.1.E-8)THEN                                             LEXMOISR.123
            SONV(I,K)=1.                                                         LEXMOISR.124
          ELSE                                                                   LEXMOISR.125
            SONV(I,K)=NOSFAC(SCRSNOW)                                            LEXMOISR.126
          ENDIF                                                                  LEXMOISR.127
C-------------------------------------------------                               LEXMOISR.128
          QVS(I,K)=QAOUT(I,K)/QVQVS(I,K)                                         LEXMOISR.129
          QVSI(I,K)=QAOUT(I,K)/QVQVSI(I,K)                                       LEXMOISR.130
C                                                                                LEXMOISR.131
C--- SUE MODS                                                                    LEXMOISR.132
          SLOR(I,K)=LAMDAW(SCRRAIN)                                              LEXMOISR.133
          SLOS(I,K)=LAMDAI(SCRSNOW)*NOSF0P25(SONV(I,K))                          LEXMOISR.134
          DUM11(I,K)=T21P81(TAOUT(I,K))/PRES(I,K)                                LEXMOISR.135
          DUM21(I,K)=T21P50(TAOUT(I,K))/(TAOUT(I,K)+120.)                        LEXMOISR.136
C-------------------------------------------------                               LEXMOISR.137
C     V8 LV FORMULA                                                              LEXMOISR.138
          DUM31(I,K)=3.1484E6-XLV1*TAOUT(I,K)                                    LEXMOISR.139
C THERMAL CONDUCTIVITY OF AIR                                                    LEXMOISR.140
          SCR4(I,K)=1.414E3*DUM21(I,K)                                           LEXMOISR.141
C A + B IN FORMULA (B7) OF DUDHIA (1992) FOR T > TO                              LEXMOISR.142
          AB(I,K)=RHO(I,K)*XLS*XLS/(SCR4(I,K)*RV*TAOUT(I,K)*TAOUT(I,K))+         LEXMOISR.143
     +            1./(QVS(I,K)*DUM11(I,K))                                       LEXMOISR.144
C A + B FOR RAIN EVAP AND T < TO                                                 LEXMOISR.145
          ABW(I,K)=RHO(I,K)*DUM31(I,K)*DUM31(I,K)/(SCR4(I,K)*RV*                 LEXMOISR.146
     +             TAOUT(I,K)*TAOUT(I,K))+1./(QVS(I,K)*DUM11(I,K))               LEXMOISR.147
C A + B FOR T < TO                                                               LEXMOISR.148
          ABI(I,K)=RHO(I,K)*XLS*XLS/(SCR4(I,K)*RV*TAOUT(I,K)*TAOUT(I,K))         LEXMOISR.149
     +             +1./(QVSI(I,K)*DUM11(I,K))                                    LEXMOISR.150
C DYNAMIC VISCOSITY OF AIR                                                       LEXMOISR.151
          PRE(I,K)=DUM21(I,K)/RHO(I,K)                                           LEXMOISR.152
C THE SCHMIDT NUMBER                                                             LEXMOISR.153
          PRC(I,K)=PRE(I,K)/DUM11(I,K)                                           LEXMOISR.154
   20   CONTINUE                                                                 LEXMOISR.155
                                                                                 LEXMOISR.156
      DO 22 K=1,KL                                                               LEXMOISR.157
        DO 22 I=IST,IEN                                                          LEXMOISR.158
C                                                                                LEXMOISR.159
C-- SUE MODS                                                                     LEXMOISR.160
          DUM11(I,K)=PRC20P33(PRC(I,K))/PRE20P5(PRE(I,K))                        LEXMOISR.161
          PRI(I,K)=0.                                                            LEXMOISR.162
          NUFCI(I,K)=0.                                                          LEXMOISR.163
          PRCI(I,K)=0.                                                           LEXMOISR.164
          PRAI(I,K)=0.                                                           LEXMOISR.165
          PSACW(I,K)=0.                                                          LEXMOISR.166
          PRD(I,K)=0.                                                            LEXMOISR.167
          PREI(I,K)=0.                                                           LEXMOISR.168
          PRC(I,K)=0.                                                            LEXMOISR.169
          PRA(I,K)=0.                                                            LEXMOISR.170
          PRE(I,K)=0.                                                            LEXMOISR.171
          PSMLT(I,K)=0.                                                          LEXMOISR.172
          PMLTEV(I,K)=0.                                                         LEXMOISR.173
   22   CONTINUE                                                                 LEXMOISR.174
                                                                                 LEXMOISR.175
C FORMULAS COME FROM DUDHIA (1989, JAS, 46),                                     LEXMOISR.176
C RUTLEDGE AND HOBBS (1983, JAS, 40), {RH},                                      LEXMOISR.177
C AND METEOROLOGICAL RESEARCH INSTITUTE {MRI} OF JAPAN (#28, 1991)               LEXMOISR.178
      DO 30 K=1,KL                                                               LEXMOISR.179
        DO 30 I=IST,IEN                                                          LEXMOISR.180
          ISAT=0                                                                 LEXMOISR.181
          IF(MEYERS.EQ.1)THEN                                                    19DEC02.1417
C MEYERS ICE CONC. EQ.                                                           19DEC02.1418
            XNC=1000.*EXP(-0.639+0.1296*(100.*                                   19DEC02.1419
     +        (QVQVSI(I,K)-1.)))/RHO(I,K)                                        19DEC02.1420
          ELSE                                                                   19DEC02.1421
C FLETCHER CURVE                                                                 LEXMOISR.182
C                                                                                LEXMOISR.183
C-- SUE MODS                                                                     LEXMOISR.184
            XNC=NCICE(TAOUT(I,K))/RHO(I,K)                                       19DEC02.1422
C----------------------------------------------------                            LEXMOISR.186
          ENDIF                                                                  19DEC02.1423
          XMI=ICE(I,K)/XNC                                                       LEXMOISR.187
          IF(XNC.LT.TNO)XNC=0.                                                   LEXMOISR.188
C INITIATION OF CLOUD ICE, DUDHIA (B4)                                           LEXMOISR.189
          SUPICE=(QAOUT(I,K)-QVSI(I,K))/DT                                       LEXMOISR.190
          IF(SUPICE.GT.0.)THEN                                                   LEXMOISR.191
            PRI(I,K)=AMAX1(0.,(XM01*XNC-ICE(I,K))/DT)                            LEXMOISR.192
            PRI(I,K)=AMIN1(PRI(I,K),SUPICE)                                      LEXMOISR.193
            IF(PRI(I,K).GT.SUPICE*.99)ISAT=1                                     LEXMOISR.194
          ENDIF                                                                  LEXMOISR.195
C FREEZING OF CLOUD DROPLETS, MRI (11-19)                                        LEXMOISR.196
          IF(CLOUD(I,K).GT.r1)THEN                                               23DEC04.3273
C                                                                                LEXMOISR.198
C-- SUE MODS                                                                     LEXMOISR.199
            NUFCI(I,K)=HETFRZ(TAOUT(I,K))*RHO(I,K)*CLOUD(I,K)*CLOUD(I,K)         LEXMOISR.200
     +                 /(CNP*DRAIN)                                              LEXMOISR.201
          ELSE                                                                   LEXMOISR.202
C---------------------------------------------------------------------           LEXMOISR.203
            NUFCI(I,K)=0.                                                        LEXMOISR.204
          ENDIF                                                                  LEXMOISR.205
          NUFCI(I,K)=AMAX1(NUFCI(I,K),0.)                                        LEXMOISR.206
C...ACCRETION OF CLOUD ICE BY SNOW, DUDHIA (B15):                                LEXMOISR.207
          IF(SNOW(I,K).LT.1.E-15)THEN                                            LEXMOISR.208
            PRAI(I,K)=0.                                                         LEXMOISR.209
          ELSE                                                                   LEXMOISR.210
C                                                                                LEXMOISR.211
C-- SUE MODS                                                                     LEXMOISR.212
            PRAI(I,K)=ACRIS*L2BVTS3(SLOS(I,K))*CLOUD(I,K)*SONV(I,K)              LEXMOISR.213
          ENDIF                                                                  LEXMOISR.214
C---------------------------------------------------------------                 LEXMOISR.215
COLLECTION OF CLOUD WATER BY SNOW, RH (A22)                                      LEXMOISR.216
          IF(SNOW(I,K).LT.1.E-15)THEN                                            LEXMOISR.217
            PSACW(I,K)=0.                                                        LEXMOISR.218
          ELSE                                                                   LEXMOISR.219
C                                                                                LEXMOISR.220
C-- SUE MODS                                                                     LEXMOISR.221
            PSACW(I,K)=ACRCS*L2BVT3(SLOS(I,K))*CLOUD(I,K)*SONV(I,K)              LEXMOISR.222
          ENDIF                                                                  LEXMOISR.223
C----------------------------------------------------------------                LEXMOISR.224
C...DEPOSITION/SUBLIMATION OF CLOUD ICE: DUDHIA (B7)                             LEXMOISR.225
C                                                                                LEXMOISR.226
C-- SUE MODS                                                                     LEXMOISR.227
          XMI=AMAX1(XMI,1.E-25)                                                  LEXMOISR.228
          XMI=AMIN1(XMI,99999*1.E-12)                                            LEXMOISR.229
          DI=16.3*XMI20P5(XMI)                                                   LEXMOISR.230
C--------------------------------                                                LEXMOISR.231
          IF(ISAT.EQ.1)THEN                                                      LEXMOISR.232
            PRD(I,K)=0.                                                          LEXMOISR.233
          ELSE                                                                   LEXMOISR.234
            PRD(I,K)=4.*DI*RHO(I,K)*SUPICE*DT*XNC/(QVSI(I,K)*ABI(I,K))           LEXMOISR.235
          ENDIF                                                                  LEXMOISR.236
          IF(PRD(I,K).LT.0.)THEN                                                 LEXMOISR.237
            PRD(I,K)=AMAX1(PRD(I,K),SUPICE/2.)                                   LEXMOISR.238
          ELSE                                                                   LEXMOISR.239
            PRD(I,K)=AMIN1(PRD(I,K),SUPICE/2.)                                   LEXMOISR.240
          ENDIF                                                                  LEXMOISR.241
          IF(ABS(PRD(I,K)+PRI(I,K)).GT.ABS(.99*SUPICE))ISAT=1                    LEXMOISR.242
C...AUTOCONVERSION OF CLOUD ICE TO SNOW, DUDHIA  (B4):                           LEXMOISR.243
          PRCI(I,K)=AMAX1(0.,(ICE(I,K)-XSMAX*XNC)/DT)                            LEXMOISR.244
C...DEPOSITION/SUBLIMATION OF SNOW: DUDHIA (B14)                                 LEXMOISR.245
C DAVE                                                                           LEXMOISR.246
C     IF(SNOW(I,K).GT.0.)                                                        LEXMOISR.247
C    - PREI(I,K)=DAVET(0.,DEPS1*(QAOUT(I,K)/QVSI(I,K)-1.)                        LEXMOISR.248
C    1 *(0.65*SLOS(I,K)*SLOS(I,K)                                                LEXMOISR.249
C    1 +DEPS2**0.5*DEPS3*DUM11(I,K)                                              LEXMOISR.250
C    1 *SLOS(I,K)**(DEPS4))/ABI(I,K),ISAT.EQ.1)                                  LEXMOISR.251
          IF((SNOW(I,K).GT.r1).AND.(ISAT.EQ.1))THEN                              23DEC04.3274
            PREI(I,K)=0.                                                         LEXMOISR.253
          ELSEIF(SNOW(I,K).GT.1.E-15)THEN                                        LEXMOISR.254
C                                                                                LEXMOISR.255
C-- SUE MODS                                                                     LEXMOISR.256
            PREI(I,K)=SONV(I,K)*DEPS1*(QAOUT(I,K)/QVSI(I,K)-1.)*(0.65*           LEXMOISR.257
     +                SLOS(I,K)*SLOS(I,K)+3.4234485*DEPS3*DUM11(I,K)*            LEXMOISR.258
     +                L2BVTS2(SLOS(I,K)))/ABI(I,K)                               LEXMOISR.259
          ENDIF                                                                  LEXMOISR.260
C---------------------------------------------------------------------           LEXMOISR.261
          IF(PREI(I,K).LT.0.)THEN                                                LEXMOISR.262
            PREI(I,K)=AMAX1(PREI(I,K),SUPICE/2.)                                 LEXMOISR.263
          ELSE                                                                   LEXMOISR.264
            PREI(I,K)=AMIN1(PREI(I,K),SUPICE/2.)                                 LEXMOISR.265
          ENDIF                                                                  LEXMOISR.266
C---AUTOCONVERSION OF CLOUD WATER TO RAINWATER: DUDHIA (B10)                     LEXMOISR.267
          PRC(I,K)=AMAX1(0.,QCK1*(CLOUD(I,K)-QCTH))                              LEXMOISR.268
C---ACCRETION OF CLOUD WATER BY RAINWATER: DUDHIA (B15)                          LEXMOISR.269
          IF(RAIN(I,K).LT.1.E-15)THEN                                            LEXMOISR.270
            PRA(I,K)=0.                                                          LEXMOISR.271
          ELSE                                                                   LEXMOISR.272
C                                                                                LEXMOISR.273
C-- SUE MODS                                                                     LEXMOISR.274
            PRA(I,K)=ACRCR*L2BVT3(SLOR(I,K))*CLOUD(I,K)                          LEXMOISR.275
          ENDIF                                                                  LEXMOISR.276
C----------------------------------------------------------                      LEXMOISR.277
C...EVAPORATION OF RAINWATER: DUDHIA (B14)                                       LEXMOISR.278
          IF(RAIN(I,K).GT.1.E-15)THEN                                            LEXMOISR.279
C                                                                                LEXMOISR.280
C-- SUE MODS                                                                     LEXMOISR.281
            PRE(I,K)=DEPR1*(QAOUT(I,K)/QVS(I,K)-1.)*(0.78*SLOR(I,K)*             LEXMOISR.282
     +               SLOR(I,K)+29.017236*DEPR3*DUM11(I,K)*                       LEXMOISR.283
     +               L2BVT2(SLOR(I,K)))/ABW(I,K)                                 LEXMOISR.284
          ENDIF                                                                  LEXMOISR.285
C--------------------------------------------------------                        LEXMOISR.286
C MELTING OF SNOW: RH (A25)                                                      LEXMOISR.287
          XLATF=XLS-DUM31(I,K)                                                   LEXMOISR.288
C DAVE                                                                           LEXMOISR.289
C     PSMLT(I,K)=DAVEZ(0.,PSM1*SCR4(I,K)/(RHO(I,K)*XLATF)                        LEXMOISR.290
C    1     *(TO-TAOUT(I,K))*(0.65*SLOS(I,K)*SLOS(I,K)                            LEXMOISR.291
C    2     +DUM11(I,K)*PSM3*PSM2**0.5*SLOS(I,K)**PSM4)                           LEXMOISR.292
C    3     ,SNOW(I,K))                                                           LEXMOISR.293
          IF(SNOW(I,K).LT.1.E-15)THEN                                            LEXMOISR.294
            PSMLT(I,K)=0.                                                        LEXMOISR.295
          ELSE                                                                   LEXMOISR.296
C                                                                                LEXMOISR.297
C-- SUE MODS                                                                     LEXMOISR.298
            PSMLT(I,K)=SONV(I,K)*PSM1*SCR4(I,K)/(RHO(I,K)*XLATF)*(TO-            LEXMOISR.299
     +                 TAOUT(I,K))*(0.65*SLOS(I,K)*SLOS(I,K)+DUM11(I,K)*         LEXMOISR.300
     +                 PSM3*3.4234485*L2BVTS2(SLOS(I,K)))                        LEXMOISR.301
          ENDIF                                                                  LEXMOISR.302
C--------------------------------------------------------------                  LEXMOISR.303
C EVAPORATION OF MELTING SNOW FOR T > TO, RH (A27)                               LEXMOISR.304
C DAVE                                                                           LEXMOISR.305
C     IF(SNOW(I,K).GT.0.)                                                        LEXMOISR.306
C    - PMLTEV(I,K)=DAVET(0.,DEPS1*(QAOUT(I,K)/QVS(I,K)-1.)                       LEXMOISR.307
C    1 *(0.65*SLOS(I,K)*SLOS(I,K)                                                LEXMOISR.308
C    1 +DEPS2**0.5*DEPS3*DUM11(I,K)                                              LEXMOISR.309
C    1 *SLOS(I,K)**(DEPS4))/AB(I,K),QAOUT(I,K)-QVS(I,K).GE.0.)                   LEXMOISR.310
          IF((SNOW(I,K).GT.r1).AND.(QAOUT(I,K)-QVS(I,K).GE.0.))THEN              23DEC04.3275
            PMLTEV(I,K)=0.                                                       LEXMOISR.312
          ELSEIF(SNOW(I,K).GT.1.E-15)THEN                                        LEXMOISR.313
            PMLTEV(I,K)=SONV(I,K)*DEPS1*(QAOUT(I,K)/QVS(I,K)-1.)*(0.65*          LEXMOISR.314
     +                  SLOS(I,K)*SLOS(I,K)+DEPS2**0.5*DEPS3*DUM11(I,K)*         LEXMOISR.315
     +                  SLOS(I,K)**(DEPS4))/AB(I,K)                              LEXMOISR.316
          ENDIF                                                                  LEXMOISR.317
   30   CONTINUE                                                                 LEXMOISR.318
                                                                                 LEXMOISR.319
      DO 40 K=1,KL                                                               LEXMOISR.320
        DO 40 I=IST,IEN                                                          LEXMOISR.321
          PRE(I,K)=AMAX1(-RAIN(I,K)/DT,PRE(I,K))                                 LEXMOISR.322
          PREI(I,K)=AMAX1(-SNOW(I,K)/DT,PREI(I,K))                               LEXMOISR.323
          IF(IEVAP(IN).LT.0)PRE(I,K)=AMAX1(PRE(I,K),0.)                          LEXMOISR.324
          IF(IEVAP(IN).LT.0)PREI(I,K)=AMAX1(PREI(I,K),0.)                        LEXMOISR.325
          IF(TAOUT(I,K).LE.TO)THEN                                               LEXMOISR.326
CONSERVATION OF QC                                                               LEXMOISR.327
            DUM11(I,K)=(PRC(I,K)+PRA(I,K)+PSACW(I,K)+NUFCI(I,K))*DT              LEXMOISR.328
            IF(DUM11(I,K).GT.CLOUD(I,K))THEN                                     LEXMOISR.329
              NUFCI(I,K)=CLOUD(I,K)*NUFCI(I,K)/DUM11(I,K)                        LEXMOISR.330
              PRC(I,K)=CLOUD(I,K)*PRC(I,K)/DUM11(I,K)                            LEXMOISR.331
              PRA(I,K)=CLOUD(I,K)*PRA(I,K)/DUM11(I,K)                            LEXMOISR.332
              PSACW(I,K)=CLOUD(I,K)*PSACW(I,K)/DUM11(I,K)                        LEXMOISR.333
            ENDIF                                                                LEXMOISR.334
CONSERVATION OF CLOUD ICE                                                        LEXMOISR.335
            DUM11(I,K)=(PRCI(I,K)+PRAI(I,K)-PRI(I,K)-PRD(I,K)-NUFCI(I,K)         LEXMOISR.336
     +                 )*DT                                                      LEXMOISR.337
            IF(DUM11(I,K).GT.ICE(I,K))THEN                                       LEXMOISR.338
              PRCI(I,K)=ICE(I,K)*PRCI(I,K)/DUM11(I,K)                            LEXMOISR.339
              PRAI(I,K)=ICE(I,K)*PRAI(I,K)/DUM11(I,K)                            LEXMOISR.340
              PRI(I,K)=ICE(I,K)*PRI(I,K)/DUM11(I,K)                              LEXMOISR.341
              PRD(I,K)=ICE(I,K)*PRD(I,K)/DUM11(I,K)                              LEXMOISR.342
              NUFCI(I,K)=ICE(I,K)*NUFCI(I,K)/DUM11(I,K)                          LEXMOISR.343
            ENDIF                                                                LEXMOISR.344
            DUM11(I,K)=-(PRE(I,K)+PREI(I,K)+PRI(I,K)+PRD(I,K))*PSB(I,J)          LEXMOISR.345
C WATER VAPOR TENDENCY                                                           LEXMOISR.346
            QV3DTEN(I,J,K)=QV3DTEN(I,J,K)+DUM11(I,K)                             LEXMOISR.347
C CLOUD WATER TENDENCY                                                           LEXMOISR.348
            QC3DTEN(I,J,K)=QC3DTEN(I,J,K)-(PRC(I,K)+PRA(I,K)+PSACW(I,K)+         LEXMOISR.349
     +                 NUFCI(I,K))*PSB(I,J)                                      LEXMOISR.350
C RAIN TENDENCY                                                                  LEXMOISR.351
            QR3DTEN(I,J,K)=QR3DTEN(I,J,K)+(PRC(I,K)+PRA(I,K)+PRE(I,K))*          LEXMOISR.352
     +                     PSB(I,J)                                              LEXMOISR.353
C CLOUD ICE TENDENCY                                                             LEXMOISR.354
            QI3DTEN(I,J,K)=QI3DTEN(I,J,K)-(PRCI(I,K)+PRAI(I,K)-PRI(I,K)-         LEXMOISR.355
     +                     PRD(I,K)-NUFCI(I,K))*PSB(I,J)                         LEXMOISR.356
C SNOW TENDENCY                                                                  LEXMOISR.357
            QNI3DTEN(I,J,K)=QNI3DTEN(I,J,K)+(PREI(I,K)+PRCI(I,K)+                LEXMOISR.358
     +                      PRAI(I,K)+PSACW(I,K))*PSB(I,J)                       LEXMOISR.359
            XLATF=XLS-DUM31(I,K)                                                 LEXMOISR.360
            TEMP=-XLS*(PREI(I,K)+PRD(I,K)+PRI(I,K))*PSB(I,J)-DUM31(I,K)*         LEXMOISR.361
     +           PRE(I,K)*PSB(I,J)-XLATF*(PSACW(I,K)+NUFCI(I,K))*                LEXMOISR.362
     +           PSB(I,J)                                                        LEXMOISR.363
            PRD(I,K)=CP*(1.+0.887*QAOUT(I,K))                                    LEXMOISR.364
            IF(IEVAP(IN).EQ.0)TEMP=AMIN1(TEMP,0.)                                LEXMOISR.365
            T3DTEN(I,J,K)=T3DTEN(I,J,K)-TEMP/PRD(I,K)*(1-IFDRY)                  LEXMOISR.366
          ELSE                                                                   LEXMOISR.367
CONSERVATION OF QC                                                               LEXMOISR.368
            DUM11(I,K)=(PRC(I,K)+PRA(I,K)+PSACW(I,K))*DT                         LEXMOISR.369
            IF(DUM11(I,K).GT.CLOUD(I,K))THEN                                     LEXMOISR.370
              PRC(I,K)=CLOUD(I,K)*PRC(I,K)/DUM11(I,K)                            LEXMOISR.371
              PRA(I,K)=CLOUD(I,K)*PRA(I,K)/DUM11(I,K)                            LEXMOISR.372
              PSACW(I,K)=CLOUD(I,K)*PSACW(I,K)/DUM11(I,K)                        LEXMOISR.373
            ENDIF                                                                LEXMOISR.374
CONSERVATION SNOW                                                                LEXMOISR.375
            DUM11(I,K)=(-PMLTEV(I,K)-PSMLT(I,K))*DT                              LEXMOISR.376
            IF(DUM11(I,K).GT.SNOW(I,K))THEN                                      LEXMOISR.377
              PMLTEV(I,K)=SNOW(I,K)*PMLTEV(I,K)/DUM11(I,K)                       LEXMOISR.378
              PSMLT(I,K)=SNOW(I,K)*PSMLT(I,K)/DUM11(I,K)                         LEXMOISR.379
            ENDIF                                                                LEXMOISR.380
            DUM11(I,K)=-(PRE(I,K)+PMLTEV(I,K))*PSB(I,J)                          LEXMOISR.381
C WATER VAPOR TENDENCY                                                           LEXMOISR.382
            QV3DTEN(I,J,K)=QV3DTEN(I,J,K)+DUM11(I,K)                             LEXMOISR.383
C CLOUD WATER TENDENCY                                                           LEXMOISR.384
            QC3DTEN(I,J,K)=QC3DTEN(I,J,K)-(PRC(I,K)+PRA(I,K)+PSACW(I,K))         LEXMOISR.385
     +                     *PSB(I,J)                                             LEXMOISR.386
C RAIN TENDENCY                                                                  LEXMOISR.387
            QR3DTEN(I,J,K)=QR3DTEN(I,J,K)+(PRC(I,K)+PRA(I,K)+PRE(I,K)-           LEXMOISR.388
     +                     PSMLT(I,K)+PSACW(I,K))*PSB(I,J)                       LEXMOISR.389
C SNOW TENDENCY                                                                  LEXMOISR.390
            QNI3DTEN(I,J,K)=QNI3DTEN(I,J,K)+(PSMLT(I,K)+PMLTEV(I,K))*            LEXMOISR.391
     +                      PSB(I,J)                                             LEXMOISR.392
            XLATF=XLS-DUM31(I,K)                                                 LEXMOISR.393
            TEMP=-XLATF*(PSMLT(I,K))*PSB(I,J)-DUM31(I,K)*(PRE(I,K)+              LEXMOISR.394
     +           PMLTEV(I,K))*PSB(I,J)                                           LEXMOISR.395
            PRD(I,K)=CP*(1.+0.887*QAOUT(I,K))                                    LEXMOISR.396
            IF(IEVAP(IN).EQ.0)TEMP=AMIN1(TEMP,0.)                                LEXMOISR.397
            T3DTEN(I,J,K)=T3DTEN(I,J,K)-TEMP/PRD(I,K)*(1-IFDRY)                  LEXMOISR.398
          ENDIF                                                                  LEXMOISR.399
   40   CONTINUE                                                                 LEXMOISR.400
                                                                                 LEXMOISR.401
CLOUD NEXT PART IS TO GET RID OF EXCESS WATER VAPOR IF                           LEXMOISR.402
C SS WITH RESPECT TO WATER, AS IN ANTHES AND WARNER                              LEXMOISR.403
C FOLLOWS EXMOISS OF DUDHIA WITH NO MODIFICATION                                 LEXMOISR.404
C---COMPUTE T, QV, AND QC AT TAU+1 WITHOUT CONDENSATIONAL TERM:                  LEXMOISR.405
      DO 50 K=1,KL                                                               LEXMOISR.406
        DO 50 I=IST,IEN                                                          LEXMOISR.407
          DUM11(I,K)=QV3D(I,J,K)+DT*QV3DTEN(I,J,K)*RPSC(I,J)                     LEXMOISR.408
          SCR4(I,K)=AMAX1(1.E-12,DUM11(I,K))                                     LEXMOISR.409
          DUM21(I,K)=QC3D(I,J,K)+DT*QC3DTEN(I,J,K)*RPSC(I,J)                     LEXMOISR.410
          SCR3(I,K)=AMAX1(0.,DUM21(I,K))                                         LEXMOISR.411
          PRE(I,K)=(A(K)*PSB(I,J)+PTOP)*1000.+PP3D(I,J,K)                        LEXMOISR.412
          SCR7(I,K)=T3D(I,J,K)+DT*T3DTEN(I,J,K)*RPSC(I,J)                        LEXMOISR.413
   50   CONTINUE                                                                 LEXMOISR.414
                                                                                 LEXMOISR.415
C----COMPUTE THE CONDENSATIONAL TERM:                                            LEXMOISR.416
      DO 60 K=1,KL                                                               LEXMOISR.417
        DO 60 I=IST,IEN                                                          LEXMOISR.418
C                                                                                LEXMOISR.419
C-- SUE MODS                                                                     LEXMOISR.420
          DUM11(I,K)=1.E3*SATVPW(SCR7(I,K))                                      LEXMOISR.421
C---------------------------------------------------------                       LEXMOISR.422
          DUM21(I,K)=DUM31(I,K)*DUM31(I,K)/(RV*PRD(I,K))                         LEXMOISR.423
          PRC(I,K)=.622*DUM11(I,K)/(PRE(I,K)-DUM11(I,K))                         LEXMOISR.424
   60   CONTINUE                                                                 LEXMOISR.425
                                                                                 LEXMOISR.426
      DO 70 K=1,KL                                                               LEXMOISR.427
        DO 70 I=IST,IEN                                                          LEXMOISR.428
          r2 = r1 * psb(i,j)                                                     23DEC04.3276
          SCR8(I,K)=(SCR4(I,K)-PRC(I,K))/(1.+DUM21(I,K)*PRC(I,K)/(               LEXMOISR.429
     +              SCR7(I,K)*SCR7(I,K)))                                        LEXMOISR.430
          DUM11(I,K)=SCR3(I,K)+SCR8(I,K)                                         LEXMOISR.431
          IF(DUM11(I,K).GE.0)THEN                                                LEXMOISR.432
            SCR6(I,K)=SCR8(I,K)/DT                                               LEXMOISR.433
          ELSE                                                                   LEXMOISR.434
            SCR6(I,K)=-SCR3(I,K)/DT                                              LEXMOISR.435
          ENDIF                                                                  LEXMOISR.436
          QV3DTEN(I,J,K)=QV3DTEN(I,J,K)-PSB(I,J)*SCR6(I,K)                       LEXMOISR.437
C...INITIATION OF CLOUD WATER:                                                   LEXMOISR.438
          QC3DTEN(I,J,K)=QC3DTEN(I,J,K)+PSB(I,J)*SCR6(I,K)                       LEXMOISR.439
          DUM21(I,K)=DUM31(I,K)/PRD(I,K)                                         LEXMOISR.440
          IF(IEVAP(IN).EQ.0)SCR6(I,K)=AMAX1(SCR6(I,K),0.)                        LEXMOISR.441
          T3DTEN(I,J,K)=T3DTEN(I,J,K)+PSB(I,J)*SCR6(I,K)*DUM21(I,K)              LEXMOISR.442
     +                  *(1-IFDRY)                                               LEXMOISR.443
C--COMPUTE P*T AND P*QR (WITHOUT FALLOUT TERM) AT TAU+1:                         LEXMOISR.444
          SCR4R(I,K)=AMAX1(r2,QR3D(I,J,K)*PSB(I,J)+                              23DEC04.3277
     +               DT*QR3DTEN(I,J,K))                                          LEXMOISR.446
          SCR4S(I,K)=AMAX1(r2,QNI3D(I,J,K)*PSB(I,J)+                             23DEC04.3278
     +               DT*QNI3DTEN(I,J,K))                                         LEXMOISR.448
          SCR4I(I,K)=AMAX1(r2,QI3D(I,J,K)*PSB(I,J)+                              23DEC04.3279
     +               DT*QI3DTEN(I,J,K))                                          LEXMOISR.450
          SCR7(I,K)=T3D(I,J,K)*PSB(I,J)+DT*T3DTEN(I,J,K)                         LEXMOISR.451
          SCR6(I,K)=SCR7(I,K)*RPSC(I,J)                                          LEXMOISR.452
   70   CONTINUE                                                                 LEXMOISR.453
                                                                                 LEXMOISR.454
C--COMPUTE THE FALLOUT TERMS:                                                    LEXMOISR.455
C     COMPUTE FALL TERM WITH SHORTER TIME STEPS WHERE VFALL>DZ/DT                LEXMOISR.456
      DO 80 I=IST,IEN                                                            LEXMOISR.457
        NSTEP=1                                                                  LEXMOISR.458
        DO 90 K=1,KL                                                             LEXMOISR.459
C IN THE NONHYDROSTATIC MODEL PSB IS THE SAME AS PSB, PSA                        LEXMOISR.460
C SIGMA=(PRESSURE-PTOP)/(PSURFACE-PTOP)                                          LEXMOISR.461
C NOTE THAT TEMPERATURE=T*PSB, P UNITS OF KILOPASCALS/M2                         LEXMOISR.462
          RHO2=(A(K)*PSB(I,J)+PTOP+PP3D(I,J,K)*0.001)/(R*SCR7(I,K))              LEXMOISR.463
          IF(SCR4R(I,K).LT.1.E-5)THEN                                            LEXMOISR.464
            VT2R=0.                                                              LEXMOISR.465
          ELSE                                                                   LEXMOISR.466
            DBLSCR=SCALE*RHO2*SCR4R(I,K)*1000.                                   LEXMOISR.467
            DBLSCR=AMIN1(DBLSCR,2.E-2*SCALE-1.)                                  LEXMOISR.468
            SLOR1=LAMDAW(DBLSCR)                                                 LEXMOISR.469
            VT2R=FRAIN*L2BVT(SLOR1)                                              LEXMOISR.470
          ENDIF                                                                  LEXMOISR.471
          IF(SCR4S(I,K).LT.1.E-5)THEN                                            LEXMOISR.472
            VT2S=0.                                                              LEXMOISR.473
          ELSE                                                                   LEXMOISR.474
            DBLSCR=SCALE*RHO2*SCR4S(I,K)*1000.                                   LEXMOISR.475
            DBLSCR=AMIN1(DBLSCR,2.E-2*SCALE-1.)                                  LEXMOISR.476
            SLOS1=LAMDAI(DBLSCR)*NOSF0P25(SONV(I,K))                             LEXMOISR.477
            VT2S=FSNOW*L2BVTS(SLOS1)                                             LEXMOISR.478
          ENDIF                                                                  LEXMOISR.479
          RHO3=((A(K)*PSB(I,J)+PTOP)*1000.+PP3D(I,J,K))/(R*                      LEXMOISR.480
     +         SCR7(I,K)*RPSC(I,J))                                              LEXMOISR.481
C  SEDIMENTATION FORMULA OF HEYMSFIELD AND DONNER (1990 JAS)                     LEXMOISR.482
          IF(ICE(I,K).GT.r1)THEN                                                 23DEC04.3280
            VSED=3.29*(RHO3*ICE(I,K))**0.16                                      LEXMOISR.484
          ELSE                                                                   LEXMOISR.485
            VSED=0.                                                              LEXMOISR.486
          ENDIF                                                                  LEXMOISR.487
C-----------------------------------------------------------                     LEXMOISR.488
C UNITS ARE G-M/S2, RHO2-S2/M2,VT2-M/S                                           LEXMOISR.489
          RHOFAC(K)=RHO0(I,J,K)/(PSB(I,J)*1000.)/RHO2                            05DEC01.746
          FR(K)=G*RHO2*VT2R                                                      LEXMOISR.490
          FS(K)=G*RHO2*VT2S                                                      LEXMOISR.491
          FI(K)=G*RHO2*VSED                                                      LEXMOISR.492
          RGVM=AMAX1(FR(K),FS(K),FI(K))                                          LEXMOISR.493
C 1 IS TO ROUND UP, REPRESENTS NUMBER OF STEPS                                   LEXMOISR.494
          NSTEP=MAX0(IFIX(RGVM*DT/DSIGMA(K)+1.),NSTEP)                           LEXMOISR.495
   90   CONTINUE                                                                 LEXMOISR.496
        DO 100 N=1,NSTEP                                                         LEXMOISR.497
          IF(N.GT.1000)THEN                                                      25JAN00.491
            PRINT *, 'STOPPING IN LEXMOISR, NEST = ',IN,' I = ',I,               25JAN00.492
     +               ' J = ',J                                                   25JAN00.493
            STOP 'IN SUB. LEXMOISR, NSTEP TOO LARGE, PROBABLY NAN'               25JAN00.494
          END IF                                                                 25JAN00.495
          DO 110 K=1,KL                                                          LEXMOISR.500
            FALOUTR(K)=FR(K)*SCR4R(I,K)                                          LEXMOISR.501
            FALOUTS(K)=FS(K)*SCR4S(I,K)                                          LEXMOISR.502
            FALOUTI(K)=FI(K)*SCR4I(I,K)                                          LEXMOISR.503
  110     CONTINUE                                                               LEXMOISR.504
C FOR TOP OF MODEL                                                               LEXMOISR.505
          K=1                                                                    LEXMOISR.506
          FALTNDR=RHOFAC(K)*FALOUTR(K)/DSIGMA(K)                                 05DEC01.747
          FALTNDS=RHOFAC(K)*FALOUTS(K)/DSIGMA(K)                                 05DEC01.748
          FALTNDI=RHOFAC(K)*FALOUTI(K)/DSIGMA(K)                                 05DEC01.749
          QR3DTEN(I,J,K)=QR3DTEN(I,J,K)-FALTNDR/NSTEP                            LEXMOISR.510
          QNI3DTEN(I,J,K)=QNI3DTEN(I,J,K)-FALTNDS/NSTEP                          LEXMOISR.511
          QI3DTEN(I,J,K)=QI3DTEN(I,J,K)-FALTNDI/NSTEP                            LEXMOISR.512
          SCR4R(I,K)=SCR4R(I,K)-FALTNDR*DT/NSTEP                                 LEXMOISR.513
          SCR4S(I,K)=SCR4S(I,K)-FALTNDS*DT/NSTEP                                 LEXMOISR.514
          SCR4I(I,K)=SCR4I(I,K)-FALTNDI*DT/NSTEP                                 LEXMOISR.515
          DO 120 K=2,KL                                                          LEXMOISR.516
            FALTNDR=RHOFAC(K)*(FALOUTR(K)-FALOUTR(K-1))/DSIGMA(K)                05DEC01.750
            FALTNDS=RHOFAC(K)*(FALOUTS(K)-FALOUTS(K-1))/DSIGMA(K)                05DEC01.751
            FALTNDI=RHOFAC(K)*(FALOUTI(K)-FALOUTI(K-1))/DSIGMA(K)                05DEC01.752
            QR3DTEN(I,J,K)=QR3DTEN(I,J,K)-FALTNDR/NSTEP                          LEXMOISR.520
            QNI3DTEN(I,J,K)=QNI3DTEN(I,J,K)-FALTNDS/NSTEP                        LEXMOISR.521
            QI3DTEN(I,J,K)=QI3DTEN(I,J,K)-FALTNDI/NSTEP                          LEXMOISR.522
            SCR4R(I,K)=SCR4R(I,K)-FALTNDR*DT/NSTEP                               LEXMOISR.523
            SCR4S(I,K)=SCR4S(I,K)-FALTNDS*DT/NSTEP                               LEXMOISR.524
            SCR4I(I,K)=SCR4I(I,K)-FALTNDI*DT/NSTEP                               LEXMOISR.525
            FR(K)=AMAX1(FR(K)/DSIGMA(K),FR(K-1)/DSIGMA(K-1))*DSIGMA(K)           LEXMOISR.526
            FS(K)=AMAX1(FS(K)/DSIGMA(K),FS(K-1)/DSIGMA(K-1))*DSIGMA(K)           LEXMOISR.527
            FI(K)=AMAX1(FI(K)/DSIGMA(K),FI(K-1)/DSIGMA(K-1))*DSIGMA(K)           LEXMOISR.528
  120     CONTINUE                                                               LEXMOISR.529
C     ACCUMULATED RAIN                                                           LEXMOISR.530
C 6000 CONVERTS FROM SEC TO MIN AND FROM M TO CM                                 LEXMOISR.531
          RAINNC(I,J)=RAINNC(I,J)+(FALOUTR(KL)+FALOUTS(KL)+FALOUTI(KL))*         LEXMOISR.532
     +                DTMIN*6000./G/NSTEP                                        LEXMOISR.533
          SNOWNCT(I,J)=(FALOUTS(KL)+FALOUTI(KL))*DTMIN*6000./G/NSTEP             05DEC01.753
  100   CONTINUE                                                                 LEXMOISR.534
   80 CONTINUE                                                                   LEXMOISR.535
      DO 95 I=IST,IEN                                                            LEXMOISR.536
        DO 92 K=2,KL                                                             LEXMOISR.537
C MELTING OF CLOUD ICE                                                           LEXMOISR.538
          IF(SCR6(I,K).GT.TO.AND.(QI3DTEN(I,J,K).GT.0.OR.                        LEXMOISR.539
     +       QI3D(I,J,K).GT.0.)) THEN                                            LEXMOISR.540
            XLATF=XLS-DUM31(I,K)                                                 LEXMOISR.541
            DUM11(I,K)=QI3D(I,J,K)*PSB(I,J)+DT*QI3DTEN(I,J,K)                    LEXMOISR.542
            QI3DTEN(I,J,K)=-QI3D(I,J,K)*PSB(I,J)/DT                              LEXMOISR.543
            QC3DTEN(I,J,K)=QC3DTEN(I,J,K)+DUM11(I,K)/DT                          LEXMOISR.544
            T3DTEN(I,J,K)=T3DTEN(I,J,K)-XLATF/PRD(I,K)*DUM11(I,K)/DT*            LEXMOISR.545
     +                    (1-IFDRY)                                              LEXMOISR.546
          ENDIF                                                                  LEXMOISR.547
C HOMOGENOUS FREEZING OF CLOUD ICE                                               LEXMOISR.548
          IF(SCR6(I,K).LT.HGFR.AND.(QC3DTEN(I,J,K).GT.0.OR.                      LEXMOISR.549
     +       QC3D(I,J,K).GT.0.)) THEN                                            LEXMOISR.550
            XLATF=XLS-DUM31(I,K)                                                 LEXMOISR.551
            DUM11(I,K)=QC3D(I,J,K)*PSB(I,J)+DT*QC3DTEN(I,J,K)                    LEXMOISR.552
            QC3DTEN(I,J,K)=-QC3D(I,J,K)*PSB(I,J)/DT                              LEXMOISR.553
            QI3DTEN(I,J,K)=QI3DTEN(I,J,K)+DUM11(I,K)/DT                          LEXMOISR.554
            T3DTEN(I,J,K)=T3DTEN(I,J,K)+XLATF/PRD(I,K)*DUM11(I,K)/DT*            LEXMOISR.555
     +                    (1-IFDRY)                                              LEXMOISR.556
          ENDIF                                                                  LEXMOISR.557
   92   CONTINUE                                                                 LEXMOISR.558
   95 CONTINUE                                                                   LEXMOISR.559
      RETURN                                                                     LEXMOISR.560
      END                                                                        LEXMOISR.561
