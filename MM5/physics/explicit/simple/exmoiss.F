      SUBROUTINE EXMOISS(J,IN,T,QV,QC,QR,PP,TB,QCB,QRB,PSB,RAINNC,               EXMOISS.1
     &                   QCTEN,QRTEN,QVTEN,TTEN,SCR9,IST,IEN)                    EXMOISS.2
      IMPLICIT NONE                                                              EXMOISS.3
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          EXMOISS.4
C                                                                     C          EXMOISS.5
C                                                                     C          EXMOISS.6
C     THIS SUBROUTINE COMPUTES THE MOISTURE TENDENCIES BY THE EXPLICITC          EXMOISS.7
C     MOISTURE SCHEME (HSIE, 1983). THE PROGNOSTIC EQUATIONS INCLUDE  C          EXMOISS.8
C     WATER VAPOR (QV), CLOUD WATER (QC), AND RAIN WATER (QR).        C          EXMOISS.9
C                                                                     C          EXMOISS.10
C     IN = 1 : FOR LARGE DOMAIN                                       C          EXMOISS.11
C        = 2 : FOR NESTED DOMAIN                                      C          EXMOISS.12
C                                                                     C          EXMOISS.13
C     ALL THE OTHER ARGUMENTS ARE PASSED FROM SUBROUTINE "TEND" AND   C          EXMOISS.14
C     EXPLAINED IN "TEND".                                            C          EXMOISS.15
C                                                                     C          19DEC02.1658
C     ADD FLAG (MEYERS) FOR USE OF MEYERS ICE NUCLEI CONC. EQ         C          19DEC02.1659
C     IN PLACE OF FLETCHER EQ                                         C          19DEC02.1660
C                                                                     C          EXMOISS.16
C                                                                     C          EXMOISS.17
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          EXMOISS.18
#     include <parame.incl>                                                      EXMOISS.19
C                                                                                EXMOISS.20
#     include <varia.incl>                                                       EXMOISS.21
#     include <param3.incl>                                                      EXMOISS.22
#     include <param2.incl>                                                      EXMOISS.23
#     include <pmoist.incl>                                                      EXMOISS.24
#     include <various.incl>                                                     EXMOISS.25
#     include <nonhyd.incl>                                                      EXMOISS.26
#     include <nhcnst.incl>                                                      EXMOISS.27
#     include <rpstar.incl>                                                      EXMOISS.28
C                                                                                EXMOISS.29
      REAL       T(MIX,MJX,MKX), TB(MIX,MJX,MKX), TTEN(MIX,MJX,MKX),             EXMOISS.30
     &          QV(MIX,MJX,MKX), PP(MIX,MJX,MKX),QVTEN(MIX,MJX,MKX)              EXMOISS.31
      REAL      PSB(MIX,MJX), RAINNC(MIX,MJX), SCR9(MIX,MKX)                     EXMOISS.32
      REAL      QC(MIXM,MJXM,MKXM), QCB(MIXM,MJXM,MKXM),                         EXMOISS.33
     &       QCTEN(MIXM,MJXM,MKXM),                                              EXMOISS.34
     &          QR(MIXM,MJXM,MKXM), QRB(MIXM,MJXM,MKXM),                         EXMOISS.35
     &       QRTEN(MIXM,MJXM,MKXM)                                               EXMOISS.36
C                                                                                EXMOISS.37
      REAL      SCR2(MIX,MKX),SCR3(MIX,MKX)                                      EXMOISS.38
      REAL      SCR4(MIX,MKX),SCR5(MIX,MKX),SCR6(MIX,MKX)                        EXMOISS.39
      REAL      DUM11(MIX,MKX),DUM21(MIX,MKX),DUM31(MIX,MKX),                    EXMOISS.40
     1           DUM41(MIX,MKX),PRA(MIX,MKX),PRE(MIX,MKX),                       EXMOISS.41
     2           PRC(MIX,MKX),PRI(MIX,MKX),PRD(MIX,MKX),                         EXMOISS.42
     3          SCR7(MIX,MKX),SCR8(MIX,MKX)                                      EXMOISS.43
      REAL      RGV(MKX),FALOUT(MKX),RHOFAC(MKX)                                 05DEC01.850
      REAL      RGVC(MKX),FALOUTC(MKX)                                           EXMOISS.45
C                                                                                EXMOISS.46
      INTEGER   I,J,K,N                                                          EXMOISS.47
      INTEGER   IST,IEN,IN,NSTEP                                                 EXMOISS.48
      INTEGER   ISAT                                                             EXMOISS.49
      REAL      EES,QVSS,BVT2,BVTS2,BVT3,BVTS3,XNC,PRACS,SUPICE,XMI,DI           EXMOISS.50
      REAL      RHO2,ALAB2,VT2,VT2C,FALTND,FALTNDC,OMQ,QCQ,QRQ,QCMF,QRMF         EXMOISS.51
C ADD FLAG FOR MEYERS ICE NUCLEI CONC. EQ                                        19DEC02.1661
      INTEGER MEYERS                                                             19DEC02.1662
C MEYERS=1 : USE MEYERS ICE NUCLEI CONC. EQ.                                     19DEC02.1663
C MEYERS=0 : USE FLETCHER ICE NUCLEI CONC. EQ.                                   19DEC02.1664
      MEYERS=0                                                                   19DEC02.1665
      IF(IPOLAR.EQ.1)MEYERS=1                                                    19DEC02.1666
C                                                                                EXMOISS.52
      DO 15 K=1,KL                                                               EXMOISS.53
        DO 15 I=2,ILXM                                                           EXMOISS.54
          IF(T(I,J,K).GT.TO)THEN                                                 EXMOISS.55
C     V8 SVP FORMULA                                                             EXMOISS.56
            EES=SVP1*EXP(SVP2*(T(I,J,K)-SVPT0)/(T(I,J,K)-SVP3))                  EXMOISS.57
          ELSE                                                                   EXMOISS.58
            EES=.611*EXP(22.514-6.15E3/T(I,J,K))                                 EXMOISS.59
          ENDIF                                                                  EXMOISS.60
          QVSS=.622*EES/(PSB(I,J)*A(K)+PTOP+PP(I,J,K)*0.001-EES)                 EXMOISS.61
          SCR4(I,K)=QV(I,J,K)/QVSS                                               EXMOISS.62
   15   CONTINUE                                                                 EXMOISS.63
                                                                                 EXMOISS.64
C---BEGIN PRODUCTION TERMS CALCULATION:                                          EXMOISS.65
C    ALL THE PRODUCTION TERMS ARE BASED ON T-1 (I.E. XXB) VARIABLES.             EXMOISS.66
      DO 20 K=1,KL                                                               EXMOISS.67
        DO 20 I=IST,IEN                                                          EXMOISS.68
          SCR2(I,K)=AMAX1(0.,QC(I,J,K))                                          EXMOISS.69
          SCR3(I,K)=AMAX1(1.E-25,QR(I,J,K))                                      EXMOISS.70
C...SCR5 CONTAINS SATURATED MIXING RATIO:                                        EXMOISS.71
          SCR5(I,K)=QV(I,J,K)/SCR4(I,K)                                          EXMOISS.72
          SCR8(I,K)=(A(K)*PSB(I,J)+PTOP)*1000.+PP(I,J,K)                         EXMOISS.73
          SCR6(I,K)=SCR8(I,K)/(R*T(I,J,K))                                       EXMOISS.74
          IF(T(I,J,K).GT.TO)THEN                                                 EXMOISS.75
            SCR7(I,K)=(SCR6(I,K)*SCR3(I,K)*PPI*0.001)**0.25                      EXMOISS.76
          ELSE                                                                   EXMOISS.77
            SCR7(I,K)=(SCR6(I,K)*SCR3(I,K)*PPIS*0.001)**0.25                     EXMOISS.78
          ENDIF                                                                  EXMOISS.79
          DUM11(I,K)=8.794E-5*T(I,J,K)**1.81/SCR8(I,K)                           EXMOISS.80
          DUM21(I,K)=1.496E-6*T(I,J,K)**1.5/(T(I,J,K)+120.)                      EXMOISS.81
C     V8 LV FORMULA                                                              EXMOISS.82
          DUM31(I,K)=3.1484E6-XLV1*T(I,J,K)                                      EXMOISS.83
          SCR4(I,K)=1.414E3*DUM21(I,K)                                           EXMOISS.84
          IF(T(I,J,K).GT.TO)THEN                                                 EXMOISS.85
            PRA(I,K)=DUM31(I,K)                                                  EXMOISS.86
          ELSE                                                                   EXMOISS.87
            PRA(I,K)=XLS                                                         EXMOISS.88
          ENDIF                                                                  EXMOISS.89
          DUM41(I,K)=SCR6(I,K)*PRA(I,K)*PRA(I,K)/(SCR4(I,K)*RV*T(I,J,K)*         EXMOISS.90
     +               T(I,J,K))+1./(SCR5(I,K)*DUM11(I,K))                         EXMOISS.91
          PRE(I,K)=DUM21(I,K)/SCR6(I,K)                                          EXMOISS.92
          PRC(I,K)=PRE(I,K)/DUM11(I,K)                                           EXMOISS.93
   20   CONTINUE                                                                 EXMOISS.94
                                                                                 EXMOISS.95
      DO 22 K=1,KL                                                               EXMOISS.96
        DO 22 I=IST,IEN                                                          EXMOISS.97
          DUM11(I,K)=PRC(I,K)**.3333/PRE(I,K)**.5                                EXMOISS.98
          PRE(I,K)=0.                                                            EXMOISS.99
          PRC(I,K)=0.                                                            EXMOISS.100
          PRA(I,K)=0.                                                            EXMOISS.101
          PRI(I,K)=0.                                                            EXMOISS.102
          PRD(I,K)=0.                                                            EXMOISS.103
   22   CONTINUE                                                                 EXMOISS.104
                                                                                 EXMOISS.105
      BVT2=2.5+.5*BVT                                                            EXMOISS.106
      BVTS2=2.5+.5*BVTS                                                          EXMOISS.107
      BVT3=3.+BVT                                                                EXMOISS.108
      BVTS3=3.+BVTS                                                              EXMOISS.109
      DO 30 K=1,KL                                                               EXMOISS.110
        DO 30 I=IST,IEN                                                          EXMOISS.111
          IF(T(I,J,K).LT.TO)THEN                                                 EXMOISS.112
            ISAT=0                                                               EXMOISS.113
            IF(MEYERS.EQ.1)THEN                                                  19DEC02.1667
C MEYERS ICE CONC. EQ.                                                           19DEC02.1668
              XNC=1000.*EXP(-0.639+0.1296*(100.*                                 19DEC02.1669
     +          (QV(I,J,K)/SCR5(I,K)-1.)))/SCR6(I,K)                             19DEC02.1670
            ELSE                                                                 19DEC02.1671
              XNC=XN0*EXP(BETAI*(TO-T(I,J,K)))/SCR6(I,K)                         19DEC02.1672
            ENDIF                                                                19DEC02.1673
            XNC=AMAX1(XNC,10000./SCR6(I,K))                                      EXMOISS.115
C...AUTOCONVERSION OF CLOUD ICE TO SNOW:                                         EXMOISS.116
            PRC(I,K)=AMAX1(0.,(SCR2(I,K)-XMMAX*XNC)/DT)                          EXMOISS.117
C...ACCRETION OF CLOUD ICE BY SNOW:                                              EXMOISS.118
            IF(SCR3(I,K).EQ.0)THEN                                               EXMOISS.119
              PRA(I,K)=0.                                                        EXMOISS.120
            ELSE                                                                 EXMOISS.121
              PRACS=PRACS1                                                       EXMOISS.122
              PRA(I,K)=PRACS*SCR7(I,K)**BVTS3*SCR2(I,K)                          EXMOISS.123
            ENDIF                                                                EXMOISS.124
            SUPICE=(QV(I,J,K)-SCR5(I,K))/DT                                      EXMOISS.125
            IF(SUPICE.GT.0.)THEN                                                 EXMOISS.126
C...INITIATION OF CLOUD ICE:                                                     EXMOISS.127
              PRI(I,K)=AMAX1(0.,(XM0*XNC-SCR2(I,K))/DT)                          EXMOISS.128
              PRI(I,K)=AMIN1(PRI(I,K),SUPICE)                                    EXMOISS.129
              IF(PRI(I,K).GT.SUPICE*.99)ISAT=1                                   EXMOISS.130
            ENDIF                                                                EXMOISS.131
            IF(SCR2(I,K).GT.0.)THEN                                              EXMOISS.132
              XMI=SCR2(I,K)/XNC                                                  EXMOISS.133
C...DEPOSITION/SUBLIMATION OF CLOUD ICE:                                         EXMOISS.134
              DI=16.3*XMI**0.5                                                   EXMOISS.135
              IF(ISAT.EQ.1)THEN                                                  EXMOISS.136
                PRD(I,K)=0.                                                      EXMOISS.137
              ELSE                                                               EXMOISS.138
                PRD(I,K)=4.*DI*SCR6(I,K)*SUPICE*DT*XNC/(SCR5(I,K)*               EXMOISS.139
     +                   DUM41(I,K))                                             EXMOISS.140
              ENDIF                                                              EXMOISS.141
              IF(PRD(I,K).LT.0.)THEN                                             EXMOISS.142
                PRD(I,K)=AMAX1(PRD(I,K),SUPICE/2.)                               EXMOISS.143
              ELSE                                                               EXMOISS.144
                PRD(I,K)=AMIN1(PRD(I,K),SUPICE/2.)                               EXMOISS.145
              ENDIF                                                              EXMOISS.146
              IF(ABS(PRD(I,K)+PRI(I,K)).GT.ABS(.99*SUPICE))ISAT=1                EXMOISS.147
            ENDIF                                                                EXMOISS.148
C...DEPOSITION/SUBLIMATION OF SNOW:                                              EXMOISS.149
C DAVE                                                                           EXMOISS.150
C     IF(SCR3(I,K).GT.0.)                                                        EXMOISS.151
C    - PRE(I,K)=DAVET(0.,(QV(I,J,K)/SCR5(I,K)-1.)*(PRECS1*SCR7(I,K)*             EXMOISS.152
C    1  SCR7(I,K)+PRECS2*DUM11(I,K)*SCR7(I,K)**BVTS2)/DUM41(I,K),                EXMOISS.153
C    2  ISAT.EQ.1)                                                               EXMOISS.154
            IF((SCR3(I,K).GT.0.).AND.(ISAT.EQ.1))THEN                            EXMOISS.155
              PRE(I,K)=0.                                                        EXMOISS.156
            ELSEIF(SCR3(I,K).GT.0.)THEN                                          EXMOISS.157
              PRE(I,K)=(QV(I,J,K)/SCR5(I,K)-1.)*(PRECS1*SCR7(I,K)*               EXMOISS.158
     +                 SCR7(I,K)+PRECS2*DUM11(I,K)*SCR7(I,K)**BVTS2)/            EXMOISS.159
     +                 DUM41(I,K)                                                EXMOISS.160
            ENDIF                                                                EXMOISS.161
            IF(PRE(I,K).LT.0.)THEN                                               EXMOISS.162
              PRE(I,K)=AMAX1(PRE(I,K),SUPICE/2.)                                 EXMOISS.163
            ELSE                                                                 EXMOISS.164
              PRE(I,K)=AMIN1(PRE(I,K),SUPICE/2.)                                 EXMOISS.165
            ENDIF                                                                EXMOISS.166
          ELSE                                                                   EXMOISS.167
C---AUTOCONVERSION OF CLOUD WATER TO RAINWATER:                                  EXMOISS.168
            PRC(I,K)=AMAX1(0.,QCK1*(SCR2(I,K)-QCTH))                             EXMOISS.169
C---ACCRETION OF CLOUD WATER BY RAINWATER:                                       EXMOISS.170
            IF(SCR3(I,K).EQ.0)THEN                                               EXMOISS.171
              PRA(I,K)=0.                                                        EXMOISS.172
            ELSE                                                                 EXMOISS.173
              PRA(I,K)=PRAC*SCR7(I,K)**BVT3*SCR2(I,K)                            EXMOISS.174
            ENDIF                                                                EXMOISS.175
C...EVAPORATION OF RAINWATER:                                                    EXMOISS.176
            IF(SCR3(I,K).GT.0.)PRE(I,K)=(QV(I,J,K)/SCR5(I,K)-1.)*(PREC1*         EXMOISS.177
     +        SCR7(I,K)*SCR7(I,K)+PREC2*DUM11(I,K)*SCR7(I,K)**BVT2)/             EXMOISS.178
     +        DUM41(I,K)                                                         EXMOISS.179
          ENDIF                                                                  EXMOISS.180
C                                                                                EXMOISS.181
C                                                                                EXMOISS.182
   30   CONTINUE                                                                 EXMOISS.183
                                                                                 EXMOISS.184
      DO 40 K=1,KL                                                               EXMOISS.185
        DO 40 I=IST,IEN                                                          EXMOISS.186
          PRE(I,K)=AMAX1(-SCR3(I,K)/DT,PRE(I,K))                                 EXMOISS.187
          IF(IEVAP(IN).LT.0)PRE(I,K)=AMAX1(PRE(I,K),0.)                          EXMOISS.188
          DUM11(I,K)=(PRC(I,K)+PRA(I,K)-PRI(I,K)-PRD(I,K))*DT                    EXMOISS.189
C---READJUST PRC,PRA,PRI,PRD IF (PRC+PRA-PRI-PRD)*DT > SCR2(I,K):                EXMOISS.190
          IF(DUM11(I,K).GT.SCR2(I,K))THEN                                        EXMOISS.191
            PRC(I,K)=SCR2(I,K)*PRC(I,K)/DUM11(I,K)                               EXMOISS.192
            PRA(I,K)=SCR2(I,K)*PRA(I,K)/DUM11(I,K)                               EXMOISS.193
            PRI(I,K)=SCR2(I,K)*PRI(I,K)/DUM11(I,K)                               EXMOISS.194
            PRD(I,K)=SCR2(I,K)*PRD(I,K)/DUM11(I,K)                               EXMOISS.195
          ENDIF                                                                  EXMOISS.196
          DUM21(I,K)=-(PRE(I,K)+PRI(I,K)+PRD(I,K))*PSB(I,J)                      EXMOISS.197
          QVTEN(I,J,K)=QVTEN(I,J,K)+DUM21(I,K)                                   EXMOISS.198
          QCTEN(I,J,K)=QCTEN(I,J,K)-(PRC(I,K)+PRA(I,K)-PRI(I,K)-PRD(I,K)         EXMOISS.199
     +                 )*PSB(I,J)                                                EXMOISS.200
          QRTEN(I,J,K)=QRTEN(I,J,K)+(PRC(I,K)+PRA(I,K)+PRE(I,K))*                EXMOISS.201
     +                 PSB(I,J)                                                  EXMOISS.202
C...LATENT HEAT RELEASE WITH RESPECT TO WATER OR ICE:                            EXMOISS.203
          IF(T(I,J,K).GT.TO)THEN                                                 EXMOISS.204
            DUM41(I,K)=DUM31(I,K)                                                EXMOISS.205
          ELSE                                                                   EXMOISS.206
            DUM41(I,K)=XLS                                                       EXMOISS.207
          ENDIF                                                                  EXMOISS.208
          PRD(I,K)=CP*(1.+0.887*QV(I,J,K))                                       EXMOISS.209
          IF(IEVAP(IN).EQ.0)DUM21(I,K)=AMIN1(DUM21(I,K),0.)                      EXMOISS.210
          TTEN(I,J,K)=TTEN(I,J,K)-DUM41(I,K)*DUM21(I,K)/PRD(I,K)*(1-             EXMOISS.211
     +                IFDRY)                                                     EXMOISS.212
   40   CONTINUE                                                                 EXMOISS.213
                                                                                 EXMOISS.214
C---COMPUTE T, QV, AND QC AT TAU+1 WITHOUT CONDENSATIONAL TERM:                  EXMOISS.215
      DO 50 K=1,KL                                                               EXMOISS.216
        DO 50 I=IST,IEN                                                          EXMOISS.217
          DUM11(I,K)=QV(I,J,K)+DT*QVTEN(I,J,K)*RPSB(I,J)                         EXMOISS.218
          SCR4(I,K)=AMAX1(1.E-12,DUM11(I,K))                                     EXMOISS.219
          DUM21(I,K)=(QCB(I,J,K)+DT*QCTEN(I,J,K))*RPSB(I,J)                      EXMOISS.220
          SCR3(I,K)=AMAX1(0.,DUM21(I,K))                                         EXMOISS.221
          PRE(I,K)=(A(K)*PSB(I,J)+PTOP)*1000.+PPB(I,J,K)*RPSB(I,J)               EXMOISS.222
          SCR7(I,K)=T(I,J,K)+DT*TTEN(I,J,K)*RPSB(I,J)                            EXMOISS.223
   50   CONTINUE                                                                 EXMOISS.224
                                                                                 EXMOISS.225
C----COMPUTE THE CONDENSATIONAL TERM:                                            EXMOISS.226
      DO 60 K=1,KL                                                               EXMOISS.227
        DO 60 I=IST,IEN                                                          EXMOISS.228
          DUM11(I,K)=1.E3*SVP1*                                                  EXMOISS.229
     +               EXP(SVP2*(SCR7(I,K)-SVPT0)/(SCR7(I,K)-SVP3))                EXMOISS.230
          IF(SCR7(I,K).GT.TO)THEN                                                EXMOISS.231
            DUM21(I,K)=DUM31(I,K)*DUM31(I,K)/(RV*PRD(I,K))                       EXMOISS.232
          ELSE                                                                   EXMOISS.233
            DUM21(I,K)=XLS*DUM31(I,K)/(RV*PRD(I,K))                              EXMOISS.234
          ENDIF                                                                  EXMOISS.235
          PRC(I,K)=.622*DUM11(I,K)/(PRE(I,K)-DUM11(I,K))                         EXMOISS.236
   60   CONTINUE                                                                 EXMOISS.237
                                                                                 EXMOISS.238
      DO 70 K=1,KL                                                               EXMOISS.239
        DO 70 I=IST,IEN                                                          EXMOISS.240
          IF(SCR4(I,K).LT.PRC(I,K).AND.SCR7(I,K).LT.TO)GOTO 65                   EXMOISS.241
          SCR8(I,K)=(SCR4(I,K)-PRC(I,K))/(1.+DUM21(I,K)*PRC(I,K)/(               EXMOISS.242
     +              SCR7(I,K)*SCR7(I,K)))                                        EXMOISS.243
          DUM11(I,K)=SCR3(I,K)+SCR8(I,K)                                         EXMOISS.244
          IF(DUM11(I,K).GE.0)THEN                                                EXMOISS.245
            SCR6(I,K)=SCR8(I,K)/DT                                               EXMOISS.246
          ELSE                                                                   EXMOISS.247
            SCR6(I,K)=-SCR3(I,K)/DT                                              EXMOISS.248
          ENDIF                                                                  EXMOISS.249
          QVTEN(I,J,K)=QVTEN(I,J,K)-PSB(I,J)*SCR6(I,K)                           EXMOISS.250
C...INITIATION OF CLOUD WATER:                                                   EXMOISS.251
          QCTEN(I,J,K)=QCTEN(I,J,K)+PSB(I,J)*SCR6(I,K)                           EXMOISS.252
          IF(SCR7(I,K).GT.TO)THEN                                                EXMOISS.253
            DUM21(I,K)=DUM31(I,K)/PRD(I,K)                                       EXMOISS.254
          ELSE                                                                   EXMOISS.255
            DUM21(I,K)=XLS/PRD(I,K)                                              EXMOISS.256
          ENDIF                                                                  EXMOISS.257
          IF(IEVAP(IN).EQ.0)SCR6(I,K)=AMAX1(SCR6(I,K),0.)                        EXMOISS.258
          TTEN(I,J,K)=TTEN(I,J,K)+PSB(I,J)*SCR6(I,K)*DUM21(I,K)*(1-IFDRY         EXMOISS.259
     +                )                                                          EXMOISS.260
C--COMPUTE P*T AND P*QR (WITHOUT FALLOUT TERM) AT TAU+1:                         EXMOISS.261
   65     CONTINUE                                                               EXMOISS.262
          SCR4(I,K)=AMAX1(1.E-25,QRB(I,J,K)+DT*QRTEN(I,J,K))                     EXMOISS.263
          SCR3(I,K)=AMAX1(1.E-25,QCB(I,J,K)+DT*QCTEN(I,J,K))                     EXMOISS.264
          SCR7(I,K)=TB(I,J,K)+DT*TTEN(I,J,K)                                     EXMOISS.265
          SCR6(I,K)=SCR7(I,K)*RPSB(I,J)                                          EXMOISS.266
   70   CONTINUE                                                                 EXMOISS.267
                                                                                 EXMOISS.268
C--COMPUTE THE FALLOUT TERM:                                                     EXMOISS.269
C     COMPUTE FALL TERM WITH SHORTER TIME STEPS WHERE VFALL>DZ/DT                EXMOISS.270
      DO 80 I=IST,IEN                                                            EXMOISS.271
        NSTEP=1                                                                  EXMOISS.272
        DO 90 K=1,KL                                                             EXMOISS.273
          RHO2=(A(K)*PSB(I,J)+PTOP+PPB(I,J,K)*RPSB(I,J)*0.001)/(R*               EXMOISS.274
     +         SCR7(I,K))                                                        EXMOISS.275
          IF(SCR6(I,K).GT.TO)THEN                                                EXMOISS.276
            ALAB2=(RHO2*SCR4(I,K)*PPI)**0.25                                     EXMOISS.277
            VT2=VTC*ALAB2**BVT                                                   EXMOISS.278
            VT2C=0.                                                              EXMOISS.279
          ELSE                                                                   EXMOISS.280
            ALAB2=(RHO2*SCR4(I,K)*PPIS)**0.25                                    EXMOISS.281
            VT2=VTCS*ALAB2**BVTS                                                 EXMOISS.282
C     SEDIMENTATION FORMULA OF HEYMSFIELD AND DONNER (1990, JAS)                 EXMOISS.283
            VT2C=3.29*(1000.*RHO2*SCR3(I,K))**0.16                               EXMOISS.284
          ENDIF                                                                  EXMOISS.285
          RHOFAC(K)=RHO0(I,J,K)/(PSB(I,J)*1000.)/RHO2                            05DEC01.851
          RGV(K)=G*RHO2*VT2                                                      EXMOISS.286
          NSTEP=MAX0(IFIX(RGV(K)*DT/DSIGMA(K)+1.),NSTEP)                         EXMOISS.287
          RGVC(K)=G*RHO2*VT2C                                                    EXMOISS.288
          NSTEP=MAX0(IFIX(RGVC(K)*DT/DSIGMA(K)+1.),NSTEP)                        EXMOISS.289
          SCR5(I,K)=0.                                                           EXMOISS.290
   90   CONTINUE                                                                 EXMOISS.291
        DO 100 N=1,NSTEP                                                         EXMOISS.292
          IF(N.GT.1000)THEN                                                      25JAN00.518
            PRINT *, 'STOPPING IN EXMOISS, NEST = ',IN,' I = ',I,                25JAN00.519
     +               ' J = ',J                                                   25JAN00.520
            STOP 'IN SUB. EXMOISS, NSTEP TOO LARGE, PROBABLY NAN'                25JAN00.521
          END IF                                                                 25JAN00.522
          DO 110 K=1,KL                                                          EXMOISS.295
            FALOUT(K)=RGV(K)*SCR4(I,K)                                           EXMOISS.296
            SCR5(I,K)=SCR5(I,K)+FALOUT(K)/DSIGMA(K)/NSTEP                        EXMOISS.297
            FALOUTC(K)=RGVC(K)*SCR3(I,K)                                         EXMOISS.298
            SCR5(I,K)=SCR5(I,K)+FALOUTC(K)/DSIGMA(K)/NSTEP                       EXMOISS.299
  110     CONTINUE                                                               EXMOISS.300
          K=1                                                                    EXMOISS.301
          FALTND=RHOFAC(K)*FALOUT(K)/DSIGMA(K)                                   05DEC01.852
          QRTEN(I,J,K)=QRTEN(I,J,K)-FALTND/NSTEP                                 EXMOISS.303
          SCR4(I,K)=SCR4(I,K)-FALTND*DT/NSTEP                                    EXMOISS.304
          FALTNDC=RHOFAC(K)*FALOUTC(K)/DSIGMA(K)                                 05DEC01.853
          QCTEN(I,J,K)=QCTEN(I,J,K)-FALTNDC/NSTEP                                EXMOISS.306
          SCR3(I,K)=SCR3(I,K)-FALTNDC*DT/NSTEP                                   EXMOISS.307
          DO 120 K=2,KL                                                          EXMOISS.308
            FALTND=RHOFAC(K)*(FALOUT(K)-FALOUT(K-1))/DSIGMA(K)                   05DEC01.854
            QRTEN(I,J,K)=QRTEN(I,J,K)-FALTND/NSTEP                               EXMOISS.310
            SCR4(I,K)=SCR4(I,K)-FALTND*DT/NSTEP                                  EXMOISS.311
            FALTNDC=RHOFAC(K)*(FALOUTC(K)-FALOUTC(K-1))/DSIGMA(K)                05DEC01.855
            QCTEN(I,J,K)=QCTEN(I,J,K)-FALTNDC/NSTEP                              EXMOISS.313
            SCR3(I,K)=SCR3(I,K)-FALTNDC*DT/NSTEP                                 EXMOISS.314
            RGV(K)=AMAX1(RGV(K)/DSIGMA(K),RGV(K-1)/DSIGMA(K-1))*                 EXMOISS.315
     +             DSIGMA(K)                                                     EXMOISS.316
            RGVC(K)=AMAX1(RGVC(K)/DSIGMA(K),RGVC(K-1)/DSIGMA(K-1))*              EXMOISS.317
     +              DSIGMA(K)                                                    EXMOISS.318
  120     CONTINUE                                                               EXMOISS.319
C     ACCUMULATED RAIN                                                           EXMOISS.320
          RAINNC(I,J)=RAINNC(I,J)+FALOUT(KL)*DTMIN*6000./G/NSTEP                 EXMOISS.321
          RAINNC(I,J)=RAINNC(I,J)+FALOUTC(KL)*DTMIN*6000./G/NSTEP                EXMOISS.322
  100   CONTINUE                                                                 EXMOISS.323
   80 CONTINUE                                                                   EXMOISS.324
      IF(TO.LT.100.)GOTO 96                                                      EXMOISS.325
      DO 95 I=IST,IEN                                                            EXMOISS.326
        DO 92 K=2,KL                                                             EXMOISS.327
          IF(SCR6(I,K).LT.TO)GOTO 92                                             EXMOISS.328
C....MELTING OR FREEZING OF FROZEN PARTICLES DEPENDING ON THE SIGN OF            EXMOISS.329
C....VERTICAL MOTION AT TOP OF LAYER                                             EXMOISS.330
C....CALCULATE OMEGA, QC AND QR AT FULL SIGMA (TAKEN TO BE FREEZING LEVE         EXMOISS.331
          OMQ=(DSIGMA(K-1)*SCR9(I,K)+DSIGMA(K)*SCR9(I,K-1))/(DSIGMA(K)+          EXMOISS.332
     +        DSIGMA(K-1))                                                       EXMOISS.333
          QCQ=(DSIGMA(K-1)*QC(I,J,K)+DSIGMA(K)*QC(I,J,K-1))/(DSIGMA(K)+          EXMOISS.334
     +        DSIGMA(K-1))                                                       EXMOISS.335
          QRQ=(DSIGMA(K-1)*QR(I,J,K)+DSIGMA(K)*QR(I,J,K-1))/(DSIGMA(K)+          EXMOISS.336
     +        DSIGMA(K-1))                                                       EXMOISS.337
          QCMF=OMQ*QCQ/DSIGMA(K)                                                 EXMOISS.338
          QRMF=OMQ*QRQ/DSIGMA(K)+SCR5(I,K-1)                                     EXMOISS.339
C     FREEZING OCCURS IN LAYER ABOVE WHEN THERE IS NET UPWARD MOTION             EXMOISS.340
          IF(QCMF.LT.0.0)THEN                                                    EXMOISS.341
            TTEN(I,J,K-1)=TTEN(I,J,K-1)-XLF0/PRD(I,K-1)*QCMF*(1-IFDRY)           EXMOISS.342
          ELSE                                                                   EXMOISS.343
            TTEN(I,J,K)=TTEN(I,J,K)-XLF0/PRD(I,K)*QCMF*(1-IFDRY)                 EXMOISS.344
          ENDIF                                                                  EXMOISS.345
          IF(QRMF.LT.0.0)THEN                                                    EXMOISS.346
            TTEN(I,J,K-1)=TTEN(I,J,K-1)-XLF0/PRD(I,K-1)*QRMF*(1-IFDRY)           EXMOISS.347
          ELSE                                                                   EXMOISS.348
            TTEN(I,J,K)=TTEN(I,J,K)-XLF0/PRD(I,K)*QRMF*(1-IFDRY)                 EXMOISS.349
          ENDIF                                                                  EXMOISS.350
          GOTO 95                                                                EXMOISS.351
   92   CONTINUE                                                                 EXMOISS.352
   95 CONTINUE                                                                   EXMOISS.353
   96 CONTINUE                                                                   EXMOISS.354
      RETURN                                                                     EXMOISS.355
      END                                                                        EXMOISS.356
                                                                                 EXMOISS.357
