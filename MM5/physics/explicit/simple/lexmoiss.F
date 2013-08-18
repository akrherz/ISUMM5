      SUBROUTINE LEXMOISS(J,IN,T,QV,QC,QR,PP,TB,QCB,QRB,PSB,RAINNC,              LEXMOISS.1
     &                   QCTEN,QRTEN,QVTEN,TTEN,SCR9,IST,IEN)                    LEXMOISS.2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          LEXMOISS.3
C                                                                     C          LEXMOISS.4
C                                                                     C          LEXMOISS.5
C     THIS SUBROUTINE COMPUTES THE MOISTURE TENDENCIES BY THE EXPLICITC          LEXMOISS.6
C     MOISTURE SCHEME (HSIE, 1983). THE PROGNOSTIC EQUATIONS INCLUDE  C          LEXMOISS.7
C     WATER VAPOR (QV), CLOUD WATER (QC), AND RAIN WATER (QR).        C          LEXMOISS.8
C                                                                     C          LEXMOISS.9
C     IN = 1 : FOR LARGE DOMAIN                                       C          LEXMOISS.10
C        = 2 : FOR NESTED DOMAIN                                      C          LEXMOISS.11
C                                                                     C          LEXMOISS.12
C     ALL THE OTHER ARGUMENTS ARE PASSED FROM SUBROUTINE "TEND" AND   C          LEXMOISS.13
C     EXPLAINED IN "TEND".                                            C          LEXMOISS.14
C                                                                     C          19DEC02.1674
C     ADD FLAG (MEYERS) FOR USE OF MEYERS ICE NUCLEI CONC. EQ         C          19DEC02.1675
C     IN PLACE OF FLETCHER EQ                                         C          19DEC02.1676
C                                                                     C          LEXMOISS.15
C                                                                     C          LEXMOISS.16
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          LEXMOISS.17
#     include <parame.incl>                                                      LEXMOISS.18
#     include <varia.incl>                                                       LEXMOISS.19
#     include <param3.incl>                                                      LEXMOISS.20
#     include <param2.incl>                                                      LEXMOISS.21
#     include <pmoist.incl>                                                      LEXMOISS.22
#     include <various.incl>                                                     LEXMOISS.23
#     include <nhcnst.incl>                                                      LEXMOISS.24
#     include <rpstar.incl>                                                      LEXMOISS.25
C                                                                                LEXMOISS.26
C                                                                                LEXMOISS.27
      DIMENSION  T(MIX,MJX,MKX), TB(MIX,MJX,MKX), TTEN(MIX,MJX,MKX),             LEXMOISS.28
     &          QV(MIX,MJX,MKX), PP(MIX,MJX,MKX),QVTEN(MIX,MJX,MKX)              LEXMOISS.29
      DIMENSION PSB(MIX,MJX), RAINNC(MIX,MJX), SCR9(MIX,MKX)                     LEXMOISS.30
      DIMENSION QC(MIXM,MJXM,MKXM), QCB(MIXM,MJXM,MKXM),                         LEXMOISS.31
     &       QCTEN(MIXM,MJXM,MKXM),                                              LEXMOISS.32
     &          QR(MIXM,MJXM,MKXM), QRB(MIXM,MJXM,MKXM),                         LEXMOISS.33
     &       QRTEN(MIXM,MJXM,MKXM)                                               LEXMOISS.34
C                                                                                LEXMOISS.35
      DIMENSION SCR2(MIX,MKX),SCR3(MIX,MKX)                                      LEXMOISS.36
      DIMENSION SCR4(MIX,MKX),SCR5(MIX,MKX),SCR6(MIX,MKX)                        LEXMOISS.37
      DIMENSION DUM11(MIX,MKX),DUM21(MIX,MKX),DUM31(MIX,MKX),                    LEXMOISS.38
     1           DUM41(MIX,MKX),PRA(MIX,MKX),PRE(MIX,MKX),                       LEXMOISS.39
     2           PRC(MIX,MKX),PRI(MIX,MKX),PRD(MIX,MKX),                         LEXMOISS.40
     3          SCR7(MIX,MKX),SCR8(MIX,MKX)                                      LEXMOISS.41
      DIMENSION RGV(MKX),FALOUT(MKX),RHOFAC(MKX)                                 05DEC01.856
      DIMENSION RGVC(MKX),FALOUTC(MKX)                                           LEXMOISS.43
C ADD FLAG FOR MEYERS ICE NUCLEI CONC. EQ                                        23DEC04.3384
      INTEGER MEYERS                                                             23DEC04.3385
                                                                                 LEXMOISS.44
#     include <hdtabl.incl>                                                      LEXMOISS.45
#     include <functb.incl>                                                      LEXMOISS.46
C MEYERS=1 : USE MEYERS ICE NUCLEI CONC. EQ.                                     19DEC02.1679
C MEYERS=0 : USE FLETCHER ICE NUCLEI CONC. EQ.                                   19DEC02.1680
      MEYERS=0                                                                   19DEC02.1681
      IF(IPOLAR.EQ.1)MEYERS=1                                                    19DEC02.1682
C                                                                                LEXMOISS.47
      DO 15 K=1,KL                                                               LEXMOISS.48
        DO 15 I=IST,IEN                                                          LEXMOISS.49
          IF(T(I,J,K).GT.TO)THEN                                                 LEXMOISS.50
C     V8 SVP FORMULA                                                             LEXMOISS.51
            EES=SATVPW(T(I,J,K))                                                 LEXMOISS.52
          ELSE                                                                   LEXMOISS.53
            EES=SATVPI(T(I,J,K))                                                 LEXMOISS.54
          ENDIF                                                                  LEXMOISS.55
          QVSS=.622*EES/(PSB(I,J)*A(K)+PTOP+PP(I,J,K)*0.001-EES)                 LEXMOISS.56
          SCR4(I,K)=QV(I,J,K)/QVSS                                               LEXMOISS.57
   15   CONTINUE                                                                 LEXMOISS.58
                                                                                 LEXMOISS.59
C---BEGIN PRODUCTION TERMS CALCULATION:                                          LEXMOISS.60
C    ALL THE PRODUCTION TERMS ARE BASED ON T-1 (I.E. XXB) VARIABLES.             LEXMOISS.61
      DO 20 K=1,KL                                                               LEXMOISS.62
        DO 20 I=IST,IEN                                                          LEXMOISS.63
          SCR2(I,K)=AMAX1(0.,QC(I,J,K))                                          LEXMOISS.64
          SCR3(I,K)=AMAX1(1.E-25,QR(I,J,K))                                      LEXMOISS.65
C...SCR5 CONTAINS SATURATED MIXING RATIO:                                        LEXMOISS.66
          SCR5(I,K)=QV(I,J,K)/SCR4(I,K)                                          LEXMOISS.67
          SCR8(I,K)=(A(K)*PSB(I,J)+PTOP)*1000.+PP(I,J,K)                         LEXMOISS.68
          SCR6(I,K)=SCR8(I,K)/(R*T(I,J,K))                                       LEXMOISS.69
          DBLSCR=SCALE*SCR6(I,K)*SCR3(I,K)                                       LEXMOISS.70
          DBLSCR=AMIN1(DBLSCR,2.E-2*SCALE-1.)                                    LEXMOISS.71
          IF(T(I,J,K).GT.TO)THEN                                                 LEXMOISS.72
            SCR7(I,K)=LAMDAW(DBLSCR)                                             LEXMOISS.73
          ELSE                                                                   LEXMOISS.74
            SCR7(I,K)=LAMDAI(DBLSCR)                                             LEXMOISS.75
          ENDIF                                                                  LEXMOISS.76
          DUM11(I,K)=T21P81(T(I,J,K))/SCR8(I,K)                                  LEXMOISS.77
          DUM21(I,K)=T21P50(T(I,J,K))/(T(I,J,K)+120.)                            LEXMOISS.78
C     V8 LV FORMULA                                                              LEXMOISS.79
          DUM31(I,K)=3.1484E6-XLV1*T(I,J,K)                                      LEXMOISS.80
          SCR4(I,K)=1.414E3*DUM21(I,K)                                           LEXMOISS.81
          IF(T(I,J,K).GT.TO)THEN                                                 LEXMOISS.82
            PRA(I,K)=DUM31(I,K)                                                  LEXMOISS.83
          ELSE                                                                   LEXMOISS.84
            PRA(I,K)=XLS                                                         LEXMOISS.85
          ENDIF                                                                  LEXMOISS.86
          DUM41(I,K)=SCR6(I,K)*PRA(I,K)*PRA(I,K)/(SCR4(I,K)*RV*T(I,J,K)*         LEXMOISS.87
     +               T(I,J,K))+1./(SCR5(I,K)*DUM11(I,K))                         LEXMOISS.88
          PRE(I,K)=DUM21(I,K)/SCR6(I,K)                                          LEXMOISS.89
          PRC(I,K)=PRE(I,K)/DUM11(I,K)                                           LEXMOISS.90
   20   CONTINUE                                                                 LEXMOISS.91
                                                                                 LEXMOISS.92
      DO 22 K=1,KL                                                               LEXMOISS.93
        DO 22 I=IST,IEN                                                          LEXMOISS.94
          DUM11(I,K)=PRC20P33(PRC(I,K))/PRE20P5(PRE(I,K))                        LEXMOISS.95
          PRE(I,K)=0.                                                            LEXMOISS.96
          PRC(I,K)=0.                                                            LEXMOISS.97
          PRA(I,K)=0.                                                            LEXMOISS.98
          PRI(I,K)=0.                                                            LEXMOISS.99
          PRD(I,K)=0.                                                            LEXMOISS.100
   22   CONTINUE                                                                 LEXMOISS.101
                                                                                 LEXMOISS.102
      BVT2=2.5+.5*BVT                                                            LEXMOISS.103
      BVTS2=2.5+.5*BVTS                                                          LEXMOISS.104
      BVT3=3.+BVT                                                                LEXMOISS.105
      BVTS3=3.+BVTS                                                              LEXMOISS.106
      DO 30 K=1,KL                                                               LEXMOISS.107
        DO 30 I=IST,IEN                                                          LEXMOISS.108
          IF(T(I,J,K).LT.TO)THEN                                                 LEXMOISS.109
            ISAT=0                                                               LEXMOISS.110
            IF(MEYERS.EQ.1)THEN                                                  19DEC02.1683
C MEYERS ICE CONC. EQ.                                                           19DEC02.1684
              XNC=1000.*EXP(-0.639+0.1296*(100.*                                 19DEC02.1685
     +          (QV(I,J,K)/SCR5(I,K)-1.)))/SCR6(I,K)                             19DEC02.1686
            ELSE                                                                 19DEC02.1687
              XNC=NCICE(T(I,J,K))/SCR6(I,K)                                      19DEC02.1688
            ENDIF                                                                19DEC02.1689
            XNC=AMAX1(XNC,10000./SCR6(I,K))                                      LEXMOISS.112
C...AUTOCONVERSION OF CLOUD ICE TO SNOW:                                         LEXMOISS.113
            PRC(I,K)=AMAX1(0.,(SCR2(I,K)-XMMAX*XNC)/DT)                          LEXMOISS.114
C...ACCRETION OF CLOUD ICE BY SNOW:                                              LEXMOISS.115
            IF(SCR3(I,K).EQ.0)THEN                                               LEXMOISS.116
              PRA(I,K)=0.                                                        LEXMOISS.117
            ELSE                                                                 LEXMOISS.118
              PRACS=PRACS1                                                       LEXMOISS.119
              PRA(I,K)=PRACS*L2BVTS3(SCR7(I,K))*SCR2(I,K)                        LEXMOISS.120
            ENDIF                                                                LEXMOISS.121
            SUPICE=(QV(I,J,K)-SCR5(I,K))/DT                                      LEXMOISS.122
            IF(SUPICE.GT.0.)THEN                                                 LEXMOISS.123
C...INITIATION OF CLOUD ICE:                                                     LEXMOISS.124
              PRI(I,K)=AMAX1(0.,(XM0*XNC-SCR2(I,K))/DT)                          LEXMOISS.125
              PRI(I,K)=AMIN1(PRI(I,K),SUPICE)                                    LEXMOISS.126
              IF(PRI(I,K).GT.SUPICE*.99)ISAT=1                                   LEXMOISS.127
            ENDIF                                                                LEXMOISS.128
            IF(SCR2(I,K).GT.0.)THEN                                              LEXMOISS.129
              XMI=SCR2(I,K)/XNC                                                  LEXMOISS.130
C...DEPOSITION/SUBLIMATION OF CLOUD ICE:                                         LEXMOISS.131
C      DI=16.3*XMI**0.5                                                          LEXMOISS.132
              XMI=AMAX1(XMI,1.E-25)                                              LEXMOISS.133
              DI=16.3*XMI20P5(XMI)                                               LEXMOISS.134
              IF(ISAT.EQ.1)THEN                                                  LEXMOISS.135
                PRD(I,K)=0.                                                      LEXMOISS.136
              ELSE                                                               LEXMOISS.137
                PRD(I,K)=4.*DI*SCR6(I,K)*SUPICE*DT*XNC/(SCR5(I,K)*               LEXMOISS.138
     +                   DUM41(I,K))                                             LEXMOISS.139
              ENDIF                                                              LEXMOISS.140
              IF(PRD(I,K).LT.0.)THEN                                             LEXMOISS.141
                PRD(I,K)=AMAX1(PRD(I,K),SUPICE/2.)                               LEXMOISS.142
              ELSE                                                               LEXMOISS.143
                PRD(I,K)=AMIN1(PRD(I,K),SUPICE/2.)                               LEXMOISS.144
              ENDIF                                                              LEXMOISS.145
              IF(ABS(PRD(I,K)+PRI(I,K)).GT.ABS(.99*SUPICE))ISAT=1                LEXMOISS.146
            ENDIF                                                                LEXMOISS.147
C...DEPOSITION/SUBLIMATION OF SNOW:                                              LEXMOISS.148
C DAVE                                                                           LEXMOISS.149
C     IF(SCR3(I,K).GT.0.)                                                        LEXMOISS.150
C    - PRE(I,K)=DAVET(0.,(QV(I,J,K)/SCR5(I,K)-1.)*(PRECS1*SCR7(I,K)*             LEXMOISS.151
C    1  SCR7(I,K)+PRECS2*DUM11(I,K)*SCR7(I,K)**BVTS2)/DUM41(I,K),                LEXMOISS.152
C    2  ISAT.EQ.1)                                                               LEXMOISS.153
            IF((SCR3(I,K).GT.0.).AND.(ISAT.EQ.1))THEN                            LEXMOISS.154
              PRE(I,K)=0.                                                        LEXMOISS.155
            ELSEIF(SCR3(I,K).GT.0.)THEN                                          LEXMOISS.156
              PRE(I,K)=(QV(I,J,K)/SCR5(I,K)-1.)*(PRECS1*SCR7(I,K)*               LEXMOISS.157
     +                 SCR7(I,K)+PRECS2*DUM11(I,K)*L2BVTS2(SCR7(I,K)))/          LEXMOISS.158
     +                 DUM41(I,K)                                                LEXMOISS.159
            ENDIF                                                                LEXMOISS.160
            IF(PRE(I,K).LT.0.)THEN                                               LEXMOISS.161
              PRE(I,K)=AMAX1(PRE(I,K),SUPICE/2.)                                 LEXMOISS.162
            ELSE                                                                 LEXMOISS.163
              PRE(I,K)=AMIN1(PRE(I,K),SUPICE/2.)                                 LEXMOISS.164
            ENDIF                                                                LEXMOISS.165
          ELSE                                                                   LEXMOISS.166
C---AUTOCONVERSION OF CLOUD WATER TO RAINWATER:                                  LEXMOISS.167
            PRC(I,K)=AMAX1(0.,QCK1*(SCR2(I,K)-QCTH))                             LEXMOISS.168
C---ACCRETION OF CLOUD WATER BY RAINWATER:                                       LEXMOISS.169
            IF(SCR3(I,K).EQ.0)THEN                                               LEXMOISS.170
              PRA(I,K)=0.                                                        LEXMOISS.171
            ELSE                                                                 LEXMOISS.172
              PRA(I,K)=PRAC*L2BVT3(SCR7(I,K))*SCR2(I,K)                          LEXMOISS.173
            ENDIF                                                                LEXMOISS.174
C...EVAPORATION OF RAINWATER:                                                    LEXMOISS.175
            IF(SCR3(I,K).GT.0.)PRE(I,K)=(QV(I,J,K)/SCR5(I,K)-1.)*(PREC1*         LEXMOISS.176
     +        SCR7(I,K)*SCR7(I,K)+PREC2*DUM11(I,K)*L2BVT2(SCR7(I,K)))/           LEXMOISS.177
     +        DUM41(I,K)                                                         LEXMOISS.178
          ENDIF                                                                  LEXMOISS.179
C                                                                                LEXMOISS.180
C                                                                                LEXMOISS.181
   30   CONTINUE                                                                 LEXMOISS.182
                                                                                 LEXMOISS.183
      DO 40 K=1,KL                                                               LEXMOISS.184
        DO 40 I=IST,IEN                                                          LEXMOISS.185
          PRE(I,K)=AMAX1(-SCR3(I,K)/DT,PRE(I,K))                                 LEXMOISS.186
          IF(IEVAP(IN).LT.0)PRE(I,K)=AMAX1(PRE(I,K),0.)                          LEXMOISS.187
          DUM11(I,K)=(PRC(I,K)+PRA(I,K)-PRI(I,K)-PRD(I,K))*DT                    LEXMOISS.188
C---READJUST PRC,PRA,PRI,PRD IF (PRC+PRA-PRI-PRD)*DT > SCR2(I,K):                LEXMOISS.189
          IF(DUM11(I,K).GT.SCR2(I,K))THEN                                        LEXMOISS.190
            PRC(I,K)=SCR2(I,K)*PRC(I,K)/DUM11(I,K)                               LEXMOISS.191
            PRA(I,K)=SCR2(I,K)*PRA(I,K)/DUM11(I,K)                               LEXMOISS.192
            PRI(I,K)=SCR2(I,K)*PRI(I,K)/DUM11(I,K)                               LEXMOISS.193
            PRD(I,K)=SCR2(I,K)*PRD(I,K)/DUM11(I,K)                               LEXMOISS.194
          ENDIF                                                                  LEXMOISS.195
          DUM21(I,K)=-(PRE(I,K)+PRI(I,K)+PRD(I,K))*PSB(I,J)                      LEXMOISS.196
          QVTEN(I,J,K)=QVTEN(I,J,K)+DUM21(I,K)                                   LEXMOISS.197
          QCTEN(I,J,K)=QCTEN(I,J,K)-(PRC(I,K)+PRA(I,K)-PRI(I,K)-PRD(I,K)         LEXMOISS.198
     +                 )*PSB(I,J)                                                LEXMOISS.199
          QRTEN(I,J,K)=QRTEN(I,J,K)+(PRC(I,K)+PRA(I,K)+PRE(I,K))*                LEXMOISS.200
     +                 PSB(I,J)                                                  LEXMOISS.201
C...LATENT HEAT RELEASE WITH RESPECT TO WATER OR ICE:                            LEXMOISS.202
          IF(T(I,J,K).GT.TO)THEN                                                 LEXMOISS.203
            DUM41(I,K)=DUM31(I,K)                                                LEXMOISS.204
          ELSE                                                                   LEXMOISS.205
            DUM41(I,K)=XLS                                                       LEXMOISS.206
          ENDIF                                                                  LEXMOISS.207
          PRD(I,K)=CP*(1.+0.887*QV(I,J,K))                                       LEXMOISS.208
          IF(IEVAP(IN).EQ.0)DUM21(I,K)=AMIN1(DUM21(I,K),0.)                      LEXMOISS.209
          TTEN(I,J,K)=TTEN(I,J,K)-DUM41(I,K)*DUM21(I,K)/PRD(I,K)*(1-             LEXMOISS.210
     +                IFDRY)                                                     LEXMOISS.211
   40   CONTINUE                                                                 LEXMOISS.212
                                                                                 LEXMOISS.213
C---COMPUTE T, QV, AND QC AT TAU+1 WITHOUT CONDENSATIONAL TERM:                  LEXMOISS.214
      DO 50 K=1,KL                                                               LEXMOISS.215
        DO 50 I=IST,IEN                                                          LEXMOISS.216
          DUM11(I,K)=QV(I,J,K)+DT*QVTEN(I,J,K)*RPSB(I,J)                         LEXMOISS.217
          SCR4(I,K)=AMAX1(1.E-12,DUM11(I,K))                                     LEXMOISS.218
          DUM21(I,K)=QC(I,J,K)+DT*QCTEN(I,J,K)*RPSB(I,J)                         LEXMOISS.219
          SCR3(I,K)=AMAX1(0.,DUM21(I,K))                                         LEXMOISS.220
          PRE(I,K)=(A(K)*PSB(I,J)+PTOP)*1000.+PP(I,J,K)                          LEXMOISS.221
          SCR7(I,K)=T(I,J,K)+DT*TTEN(I,J,K)*RPSB(I,J)                            LEXMOISS.222
   50   CONTINUE                                                                 LEXMOISS.223
                                                                                 LEXMOISS.224
C----COMPUTE THE CONDENSATIONAL TERM:                                            LEXMOISS.225
      DO 60 K=1,KL                                                               LEXMOISS.226
        DO 60 I=IST,IEN                                                          LEXMOISS.227
          DUM11(I,K)=1.E3*SATVPW(SCR7(I,K))                                      LEXMOISS.228
          IF(SCR7(I,K).GT.TO)THEN                                                LEXMOISS.229
            DUM21(I,K)=DUM31(I,K)*DUM31(I,K)/(RV*PRD(I,K))                       LEXMOISS.230
          ELSE                                                                   LEXMOISS.231
            DUM21(I,K)=XLS*DUM31(I,K)/(RV*PRD(I,K))                              LEXMOISS.232
          ENDIF                                                                  LEXMOISS.233
          PRC(I,K)=.622*DUM11(I,K)/(PRE(I,K)-DUM11(I,K))                         LEXMOISS.234
   60   CONTINUE                                                                 LEXMOISS.235
                                                                                 LEXMOISS.236
      DO 70 K=1,KL                                                               LEXMOISS.237
        DO 70 I=IST,IEN                                                          LEXMOISS.238
          IF(SCR4(I,K).LT.PRC(I,K).AND.SCR7(I,K).LT.TO)GOTO 65                   LEXMOISS.239
          SCR8(I,K)=(SCR4(I,K)-PRC(I,K))/(1.+DUM21(I,K)*PRC(I,K)/(               LEXMOISS.240
     +              SCR7(I,K)*SCR7(I,K)))                                        LEXMOISS.241
          DUM11(I,K)=SCR3(I,K)+SCR8(I,K)                                         LEXMOISS.242
          IF(DUM11(I,K).GE.0)THEN                                                LEXMOISS.243
            SCR6(I,K)=SCR8(I,K)/DT                                               LEXMOISS.244
          ELSE                                                                   LEXMOISS.245
            SCR6(I,K)=-SCR3(I,K)/DT                                              LEXMOISS.246
          ENDIF                                                                  LEXMOISS.247
          QVTEN(I,J,K)=QVTEN(I,J,K)-PSB(I,J)*SCR6(I,K)                           LEXMOISS.248
C...INITIATION OF CLOUD WATER:                                                   LEXMOISS.249
          QCTEN(I,J,K)=QCTEN(I,J,K)+PSB(I,J)*SCR6(I,K)                           LEXMOISS.250
          IF(SCR7(I,K).GT.TO)THEN                                                LEXMOISS.251
            DUM21(I,K)=DUM31(I,K)/PRD(I,K)                                       LEXMOISS.252
          ELSE                                                                   LEXMOISS.253
            DUM21(I,K)=XLS/PRD(I,K)                                              LEXMOISS.254
          ENDIF                                                                  LEXMOISS.255
          IF(IEVAP(IN).EQ.0)SCR6(I,K)=AMAX1(SCR6(I,K),0.)                        LEXMOISS.256
          TTEN(I,J,K)=TTEN(I,J,K)+PSB(I,J)*SCR6(I,K)*DUM21(I,K)*(1-IFDRY         LEXMOISS.257
     +                )                                                          LEXMOISS.258
C--COMPUTE P*T AND P*QR (WITHOUT FALLOUT TERM) AT TAU+1:                         LEXMOISS.259
   65     CONTINUE                                                               LEXMOISS.260
          SCR4(I,K)=AMAX1(1.E-25,QRB(I,J,K)+DT*QRTEN(I,J,K))                     LEXMOISS.261
          SCR3(I,K)=AMAX1(1.E-25,QCB(I,J,K)+DT*QCTEN(I,J,K))                     LEXMOISS.262
          SCR7(I,K)=TB(I,J,K)+DT*TTEN(I,J,K)                                     LEXMOISS.263
          SCR6(I,K)=SCR7(I,K)*RPSB(I,J)                                          LEXMOISS.264
   70   CONTINUE                                                                 LEXMOISS.265
                                                                                 LEXMOISS.266
C--COMPUTE THE FALLOUT TERM:                                                     LEXMOISS.267
C     COMPUTE FALL TERM WITH SHORTER TIME STEPS WHERE VFALL>DZ/DT                LEXMOISS.268
      DO 80 I=IST,IEN                                                            LEXMOISS.269
        NSTEP=1                                                                  LEXMOISS.270
        DO 90 K=1,KL                                                             LEXMOISS.271
          RHO2=(A(K)*PSB(I,J)+PTOP+PP(I,J,K)*0.001)/(R*SCR7(I,K))                LEXMOISS.272
          DBLSCR=SCALE*RHO2*SCR4(I,K)*1000.                                      LEXMOISS.273
          DBLSCR=AMIN1(DBLSCR,2.E-2*SCALE-1.)                                    LEXMOISS.274
          IF(SCR6(I,K).GT.TO)THEN                                                LEXMOISS.275
            ALAB2=LAMDAW(DBLSCR)                                                 LEXMOISS.276
            VT2=VTC*L2BVT(ALAB2)                                                 LEXMOISS.277
            VT2C=0.                                                              LEXMOISS.278
          ELSE                                                                   LEXMOISS.279
            ALAB2=LAMDAI(DBLSCR)                                                 LEXMOISS.280
            VT2=VTCS*L2BVTS(ALAB2)                                               LEXMOISS.281
C     SEDIMENTATION FORMULA OF HEYMSFIELD AND DONNER (1990, JAS)                 LEXMOISS.282
            VT2C=3.29*(1000.*RHO2*SCR3(I,K))**0.16                               LEXMOISS.283
          ENDIF                                                                  LEXMOISS.284
          RHOFAC(K)=RHO0(I,J,K)/(PSB(I,J)*1000.)/RHO2                            05DEC01.857
          RGV(K)=G*RHO2*VT2                                                      LEXMOISS.285
          NSTEP=MAX0(IFIX(RGV(K)*DT/DSIGMA(K)+1.),NSTEP)                         LEXMOISS.286
          RGVC(K)=G*RHO2*VT2C                                                    LEXMOISS.287
          NSTEP=MAX0(IFIX(RGVC(K)*DT/DSIGMA(K)+1.),NSTEP)                        LEXMOISS.288
          SCR5(I,K)=0.                                                           LEXMOISS.289
   90   CONTINUE                                                                 LEXMOISS.290
        DO 100 N=1,NSTEP                                                         LEXMOISS.291
          IF(N.GT.1000)THEN                                                      25JAN00.523
            PRINT *, 'STOPPING IN LEXMOISS, NEST = ',IN,' I = ',I,               25JAN00.524
     +               ' J = ',J                                                   25JAN00.525
            STOP 'IN SUB. LEXMOISS, NSTEP TOO LARGE, PROBABLY NAN'               25JAN00.526
          END IF                                                                 25JAN00.527
          DO 110 K=1,KL                                                          LEXMOISS.294
            FALOUT(K)=RGV(K)*SCR4(I,K)                                           LEXMOISS.295
            SCR5(I,K)=SCR5(I,K)+FALOUT(K)/DSIGMA(K)/NSTEP                        LEXMOISS.296
            FALOUTC(K)=RGVC(K)*SCR3(I,K)                                         LEXMOISS.297
            SCR5(I,K)=SCR5(I,K)+FALOUTC(K)/DSIGMA(K)/NSTEP                       LEXMOISS.298
  110     CONTINUE                                                               LEXMOISS.299
          K=1                                                                    LEXMOISS.300
          FALTND=RHOFAC(K)*FALOUT(K)/DSIGMA(K)                                   05DEC01.858
          QRTEN(I,J,K)=QRTEN(I,J,K)-FALTND/NSTEP                                 LEXMOISS.302
          SCR4(I,K)=SCR4(I,K)-FALTND*DT/NSTEP                                    LEXMOISS.303
          FALTNDC=RHOFAC(K)*FALOUTC(K)/DSIGMA(K)                                 05DEC01.859
          QCTEN(I,J,K)=QCTEN(I,J,K)-FALTNDC/NSTEP                                LEXMOISS.305
          SCR3(I,K)=SCR3(I,K)-FALTNDC*DT/NSTEP                                   LEXMOISS.306
          DO 120 K=2,KL                                                          LEXMOISS.307
            FALTND=RHOFAC(K)*(FALOUT(K)-FALOUT(K-1))/DSIGMA(K)                   05DEC01.860
            QRTEN(I,J,K)=QRTEN(I,J,K)-FALTND/NSTEP                               LEXMOISS.309
            SCR4(I,K)=SCR4(I,K)-FALTND*DT/NSTEP                                  LEXMOISS.310
            FALTNDC=RHOFAC(K)*(FALOUTC(K)-FALOUTC(K-1))/DSIGMA(K)                05DEC01.861
            QCTEN(I,J,K)=QCTEN(I,J,K)-FALTNDC/NSTEP                              LEXMOISS.312
            SCR3(I,K)=SCR3(I,K)-FALTNDC*DT/NSTEP                                 LEXMOISS.313
            RGV(K)=AMAX1(RGV(K)/DSIGMA(K),RGV(K-1)/DSIGMA(K-1))*                 LEXMOISS.314
     +             DSIGMA(K)                                                     LEXMOISS.315
            RGVC(K)=AMAX1(RGVC(K)/DSIGMA(K),RGVC(K-1)/DSIGMA(K-1))*              LEXMOISS.316
     +              DSIGMA(K)                                                    LEXMOISS.317
  120     CONTINUE                                                               LEXMOISS.318
C     ACCUMULATED RAIN                                                           LEXMOISS.319
          RAINNC(I,J)=RAINNC(I,J)+FALOUT(KL)*DTMIN*6000./G/NSTEP                 LEXMOISS.320
          RAINNC(I,J)=RAINNC(I,J)+FALOUTC(KL)*DTMIN*6000./G/NSTEP                LEXMOISS.321
  100   CONTINUE                                                                 LEXMOISS.322
   80 CONTINUE                                                                   LEXMOISS.323
      IF(TO.LT.100.)GOTO 96                                                      LEXMOISS.324
      DO 95 I=IST,IEN                                                            LEXMOISS.325
        DO 92 K=2,KL                                                             LEXMOISS.326
          IF(SCR6(I,K).LT.TO)GOTO 92                                             LEXMOISS.327
C....MELTING OR FREEZING OF FROZEN PARTICLES DEPENDING ON THE SIGN OF            LEXMOISS.328
C....VERTICAL MOTION AT TOP OF LAYER                                             LEXMOISS.329
C....CALCULATE OMEGA, QC AND QR AT FULL SIGMA (TAKEN TO BE FREEZING LEVE         LEXMOISS.330
          OMQ=(DSIGMA(K-1)*SCR9(I,K)+DSIGMA(K)*SCR9(I,K-1))/(DSIGMA(K)+          LEXMOISS.331
     +        DSIGMA(K-1))                                                       LEXMOISS.332
          QCQ=(DSIGMA(K-1)*QC(I,J,K)+DSIGMA(K)*QC(I,J,K-1))/(DSIGMA(K)+          LEXMOISS.333
     +        DSIGMA(K-1))                                                       LEXMOISS.334
          QRQ=(DSIGMA(K-1)*QR(I,J,K)+DSIGMA(K)*QR(I,J,K-1))/(DSIGMA(K)+          LEXMOISS.335
     +        DSIGMA(K-1))                                                       LEXMOISS.336
          QCMF=OMQ*QCQ/DSIGMA(K)                                                 LEXMOISS.337
          QRMF=OMQ*QRQ/DSIGMA(K)+SCR5(I,K-1)                                     LEXMOISS.338
C     FREEZING OCCURS IN LAYER ABOVE WHEN THERE IS NET UPWARD MOTION             LEXMOISS.339
          IF(QCMF.LT.0.0)THEN                                                    LEXMOISS.340
            TTEN(I,J,K-1)=TTEN(I,J,K-1)-XLF0/PRD(I,K-1)*QCMF*(1-IFDRY)           LEXMOISS.341
          ELSE                                                                   LEXMOISS.342
            TTEN(I,J,K)=TTEN(I,J,K)-XLF0/PRD(I,K)*QCMF*(1-IFDRY)                 LEXMOISS.343
          ENDIF                                                                  LEXMOISS.344
          IF(QRMF.LT.0.0)THEN                                                    LEXMOISS.345
            TTEN(I,J,K-1)=TTEN(I,J,K-1)-XLF0/PRD(I,K-1)*QRMF*(1-IFDRY)           LEXMOISS.346
          ELSE                                                                   LEXMOISS.347
            TTEN(I,J,K)=TTEN(I,J,K)-XLF0/PRD(I,K)*QRMF*(1-IFDRY)                 LEXMOISS.348
          ENDIF                                                                  LEXMOISS.349
          GOTO 95                                                                LEXMOISS.350
   92   CONTINUE                                                                 LEXMOISS.351
   95 CONTINUE                                                                   LEXMOISS.352
   96 CONTINUE                                                                   LEXMOISS.353
      RETURN                                                                     LEXMOISS.354
      END                                                                        LEXMOISS.355
                                                                                 LEXMOISS.356
