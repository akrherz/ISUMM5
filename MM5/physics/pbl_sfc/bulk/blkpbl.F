      SUBROUTINE BLKPBL(IYY,JXX,J,INEST,U3D,V3D,T3D,QV3D,PP3D,                   BLKPBL.1
     1   QC3D,U3DTEN,V3DTEN,QC3DTEN,T3DTEN,QV3DTEN)                              BLKPBL.2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          BLKPBL.3
C                                                                     C          BLKPBL.4
C                                                                     C          BLKPBL.5
C     THIS SUBROUTINE COMPUTES THE SENSIBLE HEAT, LATENT HEAT, AND    C          BLKPBL.6
C     MOMENTUM FLUXES FROM THE GROUND BY THE BULK-AERODYNAMIC         C          BLKPBL.7
C     PARAMETERIZATION.                                               C          BLKPBL.8
C     IN VERSION 3 UX AND VX ARE AVERAGED TO CROSS POINTS WHICH IS    C          BLKPBL.9
C      MORE CORRECT                                                   C          BLKPBL.10
C                                                                     C          BLKPBL.11
C     USE TGB AS TEMPERATURE, THGB AS THETA                           C          07NOV00.1676
C                                                                     C          BLKPBL.12
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          BLKPBL.13
#     include <parame.incl>                                                      BLKPBL.14
#     include <rpstar.incl>                                                      BLKPBL.15
#     include <sum.incl>                                                         BLKPBL.16
      REAL KZM,KZO,KZMAX,MBAR,MBARK,KA,KH,NSUM,NPSUM,ISUM                        BLKPBL.17
C                                                                                BLKPBL.18
      DIMENSION U3D(MIX,MJX,MKX),V3D(MIX,MJX,MKX),T3D(MIX,MJX,MKX),              BLKPBL.19
     -    QV3D(MIX,MJX,MKX),PP3D(MIX,MJX,MKX),QC3D(MIXM,MJXM,MKXM),              BLKPBL.20
     -    U3DTEN(MIX,MJX,MKX),V3DTEN(MIX,MJX,MKX),T3DTEN(MIX,MJX,MKX),           BLKPBL.21
     -    QV3DTEN(MIX,MJX,MKX),QC3DTEN(MIXM,MJXM,MKXM)                           BLKPBL.22
C                                                                                BLKPBL.23
      DIMENSION    UX(MIX,MKX),    VX(MIX,MKX),    QX(MIX,MKX),                  BLKPBL.24
     1            QCX(MIXM,MKXM), THX(MIX,MKX),  THVX(MIX,MKX),                  BLKPBL.25
     2             RC(MIX,MKX),   DZQ(MIX,MKX),   DZA(MIX,MKX),                  BLKPBL.26
     3           TTNP(MIX,MKX),  QTNP(MIX,MKX), QCTNP(MIXM,MKXM),                BLKPBL.27
     4            KZM(MIX,MKX),    ZA(MIX,MKX),   UXS(MIX,MKX),                  BLKPBL.28
     5            VXS(MIX,MKX),  THXS(MIX,MKX),   QXS(MIX,MKX),                  BLKPBL.29
     6           QCXS(MIXM,MKXM)                                                 BLKPBL.30
      DIMENSION    ZQ(MIX,KXP1), UTNP(2,MIX,MKX), VTNP(2,MIX,MKX)                BLKPBL.31
C                                                                                BLKPBL.32
      DIMENSION   RHOX(MIX),  KZMAX(MIX), GOVRTH(MIX), GZ1OZ0(MIX),              BLKPBL.33
     1            WSPD(MIX),     BR(MIX),   PSIM(MIX),   PSIH(MIX),              BLKPBL.34
     2           UFLXP(MIX),  VFLXP(MIX),  CFLXP(MIX),   FLHC(MIX),              BLKPBL.35
     3            FLQC(MIX), HFKLMX(MIX),  THXSV(MIX),   UXSV(MIX),              BLKPBL.36
     4            VXSV(MIX),   QXSV(MIX),  QCXSV(MIX),   KPBL(MIX),              BLKPBL.37
     5          KPBLM1(MIX),QGH(MIX),TGDSA(MIX),TGDSB(MIX),THGB(MIX)             07NOV00.1677
C                                                                                BLKPBL.39
      DIMENSION     FU(MKX),     FV(MKX),   FTHX(MKX),     FQ(MKX),              BLKPBL.40
     1             FQC(MKX),   ALFA(MKX),     BM(MKX),     EM(MKX),              BLKPBL.41
     2             WGT(MKX),  MBARK(MKX)                                         BLKPBL.42
C                                                                                BLKPBL.43
      DIMENSION CPM(MIX,MKX)                                                     BLKPBL.44
      EQUIVALENCE (CPM(1,1),QXS(1,1))                                            BLKPBL.45
#     include <varia.incl>                                                       BLKPBL.46
#     include <dusolve1.incl>                                                    BLKPBL.47
C                                                                                BLKPBL.48
      DIMENSION CU(MIX),CT(MIX),CUN(MIX),CTN(MIX),                               BLKPBL.49
     1          CELL1(MIX),CELL2(MIX)                                            BLKPBL.50
C                                                                                BLKPBL.51
      DIMENSION CELLA(MIX), CELLB(MIX), CELLC(MIX), CELLD(MIX)                   BLKPBL.52
#     include <param2.incl>                                                      BLKPBL.53
#     include <param3.incl>                                                      BLKPBL.54
#     include <pmoist.incl>                                                      BLKPBL.55
#     include <point2d.incl>                                                     BLKPBL.56
#     include <various.incl>                                                     BLKPBL.57
#     include <nhcnst.incl>                                                      BLKPBL.58
C                                                                                BLKPBL.59
      DATA CKZ/0.001/                                                            BLKPBL.60
      DATA SZKM/1600./                                                           BLKPBL.61
      DATA RIC/3.05/                                                             BLKPBL.62
C                                                                                BLKPBL.63
C----------------------------------------------------------------------          BLKPBL.64
C-----SOME OF THE STORAGE SPACES FOR HIGH-RESOLUTION PBL                         BLKPBL.65
C     ARE USE TO STORE THE VARIABLES IN THIS SUBROUTINE.                         BLKPBL.66
C     REGIME(I,J) : BULK-RICHARDSON NUMBER                                       BLKPBL.67
C     PBL(I,J)    : PBL HEIGHT                                                   BLKPBL.68
C     MOL(I,J)    : EXCHANGE COEFFICIENT FOR MOMENTUM                            BLKPBL.69
C     HOL(I,J)    : EXCHANGE COEFFICIENT FOR HEAT AND MOISTURE                   BLKPBL.70
C     HFX(I,J)    : SENSIBLE HEAT FLUX                                           BLKPBL.71
C     QFX(I,J)    : LATENT HEAT FLUX                                             BLKPBL.72
C     ZOL(I,J)    : TEMPORARY STORAGE SPACE                                      BLKPBL.73
C     DUM1(I)     : PSTAR AT DOT POINT                                           BLKPBL.74
C     SCR1(I,K)   : TEMPERATURE TENDENCY (TTEN)                                  BLKPBL.75
C     SCR2(I,K)   : WATER VAPOR TENDENCY (QVTEN)                                 BLKPBL.76
C                                                                                BLKPBL.77
C                                                                                BLKPBL.78
C----CONVERT GROUND TEMPERATURE TO POTENTIAL TEMPERATURE:                        BLKPBL.79
C                                                                                BLKPBL.80
#ifndef MPP1                                                                     BLKPBL.81
      JM1=MAX0(J-1,1)                                                            BLKPBL.82
#else                                                                            BLKPBL.83
      JM1=MAX0(FLIC_L2G_N(J-1),1)                                                BLKPBL.84
#endif                                                                           BLKPBL.85
      ICUT=0                                                                     BLKPBL.86
      IF(INEST.GT.1)ICUT=1                                                       BLKPBL.87
      DO 300 I=2,ILX                                                             BLKPBL.88
        TGDSA(I)=TGB(I,J)                                                        BLKPBL.89
        PS1=PSB(I,J)+PTOP+PP3D(I,J,KL)*0.001                                     BLKPBL.90
        THGB(I)=TGB(I,J)*(100./PS1)**ROVCP                                       07NOV00.1678
  300 CONTINUE                                                                   BLKPBL.93
C                                                                                BLKPBL.94
C-----DECOUPLE THE VARIABLES:                                                    BLKPBL.95
C.....SCR3(I,K) STORE TEMPERATURE,                                               BLKPBL.96
C     THXS(I,K) STORE VIRTUAL TEMPERATURE.                                       BLKPBL.97
C                                                                                BLKPBL.98
      DO 310 K=1,KL                                                              BLKPBL.99
        DO 310 I=2,ILX                                                           BLKPBL.100
          UX(I,K)=0.25*(U3D(I,J,K)+U3D(I+1,J,K)+U3D(I,J+1,K)+                    BLKPBL.101
     +            U3D(I+1,J+1,K))                                                BLKPBL.102
          VX(I,K)=0.25*(V3D(I,J,K)+V3D(I+1,J,K)+V3D(I,J+1,K)+                    BLKPBL.103
     +            V3D(I+1,J+1,K))                                                BLKPBL.104
          PL=A(K)*PSB(I,J)+PTOP+PP3D(I,J,K)*0.001                                BLKPBL.105
          SCR3(I,K)=T3D(I,J,K)                                                   BLKPBL.106
          THCON=(100./PL)**ROVCP                                                 BLKPBL.107
          THX(I,K)=SCR3(I,K)*THCON                                               BLKPBL.108
          THXS(I,K)=SCR3(I,K)                                                    BLKPBL.109
          THVX(I,K)=THX(I,K)                                                     BLKPBL.110
          CPM(I,K)=CP                                                            BLKPBL.111
          QX(I,K)=0.                                                             BLKPBL.112
  310   CONTINUE                                                                 BLKPBL.113
                                                                                 BLKPBL.114
C                                                                                BLKPBL.115
      IF(IDRY(INEST).EQ.1)GOTO 340                                               BLKPBL.116
      DO 320 K=1,KL                                                              BLKPBL.117
        DO 320 I=2,ILX                                                           BLKPBL.118
          QX(I,K)=QV3D(I,J,K)                                                    BLKPBL.119
          TVCON=(1.+XMOIST(INEST)*EP1*QX(I,K))                                   BLKPBL.120
          THVX(I,K)=THX(I,K)*TVCON                                               BLKPBL.121
          THXS(I,K)=SCR3(I,K)*TVCON                                              BLKPBL.122
          CPM(I,K)=CP*(1.+0.8*QX(I,K)*XMOIST(INEST))                             BLKPBL.123
  320   CONTINUE                                                                 BLKPBL.124
                                                                                 BLKPBL.125
C                                                                                BLKPBL.126
      IF(IMOIST(INEST).EQ.1.OR.IMOIST(INEST).EQ.3)GOTO 340                       BLKPBL.127
      DO 330 K=1,KL                                                              BLKPBL.128
        DO 330 I=2,ILX                                                           BLKPBL.129
          QCX(I,K)=QC3D(I,J,K)                                                   BLKPBL.130
  330   CONTINUE                                                                 BLKPBL.131
                                                                                 BLKPBL.132
  340 CONTINUE                                                                   BLKPBL.133
C                                                                                BLKPBL.134
C-----COMPUTE THE WIND SPEED AT THE LOWEST SIGMA LEVEL.                          BLKPBL.135
C                                                                                BLKPBL.136
      DO 10 I=2,ILX                                                              BLKPBL.137
        WSPD(I)=SQRT(UX(I,KL)*UX(I,KL)+VX(I,KL)*VX(I,KL))                        BLKPBL.138
        DUM2(I)=THGB(I)-THX(I,KL)                                                07NOV00.1679
        IF(DUM2(I).GE.0)THEN                                                     BLKPBL.140
          DTHDZ=DUM2(I)                                                          BLKPBL.141
        ELSE                                                                     BLKPBL.142
          DTHDZ=0.                                                               BLKPBL.143
        ENDIF                                                                    BLKPBL.144
        VCONV=VCONVC*SQRT(DTHDZ)                                                 BLKPBL.145
        WSPD(I)=SQRT(WSPD(I)*WSPD(I)+VCONV*VCONV)                                BLKPBL.146
        WSPD(I)=AMAX1(WSPD(I),1.0)                                               BLKPBL.147
        PS=(PSB(I,J)+PTOP+PP3D(I,J,KL)*0.001)*1000.                              BLKPBL.148
        TV=T3D(I,J,KL)*(1.+XMOIST(INEST)*EP1*QV3D(I,J,KL))                       BLKPBL.149
        RHOX(I)=PS/(R*TV)                                                        BLKPBL.150
   10 CONTINUE                                                                   BLKPBL.151
C                                                                                BLKPBL.152
      IF(ICDCON(INEST).NE.0)GOTO 50                                              BLKPBL.153
C                                                                                BLKPBL.154
C-----COMPUTE THE PBL HEIGHT,                                                    BLKPBL.155
C             THE BULK RICHARDSON NUMBER.                                        BLKPBL.156
C                                                                                BLKPBL.157
      DO 20 I=2,ILX                                                              BLKPBL.158
        PBL(I,J)=PSB(I,J)*DSIGMA(KL)*1000./(G*RHOX(I))                           BLKPBL.159
        REGIME(I,J)=G*PBL(I,J)*(-DUM2(I))/(283.16*WSPD(I)*WSPD(I))               BLKPBL.160
        REGIME(I,J)=AMIN1(REGIME(I,J),2.75)                                      BLKPBL.161
        ZOL(I,J)=ALOG(0.025*PBL(I,J)/ZNT(I,J))                                   BLKPBL.162
   20 CONTINUE                                                                   BLKPBL.163
C                                                                                BLKPBL.164
C-----COMPUTE EXCHANGE COEFFICIENT FOR MOMENTUM:                                 BLKPBL.165
C                                                                                BLKPBL.166
      XKARM=1./KARMAN                                                            BLKPBL.167
      DO 30 I=2,ILX                                                              BLKPBL.168
        CUN(I)=1./(XKARM*ZOL(I,J)+8.4)                                           BLKPBL.169
   30 CONTINUE                                                                   BLKPBL.170
C                                                                                BLKPBL.171
      DO 32 I=2,ILX                                                              BLKPBL.172
C                                                                                BLKPBL.173
C.....STABLE CASE:                                                               BLKPBL.174
C                                                                                BLKPBL.175
        IF(REGIME(I,J).GE.0.)THEN                                                BLKPBL.176
          CELL1(I)=1.-REGIME(I,J)/RIC                                            BLKPBL.177
          CU(I)=CUN(I)*CELL1(I)                                                  BLKPBL.178
        ELSE                                                                     BLKPBL.179
C                                                                                BLKPBL.180
C.....UNSTABLE CASE:                                                             BLKPBL.181
C                                                                                BLKPBL.182
          CELL2(I)=ALOG10(-REGIME(I,J))-3.5                                      BLKPBL.183
          CU(I)=1./(1./CUN(I)-25.*EXP(CELL2(I)*(0.26-0.03*CELL2(I))))            BLKPBL.184
        ENDIF                                                                    BLKPBL.185
   32 CONTINUE                                                                   BLKPBL.186
C                                                                                BLKPBL.187
      DO 34 I=2,ILX                                                              BLKPBL.188
        MOL(I,J)=CU(I)*CU(I)                                                     BLKPBL.189
        MOL(I,J)=AMIN1(MOL(I,J),1.0E-2)                                          BLKPBL.190
   34 CONTINUE                                                                   BLKPBL.191
C                                                                                BLKPBL.192
C-----COMPUTE THE EXCHANGE COEFFICIENT FOR HEAT AND MOISTURE:                    BLKPBL.193
C                                                                                BLKPBL.194
      IF(ISFFLX(INEST).EQ.0)GOTO 140                                             BLKPBL.195
      DO 40 I=2,ILX                                                              BLKPBL.196
        CTN(I)=1./(XKARM*0.74*ZOL(I,J)+7.3)                                      BLKPBL.197
   40 CONTINUE                                                                   BLKPBL.198
C                                                                                BLKPBL.199
      DO 42 I=2,ILX                                                              BLKPBL.200
C                                                                                BLKPBL.201
C.....STABLE CASE:                                                               BLKPBL.202
C                                                                                BLKPBL.203
        IF(REGIME(I,J).GE.0.)THEN                                                BLKPBL.204
          CT(I)=CTN(I)*CELL1(I)                                                  BLKPBL.205
        ELSE                                                                     BLKPBL.206
C                                                                                BLKPBL.207
C.....UNSTABLE CASE:                                                             BLKPBL.208
C                                                                                BLKPBL.209
          CT(I)=1./(1./CTN(I)+1./CU(I)-1./CUN(I))                                BLKPBL.210
        ENDIF                                                                    BLKPBL.211
   42 CONTINUE                                                                   BLKPBL.212
C                                                                                BLKPBL.213
      DO 44 I=2,ILX                                                              BLKPBL.214
        HOL(I,J)=CU(I)*CT(I)                                                     BLKPBL.215
        HOL(I,J)=AMIN1(HOL(I,J),1.0E-2)                                          BLKPBL.216
   44 CONTINUE                                                                   BLKPBL.217
      GOTO 70                                                                    BLKPBL.218
C                                                                                BLKPBL.219
   50 IF(ISFFLX(INEST).EQ.0)GOTO 140                                             BLKPBL.220
C                                                                                BLKPBL.221
      DO 60 I=2,ILX                                                              BLKPBL.222
        IF(XLAND(I,J)-1.5.LE.0)THEN                                              BLKPBL.223
          HOL(I,J)=CH                                                            BLKPBL.224
        ELSE                                                                     BLKPBL.225
          HOL(I,J)=CHSEA                                                         BLKPBL.226
        ENDIF                                                                    BLKPBL.227
   60 CONTINUE                                                                   BLKPBL.228
C                                                                                BLKPBL.229
C-----COMPUTE THE SENSIBLE HEAT FLUX (UNIT : W/(M*M)):                           BLKPBL.230
C                                                                                BLKPBL.231
   70 CONTINUE                                                                   BLKPBL.232
      DO 80 I=1,ILX                                                              BLKPBL.233
        HFX(I,J)=0.                                                              BLKPBL.234
        QFX(I,J)=0.                                                              BLKPBL.235
   80 CONTINUE                                                                   BLKPBL.236
C                                                                                BLKPBL.237
      IF(J.EQ.JLX)GOTO 140                                                       BLKPBL.238
C                                                                                BLKPBL.239
      DO 90 I=2,ILXM                                                             BLKPBL.240
        HFX(I,J)=CPM(I,KL)*RHOX(I)*HOL(I,J)*WSPD(I)*DUM2(I)                      BLKPBL.241
        HFX(I,J)=AMAX1(HFX(I,J),-25.)                                            BLKPBL.242
        FLHC(I)=CPM(I,KL)*RHOX(I)*HOL(I,J)*WSPD(I)                               BLKPBL.243
   90 CONTINUE                                                                   BLKPBL.244
C                                                                                BLKPBL.245
C-----COMPUTE THE LATENT HEAT FLUX (UNIT : W/(M*M)):                             BLKPBL.246
C                                                                                BLKPBL.247
      IF(IDRY(INEST).EQ.1)GOTO 110                                               BLKPBL.248
C                                                                                BLKPBL.249
      DO 100 I=2,ILXM                                                            BLKPBL.250
        PSFC=PSB(I,J)+PTOP+PP3D(I,J,KL)*0.001                                    BLKPBL.251
        E1=SVP1*EXP(SVP2*(TGDSA(I)-SVPT0)/(TGDSA(I)-SVP3))                       BLKPBL.252
        QVS=EP2*E1/(PSFC-E1)                                                     BLKPBL.253
        QFXX=RHOX(I)*HOL(I,J)*WSPD(I)*MAVAIL(I,J)*(QVS-QV3D(I,J,KL))             BLKPBL.254
        FLQC(I)=RHOX(I)*HOL(I,J)*WSPD(I)*MAVAIL(I,J)                             BLKPBL.255
        QFX(I,J)=AMAX1(QFXX,0.)                                                  BLKPBL.256
        PARJSUM(ITQEVA_SUM,J)=PARJSUM(ITQEVA_SUM,J)+QFX(I,J)*DX*DX*DTMIN         BLKPBL.257
     +                        *60.                                               BLKPBL.258
C        TQEVA=TQEVA+QFX(I,J)*DX*DX*DTMIN*60.                                    BLKPBL.259
  100 CONTINUE                                                                   BLKPBL.260
  110 CONTINUE                                                                   BLKPBL.261
C                                                                                BLKPBL.262
C-----COMPUTE THE GROUND TEMPERATURE:                                            BLKPBL.263
C                                                                                BLKPBL.264
      IF(ITGFLG(INEST).NE.1)GOTO 140                                             BLKPBL.265
C                                                                                BLKPBL.266
      IF(MOD(KTAU,NTRAD(INEST)).EQ.0)CALL SFCRAD(IYY,JXX,J,INEST,T3D,            BLKPBL.267
     +  QV3D,PP3D)                                                               BLKPBL.268
C                                                                                BLKPBL.269
      DO 120 I=2,ILXM                                                            BLKPBL.270
        TGDSB(I)=TGB(I,J)                                                        BLKPBL.271
  120 CONTINUE                                                                   BLKPBL.272
C                                                                                BLKPBL.273
      DELTSM=DT                                                                  BLKPBL.274
      CALL SLAB(DELTSM,J,INEST,THX,QX,FLHC,FLQC,PP3D,T3D,1,ILX)                  07NOV00.1680
C                                                                                BLKPBL.276
C-----APPLY ASSELIN FILTER TO TGD FOR LARGE TIME STEP:                           BLKPBL.277
C                                                                                BLKPBL.278
      DO 130 I=2,ILXM                                                            BLKPBL.279
        TGC=TGB(I,J)                                                             BLKPBL.280
        TGB(I,J)=OMUHF*TGA(I,J)+GNUHF*(TGC+TGDSB(I))                             BLKPBL.281
        TGA(I,J)=TGC                                                             BLKPBL.282
  130 CONTINUE                                                                   BLKPBL.283
C                                                                                BLKPBL.284
C-----COMPUTE MOMENTUM FLUXES:                                                   BLKPBL.285
C                                                                                BLKPBL.286
  140 IF(ICDCON(INEST).EQ.0)GOTO 160                                             BLKPBL.287
C                                                                                BLKPBL.288
      DO 150 I=2,ILX                                                             BLKPBL.289
        MOL(I,J)=CD                                                              BLKPBL.290
  150 CONTINUE                                                                   BLKPBL.291
C                                                                                BLKPBL.292
  160 CONTINUE                                                                   BLKPBL.293
      DO 170 I=2,ILX                                                             BLKPBL.294
        CELLA(I)=MOL(I,J)+3.E-3*HT(I,J)/(HT(I,J)+9.81E3)                         BLKPBL.295
        CELLB(I)=MOL(I,J)+3.E-3*HT(I-1,J)/(HT(I-1,J)+9.81E3)                     BLKPBL.296
        CELLC(I)=MOL(I,J)+3.E-3*HT(I,JM1)/(HT(I,JM1)+9.81E3)                     BLKPBL.297
        CELLD(I)=MOL(I,J)+3.E-3*HT(I-1,JM1)/(HT(I-1,JM1)+9.81E3)                 BLKPBL.298
  170 CONTINUE                                                                   BLKPBL.299
C                                                                                BLKPBL.300
      IF(ICDCON(INEST).EQ.0)GOTO 190                                             BLKPBL.301
      DO 180 I=2,ILX                                                             BLKPBL.302
        IF((XLAND(I,J)-1.5).GT.0)THEN                                            BLKPBL.303
          CELLA(I)=CDSEA                                                         BLKPBL.304
        ENDIF                                                                    BLKPBL.305
        IF((XLAND(I-1,J)-1.5).GT.0)THEN                                          BLKPBL.306
          CELLB(I)=CDSEA                                                         BLKPBL.307
        ENDIF                                                                    BLKPBL.308
        IF((XLAND(I,JM1)-1.5).GT.0)THEN                                          BLKPBL.309
          CELLC(I)=CDSEA                                                         BLKPBL.310
        ENDIF                                                                    BLKPBL.311
        IF((XLAND(I-1,JM1)-1.5).GT.0)THEN                                        BLKPBL.312
          CELLD(I)=CDSEA                                                         BLKPBL.313
        ENDIF                                                                    BLKPBL.314
  180 CONTINUE                                                                   BLKPBL.315
  190 CONTINUE                                                                   BLKPBL.316
C                                                                                BLKPBL.317
      DO 200 I=2,ILX                                                             BLKPBL.318
        DRAGC=0.25*(CELLA(I)+CELLB(I)+CELLC(I)+CELLD(I))*RHOX(I)*WSPD(I)         BLKPBL.319
        UFLXP(I)=DRAGC*UX(I,KL)                                                  BLKPBL.320
        VFLXP(I)=DRAGC*VX(I,KL)                                                  BLKPBL.321
  200 CONTINUE                                                                   BLKPBL.322
C                                                                                BLKPBL.323
C-----COMPUTE THE VERTICAL DIFFUSION TERM:                                       BLKPBL.324
C                                                                                BLKPBL.325
C-----COMPUTE THE HEIGHT AT FULL AND HALF SIGMA LEVELS:                          BLKPBL.326
C                                                                                BLKPBL.327
      DO 350 I=2,ILX                                                             BLKPBL.328
        ZQ(I,KLP1)=0.                                                            BLKPBL.329
  350 CONTINUE                                                                   BLKPBL.330
C                                                                                BLKPBL.331
      DO 370 KK=1,KL                                                             BLKPBL.332
        K=KLP1-KK                                                                BLKPBL.333
C                                                                                BLKPBL.334
        DO 360 I=2,ILX                                                           BLKPBL.335
          DUM1(I)=ZQ(I,K+1)                                                      BLKPBL.336
  360   CONTINUE                                                                 BLKPBL.337
C                                                                                BLKPBL.338
        DO 370 I=2,ILX                                                           BLKPBL.339
          CELL=PTOP*RPSB(I,J)                                                    BLKPBL.340
          ZQ(I,K)=ROVG*T0(I,J,K)*ALOG((SIGMA(K+1)+CELL)/(SIGMA(K)+CELL))         BLKPBL.341
     +            +DUM1(I)                                                       BLKPBL.342
  370   CONTINUE                                                                 BLKPBL.343
                                                                                 BLKPBL.344
C                                                                                BLKPBL.345
      DO 380 K=1,KL                                                              BLKPBL.346
        DO 380 I=2,ILX                                                           BLKPBL.347
          ZA(I,K)=0.5*(ZQ(I,K)+ZQ(I,K+1))                                        BLKPBL.348
          DZQ(I,K)=ZQ(I,K)-ZQ(I,K+1)                                             BLKPBL.349
  380   CONTINUE                                                                 BLKPBL.350
                                                                                 BLKPBL.351
C                                                                                BLKPBL.352
C.....DENSITY IS STORED IN SCR6(I,K).                                            BLKPBL.353
      DO 390 K=1,KLM                                                             BLKPBL.354
        DO 390 I=2,ILX                                                           BLKPBL.355
          DZA(I,K)=ZA(I,K)-ZA(I,K+1)                                             BLKPBL.356
          PS=(A(K)*PSB(I,J)+PTOP+PP3D(I,J,K)*0.001)*1000.                        BLKPBL.357
          SCR6(I,K)=PS/(R*THXS(I,K))                                             BLKPBL.358
  390   CONTINUE                                                                 BLKPBL.359
                                                                                 BLKPBL.360
C                                                                                BLKPBL.361
      DO 400 I=2,ILX                                                             BLKPBL.362
        GOVRTH(I)=G/THX(I,KL)                                                    BLKPBL.363
        SCR6(I,KL)=RHOX(I)                                                       BLKPBL.364
  400 CONTINUE                                                                   BLKPBL.365
C                                                                                BLKPBL.366
      DO 410 K=2,KL                                                              BLKPBL.367
        DO 410 I=2,ILX                                                           BLKPBL.368
          RC(I,K)=0.257*DZQ(I,K)**0.175                                          BLKPBL.369
  410   CONTINUE                                                                 BLKPBL.370
                                                                                 BLKPBL.371
C                                                                                BLKPBL.372
C-----COMPUTE THE DIFFUSION COEFFICIENT:                                         BLKPBL.373
C                                                                                BLKPBL.374
      DO 420 K=2,KL                                                              BLKPBL.375
        DO 420 I=2,ILX                                                           BLKPBL.376
          KZMAX(I)=0.8*DZA(I,K-1)*DZQ(I,K)/DT                                    BLKPBL.377
          SS=((UX(I,K-1)-UX(I,K))*(UX(I,K-1)-UX(I,K))+(VX(I,K-1)-VX(I,K)         BLKPBL.378
     +       )*(VX(I,K-1)-VX(I,K)))/(DZA(I,K-1)*DZA(I,K-1))+1.E-9                BLKPBL.379
          RI=GOVRTH(I)*(THVX(I,K-1)-THVX(I,K))/(SS*DZA(I,K-1))                   BLKPBL.380
          KZO=CKZ*DZA(I,K-1)                                                     19DEC02.1691
          IF(RI-RC(I,K).GE.0)THEN                                                BLKPBL.382
            KZM(I,K)=KZO                                                         BLKPBL.383
          ELSE                                                                   BLKPBL.384
            KZM(I,K)=KZO+SQRT(SS)*(RC(I,K)-RI)*SZKM/RC(I,K)                      BLKPBL.385
          ENDIF                                                                  BLKPBL.386
          KZM(I,K)=AMIN1(KZM(I,K),KZMAX(I))                                      BLKPBL.387
  420   CONTINUE                                                                 BLKPBL.388
                                                                                 BLKPBL.389
C                                                                                BLKPBL.390
C-----COMPUTE THE FLUXES:                                                        BLKPBL.391
C                                                                                BLKPBL.392
      DO 430 I=2,ILX                                                             BLKPBL.393
        UTNP(1,I,1)=0.                                                           BLKPBL.394
        VTNP(1,I,1)=0.                                                           BLKPBL.395
        TTNP(I,1)=0.                                                             BLKPBL.396
        QTNP(I,1)=0.                                                             BLKPBL.397
  430 CONTINUE                                                                   BLKPBL.398
C                                                                                BLKPBL.399
      IF(IDRY(INEST).EQ.1)GOTO 450                                               BLKPBL.400
      IF(IMOIST(INEST).EQ.1.OR.IMOIST(INEST).EQ.3)GOTO 450                       BLKPBL.401
      DO 440 I=2,ILX                                                             BLKPBL.402
        QCTNP(I,1)=0.                                                            BLKPBL.403
  440 CONTINUE                                                                   BLKPBL.404
  450 CONTINUE                                                                   BLKPBL.405
C                                                                                BLKPBL.406
      DO 460 K=2,KL                                                              BLKPBL.407
        DO 460 I=2,ILX                                                           BLKPBL.408
          SF=T3D(I,J,K)/THX(I,K)                                                 BLKPBL.409
          UTNP(1,I,K)=SCR6(I,K)*KZM(I,K)*(UX(I,K-1)-UX(I,K))/DZA(I,K-1)          BLKPBL.410
          VTNP(1,I,K)=SCR6(I,K)*KZM(I,K)*(VX(I,K-1)-VX(I,K))/DZA(I,K-1)          BLKPBL.411
          TTNP(I,K)=-SF*CPM(I,K)*SCR6(I,K)*KZM(I,K)*(THX(I,K-1)-THX(I,K)         BLKPBL.412
     +              )/DZA(I,K-1)                                                 BLKPBL.413
          QTNP(I,K)=-SCR6(I,K)*KZM(I,K)*(QX(I,K-1)-QX(I,K))/DZA(I,K-1)           BLKPBL.414
  460   CONTINUE                                                                 BLKPBL.415
                                                                                 BLKPBL.416
C                                                                                BLKPBL.417
      IF(IDRY(INEST).EQ.1)GOTO 480                                               BLKPBL.418
      IF(IMOIST(INEST).EQ.1.OR.IMOIST(INEST).EQ.3)GOTO 480                       BLKPBL.419
      DO 470 K=2,KL                                                              BLKPBL.420
        DO 470 I=2,ILX                                                           BLKPBL.421
          QCTNP(I,K)=-SCR6(I,K)*KZM(I,K)*(QCX(I,K-1)-QCX(I,K))/                  BLKPBL.422
     +               DZA(I,K-1)                                                  BLKPBL.423
  470   CONTINUE                                                                 BLKPBL.424
                                                                                 BLKPBL.425
  480 CONTINUE                                                                   BLKPBL.426
C                                                                                BLKPBL.427
C-----COMPUTE THE TENDENCIES:                                                    BLKPBL.428
C                                                                                BLKPBL.429
      DO 490 I=2+ICUT,ILX-ICUT                                                   BLKPBL.430
        U3DTEN(I,J,KL)=U3DTEN(I,J,KL)-G*(UFLXP(I)-UTNP(1,I,KL))/(1000.*          BLKPBL.431
     +                 DSIGMA(KL))                                               BLKPBL.432
        V3DTEN(I,J,KL)=V3DTEN(I,J,KL)-G*(VFLXP(I)-VTNP(1,I,KL))/(1000.*          BLKPBL.433
     +                 DSIGMA(KL))                                               BLKPBL.434
        T3DTEN(I,J,KL)=T3DTEN(I,J,KL)+G*(HFX(I,J)-TTNP(I,KL))/(1000.*            BLKPBL.435
     +                 CPM(I,KL)*DSIGMA(KL))                                     BLKPBL.436
        QV3DTEN(I,J,KL)=QV3DTEN(I,J,KL)+G*(QFX(I,J)-QTNP(I,KL))/(1000.*          BLKPBL.437
     +                  DSIGMA(KL))                                              BLKPBL.438
  490 CONTINUE                                                                   BLKPBL.439
C                                                                                BLKPBL.440
      IF(IDRY(INEST).EQ.1)GOTO 510                                               BLKPBL.441
      IF(IMOIST(INEST).EQ.1.OR.IMOIST(INEST).EQ.3)GOTO 510                       BLKPBL.442
      DO 500 I=2+ICUT,ILXM-ICUT                                                  BLKPBL.443
        QC3DTEN(I,J,KL)=QC3DTEN(I,J,KL)-G*QCTNP(I,KL)/(1000.*DSIGMA(KL))         BLKPBL.444
  500 CONTINUE                                                                   BLKPBL.445
  510 CONTINUE                                                                   BLKPBL.446
C                                                                                BLKPBL.447
      DO 520 K=1,KLM                                                             BLKPBL.448
        DO 520 I=2+ICUT,ILX-ICUT                                                 BLKPBL.449
          U3DTEN(I,J,K)=U3DTEN(I,J,K)-G*(UTNP(1,I,K+1)-UTNP(1,I,K))/(            BLKPBL.450
     +                  1000.*DSIGMA(K))                                         BLKPBL.451
          V3DTEN(I,J,K)=V3DTEN(I,J,K)-G*(VTNP(1,I,K+1)-VTNP(1,I,K))/(            BLKPBL.452
     +                  1000.*DSIGMA(K))                                         BLKPBL.453
          T3DTEN(I,J,K)=T3DTEN(I,J,K)+G*(TTNP(I,K+1)-TTNP(I,K))/(1000.*          BLKPBL.454
     +                  CPM(I,K)*DSIGMA(K))                                      BLKPBL.455
          QV3DTEN(I,J,K)=QV3DTEN(I,J,K)+G*(QTNP(I,K+1)-QTNP(I,K))/(1000.         BLKPBL.456
     +                   *DSIGMA(K))                                             BLKPBL.457
  520   CONTINUE                                                                 BLKPBL.458
                                                                                 BLKPBL.459
C                                                                                BLKPBL.460
      IF(IDRY(INEST).EQ.1)GOTO 540                                               BLKPBL.461
      IF(IMOIST(INEST).EQ.1.OR.IMOIST(INEST).EQ.3)GOTO 540                       BLKPBL.462
      DO 530 K=1,KLM                                                             BLKPBL.463
        DO 530 I=2+ICUT,ILXM-ICUT                                                BLKPBL.464
          QC3DTEN(I,J,K)=QC3DTEN(I,J,K)+G*(QCTNP(I,K+1)-QCTNP(I,K))/(            BLKPBL.465
     +                   1000.*DSIGMA(K))                                        BLKPBL.466
  530   CONTINUE                                                                 BLKPBL.467
                                                                                 BLKPBL.468
  540 CONTINUE                                                                   BLKPBL.469
C                                                                                BLKPBL.470
C                                                                                BLKPBL.478
C-----CHECK AT ONE POINT:                                                        BLKPBL.479
C                                                                                BLKPBL.480
      IF(INEST.EQ.1)THEN                                                         BLKPBL.481
        XXT=XTIME+DTMIN                                                          BLKPBL.482
      ELSE                                                                       BLKPBL.483
        XXT=XTIME+FLOAT(IRAX)*DTMIN                                              BLKPBL.484
      ENDIF                                                                      BLKPBL.485
C                                                                                BLKPBL.486
C                                                                                BLKPBL.487
  230 CONTINUE                                                                   BLKPBL.488
      RETURN                                                                     BLKPBL.489
      END                                                                        BLKPBL.490
                                                                                 BLKPBL.491
