      SUBROUTINE HIRPBL(IYY,JXX,J,IN,U3D,V3D,T3D,QV3D,PP3D,                      HIRPBL.1
     1                      QC3D,QI3D,QNC3D,U3DTEN,V3DTEN,QC3DTEN,               HIRPBL.2
     2                      T3DTEN,QV3DTEN,QI3DTEN,QNC3DTEN)                     HIRPBL.3
      IMPLICIT NONE                                                              HIRPBL.4
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          HIRPBL.5
C                                                                     C          HIRPBL.6
C                                                                     C          HIRPBL.7
C     THIS SUBROUTINE IS DESIGNED TO SIMULATE CHARACTERISTICS OF      C          HIRPBL.8
C     THE PLANETARY BOUNDARY LAYER AND THE EARTH`S SURFACE AS THEY    C          HIRPBL.9
C     INTERACT WITH EACH OTHER AND THE FREE ATMOSPHERE ABOVE.         C          HIRPBL.10
C     THE ALGORITHMS ARE WRITTEN TO ALLOW THE PBL TO BE DESCRIBED     C          HIRPBL.11
C     AS A DISCRETE, VARIABLE NUMBER OF MODEL LAYERS OF ARBITRARY     C          HIRPBL.12
C     THICKNESS. THE LOWEST MODEL LAYER (SURFACE LAYER) SHOULD BE     C          HIRPBL.13
C     AS THIN AS POSSIBLE (NO MORE THAN 0.002 IN SIGMA OR 20 M.)      C          HIRPBL.14
C                                                                     C          HIRPBL.15
C     THIS CODE WAS DEVELOPED BY D. L. ZHANG AND MODIFIED BY          C          HIRPBL.16
C     N. L. SEAMAN BASED ON THE BOUNDARY LAYER MODEL DEVELOPED BY     C          HIRPBL.17
C     A. K. BLACKADAR.                                                C          HIRPBL.18
C                                                                     C          HIRPBL.19
C     REORGANIZED AND MODIFIED BY E.-Y. HSIE ON MAY, 1984.            C          HIRPBL.20
C                                                                     C          HIRPBL.21
C     THE PBL SCHEME CONSIDERS 3 ATMOSPHERE ZONES AND 2 SOIL ZONES:   C          HIRPBL.22
C        1. FREE ATMOSPHERE                                           C          HIRPBL.23
C        2. OUTER LAYER OR MIXED LAYER OF THE PBL                     C          HIRPBL.24
C        3. ATMOSPHERE SURFACE LAYER                                  C          HIRPBL.25
C        4. SOIL SURFACE SLAB                                         C          HIRPBL.26
C        5. SOIL SUBSTRATE                                            C          HIRPBL.27
C                                                                     C          HIRPBL.28
C     REFERENCES:                                                     C          HIRPBL.29
C                                                                     C          HIRPBL.30
C        BLACKADAR (1976) PREPRINTS, SYMP. ON ATMOS. TURB., DIFF.,    C          HIRPBL.31
C             AND AIR QUAL., RALEIGH, N.C. - REFERRED TO AS AKB(1976).C          HIRPBL.32
C                                                                     C          HIRPBL.33
C        BLACKADAR (1979) PREPRINTS, SYMP. ON TURB., DIFF., AND AIR   C          HIRPBL.34
C             POLL., RENO, NV.- REFERRED TO AS AKB(1979).             C          HIRPBL.35
C                                                                     C          HIRPBL.36
C        ZHANG AND ANTHES (1982), J. APPL. METEOR., 1594-1609. -      C          HIRPBL.37
C             REFERRED TO AS ZA(1982).                                C          HIRPBL.38
C                                                                     C          HIRPBL.39
C     CHANGES:                                                        C          HIRPBL.40
C                                                                     C          HIRPBL.41
C        REMOVE MITOLD. NO DEPENDENCE OF MITER ON PREVIOUS TIME-STEP  C          HIRPBL.42
C        ANY MORE (21APR97)                                           C          HIRPBL.43
C                                                                     C          HIRPBL.44
C        INCREASE MITER ITERATIONS TO ALLOW THINNER LAYERS (10OCT97)  C          HIRPBL.45
C                                                                     C          HIRPBL.46
C        PUT LOWER LIMIT ON PSI TO PREVENT FLUXES FROM BECOMING TOO   C          HIRPBL.47
C        SMALL IN STABLE REGIMES (DUDHIA, FALL 1997)                  C          HIRPBL.48
C                                                                     C          HIRPBL.49
C        APPLY IMPLICIT DIFFUSION DOWN TO K=KPBL TO PREVENT SUPER-    C          HIRPBL.50
C        ADIABAT IN CLOUD-TOPPED PBL. K=KPBL NOW HAS BOTH IMPLICIT    C          HIRPBL.51
C        DIFFUSION AND FREE-CONVECTIVE TENDENCIES ADDED               C          HIRPBL.52
C                                                                     C          HIRPBL.53
C        ADD ARRAYS THCON(I,K) AND PS(I) FOR EFFICIENCY               C          HIRPBL.54
C                                                                     C          HIRPBL.55
C        CHARNOCK PARAMETERS NOW COME FROM NAMELIST (DEFAULT SAME)    C          HIRPBL.56
C                                                                     C          HIRPBL.57
C          USE TGB AS A TEMPERATURE NOT THETA IN PBL                  C          07NOV00.1749
C                                                                     C          07NOV00.1750
C        ADD IZ0TOPT SWITCH AND 2M T & Q, 10M U & V                   C          05DEC01.877
C                                                                     C          05DEC01.878
C        MODIFIED MOIST VERTICAL DIFFUSION TO PREVENT LOW-CLOUD BIAS  C          05DEC01.879
C                                                                     C          05DEC01.880
C        ISFMTHD = 1 ADDED NIELSEN-GAMMON CORRECTION TO STABLE        C          23DEC04.3424
C          REGIME 2M AND 10M INTERPOLATIONS                           C          23DEC04.3425
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          HIRPBL.58
#     include <parame.incl>                                                      HIRPBL.59
#     include <rpstar.incl>                                                      HIRPBL.60
#     include <sum.incl>                                                         HIRPBL.61
                                                                                 HIRPBL.62
      REAL       KZO,MBAR,KA,KH,NSUM,NPSUM                                       HIRPBL.63
      REAL       KZM(MIX,KXP1),KZMAX(MIX),MBARK(MKX)                             HIRPBL.64
C                                                                                HIRPBL.65
      REAL         UX(MIX,MKX),    VX(MIX,MKX),    QX(MIX,MKX),                  HIRPBL.66
     1            QCX(MIXM,MKXM), THX(MIX,MKX),  THVX(MIX,MKX),                  HIRPBL.67
     2             RC(MIX,MKX),   DZQ(MIX,MKX),   DZA(MIX,MKX),                  HIRPBL.68
     3           TTNP(MIX,MKX),  QTNP(MIX,MKX), QCTNP(MIXM,MKXM),                HIRPBL.69
     4            ZA(MIX,MKX),   UXS(MIX,MKX),                                   HIRPBL.70
     5            VXS(MIX,MKX),  THXS(MIX,MKX),   QXS(MIX,MKX),                  HIRPBL.71
     6           QCXS(MIXM,MKXM), THCON(MIX,MKX)                                 HIRPBL.72
      INTEGER   KPBL(MIX),KPBLM1(MIX)                                            HIRPBL.73
      INTEGER   KPBL3(MIX)                                                       HIRPBL.74
      REAL      QIX(MIXM,MKXM),QITNP(MIXM,MKXM),                                 HIRPBL.75
     1          QIXS(MIXM,MKXM),FLQI(MIX),QIXSV(MIX)                             HIRPBL.76
      REAL      QNCX(MIXM,MKXM),QNCNP(MIXM,MKXM),FQNC(MIXM,MKXM),                HIRPBL.77
     1          QNCXS(MIXM,MKXM),FLQNC(MIX),QNCXSV(MIX)                          HIRPBL.78
      REAL         ZQ(MIX,KXP1), UTNP(MIX,MKX), VTNP(MIX,MKX)                    HIRPBL.79
C                                                                                HIRPBL.80
      REAL        RHOX(MIX), GOVRTH(MIX), GZ1OZ0(MIX),                           HIRPBL.81
     1            WSPD(MIX),     BR(MIX),   PSIM(MIX),   PSIH(MIX),              HIRPBL.82
     2           UFLXP(MIX),  VFLXP(MIX),  CFLXP(MIX),   FLHC(MIX),              HIRPBL.83
     3            FLQC(MIX), HFKLMX(MIX),  THXSV(MIX),   UXSV(MIX),              HIRPBL.84
     4            VXSV(MIX),   QXSV(MIX),  QCXSV(MIX),                           HIRPBL.85
     5             QGH(MIX),  TGDSA(MIX),  TGDSB(MIX),   THGB(MIX),              07NOV00.1751
     6              PS(MIX), DENOMQ(MIX)                                         05DEC01.881
C                                                                                HIRPBL.88
      REAL      gz10oz0(mix),gz2oz0(mix),psih10(mix),psih2(mix),                 05DEC01.882
     -     psim10(mix),psim2(mix),u10(mix),v10(mix),th2(mix)                     05DEC01.883
     -     ,gz1oz0q(mix),gz2oz0q(mix),q2(mix)                                    05DEC01.884
                                                                                 05DEC01.885
CCCC                                                                             HIRPBL.89
      REAL      FU(MIX,MKX),FV(MIX,MKX),FTHX(MIX,MKX),FQ(MIX,MKX),               HIRPBL.90
     1          FQC(MIX,MKX),ALFA(MIX,MKX),BM(MIX,MKX),EM(MIX,MKX),              HIRPBL.91
     2          WGT(MKX),  FQI(MIX,MKX)                                          HIRPBL.92
CCCC                                                                             HIRPBL.93
#     include <varia.incl>                                                       HIRPBL.94
#     include <dusolve1.incl>                                                    HIRPBL.95
#     include <param2.incl>                                                      HIRPBL.96
#     include <param3.incl>                                                      HIRPBL.97
#     include <pmoist.incl>                                                      HIRPBL.98
#     include <point2d.incl>                                                     HIRPBL.99
#     include <various.incl>                                                     HIRPBL.100
#     include <nhcnst.incl>                                                      HIRPBL.101
#     include <soil.incl>                                                        HIRPBL.102
#     include <soilcnst.incl>                                                    HIRPBL.103
#     include <addrcu.incl>                                                      HIRPBL.104
C                                                                                HIRPBL.105
C----------------------------------------------------------------------          HIRPBL.106
      REAL      CPM(MIX)                                                         HIRPBL.107
      REAL      TSLDSB(MIXS,MLX),PISFC(MIX)                                      07NOV00.1752
C                                                                                HIRPBL.109
      REAL      U3D(MIX,MJX,MKX),V3D(MIX,MJX,MKX),T3D(MIX,MJX,MKX),              HIRPBL.110
     -    QV3D(MIX,MJX,MKX),PP3D(MIX,MJX,MKX),QC3D(MIXM,MJXM,MKXM),              HIRPBL.111
     -    QI3D(MIXIC,MJXIC,MKXIC),QNC3D(MIXIG,MJXIG,MKXIG),                      HIRPBL.112
     -    U3DTEN(MIX,MJX,MKX),V3DTEN(MIX,MJX,MKX),T3DTEN(MIX,MJX,MKX),           HIRPBL.113
     -    QV3DTEN(MIX,MJX,MKX),QC3DTEN(MIXM,MJXM,MKXM),                          HIRPBL.114
     -    QI3DTEN(MIXIC,MJXIC,MKXIC),QNC3DTEN(MIXIG,MJXIG,MKXIG)                 HIRPBL.115
C                                                                                HIRPBL.116
      INTEGER I,J,K,KK,K1,L,M                                                    HIRPBL.117
      INTEGER IN,IYY,JXX                                                         HIRPBL.118
      INTEGER KMAXD,JBGN,JEND,IBGN,IEND                                          HIRPBL.119
      INTEGER IPP,ICUT,MSTEPS,MITPAS,MITRUS,MITRU2                               HIRPBL.120
      REAL    PL,TVCON,E1,CKZ,ENTRMT,SZKM,C1,C2,CELL,DTSTEP,RDT                  HIRPBL.121
      REAL    TGBV,DTHVDZ,DTHVM,VCONV,SSKLM,RI,QMEAN,TMEAN,ALPH                  HIRPBL.122
      REAL    CHI,DTG,PSIX,ZL,DTTHX,WSPDX,USTM2,XKZ31,DTDZ                       05DEC01.886
      REAL    PSIX10,PSIT,PSIQ,PSIT2,PSIQ2,ZOL10,ZOL2                            05DEC01.887
      REAL    XMT1,XMT2,XMT3,DELTSM,PSUM,FRACTN,DTHVX,EPSUM                      HIRPBL.124
      REAL    HPBLM1,HFLXKL,UFLX,VFLX,QFLX,CIFLX,CFLX,CIFLXNC,DECAY              HIRPBL.125
      REAL    DECA,SS,BETAK,TGC,TSLC,XXT                                         HIRPBL.126
      REAL    MITER0,DTMITE,VISC,RESTAR                                          05DEC01.888
C                                                                                HIRPBL.128
      DATA CKZ/0.001/                                                            HIRPBL.129
      DATA ENTRMT/0.2/                                                           HIRPBL.130
      DATA KA/2.4E-5/                                                            HIRPBL.131
      DATA SZKM/1600./                                                           HIRPBL.132
      DATA C1/0.2721655/                                                         HIRPBL.133
      DATA C2/-0.33333/                                                          HIRPBL.134
                                                                                 05DEC01.889
      IPP=2                                                                      HIRPBL.135
      ICUT=0                                                                     HIRPBL.136
      IF(IN.GT.1)ICUT=1                                                          HIRPBL.137
      DTMITE=25.                                                                 07NOV00.1754
      MITER0=DTMIN*60./DTMITE+.99                                                07NOV00.1755
C                                                                                HIRPBL.138
C----------------------------------------------------------------------          HIRPBL.139
C-----DECOUPLE THE OLD MITER STEPS FROM THE LAND USE CATEGORY:                   HIRPBL.140
CCCC   THIS NO LONGER USED AS OF THIS VERSION                                    HIRPBL.141
CCCC  MITOLD=IFIX(SATBRT(1,J)/100.+.001)                                         HIRPBL.142
CCCC  SATOLD=SATBRT(1,J)-MITOLD*100.                                             HIRPBL.143
C                                                                                HIRPBL.144
C----CONVERT GROUND TEMPERATURE TO POTENTIAL TEMPERATURE:                        HIRPBL.145
C                                                                                HIRPBL.146
#ifdef MPP1                                                                      HIRPBL.147
CMPP1      FLIC_DO_M(I,1,ILX)                                                    HIRPBL.148
#endif                                                                           HIRPBL.149
      DO 5 I=1,ILX                                                               HIRPBL.150
        TGDSA(I)=TGB(I,J)                                                        HIRPBL.151
        PS(I)=PSB(I,J)+PTOP+PP3D(I,J,KL)*0.001                                   HIRPBL.152
        PISFC(I)=(PS(I)/100.)**ROVCP                                             07NOV00.1756
        THGB(I)=TGB(I,J)/PISFC(I)                                                07NOV00.1757
    5 CONTINUE                                                                   HIRPBL.155
C                                                                                HIRPBL.156
C-----DECOUPLE FLUX-FORM VARIABLES TO GIVE U,V,T,THETA,THETA-VIR.,               HIRPBL.157
C     T-VIR., QV, AND QC AT CROSS POINTS AND AT KTAU-1.                          HIRPBL.158
C                                                                                HIRPBL.159
   10 CONTINUE                                                                   HIRPBL.165
C                                                                                HIRPBL.166
   22 CONTINUE                                                                   HIRPBL.167
      DO 24 K=1,KL                                                               HIRPBL.168
        DO 24 I=1,ILX                                                            HIRPBL.169
          UX(I,K)=0.25*(U3D(I,J,K)+U3D(I+1,J,K)+U3D(I,J+1,K)+                    HIRPBL.170
     +            U3D(I+1,J+1,K))                                                HIRPBL.171
          VX(I,K)=0.25*(V3D(I,J,K)+V3D(I+1,J,K)+V3D(I,J+1,K)+                    HIRPBL.172
     +            V3D(I+1,J+1,K))                                                HIRPBL.173
   24   CONTINUE                                                                 HIRPBL.174
                                                                                 HIRPBL.175
   26 CONTINUE                                                                   HIRPBL.176
C                                                                                HIRPBL.177
C.....SCR3(I,K) STORE TEMPERATURE,                                               HIRPBL.178
C     SCR4(I,K) STORE VIRTUAL TEMPERATURE.                                       HIRPBL.179
C                                                                                HIRPBL.180
      DO 30 K=1,KL                                                               HIRPBL.181
        DO 30 I=1,ILX                                                            HIRPBL.182
          PL=A(K)*PSB(I,J)+PTOP+PP3D(I,J,K)*0.001                                HIRPBL.183
          SCR3(I,K)=T3D(I,J,K)                                                   HIRPBL.184
          THCON(I,K)=(100./PL)**ROVCP                                            HIRPBL.185
          THX(I,K)=SCR3(I,K)*THCON(I,K)                                          HIRPBL.186
          SCR4(I,K)=SCR3(I,K)                                                    HIRPBL.187
          THVX(I,K)=THX(I,K)                                                     HIRPBL.188
          QX(I,K)=0.                                                             HIRPBL.189
   30   CONTINUE                                                                 HIRPBL.190
                                                                                 HIRPBL.191
C                                                                                HIRPBL.192
      DO 40 I=1,ILX                                                              HIRPBL.193
        QGH(I)=0.                                                                HIRPBL.194
        HFKLMX(I)=200.                                                           HIRPBL.195
        FLHC(I)=0.                                                               HIRPBL.196
        FLQC(I)=0.                                                               HIRPBL.197
        CPM(I)=CP                                                                HIRPBL.198
   40 CONTINUE                                                                   HIRPBL.199
C                                                                                HIRPBL.200
      IF(IDRY(IN).EQ.1)GOTO 80                                                   HIRPBL.201
      DO 50 K=1,KL                                                               HIRPBL.202
        DO 50 I=1,ILX                                                            HIRPBL.203
          QX(I,K)=QV3D(I,J,K)                                                    HIRPBL.204
          TVCON=(1.+XMOIST(IN)*EP1*QX(I,K))                                      HIRPBL.205
          THVX(I,K)=THX(I,K)*TVCON                                               HIRPBL.206
          SCR4(I,K)=SCR3(I,K)*TVCON                                              HIRPBL.207
   50   CONTINUE                                                                 HIRPBL.208
                                                                                 HIRPBL.209
C                                                                                HIRPBL.210
      DO 60 I=1,ILX                                                              HIRPBL.211
        E1=SVP1*EXP(SVP2*(TGDSA(I)-SVPT0)/(TGDSA(I)-SVP3))                       HIRPBL.212
        QGH(I)=EP2*E1/(PS(I)-E1)                                                 HIRPBL.213
        CPM(I)=CP*(1.+0.8*QX(I,KL)*XMOIST(IN))                                   HIRPBL.214
   60 CONTINUE                                                                   HIRPBL.215
C                                                                                HIRPBL.216
      IF(IMOIST(IN).EQ.1.OR.IMOIST(IN).EQ.3)GOTO 80                              HIRPBL.217
      DO 70 K=1,KL                                                               HIRPBL.218
        DO 70 I=1,ILX                                                            HIRPBL.219
          QCX(I,K)=QC3D(I,J,K)                                                   HIRPBL.220
          QIX(I,K)=0.                                                            HIRPBL.221
          IF(IICE.EQ.1)QIX(I,K)=QI3D(I,J,K)                                      HIRPBL.222
          IF(IICEG.EQ.1)THEN                                                     HIRPBL.223
            QNCX(I,K)=QNC3D(I,J,K)                                               HIRPBL.224
          ENDIF                                                                  HIRPBL.225
   70   CONTINUE                                                                 HIRPBL.226
                                                                                 HIRPBL.227
   80 CONTINUE                                                                   HIRPBL.228
C                                                                                HIRPBL.229
C-----COMPUTE THE HEIGHT OF FULL- AND HALF-SIGMA LEVELS ABOVE GROUND             HIRPBL.230
C     LEVEL, AND THE LAYER THICKNESSES.                                          HIRPBL.231
C                                                                                HIRPBL.232
      DO 90 I=1,ILX                                                              HIRPBL.233
        ZQ(I,KLP1)=0.                                                            HIRPBL.234
        RHOX(I)=PS(I)*1000./(R*SCR4(I,KL))                                       HIRPBL.235
   90 CONTINUE                                                                   HIRPBL.236
C                                                                                HIRPBL.237
      DO 110 KK=1,KL                                                             HIRPBL.238
        K=KLP1-KK                                                                HIRPBL.239
C                                                                                HIRPBL.240
        DO 100 I=1,ILX                                                           HIRPBL.241
          DUM1(I)=ZQ(I,K+1)                                                      HIRPBL.242
  100   CONTINUE                                                                 HIRPBL.243
C                                                                                HIRPBL.244
        DO 110 I=1,ILX                                                           HIRPBL.245
          CELL=PTOP*RPSB(I,J)                                                    HIRPBL.246
          ZQ(I,K)=ROVG*T0(I,J,K)*ALOG((SIGMA(K+1)+CELL)/(SIGMA(K)+CELL))         HIRPBL.247
     +            +DUM1(I)                                                       HIRPBL.248
  110   CONTINUE                                                                 HIRPBL.249
                                                                                 HIRPBL.250
C                                                                                HIRPBL.251
      DO 120 K=1,KL                                                              HIRPBL.252
        DO 120 I=1,ILX                                                           HIRPBL.253
          ZA(I,K)=0.5*(ZQ(I,K)+ZQ(I,K+1))                                        HIRPBL.254
          DZQ(I,K)=ZQ(I,K)-ZQ(I,K+1)                                             HIRPBL.255
  120   CONTINUE                                                                 HIRPBL.256
                                                                                 HIRPBL.257
C                                                                                HIRPBL.258
      DO 130 K=1,KLM                                                             HIRPBL.259
        DO 130 I=1,ILX                                                           HIRPBL.260
          DZA(I,K)=ZA(I,K)-ZA(I,K+1)                                             HIRPBL.261
  130   CONTINUE                                                                 HIRPBL.262
                                                                                 HIRPBL.263
C                                                                                HIRPBL.264
C-----CALCULATE CRITICAL RICHARDSON NO. (SHIR AND BORNSTEIN,                     HIRPBL.265
C     BOUND. LAYER METEOR., 1977).                                               HIRPBL.266
C                                                                                HIRPBL.267
      DO 140 K=1,KL                                                              HIRPBL.268
        DO 140 I=1,ILX                                                           HIRPBL.269
          RC(I,K)=0.257*DZQ(I,K)**0.175                                          HIRPBL.270
  140   CONTINUE                                                                 HIRPBL.271
                                                                                 HIRPBL.272
C                                                                                HIRPBL.273
C-----SET UP THE MINOR LOOP INDEX:                                               HIRPBL.274
C                                                                                HIRPBL.275
C     THERE ARE TWO CRITERIONS TO DETERMINE WHETHER THE MINOR LOOP               HIRPBL.276
C     IS USED OR NOT:                                                            HIRPBL.277
C     1. WHEN THE PBL HEIGHT (PBL(I,J)) IS HIGHER THAN ZQ(KL)                    HIRPBL.278
C        OR LESS THAN 500 METERS (MOST LIKELY IN THE TRANSITION PERIOD           HIRPBL.279
C        FROM FORCED CONVECTION TO FREE CONVECTION).                             HIRPBL.280
C     2. THE SENSIBLE AND LATENT HEAT FLUXES ARE TOO LARGE.                      HIRPBL.281
C                                                                                HIRPBL.282
      MSTEPS=2                                                                   HIRPBL.283
      MITPAS=0                                                                   HIRPBL.284
C                                                                                HIRPBL.285
C.....FIND MITER STEPS FOR TRANSITION PERIOD:                                    HIRPBL.286
C                                                                                HIRPBL.287
      DO 150 I=1,ILX                                                             HIRPBL.288
        IF((PBL(I,J).GT.ZA(I,KL).AND.PBL(I,J).LT.500.))THEN                      HIRPBL.289
          MSTEPS=MITER0                                                          HIRPBL.290
        ENDIF                                                                    HIRPBL.291
  150 CONTINUE                                                                   HIRPBL.292
      MITRUS=2                                                                   HIRPBL.293
C                                                                                HIRPBL.294
  155 CONTINUE                                                                   HIRPBL.295
C                                                                                HIRPBL.296
      MSTEPS=MAX0(MSTEPS,MITRUS)                                                 HIRPBL.297
CCCC  MSTEPS=MAX0(MSTEPS,(MITOLD-1))                                             HIRPBL.298
#ifdef BIT_FOR_BIT_KLUDGE                                                        HIRPBL.299
C                                                                                HIRPBL.300
C NEVER SET THIS OPERATIONALLY.  STRICTLY FOR ASCERTAINING ABILITY TO            HIRPBL.301
C GIVE BIT-REPRODUCIBLE RESULTS BETWEEN MPP AND NON-MPP VERSIONS.                HIRPBL.302
C                                                                                HIRPBL.303
      MSTEPS=13                                                                  HIRPBL.304
#endif                                                                           HIRPBL.305
      DTSTEP=DT/FLOAT(MSTEPS)                                                    HIRPBL.306
      IF(DTSTEP.LT.0.001*DT)STOP 155                                             HIRPBL.307
      RDT=0.9/DTSTEP                                                             HIRPBL.308
C                                                                                HIRPBL.309
      DO 160 I=1,ILX                                                             HIRPBL.310
        GOVRTH(I)=G/THX(I,KL)                                                    HIRPBL.311
        KZMAX(I)=0.8*DZA(I,KLM)*DZQ(I,KL)/DTSTEP                                 HIRPBL.312
  160 CONTINUE                                                                   HIRPBL.313
C                                                                                HIRPBL.314
      DO 170 I=1,ILX                                                             HIRPBL.315
        TGDSB(I)=TGB(I,J)                                                        HIRPBL.316
        IF(ISOIL.EQ.1)THEN                                                       HIRPBL.317
          DO L=1,5                                                               HIRPBL.318
            TSLDSB(I,L)=TSLB(I,J,L)                                              HIRPBL.319
          ENDDO                                                                  HIRPBL.320
        ENDIF                                                                    HIRPBL.321
  170 CONTINUE                                                                   HIRPBL.322
C                                                                                HIRPBL.323
C-----SAVE THE INITIAL VALUE BEFORE MINOR LOOP IN THE SAVING ARRAYS.             HIRPBL.324
C                                                                                HIRPBL.325
      IF(MSTEPS.EQ.1)GOTO 210                                                    HIRPBL.326
      DO 180 K=1,KL                                                              HIRPBL.327
        DO 180 I=1,ILX                                                           HIRPBL.328
          THXS(I,K)=THX(I,K)                                                     HIRPBL.329
          UXS(I,K)=UX(I,K)                                                       HIRPBL.330
          VXS(I,K)=VX(I,K)                                                       HIRPBL.331
  180   CONTINUE                                                                 HIRPBL.332
                                                                                 HIRPBL.333
C                                                                                HIRPBL.334
      IF(IDRY(IN).EQ.1)GOTO 210                                                  HIRPBL.335
      DO 190 K=1,KL                                                              HIRPBL.336
        DO 190 I=1,ILX                                                           HIRPBL.337
          QXS(I,K)=QX(I,K)                                                       HIRPBL.338
  190   CONTINUE                                                                 HIRPBL.339
                                                                                 HIRPBL.340
C                                                                                HIRPBL.341
      IF(IMOIST(IN).EQ.1.OR.IMOIST(IN).EQ.3)GOTO 210                             HIRPBL.342
      DO 200 K=1,KL                                                              HIRPBL.343
        DO 200 I=1,ILX                                                           HIRPBL.344
          QCXS(I,K)=QCX(I,K)                                                     HIRPBL.345
          QIXS(I,K)=QIX(I,K)                                                     HIRPBL.346
          IF(IICEG.EQ.1)THEN                                                     HIRPBL.347
            QNCXS(I,K)=QNCX(I,K)                                                 HIRPBL.348
          ENDIF                                                                  HIRPBL.349
  200   CONTINUE                                                                 HIRPBL.350
                                                                                 HIRPBL.351
  210 CONTINUE                                                                   HIRPBL.352
C                                                                                HIRPBL.353
C----------------------------------------------------------------------          HIRPBL.354
C-----BEGINNING OF THE MINOR LOOP:                                               HIRPBL.355
C                                                                                HIRPBL.356
      DO 730 M=1,MSTEPS                                                          HIRPBL.357
C                                                                                HIRPBL.358
C-----IF MSTEPS HAS BEEN RECALCULATED FROM CURRENT TIME STEP VALUES              HIRPBL.359
C     OF UST, SKIP DOWN PAST THE UST-CALCULATION DURING FIRST MITER              HIRPBL.360
C     LOOP PASS (M=1).                                                           HIRPBL.361
C                                                                                HIRPBL.362
        IF(M.EQ.1.AND.MITPAS.GT.0)GOTO 335                                       HIRPBL.363
C                                                                                HIRPBL.364
C-----INITIALIZE VERTICAL TENDENCIES AND                                         HIRPBL.365
C                                                                                HIRPBL.366
        DO 220 K=1,KL                                                            HIRPBL.367
          DO 220 I=1,ILX                                                         HIRPBL.368
            UTNP(I,K)=0.                                                         HIRPBL.369
            VTNP(I,K)=0.                                                         HIRPBL.370
            TTNP(I,K)=0.                                                         HIRPBL.371
  220     CONTINUE                                                               HIRPBL.372
                                                                                 HIRPBL.373
C                                                                                HIRPBL.374
        IF(IDRY(IN).EQ.1)GOTO 250                                                HIRPBL.375
        DO 230 K=1,KL                                                            HIRPBL.376
          DO 230 I=1,ILX                                                         HIRPBL.377
            QTNP(I,K)=0.                                                         HIRPBL.378
  230     CONTINUE                                                               HIRPBL.379
                                                                                 HIRPBL.380
C                                                                                HIRPBL.381
        IF(IMOIST(IN).EQ.1.OR.IMOIST(IN).EQ.3)GOTO 250                           HIRPBL.382
        DO 240 K=1,KL                                                            HIRPBL.383
          DO 240 I=1,ILX                                                         HIRPBL.384
            QCTNP(I,K)=0.                                                        HIRPBL.385
            QITNP(I,K)=0.                                                        HIRPBL.386
            IF(IICEG.EQ.1)THEN                                                   HIRPBL.387
              QNCNP(I,K)=0.                                                      HIRPBL.388
            ENDIF                                                                HIRPBL.389
  240     CONTINUE                                                               HIRPBL.390
                                                                                 HIRPBL.391
  250   CONTINUE                                                                 HIRPBL.392
C                                                                                HIRPBL.393
C-----CALCULATE BULK RICHARDSON NO. OF SURFACE LAYER, ACCORDING TO               HIRPBL.394
C     AKB(1976), EQ(12).                                                         HIRPBL.395
C                                                                                HIRPBL.396
        DO 260 I=1,ILX                                                           HIRPBL.397
          GZ1OZ0(I)=ALOG(ZA(I,KL)/ZNT(I,J))                                      HIRPBL.398
          GZ10OZ0(I)=ALOG(10./ZNT(I,J))                                          05DEC01.890
          GZ2OZ0(I)=ALOG(2./ZNT(I,J))                                            05DEC01.891
        IF(IZ0TOPT.EQ.0)THEN                                                     05DEC01.892
C CARLSON-BOLAND FORMULA                                                         05DEC01.893
          IF((XLAND(I,J)-1.5).GE.0)THEN                                          05DEC01.894
            ZL=ZNT(I,J)                                                          05DEC01.895
          ELSE                                                                   05DEC01.896
            ZL=0.01                                                              05DEC01.897
          ENDIF                                                                  05DEC01.898
          GZ1OZ0Q(I)=ALOG(KARMAN*UST(I,J)*ZA(I,KL)/KA+ZA(I,KL)/ZL)               05DEC01.899
          GZ2OZ0Q(I)=ALOG(KARMAN*UST(I,J)*2./KA+2./ZL)                           05DEC01.900
        ENDIF                                                                    05DEC01.901
          WSPD(I)=SQRT(UX(I,KL)*UX(I,KL)+VX(I,KL)*VX(I,KL))                      HIRPBL.399
          TGBV=THGB(I)*(1.+XMOIST(IN)*EP1*QGH(I)*MAVAIL(I,J))                    07NOV00.1758
          DTHVDZ=(THVX(I,KL)-TGBV)                                               HIRPBL.401
          IF(-DTHVDZ.GE.0)THEN                                                   HIRPBL.402
            DTHVM=-DTHVDZ                                                        HIRPBL.403
          ELSE                                                                   HIRPBL.404
            DTHVM=0.                                                             HIRPBL.405
          ENDIF                                                                  HIRPBL.406
          VCONV=VCONVC*SQRT(DTHVM)                                               HIRPBL.407
          WSPD(I)=SQRT(WSPD(I)*WSPD(I)+VCONV*VCONV)                              HIRPBL.408
          WSPD(I)=AMAX1(WSPD(I),1.)                                              HIRPBL.409
          BR(I)=GOVRTH(I)*ZA(I,KL)*DTHVDZ/(WSPD(I)*WSPD(I))                      HIRPBL.410
  260   CONTINUE                                                                 HIRPBL.411
C                                                                                HIRPBL.412
C-----DIAGNOSE BASIC PARAMETERS FOR THE APPROPRIATED STABILITY CLASS:            HIRPBL.413
C                                                                                HIRPBL.414
C                                                                                HIRPBL.415
C     THE STABILITY CLASSES ARE DETERMINED BY BR (BULK RICHARDSON NO.)           HIRPBL.416
C     AND HOL (HEIGHT OF PBL/MONIN-OBUKHOV LENGTH).                              HIRPBL.417
C                                                                                HIRPBL.418
C     CRITERIA FOR THE CLASSES ARE AS FOLLOWS:                                   HIRPBL.419
C                                                                                HIRPBL.420
C        1. BR .GE. 0.2;                                                         HIRPBL.421
C               REPRESENTS NIGHTTIME STABLE CONDITIONS (REGIME=1),               HIRPBL.422
C                                                                                HIRPBL.423
C        2. BR .LT. 0.2 .AND. BR .GT. 0.0;                                       HIRPBL.424
C               REPRESENTS DAMPED MECHANICAL TURBULENT CONDITIONS                HIRPBL.425
C               (REGIME=2),                                                      HIRPBL.426
C                                                                                HIRPBL.427
C        3. BR .LT. 0.0 .AND. HOL .LT. 1.5;                                      HIRPBL.428
C               REPRESENTS FORCED CONVECTION CONDITIONS (REGIME=3),              HIRPBL.429
C                                                                                HIRPBL.430
C        4. BR .LT. 0.0 .AND. HOL .GT. 1.5;                                      HIRPBL.431
C               REPRESENTS FREE CONVECTION CONDITIONS (REGIME=4).                HIRPBL.432
C                                                                                HIRPBL.433
        DO 3201 I=1,ILX                                                          HIRPBL.434
C                                                                                HIRPBL.435
C-----CALCULATE SURFACE LAYER VERTICAL DIFFUSION COEFFICIENT FOR                 HIRPBL.436
C     CLASSES 1 - 3.                                                             HIRPBL.437
C     *** NOTE ***                                                               HIRPBL.438
C         THERMAL AND MOMENTUM DIFFUSION USE THE SAME DIFFUSION                  HIRPBL.439
C         COEFFICIENTS.                                                          HIRPBL.440
C                                                                                HIRPBL.441
          SSKLM=((UX(I,KLM)-UX(I,KL))*(UX(I,KLM)-UX(I,KL))+(VX(I,KLM)-           HIRPBL.442
     +          VX(I,KL))*(VX(I,KLM)-VX(I,KL)))/(DZA(I,KLM)*DZA(I,KLM))+         HIRPBL.443
     +          1.E-9                                                            HIRPBL.444
          RI=GOVRTH(I)*(THVX(I,KLM)-THVX(I,KL))/(DZA(I,KLM)*SSKLM)               HIRPBL.445
          IF(IMVDIF.EQ.1.AND.IMOIST(IN).EQ.2)THEN                                HIRPBL.446
C     CHANGED FROM K-1 AND K TO K IN NEXT LINE.                                  05DEC01.902
C     CLOUDY MIXING NOW EXTENDS ABOVE CLOUD.                                     05DEC01.903
              IF((QCX(I,KL)+QIX(I,KL)).GT.0.01E-3)THEN                           05DEC01.904
C      IN CLOUD                                                                  HIRPBL.449
              QMEAN=0.5*(QX(I,KL)+QX(I,KLM))                                     HIRPBL.450
              TMEAN=0.5*(SCR3(I,KL)+SCR3(I,KLM))                                 HIRPBL.451
              ALPH=XLV*QMEAN/R/TMEAN                                             HIRPBL.452
              CHI=XLV*XLV*QMEAN/CP/RV/TMEAN/TMEAN                                HIRPBL.453
              RI=(1.+ALPH)*(RI-G*G/SSKLM/TMEAN/CP*((CHI-ALPH)/(1.+CHI)))         HIRPBL.454
            ENDIF                                                                HIRPBL.455
          ENDIF                                                                  HIRPBL.456
          KZO=CKZ*DZA(I,KL-1)                                                    19DEC02.1696
          IF((RI.GT.RC(I,KL)))THEN                                               HIRPBL.458
            KZM(I,KL)=KZO                                                        HIRPBL.459
          ELSE                                                                   HIRPBL.460
            KZM(I,KL)=KZO+SQRT(SSKLM)*SZKM*(RC(I,KL)-RI)/RC(I,KL)                HIRPBL.461
          ENDIF                                                                  HIRPBL.462
          KZM(I,KL)=AMIN1(KZM(I,KL),KZMAX(I))                                    HIRPBL.463
CCCCCC                                                                           HIRPBL.464
 3201   CONTINUE                                                                 HIRPBL.465
CCCCCC                                                                           HIRPBL.466
        DO 320 I=1,ILX                                                           HIRPBL.467
CCCCCC                                                                           HIRPBL.468
          IF(BR(I).LT.0..AND.HOL(I,J).GT.1.5)GOTO 310                            HIRPBL.469
C                                                                                HIRPBL.470
C-----CLASS 1; STABLE (NIGHTTIME) CONDITIONS:                                    HIRPBL.471
C                                                                                HIRPBL.472
          IF(BR(I).LT.0.2)GOTO 270                                               HIRPBL.473
          REGIME(I,J)=1.                                                         HIRPBL.474
          PSIM(I)=-10.*GZ1OZ0(I)                                                 HIRPBL.475
C    LOWER LIMIT ON PSI IN STABLE CONDITIONS                                     HIRPBL.476
          PSIM(I)=AMAX1(PSIM(I),-10.)                                            HIRPBL.477
          PSIH(I)=PSIM(I)                                                        HIRPBL.478
        IF(ISFMTHD.EQ.0)THEN                                                     23DEC04.3426
          PSIM10(I)=-10.*GZ10OZ0(I)                                              23DEC04.3427
        ELSE                                                                     23DEC04.3428
          PSIM10(I)=10./ZA(I,KL)*PSIM(I)                                         23DEC04.3429
        ENDIF                                                                    23DEC04.3430
          PSIM10(I)=AMAX1(PSIM10(I),-10.)                                        19DEC02.1698
          PSIH10(I)=PSIM10(I)                                                    19DEC02.1699
        IF(ISFMTHD.EQ.0)THEN                                                     23DEC04.3431
          PSIM2(I)=-10.*GZ2OZ0(I)                                                23DEC04.3432
        ELSE                                                                     23DEC04.3433
          PSIM2(I)=2./ZA(I,KL)*PSIM(I)                                           23DEC04.3434
        ENDIF                                                                    23DEC04.3435
          PSIM2(I)=AMAX1(PSIM2(I),-10.)                                          19DEC02.1701
          PSIH2(I)=PSIM2(I)                                                      19DEC02.1702
          HOL(I,J)=0.0                                                           HIRPBL.479
          PBL(I,J)=0.0                                                           HIRPBL.480
          GOTO 320                                                               HIRPBL.481
C                                                                                HIRPBL.482
C-----CLASS 2; DAMPED MECHANICAL TURBULENCE:                                     HIRPBL.483
C                                                                                HIRPBL.484
  270     IF(BR(I).LT.0.0)GOTO 280                                               HIRPBL.485
          REGIME(I,J)=2.                                                         HIRPBL.486
          PSIM(I)=-5.0*BR(I)*GZ1OZ0(I)/(1.1-5.0*BR(I))                           HIRPBL.487
C.....AKB(1976), EQ(16).                                                         HIRPBL.488
C    LOWER LIMIT ON PSI IN STABLE CONDITIONS                                     HIRPBL.489
          PSIM(I)=AMAX1(PSIM(I),-10.)                                            HIRPBL.490
          PSIH(I)=PSIM(I)                                                        HIRPBL.491
        IF(ISFMTHD.EQ.0)THEN                                                     23DEC04.3436
          PSIM10(I)=-5.0*BR(I)*GZ10OZ0(I)/(1.1-5.0*BR(I))                        23DEC04.3437
        ELSE                                                                     23DEC04.3438
          PSIM10(I)=10./ZA(I,KL)*PSIM(I)                                         23DEC04.3439
        ENDIF                                                                    23DEC04.3440
          PSIM10(I)=AMAX1(PSIM10(I),-10.)                                        19DEC02.1704
          PSIH10(I)=PSIM10(I)                                                    19DEC02.1705
        IF(ISFMTHD.EQ.0)THEN                                                     23DEC04.3441
          PSIM2(I)=-5.0*BR(I)*GZ2OZ0(I)/(1.1-5.0*BR(I))                          23DEC04.3442
        ELSE                                                                     23DEC04.3443
          PSIM2(I)=2./ZA(I,KL)*PSIM(I)                                           23DEC04.3444
        ENDIF                                                                    23DEC04.3445
          PSIM2(I)=AMAX1(PSIM2(I),-10.)                                          19DEC02.1707
          PSIH2(I)=PSIM2(I)                                                      19DEC02.1708
          HOL(I,J)=0.0                                                           HIRPBL.492
          PBL(I,J)=0.0                                                           HIRPBL.493
          GOTO 320                                                               HIRPBL.494
C                                                                                HIRPBL.495
C-----CLASS 3; FORCED CONVECTION:                                                HIRPBL.496
C                                                                                HIRPBL.497
  280     REGIME(I,J)=3.                                                         HIRPBL.498
          PSIM(I)=0.0                                                            HIRPBL.499
          PSIH(I)=PSIM(I)                                                        19DEC02.1709
          PSIM10(I)=0.                                                           19DEC02.1710
          PSIH10(I)=PSIM10(I)                                                    19DEC02.1711
          PSIM2(I)=0.                                                            19DEC02.1712
          PSIH2(I)=PSIM2(I)                                                      19DEC02.1713
C                                                                                HIRPBL.501
          DO 290 KK=1,KLM                                                        HIRPBL.502
            K=KL-KK                                                              HIRPBL.503
            IF(THVX(I,K).GE.THVX(I,KL))GOTO 300                                  HIRPBL.504
  290     CONTINUE                                                               HIRPBL.505
          STOP 290                                                               HIRPBL.506
  300     PBL(I,J)=ZQ(I,K+1)                                                     HIRPBL.507
          KPBL3(I)=K                                                             HIRPBL.508
          ZOL(I,J)=BR(I)*GZ1OZ0(I)                                               HIRPBL.509
          HOL(I,J)=-ZOL(I,J)*PBL(I,J)/ZA(I,KL)                                   HIRPBL.510
          GOTO 320                                                               HIRPBL.511
C                                                                                HIRPBL.512
C-----CLASS 4; FREE CONVECTION:                                                  HIRPBL.513
C                                                                                HIRPBL.514
  310     IF(THVX(I,KLM).GT.THVX(I,KL))GOTO 280                                  HIRPBL.515
          REGIME(I,J)=4.                                                         HIRPBL.516
          ZOL(I,J)=BR(I)*GZ1OZ0(I)                                               HIRPBL.517
          ZOL(I,J)=AMAX1(ZOL(I,J),-2.0)                                          HIRPBL.518
C     PSIM(I)=0.0954-ZOL(I,J)*(1.86+ZOL(I,J)*(1.07+0.249*ZOL(I,J)))              HIRPBL.519
          PSIM(I)=-ZOL(I,J)*(1.86+ZOL(I,J)*(1.07+0.249*ZOL(I,J)))                HIRPBL.520
C     PSIH(I)=0.2010-ZOL(I,J)*(3.23+ZOL(I,J)*(1.99+0.474*ZOL(I,J)))              HIRPBL.521
          PSIH(I)=-ZOL(I,J)*(3.23+ZOL(I,J)*(1.99+0.474*ZOL(I,J)))                HIRPBL.522
C DIAGNOSTICS                                                                    05DEC01.905
          ZOL10=10./ZA(I,KL)*ZOL(I,J)                                            05DEC01.906
          PSIM10(I)=-ZOL10*(1.86+ZOL10*(1.07+0.249*ZOL10))                       05DEC01.907
          PSIH10(I)=-ZOL10*(3.23+ZOL10*(1.99+0.474*ZOL10))                       05DEC01.908
          ZOL2=2./ZA(I,KL)*ZOL(I,J)                                              05DEC01.909
          PSIM2(I)=-ZOL2*(1.86+ZOL2*(1.07+0.249*ZOL2))                           05DEC01.910
          PSIH2(I)=-ZOL2*(3.23+ZOL2*(1.99+0.474*ZOL2))                           05DEC01.911
                                                                                 05DEC01.912
  320   CONTINUE                                                                 HIRPBL.523
C                                                                                HIRPBL.524
C-----COMPUTE THE FRICTIONAL VELOCITY:                                           HIRPBL.525
C     ZA(1982) EQS(2.60),(2.61).                                                 HIRPBL.526
C                                                                                HIRPBL.527
        DO 330 I=1,ILX                                                           HIRPBL.528
          DTG=THX(I,KL)-THGB(I)                                                  07NOV00.1759
          PSIX=GZ1OZ0(I)-PSIM(I)                                                 HIRPBL.530
          PSIX10=GZ10OZ0(I)-PSIM10(I)                                            05DEC01.913
          UST(I,J)=KARMAN*WSPD(I)/PSIX                                           HIRPBL.531
          IF((XLAND(I,J)-1.5).LE.0.)THEN                                         05DEC01.914
C LAND: LOWER LIMIT ON USTAR                                                     05DEC01.915
            UST(I,J)=AMAX1(UST(I,J),0.1)                                         05DEC01.916
          ELSE                                                                   05DEC01.917
C WATER: ROUGHNESS LENGTH DEPENDS ON USTAR                                       05DEC01.918
            ZNT(I,J)=CZO*UST(I,J)*UST(I,J)/G+OZO                                 05DEC01.919
          ENDIF                                                                  05DEC01.920
        IF(IZ0TOPT.EQ.0)THEN                                                     05DEC01.921
C CARLSON-BOLAND FORMULA                                                         05DEC01.922
          PSIT=GZ1OZ0(I)-PSIH(I)                                                 05DEC01.923
          PSIQ=GZ1OZ0Q(I)-PSIH(I)                                                05DEC01.924
          PSIT2=GZ2OZ0(I)-PSIH2(I)                                               05DEC01.925
          PSIQ2=GZ2OZ0Q(I)-PSIH2(I)                                              05DEC01.926
        ELSE IF(IZ0TOPT.EQ.1)THEN                                                05DEC01.927
C GARRATT FORMULA                                                                05DEC01.928
          IF((XLAND(I,J)-1.5).GE.0)THEN                                          05DEC01.929
C WATER: CALCULATE ROUGHNESS REYNOLDS NUMBER                                     05DEC01.930
C        KINEMATIC VISCOSITY OF AIR (LINEAR APPROX TO                            05DEC01.931
C                 TEMP DEPENDENCE AT SEA LEVEL)                                  05DEC01.932
            VISC=(1.32+0.009*(SCR3(I,KL)-273.15))*1.E-5                          05DEC01.933
            RESTAR=UST(I,J)*ZNT(I,J)/VISC                                        05DEC01.934
            PSIT=GZ1OZ0(I)-PSIH(I)+2.48*SQRT(SQRT(RESTAR))-2.                    05DEC01.935
            PSIQ=GZ1OZ0(I)-PSIH(I)+2.28*SQRT(SQRT(RESTAR))-2.                    05DEC01.936
            PSIT2=GZ2OZ0(I)-PSIH2(I)+2.48*SQRT(SQRT(RESTAR))-2.                  05DEC01.937
            PSIQ2=GZ2OZ0(I)-PSIH2(I)+2.28*SQRT(SQRT(RESTAR))-2.                  05DEC01.938
          ELSE                                                                   05DEC01.939
C LAND                                                                           05DEC01.940
            PSIT=GZ1OZ0(I)-PSIH(I)+2.                                            05DEC01.941
            PSIQ=GZ1OZ0(I)-PSIH(I)+2.                                            05DEC01.942
            PSIT2=GZ2OZ0(I)-PSIH2(I)+2.                                          05DEC01.943
            PSIQ2=GZ2OZ0(I)-PSIH2(I)+2.                                          05DEC01.944
          ENDIF                                                                  05DEC01.945
        ELSE IF(IZ0TOPT.EQ.2)THEN                                                05DEC01.946
          IF((XLAND(I,J)-1.5).GE.0)THEN                                          05DEC01.947
C WATER (Z0T=Z0)                                                                 05DEC01.948
            PSIT=GZ1OZ0(I)-PSIH(I)                                               05DEC01.949
            PSIQ=GZ1OZ0(I)-PSIH(I)                                               05DEC01.950
            PSIT2=GZ2OZ0(I)-PSIH2(I)                                             05DEC01.951
            PSIQ2=GZ2OZ0(I)-PSIH2(I)                                             05DEC01.952
          ELSE                                                                   05DEC01.953
C LAND ZILITINKEVICH FORMULA                                                     05DEC01.954
C        KINEMATIC VISCOSITY OF AIR (LINEAR APPROX TO                            05DEC01.955
C                 TEMP DEPENDENCE AT SEA LEVEL)                                  05DEC01.956
            VISC=(1.32+0.009*(SCR3(I,KL)-273.15))*1.E-5                          05DEC01.957
            RESTAR=UST(I,J)*ZNT(I,J)/VISC                                        05DEC01.958
            PSIT=GZ1OZ0(I)-PSIH(I)+0.1*KARMAN*SQRT(RESTAR)                       05DEC01.959
            PSIQ=GZ1OZ0(I)-PSIH(I)+0.1*KARMAN*SQRT(RESTAR)                       05DEC01.960
            PSIT2=GZ2OZ0(I)-PSIH2(I)+0.1*KARMAN*SQRT(RESTAR)                     05DEC01.961
            PSIQ2=GZ2OZ0(I)-PSIH2(I)+0.1*KARMAN*SQRT(RESTAR)                     05DEC01.962
          ENDIF                                                                  05DEC01.963
        ENDIF                                                                    05DEC01.964
        U10(I)=UX(I,KL)*PSIX10/PSIX                                              05DEC01.965
        V10(I)=VX(I,KL)*PSIX10/PSIX                                              05DEC01.966
        TH2(I)=THGB(I)+DTG*PSIT2/PSIT                                            05DEC01.967
        Q2(I)=QGH(I)+(QX(I,KL)-QGH(I))*PSIQ2/PSIQ                                05DEC01.968
C       LATER Q2 WILL BE OVERWRITTEN FOR LAND POINTS IN SURFCE                   05DEC01.969
        TA2(I,J) = TH2(I)*(PS(I)/100.)**ROVCP                                    05DEC01.970
        QA2(I,J) = Q2(I)                                                         05DEC01.971
        UA10(I,J) = U10(I)                                                       05DEC01.972
        VA10(I,J) = V10(I)                                                       05DEC01.973
C                                                                                05DEC01.974
        MOL(I,J)=KARMAN*DTG/PSIT                                                 05DEC01.975
        DENOMQ(I)=PSIQ                                                           05DEC01.976
  330   CONTINUE                                                                 HIRPBL.539
C                                                                                HIRPBL.540
  335   CONTINUE                                                                 HIRPBL.541
C                                                                                HIRPBL.542
C-----COMPUTE RADIATION AND SOIL THERMAL CAPACITY:                               HIRPBL.543
C                                                                                HIRPBL.544
        IF(M.GT.1)GOTO 340                                                       HIRPBL.545
#ifndef MPP1                                                                     HIRPBL.546
        IF(MOD(KTAU,NTRAD(IN)).EQ.0)CALL SFCRAD(IYY,JXX,J,IN,T3D,QV3D,           HIRPBL.547
     +    PP3D)                                                                  HIRPBL.548
#else                                                                            HIRPBL.549
        IF(MOD(KTAU,NTRAD(IN)).EQ.0)CALL SFCRAD_COL(IYY,JXX,I,J,IN,T3D,          HIRPBL.550
     +    QV3D,PP3D)                                                             HIRPBL.551
#endif                                                                           HIRPBL.552
  340   CONTINUE                                                                 HIRPBL.553
C                                                                                HIRPBL.554
C-----COMPUTE THE SURFACE SENSIBLE AND LATENT HEAT FLUXES:                       HIRPBL.555
C                                                                                HIRPBL.556
        DO 350 I=1,ILX                                                           HIRPBL.557
          QFX(I,J)=0.                                                            HIRPBL.558
          HFX(I,J)=0.                                                            HIRPBL.559
  350   CONTINUE                                                                 HIRPBL.560
        IF(IDRY(IN).EQ.1)GOTO 354                                                HIRPBL.561
        IF(IMOIST(IN).EQ.1.OR.IMOIST(IN).EQ.3)GOTO 354                           HIRPBL.562
        DO 352 I=1,ILX                                                           HIRPBL.563
          CFLXP(I)=0.                                                            HIRPBL.564
  352   CONTINUE                                                                 HIRPBL.565
  354   CONTINUE                                                                 HIRPBL.566
C                                                                                HIRPBL.567
C                                                                                HIRPBL.568
        IF(ISFFLX(IN).EQ.0)GOTO 410                                              HIRPBL.569
C                                                                                HIRPBL.570
C-----OVER WATER, ALTER ROUGHNESS LENGTH (ZNT) ACCORDING TO WIND (UST).          HIRPBL.571
C                                                                                HIRPBL.572
        DO 360 I=1,ILX                                                           HIRPBL.573
          FLQC(I)=RHOX(I)*MAVAIL(I,J)*UST(I,J)*KARMAN/DENOMQ(I)                  05DEC01.977
          DTTHX=ABS(THX(I,KL)-THGB(I))                                           07NOV00.1760
          IF(DTTHX.GT.1.E-5)THEN                                                 HIRPBL.585
            FLHC(I)=CPM(I)*RHOX(I)*UST(I,J)*MOL(I,J)/(THX(I,KL)-THGB(I)          07NOV00.1761
     +              )                                                            HIRPBL.587
          ELSE                                                                   HIRPBL.588
            FLHC(I)=0.                                                           HIRPBL.589
          ENDIF                                                                  HIRPBL.590
C     FLHC(I)=CPM(I)*RHOX(I)*UST(I,J)*MOL(I,J)/(THX(I,KL)-THGB(I))               07NOV00.1762
  360   CONTINUE                                                                 HIRPBL.592
C                                                                                HIRPBL.593
C-----COMPUTE SURFACE MOIST FLUX:                                                HIRPBL.594
C                                                                                HIRPBL.595
        IF(IDRY(IN).EQ.1)GOTO 390                                                HIRPBL.596
C                                                                                HIRPBL.597
        DO 370 I=1,ILX                                                           HIRPBL.598
          QFX(I,J)=FLQC(I)*(QGH(I)-QX(I,KL))                                     HIRPBL.599
          QFX(I,J)=AMAX1(QFX(I,J),0.)                                            HIRPBL.600
          PARJSUM(ITQEVA_SUM,J)=PARJSUM(ITQEVA_SUM,J)+QFX(I,J)*DX*DX*0.5         HIRPBL.601
     +                          *DTSTEP                                          HIRPBL.602
C         TQEVA=TQEVA+QFX(I,J)*DX*DX*0.5*DTSTEP                                  HIRPBL.603
  370   CONTINUE                                                                 HIRPBL.604
C                                                                                HIRPBL.605
C-----COMPUTE SURFACE HEAT FLUX:                                                 HIRPBL.606
C                                                                                HIRPBL.607
  390   CONTINUE                                                                 HIRPBL.608
        DO 400 I=1,ILX                                                           HIRPBL.609
          IF(XLAND(I,J)-1.5.GT.0.)THEN                                           HIRPBL.610
            HFX(I,J)=FLHC(I)*(THGB(I)-THX(I,KL))                                 07NOV00.1763
            HFKLMX(I)=HFX(I,J)                                                   HIRPBL.612
          ELSEIF(XLAND(I,J)-1.5.LT.0.)THEN                                       HIRPBL.613
            HFX(I,J)=FLHC(I)*(THGB(I)-THX(I,KL))                                 07NOV00.1764
            HFX(I,J)=AMAX1(HFX(I,J),-250.)                                       HIRPBL.615
            HFKLMX(I)=AMAX1(200.,(HFX(I,J)*2.))                                  HIRPBL.616
          ENDIF                                                                  HIRPBL.617
  400   CONTINUE                                                                 HIRPBL.618
C                                                                                HIRPBL.619
C-----COMPUTE SURFACE MOMENTUM FLUXES:                                           HIRPBL.620
C                                                                                HIRPBL.621
  410   CONTINUE                                                                 HIRPBL.622
        DO 420 I=1,ILX                                                           HIRPBL.623
          WSPDX=SQRT(UX(I,KL)*UX(I,KL)+VX(I,KL)*VX(I,KL))+1.E-9                  HIRPBL.624
          UFLXP(I)=-UST(I,J)*UST(I,J)*UX(I,KL)/WSPDX                             HIRPBL.625
          VFLXP(I)=-UST(I,J)*UST(I,J)*VX(I,KL)/WSPDX                             HIRPBL.626
  420   CONTINUE                                                                 HIRPBL.627
C                                                                                HIRPBL.628
C-----DETERMINE THE MITER STEPS FOR THE SURFACE LAYER:                           HIRPBL.629
C                                                                                HIRPBL.630
        IF(M.LE.1.AND.MITPAS.LE.1)THEN                                           HIRPBL.631
          MITPAS=2                                                               HIRPBL.632
          USTM2=0.                                                               HIRPBL.633
          DO 435 I=1,ILX                                                         HIRPBL.634
            XKZ31=KZM(I,KL)/DZA(I,KLM)                                           HIRPBL.635
            DTDZ=DTSTEP/DZQ(I,KL)                                                HIRPBL.636
            XMT1=4.*(FLHC(I)/(RHOX(I)*CPM(I))+XKZ31)*DTDZ                        HIRPBL.637
            XMT2=4.*(FLQC(I)/RHOX(I)+XKZ31)*DTDZ                                 HIRPBL.638
            XMT3=4.*(UST(I,J)*UST(I,J)/WSPD(I)+XKZ31)*DTDZ                       HIRPBL.639
            USTM2=AMAX1(USTM2,XMT1,XMT2,XMT3)                                    HIRPBL.640
  435     CONTINUE                                                               HIRPBL.641
          MITRU2=IFIX(1.5*USTM2)+IPP                                             HIRPBL.642
          MITRUS=MAX0(MITRUS,MITRU2)                                             HIRPBL.643
#ifndef BIT_FOR_BIT_KLUDGE                                                       HIRPBL.644
          IF(MITRUS.GT.MSTEPS)GOTO 155                                           HIRPBL.645
#endif                                                                           HIRPBL.646
        ENDIF                                                                    HIRPBL.647
C                                                                                HIRPBL.648
C-----COMPUTE THE GROUND TEMPERATURE:                                            HIRPBL.649
C                                                                                HIRPBL.650
        IF(ITGFLG(IN).NE.1)GOTO 430                                              HIRPBL.651
        DELTSM=DTSTEP                                                            HIRPBL.652
#ifndef MPP1                                                                     HIRPBL.653
        CALL SLAB(DELTSM,J,IN,THX,QX,FLHC,FLQC,PP3D,T3D,1,ILX)                   07NOV00.1765
#else                                                                            HIRPBL.655
        CALL SLAB_COL(DELTSM,I,J,IN,THX,QX,FLHC,FLQC,PP3D,T3D)                   07NOV00.1766
#endif                                                                           HIRPBL.657
  430   CONTINUE                                                                 HIRPBL.658
        DO I=1,ILX                                                               07NOV00.1767
          THGB(I)=TGB(I,J)/PISFC(I)                                              07NOV00.1768
        ENDDO                                                                    07NOV00.1769
C                                                                                HIRPBL.659
C-----COMPUTE THE PBL PROGNOSTIC TERMS:                                          HIRPBL.660
C                                                                                HIRPBL.661
        DO 610 I=1,ILX                                                           HIRPBL.662
C                                                                                HIRPBL.663
          IF(REGIME(I,J).EQ.4.)GOTO 480                                          HIRPBL.664
C                                                                                HIRPBL.665
C-----EXPLICIT COMPUTATION OF SURFACE LAYER VALUES FOR CLASS 1,2,3.              HIRPBL.666
C                                                                                HIRPBL.667
          IF(BR(I).LT.0.)GOTO 450                                                HIRPBL.668
  440     TTNP(I,KL)=(HFX(I,J)/(RHOX(I)*CPM(I))-KZM(I,KL)*(THX(I,KL)-            HIRPBL.669
     +               THX(I,KLM))/DZA(I,KLM))/DZQ(I,KL)                           HIRPBL.670
          GOTO 460                                                               HIRPBL.671
  450     IF(THVX(I,KLM).GT.THVX(I,KL))GOTO 440                                  HIRPBL.672
C                                                                                HIRPBL.673
C.....PRIESTLY`S EQUATION:                                                       HIRPBL.674
C                                                                                HIRPBL.675
          HFLXKL=C1*SQRT(GOVRTH(I))*RHOX(I)*CPM(I)*((THVX(I,KL)-                 HIRPBL.676
     +           THVX(I,KLM))/(DZQ(I,KL)**C2-(2.0*ZA(I,KLM))**C2))**1.5          HIRPBL.677
          IF(XLAND(I,J)-1.5.LT.0.)THEN                                           HIRPBL.678
            HFLXKL=AMIN1(HFLXKL,HFKLMX(I))                                       HIRPBL.679
          ENDIF                                                                  HIRPBL.680
          TTNP(I,KL)=(HFX(I,J)-HFLXKL)/(RHOX(I)*CPM(I)*DZQ(I,KL))                HIRPBL.681
  460     THXSV(I)=THX(I,KL)+DTSTEP*TTNP(I,KL)                                   HIRPBL.682
C                                                                                HIRPBL.683
          UTNP(I,KL)=(UFLXP(I)-KZM(I,KL)*(UX(I,KL)-UX(I,KLM))/DZA(I,KLM)         HIRPBL.684
     +               )/DZQ(I,KL)                                                 HIRPBL.685
          VTNP(I,KL)=(VFLXP(I)-KZM(I,KL)*(VX(I,KL)-VX(I,KLM))/DZA(I,KLM)         HIRPBL.686
     +               )/DZQ(I,KL)                                                 HIRPBL.687
          UXSV(I)=UX(I,KL)+DTSTEP*UTNP(I,KL)                                     HIRPBL.688
          VXSV(I)=VX(I,KL)+DTSTEP*VTNP(I,KL)                                     HIRPBL.689
C                                                                                HIRPBL.690
          IF(IDRY(IN).EQ.1)GOTO 470                                              HIRPBL.691
          QTNP(I,KL)=(QFX(I,J)/RHOX(I)-KZM(I,KL)*(QX(I,KL)-QX(I,KLM))/           HIRPBL.692
     +               DZA(I,KLM))/DZQ(I,KL)                                       HIRPBL.693
          QXSV(I)=QX(I,KL)+DTSTEP*QTNP(I,KL)                                     HIRPBL.694
C                                                                                HIRPBL.695
          IF(IMOIST(IN).EQ.1.OR.IMOIST(IN).EQ.3)GOTO 470                         HIRPBL.696
          QCTNP(I,KL)=(CFLXP(I)-KZM(I,KL)*(QCX(I,KL)-QCX(I,KLM))/                HIRPBL.697
     +                DZA(I,KLM))/DZQ(I,KL)                                      HIRPBL.698
          QCXSV(I)=QCX(I,KL)+DTSTEP*QCTNP(I,KL)                                  HIRPBL.699
          QITNP(I,KL)=(CFLXP(I)-KZM(I,KL)*(QIX(I,KL)-QIX(I,KLM))/                HIRPBL.700
     +                DZA(I,KLM))/DZQ(I,KL)                                      HIRPBL.701
          QIXSV(I)=QIX(I,KL)+DTSTEP*QITNP(I,KL)                                  HIRPBL.702
          IF(IICEG.EQ.1)THEN                                                     HIRPBL.703
            QNCNP(I,KL)=(CFLXP(I)-KZM(I,KL)*(QNCX(I,KL)-QNCX(I,KLM))/            HIRPBL.704
     +                  DZA(I,KLM))/DZQ(I,KL)                                    HIRPBL.705
            QNCXSV(I)=QNCX(I,KL)+DTSTEP*QNCNP(I,KL)                              HIRPBL.706
          ENDIF                                                                  HIRPBL.707
C                                                                                HIRPBL.708
  470     KPBLM1(I)=KLM                                                          HIRPBL.709
          KPBL(I)=KL                                                             HIRPBL.710
          GOTO 610                                                               HIRPBL.711
C                                                                                HIRPBL.712
C-----PBL COMPUTATIONS FOR FREE CONVECTION ONLY, CLASS 4:                        HIRPBL.713
C                                                                                HIRPBL.714
  480     CONTINUE                                                               HIRPBL.715
C                                                                                HIRPBL.716
C.....PSUM IS POSITIVE ENERGY AREA IN PBL.                                       HIRPBL.717
C.....NSUM IS NEGATIVE ENERGY AREA AT TOP OF PBL, IN CAPPING INVERSION.          HIRPBL.718
C                                                                                HIRPBL.719
          PSUM=0.                                                                HIRPBL.720
          NSUM=0.                                                                HIRPBL.721
          FRACTN=1.                                                              HIRPBL.722
C                                                                                HIRPBL.723
          DO 530 KK=1,KLM                                                        HIRPBL.724
            K=KL-KK                                                              HIRPBL.725
            DTHVX=THVX(I,KL)-THVX(I,K)                                           HIRPBL.726
            IF(DTHVX.GE.0.)GOTO 520                                              HIRPBL.727
            EPSUM=ENTRMT*PSUM                                                    HIRPBL.728
            NPSUM=DTHVX*DZQ(I,K)                                                 HIRPBL.729
C                                                                                HIRPBL.730
C.....CHECK IF MORE NEGATIVE ENERGY PORTION IS NEEDED TO CONSERVE                HIRPBL.731
C     ENERGY AT TOP OF MIXED LAYER.                                              HIRPBL.732
C                                                                                HIRPBL.733
            IF(EPSUM+NPSUM+NSUM)500,510,490                                      HIRPBL.734
  490       NSUM=NSUM+NPSUM                                                      HIRPBL.735
            GOTO 530                                                             HIRPBL.736
  500       FRACTN=-(EPSUM+NSUM)/(NPSUM-1.E-9)                                   HIRPBL.737
  510       KPBL(I)=K                                                            HIRPBL.738
            PBL(I,J)=ZQ(I,K+1)+FRACTN*DZQ(I,K)                                   HIRPBL.739
            HPBLM1=ZQ(I,K)                                                       HIRPBL.740
            GOTO 540                                                             HIRPBL.741
  520       PSUM=PSUM+DTHVX*DZQ(I,K)                                             HIRPBL.742
  530     CONTINUE                                                               HIRPBL.743
C                                                                                HIRPBL.744
C.....CALCULATE ANALYTIC SOLUTION FOR SURFACE LAYER, CLASS 4:                    HIRPBL.745
C                                                                                HIRPBL.746
  540     IF(KPBL(I).LE.2)THEN                                                   HIRPBL.747
       PRINT *,'I J THETAV ',I,J,(THVX(I,K1),K1=1,KL)                            HIRPBL.748
       PRINT *,' TGB ',THGB(I)                                                   07NOV00.1770
            STOP 540                                                             HIRPBL.750
          ENDIF                                                                  HIRPBL.751
C.....SURFACE LAYER HEAT FLUX CONTAINS B1 FROM AKB(1979), EQ(10).                HIRPBL.752
          HFLXKL=C1*SQRT(GOVRTH(I))*RHOX(I)*CPM(I)*((THVX(I,KL)-                 HIRPBL.753
     +           THVX(I,KLM))/(DZQ(I,KL)**C2-(2.0*ZA(I,KLM))**C2))**1.5          HIRPBL.754
          IF(XLAND(I,J)-1.5.LT.0.)THEN                                           HIRPBL.755
            HFLXKL=AMIN1(HFLXKL,HFKLMX(I))                                       HIRPBL.756
          ENDIF                                                                  HIRPBL.757
          HOL(I,J)=PBL(I,J)*KARMAN*GOVRTH(I)*HFLXKL/(RHOX(I)*CPM(I)*             HIRPBL.758
     +             UST(I,J)*UST(I,J)*UST(I,J))                                   HIRPBL.759
          IF((RHOX(I)*CPM(I)*(1.-ENTRMT)*PSUM).NE.0)THEN                         HIRPBL.760
            MBAR=HFLXKL/(RHOX(I)*CPM(I)*(1.-ENTRMT)*PSUM)                        HIRPBL.761
          ELSE                                                                   HIRPBL.762
            MBAR=HFLXKL/1.0E-6                                                   HIRPBL.763
          ENDIF                                                                  HIRPBL.764
C     MBAR=HFLXKL/(RHOX(I)*CPM(I)*(1.-ENTRMT)*PSUM)                              HIRPBL.765
          IF(MBAR.GT.RDT)IRDTT=IRDTT+1                                           HIRPBL.766
C                                                                                HIRPBL.767
C.....FIND TENDENCIES IN PBL ABOVE SFC LAYER AND SUM OF FLUXES                   HIRPBL.768
C                                                                                HIRPBL.769
          UFLX=0.                                                                HIRPBL.770
          VFLX=0.                                                                HIRPBL.771
C                                                                                HIRPBL.772
C.....COMPUTE WGT, THE WEIGHTING FACTOR TO SIMULATE REDUCED MIXING               HIRPBL.773
C     RATES NEAR TOP OF PBL.                                                     HIRPBL.774
C                                                                                HIRPBL.775
          DO 550 K=KPBL(I),KLM                                                   HIRPBL.776
            WGT(K)=1.-ZA(I,K)/HPBLM1                                             HIRPBL.777
            IF((K.EQ.KPBL(I)))THEN                                               HIRPBL.778
              MBARK(K)=FRACTN*MBAR                                               HIRPBL.779
            ELSE                                                                 HIRPBL.780
              MBARK(K)=MBAR                                                      HIRPBL.781
            ENDIF                                                                HIRPBL.782
  550     CONTINUE                                                               HIRPBL.783
C                                                                                HIRPBL.784
          DO 560 K=KPBL(I),KLM                                                   HIRPBL.785
            UTNP(I,K)=MBARK(K)*WGT(K)*(UX(I,KL)-UX(I,K))*                        HIRPBL.786
     +                FLOAT(IVMIXM(IN))                                          HIRPBL.787
            VTNP(I,K)=MBARK(K)*WGT(K)*(VX(I,KL)-VX(I,K))*                        HIRPBL.788
     +                FLOAT(IVMIXM(IN))                                          HIRPBL.789
            TTNP(I,K)=MBARK(K)*(THVX(I,KL)-THVX(I,K))                            HIRPBL.790
            UFLX=UFLX+UTNP(I,K)*DZQ(I,K)                                         HIRPBL.791
            VFLX=VFLX+VTNP(I,K)*DZQ(I,K)                                         HIRPBL.792
  560     CONTINUE                                                               HIRPBL.793
C                                                                                HIRPBL.794
          IF(IDRY(IN).EQ.1)GOTO 590                                              HIRPBL.795
          QFLX=0.                                                                HIRPBL.796
          DO 570 K=KPBL(I),KLM                                                   HIRPBL.797
            QTNP(I,K)=MBARK(K)*(QX(I,KL)-QX(I,K))                                HIRPBL.798
            QFLX=QFLX+QTNP(I,K)*DZQ(I,K)                                         HIRPBL.799
  570     CONTINUE                                                               HIRPBL.800
          QFLX=QFLX*RHOX(I)                                                      HIRPBL.801
C                                                                                HIRPBL.802
          IF(IMOIST(IN).EQ.1.OR.IMOIST(IN).EQ.3)GOTO 590                         HIRPBL.803
          CIFLX=0.                                                               HIRPBL.804
          CFLX=0.                                                                HIRPBL.805
          CIFLXNC=0.                                                             HIRPBL.806
          DO 580 K=KPBL(I),KLM                                                   HIRPBL.807
            QCTNP(I,K)=MBARK(K)*(QCX(I,KL)-QCX(I,K))                             HIRPBL.808
            QITNP(I,K)=MBARK(K)*(QIX(I,KL)-QIX(I,K))                             HIRPBL.809
            CIFLX=CIFLX+QITNP(I,K)*DZQ(I,K)                                      HIRPBL.810
            CFLX=CFLX+QCTNP(I,K)*DZQ(I,K)                                        HIRPBL.811
            IF(IICEG.EQ.1)THEN                                                   HIRPBL.812
              QNCNP(I,K)=MBARK(K)*(QNCX(I,KL)-QNCX(I,K))                         HIRPBL.813
              CIFLXNC=CIFLXNC+QNCNP(I,K)*DZQ(I,K)                                HIRPBL.814
            ENDIF                                                                HIRPBL.815
  580     CONTINUE                                                               HIRPBL.816
          CIFLX=CIFLX*RHOX(I)                                                    HIRPBL.817
          CFLX=CFLX*RHOX(I)                                                      HIRPBL.818
          IF(IICEG.EQ.1)THEN                                                     HIRPBL.819
            CIFLXNC=CIFLXNC*RHOX(I)                                              HIRPBL.820
          ENDIF                                                                  HIRPBL.821
C                                                                                HIRPBL.822
  590     CONTINUE                                                               HIRPBL.823
          DECAY=MBAR*PBL(I,J)*DTSTEP/DZQ(I,KL)                                   HIRPBL.824
          IF(DECAY.EQ.0.)DECA=0.                                                 HIRPBL.825
          IF(DECAY.NE.0.)DECA=(EXP(-DECAY)-1.)/DECAY                             HIRPBL.826
C                                                                                HIRPBL.827
C.....SURFACE LAYER TENDENCIES:                                                  HIRPBL.828
C                                                                                HIRPBL.829
          TTNP(I,KL)=(HFX(I,J)/PBL(I,J)+(HFX(I,J)/PBL(I,J)+(HFLXKL-              HIRPBL.830
     +               HFX(I,J))/DZQ(I,KL))*DECA)/(RHOX(I)*CPM(I))                 HIRPBL.831
          UTNP(I,KL)=UFLXP(I)/PBL(I,J)+(UFLXP(I)/PBL(I,J)+(UFLX-UFLXP(I)         HIRPBL.832
     +               )/DZQ(I,KL))*DECA                                           HIRPBL.833
          VTNP(I,KL)=VFLXP(I)/PBL(I,J)+(VFLXP(I)/PBL(I,J)+(VFLX-VFLXP(I)         HIRPBL.834
     +               )/DZQ(I,KL))*DECA                                           HIRPBL.835
          THXSV(I)=THX(I,KPBL(I)+1)+DTSTEP*TTNP(I,KPBL(I)+1)                     HIRPBL.836
          UXSV(I)=UX(I,KPBL(I)+1)+DTSTEP*UTNP(I,KPBL(I)+1)                       HIRPBL.837
          VXSV(I)=VX(I,KPBL(I)+1)+DTSTEP*VTNP(I,KPBL(I)+1)                       HIRPBL.838
C                                                                                HIRPBL.839
          IF(IDRY(IN).EQ.1)GOTO 600                                              HIRPBL.840
          QTNP(I,KL)=(QFX(I,J)/PBL(I,J)+(QFX(I,J)/PBL(I,J)+(QFLX-                HIRPBL.841
     +               QFX(I,J))/DZQ(I,KL))*DECA)/RHOX(I)                          HIRPBL.842
          QXSV(I)=QX(I,KPBL(I)+1)+DTSTEP*QTNP(I,KPBL(I)+1)                       HIRPBL.843
C                                                                                HIRPBL.844
          IF(IMOIST(IN).EQ.1.OR.IMOIST(IN).EQ.3)GOTO 600                         HIRPBL.845
          QCTNP(I,KL)=CFLX*DECA/(DZQ(I,KL)*RHOX(I))                              HIRPBL.846
          QCXSV(I)=QCX(I,KPBL(I)+1)+DTSTEP*QCTNP(I,KPBL(I)+1)                    HIRPBL.847
          QITNP(I,KL)=CIFLX*DECA/(DZQ(I,KL)*RHOX(I))                             HIRPBL.848
          QIXSV(I)=QIX(I,KPBL(I)+1)+DTSTEP*QITNP(I,KPBL(I)+1)                    HIRPBL.849
          IF(IICEG.EQ.1)THEN                                                     HIRPBL.850
            QNCNP(I,KL)=CIFLXNC*DECA/(DZQ(I,KL)*RHOX(I))                         HIRPBL.851
            QNCXSV(I)=QNCX(I,KPBL(I)+1)+DTSTEP*QNCNP(I,KPBL(I)+1)                HIRPBL.852
          ENDIF                                                                  HIRPBL.853
C                                                                                HIRPBL.854
  600     KPBLM1(I)=KPBL(I)-1                                                    HIRPBL.855
          KZM(I,KPBL(I)+1)=0.                                                    HIRPBL.856
  610   CONTINUE                                                                 HIRPBL.857
C                                                                                HIRPBL.858
C-----IMPLICIT DIFFUSION:                                                        HIRPBL.859
C                                                                                HIRPBL.860
CC      DO 690 I=1,ILX                                                           HIRPBL.861
        KMAXD=0                                                                  HIRPBL.862
        DO I=1,ILX                                                               HIRPBL.863
          KMAXD=MAX0(KMAXD,KPBL(I))                                              HIRPBL.864
        ENDDO                                                                    HIRPBL.865
        KMAXD=MIN0(KMAXD,KL-1)                                                   HIRPBL.866
C                                                                                HIRPBL.867
        DO I=1,ILX                                                               HIRPBL.868
          FU(I,1)=UX(I,1)                                                        HIRPBL.869
          FV(I,1)=VX(I,1)                                                        HIRPBL.870
          FTHX(I,1)=THX(I,1)                                                     HIRPBL.871
          IF(IDRY(IN).NE.1)THEN                                                  HIRPBL.872
            FQ(I,1)=QX(I,1)                                                      HIRPBL.873
            IF(IMOIST(IN).NE.1)FQI(I,1)=QIX(I,1)                                 HIRPBL.874
            IF(IMOIST(IN).EQ.2)FQC(I,1)=QCX(I,1)                                 HIRPBL.875
            IF(IMOIST(IN).NE.1)FQNC(I,1)=QNCX(I,1)                               HIRPBL.876
          ENDIF                                                                  HIRPBL.877
          EM(I,1)=0.                                                             HIRPBL.878
        ENDDO                                                                    HIRPBL.879
C                                                                                HIRPBL.880
CC      DO 620 K=2,KPBLM1(I)                                                     HIRPBL.881
        DO 620 K=2,KMAXD                                                         HIRPBL.882
          DO 620 I=1,ILX                                                         HIRPBL.883
            IF(K.GT.KPBL(I))GOTO 620                                             HIRPBL.884
            SS=((UX(I,K-1)-UX(I,K))*(UX(I,K-1)-UX(I,K))+(VX(I,K-1)-              HIRPBL.885
     +         VX(I,K))*(VX(I,K-1)-VX(I,K)))/(DZA(I,K-1)*DZA(I,K-1))+            HIRPBL.886
     +         1.E-9                                                             HIRPBL.887
            RI=GOVRTH(I)*(THVX(I,K-1)-THVX(I,K))/(SS*DZA(I,K-1))                 HIRPBL.888
            IF(IMVDIF.EQ.1.AND.IMOIST(IN).EQ.2)THEN                              HIRPBL.889
C     CHANGED FROM .AND. TO .OR. IN NEXT LINE.                                   05DEC01.978
C     CLOUDY MIXING NOW EXTENDS ABOVE AND BELOW CLOUD.                           05DEC01.979
              IF((QCX(I,K)+QIX(I,K)).GT.0.01E-3)THEN                             05DEC01.980
C      IN CLOUD                                                                  HIRPBL.892
                QMEAN=0.5*(QX(I,K)+QX(I,K-1))                                    HIRPBL.893
                TMEAN=0.5*(SCR3(I,K)+SCR3(I,K-1))                                HIRPBL.894
                ALPH=XLV*QMEAN/R/TMEAN                                           HIRPBL.895
                CHI=XLV*XLV*QMEAN/CP/RV/TMEAN/TMEAN                              HIRPBL.896
                RI=(1.+ALPH)*(RI-G*G/SS/TMEAN/CP*((CHI-ALPH)/(1.+CHI)))          HIRPBL.897
              ENDIF                                                              HIRPBL.898
            ENDIF                                                                HIRPBL.899
            KZO=CKZ*DZA(I,K-1)                                                   19DEC02.1714
            IF(RI-RC(I,K).GE.0)THEN                                              HIRPBL.901
              KZM(I,K)=KZO                                                       HIRPBL.902
            ELSE                                                                 HIRPBL.903
              KZM(I,K)=KZO+SQRT(SS)*(RC(I,K)-RI)*SZKM/RC(I,K)                    HIRPBL.904
            ENDIF                                                                HIRPBL.905
            KZM(I,K)=AMIN1(KZM(I,K),100.)                                        HIRPBL.906
  620     CONTINUE                                                               HIRPBL.907
                                                                                 HIRPBL.908
C                                                                                HIRPBL.909
CC      DO 630 K=2,KPBLM1(I)                                                     HIRPBL.910
        DO 630 K=2,KMAXD                                                         HIRPBL.911
          DO 630 I=1,ILX                                                         HIRPBL.912
            IF(K.GT.KPBL(I))GOTO 630                                             HIRPBL.913
            ALFA(I,K)=KZM(I,K)*DTSTEP/(DZQ(I,K)*DZA(I,K-1))                      HIRPBL.914
            BETAK=KZM(I,K+1)*DTSTEP/(DZQ(I,K)*DZA(I,K))                          HIRPBL.915
            BM(I,K)=1.+ALFA(I,K)+BETAK-ALFA(I,K)*EM(I,K-1)                       HIRPBL.916
            EM(I,K)=BETAK/BM(I,K)                                                HIRPBL.917
            FU(I,K)=(UX(I,K)+ALFA(I,K)*FU(I,K-1))/BM(I,K)                        HIRPBL.918
            FV(I,K)=(VX(I,K)+ALFA(I,K)*FV(I,K-1))/BM(I,K)                        HIRPBL.919
            FTHX(I,K)=(THX(I,K)+ALFA(I,K)*FTHX(I,K-1))/BM(I,K)                   HIRPBL.920
  630     CONTINUE                                                               HIRPBL.921
                                                                                 HIRPBL.922
C                                                                                HIRPBL.923
C.....APPLY VERTICAL DIFFUSION TO OBTAIN TENDENCIES ABOVE PBL:                   HIRPBL.924
C     THIS CALCULATION NOW INCLUDES K = KPBL                                     HIRPBL.925
C                                                                                HIRPBL.926
CC      DO 640 K=KPBLM1(I),2,-1                                                  HIRPBL.927
        DO 640 K=KMAXD,2,-1                                                      HIRPBL.928
          DO 640 I=1,ILX                                                         HIRPBL.929
            IF(K.GT.KPBL(I))GOTO 640                                             HIRPBL.930
            UXSV(I)=EM(I,K)*UXSV(I)+FU(I,K)                                      HIRPBL.931
            VXSV(I)=EM(I,K)*VXSV(I)+FV(I,K)                                      HIRPBL.932
            THXSV(I)=EM(I,K)*THXSV(I)+FTHX(I,K)                                  HIRPBL.933
            UTNP(I,K)=UTNP(I,K)+(UXSV(I)-UX(I,K))/DTSTEP                         HIRPBL.934
            VTNP(I,K)=VTNP(I,K)+(VXSV(I)-VX(I,K))/DTSTEP                         HIRPBL.935
            TTNP(I,K)=TTNP(I,K)+(THXSV(I)-THX(I,K))/DTSTEP                       HIRPBL.936
  640     CONTINUE                                                               HIRPBL.937
                                                                                 HIRPBL.938
C                                                                                HIRPBL.939
        IF(IDRY(IN).EQ.1)GOTO 690                                                HIRPBL.940
CC      DO 650 K=2,KPBLM1(I)                                                     HIRPBL.941
        DO 650 K=2,KMAXD                                                         HIRPBL.942
          DO 650 I=1,ILX                                                         HIRPBL.943
            IF(K.GT.KPBL(I))GOTO 650                                             HIRPBL.944
            FQ(I,K)=(QX(I,K)+ALFA(I,K)*FQ(I,K-1))/BM(I,K)                        HIRPBL.945
  650     CONTINUE                                                               HIRPBL.946
                                                                                 HIRPBL.947
CC      DO 660 K=KPBLM1(I),2,-1                                                  HIRPBL.948
        DO 660 K=KMAXD,2,-1                                                      HIRPBL.949
          DO 660 I=1,ILX                                                         HIRPBL.950
            IF(K.GT.KPBL(I))GOTO 660                                             HIRPBL.951
            QXSV(I)=EM(I,K)*QXSV(I)+FQ(I,K)                                      HIRPBL.952
            QTNP(I,K)=QTNP(I,K)+(QXSV(I)-QX(I,K))/DTSTEP                         HIRPBL.953
  660     CONTINUE                                                               HIRPBL.954
                                                                                 HIRPBL.955
C                                                                                HIRPBL.956
        IF(IMOIST(IN).EQ.1.OR.IMOIST(IN).EQ.3)GOTO 690                           HIRPBL.957
CC      DO 670 K=2,KPBLM1(I)                                                     HIRPBL.958
        DO 670 K=2,KMAXD                                                         HIRPBL.959
          DO 670 I=1,ILX                                                         HIRPBL.960
            IF(K.GT.KPBL(I))GOTO 670                                             HIRPBL.961
            FQC(I,K)=(QCX(I,K)+ALFA(I,K)*FQC(I,K-1))/BM(I,K)                     HIRPBL.962
            FQI(I,K)=(QIX(I,K)+ALFA(I,K)*FQI(I,K-1))/BM(I,K)                     HIRPBL.963
            IF(IICEG.EQ.1)THEN                                                   HIRPBL.964
              FQNC(I,K)=(QNCX(I,K)+ALFA(I,K)*FQNC(I,K-1))/BM(I,K)                HIRPBL.965
            ENDIF                                                                HIRPBL.966
  670     CONTINUE                                                               HIRPBL.967
                                                                                 HIRPBL.968
CC      DO 680 K=KPBLM1(I),2,-1                                                  HIRPBL.969
        DO 680 K=KMAXD,2,-1                                                      HIRPBL.970
          DO 680 I=1,ILX                                                         HIRPBL.971
            IF(K.GT.KPBL(I))GOTO 680                                             HIRPBL.972
            QIXSV(I)=EM(I,K)*QIXSV(I)+FQI(I,K)                                   HIRPBL.973
            QCXSV(I)=EM(I,K)*QCXSV(I)+FQC(I,K)                                   HIRPBL.974
            QCTNP(I,K)=QCTNP(I,K)+(QCXSV(I)-QCX(I,K))/DTSTEP                     HIRPBL.975
            QITNP(I,K)=QITNP(I,K)+(QIXSV(I)-QIX(I,K))/DTSTEP                     HIRPBL.976
            IF(IICEG.EQ.1)THEN                                                   HIRPBL.977
              QNCXSV(I)=EM(I,K)*QNCXSV(I)+FQNC(I,K)                              HIRPBL.978
              QNCNP(I,K)=QNCNP(I,K)+(QNCXSV(I)-QNCX(I,K))/DTSTEP                 HIRPBL.979
            ENDIF                                                                HIRPBL.980
  680     CONTINUE                                                               HIRPBL.981
                                                                                 HIRPBL.982
  690   CONTINUE                                                                 HIRPBL.983
C                                                                                HIRPBL.984
        IF(MSTEPS.EQ.1)GOTO 770                                                  HIRPBL.985
        DO 700 K=1,KL                                                            HIRPBL.986
          DO 700 I=1,ILX                                                         HIRPBL.987
            UX(I,K)=UX(I,K)+DTSTEP*UTNP(I,K)                                     HIRPBL.988
            VX(I,K)=VX(I,K)+DTSTEP*VTNP(I,K)                                     HIRPBL.989
            THX(I,K)=THX(I,K)+DTSTEP*TTNP(I,K)                                   HIRPBL.990
            THVX(I,K)=THX(I,K)                                                   HIRPBL.991
  700     CONTINUE                                                               HIRPBL.992
                                                                                 HIRPBL.993
C                                                                                HIRPBL.994
        IF(IDRY(IN).EQ.1)GOTO 730                                                HIRPBL.995
        DO 710 K=1,KL                                                            HIRPBL.996
          DO 710 I=1,ILX                                                         HIRPBL.997
            QX(I,K)=QX(I,K)+DTSTEP*QTNP(I,K)                                     HIRPBL.998
            TVCON=(1.+XMOIST(IN)*EP1*QX(I,K))                                    HIRPBL.999
            THVX(I,K)=THX(I,K)*TVCON                                             HIRPBL.1000
  710     CONTINUE                                                               HIRPBL.1001
                                                                                 HIRPBL.1002
        DO 715 I=1,ILX                                                           HIRPBL.1003
          CPM(I)=CP*(1.+0.8*QX(I,KL)*XMOIST(IN))                                 HIRPBL.1004
  715   CONTINUE                                                                 HIRPBL.1005
C                                                                                HIRPBL.1006
        IF(IMOIST(IN).EQ.1.OR.IMOIST(IN).EQ.3)GOTO 730                           HIRPBL.1007
        DO 720 K=1,KL                                                            HIRPBL.1008
          DO 720 I=1,ILX                                                         HIRPBL.1009
            QCX(I,K)=QCX(I,K)+DTSTEP*QCTNP(I,K)                                  HIRPBL.1010
            QIX(I,K)=QIX(I,K)+DTSTEP*QITNP(I,K)                                  HIRPBL.1011
            IF(IICEG.EQ.1)THEN                                                   HIRPBL.1012
              QNCX(I,K)=QNCX(I,K)+DTSTEP*QNCNP(I,K)                              HIRPBL.1013
            ENDIF                                                                HIRPBL.1014
  720     CONTINUE                                                               HIRPBL.1015
                                                                                 HIRPBL.1016
  730 CONTINUE                                                                   HIRPBL.1017
C                                                                                HIRPBL.1018
C----------------------------------------------------------------------          HIRPBL.1019
C-----END OF MINOR LOOP.                                                         HIRPBL.1020
C                                                                                HIRPBL.1021
C-----COMPUTE THE TENDENCIES FROM THE SAVED INITIAL VALUES:                      HIRPBL.1022
C                                                                                HIRPBL.1023
      DO 740 K=1,KL                                                              HIRPBL.1024
        DO 740 I=1,ILX                                                           HIRPBL.1025
          UTNP(I,K)=(UX(I,K)-UXS(I,K))/DT                                        HIRPBL.1026
          VTNP(I,K)=(VX(I,K)-VXS(I,K))/DT                                        HIRPBL.1027
          TTNP(I,K)=(THX(I,K)-THXS(I,K))/DT                                      HIRPBL.1028
  740   CONTINUE                                                                 HIRPBL.1029
                                                                                 HIRPBL.1030
C                                                                                HIRPBL.1031
      IF(IDRY(IN).EQ.1)GOTO 770                                                  HIRPBL.1032
      DO 750 K=1,KL                                                              HIRPBL.1033
        DO 750 I=1,ILX                                                           HIRPBL.1034
          QTNP(I,K)=(QX(I,K)-QXS(I,K))/DT                                        HIRPBL.1035
  750   CONTINUE                                                                 HIRPBL.1036
                                                                                 HIRPBL.1037
C                                                                                HIRPBL.1038
      IF(IMOIST(IN).EQ.1.OR.IMOIST(IN).EQ.3)GOTO 770                             HIRPBL.1039
      DO 760 K=1,KL                                                              HIRPBL.1040
        DO 760 I=1,ILX                                                           HIRPBL.1041
          QCTNP(I,K)=(QCX(I,K)-QCXS(I,K))/DT                                     HIRPBL.1042
          QITNP(I,K)=(QIX(I,K)-QIXS(I,K))/DT                                     HIRPBL.1043
          IF(IICEG.EQ.1)THEN                                                     HIRPBL.1044
            QNCNP(I,K)=(QNCX(I,K)-QNCXS(I,K))/DT                                 HIRPBL.1045
          ENDIF                                                                  HIRPBL.1046
  760   CONTINUE                                                                 HIRPBL.1047
                                                                                 HIRPBL.1048
  770 CONTINUE                                                                   HIRPBL.1049
C                                                                                HIRPBL.1050
C-----STORE OLD MITER STEPS IN SATBRT(1,J) ARRAY BY MULTIPLYING OLD              HIRPBL.1051
C     MITER STEPS BY 100. AND ADDING TO THE LAND USE CATEGORY:                   HIRPBL.1052
C                                                                                HIRPBL.1053
CCCC  SATBRT(1,J)=MSTEPS*100.+SATOLD                                             HIRPBL.1054
C                                                                                HIRPBL.1055
C-----CONVERT ALL THE TENDENCIES TO FLUX FORM:                                   HIRPBL.1056
C                                                                                HIRPBL.1057
      DO 780 K=1,KL                                                              HIRPBL.1058
        DO 780 I=1,ILX                                                           HIRPBL.1059
          UTNP(I,K)=PSB(I,J)*UTNP(I,K)                                           HIRPBL.1060
          VTNP(I,K)=PSB(I,J)*VTNP(I,K)                                           HIRPBL.1061
          TTNP(I,K)=PSB(I,J)*TTNP(I,K)                                           HIRPBL.1062
  780   CONTINUE                                                                 HIRPBL.1063
                                                                                 HIRPBL.1064
C                                                                                HIRPBL.1065
      IF(IDRY(IN).EQ.1)GOTO 810                                                  HIRPBL.1066
      DO 790 K=1,KL                                                              HIRPBL.1067
        DO 790 I=1,ILX                                                           HIRPBL.1068
          QTNP(I,K)=PSB(I,J)*QTNP(I,K)                                           HIRPBL.1069
  790   CONTINUE                                                                 HIRPBL.1070
                                                                                 HIRPBL.1071
C                                                                                HIRPBL.1072
      IF(IMOIST(IN).EQ.1.OR.IMOIST(IN).EQ.3)GOTO 810                             HIRPBL.1073
      DO 800 K=1,KL                                                              HIRPBL.1074
        DO 800 I=1,ILX                                                           HIRPBL.1075
          QCTNP(I,K)=PSB(I,J)*QCTNP(I,K)                                         HIRPBL.1076
          QITNP(I,K)=PSB(I,J)*QITNP(I,K)                                         HIRPBL.1077
          IF(IICEG.EQ.1)THEN                                                     HIRPBL.1078
            QNCNP(I,K)=PSB(I,J)*QNCNP(I,K)                                       HIRPBL.1079
          ENDIF                                                                  HIRPBL.1080
  800   CONTINUE                                                                 HIRPBL.1081
                                                                                 HIRPBL.1082
  810 CONTINUE                                                                   HIRPBL.1083
C                                                                                HIRPBL.1084
C-----CALCULATION OF NEW VALUES DUE TO VERTICAL EXCHANGE PROCESSES IS            HIRPBL.1085
C     COMPLETED. THE FINAL STEP IS TO ADD THE TENDENCIES CALCULATED              HIRPBL.1086
C     IN HIRPBL TO THOSE OF MM4.                                                 HIRPBL.1087
C                                                                                HIRPBL.1088
C                                                                                HIRPBL.1089
      DO 820 K=1,KL                                                              HIRPBL.1090
        DO 820 I=1,ILX                                                           HIRPBL.1091
          U3DTEN(I,J,K)=UTNP(I,K)                                                HIRPBL.1092
          V3DTEN(I,J,K)=VTNP(I,K)                                                HIRPBL.1093
  820   CONTINUE                                                                 HIRPBL.1094
                                                                                 HIRPBL.1095
C                                                                                HIRPBL.1096
  825 CONTINUE                                                                   HIRPBL.1097
      JBGN=3                                                                     HIRPBL.1098
      JEND=JLXM-1                                                                HIRPBL.1099
      IF(IN.EQ.1)THEN                                                            HIRPBL.1100
        JBGN=2                                                                   HIRPBL.1101
        JEND=JLXM                                                                HIRPBL.1102
      ENDIF                                                                      HIRPBL.1103
      IF(J.LT.JBGN.OR.J.GT.JEND)GOTO 860                                         HIRPBL.1104
      IBGN=3                                                                     HIRPBL.1105
      IEND=ILXM-1                                                                HIRPBL.1106
      IF(IN.EQ.1)THEN                                                            HIRPBL.1107
        IBGN=2                                                                   HIRPBL.1108
        IEND=ILXM                                                                HIRPBL.1109
      ENDIF                                                                      HIRPBL.1110
#ifdef MPP1                                                                      HIRPBL.1111
CMPP1      IF(FLIC_L2G_M(I).GE.IBGN.AND.FLIC_L2G_M(I).LE.IEND)THEN               HIRPBL.1112
#endif                                                                           HIRPBL.1113
      DO 830 K=1,KL                                                              HIRPBL.1114
        DO 830 I=IBGN,IEND                                                       HIRPBL.1115
          T3DTEN(I,J,K)=T3DTEN(I,J,K)+TTNP(I,K)/THCON(I,K)                       HIRPBL.1116
  830   CONTINUE                                                                 HIRPBL.1117
                                                                                 HIRPBL.1118
C                                                                                HIRPBL.1119
      IF(IDRY(IN).EQ.1)GOTO 860                                                  HIRPBL.1120
      DO 840 K=1,KL                                                              HIRPBL.1121
        DO 840 I=IBGN,IEND                                                       HIRPBL.1122
          QV3DTEN(I,J,K)=QV3DTEN(I,J,K)+QTNP(I,K)                                HIRPBL.1123
  840   CONTINUE                                                                 HIRPBL.1124
                                                                                 HIRPBL.1125
C                                                                                HIRPBL.1126
      IF(IMOIST(IN).EQ.1.OR.IMOIST(IN).EQ.3)GOTO 860                             HIRPBL.1127
      DO 850 K=1,KL                                                              HIRPBL.1128
        DO 850 I=IBGN,IEND                                                       HIRPBL.1129
          QC3DTEN(I,J,K)=QC3DTEN(I,J,K)+QCTNP(I,K)                               HIRPBL.1130
          IF(IICE.EQ.1)QI3DTEN(I,J,K)=QI3DTEN(I,J,K)+QITNP(I,K)                  HIRPBL.1131
          IF(IICEG.EQ.1)QNC3DTEN(I,J,K)=QNC3DTEN(I,J,K)+QNCNP(I,K)               HIRPBL.1132
  850   CONTINUE                                                                 HIRPBL.1133
                                                                                 HIRPBL.1134
#ifdef MPP1                                                                      HIRPBL.1135
CMPP1      ENDIF                                                                 HIRPBL.1136
#endif                                                                           HIRPBL.1137
  860 CONTINUE                                                                   HIRPBL.1138
C                                                                                HIRPBL.1139
C-----APPLY ASSELIN FILTER TO TGD FOR LARGE TIME STEP:                           HIRPBL.1140
C                                                                                HIRPBL.1141
      IF(ITGFLG(IN).NE.1)GOTO 880                                                HIRPBL.1142
      DO 870 I=1,ILX                                                             HIRPBL.1143
        TGC=TGB(I,J)                                                             HIRPBL.1144
        TGB(I,J)=OMUHF*TGA(I,J)+GNUHF*(TGC+TGDSB(I))                             HIRPBL.1145
        TGA(I,J)=TGC                                                             HIRPBL.1146
        IF(ISOIL.EQ.1)THEN                                                       HIRPBL.1147
          DO L=1,5                                                               HIRPBL.1148
            TSLC=TSLB(I,J,L)                                                     HIRPBL.1149
            TSLB(I,J,L)=OMUHF*TSLA(I,J,L)+GNUHF*(TSLC+TSLDSB(I,L))               HIRPBL.1150
            TSLA(I,J,L)=TSLC                                                     HIRPBL.1151
          ENDDO                                                                  HIRPBL.1152
        ENDIF                                                                    HIRPBL.1153
  870 CONTINUE                                                                   HIRPBL.1154
  880 CONTINUE                                                                   HIRPBL.1155
C                                                                                HIRPBL.1160
C                                                                                HIRPBL.1161
#ifndef MPP1                                                                     HIRPBL.1162
      IF(IN.EQ.1)THEN                                                            HIRPBL.1163
        XXT=XTIME+DTMIN                                                          HIRPBL.1164
      ELSE                                                                       HIRPBL.1165
        XXT=XTIME+FLOAT(IRAX)*DTMIN                                              HIRPBL.1166
      ENDIF                                                                      HIRPBL.1167
#endif                                                                           HIRPBL.1168
C                                                                                HIRPBL.1169
  940 CONTINUE                                                                   HIRPBL.1170
C                                                                                HIRPBL.1171
C KPBL IS NEEDED FOR THE FDDA, AND SINCE THERE IS NO LONGER JUST ONE             HIRPBL.1172
C LARGE "J LOOP" IT MUST BE STORED AS (I,J)...                                   HIRPBL.1173
C                                                                                HIRPBL.1174
      DO 950 I=1,ILX                                                             HIRPBL.1175
        KPBL2D(I,J)=KPBL(I)                                                      HIRPBL.1176
        IF(REGIME(I,J).EQ.3.)KPBL2D(I,J)=KPBL3(I)                                HIRPBL.1177
  950 CONTINUE                                                                   HIRPBL.1178
#ifdef MPP1                                                                      HIRPBL.1179
CMPP1      FLIC_ENDDO                                                            HIRPBL.1180
      IF(IN.EQ.1)THEN                                                            HIRPBL.1181
        XXT=XTIME+DTMIN                                                          HIRPBL.1182
      ELSE                                                                       HIRPBL.1183
        XXT=XTIME+FLOAT(IRAX)*DTMIN                                              HIRPBL.1184
      ENDIF                                                                      HIRPBL.1185
#endif                                                                           HIRPBL.1186
      RETURN                                                                     HIRPBL.1187
      END                                                                        HIRPBL.1188
                                                                                 HIRPBL.1189
