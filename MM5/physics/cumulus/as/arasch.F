            SUBROUTINE ARASCH(TI,QI,Z1,TIO,QIO,PIO,                              ARASCH.1
     *             KLEV,PRE,PI,OUTTEM,OUTQ,DTIME,KBMAX,PCUT,C0,                  ARASCH.2
     *             FTEST,PSUR,PSURO,IERRT,KDET,VSP)                              ARASCH.3
C                                                                                ARASCH.4
C                                                                                ARASCH.5
C                                                                                ARASCH.6
#     include <parame.incl>                                                      ARASCH.7
#     include <pmoist.incl>                                                      ARASCH.8
#     include <param2.incl>                                                      ARASCH.9
#     include <param3.incl>                                                      ARASCH.10
      PARAMETER (KDIM=MKX)                                                       ARASCH.11
      PARAMETER (KNUM=6)                                                         ARASCH.12
      PARAMETER (MEQU=0,MNEQU=KNUM,NIMSL=2*MNEQU,                                ARASCH.13
     *          MBOTH=MEQU+MNEQU,IA=MEQU+MNEQU+2,                                ARASCH.14
     *          IW1=2*(NIMSL+MEQU)+3,IWW=IA*(IA+2)+2*(NIMSL+MEQU)                ARASCH.15
     *         ,MINP=MEQU+NIMSL+2)                                               ARASCH.16
      DIMENSION AE(2),BE(2)                                                      ARASCH.17
C                                                                                ARASCH.18
C------ ENVIRONMENTAL PROPERTIES BEFORE LARGE SCALE FORCING,                     ARASCH.19
C------ ONLY T,QE AND P ARE NEEDED FOR INPUT!                                    ARASCH.20
C                                                                                ARASCH.21
      DIMENSION HE(KDIM),HES(KDIM),QES(KDIM)                                     ARASCH.22
     *         ,Z(KDIM),TV(KDIM)                                                 ARASCH.23
      DIMENSION PI(KLEV),TI(KLEV),QI(KLEV),VSP(KLEV)                             ARASCH.24
      DIMENSION P(KDIM),T(KDIM),QE(KDIM)                                         ARASCH.25
C                                                                                ARASCH.26
C------- ENVIRONMENTAL PROPERTIES AFTER LARGE SCALE FORCING,                     ARASCH.27
C------- TO,QO,PO ARE NEEDED AS INPUT!                                           ARASCH.28
C                                                                                ARASCH.29
      DIMENSION TIO(KLEV),QIO(KLEV),PIO(KLEV)                                    ARASCH.30
C                                                                                ARASCH.31
C------- CLOUD UPDRAFT PROPERTIES , WHICH ARE CALCULATED FOR                     ARASCH.32
C------- EVERY CLOUD (I) AND STORED. KDIM CLOUDS ARE POSSIBLE.                   ARASCH.33
C                                                                                ARASCH.34
      DIMENSION HC(KDIM,KNUM),ZU(KDIM,KNUM),QRC(KDIM,KNUM)                       ARASCH.35
     *         ,PW(KDIM,KNUM),ZUO(KDIM,KNUM),                                    ARASCH.36
     *          PWO(KDIM,KNUM),QSC(KDIM)                                         ARASCH.37
C                                                                                ARASCH.38
C------- CHANGED PROPERTIES AFTER SIMULATED CLOUD INFLUENCE,                     ARASCH.39
C------- NECESSARY FOR KERNEL CALCULATIONS!                                      ARASCH.40
C                                                                                ARASCH.41
      DIMENSION XT(KDIM),XHE(KDIM),XQE(KDIM),XHES(KDIM)                          ARASCH.42
     *         ,XQES(KDIM),XZ(KDIM),XZU(KDIM,KNUM)                               ARASCH.43
     *         ,XTV(KDIM),XHH(KDIM),XPW(KDIM,KNUM),XHC(KDIM,KNUM)                ARASCH.44
     *         ,XA(KNUM),XQRC(KDIM,KNUM),XQSC(KDIM)                              ARASCH.45
C                                                                                ARASCH.46
C------- VARIOUS WORK DIMENSIONS, CLOUDWORKFUNCTIONS (A,AOLD),                   ARASCH.47
C------- KERNELS (XK), DETRAINMENT OF UPDRAFT (CD)                               ARASCH.48
C                                                                                ARASCH.49
      DIMENSION AA(KNUM),XK(KNUM,KNUM),HH(KDIM),DELLA(KDIM)                      ARASCH.50
     *         ,F(KNUM),XX(KNUM),AOLD(KNUM),CDD(KDIM,KNUM)                       ARASCH.51
     *         ,XXO(KNUM)                                                        ARASCH.52
C                                                                                ARASCH.53
C--- DOWNDRAFT DIMENSIONS                                                        ARASCH.54
C                                                                                ARASCH.55
       DIMENSION ZD(KDIM,KNUM),QRCD(KDIM,KNUM),HCD(KDIM,KNUM),                   ARASCH.56
     *           PWD(KDIM,KNUM),XZD(KDIM,KNUM),XQRCD(KDIM,KNUM),                 ARASCH.57
     *           XHCD(KDIM,KNUM),XPWD(KDIM,KNUM),ZDO(KDIM,KNUM),                 ARASCH.58
     *           PWDO(KDIM,KNUM),EDT(KNUM),                                      ARASCH.59
     *           XEDT(KNUM),EDTO(KNUM),EDTOO(KNUM)                               ARASCH.60
       INTEGER KMIN(KNUM),ITEST(KNUM),ITESTX(KNUM),ITEST2(KNUM),                 ARASCH.61
     *         KMINX(KNUM),KTOP(KNUM),KTOPX(KNUM)                                ARASCH.62
C                                                                                ARASCH.63
C------- BLOCK NECESSARY FOR IMSL SOLVER                                         ARASCH.64
C                                                                                ARASCH.65
       DIMENSION RW(IWW),DSOL(MBOTH),PSOL(NIMSL)                                 ARASCH.66
     *          ,B(MBOTH),C(NIMSL),AA1(IA,MINP)                                  ARASCH.67
       INTEGER IW(IW1)                                                           ARASCH.68
C                                                                                ARASCH.69
C------- DIMENSIONS FOR FEEDBACK ROUTINE, INCLUDING FEEDBACK                     ARASCH.70
C------- OUTPUT TO DRIVER ROUTINE (OUTTEM,OUTQ,XMB,XMFLUX)!                      ARASCH.71
C                                                                                ARASCH.72
      DIMENSION DELLT(KDIM,KNUM),DELLQ(KDIM,KNUM)                                ARASCH.73
     *         ,XMFLUX(KDIM),OUTTEM(KLEV),OUTQ(KLEV)                             ARASCH.74
     *         ,XMB(KNUM),RAD(KNUM)                                              ARASCH.75
      DATA RAD/0.,20000.,2000.,400.,267.,200./                                   ARASCH.76
      REAL MBDT                                                                  ARASCH.77
      XX(1)=0.                                                                   ARASCH.78
      DO 5 K=2,KNUM                                                              ARASCH.79
        XX(K)=.2/RAD(K)                                                          ARASCH.80
    5 CONTINUE                                                                   ARASCH.81
C                                                                                ARASCH.82
      LLOOP=0                                                                    ARASCH.83
      IERR=0                                                                     ARASCH.84
      HT1=2.5E6/1004.                                                            ARASCH.85
      HT2=2.834E6/1004.                                                          ARASCH.86
      BE(1)=.622*HT1/.286                                                        ARASCH.87
      AE(1)=BE(1)/273.+ALOG(610.71)                                              ARASCH.88
      BE(2)=.622*HT2/.286                                                        ARASCH.89
      AE(2)=BE(2)/273.+ALOG(610.71)                                              ARASCH.90
      MBDT=DTIME*5.E-03                                                          ARASCH.91
      PRE=0.                                                                     ARASCH.92
  991 CONTINUE                                                                   ARASCH.93
      LLOOP=LLOOP+1                                                              ARASCH.94
      IF(LLOOP.EQ.2)THEN                                                         ARASCH.95
        PSURF=PSURO                                                              ARASCH.96
        DO 97 K=1,KNUM                                                           ARASCH.97
          AOLD(K)=AA(K)                                                          ARASCH.98
          AA(K)=0.                                                               ARASCH.99
          XMB(K)=0.                                                              ARASCH.100
          F(K)=0.                                                                ARASCH.101
          KMIN(K)=-1                                                             ARASCH.102
          EDT(K)=0.                                                              ARASCH.103
          ITEST(K)=-1                                                            ARASCH.104
   97   CONTINUE                                                                 ARASCH.105
        DO 98 K=1,KDIM                                                           ARASCH.106
          XMFLUX(K)=0.                                                           ARASCH.107
          T(K)=TIO(K)                                                            ARASCH.108
          QE(K)=QIO(K)                                                           ARASCH.109
          P(K)=PIO(K)                                                            ARASCH.110
          Z(K)=0.                                                                ARASCH.111
   98   CONTINUE                                                                 ARASCH.112
        DO 99 LL=1,KNUM                                                          ARASCH.113
          DO 99 K=1,KDIM                                                         ARASCH.114
            ZU(K,LL)=0.                                                          ARASCH.115
            ZD(K,LL)=0.                                                          ARASCH.116
            HCD(K,LL)=0.                                                         ARASCH.117
            QRCD(K,LL)=0.                                                        ARASCH.118
            HC(K,LL)=0.                                                          ARASCH.119
            QRC(K,LL)=0.                                                         ARASCH.120
   99     CONTINUE                                                               ARASCH.121
                                                                                 ARASCH.122
      ELSE                                                                       ARASCH.123
        PSURF=PSUR                                                               ARASCH.124
        DO 971 K=1,KNUM                                                          ARASCH.125
          AA(K)=0.                                                               ARASCH.126
          F(K)=0.                                                                ARASCH.127
          XMB(K)=0.                                                              ARASCH.128
          KMIN(K)=-1                                                             ARASCH.129
          EDT(K)=0.                                                              ARASCH.130
          ITESTX(K)=-1                                                           ARASCH.131
          ITEST(K)=-1                                                            ARASCH.132
          DO 971 LL=1,KNUM                                                       ARASCH.133
            XK(K,LL)=0.                                                          ARASCH.134
  971     CONTINUE                                                               ARASCH.135
                                                                                 ARASCH.136
        DO 981 K=1,KDIM                                                          ARASCH.137
          T(K)=TI(K)                                                             ARASCH.138
          QE(K)=QI(K)                                                            ARASCH.139
          P(K)=PI(K)                                                             ARASCH.140
          DELLA(K)=0.                                                            ARASCH.141
          OUTTEM(K)=0.                                                           ARASCH.142
          OUTQ(K)=0.                                                             ARASCH.143
          HH(K)=0.                                                               ARASCH.144
          CDD(K,1)=0.                                                            ARASCH.145
          CDD(K,2)=0.                                                            ARASCH.146
          CDD(K,3)=0.                                                            ARASCH.147
          CDD(K,4)=0.                                                            ARASCH.148
          CDD(K,5)=0.                                                            ARASCH.149
          CDD(K,6)=0.                                                            ARASCH.150
  981   CONTINUE                                                                 ARASCH.151
        DO 982 LL=1,KNUM                                                         ARASCH.152
          DO 982 K=1,KDIM                                                        ARASCH.153
            PWD(K,LL)=0.                                                         ARASCH.154
            PW(K,LL)=0.                                                          ARASCH.155
            DELLT(K,LL)=0.                                                       ARASCH.156
            DELLQ(K,LL)=0.                                                       ARASCH.157
            ZUO(K,LL)=0.                                                         ARASCH.158
            PWO(K,LL)=0.                                                         ARASCH.159
            PWDO(K,LL)=0.                                                        ARASCH.160
            ZDO(K,LL)=0.                                                         ARASCH.161
            ZU(K,LL)=0.                                                          ARASCH.162
            ZD(K,LL)=0.                                                          ARASCH.163
            HCD(K,LL)=0.                                                         ARASCH.164
            QRCD(K,LL)=0.                                                        ARASCH.165
            HC(K,LL)=0.                                                          ARASCH.166
            QRC(K,LL)=0.                                                         ARASCH.167
  982     CONTINUE                                                               ARASCH.168
                                                                                 ARASCH.169
      ENDIF                                                                      ARASCH.170
C                                                                                ARASCH.171
 1103 CONTINUE                                                                   ARASCH.172
 1104 CONTINUE                                                                   ARASCH.173
C****************************************************************C               ARASCH.174
C                                                                                ARASCH.175
C                                                               *                ARASCH.176
C                CALCULATE ENVIRONMENTAL CONDITIONS             *                ARASCH.177
C                                                                                ARASCH.178
C                                                               *                ARASCH.179
C****************************************************************                ARASCH.180
      DO 89 K=1,KDIM                                                             ARASCH.181
        IPH=1                                                                    ARASCH.182
        IF(T(K).LE.TO)IPH=2                                                      ARASCH.183
        E=EXP(AE(IPH)-BE(IPH)/T(K))                                              ARASCH.184
        QES(K)=.622*E/(100.*P(K)-(1.-.622)*E)                                    ARASCH.185
        IF(QES(K).LE.1.E-08)QES(K)=1.E-08                                        ARASCH.186
        IF(QE(K).GT.QES(K))QE(K)=QES(K)                                          ARASCH.187
        TV(K)=T(K)+.608*QE(K)*T(K)                                               ARASCH.188
   89 CONTINUE                                                                   ARASCH.189
C                                                                                ARASCH.190
C                                                                                ARASCH.191
C------- HEIGHTS                                                                 ARASCH.192
C                                                                                ARASCH.193
C                                                                                ARASCH.194
      CALL HEIPRE(P,Z,TV,Z1,KDIM,1,PSURF)                                        ARASCH.195
C                                                                                ARASCH.196
C                                                                                ARASCH.197
C------- DETERMINE MOIST STATIC ENERGY                                           ARASCH.198
C                                                                                ARASCH.199
C                                                                                ARASCH.200
      CALL MOIENE(T,QE,Z,HE,KDIM,1)                                              ARASCH.201
      CALL MOIENE(T,QES,Z,HES,KDIM,1)                                            ARASCH.202
      DO 90 K=1,KDIM                                                             ARASCH.203
        IF(HE(K).GE.HES(K))HE(K)=HES(K)                                          ARASCH.204
   90 CONTINUE                                                                   ARASCH.205
C                                                                                ARASCH.206
C------- DETERMINE LEVEL WITH HIGHEST MOIST STATIC ENERGY CONTENT.               ARASCH.207
C------- FIRST ESTIMATE A LEVEL IN LOWER TROPOSPHERE WITH HIGH                   ARASCH.208
C------- MOIST STATIC ENERGY.                                                    ARASCH.209
C                                                                                ARASCH.210
      CALL MAXIM(HE,KDIM,1,KBMAX,K22)                                            ARASCH.211
      CALL MINIM(HES,KDIM,K22,KDIM,KBHE)                                         ARASCH.212
      CALL MAXIM(HE,KDIM,1,KBHE,K22)                                             ARASCH.213
C                                                                                ARASCH.214
C                                                                                ARASCH.215
C                                                                                ARASCH.216
   53 CONTINUE                                                                   ARASCH.217
   54 CONTINUE                                                                   ARASCH.218
   55 CONTINUE                                                                   ARASCH.219
      HKB=HE(K22)                                                                ARASCH.220
      QKB=QE(K22)                                                                ARASCH.221
C                                                                                ARASCH.222
C--- DECIDE FOR CONVECTIVE CLOUD BASE                                            ARASCH.223
C                                                                                ARASCH.224
      KB=K22                                                                     ARASCH.225
      KBCON=KB                                                                   ARASCH.226
      GOTO 57                                                                    ARASCH.227
   58 CONTINUE                                                                   ARASCH.228
      KBCON=KBCON+1                                                              ARASCH.229
      IF(P(KBCON).LT.400.)RETURN                                                 ARASCH.230
   57 CONTINUE                                                                   ARASCH.231
      DH=.5*HES(KBCON)+.5*HES(KBCON+1)                                           ARASCH.232
      IF(HKB.LT.DH)GOTO 58                                                       ARASCH.233
   59 CONTINUE                                                                   ARASCH.234
      KNUMEN=KNUM                                                                ARASCH.235
C                                                                                ARASCH.236
C--- IF TOO MUCH LIFTING IS REQUIRED, RETURN!                                    ARASCH.237
C                                                                                ARASCH.238
      PBCDIF=-P(KBCON)+P(KB)                                                     ARASCH.239
      IF(PBCDIF.GT.150.)RETURN                                                   ARASCH.240
C                                                                                ARASCH.241
C--- DOWNDRAFT ORIGINATING LEVEL                                                 ARASCH.242
C                                                                                ARASCH.243
      KDIM2=KDIM/2                                                               ARASCH.244
      CALL MINIM(HE,KDIM,KB,KDIM,JMIN)                                           ARASCH.245
      KDIM2=1                                                                    ARASCH.246
      IF(JMIN.LE.4)THEN                                                          ARASCH.247
        JMIN=-1                                                                  ARASCH.248
      ENDIF                                                                      ARASCH.249
      DO 88 K=1,KNUM                                                             ARASCH.250
        IF(JMIN.GE.2)ITEST(K)=JMIN                                               ARASCH.251
        KMIN(K)=JMIN                                                             ARASCH.252
   88 CONTINUE                                                                   ARASCH.253
      CALL MAXIM(VSP,KDIM,1,KDIM,KDS)                                            ARASCH.254
      IF(KDS.GE.KDIM)KDS=KDIM-1                                                  ARASCH.255
      IF(KDS.LE.KBCON)KDS=KBCON                                                  ARASCH.256
C**********************************************************************          ARASCH.257
C**********************************************************************          ARASCH.258
C*                  *********************                                        ARASCH.259
C*                  *   STATIC CONTROL  *                                        ARASCH.260
C*                  *********************                                        ARASCH.261
C*                                                                               ARASCH.262
C                                                                                ARASCH.263
C                                                                                ARASCH.264
C                                                                                ARASCH.265
      DO 100 LP=1,KNUMEN                                                         ARASCH.266
C                                                                                ARASCH.267
C                                                                                ARASCH.268
C                                                                                ARASCH.269
C------- MOIST STATIC ENERGY PROFILE INSIDE CLOUD                                ARASCH.270
C------- AND ENTRAINMENT RATE, NEGATIVE ENTRAINMENT RATES                        ARASCH.271
C------- ARE NOTPOSSIBLE.                                                        ARASCH.272
C                                                                                ARASCH.273
C                                                                                ARASCH.274
        CALL ENTR(CDD,KBCON,HE,HH,HES,XX(LP),KDIM,LP,KB,Z,HKB,KNUM,KTOP)         ARASCH.275
        IF(KTOP(1).LE.2)RETURN                                                   ARASCH.276
        KDTOP=KTOP(LP)-2                                                         ARASCH.277
        IF(KMIN(LP).GE.KDTOP)KMIN(LP)=KDTOP                                      ARASCH.278
C                                                                                ARASCH.279
C------- FINAL MOIST STATIC ENERGYINSIDE CLOUD                                   ARASCH.280
C                                                                                ARASCH.281
C                                                                                ARASCH.282
        DO 1002 K=1,KDIM                                                         ARASCH.283
          HC(K,LP)=HH(K)                                                         ARASCH.284
 1002   CONTINUE                                                                 ARASCH.285
C                                                                                ARASCH.286
C                                                                                ARASCH.287
C------- NORMALIZED MASS FLUX PROFILE                                            ARASCH.288
C                                                                                ARASCH.289
C                                                                                ARASCH.290
        CALL ZUNC(K22,ZU,KBCON,XX(LP),Z,KDIM,LP,KNUM,KTOP)                       ARASCH.291
C                                                                                ARASCH.292
C------- MOISTURE PROPERTIES                                                     ARASCH.293
C                                                                                ARASCH.294
        CALL PRECIP(CDD,KB,KBCON,KDIM,XX(LP),HH,HES,T,QE,QES,PW,QSC,QRC,         ARASCH.295
     +       Z,LP,P,QKB,PCUT,C0,ZU,KNUM,KTOP)                                    ARASCH.296
        DO 1003 K=1,KDIM                                                         ARASCH.297
          HH(K)=0.                                                               ARASCH.298
 1003   CONTINUE                                                                 ARASCH.299
C                                                                                ARASCH.300
C------- DOWNDRAFT CALCULATIONS                                                  ARASCH.301
C                                                                                ARASCH.302
C                                                                                ARASCH.303
C--- DETERMINE DOWNDRAFT STRENGTH IN TERMS OF WINDSHEAR                          ARASCH.304
C                                                                                ARASCH.305
        KK1=KTOP(LP)                                                             ARASCH.306
        VSHEAR=0.                                                                ARASCH.307
        KDSTAR=MIN0(KDS,KK1)                                                     ARASCH.308
        DO 881 KK=KB,KDSTAR                                                      ARASCH.309
          VSHEAR=VSHEAR+ABS((VSP(KK+1)-VSP(KK))/(Z(KK+1)-Z(KK)))                 ARASCH.310
  881   CONTINUE                                                                 ARASCH.311
        KNUMB=KK1-KB+1                                                           ARASCH.312
        IF(KNUMB.EQ.0)KNUMB=1                                                    ARASCH.313
        VSHEAR=1.E3*VSHEAR/FLOAT(KNUMB)                                          ARASCH.314
        E1=1.591-.639*VSHEAR+.0953*(VSHEAR**2)-.00496*(VSHEAR**3)                ARASCH.315
        EDTO(LP)=1.-E1                                                           ARASCH.316
        IF(EDTO(LP).GT.0.9)EDTO(LP)=.9                                           ARASCH.317
        IF(EDTO(LP).LT.0.3)EDTO(LP)=.3                                           ARASCH.318
        EDT(LP)=EDTO(LP)                                                         ARASCH.319
C                                                                                ARASCH.320
C--- DOWNDRAFT THERMODYNAMIC PROPERTIES+MASS FLUX                                ARASCH.321
C                                                                                ARASCH.322
        JTEST=0                                                                  ARASCH.323
        CALL SOUNDD(HE,HH,Z,KDIM,KMIN,PW,LP,HES,QES,QE,T,JTEST,QRCD,PWD,         ARASCH.324
     +       ZD,EDT,KDET,KNUM)                                                   ARASCH.325
        ITEST(LP)=JTEST                                                          ARASCH.326
        IF(JTEST.LT.0)THEN                                                       ARASCH.327
          AA(LP)=0.                                                              ARASCH.328
          KTOP(LP)=2                                                             ARASCH.329
          GOTO 1005                                                              ARASCH.330
        ENDIF                                                                    ARASCH.331
        DO 120 K=1,JMIN                                                          ARASCH.332
          HCD(K,LP)=HH(K)                                                        ARASCH.333
  120   CONTINUE                                                                 ARASCH.334
 1005   CONTINUE                                                                 ARASCH.335
        DO 1004 K=1,KDIM                                                         ARASCH.336
          HH(K)=0.                                                               ARASCH.337
 1004   CONTINUE                                                                 ARASCH.338
  100 CONTINUE                                                                   ARASCH.339
C                                                                                ARASCH.340
C------- STORE VALUES, IF LLOOP EQUALS ONE!!!                                    ARASCH.341
C                                                                                ARASCH.342
      IF(LLOOP.EQ.1)THEN                                                         ARASCH.343
        DO 150 KK=1,KNUM                                                         ARASCH.344
          EDTOO(KK)=EDT(KK)                                                      ARASCH.345
          DO 150 K=1,KDIM                                                        ARASCH.346
            ZUO(K,KK)=ZU(K,KK)                                                   ARASCH.347
            ZDO(K,KK)=ZD(K,KK)                                                   ARASCH.348
            PWO(K,KK)=PW(K,KK)                                                   ARASCH.349
            PWDO(K,KK)=PWD(K,KK)                                                 ARASCH.350
  150     CONTINUE                                                               ARASCH.351
                                                                                 ARASCH.352
      ENDIF                                                                      ARASCH.353
C***********************************************************************         ARASCH.354
C***********************************************************************         ARASCH.355
C                            200                                                 ARASCH.356
C***********************************************************************         ARASCH.357
C                                                                                ARASCH.358
C                                                                                ARASCH.359
C------- CLOUD WORK FUNCTION                                                     ARASCH.360
C                                                                                ARASCH.361
C                                                                                ARASCH.362
      PKTTOP=P(KTOP(1))-P(KBCON)                                                 ARASCH.363
      IF(PKTTOP.GT.-150.)RETURN                                                  ARASCH.364
      DO 200 LP=1,KNUMEN                                                         ARASCH.365
        IF(KTOP(LP).LE.2)GOTO 200                                                ARASCH.366
        CALL CLOUDW(LP,HC,QES,HES,ZU,Z,T,KDIM,AX,KBCON,KNUM,KTOP)                ARASCH.367
        IF(AX.LE.0.)THEN                                                         ARASCH.368
          AA(LP)=0.                                                              ARASCH.369
          KTOP(LP)=2                                                             ARASCH.370
          GOTO 199                                                               ARASCH.371
        ENDIF                                                                    ARASCH.372
        AA(LP)=AX                                                                ARASCH.373
C                                                                                ARASCH.374
C--- CLOUD WORK FUNCTION FOR DOWN DRAFTS                                         ARASCH.375
C                                                                                ARASCH.376
        IF(ITEST(LP).LT.0)GOTO 199                                               ARASCH.377
        JMIN=KMIN(LP)                                                            ARASCH.378
        CALL CLODWD(LP,1,JMIN,HCD,QES,HES,ZD,Z,T,KDIM,AXD,KNUM)                  ARASCH.379
        IF(AXD.LE.0.)THEN                                                        ARASCH.380
          KTOP(LP)=2                                                             ARASCH.381
          AA(LP)=0.                                                              ARASCH.382
          KMIN(LP)=0                                                             ARASCH.383
          ITEST(LP)=-1                                                           ARASCH.384
          GOTO 199                                                               ARASCH.385
        ENDIF                                                                    ARASCH.386
        AX=AX+AXD*EDT(LP)                                                        ARASCH.387
        AA(LP)=AX                                                                ARASCH.388
C                                                                                ARASCH.389
C------- LARGE SCALE FORCING                                                     ARASCH.390
C                                                                                ARASCH.391
  199   CONTINUE                                                                 ARASCH.392
        IF(LLOOP.EQ.1)GOTO 1991                                                  ARASCH.393
        IF((AOLD(LP).EQ.0.).OR.(AA(LP).EQ.0.))THEN                               ARASCH.394
          F(LP)=0.                                                               ARASCH.395
          GOTO 1991                                                              ARASCH.396
        ELSE                                                                     ARASCH.397
          F(LP)=(AA(LP)-AOLD(LP))/DTIME                                          ARASCH.398
        ENDIF                                                                    ARASCH.399
 1991   CONTINUE                                                                 ARASCH.400
  200 CONTINUE                                                                   ARASCH.401
C**********************************************************************          ARASCH.402
C*                             END 200                                           ARASCH.403
C**********************************************************************          ARASCH.404
C**********************************************************************          ARASCH.405
C                                                                                ARASCH.406
C                                                                                ARASCH.407
C------- LOOP TO CALCULATE KERNELS. FOR EVERY CLOUD CALCULATE:                   ARASCH.408
C-------                                                                         ARASCH.409
C-------    1.  CHANGES THAT CLOUD WOULD DO PER UNIT MASS FLUX                   ARASCH.410
C-------        IF IN EXISTENCE TO T,QE,HE FIELDS,                               ARASCH.411
C-------                                                                         ARASCH.412
C-------    2.  NEW ENVIRONMENTAL CONDITIONS,                                    ARASCH.413
C-------                                                                         ARASCH.414
C-------    2.  NEW CLOUD WORK FUNCTIONS WITH MODIFIED ENVIRONMENT,              ARASCH.415
C-------                                                                         ARASCH.416
C-------    4.  KERNELS                                                          ARASCH.417
C-------                                                                         ARASCH.418
C--------------------------------------------------------------------            ARASCH.419
C!!!!!!!!!!!!    ONLY, IF LLOOP=1 !!!!!!!!!!!!!!!!!!!!                           ARASCH.420
C--------------------------------------------------------------------            ARASCH.421
C                                                                                ARASCH.422
C                                                                                ARASCH.423
      IF(LLOOP.EQ.2)GOTO 799                                                     ARASCH.424
      DO 300 LP=1,KNUMEN                                                         ARASCH.425
        IF(KTOP(LP).LE.2)GOTO 300                                                ARASCH.426
C                                                                                ARASCH.427
C                                                                                ARASCH.428
C------- INITIALIZE OLD VALUES FIRST                                             ARASCH.429
C                                                                                ARASCH.430
        DO 3102 K=1,KNUM                                                         ARASCH.431
          KMINX(K)=KMIN(K)                                                       ARASCH.432
          XEDT(K)=EDTO(K)                                                        ARASCH.433
          XA(K)=0.                                                               ARASCH.434
          ITESTX(K)=ITEST(K)                                                     ARASCH.435
 3102   CONTINUE                                                                 ARASCH.436
        DO 3103 K=1,KDIM                                                         ARASCH.437
          XT(K)=T(K)                                                             ARASCH.438
          XQE(K)=QE(K)                                                           ARASCH.439
          XHE(K)=HE(K)                                                           ARASCH.440
          XHH(K)=0.                                                              ARASCH.441
 3103   CONTINUE                                                                 ARASCH.442
        DO 3104 LL=1,KDIM                                                        ARASCH.443
          DO 3104 K=1,KNUM                                                       ARASCH.444
            XQRC(LL,K)=0.                                                        ARASCH.445
            XPW(LL,K)=0.                                                         ARASCH.446
            XPWD(LL,K)=0.                                                        ARASCH.447
            XZU(LL,K)=0.                                                         ARASCH.448
            XZD(LL,K)=0.                                                         ARASCH.449
            XHC(LL,K)=0.                                                         ARASCH.450
            XHCD(LL,K)=0.                                                        ARASCH.451
            XQRCD(LL,K)=0.                                                       ARASCH.452
 3104     CONTINUE                                                               ARASCH.453
                                                                                 ARASCH.454
C                                                                                ARASCH.455
C------- CHANGES IN MOIST STATIC ENERGY AND MOISTURE                             ARASCH.456
C                                                                                ARASCH.457
        CALL KERHEL(HE,XX(LP),LP,ZU,HKB,HC,HH,P,Z,KBCON,KDIM,XHE,MBDT,           ARASCH.458
     +       K22,XHKB,1,CDD,HCD,ZD,ITEST,KDET,EDT,PSURF,KNUM,KTOP)               ARASCH.459
        CALL KERHEL(QE,XX(LP),LP,ZU,QKB,QRC,DELLA,P,Z,KBCON,KDIM,XQE,            ARASCH.460
     +       MBDT,K22,XQKB,2,CDD,QRCD,ZD,ITEST,KDET,EDT,PSURF,KNUM,KTOP)         ARASCH.461
C                                                                                ARASCH.462
C------- TEMPERATURE                                                             ARASCH.463
C                                                                                ARASCH.464
        DO 301 K=1,KDIM                                                          ARASCH.465
          DH=HH(K)                                                               ARASCH.466
          DQ=DELLA(K)                                                            ARASCH.467
          XT(K)=(MBDT/1004.)*(DH-2.5E06*DQ)+T(K)                                 ARASCH.468
          DELLQ(K,LP)=DQ                                                         ARASCH.469
          DELLT(K,LP)=(1./1004.)*(DH-2.5E06*DQ)                                  ARASCH.470
          HH(K)=0.                                                               ARASCH.471
          DELLA(K)=0.                                                            ARASCH.472
  301   CONTINUE                                                                 ARASCH.473
C                                                                                ARASCH.474
C------- VIRTUAL TEMPERATURE FOR HEIGHTS                                         ARASCH.475
C                                                                                ARASCH.476
C                                                                                ARASCH.477
        DO 489 K=1,KDIM                                                          ARASCH.478
          IF(XQE(K).LT.1.E-08)XQE(K)=1.E-08                                      ARASCH.479
          IPH=1                                                                  ARASCH.480
          IF(XT(K).LE.TO)IPH=2                                                   ARASCH.481
          E=EXP(AE(IPH)-BE(IPH)/XT(K))                                           ARASCH.482
          XQES(K)=.622*E/(100.*P(K)-(1.-.622)*E)                                 ARASCH.483
          IF(XQES(K).LT.1.E-08)XQES(K)=1.E-08                                    ARASCH.484
          IF(XQE(K).GT.XQES(K))XQE(K)=XQES(K)                                    ARASCH.485
          XTV(K)=XT(K)+.608*XQE(K)*XT(K)                                         ARASCH.486
  489   CONTINUE                                                                 ARASCH.487
C                                                                                ARASCH.488
C                                                                                ARASCH.489
C------- HEIGHTS                                                                 ARASCH.490
C                                                                                ARASCH.491
C                                                                                ARASCH.492
        CALL HEIPRE(P,XZ,XTV,Z1,KDIM,1,PSURF)                                    ARASCH.493
C                                                                                ARASCH.494
C                                                                                ARASCH.495
C------- DETERMINE SATURATION MOIST STATIC ENERGY                                ARASCH.496
C                                                                                ARASCH.497
C                                                                                ARASCH.498
        CALL MOIENE(XT,XQES,XZ,XHES,KDIM,1)                                      ARASCH.499
        DO 490 K=1,KDIM                                                          ARASCH.500
          IF(XHE(K).GE.XHES(K))XHE(K)=XHES(K)                                    ARASCH.501
  490   CONTINUE                                                                 ARASCH.502
  553   CONTINUE                                                                 ARASCH.503
C                                                                                ARASCH.504
C                                                                                ARASCH.505
C*********************************************************************           ARASCH.506
C*********************************************************************           ARASCH.507
C*                                                                               ARASCH.508
C*                 STATIC CONTROL                                                ARASCH.509
C*                                                                               ARASCH.510
C                                                                                ARASCH.511
C                                                                                ARASCH.512
C                                                                                ARASCH.513
        DO 500 LPX=1,KNUMEN                                                      ARASCH.514
C                                                                                ARASCH.515
C                                                                                ARASCH.516
C------- MOIST STATIC ENERGY PROFILE INSIDE CLOUD                                ARASCH.517
C------- AND ENTRAINMENT RATE, NEGATIVE ENTRAINMENT RATES                        ARASCH.518
C------- ARE NOT POSSIBLE.                                                       ARASCH.519
C                                                                                ARASCH.520
C                                                                                ARASCH.521
          CALL ENTR(CDD,KBCON,XHE,XHH,XHES,XX(LPX),KDIM,LPX,KB,XZ,XHKB,          ARASCH.522
     +         KNUM,KTOPX)                                                       ARASCH.523
          IF(KTOPX(LPX).LE.2)GOTO 500                                            ARASCH.524
C                                                                                ARASCH.525
C                                                                                ARASCH.526
C------- FINAL MOIST STATIC ENERGY INSIDE CLOUD                                  ARASCH.527
C                                                                                ARASCH.528
C                                                                                ARASCH.529
          DO 5002 K=1,KDIM                                                       ARASCH.530
            XHC(K,LPX)=XHH(K)                                                    ARASCH.531
 5002     CONTINUE                                                               ARASCH.532
C                                                                                ARASCH.533
C                                                                                ARASCH.534
C------- NORMALIZED MASS FLUX PROFILE                                            ARASCH.535
C                                                                                ARASCH.536
C                                                                                ARASCH.537
          CALL ZUNC(K22,XZU,KBCON,XX(LPX),XZ,KDIM,LPX,KNUM,KTOPX)                ARASCH.538
C                                                                                ARASCH.539
C------- MOISTURE CALCULATIONS NECESSARY FOR DOWNDRAFTS                          ARASCH.540
C                                                                                ARASCH.541
          CALL PRECIP(CDD,KB,KBCON,KDIM,XX(LPX),XHH,XHES,XT,XQE,XQES,XPW         ARASCH.542
     +         ,XQSC,XQRC,XZ,LPX,P,XQKB,PCUT,C0,XZU,KNUM,KTOPX)                  ARASCH.543
          DO 5003 K=1,KDIM                                                       ARASCH.544
            XHH(K)=0.                                                            ARASCH.545
 5003     CONTINUE                                                               ARASCH.546
C                                                                                ARASCH.547
C------- DOWNDRAFT CALCULATIONS                                                  ARASCH.548
C                                                                                ARASCH.549
          CALL SOUNDD(XHE,XHH,XZ,KDIM,KMINX,XPW,LPX,XHES,XQES,XQE,XT,            ARASCH.550
     +         JTEST,XQRCD,XPWD,XZD,XEDT,KDET,KNUM)                              ARASCH.551
          ITESTX(LPX)=JTEST                                                      ARASCH.552
          IF(JTEST.EQ.-1)THEN                                                    ARASCH.553
            XA(LPX)=0.                                                           ARASCH.554
            KTOPX(LPX)=2                                                         ARASCH.555
            GOTO 5005                                                            ARASCH.556
          ENDIF                                                                  ARASCH.557
          DO 520 K=1,JMIN                                                        ARASCH.558
            XHCD(K,LPX)=XHH(K)                                                   ARASCH.559
  520     CONTINUE                                                               ARASCH.560
 5005     CONTINUE                                                               ARASCH.561
          DO 5004 K=1,KDIM                                                       ARASCH.562
            XHH(K)=0.                                                            ARASCH.563
 5004     CONTINUE                                                               ARASCH.564
  500   CONTINUE                                                                 ARASCH.565
C                                                                                ARASCH.566
C                                        END 500                                 ARASCH.567
C*********************************************************************           ARASCH.568
C*********************************************************************           ARASCH.569
C                                                                                ARASCH.570
C                                                                                ARASCH.571
C------- CLOUD WORK FUNCTION                                                     ARASCH.572
C**********************************************************************          ARASCH.573
C                               600                                              ARASCH.574
        DO 600 LPX=1,KNUMEN                                                      ARASCH.575
          IF(KTOPX(LPX).LE.2)GOTO 699                                            ARASCH.576
C                                                                                ARASCH.577
C------- CLOUD WORK FUNCTION                                                     ARASCH.578
C                                                                                ARASCH.579
          CALL CLOUDW(LPX,XHC,XQES,XHES,XZU,XZ,XT,KDIM,XAX,KBCON,KNUM,           ARASCH.580
     +         KTOPX)                                                            ARASCH.581
C                                                                                ARASCH.582
C------- NO NEGATIVE GENERATION OF BUOYANT ENERGY, PLEEEAAAASSSEEE!              ARASCH.583
C                                                                                ARASCH.584
          IF(XAX.LE.0.)THEN                                                      ARASCH.585
            KTOPX(LPX)=2                                                         ARASCH.586
            XA(LPX)=0.                                                           ARASCH.587
            GOTO 699                                                             ARASCH.588
          ENDIF                                                                  ARASCH.589
C                                                                                ARASCH.590
          XA(LPX)=XAX                                                            ARASCH.591
          XAX=0.                                                                 ARASCH.592
C                                                                                ARASCH.593
C--- CLOUD WORK FUNCTION FOR DOWN DRAFTS                                         ARASCH.594
C                                                                                ARASCH.595
          IF(ITESTX(LPX).LT.0)GOTO 699                                           ARASCH.596
          JMIN=KMINX(LPX)                                                        ARASCH.597
          CALL CLODWD(LPX,1,JMIN,XHCD,XQES,XHES,XZD,XZ,XT,KDIM,XAXD,KNUM         ARASCH.598
     +         )                                                                 ARASCH.599
          IF(XAXD.LE.0.)THEN                                                     ARASCH.600
            KMINX(LPX)=0                                                         ARASCH.601
            KTOPX(LPX)=2                                                         ARASCH.602
            XA(LPX)=0.                                                           ARASCH.603
            ITESTX(LPX)=-1                                                       ARASCH.604
            XAXD=0.                                                              ARASCH.605
            GOTO 699                                                             ARASCH.606
          ENDIF                                                                  ARASCH.607
          XA(LPX)=XA(LPX)+XAXD*XEDT(LPX)                                         ARASCH.608
          XAXD=0.                                                                ARASCH.609
C                                                                                ARASCH.610
C------- 4. KERNELS!                                                             ARASCH.611
C                                                                                ARASCH.612
  699     CONTINUE                                                               ARASCH.613
          IF((XA(LPX).EQ.0.).AND.(AA(LPX).EQ.0.))THEN                            ARASCH.614
            XK(LPX,LP)=0.                                                        ARASCH.615
            GOTO 600                                                             ARASCH.616
          ELSE                                                                   ARASCH.617
            XK(LPX,LP)=(XA(LPX)-AA(LPX))/MBDT                                    ARASCH.618
            IF(LPX.LE.LP)THEN                                                    ARASCH.619
              IF(XK(LPX,LP).GT.0.)XK(LPX,LP)=-1.E-03                             ARASCH.620
            ENDIF                                                                ARASCH.621
          ENDIF                                                                  ARASCH.622
  600   CONTINUE                                                                 ARASCH.623
C                                                                                ARASCH.624
C                       END 600                                                  ARASCH.625
C**********************************************************************          ARASCH.626
  300 CONTINUE                                                                   ARASCH.627
C                       END 300                                                  ARASCH.628
C**********************************************************************          ARASCH.629
C**********************************************************************          ARASCH.630
C**********************************************************************          ARASCH.631
C                                                                                ARASCH.632
C                                                                                ARASCH.633
  899 CONTINUE                                                                   ARASCH.634
      KBXX=KB                                                                    ARASCH.635
      KB=1                                                                       ARASCH.636
  798 IF(LLOOP.EQ.1)GOTO 991                                                     ARASCH.637
  799 CONTINUE                                                                   ARASCH.638
C                                                                                ARASCH.639
C                                                                                ARASCH.640
C------- CALCULATE MASSFLUXES                                                    ARASCH.641
C                                                                                ARASCH.642
C                                                                                ARASCH.643
C                                                                                ARASCH.644
      IF((KBXX.EQ.0).OR.(KBXX.GT.KBMAX))THEN                                     ARASCH.645
        IER=999                                                                  ARASCH.646
        GOTO 5010                                                                ARASCH.647
      ENDIF                                                                      ARASCH.648
      CALL ARAMB(ITEST,RW,IW1,DSOL,PSOL,B,C,AA1,IW,IWW,MBOTH,NIMSL,              ARASCH.649
     +     ITEST2,IA,MINP,XK,F,XMB,KBCON,IER,DTIME,FTEST,KNUM)                   ARASCH.650
      IF((IER.GT.130).AND.(IER.LT.140))THEN                                      ARASCH.651
         PRINT *,'********    PROBLEM IN IMSL ROUTINE ******** '                 ARASCH.652
      ENDIF                                                                      ARASCH.653
 5010 IF(IER.GT.0)THEN                                                           ARASCH.654
        DO 5011 K=1,KDIM                                                         ARASCH.655
          OUTTEM(K)=0.                                                           ARASCH.656
          OUTQ(K)=0.                                                             ARASCH.657
 5011   CONTINUE                                                                 ARASCH.658
        PRE=0.                                                                   ARASCH.659
        IER=0                                                                    ARASCH.660
        RETURN                                                                   ARASCH.661
      ENDIF                                                                      ARASCH.662
      IER=0                                                                      ARASCH.663
C                                                                                ARASCH.664
C                                                                                ARASCH.665
C------- CALCULATE OUTPUT                                                        ARASCH.666
C                                                                                ARASCH.667
C                                                                                ARASCH.668
      CALL ARAOUT(XMFLUX,XMB,ZUO,DELLT,DELLQ,KDIM,OUTTEM,OUTQ,IER,PWO,           ARASCH.669
     +     PRE,PWDO,ZDO,EDTOO,KNUM)                                              ARASCH.670
      IERRT=IER                                                                  ARASCH.671
      IF(IER.EQ.999)GOTO 5010                                                    ARASCH.672
   50 CONTINUE                                                                   ARASCH.673
      RETURN                                                                     ARASCH.674
      END                                                                        ARASCH.675
                                                                                 ARASCH.676
