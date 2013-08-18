      SUBROUTINE SHALLOW(TI,QI,Z1,TIO,QIO,PIO,                                   SHALLOW.1
     * KLEV,PRE,PI,OUTTEM,OUTQ,DTIME,KBMAX,PCUT,C0,                              SHALLOW.2
     * PSUR,IERRT,RADS,XMB)                                                      SHALLOW.3
#     include <parame.incl>                                                      SHALLOW.4
#     include <param2.incl>                                                      SHALLOW.5
#     include <param3.incl>                                                      SHALLOW.6
#     include <pmoist.incl>                                                      SHALLOW.7
      PARAMETER (KDIM=MKX)                                                       SHALLOW.8
      PARAMETER (KNUM=1)                                                         SHALLOW.9
      PARAMETER (MEQU=0,MNEQU=KNUM,NIMSL=2*MNEQU,                                SHALLOW.10
     *          MBOTH=MEQU+MNEQU,IA=MEQU+MNEQU+2,                                SHALLOW.11
     *          IW1=2*(NIMSL+MEQU)+3,IWW=IA*(IA+2)+2*(NIMSL+MEQU)                SHALLOW.12
     *         ,MINP=MEQU+NIMSL+2)                                               SHALLOW.13
      DIMENSION AE(2),BE(2)                                                      SHALLOW.14
C                                                                                SHALLOW.15
C------ ENVIRONMENTAL PROPERTIES BEFORE LARGE SCALE FORCING,                     SHALLOW.16
C------ ONLY T,QE AND P ARE NEEDED FOR INPUT!                                    SHALLOW.17
C                                                                                SHALLOW.18
      DIMENSION HE(KDIM),HES(KDIM),QES(KDIM)                                     SHALLOW.19
     *         ,Z(KDIM),TV(KDIM)                                                 SHALLOW.20
      DIMENSION PI(KDIM),TI(KDIM),QI(KDIM)                                       SHALLOW.21
      DIMENSION P(KDIM),T(KDIM),QE(KDIM)                                         SHALLOW.22
C                                                                                SHALLOW.23
C------- ENVIRONMENTAL PROPERTIES AFTER LARGE SCALE FORCING,                     SHALLOW.24
C------- TO,QO,PO ARE NEEDED AS INPUT!                                           SHALLOW.25
C                                                                                SHALLOW.26
      DIMENSION TIO(KLEV),QIO(KLEV),PIO(KLEV)                                    SHALLOW.27
C                                                                                SHALLOW.28
C------- CLOUD UPDRAFT PROPERTIES , WHICH ARE CALCULATED FOR                     SHALLOW.29
C------- EVERY CLOUD (I) AND STORED. KDIM CLOUDS ARE POSSIBLE.                   SHALLOW.30
C                                                                                SHALLOW.31
      DIMENSION HC(KDIM,KNUM),ZU(KDIM,KNUM),QRC(KDIM,KNUM)                       SHALLOW.32
     *         ,PW(KDIM,KNUM),ZUO(KDIM,KNUM),                                    SHALLOW.33
     *          PWO(KDIM,KNUM),QSC(KDIM)                                         SHALLOW.34
C                                                                                SHALLOW.35
C------- CHANGED PROPERTIES AFTER SIMULATED CLOUD INFLUENCE,                     SHALLOW.36
C------- NECESSARY FOR KERNEL CALCULATIONS!                                      SHALLOW.37
C                                                                                SHALLOW.38
      DIMENSION XT(KDIM),XHE(KDIM),XQE(KDIM),XHES(KDIM)                          SHALLOW.39
     *         ,XQES(KDIM),XZ(KDIM),XZU(KDIM,KNUM)                               SHALLOW.40
     *         ,XTV(KDIM),XHH(KDIM),XPW(KDIM,KNUM),XHC(KDIM,KNUM)                SHALLOW.41
     *         ,XA(KNUM),XQRC(KDIM,KNUM),XQSC(KDIM)                              SHALLOW.42
C                                                                                SHALLOW.43
C------- VARIOUS WORK DIMENSIONS, CLOUDWORKFUNCTIONS (A,AOLD),                   SHALLOW.44
C------- KERNELS (XK), DETRAINMENT OF UPDRAFT (CD)                               SHALLOW.45
C                                                                                SHALLOW.46
      DIMENSION AA(KNUM),XK(KNUM,KNUM),HH(KDIM),DELLA(KDIM)                      SHALLOW.47
     *         ,F(KNUM),XX(KNUM),AOLD(KNUM),CDD(KDIM,KNUM)                       SHALLOW.48
     *         ,XXO(KNUM)                                                        SHALLOW.49
C                                                                                SHALLOW.50
C--- DOWNDRAFT DIMENSIONS                                                        SHALLOW.51
C                                                                                SHALLOW.52
       DIMENSION ZD(KDIM,KNUM),QRCD(KDIM,KNUM),HCD(KDIM,KNUM),                   SHALLOW.53
     *           PWD(KDIM,KNUM),XZD(KDIM,KNUM),XQRCD(KDIM,KNUM),                 SHALLOW.54
     *           XHCD(KDIM,KNUM),XPWD(KDIM,KNUM),ZDO(KDIM,KNUM),                 SHALLOW.55
     *           PWDO(KDIM,KNUM),EDT(KNUM),                                      SHALLOW.56
     *           XEDT(KNUM),EDTO(KNUM),EDTOO(KNUM)                               SHALLOW.57
       INTEGER KMIN(KNUM),ITEST(KNUM),ITESTX(KNUM),ITEST2(KNUM),                 SHALLOW.58
     *         KMINX(KNUM),KTOP(KNUM),KTOPX(KNUM)                                SHALLOW.59
C                                                                                SHALLOW.60
C------- BLOCK NECESSARY FOR IMSL SOLVER                                         SHALLOW.61
C                                                                                SHALLOW.62
       DIMENSION RW(IWW),DSOL(MBOTH),PSOL(NIMSL)                                 SHALLOW.63
     *          ,B(MBOTH),C(NIMSL),AA1(IA,MINP)                                  SHALLOW.64
       INTEGER IW(IW1)                                                           SHALLOW.65
C                                                                                SHALLOW.66
C------- DIMENSIONS FOR FEEDBACK ROUTINE, INCLUDING FEEDBACK                     SHALLOW.67
C------- OUTPUT TO DRIVER ROUTINE (OUTTEM,OUTQ,XMB,XMFLUX)!                      SHALLOW.68
C                                                                                SHALLOW.69
      DIMENSION DELLT(KDIM,KNUM),DELLQ(KDIM,KNUM)                                SHALLOW.70
     *         ,XMFLUX(KDIM),OUTTEM(KLEV),OUTQ(KLEV)                             SHALLOW.71
     *         ,XMB(KNUM),RAD(KNUM),HET(KDIM)                                    SHALLOW.72
      REAL MBDT                                                                  SHALLOW.73
      REAL TOSHALL                                                               25JAN00.481
      RAD(1)=RADS                                                                SHALLOW.74
      XX(1)=.2/RAD(1)                                                            SHALLOW.75
C                                                                                SHALLOW.76
      TOSHALL=50.                                                                25JAN00.482
      LLOOP=0                                                                    SHALLOW.78
      IERR=0                                                                     SHALLOW.79
      HT1=2.5E6/1004.                                                            SHALLOW.80
      HT2=2.834E6/1004.                                                          SHALLOW.81
      BE(1)=.622*HT1/.286                                                        SHALLOW.82
      AE(1)=BE(1)/273.+ALOG(610.71)                                              SHALLOW.83
      BE(2)=.622*HT2/.286                                                        SHALLOW.84
      AE(2)=BE(2)/273.+ALOG(610.71)                                              SHALLOW.85
      MBDT=DTIME*5.E-03                                                          SHALLOW.86
      PRE=0.                                                                     SHALLOW.87
  991 CONTINUE                                                                   SHALLOW.88
      LLOOP=LLOOP+1                                                              SHALLOW.89
      IF(LLOOP.EQ.2)THEN                                                         SHALLOW.90
        PSURF=PSUR                                                               SHALLOW.91
        DO 97 K=1,KNUM                                                           SHALLOW.92
          AOLD(K)=AA(K)                                                          SHALLOW.93
          AA(K)=0.                                                               SHALLOW.94
          XMB(K)=0.                                                              SHALLOW.95
          F(K)=0.                                                                SHALLOW.96
          KMIN(K)=-1                                                             SHALLOW.97
          EDT(K)=0.                                                              SHALLOW.98
          ITEST(K)=-1                                                            SHALLOW.99
   97   CONTINUE                                                                 SHALLOW.100
        DO 98 K=1,KDIM                                                           SHALLOW.101
          XMFLUX(K)=0.                                                           SHALLOW.102
          T(K)=TIO(K)                                                            SHALLOW.103
          QE(K)=QIO(K)                                                           SHALLOW.104
          P(K)=PIO(K)                                                            SHALLOW.105
          Z(K)=0.                                                                SHALLOW.106
   98   CONTINUE                                                                 SHALLOW.107
        DO 99 LL=1,KNUM                                                          SHALLOW.108
          DO 99 K=1,KDIM                                                         SHALLOW.109
            ZU(K,LL)=0.                                                          SHALLOW.110
            ZD(K,LL)=0.                                                          SHALLOW.111
            HCD(K,LL)=0.                                                         SHALLOW.112
            QRCD(K,LL)=0.                                                        SHALLOW.113
            HC(K,LL)=0.                                                          SHALLOW.114
            QRC(K,LL)=0.                                                         SHALLOW.115
   99     CONTINUE                                                               SHALLOW.116
                                                                                 SHALLOW.117
      ELSE                                                                       SHALLOW.118
        PSURF=PSUR                                                               SHALLOW.119
        DO 971 K=1,KNUM                                                          SHALLOW.120
          AA(K)=0.                                                               SHALLOW.121
          F(K)=0.                                                                SHALLOW.122
          XMB(K)=0.                                                              SHALLOW.123
          EDT(K)=0.                                                              SHALLOW.124
          DO 971 LL=1,KNUM                                                       SHALLOW.125
            XK(K,LL)=0.                                                          SHALLOW.126
  971     CONTINUE                                                               SHALLOW.127
                                                                                 SHALLOW.128
        DO 981 K=1,KDIM                                                          SHALLOW.129
          T(K)=TI(K)                                                             SHALLOW.130
          QE(K)=QI(K)                                                            SHALLOW.131
          P(K)=PI(K)                                                             SHALLOW.132
          DELLA(K)=0.                                                            SHALLOW.133
          OUTTEM(K)=0.                                                           SHALLOW.134
          OUTQ(K)=0.                                                             SHALLOW.135
          HH(K)=0.                                                               SHALLOW.136
          DO 981 LL=1,KNUM                                                       SHALLOW.137
            CDD(K,LL)=1.0*XX(LL)                                                 SHALLOW.138
  981     CONTINUE                                                               SHALLOW.139
                                                                                 SHALLOW.140
        DO 982 LL=1,KNUM                                                         SHALLOW.141
          DO 982 K=1,KDIM                                                        SHALLOW.142
            PWD(K,LL)=0.                                                         SHALLOW.143
            PW(K,LL)=0.                                                          SHALLOW.144
            DELLT(K,LL)=0.                                                       SHALLOW.145
            DELLQ(K,LL)=0.                                                       SHALLOW.146
            ZUO(K,LL)=0.                                                         SHALLOW.147
            PWO(K,LL)=0.                                                         SHALLOW.148
            PWDO(K,LL)=0.                                                        SHALLOW.149
            ZDO(K,LL)=0.                                                         SHALLOW.150
            ZU(K,LL)=0.                                                          SHALLOW.151
            ZD(K,LL)=0.                                                          SHALLOW.152
            HCD(K,LL)=0.                                                         SHALLOW.153
            QRCD(K,LL)=0.                                                        SHALLOW.154
            HC(K,LL)=0.                                                          SHALLOW.155
            QRC(K,LL)=0.                                                         SHALLOW.156
  982     CONTINUE                                                               SHALLOW.157
                                                                                 SHALLOW.158
      ENDIF                                                                      SHALLOW.159
C                                                                                SHALLOW.160
 1103 CONTINUE                                                                   SHALLOW.161
 1104 CONTINUE                                                                   SHALLOW.162
C****************************************************************C               SHALLOW.163
C                                                                                SHALLOW.164
C                                                               *                SHALLOW.165
C                CALCULATE ENVIRONMENTAL CONDITIONS             *                SHALLOW.166
C                                                                                SHALLOW.167
C                                                               *                SHALLOW.168
C****************************************************************                SHALLOW.169
      DO 89 K=1,KDIM                                                             SHALLOW.170
        IPH=1                                                                    SHALLOW.171
        IF(T(K).LE.TOSHALL)IPH=2                                                 25JAN00.483
        E=EXP(AE(IPH)-BE(IPH)/T(K))                                              SHALLOW.173
        QES(K)=.622*E/(100.*P(K)-(1.-.622)*E)                                    SHALLOW.174
        IF(QES(K).LE.1.E-08)QES(K)=1.E-08                                        SHALLOW.175
        IF(QE(K).GT.QES(K))QE(K)=QES(K)                                          SHALLOW.176
        TV(K)=T(K)+.608*QE(K)*T(K)                                               SHALLOW.177
   89 CONTINUE                                                                   SHALLOW.178
C                                                                                SHALLOW.179
C                                                                                SHALLOW.180
C------- HEIGHTS                                                                 SHALLOW.181
C                                                                                SHALLOW.182
C                                                                                SHALLOW.183
      CALL HEIPRE(P,Z,TV,Z1,KDIM,1,PSURF)                                        SHALLOW.184
C                                                                                SHALLOW.185
C                                                                                SHALLOW.186
C------- DETERMINE MOIST STATIC ENERGY                                           SHALLOW.187
C                                                                                SHALLOW.188
C                                                                                SHALLOW.189
      CALL MOIENE(T,QE,Z,HE,KDIM,1)                                              SHALLOW.190
      CALL MOIENE(T,QES,Z,HES,KDIM,1)                                            SHALLOW.191
      DO 90 K=1,KDIM                                                             SHALLOW.192
        IF(HE(K).GE.HES(K))HE(K)=HES(K)                                          SHALLOW.193
        IF(K.LT.KDIM)HET(K)=.5*(HE(K)+HE(K+1))                                   SHALLOW.194
   90 CONTINUE                                                                   SHALLOW.195
      HET(KDIM)=HE(KDIM)                                                         SHALLOW.196
C                                                                                SHALLOW.197
C------- DETERMINE LEVEL WITH HIGHEST MOIST STATIC ENERGY CONTENT.               SHALLOW.198
C------- FIRST ESTIMATE A LEVEL IN LOWER TROPOSPHERE WITH HIGH                   SHALLOW.199
C------- MOIST STATIC ENERGY.                                                    SHALLOW.200
C                                                                                SHALLOW.201
      KBHE=1                                                                     SHALLOW.202
      K22=1                                                                      SHALLOW.203
      CALL MINIM(HES,KDIM,K22,KDIM,KBHE)                                         SHALLOW.204
      CALL MAXIM(HET,KDIM,1,KBHE,K22)                                            SHALLOW.205
C                                                                                SHALLOW.206
C                                                                                SHALLOW.207
C                                                                                SHALLOW.208
   53 CONTINUE                                                                   SHALLOW.209
   54 CONTINUE                                                                   SHALLOW.210
   55 CONTINUE                                                                   SHALLOW.211
      HKB=.5*(HE(K22)+HE(K22+1))                                                 SHALLOW.212
      QKB=.5*(QE(K22)+QE(K22+1))                                                 SHALLOW.213
C                                                                                SHALLOW.214
C--- DECIDE FOR CONVECTIVE CLOUD BASE                                            SHALLOW.215
C                                                                                SHALLOW.216
      KB=K22                                                                     SHALLOW.217
      KBCON=KB                                                                   SHALLOW.218
      GOTO 57                                                                    SHALLOW.219
   58 CONTINUE                                                                   SHALLOW.220
      KBCON=KBCON+1                                                              SHALLOW.221
      IF(KBCON.GE.KDIM-1)GOTO 59                                                 SHALLOW.222
   57 CONTINUE                                                                   SHALLOW.223
      DH=.5*HES(KBCON)+.5*HES(KBCON+1)                                           SHALLOW.224
      IF(HKB.LT.DH)GOTO 58                                                       SHALLOW.225
   59 CONTINUE                                                                   SHALLOW.226
      PBCDIF=P(KB)-P(KBCON)                                                      SHALLOW.227
      PBCDIF=.5*(P(KB)+P(KB+1))-P(KBCON)                                         SHALLOW.228
      IF(PBCDIF.GT.50.)THEN                                                      SHALLOW.229
        K22=K22+1                                                                SHALLOW.230
        IF(K22.GT.KBMAX)RETURN                                                   SHALLOW.231
        GOTO 55                                                                  SHALLOW.232
      ENDIF                                                                      SHALLOW.233
      KNUMEN=KNUM                                                                SHALLOW.234
C                                                                                SHALLOW.235
C**********************************************************************          SHALLOW.236
C**********************************************************************          SHALLOW.237
C*                  *********************                                        SHALLOW.238
C*                  *   STATIC CONTROL  *                                        SHALLOW.239
C*                  *********************                                        SHALLOW.240
C*                                                                               SHALLOW.241
C                                                                                SHALLOW.242
C                                                                                SHALLOW.243
C                                                                                SHALLOW.244
      DO 100 LP=1,KNUMEN                                                         SHALLOW.245
C                                                                                SHALLOW.246
C                                                                                SHALLOW.247
C                                                                                SHALLOW.248
C------- MOIST STATIC ENERGY PROFILE INSIDE CLOUD                                SHALLOW.249
C------- AND ENTRAINMENT RATE, NEGATIVE ENTRAINMENT RATES                        SHALLOW.250
C------- ARE NOTPOSSIBLE.                                                        SHALLOW.251
C                                                                                SHALLOW.252
C                                                                                SHALLOW.253
        CALL ENTRS(CDD,KBCON,HE,HH,HES,XX(LP),KDIM,LP,KB,Z,HKB,KNUM,KTOP         SHALLOW.254
     +       )                                                                   SHALLOW.255
C     IF(KTOP(1).LT.2)RETURN                                                     SHALLOW.256
C                                                                                SHALLOW.257
C------- FINAL MOIST STATIC ENERGYINSIDE CLOUD                                   SHALLOW.258
C                                                                                SHALLOW.259
C                                                                                SHALLOW.260
        DO 1002 K=1,KDIM                                                         SHALLOW.261
          HC(K,LP)=HH(K)                                                         SHALLOW.262
 1002   CONTINUE                                                                 SHALLOW.263
C                                                                                SHALLOW.264
C                                                                                SHALLOW.265
C------- NORMALIZED MASS FLUX PROFILE                                            SHALLOW.266
C                                                                                SHALLOW.267
C                                                                                SHALLOW.268
C   ENTNET IS NET ENTRAINMENT MINUS DETRAINMENT WHICH IS ZERO FOR                07NOV00.1476
C   SHALLOW CONVECTION                                                           07NOV00.1477
        ENTNET=0.                                                                07NOV00.1478
        CALL ZUNC(K22,ZU,KBCON,ENTNET,Z,KDIM,LP,KNUM,KTOP)                       07NOV00.1479
C                                                                                SHALLOW.270
C------- MOISTURE PROPERTIES                                                     SHALLOW.271
C                                                                                SHALLOW.272
        CALL PRECIP(CDD,KB,KBCON,KDIM,XX(LP),HH,HES,T,QE,QES,PW,QSC,QRC,         SHALLOW.273
     +       Z,LP,P,QKB,PCUT,C0,ZU,KNUM,KTOP)                                    SHALLOW.274
        DO 1003 K=1,KDIM                                                         SHALLOW.275
          HH(K)=0.                                                               SHALLOW.276
 1003   CONTINUE                                                                 SHALLOW.277
C                                                                                SHALLOW.278
  100 CONTINUE                                                                   SHALLOW.279
C                                                                                SHALLOW.280
C------- STORE VALUES, IF LLOOP EQUALS ONE!!!                                    SHALLOW.281
C                                                                                SHALLOW.282
      IF(LLOOP.EQ.1)THEN                                                         SHALLOW.283
        DO 150 KK=1,KNUM                                                         SHALLOW.284
          DO 150 K=1,KDIM                                                        SHALLOW.285
            ZUO(K,KK)=ZU(K,KK)                                                   SHALLOW.286
            PWO(K,KK)=PW(K,KK)                                                   SHALLOW.287
  150     CONTINUE                                                               SHALLOW.288
                                                                                 SHALLOW.289
      ENDIF                                                                      SHALLOW.290
C***********************************************************************         SHALLOW.291
C***********************************************************************         SHALLOW.292
C                            200                                                 SHALLOW.293
C***********************************************************************         SHALLOW.294
C                                                                                SHALLOW.295
C                                                                                SHALLOW.296
C------- CLOUD WORK FUNCTION                                                     SHALLOW.297
C     IF(KTAU.EQ.300)PRINT *,'200'                                               SHALLOW.298
C                                                                                SHALLOW.299
C                                                                                SHALLOW.300
      DO 200 LP=1,KNUMEN                                                         SHALLOW.301
        IF(KTOP(LP).LT.2)GOTO 200                                                SHALLOW.302
        CALL CLOUDWS(LP,HC,QES,HES,ZU,Z,T,KDIM,AX,KBCON,KNUM,KTOP)               SHALLOW.303
        IF(AX.LE.0.)THEN                                                         SHALLOW.304
          AA(LP)=0.                                                              SHALLOW.305
          KTOP(LP)=1                                                             SHALLOW.306
          GOTO 199                                                               SHALLOW.307
        ENDIF                                                                    SHALLOW.308
        AA(LP)=AX                                                                SHALLOW.309
C                                                                                SHALLOW.310
C                                                                                SHALLOW.311
  199   CONTINUE                                                                 SHALLOW.312
        IF(LLOOP.EQ.1)GOTO 1991                                                  SHALLOW.313
        IF((AOLD(LP).EQ.0.).OR.(AA(LP).EQ.0.))THEN                               SHALLOW.314
          F(LP)=0.                                                               SHALLOW.315
          GOTO 1991                                                              SHALLOW.316
        ELSE                                                                     SHALLOW.317
          F(LP)=(AA(LP)-AOLD(LP))/DTIME                                          SHALLOW.318
        ENDIF                                                                    SHALLOW.319
 1991   CONTINUE                                                                 SHALLOW.320
  200 CONTINUE                                                                   SHALLOW.321
C**********************************************************************          SHALLOW.322
C*                             END 200                                           SHALLOW.323
C**********************************************************************          SHALLOW.324
C**********************************************************************          SHALLOW.325
C                                                                                SHALLOW.326
C                                                                                SHALLOW.327
C------- LOOP TO CALCULATE KERNELS. FOR EVERY CLOUD CALCULATE:                   SHALLOW.328
C-------                                                                         SHALLOW.329
C-------    1.  CHANGES THAT CLOUD WOULD DO PER UNIT MASS FLUX                   SHALLOW.330
C-------        IF IN EXISTENCE TO T,QE,HE FIELDS,                               SHALLOW.331
C-------                                                                         SHALLOW.332
C-------    2.  NEW ENVIRONMENTAL CONDITIONS,                                    SHALLOW.333
C-------                                                                         SHALLOW.334
C-------    2.  NEW CLOUD WORK FUNCTIONS WITH MODIFIED ENVIRONMENT,              SHALLOW.335
C-------                                                                         SHALLOW.336
C-------    4.  KERNELS                                                          SHALLOW.337
C-------                                                                         SHALLOW.338
C--------------------------------------------------------------------            SHALLOW.339
C!!!!!!!!!!!!    ONLY, IF LLOOP=1 !!!!!!!!!!!!!!!!!!!!                           SHALLOW.340
C--------------------------------------------------------------------            SHALLOW.341
C                                                                                SHALLOW.342
C     IF(KTAU.EQ.300)PRINT *,'300'                                               SHALLOW.343
C                                                                                SHALLOW.344
      IF(LLOOP.EQ.2)GOTO 799                                                     SHALLOW.345
      DO 300 LP=1,KNUMEN                                                         SHALLOW.346
        IF(KTOP(LP).LT.2)GOTO 300                                                SHALLOW.347
C                                                                                SHALLOW.348
C                                                                                SHALLOW.349
C------- INITIALIZE OLD VALUES FIRST                                             SHALLOW.350
C                                                                                SHALLOW.351
        DO 3102 K=1,KNUM                                                         SHALLOW.352
          XA(K)=0.                                                               SHALLOW.353
 3102   CONTINUE                                                                 SHALLOW.354
        DO 3103 K=1,KDIM                                                         SHALLOW.355
          XT(K)=T(K)                                                             SHALLOW.356
          XQE(K)=QE(K)                                                           SHALLOW.357
          XHE(K)=HE(K)                                                           SHALLOW.358
          XHH(K)=0.                                                              SHALLOW.359
 3103   CONTINUE                                                                 SHALLOW.360
        DO 3104 LL=1,KDIM                                                        SHALLOW.361
          DO 3104 K=1,KNUM                                                       SHALLOW.362
            XQRC(LL,K)=0.                                                        SHALLOW.363
            XPW(LL,K)=0.                                                         SHALLOW.364
            XZU(LL,K)=0.                                                         SHALLOW.365
            XHC(LL,K)=0.                                                         SHALLOW.366
 3104     CONTINUE                                                               SHALLOW.367
                                                                                 SHALLOW.368
C                                                                                SHALLOW.369
C------- CHANGES IN MOIST STATIC ENERGY AND MOISTURE                             SHALLOW.370
C                                                                                SHALLOW.371
        CALL KERHELS(HE,XX(LP),LP,ZU,HKB,HC,HH,P,Z,KBCON,KDIM,XHE,MBDT,          SHALLOW.372
     +       K22,XHKB,1,CDD,KDET,PSURF,KNUM,KTOP)                                SHALLOW.373
        CALL KERHELS(QE,XX(LP),LP,ZU,QKB,QRC,DELLA,P,Z,KBCON,KDIM,XQE,           SHALLOW.374
     +       MBDT,K22,XQKB,2,CDD,KDET,PSURF,KNUM,KTOP)                           SHALLOW.375
C                                                                                SHALLOW.376
C------- TEMPERATURE                                                             SHALLOW.377
C                                                                                SHALLOW.378
        DO 301 K=1,KDIM                                                          SHALLOW.379
          DH=HH(K)                                                               SHALLOW.380
          DQ=DELLA(K)                                                            SHALLOW.381
          XT(K)=(MBDT/1004.)*(DH-2.5E06*DQ)+T(K)                                 SHALLOW.382
          DELLQ(K,LP)=DQ                                                         SHALLOW.383
          DELLT(K,LP)=(1./1004.)*(DH-2.5E06*DQ)                                  SHALLOW.384
          HH(K)=0.                                                               SHALLOW.385
          DELLA(K)=0.                                                            SHALLOW.386
C     IF(KTAU.EQ.180)PRINT *,DQ,DH,DELLT(K,LP)                                   SHALLOW.387
  301   CONTINUE                                                                 SHALLOW.388
C                                                                                SHALLOW.389
C------- VIRTUAL TEMPERATURE FOR HEIGHTS                                         SHALLOW.390
C                                                                                SHALLOW.391
C                                                                                SHALLOW.392
        DO 489 K=1,KDIM                                                          SHALLOW.393
          IF(XQE(K).LT.1.E-08)XQE(K)=1.E-08                                      SHALLOW.394
          IPH=1                                                                  SHALLOW.395
          IF(XT(K).LE.TOSHALL)IPH=2                                              25JAN00.484
          E=EXP(AE(IPH)-BE(IPH)/XT(K))                                           SHALLOW.397
          XQES(K)=.622*E/(100.*P(K)-(1.-.622)*E)                                 SHALLOW.398
          IF(XQES(K).LT.1.E-08)XQES(K)=1.E-08                                    SHALLOW.399
          IF(XQE(K).GT.XQES(K))XQE(K)=XQES(K)                                    SHALLOW.400
          XTV(K)=XT(K)+.608*XQE(K)*XT(K)                                         SHALLOW.401
  489   CONTINUE                                                                 SHALLOW.402
C                                                                                SHALLOW.403
C                                                                                SHALLOW.404
C------- HEIGHTS                                                                 SHALLOW.405
C                                                                                SHALLOW.406
C                                                                                SHALLOW.407
        CALL HEIPRE(P,XZ,XTV,Z1,KDIM,1,PSURF)                                    SHALLOW.408
C                                                                                SHALLOW.409
C                                                                                SHALLOW.410
C------- DETERMINE SATURATION MOIST STATIC ENERGY                                SHALLOW.411
C                                                                                SHALLOW.412
C                                                                                SHALLOW.413
        CALL MOIENE(XT,XQES,XZ,XHES,KDIM,1)                                      SHALLOW.414
        DO 490 K=1,KDIM                                                          SHALLOW.415
          IF(XHE(K).GE.XHES(K))XHE(K)=XHES(K)                                    SHALLOW.416
  490   CONTINUE                                                                 SHALLOW.417
  553   CONTINUE                                                                 SHALLOW.418
C                                                                                SHALLOW.419
C                                                                                SHALLOW.420
C*********************************************************************           SHALLOW.421
C*********************************************************************           SHALLOW.422
C*                                                                               SHALLOW.423
C*                 STATIC CONTROL                                                SHALLOW.424
C*                                                                               SHALLOW.425
C                                                                                SHALLOW.426
C                                                                                SHALLOW.427
C                                                                                SHALLOW.428
C     IF(KTAU.EQ.300)PRINT *,'500'                                               SHALLOW.429
        DO 500 LPX=1,KNUMEN                                                      SHALLOW.430
C                                                                                SHALLOW.431
C                                                                                SHALLOW.432
C------- MOIST STATIC ENERGY PROFILE INSIDE CLOUD                                SHALLOW.433
C------- AND ENTRAINMENT RATE, NEGATIVE ENTRAINMENT RATES                        SHALLOW.434
C------- ARE NOT POSSIBLE.                                                       SHALLOW.435
C                                                                                SHALLOW.436
C                                                                                SHALLOW.437
          CALL ENTRS(CDD,KBCON,XHE,XHH,XHES,XX(LPX),KDIM,LPX,KB,XZ,XHKB,         SHALLOW.438
     +         KNUM,KTOPX)                                                       SHALLOW.439
          IF(KTOPX(LPX).LT.2)GOTO 500                                            SHALLOW.440
C                                                                                SHALLOW.441
C                                                                                SHALLOW.442
C------- FINAL MOIST STATIC ENERGY INSIDE CLOUD                                  SHALLOW.443
C                                                                                SHALLOW.444
C                                                                                SHALLOW.445
          DO 5002 K=1,KDIM                                                       SHALLOW.446
            XHC(K,LPX)=XHH(K)                                                    SHALLOW.447
 5002     CONTINUE                                                               SHALLOW.448
C                                                                                SHALLOW.449
C                                                                                SHALLOW.450
C------- NORMALIZED MASS FLUX PROFILE                                            SHALLOW.451
C                                                                                SHALLOW.452
C                                                                                SHALLOW.453
          CALL ZUNC(K22,XZU,KBCON,ENTNET,XZ,KDIM,LPX,KNUM,KTOPX)                 07NOV00.1480
C                                                                                SHALLOW.455
C------- MOISTURE CALCULATIONS NECESSARY FOR DOWNDRAFTS                          SHALLOW.456
C                                                                                SHALLOW.457
          CALL PRECIP(CDD,KB,KBCON,KDIM,XX(LPX),XHH,XHES,XT,XQE,XQES,XPW         SHALLOW.458
     +         ,XQSC,XQRC,XZ,LPX,P,XQKB,PCUT,C0,XZU,KNUM,KTOPX)                  SHALLOW.459
          DO 5003 K=1,KDIM                                                       SHALLOW.460
            XHH(K)=0.                                                            SHALLOW.461
 5003     CONTINUE                                                               SHALLOW.462
  500   CONTINUE                                                                 SHALLOW.463
C                                                                                SHALLOW.464
C                                        END 500                                 SHALLOW.465
C*********************************************************************           SHALLOW.466
C*********************************************************************           SHALLOW.467
C                                                                                SHALLOW.468
C                                                                                SHALLOW.469
C------- CLOUD WORK FUNCTION                                                     SHALLOW.470
C**********************************************************************          SHALLOW.471
C                               600                                              SHALLOW.472
        DO 600 LPX=1,KNUMEN                                                      SHALLOW.473
          IF(KTOPX(LPX).LT.2)GOTO 699                                            SHALLOW.474
C                                                                                SHALLOW.475
C------- CLOUD WORK FUNCTION                                                     SHALLOW.476
C                                                                                SHALLOW.477
          CALL CLOUDWS(LPX,XHC,XQES,XHES,XZU,XZ,XT,KDIM,XAX,KBCON,KNUM,          SHALLOW.478
     +         KTOPX)                                                            SHALLOW.479
C                                                                                SHALLOW.480
C------- NO NEGATIVE GENERATION OF BUOYANT ENERGY, PLEEEAAAASSSEEE!              SHALLOW.481
C                                                                                SHALLOW.482
          IF(XAX.LE.0.)THEN                                                      SHALLOW.483
            KTOPX(LPX)=1                                                         SHALLOW.484
            XA(LPX)=0.                                                           SHALLOW.485
            GOTO 699                                                             SHALLOW.486
          ENDIF                                                                  SHALLOW.487
C                                                                                SHALLOW.488
          XA(LPX)=XAX                                                            SHALLOW.489
          XAX=0.                                                                 SHALLOW.490
C                                                                                SHALLOW.491
  699     CONTINUE                                                               SHALLOW.492
          IF((XA(LPX).EQ.0.).AND.(AA(LPX).EQ.0.))THEN                            SHALLOW.493
            XK(LPX,LP)=0.                                                        SHALLOW.494
            GOTO 600                                                             SHALLOW.495
          ELSE                                                                   SHALLOW.496
            XK(LPX,LP)=(XA(LPX)-AA(LPX))/MBDT                                    SHALLOW.497
            IF(LPX.LE.LP)THEN                                                    SHALLOW.498
              IF(XK(LPX,LP).GT.0.)THEN                                           SHALLOW.499
                XK(LPX,LP)=-1.                                                   SHALLOW.500
              ENDIF                                                              SHALLOW.501
            ENDIF                                                                SHALLOW.502
          ENDIF                                                                  SHALLOW.503
  600   CONTINUE                                                                 SHALLOW.504
C                                                                                SHALLOW.505
C                       END 600                                                  SHALLOW.506
C**********************************************************************          SHALLOW.507
  300 CONTINUE                                                                   SHALLOW.508
C                       END 300                                                  SHALLOW.509
C**********************************************************************          SHALLOW.510
C**********************************************************************          SHALLOW.511
C**********************************************************************          SHALLOW.512
C                                                                                SHALLOW.513
C                                                                                SHALLOW.514
  899 CONTINUE                                                                   SHALLOW.515
      KBXX=KB                                                                    SHALLOW.516
      KB=1                                                                       SHALLOW.517
  798 IF(LLOOP.EQ.1)GOTO 991                                                     SHALLOW.518
  799 CONTINUE                                                                   SHALLOW.519
C                                                                                SHALLOW.520
C                                                                                SHALLOW.521
C------- CALCULATE MASSFLUXES                                                    SHALLOW.522
C                                                                                SHALLOW.523
C                                                                                SHALLOW.524
C                                                                                SHALLOW.525
      IER=0                                                                      SHALLOW.526
      IF(XK(1,1).NE.0.)XMB(1)=-F(1)/XK(1,1)                                      SHALLOW.527
      IF(XMB(1).LT.0.)XMB(1)=0.                                                  SHALLOW.528
      IF((KBXX.EQ.0).OR.(KBXX.GT.KBMAX))THEN                                     SHALLOW.529
C       PRINT *,'KBXX,KBMAX = ',KBXX,KBMAX                                       25JAN00.485
        IER=999                                                                  SHALLOW.531
        GOTO 5010                                                                SHALLOW.532
      ENDIF                                                                      SHALLOW.533
      IF((IER.GT.130).AND.(IER.LT.140))THEN                                      SHALLOW.534
         PRINT *,'********    PROBLEM IN IMSL ROUTINE ******** '                 SHALLOW.535
      ENDIF                                                                      SHALLOW.536
 5010 IF(IER.GT.0)THEN                                                           SHALLOW.537
        DO 5011 K=1,KDIM                                                         SHALLOW.538
          OUTTEM(K)=0.                                                           SHALLOW.539
          OUTQ(K)=0.                                                             SHALLOW.540
 5011   CONTINUE                                                                 SHALLOW.541
        PRE=0.                                                                   SHALLOW.542
        IER=0                                                                    SHALLOW.543
        RETURN                                                                   SHALLOW.544
      ENDIF                                                                      SHALLOW.545
      IER=0                                                                      SHALLOW.546
C                                                                                SHALLOW.547
C                                                                                SHALLOW.548
C------- CALCULATE OUTPUT                                                        SHALLOW.549
C                                                                                SHALLOW.550
C                                                                                SHALLOW.551
      CALL ARAOUTS(XMFLUX,XMB,ZUO,DELLT,DELLQ,KDIM,OUTTEM,OUTQ,IER,PRE,          SHALLOW.552
     +     KNUM)                                                                 SHALLOW.553
      IERRT=IER                                                                  SHALLOW.554
      IF(IER.GT.0)GOTO 5010                                                      SHALLOW.555
   50 CONTINUE                                                                   SHALLOW.556
      RETURN                                                                     SHALLOW.557
      END                                                                        SHALLOW.558
                                                                                 SHALLOW.559
