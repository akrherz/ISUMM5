      SUBROUTINE ACM(PSTAR,UST,KL,KLM,J,DTPBL,ILX,IMOISTX)                       ACM.1
      IMPLICIT NONE                                                              ACM.2
C**********************************************************************          ACM.3
C   SUBROUTINE TO COMPUTE CONVECTIVE MIXING IN THE CBL                           ACM.4
C     ACCORDING TO THE ASYMMETRICAL CONVECTIVE MODEL (ACM).                      ACM.5
C   REF: PLEIM AND CHANG (1992)                                                  ACM.6
C                                                                                ACM.7
C--SUMMARY:                                                                      ACM.8
C   ACM IS BASED ON THE BLACKADAR NON-LOCAL CONVECTIVE MODEL WHICH IS            ACM.9
C   USED IN HIRPBL WHERE UPWARD MIXING SIMILAR TO BLACKADAR BUT                  ACM.10
C   DOWNWARD MIXING IS TO THE NEXT LOWER LEVEL REPRESENTING MORE                 ACM.11
C   REALISTIC GRADUAL SUBSIDENCE.                                                ACM.12
C                                                                                ACM.13
C-- NOTE THAT THIS ALGORITHM DIFFERS FROM HIRPBL IN TWO OTHER WAYS:              ACM.14
C     1. VERTICAL MIXING IN SIGMA COORDS RATHER THAN Z COORD                     ACM.15
C     2. DIFF EQS SOLVED BY SEMI-IMPLICIT METHOD (CRANKP=0.5)                    ACM.16
C                                                                                ACM.17
C  ARGUMENTS:                                                                    ACM.18
C    PSTAR: P AT SURFACE - P AT TOP (CB)                                         ACM.19
C    UST:   FRICTION VELOCITY (M/S)                                              ACM.20
C    KL:    TOTAL NUMBER OF LAYERS IN THE VERTICAL                               ACM.21
C    KLM:   KL-1                                                                 ACM.22
C    J:     INDEX IN EAST-WEST DIRECTION                                         ACM.23
C    DTPBL: TIME STEP OF THE MINOR LOOP FOR THE LAND-SURFACE/PBL MODEL           ACM.24
C    ILX:   NUMBER OF ROWS IN NORTH-SOUTH DIRECTION                              ACM.25
C    IMOISTX: MOISTURE FLAG                                                      ACM.26
C                                                                                ACM.27
C  REVISION HISTORY:                                                             ACM.28
C    JEP       5/01 - Moved Mbar calc from pxpbl                                 ACM.29
C    JEP      12/99 - V3                                                         ACM.30
C    JEP       9/96 - Internalized I-Loop and combined w/ VDIFF                  ACM.31
C    JEP       9/95 - Added explicit cloud and ice                               ACM.32
C    D. TIVEL  8/95 - Minor adjustments from MM4 to MM5                          ACM.33
C    JEP       Originally addapted from RADM                                     ACM.34
C**********************************************************************          ACM.35
C                                                                                ACM.36
#     include <parame.incl>                                                      ACM.37
#     include <comicl.incl>                                                      ACM.38
C#     include <soilp.incl>                                                      ACM.39
C                                                                                ACM.40
      INTEGER NSP                                                                ACM.41
      PARAMETER (NSP = 6)                                                        ACM.42
C                                                                                ACM.43
C.......Arguments                                                                ACM.44
      REAL PSTAR(MIX,MJX),UST(MIX,MJX)                                           ACM.45
      REAL DTPBL                                                                 ACM.46
      INTEGER J,ILX,KL,KLM,IMOISTX                                               ACM.47
C                                                                                ACM.48
C.......... Local arrays                                                         ACM.49
      REAL DELC(MKX)                                                             ACM.50
      REAL*8 AI(MIX,MKX),BI(MIX,MKX),EI(MIX,MKX)                                 ACM.51
      REAL*8 DI(MIX,MKX),UI(MIX,MKX)                                             ACM.52
      REAL*8 ALPHA(MIX),BETA(MIX),GAMA(MIX)                                      ACM.53
      REAL VCI(MIX,MKX,NSP),FS(MIX,NSP)                                          ACM.54
      REAL GAM(MIX,MKX)                                                          ACM.55
      REAL BCBOTN(MIX,NSP),DTS                                                   ACM.56
      REAL PSTARI(MIX)                                                           ACM.57
      INTEGER KCBL(MIX)                                                          ACM.58
      REAL RAH(MIX)                                                              ACM.59
      REAL MBAR,MBARKS(MIX,MKX),MDWN(MIX,MKX),MBMAX                              ACM.60
C                                                                                ACM.61
C.......Index variables                                                          ACM.62
      INTEGER NLP,K,I,NL,L                                                       ACM.63
C                                                                                ACM.64
C.......Local variables                                                          ACM.65
      REAL HFLXSL,G,HFMIX                                                        ACM.66
      DATA G/9.8/                                                                ACM.67
      REAL XX                                                                    ACM.68
      DATA XX/0.5/         ! FACTOR APPLIED TO CONV MIXING TIME STEP             ACM.69
      INTEGER KCBLMX,NSPX                                                        ACM.70
      REAL G1000I,DTLIM,RZ,FM,WSPD,F1,EKZ,DTRAT                                  ACM.71
C                                                                                ACM.72
C--Start Exicutable ----                                                         ACM.73
      G1000I=1./G1000                                                            ACM.74
      DTLIM = DTPBL                                                              ACM.75
      KCBLMX = 0                                                                 ACM.76
      MBMAX=0.0                                                                  ACM.77
                                                                                 ACM.78
C---COMPUTE ACM MIXING RATE                                                      ACM.79
      DO 3 I = 1, ILX                                                            ACM.80
        PSTARI(I) = 1.0 / PSTAR(I,J)                                             ACM.81
        KCBL(I) = 1                                                              ACM.82
        IF(NOCONV(I).EQ.1) THEN                                                  ACM.83
           KCBL(I) = KLPBL(I)                                                    ACM.84
C-------MBARKS IS UPWARD MIXING RATE; MDWN IS DOWNWARD MIXING RATE               ACM.85
C-- SURFACE LAYER HEAT FLUX - BLACKADAR 1979 EQN 10                              ACM.86
C... compute HFLXSL (HEAT FLUX BETWEEN K=1 AND K=2),                             ACM.87
           HFLXSL = C1ACM * SQRT(G / THETAV(I,1)) *                              ACM.88
     &              ((THETAV(I,1) - THETAV(I,2)) /                               ACM.89
     &              (ZF(I,1) ** C2ACM -                                          ACM.90
     &              (2.0 * Z(I,2)) ** C2ACM)) ** 1.5                             ACM.91
C-- MBAR = MASS FRACTION MIXING RATE (1/S)  (SEE PLEIM AND CHANG 1992)           ACM.92
          HFMIX = (SIGMAF(1) - PBLSIG(I)) * (THETAV(I,1) -                       ACM.93
     &             THETAV(I,2))                                                  ACM.94
          MBAR = HFLXSL * DENSX(I,1) * G1000 *PSTARI(I) /                        ACM.95
     &           HFMIX                                                           ACM.96
          MBMAX = AMAX1(MBMAX,MBAR)                                              ACM.97
          DO K = 1,KCBL(I)                                                       ACM.98
             MBARKS(I,K) = MBAR                                                  ACM.99
             MDWN(I,K) = MBAR * (PBLSIG(I) - SIGMAF(K-1)) *                      ACM.100
     &                     DSIGHI(K)                                             ACM.101
          ENDDO                                                                  ACM.102
          MBARKS(I,KCBL(I)) = MDWN(I,KCBL(I))                                    ACM.103
          MDWN(I,KCBL(I)+1) = 0.0                                                ACM.104
        ENDIF                                                                    ACM.105
3     CONTINUE                                                                   ACM.106
                                                                                 ACM.107
      DO 5 I=1,ILX                                                               ACM.108
        IF(NOCONV(I).EQ.1) THEN                                                  ACM.109
          KCBLMX = AMAX0(KLPBL(I),KCBLMX)                                        ACM.110
          RZ     = (SIGMAF(KCBL(I)) - SIGMAF(1)) * DSIGHI(1)                     ACM.111
          DTLIM  = AMIN1(XX / (MBARKS(I,1) * RZ),DTLIM)                          ACM.112
        ENDIF                                                                    ACM.113
5     CONTINUE                                                                   ACM.114
C                                                                                ACM.115
      DO 10 K=1,KL                                                               ACM.116
      DO 10 I=1,ILX                                                              ACM.117
        VCI(I,K,1) = THETA(I,K)                                                  ACM.118
        VCI(I,K,2) = QVS(I,K)                                                    ACM.119
        VCI(I,K,3) = US(I,K)                                                     ACM.120
        VCI(I,K,4) = VS(I,K)                                                     ACM.121
        UI(I,K)  = 0.           ! INIT VARIABLE FOR USE BELOW                    ACM.122
        GAM(I,K) = 0.                                                            ACM.123
10    CONTINUE                                                                   ACM.124
      NSPX=4                                                                     ACM.125
                                                                                 ACM.126
C -- Also mix cloud water and ice if necessary as in MM5                         ACM.127
      IF (IMOISTX.NE.1.AND.IMOISTX.NE.3) THEN                                    ACM.128
        DO 15 K=1,KL                                                             ACM.129
        DO 15 I=1,ILX                                                            ACM.130
          VCI(I,K,5) = QCS(I,K)                                                  ACM.131
          VCI(I,K,6) = QIS(I,K)                                                  ACM.132
15      CONTINUE                                                                 ACM.133
        NSPX=6                                                                   ACM.134
      ENDIF                                                                      ACM.135
                                                                                 ACM.136
C **                                                                             ACM.137
      DO 17 I=1,ILX                                                              ACM.138
C       RAH(I) = RA(I,J) + 3.976/UST(I,J)                                        ACM.139
        FS(I,1) = -UST(I,J) * TST(I,J) * DENSX(I,1) * PSTARI(I)                  19DEC02.1801
        FS(I,2) = -UST(I,J) * QST(I) * DENSX(I,1) * PSTARI(I)                    ACM.141
        FM    = -USTM(I) * USTM(I) * DENSX(I,1) * PSTARI(I)                      19DEC02.1802
        WSPD =SQRT(US(I,1)*US(I,1)+VS(I,1)*VS(I,1))+1.E-9                        ACM.143
        FS(I,3) = FM * US(I,1)/WSPD                                              ACM.144
        FS(I,4) = FM * VS(I,1)/WSPD                                              ACM.145
        FS(I,5) = 0.0                                                            ACM.146
        FS(I,6) = 0.0    ! SURFACE FLUXES OF CLOUD WATER AND ICE = 0             ACM.147
17    CONTINUE                                                                   ACM.148
C                                                                                ACM.149
C      DO 2200 I = 1,ILX  VECTOR OVER I-LOOP INSIDE!!!!!                         ACM.150
      IF(KCBLMX.LT.2) GO TO 55  ! NO ACM                                         ACM.151
      NLP = INT(DTPBL / DTLIM + 1.0)                                             ACM.152
      DTS = (DTPBL / NLP)                                                        ACM.153
      DO 2000 NL = 1,NLP      ! LOOP OVER SUB TIME LOOP                          ACM.154
C                                                                                ACM.155
        DO 1000 L = 1,NSPX     ! loop for thetav, q, u and v (qc, qi)            ACM.156
C                                                                                ACM.157
C**   COMPUTE TENDENCY OF CBL CONCENTRATIONS - SEMI-IMPLICIT SOLUTION            ACM.158
          DO 20 K = 2,KCBLMX                                                     ACM.159
           DO 19 I = 1,ILX                                                       ACM.160
            IF(K.LE.KCBL(I))THEN                                                 ACM.161
            DELC(K) = DTS * (MBARKS(I,K) * VCI(I,1,L) - MDWN(I,K)*               ACM.162
     &                VCI(I,K,L) + DSIGH(K+1) * DSIGHI(K) *                      ACM.163
     &                MDWN(I,K+1) * VCI(I,K+1,L))                                ACM.164
            DI(I,K) = VCI(I,K,L) + (1.0 - CRANKP) * DELC(K)                      ACM.165
            EI(I,K) = -CRANKP*MDWN(I,K)*DTS*DSIGH(K)*DSIGHI(K-1)                 ACM.166
            BI(I,K) = 1.0 + CRANKP * MDWN(I,K) * DTS                             ACM.167
            AI(I,K) = -CRANKP * MBARKS(I,K) * DTS                                ACM.168
            ENDIF                                                                ACM.169
19         CONTINUE                                                              ACM.170
20        CONTINUE                                                               ACM.171
        DO 201 I = 1,ILX                                                         ACM.172
          IF(NOCONV(I).EQ.1) THEN                                                ACM.173
            BI(I,1) = 1.0 + CRANKP * MBARKS(I,1) *                               ACM.174
     &            (PBLSIG(I) - SIGMAF(1)) * DTS * DSIGHI(1)                      ACM.175
            F1 = -G1000I * (MBARKS(I,1) *                                        ACM.176
     &         (PBLSIG(I) - SIGMAF(1)) * VCI(I,1,L) -                            ACM.177
     &          MDWN(I,2) * VCI(I,2,L) * DSIGH(2))                               ACM.178
            DI(I,1) = VCI(I,1,L) - G1000 * (FS(I,L) -                            ACM.179
     &            (1.0 - CRANKP) * F1) * DSIGHI(1) * DTS                         ACM.180
C                                                                                ACM.181
C-- DEFINE ARRAYS A,B,E WHICH MAKE UP MATRIX AND D WHICH IS RHS                  ACM.182
            BETA(I) = DI(I,1)                                                    ACM.183
            GAMA(I) = BI(I,1)                                                    ACM.184
            ALPHA(I) = 1.0                                                       ACM.185
          ENDIF                                                                  ACM.186
201     CONTINUE                                                                 ACM.187
          DO 21 K = 2,KCBLMX                                                     ACM.188
           DO 202 I = 1,ILX                                                      ACM.189
           IF(K.LE.KCBL(I))THEN                                                  ACM.190
            ALPHA(I) = -ALPHA(I) * EI(I,K) / BI(I,K)                             ACM.191
            BETA(I)  = ALPHA(I) * DI(I,K) + BETA(I)                              ACM.192
            GAMA(I)  = ALPHA(I) * AI(I,K) + GAMA(I)                              ACM.193
           ENDIF                                                                 ACM.194
202        CONTINUE                                                              ACM.195
21        CONTINUE                                                               ACM.196
                                                                                 ACM.197
          DO 222 I = 1,ILX                                                       ACM.198
            IF(KCBL(I).GT.1)THEN                                                 ACM.199
              UI(I,1)    = BETA(I) / GAMA(I)                                     ACM.200
              UI(I,KCBL(I)) = (DI(I,KCBL(I))-AI(I,KCBL(I))*UI(I,1))              ACM.201
     >                       / BI(I,KCBL(I))                                     ACM.202
            ENDIF                                                                ACM.203
222       CONTINUE                                                               ACM.204
C                                                                                ACM.205
C **  BACK SUBSTITUTION:                                                         ACM.206
          DO 22 K = KCBLMX - 1,2,-1                                              ACM.207
           DO 223 I = 1,ILX                                                      ACM.208
            IF(K.LE.KCBL(I)-1)THEN                                               ACM.209
               UI(I,K) = (DI(I,K) - AI(I,K) * UI(I,1) - EI(I,K+1)                ACM.210
     >                 * UI(I,K+1)) / BI(I,K)                                    ACM.211
            ENDIF                                                                ACM.212
223        CONTINUE                                                              ACM.213
22        CONTINUE                                                               ACM.214
C                                                                                ACM.215
C-- COMPUTE NEW THETAV AND Q                                                     ACM.216
          DO 30 K = 1,KCBLMX                                                     ACM.217
           DO 233 I = 1,ILX                                                      ACM.218
            IF(K.LE.KCBL(I).AND.NOCONV(I).EQ.1)THEN                              ACM.219
               VCI(I,K,L) = UI(I,K)                                              ACM.220
            ENDIF                                                                ACM.221
233        CONTINUE                                                              ACM.222
30        CONTINUE                                                               ACM.223
C                                                                                ACM.224
1000    CONTINUE                 ! END LOOP FOR THETAV,Q,U AND V                 ACM.225
C                                                                                ACM.226
C... update the surface heat flux                                                ACM.227
C     DO 1111 I = 1,ILX                                                          ACM.228
C       FS(I,1) = (THETAG(I) - VCI(I,1,1)) / RAH(I) * DENSX(I,1) *               ACM.229
C    &            PSTARI(I)                                                      ACM.230
C1111  CONTINUE                                                                  ACM.231
C                                                                                ACM.232
2000  CONTINUE                   ! END SUB TIME LOOP                             ACM.233
                                                                                 ACM.234
                                                                                 ACM.235
C----------VDIFF------------------                                               ACM.236
55      DTLIM = DTPBL                                                            ACM.237
        DO 60 K = 1,KLM                                                          ACM.238
          DO 65 I = 1,ILX                                                        ACM.239
            IF (K.GE.KCBL(I)) THEN                                               ACM.240
              EKZ   = EDDYZ(I,K) / DTPBL * DSIGFI(K)                             ACM.241
              DTLIM = AMIN1(0.75 / EKZ,DTLIM)                                    ACM.242
            ENDIF                                                                ACM.243
65        CONTINUE                                                               ACM.244
60      CONTINUE                                                                 ACM.245
        IF (DTLIM .EQ. DTPBL) THEN                                               ACM.246
          NLP = 1                                                                ACM.247
         ELSE                                                                    ACM.248
          NLP = INT(DTPBL / DTLIM + 1.0)                                         ACM.249
        END IF                                                                   ACM.250
        DTS   = DTPBL / NLP                                                      ACM.251
      DTRAT = DTS / DTPBL                                                        ACM.252
C                                                                                ACM.253
      DO 70 L = 1,NSPX                                                           ACM.254
        DO 75 I = 1,ILX                                                          ACM.255
          IF(KCBL(I).EQ.1) THEN                                                  ACM.256
             BCBOTN(I,L) = -G1000 * FS(I,L) * DSIGHI(1) * DTS                    ACM.257
          ELSE                                                                   ACM.258
             BCBOTN(I,L) = 0.0                                                   ACM.259
          ENDIF                                                                  ACM.260
75      CONTINUE                                                                 ACM.261
70    CONTINUE                                                                   ACM.262
C                                                                                ACM.263
          DO 80 K = 2,KL                                                         ACM.264
            DO 85 I = 1,ILX                                                      ACM.265
C              IF(K.GT.KCBL(I)) THEN                                             ACM.266
C------- note that we are using eddyz(k-1) instead of eddyz(k) here              ACM.267
C------- keep dsighi(k)                                                          ACM.268
                AI(I,K) = -EDDYZ(I,K-1) * CRANKP * DSIGHI(K) * DTRAT             ACM.269
C              ENDIF                                                             ACM.270
85          CONTINUE                                                             ACM.271
80        CONTINUE                                                               ACM.272
C                                                                                ACM.273
          DO 90 K = 1,KLM                                                        ACM.274
            DO 95 I = 1,ILX                                                      ACM.275
C              IF(K.GE.KCBL(I)) THEN                                             ACM.276
                EI(I,K) = -EDDYZ(I,K) * CRANKP * DSIGHI(K) * DTRAT               ACM.277
C              ENDIF                                                             ACM.278
95          CONTINUE                                                             ACM.279
90        CONTINUE                                                               ACM.280
C                                                                                ACM.281
          DO 100 K = 2,KLM                                                       ACM.282
            DO 105 I = 1,ILX                                                     ACM.283
C              IF(K.GT.KCBL(I)) THEN                                             ACM.284
                BI(I,K) = 1.0 - EI(I,K) - AI(I,K)                                ACM.285
C              ENDIF                                                             ACM.286
105         CONTINUE                                                             ACM.287
100       CONTINUE                                                               ACM.288
C                                                                                ACM.289
          DO 109 I = 1,ILX                                                       ACM.290
            BI(I,KCBL(I)) = 1.0 - EI(I,KCBL(I))                                  ACM.291
            BI(I,KL)  = 1.0 - AI(I,KL)                                           ACM.292
109       CONTINUE                                                               ACM.293
C                                                                                ACM.294
      DO 4000 NL = 1,NLP          ! SUB TIME LOOP IF KZ TOO BIG                  ACM.295
        DO 3000 L = 1,NSPX        ! loop for thetav, q, u and v                  ACM.296
          DO 110 K = 2,KLM                                                       ACM.297
            DO 115 I = 1,ILX                                                     ACM.298
C              IF(K.GT.KCBL(I)) THEN                                             ACM.299
                DI(I,K) = VCI(I,K,L) - (1.0 - CRANKP) * EI(I,K) /                ACM.300
     &              CRANKP * (VCI(I,K+1,L) - VCI(I,K,L))  +                      ACM.301
     &              (1.0 - CRANKP) * AI(I,K) / CRANKP *                          ACM.302
     &              (VCI(I,K,L) - VCI(I,K-1,L))                                  ACM.303
C              ENDIF                                                             ACM.304
115         CONTINUE                                                             ACM.305
110       CONTINUE                                                               ACM.306
C                                                                                ACM.307
                                                                                 ACM.308
          DO 116 I = 1,ILX                                                       ACM.309
            DI(I,KCBL(I)) = VCI(I,KCBL(I),L) - (1.0 - CRANKP) *                  ACM.310
     &             EI(I,KCBL(I)) / CRANKP *                                      ACM.311
     &             (VCI(I,KCBL(I)+1,L) - VCI(I,KCBL(I),L))+BCBOTN(I,L)           ACM.312
            DI(I,KL)  = VCI(I,KL,L) + (1.0 - CRANKP) *                           ACM.313
     &             AI(I,KL) / CRANKP *                                           ACM.314
     &             (VCI(I,KL,L) - VCI(I,KLM,L))                                  ACM.315
116       CONTINUE                                                               ACM.316
C                                                                                ACM.317
C.... SOLVE TRIDIAGONAL SYSTEM                                                   ACM.318
          DO 117 I = 1,ILX                                                       ACM.319
C            IF (BI(I,KCBL(I)) .EQ. 0.0) STOP 2345                               ACM.320
            BETA(I)  = BI(I,KCBL(I))                                             ACM.321
            UI(I,KCBL(I)) = DI(I,KCBL(I)) / BETA(I)                              ACM.322
117       CONTINUE                                                               ACM.323
C                                                                                ACM.324
C     ** DECOMPOSITION AND FORWARD SUBSTITUTION                                  ACM.325
          DO 120 K = 2,KL                                                        ACM.326
            DO 122 I = 1,ILX                                                     ACM.327
              IF(K.GT.KCBL(I)) THEN                                              ACM.328
                GAM(I,K) = EI(I,K-1) / BETA(I)                                   ACM.329
                BETA(I)  = BI(I,K) - AI(I,K) * GAM(I,K)                          ACM.330
              ENDIF                                                              ACM.331
122         CONTINUE                                                             ACM.332
            DO 125 I = 1,ILX                                                     ACM.333
              IF(K.GT.KCBL(I)) THEN                                              ACM.334
                UI(I,K)  = (DI(I,K) - AI(I,K) * UI(I,K-1)) / BETA(I)             ACM.335
              ENDIF                                                              ACM.336
125         CONTINUE                                                             ACM.337
120       CONTINUE                                                               ACM.338
C     ** BACK SUBSTITUTION                                                       ACM.339
          DO 130 K = KLM,1,-1                                                    ACM.340
            DO 135 I = 1,ILX                                                     ACM.341
C              IF (K.GE.KCBL(I)) THEN                                            ACM.342
                UI(I,K) = UI(I,K) - GAM(I,K+1) * UI(I,K+1)                       ACM.343
C              ENDIF                                                             ACM.344
135         CONTINUE                                                             ACM.345
130       CONTINUE                                                               ACM.346
C                                                                                ACM.347
C.... BACK TO VCI FOR CTM                                                        ACM.348
C                                                                                ACM.349
          DO 140 K = 1,KL                                                        ACM.350
            DO 145 I = 1,ILX                                                     ACM.351
              IF(K.GE.KCBL(I)) THEN                                              ACM.352
                VCI(I,K,L)= UI(I,K)                                              ACM.353
              ENDIF                                                              ACM.354
145         CONTINUE                                                             ACM.355
140       CONTINUE                                                               ACM.356
C**                                                                              ACM.357
3000    CONTINUE       ! END LOOP FOR THETAV, Q, U AND V                         ACM.358
4000  CONTINUE         ! END OF SUB-TIME LOOP                                    ACM.359
C2200  CONTINUE         ! END OF I LOOP                                          ACM.360
C                                                                                ACM.361
      DO 150 K = 1,KL                                                            ACM.362
      DO 150 I = 1,ILX                                                           ACM.363
        THETA(I,K) = VCI(I,K,1)                                                  ACM.364
        QVS(I,K)    = VCI(I,K,2)                                                 ACM.365
        US(I,K)     = VCI(I,K,3)                                                 ACM.366
        VS(I,K)     = VCI(I,K,4)                                                 ACM.367
150   CONTINUE                                                                   ACM.368
C                                                                                ACM.369
      IF (IMOISTX.NE.1.AND.IMOISTX.NE.3) THEN                                    ACM.370
        DO 155 K=1,KL                                                            ACM.371
        DO 155 I=1,ILX                                                           ACM.372
          QCS(I,K) = VCI(I,K,5)                                                  ACM.373
          QIS(I,K) = VCI(I,K,6)                                                  ACM.374
155      CONTINUE                                                                ACM.375
      ENDIF                                                                      ACM.376
                                                                                 ACM.377
C **                                                                             ACM.378
      RETURN                                                                     ACM.379
      END                                                                        ACM.380
