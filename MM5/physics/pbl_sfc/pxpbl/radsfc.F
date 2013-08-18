      SUBROUTINE RADSFC(IYY,J,INEST,TIMEG)                                       RADSFC.1
      IMPLICIT NONE                                                              RADSFC.2
C                                                                                RADSFC.3
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          RADSFC.4
C                                                                     C          RADSFC.5
C                                                                     C          RADSFC.6
C     THIS SUBROUTINE CALCULATES THE DOWNWARD SHORTWAVE (RG) AND      C          RADSFC.7
C     LONGWAVE (GLW) IRRADIANCES INCIDENT ON THE SURFACE.             C          RADSFC.8
C     AND THE SOLAR RADIATION ABSORBED BY THE SURFACE (GSW).          C          RADSFC.9
C     REVISED THE MM5'S SUBROUTINE SFCRAD.  -- AX                     C          RADSFC.10
C                                                                     C          RADSFC.11
C     IF ICLOUD  = 1, THEN THE EFFECT OF CLOUDS IS ACCOUNTED          C          05DEC01.1282
C     FOR IN CALCULATING THESE IRRADIANCES.                           C          RADSFC.13
C                                                                     C          RADSFC.14
C     THIS ROUTINE IS CALLED FROM PXPBL ONLY IF IFRAD LE 1            C          RADSFC.15
C                                                                     C          RADSFC.16
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          RADSFC.17
C                                                                                RADSFC.18
C..........  Include files                                                       RADSFC.19
#     include <parame.incl>                                                      RADSFC.20
#     include <param2.incl>                                                      RADSFC.21
#     include <param3.incl>                                                      RADSFC.22
#     include <pmoist.incl>                                                      RADSFC.23
#     include <point3d.incl>                                                     RADSFC.24
#     include <point2d.incl>                                                     RADSFC.25
#     include <various.incl>                                                     RADSFC.26
#     include <comicl.incl>                                                      RADSFC.27
#     include <soilp.incl>                                                       RADSFC.28
C                                                                                RADSFC.29
C----------------------------------------------------------------------          RADSFC.30
C                                                                                RADSFC.31
C.......Arguments                                                                RADSFC.32
      INTEGER IYY,J,INEST                                                        RADSFC.33
      REAL TIMEG                                                                 RADSFC.34
C..........  Local arrays                                                        RADSFC.35
      REAL CCOV (MIX,MKX), FRACSC (MIX)                                          RADSFC.36
      REAL FTABS  (MIX), FTSCA  (MIX), FBSC  (MIX),                              RADSFC.37
     1     FTABSD (MIX), FTSCAD (MIX), FBSCD (MIX),                              RADSFC.38
     2     PATH   (MIX), TACC   (MIX), TSCC  (MIX)                               RADSFC.39
      REAL TAC(3),TSC(3),CLW(3)                                                  RADSFC.40
      REAL CLDFR(MIX,3)                                                          RADSFC.41
C.......Local variables                                                          RADSFC.42
      INTEGER NOHIC,JJ,KCLMD,KCENT                                               RADSFC.43
      REAL RH,SG1,RHC                                                            RADSFC.44
      REAL PRWPT,EMATM,DEPCL,EPATCL,CPRWK,PRWSUM,SIGCENT,PCENT,TKTIR             RADSFC.45
      REAL DELHR,TGMT,ANGLAT,SUNCOL,ARG,ALBZ                                     RADSFC.46
      REAL BSCBAR,XC,TRANSMIS,FRAX                                               RADSFC.47
C.......Indexes                                                                  RADSFC.48
      INTEGER I,K,LC                                                             RADSFC.49
C-- for cloud                                                                    05DEC01.1283
      REAL CLOBOT,CMDBOT,CHIBOT,PSMB                                             05DEC01.1284
      DATA CLOBOT/970.0/   ! bottom bound pressure for low cloud                 05DEC01.1285
      DATA CMDBOT/800.0/   ! bottom bound pressure for middle cloud              05DEC01.1286
      DATA CHIBOT/450.0/   ! bottom bound pressure for high cloud                05DEC01.1287
C                                                                                RADSFC.51
C----------------------------------------------------------------------          RADSFC.52
C                                                                                RADSFC.53
C  tac is absorption transmissivity for high, middle and low cloud               RADSFC.54
      DATA TAC /0.98,0.85,0.80/                                                  RADSFC.55
C  tsc is scattering transmissivity for high, middle and low cloud               RADSFC.56
      DATA TSC /0.80,0.60,0.48/                                                  RADSFC.57
C  clw is enhancement coefficients on longwave radiation                         RADSFC.58
C  due to high, middle and low cloud                                             RADSFC.59
      DATA CLW /0.06,0.22,0.26/                                                  RADSFC.60
C                                                                                RADSFC.61
C----------------------------------------------------------------------          RADSFC.62
C-----NOTE THAT THE TAC COEFFICIENTS HAVE BEEN CHANGED SLIGHTLY                  RADSFC.63
C     FROM THOSE IN MM3, AS HAVE THE MINIMUM TRANSMISSIONS (TACC,TSCC).          RADSFC.64
C     THE MAXIMUM CLOUD FRACTION HAS ALSO BEEN CHANGED FROM                      RADSFC.65
C     80 TO 100%.  THE TRANSMISSION CHANGES ARE IN ACCORDANCE WITH               RADSFC.66
C     DRUMMOND AND HICKEY (1971), RATHER THAN THE INTERPRETED VALUES             RADSFC.67
C     OF DRUMMOND AND HICKEY BY WETHERALD AND MANABE (1980) WHICH                RADSFC.68
C     HAD BEEN PREVIOUSLY USED.                                                  RADSFC.69
C                                                                                RADSFC.70
      IF (ITGFLG(INEST) .NE. 1) GO TO 220                                        RADSFC.71
C                                                                                RADSFC.72
C..........  determine the sigma level for high, middle and low cloud            05DEC01.1288
      DO 192 I = 1,ILX                                                           05DEC01.1289
        KCLOUD(I,1) = 0                                                          05DEC01.1290
        KCLOUD(I,2) = 0                                                          05DEC01.1291
        KCLOUD(I,3) = 0                                                          05DEC01.1292
192   CONTINUE                                                                   05DEC01.1293
C                                                                                05DEC01.1294
      DO 193 K = 1,KL                                                            05DEC01.1295
      DO 193 I = 1,ILX                                                           05DEC01.1296
        PSMB = P(I,K) * 10.0                                                     05DEC01.1297
        IF ( PSMB .LE. CLOBOT .AND. KCLOUD(I,3) .EQ. 0) THEN                     05DEC01.1298
          KCLOUD(I,3) = K                                                        05DEC01.1299
         ELSE IF( PSMB .LE. CMDBOT .AND. KCLOUD(I,2) .EQ. 0) THEN                05DEC01.1300
          KCLOUD(I,2) = K                                                        05DEC01.1301
         ELSE IF( PSMB .LE. CHIBOT .AND. KCLOUD(I,1) .EQ. 0) THEN                05DEC01.1302
          KCLOUD(I,1) = K                                                        05DEC01.1303
        ENDIF                                                                    05DEC01.1304
193   CONTINUE                                                                   05DEC01.1305
C                                                                                05DEC01.1306
      DO 10 I = 1,ILX                                                            RADSFC.73
        CLDFR(I,1) = 0.0                                                         RADSFC.74
        CLDFR(I,2) = 0.0                                                         RADSFC.75
        CLDFR(I,3) = 0.0                                                         RADSFC.76
10    CONTINUE                                                                   RADSFC.77
C                                                                                RADSFC.78
      IF (ICLOUD(INEST) .NE. 0) THEN  ! consider cloud's effects                 RADSFC.79
      NOHIC=1                                                                    RADSFC.80
      JJ=1                                                                       RADSFC.81
C                                                                                RADSFC.82
C-----FIRST, DETERMINE LOW, MID, AND HIGH CLOUD FRACTION.                        RADSFC.83
C                                                                                RADSFC.84
C  DEFINE CRITICAL RH OF ALL LAYERS                                              RADSFC.85
                                                                                 RADSFC.86
        DO 31 K = 1,KL                                                           RADSFC.87
         DO 30 I = 1,ILX                                                         RADSFC.88
          RH = RHX(I,K)                                                          RADSFC.89
C... According to Geleyn et al 82, P/Ps, rather than sigma should be used        RADSFC.90
C... in RHC calculation, AX 6/95                                                 RADSFC.91
            SG1 = P(I,K) / (PSB(I,J) + PTOP)                                     RADSFC.92
            RHC = 1.0 - 2.0 * SG1 * (1.0 - SG1) *                                RADSFC.93
     &            (1.0 + 1.732 * (SG1 - 0.5))                                    RADSFC.94
C                                                                                RADSFC.95
C... set RHC to 90% in CBL - AX 10/95                                            RADSFC.96
          IF(ZF(I,K-1).LT.PBL(I,J).AND.NOCONV(I) .EQ.1)                          RADSFC.97
     >       RHC = MAX(0.90,RHC)                                                 RADSFC.98
          IF (RH .GT. RHC) THEN                                                  RADSFC.99
C..........  This is from Geleyn et al 82                                        RADSFC.100
            CCOV(I,K) = ((RH - RHC) / (1.0 - RHC)) ** 2                          RADSFC.101
           ELSE                                                                  RADSFC.102
            CCOV(I,K) = 0.0                                                      RADSFC.103
          ENDIF                                                                  RADSFC.104
          CCOV(I,K) = AMAX1(AMIN1(CCOV(I,K),1.0),0.0)                            RADSFC.105
30        CONTINUE                                                               RADSFC.106
31      CONTINUE                                                                 RADSFC.107
C                                                                                RADSFC.108
        DO 40 I = 1,ILX                                                          RADSFC.109
          KCLO = KCLOUD(I,3)                                                     RADSFC.110
          KCMD = KCLOUD(I,2)                                                     RADSFC.111
          KCHI = KCLOUD(I,1)                                                     RADSFC.112
          DO 40 K = KL,1,-1                                                      RADSFC.113
C... The following is based on maximum overlapping                               RADSFC.114
            IF (K .GE. KCLO .AND. K .LT. KCMD)                                   RADSFC.115
     &        CLDFR(I,3) = MAX(CCOV(I,K),CLDFR(I,3))                             RADSFC.116
            IF (K .GE. KCMD .AND. K .LT. KCHI)                                   RADSFC.117
     &        CLDFR(I,2) = MAX(CCOV(I,K),CLDFR(I,2))                             RADSFC.118
            IF (K .GE. KCHI)                                                     RADSFC.119
     &        CLDFR(I,1) = MAX(CCOV(I,K),CLDFR(I,1))                             RADSFC.120
C...                                                                             RADSFC.121
40      CONTINUE                                                                 RADSFC.122
                                                                                 RADSFC.123
        DO 50 LC = 1,3                                                           RADSFC.124
        DO 50 I = 1,ILX                                                          RADSFC.125
          CLDFR(I,LC) = AMIN1 (CLDFR(I,LC), 1.0)                                 RADSFC.126
          CLDFR(I,LC) = AMAX1 (CLDFR(I,LC), 0.0)                                 RADSFC.127
50      CONTINUE                                                                 RADSFC.128
                                                                                 RADSFC.129
      ENDIF               ! endif for (ICLOUD .NE. 0)                            RADSFC.130
C                                                                                RADSFC.131
C-----CALCULATE PRECIPITABLE WATER IN CENTIMETERS                                RADSFC.132
C                                                                                RADSFC.133
      IF (IDRY(INEST) .NE. 1) THEN                                               RADSFC.134
        IF (IMOIST(INEST) .NE. 0) THEN                                           RADSFC.135
C                                                                                RADSFC.136
          DO 80 I = 1,ILX                                                        RADSFC.137
            PRW(I,J) = 0.0                                                       RADSFC.138
80        CONTINUE                                                               RADSFC.139
C                                                                                RADSFC.140
          DO 91 K = 1,KL                                                         RADSFC.141
            DO 90 I = 1,ILX                                                      RADSFC.142
              PRW(I,J) = PRW(I,J) - 100.0 * QVS(I,K) * PSB(I,J)                  RADSFC.143
     &                   * DSIGH(K) / G                                          RADSFC.144
90          CONTINUE                                                             RADSFC.145
91        CONTINUE                                                               RADSFC.146
        ENDIF                                                                    RADSFC.147
      ENDIF                                                                      RADSFC.148
C                                                                                RADSFC.149
C-----CALCULATE LONGWAVE IRRADIANCE INCIDENT ON GROUND - GLW                     RADSFC.150
C                                                                                RADSFC.151
C.....MONTEITH (1961) EQUATION FOR 'EFFECTIVE EMISSIVITY' FROM                   RADSFC.152
C     ATMOSPHERE USED BELOW TO CALCULATE IR DOWN.                                RADSFC.153
C                                                                                RADSFC.154
C.....ADD EFFECTS OF CLOUDS. NOTE THAT IF ICLOUDS=0, DEPCL=1                     RADSFC.155
C     DEPCL = FACTOR FOR EMISSIVITY CHANGE (INCREASE OF IR DOWN)                 RADSFC.156
C             DUE TO CLOUDS.                                                     RADSFC.157
C             BERLAND AND BERLAND (1952) FROM KONDRATYEV (1969)                  RADSFC.158
C                                                                                RADSFC.159
      DO 110 I = 1,ILX                                                           RADSFC.160
        PRWPT = AMAX1 (PRW(I,J), 0.1)                                            RADSFC.161
        EMATM = 0.725 + 0.17 * ALOG10(PRWPT)                                     RADSFC.162
        IF(ICLOUD(INEST).NE.0) THEN                                              05DEC01.1307
           DEPCL = 1.0 + CLW(1) * CLDFR(I,1) +                                   05DEC01.1308
     &             CLW(2) * CLDFR(I,2) + CLW(3) * CLDFR(I,3)                     05DEC01.1309
        ELSE                                                                     05DEC01.1310
           DEPCL = 1.0                                                           05DEC01.1311
        END IF                                                                   05DEC01.1312
        EPATCL = EMATM * DEPCL                                                   RADSFC.165
        EPATCL = AMIN1 (EPATCL, 1.0)                                             RADSFC.166
C DEFINE KTIR AS A FUNCTION OF PRECIPITABLE WATER: NLS/DRS SEPT 1991             RADSFC.167
C Determine the centroid of the precipitable water                               RADSFC.168
C                                                                                RADSFC.169
C      CPRWK=PRWTOT/2.                                                           RADSFC.170
        CPRWK = PRW(I,J) * 0.5        ! half total precip. water                 RADSFC.171
        PRWSUM = 0.0                                                             RADSFC.172
        DO 121 K = 1,KL                                                          RADSFC.173
C        PRWSUM=PRWSUM+PRWK(K)                                                   RADSFC.174
C NOTE:  PRWK(K)=100.*QVB(I,J,K)*DSIGMA(K)/G                                     RADSFC.175
          PRWSUM = PRWSUM - 100.0 * QVS(I,K) * PSB(I,J) * DSIGH(K) / G           RADSFC.176
          IF (PRWSUM .GE. CPRWK) THEN                                            RADSFC.177
            KCENT=K                                                              RADSFC.178
            GOTO 122                                                             RADSFC.179
          END IF                                                                 RADSFC.180
121     CONTINUE                                                                 RADSFC.181
122     CONTINUE                                                                 RADSFC.182
        SIGCENT = SIGMAH(KCENT)                                                  RADSFC.183
        PCENT = 10.0 * (SIGCENT * PSB(I,J) + PTOP)                               RADSFC.184
        KTIR = KCENT                                                             RADSFC.185
        TKTIR = TT(I,KTIR)                                                       RADSFC.186
        GLW(I,J) = EPATCL * STBOLT * TKTIR**4                                    RADSFC.187
110   CONTINUE                                                                   RADSFC.188
C                                                                                RADSFC.189
C-----CALCULATE SHORTWAVE IRRADIANCE INCIDENT ON GROUND - GSW                    RADSFC.190
C     GSW = SW FLUX ON HORIZONTAL SURFACE, ACCOUNTING FOR                        RADSFC.191
C              INTERNAL REFLECTIONS AND ABSORPTIONS                              RADSFC.192
C                                                                                RADSFC.193
C  *** DETERMINE INSOLATION AT GIVEN GRID POINT FROM SOLAR ANGLE FORMULA         RADSFC.194
C                                                                                RADSFC.195
      DO 120 I = 1,ILX                                                           RADSFC.196
C-- LOCAL HOUR ANGLE                                                             RADSFC.197
C  THIS IS SUPPOSED TO BE THE TIME IN GMT AND LONG. SHOULD BE -                  RADSFC.198
C  FOR NORTH AMERICA                                                             RADSFC.199
        DELHR = TIMEG + XLONG(I,J) /15.0 - 12.0                                  RADSFC.200
        IF (ABS(DELHR) .GT. 12.0) THEN                                           RADSFC.201
          DELHR = DELHR - SIGN(24.,DELHR)                                        RADSFC.202
        ENDIF                                                                    RADSFC.203
        TGMT = DELHR * 15.0 * DEGRAD                                             RADSFC.204
C                                                                                RADSFC.205
C-- LATITUDE ANGLE (RAD)                                                         RADSFC.206
        ANGLAT = XLAT(I,J)*DEGRAD                                                RADSFC.207
C                                                                                RADSFC.208
C-- Fraction of solar constant                                                   RADSFC.209
        FRACSC(I) = SIN(ANGLAT) * SIN(DECLIN) + COS(ANGLAT) *                    RADSFC.210
     &           COS(DECLIN) * COS(TGMT)                                         RADSFC.211
        FRACSC(I) = AMAX1(0.,FRACSC(I))                                          RADSFC.212
120   CONTINUE                                                                   RADSFC.213
C                                                                                RADSFC.214
C ***     DETERMINE IF SUN IS ABOVE HORIZON AT ANY POINTS IN STRIP               RADSFC.215
      SUNCOL = 0.0                                                               RADSFC.216
      DO 130 I = 1,ILX                                                           RADSFC.217
        SUNCOL = AMAX1(SUNCOL,FRACSC(I))                                         RADSFC.218
        GSW(I,J) = 0.0                                                           RADSFC.219
        RG(I) = 0.0                                                              RADSFC.220
130   CONTINUE                                                                   RADSFC.221
C                                                                                RADSFC.222
C ***     IF SUN IS NOT UP AT ANY POINT IN STRIP, SKIP CALCULATING               RADSFC.223
C      SHORTWAVE IRRADIANCE                                                      RADSFC.224
      IF (SUNCOL .NE. 0.0) THEN                                                  RADSFC.225
C                                                                                RADSFC.226
        DO 150 I = 1,ILX                                                         RADSFC.227
          ARG = 0.003286 * (ACOS(FRACSC(I)) / DEGRAD) ** 1.5                     RADSFC.228
          ALBZ = 0.01 * (EXP(ARG) - 1.0)                                         RADSFC.229
          ALBEDO(I,J) = ALBZ + ALB(I,J)                                          RADSFC.230
C                                                                                RADSFC.231
          PATH(I) = 1.0 / AMAX1(FRACSC(I),0.00005)                               RADSFC.232
150     CONTINUE                                                                 RADSFC.233
C                                                                                RADSFC.234
C-----SW TRANSMISSIVITY DETERMINED FROM LOOK-UP TABLE FROM                       RADSFC.235
C     SHORTWAVE MODEL OF CARLSON AND BOLAND (JAM, 1978)                          RADSFC.236
C                                                                                RADSFC.237
C.....DIRECT RADIATION : USE ACTUAL SOLAR PATH LENGTH                            RADSFC.238
C                                                                                RADSFC.239
        CALL TRANSM(PATH, PRW, FTABS, FTSCA, FBSC, PSB, J, IYY)                  RADSFC.240
C                                                                                RADSFC.241
C.....DIFFUSE RADIATION : USE PATH LENGTH (DIFFUSIVITY FACTOR)=1.67              RADSFC.242
C     FROM RODGERS AND WALSHAW,1967                                              RADSFC.243
C                                                                                RADSFC.244
        DO 170 I = 1,ILX                                                         RADSFC.245
          PATH(I) = 1.67                                                         RADSFC.246
170     CONTINUE                                                                 RADSFC.247
C                                                                                RADSFC.248
        CALL TRANSM(PATH, PRW, FTABSD,FTSCAD,FBSCD,PSB, J, IYY)                  RADSFC.249
C                                                                                RADSFC.250
        DO 180 I = 1,ILX                                                         RADSFC.251
          TACC(I) = 1.0                                                          RADSFC.252
          TSCC(I)= 1.0                                                           RADSFC.253
180     CONTINUE                                                                 RADSFC.254
C                                                                                RADSFC.255
        IF (ICLOUD(INEST) .GE. 1) THEN                                           05DEC01.1313
          DO 191 LC = 1,3                                                        RADSFC.257
            DO 190 I  = 1,ILX                                                    RADSFC.258
              TACC(I) = TACC(I) * (1.0 - ((1.0 - TAC(LC)) *                      RADSFC.259
     &                  CLDFR(I,LC)))                                            RADSFC.260
              TSCC(I) = TSCC(I) * (1.0 - ((1.0 - TSC(LC)) *                      RADSFC.261
     &                  CLDFR(I,LC)))                                            RADSFC.262
190         CONTINUE                                                             RADSFC.263
191       CONTINUE                                                               RADSFC.264
C                                                                                RADSFC.265
C.....MINIMUM ABSORPTION AND SCATTERING TRANSMISSIVITIES THROUGH CLOUD           RADSFC.266
C     FROM DRUMMOND AND HICKEY (1971)                                            RADSFC.267
C                                                                                RADSFC.268
          DO 200 I  = 1,ILX                                                      RADSFC.269
            TACC(I) = AMAX1 (TACC(I),0.70)                                       RADSFC.270
            TSCC(I) = AMAX1 (TSCC(I),0.44)                                       RADSFC.271
200       CONTINUE                                                               RADSFC.272
        ENDIF                                                                    RADSFC.273
C                                                                                RADSFC.274
C.....BSCBAR = MEAN BACKSCATTERING COEFFICIENT, INCLUDING EFFECT OF              RADSFC.275
C     CLOUDS.                                                                    RADSFC.276
C                                                                                RADSFC.277
        DO 210 I = 1,ILX                                                         RADSFC.278
          BSCBAR   = (FBSCD(I)*(1.-FTSCAD(I))+(1.-TSCC(I))) /                    RADSFC.279
     &               ((1.-FTSCAD(I))+(1.-TSCC(I)))                               RADSFC.280
          XC       = BSCBAR * (1.0 - FTSCAD(I) * TSCC(I)) *                      RADSFC.281
     &               FTABSD(I) * TACC(I)                                         RADSFC.282
          TRANSMIS = TACC(I) * TSCC(I) * FTABS(I) * (FTSCA(I) +                  RADSFC.283
     &               (1.0 - FTSCA(I)) * (1.0 - FBSC(I))) /                       RADSFC.284
     &               (1.0 - XC * ALBEDO(I,J))                                    RADSFC.285
          IF (FRACSC(I)-0.0001 .GE. 0) THEN                                      RADSFC.286
            FRAX   = FRACSC(I)                                                   RADSFC.287
           ELSE                                                                  RADSFC.288
            FRAX   = 0.0                                                         RADSFC.289
          ENDIF                                                                  RADSFC.290
C  rg is incoming solar radiation                                                RADSFC.291
          RG(I) = 1370.0 * TRANSMIS * SQADR * FRAX                               RADSFC.292
          GSW(I,J) = (1.0 - ALBEDO(I,J)) * RG(I)                                 RADSFC.293
210     CONTINUE                                                                 RADSFC.294
      ENDIF          ! endif for (SUNCOL.NE.0)                                   RADSFC.295
                                                                                 RADSFC.296
CAX      ENDIF                                                                   RADSFC.297
220   CONTINUE                                                                   RADSFC.298
C                                                                                RADSFC.299
      RETURN                                                                     RADSFC.300
      END                                                                        RADSFC.301
