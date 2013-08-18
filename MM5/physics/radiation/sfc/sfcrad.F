#include <defines.incl>                                                          07NOV00.2472
#ifndef MPP1_COLUMNWISE                                                          SFCRAD.1
#  ifndef VECTOR                                                                 07NOV00.2473
      SUBROUTINE SFCRAD (IYY,JXX,J,INEST,T3D,QV3D,PP3D)                          SFCRAD.2
#  else                                                                          07NOV00.2474
      SUBROUTINE SFCRAD (IYY,JXX,J,INEST,T3D,QV3D,PP3D,                          07NOV00.2475
     +   SVP1,SVP2,SVP3,SVPT0,EP2,                                               07NOV00.2476
     +   PSB,GLW,GSW,CAPG,MAVAIL,XLAT,XLONG,                                     07NOV00.2477
     +   PRW,ALB,SHC,THC,                                                        07NOV00.2478
     +   IST,IEN)                                                                07NOV00.2479
#  endif                                                                         07NOV00.2480
#else                                                                            SFCRAD.3
#  undef VECTOR                                                                  05DEC01.1511
      SUBROUTINE SFCRAD_COL (IYY,JXX,I,J,INEST,T3D,QV3D,PP3D)                    SFCRAD.4
#endif                                                                           SFCRAD.5
      IMPLICIT NONE                                                              SFCRAD.6
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          SFCRAD.7
C                                                                     C          SFCRAD.8
C                                                                     C          SFCRAD.9
C     THIS SUBROUTINE CALCULATES THE DOWNWARD SHORTWAVE (GSW) AND     C          SFCRAD.10
C     LONGWAVE (GLW) IRRADIANCES INCIDENT ON THE SURFACE.             C          SFCRAD.11
C                                                                     C          SFCRAD.12
C     IF ICLOUD = 1, or 2 THEN THE EFFECT OF CLOUDS IS ACCOUNTED      C          05DEC01.1512
C     FOR IN CALCULATING THESE IRRADIANCES.                           C          SFCRAD.14
C                                                                     C          SFCRAD.15
C                                                                     C          SFCRAD.16
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          SFCRAD.17
#     include <parame.incl>                                                      SFCRAD.18
                                                                                 SFCRAD.20
C                                                                                SFCRAD.21
      REAL LAMDAG                                                                SFCRAD.22
      REAL TAC(3),TSC(3),CLW(3),LAMG(4)                                          SFCRAD.23
      REAL T3D(MIX,MJX,MKX),QV3D(MIX,MJX,MKX),PP3D(MIX,MJX,MKX)                  SFCRAD.24
#     include <varia.incl>                                                       SFCRAD.25
C                                                                                SFCRAD.26
      REAL      CLDFR(MIX,3),RHX(MIX,MKX)                                        SFCRAD.27
C                                                                                SFCRAD.28
      REAL      FRACSC(MIX)                                                      SFCRAD.29
C                                                                                SFCRAD.30
      REAL      FTABS (MIX), FTSCA (MIX), FBSC (MIX),                            SFCRAD.31
     1          FTABSD(MIX), FTSCAD(MIX), FBSCD(MIX),                            SFCRAD.32
     2          PATH  (MIX), TACC  (MIX), TSCC  (MIX)                            SFCRAD.33
                                                                                 SFCRAD.34
      INTEGER   IYY,JXX                                                          SFCRAD.35
      INTEGER   I,J,K,LC                                                         SFCRAD.36
      INTEGER   INEST,KCENT                                                      SFCRAD.37
      REAL      CSW,PS,VAPPRS,QSBT,CPRWK,PRWSUM,PRWPT,EMATM,DEPCL                SFCRAD.38
      REAL      EPATCL,TKTIR,XT24,TLOCAP,OMEGA,XXLAT,SUNCOL,BSCBAR               SFCRAD.39
      REAL      XSER,HI,SFLUX,FRAX,CS,RHOG1,RHOG2,RHOG3                          SFCRAD.40
#     include <param2.incl>                                                      SFCRAD.41
#     include <param3.incl>                                                      SFCRAD.42
#ifndef VECTOR                                                                   07NOV00.2481
#     include <pmoist.incl>                                                      SFCRAD.43
#     include <point2d.incl>                                                     SFCRAD.45
#     include <addrcu.incl>                                                      07NOV00.2482
#else                                                                            07NOV00.2483
      REAL SVP1,SVP2,SVP3,SVPT0,EP2                                              07NOV00.2484
      REAL PSB(MIX,MJX),GLW(MIX,MJX),GSW(MIX,MJX),CAPG(MIX,MJX)                  07NOV00.2485
      REAL MAVAIL(MIX,MJX),XLAT(MIX,MJX),XLONG(MIX,MJX)                          07NOV00.2486
      REAL PRW(MIX,MJX),ALB(MIX,MJX),SHC(MIX,MJX),THC(MIX,MJX)                   07NOV00.2487
      INTEGER IST,IEN                                                            07NOV00.2488
#endif                                                                           07NOV00.2489
#     include <various.incl>                                                     SFCRAD.46
C                                                                                SFCRAD.47
C----------------------------------------------------------------------          SFCRAD.49
C                                                                                SFCRAD.50
      DATA TAC /0.98,0.85,0.80/                                                  SFCRAD.51
      DATA TSC /0.80,0.60,0.48/                                                  SFCRAD.52
      DATA CLW /0.06,0.22,0.26/                                                  SFCRAD.53
C                                                                                SFCRAD.54
C                                                                                SFCRAD.55
      DATA CSW/4.183E6/                                                          SFCRAD.56
      DATA LAMG/1.407E-8, -1.455E-5, 6.290E-3, 0.16857/                          SFCRAD.57
C                                                                                SFCRAD.58
C----------------------------------------------------------------------          SFCRAD.59
C-----NOTE THAT THE TAC COEFFICIENTS HAVE BEEN CHANGED SLIGHTLY                  SFCRAD.60
C     FROM THOSE IN MM3, AS HAVE THE MINIMUM TRANSMISSIONS (TACC,TSCC).          SFCRAD.61
C     THE MAXIMUM CLOUD FRACTION HAS ALSO BEEN CHANGED FROM                      SFCRAD.62
C     80 TO 100%.  THE TRANSMISSION CHANGES ARE IN ACCORDANCE WITH               SFCRAD.63
C     DRUMMOND AND HICKEY (1971), RATHER THAN THE INTERPRETED VALUES             SFCRAD.64
C     OF DRUMMOND AND HICKEY BY WETHERALD AND MANABE (1980) WHICH                SFCRAD.65
C     HAD BEEN PREVIOUSLY USED.                                                  SFCRAD.66
C                                                                                SFCRAD.67
      IF(ITGFLG(INEST).NE.1)GOTO 220                                             SFCRAD.68
C   LONGWAVE AND SHORTWAVE RADIATIVE FLUX ON GROUND WERE CALCULATED              SFCRAD.69
C   BY JIMY DUDHIA`S RADIATION SCHEME:                                           SFCRAD.70
C                                                                                SFCRAD.71
      IF(IFRAD.GE.2)GOTO 220                                                     SFCRAD.72
C                                                                                SFCRAD.73
C                                                                                SFCRAD.74
      DO 10 I=1,ILX                                                              SFCRAD.75
        CLDFR(I,1)=0.                                                            SFCRAD.76
        CLDFR(I,2)=0.                                                            SFCRAD.77
        CLDFR(I,3)=0.                                                            SFCRAD.78
   10 CONTINUE                                                                   SFCRAD.79
C                                                                                SFCRAD.80
      IF(ICLOUD(INEST).EQ.0)GOTO 70                                              SFCRAD.81
C                                                                                SFCRAD.82
C-----FIRST, DETERMINE LOW, MID, AND HIGH CLOUD FRACTION.                        SFCRAD.83
C                                                                                SFCRAD.84
      DO 20 K=1,KL                                                               SFCRAD.85
        DO 20 I=1,ILX                                                            SFCRAD.86
          PS=PSB(I,J)*A(K)+PTOP+PP3D(I,J,K)*0.001                                SFCRAD.87
          VAPPRS=SVP1*                                                           SFCRAD.88
     +           EXP(SVP2*(T3D(I,J,K)-SVPT0)/(T3D(I,J,K)-SVP3))                  SFCRAD.89
          QSBT=EP2*VAPPRS/(PS-VAPPRS)                                            SFCRAD.90
          RHX(I,K)=QV3D(I,J,K)/QSBT                                              SFCRAD.91
   20   CONTINUE                                                                 SFCRAD.92
                                                                                 SFCRAD.93
C                                                                                SFCRAD.94
      DO 40 K=1,KL                                                               SFCRAD.95
        DO 40 I=1,ILX                                                            SFCRAD.96
          IF(K.LE.KCLO.AND.K.GT.KCMD)CLDFR(I,3)=                                 SFCRAD.97
     +      AMAX1(RHX(I,K),CLDFR(I,3))                                           SFCRAD.98
          IF(K.LE.KCMD.AND.K.GT.KCHI)CLDFR(I,2)=                                 SFCRAD.99
     +      AMAX1(RHX(I,K),CLDFR(I,2))                                           SFCRAD.100
          IF(K.LE.KCHI)CLDFR(I,1)=AMAX1(RHX(I,K),CLDFR(I,1))                     SFCRAD.101
   40   CONTINUE                                                                 SFCRAD.102
                                                                                 SFCRAD.103
C                                                                                SFCRAD.104
C-----STORE CLOUD FRACTION IN CLDFR BELOW                                        SFCRAD.105
C                                                                                SFCRAD.106
      DO 50 I=1,ILX                                                              SFCRAD.107
        CLDFR(I,3)=4.0*CLDFR(I,3)-3.0                                            SFCRAD.108
        CLDFR(I,2)=4.0*CLDFR(I,2)-3.0                                            SFCRAD.109
        CLDFR(I,1)=2.5*CLDFR(I,1)-1.5                                            SFCRAD.110
   50 CONTINUE                                                                   SFCRAD.111
C                                                                                SFCRAD.112
      DO 60 LC=1,3                                                               SFCRAD.113
        DO 60 I=1,ILX                                                            SFCRAD.114
          CLDFR(I,LC)=AMIN1(CLDFR(I,LC),1.0)                                     SFCRAD.115
          CLDFR(I,LC)=AMAX1(CLDFR(I,LC),0.0)                                     SFCRAD.116
   60   CONTINUE                                                                 SFCRAD.117
                                                                                 SFCRAD.118
C                                                                                SFCRAD.119
C-----CALCULATION OF CLOUD FRACTION COMPLETE                                     SFCRAD.120
C                                                                                SFCRAD.121
   70 CONTINUE                                                                   SFCRAD.122
C                                                                                SFCRAD.123
C-----CALCULATE PRECIPITABLE WATER IN CENTIMETERS                                SFCRAD.124
C                                                                                SFCRAD.125
C                                                                                SFCRAD.126
      DO 80 I=1,ILX                                                              SFCRAD.127
        PRW(I,J)=0.                                                              SFCRAD.128
   80 CONTINUE                                                                   SFCRAD.129
C                                                                                SFCRAD.130
      DO 90 K=1,KL                                                               SFCRAD.131
        DO 90 I=1,ILX                                                            SFCRAD.132
          PRW(I,J)=PRW(I,J)+100.*PSB(I,J)*QV3D(I,J,K)*DSIGMA(K)/G                SFCRAD.133
   90   CONTINUE                                                                 SFCRAD.134
                                                                                 SFCRAD.135
  100 CONTINUE                                                                   SFCRAD.136
C                                                                                SFCRAD.137
C-----CALCULATE LONGWAVE IRRADIANCE INCIDENT ON GROUND - GLW                     SFCRAD.138
C                                                                                SFCRAD.139
C.....MONTEITH (1961) EQUATION FOR 'EFFECTIVE EMISSIVITY' FROM                   SFCRAD.140
C     ATMOSPHERE USED BELOW TO CALCULATE IR DOWN.                                SFCRAD.141
C                                                                                SFCRAD.142
C.....ADD EFFECTS OF CLOUDS. NOTE THAT IF ICLOUD=0, DEPCL=1                      SFCRAD.143
C     DEPCL = FACTOR FOR EMISSIVITY CHANGE (INCREASE OF IR DOWN)                 SFCRAD.144
C             DUE TO CLOUDS.                                                     SFCRAD.145
C             BERLAND AND BERLAND (1952) FROM KONDRATYEV (1969)                  SFCRAD.146
C                                                                                SFCRAD.147
C DEFINE KTIR AS A FUNCTION OF PRECIPITABLE WATER: NLS/DRS SEPT 1991             SFCRAD.148
C DETERMINE THE CENTROID OF THE PRECIPITABLE WATER TO CALCULATE                  SFCRAD.149
C DOWNWARD LONGWAVE FLUX                                                         SFCRAD.150
C                                                                                SFCRAD.151
      DO 110 I=1,ILX                                                             SFCRAD.152
      CPRWK=PRW(I,J)/2.                                                          SFCRAD.153
      PRWSUM=0.0                                                                 SFCRAD.154
      KCENT=1                                                                    SFCRAD.155
      DO 121 K=KL,1,-1                                                           SFCRAD.156
        PRWSUM=PRWSUM+100.*PSB(I,J)*QV3D(I,J,K)*DSIGMA(K)/G                      SFCRAD.157
        IF(PRWSUM.GE.CPRWK)THEN                                                  SFCRAD.158
          KCENT=K                                                                SFCRAD.159
          GOTO 122                                                               SFCRAD.160
        ENDIF                                                                    SFCRAD.161
  121 CONTINUE                                                                   SFCRAD.162
  122 CONTINUE                                                                   SFCRAD.163
      KTIR=KCENT                                                                 SFCRAD.164
        PRWPT=AMAX1(PRW(I,J),0.1)                                                SFCRAD.165
        EMATM=0.725+0.17*ALOG10(PRWPT)                                           SFCRAD.166
        IF(ICLOUD(INEST).NE.0) THEN                                              05DEC01.1513
           DEPCL=1.+CLW(1)*CLDFR(I,1)+CLW(2)*CLDFR(I,2)+                         05DEC01.1514
     +           CLW(3)*CLDFR(I,3)                                               05DEC01.1515
        ELSE                                                                     05DEC01.1516
           DEPCL=1.                                                              05DEC01.1517
        END IF                                                                   05DEC01.1518
        EPATCL=EMATM*DEPCL                                                       SFCRAD.169
        EPATCL=AMIN1(EPATCL,1.0)                                                 SFCRAD.170
        TKTIR=T3D(I,J,KTIR)                                                      SFCRAD.171
        GLW(I,J)=EPATCL*STBOLT*TKTIR**4                                          SFCRAD.172
  110 CONTINUE                                                                   SFCRAD.173
C                                                                                SFCRAD.174
C-----CALCULATE SHORTWAVE IRRADIANCE INCIDENT ON GROUND - GSW                    SFCRAD.175
C     GSW = SW FLUX ON HORIZONTAL SURFACE, ACCOUNTING FOR                        SFCRAD.176
C           INTERNAL REFLECTIONS AND ABSORPTIONS                                 SFCRAD.177
C                                                                                SFCRAD.178
      XT24=AMOD(XTIME+RADFRQ*0.5,1440.)                                          SFCRAD.179
      DO 120 I=1,ILX                                                             SFCRAD.180
        TLOCAP=GMT+XT24/60.+XLONG(I,J)/15.                                       SFCRAD.181
        TLOCAP=AMOD(TLOCAP+24.,24.)                                              SFCRAD.182
        OMEGA=15.*(TLOCAP-12.)*DEGRAD                                            SFCRAD.183
        XXLAT=XLAT(I,J)*DEGRAD                                                   SFCRAD.184
C.....FRACSC = FRACTION OF SOLAR CONSTANT                                        SFCRAD.185
        FRACSC(I)=SIN(DECLIN)*SIN(XXLAT)+COS(DECLIN)*COS(XXLAT)*                 SFCRAD.186
     +            COS(OMEGA)                                                     SFCRAD.187
        FRACSC(I)=AMAX1(0.,FRACSC(I))                                            SFCRAD.188
  120 CONTINUE                                                                   SFCRAD.189
C                                                                                SFCRAD.190
C-----DETERMINE IF SUN IS ABOVE HORIZON AT ANY POINTS IN STRIP                   SFCRAD.191
C                                                                                SFCRAD.192
      SUNCOL=0.0                                                                 SFCRAD.193
      DO 130 I=1,ILX                                                             SFCRAD.194
        SUNCOL=AMAX1(SUNCOL,FRACSC(I))                                           SFCRAD.195
  130 CONTINUE                                                                   SFCRAD.196
      DO 140 I=1,ILX                                                             SFCRAD.197
        GSW(I,J)=0.0                                                             SFCRAD.198
  140 CONTINUE                                                                   SFCRAD.199
C                                                                                SFCRAD.200
C-----IF SUN IS NOT UP AT ANY POINT IN STRIP, SKIP CALCULATING                   SFCRAD.201
C       SHORTWAVE IRRADIANCE                                                     SFCRAD.202
C                                                                                SFCRAD.203
#ifndef MPP1_COLUMNWISE                                                          SFCRAD.204
      IF(SUNCOL.EQ.0.)GOTO 220                                                   SFCRAD.205
#endif                                                                           SFCRAD.206
C                                                                                SFCRAD.207
      DO 150 I=1,ILX                                                             SFCRAD.208
        PATH(I)=1./AMAX1(FRACSC(I),0.00005)                                      SFCRAD.209
  150 CONTINUE                                                                   SFCRAD.210
C                                                                                SFCRAD.211
C-----SW TRANSMISSIVITY DETERMINED FROM LOOK-UP TABLE FROM                       SFCRAD.212
C     SHORTWAVE MODEL OF CARLSON AND BOLAND (JAM, 1978)                          SFCRAD.213
C                                                                                SFCRAD.214
C.....DIRECT RADIATION : USE ACTUAL SOLAR PATH LENGTH                            SFCRAD.215
C                                                                                SFCRAD.216
#ifndef MPP1_COLUMNWISE                                                          SFCRAD.217
      CALL TRANSM(PATH,PRW,FTABS,FTSCA,FBSC,PSB,J,IYY)                           SFCRAD.218
#else                                                                            SFCRAD.219
      CALL TRANSM_COL(PATH,PRW,FTABS,FTSCA,FBSC,PSB,I,J,IYY)                     SFCRAD.220
#endif                                                                           SFCRAD.221
C                                                                                SFCRAD.222
C.....DIFFUSE RADIATION : USE PATH LENGTH (DIFFUSIVITY FACTOR)=1.67              SFCRAD.223
C     FROM RODGERS AND WALSHAW,1967                                              SFCRAD.224
C                                                                                SFCRAD.225
      DO 160 I=1,ILX                                                             SFCRAD.226
        PATH(I)=1.67                                                             SFCRAD.227
  160 CONTINUE                                                                   SFCRAD.228
C                                                                                SFCRAD.229
#ifndef MPP1_COLUMNWISE                                                          SFCRAD.230
      CALL TRANSM(PATH,PRW,FTABSD,FTSCAD,FBSCD,PSB,J,IYY)                        SFCRAD.231
#else                                                                            SFCRAD.232
      CALL TRANSM_COL(PATH,PRW,FTABSD,FTSCAD,FBSCD,PSB,I,J,IYY)                  SFCRAD.233
#endif                                                                           SFCRAD.234
C                                                                                SFCRAD.235
      DO 170 I=1,ILX                                                             SFCRAD.236
        TACC(I)=1.                                                               SFCRAD.237
        TSCC(I)=1.                                                               SFCRAD.238
  170 CONTINUE                                                                   SFCRAD.239
C                                                                                SFCRAD.240
      IF(ICLOUD(INEST).EQ.0)GOTO 200                                             05DEC01.1519
C                                                                                SFCRAD.242
      DO 180 LC=1,3                                                              SFCRAD.243
        DO 180 I=1,ILX                                                           SFCRAD.244
          TACC(I)=TACC(I)*(1.-((1.-TAC(LC))*CLDFR(I,LC)))                        SFCRAD.245
          TSCC(I)=TSCC(I)*(1.-((1.-TSC(LC))*CLDFR(I,LC)))                        SFCRAD.246
  180   CONTINUE                                                                 SFCRAD.247
                                                                                 SFCRAD.248
C                                                                                SFCRAD.249
C.....MINIMUM ABSORPTION AND SCATTERING TRANSMISSIVITIES THROUGH CLOUD           SFCRAD.250
C     FROM DRUMMOND AND HICKEY (1971)                                            SFCRAD.251
C                                                                                SFCRAD.252
      DO 190 I=1,ILX                                                             SFCRAD.253
        TACC(I)=AMAX1(TACC(I),0.70)                                              SFCRAD.254
        TSCC(I)=AMAX1(TSCC(I),0.44)                                              SFCRAD.255
  190 CONTINUE                                                                   SFCRAD.256
  200 CONTINUE                                                                   SFCRAD.257
C                                                                                SFCRAD.258
C.....BSCBAR = MEAN BACKSCATTERING COEFFICIENT, INCLUDING EFFECT OF              SFCRAD.259
C     CLOUDS.                                                                    SFCRAD.260
C                                                                                SFCRAD.261
      DO 210 I=1,ILX                                                             SFCRAD.262
        BSCBAR=(FBSCD(I)*(1.-FTSCAD(I))+(1.-TSCC(I)))/((1.-FTSCAD(I))+(          SFCRAD.263
     +         1.-TSCC(I)))                                                      SFCRAD.264
        XSER=BSCBAR*ALB(I,J)*(1.-FTSCAD(I)*TSCC(I))*FTABSD(I)*TACC(I)            SFCRAD.265
        HI=SOLCON*FTABS(I)*TACC(I)*(FTSCA(I)+(1.-FTSCA(I))*(1.-FBSC(I)))         SFCRAD.266
        SFLUX=HI*TSCC(I)*(1.-ALB(I,J))/(1.-XSER)                                 SFCRAD.267
        IF(FRACSC(I)-0.0001.GE.0)THEN                                            SFCRAD.268
          FRAX=FRACSC(I)                                                         SFCRAD.269
        ELSE                                                                     SFCRAD.270
          FRAX=0.                                                                SFCRAD.271
        ENDIF                                                                    SFCRAD.272
  210 GSW(I,J)=SFLUX*FRAX                                                        SFCRAD.273
  220 CONTINUE                                                                   SFCRAD.274
C                                                                                SFCRAD.348
      RETURN                                                                     SFCRAD.350
      END                                                                        SFCRAD.351
