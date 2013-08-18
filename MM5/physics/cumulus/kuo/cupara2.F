      SUBROUTINE CUPARA2(J,IN,ISTART,IEND,QV3DTEN,T3DTEN,                        CUPARA2.1
     &                   T3D,QV3D,PP3D,QDOT,RAINC,PSB)                           CUPARA2.2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          CUPARA2.3
C                                                                     C          CUPARA2.4
C                                                                     C          CUPARA2.5
C     THIS SUBROUTINE PERFORMS CUMULUS PARAMETERIZATION SCHEME.       C          CUPARA2.6
C     THE BASIC METHOD FOLLOWS ANTHES AND KEYSER (1979) AND           C          CUPARA2.7
C     KUO (1983).                                                     C          CUPARA2.8
C                                                                     C          CUPARA2.9
C     IN = 1 : FOR LARGE DOMAIN                                       C          CUPARA2.10
C        = 2 : FOR NESTED DOMAIN                                      C          CUPARA2.11
C                                                                     C          CUPARA2.12
C     ALL THE OTHER ARGUMENTS ARE PASSED FROM SUBROUTINE "TEND" AND   C          CUPARA2.13
C     EXPLAINED IN "TEND".                                            C          CUPARA2.14
C                                                                     C          CUPARA2.15
C                                                                     C          CUPARA2.16
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          CUPARA2.17
                                                                                 07NOV00.1444
      IMPLICIT NONE                                                              07NOV00.1445
                                                                                 07NOV00.1446
#     include <parame.incl>                                                      CUPARA2.18
#     include <param2.incl>                                                      CUPARA2.19
#     include <param3.incl>                                                      CUPARA2.20
#     include <pmoist.incl>                                                      CUPARA2.21
#     include <various.incl>                                                     CUPARA2.22
#     include <rpstar.incl>                                                      CUPARA2.23
#     include <sum.incl>                                                         CUPARA2.24
#     include <parakcu.incl>                                                     CUPARA2.25
C                                                                                CUPARA2.26
C----------------------------------------------------------------------          CUPARA2.27
C                                                                                CUPARA2.28
C     PERT   : PERTURBATION TEMPERATURE                                          CUPARA2.29
C     PERQ   : PERTURBATION MIXING RATIO                                         CUPARA2.30
C     DLT    : TEMPERATURE DIFFERENCE USED TO ALLOW OVER SHOOTING.               CUPARA2.31
C     CDSCLD : CRITICAL CLOUD DEPTH IN DELTA SIGMA.                              CUPARA2.32
C                                                                                CUPARA2.33
      INTEGER J,IN,ISTART,IEND                                                   07NOV00.1447
      REAL QV3DTEN(MIX,MJX,MKX), T3DTEN(MIX,MJX,MKX)                             07NOV00.1448
      REAL QV3D(MIX,MJX,MKX), T3D(MIX,MJX,MKX)                                   07NOV00.1449
      REAL PP3D(MIX,MJX,MKX), QDOT(MIX,MJX,KXP1)                                 07NOV00.1450
      REAL RAINC(MIX,MJX), PSB(MIX,MJX)                                          07NOV00.1451
      REAL SCR3(MIX,MKX)                                                         07NOV00.1452
      REAL SEQT(MKX)                                                             07NOV00.1453
C                                                                                CUPARA2.40
      REAL PERT,PERQ,DLT,CDSCLD                                                  07NOV00.1454
      DATA PERT, PERQ/1.,1.E-3/                                                  CUPARA2.41
      DATA DLT, CDSCLD/3.0,0.3/                                                  CUPARA2.42
                                                                                 07NOV00.1455
      INTEGER I,K,KK                                                             07NOV00.1456
      INTEGER KBASE,KTOP                                                         07NOV00.1457
      REAL SCA,EQTM,TTP,Q,PSG,T1,EQT                                             07NOV00.1458
      REAL TMAX,QMAX,PMAX,EMAX,TDMAX,TMEAN                                       07NOV00.1459
      REAL DALR,DPLR,ZLCL,TLCL,PLCL,DLNP,SIGLCL                                  07NOV00.1460
      REAL ES,QS,DEQT,DSC,TTSUM,SUM,SUMB,ARH                                     07NOV00.1461
      REAL PSX,PUX,E1,RH,XSAV,C301                                               07NOV00.1462
      REAL TTCONV,APCNT,EDDYF                                                    07NOV00.1463
      REAL EE,TDPT                                                               05DEC01.700
C                                                                                CUPARA2.43
C                                                                                CUPARA2.44
C-----COMPUTE THE MOISTURE CONVERGENCE IN A COLUMN:                              CUPARA2.45
C     AT THIS STAGE, QV3DTEN(I,J,K) ONLY INCLUDES HORIZONTAL ADVECTION.          CUPARA2.46
C     SCA: IS THE AMOUNT OF TOTAL MOISTURE CONVERGENCE                           CUPARA2.47
C                                                                                CUPARA2.48
      DO 100 I=ISTART,IEND                                                       CUPARA2.49
        SCA=0.0                                                                  CUPARA2.50
        DO 20 K=1,KL                                                             CUPARA2.51
          SCA=SCA+QV3DTEN(I,J,K)*DSIGMA(K)                                       CUPARA2.52
   20   CONTINUE                                                                 CUPARA2.53
C                                                                                CUPARA2.54
C-----DETERMINE IF MOIST CONVECTION EXISTS:                                      CUPARA2.55
C                                                                                CUPARA2.56
        IF(SCA.LT.QDCRIT)GOTO 70                                                 CUPARA2.57
C                                                                                CUPARA2.58
        IF(ICUSTB.EQ.0)GOTO 290                                                  CUPARA2.59
C                                                                                CUPARA2.60
C-----CHECK FOR STABILITY                                                        CUPARA2.61
C                                                                                CUPARA2.62
C--1--COMPUTE EQT (EQUIVALENT POTENTIAL TEMPERATURE)                             CUPARA2.63
C     BETWEEN SURFACE AND 700 MB, WITH PERTURBATION TEMPERATURE                  CUPARA2.64
C     AND MOISTURE ADDED. THE MAXIMUM EQT WILL BE REGARDED                       CUPARA2.65
C     AS THE ORIGIN OF AIR PARCEL THAT PRODUCE CLOUD.                            CUPARA2.66
C                                                                                CUPARA2.67
        EQTM=0.0                                                                 CUPARA2.68
        DO 210 K=K700,KL                                                         CUPARA2.69
          TTP=T3D(I,J,K)+PERT                                                    CUPARA2.70
          Q=QV3D(I,J,K)+PERQ                                                     CUPARA2.71
          PSG=PSB(I,J)*A(K)+PTOP+PP3D(I,J,K)*0.001                               CUPARA2.72
          T1=TTP*(100./PSG)**ROVCP                                               CUPARA2.73
          EE=PSG*Q/(0.622+Q)                                                     05DEC01.701
          TDPT=1./(1./SVPT0-RV/XLV*ALOG(EE/.611))                                05DEC01.702
          TDPT=AMIN1(TDPT,TTP)                                                   05DEC01.703
          TLCL=TDPT-(.212+1.571E-3*(TDPT-SVPT0)-4.36E-4*                         05DEC01.704
     +         (TTP-SVPT0))*(TTP-TDPT)                                           05DEC01.705
          EQT=T1*EXP(XLVOCP*Q/TLCL)                                              05DEC01.706
          IF(EQT.GT.EQTM)THEN                                                    CUPARA2.75
            EQTM=EQT                                                             CUPARA2.76
            TMAX=TTP                                                             CUPARA2.77
            QMAX=Q                                                               CUPARA2.78
            PMAX=PSG                                                             CUPARA2.79
          ENDIF                                                                  CUPARA2.80
  210   CONTINUE                                                                 CUPARA2.81
C                                                                                CUPARA2.82
C--2--COMPUTE LCL, GET THE SIGMA AND P OF LCL                                    CUPARA2.83
C                                                                                CUPARA2.84
        EMAX=QMAX*PMAX/(EP2+QMAX)                                                CUPARA2.85
        TDMAX=(SVP3*ALOG(EMAX/SVP1)-SVP2*SVPT0)/(ALOG(EMAX/SVP1)-SVP2)           CUPARA2.86
        DALR=G/CP                                                                CUPARA2.87
        DPLR=(G*TDMAX*TDMAX)/(EP2*XLV*TMAX)                                      CUPARA2.88
        ZLCL=(TMAX-TDMAX)/(DALR-DPLR)                                            CUPARA2.89
        TLCL=TMAX-DALR*ZLCL                                                      CUPARA2.90
        TMEAN=0.5*(TMAX+TLCL)                                                    CUPARA2.91
        DLNP=(G*ZLCL)/(R*TMEAN)                                                  CUPARA2.92
        PLCL=PMAX*EXP(-DLNP)                                                     CUPARA2.93
        SIGLCL=(PLCL-PTOP)*RPSB(I,J)                                             CUPARA2.94
C                                                                                CUPARA2.95
C--3--COMPUTE SEQT (SATURATION EQUIVALENT POTENTIAL TEMPERATURE)                 CUPARA2.96
C     OF ALL THE LEVELS THAT ARE ABOVE THE LCL                                   CUPARA2.97
C                                                                                CUPARA2.98
        DO 220 K=1,KL                                                            CUPARA2.99
          IF(A(K).GE.SIGLCL)GOTO 230                                             CUPARA2.100
  220   CONTINUE                                                                 CUPARA2.101
  230   CONTINUE                                                                 CUPARA2.102
        KBASE=K                                                                  CUPARA2.103
        IF(KBASE.GT.KL)KBASE=KL                                                  CUPARA2.104
C                                                                                CUPARA2.105
C.....KBASE IS THE LAYER WHERE LCL IS LOCATED.                                   CUPARA2.106
C                                                                                CUPARA2.107
        DO 240 K=1,KBASE                                                         CUPARA2.108
          TTP=T3D(I,J,K)                                                         CUPARA2.109
          PSG=PSB(I,J)*A(K)+PTOP+PP3D(I,J,K)*0.001                               CUPARA2.110
          ES=SVP1*EXP(SVP2*(TTP-SVPT0)/(TTP-SVP3))                               CUPARA2.111
          QS=EP2*ES/(PSG-ES)                                                     CUPARA2.112
          T1=TTP*(100./PSG)**ROVCP                                               CUPARA2.113
          SEQT(K)=T1*EXP(XLVOCP*QS/TTP)                                          CUPARA2.114
  240   CONTINUE                                                                 CUPARA2.115
C                                                                                CUPARA2.116
C--4--WHEN SEQT = EQT + DT, CLOUD TOP IS REACHED.                                CUPARA2.117
C     EQT IS THE EQT OF CLOUD (SAME AS LCL EQT).                                 CUPARA2.118
C                                                                                CUPARA2.119
        DO 260 KK=1,KBASE                                                        CUPARA2.120
          K=KBASE+1-KK                                                           CUPARA2.121
          DEQT=SEQT(K)-EQTM                                                      CUPARA2.122
          IF(DEQT.GT.DLT)GOTO 270                                                CUPARA2.123
  260   CONTINUE                                                                 CUPARA2.124
  270   CONTINUE                                                                 CUPARA2.125
C                                                                                CUPARA2.126
C.....CLOUD TOP HAS BEEN REACHED                                                 CUPARA2.127
C                                                                                CUPARA2.128
        KTOP=K                                                                   CUPARA2.129
C                                                                                CUPARA2.130
C--5--CHECK CLOUD DEPTH                                                          CUPARA2.131
C     IF CLOUD DEPTH IS LESS THAN CRITICAL DEPTH (CDSCLD = 0.3),                 CUPARA2.132
C     THE CONVECTION IS KILLED                                                   CUPARA2.133
C                                                                                CUPARA2.134
        DSC=(SIGLCL-A(KTOP))                                                     CUPARA2.135
        IF(DSC.LT.CDSCLD)GOTO 70                                                 CUPARA2.136
C                                                                                CUPARA2.137
C--6--CHECK NEGATIVE AREA                                                        CUPARA2.138
C     IF NEGATIVE AREA IS LARGER THAN THE POSITIVE AREA                          CUPARA2.139
C     CONVECTION IS KILLED.                                                      CUPARA2.140
C                                                                                CUPARA2.141
        TTSUM=0.                                                                 CUPARA2.142
        DO 280 K=KTOP,KBASE                                                      CUPARA2.143
          TTSUM=(EQTM-SEQT(K))*DSIGMA(K)+TTSUM                                   CUPARA2.144
  280   CONTINUE                                                                 CUPARA2.145
        IF(TTSUM.LT.0.)GOTO 70                                                   CUPARA2.146
C                                                                                CUPARA2.147
C.....YOU ARE HERE IF YOU AVOIDED STABILITY CHECK, OR IF                         CUPARA2.148
C     STABILITY WAS FOUND.  IN FORMER CASE YOU HAVE NO VALUES                    CUPARA2.149
C     FOR KTOP AND KBASE, SO SET THEM TO THE MIN/MAX POSSIBLE                    CUPARA2.150
C                                                                                CUPARA2.151
  290   CONTINUE                                                                 CUPARA2.152
        IF(ICUSTB.EQ.0)THEN                                                      CUPARA2.153
          KBASE=KL                                                               CUPARA2.154
          KTOP=1                                                                 CUPARA2.155
        ENDIF                                                                    CUPARA2.156
C                                                                                CUPARA2.157
C.....IF VALUES DONT ALREADY EXIST IN ARRAY TWGHT,VQFLX FOR THIS                 CUPARA2.158
C     KBASE/KTOP, THEN FLAG IT, AND SET KBASE/KTOP TO STANDARD                   CUPARA2.159
C                                                                                CUPARA2.160
        IF((KBASE.LT.5).OR.(KTOP.GT.KBASE-3))THEN                                CUPARA2.161
          PRINT300,KTAU,I,J,KBASE,KTOP                                           CUPARA2.162
  300 FORMAT (/,' >>IN **CUPARA**: AT KTAU=',I8,                                 CUPARA2.163
     1          ' & (I,J)=(',I2,',',I2,'),   ',                                  CUPARA2.164
     2          ' KBASE/KTOP ARE NON-STANDARD:',2I3,                             CUPARA2.165
     3          '  & WILL BE SET TO CLOSEST STANDARD.')                          CUPARA2.166
          IF(KBASE.LT.5)KBASE=5                                                  CUPARA2.167
          IF(KTOP.GT.KBASE-3)KTOP=KBASE-3                                        CUPARA2.168
        ENDIF                                                                    CUPARA2.169
C                                                                                CUPARA2.170
C.....CONVECTION EXIST, COMPUTE CONVECTIVE FLUX OF WATER VAPOR AND               CUPARA2.171
C     LATENT HEATING                                                             CUPARA2.172
C     ICON   : IS A COUNTER WHICH KEEP TRACK THE TOTAL POINTS WHERE              CUPARA2.173
C              DEEP CONVECTION OCCURS.                                           CUPARA2.174
C     C301   : IS THE 'B' FACTOR IN KUO`S SCHEME.                                CUPARA2.175
C                                                                                CUPARA2.176
        IPARJSUM(IICON_SUM,J)=IPARJSUM(IICON_SUM,J)+1                            CUPARA2.177
        SUM=0.                                                                   CUPARA2.178
        SUMB=0.                                                                  CUPARA2.179
        ARH=0.                                                                   CUPARA2.180
        PSX=PSB(I,J)                                                             CUPARA2.181
        DO 310 K=1,KL                                                            CUPARA2.182
          QWGHT(J,K)=0.                                                          CUPARA2.183
  310   CONTINUE                                                                 CUPARA2.184
        DO 30 K=KTOP,KL                                                          CUPARA2.185
          PUX=PSX*A(K)+PTOP+PP3D(I,J,K)*0.001                                    CUPARA2.186
          E1=SVP1*EXP(SVP2*(T3D(I,J,K)-SVPT0)/(T3D(I,J,K)-SVP3))                 CUPARA2.187
          QS=EP2*E1/(PUX-E1)                                                     CUPARA2.188
          RH=QV3D(I,J,K)/QS                                                      CUPARA2.189
          RH=AMIN1(RH,1.0)                                                       CUPARA2.190
          XSAV=(1.0-RH)*QS                                                       CUPARA2.191
          QWGHT(J,K)=XSAV                                                        CUPARA2.192
          SUMB=SUMB+QS*DSIGMA(K)                                                 CUPARA2.193
          ARH=ARH+RH*QS*DSIGMA(K)                                                CUPARA2.194
          SUM=SUM+XSAV*DSIGMA(K)                                                 CUPARA2.195
   30   CONTINUE                                                                 CUPARA2.196
        ARH=ARH/SUMB                                                             CUPARA2.197
        C301=BFAC*(1.0-ARH)                                                      CUPARA2.198
        IF(C301.LT.0.0)C301=0.0                                                  CUPARA2.199
        IF(C301.GT.1.0)C301=1.0                                                  CUPARA2.200
        IF(SUM.GT.0.0)GOTO 40                                                    CUPARA2.201
        C301=0.0                                                                 CUPARA2.202
        SUM=1.0                                                                  CUPARA2.203
   40   CONTINUE                                                                 CUPARA2.204
        DO 50 K=KTOP,KL                                                          CUPARA2.205
          QWGHT(J,K)=QWGHT(J,K)/SUM                                              CUPARA2.206
   50   CONTINUE                                                                 CUPARA2.207
        DO 60 K=1,KL                                                             CUPARA2.208
          TTCONV=XLVOCP*(1.0-C301)*TWGHT(K,KBASE,KTOP)*SCA                       CUPARA2.209
          T3DTEN(I,J,K)=T3DTEN(I,J,K)+TTCONV*XMOIST(IN)                          CUPARA2.210
          APCNT=(1.0-C301)*SCA/4.3E-3                                            CUPARA2.211
          EDDYF=APCNT*VQFLX(K,KBASE,KTOP)                                        CUPARA2.212
          QV3DTEN(I,J,K)=C301*QWGHT(J,K)*SCA+EDDYF                               CUPARA2.213
   60   CONTINUE                                                                 CUPARA2.214
C.....THE UNIT FOR RAINFALL IS CM.                                               CUPARA2.215
        RAINC(I,J)=RAINC(I,J)+(1.-C301)*SCA*DTMIN*6000./G                        CUPARA2.216
        GOTO 100                                                                 CUPARA2.217
C                                                                                CUPARA2.218
C.....CONVECTION NOT EXIST, COMPUTE THE VERTICAL ADVECTION TERM:                 CUPARA2.219
C                                                                                CUPARA2.220
   70   CONTINUE                                                                 CUPARA2.221
        SCR3(I,1)=0.                                                             CUPARA2.222
        DO 80 K=2,KL                                                             CUPARA2.223
          IF(IVQADV.EQ.0)THEN                                                    07NOV00.1465
            SCR3(I,K)=QV3D(I,J,K)*PSB(I,J)                                       07NOV00.1466
     +              *(QV3D(I,J,K-1)/QV3D(I,J,K))**QCON(K)                        07NOV00.1467
          ELSE                                                                   07NOV00.1468
            SCR3(I,K)=(TWT(K,1)*QV3D(I,J,K)+TWT(K,2)*QV3D(I,J,K-1))              07NOV00.1469
     +                *PSB(I,J)                                                  07NOV00.1470
          ENDIF                                                                  07NOV00.1471
   80   CONTINUE                                                                 CUPARA2.225
        QV3DTEN(I,J,1)=QV3DTEN(I,J,1)-QDOT(I,J,2)*SCR3(I,2)/DSIGMA(1)            CUPARA2.226
        DO 90 K=2,KLM                                                            CUPARA2.227
          QV3DTEN(I,J,K)=QV3DTEN(I,J,K)-(QDOT(I,J,K+1)*SCR3(I,K+1)-              CUPARA2.228
     +                   QDOT(I,J,K)*SCR3(I,K))/DSIGMA(K)                        CUPARA2.229
   90   CONTINUE                                                                 CUPARA2.230
        QV3DTEN(I,J,KL)=QV3DTEN(I,J,KL)+QDOT(I,J,KL)*SCR3(I,KL)/                 CUPARA2.231
     +                  DSIGMA(KL)                                               CUPARA2.232
C                                                                                CUPARA2.233
  100 CONTINUE                                                                   CUPARA2.234
C                                                                                CUPARA2.235
C                                                                                CUPARA2.236
      RETURN                                                                     CUPARA2.237
      END                                                                        CUPARA2.238
                                                                                 CUPARA2.239
