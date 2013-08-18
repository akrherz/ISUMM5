#include <defines.incl>                                                          07NOV00.1331
       SUBROUTINE CUPARA3(T3D,QV3D,PSB,T3DTEN,QV3DTEN,RAINC,CLDFRA,              CUPARA3.1
     &                    HT,U3D,V3D,PP3D,                                       07NOV00.1332
     &                    RPSA,DT,DX,R,CP,G,TO,XLV,RV,A,PTOP,                    07NOV00.1333
     &                    INEST,J,IST,IEN)                                       07NOV00.1334
      IMPLICIT NONE                                                              07NOV00.1335
#     include <parame.incl>                                                      CUPARA3.4
#     include <param2.incl>                                                      CUPARA3.5
#     include <parccm2.incl>                                                     CUPARA3.6
#     include <pargrcu.incl>                                                     CUPARA3.11
      REAL      R,CP,G,TO,XLV,RV,A(MKX),PTOP                                     07NOV00.1336
      REAL      DT,DX                                                            07NOV00.1337
      REAL      RPSA(MIX,MJX)                                                    07NOV00.1338
#ifndef VECTOR                                                                   07NOV00.1339
#     include <sum.incl>                                                         CUPARA3.12
#endif                                                                           07NOV00.1340
C                                                                                CUPARA3.13
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          CUPARA3.14
C     CHANGES:                                                        C          CUPARA3.15
C        INCREASE RAINFALL EFFICIENCY, LIMIT D/D MASS FLUX TO 0.8     C          CUPARA3.16
C        KEEP DOWNDRAFT ORIGIN LEVEL (JMIN) BELOW CLOUD TOP (KTOP)    C          CUPARA3.17
C                                                                     C          CUPARA3.18
C        ADD NAMELIST VARIABLES FOR GRELL-SCHEME PARAMETERS           C          CUPARA3.19
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          CUPARA3.20
C                                                                                CUPARA3.21
      REAL      T3D(MIX,MJX,MKX),  QV3D(MIX,MJX,MKX),                            07NOV00.1341
     *          U3D(MIX,MJX,MKX),  V3D(MIX,MJX,MKX),                             CUPARA3.23
     *          PP3D(MIX,MJX,MKX), PSB(MIX,MJX),                                 CUPARA3.24
     *          RAINC(MIX,MJX),     HT(MIX,MJX),  CLDFRA(IPLOND,IPLEVP)          CUPARA3.25
      REAL      T3DTEN(MIX,MJX,MKX),  QV3DTEN(MIX,MJX,MKX)                       07NOV00.1342
C                                                                                CUPARA3.27
C--- DIMENSIONS NEEDED FOR LITTLE DRIVER ROUTINE (LOCAL ARRAYS)                  CUPARA3.28
C                                                                                CUPARA3.29
      REAL      T(MIX,MKX),Q(MIX,MKX),P(MIX,MKX),PO(MIX,MKX),                    07NOV00.1343
     * TN(MIX,MKX),QO(MIX,MKX),OUTT(MIX,MKX),                                    CUPARA3.31
     * OUTQ(MIX,MKX),PRET(MIX),PSUR(MIX),TER11(MIX),US(MIX,MKX),                 CUPARA3.32
     * VS(MIX,MKX)                                                               CUPARA3.33
C                                                                                07NOV00.1344
C declarations for implicit none                                                 07NOV00.1345
C                                                                                07NOV00.1346
      REAL DTIME,C0,PKK,CALC                                                     07NOV00.1347
      INTEGER I,J,K,KK,IST,IEN,ICONJ,INEST                                       07NOV00.1348
C                                                                                07NOV00.1349
      INTEGER KDET(MIX)                                                          CUPARA3.34
                                                                                 CUPARA3.35
      IF(IFDTSET)THEN                                                            CUPARA3.36
        DTIME=DTIMSET                                                            CUPARA3.37
      ELSE                                                                       CUPARA3.38
        DTIME=DT                                                                 CUPARA3.39
      ENDIF                                                                      CUPARA3.40
C     PKDCUT=75.(DEFAULT) BUT CAN BE SET IN PPARAM                               CUPARA3.41
      C0=0.002                                                                   CUPARA3.44
C                                                                                CUPARA3.45
C--- PREPARE INPUT, ERASE OUTPUT                                                 CUPARA3.46
C                                                                                CUPARA3.47
      DO 75 I=IST,IEN                                                            07NOV00.1350
        KDET(I)=2                                                                CUPARA3.49
        PRET(I)=0.                                                               CUPARA3.50
   75 CONTINUE                                                                   CUPARA3.51
      DO 100 K=1,MKX                                                             CUPARA3.52
        DO 100 I=IST,IEN                                                         07NOV00.1351
          KK=MKX-K+1                                                             CUPARA3.54
#ifndef VECTOR                                                                   07NOV00.1352
          US(I,K)=0.25*(U3D(I,J,KK)+U3D(I+1,J,KK)+U3D(I+1,J+1,KK)+               CUPARA3.55
     +            U3D(I,J+1,KK))                                                 CUPARA3.56
          VS(I,K)=0.25*(V3D(I,J,KK)+V3D(I+1,J,KK)+V3D(I+1,J+1,KK)+               CUPARA3.57
     +            V3D(I,J+1,KK))                                                 CUPARA3.58
#else                                                                            07NOV00.1353
C U and V are averaged in the caller for the vector version                      07NOV00.1354
          US(I,K) = U3D(I,J,KK)                                                  07NOV00.1355
          VS(I,K) = V3D(I,J,KK)                                                  07NOV00.1356
#endif                                                                           07NOV00.1357
          T(I,K)=T3D(I,J,KK)                                                     CUPARA3.59
          Q(I,K)=QV3D(I,J,KK)                                                    CUPARA3.60
CB          IF(Q(I,K).LT.1.E-08)Q(I,K)=1.E-08                                    23DEC04.3137
CB          TN(I,K)=T(I,K)+(T3DTEN(I,J,KK))*RPSA(I,J)*DTIME                      23DEC04.3138
CB          QO(I,K)=Q(I,K)+(QV3DTEN(I,J,KK))*RPSA(I,J)*DTIME                     23DEC04.3139
CB          P(I,K)=10.*PSB(I,J)*A(KK)+10.*PTOP+PP3D(I,J,KK)*0.01                 23DEC04.3140
CB          IF(QO(I,K).LT.1.E-08)QO(I,K)=1.E-08                                  23DEC04.3141
          Q(I,K)=amax1(Q(I,K),1.E-08)                                            23DEC04.3142
          TN(I,K)=T(I,K)+(T3DTEN(I,J,KK))*(RPSA(I,J)*DTIME)                      23DEC04.3143
          QO(I,K)=Q(I,K)+(QV3DTEN(I,J,KK))*(RPSA(I,J)*DTIME)                     23DEC04.3144
          P(I,K)=10.*(PSB(I,J)*A(KK)+PTOP)+PP3D(I,J,KK)*0.01                     23DEC04.3145
          QO(I,K)=amax1(QO(I,K),1.E-08)                                          23DEC04.3146
C                                                                                CUPARA3.66
          PO(I,K)=P(I,K)                                                         CUPARA3.67
CB          PSUR(I)=10.*PSB(I,J)+10.*PTOP+PP3D(I,J,MKX)*0.01                     23DEC04.3147
          PSUR(I)=10.*(PSB(I,J)+PTOP)+PP3D(I,J,MKX)*0.01                         23DEC04.3148
          OUTT(I,K)=0.                                                           CUPARA3.69
          PKK=PSUR(I)-PO(I,K)                                                    CUPARA3.70
          IF(PKK.LE.PKDCUT)KDET(I)=KDET(I)+1                                     CUPARA3.71
          OUTQ(I,K)=0.                                                           CUPARA3.72
CB          TER11(I)=HT(I,J)/9.81                                                23DEC04.3149
          TER11(I)=HT(I,J)*0.1019368                                             23DEC04.3150
          IF(TER11(I).LE.0.)TER11(I)=1.E-05                                      CUPARA3.74
  100   CONTINUE                                                                 CUPARA3.75
                                                                                 CUPARA3.76
C                                                                                CUPARA3.77
C--- CALL CUMULUS PARAMETERIZATION                                               CUPARA3.78
C                                                                                CUPARA3.79
      CALL CUP(T,Q,TER11,TN,QO,PO,PRET,P,OUTT,OUTQ,DTIME,PSUR,US,VS,             CUPARA3.80
     +     IST,IEN,KDET,DX,IFRAD,CLDFRA,R,CP,G,TO,XLV,RV,A,PTOP)                 07NOV00.1358
  248 CONTINUE                                                                   CUPARA3.82
 1001 CONTINUE                                                                   CUPARA3.83
      DO 300 K=1,MKX                                                             CUPARA3.84
        DO 300 I=IST,IEN                                                         07NOV00.1359
          IF(PRET(I).LE.0.)GOTO 300                                              CUPARA3.86
          KK=MKX-K+1                                                             CUPARA3.87
          T3DTEN(I,J,KK)=PSB(I,J)*OUTT(I,K)+T3DTEN(I,J,KK)                       CUPARA3.88
          QV3DTEN(I,J,KK)=PSB(I,J)*OUTQ(I,K)+QV3DTEN(I,J,KK)                     CUPARA3.89
  300   CONTINUE                                                                 CUPARA3.90
                                                                                 CUPARA3.91
C                                                                                CUPARA3.92
C--- RAIN IN CM.                                                                 CUPARA3.93
C                                                                                CUPARA3.94
CB      CALC=.5                                                                  23DEC04.3151
      CALC=.05*dt                                                                23DEC04.3152
      ICONJ=0                                                                    CUPARA3.96
      DO 50 I=IST,IEN                                                            07NOV00.1360
        IF(PRET(I).LE.0.)GOTO 50                                                 CUPARA3.98
CB        RAINC(I,J)=RAINC(I,J)+PRET(I)*CALC*DT*.1                               23DEC04.3153
        RAINC(I,J)=RAINC(I,J)+PRET(I)*CALC                                       23DEC04.3154
        ICONJ=ICONJ+1                                                            CUPARA3.100
   50 CONTINUE                                                                   CUPARA3.101
#ifndef VECTOR                                                                   07NOV00.1361
      IPARJSUM(IICON_SUM,J)=ICONJ                                                CUPARA3.102
#endif                                                                           07NOV00.1362
      RETURN                                                                     CUPARA3.103
      END                                                                        CUPARA3.104
                                                                                 CUPARA3.105
