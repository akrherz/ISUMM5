       SUBROUTINE CUPARA4(T3D,QV3D,PSB,T3DTEN,QV3DTEN,RAINC,HT,                  CUPARA4.1
     *                    U3D,V3D,PP3D,INEST,J,ISTART,IEND)                      CUPARA4.2
                                                                                 CUPARA4.3
#     include <parame.incl>                                                      CUPARA4.4
#     include <param2.incl>                                                      CUPARA4.5
#     include <param3.incl>                                                      CUPARA4.6
#     include <pmoist.incl>                                                      CUPARA4.7
#     include <various.incl>                                                     CUPARA4.8
#     include <addras.incl>                                                      CUPARA4.9
#     include <sum.incl>                                                         CUPARA4.10
                                                                                 CUPARA4.11
      PARAMETER (KDIM=MKX)                                                       CUPARA4.12
      DIMENSION   T3D(MIX,MJX,MKX), QV3D(MIX,MJX,MKX),                           CUPARA4.13
     *            U3D(MIX,MJX,MKX),  V3D(MIX,MJX,MKX),                           CUPARA4.14
     *           PP3D(MIX,MJX,MKX),  PSB(MIX,MJX),                               CUPARA4.15
     *          RAINC(MIX,MJX),       HT(MIX,MJX)                                CUPARA4.16
      DIMENSION T3DTEN(MIX,MJX,MKX),  QV3DTEN(MIX,MJX,MKX)                       CUPARA4.17
C                                                                                CUPARA4.18
C--- DIMENSIONS NEEDED FOR LITTLE DRIVER ROUTINE                                 CUPARA4.19
C                                                                                CUPARA4.20
      DIMENSION T(KDIM),Q(KDIM),P(KDIM),                                         CUPARA4.21
     *          TN(KDIM),QO(KDIM),OUTT(KDIM),OUTQ(KDIM),                         CUPARA4.22
     *          VSP(KDIM)                                                        CUPARA4.23
                                                                                 CUPARA4.24
      IASTES=0                                                                   CUPARA4.25
      IF(INEST.EQ.1.AND.KTAU.EQ.0.AND.J.EQ.2)IASTES=1                            CUPARA4.26
      IF(INEST.GT.1.AND.KTAU.EQ.0.AND.J.EQ.3)IASTES=1                            CUPARA4.27
      KBMAX=((MKX*3)/4-3)                                                        CUPARA4.28
      KLNH=MKXNH                                                                 CUPARA4.29
      PCUT=400.                                                                  CUPARA4.30
      DTIME=60.*30.                                                              CUPARA4.31
      C0=0.002                                                                   CUPARA4.32
      KDTM=IFIX(2.*DTIME/DT+.001)                                                CUPARA4.33
      IF(KTAU.EQ.0)KDTM=KDTM/2                                                   CUPARA4.34
      IF(IASTES.EQ.1)THEN                                                        CUPARA4.35
         PRINT *,' CONVECTIVE SCHEME WILL BE CALLED EVERY ',KDTM,                CUPARA4.36
     1 ' TIME STEPS FOR DOMAIN ', INEST                                          CUPARA4.37
      ENDIF                                                                      CUPARA4.38
      KCLMIN=2                                                                   CUPARA4.39
      PDDCUT=550.                                                                CUPARA4.40
      PKDCUT=75.                                                                 CUPARA4.41
      XKTAU=0.                                                                   CUPARA4.42
      DO 50 I=ISTART,IEND                                                        CUPARA4.43
        IF(KTAU.LE.1)THEN                                                        CUPARA4.44
          KDT(I,J)=KDTM                                                          CUPARA4.45
        ELSE                                                                     CUPARA4.46
          XKTAU=ABS(FLOAT(KDT(I,J)/KDTM)-(FLOAT(KDT(I,J))/FLOAT(KDTM)))          CUPARA4.47
          IF(XKTAU.GT.1.E-05)GOTO 1001                                           CUPARA4.48
        ENDIF                                                                    CUPARA4.49
C                                                                                CUPARA4.50
C--- PREPARE INPUT, ERASE OUTPUT                                                 CUPARA4.51
C                                                                                CUPARA4.52
        KDMAX=0                                                                  CUPARA4.53
        KDET=0                                                                   CUPARA4.54
        DO 100 K=1,KDIM                                                          CUPARA4.55
          KK=KDIM-K+1                                                            CUPARA4.56
          US=0.25*(U3D(I,J,KK)+U3D(I+1,J,KK)+U3D(I+1,J+1,KK)+                    CUPARA4.57
     +       U3D(I,J+1,KK))                                                      CUPARA4.58
          VS=0.25*(V3D(I,J,KK)+V3D(I+1,J,KK)+V3D(I+1,J+1,KK)+                    CUPARA4.59
     +       V3D(I,J+1,KK))                                                      CUPARA4.60
          T(K)=T3D(I,J,KK)                                                       CUPARA4.61
          Q(K)=QV3D(I,J,KK)                                                      CUPARA4.62
          IF(Q(K).LT.1.E-08)Q(K)=1.E-08                                          CUPARA4.63
          TN(K)=T(K)+(T3DTEN(I,J,KK))/PSB(I,J)*DTIME                             CUPARA4.64
          QO(K)=Q(K)+(QV3DTEN(I,J,KK))/PSB(I,J)*DTIME                            CUPARA4.65
          P(K)=10.*PSB(I,J)*A(KK)+10.*PTOP+PP3D(I,J,KK)/100.                     CUPARA4.66
          VSP(K)=SQRT(US**2+VS**2)                                               CUPARA4.67
          IF(QO(K).LT.1.E-08)QO(K)=1.E-08                                        CUPARA4.68
C                                                                                CUPARA4.69
          PSUR=10.*PSB(I,J)+10.*PTOP+PP3D(I,J,KLNH)/100.                         CUPARA4.70
          PSURO=PSUR                                                             CUPARA4.71
          PKK=PSUR-P(K)                                                          CUPARA4.72
          IF(PKK.LE.PDDCUT)KDMAX=KDMAX+1                                         CUPARA4.73
          IF(PKK.LE.PKDCUT)KDET=KDET+1                                           CUPARA4.74
          OUTT(K)=0.                                                             CUPARA4.75
          OUTQ(K)=0.                                                             CUPARA4.76
  100   CONTINUE                                                                 CUPARA4.77
        PRET=0.                                                                  CUPARA4.78
        TER11=HT(I,J)/G                                                          CUPARA4.79
        IF(TER11.LE.0.)TER11=1.E-05                                              CUPARA4.80
C                                                                                CUPARA4.81
C--- CALL CUMULUS PARAMETERIZATION                                               CUPARA4.82
C                                                                                CUPARA4.83
        FTEST=0.                                                                 CUPARA4.84
        IERR=0                                                                   CUPARA4.85
        CALL ARASCH(T,Q,TER11,TN,QO,P,KDIM,PRET,P,OUTT,OUTQ,DTIME,KBMAX,         CUPARA4.86
     +       PCUT,C0,FTEST,PSUR,PSURO,IERR,KDET,VSP)                             CUPARA4.87
  248   CONTINUE                                                                 CUPARA4.88
        DO 249 K=1,KDIM                                                          CUPARA4.89
          OUTTT(I,J,K)=OUTT(K)                                                   CUPARA4.90
          OUTTQ(I,J,K)=OUTQ(K)                                                   CUPARA4.91
  249   CONTINUE                                                                 CUPARA4.92
        PRETT(I,J)=PRET                                                          CUPARA4.93
 1001   CONTINUE                                                                 CUPARA4.94
        IF(PRETT(I,J).GT.1.E-05)KDT(I,J)=KDT(I,J)+1                              CUPARA4.95
        DO 300 K=1,KDIM                                                          CUPARA4.96
          KK=KDIM-K+1                                                            CUPARA4.97
          T3DTEN(I,J,KK)=PSB(I,J)*OUTTT(I,J,K)+T3DTEN(I,J,KK)                    CUPARA4.98
          QV3DTEN(I,J,KK)=PSB(I,J)*OUTTQ(I,J,K)+QV3DTEN(I,J,KK)                  CUPARA4.99
  300   CONTINUE                                                                 CUPARA4.100
C                                                                                CUPARA4.101
C--- RAIN IN CM.                                                                 CUPARA4.102
C                                                                                CUPARA4.103
        RAINC(I,J)=RAINC(I,J)+PRETT(I,J)*.5*DT*.1                                CUPARA4.104
        IF(PRETT(I,J).GT.0.)THEN                                                 CUPARA4.105
          IPARJSUM(IICON_SUM,J)=IPARJSUM(IICON_SUM,J)+1                          CUPARA4.106
        ENDIF                                                                    CUPARA4.107
   50 CONTINUE                                                                   CUPARA4.108
      RETURN                                                                     CUPARA4.109
      END                                                                        CUPARA4.110
                                                                                 CUPARA4.111
