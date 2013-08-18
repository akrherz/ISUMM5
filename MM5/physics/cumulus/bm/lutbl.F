      SUBROUTINE LUTBL(PTOP)                                                     LUTBL.1
C                                                                                LUTBL.2
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&           LUTBL.3
C                                                                                LUTBL.4
      PARAMETER(ITB=100,JTB=150,A2=17.2693882E0,A3=273.16E0,A4=35.86E0)          LUTBL.5
      PARAMETER(PQ0=379.90516E0,EPS=2.E-12)                                      LUTBL.6
      PARAMETER(ELIWV=2.72E6,CP=1004.E0,R=287.04,ROVCP=R/CP)                     LUTBL.7
C                                                                                LUTBL.8
      COMMON /TBLS/ PT,PL,THL,RDQ,RDTH,RDP,RDTHE,QS0(JTB),SQS(JTB),              LUTBL.9
     1             THE0(JTB),STHE(JTB),PTBL(ITB,JTB),TTBL(JTB,ITB)               LUTBL.10
      DIMENSION QSOLD(JTB),POLD(JTB),QSNEW(JTB),                                 LUTBL.11
     1          Y2P(JTB),PNEW(JTB)                                               LUTBL.12
      DIMENSION TOLD(JTB),THEOLD(JTB),THENEW(JTB),                               LUTBL.13
     1          Y2T(JTB),TNEW(JTB)                                               LUTBL.14
      DIMENSION APP(JTB),AQP(JTB),APT(JTB),AQT(JTB)                              LUTBL.15
C                                                                                LUTBL.16
C--------------COARSE LOOK-UP TABLE FOR SATURATION POINT----------------         LUTBL.17
C                                                                                LUTBL.18
      PT=PTOP*1000.                                                              LUTBL.19
! PTOP IN PASCAL                                                                 LUTBL.20
      KTHM=JTB                                                                   LUTBL.21
      KPM=ITB                                                                    LUTBL.22
      KTHM1=KTHM-1                                                               LUTBL.23
      KPM1=KPM-1                                                                 LUTBL.24
C                                                                                LUTBL.25
      THL=210.                                                                   LUTBL.26
      THH=385.                                                                   LUTBL.27
      PL=PT                                                                      LUTBL.28
      PH=105000.                                                                 LUTBL.29
C                                                                                LUTBL.30
      DTH=(THH-THL)/FLOAT(KTHM-1)                                                LUTBL.31
      DP=(PH-PL)/FLOAT(KPM-1)                                                    LUTBL.32
C                                                                                LUTBL.33
      RDTH=1./DTH                                                                LUTBL.34
      RDP=1./DP                                                                  LUTBL.35
      RDQ=KPM-1                                                                  LUTBL.36
C                                                                                LUTBL.37
      TH=THL-DTH                                                                 LUTBL.38
C-----------------------------------------------------------------------         LUTBL.39
      DO 500 KTH=1,KTHM                                                          LUTBL.40
        TH=TH+DTH                                                                LUTBL.41
        P=PL-DP                                                                  LUTBL.42
        DO 510 KP=1,KPM                                                          LUTBL.43
          P=P+DP                                                                 LUTBL.44
          APE=(100000./P)**(ROVCP)                                               LUTBL.45
          QSOLD(KP)=PQ0/P*EXP(A2*(TH-A3*APE)/(TH-A4*APE))                        LUTBL.46
          POLD(KP)=P                                                             LUTBL.47
  510   CONTINUE                                                                 LUTBL.48
C                                                                                LUTBL.49
        QS0K=QSOLD(1)                                                            LUTBL.50
        SQSK=QSOLD(KPM)-QSOLD(1)                                                 LUTBL.51
        QSOLD(1)=0.                                                              LUTBL.52
        QSOLD(KPM)=1.                                                            LUTBL.53
C                                                                                LUTBL.54
        DO 520 KP=2,KPM1                                                         LUTBL.55
          QSOLD(KP)=(QSOLD(KP)-QS0K)/SQSK                                        LUTBL.56
CWWWWWWWWWWWWWW FIX DUE TO CYBER HALF PREC. LIMITATION WWWWWWWWWWWWWWWWW         LUTBL.57
          IF((QSOLD(KP)-QSOLD(KP-1)).LT.EPS)QSOLD(KP)=QSOLD(KP-1)+EPS            LUTBL.58
CMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM         LUTBL.59
  520   CONTINUE                                                                 LUTBL.60
C                                                                                LUTBL.61
        QS0(KTH)=QS0K                                                            LUTBL.62
        SQS(KTH)=SQSK                                                            LUTBL.63
C-----------------------------------------------------------------------         LUTBL.64
        QSNEW(1)=0.                                                              LUTBL.65
        QSNEW(KPM)=1.                                                            LUTBL.66
        DQS=1./FLOAT(KPM-1)                                                      LUTBL.67
C                                                                                LUTBL.68
        DO 530 KP=2,KPM1                                                         LUTBL.69
  530   QSNEW(KP)=QSNEW(KP-1)+DQS                                                LUTBL.70
C                                                                                LUTBL.71
        Y2P(1)=0.                                                                LUTBL.72
        Y2P(KPM)=0.                                                              LUTBL.73
C                                                                                LUTBL.74
        CALL SPLINE(KPM,QSOLD,POLD,Y2P,KPM,QSNEW,PNEW,APP,AQP)                   LUTBL.75
C                                                                                LUTBL.76
        DO 540 KP=1,KPM                                                          LUTBL.77
  540   PTBL(KP,KTH)=PNEW(KP)                                                    LUTBL.78
C-----------------------------------------------------------------------         LUTBL.79
  500 CONTINUE                                                                   LUTBL.80
C--------------COARSE LOOK-UP TABLE FOR T(P) FROM CONSTANT THE----------         LUTBL.81
      P=PL-DP                                                                    LUTBL.82
      DO 550 KP=1,KPM                                                            LUTBL.83
        P=P+DP                                                                   LUTBL.84
        TH=THL-DTH                                                               LUTBL.85
        DO 560 KTH=1,KTHM                                                        LUTBL.86
          TH=TH+DTH                                                              LUTBL.87
          APE=(100000./P)**(ROVCP)                                               LUTBL.88
          QS=PQ0/P*EXP(A2*(TH-A3*APE)/(TH-A4*APE))                               LUTBL.89
          TOLD(KTH)=TH/APE                                                       LUTBL.90
          THEOLD(KTH)=TH*EXP(ELIWV*QS/(CP*TOLD(KTH)))                            LUTBL.91
  560   CONTINUE                                                                 LUTBL.92
C                                                                                LUTBL.93
        THE0K=THEOLD(1)                                                          LUTBL.94
        STHEK=THEOLD(KTHM)-THEOLD(1)                                             LUTBL.95
        THEOLD(1)=0.                                                             LUTBL.96
        THEOLD(KTHM)=1.                                                          LUTBL.97
C                                                                                LUTBL.98
        DO 570 KTH=2,KTHM1                                                       LUTBL.99
          THEOLD(KTH)=(THEOLD(KTH)-THE0K)/STHEK                                  LUTBL.100
CWWWWWWWWWWWWWW FIX DUE TO CYBER HALF PREC. LIMITATION WWWWWWWWWWWWWWWWW         LUTBL.101
          IF((THEOLD(KTH)-THEOLD(KTH-1)).LT.EPS)THEOLD(KTH)=                     LUTBL.102
     +      THEOLD(KTH-1)+EPS                                                    LUTBL.103
CMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM         LUTBL.104
  570   CONTINUE                                                                 LUTBL.105
C                                                                                LUTBL.106
        THE0(KP)=THE0K                                                           LUTBL.107
        STHE(KP)=STHEK                                                           LUTBL.108
C-----------------------------------------------------------------------         LUTBL.109
        THENEW(1)=0.                                                             LUTBL.110
        THENEW(KTHM)=1.                                                          LUTBL.111
        DTHE=1./FLOAT(KTHM-1)                                                    LUTBL.112
        RDTHE=1./DTHE                                                            LUTBL.113
C                                                                                LUTBL.114
        DO 580 KTH=2,KTHM1                                                       LUTBL.115
  580   THENEW(KTH)=THENEW(KTH-1)+DTHE                                           LUTBL.116
C                                                                                LUTBL.117
        Y2T(1)=0.                                                                LUTBL.118
        Y2T(KTHM)=0.                                                             LUTBL.119
C                                                                                LUTBL.120
        CALL SPLINE(KTHM,THEOLD,TOLD,Y2T,KTHM,THENEW,TNEW,APT,AQT)               LUTBL.121
C                                                                                LUTBL.122
        DO 590 KTH=1,KTHM                                                        LUTBL.123
          TTBL(KTH,KP)=TNEW(KTH)                                                 LUTBL.124
  590   CONTINUE                                                                 LUTBL.125
C-----------------------------------------------------------------------         LUTBL.126
  550 CONTINUE                                                                   LUTBL.127
C                                                                                LUTBL.128
      RETURN                                                                     LUTBL.129
      END                                                                        LUTBL.130
                                                                                 LUTBL.131
