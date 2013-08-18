      SUBROUTINE OUTPB (PRATIO,GWWG,ABT,SPECHUM,U,V,QW,QL,THLSB,                 OUTPB.1
     2     THLQWB,QWSB,TKE,Z1,I,J,DT,T3DTEN,QV3DTEN,U3DTEN,V3DTEN,THL,           OUTPB.2
     3     SST,QC3DTEN,QR3DTEN,INEST,NN,SIGG,KTAU,IDELAY,PSBD,                   OUTPB.3
     4     T3D,QV3D,U3D,V3D,IYY,JXX)                                             OUTPB.4
C                                                                                OUTPB.5
#     include <parame.incl>                                                      OUTPB.6
#     include <point2d.incl>                                                     OUTPB.7
#     include <navypb.incl>                                                      OUTPB.8
      PARAMETER (II=MKX)                                                         OUTPB.9
#     include <btblk2.incl>                                                      OUTPB.10
C                                                                                OUTPB.11
C                                                                                OUTPB.12
C                                                                                OUTPB.13
      DIMENSION PSBD(MIX,MJX),SIGG(MKX)                                          OUTPB.14
      DIMENSION U3D(MIX,MJX,MKX),V3D(MIX,MJX,MKX),T3D(MIX,MJX,MKX),              OUTPB.15
     -         QV3D(MIX,MJX,MKX)                                                 OUTPB.16
      DIMENSION U3DTEN(MIX,MJX,MKX),V3DTEN(MIX,MJX,MKX),                         OUTPB.17
     -          T3DTEN(MIX,MJX,MKX),QV3DTEN(MIX,MJX,MKX),                        OUTPB.18
     -          QC3DTEN(MIXM,MJXM,MKXM),QR3DTEN(MIXM,MJXM,MKXM)                  OUTPB.19
      DIMENSION ABT(II),ABTH(II),P(II),                                          OUTPB.20
     5 PRATIO(II),QL(II),QRN(II),QSL(II),                                        OUTPB.21
     6 QW(II),QWSB(II),SPECHUM(II),                                              OUTPB.22
     8 TH(II),THL(II),THLQWB(II),THLSB(II),                                      OUTPB.23
     9 THLWB(II),THVWB(II),TKE(II),                                              OUTPB.24
     A TKESR(II),TL(II),U(II),V(II)                                              OUTPB.25
C                                                                                OUTPB.26
C_FLIC_BEGIN_NOFLIC                                                              05DEC01.865
      IBNES=2                                                                    OUTPB.27
      IENES=IYY-1                                                                OUTPB.28
      JBNES=2                                                                    OUTPB.29
      JENES=JXX-1                                                                OUTPB.30
      IF(INEST.GT.1)THEN                                                         OUTPB.31
        IBNES=3                                                                  OUTPB.32
        IENES=IYY-2                                                              OUTPB.33
        JBNES=3                                                                  OUTPB.34
        JENES=JXX-2                                                              OUTPB.35
      ENDIF                                                                      OUTPB.36
C     IF(INEST.GT.1.AND.NN.NE.1)THEN                                             OUTPB.37
C        DO 50 K=1,II                                                            OUTPB.38
C          KK=II-K+1                                                             OUTPB.39
C          SPECHUM(K)=QW(K)-QL(K)                                                OUTPB.40
C          RATMIX=SPECHUM(K)/(1.-SPECHUM(K))                                     OUTPB.41
C          IF(SPECHUM(K).GT.1.)PRINT *,'SPEC,I,J,K = ',                          OUTPB.42
C    1       SPECHUM(K),I,J,K                                                    OUTPB.43
C          IF(RATMIX.LT.0.)PRINT *,'RATMIX AT I,J,K = ',RATMIX,I,J,K             OUTPB.44
C          ABTK=THL(K)*PRATIO(K)                                                 OUTPB.45
C          DABTK=ABTK-TB(I,J,KK)/PSB(I,J)                                        OUTPB.46
C          IF(DABTK.GT.50)THEN                                                   OUTPB.47
C             PRINT *,'DABTK MORE THEN HUNDRED AT ',I,J,K                        OUTPB.48
C             RETURN                                                             OUTPB.49
C          ENDIF                                                                 OUTPB.50
C          SCR1(I,K)=TTENV(I,J,K)                                                OUTPB.51
C          SCR2(I,K)=QVTENV(I,J,K)                                               OUTPB.52
C          QCTEN(I,K)=QCTENV(I,J,K)                                              OUTPB.53
C          UTEN(I,K)=UTENV(I,J,K)                                                OUTPB.54
C          VTEN(I,K)=VTENV(I,J,K)                                                OUTPB.55
C50      CONTINUE                                                                OUTPB.56
C        TGB(I,J)=TGB(I,J)+DT*TGA(I,J)/FLOAT(IRATIO)                             OUTPB.57
C        RETURN                                                                  OUTPB.58
C     ENDIF                                                                      OUTPB.59
C                                                                                OUTPB.60
C     MAVAIL(I,J)=GWWG                                                           OUTPB.61
      ZNT(I,J)=Z1                                                                OUTPB.62
      DTGB=(SST-TGB(I,J))/DT                                                     OUTPB.63
      TGA(I,J)=TGB(I,J)+.5*DT*DTGB                                               OUTPB.64
      TGB(I,J)=TGA(I,J)                                                          OUTPB.65
C     TGA(I,J)=SST                                                               OUTPB.66
C     TGA(I,J)=(SST-TGB(I,J))/DT                                                 OUTPB.67
C     TGB(I,J)=TGB(I,J)+DT*TGA(I,J)/FLOAT(IRATIO)                                OUTPB.68
C                                                                                OUTPB.69
C                                                                                OUTPB.70
      XLCP=HEATL/CPD                                                             OUTPB.71
      DO 20 K=1,II                                                               OUTPB.72
        KK=II-K+1                                                                OUTPB.73
        IF(KTAU.GT.IDELAY.AND.K.LT.II-2)THEN                                     OUTPB.74
C      SPECHUM(K)=QW(K)-QL(K)                                                    OUTPB.75
          SPECHUM(K)=QW(K)                                                       OUTPB.76
          RATMIX=SPECHUM(K)/(1.-SPECHUM(K))                                      OUTPB.77
          IF(RATMIX.LE.1.E-08)RATMIX=1.E-08                                      OUTPB.78
          ABTK=THL(K)*PRATIO(K)                                                  OUTPB.79
C AVOID UPDATING TENDENCIES IN BOUNDARY ZONE                                     OUTPB.80
#ifndef MPP1                                                                     05DEC01.1527
          IF(I.GE.IBNES.AND.I.LE.IENES-1.AND.J.GE.JBNES.AND.J.LE.JENES-1         OUTPB.81
     +      )THEN                                                                OUTPB.82
#else                                                                            05DEC01.1528
          IF(FLIC_L2G_M(I).GE.IBNES.AND.FLIC_L2G_M(I).LE.IENES-1.AND.            05DEC01.866
     +       FLIC_L2G_N(J).GE.JBNES.AND.FLIC_L2G_N(J).LE.JENES-1                 05DEC01.867
     +      )THEN                                                                05DEC01.1529
#endif                                                                           05DEC01.1530
            T3DTEN(I,J,KK)=T3DTEN(I,J,KK)+PSB(I,J)*(ABTK-T3D(I,J,KK))/DT         OUTPB.83
C      TTENV(I,J,KK)=SCR1(I,KK)                                                  OUTPB.84
            QV3DTEN(I,J,KK)=QV3DTEN(I,J,KK)+PSB(I,J)*(RATMIX-                    OUTPB.85
     +                      QV3D(I,J,KK))/DT                                     OUTPB.86
          ENDIF                                                                  OUTPB.87
C      QVTENV(I,J,KK)=SCR2(I,KK)                                                 OUTPB.88
#ifndef MPP1                                                                     05DEC01.1531
          IF(I.GE.IBNES.AND.I.LE.IENES.AND.J.GE.JBNES.AND.J.LE.JENES)            OUTPB.89
     +      THEN                                                                 OUTPB.90
#else                                                                            05DEC01.1532
          IF(FLIC_L2G_M(I).GE.IBNES.AND.FLIC_L2G_M(I).LE.IENES.AND.              05DEC01.868
     +       FLIC_L2G_N(J).GE.JBNES.AND.FLIC_L2G_N(J).LE.JENES                   05DEC01.869
     +      )THEN                                                                05DEC01.870
#endif                                                                           05DEC01.1533
            UKK=.25*(U3D(I,J,KK)+U3D(I+1,J,KK)+U3D(I,J+1,KK)+                    OUTPB.91
     +          U3D(I+1,J+1,KK))                                                 OUTPB.92
            U3DTEN(I,J,KK)=U3DTEN(I,J,KK)+PSB(I,J)*(U(K)-UKK)/DT                 OUTPB.93
            VKK=.25*(V3D(I,J,KK)+V3D(I+1,J,KK)+V3D(I,J+1,KK)+                    OUTPB.94
     +          V3D(I+1,J+1,KK))                                                 OUTPB.95
            V3DTEN(I,J,KK)=V3DTEN(I,J,KK)+PSB(I,J)*(V(K)-VKK)/DT                 OUTPB.96
          ENDIF                                                                  OUTPB.97
        ENDIF                                                                    OUTPB.98
C      UTENV(I,J,KK)=UTEN(I,KK)                                                  OUTPB.99
C      VTEN(I,KK)=PSB(I,J)*(V(K)-0.25*(VB(I,J,KK)+VB(I+1,J,KK)+                  OUTPB.100
C    1          VB(I,J+1,KK)+VB(I+1,J+1,KK))/PSB(I,J))/DT                        OUTPB.101
C      VTENV(I,J,KK)=VTEN(I,KK)                                                  OUTPB.102
        PTHLSB(I,J,KK)=THLSB(K)                                                  OUTPB.103
        PTHLQWB(I,J,KK)=THLQWB(K)                                                OUTPB.104
        PQWSB(I,J,KK)=QWSB(K)                                                    OUTPB.105
        IF(K.EQ.II)GOTO 20                                                       OUTPB.106
        TURKE(I,J,KK)=TKE(K)                                                     OUTPB.107
   20 CONTINUE                                                                   OUTPB.108
C_FLIC_END_NOFLIC                                                                05DEC01.871
      RETURN                                                                     OUTPB.109
      END                                                                        OUTPB.110
                                                                                 OUTPB.111
