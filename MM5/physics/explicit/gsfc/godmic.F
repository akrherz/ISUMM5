      SUBROUTINE GODMIC(T3DTEN,QV3DTEN,QC3DTEN,QR3DTEN,                          GODMIC.1
     &                  QI3DTEN,QNI3DTEN,QG3DTEN,T3D,QV3D,                       GODMIC.2
     &                  QC3D,QR3D,QI3D,QNI3D,QG3D,PP3D,PSB,                      GODMIC.3
     &                  RAINNC,A,DSIGMA,PTOP,IN,J,IST,IEND)                      GODMIC.4
C                                                                                05DEC01.707
C***********************************************************************         05DEC01.708
C     RECENT CHANGES (6.25.01) BY SCOTT BRAUN INCLUDE                            05DEC01.709
C     INCLUSION OF A SMALL FALL SPEED FOR CLOUD ICE OF 20 CM/S                   05DEC01.710
C***********************************************************************         05DEC01.711
C                                                                                05DEC01.712
                                                                                 GODMIC.5
#     include <parame.incl>                                                      GODMIC.6
#     include <param2.incl>                                                      GODMIC.7
#     include <pmoist.incl>                                                      GODMIC.8
#     include <varia.incl>                                                       GODMIC.9
C     include <point3d.incl>                                                     GODMIC.10
C     include <point2d.incl>                                                     GODMIC.11
#     include <various.incl>                                                     GODMIC.12
C     include <nonhyd.incl>                                                      GODMIC.13
#     include <nhcnst.incl>                                                      GODMIC.14
#     include <rpstar.incl>                                                      GODMIC.15
                                                                                 GODMIC.16
      DIMENSION  T3D(MIX,MJX,MKX),        QV3D(MIX,MJX,MKX),                     GODMIC.17
     &          QC3D(MIXM,MJXM,MKXM),     QR3D(MIXM,MJXM,MKXM),                  GODMIC.18
     &          QI3D(MIXIC,MJXIC,MKXIC), QNI3D(MIXIC,MJXIC,MKXIC),               GODMIC.19
     &          PP3D(MIX,MJX,MKX),        QG3D(MIXIG,MJXIG,MKXIG),               GODMIC.20
     &           PSB(MIX,MJX),          RAINNC(MIX,MJX)                          GODMIC.21
                                                                                 GODMIC.22
      DIMENSION  T3DTEN(MIX,MJX,MKX),        QV3DTEN(MIX,MJX,MKX),               GODMIC.23
     &          QC3DTEN(MIXM,MJXM,MKXM),     QR3DTEN(MIXM,MJXM,MKXM),            GODMIC.24
     &          QI3DTEN(MIXIC,MJXIC,MKXIC), QNI3DTEN(MIXIC,MJXIC,MKXIC),         GODMIC.25
     &          QG3DTEN(MIXIG,MJXIG,MKXIG)                                       GODMIC.26
      DIMENSION A(MKX), DSIGMA(MKX)                                              GODMIC.27
                                                                                 GODMIC.28
C ARRAYS FOR GODDAD MICROPHYSICAL SCHEME:                                        GODMIC.29
      DIMENSION DPT5P(MIX,MKX),DQV5P(MIX,MKX),QCL5P(MIX,MKX),                    GODMIC.30
     1          QRN5P(MIX,MKX),QCI5P(MIX,MKX),QCS5P(MIX,MKX),                    GODMIC.31
     2          QCG5P(MIX,MKX),RHO5P(MIX,MKX),TA15P(MKX),                        GODMIC.32
     3          QA15P(MKX),P005P(MIX,MKX),PI05P(MIX,MKX)                         GODMIC.33
      DIMENSION DT5P(MIX,MKX),DZ(MIX,MKX),PRECIP(MIX),VFI(MIX,MKX)               05DEC01.713
     1         ,VFR(MIX,MKX),VFS(MIX,MKX),VFG(MIX,MKX)                           GODMIC.35
                                                                                 GODMIC.36
      KL=MKX                                                                     GODMIC.37
      DO 20 K=1,KL                                                               GODMIC.38
        QA15P(K)=0.0                                                             GODMIC.39
        TA15P(K)=0.0                                                             GODMIC.40
   20 CONTINUE                                                                   GODMIC.41
      DO 31 K=1,KL                                                               GODMIC.42
        DO 31 I=IST,IEND                                                         GODMIC.43
          VFR(I,K)=0.0                                                           GODMIC.44
          VFS(I,K)=0.0                                                           GODMIC.45
          VFG(I,K)=0.0                                                           GODMIC.46
          VFI(I,K)=20.0                                                          05DEC01.714
   31   CONTINUE                                                                 GODMIC.47
                                                                                 GODMIC.48
C                                                                                GODMIC.49
C OBTAIN UPDATED, AND UNCOUPLED VALUES FOR ALL VARIABLES                         GODMIC.50
C                                                                                GODMIC.51
      DO 30 K=1,KL                                                               GODMIC.52
        K3D=KL-K+1                                                               GODMIC.53
        DO 30 I=IST,IEND                                                         GODMIC.54
          DQV5P(I,K)=QV3D(I,J,K3D)+QV3DTEN(I,J,K3D)*DT*RPSB(I,J)                 GODMIC.55
          QCL5P(I,K)=QC3D(I,J,K3D)+QC3DTEN(I,J,K3D)*DT*RPSB(I,J)                 GODMIC.56
          QRN5P(I,K)=QR3D(I,J,K3D)+QR3DTEN(I,J,K3D)*DT*RPSB(I,J)                 GODMIC.57
          QCI5P(I,K)=QI3D(I,J,K3D)+QI3DTEN(I,J,K3D)*DT*RPSB(I,J)                 GODMIC.58
          QCS5P(I,K)=QNI3D(I,J,K3D)+QNI3DTEN(I,J,K3D)*DT*RPSB(I,J)               GODMIC.59
          QCG5P(I,K)=QG3D(I,J,K3D)+QG3DTEN(I,J,K3D)*DT*RPSB(I,J)                 GODMIC.60
          DT5P(I,K)=T3D(I,J,K3D)+T3DTEN(I,J,K3D)*DT*RPSB(I,J)                    GODMIC.61
   30   CONTINUE                                                                 GODMIC.62
                                                                                 GODMIC.63
C                                                                                GODMIC.64
C P00 (UB), DPT (K), RHO (G/CM**3), DZ (CM), PS (CB)                             GODMIC.65
C                                                                                GODMIC.66
      DO 40 K=1,KL                                                               GODMIC.67
        K3D=KL-K+1                                                               GODMIC.68
        DO 40 I=IST,IEND                                                         GODMIC.69
          P005P(I,K)=(A(K3D)*PSB(I,J)+PTOP)*10000.+PP3D(I,J,K3D)*10.0            GODMIC.70
          PI05P(I,K)=(P005P(I,K)/1.0E6)**0.2857541050                            GODMIC.71
          DPT5P(I,K)=DT5P(I,K)/PI05P(I,K)                                        GODMIC.72
          RHO5P(I,K)=0.348*P005P(I,K)/DT5P(I,K)/1.0E6                            GODMIC.73
          DZ(I,K)=PSB(I,J)*DSIGMA(K3D)*10000./(RHO5P(I,K)*981.)                  GODMIC.74
   40   CONTINUE                                                                 GODMIC.75
                                                                                 GODMIC.76
C                                                                                GODMIC.77
C CALL GODDARD MICROPHYSICAL SCHEME                                              GODMIC.78
C                                                                                GODMIC.79
      CALL SATICE(DT,DPT5P,DQV5P,QCL5P,QRN5P,QCI5P,QCS5P,QCG5P,RHO5P,            GODMIC.80
     +     TA15P,QA15P,P005P,PI05P,VFR,VFS,VFG,PRECIP,IST,IEND,KL)               05DEC01.715
C                                                                                GODMIC.82
C FALL DOWN OF PRECIPI PARTICLES                                                 GODMIC.83
C                                                                                GODMIC.84
      CALL FALFLUX(QRN5P,VFR,RHO5P,DZ,KL,DT,IST,IEND)                            GODMIC.85
      CALL FALFLUX(QCS5P,VFS,RHO5P,DZ,KL,DT,IST,IEND)                            GODMIC.86
      CALL FALFLUX(QCG5P,VFG,RHO5P,DZ,KL,DT,IST,IEND)                            GODMIC.87
      CALL FALFLUX(QCI5P,VFI,RHO5P,DZ,KL,DT,IST,IEND)                            05DEC01.716
      DO 80 K=1,KL                                                               GODMIC.88
        DO 80 I=IST,IEND                                                         GODMIC.89
          DT5P(I,K)=DPT5P(I,K)*PI05P(I,K)                                        GODMIC.90
   80   CONTINUE                                                                 GODMIC.91
                                                                                 GODMIC.92
C                                                                                GODMIC.93
C OBTAIN COUPLED TENDENCY TERMS FOR ALL VARIABLES                                GODMIC.94
C                              GODMIC.81                                         GODMIC.95
      DO 90 K=1,KL                                                               GODMIC.96
        K3D=KL-K+1                                                               GODMIC.97
        DO 90 I=IST,IEND                                                         GODMIC.98
          T3DTEN(I,J,K3D)=((DT5P(I,K)-T3D(I,J,K3D))*PSB(I,J)/DT-                 GODMIC.99
     +                    T3DTEN(I,J,K3D))*(1-IFDRY)+T3DTEN(I,J,K3D)             GODMIC.100
          QV3DTEN(I,J,K3D)=(DQV5P(I,K)-QV3D(I,J,K3D))*PSB(I,J)/DT                GODMIC.101
          QC3DTEN(I,J,K3D)=(QCL5P(I,K)-QC3D(I,J,K3D))*PSB(I,J)/DT                GODMIC.102
          QR3DTEN(I,J,K3D)=(QRN5P(I,K)-QR3D(I,J,K3D))*PSB(I,J)/DT                GODMIC.103
          QI3DTEN(I,J,K3D)=(QCI5P(I,K)-QI3D(I,J,K3D))*PSB(I,J)/DT                GODMIC.104
          QNI3DTEN(I,J,K3D)=(QCS5P(I,K)-QNI3D(I,J,K3D))*PSB(I,J)/DT              GODMIC.105
          QG3DTEN(I,J,K3D)=(QCG5P(I,K)-QG3D(I,J,K3D))*PSB(I,J)/DT                GODMIC.106
   90   CONTINUE                                                                 GODMIC.107
                                                                                 GODMIC.108
C                                                                                GODMIC.109
C CALCULATE RAINFALL: RAINNC (CM)                                                GODMIC.110
C                                                                                GODMIC.111
      DO 100 I=IST,IEND                                                          GODMIC.112
        IF(PRECIP(I).LT.0.)THEN                                                  GODMIC.113
          PRINT *,'NEGATIVE PRECIP AT I,J = ',I,J,PRECIP(I)                      GODMIC.114
          PRECIP(I)=0.                                                           GODMIC.115
        ENDIF                                                                    GODMIC.116
        RAINNC(I,J)=RAINNC(I,J)+PRECIP(I)*DTMIN*60.                              GODMIC.117
  100 CONTINUE                                                                   GODMIC.118
      RETURN                                                                     GODMIC.119
      END                                                                        GODMIC.120
                                                                                 GODMIC.121
