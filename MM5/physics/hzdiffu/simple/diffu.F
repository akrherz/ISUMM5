      SUBROUTINE DIFFU(KZZ,FTEN,F,PSB,XK,C203,                                   DIFFU.1
     &                 IBGN,IEND,JBGN,JEND,KL,INEST)                             DIFFU.2
      IMPLICIT NONE                                                              DIFFU.3
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          DIFFU.4
C                                                                     C          DIFFU.5
C                                                                     C          DIFFU.6
C     THIS SUBROUTINE COMPUTES THE DIFFUSION TERM FOR DECOUPLED       C          DIFFU.7
C     VARIABLE ON CONSTANT SIGMA SURFACE.                             C          DIFFU.8
C                                                                     C          DIFFU.9
C                                                                     C          DIFFU.10
C     IYY, JXX, KZZ : DIMENSIONS                                      C          DIFFU.11
C                                                                     C          DIFFU.12
C     FTEN    : TENDENCY FOR VARIABLE FB                              C          DIFFU.13
C                                                                     C          DIFFU.14
C     F       : DECOUPLED VARIABLE AT TIME T-1                        C          DIFFU.15
C                                                                     C          DIFFU.16
C     PSB     : P* AT THE POINTS WHERE FB IS DEFINED                  C          DIFFU.17
C                                                                     C          DIFFU.18
C     XK      : HORIZONTAL DIFFUSION COEFFICIENT                      C          DIFFU.19
C                                                                     C          DIFFU.20
C     C203    : 1./(DX*DX), DEFINED IN 'PARAM'                        C          DIFFU.21
C                                                                     C          DIFFU.22
C     IEND    : = ILXM FOR CROSS-POINT VARIABLES                      C          DIFFU.23
C               = ILX  FOR DOT-POINT   VARIABLES                      C          DIFFU.24
C                                                                     C          DIFFU.25
C     JEND    : = JLXM FOR CROSS-POINT VARIABLES                      C          DIFFU.26
C               = JLX  FOR DOT-POINT   VARIABLES                      C          DIFFU.27
C                                                                     C          DIFFU.28
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          DIFFU.29
#       include <parame.incl>                                                    DIFFU.30
C                                                                                DIFFU.31
      INTEGER   KZZ,IBGN,IEND,JBGN,JEND,KL,INEST                                 DIFFU.32
      REAL      FTEN(MIX,MJX,KZZ), XK(MIX,MJX,MKX),                              DIFFU.33
     1             F(MIX,MJX,KZZ),PSB(MIX,MJX)                                   DIFFU.34
      REAL      C203                                                             DIFFU.35
C                                                                                DIFFU.36
      INTEGER   I,J,K,IB,JB,IE,JE                                                DIFFU.37
C                                                                                DIFFU.38
C----------------------------------------------------------------------          DIFFU.39
C                                                                                DIFFU.40
C.....FOURTH-ORDER SCHEME FOR INTERIOR:                                          DIFFU.41
C                                                                                DIFFU.42
      IF(IBGN.GT.2)THEN                                                          DIFFU.43
        IB=IBGN                                                                  DIFFU.44
        JB=JBGN                                                                  DIFFU.45
        IE=IEND                                                                  DIFFU.46
        JE=JEND                                                                  DIFFU.47
      ELSE                                                                       DIFFU.48
        IB=IBGN+1                                                                DIFFU.49
        JB=JBGN+1                                                                DIFFU.50
        IE=IEND-1                                                                DIFFU.51
        JE=JEND-1                                                                DIFFU.52
      ENDIF                                                                      DIFFU.53
C                                                                                DIFFU.54
cmic$ do all autoscope                                                           DIFFU.55
cmic$1 shared(IB,IE,JB,JE,KL,                                                    DIFFU.56
cmic$1        F,FTEN,XK,PSB)                                                     DIFFU.57
cmic$2 private(I,J,K)                                                            DIFFU.58
c$doacross                                                                       DIFFU.59
c$& local(i,j,k)                                                                 DIFFU.60
c$omp parallel do default(shared)                                                DIFFU.61
c$omp&private(i,j,k)                                                             DIFFU.62
      DO J=JB,JE                                                                 DIFFU.63
        DO K=1,KL                                                                DIFFU.64
          DO I=IB,IE                                                             DIFFU.65
            FTEN(I,J,K)=FTEN(I,J,K)-XK(I,J,K)*C203*PSB(I,J)*(F(I,J+2,K)+         DIFFU.66
     +                  F(I,J-2,K)+F(I+2,J,K)+F(I-2,J,K)-4.*(F(I,J+1,K)+         DIFFU.67
     +                  F(I,J-1,K)+F(I+1,J,K)+F(I-1,J,K))+12.*F(I,J,K))          DIFFU.68
          ENDDO                                                                  DIFFU.69
        ENDDO                                                                    DIFFU.70
      ENDDO                                                                      DIFFU.71
C                                                                                DIFFU.72
C.....SECOND-ORDER SCHEME FOR BOUNDARIES:                                        DIFFU.73
C                                                                                DIFFU.74
      IF(INEST.GT.1)RETURN                                                       DIFFU.75
cmic$ do all autoscope                                                           DIFFU.76
cmic$1 shared(IBGN,IEND,JBGN,JEND,KL,                                            DIFFU.77
cmic$1        F,FTEN,XK,PSB)                                                     DIFFU.78
cmic$2 private(I,J,K)                                                            DIFFU.79
c$doacross                                                                       DIFFU.80
c$& local(i,j,k)                                                                 DIFFU.81
c$omp parallel do default(shared)                                                DIFFU.82
#ifndef MPP1                                                                     DIFFU.83
c$omp&private(i,j,k)                                                             DIFFU.84
#else                                                                            DIFFU.85
c$omp&private(i,j,k,jflict_0,jflict_1,jflict_2,jflict_3,                         DIFFU.86
c$omp&        jflict_4,jflict_5,iflict_6,iflict_7,iflict_8,                      DIFFU.87
c$omp&        iflict_9,iflict_10,iflict_11)                                      DIFFU.88
#endif                                                                           DIFFU.89
      DO K=1,KL                                                                  DIFFU.90
        J=JBGN                                                                   DIFFU.91
        DO I=IBGN,IEND                                                           DIFFU.92
          FTEN(I,J,K)=FTEN(I,J,K)+XK(I,J,K)*C203*PSB(I,J)*(F(I,J+1,K)+           DIFFU.93
     +                F(I,J-1,K)+F(I+1,J,K)+F(I-1,J,K)-4.*F(I,J,K))              DIFFU.94
        ENDDO                                                                    DIFFU.95
        J=JEND                                                                   DIFFU.96
        DO I=IBGN,IEND                                                           DIFFU.97
          FTEN(I,J,K)=FTEN(I,J,K)+XK(I,J,K)*C203*PSB(I,J)*(F(I,J+1,K)+           DIFFU.98
     +                F(I,J-1,K)+F(I+1,J,K)+F(I-1,J,K)-4.*F(I,J,K))              DIFFU.99
        ENDDO                                                                    DIFFU.100
        I=IBGN                                                                   19DEC02.1690
        DO J=JB,JE                                                               DIFFU.102
          FTEN(I,J,K)=FTEN(I,J,K)+XK(I,J,K)*C203*PSB(I,J)*(F(I,J+1,K)+           DIFFU.103
     +                F(I,J-1,K)+F(I+1,J,K)+F(I-1,J,K)-4.*F(I,J,K))              DIFFU.104
        ENDDO                                                                    DIFFU.105
        I=IEND                                                                   DIFFU.106
        DO J=JB,JE                                                               DIFFU.107
          FTEN(I,J,K)=FTEN(I,J,K)+XK(I,J,K)*C203*PSB(I,J)*(F(I,J+1,K)+           DIFFU.108
     +                F(I,J-1,K)+F(I+1,J,K)+F(I-1,J,K)-4.*F(I,J,K))              DIFFU.109
        ENDDO                                                                    DIFFU.110
      ENDDO                                                                      DIFFU.111
C                                                                                DIFFU.112
      RETURN                                                                     DIFFU.113
      END                                                                        DIFFU.114
                                                                                 DIFFU.115
