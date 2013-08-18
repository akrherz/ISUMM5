      SUBROUTINE CONVAD(ISTART,IEND,KL,NUMNES,J,                                 CONVAD.1
     &                  TC,QVC,PSC)                                              CONVAD.2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          CONVAD.3
C                                                                     C          CONVAD.4
C                                                                     C          CONVAD.5
C     THIS SUBROUTINE PERFORMS DRY CONVECTIVE ADJUSTMENT WHENEVER     C          CONVAD.6
C     THE VERTICAL DERIVATIVE OF POTENTIAL TEMPERATURE WITH RESPECT   C          CONVAD.7
C     TO PRESSURE IS GREATER THAN DTDPC.                              C          CONVAD.8
C                                                                     C          CONVAD.9
C                                                                     C          CONVAD.10
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          CONVAD.11
#     include <parame.incl>                                                      CONVAD.12
C                                                                                CONVAD.13
      DIMENSION TC(MIX,MJX,MKX), QVC(MIX,MJX,MKX), PSC(MIX,MJX)                  CONVAD.14
      DIMENSION P(MKX),PI(MKX),S(MKX),AX(MKX),B(MKX),C(MKX),D(MKX),              CONVAD.15
     1          Y(MKX),THETA(MKX),Q(MKX)                                         CONVAD.16
#     include <param3.incl>                                                      CONVAD.17
#     include <param2.incl>                                                      CONVAD.18
#     include <pmoist.incl>                                                      CONVAD.19
#     include <nonhyd.incl>                                                      CONVAD.20
#     include <rpstar.incl>                                                      CONVAD.21
C                                                                                CONVAD.22
C---------------------------------------------------------------------           CONVAD.23
C                                                                                CONVAD.24
      DATA DTDPC/-0.03/                                                          CONVAD.25
C                                                                                CONVAD.26
C---------------------------------------------------------------------           CONVAD.27
C                                                                                CONVAD.28
      KOUNT=0                                                                    CONVAD.29
      DTDPC1=0.95*DTDPC                                                          CONVAD.30
C                                                                                CONVAD.31
C-----BEGIN ADJUSTMENT:                                                          CONVAD.32
C                                                                                CONVAD.33
      DO 100 I=ISTART,IEND                                                       CONVAD.34
        NADJ=0                                                                   CONVAD.35
        NCOUNT=0                                                                 CONVAD.36
C                                                                                CONVAD.37
C-----COMPUTE POTENTIAL TEMPERATURE:                                             CONVAD.38
C                                                                                CONVAD.39
        DO 10 K=1,KL                                                             CONVAD.40
          P(K)=PSC(I,J)*A(K)+PTOP+PPA(I,J,K)*RPSC(I,J)*0.001                     CONVAD.41
          PI(K)=(100./P(K))**0.286                                               CONVAD.42
          S(K)=TC(I,J,K)*RPSC(I,J)                                               CONVAD.43
          THETA(K)=S(K)*PI(K)                                                    CONVAD.44
   10   CONTINUE                                                                 CONVAD.45
        IF(IDRY(NUMNES).EQ.0)THEN                                                CONVAD.46
          DO 15 K=1,KL                                                           CONVAD.47
            Q(K)=QVC(I,J,K)*RPSC(I,J)                                            CONVAD.48
   15     CONTINUE                                                               CONVAD.49
        ENDIF                                                                    CONVAD.50
C                                                                                CONVAD.51
C-----DETERMINE LAYERS TO BE ADJUSTED:                                           CONVAD.52
C                                                                                CONVAD.53
   20   CONTINUE                                                                 CONVAD.54
        NCOUNT=NCOUNT+1                                                          CONVAD.55
C                                                                                CONVAD.56
        DO 40 KK=2,KL                                                            CONVAD.57
          K=KL-KK+1                                                              CONVAD.58
C.....K RANGES FROM KL-1 TO 1.                                                   CONVAD.59
          DTDP=(THETA(K+1)-THETA(K))/(P(K+1)-P(K))                               CONVAD.60
          IF(DTDP.LT.DTDPC1)GOTO 40                                              CONVAD.61
          KE=K+1                                                                 CONVAD.62
          KB=K                                                                   CONVAD.63
   30     IF(KB.EQ.1)GOTO 50                                                     CONVAD.64
C                                                                                CONVAD.65
C-----AN UNSTABLE LAYER HAS BEEN FOUND. CHECK TO SEE IF MULTIPLE-LAYER           CONVAD.66
C     ADJUSTMENT IS NEEDED.                                                      CONVAD.67
C                                                                                CONVAD.68
          DTDP=(THETA(KE)-THETA(KB-1))/(P(KE)-P(KB-1))                           CONVAD.69
          IF(DTDP.LT.DTDPC1)GOTO 50                                              CONVAD.70
          KB=KB-1                                                                CONVAD.71
          GOTO 30                                                                CONVAD.72
   40   CONTINUE                                                                 CONVAD.73
        GOTO 70                                                                  CONVAD.74
   50   CONTINUE                                                                 CONVAD.75
        IF(NCOUNT.GT.20)THEN                                                     CONVAD.76
          PRINT125,NCOUNT,I,J                                                    CONVAD.77
 125  FORMAT('******* NUMBER OF ITERATIONS IN CONVAD EXCEEDS 20; NCOUNT'         CONVAD.78
     * ,', I, J =',3(1X,I4))                                                     CONVAD.79
          STOP 111                                                               CONVAD.80
        ENDIF                                                                    CONVAD.81
C                                                                                CONVAD.82
C-----AN UNSTABLE LAYER HAS BEEN FOUND. ADJUST LAYERS BETWEEN KB AND KE          CONVAD.83
C     INCLUSIVE.                                                                 CONVAD.84
C                                                                                CONVAD.85
        CALL CADJMX(KB,KE,PI,S,NADJ,DSIGMA,DTDPC,P,AX,B,C,D,Y,KL,MKX)            CONVAD.86
C                                                                                CONVAD.87
C-----PUT ADJUSTED SOUNDING INTO S AND THETA. CHECK NEW THETA FOR                CONVAD.88
C     UNSTABLE LAYERS.                                                           CONVAD.89
C                                                                                CONVAD.90
        DO 60 N=1,NADJ                                                           CONVAD.91
          K=KE-NADJ+N                                                            CONVAD.92
          S(K)=S(K)+Y(N)                                                         CONVAD.93
          THETA(K)=S(K)*PI(K)                                                    CONVAD.94
   60   CONTINUE                                                                 CONVAD.95
        IF(IDRY(NUMNES).EQ.0)THEN                                                CONVAD.96
          TQQ=0.0                                                                CONVAD.97
          TDS=0.0                                                                CONVAD.98
          DO 62 N=1,NADJ                                                         CONVAD.99
            K=KE-NADJ+N                                                          CONVAD.100
            TQQ=TQQ+Q(K)*DSIGMA(K)                                               CONVAD.101
            TDS=TDS+DSIGMA(K)                                                    CONVAD.102
   62     CONTINUE                                                               CONVAD.103
          QADJ=TQQ/TDS*PSC(I,J)                                                  CONVAD.104
          DO 64 N=1,NADJ                                                         CONVAD.105
            K=KE-NADJ+N                                                          CONVAD.106
            QVC(I,J,K)=QADJ                                                      CONVAD.107
   64     CONTINUE                                                               CONVAD.108
        ENDIF                                                                    CONVAD.109
        GOTO 20                                                                  CONVAD.110
   70   CONTINUE                                                                 CONVAD.111
C                                                                                CONVAD.112
C-----IF ANY ADJUSTMENTS HAVE BEEN MADE, DEFINE NEW TC IN TERMS OF S.            CONVAD.113
C                                                                                CONVAD.114
        IF(NADJ.EQ.0)GOTO 90                                                     CONVAD.115
        KOUNT=KOUNT+1                                                            CONVAD.116
        DO 80 K=1,KL                                                             CONVAD.117
          TC(I,J,K)=S(K)*PSC(I,J)                                                CONVAD.118
   80   CONTINUE                                                                 CONVAD.119
   90   CONTINUE                                                                 CONVAD.120
  100 CONTINUE                                                                   CONVAD.121
C                                                                                CONVAD.122
C-----PRINT THE ADJUSTMENT INFORMATION:                                          CONVAD.123
C                                                                                CONVAD.124
C*****PRINT 110,KOUNT,KTAU                                                       CONVAD.125
  110 FORMAT(10X,'NUMBER OF POINTS ADJUSTED = ',I5,'  AT KTAU = ',I6)            CONVAD.126
C                                                                                CONVAD.127
      RETURN                                                                     CONVAD.128
      END                                                                        CONVAD.129
                                                                                 CONVAD.130
