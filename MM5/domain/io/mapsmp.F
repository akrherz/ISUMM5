      SUBROUTINE MAPSMP(FLD,IYY,JXX,IA,IB,INY,JA,JB,JNX,CONST,                   MAPSMP.1
     1                  ICHOS,NAME,TIME)                                         MAPSMP.2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          MAPSMP.3
C                                                                     C          MAPSMP.4
C                                                                     C          MAPSMP.5
C     THIS SUBROUTINE PRINTS A SAMPLE OF A TWO-DIMENSIONAL DATA FIELD C          MAPSMP.6
C     ON THE LINE PRINTER WITH 5 SIGNIFICANT DIGITS.                  C          MAPSMP.7
C                                                                     C          MAPSMP.8
C     *** NOTE *** THE VALUES OF FLD(I,J) SHOULD BE LIMITED WITHIN    C          MAPSMP.9
C                  1.E30 --- 1.E-30. IF THE VALUE OUTSIDE THIS        C          MAPSMP.10
C                  RANGE IS DESIRED, THE PROGRAM SHOULD BE CHANGED    C          MAPSMP.11
C                  ACCORDINGLY (IN DO LOOP 20).                       C          MAPSMP.12
C                                                                     C          MAPSMP.13
C                                                                     C          MAPSMP.14
C     FLD    : A TWO-DIMENSIONAL ARRAY TO HOLD THE DATA FIELD TO BE   C          MAPSMP.15
C              SAMPLED AND PRINTED. FLD COULD BE A HORIZONTAL SLICE,  C          MAPSMP.16
C              FLD(I,J), OR A VERTICAL SLICE FLD(K,I) OR FLD(K,J).    C          MAPSMP.17
C                                                                     C          MAPSMP.18
C     IYY    : THE FIRST DIMENSION OF FLD.                            C          MAPSMP.19
C              FOR THE HORIZONTAL SLICE, IYY IS THE DIMENSION IN THE  C          MAPSMP.20
C                                        Y DIRECTION.                 C          MAPSMP.21
C              FOR THE VERTICAL SLICE, IYY IS THE DIMENSION IN THE    C          MAPSMP.22
C                                      Z DIRECTION.                   C          MAPSMP.23
C                                                                     C          MAPSMP.24
C     JXX    : THE SECOND DIMENSION OF FLD.                           C          MAPSMP.25
C              FOR THE HORIZONTAL SLICE, JXX IS THE DIMENSION IN THE  C          MAPSMP.26
C                                        X DIRECTION.                 C          MAPSMP.27
C              FOR THE VERTICAL SLICE, JXX IS THE DIMENSION IN EITHER C          MAPSMP.28
C                                      THE X OR Y DIRECTION.          C          MAPSMP.29
C                                                                     C          MAPSMP.30
C     IA     : INITIAL SAMPLING POINT IN THE FIRST DIMENSION.         C          MAPSMP.31
C                                                                     C          MAPSMP.32
C     IB     : FINAL SAMPLING POINT IN THE FIRST DIMENSION.           C          MAPSMP.33
C                                                                     C          MAPSMP.34
C     INY    : SAMPLING INTERVAL IN THE FIRST DIMENSION.              C          MAPSMP.35
C                                                                     C          MAPSMP.36
C     JA     : INITIAL SAMPLING POINT IN THE SECOND DIMENSION.        C          MAPSMP.37
C                                                                     C          MAPSMP.38
C     JB     : FINAL SAMPLING POINT IN THE SECOND DIMENSION.          C          MAPSMP.39
C                                                                     C          MAPSMP.40
C     JNX    : SAMPLING INTERVAL IN THE SECOND DIMENSION.             C          MAPSMP.41
C                                                                     C          MAPSMP.42
C     CONST  : CONSTANT USED TO BE SUBTRACTED FROM FLD.               C          MAPSMP.43
C                                                                     C          MAPSMP.44
C     ICHOS > 0 : FOR HORIZONTAL ARRAY FLD(Y,X)                       C          MAPSMP.45
C           < 0 : FOR VERTICAL CROSS SECTION FLD(Z,Y) OR FLD(Z,X)     C          MAPSMP.46
C                                                                     C          MAPSMP.47
C                                                                     C          MAPSMP.48
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          MAPSMP.49
C                                                                                MAPSMP.50
      DIMENSION FLD(JXX,IYY)                                                     MAPSMP.51
      CHARACTER*21 JFMT1,JFMT2                                                   MAPSMP.52
      CHARACTER*40 NAME                                                          MAPSMP.53
      DIMENSION JM(50)                                                           MAPSMP.54
      DATA KSIGT/5/                                                              MAPSMP.55
C                                                                                MAPSMP.56
      CALL VTRAN(FLD,IYY,JXX)                                                    MAPSMP.57
C                                                                                MAPSMP.58
      N=6                                                                        MAPSMP.59
      K1=KSIGT+2                                                                 MAPSMP.60
      K2=124/K1                                                                  MAPSMP.61
      K3=KSIGT/2                                                                 MAPSMP.62
      K4=KSIGT-K3                                                                MAPSMP.63
C                                                                                MAPSMP.64
C                                                                                MAPSMP.65
      FLDMAX=FLD(JA,IA)                                                          MAPSMP.66
      FLDMIN=FLD(JA,IA)                                                          MAPSMP.67
      FLDU=10.**KSIGT                                                            MAPSMP.68
      FLDL=10.**(KSIGT-1)                                                        MAPSMP.69
      DO 20 J=JA,JB,JNX                                                          MAPSMP.70
        DO 20 I=IA,IB,INY                                                        MAPSMP.71
          AFLDJI=ABS(FLD(J,I)-CONST)                                             MAPSMP.72
          IF((AFLDJI.GT.1.E30).OR.(AFLDJI.LT.1.E-30))GOTO 20                     MAPSMP.73
          IF(AFLDJI.GT.FLDMAX)FLDMAX=AFLDJI                                      MAPSMP.74
          IF(AFLDJI.LT.FLDMIN)FLDMIN=AFLDJI                                      MAPSMP.75
   20   CONTINUE                                                                 MAPSMP.76
                                                                                 MAPSMP.77
      IF(FLDMAX.EQ.FLDMIN)GOTO 150                                               MAPSMP.78
      IEXP=0                                                                     MAPSMP.79
      DO 50 N1=1,500                                                             MAPSMP.80
        IF(FLDMAX.GE.FLDU)GOTO 30                                                MAPSMP.81
        IF(FLDMAX.LT.FLDL)GOTO 40                                                MAPSMP.82
        IF(FLDMAX.LT.FLDU.AND.FLDMAX.GE.FLDL)GOTO 60                             MAPSMP.83
   30   FLDMAX=FLDMAX/10.                                                        MAPSMP.84
        IEXP=IEXP-1                                                              MAPSMP.85
        GOTO 50                                                                  MAPSMP.86
   40   FLDMAX=FLDMAX*10.                                                        MAPSMP.87
        IEXP=IEXP+1                                                              MAPSMP.88
   50 CONTINUE                                                                   MAPSMP.89
   60 CONTINUE                                                                   MAPSMP.90
      RMULT=10.**IEXP                                                            MAPSMP.91
      JX=JB-JA+1                                                                 MAPSMP.92
      IY=IB-IA+1                                                                 MAPSMP.93
      JN=K2*JNX                                                                  MAPSMP.94
      JN1=JN-1                                                                   MAPSMP.95
      WRITE (N,80)NAME,IEXP,TIME                                                 MAPSMP.96
   80 FORMAT(////' THIS IS A LIST OF  ',A40,'  ,SCALED BY  1.E',I3,5X,           MAPSMP.97
     1       'AT TIME = ',F10.3)                                                 MAPSMP.98
      DO 130 J1=JA,JB,JN                                                         MAPSMP.99
        JL=MIN0(J1+JN1,JB)                                                       MAPSMP.100
        JT=JL-J1+1                                                               MAPSMP.101
        JTN=(JT-1)/JNX+1                                                         MAPSMP.102
        J2N=0                                                                    MAPSMP.103
        DO 90 J2=1,JT,JNX                                                        MAPSMP.104
          J2N=J2N+1                                                              MAPSMP.105
   90   JM(J2N)=J1+J2-1                                                          MAPSMP.106
        WRITE (JFMT1(1:21),FMT=100)JTN,K4,K3                                     MAPSMP.107
C      ENCODE(21,100,JFMT1) JTN,K4,K3                                            MAPSMP.108
  100   FORMAT (5H(/4X,,I2,1H(,I2,5HX,I2,,I2,4HX)/))                             MAPSMP.109
        WRITE (N,JFMT1)(JM(JJ),JJ=1,J2N)                                         MAPSMP.110
        WRITE (JFMT2(1:21),FMT=110)JTN,K1                                        MAPSMP.111
C      ENCODE(21,110,JFMT2) JTN,K1                                               MAPSMP.112
  110   FORMAT (7H(1X,I2,,I2,1HF,I2,9H.0,2X,I2))                                 MAPSMP.113
        IT=(IY-1)/INY                                                            MAPSMP.114
        IR=IY-IT*INY                                                             MAPSMP.115
        DO 120 I2=IA,IB,INY                                                      MAPSMP.116
          I1=IB+IA-I2-IR+1                                                       MAPSMP.117
          IF(ICHOS.LT.0)I1=I2                                                    MAPSMP.118
          WRITE (N,JFMT2)I1,((FLD(J3,I1)-CONST)*RMULT,J3=J1,JL,JNX),I1           MAPSMP.119
  120   CONTINUE                                                                 MAPSMP.120
  130 WRITE (N,JFMT1)(JM(JJ),JJ=1,J2N)                                           MAPSMP.121
      GOTO 180                                                                   MAPSMP.122
C                                                                                MAPSMP.123
  150 CONTINUE                                                                   MAPSMP.124
      WRITE (N,170)NAME,FLDMAX,TIME                                              MAPSMP.125
  170 FORMAT(/'   ALL OF THE VALUES OF ',A40,' ARE EQUAL TO ',E15.5,5X,          MAPSMP.126
     1       'AT TIME = ',F10.3)                                                 MAPSMP.127
  180 CONTINUE                                                                   MAPSMP.128
C                                                                                MAPSMP.129
      CALL VTRAN(FLD,JXX,IYY)                                                    MAPSMP.130
C                                                                                MAPSMP.131
      RETURN                                                                     MAPSMP.132
      END                                                                        MAPSMP.133
                                                                                 MAPSMP.134
