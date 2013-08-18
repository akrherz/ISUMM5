      SUBROUTINE OUTPT(FLD,IYY,IA,IB,INY,JXX,JA,JB,JNX,KSIGT,NAME)               OUTPT.1
C                                                                                OUTPT.2
C   PURPOSE      : PRINTS OUT A SAMPLE OF THE FIELDS (ON MESOSCALE               OUTPT.3
C                  GRID).                                                        OUTPT.4
C   ON ENTRY     :                                                               OUTPT.5
C   FLD(IYY,JXX) : ARRAY HOLDS THE FIELD ON THE MESOSCALE GRID.                  OUTPT.6
C   IA, IB       : INITIAL AND FINAL SAMPLING POINTS IN THE I DIRECTION.         OUTPT.7
C   JA, JB       : INITIAL AND FINAL SAMPLING POINTS IN THE J DIRECTION.         OUTPT.8
C   INY, JNX     : SAMPLING INTERVALS IN THE I AND J DIRECTIONS.                 OUTPT.9
C   KSIGT        : NUMBER OF SIGNIFICANT DIGITS TO BE PRINTED FOR EACH           OUTPT.10
C                  DATA POINT.                                                   OUTPT.11
C   NAME         : HOLLERITH IDENTIFIER OF VARIABLE TO PRINT.                    OUTPT.12
C                                                                                OUTPT.13
C   CALLS THE SUBROUTINE VTRAN.                                                  OUTPT.14
C   CALLED BY SUBROUTINES LATLON, TFUDGE, FINPRT.                                OUTPT.15
C                                                                                OUTPT.16
C   COMMENTS :                                                                   OUTPT.17
C THE NUMBER OF DIGITS USED TO REPRESENT EACH PRINTED VALUE IS ASSIGNED          OUTPT.18
C FROM KSIGT.  THE FIELD IS AUTOMATICALLY SCALED SO THAT THE LARGEST             OUTPT.19
C VALUE (ABSOLUTE) WILL HAVE KSIGT DIGITS.  VALUES MORE THAN KSIGT               OUTPT.20
C ORDERS OF MAGNITUDE LESS THAN THE LARGEST ABSOLUTE VALUE WILL BE               OUTPT.21
C PRINTED AS A POSITIVE OR NEGATIVE ZERO. DATA POINTS MAY BE EXCLUDED            OUTPT.22
C FROM THE SCALING PROCESS BY SETTING THE ABSOLUTE VALUES GREATER THAN           OUTPT.23
C 1.0 X 10**30.  AS THE SCALED DATA VALUES ARE PRINTED, LABELS SHOWING           OUTPT.24
C THE I AND J INDICES ARE PRINTED AT THE BOUNDARIES OF THE FIELD.                OUTPT.25
C A CAPTION IS WRITTEN ABOVE THE DATA TO IDENTIFY THE VARIABLE WITH A            OUTPT.26
C CHARACTER STRING AND TO GIVE THE SCALING POWER APPLIED TO THE FIELD.           OUTPT.27
C SUBROUTINE VTRAN (NOT DISCUSSED) IS USED TO TRANSPOSE THE DATA FIELD           OUTPT.28
C ARRAY FLD, BEFORE AND AFTER THE SCALING AND PRINTING OPERATIONS                OUTPT.29
C OF OUTPT.                                                                      OUTPT.30
C                                                                                OUTPT.31
      DIMENSION FLD(JXX,IYY),IFMT1(6),IFMT2(6),JM(1000)                          OUTPT.32
      CHARACTER*8 NAME                                                           OUTPT.33
      CHARACTER*21 JFMT1,JFMT2                                                   OUTPT.34
C                                                                                OUTPT.35
      CALL VTRAN(FLD,IYY,JXX)                                                    OUTPT.36
      K1 = KSIGT+2                                                               OUTPT.37
      IWIDTH=132                                                                 OUTPT.38
C     IWIDTH= 80                                                                 OUTPT.39
      K2 = (IWIDTH-8)/K1                                                         OUTPT.40
      K3 = KSIGT/2                                                               OUTPT.41
      K4 = KSIGT-K3                                                              OUTPT.42
C                                                                                OUTPT.43
C-----SCALING:                                                                   OUTPT.44
C                                                                                OUTPT.45
      FLDMAX = 0.                                                                OUTPT.46
      FLDU   = 10.**KSIGT                                                        OUTPT.47
      FLDL   = 10.**(KSIGT-1)                                                    OUTPT.48
      DO 10 J=JA,JB,JNX                                                          OUTPT.49
      DO 10 I=IA,IB,INY                                                          OUTPT.50
      IF (ABS(FLD(J,I)) .GT. 1.E30 .OR. ABS(FLD(J,I)) .LT. 1.E-30)               OUTPT.51
     1     GO TO 10                                                              OUTPT.52
      IF (ABS(FLD(J,I)) .GT. FLDMAX) FLDMAX=ABS(FLD(J,I))                        OUTPT.53
   10 CONTINUE                                                                   OUTPT.54
      IF (FLDMAX .NE. 0.) THEN                                                   OUTPT.55
         IEXP = 0                                                                OUTPT.56
         DO 20 N1=1,500                                                          OUTPT.57
         IF (FLDMAX .GE. FLDU) THEN                                              OUTPT.58
            FLDMAX = FLDMAX/10.                                                  OUTPT.59
            IEXP   = IEXP-1                                                      OUTPT.60
         ENDIF                                                                   OUTPT.61
         IF (FLDMAX .LT. FLDL) THEN                                              OUTPT.62
            FLDMAX = FLDMAX*10.                                                  OUTPT.63
            IEXP   = IEXP+1                                                      OUTPT.64
         ENDIF                                                                   OUTPT.65
         IF (FLDMAX .LT. FLDU .AND. FLDMAX .GE. FLDL) GO TO 30                   OUTPT.66
   20    CONTINUE                                                                OUTPT.67
   30    CONTINUE                                                                OUTPT.68
      POWER=10.**IEXP                                                            OUTPT.69
C                                                                                OUTPT.70
         JX  = JB-JA+1                                                           OUTPT.71
         IY  = IB-IA+1                                                           OUTPT.72
         JN  = K2*JNX                                                            OUTPT.73
         JN1 = JN-1                                                              OUTPT.74
         WRITE(6,50) NAME,IEXP                                                   OUTPT.75
   50    FORMAT(////' THIS IS A LIST OF   ',A8,'    ,SCALED BY  1.E',I3)         OUTPT.76
         DO 100 J1=JA,JB,JN                                                      OUTPT.77
         JL  = MIN0(J1+JN1,JB)                                                   OUTPT.78
         JT  = JL-J1+1                                                           OUTPT.79
         JTN = (JT-1)/JNX+1                                                      OUTPT.80
         DO 60 J2=1,JT,JNX                                                       OUTPT.81
   60    JM(J2) = J1+J2-1                                                        OUTPT.82
C                                                                                OUTPT.83
         WRITE (JFMT1(1:21),FMT=70)JTN,K4-1,K3                                   05DEC01.8
   70    FORMAT(5H(/4X,,I2,1H(,I2,5HX,I3,,I2,4HX)/))                             05DEC01.9
         WRITE(6,JFMT1) (JM(JJ),JJ=1,JT,JNX)                                     OUTPT.86
C                                                                                OUTPT.87
         WRITE (JFMT2(1:21),FMT=80)JTN,K1                                        OUTPT.88
   80    FORMAT(4H(I3,,I2,1HF,I2,9H.0,2X,I3))                                 O  05DEC01.10
         IT = (IY-1)/INY                                                         OUTPT.90
         IR = IY-IT*INY                                                          OUTPT.91
         DO 90 I2=IA,IB,INY                                                      OUTPT.92
         I1 = IA+IB-I2-IR+1                                                      OUTPT.93
         WRITE(6,JFMT2)I1,(FLD(J3,I1)*POWER,J3 = J1,JL,JNX),I1                   OUTPT.94
   90    CONTINUE                                                                OUTPT.95
  100    WRITE(6,JFMT1) (JM(JJ),JJ=1,JT,JNX)                                     OUTPT.96
         CALL VTRAN(FLD,JXX,IYY)                                                 OUTPT.97
         RETURN                                                                  OUTPT.98
      ENDIF                                                                      OUTPT.99
      PRINT 120,FLDMAX                                                           OUTPT.100
  120 FORMAT(/'     ALL THE VALUES OF THE ARRAY ARE EQUAL TO ',F10.3/)           OUTPT.101
      RETURN                                                                     OUTPT.102
      END                                                                        OUTPT.103
