      SUBROUTINE NESTBDY(TC,TF,TFSINT,IYY,JXX,IYYF,JXXF,                         NESTBDY.1
     -                   ISOUTH,JWEST,IRATIOF,MIXX,MJXX,PFLAG,IB)                NESTBDY.2
C                                                                                NESTBDY.3
C  IB -- 0, ALL  1 TO 7 ROWS/COLUMNS ARE AFFECTED.                               NESTBDY.4
C        1, ONLY 1 TO 4 ROWS/COLUMNS ARE AFFECTED,                               NESTBDY.5
C           NO BLENDING FOR 5 TO 7 ROWS/COLUMNS                                  NESTBDY.6
C                                                                                NESTBDY.7
# include <parame.incl>                                                          NESTBDY.8
      PARAMETER (KSIGT=6, ICRSDOT=1, NF=9)                                       NESTBDY.9
      DIMENSION WORK(NF*NSIZE)                                                   NESTBDY.10
      CHARACTER*10 NAME                                                          NESTBDY.11
      LOGICAL PFLAG                                                              NESTBDY.12
      DIMENSION TC(IYY,JXX), TF(IYYF,JXXF), TFSINT(IYYF,JXXF)                    NESTBDY.13
                                                                                 NESTBDY.14
       IF(IRATIOF.EQ.3) THEN                                                     NESTBDY.15
C=======================================================================         NESTBDY.16
C                                                                                NESTBDY.17
C     INTERPOLATE FROM CG TERRAIN (TC) TO MAKE AN ADDITIONAL FG TERRAIN          NESTBDY.18
C         FIELD (TFSINT).                                                        NESTBDY.19
C                                                                                NESTBDY.20
      CALL EXAINT (TC,IYY,JXX,TFSINT,IYYF,JXXF,ISOUTH,JWEST,                     NESTBDY.21
     1             ICRSDOT,MIXX,MJXX,WORK)                                       NESTBDY.22
      IF (PFLAG) THEN                                                            NESTBDY.23
         NAME='TF_SINT'                                                          NESTBDY.24
         CALL OUTPT(TFSINT,IYYF,1,IYYF-1,1,JXXF,1,JXXF-1,1,KSIGT,NAME)           NESTBDY.25
      ENDIF                                                                      NESTBDY.26
                                                                                 NESTBDY.27
       ELSE                                                                      NESTBDY.28
                                                                                 NESTBDY.29
C ONE WAY, USE BINT TO INTERPOLATE THE COARSE MESH TERRAIN TO FINE MESH          NESTBDY.30
       DO 30 I=1,IYYF                                                            NESTBDY.31
       DO 30 J=1,JXXF                                                            NESTBDY.32
          YY=FLOAT(ISOUTH)-0.5 + (FLOAT(I)-0.5)/(FLOAT(IRATIOF))                 NESTBDY.33
          XX=FLOAT(JWEST) -0.5 + (FLOAT(J)-0.5)/(FLOAT(IRATIOF))                 NESTBDY.34
          TFSINT(I,J)=BINT(YY,XX,TC,IYY,JXX,.FALSE.)                             NESTBDY.35
  30   CONTINUE                                                                  NESTBDY.36
                                                                                 NESTBDY.37
       ENDIF                                                                     NESTBDY.38
C                                                                                NESTBDY.39
C=======================================================================         NESTBDY.40
C                                                                                NESTBDY.41
C     REPLACE THE 3 ROWS ON THE EDGE OF TF WITH SINTED TERRAIN (TFSINT)          NESTBDY.42
C                                                                                NESTBDY.43
      DO 50 I=1,IYYF-1                                                           NESTBDY.44
        DO 60 J=1,4                                                              NESTBDY.45
            TF(I,J)=TFSINT(I,J)                                                  NESTBDY.46
 60      CONTINUE                                                                NESTBDY.47
      DO 70 J=JXXF-4,JXXF-1                                                      NESTBDY.48
            TF(I,J)=TFSINT(I,J)                                                  NESTBDY.49
 70      CONTINUE                                                                NESTBDY.50
 50   CONTINUE                                                                   NESTBDY.51
      DO 80 J=1,JXXF-1                                                           NESTBDY.52
        DO 90 I=1,4                                                              NESTBDY.53
            TF(I,J)=TFSINT(I,J)                                                  NESTBDY.54
 90      CONTINUE                                                                NESTBDY.55
      DO 100 I=IYYF-4,IYYF-1                                                     NESTBDY.56
            TF(I,J)=TFSINT(I,J)                                                  NESTBDY.57
 100     CONTINUE                                                                NESTBDY.58
 80   CONTINUE                                                                   NESTBDY.59
      IF (PFLAG) THEN                                                            NESTBDY.60
         NAME='TF_REP3'                                                          NESTBDY.61
         CALL OUTPT(TF,IYYF,1,IYYF-1, 1,JXXF, 1, JXXF-1,1, KSIGT,NAME)           NESTBDY.62
      ENDIF                                                                      NESTBDY.63
C                                                                                NESTBDY.64
C================================================================                NESTBDY.65
      IF (IB.EQ.1) GO TO 200                                                     NESTBDY.66
C                                                                                NESTBDY.67
C     REPLACE ROWS AND COLUMNS 5 - 7 OF TF WITH A BLEND                          NESTBDY.68
C                                                                                NESTBDY.69
      DO 150 I = 5, IYYF-5                                                       NESTBDY.70
         J = JXXF-5                                                              NESTBDY.71
            TF(I,5)= .75*TFSINT(I,5) + .25*TF(I,5)                               NESTBDY.72
            TF(I,J)= .75*TFSINT(I,J) + .25*TF(I,J)                               NESTBDY.73
         J = JXXF-6                                                              NESTBDY.74
            TF(I,6)=  .5*TFSINT(I,6) +  .5*TF(I,6)                               NESTBDY.75
            TF(I,J)=  .5*TFSINT(I,J) +  .5*TF(I,J)                               NESTBDY.76
         J = JXXF-7                                                              NESTBDY.77
            TF(I,7)= .25*TFSINT(I,7) + .75*TF(I,7)                               NESTBDY.78
            TF(I,J)= .25*TFSINT(I,J) + .75*TF(I,J)                               NESTBDY.79
 150  CONTINUE                                                                   NESTBDY.80
      DO 160 J = 5, JXXF-5                                                       NESTBDY.81
         I = IYYF-5                                                              NESTBDY.82
            TF(5,J)=.75*TFSINT(5,J) + .25*TF(5,J)                                NESTBDY.83
            TF(I,J)=.75*TFSINT(I,J) + .25*TF(I,J)                                NESTBDY.84
         I = IYYF-6                                                              NESTBDY.85
            TF(6,J)= .5*TFSINT(6,J) +  .5*TF(6,J)                                NESTBDY.86
            TF(I,J)= .5*TFSINT(I,J) +  .5*TF(I,J)                                NESTBDY.87
         I = IYYF-7                                                              NESTBDY.88
            TF(7,J)=.25*TFSINT(7,J) + .75*TF(7,J)                                NESTBDY.89
            TF(I,J)=.25*TFSINT(I,J) + .75*TF(I,J)                                NESTBDY.90
 160  CONTINUE                                                                   NESTBDY.91
 200  CONTINUE                                                                   NESTBDY.92
      IF (PFLAG) THEN                                                            NESTBDY.93
         NAME='TF_REP6'                                                          NESTBDY.94
         CALL OUTPT(TF,IYYF,1,IYYF-1, 1,JXXF, 1, JXXF-1,1, KSIGT,NAME)           NESTBDY.95
      ENDIF                                                                      NESTBDY.96
C                                                                                NESTBDY.97
      RETURN                                                                     NESTBDY.98
      END                                                                        NESTBDY.99
