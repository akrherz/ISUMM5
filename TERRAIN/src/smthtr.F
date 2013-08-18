      SUBROUTINE SMTHTR(SLAB1,IS1,IS2,IP)                                        06NOV00.447
C                                                                                SMTHTR.2
C   PURPOSE :   SMOOTHES TERRAIN HEIGHT FIELDS.                                  SMTHTR.3
C   ON ENTRY :  SLAB1(IS1,IS2): THE 2-D FIELD TO BE SMOOTHED.                    SMTHTR.4
C                      IS1,IS2: THE DIMENSIONS OF THE FIELD SLAB.                SMTHTR.5
C   ON EXIT :   SLAB1(IS1,IS2): THE SMOOTHED TERRAIN HEIGHT FIELD.               SMTHTR.6
C   CALLS :     SUBROUTINE SMTHTR CALLS THE SUBROUTINE SMTHER.                   SMTHTR.7
C   CALLED BY : SUBROUTINE TERDRV.                                               SMTHTR.8
C   COMMENTS :                                                                   SMTHTR.9
C  IN THIS SUBROUTINE, THE OCEAN POINTS (HEIGHT < 0) ARE COUNTED AND             SMTHTR.10
C  SET TO BE ZERO.  AFTER THE TWO PASSES SMOOTHER-DESMOOTHER, SET THE            SMTHTR.11
C  TERRAIN HEIGHT TO ZERO AT THE GRID POINTS WITH THE NEGATIVE VALUES            SMTHTR.12
C  AND SET THE OCEAN POINTS TO --0.001.                                          SMTHTR.13
C                                                                                SMTHTR.14
# include <parame.incl>                                                          SMTHTR.15
      DIMENSION II(IIMX*JJMX), JJ(IIMX*JJMX)                                     SMTHTR.16
      DIMENSION SLAB1(IS1,IS2)                                                   SMTHTR.17
      N=1                                                                        SMTHTR.18
      DO 10 I = 1,IS1                                                            SMTHTR.19
      DO 10 J = 1,IS2                                                            SMTHTR.20
      IF(SLAB1(I,J).LT.0.0) THEN                                                 SMTHTR.21
      II(N)=I                                                                    SMTHTR.22
      JJ(N)=J                                                                    SMTHTR.23
      SLAB1(I,J)=0.0                                                             SMTHTR.24
      N=N+1                                                                      SMTHTR.25
      END IF                                                                     SMTHTR.26
   10 CONTINUE                                                                   SMTHTR.27
      IF (IP.EQ.1) PRINT 15,N                                                    06NOV00.448
   15 FORMAT(5X,'  THERE ARE A TOTAL OF ',I5,' POINTS OF OCEAN')                 SMTHTR.29
      CALL SMTHER(SLAB1,IS1,IS2,2,'CROSS')                                       SMTHTR.30
      DO 20 I = 1, IS1                                                           SMTHTR.31
      DO 20 J = 1, IS2                                                           SMTHTR.32
      IF(SLAB1(I,J).LT.0.0) SLAB1(I,J)=0.0                                       SMTHTR.33
   20 CONTINUE                                                                   SMTHTR.34
      DO 25 K=1,N-1                                                              SMTHTR.35
      I=II(K)                                                                    SMTHTR.36
      J=JJ(K)                                                                    SMTHTR.37
      SLAB1(I,J)=-0.001                                                          SMTHTR.38
   25 CONTINUE                                                                   SMTHTR.39
      RETURN                                                                     SMTHTR.40
      END                                                                        SMTHTR.41
