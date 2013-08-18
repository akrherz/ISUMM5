C                                                                                MINIM.1
C***************************************************************                 MINIM.2
C*                                                             *                 MINIM.3
C*   PROGRAM TO CALCULATE LEVEL OF ARRAY MINIMUM               *                 MINIM.4
C*                                                             *                 MINIM.5
C***************************************************************                 MINIM.6
C                                                                                MINIM.7
C                                                                                MINIM.8
          SUBROUTINE MINIM (ARRAY,KDIM,KS,KEND,KT)                               MINIM.9
      DIMENSION ARRAY(KDIM)                                                      MINIM.10
      KT=KS                                                                      MINIM.11
      X=ARRAY(KS)                                                                MINIM.12
C                                                                                MINIM.13
      DO 100 K=KS+1,KEND                                                         MINIM.14
        IF(ARRAY(K).LT.X)THEN                                                    MINIM.15
          X=ARRAY(K)                                                             MINIM.16
          KT=K                                                                   MINIM.17
        ENDIF                                                                    MINIM.18
  100 CONTINUE                                                                   MINIM.19
C                                                                                MINIM.20
      RETURN                                                                     MINIM.21
      END                                                                        MINIM.22
                                                                                 MINIM.23
