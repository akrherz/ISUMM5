         SUBROUTINE MAXIM(ARRAY,KDIM,KS,KE,MAX)                                  MAXIM.1
      DIMENSION ARRAY(KDIM)                                                      MAXIM.2
      MAX=KS                                                                     MAXIM.3
      X=ARRAY(KS)                                                                MAXIM.4
C                                                                                MAXIM.5
      DO 100 K=KS,KE                                                             MAXIM.6
        XAR=ARRAY(K)                                                             MAXIM.7
        IF(XAR.GE.X)THEN                                                         MAXIM.8
          X=XAR                                                                  MAXIM.9
          MAX=K                                                                  MAXIM.10
        ENDIF                                                                    MAXIM.11
  100 CONTINUE                                                                   MAXIM.12
C                                                                                MAXIM.13
      RETURN                                                                     MAXIM.14
      END                                                                        MAXIM.15
                                                                                 MAXIM.16
