CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC            RESETR.1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC              RESETR.2
      SUBROUTINE RESETR(A,LENGTH,VAL)                                            RESETR.3
      DIMENSION A(1)                                                             RESETR.4
      DO 10 N=1,LENGTH                                                           RESETR.5
        A(N)=VAL                                                                 RESETR.6
   10 CONTINUE                                                                   RESETR.7
      RETURN                                                                     RESETR.8
      END                                                                        RESETR.9
                                                                                 RESETR.10
