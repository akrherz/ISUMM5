      SUBROUTINE UNITY(SLAB1,IMAX,JMAX)                                          UNITY.1
#     include <parame.incl>                                                      UNITY.2
      DIMENSION SLAB1(MIX,MJX)                                                   UNITY.3
      DO 1 I=1,IMAX                                                              UNITY.4
        DO 1 J=1,JMAX                                                            UNITY.5
          SLAB1(I,J)=1.0                                                         UNITY.6
    1   CONTINUE                                                                 UNITY.7
                                                                                 UNITY.8
      RETURN                                                                     UNITY.9
      END                                                                        UNITY.10
                                                                                 UNITY.11
