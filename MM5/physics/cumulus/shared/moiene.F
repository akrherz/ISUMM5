C                                                                                MOIENE.1
C                                                                                MOIENE.2
C**********************************************************************          MOIENE.3
C*                                                                    *          MOIENE.4
C*        MOIST STATIC ENERGY                                         *          MOIENE.5
C*                                                                    *          MOIENE.6
C**********************************************************************          MOIENE.7
C                                                                                MOIENE.8
C                                                                                MOIENE.9
         SUBROUTINE MOIENE (T,Q,Z,H,KDIM,KS)                                     MOIENE.10
      DIMENSION T(KDIM),Q(KDIM),Z(KDIM),H(KDIM)                                  MOIENE.11
      DO 100 K=KS,KDIM                                                           MOIENE.12
        H(K)=9.81*Z(K)+1004.*T(K)+2.5E06*Q(K)                                    MOIENE.13
  100 CONTINUE                                                                   MOIENE.14
      RETURN                                                                     MOIENE.15
      END                                                                        MOIENE.16
                                                                                 MOIENE.17
