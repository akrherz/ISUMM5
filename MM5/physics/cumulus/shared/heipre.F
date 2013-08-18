C                                                                                HEIPRE.1
C                                                                                HEIPRE.2
          SUBROUTINE HEIPRE (P,Z,T,Z1,KDIM,KS,PSURF)                             HEIPRE.3
      DIMENSION P(KDIM),Z(KDIM),T(KDIM)                                          HEIPRE.4
      ZSTART=Z1-(ALOG(P(1))-ALOG(PSURF))*287.04*T(1)/9.81                        HEIPRE.5
      Z(KS)=ZSTART                                                               HEIPRE.6
      DO 100 K=KS+1,KDIM                                                         HEIPRE.7
        TVBAR=.5*T(K)+.5*T(K-1)                                                  HEIPRE.8
        Z(K)=Z(K-1)-(ALOG(P(K))-ALOG(P(K-1)))*287.04*TVBAR/9.81                  HEIPRE.9
  100 CONTINUE                                                                   HEIPRE.10
      RETURN                                                                     HEIPRE.11
      END                                                                        HEIPRE.12
                                                                                 HEIPRE.13
