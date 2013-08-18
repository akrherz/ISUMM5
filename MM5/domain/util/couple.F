C                                                                                COUPLE.1
      SUBROUTINE COUPLE(F3INOUT,F2IN,MIX,MJX,MKX,IX,JX,KX)                       COUPLE.2
C                                                                                COUPLE.3
C     PURPOSE REPLACE 3D ARRAY F3INOUT(I,J,K)                                    COUPLE.4
C               WITH F3INOUT(I,J,K)*F2IN(I,J)                                    COUPLE.5
C                                                                                COUPLE.6
      DIMENSION F3INOUT(MIX,MJX,MKX),F2IN(MIX,MJX)                               COUPLE.7
C                                                                                COUPLE.8
c$omp parallel do default(shared)                                                COUPLE.9
c$omp&private(i,j,k)                                                             COUPLE.10
      DO 100 J=1,JX                                                              COUPLE.11
        DO 100 K=1,KX                                                            COUPLE.12
          DO 100 I=1,IX                                                          COUPLE.13
            F3INOUT(I,J,K)=F3INOUT(I,J,K)*F2IN(I,J)                              COUPLE.14
  100     CONTINUE                                                               COUPLE.15
                                                                                 COUPLE.16
                                                                                 COUPLE.17
      RETURN                                                                     COUPLE.18
      END                                                                        COUPLE.19
                                                                                 COUPLE.20
