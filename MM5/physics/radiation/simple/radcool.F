      SUBROUTINE RADCOOL(TTEN,T,PSA,ISTART,IEND,JSTART,JEND,KL)                  RADCOOL.1
C     SIMPLE RADIATIVE COOLING TERM                                              RADCOOL.2
#     include <parame.incl>                                                      RADCOOL.3
      DIMENSION TTEN(MIX,MJX,MKX),T(MIX,MJX,MKX),PSA(MIX,MJX)                    RADCOOL.4
C                                                                                RADCOOL.5
      A=0.017/8.64E4                                                             RADCOOL.6
      B=1.8/8.64E4-A*273.16                                                      RADCOOL.7
C                                                                                RADCOOL.8
cmic$ do all autoscope                                                           RADCOOL.9
cmic$1 shared (ISTART,IEND,JSTART,JEND,KL,A,B,                                   RADCOOL.10
cmic$1         TTEN,T,PSA)                                                       RADCOOL.11
cmic$2 private (I,J,K)                                                           RADCOOL.12
c$doacross                                                                       RADCOOL.13
c$& local(i,j,k)                                                                 RADCOOL.14
c$omp parallel do default(shared)                                                RADCOOL.15
c$omp&private(i,j,k)                                                             RADCOOL.16
      DO J=JSTART,JEND                                                           RADCOOL.17
        DO K=1,KL                                                                RADCOOL.18
          DO I=ISTART,IEND                                                       RADCOOL.19
            TTEN(I,J,K)=TTEN(I,J,K)-PSA(I,J)*(A*T(I,J,K)+B)                      RADCOOL.20
          ENDDO                                                                  RADCOOL.21
        ENDDO                                                                    RADCOOL.22
      ENDDO                                                                      RADCOOL.23
C                                                                                RADCOOL.24
      RETURN                                                                     RADCOOL.25
      END                                                                        RADCOOL.26
                                                                                 RADCOOL.27
