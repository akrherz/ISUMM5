      SUBROUTINE DCPL3D(F,FB,RPSB,IEND,JEND,KL,MKZ)                              DCPL3D.1
#       include <parame.incl>                                                    DCPL3D.2
      DIMENSION F(MIX,MJX,MKZ),FB(MIX,MJX,MKZ),RPSB(MIX,MJX)                     DCPL3D.3
C                                                                                DCPL3D.4
C_FLIC_RUNPAD(2)                                                                 DCPL3D.5
cmic$ do all autoscope                                                           DCPL3D.6
c$doacross                                                                       DCPL3D.7
c$& local(i,j,k)                                                                 DCPL3D.8
c$omp parallel do default(shared)                                                DCPL3D.9
c$omp&private(i,j,k)                                                             DCPL3D.10
      DO J=1,JEND                                                                DCPL3D.11
        DO K=1,KL                                                                DCPL3D.12
          DO I=1,IEND                                                            DCPL3D.13
            F(I,J,K)=FB(I,J,K)*RPSB(I,J)                                         DCPL3D.14
          ENDDO                                                                  DCPL3D.15
        ENDDO                                                                    DCPL3D.16
      ENDDO                                                                      DCPL3D.17
C                                                                                DCPL3D.18
      RETURN                                                                     DCPL3D.19
      END                                                                        DCPL3D.20
                                                                                 DCPL3D.21
