      SUBROUTINE BDYVAL(U3D,V3D,QCA,QRA,PSA,RPSA)                                BDYVAL.1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          BDYVAL.2
#     include <parame.incl>                                                      BDYVAL.3
#     include <various.incl>                                                     BDYVAL.4
C                                                                                BDYVAL.5
      DIMENSION U3D(MIX,MJX,MKX),V3D(MIX,MJX,MKX), PSA(MIX,MJX)                  BDYVAL.6
      DIMENSION QCA(MIX,MJX,MKX),QRA(MIX,MJX,MKX),RPSA(MIX,MJX)                  BDYVAL.7
C                                                                                BDYVAL.8
cmic$ do all autoscope                                                           BDYVAL.9
c$doacross                                                                       BDYVAL.10
c$& local(i,j,k,qcx,qrx,uavg,vavg)                                               BDYVAL.11
c$omp parallel do default(shared)                                                BDYVAL.12
c$omp&private(i,j,k,qcx,qrx,uavg,vavg)                                           BDYVAL.13
      DO 730 K=1,KL                                                              BDYVAL.14
        DO 710 I=1,ILX                                                           BDYVAL.15
C                                                                                BDYVAL.16
C.....WEST BOUNDARY:                                                             BDYVAL.17
C                                                                                BDYVAL.18
          UAVG=U3D(I,1,K)+U3D(I+1,1,K)+U3D(I,2,K)+U3D(I+1,2,K)                   BDYVAL.19
          IF(UAVG.GE.0.0)THEN                                                    BDYVAL.20
            QCX=0.                                                               BDYVAL.21
            QRX=0.                                                               BDYVAL.22
          ELSE                                                                   BDYVAL.23
            QCX=QCA(I,2,K)*RPSA(I,2)                                             BDYVAL.24
            QRX=QRA(I,2,K)*RPSA(I,2)                                             BDYVAL.25
          ENDIF                                                                  BDYVAL.26
C                                                                                BDYVAL.27
          QCA(I,1,K)=QCX*PSA(I,1)                                                BDYVAL.28
          QRA(I,1,K)=QRX*PSA(I,1)                                                BDYVAL.29
C                                                                                BDYVAL.30
C.....EAST BOUNDARY:                                                             BDYVAL.31
C                                                                                BDYVAL.32
          UAVG=U3D(I,JLX,K)+U3D(I+1,JLX,K)+U3D(I,JL,K)+U3D(I+1,JL,K)             BDYVAL.33
          IF(UAVG.LT.0.0)THEN                                                    BDYVAL.34
            QCX=0.                                                               BDYVAL.35
            QRX=0.                                                               BDYVAL.36
          ELSE                                                                   BDYVAL.37
            QCX=QCA(I,JLXM,K)*RPSA(I,JLXM)                                       BDYVAL.38
            QRX=QRA(I,JLXM,K)*RPSA(I,JLXM)                                       BDYVAL.39
          ENDIF                                                                  BDYVAL.40
C                                                                                BDYVAL.41
          QCA(I,JLX,K)=QCX*PSA(I,JLX)                                            BDYVAL.42
          QRA(I,JLX,K)=QRX*PSA(I,JLX)                                            BDYVAL.43
  710   CONTINUE                                                                 BDYVAL.44
C                                                                                BDYVAL.45
C.....SOUTH BOUNDARY:                                                            BDYVAL.46
C                                                                                BDYVAL.47
        DO 720 J=2,JLX-1                                                         BDYVAL.48
          VAVG=V3D(1,J,K)+V3D(1,J+1,K)+V3D(2,J,K)+V3D(2,J+1,K)                   BDYVAL.49
          IF(VAVG.GE.0.0)THEN                                                    BDYVAL.50
            QCX=0.                                                               BDYVAL.51
            QRX=0.                                                               BDYVAL.52
          ELSE                                                                   BDYVAL.53
            QCX=QCA(2,J,K)*RPSA(2,J)                                             BDYVAL.54
            QRX=QRA(2,J,K)*RPSA(2,J)                                             BDYVAL.55
          ENDIF                                                                  BDYVAL.56
          QCA(1,J,K)=QCX*PSA(1,J)                                                BDYVAL.57
          QRA(1,J,K)=QRX*PSA(1,J)                                                BDYVAL.58
C                                                                                BDYVAL.59
C.....NORTH BOUNDARY:                                                            BDYVAL.60
C                                                                                BDYVAL.61
          VAVG=V3D(ILX,J,K)+V3D(ILX,J+1,K)+V3D(IL,J,K)+V3D(IL,J+1,K)             BDYVAL.62
          IF(VAVG.LT.0.0)THEN                                                    BDYVAL.63
            QCX=0.                                                               BDYVAL.64
            QRX=0.                                                               BDYVAL.65
          ELSE                                                                   BDYVAL.66
            QCX=QCA(ILXM,J,K)*RPSA(ILXM,J)                                       BDYVAL.67
            QRX=QRA(ILXM,J,K)*RPSA(ILXM,J)                                       BDYVAL.68
          ENDIF                                                                  BDYVAL.69
          QCA(ILX,J,K)=QCX*PSA(ILX,J)                                            BDYVAL.70
          QRA(ILX,J,K)=QRX*PSA(ILX,J)                                            BDYVAL.71
  720   CONTINUE                                                                 BDYVAL.72
  730 CONTINUE                                                                   BDYVAL.73
C                                                                                BDYVAL.74
      RETURN                                                                     BDYVAL.75
      END                                                                        BDYVAL.76
                                                                                 BDYVAL.77
