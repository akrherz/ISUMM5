C                                                                                ERASE.1
C                                                                                ERASE.2
C ===================================================================            ERASE.3
C                                                                                ERASE.4
      SUBROUTINE ERASE                                                           ERASE.5
C                                                                                ERASE.6
#     include <parame.incl>                                                      ERASE.7
      PARAMETER (II=MKX)                                                         ERASE.8
#     include <btblk1.incl>                                                      ERASE.9
#     include <btblk2.incl>                                                      ERASE.10
      ALF=.1                                                                     ERASE.11
      VISK=1.33E-5                                                               ERASE.12
      A1=.92                                                                     ERASE.13
      A2=.74                                                                     ERASE.14
      B1=16.6                                                                    ERASE.15
      B2=10.1                                                                    ERASE.16
      C=0.08                                                                     ERASE.17
      G=9.81                                                                     ERASE.18
      BETA=3.33E-3                                                               ERASE.19
      VK=.4                                                                      ERASE.20
      SQ=.23                                                                     ERASE.21
      BT=4.7                                                                     ERASE.22
      R=.74                                                                      ERASE.23
      GAMM=15.                                                                   ERASE.24
      GAMH=9.                                                                    ERASE.25
      HEATL=2.5E6                                                                ERASE.26
      CPD=1004.                                                                  ERASE.27
      RD=287.                                                                    ERASE.28
      GRD=3.414E-2                                                               ERASE.29
      DO 100 I=1,II                                                              ERASE.30
        ABTH(I)=0.                                                               ERASE.31
        C1(I)=0.                                                                 ERASE.32
        C2(I)=0.                                                                 ERASE.33
        C3(I)=0.                                                                 ERASE.34
        CLAM2(I)=0.                                                              ERASE.35
        UN(I)=0.                                                                 ERASE.36
        VN(I)=0.                                                                 ERASE.37
        ENUL(I)=0.                                                               ERASE.38
        DQWDZ(I)=0.                                                              ERASE.39
        EKM(I)=0.                                                                ERASE.40
        EKH(I)=0.                                                                ERASE.41
        DTHLDZ(I)=0.                                                             ERASE.42
        DUDZ(I)=0.                                                               ERASE.43
        DUMV(I)=0.                                                               ERASE.44
        DVDZ(I)=0.                                                               ERASE.45
        X1U(I)=0.                                                                ERASE.46
        X2U(I)=0.                                                                ERASE.47
        DTHVDZ(I)=0.                                                             ERASE.48
        QL(I)=0.                                                                 ERASE.49
        QRN(I)=0.                                                                ERASE.50
        QSL(I)=0.                                                                ERASE.51
        QS(I)=0.                                                                 ERASE.52
        X2UL(I)=0.                                                               ERASE.53
        RC(I)=0.                                                                 ERASE.54
        TH(I)=0.                                                                 ERASE.55
        THL(I)=0.                                                                ERASE.56
        THLWB(I)=0.                                                              ERASE.57
        THVWB(I)=0.                                                              ERASE.58
        TL(I)=0.                                                                 ERASE.59
        TV(I)=0.                                                                 ERASE.60
        UG(I)=0.                                                                 ERASE.61
        VG(I)=0.                                                                 ERASE.62
        WQWB(I)=0.                                                               ERASE.63
        XL(I)=0.                                                                 ERASE.64
        VT(I)=0.                                                                 ERASE.65
        TDEW(I)=0.                                                               ERASE.66
        ZN(I)=0.                                                                 ERASE.67
        ZS(I)=0.                                                                 ERASE.68
        QRL(I)=0.                                                                ERASE.69
        QRS(I)=0.                                                                ERASE.70
        XM(I)=0.                                                                 ERASE.71
        FML(I)=0.                                                                ERASE.72
        FPL(I)=0.                                                                ERASE.73
        FMS(I)=0.                                                                ERASE.74
        FPS(I)=0.                                                                ERASE.75
  100 CONTINUE                                                                   ERASE.76
      DO 200 N=1,11                                                              ERASE.77
        DO 200 I=1,II                                                            ERASE.78
          WK(I,N)=0.                                                             ERASE.79
  200   CONTINUE                                                                 ERASE.80
                                                                                 ERASE.81
      QWSTAR=0.                                                                  ERASE.82
      IBLKST=0                                                                   ERASE.83
      PHIM=0                                                                     ERASE.84
      COSALP=0.                                                                  ERASE.85
      SINALP=0.                                                                  ERASE.86
      HDUCT=0.                                                                   ERASE.87
      THLSTAR=0.                                                                 ERASE.88
      USTAR=0.                                                                   ERASE.89
      Y1=0.                                                                      ERASE.90
      ZL=0.                                                                      ERASE.91
      XMZ=0.                                                                     ERASE.92
      XBC=0.                                                                     ERASE.93
      RETURN                                                                     ERASE.94
      END                                                                        ERASE.95
                                                                                 ERASE.96
