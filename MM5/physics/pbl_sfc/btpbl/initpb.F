      SUBROUTINE INITPB (PSBD,PINP,PHI,GWWG,PS,                                  INITPB.1
     1     SST,P,WK,ABT,ZTIME,TRSTORE,                                           INITPB.2
     1     SPECHUM,U,V,TV,PRATIO,THL,ABTH,TH,QW,QL,RC,THLSB,                     INITPB.3
     2     QRL,QRS,QRN,WQWB,THLQWB,QWSB,TKE,TKESR,RHOAU,RHOVU,                   INITPB.4
     3     X1U,X2U,X2UL,ENUL,RLAT,RLON,GMT,Z1,I,J,                               INITPB.5
     4     PTOP,LL,QWZ,IT,THLZ,SIGG,U3D,V3D,T3D,QV3D)                            INITPB.6
C                                                                                INITPB.7
C----------------------------------------------------------------------          INITPB.8
#     include <parame.incl>                                                      INITPB.9
#     include <point2d.incl>                                                     INITPB.10
#     include <navypb.incl>                                                      INITPB.11
C                                                                                INITPB.12
      PARAMETER (II=MKX)                                                         INITPB.13
      DIMENSION ABT(II),ABTH(II),ENUL(II),P(II),X1U(II),X2U(II),                 INITPB.14
     5 PRATIO(II),QL(II),QRN(II),QSL(II),QS(II),X2UL(II),                        INITPB.15
     6 QW(II),QWSB(II),RC(II),SPECHUM(II),WK(II,11),                             INITPB.16
     8 TH(II),THL(II),THLQWB(II),THLSB(II),                                      INITPB.17
     9 THLWB(II),THVWB(II),TKE(II),                                              INITPB.18
     A TKESR(II),TL(II),TV(II),U(II),UG(II),V(II),VG(II),                        INITPB.19
     B WQWB(II),XL(II),THTEND(II),QTEND(II),VT(II),TDEW(II),                     INITPB.20
     C ZN(II),ZS(II),QRL(II),QRS(II),PSBD(MIX,MJX),                              INITPB.21
     E FML(II),FPL(II),PINP(MIX,MJX,MKX),                                        INITPB.22
     F FMS(II),FPS(II)                                                           INITPB.23
#     include <btblk2.incl>                                                      INITPB.24
C                                                                                INITPB.25
      DIMENSION RHOAU(II),RHOVU(II),PHI(MIX,MJX,MKX),SIGG(MKX)                   INITPB.26
C                                                                                INITPB.27
      DIMENSION U3D(MIX,MJX,MKX),V3D(MIX,MJX,MKX),T3D(MIX,MJX,MKX),              INITPB.28
     -         QV3D(MIX,MJX,MKX)                                                 INITPB.29
C                                                                                INITPB.30
C  DEFINE INITIAL GROUND WETNESS, GW;                                            INITPB.31
C  AND THE UNIVERSAL HOUR, IGMT.                                                 INITPB.32
C  AND THE ROUGHNESS LENGTH (M), Z1.                                             INITPB.33
C                                                                                INITPB.34
      GWWG=MAVAIL(I,J)                                                           INITPB.35
      RLAT=XLAT(I,J)                                                             INITPB.36
C                                                                                INITPB.37
      IF(XLONG(I,J).GE.0)THEN                                                    INITPB.38
        RLON=XLONG(I,J)                                                          INITPB.39
      ELSE                                                                       INITPB.40
        RLON=360.+XLONG(I,J)                                                     INITPB.41
      ENDIF                                                                      INITPB.42
      ZTIME=GMT                                                                  INITPB.43
C                                                                                INITPB.44
C  RLAT= LATITUDE IN DEG.                                                        INITPB.45
C  RLON= LONGITUDE IN DEG FROM 0 TO 360.                                         INITPB.46
C                                                                                INITPB.47
C     PS = 10.*PSB(I,J)+10.*PTOP                                                 INITPB.48
      SST=TGB(I,J)                                                               INITPB.49
      TERRA=HT(I,J)/9.81                                                         INITPB.50
      CALL ESATPB(E,SST)                                                         INITPB.51
      QWZ=.622*E/(PS-.378*E)                                                     INITPB.52
      Z1=ZNT(I,J)                                                                INITPB.53
      TRSTORE=TMN(I,J)                                                           INITPB.54
C                                                                                INITPB.55
C                                                                                INITPB.56
      DO 20 K=1,II                                                               INITPB.57
        KK=II-K+1                                                                INITPB.58
        P(KK)=PINP(I,J,KK)*1000.                                                 INITPB.59
        IF(Z(KK).LT.0.)Z(KK)=1.E-08                                              INITPB.60
        ABT(KK)=T3D(I,J,K)                                                       INITPB.61
        RATMI=QV3D(I,J,K)                                                        INITPB.62
        SPECHUM(KK)=AMAX1(1.E-8,RATMI/(1.+RATMI))                                INITPB.63
C     QL(KK)=QCB(I,J,K)/PSB(I,J)+QRB(I,J,K)/PSB(I,J)                             INITPB.64
        QL(KK)=0.                                                                INITPB.65
        QW(KK)=SPECHUM(KK)+QL(KK)                                                INITPB.66
        TV(KK)=ABT(KK)*(1.0+0.609*SPECHUM(KK))                                   INITPB.67
        V(KK)=.25*(V3D(I,J,K)+V3D(I+1,J,K)+V3D(I,J+1,K)+V3D(I+1,J+1,K))          INITPB.68
        U(KK)=.25*(U3D(I,J,K)+U3D(I+1,J,K)+U3D(I,J+1,K)+U3D(I+1,J+1,K))          INITPB.69
   20 CONTINUE                                                                   INITPB.70
C  LOAD WORK ARRAYS FOR CALLS TO VINTRP                                          INITPB.71
C                                                                                INITPB.72
C                                                                                INITPB.73
      DO 130 K=1,II                                                              INITPB.74
        PRATIO(K)=(P(K)/1000.)**0.286                                            INITPB.75
  130 CONTINUE                                                                   INITPB.76
      THLZ=SST*(1000./PS)**0.286                                                 INITPB.77
      LL=1                                                                       INITPB.78
      DO 140 K=1,II                                                              INITPB.79
        KK=II-K+1                                                                INITPB.80
        IF(IT.EQ.0)THEN                                                          INITPB.81
C       PTHLQWB(I,J,KK)=0.                                                       INITPB.82
C       PQWSB(I,J,KK)=0.                                                         INITPB.83
C       PTHLSB(I,J,KK)=0.                                                        INITPB.84
          TURKE(I,J,KK)=1.E-06                                                   INITPB.85
        ENDIF                                                                    INITPB.86
        ENUL(K)=0.                                                               INITPB.87
        THL(K)=ABT(K)/PRATIO(K)                                                  INITPB.88
        ABTH(K)=THL(K)                                                           INITPB.89
        TH(K)=ABTH(K)*(1.0+0.609*SPECHUM(K))                                     INITPB.90
        RC(K)=0.0                                                                INITPB.91
        IF(QL(K).GT.1.E-08)RC(K)=1.                                              INITPB.92
        THLSB(K)=PTHLSB(I,J,KK)                                                  INITPB.93
        QRL(K)=0.0                                                               INITPB.94
        QRS(K)=0.0                                                               INITPB.95
        QRN(K)=0.0                                                               INITPB.96
        WQWB(K)=0.0                                                              INITPB.97
        THLQWB(K)=PTHLQWB(I,J,KK)                                                INITPB.98
        QWSB(K)=PQWSB(I,J,KK)                                                    INITPB.99
        IF(K.EQ.II)GOTO 140                                                      INITPB.100
        IF(TURKE(I,J,KK).LT.1.E-04)TURKE(I,J,KK)=1.E-04                          INITPB.101
        TKE(K)=AMAX1(1.E-4,TURKE(I,J,KK))                                        INITPB.102
        TKESR(K)=SQRT(TKE(K))                                                    INITPB.103
  140 CONTINUE                                                                   INITPB.104
C   THIS SECTION INITIALIZES UPPER ATMOS. VARIABLES FOR RADIATION                INITPB.105
C  THE WORK ARRAY WK IS USED IN PLACE OF OLD UPPER ATMOS. VARBLS:                INITPB.106
C      WK(I,7)=PW(I)     WK(I,8)=ABTU(I)                                         INITPB.107
C      WK(I,9)=SPHMU(I)  WK(I,10)=ZU(I)                                          INITPB.108
C      WK(I,11)=DELZU(I)                                                         INITPB.109
      SIGMA=5.67E-8                                                              INITPB.110
      TURBF=1.0                                                                  INITPB.111
      WK(1,7)=P(II)                                                              INITPB.112
      WK(1,8)=ABT(II)                                                            INITPB.113
      WK(1,9)=SPECHUM(II)                                                        INITPB.114
      WK(1,10)=Z(II)                                                             INITPB.115
      RHOVU(1)=0.                                                                INITPB.116
      RHOAU(1)=0.                                                                INITPB.117
      X1U(1)=RHOVU(1)*WK(1,11)                                                   INITPB.118
      X2U(1)=RHOAU(1)*WK(1,11)*(1.0+TURBF)                                       INITPB.119
      X2UL(1)=0.0003*RHOAU(1)*WK(1,11)                                           INITPB.120
      ENUL(1)=SIGMA*ABT(II)**4                                                   INITPB.121
 7000 CONTINUE                                                                   INITPB.122
      RETURN                                                                     INITPB.123
      END                                                                        INITPB.124
                                                                                 INITPB.125
