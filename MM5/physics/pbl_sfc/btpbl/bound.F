      SUBROUTINE BOUND (IJ,JJ,XLAND,ISFFLX,HFX,QFX,UST)                          BOUND.1
C                                                                                BOUND.2
#     include <parame.incl>                                                      BOUND.3
C                                                                                BOUND.4
      PARAMETER (II=MKX)                                                         BOUND.5
C                                                                                BOUND.6
#     include <btblk1.incl>                                                      BOUND.7
#     include <btblk2.incl>                                                      BOUND.8
      DIMENSION HFX(MIX,MJX),QFX(MIX,MJX),UST(MIX,MJX)                           BOUND.9
C                                                                                BOUND.10
C   CONSTANTS FROM J.F.LOUIS/ECMWF APPROACH                                      BOUND.11
      BB=5.0                                                                     BOUND.12
      CC=5.0                                                                     BOUND.13
      DD=5.0                                                                     BOUND.14
      UTOT2=1.E-8+SQRT(U(1)*U(1)+V(1)*V(1))                                      BOUND.15
      COSALP=U(1)/UTOT2                                                          BOUND.16
      SINALP=V(1)/UTOT2                                                          BOUND.17
      THZ=THLZ*(1.0+0.609*SPECHUM(1))                                            BOUND.18
      Y=TH(1)-THZ                                                                BOUND.19
      SCR=ALOG(Z(1)/Z1)                                                          BOUND.20
      ASQ=(VK*VK)/(SCR*SCR)                                                      BOUND.21
      IF(UTOT2.LT.1.E-2)UTOT2=1.E-2                                              BOUND.22
      RIB=(G*Z(1)*Y)/(0.5*(TH(1)+THZ)*UTOT2*UTOT2)                               BOUND.23
      IF(RIB.EQ.0.0)GOTO 100                                                     BOUND.24
      IF(RIB.LT.0.0)GOTO 10                                                      BOUND.25
C   COMPUTE J.F.LOUIS/ECMWF DRAG COEFS. ON STABLE SIDE                           BOUND.26
      TL1=SQRT(1.0+DD*RIB)                                                       BOUND.27
      TL2=1.0+(2.*BB*RIB)/TL1                                                    BOUND.28
      CM=ASQ/TL2                                                                 BOUND.29
      TL3=1.0+(3.*BB*RIB)*TL1                                                    BOUND.30
      CH=ASQ/TL3                                                                 BOUND.31
      GOTO 20                                                                    BOUND.32
   10 CONTINUE                                                                   BOUND.33
C   COMPUTE J.F.LOUIS/ECMWF DRAG COEFS. ON UNSTABLE SIDE                         BOUND.34
      TL1=-RIB                                                                   BOUND.35
      TL2=SQRT(Z(1)*TL1/Z1)                                                      BOUND.36
      TL3=1.0+3.*ASQ*BB*CC*TL2                                                   BOUND.37
      TL4=1.0-(2.*BB*RIB)/TL3                                                    BOUND.38
      CM=ASQ*TL4                                                                 BOUND.39
      TL5=1.0-(3.*BB*RIB)/TL3                                                    BOUND.40
      CH=ASQ*TL5                                                                 BOUND.41
   20 CONTINUE                                                                   BOUND.42
C   COMPUTE THE SURFACE MOMENTUM,HEAT,AND MOISTURE FLUXES USING                  BOUND.43
C   THE LOUIS/ECMWF DRAG COEFS.                                                  BOUND.44
      USTSQ=CM*UTOT2*UTOT2                                                       BOUND.45
      IF(XLAND.LT.1.5)USTSQ=AMAX1(1.E-2,CM*UTOT2*UTOT2)                          BOUND.46
      THVWB(1)=CH*UTOT2*Y                                                        BOUND.47
      WQWB(1)=CH*UTOT2*GWWG*(QW(1)-QWZ)*ISFFLX                                   BOUND.48
      USTAR=SQRT(USTSQ)                                                          BOUND.49
      THLWB(1)=(THVWB(1)-.609*THL(1)*WQWB(1))/(1.+.609*QW(1))*ISFFLX             BOUND.50
      THLSTAR=THLWB(1)/USTAR                                                     BOUND.51
      THVSTAR=THVWB(1)/USTAR                                                     BOUND.52
      QWSTAR=WQWB(1)/USTAR                                                       BOUND.53
      TL1=0.5*(TH(1)+THZ)*USTSQ                                                  BOUND.54
      TL2=VK*G*THVSTAR                                                           BOUND.55
      IF(THVSTAR.EQ.0.0)GOTO 100                                                 BOUND.56
      ZL=TL1/TL2                                                                 BOUND.57
      IF(ABS(ZL).LT.1.0)ZL=SIGN(1.0,ZL)                                          BOUND.58
C   COMPUTE SURFACE LAYER VALUES OF TKE,THLSB,THLQWB,AND QWSB                    BOUND.59
C   BY PRODUCTION/DISSIPATION BALANCE EXPRESSIONS WHICH INCLUDE                  BOUND.60
C   EFFECTS OF STABILITY.                                                        BOUND.61
      IF(ZL.LT.0.0)GOTO 40                                                       BOUND.62
      PHIM=1.+BT*Z(1)/ZL                                                         BOUND.63
      PHIT=R+BT*Z(1)/ZL                                                          BOUND.64
      GOTO 50                                                                    BOUND.65
   40 CONTINUE                                                                   BOUND.66
      PHIM=1./(1.-GAMM*Z(1)/ZL)**0.25                                            BOUND.67
      PHIT=R/SQRT(1.-GAMH*Z(1)/ZL)                                               BOUND.68
   50 CONTINUE                                                                   BOUND.69
      GOTO 110                                                                   BOUND.70
  100 CONTINUE                                                                   BOUND.71
      ZL=1.E+20                                                                  BOUND.72
      THVWB(1)=0.0                                                               BOUND.73
      PHIT=R                                                                     BOUND.74
      CH=ASQ                                                                     BOUND.75
      CM=ASQ                                                                     BOUND.76
      WQWB(1)=CH*UTOT2*GWWG*(QW(1)-QWZ)*ISFFLX                                   BOUND.77
      USTSQ=CM*UTOT2*UTOT2                                                       BOUND.78
      IF(XLAND.LT.1.5)USTSQ=AMAX1(1.E-2,CM*UTOT2*UTOT2)                          BOUND.79
      USTAR=SQRT(USTSQ)                                                          BOUND.80
      PHIM=1.0                                                                   BOUND.81
      THLWB(1)=(THVWB(1)-.609*THL(1)*WQWB(1))/(1.+.609*QW(1))*ISFFLX             BOUND.82
      THLSTAR=THLWB(1)/USTAR                                                     BOUND.83
      QWSTAR=WQWB(1)/USTAR                                                       BOUND.84
  110 CONTINUE                                                                   BOUND.85
C     DIAGNOSTICS FOR OUTPUT ONLY                                                BOUND.86
      RHOAT=(PS*100.)/(RD*ABT(1))                                                BOUND.87
      HFX(IJ,JJ)=-RHOAT*CPD*THLWB(1)                                             BOUND.88
      QFX(IJ,JJ)=-1.*WQWB(1)                                                     BOUND.89
      UST(IJ,JJ)=USTAR                                                           BOUND.90
C                                                                                BOUND.91
      TM1=USTAR*USTAR*USTAR*(B1*XL(1)*PHIM/(VK*Z(1)))                            BOUND.92
      TL1=0.5*(TH(1)+THZ)                                                        BOUND.93
C                                                                                BOUND.94
      TM2=(G*B1*XL(1)*THVWB(1))/TL1                                              BOUND.95
C                                                                                BOUND.96
      TL2=TM1-TM2                                                                BOUND.97
C                                                                                BOUND.98
      IF(TL2.LE.0.0)GOTO 400                                                     BOUND.99
      TKE(1)=TL2**0.66667                                                        BOUND.100
C     IF(TKE(1).LT.1.E-4)GO TO 400                                               BOUND.101
C     PRINT *,USTAR,BT,ZL,R                                                      BOUND.102
C     PRINT *,THLSTAR                                                            BOUND.103
C     PRINT *,PHIT                                                               BOUND.104
C     PRINT *,QWSTAR                                                             BOUND.105
C     PRINT *,VK                                                                 BOUND.106
C     PRINT *,Z(1)                                                               BOUND.107
C     PRINT *,TKESR(1)                                                           BOUND.108
C     PRINT *,'********************************',IJ,JJ                           BOUND.109
C     PRINT *,CLAM2(1)                                                           BOUND.110
C     PRINT *,TKE(1)                                                             BOUND.111
C     PRINT *,WQWB(1)                                                            BOUND.112
C     PRINT *,THLWB(1)                                                           BOUND.113
C                                                                                BOUND.114
      TKESR(1)=SQRT(TKE(1))                                                      BOUND.115
      THLSB(1)=(CLAM2(1)*USTAR*THLSTAR*THLSTAR*PHIT)/(VK*Z(1)*TKESR(1))          BOUND.116
C                                                                                BOUND.117
      THLQWB(1)=((CLAM2(1)*PHIT)/(2.*TKESR(1)*VK*Z(1)))*(QWSTAR*THLWB(1)         BOUND.118
     +          +THLSTAR*WQWB(1))                                                BOUND.119
      QWSB(1)=(CLAM2(1)*WQWB(1)*QWSTAR*PHIT)/(VK*Z(1)*TKESR(1))                  BOUND.120
C                                                                                BOUND.121
      GOTO 410                                                                   BOUND.122
  400 CONTINUE                                                                   BOUND.123
      TKE(1)=0.                                                                  BOUND.124
      TKE(1)=1.                                                                  BOUND.125
      TKE(1)=1.E-4                                                               BOUND.126
      TKESR(1)=1.E-4                                                             BOUND.127
C     TKESR(1)=SQRT(TKE(1))                                                      BOUND.128
      THLSB(1)=0.0                                                               BOUND.129
      QWSB(1)=0.0                                                                BOUND.130
      QWSB(1)=0.0                                                                BOUND.131
      THLQWB(1)=0.0                                                              BOUND.132
  410 CONTINUE                                                                   BOUND.133
C   SET UPPER B.C.S.                                                             BOUND.134
      BLAP=THL(IM1)-THL(IM2)                                                     BOUND.135
C   SET SFC LAYER EDDY COEFS.                                                    BOUND.136
      EKM(1)=VK*USTAR*Z(1)/PHIM                                                  BOUND.137
      EKH(1)=VK*USTAR*Z(1)/PHIT                                                  BOUND.138
      TL1=10./ZL                                                                 BOUND.139
      IF(TL1.GE.0.0)GOTO 300                                                     BOUND.140
      IF(TL1.LT.-3.0)GOTO 301                                                    BOUND.141
      ALF=0.1-0.2*TL1/3.0                                                        BOUND.142
      GOTO 302                                                                   BOUND.143
  300 CONTINUE                                                                   BOUND.144
      ALF=0.1                                                                    BOUND.145
      GOTO 302                                                                   BOUND.146
  301 CONTINUE                                                                   BOUND.147
      ALF=0.3                                                                    BOUND.148
  302 CONTINUE                                                                   BOUND.149
C  GET LOWER BOUNDARY VALUE, XBC, TO BE WORKED INTO                              BOUND.150
C  TRIADIAGONAL EQS. FOR THL AND QW.                                             BOUND.151
      IF(ZL.GT.0.0)GOTO 1110                                                     BOUND.152
      TL1=ALOG(Z(1)/Z1)                                                          BOUND.153
      TL2=ALOG(Z(2)/Z1)                                                          BOUND.154
      XETA1=SQRT(1.-GAMH*Z(1)/ZL)                                                BOUND.155
      XETA2=SQRT(1.-GAMH*Z(2)/ZL)                                                BOUND.156
      PSI1=2.*ALOG((1.+XETA1)*0.5)                                               BOUND.157
      PSI2=2.*ALOG((1.+XETA2)*0.5)                                               BOUND.158
C     LIMIT TO PREVENT NEGATIVE OR ZERO DENOMINATOR                              05DEC01.862
      PSI1=AMIN1(PSI1,0.9*TL1)                                                   05DEC01.863
      PSI2=AMIN1(PSI2,0.9*TL2)                                                   05DEC01.864
      XBC=(TL1-PSI1)/(TL2-PSI2)                                                  BOUND.159
      GOTO 1001                                                                  BOUND.160
 1110 CONTINUE                                                                   BOUND.161
      TL1=ALOG(Z(1)/Z1)                                                          BOUND.162
      TL2=ALOG(Z(2)/Z1)                                                          BOUND.163
      PSI1=-(4.7/R)*Z(1)/ZL                                                      BOUND.164
      PSI2=-(4.7/R)*Z(2)/ZL                                                      BOUND.165
      XBC=(TL1-PSI1)/(TL2-PSI2)                                                  BOUND.166
 1001 CONTINUE                                                                   BOUND.167
C                                                                                BOUND.168
      RETURN                                                                     BOUND.169
      END                                                                        BOUND.170
                                                                                 BOUND.171
