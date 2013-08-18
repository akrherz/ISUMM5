      SUBROUTINE SFCMM4(PBL,UST,MOL,HOL,REGIME,XLAND,ZNT,                        SFCMM4.1
     &                  KARMAN,J,ILX,KL)                                         19DEC02.1810
      IMPLICIT NONE                                                              SFCMM4.3
C***********************************************************************         SFCMM4.4
C                                                                                SFCMM4.5
C     THIS SUBROUTINE COMPUTES HEAT AND MOMENTUM FLUXES AS WELL AS OTHER         SFCMM4.6
C     SFC RELATED PARAMETERS (MOL, RA, RB, WSTAR) USING A MODIFIED RICHARDSON    SFCMM4.7
C     NUMBER PARAMETERIZATIONS SIMILAR TO HIRPBL.                                SFCMM4.8
C                                                                                SFCMM4.9
C     NOTE THAT THE PSI FUNCTIONS AND RICHARDSON NUMBER PARAMETERIZATIONS        SFCMM4.10
C     FOR STABLE CONDITIONS HAVE BEEN UPDATED TO CONFORM MORE CLOSELY WITH       SFCMM4.11
C     HOGSTROM (1988)             - JP 3/01                                      SFCMM4.12
C                                                                                SFCMM4.13
C     THE PARAMETERIZATIONS OF THE PSI FUNCTIONS FOR UNSTABLE CONDITIONS         SFCMM4.14
C     HAVE BEEN REPLACED WITH EMPIRICAL EXPRESSIONS WHICH RELATE RB DIRECTLY     SFCMM4.15
C     PSIH AND PSIM.  THESE EXPRESSIONS ARE FIT TO THE DYER (1974) FUNCTIONS     SFCMM4.16
C     WITH HOGSTROM (1988) REVISED COEFFICIENTS.  ALSO, THESE EXPERESSIONS       SFCMM4.17
C     ASSUME A LAMINAR SUBLAYER RESISTANCE FOR HEAT (Rb = 5/U*)   - JP 8/01      SFCMM4.18
C                                                                                SFCMM4.19
C     NOTE THAT THE MINIMUM U* AND CONVECTIVE VELOCITY HAVE BEEN REMOVED         SFCMM4.20
C                                  - JP 8/01                                     SFCMM4.21
C***********************************************************************         SFCMM4.22
C                                                                                SFCMM4.23
      INTEGER ITMAX                                                              SFCMM4.24
      PARAMETER (ITMAX=0)      ! CONTROLS ITERATION                              SFCMM4.25
#     include <parame.incl>                                                      SFCMM4.26
#     include <comicl.incl>                                                      SFCMM4.27
#     include <soilp.incl>                                                       SFCMM4.28
C                                                                                SFCMM4.29
C...... arguments                                                                SFCMM4.30
      REAL KARMAN                                                                SFCMM4.31
      REAL PBL(MIX,MJX), UST(MIX,MJX)                                            SFCMM4.32
      REAL MOL(MIX,MJX), HOL(MIX,MJX)                                            SFCMM4.33
      REAL REGIME(MIX,MJX),XLAND(MIX,MJX)                                        SFCMM4.34
      REAL ZNT(MIX,MJX)                                                          SFCMM4.35
      INTEGER J,ILX,KL                                                           SFCMM4.36
C......local arrays                                                              SFCMM4.37
      REAL GZ1OZ0(MIX),GOVRTH(MIX),RICH(MIX),WSPD(MIX)                           SFCMM4.38
      REAL PSIM(MIX),PSIH(MIX)                                                   SFCMM4.39
      REAL RICUT(MIX)                                                            SFCMM4.40
      REAL WS(MIX)                                                               19DEC02.1811
C.......Local variables                                                          SFCMM4.41
      REAL CZO, OZO                                                              SFCMM4.42
      REAL G,DTHVDZ,DTHVM,VCONV                                                  SFCMM4.43
      REAL DZH,SSKLM,RI,ZOL                                                      SFCMM4.44
      REAL DTG,PSIX,RBH,TSTV                                                     19DEC02.1812
      INTEGER ITER                                                               SFCMM4.47
      REAL BETAH,BETAM,GAMAM,GAMAH,PR0,RICRIT,RICRITI                            SFCMM4.48
      REAL BH,BM,AH,AM,SQLNZZ0                                                   SFCMM4.49
C.......Indexes                                                                  SFCMM4.50
      INTEGER I                                                                  SFCMM4.51
                                                                                 SFCMM4.52
      DATA BETAH/8.21/,BETAM/6.0/,GAMAM/19.3/,GAMAH/11.6/,PR0/0.95/              SFCMM4.53
      DATA BH/15.7/, BM/13.0/                                                    SFCMM4.54
                                                                                 SFCMM4.55
      DATA RICRIT/0.25/                                                          SFCMM4.56
      DATA CZO/0.032/, OZO/0.0001/                                               SFCMM4.57
      SAVE CZO, OZO                                                              SFCMM4.58
      SAVE BETAH,BETAM,GAMAM,GAMAH,PR0,RICRIT                                    SFCMM4.59
      SAVE BH,BM                                                                 SFCMM4.60
C                                                                                SFCMM4.61
C-----CALCULATE BULK RICHARDSON NO. OF SURFACE LAYER, ACCORDING TO               SFCMM4.62
C     AKB(1976), EQ(12).                                                         SFCMM4.63
C                                                                                SFCMM4.64
      RICRITI = 1./RICRIT                                                        SFCMM4.65
      G = G1000 * 1000.                                                          SFCMM4.66
      DO 10 I = 1,ILX                                                            SFCMM4.67
C       write(6,*) ' i,j=',i,j,' z0=',ZNT(i,j)                                   SFCMM4.68
        GZ1OZ0(I) = ALOG(Z(I,1) / ZNT(I,J))                                      SFCMM4.69
        WS(I) = SQRT(US(I,1) * US(I,1) + VS(I,1) * VS(I,1))                      19DEC02.1813
        DTHVDZ = THETAV(I,1) - TH0(I)                                            SFCMM4.71
c       DTHVM = CVMGP(-DTHVDZ, 0.0, -DTHVDZ)                                     SFCMM4.72
        DTHVM = AMAX1(-DTHVDZ,0.0)                                               19DEC02.1814
        VCONV = 2.0 * SQRT(DTHVM)                                                19DEC02.1815
        WSPD(I) = SQRT(WS(I) * WS(I) + VCONV * VCONV)                            19DEC02.1816
        WSPD(I) = AMAX1(WSPD(I),1.0 )                                            19DEC02.1817
        GOVRTH(I) = G / THETA(I,1)                                               SFCMM4.78
        RICH(I) = GOVRTH(I) * Z(I,1) * DTHVDZ / (WSPD(I) * WSPD(I))              19DEC02.1818
        RICUT(I) = 1/(RICRITI+GZ1OZ0(I))                                         SFCMM4.80
10    CONTINUE                                                                   SFCMM4.81
C                                                                                SFCMM4.82
      DO 30 I = 1,ILX                                                            SFCMM4.83
C------------------------------------------------------------------              SFCMM4.84
C-- NOTE THAT THE REGIMES USED IN HIRPBL HAVE BEEN CHANGED:                      SFCMM4.85
        ZOL = 0.0                                                                SFCMM4.86
        IF (RICH(I) .GE. RICUT(I)) THEN                                          SFCMM4.87
C                                                                                SFCMM4.88
C-----CLASS 1; VERY STABLE CONDITIONS:  Z/L > 1                                  SFCMM4.89
C                                                                                SFCMM4.90
            REGIME(I,J) = 1.0                                                    SFCMM4.91
            ZOL = RICH(I)*GZ1OZ0(I)/(1-RICRITI*RICUT(I))                         SFCMM4.92
            PSIM(I)    =  1.-BETAM-ZOL                                           SFCMM4.93
            PSIH(I)    =  1.-BETAH-ZOL                                           SFCMM4.94
                                                                                 SFCMM4.95
        ELSE IF(RICH(I).GE.0.0) THEN                                             SFCMM4.96
                                                                                 SFCMM4.97
C-----CLASS 2; STABLE: for 1 > Z/L >0                                            SFCMM4.98
C                                                                                SFCMM4.99
            REGIME(I,J) = 2.0                                                    SFCMM4.100
            ZOL = RICH(I)*GZ1OZ0(I)/(1-RICRITI*RICH(I))                          SFCMM4.101
            PSIM(I) = -BETAM * ZOL                                               SFCMM4.102
            PSIH(I) = -BETAH * ZOL                                               SFCMM4.103
                                                                                 SFCMM4.104
        ELSE                                                                     SFCMM4.105
C                                                                                SFCMM4.106
C-----CLASS 3 or 4; UNSTABLE:                                                    SFCMM4.107
C        CLASS 4 IS FOR ACM NON-LOCAL CONVECTION (H/L < -3)                      SFCMM4.108
C                                                                                SFCMM4.109
          REGIME(I,J) = 3.0   ! Regime will be reset to 4 if ACM is used         SFCMM4.110
C          ZOL = AMAX1(0.65*RICH(I) * GZ1OZ0(I), -2.0)                           SFCMM4.111
C          PSIM(I) = -ZOL * (1.86 + ZOL * (1.07 + 0.249 * ZOL))                  SFCMM4.112
C          PSIH(I) = -ZOL * (3.23 + ZOL * (1.99 + 0.474 * ZOL))                  SFCMM4.113
          AM=0.031+0.276*ALOG(GZ1OZ0(I))                                         SFCMM4.114
          AH=0.04+0.355*ALOG(GZ1OZ0(I))                                          SFCMM4.115
          SQLNZZ0=SQRT(GZ1OZ0(I))                                                SFCMM4.116
          PSIM(I) = AM*ALOG(1.-BM*SQLNZZ0*RICH(I))                               SFCMM4.117
          PSIH(I) = AH*ALOG(1.-BH*SQLNZZ0*RICH(I))                               SFCMM4.118
C                                                                                SFCMM4.119
        ENDIF                                                                    SFCMM4.120
30    CONTINUE                                                                   SFCMM4.121
C                                                                                SFCMM4.122
C-----COMPUTE THE FRICTIONAL VELOCITY AND SURFACE FLUXES:                        SFCMM4.123
C                                                                                SFCMM4.124
      DO 40 I = 1,ILX                                                            SFCMM4.125
        ITER = 0                                                                 SFCMM4.126
        DTG = THETA(I,1) - THETAG(I)                                             SFCMM4.127
        PSIX = GZ1OZ0(I) - PSIM(I)                                               SFCMM4.128
        UST(I,J) = KARMAN * WSPD(I) / PSIX                                       SFCMM4.129
        USTM(I) = KARMAN * WS(I) / PSIX                                          19DEC02.1819
C        USTM = AMAX1(UST(I,J), 0.1)                                             SFCMM4.130
c       UST(I,J) = CVMGP(UST(I,J), USTM, (XLAND(I,J) - 1.5))                     SFCMM4.131
C        IF((XLAND(I,J) - 1.5).GT.0.0)THEN                                       SFCMM4.132
C          UST(I,J) = UST(I,J)                                                   SFCMM4.133
C        ELSE                                                                    SFCMM4.134
C          UST(I,J) = USTM                                                       SFCMM4.135
C        ENDIF                                                                   SFCMM4.136
        RA(I,J) = PR0*(GZ1OZ0(I)-PSIH(I))/(KARMAN*UST(I,J))                      SFCMM4.137
        RBH = 5.0/UST(I,J) ! 5/U*  ! WESELY AND HICKS (1977)                     SFCMM4.138
        TST(I,J) = DTG / (UST(I,J)*(RA(I,J) + RBH))                              19DEC02.1820
        TSTV = (THETAV(I,1) - TH0(I)) / (UST(I,J)*(RA(I,J) + RBH))               SFCMM4.140
        IF(ABS(TSTV).LT.1.E-5)TSTV=1.E-5                                         SFCMM4.141
        MOL(I,J) = THETAV(I,1) * UST(I,J) * UST(I,J) / (KARMAN *                 SFCMM4.142
     &             G * TSTV)                                                     SFCMM4.143
        WST(I,J) = UST(I,J) * (PBL(I,J) /                                        19DEC02.1821
     &             (KARMAN * ABS(MOL(I,J)))) ** 0.333333                         SFCMM4.145
c       WST(I) = CVMGP(0.0, WST(I), MOL(I,J))                                    SFCMM4.146
        IF(MOL(I,J).GT.0.0)THEN                                                  SFCMM4.147
          WST(I,J) = 0.0                                                         19DEC02.1822
        ENDIF                                                                    SFCMM4.149
C                                                                                SFCMM4.150
C-----OVER WATER, ALTER ROUGHNESS LENGTH (Z0) ACCORDING TO WIND (UST).           SFCMM4.151
C                                                                                SFCMM4.152
      IF ((XLAND(I,J)-1.5) .GE. 0.0) THEN                                        SFCMM4.153
         ZNT(I,J) = CZO*UST(I,J)*UST(I,J)/G+OZO                                  SFCMM4.154
      ENDIF                                                                      SFCMM4.155
40    CONTINUE                                                                   SFCMM4.156
C---------------                                                                 SFCMM4.157
      RETURN                                                                     SFCMM4.158
      END                                                                        SFCMM4.159
