      SUBROUTINE SURFPX(PSURF,GSW,GLW,EMISS,PBL,MOL,ZNT,                         SURFACE.1
     &                   UST,HFX,QFX,HOL,REGIME,XLAND,THOBS,                     SURFACE.2
     &                   CAPG,PTOP,STBOLT,KARMAN,R,ILX,JLX,KL,J,                 SURFACE.3
     &                   DTPBL,NUDGE,TA2)                                        19DEC02.1823
      IMPLICIT NONE                                                              SURFACE.5
C**********************************************************************          SURFACE.6
C                                                                                SURFACE.7
C  FUNCTION:                                                                     SURFACE.8
C    THIS SUBROUTINE COMPUTES SOIL MOISTURE AND TEMPERATURE TENDENCIES           SURFACE.9
C    BY SOLVING THE PROGNOSTIC EQUATIONS IN PX95.                                SURFACE.10
C                                                                                SURFACE.11
C  SUBROUTINES CALLED:                                                           SURFACE.12
C    SUB. SFCMM4  calculate U*,TH*,W*,RA,RB & MOL                                SURFACE.13
C    SUB. QFLUX   compute the soil and canopy evaporation, and transpiration     SURFACE.14
C                                                                                SURFACE.15
C  ARGUMENTS:                                                                    SURFACE.16
C    PSURF:  P AT SURFACE (CB)                                                   SURFACE.17
C    GSW:    SHORT-WAVE RADIATION                                                SURFACE.18
C    GLW:    LONG-WAVE RADIATION                                                 SURFACE.19
C    EMISS:  EMISSIVITY                                                          SURFACE.20
C    PBL:    PBL HEIGHT (M)                                                      SURFACE.21
C    MOL:    MONIN-OBUKOV LENGH (M)                                              SURFACE.22
C    ZNT:    ROUGHNESS LENGTH (M)                                                SURFACE.23
C    UST:    FRICTION VELOCITY (M/S)                                             SURFACE.24
C    HFX:    SENSIBLE HEAT FLUX (W M^2)                                          SURFACE.25
C    QFX:    LATENT HEAT FLUX (W M^2)                                            SURFACE.26
C    HOL:    PBL HEIGHT/MONIN-OBUKHOV LENGH                                      SURFACE.27
C    REGIME: STABILITY REGIME                                                    SURFACE.28
C    XLAND:  INDEX WHICH INDICATES THE TYPE OF SURFACE,=1,LAND;=2,SEA            SURFACE.29
C    THOBS:  TEMP FROM ANAL OF OBS TEMPS AT SCREEN HT                            SURFACE.30
C    CAPG:   THERMAL CAPACITY OF GROUND SLAB (J/M^2/K)                           SURFACE.31
C    PTOP:   P AT TOP (CB)                                                       SURFACE.32
C    STBOLT: STEFAN BOLTZMANN'S CONSTANT                                         SURFACE.33
C    KARMAN: VON KARMAN CONSTANT                                                 SURFACE.34
C    R:      GAS CONSTANT FOR DRY AIR (J/KG/K)                                   SURFACE.35
C    ILX:    IMAX-1                                                              SURFACE.36
C    JLX:    JMAX-1                                                              SURFACE.37
C    KL:     TOTAL NUMBER OF LAYERS IN THE VERTICAL                              SURFACE.38
C    J:      INDEX IN EAST-WEST DIRECTION                                        SURFACE.39
C    DTPBL:  TIME STEP OF THE MINOR LOOP FOR THE LAND-SURFACE/PBL MODEL          SURFACE.40
C    NUDGE:  SWITCH FOR SOIL MOISTURE NUDGING                                    SURFACE.41
C                                                                                SURFACE.42
C  REVISION HISTORY:                                                             SURFACE.43
C    JP            5/01 - changed nugding - scale model T to obs ht
C    JP           12/99 - fixed glw term and cleanup                             SURFACE.44
C    JP           11/99 - Version 3                                              SURFACE.45
C    D. TIVEL      8/95 - Minor adjustments from MM4 to MM5                      SURFACE.46
C    JP           12/94 -- set Wr to 0.0 when < 1.0e-8                           SURFACE.47
C    JP           11/94 Updated version w/ fdda to sfmm4 version                 SURFACE.48
C    JP           11/93  -- vectorized sfcflux subroutine                        SURFACE.49
C    AX            7/93                                                          SURFACE.50
C    JP            4/93                                                          SURFACE.51
C                                                                                SURFACE.52
C**********************************************************************          SURFACE.53
C                                                                                SURFACE.54
C                                                                                SURFACE.55
#     include <parame.incl>                                                      SURFACE.56
#     include <comicl.incl>                                                      SURFACE.57
#     include <soilp.incl>                                                       SURFACE.58
C                                                                                SURFACE.59
C----------------------------------------------------------------------          SURFACE.60
C                                                                                SURFACE.61
C.......Arguments                                                                SURFACE.62
      REAL GSW(MIX,MJX),GLW(MIX,MJX),EMISS(MIX,MJX)                              SURFACE.63
      REAL MOL(MIX,MJX),PBL(MIX,MJX),UST(MIX,MJX)                                SURFACE.64
      REAL HFX(MIX,MJX),QFX(MIX,MJX)                                             SURFACE.65
      REAL THOBS(MIX),ZNT(MIX,MJX)                                               SURFACE.66
      REAL HOL(MIX,MJX),REGIME(MIX,MJX),XLAND(MIX,MJX)                           SURFACE.67
      REAL CAPG(MIX,MJX)                                                         SURFACE.68
      REAL PSURF(MIX)                                                            19DEC02.1824
      REAL PTOP,STBOLT,KARMAN,R,DTPBL                                            SURFACE.70
      INTEGER ILX,JLX,KL,J,NUDGE                                                 SURFACE.71
      REAL TA2(MIX,MJX)                                                          19DEC02.1825
C                                                                                SURFACE.72
C..........  Local arrays                                                        SURFACE.73
      REAL CT(MIX) ,CPOT(MIX)                                                    SURFACE.74
      REAL LV(MIX),CQ4(MIX)                                                      SURFACE.75
      REAL PG(MIX),W2HLF(MIX)                                                    SURFACE.76
      REAL ALPH1(MIX),ALPH2(MIX),BET1(MIX),BET2(MIX) ! ASSIM COEFFS              SURFACE.77
      REAL WGNUDG(MIX),W2NUDG(MIX)                                               SURFACE.78
      REAL RAW(MIX),RAH(MIX)                                                     SURFACE.79
C.......Local variables                                                          SURFACE.80
      REAL ZOBS,ROVCPX,RHMOD                                                     SURFACE.81
      REAL HF,ET                                                                 SURFACE.82
      REAL W2CG,CG,SOILFLX                                                       SURFACE.83
      REAL T1P5                                                                  SURFACE.84
      REAL CQ1,CQ2,CQ3,COEFFNP1,COEFFN,TSNEW,TSHLF,T2NEW                         SURFACE.85
      REAL ROFF,WRMAX,PC,DWR,PNET                                                SURFACE.86
      REAL TENDWR,WRNEW,COF1,CFNP1WR,CFNWR                                       SURFACE.87
      REAL TENDW2,W2NEW,W2REL                                                    SURFACE.88
      REAL C1,C2,WEQ,CFNP1,CFN,WGNEW                                             SURFACE.89
      REAL PSIH15,GAMAH,ZOL,ZOBOL,Y,Y0
      REAL PSIH,BETAH,ZNTOL,XMOL                                                 19DEC02.1826
      INTEGER ISTI                                                               SURFACE.90
C.......Indexes                                                                  SURFACE.91
      INTEGER I                                                                  SURFACE.92
C                                                                                SURFACE.93
      DATA ZOBS/1.5/  ! height for observed screen temp., (m)                    SURFACE.94
      DATA GAMAH/11.6/,BETAH/8.21/                                               19DEC02.1827
C                                                                                SURFACE.95
      ROVCPX = 287.04/1004.   !  SAME AS ROVCP                                   SURFACE.96
                                                                                 SURFACE.97
      DO 10 I = 1,ILX                                                            SURFACE.98
        RADNET(I,J) = GSW(I,J) - (EMISS(I,J) *(STBOLT *TG(I) **4                 SURFACE.99
     &           - GLW(I,J)))        ! NET RADIATION                             SURFACE.100
10    CONTINUE                                                                   SURFACE.101
C                                                                                SURFACE.102
C  CALCULATE (U*,THETA*,W*,MOL)                                                  SURFACE.103
C                                                                                SURFACE.104
      CALL SFCMM4(PBL,UST,MOL,HOL,REGIME,XLAND,ZNT,KARMAN,                       SURFACE.105
     &            J,ILX,KL)                                                      19DEC02.1828
C                                                                                SURFACE.107
      DO 22 I = 1,ILX                                                            SURFACE.108
C.... compute the sensible heat flux                                             SURFACE.109
        HF = UST(I,J) * TST(I,J)                                                 19DEC02.1829
        HFX(I,J) = -DENS(I,1) * CPAIR(I) * HF                                    SURFACE.111
        HFX(I,J) = AMAX1(HFX(I,J), -250.0)  ! using -250. from MM5               SURFACE.112
                                                                                 SURFACE.113
C-- ADD RA AND RB FOR HEAT AND MOISTURE                                          SURFACE.114
C... RB FOR HEAT = 5 /UST                                                        SURFACE.115
C... RB FOR WATER VAPOR =  5*(0.599/0.709)^2/3 /UST = 4.47/UST                   SURFACE.116
        RAH(I) = RA(I,J) + 5.0/UST(I,J)                                          SURFACE.117
        RAW(I) = RA(I,J) + 4.47/UST(I,J)                                         SURFACE.118
                                                                                 SURFACE.119
22    CONTINUE                                                                   SURFACE.120
C                                                                                SURFACE.121
C--MOISTURE FLUX                                                                 SURFACE.122
      CALL QFLUX(ILX,J,CQ4,RAW,XLAND)                                            SURFACE.123
C                                                                                SURFACE.124
      DO 23 I = 1,ILX                                                            SURFACE.125
C..........Total evaporation (ET)                                                SURFACE.126
        ET = EG(I) + ER(I) + ETR(I)                                              SURFACE.127
        QST(I) = -ET / (DENS(I,1) * UST(I,J))                                    SURFACE.128
        ISTI = ISLTYP(I,J)                                                       SURFACE.129
        IF (ISNOW(I) .NE. 1) THEN                                                SURFACE.130
C..........  This LV is FROM STULL(1988) at J/KG                                 SURFACE.131
          LV(I) = (2.501 - 0.00237 * (TG(I) - 273.15)) * 1.E6                    SURFACE.132
         ELSE                                                                    SURFACE.133
C--        LATENT HEAT OF SUBLIMATION AT 0C FROM STULL(1988)                     SURFACE.134
          LV(I) = 2.83E6            !J/KG                                        SURFACE.135
        ENDIF                                                                    SURFACE.136
        IF (XLAND(I,J).LT.1.5 ) THEN                                             SURFACE.137
          if(isti.gt.11.or.isti.lt.1) print *,' i,j,ist=',i,j,isti               SURFACE.138
          W2CG = AMAX1(W2(I),WWLT(ISTI))                                         SURFACE.139
          CG = CGSAT(ISTI) * 1.0E-6 * (WSAT(ISTI) / W2CG) **                     SURFACE.140
     &         (0.5 * B(ISTI) / ALN10)                                           SURFACE.141
C         CT(I)=1./((1-VEGFRC(ILU))/CG+VEGFRC(ILU)/CV)                           SURFACE.142
          IF (ISNOW(I) .EQ. 1) THEN                                              SURFACE.143
C--    FOR SNOW COVERED SURFACE                                                  SURFACE.144
            CT(I) = 5.54E-5           ! M2*K/J Deardorff 78;  BATS               SURFACE.145
           ELSE                                                                  SURFACE.146
C..........  TRY THIS CT FOR FIFE, MAYBE USE NP89 FORM ONLY FOR FOREST?          SURFACE.147
            CT(I) = CG                                                           SURFACE.148
          ENDIF                                                                  SURFACE.149
          SOILFLX = 2.0 * PI * TAUINV * (TG(I) - T2(I))                          SURFACE.150
          GRNFLX(I,J) = SOILFLX / CT(I)                                          SURFACE.151
          CAPG(I,J) = 1.0 / CT(I)                                                SURFACE.152
        ENDIF                                                                    SURFACE.153
        QFX(I,J) = LV(I) * ET                                                    SURFACE.154
        CPOT(I)= (1000.0 / PSURF(I)) ** ROVCPX                                   SURFACE.193
23    CONTINUE                                                                   SURFACE.155
C                                                                                SURFACE.156
C-- ASSIMILATION --- COMPUTE SOIL MOISTURE NUDGING FROM Ta and RH                SURFACE.157
C-------COMPUTE ASSIMILATION COEFFICIENTS FOR ALL I                              SURFACE.158
      IF (NUDGE .EQ. 0 .OR. (J .LE. 2 .OR. J .GE. JLX)) THEN                     SURFACE.159
        DO 144 I = 1,ILX                                                         SURFACE.160
          WGNUDG(I) = 0.0                                                        SURFACE.161
          W2NUDG(I) = 0.0                                                        SURFACE.162
          XMOL = MOL(I,J)                                                        19DEC02.1830
          IF(MOL(I,J).GT.0.0) XMOL = AMAX1(MOL(I,J),2.0)                         19DEC02.1831
          ZOL=Z(I,1)/XMOL                                                        19DEC02.1832
          ZOBOL=ZOBS/XMOL                                                        19DEC02.1833
          ZNTOL = ZNT(I,J)/XMOL                                                  19DEC02.1834
          IF(MOL(I,J).LT.0.0) THEN                                               19DEC02.1835
            Y = ( 1.0 - GAMAH * ZOL )**0.5                                       19DEC02.1836
            Y0 = ( 1.0 - GAMAH * ZOBOL )**0.5                                    19DEC02.1837
            PSIH15 =  2. * ALOG((Y+1.0)/(Y0+1.0))                                19DEC02.1838
          ELSE                                                                   19DEC02.1839
            IF((ZOL-ZNTOL).LE.1.0) THEN                                          19DEC02.1840
               PSIH = -BETAH*(ZOL-ZNTOL)                                         19DEC02.1841
             ELSE                                                                19DEC02.1842
               PSIH = 1.-BETAH-(ZOL-ZNTOL)                                       19DEC02.1843
             ENDIF                                                               19DEC02.1844
             IF((ZOBOL-ZNTOL).LE.1.0) THEN                                       19DEC02.1845
               PSIH15 = -BETAH*(ZOBOL-ZNTOL)                                     19DEC02.1846
             ELSE                                                                19DEC02.1847
               PSIH15 = 1.-BETAH-(ZOBOL-ZNTOL)                                   19DEC02.1848
             ENDIF                                                               19DEC02.1849
             PSIH15 =  PSIH - PSIH15                                             19DEC02.1850
C            PSIH15 = PSIH(I) - PSIH(I) * ZOBS/ Z(I,1)                           19DEC02.1851
          ENDIF                                                                  19DEC02.1852
          TA2(I,J) = (THETA(I,1) - TST(I,J) / KARMAN *                           19DEC02.1853
     &            (ALOG(Z(I,1) / ZOBS) - PSIH15))/CPOT(I)                        19DEC02.1854
 144     CONTINUE                                                                19DEC02.1855
      ELSE                                                                       SURFACE.164
        CALL SMASS(ALPH1,ALPH2,BET1,BET2,J,ILX)                                  SURFACE.165
C                                                                                SURFACE.166
        DO 44 I = 1,ILX                                                          SURFACE.167
          ISTI = ISLTYP(I,J)                                                     SURFACE.168
C-- Scale Model Pot Temp down to Screen Ht                                       SURFACE.172
          THZ1OB(I,J) = THOBS(I)       ! dont scale obs                          SURFACE.176
          XMOL = MOL(I,J)                                                        19DEC02.1856
          IF(MOL(I,J).GT.0.0) XMOL = AMAX1(MOL(I,J),2.0)                         19DEC02.1857
          ZOL=Z(I,1)/XMOL                                                        19DEC02.1858
          ZOBOL=ZOBS/XMOL                                                        19DEC02.1859
          ZNTOL = ZNT(I,J)/XMOL                                                  19DEC02.1860
          IF(MOL(I,J).LT.0.0) THEN
            Y = ( 1.0 - GAMAH * ZOL )**0.5
            Y0 = ( 1.0 - GAMAH * ZOBOL )**0.5
            PSIH15 =  2. * ALOG((Y+1.0)/(Y0+1.0))
          ELSE 
            IF((ZOL-ZNTOL).LE.1.0) THEN                                          19DEC02.1861
               PSIH = -BETAH*(ZOL-ZNTOL)                                         19DEC02.1862
             ELSE                                                                19DEC02.1863
               PSIH = 1.-BETAH-(ZOL-ZNTOL)                                       19DEC02.1864
             ENDIF                                                               19DEC02.1865
             IF((ZOBOL-ZNTOL).LE.1.0) THEN                                       19DEC02.1866
               PSIH15 = -BETAH*(ZOBOL-ZNTOL)                                     19DEC02.1867
             ELSE                                                                19DEC02.1868
               PSIH15 = 1.-BETAH-(ZOBOL-ZNTOL)                                   19DEC02.1869
             ENDIF                                                               19DEC02.1870
             PSIH15 =  PSIH - PSIH15                                             19DEC02.1871
C            PSIH15 = PSIH(I) - PSIH(I) * ZOBS/ Z(I,1)                           19DEC02.1872
          ENDIF            
          T1P5 = (THETA(I,1) - TST(I,J) / KARMAN *                               19DEC02.1873
     &            (ALOG(Z(I,1) / ZOBS) - PSIH15))/CPOT(I)                        SURFACE.176
          TA2(I,J)= T1P5                                                         19DEC02.1874
C--COMPUTE MODEL RH                                                              SURFACE.178
          RHMOD = RHX(I,1) * 100.0                                               SURFACE.179
          WGNUDG(I) = ALPH1(I) * (THZ1OB(I,J) - T1P5)                            SURFACE.180
     >              + ALPH2(I) * (RHOBS(I,J) - RHMOD)  ! m/s                     SURFACE.181
          W2NUDG(I) = BET1(I) * (THZ1OB(I,J) - T1P5)                             SURFACE.182
     >              + BET2(I) * (RHOBS(I,J) - RHMOD)                             SURFACE.183
          IF(W2(I).GE.WFC(ISTI)) W2NUDG(I) = AMIN1(W2NUDG(I),0.0)                SURFACE.184
          IF(W2(I).LE.WWLT(ISTI)) W2NUDG(I) = AMAX1(W2NUDG(I),0.0)               SURFACE.185
44      CONTINUE                                                                 SURFACE.186
      ENDIF                   ! endif for nudge=0                                SURFACE.187
C-----------------------------------------------                                 SURFACE.188
C                                                                                SURFACE.189
C..........  Compute new value for TS,T2,WG,W2 and WR                            SURFACE.190
      DO 50 I = 1,ILX                                                            SURFACE.191
        ISTI = ISLTYP(I,J)                                                       SURFACE.192
C                                                                                SURFACE.194
C-- COMPUTE TENDENCIES:                                                          SURFACE.195
C-- OVER OCEAN OR PERMANENT ICE, NO CHANGE TO TS,T2,WR,W2 AND WG                 SURFACE.196
        IF (XLAND(I,J).LT.1.5 ) THEN                                             SURFACE.197
C--TEMPERATURE                                                                   SURFACE.198
C-TG                                                                             SURFACE.199
C                                                                                SURFACE.200
C-- SOLVE BY CRANK-NIC --   TENDTS=CT*(RADNET-HFX-QFX)-SOILFLX                   SURFACE.201
C-- N+1 COEFF                                                                    SURFACE.202
C... calculate the coefficients for implicit calculation of TG                   SURFACE.203
          CQ1 = (1.0 - 0.622 * LV(I) * CRANKP / (R * TG(I))) * QSS(I)            SURFACE.204
          CQ2 = 0.622 * LV(I) * QSS(I) * CRANKP / (R * TG(I) * TG(I))            SURFACE.205
          CQ3 = DENS(I,1) * BETAP(I)*(1.0 - VEGFRC(I,J)) / RAW(I)                SURFACE.206
          COEFFNP1 = 1.0 + DTPBL * CRANKP * (4.0 * EMISS(I,J) *                  SURFACE.207
     &               STBOLT * TG(I) ** 3 * CT(I) + DENS(I,1) *                   SURFACE.208
     &               CPAIR(I) / RAH(I) * CPOT(I) * CT(I) +                       SURFACE.209
     &               2.0 * PI * TAUINV ) + DTPBL * (CT(I) *                      SURFACE.210
     &               LV(I) * CQ2 * (CQ3 + CQ4(I)))                               SURFACE.211
          COEFFN = CT(I) * (GSW(I,J) + EMISS(I,J) * (STBOLT * (4.0 *             SURFACE.212
     &            CRANKP - 1.0) * TG(I) ** 4 + GLW(I,J))    !NET RAD             SURFACE.213
     &            + DENS(I,1) * CPAIR(I) / RAH(I) * (THETA(I,1) -                SURFACE.214
     &            (1.0 - CRANKP) * THETAG(I)) - LV(I) * (CQ3 *                   SURFACE.215
     &            (CQ1 - QVS(I,1)) + CQ4(I) *                                    SURFACE.216
     &            (CQ1 - QVS(I,1)))) !SFC HEAT FLUX                              SURFACE.217
     &            - 2.0 * PI * TAUINV *                                          SURFACE.218
     &            ((1.0 - CRANKP) * TG(I) - T2(I))    !SOIL FLUX                 SURFACE.219
          TSNEW = (TG(I) + DTPBL * COEFFN) / COEFFNP1                            SURFACE.220
C*                                                                               SURFACE.221
C-- FOR SNOW COVERED SURFACE TEMPERATURE IS NOT MORE THAN ZERO                   SURFACE.222
          IF (ISNOW(I) .EQ.1) TSNEW = AMIN1(TSNEW,273.15)                        SURFACE.223
C*                                                                               SURFACE.224
          TSHLF = 0.5 * ( TSNEW + TG(I))                                         SURFACE.225
C-T2                                                                             SURFACE.226
C          T2NEW = (T2(I) + DTPBL * TAUINV * (TSHLF - (1 - CRANKP) *             SURFACE.227
C     &            T2(I))) / (1.0 + DTPBL * TAUINV * CRANKP)                     SURFACE.228
          T2NEW = (T2(I) + DTPBL * TAUINV * 0.1 * (TSHLF -                       SURFACE.229
     &            (1 - CRANKP) * T2(I))) / (1.0 + DTPBL *                        SURFACE.230
     &             TAUINV * 0.1 * CRANKP)                                        SURFACE.231
C** New Values                                                                   SURFACE.232
          TG(I) = TSNEW                                                          SURFACE.233
          T2(I) = T2NEW                                                          SURFACE.234
        ENDIF                                                                    SURFACE.235
50    CONTINUE                                                                   SURFACE.236
C*                                                                               SURFACE.237
      DO 60 I = 1,ILX                                                            SURFACE.238
        IF (XLAND(I,J).LT.1.5) THEN  ! over ocean no change to wg w2,wr          SURFACE.239
          ISTI = ISLTYP(I,J)                                                     SURFACE.240
C-WR                                                                             SURFACE.241
          ROFF = 0.0                                                             SURFACE.242
          WRMAX = 0.2E-3 * VEGFRC(I,J) * LAI(I,J)  ! max. WR IN M                SURFACE.243
C..........  PC is precip. intercepted by veg.(M/S)                              SURFACE.244
          PC = VEGFRC(I,J) * SIGF * PRECIP(I)                                    SURFACE.245
          DWR = (WRMAX - WR(I)) / DTPBL !the tendency to reach max.              SURFACE.246
          PNET = PC - ER(I)/ DENW     ! residual of precip. and evap.            SURFACE.247
          IF (PNET .GT. DWR) THEN                                                SURFACE.248
            ROFF = PNET - DWR                                                    SURFACE.249
            PC = PC - ROFF                                                       SURFACE.250
          ENDIF                                                                  SURFACE.251
          IF (QSS(I) .LT. QVS(I,1)) THEN                                         SURFACE.252
            TENDWR = PC - ER(I) / DENW                                           SURFACE.253
            WRNEW = WR(I) + DTPBL * TENDWR                                       SURFACE.254
           ELSE                                                                  SURFACE.255
            COF1 = DENS(I,1) / DENW * VEGFRC(I,J) *                              SURFACE.256
     &             (QSS(I) - QVS(I,1)) / RAW(I)                                  SURFACE.257
C... using delta=wr/wrmax                                                        SURFACE.258
            CFNP1WR = 1.0 + DTPBL * COF1 * CRANKP / WRMAX                        SURFACE.259
            CFNWR = PC - COF1 * (1.0 - CRANKP) * WR(I) / WRMAX                   SURFACE.260
C                                                                                SURFACE.261
            WRNEW = (WR(I) + DTPBL * CFNWR) / CFNP1WR                            SURFACE.262
          ENDIF                                                                  SURFACE.263
C-W2                                                                             SURFACE.264
C..........  PG is precip. reaching soil                                         SURFACE.265
          PG(I) = DENW * (PRECIP(I) - PC)   ! PC already including ROFF          SURFACE.266
          TENDW2 = 1.0 / (DENW * DS2) * (PG(I) - EG(I) - ETR(I))                 SURFACE.267
     &             + (W2NUDG(I) + WGNUDG(I)) / DS2     ! NUDGING                 SURFACE.268
          W2NEW = W2(I) + DTPBL * TENDW2                                         SURFACE.269
          W2NEW = AMIN1(W2NEW,WSAT(ISTI))                                        SURFACE.270
          W2NEW = AMAX1(W2NEW,0.05)                                              SURFACE.271
          W2HLF(I) = 0.5 * (W2(I) + W2NEW)                                       SURFACE.272
C.. new values                                                                   SURFACE.273
          W2(I) = W2NEW                                                          SURFACE.274
          WR(I) = AMIN1(WRMAX,WRNEW)                                             SURFACE.275
          if(WR(I).lt.1.0E-8) WR(I) = 0.0  !  12/94 jp                           SURFACE.276
        ENDIF                                                                    SURFACE.277
60    CONTINUE                                                                   SURFACE.278
C                                                                                SURFACE.279
C-WG                                                                             SURFACE.280
        DO 70 I =1,ILX                                                           SURFACE.281
          IF (XLAND(I,J).LT.1.5) THEN  ! over ocean no change to wg w2,wr        SURFACE.282
            ISTI = ISLTYP(I,J)                                                   SURFACE.283
C--    FOR SNOW COVERED SURFACE, ASSUME SURFACE IS SATURATED AND                 SURFACE.284
C      WG AND W2 ARE NOT CHANGED                                                 SURFACE.285
            IF (ISNOW(I) .EQ.1) THEN                                             SURFACE.286
              WG(I) = WSAT(ISTI)                                                 SURFACE.287
             ELSE                                                                SURFACE.288
              W2REL = W2HLF(I) / WSAT(ISTI)                                      SURFACE.289
              IF (WG(I) .GT. WWLT(ISTI)) THEN                                    SURFACE.290
                C1 = C1SAT(ISTI) * (WSAT(ISTI) / WG(I)) **                       SURFACE.291
     &               (0.5 * B(ISTI) + 1.0)                                       SURFACE.292
               ELSE         ! elimilate C1 for wg < wilting point                SURFACE.293
                C1 = C1SAT(ISTI) * (WSAT(ISTI) / WWLT(ISTI)) **                  SURFACE.294
     &               (0.5 * B(ISTI) + 1.0)                                       SURFACE.295
              ENDIF                                                              SURFACE.296
              C2 = C2R(ISTI) * W2HLF(I) / (WSAT(ISTI) -                          SURFACE.297
     &             W2HLF(I) + 1.E-11)                                            SURFACE.298
              IF (W2HLF(I) .EQ. WSAT(ISTI)) THEN                                 SURFACE.299
                WEQ = WSAT(ISTI)                                                 SURFACE.300
               ELSE                                                              SURFACE.301
                WEQ = W2HLF(I) - AS(ISTI) * WSAT(ISTI) *                         SURFACE.302
     &                W2REL ** JP(ISTI) * (1.0 -                                 SURFACE.303
     &                W2REL ** (8.0 * JP(ISTI)))                                 SURFACE.304
              ENDIF                                                              SURFACE.305
C                                                                                SURFACE.306
C.... this is for the beta method, Lee & Pielke (JAM, May 1992)                  SURFACE.307
              CFNP1 = 1.0 + DTPBL * C2 * TAUINV * CRANKP                         SURFACE.308
              CFN = C1 / (DENW * DS1) * (PG(I) - EG(I)) - C2 *                   SURFACE.309
     &              TAUINV * ((1.0 - CRANKP) * WG(I) - WEQ)                      SURFACE.310
     &               + WGNUDG(I)/DS1   ! NUDGING                                 SURFACE.311
C                                                                                SURFACE.312
              WGNEW = AMAX1((WG(I) + DTPBL * CFN) / CFNP1,0.001)                 SURFACE.313
C-- NEW VALUES                                                                   SURFACE.314
              WG(I) = AMIN1(WGNEW,WSAT(ISTI))                                    SURFACE.315
            ENDIF                  !endif for ISNOW                              SURFACE.316
          ENDIF                    !endif for XLAND                              SURFACE.317
C                                                                                SURFACE.318
70      CONTINUE                                                                 SURFACE.319
      RETURN                                                                     SURFACE.320
      END                                                                        SURFACE.321
