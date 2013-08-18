      SUBROUTINE MM5ATM(I,J,CLDFRA,O3PROF,INEST,T3D,QV3D,QC3D,QR3D               MM5ATM.1
     +           ,QI3D,QNI3D,QG3D,PP3D,PSA,TGA,EMISS)                            MM5ATM.2
C***************************************************************************     MM5ATM.3
C  RRTM Longwave Radiative Transfer Model                                        MM5ATM.4
C  Atmospheric and Environmental Research, Inc., Cambridge, MA                   MM5ATM.5
C                                                                                MM5ATM.6
C  Revision for NCAR MM5:  J. Dudhia (converted from CCM code)                   MM5ATM.7
C                                                                                MM5ATM.8
C  Input atmospheric profile from NCAR MM5, and prepare it for use in RRTM.      MM5ATM.9
C  Set other RRTM input parameters.  Values are passed back through existing     MM5ATM.10
C  RRTM arrays and commons.                                                      MM5ATM.11
C***************************************************************************     MM5ATM.12
                                                                                 MM5ATM.13
#     include <parame.incl>                                                      MM5ATM.14
#     include <nonhyd.incl>                                                      MM5ATM.15
#     include <nhcnst.incl>                                                      MM5ATM.16
#     include <param2.incl>                                                      MM5ATM.17
#     include <param3.incl>                                                      MM5ATM.18
#     include <rpstar.incl>                                                      MM5ATM.19
#     include <radiat.incl>                                                      MM5ATM.20
#ifndef MPP1                                                                     MM5ATM.21
      DIMENSION CLDFRA(MIX-1,MKX),O3PROF(MIX-1,KXP1),CLDFRC(MKX)                 MM5ATM.22
#else                                                                            MM5ATM.23
      DIMENSION CLDFRA(MIX,MKX),O3PROF(MIX,KXP1),CLDFRC(MKX)                     MM5ATM.24
#endif                                                                           MM5ATM.25
      DIMENSION P(MKX),PINT(KXP1),T(MKX),TINT(KXP1),QV(MKX),                     MM5ATM.26
     -    QC(MKX),QR(MKX),QI(MKX),QNI(MKX),QG(MKX)                               MM5ATM.27
      DIMENSION O3(MKX),N2O(MKX),CH4(MKX)                                        MM5ATM.28
      DIMENSION CLWP(MKX),CIWP(MKX),PLWP(MKX),PIWP(MKX)                          MM5ATM.29
     &           ,T3D(MIX,MJX,MKX),                                              MM5ATM.30
     &           QV3D(MIX,MJX,MKX), QC3D(MIXM,MJXM,MKXM),                        MM5ATM.31
     &           PP3D(MIX,MJX,MKX), QR3D(MIXM,MJXM,MKXM),                        MM5ATM.32
     &           QI3D(MIXIC,MJXIC,MKXIC),QNI3D(MIXIC,MJXIC,MKXIC),               MM5ATM.33
     &           QG3D(MIXIG,MJXIG,MKXIG),                                        MM5ATM.34
     &           PSA(MIX,MJX),TGA(MIX,MJX),EMISS(MIX,MJX)                        MM5ATM.35
C Parameters                                                                     MM5ATM.36
      PARAMETER (MXLAY=203)                                                      MM5ATM.37
      PARAMETER (MAXINPX=35)                                                     MM5ATM.38
      PARAMETER (MXCBANDS=5)                                                     MM5ATM.39
      PARAMETER (MAXXSEC=4)                                                      MM5ATM.40
      PARAMETER (MAXPRDW = MXLAY*35)                                             MM5ATM.41
      PARAMETER (MAXPROD = MXLAY*MAXXSEC)                                        MM5ATM.42
      PARAMETER (NBANDS=16)                                                      MM5ATM.43
      PARAMETER (GRAVIT = 980.62)                                                MM5ATM.44
C Input                                                                          MM5ATM.45
      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                       MM5ATM.46
     &                  PZ(0:MXLAY),TZ(0:MXLAY)                                  MM5ATM.47
      COMMON /XSECCTRL/ NXMOL,IXINDX(MAXINPX)                                    MM5ATM.48
      COMMON /SURFACE/  TBOUND,IREFLECT,SEMISS(NBANDS)                           MM5ATM.49
C Output                                                                         MM5ATM.50
      COMMON /SPECIES/  COLDRY(MXLAY),WKL(35,MXLAY),WBRODL(MXLAY),               MM5ATM.51
     &                  NMOL                                                     MM5ATM.52
      COMMON /XSEC/     WX(MAXXSEC,MXLAY)                                        MM5ATM.53
      COMMON /CLOUDDAT/ CLDFRAC(MXLAY),TAUCLOUD(MXLAY)                           MM5ATM.54
C RRTM Definitions                                                               MM5ATM.55
C    MXLAY                        ! Maximum number of model layers               MM5ATM.56
C    MAXXSEC                      ! Maximum number of cross sections             MM5ATM.57
C    NLAYERS                      ! Number of model layers (mkx+1)               MM5ATM.58
C    PAVEL(MXLAY)                 ! Layer pressures (mb)                         MM5ATM.59
C    PZ(0:MXLAY)                  ! Level (interface) pressures (mb)             MM5ATM.60
C    TAVEL(MXLAY)                 ! Layer temperatures (K)                       MM5ATM.61
C    TZ(0:MXLAY)                  ! Level (interface) temperatures(mb)           MM5ATM.62
C    TBOUND                       ! Surface temperature (K)                      MM5ATM.63
C    COLDRY(MXLAY)                ! Dry air column (molecules/cm2)               MM5ATM.64
C    WKL(35,MXLAY)                ! Molecular amounts (molecules/cm2)            MM5ATM.65
C    WBRODL(MXLAY)                ! Inactive in this version                     MM5ATM.66
C    WX(MAXXSEC)                  ! Cross-section amounts (molecules/cm2)        MM5ATM.67
C    CLDFRAC(MXLAY)               ! Layer cloud fraction                         MM5ATM.68
C    TAUCLOUD(MXLAY)              ! Layer cloud optical depth                    MM5ATM.69
C    AMD                          ! Atomic weight of dry air                     MM5ATM.70
C    AMW                          ! Atomic weight of water                       MM5ATM.71
C    AMO                          ! Atomic weight of ozone                       MM5ATM.72
C    AMCH4                        ! Atomic weight of methane                     MM5ATM.73
C    AMN2O                        ! Atomic weight of nitrous oxide               MM5ATM.74
C    AMC11                        ! Atomic weight of CFC-11                      MM5ATM.75
C    AMC12                        ! Atomic weight of CFC-12                      MM5ATM.76
C    NXMOL                        ! Number of cross-section molecules            MM5ATM.77
C    IXINDX                       ! Cross-section molecule index (see below)     MM5ATM.78
C    IXSECT                       ! On/off flag for cross-sections (inactive)    MM5ATM.79
C    IXMAX                        ! Maximum number of cross-sections (inactive)  MM5ATM.80
C                                                                                MM5ATM.81
                                                                                 MM5ATM.82
C This compiler directive was added to insure private common block storage       MM5ATM.83
C in multi-tasked mode on a CRAY or SGI for all commons except those that        MM5ATM.84
C carry constants.                                                               MM5ATM.85
C SGI                                                                            MM5ATM.88
C$OMP THREADPRIVATE (/PROFILE/,/SPECIES/,/XSEC/,/CLOUDDAT/)                      MM5ATM.89
C$OMP THREADPRIVATE (/SURFACE/)                                                  MM5ATM.90
C ***                                                                            MM5ATM.91
                                                                                 MM5ATM.92
      real amd                  ! Effective molecular weight of dry air (g/mol)  MM5ATM.93
      real amw                  ! Molecular weight of water vapor (g/mol)        MM5ATM.94
      real amo                  ! Molecular weight of ozone (g/mol)              MM5ATM.95
      real amch4                ! Molecular weight of methane (g/mol)            MM5ATM.96
      real amn2o                ! Molecular weight of nitrous oxide (g/mol)      MM5ATM.97
      real amc11                ! Molecular weight of CFC11 (g/mol) - CFCL3      MM5ATM.98
      real amc12                ! Molecular weight of CFC12 (g/mol) - CF2CL2     MM5ATM.99
      real avgdro               ! Avogadro's number (molecules/mole)             MM5ATM.100
                                                                                 MM5ATM.101
C Atomic weights for conversion from mass to volume mixing ratios                MM5ATM.102
      data amd   /  28.9644   /                                                  MM5ATM.103
      data amw   /  18.0154   /                                                  MM5ATM.104
      data amo   /  47.9998   /                                                  MM5ATM.105
      data amch4 /  16.0430   /                                                  MM5ATM.106
      data amn2o /  44.0128   /                                                  MM5ATM.107
      data amc11 / 137.3684   /                                                  MM5ATM.108
      data amc12 / 120.9138   /                                                  MM5ATM.109
      data avgdro/ 6.022E23   /                                                  MM5ATM.110
                                                                                 MM5ATM.111
C Set molecular weight ratios                                                    MM5ATM.112
      real amdw,                ! Molecular weight of dry air / water vapor      MM5ATM.113
     $     amdc,                ! Molecular weight of dry air / methane          MM5ATM.114
     $     amdn,                ! Molecular weight of dry air / nitrous oxide    MM5ATM.115
     $     amdc1,               ! Molecular weight of dry air / CFC11            MM5ATM.116
     $     amdc2                ! Molecular weight of dry air / CFC12            MM5ATM.117
      data amdw /  1.607758 /                                                    MM5ATM.118
      data amdc /  1.805423 /                                                    MM5ATM.119
      data amdn /  0.658090 /                                                    MM5ATM.120
      data amdc1/  0.210852 /                                                    MM5ATM.121
      data amdc2/  0.239546 /                                                    MM5ATM.122
c  Put in CO2 volume mixing ratio here (330 ppmv)                                MM5ATM.123
      data co2vmr / 330.e-6 /                                                    MM5ATM.124
                                                                                 MM5ATM.125
C  Activate cross section molecules:                                             MM5ATM.126
C     NXMOL     - number of cross-sections input by user                         MM5ATM.127
C     IXINDX(I) - index of cross-section molecule corresponding to Ith           MM5ATM.128
C                 cross-section specified by user                                MM5ATM.129
C                 = 0 -- not allowed in RRTM                                     MM5ATM.130
C                 = 1 -- CCL4                                                    MM5ATM.131
C                 = 2 -- CFC11                                                   MM5ATM.132
C                 = 3 -- CFC12                                                   MM5ATM.133
C                 = 4 -- CFC22                                                   MM5ATM.134
      DATA NXMOL  /2/                                                            MM5ATM.135
      DATA IXINDX /0,2,3,0,31*0/                                                 MM5ATM.136
                                                                                 MM5ATM.137
C    CLOUD EMISSIVITIES (M^2/G)                                                  MM5ATM.138
C    THESE ARE CONSISTENT WITH LWRAD (ABCW=0.5*(ABUP+ABDOWN))                    MM5ATM.139
      DATA ABCW /0.144/                                                          MM5ATM.140
      DATA ABICE /0.0735/                                                        MM5ATM.141
      DATA ABRN /0.330E-3/                                                       MM5ATM.142
      DATA ABSN /2.34E-3/                                                        MM5ATM.143
                                                                                 MM5ATM.144
                                                                                 MM5ATM.145
                                                                                 MM5ATM.146
C  Extract columns from MM5                                                      MM5ATM.147
      IF(I.EQ.3.AND.J.EQ.3)PRINT5001                                             MM5ATM.148
 5001 FORMAT(1X,'CALCULATING RRTM RADIATION')                                    MM5ATM.149
        PSAA=PSA(I,J)                                                            MM5ATM.150
C                                                                                MM5ATM.151
C  MID-LAYER VALUES                                                              MM5ATM.152
      DO K=1,MKX                                                                 MM5ATM.153
          P(K)=10.*(PSAA*A(K)+PTOP)+PP3D(I,J,K)*0.01                             MM5ATM.154
          T(K)=T3D(I,J,K)                                                        MM5ATM.155
                                                                                 MM5ATM.156
          RO=P(K)/(R*T(K))*100.                                                  MM5ATM.157
          RO0=P(K)/(R*T0(I,J,K))*100.                                            MM5ATM.158
          DZ=PSAA*DSIGMA(K)*1000./(RO0*G)                                        MM5ATM.159
                                                                                 MM5ATM.160
          QV(K)=AMAX1(QV3D(I,J,K),1.E-12)                                        MM5ATM.161
          QC(K)=QC3D(I,J,K)                                                      MM5ATM.162
          QR(K)=QR3D(I,J,K)                                                      MM5ATM.163
                                                                                 MM5ATM.164
          IF(IICE.EQ.1)THEN                                                      MM5ATM.165
            QI(K)=QI3D(I,J,K)                                                    MM5ATM.166
            QNI(K)=QNI3D(I,J,K)                                                  MM5ATM.167
          ELSE                                                                   MM5ATM.168
            QI(K)=0.0                                                            MM5ATM.169
            QNI(K)=0.0                                                           MM5ATM.170
          ENDIF                                                                  MM5ATM.171
          IF(IICEG.EQ.1)THEN                                                     MM5ATM.172
            QG(K)=QG3D(I,J,K)                                                    MM5ATM.173
          ELSE                                                                   MM5ATM.174
            QG(K)=0.0                                                            MM5ATM.175
          ENDIF                                                                  MM5ATM.176
         CLDFRC(K)=CLDFRA(I,K)                                                   MM5ATM.177
                                                                                 MM5ATM.178
C  PATHS IN G/M^2                                                                MM5ATM.179
          IF(IMPHYS(INEST).EQ.4)THEN                                             MM5ATM.180
            IF(T(K).GT.273.15)THEN                                               MM5ATM.181
              CLWP(K)=RO*QC(K)*DZ*1000.                                          MM5ATM.182
              PLWP(K)=(RO*QR(K))**0.75*DZ*1000.                                  MM5ATM.183
              CIWP(K)=0.0                                                        MM5ATM.184
              PIWP(K)=0.0                                                        MM5ATM.185
            ELSE                                                                 MM5ATM.186
              CLWP(K)=0.0                                                        MM5ATM.187
              PLWP(K)=0.0                                                        MM5ATM.188
              CIWP(K)=RO*QC(K)*DZ*1000.                                          MM5ATM.189
              PIWP(K)=(RO*QR(K))**0.75*DZ*1000.                                  MM5ATM.190
            ENDIF                                                                MM5ATM.191
          ELSE IF(IMPHYS(INEST).GE.5)THEN                                        MM5ATM.192
            CLWP(K)=RO*QC(K)*DZ*1000.                                            MM5ATM.193
            CIWP(K)=RO*QI(K)*DZ*1000.                                            MM5ATM.194
            PLWP(K)=(RO*QR(K))**0.75*DZ*1000.                                    MM5ATM.195
            PIWP(K)=(RO*QNI(K))**0.75*DZ*1000.                                   MM5ATM.196
          ENDIF                                                                  MM5ATM.197
                                                                                 MM5ATM.198
          O3(K)=O3PROF(I,K)                                                      MM5ATM.199
          N2O(K)=0.                                                              MM5ATM.200
          CH4(K)=0.                                                              MM5ATM.201
                                                                                 MM5ATM.202
      ENDDO                                                                      MM5ATM.203
                                                                                 MM5ATM.204
C  LAYER INTERFACE VALUES                                                        MM5ATM.205
      DO K=2,MKX                                                                 MM5ATM.206
          PINT(K)=TWT(K,2)*P(K-1)+TWT(K,1)*P(K)                                  MM5ATM.207
          TINT(K)=TWT(K,2)*T(K-1)+TWT(K,1)*T(K)                                  MM5ATM.208
      ENDDO                                                                      MM5ATM.209
      TINT(KXP1)=TGA(I,J)                                                        MM5ATM.210
      PINT(KXP1)=10.*(PSAA+PTOP)+PP3D(I,J,MKX)*0.01                              MM5ATM.211
      PINT(1)=10.*PTOP+PP3D(I,J,1)*0.01                                          MM5ATM.212
                                                                                 MM5ATM.213
                                                                                 MM5ATM.214
C  Initialize all molecular amounts to zero here, then pass MM5 amounts          MM5ATM.215
C  into RRTM arrays WKL and WX below.                                            MM5ATM.216
                                                                                 MM5ATM.217
      DO 1000 ILAY = 1,MXLAY                                                     MM5ATM.218
         DO 1100 ISP = 1,35                                                      MM5ATM.219
 1100       WKL(ISP,ILAY) = 0.0                                                  MM5ATM.220
         DO 1200 ISP = 1,MAXXSEC                                                 MM5ATM.221
 1200       WX(ISP,ILAY) = 0.0                                                   MM5ATM.222
 1000 CONTINUE                                                                   MM5ATM.223
                                                                                 MM5ATM.224
C  Set parameters needed for RRTM execution:                                     MM5ATM.225
      IXSECT = 1                                                                 MM5ATM.226
      IXMAX = 4                                                                  MM5ATM.227
                                                                                 MM5ATM.228
C  Set surface temperature.  The longwave upward surface flux is                 MM5ATM.229
C  computed in the Land Surface Model based on the surface                       MM5ATM.230
C  temperature and the emissivity of the surface type for each                   MM5ATM.231
C  grid point.  The bottom interface temperature, tint(kxp1), is                 MM5ATM.232
C  ground temperature consistent with this LW upward flux, and                   MM5ATM.233
C  TBOUND is set to this temperature here.                                       MM5ATM.234
                                                                                 MM5ATM.235
      TBOUND = TINT(KXP1)                                                        MM5ATM.236
                                                                                 MM5ATM.237
C  Install MM5 profiles into RRTM arrays for pressure, temperature,              MM5ATM.238
C  and molecular amounts.  Pressures are converted from cb                       MM5ATM.239
C  (CCM) to mb (RRTM).  H2O and trace gas amounts are converted from             MM5ATM.240
C  mass mixing ratio to volume mixing ratio.  CO2 vmr is constant at all         MM5ATM.241
C  levels.  The dry air column COLDRY (in molec/cm2) is calculated               MM5ATM.242
C  from the level pressures PZ (in mb) based on the hydrostatic equation         MM5ATM.243
C  and includes a correction to account for H2O in the layer.  The               MM5ATM.244
C  molecular weight of moist air (amm) is calculated for each layer.             MM5ATM.245
                                                                                 MM5ATM.246
C  RRTM is executed for an additional layer (L=KXP1), which extends              MM5ATM.247
C  from the model top (ptop) to 0 mb, to calculate the downward                  MM5ATM.248
C  flux at the model top interface.  H2O, CO2, and O3 vmrs for this              MM5ATM.249
C  extra layer are set to the values in the model's top layer, though            MM5ATM.250
C  the O3 value is reduced by a fraction (0.6) based on the US Std Atm.          MM5ATM.251
C  For GCMs with a model top near 0 mb, this extra layer is not needed, and      MM5ATM.252
C  NLAYERS should be set to the number of model layers (mkx in this case).       MM5ATM.253
C  Note: RRTM levels count from bottom to top, while MM5 levels count            MM5ATM.254
C  from the top down and must be reversed here.                                  MM5ATM.255
                                                                                 MM5ATM.256
      NLAYERS = KXP1                                                             MM5ATM.257
      NMOL = 6                                                                   MM5ATM.258
      PZ(0) = pint(kxp1)                                                         MM5ATM.259
      TZ(0) = tint(kxp1)                                                         MM5ATM.260
      DO 2000 L = 1, NLAYERS-1                                                   MM5ATM.261
         PAVEL(L) = p(mkx+1-L)                                                   MM5ATM.262
         TAVEL(L) = t(mkx+1-L)                                                   MM5ATM.263
         PZ(L) = pint(kxp1-L)                                                    MM5ATM.264
         TZ(L) = tint(kxp1-L)                                                    MM5ATM.265
         WKL(1,L) = qv(mkx+1-L)*amdw                                             MM5ATM.266
         WKL(2,L) = co2vmr                                                       MM5ATM.267
         WKL(3,L) = o3(mkx+1-L)                                                  MM5ATM.268
         WKL(4,L) = n2o(mkx+1-L)*amdn                                            MM5ATM.269
         WKL(6,L) = ch4(mkx+1-L)*amdc                                            MM5ATM.270
         amm = (1-WKL(1,L))*amd + WKL(1,L)*amw                                   MM5ATM.271
         COLDRY(L) = (PZ(L-1)-PZ(L))*1.E3*avgdro/                                MM5ATM.272
     1                         (gravit*amm*(1+WKL(1,L)))                         MM5ATM.273
 2000    CONTINUE                                                                MM5ATM.274
                                                                                 MM5ATM.275
C  Set cross section molecule amounts from CCM; convert to vmr                   MM5ATM.276
      DO 2100 L=1, NLAYERS-1                                                     MM5ATM.277
C        WX(2,L) = c11mmr(mkx+1-L)*amdc1                                         MM5ATM.278
C        WX(3,L) = c12mmr(mkx+1-L)*amdc2                                         MM5ATM.279
         WX(2,L) = 0.                                                            MM5ATM.280
         WX(3,L) = 0.                                                            MM5ATM.281
 2100 CONTINUE                                                                   MM5ATM.282
                                                                                 MM5ATM.283
C  *****                                                                         MM5ATM.284
C  Set up values for extra layer at top of the atmosphere.                       MM5ATM.285
C  The top layer temperature for all gridpoints is set to the top layer-1        MM5ATM.286
C  temperature plus a constant (0 K) that represents an isothermal layer         MM5ATM.287
C  above ptop.  Top layer interface temperatures are                             MM5ATM.288
C  linearly interpolated from the layer temperatures.                            MM5ATM.289
C  Note: The top layer temperature and ozone amount are based on a 0-3mb         MM5ATM.290
C  top layer and must be modified if the layering is changed.                    MM5ATM.291
C  This section should be commented if the extra layer is not needed.            MM5ATM.292
                                                                                 MM5ATM.293
      PAVEL(NLAYERS) = 0.5*PZ(NLAYERS-1)                                         MM5ATM.294
      TAVEL(NLAYERS) = TAVEL(NLAYERS-1) + 0.0                                    MM5ATM.295
      PZ(NLAYERS) = 0.00                                                         MM5ATM.296
      TZ(NLAYERS-1) = 0.5*(TAVEL(NLAYERS)+TAVEL(NLAYERS-1))                      MM5ATM.297
      TZ(NLAYERS) = TZ(NLAYERS-1)+0.0                                            MM5ATM.298
      WKL(1,NLAYERS) = WKL(1,NLAYERS-1)                                          MM5ATM.299
      WKL(2,NLAYERS) = co2vmr                                                    MM5ATM.300
      WKL(3,NLAYERS) = 0.6*WKL(3,NLAYERS-1)                                      MM5ATM.301
      WKL(4,NLAYERS) = WKL(4,NLAYERS-1)                                          MM5ATM.302
      WKL(6,NLAYERS) = WKL(6,NLAYERS-1)                                          MM5ATM.303
      amm = (1-WKL(1,NLAYERS-1))*amd + WKL(1,NLAYERS-1)*amw                      MM5ATM.304
      COLDRY(NLAYERS) = (PZ(NLAYERS-1))*1.E3*avgdro/                             MM5ATM.305
     1                         (gravit*amm*(1+WKL(1,NLAYERS-1)))                 MM5ATM.306
      WX(2,NLAYERS) = WX(2,NLAYERS-1)                                            MM5ATM.307
      WX(3,NLAYERS) = WX(3,NLAYERS-1)                                            MM5ATM.308
C  *****                                                                         MM5ATM.309
                                                                                 MM5ATM.310
C  Here, all molecules in WKL and WX are in volume mixing ratio; convert to      MM5ATM.311
C  molec/cm2 based on COLDRY for use in RRTM                                     MM5ATM.312
                                                                                 MM5ATM.313
      DO 5000 L = 1, NLAYERS                                                     MM5ATM.314
         DO 4200 IMOL = 1, NMOL                                                  MM5ATM.315
            WKL(IMOL,L) = COLDRY(L) * WKL(IMOL,L)                                MM5ATM.316
 4200    CONTINUE                                                                MM5ATM.317
         DO 4400 IX = 1,MAXXSEC                                                  MM5ATM.318
            IF (IXINDX(IX) .NE. 0) THEN                                          MM5ATM.319
               WX(IXINDX(IX),L) = COLDRY(L) * WX(IX,L) * 1.E-20                  MM5ATM.320
            ENDIF                                                                MM5ATM.321
 4400    CONTINUE                                                                MM5ATM.322
 5000 CONTINUE                                                                   MM5ATM.323
                                                                                 MM5ATM.324
C  Set spectral surface emissivity for each longwave band.  The default value    MM5ATM.325
C  is set here to emiss(i,j) based on land-use (taken to be constant across band MM5ATM.326
C  Comment: if land-surface uses skin temperature, emissivity must match that    MM5ATM.327
C   used in its calculation (e.g. 1.0)                                           MM5ATM.328
      DO 5500 N=1,NBANDS                                                         MM5ATM.329
         SEMISS(N) = EMISS(I,J)                                                  MM5ATM.330
 5500 CONTINUE                                                                   MM5ATM.331
                                                                                 MM5ATM.332
C  Transfer cloud fraction to RRTM array; compute cloud optical depth, TAUCLOUD, MM5ATM.333
C  as the product of clwp and cloud mass absorption coefficient in MM5, which is MM5ATM.334
C  a  combination of liquid and ice absorption coefficients.                     MM5ATM.335
C  Note: RRTM levels count from bottom to top, while CCM levels count from the   MM5ATM.336
C  top down and must be reversed here.  Values for the extra RRTM level (above   MM5ATM.337
C  the model top) are set to zero.                                               MM5ATM.338
                                                                                 MM5ATM.339
C     if(i.eq.26.and.j.eq.63)print *,'sounding p t q taucloud'                   MM5ATM.340
C     if(i.eq.26.and.j.eq.63)print *,0,pz(0),tz(0)                               MM5ATM.341
      DO 7000 L = 1, NLAYERS-1                                                   MM5ATM.342
         TAUCLOUD(L) = ABCW*CLWP(kxp1-L)+ABICE*CIWP(kxp1-L)                      MM5ATM.343
     -                +ABRN*PLWP(kxp1-L)+ABSN*PIWP(kxp1-L)                       MM5ATM.344
         IF(TAUCLOUD(L).GT.0.01)CLDFRC(kxp1-L)=1.                                MM5ATM.345
         CLDFRAC(L) = cldfrc(kxp1-L)                                             MM5ATM.346
C        if(i.eq.26.and.j.eq.63)print *,l,p(kxp1-L),t(kxp1-L),                   MM5ATM.347
C    -     qv(kxp1-L),taucloud(l)                                                MM5ATM.348
 7000 CONTINUE                                                                   MM5ATM.349
      CLDFRAC(NLAYERS) = 0.0                                                     MM5ATM.350
      TAUCLOUD(NLAYERS) = 0.0                                                    MM5ATM.351
                                                                                 MM5ATM.352
      RETURN                                                                     MM5ATM.353
      END                                                                        MM5ATM.354
