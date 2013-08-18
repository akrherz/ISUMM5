                                                                                 RADABS.1
C-----------------------------------------------------------------------         RADABS.2
C-----------------------------------------------------------------------         RADABS.3
      SUBROUTINE RADABS(PBR    ,PNM    ,CO2EM ,CO2EML ,TPLNKA,                   RADABS.4
     $                  S2C    ,S2T    ,W     ,H2OTR  ,PLCO2 ,                   RADABS.5
     $                  PLH2O  ,CO2T   ,TINT  ,TLAYR  ,PLOL  ,                   RADABS.6
     $                  PLOS   ,PMLN   ,PILN  ,ABSTOT ,ABSNXT)                   RADABS.7
C                                                                                RADABS.8
C COMPUTE ABSORPTIVITIES FOR H2O, CO2, AND O3                                    RADABS.9
C                                                                                RADABS.10
C                                                                                RADABS.11
C H2O  ....  USES NONISOTHERMAL EMISSIVITY FOR WATER VAPOR FROM                  RADABS.12
C            RAMANATHAN, V. AND  P.DOWNEY, 1986: A NONISOTHERMAL                 RADABS.13
C            EMISSIVITY AND ABSORPTIVITY FORMULATION FOR WATER VAPOR             RADABS.14
C            JOURANL OF GEOPHYSICAL RESEARCH, VOL. 91., D8, PP 8649-8666         RADABS.15
C                                                                                RADABS.16
C                                                                                RADABS.17
C CO2  ....  USES ABSORPTANCE PARAMETERIZATION OF THE 15 MICRO-METER             RADABS.18
C            (500 - 800 CM-1) BAND SYSTEM OF CARBON DIOXIDE, FROM                RADABS.19
C            KIEHL, J.T. AND B.P.BRIEGLEB, 1991: A NEW PARAMETERIZATION          RADABS.20
C            OF THE ABSORPTANCE DUE TO THE 15 MICRO-METER BAND SYSTEM            RADABS.21
C            OF CARBON DIOXIDE JOURANL OF GEOPHYSICAL RESEARCH,                  RADABS.22
C            VOL. 96., D5, PP 9013-9019                                          RADABS.23
C                                                                                RADABS.24
C O3   ....  USES ABSORPTANCE PARAMETERIZATION OF THE 9.6 MICRO-METER            RADABS.25
C            BAND SYSTEM OF OZONE, FROM RAMANATHAN, V. AND R.E.DICKINSON         RADABS.26
C            1979: THE ROLE OF STRATOSPHERIC OZONE IN THE ZONAL AND              RADABS.27
C            SEASONAL RADIATIVE ENERGY BALANCE OF THE EARTH-TROPOSPHERE          RADABS.28
C            SYSTEM. JOURNAL OF THE ATMOSPHERIC SCIENCES, VOL. 36,               RADABS.29
C            PP 1084-1104                                                        RADABS.30
C                                                                                RADABS.31
C                                                                                RADABS.32
C COMPUTES INDIVIDUAL ABSORPTIVITIES FOR NON-ADJACENT LAYERS, ACCOUNTING         RADABS.33
C FOR BAND OVERLAP, AND SUMS TO OBTAIN THE TOTAL; THEN, COMPUTES THE             RADABS.34
C NEAREST LAYER CONTRIBUTION.                                                    RADABS.35
C                                                                                RADABS.36
C                                                                                RADABS.37
C     INCLUDE 'PARAME'                                                           RADABS.38
C     INCLUDE 'PARAM1.CB'                                                        RADABS.39
C     INCLUDE 'RADCCM2.DATA'                                                     RADABS.40
#     include <parame.incl>                                                      RADABS.41
#     include <parccm2.incl>                                                     RADABS.42
#     include <radccm2.incl>                                                     RADABS.43
#ifdef MPP1                                                                      RADABS.44
#     include <various.incl>                                                     RADABS.45
#endif                                                                           RADABS.46
C-----------------------------------------------------------------------         RADABS.47
C                                                                                RADABS.48
C WATER VAPOR NARROW BAND CONSTANTS FOR LW COMPUTATIONS                          RADABS.49
C                                                                                RADABS.50
      REAL REALK,ST,A1,A2,B1,B2,                                                 RADABS.51
     $     COEFA,COEFB,COEFC,COEFD,                                              RADABS.52
     $     COEFE,COEFF,COEFG,COEFH,                                              RADABS.53
     $     COEFI,COEFJ,COEFK,                                                    RADABS.54
     $     C1,C2,C3,C4,C5,C6,C7,                                                 RADABS.55
     $     C8 ,C9 ,C10,C11,C12,C13,C14,C15,C16,C17,                              RADABS.56
     $     C18,C19,C20,C21,C22,C23,C24,C25,C26,C27,                              RADABS.57
     $     C28,C29,C30,C31,                                                      RADABS.58
     $     FWCOEF,FWC1,FWC2,FC1,CFA1                                             RADABS.59
C                                                                                RADABS.60
      COMMON/CRDCAE/REALK(2), ST(2), A1(2), A2(2), B1(2), B2(2),                 RADABS.61
C                                                                                RADABS.62
C CONSTANT COEFFICIENTS FOR WATER VAPOR ABSORPTIVITY AND EMISSIVI                RADABS.63
C                                                                                RADABS.64
     $              COEFA(3,4),COEFB(4,4),COEFC(3,4),COEFD(4,4),                 RADABS.65
     $              COEFE(3,4),COEFF(6,2),COEFG(2,4),COEFH(2,4),                 RADABS.66
     $              COEFI(6,2),COEFJ(3,2),COEFK(3,2),                            RADABS.67
     $              C1(4),C2(4),C3(4),C4(4),C5(4),C6(4),C7(4),                   RADABS.68
     $              C8 ,C9 ,C10,C11,C12,C13,C14,C15,C16,C17,                     RADABS.69
     $              C18,C19,C20,C21,C22,C23,C24,C25,C26,C27,                     RADABS.70
     $              C28,C29,C30,C31,                                             RADABS.71
C                                                                                RADABS.72
C FARWING CORRECTION CONSTANTS FOR NARROW-BAND EMISSIVITY MODEL                  RADABS.73
C INTRODUCE FARWING CORRECTION TO ACCOUNT FOR THE                                RADABS.74
C DEFICIENCIES IN NARROW-BAND MODEL USED TO DERIVE THE                           RADABS.75
C EMISSIVITY. TUNED WITH ARKINGS LINE CALCULATIONS.                              RADABS.76
C                                                                                RADABS.77
     $              FWCOEF,FWC1,FWC2,FC1,CFA1                                    RADABS.78
C                                                                                RADABS.79
C                                                                                RADABS.80
C                                                                                RADABS.81
C INPUT ARGUMENTS                                                                RADABS.82
C                                                                                RADABS.83
      REAL PBR(IPLOND,IPLEV),           ! PRSSR AT MID-LEVELS (DYNES/CM2         RADABS.84
     $     PNM(IPLOND,IPLEVP),          ! PRSSR AT INTERFACES (DYNES/CM2         RADABS.85
     $     CO2EM(IPLOND,IPLEVP),        ! CO2 EMISSIVITY FUNCTION                RADABS.86
     $     CO2EML(IPLOND,IPLEV),        ! CO2 EMISSIVITY FUNCTION                RADABS.87
     $     TPLNKA(IPLOND,IPLEVP),       ! PLANCK FNCTN LEVEL TEMPERATURE         RADABS.88
     $     S2C(IPLOND,IPLEVP),          ! H2O CONTINUUM PATH LENGTH              RADABS.89
     $     S2T(IPLOND,IPLEVP),          ! H2O TMP AND PRS WGHTED PATH            RADABS.90
     $     W(IPLOND,IPLEVP),            ! H2O PRS WGHTED PATH                    RADABS.91
     $     H2OTR(IPLOND,IPLEVP),        ! H2O TRNSMSSN FNCTN FOR O3 OVER         RADABS.92
     $     PLCO2(IPLOND,IPLEVP),        ! CO2 PRS WGHTED PATH LENGTH             RADABS.93
     $     PLH2O(IPLOND,IPLEVP),        ! H2O PRS WFHTED PATH LENGTH             RADABS.94
     $     CO2T(IPLOND,IPLEVP),         ! TMP AND PRS WGHTED PATH LENGTH         RADABS.95
     $     TINT(IPLOND,IPLEVP),         ! INTERFACE TEMPERATURES                 RADABS.96
     $     TLAYR(IPLOND,IPLEVP),        ! K-1 LEVEL TEMPERATURES                 RADABS.97
     $     PLOL(IPLOND,IPLEVP),         ! OZONE PRS WGHTED PATH LENGTH           RADABS.98
     $     PLOS(IPLOND,IPLEVP)          ! OZONE PATH LENGTH                      RADABS.99
      REAL PMLN(IPLOND,IPLEV),          ! LN(PMIDM1)                             RADABS.100
     $     PILN(IPLOND,IPLEVP)          ! LN(PINTM1)                             RADABS.101
C                                                                                RADABS.102
C OUTPUT ARGUMENTS                                                               RADABS.103
C                                                                                RADABS.104
      REAL ABSTOT(IPLOND,IPLEVP,IPLEVP), ! TOTAL ABSORPTIVITY                    RADABS.105
     $     ABSNXT(IPLOND,IPLEV,4)       ! TOTAL NEAREST LAYER ABSORPTIVI         RADABS.106
C                                                                                RADABS.107
C LOCAL WORKSPACE                                                                RADABS.108
C                                                                                RADABS.109
      INTEGER       I,   ! LOGITUDE INDEX                                        RADABS.110
     $              K,   ! LEVEL INDEX                                           RADABS.111
     $             K1,   ! LEVEL INDEX                                           RADABS.112
     $             K2,   ! LEVEL INDEX                                           RADABS.113
     $             KN,   ! NEAREST LEVEL INDEX                                   RADABS.114
     $          IBAND    ! BAND  INDEX                                           RADABS.115
C                                                                                RADABS.116
      REAL PNEW(IPLOND),     !                                                   RADABS.117
     $     TRLINE(IPLOND,2), !                                                   RADABS.118
     $     U(IPLOND),        !                                                   RADABS.119
     $     TBAR(IPLOND,4),   !                                                   RADABS.120
     $     EMM(IPLOND,4),    !                                                   RADABS.121
     $     O3EMM(IPLOND,4),  !                                                   RADABS.122
     $     O3BNDI,          ! OZONE BAND PARAMETER                               RADABS.123
     $     TEMH2O(IPLOND,4), !                                                   RADABS.124
     $     K21,             ! EXPONENTIAL COEFFICIENT USED TO CALCULATE          RADABS.125
C                           ! ROTATION BAND TRANSMISSIVITY IN THE 650-80         RADABS.126
C                           ! CM-1 REGION (TR1)                                  RADABS.127
     $     K22,             ! EXPONENTIAL COEFFICIENT USED TO CALCULATE          RADABS.128
C                           ! ROTATION BAND TRANSMISSIVITY IN THE 500-65         RADABS.129
C                           !  CM-1 REGION (TR2)                                 RADABS.130
     $     UC1(IPLOND)       !                                                   RADABS.131
      REAL TO3H2O(IPLOND),   ! H2O TRNSMSN FOR OVERLAP WITH O3                   RADABS.132
     $     PI,              ! FOR CO2 ABSORPTIVITY COMPUTATION                   RADABS.133
     $     SQTI(IPLOND),     ! " "                                               RADABS.134
     $     ET,              ! " "                                                RADABS.135
     $     ET2,             ! " "                                                RADABS.136
     $     ET4,             ! " "                                                RADABS.137
     $     OMET,            ! " "                                                RADABS.138
     $     F1CO2,           ! " "                                                RADABS.139
     $     F2CO2(IPLOND),    ! " "                                               RADABS.140
     $     F3CO2(IPLOND),    ! " "                                               RADABS.141
     $     T1CO2(IPLOND),    ! " "                                               RADABS.142
     $     SQWP,            ! " "                                                RADABS.143
     $     F1SQWP(IPLOND)    ! " "                                               RADABS.144
      REAL ONEME,           ! " "                                                RADABS.145
     $     ALPHAT,          ! " "                                                RADABS.146
     $     WCO2,            ! " "                                                RADABS.147
     $     POSQT,           ! " "                                                RADABS.148
     $     U7,              ! " "                                                RADABS.149
     $     U8,              ! " "                                                RADABS.150
     $     U9,              ! " "                                                RADABS.151
     $     U13,             ! " "                                                RADABS.152
     $     RBETA7,          ! " "                                                RADABS.153
     $     RBETA8,          ! " "                                                RADABS.154
     $     RBETA9,          ! " "                                                RADABS.155
     $     RBETA13          ! " "                                                RADABS.156
      REAL TPATHA(IPLOND),     ! FOR ABSORPTIVITY COMPUTATION                    RADABS.157
     $     A,                 ! " "                                              RADABS.158
     $     ABSO(IPLOND,6),     ! ABSORPTIVITY FOR VARIOUS GASES/BANDS            RADABS.159
     $     DTP(IPLOND),        ! " "                                             RADABS.160
     $     DTX(IPLOND),        ! " "                                             RADABS.161
     $     DTY(IPLOND),        ! " "                                             RADABS.162
     $     DTZ(IPLOND),        ! " "                                             RADABS.163
     $     TERM1(IPLOND,4),    ! " "                                             RADABS.164
     $     TERM2(IPLOND,4)     ! " "                                             RADABS.165
      REAL TERM3(IPLOND,4),    ! " "                                             RADABS.166
     $     TERM4(IPLOND,4),    ! " "                                             RADABS.167
     $     TERM5(IPLOND,4),    ! " "                                             RADABS.168
     $     TERM6(IPLOND,IPLEVP),! " "                                            RADABS.169
     $     TERM7(IPLOND,2),    ! " "                                             RADABS.170
     $     TERM8(IPLOND,2),    ! " "                                             RADABS.171
     $     TERM9(IPLOND,IPLEVP),! " "                                            RADABS.172
     $     TR1,               ! " "                                              RADABS.173
     $     TR10(IPLOND),       ! " "                                             RADABS.174
     $     TR2                ! " "                                              RADABS.175
      REAL TR5,               ! " "                                              RADABS.176
     $     TR6,               ! " "                                              RADABS.177
     $     TR9(IPLOND),        ! " "                                             RADABS.178
     $     UC(IPLOND)          ! " "                                             RADABS.179
      REAL SQRTU(IPLOND),      ! " "                                             RADABS.180
     $     FWK(IPLOND),        ! " "                                             RADABS.181
     $     FWKU(IPLOND),       ! " "                                             RADABS.182
     $     R2ST(2),           ! " "                                              RADABS.183
     $     DTYP15(IPLOND),     ! " "                                             RADABS.184
     $     DTYP15SQ(IPLOND),   ! " "                                             RADABS.185
     $     TO3CO2(IPLOND),     ! " "                                             RADABS.186
     $     DPNM(IPLOND),       ! " "                                             RADABS.187
     $     PNMSQ(IPLOND,IPLEVP),! " "                                            RADABS.188
     $     DW(IPLOND),         ! " "                                             RADABS.189
     $     UINPL(IPLOND,4),    ! " "                                             RADABS.190
     $     WINPL(IPLOND,4),    ! " "                                             RADABS.191
     $     ZINPL(IPLOND,4),    ! " "                                             RADABS.192
     $     PINPL(IPLOND,4),    ! " "                                             RADABS.193
     $     DPLH2O(IPLOND)      ! " "                                             RADABS.194
      REAL            R80257, ! " "                                              RADABS.195
     $                  R293, ! " "                                              RADABS.196
     $                  R250, ! " "                                              RADABS.197
     $                 R3205, ! " "                                              RADABS.198
     $                  R300, ! " "                                              RADABS.199
     $                 RSSLP, ! " "                                              RADABS.200
     $                R2SSLP  ! " "                                              RADABS.201
C                                                                                RADABS.202
      REAL  DS2C,     ! USED IN ABSORPTIVITY COMPUTATION                         RADABS.203
     $      A11,      ! " "                                                      RADABS.204
     $      A31,      ! " "                                                      RADABS.205
     $      A21,      ! " "                                                      RADABS.206
     $      A22,      ! " "                                                      RADABS.207
     $      A23,      ! " "                                                      RADABS.208
     $      T1T4,     ! " "                                                      RADABS.209
     $      T2T5,     ! " "                                                      RADABS.210
     $      RSUM,     ! " "                                                      RADABS.211
     $      A41,      ! " "                                                      RADABS.212
     $      A51,      ! " "                                                      RADABS.213
     $      A61       ! " "                                                      RADABS.214
C                                                                                RADABS.215
      REAL  PHI,      ! " "                                                      RADABS.216
     $      PSI,      ! " "                                                      RADABS.217
     $      CF812,    ! " "                                                      RADABS.218
     $      UBAR,     ! " "                                                      RADABS.219
     $      PBAR,     ! " "                                                      RADABS.220
     $      G4        ! " "                                                      RADABS.221
C                                                                                RADABS.222
      REAL  DPLOS,    ! USED IN ABSORPTIVITY COMPUTATION                         RADABS.223
     $      DPLOL,    ! " "                                                      RADABS.224
     $      TLOCAL,   ! " "                                                      RADABS.225
     $      BETA,     ! " "                                                      RADABS.226
     $      RPHAT,    ! " "                                                      RADABS.227
     $      TCRFAC,   ! " "                                                      RADABS.228
     $      TMP1,     ! " "                                                      RADABS.229
     $      U1,       ! " "                                                      RADABS.230
     $      REALNU,   ! " "                                                      RADABS.231
     $      TMP2,     ! " "                                                      RADABS.232
     $      U2,       ! " "                                                      RADABS.233
     $      RSQTI     ! " "                                                      RADABS.234
C                                                                                RADABS.235
      REAL  TPATH,    ! " "                                                      RADABS.236
     $      TMP3,     ! " "                                                      RADABS.237
     $      RDPNMSQ,  ! " "                                                      RADABS.238
     $      RDPNM,    ! " "                                                      RADABS.239
     $      P1,       ! " "                                                      RADABS.240
     $      P2,       ! " "                                                      RADABS.241
     $      DTYM10,   ! " "                                                      RADABS.242
     $      DPLCO2,   ! " "                                                      RADABS.243
     $      CORFAC,   ! " "                                                      RADABS.244
     $      G2,       ! " "                                                      RADABS.245
     $      TE,       ! " "                                                      RADABS.246
     $      DENOM     ! " "                                                      RADABS.247
C                                                                                RADABS.248
C TRANSMISSION TERMS FOR VARIOUS SPECTRAL INTERVALS:                             RADABS.249
C                                                                                RADABS.250
      REAL    TRAB1(IPLOND),  ! H2O     0 -  800 CM-1                            RADABS.251
     $        TRAB2(IPLOND),  ! H2O   500 -  800 CM-1                            RADABS.252
     $        TRAB3(IPLOND),  ! CO2   500 -  800 CM-1                            RADABS.253
     $        TRAB4(IPLOND),  ! H2O   800 - 1000 CM-1                            RADABS.254
     $        TRAB5(IPLOND),  ! O3     9.6 MICRO-METER BAND                      RADABS.255
     $        TRAB6(IPLOND),  ! H2O  1000 - 1200 CM-1                            RADABS.256
     $        TRAB7(IPLOND)   ! H2O  1200 - 2200 CM-1                            RADABS.257
C                                                                                RADABS.258
      REAL BNDFCT, ! BAND ABSORPTANCE PARAMETER FOR CO2                          RADABS.259
     $     ABSBND  ! PROPORTIONAL TO CO2 BAND ABSORPTANCE                        RADABS.260
C                                                                                RADABS.261
      REAL DBVTIT(IPLOND,IPLEVP),      ! INTRFC DRVTV PLNCK FNCTN FOR O3         RADABS.262
     $     DBVTLY(IPLOND,IPLEV)        ! LEVEL DRVTV PLNCK FNCTN FOR O3          RADABS.263
C                                                                                RADABS.264
      REAL DBVT,T     ! PLANCK FNCTN TMP DERIVATIVE FOR O3                       RADABS.265
C                                                                                RADABS.266
      DBVT(T)=(-2.8911366682E-4+(2.3771251896E-6+1.1305188929E-10*T)*T)/         RADABS.267
     $  (1.0+(-6.1364820707E-3+1.5550319767E-5*T)*T)                             RADABS.268
                                                                                 RADABS.269
#ifdef MPP1                                                                      RADABS.270
#  define IPLON I  LX                                                            RADABS.271
#  define IPLOND I  LX                                                           RADABS.272
#endif                                                                           RADABS.273
                                                                                 RADABS.274
C                                                                                RADABS.275
C                                                                                RADABS.276
C INITIALIZE                                                                     RADABS.277
C                                                                                RADABS.278
      DO 2 K1=1,IPLEVP                                                           RADABS.279
        DO 3 I=1,IPLON                                                           RADABS.280
          DBVTIT(I,K1)=DBVT(TINT(I,K1))                                          RADABS.281
    3   CONTINUE                                                                 RADABS.282
    2 CONTINUE                                                                   RADABS.283
      DO 4 K=1,IPLEV                                                             RADABS.284
        DO 5 I=1,IPLON                                                           RADABS.285
          DBVTLY(I,K)=DBVT(TLAYR(I,K+1))                                         RADABS.286
    5   CONTINUE                                                                 RADABS.287
    4 CONTINUE                                                                   RADABS.288
C                                                                                RADABS.289
      R80257=1./8.0257E-04                                                       RADABS.290
      R293=1./293.                                                               RADABS.291
      R250=1./250.                                                               RADABS.292
      R3205=1./.3205                                                             RADABS.293
      R300=1./300.                                                               RADABS.294
      RSSLP=1./SSLP                                                              RADABS.295
      R2SSLP=1./(2.*SSLP)                                                        RADABS.296
      R2ST(1)=1./(2.*ST(1))                                                      RADABS.297
      R2ST(2)=1./(2.*ST(2))                                                      RADABS.298
      BNDFCT=2.0*22.18/(SQRT(196.)*300.)                                         RADABS.299
C                                                                                RADABS.300
C NON-ADJACENT LAYER ABSORPTIVITY:                                               RADABS.301
C                                                                                RADABS.302
C ABSO(I,1)     0 -  800 CM-1   H2O ROTATION BAND                                RADABS.303
C ABSO(I,2)  1200 - 2200 CM-1   H2O VIBRATION-ROTATION BAND                      RADABS.304
C ABSO(I,3)   800 - 1200 CM-1   H2O WINDOW                                       RADABS.305
C ABSO(I,4)   500 -  800 CM-1   H2O ROTATION BAND OVERLAP WITH CO2               RADABS.306
C ABSO(I,5)   ?  CM-1   O3  9.6 MICRO-METER BAND                                 RADABS.307
C ABSO(I,6)   500 -  800 CM-1   CO2 15  MICRO-METER BAND                         RADABS.308
C                                                                                RADABS.309
      DO 8 K=1,IPLEVP                                                            RADABS.310
        DO 9 I=1,IPLON                                                           RADABS.311
          PNMSQ(I,K)=PNM(I,K)**2                                                 RADABS.312
          DTX(I)=TPLNKA(I,K)-250.                                                RADABS.313
          TERM6(I,K)=COEFF(1,2)+COEFF(2,2)*DTX(I)*(1.+C9*DTX(I)*(1.+C11*         RADABS.314
     +               DTX(I)*(1.+C13*DTX(I)*(1.+C15*DTX(I)))))                    RADABS.315
          TERM9(I,K)=COEFI(1,2)+COEFI(2,2)*DTX(I)*(1.+C19*DTX(I)*(1.+C21         RADABS.316
     +               *DTX(I)*(1.+C23*DTX(I)*(1.+C25*DTX(I)))))                   RADABS.317
    9   CONTINUE                                                                 RADABS.318
    8 CONTINUE                                                                   RADABS.319
C                                                                                RADABS.320
C BEGIN NON-NEAREST LAYER LEVEL LOOPS                                            RADABS.321
C                                                                                RADABS.322
      DO 180 K1=IPLEVP,1,-1                                                      RADABS.323
        DO 170 K2=IPLEVP,1,-1                                                    RADABS.324
          DO 10 I=1,IPLON                                                        RADABS.325
            DPLH2O(I)=PLH2O(I,K1)-PLH2O(I,K2)                                    RADABS.326
            U(I)=ABS(DPLH2O(I))                                                  RADABS.327
            SQRTU(I)=SQRT(U(I))                                                  RADABS.328
            DS2C=ABS(S2C(I,K1)-S2C(I,K2))                                        RADABS.329
            DW(I)=ABS(W(I,K1)-W(I,K2))                                           RADABS.330
            UC1(I)=(DS2C+1.7E-3*U(I))*(1.+2.*DS2C)/(1.+15.*DS2C)                 RADABS.331
            UC(I)=DS2C+2.E-3*U(I)                                                RADABS.332
   10     CONTINUE                                                               RADABS.333
          IF(K1.EQ.K2)THEN                                                       RADABS.334
            DO 20 I=1,IPLON                                                      RADABS.335
              PNEW(I)=0.5                                                        RADABS.336
              TPATHA(I)=S2T(I,K1)-S2T(I,K2)                                      RADABS.337
   20       CONTINUE                                                             RADABS.338
          ELSE                                                                   RADABS.339
! K1.NE.K2                                                                       RADABS.340
            DO 30 I=1,IPLON                                                      RADABS.341
              PNEW(I)=U(I)/DW(I)                                                 RADABS.342
              TPATHA(I)=(S2T(I,K1)-S2T(I,K2))/DPLH2O(I)                          RADABS.343
   30       CONTINUE                                                             RADABS.344
          ENDIF                                                                  RADABS.345
          DO 40 I=1,IPLON                                                        RADABS.346
            DTX(I)=TPLNKA(I,K2)-250.                                             RADABS.347
            DTY(I)=TPATHA(I)-250.                                                RADABS.348
            DTYP15(I)=DTY(I)+15.                                                 RADABS.349
            DTYP15SQ(I)=DTYP15(I)**2                                             RADABS.350
            DTZ(I)=DTX(I)-50.                                                    RADABS.351
            DTP(I)=DTY(I)-50.                                                    RADABS.352
   40     CONTINUE                                                               RADABS.353
          DO 60 IBAND=2,4,2                                                      RADABS.354
            DO 50 I=1,IPLON                                                      RADABS.355
              TERM1(I,IBAND)=COEFE(1,IBAND)+COEFE(2,IBAND)*DTX(I)*(1.+           RADABS.356
     +                       C1(IBAND)*DTX(I))                                   RADABS.357
              TERM2(I,IBAND)=COEFB(1,IBAND)+COEFB(2,IBAND)*DTX(I)*(1.+           RADABS.358
     +                       C2(IBAND)*DTX(I)*(1.+C3(IBAND)*DTX(I)))             RADABS.359
              TERM3(I,IBAND)=COEFD(1,IBAND)+COEFD(2,IBAND)*DTX(I)*(1.+           RADABS.360
     +                       C4(IBAND)*DTX(I)*(1.+C5(IBAND)*DTX(I)))             RADABS.361
              TERM4(I,IBAND)=COEFA(1,IBAND)+COEFA(2,IBAND)*DTY(I)*(1.+           RADABS.362
     +                       C6(IBAND)*DTY(I))                                   RADABS.363
              TERM5(I,IBAND)=COEFC(1,IBAND)+COEFC(2,IBAND)*DTY(I)*(1.+           RADABS.364
     +                       C7(IBAND)*DTY(I))                                   RADABS.365
   50       CONTINUE                                                             RADABS.366
   60     CONTINUE                                                               RADABS.367
          DO 70 I=1,IPLON                                                        RADABS.368
C                                                                                RADABS.369
C ABSO(I,1)     0 -  800 CM-1   H2O ROTATION BAND                                RADABS.370
C                                                                                RADABS.371
            A11=0.44+3.380E-4*DTZ(I)-1.520E-6*DTZ(I)*DTZ(I)                      RADABS.372
            A31=1.05-6.000E-3*DTP(I)+3.000E-6*DTP(I)*DTP(I)                      RADABS.373
            A21=1.00+1.717E-3*DTZ(I)-1.133E-5*DTZ(I)*DTZ(I)                      RADABS.374
            A22=1.00+4.443E-3*DTP(I)+2.750E-5*DTP(I)*DTP(I)                      RADABS.375
            A23=1.00+3.600*SQRTU(I)                                              RADABS.376
            CORFAC=A31*(A11+((2.*A21*A22)/A23))                                  RADABS.377
            T1T4=TERM1(I,2)*TERM4(I,2)                                           RADABS.378
            T2T5=TERM2(I,2)*TERM5(I,2)                                           RADABS.379
            A=T1T4+T2T5/(1.+T2T5*SQRTU(I)*CORFAC)                                RADABS.380
            FWK(I)=FWCOEF+FWC1/(1.+FWC2*U(I))                                    RADABS.381
            FWKU(I)=FWK(I)*U(I)                                                  RADABS.382
            RSUM=EXP(-A*(SQRTU(I)+FWKU(I)))                                      RADABS.383
            ABSO(I,1)=(1.-RSUM)*TERM3(I,2)                                       RADABS.384
            TRAB1(I)=RSUM                                                        RADABS.385
   70     CONTINUE                                                               RADABS.386
          DO 72 I=1,IPLON                                                        RADABS.387
C                                                                                RADABS.388
C ABSO(I,2)  1200 - 2200 CM-1   H2O VIBRATION-ROTATION BAND                      RADABS.389
C                                                                                RADABS.390
            A41=1.75-3.960E-03*DTZ(I)                                            RADABS.391
            A51=1.00+1.3*SQRTU(I)                                                RADABS.392
            A61=1.00+1.250E-03*DTP(I)+6.250E-05*DTP(I)*DTP(I)                    RADABS.393
            CORFAC=.29*(1.+A41/A51)*A61                                          RADABS.394
            T1T4=TERM1(I,4)*TERM4(I,4)                                           RADABS.395
            T2T5=TERM2(I,4)*TERM5(I,4)                                           RADABS.396
            A=T1T4+T2T5/(1.+T2T5*SQRTU(I)*CORFAC)                                RADABS.397
            RSUM=EXP(-A*(SQRTU(I)+FWKU(I)))                                      RADABS.398
            ABSO(I,2)=(1.-RSUM)*TERM3(I,4)                                       RADABS.399
            TRAB7(I)=RSUM                                                        RADABS.400
   72     CONTINUE                                                               RADABS.401
C                                                                                RADABS.402
C LINE TRANSMISSION IN 800-1000 AND 1000-1200 CM-1 INTERVALS                     RADABS.403
C                                                                                RADABS.404
          DO 90 K=1,2                                                            RADABS.405
            DO 80 I=1,IPLON                                                      RADABS.406
              PHI=EXP(A1(K)*DTYP15(I)+A2(K)*DTYP15SQ(I))                         RADABS.407
              PSI=EXP(B1(K)*DTYP15(I)+B2(K)*DTYP15SQ(I))                         RADABS.408
              UBAR=DW(I)*PHI*1.66*R80257                                         RADABS.409
              PBAR=PNEW(I)*(PSI/PHI)                                             RADABS.410
              CF812=CFA1+(1.-CFA1)/(1.+UBAR*PBAR*10.)                            RADABS.411
              G2=1.+UBAR*4.0*ST(K)*CF812/PBAR                                    RADABS.412
              G4=REALK(K)*PBAR*R2ST(K)*(SQRT(G2)-1.)                             RADABS.413
              TRLINE(I,K)=EXP(-G4)                                               RADABS.414
   80       CONTINUE                                                             RADABS.415
   90     CONTINUE                                                               RADABS.416
          DO 100 I=1,IPLON                                                       RADABS.417
            TERM7(I,1)=COEFJ(1,1)+COEFJ(2,1)*DTY(I)*(1.+C16*DTY(I))              RADABS.418
            TERM8(I,1)=COEFK(1,1)+COEFK(2,1)*DTY(I)*(1.+C17*DTY(I))              RADABS.419
            TERM7(I,2)=COEFJ(1,2)+COEFJ(2,2)*DTY(I)*(1.+C26*DTY(I))              RADABS.420
            TERM8(I,2)=COEFK(1,2)+COEFK(2,2)*DTY(I)*(1.+C27*DTY(I))              RADABS.421
  100     CONTINUE                                                               RADABS.422
          DO 110 I=1,IPLON                                                       RADABS.423
C                                                                                RADABS.424
C ABSO(I,3)   800 - 1200 CM-1   H2O WINDOW                                       RADABS.425
C ABSO(I,4)   500 -  800 CM-1   H2O ROTATION BAND OVERLAP WITH CO2               RADABS.426
C                                                                                RADABS.427
            K21=TERM7(I,1)+TERM8(I,1)/(1.+(C30+C31*(DTY(I)-10.)*(DTY(I)-         RADABS.428
     +          10.))*SQRTU(I))                                                  RADABS.429
            K22=TERM7(I,2)+TERM8(I,2)/(1.+(C28+C29*(DTY(I)-10.))*                RADABS.430
     +          SQRTU(I))                                                        RADABS.431
            TR1=EXP(-(K21*(SQRTU(I)+FC1*FWKU(I))))                               RADABS.432
            TR2=EXP(-(K22*(SQRTU(I)+FC1*FWKU(I))))                               RADABS.433
            TR5=EXP(-((COEFH(1,3)+COEFH(2,3)*DTX(I))*UC1(I)))                    RADABS.434
            TR6=EXP(-((COEFH(1,4)+COEFH(2,4)*DTX(I))*UC1(I)))                    RADABS.435
            TR9(I)=TR1*TR5                                                       RADABS.436
            TR10(I)=TR2*TR6                                                      RADABS.437
            TRAB2(I)=0.65*TR9(I)+0.35*TR10(I)                                    RADABS.438
            TRAB4(I)=EXP(-(COEFG(1,3)+COEFG(2,3)*DTX(I))*UC(I))                  RADABS.439
            TRAB6(I)=EXP(-(COEFG(1,4)+COEFG(2,4)*DTX(I))*UC(I))                  RADABS.440
            ABSO(I,3)=TERM6(I,K2)*(1.-.5*TRAB4(I)*TRLINE(I,2)-.5*                RADABS.441
     +                TRAB6(I)*TRLINE(I,1))                                      RADABS.442
            ABSO(I,4)=TERM9(I,K2)*.5*(TR1-TR9(I)+TR2-TR10(I))                    RADABS.443
  110     CONTINUE                                                               RADABS.444
          IF(K1.NE.K2)THEN                                                       RADABS.445
            IF(K2.LT.K1)THEN                                                     RADABS.446
              DO 120 I=1,IPLON                                                   RADABS.447
                TO3H2O(I)=H2OTR(I,K1)/H2OTR(I,K2)                                RADABS.448
  120         CONTINUE                                                           RADABS.449
            ELSE                                                                 RADABS.450
              DO 130 I=1,IPLON                                                   RADABS.451
                TO3H2O(I)=H2OTR(I,K2)/H2OTR(I,K1)                                RADABS.452
  130         CONTINUE                                                           RADABS.453
            ENDIF                                                                RADABS.454
            DO 140 I=1,IPLON                                                     RADABS.455
C                                                                                RADABS.456
C ABSO(I,5)   ?  CM-1   O3  9.6 MICRO-METER BAND                                 RADABS.457
C                                                                                RADABS.458
              DPNM(I)=PNM(I,K1)-PNM(I,K2)                                        RADABS.459
              TO3CO2(I)=(PNM(I,K1)*CO2T(I,K1)-PNM(I,K2)*CO2T(I,K2))/             RADABS.460
     +                  DPNM(I)                                                  RADABS.461
              TE=(TO3CO2(I)*R293)**.7                                            RADABS.462
              DPLOS=PLOS(I,K1)-PLOS(I,K2)                                        RADABS.463
              DPLOL=PLOL(I,K1)-PLOL(I,K2)                                        RADABS.464
              U1=18.29*ABS(DPLOS)/TE                                             RADABS.465
              U2=.5649*ABS(DPLOS)/TE                                             RADABS.466
              RPHAT=DPLOL/DPLOS                                                  RADABS.467
              TLOCAL=TINT(I,K2)                                                  RADABS.468
              TCRFAC=SQRT(TLOCAL*R250)*TE                                        RADABS.469
              BETA=R3205*(RPHAT+DPFO3*TCRFAC)                                    RADABS.470
              REALNU=TE/BETA                                                     RADABS.471
              TMP1=U1/SQRT(4.+U1*(1.+REALNU))                                    RADABS.472
              TMP2=U2/SQRT(4.+U2*(1.+REALNU))                                    RADABS.473
              O3BNDI=74.*TE*ALOG(1.+TMP1+TMP2)                                   RADABS.474
              ABSO(I,5)=O3BNDI*TO3H2O(I)*DBVTIT(I,K2)                            RADABS.475
              TRAB5(I)=1.-(O3BNDI/(1060-980.))                                   RADABS.476
  140       CONTINUE                                                             RADABS.477
            DO 150 I=1,IPLON                                                     RADABS.478
C                                                                                RADABS.479
C ABSO(I,6)   500 -  800 CM-1   CO2 15  MICRO-METER BAND                         RADABS.480
C                                                                                RADABS.481
              SQWP=SQRT(ABS(PLCO2(I,K1)-PLCO2(I,K2)))                            RADABS.482
              ET=EXP(-480./TO3CO2(I))                                            RADABS.483
              SQTI(I)=SQRT(TO3CO2(I))                                            RADABS.484
              RSQTI=1./SQTI(I)                                                   RADABS.485
              ET2=ET*ET                                                          RADABS.486
              ET4=ET2*ET2                                                        RADABS.487
              OMET=1.-1.5*ET2                                                    RADABS.488
              F1CO2=899.70*OMET*(1.+1.94774*ET+4.73486*ET2)*RSQTI                RADABS.489
              F1SQWP(I)=F1CO2*SQWP                                               RADABS.490
              T1CO2(I)=1./(1.+(245.18*OMET*SQWP*RSQTI))                          RADABS.491
              ONEME=1.-ET2                                                       RADABS.492
              ALPHAT=ONEME**3*RSQTI                                              RADABS.493
              PI=ABS(DPNM(I))                                                    RADABS.494
              WCO2=2.5221*CO2VMR*PI*RGA                                          RADABS.495
              U7=4.9411E4*ALPHAT*ET2*WCO2                                        RADABS.496
              U8=3.9744E4*ALPHAT*ET4*WCO2                                        RADABS.497
              U9=1.0447E5*ALPHAT*ET4*ET2*WCO2                                    RADABS.498
              U13=2.8388E3*ALPHAT*ET4*WCO2                                       RADABS.499
              TPATH=TO3CO2(I)                                                    RADABS.500
              TLOCAL=TINT(I,K2)                                                  RADABS.501
              TCRFAC=SQRT(TLOCAL*R250*TPATH*R300)                                RADABS.502
              POSQT=((PNM(I,K2)+PNM(I,K1))*R2SSLP+DPFCO2*TCRFAC)*RSQTI           RADABS.503
              RBETA7=1./(5.3228*POSQT)                                           RADABS.504
              RBETA8=1./(10.6576*POSQT)                                          RADABS.505
              RBETA9=RBETA7                                                      RADABS.506
              RBETA13=RBETA9                                                     RADABS.507
              F2CO2(I)=(U7/SQRT(4.+U7*(1.+RBETA7)))+(U8/                         RADABS.508
     +                 SQRT(4.+U8*(1.+RBETA8)))+(U9/                             RADABS.509
     +                 SQRT(4.+U9*(1.+RBETA9)))                                  RADABS.510
              F3CO2(I)=U13/SQRT(4.+U13*(1.+RBETA13))                             RADABS.511
  150       CONTINUE                                                             RADABS.512
            IF(K2.GE.K1)THEN                                                     RADABS.513
              DO 152 I=1,IPLON                                                   RADABS.514
                SQTI(I)=SQRT(TLAYR(I,K2))                                        RADABS.515
  152         CONTINUE                                                           RADABS.516
            ENDIF                                                                RADABS.517
C                                                                                RADABS.518
            DO 160 I=1,IPLON                                                     RADABS.519
              TMP1=ALOG(1.+F1SQWP(I))                                            RADABS.520
              TMP2=ALOG(1.+F2CO2(I))                                             RADABS.521
              TMP3=ALOG(1.+F3CO2(I))                                             RADABS.522
              ABSBND=(TMP1+2.*T1CO2(I)*TMP2+2.*TMP3)*SQTI(I)                     RADABS.523
              ABSO(I,6)=TRAB2(I)*CO2EM(I,K2)*ABSBND                              RADABS.524
              TRAB3(I)=1.-BNDFCT*ABSBND                                          RADABS.525
  160       CONTINUE                                                             RADABS.526
            DO 165 I=1,IPLON                                                     RADABS.527
C                                                                                RADABS.528
C SUM TOTAL ABSORPTIVITY                                                         RADABS.529
C                                                                                RADABS.530
              ABSTOT(I,K1,K2)=ABSO(I,1)+ABSO(I,2)+ABSO(I,3)+ABSO(I,4)+           RADABS.531
     +                        ABSO(I,5)+ABSO(I,6)                                RADABS.532
  165       CONTINUE                                                             RADABS.533
          ENDIF                                                                  RADABS.534
  170   CONTINUE                                                                 RADABS.535
  180 CONTINUE                                                                   RADABS.536
C                                                                                RADABS.537
C END OF NON-NEAREST LAYER LEVEL LOOPS                                           RADABS.538
C                                                                                RADABS.539
C                                                                                RADABS.540
C NON-ADJACENT LAYER ABSORPTIVITY:                                               RADABS.541
C                                                                                RADABS.542
C ABSO(I,1)     0 -  800 CM-1   H2O ROTATION BAND                                RADABS.543
C ABSO(I,2)  1200 - 2200 CM-1   H2O VIBRATION-ROTATION BAND                      RADABS.544
C ABSO(I,3)   800 - 1200 CM-1   H2O WINDOW                                       RADABS.545
C ABSO(I,4)   500 -  800 CM-1   H2O ROTATION BAND OVERLAP WITH CO2               RADABS.546
C ABSO(I,5)   ?  CM-1   O3  9.6 MICRO-METER BAND                                 RADABS.547
C ABSO(I,6)   500 -  800 CM-1   CO2 15  MICRO-METER BAND                         RADABS.548
C                                                                                RADABS.549
C BEGIN NEAREST LAYER LEVEL LOOP                                                 RADABS.550
C                                                                                RADABS.551
      DO 360 K2=IPLEV,1,-1                                                       RADABS.552
        DO 190 I=1,IPLON                                                         RADABS.553
          TBAR(I,1)=0.5*(TINT(I,K2+1)+TLAYR(I,K2+1))                             RADABS.554
          EMM(I,1)=0.5*(CO2EM(I,K2+1)+CO2EML(I,K2))                              RADABS.555
          TBAR(I,2)=0.5*(TLAYR(I,K2+1)+TINT(I,K2))                               RADABS.556
          EMM(I,2)=0.5*(CO2EM(I,K2)+CO2EML(I,K2))                                RADABS.557
          TBAR(I,3)=0.5*(TBAR(I,2)+TBAR(I,1))                                    RADABS.558
          EMM(I,3)=EMM(I,1)                                                      RADABS.559
          TBAR(I,4)=TBAR(I,3)                                                    RADABS.560
          EMM(I,4)=EMM(I,2)                                                      RADABS.561
          O3EMM(I,1)=0.5*(DBVTIT(I,K2+1)+DBVTLY(I,K2))                           RADABS.562
          O3EMM(I,2)=0.5*(DBVTIT(I,K2)+DBVTLY(I,K2))                             RADABS.563
          O3EMM(I,3)=O3EMM(I,1)                                                  RADABS.564
          O3EMM(I,4)=O3EMM(I,2)                                                  RADABS.565
          TEMH2O(I,1)=TBAR(I,1)                                                  RADABS.566
          TEMH2O(I,2)=TBAR(I,2)                                                  RADABS.567
          TEMH2O(I,3)=TBAR(I,1)                                                  RADABS.568
          TEMH2O(I,4)=TBAR(I,2)                                                  RADABS.569
          DPNM(I)=PNM(I,K2+1)-PNM(I,K2)                                          RADABS.570
  190   CONTINUE                                                                 RADABS.571
        DO 205 I=1,IPLON                                                         RADABS.572
          RDPNMSQ=1./(PNMSQ(I,K2+1)-PNMSQ(I,K2))                                 RADABS.573
          RDPNM=1./DPNM(I)                                                       RADABS.574
          P1=.5*(PBR(I,K2)+PNM(I,K2+1))                                          RADABS.575
          P2=.5*(PBR(I,K2)+PNM(I,K2))                                            RADABS.576
          UINPL(I,1)=(PNMSQ(I,K2+1)-P1**2)*RDPNMSQ                               RADABS.577
          UINPL(I,2)=-(PNMSQ(I,K2)-P2**2)*RDPNMSQ                                RADABS.578
          UINPL(I,3)=-(PNMSQ(I,K2)-P1**2)*RDPNMSQ                                RADABS.579
          UINPL(I,4)=(PNMSQ(I,K2+1)-P2**2)*RDPNMSQ                               RADABS.580
          WINPL(I,1)=(.5*(PNM(I,K2+1)-PBR(I,K2)))*RDPNM                          RADABS.581
          WINPL(I,2)=(.5*(-PNM(I,K2)+PBR(I,K2)))*RDPNM                           RADABS.582
          WINPL(I,3)=(.5*(PNM(I,K2+1)+PBR(I,K2))-PNM(I,K2))*RDPNM                RADABS.583
          WINPL(I,4)=(.5*(-PNM(I,K2)-PBR(I,K2))+PNM(I,K2+1))*RDPNM               RADABS.584
          TMP1=1./(PILN(I,K2+1)-PILN(I,K2))                                      RADABS.585
          TMP2=PILN(I,K2+1)-PMLN(I,K2)                                           RADABS.586
          TMP3=PILN(I,K2)-PMLN(I,K2)                                             RADABS.587
          ZINPL(I,1)=(.5*TMP2)*TMP1                                              RADABS.588
          ZINPL(I,2)=(-.5*TMP3)*TMP1                                             RADABS.589
          ZINPL(I,3)=(.5*TMP2-TMP3)*TMP1                                         RADABS.590
          ZINPL(I,4)=(TMP2-.5*TMP3)*TMP1                                         RADABS.591
          PINPL(I,1)=0.5*(P1+PNM(I,K2+1))                                        RADABS.592
          PINPL(I,2)=0.5*(P2+PNM(I,K2))                                          RADABS.593
          PINPL(I,3)=0.5*(P1+PNM(I,K2))                                          RADABS.594
          PINPL(I,4)=0.5*(P2+PNM(I,K2+1))                                        RADABS.595
  205   CONTINUE                                                                 RADABS.596
        DO 350 KN=1,4                                                            RADABS.597
          DO 210 I=1,IPLON                                                       RADABS.598
            U(I)=UINPL(I,KN)*ABS(PLH2O(I,K2)-PLH2O(I,K2+1))                      RADABS.599
            SQRTU(I)=SQRT(U(I))                                                  RADABS.600
            DW(I)=ABS(W(I,K2)-W(I,K2+1))                                         RADABS.601
            PNEW(I)=U(I)/(WINPL(I,KN)*DW(I))                                     RADABS.602
            DS2C=ABS(S2C(I,K2)-S2C(I,K2+1))                                      RADABS.603
            UC1(I)=UINPL(I,KN)*DS2C                                              RADABS.604
            UC1(I)=(UC1(I)+1.7E-3*U(I))*(1.+2.*UC1(I))/(1.+15.*UC1(I))           RADABS.605
            UC(I)=UINPL(I,KN)*DS2C+2.E-3*U(I)                                    RADABS.606
  210     CONTINUE                                                               RADABS.607
          DO 230 I=1,IPLON                                                       RADABS.608
            DTX(I)=TEMH2O(I,KN)-250.                                             RADABS.609
            DTY(I)=TBAR(I,KN)-250.                                               RADABS.610
            DTYP15(I)=DTY(I)+15.                                                 RADABS.611
            DTYP15SQ(I)=DTYP15(I)**2                                             RADABS.612
            DTZ(I)=DTX(I)-50.                                                    RADABS.613
            DTP(I)=DTY(I)-50.                                                    RADABS.614
  230     CONTINUE                                                               RADABS.615
          DO 270 IBAND=2,4,2                                                     RADABS.616
            DO 260 I=1,IPLON                                                     RADABS.617
              TERM1(I,IBAND)=COEFE(1,IBAND)+COEFE(2,IBAND)*DTX(I)*(1.+           RADABS.618
     +                       C1(IBAND)*DTX(I))                                   RADABS.619
              TERM2(I,IBAND)=COEFB(1,IBAND)+COEFB(2,IBAND)*DTX(I)*(1.+           RADABS.620
     +                       C2(IBAND)*DTX(I)*(1.+C3(IBAND)*DTX(I)))             RADABS.621
              TERM3(I,IBAND)=COEFD(1,IBAND)+COEFD(2,IBAND)*DTX(I)*(1.+           RADABS.622
     +                       C4(IBAND)*DTX(I)*(1.+C5(IBAND)*DTX(I)))             RADABS.623
              TERM4(I,IBAND)=COEFA(1,IBAND)+COEFA(2,IBAND)*DTY(I)*(1.+           RADABS.624
     +                       C6(IBAND)*DTY(I))                                   RADABS.625
              TERM5(I,IBAND)=COEFC(1,IBAND)+COEFC(2,IBAND)*DTY(I)*(1.+           RADABS.626
     +                       C7(IBAND)*DTY(I))                                   RADABS.627
  260       CONTINUE                                                             RADABS.628
  270     CONTINUE                                                               RADABS.629
          DO 280 I=1,IPLON                                                       RADABS.630
C                                                                                RADABS.631
C ABSO(I,1)     0 -  800 CM-1   H2O ROTATION BAND                                RADABS.632
C                                                                                RADABS.633
            A11=0.44+3.380E-4*DTZ(I)-1.520E-6*DTZ(I)*DTZ(I)                      RADABS.634
            A31=1.05-6.000E-3*DTP(I)+3.000E-6*DTP(I)*DTP(I)                      RADABS.635
            A21=1.00+1.717E-3*DTZ(I)-1.133E-5*DTZ(I)*DTZ(I)                      RADABS.636
            A22=1.00+4.443E-3*DTP(I)+2.750E-5*DTP(I)*DTP(I)                      RADABS.637
            A23=1.00+3.600*SQRTU(I)                                              RADABS.638
            CORFAC=A31*(A11+((2.*A21*A22)/A23))                                  RADABS.639
            T1T4=TERM1(I,2)*TERM4(I,2)                                           RADABS.640
            T2T5=TERM2(I,2)*TERM5(I,2)                                           RADABS.641
            A=T1T4+T2T5/(1.+T2T5*SQRTU(I)*CORFAC)                                RADABS.642
            FWK(I)=FWCOEF+FWC1/(1.+FWC2*U(I))                                    RADABS.643
            FWKU(I)=FWK(I)*U(I)                                                  RADABS.644
            RSUM=EXP(-A*(SQRTU(I)+FWKU(I)))                                      RADABS.645
            ABSO(I,1)=(1.-RSUM)*TERM3(I,2)                                       RADABS.646
            TRAB1(I)=RSUM                                                        RADABS.647
  280     CONTINUE                                                               RADABS.648
          DO 282 I=1,IPLON                                                       RADABS.649
C                                                                                RADABS.650
C ABSO(I,2)  1200 - 2200 CM-1   H2O VIBRATION-ROTATION BAND                      RADABS.651
C                                                                                RADABS.652
            A41=1.75-3.960E-03*DTZ(I)                                            RADABS.653
            A51=1.00+1.3*SQRTU(I)                                                RADABS.654
            A61=1.00+1.250E-03*DTP(I)+6.250E-05*DTP(I)*DTP(I)                    RADABS.655
            CORFAC=.29*(1.+A41/A51)*A61                                          RADABS.656
            T1T4=TERM1(I,4)*TERM4(I,4)                                           RADABS.657
            T2T5=TERM2(I,4)*TERM5(I,4)                                           RADABS.658
            A=T1T4+T2T5/(1.+T2T5*SQRTU(I)*CORFAC)                                RADABS.659
            RSUM=EXP(-A*(SQRTU(I)+FWKU(I)))                                      RADABS.660
            ABSO(I,2)=(1.-RSUM)*TERM3(I,4)                                       RADABS.661
            TRAB7(I)=RSUM                                                        RADABS.662
  282     CONTINUE                                                               RADABS.663
C                                                                                RADABS.664
C LINE TRANSMISSION IN 800-1000 AND 1000-1200 CM-1 INTERVALS                     RADABS.665
C                                                                                RADABS.666
          DO 300 K=1,2                                                           RADABS.667
            DO 290 I=1,IPLON                                                     RADABS.668
              PHI=EXP(A1(K)*DTYP15(I)+A2(K)*DTYP15SQ(I))                         RADABS.669
              PSI=EXP(B1(K)*DTYP15(I)+B2(K)*DTYP15SQ(I))                         RADABS.670
              UBAR=DW(I)*PHI*WINPL(I,KN)*1.66*R80257                             RADABS.671
              PBAR=PNEW(I)*(PSI/PHI)                                             RADABS.672
              CF812=CFA1+(1.-CFA1)/(1.+UBAR*PBAR*10.)                            RADABS.673
              G2=1.+UBAR*4.0*ST(K)*CF812/PBAR                                    RADABS.674
              G4=REALK(K)*PBAR*R2ST(K)*(SQRT(G2)-1.)                             RADABS.675
              TRLINE(I,K)=EXP(-G4)                                               RADABS.676
  290       CONTINUE                                                             RADABS.677
  300     CONTINUE                                                               RADABS.678
          DO 310 I=1,IPLON                                                       RADABS.679
            TERM7(I,1)=COEFJ(1,1)+COEFJ(2,1)*DTY(I)*(1.+C16*DTY(I))              RADABS.680
            TERM8(I,1)=COEFK(1,1)+COEFK(2,1)*DTY(I)*(1.+C17*DTY(I))              RADABS.681
            TERM7(I,2)=COEFJ(1,2)+COEFJ(2,2)*DTY(I)*(1.+C26*DTY(I))              RADABS.682
            TERM8(I,2)=COEFK(1,2)+COEFK(2,2)*DTY(I)*(1.+C27*DTY(I))              RADABS.683
  310     CONTINUE                                                               RADABS.684
          DO 320 I=1,IPLON                                                       RADABS.685
C                                                                                RADABS.686
C ABSO(I,3)   800 - 1200 CM-1   H2O WINDOW                                       RADABS.687
C ABSO(I,4)   500 -  800 CM-1   H2O ROTATION BAND OVERLAP WITH CO2               RADABS.688
C                                                                                RADABS.689
            DTYM10=DTY(I)-10.                                                    RADABS.690
            DENOM=1.+(C30+C31*DTYM10*DTYM10)*SQRTU(I)                            RADABS.691
            K21=TERM7(I,1)+TERM8(I,1)/DENOM                                      RADABS.692
            DENOM=1.+(C28+C29*DTYM10)*SQRTU(I)                                   RADABS.693
            K22=TERM7(I,2)+TERM8(I,2)/DENOM                                      RADABS.694
            TERM9(I,2)=COEFI(1,2)+COEFI(2,2)*DTX(I)*(1.+C19*DTX(I)*(1.+          RADABS.695
     +                 C21*DTX(I)*(1.+C23*DTX(I)*(1.+C25*DTX(I)))))              RADABS.696
            TR1=EXP(-(K21*(SQRTU(I)+FC1*FWKU(I))))                               RADABS.697
            TR2=EXP(-(K22*(SQRTU(I)+FC1*FWKU(I))))                               RADABS.698
            TR5=EXP(-((COEFH(1,3)+COEFH(2,3)*DTX(I))*UC1(I)))                    RADABS.699
            TR6=EXP(-((COEFH(1,4)+COEFH(2,4)*DTX(I))*UC1(I)))                    RADABS.700
            TR9(I)=TR1*TR5                                                       RADABS.701
            TR10(I)=TR2*TR6                                                      RADABS.702
            TRAB2(I)=0.65*TR9(I)+0.35*TR10(I)                                    RADABS.703
            TRAB4(I)=EXP(-(COEFG(1,3)+COEFG(2,3)*DTX(I))*UC(I))                  RADABS.704
            TRAB6(I)=EXP(-(COEFG(1,4)+COEFG(2,4)*DTX(I))*UC(I))                  RADABS.705
            TERM6(I,2)=COEFF(1,2)+COEFF(2,2)*DTX(I)*(1.+C9*DTX(I)*(1.+           RADABS.706
     +                 C11*DTX(I)*(1.+C13*DTX(I)*(1.+C15*DTX(I)))))              RADABS.707
            ABSO(I,3)=TERM6(I,2)*(1.-.5*TRAB4(I)*TRLINE(I,2)-.5*TRAB6(I)         RADABS.708
     +                *TRLINE(I,1))                                              RADABS.709
            ABSO(I,4)=TERM9(I,2)*.5*(TR1-TR9(I)+TR2-TR10(I))                     RADABS.710
  320     CONTINUE                                                               RADABS.711
          DO 330 I=1,IPLON                                                       RADABS.712
C                                                                                RADABS.713
C ABSO(I,5)   ?  CM-1   O3  9.6 MICRO-METER BAND                                 RADABS.714
C                                                                                RADABS.715
            TE=(TBAR(I,KN)*R293)**.7                                             RADABS.716
            DPLOS=ABS(PLOS(I,K2+1)-PLOS(I,K2))                                   RADABS.717
            U1=ZINPL(I,KN)*18.29*DPLOS/TE                                        RADABS.718
            U2=ZINPL(I,KN)*.5649*DPLOS/TE                                        RADABS.719
            TLOCAL=TBAR(I,KN)                                                    RADABS.720
            TCRFAC=SQRT(TLOCAL*R250)*TE                                          RADABS.721
            BETA=R3205*(PINPL(I,KN)*RSSLP+DPFO3*TCRFAC)                          RADABS.722
            REALNU=TE/BETA                                                       RADABS.723
            TMP1=U1/SQRT(4.+U1*(1.+REALNU))                                      RADABS.724
            TMP2=U2/SQRT(4.+U2*(1.+REALNU))                                      RADABS.725
            O3BNDI=74.*TE*ALOG(1.+TMP1+TMP2)                                     RADABS.726
            ABSO(I,5)=O3BNDI*O3EMM(I,KN)*(H2OTR(I,K2+1)/H2OTR(I,K2))             RADABS.727
            TRAB5(I)=1.-(O3BNDI/(1060-980.))                                     RADABS.728
  330     CONTINUE                                                               RADABS.729
          DO 340 I=1,IPLON                                                       RADABS.730
C                                                                                RADABS.731
C ABSO(I,6)   500 -  800 CM-1   CO2 15  MICRO-METER BAND                         RADABS.732
C                                                                                RADABS.733
            DPLCO2=PLCO2(I,K2+1)-PLCO2(I,K2)                                     RADABS.734
            SQWP=SQRT(UINPL(I,KN)*DPLCO2)                                        RADABS.735
            ET=EXP(-480./TBAR(I,KN))                                             RADABS.736
            SQTI(I)=SQRT(TBAR(I,KN))                                             RADABS.737
            RSQTI=1./SQTI(I)                                                     RADABS.738
            ET2=ET*ET                                                            RADABS.739
            ET4=ET2*ET2                                                          RADABS.740
            OMET=(1.-1.5*ET2)                                                    RADABS.741
            F1CO2=899.70*OMET*(1.+1.94774*ET+4.73486*ET2)*RSQTI                  RADABS.742
            F1SQWP(I)=F1CO2*SQWP                                                 RADABS.743
            T1CO2(I)=1./(1.+(245.18*OMET*SQWP*RSQTI))                            RADABS.744
            ONEME=1.-ET2                                                         RADABS.745
            ALPHAT=ONEME**3*RSQTI                                                RADABS.746
            PI=ABS(DPNM(I))*WINPL(I,KN)                                          RADABS.747
            WCO2=2.5221*CO2VMR*PI*RGA                                            RADABS.748
            U7=4.9411E4*ALPHAT*ET2*WCO2                                          RADABS.749
            U8=3.9744E4*ALPHAT*ET4*WCO2                                          RADABS.750
            U9=1.0447E5*ALPHAT*ET4*ET2*WCO2                                      RADABS.751
            U13=2.8388E3*ALPHAT*ET4*WCO2                                         RADABS.752
            TPATH=TBAR(I,KN)                                                     RADABS.753
            TLOCAL=TBAR(I,KN)                                                    RADABS.754
            TCRFAC=SQRT((TLOCAL*R250)*(TPATH*R300))                              RADABS.755
            POSQT=(PINPL(I,KN)*RSSLP+DPFCO2*TCRFAC)*RSQTI                        RADABS.756
            RBETA7=1./(5.3228*POSQT)                                             RADABS.757
            RBETA8=1./(10.6576*POSQT)                                            RADABS.758
            RBETA9=RBETA7                                                        RADABS.759
            RBETA13=RBETA9                                                       RADABS.760
            F2CO2(I)=U7/SQRT(4.+U7*(1.+RBETA7))+U8/                              RADABS.761
     +               SQRT(4.+U8*(1.+RBETA8))+U9/SQRT(4.+U9*(1.+RBETA9))          RADABS.762
            F3CO2(I)=U13/SQRT(4.+U13*(1.+RBETA13))                               RADABS.763
            TMP1=ALOG(1.+F1SQWP(I))                                              RADABS.764
            TMP2=ALOG(1.+F2CO2(I))                                               RADABS.765
            TMP3=ALOG(1.+F3CO2(I))                                               RADABS.766
            ABSBND=(TMP1+2.*T1CO2(I)*TMP2+2.*TMP3)*SQTI(I)                       RADABS.767
            ABSO(I,6)=TRAB2(I)*EMM(I,KN)*ABSBND                                  RADABS.768
            TRAB3(I)=1.-BNDFCT*ABSBND                                            RADABS.769
  340     CONTINUE                                                               RADABS.770
C                                                                                RADABS.771
C COMPUTE TOTAL NEXT LAYER ABSORPTIVITY:                                         RADABS.772
C                                                                                RADABS.773
          DO 370 I=1,IPLON                                                       RADABS.774
            ABSNXT(I,K2,KN)=ABSO(I,1)+ABSO(I,2)+ABSO(I,3)+ABSO(I,4)+             RADABS.775
     +                      ABSO(I,5)+ABSO(I,6)                                  RADABS.776
  370     CONTINUE                                                               RADABS.777
C                                                                                RADABS.778
  350   CONTINUE                                                                 RADABS.779
  360 CONTINUE                                                                   RADABS.780
C                                                                                RADABS.781
C END OF NEAREST LAYER LEVEL LOOP                                                RADABS.782
C                                                                                RADABS.783
C                                                                                RADABS.784
C DONE                                                                           RADABS.785
C                                                                                RADABS.786
      RETURN                                                                     RADABS.787
      END                                                                        RADABS.788
                                                                                 RADABS.789
