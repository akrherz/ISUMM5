C-----------------------------------------------------------------------         RADEMS.1
C-----------------------------------------------------------------------         RADEMS.2
      SUBROUTINE RADEMS(S2C    ,S2T    ,W     ,TPLNKE ,PLH2O ,                   RADEMS.3
     $                  PNM    ,PLCO2  ,TINT  ,TINT4  ,TLAYR ,                   RADEMS.4
     $                  TLAYR4 ,PLOL   ,PLOS  ,CO2EM  ,CO2EML,                   RADEMS.5
     $                  CO2T   ,H2OTR  ,EMSTOT)                                  RADEMS.6
C                                                                                RADEMS.7
C COMPUTE EMISSIVITY FOR H2O, CO2, O3                                            RADEMS.8
C                                                                                RADEMS.9
C                                                                                RADEMS.10
C H2O  ....  USES NONISOTHERMAL EMISSIVITY FOR WATER VAPOR FROM                  RADEMS.11
C            RAMANATHAN, V. AND  P.DOWNEY, 1986: A NONISOTHERMAL                 RADEMS.12
C            EMISSIVITY AND ABSORPTIVITY FORMULATION FOR WATER VAPOR             RADEMS.13
C            JOURANL OF GEOPHYSICAL RESEARCH, VOL. 91., D8, PP 8649-8666         RADEMS.14
C                                                                                RADEMS.15
C                                                                                RADEMS.16
C CO2  ....  USES ABSORPTANCE PARAMETERIZATION OF THE 15 MICRO-METER             RADEMS.17
C            (500 - 800 CM-1) BAND SYSTEM OF CARBON DIOXIDE, FROM                RADEMS.18
C            KIEHL, J.T. AND B.P.BRIEGLEB, 1991: A NEW PARAMETERIZATION          RADEMS.19
C            OF THE ABSORPTANCE DUE TO THE 15 MICRO-METER BAND SYSTEM            RADEMS.20
C            OF CARBON DIOXIDE JOURANL OF GEOPHYSICAL RESEARCH,                  RADEMS.21
C            VOL. 96., D5, PP 9013-9019                                          RADEMS.22
C                                                                                RADEMS.23
C O3   ....  USES ABSORPTANCE PARAMETERIZATION OF THE 9.6 MICRO-METER            RADEMS.24
C            BAND SYSTEM OF OZONE, FROM RAMANATHAN, V. AND R.E.DICKINSON         RADEMS.25
C            1979: THE ROLE OF STRATOSPHERIC OZONE IN THE ZONAL AND              RADEMS.26
C            SEASONAL RADIATIVE ENERGY BALANCE OF THE EARTH-TROPOSPHERE          RADEMS.27
C            SYSTEM. JOURNAL OF THE ATMOSPHERIC SCIENCES, VOL. 36,               RADEMS.28
C            PP 1084-1104                                                        RADEMS.29
C                                                                                RADEMS.30
C                                                                                RADEMS.31
C COMPUTES INDIVIDUAL EMISSIVITIES, ACCOUNTING FOR BAND OVERLAP, AND             RADEMS.32
C SUMS TO OBTAIN THE TOTAL.                                                      RADEMS.33
C                                                                                RADEMS.34
C     INCLUDE 'PARAME'                                                           RADEMS.35
C     INCLUDE 'RADCCM2.DATA'                                                     RADEMS.36
#     include <parame.incl>                                                      RADEMS.37
#     include <parccm2.incl>                                                     RADEMS.38
#     include <radccm2.incl>                                                     RADEMS.39
#ifdef MPP1                                                                      RADEMS.40
#     include <various.incl>                                                     RADEMS.41
#endif                                                                           RADEMS.42
C-----------------------------------------------------------------------         RADEMS.43
C                                                                                RADEMS.44
C WATER VAPOR NARROW BAND CONSTANTS FOR LW COMPUTATIONS                          RADEMS.45
C                                                                                RADEMS.46
      REAL REALK,ST,A1,A2,B1,B2,                                                 RADEMS.47
     $     COEFA,COEFB,COEFC,COEFD,                                              RADEMS.48
     $     COEFE,COEFF,COEFG,COEFH,                                              RADEMS.49
     $     COEFI,COEFJ,COEFK,                                                    RADEMS.50
     $     C1,C2,C3,C4,C5,C6,C7,                                                 RADEMS.51
     $     C8 ,C9 ,C10,C11,C12,C13,C14,C15,C16,C17,                              RADEMS.52
     $     C18,C19,C20,C21,C22,C23,C24,C25,C26,C27,                              RADEMS.53
     $     C28,C29,C30,C31,                                                      RADEMS.54
     $     FWCOEF,FWC1,FWC2,FC1,CFA1                                             RADEMS.55
C                                                                                RADEMS.56
      COMMON/CRDCAE/REALK(2), ST(2), A1(2), A2(2), B1(2), B2(2),                 RADEMS.57
C                                                                                RADEMS.58
C CONSTANT COEFFICIENTS FOR WATER VAPOR ABSORPTIVITY AND EMISSIVI                RADEMS.59
C                                                                                RADEMS.60
     $              COEFA(3,4),COEFB(4,4),COEFC(3,4),COEFD(4,4),                 RADEMS.61
     $              COEFE(3,4),COEFF(6,2),COEFG(2,4),COEFH(2,4),                 RADEMS.62
     $              COEFI(6,2),COEFJ(3,2),COEFK(3,2),                            RADEMS.63
     $              C1(4),C2(4),C3(4),C4(4),C5(4),C6(4),C7(4),                   RADEMS.64
     $              C8 ,C9 ,C10,C11,C12,C13,C14,C15,C16,C17,                     RADEMS.65
     $              C18,C19,C20,C21,C22,C23,C24,C25,C26,C27,                     RADEMS.66
     $              C28,C29,C30,C31,                                             RADEMS.67
C                                                                                RADEMS.68
C FARWING CORRECTION CONSTANTS FOR NARROW-BAND EMISSIVITY MODEL                  RADEMS.69
C INTRODUCE FARWING CORRECTION TO ACCOUNT FOR THE                                RADEMS.70
C DEFICIENCIES IN NARROW-BAND MODEL USED TO DERIVE THE                           RADEMS.71
C EMISSIVITY. TUNED WITH ARKINGS LINE CALCULATIONS.                              RADEMS.72
C                                                                                RADEMS.73
     $              FWCOEF,FWC1,FWC2,FC1,CFA1                                    RADEMS.74
C                                                                                RADEMS.75
C                                                                                RADEMS.76
C INPUT ARGUMENTS                                                                RADEMS.77
C                                                                                RADEMS.78
      REAL S2C(IPLOND,IPLEVP),    ! H2O CONTINUUM PATH LENGTH                    RADEMS.79
     $     S2T(IPLOND,IPLEVP),    ! TMP AND PRS WGHTED H2O PATH LENGTH           RADEMS.80
     $     W(IPLOND,IPLEVP),      ! H2O PATH LENGTH                              RADEMS.81
     $     TPLNKE(IPLOND),       ! LAYER PLANCK TEMPERATURE                      RADEMS.82
     $     PLH2O(IPLOND,IPLEVP),  ! H2O PRS WGHTED PATH LENGTH                   RADEMS.83
     $     PNM(IPLOND,IPLEVP),    ! MODEL INTERFACE PRESSURE                     RADEMS.84
     $     PLCO2(IPLOND,IPLEVP),  ! PRS WGHTED PATH OF CO2                       RADEMS.85
     $     TINT(IPLOND,IPLEVP),   ! MODEL INTERFACE TEMPERATURES                 RADEMS.86
     $     TINT4(IPLOND,IPLEVP),  ! TINT TO THE 4TH POWER                        RADEMS.87
     $     TLAYR(IPLOND,IPLEVP),  ! K-1 MODEL LAYER TEMPERATURE                  RADEMS.88
     $     TLAYR4(IPLOND,IPLEVP), ! TLAYR TO THE 4TH POWER                       RADEMS.89
     $     PLOL(IPLOND,IPLEVP),   ! PRESSURE WGHTD OZONE PATH                    RADEMS.90
     $     PLOS(IPLOND,IPLEVP)    ! OZONE PATH                                   RADEMS.91
C                                                                                RADEMS.92
C OUTPUT ARGUMENTS                                                               RADEMS.93
C                                                                                RADEMS.94
      REAL EMSTOT(IPLOND,IPLEVP),  ! TOTAL EMISSIVITY                            RADEMS.95
     $     CO2EM(IPLOND,IPLEVP),   ! LAYER CO2 NORMALIZED PLNCK FUNCTION         RADEMS.96
     $     CO2EML(IPLOND,IPLEV),   ! INTRFC CO2 NORMALIZED PLNCK FUNCTIO         RADEMS.97
     $     CO2T(IPLOND,IPLEVP),    ! TMP AND PRS WEIGHTED PATH LENGTH            RADEMS.98
     $     H2OTR(IPLOND,IPLEVP)    ! H2O TRANSMISSION OVER O3 BAND               RADEMS.99
C                                                                                RADEMS.100
C LOCAL WORKSPACE FOR H2O:                                                       RADEMS.101
C                                                                                RADEMS.102
      INTEGER   I,            ! LONGITUDE INDEX                                  RADEMS.103
     $          K,            ! LEVEL INDEX]                                     RADEMS.104
     $         K1,            ! LEVEL INDEX                                      RADEMS.105
     $      IBAND             ! H2O BAND INDEX                                   RADEMS.106
C                                                                                RADEMS.107
      REAL H2OEMS(IPLOND,IPLEVP),! H2O EMISSIVITY                                RADEMS.108
     $     TPATHE(IPLOND),     ! USED TO COMPUTE H2O EMISSIVITY                  RADEMS.109
     $     A(IPLOND),          ! " "                                             RADEMS.110
     $     CORFAC(IPLOND),     ! " "                                             RADEMS.111
     $     DTP(IPLOND),        ! " "                                             RADEMS.112
     $     DTX(IPLOND),        ! " "                                             RADEMS.113
     $     DTY(IPLOND),        ! " "                                             RADEMS.114
     $     DTZ(IPLOND),        ! " "                                             RADEMS.115
     $     EMIS(IPLOND,4),     ! " "                                             RADEMS.116
     $     RSUM(IPLOND),       ! " "                                             RADEMS.117
     $     TERM1(IPLOND,4),    ! " "                                             RADEMS.118
     $     TERM2(IPLOND,4)     ! " "                                             RADEMS.119
      REAL TERM3(IPLOND,4),    ! " "                                             RADEMS.120
     $     TERM4(IPLOND,4),    ! " "                                             RADEMS.121
     $     TERM5(IPLOND,4),    ! " "                                             RADEMS.122
     $     TERM6(IPLOND,2),    ! " "                                             RADEMS.123
     $     TERM7(IPLOND,2),    ! " "                                             RADEMS.124
     $     TERM8(IPLOND,2),    ! " "                                             RADEMS.125
     $     TERM9(IPLOND,2),    ! " "                                             RADEMS.126
     $     TR1(IPLOND),        ! " "                                             RADEMS.127
     $     TR2(IPLOND),        ! " "                                             RADEMS.128
     $     TR3(IPLOND)         ! " "                                             RADEMS.129
      REAL TR4(IPLOND),        ! " "                                             RADEMS.130
     $     TR7(IPLOND),        ! " "                                             RADEMS.131
     $     TR8(IPLOND),        ! " "                                             RADEMS.132
     $     UC(IPLOND),         ! " "                                             RADEMS.133
     $     PNEW(IPLOND),       ! " "                                             RADEMS.134
     $     TRLINE(IPLOND,2),   ! " "                                             RADEMS.135
     $     K21(IPLOND),        ! " "                                             RADEMS.136
     $     K22(IPLOND),        ! " "                                             RADEMS.137
     $     U(IPLOND),          ! " "                                             RADEMS.138
     $     UC1(IPLOND),        ! " "                                             RADEMS.139
     $        R80257                                                             RADEMS.140
      REAL      A11,          ! USED TO COMPUTE H2O EMISSIVITY                   RADEMS.141
     $          A31,          ! " "                                              RADEMS.142
     $          A21,          ! " "                                              RADEMS.143
     $          A22,          ! " "                                              RADEMS.144
     $          A23,          ! " "                                              RADEMS.145
     $          T1T4,         ! " "                                              RADEMS.146
     $          T2T5,         ! " "                                              RADEMS.147
     $          FWK,          ! " "                                              RADEMS.148
     $          A41,          ! " "                                              RADEMS.149
     $          A51,          ! " "                                              RADEMS.150
     $          A61,          ! " "                                              RADEMS.151
     $          PHI,          ! " "                                              RADEMS.152
     $          PSI,          ! " "                                              RADEMS.153
     $          UBAR,         ! " "                                              RADEMS.154
     $          G1,           ! " "                                              RADEMS.155
     $          PBAR,         ! " "                                              RADEMS.156
     $          G3,           ! " "                                              RADEMS.157
     $          G2,           ! " "                                              RADEMS.158
     $          G4,           ! " "                                              RADEMS.159
     $       CF812            ! " "                                              RADEMS.160
      REAL TROCO2(IPLOND,IPLEVP)  ! H2O OVERLAP FACTOR FOR CO2 ABSORPTIO         RADEMS.161
C                                                                                RADEMS.162
C LOCAL WORKSPACE FOR CO2:                                                       RADEMS.163
C                                                                                RADEMS.164
      REAL CO2EMS(IPLOND,IPLEVP), ! CO2 EMISSIVITY                               RADEMS.165
     $     CO2PLK(IPLOND),    ! USED TO COMPUTE CO2 EMISSIVITY                   RADEMS.166
     $        SUM(IPLOND),    ! " "                                              RADEMS.167
     $               T1I,    ! " "                                               RADEMS.168
     $              SQTI,    ! " "                                               RADEMS.169
     $                PI,    ! " "                                               RADEMS.170
     $                ET,    ! " "                                               RADEMS.171
     $               ET2,    ! " "                                               RADEMS.172
     $               ET4,    ! " "                                               RADEMS.173
     $              OMET,    ! " "                                               RADEMS.174
     $                EX     ! " "                                               RADEMS.175
      REAL         F1CO2,    ! " "                                               RADEMS.176
     $             F2CO2,    ! " "                                               RADEMS.177
     $             F3CO2,    ! " "                                               RADEMS.178
     $             T1CO2,    ! " "                                               RADEMS.179
     $              SQWP,    ! " "                                               RADEMS.180
     $            F1SQWP,    ! " "                                               RADEMS.181
     $             ONEME,    ! " "                                               RADEMS.182
     $            ALPHAT,    ! " "                                               RADEMS.183
     $              WCO2,    ! " "                                               RADEMS.184
     $             POSQT,    ! " "                                               RADEMS.185
     $            RBETA7,    ! " "                                               RADEMS.186
     $            RBETA8,    ! " "                                               RADEMS.187
     $            RBETA9,    ! " "                                               RADEMS.188
     $           RBETA13     ! " "                                               RADEMS.189
      REAL         TPATH,    ! " "                                               RADEMS.190
     $              TMP1,    ! " "                                               RADEMS.191
     $              TMP2,    ! " "                                               RADEMS.192
     $              TMP3,    ! " "                                               RADEMS.193
     $            TLAYR5,    ! " "                                               RADEMS.194
     $             RSQTI,    ! " "                                               RADEMS.195
     $            EXM1SQ     ! " "                                               RADEMS.196
C                                                                                RADEMS.197
      REAL U7,    ! ABSORBER AMOUNT FOR VARIOUS CO2 BAND SYSTEMS                 RADEMS.198
     $     U8,    !    " "                                                       RADEMS.199
     $     U9,    !    " "                                                       RADEMS.200
     $     U13    !    " "                                                       RADEMS.201
C                                                                                RADEMS.202
      REAL R250,  ! INVERSE 250K                                                 RADEMS.203
     $     R300,  ! INVERSE 300K                                                 RADEMS.204
     $    RSSLP   ! INVERSE STANDARD SEA-LEVEL PRESSURE                          RADEMS.205
C                                                                                RADEMS.206
C LOCAL WORKSPACE FOR O3:                                                        RADEMS.207
C                                                                                RADEMS.208
      REAL O3EMS(IPLOND,IPLEVP),  ! OZONE EMISSIVITY                             RADEMS.209
     $     DBVTT(IPLOND),   ! TMP DRVTV OF PLANCK FCTN FOR TPLNKE                RADEMS.210
     $               TE,   ! TEMPERATURE FACTOR                                  RADEMS.211
     $               U1,   ! PATH LENGTH FACTOR                                  RADEMS.212
     $               U2,   ! PATH LENGTH FACTOR                                  RADEMS.213
     $             PHAT,   ! EFFECITIVE PATH LENGTH PRESSURE                     RADEMS.214
     $           TLOCAL,   ! LOCAL PLANCK FUNCTION TEMPERATURE                   RADEMS.215
     $           TCRFAC,   ! SCALED TEMPERATURE FACTOR                           RADEMS.216
     $             BETA,   ! ABSORPTION FUNCTION FACTOR WITH VOIGT EFFEC         RADEMS.217
     $           REALNU,   ! ABSORPTION FUNCTION FACTOR                          RADEMS.218
     $           O3BNDI    ! BAND ABSORPTION FACTOR                              RADEMS.219
C                                                                                RADEMS.220
C TRANSMISSION TERMS FOR VARIOUS SPECTRAL INTERVALS:                             RADEMS.221
C                                                                                RADEMS.222
      REAL    TREM1(IPLOND),  ! H2O     0 -  800 CM-1                            RADEMS.223
     $        TREM2(IPLOND),  ! H2O   500 -  800 CM-1                            RADEMS.224
     $        TREM3(IPLOND),  ! CO2   500 -  800 CM-1                            RADEMS.225
     $        TREM4(IPLOND),  ! H2O   800 - 1000 CM-1                            RADEMS.226
     $        TREM5(IPLOND),  ! O3     9.6 MICRO-METER BAND                      RADEMS.227
     $        TREM6(IPLOND),  ! H2O  1000 - 1200 CM-1                            RADEMS.228
     $        TREM7(IPLOND)   ! H2O  1200 - 2200 CM-1                            RADEMS.229
C                                                                                RADEMS.230
      REAL BNDFCT, ! BAND ABSORPTANCE PARAMETER FOR CO2                          RADEMS.231
     $     ABSBND  ! PROPORTIONAL TO CO2 BAND ABSORPTANCE                        RADEMS.232
C                                                                                RADEMS.233
C DERIVATIVE OF PLANCK FUNCTION AT 9.6 MICRO-METER WAVELENGTH, AND               RADEMS.234
C AN ABSORPTION FUNCTION FACTOR:                                                 RADEMS.235
C                                                                                RADEMS.236
      REAL DBVT,FO3,T,UX,VX                                                      RADEMS.237
C                                                                                RADEMS.238
      DBVT(T)=(-2.8911366682E-4+(2.3771251896E-6+1.1305188929E-10*T)*T)/         RADEMS.239
     $  (1.0+(-6.1364820707E-3+1.5550319767E-5*T)*T)                             RADEMS.240
C                                                                                RADEMS.241
      FO3(UX,VX)=UX/SQRT(4.+UX*(1.+VX))                                          RADEMS.242
C                                                                                RADEMS.243
                                                                                 RADEMS.244
#ifdef MPP1                                                                      RADEMS.245
#     define IPLON I  LX                                                         RADEMS.246
#     define IPLOND I  LX                                                        RADEMS.247
#endif                                                                           RADEMS.248
C                                                                                RADEMS.249
C                                                                                RADEMS.250
C INITIALIZE                                                                     RADEMS.251
C                                                                                RADEMS.252
      R80257=1./8.0257E-04                                                       RADEMS.253
C                                                                                RADEMS.254
      R250=1./250.                                                               RADEMS.255
      R300=1./300.                                                               RADEMS.256
      RSSLP=1./SSLP                                                              RADEMS.257
C                                                                                RADEMS.258
C PLANCK FUNCTION FOR CO2                                                        RADEMS.259
C                                                                                RADEMS.260
      DO 110 I=1,IPLON                                                           RADEMS.261
        EX=EXP(960./TPLNKE(I))                                                   RADEMS.262
        CO2PLK(I)=5.E8/((TPLNKE(I)**4)*(EX-1.))                                  RADEMS.263
        CO2T(I,1)=TPLNKE(I)                                                      RADEMS.264
        SUM(I)=CO2T(I,1)*PNM(I,1)                                                RADEMS.265
  110 CONTINUE                                                                   RADEMS.266
      K=1                                                                        RADEMS.267
      DO 140 K1=IPLEVP,2,-1                                                      RADEMS.268
        K=K+1                                                                    RADEMS.269
        DO 130 I=1,IPLON                                                         RADEMS.270
          SUM(I)=SUM(I)+TLAYR(I,K)*(PNM(I,K)-PNM(I,K-1))                         RADEMS.271
          EX=EXP(960./TLAYR(I,K1))                                               RADEMS.272
          TLAYR5=TLAYR(I,K1)*TLAYR4(I,K1)                                        RADEMS.273
          CO2EML(I,K1-1)=1.2E11*EX/(TLAYR5*(EX-1.)**2)                           RADEMS.274
          CO2T(I,K)=SUM(I)/PNM(I,K)                                              RADEMS.275
  130   CONTINUE                                                                 RADEMS.276
  140 CONTINUE                                                                   RADEMS.277
      BNDFCT=2.0*22.18/(SQRT(196.)*300.)                                         RADEMS.278
C                                                                                RADEMS.279
C INITIALIZE PLANCK FUNCTION DERIVATIVE FOR O3                                   RADEMS.280
C                                                                                RADEMS.281
      DO 210 I=1,IPLON                                                           RADEMS.282
        DBVTT(I)=DBVT(TPLNKE(I))                                                 RADEMS.283
  210 CONTINUE                                                                   RADEMS.284
C                                                                                RADEMS.285
C BEGIN INTERFACE LOOP                                                           RADEMS.286
C                                                                                RADEMS.287
      DO 100 K1=1,IPLEVP                                                         RADEMS.288
C                                                                                RADEMS.289
C H2O EMISSIVITY                                                                 RADEMS.290
C                                                                                RADEMS.291
C EMIS(I,1)     0 -  800 CM-1   ROTATION BAND                                    RADEMS.292
C EMIS(I,2)  1200 - 2200 CM-1   VIBRATION-ROTATION BAND                          RADEMS.293
C EMIS(I,3)   800 - 1200 CM-1   WINDOW                                           RADEMS.294
C EMIS(I,4)   500 -  800 CM-1   ROTATION BAND OVERLAP WITH CO2                   RADEMS.295
C                                                                                RADEMS.296
C FOR THE P TYPE CONTINUUM                                                       RADEMS.297
C                                                                                RADEMS.298
        DO 10 I=1,IPLON                                                          RADEMS.299
          UC(I)=S2C(I,K1)+2.E-3*PLH2O(I,K1)                                      RADEMS.300
          U(I)=PLH2O(I,K1)                                                       RADEMS.301
          PNEW(I)=U(I)/W(I,K1)                                                   RADEMS.302
C                                                                                RADEMS.303
C APPLY SCALING FACTOR FOR 500-800 CONTINUUM                                     RADEMS.304
C                                                                                RADEMS.305
          UC1(I)=(S2C(I,K1)+1.7E-3*PLH2O(I,K1))*(1.+2.*S2C(I,K1))/(1.+           RADEMS.306
     +           15.*S2C(I,K1))                                                  RADEMS.307
          TPATHE(I)=S2T(I,K1)/PLH2O(I,K1)                                        RADEMS.308
   10   CONTINUE                                                                 RADEMS.309
        DO 20 I=1,IPLON                                                          RADEMS.310
          DTX(I)=TPLNKE(I)-250.                                                  RADEMS.311
          DTY(I)=TPATHE(I)-250.                                                  RADEMS.312
          DTZ(I)=DTX(I)-50.                                                      RADEMS.313
          DTP(I)=DTY(I)-50.                                                      RADEMS.314
   20   CONTINUE                                                                 RADEMS.315
        DO 40 IBAND=1,3,2                                                        RADEMS.316
          DO 30 I=1,IPLON                                                        RADEMS.317
            TERM1(I,IBAND)=COEFE(1,IBAND)+COEFE(2,IBAND)*DTX(I)*(1.+             RADEMS.318
     +                     C1(IBAND)*DTX(I))                                     RADEMS.319
            TERM2(I,IBAND)=COEFB(1,IBAND)+COEFB(2,IBAND)*DTX(I)*(1.+             RADEMS.320
     +                     C2(IBAND)*DTX(I)*(1.+C3(IBAND)*DTX(I)))               RADEMS.321
            TERM3(I,IBAND)=COEFD(1,IBAND)+COEFD(2,IBAND)*DTX(I)*(1.+             RADEMS.322
     +                     C4(IBAND)*DTX(I)*(1.+C5(IBAND)*DTX(I)))               RADEMS.323
            TERM4(I,IBAND)=COEFA(1,IBAND)+COEFA(2,IBAND)*DTY(I)*(1.+             RADEMS.324
     +                     C6(IBAND)*DTY(I))                                     RADEMS.325
            TERM5(I,IBAND)=COEFC(1,IBAND)+COEFC(2,IBAND)*DTY(I)*(1.+             RADEMS.326
     +                     C7(IBAND)*DTY(I))                                     RADEMS.327
   30     CONTINUE                                                               RADEMS.328
   40   CONTINUE                                                                 RADEMS.329
        DO 50 I=1,IPLON                                                          RADEMS.330
C                                                                                RADEMS.331
C EMIS(I,1)     0 -  800 CM-1   ROTATION BAND                                    RADEMS.332
C                                                                                RADEMS.333
          A11=.37-3.33E-5*DTZ(I)+3.33E-6*DTZ(I)*DTZ(I)                           RADEMS.334
          A31=1.07-1.00E-3*DTP(I)+1.475E-5*DTP(I)*DTP(I)                         RADEMS.335
          A21=1.3870+3.80E-3*DTZ(I)-7.8E-6*DTZ(I)*DTZ(I)                         RADEMS.336
          A22=1.0-1.21E-3*DTP(I)-5.33E-6*DTP(I)*DTP(I)                           RADEMS.337
          A23=0.9+2.62*SQRT(U(I))                                                RADEMS.338
          CORFAC(I)=A31*(A11+((A21*A22)/A23))                                    RADEMS.339
          T1T4=TERM1(I,1)*TERM4(I,1)                                             RADEMS.340
          T2T5=TERM2(I,1)*TERM5(I,1)                                             RADEMS.341
          A(I)=T1T4+T2T5/(1.+T2T5*SQRT(U(I))*CORFAC(I))                          RADEMS.342
          FWK=FWCOEF+FWC1/(1.+FWC2*U(I))                                         RADEMS.343
          RSUM(I)=EXP(-A(I)*(SQRT(U(I))+FWK*U(I)))                               RADEMS.344
          EMIS(I,1)=(1.-RSUM(I))*TERM3(I,1)                                      RADEMS.345
          TREM1(I)=RSUM(I)                                                       RADEMS.346
C                                                                                RADEMS.347
C EMIS(I,2)  1200 - 2200 CM-1   VIBRATION-ROTATION BAND                          RADEMS.348
C                                                                                RADEMS.349
          A41=1.75-3.96E-3*DTZ(I)                                                RADEMS.350
          A51=1.00+1.3*SQRT(U(I))                                                RADEMS.351
          A61=1.00+1.25E-3*DTP(I)+6.25E-5*DTP(I)*DTP(I)                          RADEMS.352
          CORFAC(I)=.3*(1.+(A41)/(A51))*A61                                      RADEMS.353
          T1T4=TERM1(I,3)*TERM4(I,3)                                             RADEMS.354
          T2T5=TERM2(I,3)*TERM5(I,3)                                             RADEMS.355
          A(I)=T1T4+T2T5/(1.+T2T5*SQRT(U(I))*CORFAC(I))                          RADEMS.356
          FWK=FWCOEF+FWC1/(1.+FWC2*U(I))                                         RADEMS.357
          RSUM(I)=EXP(-A(I)*(SQRT(U(I))+FWK*U(I)))                               RADEMS.358
          EMIS(I,2)=(1.-RSUM(I))*TERM3(I,3)                                      RADEMS.359
          TREM7(I)=RSUM(I)                                                       RADEMS.360
   50   CONTINUE                                                                 RADEMS.361
C                                                                                RADEMS.362
C LINE TRANSMISSION IN 800-1000 AND 1000-1200 CM-1 INTERVALS                     RADEMS.363
C                                                                                RADEMS.364
        DO 70 K=1,2                                                              RADEMS.365
          DO 60 I=1,IPLON                                                        RADEMS.366
            PHI=A1(K)*(DTY(I)+15.)+A2(K)*(DTY(I)+15.)**2                         RADEMS.367
            PSI=B1(K)*(DTY(I)+15.)+B2(K)*(DTY(I)+15.)**2                         RADEMS.368
            PHI=EXP(PHI)                                                         RADEMS.369
            PSI=EXP(PSI)                                                         RADEMS.370
            UBAR=W(I,K1)*PHI                                                     RADEMS.371
            UBAR=(UBAR*1.66)*R80257                                              RADEMS.372
            PBAR=PNEW(I)*(PSI/PHI)                                               RADEMS.373
            CF812=CFA1+((1.-CFA1)/(1.+UBAR*PBAR*10.))                            RADEMS.374
            G1=(REALK(K)*PBAR)/(2.*ST(K))                                        RADEMS.375
            G2=1.+(UBAR*4.0*ST(K)*CF812)/PBAR                                    RADEMS.376
            G3=SQRT(G2)-1.                                                       RADEMS.377
            G4=G1*G3                                                             RADEMS.378
            TRLINE(I,K)=EXP(-G4)                                                 RADEMS.379
   60     CONTINUE                                                               RADEMS.380
   70   CONTINUE                                                                 RADEMS.381
C                                                                                RADEMS.382
        DO 80 I=1,IPLON                                                          RADEMS.383
          TERM7(I,1)=COEFJ(1,1)+COEFJ(2,1)*DTY(I)*(1.+C16*DTY(I))                RADEMS.384
          TERM8(I,1)=COEFK(1,1)+COEFK(2,1)*DTY(I)*(1.+C17*DTY(I))                RADEMS.385
          TERM7(I,2)=COEFJ(1,2)+COEFJ(2,2)*DTY(I)*(1.+C26*DTY(I))                RADEMS.386
          TERM8(I,2)=COEFK(1,2)+COEFK(2,2)*DTY(I)*(1.+C27*DTY(I))                RADEMS.387
   80   CONTINUE                                                                 RADEMS.388
C                                                                                RADEMS.389
        DO 90 I=1,IPLON                                                          RADEMS.390
C                                                                                RADEMS.391
C EMIS(I,3)   800 - 1200 CM-1   WINDOW                                           RADEMS.392
C                                                                                RADEMS.393
          TERM6(I,1)=COEFF(1,1)+COEFF(2,1)*DTX(I)*(1.+C8*DTX(I)*(1.+C10*         RADEMS.394
     +               DTX(I)*(1.+C12*DTX(I)*(1.+C14*DTX(I)))))                    RADEMS.395
C                                                                                RADEMS.396
          TREM4(I)=EXP(-(COEFG(1,1)+COEFG(2,1)*DTX(I))*UC(I))*                   RADEMS.397
     +             TRLINE(I,2)                                                   RADEMS.398
          TREM6(I)=EXP(-(COEFG(1,2)+COEFG(2,2)*DTX(I))*UC(I))*                   RADEMS.399
     +             TRLINE(I,1)                                                   RADEMS.400
C                                                                                RADEMS.401
          EMIS(I,3)=TERM6(I,1)*(1.-.5*TREM4(I)-.5*TREM6(I))                      RADEMS.402
C                                                                                RADEMS.403
C EMIS(I,4)   500 -  800 CM-1   ROTATION BAND OVERLAP WITH CO2                   RADEMS.404
C                                                                                RADEMS.405
          K21(I)=TERM7(I,1)+TERM8(I,1)/(1.+(C30+C31*(DTY(I)-10.)*(DTY(I)         RADEMS.406
     +           -10.))*SQRT(U(I)))                                              RADEMS.407
          K22(I)=TERM7(I,2)+TERM8(I,2)/(1.+(C28+C29*(DTY(I)-10.))*               RADEMS.408
     +           SQRT(U(I)))                                                     RADEMS.409
          TERM9(I,1)=COEFI(1,1)+COEFI(2,1)*DTX(I)*(1.+C18*DTX(I)*(1.+C20         RADEMS.410
     +               *DTX(I)*(1.+C22*DTX(I)*(1.+C24*DTX(I)))))                   RADEMS.411
          FWK=FWCOEF+FWC1/(1.+FWC2*U(I))                                         RADEMS.412
          TR1(I)=EXP(-(K21(I)*(SQRT(U(I))+FC1*FWK*U(I))))                        RADEMS.413
          TR2(I)=EXP(-(K22(I)*(SQRT(U(I))+FC1*FWK*U(I))))                        RADEMS.414
          TR3(I)=EXP(-((COEFH(1,1)+COEFH(2,1)*DTX(I))*UC1(I)))                   RADEMS.415
          TR4(I)=EXP(-((COEFH(1,2)+COEFH(2,2)*DTX(I))*UC1(I)))                   RADEMS.416
          TR7(I)=TR1(I)*TR3(I)                                                   RADEMS.417
          TR8(I)=TR2(I)*TR4(I)                                                   RADEMS.418
          EMIS(I,4)=TERM9(I,1)*.5*(TR1(I)-TR7(I)+TR2(I)-TR8(I))                  RADEMS.419
          H2OEMS(I,K1)=EMIS(I,1)+EMIS(I,2)+EMIS(I,3)+EMIS(I,4)                   RADEMS.420
          TROCO2(I,K1)=0.65*TR7(I)+0.35*TR8(I)                                   RADEMS.421
          TREM2(I)=TROCO2(I,K1)                                                  RADEMS.422
   90   CONTINUE                                                                 RADEMS.423
C                                                                                RADEMS.424
C CO2 EMISSIVITY FOR 500-800 CM-1                                                RADEMS.425
C                                                                                RADEMS.426
        DO 150 I=1,IPLON                                                         RADEMS.427
C                                                                                RADEMS.428
          T1I=EXP(-480./CO2T(I,K1))                                              RADEMS.429
          SQTI=SQRT(CO2T(I,K1))                                                  RADEMS.430
          RSQTI=1./SQTI                                                          RADEMS.431
          ET=T1I                                                                 RADEMS.432
          ET2=ET*ET                                                              RADEMS.433
          ET4=ET2*ET2                                                            RADEMS.434
          OMET=1.-1.5*ET2                                                        RADEMS.435
          F1CO2=899.70*OMET*(1.+1.94774*ET+4.73486*ET2)*RSQTI                    RADEMS.436
          SQWP=SQRT(PLCO2(I,K1))                                                 RADEMS.437
          F1SQWP=F1CO2*SQWP                                                      RADEMS.438
          T1CO2=1./(1.+245.18*OMET*SQWP*RSQTI)                                   RADEMS.439
          ONEME=1.-ET2                                                           RADEMS.440
          ALPHAT=ONEME**3*RSQTI                                                  RADEMS.441
          WCO2=2.5221*CO2VMR*PNM(I,K1)*RGA                                       RADEMS.442
          U7=4.9411E4*ALPHAT*ET2*WCO2                                            RADEMS.443
          U8=3.9744E4*ALPHAT*ET4*WCO2                                            RADEMS.444
          U9=1.0447E5*ALPHAT*ET4*ET2*WCO2                                        RADEMS.445
          U13=2.8388E3*ALPHAT*ET4*WCO2                                           RADEMS.446
C                                                                                RADEMS.447
          TPATH=CO2T(I,K1)                                                       RADEMS.448
          TLOCAL=TPLNKE(I)                                                       RADEMS.449
          TCRFAC=SQRT((TLOCAL*R250)*(TPATH*R300))                                RADEMS.450
          PI=PNM(I,K1)*RSSLP+2.*DPFCO2*TCRFAC                                    RADEMS.451
          POSQT=PI/(2.*SQTI)                                                     RADEMS.452
          RBETA7=1./(5.3288*POSQT)                                               RADEMS.453
          RBETA8=1./(10.6576*POSQT)                                              RADEMS.454
          RBETA9=RBETA7                                                          RADEMS.455
          RBETA13=RBETA9                                                         RADEMS.456
          F2CO2=(U7/SQRT(4.+U7*(1.+RBETA7)))+(U8/SQRT(4.+U8*(1.+RBETA8))         RADEMS.457
     +          )+(U9/SQRT(4.+U9*(1.+RBETA9)))                                   RADEMS.458
          F3CO2=U13/SQRT(4.+U13*(1.+RBETA13))                                    RADEMS.459
          TMP1=ALOG(1.+F1SQWP)                                                   RADEMS.460
          TMP2=ALOG(1.+F2CO2)                                                    RADEMS.461
          TMP3=ALOG(1.+F3CO2)                                                    RADEMS.462
          ABSBND=(TMP1+2.*T1CO2*TMP2+2.*TMP3)*SQTI                               RADEMS.463
          CO2EMS(I,K1)=TROCO2(I,K1)*ABSBND*CO2PLK(I)                             RADEMS.464
          EX=EXP(960./TINT(I,K1))                                                RADEMS.465
          EXM1SQ=(EX-1.)**2                                                      RADEMS.466
          CO2EM(I,K1)=1.2E11*EX/(TINT(I,K1)*TINT4(I,K1)*EXM1SQ)                  RADEMS.467
          TREM3(I)=1.-BNDFCT*ABSBND                                              RADEMS.468
C                                                                                RADEMS.469
  150   CONTINUE                                                                 RADEMS.470
C                                                                                RADEMS.471
C O3 EMISSIVITY                                                                  RADEMS.472
C                                                                                RADEMS.473
        DO 220 I=1,IPLON                                                         RADEMS.474
          H2OTR(I,K1)=EXP(-12.*S2C(I,K1))                                        RADEMS.475
          TE=(CO2T(I,K1)/293.)**.7                                               RADEMS.476
          U1=18.29*PLOS(I,K1)/TE                                                 RADEMS.477
          U2=.5649*PLOS(I,K1)/TE                                                 RADEMS.478
          PHAT=PLOS(I,K1)/PLOL(I,K1)                                             RADEMS.479
          TLOCAL=TPLNKE(I)                                                       RADEMS.480
          TCRFAC=SQRT(TLOCAL/250.)*TE                                            RADEMS.481
          BETA=(1./.3205)*((1./PHAT)+(DPFO3*TCRFAC))                             RADEMS.482
          REALNU=(1./BETA)*TE                                                    RADEMS.483
          O3BNDI=74.*TE*(TPLNKE(I)/375.)*                                        RADEMS.484
     +           ALOG(1.+FO3(U1,REALNU)+FO3(U2,REALNU))                          RADEMS.485
          O3EMS(I,K1)=DBVTT(I)*H2OTR(I,K1)*O3BNDI                                RADEMS.486
          TREM5(I)=1.-(O3BNDI/(1060-980.))                                       RADEMS.487
  220   CONTINUE                                                                 RADEMS.488
C                                                                                RADEMS.489
  100 CONTINUE                                                                   RADEMS.490
C                                                                                RADEMS.491
C END OF INTERFACE LOOP                                                          RADEMS.492
C                                                                                RADEMS.493
C COMPUTE TOTAL EMISSIVITY:                                                      RADEMS.494
C                                                                                RADEMS.495
      DO 300 K1=1,IPLEVP                                                         RADEMS.496
        DO 310 I=1,IPLON                                                         RADEMS.497
          EMSTOT(I,K1)=H2OEMS(I,K1)+CO2EMS(I,K1)+O3EMS(I,K1)                     RADEMS.498
  310   CONTINUE                                                                 RADEMS.499
  300 CONTINUE                                                                   RADEMS.500
      RETURN                                                                     RADEMS.501
      END                                                                        RADEMS.502
                                                                                 RADEMS.503
