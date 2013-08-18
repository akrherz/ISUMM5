C-----------------------------------------------------------------------         RADCSW.1
C-----------------------------------------------------------------------         RADCSW.2
      SUBROUTINE RADCSW(COSZRS, PINT    ,H2OMMR  ,                               RADCSW.3
     $                  CLD ,CLWP , CIWP, CONWP, O3MMR  ,ECCF   ,                05DEC01.1424
     $                  SOLIN   ,QRS    ,FRSA    ,SABTP   ,CLRSS   ,             RADCSW.5
     $                  CLRST , KTAUW  , JSLC )                                  RADCSW.6
C SOLAR RADIATION CODE                                                           RADCSW.7
C                                                                                RADCSW.8
C COMPUTES INCIDENT SOLAR FLUX, SOLAR HEATING RATE, SURFACE ABSORBED             RADCSW.9
C SOLAR FLUX, AND TOTAL COLUMN ABSORBED SOLAR FLUX                               RADCSW.10
C                                                                                RADCSW.11
C USES THE DELTA-EDDINGTON METHOD                                                RADCSW.12
C                                                                                RADCSW.13
C DIVIDES SOLAR SPECTRUM INTO 18 INTERVALS FROM 0.2-5.0 MICRO-METERS.            RADCSW.14
C SOLAR FLUX FRACTIONS SPECIFIED FOR EACH INTERVAL. ALLOWS FOR SEASONALL         RADCSW.15
C AND DIURNALLY VARYING SOLAR INPUT.  INCLUDES MOLECULAR, CLOUD, AND             RADCSW.16
C SURFACE SCATTERING, ALONG WITH H2O,O3,CO2,O2,CLOUD, AND SURFACE                RADCSW.17
C ABSORPTION. COMPUTES DELTA-EDDINGTON REFLECTIONS AND TRANSMISSIONS             RADCSW.18
C ASSUMING HOMOGENEOUSLY MIXED LAYERS.  COMPUTES SURFACE ALBEDOS BY              RADCSW.19
C INVOKING RADALB. ADDS THE LAYERS ASSUMING SCATTERING BETWEEN LAYERS TO         RADCSW.20
C BE ISOTROPIC, AND DISTINGUISHES DIRECT SOLAR BEAM FROM SCATTERED               RADCSW.21
C RADIATION DOWN TO THE SURFACE.                                                 RADCSW.22
C                                                                                RADCSW.23
C LONGITUDE LOOPS ARE BROKEN INTO 1 OR 2 SECTIONS, SO THAT ONLY DAYLIGHT         RADCSW.24
C (I.E. COSZRS > 0) COMPUTATIONS ARE DONE.                                       RADCSW.25
C                                                                                RADCSW.26
C NOTE THAT AN EXTRAE LAYER ABOVE THE MODEL TOP LAYER IS ADDED.                  RADCSW.27
C                                                                                RADCSW.28
C CGS UNITS ARE USED.                                                            RADCSW.29
C                                                                                RADCSW.30
C SPECIAL DIAGNOSTIC CALCULATION OF THE CLEAR SKY SURFACE AND TOTAL COLU         RADCSW.31
C ABSORBED FLUX IS ALSO DONE; THIS CALCULATION DOES NOT EFFECT THE REST          RADCSW.32
C OF THE MODEL, BUT IS INCLUDED FOR CLOUD FORCING DIAGNOSTICS.                   RADCSW.33
C                                                                                RADCSW.34
C FOR MORE DETAILS, SEE  BRIEGLEB, B.P.   DELTA-EDDINGTON APPROXIMATION          RADCSW.35
C FOR SOLAR RADIATION IN THE NCAR COMMUNITY CLIMATE MODEL, SUBMITTED             RADCSW.36
C TO JOURNAL OF GEOPHYSICAL RESEARCH                                             RADCSW.37
C                                                                                RADCSW.38
C     INCLUDE 'PARAME'                                                           RADCSW.39
C     INCLUDE 'RADCCM2.DATA'                                                     RADCSW.40
C     INCLUDE 'BATS.CB2'                                                         RADCSW.41
#     include <parame.incl>                                                      RADCSW.42
#     include <param3.incl>                                                      RADCSW.43
#     include <parccm2.incl>                                                     RADCSW.44
#     include <radccm2.incl>                                                     RADCSW.45
#     include <various.incl>                                                     RADCSW.46
#     include <point2d.incl>                                                     RADCSW.47
C-----------------------------------------------------------------------         RADCSW.48
C                                                                                RADCSW.49
C INPUT ARGUMENTS                                                                RADCSW.50
C                                                                                RADCSW.51
      REAL COSZRS(IPLOND),       ! COS SOLAR ZENITH ANGLE                        RADCSW.52
     $   PINT(IPLOND,IPLEVP),    ! INTERFACE PRESSURE                            RADCSW.53
     $ H2OMMR(IPLOND,IPLEV),  ! SPECIFIC HUMIDITY (H2O MASS MIXING RA            RADCSW.54
     $     CLD(IPLOND,IPLEVP),     ! FRACTIONAL CLOUD COVER                      RADCSW.55
     $     CLWP(IPLOND,IPLEV),     ! LAYER LIQUID WATER PATH                     RADCSW.56
     $     CIWP(IPLOND,IPLEV),     ! LAYER ICE WATER PATH                        05DEC01.1425
     $     CONWP(IPLOND,IPLEV),    ! LAYER CONVECTIVE WATER PATH                 05DEC01.1426
     $     O3MMR(IPLOND,IPLEV),    ! OZONE MASS MIXING RATIO                     RADCSW.57
     $ ECCF                  ! ECCTRCTY FCTR (EARTH-SUN INV DSTNC SQ             RADCSW.58
C                                                                                RADCSW.59
C OUTPUT ARGUMENTS                                                               RADCSW.60
C                                                                                RADCSW.61
      REAL SOLIN(IPLOND),     ! INCIDENT SOLAR FLUX                              RADCSW.62
     $     QRS(IPLOND,IPLEV),  ! SOLAR HEATING RATE                              RADCSW.63
     $     FRSA(IPLOND),      ! SURFACE ABSORBED SOLAR FLUX                      RADCSW.64
     $     SABTP(IPLOND),     ! TOP OUTWARD SOLAR FLUX                           07NOV00.2174
     $     CLRSS(IPLOND),     ! CLEAR SKY SURFACE ABSORBED SOLAR FLUX            RADCSW.66
     $     CLRST(IPLOND)      ! CLEAR SKY TOTAL COLUMN ABSORBED SOLAR FL         RADCSW.67
C                                                                                RADCSW.68
C EXTERNALS                                                                      RADCSW.69
C                                                                                RADCSW.70
C      INTEGER   ISRCHFGT,    ! SEARCH FOR FIRST ARRAY ELEMENT > 0               RADCSW.71
C     $          ISRCHFLE     ! SEARCH FOR FIRST ARRAY ELEMENT < 0               RADCSW.72
C      EXTERNAL    RADDED,    ! COMPUTES DELTA-EDDINGTON SOLUTION                RADCSW.73
C     $            RADCLR,    ! COMPUTES CLEAR SKY DELTA-EDD SOLUTION            RADCSW.74
C      EXTERNAL  ISRCHFGT,    ! SEARCH FOR FIRST ARRAY ELEMENT > 0               RADCSW.75
C     $          ISRCHFLE     ! SEARCH FOR FIRST ARRAY ELEMENT < 0               RADCSW.76
C                                                                                RADCSW.77
C LOCAL ARRAYS AND CONSTANTS                                                     RADCSW.78
C                                                                                RADCSW.79
      INTEGER       NS,  ! SPECTRAL LOOP INDEX                                   RADCSW.80
     $               I,  ! LONGITUDE LOOP INDEX                                  RADCSW.81
     $               K,  ! LEVEL LOOP INDEX                                      RADCSW.82
     $               N,  ! LOOP INDEX FOR DAYLIGHT                               RADCSW.83
     $           NLOOP,  ! NUMBER OF DAYLIGHT LOOPS                              RADCSW.84
     $             ISN,  ! STARTING DAYLIGHT LONGITUDE INDEX                     RADCSW.85
     $             IEN,  ! ENDING   DAYLIGHT LONGITUDE INDEX                     RADCSW.86
     $           IS(2),  ! DAYTIME START INDICES                                 RADCSW.87
     $           IE(2),  ! DAYTIME END INDICES                                   RADCSW.88
     $          INDXSL   ! INDEX FOR CLOUD PARTICLE PROPERTIES                   RADCSW.89
C                                                                                RADCSW.90
      REAL SCON    ! SOLAR CONSTANT                                              RADCSW.91
      DATA SCON / 1.370E6 /                                                      RADCSW.92
C                                                                                RADCSW.93
C A. SLINGO`S DATA FOR CLOUD PARTICLE RADIATIVE PROPERTIES                       RADCSW.94
C (FROM `A GCM PARAMETERIZATION FOR THE SHORTWAVE PROPERTIES OF                  RADCSW.95
C WATER CLOUDS` JAS VOL. 46 MAY 1989  PP 1419-1427)                              RADCSW.96
C                                                                                RADCSW.97
      REAL ABAR(4), ! A COEFFICIENT FOR EXTINCTION OPTICAL DEPTH                 RADCSW.98
     $     BBAR(4), ! B COEFFICIANT FOR EXTINCTION OPTICAL DEPTH                 RADCSW.99
     $     CBAR(4), ! C COEFFICIANT FOR SINGLE PARTICLE SCAT ALBEDO              RADCSW.100
     $     DBAR(4), ! D COEFFICIANT FOR SINGLE PARTICLE SCAT ALBEDO              RADCSW.101
     $     EBAR(4), ! E COEFFICIANT FOR ASYMMETRY PARAMETER                      RADCSW.102
     $     FBAR(4)  ! F COEFFICIANT FOR ASYMMETRY PARAMETER                      RADCSW.103
C                                                                                RADCSW.104
      DATA ABAR/ 2.817E-02, 2.682E-02,2.264E-02,1.281E-02/                       RADCSW.105
      DATA BBAR/ 1.305    , 1.346    ,1.454    ,1.641    /                       RADCSW.106
      DATA CBAR/-5.62E-08 ,-6.94E-06 ,4.64E-04 ,0.201    /                       RADCSW.107
      DATA DBAR/ 1.63E-07 , 2.35E-05 ,1.24E-03 ,7.56E-03 /                       RADCSW.108
      DATA EBAR/ 0.829    , 0.794    ,0.754    ,0.826    /                       RADCSW.109
      DATA FBAR/ 2.482E-03, 4.226E-03,6.560E-03,4.353E-03/                       RADCSW.110
C                                                                                RADCSW.111
      REAL ABARI,   ! A COEFFICIANT FOR CURRENT SPECTRAL INTERVAL                RADCSW.112
     $     BBARI,   ! B COEFFICIANT FOR CURRENT SPECTRAL INTERVAL                RADCSW.113
     $     CBARI,   ! C COEFFICIANT FOR CURRENT SPECTRAL INTERVAL                RADCSW.114
     $     DBARI,   ! D COEFFICIANT FOR CURRENT SPECTRAL INTERVAL                RADCSW.115
     $     EBARI,   ! E COEFFICIANT FOR CURRENT SPECTRAL INTERVAL                RADCSW.116
     $     FBARI    ! F COEFFICIANT FOR CURRENT SPECTRAL INTERVAL                RADCSW.117
C                                                                                RADCSW.118
C CAUTION... A. SLINGO RECOMMENDS NO LESS THAN 4.0 MICRO-METERS                  RADCSW.119
C NOR GREATER THAN 20 MICRO-METERS                                               RADCSW.120
C                                                                                RADCSW.121
C Ice water coefficients (Ebert and Curry, 1992, JGR, 97, 3831-3836)             05DEC01.1427
      REAL ABRIC(4), ! A COEFFICIENT FOR EXTINCTION OPTICAL DEPTH                05DEC01.1428
     $     BBRIC(4), ! B COEFFICIANT FOR EXTINCTION OPTICAL DEPTH                05DEC01.1429
     $     CBRIC(4), ! C COEFFICIANT FOR SINGLE PARTICLE SCAT ALBEDO             05DEC01.1430
     $     DBRIC(4), ! D COEFFICIANT FOR SINGLE PARTICLE SCAT ALBEDO             05DEC01.1431
     $     EBRIC(4), ! E COEFFICIANT FOR ASYMMETRY PARAMETER                     05DEC01.1432
     $     FBRIC(4)  ! F COEFFICIANT FOR ASYMMETRY PARAMETER                     05DEC01.1433
C                                                                                05DEC01.1434
      DATA ABRIC/ 3.448E-03, 3.448E-03,3.448E-03,3.448E-03/                      05DEC01.1435
      DATA BBRIC/ 2.431    , 2.431    ,2.431    ,2.431    /                      05DEC01.1436
      DATA CBRIC/ 1.00E-05 , 1.10E-04 ,1.861E-02,0.46658  /                      05DEC01.1437
      DATA DBRIC/ 0.0      , 1.405E-05,8.328E-04,2.05E-05 /                      05DEC01.1438
      DATA EBRIC/ 0.7661   , 0.7730   ,0.794    ,0.9595   /                      05DEC01.1439
      DATA FBRIC/ 5.851E-04, 5.665E-04,7.267E-04,1.076E-04/                      05DEC01.1440
C                                                                                05DEC01.1441
      REAL ABRICI,   ! A COEFFICIANT FOR CURRENT SPECTRAL INTERVAL               05DEC01.1442
     $     BBRICI,   ! B COEFFICIANT FOR CURRENT SPECTRAL INTERVAL               05DEC01.1443
     $     CBRICI,   ! C COEFFICIANT FOR CURRENT SPECTRAL INTERVAL               05DEC01.1444
     $     DBRICI,   ! D COEFFICIANT FOR CURRENT SPECTRAL INTERVAL               05DEC01.1445
     $     EBRICI,   ! E COEFFICIANT FOR CURRENT SPECTRAL INTERVAL               05DEC01.1446
     $     FBRICI    ! F COEFFICIANT FOR CURRENT SPECTRAL INTERVAL               05DEC01.1447
C                                                                                05DEC01.1448
      REAL CLDEFR      !  UNIVERSAL CLOUD EFFECTIVE RADIUS IN MICRO-METE         RADCSW.122
      DATA CLDEFR / 10.0 /                                                       RADCSW.123
C                                                                                RADCSW.124
      REAL DELTA  ! PRESSURE (ATMOSPHERES) FOR STRATOSPHERIC H2O LIMIT:          RADCSW.125
      DATA DELTA  /  1.70E-3 /                                                   RADCSW.126
C                                                                                RADCSW.127
      REAL O2MMR  ! O2 MASS MIXING RATIO:                                        RADCSW.128
      DATA O2MMR / .23143 /                                                      RADCSW.129
C                                                                                RADCSW.130
C CO2 INFO:                                                                      RADCSW.131
C                                                                                RADCSW.132
      REAL MMWAIR,   ! MEAN MOLECULAR WEIGHT OF AIR                              RADCSW.133
     $     MMWCO2,   ! MEAN MOLECULAR WEIGHT OF CO2                              RADCSW.134
     $     CO2MMR    ! CO2 MASS MIXING RATIO                                     RADCSW.135
      DATA MMWAIR / 28.9644 /                                                    RADCSW.136
      DATA MMWCO2 / 44.0000 /                                                    RADCSW.137
C                                                                                RADCSW.138
C      REAL ALBDIR(IPLOND),    ! CURRENT SPC INTRVL SRF ALB TO DIRECT RA         RADCSW.139
C     $     ALBDIF(IPLOND)     ! CURRENT SPC INTRVL SRF ALB TO DIFFUSE R         RADCSW.140
       REAL ALBDIR(IPLOND), ALBDIF(IPLOND)                                       RADCSW.141
C                                                                                RADCSW.142
C      REAL ALBS(IPLOND),      ! 0.2-0.7 MICRO-METER SRFC ALBEDO TO DIRE         RADCSW.143
C     $     ALBL(IPLOND),      ! 0.7-5.0 MICRO-METER SRFC ALBEDO TO DIRE         RADCSW.144
C     $     ALBSD(IPLOND),     ! 0.2-0.7 MICRO-METER SRFC ALBEDO TO DIFF         RADCSW.145
C     $     ALBLD(IPLOND)      ! 0.7-5.0 MICRO-METER SRFC ALBEDO TO DIFF         RADCSW.146
C                                                                                RADCSW.147
      INTEGER NSPINT  ! NUMBER OF SPECTRAL INTERVALS ACROSS SOLAR SPECTR         RADCSW.148
      PARAMETER ( NSPINT = 18 )                                                  RADCSW.149
C                                                                                RADCSW.150
C NEXT SERIES DEPENDS ON SPECTRAL INTERVAL                                       RADCSW.151
C                                                                                RADCSW.152
      REAL FRCSOL(NSPINT),      ! FRACTION OF SOLAR FLUX IN EACH SPEC IN         RADCSW.153
     $     WAVMIN(NSPINT),      ! MIN WAVELENGTH (MICRO-METERS) OF INTER         RADCSW.154
     $     WAVMAX(NSPINT),      ! MAX WAVELENGTH (MICRO-METERS) OF INTER         RADCSW.155
     $     RAYTAU(NSPINT),      ! RAYLEIGH SCATTERING OPTICAL DEPTH              RADCSW.156
     $     ABH2O(NSPINT),       ! ABSORPTION COEFFICIANT FOR H2O (CM2/G)         RADCSW.157
     $     ABO3 (NSPINT),       ! ABSORPTION COEFFICIANT FOR O3    " "           RADCSW.158
     $     ABCO2(NSPINT),       ! ABSORPTION COEFFICIANT FOR CO2   " "           RADCSW.159
     $     ABO2 (NSPINT),       ! ABSORPTION COEFFICIANT FOR O2    " "           RADCSW.160
     $     PH2O(NSPINT),        ! WEIGHT OF H2O IN SPECTRAL INTERVAL             RADCSW.161
     $     PO3 (NSPINT),        ! WEIGHT OF O3  IN SPECTRAL INTERVAL             RADCSW.162
     $     PCO2(NSPINT),        ! WEIGHT OF CO2 IN SPECTRAL INTERVAL             RADCSW.163
     $     PO2 (NSPINT)         ! WEIGHT OF O2  IN SPECTRAL INTERVAL             RADCSW.164
C                                                                                RADCSW.165
      DATA FRCSOL / .001488, .001389, .001290, .001686, .002877,                 RADCSW.166
     $              .003869, .026336, .426131, .526861, .526861,                 RADCSW.167
     $              .526861, .526861, .526861, .526861, .526861,                 RADCSW.168
     $              .006239, .001834, .001834/                                   RADCSW.169
C                                                                                RADCSW.170
      DATA WAVMIN / .200,  .245,  .265,  .275,  .285,                            RADCSW.171
     $              .295,  .305,  .350,  .700,  .701,                            RADCSW.172
     $              .701,  .701,  .701,  .702,  .702,                            RADCSW.173
     $             2.630, 4.160, 4.160/                                          RADCSW.174
      DATA WAVMAX / .245,  .265,  .275,  .285,  .295,                            RADCSW.175
     $              .305,  .350,  .700, 5.000, 5.000,                            RADCSW.176
     $             5.000, 5.000, 5.000, 5.000, 5.000,                            RADCSW.177
     $             2.860, 4.550, 4.550/                                          RADCSW.178
C                                                                                RADCSW.179
      DATA RAYTAU / 4.020, 2.180, 1.700, 1.450, 1.250,                           RADCSW.180
     $              1.085, 0.730, 0.135, 0.020, .0001,                           RADCSW.181
     $              .0001, .0001, .0001, .0001, .0001,                           RADCSW.182
     $              .0001, .0001, .0001/                                         RADCSW.183
C                                                                                RADCSW.184
C ABSORPTION COEFFICIANTS                                                        RADCSW.185
C                                                                                RADCSW.186
      DATA ABH2O /    .000,     .000,    .000,    .000,    .000,                 RADCSW.187
     $                .000,     .000,    .000,    .002,    .035,                 RADCSW.188
     $                .377,    1.950,   9.400,  44.600, 190.000,                 RADCSW.189
     $                .000,     .000,    .000/                                   RADCSW.190
C                                                                                RADCSW.191
      DATA ABO3  /                                                               RADCSW.192
     $ 5.370E+04, 13.080E+04,  9.292E+04, 4.530E+04, 1.616E+04,                  RADCSW.193
     $ 4.441E+03,  1.775E+02,  2.101E+01,      .000,      .000,                  RADCSW.194
     $  .000    ,   .000    ,   .000    ,      .000,      .000,                  RADCSW.195
     $  .000    ,   .000    ,   .000    /                                        RADCSW.196
C                                                                                RADCSW.197
      DATA ABCO2  /    .000,     .000,    .000,    .000,    .000,                RADCSW.198
     $                 .000,     .000,    .000,    .000,    .000,                RADCSW.199
     $                 .000,     .000,    .000,    .000,    .000,                RADCSW.200
     $                 .094,     .196,   1.963/                                  RADCSW.201
C                                                                                RADCSW.202
      DATA ABO2  /    .000,     .000,    .000,    .000,    .000,                 RADCSW.203
     $                .000,     .000,1.11E-05,6.69E-05,    .000,                 RADCSW.204
     $                .000,     .000,    .000,    .000,    .000,                 RADCSW.205
     $                .000,    .000,    .000/                                    RADCSW.206
C                                                                                RADCSW.207
C SPECTRAL INTERVAL WEIGHTS                                                      RADCSW.208
C                                                                                RADCSW.209
      DATA PH2O  /    .000,     .000,    .000,    .000,    .000,                 RADCSW.210
     $                .000,     .000,    .000,    .505,    .210,                 RADCSW.211
     $                .120,     .070,    .048,    .029,    .018,                 RADCSW.212
     $                .000,     .000,    .000/                                   RADCSW.213
      DATA PO3   /   1.000,    1.000,   1.000,   1.000,   1.000,                 RADCSW.214
     $               1.000,    1.000,   1.000,    .000,    .000,                 RADCSW.215
     $                .000,     .000,    .000,    .000,    .000,                 RADCSW.216
     $                .000,     .000,    .000/                                   RADCSW.217
      DATA PCO2  /    .000,     .000,    .000,    .000,    .000,                 RADCSW.218
     $                .000,     .000,    .000,    .000,    .000,                 RADCSW.219
     $                .000,     .000,    .000,    .000,    .000,                 RADCSW.220
     $               1.000,     .640,    .360/                                   RADCSW.221
      DATA PO2   /    .000,     .000,    .000,    .000,    .000,                 RADCSW.222
     $                .000,     .000,   1.000,   1.000,    .000,                 RADCSW.223
     $                .000,     .000,    .000,    .000,    .000,                 RADCSW.224
     $                .000,     .000,    .000/                                   RADCSW.225
C                                                                                RADCSW.226
C DIAGNOSTIC AND ACCUMULATION ARRAYS; NOTE THAT SFLTOT, FSWUP, AND               RADCSW.227
C FSWDN ARE NOT USED IN THE COMPUTATION, BUT ARE RETAINED FOR FUTURE USE         RADCSW.228
C                                                                                RADCSW.229
      REAL SOLFLX(IPLOND),         ! SOLAR FLUX IN CURRENT INTERVAL              RADCSW.230
     $     SFLTOT(IPLOND),         ! SPECTRALLY SUMMED TOTAL SOLAR FLUX          RADCSW.231
C    $     SOLIS(IPLOND),          ! INCIDENT VISIBLE SOLAR RADIATION            RADCSW.232
     $     TOTFLD(IPLOND,0:IPLEV),  ! SPECTRALLY SUMMED FLUX DIVERGENCE          RADCSW.233
     $     FSWUP(IPLOND,0:IPLEVP),  ! SPECTRALLY SUMMED UP FLUX                  RADCSW.234
     $     FSWDN(IPLOND,0:IPLEVP)   ! SPECTRALLY SUMMED DOWN FLUX                RADCSW.235
C                                                                                RADCSW.236
C CLOUD RADIATIVE PROPERTY ARRAYS                                                RADCSW.237
C                                                                                RADCSW.238
      REAL TAUEXC(IPLOND,0:IPLEV),     ! CLOUD EXTINCTION OPTICAL DEPTH          RADCSW.239
     $         WC(IPLOND,0:IPLEV),     ! CLOUD SINGLE SCATTERING ALBEDO          RADCSW.240
     $         GC(IPLOND,0:IPLEV),     ! CLOUD ASSYMETRY PARAMETER               RADCSW.241
     $         FC(IPLOND,0:IPLEV),     ! CLOUD FORWARD SCATTERED FRACTIO         RADCSW.242
     $         CRE(IPLEV),            ! CLOUD EFFECTIVE RADIUS (MICRO-ME         RADCSW.243
     $         RCRE(IPLEV)            ! INVERSE OF CLOUD EFFECTIVE RADIU         RADCSW.244
      REAL TAUEXI(IPLOND,0:IPLEV),     ! CLOUD EXT OPT DEPTH - ICE               05DEC01.1449
     $         WCI(IPLOND,0:IPLEV),    ! CLOUD SINGLE SCAT ALB - ICE             05DEC01.1450
     $         GCI(IPLOND,0:IPLEV),    ! CLOUD ASSYMETRY PARAM - ICE             05DEC01.1451
     $         FCI(IPLOND,0:IPLEV),    ! CLOUD FORWARD SCAT FRAC - ICE           05DEC01.1452
     $         CREI(IPLEV),            ! CLOUD EFF RAD - ICE                     05DEC01.1453
     $         RCREI(IPLEV)            ! INVERSE CREI - ICE                      05DEC01.1454
C                                                                                RADCSW.245
C VARIOUS ARRAYS AND OTHER CONSTANTS:                                            RADCSW.246
C                                                                                RADCSW.247
      REAL PFLX(IPLOND,0:IPLEVP), ! INTERFACE PRESSURE, INCLUDING EXTRAE         RADCSW.248
     $     ZENFAC(IPLOND),       ! SQUARE ROOT OF COSINE SOLAR ZENITH AN         RADCSW.249
     $     SQRCO2,              ! SQUARE ROOT OF THE CO2 MASS MIXING RAT         RADCSW.250
     $     TMP1,                ! TEMPORARY CONSTANT ARRAY                       RADCSW.251
     $     TMP2,                ! TEMPORARY CONSTANT ARRAY                       RADCSW.252
     $     TMP3,                ! TEMPORARY CONSTANT ARRAY                       RADCSW.253
     $     TMP1I,               ! TEMPORARY CONSTANT ARRAY                       05DEC01.1455
     $     TMP2I,               ! TEMPORARY CONSTANT ARRAY                       05DEC01.1456
     $     TMP3I,               ! TEMPORARY CONSTANT ARRAY                       05DEC01.1457
     $     PDEL,                ! PRESSURE DIFFERENCE ACROSS LAYER               RADCSW.254
     $     PATH,                ! MASS PATH OF LAYER                             RADCSW.255
     $     PTOPR,                ! LOWER INTERFACE PRESSURE OF EXTRAE LA         RADCSW.256
     $     PTHO2,               ! USED TO COMPUTE MASS PATH OF O2                RADCSW.257
     $     PTHO3,               ! USED TO COMPUTE MASS PATH OF O3                RADCSW.258
     $     PTHCO2,              ! USED TO COMPUTE MASS PATH OF CO2               RADCSW.259
     $     PTHH2O,              ! USED TO COMPUTE MASS PATH OF H2O               RADCSW.260
     $     H2OSTR,              ! INVERSE SQUARE ROOT H2O MASS MIXING RA         RADCSW.261
     $     WAVMID,              ! SPECTRAL INTERVAL MIDDLE WAVELENGTH            RADCSW.262
     $     TRAYOSLP             ! RAYLEIGH OPT DPTH OVER STANDARD PRESSU         RADCSW.263
C                                                                                RADCSW.264
      REAL RDENOM,              ! MULTIPLE SCATTERING TERM                       RADCSW.265
     $     PSF,                 ! FRACTION OF SOLAR FLUX IN SPECTRAL INT         RADCSW.266
     $     GOCP                 ! GRAV ACC OVER HEAT CAPACITY CONSTANT P         RADCSW.267
C                                                                                RADCSW.268
C LAYER ABSORBER AMOUNTS; NOTE THAT 0 REFERS TO THE EXTRAE LAYER                 RADCSW.269
C ADDED ABOVE THE TOP MODEL LAYER                                                RADCSW.270
C                                                                                RADCSW.271
      REAL UH2O(IPLOND,0:IPLEV), ! LAYER ABSORBER AMOUNT OF H2O                  RADCSW.272
     $      UO3(IPLOND,0:IPLEV), ! LAYER ABSORBER AMOUNT OF  O3                  RADCSW.273
     $     UCO2(IPLOND,0:IPLEV), ! LAYER ABSORBER AMOUNT OF CO2                  RADCSW.274
     $      UO2(IPLOND,0:IPLEV)  ! LAYER ABSORBER AMOUNT OF  O2                  RADCSW.275
C                                                                                RADCSW.276
C TOTAL COLUMN ABSORBER AMOUNTS:                                                 RADCSW.277
C                                                                                RADCSW.278
      REAL UTH2O(IPLOND),  ! TOTAL COLUMN  ABSORBER AMOUNT OF  H2O               RADCSW.279
     $     UTO3(IPLOND),   ! TOTAL COLUMN  ABSORBER AMOUNT OF  O3                RADCSW.280
     $     UTCO2(IPLOND),  ! TOTAL COLUMN  ABSORBER AMOUNT OF  CO2               RADCSW.281
     $     UTO2(IPLOND)    ! TOTAL COLUMN  ABSORBER AMOUNT OF  O2                RADCSW.282
C                                                                                RADCSW.283
C THESE ARRAYS ARE DEFINED FOR IPLEV MODEL LAYERS; 0 REFERS TO THE               RADCSW.284
C EXTRAE LAYER ON TOP:                                                           RADCSW.285
C                                                                                RADCSW.286
      REAL RDIR(IPLOND,0:IPLEV),   ! LAYER REFLECTIVITY TO DIRECT RADIAT         RADCSW.287
     $     RDIF(IPLOND,0:IPLEV),   ! LAYER REFLECTIVITY TO DIFFUSE RADIA         RADCSW.288
     $     TDIR(IPLOND,0:IPLEV),   ! LAYER TRANSMISSION TO DIRECT RADIAT         RADCSW.289
     $     TDIF(IPLOND,0:IPLEV),   ! LAYER TRANSMISSION TO DIFFUSE RADIA         RADCSW.290
     $     EXPLAY(IPLOND,0:IPLEV), ! SOLAR BEAM EXP TRANSMISSION FOR LAY         RADCSW.291
     $     FLXDIV(IPLOND,0:IPLEV)  ! FLUX DIVERGENCE FOR LAYER                   RADCSW.292
C                                                                                RADCSW.293
C THESE ARRAYS ARE DEFINED AT MODEL INTERFACES; 0 IS THE TOP OF THE              RADCSW.294
C EXTRAE LAYER ABOVE THE MODEL TOP; IPLEVP IS THE EARTH SURFACE:                 RADCSW.295
C                                                                                RADCSW.296
      REAL RUPDIR(IPLOND,0:IPLEVP), ! REF TO DIR RADIATION FOR LAYERS BE         RADCSW.297
     $     RUPDIF(IPLOND,0:IPLEVP), ! REF TO DIF RADIATION FOR LAYERS BE         RADCSW.298
     $     RDNDIF(IPLOND,0:IPLEVP), ! REF TO DIF RADIATION FOR LAYERS AB         RADCSW.299
     $     EXPTDN(IPLOND,0:IPLEVP), ! SOLAR BEAM EXP DOWN TRANSMISSION F         RADCSW.300
     $     TOTTRN(IPLOND,0:IPLEVP), ! TOTAL TRANSMISSION FOR LAYERS ABOV         RADCSW.301
     $     FLUXUP(IPLOND,0:IPLEVP), ! UP   FLUX AT MODEL INTERFACE               RADCSW.302
     $     FLUXDN(IPLOND,0:IPLEVP)  ! DOWN FLUX AT MODEL INTERFACE               RADCSW.303
C                                                                                RADCSW.304
#ifdef MPP1                                                                      RADCSW.305
#   define IPLON I  LX                                                           RADCSW.306
#   define IPLOND I  LX                                                          RADCSW.307
#endif                                                                           RADCSW.308
C                                                                                RADCSW.309
C                                                                                RADCSW.310
C SET CLOUD EFFECTIVE RADIUS IN MICROMETERS:                                     RADCSW.311
C CHANGED FROM 14.6 TO 35 MICRONS FOR 3.7                                        23DEC04.3889
C                                                                                RADCSW.312
      DO 8 K=1,IPLEV                                                             RADCSW.313
        CRE(K)=CLDEFR                                                            RADCSW.314
        CREI(K)=35.0                                                             23DEC04.3890
        RCRE(K)=1./CRE(K)                                                        RADCSW.315
        RCREI(K)=1./CREI(K)                                                      05DEC01.1459
    8 CONTINUE                                                                   RADCSW.316
C                                                                                RADCSW.317
C INITIALIZE OUTPUT FIELDS:                                                      RADCSW.318
C                                                                                RADCSW.319
      DO 13 I=1,IPLON                                                            RADCSW.320
        SABTP(I)=0.0                                                             RADCSW.321
        FRSA(I)=0.0                                                              RADCSW.322
        SOLIN(I)=0.0                                                             RADCSW.323
        CLRSS(I)=0.0                                                             RADCSW.324
        CLRST(I)=0.0                                                             RADCSW.325
        TAUEXC(I,0)=0.0                                                          05DEC01.1460
        TAUEXI(I,0)=0.0                                                          05DEC01.1461
C        SABVEG(I) = 0.0                                                         RADCSW.326
C        SOLIS(I)=0.0                                                            RADCSW.327
   13 CONTINUE                                                                   RADCSW.328
      DO 15 K=1,IPLEV                                                            RADCSW.329
        DO 20 I=1,IPLON                                                          RADCSW.330
          QRS(I,K)=0.0                                                           RADCSW.331
          TAUEXC(I,K)=0.0                                                        05DEC01.1462
          TAUEXI(I,K)=0.0                                                        05DEC01.1463
   20   CONTINUE                                                                 RADCSW.332
   15 CONTINUE                                                                   RADCSW.333
C                                                                                RADCSW.334
C COMPUTE STARTING, ENDING DAYTIME LOOP INDICES:                                 RADCSW.335
C                                                                                RADCSW.336
      NLOOP=0                                                                    RADCSW.337
                                                                                 RADCSW.338
#ifndef MPP1                                                                     RADCSW.339
      IS(1)=IPLON+1                                                              RADCSW.340
      IE(1)=IPLON                                                                RADCSW.341
      DO 21 I=1,IPLON                                                            RADCSW.342
        IF(COSZRS(I).GT.0.)THEN                                                  RADCSW.343
          IS(1)=I                                                                RADCSW.344
          GOTO 22                                                                RADCSW.345
        ENDIF                                                                    RADCSW.346
   21 CONTINUE                                                                   RADCSW.347
   22 CONTINUE                                                                   RADCSW.348
#else                                                                            RADCSW.349
      IS(1)=ILX+1                                                                RADCSW.350
      IE(1)=ILX                                                                  RADCSW.351
      DO 21 I=1,ILX                                                              RADCSW.352
        IF(COSZRS(I).GT.0.)THEN                                                  RADCSW.353
          IS(1)=FLIC_L2G_M(I)                                                    RADCSW.354
          GOTO 22                                                                RADCSW.355
        ENDIF                                                                    RADCSW.356
   21 CONTINUE                                                                   RADCSW.357
   22 CONTINUE                                                                   RADCSW.358
#endif                                                                           RADCSW.359
                                                                                 RADCSW.360
C      IS(1) = ISRCHFGT(IPLON,COSZRS,1,0.0)                                      RADCSW.361
C                                                                                RADCSW.362
C IF NIGHT EVERYWHERE, RETURN:                                                   RADCSW.363
C                                                                                RADCSW.364
#ifndef MPP1                                                                     RADCSW.365
      IF(IS(1).EQ.IPLON+1)RETURN                                                 RADCSW.366
      DO 23 I=IS(1),IPLON                                                        RADCSW.367
        IF(COSZRS(I).LE.0.)THEN                                                  RADCSW.368
          IE(1)=I-1                                                              RADCSW.369
          GOTO 24                                                                RADCSW.370
        ENDIF                                                                    RADCSW.371
   23 CONTINUE                                                                   RADCSW.372
   24 CONTINUE                                                                   RADCSW.373
#else                                                                            RADCSW.374
      IF(IS(1).GE.IPLON+1)RETURN                                                 RADCSW.375
      DO 23 I=IS(1),IPLON                                                        RADCSW.376
        IF(COSZRS(I).LE.0.)THEN                                                  RADCSW.377
          IE(1)=FLIC_L2G_M(I-1)                                                  RADCSW.378
          GOTO 24                                                                RADCSW.379
        ENDIF                                                                    RADCSW.380
   23 CONTINUE                                                                   RADCSW.381
   24 CONTINUE                                                                   RADCSW.382
#endif                                                                           RADCSW.383
C     IE(1) = ISRCHFLE(IPLON-IS(1),COSZRS(IS(1)+1),1,0.0) + IS(1) - 1            RADCSW.384
                                                                                 RADCSW.385
                                                                                 RADCSW.386
      NLOOP=1                                                                    RADCSW.387
C                                                                                RADCSW.388
C POSSIBLY 2 DAYTIME LOOPS NEEDED:                                               RADCSW.389
C                                                                                RADCSW.390
      IF(IE(1).NE.IPLON)THEN                                                     RADCSW.391
        IS(2)=IPLON+1                                                            RADCSW.392
        IE(2)=IPLON                                                              RADCSW.393
        DO 25 I=IE(1)+1,IPLON                                                    RADCSW.394
          IF(COSZRS(I).GT.0.)THEN                                                RADCSW.395
#ifndef MPP1                                                                     RADCSW.396
            IS(2)=I                                                              RADCSW.397
#else                                                                            RADCSW.398
            IS(2)=FLIC_L2G_M(I)                                                  RADCSW.399
#endif                                                                           RADCSW.400
            GOTO 26                                                              RADCSW.401
          ENDIF                                                                  RADCSW.402
   25   CONTINUE                                                                 RADCSW.403
   26   CONTINUE                                                                 RADCSW.404
C        IS(2) = ISRCHFGT(IPLON-IE(1),COSZRS(IE(1)+1),1,0.0) + IE(1)             RADCSW.405
        IF(IS(2).LE.IPLON)THEN                                                   RADCSW.406
          NLOOP=2                                                                RADCSW.407
          IE(2)=IPLON                                                            RADCSW.408
        ENDIF                                                                    RADCSW.409
      ENDIF                                                                      RADCSW.410
      IF(KTAU.EQ.80.OR.KTAU.EQ.140.OR.KTAU.EQ.200.OR.KTAU.EQ.260.OR.KTAU         RADCSW.411
     +  .EQ.320)THEN                                                             RADCSW.412
C      PRINT *,'SUN PARAMETERS= ',KTAU,NLOOP,IS(1),IE(1),IS(2),IE(2)             RADCSW.413
C      PRINT *,'COSZRS = ', COSZRS                                               RADCSW.414
      ENDIF                                                                      RADCSW.415
#ifdef MPP1                                                                      RADCSW.416
C THIS MAY NEED MODIFICATION AT SOME POINT BUT FOR A REGIONAL MODEL              RADCSW.417
C SCANNING THROUGH RADIATION IN THE NS DIMENSION, CASES WHERE THERE ARE          RADCSW.418
C MORE THAN ONE VECTOR OF CONTIGUOUS DAYLIGHT POINTS IN A SWEEP WOULD            RADCSW.419
C BE UNLIKELY.  THIS LOGIC WAS ORIGINALLY INCLUDED FOR THE EW SWEEP              RADCSW.420
C OVER POINTS IN THE GLOBAL CCM MODEL.                                           RADCSW.421
      NLOOP = 1                                                                  RADCSW.422
      IS(1) = 1                                                                  RADCSW.423
      IE(1) = ILX                                                                RADCSW.424
#endif                                                                           RADCSW.425
C                                                                                RADCSW.426
C DEFINE SOLAR INCIDENT RADIATION AND INTERFACE PRESSURES:                       RADCSW.427
C                                                                                RADCSW.428
      DO 27 N=1,NLOOP                                                            RADCSW.429
        DO 28 I=IS(N),IE(N)                                                      RADCSW.430
C***          SOLIN(I)=SCON*ECCF*COSZRS(I)                                       07NOV00.2175
C***    USE SOLAR CONSTANT FROM SOLAR1 ROUTINE                                   07NOV00.2176
          SOLIN(I)=SOLCON*1.E3*COSZRS(I)                                         07NOV00.2177
          PFLX(I,0)=0.                                                           RADCSW.432
   28   CONTINUE                                                                 RADCSW.433
   27 CONTINUE                                                                   RADCSW.434
      DO 33 K=1,IPLEVP                                                           RADCSW.435
        DO 34 N=1,NLOOP                                                          RADCSW.436
          DO 36 I=IS(N),IE(N)                                                    RADCSW.437
            PFLX(I,K)=PINT(I,K)                                                  RADCSW.438
   36     CONTINUE                                                               RADCSW.439
   34   CONTINUE                                                                 RADCSW.440
   33 CONTINUE                                                                   RADCSW.441
C                                                                                RADCSW.442
C COMPUTE OPTICAL PATHS:                                                         RADCSW.443
C                                                                                RADCSW.444
      TMP1=0.5/(GRAVIT*SSLP)                                                     RADCSW.445
      CO2MMR=CO2VMR*(MMWCO2/MMWAIR)                                              RADCSW.446
      SQRCO2=SQRT(CO2MMR)                                                        RADCSW.447
      DO 38 N=1,NLOOP                                                            RADCSW.448
        DO 40 I=IS(N),IE(N)                                                      RADCSW.449
          PTOPR=PFLX(I,1)                                                        RADCSW.450
          PTHO2=O2MMR*PTOPR/GRAVIT                                               RADCSW.451
          PTHO3=O3MMR(I,1)*PTOPR/GRAVIT                                          RADCSW.452
          PTHCO2=SQRCO2*(PTOPR/GRAVIT)                                           RADCSW.453
C                                                                                RADCSW.454
          H2OSTR=SQRT(1./H2OMMR(I,1))                                            RADCSW.455
          ZENFAC(I)=SQRT(COSZRS(I))                                              RADCSW.456
          PTHH2O=PTOPR**2*TMP1+(PTOPR*RGA)*(H2OSTR*ZENFAC(I)*DELTA)              RADCSW.457
C                                                                                RADCSW.458
          UH2O(I,0)=H2OMMR(I,1)*PTHH2O                                           RADCSW.459
          UCO2(I,0)=ZENFAC(I)*PTHCO2                                             RADCSW.460
          UO2(I,0)=ZENFAC(I)*PTHO2                                               RADCSW.461
          UO3(I,0)=PTHO3                                                         RADCSW.462
   40   CONTINUE                                                                 RADCSW.463
   38 CONTINUE                                                                   RADCSW.464
C                                                                                RADCSW.465
      TMP2=DELTA/GRAVIT                                                          RADCSW.466
      DO 43 K=1,IPLEV                                                            RADCSW.467
        DO 44 N=1,NLOOP                                                          RADCSW.468
          DO 46 I=IS(N),IE(N)                                                    RADCSW.469
            PDEL=PFLX(I,K+1)-PFLX(I,K)                                           RADCSW.470
            PATH=PDEL/GRAVIT                                                     RADCSW.471
            PTHO2=O2MMR*PATH                                                     RADCSW.472
            PTHO3=O3MMR(I,K)*PATH                                                RADCSW.473
            PTHCO2=SQRCO2*PATH                                                   RADCSW.474
C                                                                                RADCSW.475
            H2OSTR=SQRT(1.0/H2OMMR(I,K))                                         RADCSW.476
            PTHH2O=(PFLX(I,K+1)**2-PFLX(I,K)**2)*TMP1+PDEL*H2OSTR*               RADCSW.477
     +             ZENFAC(I)*TMP2                                                RADCSW.478
C                                                                                RADCSW.479
            UH2O(I,K)=H2OMMR(I,K)*PTHH2O                                         RADCSW.480
            UCO2(I,K)=ZENFAC(I)*PTHCO2                                           RADCSW.481
            UO2(I,K)=ZENFAC(I)*PTHO2                                             RADCSW.482
            UO3(I,K)=PTHO3                                                       RADCSW.483
   46     CONTINUE                                                               RADCSW.484
   44   CONTINUE                                                                 RADCSW.485
   43 CONTINUE                                                                   RADCSW.486
C                                                                                RADCSW.487
C COMPUTE COLUMN ABSORBER AMOUNTS FOR THE CLEAR SKY COMPUTATION:                 RADCSW.488
C                                                                                RADCSW.489
      DO 47 N=1,NLOOP                                                            RADCSW.490
        DO 48 I=IS(N),IE(N)                                                      RADCSW.491
          UTH2O(I)=0.0                                                           RADCSW.492
          UTO3(I)=0.0                                                            RADCSW.493
          UTCO2(I)=0.0                                                           RADCSW.494
          UTO2(I)=0.0                                                            RADCSW.495
          DO 49 K=0,IPLEV                                                        RADCSW.496
            UTH2O(I)=UTH2O(I)+UH2O(I,K)                                          RADCSW.497
            UTO3(I)=UTO3(I)+UO3(I,K)                                             RADCSW.498
            UTCO2(I)=UTCO2(I)+UCO2(I,K)                                          RADCSW.499
            UTO2(I)=UTO2(I)+UO2(I,K)                                             RADCSW.500
   49     CONTINUE                                                               RADCSW.501
   48   CONTINUE                                                                 RADCSW.502
   47 CONTINUE                                                                   RADCSW.503
C                                                                                RADCSW.504
C INITIALIZE SPECTRALLY INTEGRATED TOTALS:                                       RADCSW.505
C                                                                                RADCSW.506
      DO 50 K=0,IPLEV                                                            RADCSW.507
        DO 52 I=1,IPLON                                                          RADCSW.508
          TOTFLD(I,K)=0.0                                                        RADCSW.509
          FSWUP(I,K)=0.0                                                         RADCSW.510
          FSWDN(I,K)=0.0                                                         RADCSW.511
   52   CONTINUE                                                                 RADCSW.512
   50 CONTINUE                                                                   RADCSW.513
      DO 58 I=1,IPLON                                                            RADCSW.514
        SFLTOT(I)=0.0                                                            RADCSW.515
        FSWUP(I,IPLEVP)=0.0                                                      RADCSW.516
        FSWDN(I,IPLEVP)=0.0                                                      RADCSW.517
   58 CONTINUE                                                                   RADCSW.518
C                                                                                RADCSW.519
C SET CLOUD PROPERTIES FOR TOP (0) LAYER; SO LONG AS TAUEXC IS ZER               RADCSW.520
C THERE IS NO CLOUD ABOVE TOP OF MODEL; THE OTHER CLOUD PROPERTIES               RADCSW.521
C ARE ARBITRARY:                                                                 RADCSW.522
C                                                                                RADCSW.523
      DO 206 N=1,NLOOP                                                           RADCSW.524
        DO 208 I=IS(N),IE(N)                                                     RADCSW.525
          TAUEXC(I,0)=0.                                                         RADCSW.526
          WC(I,0)=0.999999                                                       RADCSW.527
          GC(I,0)=0.85                                                           RADCSW.528
          FC(I,0)=0.725                                                          RADCSW.529
          TAUEXI(I,0)=0.                                                         05DEC01.1464
          WCI(I,0)=0.999999                                                      05DEC01.1465
          GCI(I,0)=0.85                                                          05DEC01.1466
          FCI(I,0)=0.725                                                         05DEC01.1467
  208   CONTINUE                                                                 RADCSW.530
  206 CONTINUE                                                                   RADCSW.531
C                                                                                RADCSW.532
C BEGIN SPECTRAL LOOP                                                            RADCSW.533
C                                                                                RADCSW.534
      DO 100 NS=1,NSPINT                                                         RADCSW.535
C                                                                                RADCSW.536
C SET INDEX FOR CLOUD PARTICLE PROPERTIES BASED ON THE WAVELENGTH,               RADCSW.537
C ACCORDING TO A. SLINGO (1989) EQUATIONS 1-3:                                   RADCSW.538
C USE INDEX 1 (0.25 TO 0.69 MICROMETERS) FOR VISIBLE                             RADCSW.539
C USE INDEX 2 (0.69 - 1.19 MICROMETERS) FOR NEAR-INFRARED                        RADCSW.540
C USE INDEX 3 (1.19 TO 2.38 MICROMETERS) FOR NEAR-INFRARED                       RADCSW.541
C USE INDEX 4 (2.38 TO 4.00 MICROMETERS) FOR NEAR-INFRARED                       RADCSW.542
C                                                                                RADCSW.543
C NOTE THAT THE MINIMUM WAVELENGTH IS ENCODED (WITH .001, .002, .003)            RADCSW.544
C IN ORDER TO SPECIFY THE INDEX APPROPRIATE FOR THE NEAR-INFRARED                RADCSW.545
C CLOUD ABSORPTION PROPERTIES                                                    RADCSW.546
C                                                                                RADCSW.547
        IF(WAVMAX(NS).LE.0.7)THEN                                                RADCSW.548
          INDXSL=1                                                               RADCSW.549
        ELSEIF(WAVMIN(NS).EQ.0.700)THEN                                          RADCSW.550
          INDXSL=2                                                               RADCSW.551
        ELSEIF(WAVMIN(NS).EQ.0.701)THEN                                          RADCSW.552
          INDXSL=3                                                               RADCSW.553
        ELSEIF(WAVMIN(NS).EQ.0.702.OR.WAVMIN(NS).GT.2.38)THEN                    RADCSW.554
          INDXSL=4                                                               RADCSW.555
        ENDIF                                                                    RADCSW.556
C                                                                                RADCSW.557
C SET CLOUD EXTINCTION OPTICAL DEPTH, SINGLE SCATTER ALBEDO,                     RADCSW.558
C ASYMMETRY PARAMETER, AND FORWARD SCATTERED FRACTION:                           RADCSW.559
C                                                                                RADCSW.560
        ABARI=ABAR(INDXSL)                                                       RADCSW.561
        BBARI=BBAR(INDXSL)                                                       RADCSW.562
        CBARI=CBAR(INDXSL)                                                       RADCSW.563
        DBARI=DBAR(INDXSL)                                                       RADCSW.564
        EBARI=EBAR(INDXSL)                                                       RADCSW.565
        FBARI=FBAR(INDXSL)                                                       RADCSW.566
C                                                                                05DEC01.1468
        ABRICI=ABRIC(INDXSL)                                                     05DEC01.1469
        BBRICI=BBRIC(INDXSL)                                                     05DEC01.1470
        CBRICI=CBRIC(INDXSL)                                                     05DEC01.1471
        DBRICI=DBRIC(INDXSL)                                                     05DEC01.1472
        EBRICI=EBRIC(INDXSL)                                                     05DEC01.1473
        FBRICI=FBRIC(INDXSL)                                                     05DEC01.1474
        DO 228 K=1,IPLEV                                                         RADCSW.567
          TMP1=ABARI+BBARI*RCRE(K)                                               RADCSW.568
          TMP2=1.-CBARI-DBARI*CRE(K)                                             RADCSW.569
          TMP3=FBARI*CRE(K)                                                      RADCSW.570
C for ice cloud                                                                  05DEC01.1475
          TMP1I=ABRICI+BBRICI*RCREI(K)                                           05DEC01.1476
          TMP2I=1.-CBRICI-DBRICI*CREI(K)                                         05DEC01.1477
          TMP3I=FBRICI*CREI(K)                                                   05DEC01.1478
          DO 229 N=1,NLOOP                                                       RADCSW.571
            DO 230 I=IS(N),IE(N)                                                 RADCSW.572
              TAUEXC(I,K)=(CLWP(I,K)+CONWP(I,K))*TMP1                            05DEC01.1479
              TAUEXI(I,K)=CIWP(I,K)*TMP1I                                        05DEC01.1480
C                                                                                RADCSW.574
C CLOUD FRACTION INCORPORATED INTO CLOUD EXTINCTION OPTICAL DEPTH                RADCSW.575
C                                                                                RADCSW.576
              TAUEXC(I,K)=TAUEXC(I,K)*CLD(I,K)*SQRT(CLD(I,K))                    RADCSW.577
              TAUEXI(I,K)=TAUEXI(I,K)*CLD(I,K)*SQRT(CLD(I,K))                    05DEC01.1481
C                                                                                RADCSW.578
C DO NOT LET SINGLE SCATTER ALBEDO BE 1; DELTA-EDDINGTON SOLUTION                RADCSW.579
C FOR NON-CONSERVATIVE CASE:                                                     RADCSW.580
C                                                                                RADCSW.581
              WC(I,K)=AMIN1(TMP2,.999999)                                        RADCSW.582
              GC(I,K)=EBARI+TMP3                                                 RADCSW.583
              FC(I,K)=GC(I,K)*GC(I,K)                                            RADCSW.584
              WCI(I,K)=AMIN1(TMP2I,.999999)                                      05DEC01.1482
              GCI(I,K)=EBRICI+TMP3I                                              05DEC01.1483
              FCI(I,K)=GCI(I,K)*GCI(I,K)                                         05DEC01.1484
  230       CONTINUE                                                             RADCSW.585
  229     CONTINUE                                                               RADCSW.586
  228   CONTINUE                                                                 RADCSW.587
C                                                                                RADCSW.588
C SET REFLECTIVITIES FOR SURFACE BASED ON MID-POINT WAVELENGTH                   RADCSW.589
C                                                                                RADCSW.590
        WAVMID=0.5*(WAVMIN(NS)+WAVMAX(NS))                                       RADCSW.591
C                                                                                RADCSW.592
C WAVELENGTH LESS  THAN 0.7 MICRO-METER                                          RADCSW.593
C                                                                                RADCSW.594
        IF(WAVMID.LT.0.7)THEN                                                    RADCSW.595
          DO 412 N=1,NLOOP                                                       RADCSW.596
            DO 414 I=IS(N),IE(N)                                                 RADCSW.597
#ifndef MPP1                                                                     RADCSW.598
              ILIM=MIN0(I,ILX)                                                   RADCSW.599
#else                                                                            RADCSW.600
              ILIM=I                                                             RADCSW.601
#endif                                                                           RADCSW.602
C                 ALBDIR(I)  = ALBS(I)                                           RADCSW.603
C                 ALBDIF(I)  = ALBSD(I)                                          RADCSW.604
C            ALBDIR(I)  = ALDIRS(I)                                              RADCSW.605
C            ALBDIF(I)  = ALDIFS(I)                                              RADCSW.606
              ALBDIR(I)=ALB(ILIM,JSLC)                                           RADCSW.607
              ALBDIF(I)=ALB(ILIM,JSLC)                                           RADCSW.608
  414       CONTINUE                                                             RADCSW.609
  412     CONTINUE                                                               RADCSW.610
C                                                                                RADCSW.611
C WAVELENGTH GREATER THAN 0.7 MICRO-METER                                        RADCSW.612
C                                                                                RADCSW.613
        ELSE                                                                     RADCSW.614
          DO 422 N=1,NLOOP                                                       RADCSW.615
            DO 424 I=IS(N),IE(N)                                                 RADCSW.616
#ifndef MPP1                                                                     RADCSW.617
              ILIM=MIN0(I,ILX)                                                   RADCSW.618
#else                                                                            RADCSW.619
              ILIM=I                                                             RADCSW.620
#endif                                                                           RADCSW.621
C                 ALBDIR(I)  = ALBL(I)                                           RADCSW.622
C                 ALBDIF(I)  = ALBLD(I)                                          RADCSW.623
C            ALBDIR(I)  = ALDIRL(I)                                              RADCSW.624
C            ALBDIF(I)  = ALDIFL(I)                                              RADCSW.625
              ALBDIR(I)=ALB(ILIM,JSLC)                                           RADCSW.626
              ALBDIF(I)=ALB(ILIM,JSLC)                                           RADCSW.627
  424       CONTINUE                                                             RADCSW.628
  422     CONTINUE                                                               RADCSW.629
        ENDIF                                                                    RADCSW.630
        TRAYOSLP=RAYTAU(NS)/SSLP                                                 RADCSW.631
C                                                                                RADCSW.632
C LAYER INPUT PROPERTIES NOW COMPLETELY SPECIFIED; COMPUTE THE                   RADCSW.633
C DELTA-EDDINGTON SOLUTION REFLECTIVITIES AND TRANSMISSIVITIES                   RADCSW.634
C FOR EACH LAYER, STARTING FROM THE TOP AND WORKING DOWNWARDS:                   RADCSW.635
C                                                                                RADCSW.636
        CALL RADDED(COSZRS,TRAYOSLP,PFLX,ABH2O(NS),ABO3(NS),ABCO2(NS),           RADCSW.637
     +       ABO2(NS),UH2O,UO3,UCO2,UO2,TAUEXC,WC,GC,FC,NLOOP,IS,IE,RDIR         RADCSW.638
     +       ,RDIF,TDIR,TDIF,EXPLAY,EXPTDN,RDNDIF,TOTTRN,KTAUW                   05DEC01.1485
     +       ,TAUEXI,WCI,GCI,FCI)                                                05DEC01.1486
C                                                                                RADCSW.640
C COMPUTE REFLECTIVITY TO DIRECT AND DIFFUSE RADIATION FOR LAYERS BELOW          RADCSW.641
C BY ADDING SUCCESIVE LAYERS STARTING FROM THE SURFACE AND WORKING               RADCSW.642
C UPWARDS:                                                                       RADCSW.643
C                                                                                RADCSW.644
        DO 560 N=1,NLOOP                                                         RADCSW.645
          DO 570 I=IS(N),IE(N)                                                   RADCSW.646
            RUPDIR(I,IPLEVP)=ALBDIR(I)                                           RADCSW.647
            RUPDIF(I,IPLEVP)=ALBDIF(I)                                           RADCSW.648
  570     CONTINUE                                                               RADCSW.649
  560   CONTINUE                                                                 RADCSW.650
        DO 580 K=IPLEV,0,-1                                                      RADCSW.651
          DO 582 N=1,NLOOP                                                       RADCSW.652
            DO 584 I=IS(N),IE(N)                                                 RADCSW.653
              RDENOM=1./(1.-RDIF(I,K)*RUPDIF(I,K+1))                             RADCSW.654
              RUPDIR(I,K)=RDIR(I,K)+TDIF(I,K)*(RUPDIR(I,K+1)*EXPLAY(I,K)         RADCSW.655
     +                    +RUPDIF(I,K+1)*(TDIR(I,K)-EXPLAY(I,K)))*RDENOM         RADCSW.656
              RUPDIF(I,K)=RDIF(I,K)+RUPDIF(I,K+1)*TDIF(I,K)**2*RDENOM            RADCSW.657
  584       CONTINUE                                                             RADCSW.658
  582     CONTINUE                                                               RADCSW.659
  580   CONTINUE                                                                 RADCSW.660
C                                                                                RADCSW.661
C COMPUTE UP AND DOWN FLUXES FOR EACH INTERFACE, USING THE ADDED                 RADCSW.662
C ATMOSPHERIC LAYER PROPERTIES AT EACH INTERFACE:                                RADCSW.663
C                                                                                RADCSW.664
        DO 600 K=0,IPLEVP                                                        RADCSW.665
          DO 610 N=1,NLOOP                                                       RADCSW.666
            DO 620 I=IS(N),IE(N)                                                 RADCSW.667
              RDENOM=1./(1.-RDNDIF(I,K)*RUPDIF(I,K))                             RADCSW.668
              FLUXUP(I,K)=(EXPTDN(I,K)*RUPDIR(I,K)+(TOTTRN(I,K)-                 RADCSW.669
     +                    EXPTDN(I,K))*RUPDIF(I,K))*RDENOM                       RADCSW.670
              FLUXDN(I,K)=EXPTDN(I,K)+(TOTTRN(I,K)-EXPTDN(I,K)+                  RADCSW.671
     +                    EXPTDN(I,K)*RUPDIR(I,K)*RDNDIF(I,K))*RDENOM            RADCSW.672
  620       CONTINUE                                                             RADCSW.673
  610     CONTINUE                                                               RADCSW.674
  600   CONTINUE                                                                 RADCSW.675
C                                                                                RADCSW.676
C COMPUTE FLUX DIVERGENCE IN EACH LAYER USING THE INTERFACE                      RADCSW.677
C UP AND DOWN FLUXES:                                                            RADCSW.678
C                                                                                RADCSW.679
        DO 630 K=0,IPLEV                                                         RADCSW.680
          DO 640 N=1,NLOOP                                                       RADCSW.681
            DO 650 I=IS(N),IE(N)                                                 RADCSW.682
              FLXDIV(I,K)=(FLUXDN(I,K)-FLUXDN(I,K+1))+(FLUXUP(I,K+1)-            RADCSW.683
     +                    FLUXUP(I,K))                                           RADCSW.684
  650       CONTINUE                                                             RADCSW.685
  640     CONTINUE                                                               RADCSW.686
  630   CONTINUE                                                                 RADCSW.687
C                                                                                RADCSW.688
C MONOCHROMATIC COMPUTATION  COMPLETED; ACCUMULATE IN TOTALS;                    RADCSW.689
C ADJUST FRACTION WITHIN SPECTRAL INTERVAL TO ALLOW FOR THE                      RADCSW.690
C POSSIBILITY OF SUB-DIVISIONS WITHIN A PARTICULAR INTERVAL:                     RADCSW.691
C                                                                                RADCSW.692
        PSF=1.0                                                                  RADCSW.693
        IF(PH2O(NS).NE.0.)PSF=PSF*PH2O(NS)                                       RADCSW.694
        IF(PCO2(NS).NE.0.)PSF=PSF*PCO2(NS)                                       RADCSW.695
        IF(PO2(NS).NE.0.)PSF=PSF*PO2(NS)                                         RADCSW.696
C ZERO OUT SOLFLX FOR USE IN BATS                                                RADCSW.697
        DO 652 ICING=1,IPLOND                                                    RADCSW.698
          SOLFLX(ICING)=0.                                                       RADCSW.699
  652   CONTINUE                                                                 RADCSW.700
        DO 656 N=1,NLOOP                                                         RADCSW.701
          DO 658 I=IS(N),IE(N)                                                   RADCSW.702
            SOLFLX(I)=SOLIN(I)*FRCSOL(NS)*PSF                                    RADCSW.703
            SABTP(I)=SABTP(I)+SOLFLX(I)*FLUXUP(I,0)                              07NOV00.2178
            FRSA(I)=FRSA(I)+SOLFLX(I)*(FLUXDN(I,IPLEVP)-FLUXUP(I,IPLEVP)         RADCSW.705
     +              )                                                            RADCSW.706
C   CALCULATE SABVEG BASED ON SPECTRAL INTERVALS                                 RADCSW.707
C   NOTE: DIRECT AND DIFFUSE ALBEDOES ARE ASSUMED TO BE THE SAME                 RADCSW.708
C             IF (WAVMID .LT. 0.7) THEN                                          RADCSW.709
C             SABVEG(I)=SABVEG(I) + 0.001*(SOLFLX(I)*(FLUXDN(I,IPLEVP)-          RADCSW.710
C    $        FLUXUP(I,IPLEVP)))*(1.-ALBVS(I))/(1.-ALBDIR(I))                    RADCSW.711
C             ELSE                                                               RADCSW.712
C             SABVEG(I)=SABVEG(I) + 0.001*(SOLFLX(I)*(FLUXDN(I,IPLEVP)-          RADCSW.713
C    $        FLUXUP(I,IPLEVP)))*(1.-ALBVL(I))/(1.-ALBDIR(I))                    RADCSW.714
C             ENDIF                                                              RADCSW.715
C      IF (SOLFLX(I).GT.0.00000000001) THEN                                      RADCSW.716
C      PRINT *,'CHECK SABVEG 1 ',I,NS,SABVEG(I),ALBVS(I),ALBVL(I),               RADCSW.717
C     $     ALBDIR(I),SOLFLX(I),WAVMID,FLUXDN(I,IPLEVP),                         RADCSW.718
C     $     FLUXUP(I,IPLEVP),IS(N),IE(N)                                         RADCSW.719
C      ENDIF                                                                     RADCSW.720
            SFLTOT(I)=SFLTOT(I)+SOLFLX(I)                                        RADCSW.721
            FSWUP(I,0)=FSWUP(I,0)+SOLFLX(I)*FLUXUP(I,0)                          RADCSW.722
            FSWDN(I,0)=FSWDN(I,0)+SOLFLX(I)*FLUXDN(I,0)                          RADCSW.723
  658     CONTINUE                                                               RADCSW.724
  656   CONTINUE                                                                 RADCSW.725
        DO 680 K=0,IPLEV                                                         RADCSW.726
          DO 682 N=1,NLOOP                                                       RADCSW.727
            DO 684 I=IS(N),IE(N)                                                 RADCSW.728
              TOTFLD(I,K)=TOTFLD(I,K)+SOLFLX(I)*FLXDIV(I,K)                      RADCSW.729
              FSWUP(I,K+1)=FSWUP(I,K+1)+SOLFLX(I)*FLUXUP(I,K+1)                  RADCSW.730
              FSWDN(I,K+1)=FSWDN(I,K+1)+SOLFLX(I)*FLUXDN(I,K+1)                  RADCSW.731
  684       CONTINUE                                                             RADCSW.732
  682     CONTINUE                                                               RADCSW.733
  680   CONTINUE                                                                 RADCSW.734
C    SOLIS IS INCIDENT VISIBLE SOLAR RADIATION                                   RADCSW.735
C       IF(NS.EQ.8)THEN                                                          RADCSW.736
C         DO 690 I=1,IPLOND                                                      RADCSW.737
C           SOLIS(I)=SOLFLX(I)*0.001*FLUXDN(I,IPLEVP)                            RADCSW.738
C 690     CONTINUE                                                               RADCSW.739
C       ENDIF                                                                    RADCSW.740
C                                                                                RADCSW.741
C                                                                                RADCSW.742
C FOLLOWING CODE IS THE DIAGNOSTIC CLEAR SKY COMPUTATION:                        RADCSW.743
C                                                                                RADCSW.744
C COMPUTE DELTA-EDDINGTON SOLUTION REFLECTIVITIES AND TRANSMISSIVITIES           RADCSW.745
C FOR THE ENTIRE COLUMN; NOTE, FOR CONVENIENCE, WE USE THE SAME                  RADCSW.746
C REFLECTIVITY AND TRANSMISSIVITY ARRAYS AS FOR THE FULL CALCULATION             RADCSW.747
C ABOVE, WHERE 0 FOR LAYER QUANTITIES REFERS TO THE ENTIRE ATMOSPHERIC           RADCSW.748
C COLUMN, AND WHERE 0 FOR INTERFACE QUANTITIES REFERS TO TOP OF ATMOS-           RADCSW.749
C PHERE, WHILE 1 REFERS TO THE SURFACE:                                          RADCSW.750
C                                                                                RADCSW.751
        CALL RADCLR(COSZRS,TRAYOSLP,PFLX,ABH2O(NS),ABO3(NS),ABCO2(NS),           RADCSW.752
     +       ABO2(NS),UTH2O,UTO3,UTCO2,UTO2,NLOOP,IS,IE,RDIR,RDIF,TDIR,          RADCSW.753
     +       TDIF,EXPLAY,EXPTDN,RDNDIF,TOTTRN)                                   RADCSW.754
C                                                                                RADCSW.755
C COMPUTE REFLECTIVITY TO DIRECT AND DIFFUSE RADIATION FOR                       RADCSW.756
C ENTIRE COLUMN; 0,1 ON LAYER QUANTITIES REFERS TO TWO EFFECTIVE LAYERS          RADCSW.757
C OVERLYING SURFACE; 0 ON INTERFACE QUANTITIES REFERS TO TOP OF COLUMN;          RADCSW.758
C 2 ON INTERFACE QUANTITIES REFERS TO THE SURFACE:                               RADCSW.759
C                                                                                RADCSW.760
        DO 705 N=1,NLOOP                                                         RADCSW.761
          DO 710 I=IS(N),IE(N)                                                   RADCSW.762
            RUPDIR(I,2)=ALBDIR(I)                                                RADCSW.763
            RUPDIF(I,2)=ALBDIF(I)                                                RADCSW.764
  710     CONTINUE                                                               RADCSW.765
  705   CONTINUE                                                                 RADCSW.766
C                                                                                RADCSW.767
        DO 715 K=1,0,-1                                                          RADCSW.768
          DO 720 N=1,NLOOP                                                       RADCSW.769
            DO 725 I=IS(N),IE(N)                                                 RADCSW.770
              RDENOM=1./(1.-RDIF(I,K)*RUPDIF(I,K+1))                             RADCSW.771
              RUPDIR(I,K)=RDIR(I,K)+TDIF(I,K)*(RUPDIR(I,K+1)*EXPLAY(I,K)         RADCSW.772
     +                    +RUPDIF(I,K+1)*(TDIR(I,K)-EXPLAY(I,K)))*RDENOM         RADCSW.773
              RUPDIF(I,K)=RDIF(I,K)+RUPDIF(I,K+1)*TDIF(I,K)**2*RDENOM            RADCSW.774
  725       CONTINUE                                                             RADCSW.775
  720     CONTINUE                                                               RADCSW.776
  715   CONTINUE                                                                 RADCSW.777
C                                                                                RADCSW.778
C COMPUTE UP AND DOWN FLUXES FOR EACH INTERFACE, USING THE ADDED                 RADCSW.779
C ATMOSPHERIC LAYER PROPERTIES AT EACH INTERFACE:                                RADCSW.780
C                                                                                RADCSW.781
        DO 730 K=0,2                                                             RADCSW.782
          DO 735 N=1,NLOOP                                                       RADCSW.783
            DO 740 I=IS(N),IE(N)                                                 RADCSW.784
              RDENOM=1./(1.-RDNDIF(I,K)*RUPDIF(I,K))                             RADCSW.785
              FLUXUP(I,K)=(EXPTDN(I,K)*RUPDIR(I,K)+(TOTTRN(I,K)-                 RADCSW.786
     +                    EXPTDN(I,K))*RUPDIF(I,K))*RDENOM                       RADCSW.787
              FLUXDN(I,K)=EXPTDN(I,K)+(TOTTRN(I,K)-EXPTDN(I,K)+                  RADCSW.788
     +                    EXPTDN(I,K)*RUPDIR(I,K)*RDNDIF(I,K))*RDENOM            RADCSW.789
  740       CONTINUE                                                             RADCSW.790
  735     CONTINUE                                                               RADCSW.791
  730   CONTINUE                                                                 RADCSW.792
C                                                                                RADCSW.793
        DO 745 N=1,NLOOP                                                         RADCSW.794
          DO 750 I=IS(N),IE(N)                                                   RADCSW.795
            CLRST(I)=CLRST(I)+SOLFLX(I)*(FLUXDN(I,0)-FLUXUP(I,0))                RADCSW.796
            CLRSS(I)=CLRSS(I)+SOLFLX(I)*(FLUXDN(I,2)-FLUXUP(I,2))                RADCSW.797
  750     CONTINUE                                                               RADCSW.798
  745   CONTINUE                                                                 RADCSW.799
C                                                                                RADCSW.800
C END OF CLEAR SKY COMPUTATION                                                   RADCSW.801
C                                                                                RADCSW.802
C                                                                                RADCSW.803
C END OF SPECTRAL INTERVAL LOOP                                                  RADCSW.804
C                                                                                RADCSW.805
  100 CONTINUE                                                                   RADCSW.806
C                                                                                RADCSW.807
C COMPUTE SOLAR HEATING RATE (K/S)                                               RADCSW.808
C                                                                                RADCSW.809
      GOCP=GRAVIT/CPAIR                                                          RADCSW.810
      DO 800 K=1,IPLEV                                                           RADCSW.811
        DO 802 N=1,NLOOP                                                         RADCSW.812
          ISN=IS(N)                                                              RADCSW.813
          IEN=IE(N)                                                              RADCSW.814
          DO 804 I=ISN,IEN                                                       RADCSW.815
            QRS(I,K)=-GOCP*TOTFLD(I,K)/(PINT(I,K)-PINT(I,K+1))                   RADCSW.816
  804     CONTINUE                                                               RADCSW.817
  802   CONTINUE                                                                 RADCSW.818
  800 CONTINUE                                                                   RADCSW.819
      RETURN                                                                     RADCSW.820
      END                                                                        RADCSW.821
                                                                                 RADCSW.822
