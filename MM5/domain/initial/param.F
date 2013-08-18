      SUBROUTINE PARAM (IEXEC)                                                   PARAM.1
      IMPLICIT NONE                                                              PARAM.2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          PARAM.3
C                                                                     C          PARAM.4
C                                                                     C          PARAM.5
C     THIS SUBROUTINE DEFINES VARIOUS MODEL PARAMETERS.               C          PARAM.6
C     CHANGES: READS IN LANDUSE.TBL FOR PROPERTIES (REMOVE DATA STMTS)C          PARAM.7
C                                                                     C          PARAM.8
C                                                                     C          PARAM.9
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          PARAM.10
#ifdef MPP1                                                                      PARAM.11
#  include <mpp_param_00.incl>                                                   PARAM.12
#endif                                                                           PARAM.13
#     include <defines.incl>                                                     PARAM.14
#     include <parame.incl>                                                      PARAM.15
#     include <param2.incl>                                                      PARAM.16
#     include <parfdda.incl>                                                     PARAM.17
#     include <param3.incl>                                                      PARAM.18
#     include <pmoist.incl>                                                      PARAM.19
#     include <landuse.incl>                                                     PARAM.20
#     include <addr0.incl>                                                       PARAM.21
#     include <various.incl>                                                     PARAM.22
#     include <hdrv3.incl>                                                       PARAM.23
#     include <chardate.incl>                                                    PARAM.24
#     include <nhcnst.incl>                                                      PARAM.25
#     include <fddagd.incl>                                                      PARAM.26
#     include <fddaob.incl>                                                      PARAM.27
#     include <soilcnst.incl>                                                    PARAM.28
#     include <pbltb.incl>                                                       PARAM.29
#     include <pargrcu.incl>                                                     PARAM.30
#     include <parkfcu.incl>                                                     PARAM.31
#     include <parkfcu2.incl>                                                    05DEC01.110
#     include <parbmcu.incl>                                                     PARAM.32
#     include <parakcu.incl>                                                     PARAM.33
#     include <landinp.incl>                                                     24SEP99.30
#     include <parpx.incl>                                                       07NOV00.102
#     include <bucket.incl>                                                      07NOV00.103
#     include <paramveg_USGS.incl>                                               19DEC02.205
#     include <paramsoil_STAS.incl>                                              19DEC02.206
#     include <paramgen_LSM.incl>                                                19DEC02.207
      INTEGER IEXEC(MAXNES)                                                      PARAM.34
      CHARACTER*4 MMINSL                                                         19DEC02.208
      CHARACTER*19 CDATEST                                                       PARAM.36
C                                                                                PARAM.37
      REAL GAMMA                                                                 PARAM.38
      EXTERNAL GAMMA                                                             PARAM.39
      LOGICAL DM_IONODE                                                          PARAM.40
      EXTERNAL DM_IONODE                                                         PARAM.41
C                                                                                PARAM.42
C                                                                                PARAM.43
C----------------------------------------------------------------------          PARAM.44
C-----VQRANG IS THE RANGE LIMIT ON VQFLX.                                        PARAM.45
C.....QDCRIT IS THE PRECIPITATION THRESHOLD FOR MOISTURE CONVERGENCE.            PARAM.46
C                                                                                PARAM.47
      INTEGER I,J,K,L,L2,IINDEX, NUM_SLOPE                                       19DEC02.209
      INTEGER MMD, ISTATE                                                        PARAM.49
      DIMENSION MMD(12),ISTATE(2)                                                PARAM.50
      INTEGER IWARN,INIEXMS,INIICE,INIICEG,IER,KXCHECK                           PARAM.51
      INTEGER NDIM,LUMATCH,LUSEAS                                                19DEC02.210
      INTEGER LI,LS,LC                                                           PARAM.53
      INTEGER NN2,NN3,N,NN,NNL,IN,NV                                             24SEP99.31
      INTEGER NOC,NOV                                                            PARAM.55
      INTEGER MYEAR,MMON,MDAY,MHOUR,MMIN,MSEC                                    PARAM.56
      INTEGER MY1,MY2,MY3,MONSS                                                  PARAM.57
      REAL EQOFT                                                                 PARAM.58
      REAL AA,CC,SSUM,XX,XTOP,XBOT,VQMAX,YY,WK,QK,WKP1,QKP1                      19DEC02.211
      REAL VQRANG,PI,ESI                                                         PARAM.60
      REAL G4PBS,G3PBS,G5PBS,PRACS                                               PARAM.61
      REAL FACTOR,C,DTMAX,CLOBOT,CMDBOT,CHIBOT,PTMB,PZ,PK,WP                     PARAM.62
      REAL SIG700,DIFP,WR14,WR1Z,WR24,WR2Z,ZOLN,X,Y                              PARAM.63
      REAL N0S                                                                   PARAM.64
      INTEGER MPHYTBL                                                            PARAM.65
      INTEGER KBASE,KTOP,IPERHR                                                  PARAM.66
      INTEGER JOUT,I4DCHECK                                                      PARAM.67
      REAL DUMMY                                                                 PARAM.68
      REAL XIG,XJG                                                               PARAM.69
      INTEGER IG0,JG0                                                            PARAM.70
      INTEGER IFOGMD                                                             07NOV00.104
      INTEGER IMOIAVI                                                            07NOV00.105
      DIMENSION IFOGMD(MAXSES)                                                   07NOV00.106
      COMMON /DEPAR2/ XIG(9),XJG(9),IG0(MJX,MIX),JG0(MJX,MIX)                    PARAM.71
C                                                                                PARAM.72
      INTEGER I_FRAD,I_ICUPA,I_IBLT,I_IMPHYS,I_ISOIL,I_MPHYSTBL                  23DEC04.107
      DIMENSION I_FRAD(0:10),I_ICUPA(0:10),I_IBLT(0:10),I_IMPHYS(0:10),          PARAM.74
     -          I_ISOIL(0:10),I_MPHYSTBL(0:10)                                   23DEC04.108
#ifdef MPP1                                                                      PARAM.76
#  include <mpp_param_05.incl>                                                   PARAM.77
#  include <cflcom.incl>                                                         PARAM.78
#  include <read_config1.h>                                                      PARAM.79
#else                                                                            PARAM.80
C THESE ARE NOT USED IN NON-MPP CODE BUT DEFINE SO THAT NAMELISTS THAT           PARAM.81
C CONTAIN WILL WORK WITHOUT MOD                                                  PARAM.82
      INTEGER NPROC_LT,NPROC_LN                                                  PARAM.83
     +       ,MPP_IO_NODE                                                        PARAM.84
     +       ,ICFLPERIOD                                                         PARAM.85
      REAL    CFLTHRESH                                                          PARAM.86
#endif                                                                           PARAM.87
      INTEGER I_FDDAOBS1,I_FDDAGD1                                               PARAM.88
CB      INTEGER I_NESTED,I_ISHALLO1,I_MPHYSTBL1                                  23DEC04.109
      INTEGER I_NESTED,I_ISHALLO1                                                23DEC04.110
C----------------------------------------------------------------------          PARAM.90
C-----NAMELIST:                                                                  PARAM.91
C                                                                                PARAM.92
      REAL TISTEP                                                                PARAM.93
      NAMELIST/OPARAM/ TIMAX,TISTEP,IFREST,IXTIMR,IFSAVE,SVLAST,                 PARAM.94
     1                 SAVFRQ,NTSAVE,IFTAPE,TAPFRQ,INCTAP,NTTAPE,                PARAM.95
     2                 IFSKIP,CDATEST,IFRSFA,                                    23DEC04.111
     +                 IFPRT,PRTFRQ,MASCHK,ISFOUT,                               23DEC04.112
     3                 BUFFRQ,IFTSOUT,TSLAT,TSLON,ITSN,JTSN,                     07NOV00.107
     +                 NPROC_LT,NPROC_LN,                                        PARAM.98
     +                 MPP_IO_NODE,                                              PARAM.99
     +                 ICFLPERIOD,CFLTHRESH                                      PARAM.100
C                                                                                PARAM.101
      NAMELIST/LPARAM/ RADFRQ,IMVDIF,IVQADV,IVTADV,ITHADV,ITPDIF,                PARAM.102
     &        ICOR3D,IFUPR,IFDRY,ICUSTB,IBOUDY,IFSNOW,ISFFLX,                    PARAM.103
     &        ITGFLG,ISFPAR,ICLOUD,ICDCON,IVMIXM,HYDPRE,                         PARAM.104
     &        IEVAP,IFOGMD,ISTLYR,ISMLYR,ISSTVAR,                                07NOV00.108
     &        IFRAD,ICUPA,IMPHYS,IBLTYP,ISHALLO,ISOIL,IPOLAR,                    19DEC02.212
     &        IMOIST,XMOIST,IMOIAV,IEXICE,KXOUT,JXSEX,IBMOIST                    PARAM.107
     &        ,ISMRD,NUDGE,IFGROW,IZ0TOPT,IEXSI,RDMAXALB,RDBRDALB                19DEC02.213
     &        ,ISFMTHD,LEVSLP,OROSHAW,ITADVM,IQADVM,TDKORR                       23DEC04.113
C                                                                                PARAM.108
      NAMELIST/NPARAM/ NUMNC,LEVIDN,IOVERW,                                      PARAM.109
     4        NESTI, NESTJ,XSTNES,XENNES,IACTIV,IMOVE,IMOVET,                    PARAM.110
     5        IMOVEI,IMOVEJ,IMOVCO,IFEED,NESTIX,NESTJX                           PARAM.111
C                                                                                PARAM.112
      NAMELIST/PPARAM/  SIGMA,PTOP, ZZLND, ZZWTR,ALBLND,                         PARAM.113
     1       THINLD,XMAVA,CONF,QCTH,QCK1,IABSOR,                                 PARAM.114
     2       IFDTSET,DTIMSET,PBCMAX,IFCDZ,CDMINP,CDMINZ,                         PARAM.115
     3       EDTMIN,EDTMAX,EDTSMX,EDTSMN,TCRIT,SIGCBHI,PKDCUT,                   PARAM.116
     4       CUQMAX,CUQMIN,SOLSET,CZO,OZO,VCONVC,RAD,TREL,BFAC,                  07NOV00.110
     5       SOILFAC,CKH,FRQKFDIAG                                               05DEC01.113
C                                                                                PARAM.118
      INTEGER I4DTOT,IFDDA,ILIF10,INNPFI                                         PARAM.119
      NAMELIST/FPARAM/ I4D   ,DIFTIM,IWIND ,GV    ,ITEMP ,GT    ,                PARAM.120
     *                 IMOIS ,GQ    ,IROT  ,GR    ,INONBL,RINBLW,                PARAM.121
     *                 NPFG  ,I4DI  ,ISWIND,GIV   ,ISTEMP,GIT   ,                PARAM.122
     *                 ISMOIS,GIQ   ,RINXY ,RINSIG,TWINDO,NPFI  ,                PARAM.123
     *                 RINFMN,RINFMX,PFREE ,DPSMX ,IONF  ,IWINDS                 PARAM.124
     *                 ,FDASTA,FDAEND ,IDYNIN,DTRAMP                             PARAM.125
      REAL DQCRIT                                                                PARAM.127
      REAL KZOC                                                                  PARAM.129
      DATA KZOC/0.1/                                                             PARAM.130
      DATA DQCRIT/0.05/                                                          PARAM.131
      DATA (MMD(I),I=1,12) /31,28,31,30,31,30,31,31,30,31,30,31/                 PARAM.132
      DATA VQRANG /5.0E-4/                                                       PARAM.133
      DATA QDCRIT /3.0E-7/                                                       PARAM.134
C                                                                                PARAM.135
      IF(IICE.EQ.1)CALL PARAMR                                                   PARAM.136
      IF(IICEG.EQ.1)CALL CONSAT                                                  PARAM.137
C                                                                                PARAM.138
C--- FILL IN COMMON BLOCK                                                        PARAM.139
C                                                                                PARAM.140
      DO 11 I=1,3                                                                PARAM.141
        DO 11 J=1,3                                                              PARAM.142
          XIG(J+(I-1)*3)=1./3.-FLOAT(J-1)*1./3                                   PARAM.143
          XJG(J+(I-1)*3)=1./3.-FLOAT(I-1)*1./3.                                  PARAM.144
   11   CONTINUE                                                                 PARAM.145
                                                                                 PARAM.146
      DO 21 I=1,MIX                                                              PARAM.147
        DO 21 J=1,MJX                                                            PARAM.148
          IG0(J,I)=J                                                             PARAM.149
          JG0(J,I)=I                                                             PARAM.150
   21   CONTINUE                                                                 PARAM.151
C                                                                                PARAM.152
C                                                                                PARAM.153
C----------------------------------------------------------------------          PARAM.154
C-----SPECIFY THE PARAMETERS USED IN THE MODEL:                                  PARAM.155
C                                                                                PARAM.156
C                                                                                PARAM.157
C     IFREST : WHETHER THIS RUN IS RESTARTING FROM A SAVED TAPE                  PARAM.158
C              (UNIT 13).                                                        PARAM.159
C              = .TRUE. ; YES                                                    PARAM.160
C              = .FALSE. ; NO                                                    PARAM.161
C                                                                                PARAM.162
C                                                                                PARAM.163
C     IFSAVE : SPECIFY WHETHER A SAVED TAPE (UNIT 24) WILL BE WRITTEN            PARAM.164
C              FOR RESTARTING.                                                   PARAM.165
C              = .TRUE. ; YES                                                    PARAM.166
C              = .FALSE. ; NO                                                    PARAM.167
C                                                                                PARAM.168
C     SVLAST : SPECIFY WHETHER TO SAVE ONLY LAST RESTART TIME (SAVES DIS         PARAM.169
C              SPACE BY REWINDING RESTART FILE BETWEEN WRITES)                   PARAM.170
C              = .TRUE. ; YES (SAVE ONLY LAST TIME)                              PARAM.171
C              = .FALSE. ; NO (SAVE ALL RESTART TIMES)                           PARAM.172
C                                                                                PARAM.173
C     IFSKIP : WHETHER TO START AT A SPECIFIED CDATE IN INPUT/BDY FILES          PARAM.174
C              = .TRUE. ; YES (NEED ALSO TO SPECIFY CDATEST)                     PARAM.175
C              = .FALSE. ; NO                                                    PARAM.176
C     IFRSFA : USED IF THIS IS A RESTART RUN, FDDA IS ACTIVE AND MULTIPLE        23DEC04.114
C              BOUNDARY FILES ARE USED                                           23DEC04.115
C              = .TRUE. ; YES (NEED ALSO TO SPECIFY CDATEST)                     23DEC04.116
C              = .FALSE. ; NO                                                    23DEC04.117
C                                                                                23DEC04.118
C     SAVFRQ : IF IFSAVE=.TRUE., SPECIFY THE INTERVAL IN MINUTES                 PARAM.177
C                                BETWEEN SAVE OPERATIONS.                        PARAM.178
C                                                                                PARAM.179
C     IFTSOUT: WHETHER TO OUTPUT TIME SERIES                                     07NOV00.112
C              = 1; YES                                                          07NOV00.113
C              = 0; NO                                                           07NOV00.114
C      TSLAT : LATITUDE LOCATION FOR TIME SERIES, SOUTHERN LAT IS NEGATIVE       07NOV00.115
C      TSLON : LONGITUDE LOCATION FOR TIME SERIES, WESTERN LON IS NEGATIVE       07NOV00.116
C              DEFAULT NUMBER OF TIME SERIES POINTS IS 30, BUT MAY BE            07NOV00.117
C              INCREASED BY MODIFYING PARAMETER ITSDIF IN include/parame         07NOV00.118
C                                                                                07NOV00.119
C     IBLTYP : SPECIFY WHETHER BULK-AERODYNAMIC PBL OR MULTI-LEVEL PBL           PARAM.180
C              PARAMETERIZATION IS TO BE USED IN THE MODEL.                      PARAM.181
C              = 0 ; FRICTIONLESS                                                PARAM.182
C              = 1 ; BULK PBL                                                    PARAM.183
C              = 2 ; MULTI-LEVEL PBL (ZHANG AND ANTHES, 1982)                    PARAM.184
C              = 3 ; BURK-THOMPSON PBL                                           PARAM.185
C              = 4 ; ETA-MELLOR-YAMADA PBL (JANJIC, 1990, 1994)                  PARAM.186
C              = 5 ; MRF PBL (HONG AND PAN, 1996)                                PARAM.187
C              = 6 ; GAYNO-SEAMAN PBL                                            PARAM.188
C              = 7 ; PLEIM-XIU LSM and PBL                                       07NOV00.120
C                                                                                PARAM.189
C     ISFFLX : WHETHER THE SURFACE HEAT AND MOISTURE FLUXES ARE TO BE            PARAM.190
C              CALCULATED.                                                       PARAM.191
C              = O ; NO                                                          PARAM.192
C              = 1 ; YES                                                         PARAM.193
C                                                                                PARAM.194
C     ITGFLG : IF ISFFLX=1, SPECIFY THE WAY TO CALCULATE THE GROUND              PARAM.195
C              TEMPERATURE.                                                      PARAM.196
C              = 1 ; FROM BUDGET.                                                PARAM.197
C              = 3 ; CONSTANTS I.E. SPECIFIED.                                   PARAM.198
C                                                                                PARAM.199
C     ICDCON : WHETHER THE DRAG COEFFICIENTS ARE SET TO CONSTANT IN              PARAM.200
C              BULK-PBL PARAMETERIZATION (IBLTYP=1) OR NOT.                      PARAM.201
C              = 0 ; NO,                                                         PARAM.202
C              = 1 ; YES.                                                        PARAM.203
C                                                                                PARAM.204
C     ICLOUD : WHETHER THE RADIATION EFFECTS DUE TO CLOUDS WILL BE               PARAM.205
C              CONSIDERED AT SURFACE, WHEN ISFFLX=1 AND ITGFLG=1.                05DEC01.114
C              IT IS ALSO USED TO ADD CLOUD EFFECT ON ATMOSPHERIC                05DEC01.115
C              RADIATION FOR IFRAD = 2,3 AND 4.                                  05DEC01.116
C              = 0 ; NO,                                                         PARAM.207
C              = 1 ; YES. FOR SFC RAD, RH IS USED. FOR ATMOS RAD,                05DEC01.117
C                CLOUD WATER/ICE EFFECT IS CONSIDERED.                           05DEC01.118
C              = 2 ; CLOUD EFFECT USING RH IN CCM2 ONLY.                         05DEC01.119
C                                                                                PARAM.209
C     ISFPAR : WHETHER THE SURFACE/TERRAIN PARAMETERS ARE VARIABLES OR           PARAM.210
C              CONSTANTS OVER THE WHOLE DOMAIN, WHEN ISFFLX=1 AND                PARAM.211
C              ITGFLG=1.                                                         PARAM.212
C              = 0 ; CONSTANTS                                                   PARAM.213
C              = 1 ; VARIABLES                                                   PARAM.214
C                                                                                07NOV00.121
C     ISSTVAR: WHETHER TO VARY SST DURING MODEL INTEGRATION.                     07NOV00.122
C              ALSO DEPENDING ON INPUT DATA.                                     07NOV00.123
C              = 0 ; NO                                                          07NOV00.124
C              = 1 ; YES                                                         07NOV00.125
C                                                                                05DEC01.120
C     IZ0TOPT: THERMAL ROUGHNESS LENGTH OPTION                                   05DEC01.121
C              = 0 ; CARLSON-BOLAND (OLD METHOD: VISCOUS SUB-LAYER FOR Q ONLY)   05DEC01.122
C              = 1 ; GARRATT FORMULA FOR THERMAL ROUGHNESS (T AND Q)             05DEC01.123
C              = 2 ; ZILITINKEVICH FORMULA FOR THERMAL ROUGHNESS (T AND Q)       05DEC01.124
C     ISFMTHD : METHOD USED TO CALCULATE 2M AND 10M DIAGNOSTICS                  23DEC04.119
C               IN HIRPBL AND MRFPBL REGIMES 1 AND 2 ONLY                        23DEC04.120
C              = 0 ; OLD METHOD (CONSTANT RI BELOW LOWEST LEVEL)                 23DEC04.121
C              = 1 ; NEW METHOD (USE CONSTANT M-O LENGTH INSTEAD)                23DEC04.122
C     RDMAXALB: READ MAX SNOW ALBEDO FROM INPUT CLIMATOLOGY FILE                 19DEC02.214
C              = .T. ; READ IN (SUPPLIED BY REGRID)                              19DEC02.215
C              = .F. ; USE TABLE (VEGPARM.TBL) VALUE AND LAND-USE                19DEC02.216
C     RDBRDALB: READ BACKGROUND ALBEDO FROM MONTHLY INPUT FILE                   19DEC02.217
C              = .T. ; READ IN (SUPPLIED BY REGRID)                              19DEC02.218
C              = .F. ; USE TABLE (LANDUSE.TBL) VALUE AND LAND-USE                19DEC02.219
C                                                                                PARAM.216
C     IFSNOW : WHETHER THE SNOW COVER DATA WILL BE CONSIDERED IN THE             PARAM.217
C              SURFACE CHARACTERISTICS OR NOT.                                   PARAM.218
C              = 0 ; NO                                                          PARAM.219
C              = 1 ; YES, SNOWC IS A FIXED FLAG THRU FCST                        05DEC01.125
C              = 2 ; YES, SNOWC CONTAINS PREDICTED LIQUID AMOUNT IN MM           05DEC01.126
C                                                                                PARAM.221
C     IFRAD  : WHETHER THE RADIATIVE COOLING OF THE ATMOSPHERE WILL BE           PARAM.222
C              CONSIDERED.                                                       PARAM.223
C              = 0 ; NO                                                          PARAM.224
C              = 1 ; SIMPLE COOLING                                              PARAM.225
C              = 2 ; LONGWAVE AND SHORTWAVE (DUDHIA, 1989)                       PARAM.226
C              = 3 ; LONGWAVE AND SHORTWAVE (CCM2 SCHEME)                        PARAM.227
C              = 4 ; RRTM LONGWAVE AND DUDHIA SHORTWAVE (RRTM SCHEME)            25JAN00.58
C                                                                                PARAM.228
C     RADFRQ : IF ISFFLX=1 AND ITGFLG=1, SPECIFY THE FREQUENCY IN                PARAM.229
C              MINUTES, THE SOLAR RADIATION WILL BE COMPUTED IN                  PARAM.230
C              SUBROUTINE "SFCRD". A TYPICAL VALUE WOULD BE 30 MINUTES.          PARAM.231
C                                                                                PARAM.232
C     ISFOUT : WHETHER YOU WANT OUTPUT FOR SURFACE/TERRAIN PARAMETERS.           PARAM.233
C              = 0 ; NO                                                          PARAM.234
C              = 1 ; YES                                                         PARAM.235
C                                                                                PARAM.236
C     IMOIAV : WHETHER THE MOISTURE AVAILABILITY IS A FUNCTION OF TIME           PARAM.237
C              USING A SIMPLE BUCKET MODEL OR NOT.                               07NOV00.126
C              = 0 ; NO                                                          PARAM.239
C              = 1 ; YES, USE DEFAULT TABLE VALUES                               07NOV00.127
C              = 2 ; YES, USE CONVERTED VALUES FROM TOP LAYER (10 CM)            07NOV00.128
C                    SOIL MOISTURE DATA AS INPUT                                 07NOV00.129
C                                                                                PARAM.241
C     IVMIXM : WHETHER YOU WANT VERTICAL MIXING OF MOMENTUM, WHEN                PARAM.242
C              IBLTYP=2.                                                         PARAM.243
C              = 0 ; NO                                                          PARAM.244
C              = 1 ; YES                                                         PARAM.245
C                                                                                PARAM.246
C                                                                                PARAM.247
C     IDRY   : WHETHER THIS RUN IS A MOIST OR DRY FORECAST.                      PARAM.248
C              = 0 ; MOIST                                                       PARAM.249
C              = 1 ; DRY                                                         PARAM.250
C                                                                                PARAM.251
C     IMOIST : IF IDRY=0,                                                        PARAM.252
C              WHETHER CUMULUS PARAMETERIZATION (ANTHES, 1977) OR                PARAM.253
C              EXPLICIT MOISTURE SCHEME (HSIE, 1984) WILL BE USED.               PARAM.254
C              = 0 ; DRY CASE WITH PASSIVE MOIST VARIABLES.                      PARAM.255
C              = 1 ; STABLE (NONCONVECTIVE) PRECIP                               PARAM.256
C              = 2 ; EXPLICIT MOISTURE.                                          PARAM.257
C                                                                                PARAM.258
C     ICUSTB : WHETHER THE STABILITY CHECK FOR THE CUMULUS PARAMETER-            PARAM.259
C              IZATION (IDRY=0 .AND. IMOIST=1) WILL BE APPLIED.                  PARAM.260
C              = 0 ; NO                                                          PARAM.261
C              = 1 ; YES                                                         PARAM.262
C                                                                                PARAM.263
C     XMOIST : WHETHER YOU WANT TO CONSIDER THE MOIST EFFECTS IN THE             PARAM.264
C              THERMODYNAMIC EQUATION.                                           PARAM.265
C              = 0., NO, WHEN IDRY=1 OR IMOIST=0.                                PARAM.266
C              = 1., YES, WHEN IDRY=0 AND IMOIST=1 OR 2.                         PARAM.267
C                                                                                PARAM.268
C     IEVAP  : WHETHER YOU WANT TO CONSIDER THE EVAPORATION EFFECTS IN           PARAM.274
C              THE EXPLICIT MOISTURE SCHEME.                                     PARAM.275
C              < 0 ; DO NOT CONSIDER EVAPORATION OF RAINWATER.                   PARAM.276
C              = 0 ; DO NOT CONSIDER EVAPORATION EFFECTS AT ALL                  PARAM.277
C                    (INCLUDING BOTH RAINWATER AND CLOUD WATER).                 PARAM.278
C              > 0 ; CONSIDER EVAPORATION EFFECTS.                               PARAM.279
C                                                                                PARAM.280
C     IBOUDY : SPECIFY THE LATERAL BOUNDARY CONDITIONS.                          PARAM.281
C              = 0 ; FIXED.                                                      PARAM.282
C              = 1 ; RELAXATION.                                                 PARAM.283
C              = 2 ; TIME-DEPENDENT (FROM OBSERVATIONS OR LARGE-SCALE            PARAM.284
C                                    MODEL).                                     PARAM.285
C              = 3 ; TIME AND INFLOW/OUTFLOW DEPENDENT.                          PARAM.286
C              = 4 ; SPONGE (PERKEY & KREITZBERG, MWR 1976).                     PARAM.287
C              = 5 ; PERIODIC IN EAST/WEST AND FIXED IN NORTH/SOUTH.             PARAM.288
C                                                                                PARAM.289
C     IFTAPE : WHETHER YOU WANT OUTPUT IN A SAVED TAPE (UNIT 20) FOR             PARAM.290
C              ANALYSES IN DATAFLOW.                                             PARAM.291
C              = 0 ; NO                                                          PARAM.292
C              = 1 ; YES                                                         PARAM.293
C                                                                                PARAM.294
C     TAPFRQ : IF IFTAPE=1, SPECIFY THE OUTPUT INTERVAL IN MINUTES.              PARAM.295
C                                                                                PARAM.296
C                                                                                PARAM.297
C                                                                                PARAM.298
C     MASCHK : SPECIFY THE FREQUENCY IN TIME STEPS, THE MASS-                    PARAM.299
C              CONSERVATION INFORMATION WILL BE PRINTED OUT.                     PARAM.300
C                                                                                PARAM.301
C                                                                                PARAM.302
C                                                                                PARAM.303
C     I4D    : IS THIS A GRID FOUR DIM. DATA ASSIMILATION RUN?                   PARAM.304
C              = 0 ; NO                                                          PARAM.305
C              = 1 ; YES                                                         PARAM.306
C                   I4D(1,1), IS FOR CGM ANALYSIS NUDGING                        PARAM.307
C                   I4D(MAXNES), IS FOR FGM ANALYSIS NUDGING                     PARAM.308
C                   I4D(1,2), IS FOR CGM SURFACE-ANALYSIS NUDGING                PARAM.309
C                   I4D(INEST,2), IS FOR FGM SURFACE-ANALYSIS NUDGING            PARAM.310
C                                                                                PARAM.311
C     DIFTIM : SPECIFY THE TIME IN MINUTES BETWEEN THE INPUT (USUALLY            PARAM.312
C              FROM INTERP) THREE-DIMENSIONAL DATA ANALYSES USED FOR             PARAM.313
C              GRID FDDA:                                                        PARAM.314
C                   DIFTIM(1,1), IS FOR CGM ANALYSIS NUDGING                     PARAM.315
C                   DIFTIM(MAXNES), IS FOR FGM ANALYSIS NUDGING                  PARAM.316
C            : SPECIFY THE TIME IN MINUTES BETWEEN THE INPUT (USUALLY            PARAM.317
C              FROM RAWINS) TWO-DIMENSIONAL SURFACE DATA ANALYSES USED           PARAM.318
C              FOR GRID FDDA:                                                    PARAM.319
C                   DIFTIM(1,2), IS FOR CGM SURFACE-ANALYSIS NUDGING             PARAM.320
C                   DIFTIM(INEST,2), IS FOR FGM SURFACE-ANALYSIS NUDGING         PARAM.321
C                                                                                PARAM.322
C     IWIND  : GRID NUDGE THE WIND FIELD FROM 3D ANALYSES?                       PARAM.323
C              = 0 ; NO                                                          PARAM.324
C              = 1 ; YES                                                         PARAM.325
C                   IWIND(1,1), IS FOR CGM ANALYSIS NUDGING                      PARAM.326
C                   IWIND(MAXNES), IS FOR FGM ANALYSIS NUDGING                   PARAM.327
C            : GRID NUDGE THE WIND FIELD FROM SURFACE ANALYSES?                  PARAM.328
C                   IWIND(1,2), IS FOR CGM SURFACE-ANALYSIS NUDGING              PARAM.329
C                   IWIND(INEST,2), IS FOR FGM SURFACE-ANALYSIS NUDGING          PARAM.330
C                                                                                PARAM.331
C     GV     : NUDGING COEFFICIENT FOR WINDS FROM 3D ANALYSES                    PARAM.332
C                   GV(1,1), IS FOR CGM ANALYSIS NUDGING                         PARAM.333
C                   GV(MAXNES), IS FOR FGM ANALYSIS NUDGING                      PARAM.334
C            : NUDGING COEFFICIENT FOR WINDS FROM SURFACE ANALYSES               PARAM.335
C                   GV(1,2), IS FOR CGM SURFACE-ANALYSIS NUDGING                 PARAM.336
C                   GV(INEST,2), IS FOR FGM SURFACE-ANALYSIS NUDGING             PARAM.337
C                                                                                PARAM.338
C     ITEMP  : GRID NUDGE THE TEMPERATURE FIELD FROM 3D ANALYSES?                PARAM.339
C              = 0 ; NO                                                          PARAM.340
C              = 1 ; YES                                                         PARAM.341
C                   ITEMP(1,1), IS FOR CGM ANALYSIS NUDGING                      PARAM.342
C                   ITEMP(MAXNES), IS FOR FGM ANALYSIS NUDGING                   PARAM.343
C            : GRID NUDGE THE TEMP FIELD FROM SURFACE ANALYSES?                  PARAM.344
C                   ITEMP(1,2), IS FOR CGM SURFACE-ANALYSIS NUDGING              PARAM.345
C                   ITEMP(INEST,2), IS FOR FGM SURFACE-ANALYSIS NUDGING          PARAM.346
C                                                                                PARAM.347
C     GT     : NUDGING COEFFICIENT FOR TEMPERATURES FROM 3D ANALYSES             PARAM.348
C                   GT(1,1), IS FOR CGM ANALYSIS NUDGING                         PARAM.349
C                   GT(MAXNES), IS FOR FGM ANALYSIS NUDGING                      PARAM.350
C            : NUDGING COEFFICIENT FOR TEMPERATURES FROM                         PARAM.351
C              SURFACE ANALYSES                                                  PARAM.352
C                   GT(1,2), IS FOR CGM SURFACE-ANALYSIS NUDGING                 PARAM.353
C                   GT(INEST,2), IS FOR FGM SURFACE-ANALYSIS NUDGING             PARAM.354
C                                                                                PARAM.355
C     IMOIS  : GRID NUDGE THE MIXING RATIO FIELD FROM 3D ANALYSES?               PARAM.356
C              = 0 ; NO                                                          PARAM.357
C              = 1 ; YES                                                         PARAM.358
C                   IMOIS(1,1), IS FOR CGM ANALYSIS NUDGING                      PARAM.359
C                   IMOIS(MAXNES), IS FOR FGM ANALYSIS NUDGING                   PARAM.360
C            : GRID NUDGE THE MIXING RATIO FIELD FROM SURFACE                    PARAM.361
C              ANALYSES?                                                         PARAM.362
C                   IMOIS(1,2), IS FOR CGM SURFACE-ANALYSIS NUDGING              PARAM.363
C                   IMOIS(INEST,2), IS FOR FGM SURFACE-ANALYSIS NUDGING          PARAM.364
C                                                                                PARAM.365
C     GQ     : NUDGING COEFFICIENT FOR THE MIXING RATIO FROM                     PARAM.366
C              3D ANALYSES                                                       PARAM.367
C                   GQ(1,1), IS FOR CGM ANALYSIS NUDGING                         PARAM.368
C                   GQ(MAXNES), IS FOR FGM ANALYSIS NUDGING                      PARAM.369
C            : NUDGING COEFFICIENT FOR THE MIXING RATIO FROM                     PARAM.370
C              SURFACE ANALYSES                                                  PARAM.371
C                   GQ(1,2), IS FOR CGM SURFACE-ANALYSIS NUDGING                 PARAM.372
C                   GQ(INEST,2), IS FOR FGM SURFACE-ANALYSIS NUDGING             PARAM.373
C                                                                                PARAM.374
C     IROT   : GRID NUDGE THE ROTATIONAL COMPONENT OF THE                        PARAM.375
C              WIND FIELD FROM 3D ANALYSES?                                      PARAM.376
C              = 0 ; NO                                                          PARAM.377
C              = 1 ; YES                                                         PARAM.378
C                   IROT(1,1), IS FOR CGM ANALYSIS NUDGING                       PARAM.379
C                   IROT(MAXNES), IS FOR FGM ANALYSIS NUDGING                    PARAM.380
C                                                                                PARAM.381
C     GR     : NUDGING COEFFICIENT FOR THE ROTATIONAL COMPONENT OF THE           PARAM.382
C              WINDS FROM 3D ANALYSES                                            PARAM.383
C                   GR(1,1), IS FOR CGM ANALYSIS NUDGING                         PARAM.384
C                   GR(MAXNES), IS FOR FGM ANALYSIS NUDGING                      PARAM.385
C                                                                                PARAM.386
C     INONBL : IF GRID NUDGING (I4D(1,1)=1) AND YOU WISH                         PARAM.387
C              TO EXCLUDE THE BOUNDARY LAYER FROM FDDA OF COARSE                 PARAM.388
C              GRID THREE DIMENSIONAL DATA (USUALLY FROM INTERP):                PARAM.389
C              (NOTE: IF NUDGING SURFACE DATA IN THE BOUNDARY LAYER              PARAM.390
C              (I4D(1,2)=1 OR I4D(INEST,2)=1) FOR THIS PARTICULAR VARIAB         PARAM.391
C              THIS SWITCH IS EFFECTIVELY SET TO 1.)                             PARAM.392
C              =0  ; NO, INCLUDE BOUNDARY LAYER NUDGING                          PARAM.393
C              =1  ; YES, EXCLUDE BOUNDARY LAYER NUDGING                         PARAM.394
C                   INONBL(1,1), CGM U                                           PARAM.395
C                   INONBL(1,2), CGM V                                           PARAM.396
C                   INONBL(1,3), CGM T                                           PARAM.397
C                   INONBL(1,4), CGM Q                                           PARAM.398
C            : IF GRID NUDGING (I4D(MAXNES)=1) AND YOU WISH                      PARAM.399
C              TO EXCLUDE THE BOUNDARY LAYER FROM FDDA OF FINE                   PARAM.400
C              GRID THREE DIMENSIONAL DATA (USUALLY FROM INTERP):                PARAM.401
C                   INONBL(2,1), FGM U                                           PARAM.402
C                   INONBL(2,2), FGM V                                           PARAM.403
C                   INONBL(2,3), FGM T                                           PARAM.404
C                   INONBL(2,4), FGM Q                                           PARAM.405
C                                                                                PARAM.406
C     ISWIND : OBS NUDGE THE WIND FIELD FROM STATION DATA?                       PARAM.407
C              = 0 ; NO                                                          PARAM.408
C              = 1 ; YES                                                         PARAM.409
C                   ISWIND(1), IS FOR CGM OBS NUDGING                            PARAM.410
C                   ISWIND(MAXNES), IS FOR FGM OBS NUDGING                       PARAM.411
C                                                                                PARAM.412
C     GIV    : NUDGING COEFFICIENT FOR WINDS FROM STATION DATA                   PARAM.413
C                   GIV(1), IS FOR CGM OBS NUDGING                               PARAM.414
C                   GIV(MAXNES), IS FOR FGM OBS NUDGING                          PARAM.415
C                                                                                PARAM.416
C     ISTEMP : OBS NUDGE THE TEMPERATURE FIELD FROM STATION DATA?                PARAM.417
C              = 0 ; NO                                                          PARAM.418
C              = 1 ; YES                                                         PARAM.419
C                   ISTEMP(1), IS FOR CGM OBS NUDGING                            PARAM.420
C                   ISTEMP(MAXNES), IS FOR FGM OBS NUDGING                       PARAM.421
C                                                                                PARAM.422
C     GIT    : NUDGING COEFFICIENT FOR TEMPERATURES FROM STATION                 PARAM.423
C              DATA                                                              PARAM.424
C                   GIT(1), IS FOR CGM OBS NUDGING                               PARAM.425
C                   GIT(MAXNES), IS FOR FGM OBS NUDGING                          PARAM.426
C                                                                                PARAM.427
C     ISMOIS : OBS NUDGE THE MIXING RATIO FIELD FROM STATION DATA?               PARAM.428
C              = 0 ; NO                                                          PARAM.429
C              = 1 ; YES                                                         PARAM.430
C                   ISMOIS(1), IS FOR CGM OBS NUDGING                            PARAM.431
C                   ISMOIS(MAXNES), IS FOR FGM OBS NUDGING                       PARAM.432
C                                                                                PARAM.433
C     GIQ    : NUDGING COEFFICIENT FOR THE MIXING RATIO FROM                     PARAM.434
C              3D ANALYSES                                                       PARAM.435
C                   GIQ(1), IS FOR CGM OBS NUDGING                               PARAM.436
C                   GIQ(MAXNES), IS FOR FGM OBS NUDGING                          PARAM.437
C                                                                                PARAM.438
C     RINBLW : IF I4D(1,2)=1 OR I4D(INEST,2)=1, DEFINE RINBLW (KM) USED          PARAM.439
C              SUBROUTINE BLW TO DETERMINE THE HORIZONTAL VARIABILITY            PARAM.440
C              OF THE SURFACE-ANALYSIS NUDGING AS A FUNCTION OF SURFACE          PARAM.441
C              DATA DENSITY.  OVER LAND, THE STRENGTH OF THE SURFACE-            PARAM.442
C              ANALYSIS NUDGING IS LINEARLY DECREASED BY 80 PERCENT AT           PARAM.443
C              THOSE GRID POINTS GREATER THAN RINBLW FROM AN OBSERVATION         PARAM.444
C              TO ACCOUNT FOR DECREASED CONFIDENCE IN THE ANALYSIS               PARAM.445
C              IN REGIONS NOT NEAR ANY OBSERVATIONS.                             PARAM.446
C                                                                                PARAM.447
C     NPFG   : SET THE NUDGING PRINT FREQUENCY FOR SELECTED DIAGNOSTIC           PARAM.448
C              PRINTS IN THE GRID (ANALYSIS) NUDGING CODE (IN CGM                PARAM.449
C              TIMESTEPS)                                                        PARAM.450
C                                                                                PARAM.451
C     RINXY  : THE OBS NUDGING RADIUS OF INFLUENCE IN THE                        PARAM.452
C              HORIZONTAL IN KM FOR CRESSMAN-TYPE DISTANCE-WEIGHTED              PARAM.453
C              FUNCTIONS WHICH SPREAD THE OBS-NUDGING CORRECTION                 PARAM.454
C              IN THE HORIZONTAL.                                                PARAM.455
C                                                                                PARAM.456
C     RINSIG : THE OBS NUDGING RADIUS OF INFLUENCE IN THE                        PARAM.457
C              VERTICAL IN SIGMA UNITS FOR CRESSMAN-TYPE DISTANCE-               PARAM.458
C              WEIGHTED FUNCTIONS WHICH SPREAD THE OBS-NUDGING                   PARAM.459
C              CORRECTION IN THE VERTICAL.                                       PARAM.460
C                                                                                PARAM.461
C     TWINDO : THE HALF-PERIOD OF THE TIME WINDOW, IN MINUTES, OVER              PARAM.462
C              WHICH AN OBSERVATION WILL AFFECT THE FORECAST VIA OBS             PARAM.463
C              NUDGING. THAT IS, THE OBS WILL INFLUENCE THE FORECAST             PARAM.464
C              FROM TIMEOBS-TWINDO TO TIMEOBS+TWINDO.  THE TEMPORAL              PARAM.465
C              WEIGHTING FUNCTION IS DEFINED SUCH THAT THE OBSERVATION           PARAM.466
C              IS APPLIED WITH FULL STRENGTH WITHIN TWINDO/2. MINUTES            PARAM.467
C              BEFORE OR AFTER THE OBSERVATION TIME, AND THEN LINEARLY           PARAM.468
C              DECREASES TO ZERO TWINDO MINUTES BEFORE OR AFTER THE              PARAM.469
C              OBSERVATION TIME.                                                 PARAM.470
C                                                                                PARAM.471
C     NPFI   : THE NUDGING PRINT FREQUENCY FOR SELECTED DIAGNOSTIC PRINT         PARAM.472
C              IN THE OBS NUDGING CODE (IN CGM TIMESTEPS)                        PARAM.473
C                                                                                PARAM.474
C                                                                                PARAM.475
C                                                                                PARAM.476
C----------------------------------------------------------------------          PARAM.477
                                                                                 PARAM.478
      IWARN=0                                                                    PARAM.479
      INIEXMS=0                                                                  PARAM.480
      INIICE=0                                                                   PARAM.481
      INIICEG=0                                                                  PARAM.482
C                                                                                PARAM.483
C-----INPUT THE COARSE GRID RECORD HEADER                                        PARAM.484
C                                                                                PARAM.485
      IF (DM_IONODE()) THEN                                                      24SEP99.33
        OPEN(11, FILE='MMINPUT_DOMAIN1',FORM='UNFORMATTED',                      24SEP99.34
     &       STATUS='OLD',ERR=9003)                                              25JAN00.59
      ENDIF                                                                      24SEP99.36
  100 CONTINUE                                                                   PARAM.488
      IF(DM_IONODE())READ (11,ERR=9001,END=9002)FLAG                             PARAM.489
#ifdef MPP1                                                                      PARAM.490
      CALL DM_BCAST_INTEGERS(FLAG,1)                                             PARAM.491
#endif                                                                           PARAM.492
      IF(FLAG.EQ.0)THEN                                                          PARAM.493
        IF(DM_IONODE())THEN                                                      PARAM.494
          READ (11,IOSTAT=IER)BHI,BHR,BHIC,BHRC                                  PARAM.495
          IF(IER.NE.0)THEN                                                       PARAM.496
            write(*,'("Error reading big header")')                              PARAM.497
            STOP                                                                 PARAM.498
          ENDIF                                                                  PARAM.499
        ENDIF                                                                    PARAM.500
#ifdef MPP1                                                                      PARAM.501
        CALL DM_BCAST_INTEGERS(BHI,NUMINT*NUMPROGS)                              PARAM.502
        CALL DM_BCAST_REALS(BHR,NUMREAL*NUMPROGS)                                25JAN00.60
#endif                                                                           PARAM.504
        IF(BHI(1,1).NE.5)THEN                                                    PARAM.505
          PRINT *,'THE INPUT FILE HAS AN INCORRECT PROGRAM NUMBER'               PARAM.506
          PRINT *,'THE PROGRAM NUMBER BHI(1,1) IS ',BHI(1,1)                     PARAM.507
          PRINT *,'THE PROGRAM NUMBER SHOULD BE 5'                               PARAM.508
          STOP 'INCORRECT INPUT FILE'                                            PARAM.509
        ENDIF                                                                    PARAM.510
        KXCHECK=BHI(12,5)                                                        PARAM.511
        IF(KXCHECK.NE.MKX)THEN                                                   PARAM.512
          PRINT *,'THE INPUT DATA DOES NOT MATCH THE EXPECTED LEVELS'            PARAM.513
          PRINT *,'   MKX IN MODEL = ',MKX                                       PARAM.514
          PRINT *,'   MKX IN INPUT FILE = ',KXCHECK                              PARAM.515
          STOP 'KX IN PARAM'                                                     PARAM.516
        ENDIF                                                                    PARAM.517
        GOTO 100                                                                 PARAM.518
      ELSEIF(FLAG.EQ.1)THEN                                                      PARAM.519
        IF(DM_IONODE())THEN                                                      PARAM.520
          READ (11,IOSTAT=IER)NDIM,START_INDEX,END_INDEX,RTIME                   PARAM.521
     +                       ,STAGGERING                                         PARAM.522
     +                       ,ORDERING,CURRENT_DATE,NAME,UNITS,                  PARAM.523
     +                       DESCRIPTION                                         PARAM.524
          IF(IER.NE.0)THEN                                                       PARAM.525
            write(*,'("Error reading subheader")')                               PARAM.526
            STOP                                                                 PARAM.527
          ENDIF                                                                  PARAM.528
        ENDIF                                                                    PARAM.529
#ifdef MPP1                                                                      PARAM.530
        CALL DM_BCAST_INTEGERS(NDIM,1)                                           PARAM.531
        CALL DM_BCAST_INTEGERS(START_INDEX,4)                                    PARAM.532
        CALL DM_BCAST_INTEGERS(END_INDEX,4)                                      PARAM.533
        CALL DM_BCAST_REALS(RTIME,1)                                             PARAM.534
        CALL DM_BCAST_STRING(STAGGERING,4)                                       PARAM.535
        CALL DM_BCAST_STRING(ORDERING,4)                                         PARAM.536
        CALL DM_BCAST_STRING(START_DATE,24)                                      PARAM.537
        CALL DM_BCAST_STRING(CURRENT_DATE,24)                                    PARAM.538
        CALL DM_BCAST_STRING(NAME,9)                                             PARAM.539
        CALL DM_BCAST_STRING(UNITS,25)                                           PARAM.540
        CALL DM_BCAST_STRING(DESCRIPTION,46)                                     PARAM.541
#endif                                                                           PARAM.542
        IF(NAME(1:8).EQ.'SIGMAH  ')THEN                                          PARAM.543
          IF(DM_IONODE())READ (11)A                                              PARAM.544
#ifdef MPP1                                                                      PARAM.545
          CALL DM_BCAST_REALS(A,MKX)                                             PARAM.546
#endif                                                                           PARAM.547
          GOTO 100                                                               PARAM.548
        ELSE                                                                     PARAM.549
c         detect moisture fields in input                                        PARAM.550
          IF(NAME(1:8).EQ.'CLW     ')INIEXMS=1                                   PARAM.551
          IF(NAME(1:8).EQ.'ICE     ')INIICE=1                                    PARAM.552
          IF(NAME(1:8).EQ.'GRAUPEL ')INIICEG=1                                   PARAM.553
          IF(DM_IONODE())READ (11)DUMMY                                          PARAM.554
          GOTO 100                                                               PARAM.555
        ENDIF                                                                    PARAM.556
      ELSEIF(FLAG.EQ.2)THEN                                                      PARAM.557
C       print *,' flag = 2'                                                      PARAM.558
        GOTO 101                                                                 PARAM.559
      ELSE                                                                       PARAM.560
        STOP                                                                     PARAM.561
      ENDIF                                                                      PARAM.562
  101 CONTINUE                                                                   PARAM.563
      SIGMA(1)=0.                                                                PARAM.564
      DO 2 L=1,MKX                                                               PARAM.565
        SIGMA(L+1)=2.*A(L)-SIGMA(L)                                              PARAM.566
        DSIGMA(L)=SIGMA(L+1)-SIGMA(L)                                            PARAM.567
    2 CONTINUE                                                                   PARAM.568
      SIGMA(MKX+1)=1.                                                            PARAM.569
c  assume MKS units                                                              PARAM.570
      DX=BHR(9,1)                                                                PARAM.571
      PTOP=BHR(2,2)*.001                                                         PARAM.572
      P0=BHR(2,BHI(1,1))                                                         PARAM.573
      TS0=BHR(3,BHI(1,1))                                                        PARAM.574
      TLP=BHR(4,BHI(1,1))                                                        PARAM.575
      TISO=BHR(5,BHI(1,1))                                                       PARAM.576
      WRITE (cdate,                                                              PARAM.577
     * '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)')                    PARAM.578
     * bhi(5,5),bhi(6,5),bhi(7,5),bhi(8,5),bhi(9,5),bhi(10,5)                    PARAM.579
      PRINT458,CDATE                                                             PARAM.580
458   FORMAT(1X,'STARTING DATE FOR THIS MM5V3 INPUT FILE IS ',A19)               PARAM.581
      PRINT*, 'INPUT DOMAIN ID IS ', BHI(13,1)                                   PARAM.582
      PRINT*, 'IX, JX, DX = ', BHI(16,1), BHI(17,1), DX, ' (METERS)'             PARAM.583
      PRINT457,P0,TS0,TLP,TISO                                                   PARAM.584
457   FORMAT(1X,'REFERENCE STATE PARAMETERS: P0=',F7.0,' TS0=',                  PARAM.585
     -     F6.2,' TLP=',F6.2,' TISO=',F6.2)                                      PARAM.586
      PRINT459,PTOP*10.                                                          24SEP99.37
459   FORMAT(1X,'PTOP = ',F10.3,' MB')                                           24SEP99.38
C-----SPECIFY THE SURFACE CHARACTERISTICS :                                      PARAM.587
C     ALBD   : SURFACE ALBEDO IN PERCENTAGE.                                     PARAM.588
C     SLMO   : SURFACE MOISTURE AVAILABILITY IN FRACTION OF ONE.                 PARAM.589
C     THERIN : SURFACE THERMAL INERTIAL IN CAL CM-2 K-1 S-0.5.                   PARAM.590
C     SFEM   : SURFACE EMISSIVITY AT 9 MICROMETER.                               PARAM.591
C     SFZ0   : SURFACE ROUGHNESS LENGTH IN CM.                                   PARAM.592
C     SFHC   : SURFACE HEAT CAPACITY PER UINT VOLUME IN J M-3 K-1.               PARAM.593
C                                                                                PARAM.594
C-----READ IN LAND-USE PROPERTIES FROM LANDUSE.TBL FILE                          PARAM.595
C                                                                                PARAM.596
C     THE WATER CATEGORY NUMBER IS IN THE TERRAIN HEADER BHI(11,1)               PARAM.597
C     THE LAND-USE FIELD IS UNIQUELY DESCRIBED BY BHIC(11,1)(1:4)                PARAM.598
C                                                                                PARAM.599
      IF (DM_IONODE()) THEN                                                      24SEP99.39
        OPEN(19, FILE='LANDUSE.TBL',FORM='FORMATTED',STATUS='OLD',               07NOV00.130
     -           ERR=9006)                                                       07NOV00.131
      ENDIF                                                                      24SEP99.41
      MMINLU=BHIC(23,1)(1:4)                                                     PARAM.601
      ISWATER=BHI(23,1)                                                          PARAM.602
      IF(MMINLU.EQ.'OLD ')ISICE=11                                               PARAM.604
      IF(MMINLU.EQ.'USGS')ISICE=24                                               PARAM.605
      IF(MMINLU.EQ.'SiB ')ISICE=16                                               PARAM.606
      CALL DM_BCAST_STRING( MMINLU , 4 )                                         07NOV00.132
      CALL DM_BCAST_INTEGERS ( ISICE ,  1 )                                      07NOV00.133
      CALL DM_BCAST_INTEGERS ( ISWATER ,  1 )                                    07NOV00.134
      PRINT *, 'INPUT LANDUSE = ',MMINLU                                         24SEP99.42
      IF (DM_IONODE()) THEN                                                      24SEP99.43
        LUMATCH=0                                                                24SEP99.44
 1999   CONTINUE                                                                 24SEP99.45
        READ (19,2000,END=2001)LUTYPE                                            24SEP99.46
        READ (19,*)LUCATS,LUSEAS                                                 24SEP99.47
 2000   FORMAT (A4)                                                              24SEP99.48
        IF(LUTYPE.EQ.MMINLU)THEN                                                 24SEP99.49
          LUN=LUCATS                                                             24SEP99.50
          NSN=LUSEAS                                                             24SEP99.51
            PRINT *, 'LANDUSE TYPE = ',LUTYPE,' FOUND',                          24SEP99.52
     -             LUCATS,' CATEGORIES',LUSEAS,' SEASONS',                       24SEP99.53
     -             ' WATER CATEGORY = ',ISWATER,                                 24SEP99.54
     -             ' SNOW CATEGORY = ',ISICE                                     24SEP99.55
          LUMATCH=1                                                              24SEP99.56
        ENDIF                                                                    24SEP99.57
        DO LS=1,LUSEAS                                                           24SEP99.58
          READ (19,*)                                                            24SEP99.59
          DO LC=1,LUCATS                                                         24SEP99.60
            IF(LUTYPE.EQ.MMINLU)THEN                                             24SEP99.61
              READ (19,*)LI,ALBD(LC,LS),SLMO(LC,LS),SFEM(LC,LS),                 24SEP99.62
     +                   SFZ0(LC,LS),THERIN(LC,LS),SCFX(LC),SFHC(LC,LS)          24SEP99.63
              IF(LC.NE.LI)STOP 'MISSING LANDUSE: UNIT 19'                        24SEP99.64
            ELSE                                                                 24SEP99.65
              READ (19,*)                                                        24SEP99.66
            ENDIF                                                                24SEP99.67
          ENDDO                                                                  24SEP99.68
        ENDDO                                                                    24SEP99.69
        GOTO 1999                                                                24SEP99.70
 2001   CONTINUE                                                                 24SEP99.71
      CLOSE (19)                                                                 19DEC02.220
        IF(LUMATCH.EQ.0)THEN                                                     24SEP99.72
          PRINT *,'LANDUSE IN INPUT FILE DOES NOT MATCH LUTABLE'                 24SEP99.73
          STOP 'INCONSISTENT OR MISSING LUTABLE FILE'                            24SEP99.74
        ENDIF                                                                    24SEP99.75
      ENDIF                                                                      24SEP99.76
      CALL DM_BCAST_STRING( LUTYPE , 4 )                                         07NOV00.135
      CALL DM_BCAST_STRING( MMINLU , 4 )                                         07NOV00.136
      CALL DM_BCAST_INTEGERS ( LUN   ,  1 )                                      07NOV00.137
      CALL DM_BCAST_INTEGERS ( NSN   ,  1 )                                      07NOV00.138
      CALL DM_BCAST_INTEGERS ( LUSEAS,  1 )                                      07NOV00.139
      CALL DM_BCAST_INTEGERS ( LUCATS,  1 )                                      07NOV00.140
      CALL DM_BCAST_INTEGERS ( LI,      1 )                                      07NOV00.141
      CALL DM_BCAST_REALS    ( ALBD,    NLU*NSEAS )                              07NOV00.142
      CALL DM_BCAST_REALS    ( SLMO,    NLU*NSEAS )                              07NOV00.143
      CALL DM_BCAST_REALS    ( SFEM,    NLU*NSEAS )                              07NOV00.144
      CALL DM_BCAST_REALS    ( SFZ0,    NLU*NSEAS )                              07NOV00.145
      CALL DM_BCAST_REALS    ( THERIN,  NLU*NSEAS )                              07NOV00.146
      CALL DM_BCAST_REALS    ( SCFX,    NLU       )                              07NOV00.147
      CALL DM_BCAST_REALS    ( SFHC,    NLU*NSEAS )                              07NOV00.148
                                                                                 19DEC02.221
C                                                                                19DEC02.222
C-----FOLLOWING 3 TABLES ARE REQUIRED ONLY FOR NOAH LSM                          19DEC02.223
C                                                                                19DEC02.224
#ifdef SOIL2                                                                     19DEC02.225
C                                                                                19DEC02.226
C-----SPECIFY VEGETATION RELATED CHARACTERISTICS :                               19DEC02.227
C             ALBEDO: SFC albedo (in percentage)                                 19DEC02.228
C                 Z0: Roughness length (m)                                       19DEC02.229
C             SHDFAC: Green vegetation fraction (in percentage)                  19DEC02.230
C  Note: The ALBEDO, Z0, and SHDFAC values read from the following table         19DEC02.231
C          ALBEDO, amd Z0 are specified in LAND-USE TABLE; and SHDFAC is         19DEC02.232
C          the monthly green vegetation data                                     19DEC02.233
C             CMXTBL: MAX CNPY Capacity (m)                                      19DEC02.234
C              NROOT: Rooting depth (M)                                          19DEC02.235
C              RSMIN: Mimimum stomatal resistance (s m-1)                        19DEC02.236
C              RSMAX: Max. stomatal resistance (s m-1)                           19DEC02.237
C                RGL: Parameters used in radiation stress function               19DEC02.238
C                 HS: Parameter used in vapor pressure deficit functio           19DEC02.239
C               TOPT: Optimum transpiration air temperature. (K)                 19DEC02.240
C             CMCMAX: Maximum canopy water capacity                              19DEC02.241
C             CFACTR: Parameter used in the canopy inteception calculati         19DEC02.242
C               SNUP: Threshold snow depth (in water equivalent m) that          19DEC02.243
C                     implies 100% snow cover                                    19DEC02.244
C                LAI: Leaf area index (dimensionless)                            19DEC02.245
C             MAXALB: Upper bound on maximum albedo over deep snow               19DEC02.246
C                                                                                19DEC02.247
C-----READ IN VEGETAION PROPERTIES FROM VEGPARM.TBL                              19DEC02.248
C                                                                                19DEC02.249
         IF (DM_IONODE()) THEN                                                   19DEC02.250
           OPEN(19, FILE='VEGPARM.TBL',FORM='FORMATTED',STATUS='OLD',            19DEC02.251
     -              ERR=9007)                                                    23DEC04.123
         ENDIF                                                                   19DEC02.253
         PRINT *, 'INPUT LANDUSE = ',MMINLU                                      19DEC02.254
         IF (DM_IONODE()) THEN                                                   19DEC02.255
           LUMATCH=0                                                             19DEC02.256
C1999   CONTINUE                                                                 19DEC02.257
           READ (19,*)                                                           19DEC02.258
           READ (19,2000,END=2002)LUTYPE                                         19DEC02.259
           READ (19,*)LUCATS,IINDEX                                              19DEC02.260
           IF(LUTYPE.EQ.MMINLU)THEN                                              19DEC02.261
             LUN=LUCATS                                                          19DEC02.262
               PRINT *, 'LANDUSE TYPE = ',LUTYPE,' FOUND',                       19DEC02.263
     -                LUCATS,' CATEGORIES'                                       19DEC02.264
             LUMATCH=1                                                           19DEC02.265
           ENDIF                                                                 19DEC02.266
               IF(LUTYPE.EQ.MMINLU)THEN                                          19DEC02.267
             DO LC=1,LUCATS                                                      19DEC02.268
                 READ (19,*)IINDEX,DUMMY,DUMMY,DUMMY,                            19DEC02.269
     -                      NROTBL(LC),RSTBL(LC),RGLTBL(LC),HSTBL(LC),           19DEC02.270
     -                      SNUPTBL(LC),LAITBL(LC),MAXALB(LC)                    19DEC02.271
             ENDDO                                                               19DEC02.272
             READ (19,*)                                                         19DEC02.273
             READ (19,*)TOPT_DATA                                                19DEC02.274
             READ (19,*)                                                         19DEC02.275
             READ (19,*)CMCMAX_DATA                                              19DEC02.276
             READ (19,*)                                                         19DEC02.277
             READ (19,*)CFACTR_DATA                                              19DEC02.278
             READ (19,*)                                                         19DEC02.279
             READ (19,*)RSMAX_DATA                                               19DEC02.280
             READ (19,*)                                                         19DEC02.281
             READ (19,*)BARE                                                     19DEC02.282
              ENDIF                                                              19DEC02.283
C       GOTO 1999                                                                19DEC02.284
 2002   CONTINUE                                                                 19DEC02.285
         CLOSE (19)                                                              19DEC02.286
           IF(LUMATCH.EQ.0)THEN                                                  19DEC02.287
             PRINT *,'LANDUSE IN INPUT FILE DOES NOT MATCH ',                    19DEC02.288
     -               'VEGPARM TABLE'                                             19DEC02.289
             STOP 'INCONSISTENT OR MISSING VEGPARM FILE'                         19DEC02.290
           ENDIF                                                                 19DEC02.291
         ENDIF                                                                   19DEC02.292
         CALL DM_BCAST_STRING( LUTYPE , 4 )                                      19DEC02.293
         CALL DM_BCAST_INTEGERS ( LUCATS,  1 )                                   19DEC02.294
         CALL DM_BCAST_INTEGERS ( BARE,  1 )                                     19DEC02.295
         CALL DM_BCAST_REALS    (TOPT_DATA, 1)                                   19DEC02.296
         CALL DM_BCAST_REALS    (CMCMAX_DATA, 1)                                 19DEC02.297
         CALL DM_BCAST_REALS    (CFACTR_DATA, 1)                                 19DEC02.298
         CALL DM_BCAST_REALS    (RSMAX_DATA, 1)                                  19DEC02.299
         CALL DM_BCAST_REALS    ( ALBTBL,    NLU )                               19DEC02.300
         CALL DM_BCAST_REALS    ( Z0TBL,    NLU )                                19DEC02.301
         CALL DM_BCAST_REALS    ( SHDTBL,    NLU )                               19DEC02.302
         CALL DM_BCAST_INTEGERS    ( NROTBL,    NLU )                            19DEC02.303
         CALL DM_BCAST_REALS    ( RSTBL,    NLU )                                19DEC02.304
         CALL DM_BCAST_REALS    ( RGLTBL,    NLU )                               19DEC02.305
         CALL DM_BCAST_REALS    ( HSTBL,    NLU )                                19DEC02.306
         CALL DM_BCAST_REALS    ( SNUPTBL,    NLU )                              19DEC02.307
         CALL DM_BCAST_REALS    ( LAITBL,    NLU )                               19DEC02.308
         CALL DM_BCAST_REALS    ( MAXALB,    NLU )                               19DEC02.309
C                                                                                19DEC02.310
C                                                                                19DEC02.311
C-----SPECIFY SOIL TEXTURE RELATED CHARACTERISTICS :                             19DEC02.312
C        SMCMAX: MAX soil moisture content (porosity)                            19DEC02.313
C        SMCREF: Reference soil moisture  (field capacity)                       19DEC02.314
C        SMCWLT: Wilting point soil moisture                                     19DEC02.315
C        SMCDRY: Air dry soil moist content limits                               19DEC02.316
C        SATPSI: SAT (saturation) soil potential                                 19DEC02.317
C         DKSAT: SAT soil conductivity                                           19DEC02.318
C          BEXP: B parameter                                                     19DEC02.319
C        SSATDW: SAT soil diffusivity                                            19DEC02.320
C           F1: Soil thermal diffusivity/conductivity coef.                      19DEC02.321
C        QUARTZ: Soil quartz content                                             19DEC02.322
C                                                                                19DEC02.323
C-----READ IN SOIL PROPERTIES FROM SOILPARM.TBL                                  19DEC02.324
C                                                                                19DEC02.325
         IF (DM_IONODE()) THEN                                                   19DEC02.326
           OPEN(19, FILE='SOILPARM.TBL',FORM='FORMATTED',STATUS='OLD',           19DEC02.327
     -              ERR=9008)                                                    23DEC04.124
         ENDIF                                                                   19DEC02.329
         MMINSL='STAS'                                                           19DEC02.330
         PRINT *, 'INPUT SOIL TEXTURE CLASSIFICAION = ',MMINSL                   19DEC02.331
         IF (DM_IONODE()) THEN                                                   19DEC02.332
           LUMATCH=0                                                             19DEC02.333
C1999   CONTINUE                                                                 19DEC02.334
           READ (19,*)                                                           19DEC02.335
           READ (19,2000,END=2003)SLTYPE                                         19DEC02.336
           READ (19,*)SLCATS,IINDEX                                              19DEC02.337
           IF(SLTYPE.EQ.MMINSL)THEN                                              19DEC02.338
               PRINT *, 'SOIL TEXTURE CLASSIFICATION = ',SLTYPE,                 19DEC02.339
     -                  ' FOUND',SLCATS,' CATEGORIES'                            19DEC02.340
             LUMATCH=1                                                           19DEC02.341
           ENDIF                                                                 19DEC02.342
               IF(SLTYPE.EQ.MMINSL)THEN                                          19DEC02.343
             DO LC=1,SLCATS                                                      19DEC02.344
                 READ (19,*)IINDEX,BB(LC),DRYSMC(LC),F11(LC),MAXSMC(LC),         19DEC02.345
     -                      REFSMC(LC),SATPSI(LC),SATDK(LC), SATDW(LC),          19DEC02.346
     -                      WLTSMC(LC), QTZ(LC)                                  19DEC02.347
             ENDDO                                                               19DEC02.348
              ENDIF                                                              19DEC02.349
C       GOTO 1999                                                                19DEC02.350
 2003   CONTINUE                                                                 19DEC02.351
         CLOSE (19)                                                              19DEC02.352
           IF(LUMATCH.EQ.0)THEN                                                  19DEC02.353
         PRINT *,'SOIL TEXTURE IN INPUT FILE DOES NOT MATCH SOILPARM ',          19DEC02.354
     -           'TABLE'                                                         19DEC02.355
         STOP 'INCONSISTENT OR MISSING SOILPARM FILE'                            19DEC02.356
           ENDIF                                                                 19DEC02.357
         ENDIF                                                                   19DEC02.358
         CALL DM_BCAST_STRING( SLTYPE , 4 )                                      19DEC02.359
         CALL DM_BCAST_INTEGERS ( SLCATS, 1 )                                    19DEC02.360
         CALL DM_BCAST_REALS    ( BB,     NSLTYPE )                              19DEC02.361
         CALL DM_BCAST_REALS    ( DRYSMC, NSLTYPE )                              19DEC02.362
         CALL DM_BCAST_REALS    ( F11,    NSLTYPE )                              19DEC02.363
         CALL DM_BCAST_REALS    ( MAXSMC, NSLTYPE )                              19DEC02.364
         CALL DM_BCAST_REALS    ( REFSMC, NSLTYPE )                              19DEC02.365
         CALL DM_BCAST_REALS    ( SATPSI, NSLTYPE )                              19DEC02.366
         CALL DM_BCAST_REALS    ( SATDK,  NSLTYPE )                              19DEC02.367
         CALL DM_BCAST_REALS    ( SATDW,  NSLTYPE )                              19DEC02.368
         CALL DM_BCAST_REALS    ( WLTSMC, NSLTYPE )                              19DEC02.369
         CALL DM_BCAST_REALS    ( QTZ,    NSLTYPE )                              19DEC02.370
C                                                                                19DEC02.371
C                                                                                19DEC02.372
C-----SPECIFY GENERAL PARAMETERS OF LSM                                          19DEC02.373
C ----------------------------------------------------------------------         19DEC02.374
C Class parameter 'SLOPETYP' was included to estimate linear reservoir           19DEC02.375
C coefficient 'SLOPE' to the baseflow runoff out of the bottom layer.            19DEC02.376
C lowest class (slopetyp=0) means highest slope parameter = 1.                   19DEC02.377
C definition of slopetyp from 'zobler' slope type:                               19DEC02.378
C slope class  percent slope                                                     19DEC02.379
C 1            0-8                                                               19DEC02.380
C 2            8-30                                                              19DEC02.381
C 3            > 30                                                              19DEC02.382
C 4            0-30                                                              19DEC02.383
C 5            0-8 & > 30                                                        19DEC02.384
C 6            8-30 & > 30                                                       19DEC02.385
C 7            0-8, 8-30, > 30                                                   19DEC02.386
C 9            GLACIAL ICE                                                       19DEC02.387
C BLANK        OCEAN/SEA                                                         19DEC02.388
C ----------------------------------------------------------------------         19DEC02.389
C NOTE:                                                                          19DEC02.390
C CLASS 9 FROM 'ZOBLER' FILE SHOULD BE REPLACED BY 8 AND 'BLANK' 9               19DEC02.391
C ----------------------------------------------------------------------         19DEC02.392
C       SLOPE_DATA: linear reservoir coefficient                                 19DEC02.393
C       SBETA_DATA: parameter used to caluculate vegetation effect on soil heat  19DEC02.394
C       FXEXP_DAT:  soil evaporation exponent used in DEVAP                      19DEC02.395
C       CSOIL_DATA: soil heat capacity [J M-3 K-1]                               19DEC02.396
C       SALP_DATA: shape parameter of  distribution function of snow cover       19DEC02.397
C       REFDK_DATA and REFKDT_DATA: parameters in the surface runoff parameteri  19DEC02.398
C       FRZK_DATA: frozen ground parameter                                       19DEC02.399
C       ZBOT_DATA: depth[M] of lower boundary soil temperature                   19DEC02.400
C       CZIL_DATA: calculate roughness length of heat                            19DEC02.401
C       SMLOW_DATA and MHIGH_DATA: two soil moisture wilt, soil moisture refere  19DEC02.402
C                                                                                19DEC02.403
C-----READ IN GENERAL PARAMETERS FROM GENPARM.TBL                                19DEC02.404
C                                                                                19DEC02.405
         IF (DM_IONODE()) THEN                                                   19DEC02.406
           OPEN(19, FILE='GENPARM.TBL',FORM='FORMATTED',STATUS='OLD',            19DEC02.407
     -              ERR=9009)                                                    23DEC04.125
         ENDIF                                                                   19DEC02.409
         IF (DM_IONODE()) THEN                                                   19DEC02.410
           READ (19,*)                                                           19DEC02.411
           READ (19,*)                                                           19DEC02.412
           READ (19,*) NUM_SLOPE                                                 19DEC02.413
             SLPCATS=NUM_SLOPE                                                   19DEC02.414
             DO LC=1,SLPCATS                                                     19DEC02.415
                 READ (19,*)SLOPE_DATA(LC)                                       19DEC02.416
             ENDDO                                                               19DEC02.417
             READ (19,*)                                                         19DEC02.418
             READ (19,*)SBETA_DATA                                               19DEC02.419
             READ (19,*)                                                         19DEC02.420
             READ (19,*)FXEXP_DATA                                               19DEC02.421
             READ (19,*)                                                         19DEC02.422
             READ (19,*)CSOIL_DATA                                               19DEC02.423
             READ (19,*)                                                         19DEC02.424
             READ (19,*)SALP_DATA                                                19DEC02.425
             READ (19,*)                                                         19DEC02.426
             READ (19,*)REFDK_DATA                                               19DEC02.427
             READ (19,*)                                                         19DEC02.428
             READ (19,*)REFKDT_DATA                                              19DEC02.429
             READ (19,*)                                                         19DEC02.430
             READ (19,*)FRZK_DATA                                                19DEC02.431
             READ (19,*)                                                         19DEC02.432
             READ (19,*)ZBOT_DATA                                                19DEC02.433
             READ (19,*)                                                         19DEC02.434
             READ (19,*)CZIL_DATA                                                19DEC02.435
             READ (19,*)                                                         19DEC02.436
             READ (19,*)SMLOW_DATA                                               19DEC02.437
             READ (19,*)                                                         19DEC02.438
             READ (19,*)SMHIGH_DATA                                              19DEC02.439
          ENDIF                                                                  19DEC02.440
         CLOSE (19)                                                              19DEC02.441
                                                                                 19DEC02.442
         CALL DM_BCAST_INTEGERS ( SLPCATS,  1 )                                  19DEC02.443
         CALL DM_BCAST_REALS    ( SLOPE_DATA,   NSLOPE)                          19DEC02.444
         CALL DM_BCAST_REALS    (SBETA_DATA, 1)                                  19DEC02.445
         CALL DM_BCAST_REALS    (FXEXP_DATA, 1)                                  19DEC02.446
         CALL DM_BCAST_REALS    (CSOIL_DATA, 1)                                  19DEC02.447
         CALL DM_BCAST_REALS    (SALP_DATA, 1)                                   19DEC02.448
         CALL DM_BCAST_REALS    (REFDK_DATA, 1)                                  19DEC02.449
         CALL DM_BCAST_REALS    (REFKDT_DATA, 1)                                 19DEC02.450
         CALL DM_BCAST_REALS    (FRZK_DATA, 1)                                   19DEC02.451
         CALL DM_BCAST_REALS    (ZBOT_DATA, 1)                                   19DEC02.452
         CALL DM_BCAST_REALS    (CZIL_DATA, 1)                                   19DEC02.453
         CALL DM_BCAST_REALS    (SMLOW_DATA, 1)                                  19DEC02.454
         CALL DM_BCAST_REALS    (SMHIGH_DATA, 1)                                 19DEC02.455
#endif                                                                           19DEC02.456
C                                                                                PARAM.645
C-----DEFAULT VALUES FOR ALL THE OPTIONS:                                        PARAM.646
C     (CAN BE OVERWRITTEN BY NAMELIST INPUT).                                    PARAM.647
C-----NAMELIST OPARAM:                                                           PARAM.648
C                                                                                PARAM.649
      IFREST=.FALSE.                                                             PARAM.650
      IXTIMR=0                                                                   PARAM.651
      IFSAVE=.FALSE.                                                             PARAM.652
      SVLAST=.FALSE.                                                             PARAM.653
      SAVFRQ=180.                                                                PARAM.654
      NTSAVE=0                                                                   PARAM.655
      IFTAPE=1                                                                   PARAM.656
      TAPFRQ=180.                                                                PARAM.657
      NTTAPE=0                                                                   PARAM.658
      BUFFRQ=0.                                                                  PARAM.659
      ITAPCNT=0                                                                  PARAM.660
      IFSKIP=.FALSE.                                                             PARAM.661
      CDATEST='0000-00-00_00:00:00'                                              PARAM.662
      IFRSFA=.FALSE.                                                             23DEC04.126
      IFPRT=1                                                                    PARAM.663
      PRTFRQ=360.                                                                PARAM.664
      MASCHK=10                                                                  PARAM.665
      ISFOUT=0                                                                   PARAM.666
      DO I=1,MAXNES                                                              PARAM.667
        NUMNC(I)=1                                                               PARAM.668
        LEVIDN(I)=1                                                              PARAM.669
        INCTAP(I)=1                                                              PARAM.670
      ENDDO                                                                      PARAM.671
C  INITIALIZING TIME SERIES PARAMETERS                                           07NOV00.149
      IFTSOUT=.FALSE.                                                            07NOV00.150
      DO I=1,ITSDIM                                                              07NOV00.151
         TSLAT(I)=999.                                                           07NOV00.152
         TSLON(I)=999.                                                           07NOV00.153
         DO N=1,MAXNES                                                           07NOV00.154
            ITSN(N,I)=1                                                          07NOV00.155
            JTSN(N,I)=1                                                          07NOV00.156
         ENDDO                                                                   07NOV00.157
      ENDDO                                                                      07NOV00.158
#ifdef MPP1                                                                      PARAM.672
      NPROC_LT=-1                                                                PARAM.673
      NPROC_LN=-1                                                                PARAM.674
      MPP_IO_NODE=0                                                              PARAM.675
#  include <mpp_param_15.incl>                                                   PARAM.676
#endif                                                                           PARAM.677
C                                                                                PARAM.678
C----------------------------------------------------------------------          PARAM.679
C-----NAMELIST LPARAM:                                                           PARAM.680
C                          SET DEFAULTS!!!!                                      PARAM.681
C                                                                                PARAM.682
      IACTIV(1)=1                                                                PARAM.683
      IMOVCO(1)=1                                                                PARAM.684
      IBLTYP(1)=1                                                                PARAM.685
      IBOUDY(1)=3                                                                PARAM.686
      IDRY(1)=0                                                                  PARAM.687
      IMOIST(1)=1                                                                PARAM.688
      ICUPA(1)=1                                                                 PARAM.689
      IFOGMD(1)=0                                                                PARAM.690
      IMPHYS(1)=1                                                                PARAM.691
      XMOIST(1)=1.                                                               PARAM.692
      ISFFLX(1)=1                                                                PARAM.693
      ITGFLG(1)=1                                                                PARAM.694
      ISFPAR(1)=1                                                                PARAM.695
      ICLOUD(1)=1                                                                PARAM.696
      ICDCON(1)=0                                                                PARAM.697
      ISSTVAR=0                                                                  07NOV00.159
      IZ0TOPT=0                                                                  05DEC01.127
      ISFMTHD=1                                                                  23DEC04.127
      RDMAXALB=.FALSE.                                                           19DEC02.457
      RDBRDALB=.FALSE.                                                           19DEC02.458
      IFSNOW(1)=0                                                                PARAM.698
      IMOIAV(1)=0                                                                PARAM.699
      IVMIXM(1)=1                                                                PARAM.700
      IOVERW(1)=1                                                                PARAM.701
      ISHALLO(1)=0                                                               PARAM.702
      HYDPRE(1)=1.                                                               PARAM.703
      IEVAP(1)=1                                                                 PARAM.704
      IFDRY=0                                                                    PARAM.705
      IEXICE=1                                                                   PARAM.706
      IFRAD=2                                                                    PARAM.707
      RADFRQ=30.                                                                 PARAM.708
      IBMOIST=0                                                                  PARAM.709
      IMVDIF=1                                                                   PARAM.710
      IVQADV=1                                                                   PARAM.711
      IVTADV=1                                                                   PARAM.712
      ITHADV=1                                                                   PARAM.713
      ITPDIF=1                                                                   PARAM.714
      TDKORR=2                                                                   23DEC04.128
      ICOR3D=1                                                                   PARAM.715
      IFUPR=1                                                                    PARAM.716
      LEVSLP=9                                                                   23DEC04.129
      OROSHAW=0                                                                  23DEC04.130
      ITADVM(1)=0                                                                23DEC04.131
      IQADVM(1)=0                                                                23DEC04.132
      ISOIL=1                                                                    PARAM.717
      ICUSTB=1                                                                   PARAM.718
      FDASTA(1)=0.                                                               PARAM.719
      FDAEND(1)=0.                                                               PARAM.720
      KXOUT(1)=MKX                                                               PARAM.721
      JXSEX(1)=1                                                                 PARAM.722
      NESTIX(1)=1                                                                PARAM.723
      NESTJX(1)=1                                                                PARAM.724
      NESTI(1)=1                                                                 PARAM.725
      NESTJ(1)=1                                                                 PARAM.726
      XSTNES(1)=0.                                                               PARAM.727
      XENNES(1)=0.                                                               PARAM.728
      IMOVE(1)=0                                                                 PARAM.729
      IMOVCO(1)=1                                                                PARAM.730
      DO NN2=1,MAXMV                                                             PARAM.731
        IMOVEI(1,NN2)=0                                                          PARAM.732
        IMOVEJ(1,NN2)=0                                                          PARAM.733
        IMOVET(1,NN2)=0                                                          PARAM.734
      ENDDO                                                                      PARAM.735
      DO NN3=1,ISLYR 
        ISTLYR(NN3)=0                                                            24SEP99.78
        ISMLYR(NN3)=0                                                            24SEP99.79
      ENDDO                                                                      24SEP99.80
#if defined (SOIL3)
      ISMLYR(1)=10
      ISMLYR(2)=200 
#elif defined (SOIL2)
      ISTLYR(1)=10  
      ISTLYR(2)=40 
      ISTLYR(3)=100
      ISTLYR(4)=200 
      ISMLYR(1)=10   
      ISMLYR(2)=40  
      ISMLYR(3)=100 
      ISMLYR(4)=200 
#endif             
      ISMRD=0        ! default SM init by MAVAIL                                 07NOV00.165
      NUDGE=0        ! default is no SM nudge                                    07NOV00.166
      IFGROW=0       ! default is to use VEGFRAC                                 07NOV00.167
C SET DEFAULTS FOR SEA ICE FLAG IEXSI                                            19DEC02.459
      IEXSI=0                                                                    19DEC02.460
C                                                                                PARAM.736
C----------------------------------------------------------------------          PARAM.737
C.....FOR NESTED DOMAINS:                                                        PARAM.738
C                                                                                PARAM.739
#ifdef NESTED                                                                    PARAM.740
      IF(MAXNES.GT.1)THEN                                                        PARAM.741
        DO 10 N=2,MAXNES                                                         PARAM.742
          IBLTYP(N)=IBLTYP(1)                                                    PARAM.743
          IBOUDY(N)=2                                                            PARAM.744
          IDRY(N)=IDRY(1)                                                        PARAM.745
          IMOIST(N)=IMOIST(1)                                                    PARAM.746
          ICUPA(N)=ICUPA(1)                                                      PARAM.747
          IFOGMD(N)=IFOGMD(1)                                                    PARAM.748
          IMPHYS(N)=IMPHYS(1)                                                    PARAM.749
          XMOIST(N)=XMOIST(1)                                                    PARAM.750
          ISFFLX(N)=ISFFLX(1)                                                    PARAM.751
          ITGFLG(N)=ITGFLG(1)                                                    PARAM.752
          ISFPAR(N)=ISFPAR(1)                                                    PARAM.753
          ICLOUD(N)=ICLOUD(1)                                                    PARAM.754
          ICDCON(N)=ICDCON(1)                                                    PARAM.755
          IFSNOW(N)=IFSNOW(1)                                                    PARAM.756
          IMOIAV(N)=IMOIAV(1)                                                    PARAM.757
          IVMIXM(N)=IVMIXM(1)                                                    PARAM.758
          HYDPRE(N)=HYDPRE(1)                                                    PARAM.759
          IEVAP(N)=IEVAP(1)                                                      PARAM.760
          ITADVM(n)=ITADVM(1)                                                    23DEC04.141
          IQADVM(n)=IQADVM(1)                                                    23DEC04.142
          IOVERW(N)=0                                                            PARAM.761
          ISHALLO(N)=0                                                           PARAM.762
          KXOUT(N)=1                                                             PARAM.763
          JXSEX(N)=1                                                             PARAM.764
          NESTI(N)=10                                                            PARAM.765
          NESTJ(N)=10                                                            PARAM.766
          XSTNES(N)=1.E12                                                        PARAM.767
          XENNES(N)=0.                                                           PARAM.768
          IACTIV(N)=0.                                                           PARAM.769
          IMOVE(N)=0                                                             PARAM.770
          IMOVCO(N)=1                                                            PARAM.771
          NESTIX(N)=1                                                            PARAM.772
          NESTJX(N)=1                                                            PARAM.773
          LFBTER(N)=.TRUE.                                                       PARAM.774
          DO 10 NN2=1,MAXMV                                                      PARAM.775
            IMOVET(N,NN2)=0                                                      PARAM.776
            IMOVEI(N,NN2)=0                                                      PARAM.777
            IMOVEJ(N,NN2)=0                                                      PARAM.778
   10     CONTINUE                                                               PARAM.779
                                                                                 PARAM.780
      ENDIF                                                                      PARAM.781
#endif                                                                           PARAM.782
C                                                                                PARAM.783
C----------------------------------------------------------------------          PARAM.784
C                                                                                PARAM.785
C                                                                                PARAM.786
C-----NAMELIST PPARAM:                                                           PARAM.787
C                                                                                PARAM.788
      TIMAX=0.                                                                   PARAM.789
      TISTEP=0.                                                                  PARAM.790
      IFEED=3                                                                    PARAM.791
      ZZLND=0.1                                                                  PARAM.792
      ZZWTR=0.0001                                                               PARAM.793
      ALBLND=0.15                                                                PARAM.794
      THINLD=0.04                                                                PARAM.795
      XMAVA=0.3                                                                  PARAM.796
C                                                                                PARAM.797
      CONF=1.0                                                                   PARAM.798
      QCTH=0.5E-3                                                                PARAM.799
      QCK1=1.0E-3                                                                PARAM.800
      IABSOR=0                                                                   PARAM.801
#ifdef ICUPA3                                                                    PARAM.802
C                                                                                PARAM.803
C     PARAMETERS FOR GRELL SCHEME NAMELIST VARIABLES IN PARGRCU.INCL             PARAM.804
C                                                                                PARAM.805
C IFDTSET ALLOWS SETTING DTIME USED IN CALCULATING LARGE-SCALE FORCING           PARAM.806
      IFDTSET=.FALSE.                                                            PARAM.807
      DTIMSET=900.                                                               PARAM.808
C PBCMAX IS MAX ALLOWED DEPTH (MB) OF STABLE LAYER BETWEEN LCL AND LFC           PARAM.809
      PBCMAX=50.                                                                 PARAM.810
C WHETHER TO LIMIT CLOUD DEPTH IN TERMS OF HEIGHT                                PARAM.811
      IFCDZ=.FALSE.                                                              PARAM.812
C MINIMUM CLOUD DEPTH IN METERS USED IF IFCDZ=.TRUE.                             PARAM.813
      CDMINZ=1000.                                                               PARAM.814
C MINIMUM CLOUD DEPTH IN PRESSURE (MB) USED IF IFCDZ=.FALSE.                     PARAM.815
      CDMINP=150.                                                                PARAM.816
C LIMITS OF PRECIP EFFICIENCY                                                    PARAM.817
      EDTMIN=0.2                                                                 PARAM.818
      EDTMAX=0.8                                                                 PARAM.819
C LIMITS FOR SHEAR DEPENDENCE (SETTING SAME WILL REMOVE EFFECT)                  PARAM.820
      EDTSMN=0.3                                                                 PARAM.821
      EDTSMX=0.9                                                                 PARAM.822
C FREEZING IN UPDRAFT BELOW THIS TEMPERATURE                                     PARAM.823
      TCRIT=273.15                                                               PARAM.824
C MAXIMUM CLOUD BASE HEIGHT ALLOWED IN TERMS OF SIGMA                            PARAM.825
      SIGCBHI=0.4                                                                PARAM.826
C DEPTH OF DOWNDRAFT DETRAINMENT (MB)                                            PARAM.827
      PKDCUT=75.                                                                 PARAM.828
C LIMITS ON CONVECTIVE HEATING/COOLING                                           PARAM.829
      CUQMAX=500.                                                                PARAM.830
      CUQMIN=-250.                                                               PARAM.831
#endif                                                                           PARAM.832
C                                                                                PARAM.833
C ADDED TO PPARAM NAMELIST                                                       PARAM.834
C SOLSET ALLOWS SETTING SOLAR CONSTANT (<0 MEANS USE STANDARD FORMULA)           PARAM.835
      SOLSET=-999.                                                               PARAM.836
C CHARNOCK PARAMETER SET HERE                                                    PARAM.837
      CZO=0.032                                                                  PARAM.838
      OZO=1.E-4                                                                  PARAM.839
C CONSTANT FOR COMPUTING CONVECTIVE VELOCITY                                     PARAM.840
      VCONVC=2.                                                                  PARAM.841
C CONSTANT FOR COMPUTING B PARAMETER IN KUO SCHEME                               PARAM.842
      BFAC=2.                                                                    PARAM.843
C CLOUD RADIUS IN KAIN-FRITSCH SCHEME                                            PARAM.844
      RAD=1500.                                                                  PARAM.845
C RELAXATION TIME IN BETTS-MILLER SCHEME                                         PARAM.846
      TREL=3000.                                                                 PARAM.847
C SOIL-STEP FACTOR (LARGER IS MORE CONSERVATIVE, RECOMMEND 1.0-2.0)              07NOV00.168
      SOILFAC=1.25                                                               05DEC01.128
C CONSTANT FOR BACKGROUND HORIZONTAL DIFFUSION (M/S)                             05DEC01.129
      CKH=1.0                                                                    05DEC01.130
C TIME FREQUENCY IN MINUTES FOR KF2 DIAGNOSTIC OUTPUT                            05DEC01.131
      FRQKFDIAG=60.                                                              05DEC01.132
C                                                                                PARAM.848
C------NAMELIST FPARAM:                                                          PARAM.849
C                                                                                PARAM.850
      I4D(1,1)=0                                                                 PARAM.851
      I4D(1,2)=0                                                                 PARAM.852
      DIFTIM(1,1)=720.                                                           PARAM.853
      DIFTIM(1,2)=180.                                                           PARAM.854
      IWIND(1,1)=1                                                               PARAM.855
      IWIND(1,2)=1                                                               PARAM.856
      GV(1,1)=3.E-4                                                              PARAM.857
      GV(1,2)=3.E-4                                                              PARAM.858
      ITEMP(1,1)=1                                                               PARAM.859
      ITEMP(1,2)=0                                                               PARAM.860
      GT(1,1)=3.E-4                                                              PARAM.861
      GT(1,2)=3.E-4                                                              PARAM.862
      IMOIS(1,1)=1                                                               PARAM.863
      IMOIS(1,2)=1                                                               PARAM.864
      GQ(1,1)=1.E-5                                                              PARAM.865
      GQ(1,2)=3.E-4                                                              PARAM.866
      INONBL(1,1)=0                                                              PARAM.867
      INONBL(1,2)=0                                                              PARAM.868
      INONBL(1,3)=1                                                              PARAM.869
      INONBL(1,4)=0                                                              PARAM.870
      IROT(1)=0                                                                  PARAM.871
      GR(1)=5.E6                                                                 PARAM.872
      GIQ(1)=3.E-4                                                               PARAM.873
      I4DI(1)=0                                                                  PARAM.874
      ISWIND(1)=1                                                                PARAM.875
      GIV(1)=3.E-4                                                               PARAM.876
      ISTEMP(1)=1                                                                PARAM.877
      GIT(1)=3.E-4                                                               PARAM.878
      ISMOIS(1)=1                                                                PARAM.879
C                                                                                PARAM.880
C DEFINE IWTSIG(INEST) IF I4DI(INEST)=1: (OBS NUDGING)                           PARAM.881
C IWTSIG=1 IF OBS NUDGING IS PERFORMED ON SIGMA SURFACES                         PARAM.882
C IWTSIG=0 IF OBS NUDGING IS PERFORMED ON PRESSURE SURFACES                      PARAM.883
C          EXCEPT SURFACE LAYER OBS STILL APPLIED ON SIGMA                       PARAM.884
C                                                                                PARAM.885
      IWTSIG(1)=0                                                                PARAM.886
C                                                                                PARAM.887
C-----SET IBLNOP(IN,NV) FOR BL NUDGING OPTION                                    PARAM.888
C     IBLNOP(IN)=0 CORRECTION TERM IN THE BL IS CONSTANT IN THE VERTICAL         PARAM.889
C     CORR(K)=XOBBL(KL)-XB(KL)                                                   PARAM.890
C     IBLNOP(IN)=1 CORRECTION TERM IN THE BL VARIES IN THE VERTICAL              PARAM.891
C     CORR(K)=XOBBL(KL)-XB(K)                                                    PARAM.892
C CGM U AND V:                                                                   PARAM.893
      IBLNOP(1,1)=0                                                              PARAM.894
      IBLNOP(1,2)=0                                                              PARAM.895
C CGM TEMP:                                                                      PARAM.896
      IBLNOP(1,3)=0                                                              PARAM.897
C CGM MIXR:                                                                      PARAM.898
      IBLNOP(1,4)=1                                                              PARAM.899
C                                                                                PARAM.900
C                                                                                PARAM.901
C-----IF NUDGING SURFACE WINDS IN THE BOUNDARY LAYER (IWIND(INEST+2)=1),         PARAM.902
C     DO YOU WANT TO USE A SIMILARITY CORRECTION BASED ON ROUGHNESS TO           PARAM.903
C     APPLY 10M SURFACE WIND TO THE SURFACE LAYER (K=KL AT 40M) FOR USE          PARAM.904
C     IN THE FREE CONVECTIVE BOUNDARY LAYER (REGIME=4)? [Y=1,N=0]                PARAM.905
      IWINDS(1,1)=-99                                                            PARAM.906
      IWINDS(1,2)=1                                                              PARAM.907
C                                                                                PARAM.908
C--- INITIALIZE NESTED DOMAINS FDDA SWITCHES                                     PARAM.909
C                                                                                PARAM.910
      DO 8 NN=1,MAXNES                                                           PARAM.911
        IWTSIG(NN)=0                                                             PARAM.912
        FDASTA(NN)=0.                                                            PARAM.913
        FDAEND(NN)=0.                                                            PARAM.914
        I4D(NN,1)=0                                                              PARAM.915
        I4D(NN,2)=0                                                              PARAM.916
        DIFTIM(NN,1)=720.                                                        PARAM.917
        DIFTIM(NN,2)=180.                                                        PARAM.918
        IWIND(NN,1)=1                                                            PARAM.919
        IWIND(NN,2)=1                                                            PARAM.920
        GV(NN,1)=3.E-4                                                           PARAM.921
        GV(NN,2)=3.E-4                                                           PARAM.922
        IWINDS(NN,1)=-99                                                         PARAM.923
        IWINDS(NN,2)=1                                                           PARAM.924
        ITEMP(NN,1)=1                                                            PARAM.925
        ITEMP(NN,2)=0                                                            PARAM.926
        GT(NN,1)=3.E-4                                                           PARAM.927
        GT(NN,2)=3.E-4                                                           PARAM.928
        IMOIS(NN,1)=1                                                            PARAM.929
        IMOIS(NN,2)=1                                                            PARAM.930
        GQ(NN,1)=1.E-5                                                           PARAM.931
        GQ(NN,2)=3.E-4                                                           PARAM.932
        IROT(NN)=0                                                               PARAM.933
        GR(NN)=5.E6                                                              PARAM.934
        INONBL(NN,1)=0                                                           PARAM.935
        INONBL(NN,2)=0                                                           PARAM.936
        INONBL(NN,3)=1                                                           PARAM.937
        INONBL(NN,4)=0                                                           PARAM.938
        I4DI(NN)=0                                                               PARAM.939
        GIP(NN)=0.0                                                              PARAM.940
C                                                                                PARAM.941
        ISTEMP(NN)=1                                                             PARAM.942
        GIV(NN)=3.E-4                                                            PARAM.943
        GIT(NN)=3.E-4                                                            PARAM.944
        ISWIND(NN)=1                                                             PARAM.945
        ISMOIS(NN)=1                                                             PARAM.946
        GIQ(NN)=3.E-4                                                            PARAM.947
        IBLNOP(NN,1)=0                                                           PARAM.948
        IBLNOP(NN,2)=0                                                           PARAM.949
        IBLNOP(NN,3)=0                                                           PARAM.950
        IBLNOP(NN,4)=1                                                           PARAM.951
    8 CONTINUE                                                                   PARAM.952
C                                                                                PARAM.953
      RINBLW=250.                                                                PARAM.954
      NPFG=240                                                                   PARAM.955
      RINXY=150.                                                                 PARAM.956
      RINSIG=0.001                                                               PARAM.957
      TWINDO=40.0                                                                PARAM.958
      NPFI=560                                                                   PARAM.959
      IDYNIN=0                                                                   PARAM.960
      DTRAMP=0.                                                                  PARAM.961
      IFDDA=0                                                                    PARAM.962
      IF(IFDDAG+IFDDAO.GE.1)IFDDA=1                                              PARAM.963
C DEFINE IONF: THE FREQUENCY IN CGM TIMESTEPS TO COMPUTE THE OBS                 PARAM.964
C              NUDGING WEIGHTS "WT" AND "WT2ERR" AND SAVE IN SAVWT.              PARAM.965
C              THESE SAVED WEIGHTS ARE THEN USED TO SAVE CPU TIME.               PARAM.966
      IONF=4                                                                     PARAM.967
C                                                                                PARAM.968
C NOW DEFINE THE FDDA PARAMETERS NOT IN THE NAMELIST...                          PARAM.969
C                                                                                PARAM.970
C COMPUTE VERTICAL INTERVAL FOR NUDGING PRINTS SUCH THAT NO MORE THAN            PARAM.971
C 8 LAYERS ARE PRINTED EVERY NPFG CGM TIMESTEPS                                  PARAM.972
      IF(IFDDA.EQ.1)THEN                                                         PARAM.973
        NPFV=FLOAT(MKX)/8.+0.51                                                  PARAM.974
        PRINT151,NPFV                                                            PARAM.975
      ENDIF                                                                      PARAM.976
151   FORMAT(1X,'IN PARAM: NPFV FOR FDDA PRINT = ',I2)                           07NOV00.170
C                                                                                PARAM.978
C DEFINE THE MINIMUM AND MAXIMUM RINFAC WHICH MULTIPLIES THE                     PARAM.979
C HORIZONTAL RADIUS OF INFLUENCE RINXY TO DETERMINE THE                          PARAM.980
C HORIZONTAL RADIUS OF INFLUENCE AS A LINEAR FUNCTION OF PRESSURE                PARAM.981
C BETWEEN P*+PTOP AND PFREE ABOVE TERRAIN INFLUENCES, 500 MB FOR EXAMPLE         PARAM.982
      RINFMN=1.0                                                                 PARAM.983
      RINFMX=2.0                                                                 PARAM.984
C DEFINE PFREE IN CB: (50 => 500 MB)                                             PARAM.985
      PFREE=50.                                                                  PARAM.986
C DEFINE DPSMX SUCH THAT WHEN OBS NUDGING SURFACE DATA ALONG SIGMA,              PARAM.987
C THE HORIZONTAL WEIGHTING FUNCTION DECREASES TO ZERO AT ANY                     PARAM.988
C DISTANCE FROM THE OBSERVATION WHERE THE SURFACE PRESSURE AT                    PARAM.989
C THE OBS LOCATION AND THAT AT (I,J) DIFFER BY MORE THAN                         PARAM.990
C DPSMX.                                                                         PARAM.991
C DEFINE DPSMX IN CB: (7.5 => 75 MB)                                             PARAM.992
      DPSMX=7.5                                                                  PARAM.993
      DCON=1.0/DPSMX                                                             PARAM.994
C                                                                                PARAM.995
C                                                                                PARAM.996
C.....FOR THE FDDA SWITCHES                                                      PARAM.997
C                                                                                PARAM.998
C     IF(I4D(1,1)+I4D(INEST,1)+I4D(1,2)+I4D(INEST,2)                             PARAM.999
C    1       +I4DI(1)+I4DI(INEST).LT.1) GOTO 35                                  PARAM.1000
C                                                                                PARAM.1001
C ILIF10 IS FORTRAN UNIT NUMBER FOR READING NAMELIST                             PARAM.1002
C                                                                                PARAM.1003
      IF (DM_IONODE()) THEN                                                      PARAM.1004
        OPEN(10, FILE='mmlif',FORM='FORMATTED',STATUS='OLD',ERR=9005)            07NOV00.171
      ENDIF                                                                      PARAM.1006
      ILIF10=10                                                                  PARAM.1007
#ifndef MPP1                                                                     PARAM.1008
      READ (ILIF10,OPARAM)                                                       PARAM.1009
      READ (ILIF10,LPARAM)                                                       PARAM.1010
      READ (ILIF10,NPARAM)                                                       PARAM.1011
      READ (ILIF10,PPARAM)                                                       PARAM.1012
      READ (ILIF10,FPARAM)                                                       PARAM.1013
#else                                                                            PARAM.1014
#  include <mpp_param_30.incl>                                                   PARAM.1015
#endif                                                                           PARAM.1016
C                                                                                07NOV00.172
C-----SPECIFY THE BUCKET MODEL CONSTANTS AS A FUNCTION OF LANDUSE:               07NOV00.173
C     ERATE  : EVAPORATION RATE                                                  07NOV00.174
C     MAMAX  : MAX MOISTURE AVAILABILITY ALLOWED                                 07NOV00.175
C     MAMIN  : MIN MOISTURE AVAILABILITY ALLOWED                                 07NOV00.176
C                                                                                07NOV00.177
C-----READ IN BUCKET MODEL CONSTANTS FROM BUCKET.TBL FILE                        07NOV00.178
C                                                                                07NOV00.179
      IMOIAVI = 0                                                                05DEC01.133
      DO NN = 1,MAXNES                                                           07NOV00.180
         IF(IMOIAV(NN).GE.1) IMOIAVI = 1                                         07NOV00.181
      END DO                                                                     07NOV00.182
      IF (IMOIAVI.EQ.1) THEN                                                     07NOV00.183
        IF (DM_IONODE()) THEN                                                    07NOV00.184
          OPEN(18, FILE='BUCKET.TBL',                                            07NOV00.185
     -         FORM='FORMATTED',STATUS='OLD',ERR=9004)                           07NOV00.186
        ENDIF                                                                    07NOV00.187
        IF (DM_IONODE()) THEN                                                    07NOV00.188
          LUMATCH=0                                                              07NOV00.189
 2010     CONTINUE                                                               07NOV00.190
          READ (18,2000,END=2020)LUTYPE                                          07NOV00.191
          READ (18,*)LUCATS,LUSEAS                                               07NOV00.192
          IF(LUTYPE.EQ.MMINLU)THEN                                               07NOV00.193
            LUN=LUCATS                                                           07NOV00.194
            LUMATCH=1                                                            07NOV00.195
          ENDIF                                                                  07NOV00.196
          DO LC=1,LUCATS                                                         07NOV00.197
            IF(LUTYPE.EQ.MMINLU)THEN                                             07NOV00.198
              READ (18,*)LI,MAMAX(LC),MAMIN(LC),ERATE(LC)                        07NOV00.199
              IF(LC.NE.LI) STOP 'MISSING BUCKET.TBL: UNIT 18'                    07NOV00.200
            ELSE                                                                 07NOV00.201
              READ (18,*)                                                        07NOV00.202
            ENDIF                                                                07NOV00.203
          ENDDO                                                                  07NOV00.204
          GOTO 2010                                                              07NOV00.205
 2020     CONTINUE                                                               07NOV00.206
          IF(LUMATCH.EQ.0)THEN                                                   07NOV00.207
            PRINT *,                                                             07NOV00.208
     -     'LANDUSE IN INPUT FILE DOES NOT MATCH DATA IN BUCKET TBL'             07NOV00.209
            STOP 'INCONSISTENT OR MISSING BUCKET.TBL FILE'                       07NOV00.210
          ENDIF                                                                  07NOV00.211
        ENDIF                                                                    07NOV00.212
        CALL DM_BCAST_STRING    ( LUTYPE , 4 )                                   07NOV00.213
        CALL DM_BCAST_REALS     ( MAMAX , NLU )                                  07NOV00.214
        CALL DM_BCAST_REALS     ( MAMIN , NLU )                                  07NOV00.215
        CALL DM_BCAST_REALS     ( ERATE , NLU )                                  07NOV00.216
        CALL DM_BCAST_INTEGERS  ( LI , 1 )                                       07NOV00.217
        CALL DM_BCAST_INTEGERS  ( LUN , 1 )                                      07NOV00.218
        CALL DM_BCAST_INTEGERS  ( LUSEAS , 1 )                                   07NOV00.219
        CALL DM_BCAST_INTEGERS  ( LUCATS , 1 )                                   07NOV00.220
        CALL DM_BCAST_INTEGERS  ( LUMATCH , 1 )                                  07NOV00.221
      ENDIF                                                                      07NOV00.222
C                                                                                PARAM.1017
C  CHECK IFEED OPTION FOR MPP MM5: IFEED=4 ONLY                                  25JAN00.61
C                                                                                25JAN00.62
#ifdef MPP1                                                                      25JAN00.63
      IF (IFEED.EQ.3.AND.MAXNES.GT.1) THEN                                       25JAN00.64
         PRINT *,'* ADVISORY: IFEED=4 IS RECOMMENDED FOR USE WITH *'             25JAN00.65
         PRINT *,'* MPP VERSION (MORE EFFICIENT)                  *'             25JAN00.66
      ENDIF                                                                      25JAN00.67
      IF (OROSHAW.EQ.1) THEN                                                     23DEC04.143
         OROSHAW=0                                                               23DEC04.144
         PRINT *,'* ADVISORY                                      *'             23DEC04.145
         PRINT *,'* OROSHAW=1 IS NOT AVALIABLE FOR MPP VERSION    *'             23DEC04.146
         PRINT *,'* OROSHAW has been RESET to 0                   *'             23DEC04.147
      ENDIF                                                                      23DEC04.148
#endif                                                                           25JAN00.68
C  CHECK COARSE-MESH SIZE AGAINST NAMELIST                                       PARAM.1018
      IF(NESTIX(1).NE.BHI(16,1).OR.NESTJX(1).NE.BHI(17,1))THEN                   PARAM.1019
        PRINT *,'THE INPUT DATA DOES NOT MATCH THE EXPECTED SIZE'                PARAM.1020
        PRINT *,'SIZE FROM NAMELIST NESTIX NESIJX =',NESTIX(1),NESTJX(1)         PARAM.1021
        IWARN=IWARN+1                                                            PARAM.1022
      ENDIF                                                                      PARAM.1023
C                                                                                PARAM.1024
C                                                                                PARAM.1025
C  SET IBMOIST FOR 1-WAY RUN                                                     PARAM.1026
C                                                                                PARAM.1027
      IBMOIST=0                                                                  PARAM.1028
      IF(INIEXMS.EQ.1)IBMOIST=1                                                  PARAM.1029
      IF(INIICE.EQ.1.AND.IICE.NE.1)THEN                                          PARAM.1030
          PRINT *,'IICE=0 BUT INPUT FIELDS CONTAIN ICE, SET IICE=1'              PARAM.1031
        STOP                                                                     PARAM.1032
      ENDIF                                                                      PARAM.1033
      IF(INIICEG.EQ.1.AND.IICEG.NE.1)THEN                                        PARAM.1034
            PRINT *,'IICEG=0 BUT INPUT FIELDS CONTAIN GRAUPEL, ',                PARAM.1035
     1              'SET IICEG=1'                                                PARAM.1036
        STOP                                                                     PARAM.1037
      ENDIF                                                                      PARAM.1038
      IF(SAVFRQ.LT.1.E-10)SAVFRQ=99999.                                          PARAM.1039
      IF(TAPFRQ.LT.1.E-10)TAPFRQ=99999.                                          PARAM.1040
      IF(PRTFRQ.LT.1.E-10)PRTFRQ=99999.                                          PARAM.1041
      DT=TISTEP+1.E-10                                                           PARAM.1042
C     ALLOW FOR 32-BIT PRECISION                                                 PARAM.1043
      IF(DT.LE.TISTEP)DT=TISTEP+1.E-5                                            PARAM.1044
c...temporary                                                                    PARAM.1045
      BHR(2,12)=TISTEP                                                           PARAM.1046
      XENNES(1)=AMAX1(XENNES(1),TIMAX)                                           PARAM.1047
      XSTNES(1)=0.                                                               19DEC02.461
C...Also store MPHYSTBL if it is selected                                        07NOV00.223
#ifdef MPHYSTBL1                                                                 07NOV00.224
      BHI(29,13)=1                                                               07NOV00.225
      BHIC(29,13)='MPHYSTBL: 1=USE LOOKUP TABLE VERSION OF '                     07NOV00.226
     *    //      'THE EXPLICIT MOISTURE SCHEME            '                     07NOV00.227
#endif                                                                           07NOV00.228
#ifdef MPHYSTBL2                                                                 23DEC04.149
      BHI(29,13)=2                                                               23DEC04.150
      BHIC(29,13)='MPHYSTBL: 2=USE OPTIMIZED VERSION OF '                        23DEC04.151
     *    //      'THE EXPLICIT MOISTURE SCHEME         '                        23DEC04.152
#endif                                                                           23DEC04.153
C-----DO YOU WANT TO GRID NUDGE MOISTURE BASED ON PRECIP? [Y=1,N=0]              PARAM.1048
C     (CURRENTLY NOT TESTED IN BLNUDGD)                                          PARAM.1049
      IMOISP(1)=0                                                                PARAM.1050
      RINP=160.                                                                  PARAM.1051
      IDQ=0                                                                      PARAM.1052
      IDQN=0                                                                     PARAM.1053
      IF(IMOISP(1).EQ.1)IDQ=3                                                    PARAM.1054
      IPSTR(1,1)=0                                                               PARAM.1055
      IF(IROT(1).EQ.1)IWIND(1,1)=1                                               PARAM.1056
      IPSTR(1,2)=0                                                               PARAM.1057
      GP(1,1)=0.0                                                                PARAM.1058
      GP(1,2)=0.0                                                                PARAM.1059
C-----WILL THE GRID NUDGING VARY IN TIME? [Y=1,N=0]                              PARAM.1060
      INDT(1,1)=0                                                                PARAM.1061
      INDT(1,2)=0                                                                PARAM.1062
C-----WILL THE GRID NUDGING VARY IN THE HORIZONTAL? [Y=1,N=0]                    PARAM.1063
C-----SET INDXY=1 IF WEIGHTING ARRAY IS DEFINED OR READ FROM                     PARAM.1064
C     A DATA VOLUME IN SETUPGD                                                   PARAM.1065
C-----SET INDXY=2 IF WEIGHTING ARRAY IS TO BE BASED ON THAT READ FROM TA         PARAM.1066
C     (AS WITH BLWNV FROM RAWINS SURFACE ANALYSES TAPE)                          PARAM.1067
C-----IF IMOISP=1, THE HORIZONTAL WEIGHTING IS COMPUTED INDEPENDENTLY            PARAM.1068
C-----OF THIS SWITCH.                                                            PARAM.1069
      INDXY(1,1)=0                                                               PARAM.1070
      INDXY(1,2)=2                                                               PARAM.1071
C                                                                                PARAM.1072
C-----IF IPSTR( )=1, THEN IWIND( ),ITEMP( ), AMD IMOIS( ) MUST BE SET TO         PARAM.1073
C     1 SINCE THERE WILL BE A NUDGING TERM INVOLVING PSB IN EACH OF THE          PARAM.1074
C     OTHER PROGNOSTIC EQUATIONS (DUE TO THE FLUX FORM OF THE EQUATIONS)         PARAM.1075
      DO 2500 NN=1,MAXNES                                                        PARAM.1076
        IF(IROT(NN).EQ.1)IWIND(NN,1)=1                                           PARAM.1077
        IMOISP(NN)=0                                                             PARAM.1078
        IF(IMOISP(NN).EQ.1)IDQN=3                                                PARAM.1079
        IPSTR(NN,1)=0                                                            PARAM.1080
        IPSTR(NN,2)=0                                                            PARAM.1081
        GP(NN,1)=0.0                                                             PARAM.1082
        GP(NN,2)=0.0                                                             PARAM.1083
        INDT(NN,1)=0                                                             PARAM.1084
        INDT(NN,2)=0                                                             PARAM.1085
        INDXY(NN,1)=0                                                            PARAM.1086
        INDXY(NN,2)=2                                                            PARAM.1087
        INDZ(NN,1)=0                                                             PARAM.1088
        INDZ(NN,2)=0                                                             PARAM.1089
        INT4(NN,1)=1                                                             PARAM.1090
        INT4(NN,2)=1                                                             PARAM.1091
        INOPRO(NN,2)=0                                                           PARAM.1092
C                                                                                PARAM.1093
        DO 2500 N=1,2                                                            PARAM.1094
          IF(I4D(NN,N).EQ.1.AND.IPSTR(NN,N).EQ.1)THEN                            PARAM.1095
            IWIND(NN,N)=1                                                        PARAM.1096
            GV(NN,N)=0.0                                                         PARAM.1097
            ITEMP(NN,N)=1                                                        PARAM.1098
            GT(NN,N)=0.0                                                         PARAM.1099
            IMOIS(NN,N)=1                                                        PARAM.1100
            GQ(NN,N)=0.0                                                         PARAM.1101
            STOP 2500                                                            PARAM.1102
C THIS OPTION HAS NOT BEEN TESTED                                                PARAM.1103
          ENDIF                                                                  PARAM.1104
 2500   CONTINUE                                                                 PARAM.1105
                                                                                 PARAM.1106
C FOR DYNAMIC INITIALIZATION, GRID NUDGING VARIES IN TIME                        PARAM.1107
C THIS TIME FUNCTION IS DEFINED IN SUBROUTINE SETUPGD                            PARAM.1108
      IF(DTRAMP.EQ.0..AND.IDYNIN.EQ.1)THEN                                       PARAM.1109
      PRINT*,' WARNING: DTRAMP = 0, IDYNIN SET TO ZERO'                          PARAM.1110
        IDYNIN=0                                                                 PARAM.1111
      ENDIF                                                                      PARAM.1112
C SET TEMPORAL VARIATION SWITCH FOR ANALYSIS NUDGING TO BE ON                    PARAM.1113
      DO 2501 NNL=1,MAXNES                                                       PARAM.1114
        I4DTOT=I4D(NNL,1)+I4D(NNL,2)+I4DI(NNL)                                   PARAM.1115
        IF(I4DTOT.GE.1)THEN                                                      PARAM.1116
C SET DEFAULT END-OF-DATA FLAGS TO OFF                                           PARAM.1117
C ANALYSIS NUDGING:                                                              PARAM.1118
          IEOD(NNL)=0                                                            PARAM.1119
C OBS NUDGING:                                                                   PARAM.1120
          IEODI(NNL)=0                                                           PARAM.1121
C                                                                                PARAM.1122
          DATEND(NNL)=0.                                                         PARAM.1123
          IF(IDYNIN.EQ.1)THEN                                                    PARAM.1124
            INDT(NNL,1)=1                                                        PARAM.1125
            INDT(NNL,2)=1                                                        PARAM.1126
C TIME IN MINUTES AFTER WHICH DATA ARE ASSUMED TO HAVE ENDED                     PARAM.1127
            IF(DTRAMP.GT.0.)THEN                                                 PARAM.1128
              DATEND(NNL)=FDAEND(NNL)-DTRAMP                                     PARAM.1129
            ELSE                                                                 PARAM.1130
              DATEND(NNL)=FDAEND(NNL)                                            PARAM.1131
            ENDIF                                                                PARAM.1132
      PRINT*,' '                                                                 PARAM.1133
      PRINT*,' *** DYNAMIC-INITIALIZATION OPTION FOR INEST = ',NNL,              PARAM.1134
     1' ***'                                                                     PARAM.1135
      PRINT*,' DYNIN PARAM: INDT(IN,1),INDT(IN,2),IEOD(IN),',                    PARAM.1136
     1'IEODI(IN): ',INDT(NNL,1),INDT(NNL,2),IEOD(NNL),IEODI(NNL)                 PARAM.1137
      PRINT*,' FDAEND(IN),DATEND(IN),DTRAMP: ',FDAEND(NNL),                      PARAM.1138
     1DATEND(NNL),DTRAMP                                                         PARAM.1139
      PRINT*,' '                                                                 PARAM.1140
          ENDIF                                                                  PARAM.1141
        ENDIF                                                                    PARAM.1142
 2501 CONTINUE                                                                   PARAM.1143
C                                                                                PARAM.1144
C-----IF INDXY(1,2) OR INDXY(INEST,2) = 2, DEFINE RINBLW (KM):                   PARAM.1145
C                     (SEE SUBROUTINE BLW FOR DETAILS)                           PARAM.1146
C-----WILL THE GRID NUDGING VARY IN THE VERTICAL? [Y=1,N=0]                      PARAM.1147
      INDZ(1,1)=0                                                                PARAM.1148
      INDZ(1,2)=0                                                                PARAM.1149
C                                                                                PARAM.1150
      DO 300 IN=1,MAXNES                                                         PARAM.1151
        IF(INONBL(IN,1).EQ.0)THEN                                                PARAM.1152
          INONBL(IN,2)=0                                                         PARAM.1153
        ELSE                                                                     PARAM.1154
          INONBL(IN,2)=1                                                         PARAM.1155
        ENDIF                                                                    PARAM.1156
        IF(IBLNOP(IN,1).EQ.0)THEN                                                PARAM.1157
          IBLNOP(IN,2)=0                                                         PARAM.1158
        ELSE                                                                     PARAM.1159
          IBLNOP(IN,2)=1                                                         PARAM.1160
        ENDIF                                                                    PARAM.1161
  300 CONTINUE                                                                   PARAM.1162
C THE ABOVE VALUES ARE ECHOED IN IN4DGD                                          PARAM.1163
C----SET INT4=1 IF GRID NUDGING TO THE GRID LINEAR INTERPOLATION BETWEEN         PARAM.1164
C     TWO OBS TIMES (ANALYSES), AND  SET INT4=0 IF GRID NUDGING TO THE           PARAM.1165
C     TARGET ANALYSES.                                                           PARAM.1166
      INT4(1,1)=1                                                                PARAM.1167
      INT4(1,2)=1                                                                PARAM.1168
C ----------------------------------------------------------------------         PARAM.1169
C                                                                                PARAM.1170
C OPTIONAL: IF INOPRO(IN+2)=1, DEFINE IDDATES AND IDCHK TO OVERRIDE              PARAM.1171
C IQCHK FROM TAPE AND "NOT-PROCESS" THE SURFACE DATA                             PARAM.1172
      INOPRO(1,2)=0                                                              PARAM.1173
C THIS OPTION NOT TESTED YET                                                     PARAM.1174
      DO 1505 NV=1,NVAR                                                          PARAM.1175
        IDHK(NV)=1                                                               PARAM.1176
        TIME(NV)=0.                                                              PARAM.1177
        NTE(NV)=1                                                                PARAM.1178
 1505 CONTINUE                                                                   PARAM.1179
      DO 1510 NOC=1,NCHA                                                         PARAM.1180
        IDDATE(NOC)=-999                                                         PARAM.1181
        DO 1510 NOV=1,NVAR                                                       PARAM.1182
          IDCHK(NOC,NOV)=1                                                       PARAM.1183
 1510   CONTINUE                                                                 PARAM.1184
                                                                                 PARAM.1185
C     PRINT *,IDDATE                                                             PARAM.1186
C                                                                                PARAM.1187
C INPUT MDATES (I8) INTO IDDATE(NOC) (YYMMDDHH) AND IDCHK(NOC,NOV)               PARAM.1188
C FOR EXAMPLE, FOR NUMBER-OF-CHANGE (NOC) 1...                                   PARAM.1189
C     IDDATE(1)=81041203                                                         PARAM.1190
C SET ONLY THE VARIABLE NUMBER YOU WISH TO BE TOSSED AT THIS TIME                PARAM.1191
C     IDCHK(1,NVAR)=0                                                            PARAM.1192
C                                                                                PARAM.1193
C ECHO THE IDDATE CHANGES...                                                     PARAM.1194
      IF(I4D(1,2).NE.1)GOTO 1517                                                 PARAM.1195
      IF(IDDATE(1).NE.-999)THEN                                                  PARAM.1196
C     PRINT *,IDDATE,NCHA,NTIM,NVAR,IDCHK                                        PARAM.1197
C     CALL ABORT                                                                 PARAM.1198
        DO 1515 NOC=1,NCHA                                                       PARAM.1199
          IF(IDDATE(NOC).EQ.-999)GOTO 1517                                       PARAM.1200
          PRINT1516,IDDATE(NOC),(IDCHK(NOC,NN),NN=1,8)                           PARAM.1201
1516  FORMAT(1X,'IDDATE: IDCHK(NV=1,8)  ',I8,2X,8(2X,I1))                        PARAM.1202
 1515   CONTINUE                                                                 PARAM.1203
      ENDIF                                                                      PARAM.1204
 1517 CONTINUE                                                                   PARAM.1205
C***********************************************************************         PARAM.1206
C                                                                                PARAM.1207
      ISPSTR(1)=0                                                                PARAM.1208
C                                                                                PARAM.1209
      TWINDO=TWINDO/60.                                                          PARAM.1210
   35 CONTINUE                                                                   PARAM.1211
      IF(.NOT.IFREST)THEN                                                        PARAM.1212
        KTAUR=0                                                                  PARAM.1213
        IF(IACTIV(1).NE.1)THEN                                                   PARAM.1214
          PRINT *,'IACTIV(1) RESET TO 1'                                         PARAM.1215
          IACTIV(1)=1                                                            PARAM.1216
        ENDIF                                                                    PARAM.1217
#ifdef NESTED                                                                    PARAM.1218
        DO N=2,MAXNES                                                            PARAM.1219
          IF(IACTIV(N).NE.0)THEN                                                 PARAM.1220
            PRINT *,'WARNING: IACTIV FOR NEST NON-ZERO AT START'                 PARAM.1221
            PRINT *,'IACTIV(',N,') RESET TO 0'                                   PARAM.1222
            IACTIV(N)=0                                                          PARAM.1223
          ENDIF                                                                  PARAM.1224
        ENDDO                                                                    PARAM.1225
#endif                                                                           PARAM.1226
      ENDIF                                                                      PARAM.1227
      DO N=1,MAXNES                                                              PARAM.1228
        IF(IMOVCO(N).EQ.0)THEN                                                   PARAM.1229
          IMOVCO(N)=1                                                            PARAM.1230
          PRINT *,'WARNING, IMOVCO(',N,') RESET TO 1'                            PARAM.1231
        ENDIF                                                                    PARAM.1232
      ENDDO                                                                      PARAM.1233
C                                                                                PARAM.1234
C-----SET SWITCHES ACCORDING TO IMPHYS CHOSEN                                    PARAM.1235
C                                                                                PARAM.1236
      IEXICE=1                                                                   PARAM.1237
      DO N=1,MAXNES                                                              PARAM.1238
        IF(IMPHYS(N).EQ.1)THEN                                                   PARAM.1239
          IDRY(N)=1                                                              PARAM.1240
        ELSE                                                                     PARAM.1241
          IDRY(N)=0                                                              PARAM.1242
        ENDIF                                                                    PARAM.1243
        IF(IMPHYS(N).LE.2)THEN                                                   PARAM.1244
          IMOIST(N)=1                                                            PARAM.1245
        ELSE                                                                     PARAM.1246
          IMOIST(N)=2                                                            PARAM.1247
        ENDIF                                                                    PARAM.1248
        IF(IMPHYS(N).EQ.3)IEXICE=0                                               PARAM.1249
      ENDDO                                                                      PARAM.1250
      DO N=1,MAXNES                                                              PARAM.1251
        IF(IMPHYS(N).NE.3.AND.IEXICE.EQ.0)THEN                                   PARAM.1252
          IWARN=IWARN+1                                                          PARAM.1253
          PRINT *,'ERROR: IMPHYS=3 OPTION REQUIRED FOR ALL DOMAINS'              PARAM.1254
        ENDIF                                                                    PARAM.1255
      ENDDO                                                                      PARAM.1256
C                                                                                PARAM.1257
C                                                                                PARAM.1258
C----------------------------------------------------------------------          PARAM.1259
C     SAVE COMPILATION SWITCHES                                                  PARAM.1260
C                                                                                PARAM.1261
#ifdef FDDAOBS1                                                                  PARAM.1262
      I_FDDAOBS1=1                                                               PARAM.1263
#else                                                                            PARAM.1264
      I_FDDAOBS1=0                                                               PARAM.1265
#endif                                                                           PARAM.1266
#ifdef FDDAGD1                                                                   PARAM.1267
      I_FDDAGD1=1                                                                PARAM.1268
#else                                                                            PARAM.1269
      I_FDDAGD1=0                                                                PARAM.1270
#endif                                                                           PARAM.1271
C                                                                                PARAM.1272
#ifdef NESTED                                                                    PARAM.1273
      I_NESTED=1                                                                 PARAM.1274
#else                                                                            PARAM.1275
      I_NESTED=0                                                                 PARAM.1276
#endif                                                                           PARAM.1277
C                                                                                PARAM.1278
C-----RADIATION                                                                  PARAM.1279
C                                                                                PARAM.1280
      I_FRAD(0)=1                                                                PARAM.1281
#ifdef FRAD1                                                                     PARAM.1282
      I_FRAD(1)=1                                                                PARAM.1283
#else                                                                            PARAM.1284
      I_FRAD(1)=0                                                                PARAM.1285
#endif                                                                           PARAM.1286
#ifdef FRAD2                                                                     PARAM.1287
      I_FRAD(2)=1                                                                PARAM.1288
#else                                                                            PARAM.1289
      I_FRAD(2)=0                                                                PARAM.1290
#endif                                                                           PARAM.1291
#ifdef FRAD3                                                                     PARAM.1292
      I_FRAD(3)=1                                                                PARAM.1293
#else                                                                            PARAM.1294
      I_FRAD(3)=0                                                                PARAM.1295
#endif                                                                           PARAM.1296
#ifdef FRAD4                                                                     25JAN00.69
      I_FRAD(4)=1                                                                25JAN00.70
#else                                                                            25JAN00.71
      I_FRAD(4)=0                                                                25JAN00.72
#endif                                                                           25JAN00.73
C                                                                                PARAM.1297
C-----CUMULUS                                                                    PARAM.1298
C                                                                                PARAM.1299
      I_ICUPA(1)=1                                                               PARAM.1300
#ifdef ICUPA2                                                                    PARAM.1301
      I_ICUPA(2)=1                                                               PARAM.1302
#else                                                                            PARAM.1303
      I_ICUPA(2)=0                                                               PARAM.1304
#endif                                                                           PARAM.1305
#ifdef ICUPA3                                                                    PARAM.1306
      I_ICUPA(3)=1                                                               PARAM.1307
#else                                                                            PARAM.1308
      I_ICUPA(3)=0                                                               PARAM.1309
#endif                                                                           PARAM.1310
#ifdef ICUPA4                                                                    PARAM.1311
      I_ICUPA(4)=1                                                               PARAM.1312
#else                                                                            PARAM.1313
      I_ICUPA(4)=0                                                               PARAM.1314
#endif                                                                           PARAM.1315
#ifdef ICUPA5                                                                    PARAM.1316
      I_ICUPA(5)=1                                                               PARAM.1317
#else                                                                            PARAM.1318
      I_ICUPA(5)=0                                                               PARAM.1319
#endif                                                                           PARAM.1320
#ifdef ICUPA6                                                                    PARAM.1321
      I_ICUPA(6)=1                                                               PARAM.1322
#else                                                                            PARAM.1323
      I_ICUPA(6)=0                                                               PARAM.1324
#endif                                                                           PARAM.1325
#ifdef ICUPA7                                                                    PARAM.1326
      I_ICUPA(7)=1                                                               PARAM.1327
#else                                                                            PARAM.1328
      I_ICUPA(7)=0                                                               PARAM.1329
#endif                                                                           PARAM.1330
#ifdef ICUPA8                                                                    05DEC01.134
      I_ICUPA(8)=1                                                               05DEC01.135
#else                                                                            05DEC01.136
      I_ICUPA(8)=0                                                               05DEC01.137
#endif                                                                           05DEC01.138
C                                                                                PARAM.1331
C-----SHALLOW CONVECTION                                                         PARAM.1332
C                                                                                PARAM.1333
#ifdef ISHALLO1                                                                  PARAM.1334
      I_ISHALLO1=1                                                               PARAM.1335
#else                                                                            PARAM.1336
      I_ISHALLO1=0                                                               PARAM.1337
#endif                                                                           PARAM.1338
C                                                                                PARAM.1339
C-----MICROPHYSICS                                                               PARAM.1340
C                                                                                PARAM.1341
#ifdef MPHYSTBL0                                                                 23DEC04.154
      I_MPHYSTBL(0)=1                                                            23DEC04.155
#else                                                                            23DEC04.156
      I_MPHYSTBL(0)=0                                                            23DEC04.157
#endif                                                                           23DEC04.158
#ifdef MPHYSTBL1                                                                 23DEC04.159
      I_MPHYSTBL(1)=1                                                            23DEC04.160
#else                                                                            23DEC04.161
      I_MPHYSTBL(1)=0                                                            23DEC04.162
#endif                                                                           23DEC04.163
#ifdef MPHYSTBL2                                                                 23DEC04.164
      I_MPHYSTBL(2)=1                                                            23DEC04.165
#else                                                                            23DEC04.166
      I_MPHYSTBL(2)=0                                                            23DEC04.167
#endif                                                                           23DEC04.168
      I_IMPHYS(0)=0                                                              PARAM.1347
      I_IMPHYS(1)=1                                                              PARAM.1348
#ifdef IMPHYS2                                                                   PARAM.1349
      I_IMPHYS(2)=1                                                              PARAM.1350
#else                                                                            PARAM.1351
      I_IMPHYS(2)=0                                                              PARAM.1352
#endif                                                                           PARAM.1353
#if defined(IMPHYS3) || defined(IMPHYS4)
      I_IMPHYS(3)=1                                                              PARAM.1355
      I_IMPHYS(4)=1                                                              PARAM.1356
#else                                                                            PARAM.1357
      I_IMPHYS(3)=0                                                              PARAM.1358
      I_IMPHYS(4)=0                                                              PARAM.1359
#endif                                                                           PARAM.1360
#ifdef IMPHYS5                                                                   PARAM.1361
      I_IMPHYS(5)=1                                                              PARAM.1362
#else                                                                            PARAM.1363
      I_IMPHYS(5)=0                                                              PARAM.1364
#endif                                                                           PARAM.1365
#ifdef IMPHYS6                                                                   PARAM.1366
      I_IMPHYS(6)=1                                                              PARAM.1367
#else                                                                            PARAM.1368
      I_IMPHYS(6)=0                                                              PARAM.1369
#endif                                                                           PARAM.1370
#ifdef IMPHYS7                                                                   PARAM.1371
      I_IMPHYS(7)=1                                                              PARAM.1372
#else                                                                            PARAM.1373
      I_IMPHYS(7)=0                                                              PARAM.1374
#endif                                                                           PARAM.1375
#ifdef IMPHYS8                                                                   PARAM.1376
      I_IMPHYS(8)=1                                                              PARAM.1377
#else                                                                            PARAM.1378
      I_IMPHYS(8)=0                                                              PARAM.1379
#endif                                                                           PARAM.1380
C                                                                                PARAM.1381
C-----BOUNDARY LAYER                                                             PARAM.1382
C                                                                                PARAM.1383
#ifdef IBLT0                                                                     PARAM.1384
      I_IBLT(0)=1                                                                PARAM.1385
#else                                                                            PARAM.1386
      I_IBLT(0)=0                                                                PARAM.1387
#endif                                                                           PARAM.1388
#ifdef IBLT1                                                                     PARAM.1389
      I_IBLT(1)=1                                                                PARAM.1390
#else                                                                            PARAM.1391
      I_IBLT(1)=0                                                                PARAM.1392
#endif                                                                           PARAM.1393
#ifdef IBLT2                                                                     PARAM.1394
      I_IBLT(2)=1                                                                PARAM.1395
#else                                                                            PARAM.1396
      I_IBLT(2)=0                                                                PARAM.1397
#endif                                                                           PARAM.1398
#ifdef IBLT3                                                                     PARAM.1399
      I_IBLT(3)=1                                                                PARAM.1400
#else                                                                            PARAM.1401
      I_IBLT(3)=0                                                                PARAM.1402
#endif                                                                           PARAM.1403
#ifdef IBLT4                                                                     PARAM.1404
      I_IBLT(4)=1                                                                PARAM.1405
#else                                                                            PARAM.1406
      I_IBLT(4)=0                                                                PARAM.1407
#endif                                                                           PARAM.1408
#ifdef IBLT5                                                                     PARAM.1409
      I_IBLT(5)=1                                                                PARAM.1410
#else                                                                            PARAM.1411
      I_IBLT(5)=0                                                                PARAM.1412
#endif                                                                           PARAM.1413
#ifdef IBLT6                                                                     PARAM.1414
      I_IBLT(6)=1                                                                PARAM.1415
#else                                                                            PARAM.1416
      I_IBLT(6)=0                                                                PARAM.1417
#endif                                                                           PARAM.1418
#ifdef IBLT7                                                                     07NOV00.229
      I_IBLT(7)=1                                                                07NOV00.230
#else                                                                            07NOV00.231
      I_IBLT(7)=0                                                                07NOV00.232
#endif                                                                           07NOV00.233
C                                                                                PARAM.1419
C-----SOIL LAYER                                                                 PARAM.1420
C                                                                                PARAM.1421
      I_ISOIL(0)=1                                                               PARAM.1422
#ifdef SOIL1                                                                     PARAM.1423
      I_ISOIL(1)=1                                                               PARAM.1424
#else                                                                            PARAM.1425
      I_ISOIL(1)=0                                                               PARAM.1426
#endif                                                                           PARAM.1427
#ifdef SOIL2                                                                     PARAM.1428
      I_ISOIL(2)=1                                                               PARAM.1429
#else                                                                            PARAM.1430
      I_ISOIL(2)=0                                                               PARAM.1431
#endif                                                                           PARAM.1432
#ifdef SOIL3                                                                     07NOV00.234
      I_ISOIL(3)=1                                                               07NOV00.235
#else                                                                            07NOV00.236
      I_ISOIL(3)=0                                                               07NOV00.237
#endif                                                                           07NOV00.238
C                                                                                PARAM.1433
C-----CHECK CHOSEN PHYSICS OPTIONS AGAINST COMPILED OPTIONS                      PARAM.1434
C                                                                                PARAM.1435
      IF(I_FRAD(IFRAD).NE.1)THEN                                                 PARAM.1436
        PRINT *,'ERROR:IFRAD=',IFRAD,' OPTION NOT COMPILED'                      PARAM.1437
        IWARN=IWARN+1                                                            PARAM.1438
      ENDIF                                                                      PARAM.1439
      IF(I_ISOIL(ISOIL).NE.1)THEN                                                PARAM.1440
        PRINT *,'ERROR:ISOIL=',ISOIL,' OPTION NOT COMPILED'                      PARAM.1441
        IWARN=IWARN+1                                                            PARAM.1442
      ENDIF                                                                      PARAM.1443
      DO N=1,MAXNES                                                              PARAM.1444
        IF(I_IBLT(IBLTYP(N)).NE.1)THEN                                           PARAM.1445
          PRINT *,'ERROR:IBLTYP=',IBLTYP(N),' OPTION NOT COMPILED'               PARAM.1446
          IWARN=IWARN+1                                                          PARAM.1447
        ENDIF                                                                    PARAM.1448
        IF(I_ICUPA(ICUPA(N)).NE.1)THEN                                           PARAM.1449
          PRINT *,'ERROR:ICUPA=',ICUPA(N),' OPTION NOT COMPILED'                 PARAM.1450
          IWARN=IWARN+1                                                          PARAM.1451
        ENDIF                                                                    PARAM.1452
        IF(I_ISHALLO1.NE.1.AND.ISHALLO(N).EQ.1)THEN                              PARAM.1453
          PRINT *,'ERROR:ISHALLO=',ISHALLO(N),' OPTION NOT COMPILED'             PARAM.1454
          IWARN=IWARN+1                                                          PARAM.1455
        ENDIF                                                                    PARAM.1456
        IF(I_IMPHYS(IMPHYS(N)).NE.1)THEN                                         PARAM.1457
          PRINT *,'ERROR:IMPHYS=',IMPHYS(N),' OPTION NOT COMPILED'               PARAM.1458
          IWARN=IWARN+1                                                          PARAM.1459
        ENDIF                                                                    PARAM.1460
      ENDDO                                                                      PARAM.1461
C-----------------------------------------------------------------------         PARAM.1462
C                                                                                PARAM.1463
C-----RESET THE OPTIONS:                                                         PARAM.1464
C.....FOR LARGE DOMAIN:                                                          PARAM.1465
      IF(IFDRY.EQ.1)THEN                                                         PARAM.1466
        IF(IMVDIF.EQ.1)THEN                                                      PARAM.1467
          PRINT *,'ERROR: IFDRY=1 (FAKE DRY RUN) REQUIRES IMVDIF=0'              PARAM.1468
          IWARN=IWARN+1                                                          PARAM.1469
        ENDIF                                                                    PARAM.1470
        IF(ICUPA(1).NE.1)THEN                                                    PARAM.1471
          PRINT *,'ERROR: IFDRY=1 (FAKE DRY RUN) REQUIRES ICUPA=1'               PARAM.1472
          IWARN=IWARN+1                                                          PARAM.1473
        ENDIF                                                                    PARAM.1474
        PRINT *,'  FAKE DRY RUN, NO LATENT HEAT RELEASE CONSIDERED'              PARAM.1475
      ENDIF                                                                      PARAM.1476
      IF(ICUPA(1).EQ.0)THEN                                                      PARAM.1477
        PRINT *,'ERROR: ICUPA=0: NON-DEFINED OPTION'                             PARAM.1478
        IWARN=IWARN+1                                                            PARAM.1479
      ENDIF                                                                      PARAM.1480
      IF(ICUPA(1).GT.8)THEN                                                      05DEC01.139
        IWARN=IWARN+1                                                            PARAM.1482
        PRINT *,'ERROR: YOU HAVE REQUESTED A CUMULUS PARAMETERIZATION ',         PARAM.1483
     & 'ICUPA(1) = ',ICUPA(1),' THAT IS NOT IN THE CODE.'                        PARAM.1484
      ENDIF                                                                      PARAM.1485
      IF(INHYD.EQ.1.AND.IBOUDY(1).NE.3)THEN                                      PARAM.1486
        PRINT *,'ERROR: INHYD=1 REQUIRES IBOUDY(1)=3  '                          PARAM.1487
        IWARN=IWARN+1                                                            PARAM.1488
      ENDIF                                                                      PARAM.1489
      IF(INHYD.EQ.0.AND.IBOUDY(1).EQ.3)THEN                                      PARAM.1490
        PRINT *,'ERROR: INHYD=0 USUALLY REQUIRES IBOUDY(1)=1 OR 4'               PARAM.1491
C  IF YOU WANT INFLOW/OUTFLOW CONDITIONS COMMENT OUT THE NEXT LINE               PARAM.1492
        IWARN=IWARN+1                                                            PARAM.1493
      ENDIF                                                                      PARAM.1494
C      IF(IICE.EQ.1.AND.IEXICE.EQ.1)THEN                                         PARAM.1495
C        PRINT *,'ERROR: IICE=1 (MIXED PHASE) REQUIRES IEXICE=0'                 PARAM.1496
C        PRINT *,'   OR IEXICE=1 (NO MIXED PHASE) REQUIRES IICE=0'               PARAM.1497
C          IWARN=IWARN+1                                                         PARAM.1498
C      ENDIF                                                                     PARAM.1499
      IF(IEXMS.EQ.0.AND.IMOIST(1).EQ.2)THEN                                      PARAM.1500
        PRINT *,'ERROR: IMOIST=2 REQUIRES IEXMS=1'                               PARAM.1501
        IWARN=IWARN+1                                                            PARAM.1502
      ENDIF                                                                      PARAM.1503
      IF(IMOIST(1).NE.2.AND.IMVDIF.EQ.1)THEN                                     PARAM.1504
        PRINT *,'ERROR: IMVDIF=1 REQUIRES IMPHYS>2'                              PARAM.1505
        IWARN=IWARN+1                                                            PARAM.1506
      ENDIF                                                                      PARAM.1507
      IF(INHYD.NE.1.AND.ITPDIF.EQ.1)THEN                                         PARAM.1508
        PRINT *,'ERROR: ITPDIF=1 REQUIRES INHYD=1'                               PARAM.1509
        IWARN=IWARN+1                                                            PARAM.1510
      ENDIF                                                                      PARAM.1511
      IF(ITPDIF.EQ.2 .AND. (TDKORR.LT.1.OR.TDKORR.GT.2))THEN                     23DEC04.169
        PRINT *,'ERROR: ITPDIF=2 REQUIRES TDKORR=1 OR 2'                         23DEC04.170
        IWARN=IWARN+1                                                            23DEC04.171
      ENDIF                                                                      23DEC04.172
      IF(IBLTYP(1).NE.2.AND.IBLTYP(1).NE.5.AND.IBLTYP(1).NE.7                    07NOV00.239
     >                                    .AND.IMVDIF.EQ.1)THEN                  07NOV00.240
        PRINT *,'ERROR: IMVDIF=1 REQUIRES IBLTYP=2, 5 OR 7'                      07NOV00.241
        IWARN=IWARN+1                                                            PARAM.1514
      ENDIF                                                                      PARAM.1515
      IF(INHYD.NE.1.AND.ICOR3D.EQ.1)THEN                                         PARAM.1516
        PRINT *,'ERROR: ICOR3D=1 REQUIRES INHYD=1  '                             PARAM.1517
        IWARN=IWARN+1                                                            PARAM.1518
      ENDIF                                                                      PARAM.1519
      IF(INHYD.NE.1.AND.IFUPR.EQ.1)THEN                                          PARAM.1520
        PRINT *,'ERROR: IFUPR=1 REQUIRES INHYD=1  '                              PARAM.1521
        IWARN=IWARN+1                                                            PARAM.1522
      ENDIF                                                                      PARAM.1523
      IF(ISLDIM.NE.1.AND.ISOIL.EQ.1)THEN                                         PARAM.1524
        PRINT *,'ERROR: ISOIL=1 REQUIRES ISLDIM=1 '                              PARAM.1525
        IWARN=IWARN+1                                                            PARAM.1526
      ENDIF                                                                      PARAM.1527
      IF(IBLTYP(1).NE.2.AND.IBLTYP(1).NE.5.AND.IBLTYP(1).NE.4.AND.               PARAM.1528
     +  IBLTYP(1).NE.6.AND.ISOIL.EQ.1)THEN                                       PARAM.1529
        PRINT *,'ERROR: ISOIL=1 REQUIRES IBLTYP=2,4,5 OR 6'                      PARAM.1530
        IWARN=IWARN+1                                                            PARAM.1531
      ENDIF                                                                      PARAM.1532
      IF((IBLTYP(1).EQ.5.OR.IBLTYP(1).EQ.4).AND.ISOIL.EQ.0)THEN                  PARAM.1533
        PRINT *,'ERROR: IBLTYP=4 OR 5 REQUIRES ISOIL>0'                          PARAM.1534
        IWARN=IWARN+1                                                            PARAM.1535
      ENDIF                                                                      PARAM.1536
      IF(IBLTYP(1).NE.5.AND.IBLTYP(1).NE.4.AND.ISOIL.EQ.2)THEN                   05DEC01.140
        PRINT *,'ERROR: ISOIL=2 REQUIRES IBLTYP=4 OR 5'                          05DEC01.141
        IWARN=IWARN+1                                                            PARAM.1539
      ENDIF                                                                      PARAM.1540
      IF(IBLTYP(1).EQ.7.AND.ISOIL.NE.3)THEN                                      07NOV00.242
        PRINT *,'ERROR: IBLTYP=7 REQUIRES ISOIL=3'                               07NOV00.243
        IWARN=IWARN+1                                                            07NOV00.244
      ENDIF                                                                      07NOV00.245
      IF(IBLTYP(1).NE.7.AND.ISOIL.EQ.3)THEN                                      07NOV00.246
        PRINT *,'ERROR: ISOIL=3 REQUIRES IBLTYP=7'                               07NOV00.247
        IWARN=IWARN+1                                                            07NOV00.248
      ENDIF                                                                      07NOV00.249
      IF(IFRAD.EQ.2.AND.(IMOIST(1).NE.2.OR.IRDDIM.NE.1))THEN                     PARAM.1541
        PRINT *,'ERROR: IFRAD=2 REQUIRES IRDDIM=1 AND IMPHYS>3'                  PARAM.1542
        IWARN=IWARN+1                                                            PARAM.1543
      ENDIF                                                                      PARAM.1544
      IF(IFRAD.EQ.4.AND.(IMOIST(1).NE.2.OR.IRDDIM.NE.1))THEN                     25JAN00.74
        PRINT *,'ERROR: IFRAD=4 REQUIRES IRDDIM=1 AND IMPHYS>3'                  25JAN00.75
        IWARN=IWARN+1                                                            25JAN00.76
      ENDIF                                                                      25JAN00.77
      IF(IFRAD.EQ.3.AND.(IMOIST(1).NE.2.AND.ICLOUD(1).EQ.1))THEN                 05DEC01.142
        PRINT *,'ERROR: IFRAD=3 AND ICLOUD=1 REQUIRE IMPHYS>3'                   05DEC01.143
        IWARN=IWARN+1                                                            05DEC01.144
      ENDIF                                                                      05DEC01.145
      IF(IFRAD.NE.3.AND.ICLOUD(1).EQ.2)THEN                                      05DEC01.146
        PRINT *,'ERROR: ICLOUD=2 ONLY WORKS WITH IFRAD = 3'                      05DEC01.147
        IWARN=IWARN+1                                                            05DEC01.148
      ENDIF                                                                      05DEC01.149
      IF(IARASC.EQ.0.AND.ICUPA(1).EQ.4)THEN                                      PARAM.1545
        PRINT *,'ERROR: ICUPA=4: REQUIRES IARASC=1'                              PARAM.1546
        IWARN=IWARN+1                                                            PARAM.1547
      ENDIF                                                                      PARAM.1548
      IF(IBLTYP(1).NE.2.AND.(IBLTYP(1).LT.4.OR.IBLTYP(1).GT.7).AND.              05DEC01.150
     +  I4DTOT.GT.0)THEN                                                         05DEC01.151
        IWARN=IWARN+1                                                            PARAM.1551
        PRINT *,'ERROR: FDDA REQUIRES IBLTYP=2, 4, 5, 6, OR 7'                   05DEC01.152
      ENDIF                                                                      PARAM.1553
      IF(ISOIL.NE.1.AND.IEXSI.GE.1)THEN                                          19DEC02.462
        PRINT *,'ERROR: IEXSI.GE.1 REQUIRES ISOIL=1'                             19DEC02.463
        IWARN=IWARN+1                                                            19DEC02.464
      ENDIF                                                                      19DEC02.465
      IF((IBLTYP(1).NE.4.AND.IBLTYP(1).NE.5).AND.IEXSI.GE.1)THEN                 23DEC04.173
        PRINT *,'ERROR: IEXSI.GE.1 REQUIRES IBLTYP=4 OR 5'                       23DEC04.174
        IWARN=IWARN+1                                                            19DEC02.468
      ENDIF                                                                      19DEC02.469
      IF(IFSNOW(1).EQ.2.AND.IMPHYS(1).NE.4.AND.IMPHYS(1).NE.5                    05DEC01.153
     -       .AND.IMPHYS(1).NE.7)THEN                                            05DEC01.154
        PRINT *,'ERROR: IFSNOW=2 REQUIRES IMPHYS=4,5 OR 7'                       05DEC01.155
        IWARN=IWARN+1                                                            05DEC01.156
      ENDIF                                                                      05DEC01.157
      IF(IFSNOW(1).EQ.2.AND.ISOIL.GE.2)THEN                                      23DEC04.175
        PRINT *,'ERROR: IFSNOW=2 CANNOT BE USED WHEN ISOIL=2 or ISOIL=3'         23DEC04.176
        PRINT *,'       CHANGE THE VALUE OF EITHER IFSNOW or ISOIL'              19DEC02.472
        IWARN=IWARN+1                                                            19DEC02.473
      ENDIF                                                                      19DEC02.474
      
CB
      DO I = 1,ISLYR-1
        IF ( ISTLYR(I) .EQ. 0 ) ISTLYR(I+1) = 0
        IF ( ISMLYR(I) .EQ. 0 ) ISMLYR(I+1) = 0
        IF ( ISTLYR(I+1) .LE. ISTLYR(I) ) ISTLYR(I+1) = 0
        IF ( ISMLYR(I+1) .LE. ISMLYR(I) ) ISMLYR(I+1) = 0
      ENDDO
#if defined (SOIL3)
        PRINT *, ' '
        PRINT *,'ISOIL=3: LEVELS ',ISMLYR
        PRINT *,'         ARE USED AS ISMLYR INPUT'
        PRINT *, ' '
      IF( ISMLYR(1).EQ.0 .OR. ISMLYR(2).EQ.0 )THEN 
        PRINT *,'ERROR: ISOIL=3 REQUIRES THAT AT LEAST '
        PRINT *,'       ISMLYR(1) and ISMLYR(2), BE SET TO '               
        PRINT *,'       VALID LEVELS AVAILABLE IN THE MMINPUT FILE'
        PRINT *, ' '
        IWARN=IWARN+1                                                 
      ENDIF                                                          
#elif defined (SOIL2)
        PRINT *, ' '
        PRINT *,'ISOIL=2: LEVELS ',ISTLYR
        PRINT *,'         ARE USED AS ISTLYR INPUT'
        PRINT *,'         LEVELS ',ISMLYR
        PRINT *,'         ARE USED AS ISMLYR INPUT'
        PRINT *, ' '
      IF( ISTLYR(1).EQ.0 .OR. ISTLYR(2).EQ.0 )THEN 
        PRINT *,'ERROR: ISOIL=2 REQUIRES THAT AT LEAST '
        PRINT *,'       ISTLYR(1) and ISTLYR(2), BE SET TO '               
        PRINT *,'       VALID LEVELS AVAILABLE IN THE MMINPUT FILE'
        PRINT *, ' '
        IWARN=IWARN+1                                                 
      ENDIF                                                          
      IF( ISMLYR(1).EQ.0 .OR. ISMLYR(2).EQ.0 )THEN 
        PRINT *,'ERROR: ISOIL=2 REQUIRES THAT AT LEAST '
        PRINT *,'       ISMLYR(1) and ISMLYR(2), BE SET TO '               
        PRINT *,'       VALID LEVELS AVAILABLE IN THE MMINPUT FILE'
        PRINT *, ' '
        IWARN=IWARN+1                                                 
      ENDIF                                                          
#endif
CB
C SOME MODIFICATION TO THE FDDA CODE IS NEEDED FOR BLKPBL                        PARAM.1554
      IF(IBLTYP(1).EQ.0)ISFFLX(1)=0                                              PARAM.1555
      IF(IBLTYP(1).EQ.0)ITGFLG(1)=3                                              PARAM.1556
      IF(ISFFLX(1).NE.1)ITGFLG(1)=3                                              PARAM.1557
      IF(ISFFLX(1).NE.1.OR.ITGFLG(1).NE.1)ICLOUD(1)=0                            PARAM.1558
C THE FOLLOWING LINE IS DELETED IN RELEASE-2-4                                   PARAM.1559
C     IF(ISFFLX(1).NE.1.OR.ITGFLG(1).NE.1)ISFPAR(1)=0                            PARAM.1560
      IF(ISFPAR(1).EQ.0)IFSNOW(1)=0                                              PARAM.1561
      IF(IBLTYP(1).EQ.0)IVMIXM(1)=0                                              PARAM.1562
      IF(IDRY(1).EQ.1)ICLOUD(1)=0                                                PARAM.1563
      IF(IDRY(1).EQ.1)IMOIST(1)=1                                                PARAM.1564
      IF(IDRY(1).EQ.0.AND.IMOIST(1).EQ.0)ICLOUD(1)=0                             PARAM.1565
      IF(IDRY(1).EQ.1.OR.IMOIST(1).EQ.0)XMOIST(1)=0.                             PARAM.1566
      IF(IDRY(1).EQ.1.OR.IMOIST(1).NE.2)HYDPRE(1)=0.                             PARAM.1567
      IF(IBLTYP(1).EQ.0)ISHALLO(1)=0                                             PARAM.1568
      IF(IMOIST(1).EQ.1)IFOGMD(1)=0                                              PARAM.1569
      IF(IBLTYP(1).NE.6)IFOGMD(1)=0                                              PARAM.1570
      IFOGSW(1)=IFOGMD(1)                                                        PARAM.1571
C                                                                                PARAM.1572
C.....FOR NESTED DOMAINS                                                         PARAM.1573
#ifdef NESTED                                                                    PARAM.1574
      IF(MAXNES.GT.1)THEN                                                        PARAM.1575
        DO 30 N=2,MAXNES                                                         PARAM.1576
          IF(IFDRY.EQ.1)THEN                                                     PARAM.1577
            IF(IMVDIF.EQ.1)THEN                                                  PARAM.1578
              PRINT *,'ERROR: IFDRY=1 (FAKE DRY RUN) REQUIRES IMVDIF=0'          PARAM.1579
              IWARN=IWARN+1                                                      PARAM.1580
            ENDIF                                                                PARAM.1581
            IF(ICUPA(N).NE.1)THEN                                                PARAM.1582
              PRINT *,'ERROR: IFDRY=1 (FAKE DRY RUN) REQUIRES ICUPA=1'           PARAM.1583
              IWARN=IWARN+1                                                      PARAM.1584
            ENDIF                                                                PARAM.1585
            PRINT *,'  FAKE DRY RUN, NO LATENT HEAT RELEASE CONSIDERED'          PARAM.1586
          ENDIF                                                                  PARAM.1587
          IF(ICUPA(N).EQ.0)THEN                                                  PARAM.1588
            PRINT *,'ERROR: ICUPA=0: NON-DEFINED OPTION'                         PARAM.1589
            IWARN=IWARN+1                                                        PARAM.1590
          ENDIF                                                                  PARAM.1591
          IF(ICUPA(N).GT.8)THEN                                                  05DEC01.158
            IWARN=IWARN+1                                                        PARAM.1593
            PRINT *,'ERROR: YOU HAVE REQUESTED A CUMULUS ',                      PARAM.1594
     & 'PARAMETERIZATION ',                                                      PARAM.1595
     & 'ICUPA = ',ICUPA(N),' THAT IS NOT IN THE CODE.'                           PARAM.1596
          ENDIF                                                                  PARAM.1597
          IF(IBOUDY(N).NE.2)THEN                                                 PARAM.1598
            PRINT *,'ERROR: NEST REQUIRES IBOUDY(N)=2  '                         PARAM.1599
            IWARN=IWARN+1                                                        PARAM.1600
          ENDIF                                                                  PARAM.1601
          IF(IFEED.EQ.1.AND.IOVERW(N).EQ.2)THEN                                  PARAM.1602
            PRINT *,'IOVERW=2 REQUIRES IFEED >= 2'                               PARAM.1603
            IWARN=IWARN+1                                                        PARAM.1604
          ENDIF                                                                  PARAM.1605
          IF(IEXMS.EQ.0.AND.IMOIST(N).EQ.2)THEN                                  PARAM.1606
            PRINT *,'ERROR: IMOIST=2 REQUIRES IEXMS=1'                           PARAM.1607
            IWARN=IWARN+1                                                        PARAM.1608
          ENDIF                                                                  PARAM.1609
          IF(IMOIST(N).NE.2.AND.IMVDIF.EQ.1)THEN                                 PARAM.1610
            PRINT *,'ERROR: IMVDIF=1 REQUIRES IMPHYS>2'                          PARAM.1611
            IWARN=IWARN+1                                                        PARAM.1612
          ENDIF                                                                  PARAM.1613
          IF(IBLTYP(N).NE.2.AND.IBLTYP(N).NE.5.AND.IBLTYP(1).NE.7                07NOV00.252
     >                                        .AND.IMVDIF.EQ.1)THEN              07NOV00.253
            PRINT *,'ERROR: IMVDIF=1 REQUIRES IBLTYP=2, 5, OR 7'                 07NOV00.254
            IWARN=IWARN+1                                                        PARAM.1616
          ENDIF                                                                  PARAM.1617
          IF(IBLTYP(N).NE.2.AND.IBLTYP(N).NE.5.AND.IBLTYP(N).NE.4.AND.           PARAM.1618
     +      IBLTYP(N).NE.6.AND.ISOIL.EQ.1)THEN                                   PARAM.1619
            PRINT *,'ERROR: ISOIL=1 REQUIRES IBLTYP=2,4,5 OR 6'                  PARAM.1620
            IWARN=IWARN+1                                                        PARAM.1621
          ENDIF                                                                  PARAM.1622
          IF((IBLTYP(N).EQ.5.OR.IBLTYP(N).EQ.4).AND.ISOIL.EQ.0)THEN              PARAM.1623
            PRINT *,'ERROR: IBLTYP=4 OR 5 REQUIRES ISOIL>0'                      PARAM.1624
            IWARN=IWARN+1                                                        PARAM.1625
          ENDIF                                                                  PARAM.1626
          IF(IBLTYP(N).NE.5.AND.IBLTYP(N).NE.4.AND.ISOIL.EQ.2)THEN               05DEC01.159
            PRINT *,'ERROR: ISOIL=2 REQUIRES IBLTYP=4 OR 5'                      05DEC01.160
            IWARN=IWARN+1                                                        PARAM.1629
          ENDIF                                                                  PARAM.1630
          IF((IBLTYP(N).NE.4.AND.IBLTYP(N).NE.5).AND.IEXSI.GE.1)THEN             23DEC04.177
            PRINT *,'ERROR: IEXSI.GE.1 REQUIRES IBLTYP=4 OR 5'                   23DEC04.178
            IWARN=IWARN+1                                                        19DEC02.477
          ENDIF                                                                  19DEC02.478
          IF(IBLTYP(N).EQ.7.AND.ISOIL.NE.3)THEN                                  07NOV00.255
            PRINT *,'ERROR: IBLTYP=7 REQUIRES ISOIL=3'                           07NOV00.256
            IWARN=IWARN+1                                                        07NOV00.257
          ENDIF                                                                  07NOV00.258
          IF(IBLTYP(N).NE.7.AND.ISOIL.EQ.3)THEN                                  07NOV00.259
            PRINT *,'ERROR: ISOIL=3 REQUIRES IBLTYP=7'                           07NOV00.260
            IWARN=IWARN+1                                                        07NOV00.261
          ENDIF                                                                  07NOV00.262
          IF(IFRAD.EQ.2.AND.IMOIST(N).NE.2)THEN                                  PARAM.1631
            PRINT *,'ERROR: IFRAD=2 REQUIRES IMPHYS>3'                           PARAM.1632
            IWARN=IWARN+1                                                        PARAM.1633
          ENDIF                                                                  PARAM.1634
          IF(IFRAD.EQ.4.AND.IMOIST(N).NE.2)THEN                                  25JAN00.78
            PRINT *,'ERROR: IFRAD=4 REQUIRES IMPHYS>3'                           25JAN00.79
            IWARN=IWARN+1                                                        25JAN00.80
          ENDIF                                                                  25JAN00.81
          IF(IFRAD.EQ.3.AND.(IMOIST(N).NE.2.AND.ICLOUD(N).EQ.1))THEN             05DEC01.161
            PRINT *,'ERROR: IFRAD=3 AND ICLOUD=1 REQUIRE IMPHYS>3'               05DEC01.162
            IWARN=IWARN+1                                                        05DEC01.163
          ENDIF                                                                  05DEC01.164
          IF(IFRAD.NE.3.AND.ICLOUD(N).EQ.2)THEN                                  05DEC01.165
            PRINT *,'ERROR: ICLOUD=2 ONLY WORKS WITH IFRAD = 3'                  05DEC01.166
            IWARN=IWARN+1                                                        05DEC01.167
          ENDIF                                                                  05DEC01.168
          IF(IFSNOW(N).EQ.2.AND.IMPHYS(N).NE.4.AND.IMPHYS(N).NE.5                05DEC01.169
     -           .AND.IMPHYS(N).NE.7)THEN                                        05DEC01.170
            PRINT *,'ERROR: IFSNOW=2 REQUIRES IMPHYS=4,5 OR 7'                   05DEC01.171
            IWARN=IWARN+1                                                        05DEC01.172
          ENDIF                                                                  05DEC01.173
          IF(IFSNOW(N).EQ.2.AND.ISOIL.EQ.2)THEN                                  19DEC02.479
            PRINT *,'ERROR: IFSNOW=2 CANNOT BE USED WHEN ISOIL=2'                19DEC02.480
            PRINT *,'       CHANGE THE VALUE OF EITHER IFSNOW or ISOIL'          19DEC02.481
            IWARN=IWARN+1                                                        19DEC02.482
          ENDIF                                                                  19DEC02.483
          IF(IOVERW(N).EQ.2.AND.IMOIAV(N).EQ.2) THEN                             19DEC02.484
            PRINT *,'ERROR: IMOIAV=2 REQUIRES IOVERW=1'                          19DEC02.485
            PRINT *,'       IMOIAV=2 DOES NOT WORK WELL WITH'                    19DEC02.486
            PRINT *,'       INTERPOLATED SOIL MOISTURE'                          19DEC02.487
            IWARN=IWARN+1                                                        19DEC02.488
          ENDIF                                                                  19DEC02.489
          IF(NUMNC(N).EQ.0)PRINT*,'ERROR: NUMNC NOT SET FOR NEST ',N             PARAM.1635
          IF(LEVIDN(N).EQ.0)PRINT*,'ERROR: LEVIDN NOT SET FOR NEST ',N           PARAM.1636
          IF(NUMNC(N).EQ.0.OR.LEVIDN(N).EQ.0)IWARN=IWARN+1                       PARAM.1637
          IF(LEVIDN(NUMNC(N)).NE.(LEVIDN(N)-1))THEN                              PARAM.1638
            IWARN=IWARN+1                                                        PARAM.1639
            PRINT *,'ERROR: LEVIDN OF NEST ',N,' INCORRECT'                      PARAM.1640
          ENDIF                                                                  PARAM.1641
          IF(IARASC.EQ.0.AND.ICUPA(N).EQ.4)THEN                                  PARAM.1642
            PRINT *,'ERROR: ICUPA=4: REQUIRES IARASC=1'                          PARAM.1643
            IWARN=IWARN+1                                                        PARAM.1644
          ENDIF                                                                  PARAM.1645
          IF(IBLTYP(N).NE.2.AND.(IBLTYP(N).LT.4.OR.IBLTYP(N).GT.7).AND.          05DEC01.174
     +      I4DTOT.GT.0)THEN                                                     05DEC01.175
            IWARN=IWARN+1                                                        PARAM.1648
            PRINT *,'ERROR: FDDA REQUIRES IBLTYP=2, 4, 5, 6, OR 7'               05DEC01.176
          ENDIF                                                                  PARAM.1650
          IF(IBLTYP(N).EQ.0)ISFFLX(N)=0                                          PARAM.1651
          IF(IBLTYP(N).EQ.0)ITGFLG(N)=3                                          PARAM.1652
          IF(ISFFLX(N).NE.1)ITGFLG(N)=3                                          PARAM.1653
          IF(ISFFLX(N).NE.1.OR.ITGFLG(N).NE.1)ICLOUD(N)=0                        PARAM.1654
C THE FOLLOWING LINE IS DELETED IN RELEASE-2-4                                   PARAM.1655
C         IF(ISFFLX(N).NE.1.OR.ITGFLG(N).NE.1)ISFPAR(N)=0                        PARAM.1656
          IF(ISFPAR(N).EQ.0)IFSNOW(N)=0                                          PARAM.1657
          IF(IBLTYP(N).EQ.0)IVMIXM(N)=0                                          PARAM.1658
          IF(IDRY(N).EQ.1)ICLOUD(N)=0                                            PARAM.1659
          IF(IDRY(N).EQ.1)IMOIST(N)=1                                            PARAM.1660
          IF(IDRY(N).EQ.0.AND.IMOIST(N).EQ.0)ICLOUD(N)=0                         PARAM.1661
          IF(IDRY(N).EQ.1.OR.IMOIST(N).EQ.0)XMOIST(N)=0.                         PARAM.1662
          IF(IDRY(N).EQ.1.OR.IMOIST(N).NE.2)HYDPRE(N)=0.                         PARAM.1663
          IF(IMOIST(N).EQ.1)IFOGMD(N)=0                                          PARAM.1664
          IF(IBLTYP(N).NE.6)IFOGMD(N)=0                                          PARAM.1665
          IFOGSW(N)=IFOGMD(N)                                                    PARAM.1666
C                                                                                PARAM.1667
   30   CONTINUE                                                                 PARAM.1668
      ENDIF                                                                      PARAM.1669
#endif                                                                           PARAM.1670
      KTAU=0                                                                     PARAM.1671
      XTIME=0.                                                                   PARAM.1672
C                                                                                PARAM.1673
C-----DIMENSIONS :                                                               PARAM.1674
C                                                                                PARAM.1675
C.....FOR LARGE DOMAIN:                                                          PARAM.1676
      IL=NESTIX(1)                                                               PARAM.1677
      JL=NESTJX(1)                                                               PARAM.1678
#ifndef MPP1                                                                     PARAM.1679
      IF(IL.GT.MIX.OR.JL.GT.MJX)THEN                                             PARAM.1680
#else                                                                            PARAM.1681
      IF(IL.GT.MIX_G.OR.JL.GT.MJX_G)THEN                                         PARAM.1682
#endif                                                                           PARAM.1683
        IWARN=IWARN+1                                                            PARAM.1684
        PRINT *,'ERROR: COARSE MESH NESTIX>MIX OR NESTJX>MJX'                    PARAM.1685
        PRINT *,'  MIX MJX NESTIX NESTJX = ',MIX,MJX,                            PARAM.1686
     +          NESTIX(1),NESTJX(1)                                              PARAM.1687
      ENDIF                                                                      PARAM.1688
      KL=MKX                                                                     PARAM.1689
      KLP1=KL+1                                                                  PARAM.1690
      KLM=KL-1                                                                   PARAM.1691
      ILX=IL-1                                                                   PARAM.1692
      ILXM=ILX-1                                                                 PARAM.1693
      JLX=JL-1                                                                   PARAM.1694
      JLXM=JLX-1                                                                 PARAM.1695
C                                                                                PARAM.1696
C                                                                                PARAM.1697
C                                                                                PARAM.1698
C-----SPECIFY HOW MANY FILES TO BE SKIPPED IN INITIAL DATA SET                   PARAM.1699
C     (UNIT 10):                                                                 PARAM.1700
C                                                                                PARAM.1701
#ifdef MPP1                                                                      PARAM.1702
#  include <mpp_param_20.incl>                                                   PARAM.1703
#endif                                                                           PARAM.1704
#ifdef MPP1                                                                      PARAM.1705
#  include <mpp_param_25.incl>                                                   PARAM.1706
#endif                                                                           PARAM.1707
C                                                                                PARAM.1708
C-----IF ITGFLG=1, SPECIFY THE JULIAN DATE AND GMT OF THE INITIAL DATA.          PARAM.1709
C     DECTIM : IS THE TIME IN MINUTES AFTER WHICH THE SOLAR DECLINATION          PARAM.1710
C              ANGLE MUST BE RECALCULATED.                                       PARAM.1711
C                                                                                PARAM.1712
      IF(IFSKIP .OR. IFRSFA)CDATE=CDATEST                                        23DEC04.179
      IF(.NOT.IFSKIP)CDATEST=CDATE                                               PARAM.1714
      PRINT1178,CDATE                                                            PARAM.1715
1178  FORMAT(1X,'STARTING DATE FOR THIS MM5V3 FORECAST IS ',A19)                 PARAM.1716
      CALL SPLIT_DATE_CHAR(CDATE,MYEAR,MMON,MDAY,MHOUR,MMIN,MSEC)                PARAM.1717
      GMT=MHOUR+FLOAT(MMIN)/60.+FLOAT(MSEC)/3600.                                PARAM.1718
C.....FIND THE YEAR IS A LEAP YEAR OR NOT                                        PARAM.1719
      MY1=MOD(MYEAR,4)                                                           PARAM.1720
      MY2=MOD(MYEAR,100)                                                         PARAM.1721
      MY3=MOD(MYEAR,400)                                                         PARAM.1722
      IF(MY1.EQ.0.AND.MY2.NE.0.OR.MY3.EQ.0)MMD(2)=29                             PARAM.1723
      JULDAY=MDAY                                                                PARAM.1724
      DO 85 MONSS=1,MMON-1                                                       PARAM.1725
        JULDAY=JULDAY+MMD(MONSS)                                                 PARAM.1726
   85 CONTINUE                                                                   PARAM.1727
C  DECLINATION RECALCULATED EVERY 24 HOURS AFTER START (PREVIOUSLY EVERY         PARAM.1728
      DECTIM=1440.                                                               PARAM.1729
      DEGRAD=0.0174533                                                           PARAM.1730
C  EOMEG USES THE SIDEREAL DAY (NOT SOLAR DAY)                                   07NOV00.263
      EOMEG=7.2921E-5                                                            07NOV00.264
      DPD=0.986301                                                               PARAM.1732
      EQOFT=0.                                                                   PARAM.1733
C      SOLCON=1.3956E3 (THIS LINE REPLACED BY CALCULATION IN SOLAR1)             PARAM.1734
      STBOLT=5.67051E-8                                                          PARAM.1735
   90 CONTINUE                                                                   PARAM.1736
C                                                                                25JAN00.82
C  DEFINE SEASON ACCORDING TO JULIAN DAY AND CENTER LATITUDE (BHR(2,1))          25JAN00.83
C                                                                                25JAN00.84
      ISN=1                                                                      25JAN00.85
      IF(JULDAY.LT.105.OR.JULDAY.GT.288)ISN=2                                    25JAN00.86
      IF(BHR(2,1).LT.0.0)ISN=3-ISN                                               25JAN00.87
C                                                                                PARAM.1737
C-----SPECIFY HEATING PROFILE (TWGHT) AND WEIGHTED FUNCTION                      PARAM.1738
C     FOR MOISTURE FLUXES DUE TO CONVECTION (VQFLX)                              PARAM.1739
C     ASSUME BASE OF CLOUD VARIES AS  < KBASE = 5,KL >                           PARAM.1740
C            TOP  OF CLOUD VARIES AS  < KTOP  = 1,KBASE-3 >                      PARAM.1741
C     EXCEPTIONS TO THIS ARE TREATED EXPLICITLY IN SUBROUTINE                    PARAM.1742
C     "CUPARA".                                                                  PARAM.1743
C                                                                                PARAM.1744
#ifdef ICUPA2                                                                    07NOV00.265
      DO 108 KBASE=5,KL                                                          PARAM.1745
        DO 108 KTOP=1,KBASE-3                                                    PARAM.1746
C                                                                                PARAM.1747
          DO 102 K=1,KL                                                          PARAM.1748
            TWGHT(K,KBASE,KTOP)=0.                                               PARAM.1749
            VQFLX(K,KBASE,KTOP)=0.                                               PARAM.1750
  102     CONTINUE                                                               PARAM.1751
C                                                                                PARAM.1752
C.....GET TWGHT FROM 1/2 LEVEL SIGMA VALUES                                      PARAM.1753
C                                                                                PARAM.1754
          AA=ALOG(A(KTOP))+ALOG(A(KBASE))                                        19DEC02.490
          CC=ALOG(A(KTOP))*ALOG(A(KBASE))                                        PARAM.1756
          SSUM=0.                                                                PARAM.1757
          DO 103 K=KTOP,KBASE                                                    PARAM.1758
            XX=ALOG(A(K))                                                        PARAM.1759
            TWGHT(K,KBASE,KTOP)=(XX*XX)-(AA*XX)+CC                               19DEC02.491
            SSUM=SSUM+TWGHT(K,KBASE,KTOP)*DSIGMA(K)                              PARAM.1761
  103     CONTINUE                                                               PARAM.1762
          DO 104 K=KTOP,KBASE                                                    PARAM.1763
            TWGHT(K,KBASE,KTOP)=TWGHT(K,KBASE,KTOP)/SSUM                         PARAM.1764
  104     CONTINUE                                                               PARAM.1765
C                                                                                PARAM.1766
C.....GET VQFLX FROM  D(W*Q) / DSIGMA ON FULL LEVELS                             PARAM.1767
C     DO COMPUTATIONS IN P TO AVOID SIGMA=0. DISCONTINUITY                       PARAM.1768
C                                                                                PARAM.1769
          XTOP=ALOG((100.-PTOP)*SIGMA(KTOP)+PTOP)                                PARAM.1770
          XBOT=ALOG((100.-PTOP)*SIGMA(KBASE+1)+PTOP)                             PARAM.1771
          AA=XTOP+XBOT                                                           19DEC02.492
          CC=XTOP*XBOT                                                           PARAM.1773
          VQMAX=0.                                                               PARAM.1774
          SSUM=0.                                                                PARAM.1775
C                                                                                PARAM.1776
          XX=XTOP                                                                PARAM.1777
          YY=XBOT                                                                PARAM.1778
          WK=(XX*XX)-(AA*XX)+CC                                                  19DEC02.493
          QK=-((YY*YY)-(AA*YY)+CC)                                               19DEC02.494
          DO 105 K=KTOP,KBASE                                                    PARAM.1781
            XX=ALOG((100.-PTOP)*SIGMA(K+1)+PTOP)                                 PARAM.1782
            YY=                                                                  PARAM.1783
     +         ALOG((100.-PTOP)*(SIGMA(KTOP)+SIGMA(KBASE+1)-SIGMA(K+1))+         PARAM.1784
     +         PTOP)                                                             PARAM.1785
            WKP1=(XX*XX)-(AA*XX)+CC                                              19DEC02.495
            QKP1=-((YY*YY)-(AA*YY)+CC)                                           19DEC02.496
            VQFLX(K,KBASE,KTOP)=-((WKP1*QKP1)-(WK*QK))/DSIGMA(K)                 PARAM.1788
            SSUM=SSUM+VQFLX(K,KBASE,KTOP)                                        PARAM.1789
            IF(ABS(VQFLX(K,KBASE,KTOP)).GT.VQMAX)VQMAX=                          PARAM.1790
     +        ABS(VQFLX(K,KBASE,KTOP))                                           PARAM.1791
            WK=WKP1                                                              PARAM.1792
            QK=QKP1                                                              PARAM.1793
  105     CONTINUE                                                               PARAM.1794
          DO 106 K=KTOP,KBASE                                                    PARAM.1795
            VQFLX(K,KBASE,KTOP)=VQFLX(K,KBASE,KTOP)*VQRANG/VQMAX                 PARAM.1796
  106     CONTINUE                                                               PARAM.1797
C                                                                                PARAM.1798
  108   CONTINUE                                                                 PARAM.1799
#endif                                                                           07NOV00.266
                                                                                 PARAM.1800
C                                                                                PARAM.1801
C                                                                                PARAM.1802
C-----SPECIFY THE CONSTANTS USED IN THE MODEL.                                   PARAM.1803
C                                                                                PARAM.1804
      R=287.04                                                                   PARAM.1805
      CP=1004.                                                                   PARAM.1806
      ROVCP=R/CP                                                                 PARAM.1807
      G=9.81                                                                     PARAM.1808
      ROVG=R/G                                                                   PARAM.1809
      KARMAN=0.4                                                                 PARAM.1810
      RHOS=1.16                                                                  PARAM.1811
      PTOP4=4.*PTOP                                                              PARAM.1812
      CD=0.002                                                                   PARAM.1813
      CDSEA=0.0015                                                               PARAM.1814
      CH=0.002                                                                   PARAM.1815
      CHSEA=0.0015                                                               PARAM.1816
      ALPHA=0.                                                                   PARAM.1817
      BETA=1.-2.*ALPHA                                                           PARAM.1818
      GNU=0.1                                                                    PARAM.1819
      OMU=1.-2.*GNU                                                              PARAM.1820
      GNUHF=GNU                                                                  PARAM.1821
      OMUHF=1.-2.*GNUHF                                                          PARAM.1822
      DX2=2.*DX                                                                  PARAM.1823
      DX4=4.*DX                                                                  PARAM.1824
      DX8=8.*DX                                                                  PARAM.1825
      DX16=16.*DX                                                                PARAM.1826
      DXSQ=DX*DX                                                                 PARAM.1827
      C200=KARMAN*KARMAN*DX/4.                                                   PARAM.1828
      C201=(100.-PTOP)/DXSQ                                                      PARAM.1829
      C203=1./DXSQ                                                               PARAM.1830
      RV=461.5                                                                   PARAM.1831
      XLV=2.5E6                                                                  PARAM.1832
      EP1=0.608                                                                  PARAM.1833
      EP2=0.622                                                                  PARAM.1834
      XLVOCP=XLV/CP                                                              PARAM.1835
      SVP1=0.6112                                                                PARAM.1836
      SVP2=17.67                                                                 PARAM.1837
      SVP3=29.65                                                                 PARAM.1838
      SVPT0=273.15                                                               PARAM.1839
      PI=4.*ATAN(1.)                                                             PARAM.1840
      AVT=841.99667                                                              PARAM.1841
      BVT=0.8                                                                    PARAM.1842
      G4PB=GAMMA(4.+BVT)                                                         PARAM.1843
      G3PB=GAMMA(3.+BVT)                                                         PARAM.1844
      G5PB=GAMMA(0.5*(5.+BVT))                                                   PARAM.1845
      N0R=8.E6                                                                   PARAM.1846
      PPI=1./(PI*N0R)                                                            PARAM.1847
      VTC=AVT*G4PB/6.                                                            PARAM.1848
      PRAC=PI*N0R*AVT*G3PB*0.25                                                  PARAM.1849
      PREC1=2.*PI*N0R*0.78                                                       PARAM.1850
      PREC2=2.*PI*N0R*0.32*AVT**0.5*G5PB                                         PARAM.1851
      AVTS=11.72                                                                 PARAM.1852
      BVTS=.41                                                                   PARAM.1853
      BETAI=.6                                                                   PARAM.1854
      ESI=0.1                                                                    PARAM.1855
      G4PBS=GAMMA(4.+BVTS)                                                       PARAM.1856
      G3PBS=GAMMA(3.+BVTS)                                                       PARAM.1857
      G5PBS=GAMMA(0.5*(5.+BVTS))                                                 PARAM.1858
      N0S=2.E7                                                                   PARAM.1859
      PRACS=PI*N0S*AVTS*G3PBS*.25*ESI                                            PARAM.1860
      PRACS1=PRACS                                                               PARAM.1861
      PRECS1=4.*N0S*.65                                                          PARAM.1862
      PRECS2=4.*N0S*.44*AVTS**.5*G5PBS                                           PARAM.1863
      RHOS=0.1                                                                   PARAM.1864
      PPIS=1./(PI*N0S*RHOS)                                                      PARAM.1865
      VTCS=AVTS*G4PBS/6.                                                         PARAM.1866
      TO=273.15                                                                  PARAM.1867
      IF(IEXICE.EQ.0)TO=50.                                                      PARAM.1868
      XLV0=3.15E6                                                                PARAM.1869
      XLV1=2370.                                                                 PARAM.1870
      XN0=1.E-2                                                                  PARAM.1871
      XMMAX=9.4E-10                                                              PARAM.1872
      XM0=(12.9E-6/16.3)**2                                                      PARAM.1873
      XLF0=.3337E6                                                               PARAM.1874
      XLS=XLV0-XLV1*273.16+XLF0                                                  PARAM.1875
  120 CONTINUE                                                                   PARAM.1876
#ifdef FRAD3                                                                     PARAM.1877
      IF(IFRAD.EQ.3)CALL RADINI(G,CP,EP2,STBOLT)                                 PARAM.1878
#endif                                                                           PARAM.1879
#ifdef FRAD4                                                                     25JAN00.88
      IF(IFRAD.EQ.4)CALL RRTMINIT                                                25JAN00.89
#endif                                                                           25JAN00.90
                                                                                 25JAN00.91
      IF(.NOT.IFREST)THEN                                                        PARAM.1880
C.....INDEXES:                                                                   PARAM.1881
        JOUT=1                                                                   PARAM.1882
C                                                                                PARAM.1883
  130   CONTINUE                                                                 PARAM.1884
C                                                                                PARAM.1885
C--- CALCULATE DT FOR NON-HYDROSTATIC MODEL                                      PARAM.1886
C                                                                                PARAM.1887
        IF(INHYD.EQ.1.AND.TISTEP.LT.1.E-04)THEN                                  PARAM.1888
C      MAXIMUM FACTOR (NOT 0.3)                                                  PARAM.1889
          FACTOR=1.0                                                             PARAM.1890
C      SPEED OF FASTEST INTERNAL GRAVITY WAVE (OR WMAX*DX/DZ)                    PARAM.1891
          C=200.-0.01                                                            PARAM.1892
          DTMAX=DX*FACTOR/C                                                      PARAM.1893
C     INTEGER TIME STEPS PER HOUR                                                PARAM.1894
          IPERHR=IFIX(3600./DTMAX)+1                                             PARAM.1895
          DT=3600./IPERHR+1.E-10                                                 PARAM.1896
        ENDIF                                                                    PARAM.1897
      ENDIF                                                                      PARAM.1898
      DT2=2.*DT                                                                  PARAM.1899
C.....CALCULATE THE TIME STEP IN MINUTES.                                        PARAM.1900
      DTMIN=DT/60.                                                               PARAM.1901
      DELTMX=DT                                                                  PARAM.1902
C.....COMPUTE THE TIME STEPS FOR RADIATION COMPUTATION.                          PARAM.1903
      NTRAD(1)=NINT(RADFRQ/DTMIN)                                                PARAM.1904
  140 CONTINUE                                                                   PARAM.1905
C      XKHZ=3.E-3*DXSQ/DT                                                        05DEC01.177
C     NEW CALCULATION MAKES BACKGROUND DIFFUSION INDEPENDENT OF TIMESTEP         05DEC01.178
C     (XU ET AL., MWR, 2001, 502-516)                                            05DEC01.179
      XKHZ=CKH*DX                                                                05DEC01.180
      XKHMAX=2.*DXSQ/(64.*DT)                                                    PARAM.1907
C                                                                                PARAM.1908
C-----COMPUTE THE VERTICAL INTERPOLATION COEFFICIENTS FOR T AND QV.              PARAM.1909
C                                                                                PARAM.1910
      TWT(1,1)=0.                                                                PARAM.1911
      TWT(1,2)=0.                                                                PARAM.1912
      QCON(1)=0.                                                                 PARAM.1913
      DO 150 K=2,KL                                                              PARAM.1914
        TWT(K,1)=(SIGMA(K)-A(K-1))/(A(K)-A(K-1))                                 PARAM.1915
        TWT(K,2)=1.-TWT(K,1)                                                     PARAM.1916
        QCON(K)=(SIGMA(K)-A(K))/(A(K-1)-A(K))                                    PARAM.1917
  150 CONTINUE                                                                   PARAM.1918
C                                                                                PARAM.1919
C-----DETERMINE THE CLOUD BOUNDARIES FOR RADIATION CALCULATION.                  PARAM.1920
C     DETERMINE SIGMA LEVELS FOR LOWER BOUNDS OF HI, MID, AND LOW                PARAM.1921
C     CLOUDS (FROM DEFINITIONS OF LOW/ALTO BOUNDARY AS 6 KFT AND                 PARAM.1922
C     ALTO/CIRRO BOUNDARY AS 20 KFT).                                            PARAM.1923
C                                                                                PARAM.1924
      IF(ICLOUD(1).EQ.0)GOTO 170                                                 PARAM.1925
      CLOBOT=970.                                                                PARAM.1926
      CMDBOT=800.                                                                PARAM.1927
      CHIBOT=450.                                                                PARAM.1928
      PTMB=10.*PTOP                                                              PARAM.1929
      PZ=A(1)*(1000.-PTMB)+PTMB                                                  PARAM.1930
      IF(PZ.GT.CHIBOT)STOP 160                                                   PARAM.1931
      DO 160 K=1,KL                                                              PARAM.1932
        PK=A(K)*(1000.-PTMB)+PTMB                                                PARAM.1933
        IF(PK.GT.CLOBOT)GOTO 160                                                 PARAM.1934
        KCLO=K                                                                   PARAM.1935
        IF(PK.GT.CMDBOT)GOTO 160                                                 PARAM.1936
        KCMD=K                                                                   PARAM.1937
        IF(PK.GT.CHIBOT)GOTO 160                                                 PARAM.1938
        KCHI=K                                                                   PARAM.1939
  160 CONTINUE                                                                   PARAM.1940
  170 CONTINUE                                                                   PARAM.1941
C                                                                                PARAM.1942
C-----COMPUTE THE K LEVEL UNDER WHICH THE MAXIMUM EQUIVALENT POTENTIAL           PARAM.1943
C     TEMPERATURE WILL BE REGARDED AS THE ORIGIN OF AIR PARCEL THAT              PARAM.1944
C     PRODUCES CLOUD (USED IN THE CUMULUS PARAMETERIZATION SCHEME).              PARAM.1945
C                                                                                PARAM.1946
      SIG700=(70.-PTOP)/(100.-PTOP)                                              PARAM.1947
      DO 192 K=1,KL                                                              PARAM.1948
        K700=K                                                                   PARAM.1949
        IF(SIG700.LE.SIGMA(K+1).AND.SIG700.GT.SIGMA(K))GOTO 194                  PARAM.1950
  192 CONTINUE                                                                   PARAM.1951
  194 CONTINUE                                                                   PARAM.1952
      IF(ISOIL.EQ.1.AND.ISLDIM.EQ.1)THEN                                         PARAM.1953
C---SPECIFY CONSTANTS AND LAYERS FOR SOIL MODEL                                  PARAM.1954
C   SOIL DIFFUSION CONSTANT SET (M^2/S)                                          PARAM.1955
        DIFSL=5.E-7                                                              PARAM.1956
C   DEFINE LAYERS (TOP LAYER = 0.01 M)                                           PARAM.1957
        DZS(1)=.01                                                               PARAM.1958
        ZS(1)=.5*DZS(1)                                                          PARAM.1959
        DO L=2,MLX                                                               PARAM.1960
          DZS(L)=2*DZS(L-1)                                                      PARAM.1961
          ZS(L)=ZS(L-1)+.5*DZS(L-1)+.5*DZS(L)                                    PARAM.1962
        ENDDO                                                                    PARAM.1963
#if defined (SOIL3) 
      ELSEIF(ISOIL.EQ.3.AND.ILDDIM.EQ.1)THEN                                     07NOV00.268
        DZS(1)=0.01                                                              07NOV00.269
        DZS(2)=0.99                                                              07NOV00.270
        ZS(1)=0.5*DZS(1)                                                         07NOV00.271
        ZS(2)=DZS(1)+0.5*DZS(2)                                                  07NOV00.272
        DO NN3=1,MLX                                                             07NOV00.273
          IF(ISTLYR(NN3).GT.0)NSTL=NN3                                           07NOV00.274
          IF(ISMLYR(NN3).GT.0)NSML=NN3                                           07NOV00.275
        ENDDO                                                                    07NOV00.276
#elif defined (SOIL2)
      ELSEIF(ISOIL.EQ.2.AND.ILDDIM.EQ.1)THEN                                     PARAM.1964
        DZS(1)=0.1                                                               PARAM.1965
        DZS(2)=0.3                                                               PARAM.1966
        DZS(3)=0.6                                                               PARAM.1967
        DZS(4)=1.0                                                               PARAM.1968
        ZS(1)=0.5*DZS(1)                                                         PARAM.1969
        ZS(2)=DZS(1)+0.5*DZS(2)                                                  PARAM.1970
        ZS(3)=DZS(1)+DZS(2)+0.5*DZS(3)                                           PARAM.1971
        ZS(4)=DZS(1)+DZS(2)+DZS(3)+0.5*DZS(4)                                    PARAM.1972
        DO NN3=1,MLX                                                             24SEP99.86
          IF(ISTLYR(NN3).GT.0)NSTL=NN3                                           24SEP99.87
          IF(ISMLYR(NN3).GT.0)NSML=NN3                                           24SEP99.88
        ENDDO                                                                    24SEP99.89
#endif                                                                           07NOV00.278
      ENDIF                                                                      PARAM.1973
C                                                                                PARAM.1974
C-----SPECIFY THE COEFFICIENTS FOR SPONGE BOUNDARY CONDITIONS.                   PARAM.1975
C                                                                                PARAM.1976
      ISPGD=NSPGD-1                                                              PARAM.1977
      ISPGX=NSPGX-1                                                              PARAM.1978
C.....FOR DOT POINT VARIABLES:                                                   PARAM.1979
      IF(IBOUDY(1).NE.4)GOTO 200                                                 PARAM.1980
      WGTD(1)=0.                                                                 PARAM.1981
      WGTD(2)=0.2                                                                PARAM.1982
      WGTD(3)=0.55                                                               PARAM.1983
      WGTD(4)=0.8                                                                PARAM.1984
      WGTD(5)=0.95                                                               PARAM.1985
C.....FOR CROSS POINT VARIABLES:                                                 PARAM.1986
      WGTX(1)=0.0                                                                PARAM.1987
      WGTX(2)=0.4                                                                PARAM.1988
      WGTX(3)=.7                                                                 PARAM.1989
      WGTX(4)=.9                                                                 PARAM.1990
  200 CONTINUE                                                                   PARAM.1991
C                                                                                PARAM.1992
C-----SPECIFY THE COEFFICIENTS FOR NUDGING BOUNDARY CONDITIONS:                  PARAM.1993
C                                                                                PARAM.1994
C.....FOR LARGE DOMAIN:                                                          PARAM.1995
      IF(IBOUDY(1).EQ.1.OR.IBOUDY(1).EQ.3)THEN                                   PARAM.1996
        FNUDGE=0.1/DT2                                                           PARAM.1997
        GNUDGE=(DXSQ/DT)/50.                                                     PARAM.1998
  202   CONTINUE                                                                 PARAM.1999
      ENDIF                                                                      PARAM.2000
      BDYTIM=0.                                                                  PARAM.2001
      TSKIP=0.                                                                   PARAM.2002
      BDYFRQ=0.                                                                  PARAM.2003
      BDYLFRQ=0.                                                                 07NOV00.279
      BDYLTIM=0.                                                                 07NOV00.280
C                                                                                PARAM.2004
C-----READ IN TRANSMISSIONS AS F(PREC. WATER, PATH LENGTH)                       PARAM.2005
C                                                                                PARAM.2006
      IF(ITGFLG(1).NE.1)GOTO 210                                                 PARAM.2007
      CALL TRANS                                                                 PARAM.2008
  210 CONTINUE                                                                   PARAM.2009
#if defined(IBLT6) || defined(IBLT7)
C FOR REGIME 1, ADJUST KZO: NLS/DRS SEPT 1991                                    PARAM.2011
      DO 5678 K=1,KL                                                             PARAM.2012
        IF(A(K).GE.0.9925)THEN                                                   PARAM.2013
          KZOGS(K)=0.5                                                           PARAM.2014
        ELSEIF(A(K).LE.0.9800)THEN                                               PARAM.2015
          KZOGS(K)=0.05                                                          PARAM.2016
        ELSE                                                                     PARAM.2017
          KZOGS(K)=36.0*(A(K)-0.98)+0.05                                         PARAM.2018
        ENDIF                                                                    PARAM.2019
 5678 CONTINUE                                                                   PARAM.2020
C      PRINT*,'******************** KZO *************************'               PARAM.2021
C      DO 5680 K=1,MKX                                                           PARAM.2022
C         WRITE(6,5679)K,KZOGS(K)                                                PARAM.2023
C5680  CONTINUE                                                                  PARAM.2024
C5679  FORMAT(10X,'KZOGS(',I2,') = ',F5.2)                                       PARAM.2025
C      PRINT*,'************************************************** '              PARAM.2026
#endif                                                                           PARAM.2027
C                                                                                PARAM.2028
#if defined(ICUPA6) || defined(ICUPA7) || defined(ICUPA8)
      CALL KFBMDATA                                                              PARAM.2030
#endif                                                                           PARAM.2031
#ifdef ICUPA7                                                                    PARAM.2032
      CALL LUTBL(PTOP)                                                           PARAM.2033
#endif                                                                           PARAM.2034
#ifdef ICUPA8                                                                    05DEC01.182
      CALL LUTAB                                                                 05DEC01.183
#endif                                                                           05DEC01.184
C BMMODS                                                                         PARAM.2035
C      PRINT OPARAM                                                              PARAM.2036
C      PRINT LPARAM                                                              PARAM.2037
C      PRINT PPARAM                                                              PARAM.2038
C      PRINT FPARAM                                                              PARAM.2039
#ifdef ICUPA3                                                                    PARAM.2040
C                                                                                PARAM.2041
C     PARAMETERS FOR GRELL SCHEME NAMELIST VARIABLES IN PARGRCU.INCL             PARAM.2042
C                                                                                PARAM.2043
      DO K=1,MKX                                                                 PARAM.2044
        IF(A(K).LE.SIGCBHI)THEN                                                  PARAM.2045
          KBMAX=MKX-K                                                            PARAM.2046
        ENDIF                                                                    PARAM.2047
      ENDDO                                                                      PARAM.2048
#endif                                                                           PARAM.2049
C                                                                                PARAM.2050
C  SET I4DTOT AND I4DITOT FOR BOTH START AT BEGINNING AND RESTART:               PARAM.2051
C                                                                                PARAM.2052
      I4DCHECK=MAXNES                                                            PARAM.2053
      I4DITOT=0                                                                  PARAM.2054
      I4DTOT=0                                                                   PARAM.2055
      DO 1310 IN=1,I4DCHECK                                                      PARAM.2056
        I4DTOT=I4DTOT+I4D(IN,1)+I4D(IN,2)+I4DI(IN)                               PARAM.2057
        I4DITOT=I4DITOT+I4DI(IN)                                                 PARAM.2058
C                                                                                PARAM.2059
        IF(I4DI(IN).EQ.1.AND.NIOBF.EQ.1)THEN                                     PARAM.2060
          PRINT1306                                                              PARAM.2061
1306  FORMAT(' !!!!! ATTEMPTING OBS NUDGING WITHOUT COMPILING OBS',              19DEC02.497
     1' NUDGING CODE.',/,'       SET FDDAOBS=1 IN configure.user',               19DEC02.498
     2' FILE AND RECOMPILE.')                                                    19DEC02.499
          STOP 1306                                                              PARAM.2064
        ENDIF                                                                    PARAM.2065
C                                                                                PARAM.2066
        IF(I4D(IN,2).EQ.1)THEN                                                   PARAM.2067
C                                                                                PARAM.2068
C NEW DRSMODS...                                                                 PARAM.2069
          IF(ABS(A(KL)-0.995).GT.0.0001)THEN                                     PARAM.2070
C COMPUTE CORRECTION FOR A SURFACE LAYER NOT EQUAL TO 40 M                       PARAM.2071
C (SEE STAUFFER ET AL., 1991, MWR)                                               PARAM.2072
C COMPUTE APPROXIMATE HEIGHT AT A(KL)...                                         PARAM.2073
            WP=A(KL)*(100.-PTOP)+PTOP                                            PARAM.2074
C CONVERT TO MB...                                                               PARAM.2075
            DIFP=(100.-WP)*10.                                                   PARAM.2076
C DIFP IS THE PRESSURE HEIGHT OF A(KL) IN MB...                                  PARAM.2077
C USE 9M/MB TO COMPUTE DIFZ, THE HEIGHT OF A(KL) IN METERS...                    PARAM.2078
            DIFZ=DIFP*9.                                                         PARAM.2079
            WR14=ALOG(40.0/0.05)                                                 PARAM.2080
            WR1Z=ALOG(DIFZ/0.05)                                                 PARAM.2081
            WR24=ALOG(40.0/1.0)                                                  PARAM.2082
            WR2Z=ALOG(DIFZ/1.0)                                                  PARAM.2083
            WNDFAC=0.5*(WR1Z/WR14+WR2Z/WR24)                                     PARAM.2084
C NOTE: IF DIFZ.LE.10. THEN WIND CORRECTION IS SET TO ONE IN BLNUDGD             PARAM.2085
C  (THAT IS, IF HEIGHT OF A(KL) .LE. 10 M, NO WIND CORRECTION NEEDED)            PARAM.2086
C TEMPORARY TEST...                                                              PARAM.2087
            PRINT*,' DIFP,DIFZ,A(KL),PTOP,WP: ',DIFP,DIFZ,A(KL),                 PARAM.2088
     1               PTOP,WP                                                     PARAM.2089
            PRINT*,' WR14,WR1Z,WR24,WR2Z,WNDFAC: ',WR14,WR1Z,WR24,               PARAM.2090
     1               WR2Z,WNDFAC                                                 PARAM.2091
C TEMPORARY TEST...                                                              PARAM.2092
          ENDIF                                                                  PARAM.2093
        ENDIF                                                                    PARAM.2094
C                                                                                PARAM.2095
 1310 CONTINUE                                                                   PARAM.2096
      IF(I4DITOT.NE.0)THEN                                                       PARAM.2097
        INNPFI=NPFI                                                              PARAM.2098
        NPFI=(INNPFI/IONF)*IONF                                                  PARAM.2099
        IF(NPFI.NE.INNPFI)THEN                                                   PARAM.2100
          PRINT *,'!!! NPFI MUST EQUAL N*IONF - SETTING NPFI = ', NPFI           PARAM.2101
        ENDIF                                                                    PARAM.2102
      ENDIF                                                                      PARAM.2103
      IF(I4DTOT.GT.0.AND.IFDDA.EQ.0)THEN                                         PARAM.2104
        PRINT*,' ALSO SET IFDDAG/IFDDAO=1 IF YOU WISH TO USE FDDA ...'           PARAM.2105
        STOP 1311                                                                PARAM.2106
      ENDIF                                                                      PARAM.2107
      IF(I4DTOT.EQ.0.AND.IFDDA.EQ.1)THEN                                         PARAM.2108
       PRINT*,' ALSO SET I4D AND/OR I4DI = 1 IF YOU WISH TO',                    PARAM.2109
     &   ' USE FDDA ...'                                                         PARAM.2110
      ENDIF                                                                      PARAM.2111
      MPHYTBL=0                                                                  PARAM.2112
#ifdef MPHYSTBL1                                                                 PARAM.2113
      MPHYTBL=1                                                                  PARAM.2114
      IF(IMPHYS(1).EQ.5) THEN                                                    23DEC04.180
C CONSTANT IN COOPER CURVES (CONSISTENT WITH CONSTANTS USED IN EXMOISR)          23DEC04.181
        BETAI=0.304                                                              23DEC04.182
        XN0=5.                                                                   23DEC04.183
      END IF                                                                     23DEC04.184
C WHEN LOOKUP TABLE IS USED, USE SAME IMPHYS IN ALL DOMAINS                      23DEC04.185
C   OTHERWISE CONSTANTS USED IN DIFFERENT SCHEMES WILL NOT BE THE SAME           23DEC04.186
C   AS THEY WOULD BE WHEN USING THE NON-LOOKUP TABLE VERSION                     23DEC04.187
      DO N = 2,MAXNES                                                            23DEC04.188
         IF(IMPHYS(N).NE.IMPHYS(1)) PRINT 3110                                   23DEC04.189
      END DO                                                                     23DEC04.190
3110  FORMAT(/,'WARNING: DIFF IMPHYS ARE USED IN DIFF DOMAINS - USING ',         23DEC04.191
     +       'SAME IMPHYS IN ALL DOMAINS IS RECOMMENDED')                        23DEC04.192
      CALL SETTBL(IICE,TO,BVT,BVTS,PPI,PPIS,BETAI,XN0,SVP1,SVP2,SVP3,            PARAM.2115
     +     SVPT0)                                                                PARAM.2116
#endif                                                                           PARAM.2117
#ifdef IBLT5                                                                     PARAM.2118
      DO N=0,1000                                                                PARAM.2119
        ZOLN=-FLOAT(N)*0.01                                                      PARAM.2120
        X=(1-16.*ZOLN)**0.25                                                     PARAM.2121
        PSIMTB(N)=2*ALOG(0.5*(1+X))+ALOG(0.5*(1+X*X))-2.*ATAN(X)+2.*             PARAM.2122
     +            ATAN(1.)                                                       PARAM.2123
        Y=(1-16*ZOLN)**0.5                                                       PARAM.2124
        PSIHTB(N)=2*ALOG(0.5*(1+Y))                                              PARAM.2125
      ENDDO                                                                      PARAM.2126
#endif                                                                           PARAM.2127
C                                                                                PARAM.2128
C ... CLOSE OUT FORTRAN UNITS                                                    PARAM.2129
C                                                                                PARAM.2130
      CLOSE (11)                                                                 PARAM.2131
      CLOSE (19)                                                                 PARAM.2132
      CLOSE (18)                                                                 07NOV00.283
      CLOSE (10)                                                                 PARAM.2133
      IF(IWARN.GT.0)STOP 'SEE ERRORS IN PRINT-OUT'                               PARAM.2134
      IF(IFREST)RETURN                                                           PARAM.2135
C                                                                                PARAM.2136
C-----PRINT OUT THE PARAMETERS SPECIFIED IN THE MODEL.                           PARAM.2137
C                                                                                PARAM.2138
C.....FOR LARGE DOMAIN:                                                          PARAM.2139
C                                                                                PARAM.2140
      PRINT9000                                                                  PARAM.2141
      PRINT9200,MIX,MJX,MKX,MLX,MAXNES                                           07NOV00.284
      PRINT9210,INHYD,IKFFC,IARASC,IEXMS,IICE,IICEG,IRDDIM,ISLDIM,IFDDAG         PARAM.2143
     +          ,IFDDAO                                                          PARAM.2144
      PRINT9000                                                                  PARAM.2145
      IF(IDRY(1).EQ.1)ICUPA(1)=1                                                 PARAM.2146
      IF(IDRY(1).EQ.1)PRINT1000,IDRY(1)                                          PARAM.2147
      IF(IMPHYS(1).EQ.1)PRINT1001,IMPHYS(1)                                      PARAM.2148
      IF(IDRY(1).EQ.0.AND.IMOIST(1).EQ.0)PRINT1010,IMOIST(1)                     PARAM.2149
      IF(IMPHYS(1).EQ.2)PRINT1002,IMPHYS(1)                                      PARAM.2150
      IF(IDRY(1).EQ.0.AND.IMOIST(1).EQ.1)PRINT1020,IMOIST(1)                     PARAM.2151
      IF(IDRY(1).EQ.0.AND.IMOIST(1).EQ.2)THEN                                    PARAM.2152
        IF(IMPHYS(1).EQ.3)PRINT1021,IMPHYS(1)                                    PARAM.2153
        IF(IMPHYS(1).EQ.4)PRINT1022,IMPHYS(1)                                    PARAM.2154
        IF(IMPHYS(1).EQ.5)PRINT1023,IMPHYS(1)                                    PARAM.2155
        IF(IMPHYS(1).EQ.6)PRINT1024,IMPHYS(1)                                    PARAM.2156
        IF(IMPHYS(1).EQ.7)PRINT1025,IMPHYS(1)                                    PARAM.2157
        IF(IMPHYS(1).EQ.8)PRINT1027,IMPHYS(1)                                    PARAM.2158
        PRINT1020,IMOIST(1)                                                      PARAM.2159
        IF(IFDRY.EQ.1)PRINT1029,IFDRY                                            05DEC01.185
        IF(IEVAP(1).LT.0)PRINT1041,IEVAP(1)                                      PARAM.2161
        IF(IEVAP(1).EQ.0)PRINT1042                                               PARAM.2162
        IF(MPHYTBL.EQ.1)PRINT1026,MPHYTBL                                        PARAM.2163
      ENDIF                                                                      PARAM.2164
      IF(ICUPA(1).EQ.1)PRINT1031,ICUPA(1)                                        PARAM.2165
      IF(ICUPA(1).EQ.2)PRINT1032,ICUPA(1)                                        PARAM.2166
      IF(ICUPA(1).EQ.2.AND.ICUSTB.EQ.0)PRINT1039                                 PARAM.2167
      IF(ICUPA(1).EQ.3)PRINT1033,ICUPA(1)                                        PARAM.2168
      IF(ICUPA(1).EQ.4)PRINT1034,ICUPA(1)                                        PARAM.2169
      IF(ICUPA(1).EQ.5)PRINT1035,ICUPA(1)                                        PARAM.2170
      IF(ICUPA(1).EQ.6)PRINT1036,ICUPA(1)                                        PARAM.2171
      IF(ICUPA(1).EQ.7)PRINT1037,ICUPA(1)                                        PARAM.2172
      IF(ICUPA(1).EQ.8)PRINT1049,ICUPA(1)                                        05DEC01.186
      IF(ISHALLO(1).EQ.1)PRINT1038                                               PARAM.2173
C     IF(IEXICE.EQ.1.AND.IMOIST(1).EQ.2)                                         PARAM.2174
C    1   PRINT *,'THIS RUN WITH ICE PHYSICS EFFECTS, NOT MIXED PHASE'            PARAM.2175
C                                                                                PARAM.2176
      IF(IBLTYP(1).EQ.0)PRINT1040,IBLTYP(1)                                      PARAM.2177
      IF(IBLTYP(1).EQ.1)PRINT1050,IBLTYP(1)                                      PARAM.2178
      IF(IBLTYP(1).EQ.2)PRINT1060,IBLTYP(1)                                      PARAM.2179
      IF(IBLTYP(1).EQ.3)PRINT1061,IBLTYP(1)                                      PARAM.2180
      IF(IBLTYP(1).EQ.6)PRINT1062                                                PARAM.2181
      IF(IFOGMD(1).EQ.1)PRINT1066                                                PARAM.2182
      IF(IBLTYP(1).EQ.6.AND.IFOGMD(1).NE.1)PRINT1064                             PARAM.2183
      IF(IICE.EQ.1.AND.IBLTYP(1).EQ.6)THEN                                       PARAM.2184
        PRINT1065                                                                PARAM.2185
      ENDIF                                                                      PARAM.2187
      IF(IBLTYP(1).NE.6.AND.IFOGMD(1).EQ.1)THEN                                  PARAM.2188
        PRINT1067                                                                PARAM.2189
        STOP                                                                     PARAM.2190
      ENDIF                                                                      PARAM.2191
      IF(IBLTYP(1).EQ.4)PRINT1068,IBLTYP(1)                                      PARAM.2192
      IF(IBLTYP(1).EQ.5)PRINT1063,IBLTYP(1)                                      PARAM.2193
      IF(IBLTYP(1).EQ.7)PRINT1069,IBLTYP(1)                                      07NOV00.285
      IF(IMVDIF.EQ.1)PRINT1132                                                   PARAM.2194
      IF(IVMIXM(1).EQ.0.AND.IBLTYP(1).EQ.2)PRINT1130                             PARAM.2195
      IF(IBLTYP(1).EQ.1.AND.ICDCON(1).EQ.1)PRINT1120,CD,CDSEA,CH,CHSEA           PARAM.2196
      IF(ITGFLG(1).EQ.1)PRINT1070,ITGFLG(1),JULDAY,GMT                           PARAM.2197
      IF(ITGFLG(1).EQ.3)PRINT1090,ITGFLG(1)                                      PARAM.2198
      IF(ISOIL.EQ.1)PRINT1071,ISOIL                                              PARAM.2199
      IF(ISOIL.EQ.2)PRINT1072,ISOIL                                              PARAM.2200
      IF(ISOIL.EQ.3)PRINT1073,ISOIL                                              07NOV00.286
      IF(ISFFLX(1).EQ.0)PRINT1100,ISFFLX(1)                                      PARAM.2201
      IF(ISFFLX(1).EQ.1)PRINT1110,ISFFLX(1)                                      PARAM.2202
      IF(ISFFLX(1).NE.1.AND.ITGFLG(1).NE.1)GOTO 240                              PARAM.2203
      IF(ISFPAR(1).EQ.0)PRINT1160                                                PARAM.2204
      IF(ISFPAR(1).EQ.1)PRINT1170                                                PARAM.2205
      IF(IEXSI.EQ.1)PRINT3101,IEXSI                                              19DEC02.500
      IF(IEXSI.EQ.2)PRINT3102,IEXSI                                              19DEC02.501
  240 CONTINUE                                                                   PARAM.2207
      IF(ISSTVAR.EQ.1)PRINT 1165                                                 07NOV00.287
      IF(ISSTVAR.EQ.0)PRINT 1166                                                 07NOV00.288
      IF(IBLTYP(1).EQ.2.OR.IBLTYP(1).EQ.5)THEN                                   05DEC01.187
        IF(IZ0TOPT.EQ.0)PRINT 1161                                               05DEC01.188
        IF(IZ0TOPT.EQ.1)PRINT 1162                                               05DEC01.189
        IF(IZ0TOPT.EQ.2)PRINT 1163                                               05DEC01.190
      ENDIF                                                                      05DEC01.191
      IF(IMOIAV(1).EQ.1)PRINT1168,IMOIAV(1)                                      07NOV00.289
      IF(IMOIAV(1).EQ.2)PRINT1169,IMOIAV(1)                                      07NOV00.290
      IF(IFSNOW(1).EQ.1)PRINT1171                                                PARAM.2208
      IF(IFSNOW(1).EQ.2)PRINT1175                                                05DEC01.192
      IF(IFRAD.EQ.0)PRINT1173,IFRAD                                              PARAM.2209
      IF(IFRAD.EQ.1)PRINT1172,IFRAD                                              PARAM.2210
      IF(IFRAD.EQ.2)PRINT1174,IFRAD                                              PARAM.2211
      IF(IFRAD.EQ.3)PRINT1176,IFRAD                                              PARAM.2212
      IF(IFRAD.EQ.4)PRINT1177,IFRAD                                              25JAN00.92
      IF(ICLOUD(1).EQ.0)PRINT1140                                                PARAM.2213
      IF(ICLOUD(1).EQ.1)PRINT1150                                                PARAM.2214
      IF(ICLOUD(1).EQ.2.AND.IFRAD.EQ.3)PRINT1152                                 05DEC01.193
      PRINT1151,NTRAD(1)                                                         PARAM.2215
C                                                                                PARAM.2216
      IF(ITPDIF.EQ.1)PRINT1133,ITPDIF                                            PARAM.2217
      IF(IVQADV.EQ.1)PRINT1131,IVQADV                                            PARAM.2218
      IF(IVTADV.EQ.1)PRINT1137,IVTADV                                            PARAM.2219
      IF(ITHADV.EQ.1)PRINT1138,ITHADV                                            PARAM.2220
      IF(ICOR3D.EQ.1)PRINT1134,ICOR3D                                            PARAM.2221
      IF(IFUPR.EQ.1)PRINT1135,IFUPR                                              PARAM.2222
C     IF (IBMOIST .EQ. 1) PRINT 1136, IBMOIST                                    PARAM.2223
      IF(IBOUDY(1).EQ.0)PRINT1180,IBOUDY(1)                                      PARAM.2224
      IF(IBOUDY(1).EQ.1)PRINT1182,IBOUDY(1),FNUDGE,GNUDGE                        PARAM.2225
      IF(IBOUDY(1).EQ.2)PRINT1184,IBOUDY(1)                                      PARAM.2226
      IF(IBOUDY(1).EQ.3.AND.INHYD.EQ.0)PRINT1190,IBOUDY(1)                       PARAM.2227
      IF(IBOUDY(1).EQ.3.AND.INHYD.EQ.1)PRINT1195,IBOUDY(1)                       PARAM.2228
      IF(IBOUDY(1).EQ.4)PRINT1200,IBOUDY(1)                                      PARAM.2229
      IF(IFDDA.EQ.1)THEN                                                         PARAM.2230
        PRINT9100,IFDDA                                                          PARAM.2231
        IF(IFDDAG.EQ.1)PRINT9110                                                 PARAM.2232
        IF(IFDDAO.EQ.1)PRINT9120                                                 PARAM.2233
      ENDIF                                                                      PARAM.2234
      PRINT9000                                                                  PARAM.2235
      PRINT1210                                                                  PARAM.2236
C                                                                                PARAM.2237
      DO 250 K=1,KL                                                              PARAM.2238
  250 PRINT1220,K,SIGMA(K),A(K),DSIGMA(K),TWT(K,1),TWT(K,2),QCON(K)              PARAM.2239
      PRINT1230,KLP1,SIGMA(KLP1)                                                 PARAM.2240
      PRINT1240,TIMAX                                                            PARAM.2241
      PRINT1250,DT                                                               PARAM.2242
      PRINT1260,DX                                                               PARAM.2243
      PRINT1270,JL,IL                                                            PARAM.2244
      PRINT1280,KL                                                               PARAM.2245
      PRINT1290,XKHZ                                                             PARAM.2246
      PRINT1300,XKHMAX                                                           PARAM.2247
C                                                                                PARAM.2248
 9000 FORMAT (//)                                                                PARAM.2249
 9200 FORMAT(1X,'MIX = ',I4,'   MJX = ',I4,'   MKX = ',I3,                       PARAM.2250
     1 '   MLX = ',I3,',   MAXNES = ',I3)                                        07NOV00.291
 9210 FORMAT(1X,'INHYD  = ',I2,'  IKFFC  = ',I2,'  IARASC = ',I2,                PARAM.2252
     1 '  IEXMS  = ',I2,'  IICE   = ',I2,'  IICEG  = ',I2,/,                     PARAM.2253
     2 ' IRDDIM = ',I2,'  ISLDIM = ',I2,'  IFDDAG = ',I2,                        PARAM.2254
     3 '  IFDDAO = ',I2)                                                         PARAM.2255
 1000 FORMAT(' IDRY   = ',I3,'  THIS IS A DRY CASE.')                            PARAM.2256
 1001 FORMAT(' IMPHYS = ',I3,'  THIS IS A DRY CASE WITH PASSIVE ',               07NOV00.292
     1 'MOISTURE VARIABLES.')                                                    PARAM.2258
 1010 FORMAT(12X,'IMOIST = ',I3)                                                 PARAM.2259
 1002 FORMAT(' IMPHYS = ',I3,'  THIS IS A MOIST CASE WITH STABLE '               PARAM.2260
     1,'(OR NONCONVECTIVE) PRECIP SCHEME.')                                      PARAM.2261
 1020 FORMAT(12X,'IMOIST = ',I3)                                                 PARAM.2262
 1021 FORMAT(' IMPHYS = ',I3,'  WARM RAIN SCHEME (HSIE).')                       PARAM.2263
 1022 FORMAT(' IMPHYS = ',I3,'  SIMPLE ICE SCHEME (DUDHIA).')                    PARAM.2264
 1023 FORMAT(' IMPHYS = ',I3,'  MIXED PHASE SCHEME (REISNER).')                  PARAM.2265
 1024 FORMAT(' IMPHYS = ',I3,'  GRAUPEL INCLUDED (GODDARD).')                    PARAM.2266
 1025 FORMAT(' IMPHYS = ',I3,'  GRAUPEL INCLUDED (REISNER).')                    PARAM.2267
 1026 FORMAT(12X,'MPHYSTBL = ',I3,'  LOOK-UP TABLE USED IF IMPHYS=',             PARAM.2268
     1 '3, 4 OR 5.')                                                             PARAM.2269
 1027 FORMAT(' IMPHYS = ',I3,'  GRAUPEL INCLUDED (SCHULTZ).')                    PARAM.2270
 1029 FORMAT(12X,'IFDRY  = ',I3,'  LATENT HEATING NOT CONSIDERED')               PARAM.2271
 1041 FORMAT(12X,'IEVAP  = ',I3,'  EVAPORATION EFFECT OF RAINWATER ',            07NOV00.293
     1 'NOT CONSIDERED IN THIS EXPLICIT SCHEME.')                                PARAM.2273
 1042 FORMAT(12X,'IEVAP  = ',I3,'  EVAPORATION EFFECT OF BOTH RAIN',             PARAM.2274
     1 'WATER AND CLOUD WATER NOT CONSIDERED IN THIS EXPLICIT SCHEME.')          PARAM.2275
 1031 FORMAT(/' ICUPA  = ',I3,'  NO CONVECTIVE PARAMETERIZATION')                PARAM.2276
 1032 FORMAT(/' ICUPA  = ',I3,'  ANTHES KUO CONVECTIVE ',                        PARAM.2277
     1 'PARAMETERIZATION IS USED')                                               PARAM.2278
 1033 FORMAT(/' ICUPA  = ',I3,'  GRELL CONVECTIVE PARAMETERIZATION ',            PARAM.2279
     1 'IS USED')                                                                PARAM.2280
 1034 FORMAT(/' ICUPA  = ',I3,'  MODIFIED ARAKAWA SCUBERT ',                     PARAM.2281
     1 'CONVECTIVE PARAMETERIZATION  IS USED')                                   PARAM.2282
 1035 FORMAT(/' ICUPA  = ',I3,'  FRITSCH/CHAPPEL CONVECTIVE ',                   PARAM.2283
     1 'PARAMETERIZATION SCHEME.')                                               PARAM.2284
 1036 FORMAT(/' ICUPA  = ',I3,'  KAIN/FRITSCH CONVECTIVE ',                      PARAM.2285
     1 'PARAMETERIZATION SCHEME.')                                               PARAM.2286
 1037 FORMAT(/' ICUPA  = ',I3,'  BETTS/MILLER CONVECTIVE ',                      PARAM.2287
     1 'PARAMETERIZATION SCHEME.')                                               PARAM.2288
 1049 FORMAT(/' ICUPA  = ',I3,'  KAIN/FRITSCH 2 CONVECTIVE ',                    05DEC01.194
     1 'PARAM SCHEME WITH SHALLOW CONVECTION OPTION.')                           05DEC01.195
 1038 FORMAT(12X,'SHALLOW CONVECTION INCLUDED FOR DOMAIN 1.')                    PARAM.2289
 1039 FORMAT(12X,'STABILITY CHECK IN ANTHES-KUO SCHEME IS TURNED OFF.')          PARAM.2290
 1040 FORMAT(/' IBLTYP = ',I3,'  FRICTIONLESS AND INSULATED FOR THE ',           07NOV00.294
     1 'LOWER BOUNDARY.')                                                        PARAM.2292
 1050 FORMAT(/' IBLTYP = ',I3,'  BULK PBL IS USED.')                             PARAM.2293
 1060 FORMAT(/' IBLTYP = ',I3,'  BLACKADAR PBL IS USED.')                        PARAM.2294
 1061 FORMAT(/' IBLTYP = ',I3,'  BURK-THOMPSON PBL PARAMETERIZATION ',           PARAM.2295
     1 'IS USED.')                                                               PARAM.2296
 1062 FORMAT(/'   GAYNO-SEAMAN PBL IS USED.')                                    PARAM.2297
 1066 FORMAT(/'    FOG MODEL IS USED.')                                          PARAM.2298
 1064 FORMAT(/'    FOG MODEL IS NOT USED.')                                      PARAM.2299
 1065 FORMAT(/' WARNING...RUNNING G-S PBL ICE. PBL WILL IGNORE ICE')             07NOV00.295
 1067 FORMAT(/' STOP...IFOGMD CAN ONLY BE USED WITH G-S PBL,',                   07NOV00.296
     & ' PLEASE RESET IFOGMD TO 0')                                              PARAM.2302
 1068 FORMAT(/' IBLTYP = ',I3,'  ETA-MELLOR-YAMADA PBL IS USED.')                PARAM.2303
 1063 FORMAT(/' IBLTYP = ',I3,'  MRF PBL IS USED.')                              PARAM.2304
 1069 FORMAT(/' IBLTYP = ',I3,'  PLEIM-XIU PBL IS USED.')                        07NOV00.297
 1070 FORMAT(/' ITGFLG = ',I3,'  THE SURFACE ENERGY BUDGET IS USED TO ',         PARAM.2305
     1 'CALCULATE THE GROUND TEMPERATURE.',/,                                    PARAM.2306
     2 12X,'JULDAY = ',I3,'  GMT = ',F4.1)                                       PARAM.2307
 1090 FORMAT(/' ITGFLG = ',I3,'  THE GROUND TEMPERATURE DOES NOT VARY ',         PARAM.2308
     1 'IN TIME.')                                                               PARAM.2309
 1071 FORMAT(' ISOIL  = ',I3, '  MULTI-LAYER SOIL THERMAL DIFFUSION .')          PARAM.2310
 1072 FORMAT(' ISOIL  = ',I3, '  NOAH LAND-SURFACE MODEL.')                      19DEC02.502
 1073 FORMAT(' ISOIL  = ',I3, '  PLEIM-XIU LAND-SURFACE MODEL.')                 07NOV00.298
 1100 FORMAT(' ISFFLX = ',I3, '  HEAT AND MOISTURE FLUXES FROM THE ',            07NOV00.299
     1 'GROUND ARE TURNED OFF.')                                                 PARAM.2313
 1110 FORMAT(' ISFFLX = ',I3, '  HEAT AND MOISTURE FLUXES FROM THE ',            PARAM.2314
     1 'GROUND ARE CONSIDERED.')                                                 PARAM.2315
 1120 FORMAT(12X,'DRAG COEFFICIENTS ARE CONSTANTS, CD = ',F5.4,                  PARAM.2316
     1 ',   CDSEA = ',F5.4,',   CT = ',F5.4,',   CTSEA = ',F5.4)                 PARAM.2317
 1130 FORMAT(12X,'VERTICAL MIXING OF MOMENTUM IS TURNED OFF.')                   PARAM.2318
 1131 FORMAT(' IVQADV = ', I3, '  VERTICAL MOISTURE ADVECTION USES ',            PARAM.2319
     1 'LINEAR INTERPOLATION.')                                                  PARAM.2320
 1137 FORMAT(' IVTADV = ', I3, '  VERTICAL TEMPERATURE ADVECTION USES ',         PARAM.2321
     1 'LINEAR INTERPOLATION.')                                                  PARAM.2322
 1138 FORMAT(' ITHADV = ', I3, '  POTENTIAL TEMPERATURE ADVECTION USED')         PARAM.2323
 1132 FORMAT(12X,'VERTICAL MIXING MOIST ADIABATIC IN CLOUDS.')                   PARAM.2324
 1133 FORMAT(/' ITPDIF = ',I3, '  HORIZONTAL DIFFUSION OF PERTURBATION',         PARAM.2325
     1 ' TEMPERATURE.')                                                          PARAM.2326
 1134 FORMAT(' ICOR3D = ', I3, '  FULL 3D CORIOLIS FORCE.')                      PARAM.2327
 1135 FORMAT(' IFUPR  = ', I3, '  UPPER RADIATIVE BOUNDARY CONDITION.')          PARAM.2328
 1136 FORMAT(' IBMOIST = ',I3, '  ALL MOISTURE FIELDS ARE IN INITIAL ',          PARAM.2329
     1 ' AND BOUNDARY CONDITIONS.')                                              PARAM.2330
 1140 FORMAT(12X,'THE RADIATION EFFECTS DUE TO CLOUDS ARE NOT ',                 PARAM.2331
     1 'CONSIDERED.')                                                            PARAM.2332
 1150 FORMAT(12X,'THE RADIATION EFFECTS DUE TO CLOUDS ARE CONSIDERED.')          PARAM.2333
 1152 FORMAT(12X,'THE CLOUD EFFECT IN CCM2 IS CONSIDERED THROUGH RH.')           05DEC01.196
 1151 FORMAT(12X,'THE RADIATION IS COMPUTED EVERY ',I4,' STEPS.')                PARAM.2334
 1160 FORMAT(12X,'SURFACE PARAMETERS ARE CONSTANT OVER THE WHOLE ',              PARAM.2335
     1 'DOMAIN.')                                                                PARAM.2336
 1170 FORMAT(12X,'SURFACE PARAMETERS ARE VARIABLE.')                             PARAM.2337
 1161 FORMAT(12X,'VISCOUS SUB-LAYER FOR MOISTURE: CARLSON-BOLAND')               05DEC01.197
 1162 FORMAT(12X,'THERMAL ROUGHNESS LENGTH FROM GARRATT FORMULA')                05DEC01.198
 1163 FORMAT(12X,'THERMAL ROUGHNESS LENGTH FROM ZILITINKEVICH FORMULA')          05DEC01.199
 1165 FORMAT(12X,'SST WILL BE UPDATED DURING MODEL INTEGRATION')                 07NOV00.300
 1166 FORMAT(12X,'SST WILL NOT BE UPDATED DURING MODEL INTEGRATION')             07NOV00.301
 1168 FORMAT(' IMOIAV = ',I3, '  MOISTURE AVAIL WILL VARY IN TIME ',             07NOV00.302
     1 'USING BUCKET MODEL,',/,15X,'USING TABLE VALUE TO START.')                07NOV00.303
 1169 FORMAT(' IMOIAV = ',I3, '  MOISTURE AVAIL WILL VARY IN TIME ',             07NOV00.304
     1 'USING BUCKET MODEL,',/,15X,                                              07NOV00.305
     2 'USING SOIL-MOISTURE CONVERTED VALUE TO START.')                          07NOV00.306
 1171 FORMAT(12X,'IFSNOW=1: SNOW EFFECT IS CONSIDERED.')                         05DEC01.200
 1175 FORMAT(12X,'IFSNOW=2: SNOW COVER AMOUNT IS PREDICTED.')                    05DEC01.201
 1172 FORMAT(/' IFRAD  = ',I3,'  SIMPLE RADIATIVE COOLING.')                     PARAM.2340
 1173 FORMAT(/' IFRAD  = ',I3,'  NO ATMOSPHERIC RADIATIVE EFFECT.')              PARAM.2341
 1174 FORMAT(/' IFRAD  = ',I3,'  LONGWAVE AND SHORTWAVE SCHEMES ',               PARAM.2342
     1 '(DUDHIA, 1989).')                                                        PARAM.2343
 1176 FORMAT(/' IFRAD  = ',I3,'  CCM2 LONGWAVE AND SHORTWAVE SCHEMES.')          PARAM.2344
 1177 FORMAT(/' IFRAD  = ',I3,'  RRTM LONGWAVE AND DUDHIA SHORTWAVE.')           25JAN00.93
 1180 FORMAT(/' IBOUDY = ',I3,'  THE LATERAL BOUNDARY CONDITIONS ARE ',          PARAM.2345
     1 'FIXED.')                                                                 PARAM.2346
 1182 FORMAT(/' IBOUDY = ',I3,'  RELAXATION BOUNDARY CONDITIONS ARE ',           PARAM.2347
     1 'USED. FNUDGE = ',E15.5,' GNUDGE = ',E15.5)                               PARAM.2348
 1184 FORMAT(/' IBOUDY = ',I3,'  TIME DEPENDENT BOUNDARY CONDITIONS ',           PARAM.2349
     1 'ARE USED.')                                                              PARAM.2350
 1190 FORMAT(/' IBOUDY = ',I3,'  INFLOW/OUTFLOW BOUNDARY CONDITIONS ',           PARAM.2351
     1 'ARE USED.')                                                              PARAM.2352
 1195 FORMAT(/' IBOUDY = ',I3,'  RELAXATION I/O BOUNDARY CONDITIONS ',           PARAM.2353
     1 'ARE USED.')                                                              PARAM.2354
 1200 FORMAT(/' IBOUDY = ',I3,'  SPONGE BOUNDARY CONDITIONS ARE USED.')          PARAM.2355
 9100 FORMAT(/' IFDDA  = ',I3,'  FDDA IS COMPILED.')                             PARAM.2356
 9110 FORMAT(12X,'ANALYSIS NUDGING IS USED.')                                    PARAM.2357
 9120 FORMAT(12X,'OBSERVATION NUDGING IS USED.')                                 PARAM.2358
 1210 FORMAT('0 K',4X,'SIGMA(K)',4X,' A(K)',5X,'DSIGMA(K)',4X,'TWT(K,1)'         PARAM.2359
     1,5X,'TWT(K,2)',5X,'QCON(K)'/)                                              PARAM.2360
 1220 FORMAT (1X,I2,5X,F6.4,5X,F6.4,5X,F6.4,5X,F8.4,5X,F8.4,5X,F8.4)             PARAM.2361
 1230 FORMAT (1X,I2,5X,F6.4)                                                     PARAM.2362
 1240 FORMAT(//' MAXIMUM TIME      = ',F7.0,' MINUTES')                          25JAN00.94
 1250 FORMAT(' TIME STEP         = ',F7.2,' SECONDS')                            PARAM.2364
 1260 FORMAT(' COARSE MESH DX    = ',F7.0,' METERS')                             PARAM.2365
 1270 FORMAT(' GRID POINTS (X,Y) = (',I3,',',I3,')')                             PARAM.2366
 1280 FORMAT(' NUMBER OF LEVELS  = ',I2,/)                                       PARAM.2367
 1290 FORMAT(' CONSTANT HOR. DIFF. COEF. = ',E12.5,' M*M/S')                     PARAM.2368
 1300 FORMAT(' MAXIMUM  HOR. DIFF. COEF. = ',E12.5,' M*M/S')                     PARAM.2369
 3101 FORMAT(' IEXSI  = ',I3,' SEA ICE BASED ON TG')                             19DEC02.503
 3102 FORMAT(' IEXSI  = ',I3,' SEA ICE FROM INPUT FILE')                         19DEC02.504
C                                                                                PARAM.2370
      RETURN                                                                     PARAM.2371
#ifndef MPP1                                                                     25JAN00.95
9001  PRINT *, 'ERROR IN READING HEADER. CHECK INPUT FILE UNIT 11'               PARAM.2372
      STOP '9001 IN PARAM'                                                       PARAM.2373
9002  PRINT *, 'INPUT FILE EMPTY. CHECK IF INPUT FILE UNIT 11 EXISTS'            PARAM.2374
      STOP '9002 IN PARAM'                                                       PARAM.2375
9003  PRINT *, 'ERROR OPENING MMINPUT_DOMAIN1'                                   25JAN00.96
      STOP '9003 IN PARAM'                                                       25JAN00.97
9004  PRINT *, 'ERROR OPENING BUCKET MODEL DATA FILE FROM UNIT 18'               23DEC04.193
      STOP '9004 IN PARAM'                                                       07NOV00.308
9005  PRINT *, 'ERROR OPENING NAMELIST FILE mmlif'                               23DEC04.194
      STOP '9005 IN PARAM'                                                       07NOV00.310
9006  PRINT *, 'ERROR OPENING LANDUSE.TBL FILE FROM UNIT 19'                     23DEC04.195
      STOP '9006 IN PARAM'                                                       07NOV00.312
9007  PRINT *, 'ERROR OPENING VEGPARM.TBL FILE FROM UNIT 19'                     23DEC04.196
      STOP '9007 IN PARAM'                                                       23DEC04.197
9008  PRINT *, 'ERROR OPENING SOILPARM.TBL FILE FROM UNIT 19'                    23DEC04.198
      STOP '9008 IN PARAM'                                                       23DEC04.199
9009  PRINT *, 'ERROR OPENING GENPARM.TBL FILE FROM UNIT 19'                     23DEC04.200
      STOP '9009 IN PARAM'                                                       23DEC04.201
#else                                                                            25JAN00.98
C PARAM IS NOT PASSED THROUGH FLIC SO STOP STATEMENTS CONVERTED MANUALLY         25JAN00.99
9001  PRINT *, 'ERROR IN READING HEADER. CHECK INPUT FILE UNIT 11'               25JAN00.100
      CALL FKILL_MODEL( '9001 IN PARAM' )                                        25JAN00.101
9002  PRINT *, 'INPUT FILE EMPTY. CHECK IF INPUT FILE UNIT 11 EXISTS'            25JAN00.102
      CALL FKILL_MODEL( '9002 IN PARAM' )                                        25JAN00.103
9003  PRINT *, 'ERROR OPENING MMINPUT_DOMAIN1'                                   25JAN00.104
      CALL FKILL_MODEL( '9003 IN PARAM' )                                        25JAN00.105
9004  PRINT *, 'ERROR OPENING BUCKET MODEL DATA FILE FROM UNIT 18'               23DEC04.202
      CALL FKILL_MODEL( '9004 IN PARAM' )                                        07NOV00.314
9005  PRINT *, 'ERROR OPENING NAMELIST FILE mmlif'                               07NOV00.315
      CALL FKILL_MODEL( '9005 IN PARAM' )                                        07NOV00.316
9006  PRINT *, 'ERROR OPENING LANDUSE.TBL FILE FROM UNIT 19'                     23DEC04.203
      CALL FKILL_MODEL( '9006 IN PARAM' )                                        07NOV00.318
9007  PRINT *, 'ERROR OPENING VEGPARM.TBL FILE FROM UNIT 19'                     23DEC04.204
      CALL FKILL_MODEL( '9007 IN PARAM' )                                        23DEC04.205
9008  PRINT *, 'ERROR OPENING SOILPARM.TBL FILE FROM UNIT 19'                    23DEC04.206
      CALL FKILL_MODEL( '9008 IN PARAM' )                                        23DEC04.207
9009  PRINT *, 'ERROR OPENING GENPARM.TBL FILE FROM UNIT 19'                     23DEC04.208
      CALL FKILL_MODEL( '9009 IN PARAM' )                                        23DEC04.209
#endif                                                                           25JAN00.106
      END                                                                        PARAM.2376
                                                                                 19DEC02.505
