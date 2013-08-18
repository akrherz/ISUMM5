C                                                                                GETDAT.1
C-----------------------------------------------------------------------         GETDAT.2
                                                                                 GETDAT.3
      SUBROUTINE GETDAT(CLDFRA, O3PROF, GRAVX ,CPAIRX ,EPSILOX,STEBOLX,          GETDAT.4
     $                  TS     ,     TG,                                         GETDAT.5
     $                  PSURF ,PMID   ,PINT   ,PMLN   ,   PILN,                  GETDAT.6
     $                  T      ,H2OMMR ,PLOL   ,PLOS   , CLDFRC,                 GETDAT.7
     $                  CLWP ,CIWP, CONWP, EFFCLD, JSLC, ILX, DX,                05DEC01.1338
     $                  MCPHYS,T3D,QV3D,PP3D,QC3D,QR3D,QI3D,QNI3D,               05DEC01.1339
     $                  ICLOUDE)                                                 05DEC01.1340
C                                                                                GETDAT.10
C INTERFACE ROUTINE FOR COLUMN MODEL THAT BOTH INITIALIZES                       GETDAT.11
C CERTAIN CONSTANTS AND READS EXTERNAL DATA:                                     GETDAT.12
C                                                                                GETDAT.13
C O3 MASS MIXING RATIOS ARE READ IN, BUT THE MODEL ALSO REQUIRES THE             GETDAT.14
C PATH LENGTHS; THEY ARE COMPUTED HERE                                           GETDAT.15
C                                                                                GETDAT.16
C ALSO, FROM THE CLOUD INPUT (FRACTION AND LIQUID WATER PATH), THE               GETDAT.17
C CLOUD LONGWAVE EMISSIVITY MUST BE COMPUTED; THIS IS DONE HERE                  GETDAT.18
C                                                                                GETDAT.19
C                                                                                GETDAT.20
#     include <parame.incl>                                                      GETDAT.21
#     include <param3.incl>                                                      GETDAT.22
#     include <point2d.incl>                                                     GETDAT.23
#     include <pmoist.incl>                                                      GETDAT.24
#     include <parccm2.incl>                                                     GETDAT.25
#     include <radccm2.incl>                                                     GETDAT.26
#     include <nonhyd.incl>                                                      GETDAT.27
C     INCLUDE 'PARAME'                                                           GETDAT.28
C     INCLUDE 'PARAM3.CB'                                                        GETDAT.29
C     INCLUDE 'RADCCM2.DATA'                                                     GETDAT.30
C     INCLUDE 'MAIN.CB'                                                          GETDAT.31
C     INCLUDE 'PMOIST.CB'                                                        GETDAT.32
C     INCLUDE 'RAD.CB'                                                           GETDAT.33
C     INCLUDE 'BATS.CB2'                                                         GETDAT.34
C-----------------------------------------------------------------------         GETDAT.35
      PARAMETER (IXM1=MIX-1)                                                     GETDAT.36
C    cmr - cloud water mixing ratio (g/kg)                                       05DEC01.1341
C   cimr - cloud ice mixing ratio (g/kg)                                         05DEC01.1342
      REAL CMR(IPLOND,IPLEVP),CIMR(IPLOND,IPLEVP)                                05DEC01.1343
      DIMENSION SCLPR(KX)                                                        GETDAT.37
      DIMENSION ZI(IXM1,KXP1),DELTAZ(IXM1,KX)                                    GETDAT.38
      DIMENSION T3D(MIX,MJX,MKX),QV3D(MIX,MJX,MKX),PP3D(MIX,MJX,MKX)             GETDAT.39
      REAL QC3D,QR3D,QI3D,QNI3D                                                  05DEC01.1344
      DIMENSION QC3D(MIXM,MJXM,MKXM),QR3D(MIXM,MJXM,MKXM)                        05DEC01.1345
      DIMENSION QI3D(MIXIC,MJXIC,MKXIC),QNI3D(MIXIC,MJXIC,MKXIC)                 05DEC01.1346
      INTEGER MCPHYS                                                             05DEC01.1347
C INPUT ARGUMENTS                                                                GETDAT.40
C                                                                                GETDAT.41
      REAL CLDFRA(IPLOND,IPLEVP) ! FRACTIONAL CONVECTIVE CLOUD COVER             GETDAT.42
      REAL O3PROF(IPLOND,IPLEVP) ! OZONE PROFILE                                 GETDAT.43
C                                                                                GETDAT.44
C OUTPUT ARGUMENTS                                                               GETDAT.45
C                                                                                GETDAT.46
      REAL GRAVX,       ! GRAVITATIONAL ACCELERATION (M/S**2)                    GETDAT.47
     $     CPAIRX,      ! HEAT CAPACITY DRY AIR AT CONSTANT PRS (J/KG/K)         GETDAT.48
     $     EPSILOX,     ! RATIO MEAN MOL WEIGHT H2O TO DRY AIR                   GETDAT.49
     $     STEBOLX      ! STEFAN-BOLTZMANN CONSTANT (W/M**2/K**4)                GETDAT.50
C                                                                                GETDAT.51
      REAL TS(IPLOND),          ! SURFACE (AIR)  TEMPERATURE                     GETDAT.52
     $     TG(IPLOND),          ! SURFACE (SKIN) TEMPERATURE                     GETDAT.53
     $     PSURF(IPLOND),          ! MODEL SURFACE PRESSURE FIELD                GETDAT.54
     $     PMID(IPLOND,IPLEV),   ! PRESSURE AT MODEL MID-LEVELS                  GETDAT.55
     $     PINT(IPLOND,IPLEVP),  ! PRESSURE AT MODEL INTERFACES                  GETDAT.56
     $     PMLN(IPLOND,IPLEV),   ! LN(PMID)                                      GETDAT.57
     $     PILN(IPLOND,IPLEVP),  ! LN(PINT)                                      GETDAT.58
     $     T(IPLOND,IPLEV),      ! ATMOSPHERIC TEMPERATURE                       GETDAT.59
     $     H2OMMR(IPLOND,IPLEV), ! MOISTURE FIELD                                GETDAT.60
     $     PLOL(IPLOND,IPLEVP),  ! O3 PRESSURE WEIGHTED PATH LENGTH              GETDAT.61
     $     PLOS(IPLOND,IPLEVP),  ! O3 PATH LENGTH                                GETDAT.62
     $     CLDFRC(IPLOND,IPLEVP),! CLOUD FRACTION                                GETDAT.63
     $     CLWP(IPLOND,IPLEV),   ! CLOUD LIQUID WATER PATH (G/M**2)              GETDAT.64
     $     CIWP(IPLOND,IPLEV),   ! CLOUD ICE WATER PATH (G/M**2)                 05DEC01.1348
     $     CONWP(IPLOND,IPLEV),  ! CONVECTIVE CLOUD WATER PATH (G/M**2)          05DEC01.1349
     $     EFFCLD(IPLOND,IPLEVP) ! EFFECTIVE CLOUD FRACTION                      GETDAT.65
C                                                                                GETDAT.66
C LOCAL WORKSPACE                                                                GETDAT.67
C                                                                                GETDAT.68
      REAL DAYYR(IPLOND),       ! DAY OF YEAR                                    GETDAT.69
     $     O3MMR(IPLOND,IPLEV),  ! O3 MASS MIXING RATIO                          GETDAT.70
     $     EMIS(IPLOND,IPLEV),   ! CLOUD EMISSIVITY FOR LONGWAVE                 GETDAT.71
     $     PTOPR,               ! TOP LAYER INTERFACE PRESSURE                   GETDAT.72
     $     PBOTR                ! BOTTOM LAYER INTERFACE PRESSURE                GETDAT.73
C                                                                                GETDAT.74
      INTEGER LEV(IPLEV),       ! LEVEL INPUT                                    GETDAT.75
     $                I,       ! LONGITUDE INDEX                                 GETDAT.76
     $                K        ! LEVEL  INDEX                                    GETDAT.77
C                                                                                GETDAT.78
      CHARACTER*80 LABEL                                                         GETDAT.79
C                                                                                GETDAT.80
      REAL     V0,  ! VOLUME OF A GAS AT STP (CM**3/MOL)                         GETDAT.81
     $         P0,  ! STANDARD PRESSURE (DYNES/CM**2)                            GETDAT.82
     $        AMD,  ! EFFECTIVE MOLECULAR WEIGHT OF DRY AIR (G/MOL)              GETDAT.83
     $        AMO,  ! MOLECULAR WEIGHT OF OZONE (G/MOL)                          GETDAT.84
     $        CPL,  ! CONSTANT IN OZONE PATH LENGTH TO MIXING RATIO              GETDAT.85
     $      CPWPL,  ! PRESSURE WEIGHTED OZONE PATH LENGTH CONSTANT               GETDAT.86
     $       VMMR   ! OZONE VOLUME MIXING RATIO                                  GETDAT.87
C                                                                                GETDAT.88
      DATA V0    /  22413.6   /                                                  GETDAT.89
      DATA P0    /  1.01325E6 /                                                  GETDAT.90
      DATA AMD   /  28.9644   /                                                  GETDAT.91
      DATA AMO   /  48.0000   /                                                  GETDAT.92
      DATA CLWLO,CLWHI/0.15E-6,0.015E-6/                                         GETDAT.93
      DATA CLW0ST,CLWMIN,CLWMAX,T0ST,T0MIN,T0MAX                                 GETDAT.94
     1         /0.2E-6,0.03E-6,0.4E-6,265.,220.,295./                            GETDAT.95
      DATA RHLARG,RHSMAL,DLARGS,DSMALS /0.75,0.90,100.0E3,10.0E3/                GETDAT.96
C                                                                                GETDAT.97
C SET FUNDAMENTAL CONSTANTS (MKS):                                               GETDAT.98
C                                                                                GETDAT.99
C     SVP1    =   0.611                                                          GETDAT.100
C     SVP2    =   19.84659                                                       GETDAT.101
C     SVP3    =   5418.12                                                        GETDAT.102
C     EP2     =   0.622                                                          GETDAT.103
      NCLD=3                                                                     GETDAT.104
C     GRAVX   =   9.80616                                                        GETDAT.105
      GRAVX=G                                                                    GETDAT.106
C     CPAIRX  =   1.00464E3                                                      GETDAT.107
      CPAIRX=CP                                                                  GETDAT.108
C     EPSILOX =   0.622                                                          GETDAT.109
      EPSILOX=EP2                                                                GETDAT.110
C     STEBOLX =   5.67E-8                                                        GETDAT.111
      STEBOLX=STBOLT                                                             GETDAT.112
C     RX=287.                                                                    GETDAT.113
      RX=R                                                                       GETDAT.114
C                                                                                GETDAT.115
C   FOR LIQUID WATER CONTENT                                                     GETDAT.116
C                                                                                GETDAT.117
      ACLW0=0.18                                                                 GETDAT.118
      AHL=2000.                                                                  GETDAT.119
C                                                                                GETDAT.120
C BEGIN READ OF DATA:                                                            GETDAT.121
C-----                                                                           GETDAT.122
C-----SURFACE PRESSURE AND SCALED PRESSURE, FROM WHICH LEVEL PRESSURES           GETDAT.123
C-----ARE COMPUTED                                                               GETDAT.124
#ifndef MPP1                                                                     GETDAT.125
      DO 10 N=1,IPLOND                                                           GETDAT.126
        NLIM=MIN0(N,ILX)                                                         GETDAT.127
#else                                                                            GETDAT.128
        DO 10 N=1,ILX                                                            GETDAT.129
          NLIM=N                                                                 GETDAT.130
#endif                                                                           GETDAT.131
          PSURF(N)=(PSB(NLIM,JSLC)+PTOP)*10.+PP3D(NLIM,JSLC,IPLEV)*.01           GETDAT.133
          DO 10 NLL=1,IPLEV                                                      GETDAT.134
            PMID(N,NLL)=(PSB(NLIM,JSLC)*A(NLL)+PTOP)*10.+                        GETDAT.135
     +                  PP3D(NLIM,JSLC,NLL)*.01                                  GETDAT.136
            SCLPR(NLL)=PMID(N,NLL)/PSURF(N)                                      GETDAT.137
   10     CONTINUE                                                               GETDAT.138
                                                                                 GETDAT.139
                                                                                 GETDAT.140
C                                                                                GETDAT.141
C.......... CONVERT PRESSURES FROM MB TO PASCALS AND DEFINE                      GETDAT.142
C.......... INTERFACE PRESSURES:                                                 GETDAT.143
C                                                                                GETDAT.144
#ifndef MPP1                                                                     GETDAT.145
      DO 30 I=1,IPLOND                                                           GETDAT.146
        ILIM=MIN0(I,ILX)                                                         GETDAT.147
#else                                                                            GETDAT.148
        DO 30 I=1,ILX                                                            GETDAT.149
          ILIM=I                                                                 GETDAT.150
#endif                                                                           GETDAT.151
          PSURF(I)=PSURF(I)*100.                                                 GETDAT.153
          DO 20 K=1,IPLEV                                                        GETDAT.154
C                                                                                GETDAT.155
            PMID(I,K)=PMID(I,K)*100.                                             GETDAT.156
            PMLN(I,K)=ALOG(PMID(I,K))                                            GETDAT.157
C                                                                                GETDAT.158
   20     CONTINUE                                                               GETDAT.159
          DO 30 K=1,IPLEVP                                                       GETDAT.160
            IF(K.GT.1.AND.K.LT.IPLEVP)PPBS=TWT(K,2)*PP3D(ILIM,JSLC,K-1)+         GETDAT.161
     +        TWT(K,1)*PP3D(ILIM,JSLC,K)                                         GETDAT.162
            IF(K.EQ.1)PPBS=PP3D(ILIM,JSLC,1)                                     GETDAT.163
            IF(K.EQ.IPLEVP)PPBS=PP3D(ILIM,JSLC,IPLEV)                            GETDAT.164
            PINT(I,K)=(PSB(ILIM,JSLC)*SIGMA(K)+PTOP)*1000.+PPBS                  GETDAT.165
            PILN(I,K)=ALOG(PINT(I,K))                                            GETDAT.166
   30     CONTINUE                                                               GETDAT.167
                                                                                 GETDAT.168
                                                                                 GETDAT.169
C                                                                                GETDAT.170
C-----                                                                           GETDAT.171
C-----AIR TEMPERATURES                                                           GETDAT.172
C-----                                                                           GETDAT.173
      DO 40 NLL=1,IPLEV                                                          GETDAT.174
#ifndef MPP1                                                                     GETDAT.175
        DO 40 N=1,IPLOND                                                         GETDAT.176
          NLIM=MIN0(N,ILX)                                                       GETDAT.177
#else                                                                            GETDAT.178
          DO 40 N=1,ILX                                                          GETDAT.179
            NLIM=N                                                               GETDAT.180
#endif                                                                           GETDAT.181
            T(N,NLL)=T3D(NLIM,JSLC,NLL)                                          GETDAT.183
            IF(MCPHYS.GE.5)THEN                                                  05DEC01.1350
C CONSIDER RADIATIVE PROPERTIES OF CLOUD SIZE PARTICLES ONLY                     05DEC01.1351
              CMR(N,NLL)=(QC3D(NLIM,JSLC,NLL))                                   05DEC01.1352
     +                   *1000.                                                  05DEC01.1353
              CIMR(N,NLL)=(QI3D(NLIM,JSLC,NLL))                                  05DEC01.1354
     +                    *1000.                                                 05DEC01.1355
C CONSIDER RADIATIVE PROPERTIES OF CLOUD AND PRECIP SIZE PARTICLES               05DEC01.1356
C             CMR(N,NLL)=(QC3D(NLIM,JSLC,NLL)+QR3D(NLIM,JSLC,NLL))               05DEC01.1357
C    +                   *1000.                                                  05DEC01.1358
C             CIMR(N,NLL)=(QI3D(NLIM,JSLC,NLL)+QNI3D(NLIM,JSLC,NLL))             05DEC01.1359
C    +                    *1000.                                                 05DEC01.1360
            ELSE                                                                 05DEC01.1361
              IF(T(N,NLL).GE.273.15)THEN                                         05DEC01.1362
C CONSIDER RADIATIVE PROPERTIES OF CLOUD SIZE PARTICLES ONLY                     05DEC01.1363
                CMR(N,NLL)=(QC3D(NLIM,JSLC,NLL))                                 05DEC01.1364
     +                     *1000.                                                05DEC01.1365
C CONSIDER RADIATIVE PROPERTIES OF CLOUD AND PRECIP SIZE PARTICLES               05DEC01.1366
C               CMR(N,NLL)=(QC3D(NLIM,JSLC,NLL)+QR3D(NLIM,JSLC,NLL))             05DEC01.1367
C    +                     *1000.                                                05DEC01.1368
                CIMR(N,NLL)=0.                                                   05DEC01.1369
              ELSE                                                               05DEC01.1370
C CONSIDER RADIATIVE PROPERTIES OF CLOUD SIZE PARTICLES ONLY                     05DEC01.1371
                CIMR(N,NLL)=(QC3D(NLIM,JSLC,NLL))                                05DEC01.1372
     +                      *1000.                                               05DEC01.1373
C CONSIDER RADIATIVE PROPERTIES OF CLOUD AND PRECIP SIZE PARTICLES               05DEC01.1374
C               CIMR(N,NLL)=(QC3D(NLIM,JSLC,NLL)+QR3D(NLIM,JSLC,NLL))            05DEC01.1375
C    +                      *1000.                                               05DEC01.1376
                CMR(N,NLL)=0.                                                    05DEC01.1377
              ENDIF                                                              05DEC01.1378
            ENDIF                                                                05DEC01.1379
   40     CONTINUE                                                               GETDAT.184
                                                                                 GETDAT.185
                                                                                 GETDAT.186
C-----                                                                           GETDAT.187
C-----SURFACE AIR TEMPERATURE                                                    GETDAT.188
C-----                                                                           GETDAT.189
#ifndef MPP1                                                                     GETDAT.190
      DO 50 N=1,IPLOND                                                           GETDAT.191
        NLIM=MIN0(N,ILX)                                                         GETDAT.192
#else                                                                            GETDAT.193
        DO 50 N=1,ILX                                                            GETDAT.194
          NLIM=N                                                                 GETDAT.195
#endif                                                                           GETDAT.196
          TS(N)=T3D(NLIM,JSLC,IPLEV)                                             GETDAT.198
   50   CONTINUE                                                                 GETDAT.199
                                                                                 GETDAT.200
C-----                                                                           GETDAT.201
C-----H2O MASS MIXING RATIO                                                      GETDAT.202
C-----                                                                           GETDAT.203
      DO 60 NLL=1,IPLEV                                                          GETDAT.204
#ifndef MPP1                                                                     GETDAT.205
        DO 60 N=1,IPLOND                                                         GETDAT.206
          NLIM=MIN0(N,ILX)                                                       GETDAT.207
#else                                                                            GETDAT.208
          DO 60 N=1,ILX                                                          GETDAT.209
            NLIM=N                                                               GETDAT.210
#endif                                                                           GETDAT.211
            H2OMMR(N,NLL)=AMAX1(1.E-05,QV3D(NLIM,JSLC,NLL))                      GETDAT.213
   60     CONTINUE                                                               GETDAT.214
                                                                                 GETDAT.215
                                                                                 GETDAT.216
C-----                                                                           GETDAT.217
C-----O3 MASS MIXING RATIO                                                       GETDAT.218
C-----                                                                           GETDAT.219
      DO 70 NLL=1,IPLEV                                                          GETDAT.220
#ifndef MPP1                                                                     GETDAT.221
        DO 70 N=1,IPLOND                                                         GETDAT.222
#else                                                                            GETDAT.223
          DO 70 N=1,ILX                                                          GETDAT.224
#endif                                                                           GETDAT.225
            KJ=IPLEV+1-NLL                                                       GETDAT.226
            O3MMR(N,NLL)=O3PROF(N,KJ)                                            GETDAT.227
   70     CONTINUE                                                               GETDAT.228
                                                                                 GETDAT.229
                                                                                 GETDAT.230
C-----                                                                           GETDAT.231
C-----FRACTIONAL CLOUD COVER (DEPENDENT ON RELATIVE HUMIDITY)                    GETDAT.232
C-----                                                                           GETDAT.233
      DXTEMS=AMAX1(DX,DSMALS)                                                    GETDAT.234
      DXTEMS=AMIN1(DXTEMS,DLARGS)                                                GETDAT.235
      THREL=RHLARG+(RHSMAL-RHLARG)*((DLARGS-DXTEMS)/(DLARGS-DSMALS))             GETDAT.236
C                                                                                GETDAT.237
      DO 80 NLL=1,IPLEV                                                          GETDAT.238
#ifndef MPP1                                                                     GETDAT.239
        DO 80 N=1,IPLOND                                                         GETDAT.240
          NLIM=MIN0(N,ILX)                                                       GETDAT.241
#else                                                                            GETDAT.242
          DO 80 N=1,ILX                                                          GETDAT.243
            NLIM=N                                                               GETDAT.244
#endif                                                                           GETDAT.245
            PPS=PSB(NLIM,JSLC)*A(NLL)+PTOP+PP3D(NLIM,JSLC,NLL)*.001              GETDAT.247
            IF(T(N,NLL).GE.273.15)THEN                                           GETDAT.248
              VAPPRS=SVP1*EXP(SVP2*(T(N,NLL)-SVPT0)/(T(N,NLL)-SVP3))             GETDAT.249
            ELSE                                                                 GETDAT.250
              VAPPRS=.611*EXP(22.514-6.15E3/T(N,NLL))                            GETDAT.251
            ENDIF                                                                GETDAT.252
            QVSWAT=EP2*VAPPRS/(PPS-VAPPRS)                                       GETDAT.253
            QSBT=QVSWAT                                                          GETDAT.254
            RELHUM=QV3D(NLIM,JSLC,NLL)/QSBT                                      GETDAT.255
            THRERH=THREL                                                         GETDAT.256
            CCVTEM=(AMAX1((RELHUM-THRERH),0.)/(1.-THRERH))**2                    GETDAT.257
C   ACCOUNT FOR THICKNESS OF LAYER (SCALED TO DSIGMA=0.05)                       GETDAT.258
            CCVTEM=AMIN1(CCVTEM,0.9999999)                                       GETDAT.259
            CCVTEM=1.-(1.-CCVTEM)**(DSIGMA(NLL)/0.05)                            GETDAT.260
            CCVTEM=AMAX1(CCVTEM,0.)                                              GETDAT.261
C   IMPLEMENT HERE THE NEW FORMULA THEN MULTIPLY BY 10E6                         GETDAT.262
            IF(T(N,NLL).GT.T0MAX)CLWTEM=CLWMAX                                   GETDAT.263
            IF(T(N,NLL).GE.T0ST.AND.T(N,NLL).LE.T0MAX)CLWTEM=CLW0ST+((           GETDAT.264
     +        T(N,NLL)-T0ST)/(T0MAX-T0ST))**2*(CLWMAX-CLW0ST)                    GETDAT.265
            IF(T(N,NLL).GE.T0MIN.AND.T(N,NLL).LT.T0ST)CLWTEM=CLW0ST+(            GETDAT.266
     +        T(N,NLL)-T0ST)/(T0MIN-T0ST)*(CLWMIN-CLW0ST)                        GETDAT.267
            IF(T(N,NLL).LT.T0MIN)CLWTEM=CLWMIN                                   GETDAT.268
            CLWTEM=CLWTEM*1.E6                                                   GETDAT.269
                                                                                 05DEC01.1380
            IF(ICLOUDE.EQ.1) THEN                                                05DEC01.1381
C CALCULATE CLDFRC BASED ON CMR AND CIMR (CLDFRC=1 FOR CMR+CMR.GT.1E-4 G/KG      05DEC01.1382
              IF((CMR(N,NLL)+CIMR(N,NLL)).GT.1E-4)THEN                           05DEC01.1383
                CLDFRC(N,NLL)=0.99999                                            05DEC01.1384
              ELSE                                                               05DEC01.1385
                CLDFRC(N,NLL)=0.                                                 05DEC01.1386
              ENDIF                                                              05DEC01.1387
C Allow for convective cloud cover to contribute to total cloud cover            05DEC01.1388
              CLDFRC(N,NLL)=AMAX1(CLDFRA(NLIM,NLL)*0.9999,CLDFRC(N,NLL))         05DEC01.1389
            ELSE                                                                 05DEC01.1390
              CLDFRC(N,NLL)=AMAX1(CLDFRA(NLIM,NLL)*0.9999999,CCVTEM)             05DEC01.1391
              CLDFRC(N,NLL)=AMIN1(CLDFRC(N,NLL),0.99)                            05DEC01.1392
            ENDIF                                                                05DEC01.1393
C                                                                                GETDAT.272
C   CONVERT LIQUID WATER CONTENT INTO LIQUID WATER PATH, I.E. MULTIPLY BY        05DEC01.1394
C   DELTAZ                                                                       GETDAT.274
            DELTAZ(N,NLL)=RX*T(N,NLL)*(PINT(N,NLL+1)-PINT(N,NLL))/(GRAVX         GETDAT.275
     +                    *PMID(N,NLL))                                          GETDAT.276
                                                                                 05DEC01.1395
            IF (ICLOUDE.EQ.0) THEN                                               05DEC01.1396
C   No cloud effect on radiation                                                 05DEC01.1397
              CIWP(N,NLL)=0.                                                     05DEC01.1398
              CLWP(N,NLL)=0.                                                     05DEC01.1399
              CONWP(N,NLL)=0.                                                    05DEC01.1400
            ELSE IF (ICLOUDE.EQ.1) THEN                                          05DEC01.1401
C   Radiation interaction with cloud water/ice                                   05DEC01.1402
              CONWP(N,NLL)=CLWTEM*DELTAZ(N,NLL)*CLDFRA(NLIM,NLL)                 05DEC01.1403
              CLWP(N,NLL)=CMR(N,NLL)*(PPS*1000.)/(T(N,NLL)*287.)*                05DEC01.1404
     +                    DELTAZ(N,NLL)                                          05DEC01.1405
              CIWP(N,NLL)=CIMR(N,NLL)*(PPS*1000.)/(T(N,NLL)*287.)*               05DEC01.1406
     +                    DELTAZ(N,NLL)                                          05DEC01.1407
            ELSE IF (ICLOUDE.EQ.2) THEN                                          05DEC01.1408
C   Radiation interaction with RH                                                05DEC01.1409
              CLWP(N,NLL)=CLWTEM*DELTAZ(N,NLL)                                   05DEC01.1410
              CIWP(N,NLL)=0.                                                     05DEC01.1411
              CONWP(N,NLL)=0.                                                    05DEC01.1412
            END IF                                                               05DEC01.1413
            IF(CLDFRC(N,NLL).EQ.0.)THEN                                          05DEC01.1414
              CLWP(N,NLL)=0.                                                     05DEC01.1415
              CIWP(N,NLL)=0.                                                     05DEC01.1416
              CONWP(N,NLL)=0.                                                    05DEC01.1417
            ENDIF                                                                05DEC01.1418
   80     CONTINUE                                                               GETDAT.279
                                                                                 GETDAT.281
C                                                                                GETDAT.282
C   LIQUID WATER CONTENT AND LIQUID WATER PATH. USE CCM2 FORMULA                 GETDAT.283
C                                                                                GETDAT.284
CS      DO 85 I=1,IPLOND                                                         GETDAT.285
CS       ZI(I,IPLEV+1)=HT(I,JSLC)                                                GETDAT.286
CS      DO 85 K=IPLEV,1,-1                                                       GETDAT.287
CS       ZI(I,K)=ZI(I,K+1)+DELTAZ(I,K)                                           GETDAT.288
CS85    CONTINUE                                                                 GETDAT.289
CS      DO 87 K=1,IPLEV                                                          GETDAT.290
CS      DO 87 I=1,IPLOND                                                         GETDAT.291
CS       CLWP(I,K)=ACLW0*AHL*(EXP(-ZI(I,K+1)/AHL)-EXP(-ZI(I,K)/AHL))             GETDAT.292
CS       IF (CLDFRC(I,K).EQ.0.) CLWP(I,K)=0.                                     GETDAT.293
CS87    CONTINUE                                                                 GETDAT.294
C     SET CLOUD FRACTIONAL COVER AT TOP MODEL LEVEL = 0                          GETDAT.295
#ifndef MPP1                                                                     GETDAT.296
      DO 90 N=1,IPLON                                                            GETDAT.297
#else                                                                            GETDAT.298
        DO 90 N=1,ILX                                                            GETDAT.299
#endif                                                                           GETDAT.300
          CLDFRC(N,1)=0.                                                         GETDAT.301
          CLWP(N,1)=0.                                                           GETDAT.302
          CIWP(N,1)=0.                                                           05DEC01.1419
          CONWP(N,1)=0.                                                          05DEC01.1420
   90   CONTINUE                                                                 GETDAT.303
                                                                                 GETDAT.304
C                                                                                GETDAT.305
C     SET CLOUD FRACTIONAL COVER AT BOTTOM (NCLD) MODEL LEVELS = 0               GETDAT.306
C                                                                                GETDAT.307
      NCLDM1=NCLD-1                                                              GETDAT.308
      DO 100 NLL=IPLEV-NCLDM1,IPLEV                                              GETDAT.309
#ifndef MPP1                                                                     GETDAT.310
        DO 100 N=1,IPLON                                                         GETDAT.311
#else                                                                            GETDAT.312
          DO 100 N=1,ILX                                                         GETDAT.313
#endif                                                                           GETDAT.314
            CLDFRC(N,NLL)=0.                                                     GETDAT.315
            CLWP(N,NLL)=0.                                                       GETDAT.316
            CIWP(N,NLL)=0.                                                       05DEC01.1421
            CONWP(N,NLL)=0.                                                      05DEC01.1422
  100     CONTINUE                                                               GETDAT.317
                                                                                 GETDAT.318
                                                                                 GETDAT.319
C                                                                                GETDAT.320
C-----                                                                           GETDAT.321
C-----GROUND TEMPERATURE                                                         GETDAT.322
C-----                                                                           GETDAT.323
#ifndef MPP1                                                                     GETDAT.324
      DO 101 N=1,IPLOND                                                          GETDAT.325
        NLIM=MIN0(N,ILX)                                                         GETDAT.326
#else                                                                            GETDAT.327
        DO 101 N=1,ILX                                                           GETDAT.328
          NLIM=N                                                                 GETDAT.329
#endif                                                                           GETDAT.330
          TG(N)=TGB(NLIM,JSLC)                                                   GETDAT.332
C   WHEN USING BATS CALCULATE AN EQUIVALENT GROUND (SKIN) TEMPERATURE            GETDAT.333
C   BY AVERAGING OVER VEGETATED AND NON-VEGETATED AREAS                          GETDAT.334
C      TG(N)=((1.-VGFRAC(N))*TGB(N,JSLC)**4.+VGFRAC(N)*                          GETDAT.335
C    1   TLEF2D(N,JSLC)**4.)**0.25                                               GETDAT.336
  101   CONTINUE                                                                 GETDAT.337
                                                                                 GETDAT.338
C                                                                                GETDAT.339
      PIE=4.*ATAN(1.)                                                            GETDAT.340
C                                                                                GETDAT.341
C COMPUTE OZONE PATH LENGTHS FROM MIXING RATIO:                                  GETDAT.342
C                                                                                GETDAT.343
C CONSTANTS FOR FOLLOWING SUMS:                                                  GETDAT.344
C                                                                                GETDAT.345
C     GRAVIT = GRAVX * 100.                                                      GETDAT.346
      CPL=V0/(AMD*GRAVIT)                                                        GETDAT.347
      CPWPL=0.5*V0/(AMD*GRAVIT*P0)                                               GETDAT.348
      VMMR=AMD/AMO                                                               GETDAT.349
C                                                                                GETDAT.350
#ifndef MPP1                                                                     GETDAT.351
      DO 225 I=1,IPLON                                                           GETDAT.352
#else                                                                            GETDAT.353
        DO 225 I=1,ILX                                                           GETDAT.354
#endif                                                                           GETDAT.355
C                                                                                GETDAT.356
C SET TOP LEVEL TO SPACE PATH LENGTHS:                                           GETDAT.357
C                                                                                GETDAT.358
          PBOTR=PINT(I,1)*10.                                                    GETDAT.359
C                                                                                GETDAT.360
          PLOS(I,1)=CPL*VMMR*O3MMR(I,1)*PBOTR                                    GETDAT.361
C                                                                                GETDAT.362
          PLOL(I,1)=CPWPL*VMMR*O3MMR(I,1)*(PBOTR*PBOTR)                          GETDAT.363
C                                                                                GETDAT.364
          PTOPR=0.0                                                              GETDAT.365
C                                                                                GETDAT.366
C SET REST OF LEVEL PATH LENGTHS:                                                GETDAT.367
C                                                                                GETDAT.368
          DO 250 K=2,IPLEVP                                                      GETDAT.369
C                                                                                GETDAT.370
            PTOPR=PINT(I,K-1)*10.                                                GETDAT.371
            PBOTR=PINT(I,K)*10.                                                  GETDAT.372
C                                                                                GETDAT.373
            PLOS(I,K)=PLOS(I,K-1)+(CPL*VMMR*O3MMR(I,K-1)*(PBOTR-PTOPR))          GETDAT.374
C                                                                                GETDAT.375
            PLOL(I,K)=PLOL(I,K-1)+(CPWPL*VMMR*O3MMR(I,K-1)*(PBOTR*PBOTR-         GETDAT.376
     +                PTOPR*PTOPR))                                              GETDAT.377
C                                                                                GETDAT.378
  250     CONTINUE                                                               GETDAT.379
  225   CONTINUE                                                                 GETDAT.380
                                                                                 GETDAT.381
C                                                                                GETDAT.382
C COMPUTE EFFECTIVE CLOUD COVER                                                  GETDAT.383
C                                                                                GETDAT.384
      CALL CLDEMS(CLWP,CIWP,CONWP,EMIS)                                          05DEC01.1423
C                                                                                GETDAT.386
      DO 300 K=1,IPLEV                                                           GETDAT.387
#ifndef MPP1                                                                     GETDAT.388
        DO 400 I=1,IPLON                                                         GETDAT.389
#else                                                                            GETDAT.390
        DO 400 I=1,ILX                                                           GETDAT.391
#endif                                                                           GETDAT.392
          EFFCLD(I,K)=CLDFRC(I,K)*EMIS(I,K)                                      GETDAT.393
  400   CONTINUE                                                                 GETDAT.394
                                                                                 GETDAT.395
  300 CONTINUE                                                                   GETDAT.396
C                                                                                GETDAT.397
C CLOUD COVER AT SURFACE INTERFACE ALWAYS ZERO                                   GETDAT.398
C                                                                                GETDAT.399
#ifndef MPP1                                                                     GETDAT.400
      DO 500 I=1,IPLON                                                           GETDAT.401
#else                                                                            GETDAT.402
      DO 500 I=1,ILX                                                             GETDAT.403
#endif                                                                           GETDAT.404
        EFFCLD(I,IPLEVP)=0.                                                      GETDAT.405
        CLDFRC(I,IPLEVP)=0.                                                      GETDAT.406
  500 CONTINUE                                                                   GETDAT.407
                                                                                 GETDAT.408
C                                                                                GETDAT.409
C                                                                                GETDAT.410
      RETURN                                                                     GETDAT.411
      END                                                                        GETDAT.412
                                                                                 GETDAT.413
