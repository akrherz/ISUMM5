      SUBROUTINE TERDRV(HT,LAND,LNDOUT,FORCETER,IX, JX, N,                       TERDRV.1
     >                  VEGEC, LVEGE, VEGEF, SOILC, LSOIL, TEMPG)                TERDRV.2
C                                                                                TERDRV.3
C   PURPOSE :  THIS IS A DRIVER ROUTINE TO CREATE THE TERRAIN HEIGHT             TERDRV.4
C              AND LAND-USE DATA ON THE MESOSCALE GRIDS (SECTION 5.2.2           TERDRV.5
C              AND FIG. 5.1B).                                                   TERDRV.6
C   ON ENTRY : IX,JX: THE DIMENSIONS OF THE MESOSCALE DOMAIN. FOR                TERDRV.7
C                     THE COARSE DOMAIN IX AND JX ARE THE EXPANDED GRID          TERDRV.8
C                     DIMENSION IF IEXP = .TRUE..                                TERDRV.9
C                  N: THE ID NUMBER OF THE MESOSCALE DOMAIN.                     TERDRV.10
C   ON EXIT :  THE HEADER RECORD, TERRAIN HEIGHT, AND LAND-USE FILES             TERDRV.11
C              FOR THE MESOSCALE DOMAIN N ARE CREATED AND WRITTEN OUT            TERDRV.12
C              TO THE DISK WITH THE UNIT NUMBER 29+N.                            TERDRV.13
C   CALLS :    THE SUBROUTINES LATLON, MXMNLL, RDLDTR, CRTER, CRLND,             TERDRV.14
C              SMTH121, SMTHTR, LAKES, BNDRY, AND THE CRAY DEPENDENT             TERDRV.15
C              ROUTINES HPALLOC AND HPDEALLC (SEE SECTION 5.1.2).                TERDRV.16
C   CALLED BY: THE MAIN PROGRAM TERRAIN.                                         TERDRV.17
C   COMMENTS :                                                                   TERDRV.18
C  IN ADDITION TO THE THREE ARGUMENTS, THE DRIVER SUBROUTINE TERDRV GETS         TERDRV.19
C  MOST OF THE INFORMATION FROM THE COMMON BLOCKS /MAPS/, /NESTDMN/,             TERDRV.20
C  /OPTION/, /LTDATA/, AND /TRFUDGE/. THE COMMON BLOCK /SPACE/ AND               TERDRV.21
C  POINTER STATEMENT ARE SERVED TO ALLOCATE THE CORE MEMORY FOR THE              TERDRV.22
C  SOURCE DATA. THE COMMON BLOCK /HEADER/ IS USED TO STORE THE                   TERDRV.23
C  INFORMATION RELATED TO THE DOMAINS.                                           TERDRV.24
C                                                                                TERDRV.25
# include <paramed.incl>                                                         TERDRV.26
# include <parame.incl>                                                          TERDRV.27
# include <io.incl>                                                              06NOV00.452
# include <paramesv.incl>                                                        TERDRV.28
# include <maps.incl>                                                            TERDRV.29
# include <nestdmn.incl>                                                         TERDRV.30
# include <option.incl>                                                          TERDRV.31
# include <ltdata.incl>                                                          TERDRV.32
# include <trfudge.incl>                                                         TERDRV.33
# include <fudge.incl>
# include <space.incl>                                                           TERDRV.34
# include <vs_cmn1.incl>                                                         TERDRV.36
# include <vs_cmn2.incl>                                                         TERDRV.37
C                                                                                TERDRV.38
      DIMENSION HT(IX,JX), LNDOUT(IX,JX), FORCETER(IX,JX), LAND(IX,JX)           TERDRV.39
      DIMENSION VEGEC(IX,JX,NVC), SOILC(IX,JX,NSC), VEGEF(IX,JX,12),             TERDRV.40
     >          TEMPG(IX,JX), LVEGE(IX,JX), LSOIL(IX,JX)                         TERDRV.41
      DIMENSION XLAT(IIMX*JJMX), XLON(IIMX*JJMX), IWORK(NSIZE),                  TERDRV.42
     -          WORK(NOBT),IWORK1(13*2160/4+2),IWORK2(8401/2+2)                  TERDRV.43
                                                                                 TERDRV.44
#if (defined(SUN) || defined(HP))
      INTEGER*2 IWORK3(NCMAX*43200)                                              TERDRV.46
#else                                                                            TERDRV.47
      INTEGER*1 IWORK3(NCMAX*43200)                                              TERDRV.48
#endif                                                                           TERDRV.49
      INTEGER*2 IWORK4(43200)                                                    TERDRV.50
      CHARACTER*1 IWORK3_CHR(NCMAX*43200)                                        TERDRV.51
      CHARACTER*2 IWORK4_CHR(43200)                                              TERDRV.52
      CHARACTER*80 FCHR                                                          TERDRV.53
C                                                                                TERDRV.54
      DIMENSION LPOLE(20)                                                        TERDRV.55
      REAL LNDOUT                                                                TERDRV.56
      INTEGER INN                                                                23DEC04.20
      LOGICAL IFT,IFS                                                            TERDRV.57
C                                                                                TERDRV.58
C  INITIALIZE THE TERRAIN HEIGHT AND LAND USE ARRAYS TO BE 0                     TERDRV.59
C                                                                                TERDRV.60
      DO 5 I = 1,IX                                                              TERDRV.61
      DO 5 J = 1,JX                                                              TERDRV.62
        HT(I,J) = 0.0                                                            TERDRV.63
        LAND(I,J) = 0                                                            TERDRV.64
        LNDOUT(I,J) = 0.0                                                        TERDRV.65
        FORCETER(I,J) = 9E9                                                      TERDRV.66
5     CONTINUE                                                                   TERDRV.67
C                                                                                TERDRV.68
C   GET THE CLOSEST TERRAIN AND LAND USE RESOLUTIONS BASED ON THE GRID S         TERDRV.69
C                                                                                TERDRV.70
      XEXTND = 2.5                                                               TERDRV.71
      DSS = 2.*DIS(N)                                                            TERDRV.72
      IF (DATASW) THEN                                                           TERDRV.73
        NI=NTYPE(N)                                                              TERDRV.74
      ELSE                                                                       TERDRV.75
        DO 10 I = 1,max_type                                                     06NOV00.453
          IF (DSS.GT.DISOBS(I)) THEN                                             TERDRV.77
            NI = I                                                               TERDRV.78
            GO TO 11                                                             TERDRV.79
            NTYPE(N) = NI                                                        TERDRV.80
          ENDIF                                                                  TERDRV.81
10      CONTINUE                                                                 TERDRV.82
      ENDIF                                                                      TERDRV.83
11    IF (NI.GE.3) XEXTND = 1.0                                                  TERDRV.84
      DSS = DSS/2.0                                                              TERDRV.85
C                                                                                TERDRV.86
C   DEFINE THE SEARCH AREA FOR THE DOMAIN:                                       TERDRV.87
C                                                                                TERDRV.88
      IRAT = NRATIO(N)                                                           TERDRV.89
      CALL LATLON(IX,JX,XLAT,XLON,DSS,XSOUTH(N),XWEST(N),N,IRAT,                 TERDRV.90
     1                                                   IPRNTD)                 TERDRV.91
      CALL MXMNLL(IX,JX,XLAT,XLON,XSOUTH(N),XNORTH(N),XWEST(N),                  TERDRV.92
     1              XEAST(N),XEXTND,XMAXLAT,XMINLAT,XMAXLON,XMINLON,             TERDRV.93
     2              LPOLE,N-1)                                                   TERDRV.94
      IF(IPRINT.EQ.1) PRINT 13,XEXTND,XMAXLAT,XMINLAT,XMAXLON,XMINLON            06NOV00.454
13    FORMAT(1X,'XEXTND,XMAXLAT,XMINLAT,XMAXLON,XMINLON:',5F10.2)                TERDRV.96
C                                                                                TERDRV.97
      NVOL0 = IVOL + (NI-1)*2                                                    TERDRV.98
      NVOL1 = NVOL0+ 1                                                           TERDRV.99
      PRINT 12, CENTER(NI),N                                                     06NOV00.455
C                                                                                06NOV00.456
C  CALCULATE THE DIMESION NEEDED (IBLOCK) FOR READING IN THE TERRAIN DAT         06NOV00.457
C                                                                                06NOV00.458
        INCR = 1./CENTER(NI)+0.001                                               06NOV00.459
        IHMAX = (XMAXLAT-XMINLAT)*INCR + 2                                       06NOV00.460
        JHMAX = (XMAXLON-XMINLON)*INCR + 2                                       06NOV00.461
        IBLOCK = IHMAX*JHMAX                                                     06NOV00.462
        LEND1D = IBLOCK                                                          06NOV00.463
        LEND2D = 13*IBLOCK                                                       06NOV00.464
        LENTOT=3*LEND1D + LEND2D                                                 06NOV00.465
        IF (IBLOCK.GT.NOBT.OR.IHMAX.GT.ITRH.OR.JHMAX.GT.JTRH) THEN               05DEC01.42
          PRINT 926,IHMAX,JHMAX,ITRH,JTRH                                        06NOV00.467
926       FORMAT(/1X,'THE DIMENSIONS OF DATA IN I AND J ARE ',2I10,              06NOV00.468
     1           /1X,'THE DECLARED DIMENSIONS ITRH,JTRH IN DECK ARE ',           06NOV00.469
     2           2I10,/1X,'INCREASE THESE DIMENSIONS AND RERUN')                 06NOV00.470
          STOP 'DATA DIMENSIONS TOO SMALL'                                       06NOV00.471
        ENDIF                                                                    06NOV00.472
                                                                                 06NOV00.473
        IF(IPRINT.EQ.1) THEN                                                     06NOV00.474
        PRINT 26, N,NI,IHMAX,JHMAX,IBLOCK,LENTOT                                 06NOV00.475
26      FORMAT(1X,'DOMAIN ',I2,' USES NO. ',I3,' TERRAIN FILE, THE ',            06NOV00.476
     1  'DIMENSIONS FOR READING IN THE TERRAIN DATA ARE: I='                     06NOV00.477
     2  ,I8,', J=',I8/5X,'THE NUMBER OF DATA POINTS: IBLOCK=',I10,2X,            06NOV00.478
     3  'TOTAL SIZE OF HOLDING ARRAYS =',I10)                                    06NOV00.479
        ENDIF                                                                    06NOV00.480
C                                                                                TERDRV.101
      IF (.NOT.IFEZFUG .OR. VEGESOIL) THEN
C  --- GENERATE THE LAND-WATER MASK ARRAY FORCETER:                              TERDRV.103
C         PRINT *, '  === CREATE LAND-WATER MASK DATA TO SET COASTLINE'          06NOV00.481
          NU_LWM   = NI + IVOL_lw                                                06NOV00.482
          NLON     = NLONVEG(NI)                                                 TERDRV.106
          NLAT     = NLATVEG(NI)                                                 TERDRV.107
          RESOL    = RESOLV (NI)                                                 TERDRV.108
          XLAT_MSK = XLATV0 (NI)                                                 TERDRV.109
          XLON_MSK = XLONV0 (NI)                                                 TERDRV.110
          IVWATER  = IVW(NI)                                                     05DEC01.43
C                                                                                TERDRV.111
        CALL CRLWMSK(NLAT, NLON, RESOL, XLAT_MSK, XLON_MSK,                      TERDRV.126
     -                   NU_LWM,     0,  IX,   JX, XLAT,  XLON,                  TERDRV.127
     -                     ZLON,     ZLAT, VEGECAT30S, N, LREC,                  TERDRV.128
     -                  GRDLNMN, GRDLTMN, FORCETER, VEGEC,'LND-WATR',            06NOV00.483
     -                  IVWATER, IPRINT)                                         05DEC01.44
      ENDIF                                                                      TERDRV.130
C                                                                                TERDRV.131
      IF (NI.EQ.max_type .AND. G30SD) THEN                                       06NOV00.485
        IF(IPRINT.EQ.1) PRINT *, 'ACCESSING 30 SEC DATA'                         06NOV00.486
        OPEN(97,FILE='Data/new_30sdata_info',FORM='UNFORMATTED',                 TERDRV.133
     -                                   STATUS='OLD',ERR=999)                   TERDRV.134
        REWIND (97)                                                              TERDRV.135
        READ(97) XLAT00,XLAT11,XLON00,XLON11,NLATS,NPOINTS                       TERDRV.136
C                                                                                06NOV00.487
        IF (XMINLON.GT. 0.0 .AND. XLON00.LT.-180.) THEN                          06NOV00.488
          XLON00 = XLON00 + 360.                                                 06NOV00.489
          XLON11 = XLON11 + 360.                                                 06NOV00.490
        ENDIF                                                                    06NOV00.491
C                                                                                06NOV00.492
        IF(IPRINT.EQ.1)                                                          06NOV00.493
     -     PRINT 996, XLAT00,XLAT11,XLON00,XLON11,NLATS,NPOINTS                  06NOV00.494
 996    FORMAT(//'XLAT00=',F15.8,' XLAT11=',F15.8,' XLON00=',F15.8,              TERDRV.138
     -        ' XLON11=',F15.8,' NREC=',I5,' NLONG=',I5)                         TERDRV.139
C                                                                                TERDRV.140
        CLOSE (97)                                                               TERDRV.141
        NREC(NI) =  NLATS                                                        TERDRV.142
        ID2 (NI) =  NPOINTS                                                      TERDRV.143
        XLATI(NI)=  XLAT00                                                       TERDRV.144
        XLONI(NI)=  XLON00                                                       TERDRV.145
        LENGTH = ID2(NI)/2                                                       TERDRV.146
        IF (MOD(ID2(NI),2).NE.0) LENGTH = LENGTH+1                               TERDRV.147
#ifdef RECLENBYTE                                                                TERDRV.148
        LENGTH = LENGTH*4                                                        TERDRV.149
#endif                                                                           TERDRV.150
        OPEN(NVOL1,FILE='Data/new_30sdata',ACCESS='DIRECT',                      TERDRV.151
     -                            RECL=LENGTH,STATUS='OLD')                      TERDRV.152
        PRINT 997, NI, NVOL1, LENGTH                                             19DEC02.15
 997    FORMAT(/,I2,'==> OPEN DIRECT-ACCESS: FORT.',I2.2,'  FILE=',              19DEC02.16
     -         'Data/new_30sdata      RECORD LENGTH=',I6,/)                      19DEC02.17
        GO TO 998                                                                TERDRV.156
      ENDIF                                                                      TERDRV.157
                                                                                 TERDRV.158
 999  CONTINUE                                                                   TERDRV.159
                                                                                 TERDRV.160
 998  CONTINUE                                                                   TERDRV.161
C                                                                                TERDRV.207
12    FORMAT(3X,'+++ USING DATASETS AT',F10.5,' DEG RES. ',                      06NOV00.496
     1       ' FOR DOMAIN ',I2)                                                  06NOV00.497
C                                                                                TERDRV.211
C  ..READ IN THE TERRAIN (AND LANDUSE) DATA:                                     TERDRV.212
C                                                                                TERDRV.213
      CALL RDLDTR(ID1(NI),ID2(NI),NREC(NI),LREC,                                 TERDRV.214
     1            XLATI(NI),XLONI(NI),CENTER(NI),GRDLTMN,GRDLNMN,                TERDRV.215
     2            XMAXLAT,XMINLAT,XMAXLON,XMINLON,NI,NIOLD,                      TERDRV.216
     3            LND(NI),NVOL0,LNDNAME(NI),ITR(NI),NVOL1,TERNAME(NI),           TERDRV.217
     4            IWORK1,IWORK2,IWORK3,IWORK4,                                   TERDRV.218
     5                          IWORK3_CHR,IWORK4_CHR,IPRINT)                    06NOV00.498
      IF (G30SD.AND.NI.EQ.max_type) CLOSE (NVOL1)                                06NOV00.499
C                                                                                TERDRV.221
C  .. CREATE THE TERRAIN HEIGHT:                                                 TERDRV.222
C                                                                                TERDRV.223
      CALL CRTER(HT,IX,JX,N,DSS,WORK,IHMAX,JHMAX,ZLON,ZLAT,HTOB,                 TERDRV.224
     1           NI,XLAT,XLON,INCR,LREC,GRDLNMN,GRDLTMN                          TERDRV.225
     2           ,IXEX,JXEX)                                                     TERDRV.226
C                                                                                TERDRV.227
1000  CONTINUE                                                                   TERDRV.228
C
      IF (IFEZFUG .OR. .NOT.VEGESOIL) THEN                                       05DEC01.45
C                                                                                TERDRV.229
C  CREATE THE LANDUSE USING OLD DATA:                                            TERDRV.230
C     OR REDEFINE FORCETER USING NCAR GRAPHICS INFO                              05DEC01.46
C                                                                                TERDRV.231
      IF(NI.GT.3) THEN                                                           TERDRV.232
C                                                                                TERDRV.233
C   .. GET THE OBSERVED DATA:                                                    TERDRV.234
C                                                                                TERDRV.235
C  IF THE RESOLUTION OF THE GRID SIZE IS LESS THAN 10 MIN.,  GET THE             TERDRV.238
C  LANDUSE BY INTERPOLATING THE 10 MIN. LANDUSE DATA                             TERDRV.239
C                                                                                TERDRV.240
        NN = 3                                                                   TERDRV.242
        PRINT *,' *** USING 10 MIN. LANDUSE DATA FILE FOR NTYPE = ',NI           06NOV00.500
C                                                                                TERDRV.245
        INCR = 1./CENTER(NN)+0.001                                               TERDRV.246
        IHMAX = (XMAXLAT-XMINLAT)*INCR + 2                                       TERDRV.247
        JHMAX = (XMAXLON-XMINLON)*INCR + 2                                       TERDRV.248
        IBLOCK = IHMAX*JHMAX                                                     TERDRV.249
C                                                                                TERDRV.250
C      LEND1D = IBLOCK                                                           TERDRV.251
C      LEND2D = 13*IBLOCK                                                        TERDRV.252
C      LENTOT=LEND2D + 2*1E6                                                     TERDRV.253
C      CALL HPDEALLC(PTRD11,IER,0)                                               TERDRV.254
C      CALL HPALLOC(PTRD11,LENTOT,IER,0)                                         TERDRV.255
C      IF(IER.NE.0) THEN                                                         TERDRV.256
C      PRINT *,'ERROR IN HEAP ALLOCATION=',IER                                   TERDRV.257
C        PRINT *,'MINIMUM MEMORY REQUIRED FOR LANDUSE SOURCE DATA                TERDRV.258
C     * IS = ', LENTOT*1E-6,' MW'                                                TERDRV.259
C      STOP                                                                      TERDRV.260
C      ENDIF                                                                     TERDRV.261
C      CALL HPCLMOVE(PTRD11,LENTOT,IER,0)                                        TERDRV.262
C                                                                                TERDRV.263
C      IF(IER.NE.0) THEN                                                         TERDRV.264
C      PRINT *,'ERROR IN HEAP ALLOCATION=',IER                                   TERDRV.265
C      STOP                                                                      TERDRV.266
C      ENDIF                                                                     TERDRV.267
C      PTRD12 = PTRD11 + LEND1D                                                  TERDRV.268
C      PTRD13 = PTRD12 + LEND1D                                                  TERDRV.269
C      PTRD21 = PTRD13 + LEND1D                                                  TERDRV.270
C                                                                                TERDRV.271
      NVOL3 = IVOL + (NN-1)*2                                                    TERDRV.272
      NVOL4 = NVOL3+ 1                                                           TERDRV.273
C                                                                                TERDRV.274
         CALL RDLDTR(ID1(NN),ID2(NN),NREC(NN),LREC,                              TERDRV.275
     1            XLATI(NN),XLONI(NN),CENTER(NN),GRDLTMN,GRDLNMN,                TERDRV.276
     2            XMAXLAT,XMINLAT,XMAXLON,XMINLON,NI,3,                          TERDRV.277
     3            LND(NN),NVOL3,LNDNAME(NN),0,NVOL4,TERNAME(NN),                 TERDRV.278
     4            IWORK1,IWORK2,IWORK3,IWORK4,                                   TERDRV.279
     5                          IWORK3_CHR,IWORK4_CHR,IPRINT)                    06NOV00.501
C                                                                                TERDRV.281
      ENDIF                                                                      TERDRV.282
C                                                                                TERDRV.283
      CALL CRLND(LAND,LNDOUT,FORCETER,XLAT,XLON,IX,JX,IHMAX,JHMAX,N,             TERDRV.284
     1           NI,LREC,GRDLNMN,GRDLTMN,ZLON,ZLAT,ZLAND)                        TERDRV.285
C                                                                                TERDRV.286
      END IF
2000  CONTINUE                                                                   TERDRV.287
C                                                                                TERDRV.288
      IF (.NOT.VEGESOIL) GO TO 5000                                              TERDRV.289
C                                                                                TERDRV.290
C -- READ IN VEGETATION DATA:                                                    TERDRV.291
C                                                                                TERDRV.292
      IF (NI.LE.(max_type-1)) THEN                                               06NOV00.502
        NDATA = 1                                                                TERDRV.294
        NCATVO = NVC                                                             TERDRV.295
        NCATSO = NSC                                                             TERDRV.296
CC        NNI = 4                                                                TERDRV.297
      ELSE IF (NI.EQ.max_type) THEN                                              06NOV00.503
        NDATA = 2                                                                TERDRV.299
        NCATVO = 1                                                               TERDRV.300
        NCATSO = 1                                                               TERDRV.301
CC        NNI = NI                                                               TERDRV.302
      ELSE                                                                       TERDRV.303
        PRINT 4999,NI                                                            TERDRV.304
 4999   FORMAT(//10X,'NO DATA TYPE: ',I3,                                        TERDRV.305
     -        ', PLEASE CHECK THE DATATYPE SETTING IN NAMELIST.'/)               TERDRV.306
        STOP                                                                     TERDRV.307
      ENDIF                                                                      TERDRV.308
                                                                                 TERDRV.309
      NU_VG  = NI + IVOL_vg                                                      06NOV00.504
      IVWATER = IVW(NI)                                                          TERDRV.311
      RESOLVG = RESOLV(NI)                                                       TERDRV.312
      XLAT_V0 = XLATV0(NI)                                                       TERDRV.313
      XLON_V0 = XLONV0(NI)                                                       TERDRV.314
      NLONV = NLONVEG(NI)                                                        TERDRV.315
      NLATV = NLATVEG(NI)                                                        TERDRV.316
CC      REWIND NU_VG                                                             TERDRV.317
C                                                                                TERDRV.318
      CALL CRSOIL(NCATVO , NLATV, NLONV, RESOLVG, XLAT_V0, XLON_V0,              TERDRV.319
     -                    -1,NDATA,     1,   0,   NU_VG,   NU_VF,                TERDRV.320
     -                    IX,   JX, XLAT,  XLON,    VEGEC,    LVEGE,             TERDRV.321
     -                  ZLON,  ZLAT,  VEGECAT, VEGECAT30S,  0.0,                 TERDRV.322
     -               N, LREC, GRDLNMN, GRDLTMN, IVWATER, 'VEGE_CAT',             06NOV00.505
     -                IPRINT)                                                    06NOV00.506
C                                                                                TERDRV.324
      IF (VEGONLY) GO TO 5000                                                    TERDRV.325
C                                                                                TERDRV.326
      NU_SL  = NI + IVOL_so                                                      06NOV00.507
      ISWATER = ISW(NI)                                                          TERDRV.328
      RESOLSL = RESOLS(NI)                                                       TERDRV.329
      XLAT_S0 = XLATS0(NI)                                                       TERDRV.330
      XLON_S0 = XLONS0(NI)                                                       TERDRV.331
      NLONS = NLONSOIL(NI)                                                       TERDRV.332
      NLATS = NLATSOIL(NI)                                                       TERDRV.333
C                                                                                TERDRV.334
CC      REWIND NU_SL                                                             TERDRV.335
C                                                                                TERDRV.336
      CALL CRSOIL(NCATSO , NLATS, NLONS, RESOLSL, XLAT_S0, XLON_S0,              TERDRV.337
     -                    -2,NDATA,     1,   0,   NU_SL,   NU_VF,                TERDRV.338
     -                    IX,   JX, XLAT,  XLON,    SOILC,    LSOIL,             TERDRV.339
     -                  ZLON,  ZLAT,  SOILCAT, SOILCAT30S,      0.0,             TERDRV.340
     -               N, LREC, GRDLNMN, GRDLTMN, ISWATER, 'SOIL_CAT',             06NOV00.508
     -                IPRINT)                                                    06NOV00.509
C                                                                                TERDRV.342
C .. VEGETATION FRACTION:                                                        TERDRV.343
      NU_VF  = IVOL_vf                                                           06NOV00.510
CC      REWIND NU_VF                                                             TERDRV.345
      RESOL     = 10./60.                                                        TERDRV.346
      XLAT_VF0  =   90.                                                          05DEC01.47
      XLON_VF0  = -180.                                                          05DEC01.48
      NLAT_VF   = 1080                                                           TERDRV.349
      NLON_VF   = 2160                                                           TERDRV.350
      SHIFT_VF = RESOL/2.                                                        TERDRV.351
      CALL CRSOIL(12, NLAT_VF, NLON_VF, RESOL, XLAT_VF0, XLON_VF0,               TERDRV.352
     -                    -3,    1,     1,   0,   NU_VF,   NU_TG,                TERDRV.353
     -                    IX,   JX, XLAT,  XLON,  VEGEF,   IWORK,                TERDRV.354
     -                  ZLON,  ZLAT,VEGEFRC, VEGECAT30S,SHIFT_VF,                TERDRV.355
     -                     N, LREC, GRDLNMN, GRDLTMN,  0, 'VEGE_FRC',            06NOV00.511
     -                IPRINT)                                                    06NOV00.512
C                                                                                TERDRV.357
                                                                                 TERDRV.358
C                                                                                TERDRV.359
C .. GROUD TEMPERATURE:                                                          TERDRV.360
      NU_TG  = IVOL_ts                                                           06NOV00.513
CC      REWIND NU_TG                                                             TERDRV.362
      XLAT_TG0 =   90.                                                           TERDRV.363
      XLON_TG0 = -180.                                                           TERDRV.364
      NLAT_TG = 180                                                              TERDRV.365
      NLON_TG = 360                                                              TERDRV.366
      RESOL = 1.0                                                                TERDRV.367
      SHIFT_TG = RESOL/2.                                                        TERDRV.368
      CALL CRSOIL(1, NLAT_TG, NLON_TG, RESOL, XLAT_TG0, XLON_TG0,                TERDRV.369
     -                    -4,    1,     0,   1,   NU_VF,   NU_TG,                TERDRV.370
     -                    IX,   JX, XLAT,  XLON,  TEMPG,   IWORK,                TERDRV.371
     -                  ZLON,  ZLAT,TEMPGRD, VEGECAT30S,SHIFT_TG,                TERDRV.372
     -                     N, LREC, GRDLNMN, GRDLTMN,  0, 'TEMP_GRD',            06NOV00.514
     -                IPRINT)                                                    06NOV00.515
C                                                                                TERDRV.374
 5000 CONTINUE                                                                   TERDRV.375
C                                                                                TERDRV.376
C   FOR WATER BODIES, FORCE THE TERRAIN TO BE EQUAL TO FORCETER,                 TERDRV.377
C   AND THEN SET ALL TERRAIN HEIGHTS TO BE > OR = 0.0                            TERDRV.378
C                                                                                TERDRV.379
      IHERE=0                                                                    TERDRV.380
2005  IHERE=IHERE+1                                                              TERDRV.381
C                                                                                TERDRV.382
      IF (IFEZFUG) THEN                                                          TERDRV.383
         DO 2010 I=1,IX-1                                                        TERDRV.384
         DO 2010 J=1,JX-1                                                        TERDRV.385
            IF (FORCETER(I,J).EQ.-.001) THEN                                     TERDRV.386
               HT(I,J)=-.001                                                     TERDRV.387
            ELSEIF (FORCETER(I,J).NE.9E9) THEN                                   TERDRV.388
               HT(I,J)=MAX(0.001,FORCETER(I,J))                                  TERDRV.389
            ELSE                                                                 TERDRV.390
               HT(I,J)=MAX(0.001,HT(I,J))                                        TERDRV.391
            ENDIF                                                                TERDRV.392
2010     CONTINUE                                                                TERDRV.393
      ELSE                                                                       TERDRV.394
C .. SET THE HEIGHTS FOR THE GREAT LAKES:                                        TERDRV.395
         CALL LAKES(IX,JX,HT,FORCETER,XLAT,XLON,IPRINT)                          06NOV00.516
C                                                                                TERDRV.397
C .. TO FIX THE COAST LINE BASED ON THE LAND-WATER MASK:                         TERDRV.398
C                                                                                TERDRV.399
         IF(IHERE.EQ.1)                                                          06NOV00.517
     -      PRINT *, '  +++ USING LAND-WATER MASK DATA TO SET COASTLINE'         06NOV00.518
         DO 2012 I=1,IX-1                                                        TERDRV.400
         DO 2012 J=1,JX-1                                                        TERDRV.401
            IF (FORCETER(I,J).NE.9E9) THEN                                       TERDRV.402
               HT(I,J)=MAX(-.001,HT(I,J))                                        TERDRV.403
               IF (HT(I,J).LE.5.) HT(I,J)=-.001                                  TERDRV.404
            ELSE                                                                 TERDRV.405
               HT(I,J)=MAX(.001,HT(I,J))                                         TERDRV.406
            ENDIF                                                                TERDRV.407
2012     CONTINUE                                                                TERDRV.408
      ENDIF                                                                      TERDRV.409
C                                                                                TERDRV.410
C   IF THIS IS THE SECOND TIME THROUGH THIS, SKIP PAST SMOOTHING                 TERDRV.411
C                                                                                TERDRV.412
      IF (IHERE.EQ.2) GOTO 2040                                                  TERDRV.413
C                                                                                TERDRV.414
C  5, MISCELLANEOUS:                                                             TERDRV.415
C                                                                                TERDRV.416
C ** NOTE THAT AFTER SMOOTHING THE ZERO HEIGHT CONTOUR MAY SHIFTED               TERDRV.417
C ** TO OFF SHORE, ESPECIALLY FOR COARSE GRIDS. BUT TO AVOID THE                 TERDRV.418
C ** CRIPPLE THIS IS REQUIRED BY DYNIMICS. SO THIS PROCEDURE IS DONE             TERDRV.419
C ** AFTER THE COAST-LINE FIXED.                                                 TERDRV.420
C                                                                                TERDRV.421
C THIS IS A "HARSH" 1-2-1 SMOOTHER                                               TERDRV.422
      IF(ISMTHTR.EQ.1) THEN                                                      TERDRV.423
      CALL SMTH121(HT,IX,JX,3)                                                   TERDRV.424
      ELSEIF(ISMTHTR.EQ.2) THEN                                                  TERDRV.425
C THIS IS A 2 PASS SMOOTHER/DESMOOTHER                                           TERDRV.426
      CALL SMTHTR(HT,IX,JX,IPRINT)                                               06NOV00.519
      ELSEIF(ISMTHTR.GE.4) THEN                                                  23DEC04.21
        DO INN=1,ISMTHTR                                                         23DEC04.22
          CALL SMTH121(HT,IX,JX,3)                                               23DEC04.23
        ENDDO                                                                    23DEC04.24
      ENDIF                                                                      TERDRV.428
      NIOLD = NI                                                                 TERDRV.429
C                                                                                TERDRV.430
C   REPEAT LOOP 2010 ABOVE                                                       TERDRV.431
C                                                                                TERDRV.432
      GOTO 2005                                                                  TERDRV.433
2040  CONTINUE                                                                   TERDRV.434
C                                                                                TERDRV.435
C FORCE ZERO TERRAIN GRADIENT AROUND OUTER 2 ROWS AND COLUMNS                    TERDRV.436
C                                                                                TERDRV.437
      IF (IBNDRY.AND.N.EQ.1) CALL BNDRY(IX,JX,N,HT,LAND,LNDOUT)                  TERDRV.438
C                                                                                TERDRV.439
      IF (VEGESOIL) THEN                                                         TERDRV.440
C                                                                                TERDRV.441
        IF (VEGONLY) GO TO 2091                                                  TERDRV.442
         TSPVAL = 0.0                                                            TERDRV.443
         RAD = 300.                                                              TERDRV.444
         IF(IPRINT.EQ.1) PRINT *,'RADIUS OF INFLUENCE =',RAD,' KM (',            06NOV00.520
     >                                    RAD/DIS(N),' GRIDS)'                   TERDRV.446
C                                                                                TERDRV.447
         DO 2090 I=1,IX-1                                                        TERDRV.448
         DO 2090 J=1,JX-1                                                        TERDRV.449
            IF (FORCETER(I,J).EQ.9E9) THEN                                       TERDRV.450
               IF(TEMPG(I,J).EQ.TSPVAL)                                          TERDRV.451
     >           CALL PATCH(IX,JX,TEMPG,I,J,RAD,DIS(N),TSPVAL)                   TERDRV.452
            ELSE                                                                 TERDRV.453
               TEMPG(I,J) = TSPVAL                                               TERDRV.454
            ENDIF                                                                TERDRV.455
2090     CONTINUE                                                                TERDRV.456
C                                                                                TERDRV.457
C .. USING LATITUDE-DEPENDED SOIL TEMPG TO FILL IN THE MISSING                   TERDRV.458
C    VALUES OVER THE LAND.                                                       TERDRV.459
C                                                                                06NOV00.521
        CALL SOIL_TG(IX,JX,XLAT,TEMPG,FORCETER,TSPVAL,IPRINT)                    06NOV00.522
C                                                                                TERDRV.461
C .. ADJUST THE TEMPG TO GROUND T FROM 0M (1000MB) TEMPG:........                TERDRV.462
         DO I = 1,IX-1                                                           TERDRV.463
         DO J = 1,JX-1                                                           TERDRV.464
         IF (TEMPG(I,J).NE.TSPVAL) THEN                                          TERDRV.465
           TEMPAD = 6.50 * HT(I,J)/1000.                                         TERDRV.466
           TEMPG(I,J) = TEMPG(I,J)-TEMPAD                                        TERDRV.467
         ENDIF                                                                   TERDRV.468
         END DO                                                                  TERDRV.469
         END DO                                                                  TERDRV.470
2091    CONTINUE                                                                 TERDRV.471
C
C ALLOW FOR FUDGING OF NEW LANDUSE
C
C IFFUG  = .T.    FUDGE LANDUSE; NPFUG = # POINTS TO FUDGE
C
      IF (IFFUG(N)) THEN
         PRINT 62,N,NDFUG(N)
 62      FORMAT (/,'  ***** LAND USE FUDGING IS PERFORMED',
     -              ' ON THE NEST NO.:',I3,2X,I5,' GRID-POINTS.')
         CALL FUDGER(N,NDFUG(N),IFUG(1,N),JFUG(1,N),LNDFUG(1,N),
     -                                    LVEGE,LNDOUT,IX,JX)
      ENDIF
C                                                                                TERDRV.472
C .. ADJUST THE VEGETATION AND SOIL PERCENTAGES BASED ON THE                     TERDRV.473
C    LAND-WATER MASK ARRAY FORCETER:                                             TERDRV.474
C                                                                                TERDRV.475
         CALL WATERCHECK(IX,JX,NVC,FORCETER, VEGEC, LVEGE, IVWATER,              TERDRV.476
     -                   49.0, RAD,DIS(N),IPRINT)                                23DEC04.25
C                                                                                TERDRV.478
       IF (.NOT.VEGONLY)                                                         TERDRV.479
     -   CALL WATERCHECK(IX,JX,NSC,FORCETER, SOILC, LSOIL, ISWATER,              TERDRV.480
     -                   49.0, RAD,DIS(N),IPRINT)                                23DEC04.26
C                                                                                TERDRV.482
       IF (.NOT.VEGONLY)                                                         TERDRV.483
     -   CALL WATER_VFR(IX,JX,12,FORCETER,  VEGEF, 65.0, DIS(N),IPRINT)          06NOV00.523
C                                                                                TERDRV.485
      ENDIF                                                                      TERDRV.486
C                                                                                TERDRV.487
C ................................................................               TERDRV.488
C                                                                                TERDRV.489
C   WRITE OUT ALL TERRESTIAL FIELDS                                              TERDRV.490
C                                                                                TERDRV.491
C   - APPLY 1-2-1 SMOOTH TO THE BIGGEST DOMAIN:                                  TERDRV.492
C                                                                                TERDRV.493
C MOVE SMOOTHER ABOVE TERRAIN/LANDUSE FUDGING/CONSISTECY CHECKS                  TERDRV.494
                                                                                 TERDRV.495
      NUOUT = IVOL_mdl + N                                                       06NOV00.524
      LENT = NESTIX(N)*NESTJX(N)                                                 TERDRV.497
C                                                                                TERDRV.498
C     WRITE(NUOUT) MIF,MRF,MIFC,MRFC                                             TERDRV.499
C                                                                                TERDRV.500
        IF(IPRINT.EQ.1)PRINT 2060,NUOUT,IX*JX                                    06NOV00.525
 2060   FORMAT(/' ==>> WRITE OUT THE TERRAIN HT ON UNIT',I2,                     06NOV00.526
     -         ',   SIZE=',I6,' <<==')                                           TERDRV.503
        WRITE(NUOUT) HT                                                          TERDRV.504
C                                                                                TERDRV.505
C  OUTPUT LANDUSE IN FLOAT NO.                                                   TERDRV.506
C                                                                                TERDRV.507
C     PRINT *,'LAND = ',(LAND(IIP,10),IIP=2,10)                                  TERDRV.508
      DO 2050 I=1,IX                                                             TERDRV.509
      DO 2050 J=1,JX                                                             TERDRV.510
       LNDOUT(I,J)=FLOAT(LAND(I,J))                                              TERDRV.511
       IF (VEGESOIL) LNDOUT(I,J)=FLOAT(LVEGE(I,J))                               TERDRV.512
 2050 CONTINUE                                                                   TERDRV.513
        IF(IPRINT.EQ.1)PRINT 2061,NUOUT,IX*JX                                    06NOV00.527
 2061   FORMAT(' ==>> WRITE OUT THE LANDUSE    ON UNIT',I2,                      06NOV00.528
     -         ',   SIZE=',I6,' <<==')                                           TERDRV.516
      WRITE(NUOUT) LNDOUT                                                        TERDRV.517
C                                                                                TERDRV.518
C  OUTPUT VEGETATION AND SOIL DATA FILES:                                        TERDRV.519
C                                                                                TERDRV.520
      IF (VEGESOIL) THEN                                                         TERDRV.521
C                                                                                TERDRV.522
        IF (VEGONLY) GO TO 3000                                                  TERDRV.523
C                                                                                TERDRV.524
        DO NC = 1,NVC                                                            TERDRV.525
          DO I = 1,IX                                                            TERDRV.526
          DO J = 1,JX                                                            TERDRV.527
            HT(I,J) = VEGEC(I,J,NC)                                              TERDRV.528
          END DO                                                                 TERDRV.529
          END DO                                                                 TERDRV.530
          IF(IPRINT.EQ.1)PRINT 2062,NC,NUOUT,IX*JX                               06NOV00.529
 2062     FORMAT(I2,' >> WRITE OUT THE VEGE. CAT. ON UNIT',I2,                   TERDRV.532
     -         ',   SIZE=',I6,' <<==')                                           TERDRV.533
          WRITE(NUOUT) HT                                                        TERDRV.534
        END DO                                                                   TERDRV.535
C                                                                                TERDRV.536
        DO NC = 1,NSC                                                            TERDRV.537
          DO I = 1,IX                                                            TERDRV.538
          DO J = 1,JX                                                            TERDRV.539
            HT(I,J) = SOILC(I,J,NC)                                              TERDRV.540
          END DO                                                                 TERDRV.541
          END DO                                                                 TERDRV.542
          IF(IPRINT.EQ.1)PRINT 2063,NC,NUOUT,IX*JX                               06NOV00.530
 2063     FORMAT(I2,' >> WRITE OUT THE SOIL  CAT. ON UNIT',I2,                   TERDRV.544
     -         ',   SIZE=',I6,' <<==')                                           TERDRV.545
          WRITE(NUOUT) HT                                                        TERDRV.546
        END DO                                                                   TERDRV.547
C                                                                                TERDRV.548
        DO NC = 1,12                                                             TERDRV.549
          DO I = 1,IX                                                            TERDRV.550
          DO J = 1,JX                                                            TERDRV.551
            HT(I,J) = VEGEF(I,J,NC)                                              TERDRV.552
          END DO                                                                 TERDRV.553
          END DO                                                                 TERDRV.554
          IF(IPRINT.EQ.1)PRINT 2064,NC,NUOUT,IX*JX                               06NOV00.531
 2064     FORMAT(I2,' >> WRITE OUT THE VEGE. FRC. ON UNIT',I2,                   TERDRV.556
     -         ',   SIZE=',I6,' <<==')                                           TERDRV.557
          WRITE(NUOUT) HT                                                        TERDRV.558
        END DO                                                                   TERDRV.559
C                                                                                TERDRV.560
        IF(IPRINT.EQ.1)PRINT 2065,NUOUT,IX*JX                                    06NOV00.532
 2065   FORMAT(' ==>> WRITE OUT THE GRND TEMP. ON UNIT',I2,                      06NOV00.533
     -         ',   SIZE=',I6,' <<==')                                           TERDRV.563
        WRITE(NUOUT) TEMPG                                                       TERDRV.564
C                                                                                TERDRV.565
        IF(IPRINT.EQ.1)PRINT 2066,NUOUT,IX*JX                                    06NOV00.534
 2066   FORMAT(' ==>> WRITE OUT THE LAND/WATER MASK ON UNIT',I2,                 06NOV00.535
     -         ',   SIZE=',I6,' <<==')                                           TERDRV.568
        DO I = 1,IX                                                              TERDRV.569
        DO J = 1,JX                                                              TERDRV.570
          IF (FORCETER(I,J).EQ.9E9) THEN                                         TERDRV.571
            FORCETER(I,J) = 1.0                                                  TERDRV.572
          ELSE                                                                   TERDRV.573
            FORCETER(I,J) = 0.0                                                  TERDRV.574
          ENDIF                                                                  TERDRV.575
        END DO                                                                   TERDRV.576
        END DO                                                                   TERDRV.577
        WRITE(NUOUT) FORCETER                                                    TERDRV.578
C                                                                                TERDRV.579
C VEGETATION INDEX IS WRITTEN OUT IN PLACE OF LAND USE IN V3                     TERDRV.580
C                                                                                TERDRV.581
C       PRINT 2067,NUOUT,IX*JX                                                   TERDRV.582
 2067   FORMAT(' ==>> WRITE OUT THE VEGE INDEX ON UNIT',I2,                      06NOV00.536
     -         ',   SIZE=',I6,' <<==')                                           TERDRV.584
C       DO I = 1,IX                                                              TERDRV.585
C       DO J = 1,JX                                                              TERDRV.586
C         LNDOUT(I,J)=FLOAT(LVEGE(I,J))                                          TERDRV.587
C       END DO                                                                   TERDRV.588
C       END DO                                                                   TERDRV.589
C       WRITE(NUOUT) LNDOUT                                                      TERDRV.590
C                                                                                TERDRV.591
        IF (.NOT. VEGONLY) THEN                                                  TERDRV.592
        IF(IPRINT.EQ.1)PRINT 2068,NUOUT,IX*JX                                    06NOV00.537
 2068   FORMAT(' ==>> WRITE OUT THE SOIL INDEX ON UNIT',I2,                      06NOV00.538
     -         ',   SIZE=',I6,' <<==')                                           TERDRV.595
        DO I = 1,IX                                                              TERDRV.596
        DO J = 1,JX                                                              TERDRV.597
          LNDOUT(I,J)=FLOAT(LSOIL(I,J))                                          TERDRV.598
        END DO                                                                   TERDRV.599
        END DO                                                                   TERDRV.600
        WRITE(NUOUT) LNDOUT                                                      TERDRV.601
        ENDIF                                                                    TERDRV.602
C                                                                                TERDRV.603
      ENDIF                                                                      TERDRV.604
3000  CONTINUE                                                                   TERDRV.605
C                                                                                TERDRV.606
C      IF (N.EQ.1 .AND. NSTTYP.EQ.2) GO TO 4000                                  TERDRV.607
C                                                                                TERDRV.608
C      CALL HPDEALLC(PTRD11,IER,0)                                               TERDRV.609
C      IF(IER.NE.0) THEN                                                         TERDRV.610
C      PRINT *,'ERROR IN HEAP ALLOCATION=',IER                                   TERDRV.611
C      STOP                                                                      TERDRV.612
C      ENDIF                                                                     TERDRV.613
4000  RETURN                                                                     TERDRV.614
      END                                                                        TERDRV.615
