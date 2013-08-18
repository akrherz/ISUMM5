      SUBROUTINE SETUP                                                           SETUP.1
C                                                                                SETUP.2
C   PURPOSE :  INITIALIZES THE CONSTANTS AND VARIABLES WHICH CAN BE              SETUP.3
C              DETERMINED BASED ON THE INPUT NAMELIST VARIABLES.                 SETUP.4
C   CALLS :    THE INTRINSIC FUNCTIONS SIN, COS, TAN, ALOG, AND ALOG10.          SETUP.5
C   CALLED BY :THE MAIN PROGRAM TERRAIN.                                         SETUP.6
C   COMMENTS :                                                                   SETUP.7
C  THE COMMON BLOCKS /MAPS/, /NESTDMN/, /FUDG/, /TRFUDGE/, /OPTION/,             SETUP.8
C  /LTDATA/, AND /HEADER/ ARE USED IN THIS SUBROUTINE. THERE ARE FIVE            SETUP.9
C  NAMELISTS /MAPBG/, /DOMAIN/, /OPTN/, /FUDGE/, AND /FUDGET/ IN THIS            SETUP.10
C  SUBROUTINE. THIS SUBROUTINE HAS NO DUMMY ARGUMENTS. PLEASE SEE                SETUP.11
C  SECTION 5.1.1 AND 5.2.1 FOR DETAILS.                                          SETUP.12
C                                                                                06NOV00.273
C  Two new namelist variables:                                                   06NOV00.274
C                                                                                06NOV00.275
      LOGICAL LSMDATA                                                            06NOV00.276
      INTEGER VEGTYPE                                                            06NOV00.277
C                                                                                SETUP.13
# include <ezwater.incl>                                                         SETUP.14
# include <parame.incl>                                                          SETUP.15
# include <paramed.incl>                                                         06NOV00.278
# include <io.incl>                                                              06NOV00.279
# include <paramesv.incl>                                                        SETUP.16
# include <maps.incl>                                                            SETUP.17
# include <nestdmn.incl>                                                         SETUP.18
# include <fudge.incl>                                                           SETUP.19
# include <trfudge.incl>                                                         SETUP.20
# include <option.incl>                                                          SETUP.21
# include <ltdata.incl>                                                          SETUP.22
# include <namelist.incl>                                                        SETUP.23
# include <vs_cmn1.incl>                                                         SETUP.24
# include <vs_cmn2.incl>                                                         SETUP.25
# include <vs_data.incl>                                                         SETUP.26
C                                                                                SETUP.27
C   ID1   :                                                                      SETUP.28
C   ID2   :                                                                      SETUP.29
C   NREC  :                                                                      SETUP.30
C   LND   :                                                                      SETUP.31
C   ITR   :                                                                      SETUP.32
C   XLATI :                                                                      SETUP.33
C   XLONI :                                                                      SETUP.34
C   NIOLD :                                                                      SETUP.35
      DATA    ID1/    13,    13,    13,     1,     1,     1/,                    06NOV00.280
     1        ID2/   360,   720,  2160,  4320, 10800, 43200/,                    06NOV00.281
     2       NREC/   180,   360,  1080,  2160,  5400, 21600/,                    06NOV00.282
     3        LND/     1,     1,     1,     0,     0,     0/,                    06NOV00.283
     4        ITR/     1,     1,     1,     1,     1,     1/,                    06NOV00.284
     5      XLATI/   90.,   90.,   90.,   90.,   90.,   90./,                    06NOV00.285
     6      XLONI/ -180., -180., -180., -180., -180., -180./,                    06NOV00.286
     7      NIOLD/ 0/                                                            SETUP.43
C                                                                                SETUP.44
      CHARACTER*16 FILENAME                                                      SETUP.45
      LOGICAL OPENED
C                                                                                SETUP.46
C    DISOBS ARE THE DISTANCES(KM) OF 1, .5, 1/6, 1/12 AND 1/120 DEGREE.          SETUP.47
      DATA DISOBS/ 111. ,  55. ,  18.5,  9.25, 3.70, 0.925/                      06NOV00.287
C                                                                                SETUP.50
      DATA CONV/57.29578/, A/6370./                                              SETUP.51
      DATA NPROJ/'LAMCON','POLSTR','MERCAT'/                                     SETUP.52
C                                                                                SETUP.53
      PRINT *,'THIS IS TERRAIN VERSION 3.7.0'                                    23DEC04.14
      PRINT *,'==> CALL SETUP <=='                                               06NOV00.289
C                                                                                SETUP.56
C  TERRAIN AND LANDUSE RESOLUTION:                                               SETUP.57
      CENTER(1) =   1.0                                                          SETUP.58
      CENTER(2) =   1./2.                                                        SETUP.59
      CENTER(3) =   1./6.                                                        SETUP.60
      CENTER(4) =   1./12.                                                       SETUP.61
      CENTER(5) =   1./30.                                                       06NOV00.290
      CENTER(6) =   1./120.                                                      06NOV00.291
C                                                                                SETUP.63
C  READ IN MASTER INPUT FILE:                                                    SETUP.64
C                                                                                SETUP.65
C  INITIALIZE THE TERRAIN AND LAND USE RESOLUTIONS TO BE 3                       SETUP.66
C  (5 MIN TERRAIN AND 10 MINUTES LAND USE)                                       SETUP.67
        DO M=1,100                                                               SETUP.68
          NTYPE(M)=3                                                             SETUP.69
        NSTTYP(M) = 2                                                            SETUP.70
        ENDDO                                                                    SETUP.71
C                                                                                SETUP.72
C  The starting unit numbers for land-water mask, vegetation, soil,              06NOV00.292
C      vegetation fraction, and annual deep soil temperature:                    06NOV00.293
C                                                                                06NOV00.294
        IVOL_lw = IVOL    + (max_type-1)*2+1                                     06NOV00.295
        IVOL_vg = IVOL_lw + max_type                                             06NOV00.296
        IVOL_so = IVOL_vg + max_type                                             06NOV00.297
        IVOL_vf = IVOL_so + max_type + 1                                         06NOV00.298
        IVOL_ts = IVOL_vf + 1                                                    06NOV00.299
        DO I = 1,max_type                                                        06NOV00.300
          TERNAME(I) = '                    '                                    06NOV00.301
          LNDNAME(I) = '                    '                                    06NOV00.302
          LWNAME(I)  = '                    '                                    06NOV00.303
          VGNAME(I)  = '                    '                                    06NOV00.304
          SONAME(I)  = '                    '                                    06NOV00.305
        END DO                                                                   06NOV00.306
        VFNAME = '                    '                                          06NOV00.307
        TSNAME = '                    '                                          06NOV00.308
C                                                                                06NOV00.309
C  INTIALIZE THE FUDGE ARRAY:                                                    SETUP.73
C                                                                                SETUP.74
      DO 1 M = 1,MAXNES                                                          SETUP.75
        IFFUG(M) = .FALSE.                                                       SETUP.76
        NDFUG(M) = 1                                                             SETUP.77
      DO 1 K=1,200                                                               SETUP.78
        IFUG(K,M) = 0                                                            SETUP.79
        JFUG(K,M) = 0                                                            SETUP.80
        LNDFUG(K,M) = 0                                                          SETUP.81
1     CONTINUE                                                                   SETUP.82
                                                                                 SETUP.83
C  INITIALIZE THE OPTN NAMELIST                                                  SETUP.84
                                                                                 SETUP.85
      IPRINT  =  0                                                               SETUP.86
      IPRNTD  = .FALSE.                                                          SETUP.87
      IFFUDG  = .FALSE.                                                          SETUP.88
      IFTFUG  = .FALSE.                                                          SETUP.89
      IBNDRY  = .TRUE.                                                           SETUP.90
      IPRTHT  = .FALSE.                                                          SETUP.91
      IPRTLU  = .FALSE.                                                          SETUP.92
      IWRITE  = .TRUE.                                                           SETUP.93
      IPFLAG  = .FALSE.                                                          SETUP.94
      IFANAL  = .FALSE.                                                          SETUP.95
      ISURF   = .TRUE.                                                           SETUP.96
      IEXTRA  = .FALSE.                                                          SETUP.97
      IFTER   = .TRUE.                                                           SETUP.98
      IFILL   = .FALSE.                                                          SETUP.99
      DO I = 1,10                                                                SETUP.100
      FIN(I)  = 100.      ! TERRAIN PLOT CONTOUR INTERVAL                        SETUP.101
      END DO                                                                     SETUP.102
      ISMTHTR=1                                                                  SETUP.103
      DATASW  = .TRUE.                                                           05DEC01.31
      IFEZFUG = .FALSE.                                                          SETUP.105
      VEGONLY = .FALSE.
      VEGESOIL= .FALSE.                                                          SETUP.106
      VSPLOT  = .FALSE.                                                          SETUP.107
      G30SD   = .FALSE.                                                          SETUP.108
      LSMDATA = .FALSE.                                                          06NOV00.310
      VEGTYPE = 0                                                                06NOV00.311
C                                                                                SETUP.109
C   INITIALIZE THE HEIGHTS OF EZMAP WATER BODIES                                 SETUP.110
C                                                                                SETUP.111
      DO 3 I=1,2000                                                              SETUP.112
         HTPS(I)=9E9                                                             SETUP.113
 3    CONTINUE                                                                   SETUP.114
C                                                                                SETUP.115
      TRUELAT1=91.0                                                              SETUP.116
      TRUELAT2=91.0                                                              SETUP.117
C                                                                                SETUP.118
      READ(15,MAPBG)                                                             SETUP.119
      READ(15,DOMAINS)                                                           SETUP.121
      READ(15,OPTN)                                                              SETUP.123
      READ(15,FUDGE)                                                             SETUP.125
      READ(15,FUDGET)                                                            SETUP.127
      READ(15,EZFUDGE)                                                           SETUP.129
      READ(15,DATANAME)                                                          SETUP.131
C                                                                                06NOV00.312
C PRINT BASIC NAMELIST NUMBERS                                                   06NOV00.313
C                                                                                06NOV00.314
      PRINT 990                                                                  06NOV00.315
990   FORMAT(/1X,'INPUT NAMELIST VALUES:')                                       06NOV00.316
      PRINT 900, PHIC,XLONC                                                      06NOV00.317
900   FORMAT(3X,'CENTRAL LAT/LONG = ', 2F8.2)                                    06NOV00.318
      PRINT 910, IEXP, AEXP                                                      06NOV00.319
910   FORMAT(3X,'IEXP, AEXP       = ',L6,2X,F8.1)                                06NOV00.320
      PRINT 920, IPROJ                                                           06NOV00.321
920   FORMAT(3X,'PROJECTION       = ',A8)                                        06NOV00.322
      PRINT 930, MAXNES                                                          06NOV00.323
930   FORMAT(3X,'MAXNES           = ',I2)                                        06NOV00.324
      PRINT 940                                                                  06NOV00.325
940   FORMAT(3X,'DOMAIN NESTIX NEXTJX      DIS NUMNC  NESTI  NESTJ ',            06NOV00.326
     -          'NTYPE NSTTYP')                                                  06NOV00.327
      DO NN = 1,MAXNES                                                           06NOV00.328
         PRINT 950, NN,NESTIX(NN),NESTJX(NN),DIS(NN),NUMNC(NN),                  06NOV00.329
     -              NESTI(NN),NESTJ(NN),NTYPE(NN),NSTTYP(NN)                     06NOV00.330
         IF (NTYPE(NN).EQ.6) G30SD = .TRUE.                                      06NOV00.331
      END DO                                                                     06NOV00.332
      IF (VEGESOIL .OR. VEGONLY) THEN                                            06NOV00.333
          PRINT 945                                                              06NOV00.334
          STOP 945                                                               06NOV00.335
      ENDIF                                                                      06NOV00.336
945   FORMAT(1X,'xxxxx PLEASE RE-GENERATE terrain.deck FOR TERRAIN ',            06NOV00.337
     -       'VERSION 3.4 OR LATER. YOUR DECK IS OBSOLETE.')                     06NOV00.338
      IF (VEGTYPE.GT.0 .AND. LSMDATA) THEN                                       06NOV00.339
C ..... process vegetation and soil data for LSM                                 06NOV00.340
         VEGESOIL = .TRUE.                                                       06NOV00.341
          VEGONLY = .FALSE.                                                      06NOV00.342
      ELSE IF (VEGTYPE.GT.0 .AND. .NOT.LSMDATA) THEN                             06NOV00.343
C ..... process vegetation for landuse definition only                           06NOV00.344
         VEGESOIL = .TRUE.                                                       06NOV00.345
          VEGONLY = .TRUE.                                                       06NOV00.346
      ELSE IF (VEGTYPE.EQ.0) THEN                                                06NOV00.347
C ..... use old landuse data                                                     06NOV00.348
         VEGESOIL = .FALSE.                                                      06NOV00.349
          VEGONLY = .FALSE.                                                      06NOV00.350
      END IF                                                                     06NOV00.351
      IF (VEGTYPE.GT.0) THEN                                                     06NOV00.352
C ..... if old landuse data are not selected, do not read it                     06NOV00.353
         DO I = 1,MAX_TYPE                                                       06NOV00.354
            LND(I) = 0                                                           06NOV00.355
         END DO                                                                  06NOV00.356
      END IF                                                                     06NOV00.357
950   FORMAT(3X,I6,2I7,1X,F8.3,1X,I5,2I7,1X,I5,1X,I6)                            06NOV00.358
      PRINT 960, DATASW,IFEZFUG                                                  06NOV00.359
960   FORMAT(3X,'DATASW,IFEZFUG           = ',2L6)                               06NOV00.360
      IF (LSMDATA) PRINT 972                                                     06NOV00.361
972   FORMAT(3X,'TERRAIN WILL PROCESS LAND-SURFACE MODEL DATA')                  06NOV00.362
      IF (VEGTYPE.EQ.0) PRINT 973                                                06NOV00.363
973   FORMAT(3X,'TERRAIN WILL USE OLD 13-CATEGORY LANDUSE DATA')                 06NOV00.364
      IF (VEGTYPE.EQ.1) PRINT 974                                                06NOV00.365
974   FORMAT(3X,'TERRAIN WILL USE 24-CATEGORY USGS LANDUSE DATA')                06NOV00.366
      IF (VEGTYPE.EQ.2) PRINT 975                                                06NOV00.367
975   FORMAT(3X,'TERRAIN WILL USE 16-CATEGORY SiB LANDUSE DATA')                 06NOV00.368
      PRINT 970, VEGESOIL,VEGONLY,IEXTRA                                         06NOV00.369
970   FORMAT(3X,'VEGESOIL,VEGONLY,IEXTRA  = ',3L6)                               06NOV00.370
      PRINT 980, G30SD                                                           06NOV00.371
980   FORMAT(3X,'SELECT GLOBAL 30 SEC DATA? ',L6,/)                              06NOV00.372
C                                                                                SETUP.133
C RESET IFEZFUG IF NCAR GRAPHICS IS NOT USED                                     SETUP.134
C                                                                                SETUP.135
#ifndef NCARG                                                                    SETUP.136
      IFEZFUG = .FALSE.                                                          SETUP.137
#endif                                                                           SETUP.138
C                                                                                SETUP.139
C .. OPEN REQUESTED DIRECT-ACCESS DATA FILES IF IFTER = TRUE:                    SETUP.140
C                                                                                SETUP.141
      IF (IFTER) THEN
      DO NN = 1,MAXNES                                                           SETUP.142
         DO I = 1,max_type                                                       06NOV00.373
C                                                                                SETUP.144
C ------ FIRST CHECK FOR TYPE OF DATA REQUIRED                                   SETUP.145
C                                                                                SETUP.146
            IF (NTYPE(NN).EQ.I) THEN                                             SETUP.147
C                                                                                SETUP.148
C .. ELEVATION:                                                                  SETUP.149
C                                                                                SETUP.150
        IF (TERNAME(I).NE.'                    ') THEN                           SETUP.151
          IF (I.EQ.max_type .AND. G30SD) GO TO 6                                 06NOV00.374
                                                                                 SETUP.153
          LENGTH = ID2(I)/2                                                      SETUP.154
          IF (MOD(ID2(I),2).NE.0) LENGTH = LENGTH+1                              SETUP.155
#ifdef RECLENBYTE                                                                SETUP.156
          LENGTH = LENGTH*4                                                      SETUP.157
#endif                                                                           SETUP.158
          IUNIT = IVOL + (I-1)*2 + 1                                             SETUP.159
          OPEN(IUNIT,FILE=TERNAME(I),ACCESS='DIRECT',RECL=LENGTH,                SETUP.160
     >                                              STATUS='OLD')                SETUP.161
          PRINT 5,I, IUNIT, TERNAME(I), LENGTH                                   SETUP.162
 5        FORMAT(I2,'==> OPEN DIRECT-ACCESS: FORT.',I2.2,'  FILE=',              SETUP.163
     >            A20,'  RECORD LENGTH=',I6)                                     SETUP.164
 6        CONTINUE                                                               SETUP.165
        ENDIF                                                                    SETUP.166
                                                                                 06NOV00.375
C                                                                                06NOV00.376
C .. LAND-USE:                                                                   SETUP.168
C                                                                                SETUP.169
        IF (LNDNAME(I).NE.'                    '.AND.VEGTYPE.EQ.0) THEN          06NOV00.377
          LENGTH = (ID1(I)*ID2(I))/4                                             SETUP.171
          IF (MOD((ID1(I)*ID2(I)),4).NE.0) LENGTH = LENGTH+1                     SETUP.172
#ifdef RECLENBYTE                                                                SETUP.173
          LENGTH = LENGTH*4                                                      SETUP.174
#endif                                                                           SETUP.175
          IUNIT = IVOL + (I-1)*2                                                 SETUP.176
          OPEN(IUNIT,FILE=LNDNAME(I),ACCESS='DIRECT',RECL=LENGTH,                SETUP.177
     >                                              STATUS='OLD')                SETUP.178
          PRINT 5, I, IUNIT, LNDNAME(I), LENGTH                                  SETUP.179
        ENDIF                                                                    SETUP.180
C                                                                                SETUP.181
C .. LAND-WATER MASK: ONLY REQUIRED IF NOT USING NCAR GRAPHICS
C                                                                                SETUP.183
        IF (.NOT. IFEZFUG .OR. VEGESOIL) THEN                                    06NOV00.378
        IF (LWNAME(I).NE.'                    ') THEN                            SETUP.184
          LENGTH = NLONVEG(I)/4                                                  SETUP.185
          IF (MOD(NLONVEG(I),4).NE.0) LENGTH = LENGTH+1                          SETUP.186
#ifdef RECLENBYTE                                                                SETUP.187
          LENGTH = LENGTH*4                                                      SETUP.188
#endif                                                                           SETUP.189
          IUNIT = I + IVOL_lw                                                    06NOV00.379
          OPEN(IUNIT,FILE=LWNAME(I),ACCESS='DIRECT',RECL=LENGTH,                 SETUP.191
     >                                              STATUS='OLD')                SETUP.192
          PRINT 5, I, IUNIT, LWNAME(I), LENGTH                                   SETUP.193
        ENDIF                                                                    SETUP.194
        ENDIF
C                                                                                SETUP.195
C .. VEGETATION:                                                                 SETUP.196
C                                                                                SETUP.197
        IF (VEGONLY .OR. VEGESOIL) THEN                                          SETUP.198
        IF (VGNAME(I).NE.'                    ') THEN                            SETUP.199
          IF (I.LT.max_type) THEN                                                06NOV00.380
            LENG = NCATVEG*NLONVEG(I)                                            SETUP.201
          ELSE                                                                   SETUP.202
            LENG = NLONVEG(I)                                                    SETUP.203
          ENDIF                                                                  SETUP.204
          LENGTH = LENG/4                                                        SETUP.205
          IF (MOD(LENG,4).NE.0) LENGTH = LENGTH+1                                SETUP.206
#ifdef RECLENBYTE                                                                SETUP.207
          LENGTH = LENGTH*4                                                      SETUP.208
#endif                                                                           SETUP.209
          IUNIT = I + IVOL_vg                                                    06NOV00.381
          OPEN(IUNIT,FILE=VGNAME(I),ACCESS='DIRECT',RECL=LENGTH,                 SETUP.211
     >                                              STATUS='OLD')                SETUP.212
          PRINT 5, I, IUNIT, VGNAME(I), LENGTH                                   SETUP.213
        ENDIF                                                                    SETUP.214
        ENDIF                                                                    SETUP.215
C                                                                                SETUP.216
C .. SOIL:                                                                       SETUP.217
C                                                                                SETUP.218
        IF (.NOT. VEGONLY .AND. VEGESOIL) THEN                                   SETUP.219
        IF (SONAME(I).NE.'                    ') THEN                            SETUP.220
          IF (I.LT.max_type) THEN                                                06NOV00.382
            LENG = NCATSOIL*NLONSOIL(I)                                          SETUP.222
          ELSE                                                                   SETUP.223
            LENG = NLONSOIL(I)                                                   SETUP.224
          ENDIF                                                                  SETUP.225
          LENGTH = LENG/4                                                        SETUP.226
          IF (MOD(LENG,4).NE.0) LENGTH = LENGTH+1                                SETUP.227
#ifdef RECLENBYTE                                                                SETUP.228
          LENGTH = LENGTH*4                                                      SETUP.229
#endif                                                                           SETUP.230
          IUNIT = I + IVOL_so                                                    06NOV00.383
          OPEN(IUNIT,FILE=SONAME(I),ACCESS='DIRECT',RECL=LENGTH,                 SETUP.232
     >                                              STATUS='OLD')                SETUP.233
          PRINT 5, I, IUNIT, SONAME(I), LENGTH                                   SETUP.234
        ENDIF                                                                    SETUP.235
        ENDIF                                                                    SETUP.236
C                                                                                SETUP.237
            ENDIF                                                                SETUP.238
         END DO                                                                  SETUP.239
      END DO                                                                     SETUP.240
C
C .. LAND-USE AGAIN: IF 10 MIN LANDUSE FILE IS NOT OPENED, OPEN IT
C
      IF (VEGTYPE.EQ.0) THEN                                                     06NOV00.384
      INQUIRE (IVOL+4,OPENED=OPENED)                                             06NOV00.385
      IF (.NOT.OPENED) THEN                                                      06NOV00.386
          LENGTH = (ID1(3)*ID2(3))/4                                             06NOV00.387
          IF (MOD((ID1(3)*ID2(3)),4).NE.0) LENGTH = LENGTH+1
#ifdef RECLENBYTE
          LENGTH = LENGTH*4
#endif 
          OPEN(IVOL+4,FILE=LNDNAME(3),ACCESS='DIRECT',RECL=LENGTH,               06NOV00.388
     >                                            STATUS='OLD')
          PRINT 7, LNDNAME(3), LENGTH
      ENDIF
 7    FORMAT(' 3','==> OPEN DIRECT-ACCESS: FORT.24','  FILE=',
     >            A20,'  RECORD LENGTH=',I6)
      ENDIF                                                                      06NOV00.389
C                                                                                SETUP.241
C .. 12 MONTHS' VEGETATION FRACTION-10M RESOLUTION (6*360=2160):                 SETUP.242
C                                                                                SETUP.243
      IF (.NOT. VEGONLY .AND. VEGESOIL) THEN                                     SETUP.244
      IF (VFNAME.NE.'                    ') THEN                                 SETUP.245
        LENGTH = 12*2160/4                                                       SETUP.246
#ifdef RECLENBYTE                                                                SETUP.247
        LENGTH = LENGTH*4                                                        SETUP.248
#endif                                                                           SETUP.249
        OPEN(IVOL_vf,FILE=VFNAME,ACCESS='DIRECT',RECL=LENGTH,                    06NOV00.390
     >                                         STATUS='OLD')                     SETUP.252
        PRINT 5, I, IVOL_vf, VFNAME, LENGTH                                      06NOV00.391
      ENDIF                                                                      SETUP.254
C                                                                                SETUP.255
C .. ANNUAL MEAN DEEP SOIL TEMPERATURE (1-DEGREE RESOLUTION):                    SETUP.256
C                                                                                SETUP.257
      IF (TSNAME.NE.'                    ') THEN                                 SETUP.258
        LENGTH = 360/2                                                           SETUP.259
#ifdef RECLENBYTE                                                                SETUP.260
        LENGTH = LENGTH*4                                                        SETUP.261
#endif                                                                           SETUP.262
        OPEN(IVOL_ts,FILE=TSNAME,ACCESS='DIRECT',RECL=LENGTH,                    06NOV00.392
     >                                         STATUS='OLD')                     SETUP.265
        PRINT 5, I, IVOL_ts, TSNAME, LENGTH                                      06NOV00.393
      ENDIF                                                                      SETUP.267
      ENDIF                                                                      SETUP.268
C  ...................................................................           SETUP.269
C                                                                                SETUP.270
C .. OPEN FORT.MUNIT FOR OUTPUT:                                                 SETUP.271
C                                                                                SETUP.272
      DO MD = 1,MAXNES                                                           SETUP.273
       IF (ISMTHTR.LE.3) THEN                                                    23DEC04.15
        WRITE(FILENAME,'(''TERRAIN_DOMAIN'',I1)') MD                             SETUP.274
        IF (MAXNES.GE.10) WRITE(FILENAME,'(''TERRAIN_DOMAIN'',I2)') MD           SETUP.275
       ELSE                                                                      23DEC04.16
        WRITE(FILENAME,'(''TERRSMT_DOMAIN'',I1)') MD                             23DEC04.17
        IF (MAXNES.GE.10) WRITE(FILENAME,'(''TERRSMT_DOMAIN'',I2)') MD           23DEC04.18
       ENDIF                                                                     23DEC04.19
        MUNIT = IVOL_mdl+MD                                                      06NOV00.394
        OPEN(MUNIT,FILE=FILENAME,FORM='UNFORMATTED',STATUS='UNKNOWN')            SETUP.277
      END DO                                                                     SETUP.278
      END IF
C                                                                                SETUP.279
C   DEFINE THE PARAMETERS OF MAP BASED ON THE IPROJ:                             SETUP.280
C                                                                                SETUP.281
      XN = -1.0E36                                                               SETUP.282
      IF(PHIC.LT.0) THEN                                                         SETUP.283
         SIGN=-1.      ! SOUTH HEMESPHERE                                        SETUP.284
      ELSE                                                                       SETUP.285
         SIGN=1.       ! NORTH HEMESPHERE                                        SETUP.286
      ENDIF                                                                      SETUP.287
                                                                                 SETUP.288
      POLE = 90.                                                                 SETUP.289
      IF (IPROJ.EQ.NPROJ(1)) THEN                                                SETUP.290
      IF(ABS(TRUELAT1).GT.90.) THEN                                              SETUP.291
         TRUELAT1=60.                                                            SETUP.292
         TRUELAT2=30.                                                            SETUP.293
         TRUELAT1=SIGN*TRUELAT1                                                  SETUP.294
         TRUELAT2=SIGN*TRUELAT2                                                  SETUP.295
      ENDIF                                                                      SETUP.296
      IF (ABS(TRUELAT1-TRUELAT2) .GT. 1.E-1) THEN                                19DEC02.11
      XN = ALOG10(COS(TRUELAT1 / CONV)) -                                        SETUP.297
     *     ALOG10(COS(TRUELAT2 / CONV))                                          SETUP.298
      XN = XN/(ALOG10(TAN((45.0 -  SIGN*TRUELAT1/2.0) / CONV)) -                 SETUP.299
     *                 ALOG10(TAN((45.0 - SIGN*TRUELAT2/2.0) / CONV)))           SETUP.300
      ELSE                                                                       19DEC02.12
      XN=SIGN*SIN(TRUELAT1/CONV)                                                 19DEC02.13
      ENDIF                                                                      19DEC02.14
      PSI1=90.-SIGN*TRUELAT1                                                     SETUP.301
         PROJECT='LC'                                                            SETUP.302
      ELSE IF (IPROJ.EQ.NPROJ(2)) THEN                                           SETUP.303
         XN = 1.0                                                                SETUP.304
      IF(ABS(TRUELAT1).GT.90.) THEN                                              SETUP.305
      TRUELAT1=60.                                                               SETUP.306
      TRUELAT2=0.                                                                SETUP.307
      TRUELAT1=SIGN*TRUELAT1                                                     SETUP.308
      TRUELAT2=SIGN*TRUELAT2                                                     SETUP.309
      ENDIF                                                                      SETUP.310
C PSI1 IS THE PSEUDO-LATITUDE                                                    SETUP.311
      PSI1=90.-SIGN*TRUELAT1                                                     SETUP.312
         PROJECT='ST'                                                            SETUP.313
      ELSE IF (IPROJ.EQ.NPROJ(3)) THEN                                           SETUP.314
         XN = 0.                                                                 SETUP.315
      IF(ABS(TRUELAT1).GT.90.) THEN                                              SETUP.316
      TRUELAT1=0.                                                                SETUP.317
      TRUELAT2=0.                                                                SETUP.318
      ENDIF                                                                      SETUP.319
      IF(TRUELAT1.NE.0.) THEN                                                    SETUP.320
      PRINT *,'TERRAIN AND MAPDRV ONLY SUPPORT MERCATOR PROJECTION               SETUP.321
     *AT 0 DEGREE TURE LATITUDE, RESET TRUELAT1=0.'                              SETUP.322
      TRUELAT1=0.                                                                SETUP.323
      ENDIF                                                                      SETUP.324
      PSI1=0.                                                                    SETUP.325
         PROJECT='ME'                                                            SETUP.326
      END IF                                                                     SETUP.327
C                                                                                SETUP.328
      PSI1 = PSI1/CONV                                                           SETUP.329
      IF (PHIC.LT.0.) THEN                                                       SETUP.330
        PSI1 = -PSI1                                                             SETUP.331
        POLE = -POLE                                                             SETUP.332
      ENDIF                                                                      SETUP.333
C                                                                                SETUP.334
C--------CALCULATE R                                                             SETUP.335
C                                                                                SETUP.336
      IF (IPROJ.NE.NPROJ(3)) THEN                                                SETUP.337
         PSX = (POLE-PHIC)/CONV                                                  SETUP.338
         IF (IPROJ.EQ.NPROJ(1)) THEN                                             SETUP.339
            CELL  = A*SIN(PSI1)/XN                                               SETUP.340
            CELL2 = (TAN(PSX/2.))/(TAN(PSI1/2.))                                 SETUP.341
         ENDIF                                                                   SETUP.342
         IF (IPROJ.EQ.NPROJ(2)) THEN                                             SETUP.343
            CELL  = A*SIN(PSX)/XN                                                SETUP.344
            CELL2 = (1. + COS(PSI1))/(1. + COS(PSX))                             SETUP.345
         ENDIF                                                                   SETUP.346
         R = CELL*(CELL2)**XN                                                    SETUP.347
         XCNTR = 0.0                                                             SETUP.348
         YCNTR = -R                                                              SETUP.349
      ENDIF                                                                      SETUP.350
C-----FOR MERCATOR PROJECTION, THE PROJECTION IS TRUE AT LAT AT PHI1             SETUP.351
      IF (IPROJ.EQ.NPROJ(3)) THEN                                                SETUP.352
         C2     = A*COS(PSI1)                                                    SETUP.353
         XCNTR  = 0.0                                                            SETUP.354
         PHICTR = PHIC/CONV                                                      SETUP.355
         CELL   = COS(PHICTR)/(1.0+SIN(PHICTR))                                  SETUP.356
         YCNTR  = - C2*ALOG(CELL)                                                SETUP.357
      ENDIF                                                                      SETUP.358
      IF(IPRINT.EQ.1) THEN                                                       SETUP.359
      PRINT 10,XCNTR,YCNTR                                                       SETUP.360
10    FORMAT(1X,'COARSE GRID CENTER IS AT X =',F8.1,' KM AND Y = ',              SETUP.361
     1       F8.1,' KM ')                                                        SETUP.362
      ENDIF                                                                      SETUP.363
C                                                                                SETUP.364
C   CHECK THE COMPATIBILITY OF NEST DOMAINS WITH THE COARSE DOMAINS              SETUP.365
C     AND CALCULATE THE IRATIOS, XNORTHS, XSOUTHS, XWESTS AND XEASTS             06NOV00.395
C                                                                                SETUP.367
C     A) EXTENDING THE COARSE DOMAIN IF IEXP = .T.                               SETUP.368
C                                                                                SETUP.369
        IXEX = NESTIX(1)                                                         SETUP.370
        JXEX = NESTJX(1)                                                         SETUP.371
        IOFFST = 0                                                               SETUP.372
        JOFFST = 0                                                               SETUP.373
      IF (IEXP) THEN                                                             SETUP.374
        INCR = INT(AEXP/DIS(1) + 1.001)                                          SETUP.375
        IXEX = NESTIX(1) + INCR*2                                                SETUP.376
        JXEX = NESTJX(1) + INCR*2                                                SETUP.377
        IOFFST = INCR                                                            SETUP.378
        JOFFST = INCR                                                            SETUP.379
      IF (IFFUG(1)) THEN                                                         SETUP.380
         DO NF = 1,NDFUG(1)                                                      SETUP.381
            IFUG(NF,1) = IFUG(NF,1) + IOFFST                                     SETUP.382
            JFUG(NF,1) = JFUG(NF,1) + JOFFST                                     SETUP.383
         END DO                                                                  SETUP.384
      ENDIF                                                                      SETUP.385
C                                                                                SETUP.386
        PRINT 20,INCR                                                            SETUP.387
        PRINT 22,IXEX,JXEX                                                       SETUP.388
20    FORMAT(/1X,'$$$$ GRID IS EXPANDED BY ',I4,                                 06NOV00.396
     1                         ' GRID POINTS ON EACH SIDE $$$$')                 06NOV00.397
22    FORMAT(6X,'EXPANDED GRID SIZE IS (',I5,',',I5,')')                         06NOV00.398
      ENDIF                                                                      SETUP.392
C-----CENTER OF GRID IN THE COARSE DOMAIN                                        SETUP.393
      CNTRJ0 = FLOAT(JXEX+1)/2.                                                  SETUP.394
      CNTRI0 = FLOAT(IXEX+1)/2.                                                  SETUP.395
C     PRINT 21,NESTIX(1),NESTJX(1)                                               06NOV00.399
21    FORMAT(2X,'COARSE DOMAIN SIZE (IX,JX) =',2I5)                              SETUP.397
C                                                                                SETUP.398
C  MIX, MJX ARE USED IN SUB. TFUDGE:                                             SETUP.399
      MIX = IXEX                                                                 SETUP.400
      MJX = JXEX                                                                 SETUP.401
      DO 23 M = 1, MAXNES                                                        SETUP.402
      MIX = MAX0(NESTIX(M),MIX)                                                  SETUP.403
      MJX = MAX0(NESTJX(M),MJX)                                                  SETUP.404
23    CONTINUE                                                                   SETUP.405
      PRINT 24, MIX,MJX                                                          SETUP.406
24    FORMAT(1X,'THE MAXIMUM DIMENSION IS   (',I5,',',I5,')')                    06NOV00.400
C                                                                                SETUP.408
C .. CHECK THE DIMENSION DECLARED IN PARAME.INCL:                                SETUP.409
      IF (IIMX.LT.MIX .OR. JJMX.LT.MJX) THEN                                     SETUP.410
        PRINT 25,IIMX,JJMX                                                       SETUP.411
25      FORMAT(/1X,'THE DIMENSIONS DECLARED IN PARAME.INCL:',                    06NOV00.401
     -           ' IIMX=',I4,'  JJMX=',I4,' ARE TOO SMALL.')                     SETUP.413
        STOP 25                                                                  SETUP.414
      ENDIF                                                                      SETUP.415
C                                                                                SETUP.416
C  CHECK IF POLE IS INSIDE THE DOMAIN OR NOT FOR LAMBERT PROJECTION:             SETUP.417
      HDSIZE = (IXEX-1)*DIS(1)/2.                                                SETUP.418
      IF (HDSIZE.GT.ABS(YCNTR) .AND. IPROJ.EQ.NPROJ(1))THEN                      SETUP.419
         PRINT *,'-------------------------------------------------'             SETUP.420
         PRINT *,'HALF DOMAIN SIZE IN Y-DIRECTION = ',HDSIZE                     SETUP.421
         PRINT *,'    DISTANCE FROM CNTER TO POLE = ',ABS(YCNTR)                 SETUP.422
         PRINT *,'NOT MAKE SENSE WITH THE POLE IS INSIDE THE DOMAIN '            SETUP.423
         PRINT *,'    FOR LAMBERT CONFORMAL PROJECTION!'                         SETUP.424
         PRINT *,'=== PLEASE RE-SPECIFY THE CENTER OR DOMAIN SIZE. ==='          SETUP.425
         STOP                                                                    SETUP.426
      ENDIF                                                                      SETUP.427
C                                                                                SETUP.428
C     B) CALCULATING THE IRATIOS, XNORTHS AND XEASTS:                            06NOV00.402
C                                                                                SETUP.430
      IRATIO(1) = 1                                                              SETUP.431
      NRATIO(1) = 1                                                              SETUP.432
      XSOUTH(1) = 1.                                                             SETUP.433
      XWEST(1)  = 1.                                                             SETUP.434
      XNORTH(1) = FLOAT(IXEX)                                                    SETUP.435
      XEAST(1)  = FLOAT(JXEX)                                                    SETUP.436
      XJC = (XEAST(1) + 1.0)/2.                                                  SETUP.437
      IF(IPRINT.EQ.1) THEN                                                       SETUP.438
      PRINT 27,XSOUTH(1),XWEST(1),XNORTH(1),XEAST(1),DIS(1),                     SETUP.439
     1         IRATIO(1),NRATIO(1)                                               SETUP.440
 27   FORMAT(1X,'XSOUTH(1)= ',F6.1,2X,'XWEST(1)= ',F6.1,2X,                      SETUP.441
     1'XNORTH(1)= ',F6.1,2X,'XEAST(1)= ',F6.1,2X,'DIS(1)= ',F6.1,                SETUP.442
     22X,'IRATIO(1)= ',I3,2X,'NRATIO(1)= ',I3)                                   SETUP.443
      ENDIF                                                                      SETUP.444
C                                                                                SETUP.445
      MISMATCH1 = 0                                                              06NOV00.403
      MISMATCH  = 0                                                              06NOV00.404
                                                                                 SETUP.447
      DO 30 NM = 2, MAXNES                                                       SETUP.448
C                                                                                SETUP.449
C  DOMAINS' CONSISTENCY CHECK:                                                   SETUP.450
C                                                                                SETUP.451
      NMC = NUMNC(NM)                                                            06NOV00.405
C                                                                                SETUP.453
C   MOTHER DOMAIN ID CANNOT BE DOMAIN ITSELF                                     SETUP.454
C                                                                                SETUP.455
      IF (NMC.EQ.NM) THEN                                                        SETUP.456
         MISMATCH = MISMATCH + 1                                                 SETUP.457
         PRINT 28, NMC, NM                                                       SETUP.458
28      FORMAT(2X,'MOTHER DOMAIN ID (NUMNC) CANNOT BE DOMAIN ITSELF.',           SETUP.459
     1   'YOU HAVE SET NUMNC = ',I2,' FOR DOMAIN ',I2)                           SETUP.460
         GO TO 30                                                                SETUP.461
      ENDIF                                                                      SETUP.462
C                                                                                SETUP.463
C   NEST GRID DISTANCE AND IRATIO CHECK                                          SETUP.464
C                                                                                SETUP.465
      NDR = NINT(DIS(NMC)/DIS(NM))                                               SETUP.466
      IF (ABS(DIS(NMC)-NDR*DIS(NM)) .GT. 0.1*DIS(NM)) THEN                       SETUP.467
         MISMATCH = MISMATCH + 1                                                 SETUP.468
         PRINT 29, NM,NMC                                                        SETUP.469
         PRINT 31,NM,DIS(NM),NMC,DIS(NMC)                                        SETUP.470
29      FORMAT(2X,'DOMAIN ',I2,' HAS INCORRECT GRID SIZE ',                      SETUP.471
     1'  IT HAS TO BE THE MULTIPLE OF DOMAIN ',I2)                               SETUP.472
31      FORMAT(2X,'DOMAIN ',I2,' GRID SIZE= ',F8.3,' KM',                        SETUP.473
     1         '  DOMAIN ',I2,' GRID SIZE= ',F8.3,' KM')                         SETUP.474
        GO TO 30                                                                 SETUP.475
      ENDIF                                                                      SETUP.476
      IRATIO(NM) = NINT(DIS(NMC)/DIS(NM))                                        SETUP.477
C                                                                                SETUP.478
C .. PASSED THE RATIO CHECK, NOW RESET THE GRID DISTANCE FOR THE NEST:           SETUP.479
C                                                                                SETUP.480
      DIS(NM) = DIS(NMC)/FLOAT(IRATIO(NM))                                       SETUP.481
      IF(IPRINT.EQ.1) PRINT 38, NM, DIS(NM)                                      06NOV00.414
 38   FORMAT(/1X,'NM=',I3,'  DIS=',F8.2)                                         06NOV00.415
      NRATIO(NM) = NINT(DIS(1)/DIS(NM))                                          SETUP.483
C                                                                                SETUP.484
C   MAKE SURE THE 4 CORNER POINTS OF NEST DOMAINS ARE ON THE                     SETUP.485
C   PREVIOUS DOMAIN GRID-POINTS                                                  SETUP.486
C                                                                                SETUP.487
      IF (MOD((NESTIX(NM)-1),IRATIO(NM)).NE.0) THEN                              SETUP.488
        MISMATCH = MISMATCH + 1                                                  SETUP.489
        IIMXN = (INT(FLOAT(NESTIX(NM)-1)/IRATIO(NM))+1)*IRATIO(NM) + 1           SETUP.490
        PRINT 32,NM,NESTIX(NM),IRATIO(NM),IIMXN                                  SETUP.491
32      FORMAT(2X,'NESTIX(',I2,')=',I4,' AND IRATIO=',I2,' DOES NOT MATC         SETUP.492
     1H, YOU MAY SET NESTIX TO ',I4)                                             SETUP.493
      ENDIF                                                                      SETUP.494
      IF (MOD((NESTJX(NM)-1),IRATIO(NM)).NE.0) THEN                              SETUP.495
        MISMATCH = MISMATCH + 1                                                  SETUP.496
        JJMXN = (INT(FLOAT(NESTJX(NM)-1)/IRATIO(NM))+1)*IRATIO(NM) + 1           SETUP.497
        PRINT 33,NM,NESTJX(NM),IRATIO(NM),JJMXN                                  SETUP.498
33      FORMAT(2X,'NESTJX(',I2,')=',I4,' AND IRATIO=',I2,' DOES NOT MATC         SETUP.499
     1H, YOU MAY SET NESTJX TO ',I4)                                             SETUP.500
      ENDIF                                                                      SETUP.501
C                                                                                SETUP.502
C--------CHECK NSTTYP AGAINST IRATIO                                             SETUP.503
C                                                                                SETUP.504
      IF(IPRINT.EQ.1)PRINT *, 'NSTTYP, DOMAIN ID = ', NSTTYP(NM),NM              06NOV00.416
      IF (NSTTYP(NM).EQ.2 .AND. IRATIO(NM).NE.3) THEN                            SETUP.506
         MISMATCH = MISMATCH + 1                                                 SETUP.507
         PRINT 42, NSTTYP(NM), IRATIO(NM)                                        SETUP.508
         GO TO 30                                                                SETUP.509
      END IF                                                                     SETUP.510
42    FORMAT(2X, 'TWO-WAY OPTION (NSTTYP=',I2,') MUST REQUIRE THE ',             SETUP.511
     1       'RATIO BETWEEN COARSE AND FINE GRID DISTANCE TO BE 3. ',            SETUP.512
     2       'YOU HAVE SELECTED THE GRID DISTANCES SO THAT THE RATIO '           SETUP.513
     3       'IS ',I2)                                                           SETUP.514
C                                                                                SETUP.515
C--------REDEFINE LOCATION OF LOWER LEFT CORNER OF FINE MESH (IN TERMS           SETUP.516
C        OF EXPANDED COARSE MESH - DOMAIN 1 INDICES) IF USING EXPANDED GRID      06NOV00.417
                                                                                 SETUP.518
       IF(IPRINT.EQ.1)PRINT 34,NM,NESTIX(NM),NESTJX(NM),DIS(NM),                 06NOV00.418
     1    NESTI(NM),NESTJ(NM),NUMNC(NM),IRATIO(NM),NRATIO(NM),IEXP               06NOV00.419
34     FORMAT(/1X,'DOMAIN ',I2,2X,'IX=',I4,2X,'JX=',I4,2X,                       SETUP.521
     1 'DS= ',F8.3,2X,'ICNS=',I4,2X,'JCNS=',I4,2X,                               SETUP.522
     2 'NUMNC= ',I2,2X,'IRATIO= ',I3,2X,'NRATIO= ',I3,2X,                        SETUP.523
     3 'IEXP= ',L2)                                                              SETUP.524
C                                                                                SETUP.525
      XDIS = 0.0                                                                 SETUP.526
      YDIS = 0.0                                                                 SETUP.527
      ND1 = NM                                                                   SETUP.528
      ND2 = NMC                                                                  SETUP.529
40    CONTINUE                                                                   SETUP.530
      XDIS = (NESTI(ND1)-1)*DIS(ND2) + XDIS                                      SETUP.531
      YDIS = (NESTJ(ND1)-1)*DIS(ND2) + YDIS                                      SETUP.532
      IF (ND2 .GT. 1) THEN                                                       SETUP.533
        ND1 = ND2                                                                SETUP.534
        ND2 = NUMNC(ND2)                                                         SETUP.535
        GO TO 40                                                                 SETUP.536
      ENDIF                                                                      SETUP.537
C                                                                                SETUP.538
       XSOUTH(NM) = XDIS/DIS(1) + FLOAT(IOFFST) + 1                              SETUP.539
       XWEST(NM)  = YDIS/DIS(1) + FLOAT(JOFFST) + 1                              SETUP.540
       XNORTH(NM) = XSOUTH(NM) + FLOAT(NESTIX(NM)-1)*DIS(NM)/DIS(1)              SETUP.541
       XEAST(NM)  =  XWEST(NM) + FLOAT(NESTJX(NM)-1)*DIS(NM)/DIS(1)              SETUP.542
                                                                                 SETUP.543
       IF(IPRINT.EQ.1) THEN                                                      06NOV00.420
       PRINT 35                                                                  SETUP.544
       PRINT 36,XSOUTH(NM),XWEST(NM),XNORTH(NM),XEAST(NM)                        SETUP.545
       ENDIF                                                                     06NOV00.421
35     FORMAT(2X,'COARSE MESH INDICES FOR THE 4 CORNER POINTS ARE')              SETUP.546
36     FORMAT(2X,'SOUTH=',F6.1,3X,'WEST=',F6.1,3X,'NORTH =',F6.1,3X,             SETUP.547
     1        'EAST = ',F6.1)                                                    SETUP.548
C                                                                                SETUP.549
C--------CHECK IF NEST IS TOO CLOSE TO MOTHER DOMAIN BOUNDARY                    06NOV00.422
C                                                                                06NOV00.423
       INORTH = NESTI(NM)+(NESTIX(NM)-1)/IRATIO(NM)                              06NOV00.424
       JEAST  = NESTJ(NM)+(NESTJX(NM)-1)/IRATIO(NM)                              06NOV00.425
       IF (NESTI(NM).LT.6 .OR. INORTH.GE.NESTIX(NMC)-5 .OR.                      06NOV00.426
     1     NESTJ(NM).LT.6 .OR.  JEAST.GE.NESTJX(NMC)-5) THEN                     06NOV00.427
         MISMATCH = MISMATCH + 1                                                 06NOV00.428
         PRINT 901, NM, NMC                                                      06NOV00.429
901      FORMAT(1X,'THE NEST ',I2,' IS TOO CLOSE TO THE BOUNDARY OF ',           06NOV00.430
     1         'THE MOTHER DOMAIN ',I2)                                          06NOV00.431
         PRINT *,'YOU NEED AT LEAST 5 GRID POINTS AWAY FROM THE ',               06NOV00.432
     1           'COARSE DOMAIN ON ALL SIDES'                                    06NOV00.433
         PRINT 902                                                               06NOV00.434
         PRINT 903, NESTI(NM),(NESTIX(NMC)-NESTI(NM)-6)*IRATIO(NM)+1             06NOV00.435
902      FORMAT(1X,'THE NEST MUST START AT LEAST AT NESTI, NESTJ = 6')           06NOV00.436
903      FORMAT(1X,'IF YOUR NEST STARTS AT NESTI = ',I3,' YOUR ',                06NOV00.437
     1         'DIMENSION FOR THE NEST CANNOT EXCEED NESTIX = ',I4)              06NOV00.438
         PRINT 904, NESTJ(NM),(NESTJX(NMC)-NESTJ(NM)-6)*IRATIO(NM)+1             06NOV00.439
904      FORMAT(1X,'IF YOUR NEST STARTS AT NESTJ = ',I3,' YOUR ',                06NOV00.440
     1         'DIMENSION FOR THE NEST CANNOT EXCEED NESTJX = ',I4)              06NOV00.441
       ENDIF                                                                     06NOV00.442
                                                                                 05DEC01.32
      IF (MISMATCH.GT.0) THEN                                                    05DEC01.33
        MISMATCH1 = MISMATCH1 + MISMATCH                                         05DEC01.34
        PRINT 39, MISMATCH1,NM-1,NM-2,MISMATCH                                   05DEC01.35
 39     FORMAT(/1X,'?? DOMAIN MISMATCH: MISMATCH1(ALL)=',I2,                     05DEC01.36
     1         ',',2X,'MISMATCH(DOMAINS',I3,' AND ',I3,')=',I2//)                05DEC01.37
      MISMATCH = 0                                                               05DEC01.38
      ENDIF                                                                      05DEC01.39
30     CONTINUE                                                                  SETUP.550
C                                                                                SETUP.551
       IF (MISMATCH1.GT.0) THEN                                                  06NOV00.443
       PRINT 49, MISMATCH1                                                       06NOV00.444
 49    FORMAT(//1X,'??? TERRAIN STOP IN SUBROUTINE SETUP DUE TO',                06NOV00.445
     1          ' INCORRECT NEST DOMAIN SET UP, MISMATCH1=',I3)                  06NOV00.446
       STOP 1111                                                                 SETUP.555
       ENDIF                                                                     SETUP.556
C                                                                                SETUP.557
       RETURN                                                                    SETUP.558
       END                                                                       SETUP.559
