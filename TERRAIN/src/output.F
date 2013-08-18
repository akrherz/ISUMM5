      SUBROUTINE OUTPUT(CT,CL,VC,SC,VFC,TGC,WLC,XLVC,XLSC,                       OUTPUT.1
     -                 IYY,JXX,IINC,PFLAG)                                       OUTPUT.2
C                                                                                OUTPUT.3
C   PURPOSE  :  CREATE V3 HEADER;                                                OUTPUT.4
C               CALCULATE CONSTANT FIELDS:                                       OUTPUT.5
C                         MAP SCALE FACTOR: DOT AND CROSS POINTS                 OUTPUT.6
C                         LAT AND LONG:     DOT AND CROSS POINTS                 OUTPUT.7
C                         CORIOLIS:         DOT POINT                            OUTPUT.8
C                                                                                OUTPUT.9
C   ON ENTRY :  IYY,JXX: THE DIMENSIONS OF THE DOMAIN IN INTEREST.               OUTPUT.10
C                  IINC: THE INPUT AND OUTPUT UNIT                               OUTPUT.11
C                 PFLAG: THE PRINT SWITCH. IF .TRUE., PRINT THE                  OUTPUT.12
C                        TERRAIN HEIGHT FIELDS AFTER THE REPLACEMENT.            OUTPUT.13
C                        IF .FALSE., DO NOT PRINT.                               OUTPUT.14
C                                                                                OUTPUT.15
C   ON EXIT :   COMPLETE TERRAIN OUTPUT FOR ONE DOMAIN                           OUTPUT.16
C                                                                                OUTPUT.17
C   CALLS :     SUBROUTINE OUTPUT CALLS THE SUBROUTINE TERRRESTIAL_INFO.         OUTPUT.18
C   CALLED BY : THE MAIN PROGRAM TERRAIN.                                        OUTPUT.19
C                                                                                OUTPUT.20
C   COMMENTS :                                                                   OUTPUT.21
C                                                                                OUTPUT.22
# include <parame.incl>                                                          06NOV00.109
# include <paramed.incl>                                                         06NOV00.110
# include <io.incl>                                                              06NOV00.111
# include <option.incl>                                                          OUTPUT.23
# include <paramesv.incl>                                                        OUTPUT.24
# include <ltdata.incl>                                                          OUTPUT.25
# include <headerv3.incl>                                                        OUTPUT.26
# include <nestdmn.incl>                                                         OUTPUT.27
# include <maps.incl>                                                            OUTPUT.28
# include <namelist.incl>                                                        OUTPUT.29
# include <vs_cmn2.incl>                                                         OUTPUT.30
# include <vs_cmn1.incl>                                                         OUTPUT.31
C                                                                                OUTPUT.32
      DIMENSION CT(IYY,JXX), CL(IYY,JXX)                                         OUTPUT.33
      DIMENSION VC(IYY,JXX,NVC), SC(IYY,JXX,NSC), VFC(IYY,JXX,12)                OUTPUT.34
      DIMENSION TGC(IYY,JXX),  WLC(IYY,JXX),                                     OUTPUT.35
     &          XLVC(IYY,JXX), XLSC(IYY,JXX)                                     OUTPUT.36
      CHARACTER*10 NAMEP                                                         OUTPUT.37
      LOGICAL   PFLAG                                                            OUTPUT.38
      INTEGER   D2, BH_FLAG, EOT_FLAG                                            OUTPUT.39
C                                                                                OUTPUT.40
      NESTID = IINC - IVOL_mdl                                                   06NOV00.112
      NVW = IVW(NTYPE(NESTID))                                                   23DEC04.2
      IF(IPRINT.EQ.1) PRINT *,'==> CALL OUTPUT FOR DOMAIN ',NESTID               06NOV00.113
      IF(IPRINT.EQ.1) THEN                                                       OUTPUT.44
        PRINT *,'IYY,JXX = ',IYY,JXX                                             OUTPUT.45
        PRINT *,'IINC    = ',IINC                                                OUTPUT.46
      ENDIF                                                                      OUTPUT.47
      KSIGT = 6                                                                  OUTPUT.48
                                                                                 OUTPUT.49
C   BUILD V3 HEADER                                                              OUTPUT.50
                                                                                 OUTPUT.51
      DO J = 1,NUMPROGS                                                          OUTPUT.52
      DO I = 1,NUMINT                                                            OUTPUT.53
         BHI(I,J) = -999                                                         OUTPUT.54
         BHIC(I,J) = '  '                                                        OUTPUT.55
      END DO                                                                     OUTPUT.56
      END DO                                                                     OUTPUT.57
      DO J = 1,NUMPROGS                                                          OUTPUT.58
      DO I = 1,NUMREAL                                                           OUTPUT.59
         BHR(I,J) = -999.                                                        OUTPUT.60
         BHRC(I,J) = '  '                                                        OUTPUT.61
      END DO                                                                     OUTPUT.62
      END DO                                                                     OUTPUT.63
                                                                                 OUTPUT.64
C INTEGER HEADER                                                                 OUTPUT.65
                                                                                 OUTPUT.66
      BHI (1,1) = 1                                                              OUTPUT.67
      BHIC(1,1) = 'PROGRAM NAME : TERRAIN'                                       OUTPUT.68
      BHI (2,1) = 1                                                              OUTPUT.69
      BHIC(2,1) = 'TERRAIN VERSION 3 MM5 SYSTEM FORMAT EDITION NUMBER'           OUTPUT.70
      BHI (3,1) = 7                                                              23DEC04.3
      BHIC(3,1) = 'TERRAIN PROGRAM VERSION NUMBER'                               OUTPUT.72
      BHI (4,1) = 0                                                              23DEC04.4
      BHIC(4,1) = 'TERRAIN PROGRAM MINOR REVISION NUMBER'                        OUTPUT.74
                                                                                 OUTPUT.75
      BHI (5,1) = NESTIX(1)                                                      OUTPUT.76
      BHIC(5,1) = 'COARSE DOMAIN GRID DIMENSION IN I (N-S) DIRECTION'            OUTPUT.77
      BHI (6,1) = NESTJX(1)                                                      OUTPUT.78
      BHIC(6,1) = 'COARSE DOMAIN GRID DIMENSION IN J (E-W) DIRECTION'            OUTPUT.79
                                                                                 OUTPUT.80
      IF(IPROJ.EQ.NPROJ(1)) THEN                                                 OUTPUT.81
      BHI (7,1)  = 1                                                             OUTPUT.82
      ELSEIF(IPROJ.EQ.NPROJ(2)) THEN                                             OUTPUT.83
      BHI (7,1)  = 2                                                             OUTPUT.84
      ELSE                                                                       OUTPUT.85
      BHI (7,1) = 3                                                              OUTPUT.86
      ENDIF                                                                      OUTPUT.87
      BHIC(7,1)  = 'MAP PROJECTION. 1: LAMBERT CONFORMAL, 2: POLAR'//            OUTPUT.88
     1 ' STEREOGRAPHIC, 3: MERCATOR'                                             OUTPUT.89
                                                                                 OUTPUT.90
      IF(IEXP) THEN                                                              OUTPUT.91
         BHI (8,1)  = 1                                                          OUTPUT.92
      ELSE                                                                       OUTPUT.93
         BHI (8,1)  = 0                                                          OUTPUT.94
      ENDIF                                                                      OUTPUT.95
      BHIC( 8,1)= 'IS COARSE DOMAIN EXPANDED?, 1: YES, 0: NO'                    OUTPUT.96
      BHI ( 9,1)= IXEX                                                           OUTPUT.97
      BHIC( 9,1)= 'EXPANDED COARSE DOMAIN GRID DIMENSION IN I DIRECTION'         OUTPUT.98
      BHI (10,1)= JXEX                                                           OUTPUT.99
      BHIC(10,1)= 'EXPANDED COARSE DOMAIN GRID DIMENSION IN J DIRECTION'         OUTPUT.100
      BHI (11,1)= IOFFST                                                         OUTPUT.101
      BHIC(11,1)= 'GRID OFFSET IN I DIR DUE TO COARSE GRID EXPANSION'            OUTPUT.102
      BHI (12,1)= JOFFST                                                         OUTPUT.103
      BHIC(12,1)= 'GRID OFFSET IN J DIR DUE TO COARSE GRID EXPANSION'            OUTPUT.104
                                                                                 OUTPUT.105
      BHI (13,1)= NESTID                                                         OUTPUT.106
      BHIC(13,1)= 'DOMAIN ID'                                                    OUTPUT.107
      BHI (14,1)= NUMNC(NESTID)                                                  OUTPUT.108
      BHIC(14,1)= 'MOTHER DOMAIN ID'                                             OUTPUT.109
                                                                                 OUTPUT.110
      LVLID = 0                                                                  OUTPUT.111
      IF (NESTID.EQ.1) GO TO 2110                                                OUTPUT.112
      MID0=NESTID                                                                OUTPUT.113
2105  CONTINUE                                                                   OUTPUT.114
      MID1 = NUMNC(MID0)                                                         OUTPUT.115
      LVLID = LVLID + 1                                                          OUTPUT.116
      IF (MID1.EQ.1) GO TO 2110                                                  OUTPUT.117
      MID0 = MID1                                                                OUTPUT.118
      GO TO 2105                                                                 OUTPUT.119
2110  CONTINUE                                                                   OUTPUT.120
      BHI (15,1) = LVLID                                                         OUTPUT.121
      BHIC(15,1) = 'NEST LEVEL (0: COARSE MESH)'                                 OUTPUT.122
                                                                                 OUTPUT.123
      BHI (16,1) = NESTIX(NESTID)                                                OUTPUT.124
      BHIC(16,1) = 'DOMAIN GRID DIMENSION IN I DIRECTION'                        OUTPUT.125
      BHI (17,1) = NESTJX(NESTID)                                                OUTPUT.126
      BHIC(17,1) = 'DOMAIN GRID DIMENSION IN J DIRECTION'                        OUTPUT.127
      BHI (18,1) = NESTI(NESTID)                                                 OUTPUT.128
      BHIC(18,1) = 'I LOCATION IN THE MOTHER DOMAIN OF THE DOMAIN'               OUTPUT.129
     &           //' POINT (1,1)'                                                OUTPUT.130
      BHI (19,1) = NESTJ(NESTID)                                                 OUTPUT.131
      BHIC(19,1) = 'J LOCATION IN THE MOTHER DOMAIN OF THE DOMAIN'               OUTPUT.132
     &           //' POINT (1,1)'                                                OUTPUT.133
      BHI (20,1) = NRATIO(NESTID)                                                OUTPUT.134
      BHIC(20,1) = 'DOMAIN GRID SIZE RATIO WITH RESPECT TO COARSE'               OUTPUT.135
     &           //' DOMAIN'                                                     OUTPUT.136
      BHI (21,1) = IRATIO(NESTID)                                                OUTPUT.137
      BHIC(21,1) = 'DOMAIN GRID SIZE RATIO WITH RESPECT TO MOTHER'               OUTPUT.138
     &           //' DOMAIN'                                                     OUTPUT.139
      BHI (22,1) = ISMTHTR                                                       OUTPUT.140
      BHIC(22,1) = 'SMOOTHER (1: 1-2-1, 2:SMOOTHER-DESMOOTHER)'                  OUTPUT.141
                                                                                 OUTPUT.142
      BHI (23,1) = 7                                                             OUTPUT.143
      BHIC(23,1) = 'OLD  13-CATEGORY LAND USE: WATER CATEGORY'                   OUTPUT.144
      IF (VEGESOIL) THEN                                                         OUTPUT.145
       IF (NVC.EQ.25) THEN                                                       OUTPUT.146
         BHI (23,1) = IVW(NTYPE(NESTID))                                         23DEC04.5
         BHIC(23,1) = 'USGS 25-CATEGORY LAND USE: WATER CATEGORY'                OUTPUT.148
       ELSE IF (NVC.EQ.17) THEN                                                  OUTPUT.149
         BHI (23,1) = IVW(NTYPE(NESTID))                                         23DEC04.6
         BHIC(23,1) = 'SiB  17-CATEGORY LAND USE: WATER CATEGORY'                OUTPUT.151
       END IF                                                                    OUTPUT.152
      END IF                                                                     OUTPUT.153

      BHI (24,1) = NSTTYP(NESTID)
      IF (NESTID.EQ.1) BHI (24,1) = 1
      BHIC(24,1) = 'IS THIS DOMAIN A ONE-WAY OR TWO-WAY NEST?'
     &           //' 1: 1-WAY, 2: 2-WAY. 1 FOR DOMAIN 1'
                                                                                 OUTPUT.154
      BHR (1,1)  = DIS(1)*1000.                                                  OUTPUT.155
      BHRC(1,1)  = 'COARSE DOMAIN GRID DISTANCE (m)'                             OUTPUT.156
      BHR (2,1)  = PHIC                                                          OUTPUT.157
      BHRC(2,1)  = 'COARSE DOMAIN CENTER LATITUDE (degree)'                      OUTPUT.158
      BHR (3,1)  = XLONC                                                         OUTPUT.159
      BHRC(3,1)  = 'COARSE DOMAIN CENTER LONGITUDE (degree)'                     OUTPUT.160
      BHR (4,1)  = XN                                                            OUTPUT.161
      BHRC(4,1)  = 'CONE FACTOR'                                                 OUTPUT.162
      BHR (5,1)  = TRUELAT1                                                      OUTPUT.163
      BHRC(5,1)  = 'TRUE LATITUDE 1 (degree)'                                    OUTPUT.164
      BHR (6,1)  = TRUELAT2                                                      OUTPUT.165
      BHRC(6,1)  = 'TRUE LATITUDE 2 (degree)'                                    OUTPUT.166
      BHR (7,1)  = POLE                                                          OUTPUT.167
      BHRC(7,1)  = 'POLE POSITION IN DEGREE LATITUDE'                            OUTPUT.168
      BHR (8,1)  = AEXP*1000.                                                    OUTPUT.169
C     BHR (8,1)  = 360.*1000.                                                    OUTPUT.170
      BHRC(8,1)  = 'APPROX EXPANSION (m)'                                        OUTPUT.171
                                                                                 OUTPUT.172
      BHR (9,1) = DIS(NESTID)*1000.                                              OUTPUT.173
      BHRC(9,1) = 'GRID DISTANCE (m) OF THIS DOMAIN'                             OUTPUT.174
      IF (NESTID.EQ.1) THEN                                                      OUTPUT.175
         BHR (10,1) = 1.                                                         OUTPUT.176
         BHR (11,1) = 1.                                                         OUTPUT.177
         BHR (12,1) = NESTIX(1)                                                  OUTPUT.178
         BHR (13,1) = NESTJX(1)                                                  OUTPUT.179
      ELSE                                                                       OUTPUT.180
         BHR (10,1) = XSOUTH(NESTID) - FLOAT(BHI(11,1))                          OUTPUT.181
         BHR (11,1) = XWEST(NESTID)  - FLOAT(BHI(12,1))                          OUTPUT.182
         BHR (12,1) = XNORTH(NESTID) - FLOAT(BHI(11,1))                          OUTPUT.183
         BHR (13,1) = XEAST(NESTID)  - FLOAT(BHI(12,1))                          OUTPUT.184
      END IF                                                                     OUTPUT.185
      BHRC(10,1) = 'I LOCATION IN THE COARSE DOMAIN'                             OUTPUT.186
     1           //' OF THE DOMAIN POINT (1,1)'                                  OUTPUT.187
      BHRC(11,1) = 'J LOCATION IN THE COARSE DOMAIN'                             OUTPUT.188
     1           //' OF THE DOMAIN POINT (1,1)'                                  OUTPUT.189
      BHRC(12,1) = 'I LOCATION IN THE COARSE DOMAIN'                             OUTPUT.190
     1           //' OF THE DOMAIN POINT (IX,JX)'                                OUTPUT.191
      BHRC(13,1) = 'J LOCATION IN THE COARSE DOMAIN'                             OUTPUT.192
     1           //' OF THE DOMAIN POINT (IX,JX)'                                OUTPUT.193
                                                                                 OUTPUT.194
      BHR (14,1) = CENTER(NTYPE(NESTID))                                         OUTPUT.195
      BHRC(14,1) = 'TERRAIN DATA RESOLUTION (in degree)'                         OUTPUT.196
      BHR (15,1) = CENTER(NTYPE(NESTID))                                         OUTPUT.197
      BHRC(15,1) = 'LANDUSE DATA RESOLUTION (in degree)'                         OUTPUT.198
                                                                                 OUTPUT.199
      IF (IFTER) THEN                                                            OUTPUT.200
      REWIND IINC                                                                OUTPUT.201
C                                                                                OUTPUT.202
C   READ IN FIELDS ALREADY COMPUTED                                              OUTPUT.203
C                                                                                OUTPUT.204
C     READ(IINC) MIF,MRF,MIFC,MRFC                                               OUTPUT.205
                                                                                 OUTPUT.206
      READ(IINC) CT                                                              OUTPUT.207
      READ(IINC) CL                                                              OUTPUT.208
                                                                                 OUTPUT.209
      IF (VEGESOIL) THEN                                                         OUTPUT.210
        IF (.NOT.VEGONLY) THEN                                                   OUTPUT.211
          DO IC = 1,NVC                                                          OUTPUT.212
            READ(IINC) TGC                                                       OUTPUT.213
            CALL EQUATE(TGC,VC (1,1,IC),IYY ,JXX , 1,'VEGECATC')                 OUTPUT.214
          END DO                                                                 OUTPUT.215
          DO IC = 1,NSC                                                          OUTPUT.216
            READ(IINC) TGC                                                       OUTPUT.217
            CALL EQUATE(TGC,SC (1,1,IC),IYY ,JXX , 1,'SOILCATC')                 OUTPUT.218
          END DO                                                                 OUTPUT.219
          DO IC = 1,12                                                           OUTPUT.220
            READ(IINC) TGC                                                       OUTPUT.221
            CALL EQUATE(TGC,VFC(1,1,IC),IYY ,JXX , 1,'VEGEFRCC')                 OUTPUT.222
          END DO                                                                 OUTPUT.223
          READ(IINC) TGC                                                         OUTPUT.224
          READ(IINC) WLC                                                         OUTPUT.225
C         READ(IINC) XLVC                                                        OUTPUT.226
C         CALL EQUATE(XLVC,CL,IYY,JXX,1,'LAND USE')                              OUTPUT.227
          READ(IINC) XLSC                                                        OUTPUT.228
        ENDIF                                                                    OUTPUT.229
      ENDIF                                                                      OUTPUT.230
                                                                                 OUTPUT.231
      REWIND IINC                                                                OUTPUT.232
                                                                                 OUTPUT.240
C     PRINT *, 'HEADER FROM OUTPUT'                                              OUTPUT.241
C     DO J = 1, NUMPROGS                                                         OUTPUT.242
C     DO I = 1, NUMINT                                                           OUTPUT.243
C        IF (BHI(I,J).NE.-999) PRINT 15,I,J,BHI(I,J),BHIC(I,J)                   OUTPUT.244
C15      FORMAT('BHI(',I3,',',I1,') = ',I10,' : ',A80)                           OUTPUT.245
C     ENDDO                                                                      OUTPUT.246
C     DO I = 1, NUMREAL                                                          OUTPUT.247
C        IF (BHR(I,J).NE.-999.) PRINT 16,I,J,BHR(I,J),BHRC(I,J)                  OUTPUT.248
C16      FORMAT('BHR(',I3,',',I1,') =',F11.4,' : ',A80)                          OUTPUT.249
C     ENDDO                                                                      OUTPUT.250
C     ENDDO                                                                      OUTPUT.251
                                                                                 OUTPUT.252
C WRITE FINAL OUTPUT                                                             OUTPUT.253
C                                                                                OUTPUT.254
C----- DEFINE HEADER FLAGS, AND FIELD DIMENSIONS                                 OUTPUT.255
C                                                                                OUTPUT.256
      BH_FLAG = 0                                                                OUTPUT.257
      EOT_FLAG = 2                                                               OUTPUT.258
      D2 = 2                                                                     OUTPUT.259
C                                                                                OUTPUT.260
      XTIME = 0.0                                                                OUTPUT.261
      CURRENT_DATE = '0000-00-00_00:00:00'                                       OUTPUT.262
                                                                                 OUTPUT.263
      WRITE(IINC) BH_FLAG                                                        OUTPUT.264
      WRITE(IINC) BHI,BHR,BHIC,BHRC                                              OUTPUT.265
                                                                                 OUTPUT.266
C TERRAIN                                                                        OUTPUT.267
                                                                                 OUTPUT.268
      CALL WRITE_FIELDREC( IINC,D2,CT,XTIME,                                     OUTPUT.269
     +                     'TERRAIN',CURRENT_DATE,                               OUTPUT.270
     +                     'C','YX','m',                                         OUTPUT.271
     +                     'TERRAIN',                                            OUTPUT.272
     +                     IYY,JXX,1)                                            OUTPUT.273
                                                                                 OUTPUT.274
C LANDUSE                                                                        OUTPUT.275
                                                                                 OUTPUT.276
      CALL WRITE_FIELDREC( IINC,D2,CL,XTIME,                                     OUTPUT.277
     +                     'LAND USE',CURRENT_DATE,                              OUTPUT.278
     +                     'C','YX','category',                                  OUTPUT.279
     +                     'LAND USE',                                           OUTPUT.280
     +                     IYY,JXX,1)                                            OUTPUT.281
                                                                                 OUTPUT.282
      IF (VEGESOIL .AND. .NOT.VEGONLY) THEN                                      OUTPUT.283
          IF (IEXTRA) THEN                                                       OUTPUT.284
                                                                                 OUTPUT.285
C VEGETATION CATEGORY PER GRID PER TYPE                                          OUTPUT.286
                                                                                 OUTPUT.287
            DO IC = 1,NVC                                                        OUTPUT.288
              CALL EQUATE(VC(1,1,IC),CT,IYY,JXX,1,'VEGECATC')                    OUTPUT.289
              WRITE(NAME(1:9),9900) IC                                           06NOV00.115
              IF(IPRINT.EQ.1) PRINT *, 'FIELD = ', LLBV(IC)                      06NOV00.116
              CALL WRITE_FIELDREC( IINC,D2,CT,XTIME,                             OUTPUT.292
     +                            NAME,CURRENT_DATE,                             OUTPUT.293
     +                            'C','YX','%',                                  OUTPUT.294
     +                            LLBV(IC),                                      OUTPUT.295
     +                            IYY,JXX,1)                                     OUTPUT.296
            END DO                                                               OUTPUT.297
9900        FORMAT('VEGCAT',I2.2,' ')                                            06NOV00.117
                                                                                 OUTPUT.299
C SOIL CATEGORY PER GRID PER TYPE                                                OUTPUT.300
                                                                                 OUTPUT.301
            DO IC = 1,NSC                                                        OUTPUT.302
              CALL EQUATE(SC(1,1,IC),CT,IYY,JXX,1,'SOILCATC')                    OUTPUT.303
              WRITE(NAME(1:9),9910) IC                                           06NOV00.118
              CALL WRITE_FIELDREC( IINC,D2,CT,XTIME,                             OUTPUT.305
     +                            NAME,CURRENT_DATE,                             OUTPUT.306
     +                            'C','YX','%',                                  OUTPUT.307
     +                            LLBS(IC),                                      OUTPUT.308
     +                            IYY,JXX,1)                                     OUTPUT.309
            END DO                                                               OUTPUT.310
9910        FORMAT('SOICAT',I2.2,' ')                                            06NOV00.119
                                                                                 OUTPUT.312
          END IF                                                                 OUTPUT.313
                                                                                 OUTPUT.314
C VEGETATION FRACTION FOR EACH MONTH                                             OUTPUT.315
                                                                                 OUTPUT.316
            DO IC = 1,12                                                         OUTPUT.317
              CALL EQUATE(VFC(1,1,IC),CT,IYY,JXX,1,'VEGEFRAC')                   OUTPUT.318
              WRITE(NAME(1:9),9920) IC                                           06NOV00.120
              CALL WRITE_FIELDREC( IINC,D2,CT,XTIME,                             OUTPUT.320
     +                            NAME,CURRENT_DATE,                             OUTPUT.321
     +                            'C','YX','%',                                  OUTPUT.322
     +                            LLBF(IC),                                      OUTPUT.323
     +                            IYY,JXX,1)                                     OUTPUT.324
            END DO                                                               OUTPUT.325
9920        FORMAT('VEGFRC',I2.2,' ')                                            06NOV00.121
                                                                                 OUTPUT.327
C DEEP SOIL TEMP                                                                 OUTPUT.328
                                                                                 OUTPUT.329
          CALL WRITE_FIELDREC( IINC,D2,TGC,XTIME,                                OUTPUT.330
     +                         'TEMPGRD',CURRENT_DATE,                           OUTPUT.331
     +                         'C','YX','K',                                     OUTPUT.332
     +                         'ANNUAL DEEP SOIL TEMPERATURE',                   OUTPUT.333
     +                         IYY,JXX,1)                                        OUTPUT.334
                                                                                 OUTPUT.335
C LAND MASK                                                                      OUTPUT.336
                                                                                 OUTPUT.337
          CALL WRITE_FIELDREC( IINC,D2,WLC,XTIME,                                OUTPUT.338
     +                         'LANDMASK',CURRENT_DATE,                          OUTPUT.339
     +                         'C','YX','category',                              OUTPUT.340
     +                         '1/0=LAND/WATER',                                 OUTPUT.341
     +                         IYY,JXX,1)                                        OUTPUT.342
                                                                                 OUTPUT.343
C DOMINANT VEG CATEGORY INDEX                                                    OUTPUT.344
C                                                                                OUTPUT.345
C         CALL WRITE_FIELDREC( IINC,D2,XLVC,XTIME,                               OUTPUT.346
C    +                         'VEGEINDX',CURRENT_DATE,                          OUTPUT.347
C    +                         'C','YX','category',                              OUTPUT.348
C    +                         'DOMINANT TYPE VEGE CATEGORY',                    OUTPUT.349
C    +                         IYY,JXX,1)                                        OUTPUT.350
                                                                                 OUTPUT.351
C DOMINANT SOIL CATEGORY INDEX                                                   OUTPUT.352
                                                                                 OUTPUT.353
          CALL WRITE_FIELDREC( IINC,D2,XLSC,XTIME,                               OUTPUT.354
     +                         'SOILINDX',CURRENT_DATE,                          OUTPUT.355
     +                         'C','YX','category',                              OUTPUT.356
     +                         'DOMINANT TYPE SOIL CATEGORY',                    OUTPUT.357
     +                         IYY,JXX,1)                                        OUTPUT.358
      ENDIF                                                                      OUTPUT.359
                                                                                 OUTPUT.360
C   CALCULATE LAT/LONG ON DOT/CROSS POINTS                                       OUTPUT.361
C     CT: LATX                                                                   OUTPUT.362
C     CL: LONGX                                                                  OUTPUT.363
C    TGC: MAP SCALE FACTOR (CROSS)                                               OUTPUT.364
                                                                                 OUTPUT.365
      DS = DIS(NESTID)                                                           OUTPUT.366
      ICROSS = 1                                                                 OUTPUT.367
      IRAT   = NRATIO(NESTID)                                                    OUTPUT.368
C     PRINT *,'XSOUTH,XWEST,CNTRI0,CNTRJ0 = ',                                   OUTPUT.369
C    -         XSOUTH(NESTID),XWEST(NESTID),CNTRI0,CNTRJ0                        OUTPUT.370
      CALL TERRESTRIAL_INFO(XSOUTH(NESTID),XWEST(NESTID),CNTRI0,CNTRJ0,          OUTPUT.371
     &     DS,ICROSS,IRAT,WLC,TGC,CL,CT,IYY,JXX)                                 OUTPUT.372
                                                                                 OUTPUT.373
       CALL WRITE_FIELDREC( IINC,D2,CT,XTIME,                                    OUTPUT.374
     +                      'LATITCRS',CURRENT_DATE,                             OUTPUT.375
     +                      'C','YX','degree',                                   OUTPUT.376
     +                      'LATITUDE (SOUTH NEGATIVE)',                         OUTPUT.377
     +                      IYY,JXX,1)                                           OUTPUT.378
       CALL WRITE_FIELDREC( IINC,D2,CL,XTIME,                                    OUTPUT.379
     +                      'LONGICRS',CURRENT_DATE,                             OUTPUT.380
     +                      'C','YX','degree',                                   OUTPUT.381
     +                      'LONGITUDE (WEST NEGATIVE)',                         OUTPUT.382
     +                      IYY,JXX,1)                                           OUTPUT.383
       CALL WRITE_FIELDREC( IINC,D2,TGC,XTIME,                                   OUTPUT.384
     +                      'MAPFACCR',CURRENT_DATE,                             OUTPUT.385
     +                      'C','YX','dimensionless',                            OUTPUT.386
     +                      'MAP-SCALE FACTOR',                                  OUTPUT.387
     +                      IYY,JXX,1)                                           OUTPUT.388
                                                                                 OUTPUT.389
C     CT: LATD                                                                   OUTPUT.390
C     CL: LONGD                                                                  OUTPUT.391
C    TGC: MAP SCALE FACTOR (DOT)                                                 OUTPUT.392
C    WLC: CORIOLIS (DOT)                                                         OUTPUT.393
                                                                                 OUTPUT.394
      ICROSS = 0                                                                 OUTPUT.395
C     PRINT *,'XSOUTH,XWEST,CNTRI0,CNTRJ0 = ',                                   OUTPUT.396
C    -         XSOUTH(NESTID),XWEST(NESTID),CNTRI0,CNTRJ0                        OUTPUT.397
      CALL TERRESTRIAL_INFO(XSOUTH(NESTID),XWEST(NESTID),CNTRI0,CNTRJ0,          OUTPUT.398
     &     DS,ICROSS,IRAT,WLC,TGC,CL,CT,IYY,JXX)                                 OUTPUT.399
                                                                                 OUTPUT.400
       CALL WRITE_FIELDREC( IINC,D2,CT,XTIME,                                    OUTPUT.401
     +                      'LATITDOT',CURRENT_DATE,                             OUTPUT.402
     +                      'D','YX','degree',                                   OUTPUT.403
     +                      'LATITUDE (SOUTH NEGATIVE)',                         OUTPUT.404
     +                      IYY,JXX,1)                                           OUTPUT.405
       CALL WRITE_FIELDREC( IINC,D2,CL,XTIME,                                    OUTPUT.406
     +                      'LONGIDOT',CURRENT_DATE,                             OUTPUT.407
     +                      'D','YX','degree',                                   OUTPUT.408
     +                      'LONGITUDE (WEST NEGATIVE)',                         OUTPUT.409
     +                      IYY,JXX,1)                                           OUTPUT.410
       CALL WRITE_FIELDREC( IINC,D2,TGC,XTIME,                                   OUTPUT.411
     +                      'MAPFACDT',CURRENT_DATE,                             OUTPUT.412
     +                      'D','YX','dimensionless',                            OUTPUT.413
     +                      'MAP-SCALE FACTOR',                                  OUTPUT.414
     +                      IYY,JXX,1)                                           OUTPUT.415
       CALL WRITE_FIELDREC( IINC,D2,WLC,XTIME,                                   OUTPUT.416
     +                      'CORIOLIS',CURRENT_DATE,                             OUTPUT.417
     +                      'D','YX','1/s',                                      OUTPUT.418
     +                      'CORIOLIS PARAMETER',                                OUTPUT.419
     +                      IYY,JXX,1)                                           OUTPUT.420
                                                                                 OUTPUT.421
C END OF TIME FLAG                                                               OUTPUT.422
                                                                                 OUTPUT.423
      WRITE (IINC) EOT_FLAG                                                      OUTPUT.424
                                                                                 OUTPUT.425
      END IF                                                                     OUTPUT.426
C END OF IFTER BLOCK                                                             OUTPUT.427
      RETURN                                                                     OUTPUT.428
      END                                                                        OUTPUT.429
