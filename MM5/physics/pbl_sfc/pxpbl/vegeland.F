      SUBROUTINE VEGELAND(VEGCAT,VEGFMO,JDATE,ILX,JLX,ZNT,RSTMIN                 VEGELAND.1
     >          ,XLAI,XVEG,XLAIMN,XVEGMN)                                        VEGELAND.2
      IMPLICIT NONE                                                              VEGELAND.3
C***************************************************************************     VEGELAND.4
C   VEGELAND5                                                                    VEGELAND.5
C                                                                                VEGELAND.6
C   CALLED FROM RDINIT                                                           VEGELAND.7
C                                                                                VEGELAND.8
C   THIS PROGRAM PROCESSES THE USGS LANDUSE DATA                                 VEGELAND.9
C   WHICH HAS BEEN GRIDDED BY THE TERRAIN PROGRAM                                VEGELAND.10
C   AND PRODUCES 2-D FIELDS OF LU RELATED PARAMETERS                             VEGELAND.11
C   FOR USE IN THE PX SURFACE MODEL                                              VEGELAND.12
C                                                                                VEGELAND.13
C-- ARGUMENT LIST:                                                               VEGELAND.14
C   VEGCAT       : PERCENT AREA OF EACH LU CAT BY GRID CELL                      VEGELAND.15
C   VEGFMO       : PERCENT AREA OF VEGETATION COVERAGE BY MONTH                  VEGELAND.16
C   JDATE        : JULIAN DAY                                                    VEGELAND.17
C                                                                                VEGELAND.18
C-- THREE OPTIONS FOR VEGETATION GROWTH (IFGROW IS A NAMELIST PARAMETER)         VEGELAND.19
C     1. IFGROW = 0 - USE VEGFRAC FOR ALL VEGCATS                                VEGELAND.20
C     2. IFGROW = 1 - READ PLANTING DATES FOR CROPS AND USE ORTEC GROWTH CURVES  VEGELAND.21
C                     STATE OF NATURAL VEG IS ESTIMATED ACCORDING TO SOIL TEMP   VEGELAND.22
C                     IN THE PX MODEL                                            VEGELAND.23
C     3. IFGROW = 2 - SAME AS OPTION 2 EXCEPT NO PLANTING DATES ARE INPUT        VEGELAND.24
C                     EMERGENCE DATES ARE ESTIMATED FROM VEGFMO                  VEGELAND.25
C                                                                                VEGELAND.26
C   INPUT FILES:  LEAF DATE FILE     -- IF IFGROW = 1                            VEGELAND.27
C                                                                                23DEC04.3809
C Modification after v3.5.3:                                                     23DEC04.3810
C          1.  Small correction on line200 (see comment). Affects IFGROW = 2     23DEC04.3811
C          2.  Added die off date (Oct 15) for crops. Affects IFGROW = 1 or 2    23DEC04.3812
C                                                                                VEGELAND.28
C******************************************************************************  VEGELAND.29
      INTEGER NCAT                                                               VEGELAND.30
      PARAMETER (NCAT = 24)    ! USGS 24 CATS                                    VEGELAND.31
#     include <parame.incl>                                                      VEGELAND.32
#     include <parpx.incl>                                                       VEGELAND.33
                                                                                 VEGELAND.34
C     NVC=NUMBER OF VEGETATION CATEGORIES (USGS)                                 VEGELAND.35
C     NMO=NUMBER OF MONTHS                                                       VEGELAND.36
      INTEGER NVC,NMO                                                            VEGELAND.37
      PARAMETER (NVC=25, NMO=12)          ! vcat = 25 is nodata                  VEGELAND.38
      INTEGER KWAT,IMO,K,IR,IRT,JC,JCT, IMO1,IMO2                                VEGELAND.39
      INTEGER JDATE,IDAYPE,IDAYPE2,JDEMRG                                        VEGELAND.40
                                                                                 VEGELAND.41
      REAL VEGCAT(MIXL,MJXL,NVC),VEGFMO(MIXL,MJXL,NMO)                           VEGELAND.42
      REAL ZNT(MIX,MJX),RSTMIN(MIXL,MJXL)                                        VEGELAND.43
      REAL XLAI(MIXL,MJXL),XVEG(MIXL,MJXL)                                       VEGELAND.44
      REAL XLAIMN(MIXL,MJXL),XVEGMN(MIXL,MJXL)                                   VEGELAND.45
      REAL RSMIN(NCAT),Z0(NCAT),VEG(NCAT),VEGMN(NCAT)                            VEGELAND.46
      REAL LAIMX(NCAT), LAIMN(NCAT)                                              VEGELAND.47
C-- PLANT DATE DATA ---                                                          VEGELAND.48
      INTEGER IPLANT(200,200), JMO(12)                                           VEGELAND.49
      REAL    LAI0(NCAT),LAIMN0(NCAT),Z00(NCAT)                                  VEGELAND.50
      REAL    VEG0(NCAT),VEGMN0(NCAT)                                            VEGELAND.51
                                                                                 VEGELAND.52
      REAL SUMLAI,SUMLMN,SUMRSI,SUMLZ0,SUMVEG,SUMVMN                             VEGELAND.53
      REAL ALAI,VFMX,VF1,VF2                                                     VEGELAND.54
      REAL SIG,ZNOTC,CLAI                                                        VEGELAND.55
      REAL VEGF,FAREA,FWAT,FDATE                                                 VEGELAND.56
      INTEGER ILX,JLX                                                            VEGELAND.57
C*****************************************************************************   VEGELAND.58
C  USGS LU characterization                                                      VEGELAND.59
c---------------------------                                                     VEGELAND.60
C    Name  Rstmin  Zo  Mxfr MnFr  MxLA MnLA                                      VEGELAND.61
C 1  Urban  150.   50.  40.  20.  2.0  0.5  Urban or Built-up Land               VEGELAND.62
C 2  DrCrp   70.   10.  95.  15.  3.0  0.5  Dryland Cropland and Pasture         VEGELAND.63
C 3  IrCrp   60.   10.  95.  10.  3.0  0.5  Irrigated Cropland and Pasture       VEGELAND.64
C 4  MixCp   70.   10.  95.  15.  3.0  0.5  Mixed Dry/Irr Crop and Past          VEGELAND.65
C 5  CrGrM   80.   10.  95.  35.  2.5  1.0  Grassland/Cropland Mosaic            VEGELAND.66
C 6  CrWdM  180.   40.  95.  40.  4.0  1.5  Woodland/Cropland Mosaic             VEGELAND.67
C 7  GrsLd  100.    7.  95.  70.  2.5  1.0  Grassland                            VEGELAND.68
C 8  ShrLd  200.   20.  70.  50.  3.0  1.0  Shrubland                            VEGELAND.69
C 9  ShrGr  150.   20.  85.  60.  3.0  1.0  Mixed Shrubland/Grassland            VEGELAND.70
C 10 Savan  120.   20.  80.  60.  2.0  1.0  Savanna                              VEGELAND.71
C 11 DBFst  200.   50.  95.  50.  5.0  1.0  Broadleaf Deciduous Forest           VEGELAND.72
C 12 DNFst  175.   50.  95.  50.  5.0  1.0  Deciduous Coniferous Forest          VEGELAND.73
C 13 EBFst  120.   40.  95.  85.  5.0  4.0  Evergreen Broadleaf Forest (Palm?)   VEGELAND.74
C 14 ENFst  175.   50.  90.  80.  4.0  3.0  Evergreen Coniferous Forest          VEGELAND.75
C 15 MxFst  200.   50.  95.  60.  5.0  2.0  Mixed forest                         VEGELAND.76
C 16 Water 9999.  .001  00.  00.  0.0  0.0  Water                                VEGELAND.77
C 17 HWtld  164.   15.  60.  40.  2.0  1.0  Herbaceous Wetland (none in east)    VEGELAND.78
C 18 WWtld  200.   45.  90.  80.  5.0  3.0  Forested Wetlands (e.g. Everglades)  VEGELAND.79
C 19 BarSp  100.    5.  10.  05.  0.5  0.2  Barren or Sparsely Vegetated         VEGELAND.80
C 20 HrTun  150.   10.  20.  10.  1.0  0.5  Herbaceous Tundra                    VEGELAND.81
C 21 WdTun  200.   10.  30.  10.  1.0  0.5  Shrub and Brush Tundra               VEGELAND.82
C 22 MxTun  150.    5.  20.  05.  1.0  0.5  Mixed Tundra                         VEGELAND.83
C 23 BGTun  100.    5.   5.  02.  0.1  0.1  Bare Ground Tundra                   VEGELAND.84
C 24 SnwIc  300.    5.   5.  02.  0.1  0.1  Perennial Snowfields or Glaciers     VEGELAND.85
C-----------------------------------------------------------------------------   VEGELAND.86
      DATA RSMIN/150.0,70.0,60.0,70.0,80.0,180.0,100.0,200.0,                    VEGELAND.87
     >150.0,120.0,200.0,175.0,120.0,175.0,200.0,9999.0,                          VEGELAND.88
     >164.0,200.0,100.0,150.0,200.0,150.0,100.0,300.0/                           VEGELAND.89
      DATA Z00/50.0,10.0,10.0,10.0,10.0,40.0,7.0,20.0,20.0,20.0,                 VEGELAND.90
     >50.0,50.0,40.0,50.0,50.0,0.001,15.0,45.0,5.0,                              VEGELAND.91
     >10.0,10.0,5.0,5.0,5.0/                                                     VEGELAND.92
      DATA VEG0/40.0,95.0,95.0,95.0,95.0,95.0,95.0,70.0,85.0,80.0,               VEGELAND.93
     >95.0,95.0,95.0,90.0,95.0,0.00,60.0,90.0,10.0,20.0,30.0,                    VEGELAND.94
     >20.0,5.0,5.0/                                                              VEGELAND.95
      DATA VEGMN0/20.0,15.0,10.0,15.0,35.0,40.0,70.0,50.0,60.0,60.0,             VEGELAND.96
     >50.0,50.0,85.0,80.0,60.0,0.0,40.0,80.0,5.0,10.0,                           VEGELAND.97
     >10.0,5.0,2.0,2.0/                                                          VEGELAND.98
      DATA LAI0/2.0,3.0,3.0,3.0,2.5,4.0,2.5,3.0,3.0,2.0,5.0,5.0,5.0,             VEGELAND.99
     >4.0,5.0,0.0,2.0,5.0,0.50,1.0,1.0,1.0,0.1,0.1/                              VEGELAND.100
      DATA LAIMN0/0.50,0.50,0.50,0.50,1.0,1.5,1.0,1.0,1.0,1.0,1.0,               VEGELAND.101
     >1.0,4.0,3.0,2.0,0.0,1.0,3.0,0.20,0.50,0.50,0.50,0.10,                      VEGELAND.102
     >0.10/                                                                      VEGELAND.103
                                                                                 VEGELAND.104
C---- INITIALIZE PARAMETERS                                                      VEGELAND.105
                                                                                 VEGELAND.106
      DATA KWAT/16/                                                              VEGELAND.107
      DATA JMO/15,46,74,105,135,166,196,227,258,288,319,349/                     VEGELAND.108
                                                                                 VEGELAND.109
      DO K=1,NCAT                                                                VEGELAND.110
        LAIMX(K) = LAI0(K)                                                       VEGELAND.111
        LAIMN(K) = LAIMN0(K)                                                     VEGELAND.112
        Z0(K) = Z00(K)                                                           VEGELAND.113
        VEG(K) = VEG0(K)                                                         VEGELAND.114
        VEGMN(K) = VEGMN0(K)                                                     VEGELAND.115
      ENDDO                                                                      VEGELAND.116
      DO 10 IR = 1, ILX                                                          VEGELAND.117
      DO 10 JC = 1, JLX                                                          VEGELAND.118
        XLAI(IR,JC) = 0.0                                                        VEGELAND.119
        XLAIMN(IR,JC) = 0.0                                                      VEGELAND.120
        RSTMIN(IR,JC) = 9999.                                                    VEGELAND.121
        ZNT(IR,JC) = .00001                                                      VEGELAND.122
        XVEG(IR,JC) = 0.0                                                        VEGELAND.123
        XVEGMN(IR,JC) = 0.0                                                      VEGELAND.124
10    CONTINUE                                                                   VEGELAND.125
                                                                                 VEGELAND.126
      IF(IFGROW.EQ.1) THEN                                                       VEGELAND.127
C********                                                                        VEGELAND.128
       OPEN (UNIT=12, FILE='LEAFDATE', STATUS='OLD',                             VEGELAND.129
     &       FORM='FORMATTED')                                                   VEGELAND.130
                                                                                 VEGELAND.131
C---- READ GRIDDED LEAFOUT DATA                                                  VEGELAND.132
      DO 100 IR = 1, ILX                                                         VEGELAND.133
      DO 100 JC = 1, JLX                                                         VEGELAND.134
      READ(12, 95) IPLANT(IR,JC)  !  , IBUD, ILEAF                               VEGELAND.135
95    FORMAT (30X,I3)                                                            VEGELAND.136
100   CONTINUE                                                                   VEGELAND.137
      WRITE(6,*) ' end of LEAFDATE'                                              VEGELAND.138
      ENDIF                                                                      VEGELAND.139
                                                                                 VEGELAND.140
      PRINT 22, JDATE,IFGROW                                                     VEGELAND.141
22    FORMAT(' VEGELAND PARAMS: JDATE=',I3,' IFGROW=',I1)                        VEGELAND.142
                                                                                 VEGELAND.143
C-- INTERPOLATE BY JULIAN DATE                                                   VEGELAND.144
      IF(IFGROW.EQ.0) THEN                                                       VEGELAND.145
        DO IMO=1,12                                                              VEGELAND.146
          IF(JMO(IMO).GT.JDATE) GO TO 60                                         VEGELAND.147
        ENDDO                                                                    VEGELAND.148
        IMO=1                                                                    VEGELAND.149
60      CONTINUE                                                                 VEGELAND.150
        IF(IMO.EQ.1) THEN                                                        VEGELAND.151
          IMO1 = 12                                                              VEGELAND.152
          IMO2 = 1                                                               VEGELAND.153
          IF(JDATE.LE.JMO(1)) JDATE = JDATE + 365                                VEGELAND.154
          FDATE = FLOAT(JDATE-JMO(IMO1))/31.                                     VEGELAND.155
        ELSE                                                                     VEGELAND.156
          IMO1 = IMO - 1                                                         VEGELAND.157
          IMO2 = IMO                                                             VEGELAND.158
          FDATE = FLOAT(JDATE-JMO(IMO1))/FLOAT(JMO(IMO2)-JMO(IMO1))              VEGELAND.159
        ENDIF                                                                    VEGELAND.160
      ENDIF                                                                      VEGELAND.161
C...START OF GRID CELL BY GRID CELL PROCESSING                                   VEGELAND.162
                                                                                 VEGELAND.163
      DO 99 IRT=1,ILX                                                            VEGELAND.164
      DO 99 JCT=1,JLX                                                            VEGELAND.165
                                                                                 VEGELAND.166
c     IF(VEGCAT(IRT,JCT,NCAT+1).GT.99.0) THEN                                    VEGELAND.167
c       PRINT *,' NO LU DATA AT I=',IRT,' J=',JCT                                VEGELAND.168
c       STOP                                                                     VEGELAND.169
c     ENDIF                                                                      VEGELAND.170
                                                                                 VEGELAND.171
C--- INITIALIZE SUMS                                                             VEGELAND.172
                                                                                 VEGELAND.173
      SUMLAI = 0.0                                                               VEGELAND.174
      SUMLMN = 0.0                                                               VEGELAND.175
      SUMRSI = 0.0                                                               VEGELAND.176
      SUMLZ0 = 0.0                                                               VEGELAND.177
      SUMVEG = 0.0                                                               VEGELAND.178
      SUMVMN = 0.0                                                               VEGELAND.179
      ALAI   = 0.0                                                               VEGELAND.180
                                                                                 VEGELAND.181
      VFMX   = 0.0                                                               VEGELAND.182
                                                                                 VEGELAND.183
C-- ESTIMATE CROP EMERGANCE DATE FROM VEGFRAC                                    VEGELAND.184
      DO IMO = 1,12                                                              VEGELAND.185
         IF(VFMX.LT.VEGFMO(IRT,JCT,IMO)) VFMX = VEGFMO(IRT,JCT,IMO)              VEGELAND.186
      ENDDO                                                                      VEGELAND.187
      IF(VFMX.GT.0.0.AND.VEGCAT(IRT,JCT,KWAT).LT.99.0) THEN                      VEGELAND.188
        IF(IFGROW.GT.0) THEN                                                     VEGELAND.189
          DO IMO = 1,12                                                          VEGELAND.190
            IF(VEGFMO(IRT,JCT,IMO).GT.VFMX*0.4) GO TO 62                         VEGELAND.191
          ENDDO                                                                  VEGELAND.192
62        CONTINUE                                                               VEGELAND.193
          IF(IMO.EQ.1) THEN                                                      VEGELAND.194
            IDAYPE = 88                                                          VEGELAND.195
            GO TO 70                                                             VEGELAND.196
          ENDIF                                                                  VEGELAND.197
          VF1 = VEGFMO(IRT,JCT,IMO-1)/VFMX                                       VEGELAND.198
          VF2 = VEGFMO(IRT,JCT,IMO)/VFMX                                         VEGELAND.199
C-- Corrected next line.  Changed (0.5 - VF1) to (0.4 - VF1)                     23DEC04.3813
          JDEMRG = JMO(IMO-1)+(0.4 - VF1)/(VF2-VF1)                              23DEC04.3814
     >             *(JMO(IMO)-JMO(IMO-1))                                        VEGELAND.201
          JDEMRG = MIN(JDEMRG,170)                                               VEGELAND.202
C -- COMPUTE CROP GROWTH                                                         VEGELAND.203
          IF(IPLANT(IRT,JCT).GT.0.0.AND.IFGROW.EQ.1) THEN                        VEGELAND.204
            IDAYPE = JDATE - (IPLANT(IRT,JCT) + 14)                              VEGELAND.205
          ELSE                                                                   VEGELAND.206
            IDAYPE = JDATE - JDEMRG                                              VEGELAND.207
          ENDIF                                                                  VEGELAND.208
          IDAYPE2 = IDAYPE*IDAYPE                                                VEGELAND.209
C-- Crop growth by days past emergence (from ORTECH model)                       VEGELAND.210
          SIG = 0.0184 * IDAYPE - 1.057E-4 * IDAYPE2                             VEGELAND.211
          ZNOTC = 5.00 + .23 * IDAYPE - 1.32E-3 * IDAYPE2                        VEGELAND.212
C-- Added die off date for crops - Oct 15  - jp 10/02                            23DEC04.3815
70        IF(IDAYPE.LE.0.OR.JDATE.GT.288) THEN                                   23DEC04.3816
            SIG = 0.                                                             VEGELAND.214
            ZNOTC = 5.0  ! CM -  5 cm is winter value for ag in MM5              VEGELAND.215
          ELSE IF(IDAYPE.GE.87) THEN                                             VEGELAND.216
            SIG = 0.8                                                            VEGELAND.217
            ZNOTC = 15.0  ! CM                                                   VEGELAND.218
          ENDIF                                                                  VEGELAND.219
          CLAI = AMAX1((2.273*SIG)**1.923,0.5)  ! SET MIN= 0.5                   VEGELAND.220
          DO 310 K=1, NCAT                                                       VEGELAND.221
C-- IF DrCrp (k=2) OR IrCrp (k=3) or MxCrp (k=4) USE ORTECH SCHEME               VEGELAND.222
            IF (K.GE.2.AND.K.LE.4) THEN                                          VEGELAND.223
                LAIMX(K) = CLAI                                                  VEGELAND.224
                LAIMN(K) = CLAI                                                  VEGELAND.225
                Z0(K) = ZNOTC                                                    VEGELAND.226
C-- IF CrGrM (k=5) or CrWdM (k=6) USE AVG WITH GRASS AND FOREST                  VEGELAND.227
             ELSE IF (K.GE.5.AND.K.LE.6) THEN                                    VEGELAND.228
                LAIMX(K) = 0.5*(CLAI + LAI0(K))                                  VEGELAND.229
                LAIMN(K) = 0.5*(CLAI + LAIMN0(K))                                VEGELAND.230
                Z0(K) = 0.5*(ZNOTC + Z00(K))                                     VEGELAND.231
             ENDIF                                                               VEGELAND.232
310       CONTINUE                                                               VEGELAND.233
        ELSE         ! IFGROW = 0                                                VEGELAND.234
          VF1 = VEGFMO(IRT,JCT,IMO1)/VFMX                                        VEGELAND.235
          VF2 = VEGFMO(IRT,JCT,IMO2)/VFMX                                        VEGELAND.236
          VEGF = VF1 + FDATE * (VF2-VF1)                                         VEGELAND.237
C       print *, ' IRT,JCT,jmo1,jmo2=',IRT,JCT,jmo(imo),jmo(imo-1)               VEGELAND.238
          DO K=1, NCAT                                                           VEGELAND.239
            LAIMX(K) = LAIMN0(K) * (1-VEGF) + LAI0(K) * VEGF                     VEGELAND.240
            LAIMN(K) = LAIMX(K)                                                  VEGELAND.241
            VEG(K) = VEGMN0(K) * (1-VEGF) + VEG0(K) * VEGF                       VEGELAND.242
            VEGMN(K) = VEG(K)                                                    VEGELAND.243
          ENDDO                                                                  VEGELAND.244
        ENDIF     ! IFGROW                                                       VEGELAND.245
      ENDIF       ! VEGETATED GRID CELLS                                         VEGELAND.246
C-------------------------------                                                 VEGELAND.247
C-- LOOP THROUGH VEGCATS                                                         VEGELAND.248
      DO 320 K=1, NCAT                                                           VEGELAND.249
            FAREA = 0.01 * VEGCAT(IRT,JCT,K)                                     VEGELAND.250
C            IF(VEGID(K).EQ.'EvFr'.AND.JRT.LT.JSP) K = NCAT                      VEGELAND.251
            SUMLAI = SUMLAI + LAIMX(K)*FAREA                                     VEGELAND.252
            SUMLMN = SUMLMN + LAIMN(K)*FAREA                                     VEGELAND.253
            ALAI   = ALAI + FAREA                                                VEGELAND.254
            SUMRSI = SUMRSI+FAREA*LAIMX(K)/RSMIN(K)                              VEGELAND.255
            SUMLZ0 = SUMLZ0 + FAREA*ALOG(Z0(K))                                  VEGELAND.256
            SUMVEG = SUMVEG + FAREA*VEG(K)                                       VEGELAND.257
            SUMVMN = SUMVMN + FAREA*VEGMN(K)                                     VEGELAND.258
 320  CONTINUE                                                                   VEGELAND.259
                                                                                 VEGELAND.260
C-- CHECK FOR WATER                                                              VEGELAND.261
        FWAT = 0.01 * VEGCAT(IRT,JCT,KWAT)                                       VEGELAND.262
        IF(FWAT.GT.0.999) THEN                                                   VEGELAND.263
                        XLAI(IRT,JCT) = LAIMX(KWAT)                              VEGELAND.264
                        XLAIMN(IRT,JCT) = LAIMN(KWAT)                            VEGELAND.265
                        RSTMIN(IRT,JCT) = RSMIN(KWAT)                            VEGELAND.266
                        ZNT(IRT,JCT) = Z0(KWAT)                                  VEGELAND.267
                        XVEG(IRT,JCT) = VEG(KWAT)                                VEGELAND.268
                        XVEGMN(IRT,JCT) = VEGMN(KWAT)                            VEGELAND.269
         ELSE                                                                    VEGELAND.270
            IF(FWAT.GT.0.10) THEN                                                VEGELAND.271
               ALAI   = ALAI - FWAT                                              VEGELAND.272
               SUMLZ0 = SUMLZ0 -                                                 VEGELAND.273
     >                           FWAT * ALOG(Z0(KWAT))                           VEGELAND.274
            ENDIF                                                                VEGELAND.275
                        XLAI(IRT,JCT) = SUMLAI / ALAI                            VEGELAND.276
                        XLAIMN(IRT,JCT) = SUMLMN / ALAI                          VEGELAND.277
                        RSTMIN(IRT,JCT) = SUMLAI/SUMRSI                          VEGELAND.278
                        ZNT(IRT,JCT)  = EXP(SUMLZ0/ALAI)                         VEGELAND.279
                        XVEG(IRT,JCT) = SUMVEG/ALAI                              VEGELAND.280
                        XVEGMN(IRT,JCT) = SUMVMN/ALAI                            VEGELAND.281
         ENDIF                                                                   VEGELAND.282
         ZNT(IRT,JCT) = ZNT(IRT,JCT)*0.01           !CONVERT TO M                VEGELAND.283
         XVEG(IRT,JCT) = XVEG(IRT,JCT)*0.01       !CONVERT TO FRAC               VEGELAND.284
         XVEGMN(IRT,JCT) = XVEGMN(IRT,JCT)*0.01                                  VEGELAND.285
                                                                                 VEGELAND.286
C*** WRITE NORMALIZED GRIDDED EMISSIONS TO OUTPUT FILE                           VEGELAND.287
C       WRITE(20,805) JCT,IRT, AVGLAI, AVGLMN,                                   VEGELAND.288
C     &  AVGRSMN, AVGZ0, AVGVEG,AVGVMN, ALAI                                     VEGELAND.289
                                                                                 VEGELAND.290
99    CONTINUE                                                                   VEGELAND.291
C805    FORMAT (2I4, 3F15.2,F15.4,3F15.2)                                        VEGELAND.292
      RETURN                                                                     VEGELAND.293
       END                                                                       VEGELAND.294
