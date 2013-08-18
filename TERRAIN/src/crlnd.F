      SUBROUTINE CRLND(LAND,LNDOUT,FORCETER,XLAT,XLON,IX,JX,III,JJJ,N,           CRLND.1
     1NI,NOBS,GRDLNMN,GRDLTMN,ZLON,ZLAT,ZLAND)                                   CRLND.2
C                                                                                CRLND.3
C   PURPOSE       : CREATE THE LAND-USE CATEGORIES FIELD ON THE                  CRLND.4
C                   MESOSCALE GRIDS BY USING THE OVERLAPPING PARABOLIC           CRLND.5
C                   INTERPOLATION METHOD WITH THE SOURCE LAND-USE                CRLND.6
C                   DATA (PERCENTAGE) ON THE                                     CRLND.7
C                   REGULAR LATITUDE-LONGITUDE MESH AS INPUT.                    CRLND.8
C   XLAT(IX,JX)   : LATITUDES OF THE MESOSCALE GRID POINTS.                      CRLND.9
C   XLON(IX,JX)   : LONGITUDES OF THE MESOSCALE GRID POINTS.                     CRLND.10
C   IX,JX         : THE DIMENSIONS OF THE MESOSCALE DOMAIN N.                    CRLND.11
C   III,JJJ       : THE DIMENSIONS OF THE FIELD WITH THE REGULAR                 CRLND.12
C                   LATITUDE-LONGITUDE INTERVALS.                                CRLND.13
C   N             : THE ID NUMBER OF THE MESOSCALE DOMAIN.                       CRLND.14
C   NI            : THE ID NUMBER OF SOURCE DATA FILE.  THE LAND-USE             CRLND.15
C                   SOURCE DATA ID SHOULD NOT EXCEED 3 (SEE SUBROUTINE           CRLND.16
C                   ANAL2).                                                      CRLND.17
C   NOBS          : THE NUMBER OF INPUT SOURCE DATA POINTS.                      CRLND.18
C   GRDLNMN       : THE MINIMUM LONGITUDE OF THE SOURCE DATA.                    CRLND.19
C   GRDLTMN       : THE MINIMUM LATITUDE OF THE SOURCE DATA.                     CRLND.20
C   ZLON(NOBS)    : LONGITUDES OF THE INPUT SOURCE DATA.                         CRLND.21
C   ZLAT(NOBS)    : LATITUDES OF THE INPUT SOURCE DATA                           CRLND.22
C   ZLAND(13,NOBS): THE 13 CATEGORIES OFF THE SOURCE LAND-USE DATA.              CRLND.23
C                   1. URBAN LAND                                                CRLND.24
C                   2. AGRICULTURAL LAND                                         CRLND.25
C                   3. RANGE LAND/GRASSLAND                                      CRLND.26
C                   4. DECIDUOUS FOREST                                          CRLND.27
C                   5. CONIFEROUS FOREST                                         CRLND.28
C                   6. MIXED FOREST INCLUDING WETLAND                            CRLND.29
C                   7. WATER                                                     CRLND.30
C                   8. NON-FORESTED MARSH OR WETLAND                             CRLND.31
C                   9. DESERT                                                    CRLND.32
C                  10. TUNDRA                                                    CRLND.33
C                  11. PERMANENT ICE                                             CRLND.34
C                  12. NONSEASONAL TROPICAL OR SUBTROPICAL FOREST                CRLND.35
C                  13. SAVANNAH                                                  CRLND.36
C    LAND(IX,JX)  : THE LAND-USE CATEGORIES ON THE MESOSCALE DOMAIN N.           CRLND.37
C                   THIS IS AN INTEGER ARRAY.                                    CRLND.38
C    LNDOUT(IX,JX): THE LAND-USE CATEGORIES ON THE MESOSCALE DOMAIN N.           CRLND.39
C                   THIS IS A REAL ARRAY.                                        CRLND.40
C                                                                                CRLND.41
C   CALLS FUNCTION BINT AND FUDGER.                                              CRLND.42
C   CALLED BY SUBROUTINE TERDRV.                                                 CRLND.43
C                                                                                CRLND.44
C   COMMENTS :                                                                   CRLND.45
C IN ADDITION TO THE LIST OF ARGUMENTS, THIS SUBROUTINE GETS THE                 CRLND.46
C REST OF INFORMATION FROM THE COMMON BLOCKS /LTDATA/, /FUDG/, AND               CRLND.47
C /OPTION/. BASED ON THE RESOLUTION AND THE MINIMUM LATITUDE AND                 CRLND.48
C LONGITUDE OF THE SOURCE DATA, THE ONE-DIMENSIONAL SOURCE DATA ARE              CRLND.49
C PUT INTO A 2-DIMENSIONAL ARRAY LNDIN(III,JJJ). THEN THE INDICES OF             CRLND.50
C THE MESOSCALE GRID POINTS IN THE LATITUDE-LONGITUDE MESH ARE                   CRLND.51
C COMPUTED FROM XLAT AND XLON, AND THE FUNCTION BINT IS CALLED TO                CRLND.52
C INTERPOLATE THE PERCENTAGES OF EACH OF THE CATEGORIES TO THE                   CRLND.53
C MESOSCALE GRIDS. THE CATEGORY WITH THE HIGHEST PERCENTAGE IS SET               CRLND.54
C TO BE THE LAND-USE VALUE AT THAT GRID POINT                                    CRLND.55
C                                                                                CRLND.56
C IF IFFUG = .TRUE., THE SUBROUTINE FUDGER IS CALLED TO FUDGE THE                CRLND.57
C LAND-USE WITH THE INFORMATION FROM COMMON BLOCK /FUDG/.                        CRLND.58
# include <parame.incl>                                                          CRLND.59
# include <paramed.incl>                                                         CRLND.60
# include <io.incl>                                                              06NOV00.7
# include <ltdata.incl>                                                          CRLND.61
# include <fudge.incl>                                                           CRLND.62
# include <option.incl>                                                          CRLND.63
# include <maps.incl>                                                            CRLND.64
# include <nestdmn.incl>                                                         CRLND.65
# include <trfudge.incl>                                                         CRLND.66
# include <ezwater.incl>                                                         CRLND.67
      INTEGER          AMAPF(255000)     ! FOR COMMON BLOCK ARAMAP               CRLND.68
      COMMON /ARAMAP/  AMAPF             ! AREA MAP FOR LAT/LON LINES            CRLND.69
      DIMENSION IAREA(10),IGRP(10)       ! FOR CALL TO ARGTAI                    CRLND.70
      CHARACTER*80 TITLE                 ! TITLE STRING FOR MAPDRV               CRLND.71
C                                                                                CRLND.72
      DIMENSION   LAND(IX,JX), FORCETER(IX,JX), LNDOUT(IX,JX),                   CRLND.73
     1            XLAT(IX,JX),      XLON(IX,JX)                                  CRLND.74
      DIMENSION ZLON(NOBS), ZLAT(NOBS), ZLAND(13,NOBS)                           CRLND.75
      DIMENSION LND0(IIMX,JJMX,2), LNDIN(ITRH,JTRH)                              CRLND.76
      REAL LNDIN, LNDOUT, LND0                                                   CRLND.77
      LOGICAL FLAG                                                               06NOV00.8
C                                                                                CRLND.79
      FLAG    = .TRUE.                                                           CRLND.80
      DO 5 K=1,2                                                                 CRLND.81
      DO 5 J=1,JX                                                                CRLND.82
      DO 5 I=1,IX                                                                CRLND.83
    5 LND0(I,J,K) = 0                                                            CRLND.84
C                                                                                CRLND.85
C  NI=1 -- 1. DEG., NI=2 -- .5 DEG. AND NI=3 -- 1/6 DEG.                         CRLND.86
C                                                                                CRLND.87
C                                                                                CRLND.88
      NF = NI                                                                    CRLND.89
      IF (NF.GT.3) NF = 3                                                        CRLND.90
      DSGRID = CENTER(NF)                                                        CRLND.91
      INCR = 1./DSGRID+0.001                                                     CRLND.92
      IF(IPRINT.EQ.1) THEN                                                       CRLND.93
      PRINT 6, NI, NF, CENTER(NF),INCR                                           CRLND.94
6     FORMAT(' ==> CALL LNDDVR: NI=',I2,' NF=',I2,' RESOLUTION =',               CRLND.95
     1       F8.5,3X,'INCR=',I3)                                                 CRLND.96
      PRINT 7, NOBS,GRDLNMN, GRDLTMN                                             CRLND.97
7     FORMAT(/2X,'LANDUSE: NOBS=',I8,4X,' ==< GRDLNMN=',F10.2,                   CRLND.98
     1                                    '   GRDLTMN=',F10.2,'>==')             CRLND.99
      ENDIF                                                                      CRLND.100
C                                                                                CRLND.101
C-----GRID THE DATA.  GRDLTMN=MINIMUM LATITUDE OF INCOMING DATA.                 CRLND.102
C-----GRDLNMN = MINIMUM LONGITUDE OF INCOMING DATA.  POINT(1,1)                  CRLND.103
C-----OF ARRAY LNDIN IS LANDUSE AT (GRDLTMN,GRDLNMN)                             CRLND.104
C                                                                                CRLND.105
C .. IF REQUESTED RAW DATA COVERAGE IS FROM -180. TO 180., THE ARRAY             CRLND.106
C    LNDIN HOLDING DATA SHOULD BE EXPANDED TO AVOID THE ERRORS OCCURED           CRLND.107
C    OVER THE GRID POINTS LOCATED IN AREA OF -180+DSGRID/2 TO                    CRLND.108
C    180-DSGRID/2.                         YONG-RUN  09/26/97                    CRLND.109
C                                                                                CRLND.110
      IF (ABS(XMAXLON-180.).LT.1.E-3  .AND.                                      CRLND.111
     1    ABS(XMINLON+180.).LT.1.E-3) THEN                                       CRLND.112
        START_J = 2.0                                                            CRLND.113
C        PRINT 26,START_J,XMAXLON,XMINLON                                        CRLND.114
 26     FORMAT(/'START_J=',F6.1,' XMAXLON=',F8.2,' XMINLON=',F8.2)               CRLND.115
      ELSE                                                                       CRLND.116
        START_J = 1.0                                                            CRLND.117
      ENDIF                                                                      CRLND.118
C                                                                                CRLND.119
      IIMAXLND = -100000                                                         CRLND.120
      JIMAXLN  = -100000                                                         CRLND.121
      IIMINLND =  100000                                                         CRLND.122
      JIMINLN  =  100000                                                         CRLND.123
      DO 10 ILEV=1,13                                                            CRLND.124
      DO 20 II=1,NOBS                                                            CRLND.125
      JINDEX = NINT((ZLON(II) - GRDLNMN)*INCR + START_J)                         CRLND.126
      IINDEX = (ZLAT(II) - GRDLTMN)*INCR + 1.1                                   CRLND.127
C                                                                                CRLND.128
      IF (ILEV.EQ.1 .AND. IINDEX.GT.IIMAXLND) IIMAXLND = IINDEX                  CRLND.129
      IF (ILEV.EQ.1 .AND. JINDEX.GT.JIMAXLN ) JIMAXLN  = JINDEX                  CRLND.130
      IF (ILEV.EQ.1 .AND. IINDEX.LT.IIMINLND) IIMINLND = IINDEX                  CRLND.131
      IF (ILEV.EQ.1 .AND. JINDEX.LT.JIMINLN ) JIMINLN  = JINDEX                  CRLND.132
C                                                                                CRLND.133
      LNDIN(IINDEX,JINDEX) = ZLAND(ILEV,II)                                      CRLND.134
20    CONTINUE                                                                   CRLND.135
C                                                                                CRLND.136
      IF (ABS(XMAXLON-180.).LT.1.E-3  .AND.                                      CRLND.137
     1    ABS(XMINLON+180.).LT.1.E-3) THEN                                       CRLND.138
          DO II = IIMINLND,IIMAXLND                                              CRLND.139
            LNDIN(II,         1) = LNDIN(II,JIMAXLN )                            CRLND.140
            LNDIN(II,JIMAXLN +1) = LNDIN(II,       2)                            CRLND.141
          END DO                                                                 CRLND.142
          JIMINLND = 1                                                           CRLND.143
          JIMAXLND = JIMAXLN +1                                                  CRLND.144
      ELSE                                                                       CRLND.145
          JIMINLND = JIMINLN                                                     CRLND.146
          JIMAXLND = JIMAXLN                                                     CRLND.147
      ENDIF                                                                      CRLND.148
C                                                                                CRLND.149
      IF (IIMAXLND.GT.ITRH .OR. JIMAXLND.GT.JTRH) THEN                           CRLND.150
         PRINT 60,IIMAXLND,JIMAXLND,ITRH,JTRH,GRDLTMN,GRDLNMN                    CRLND.151
 60      FORMAT(1X,'*** IIMAXLND = ',I6,'  JIMAXLND = ',I6,                      CRLND.152
     1       '   ITRH=',I6,' JTRH=',I6/3X,'GRDLTMN=',F10.3,5X,                   CRLND.153
     2       'GRDLNMN = ',F10.3,' IN CRLND.')                                    CRLND.154
C                                                                                CRLND.155
         STOP 60                                                                 CRLND.156
      ENDIF                                                                      CRLND.157
C                                                                                CRLND.158
      DO 30 II=1,IX                                                              CRLND.159
      DO 30 JJ=1,JX                                                              CRLND.160
      YY = - (GRDLTMN-XLAT(II,JJ))/DSGRID + 1.0                                  CRLND.161
        IF (XLON(II,JJ).GT.XMAXLON .AND. GRDLNMN.LT.-180)                        CRLND.162
     2       XLON(II,JJ)=XLON(II,JJ)-360.                                        CRLND.163
      XX = - (GRDLNMN-XLON(II,JJ))/DSGRID + START_J                              CRLND.164
      LNDOUT(II,JJ) = BINT(YY,XX,LNDIN,ITRH,JTRH,FLAG)                           CRLND.165
C                                                                                CRLND.166
      IF (YY.LT.IIMINLND .OR. YY.GT.IIMAXLND .OR.                                CRLND.167
     1    XX.LT.JIMINLND .OR. XX.GT.JIMAXLND) THEN                               CRLND.168
          PRINT 25,II,JJ,XX,YY,XLAT(II,JJ),XLON(II,JJ),LNDOUT(II,JJ)             CRLND.169
 25       FORMAT(/'POINT(II,JJ):',2I5,' XX=',F10.2,' YY=',F10.2,                 CRLND.170
     1            ' XLAT=',F8.2,' XLON=',F8.2,' LNDOUT=',F8.2)                   CRLND.171
      ENDIF                                                                      CRLND.172
C                                                                                CRLND.173
C     NOTE: IT IS DESIRABLE TO FORCE GRID BOXES WITH LESS                        CRLND.174
C           THAN 50 PERCENT WATER TO BE A LAND CATEGORY,                         CRLND.175
C           EVEN IF WATER IS THE LARGEST SINGLE CATEGORY.                        CRLND.176
C                                                                                CRLND.177
      IF (ILEV.EQ.7 .AND. LNDOUT(II,JJ).LT.50.0) GOTO 30                         CRLND.178
C                                                                                CRLND.179
      IF (LNDOUT(II,JJ).GT.LND0(II,JJ,1)) THEN                                   CRLND.180
         LND0(II,JJ,1) = LNDOUT(II,JJ)                                           CRLND.181
         LND0(II,JJ,2) = ILEV                                                    CRLND.182
      ENDIF                                                                      CRLND.183
30    CONTINUE                                                                   CRLND.184
10    CONTINUE                                                                   CRLND.185
C                                                                                CRLND.186
      DO 40 I=1,IX                                                               CRLND.187
      DO 40 J=1,JX                                                               CRLND.188
      LNDOUT(I,J) = LND0(I,J,2)                                                  CRLND.189
40    LAND(I,J) = IFIX(LND0(I,J,2))                                              CRLND.190
#ifdef NCARG                                                                     CRLND.191
C                                                                                CRLND.192
C   SET UP EZMAP AREA FILL TO DEFINE EZMAP AREAS                                 CRLND.193
C      (THIS CODE PIRATED FROM ROUTINE PLTTER)                                   CRLND.194
C                                                                                CRLND.195
      IF (.NOT.IFEZFUG) GOTO 305                                                 CRLND.196
      DSKM = DIS(N)                                                              CRLND.197
      IR = NRATIO(N)                                                             CRLND.198
      XIR = FLOAT(IR)                                                            CRLND.199
      JEND = (NESTJX(1)+2*JOFFST-1)*IR + 1                                       CRLND.200
      IEND = (NESTIX(1)+2*IOFFST-1)*IR + 1                                       CRLND.201
      ISTART = 1                                                                 CRLND.202
      JSTART = 1                                                                 CRLND.203
      IF (N.EQ.1) THEN                                                           CRLND.204
      XA = 1.0                                                                   CRLND.205
      YA = 1.0                                                                   CRLND.206
      XB = FLOAT(JEND)                                                           CRLND.207
      YB = FLOAT(IEND)                                                           CRLND.208
      ELSE                                                                       CRLND.209
      XA = (XWEST(N) - 1.)*XIR + 1.                                              CRLND.210
      XB = (XEAST(N) - 1.)*XIR + 1.                                              CRLND.211
      YA = (XSOUTH(N) - 1.)*XIR + 1.                                             CRLND.212
      YB = (XNORTH(N) - 1.)*XIR + 1.                                             CRLND.213
      ENDIF                                                                      CRLND.214
C                                                                                CRLND.215
C  AVOID THE XA, XB, YA, YB ARE OUTSIDE THE BIG DOMAIN:                          CRLND.216
      IF (XA.LT.1.0) THEN                                                        CRLND.217
          XA = 1.0                                                               CRLND.218
          PRINT *,'XA < 1.0, XA=1.0'                                             CRLND.219
          STOP                                                                   CRLND.220
      ENDIF                                                                      CRLND.221
      IF (XB.GT.FLOAT(JEND)) THEN                                                CRLND.222
          XB = FLOAT(JEND)                                                       CRLND.223
          PRINT *,'XB > JEND, XB = FLOAT(JEND)'                                  CRLND.224
          STOP                                                                   CRLND.225
      ENDIF                                                                      CRLND.226
      IF (YA.LT.1.0) THEN                                                        CRLND.227
          YA = 1.0                                                               CRLND.228
          PRINT *,'YA < 1.0, YA = 1.0'                                           CRLND.229
          STOP                                                                   CRLND.230
      ENDIF                                                                      CRLND.231
      IF (YB.GT.FLOAT(IEND)) THEN                                                CRLND.232
          YB = FLOAT(IEND)                                                       CRLND.233
          PRINT *,'YB > IEND, YB = FLOAT(IEND)'                                  CRLND.234
          STOP                                                                   CRLND.235
      ENDIF                                                                      CRLND.236
C                                                                                CRLND.237
      IF(IPRINT.EQ.1) THEN                                                       CRLND.238
      PRINT 2001, N,ISTART,IEND,JSTART,JEND,XA,XB,YA,YB                          CRLND.239
2001  FORMAT(/2X,'--- SETTING UP EZMAP AREAS IN CRLND ..........',I3             CRLND.240
     1    /2X,'ISTART,IEND,JSTART,JEND:',4I6,3X,'XA,XB,YA,YB:',4F8.1)            CRLND.241
      ENDIF                                                                      CRLND.242
C                                                                                CRLND.243
      WRITE(TITLE(1:9),22) N                                                     CRLND.244
 22   FORMAT('DOMAIN ',I2)                                                       CRLND.245
C                                                                                CRLND.246
C  OPEN THE COLOR TABLE FILE                                                     CRLND.247
                                                                                 CRLND.248
      MAPUNIT=17                                                                 CRLND.249
                                                                                 CRLND.250
C  OPEN THE CORRECT TABLE FILE                                                   CRLND.251
                                                                                 CRLND.252
      OPEN (MAPUNIT,FILE='maparea.tbl',STATUS='OLD')                             CRLND.253
                                                                                 CRLND.254
C  SET UP A COLOR TABLE                                                          CRLND.255
                                                                                 CRLND.256
      CALL RDCOLT(MAPUNIT)                                                       CRLND.257
                                                                                 CRLND.258
C  MAKE THE BLACK AND WHITE MAP                                                  CRLND.259
                                                                                 CRLND.260
C     ... TURN OF CLIPPING                                                       CRLND.261
                                                                                 CRLND.262
      CALL GSCLIP (0)                                                            CRLND.263
                                                                                 CRLND.264
C     ... USE WHOLE DOMAIN                                                       CRLND.265
                                                                                 CRLND.266
      CALL SET (0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)                               CRLND.267
                                                                                 CRLND.268
C                                                                                CRLND.269
C     ... ERRSEV: <0 ABORT ON WARNING, =0 ABORT ON ERROR, >0 NO ABORT            CRLND.270
                                                                                 CRLND.271
      ERRSEV=1                                                                   CRLND.272
      NBUF=N+10                                                                  CRLND.273
                                                                                 CRLND.274
      CALL MAPDRV (PROJECT,TRUELAT1,TRUELAT2,PHIC,XLONC,                         CRLND.275
     *             DSKM,JEND,IEND,XA,XB,YA,YB,TITLE,9,                           CRLND.276
     *             NBUF,MAPUNIT,.FALSE.,ERRSEV)                                  CRLND.277
                                                                                 CRLND.278
C  CLOSE THE TABLE FILE                                                          CRLND.279
                                                                                 CRLND.280
      CLOSE (MAPUNIT)                                                            CRLND.281
C                                                                                CRLND.282
C   OK, THE EZMAP AREAS SHOULD BE SET UP, SO NOW WE CAN                          CRLND.283
C      ADJUST THE LAND USE                                                       CRLND.284
C                                                                                CRLND.285
      CALL GETSET (FL,FR,FB,FT,UL,UR,UB,UT,LL)                                   CRLND.286
      CALL SET (FL,FR,FB,FT,1.,FLOAT(JX),1.,FLOAT(IX),LL)                        CRLND.287
C                                                                                CRLND.288
      ICF=1                                                                      CRLND.289
      CALL ARGTAI(AMAPF,3.,3.,IAREA,IGRP,10,NGRPS,ICF)                           CRLND.290
      ICF=0                                                                      CRLND.291
C                                                                                CRLND.292
C   SET REFINEMENT RATIO FOR WATER-CHECK GRID (PREFERABLY ODD)                   CRLND.293
C                                                                                CRLND.294
      IREFRAT=7                                                                  CRLND.295
      SUBINT=1/FLOAT(IREFRAT)                                                    CRLND.296
      HSUBINT=.5*SUBINT                                                          CRLND.297
      NDOMINANT=(IREFRAT**2+1)/2                                                 CRLND.298
      DO 300 J=1,JX-1                                                            CRLND.299
      DO 300 I=1,IX-1                                                            CRLND.300
         IF (.NOT.IFTFUG) GOTO 132                                               CRLND.301
         DO 130 IFB=1,NFUGBOX                                                    CRLND.302
            IF(   ( XLAT(I,J).GE.STARTLAT(IFB).AND.                              CRLND.303
     &              XLAT(I,J).LE.ENDLAT(IFB)         ) .AND.                     CRLND.304
     *            ( XLON(I,J).GE.STARTLON(IFB).AND.                              CRLND.305
     &              XLON(I,J) .LE. ENDLON(IFB)       )    ) GOTO 300             CRLND.306
 130     CONTINUE                                                                CRLND.307
 132     CONTINUE                                                                CRLND.308
         NWATER=0                                                                CRLND.309
         DISTSQMN=1.                                                             CRLND.310
         DO 150 JJ=1,IREFRAT                                                     CRLND.311
            XJJ=FLOAT(J)-HSUBINT+JJ*SUBINT                                       CRLND.312
         DO 150 II=1,IREFRAT                                                     CRLND.313
            YII=FLOAT(I)-HSUBINT+II*SUBINT                                       CRLND.314
            CALL ARGTAI(AMAPF,XJJ,YII,IAREA,IGRP,10,NGRPS,ICF)                   CRLND.315
            DO 140 IAR=1,NWAREAS                                                 CRLND.316
               IF (IAREA(1).EQ.IDWATER(IAR)) THEN                                CRLND.317
                  NWATER=NWATER+1                                                CRLND.318
                  DISTSQ=(XJJ-FLOAT(J)-.5)**2+(YII-FLOAT(I)-.5)**2               CRLND.319
                  IF (DISTSQ.LT.DISTSQMN) IUSEIT=IAR                             CRLND.320
                  GOTO 145                                                       CRLND.321
               ENDIF                                                             CRLND.322
 140        CONTINUE                                                             CRLND.323
 145        CONTINUE                                                             CRLND.324
 150     CONTINUE                                                                CRLND.325
         IF (NWATER.GE.NDOMINANT) THEN                                           CRLND.326
            LAND(I,J)=7                                                          CRLND.327
            LNDOUT(I,J)=7.                                                       CRLND.328
            FORCETER(I,J)=HTWATER(IUSEIT)                                        CRLND.329
         ELSEIF (LAND(I,J).EQ.7) THEN                                            CRLND.330
            DO 250 IR=1,1000                                                     CRLND.331
               JB=MAX(1,J-IR)                                                    CRLND.332
               JE=MIN(JX-1,J+IR)                                                 CRLND.333
               IB=MAX(1,I-IR)                                                    CRLND.334
               IE=MIN(IX-1,I+IR)                                                 CRLND.335
               DO 210 II=IB,IE                                                   CRLND.336
                  IF (LAND(II,JB).NE.7) THEN                                     CRLND.337
                     LAND(I,J)=LAND(II,JB)                                       CRLND.338
                     LNDOUT(I,J)=LAND(I,J)                                       CRLND.339
                     GOTO 260                                                    CRLND.340
                  ENDIF                                                          CRLND.341
                  IF (LAND(II,JE).NE.7) THEN                                     CRLND.342
                     LAND(I,J)=LAND(II,JE)                                       CRLND.343
                     LNDOUT(I,J)=LAND(I,J)                                       CRLND.344
                     GOTO 260                                                    CRLND.345
                  ENDIF                                                          CRLND.346
  210          CONTINUE                                                          CRLND.347
               DO 220 JJ=JB+1,JE-1                                               CRLND.348
                  IF (LAND(IB,JJ).NE.7) THEN                                     CRLND.349
                     LAND(I,J)=LAND(IB,JJ)                                       CRLND.350
                     LNDOUT(I,J)=LAND(I,J)                                       CRLND.351
                     GOTO 260                                                    CRLND.352
                  ENDIF                                                          CRLND.353
                  IF (LAND(IE,JJ).NE.7) THEN                                     CRLND.354
                     LAND(I,J)=LAND(IE,JJ)                                       CRLND.355
                     LNDOUT(I,J)=LAND(I,J)                                       CRLND.356
                     GOTO 260                                                    CRLND.357
                  ENDIF                                                          CRLND.358
  220          CONTINUE                                                          CRLND.359
  250       CONTINUE                                                             CRLND.360
            PRINT*,'TRYING TO DO AUTO-FUDGING OF LAND USE.'                      CRLND.361
            PRINT*,'COULDN''T FIND A LAND POINT TO ASSIGN.'                      CRLND.362
            PRINT*,'I,J=',I,J                                                    CRLND.363
            STOP                                                                 CRLND.364
  260       CONTINUE                                                             CRLND.365
         ENDIF                                                                   CRLND.366
 300  CONTINUE                                                                   CRLND.367
 305  CONTINUE                                                                   CRLND.368
#endif                                                                           CRLND.369
C                                                                                CRLND.370
C ALLOW FOR FUDGING OF LANDUSE                                                   CRLND.371
C                                                                                CRLND.372
C IFFUG  = .T.    FUDGE LANDUSE      NPFUG = # POINTS TO FUDGE                   CRLND.373
C                                                                                CRLND.374
      IF (IFFUG(N)) THEN                                                         CRLND.375
          PRINT 62,N,NDFUG(N)                                                    CRLND.376
 62      FORMAT (/,'  ***** LAND USE FUDGING IS PERFORMED',                      CRLND.377
     |              ' ON THE NEST NO.:',I3,2X,I5,' GRID-POINTS.')                CRLND.378
                                                                                 CRLND.379
      CALL FUDGER(N,NDFUG(N),IFUG(1,N),JFUG(1,N),LNDFUG(1,N),                    CRLND.380
     1                                    LAND,LNDOUT,IX,JX)                     CRLND.381
      ENDIF                                                                      CRLND.382
                                                                                 CRLND.383
      RETURN                                                                     CRLND.384
      END                                                                        CRLND.385
                                                                                 CRLND.386
