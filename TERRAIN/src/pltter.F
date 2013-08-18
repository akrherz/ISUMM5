      SUBROUTINE PLTTER (GRIDIN,LNDXX,GRDOUT,IMX,JMX,IYY,JXX,N,                  PLTTER.1
     -              VEGEC,SOILC,VEGEF,TEMPG,WLMSK,LVEGE,LSOIL)                   PLTTER.2
                                                                                 PLTTER.3
C   PURPOSE         : PLOTS THE COLOR-FILLED OR NON-COLOR-FILLED MAP             PLTTER.4
C                     BACKGROUND, TERRAIN HEIGHT, AND LAND-USE FIELDS,           PLTTER.5
C                     THE MESOSCALE MESH, THE RAWINSOUND STATIONS,               PLTTER.6
C                     AND THE NESTED DOMAIN SETTINGS.                            PLTTER.7
C   ON ENTRY        :                                                            PLTTER.8
C   GRIDIN(IMX,JMX) : THE TERRAIN HEIGHT FIELD. FOR THE EXPANDED                 PLTTER.9
C                     COARSE DOMAIN (IEXP = .TRUE.), IT HAS THE                  PLTTER.10
C                     EXPANDED GRID DIMENSION.                                   PLTTER.11
C   LNDXX(IMX,JMX)  : THE LAND-USE FIELD. FOR THE EXPANDED COARSE                PLTTER.12
C                     DOMAIN (IEXP = .TRUE.), IT HAS THE EXPANDED                PLTTER.13
C                     GRID DIMENSION.                                            PLTTER.14
C   IMX,JMX         : THE DIMENSION OF THE DOMAIN. IF IEXP = .TRUE.,             PLTTER.15
C                     THEY ARE THE EXPANDED GRID DIMENSIONS FOR THE              PLTTER.16
C                     COARSE DOMAIN.                                             PLTTER.17
C   IYY,JXX         : THE DIMENSION OF THE (NON-EXPANDED) DOMAIN.                PLTTER.18
C   N               : THE ID NUMBER OF THE DOMAIN.                               PLTTER.19
C   ON EXIT         :                                                            PLTTER.20
C                     GMETA FILE TER.PLT.                                        PLTTER.21
C                                                                                PLTTER.22
C   CALLS SUBROUTINE MAPDRV, CONDRV, LABEL AND MANY OF THE NCAR GKS              PLTTER.23
C   SUBROUTINES.                                                                 PLTTER.24
C   CALLED BY SUBROUTINE FINPRT.                                                 PLTTER.25
C                                                                                PLTTER.26
C   COMMENTS :                                                                   PLTTER.27
C IN ADDITION TO THE LIST OF ARGUMENTS, SUBROUTINE PLTTER GETS                   PLTTER.28
C ADDITIONAL INFORMATION FROM THE COMMON BLOCKS /MAPS/, /NESTDMN/,               PLTTER.29
C AND /OPTION/. THERE IS NO RAWINSOUND STATION PLOT GENERATED FOR                PLTTER.30
C MERCATOR PROJECTION. THE FOLLOWING TABLE SHOWS THE TYPE OF PLOTS               PLTTER.31
C FOR THE DOMAINS ON THE DIFFERENT NEST LEVELS.                                  PLTTER.32
C                                                                                PLTTER.33
C PLOT AND DOMAIN LEVEL      1        2 TO MAXLVL-1        MAXLVL                PLTTER.34
C MAP BACKGROUND             YES      YES                  YES                   PLTTER.35
C TERRAIN HEIGHT             YES      YES                  YES                   PLTTER.36
C LAND-USE                   YES      YES                  YES                   PLTTER.37
C GRID MESH                  YES      YES                  YES                   PLTTER.38
C RAWINSOUND STATIONS        YES      YES                  YES                   PLTTER.39
C NESTED DOMAINS SETTING     YES      YES                  NO                    PLTTER.40
C                                                                                PLTTER.41
C IF IFILL = .TRUE., THE COLOR-FILLED MAP BACKGROUND, TERRAIN HEIGHT             PLTTER.42
C AND LAND-USE FIELDS WILL BE GENERATED.                                         PLTTER.43
C THE TOTAL NUMBER OF THE NEST LEVELS IS 'MAXLVL', THE COARSE                    PLTTER.44
C DOMAIN IS ON THE LEVEL 1, AND THE FINEST DOMAINS ARE ON THE LEVEL              PLTTER.45
C 'MAXLVL'.                                                                      PLTTER.46
# include <parame.incl>                                                          PLTTER.47
# include <paramed.incl>                                                         06NOV00.156
# include <io.incl>                                                              06NOV00.157
# include <maps.incl>                                                            PLTTER.48
# include <nestdmn.incl>                                                         PLTTER.49
# include <option.incl>                                                          PLTTER.50
# include <paramesv.incl>                                                        PLTTER.51
# include <vs_cmn1.incl>                                                         PLTTER.52
# include <vs_cmn2.incl>                                                         PLTTER.53
C                                                                                PLTTER.54
      DIMENSION GRDOUT(JXX,IYY), XLAND(JJMX,IIMX)                                PLTTER.55
                                                                                 PLTTER.56
      DIMENSION    GRIDIN(IMX,JMX),  LNDXX(IMX,JMX)                              PLTTER.57
      DIMENSION VEGEC(IMX,JMX,NVC), SOILC(IMX,JMX,NSC),                          PLTTER.58
     >          VEGEF(IMX,JMX,12), TEMPG(IMX,JMX)   ,                            PLTTER.59
     >          LVEGE(IMX,JMX)   , LSOIL(IMX,JMX),                               PLTTER.60
     >          WLMSK(IMX,JMX),  NUMPOINTS(NCMAX)                                PLTTER.61
C                                                                                PLTTER.62
      CHARACTER*2  LANDCHAR                                                      PLTTER.63
      CHARACTER*3  DOMCHAR                                                       PLTTER.64
      CHARACTER*80 CDUM                                                          PLTTER.65
      REAL  NORTH, LEVELS(100)        ! LEVELS FOR CONDRV                        PLTTER.66
      CHARACTER*80 TITLE              ! TITLE STRING                             PLTTER.67
      INTEGER ERRSEV                                                             PLTTER.68
      CHARACTER *5 SSTA                                                          PLTTER.69
      CHARACTER *3 GRIDLOC                                                       PLTTER.70
      DIMENSION XRA(4),YRA(4),DST(10),IND(10)                                    PLTTER.71
      DIMENSION IAS(13)                                                          PLTTER.72
      DATA IAS /13*1/                                                            PLTTER.73
C                                                                                PLTTER.74
#ifdef NCARG                                                                     PLTTER.75
      DSKM = DIS(N)                                                              PLTTER.76
      IR = NRATIO(N)                                                             PLTTER.77
      XIR = FLOAT(IR)                                                            PLTTER.78
      JEND = (NESTJX(1)-1)*IR + 1                                                PLTTER.79
      IEND = (NESTIX(1)-1)*IR + 1                                                PLTTER.80
      ISTART = 1                                                                 PLTTER.81
      JSTART = 1                                                                 PLTTER.82
        JEX = 0                                                                  PLTTER.83
        IEX = 0                                                                  PLTTER.84
      IF (N.EQ.1) THEN                                                           PLTTER.85
        JEX = JOFFST                                                             PLTTER.86
        IEX = IOFFST                                                             PLTTER.87
      ENDIF                                                                      PLTTER.88
      IF (N.EQ.1) THEN                                                           PLTTER.89
      XA = 1.0                                                                   PLTTER.90
      YA = 1.0                                                                   PLTTER.91
      XB = FLOAT(JEND)                                                           PLTTER.92
      YB = FLOAT(IEND)                                                           PLTTER.93
      ELSE                                                                       PLTTER.94
      XA = (XWEST(N) - FLOAT(JOFFST) - 1.)*XIR + 1.                              PLTTER.95
      XB = (XEAST(N) - FLOAT(JOFFST) - 1.)*XIR + 1.                              PLTTER.96
      YA = (XSOUTH(N) -FLOAT(IOFFST) - 1.)*XIR + 1.                              PLTTER.97
      YB = (XNORTH(N) -FLOAT(IOFFST) - 1.)*XIR + 1.                              PLTTER.98
      ENDIF                                                                      PLTTER.99
C                                                                                PLTTER.100
C  AVOID THE XA, XB, YA, YB ARE OUTSIDE THE BIG DOMAIN:                          PLTTER.101
      IF (XA.LT.1.0) THEN                                                        PLTTER.102
          XA = 1.0                                                               PLTTER.103
          PRINT *,'XA < 1.0, XA=1.0'                                             PLTTER.104
          STOP                                                                   PLTTER.105
      ENDIF                                                                      PLTTER.106
      IF (XB.GT.FLOAT(JEND)) THEN                                                PLTTER.107
          XB = FLOAT(JEND)                                                       PLTTER.108
          PRINT *,'XB > JEND, XB = FLOAT(JEND)'                                  PLTTER.109
          STOP                                                                   PLTTER.110
      ENDIF                                                                      PLTTER.111
      IF (YA.LT.1.0) THEN                                                        PLTTER.112
          YA = 1.0                                                               PLTTER.113
          PRINT *,'YA < 1.0, YA = 1.0'                                           PLTTER.114
          STOP                                                                   PLTTER.115
      ENDIF                                                                      PLTTER.116
      IF (YB.GT.FLOAT(IEND)) THEN                                                PLTTER.117
          YB = FLOAT(IEND)                                                       PLTTER.118
          PRINT *,'YB > IEND, YB = FLOAT(IEND)'                                  PLTTER.119
          STOP                                                                   PLTTER.120
      ENDIF                                                                      PLTTER.121
C                                                                                PLTTER.122
      IF(IPRINT.EQ.1) THEN                                                       PLTTER.123
      PRINT *,'*** CALL PLTTER FOR DOMAIN ', N                                   06NOV00.158
      PRINT 2001, N,ISTART,IEND,JSTART,JEND,XA,XB,YA,YB                          PLTTER.124
2001  FORMAT(/2X,'--- CALL PLTTER ..........',I3                                 PLTTER.125
     1    /2X,'ISTART,IEND,JSTART,JEND:',4I6,3X,'XA,XB,YA,YB:',4F8.1)            PLTTER.126
      ENDIF                                                                      PLTTER.127
                                                                                 PLTTER.128
C     WRITE(TITLE(1:9),22) N                                                     06NOV00.159
 22   FORMAT('DOMAIN ',I2)                                                       PLTTER.130
C     WRITE(6,23) TITLE                                                          06NOV00.160
 23   FORMAT('TITLE= ',A9)                                                       PLTTER.132
                                                                                 06NOV00.161
C  OPEN THE COLOR TABLE FILE                                                     PLTTER.133
                                                                                 PLTTER.134
      MAPUNIT=17                                                                 PLTTER.135
                                                                                 PLTTER.136
      IF(IFILL) THEN                                                             PLTTER.137
                                                                                 PLTTER.138
C  OPEN THE CORRECT TABLE FILE                                                   PLTTER.139
                                                                                 PLTTER.140
      OPEN (MAPUNIT,FILE='mapfi.tbl',STATUS='OLD')                               PLTTER.141
                                                                                 PLTTER.142
C  SET UP A COLOR TABLE                                                          PLTTER.143
                                                                                 PLTTER.144
      CALL RDCOLT(MAPUNIT)                                                       PLTTER.145
                                                                                 PLTTER.146
C  TITLE                                                                         06NOV00.162
                                                                                 06NOV00.163
      CALL GSTXCI (16)                                                           06NOV00.164
      CALL GSPLCI (16)                                                           06NOV00.165
                                                                                 06NOV00.166
      IF (IPRINT.EQ.1) PRINT *, '*** N = ', n                                    06NOV00.167
      CALL SET (0.0,1.0,0.0,1.0,0.0,100.,0.0,100.,1)                             06NOV00.168
      TITLE(1:30)='MAP OF DOMAIN 1 (NON-EXPANDED)'                               06NOV00.169
      IF (N.GE.2) THEN                                                           06NOV00.170
         WRITE (TITLE(1:30),90) N                                                06NOV00.171
90       FORMAT('       MAP OF DOMAIN',I2,'       ')                             06NOV00.172
      END IF                                                                     06NOV00.173
      CALL PLCHHQ(50.,95.,TITLE(1:30),.02,0.,0.)                                 06NOV00.174
                                                                                 06NOV00.175
C  MAKE THE BLACK AND WHITE MAP                                                  PLTTER.147
                                                                                 PLTTER.148
C     ... TURN OF CLIPPING                                                       PLTTER.149
                                                                                 PLTTER.150
      CALL GSCLIP (0)                                                            PLTTER.151
                                                                                 PLTTER.152
C     ... USE WHOLE DOMAIN                                                       PLTTER.153
                                                                                 PLTTER.154
      CALL SET (0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)                               PLTTER.155
                                                                                 PLTTER.156
C                                                                                PLTTER.157
C     ... ERRSEV: <0 ABORT ON WARNING, =0 ABORT ON ERROR, >0 NO ABORT            PLTTER.158
                                                                                 PLTTER.159
      ERRSEV=1                                                                   PLTTER.160
      N2=N+70                                                                    PLTTER.161
                                                                                 PLTTER.162
      CALL MAPDRV (PROJECT,TRUELAT1,TRUELAT2,PHIC,XLONC,                         PLTTER.163
     *             DSKM,JEND,IEND,XA,XB,YA,YB,                                   PLTTER.164
     *             TITLE,9,N2,MAPUNIT,.FALSE.,ERRSEV)                            PLTTER.165
                                                                                 PLTTER.166
C  CLOSE THE TABLE FILE                                                          PLTTER.167
                                                                                 PLTTER.168
      CLOSE (MAPUNIT)                                                            PLTTER.169
C                                                                                PLTTER.170
C     ... NICE ORDINARY MAP                                                      PLTTER.171
C                                                                                PLTTER.172
      CALL GFLAS3(N2)                                                            PLTTER.173
      CALL FRAME                                                                 PLTTER.174
      ENDIF                                                                      PLTTER.175
                                                                                 PLTTER.176
C  OPEN THE CORRECT TABLE FILE                                                   PLTTER.177
                                                                                 PLTTER.178
      OPEN (MAPUNIT,FILE='map.tbl',STATUS='OLD')                                 PLTTER.179
                                                                                 PLTTER.180
C  SET UP A COLOR TABLE                                                          PLTTER.181
                                                                                 PLTTER.182
      CALL RDCOLT(MAPUNIT)                                                       PLTTER.183
                                                                                 PLTTER.184
C  TITLE                                                                         06NOV00.176
                                                                                 06NOV00.177
      CALL GSTXCI (16)                                                           06NOV00.178
      CALL GSPLCI (16)                                                           06NOV00.179
                                                                                 06NOV00.180
      IF (.NOT.IFILL) THEN                                                       06NOV00.181
      CALL SET (0.0,1.0,0.0,1.0,0.0,100.,0.0,100.,1)                             06NOV00.182
      TITLE(1:30)='MAP OF DOMAIN 1 (NON-EXPANDED)'                               06NOV00.183
      IF (N.GE.2) THEN                                                           06NOV00.184
         WRITE (TITLE(1:30),90) N                                                06NOV00.185
      END IF                                                                     06NOV00.186
      CALL PLCHHQ(50.,95.,TITLE(1:30),.02,0.,0.)                                 06NOV00.187
      END IF                                                                     06NOV00.188
                                                                                 06NOV00.189
C  MAKE THE BLACK AND WHITE MAP                                                  PLTTER.185
                                                                                 PLTTER.186
C     ... TURN OF CLIPPING                                                       PLTTER.187
                                                                                 PLTTER.188
      CALL GSCLIP (0)                                                            PLTTER.189
                                                                                 PLTTER.190
C     ... USE WHOLE DOMAIN                                                       PLTTER.191
                                                                                 PLTTER.192
      CALL SET (0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)                               PLTTER.193
                                                                                 PLTTER.194
C     ... ERRSEV: <0 ABORT ON WARNING, =0 ABORT ON ERROR, >0 NO ABORT            PLTTER.195
                                                                                 PLTTER.196
      ERRSEV=1                                                                   PLTTER.197
      N1=N+50                                                                    PLTTER.198
                                                                                 PLTTER.199
      CALL MAPDRV (PROJECT,TRUELAT1,TRUELAT2,PHIC,XLONC,                         PLTTER.200
     *             DSKM,JEND,IEND,XA,XB,YA,YB,                                   PLTTER.201
     *             TITLE,9,N1,MAPUNIT,.FALSE.,ERRSEV)                            PLTTER.202
                                                                                 PLTTER.203
C  CLOSE THE TABLE FILE                                                          PLTTER.204
                                                                                 PLTTER.205
      CLOSE (MAPUNIT)                                                            PLTTER.206
C                                                                                PLTTER.207
C     ... NICE ORDINARY MAP                                                      PLTTER.208
C                                                                                PLTTER.209
      IF(.NOT.IFILL) THEN                                                        PLTTER.210
       CALL GFLAS3(N1)                                                           PLTTER.211
       CALL FRAME                                                                PLTTER.212
      ENDIF                                                                      PLTTER.213
C                                                                                PLTTER.214
      CALL GETSET(XSA1,XSB1,YSA1,YSB1,XSC1,XSD1,YSC1,YSD1,LTYPE1)                PLTTER.215
C                                                                                PLTTER.216
      PRINT *,'1, -----> MAP PLOTTED'                                            PLTTER.217
C                                                                                PLTTER.218
C  IF NO TERRAIN                                                                 PLTTER.219
C                                                                                PLTTER.220
      IF (.NOT.IFTER) GO TO 501                                                  PLTTER.221
C                                                                                PLTTER.222
      DO 50 I=1,IYY-1                                                            PLTTER.223
      DO 50 J=1,JXX-1                                                            PLTTER.224
      GRDOUT(J,I) = GRIDIN(IEX+I,JEX+J)                                          PLTTER.225
      IF((GRDOUT(J,I).LT.0.).AND.(GRDOUT(J,I).GT.-1.))                           PLTTER.226
     *  GRDOUT(J,I)=-200.                                                        PLTTER.227
   50 CONTINUE                                                                   PLTTER.228
                                                                                 PLTTER.229
C  TURN OFF CLIPPING                                                             PLTTER.230
                                                                                 PLTTER.231
      CALL GSCLIP (0)                                                            PLTTER.232
                                                                                 PLTTER.233
      IF(IFILL) THEN                                                             PLTTER.234
                                                                                 PLTTER.235
C  OPEN UNIT MAPUNIT AS TABLE TO READ CONDRV INFORMATION FROM                    PLTTER.236
                                                                                 PLTTER.237
         OPEN (MAPUNIT,FILE='confi.tbl',STATUS='OLD')                            PLTTER.238
                                                                                 PLTTER.239
C  SET UP A COLOR TABLE                                                          PLTTER.240
                                                                                 PLTTER.241
         CALL RDCOLT (MAPUNIT)                                                   PLTTER.242
                                                                                 PLTTER.243
C  DEFINE TITLE                                                                  PLTTER.244
                                                                                 PLTTER.245
         CALL GSTXCI (16)                                                        06NOV00.190
         CALL GSPLCI (16)                                                        06NOV00.191
                                                                                 06NOV00.192
         CALL SET (0.0,1.0,0.0,1.0,0.0,100.,0.0,100.,1)                          06NOV00.193
         CALL PLCHHQ(50.,95.,'TERRAIN HEIGHT IN COLOR',.02,0.,0.)                06NOV00.194
                                                                                 06NOV00.195
         XINC=1./FLOAT(JXX) * 0.5                                                PLTTER.246
         CALL SET (XINC,1.-XINC,XINC,1.-XINC,                                    PLTTER.247
     *          0.0,1.0,0.0,1.0,1)                                               PLTTER.248
                                                                                 PLTTER.249
         LEVELS(1)=FIN(N)                                                        PLTTER.250
         LEVELS(2)=6000.                                                         PLTTER.251
         LEVELS(3)=0.                                                            PLTTER.252
                                                                                 PLTTER.253
         CALL CONDRV (GRDOUT,JXX,IYY,1,1,JXX-1,IYY-1,1,-2,                       PLTTER.254
     *                LEVELS,.TRUE.,                                             PLTTER.255
     *                'NO',1.0,TITLE,0,MAPUNIT,0,0)                              PLTTER.256
                                                                                 PLTTER.257
C  CLOSE THE TABLES                                                              PLTTER.258
                                                                                 PLTTER.259
         CLOSE (MAPUNIT)                                                         PLTTER.260
                                                                                 PLTTER.261
         CALL GFLAS3(N1)                                                         PLTTER.262
         CALL FRAME                                                              PLTTER.263
         PRINT *,'2, -----> COLOR TERRAIN HEIGHT PLOTTED'                        PLTTER.264
C                                                                                PLTTER.265
      ENDIF                                                                      PLTTER.266
                                                                                 PLTTER.267
C  OPEN UNIT MAPUNIT AS TABLE TO READ CONDRV INFORMATION FROM                    PLTTER.268
                                                                                 PLTTER.269
      OPEN (MAPUNIT,FILE='con.tbl',STATUS='OLD')                                 PLTTER.270
                                                                                 PLTTER.271
C  SET UP A COLOR TABLE                                                          PLTTER.272
                                                                                 PLTTER.273
      CALL RDCOLT (MAPUNIT)                                                      PLTTER.274
                                                                                 PLTTER.275
C  DEFINE TITLE                                                                  PLTTER.276

      CALL GSTXCI (16)                                                           06NOV00.196
      CALL GSPLCI (16)                                                           06NOV00.197
                                                                                 06NOV00.198
      CALL SET (0.0,1.0,0.0,1.0,0.0,100.,0.0,100.,1)                             06NOV00.199
      CALL PLCHHQ(50.,95.,'TERRAIN HEIGHT IN B/W',.02,0.,0.)                     06NOV00.200
                                                                                 PLTTER.277
      XINC=1./FLOAT(JXX) * 0.5                                                   PLTTER.278
      CALL SET (XINC,1.-XINC,XINC,1.-XINC,                                       PLTTER.279
     *          0.0,1.0,0.0,1.0,1)                                               PLTTER.280
                                                                                 PLTTER.281
C  DO THE PLOT                                                                   PLTTER.282
                                                                                 PLTTER.283
         LEVELS(1)=FIN(N)                                                        PLTTER.284
         LEVELS(2) = 6000.                                                       PLTTER.285
         LEVELS(3) = 0.                                                          PLTTER.286
C                                                                                PLTTER.287
      CALL CONDRV (GRDOUT,JXX,IYY,1,1,JXX-1,IYY-1,1,-1,                          PLTTER.288
     *             LEVELS,.TRUE.,                                                PLTTER.289
     *             'NO',1.0,TITLE,0,-1,0,0)                                      PLTTER.290
   51 CONTINUE                                                                   PLTTER.291
C                                                                                PLTTER.292
C  CLOSE THE TABLES                                                              PLTTER.293
                                                                                 PLTTER.294
      CLOSE (MAPUNIT)                                                            PLTTER.295
                                                                                 PLTTER.296
      CALL GFLAS3(N1)                                                            PLTTER.297
      CALL FRAME                                                                 PLTTER.298
C                                                                                PLTTER.299
      PRINT *,'3, -----> TERRAIN HEIGHT PLOTTED'                                 PLTTER.300
C                                                                                PLTTER.301
C                                                                                PLTTER.302
C.....WRITE OUT LANDUSE VALUES ON MAP, LEAVING WATER CATEGORY BLANK              PLTTER.303
C                                                                                PLTTER.304
C MAP IS ON DOT GRID; TERRAIN/LANDUSE ARE ON CROSS                               PLTTER.305
C SO SHIFT CORNER POINTS ON MAP SO YOU ARE PLOTTING 1 TO ICROSS                  PLTTER.306
C AND 1 TO JCROSS PROPERLY --- RECALL SET TO VOID LAST CALL IN CONREC            PLTTER.307
C                                                                                PLTTER.308
      IF (ISURF) THEN                                                            PLTTER.309
                                                                                 PLTTER.310
      DO 950 I=1,IYY                                                             PLTTER.311
      DO 950 J=1,JXX                                                             PLTTER.312
         XLAND(J,I)=FLOAT(LNDXX(IEX+I,JEX+J)) + 0.001                            PLTTER.313
950   CONTINUE                                                                   PLTTER.314
                                                                                 PLTTER.315
      IF (.NOT.VEGESOIL) THEN                                                    PLTTER.316
                                                                                 PLTTER.317
C  OPEN UNIT MAPUNIT AS TABLE TO READ CONDRV INFORMATION FROM                    PLTTER.318
      OPEN (MAPUNIT,FILE='luco.tbl',STATUS='OLD')                                PLTTER.319
                                                                                 PLTTER.320
C  SET UP A COLOR TABLE                                                          PLTTER.321
      CALL RDCOLT (MAPUNIT)                                                      PLTTER.322
                                                                                 PLTTER.323
C  TURN OFF CLIPPING                                                             PLTTER.324
      CALL GSCLIP (0)                                                            PLTTER.325
                                                                                 PLTTER.326
C  GET THE REVIOUS SET CALL                                                      PLTTER.327
      CALL GETSET (XA2,XB2,YA2,YB2,XUA2,XUB2,YUA2,YUB2,NTYPEPLT)                 PLTTER.328

C  TITLE                                                                         06NOV00.201
      CALL GSTXCI (16)                                                           06NOV00.202
      CALL GSPLCI (16)                                                           06NOV00.203
                                                                                 06NOV00.204
      CALL SET (0.0,1.0,0.0,1.0,0.0,100.,0.0,100.,1)                             06NOV00.205
      CALL PLCHHQ(50.,95.,'OLD LAND-USE TYPE',.02,0.,0.)                         06NOV00.206
                                                                                 PLTTER.329
C        XINC=1./FLOAT(JXX) * 0.5                                                PLTTER.330
         CALL SET (XA2,XB2,YA2,YB2,                                              PLTTER.331
     *             1.0,FLOAT(JXX-1),1.0,FLOAT(IYY-1),NTYPEPLT)                   PLTTER.332
C        PRINT *,'XA2,XB2,YA2,YB2= ',XA2,XB2,YA2,YB2                             PLTTER.333
                                                                                 PLTTER.334
      IF(IFILL) THEN                                                             PLTTER.335
C       ... PUT ON OLD 13-CATEGORY LAND USE COLORS                               PLTTER.336
                                                                                 PLTTER.337
      DO 960 I=1,IYY-1                                                           PLTTER.338
      DO 960 J=1,JXX-1                                                           PLTTER.339
      ICI=NINT(XLAND(J,I))+17                                                    PLTTER.340
      XRA(1)=FLOAT(J)-.5                                                         PLTTER.341
      XRA(2)=FLOAT(J+1)-.5                                                       PLTTER.342
      XRA(3)=FLOAT(J+1)-.5                                                       PLTTER.343
      XRA(4)=FLOAT(J)-.5                                                         PLTTER.344
      YRA(1)=FLOAT(I)-.5                                                         PLTTER.345
      YRA(2)=FLOAT(I)-.5                                                         PLTTER.346
      YRA(3)=FLOAT(I+1)-.5                                                       PLTTER.347
      YRA(4)=FLOAT(I+1)-.5                                                       PLTTER.348
      CALL SFSGFA(XRA,YRA,4,DST,10,IND,10,ICI)                                   PLTTER.349
960   CONTINUE                                                                   PLTTER.350
                                                                                 PLTTER.351
C  PUT ON LABEL BAR                                                              PLTTER.352
      CALL LABEL                                                                 PLTTER.353
                                                                                 PLTTER.354
      ELSE                                                                       PLTTER.355
C       ... PUT ON OLD 13-CATEGROY LAND USE NUMBERS                              PLTTER.356
                                                                                 PLTTER.357
      DO 965 I=1,IYY-1                                                           PLTTER.358
      DO 965 J=1,JXX-1                                                           PLTTER.359
         LANDINT=NINT(XLAND(J,I))                                                PLTTER.360
         WRITE (LANDCHAR,'(I2)') LANDINT                                         PLTTER.361
         IF (LANDINT.EQ.7) THEN                                                  PLTTER.362
            CALL GSTXCI (23)                                                     PLTTER.363
            CALL GSPLCI (23)                                                     PLTTER.364
         ELSE                                                                    PLTTER.365
            CALL GSTXCI (19)                                                     PLTTER.366
            CALL GSPLCI (19)                                                     PLTTER.367
         ENDIF                                                                   PLTTER.368
         IF (LANDINT.LE.9) THEN                                                  PLTTER.369
            CALL PLCHLQ(FLOAT(J),FLOAT(I),LANDCHAR(2:2),7.,0.,0.)                PLTTER.370
         ELSE                                                                    PLTTER.371
            CALL PLCHLQ(FLOAT(J),FLOAT(I),LANDCHAR,4.,0.,0.)                     PLTTER.372
         ENDIF                                                                   PLTTER.373
965   CONTINUE                                                                   PLTTER.374
C                                                                                PLTTER.375
      CALL GSTXCI (27)                                                           PLTTER.376
      CALL GSPLCI (27)                                                           PLTTER.377
      XXEND=JXX-.5                                                               PLTTER.378
      DO 967 I=1,IYY-2                                                           PLTTER.379
         CALL LINE(.5,FLOAT(I)+.5,XXEND,FLOAT(I)+.5)                             PLTTER.380
  967 CONTINUE                                                                   PLTTER.381
      YYEND=IYY-.5                                                               PLTTER.382
      DO 968 J=1,JXX-2                                                           PLTTER.383
         CALL LINE(FLOAT(J)+.5,.5,FLOAT(J)+.5,YYEND)                             PLTTER.384
  968 CONTINUE                                                                   PLTTER.385
C                                                                                PLTTER.386
      ENDIF                                                                      PLTTER.387
                                                                                 PLTTER.388
      ELSE                                                                       PLTTER.389
                                                                                 PLTTER.390
C  OPEN UNIT MAPUNIT AS TABLE TO READ CONDRV INFORMATION FROM                    PLTTER.391
      OPEN (MAPUNIT,FILE=VEGECOFILE,STATUS='OLD')                                PLTTER.392
                                                                                 PLTTER.393
C  SET UP A COLOR TABLE                                                          PLTTER.394
      CALL RDCOLT (MAPUNIT)                                                      PLTTER.395
                                                                                 PLTTER.396
C  TURN OFF CLIPPING                                                             PLTTER.397
      CALL GSCLIP (0)                                                            PLTTER.398
                                                                                 PLTTER.399
C  GET THE REVIOUS SET CALL                                                      PLTTER.400
      CALL GETSET (XA2,XB2,YA2,YB2,XUA2,XUB2,YUA2,YUB2,NTYPEPLT)                 PLTTER.401
                                                                                 PLTTER.402
C  TITLE                                                                         06NOV00.207
      CALL GSTXCI (16)                                                           06NOV00.208
      CALL GSPLCI (16)                                                           06NOV00.209
                                                                                 06NOV00.210
      CALL SET (0.0,1.0,0.0,1.0,0.0,100.,0.0,100.,1)                             06NOV00.211
      CALL PLCHHQ(50.,95.,'DOMINANT VEGECAT/NEW LANDUSE TYPE',                   06NOV00.212
     *           .02,0.,0.)                                                      06NOV00.213

         CALL SET (XA2,XB2,YA2,YB2,                                              PLTTER.403
     *             1.0,FLOAT(JXX-1),1.0,FLOAT(IYY-1),NTYPEPLT)                   PLTTER.404
C        PRINT *,'XA2,XB2,YA2,YB2= ',XA2,XB2,YA2,YB2                             PLTTER.405
                                                                                 PLTTER.334
      IF(IFILL) THEN                                                             PLTTER.407
C       ... PUT ON NEW 16/24-CATEGORY LAND USE COLORS                            PLTTER.408
                                                                                 PLTTER.409
      DO I=1,IYY-1                                                               PLTTER.410
      DO J=1,JXX-1                                                               PLTTER.411
      ICI=LNDXX(IEX+I,JEX+J)                                                     PLTTER.412
C     ICI=NINT(XLAND(J,I))                                                       PLTTER.413
      IF (ICI.EQ.NVC) THEN                                                       PLTTER.414
        ICI =     18                             ! NO VEGETATION DATA            PLTTER.415
      ELSE                                                                       PLTTER.416
        ICI = ICI+18                                                             PLTTER.417
      ENDIF                                                                      PLTTER.418
      XRA(1)=FLOAT(J)-.5                                                         PLTTER.419
      XRA(2)=FLOAT(J+1)-.5                                                       PLTTER.420
      XRA(3)=FLOAT(J+1)-.5                                                       PLTTER.421
      XRA(4)=FLOAT(J)-.5                                                         PLTTER.422
      YRA(1)=FLOAT(I)-.5                                                         PLTTER.423
      YRA(2)=FLOAT(I)-.5                                                         PLTTER.424
      YRA(3)=FLOAT(I+1)-.5                                                       PLTTER.425
      YRA(4)=FLOAT(I+1)-.5                                                       PLTTER.426
      CALL SFSGFA(XRA,YRA,4,DST,10,IND,10,ICI)                                   PLTTER.427
      END DO                                                                     PLTTER.428
      END DO                                                                     PLTTER.429
                                                                                 PLTTER.430
C  PUT ON LABEL BAR                                                              PLTTER.431
      CALL LABELV                                                                PLTTER.432
                                                                                 PLTTER.433
      ELSE                                                                       PLTTER.434
C       ... PUT ON NEW 16/24 CATEGORY LAND USE NUMBERS                           PLTTER.435
                                                                                 PLTTER.436
      DO I=1,IYY-1                                                               PLTTER.437
      DO J=1,JXX-1                                                               PLTTER.438
         LANDINT= LNDXX(IEX+I,JEX+J)                                             PLTTER.439
C        LANDINT= NINT(XLAND(J,I))                                               PLTTER.440
         WRITE (LANDCHAR,'(I2)') LANDINT                                         PLTTER.441
         IF (LANDINT.EQ.IVW(5)) THEN                                             PLTTER.442
            CALL GSTXCI (33)                                                     PLTTER.443
            CALL GSPLCI (33)                                                     PLTTER.444
         ELSE                                                                    PLTTER.445
            CALL GSTXCI (20)                                                     PLTTER.446
            CALL GSPLCI (20)                                                     PLTTER.447
         ENDIF                                                                   PLTTER.448
         IF (LANDINT.LE.9) THEN                                                  PLTTER.449
            CALL PLCHLQ(FLOAT(J),FLOAT(I),LANDCHAR(2:2),7.,0.,0.)                PLTTER.450
         ELSE                                                                    PLTTER.451
            CALL PLCHLQ(FLOAT(J),FLOAT(I),LANDCHAR,4.,0.,0.)                     PLTTER.452
         ENDIF                                                                   PLTTER.453
      END DO                                                                     PLTTER.454
      END DO                                                                     PLTTER.455
C                                                                                PLTTER.456
      CALL GSTXCI (28)                                                           PLTTER.457
      CALL GSPLCI (28)                                                           PLTTER.458
      XXEND=JXX-.5                                                               PLTTER.459
      DO I=1,IYY-2                                                               PLTTER.460
         CALL LINE(.5,FLOAT(I)+.5,XXEND,FLOAT(I)+.5)                             PLTTER.461
      END DO                                                                     PLTTER.462
      YYEND=IYY-.5                                                               PLTTER.463
      DO J=1,JXX-2                                                               PLTTER.464
         CALL LINE(FLOAT(J)+.5,.5,FLOAT(J)+.5,YYEND)                             PLTTER.465
      END DO                                                                     PLTTER.466
C                                                                                PLTTER.467
      ENDIF                                                                      PLTTER.468
                                                                                 PLTTER.469
      ENDIF                                                                      PLTTER.470
                                                                                 PLTTER.471
      CALL GFLAS3(N1)                                                            PLTTER.472
      CALL FRAME                                                                 PLTTER.473
      CLOSE (MAPUNIT)                                                            PLTTER.474
C                                                                                PLTTER.475
      PRINT *,'4, -----> LANDUSE PLOTTED'                                        PLTTER.476
      END IF                                                                     PLTTER.477
C                                                                                PLTTER.478
501   CONTINUE                                                                   PLTTER.479
C                                                                                PLTTER.480
C     ... PUT ON LOCATIONS OF GRID                                               PLTTER.481
C                                                                                PLTTER.482
      DO imeshtwo=1,2                                                            23DEC04.7
      CALL SET(XSA1,XSB1,YSA1,YSB1,1.,FLOAT(JXX),1.,FLOAT(IYY),LTYPE1)           PLTTER.483
      CALL PERIM (JXX-1,1,IYY-1,1)                                               PLTTER.484
      CALL GSCLIP (0)                                                            PLTTER.485
      CALL GSTXCI (N+8)                                                          PLTTER.486
      CALL GSPLCI (N+8)                                                          PLTTER.487
      WRITE (DOMCHAR,11) N                                                       PLTTER.488
  11  FORMAT('D',I2.2)                                                           PLTTER.489
      CALL PLCHLQ(6.,FLOAT(NESTIX(N))-4.,                                        PLTTER.490
     *      DOMCHAR, 16./1023.,0.,0.)                                            PLTTER.491
      DO 400 IXL=5,JXX,5                                                         PLTTER.492
         CALL LINE (FLOAT(IXL),1.,FLOAT(IXL),FLOAT(IYY))                         PLTTER.493
         WRITE (GRIDLOC,'(I3)') IXL                                              PLTTER.494
         CALL PLCHLQ(FLOAT(IXL),-2.,GRIDLOC,0.01,0.,0.)                          PLTTER.495
         CALL PLCHLQ(FLOAT(IXL),FLOAT(IYY+2),GRIDLOC,0.01,0.,0.)                 PLTTER.496
400   CONTINUE                                                                   PLTTER.497
      DO 410 IYL=5,IYY,5                                                         PLTTER.498
         CALL LINE (1.,FLOAT(IYL),FLOAT(JXX),FLOAT(IYL))                         PLTTER.499
         WRITE (GRIDLOC,'(I3)') IYL                                              PLTTER.500
         CALL PLCHLQ(-1.,FLOAT(IYL),GRIDLOC,0.01,0.,1.)                          PLTTER.501
         CALL PLCHLQ(FLOAT(JXX+1),FLOAT(IYL),GRIDLOC,0.01,0.,-1.)                PLTTER.502
410   CONTINUE                                                                   PLTTER.503
      DO 420 IXL=1,JXX                                                           PLTTER.504
         IF(MOD(IXL,5).EQ.0) GOTO 420                                            PLTTER.505
         DO 419 IYL=1,IYY                                                        PLTTER.506
            IF(MOD(IYL,5).NE.0) CALL POINT(FLOAT(IXL),FLOAT(IYL))                PLTTER.507
419      CONTINUE                                                                PLTTER.508
420   CONTINUE                                                                   PLTTER.509
      CALL SET(XSA1,XSB1,YSA1,YSB1,XSC1,XSD1,YSC1,YSD1,LTYPE1)                   PLTTER.510
C      CALL SET(XSA,XSB,YSA,YSB,XSC,XSD,YSC,YSD,LTYPE)                           PLTTER.511
      CALL GSCLIP(0)                                                             PLTTER.512
      IF (IMESHTWO .eq. 2) THEN                                                  23DEC04.8
        CALL GFLAS3(N1)                                                          23DEC04.9
      ENDIF                                                                      23DEC04.10
      CALL FRAME                                                                 PLTTER.513
      ENDDO                                                                      23DEC04.11
C                                                                                PLTTER.514
        PRINT *,'5, -----> MESH PLOTTED'                                         23DEC04.12
C                                                                                PLTTER.516
C                                                                                PLTTER.517
C        ... PUT ON REPRESENTATIVE STATIONS FOR MAPBKG RESPONSIBILITIES          PLTTER.518
C                                                                                PLTTER.519
      CALL GFLAS3(N1)                                                            PLTTER.520
         REWIND (16)                                                             PLTTER.521
         CALL GETSET(XSA,XSB,YSA,YSB,XSC,XSD,YSC,YSD,LTYPE)                      PLTTER.522
C        PRINT *,'XSA,XSB,YSA,YSB,IEND,JEND= ',                                  PLTTER.523
C    *XSA,XSB,YSA,YSB,IEND,JEND                                                  PLTTER.524
         CALL SET(XSA,XSB,YSA,YSB,1.,FLOAT(JXX),1.,FLOAT(IYY),LTYPE)             PLTTER.525
         DIST=AMAX1(FLOAT(JXX),FLOAT(IYY))                                       PLTTER.526
         DELT=DIST/50.                                                           PLTTER.527
         CALL GSCLIP(0)                                                          PLTTER.528
         CALL GSPMCI(8+N)                                                        PLTTER.529
         CALL GSTXCI(8+N)                                                        PLTTER.530
         CALL GSPLCI(8+N)                                                        PLTTER.531
         NEAR=(IMX-IYY)/2                                                        PLTTER.532
310      CONTINUE                                                                PLTTER.533
         READ(16,END=320) YLAT,YLON,SSTA                                         PLTTER.534
         CALL LLXY(YLAT,YLON,XX,YY,IEND,JEND,DSKM)                               PLTTER.535
C  GET BACK TO THE NEST DOMAIN COORDINATE                                        PLTTER.536
C        IF(SSTA.EQ.'72270')                                                     06NOV00.214
C    *    PRINT *,'XX,YY,IEND,JEND,XA,YA= ',                                     06NOV00.215
C    *    XX,YY,IEND,JEND,XA,YA                                                  06NOV00.216
         XX=XX-XA+1.                                                             PLTTER.540
         YY=YY-YA+1.                                                             PLTTER.541
         IF((XX+NEAR.GT.1).AND.(XX-NEAR.LT.JXX).AND.                             PLTTER.542
     *      (YY+NEAR.GT.1).AND.(YY-NEAR.LT.IYY)) THEN                            PLTTER.543
            CALL POINTS(XX,YY,1,-4,0)                                            PLTTER.544
            CALL PLCHLQ(XX,YY+DELT,SSTA,0.01,0.,0.)                              PLTTER.545
         ENDIF                                                                   PLTTER.546
         GOTO 310                                                                PLTTER.547
  320    CONTINUE                                                                PLTTER.548
         CALL SET(XSA,XSB,YSA,YSB,XSC,XSD,YSC,YSD,LTYPE)                         PLTTER.549
         CALL GSCLIP(0)                                                          PLTTER.550
         CALL FRAME                                                              PLTTER.551
        PRINT *,'6, -----> RAOB PLOTTED'                                         PLTTER.552
C                                                                                PLTTER.553
C     ... PUT ON MAP NEST DOMAIN                                                 PLTTER.554
C                                                                                PLTTER.555
      IF(IPRINT.EQ.1) THEN                                                       06NOV00.217
      PRINT *, 'IMX,JMX,IYY,JXX,N = ',IMX,JMX,IYY,JXX,N                          PLTTER.556
      PRINT *, 'NUMNC = ',NUMNC(1), NUMNC(2)                                     PLTTER.557
      PRINT *, XSA,XSB,YSA,YSB,XSC,XSD,YSC,YSD                                   PLTTER.558
      PRINT *, XSA,XSB,YSA,YSB,1.,FLOAT(JXX),1.,FLOAT(IYY)                       PLTTER.559
      ENDIF                                                                      06NOV00.218
      IFPLOT=0                                                                   PLTTER.560
      DO NN = 2,MAXNES                                                           PLTTER.561
      IF (NUMNC(NN).EQ.N .OR. N.EQ.1) IFPLOT=1                                   PLTTER.562
      ENDDO                                                                      PLTTER.563
      IF (IFPLOT.EQ.1) THEN                                                      PLTTER.564
      IF(N.EQ.MAXNES) GO TO 10                                                   PLTTER.565
C                                                                                PLTTER.566
         CALL GSTXCI (1)                                                         PLTTER.567
         CALL GSPLCI (1)                                                         PLTTER.568
         CALL GETSET(XSA,XSB,YSA,YSB,XSC,XSD,YSC,YSD,LTYPE)                      PLTTER.569
         CALL SET(XSA,XSB,YSA,YSB,1.,FLOAT(JXX),1.,FLOAT(IYY),LTYPE)             PLTTER.570
      IF(IFILL) THEN                                                             PLTTER.571
      CALL GFLAS3(N2)                                                            PLTTER.572
      ELSE                                                                       PLTTER.573
      CALL GFLAS3(N1)                                                            PLTTER.574
      ENDIF                                                                      PLTTER.575
      DO 12 NN = 2,MAXNES                                                        PLTTER.576
      IF (NUMNC(NN).EQ.N .OR. N.EQ.1) THEN                                       PLTTER.577
        IF(N.EQ.1) THEN                                                          PLTTER.578
        IF(IPRINT.EQ.1)PRINT *,XWEST(NN),XEAST(NN),XSOUTH(NN),XNORTH(NN)         06NOV00.219
        WEST = XWEST(NN) - FLOAT(JOFFST)                                         PLTTER.580
        EAST = XEAST(NN) - FLOAT(JOFFST)                                         PLTTER.581
        SOUTH = XSOUTH(NN) - FLOAT(IOFFST)                                       PLTTER.582
        NORTH = XNORTH(NN) - FLOAT(IOFFST)                                       PLTTER.583
        ELSE                                                                     PLTTER.584
        WEST= FLOAT(NESTJ(NN))                                                   PLTTER.585
        SOUTH= FLOAT(NESTI(NN))                                                  PLTTER.586
        EAST= WEST+FLOAT((NESTJX(NN)-1)/IRATIO(NN))                              PLTTER.587
        NORTH=SOUTH+FLOAT((NESTIX(NN)-1)/IRATIO(NN))                             PLTTER.588
C       PRINT *,'SOUTH,WEST,NORTH,EAST= ',SOUTH,WEST,NORTH,EAST                  PLTTER.589
        ENDIF                                                                    PLTTER.590
        IF(IPRINT.EQ.1)                                                          06NOV00.220
     -     PRINT *,'SOUTH,WEST,NORTH,EAST= ',SOUTH,WEST,NORTH,EAST               06NOV00.221
         CALL SETUSV('LW',1000)                                                  PLTTER.592
         CALL LINE(WEST,SOUTH,EAST,SOUTH)                                        PLTTER.593
         CALL LINE(WEST,NORTH,EAST,NORTH)                                        PLTTER.594
         CALL LINE(WEST,NORTH,WEST,SOUTH)                                        PLTTER.595
         CALL LINE(EAST,NORTH,EAST,SOUTH)                                        PLTTER.596
      WRITE (DOMCHAR,11) NN                                                      PLTTER.597
      CALL PLCHLQ(WEST+4.,NORTH-4.,                                              PLTTER.598
     *      DOMCHAR, 16./1023.,0.,0.)                                            PLTTER.599
        ENDIF                                                                    PLTTER.600
12      CONTINUE                                                                 PLTTER.601
      WRITE (DOMCHAR,11) N                                                       PLTTER.602
      CALL PLCHLQ(4.,FLOAT(NESTIX(N))-4.,                                        PLTTER.603
     *      DOMCHAR, 16./1023.,0.,0.)                                            PLTTER.604
      CALL FRAME                                                                 PLTTER.605
        PRINT *,'7, -----> MAP NESTS PLOTTED'                                    PLTTER.606
      ENDIF                                                                      PLTTER.607
10      CONTINUE                                                                 PLTTER.608
C                                                                                PLTTER.609
C PLOT NEW FIELDS                                                                PLTTER.610
C                                                                                PLTTER.611
      IF (.NOT.IFTER) GO TO 601                                                  PLTTER.612
                                                                                 PLTTER.613
         IF (.NOT.VEGESOIL) GO TO 601                                            PLTTER.614
         IF (VEGONLY) GO TO 601                                                  PLTTER.615
C                                                                                PLTTER.616
C DEEP SOIL TEMPERATURE (ANNUAL MEAN)                                            PLTTER.617
C                                                                                PLTTER.618
      IMIN = -100                                                                PLTTER.619
      JMIN = -100                                                                PLTTER.620
      IMAX = -100                                                                PLTTER.621
      JMAX = -100                                                                PLTTER.622
      TMPMIN =  10000.                                                           PLTTER.623
      TMPMAX = -10000.                                                           PLTTER.624
      DO I=1,IMX                                                                 PLTTER.625
      DO J=1,JMX                                                                 PLTTER.626
      IF (TEMPG(I,J).LT.TMPMIN.AND.TEMPG(I,J).GT.0.) THEN                        PLTTER.627
        TMPMIN=TEMPG(I,J)                                                        PLTTER.628
        IMIN = I                                                                 PLTTER.629
        JMIN = J                                                                 PLTTER.630
      ENDIF                                                                      PLTTER.631
      IF (TEMPG(I,J).GT.TMPMAX.AND.TEMPG(I,J).GT.0.) THEN                        PLTTER.632
        TMPMAX=TEMPG(I,J)                                                        PLTTER.633
        IMAX = I                                                                 PLTTER.634
        JMAX = J                                                                 PLTTER.635
      ENDIF                                                                      PLTTER.636
      END DO                                                                     PLTTER.637
      END DO                                                                     PLTTER.638
      IF(IPRINT.EQ.1)PRINT *,'MIN GROUND TEMP (',IMIN,JMIN,') =',TMPMIN          06NOV00.222
      IF(IPRINT.EQ.1)PRINT *,'MAX GROUND TEMP (',IMAX,JMAX,') =',TMPMAX          06NOV00.223
C                                                                                PLTTER.641
      TMPDIFF = TMPMAX-TMPMIN                                                    PLTTER.642
      IF(IPRINT.EQ.1)PRINT *,'TMPDIFF =',TMPDIFF                                 06NOV00.224
      IF (TMPDIFF.LT.1.E-20) THEN                                                PLTTER.644
        PRINT *,' '                                                              PLTTER.645
        PRINT *,'** NO GROUND TEMPERATURE GENERATED OR',                         PLTTER.646
     -                         ' TEMPG IS CONSTANT. **'                          PLTTER.647
        GO TO 602                                                                PLTTER.648
      ENDIF                                                                      PLTTER.649
C                                                                                PLTTER.650
      DO I=1,IYY-1                                                               PLTTER.651
      DO J=1,JXX-1                                                               PLTTER.652
         GRDOUT(J,I) = TEMPG(IEX+I,JEX+J)                                        PLTTER.653
         IF(GRDOUT(J,I).LE.0.) GRDOUT(J,I)= TMPMIN                               PLTTER.654
      END DO                                                                     PLTTER.655
      END DO                                                                     PLTTER.656
                                                                                 PLTTER.657
C  TURN OFF CLIPPING                                                             PLTTER.658
      CALL GSCLIP (0)                                                            PLTTER.659
                                                                                 PLTTER.660
      IF(IFILL) THEN                                                             PLTTER.661
                                                                                 PLTTER.662
C  OPEN UNIT MAPUNIT AS TABLE TO READ CONDRV INFORMATION FROM                    PLTTER.663
         OPEN (MAPUNIT,FILE='confiT.tbl',STATUS='OLD')                           PLTTER.664
                                                                                 PLTTER.665
C  SET UP A COLOR TABLE                                                          PLTTER.666
         CALL RDCOLT (MAPUNIT)                                                   PLTTER.667
                                                                                 PLTTER.668
C  DEFINE TITLE                                                                  PLTTER.669

         CALL GSTXCI (16)                                                        06NOV00.225
         CALL GSPLCI (16) 
                                                                                 06NOV00.226
         CALL SET (0.0,1.0,0.0,1.0,0.0,100.,0.0,100.,1)                          06NOV00.227
         CALL PLCHHQ(50.,95.,'DEEP SOIL TEMP (ANNUAL MEAN)',.02,0.,0.)           06NOV00.228
                                                                                 PLTTER.674
         XINC=1./FLOAT(JXX) * 0.5                                                PLTTER.675
         CALL SET (XINC,1.-XINC,XINC,1.-XINC,0.0,1.0,0.0,1.0,1)                  PLTTER.676
                                                                                 PLTTER.677
         LEVELS(1) =  3.                                                         PLTTER.678
         IF (TMPDIFF.LT.LEVELS(1)) LEVELS(1) = TMPDIFF/2.                        PLTTER.679
         LEVELS(2)=310.                                                          PLTTER.680
         LEVELS(3)=240.                                                          PLTTER.681
                                                                                 PLTTER.682
         CALL CONDRV (GRDOUT,JXX,IYY,1,1,JXX-1,IYY-1,1,-2,                       PLTTER.683
     *                LEVELS,.TRUE.,                                             PLTTER.684
     *                'NO',1.0,TITLE,28,MAPUNIT,0,0)                             PLTTER.685
                                                                                 PLTTER.686
C  CLOSE THE TABLES                                                              PLTTER.687
         CLOSE (MAPUNIT)                                                         PLTTER.688
                                                                                 PLTTER.689
         CALL GFLAS3(N1)                                                         PLTTER.690
         CALL FRAME                                                              PLTTER.691
         PRINT *,'8, -----> DEEP SOIL TEMP PLOTTED'                              05DEC01.14
                                                                                 PLTTER.692
      ELSE                                                                       PLTTER.693
                                                                                 PLTTER.694
C  OPEN UNIT MAPUNIT AS TABLE TO READ CONDRV INFORMATION FROM                    PLTTER.695
      OPEN (MAPUNIT,FILE='con.tbl',STATUS='OLD')                                 PLTTER.696
                                                                                 PLTTER.697
C  SET UP A COLOR TABLE                                                          PLTTER.698
      CALL RDCOLT (MAPUNIT)                                                      PLTTER.699
                                                                                 PLTTER.700
C  DEFINE TITLE                                                                  PLTTER.701
                                                                                 PLTTER.702
      XINC=1./FLOAT(JXX) * 0.5                                                   PLTTER.703
      CALL SET (XINC,1.-XINC,XINC,1.-XINC,0.0,1.0,0.0,1.0,1)                     PLTTER.704
                                                                                 PLTTER.705
C  DO THE PLOT                                                                   PLTTER.706
                                                                                 PLTTER.707
         LEVELS(1)=  2.                                                          PLTTER.708
         LEVELS(2) = 320.                                                        PLTTER.709
         LEVELS(3) = 240.                                                        PLTTER.710
C                                                                                PLTTER.711
      CALL CONDRV (GRDOUT,JXX,IYY,1,1,JXX-1,IYY-1,1,-1,                          PLTTER.712
     *             LEVELS,.TRUE.,                                                PLTTER.713
     *             'NO',1.0,TITLE,0,-1,0,0)                                      PLTTER.714
C                                                                                PLTTER.715
C  CLOSE THE TABLES                                                              PLTTER.716
      CLOSE (MAPUNIT)                                                            PLTTER.717
                                                                                 PLTTER.718
      CALL GFLAS3(N1)                                                            PLTTER.719
      CALL FRAME                                                                 PLTTER.720
                                                                                 PLTTER.721
      PRINT *,'8, -----> GROUND TEMPERATURE PLOTTED'                             PLTTER.722
      ENDIF                                                                      PLTTER.723
C                                                                                PLTTER.724
602   CONTINUE                                                                   PLTTER.725
C                                                                                PLTTER.726
      IF (VEGESOIL .AND. IEXTRA) THEN                                            PLTTER.727
C                                                                                PLTTER.728
C VEGETATION CATEGORIES                                                          PLTTER.729
                                                                                 PLTTER.730
      DO II = 1,NCMAX                                                            PLTTER.731
        NUMPOINTS(II) = 0                                                        PLTTER.732
      END DO                                                                     PLTTER.733
C                                                                                PLTTER.734
      DO  I=1,IYY                                                                PLTTER.735
      DO  J=1,JXX                                                                PLTTER.736
         INDX = LVEGE(IEX+I,JEX+J)                                               PLTTER.737
         NUMPOINTS(INDX) =  NUMPOINTS(INDX)+1                                    PLTTER.738
      END DO                                                                     PLTTER.739
      END DO                                                                     PLTTER.740
C                                                                                PLTTER.741
      IF(IPRINT.EQ.1) THEN                                                       06NOV00.229
      PRINT *,'VEGETATION CATEGORY:'                                             06NOV00.230
      DO II = 1,NVC                                                              PLTTER.743
        PRINT *,'II=',II,'  NUMPOINTS=',NUMPOINTS(II)                            PLTTER.744
      END DO                                                                     PLTTER.745
      END IF                                                                     06NOV00.231
C                                                                                PLTTER.746
      IF (VSPLOT)                                                                PLTTER.747
     -     CALL PLOTCON(MAPUNIT,IMX,JMX,IYY,JXX,IEX,JEX,VEGEC,NVC,               PLTTER.748
     -                      GRDOUT, 20.,N,N1,'VEGECAT ',IFILL,IPRINT)            06NOV00.232
C                                                                                PLTTER.750
C     CALL GFLAS3(N1)                                                            PLTTER.751
C     CALL FRAME                                                                 PLTTER.752
C     CLOSE (MAPUNIT)                                                            PLTTER.753
C                                                                                PLTTER.754
        PRINT *,'9, -----> VEGETATION CATEGORY PLOTTED'                          PLTTER.755
C                                                                                PLTTER.756
C .. SOIL CATEGORIES                                                             PLTTER.757
C                                                                                PLTTER.758
      DO II = 1,NCMAX                                                            PLTTER.759
        NUMPOINTS(II) = 0                                                        PLTTER.760
      END DO                                                                     PLTTER.761
C                                                                                PLTTER.762
      DO  I=1,IYY                                                                PLTTER.763
      DO  J=1,JXX                                                                PLTTER.764
         INDX = LSOIL(IEX+I,JEX+J)                                               PLTTER.765
         NUMPOINTS(INDX) =  NUMPOINTS(INDX)+1                                    PLTTER.766
      END DO                                                                     PLTTER.767
      END DO                                                                     PLTTER.768
C                                                                                PLTTER.769
      IF (IPRINT.EQ.1) THEN                                                      06NOV00.233
      PRINT *,'SOIL CATEGORY:'                                                   PLTTER.770
      DO II = 1,NVC                                                              PLTTER.771
        PRINT *,'II=',II,'  NUMPOINTS=',NUMPOINTS(II)                            PLTTER.772
      END DO                                                                     PLTTER.773
      END IF                                                                     06NOV00.234
C                                                                                PLTTER.774
      IF (VSPLOT)                                                                PLTTER.775
     -     CALL PLOTCON(MAPUNIT,IMX,JMX,IYY,JXX,IEX,JEX,SOILC,NSC,               PLTTER.776
     -                      GRDOUT, 20.,N,N1,'SOILCAT ',IFILL,IPRINT)            06NOV00.235
                                                                                 PLTTER.778
      END IF                                                                     PLTTER.779
C END OF IEXTRA BLOCK                                                            PLTTER.780
C                                                                                PLTTER.781
C  OPEN UNIT MAPUNIT AS TABLE TO READ CONDRV INFORMATION FROM                    PLTTER.782
                                                                                 PLTTER.783
      OPEN (MAPUNIT,FILE=SOILCOFILE,STATUS='OLD')                                PLTTER.784
                                                                                 PLTTER.785
C  SET UP A COLOR TABLE                                                          PLTTER.786
                                                                                 PLTTER.787
      CALL RDCOLT (MAPUNIT)                                                      PLTTER.788
C  TURN OFF CLIPPING                                                             PLTTER.789
                                                                                 PLTTER.790
      CALL GSCLIP (0)                                                            PLTTER.791
                                                                                 PLTTER.792
C  GET THE REVIOUS SET CALL                                                      PLTTER.793
                                                                                 PLTTER.794
      CALL GETSET (XA2,XB2,YA2,YB2,XUA2,XUB2,YUA2,YUB2,NTYPEPLT)                 PLTTER.795
                                                                                 PLTTER.796
C  TITLE                                                                         06NOV00.236
      CALL GSTXCI (16)                                                           06NOV00.237
      CALL GSPLCI (16)                                                           06NOV00.238

      CALL SET (0.0,1.0,0.0,1.0,0.0,100.,0.0,100.,1)                             06NOV00.239
      CALL PLCHHQ(50.,95.,'DOMINANT SOIL TYPE',.02,0.,0.)                        06NOV00.240
                                                                                 PLTTER.800
C        XINC=1./FLOAT(JXX) * 0.5                                                PLTTER.801
         CALL SET (XA2,XB2,YA2,YB2,                                              PLTTER.802
     *             1.0,FLOAT(JXX-1),1.0,FLOAT(IYY-1),NTYPEPLT)                   PLTTER.803
C        PRINT *,'XA2,XB2,YA2,YB2= ',XA2,XB2,YA2,YB2                             PLTTER.804
                                                                                 PLTTER.805
      IF(IFILL) THEN                                                             PLTTER.806
C       ... PUT ON THE SOIL CATEGORY COLORS                                      PLTTER.807
                                                                                 PLTTER.808
      DO 2960 I=1,IYY-1                                                          PLTTER.809
      DO 2960 J=1,JXX-1                                                          PLTTER.810
      ICI=LSOIL(IEX+I,JEX+J)                                                     PLTTER.811
      IF (ICI.EQ.NSC) THEN                                                       PLTTER.812
        ICI =     18                             ! NO SOIL DATA                  PLTTER.813
      ELSE                                                                       PLTTER.814
        ICI = ICI+18                                                             PLTTER.815
      ENDIF                                                                      PLTTER.816
      XRA(1)=FLOAT(J)-.5                                                         PLTTER.817
      XRA(2)=FLOAT(J+1)-.5                                                       PLTTER.818
      XRA(3)=FLOAT(J+1)-.5                                                       PLTTER.819
      XRA(4)=FLOAT(J)-.5                                                         PLTTER.820
      YRA(1)=FLOAT(I)-.5                                                         PLTTER.821
      YRA(2)=FLOAT(I)-.5                                                         PLTTER.822
      YRA(3)=FLOAT(I+1)-.5                                                       PLTTER.823
      YRA(4)=FLOAT(I+1)-.5                                                       PLTTER.824
      CALL SFSGFA(XRA,YRA,4,DST,10,IND,10,ICI)                                   PLTTER.825
2960  CONTINUE                                                                   PLTTER.826
C  PUT ON LABEL BAR                                                              PLTTER.827
                                                                                 PLTTER.828
      CALL LABELS                                                                PLTTER.829
                                                                                 PLTTER.830
      ELSE                                                                       PLTTER.831
C       ... PUT ON THE SOIL CATEGORY NUMBERS                                     PLTTER.832
                                                                                 PLTTER.833
      DO 2965 I=1,IYY-1                                                          PLTTER.834
      DO 2965 J=1,JXX-1                                                          PLTTER.835
         LANDINT= LSOIL(IEX+I,JEX+J)                                             PLTTER.836
         WRITE (LANDCHAR,'(I2)') LANDINT                                         PLTTER.837
         IF (LANDINT.EQ.ISW(5)) THEN                                             PLTTER.838
            CALL GSTXCI (32)                                                     PLTTER.839
            CALL GSPLCI (32)                                                     PLTTER.840
         ELSE                                                                    PLTTER.841
            CALL GSTXCI (20)                                                     PLTTER.842
            CALL GSPLCI (20)                                                     PLTTER.843
         ENDIF                                                                   PLTTER.844
         IF (LANDINT.LE.9) THEN                                                  PLTTER.845
            CALL PLCHLQ(FLOAT(J),FLOAT(I),LANDCHAR(2:2),7.,0.,0.)                PLTTER.846
         ELSE                                                                    PLTTER.847
            CALL PLCHLQ(FLOAT(J),FLOAT(I),LANDCHAR,4.,0.,0.)                     PLTTER.848
         ENDIF                                                                   PLTTER.849
2965  CONTINUE                                                                   PLTTER.850
C                                                                                PLTTER.851
      CALL GSTXCI (28)                                                           PLTTER.852
      CALL GSPLCI (28)                                                           PLTTER.853
      XXEND=JXX-.5                                                               PLTTER.854
      DO 2967 I=1,IYY-2                                                          PLTTER.855
         CALL LINE(.5,FLOAT(I)+.5,XXEND,FLOAT(I)+.5)                             PLTTER.856
 2967 CONTINUE                                                                   PLTTER.857
      YYEND=IYY-.5                                                               PLTTER.858
      DO 2968 J=1,JXX-2                                                          PLTTER.859
         CALL LINE(FLOAT(J)+.5,.5,FLOAT(J)+.5,YYEND)                             PLTTER.860
 2968 CONTINUE                                                                   PLTTER.861
C                                                                                PLTTER.862
      ENDIF                                                                      PLTTER.863
                                                                                 PLTTER.864
      CALL GFLAS3(N1)                                                            PLTTER.865
      CALL FRAME                                                                 PLTTER.866
      CLOSE (MAPUNIT)                                                            PLTTER.867
C                                                                                PLTTER.868
      PRINT *,'10, -----> SOIL CATEGORY PLOTTED'                                 PLTTER.869
C                                                                                PLTTER.870
      IF (VSPLOT)                                                                PLTTER.871
     -     CALL PLOTCON(MAPUNIT,IMX,JMX,IYY,JXX,IEX,JEX,VEGEF, 12,               PLTTER.872
     -                      GRDOUT,10.0,N,N1,'VEGEFRC ',IFILL,IPRINT)            06NOV00.241
C                                                                                PLTTER.874
      PRINT *,'11, -----> VEGETATION FRACTION PLOTTED'                           PLTTER.875
                                                                                 PLTTER.876
C                                                                                PLTTER.877
C  OPEN UNIT MAPUNIT AS TABLE TO READ CONDRV INFORMATION FROM                    PLTTER.878
                                                                                 PLTTER.879
      OPEN (MAPUNIT,FILE=VEGECOFILE,STATUS='OLD')                                PLTTER.880
                                                                                 PLTTER.881
C  SET UP A COLOR TABLE                                                          PLTTER.882
                                                                                 PLTTER.883
      CALL RDCOLT (MAPUNIT)                                                      PLTTER.884
C  TURN OFF CLIPPING                                                             PLTTER.885
                                                                                 PLTTER.886
      CALL GSCLIP (0)                                                            PLTTER.887

C  GET THE REVIOUS SET CALL                                                      PLTTER.889
                                                                                 PLTTER.890
      CALL GETSET (XA2,XB2,YA2,YB2,XUA2,XUB2,YUA2,YUB2,NTYPEPLT)                 PLTTER.891
                                                                                 PLTTER.888
C  TITLE                                                                         06NOV00.242
      CALL GSTXCI (16)                                                           06NOV00.243
      CALL GSPLCI (16)                                                           06NOV00.244

      CALL SET (0.0,1.0,0.0,1.0,0.0,100.,0.0,100.,1)                             06NOV00.245
      CALL PLCHHQ(50.,95.,'LAND-WATER MASK',.02,0.,0.)                           06NOV00.246
                                                                                 PLTTER.892
C         XINC=1./FLOAT(JXX) * 0.5                                               PLTTER.893
         CALL SET (XA2,XB2,YA2,YB2,                                              PLTTER.894
     *             1.0,FLOAT(JXX-1),1.0,FLOAT(IYY-1),NTYPEPLT)                   PLTTER.895
C        PRINT *,'XA2,XB2,YA2,YB2= ',XA2,XB2,YA2,YB2                             PLTTER.896
                                                                                 PLTTER.897
      DO I=1,IYY-1                                                               PLTTER.898
      DO J=1,JXX-1                                                               PLTTER.899
         LANDINT= NINT(WLMSK(IEX+I,JEX+J))                                       PLTTER.900
         WRITE (LANDCHAR,'(I2)') LANDINT                                         PLTTER.901
         IF (LANDINT.EQ.0) THEN                                                  PLTTER.902
            CALL GSTXCI (6)                                                      PLTTER.903
            CALL GSPLCI (6)                                                      PLTTER.904
         ELSE                                                                    PLTTER.905
            CALL GSTXCI (9)                                                      PLTTER.906
            CALL GSPLCI (9)                                                      PLTTER.907
         ENDIF                                                                   PLTTER.908
            CALL PLCHLQ(FLOAT(J),FLOAT(I),LANDCHAR(2:2),7.,0.,0.)                05DEC01.15
      END DO                                                                     PLTTER.910
      END DO                                                                     PLTTER.911
      CALL GFLAS3(N1)                                                            PLTTER.912
      CALL FRAME                                                                 PLTTER.913
      CLOSE (MAPUNIT)                                                            PLTTER.914
C                                                                                PLTTER.915
      PRINT *,'12, -----> LAND-WATER MASK PLOTTED'                               PLTTER.916
                                                                                 PLTTER.917
 601  CONTINUE                                                                   PLTTER.918
#endif                                                                           PLTTER.919
C                                                                                PLTTER.920
      RETURN                                                                     PLTTER.921
      END                                                                        PLTTER.922
