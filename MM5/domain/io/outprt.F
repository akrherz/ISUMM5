      SUBROUTINE OUTPRT(IEXEC,N)                                                 OUTPRT.1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          OUTPRT.2
C                                                                     C          OUTPRT.3
C                                                                     C          OUTPRT.4
C     THIS SUBROUTINE CREATES THE PRINTER OUTPUT FOR A QUICK CHECK.   C          OUTPRT.5
C     THE COMPLETE ANALYSIS IS HANDLED BY OTHER PROGRAM "DATAFLOW"    C          OUTPRT.6
C     LATER.                                                          C          OUTPRT.7
C                                                                     C          OUTPRT.8
C     CHANGES                                                         C          OUTPRT.9
C       CHECK FOR NEST N INSTEAD OF NEST 1 OPTIONS FOR PRINTING       C          OUTPRT.10
C       ONLY PRINT MOL AND HOL FOR IBLTYP=1 (BULK)                    C          OUTPRT.11
C                                                                     C          OUTPRT.12
C                                                                     C          OUTPRT.13
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          OUTPRT.14
C                                                                                OUTPRT.15
C----------------------------------------------------------------------          OUTPRT.16
C                                                                                OUTPRT.17
#     include <parame.incl>                                                      OUTPRT.18
#     include <addr0.incl>                                                       OUTPRT.19
#     include <param2.incl>                                                      OUTPRT.20
#     include <param3.incl>                                                      OUTPRT.21
#     include <point3d.incl>                                                     OUTPRT.22
#     include <point2d.incl>                                                     OUTPRT.23
#     include <various.incl>                                                     OUTPRT.24
#     include <varia.incl>                                                       OUTPRT.25
#     include <pointbc.incl>                                                     OUTPRT.26
#     include <pmoist.incl>                                                      OUTPRT.27
#     include <nonhyd.incl>                                                      OUTPRT.28
#     include <nonhydb.incl>                                                     OUTPRT.29
                                                                                 OUTPRT.30
      DIMENSION SCRX1(MKX,MIX),SCRX2(MKX,MIX)                                    OUTPRT.31
      CHARACTER*40 NAME                                                          OUTPRT.32
      DIMENSION PSTD(MIX,MJX)                                                    OUTPRT.33
      DATA IXN,JXN,KXN/2,2,1/                                                    OUTPRT.34
C                                                                                OUTPRT.35
C----------------------------------------------------------------------          OUTPRT.36
C-----LARGE DOMAIN:                                                              OUTPRT.37
C                                                                                OUTPRT.38
      XTH=XTIME/60.                                                              OUTPRT.39
      PRINT10,N,XTIME,KTAU                                                       OUTPRT.40
      JCX=JXSEX(N)                                                               OUTPRT.41
      KOUT=KXOUT(N)                                                              OUTPRT.42
   10 FORMAT(///1X,'--------------------------------------------------',         OUTPRT.43
     1'-----------------'/1X,'*****',4X,'MESH NO. ',I2,' AT T = ',               OUTPRT.44
     1F11.5,                                                                     OUTPRT.45
     2' MINUTES, KTAU = ',I5,3X,'*****'/1X,'--------------------------',         OUTPRT.46
     3'-----------------------------------------'///)                            OUTPRT.47
C                                                                                OUTPRT.48
C-----NORTH-SOUTH CROSS SECTIONS:                                                OUTPRT.49
C                                                                                OUTPRT.50
C.....U AND V:                                                                   OUTPRT.51
C                                                                                OUTPRT.52
      IF(IEXEC.GT.1)GOTO 100                                                     OUTPRT.53
C                                                                                OUTPRT.54
C.....FIRST TIME :                                                               OUTPRT.55
C                                                                                OUTPRT.56
C                                                                                OUTPRT.57
      IF(JCX.EQ.1)GOTO 40                                                        OUTPRT.58
      IF(JCX.EQ.JL)GOTO 70                                                       OUTPRT.59
C                                                                                OUTPRT.60
C.....INTERIOR SLICE:                                                            OUTPRT.61
C                                                                                OUTPRT.62
      DO 20 K=1,KL                                                               OUTPRT.63
        DO 20 I=2,ILX                                                            OUTPRT.64
          PSABAR=0.25*(PSA(I,JCX)+PSA(I,JCX-1)+PSA(I-1,JCX)+                     OUTPRT.65
     +           PSA(I-1,JCX-1))                                                 OUTPRT.66
          SCRX1(K,I)=UA(I,JCX,K)/PSABAR                                          OUTPRT.67
          SCRX2(K,I)=VA(I,JCX,K)/PSABAR                                          OUTPRT.68
   20   CONTINUE                                                                 OUTPRT.69
                                                                                 OUTPRT.70
      DO 30 K=1,KL                                                               OUTPRT.71
        PSAB1=0.5*(PSA(1,JCX-1)+PSA(1,JCX))                                      OUTPRT.72
        PSAB2=0.5*(PSA(ILX,JCX-1)+PSA(ILX,JCX))                                  OUTPRT.73
        SCRX1(K,1)=UA(1,JCX,K)/PSAB1                                             OUTPRT.74
        SCRX1(K,IL)=UA(IL,JCX,K)/PSAB2                                           OUTPRT.75
        SCRX2(K,1)=VA(1,JCX,K)/PSAB1                                             OUTPRT.76
        SCRX2(K,IL)=VA(IL,JCX,K)/PSAB2                                           OUTPRT.77
   30 CONTINUE                                                                   OUTPRT.78
      GOTO 170                                                                   OUTPRT.79
C                                                                                OUTPRT.80
C.....FOR JCX = 1:                                                               OUTPRT.81
C                                                                                OUTPRT.82
   40 CONTINUE                                                                   OUTPRT.83
      DO 50 K=1,KL                                                               OUTPRT.84
        DO 50 I=2,ILX                                                            OUTPRT.85
          PSABAR=0.5*(PSA(I,JCX)+PSA(I-1,JCX))                                   OUTPRT.86
          SCRX1(K,I)=UA(I,JCX,K)/PSABAR                                          OUTPRT.87
          SCRX2(K,I)=VA(I,JCX,K)/PSABAR                                          OUTPRT.88
   50   CONTINUE                                                                 OUTPRT.89
                                                                                 OUTPRT.90
      DO 60 K=1,KL                                                               OUTPRT.91
        SCRX1(K,1)=UA(1,JCX,K)/PSA(1,JCX)                                        OUTPRT.92
        SCRX1(K,IL)=UA(IL,JCX,K)/PSA(ILX,JCX)                                    OUTPRT.93
        SCRX2(K,1)=VA(1,JCX,K)/PSA(1,JCX)                                        OUTPRT.94
        SCRX2(K,IL)=VA(IL,JCX,K)/PSA(ILX,JCX)                                    OUTPRT.95
   60 CONTINUE                                                                   OUTPRT.96
      GOTO 170                                                                   OUTPRT.97
C                                                                                OUTPRT.98
C.....FOR JCX = JL:                                                              OUTPRT.99
C                                                                                OUTPRT.100
   70 CONTINUE                                                                   OUTPRT.101
      DO 80 K=1,KL                                                               OUTPRT.102
        DO 80 I=2,ILX                                                            OUTPRT.103
          PSABAR=0.5*(PSA(I,JLX)+PSA(I-1,JLX))                                   OUTPRT.104
          SCRX1(K,I)=UA(I,JCX,K)/PSABAR                                          OUTPRT.105
          SCRX2(K,I)=VA(I,JCX,K)/PSABAR                                          OUTPRT.106
   80   CONTINUE                                                                 OUTPRT.107
                                                                                 OUTPRT.108
      DO 90 K=1,KL                                                               OUTPRT.109
        SCRX1(K,1)=UA(1,JCX,K)/PSA(1,JLX)                                        OUTPRT.110
        SCRX1(K,IL)=UA(IL,JCX,K)/PSA(ILX,JLX)                                    OUTPRT.111
        SCRX2(K,1)=VA(1,JCX,K)/PSA(1,JLX)                                        OUTPRT.112
        SCRX2(K,IL)=VA(IL,JCX,K)/PSA(ILX,JLX)                                    OUTPRT.113
   90 CONTINUE                                                                   OUTPRT.114
      GOTO 170                                                                   OUTPRT.115
C                                                                                OUTPRT.116
C.....SUBSEQUENT CALLS:                                                          OUTPRT.117
C                                                                                OUTPRT.118
  100 IF(JCX.EQ.1)GOTO 130                                                       OUTPRT.119
      IF(JCX.EQ.JL)GOTO 150                                                      OUTPRT.120
C                                                                                OUTPRT.121
C.....INTERIOR SLICE:                                                            OUTPRT.122
C                                                                                OUTPRT.123
      DO 110 K=1,KL                                                              OUTPRT.124
        DO 110 I=2,ILX                                                           OUTPRT.125
          PSABAR=0.25*(PSA(I,JCX)+PSA(I,JCX-1)+PSA(I-1,JCX)+                     OUTPRT.126
     +           PSA(I-1,JCX-1))                                                 OUTPRT.127
          SCRX1(K,I)=UA(I,JCX,K)/PSABAR                                          OUTPRT.128
          SCRX2(K,I)=VA(I,JCX,K)/PSABAR                                          OUTPRT.129
  110   CONTINUE                                                                 OUTPRT.130
                                                                                 OUTPRT.131
      DO 120 K=1,KL                                                              OUTPRT.132
        SCRX1(K,1)=UI1(JCX,K)                                                    OUTPRT.133
        SCRX1(K,IL)=UIL(JCX,K)                                                   OUTPRT.134
        SCRX2(K,1)=VI1(JCX,K)                                                    OUTPRT.135
        SCRX2(K,IL)=VIL(JCX,K)                                                   OUTPRT.136
  120 CONTINUE                                                                   OUTPRT.137
      GOTO 170                                                                   OUTPRT.138
C                                                                                OUTPRT.139
C.....FOR JCX = 1:                                                               OUTPRT.140
C                                                                                OUTPRT.141
  130 CONTINUE                                                                   OUTPRT.142
      DO 140 K=1,KL                                                              OUTPRT.143
        DO 140 I=1,IL                                                            OUTPRT.144
          SCRX1(K,I)=UJ1(I,K)                                                    OUTPRT.145
          SCRX2(K,I)=VJ1(I,K)                                                    OUTPRT.146
  140   CONTINUE                                                                 OUTPRT.147
                                                                                 OUTPRT.148
      GOTO 170                                                                   OUTPRT.149
C                                                                                OUTPRT.150
C.....FOR JCX = JL:                                                              OUTPRT.151
C                                                                                OUTPRT.152
  150 CONTINUE                                                                   OUTPRT.153
      DO 160 K=1,KL                                                              OUTPRT.154
        DO 160 I=1,IL                                                            OUTPRT.155
          SCRX1(K,I)=UJL(I,K)                                                    OUTPRT.156
          SCRX2(K,I)=VJL(I,K)                                                    OUTPRT.157
  160   CONTINUE                                                                 OUTPRT.158
                                                                                 OUTPRT.159
C                                                                                OUTPRT.160
  170 WRITE (NAME,FMT=180)JCX                                                    OUTPRT.161
C  170 ENCODE(40,180,NAME) JCX                                                   OUTPRT.162
  180 FORMAT('  U  CROSS-SECTION AT J = ',I3,8X)                                 OUTPRT.163
      CALL MAPSMP(SCRX1,MKX,MIX,1,KL,KXN,1,IL,IXN,0.,-1,NAME,XTH)                OUTPRT.164
      WRITE (NAME,FMT=190)JCX                                                    OUTPRT.165
C      ENCODE(40,190,NAME) JCX                                                   OUTPRT.166
  190 FORMAT('  V  CROSS-SECTION AT J = ',I3,8X)                                 OUTPRT.167
      CALL MAPSMP(SCRX2,MKX,MIX,1,KL,KXN,1,IL,IXN,0.,-1,NAME,XTH)                OUTPRT.168
C                                                                                OUTPRT.169
C.....T:                                                                         OUTPRT.170
C                                                                                OUTPRT.171
      DO 200 K=1,KL                                                              OUTPRT.172
        DO 200 I=1,ILX                                                           OUTPRT.173
          SCRX1(K,I)=TA(I,JCX,K)/PSA(I,JCX)                                      OUTPRT.174
  200   CONTINUE                                                                 OUTPRT.175
                                                                                 OUTPRT.176
      WRITE (NAME,FMT=210)JCX                                                    OUTPRT.177
C      ENCODE(40,210,NAME) JCX                                                   OUTPRT.178
  210 FORMAT('  T  CROSS-SECTION AT J = ',I3,8X)                                 OUTPRT.179
      CALL MAPSMP(SCRX1,MKX,MIX,1,KL,KXN,1,ILX,IXN,273.16,-1,NAME,XTH)           OUTPRT.180
C.....W                                                                          OUTPRT.181
C                                                                                OUTPRT.182
      DO 216 K=1,KL                                                              OUTPRT.183
        DO 216 I=1,ILX                                                           OUTPRT.184
          SCRX1(K,I)=0.5*(WA(I,JCX,K)+WA(I,JCX,K+1))/PSA(I,JCX)                  OUTPRT.185
  216   CONTINUE                                                                 OUTPRT.186
                                                                                 OUTPRT.187
      WRITE (NAME,FMT=218)JCX                                                    OUTPRT.188
C      ENCODE(40,218,NAME)JCX                                                    OUTPRT.189
  218 FORMAT('  WA  CROSS-SECTION AT J = ',I3,7X)                                OUTPRT.190
      CALL MAPSMP(SCRX1,MKX,MIX,1,KL,KXN,1,ILX,IXN,0.,-1,NAME,XTH)               OUTPRT.191
C                                                                                OUTPRT.192
C                                                                                OUTPRT.193
C.....QV:                                                                        OUTPRT.194
C                                                                                OUTPRT.195
      IF(IDRY(N).EQ.1)GOTO 270                                                   OUTPRT.196
      DO 220 K=1,KL                                                              OUTPRT.197
        DO 220 I=1,ILX                                                           OUTPRT.198
          SCRX1(K,I)=QVA(I,JCX,K)/PSA(I,JCX)                                     OUTPRT.199
  220   CONTINUE                                                                 OUTPRT.200
                                                                                 OUTPRT.201
      WRITE (NAME,FMT=230)JCX                                                    OUTPRT.202
C      ENCODE(40,230,NAME) JCX                                                   OUTPRT.203
  230 FORMAT('  QV  CROSS-SECTION AT J = ',I3,7X)                                OUTPRT.204
      CALL MAPSMP(SCRX1,MKX,MIX,1,KL,KXN,1,ILX,IXN,0.,-1,NAME,XTH)               OUTPRT.205
C                                                                                OUTPRT.206
C....QC AND QR:                                                                  OUTPRT.207
C                                                                                OUTPRT.208
      IF(IMOIST(N).EQ.1)GOTO 270                                                 OUTPRT.209
      DO 240 K=1,KL                                                              OUTPRT.210
        DO 240 I=1,ILX                                                           OUTPRT.211
          SCRX1(K,I)=QCA(I,JCX,K)/PSA(I,JCX)                                     OUTPRT.212
          SCRX2(K,I)=QRA(I,JCX,K)/PSA(I,JCX)                                     OUTPRT.213
  240   CONTINUE                                                                 OUTPRT.214
                                                                                 OUTPRT.215
      WRITE (NAME,FMT=250)JCX                                                    OUTPRT.216
C      ENCODE(40,250,NAME) JCX                                                   OUTPRT.217
  250 FORMAT('  QC  CROSS-SECTION AT J = ',I3,7X)                                OUTPRT.218
      CALL MAPSMP(SCRX1,MKX,MIX,1,KL,KXN,1,ILX,IXN,0.,-1,NAME,XTH)               OUTPRT.219
      WRITE (NAME,FMT=260)JCX                                                    OUTPRT.220
C      ENCODE(40,260,NAME) JCX                                                   OUTPRT.221
  260 FORMAT('  QR  CROSS-SECTION AT J = ',I3,7X)                                OUTPRT.222
      CALL MAPSMP(SCRX2,MKX,MIX,1,KL,KXN,1,ILX,IXN,0.,-1,NAME,XTH)               OUTPRT.223
  270 IF(KTAU.NE.0)GOTO 380                                                      OUTPRT.224
C                                                                                OUTPRT.225
      WRITE (NAME,FMT=280)                                                       OUTPRT.226
C      ENCODE(40,280,NAME)                                                       OUTPRT.227
  280 FORMAT(8X,'LATITUDE AT CROSS POINTS',8X)                                   OUTPRT.228
      CALL MAPSMP(XLAT,MIX,MJX,1,ILX,5,1,JLX,5,0.,1,NAME,XTH)                    OUTPRT.229
C                                                                                OUTPRT.230
      WRITE (NAME,FMT=290)                                                       OUTPRT.231
C      ENCODE(40,290,NAME)                                                       OUTPRT.232
  290 FORMAT(8X,'LONGITUDE AT CROSS POINTS',7X)                                  OUTPRT.233
      CALL MAPSMP(XLONG,MIX,MJX,1,ILX,5,1,JLX,5,0.,1,NAME,XTH)                   OUTPRT.234
C                                                                                OUTPRT.235
      DO 295 I=1,ILX                                                             OUTPRT.236
        DO 295 J=1,JLX                                                           OUTPRT.237
          HSCR1(I,J)=HT(I,J)/G                                                   OUTPRT.238
  295   CONTINUE                                                                 OUTPRT.239
                                                                                 OUTPRT.240
      WRITE (NAME,FMT=300)                                                       OUTPRT.241
C      ENCODE(40,300,NAME)                                                       OUTPRT.242
  300 FORMAT(5X,'TERRAIN HEIGHT AT CROSS POINTS',5X)                             OUTPRT.243
      CALL MAPSMP(HSCR1,MIX,MJX,1,ILX,1,1,JLX,1,0.,1,NAME,XTH)                   OUTPRT.244
C                                                                                OUTPRT.245
      WRITE (NAME,FMT=310)                                                       OUTPRT.246
C      ENCODE(40,310,NAME)                                                       OUTPRT.247
  310 FORMAT(4X,'CORIOLIS PARAMETER AT DOT POINTS',4X)                           OUTPRT.248
      CALL MAPSMP(F,MIX,MJX,1,IL,5,1,JL,5,0.,1,NAME,XTH)                         OUTPRT.249
C                                                                                OUTPRT.250
      WRITE (NAME,FMT=320)                                                       OUTPRT.251
C      ENCODE(40,320,NAME)                                                       OUTPRT.252
  320 FORMAT(5X,'MAP SCALE FACTOR AT DOT POINTS',5X)                             OUTPRT.253
      CALL MAPSMP(MSFD,MIX,MJX,1,IL,5,1,JL,5,0.,1,NAME,XTH)                      OUTPRT.254
C                                                                                OUTPRT.255
      IF(ISFOUT.EQ.0)GOTO 380                                                    OUTPRT.256
C                                                                                OUTPRT.257
      WRITE (NAME,FMT=330)                                                       OUTPRT.258
C      ENCODE(40,330,NAME)                                                       OUTPRT.259
  330 FORMAT(5X,'SURFACE ALBEDO AT CROSS POINTS',5X)                             OUTPRT.260
      CALL MAPSMP(ALB,MIX,MJX,1,ILX,5,1,JLX,5,0.,1,NAME,XTH)                     OUTPRT.261
C                                                                                OUTPRT.262
      WRITE (NAME,FMT=340)                                                       OUTPRT.263
C      ENCODE(40,340,NAME)                                                       OUTPRT.264
  340 FORMAT(4X,'SURFACE ROUGHNESS AT CROSS POINTS',3X)                          OUTPRT.265
      CALL MAPSMP(ZNT,MIX,MJX,1,ILX,5,1,JLX,5,0.,1,NAME,XTH)                     OUTPRT.266
C                                                                                OUTPRT.267
      WRITE (NAME,FMT=350)                                                       OUTPRT.268
C      ENCODE(40,350,NAME)                                                       OUTPRT.269
  350 FORMAT(2X,'MOISTURE AVAILABILITY AT CROSS POINTS',1X)                      OUTPRT.270
      CALL MAPSMP(MAVAIL,MIX,MJX,1,ILX,5,1,JLX,5,0.,1,NAME,XTH)                  OUTPRT.271
C                                                                                OUTPRT.272
      WRITE (NAME,FMT=360)                                                       OUTPRT.273
C      ENCODE(40,360,NAME)                                                       OUTPRT.274
  360 FORMAT(5X,'THERMAL INERTIA AT CROSS POINTS',4X)                            OUTPRT.275
      CALL MAPSMP(THC,MIX,MJX,1,ILX,5,1,JLX,5,0.,1,NAME,XTH)                     OUTPRT.276
C                                                                                OUTPRT.277
      WRITE (NAME,FMT=370)                                                       OUTPRT.278
C      ENCODE(40,370,NAME)                                                       OUTPRT.279
  370 FORMAT(3X,'SURFACE EMISSIVITY AT CROSS POINTS',3X)                         OUTPRT.280
      CALL MAPSMP(EMISS,MIX,MJX,1,ILX,5,1,JLX,5,0.,1,NAME,XTH)                   OUTPRT.281
C                                                                                OUTPRT.282
  380 CONTINUE                                                                   OUTPRT.283
C                                                                                OUTPRT.284
C....SURFACE PRESSURE AND PRESSURE CHANGE:                                       OUTPRT.285
C                                                                                OUTPRT.286
      CONST=-PTOP                                                                OUTPRT.287
      WRITE (NAME,FMT=390)                                                       OUTPRT.288
C      ENCODE(40,390,NAME)                                                       OUTPRT.289
  390 FORMAT(10X,'SURFACE PRESSURE (CB)',9X)                                     OUTPRT.290
C PRINT TOTAL PRESSURE IN CB (BASE STATE + PERTURBATION)                         OUTPRT.291
      DO 391 I=1,ILX                                                             OUTPRT.292
        DO 391 J=1,JLX                                                           OUTPRT.293
          HSCR1(I,J)=PSA(I,J)+PPA(I,J,KL)/PSA(I,J)*0.001                         OUTPRT.294
  391   CONTINUE                                                                 OUTPRT.295
                                                                                 OUTPRT.296
      PRINT*,' OUTPRT: CONST=-PTOP= ',CONST                                      OUTPRT.297
      CALL MAPSMP(HSCR1,MIX,MJX,1,ILX,IXN,1,JLX,JXN,CONST,1,NAME,XTH)            OUTPRT.298
C                                                                                OUTPRT.299
      IF(KTAU.EQ.0)GOTO 420                                                      OUTPRT.300
      DO 400 I=1,ILX                                                             OUTPRT.301
        DO 400 J=1,JLX                                                           OUTPRT.302
          HSCR1(I,J)=PSA(I,J)+PPA(I,J,KL)/PSA(I,J)*0.001-PSAINI(I,J)             OUTPRT.303
  400   CONTINUE                                                                 OUTPRT.304
                                                                                 OUTPRT.305
      WRITE (NAME,FMT=410)                                                       OUTPRT.306
C      ENCODE(40,410,NAME)                                                       OUTPRT.307
  410 FORMAT(10X,'PRESSURE CHANGE (CB)',10X)                                     OUTPRT.308
      CALL MAPSMP(HSCR1,MIX,MJX,1,ILX,1,1,JLX,1,0.,1,NAME,XTH)                   OUTPRT.309
  420 CONTINUE                                                                   OUTPRT.310
C                                                                                OUTPRT.311
C.....U AND V:                                                                   OUTPRT.312
C                                                                                OUTPRT.313
  421 CONTINUE                                                                   OUTPRT.314
      NPTS=(IL-2)*(JL-2)                                                         OUTPRT.315
      CALL DOTS(PSA,PSTD,IL,JL,MIX,MJX)                                          OUTPRT.316
      DO 12430 K=1,KL                                                            OUTPRT.317
        UAVE=0.0                                                                 OUTPRT.318
        VAVE=0.0                                                                 OUTPRT.319
        SPDAVE=0.0                                                               OUTPRT.320
        UMAX=-1.0                                                                OUTPRT.321
        VMAX=-1.0                                                                OUTPRT.322
        SPDMAX=-1.0                                                              OUTPRT.323
        DO 12431 I=2,ILX                                                         OUTPRT.324
          DO 12431 J=2,JLX                                                       OUTPRT.325
            UVAR=UA(I,J,K)/PSTD(I,J)                                             OUTPRT.326
            VVAR=VA(I,J,K)/PSTD(I,J)                                             OUTPRT.327
            SPD=SQRT(UVAR*UVAR+VVAR*VVAR)                                        OUTPRT.328
            IF(ABS(UVAR).GT.UMAX)UMAX=ABS(UVAR)                                  OUTPRT.329
            IF(ABS(VVAR).GT.VMAX)VMAX=ABS(VVAR)                                  OUTPRT.330
            IF(SPD.GT.SPDMAX)SPDMAX=SPD                                          OUTPRT.331
            UAVE=UAVE+UVAR                                                       OUTPRT.332
            VAVE=VAVE+VVAR                                                       OUTPRT.333
            SPDAVE=SPDAVE+SPD                                                    OUTPRT.334
12431     CONTINUE                                                               OUTPRT.335
                                                                                 OUTPRT.336
        SPDAVE=SPDAVE/NPTS                                                       OUTPRT.337
        UAVE=UAVE/NPTS                                                           OUTPRT.338
        VAVE=VAVE/NPTS                                                           OUTPRT.339
        PRINT12432,K,XTH,UAVE,VAVE,SPDAVE,UMAX,VMAX,SPDMAX                       OUTPRT.340
12432 FORMAT(1X,' K= ',I2,' T= ',F6.2,': AVE U,V,SPD ',                          OUTPRT.341
     13(1X,E12.5),'  MAX U,V,SPD ',3(1X,E12.5))                                  OUTPRT.342
12430 CONTINUE                                                                   OUTPRT.343
C                                                                                OUTPRT.344
      DO 430 I=2,ILX                                                             OUTPRT.345
        DO 430 J=2,JLX                                                           OUTPRT.346
          PSABAR=0.25*(PSA(I,J)+PSA(I,J-1)+PSA(I-1,J)+PSA(I-1,J-1))              OUTPRT.347
          HSCR1(I,J)=UA(I,J,KOUT)/PSABAR                                         OUTPRT.348
          HSCR2(I,J)=VA(I,J,KOUT)/PSABAR                                         OUTPRT.349
  430   CONTINUE                                                                 OUTPRT.350
                                                                                 OUTPRT.351
C                                                                                OUTPRT.352
      IF(IEXEC.GT.1)GOTO 460                                                     OUTPRT.353
C                                                                                OUTPRT.354
C.....FIRST TIME:                                                                OUTPRT.355
C                                                                                OUTPRT.356
      DO 440 I=2,ILX                                                             OUTPRT.357
        PSAB1=0.5*(PSA(I,1)+PSA(I-1,1))                                          OUTPRT.358
        PSAB2=0.5*(PSA(I,JLX)+PSA(I-1,JLX))                                      OUTPRT.359
        HSCR1(I,1)=UA(I,1,KOUT)/PSAB1                                            OUTPRT.360
        HSCR1(I,JL)=UA(I,JL,KOUT)/PSAB2                                          OUTPRT.361
        HSCR2(I,1)=VA(I,1,KOUT)/PSAB1                                            OUTPRT.362
        HSCR2(I,JL)=VA(I,JL,KOUT)/PSAB2                                          OUTPRT.363
  440 CONTINUE                                                                   OUTPRT.364
C                                                                                OUTPRT.365
      DO 450 J=2,JLX                                                             OUTPRT.366
        PSAB1=0.5*(PSA(1,J)+PSA(1,J-1))                                          OUTPRT.367
        PSAB2=0.5*(PSA(ILX,J)+PSA(ILX,J-1))                                      OUTPRT.368
        HSCR1(1,J)=UA(1,J,KOUT)/PSAB1                                            OUTPRT.369
        HSCR1(IL,J)=UA(IL,J,KOUT)/PSAB2                                          OUTPRT.370
        HSCR2(1,J)=VA(1,J,KOUT)/PSAB1                                            OUTPRT.371
        HSCR2(IL,J)=VA(IL,J,KOUT)/PSAB2                                          OUTPRT.372
  450 CONTINUE                                                                   OUTPRT.373
C                                                                                OUTPRT.374
      HSCR1(1,1)=UA(1,1,KOUT)/PSA(1,1)                                           OUTPRT.375
      HSCR1(1,JL)=UA(1,JL,KOUT)/PSA(1,JLX)                                       OUTPRT.376
      HSCR1(IL,1)=UA(IL,1,KOUT)/PSA(ILX,1)                                       OUTPRT.377
      HSCR1(IL,JL)=UA(IL,JL,KOUT)/PSA(ILX,JLX)                                   OUTPRT.378
      HSCR2(1,1)=VA(1,1,KOUT)/PSA(1,1)                                           OUTPRT.379
      HSCR2(1,JL)=VA(1,JL,KOUT)/PSA(1,JLX)                                       OUTPRT.380
      HSCR2(IL,1)=VA(IL,1,KOUT)/PSA(ILX,1)                                       OUTPRT.381
      HSCR2(IL,JL)=VA(IL,JL,KOUT)/PSA(ILX,JLX)                                   OUTPRT.382
      GOTO 490                                                                   OUTPRT.383
C                                                                                OUTPRT.384
C.....SUBSEQUENCE CALLS:                                                         OUTPRT.385
C                                                                                OUTPRT.386
  460 CONTINUE                                                                   OUTPRT.387
      DO 470 I=1,IL                                                              OUTPRT.388
        HSCR1(I,1)=UJ1(I,KOUT)                                                   OUTPRT.389
        HSCR1(I,JL)=UJL(I,KOUT)                                                  OUTPRT.390
        HSCR2(I,1)=VJ1(I,KOUT)                                                   OUTPRT.391
        HSCR2(I,JL)=VJL(I,KOUT)                                                  OUTPRT.392
  470 CONTINUE                                                                   OUTPRT.393
C                                                                                OUTPRT.394
      DO 480 J=2,JLX                                                             OUTPRT.395
        HSCR1(1,J)=UI1(J,KOUT)                                                   OUTPRT.396
        HSCR1(IL,J)=UIL(J,KOUT)                                                  OUTPRT.397
        HSCR2(1,J)=VI1(J,KOUT)                                                   OUTPRT.398
        HSCR2(IL,J)=VIL(J,KOUT)                                                  OUTPRT.399
  480 CONTINUE                                                                   OUTPRT.400
C                                                                                OUTPRT.401
  490 WRITE (NAME,FMT=500)KOUT                                                   OUTPRT.402
C  490 ENCODE(40,500,NAME) KOUT                                                  OUTPRT.403
  500 FORMAT('  U   AT K = ',I3,24X)                                             OUTPRT.404
      CALL MAPSMP(HSCR1,MIX,MJX,1,IL,IXN,1,JL,JXN,0.,1,NAME,XTH)                 OUTPRT.405
      WRITE (NAME,FMT=510)KOUT                                                   OUTPRT.406
C      ENCODE(40,510,NAME) KOUT                                                  OUTPRT.407
  510 FORMAT('  V   AT K = ',I3,24X)                                             OUTPRT.408
      CALL MAPSMP(HSCR2,MIX,MJX,1,IL,IXN,1,JL,JXN,0.,1,NAME,XTH)                 OUTPRT.409
C                                                                                OUTPRT.410
C.....T:                                                                         OUTPRT.411
C                                                                                OUTPRT.412
      DO 520 I=1,ILX                                                             OUTPRT.413
        DO 520 J=1,JLX                                                           OUTPRT.414
          HSCR1(I,J)=TA(I,J,KOUT)/PSA(I,J)                                       OUTPRT.415
  520   CONTINUE                                                                 OUTPRT.416
                                                                                 OUTPRT.417
      WRITE (NAME,FMT=530)KOUT                                                   OUTPRT.418
C      ENCODE(40,530,NAME) KOUT                                                  OUTPRT.419
  530 FORMAT('  T (C)  AT K = ',I3,21X)                                          OUTPRT.420
      CALL MAPSMP(HSCR1,MIX,MJX,1,ILX,1,1,JLX,1,273.16,1,NAME,XTH)               OUTPRT.421
C                                                                                OUTPRT.422
C.....RELATIVE HUMIDITY:                                                         OUTPRT.423
C                                                                                OUTPRT.424
      IF(IDRY(N).EQ.1)GOTO 590                                                   OUTPRT.425
      DO 540 I=1,ILX                                                             OUTPRT.426
        DO 540 J=1,JLX                                                           OUTPRT.427
          PRES=A(KOUT)*PSA(I,J)+PTOP+PPA(I,J,KOUT)/PSA(I,J)*0.001                OUTPRT.428
          SATVP=SVP1*EXP(SVP2*(HSCR1(I,J)-SVPT0)/(HSCR1(I,J)-SVP3))              OUTPRT.429
          QVS=EP2*SATVP/(PRES-SATVP)                                             OUTPRT.430
          HSCR2(I,J)=(QVA(I,J,KOUT)/PSA(I,J))/QVS                                OUTPRT.431
  540   CONTINUE                                                                 OUTPRT.432
                                                                                 OUTPRT.433
      WRITE (NAME,FMT=550)KOUT                                                   OUTPRT.434
C      ENCODE(40,550,NAME) KOUT                                                  OUTPRT.435
  550 FORMAT('  RELATIVE HUMIDITY AT K = ',I3,10X)                               OUTPRT.436
      CALL MAPSMP(HSCR2,MIX,MJX,1,ILX,IXN,1,JLX,JXN,0.,1,NAME,XTH)               OUTPRT.437
C                                                                                OUTPRT.438
C.....QC AND QR:                                                                 OUTPRT.439
C                                                                                OUTPRT.440
      IF(IMOIST(N).EQ.1)GOTO 590                                                 OUTPRT.441
      DO 560 I=1,ILX                                                             OUTPRT.442
        DO 560 J=1,JLX                                                           OUTPRT.443
          HSCR1(I,J)=QCA(I,J,KOUT)/PSA(I,J)                                      OUTPRT.444
          HSCR2(I,J)=QRA(I,J,KOUT)/PSA(I,J)                                      OUTPRT.445
  560   CONTINUE                                                                 OUTPRT.446
                                                                                 OUTPRT.447
      WRITE (NAME,FMT=570)KOUT                                                   OUTPRT.448
C      ENCODE(40,570,NAME) KOUT                                                  OUTPRT.449
  570 FORMAT('  QC  AT K = ',I3,24X)                                             OUTPRT.450
      CALL MAPSMP(HSCR1,MIX,MJX,1,ILX,IXN,1,JLX,JXN,0.,1,NAME,XTH)               OUTPRT.451
      WRITE (NAME,FMT=580)KOUT                                                   OUTPRT.452
C      ENCODE(40,580,NAME) KOUT                                                  OUTPRT.453
  580 FORMAT('  QR  AT K = ',I3,24X)                                             OUTPRT.454
      CALL MAPSMP(HSCR2,MIX,MJX,1,ILX,IXN,1,JLX,JXN,0.,1,NAME,XTH)               OUTPRT.455
C                                                                                OUTPRT.456
C.....PBL VARIABLES:                                                             OUTPRT.457
C                                                                                OUTPRT.458
  590 IF(IBLTYP(N).EQ.0)GOTO 680                                                 OUTPRT.459
      IF(IFREST.AND.KTAU.EQ.KTAUR)GOTO 680                                       OUTPRT.460
C                                                                                OUTPRT.461
      IF(IBLTYP(N).EQ.2)THEN                                                     OUTPRT.462
        WRITE (NAME,FMT=660)                                                     OUTPRT.463
C      ENCODE(40,660,NAME)                                                       OUTPRT.464
  660 FORMAT(15X,'PBL REGIME',15X)                                               OUTPRT.465
        CALL MAPSMP(REGIME,MIX,MJX,1,ILX,IXN,1,JLX,JXN,0.,1,NAME,XTH)            OUTPRT.466
      ELSEIF(IBLTYP(N).EQ.1)THEN                                                 OUTPRT.467
        WRITE (NAME,FMT=665)                                                     OUTPRT.468
C      ENCODE(40,665,NAME)                                                       OUTPRT.469
  665 FORMAT(6X,'EXCHANGE COEF. FOR MOMENTUM',7X)                                OUTPRT.470
        CALL MAPSMP(MOL,MIX,MJX,1,ILX,IXN,1,JLX,JXN,0.,1,NAME,XTH)               OUTPRT.471
      ENDIF                                                                      OUTPRT.472
C                                                                                OUTPRT.473
      WRITE (NAME,FMT=600)                                                       OUTPRT.474
C      ENCODE(40,600,NAME)                                                       OUTPRT.475
  600 FORMAT(14X,'PBL DEPTH (M)',13X)                                            OUTPRT.476
      CALL MAPSMP(PBL,MIX,MJX,1,ILX,IXN,1,JLX,JXN,0.,1,NAME,XTH)                 OUTPRT.477
C                                                                                OUTPRT.478
      IF(IBLTYP(N).EQ.2)THEN                                                     OUTPRT.479
        WRITE (NAME,FMT=610)                                                     OUTPRT.480
C      ENCODE(40,610,NAME)                                                       OUTPRT.481
  610 FORMAT(15X,'USTAR (M/S)',14X)                                              OUTPRT.482
        CALL MAPSMP(UST,MIX,MJX,1,ILX,IXN,1,JLX,JXN,0.,1,NAME,XTH)               OUTPRT.483
      ELSE                                                                       OUTPRT.484
        WRITE (NAME,FMT=615)                                                     OUTPRT.485
C      ENCODE(40,615,NAME)                                                       OUTPRT.486
  615 FORMAT(9X,'BULK-RICHARDSON NUMBER',9X)                                     OUTPRT.487
        CALL MAPSMP(REGIME,MIX,MJX,1,ILX,IXN,1,JLX,JXN,0.,1,NAME,XTH)            OUTPRT.488
      ENDIF                                                                      OUTPRT.489
C                                                                                OUTPRT.490
      IF(ISFFLX(N).EQ.0)GOTO 635                                                 OUTPRT.491
      WRITE (NAME,FMT=620)                                                       OUTPRT.492
C      ENCODE(40,620,NAME)                                                       OUTPRT.493
  620 FORMAT(6X,'SENSIBLE HEAT FLUX (W/M**2)',7X)                                OUTPRT.494
      CALL MAPSMP(HFX,MIX,MJX,1,ILX,IXN,1,JLX,JXN,0.,1,NAME,XTH)                 OUTPRT.495
C                                                                                OUTPRT.496
      DO 625 I=1,ILX                                                             OUTPRT.497
        DO 625 J=1,JLX                                                           OUTPRT.498
          TTMP=TA(I,J,KOUT)/PSA(I,J)                                             OUTPRT.499
          HSCR1(I,J)=QFX(I,J)*XLV                                                OUTPRT.500
  625   CONTINUE                                                                 OUTPRT.501
                                                                                 OUTPRT.502
      WRITE (NAME,FMT=630)                                                       OUTPRT.503
C      ENCODE(40,630,NAME)                                                       OUTPRT.504
  630 FORMAT(7X,'LATENT HEAT FLUX (W/M**2)',8X)                                  OUTPRT.505
      CALL MAPSMP(HSCR1,MIX,MJX,1,ILX,IXN,1,JLX,JXN,0.,1,NAME,XTH)               OUTPRT.506
  635 CONTINUE                                                                   OUTPRT.507
C                                                                                OUTPRT.508
      IF(ITGFLG(N).NE.1)GOTO 655                                                 OUTPRT.509
      WRITE (NAME,FMT=640)                                                       OUTPRT.510
C      ENCODE(40,640,NAME)                                                       OUTPRT.511
  640 FORMAT(6X,'SHORT WAVE RADIATION (W/M**2)',5X)                              OUTPRT.512
      CALL MAPSMP(GSW,MIX,MJX,1,ILX,IXN,1,JLX,JXN,0.,1,NAME,XTH)                 OUTPRT.513
C                                                                                OUTPRT.514
      WRITE (NAME,FMT=650)                                                       OUTPRT.515
C      ENCODE(40,650,NAME)                                                       OUTPRT.516
  650 FORMAT(6X,'LONG WAVE RADIATION (W/M**2)',6X)                               OUTPRT.517
      CALL MAPSMP(GLW,MIX,MJX,1,ILX,IXN,1,JLX,JXN,0.,1,NAME,XTH)                 OUTPRT.518
  655 CONTINUE                                                                   OUTPRT.519
C                                                                                OUTPRT.520
      IF(IBLTYP(N).EQ.2)THEN                                                     OUTPRT.521
        WRITE (NAME,FMT=670)                                                     OUTPRT.522
C      ENCODE(40,670,NAME)                                                       OUTPRT.523
  670 FORMAT(9X,'PRECIPITABLE WATER (CM)',8X)                                    OUTPRT.524
        CALL MAPSMP(PRW,MIX,MJX,1,ILX,IXN,1,JLX,JXN,0.,1,NAME,XTH)               OUTPRT.525
      ELSEIF(IBLTYP(N).EQ.1)THEN                                                 OUTPRT.526
        WRITE (NAME,FMT=675)                                                     OUTPRT.527
C      ENCODE(40,675,NAME)                                                       OUTPRT.528
  675 FORMAT(8X,'EXCHANGE COEF. FOR HEAT',9X)                                    OUTPRT.529
        CALL MAPSMP(HOL,MIX,MJX,1,ILX,IXN,1,JLX,JXN,0.,1,NAME,XTH)               OUTPRT.530
      ENDIF                                                                      OUTPRT.531
C                                                                                OUTPRT.532
C.....SURFACE TEMPERATURE:                                                       OUTPRT.533
C                                                                                OUTPRT.534
  680 IF(ITGFLG(N).EQ.3)GOTO 740                                                 OUTPRT.535
      IF(IBLTYP(N).EQ.0)GOTO 740                                                 OUTPRT.536
      CONST=273.16                                                               OUTPRT.537
      WRITE (NAME,FMT=730)                                                       OUTPRT.538
C      ENCODE(40,730,NAME)                                                       OUTPRT.539
  730 FORMAT(9X,'GROUND TEMPERATURE (C)',9X)                                     OUTPRT.540
      CALL MAPSMP(TGA,MIX,MJX,1,ILX,IXN,1,JLX,JXN,CONST,1,NAME,XTH)              OUTPRT.541
C                                                                                OUTPRT.542
C.....PRECIPITATION:                                                             OUTPRT.543
C                                                                                OUTPRT.544
  740 IF(IDRY(N).EQ.1)GOTO 790                                                   OUTPRT.545
      WRITE (NAME,FMT=750)                                                       OUTPRT.546
C      ENCODE(40,750,NAME)                                                       OUTPRT.547
  750 FORMAT(6X,'SUB-GRID-SCALE RAINFALL (CM)',6X)                               OUTPRT.548
      CALL MAPSMP(RAINC,MIX,MJX,1,ILX,IXN,1,JLX,JXN,0.,1,NAME,XTH)               OUTPRT.549
C                                                                                OUTPRT.550
      WRITE (NAME,FMT=760)                                                       OUTPRT.551
C      ENCODE(40,760,NAME)                                                       OUTPRT.552
  760 FORMAT(9X,'RESOLVED RAINFALL (CM)',9X)                                     OUTPRT.553
      CALL MAPSMP(RAINNC,MIX,MJX,1,ILX,IXN,1,JLX,JXN,0.,1,NAME,XTH)              OUTPRT.554
C                                                                                OUTPRT.555
      DO 770 I=1,ILX                                                             OUTPRT.556
        DO 770 J=1,JLX                                                           OUTPRT.557
          HSCR1(I,J)=RAINC(I,J)+RAINNC(I,J)                                      OUTPRT.558
  770   CONTINUE                                                                 OUTPRT.559
                                                                                 OUTPRT.560
      WRITE (NAME,FMT=780)                                                       OUTPRT.561
C      ENCODE(40,780,NAME)                                                       OUTPRT.562
  780 FORMAT(11X,'TOTAL RAINFALL (CM)',10X)                                      OUTPRT.563
      CALL MAPSMP(HSCR1,MIX,MJX,1,ILX,IXN,1,JLX,JXN,0.,1,NAME,XTH)               OUTPRT.564
C----------------------------------------------------------------------          OUTPRT.565
C                                                                                OUTPRT.566
  790 RETURN                                                                     OUTPRT.567
      END                                                                        OUTPRT.568
                                                                                 OUTPRT.569
