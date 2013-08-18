C                                                                                IN4DOB.1
C                                                                                IN4DOB.2
      SUBROUTINE IN4DOB(INEST,XTIME,KTAU,KTAUR,DTMIN,JULDAY,GMT)                 IN4DOB.3
C THIS IS SUBROUTINE READS AN OBSERVATION DATA FILE AND                          IN4DOB.4
C SELECTS ONLY THOSE VALUES OBSERVED AT TIMES THAT FALL                          IN4DOB.5
C WITHIN A TIME WINDOW (TWINDO) CENTERED ABOUT THE CURRENT                       IN4DOB.6
C FORECAST TIME (XTIME).  THE INCOMING OBS FILES MUST BE                         IN4DOB.7
C IN CHRONOLOGICAL ORDER.                                                        IN4DOB.8
C                                                                                IN4DOB.9
C NOTE - IN4DOB IS CALLED ONLY FOR THE COARSE MESH TIMES                         IN4DOB.10
C      - VAROBS(IVAR,N) HOLDS THE OBSERVATIONS.                                  IN4DOB.11
C        IVAR=1   UOBS                                                           IN4DOB.12
C        IVAR=2   VOBS                                                           IN4DOB.13
C        IVAR=3   TOBS                                                           IN4DOB.14
C        IVAR=4   QOBS                                                           IN4DOB.15
C        IVAR=5   PSOBS (CROSS)                                                  IN4DOB.16
#     include <parame.incl>                                                      IN4DOB.17
#     include <param2.incl>                                                      IN4DOB.18
#     include <parfdda.incl>                                                     IN4DOB.19
#     include <param3.incl>                                                      IN4DOB.20
#     include <fddaob.incl>                                                      IN4DOB.21
      LOGICAL OPENED                                                             IN4DOB.22
      DATA NMOVE/0/,IEOF/0/,NVOLA/61/                                            IN4DOB.23
      IF(I4DI(INEST).NE.1)RETURN                                                 IN4DOB.24
      IF(KTAU.EQ.0.OR.KTAU.EQ.KTAUR)THEN                                         IN4DOB.25
        DO 5 N=1,NIOBF                                                           IN4DOB.26
          TIMEOB(N)=99999.                                                       IN4DOB.27
    5   CONTINUE                                                                 IN4DOB.28
      ENDIF                                                                      IN4DOB.29
      IF(KTAU.EQ.KTAUR)NSTAT(INEST)=0                                            IN4DOB.30
      NSTA=NSTAT(INEST)                                                          IN4DOB.31
      WINDOW=TWINDO                                                              IN4DOB.32
      XHOUR=(XTIME-DTMIN)/60.                                                    IN4DOB.33
      XHOUR=AMAX1(XHOUR,0.0)                                                     IN4DOB.34
C                                                                                IN4DOB.35
C DEFINE THE MAX LIMITS OF THE WINDOW                                            IN4DOB.36
C                                                                                IN4DOB.37
   10 CONTINUE                                                                   IN4DOB.38
      TBACK=XHOUR-WINDOW                                                         IN4DOB.39
      TFORWD=XHOUR+WINDOW                                                        IN4DOB.40
C CHECK FOR OLD OBS THAT CAN BE DISCARDED                                        IN4DOB.41
      IF(KTAU.EQ.0.OR.KTAU.EQ.KTAUR)THEN                                         IN4DOB.42
        RTLAST(INEST)=-999.                                                      IN4DOB.43
        INQUIRE (NVOLA+INEST-1,OPENED=OPENED)                                    IN4DOB.44
        IF (.NOT. OPENED) THEN                                                   IN4DOB.45
          OPEN(NVOLA+INEST-1,                                                    IN4DOB.46
     &        FILE='MM5OBS_DOMAIN'//CHAR(INEST+ICHAR('0')),                      IN4DOB.47
     &        FORM='UNFORMATTED',STATUS='OLD')                                   IN4DOB.48
        ENDIF                                                                    IN4DOB.49
#ifndef MPP1                                                                     IN4DOB.50
   11   READ (NVOLA+INEST-1,END=12)RTIMOB,RIO(1),RJO(1),RKO(1),(                 IN4DOB.51
     +                            VAROBS(IVAR,1),IVAR=1,5)                       IN4DOB.52
#else                                                                            IN4DOB.53
   11   CONTINUE                                                                 IN4DOB.54
#  include "mpp_in4dob_10.incl"                                                  IN4DOB.55
#endif                                                                           IN4DOB.56
        IF(RTIMOB.LT.RTLAST(INEST))THEN                                          IN4DOB.57
        PRINT *,'OBS ARE NOT IN CHRONOLOGICAL ORDER'                             IN4DOB.58
        PRINT *,'NEW YEAR?'                                                      IN4DOB.59
          STOP 11                                                                IN4DOB.60
        ELSE                                                                     IN4DOB.61
          RTLAST(INEST)=RTIMOB                                                   IN4DOB.62
        ENDIF                                                                    IN4DOB.63
        JULOB=RTIMOB/100.+0.000001                                               IN4DOB.64
        RJULOB=FLOAT(JULOB)*100.                                                 IN4DOB.65
        HOUROB=RTIMOB-RJULOB                                                     IN4DOB.66
        RTIMOB=FLOAT(JULOB-JULDAY)*24.-GMT+HOUROB                                IN4DOB.67
        TIMEOB(1)=RTIMOB                                                         IN4DOB.68
        IF(RTIMOB.LT.TBACK-WINDOW)GOTO 11                                        IN4DOB.69
      ENDIF                                                                      IN4DOB.70
      GOTO 13                                                                    IN4DOB.71
12    PRINT*,'EOF REACHED ON OBSFDDA FILE: UNIT=',NVOLA+INEST-1                  IN4DOB.72
      STOP 12                                                                    IN4DOB.73
   13 CONTINUE                                                                   IN4DOB.74
      IF(NSTA.EQ.0)GOTO 70                                                       IN4DOB.75
      NDUM=0                                                                     IN4DOB.76
      DO 20 N=1,NSTA+1                                                           IN4DOB.77
        IF((TIMEOB(N)-TBACK).LT.0)THEN                                           IN4DOB.78
          TIMEOB(N)=99999.                                                       IN4DOB.79
        ENDIF                                                                    IN4DOB.80
        IF(TIMEOB(N).LT.9.E4)GOTO 30                                             IN4DOB.81
        NDUM=N                                                                   IN4DOB.82
   20 CONTINUE                                                                   IN4DOB.83
C REMOVE OLD OBS DENOTED BY 99999. AT THE FRONT OF TIMEOB ARRAY                  IN4DOB.84
   30 CONTINUE                                                                   IN4DOB.85
      NDUM=ABS(NDUM)                                                             IN4DOB.86
      NMOVE=NIOBF-NDUM                                                           IN4DOB.87
      IF(NMOVE.LE.0)GOTO 50                                                      IN4DOB.88
      IF(NDUM.EQ.0)GOTO 50                                                       IN4DOB.89
      DO 40 N=1,NMOVE                                                            IN4DOB.90
        VAROBS(1,N)=VAROBS(1,N+NDUM)                                             IN4DOB.91
        VAROBS(2,N)=VAROBS(2,N+NDUM)                                             IN4DOB.92
        VAROBS(3,N)=VAROBS(3,N+NDUM)                                             IN4DOB.93
        VAROBS(4,N)=VAROBS(4,N+NDUM)                                             IN4DOB.94
        VAROBS(5,N)=VAROBS(5,N+NDUM)                                             IN4DOB.95
        RIO(N)=RIO(N+NDUM)                                                       IN4DOB.96
        RJO(N)=RJO(N+NDUM)                                                       IN4DOB.97
        RKO(N)=RKO(N+NDUM)                                                       IN4DOB.98
        TIMEOB(N)=TIMEOB(N+NDUM)                                                 IN4DOB.99
   40 CONTINUE                                                                   IN4DOB.100
   50 CONTINUE                                                                   IN4DOB.101
      NOPEN=NMOVE+1                                                              IN4DOB.102
      IF(NOPEN.GT.NIOBF)GOTO 70                                                  IN4DOB.103
      DO 60 N=NOPEN,NIOBF                                                        IN4DOB.104
        VAROBS(1,N)=99999.                                                       IN4DOB.105
        VAROBS(2,N)=99999.                                                       IN4DOB.106
        VAROBS(3,N)=99999.                                                       IN4DOB.107
        VAROBS(4,N)=99999.                                                       IN4DOB.108
        VAROBS(5,N)=99999.                                                       IN4DOB.109
        RIO(N)=99999.                                                            IN4DOB.110
        RJO(N)=99999.                                                            IN4DOB.111
        RKO(N)=99999.                                                            IN4DOB.112
        TIMEOB(N)=99999.                                                         IN4DOB.113
   60 CONTINUE                                                                   IN4DOB.114
   70 CONTINUE                                                                   IN4DOB.115
C                                                                                IN4DOB.116
C FIND THE LAST OBS IN THE LIST                                                  IN4DOB.117
      NLAST=0                                                                    IN4DOB.118
      DO 80 N=1,NIOBF                                                            IN4DOB.119
        IF(TIMEOB(N).GT.9.E4)GOTO 90                                             IN4DOB.120
        NLAST=N                                                                  IN4DOB.121
   80 CONTINUE                                                                   IN4DOB.122
   90 CONTINUE                                                                   IN4DOB.123
C                                                                                IN4DOB.124
C NOW CHECK TO SEE IF EXTRA DATA MUST BE READ IN FROM THE                        IN4DOB.125
C DATA FILE.  CONTINUE READING UNTIL THE REACHING THE EOF                        IN4DOB.126
C (DATA TIME IS NEGATIVE) OR FIRST TIME PAST TFORWD. THE                         IN4DOB.127
C LAST OBS CURRENTLY AVAILABLE IS IN N=NMOVE.                                    IN4DOB.128
      N=NLAST                                                                    IN4DOB.129
      IF(N.EQ.0)GOTO 110                                                         IN4DOB.130
      IF(IEOF.GT.0)GOTO 130                                                      IN4DOB.131
  100 IF(TIMEOB(N).GT.TFORWD)GOTO 130                                            IN4DOB.132
C THE TIME OF THE MOST RECENTLY ACQUIRED OBS IS .LE. TFORWD,                     IN4DOB.133
C SO CONTINUE READING                                                            IN4DOB.134
  110 N=N+1                                                                      IN4DOB.135
      IF(N.GT.NIOBF)GOTO 120                                                     IN4DOB.136
C REPLACE NVOLA WITH LUN 70, AND USE NVOLA AS A FILE COUNTER                     IN4DOB.137
      NVOL=NVOLA+INEST-1                                                         IN4DOB.138
      IF(IEODI(INEST).EQ.1)GOTO 111                                              IN4DOB.139
#ifndef MPP1                                                                     IN4DOB.140
      READ (NVOL,END=111)TIMEOB(N),RIO(N),RJO(N),RKO(N),(VAROBS(IVAR,N),         IN4DOB.141
     +                  IVAR=1,5)                                                IN4DOB.142
#else                                                                            IN4DOB.143
#  include "mpp_in4dob_20.incl"                                                  IN4DOB.144
#endif                                                                           IN4DOB.145
      IF(TIMEOB(N).LT.RTLAST(INEST))THEN                                         IN4DOB.146
        PRINT *,'OBS ARE NOT IN CHRONOLOGICAL ORDER'                             IN4DOB.147
        PRINT *,'NEW YEAR?'                                                      IN4DOB.148
        STOP 111                                                                 IN4DOB.149
      ELSE                                                                       IN4DOB.150
        RTLAST(INEST)=TIMEOB(N)                                                  IN4DOB.151
      ENDIF                                                                      IN4DOB.152
CCCCCC DATA MUST BE WITHIN VERTICAL DOMAIN CCCCCCCCCC                            IN4DOB.153
      IF(RKO(N).GT.FLOAT(KX).OR.RKO(N).LT.1.)STOP 70                             IN4DOB.154
C CONVERT TIMEOB FROM JULIAN DATE AND GMT FORM TO FORECAST                       IN4DOB.155
C TIME IN HOURS (EX. TIMEOB=13002.4 REPRESENTS JULDAY 130                        IN4DOB.156
C AND GMT (HOUR) = 2.4)                                                          IN4DOB.157
      JULOB=TIMEOB(N)/100.+0.000001                                              IN4DOB.158
      RJULOB=FLOAT(JULOB)*100.                                                   IN4DOB.159
      HOUROB=TIMEOB(N)-RJULOB                                                    IN4DOB.160
      TIMEOB(N)=FLOAT(JULOB-JULDAY)*24.-GMT+HOUROB                               IN4DOB.161
      IF(IDYNIN.EQ.1.AND.TIMEOB(N)*60..GT.DATEND(INEST))THEN                     IN4DOB.162
      PRINT*,' IN4DOB: FOR INEST = ',INEST,' AT XTIME = ',XTIME,                 IN4DOB.163
     1' TIMEOB = ',TIMEOB(N)*60.,' AND DATEND = ',DATEND(INEST),' :'             IN4DOB.164
      PRINT*,'         END-OF-DATA FLAG SET FOR OBS-NUDGING',                    IN4DOB.165
     1' DYNAMIC INITIALIZATION'                                                  IN4DOB.166
        IEODI(INEST)=1                                                           IN4DOB.167
        TIMEOB(N)=99999.                                                         IN4DOB.168
      ENDIF                                                                      IN4DOB.169
      GOTO 100                                                                   IN4DOB.170
  111 CONTINUE                                                                   IN4DOB.171
      PRINT5403,NVOL,XTIME                                                       IN4DOB.172
      GOTO 112                                                                   IN4DOB.173
      N=N-1                                                                      IN4DOB.174
      N=MAX0(0,N)                                                                IN4DOB.175
      GOTO 110                                                                   IN4DOB.176
  112 CONTINUE                                                                   IN4DOB.177
      IEOF=1                                                                     IN4DOB.178
      TIMEOB(N)=99999.                                                           IN4DOB.179
      GOTO 130                                                                   IN4DOB.180
C                                                                                IN4DOB.181
C THE OBSERVATION ARRAYS ARE FULL AND THE MOST RECENTLY                          IN4DOB.182
C ACQUIRED OBS STILL HAS TIMEOB .LE. TFORWD.  SO START                           IN4DOB.183
C DECREASING THE SIZE OF THE WINDOW                                              IN4DOB.184
  120 CONTINUE                                                                   IN4DOB.185
      PRINT121,N,NIOBF                                                           IN4DOB.186
      PRINT122                                                                   IN4DOB.187
      STOP 122                                                                   IN4DOB.188
      WINDOW=WINDOW-0.1*TWINDO                                                   IN4DOB.189
      IF(TWINDO.LT.0)STOP 120                                                    IN4DOB.190
C IF THE WINDOW BECOMES NEGATIVE, THE INCOMING DATA IS                           IN4DOB.191
C PROBABLY GARBLED. STOP.                                                        IN4DOB.192
      GOTO 10                                                                    IN4DOB.193
C                                                                                IN4DOB.194
C READ CYCLE IS COMPLETED. DETERMINE THE NUMBER OF OBS IN                        IN4DOB.195
C THE CURRENT WINDOW                                                             IN4DOB.196
C                                                                                IN4DOB.197
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC                                          IN4DOB.198
C BUT FIRST, WHEN KTAU.EQ.0 (OR IN GENERAL, KTAUR), DISCARD THE                  IN4DOB.199
C "OLD" OBS FIRST...                                                             IN4DOB.200
  130 CONTINUE                                                                   IN4DOB.201
      IF(KTAU.EQ.KTAUR)THEN                                                      IN4DOB.202
        NSTA=0                                                                   IN4DOB.203
        DO 131 N=1,NIOBF                                                         IN4DOB.204
          IF(TIMEOB(N).GT.TFORWD)GOTO 132                                        IN4DOB.205
          NSTA=N                                                                 IN4DOB.206
  131   CONTINUE                                                                 IN4DOB.207
  132   CONTINUE                                                                 IN4DOB.208
        NDUM=0                                                                   IN4DOB.209
        DO 180 N=1,NSTA+1                                                        IN4DOB.210
          IF((TIMEOB(N)-TBACK).LT.0)THEN                                         IN4DOB.211
            TIMEOB(N)=99999.                                                     IN4DOB.212
          ENDIF                                                                  IN4DOB.213
          IF(TIMEOB(N).LT.9.E4)GOTO 190                                          IN4DOB.214
          NDUM=N                                                                 IN4DOB.215
  180   CONTINUE                                                                 IN4DOB.216
C REMOVE OLD OBS DENOTED BY 99999. AT THE FRONT OF TIMEOB ARRAY                  IN4DOB.217
  190   CONTINUE                                                                 IN4DOB.218
        NDUM=ABS(NDUM)                                                           IN4DOB.219
        NMOVE=NIOBF-NDUM                                                         IN4DOB.220
        IF(NMOVE.LE.0)GOTO 210                                                   IN4DOB.221
        IF(NDUM.EQ.0)GOTO 210                                                    IN4DOB.222
        DO 200 N=1,NMOVE                                                         IN4DOB.223
          VAROBS(1,N)=VAROBS(1,N+NDUM)                                           IN4DOB.224
          VAROBS(2,N)=VAROBS(2,N+NDUM)                                           IN4DOB.225
          VAROBS(3,N)=VAROBS(3,N+NDUM)                                           IN4DOB.226
          VAROBS(4,N)=VAROBS(4,N+NDUM)                                           IN4DOB.227
          VAROBS(5,N)=VAROBS(5,N+NDUM)                                           IN4DOB.228
          RIO(N)=RIO(N+NDUM)                                                     IN4DOB.229
          RJO(N)=RJO(N+NDUM)                                                     IN4DOB.230
          RKO(N)=RKO(N+NDUM)                                                     IN4DOB.231
          TIMEOB(N)=TIMEOB(N+NDUM)                                               IN4DOB.232
  200   CONTINUE                                                                 IN4DOB.233
  210   CONTINUE                                                                 IN4DOB.234
        NOPEN=NMOVE+1                                                            IN4DOB.235
        IF(NOPEN.GT.NIOBF)GOTO 230                                               IN4DOB.236
        DO 220 N=NOPEN,NIOBF                                                     IN4DOB.237
          VAROBS(1,N)=99999.                                                     IN4DOB.238
          VAROBS(2,N)=99999.                                                     IN4DOB.239
          VAROBS(3,N)=99999.                                                     IN4DOB.240
          VAROBS(4,N)=99999.                                                     IN4DOB.241
          VAROBS(5,N)=99999.                                                     IN4DOB.242
          RIO(N)=99999.                                                          IN4DOB.243
          RJO(N)=99999.                                                          IN4DOB.244
          RKO(N)=99999.                                                          IN4DOB.245
          TIMEOB(N)=99999.                                                       IN4DOB.246
  220   CONTINUE                                                                 IN4DOB.247
  230   CONTINUE                                                                 IN4DOB.248
      ENDIF                                                                      IN4DOB.249
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC                                          IN4DOB.250
      NSTA=0                                                                     IN4DOB.251
      DO 140 N=1,NIOBF                                                           IN4DOB.252
        IF(TIMEOB(N).GT.TFORWD)GOTO 150                                          IN4DOB.253
        NSTA=N                                                                   IN4DOB.254
  140 CONTINUE                                                                   IN4DOB.255
C                                                                                IN4DOB.256
  150 PRINT160,KTAU,XTIME,NSTA                                                   IN4DOB.257
      IF(KTAU.EQ.KTAUR)THEN                                                      IN4DOB.258
        IF(I4DI(1).EQ.1)THEN                                                     IN4DOB.259
          TWDOP=TWINDO*60.                                                       IN4DOB.260
          PRINT1449,INEST,RINXY,RINSIG,TWDOP                                     IN4DOB.261
          IF(ISWIND(INEST).EQ.1)PRINT1450,GIV(INEST)                             IN4DOB.262
          IF(ISTEMP(INEST).EQ.1)PRINT1451,GIT(INEST)                             IN4DOB.263
          IF(ISMOIS(INEST).EQ.1)PRINT1452,GIQ(INEST)                             IN4DOB.264
          IF(ISPSTR(INEST).EQ.1)PRINT1453,GIP(INEST)                             IN4DOB.265
        ENDIF                                                                    IN4DOB.266
      ENDIF                                                                      IN4DOB.267
      IF(KTAU.EQ.KTAUR)THEN                                                      IN4DOB.268
        PRINT553                                                                 IN4DOB.269
        PRINT554                                                                 IN4DOB.270
        IF(IWTSIG(INEST).NE.1)THEN                                               IN4DOB.271
          PRINT555                                                               IN4DOB.272
          PRINT556,RINFMN*RINXY,RINFMX*RINXY,PFREE*10.                           IN4DOB.273
          IF(RINFMN.GT.RINFMX)STOP 556                                           IN4DOB.274
C IS MINIMUM GREATER THAN MAXIMUM?                                               IN4DOB.275
          PRINT557,DPSMX*10.,DCON                                                IN4DOB.276
          IF(DPSMX.GT.10.)STOP 557                                               IN4DOB.277
        ENDIF                                                                    IN4DOB.278
      ENDIF                                                                      IN4DOB.279
C IS DPSMX IN CB?                                                                IN4DOB.280
C                                                                                IN4DOB.281
      IF(KTAU.EQ.KTAUR)THEN                                                      IN4DOB.282
        PRINT601,INEST,IONF                                                      IN4DOB.283
      ENDIF                                                                      IN4DOB.284
      NSTAT(INEST)=NSTA                                                          IN4DOB.285
555   FORMAT(1X,'   ABOVE THE SURFACE LAYER, OBS NUDGING IS PERFORMED',          IN4DOB.286
     1' ON PRESSURE LEVELS,')                                                    IN4DOB.287
556   FORMAT(1X,'   WHERE RINXY VARIES LINEARLY FROM ',E11.3,' KM AT',           IN4DOB.288
     1' THE SURFACE TO ',E11.3,' KM AT ',F7.2,' MB AND ABOVE')                   IN4DOB.289
557   FORMAT(1X,'   IN THE SURFACE LAYER, WXY IS A FUNCTION OF ',                IN4DOB.290
     1'DPSMX = ',F7.2,' MB WITH DCON = ',E11.3,                                  IN4DOB.291
     2' - SEE SUBROUTINE NUDOB')                                                 IN4DOB.292
601   FORMAT('0','FOR EFFICIENCY, THE OBS NUDGING FREQUENCY ',                   IN4DOB.293
     1  'FOR MESH #',I2,' IS ',1I2,' CGM TIMESTEPS ',/)                          IN4DOB.294
121   FORMAT('0','!!! WARNING: NOBS  = ',I4,' IS GREATER THAN NIOBF = ',         IN4DOB.295
     1I4,': INCREASE PARAMETER NIOBF')                                           IN4DOB.296
5403  FORMAT(1H0,'-------------EOF REACHED FOR NVOL = ',I3,                      IN4DOB.297
     1 ' AND XTIME = ',F6.2,'------------------------')                          IN4DOB.298
122   FORMAT(1X,'     ...OR THE CODE WILL REDUCE THE TIME WINDOW')               IN4DOB.299
160   FORMAT('0','******CALL IN4DOB AT KTAU = ',I5,' AND XTIME = ',              IN4DOB.300
     1F7.2,':  NSTA = ',I5,'******')                                             IN4DOB.301
1449  FORMAT(1H0,'*****NUDGING INDIVIDUAL OBS ON MESH #',I2,                     IN4DOB.302
     & ' WITH RINXY = ',                                                         IN4DOB.303
     1E11.3,' KM, RINSIG = ',E11.3,' AND TWINDO (HALF-PERIOD) = ',               IN4DOB.304
     2E11.3,' MIN')                                                              IN4DOB.305
1450  FORMAT(1X,'NUDGING IND. OBS WINDS WITH GIV = ',E11.3)                      IN4DOB.306
1451  FORMAT(1X,'NUDGING IND. OBS TEMPERATURE WITH GIT = ',E11.3)                IN4DOB.307
1452  FORMAT(1X,'NUDGING IND. OBS MOISTURE WITH GIQ = ',E11.3)                   IN4DOB.308
1453  FORMAT(1X,'NUDGING IND. OBS SURFACE PRESSURE WITH GIP = ,'E11.3)           IN4DOB.309
1469  FORMAT(1H0,'*****NUDGING INDIVIDUAL OBS ON FGM WITH RINXY = ',             IN4DOB.310
     1E11.3,' KM, RINSIG = ',E11.3,' AND TWINDO (HALF-PERIOD) = ',               IN4DOB.311
     2E11.3,' MIN')                                                              IN4DOB.312
553   FORMAT(1X,'BY DEFAULT: OBS NUDGING OF TEMPERATURE AND MOISTURE ',          IN4DOB.313
     1'IS RESTRICTED TO ABOVE THE BOUNDARY LAYER')                               IN4DOB.314
554   FORMAT(1X,'...WHILE OBS NUDGING OF WIND IS INDEPENDENT OF THE ',           IN4DOB.315
     1'BOUNDARY LAYER')                                                          IN4DOB.316
      RETURN                                                                     IN4DOB.317
      END                                                                        IN4DOB.318
                                                                                 IN4DOB.319
