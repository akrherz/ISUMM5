      SUBROUTINE IN4DGD(IN,IYY,JXX,KZZ)                                          IN4DGD.1
      IMPLICIT NONE                                                              IN4DGD.2
C     REMOVE INT4 OPTION (ALWAYS 1)                                              IN4DGD.3
C NOTE: FOR V9 THIS SUBROUTINE ONLY READS GRIDDED CGM FDDA INPUT                 IN4DGD.4
C       THE READS FOR THE ____N VARIABLES HAVE BEEN REMOVED.                     IN4DGD.5
C       SOME SMALLER PORTIONS OF CODE INVOLVING THE FGM PARAMETERS               IN4DGD.6
C       ARE COMMENTED OUT WITH "CCC" FOR GUIDANCE ON FUTURE HANDLING             IN4DGD.7
C       OF THE NESTING...                                                        IN4DGD.8
C                                                                                IN4DGD.9
C THIS SUBROUTINE READS IN THE "OBSERVED" GRID DATA TO BE ASSIMILATED            IN4DGD.10
C BY THE ANALYSIS NUDGING MODULE OF THE FDDA CODE.                               IN4DGD.11
C                                                                                IN4DGD.12
C IN (=INEST) = 1 FOR CGM                                                        IN4DGD.13
C                                                                                IN4DGD.14
C IN4DGD HAS BEEN MODIFIED TO AUTOMATICALLY CONVERT THE DATE/TIME                IN4DGD.15
C GROUP READ IN, TO THE CORRESPONDING MODEL TIME.  DIFTIM(IN), THE               IN4DGD.16
C DIFFERENCE IN TIME BETWEEN SUCCESSIVE ANALYSIS TIMES, IS                       IN4DGD.17
C IN MINUTES, AND IS SPECIFIED IN PARAM.  THIS VALUE IS ALSO                     IN4DGD.18
C CHECKED AGAINST THE DATA TIMES FOR CONSISTENCY.                                IN4DGD.19
C                                                                                IN4DGD.20
C IN4DGD ALSO READS AND ARCHIVES SURFACE DATA FOR FOUR-DIMENSIONAL               IN4DGD.21
C DATA ASSIMILATION.  THESE DATA COME FROM A FILE WRITTEN IN                     IN4DGD.22
C RAWINS AND CONTAINS SURFACE ANALYSES OF WIND, TEMP, MIXR,                      IN4DGD.23
C PSFC (PSB), SLP, AND RH.  THESE DATA ARE READ IF I4D(IN) AND                   IN4DGD.24
C I4D(IN+2) = 1. DIFTIM(IN+2) IN MINUTES, AND ALSO SPECIFIED IN PARAM,           IN4DGD.25
C REPRESENTS THE MINIMUM DIFTIM FOR SURFACE DATA TO BE ARCHIVED.                 IN4DGD.26
C IN OTHER WORDS, HAVING SPECIFIED DIFTIM(1,2) TO BE 360. WILL CAUSE THE         IN4DGD.27
C SURFACE DATA TO BE ARCHIVED ONLY EVERY 6 H (RATHER THAN EVERY 3 H, IF          IN4DGD.28
C 3-HLY DATA ARE ALSO ON THE RAWINS TAPE) ON THE COARSE GRID MESH.               IN4DGD.29
C                                                                                IN4DGD.30
C IN4DGD HAS NOT BEEN TESTED RECENTLY FOR TARGET NUDGING OF THE 3-D              IN4DGD.31
C RAWINS ANALYSES.  FOR NOW, NUDGING SURFACE DATA IN THE BL IS ONLY              IN4DGD.32
C POSSIBLE WHEN NUDGING TO TEMPORALLY-INTERPOLATED ANALYSES OF 3-D               IN4DGD.33
C RAWINS ANALYSES (INT4(IN,1)=1).  BLNUDGD USES XTIM1 AND XTIM2 TO BRACK         IN4DGD.34
C THE SURFACE ANALYSES, ETC. AND THEREFORE, TARGET NUDGING, WHICH ONLY U         IN4DGD.35
C XTIM1 (THE TARGET ANALYSIS TIME) IS NOT COMPATIBLE WITH THE BLNUDGD            IN4DGD.36
C CODE.  THEREFORE, IF INT4(IN,1).NE.1 IN IN4DGD WHEN I4D(IN+2))=1,              IN4DGD.37
C THE PROGRAM IS STOPPED WITH STOP2010 (CGM) OR STOP12010 (FGM).                 IN4DGD.38
C                                                                                IN4DGD.39
C NOTE: UNITS 11 AND 31 ARE FOR CGM INPUT 3-D ANALYSES                           IN4DGD.40
C       UNITS 12 AND 32 ARE FOR FGM INPUT 3-D ANALYSES                           IN4DGD.41
C       UNITS 71 (CGM) AND 72 (FGM) ARE FOR INPUT SURFACE ANALYSES               IN4DGD.42
C                                                                                IN4DGD.43
C                                                                                IN4DGD.44
C                                                                                IN4DGD.45
#     include <parame.incl>                                                      IN4DGD.46
#     include <param2.incl>                                                      IN4DGD.47
#     include <parfdda.incl>                                                     IN4DGD.48
#     include <param3.incl>                                                      IN4DGD.49
#     include <fddagd.incl>                                                      IN4DGD.50
#     include <various.incl>                                                     IN4DGD.51
#     include <hdrv3.incl>                                                       IN4DGD.52
#     include <chardate.incl>                                                    IN4DGD.53
      REAL HDOTH(MIX*MJX*(MKX+1))                                                IN4DGD.57
C                                                                                IN4DGD.58
      INTEGER ICHK(NVAR)                                                         IN4DGD.59
      REAL VAROUT(MIXFG,MJXFG)                                                   IN4DGD.60
      REAL PSODOT(MIX,MJX)                                                       IN4DGD.61
      REAL JULGMT1,JULGMT2,JULGMTM,JULGMTS                                       IN4DGD.62
      CHARACTER *19CDATE1,CDATE2,CDATENEW,CDATES                                 IN4DGD.63
      CHARACTER *8NAME8                                                          IN4DGD.64
      LOGICAL OPENED                                                             IN4DGD.65
C                                                                                IN4DGD.66
      INTEGER I,J,K                                                              IN4DGD.67
      INTEGER IN,IUNS,KTAURB,KTAUB,NV,NVV                                        IN4DGD.68
      INTEGER NDIM,LENDIM1,LENDIM2,LENDIM3,IX,JX,IDTS                            IN4DGD.69
      INTEGER ICHT,ISAVE,NN,IYY,JXX,KZZ,NT                                       IN4DGD.70
C                                                                                IN4DGD.71
      REAL XTIM,XT1SAV,XTNSAV,DIFFHR,DIFJ,DIFT                                   IN4DGD.72
      REAL TIMANL2,RDXTIM1,TIMANL1,TIMANLS                                       IN4DGD.73
C                                                                                IN4DGD.74
      LOGICAL DM_IONODE                                                          IN4DGD.75
      EXTERNAL DM_IONODE                                                         IN4DGD.76
C                                                                                IN4DGD.77
C-------------------------------------------------------------------             IN4DGD.78
C                                                                                IN4DGD.79
      IUN1=10+IN                                                                 IN4DGD.80
      IUN2=30+IN                                                                 IN4DGD.81
      IUNS=70+IN                                                                 IN4DGD.82
                                                                                 05DEC01.588
C CHECK IF IT NEEDS TO READ DATA. e.g. at the end of data time                   05DEC01.589
      IF(IDYNIN.EQ.1.AND.XTIME.GT.DATEND(IN))THEN                                05DEC01.590
      PRINT*,' IN4DGD: FOR INEST = ',IN,' AT XTIME = ',XTIME,                    05DEC01.591
     1'AND DATEND = ',DATEND(IN),' :'                                            05DEC01.592
      PRINT*,'         END-OF-DATA FLAG SET FOR ANALYSIS-NUDGING',               05DEC01.593
     1'DYNAMIC INITIALIZATION'                                                   05DEC01.594
        IEOD(IN)=1                                                               05DEC01.595
        RETURN                                                                   05DEC01.596
      ENDIF                                                                      05DEC01.597
                                                                                 05DEC01.598
C_FLIC_BEGIN_NOFLIC                                                              IN4DGD.83
      IF(DM_IONODE())THEN                                                        IN4DGD.84
        INQUIRE (IUN1,OPENED=OPENED)                                             IN4DGD.85
        IF (.NOT. OPENED) THEN                                                   IN4DGD.86
          OPEN(IUN1, FILE='MMINPUT_DOMAIN'//CHAR(IN+ICHAR('0')),                 IN4DGD.87
     &         FORM='UNFORMATTED',STATUS='OLD',ERR=9010)                         25JAN00.339
        ENDIF                                                                    IN4DGD.89
        INQUIRE (IUN2,OPENED=OPENED)                                             IN4DGD.90
#if defined(rs6000)||defined(SGI_Origin)||defined(linux)   
        IF (.NOT. OPENED) THEN                                                   IN4DGD.92
          OPEN(IUN2, FILE='MMINPUT2_DOMAIN'//CHAR(IN+ICHAR('0')),                IN4DGD.93
     &         FORM='UNFORMATTED',STATUS='OLD',ERR=9020)                         25JAN00.341
        ENDIF                                                                    IN4DGD.95
#elif defined(CRAY)||defined(crayx1) 
        IF (.NOT. OPENED) THEN                                                   23DEC04.2689
          OPEN(IUN2, FILE='MMINPUT2_DOMAIN'//CHAR(IN+ICHAR('0')),                23DEC04.2690
     &         FORM='UNFORMATTED',STATUS='OLD',ERR=9020)                         23DEC04.2691
        ENDIF                                                                    23DEC04.2692
#else                                                                            IN4DGD.96
        IF (.NOT. OPENED) THEN                                                   IN4DGD.97
          OPEN(IUN2, FILE='MMINPUT_DOMAIN'//CHAR(IN+ICHAR('0')),                 IN4DGD.98
     &         FORM='UNFORMATTED',STATUS='OLD',ERR=9020)                         25JAN00.342
        ENDIF                                                                    IN4DGD.100
#endif                                                                           IN4DGD.101
      ENDIF                                                                      IN4DGD.102
C                                                                                IN4DGD.103
C KTAURB MUST BE KTAUR OR KTAURN, DEPENDING ON INEST (="IN")                     IN4DGD.104
C                                                                                IN4DGD.105
      KTAURB=0                                                                   IN4DGD.106
      IF(IFREST)KTAURB=KTAUR                                                     IN4DGD.107
      KTAUB=KTAU                                                                 IN4DGD.108
      XTIM=XTIME                                                                 IN4DGD.109
      IF(DM_IONODE())THEN                                                        IN4DGD.110
        IF(KTAUB.EQ.KTAURB)THEN                                                  IN4DGD.111
          REWIND (IUN1)                                                          IN4DGD.112
          IF(ISKF.GT.0)THEN                                                      IN4DGD.113
            DO NV=1,ISKF                                                         IN4DGD.114
  400         CONTINUE                                                           IN4DGD.115
              READ (IUN1)FLAG                                                    IN4DGD.116
              IF(FLAG.EQ.0)THEN                                                  IN4DGD.117
                READ (IUN1)JBHI,JBHR,JBHIC,JBHRC                                 IN4DGD.118
                GOTO 400                                                         IN4DGD.119
              ELSEIF(FLAG.EQ.1)THEN                                              IN4DGD.120
                READ (IUN1)                                                      IN4DGD.121
                READ (IUN1)                                                      IN4DGD.122
                GOTO 400                                                         IN4DGD.123
              ELSEIF(FLAG.NE.2)THEN                                              IN4DGD.124
                print *,'error in fdda read: unit=',iun1                         IN4DGD.125
#ifndef MPP1                                                                     25JAN00.343
                STOP 'in4dgd 1'                                                  IN4DGD.126
#else                                                                            25JAN00.344
                CALL FKILL_MODEL( 'in4dgd 1' )                                   25JAN00.345
#endif                                                                           25JAN00.346
              ENDIF                                                              IN4DGD.127
            ENDDO                                                                IN4DGD.128
          ENDIF                                                                  IN4DGD.129
        ENDIF                                                                    IN4DGD.130
        IF(KTAUB.EQ.KTAURB)THEN                                                  IN4DGD.131
          REWIND (IUN2)                                                          IN4DGD.132
C IN THE JCL FOR THE CGM FOR EXAMPLE, UNIT 11 IS COPIED INTO UNIT 31,            IN4DGD.133
C AND UNIT 31 IS SET 1 FILE MARK AHEAD OF UNIT 11 SO THAT BOTH TIMES FOR         IN4DGD.134
C THE LINEAR INTERPOLATION IN TIME CAN BE READ SIMULTANEOUSLY.                   IN4DGD.135
          DO NV=1,ISKF+1                                                         IN4DGD.136
  401       CONTINUE                                                             IN4DGD.137
            READ (IUN2)FLAG                                                      IN4DGD.138
            IF(FLAG.EQ.0)THEN                                                    IN4DGD.139
              READ (IUN2)                                                        IN4DGD.140
              GOTO 401                                                           IN4DGD.141
            ELSEIF(FLAG.EQ.1)THEN                                                IN4DGD.142
              READ (IUN2)                                                        IN4DGD.143
              READ (IUN2)                                                        IN4DGD.144
              GOTO 401                                                           IN4DGD.145
            ELSEIF(FLAG.NE.2)THEN                                                IN4DGD.146
              print *,'error in fdda read: unit=',iun2                           IN4DGD.147
#ifndef MPP1                                                                     25JAN00.347
              STOP 'in4dgd 2'                                                    IN4DGD.148
#else                                                                            25JAN00.348
              CALL FKILL_MODEL( 'in4dgd 2' )                                     25JAN00.349
#endif                                                                           25JAN00.350
            ENDIF                                                                IN4DGD.149
          ENDDO                                                                  IN4DGD.150
        ENDIF                                                                    IN4DGD.151
      ENDIF                                                                      IN4DGD.152
      PRINT333,KTAUB,XTIM,IN                                                     IN4DGD.153
333   FORMAT(1H0,'******CALL IN4DGD AT KTAU = ',I4,' AND ',                      IN4DGD.154
     1'XTIME = ',F7.2,' FOR INEST = ',I2,'******')                               IN4DGD.155
C                                                                                IN4DGD.156
  500 CONTINUE                                                                   IN4DGD.157
      XT1SAV=XTIM1(IN)                                                           05DEC01.599
      XTNSAV=XTNEX(IN)                                                           05DEC01.600
#ifndef MPP1                                                                     25JAN00.351
      READ (IUN1,END=7002)FLAG                                                   25JAN00.352
#else                                                                            25JAN00.353
      IF(DM_IONODE()) THEN                                                       25JAN00.354
         READ (IUN1,END=9002)FLAG                                                25JAN00.355
         GOTO 8002                                                               25JAN00.356
 9002    CONTINUE                                                                25JAN00.357
         FLAG = FILEDONE                                                         25JAN00.358
 8002    CONTINUE                                                                25JAN00.359
      ENDIF                                                                      25JAN00.360
      CALL DM_BCAST_INTEGERS(FLAG,1)                                             IN4DGD.160
      IF(FLAG.EQ.FILEDONE) GOTO 7002                                             25JAN00.361
#endif                                                                           IN4DGD.161
      IF(FLAG.EQ.0)THEN                                                          IN4DGD.162
        IF(DM_IONODE())THEN                                                      IN4DGD.163
          READ (IUN1)JBHI,JBHR,JBHIC,JBHRC                                       IN4DGD.164
        ENDIF                                                                    IN4DGD.165
#ifdef MPP1                                                                      IN4DGD.166
        CALL DM_BCAST_INTEGERS(JBHI,NUMINT*NUMPROGS)                             IN4DGD.167
        CALL DM_BCAST_REALS(JBHR,NUMREAL*NUMPROGS)                               25JAN00.362
#endif                                                                           IN4DGD.169
        GOTO 500                                                                 IN4DGD.170
      ELSEIF(FLAG.EQ.1)THEN                                                      IN4DGD.171
        IF(DM_IONODE())THEN                                                      IN4DGD.172
          READ (IUN1)NDIM,START_INDEX,END_INDEX,RTIME,STAGGERING,                IN4DGD.173
     +               ORDERING,CURRENT_DATE,NAME,UNITS,DESCRIPTION                IN4DGD.174
        ENDIF                                                                    IN4DGD.175
#ifdef MPP1                                                                      IN4DGD.176
        CALL DM_BCAST_INTEGERS(NDIM,1)                                           IN4DGD.177
        CALL DM_BCAST_INTEGERS(START_INDEX,LEN_START_INDEX)                      IN4DGD.178
        CALL DM_BCAST_INTEGERS(END_INDEX,LEN_END_INDEX)                          IN4DGD.179
        CALL DM_BCAST_REALS(RTIME,1)                                             IN4DGD.180
        CALL DM_BCAST_STRING(STAGGERING,LEN_STAGGERING)                          IN4DGD.181
        CALL DM_BCAST_STRING(ORDERING,LEN_ORDERING)                              IN4DGD.182
        CALL DM_BCAST_STRING(CURRENT_DATE,LEN_CURRENT_DATE)                      IN4DGD.183
        CALL DM_BCAST_STRING(NAME,LEN_NAME)                                      IN4DGD.184
        CALL DM_BCAST_STRING(UNITS,LEN_UNITS)                                    IN4DGD.185
        CALL DM_BCAST_STRING(DESCRIPTION,LEN_DESCRIPTION)                        IN4DGD.186
#endif                                                                           IN4DGD.187
        CDATE1=CURRENT_DATE(1:19)                                                IN4DGD.188
        LENDIM1=(END_INDEX(1)-START_INDEX(1)+1)                                  IN4DGD.189
        LENDIM2=(END_INDEX(2)-START_INDEX(2)+1)                                  IN4DGD.190
        LENDIM3=(END_INDEX(3)-START_INDEX(3)+1)                                  IN4DGD.191
        IX=LENDIM1                                                               IN4DGD.192
        JX=LENDIM2                                                               IN4DGD.193
        IF(NDIM.GE.2)THEN                                                        IN4DGD.194
#ifndef MPP1                                                                     IN4DGD.195
          IF (LENDIM1*LENDIM2*LENDIM3.LE.MIX*MJX*(MKX+1)) THEN                   IN4DGD.196
            READ (IUN1)(HDOTH(I),I=1,LENDIM1*LENDIM2*LENDIM3)                    IN4DGD.197
          ENDIF                                                                  IN4DGD.198
#else                                                                            IN4DGD.199
          DO I = 1, MIX*MJX*(MKX+1)                                              IN4DGD.200
            HDOTH(I) = 0.                                                        IN4DGD.201
          ENDDO                                                                  IN4DGD.202
          CALL DM_DIST_READ(IUN1,IN,HDOTH,NDIM,LENDIM1,LENDIM2,LENDIM3,          IN4DGD.203
     +         MIX,MJX,MKX+1)                                                    IN4DGD.204
#endif                                                                           IN4DGD.205
        ELSE                                                                     IN4DGD.206
          IF(DM_IONODE())READ (IUN1)                                             IN4DGD.207
        ENDIF                                                                    IN4DGD.208
        NAME8=NAME(1:8)                                                          IN4DGD.209
        IF(NAME8.EQ.'U       ')THEN                                              IN4DGD.210
          CALL EQUATE(HDOTH,IX,JX,MKX,UBO,MIX,MJX,MKX)                           IN4DGD.211
        ELSEIF(NAME8.EQ.'V       ')THEN                                          IN4DGD.212
          CALL EQUATE(HDOTH,IX,JX,MKX,VBO,MIX,MJX,MKX)                           IN4DGD.213
        ELSEIF(NAME8.EQ.'T       ')THEN                                          IN4DGD.214
          CALL EQUATE(HDOTH,IX,JX,MKX,TBO,MIX,MJX,MKX)                           IN4DGD.215
        ELSEIF(NAME8.EQ.'Q       ')THEN                                          IN4DGD.216
          CALL EQUATE(HDOTH,IX,JX,MKX,QBO,MIX,MJX,MKX)                           IN4DGD.217
        ELSEIF(NAME8.EQ.'PSTARCRS')THEN                                          IN4DGD.218
          CALL EQUATE(HDOTH,IX,JX,1,PSO,MIX,MJX,1)                               IN4DGD.219
        ENDIF                                                                    IN4DGD.220
        GOTO 500                                                                 IN4DGD.221
      ELSEIF(FLAG.NE.2)THEN                                                      IN4DGD.222
        print *,'error in fdda read: unit=',iun1                                 IN4DGD.223
#ifndef MPP1                                                                     25JAN00.363
        STOP 'in4dgd 3'                                                          IN4DGD.224
#else                                                                            25JAN00.364
        CALL FKILL_MODEL( 'in4dgd 3' )                                           25JAN00.365
#endif                                                                           25JAN00.366
      ENDIF                                                                      IN4DGD.225
      PRINT7000,CDATE1,IUN1                                                      IN4DGD.226
7000  FORMAT(1X,'****READ OBS ANALYSES FOR ',A19,' ON UNIT ',I2)                 IN4DGD.227
C DETERMINE TIMANL(IN) AND COMPARE TO MODEL TIME (XTIME) IN MINUTES              IN4DGD.228
C USE CURRENT CGM TIME, XTIME, FOR THE OBSERVATION TIME/MODEL TIME               IN4DGD.229
C CONSISTENCY CHECKS FOR BOTH THE CGM AND FGM...                                 IN4DGD.230
C***      CALL JULGMT(CDATE1,JULGMT1,TIMANL(IN),JULDAY,GMT,0)                    IN4DGD.231
      CALL GETH_IDTS(CDATE1,CDATE,IDTS)                                          IN4DGD.232
      TIMANL(IN)=FLOAT(IDTS)/60.                                                 IN4DGD.233
C IF RESTARTING...                                                               IN4DGD.234
      IF(IFREST.AND.KTAUB.EQ.KTAURB)THEN                                         IN4DGD.235
        IF(XTIM.GE.TIMANL(IN).AND.XTIM.LT.TIMANL(IN)+DIFTIM(IN,1))THEN           IN4DGD.236
          GOTO 550                                                               IN4DGD.237
        ENDIF                                                                    IN4DGD.238
        IF(DM_IONODE())THEN                                                      IN4DGD.239
  403     CONTINUE                                                               IN4DGD.240
          READ (IUN2)FLAG                                                        IN4DGD.241
          IF(FLAG.EQ.0)THEN                                                      IN4DGD.242
            READ (IUN2)                                                          IN4DGD.243
            GOTO 403                                                             IN4DGD.244
          ELSEIF(FLAG.EQ.1)THEN                                                  IN4DGD.245
            READ (IUN2)                                                          IN4DGD.246
            READ (IUN2)                                                          IN4DGD.247
            GOTO 403                                                             IN4DGD.248
          ELSEIF(FLAG.NE.2)THEN                                                  IN4DGD.249
            print *,'error in fdda read: unit=',iun2                             IN4DGD.250
#ifndef MPP1                                                                     25JAN00.367
            STOP 'in4dgd 4'                                                      IN4DGD.251
#else                                                                            25JAN00.368
            CALL FKILL_MODEL( 'in4dgd 4' )                                       25JAN00.369
#endif                                                                           25JAN00.370
          ENDIF                                                                  IN4DGD.252
        ENDIF                                                                    IN4DGD.253
        GOTO 500                                                                 IN4DGD.254
      ENDIF                                                                      IN4DGD.255
  550 CONTINUE                                                                   IN4DGD.256
C                                                                                05DEC01.601
      XTIM1(IN)=TIMANL(IN)                                                       IN4DGD.267
C XTIM2 WILL BE CHECKED WITH THE OBSERVED DATE AND TIME AFTER UNIT 71 (C         IN4DGD.268
C OR UNIT 73 (FGM) IS READ BELOW:                                                IN4DGD.269
      XTIM2(IN)=XTIM1(IN)+DIFTIM(IN,1)                                           IN4DGD.270
      XTNEX(IN)=XTIM2(IN)                                                        IN4DGD.271
C COMPUTE THE CURRENT MODEL CDATENEW                                             IN4DGD.272
C***      CALL JULGMT(MDATE,JULGMTM,XTIME,JULDAY,GMT,1)                          IN4DGD.273
      CALL GETH_NEWDATE(CDATENEW,CDATE,NINT(XTIME*60.))                          IN4DGD.274
      IF(IFREST.AND.KTAUB.EQ.KTAURB)GOTO 7999                                    IN4DGD.275
      CALL GETH_IDTS(CDATENEW,CDATE1,IDTS)                                       IN4DGD.276
      DIFFHR=FLOAT(IDTS)/3600.                                                   IN4DGD.277
      IF(ABS(DIFFHR).GT.1.1*DTMIN/60..OR.ABS(XTIME-XTIM1(IN)).GT.1.1*            IN4DGD.278
     +  DTMIN)THEN                                                               IN4DGD.279
        PRINT4000,XTIME,CDATENEW,CDATE1,XTIM1(IN)                                IN4DGD.280
4000  FORMAT(1X,'---- STOPPING EXECUTION AT XTIME = ',F8.2,                      IN4DGD.281
     1':',/,5X,' MODEL CDATE = ',A19,' DOES NOT MATCH INPUT DATA',               IN4DGD.282
     2' CDATE = ',A19,' AND TIMANL = ',F8.2,' ----')                             IN4DGD.283
        DIFJ=DIFFHR                                                              IN4DGD.284
        DIFT=XTIME-XTIM1(IN)                                                     IN4DGD.285
        PRINT4010,DTMIN,JULDAY,GMT,DIFTIM(IN,1),DIFJ,DIFT                        IN4DGD.286
4010  FORMAT(1X,'DTMIN,JULDAY,GMT,DIFTIM,DIFJ,DIFT',1X,F5.2,1X,I3,               IN4DGD.287
     14(1X,F10.4))                                                               IN4DGD.288
#ifndef MPP1                                                                     25JAN00.371
        STOP 70                                                                  IN4DGD.289
#else                                                                            25JAN00.372
        CALL FKILL_MODEL( 'IN4DGD 70' )                                          25JAN00.373
#endif                                                                           25JAN00.374
      ENDIF                                                                      IN4DGD.290
 7999 CONTINUE                                                                   IN4DGD.291
      PRINT8000,CDATE1,XTIM1(IN)                                                 IN4DGD.292
8000  FORMAT(1X,'----ARCHIVING OBS ANALYSES FOR ',A19,' AND TIMANL = ',          IN4DGD.293
     1F7.2,' MINUTES')                                                           IN4DGD.294
      GOTO 7004                                                                  IN4DGD.295
 7002 CONTINUE                                                                   IN4DGD.296
      IF(IFREST.AND.KTAUB.EQ.KTAURB)THEN                                         IN4DGD.297
        PRINT551,IUN1                                                            IN4DGD.298
551   FORMAT(1X,'**** XTIME NOT BRACKETED BY DATA ON UNIT ',I2,                  IN4DGD.299
     1' - CHECK INPUT ****')                                                     IN4DGD.300
#ifndef MPP1                                                                     25JAN00.375
        STOP 551                                                                 IN4DGD.301
#else                                                                            25JAN00.376
        CALL FKILL_MODEL( 'IN4DGD 551' )                                         25JAN00.377
#endif                                                                           25JAN00.378
      ENDIF                                                                      IN4DGD.302
      PRINT7003,IUN1                                                             IN4DGD.303
7003  FORMAT(1X,'**** END OF FILE REACHED ON UNIT ',I2,                          IN4DGD.304
     1' IN IN4DGD XXXX')                                                         IN4DGD.305
C                                                                                IN4DGD.306
      IF(I4D(IN,2).EQ.1)THEN                                                     IN4DGD.307
        DO 7010 NVV=1,8                                                          IN4DGD.308
          IF(IDYNIN.NE.1)TIME(NVV)=TIME(NVV)+15.                                 IN4DGD.309
 7010   CONTINUE                                                                 IN4DGD.310
      ENDIF                                                                      IN4DGD.311
      IF(IDYNIN.EQ.1)THEN                                                        IN4DGD.312
        XTIM1(IN)=XT1SAV                                                         IN4DGD.313
        XTNEX(IN)=XTNSAV                                                         IN4DGD.314
        RETURN                                                                   IN4DGD.315
      ENDIF                                                                      IN4DGD.316
C                                                                                IN4DGD.317
      PRINT7020                                                                  IN4DGD.318
7020  FORMAT(1X,'XXXX WARNING: PAST END OF DATA FOR FDDA XXXX')                  IN4DGD.319
      IF(KTAUB.EQ.0)THEN                                                         IN4DGD.320
        PRINT7019                                                                IN4DGD.321
7019  FORMAT(1X,'XXXX ...APPARENTLY, NO DATA AVAILABLE FOR FDDA: CHECK '         IN4DGD.322
     1       'INPUT XXXX')                                                       IN4DGD.323
#ifndef MPP1                                                                     25JAN00.379
        STOP 7002                                                                IN4DGD.324
#else                                                                            25JAN00.380
        CALL FKILL_MODEL( 'IN4DGD 7002' )                                        25JAN00.381
#endif                                                                           25JAN00.382
      ENDIF                                                                      IN4DGD.325
      PRINT7021                                                                  IN4DGD.326
7021  FORMAT(1X,'XXXX EXECUTION MUST STOP WITHIN 15 MINUTES XXXX')               IN4DGD.327
      IF(ABS(TIMAX-XTIME).GT.15.-DTMIN)THEN                                      IN4DGD.328
        TIMAX=XTIME+15.-DTMIN                                                    IN4DGD.329
        PRINT7022,TIMAX,XTIME                                                    IN4DGD.330
7022  FORMAT(1X,'XXXX TIMAX IS RESET TO ',F7.2,' MINUTES AT XTIME = ',           IN4DGD.331
     1F7.2,' MINUTES XXXX')                                                      IN4DGD.332
      ENDIF                                                                      IN4DGD.333
      XTIM1(IN)=XT1SAV                                                           IN4DGD.334
      XTNEX(IN)=XTNSAV                                                           IN4DGD.335
      RETURN                                                                     IN4DGD.336
 7004 CONTINUE                                                                   IN4DGD.337
#ifndef MPP1                                                                     25JAN00.383
      READ (IUN2,END=7005)FLAG                                                   25JAN00.384
#else                                                                            25JAN00.385
      IF(DM_IONODE()) THEN                                                       25JAN00.386
        READ (IUN2,END=9005)FLAG                                                 25JAN00.387
        GOTO 8005                                                                25JAN00.388
 9005   CONTINUE                                                                 25JAN00.389
        FLAG = FILEDONE                                                          25JAN00.390
 8005   CONTINUE                                                                 25JAN00.391
      ENDIF                                                                      25JAN00.392
      CALL DM_BCAST_INTEGERS(FLAG,1)                                             IN4DGD.340
      IF(FLAG.EQ.FILEDONE) GOTO 7005                                             25JAN00.393
#endif                                                                           IN4DGD.341
      IF(FLAG.EQ.0)THEN                                                          IN4DGD.342
        IF(DM_IONODE())READ (IUN2)JBHI,JBHR,JBHIC,JBHRC                          IN4DGD.343
#ifdef MPP1                                                                      IN4DGD.344
        CALL DM_BCAST_INTEGERS(JBHI,NUMINT*NUMPROGS)                             IN4DGD.345
        CALL DM_BCAST_REALS(JBHR,NUMREAL*NUMPROGS)                               25JAN00.394
#endif                                                                           IN4DGD.347
        GOTO 7004                                                                IN4DGD.348
      ELSEIF(FLAG.EQ.1)THEN                                                      IN4DGD.349
        IF(DM_IONODE())THEN                                                      IN4DGD.350
          READ (IUN2)NDIM,START_INDEX,END_INDEX,RTIME,STAGGERING,                IN4DGD.351
     +               ORDERING,CURRENT_DATE,NAME,UNITS,DESCRIPTION                IN4DGD.352
        ENDIF                                                                    IN4DGD.353
#ifdef MPP1                                                                      IN4DGD.354
        CALL DM_BCAST_INTEGERS(NDIM,1)                                           IN4DGD.355
        CALL DM_BCAST_INTEGERS(START_INDEX,LEN_START_INDEX)                      IN4DGD.356
        CALL DM_BCAST_INTEGERS(END_INDEX,LEN_END_INDEX)                          IN4DGD.357
        CALL DM_BCAST_REALS(RTIME,1)                                             IN4DGD.358
        CALL DM_BCAST_STRING(STAGGERING,LEN_STAGGERING)                          IN4DGD.359
        CALL DM_BCAST_STRING(ORDERING,LEN_ORDERING)                              IN4DGD.360
        CALL DM_BCAST_STRING(CURRENT_DATE,LEN_CURRENT_DATE)                      IN4DGD.361
        CALL DM_BCAST_STRING(NAME,LEN_NAME)                                      IN4DGD.362
        CALL DM_BCAST_STRING(UNITS,LEN_UNITS)                                    IN4DGD.363
        CALL DM_BCAST_STRING(DESCRIPTION,LEN_DESCRIPTION)                        IN4DGD.364
#endif                                                                           IN4DGD.365
        CDATE2=CURRENT_DATE(1:19)                                                IN4DGD.366
        LENDIM1=(END_INDEX(1)-START_INDEX(1)+1)                                  IN4DGD.367
        LENDIM2=(END_INDEX(2)-START_INDEX(2)+1)                                  IN4DGD.368
        LENDIM3=(END_INDEX(3)-START_INDEX(3)+1)                                  IN4DGD.369
        IX=LENDIM1                                                               IN4DGD.370
        JX=LENDIM2                                                               IN4DGD.371
        IF(NDIM.GE.2)THEN                                                        IN4DGD.372
#ifndef MPP1                                                                     IN4DGD.373
          IF (LENDIM1*LENDIM2*LENDIM3.LE.MIX*MJX*(MKX+1)) THEN                   IN4DGD.374
            READ (IUN2)(HDOTH(I),I=1,LENDIM1*LENDIM2*LENDIM3)                    IN4DGD.375
          ENDIF                                                                  IN4DGD.376
#else                                                                            IN4DGD.377
          CALL DM_DIST_READ(IUN2,IN,HDOTH,NDIM,LENDIM1,LENDIM2,LENDIM3,          IN4DGD.378
     +         MIX,MJX,MKX+1)                                                    IN4DGD.379
#endif                                                                           IN4DGD.380
        ELSE                                                                     IN4DGD.381
          IF(DM_IONODE())READ (IUN2)                                             IN4DGD.382
        ENDIF                                                                    IN4DGD.383
        NAME8=NAME(1:8)                                                          IN4DGD.384
        IF(NAME8.EQ.'U       ')THEN                                              IN4DGD.385
          CALL EQUATE(HDOTH,IX,JX,MKX,UBOTEN,MIX,MJX,MKX)                        IN4DGD.386
        ELSEIF(NAME8.EQ.'V       ')THEN                                          IN4DGD.387
          CALL EQUATE(HDOTH,IX,JX,MKX,VBOTEN,MIX,MJX,MKX)                        IN4DGD.388
        ELSEIF(NAME8.EQ.'T       ')THEN                                          IN4DGD.389
          CALL EQUATE(HDOTH,IX,JX,MKX,TBOTEN,MIX,MJX,MKX)                        IN4DGD.390
        ELSEIF(NAME8.EQ.'Q       ')THEN                                          IN4DGD.391
          CALL EQUATE(HDOTH,IX,JX,MKX,QBOTEN,MIX,MJX,MKX)                        IN4DGD.392
        ELSEIF(NAME8.EQ.'PSTARCRS')THEN                                          IN4DGD.393
          CALL EQUATE(HDOTH,IX,JX,1,PSOTEN,MIX,MJX,1)                            IN4DGD.394
        ENDIF                                                                    IN4DGD.395
        GOTO 7004                                                                IN4DGD.396
      ELSEIF(FLAG.NE.2)THEN                                                      IN4DGD.397
            print *,'error in fdda read: unit=',iun2                             IN4DGD.398
#ifndef MPP1                                                                     25JAN00.395
        STOP 'in4dgd 5'                                                          IN4DGD.399
#else                                                                            25JAN00.396
        CALL FKILL_MODEL( 'in4dgd 5' )                                           25JAN00.397
#endif                                                                           25JAN00.398
      ENDIF                                                                      IN4DGD.400
      GOTO 7006                                                                  IN4DGD.401
 7005 PRINT7003,IUN2                                                             IN4DGD.402
      IF(KTAUB.EQ.0)THEN                                                         IN4DGD.403
        PRINT7019                                                                IN4DGD.404
#ifndef MPP1                                                                     25JAN00.399
        STOP 7005                                                                IN4DGD.405
#else                                                                            25JAN00.400
        CALL FKILL_MODEL( 'in4dgd 7005' )                                        25JAN00.401
#endif                                                                           25JAN00.402
      ENDIF                                                                      IN4DGD.406
C                                                                                IN4DGD.407
      IF(I4D(IN,2).EQ.1)THEN                                                     IN4DGD.408
        DO 7030 NVV=1,8                                                          IN4DGD.409
          TIME(NVV)=TIME(NVV)+15.                                                IN4DGD.410
 7030   CONTINUE                                                                 IN4DGD.411
      ENDIF                                                                      IN4DGD.412
C                                                                                IN4DGD.413
      PRINT7020                                                                  IN4DGD.414
      PRINT7021                                                                  IN4DGD.415
      IF(ABS(TIMAX-XTIME).GT.15.-DTMIN)THEN                                      IN4DGD.416
        TIMAX=XTIME+15.-DTMIN                                                    IN4DGD.417
        PRINT7022,TIMAX,XTIME                                                    IN4DGD.418
      ENDIF                                                                      IN4DGD.419
      XTIM1(IN)=XT1SAV                                                           IN4DGD.420
      XTNEX(IN)=XTNSAV                                                           IN4DGD.421
      RETURN                                                                     IN4DGD.422
 7006 CONTINUE                                                                   IN4DGD.423
      PRINT7001,CDATE2,IUN2,DIFTIM(IN,1)                                         IN4DGD.424
7001  FORMAT(1X,'****READ OBS ANALYSES FOR ',A19,' ON UNIT ',I2,                 IN4DGD.425
     1' AND DIFTIM = ',F7.1)                                                     IN4DGD.426
C***        CALL JULGMT(NDATE2,JULGMT2,TIMANL2,JULDAY,GMT,0)                     IN4DGD.427
      CALL GETH_IDTS(CDATE2,CDATE,IDTS)                                          IN4DGD.428
      TIMANL2=FLOAT(IDTS)/60.                                                    IN4DGD.429
      IF(ABS(XTIM2(IN)-TIMANL2).GT.1.1*DTMIN)THEN                                IN4DGD.430
        PRINT410,XTIME,DIFTIM(IN,1)                                              IN4DGD.431
410   FORMAT(1X,'---- STOPPING EXECUTION AT XTIME = ',F8.2,                      IN4DGD.432
     1': DIFTIM = ',F8.2,'MIN DOES NOT MATCH THE INPUT DATA  ----')              IN4DGD.433
#ifndef MPP1                                                                     25JAN00.403
        STOP 71                                                                  IN4DGD.434
#else                                                                            25JAN00.404
        CALL FKILL_MODEL( 'in4dgd 71' )                                          25JAN00.405
#endif                                                                           25JAN00.406
      ELSE                                                                       IN4DGD.435
        PRINT8001,CDATE2,XTIM2(IN)                                               IN4DGD.436
8001  FORMAT(1X,'----ARCHIVING OBS ANALYSES FOR ',A19,' AND TIMANL = ',          IN4DGD.437
     1F7.2,' MINUTES')                                                           IN4DGD.438
      ENDIF                                                                      IN4DGD.439
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC                                      IN4DGD.440
C GRID NUMBER IN                                                                 IN4DGD.441
C                                                                                IN4DGD.442
C                                                                                IN4DGD.443
C_FLIC_END_NOFLIC                                                                IN4DGD.444
#ifdef MPP1                                                                      IN4DGD.445
#  include <mpp_int4dgd_10.incl>                                                 IN4DGD.446
#endif                                                                           IN4DGD.447
      CALL DOTS(PSO,PSODOT,IL,JL,MIX,MJX)                                        IN4DGD.448
      RDXTIM1=1./(XTIM2(IN)-XTIM1(IN))                                           IN4DGD.449
      IF(IWIND(IN,1).NE.1)GOTO 5                                                 IN4DGD.450
      IF(IROT(IN).NE.1)PRINT1000,GV(IN,1)                                        IN4DGD.451
      IF(IROT(IN).EQ.1)PRINT1003,GR(IN)                                          IN4DGD.452
      IF((I4D(IN,2).EQ.1.AND.IWIND(IN,2).EQ.1).OR.INONBL(IN,1).EQ.1)             IN4DGD.453
     +  PRINT999                                                                 IN4DGD.454
      PRINT1001,XTIM1(IN),XTIM2(IN)                                              IN4DGD.455
      PRINT777,IUN1,IUN2,IN,XTNEX(IN)                                            IN4DGD.456
      DO 4 K=1,KL                                                                IN4DGD.457
        DO 4 J=1,JL                                                              IN4DGD.458
          DO 4 I=1,IL                                                            IN4DGD.459
            UBOTEN(I,J,K)=(UBOTEN(I,J,K)-UBO(I,J,K))*RDXTIM1                     IN4DGD.460
C        COUPLE AND CONVERT PS TO KPA                                            IN4DGD.461
            UBO(I,J,K)=UBO(I,J,K)*PSODOT(I,J)*.001                               IN4DGD.462
            UBOTEN(I,J,K)=UBOTEN(I,J,K)*PSODOT(I,J)*.001                         IN4DGD.463
    4     CONTINUE                                                               IN4DGD.464
                                                                                 IN4DGD.465
                                                                                 IN4DGD.466
    5 CONTINUE                                                                   IN4DGD.467
C READ VA                                                                        IN4DGD.468
      IF(IWIND(IN,1).NE.1)GOTO 15                                                IN4DGD.469
      DO 14 K=1,KL                                                               IN4DGD.470
        DO 14 J=1,JL                                                             IN4DGD.471
          DO 14 I=1,IL                                                           IN4DGD.472
            VBOTEN(I,J,K)=(VBOTEN(I,J,K)-VBO(I,J,K))*RDXTIM1                     IN4DGD.473
C        COUPLE AND CONVERT PS TO KPA                                            IN4DGD.474
            VBO(I,J,K)=VBO(I,J,K)*PSODOT(I,J)*.001                               IN4DGD.475
            VBOTEN(I,J,K)=VBOTEN(I,J,K)*PSODOT(I,J)*.001                         IN4DGD.476
   14     CONTINUE                                                               IN4DGD.477
                                                                                 IN4DGD.478
                                                                                 IN4DGD.479
   15 CONTINUE                                                                   IN4DGD.480
C                                                                                IN4DGD.481
      IF(IMOIS(IN,1).NE.1)GOTO 25                                                IN4DGD.482
      PRINT1005,GQ(IN,1)                                                         IN4DGD.483
      IF(IMOISP(IN).EQ.1)PRINT1006,RINP                                          IN4DGD.484
      IF((IMOIS(IN,2).EQ.1.OR.INONBL(IN,4).EQ.1))PRINT999                        IN4DGD.485
      PRINT1001,XTIM1(IN),XTIM2(IN)                                              IN4DGD.486
      PRINT777,IUN1,IUN2,IN,XTNEX(IN)                                            IN4DGD.487
      DO 24 K=1,KL                                                               IN4DGD.488
        DO 24 J=1,JLX                                                            IN4DGD.489
          DO 24 I=1,ILX                                                          IN4DGD.490
            QBOTEN(I,J,K)=(QBOTEN(I,J,K)-QBO(I,J,K))*RDXTIM1                     IN4DGD.491
C        COUPLE AND CONVERT PS TO KPA                                            IN4DGD.492
            QBO(I,J,K)=QBO(I,J,K)*PSO(I,J)*.001                                  IN4DGD.493
            QBOTEN(I,J,K)=QBOTEN(I,J,K)*PSO(I,J)*.001                            IN4DGD.494
   24     CONTINUE                                                               IN4DGD.495
                                                                                 IN4DGD.496
                                                                                 IN4DGD.497
   25 CONTINUE                                                                   IN4DGD.498
      IF(ITEMP(IN,1).NE.1)GOTO 35                                                IN4DGD.499
      PRINT1010,GT(IN,1)                                                         IN4DGD.500
      IF((ITEMP(IN,2).EQ.1.OR.INONBL(IN,3).EQ.1))PRINT999                        IN4DGD.501
      PRINT1001,XTIM1(IN),XTIM2(IN)                                              IN4DGD.502
      PRINT777,IUN1,IUN2,IN,XTNEX(IN)                                            IN4DGD.503
      DO 34 K=1,KL                                                               IN4DGD.504
        DO 34 J=1,JLX                                                            IN4DGD.505
          DO 34 I=1,ILX                                                          IN4DGD.506
            TBOTEN(I,J,K)=(TBOTEN(I,J,K)-TBO(I,J,K))*RDXTIM1                     IN4DGD.507
C        COUPLE AND CONVERT PS TO KPA                                            IN4DGD.508
            TBO(I,J,K)=TBO(I,J,K)*PSO(I,J)*.001                                  IN4DGD.509
            TBOTEN(I,J,K)=TBOTEN(I,J,K)*PSO(I,J)*.001                            IN4DGD.510
   34     CONTINUE                                                               IN4DGD.511
                                                                                 IN4DGD.512
                                                                                 IN4DGD.513
   35 CONTINUE                                                                   IN4DGD.514
      IF(IPSTR(IN,1).EQ.1)PRINT1015,GP(IN,1)                                     IN4DGD.515
      IF(IPSTR(IN,1).EQ.1)PRINT1001,XTIM1(IN),XTIM2(IN)                          IN4DGD.516
      IF(IPSTR(IN,1).NE.1)PRINT1016                                              IN4DGD.517
      IF(IPSTR(IN,1).EQ.1)PRINT777,IUN1,IUN2,IN,XTNEX(IN)                        IN4DGD.518
      DO 44 J=1,JLX                                                              IN4DGD.519
        DO 44 I=1,ILX                                                            IN4DGD.520
C         CONVERT PS TO KPA                                                      IN4DGD.521
          PSOTEN(I,J)=(PSOTEN(I,J)-PSO(I,J))*RDXTIM1*.001                        IN4DGD.522
          PSO(I,J)=PSO(I,J)*.001                                                 IN4DGD.523
   44   CONTINUE                                                                 IN4DGD.524
                                                                                 IN4DGD.525
   45 CONTINUE                                                                   IN4DGD.526
C                                                                                IN4DGD.527
C END OF INTERP READS FOR THE CGM                                                IN4DGD.528
C                                                                                IN4DGD.529
    1 CONTINUE                                                                   IN4DGD.530
C                                                                                IN4DGD.531
C                                                                                IN4DGD.532
C FOR INPUT DATA FOR BLNUDGD FOR CGM                                             IN4DGD.533
C                                                                                IN4DGD.534
C                                                                                IN4DGD.535
      IF(I4D(IN,2).NE.1)GOTO 5004                                                IN4DGD.536
C                                                                                IN4DGD.537
C READ IN OBS SURFACE ANALYSES...                                                IN4DGD.538
C                                                                                IN4DGD.539
C_FLIC_BEGIN_NOFLIC                                                              IN4DGD.540
C                                                                                IN4DGD.541
      IF(DM_IONODE())THEN                                                        IN4DGD.542
        INQUIRE(IUNS, OPENED=OPENED)                                             IN4DGD.543
        IF (.NOT. OPENED) THEN                                                   IN4DGD.544
          OPEN(IUNS, FILE='SFCFDDA_DOMAIN'//CHAR(IN+ICHAR('0')),                 24SEP99.389
     &         FORM='UNFORMATTED',STATUS='OLD',ERR=9030)                         25JAN00.407
        ENDIF                                                                    IN4DGD.547
      ENDIF                                                                      IN4DGD.548
      NT=0                                                                       IN4DGD.549
      TIMANL1=-999.                                                              IN4DGD.550
 2010 CONTINUE                                                                   IN4DGD.551
      ICHT=0                                                                     IN4DGD.552
#ifndef MPP1                                                                     25JAN00.408
      READ (IUNS,END=2499)FLAG                                                   25JAN00.409
#else                                                                            25JAN00.410
      IF(DM_IONODE())THEN                                                        25JAN00.411
        READ (IUNS,END=9499)FLAG                                                 25JAN00.412
        GOTO 8499                                                                25JAN00.413
 9499   CONTINUE                                                                 25JAN00.414
        FLAG = FILEDONE                                                          25JAN00.415
 8499   CONTINUE                                                                 25JAN00.416
      ENDIF                                                                      25JAN00.417
      CALL DM_BCAST_INTEGERS(FLAG,1)                                             IN4DGD.555
      IF(FLAG.EQ.FILEDONE) GOTO 2499                                             25JAN00.418
#endif                                                                           IN4DGD.556
      IF(FLAG.EQ.0)THEN                                                          IN4DGD.557
        IF (DM_IONODE()) THEN                                                    IN4DGD.558
          READ (IUNS)JBHI,JBHR,JBHIC,JBHRC                                       IN4DGD.559
        ENDIF                                                                    IN4DGD.560
#ifdef MPP1                                                                      IN4DGD.561
        CALL DM_BCAST_INTEGERS(JBHI,NUMINT*NUMPROGS)                             IN4DGD.562
        CALL DM_BCAST_REALS(JBHR,NUMREAL*NUMPROGS)                               25JAN00.419
#endif                                                                           IN4DGD.564
        GOTO 2010                                                                IN4DGD.565
      ELSEIF(FLAG.EQ.1)THEN                                                      IN4DGD.566
        IF ( DM_IONODE() ) THEN                                                  IN4DGD.567
          READ (IUNS)NDIM,START_INDEX,END_INDEX,RTIME,STAGGERING,                IN4DGD.568
     +               ORDERING,CURRENT_DATE,NAME,UNITS,DESCRIPTION                IN4DGD.569
        ENDIF                                                                    IN4DGD.570
#ifdef MPP1                                                                      IN4DGD.571
        CALL DM_BCAST_INTEGERS(NDIM,1)                                           IN4DGD.572
        CALL DM_BCAST_INTEGERS(START_INDEX,LEN_START_INDEX)                      IN4DGD.573
        CALL DM_BCAST_INTEGERS(END_INDEX,LEN_END_INDEX)                          IN4DGD.574
        CALL DM_BCAST_REALS(RTIME,1)                                             IN4DGD.575
        CALL DM_BCAST_STRING(STAGGERING,LEN_STAGGERING)                          IN4DGD.576
        CALL DM_BCAST_STRING(ORDERING,LEN_ORDERING)                              IN4DGD.577
        CALL DM_BCAST_STRING(CURRENT_DATE,LEN_CURRENT_DATE)                      IN4DGD.578
        CALL DM_BCAST_STRING(NAME,LEN_NAME)                                      IN4DGD.579
        CALL DM_BCAST_STRING(UNITS,LEN_UNITS)                                    IN4DGD.580
        CALL DM_BCAST_STRING(DESCRIPTION,LEN_DESCRIPTION)                        IN4DGD.581
#endif                                                                           IN4DGD.582
        CDATES=CURRENT_DATE(1:19)                                                IN4DGD.583
        CALL GETH_IDTS(CDATES,CDATE,IDTS)                                        IN4DGD.584
        TIMANLS=FLOAT(IDTS)/60.                                                  IN4DGD.585
        LENDIM1=(END_INDEX(1)-START_INDEX(1)+1)                                  IN4DGD.586
        LENDIM2=(END_INDEX(2)-START_INDEX(2)+1)                                  IN4DGD.587
        LENDIM3=(END_INDEX(3)-START_INDEX(3)+1)                                  IN4DGD.588
        IX=LENDIM1                                                               IN4DGD.589
        JX=LENDIM2                                                               IN4DGD.590
        IF(NDIM.GE.2)THEN                                                        IN4DGD.591
#ifndef MPP1                                                                     IN4DGD.592
          IF(LENDIM1*LENDIM2*LENDIM3.LE.MIX*MJX*(MKX+1))THEN                     IN4DGD.593
            READ (IUNS)(HDOTH(I),I=1,LENDIM1*LENDIM2*LENDIM3)                    IN4DGD.594
          ENDIF                                                                  IN4DGD.595
#else                                                                            IN4DGD.596
          CALL DM_DIST_READ(IUNS,IN,HDOTH,NDIM,LENDIM1,LENDIM2,LENDIM3,          IN4DGD.597
     +         MIX,MJX,MKX+1)                                                    IN4DGD.598
#endif                                                                           IN4DGD.599
        ELSE                                                                     IN4DGD.600
          IF(DM_IONODE())READ (IUNS)                                             IN4DGD.601
        ENDIF                                                                    IN4DGD.602
        NV=-99                                                                   IN4DGD.603
        NAME8=NAME(1:8)                                                          IN4DGD.604
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC            IN4DGD.605
C ARCHIVE THE SURFACE DATA:                                         C            IN4DGD.606
C        SFCOBS                            NV                       C            IN4DGD.607
C         USFC (M/S)                        1                       C            IN4DGD.608
C         VSFC (M/S)                        2                       C            IN4DGD.609
C         TSFC (K)                          3                       C            IN4DGD.610
C         QSFC (KG/KG)                      4                       C            IN4DGD.611
C         PSB (PSFC-PTOP) (CB)              5                       C            IN4DGD.612
C         REL HUMIDITY (PERCENT)            6                       C            IN4DGD.613
C         SEA LEVEL PRESSURE (CB)           7                       C            IN4DGD.614
C         BL SURFACE DATA DENSITY (REAL)    8                       C            IN4DGD.615
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC            IN4DGD.616
        IF(NAME8.EQ.'U       ')THEN                                              IN4DGD.617
          CALL EQUATE(HDOTH,IX,JX,1,DUM2D,MIX,MJX,1)                             IN4DGD.618
          NV=1                                                                   IN4DGD.619
          ICHK(NV)=1                                                             IN4DGD.620
        ELSEIF(NAME8.EQ.'V       ')THEN                                          IN4DGD.621
          CALL EQUATE(HDOTH,IX,JX,1,DUM2D,MIX,MJX,1)                             IN4DGD.622
          NV=2                                                                   IN4DGD.623
          ICHK(NV)=1                                                             IN4DGD.624
        ELSEIF(NAME8.EQ.'T       ')THEN                                          IN4DGD.625
          CALL EQUATE(HDOTH,IX,JX,1,DUM2D,MIX,MJX,1)                             IN4DGD.626
          NV=3                                                                   IN4DGD.627
          ICHK(NV)=1                                                             IN4DGD.628
        ELSEIF(NAME8.EQ.'Q       ')THEN                                          IN4DGD.629
          CALL EQUATE(HDOTH,IX,JX,1,DUM2D,MIX,MJX,1)                             IN4DGD.630
          NV=4                                                                   IN4DGD.631
          ICHK(NV)=1                                                             IN4DGD.632
        ELSEIF(NAME8.EQ.'PSTARCRS')THEN                                          IN4DGD.633
          CALL EQUATE(HDOTH,IX,JX,1,DUM2D,MIX,MJX,1)                             IN4DGD.634
C_FLIC_END_NOFLIC                                                                IN4DGD.635
C         CONVERT PA TO CB                                                       IN4DGD.636
          DO J=1,JX                                                              IN4DGD.637
            DO I=1,IX                                                            IN4DGD.638
              DUM2D(I,J)=DUM2D(I,J)*0.001                                        IN4DGD.639
            ENDDO                                                                IN4DGD.640
          ENDDO                                                                  IN4DGD.641
C_FLIC_BEGIN_NOFLIC                                                              IN4DGD.642
          NV=5                                                                   IN4DGD.643
          ICHK(NV)=1                                                             IN4DGD.644
        ELSEIF(NAME8.EQ.'RH      ')THEN                                          IN4DGD.645
          CALL EQUATE(HDOTH,IX,JX,1,DUM2D,MIX,MJX,1)                             IN4DGD.646
          NV=6                                                                   IN4DGD.647
          ICHK(NV)=1                                                             IN4DGD.648
        ELSEIF(NAME8.EQ.'PSEALVLC')THEN                                          IN4DGD.649
          CALL EQUATE(HDOTH,IX,JX,1,DUM2D,MIX,MJX,1)                             IN4DGD.650
C         CONVERT PA TO MB                                                       IN4DGD.651
C_FLIC_END_NOFLIC                                                                IN4DGD.652
          DO J=1,JX                                                              IN4DGD.653
            DO I=1,IX                                                            IN4DGD.654
              DUM2D(I,J)=DUM2D(I,J)*0.01                                         IN4DGD.655
            ENDDO                                                                IN4DGD.656
          ENDDO                                                                  IN4DGD.657
C_FLIC_BEGIN_NOFLIC                                                              IN4DGD.658
          NV=7                                                                   IN4DGD.659
          ICHK(NV)=1                                                             IN4DGD.660
        ELSEIF(NAME8.EQ.'TOBBOX  ')THEN                                          IN4DGD.661
          CALL EQUATE(HDOTH,IX,JX,1,DUM2D,MIX,MJX,1)                             IN4DGD.662
          NV=8                                                                   IN4DGD.663
          ICHK(NV)=1                                                             IN4DGD.664
C CONVERT SURFACE DATA DENSITY ARRAY READ FROM RAWINS INTO BLWXY                 IN4DGD.665
          CALL BLW(IL,JL,WXYTOP,DUM2D,BLDUM2D,RINBLW,DX,TIMANLS,IN)              07NOV00.1112
        ENDIF                                                                    IN4DGD.668
        IF((TIMANLS-XTIM2(IN)).GT.1.E-3)GOTO 2500                                25JAN00.420
        IF(NV.GT.0)THEN                                                          IN4DGD.670
          IQCHK(NT+1,NV)=ICHK(NV)                                                IN4DGD.671
          CALL CONV3(DUM2D,SFCOBS,MIXFG,MJXFG,TIMANLS,SFCTIM,NTIM,NVAR,          IN4DGD.672
     +         IL,JL,NT+1,NV)                                                    IN4DGD.673
        ENDIF                                                                    IN4DGD.674
        GOTO 2010                                                                IN4DGD.675
      ELSEIF(FLAG.NE.2)THEN                                                      IN4DGD.676
        print *,'error in fdda read: unit=',iuns                                 IN4DGD.677
#ifndef MPP1                                                                     25JAN00.421
        STOP 'in4dgd 6'                                                          IN4DGD.678
#else                                                                            25JAN00.422
        CALL FKILL_MODEL( 'in4dgd 6' )                                           25JAN00.423
#endif                                                                           25JAN00.424
      ENDIF                                                                      IN4DGD.679
      DO NV=1,8                                                                  IN4DGD.680
        ICHT=ICHT+ICHK(NV)                                                       IN4DGD.681
      ENDDO                                                                      IN4DGD.682
C     PRINT *,'CDATE,ICHECK=',CDATES,(ICHK(NV8),NV8=1,NUMLEVELS)                 IN4DGD.683
 2008 CONTINUE                                                                   IN4DGD.684
C                                                                                IN4DGD.685
C NV=8 IS THE BLWSDD (BOUNDARY LAYER WEIGHT SURFACE DATA DENSITY)                IN4DGD.686
C                                                                                IN4DGD.687
      ICHK(8)=1                                                                  IN4DGD.688
C***      CALL JULGMT(NDATES,JULGMTS,TIMANLS,JULDAY,GMT,0)                       IN4DGD.689
      ISAVE=0                                                                    IN4DGD.690
      PRINT2000,CDATES,IUNS                                                      IN4DGD.691
2000  FORMAT(1X,'****READ OBS SURFACE ANALYSES FOR ',A19,' ON UNIT ',            IN4DGD.692
     1I2)                                                                        IN4DGD.693
      IF(ABS(TIMANLS-XTIM1(IN)).LT.DTMIN)THEN                                    IN4DGD.694
        ISAVE=1                                                                  IN4DGD.695
        TIMANLS=XTIM1(IN)                                                        IN4DGD.696
      ENDIF                                                                      IN4DGD.697
      IF(ABS(TIMANLS-XTIM2(IN)).LT.DTMIN)THEN                                    IN4DGD.698
        ISAVE=1                                                                  IN4DGD.699
        TIMANLS=XTIM2(IN)                                                        IN4DGD.700
      ENDIF                                                                      IN4DGD.701
      IF(ISAVE.NE.1.AND.TIMANLS.LT.XTIM1(IN))THEN                                IN4DGD.702
        GOTO 2010                                                                IN4DGD.703
      ENDIF                                                                      IN4DGD.704
      IF(TIMANLS.GE.XTIM1(IN).AND.TIMANLS.LE.XTIM2(IN))ISAVE=1                   IN4DGD.705
      IF(ISAVE.NE.1.AND.TIMANLS.GT.XTIM2(IN))GOTO 2500                           IN4DGD.706
      NT=NT+1                                                                    IN4DGD.707
      ICHT=0                                                                     IN4DGD.708
      IF(NT.EQ.1)THEN                                                            IN4DGD.709
C NT=1 SHOULD BE A 12-HR TIME...                                                 IN4DGD.710
        IF(ABS(TIMANLS-XTIM1(IN)).GT.1.1*DTMIN) THEN                             25JAN00.425
#ifndef MPP1                                                                     25JAN00.426
          STOP 2020                                                              25JAN00.427
#else                                                                            25JAN00.428
          CALL FKILL_MODEL( 'IN4DGD 2020' )                                      25JAN00.429
#endif                                                                           25JAN00.430
        ENDIF                                                                    25JAN00.431
        TIMANL1=TIMANLS                                                          IN4DGD.712
      ELSE                                                                       IN4DGD.713
C CHECK SO THAT ARCHIVED SURFACE DATA SETS EXCEED DIFTIM(IN+2)...                IN4DGD.714
        DIFT=TIMANLS-TIMANL1                                                     IN4DGD.715
        IF(DIFT.LT.(DIFTIM(IN,2)-2.*DTMIN))THEN                                  IN4DGD.716
C IF DIFFERENCE IN DATA TIMES IS LESS THAN THE PRESCRIBED DIFTIM,                IN4DGD.717
C THEN DO NOT ARCHIVE THE DATA.  IN OTHER WORDS, THE ARCHIVED DATA               IN4DGD.718
C ANALYSES MUST EXCEED OR EQUAL DIFTIMS TO BE ARCHIVED.  IN THIS WAY,            IN4DGD.719
C ONE CAN EASILY PROCESS EVERY 6 H VS. EVERY 3 H BY SETTING DIFTIM               IN4DGD.720
C TO BE 360. (MINUTES)                                                           IN4DGD.721
          NT=NT-1                                                                IN4DGD.722
          GOTO 2010                                                              IN4DGD.723
        ELSE                                                                     IN4DGD.724
          TIMANL1=TIMANLS                                                        IN4DGD.725
        ENDIF                                                                    IN4DGD.726
      ENDIF                                                                      IN4DGD.727
C***      IF(INOPRO(IN,2).EQ.1)THEN                                              IN4DGD.728
C***        CALL NOPRO(NDATES,IDDATE,IDCHK,NCHA,NVAR,IDHK)                       IN4DGD.729
C***      ENDIF                                                                  IN4DGD.730
      PRINT2015,CDATES,TIMANLS                                                   IN4DGD.731
2015  FORMAT(1X,'----ARCHIVING OBS SURFACE ANALYSES FOR ',A19,' AND ',           IN4DGD.732
     1'TIMANL = ',F7.2,' MINUTES')                                               IN4DGD.733
      PRINT2016,(ICHK(NN),NN=1,7)                                                IN4DGD.734
2016  FORMAT(1X,'++++SURFACE DATA INVENTORY: U=',I2,' V=',I2,                    IN4DGD.735
     1' TEMP=',I2,' MIXR=',I2,' PSB=',I2,' RH=',I2,' SLP=',I2)                   IN4DGD.736
C IF U OR V IS FLAGGED, MAKE SURE BOTH ARE FLAGGED                               IN4DGD.737
C***      IF(INOPRO(IN,2).EQ.1)THEN                                              IN4DGD.738
C***        IF(IDHK(1).NE.1.OR.IDHK(2).NE.1.OR.IQCHK(NT,1).NE.1.OR.              IN4DGD.739
C***     +    IQCHK(NT,2).NE.1)THEN                                              IN4DGD.740
C***          IDHK(1)=0                                                          IN4DGD.741
C***          IDHK(2)=0                                                          IN4DGD.742
C***        ENDIF                                                                IN4DGD.743
C***        DO 2018 NV=1,7                                                       IN4DGD.744
C***          IF(IDHK(NV).NE.1)THEN                                              IN4DGD.745
C***            PRINT2019,CDATES,TIMANLS,NV                                      IN4DGD.746
C***2019  FORMAT(1X,'----AT ',A19,' AND TIMANL = ',F8.2,': NOT USING',           IN4DGD.747
C***     1' IVAR = ',I2,' BY INOPRO OVERRIDE')                                   IN4DGD.748
C***            IQCHK(NT,NV)=0                                                   IN4DGD.749
C***          ENDIF                                                              IN4DGD.750
C*** 2018   CONTINUE                                                             IN4DGD.751
C***      ENDIF                                                                  IN4DGD.752
      IF(IQCHK(NT,1).EQ.0.AND.ICHT.EQ.1)THEN                                     IN4DGD.753
        PRINT2117,CDATES,TIMANLS                                                 IN4DGD.754
2117  FORMAT(1X,'!!!!ARCHIVED OBSERVED SURFACE ANALYSES FOR ',A19,               IN4DGD.755
     1' AND TIMANL = ',F7.2,' ARE FLAGGED FOR QUESTIONABLE QUALITY')             IN4DGD.756
        PRINT2118                                                                IN4DGD.757
2118  FORMAT(5X,'...AND WILL NOT BE USED FOR FDDA')                              IN4DGD.758
      ENDIF                                                                      IN4DGD.759
      GOTO 2010                                                                  IN4DGD.760
 2499 CONTINUE                                                                   IN4DGD.761
      IF(ICHT.EQ.0)PRINT2017,CDATE1,CDATE2,XTIM1(1),XTIM2(1)                     IN4DGD.762
2017  FORMAT(1X,'!!! NO MORE SURFACE DATA AVAILABLE FROM ',A19,' TO ',           IN4DGD.763
     1   A19,' FOR TIMANL = ',F7.2,' TO ',F7.2)                                  IN4DGD.764
 2500 CONTINUE                                                                   IN4DGD.765
      IF (DM_IONODE()) REWIND (IUNS)                                             IN4DGD.766
      DO 2501 NV=1,8                                                             IN4DGD.767
        NTE(NV)=1                                                                IN4DGD.768
 2501 CONTINUE                                                                   IN4DGD.769
C IF NO SURFACE DATA WAS FOUND, SET I4D(IN+2)=0 OR                               IN4DGD.770
C STOP SINCE SOMETHING HAS GONE WRONG...                                         IN4DGD.771
C (I ASSUME THE USER WANTS A STOP IF THIS OCCURS)                                IN4DGD.772
      IF(NT.EQ.0)THEN                                                            IN4DGD.773
        I4D(IN,2)=0                                                              IN4DGD.774
#ifndef MPP1                                                                     25JAN00.432
        STOP 2501                                                                IN4DGD.775
#else                                                                            25JAN00.433
        CALL FKILL_MODEL( 'IN4DGD 2501' )                                        25JAN00.434
#endif                                                                           25JAN00.435
      ELSE                                                                       IN4DGD.776
        I4D(IN,2)=1                                                              IN4DGD.777
      ENDIF                                                                      IN4DGD.778
      IF(I4D(IN,2).NE.1)GOTO 5004                                                IN4DGD.779
      PRINT2100,IN                                                               IN4DGD.780
2100  FORMAT(1H0,'*** NUDGING THE FOLLOWING SURFACE ',                           IN4DGD.781
     1'VARIABLES IN THE BOUNDARY LAYER FOR INEST =',I2,': ***')                  IN4DGD.782
      IF(IWIND(IN,2).EQ.1.AND.IWIND(IN,1).EQ.1)THEN                              IN4DGD.783
        PRINT2101,GV(IN,2)                                                       IN4DGD.784
        IF(IBLNOP(IN,1).EQ.0)THEN                                                IN4DGD.785
          PRINT2201                                                              IN4DGD.786
        ELSE                                                                     IN4DGD.787
          PRINT2202                                                              IN4DGD.788
        ENDIF                                                                    IN4DGD.789
        IF(IWINDS(IN,2).EQ.1.AND.IWIND(IN,1).EQ.1.AND.IWIND(IN,2).EQ.1)          IN4DGD.790
     +    PRINT2203                                                              IN4DGD.791
      ENDIF                                                                      IN4DGD.792
      IF(ITEMP(IN,2).EQ.1.AND.ITEMP(IN,1).EQ.1)THEN                              IN4DGD.793
        PRINT2102,GT(IN,2)                                                       IN4DGD.794
        IF(IBLNOP(IN,3).EQ.0)THEN                                                IN4DGD.795
          PRINT2201                                                              IN4DGD.796
        ELSE                                                                     IN4DGD.797
          PRINT2202                                                              IN4DGD.798
        ENDIF                                                                    IN4DGD.799
      ENDIF                                                                      IN4DGD.800
      IF(IMOIS(IN,2).EQ.1.AND.IMOIS(IN,1).EQ.1)THEN                              IN4DGD.801
        PRINT2103,GQ(IN,2)                                                       IN4DGD.802
        IF(IBLNOP(IN,4).EQ.0)THEN                                                IN4DGD.803
          PRINT2201                                                              IN4DGD.804
        ELSE                                                                     IN4DGD.805
          PRINT2202                                                              IN4DGD.806
        ENDIF                                                                    IN4DGD.807
      ENDIF                                                                      IN4DGD.808
      IF(IPSTR(IN,2).EQ.1.AND.IPSTR(IN,1).EQ.1)PRINT2104,GP(IN,2)                IN4DGD.809
2101  FORMAT(5X,'SURFACE WIND WITH GV = ',E11.3)                                 IN4DGD.810
2102  FORMAT(5X,'SURFACE TEMPERATURE WITH GT = ',E11.3)                          IN4DGD.811
2103  FORMAT(5X,'SURFACE MOISTURE WITH GQ = ',E11.3)                             IN4DGD.812
2104  FORMAT(5X,'SURFACE PRESSURE WITH GP = ',E11.3)                             IN4DGD.813
2201  FORMAT(8X,'BASED ON CORRECTION IN THE SURFACE LAYER')                      IN4DGD.814
2202  FORMAT(8X,'BASED ON LOCAL CORRECTION AT EACH LAYER')                       IN4DGD.815
2203  FORMAT(8X,'WITH SIMILARITY ADJUSTMENT BASED ON ROUGHNESS')                 IN4DGD.816
      PRINT2105,DIFTIM(IN,2)                                                     IN4DGD.817
2105  FORMAT(1X,'THESE SURFACE DATA ARE INTERPOLATED IN TIME WITH',              IN4DGD.818
     1' A MINIMUM DATA FREQUENCY OF ',F7.2,' MINUTES')                           IN4DGD.819
C***      IF(INOPRO(IN,2).EQ.1)THEN                                              IN4DGD.820
C***        PRINT2106                                                            IN4DGD.821
C***2106  FORMAT(1X,'NOT PROCESSING SURFACE DATA AT THE FOLLOWING TIMES:         IN4DGD.822
C***        DO 2110 NOC=1,NCHA                                                   IN4DGD.823
C***          IF(IDDATE(NOC).EQ.-999)GOTO 2110                                   IN4DGD.824
C***          PRINT2107,IDDATE(NOC),(IDCHK(NOC,NN),NN=1,8)                       IN4DGD.825
C***2107  FORMAT(5X,'IDDATE: ',I8,' IDCHK:  U=',I2,' V=',I2,' TEMP=',I2,         IN4DGD.826
C***     1' MIXR=',I2,' PSB=',I2,' RH=',I2,' SLP=',I2)                           IN4DGD.827
C*** 2110   CONTINUE                                                             IN4DGD.828
C***      ENDIF                                                                  IN4DGD.829
 5004 CONTINUE                                                                   IN4DGD.830
      IF(INDT(IN,1).EQ.1)PRINT5001                                               IN4DGD.831
      IF(INDXY(IN,1).NE.0)PRINT5003                                              IN4DGD.832
      IF(INDZ(IN,1).EQ.1)PRINT5002                                               IN4DGD.833
5001  FORMAT(1H0,'THE GRID NUDGING COEFFICIENT VARIES IN TIME -- SEE',           IN4DGD.834
     1' SUBROUTINE SETUPGD')                                                     IN4DGD.835
5002  FORMAT(1H0,'THE GRID NUDGING COEFFICIENT VARIES IN THE VERTICAL',          IN4DGD.836
     1' -- SEE SUBROUTINE SETUPGD')                                              IN4DGD.837
5003  FORMAT(1H0,'THE GRID NUDGING COEFFICIENT VARIES IN THE ',                  IN4DGD.838
     1'HORIZONTAL -- SEE SUBROUTINE SETUPGD')                                    IN4DGD.839
      IF(I4D(IN,2).NE.1)GOTO 5005                                                IN4DGD.840
      IF(INDT(IN,2).EQ.1)PRINT5101                                               IN4DGD.841
      IF(INDXY(IN,2).NE.0)PRINT5103                                              IN4DGD.842
      IF(INDXY(IN,2).EQ.1)PRINT5105                                              IN4DGD.843
      IF(INDXY(IN,2).EQ.2)PRINT5104,RINBLW                                       IN4DGD.844
      IF(INDXY(IN,2).EQ.2)PRINT5106                                              IN4DGD.845
      IF(INDZ(IN,2).EQ.1)PRINT5102                                               IN4DGD.846
5101  FORMAT(1H0,'THE BL GRID NUDGING COEFFICIENT VARIES IN TIME')               IN4DGD.847
5102  FORMAT(1H0,'THE BL GRID NUDGING COEFFICIENT VARIES IN THE ',               IN4DGD.848
     1'VERTICAL -- SEE SUBROUTINE SETUPGD')                                      IN4DGD.849
5103  FORMAT(1H0,'THE BL GRID NUDGING COEFFICIENT VARIES IN THE ',               IN4DGD.850
     1'HORIZONTAL')                                                              IN4DGD.851
5104  FORMAT(1X,'WITH A WEIGHTING FUNCTION BASED ON SURFACE DATA',               IN4DGD.852
     1' DENSITY WITH RINBLW = ',F6.1,' KM')                                      IN4DGD.853
5105  FORMAT(1X,' --SEE SUBROUTINE SETUPGD')                                     IN4DGD.854
5106  FORMAT(1X,' --SEE SUBROUTINE BLW')                                         IN4DGD.855
 5005 CONTINUE                                                                   IN4DGD.856
C                                                                                IN4DGD.857
C     ... CGM FORMATS FROM READS                                                 IN4DGD.858
C                                                                                IN4DGD.859
1000  FORMAT(1H0,'IN4DGD: THE WINDS ARE BEING GRID NUDGED WITH',                 IN4DGD.860
     1 ' GV = ',E11.3)                                                           IN4DGD.861
1003  FORMAT(1H0,'IN4DGD: THE ROTATIONAL WIND IS BEING GRID NUDGED',             IN4DGD.862
     1' WITH GR = ',E11.3)                                                       IN4DGD.863
999   FORMAT(1X,'ABOVE THE BOUNDARY LAYER')                                      IN4DGD.864
1001  FORMAT(1X,'USING LINEAR INTERP OF GRID ANALYSIS FROM XTIME = ',            IN4DGD.865
     1F6.1,' TO ',F6.1)                                                          IN4DGD.866
1002  FORMAT(1X,'USING GRID ANALYSIS AT TARGET TIME = ',F6.1)                    IN4DGD.867
777   FORMAT(1X,'READ FROM IUN1 = ',I3,' AND IUN2 = ',I3,                        IN4DGD.868
     1' ; AND XTNEX(',I1,') = ',F6.1)                                            IN4DGD.869
1005  FORMAT(1H0,'IN4DGD: MOISTURE IS BEING GRID NUDGED WITH',                   IN4DGD.870
     1' GQ = ',E11.3)                                                            IN4DGD.871
1006  FORMAT('            AND USING PRECIP',                                     IN4DGD.872
     1' DATA WITH RINP = ',F7.1,' KM')                                           IN4DGD.873
1010  FORMAT(1H0,'IN4DGD: TEMPERATURE IS BEING GRID NUDGED',                     IN4DGD.874
     1' WITH GT = ',E11.3)                                                       IN4DGD.875
1015  FORMAT(1H ,'IN4DGD: PSTAR IS BEING GRID NUDGED IN THE ',                   IN4DGD.876
     1 'CONT EQ WITH GP = ',E11.3)                                               IN4DGD.877
1016  FORMAT(1X,'PSTAR IS NOT BEING GRID NUDGED IN THE CONT. EQ., AND',          IN4DGD.878
     1' THE GRID NUDGING TERM FOR U,V,T, OR Q HAS ONLY THE ONE TERM')            IN4DGD.879
C                                                                                IN4DGD.880
C_FLIC_END_NOFLIC                                                                IN4DGD.881
      RETURN                                                                     IN4DGD.882
 9010 PRINT*,'ERROR OPENING FDDAGD INPUT IUN1=',IUN1,' IN=',IN                   25JAN00.436
      STOP 'STOP 9010 IN IN4DGD'                                                 25JAN00.437
 9020 PRINT*,'ERROR OPENING FDDAGD INPUT IUN2=',IUN2,' IN=',IN                   25JAN00.438
      STOP 'STOP 9020 IN IN4DGD'                                                 25JAN00.439
 9030 PRINT*,'ERROR OPENING FDDAGD INPUT IUNS=',IUNS,' IN=',IN                   25JAN00.440
      STOP 'STOP 9030 IN IN4DGD'                                                 25JAN00.441
      END                                                                        IN4DGD.883
                                                                                 IN4DGD.884
