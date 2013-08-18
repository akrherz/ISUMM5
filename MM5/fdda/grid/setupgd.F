C                                                                                SETUPGD.1
      SUBROUTINE SETUPGD(PSB,PSBDT,IN)                                           SETUPGD.2
C THIS SUBROUTINE DEFINES THE TIME AND VERTICAL SPACE FACTORS                    SETUPGD.3
C (TFAC AND ZFAC) AND CALCULATES P* ON DOT POINTS IF IWIND(IN)                   SETUPGD.4
C IS 1.  THIS SUBROUTINE IS CALLED ONLY ONCE EACH TIMESTEP,                      SETUPGD.5
C AND THUS IS CALLED BEFORE THE "J LOOP" IN SOLVE.                               SETUPGD.6
C THIS SUBROUTINE ALSO COMPUTES THE WEIGHTING FACTOR FOR MOISTURE                SETUPGD.7
C NUDGING, WQ, BASED ON OBS PRECIP READ IN FROM UNIT 40, THE OUTPUT              SETUPGD.8
C OF SAMHOR.                                                                     SETUPGD.9
C NOTE: THE HORIZONTAL AND VERTICAL WEIGHTS SPECIFIED HERE ARE CONSTANT-         SETUPGD.10
C       IN TIME THROUGH THE MODEL INTEGRATION.  THEREFORE, WHEN                  SETUPGD.11
C       INDXY(IN)=1 OR INDZ(IN)=1, WXY, BLWXY, ZFAC, ETC. ARE DEFINED            SETUPGD.12
C       ONLY ONCE WHEN KTAU=0 OR KTAU=KTAUR.                                     SETUPGD.13
#     include <parame.incl>                                                      SETUPGD.14
#     include <param2.incl>                                                      SETUPGD.15
#     include <parfdda.incl>                                                     SETUPGD.16
#     include <param3.incl>                                                      SETUPGD.17
#     include <various.incl>                                                     SETUPGD.18
#     include <fddagd.incl>                                                      SETUPGD.19
#ifdef MPP1                                                                      SETUPGD.20
#  include <mpp_setupgd_00.incl>                                                 SETUPGD.21
#endif                                                                           SETUPGD.22
      DIMENSION PSBDT(MIX,MJX),PSB(MIX,MJX)                                      SETUPGD.23
      REAL JULGMTP,JULGMTB                                                       SETUPGD.24
C_FLIC_RUNPAD(8)                                                                 07NOV00.1113
#ifdef MPP1                                                                      SETUPGD.26
#  include <mpp_setupgd_10.incl>                                                 SETUPGD.27
#endif                                                                           SETUPGD.28
      IF(MOD(KTAU,NPF).EQ.0)PRINT333,IN,KTAU,XTIME                               SETUPGD.29
333   FORMAT(1X,'IN SETUPGD: IN,KTAU,XTIME',I2,1X,I4,1X,F6.1)                    SETUPGD.30
      IF(IWIND(IN,1).EQ.1)CALL DOTS(PSB,PSBDT,IL,JL,MIX,MJX)                     SETUPGD.31
      IF(INDT(IN,1).NE.1)GOTO 50                                                 SETUPGD.32
C DEFINE THE TIME FUNCTION FOR GRID FDDA OF 3-D DATA FOR EACH MESH               SETUPGD.33
C                                                                                SETUPGD.34
C DTRAMP IS THE TIME PERIOD IN MINUTES OVER WHICH THE NUDGING IS                 SETUPGD.35
C RAMPED DOWN FROM ONE TO ZERO                                                   SETUPGD.36
      IF(IDYNIN.EQ.1)THEN                                                        SETUPGD.37
        DTR=ABS(DTRAMP)                                                          SETUPGD.38
        TCONST=1./DTR                                                            SETUPGD.39
C FDAEND(IN) IS THE TIME IN MINUTES TO END THE DYNAMIC INITIALIZATION CY         SETUPGD.40
        IF(XTIME.LT.FDAEND(IN)-DTR)THEN                                          SETUPGD.41
          TFAC(IN,1)=1.                                                          SETUPGD.42
        ELSEIF(XTIME.GE.FDAEND(IN)-DTR.AND.XTIME.LE.FDAEND(IN))THEN              SETUPGD.43
          TFAC(IN,1)=(FDAEND(IN)-XTIME)*TCONST                                   SETUPGD.44
        ELSE                                                                     SETUPGD.45
          TFAC(IN,1)=0.0                                                         SETUPGD.46
        ENDIF                                                                    SETUPGD.47
        IF(KTAU.EQ.KTAUR.OR.MOD(KTAU,10).EQ.0)THEN                               SETUPGD.48
         PRINT*,' DYNIN3D: IN,KTAU,XTIME,FDAEND,DTRAMP,DTR,TCONST',              SETUPGD.49
     1   ',TFAC: ',IN,KTAU,XTIME,FDAEND(IN),DTRAMP,DTR,TCONST,                   SETUPGD.50
     2    TFAC(IN,1)                                                             SETUPGD.51
        ENDIF                                                                    SETUPGD.52
      ELSE                                                                       SETUPGD.53
      PRINT*,' YOU MUST DEFINE YOUR USER-DEFINED TIME FUNCTION ',                SETUPGD.54
     1'TFAC(IN,1)'                                                               SETUPGD.55
        STOP 50                                                                  SETUPGD.56
      ENDIF                                                                      SETUPGD.57
   50 CONTINUE                                                                   SETUPGD.58
      IF(I4D(IN,2).EQ.1.AND.INDT(IN,2).EQ.1)THEN                                 SETUPGD.59
C DEFINE TFAC FUNCTION HERE FOR BLNUDGD                                          SETUPGD.60
C DEFINE THE TIME FUNCTION FOR GRID FDDA OF SFC DATA FOR EACH MESH               SETUPGD.61
C                                                                                SETUPGD.62
C DTRAMP IS THE TIME PERIOD IN MINUTES OVER WHICH THE NUDGING IS                 SETUPGD.63
C RAMPED DOWN FROM ONE TO ZERO                                                   SETUPGD.64
        IF(IDYNIN.EQ.1)THEN                                                      SETUPGD.65
          DTR=ABS(DTRAMP)                                                        SETUPGD.66
          TCONST=1./DTR                                                          SETUPGD.67
C FDAEND(IN) IS THE TIME IN MINUTES TO END THE DYNAMIC INITIALIZATION CY         SETUPGD.68
          IF(XTIME.LT.FDAEND(IN)-DTR)THEN                                        SETUPGD.69
            TFAC(IN,2)=1.                                                        SETUPGD.70
          ELSEIF(XTIME.GE.FDAEND(IN)-DTR.AND.XTIME.LE.FDAEND(IN))THEN            SETUPGD.71
            TFAC(IN,2)=(FDAEND(IN)-XTIME)*TCONST                                 SETUPGD.72
          ELSE                                                                   SETUPGD.73
            TFAC(IN,2)=0.0                                                       SETUPGD.74
          ENDIF                                                                  SETUPGD.75
          IF(KTAU.EQ.KTAUR.OR.MOD(KTAU,10).EQ.0)THEN                             SETUPGD.76
         PRINT*,' DYNINSFC: IN,KTAU,XTIME,FDAEND,DTRAMP,DTR,TCONST',             SETUPGD.77
     1   ',TFAC: ',IN,KTAU,XTIME,FDAEND(IN),DTRAMP,DTR,TCONST,                   SETUPGD.78
     2    TFAC(IN,2)                                                             SETUPGD.79
          ENDIF                                                                  SETUPGD.80
        ELSE                                                                     SETUPGD.81
      PRINT*,' YOU MUST DEFINE YOUR USER-DEFINED TIME FUNCTION ',                SETUPGD.82
     1'TFAC(IN,2)'                                                               SETUPGD.83
          STOP 50                                                                SETUPGD.84
        ENDIF                                                                    SETUPGD.85
      ENDIF                                                                      SETUPGD.86
      IF(IMOISP(IN).EQ.0.OR.IMOIS(IN,1).EQ.0)GOTO 55                             SETUPGD.87
      STOP '55 imoisp=1 not supported in v3'                                     SETUPGD.88
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC              SETUPGD.89
C COMPUTE HORIZONTAL WEIGHT FOR MOISTURE NUDGING IF IMOISP(IN).NE.0              SETUPGD.90
C                                                                                SETUPGD.91
   51 CONTINUE                                                                   SETUPGD.92
      IF(KTAU.EQ.KTAUR.AND.IN.EQ.1)THEN                                          SETUPGD.93
C NOTE: ALL 2-D FIELDS MUST BE READ IN (X,Y) THEN CONVERTED TO (Y,X)             SETUPGD.94
#ifndef MPP1                                                                     SETUPGD.95
        READ (40)NPERDS,DMSCR                                                    SETUPGD.96
#else                                                                            SETUPGD.97
#  include <mpp_setupgd_20.incl>                                                 SETUPGD.98
#endif                                                                           SETUPGD.99
        JULGMTB=FLOAT(JULDAY)*100.+GMT                                           SETUPGD.100
        PRINT101,NPERDS,JULGMTB                                                  SETUPGD.101
101   FORMAT(1X,'+++READING CM FDDAMSG PRECIP DATA FOR ',I3,' HOURS',            SETUPGD.102
     1' TO BEGIN AT JULGMTP = ',F9.2,'+++')                                      SETUPGD.103
#ifndef MPP1                                                                     SETUPGD.104
        READ (40)NP,JULGMTP,JYEAR,JMONTH,JDAY,JHOUR,DMSCR,IPSCR                  SETUPGD.105
#else                                                                            SETUPGD.106
#  include <mpp_setupgd_30.incl>                                                 SETUPGD.107
#endif                                                                           SETUPGD.108
C CONVERT JULGMTP TO EQUIVALENT MODEL-RELATIVE TIME IN MINUTES...                SETUPGD.109
        CALL JULGMT(MDATE,JULGMTP,TIMOBP(1),JULDAY,GMT,2)                        SETUPGD.110
C     TIMOBP(1)=FLOAT(NP)*60.                                                    SETUPGD.111
        PRINT102,NP,JULGMTP,JYEAR,JMONTH,JDAY,JHOUR,DMSCR(31,24),                SETUPGD.112
     +           IPSCR(31,24),XTIME                                              SETUPGD.113
102   FORMAT(1X,'+++READING CM NP,JULGMTP,YEAR,MONTH,DAY,HOUR:',1X,I2,           SETUPGD.114
     1F9.2,4(1X,I2),' AND DMI,IPRE ',F4.1,1X,I5,' AT XTIME = ',F6.1)             SETUPGD.115
        PRINT104,TIMOBP(1)                                                       SETUPGD.116
104   FORMAT(4X,'FOR HOURLY PRECIP ENDING AT TIMOBP = ',F6.1,'+++')              SETUPGD.117
        IF(ABS(JULGMTB-JULGMTP).GT.1.1*DTMIN/60.)THEN                            SETUPGD.118
          PRINT103,JULGMTB,JULGMTP                                               SETUPGD.119
103   FORMAT('   STOPPING EXECUTION: JULGMTB (',F9.2,') DOES NOT MATCH',         SETUPGD.120
     1' JULGMTP (',F9.2,') FROM THE CM DATA VOLUME')                             SETUPGD.121
          STOP 0                                                                 SETUPGD.122
        ENDIF                                                                    SETUPGD.123
      ENDIF                                                                      SETUPGD.124
C FOR KTAU.GT.0.OR.KTAUR...                                                      SETUPGD.125
C C HOURLY PRECIP REPRESENTS THE HOUR ENDING AT JULGMTP.                         SETUPGD.126
      IF(ABS(TIMOBP(1)-XTIME).LT.DTMIN.AND.IN.EQ.1)THEN                          SETUPGD.127
        READ (40)NP,JULGMTP,JYEAR,JMONTH,JDAY,JHOUR,DMSCR,IPSCR                  SETUPGD.128
C     CALL MAPSMP(DMSCR,JXX,IYY,1,JLX,1,1,ILX,1,0.,1,8HDMSCR B3,XTIME)           SETUPGD.129
        PRINT102,NP,JULGMTP,JYEAR,JMONTH,JDAY,JHOUR,DMSCR(31,24),                SETUPGD.130
     +           IPSCR(31,24),XTIME                                              SETUPGD.131
C CONVERT JULGMTP TO EQUIVALENT MODEL-RELATIVE TIME IN MINUTES...                SETUPGD.132
        CALL JULGMT(MDATE,JULGMTP,TIMOBP(1),JULDAY,GMT,2)                        SETUPGD.133
        TIMCHK=FLOAT(NP)*60.                                                     SETUPGD.134
        IF(ABS(TIMCHK-TIMOBP(1)).GT.1.1*DTMIN)STOP 104                           SETUPGD.135
        PRINT104,TIMOBP(1)                                                       SETUPGD.136
C CONVERT 2-D FIELDS OF PRECIP (INTEGER IN CM X 10-3) AND MISSING DATA           SETUPGD.137
C ARRAY (1.0 IF MISSING. 0.0 IF NOT) FROM (X,Y) TO (Y,X)                         SETUPGD.138
C     CALL MAPSMP(DMSCR,JXX,IYY,1,JLX,1,1,ILX,1,0.,1,8HDMSCR B4,XTIME)           SETUPGD.139
C     PRINT 199,DMSCR(1,1),DMSCR(1,19),DMSCR(1,30),DMSCR(1,38),                  SETUPGD.140
C    1DMSCR(26,26)                                                               SETUPGD.141
C199  FORMAT(1X,'DMSCR AT Y=1,19,30,38 AND 26,26',5(1X,F15.13))                  SETUPGD.142
#ifndef MPP1                                                                     SETUPGD.143
        DO 200 I=1,IL                                                            SETUPGD.144
          DO 200 J=1,JL                                                          SETUPGD.145
            DMI(I,J)=DMSCR(J,I)                                                  SETUPGD.146
            IF(DMI(I,J).GT.0.5)THEN                                              SETUPGD.147
              DMI(I,J)=1.0                                                       SETUPGD.148
            ELSE                                                                 SETUPGD.149
              DMI(I,J)=0.0                                                       SETUPGD.150
            ENDIF                                                                SETUPGD.151
            IPRE(I,J)=IPSCR(J,I)                                                 SETUPGD.152
  200     CONTINUE                                                               SETUPGD.153
                                                                                 SETUPGD.154
C     CALL MAPSMP(DMI,IYY,JXX,1,ILX,1,1,JLX,1,0.,1,8HDMIAFTER,XTIME)             SETUPGD.155
C SET WEIGHTS TO 1.0 OVER ENTIRE GRID                                            SETUPGD.156
        IF(IN.EQ.1)CALL UNITY(WQ,IL,JL)                                          SETUPGD.157
C SET IGRID TO 0 FOR DOT POINT FIELD AND 1 FOR CROSS                             SETUPGD.158
        IGRID=0                                                                  SETUPGD.159
        RINDX=RINP*1000./DX                                                      SETUPGD.160
        DO 210 I=1,IL                                                            SETUPGD.161
          DO 210 J=1,JL                                                          SETUPGD.162
            IF((IPRE(I,J).NE.0.AND.DMI(I,J).LT.0.5).OR.DMI(I,J).GT.0.5)          SETUPGD.163
     +        THEN                                                               SETUPGD.164
              MAXJ=J+IFIX(RINDX+0.99)                                            SETUPGD.165
              MAXJ=MIN0(JL-IGRID,MAXJ)                                           SETUPGD.166
              MINJ=J-IFIX(RINDX+0.99)                                            SETUPGD.167
              MINJ=MAX0(1,MINJ)                                                  SETUPGD.168
              MAXI=I+IFIX(RINDX+0.99)                                            SETUPGD.169
              MAXI=MIN0(IL-IGRID,MAXI)                                           SETUPGD.170
              MINI=I-IFIX(RINDX+0.99)                                            SETUPGD.171
              MINI=MAX0(1,MINI)                                                  SETUPGD.172
              DO 220 II=MINI,MAXI                                                SETUPGD.173
                DO 220 JJ=MINJ,MAXJ                                              SETUPGD.174
                  WQ(II,JJ)=0.0                                                  SETUPGD.175
  220           CONTINUE                                                         SETUPGD.176
                                                                                 SETUPGD.177
            ENDIF                                                                SETUPGD.178
  210     CONTINUE                                                               SETUPGD.179
                                                                                 SETUPGD.180
#else                                                                            SETUPGD.181
#  include <mpp_setupgd_40.incl>                                                 SETUPGD.182
#endif                                                                           SETUPGD.183
                                                                                 SETUPGD.184
      ENDIF                                                                      SETUPGD.185
C                                                                                SETUPGD.186
C IF(IMOISP(INEST)=1), CODE MUST BE ADDED HERE TO CALCULATE WQN...               SETUPGD.187
      IF(IN.EQ.2)STOP 55                                                         SETUPGD.188
C                                                                                SETUPGD.189
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC           SETUPGD.190
   55 CONTINUE                                                                   SETUPGD.191
C THIS CODE DEFINES THE ZFAC AND HORIZONTAL WEIGHTING ARRAYS                     SETUPGD.192
C ASSUMED CONSTANT IN TIME                                                       SETUPGD.193
C IF I4D(IN+2)=1, THEN THERE IS PARTITIONING IN THE HORIZONTAL                   SETUPGD.194
C AND VERTICAL BETWEEN NUDGING SURFACE DATA IN THE BOUNDARY                      SETUPGD.195
C LAYER AND OVER THE CONTINENT, AND NUDGING 3-D 12-HLY ANALYSES.                 SETUPGD.196
C THE VERTICAL WEIGHTING DEPENDS ON THE PBL HEIGHT, AND THE                      SETUPGD.197
C VERTICAL FACTOR ZFAC(IN+2) OPTIONALLY DEFINED HERE WHEN INDZ(IN+2)=1,          SETUPGD.198
C IS CONSTANT IN TIME, AS ARE THE HORIZONTAL WEIGHTING FUNCTIONS                 SETUPGD.199
C SPECIFIED IN THIS SUBROUTINE.  IF INDXY(IN+2)=2, THEN THE HORIZONTAL           SETUPGD.200
C WEIGHTING FUNCTION FOR BLNUDGD IS BASED ON THE SURFACE DATA DISTRIB-           SETUPGD.201
C UTION (READ IN FROM THE SURFACE DATA RAWINS FILE IN SUBROUTINE                 SETUPGD.202
C IN4DGD) AND CALCULATED IN SUBROUTINE BLW BASED ON RINBLW.                      SETUPGD.203
C                                                                                SETUPGD.204
C NOTE: FOR OBS NUDGING, THE TEMPORAL AND SPATIAL WEIGHTS ARE COMPUTED           SETUPGD.205
C       DIRECTLY IN SUBROUTINE NUDOB BASED ON RINXY, RINSIG, AND TWINDO          SETUPGD.206
      IF(KTAU.GT.KTAUR)RETURN                                                    SETUPGD.207
      IF(INDZ(IN,1).EQ.1)THEN                                                    SETUPGD.208
C DEFINE ZFAC(IN,K) HERE...                                                      SETUPGD.209
        STOP 70                                                                  SETUPGD.210
      ELSE                                                                       SETUPGD.211
        DO 5703 L=1,KL                                                           SETUPGD.212
          ZFAC(IN,1,L)=1.0                                                       SETUPGD.213
 5703   CONTINUE                                                                 SETUPGD.214
      ENDIF                                                                      SETUPGD.215
   70 CONTINUE                                                                   SETUPGD.216
C DETERMINE HORIZONTAL WEIGHTING ARRAYS                                          SETUPGD.217
      IF(I4D(IN,2).NE.1)GOTO 58                                                  SETUPGD.218
      IF(INDT(IN,2).NE.1)THEN                                                    SETUPGD.219
        TFAC(IN,2)=1.0                                                           SETUPGD.220
      ENDIF                                                                      SETUPGD.221
      IF(INDZ(IN,2).EQ.1)THEN                                                    SETUPGD.222
C DEFINE ZFAC(IN,2,K) HERE...                                                    SETUPGD.223
        STOP 61                                                                  SETUPGD.224
      ELSE                                                                       SETUPGD.225
        DO 61 L=1,KL                                                             SETUPGD.226
          ZFAC(IN,2,L)=1.0                                                       SETUPGD.227
   61   CONTINUE                                                                 SETUPGD.228
      ENDIF                                                                      SETUPGD.229
      IF(INDXY(IN,2).EQ.1)THEN                                                   SETUPGD.230
C DETERMINE HORIZONTAL WEIGHTS FOR BLNUDGD...                                    SETUPGD.231
        STOP 62                                                                  SETUPGD.232
      ELSE                                                                       SETUPGD.233
#ifndef MPP1                                                                     SETUPGD.234
        CALL UNITY(BLWDT,IL,JL)                                                  SETUPGD.235
        CALL UNITY(BLWCS,IL,JL)                                                  SETUPGD.236
#else                                                                            SETUPGD.237
#  include <mpp_setupgd_50.incl>                                                 SETUPGD.238
#endif                                                                           SETUPGD.239
      ENDIF                                                                      SETUPGD.240
C                                                                                SETUPGD.241
C IF INDXY(IN)=2, THEN BL WEIGHTING ARRAYS ARE READ IN FROM TAPE                 SETUPGD.242
C                                                                                SETUPGD.243
C PRINT OUT WEIGHTING ARRAYS...                                                  SETUPGD.244
   58 CONTINUE                                                                   SETUPGD.245
      IF(I4D(IN,1).NE.1)GOTO 5710                                                SETUPGD.246
      IF(INDT(IN,1).NE.1)TFAC(IN,1)=1.0                                          SETUPGD.247
      IF(INDXY(IN,1).NE.1.)THEN                                                  SETUPGD.248
#ifndef MPP1                                                                     SETUPGD.249
        CALL UNITY(WDT,IL,JL)                                                    SETUPGD.250
        CALL UNITY(WCS,IL,JL)                                                    SETUPGD.251
#else                                                                            SETUPGD.252
#  include <mpp_setupgd_60.incl>                                                 SETUPGD.253
#endif                                                                           SETUPGD.254
      ELSE                                                                       SETUPGD.255
C SPECIFY CONSTANT-IN-TIME WDT AND WCS HERE...                                   SETUPGD.256
        STOP 1                                                                   SETUPGD.257
      ENDIF                                                                      SETUPGD.258
 5710 CONTINUE                                                                   SETUPGD.259
      RETURN                                                                     SETUPGD.260
      END                                                                        SETUPGD.261
                                                                                 SETUPGD.262
