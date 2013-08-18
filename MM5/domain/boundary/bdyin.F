      SUBROUTINE BDYIN(IUNIT,TBDYBE,BDYTIM,BDYFRQ,IX,JX,IBMOIST)                 19DEC02.5
C      IMPLICIT NONE                                                             25JAN00.1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          BDYIN.3
C                                                                     C          BDYIN.4
C     THIS SUBROUTINE READS IN THE BOUNDARY CONDITIONS.               C          BDYIN.5
C                                                                     C          BDYIN.6
C                                                                     C          BDYIN.7
C        IUNIT : IS THE UNIT NUMBER FROM WHICH THE DATA ARE READ IN.  C          BDYIN.8
C                                                                     C          BDYIN.9
C        XTIME : IS THE TIME IN MINUTES INTO THE FORECAST.            C          BDYIN.10
C                                                                     C          BDYIN.11
C        TBDYBE : IS THE INITIAL TIME IN MINUTES OF THE PRESENT       C          BDYIN.12
C                 BOUNDARY CONDITIONS (OUTPUT).                       C          BDYIN.13
C                                                                     C          BDYIN.14
C        BDYTIM : IS THE TIME IN MINUTES AFTER WHICH THE BOUNDARY     C          BDYIN.15
C                 CONDITIONS ARE NEEDED.                              C          BDYIN.16
C                                                                     C          19DEC02.6
C        IBMOIST: WILL BE SET TO 1 IF CLOUD EXISTS IN BDYOUT          C          19DEC02.7
C                                                                     C          BDYIN.17
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          BDYIN.18
C                                                                                BDYIN.19
#     include <parame.incl>                                                      BDYIN.20
#     include <addr0.incl>                                                       BDYIN.21
#     include <various.incl>                                                     BDYIN.22
#     include <nonhydb.incl>                                                     BDYIN.23
#     include <pointbc.incl>                                                     BDYIN.24
#     include <hdrv3.incl>                                                       BDYIN.25
#     include <chardate.incl>                                                    BDYIN.26
                                                                                 BDYIN.27
#     include <nhcnst.incl>                                                      BDYIN.28
                                                                                 BDYIN.29
C                                                                                BDYIN.30
      CHARACTER*19 CDATENEW,CDATEFINAL                                           BDYIN.31
C                                                                                BDYIN.32
      INTEGER IUNIT,IX,JX,IBMOIST                                                19DEC02.8
      REAL TBDYBE,BDYTIM,BDYFRQ                                                  BDYIN.34
      INTEGER TIMDIF                                                             BDYIN.35
#ifndef MPP1                                                                     BDYIN.36
      REAL SCR(MIX*MJX*MKX*NSPGD)                                                24SEP99.1
#else                                                                            BDYIN.38
      INTEGER MIX2,MJX2,MIJX,NSPG2,SCRSZ,HDOTHSZ,IOSCRSZ                         07NOV00.29
      PARAMETER(MIX2=MAX(MIX,MIX_G))                                             07NOV00.30
      PARAMETER(MJX2=MAX(MJX,MJX_G))                                             07NOV00.31
      PARAMETER(MIJX=MAX(MIX2,MJX2))                                             07NOV00.32
      PARAMETER(NSPG2=MAX(NSPGD,NSPGX))                                          07NOV00.33
      PARAMETER(SCRSZ = MIJX*NSPG2*(MKX+1))                                      07NOV00.34
      PARAMETER(HDOTHSZ = MIX2*MJX2*(MKX+1))                                     07NOV00.35
      PARAMETER(IOSCRSZ = MAX(SCRSZ,HDOTHSZ))                                    07NOV00.36
      REAL SCR(IOSCRSZ), HDOTH(IOSCRSZ)                                          07NOV00.37
      COMMON /OUTSCRATCH/ SCR                                                    07NOV00.38
      EQUIVALENCE (SCR,HDOTH)                                                    07NOV00.39
#endif                                                                           BDYIN.41
      INTEGER NIV                                                                07NOV00.40
      INTEGER I,J                                                                BDYIN.42
      INTEGER IER,IXCHECK,JXCHECK                                                BDYIN.43
      INTEGER NDIM,LENDIM1,LENDIM2,LENDIM3                                       BDYIN.44
      LOGICAL DM_IONODE                                                          BDYIN.45
      EXTERNAL DM_IONODE                                                         BDYIN.46
      LOGICAL OPENED                                                             BDYIN.47
      LOGICAL IFOUNDCLD                                                          19DEC02.9
                                                                                 19DEC02.10
      IFOUNDCLD = .FALSE.                                                        19DEC02.11
C                                                                                BDYIN.48
C     CHECK IF BDY FILE IS ALREADY OPENED                                        BDYIN.49
C                                                                                BDYIN.50
      IF (DM_IONODE()) THEN                                                      BDYIN.51
        INQUIRE(IUNIT, OPENED=OPENED)                                            BDYIN.52
        IF (.NOT. OPENED) THEN                                                   BDYIN.53
            OPEN(IUNIT,FILE='BDYOUT_DOMAIN1',FORM='UNFORMATTED',                 BDYIN.54
     &           STATUS='OLD',ERR=9020)                                          25JAN00.2
        ENDIF                                                                    BDYIN.56
      ENDIF                                                                      BDYIN.57
C                                                                                BDYIN.58
C     THE BOUNDARY TENDENCIES ARE CALCULATED AS:                                 BDYIN.59
C         A(AT CDATEFINAL) - A (AT CDATENEW) / TIME DIFFERENCE IN SEC            BDYIN.60
C     WHERE A IS P*, P*U, P*V, P*T, AND P*QV.                                    BDYIN.61
C                                                                                BDYIN.62
  100 CONTINUE                                                                   BDYIN.63
#ifndef MPP1                                                                     25JAN00.3
      READ (IUNIT,ERR=9009,END=9010)FLAG                                         25JAN00.4
#else                                                                            25JAN00.5
      IF(DM_IONODE())THEN                                                        25JAN00.6
        READ (IUNIT,ERR=9009,END=7010)FLAG                                       25JAN00.7
        GOTO 8010                                                                25JAN00.8
 7010   CONTINUE                                                                 25JAN00.9
        FLAG = FILEDONE                                                          25JAN00.10
 8010   CONTINUE                                                                 25JAN00.11
      ENDIF                                                                      25JAN00.12
      CALL DM_BCAST_INTEGERS(FLAG,1)                                             BDYIN.66
      IF(FLAG.EQ.FILEDONE) GOTO 9010                                             25JAN00.13
#endif                                                                           BDYIN.67
      IF(FLAG.EQ.0)THEN                                                          BDYIN.68
        IF(DM_IONODE())THEN                                                      BDYIN.69
          READ (IUNIT,IOSTAT=IER)JBHI,JBHR,JBHIC,JBHRC                           BDYIN.70
          IF(IER.NE.0)THEN                                                       BDYIN.71
              WRITE(*,'("Error reading big header from bdy file")')              BDYIN.72
            STOP                                                                 BDYIN.73
          ENDIF                                                                  BDYIN.74
          IF(KTAU.EQ.0.OR.KTAU.EQ.KTAUR) THEN                                    07NOV00.41
C                                                                                07NOV00.42
C ... Fill header with BDYOUT information                                        07NOV00.43
C                                                                                07NOV00.44
          DO NIV = 1,NUMINT                                                      07NOV00.45
             BHI(NIV,7) = JBHI(NIV,7)                                            07NOV00.46
             BHIC(NIV,7) = JBHIC(NIV,7)                                          07NOV00.47
          END DO                                                                 07NOV00.48
          DO NIV = 1,NUMREAL                                                     07NOV00.49
             BHR(NIV,7) = JBHR(NIV,7)                                            07NOV00.50
             BHRC(NIV,7) = JBHRC(NIV,7)                                          07NOV00.51
          END DO                                                                 07NOV00.52
          ENDIF                                                                  07NOV00.53
        ENDIF                                                                    BDYIN.75
#ifdef MPP1                                                                      BDYIN.76
        CALL DM_BCAST_INTEGERS(JBHI,NUMINT*NUMPROGS)                             BDYIN.77
        CALL DM_BCAST_REALS(JBHR,NUMREAL*NUMPROGS)                               25JAN00.14
#endif                                                                           BDYIN.79
C                                                                                BDYIN.80
C ... Do a simple array dimension check here                                     BDYIN.81
C                                                                                BDYIN.82
        IXCHECK=JBHI(16,1)                                                       BDYIN.83
        JXCHECK=JBHI(17,1)                                                       BDYIN.84
        IF((IXCHECK.NE.IX).OR.(JXCHECK.NE.JX))THEN                               BDYIN.85
            PRINT *,'THE BOUNDARY FILE DOES NOT MATCH THE EXPECTED SIZE'         BDYIN.86
            PRINT *,'NESTIX NESTJX FROM NAMELIST      =',IX,JX                   BDYIN.87
            PRINT *,'NESTIX NESTJX FROM BOUNDARY FILE =',IXCHECK,JXCHECK         BDYIN.88
          STOP 'BOUNDARY FILE PROBLEM'                                           BDYIN.89
        ENDIF                                                                    BDYIN.90
C                                                                                BDYIN.91
C ... STORE BDY FREQUENCY (IN sec)                                               BDYIN.92
C                                                                                BDYIN.93
        BDYFRQ=JBHR(1,7)                                                         BDYIN.94
        GOTO 100                                                                 BDYIN.95
      ELSEIF(FLAG.EQ.1)THEN                                                      BDYIN.96
        IF(DM_IONODE())THEN                                                      BDYIN.97
          READ (IUNIT,IOSTAT=IER)NDIM,START_INDEX,END_INDEX,RTIME,               BDYIN.98
     +                          STAGGERING,ORDERING,CURRENT_DATE,NAME,           BDYIN.99
     +                          UNITS,DESCRIPTION                                BDYIN.100
          IF(IER.NE.0)THEN                                                       BDYIN.101
             WRITE(*,'("Error reading subheader from subs temp file")')          BDYIN.102
            STOP                                                                 BDYIN.103
          ENDIF                                                                  BDYIN.104
        ENDIF                                                                    BDYIN.105
#ifdef MPP1                                                                      BDYIN.106
        CALL DM_BCAST_INTEGERS(NDIM,1)                                           BDYIN.107
        CALL DM_BCAST_INTEGERS(START_INDEX,LEN_START_INDEX)                      BDYIN.108
        CALL DM_BCAST_INTEGERS(END_INDEX,LEN_END_INDEX)                          BDYIN.109
        CALL DM_BCAST_REALS(RTIME,1)                                             BDYIN.110
        CALL DM_BCAST_STRING(STAGGERING,LEN_STAGGERING)                          BDYIN.111
        CALL DM_BCAST_STRING(ORDERING,LEN_ORDERING)                              BDYIN.112
        CALL DM_BCAST_STRING(CURRENT_DATE,LEN_CURRENT_DATE)                      BDYIN.113
        CALL DM_BCAST_STRING(NAME,LEN_NAME)                                      BDYIN.114
        CALL DM_BCAST_STRING(UNITS,LEN_UNITS)                                    BDYIN.115
        CALL DM_BCAST_STRING(DESCRIPTION,LEN_DESCRIPTION)                        BDYIN.116
#endif                                                                           BDYIN.117
C                                                                                BDYIN.118
        LENDIM1=(END_INDEX(1)-START_INDEX(1)+1)                                  BDYIN.119
        LENDIM2=(END_INDEX(2)-START_INDEX(2)+1)                                  BDYIN.120
        LENDIM3=(END_INDEX(3)-START_INDEX(3)+1)                                  BDYIN.121
C                                                                                BDYIN.122
        IF(DM_IONODE())THEN                                                      BDYIN.123
          READ (IUNIT)(SCR(I),I=1,LENDIM1*LENDIM2*LENDIM3)                       BDYIN.124
        ENDIF                                                                    BDYIN.125
        CALL DM_DIST_BDY(ORDERING,NAME,IUNIT,1,SCR,NDIM,LENDIM1,LENDIM2,         BDYIN.126
     +       LENDIM3,MIX,MJX,MKX)                                                BDYIN.127
C                                                                                BDYIN.128
C ... CHECK FOR FIELD NAMES AND FILL BDY ARRAYS                                  BDYIN.129
C                                                                                BDYIN.130
        IF(NAME(1:4).EQ.'PEB ')THEN                                              BDYIN.131
          CALL EQUATE(SCR,IL,NSPGX,1,PEB,MIX,NSPGX,1)                            BDYIN.132
        ELSEIF(NAME(1:4).EQ.'PWW ')THEN                                          BDYIN.133
          CALL EQUATE(SCR,IL,NSPGX,1,PWW,MIX,NSPGX,1)                            BDYIN.134
        ELSEIF(NAME(1:4).EQ.'PNB ')THEN                                          BDYIN.135
          CALL EQUATE(SCR,JL,NSPGX,1,PNB,MJX,NSPGX,1)                            BDYIN.136
        ELSEIF(NAME(1:4).EQ.'PSS ')THEN                                          BDYIN.137
          CALL EQUATE(SCR,JL,NSPGX,1,PSS,MJX,NSPGX,1)                            BDYIN.138
        ELSEIF(NAME(1:4).EQ.'PEBT')THEN                                          BDYIN.139
          CALL EQUATE(SCR,IL,NSPGX,1,PEBT,MIX,NSPGX,1)                           BDYIN.140
        ELSEIF(NAME(1:4).EQ.'PWBT')THEN                                          BDYIN.141
          CALL EQUATE(SCR,IL,NSPGX,1,PWBT,MIX,NSPGX,1)                           BDYIN.142
        ELSEIF(NAME(1:4).EQ.'PNBT')THEN                                          BDYIN.143
          CALL EQUATE(SCR,JL,NSPGX,1,PNBT,MJX,NSPGX,1)                           BDYIN.144
        ELSEIF(NAME(1:4).EQ.'PSBT')THEN                                          BDYIN.145
          CALL EQUATE(SCR,JL,NSPGX,1,PSBT,MJX,NSPGX,1)                           BDYIN.146
        ELSEIF(NAME(1:4).EQ.'UEB ')THEN                                          BDYIN.147
          CALL EQUATE(SCR,IL,KL,NSPGD,UEB,MIX,MKX,NSPGD)                         BDYIN.148
        ELSEIF(NAME(1:4).EQ.'UWB ')THEN                                          BDYIN.149
          CALL EQUATE(SCR,IL,KL,NSPGD,UWB,MIX,MKX,NSPGD)                         BDYIN.150
        ELSEIF(NAME(1:4).EQ.'UNB ')THEN                                          BDYIN.151
          CALL EQUATE(SCR,JL,KL,NSPGD,UNB,MJX,MKX,NSPGD)                         BDYIN.152
        ELSEIF(NAME(1:4).EQ.'USB ')THEN                                          BDYIN.153
          CALL EQUATE(SCR,JL,KL,NSPGD,USB,MJX,MKX,NSPGD)                         BDYIN.154
        ELSEIF(NAME(1:4).EQ.'UEBT')THEN                                          BDYIN.155
          CALL EQUATE(SCR,IL,KL,NSPGD,UEBT,MIX,MKX,NSPGD)                        BDYIN.156
        ELSEIF(NAME(1:4).EQ.'UWBT')THEN                                          BDYIN.157
          CALL EQUATE(SCR,IL,KL,NSPGD,UWBT,MIX,MKX,NSPGD)                        BDYIN.158
        ELSEIF(NAME(1:4).EQ.'UNBT')THEN                                          BDYIN.159
          CALL EQUATE(SCR,JL,KL,NSPGD,UNBT,MJX,MKX,NSPGD)                        BDYIN.160
        ELSEIF(NAME(1:4).EQ.'USBT')THEN                                          BDYIN.161
          CALL EQUATE(SCR,JL,KL,NSPGD,USBT,MJX,MKX,NSPGD)                        BDYIN.162
        ELSEIF(NAME(1:4).EQ.'VEB ')THEN                                          BDYIN.163
          CALL EQUATE(SCR,IL,KL,NSPGD,VEB,MIX,MKX,NSPGD)                         BDYIN.164
        ELSEIF(NAME(1:4).EQ.'VWB ')THEN                                          BDYIN.165
          CALL EQUATE(SCR,IL,KL,NSPGD,VWB,MIX,MKX,NSPGD)                         BDYIN.166
        ELSEIF(NAME(1:4).EQ.'VNB ')THEN                                          BDYIN.167
          CALL EQUATE(SCR,JL,KL,NSPGD,VNB,MJX,MKX,NSPGD)                         BDYIN.168
        ELSEIF(NAME(1:4).EQ.'VSB ')THEN                                          BDYIN.169
          CALL EQUATE(SCR,JL,KL,NSPGD,VSB,MJX,MKX,NSPGD)                         BDYIN.170
        ELSEIF(NAME(1:4).EQ.'VEBT')THEN                                          BDYIN.171
          CALL EQUATE(SCR,IL,KL,NSPGD,VEBT,MIX,MKX,NSPGD)                        BDYIN.172
        ELSEIF(NAME(1:4).EQ.'VWBT')THEN                                          BDYIN.173
          CALL EQUATE(SCR,IL,KL,NSPGD,VWBT,MIX,MKX,NSPGD)                        BDYIN.174
        ELSEIF(NAME(1:4).EQ.'VNBT')THEN                                          BDYIN.175
          CALL EQUATE(SCR,JL,KL,NSPGD,VNBT,MJX,MKX,NSPGD)                        BDYIN.176
        ELSEIF(NAME(1:4).EQ.'VSBT')THEN                                          BDYIN.177
          CALL EQUATE(SCR,JL,KL,NSPGD,VSBT,MJX,MKX,NSPGD)                        BDYIN.178
        ELSEIF(NAME(1:4).EQ.'TEB ')THEN                                          BDYIN.179
          CALL EQUATE(SCR,IL,KL,NSPGD,TEB,MIX,MKX,NSPGX)                         BDYIN.180
        ELSEIF(NAME(1:4).EQ.'TWB ')THEN                                          BDYIN.181
          CALL EQUATE(SCR,IL,KL,NSPGD,TWB,MIX,MKX,NSPGX)                         BDYIN.182
        ELSEIF(NAME(1:4).EQ.'TNB ')THEN                                          BDYIN.183
          CALL EQUATE(SCR,JL,KL,NSPGD,TNB,MJX,MKX,NSPGX)                         BDYIN.184
        ELSEIF(NAME(1:4).EQ.'TSB ')THEN                                          BDYIN.185
          CALL EQUATE(SCR,JL,KL,NSPGD,TSB,MJX,MKX,NSPGX)                         BDYIN.186
        ELSEIF(NAME(1:4).EQ.'TEBT')THEN                                          BDYIN.187
          CALL EQUATE(SCR,IL,KL,NSPGD,TEBT,MIX,MKX,NSPGX)                        BDYIN.188
        ELSEIF(NAME(1:4).EQ.'TWBT')THEN                                          BDYIN.189
          CALL EQUATE(SCR,IL,KL,NSPGD,TWBT,MIX,MKX,NSPGX)                        BDYIN.190
        ELSEIF(NAME(1:4).EQ.'TNBT')THEN                                          BDYIN.191
          CALL EQUATE(SCR,JL,KL,NSPGD,TNBT,MJX,MKX,NSPGX)                        BDYIN.192
        ELSEIF(NAME(1:4).EQ.'TSBT')THEN                                          BDYIN.193
          CALL EQUATE(SCR,JL,KL,NSPGD,TSBT,MJX,MKX,NSPGX)                        BDYIN.194
        ELSEIF(NAME(1:4).EQ.'QEB ')THEN                                          BDYIN.195
          CALL EQUATE(SCR,IL,KL,NSPGX,QEB,MIX,MKX,NSPGX)                         BDYIN.196
        ELSEIF(NAME(1:4).EQ.'QWB ')THEN                                          BDYIN.197
          CALL EQUATE(SCR,IL,KL,NSPGX,QWB,MIX,MKX,NSPGX)                         BDYIN.198
        ELSEIF(NAME(1:4).EQ.'QNB ')THEN                                          BDYIN.199
          CALL EQUATE(SCR,JL,KL,NSPGX,QNB,MJX,MKX,NSPGX)                         BDYIN.200
        ELSEIF(NAME(1:4).EQ.'QSB ')THEN                                          BDYIN.201
          CALL EQUATE(SCR,JL,KL,NSPGX,QSB,MJX,MKX,NSPGX)                         BDYIN.202
        ELSEIF(NAME(1:4).EQ.'QEBT')THEN                                          BDYIN.203
          CALL EQUATE(SCR,IL,KL,NSPGX,QEBT,MIX,MKX,NSPGX)                        BDYIN.204
        ELSEIF(NAME(1:4).EQ.'QWBT')THEN                                          BDYIN.205
          CALL EQUATE(SCR,IL,KL,NSPGX,QWBT,MIX,MKX,NSPGX)                        BDYIN.206
        ELSEIF(NAME(1:4).EQ.'QNBT')THEN                                          BDYIN.207
          CALL EQUATE(SCR,JL,KL,NSPGX,QNBT,MJX,MKX,NSPGX)                        BDYIN.208
        ELSEIF(NAME(1:4).EQ.'QSBT')THEN                                          BDYIN.209
          CALL EQUATE(SCR,JL,KL,NSPGX,QSBT,MJX,MKX,NSPGX)                        BDYIN.210
        ELSEIF(NAME(1:4).EQ.'WEB ')THEN                                          BDYIN.211
          CALL EQUATE(SCR,IL,KL+1,NSPGX,WEB,MIXNH,MKXNH+1,NSPGX)                 BDYIN.212
        ELSEIF(NAME(1:4).EQ.'WWB ')THEN                                          BDYIN.213
          CALL EQUATE(SCR,IL,KL+1,NSPGX,WWB,MIXNH,MKXNH+1,NSPGX)                 BDYIN.214
        ELSEIF(NAME(1:4).EQ.'WNB ')THEN                                          BDYIN.215
          CALL EQUATE(SCR,JL,KL+1,NSPGX,WNB,MJXNH,MKXNH+1,NSPGX)                 BDYIN.216
        ELSEIF(NAME(1:4).EQ.'WSB ')THEN                                          BDYIN.217
          CALL EQUATE(SCR,JL,KL+1,NSPGX,WSB,MJXNH,MKXNH+1,NSPGX)                 BDYIN.218
        ELSEIF(NAME(1:4).EQ.'WEBT')THEN                                          BDYIN.219
          CALL EQUATE(SCR,IL,KL+1,NSPGX,WEBT,MIXNH,MKXNH+1,NSPGX)                BDYIN.220
        ELSEIF(NAME(1:4).EQ.'WWBT')THEN                                          BDYIN.221
          CALL EQUATE(SCR,IL,KL+1,NSPGX,WWBT,MIXNH,MKXNH+1,NSPGX)                BDYIN.222
        ELSEIF(NAME(1:4).EQ.'WNBT')THEN                                          BDYIN.223
          CALL EQUATE(SCR,JL,KL+1,NSPGX,WNBT,MJXNH,MKXNH+1,NSPGX)                BDYIN.224
        ELSEIF(NAME(1:4).EQ.'WSBT')THEN                                          BDYIN.225
          CALL EQUATE(SCR,JL,KL+1,NSPGX,WSBT,MJXNH,MKXNH+1,NSPGX)                BDYIN.226
        ELSEIF(NAME(1:5).EQ.'PPEB ')THEN                                         BDYIN.227
          CALL EQUATE(SCR,IL,KL,NSPGX,PPEB,MIXNH,MKXNH,NSPGX)                    BDYIN.228
        ELSEIF(NAME(1:5).EQ.'PPWB ')THEN                                         BDYIN.229
          CALL EQUATE(SCR,IL,KL,NSPGX,PPWB,MIXNH,MKXNH,NSPGX)                    BDYIN.230
        ELSEIF(NAME(1:5).EQ.'PPNB ')THEN                                         BDYIN.231
          CALL EQUATE(SCR,JL,KL,NSPGX,PPNB,MJXNH,MKXNH,NSPGX)                    BDYIN.232
        ELSEIF(NAME(1:5).EQ.'PPSB ')THEN                                         BDYIN.233
          CALL EQUATE(SCR,JL,KL,NSPGX,PPSB,MJXNH,MKXNH,NSPGX)                    BDYIN.234
        ELSEIF(NAME(1:5).EQ.'PPEBT')THEN                                         BDYIN.235
          CALL EQUATE(SCR,IL,KL,NSPGX,PPEBT,MIXNH,MKXNH,NSPGX)                   BDYIN.236
        ELSEIF(NAME(1:5).EQ.'PPWBT')THEN                                         BDYIN.237
          CALL EQUATE(SCR,IL,KL,NSPGX,PPWBT,MIXNH,MKXNH,NSPGX)                   BDYIN.238
        ELSEIF(NAME(1:5).EQ.'PPNBT')THEN                                         BDYIN.239
          CALL EQUATE(SCR,JL,KL,NSPGX,PPNBT,MJXNH,MKXNH,NSPGX)                   BDYIN.240
        ELSEIF(NAME(1:5).EQ.'PPSBT')THEN                                         BDYIN.241
          CALL EQUATE(SCR,JL,KL,NSPGX,PPSBT,MJXNH,MKXNH,NSPGX)                   BDYIN.242
        ELSEIF(NAME(1:5).EQ.'QCEB '.AND.MIXM.GT.1)THEN                           BDYIN.243
          CALL EQUATE(SCR,IL,KL,NSPGX,QCEB,MIXM,MKXM,NSPGX)                      BDYIN.244
          IFOUNDCLD = .TRUE.                                                     19DEC02.12
        ELSEIF(NAME(1:5).EQ.'QCWB '.AND.MIXM.GT.1)THEN                           BDYIN.245
          CALL EQUATE(SCR,IL,KL,NSPGX,QCWB,MIXM,MKXM,NSPGX)                      BDYIN.246
        ELSEIF(NAME(1:5).EQ.'QCNB '.AND.MIXM.GT.1)THEN                           BDYIN.247
          CALL EQUATE(SCR,JL,KL,NSPGX,QCNB,MJXM,MKXM,NSPGX)                      BDYIN.248
        ELSEIF(NAME(1:5).EQ.'QCSB '.AND.MIXM.GT.1)THEN                           BDYIN.249
          CALL EQUATE(SCR,JL,KL,NSPGX,QCSB,MJXM,MKXM,NSPGX)                      BDYIN.250
        ELSEIF(NAME(1:5).EQ.'QCEBT'.AND.MIXM.GT.1)THEN                           BDYIN.251
          CALL EQUATE(SCR,IL,KL,NSPGX,QCEBT,MIXM,MKXM,NSPGX)                     BDYIN.252
        ELSEIF(NAME(1:5).EQ.'QCWBT'.AND.MIXM.GT.1)THEN                           BDYIN.253
          CALL EQUATE(SCR,IL,KL,NSPGX,QCWBT,MIXM,MKXM,NSPGX)                     BDYIN.254
        ELSEIF(NAME(1:5).EQ.'QCNBT'.AND.MIXM.GT.1)THEN                           BDYIN.255
          CALL EQUATE(SCR,JL,KL,NSPGX,QCNBT,MJXM,MKXM,NSPGX)                     BDYIN.256
        ELSEIF(NAME(1:5).EQ.'QCSBT'.AND.MIXM.GT.1)THEN                           BDYIN.257
          CALL EQUATE(SCR,JL,KL,NSPGX,QCSBT,MJXM,MKXM,NSPGX)                     BDYIN.258
        ELSEIF(NAME(1:5).EQ.'QREB '.AND.MIXM.GT.1)THEN                           BDYIN.259
          CALL EQUATE(SCR,IL,KL,NSPGX,QREB,MIXM,MKXM,NSPGX)                      BDYIN.260
        ELSEIF(NAME(1:5).EQ.'QRWB '.AND.MIXM.GT.1)THEN                           BDYIN.261
          CALL EQUATE(SCR,IL,KL,NSPGX,QRWB,MIXM,MKXM,NSPGX)                      BDYIN.262
        ELSEIF(NAME(1:5).EQ.'QRNB '.AND.MIXM.GT.1)THEN                           BDYIN.263
          CALL EQUATE(SCR,JL,KL,NSPGX,QRNB,MJXM,MKXM,NSPGX)                      BDYIN.264
        ELSEIF(NAME(1:5).EQ.'QRSB '.AND.MIXM.GT.1)THEN                           BDYIN.265
          CALL EQUATE(SCR,JL,KL,NSPGX,QRSB,MJXM,MKXM,NSPGX)                      BDYIN.266
        ELSEIF(NAME(1:5).EQ.'QREBT'.AND.MIXM.GT.1)THEN                           BDYIN.267
          CALL EQUATE(SCR,IL,KL,NSPGX,QREBT,MIXM,MKXM,NSPGX)                     BDYIN.268
        ELSEIF(NAME(1:5).EQ.'QRWBT'.AND.MIXM.GT.1)THEN                           BDYIN.269
          CALL EQUATE(SCR,IL,KL,NSPGX,QRWBT,MIXM,MKXM,NSPGX)                     BDYIN.270
        ELSEIF(NAME(1:5).EQ.'QRNBT'.AND.MIXM.GT.1)THEN                           BDYIN.271
          CALL EQUATE(SCR,JL,KL,NSPGX,QRNBT,MJXM,MKXM,NSPGX)                     BDYIN.272
        ELSEIF(NAME(1:5).EQ.'QRSBT'.AND.MIXM.GT.1)THEN                           BDYIN.273
          CALL EQUATE(SCR,JL,KL,NSPGX,QRSBT,MJXM,MKXM,NSPGX)                     BDYIN.274
        ELSEIF(NAME(1:5).EQ.'QIEB '.AND.MIXIC.GT.1)THEN                          BDYIN.275
          CALL EQUATE(SCR,IL,KL,NSPGX,QIEB,MIXIC,MKXIC,NSPGX)                    BDYIN.276
        ELSEIF(NAME(1:5).EQ.'QIWB '.AND.MIXIC.GT.1)THEN                          BDYIN.277
          CALL EQUATE(SCR,IL,KL,NSPGX,QIWB,MIXIC,MKXIC,NSPGX)                    BDYIN.278
        ELSEIF(NAME(1:5).EQ.'QINB '.AND.MIXIC.GT.1)THEN                          BDYIN.279
          CALL EQUATE(SCR,JL,KL,NSPGX,QINB,MJXIC,MKXIC,NSPGX)                    BDYIN.280
        ELSEIF(NAME(1:5).EQ.'QISB '.AND.MIXIC.GT.1)THEN                          BDYIN.281
          CALL EQUATE(SCR,JL,KL,NSPGX,QISB,MJXIC,MKXIC,NSPGX)                    BDYIN.282
        ELSEIF(NAME(1:5).EQ.'QIEBT'.AND.MIXIC.GT.1)THEN                          BDYIN.283
          CALL EQUATE(SCR,IL,KL,NSPGX,QIEBT,MIXIC,MKXIC,NSPGX)                   BDYIN.284
        ELSEIF(NAME(1:5).EQ.'QIWBT'.AND.MIXIC.GT.1)THEN                          BDYIN.285
          CALL EQUATE(SCR,IL,KL,NSPGX,QIWBT,MIXIC,MKXIC,NSPGX)                   BDYIN.286
        ELSEIF(NAME(1:5).EQ.'QINBT'.AND.MIXIC.GT.1)THEN                          BDYIN.287
          CALL EQUATE(SCR,JL,KL,NSPGX,QINBT,MJXIC,MKXIC,NSPGX)                   BDYIN.288
        ELSEIF(NAME(1:5).EQ.'QISBT'.AND.MIXIC.GT.1)THEN                          BDYIN.289
          CALL EQUATE(SCR,JL,KL,NSPGX,QISBT,MJXIC,MKXIC,NSPGX)                   BDYIN.290
        ELSEIF(NAME(1:6).EQ.'QNIEB '.AND.MIXIC.GT.1)THEN                         BDYIN.291
          CALL EQUATE(SCR,IL,KL,NSPGX,QNIEB,MIXIC,MKXIC,NSPGX)                   BDYIN.292
        ELSEIF(NAME(1:6).EQ.'QNIWB '.AND.MIXIC.GT.1)THEN                         BDYIN.293
          CALL EQUATE(SCR,IL,KL,NSPGX,QNIWB,MIXIC,MKXIC,NSPGX)                   BDYIN.294
        ELSEIF(NAME(1:6).EQ.'QNINB '.AND.MIXIC.GT.1)THEN                         BDYIN.295
          CALL EQUATE(SCR,JL,KL,NSPGX,QNINB,MJXIC,MKXIC,NSPGX)                   BDYIN.296
        ELSEIF(NAME(1:6).EQ.'QNISB '.AND.MIXIC.GT.1)THEN                         BDYIN.297
          CALL EQUATE(SCR,JL,KL,NSPGX,QNISB,MJXIC,MKXIC,NSPGX)                   BDYIN.298
        ELSEIF(NAME(1:6).EQ.'QNIEBT'.AND.MIXIC.GT.1)THEN                         BDYIN.299
          CALL EQUATE(SCR,IL,KL,NSPGX,QNIEBT,MIXIC,MKXIC,NSPGX)                  BDYIN.300
        ELSEIF(NAME(1:6).EQ.'QNIWBT'.AND.MIXIC.GT.1)THEN                         BDYIN.301
          CALL EQUATE(SCR,IL,KL,NSPGX,QNIWBT,MIXIC,MKXIC,NSPGX)                  BDYIN.302
        ELSEIF(NAME(1:6).EQ.'QNINBT'.AND.MIXIC.GT.1)THEN                         BDYIN.303
          CALL EQUATE(SCR,JL,KL,NSPGX,QNINBT,MJXIC,MKXIC,NSPGX)                  BDYIN.304
        ELSEIF(NAME(1:6).EQ.'QNISBT'.AND.MIXIC.GT.1)THEN                         BDYIN.305
          CALL EQUATE(SCR,JL,KL,NSPGX,QNISBT,MJXIC,MKXIC,NSPGX)                  BDYIN.306
        ELSEIF(NAME(1:5).EQ.'QGEB '.AND.MIXIG.GT.1)THEN                          BDYIN.307
          CALL EQUATE(SCR,IL,KL,NSPGX,QGEB,MIXIG,MKXIG,NSPGX)                    BDYIN.308
        ELSEIF(NAME(1:5).EQ.'QGWB '.AND.MIXIG.GT.1)THEN                          BDYIN.309
          CALL EQUATE(SCR,IL,KL,NSPGX,QGWB,MIXIG,MKXIG,NSPGX)                    BDYIN.310
        ELSEIF(NAME(1:5).EQ.'QGNB '.AND.MIXIG.GT.1)THEN                          BDYIN.311
          CALL EQUATE(SCR,JL,KL,NSPGX,QGNB,MJXIG,MKXIG,NSPGX)                    BDYIN.312
        ELSEIF(NAME(1:5).EQ.'QGSB '.AND.MIXIG.GT.1)THEN                          BDYIN.313
          CALL EQUATE(SCR,JL,KL,NSPGX,QGSB,MJXIG,MKXIG,NSPGX)                    BDYIN.314
        ELSEIF(NAME(1:5).EQ.'QGEBT'.AND.MIXIG.GT.1)THEN                          BDYIN.315
          CALL EQUATE(SCR,IL,KL,NSPGX,QGEBT,MIXIG,MKXIG,NSPGX)                   BDYIN.316
        ELSEIF(NAME(1:5).EQ.'QGWBT'.AND.MIXIG.GT.1)THEN                          BDYIN.317
          CALL EQUATE(SCR,IL,KL,NSPGX,QGWBT,MIXIG,MKXIG,NSPGX)                   BDYIN.318
        ELSEIF(NAME(1:5).EQ.'QGNBT'.AND.MIXIG.GT.1)THEN                          BDYIN.319
          CALL EQUATE(SCR,JL,KL,NSPGX,QGNBT,MJXIG,MKXIG,NSPGX)                   BDYIN.320
        ELSEIF(NAME(1:5).EQ.'QGSBT'.AND.MIXIG.GT.1)THEN                          BDYIN.321
          CALL EQUATE(SCR,JL,KL,NSPGX,QGSBT,MJXIG,MKXIG,NSPGX)                   BDYIN.322
        ELSEIF(NAME(1:6).EQ.'QNCEB '.AND.MIXIG.GT.1)THEN                         BDYIN.323
          CALL EQUATE(SCR,IL,KL,NSPGX,QNCEB,MIXIG,MKXIG,NSPGX)                   BDYIN.324
        ELSEIF(NAME(1:6).EQ.'QNCWB '.AND.MIXIG.GT.1)THEN                         BDYIN.325
          CALL EQUATE(SCR,IL,KL,NSPGX,QNCWB,MIXIG,MKXIG,NSPGX)                   BDYIN.326
        ELSEIF(NAME(1:6).EQ.'QNCNB '.AND.MIXIG.GT.1)THEN                         BDYIN.327
          CALL EQUATE(SCR,JL,KL,NSPGX,QNCNB,MJXIG,MKXIG,NSPGX)                   BDYIN.328
        ELSEIF(NAME(1:6).EQ.'QNCSB '.AND.MIXIG.GT.1)THEN                         BDYIN.329
          CALL EQUATE(SCR,JL,KL,NSPGX,QNCSB,MJXIG,MKXIG,NSPGX)                   BDYIN.330
        ELSEIF(NAME(1:6).EQ.'QNCEBT'.AND.MIXIG.GT.1)THEN                         BDYIN.331
          CALL EQUATE(SCR,IL,KL,NSPGX,QNCEBT,MIXIG,MKXIG,NSPGX)                  BDYIN.332
        ELSEIF(NAME(1:6).EQ.'QNCWBT'.AND.MIXIG.GT.1)THEN                         BDYIN.333
          CALL EQUATE(SCR,IL,KL,NSPGX,QNCWBT,MIXIG,MKXIG,NSPGX)                  BDYIN.334
        ELSEIF(NAME(1:6).EQ.'QNCNBT'.AND.MIXIG.GT.1)THEN                         BDYIN.335
          CALL EQUATE(SCR,JL,KL,NSPGX,QNCNBT,MJXIG,MKXIG,NSPGX)                  BDYIN.336
        ELSEIF(NAME(1:6).EQ.'QNCSBT'.AND.MIXIG.GT.1)THEN                         BDYIN.337
          CALL EQUATE(SCR,JL,KL,NSPGX,QNCSBT,MJXIG,MKXIG,NSPGX)                  BDYIN.338
        ENDIF                                                                    BDYIN.339
        GOTO 100                                                                 BDYIN.340
      ELSEIF(FLAG.EQ.2)THEN                                                      BDYIN.341
         PRINT *,'END OF BOUNDARY INPUT TIME AT ',CURRENT_DATE(1:19)             BDYIN.342
C                                                                                BDYIN.343
C ... Compare time read from bdy file to that from IC file                       BDYIN.344
C        CDATE is the model initial time                                         BDYIN.345
C        CDATENEW is the current model time                                      BDYIN.346
C        CALL date add utility (cdate + xtime*60. = cdatenew) to get             BDYIN.347
C             current model date                                                 BDYIN.348
C     Note: may need a buffer time zone                                          BDYIN.349
C                                                                                BDYIN.350
        CALL GETH_NEWDATE(CDATENEW,CDATE,NINT(XTIME*60.))                        BDYIN.351
        CALL GETH_NEWDATE(CDATEFINAL,CURRENT_DATE(1:19),NINT(BDYFRQ))            BDYIN.352
        IF(KTAU.EQ.0.OR.KTAU.EQ.KTAUR)THEN                                       BDYIN.353
          IF(CURRENT_DATE(1:19).LT.CDATENEW.AND.CDATEFINAL.LE.CDATENEW)          BDYIN.354
     +      THEN                                                                 BDYIN.355
              PRINT *,'SKIP BOUNDARY CONDITION AT ', CURRENT_DATE(1:19)          BDYIN.356
            GOTO 100                                                             BDYIN.357
          ENDIF                                                                  BDYIN.358
          IF(XTIME.EQ.0..AND.CURRENT_DATE(1:19).NE.CDATE)THEN                    BDYIN.359
            PRINT30,XTIME,CURRENT_DATE(1:19),CDATE                               BDYIN.360
            STOP ' 9011 IN BDYIN'                                                BDYIN.361
          ENDIF                                                                  BDYIN.362
        ENDIF                                                                    BDYIN.363
         PRINT *,'FOUND BOUNDARY CONDITION AT MODEL TIME ',CDATENEW              BDYIN.364
        PRINT20,CURRENT_DATE(1:16),CDATEFINAL(1:16),XTIME                        BDYIN.365
        CALL GETH_IDTS(CURRENT_DATE(1:16),CDATE(1:16),TIMDIF)                    BDYIN.366
        TBDYBE=FLOAT(TIMDIF)                                                     BDYIN.367
        BDYTIM=TBDYBE+BDYFRQ/60.                                                 BDYIN.368
C        PRINT *, 'TBDYBE,BDYTIM = ',TBDYBE,BDYTIM                               BDYIN.369
        IF(IBMOIST.EQ.1.AND.(.NOT.IFOUNDCLD))THEN                                19DEC02.13
C       THIS MAY OCCUR IN FORECASTS WITH CYCLED INITIAL CONDITIONS               19DEC02.14
          PRINT *,'CLOUD NOT IN BOUNDARY FILE BUT IN INPUT FILE'                 19DEC02.15
          PRINT *,'RESETTING IBMOIST TO ZERO'                                    19DEC02.16
          IBMOIST = 0                                                            19DEC02.17
        ENDIF                                                                    19DEC02.18
        IF(IBMOIST.EQ.0.AND.(IFOUNDCLD))THEN                                     19DEC02.19
C       THIS SHOULD BE AN UNUSUAL OCCURRENCE                                     19DEC02.20
C       ASSUME THAT IF CLOUD IS IN BDY FILE IT SHOULD BE USED                    19DEC02.21
          PRINT *,'CLOUD IN BOUNDARY FILE BUT NOT IN INPUT FILE (?)'             19DEC02.22
          PRINT *,'RESETTING IBMOIST TO ONE'                                     19DEC02.23
          IBMOIST = 1                                                            19DEC02.24
        ENDIF                                                                    19DEC02.25
        RETURN                                                                   BDYIN.370
      ELSE                                                                       BDYIN.371
         PRINT *, 'HEADER FLAG IS WRONG!'                                        BDYIN.372
        STOP ' 9012 IN BDYIN'                                                    BDYIN.373
      ENDIF                                                                      BDYIN.374
20    FORMAT(/' *** BOUNDARY CONDITIONS VALID BETWEEN ',A16,' - ',               BDYIN.375
     1       A16,' ARE READ IN AT TIME = ',F10.2,' MINUTES.')                    BDYIN.376
30    FORMAT(/' *** BEGINNING OF BOUNDARY TIME DOES NOT MATCH THE ',             BDYIN.377
     &      'MODEL STARTING TIME. XTIME,CURRENT_DATE,CDATE = ',F10.2,            25JAN00.15
     &       2X,A19,2X,A19)                                                      BDYIN.379
 9009 CONTINUE                                                                   BDYIN.380
      PRINT *,'ERROR IN READING BC FILE. CHECK IF IT IS THE RIGHT FILE'          BDYIN.381
      STOP '9009 IN BDYIN'                                                       BDYIN.382
9010  PRINT *,'REACHING THE END OF BC FILE. CHECK THE FILE'                      BDYIN.383
      STOP '9010 IN BDYIN'                                                       BDYIN.384
9020  PRINT *,'ERROR OPENING BOUNDARY FILE'                                      25JAN00.16
      STOP '9020 IN BDYIN'                                                       25JAN00.17
      END                                                                        BDYIN.385
                                                                                 BDYIN.386
