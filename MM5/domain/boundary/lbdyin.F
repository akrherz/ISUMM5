      SUBROUTINE LBDYIN(INEST,TBDYLBE,BDYLTIM,BDYLFRQ,ISOIL,IACTIV               LBDYIN.1
     -     ,IFSNOW,IEXSI,IMOIAV,IX,JX)                                           23DEC04.50
       IMPLICIT NONE                                                             LBDYIN.3
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          LBDYIN.4
C                                                                     C          LBDYIN.5
C     THIS SUBROUTINE READS IN THE LOWER BOUNDARY CONDITIONS.         C          LBDYIN.6
C                                                                     C          LBDYIN.7
C                                                                     C          LBDYIN.8
C        INEST : IS THE DOMAIN NUMBER FOR WHICH THE DATA ARE READ IN. C          LBDYIN.9
C                                                                     C          LBDYIN.10
C        TBDYLBE : IS THE TIME IN MINUTES OF THE PRESENT              C          LBDYIN.11
C                 BOUNDARY CONDITIONS (XTIME).                        C          LBDYIN.12
C                                                                     C          LBDYIN.13
C        BDYLTIM : IS THE TIME IN MINUTES AFTER WHICH NEW BOUNDARY    C          LBDYIN.14
C                 CONDITIONS ARE NEEDED (XTIME).                      C          LBDYIN.15
C                                                                     C          LBDYIN.16
C        BDYLFRQ : IS THE TIME IN SECONDS BETWEEN BOUNDARY            C          LBDYIN.17
C                 CONDITIONS.                                         C          LBDYIN.18
C                                                                     C          LBDYIN.19
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          LBDYIN.20
C                                                                                LBDYIN.21
#     include <defines.incl>                                                     LBDYIN.22
#     include <parame.incl>                                                      LBDYIN.23
#     include <addr0.incl>                                                       LBDYIN.24
#     include <point2d.incl>                                                     LBDYIN.25
#     include <soilp.incl>                                                       LBDYIN.26
#     include <various.incl>                                                     LBDYIN.27
#     include <point2dn.incl>                                                    LBDYIN.28
#     include <soilpn.incl>                                                      LBDYIN.29
#     include <variousn.incl>                                                    LBDYIN.30
#     include <nestl.incl>                                                       LBDYIN.31
#     include <hdrv3.incl>                                                       LBDYIN.32
#     include <chardate.incl>                                                    LBDYIN.33
#     include <landuse.incl>                                                     05DEC01.6
C                                                                                LBDYIN.34
      CHARACTER*19 CDATENEW,CDATEFINAL                                           LBDYIN.35
C                                                                                LBDYIN.36
      REAL TBDYLBE,BDYLTIM,BDYLFRQ                                               LBDYIN.37
      REAL HELP1(MIX,MJX),HSCR1N(MIX,MJX)                                        LBDYIN.38
      INTEGER INEST,IUNIT,IX,JX,ISOIL,IEXSI                                      19DEC02.27
      INTEGER TIMDIF                                                             LBDYIN.40
      INTEGER IACTIV(MAXSES),IFSNOW(MAXSES),IMOIAV(MAXSES)                       23DEC04.51
      INTEGER I,J,NC,NN,NIV,IS                                                   05DEC01.8
      INTEGER IFOUNDTMN, IFOUNDTSS, IFOUNDSNC, IFOUNDSIC, IFOUNDSIF              19DEC02.28
      INTEGER IER,IXCHECK,JXCHECK                                                LBDYIN.44
      INTEGER NDIM,LENDIM1,LENDIM2,LENDIM3                                       LBDYIN.45
#ifdef MPP1                                                                      LBDYIN.46
# include <mpp_lbdyin_00.incl>                                                   LBDYIN.47
#endif                                                                           LBDYIN.48
      LOGICAL DM_IONODE                                                          LBDYIN.49
      EXTERNAL DM_IONODE                                                         LBDYIN.50
      LOGICAL OPENED                                                             LBDYIN.51
C                                                                                LBDYIN.52
C     CHECK IF LOWBDY FILE IS ALREADY OPENED                                     LBDYIN.53
C                                                                                LBDYIN.54
      IUNIT=20+INEST                                                             LBDYIN.55
      PRINT 9000, INEST                                                          LBDYIN.56
9000  FORMAT(/,1X,'ATTEMPT TO READ LOWBDY CONDITIONS FOR DOMAIN ',I2)            LBDYIN.57
      IER = 0                                                                    LBDYIN.58
      IF (DM_IONODE()) THEN                                                      LBDYIN.59
        INQUIRE(IUNIT, OPENED=OPENED)                                            LBDYIN.60
        IF (.NOT. OPENED) THEN                                                   LBDYIN.61
            OPEN(IUNIT,FILE='LOWBDY_DOMAIN'//CHAR(INEST+ICHAR('0')),             LBDYIN.62
     &           FORM='UNFORMATTED',STATUS='OLD',IOSTAT=IER)                     LBDYIN.63
        ENDIF                                                                    LBDYIN.64
      ENDIF                                                                      LBDYIN.65
      CALL DM_BCAST_INTEGERS ( IER , 1 )                                         LBDYIN.66
      IF ( IER .NE. 0 ) GOTO 9020                                                LBDYIN.67
C                                                                                LBDYIN.68
      IFOUNDTMN=0                                                                LBDYIN.69
      IFOUNDTSS=0                                                                LBDYIN.70
      IFOUNDSNC=0                                                                LBDYIN.71
      IFOUNDSIC=0                                                                LBDYIN.72
      IFOUNDSIF=0                                                                19DEC02.29
                                                                                 LBDYIN.73
  100 CONTINUE                                                                   LBDYIN.74
      IF(DM_IONODE())READ (IUNIT,ERR=9007,END=9008)FLAG                          LBDYIN.75
#ifdef MPP1                                                                      LBDYIN.76
        CALL DM_BCAST_INTEGERS(FLAG,1)                                           LBDYIN.77
#endif                                                                           LBDYIN.78
        IF(FLAG.EQ.0)THEN                                                        LBDYIN.79
          IF(DM_IONODE())THEN                                                    LBDYIN.80
            READ (IUNIT,IOSTAT=IER)JBHI,JBHR,JBHIC,JBHRC                         LBDYIN.81
          ENDIF                                                                  LBDYIN.82
#ifdef MPP1                                                                      LBDYIN.83
          CALL DM_BCAST_INTEGERS(JBHI,NUMINT*NUMPROGS)                           LBDYIN.84
          CALL DM_BCAST_REALS(JBHR,NUMREAL*NUMPROGS)                             LBDYIN.85
          CALL DM_BCAST_INTEGERS(IER,1)                                          LBDYIN.86
#endif                                                                           LBDYIN.87
          IF(IER.NE.0)THEN                                                       LBDYIN.88
            write(*,'("Error reading big header from subs temp file")')          LBDYIN.89
            STOP                                                                 LBDYIN.90
          ENDIF                                                                  LBDYIN.91
C should do a simple array dimension check here                                  LBDYIN.92
          IXCHECK=JBHI(16,1)                                                     LBDYIN.93
          JXCHECK=JBHI(17,1)                                                     LBDYIN.94
          IF((IXCHECK.NE.IX).OR.(JXCHECK.NE.JX))THEN                             LBDYIN.95
            PRINT *,                                                             LBDYIN.96
     &      'THE LOWBDY FILE DOES NOT MATCH THE EXPECTED SIZE'                   LBDYIN.97
            PRINT *,'NESTIX NESTJX FROM NAMELIST            =',IX,JX             LBDYIN.98
            PRINT *,'NESTIX NESTJX FROM LOWBDY FILE =',                          LBDYIN.99
     &       IXCHECK,JXCHECK                                                     LBDYIN.100
            STOP 'LOWBDY INPUT FILE PROBLEM'                                     LBDYIN.101
          ENDIF                                                                  LBDYIN.102
          IF(INEST.EQ.1.AND.(KTAU.EQ.0.OR.KTAU.EQ.KTAUR)) THEN                   LBDYIN.103
C                                                                                LBDYIN.104
C ... STORE HEADER INFORMATION FROM LOWBDY                                       LBDYIN.105
C                                                                                LBDYIN.106
          DO NIV = 1,NUMINT                                                      LBDYIN.107
             BHI(NIV,6) = JBHI(NIV,6)                                            LBDYIN.108
             BHIC(NIV,6) = JBHIC(NIV,6)                                          LBDYIN.109
          END DO                                                                 LBDYIN.110
          DO NIV = 1,NUMREAL                                                     LBDYIN.111
             BHR(NIV,6) = JBHR(NIV,6)                                            LBDYIN.112
             BHRC(NIV,6) = JBHRC(NIV,6)                                          LBDYIN.113
          END DO                                                                 LBDYIN.114
          ENDIF                                                                  LBDYIN.115
C                                                                                LBDYIN.116
C ... STORE BDY FREQUENCY (IN sec)                                               LBDYIN.117
C                                                                                LBDYIN.118
          IF(INEST.EQ.1)BDYLFRQ=JBHR(1,6)                                        LBDYIN.119
                                                                                 LBDYIN.120
          GOTO 100                                                               LBDYIN.121
        ELSEIF(FLAG.EQ.1)THEN                                                    LBDYIN.122
          IF(DM_IONODE())THEN                                                    LBDYIN.123
            READ (IUNIT,IOSTAT=IER)NDIM,START_INDEX,END_INDEX,RTIME,             LBDYIN.124
     +                        STAGGERING,ORDERING,CURRENT_DATE,NAME,             LBDYIN.125
     +                        UNITS,DESCRIPTION                                  LBDYIN.126
          ENDIF                                                                  LBDYIN.127
#ifdef MPP1                                                                      LBDYIN.128
          CALL DM_BCAST_INTEGERS(NDIM,1)                                         LBDYIN.129
          CALL DM_BCAST_INTEGERS(START_INDEX,LEN_START_INDEX)                    LBDYIN.130
          CALL DM_BCAST_INTEGERS(END_INDEX,LEN_END_INDEX)                        LBDYIN.131
          CALL DM_BCAST_REALS(RTIME,1)                                           LBDYIN.132
          CALL DM_BCAST_STRING(STAGGERING,LEN_STAGGERING)                        LBDYIN.133
          CALL DM_BCAST_STRING(ORDERING,LEN_ORDERING)                            LBDYIN.134
          CALL DM_BCAST_STRING(CURRENT_DATE,LEN_CURRENT_DATE)                    LBDYIN.135
          CALL DM_BCAST_STRING(NAME,LEN_NAME)                                    LBDYIN.136
          CALL DM_BCAST_STRING(UNITS,LEN_UNITS)                                  LBDYIN.137
          CALL DM_BCAST_STRING(DESCRIPTION,LEN_DESCRIPTION)                      LBDYIN.138
          CALL DM_BCAST_INTEGERS(IER,1)                                          LBDYIN.139
#endif                                                                           LBDYIN.140
          IF(IER.NE.0)THEN                                                       LBDYIN.141
            write(*,'("Error reading subheader from subs temp file")')           LBDYIN.142
            STOP                                                                 LBDYIN.143
          ENDIF                                                                  LBDYIN.144
C may need to check on time too - but later                                      LBDYIN.145
          IF(NDIM.EQ.2.AND.NAME(1:8).EQ.'RES TEMP'.AND.ISOIL.NE.2)THEN           LBDYIN.146
#ifndef MPP1                                                                     LBDYIN.147
            READ (IUNIT)((TMN(I,J),I=1,END_INDEX(1)),J=1,END_INDEX(2))           LBDYIN.148
#else                                                                            LBDYIN.149
            DO J = 1, MJX                                                        05DEC01.9
              DO I = 1, MIX                                                      LBDYIN.151
                TMN(I,J) = 0.                                                    LBDYIN.152
              ENDDO                                                              LBDYIN.153
            ENDDO                                                                LBDYIN.154
            LENDIM1=(END_INDEX(1)-START_INDEX(1)+1)                              LBDYIN.155
            LENDIM2=(END_INDEX(2)-START_INDEX(2)+1)                              LBDYIN.156
            LENDIM3=(END_INDEX(3)-START_INDEX(3)+1)                              LBDYIN.157
            CALL DM_DIST_READ(IUNIT,INEST,TMN,NDIM,LENDIM1,LENDIM2,1,            LBDYIN.158
     +           MIX,MJX,1)                                                      LBDYIN.159
#endif                                                                           LBDYIN.160
            PRINT *, 'FOUND INPUT FIELD ', NAME, ' FOR ', CURRENT_DATE           LBDYIN.161
            IFOUNDTMN=1                                                          LBDYIN.162
          ELSE IF(NDIM.EQ.2.AND.NAME(1:8).EQ.'TSEASFC ')THEN                     LBDYIN.163
#ifndef MPP1                                                                     LBDYIN.164
            READ (IUNIT)((TSS(I,J),I=1,END_INDEX(1)),J=1,END_INDEX(2))           LBDYIN.165
#else                                                                            LBDYIN.166
            DO J = 1, MJX                                                        05DEC01.10
              DO I = 1, MIX                                                      LBDYIN.168
                TSS(I,J) = 0.                                                    LBDYIN.169
              ENDDO                                                              LBDYIN.170
            ENDDO                                                                LBDYIN.171
            LENDIM1=(END_INDEX(1)-START_INDEX(1)+1)                              LBDYIN.172
            LENDIM2=(END_INDEX(2)-START_INDEX(2)+1)                              LBDYIN.173
            LENDIM3=(END_INDEX(3)-START_INDEX(3)+1)                              LBDYIN.174
            CALL DM_DIST_READ(IUNIT,INEST,TSS,NDIM,LENDIM1,LENDIM2,1,            LBDYIN.175
     +           MIX,MJX,1)                                                      LBDYIN.176
#endif                                                                           LBDYIN.177
            PRINT *, 'FOUND INPUT FIELD ', NAME, ' FOR ', CURRENT_DATE           LBDYIN.178
            IFOUNDTSS=1                                                          LBDYIN.179
          ELSE IF(NDIM.EQ.2.AND.NAME(1:8).EQ.'SNOWCOVR'                          05DEC01.11
     +            .AND.ISOIL.NE.2.AND.IFSNOW(INEST).NE.2)THEN                    19DEC02.30
#ifndef MPP1                                                                     LBDYIN.181
            READ (IUNIT)((SNOWC(I,J),I=1,END_INDEX(1)),J=1,END_INDEX(2))         LBDYIN.182
#else                                                                            LBDYIN.183
            DO J = 1, MJX                                                        05DEC01.13
              DO I = 1, MIX                                                      LBDYIN.185
                SNOWC(I,J) = 0.                                                  LBDYIN.186
              ENDDO                                                              LBDYIN.187
            ENDDO                                                                LBDYIN.188
            LENDIM1=(END_INDEX(1)-START_INDEX(1)+1)                              LBDYIN.189
            LENDIM2=(END_INDEX(2)-START_INDEX(2)+1)                              LBDYIN.190
            LENDIM3=(END_INDEX(3)-START_INDEX(3)+1)                              LBDYIN.191
            CALL DM_DIST_READ(IUNIT,INEST,SNOWC,NDIM,LENDIM1,LENDIM2,1,          LBDYIN.192
     +           MIX,MJX,1)                                                      LBDYIN.193
#endif                                                                           LBDYIN.194
            PRINT *, 'FOUND INPUT FIELD ', NAME, ' FOR ', CURRENT_DATE           LBDYIN.195
            IFOUNDSNC=1                                                          LBDYIN.196
          ELSE IF(NDIM.EQ.2.AND.NAME(1:8).EQ.'SEAICE  '.AND.ISOIL.EQ.2)          LBDYIN.197
     +           THEN                                                            LBDYIN.198
#ifndef MPP1                                                                     LBDYIN.199
            READ (IUNIT)((XICE(I,J),I=1,END_INDEX(1)),J=1,END_INDEX(2))          LBDYIN.200
#else                                                                            LBDYIN.201
            DO J = 1, MJXL                                                       05DEC01.14
              DO I = 1, MIXL                                                     LBDYIN.203
                XICE(I,J) = 0.                                                   LBDYIN.204
              ENDDO                                                              LBDYIN.205
            ENDDO                                                                LBDYIN.206
            LENDIM1=(END_INDEX(1)-START_INDEX(1)+1)                              LBDYIN.207
            LENDIM2=(END_INDEX(2)-START_INDEX(2)+1)                              LBDYIN.208
            LENDIM3=(END_INDEX(3)-START_INDEX(3)+1)                              LBDYIN.209
            CALL DM_DIST_READ(IUNIT,INEST,XICE,NDIM,LENDIM1,LENDIM2,1,           LBDYIN.210
     +           MIX,MJX,1)                                                      LBDYIN.211
#endif                                                                           LBDYIN.212
            PRINT *, 'FOUND INPUT FIELD ', NAME, ' FOR ', CURRENT_DATE           LBDYIN.213
            IFOUNDSIC=1                                                          LBDYIN.214
          ELSE IF(NDIM.EQ.2.AND.NAME(1:8).EQ.'SEAICEFR'.AND.IEXSI.EQ.2)          19DEC02.31
     +           THEN                                                            19DEC02.32
#ifndef MPP1                                                                     19DEC02.33
            READ (IUNIT)                                                         19DEC02.34
     -        ((SEAICE(I,J),I=1,END_INDEX(1)),J=1,END_INDEX(2))                  19DEC02.35
#else                                                                            19DEC02.36
            DO J = 1, MJXL                                                       19DEC02.37
              DO I = 1, MIXL                                                     19DEC02.38
                SEAICE(I,J) = 0.                                                 19DEC02.39
              ENDDO                                                              19DEC02.40
            ENDDO                                                                19DEC02.41
            LENDIM1=(END_INDEX(1)-START_INDEX(1)+1)                              19DEC02.42
            LENDIM2=(END_INDEX(2)-START_INDEX(2)+1)                              19DEC02.43
            LENDIM3=(END_INDEX(3)-START_INDEX(3)+1)                              19DEC02.44
            CALL DM_DIST_READ(IUNIT,INEST,SEAICE,NDIM,LENDIM1,LENDIM2,1,         19DEC02.45
     +           MIX,MJX,1)                                                      19DEC02.46
#endif                                                                           19DEC02.47
            PRINT *, 'FOUND INPUT FIELD ', NAME, ' FOR ', CURRENT_DATE           19DEC02.48
            IFOUNDSIF=1                                                          19DEC02.49
          ELSE                                                                   LBDYIN.215
            IF(DM_IONODE())READ (IUNIT)                                          LBDYIN.216
            PRINT *, 'SKIP FIELD NAMED ', NAME                                   LBDYIN.217
          ENDIF                                                                  LBDYIN.218
          GOTO 100                                                               LBDYIN.219
        ELSEIF(FLAG.EQ.2)THEN                                                    LBDYIN.220
C                                                                                LBDYIN.221
C ... Compare time read from bdy file to that from IC file                       LBDYIN.222
C        CDATE is the model initial time                                         LBDYIN.223
C        CDATENEW is the current model time                                      LBDYIN.224
C        CALL date add utility (cdate + xtime*60. = cdatenew) to get             LBDYIN.225
C             current model date                                                 LBDYIN.226
C     Note: may need a buffer time zone                                          LBDYIN.227
C                                                                                LBDYIN.228
          IF(BHI(3,6).GE.4) THEN                                                 LBDYIN.229
C ..... Do the following only if LOWBDY file is produced with                    LBDYIN.230
C          INTERPF V3.4 or later                                                 LBDYIN.231
          CALL GETH_NEWDATE(CDATENEW,CDATE,NINT(XTIME*60.))                      LBDYIN.232
          CALL GETH_NEWDATE(CDATEFINAL,CURRENT_DATE(1:19),NINT(BDYLFRQ))         LBDYIN.233
          IF(KTAU.EQ.0.OR.KTAU.EQ.KTAUR)THEN                                     LBDYIN.234
           IF(CURRENT_DATE(1:19).LT.CDATENEW.AND.CDATEFINAL.LE.CDATENEW)         LBDYIN.235
     +        THEN                                                               LBDYIN.236
                PRINT *,'SKIP LOWER BOUNDARY CONDITION AT ',                     LBDYIN.237
     +                   CURRENT_DATE(1:19)                                      LBDYIN.238
                IFOUNDTMN=0                                                      LBDYIN.239
                IFOUNDTSS=0                                                      LBDYIN.240
                IFOUNDSNC=0                                                      LBDYIN.241
                IFOUNDSIC=0                                                      LBDYIN.242
                IFOUNDSIF=0                                                      19DEC02.50
              GOTO 100                                                           LBDYIN.243
            ENDIF                                                                LBDYIN.244
            IF(XTIME.EQ.0..AND.CURRENT_DATE(1:19).NE.CDATE)THEN                  LBDYIN.245
              PRINT30,XTIME,CURRENT_DATE(1:19),CDATE                             LBDYIN.246
              IF(CURRENT_DATE(1:19).LE.CDATEFINAL)THEN                           LBDYIN.247
                 PRINT 40, CURRENT_DATE(1:16),CDATE(1:16),INEST                  LBDYIN.248
              ELSE                                                               LBDYIN.249
                 STOP ' LBDYIN TIME DOES NOT MATCH MODEL TIME'                   LBDYIN.250
              END IF                                                             LBDYIN.251
            ENDIF                                                                LBDYIN.252
          ENDIF                                                                  LBDYIN.253
          PRINT *,'FOUND LOWER BOUNDARY CONDITION AT MODEL TIME ',               LBDYIN.254
     -             CDATENEW                                                      LBDYIN.255
          PRINT 20, CURRENT_DATE(1:16),CDATEFINAL(1:16),XTIME,INEST              LBDYIN.256
          IF(INEST.EQ.1)THEN                                                     LBDYIN.257
            CALL GETH_IDTS(CURRENT_DATE(1:16),CDATE(1:16),TIMDIF)                LBDYIN.258
            TBDYLBE=FLOAT(TIMDIF)                                                LBDYIN.259
            BDYLTIM=TBDYLBE+BDYLFRQ/60.                                          LBDYIN.260
          ENDIF                                                                  LBDYIN.261
          ELSE                                                                   LBDYIN.262
C ..... Do the following for older LOWBDY files                                  LBDYIN.263
          PRINT *,'FOUND LOWER BOUNDARY CONDITION AT TIME ',                     LBDYIN.264
     -             CURRENT_DATE(1:16)                                            LBDYIN.265
          PRINT *,'THIS IS A PRE-V3.4 LOWBDY FILE'                               LBDYIN.266
          PRINT 40, CURRENT_DATE(1:16),CDATE(1:16),INEST                         LBDYIN.267
          ENDIF                                                                  LBDYIN.268
#ifdef MPP1                                                                      LBDYIN.269
          CALL DM_BCAST_INTEGERS(IFOUNDTMN,1)                                    LBDYIN.270
          CALL DM_BCAST_INTEGERS(IFOUNDTSS,1)                                    LBDYIN.271
          CALL DM_BCAST_INTEGERS(IFOUNDSNC,1)                                    LBDYIN.272
          CALL DM_BCAST_INTEGERS(IFOUNDSIC,1)                                    LBDYIN.273
          CALL DM_BCAST_INTEGERS(IFOUNDSIF,1)                                    23DEC04.52
#endif                                                                           LBDYIN.274
          IF(IFOUNDTMN.NE.1.AND.ISOIL.NE.2)THEN                                  LBDYIN.275
            PRINT *, 'SUBSTRATE TEMP NOT FOUND'                                  LBDYIN.276
            STOP                                                                 LBDYIN.277
          ENDIF                                                                  LBDYIN.278
          IF(IFOUNDTSS.NE.1)THEN                                                 LBDYIN.279
            PRINT *, 'SEA SURFACE TEMP NOT FOUND: NOT UPDATED'                   LBDYIN.280
          ENDIF                                                                  LBDYIN.281
          IF(IFOUNDSNC.NE.1.AND.ISOIL.EQ.2)THEN                                  05DEC01.15
            PRINT *, 'SNOW COVER NOT FOUND: NOT UPDATED'                         LBDYIN.283
          ENDIF                                                                  LBDYIN.284
          IF(IFOUNDSIC.NE.1.AND.ISOIL.EQ.2)THEN                                  LBDYIN.285
            PRINT *, 'SEA ICE NOT FOUND: NOT UPDATED'                            LBDYIN.286
          ENDIF                                                                  LBDYIN.287
          IF(IFOUNDSIF.NE.1.AND.IEXSI.EQ.2)THEN                                  19DEC02.51
            PRINT *, 'SEA ICE FRACTION NOT FOUND: NOT UPDATED'                   19DEC02.52
          ENDIF                                                                  19DEC02.53
        ENDIF                                                                    LBDYIN.288
  270 CONTINUE                                                                   LBDYIN.289
      IF(IFOUNDTSS.EQ.1)THEN                                                     LBDYIN.290
        PRINT *, 'UPDATING SST FOR DOMAIN ',INEST                                05DEC01.16
C   WATER TEMP ADJUSTMENT TO USE SST FROM LOWBDY FILE                            LBDYIN.291
        DO J=1,JLX                                                               LBDYIN.292
          DO I=1,ILX                                                             LBDYIN.293
C         CHECK IF WATER POINT                                                   LBDYIN.294
            IF(XLAND(I,J).GT.1.5)THEN                                            LBDYIN.295
              TGA(I,J)=TSS(I,J)                                                  LBDYIN.296
              TGB(I,J)=TSS(I,J)                                                  LBDYIN.297
            ENDIF                                                                LBDYIN.298
          ENDDO                                                                  LBDYIN.299
        ENDDO                                                                    LBDYIN.300
      ENDIF                                                                      LBDYIN.301
      IF(IFOUNDSNC.EQ.1.AND.IFSNOW(INEST).EQ.1.AND.ISOIL.NE.2)THEN               05DEC01.17
        PRINT *, 'UPDATING SNOW COVER FOR DOMAIN ',INEST                         05DEC01.18
C   SNOW COVER ADJUSTMENT TO LAND-USE PROPERTIES                                 05DEC01.19
        DO J=1,JLX                                                               05DEC01.20
          DO I=1,ILX                                                             05DEC01.21
            IS=INT(SATBRT(I,J)+0.001)                                            05DEC01.22
            IF(XLAND(I,J).GT.1.5)SNOWC(I,J)=-0.01                                05DEC01.23
            ALB(I,J)=ALBD(IS,ISN)/100.                                           05DEC01.24
            THC(I,J)=THERIN(IS,ISN)/100.                                         05DEC01.25
            EMISS(I,J)=SFEM(IS,ISN)                                              05DEC01.26
            IF(IMOIAV(INEST).EQ.0)MAVAIL(I,J)=SLMO(IS,ISN)                       23DEC04.53
            IF(SNOWC(I,J).GT.0.)THEN                                             05DEC01.28
              ALB(I,J)=ALB(I,J)+SCFX(IS)*ALB(I,J)                                05DEC01.29
              THC(I,J)=THERIN(ISICE,ISN)/100.                                    05DEC01.30
              EMISS(I,J)=SFEM(ISICE,ISN)                                         05DEC01.31
              MAVAIL(I,J)=SLMO(ISICE,ISN)                                        05DEC01.32
            ENDIF                                                                05DEC01.33
          ENDDO                                                                  05DEC01.34
        ENDDO                                                                    05DEC01.35
      ENDIF                                                                      05DEC01.36
C                                                                                LBDYIN.302
#ifdef NESTED                                                                    LBDYIN.303
C                                                                                LBDYIN.304
C     UPDATE NESTS BY INTERPOLATING FROM PARENT DOMAIN                           LBDYIN.305
C     THIS WILL REPEAT INTERPOLATIONS IF MORE THAN ONE DOMAIN HAS A              LBDYIN.306
C       LOWBDY FILE                                                              LBDYIN.307
C                                                                                LBDYIN.308
      IF(INEST+1.LE.MAXNES)THEN                                                  LBDYIN.309
      DO NN=INEST+1,MAXNES                                                       LBDYIN.310
        IF(IACTIV(NN).EQ.1)THEN                                                  LBDYIN.311
          NC=NUMNC(NN)                                                           LBDYIN.312
          PRINT60,NC,NN                                                          LBDYIN.313
          IF(NC.GE.NN)THEN                                                       LBDYIN.314
C           THIS ROUTINE ASSUMES MOTHER DOMAIN HAS ID NUMBER LESS THAN           LBDYIN.315
C             CHILD DOMAIN                                                       LBDYIN.316
            PRINT50                                                              LBDYIN.317
            STOP                                                                 LBDYIN.318
          ENDIF                                                                  LBDYIN.319
          CALL ADDRX1C(IAXALL(1,NC))                                             LBDYIN.320
          CALL ADDRX1N(IAXALL(1,NN))                                             LBDYIN.321
#ifndef MPP1                                                                     LBDYIN.322
C                                                                                LBDYIN.323
          IF(IFOUNDTSS.EQ.1)THEN                                                 LBDYIN.324
            CALL FILL(TSS,HELP1,MIX,MJX,IL,JL,ILX,JLX)                           LBDYIN.325
            CALL EXAINT(HELP1,IL,JL,HSCR1N,INL,JNL,ISOUTH,JWEST,1)               LBDYIN.326
            CALL FILL(HSCR1N,TSSN,INL,JNL,MIX,MJX,INLX,JNLX)                     LBDYIN.327
C   WATER TEMP ADJUSTMENT TO USE SST FROM LOWBDY FILE                            LBDYIN.328
            PRINT *, 'INTERPOLATING SST FOR DOMAIN ',NN                          05DEC01.37
            DO J=1,JNLX                                                          LBDYIN.329
              DO I=1,INLX                                                        LBDYIN.330
C             CHECK IF WATER POINT                                               LBDYIN.331
                IF(XLANDN(I,J).GT.1.5)THEN                                       LBDYIN.332
                  TGAN(I,J)=TSSN(I,J)                                            LBDYIN.333
                  TGBN(I,J)=TSSN(I,J)                                            LBDYIN.334
                ENDIF                                                            LBDYIN.335
              ENDDO                                                              LBDYIN.336
            ENDDO                                                                LBDYIN.337
          ENDIF                                                                  LBDYIN.338
C                                                                                LBDYIN.339
          IF(IFOUNDTMN.EQ.1)THEN                                                 LBDYIN.340
            CALL FILL(TMN,HELP1,MIX,MJX,ILX,JLX,ILXM,JLXM)                       LBDYIN.341
            CALL EXAINT(HELP1,ILX,JLX,HSCR1N,INLX,JNLX,ISOUTH,JWEST,1)           LBDYIN.342
            CALL FILL(HSCR1N,TMNN,INLX,JNLX,MIX,MJX,INLXM,JNLXM)                 LBDYIN.343
          ENDIF                                                                  LBDYIN.344
C                                                                                LBDYIN.345
          IF(IFOUNDSNC.EQ.1)THEN                                                 LBDYIN.346
            CALL FILL(SNOWC,HELP1,MIX,MJX,ILX,JLX,ILXM,JLXM)                     LBDYIN.347
            CALL EXAINT(HELP1,ILX,JLX,HSCR1N,INLX,JNLX,ISOUTH,JWEST,1)           LBDYIN.348
            CALL FILL(HSCR1N,SNOWCN,INLX,JNLX,MIX,MJX,INLXM,JNLXM)               LBDYIN.349
            IF(IFSNOW(NN).EQ.1.AND.ISOIL.NE.2)THEN                               05DEC01.38
C   SNOW COVER ADJUSTMENT TO LAND-USE PROPERTIES. ISOIL=2 WILL IGNORE THIS.      05DEC01.39
            PRINT *, 'INTERPOLATING SNOW COVER FOR DOMAIN ',NN                   05DEC01.40
              DO J=1,JNLX                                                        05DEC01.41
                DO I=1,INLX                                                      05DEC01.42
                  IS=INT(SATBRN(I,J)+0.001)                                      05DEC01.43
                  IF(XLANDN(I,J).GT.1.5)SNOWCN(I,J)=-0.01                        05DEC01.44
                  ALBN(I,J)=ALBD(IS,ISN)/100.                                    05DEC01.45
                  THCN(I,J)=THERIN(IS,ISN)/100.                                  05DEC01.46
                  EMISSN(I,J)=SFEM(IS,ISN)                                       05DEC01.47
                  IF(IMOIAV(NN).EQ.0)MAVAIN(I,J)=SLMO(IS,ISN)                    23DEC04.54
                  IF(SNOWCN(I,J).GT.0.)THEN                                      05DEC01.49
                    ALBN(I,J)=ALBN(I,J)+SCFX(IS)*ALBN(I,J)                       05DEC01.50
                    THCN(I,J)=THERIN(ISICE,ISN)/100.                             05DEC01.51
                    EMISSN(I,J)=SFEM(ISICE,ISN)                                  05DEC01.52
                    MAVAIN(I,J)=SLMO(ISICE,ISN)                                  05DEC01.53
                  ENDIF                                                          05DEC01.54
                ENDDO                                                            05DEC01.55
              ENDDO                                                              05DEC01.56
            ENDIF                                                                05DEC01.57
          ENDIF                                                                  LBDYIN.350
C                                                                                LBDYIN.351
          IF(ISOIL.EQ.2.AND.IFOUNDSIC.EQ.1)THEN                                  LBDYIN.352
            CALL FILL(XICE,HELP1,MIX,MJX,ILX,JLX,ILXM,JLXM)                      LBDYIN.353
            CALL EXAINT(HELP1,ILX,JLX,HSCR1N,INLX,JNLX,ISOUTH,JWEST,1)           LBDYIN.354
            CALL FILL(HSCR1N,XICEN,INLX,JNLX,MIX,MJX,INLXM,JNLXM)                LBDYIN.355
          ENDIF                                                                  19DEC02.54
          IF(IEXSI.EQ.2.AND.IFOUNDSIF.EQ.1)THEN                                  19DEC02.55
            CALL FILL(SEAICE,HELP1,MIX,MJX,ILX,JLX,ILXM,JLXM)                    19DEC02.56
            CALL EXAINT(HELP1,ILX,JLX,HSCR1N,INLX,JNLX,ISOUTH,JWEST,1)           19DEC02.57
            CALL FILL(HSCR1N,SEAICN,INLX,JNLX,MIX,MJX,INLXM,JNLXM)               19DEC02.58
          ENDIF                                                                  LBDYIN.356
#else                                                                            LBDYIN.357
# include <mpp_lbdyin_10.incl>                                                   LBDYIN.358
#endif                                                                           LBDYIN.359
        ENDIF                                                                    LBDYIN.360
      ENDDO                                                                      LBDYIN.361
      ENDIF                                                                      LBDYIN.362
                                                                                 LBDYIN.363
      CALL ADDRX1C(IAXALL(1,1))                                                  LBDYIN.364
#endif                                                                           LBDYIN.365
      RETURN                                                                     LBDYIN.366
C                                                                                LBDYIN.367
9007  PRINT *,'ERROR IN READING LOWBDY FILE.'                                    LBDYIN.368
      PRINT *,'CHECK IF IT IS THE RIGHT FILE'                                    LBDYIN.369
      STOP '9007 IN LBDYIN'                                                      LBDYIN.370
9008  PRINT *,'END OF LOWBDY FILE OR FILE EMPTY.'                                LBDYIN.371
      PRINT *,'OR FOR PRE-V3.4 LOWBDY FILE, SET ISSTVAR=0 AND RUN AGAIN'         LBDYIN.372
      IF(INEST.EQ.1)THEN                                                         LBDYIN.373
        STOP '9008 IN LBDYIN'                                                    LBDYIN.374
      ELSE                                                                       LBDYIN.375
        PRINT *,'EMPTY LOWBDY FILE FOR DOMAIN',INEST,                            LBDYIN.376
     -          '. INTERPOLATION USED'                                           LBDYIN.377
        RETURN                                                                   LBDYIN.378
      ENDIF                                                                      LBDYIN.379
9020  CONTINUE                                                                   23DEC04.55
      IF(INEST.EQ.1)THEN                                                         LBDYIN.381
        PRINT *,'ERROR OPENING LOWBDY FILE'                                      23DEC04.56
        STOP '9020 IN LBDYIN'                                                    LBDYIN.382
      ELSE                                                                       LBDYIN.383
        PRINT *,'ADVISORY:'                                                      23DEC04.57
        PRINT *,'NO LOWBDY FILE FOR DOMAIN',INEST,'. INTERPOLATION USED'         LBDYIN.384
        RETURN                                                                   LBDYIN.385
      ENDIF                                                                      LBDYIN.386
20    FORMAT(/' *** LOWER BDY CONDITIONS VALID BETWEEN ',A16,' - ',              LBDYIN.387
     1    A16,' ARE READ IN AT TIME = ',F10.2,' MINUTES FOR DOMAIN ',I2)         LBDYIN.388
30    FORMAT(/' *** BEGINNING OF LOWER BDY TIME DOES NOT MATCH THE ',            LBDYIN.389
     &      'MODEL STARTING TIME. XTIME,CURRENT_DATE,CDATE = ',F10.2,            LBDYIN.390
     &       2X,A19,2X,A19)                                                      LBDYIN.391
40    FORMAT(/' *** LOWER BDY CONDITIONS ARE READ IN AT TIME = ',A16,            LBDYIN.392
     &      ' AND USED FOR MODEL STARTING AT ',A16,' FOR DOMAIN ',I2)            LBDYIN.393
50    FORMAT(/' NEST ORDER WILL CONFUSE LBDYIN: REARRANGE')                      LBDYIN.394
60    FORMAT(/' INTERPOLATE LOWBDY FIELDS FROM NEST ',I2,' TO NEST ',I2)         LBDYIN.395
      END                                                                        LBDYIN.396
