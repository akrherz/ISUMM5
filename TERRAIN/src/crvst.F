      SUBROUTINE CRVST(VARNAME,NCAT,IX,JX,XLAT,XLON,YYY,ZZZ,LZZ,N,               CRVST.1
     1                 NOBS,GRDLNMN,GRDLTMN,ZLON,ZLAT,ZXXX,RESOL,IDW,IP)         06NOV00.53
C                                                                                CRVST.3
C   PURPOSE : TO CREAT THE VEGETATION, SOIL, VEGETATION FRACTION AND             CRVST.4
C             GROUND TEMPERATURE FIELDS FOR LAND-SURFACE MODEL.                  CRVST.5
C                                                                                CRVST.6
C   ON ENTRY: VARNAME    -- NAME OF THE FIELD                                    CRVST.7
C             NCAT       -- THE NUMBER OF CATEGORIES                             CRVST.8
C             IX,JX      -- DOMAIN SIZE                                          CRVST.9
C             XLAT, XLON -- LATITUDE AND LONGITUDE FOR MESOSCALE GRID            CRVST.10
C             N          -- DOMAIN ID NUMBER                                     CRVST.11
C             NOBS       -- NUMBER OF (LAT/LON) DATA POINTS                      CRVST.12
C             ZLON,ZLAT  -- LATITUDE AND LONGITUDE OF THE SOURCE DATA POINTS     CRVST.13
C             ZXXX       -- SOURCE DATA AT (ZLON,ZLAT)                           CRVST.14
C             RESOL      -- RESOULUTION OF THE SOURCE DATA                       CRVST.15
C             IDW        -- CATEGORY OF WATER, OTHERWISE SET IDW = 0             CRVST.16
C             GRDLTMN    -- MINIMUM LATITUDE IN THE SEARCH AREA                  CRVST.17
C             GRDLNMN    -- MINIMUM LONGITUDE IN THE SEARCH AREA                 CRVST.18
C    ON EXIT: YYY        -- THE PERCENTAGE FOR EACH OF CATEGORIES                CRVST.19
C                           ON MESOSCALE GRID                                    CRVST.20
C             ZZZ        -- THE PERCANTAGE CORRESPONDING TO THE DOMINATING       CRVST.21
C                           COTEGORY                                             CRVST.22
C             LZZ        -- THE DOMINATING COTEGORY AT THE GRID POINT            CRVST.23
C                                                                                CRVST.24
# include <parame.incl>                                                          CRVST.25
# include <paramed.incl>                                                         CRVST.26
# include <io.incl>                                                              06NOV00.54
# include <ltdata.incl>                                                          CRVST.27
C                                                                                CRVST.28
      DIMENSION YYY(IX,JX,NCAT), XLAT(IX,JX), XLON(IX,JX)                        CRVST.29
      DIMENSION ZLON(NOBS), ZLAT(NOBS), ZXXX(NCAT,NOBS)                          CRVST.30
      DIMENSION XXX(ITRH,JTRH), ZZZ(IX,JX), LZZ(IX,JX),                          CRVST.31
     -      WORK(ITRH*JTRH), NII(300),NJJ(300)                                   CRVST.32
      CHARACTER*(*) VARNAME                                                      CRVST.33
      LOGICAL FLAG                                                               CRVST.34
C                                                                                CRVST.35
      FLAG    = .TRUE.                                                           CRVST.36
      DO I = 1,IX                                                                CRVST.37
        DO J = 1,JX                                                              CRVST.38
          ZZZ(I,J) = 0.                                                          CRVST.39
          LZZ(I,J) = 0                                                           CRVST.40
        END DO                                                                   CRVST.41
      END DO                                                                     CRVST.42
C                                                                                CRVST.43
      DSGRID = RESOL                                                             CRVST.44
      INCR = 1./DSGRID+0.01                                                      CRVST.45
      IF(IP.EQ.1) PRINT 6, VARNAME,RESOL,INCR,IDW, NCAT                          06NOV00.55
6     FORMAT(' ==> CALL CRVST FOR ',A8,'  RESOLUTION =',                         CRVST.47
     1       F8.5,3X,'INCR=',I3,'   WATER CAT.=',I2,                             CRVST.48
     2       '  # OF CATEG.=',I2)                                                CRVST.49
      IF(IP.EQ.1) PRINT 7, NOBS,GRDLNMN, GRDLTMN                                 06NOV00.56
7     FORMAT(/2X,' NOBS=',I8,4X,' ==< GRDLNMN=',F10.2,                           CRVST.51
     1                            '   GRDLTMN=',F10.2,'>==')                     CRVST.52
C                                                                                CRVST.53
C-----GRID THE DATA.  GRDLTMN=MINIMUM LATITUDE OF INCOMING DATA.                 CRVST.54
C-----GRDLNMN = MINIMUM LONGITUDE OF INCOMING DATA.  POINT(1,1)                  CRVST.55
C-----OF ARRAY LNDIN IS LANDUSE AT (GRDLTMN,GRDLNMN)                             CRVST.56
C                                                                                CRVST.57
C .. IF REQUESTED RAW DATA COVERAGE IS FROM -180. TO 180., THE ARRAY             CRVST.58
C    LNDIN HOLDING DATA SHOULD BE EXPANDED TO AVOID THE ERRORS OCCURED           CRVST.59
C    OVER THE GRID POINTS LOCATED IN AREA OF -180+DSGRID/2 TO                    CRVST.60
C    180-DSGRID/2.                         YONG-RUN  09/26/97                    CRVST.61
C                                                                                CRVST.62
      IF (ABS(XMAXLON-180.).LT.1.E-3  .AND.                                      CRVST.63
     1    ABS(XMINLON+180.).LT.1.E-3) THEN                                       CRVST.64
        START_J = 2.0                                                            CRVST.65
        PRINT 26,START_J,XMAXLON,XMINLON                                         CRVST.66
 26     FORMAT(/'START_J=',F6.1,' XMAXLON=',F8.2,' XMINLON=',F8.2)               CRVST.67
      ELSE                                                                       CRVST.68
        START_J = 1.0                                                            CRVST.69
      ENDIF                                                                      CRVST.70
C                                                                                CRVST.71
C ..                                                                             CRVST.72
      IIMAXLND = -100000                                                         CRVST.73
      JIMAXLN  = -100000                                                         CRVST.74
      IIMINLND =  100000                                                         CRVST.75
      JIMINLN  =  100000                                                         CRVST.76
      DO 10 ILEV=1,NCAT                                                          CRVST.77
      NIL = 0                                                                    CRVST.78
      NPP = 0                                                                    CRVST.79
      SPP = 0.0                                                                  CRVST.80
C                                                                                CRVST.81
C -- PLACED THE 1-D DATA ZXXX TO 2-D DATA XXX:                                   CRVST.82
C                                                                                CRVST.83
      DO 20 II=1,NOBS                                                            CRVST.84
      JINDEX = NINT((ZLON(II) - GRDLNMN)*INCR + START_J)                         CRVST.85
      IINDEX = (ZLAT(II) - GRDLTMN)*INCR + 1.1                                   CRVST.86
C                                                                                CRVST.87
        IF (IINDEX.LT.1 .OR. IINDEX.GT.ITRH .OR.                                 CRVST.88
     -      JINDEX.LT.1 .OR. JINDEX.GT.JTRH) THEN                                CRVST.89
          PRINT *,'II=',II,'  IINDEX,JINDEX:',IINDEX,JINDEX                      CRVST.90
          PRINT *,'CURRENT ITRH=',ITRH,' JTRH=',JTRH,                            CRVST.91
     -            ' PLEASE INCREASE THEM !'                                      CRVST.92
          STOP                                                                   CRVST.93
      ENDIF                                                                      CRVST.94
C                                                                                CRVST.95
      IF (ILEV.EQ.1 .AND. IINDEX.GT.IIMAXLND) IIMAXLND = IINDEX                  CRVST.96
      IF (ILEV.EQ.1 .AND. JINDEX.GT.JIMAXLN ) JIMAXLN  = JINDEX                  CRVST.97
      IF (ILEV.EQ.1 .AND. IINDEX.LT.IIMINLND) IIMINLND = IINDEX                  CRVST.98
      IF (ILEV.EQ.1 .AND. JINDEX.LT.JIMINLN ) JIMINLN  = JINDEX                  CRVST.99
C                                                                                CRVST.100
      XXX(IINDEX,JINDEX) = ZXXX(ILEV,II)                                         CRVST.101
20    CONTINUE                                                                   CRVST.102
C                                                                                CRVST.103
      IF (ABS(XMAXLON-180.).LT.1.E-3  .AND.                                      CRVST.104
     1    ABS(XMINLON+180.).LT.1.E-3) THEN                                       CRVST.105
          DO II = IIMINLND,IIMAXLND                                              CRVST.106
            XXX(II,         1) = XXX(II,JIMAXLN )                                CRVST.107
            XXX(II,JIMAXLN +1) = XXX(II,       2)                                CRVST.108
          END DO                                                                 CRVST.109
          JIMINLND = 1                                                           CRVST.110
          JIMAXLND = JIMAXLN +1                                                  CRVST.111
      ELSE                                                                       CRVST.112
          JIMINLND = JIMINLN                                                     CRVST.113
          JIMAXLND = JIMAXLN                                                     CRVST.114
      ENDIF                                                                      CRVST.115
      IF (ILEV.EQ.1 .AND. IP.EQ.1) THEN                                          06NOV00.57
        PRINT *,'IIMAXLND=',IIMAXLND,'  JIMAXLND=',JIMAXLND                      CRVST.117
        PRINT *,'IIMINLND=',IIMINLND,'  JIMINLND=',JIMINLND                      CRVST.118
      ENDIF                                                                      CRVST.119
C                                                                                CRVST.120
      IF (IIMAXLND.GT.ITRH .OR. JIMAXLND.GT.JTRH) THEN                           CRVST.121
         PRINT 60,IIMAXLND,JIMAXLND,ITRH,JTRH,                                   CRVST.122
     1            GRDLTMN,GRDLNMN                                                CRVST.123
 60      FORMAT(1X,'*** IIMAXLND = ',I6,'  JIMAXLND = ',I6,                      CRVST.124
     1       '   ITRH=',I6,' JTRH=',I6/3X,'GRDLTMN=',F10.3,5X,                   CRVST.125
     2       'GRDLNMN = ',F10.3,' IN CRLND.')                                    CRVST.126
C                                                                                CRVST.127
         STOP 60                                                                 CRVST.128
      ENDIF                                                                      CRVST.129
C                                                                                CRVST.130
      DO    II=1,IX                                                              CRVST.131
      DO    JJ=1,JX                                                              CRVST.132
      YY = - (GRDLTMN-XLAT(II,JJ))/DSGRID + 1.0                                  CRVST.133
        IF (XLON(II,JJ).GT.XMAXLON .AND. GRDLNMN.LT.-180)                        CRVST.134
     2       XLON(II,JJ)=XLON(II,JJ)-360.                                        CRVST.135
      XX = - (GRDLNMN-XLON(II,JJ))/DSGRID + START_J                              CRVST.136
      YYY(II,JJ,ILEV) = BINT(YY,XX,XXX,ITRH,JTRH,FLAG)                           CRVST.137
C                                                                                CRVST.138
      IF (YY.LT.IIMINLND .OR. YY.GT.IIMAXLND .OR.                                CRVST.139
     1    XX.LT.JIMINLND .OR. XX.GT.JIMAXLND .AND. ILEV.EQ.1) THEN               CRVST.140
          NIL = NIL+1                                                            CRVST.141
          NII(NIL) = II                                                          CRVST.142
          NJJ(NIL) = JJ                                                          CRVST.143
          PRINT 25,NIL,ILEV,II,JJ,XX,YY,XLAT(II,JJ),XLON(II,JJ),                 CRVST.144
     1             YYY(II,JJ,ILEV)                                               23DEC04.1
 25       FORMAT(/'N=',I4,' ILEV=',I2,'  POINT(II,JJ):',2I5,' XX=',              CRVST.146
     1     F10.2,' YY=',F10.2,' XLAT=',F8.2,' XLON=',F8.2,' YYY=',F8.2)          CRVST.147
          IF (NIL.GT.300) THEN                                                   CRVST.148
            PRINT *,' NIL=',NIL,' > 300, TOO MANY MISSING POINTS '               CRVST.149
            STOP 25252                                                           CRVST.150
          ENDIF                                                                  CRVST.151
      ENDIF                                                                      CRVST.152
C   .........................................................................    CRVST.153
C                                                                                CRVST.154
      NPP = NPP+1                                                                CRVST.155
      SPP = SPP + YYY(II,JJ,ILEV)*YYY(II,JJ,ILEV)                                CRVST.156
C                                                                                CRVST.157
C     NOTE: IT IS DESIRABLE TO FORCE GRID BOXES WITH LESS                        CRVST.158
C           THAN 50 PERCENT WATER TO BE A LAND CATEGORY,                         CRVST.159
C           EVEN IF WATER IS THE LARGEST SINGLE CATEGORY.                        CRVST.160
C                                                                                CRVST.161
      IF (IDW .EQ. 0) GOTO 30                                                    CRVST.162
      IF (ILEV.EQ.IDW .AND. YYY(II,JJ,ILEV).LT.50.0) GOTO 30                     CRVST.163
C                                                                                CRVST.164
      IF (YYY(II,JJ,ILEV).GT.ZZZ(II,JJ)) THEN                                    CRVST.165
         ZZZ(II,JJ) = YYY(II,JJ,ILEV)                                            CRVST.166
         LZZ(II,JJ) = ILEV                                                       CRVST.167
      ENDIF                                                                      CRVST.168
C                                                                                CRVST.169
30    CONTINUE                                                                   CRVST.170
CC      IF (II.EQ.35.AND.JJ.EQ.31) PRINT 31,II,JJ,ILEV,                          CRVST.171
CC     -                            YYY(II,JJ,ILEV),ZZZ(II,JJ),LZZ(II,JJ)        CRVST.172
31    FORMAT('II,JJ,ILEV:',3I4,'  YYY=',F6.1,'  ZZZ=',F6.1,'  LZZ=',I2)          CRVST.173
C                                                                                CRVST.174
      END DO                                                                     CRVST.175
      END DO                                                                     CRVST.176
C                                                                                CRVST.177
      IF (NPP.GT.0) THEN                                                         CRVST.178
        SPP = SQRT(SPP/FLOAT(NPP))                                               CRVST.179
        IF (IP.EQ.1) PRINT 33, ILEV, NPP, SPP                                    06NOV00.58
 33     FORMAT(5X,'CATEG.=',I2,'  NPOINT=',I8,'  SQRT(P)=',E15.5)                CRVST.181
      ENDIF                                                                      CRVST.182
C                                                                                CRVST.183
10    CONTINUE                                                                   CRVST.184
C                                                                                CRVST.185
C .. TO FILL THE MISSING DATA AT GRID POINTS:                                    CRVST.186
C                                                                                CRVST.187
      IF (NIL.GT.0) THEN                                                         CRVST.188
        DO JL = 1, NIL                                                           CRVST.189
          IM = NII(JL)                                                           CRVST.190
          JM = NJJ(JL)                                                           CRVST.191
          INC = 0                                                                CRVST.192
 105      INC = INC+1                                                            CRVST.193
C                                                                                CRVST.194
          YSUM = 0.                                                              CRVST.195
          DO IL = 1,NCAT                                                         CRVST.196
            NLL = 0                                                              CRVST.197
            YSS = 0.0                                                            CRVST.198
            DO II = IM-INC,IM+INC                                                CRVST.199
            DO JJ = JM-INC,JM+INC                                                CRVST.200
              IF (II.NE.IM.AND.JJ.NE.JM) THEN                                    CRVST.201
                 NLL = NLL+1                                                     CRVST.202
                 YSS = YSS+YYY(II,JJ,IL)                                         CRVST.203
              ENDIF                                                              CRVST.204
            END DO                                                               CRVST.205
            END DO                                                               CRVST.206
            YYY(IM,JM,IL) = YSS/FLOAT(NLL)                                       CRVST.207
            YSUM = YSUM + YYY(IM,JM,IL)                                          CRVST.208
          END DO                                                                 CRVST.209
C                                                                                CRVST.210
          IF (YSUM.EQ.0.0) THEN                                                  CRVST.211
            GO TO 105                                                            CRVST.212
          ELSE                                                                   CRVST.213
            DO IL = 1,NCAT                                                       CRVST.214
              YYY(IM,JM,IL) = 100.*YYY(IM,JM,IL)/YSUM                            CRVST.215
            END DO                                                               CRVST.216
          ENDIF                                                                  CRVST.217
C                                                                                CRVST.218
C .. DOMINATED CATEGORY:                                                         CRVST.219
          IF (IDW .GT. 0) THEN                                                   CRVST.220
            DO 106 IL = 1,NCAT                                                   CRVST.221
              IF (IL.EQ.IDW .AND. YYY(IM,JM,IL).LT.50.0) GOTO 106                CRVST.222
C                                                                                CRVST.223
              IF (YYY(IM,JM,IL).GT.ZZZ(IM,JM)) THEN                              CRVST.224
                ZZZ(IM,JM) = YYY(IM,JM,IL)                                       CRVST.225
                LZZ(IM,JM) = IL                                                  CRVST.226
              ENDIF                                                              CRVST.227
106         CONTINUE                                                             CRVST.228
            IF(IP.EQ.1)PRINT 107,JL,IM,JM,INC,ZZZ(IM,JM),LZZ(IM,JM)              06NOV00.59
107         FORMAT('POINT. ',I3,' I,J:',2I4,' INC=',I3,                          CRVST.230
     >                    ' MAX_Y=',F6.1,' DOMINAT CAT.=',I3)                    CRVST.231
          ENDIF                                                                  CRVST.232
C                                                                                CRVST.233
        END DO                                                                   CRVST.234
      ENDIF                                                                      CRVST.235
C                                                                                CRVST.236
C ... NORMALIZE THE PERCENTAGES:                                                 CRVST.237
C                                                                                CRVST.238
      IF (IDW.NE.0) THEN                                                         CRVST.239
        NADJ = 0                                                                 CRVST.240
        DO I = 1,IX                                                              CRVST.241
        DO J = 1,JX                                                              CRVST.242
          SPCNT = 0.0                                                            CRVST.243
          DO ILEV = 1,NCAT                                                       CRVST.244
            SPCNT = SPCNT + YYY(I,J,ILEV)                                        CRVST.245
          END DO                                                                 CRVST.246
          IF (SPCNT.GT.0.0) THEN                                                 CRVST.247
            PADJC = 100./SPCNT                                                   CRVST.248
            IF (ABS(SPCNT-100.0).GT.0.1) THEN                                    CRVST.249
              NADJ = NADJ+1                                                      CRVST.250
CC            PRINT *,'I.J:',I,J,'  PADJC=',PADJC,'  SPCNT=',SPCNT               CRVST.251
            ENDIF                                                                CRVST.252
            DO ILEV = 1,NCAT                                                     CRVST.253
              YYY(I,J,ILEV) = YYY(I,J,ILEV)*PADJC                                CRVST.254
            END DO                                                               CRVST.255
          ENDIF                                                                  CRVST.256
        END DO                                                                   CRVST.257
        END DO                                                                   CRVST.258
        IF (IP.EQ.1) PRINT 111,NADJ                                              06NOV00.60
 111    FORMAT(10X,'NUMBER OF ADJUSTED POINTS =',I8)                             CRVST.260
      ENDIF                                                                      CRVST.261
C                                                                                CRVST.262
      RETURN                                                                     CRVST.263
      END                                                                        CRVST.264
