      SUBROUTINE DATA30S(XMAXLAT,XMINLAT,XMAXLON,XMINLON,CENTER,                 DATA30S.1
     >                   IDEM, IFORT)                                            DATA30S.2
C                                                                                DATA30S.3
C 40X50 DEMS                                                                     DATA30S.4
      PARAMETER (N1 = 4800, N2 = 6000)                                           DATA30S.5
C 60X30 DEMS                                                                     DATA30S.6
      PARAMETER (N3 = 7200, N4 = 3600)                                           DATA30S.7
C                                                                                DATA30S.8
      DIMENSION DLAT0(33),DLON0(33),DLAT1(33),DLON1(33),KD(33),                  DATA30S.9
     >          KDD(9,4)                                                         DATA30S.10
      CHARACTER MSS_FILE(33)*6, DEM_FILE(33)*11, DEM_TAR(33)*11                  DATA30S.11
      DATA  MSS_FILE/'Y39026'     ,'Y39027'     ,'Y39028'     ,                  DATA30S.12
     >               'Y39029'     ,'Y39030'     ,'Y39031'     ,                  DATA30S.13
     >               'Y39032'     ,'Y39033'     ,'Y39034'     ,                  DATA30S.14
     >               'Y39035'     ,'Y39036'     ,'Y39037'     ,                  DATA30S.15
     >               'Y39038'     ,'Y39039'     ,'Y39040'     ,                  DATA30S.16
     >               'Y39041'     ,'Y39042'     ,'Y39043'     ,                  DATA30S.17
     >               'Y39044'     ,'Y39045'     ,'Y39046'     ,                  DATA30S.18
     >               'Y39047'     ,'Y39048'     ,'Y39049'     ,                  DATA30S.19
     >               'Y39050'     ,'Y39051'     ,'Y39052'     ,                  DATA30S.20
     >               'Y39053'     ,'Y39054'     ,'Y39055'     ,                  DATA30S.21
     >               'Y39056'     ,'Y39057'     ,'Y39058'     /,                 DATA30S.22
     >      DEM_FILE/'E020N90.DEM','E060N90.DEM','E100N90.DEM',                  DATA30S.23
     >               'E140N90.DEM','W180N90.DEM','W140N90.DEM',                  DATA30S.24
     >               'W100N90.DEM','W060N90.DEM','W020N90.DEM',                  DATA30S.25
     >               'E020N40.DEM','E060N40.DEM','E100N40.DEM',                  DATA30S.26
     >               'E140N40.DEM','W180N40.DEM','W140N40.DEM',                  DATA30S.27
     >               'W100N40.DEM','W060N40.DEM','W020N40.DEM',                  DATA30S.28
     >               'E020S10.DEM','E060S10.DEM','E100S10.DEM',                  DATA30S.29
     >               'E140S10.DEM','W180S10.DEM','W140S10.DEM',                  DATA30S.30
     >               'W100S10.DEM','W060S10.DEM','W020S10.DEM',                  DATA30S.31
     >               'E060S60.DEM','E120S60.DEM','W180S60.DEM',                  DATA30S.32
     >               'W120S60.DEM','W060S60.DEM','W000S60.DEM'/,                 DATA30S.33
     >      DEM_TAR /'e020n90.tar','e060n90.tar','e100n90.tar',                  DATA30S.34
     >               'e140n90.tar','w180n90.tar','w140n90.tar',                  DATA30S.35
     >               'w100n90.tar','w060n90.tar','w020n90.tar',                  DATA30S.36
     >               'e020n40.tar','e060n40.tar','e100n40.tar',                  DATA30S.37
     >               'e140n40.tar','w180n40.tar','w140n40.tar',                  DATA30S.38
     >               'w100n40.tar','w060n40.tar','w020n40.tar',                  DATA30S.39
     >               'e020s10.tar','e060s10.tar','e100s10.tar',                  DATA30S.40
     >               'e140s10.tar','w180s10.tar','w140s10.tar',                  DATA30S.41
     >               'w100s10.tar','w060s10.tar','w020s10.tar',                  DATA30S.42
     >               'e060s60.tar','e120s60.tar','w180s60.tar',                  DATA30S.43
     >               'w120s60.tar','w060s60.tar','w000s60.tar'/                  DATA30S.44
C                                                                                DATA30S.45
      LOGICAL INSIDE                                                             DATA30S.46
C                                                                                DATA30S.47
      OPEN(IDEM  ,FILE = 'dem_read' ,FORM='FORMATTED',STATUS='UNKNOWN')          DATA30S.48
      OPEN(IDEM+2,FILE = 'data30sID',FORM='FORMATTED',STATUS='UNKNOWN')          DATA30S.50
C                                                                                DATA30S.51
      IFORT0 = IFORT                                                             DATA30S.52
      NRS = NINT(1./CENTER)                                                      DATA30S.53
      IT = (XMAXLON-XMINLON)*NRS+1                                               DATA30S.54
      JT = (XMAXLAT-XMINLAT)*NRS+1                                               DATA30S.55
      PRINT 15,IT,JT,NRS                                                         DATA30S.56
 15   FORMAT(/'IT=',I5,'  JT=',I5,'  NO. OF POINTS IN 1 DEG.=',I3)               DATA30S.57
C ........................................................................       DATA30S.58
C                                                                                DATA30S.59
      NK = 0                                                                     DATA30S.60
      DO 20 K = 1,33                                                             DATA30S.61
C                                                                                DATA30S.62
        READ(DEM_FILE(K),21) LON, LAT                                            DATA30S.63
 21     FORMAT(1X,I3,1X,I2)                                                      DATA30S.64
        IF (DEM_FILE(K)(1:1).EQ.'W') THEN                                        DATA30S.65
          XLON0 = -LON                                                           DATA30S.66
        ELSE                                                                     DATA30S.67
          XLON0 = LON                                                            DATA30S.68
        ENDIF                                                                    DATA30S.69
        IF (DEM_FILE(K)(5:5).EQ.'S') THEN                                        DATA30S.70
          XLAT1 = -LAT                                                           DATA30S.71
        ELSE                                                                     DATA30S.72
          XLAT1 = LAT                                                            DATA30S.73
        ENDIF                                                                    DATA30S.74
        IF (K.LE.27) THEN                                                        DATA30S.75
          XLON1 = XLON0 + FLOAT(N1-1)/120.                                       DATA30S.76
          XLAT0 = XLAT1 - FLOAT(N2-1)/120.                                       DATA30S.77
        ELSE                                                                     DATA30S.78
          XLON1 = XLON0 + FLOAT(N3-1)/120.                                       DATA30S.79
          XLAT0 = XLAT1 - FLOAT(N4-1)/120.                                       DATA30S.80
        ENDIF                                                                    DATA30S.81
C                                                                                DATA30S.82
C        PRINT 22,K,DEM_FILE(K),XLAT0,XLAT1,XLON0,XLON1                          DATA30S.83
 22     FORMAT('=> TILE ',I2,2X,A11,'  LAT->',2F10.3,' LON->',2F10.3)            DATA30S.84
C                                                                                DATA30S.85
        IF (XMINLON.GT.XLON1 .AND. XMINLAT.GT.XLAT1) GO TO 20                    DATA30S.86
C                                                                                DATA30S.87
        IF (XMINLON.EQ.-180. .AND. XMAXLON.EQ.180.) THEN                         DATA30S.88
C .. THE POLE INSIDE THE DOMAIN:                                                 DATA30S.89
          IF (XMAXLAT.EQ.90. .AND. K.LE.9) THEN                                  DATA30S.90
C   .. NORTH POLE                                                                DATA30S.91
            NK = NK+1                                                            DATA30S.92
            KD(NK) = K                                                           DATA30S.93
            DLAT0(NK) = XLAT0                                                    DATA30S.94
            DLON0(NK) = XLON0                                                    DATA30S.95
            DLAT1(NK) = XLAT1                                                    DATA30S.96
            DLON1(NK) = XLON1                                                    DATA30S.97
          ELSE IF (XMINLAT.EQ.-90. .AND. K.GT.27) THEN                           DATA30S.98
C   .. SOUTH POLE                                                                DATA30S.99
            NK = NK+1                                                            DATA30S.100
            KD(NK) = K                                                           DATA30S.101
            DLAT0(NK) = XLAT0                                                    DATA30S.102
            DLON0(NK) = XLON0                                                    DATA30S.103
            DLAT1(NK) = XLAT1                                                    DATA30S.104
            DLON1(NK) = XLON1                                                    DATA30S.105
          ENDIF                                                                  DATA30S.106
        ELSE                                                                     DATA30S.107
C .. THE POLE OUTSIDE THE DOMAIN:                                                DATA30S.108
          DO I = 1,IT                                                            DATA30S.109
          DO J = 1,JT                                                            DATA30S.110
            XX = XMINLON + (I-1)/FLOAT(NRS)                                      DATA30S.111
C                                                                                DATA30S.112
C**** 1 N HENRY 05/05/99 FOR DOMAINS THAT STRADDLE LONGITUDE 180                 DATA30S.113
C     WE MAY HAVE XMINLON LESS THAN -180... CHANGE THIS TO A +VE                 DATA30S.114
C     EAST LONGITUDE OTHERWISE IT WON'T PICK UP THE DATA TILE                    DATA30S.115
C     WEST OF 180.                                                               DATA30S.116
C                                                                                DATA30S.117
            IF (XX.LT.-180) XX = XX + 360                                        DATA30S.118
C                                                                                DATA30S.119
C**** END 1                                                                      DATA30S.120
C                                                                                DATA30S.121
            YY = XMINLAT + (J-1)/FLOAT(NRS)                                      DATA30S.122
            IF (INSIDE(YY,XX,XLAT0,XLAT1,XLON0,XLON1)) THEN                      DATA30S.123
              NK = NK+1                                                          DATA30S.124
              DLAT0(NK) = XLAT0                                                  DATA30S.125
              DLON0(NK) = XLON0                                                  DATA30S.126
              DLAT1(NK) = XLAT1                                                  DATA30S.127
              DLON1(NK) = XLON1                                                  DATA30S.128
              KD(NK)    = K                                                      DATA30S.129
              GO TO 20                                                           DATA30S.130
            ENDIF                                                                DATA30S.131
          END DO                                                                 DATA30S.132
          END DO                                                                 DATA30S.133
        ENDIF                                                                    DATA30S.134
C                                                                                DATA30S.135
 20   CONTINUE                                                                   DATA30S.136
C                                                                                DATA30S.137
      PRINT 24,NK                                                                DATA30S.138
 24   FORMAT(//'NEED ',I2,'  30-SEC TILES')                                      DATA30S.139
C                                                                                DATA30S.140
      DO I = 1,9                                                                 DATA30S.141
      DO J = 1,4                                                                 DATA30S.142
        KDD(I,J) = 0                                                             DATA30S.143
      END DO                                                                     DATA30S.144
      END DO                                                                     DATA30S.145
C                                                                                DATA30S.146
      NLAT = 1                                                                   DATA30S.147
      NBEG = 1                                                                   DATA30S.148
 30   CONTINUE                                                                   DATA30S.149
      PRINT *,'K-BEGIN =',NBEG,' NLAT=',NLAT                                     DATA30S.150
      NLON = 1                                                                   DATA30S.151
      K = NBEG                                                                   DATA30S.152
      KDD(NLON,NLAT) = K                                                         DATA30S.153
      KK = KD(K)                                                                 DATA30S.154
CC      PRINT 25,K,KK,DEM_FILE(KK),DLAT0(K),DLAT1(K),                            DATA30S.155
CC     -                             DLON0(K),DLON1(K)                           DATA30S.156
      DO K = NBEG+1,NK                                                           DATA30S.157
        KK = KD(K)                                                               DATA30S.158
CC        PRINT 25,K,KK,DEM_FILE(KK),DLAT0(K),DLAT1(K),                          DATA30S.159
CC     -                             DLON0(K),DLON1(K)                           DATA30S.160
 25     FORMAT(I2,' NO.',I2,2X,A11,' LAT:',2F10.3,' LON:',2F10.3)                DATA30S.161
        IF (DLAT0(K).EQ.DLAT0(NBEG)) THEN                                        DATA30S.162
          NLON = NLON+1                                                          DATA30S.163
          KDD(NLON,NLAT) = K                                                     DATA30S.164
        ENDIF                                                                    DATA30S.165
      END DO                                                                     DATA30S.166
C                                                                                DATA30S.167
      IF (KDD(NLON,NLAT).LT.NK) THEN                                             DATA30S.168
        NBEG = KDD(NLON,NLAT)+1                                                  DATA30S.169
        NLAT = NLAT+1                                                            DATA30S.170
        GO TO 30                                                                 DATA30S.171
      ENDIF                                                                      DATA30S.172
C                                                                                DATA30S.173
      DO I = 1,9                                                                 DATA30S.174
        IF (KDD(I,1).EQ.0) GO TO 28                                              DATA30S.175
        NLNEND = I                                                               DATA30S.176
      END DO                                                                     DATA30S.177
 28   DO J = 1,4                                                                 DATA30S.178
        IF (KDD(1,J).EQ.0) GO TO 29                                              DATA30S.179
        NLTEND = J                                                               DATA30S.180
      END DO                                                                     DATA30S.181
 29   CONTINUE                                                                   DATA30S.182
      DO J = 1,NLTEND                                                            DATA30S.183
        K = KDD(1,J)                                                             DATA30S.184
        LD = KD(K)                                                               DATA30S.185
        BLAT = DLAT0(K)                                                          DATA30S.186
        PRINT 31,J,LD,BLAT                                                       DATA30S.187
 31     FORMAT(/'J=',I2,' ND=',I2,' LAT=',F10.3)                                 DATA30S.188
        DO I = 1,NLNEND                                                          DATA30S.189
          MD = KDD(I,J)                                                          DATA30S.190
          LD = KD(MD)                                                            DATA30S.191
          IFORT = IFORT+1                                                        DATA30S.192
          PRINT 32,I,MD,DLAT0(MD),DLON0(MD),LD,DEM_FILE(LD)                      DATA30S.193
 32       FORMAT('I=',I2,' M=',I2,'  LAT/LON:',2F10.3,                           DATA30S.194
     -           '  D#=',I2,'  DEM_FILE=',A11)                                   DATA30S.195
          WRITE(IDEM,35)  DEM_FILE(LD), MSS_FILE(LD),                            DATA30S.196
     -                     DEM_TAR(LD), IFORT                                    DATA30S.197
 35       FORMAT(3X,A11,2X,                                                      DATA30S.198
     -           '/DSS/',                                                        DATA30S.199
     -           A6,2X,A11,2X,I2.2)                                              DATA30S.200
C     -           '/MESOUSER/MM5V2/TERRAIN/DATA/USGS_30S_ELEVATION/',            DATA30S.201
C     -           A11,2X,A11,2X,I2.2)                                            DATA30S.202
        END DO                                                                   DATA30S.203
      END DO                                                                     DATA30S.204
C                                                                                DATA30S.205
      WRITE(IDEM+2,37) XMINLAT,XMAXLAT,XMINLON,XMAXLON,                          DATA30S.206
     -                 NLNEND,NLTEND,IFORT0                                      DATA30S.207
 37   FORMAT(2X,'XMINLAT=',F8.3,', XMAXLAT=',F8.3,
     -       2X,'XMINLON=',F8.3,', XMAXLON=',F8.3,
     -       5X,'NLN =',I2,', NLT =',I2,', IFORT=',I2)
      WRITE(IDEM+2,38) NK                                                        DATA30S.211
 38   FORMAT('# OF TILES =',I2)                                                  DATA30S.212
      WRITE(IDEM+2,39) (KD(I),I=1,NK)                                            DATA30S.213
 39   FORMAT(33I4)                                                               DATA30S.214
      WRITE(IDEM+2,40)                                                           DATA30S.215
 40   FORMAT('KKK','   DLAT0       ','   DLAT1       ',                          DATA30S.216
     -             '   DLON0       ','   DLON1       ')                          DATA30S.217
      WRITE(IDEM+2,41) (K,DLAT0(K),DLAT1(K),DLON0(K),DLON1(K),K=1,NK)            DATA30S.218
 41   FORMAT(I3,4F15.8)                                                          DATA30S.219
C                                                                                DATA30S.220
      RETURN                                                                     DATA30S.221
      END                                                                        DATA30S.222
C                                                                                DATA30S.223
      LOGICAL FUNCTION INSIDE(YLAT,YLON,XLAT0,XLAT1,XLON0,XLON1)                 DATA30S.224
C                                                                                DATA30S.225
      INSIDE = (YLAT.GE.XLAT0.AND.YLAT.LE.XLAT1 .AND.                            DATA30S.226
     >          YLON.GE.XLON0.AND.YLON.LE.XLON1)                                 DATA30S.227
C                                                                                DATA30S.228
      RETURN                                                                     DATA30S.229
      END                                                                        DATA30S.230
      FUNCTION LEN_CHR(CHR)                                                      DATA30S.231
C                                                                                DATA30S.232
      CHARACTER*(*) CHR                                                          DATA30S.233
C                                                                                DATA30S.234
      N = LEN(CHR)                                                               DATA30S.235
C                                                                                DATA30S.236
      DO I = N,1,-1                                                              DATA30S.237
      IASCII = ICHAR(CHR(I:I))                                                   DATA30S.238
      IF (IASCII.NE.32.AND.IASCII.NE.0) GO TO 100                                DATA30S.239
      END DO                                                                     DATA30S.240
      LEN_CHR = 0                                                                DATA30S.241
      GO TO 101                                                                  DATA30S.242
C                                                                                DATA30S.243
 100  LEN_CHR = I                                                                DATA30S.244
C                                                                                DATA30S.245
 101  CONTINUE                                                                   DATA30S.246
      RETURN                                                                     DATA30S.247
      END                                                                        DATA30S.248
